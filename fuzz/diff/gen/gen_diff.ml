(** Generate EverParse C validators + FFI stubs for the differential fuzzer.

    Every codec in {!Diff_codecs.included} is run through EverParse to produce
    its validator. A codec the registry deemed includable but EverParse rejects
    is a hard error rather than a silent skip: dropping it would shrink
    differential coverage without anyone noticing (it lands in neither the
    fuzzed set nor {!Diff_codecs.excluded}), so projection and the [includable]
    predicate must stay in agreement. Emits the EverParse C into [<argv1>/]
    (default [schemas/]) and [wire_ffi.c] + [stubs.ml] + [diff_index.ml] (a
    [covered] array pairing each codec's label with a [bytes -> bool] accept
    check) into the current directory. Run by the [gen] dune rule.

    EverParse spends ~9s verifying each module, so the codecs are generated in a
    bounded fork pool, each run in a private directory (EverParse's shared
    intermediate files would otherwise race), with one serial seed run to lay
    down the shared headers. [WIRE_DIFF_JOBS] caps the concurrency. *)

let lower_name g =
  String.lowercase_ascii
    (Wire.Everparse.Raw.struct_name
       (Option.get
          (Wire.Everparse.entrypoint_struct
             (Wire.Everparse.schema (Fuzz_gen.codec g)))))

let copy_file ~src ~dst =
  In_channel.with_open_bin src (fun ic ->
      Out_channel.with_open_bin dst (fun oc ->
          Out_channel.output_string oc (In_channel.input_all ic)))

(* Publish an isolated run's per-codec artifacts into the shared [schema_dir].
   Only files whose name starts with this codec's module [base] are copied: the
   shared ones a run also emits ([EverParse.h], [EverParseEndianness.h],
   [test.c], [dune.inc]) come once from the serial seed run, so concurrent runs
   neither race to rewrite them nor leave nondeterministic copies behind. Each
   run holds only its own module's files, so the prefix is unambiguous and the
   per-codec names never collide across runs. *)
let publish_outputs ~base ~src ~dst =
  Array.iter
    (fun f ->
      if String.starts_with ~prefix:base f then
        copy_file ~src:(Filename.concat src f) ~dst:(Filename.concat dst f))
    (Sys.readdir src)

let rm_rf dir =
  (try Sys.readdir dir with Sys_error _ -> [||])
  |> Array.iter (fun f ->
      try Sys.remove (Filename.concat dir f) with Sys_error _ -> ());
  try Sys.rmdir dir with Sys_error _ -> ()

let redirect_to file =
  let fd =
    Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  Unix.dup2 fd Unix.stdout;
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

(* Run one codec's EverParse codegen in a private directory so concurrent runs
   cannot race on EverParse's shared intermediate files, then publish its
   per-codec artifacts. Runs inside a fork-pool child: stdout/stderr are
   captured to a per-codec [<base>.faillog] so a failure's F* diagnostics are
   attributable rather than interleaved; the log is dropped on success. *)
let gen_isolated ~schema_dir ~base schema =
  redirect_to (Filename.concat schema_dir (base ^ ".faillog"));
  let tmp = Filename.temp_dir "wire_diffgen" "" in
  Fun.protect
    ~finally:(fun () -> rm_rf tmp)
    (fun () ->
      Wire_3d.run ~quiet:false ~outdir:tmp [ schema ];
      publish_outputs ~base ~src:tmp ~dst:schema_dir);
  try Sys.remove (Filename.concat schema_dir (base ^ ".faillog"))
  with Sys_error _ -> ()

(* Conservative default: F* peaks near 1GB per module, so cap concurrency to
   keep memory in check on CI; override with WIRE_DIFF_JOBS. *)
let job_count () =
  match Sys.getenv_opt "WIRE_DIFF_JOBS" with
  | Some s -> (
      match int_of_string_opt s with Some k when k >= 1 -> k | _ -> 4)
  | None -> max 1 (min 4 (Domain.recommended_domain_count ()))

let schema_of (_, Fuzz_gen.Pack g) = Wire.Everparse.schema (Fuzz_gen.codec g)

let base_of item =
  Filename.remove_extension (Wire.Everparse.filename (schema_of item))

let read_log path =
  try In_channel.with_open_text path In_channel.input_all
  with Sys_error _ -> ""

(* Generate every included codec's EverParse C. The first runs serially into
   [schema_dir] to seed the shared headers; the rest run in the fork pool, each
   in a private directory. Returns the codecs EverParse rejected. *)
let generate_all schema_dir included =
  let included = Array.of_list included in
  let n = Array.length included in
  if n = 0 then []
  else begin
    let max_jobs = job_count () in
    Fmt.epr "diff-gen: generating %d validators, up to %d in parallel\n%!" n
      max_jobs;
    let seed_fail =
      match Wire_3d.run ~outdir:schema_dir [ schema_of included.(0) ] with
      | () -> []
      | exception e -> [ (fst included.(0), Printexc.to_string e) ]
    in
    let jobs =
      Array.init (n - 1) (fun k ->
          let item = included.(k + 1) in
          let schema = schema_of item and base = base_of item in
          fun () -> gen_isolated ~schema_dir ~base schema)
    in
    let rest_fail =
      Wire_3d.fork_pool ~max_jobs jobs
      |> Array.to_list
      |> List.mapi (fun k ok ->
          if ok then None
          else
            let item = included.(k + 1) in
            let log = Filename.concat schema_dir (base_of item ^ ".faillog") in
            Some (fst item, read_log log))
      |> List.filter_map Fun.id
    in
    seed_fail @ rest_fail
  end

let () =
  let schema_dir =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "schemas"
  in
  Fmt.epr "diff-gen: %s\n" Diff_codecs.summary;
  let failures = generate_all schema_dir Diff_codecs.included in
  (match failures with
  | [] -> ()
  | _ :: _ ->
      List.iter
        (fun (label, msg) ->
          Fmt.epr
            "diff-gen: FATAL %s is included but EverParse rejected it: %s\n"
            label msg)
        failures;
      Fmt.epr
        "diff-gen: %d included codec(s) failed to project; differential \
         coverage would shrink silently. Project the shape, or give it an \
         explicit exclusion reason in diff_codecs.\n"
        (List.length failures);
      exit 1);
  Wire_stubs.generate ~schema_dir ~outdir:"."
    (List.map
       (fun (_, Fuzz_gen.Pack g) -> Wire_stubs.C (Fuzz_gen.codec g))
       Diff_codecs.included);
  (* Archive the per-codec validators so [wire_ffi.c] (headers only) links
     against them as separate translation units. *)
  Wire_stubs.build_codec_archive ~schema_dir ~archive:"libdiffcodecs.a";
  let oc = open_out "diff_index.ml" in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "(* Generated by gen_diff: each included codec's label paired with a\n";
  pr "   [bytes -> bool] accept check. A Failure (validation reject or short\n";
  pr "   input) reads as reject. *)\n\n";
  pr "let covered : (string * (bytes -> bool)) array =\n  [|\n";
  List.iter
    (fun (label, Fuzz_gen.Pack g) ->
      pr
        "    (%S, fun b -> try ignore (Stubs.%s_parse b 0); true with Failure \
         _ -> false);\n"
        label (lower_name g))
    Diff_codecs.included;
  pr "  |]\n";
  Format.pp_print_flush ppf ();
  close_out oc
