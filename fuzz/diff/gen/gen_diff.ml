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

(* Publish an isolated run's artifacts into the shared [schema_dir], leaving its
   private [EverParse.h] / [EverParseEndianness.h] behind: a single shared copy
   comes from the serial seed run, so concurrent runs never race to rewrite
   them. Every other file is uniquely named per codec, so concurrent copies into
   the same directory never collide. *)
let publish_outputs ~src ~dst =
  Array.iter
    (fun f ->
      if
        not
          (String.equal f "EverParse.h"
          || String.equal f "EverParseEndianness.h")
      then copy_file ~src:(Filename.concat src f) ~dst:(Filename.concat dst f))
    (Sys.readdir src)

let rm_rf dir =
  (try Sys.readdir dir with Sys_error _ -> [||])
  |> Array.iter (fun f ->
      try Sys.remove (Filename.concat dir f) with Sys_error _ -> ());
  try Sys.rmdir dir with Sys_error _ -> ()

(* Run one schema's EverParse codegen in a private directory so concurrent runs
   cannot race on EverParse's shared intermediate files, then publish it. *)
let gen_isolated ~schema_dir schema =
  let tmp = Filename.temp_dir "wire_diffgen" "" in
  Fun.protect
    ~finally:(fun () -> rm_rf tmp)
    (fun () ->
      Wire_3d.run ~outdir:tmp [ schema ];
      publish_outputs ~src:tmp ~dst:schema_dir)

let exited_ok = function Unix.WEXITED 0 -> true | _ -> false

(* A bounded fork pool: each job runs in its own process, so the ~9s-per-module
   F* verification overlaps across cores ([Sys.command] inside [Wire_3d.run] is
   blocking) with full isolation. Returns per-job success in input order. *)
let run_jobs ~max_jobs jobs =
  let n = Array.length jobs in
  let ok = Array.make n false in
  let pid_idx = Hashtbl.create 64 in
  let next = ref 0 and running = ref 0 in
  let reap () =
    let pid, status = Unix.wait () in
    match Hashtbl.find_opt pid_idx pid with
    | Some i ->
        Hashtbl.remove pid_idx pid;
        decr running;
        ok.(i) <- exited_ok status
    | None -> ()
  in
  while !next < n || !running > 0 do
    if !next < n && !running < max_jobs then begin
      let i = !next in
      incr next;
      match Unix.fork () with
      | 0 -> (
          try
            jobs.(i) ();
            Unix._exit 0
          with e ->
            Printf.eprintf "diff-gen: %s\n%!" (Printexc.to_string e);
            Unix._exit 1)
      | pid ->
          Hashtbl.add pid_idx pid i;
          incr running
    end
    else reap ()
  done;
  ok

(* Conservative default: F* peaks near 1GB per module, so cap concurrency to
   keep memory in check on CI; override with WIRE_DIFF_JOBS. *)
let job_count () =
  match Sys.getenv_opt "WIRE_DIFF_JOBS" with
  | Some s -> (
      match int_of_string_opt s with Some k when k >= 1 -> k | _ -> 4)
  | None -> max 1 (min 4 (Domain.recommended_domain_count ()))

let schema_of (_, Fuzz_gen.Pack g) = Wire.Everparse.schema (Fuzz_gen.codec g)

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
          let schema = schema_of included.(k + 1) in
          fun () -> gen_isolated ~schema_dir schema)
    in
    let rest_fail =
      run_jobs ~max_jobs jobs |> Array.to_list
      |> List.mapi (fun k ok ->
          if ok then None
          else
            Some (fst included.(k + 1), "EverParse codegen failed (see above)"))
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
