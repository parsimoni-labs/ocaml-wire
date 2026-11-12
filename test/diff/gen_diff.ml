(** Generate EverParse C validators + FFI stubs for the differential fuzzer.

    Every codec in {!Diff_codecs.included} is run through EverParse to produce
    its validator. A codec the registry deemed includable but EverParse rejects
    is a hard error rather than a silent skip: dropping it would shrink
    differential coverage without anyone noticing (it lands in neither the
    fuzzed set nor {!Diff_codecs.excluded}), so projection and the [includable]
    predicate must stay in agreement. Emits the EverParse C into [<argv1>/]
    (default [schemas/]) and [wire_ffi.c] + [stubs.ml] + [diff_index.ml] (a
    [covered] array pairing each codec's label with a [bytes -> bool] accept
    check) into the current directory. Run by the code generation rule in
    [fuzz/diff/].

    EverParse spends ~9s verifying each module, so the codecs are generated in a
    bounded fork pool, each run in a private directory (EverParse's shared
    intermediate files would otherwise race), with one serial seed run to lay
    down the shared headers. [WIRE_DIFF_JOBS] caps the concurrency. *)

let lower_name g =
  String.lowercase_ascii
    (Wire.Everparse.Raw.struct_name
       (Option.get
          (Wire.Everparse.entrypoint_struct
             (Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g)))))

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
    ~finally:(fun () -> Wire_3d.rm_rf tmp)
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

let schema_of (_, Fuzz_gen.Pack g) =
  Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g)

let read_log path =
  try In_channel.with_open_text path In_channel.input_all
  with Sys_error _ -> ""

(* Generate every included codec's EverParse C. The first runs serially into
   [schema_dir] to seed the shared headers; the rest run in the fork pool, each
   in a private directory. Returns the codecs EverParse rejected. *)
let generate_schemas schema_dir included =
  let included = Array.of_list included in
  let n = Array.length included in
  if n = 0 then []
  else begin
    let max_jobs = job_count () in
    Fmt.epr "diff-gen: generating %d validators, up to %d in parallel\n%!" n
      max_jobs;
    let seed_fail =
      match Wire_3d.run ~outdir:schema_dir [ snd included.(0) ] with
      | () -> []
      | exception (Failure msg | Invalid_argument msg) ->
          [ (fst included.(0), msg) ]
    in
    let jobs =
      Array.init (n - 1) (fun k ->
          let item = included.(k + 1) in
          let schema = snd item in
          let base =
            Filename.remove_extension (Wire.Everparse.filename schema)
          in
          fun () -> gen_isolated ~schema_dir ~base schema)
    in
    let rest_fail =
      Wire_3d.fork_pool ~max_jobs jobs
      |> Array.to_list
      |> List.mapi (fun k ok ->
          if ok then None
          else
            let item = included.(k + 1) in
            let log =
              Filename.concat schema_dir
                (Filename.remove_extension (Wire.Everparse.filename (snd item))
                ^ ".faillog")
            in
            Some (fst item, read_log log))
      |> List.filter_map Fun.id
    in
    seed_fail @ rest_fail
  end

(* The generated OCaml that reads one codec's field values back out of the C
   plug. [<lower>_parse_k] applies its continuation to one argument per named
   field, in declaration order and in the OCaml type [field_kinds] gives it, so
   the continuation only has to name the arguments and keep the compared ones.
   A field the comparison leaves out ([Diff_codecs.compared_fields]) gets a
   [_]-prefixed argument, and a codec with nothing to compare never calls into
   C at all. *)
let value_reader (label, (Fuzz_gen.Pack g as p)) ppf =
  let kinds =
    Wire.Everparse.Raw.field_kinds
      (Wire.Everparse.Raw.struct_of_codec (Fuzz_gen.codec g))
    |> List.mapi (fun i (name, kind) -> (i, name, kind))
  in
  let compared = Diff_codecs.compared_fields p in
  let is_compared name =
    List.exists (fun (c : Diff_codecs.compared_field) -> c.name = name) compared
  in
  let int64_of (i, name, kind) =
    match kind with
    | Wire.Everparse.Raw.Int -> Fmt.str "Int64.of_int a%d" i
    | Int64 -> Fmt.str "a%d" i
    | Float32 | Float64 | Bool | String | Unit ->
        Fmt.failwith
          "gen_diff: %s field %s is compared by value, but the FFI hands it \
           back as a non-integer"
          label name
  in
  let arg ppf (i, name, _) =
    Fmt.pf ppf "%sa%d" (if is_compared name then "" else "_") i
  in
  let binding ppf ((_, name, _) as f) =
    Fmt.pf ppf "(%S, %s)" name (int64_of f)
  in
  if compared = [] then Fmt.pf ppf "    (%S, fun _ -> []);@\n" label
  else
    Fmt.pf ppf
      "    ( %S,@\n\
      \      fun b ->@\n\
      \        Stubs.%s_parse_k@\n\
      \          (fun %a -> [ %a ])@\n\
      \          b 0 );@\n"
      label (lower_name g)
      Fmt.(list ~sep:(any " ") arg)
      kinds
      Fmt.(list ~sep:(any "; ") binding)
      (List.filter (fun (_, name, _) -> is_compared name) kinds)

let fixed schema_dir =
  Fmt.epr "diff-gen: %s\n" Diff_codecs.summary;
  let failures =
    generate_schemas schema_dir
      (List.map (fun item -> (fst item, schema_of item)) Diff_codecs.included)
  in
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
  pr "  |]\n\n";
  pr "(* Each codec's label paired with a [bytes -> (string * int64) list]\n";
  pr "   reader of the field values the EverParse validator extracted, taken\n";
  pr "   from the default <Name>Fields plug through the generated FFI\n";
  pr "   continuation. Scoped by [Diff_codecs.compared_fields], which the\n";
  pr "   OCaml side of the comparison scopes off too. *)\n";
  pr "let values : (string * (bytes -> (string * int64) list)) array =\n  [|\n";
  List.iter (fun item -> pr "%t" (value_reader item)) Diff_codecs.included;
  pr "  |]\n";
  Format.pp_print_flush ppf ();
  close_out oc

(* The scalar members of a schema's default plug, with the width of their C
   type: a byte span's member holds an offset, not a value, and a float has no
   integer view, so neither is compared. *)
let scalar_members (schema : Wire.Everparse.t) =
  List.filter_map
    (fun (f : Wire.Everparse.plug_field) ->
      let bits =
        match f.c_type with
        | "uint8_t" -> Some 8
        | "uint16_t" -> Some 16
        | "uint32_t" -> Some 32
        | "uint64_t" -> Some 64
        | _ -> None
      in
      match bits with
      | Some bits when not (String.ends_with ~suffix:"SetBytes" f.setter) ->
          Some (f.name, bits)
      | _ -> None)
    (Wire.Everparse.plug_fields schema)

(* Each structured case through its FFI projection: the stub runs the verified
   validator with a zeroed default plug, as the generated stubs do, and copies
   every scalar member into [out]. It returns the consumed prefix, or [-1] for
   a rejected or out-of-bounds frame. *)
let pp_scan_stub ppf i (schema : Wire.Everparse.t) =
  let members = scalar_members schema in
  Fmt.pf ppf
    "CAMLprim value wire_diff_scan_%d(value buffer, value offset, value \
     length, value out) {@\n"
    i;
  Fmt.pf ppf "  uint8_t *data; uint64_t len;@\n";
  Fmt.pf ppf
    "  if (!scan_frame(buffer, offset, length, &data, &len)) return \
     Val_long(-1);@\n";
  Fmt.pf ppf "  %sFields f;@\n  memset(&f, 0, sizeof f);@\n"
    (Wire_3d.everparse_name schema.name);
  Fmt.pf ppf
    "  uint64_t end = %s((WIRECTX *) &f, NULL, scan_error, data, len, 0);@\n"
    (Wire_stubs.validator_name schema.name);
  Fmt.pf ppf "  if (!scan_ok(end, len)) return Val_long(-1);@\n";
  List.iteri
    (fun k (name, _) ->
      Fmt.pf ppf "  Field(out, %d) = Val_long((intnat) f.%s);@\n" k
        (Wire.Everparse.plug_member name))
    members;
  Fmt.pf ppf "  return Val_long((intnat) end);@\n}@\n"

(* The same case through its standalone projection, the default [.3d] output:
   the consumed prefix, or [-1]. *)
let pp_standalone_stub ppf i (schema : Wire.Everparse.t) =
  Fmt.pf ppf
    "CAMLprim value wire_diff_standalone_%d(value buffer, value offset, value \
     length) {@\n"
    i;
  Fmt.pf ppf "  uint8_t *data; uint64_t len;@\n";
  Fmt.pf ppf
    "  if (!scan_frame(buffer, offset, length, &data, &len)) return \
     Val_long(-1);@\n";
  Fmt.pf ppf "  uint64_t end = %s(NULL, scan_error, data, len, 0);@\n"
    (Wire_stubs.validator_name schema.name);
  Fmt.pf ppf "  if (!scan_ok(end, len)) return Val_long(-1);@\n";
  Fmt.pf ppf "  return Val_long((intnat) end);@\n}@\n"

(* The standalone projection is of the codec under a name of its own, so its
   validator links next to the FFI one. *)
let standalone codec =
  let name =
    "Standalone" ^ Wire.Everparse.Raw.(struct_name (struct_of_codec codec))
  in
  Wire.Everparse.project ~mode:`Standalone (Wire.Codec.rename name codec)

let scanners schema_dir =
  let cases =
    List.map
      (fun (Diff_codecs.Scan c) ->
        (c.label, Wire.Everparse.project ~mode:`Ffi c.codec))
      Diff_codecs.scan_cases
  in
  let standalones =
    List.map
      (fun (Diff_codecs.Scan c) ->
        (c.label ^ " (standalone)", standalone c.codec))
      Diff_codecs.scan_cases
  in
  let failures = generate_schemas schema_dir (cases @ standalones) in
  List.iter
    (fun (label, message) -> Fmt.epr "diff-gen: FATAL %s: %s@." label message)
    failures;
  if failures <> [] then
    failwith "EverParse rejected a required structured oracle";
  Wire_stubs.build_codec_archive ~schema_dir ~archive:"libscanvalidators.a";
  let b = Buffer.create 2048 in
  let ppf = Format.formatter_of_buffer b in
  Fmt.pf ppf
    "#include <stdint.h>\n#include <string.h>\n#include <caml/mlvalues.h>\n";
  Fmt.pf ppf "#include \"EverParse.h\"\n";
  List.iter
    (fun (_, (schema : Wire.Everparse.t)) ->
      let base = String.capitalize_ascii schema.name in
      Fmt.pf ppf "#include \"%s_Fields.h\"\n#include \"%s.h\"\n" base base)
    cases;
  List.iter
    (fun (_, (schema : Wire.Everparse.t)) ->
      Fmt.pf ppf "#include \"%s.h\"\n" (String.capitalize_ascii schema.name))
    standalones;
  Fmt.pf ppf
    {|
static void scan_error(const char *s, const char *f, const char *r, uint64_t k,
                       uint8_t *ctx, uint8_t *input, uint64_t pos) {
  (void)s; (void)f; (void)r; (void)k; (void)ctx; (void)input; (void)pos;
}
static int scan_frame(value buffer, value offset, value length,
                      uint8_t **data, uint64_t *len) {
  intnat off = Long_val(offset), n = Long_val(length);
  mlsize_t available = caml_string_length(buffer);
  if (off < 0 || n < 0 || (uintnat)off > available ||
      (uintnat)n > available - (uintnat)off || (uintnat)n > UINT32_MAX)
    return 0;
  *data = (uint8_t *)Bytes_val(buffer) + off;
  *len = (uint64_t)n;
  return 1;
}
static int scan_ok(uint64_t end, uint64_t len) {
  return EverParseIsSuccess(end) && end <= len && end <= (uint64_t)Max_long;
}
|};
  List.iteri (fun i (_, schema) -> pp_scan_stub ppf i schema) cases;
  List.iteri (fun i (_, schema) -> pp_standalone_stub ppf i schema) standalones;
  Format.pp_print_flush ppf ();
  Out_channel.with_open_text "scan_ffi.c" (fun oc ->
      output_string oc (Buffer.contents b));
  Out_channel.with_open_text "scan_oracle_index.ml" (fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      List.iteri
        (fun i _ ->
          Fmt.pf ppf
            "external scan_%d : bytes -> int -> int -> int array -> int = \
             \"wire_diff_scan_%d\" [@@@@noalloc]\n\
             external standalone_%d : bytes -> int -> int -> int = \
             \"wire_diff_standalone_%d\" [@@@@noalloc]\n"
            i i i i)
        cases;
      Fmt.pf ppf
        "(* Each case's label, the scalar members of its plug with their width \
         in bits, the stub of its FFI projection and the stub of its \
         standalone one. *)@\n";
      Fmt.pf ppf "let covered = [\n";
      List.iteri
        (fun i (label, schema) ->
          Fmt.pf ppf "(%S, ([ %a ], scan_%d, standalone_%d));\n" label
            Fmt.(list ~sep:(any "; ") (fun ppf (n, w) -> pf ppf "(%S, %d)" n w))
            (scalar_members schema) i i)
        cases;
      Fmt.pf ppf "]@.";
      Format.pp_print_flush ppf ())

let () =
  match Array.to_list Sys.argv with
  | [ _; "--scanners"; schema_dir ] -> scanners schema_dir
  | [ _; schema_dir ] -> fixed schema_dir
  | _ -> fixed "schemas"
