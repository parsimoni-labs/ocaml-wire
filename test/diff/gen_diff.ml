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

    EverParse spends ~9s verifying each module; {!Wire_3d.run} overlaps the
    modules in {!Wire_3d.pool}, whose concurrency [WIRE_3D_JOBS] caps. *)

let module_name g =
  Wire_stubs.module_name
    (Option.get
       (Wire.Private.Everparse.entrypoint_struct
          (Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g))))

let schema_of (_, Fuzz_gen.Pack g) =
  Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g)

(* Generate the EverParse C of [schemas] into [schema_dir]. Is [None], or
   EverParse's diagnostics for the schemas it rejected, each named by its
   [.3d] file. *)
let generate_schemas schema_dir schemas =
  Fmt.epr "diff-gen: generating %d validators, up to %d in parallel\n%!"
    (List.length schemas) (Wire_3d.max_jobs ());
  match Wire_3d.run ~outdir:schema_dir schemas with
  | () -> None
  | exception (Failure msg | Invalid_argument msg) -> Some msg

(* The generated OCaml that reads one codec's field values back out of the C
   visitor. [parse_k] applies its continuation to one argument per named
   field, in declaration order and in the OCaml type [field_kinds] gives it, so
   the continuation only has to name the arguments and keep the compared ones.
   A field the comparison leaves out ([Diff_codecs.compared_fields]) gets a
   [_]-prefixed argument, and a codec with nothing to compare never calls into
   C at all. *)
let value_reader label (Fuzz_gen.Pack g as p) ppf =
  let kinds =
    Wire.Private.Everparse.Raw.field_kinds
      (Wire.Private.Everparse.Raw.struct_of_codec (Fuzz_gen.codec g))
    |> List.mapi (fun i (name, kind) -> (i, name, kind))
  in
  let compared = Diff_codecs.compared_fields p in
  let is_compared name =
    List.exists (fun (c : Diff_codecs.compared_field) -> c.name = name) compared
  in
  let int64_of (i, name, kind) =
    match kind with
    | Wire.Private.Everparse.Raw.Int -> Fmt.str "Int64.of_int a%d" i
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
  if compared = [] then Fmt.pf ppf "fun _ -> []"
  else
    Fmt.pf ppf
      "fun b ->@\n\
      \      Result.value ~default:[]@\n\
      \        (S.parse_k (fun %a -> [ %a ]) b 0)"
      Fmt.(list ~sep:(any " ") arg)
      kinds
      Fmt.(list ~sep:(any "; ") binding)
      (List.filter (fun (_, name, _) -> is_compared name) kinds)

(* The stubs of the [i]th included codec, applied to that codec: its accept
   check and its value reader. *)
let pp_stubs ppf i (label, (Fuzz_gen.Pack g as p)) =
  Fmt.pf ppf
    "let stubs_%d (type a) (codec : a Wire.Codec.t) =@\n\
    \  let module S = Stubs.%s.Make (struct@\n\
    \    type r = a@\n\
     @\n\
    \    let codec = codec@\n\
    \  end) in@\n\
    \  ((fun b -> Result.is_ok (S.parse b 0)), %t)@\n\
     @\n\
     let stubs_%d =@\n\
    \  match List.nth Diff_codecs.included %d with@\n\
    \  | _, Fuzz_gen.Pack g -> stubs_%d (Fuzz_gen.codec g)@\n\
     @\n"
    i (module_name g) (value_reader label p) i i i

let fixed schema_dir =
  Fmt.epr "diff-gen: %s\n" Diff_codecs.summary;
  (match
     generate_schemas schema_dir (List.map schema_of Diff_codecs.included)
   with
  | None -> ()
  | Some msg ->
      Fmt.epr
        "diff-gen: FATAL included codecs EverParse rejected:\n\
         %s\n\
         diff-gen: differential coverage would shrink silently. Project the \
         shape, or give it an explicit exclusion reason in diff_codecs.\n"
        msg;
      exit 1);
  Wire_stubs.generate ~schema_dir ~outdir:"."
    (List.map
       (fun (_, Fuzz_gen.Pack g) -> Wire_stubs.C (Fuzz_gen.codec g))
       Diff_codecs.included);
  (* Archive the per-codec validators so [wire_ffi.c] (headers only) links
     against them as separate translation units. *)
  Wire_stubs.build_codec_archive ~schema_dir ~archive:"libdiffcodecs.a" ();
  let oc = open_out "diff_index.ml" in
  let ppf = Format.formatter_of_out_channel oc in
  let pr fmt = Fmt.pf ppf fmt in
  pr "(* Generated by gen_diff: each included codec's stubs, applied to the\n";
  pr "   codec. *)\n\n";
  List.iteri (pp_stubs ppf) Diff_codecs.included;
  pr "(* Each included codec's label paired with a [bytes -> bool] accept\n";
  pr "   check: an error (validation reject or short input) reads as\n";
  pr "   reject. *)\n";
  pr "let covered : (string * (bytes -> bool)) array =\n  [|\n";
  List.iteri
    (fun i (label, _) -> pr "    (%S, fst stubs_%d);\n" label i)
    Diff_codecs.included;
  pr "  |]\n\n";
  pr "(* Each codec's label paired with a [bytes -> (string * int64) list]\n";
  pr "   reader of the field values the EverParse validator extracted, taken\n";
  pr "   from the default <Name>Fields visitor through the generated FFI\n";
  pr "   continuation. Scoped by [Diff_codecs.compared_fields], which the\n";
  pr "   OCaml side of the comparison scopes off too. *)\n";
  pr "let values : (string * (bytes -> (string * int64) list)) array =\n  [|\n";
  List.iteri
    (fun i (label, _) -> pr "    (%S, snd stubs_%d);\n" label i)
    Diff_codecs.included;
  pr "  |]\n";
  Format.pp_print_flush ppf ();
  close_out oc

(* The scalar members of a schema's default visitor, with the width of their C
   type: a byte span's member holds an offset, not a value, and a float has no
   integer view, so neither is compared. *)
let scalar_members (schema : Wire.Everparse.t) =
  List.filter_map
    (fun (f : Wire.Private.Everparse.visitor_field) ->
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
    (Wire.Private.Everparse.visitor_fields schema)

(* Each structured case through its FFI projection: the stub runs the verified
   validator with a zeroed default visitor, as the generated stubs do, and copies
   each of [members], a visitor field's name and what the oracle calls it, into
   [out]. It returns the consumed prefix, or [-1] for
   a rejected or out-of-bounds frame. *)
let pp_scan_stub ppf i (schema : Wire.Everparse.t) members =
  Fmt.pf ppf
    "CAMLprim value wire_diff_scan_%d(value buffer, value offset, value \
     length, value out) {@\n"
    i;
  Fmt.pf ppf "  uint8_t *data; uint64_t len;@\n";
  Fmt.pf ppf
    "  if (!scan_frame(buffer, offset, length, &data, &len)) return \
     Val_long(-1);@\n";
  Fmt.pf ppf "  %sFields f;@\n  memset(&f, 0, sizeof f);@\n"
    (Wire_3d.everparse_name (Wire.Everparse.name schema));
  Fmt.pf ppf
    "  uint64_t end = %s((WIRECTX *) &f, NULL, scan_error, data, len, 0);@\n"
    (Wire_3d.validator_name (Wire.Everparse.name schema));
  Fmt.pf ppf "  if (!scan_ok(end, len)) return Val_long(-1);@\n";
  List.iteri
    (fun k (member, _, _) ->
      Fmt.pf ppf "  Field(out, %d) = Val_long((intnat) f.%s);@\n" k
        (Wire.Private.Everparse.visitor_member member))
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
    (Wire_3d.validator_name (Wire.Everparse.name schema));
  Fmt.pf ppf "  if (!scan_ok(end, len)) return Val_long(-1);@\n";
  Fmt.pf ppf "  return Val_long((intnat) end);@\n}@\n"

(* The standalone projection is of the codec under a name of its own, so its
   validator links next to the FFI one. *)
let standalone codec =
  let name =
    "Standalone"
    ^ Wire.Private.Everparse.Raw.(struct_name (struct_of_codec codec))
  in
  Wire.Everparse.project ~mode:`Standalone (Wire.Codec.rename name codec)

(* Every sampled shape the OCaml scanner takes, as the scanner lanes run
   them. *)
let sampled_shapes () =
  List.filter
    (fun (_, Fuzz_gen.Pack g) ->
      match Wire_ocaml.outputs (Fuzz_gen.codec g) with
      | _ -> true
      | exception Invalid_argument _ -> false)
    Diff_ocaml.scan_codecs

(* A sampled shape's reference states its top-level integer fields under
   their OCaml label, so the oracle reads those members under that label. *)
let top_level codec members =
  let top =
    List.filter_map
      (fun (Wire.Private.Types.Field f) -> f.field_name)
      (Wire.Private.Gen.to_struct codec).fields
  in
  List.filter_map
    (fun (name, bits) ->
      if List.mem name top then
        Some (name, Wire.Private.Names.ocaml_label name, bits)
      else None)
    members

let scanners schema_dir =
  let structured =
    List.map
      (fun (Diff_codecs.Scan c) ->
        let schema = Wire.Everparse.project ~mode:`Ffi c.codec in
        ( c.label,
          schema,
          standalone c.codec,
          List.map (fun (n, bits) -> (n, n, bits)) (scalar_members schema) ))
      Diff_codecs.scan_cases
  in
  (* Each sampled shape under a name of its own, so its validators link next
     to the fixed pipeline's of the same codec. A shape the scanner takes must
     project: one that does not is a defect, as an EverParse rejection is. *)
  let sampled, unprojected =
    List.partition_map
      (fun (i, (label, Fuzz_gen.Pack g)) ->
        let codec =
          Wire.Codec.rename (Fmt.str "Sampled%d" i) (Fuzz_gen.codec g)
        in
        match
          let schema = Wire.Everparse.project ~mode:`Ffi codec in
          ( label,
            schema,
            standalone codec,
            top_level codec (scalar_members schema) )
        with
        | entry -> Left entry
        | exception Invalid_argument message -> Right (label, message))
      (List.mapi (fun i s -> (i, s)) (sampled_shapes ()))
  in
  Fmt.epr "diff-gen: structured oracle: %d cases and %d sampled shapes@."
    (List.length structured)
    (List.length sampled + List.length unprojected);
  let fatal failures =
    List.iter
      (fun (label, message) -> Fmt.epr "diff-gen: FATAL %s: %s@." label message)
      failures;
    if failures <> [] then failwith "a scanned shape has no EverParse validator"
  in
  fatal unprojected;
  let all = structured @ sampled in
  Option.iter
    (fun msg -> fatal [ ("EverParse", msg) ])
    (generate_schemas schema_dir
       (List.concat_map
          (fun (_, ffi, standalone, _) -> [ ffi; standalone ])
          all));
  Wire_stubs.build_codec_archive ~schema_dir ~archive:"libscanvalidators.a" ();
  let b = Buffer.create 2048 in
  let ppf = Format.formatter_of_buffer b in
  Fmt.pf ppf
    "#include <stdint.h>\n#include <string.h>\n#include <caml/mlvalues.h>\n";
  Fmt.pf ppf "#include \"EverParse.h\"\n";
  List.iter
    (fun (_, (ffi : Wire.Everparse.t), (standalone : Wire.Everparse.t), _) ->
      let base = String.capitalize_ascii (Wire.Everparse.name ffi) in
      Fmt.pf ppf "#include \"%s_Fields.h\"\n#include \"%s.h\"\n" base base;
      Fmt.pf ppf "#include \"%s.h\"\n"
        (String.capitalize_ascii (Wire.Everparse.name standalone)))
    all;
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
  List.iteri
    (fun i (_, ffi, standalone, members) ->
      pp_scan_stub ppf i ffi members;
      pp_standalone_stub ppf i standalone)
    all;
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
        all;
      let entries ~first l =
        List.iteri
          (fun i (label, _, _, fields) ->
            let i = first + i in
            Fmt.pf ppf "(%S, ([ %a ], scan_%d, standalone_%d));\n" label
              Fmt.(
                list ~sep:(any "; ") (fun ppf (_, n, w) ->
                    pf ppf "(%S, %d)" n w))
              fields i i)
          l
      in
      Fmt.pf ppf
        "(* Each case's label, the scalar members of its visitor with their \
         width in bits, the stub of its FFI projection and the stub of its \
         standalone one. *)@\n";
      Fmt.pf ppf "let covered = [\n";
      entries ~first:0 structured;
      Fmt.pf ppf "]@\n@\n";
      Fmt.pf ppf
        "(* The same for each sampled shape the OCaml scanner takes, its \
         visitor members its top-level fields, each named by its OCaml label. \
         *)@\n";
      Fmt.pf ppf "let sampled = [\n";
      entries ~first:(List.length structured) sampled;
      Fmt.pf ppf "]@.";
      Format.pp_print_flush ppf ())

let () =
  match Array.to_list Sys.argv with
  | [ _; "--scanners"; schema_dir ] -> scanners schema_dir
  | [ _; schema_dir ] -> fixed schema_dir
  | _ -> fixed "schemas"
