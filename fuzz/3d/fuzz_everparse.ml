(** Fuzz tests for 3D / EverParse projection, driven entirely by {!Fuzz_gen}.

    Every generated codec is projected to a 3D schema and pretty-printed
    ({!Fuzz_gen.everparse_cases}), so the projection and code-generation path is
    exercised on the same compositions the OCaml round-trip suite uses, plus
    arbitrary nested ones. When [3d.exe] is available, a bounded set of the
    composite schemas is additionally run through EverParse ([3d.exe --batch]):
    every codec that builds must also project to a schema EverParse verifies.
    That extraction pass is the only check that catches kind errors (a list over
    a possibly-empty element), which surface during F* verification, not during
    pretty-printing. It is skipped where [3d.exe] is absent (CI, AFL). *)

open Alcobar

(* Projection + pretty-print coverage: cheap, no subprocess, always runs. *)
let pp_cases =
  List.concat_map
    (fun (name, Fuzz_gen.Pack g) -> Fuzz_gen.everparse_cases name g)
    Fuzz_gen.registry

let nested_pp_cases =
  Fuzz_gen.everparse_nested_cases "nested(d2)" 2
  @ Fuzz_gen.everparse_nested_cases "nested(d3)" 3
  @ Fuzz_gen.everparse_nested_cases "nested(d4)" 4

(* The bounded set run through real EverParse: the composite / structural
   families, where a projection that builds but does not verify can hide. Plain
   scalars project trivially, so they are left to the pp pass. *)
let extract_names =
  [
    (* uint63 is the one scalar that does not project trivially: it has no
       native 3D type and must go through UINT64, so it gets a real EverParse
       check rather than the pp pass. *)
    "uint63";
    "uint63be";
    "array(3,uint16be)";
    "array_seq(3,uint16be)";
    "array(2,record)";
    "repeat(8,uint8)";
    "repeat_seq(8,uint8)";
    "casetype_u8";
    "nested(4,uint16be)";
    "nested_at_most(4,uint16be)";
    "optional(uint8)";
    "optional_or(uint8)";
    "record";
    "rest_bytes";
  ]

(* EverParse derives the module name from the .3d filename and requires it to
   start with a capital letter; the composer names its codecs [_leaf] / [_opt] /
   etc. Sanitise to a valid module name for the file (the internal struct names
   are unaffected and need no change). *)
let valid_module_name n =
  match String.concat "" (String.split_on_char '_' n) with
  | "" -> "Schema"
  | s -> String.capitalize_ascii s

(* Run one schema through [3d.exe --batch] in its own temp directory and return
   the verdict. The generated C is incidental and discarded: only EverParse's
   acceptance matters. [parse_3d ~batch:true] verifies without [run_everparse]'s
   endianness-header copy, so it works on a bare directory. *)
let rm_quietly f = try Sys.remove f with Sys_error _ -> ()
let err fmt = Fmt.kstr (fun s -> Error s) fmt

let extract_one g =
  match Wire.Everparse.schema (Fuzz_gen.codec g) with
  | exception e -> err "3D projection raised %s" (Printexc.to_string e)
  | schema0 ->
      let schema = { schema0 with name = valid_module_name schema0.name } in
      let outdir = Filename.temp_dir "wire_fuzz3d_" ("_" ^ schema.name) in
      Fun.protect
        ~finally:(fun () ->
          (try
             Array.iter
               (fun f -> rm_quietly (Filename.concat outdir f))
               (Sys.readdir outdir)
           with Sys_error _ -> ());
          try Unix.rmdir outdir with Unix.Unix_error _ -> ())
        (fun () ->
          try
            Wire_3d.generate_3d ~outdir [ schema ];
            Wire_3d.parse_3d ~batch:true ~outdir
              (Wire.Everparse.filename schema)
          with (Failure _ | Sys_error _ | Unix.Unix_error _) as e ->
            Error (Printexc.to_string e))

(* [3d.exe --batch] takes seconds per schema, so it cannot run inside Alcobar's
   repeat/timeout loop. Run each check once, eagerly, when this module loads, and
   let the test cases report the cached verdict instantly. Skipped without
   [3d.exe] (CI) and in corpus / AFL modes, where the long startup is unwanted. *)
let normal_mode () =
  let argv = Sys.argv in
  let n = Array.length argv in
  (not (Array.exists (String.equal "--gen-corpus") argv))
  && not (n > 1 && Sys.file_exists argv.(n - 1))

let extract_results =
  if not (Wire_3d.has_3d_exe () && normal_mode ()) then []
  else
    List.filter_map
      (fun (name, Fuzz_gen.Pack g) ->
        if List.mem name extract_names then Some (name, extract_one g) else None)
      Fuzz_gen.registry

let extract_cases =
  List.map
    (fun (name, res) ->
      Alcobar.test_case ("3d.exe " ^ name)
        [ const () ]
        (fun () ->
          match res with
          | Ok () -> ()
          | Error m ->
              Alcobar.failf "%s: EverParse rejected a schema that built: %s"
                name m))
    extract_results

let suite = ("everparse", pp_cases @ nested_pp_cases @ extract_cases)
