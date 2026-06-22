(** Fuzz tests for 3D / EverParse projection, driven entirely by {!Fuzz_gen}.

    Every generated codec is projected to a 3D schema and pretty-printed
    ({!Fuzz_gen.everparse_cases}), so the projection and code-generation path is
    exercised on the same compositions the OCaml round-trip suite uses, plus
    arbitrary nested ones. When [3d.exe] is available, the {e whole} registry is
    additionally run through EverParse ([3d.exe --batch]): every codec that
    builds must also project to a schema EverParse verifies, except the few in
    {!no_3d_projection} that wire rejects at projection (asserted to reject
    instead). That extraction pass is the only check that catches errors
    surfacing during F* verification rather than pretty-printing (a kind error
    on a possibly-empty list, an invalid action or expression). It is skipped
    where [3d.exe] is absent (the plain CI build, AFL). *)

open Alcobar

(* Shapes wire rejects at projection: they use a construct with no 3D form, so
   [to_3d] raises a clear [Invalid_argument]. [expr_ops] uses a negative integer
   literal (3D has no negative literals); [sizeof] uses [field_pos] (no 3D
   keyword). Every other operator they exercise (shifts, bitwise, casts, mod,
   div, comparisons, logic, [sizeof(this)], [sizeof(<type>)]) does project and is
   covered by the rest of the registry. These are the "reject with a clear
   error" half of closing the gaps: asserted to reject (below), not swept through
   [3d.exe]. *)
let no_3d_projection = [ "expr_ops"; "sizeof" ]

let to_3d_of g =
  Wire.Everparse.Raw.to_3d (Wire.Everparse.schema (Fuzz_gen.codec g)).module_

(* For a shape with no 3D projection: [to_3d] must raise [Invalid_argument] with
   a clear message, not produce 3D EverParse would reject. *)
let rejection_case name g =
  [
    Alcobar.test_case
      ("rejects projection " ^ name)
      [ const () ]
      (fun () ->
        match to_3d_of g with
        | _ ->
            Alcobar.failf
              "%s: expected projection to be rejected, but it succeeded" name
        | exception Invalid_argument _ -> ());
  ]

(* Projection + pretty-print coverage: cheap, no subprocess, always runs. A
   shape with no 3D projection is asserted to reject; every other shape must
   project and pretty-print. *)
let pp_cases () =
  List.concat_map
    (fun (name, Fuzz_gen.Pack g) ->
      if List.mem name no_3d_projection then rejection_case name g
      else Fuzz_gen.everparse_cases name g)
    Fuzz_gen.registry

let nested_pp_cases () =
  Fuzz_gen.everparse_nested_cases "nested(d2)" 2
  @ Fuzz_gen.everparse_nested_cases "nested(d3)" 3
  @ Fuzz_gen.everparse_nested_cases "nested(d4)" 4

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
  (not (Fuzz_gen.corpus_generation_mode ()))
  && not (Fuzz_gen.file_input_mode ())

(* Every shape that projects must verify: the whole registry except the shapes
   wire rejects at projection (those are asserted to reject in [pp_cases]) is
   run through [3d.exe], and any failure is a build-but-fail-3d hole. *)
let extract_results () =
  if not (Wire_3d.has_3d_exe () && normal_mode ()) then []
  else
    Fuzz_gen.registry
    |> List.filter (fun (name, _) -> not (List.mem name no_3d_projection))
    |> List.map (fun (name, Fuzz_gen.Pack g) -> (name, extract_one g))

let extract_cases () =
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
    (extract_results ())

let suite =
  if Fuzz_gen.file_input_mode () then
    ("everparse", Fuzz_gen.afl_everparse_cases "everparse")
  else ("everparse", pp_cases () @ nested_pp_cases () @ extract_cases ())
