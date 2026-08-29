(** Fuzz tests for 3D / EverParse projection, driven entirely by {!Fuzz_gen}.

    Every generated codec is projected to a 3D schema and pretty-printed
    ({!Fuzz_gen.everparse_cases}), so the projection and code-generation path is
    exercised on the same compositions the OCaml round-trip suite uses, plus
    arbitrary nested ones. When [3d.exe] is available {e and} [WIRE_3D_BATCH=1]
    is set, the {e whole} registry is additionally run through EverParse (one
    [3d.exe --batch] over all the generated files): every codec that builds must
    also project to a schema EverParse verifies, except the few in
    {!no_3d_projection} that wire rejects at projection (asserted to reject
    instead). That extraction pass is the only check that catches errors
    surfacing during F* verification rather than pretty-printing (a kind error
    on a possibly-empty list, an invalid action or expression). It is opt-in
    because it takes ~90s (too heavy for a plain [dune test]); the everparse-3d
    CI job sets [WIRE_3D_BATCH=1]. It is skipped where [3d.exe] is absent (the
    plain CI build, AFL) or the variable is unset. *)

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
  Wire.Everparse.Raw.to_3d
    (Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g)).module_

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

let normal_mode () =
  (not (Fuzz_gen.corpus_generation_mode ()))
  && not (Fuzz_gen.file_input_mode ())

(* Adversarially test the generated 3d files: every shape in the registry that
   wire claims projects (all but the {!no_3d_projection} ones, asserted to reject
   in [pp_cases]) must verify, or it is a build-but-fail-3d hole. Running one
   [3d.exe --batch] per schema is far too slow for the expanded, adversarially
   biased registry (hundreds of schemas, each paying EverParse's F* / KaRaMeL
   startup), and doing it lazily inside the test cases also trips alcobar's
   per-test timeout. Instead, the whole set is validated in a single
   [3d.exe --batch] over all the generated files (EverParse's own corpus-test
   strategy), run once at module load.

   This still takes ~90s, which is too heavy for a plain [dune test], so it is
   off by default and opt-in via [WIRE_3D_BATCH=1] (set in the everparse-3d CI
   job). It is also skipped without [3d.exe] and in corpus / AFL modes. *)
let batch_requested () = Sys.getenv_opt "WIRE_3D_BATCH" = Some "1"

(* A distinct, valid 3d module name per schema (one [.3d] file each), keeping the
   registry label in it so a batch failure points back to the codec. *)
let batch_module_name i label =
  let b = Buffer.create (String.length label + 8) in
  Buffer.add_string b ("S" ^ string_of_int i ^ "_");
  String.iter
    (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
      then Buffer.add_char b c)
    label;
  Buffer.contents b

let batch_schemas =
  if not (Wire_3d.has_3d_exe () && normal_mode () && batch_requested ()) then []
  else
    Fuzz_gen.registry
    |> List.filter (fun (name, _) -> not (List.mem name no_3d_projection))
    |> List.mapi (fun i (label, Fuzz_gen.Pack g) ->
        let s = Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g) in
        { s with Wire.Everparse.name = batch_module_name i label })

let batch_result =
  match batch_schemas with
  | [] -> None
  | schemas ->
      let outdir = Filename.temp_dir "wire_fuzz3d_batch" "" in
      Some (Wire_3d.batch_check ~outdir schemas)

let extract_cases =
  match batch_result with
  | None -> []
  | Some res ->
      [
        Alcobar.test_case
          (Fmt.str "3d.exe batch (%d schemas)" (List.length batch_schemas))
          [ const () ]
          (fun () ->
            match res with
            | Ok () -> ()
            | Error m ->
                Alcobar.failf "EverParse rejected a generated schema:\n%s" m);
      ]

(* {1 Corpus oracle}

   [Wire_3d.generate_corpus] is the differential's oracle: the verdict column it
   writes is what the generated C validator is checked against. Nothing checked
   the oracle itself, so a wrong verdict did not refuse, it disagreed, and the
   disagreement was reported against the validator rather than against the
   harness. Sweep the registry through it and replay every line against the
   codec it claims to speak for.

   The replay is written out here rather than reusing [Wire_3d]'s own verdict,
   so the check does not lean on the code it is checking. It answers the same
   question: does the record decode, validate, and span exactly the bytes on the
   line, under that line's parameters. *)

let corpus_count = 24

let bytes_of_hex h =
  if String.equal h "-" then Bytes.empty
  else
    Bytes.init
      (String.length h / 2)
      (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

(* [name params hex verdict] per line; [params] is "-" or comma separated. *)
let corpus_lines codec =
  let buf = Buffer.create 4096 in
  let ppf = Fmt.with_buffer buf in
  Wire_3d.generate_corpus ~count:corpus_count ppf [ Wire_3d.pack codec ];
  Format.pp_print_flush ppf ();
  Buffer.contents buf |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match String.split_on_char ' ' line with
      | [ _; params; hex; verdict ] ->
          let pvals =
            if String.equal params "-" then []
            else List.map int_of_string (String.split_on_char ',' params)
          in
          Some (pvals, hex, String.equal verdict "1")
      | _ -> None)

let input_params codec =
  match (Wire.Everparse.project ~mode:`Standalone codec).source with
  | Some source -> Wire.Everparse.Raw.input_param_names source
  | None -> []

let env_of codec pnames pvals =
  match pnames with
  | [] -> None
  | _ ->
      Some
        (List.fold_left2
           (fun env name value -> Wire.Param.bind_by_name name value env)
           (Wire.Codec.env codec) pnames pvals)

let record_spans codec env b =
  match Wire.Codec.decode ?env codec b 0 with
  | Error _ -> false
  | Ok _ -> (
      match
        Wire.Codec.validate ?env codec b 0;
        Wire.Codec.wire_size_at ?env codec b 0
      with
      | n -> Int.equal n (Bytes.length b)
      | exception Wire.Parse_error _ -> false)

(* Shapes whose accepting side the seeder cannot construct, so
   [generate_corpus] refuses rather than emit a corpus that says the same thing
   about every input. Pinned so the set stays visible: a shape that newly stops
   being seedable is a regression, and one that becomes seedable has to be
   dropped from here. Vacuity is the seeder reporting that it cannot build a
   record, which is exactly how the casetype-tag and length-field gaps hid, as
   an absence rather than a failure. *)
let no_corpus_seed =
  [
    (* Accepts every buffer it is given, so there is no rejecting side to
       reach. One-sided by nature rather than by omission, and the only entry
       here that a better seeder could not remove. *)
    "all_bytes";
  ]

let corpus_oracle_case (label, Fuzz_gen.Pack g) =
  let codec = Fuzz_gen.codec g in
  Alcobar.test_case ("corpus oracle " ^ label)
    [ const () ]
    (fun () ->
      match corpus_lines codec with
      | exception Failure msg ->
          if not (List.mem label no_corpus_seed) then
            Alcobar.failf "%s: generate_corpus refused a corpus: %s" label msg
      | lines ->
          if List.mem label no_corpus_seed then
            Alcobar.failf "%s: is seedable now; drop it from [no_corpus_seed]"
              label;
          let pnames = input_params codec in
          List.iter
            (fun (pvals, hex, verdict) ->
              let env = env_of codec pnames pvals in
              let b = bytes_of_hex hex in
              let spans = record_spans codec env b in
              if not (Bool.equal spans verdict) then
                Alcobar.failf
                  "%s: corpus records %b for params [%s] %s, the codec answers \
                   %b"
                  label verdict
                  (String.concat "," (List.map string_of_int pvals))
                  hex spans)
            lines)

let corpus_cases () =
  if not (normal_mode ()) then []
  else
    Fuzz_gen.registry
    |> List.filter (fun (name, _) -> not (List.mem name no_3d_projection))
    |> List.map corpus_oracle_case

let suite =
  if Fuzz_gen.file_input_mode () then
    ("everparse", Fuzz_gen.afl_everparse_cases "everparse")
  else
    ( "everparse",
      pp_cases () @ nested_pp_cases () @ corpus_cases () @ extract_cases )
