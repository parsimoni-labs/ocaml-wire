(** Fuzz tests for 3D / EverParse projection, driven entirely by {!Fuzz_gen}.

    Every generated codec is projected to a 3D schema and pretty-printed
    ({!Fuzz_gen.everparse_cases}), so the projection and code-generation path is
    exercised on the same compositions the OCaml round-trip suite uses, plus
    arbitrary nested ones. When [3d.exe] is available {e and} [WIRE_3D_BATCH=1]
    is set, the {e whole} registry is additionally run through EverParse, once
    per projection mode: every codec that builds must project to a schema
    EverParse verifies as the standalone spec {e and} as the FFI schema, except
    the few in {!no_3d_projection} that wire rejects at projection and the known
    standalone-only gaps in {!no_3d_standalone} (both asserted to reject). That
    extraction pass is the only check that catches errors surfacing during F*
    verification rather than pretty-printing (a kind error on a possibly-empty
    list, an invalid action or expression). It is opt-in because it takes
    several minutes (too heavy for a plain [dune test]); the everparse-3d CI job
    sets [WIRE_3D_BATCH=1]. It is skipped where [3d.exe] is absent (the plain CI
    build, AFL) or the variable is unset. *)

open Alcobar

let err_c_compile fmt = Fmt.kstr (fun s -> Error s) fmt

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
   in [pp_cases]) must verify in both projection modes, or it is a
   build-but-fail-3d hole. Doing that lazily inside the test cases trips
   alcobar's per-test timeout, so the whole set is verified once at module load,
   one [.3d] module per schema, run concurrently (EverParse's own corpus-test
   strategy; the cost is F* verification, not startup).

   Two modes over a few hundred schemas takes several minutes, which is too
   heavy for a plain [dune test], so it is off by default and opt-in via
   [WIRE_3D_BATCH=1] (set in the everparse-3d CI job). It is also skipped
   without [3d.exe] and in corpus / AFL modes. *)
let batch_requested () = Sys.getenv_opt "WIRE_3D_BATCH" = Some "1"

(* A distinct, valid 3d module name per schema (one [.3d] file each), keeping the
   registry label and the projection mode in it, so a batch failure points back
   to the codec and says which of its two projections broke. The mode tag is
   what keeps the two sweeps below from writing one file twice. *)
let batch_module_name i tag label =
  let b = Buffer.create (String.length label + 8) in
  Buffer.add_string b ("S" ^ string_of_int i ^ tag ^ "_");
  String.iter
    (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
      then Buffer.add_char b c)
    label;
  Buffer.contents b

let batch_enabled () =
  Wire_3d.has_3d_exe () && normal_mode () && batch_requested ()

(* Shapes whose FFI projection verifies but whose standalone rendering does
   not. Keep each in the registry and assert its standalone rejection below, so
   the exception cannot hide a new gap or outlive the projection bug that
   requires it. *)
let no_3d_standalone = [ "casetype_where_tag"; "action"; "action_on_act" ]

(* Every registry entry [keep] admits, projected under [mode], each schema named
   apart from its sibling in the other mode. *)
let sweep_schemas ~mode ~tag keep =
  if not (batch_enabled ()) then []
  else
    Fuzz_gen.registry
    |> List.filter (fun (name, _) ->
        (not (List.mem name no_3d_projection)) && keep name)
    |> List.mapi (fun i (label, Fuzz_gen.Pack g) ->
        let s = Wire.Everparse.project ~mode (Fuzz_gen.codec g) in
        { s with Wire.Everparse.name = batch_module_name i tag label })

(* Each mode is swept through the writer that actually ships it, since the two
   writers do not render the same schema the same way: the [`Ffi] one emits a
   file per schema and spells an enum field as its base type plus a membership
   refinement, the [`Standalone] one merges a family into one spec and spells it
   as the named 3D enum type. Running a [`Standalone] projection through the FFI
   writer would verify a file nothing ever emits. [Wire_3d.batch_check] is the
   FFI writer already, so [`Ffi] uses it and [`Standalone] gets the same
   one-module-per-schema treatment over [Everparse.write ~mode:`Standalone],
   which is what [Wire_3d.generate_3d_standalone] calls. *)
let ffi_check schemas =
  let outdir = Filename.temp_dir "wire_fuzz3d_ffi" "" in
  Fun.protect
    ~finally:(fun () -> Wire_3d.rm_rf outdir)
    (fun () -> Wire_3d.batch_check ~outdir schemas)

let standalone_check schemas =
  let outdir = Filename.temp_dir "wire_fuzz3d_std" "" in
  Fun.protect
    ~finally:(fun () -> Wire_3d.rm_rf outdir)
    (fun () ->
      let arr = Array.of_list schemas in
      let log_of i = Filename.concat outdir (string_of_int i ^ ".log") in
      let jobs =
        Array.mapi
          (fun i (s : Wire.Everparse.t) () ->
            (* Concurrent [3d.exe] runs race on EverParse's shared intermediate
               files, so each job gets a directory of its own. *)
            let work = Filename.temp_dir "wire_fuzz3d_stdjob" "" in
            Fun.protect
              ~finally:(fun () -> Wire_3d.rm_rf work)
              (fun () ->
                Wire.Everparse.write ~mode:`Standalone ~outdir:work ~name:s.name
                  [ s ];
                match
                  Wire_3d.parse_3d ~batch:true ~outdir:work
                    (Wire.Everparse.filename s)
                with
                | Ok () -> ()
                | Error m ->
                    Out_channel.with_open_text (log_of i) (fun oc ->
                        Out_channel.output_string oc m);
                    failwith "EverParse rejected"))
          arr
      in
      let max_jobs = max 1 (min 4 (Domain.recommended_domain_count ())) in
      let errors =
        Wire_3d.fork_pool ~max_jobs jobs
        |> Array.to_list
        |> List.mapi (fun i passed ->
            if passed then None
            else
              let msg =
                try In_channel.with_open_text (log_of i) In_channel.input_all
                with Sys_error _ -> ""
              in
              Fmt.kstr
                (fun s -> Some s)
                "%s:\n%s" arr.(i).Wire.Everparse.name msg)
        |> List.filter_map Fun.id
      in
      match errors with [] -> Ok () | _ -> Error (String.concat "\n" errors))

let run check = function [] -> None | schemas -> Some (check schemas)

(* One sweep: its mode's name, the schemas it covers, and EverParse's verdict on
   them ([None] when the sweep is off). *)
let sweep ~mode ~tag ~check ~name keep =
  let schemas = sweep_schemas ~mode ~tag keep in
  (name, schemas, run check schemas)

let sweeps =
  [
    sweep ~mode:`Standalone ~tag:"S" ~check:standalone_check ~name:"standalone"
      (fun name -> not (List.mem name no_3d_standalone));
    sweep ~mode:`Ffi ~tag:"F" ~check:ffi_check ~name:"ffi" (Fun.const true);
  ]

let refused_sweeps =
  [
    sweep ~mode:`Standalone ~tag:"XS" ~check:standalone_check ~name:"standalone"
      (fun name -> List.mem name no_3d_standalone);
  ]

let verdict_case name schemas result ~expected =
  match result with
  | None -> []
  | Some res ->
      [
        Alcobar.test_case
          (Fmt.str "3d.exe %s %s (%d schemas)"
             (if expected then "accepts" else "rejects")
             name (List.length schemas))
          [ const () ]
          (fun () ->
            match (res, expected) with
            | Ok (), true | Error _, false -> ()
            | Error m, true ->
                Alcobar.failf "EverParse rejected a generated schema:\n%s" m
            | Ok (), false ->
                Alcobar.failf
                  "EverParse now accepts %s; drop it from [no_3d_standalone]"
                  (String.concat ", " no_3d_standalone));
      ]

let extract_cases =
  List.concat_map
    (fun (name, schemas, res) -> verdict_case name schemas res ~expected:true)
    sweeps
  @ List.concat_map
      (fun (name, schemas, res) ->
        verdict_case name schemas res ~expected:false)
      refused_sweeps

(* {1 The generated C compiles}

   [3d.exe --batch] verifying a schema says nothing about the C wire ships.
   After EverParse writes the parser, [Wire_3d.generate_c] rewrites the
   [<Base>CheckWire<Codec>] wrapper's success tail to demand full consumption,
   and that rewrite is textual: it splices in a comparison naming a variable
   EverParse chose. When a future EverParse names it something else, the splice
   still matches the surrounding tail, the C stops compiling, and nothing in the
   sweep notices, because the sweep stops at verification. So take a handful of
   registry shapes the whole way: generate the standalone spec, run EverParse
   over it, and compile the parser and its hardened wrapper with the same strict
   flags the shipped dune rule uses.

   A handful, not the registry: each one is a full EverParse C generation, and
   the wrapper shape follows the entrypoint's kind (plain, parameterized,
   variable-size, tag-dispatched), not the leaf types under it. *)

let c_compile_labels =
  [
    "uint32be";
    "record";
    "casetype_u8";
    "subcodec_last(record)";
    "repeat(8,uint8)";
    "zeroterm";
    "param_size";
  ]

(* The [.3d] the standalone writer just produced, found by listing rather than by
   recomputing the base, so the module name stays [Wire_3d]'s business. *)
let sole_3d_base outdir =
  Sys.readdir outdir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".3d")

let shell cmd =
  let ic = Unix.open_process_in (cmd ^ " 2>&1") in
  let out = In_channel.input_all ic in
  (out, Unix.close_process_in ic)

let c_compiles (Fuzz_gen.Pack g) =
  let outdir = Filename.temp_dir "wire_fuzz3d_cc" "" in
  Fun.protect
    ~finally:(fun () -> Wire_3d.rm_rf outdir)
    (fun () ->
      let name = "fuzzcc" in
      Wire_3d.generate_standalone ~outdir ~name ~package:name
        [ Wire_3d.pack (Fuzz_gen.codec g) ];
      match sole_3d_base outdir with
      | [ file ] -> (
          let base = Filename.remove_extension file in
          let out, status =
            Fmt.kstr shell "cd %s && cc %s %s -c %s.c %sWrapper.c"
              (Filename.quote outdir) Wire_3d.strict_cc_flags
              Wire_3d.everparse_type_defines base base
          in
          match status with Unix.WEXITED 0 -> Ok () | _ -> Error out)
      | files ->
          err_c_compile "expected one .3d in %s, found [%s]" outdir
            (String.concat "; " files))

(* Run at module load, like the sweeps and for the same reason: a full EverParse
   C generation inside a test body outlasts alcobar's per-test timeout. The case
   only reports what the run already found. *)
let c_compile_results =
  if not (batch_enabled ()) then []
  else
    Fuzz_gen.registry
    |> List.filter (fun (label, _) -> List.mem label c_compile_labels)
    |> List.map (fun (label, packed) -> (label, c_compiles packed))

let c_compile_cases =
  List.map
    (fun (label, res) ->
      Alcobar.test_case ("cc " ^ label)
        [ const () ]
        (fun () ->
          match res with
          | Ok () -> ()
          | Error out ->
              Alcobar.failf "%s: the generated C does not compile:\n%s" label
                out))
    c_compile_results

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
    (* A per-byte refinement is not one of the constraint forms
       [Everparse.Raw.field_seeds] names a value for, so nothing seeds the
       accepting side and all 24 lines come out rejected. The flat
       [byte_array_where(4)] is in the same position and stays out of this list
       only by luck: it draws no seed either, and lands exactly one accepted
       line out of 24 because the fixed corpus RNG happens to roll four
       printable bytes. Seeding a refined span would remove both. *)
    "nested(4,byte_array_where4)";
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

(* {1 What a corpus line carries beyond its verdict}

   Each line pairs an env with a buffer the codec accepts or rejects, so both
   sides answer more than the verdict. An accepted buffer has to agree with
   what the codec says it would encode, and a rejected one has to say which
   field went wrong. Both were places a wrong answer arrived silently: sizing
   with an unbound param read 0, so a param-sized field measured as empty, and
   an unmatched tag arrived with no field path at all. *)

(* Re-encoding an accepted record preserves it. Compared through the
   generator's own equality rather than byte-wise, since a padded region
   re-encodes to different bytes for the same value. *)
let reencodes g codec env v sz =
  let buf = Bytes.create sz in
  match Wire.Codec.encode ?env codec v buf 0 with
  | exception Wire.Parse_error e ->
      Fmt.kstr (fun s -> Some s) "re-encoding raised %a" Wire.pp_parse_error e
  | () -> (
      match Wire.Codec.decode ?env codec buf 0 with
      | Error e ->
          Fmt.kstr
            (fun s -> Some s)
            "re-encoded bytes no longer decode: %a" Wire.pp_parse_error e
      | Ok v' ->
          if Fuzz_gen.equal g v v' then None
          else Some "re-encoding does not preserve the value")

(* An accepted record: the size agrees with the buffer it was read from, and
   the value survives a round trip. Sizing with an unbound param used to read
   0, so a param-sized field measured as empty and a caller sizing a buffer
   from the answer got one too short. *)
let accepted_agrees g codec env b =
  match Wire.Codec.decode ?env codec b 0 with
  | Error _ -> None
  | Ok v -> (
      let len = Bytes.length b in
      match Wire.Codec.size_of_value ?env codec v with
      | exception Invalid_argument m ->
          Fmt.kstr
            (fun s -> Some s)
            "size_of_value refused an accepted record: %s" m
      | sz when not (Int.equal sz len) ->
          Fmt.kstr
            (fun s -> Some s)
            "size_of_value answers %d for a %d byte record" sz len
      | sz -> reencodes g codec env v sz)

(* A rejected record: the offset is a real position, and a failure about one
   field's value names that field. A path may legitimately be empty at a
   top-level or anonymous position, so only the kinds that always come from a
   named field are required to carry one. The offset has to be a position in
   the buffer: a span sized past the end used to leave the failure to whichever
   later field had a check, reported where the overrun had already carried
   to. *)
let rejection_is_attributed codec env b =
  let len = Bytes.length b in
  match Wire.Codec.decode ?env codec b 0 with
  | Ok _ -> None
  | Error e -> (
      if e.Wire.at < 0 || e.Wire.at > len then
        Fmt.kstr
          (fun s -> Some s)
          "reports offset %d outside a %d byte buffer, for %a at [%s]" e.Wire.at
          len Wire.pp_error_kind e.Wire.kind
          (String.concat "/" e.Wire.field)
      else
        match e.Wire.kind with
        | (Wire.Invalid_tag _ | Wire.Invalid_enum _ | Wire.Value_out_of_range _)
          when List.compare_length_with e.Wire.field 0 = 0 ->
            Fmt.kstr
              (fun s -> Some s)
              "blames no field for %a" Wire.pp_error_kind e.Wire.kind
        | _ -> None)

let corpus_property_case (label, Fuzz_gen.Pack g) =
  let codec = Fuzz_gen.codec g in
  Alcobar.test_case
    ("corpus properties " ^ label)
    [ const () ]
    (fun () ->
      match corpus_lines codec with
      | exception Failure _ -> ()
      | lines ->
          let pnames = input_params codec in
          List.iter
            (fun (pvals, hex, verdict) ->
              let env = env_of codec pnames pvals in
              let b = bytes_of_hex hex in
              let checks =
                if verdict then [ accepted_agrees g codec env b ]
                else [ rejection_is_attributed codec env b ]
              in
              List.iter
                (function
                  | None -> ()
                  | Some msg -> Alcobar.failf "%s: %s (on %s)" label msg hex)
                checks)
            lines)

let corpus_property_cases () =
  if not (normal_mode ()) then []
  else
    Fuzz_gen.registry
    |> List.filter (fun (name, _) -> not (List.mem name no_3d_projection))
    |> List.map corpus_property_case

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
      pp_cases () @ nested_pp_cases () @ corpus_cases ()
      @ corpus_property_cases () @ extract_cases @ c_compile_cases )
