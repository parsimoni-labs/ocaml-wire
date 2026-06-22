(** Differential AFL fuzzer: OCaml wire codec vs EverParse C validator, driven
    by the registry.

    For each codec EverParse could generate a validator for
    ({!Diff_index.covered}, looked up in {!Fuzz_gen.registry} by label), feed
    the AFL input to both the OCaml {!Wire.Codec.validate} and the generated C
    validator and flag any accept/reject divergence: both decoders must agree on
    which inputs are valid. Codecs the candidate filter dropped (variable-size,
    parameterised, casetype, or a shared-name duplicate of an already-included
    codec) are reported via {!Diff_codecs.excluded}. *)

open Fuzz_gen

(* The EverParse validator accepts iff the bytes parse (enough input, valid
   structure) and every refinement holds. [Codec.decode_exn] is exactly that:
   it decodes (length + structure) and validates (refinements), raising
   [Parse_error] / [Validation_error] on a clean rejection. (Plain [Codec.decode]
   returns a [result], so its rejection is easy to drop by mistake.) Any other
   exception surfaces as a crash, which is itself a divergence to report. *)
let ocaml_accepts (Pack g) b =
  match ignore (Wire.Codec.decode_exn (codec g) b 0) with
  | () -> true
  | exception (Wire.Validation_error _ | Wire.Parse_error _) -> false

(* Compare both decoders on one codec and flag any accept/reject divergence. *)
let diff_check label p c_check b =
  let o = ocaml_accepts p b and c = c_check b in
  if o <> c then
    Alcobar.failf "%s: OCaml accepts=%b but EverParse C accepts=%b" label o c

(* Each covered codec paired with both its decoders: the OCaml side ([Pack g],
   from {!Diff_codecs.included}) and the generated C validator ([c_check], from
   {!Diff_index.covered}). *)
let covered_cases =
  Array.to_list Diff_index.covered
  |> List.map (fun (label, c_check) ->
      (label, List.assoc label Diff_codecs.included, c_check))

(* Normal [dune test]: one case per covered codec, each on its own random bytes,
   so a single run touches every codec. *)
let registry_case (label, p, c_check) =
  Alcobar.test_case label [ Alcobar.bytes ] (fun buf ->
      diff_check label p c_check (Bytes.of_string buf))

(* AFL file-input mode: pick one codec per input (by the first byte) so each
   exec runs a single differential check rather than one per covered codec, with
   coverage of the whole set accumulating across inputs. Mirrors the shared
   {!Fuzz_gen.afl_cases} path the other fuzzers use. *)
let afl_case ?(max_len = 256) () =
  Alcobar.test_case "diff afl" [ bytes_any ] (fun bs ->
      let bs = truncate_bytes ~max_len bs in
      let label, p, c_check = pick_by_first_byte covered_cases bs in
      diff_check label p c_check bs)

let cases =
  if file_input_mode () then [ afl_case () ]
  else List.map registry_case covered_cases

let () =
  List.iter
    (fun (label, _, reason) ->
      Printf.eprintf "diff: skipping %s (%s)\n" label
        (Diff_codecs.string_of_exclusion reason))
    Diff_codecs.excluded;
  Alcobar.run "diff" [ ("diff", cases) ]
