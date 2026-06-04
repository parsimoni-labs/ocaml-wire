(** Differential AFL fuzzer: OCaml wire codec vs EverParse C validator, driven
    by the registry.

    For each codec EverParse could generate a validator for
    ({!Diff_index.covered}, looked up in {!Fuzz_gen.registry} by label), feed
    the AFL input to both the OCaml {!Wire.Codec.validate} and the generated C
    validator and flag any accept/reject divergence: both decoders must agree on
    which inputs are valid. Codecs the candidate filter dropped (variable-size,
    parameterised, casetype, or a shared-name duplicate of an already-included
    codec) are reported via {!Diff_codecs.excluded}. *)

(* The EverParse validator accepts iff the bytes parse (enough input, valid
   structure) and every refinement holds. [Codec.decode_exn] is exactly that:
   it decodes (length + structure) and validates (refinements), raising
   [Parse_error] / [Validation_error] on a clean rejection. (Plain [Codec.decode]
   returns a [result], so its rejection is easy to drop by mistake.) Any other
   exception surfaces as a crash, which is itself a divergence to report. *)
let ocaml_accepts (Fuzz_gen.Pack g) b =
  match ignore (Wire.Codec.decode_exn (Fuzz_gen.codec g) b 0) with
  | () -> true
  | exception (Wire.Validation_error _ | Wire.Parse_error _) -> false

let case (label, c_check) =
  let p = List.assoc label Diff_codecs.included in
  Alcobar.test_case label [ Alcobar.bytes ] (fun buf ->
      let b = Bytes.of_string buf in
      let o = ocaml_accepts p b and c = c_check b in
      if o <> c then
        Alcobar.failf "%s: OCaml accepts=%b but EverParse C accepts=%b" label o
          c)

let () =
  List.iter
    (fun (label, _, reason) ->
      Printf.eprintf "diff: skipping %s (%s)\n" label
        (Diff_codecs.string_of_exclusion reason))
    Diff_codecs.excluded;
  Alcobar.run "diff"
    [ ("diff", Array.to_list Diff_index.covered |> List.map case) ]
