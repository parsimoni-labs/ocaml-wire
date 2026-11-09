(** Differential fuzzer: Wire's codecs against the OCaml {!Wire_ocaml} generates
    from them.

    The codecs are {!Diff_ocaml.codecs}: the ones the EverParse backend
    compares, and the flat records of a longer run of the same sample.
    {!Ocaml_index} holds the module generated for each one the generator
    accepts. On every input both sides must agree on decode and validate, error
    included, on the bytes encode writes, on every getter and on [Valid.v] with
    its reads, over [bytes] and over a bigarray. A codec the generator refuses
    must be one it documents it cannot specialise, or one whose field names
    would spell an OCaml name twice. *)

let refused =
  List.map
    (fun (label, msg) -> (label, List.assoc label Diff_ocaml.codecs, msg))
    Ocaml_index.refused

let cases =
  List.map
    (fun (label, make) ->
      let p = List.assoc label Diff_ocaml.codecs in
      (label, Diff_ocaml.check label p (make p)))
    Ocaml_index.covered

(* The report is the same on every run, so an AFL exec does without it. *)
let () =
  if
    (not (Fuzz_gen.afl_mode ()))
    && not (Diff_ocaml.report Ocaml_index.name refused)
  then exit 1;
  Diff_driver.run Ocaml_index.name cases
