(** Differential fuzzer: Wire's codecs against the OCaml {!Wire_ocaml} generates
    from them.

    The record modules are compared on {!Diff_ocaml.codecs}: the ones the
    EverParse backend compares, and the flat records of a longer run of the same
    sample. {!Ocaml_index} holds the module generated for each one the generator
    accepts. On every input both sides must agree on decode and validate, error
    included, on the bytes encode writes, on every getter and on [Valid.v] with
    its reads, over [bytes] and over a bigarray. A codec the generator refuses
    must be one it documents it cannot specialise, or one whose field names
    would spell an OCaml name twice.

    The scanners are compared on every sampled shape, fixed or not, and on the
    structured cases of {!Diff_codecs.scan_cases}. *)

let refused label = List.map (fun (l, msg) -> (l, List.assoc l label, msg))

let cases =
  List.map
    (fun (label, make) ->
      let p = List.assoc label Diff_ocaml.codecs in
      (label, Diff_ocaml.check label p (make p)))
    Ocaml_index.covered

let sampled =
  List.map
    (fun (label, make) ->
      let p = List.assoc label Diff_ocaml.scan_codecs in
      ( "scan " ^ label,
        Diff_ocaml.check_scan (Diff_ocaml.of_sampled label p) (make p) ))
    Ocaml_index.sampled_scanners

let structured =
  List.map
    (fun (Diff_codecs.Scan c as case) ->
      ( c.label,
        Diff_ocaml.check_scan
          (Diff_ocaml.of_scan_case case)
          ((List.assoc c.label Ocaml_index.scanners) ()) ))
    Diff_codecs.scan_cases

(* The reports are the same on every run, so an AFL exec does without them. *)
let () =
  if not (Fuzz_gen.afl_mode ()) then begin
    let fixed =
      Diff_ocaml.report Ocaml_index.name
        (refused Diff_ocaml.codecs Ocaml_index.refused)
    in
    let scanned =
      Diff_ocaml.scan_report Ocaml_index.name
        (refused Diff_ocaml.scan_codecs Ocaml_index.scan_refused)
    in
    if not (fixed && scanned) then exit 1
  end;
  Diff_driver.run Ocaml_index.name (cases @ sampled @ structured)
