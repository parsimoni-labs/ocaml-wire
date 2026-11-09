(** Differential fuzzer: Wire's codecs against the Rust {!Wire_rust} generates
    from them.

    The codecs are {!Diff_ocaml.codecs}, the ones the OCaml backend compares.
    {!Rust_index} lists the module generated for each one the generator accepts,
    all compiled into one [no_std] static library. On every input both sides
    must agree on decode and validate, on every getter, on the bytes encode and
    the setters write, and on [Valid::new] with its reads. A codec the generator
    refuses must be one it documents it cannot specialise, or one whose field
    names would spell a Rust name twice. *)

let refused =
  List.map
    (fun (label, msg) -> (label, List.assoc label Diff_ocaml.codecs, msg))
    Rust_index.refused

let cases =
  List.map
    (fun (label, case) ->
      let p = List.assoc label Diff_ocaml.codecs in
      (label, Diff_rust.check label p case))
    Rust_index.covered

(* The report is the same on every run, so an AFL exec does without it. *)
let () =
  if
    (not (Fuzz_gen.afl_mode ()))
    && not (Diff_ocaml.report Rust_index.name refused)
  then exit 1;
  Diff_driver.run Rust_index.name cases
