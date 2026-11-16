(** Differential fuzzer: Wire's codecs against the Rust {!Wire_rust} generates
    from them.

    The codecs are {!Diff_ocaml.codecs}, the ones the OCaml backend compares.
    {!Rust_index} lists the module generated for each one the generator accepts,
    all compiled into one [no_std] static library. On every input both sides
    must agree on decode and validate, on every getter, on the bytes encode and
    the setters write, and on [Valid::at] with its reads, at offsets from 0 to
    past the end of the buffer. The library is compiled with overflow checks, so
    arithmetic that would wrap aborts the run. A codec the generator refuses
    must be one it documents it cannot specialise, or one whose field names
    would spell a Rust name twice.

    The scanners are compared as the OCaml backend's are: on the structured
    cases of {!Diff_codecs.scan_cases}, with every output, every other one and
    none, on each case's boundary corpus and on random inputs, with the
    EverParse C validator too when it is built; on scanned schemas of
    test/ocaml, whose arithmetic overflows the native int or whose decision tree
    shares a subtree; and on every sampled shape the OCaml scanner accepts, with
    every output and every other one. Each must accept exactly what [Codec]
    decodes on a frame and return the same scalar fields. *)

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

let structured =
  List.map
    (fun (Diff_codecs.Scan c as case) ->
      ( case,
        List.map Diff_rust.scan_ops (List.assoc c.label Rust_index.scanners) ))
    Diff_codecs.scan_cases

let scans =
  List.map
    (fun ((Diff_codecs.Scan c as case), ops) ->
      ( "scan " ^ c.label,
        Diff_ocaml.check_scan
          ?oracle:(Diff_driver.oracle c.label)
          (Diff_ocaml.of_scan_case case)
          ops ))
    structured

(* Scanned schemas of test/ocaml, each with its boundary corpus. In [sized],
   products of a 32-bit [n] overflow the native int for a large [n]: an
   optional slice's condition and size. In [overflow], checks add to a field
   near the largest int and the where clause multiplies two 32-bit fields.
   The codec rejects such a record, and so must the scanner, not test a
   wrapped value. In [shared], the decision tree of three optional fields
   reaches a test from two paths. In [skipped], a condition the facts decide
   still overflows where the codec evaluates it. *)
let reference label codec project =
  Diff_ocaml.Reference
    { label; codec; known = None; project; inputs = (fun b -> [ b ]) }

(* [n], then each of [rests]: the values around the overflow of [n * n], and
   of [n * n * 4]. *)
let wrapped rests =
  List.concat_map
    (fun n ->
      List.map
        (fun rest ->
          let b = Bytes.create (4 + String.length rest) in
          Bytes.set_int32_be b 0 n;
          Bytes.blit_string rest 0 b 4 (String.length rest);
          b)
        rests)
    [ 0l; 1l; 2l; 3l; 6l; 46341l; 0x5a827999l; 0x7fffffffl; -1l ]

let sizes =
  wrapped [ ""; "\001"; "\001\002"; "abcdefghij"; String.make 40 '\255' ]

(* [x] picks the optional field whose condition skips [n * n]. *)
let skips =
  wrapped
    (List.concat_map
       (fun x -> [ x; x ^ "\007"; x ^ "\007\008" ])
       [ "\003"; "\004"; "\005" ])

(* Every [a], [b] and [c] the conditions test, then up to two bytes. *)
let tags =
  List.concat_map
    (fun i ->
      List.map
        (fun rest ->
          Fmt.kstr Bytes.of_string "%c%c%c%s"
            (Char.chr (i mod 4))
            (Char.chr (i / 4 mod 4))
            (Char.chr (i / 16))
            rest)
        [ ""; "\007"; "\007\008" ])
    (List.init 48 Fun.id)

let schemas =
  let ops label =
    List.map Diff_rust.scan_ops (List.assoc label Rust_index.schema_scanners)
  in
  let int8 = Wire.UInt8.to_int in
  let opt name v =
    [
      (name ^ "_present", if Option.is_some v then 1 else 0);
      (name, Option.fold ~none:0 ~some:int8 v);
    ]
  in
  [
    ( reference "sized" Schemas.sized (fun _ (n, body, tail) ->
          [
            ("n", Wire.UInt32.to_int n);
            ("body_present", if Option.is_some body then 1 else 0);
            ("tail", int8 tail);
          ]),
      ops "sized",
      sizes );
    ( reference "overflow" Schemas.overflow (fun _ (a, hi, lo, b, c) ->
          [
            ("a", Wire.UInt32.to_int a);
            ("hi", hi);
            ("lo", lo);
            ("b", Wire.UInt32.to_int b);
            ("c", Wire.UInt32.to_int c);
          ]),
      ops "overflow",
      sizes );
    ( reference "shared" Schemas.shared (fun _ (r : Schemas.shared) ->
          [ ("a", int8 r.a); ("b", int8 r.b); ("c", int8 r.c) ]
          @ opt "x" r.x @ opt "y" r.y @ opt "z" r.z),
      ops "shared",
      tags );
    ( reference "skipped" Schemas.skipped (fun _ (n, x, a, b) ->
          [ ("n", Wire.UInt32.to_int n); ("x", int8 x) ] @ opt "a" a @ opt "b" b),
      ops "skipped",
      skips );
  ]

let sampled =
  List.map
    (fun (label, scans) ->
      let p = List.assoc label Diff_ocaml.scan_codecs in
      ( "scan " ^ label,
        Diff_ocaml.check_scan
          (Diff_ocaml.of_sampled label p)
          (List.map Diff_rust.scan_ops scans) ))
    Rust_index.sampled_scanners

(* The report and each case's boundary corpus are the same on every run, so
   an AFL exec does without them. *)
let () =
  if not (Fuzz_gen.afl_mode ()) then begin
    if not (Diff_ocaml.report Rust_index.name refused) then exit 1;
    List.iter
      (fun ((Diff_codecs.Scan c as case), ops) ->
        Diff_ocaml.scan_corpus ?oracle:(Diff_driver.oracle c.label) case ops)
      structured;
    List.iter
      (fun (reference, ops, corpus) ->
        List.iter (Diff_ocaml.check_scan reference ops) corpus)
      schemas
  end;
  let schemas =
    List.map
      (fun ((Diff_ocaml.Reference r as reference), ops, _) ->
        ("scan " ^ r.label, Diff_ocaml.check_scan reference ops))
      schemas
  in
  Diff_driver.run Rust_index.name (cases @ scans @ schemas @ sampled)
