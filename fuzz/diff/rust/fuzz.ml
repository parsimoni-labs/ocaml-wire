(** Differential fuzzer: Wire's codecs against the Rust {!Wire_rust} generates
    from them.

    {!Rust_index} lists the module generated for each codec, all compiled into
    one [no_std] static library. Each module is compared as the OCaml backend
    compares the OCaml module of the same codec: the folds of its modules of
    every field, of every other one and, for a structured case, of none, through
    one frame, the slots of a stride and an index; its check with every
    accessor; and its values, decode written back by encode in the bytes size
    measures, and validate, errors exactly. The structured cases of
    {!Diff_codecs.scan_cases} run on their boundary corpus and on random inputs,
    with the EverParse C validator too when it is built; scanned schemas of
    test/ocaml, whose arithmetic overflows the native int or whose decision tree
    shares a subtree, on theirs; and every sampled shape the OCaml scanner
    accepts on random inputs. A record of constant size of {!Diff_ocaml.codecs}
    is also compared at offsets from 0 to past the end of the buffer: validate
    and decode, every accessor, encode into buffers around the record and the
    setters; and records of [Schemas.checked] that break its constraints or its
    [where] clause are encoded by both, errors compared exactly. The library is
    compiled with overflow checks, so arithmetic that would wrap aborts the run.
    A codec the generator refuses must be one it documents it cannot specialise,
    or one whose field names would spell a Rust name twice. *)

let backends (type r) (c : r Wire.Codec.t) m =
  Diff_rust.folds m @ [ Diff_rust.views c m; Diff_rust.values c m ]

let structured =
  List.filter_map
    (fun (Diff_codecs.Scan c as case) ->
      Option.map
        (fun m -> (case, backends c.codec m))
        (List.assoc_opt c.label Rust_index.structured))
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
   products of the high half of a 32-bit [n] fill 32 bits: an optional slice's
   condition and size. In [overflow], checks add to a field near the largest
   int. In [shared], the decision tree of three optional fields reaches a test
   from two paths. In [skipped], a condition the facts decide is still
   evaluated where the codec evaluates it. *)
let reference label codec project =
  Diff_ocaml.Reference
    { label; codec; known = None; project; inputs = (fun b -> [ b ]) }

(* [n], then each of [rests]: the values around the top of a 32-bit [n * n]. *)
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

let schema label codec project corpus =
  let m = List.assoc label Rust_index.schemas in
  (reference label codec project, backends codec m, corpus)

let schemas =
  let int8 = Wire.UInt8.to_int in
  let opt name v =
    [
      (name ^ "_present", if Option.is_some v then 1 else 0);
      (name, Option.fold ~none:0 ~some:int8 v);
    ]
  in
  [
    schema "sized" Schemas.sized
      (fun _ (n, body, tail) ->
        [
          ("n", Wire.UInt32.to_int n);
          ("body_present", if Option.is_some body then 1 else 0);
          ("tail", int8 tail);
        ])
      sizes;
    schema "overflow" Schemas.overflow
      (fun _ (a, hi, lo, b, c) ->
        [
          ("a", Wire.UInt32.to_int a);
          ("hi", hi);
          ("lo", lo);
          ("b", Wire.UInt32.to_int b);
          ("c", Wire.UInt32.to_int c);
        ])
      sizes;
    schema "shared" Schemas.shared
      (fun _ (r : Schemas.shared) ->
        [ ("a", int8 r.a); ("b", int8 r.b); ("c", int8 r.c) ]
        @ opt "x" r.x @ opt "y" r.y @ opt "z" r.z)
      tags;
    schema "skipped" Schemas.skipped
      (fun _ (n, x, a, b) ->
        [ ("n", Wire.UInt32.to_int n); ("x", int8 x) ] @ opt "a" a @ opt "b" b)
      skips;
  ]

let sampled =
  List.filter_map
    (fun (label, (Fuzz_gen.Pack g as p)) ->
      Option.map
        (fun m ->
          ( "scan " ^ label,
            Diff_ocaml.check_scan
              ?oracle:(Diff_driver.sampled_oracle label)
              (Diff_ocaml.of_sampled label p)
              (backends (Fuzz_gen.codec g) m) ))
        (List.assoc_opt label Rust_index.sampled))
    Diff_ocaml.scan_codecs

(* The records of constant size of [Diff_ocaml.codecs], at offsets. *)
let cases =
  List.filter_map
    (fun (label, p) ->
      match List.assoc_opt label Rust_index.sampled with
      | Some m when m.Diff_rust.fixed -> Some (label, Diff_rust.check label p m)
      | _ -> None)
    Diff_ocaml.codecs

(* Records of [Schemas.checked] whose values each field holds, so that only
   its constraints and its [where] clause refuse them: the clause, that
   [Length] holds [IHL] words of 4 bytes, fails for about half. *)
let checked =
  let m = List.assoc "checked" Rust_index.schemas in
  let protos = [| 1; 6; 17 |] in
  ( "encode checked",
    fun buf ->
      let byte i =
        if i < Bytes.length buf then Char.code (Bytes.get buf i) else 0
      in
      Schemas.checked_make
        (byte 0 land 0xf)
        (byte 1 land 0xf)
        (Wire.UInt16.v (byte 2 land 0x3f))
        (Wire.UInt8.v (byte 4))
        (Wire.UInt8.v protos.(byte 5 mod 3))
      |> Diff_rust.raw "checked" Schemas.checked m )

let with_codec l =
  List.map
    (fun (label, msg) ->
      (label, List.assoc label (Diff_ocaml.codecs @ Diff_ocaml.scan_codecs), msg))
    l

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

(* A module's views and folds name each field by its output, so two fields
   whose outputs would be one name are a clash of names, as for the
   scanners: reported apart. *)
let ambiguous, fixed_refused =
  List.partition (fun (_, msg) -> contains msg "ambiguous") Rust_index.refused

(* The report and each case's boundary corpus are the same on every run, so
   an AFL exec does without them. *)
let () =
  if not (Fuzz_gen.afl_mode ()) then begin
    let fixed = Diff_ocaml.report Rust_index.name (with_codec fixed_refused)
    and scanned =
      Diff_ocaml.scan_report Rust_index.name
        ~scanned:(List.map fst Rust_index.sampled)
        (with_codec Rust_index.scan_refused)
    in
    Fmt.epr "%s: %d flat records refused for an ambiguous output name@."
      Rust_index.name (List.length ambiguous);
    if not (fixed && scanned) then exit 1;
    Diff_driver.report_oracle Rust_index.name;
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
  Diff_driver.run Rust_index.name
    ((checked :: cases) @ scans @ schemas @ sampled)
