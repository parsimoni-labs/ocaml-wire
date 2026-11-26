(* What Wire_rust accepts, refuses and names. The code it writes is compiled
   and compared with the codec by the Rust lane of the differential fuzzer
   (fuzz/diff/rust), and compiled here without std when BUILD_RUST=1. *)

open Wire
open Rust_src

(* A field's name becomes a Rust identifier, its record field's and its
   accessor's: lower-cased, made an identifier, escaped when a keyword of any
   edition, never the lone [_] pattern. *)
let test_ident () =
  List.iter
    (fun (label, ident) ->
      let src =
        rs (Codec.v "C" Fun.id Codec.[ Field.v label uint8 $ Fun.id ])
      in
      has label src (Fmt.str "    pub %s: u8," ident);
      has label src (Fmt.str "pub fn %s(&self) -> u8 {" ident))
    [
      ("Version", "version");
      ("a-b", "a_b");
      ("1x", "f_1x");
      ("_", "f__");
      ("", "f_");
      ("type", "type_");
      ("Self", "self_");
      ("async", "async_");
      ("gen", "gen_");
      ("union", "union");
    ]

let refused what ?folds c names =
  match Wire_rust.v ?folds c with
  | _ -> Alcotest.failf "%s: generated" what
  | exception Invalid_argument m ->
      List.iter
        (fun n ->
          Alcotest.(check bool)
            (Fmt.str "%s: names %s in %S" what n m)
            true (contains m n))
        names

(* What the OCaml writer refuses is refused with its message; a map calls
   OCaml functions and a name that is not UTF-8 has no Rust literal. *)
let test_refused () =
  refused "a map" Schemas.mapped
    [ "Wire_rust: cannot specialise"; "Len"; "a map" ];
  refused "a 64-bit scalar"
    (Codec.v "Wide" Fun.id Codec.[ Field.v "w" uint64be $ Fun.id ])
    [ "Wire_rust: cannot specialise"; "UINT64" ];
  refused "an unknown output"
    ~folds:[ Wire_rust.fold ~outputs:[ "nope" ] "fold" ]
    Packet_stack.codec [ "nope" ];
  refused "a name that is not UTF-8"
    (Codec.v "C" Fun.id Codec.[ Field.v "\xff" uint8 $ Fun.id ])
    [ "UTF-8" ]

(* Field names that spell one name twice are refused naming both: two
   functions of one name do not compile. *)
let test_clash () =
  let pair n1 t1 n2 t2 =
    Codec.v "C"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 t1 $ fst; Field.v n2 t2 $ snd ]
  in
  refused "X and x" (pair "X" uint8 "x" uint8) [ "\"x\"" ];
  (* The keyword escape is injective: [type] and [type_] are [type_] and
     [type__]. *)
  has "type and type_"
    (rs (pair "type" uint8 "type_" uint8))
    "pub type_: u8,\n    pub type__: u8,";
  refused "two folds of one name"
    ~folds:Wire_rust.[ fold "f"; fold "f" ]
    Schemas.checked [ "\"f\"" ];
  (* A field may be called [buf], [off] or [b], the parameters of the
     module's functions. *)
  let c =
    Codec.v "C"
      (fun a b c -> (a, b, c))
      Codec.
        [
          (Field.v "buf" uint8 $ fun (x, _, _) -> x);
          (Field.v "off" uint8 $ fun (_, x, _) -> x);
          (Field.v "b" uint8 $ fun (_, _, x) -> x);
        ]
  in
  has "buf, off, b" (rs c) "pub buf: u8,\n    pub off: u8,\n    pub b: u8,"

(* The visitor a rule names is checked to implement the fold's trait, by its
   path or by its file. *)
let test_visitor () =
  let folds = [ Wire_rust.fold "count" ] in
  has "a path"
    (rs ~folds ~visitors:[ ("count", "crate::Count") ] Schemas.checked)
    "implements::<crate::Count>();";
  has "a file"
    (rs ~folds ~visitors:[ ("count", "flow_step.rs") ] Schemas.checked)
    "implements::<crate::flow_step::FlowStep>();";
  match
    Wire_rust.(
      to_rs ~visitors:[ ("nope", "crate::X") ] (v ~folds Schemas.checked))
  with
  | _ -> Alcotest.fail "a visitor of no fold"
  | exception Invalid_argument m -> has "a visitor of no fold" m "nope"

(* {1 Flat modules} *)

(* The flat module of a plain header: its size and digest, a record, the checks,
   and the functions of each field, with the field's original name in its
   errors. *)
let test_flat_shape () =
  let src = Wire_rust.rs ~codec:"Schemas.checked" Schemas.checked in
  let has = has "checked" src in
  has "pub const SIZE: usize = 5;";
  Fmt.kstr has "pub const DIGEST: &str = %S;" (Wire_rust.digest Schemas.checked);
  has "pub struct Record {";
  has "    pub version: u8,";
  has "pub fn validate(buf: &[u8], off: usize) -> Result<(), Error>";
  has "pub fn decode(buf: &[u8], off: usize) -> Result<Record, Error>";
  has "pub fn encode(r: &Record, buf: &mut [u8], off: usize)";
  has "pub fn get_length(buf: &[u8], off: usize) -> Result<u16, Error>";
  has "pub fn set_ttl(buf: &mut [u8], off: usize, v: u8)";
  has "pub fn version_of_word(w: u8) -> u8";
  has "pub struct Valid<'a>(&'a [u8; SIZE]);";
  has "Error::Constraint { field: \"TTL\", at: off + 3 }";
  has "Error::InvalidEnum { field: \"Proto\"";
  has "Error::Constraint { field: \"\", at: off }";
  has "#![forbid(unsafe_code)]";
  Alcotest.(check bool) "no std" false (contains src "std::")

(* A field name reaches the generated code as a Rust string literal, which
   OCaml's escapes are not. *)
let test_flat_literals () =
  let c =
    Codec.v "C" Fun.id
      Codec.
        [
          Field.v "a\"b\\c\nd" uint8 ~self_constraint:(fun self ->
              Expr.(self > int 0))
          $ Fun.id;
        ]
  in
  has "escaped" (Wire_rust.rs ~codec:"c" c) "field: \"a\\\"b\\\\c\\x0ad\""

let flat_refused what c names =
  match Wire_rust.rs ~codec:"c" c with
  | _ -> Alcotest.failf "%s: generated" what
  | exception Invalid_argument m ->
      List.iter
        (fun n ->
          Alcotest.(check bool)
            (Fmt.str "%s: names %s" what n)
            true (contains m n))
        names

(* What Rust code cannot do is refused naming the field: a map calls OCaml
   functions, and a size the data decides is not one record of [SIZE]
   bytes. *)
let test_flat_refused () =
  flat_refused "a map" Schemas.mapped [ "Len"; "a map" ];
  flat_refused "a variable size" Schemas.variable [ "Rest" ];
  flat_refused "a name that is not UTF-8"
    (Codec.v "C" Fun.id Codec.[ Field.v "\xff" uint8 $ Fun.id ])
    [ "UTF-8" ]

(* Field names that spell one Rust name twice are refused naming both
   fields: two functions of one name do not compile. *)
let test_flat_clash () =
  let pair n1 t1 n2 t2 =
    Codec.v "C"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 t1 $ fst; Field.v n2 t2 $ snd ]
  in
  flat_refused "X and x" (pair "X" uint8 "x" uint8) [ "\"X\""; "\"x\"" ];
  (* The keyword escape is injective: [type] and [type_] are [type_] and
     [type__]. *)
  has "type and type_"
    (Wire_rust.rs ~codec:"c" (pair "type" uint8 "type_" uint8))
    "pub type_: u8,\n    pub type__: u8,";
  flat_refused "a bitfield x and x_word"
    (pair "x" (bits ~width:3 U8) "x_word" uint8)
    [ "\"x\""; "\"x_word\"" ];
  flat_refused "new, a method of Valid"
    (Codec.v "C" Fun.id Codec.[ Field.v "new" uint8 $ Fun.id ])
    [ "\"new\""; "new" ];
  (* A field's name is a parameter of [write] only behind a prefix, so a
     field may be called [buf], [off] or [b]. *)
  let c =
    Codec.v "C"
      (fun a b c -> (a, b, c))
      Codec.
        [
          (Field.v "buf" uint8 $ fun (x, _, _) -> x);
          (Field.v "off" uint8 $ fun (_, x, _) -> x);
          (Field.v "b" uint8 $ fun (_, _, x) -> x);
        ]
  in
  has "buf, off, b" (Wire_rust.rs ~codec:"c" c) "v_buf: u8, v_off: u8, v_b: u8"

let suite =
  ( "wire_rust",
    [
      Alcotest.test_case "identifiers" `Quick test_ident;
      Alcotest.test_case "unsupported codecs are refused" `Quick test_refused;
      Alcotest.test_case "clashing names are refused" `Quick test_clash;
      Alcotest.test_case "visitors" `Quick test_visitor;
      Alcotest.test_case "flat module shape" `Quick test_flat_shape;
      Alcotest.test_case "flat string literals" `Quick test_flat_literals;
      Alcotest.test_case "flat refusals" `Quick test_flat_refused;
      Alcotest.test_case "flat clashing names" `Quick test_flat_clash;
    ] )
