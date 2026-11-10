(* What Wire_rust accepts, refuses and names. The code it writes is compiled
   and compared with the codec by the Rust lane of the differential fuzzer
   (fuzz/diff/rust), and compiled here without std when BUILD_RUST=1. *)

open Wire

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let has what src sub =
  Alcotest.(check bool) (Fmt.str "%s: %S" what sub) true (contains src sub)

(* A field's name becomes a Rust identifier: lower-cased, made an identifier,
   escaped when a keyword of any edition, never the lone [_] pattern. *)
let test_ident () =
  List.iter
    (fun (name, ident) ->
      Alcotest.(check string) name ident (Wire_rust.ident name))
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

(* The module of a plain header: its size and digest, a record, the checks,
   and the functions of each field, with the field's original name in its
   errors. *)
let test_shape () =
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
let test_literals () =
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

let refused what c names =
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
let test_refused () =
  refused "a map" Schemas.mapped [ "Len"; "a map" ];
  refused "a variable size" Schemas.variable [ "Rest" ];
  refused "a name that is not UTF-8"
    (Codec.v "C" Fun.id Codec.[ Field.v "\xff" uint8 $ Fun.id ])
    [ "UTF-8" ]

(* Field names that spell one Rust name twice are refused naming both
   fields: two functions of one name do not compile. *)
let test_clash () =
  let pair n1 t1 n2 t2 =
    Codec.v "C"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 t1 $ fst; Field.v n2 t2 $ snd ]
  in
  refused "X and x" (pair "X" uint8 "x" uint8) [ "\"X\""; "\"x\"" ];
  refused "type and type_"
    (pair "type" uint8 "type_" uint8)
    [ "\"type\""; "\"type_\"" ];
  refused "a bitfield x and x_word"
    (pair "x" (bits ~width:3 U8) "x_word" uint8)
    [ "\"x\""; "\"x_word\"" ];
  refused "new, a method of Valid"
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
      Alcotest.test_case "module shape" `Quick test_shape;
      Alcotest.test_case "string literals" `Quick test_literals;
      Alcotest.test_case "unsupported codecs are refused" `Quick test_refused;
      Alcotest.test_case "clashing names are refused" `Quick test_clash;
    ] )
