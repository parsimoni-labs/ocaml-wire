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

(* The scanner of the packet stack: a struct of its outputs typed by their
   width and sign, [scan] over a slice and [decode] over a buffer, each prefix
   cut by [split_first_chunk], and no unsafe code. *)
let test_scanner () =
  let src = Wire_rust.scanner ~codec:"Packet_stack.codec" Packet_stack.codec in
  let scanner = has "scanner" src in
  scanner "#![forbid(unsafe_code)]";
  Fmt.kstr scanner "pub const DIGEST: &str = %S;"
    (Wire_rust.digest Packet_stack.codec);
  scanner "pub fn scan(frame: &[u8]) -> Option<Fields> {";
  scanner
    "pub fn decode(buf: &[u8], off: usize, len: usize) -> Option<Fields> {";
  scanner "    pub ether_type: u16,";
  scanner "    pub ipv4_present: bool,";
  scanner "    pub ipv4_src: u32,";
  scanner "    pub ipv4_tcp_flags: u16,";
  scanner ".split_first_chunk::<14>() else { return None; };";
  scanner ".split_first_chunk::<20>() else { return None; };";
  Alcotest.(check bool)
    "no unsafe code" false
    (contains src "unsafe {" || contains src "unsafe fn");
  Alcotest.(check bool) "no std" false (contains src "std::");
  (* Selecting no output keeps every check and decision: the IPv4 version,
     the header length, the TCP data offset and the UDP length. *)
  let v = Wire_rust.scanner ~fields:[] ~codec:"v" Packet_stack.codec in
  let validation = has "validation only" v in
  validation "pub struct Fields {\n}";
  validation "Some(Fields { })";
  validation " == 4i64)) { return None; }";
  validation " >= 5i64)) { return None; }";
  validation " >= 8i64)) { return None; }";
  validation " == 2048i64) {";
  validation " == 6i64) {";
  validation " == 17i64) {"

let one t self =
  Codec.v "One" Fun.id Codec.[ Field.v "x" t ~self_constraint:self $ Fun.id ]

(* Arithmetic is plain where the ranges the plan knows, narrowed by the
   checks passed, keep it in the 63-bit int, and checked otherwise: a checked
   operation returns [None] where the codec's overflows, which rejects the
   record. A size is compared with the tail as [u64], where a negative one is
   larger than any length: it has no test of its own. The Rust lane runs the
   sums and products of test/ocaml that do overflow. *)
let test_scanner_arithmetic () =
  let src c = Wire_rust.scanner ~codec:"c" c in
  has "u8 squared"
    (src (one uint8 (fun x -> Expr.(x * x >= int 0))))
    "(f1_x * f1_x)";
  let u32 = src (one uint32be (fun x -> Expr.(x * x >= int 0))) in
  has "u32 squared" u32 "mul(f1_x, f1_x)?";
  has "u32 squared" u32 "fn mul(a: i64, b: i64) -> Option<i64>";
  let x =
    Field.v "x" uint32be ~self_constraint:(fun x -> Expr.(x <= int 1000))
  in
  let refined =
    Codec.v "Refined"
      (fun x m -> (x, m))
      Codec.
        [
          x $ fst;
          Field.v "m" uint8
            ~constraint_:Expr.(Field.ref x * Field.ref x * Field.ref x > int 0)
          $ snd;
        ]
  in
  has "u32 below 1000, cubed" (src refined) "((f1_x * f1_x) * f1_x)";
  let overflow = src Schemas.overflow in
  let near_max = Fmt.str "%di64" (max_int - 5) in
  has "overflow" overflow (Fmt.str "add(f1_a, %s)?" near_max);
  has "overflow" overflow (Fmt.str "add(f3_lo, %s)?" near_max);
  has "overflow" overflow "mul(f4_b, f5_c)?";
  let sized = src Schemas.sized in
  has "sized" sized "if (mul(f1_n, f1_n)? != 1i64) {";
  has "sized" sized "sub(mul(f1_n, f1_n)?, 1i64)?";
  has "sized" sized "if (n4 as u64) > (r2.len() as u64) { return None; }";
  Alcotest.(check bool)
    "sized: no test for a negative size" false (contains sized "< 0");
  has "packet" (src Packet_stack.codec) "if (n9 as u64) > (r5.len() as u64)"

(* A closed enum's membership is a check like any other. *)
let test_scanner_enum () =
  let e = enum "E" [ ("a", 1); ("b", 2) ] uint8 in
  let c = Codec.v "E" Fun.id Codec.[ Field.v "e" e $ Fun.id ] in
  let check = "if !(((f1_e == 1i64) || (f1_e == 2i64))) { return None; }" in
  has "enum" (Wire_rust.scanner ~codec:"c" c) check;
  has "enum, no output" (Wire_rust.scanner ~fields:[] ~codec:"c" c) check

(* The code after an optional field is written once, so it grows linearly
   with the number of optional fields, in a run or nested; a run binds the
   outputs under it, so nested fields handing out everything grow with the
   depth times the outputs. As test/ocaml measures the OCaml scanner. *)
let lines s = List.length (String.split_on_char '\n' s)

let test_scanner_linear () =
  List.iter
    (fun (name, codec, fields) ->
      let size n = lines (Wire_rust.scanner ?fields ~codec:"g" (codec n)) in
      let s8 = size 8 and s16 = size 16 and s32 = size 32 in
      if s32 - s16 > (2 * (s16 - s8)) + 2 then
        Alcotest.failf "%s: 8, 16 and 32 optional fields: %d, %d and %d lines"
          name s8 s16 s32;
      if s32 > 150 * 32 then
        Alcotest.failf "%s: 32 optional fields: %d lines" name s32)
    [
      ("in a run", Schemas.gated, None);
      ("in a run, checks only", Schemas.gated, Some []);
      ("nested, checks only", Schemas.nested, Some []);
    ];
  let nested = lines (Wire_rust.scanner ~codec:"g" (Schemas.nested 32)) in
  if nested > 150 * 32 then
    Alcotest.failf "32 nested optional fields: %d lines" nested

(* A run of optional fields is one decision tree, which tests each atom once
   on a path. A subtree two paths reach is one closure, and the case it
   picks runs in one [match]; each case is written once. *)
let test_scanner_decision () =
  let src = Wire_rust.scanner ~codec:"c" Schemas.shared in
  let shared = has "shared" src in
  shared "|| -> Option<usize> { Some({";
  shared "()?";
  shared " = match sel";
  let count sub =
    let n = String.length sub in
    let rec go i acc =
      if i + n > String.length src then acc
      else go (i + 1) (if String.sub src i n = sub then acc + 1 else acc)
    in
    go 0 0
  in
  Alcotest.(check int) "the test of c, once" 1 (count "(f3_c == 1i64)");
  Alcotest.(check int) "x read once" 1 (count "let f5_x: i64")

let scanner_refused what ?fields c names =
  match Wire_rust.scanner ?fields ~codec:"c" c with
  | _ -> Alcotest.failf "%s: generated" what
  | exception Invalid_argument m ->
      List.iter
        (fun n ->
          Alcotest.(check bool)
            (Fmt.str "%s: names %s" what n)
            true (contains m n))
        names

(* What the plan refuses, the Rust scanner refuses: a map, whose functions
   are OCaml code, a 64-bit scalar and an unknown output. So are two outputs
   that only Rust spells alike, once its keywords are escaped. *)
let test_scanner_refused () =
  let pair n1 n2 =
    Codec.v "C"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 uint8 $ fst; Field.v n2 uint8 $ snd ]
  in
  scanner_refused "fn and fn_" (pair "fn" "fn_") [ "\"fn\""; "\"fn_\"" ];
  scanner_refused "two outputs of one name" (pair "a_b" "a_B") [ "a_b" ];
  scanner_refused "an unknown output" ~fields:[ "nope" ] Packet_stack.codec
    [ "nope" ];
  scanner_refused "a map" Schemas.mapped [ "Len"; "a map" ];
  scanner_refused "a 64-bit scalar"
    (Codec.v "Wide" Fun.id Codec.[ Field.v "w" uint64be $ Fun.id ])
    [ "UINT64" ]

let suite =
  ( "wire_rust",
    [
      Alcotest.test_case "structured scanner" `Quick test_scanner;
      Alcotest.test_case "scanned arithmetic" `Quick test_scanner_arithmetic;
      Alcotest.test_case "scanned enums" `Quick test_scanner_enum;
      Alcotest.test_case "optional fields in linear code" `Quick
        test_scanner_linear;
      Alcotest.test_case "decision trees" `Quick test_scanner_decision;
      Alcotest.test_case "unsupported scanners are refused" `Quick
        test_scanner_refused;
      Alcotest.test_case "identifiers" `Quick test_ident;
      Alcotest.test_case "module shape" `Quick test_shape;
      Alcotest.test_case "string literals" `Quick test_literals;
      Alcotest.test_case "unsupported codecs are refused" `Quick test_refused;
      Alcotest.test_case "clashing names are refused" `Quick test_clash;
    ] )
