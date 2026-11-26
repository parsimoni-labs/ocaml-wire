(* The walk of a plan as Rust: the folds, their arithmetic, enums, optional
   fields in linear code and decision trees. *)

open Wire
open Rust_src

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
  (* Selecting no output keeps every check: the range of the IPv4 version and
     header length byte, the TCP data offset and the UDP length. *)
  let v = Wire_rust.scanner ~fields:[] ~codec:"v" Packet_stack.codec in
  let validation = has "validation only" v in
  validation "pub struct Fields {\n}";
  validation "Some(Fields { })";
  validation " >= 69i64 && ";
  validation " <= 79i64) { return None; }";
  validation " >= 8i64)) { return None; }";
  validation " == 2048i64) {";
  validation " == 6i64) {";
  validation " == 17i64) {"

(* An optional byte gated by [present] over a field [x] of type [t]. *)
let gated t present =
  let x = Field.v "x" t in
  Codec.v "Gated"
    (fun x y -> (x, y))
    Codec.
      [
        x $ fst; Field.optional "y" ~present:(present (Field.ref x)) uint8 $ snd;
      ]

(* Arithmetic is plain where the ranges the plan knows, narrowed by the
   checks passed, keep it in the 63-bit int, and a comparison is on [u64]
   where they do not, as the codec computes it on 64 bits. A size is compared with the tail as [u64], where a
   negative one is larger than any length: it has no test of its own. *)
let test_scanner_arithmetic () =
  let src c = Wire_rust.scanner ~codec:"c" c in
  has "u8 squared"
    (src (gated uint8 (fun x -> Expr.(x * x >= int 0))))
    "(f1_x * f1_x)";
  let x =
    Field.v "x" uint32be ~self_constraint:(fun x -> Expr.(x <= int 1000))
  in
  let refined =
    Codec.v "Refined"
      (fun x m -> (x, m))
      Codec.
        [
          x $ fst;
          Field.optional "m"
            ~present:Expr.(Field.ref x * Field.ref x * Field.ref x > int 0)
            uint8
          $ snd;
        ]
  in
  has "u32 below 1000, cubed" (src refined) "((f1_x * f1_x) * f1_x)";
  has "u32 squared"
    (src (gated uint32be (fun x -> Expr.(x * x >= int 0))))
    "(((f1_x as u64) * (f1_x as u64)) >= 0u64)";
  let overflow = src Schemas.overflow in
  let near_max = Fmt.str "%du64" (max_int - 5) in
  let max = Fmt.str "%du64" max_int in
  has "overflow" overflow (Fmt.str "(((f1_a as u64) + %s) != %s)" near_max max);
  has "overflow" overflow (Fmt.str "(((f3_lo as u64) + %s) != %s)" near_max max);
  let sized = src Schemas.sized in
  has "sized" sized "if (((f1_n >> 16) * (f1_n >> 16)) != 1i64) {";
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

(* The code after an optional field is written once and an output under
   more than [Plan.slot_depth] of them is bound once, so the code grows
   linearly with the number of optional fields, in a run or nested, whatever
   it hands out. As test/ocaml measures the OCaml scanner, in lines and in
   words, a word being a run of letters, digits and underscores, which a
   long line cannot hide. *)
let lines s = List.length (String.split_on_char '\n' s)

let words s =
  let word = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let n = ref 0 and inside = ref false in
  String.iter
    (fun c ->
      if word c && not !inside then incr n;
      inside := word c)
    s;
  !n

(* [n] optional fields nested inside each other that hand out every output:
   a level costs 18 lines and 74 words, its three slots, the split and reads
   of its two prefixes, its presence, the block that leaves its tail, and
   its three fields of [Fields] and of the result. The first four levels
   bind their outputs in tuples instead. *)
let nested_budget n = ((20 * n) + 80, (80 * n) + 450)

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
      ("nested", Schemas.nested, None);
    ];
  List.iter
    (fun n ->
      let src = Wire_rust.scanner ~codec:"g" (Schemas.nested n) in
      let max_lines, max_words = nested_budget n in
      if lines src > max_lines || words src > max_words then
        Alcotest.failf
          "%d nested optional fields, every output: %d lines and %d words, \
           over %d and %d"
          n (lines src) (words src) max_lines max_words)
    [ 8; 16; 32; 64 ]

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
   of one name. *)
let test_scanner_refused () =
  let pair n1 n2 =
    Codec.v "C"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 uint8 $ fst; Field.v n2 uint8 $ snd ]
  in
  scanner_refused "two outputs of one name" (pair "a_b" "a_B") [ "a_b" ];
  scanner_refused "an unknown output" ~fields:[ "nope" ] Packet_stack.codec
    [ "nope" ];
  scanner_refused "a map" Schemas.mapped [ "Len"; "a map" ];
  scanner_refused "a 64-bit scalar"
    (Codec.v "Wide" Fun.id Codec.[ Field.v "w" uint64be $ Fun.id ])
    [ "UINT64" ]

let suite =
  ( "walk",
    [
      Alcotest.test_case "structured scanner" `Quick test_scanner;
      Alcotest.test_case "scanned arithmetic" `Quick test_scanner_arithmetic;
      Alcotest.test_case "scanned enums" `Quick test_scanner_enum;
      Alcotest.test_case "optional fields in linear code" `Quick
        test_scanner_linear;
      Alcotest.test_case "decision trees" `Quick test_scanner_decision;
      Alcotest.test_case "unsupported scanners are refused" `Quick
        test_scanner_refused;
    ] )
