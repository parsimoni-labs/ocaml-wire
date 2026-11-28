(* The members of the module Wire_rust writes for a codec: its check, views,
   values and errors, as the OCaml module has them. *)

open Wire
open Rust_src

(* The module of a plain header has the members of the OCaml module: its
   fewest bytes, its check and view, an accessor and a setter per
   field, its record, its values and the error of the codec, a field's
   original name in its path. *)
let test_shape () =
  let src = folded Schemas.checked in
  let has = has "checked" src in
  has "pub const MIN_SIZE: usize = 5;";
  has "pub fn check(buf: &[u8], off: usize) -> Check<'_> {";
  has "pub fn is_ok(&self) -> bool {";
  has "pub fn get(self) -> View<'a> {";
  has "pub fn ttl(&self) -> u8 {";
  has "pub fn set_ttl(buf: &mut [u8], at: usize, x: u8) -> Result<(), Error>";
  has "pub struct Record {";
  has "    pub version: u8,";
  has "pub fn validate(buf: &[u8], off: usize) -> Result<(), Error>";
  has "pub fn decode(buf: &[u8], off: usize) -> Result<Record, Error>";
  has "pub fn encode(r: &Record, buf: &mut [u8], off: usize)";
  has "pub fn size(r: &Record) -> Result<usize, Error>";
  has "impl core::error::Error for Error {}";
  has "path.push(\"TTL\")";
  has "Kind::InvalidEnum { value: f5_proto, valid: &[";
  has "Predicate::Where";
  has "pub enum Records<'r> {";
  has "pub fn fold<A, F>(";
  has "F: FnMut(A, u8, u8, u16, u8, u8) -> A,";
  Alcotest.(check bool) "no visitor" false (contains src "trait Visitor");
  Alcotest.(check bool)
    "no fold without ~fold" false
    (contains (rs Schemas.checked) "pub fn fold");
  has "#![forbid(unsafe_code)]";
  Alcotest.(check bool) "no std" false (contains src "std::");
  Alcotest.(check bool) "no digest" false (contains src "DIGEST");
  Alcotest.(check bool)
    "no unsafe code" false
    (contains src "unsafe {" || contains src "unsafe fn");
  (* A [Record] literal names its fields, so [encode] writes what the
     labelled [write] of the OCaml module writes. *)
  Alcotest.(check bool) "no write" false (contains src "pub fn write(")

(* A codec of variable size checks a window, and has a module per record a
   field holds, which an accessor answers as a check. *)
let test_variable () =
  let src = rs Packet_stack.capture in
  let has = has "capture" src in
  has "pub fn check(buf: &[u8], off: usize, len: usize) -> Check<'_> {";
  has "pub fn ipv4(&self) -> captured_ipv4::Check<'a> {";
  has "pub mod captured_ipv4 {";
  has "pub fn decode(buf: &[u8], off: usize) -> Result<Record<'_>, Error>";
  has "    pub ipv4: Option<captured_ipv4::Record<'a>>,"

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
  has "escaped" (rs c) "path.push(\"a\\\"b\\\\c\\x0ad\")"

let suite =
  ( "rust_module",
    [
      Alcotest.test_case "module shape" `Quick test_shape;
      Alcotest.test_case "variable size" `Quick test_variable;
      Alcotest.test_case "string literals" `Quick test_literals;
    ] )
