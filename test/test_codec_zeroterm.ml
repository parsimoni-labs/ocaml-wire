(* Tests for codec.ml: zero-terminated strings. *)

open Wire
open Test_helpers

(* -- Zero-terminated strings ([zeroterm] / [zeroterm_at_most]) -- *)

let zt_f_name = Field.v "name" zeroterm
let zt_f_tag = Field.v "tag" (zeroterm_at_most ~size:(int 8))
let zt_f_n = Field.v "n" uint8
let zt_codec = Codec.v "ZtRec" Fun.id Codec.[ zt_f_name $ Fun.id ]

type zt_bounded_rec = { tag : string; n : UInt8.t }

let zt_bounded_codec =
  Codec.v "ZtBoundedRec"
    (fun tag n -> { tag; n })
    Codec.[ (zt_f_tag $ fun r -> r.tag); (zt_f_n $ fun r -> r.n) ]

let test_zeroterm_roundtrip () =
  let buf = Bytes.create 6 in
  Codec.encode zt_codec "hello" buf 0;
  Alcotest.(check int) "NUL after name" 0 (Bytes.get_uint8 buf 5);
  Alcotest.(check string)
    "name" "hello"
    (decode_ok (Codec.decode zt_codec buf 0));
  let bounded = Bytes.create 9 in
  Codec.encode zt_bounded_codec { tag = "ab"; n = UInt8.v 7 } bounded 0;
  Alcotest.(check int) "NUL after tag" 0 (Bytes.get_uint8 bounded 2);
  let r = decode_ok (Codec.decode zt_bounded_codec bounded 0) in
  Alcotest.(check string) "tag" "ab" r.tag;
  Alcotest.(check int) "n" 7 (UInt8.to_int r.n)

let test_zeroterm_empty () =
  let buf = Bytes.create 1 in
  Codec.encode zt_codec "" buf 0;
  Alcotest.(check string) "name" "" (decode_ok (Codec.decode zt_codec buf 0));
  let bounded = Bytes.create 9 in
  Codec.encode zt_bounded_codec { tag = ""; n = UInt8.v 0 } bounded 0;
  let r = decode_ok (Codec.decode zt_bounded_codec bounded 0) in
  Alcotest.(check string) "tag" "" r.tag

let test_zeroterm_embedded_nul_rejected () =
  let buf = Bytes.create 4 in
  match Codec.encode zt_codec "a\000b" buf 0 with
  | () -> Alcotest.fail "expected Invalid_argument for embedded NUL"
  | exception Invalid_argument _ -> ()

(* A casetype case body is encoded by the element encoder rather than the field
   encoder, so each zeroterm form has a second compiled path behind a case. *)
type zt_case = Zt of string | Zt_at_most of string

let zt_case_typ : zt_case typ =
  casetype "ZtCase" uint8
    [
      case ~index:(UInt8.v 1) zeroterm
        ~inject:(fun s -> Zt s)
        ~project:(function Zt s -> Some s | _ -> None);
      case ~index:(UInt8.v 2)
        (zeroterm_at_most ~size:(int 8))
        ~inject:(fun s -> Zt_at_most s)
        ~project:(function Zt_at_most s -> Some s | _ -> None);
    ]

let zt_case_codec =
  Codec.v "ZtCaseRec"
    (fun v -> v)
    Codec.[ (Field.v "body" zt_case_typ $ fun v -> v) ]

(* One guard behind every encoder, so the message does not depend on whether the
   value went through [Wire.to_string] or [Codec.encode], nor on whether the
   string sat in a field or in a casetype case body. *)
let test_zeroterm_nul_message_shared () =
  let expected = "Wire.encode: zeroterm string contains a NUL byte" in
  let check label f =
    match f () with
    | () ->
        Alcotest.failf "%s: expected Invalid_argument for embedded NUL" label
    | exception Invalid_argument msg ->
        Alcotest.(check string) label expected msg
  in
  let nul = "x\000y" in
  check "Wire.to_string zeroterm" (fun () ->
      ignore (Wire.to_string zeroterm nul));
  check "Wire.to_string zeroterm_at_most" (fun () ->
      ignore (Wire.to_string (zeroterm_at_most ~size:(int 8)) nul));
  let field_buf = Bytes.create 4 in
  check "Codec.encode zeroterm field" (fun () ->
      Codec.encode zt_codec nul field_buf 0);
  check "Codec.encode zeroterm_at_most field" (fun () ->
      Codec.encode zt_bounded_codec
        { tag = nul; n = UInt8.v 0 }
        (Bytes.create 9) 0);
  let case_buf = Bytes.create 16 in
  check "Codec.encode zeroterm case body" (fun () ->
      Codec.encode zt_case_codec (Zt nul) case_buf 0);
  check "Codec.encode zeroterm_at_most case body" (fun () ->
      Codec.encode zt_case_codec (Zt_at_most nul) case_buf 0)

(* The region guard is shared the same way: a value with no room for its
   terminator reports one message from every [zeroterm_at_most] encoder. *)
let test_zeroterm_region_message_shared () =
  let expected = "Wire.encode: zeroterm string needs 9 bytes but region is 8" in
  let check label f =
    match f () with
    | () ->
        Alcotest.failf "%s: expected Invalid_argument for a full region" label
    | exception Invalid_argument msg ->
        Alcotest.(check string) label expected msg
  in
  let full = String.make 8 'x' in
  check "Wire.to_string" (fun () ->
      ignore (Wire.to_string (zeroterm_at_most ~size:(int 8)) full));
  check "Codec.encode field" (fun () ->
      Codec.encode zt_bounded_codec
        { tag = full; n = UInt8.v 0 }
        (Bytes.create 9) 0);
  check "Codec.encode case body" (fun () ->
      Codec.encode zt_case_codec (Zt_at_most full) (Bytes.create 16) 0)

let test_zeroterm_missing_terminator () =
  (* No NUL anywhere: decode must fail rather than read past the buffer. *)
  let buf = Bytes.make 6 'x' in
  match Codec.decode zt_codec buf 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected decode error on unterminated string"

let suite =
  ( "codec_zeroterm",
    [
      Alcotest.test_case "zeroterm: roundtrip" `Quick test_zeroterm_roundtrip;
      Alcotest.test_case "zeroterm: empty" `Quick test_zeroterm_empty;
      Alcotest.test_case "zeroterm: embedded NUL rejected" `Quick
        test_zeroterm_embedded_nul_rejected;
      Alcotest.test_case "zeroterm: one NUL message on every encode path" `Quick
        test_zeroterm_nul_message_shared;
      Alcotest.test_case "zeroterm: one region message on every encode path"
        `Quick test_zeroterm_region_message_shared;
      Alcotest.test_case "zeroterm: missing terminator" `Quick
        test_zeroterm_missing_terminator;
    ] )
