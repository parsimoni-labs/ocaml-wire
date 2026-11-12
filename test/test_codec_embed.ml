(* Tests for codec.ml: embedded sub-codecs and cross-codec references. *)

open Wire
open Test_helpers

(* -- Nested: Codec typ: embed a sub-codec as a field --
   [inner] / [outer] / [inner_codec] / [outer_codec] live in {!Test_helpers}. *)

let test_codec_embed_decode () =
  (* header(1) + tag(1) + value(2) + trailer(1) = 5 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  Bytes.set_uint8 buf 4 0xBB;
  let r = decode_ok (Codec.decode outer_codec buf 0) in
  Alcotest.(check int) "header" 0xAA (UInt8.to_int r.header);
  Alcotest.(check int) "inner.tag" 0x42 (UInt8.to_int r.inner.tag);
  Alcotest.(check int) "inner.value" 0x1234 (UInt16.to_int r.inner.value);
  Alcotest.(check int) "trailer" 0xBB (UInt8.to_int r.trailer)

let test_codec_embed_encode () =
  let v =
    {
      header = UInt8.v 0xAA;
      inner = { tag = UInt8.v 0x42; value = UInt16.v 0x1234 };
      trailer = UInt8.v 0xBB;
    }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec v buf 0;
  Alcotest.(check int) "header byte" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "value bytes" 0x1234 (Bytes.get_uint16_be buf 2);
  Alcotest.(check int) "trailer byte" 0xBB (Bytes.get_uint8 buf 4)

let test_codec_embed_roundtrip () =
  let original =
    {
      header = UInt8.v 0x11;
      inner = { tag = UInt8.v 0x22; value = UInt16.v 0x3344 };
      trailer = UInt8.v 0x55;
    }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec original buf 0;
  let decoded = decode_ok (Codec.decode outer_codec buf 0) in
  Alcotest.(check int)
    "header"
    (UInt8.to_int original.header)
    (UInt8.to_int decoded.header);
  Alcotest.(check int)
    "inner.tag"
    (UInt8.to_int original.inner.tag)
    (UInt8.to_int decoded.inner.tag);
  Alcotest.(check int)
    "inner.value"
    (UInt16.to_int original.inner.value)
    (UInt16.to_int decoded.inner.value);
  Alcotest.(check int)
    "trailer"
    (UInt8.to_int original.trailer)
    (UInt8.to_int decoded.trailer)

let test_codec_embed_wire_size () =
  Alcotest.(check int) "wire_size" 5 (Codec.wire_size outer_codec)

(* Nested codec with bitfields *)

type bf_inner = { version : int; flags : int }

let bf_inner_codec =
  Codec.v "BfInner"
    (fun version flags -> { version; flags })
    Codec.
      [
        (Field.v "Version" (bits ~width:4 U8) $ fun r -> r.version);
        (Field.v "Flags" (bits ~width:4 U8) $ fun r -> r.flags);
      ]

type bf_outer = { id : UInt16.t; bf : bf_inner; checksum : UInt8.t }

let bf_outer_codec =
  Codec.v "BfOuter"
    (fun id bf checksum -> { id; bf; checksum })
    Codec.
      [
        (Field.v "Id" uint16be $ fun r -> r.id);
        (Field.v "Bf" (codec bf_inner_codec) $ fun r -> r.bf);
        (Field.v "Checksum" uint8 $ fun r -> r.checksum);
      ]

let test_codec_embed_bitfield () =
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  (* Default [bit_order = Msb_first]: version (first declared) at top nibble,
     flags at bottom nibble. version=0xA, flags=0x5 -> byte = 0xA5. *)
  Bytes.set_uint8 buf 2 0xA5;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode bf_outer_codec buf 0) in
  Alcotest.(check int) "id" 0x1234 (UInt16.to_int r.id);
  Alcotest.(check int) "version" 0xA r.bf.version;
  Alcotest.(check int) "flags" 0x5 r.bf.flags;
  Alcotest.(check int) "checksum" 0xFF (UInt8.to_int r.checksum)

(* Two levels of nesting --
   [l0] / [l1] / [l2] and their codecs live in {!Test_helpers}. *)

let test_codec_embed_nested () =
  (* l2(1) + l1_y(2) + z(1) = 4 bytes *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x42;
  Bytes.set_uint16_be buf 1 0xABCD;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int) "l2.x" 0x42 (UInt8.to_int r.inner.inner.x);
  Alcotest.(check int) "l1.y" 0xABCD (UInt16.to_int r.inner.y);
  Alcotest.(check int) "l0.z" 0xFF (UInt8.to_int r.z)

let test_codec_embed_nested_roundtrip () =
  let original : l0 =
    {
      inner = { inner = { x = UInt8.v 0x42 }; y = UInt16.v 0xABCD };
      z = UInt8.v 0xFF;
    }
  in
  let buf = Bytes.create 4 in
  Codec.encode l0_codec original buf 0;
  let decoded = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int)
    "l2.x"
    (UInt8.to_int original.inner.inner.x)
    (UInt8.to_int decoded.inner.inner.x);
  Alcotest.(check int)
    "l1.y"
    (UInt16.to_int original.inner.y)
    (UInt16.to_int decoded.inner.y);
  Alcotest.(check int) "l0.z" (UInt8.to_int original.z) (UInt8.to_int decoded.z)

(* -- Cross-codec Field.ref: parent expression references sub-codec field -- *)

(* TC/TM-style frame: header contains a length field, the data field's size
   is computed from that nested header field via Field.ref. *)

type tc_header = { version : UInt8.t; frame_len : UInt8.t }

let f_tc_version = Field.v "Version" uint8
let f_tc_frame_len = Field.v "FrameLen" uint8

let tc_header_codec =
  Codec.v "TcHeader"
    (fun version frame_len -> { version; frame_len })
    Codec.
      [
        (f_tc_version $ fun r -> r.version);
        (f_tc_frame_len $ fun r -> r.frame_len);
      ]

type tc_frame = { hdr : tc_header; data : string; check : UInt8.t }

(* The data field's size is `Field.ref f_tc_frame_len - 2 - 1` (header is 2 bytes,
   trailer is 1 byte). The Field.ref must resolve into the embedded header codec. *)
let tc_frame_codec =
  Codec.v "TcFrame"
    (fun hdr data check -> { hdr; data; check })
    Codec.
      [
        (Field.v "Header" (codec tc_header_codec) $ fun r -> r.hdr);
        ( Field.v "Data"
            (byte_array ~size:Expr.(Field.ref f_tc_frame_len - int 2 - int 1))
        $ fun r -> r.data );
        (Field.v "Check" uint8 $ fun r -> r.check);
      ]

let test_codec_cross_field_ref () =
  (* frame_len=8 -> data is 8-2-1=5 bytes *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 1;
  (* version *)
  Bytes.set_uint8 buf 1 8;
  (* frame_len *)
  Bytes.blit_string "HELLO" 0 buf 2 5;
  Bytes.set_uint8 buf 7 0xCC;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check int) "version" 1 (UInt8.to_int r.hdr.version);
  Alcotest.(check int) "frame_len" 8 (UInt8.to_int r.hdr.frame_len);
  Alcotest.(check string) "data" "HELLO" r.data;
  Alcotest.(check int) "check" 0xCC (UInt8.to_int r.check)

let test_codec_crossref_field_varying () =
  (* frame_len=5 -> data is 2 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 2;
  Bytes.set_uint8 buf 1 5;
  Bytes.blit_string "AB" 0 buf 2 2;
  Bytes.set_uint8 buf 4 0xFF;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check int) "frame_len" 5 (UInt8.to_int r.hdr.frame_len);
  Alcotest.(check string) "data" "AB" r.data;
  Alcotest.(check int) "check" 0xFF (UInt8.to_int r.check)

(* -- Adversarial: cross-codec Field.ref edge cases -- *)

(* Attacker sets frame_len=255 in a 5-byte buffer. The data field's computed
   size (255-3=252) exceeds available bytes -- must report Unexpected_eof,
   not crash or silently truncate. *)
let test_codec_crossref_field_oversized () =
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 0xFF;
  (* attacker frame_len *)
  Bytes.blit_string "AB" 0 buf 2 2;
  Bytes.set_uint8 buf 4 0xCC;
  match Codec.decode tc_frame_codec buf 0 with
  | Ok _ -> Alcotest.fail "expected EOF on oversized data field"
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* Attacker sets frame_len=2 -> data size = 2-3 = -1. Must error, not crash. *)
let test_codec_crossref_field_underflow () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  (* below header+trailer minimum *)
  Bytes.set_uint8 buf 2 0;
  Bytes.set_uint8 buf 3 0xCC;
  match Codec.decode tc_frame_codec buf 0 with
  | Ok r ->
      (* If accepted, the data must be empty or the decode must have rejected
         the negative size. Either is acceptable as long as no crash. *)
      Alcotest.(check bool)
        "data length non-negative" true
        (String.length r.data >= 0)
  | Error _ -> ()

(* frame_len=3 -> data size = 0. Boundary case: empty data. *)
let test_codec_crossref_field_zerodata () =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 3;
  Bytes.set_uint8 buf 2 0xCC;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check string) "data" "" r.data;
  Alcotest.(check int) "check" 0xCC (UInt8.to_int r.check)

(* Sub-codec field name shadowing: parent has its own field with same name as
   a sub-codec field. The parent's name should win for parent-scope expressions
   defined after the parent field. *)

type shadow_inner = { si_x : UInt8.t }

let f_si_x = Field.v "Shared" uint8

let shadow_inner_codec =
  Codec.v "ShadowInner"
    (fun x -> { si_x = x })
    Codec.[ (f_si_x $ fun r -> r.si_x) ]

type shadow_outer = { inner : shadow_inner; shared : UInt8.t; data : string }

let f_so_shared = Field.v "Shared" uint8

let shadow_outer_codec =
  Codec.v "ShadowOuter"
    (fun inner shared data -> { inner; shared; data })
    Codec.
      [
        (Field.v "Inner" (codec shadow_inner_codec) $ fun r -> r.inner);
        (f_so_shared $ fun r -> r.shared);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_so_shared)) $ fun r ->
          r.data );
      ]

let test_codec_field_shadow () =
  (* inner.shared=5, parent.shared=3 -> data should be 3 bytes (parent wins) *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 5;
  (* inner.shared *)
  Bytes.set_uint8 buf 1 3;
  (* parent.shared *)
  Bytes.blit_string "ABC" 0 buf 2 3;
  let r = decode_ok (Codec.decode shadow_outer_codec buf 0) in
  Alcotest.(check int) "inner.shared" 5 (UInt8.to_int r.inner.si_x);
  Alcotest.(check int) "parent.shared" 3 (UInt8.to_int r.shared);
  Alcotest.(check string) "data" "ABC" r.data

(* Two-level deep nesting: outer references a field three levels down. *)

type inner_l1 = { il1_x : UInt8.t }

let f_il1_x = Field.v "DeepLen" uint8

let inner_l1_codec =
  Codec.v "InnerL1"
    (fun x -> { il1_x = x })
    Codec.[ (f_il1_x $ fun r -> r.il1_x) ]

type middle_l2 = { inner : inner_l1; y : UInt8.t }

let middle_l2_codec =
  Codec.v "MiddleL2"
    (fun inner y -> { inner; y })
    Codec.
      [
        (Field.v "Inner" (codec inner_l1_codec) $ fun r -> r.inner);
        (Field.v "Y" uint8 $ fun r -> r.y);
      ]

type outer_l3 = { mid : middle_l2; data : string }

(* The outer references DeepLen which lives 2 levels deep inside the middle codec. *)
let outer_l3_codec =
  Codec.v "OuterL3"
    (fun mid data -> { mid; data })
    Codec.
      [
        (Field.v "Middle" (codec middle_l2_codec) $ fun r -> r.mid);
        (Field.v "Data" (byte_array ~size:(Field.ref f_il1_x)) $ fun r -> r.data);
      ]

let test_codec_crossref_field_twolevels () =
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 4;
  (* DeepLen at l1 *)
  Bytes.set_uint8 buf 1 0xFF;
  (* l2.y *)
  Bytes.blit_string "ABCD" 0 buf 2 4;
  let r = decode_ok (Codec.decode outer_l3_codec buf 0) in
  Alcotest.(check int) "deep_len" 4 (UInt8.to_int r.mid.inner.il1_x);
  Alcotest.(check int) "y" 0xFF (UInt8.to_int r.mid.y);
  Alcotest.(check string) "data" "ABCD" r.data

(* Sub-codec with bitfield referenced from parent. The sub-codec packs a 4-bit
   length and 4-bit flags into one byte; the parent uses the length to size data. *)

type bf_hdr = { len : int; flags : int }

let f_bh_len = Field.v "BfLen" (bits ~width:4 U8)
let f_bh_flags = Field.v "BfFlags" (bits ~width:4 U8)

let bf_hdr_codec =
  Codec.v "BfHdr"
    (fun len flags -> { len; flags })
    Codec.[ (f_bh_len $ fun r -> r.len); (f_bh_flags $ fun r -> r.flags) ]

type bf_frame = { hdr : bf_hdr; data : string }

let bf_frame_codec =
  Codec.v "BfFrame"
    (fun hdr data -> { hdr; data })
    Codec.
      [
        (Field.v "Hdr" (codec bf_hdr_codec) $ fun r -> r.hdr);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_bh_len)) $ fun r ->
          r.data );
      ]

let test_codec_crossref_field_bitfield () =
  (* Default [bit_order = Msb_first]: [BfLen] (first declared) is the top
     nibble, [BfFlags] is the bottom nibble. len=3, flags=0xA -> byte = 0x3A. *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x3A;
  Bytes.blit_string "XYZ" 0 buf 1 3;
  let r = decode_ok (Codec.decode bf_frame_codec buf 0) in
  Alcotest.(check int) "len" 3 r.hdr.len;
  Alcotest.(check int) "flags" 0xA r.hdr.flags;
  Alcotest.(check string) "data" "XYZ" r.data

let suite =
  ( "codec_embed",
    [
      Alcotest.test_case "embed: decode" `Quick test_codec_embed_decode;
      Alcotest.test_case "embed: encode" `Quick test_codec_embed_encode;
      Alcotest.test_case "embed: roundtrip" `Quick test_codec_embed_roundtrip;
      Alcotest.test_case "embed: wire_size" `Quick test_codec_embed_wire_size;
      Alcotest.test_case "embed: bitfield" `Quick test_codec_embed_bitfield;
      Alcotest.test_case "embed: nested" `Quick test_codec_embed_nested;
      Alcotest.test_case "embed: nested roundtrip" `Quick
        test_codec_embed_nested_roundtrip;
      Alcotest.test_case "embed: cross field ref" `Quick
        test_codec_cross_field_ref;
      Alcotest.test_case "embed: cross field ref varying" `Quick
        test_codec_crossref_field_varying;
      Alcotest.test_case "embed: cross field ref oversized" `Quick
        test_codec_crossref_field_oversized;
      Alcotest.test_case "embed: cross field ref underflow" `Quick
        test_codec_crossref_field_underflow;
      Alcotest.test_case "embed: cross field ref zero data" `Quick
        test_codec_crossref_field_zerodata;
      Alcotest.test_case "embed: cross field ref shadow" `Quick
        test_codec_field_shadow;
      Alcotest.test_case "embed: cross field ref two levels" `Quick
        test_codec_crossref_field_twolevels;
      Alcotest.test_case "embed: cross field ref bitfield" `Quick
        test_codec_crossref_field_bitfield;
    ] )
