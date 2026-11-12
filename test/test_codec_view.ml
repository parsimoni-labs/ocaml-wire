(* Tests for codec.ml: zero-copy views and raw access. *)

open Wire
open Test_helpers

(* -- Zero-copy view tests -- *)

let test_view_get_uint () =
  let codec, cf_x, cf_y =
    let f_x = Field.v "x" uint16be in
    let f_y = Field.v "y" uint16be in
    let cf_x = Codec.(f_x $ fun (r : multi_record) -> r.x) in
    let cf_y = Codec.(f_y $ fun (r : multi_record) -> r.y) in
    let codec =
      Codec.v "ViewUint" (fun a b -> { x = a; y = b }) [ cf_x; cf_y ]
    in
    (codec, cf_x, cf_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  Alcotest.(check int)
    "get x" 0x1234
    (UInt16.to_int ((Staged.unstage (Codec.get codec cf_x)) buf 0));
  Alcotest.(check int)
    "get y" 0x5678
    (UInt16.to_int ((Staged.unstage (Codec.get codec cf_y)) buf 0))

let test_view_get_bool () =
  (* Default [bit_order = Msb_first]: first-declared field lives at the MSB,
     so the flag bit is bit 7 of the byte. *)
  let codec, cf_flag =
    let f_flag = Field.v "flag" (bit (bits ~width:1 U8)) in
    let cf_flag = Codec.(f_flag $ fst) in
    let codec =
      let open Codec in
      v "ViewBool"
        (fun flag code -> (flag, code))
        [ cf_flag; Field.v "code" (bits ~width:7 U8) $ snd ]
    in
    (codec, cf_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x80;
  Alcotest.(check bool)
    "get flag=true" true
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Bytes.set_uint8 buf 0 0x00;
  Alcotest.(check bool)
    "get flag=false" false
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0)

let test_view_set_uint () =
  let codec, cf_x, cf_y =
    let f_x = Field.v "x" uint16be in
    let f_y = Field.v "y" uint16be in
    let cf_x = Codec.(f_x $ fun (r : multi_record) -> r.x) in
    let cf_y = Codec.(f_y $ fun (r : multi_record) -> r.y) in
    let codec = Codec.v "ViewSetUint" (fun x y -> { x; y }) [ cf_x; cf_y ] in
    (codec, cf_x, cf_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  (Staged.unstage (Codec.set codec cf_x)) buf 0 (UInt16.v 0xAAAA);
  Alcotest.(check int)
    "get x after set" 0xAAAA
    (UInt16.to_int ((Staged.unstage (Codec.get codec cf_x)) buf 0));
  Alcotest.(check int)
    "y unchanged" 0x5678
    (UInt16.to_int ((Staged.unstage (Codec.get codec cf_y)) buf 0))

let test_view_bounds_check () =
  let codec =
    let open Codec in
    v "ViewBounds" (fun a -> a) [ (Field.v "a" uint32be $ fun a -> a) ]
  in
  let buf = Bytes.create 2 in
  match Codec.decode codec buf 0 with
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let test_view_with_offset () =
  let codec, cf_a =
    let f_a = Field.v "a" uint16be in
    let cf_a = Codec.(f_a $ fun a -> a) in
    let codec = Codec.v "ViewOff" (fun a -> a) [ cf_a ] in
    (codec, cf_a)
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_uint16_be buf 2 0x2222;
  Bytes.set_uint16_be buf 4 0x3333;
  Alcotest.(check int)
    "get at offset 2" 0x2222
    (UInt16.to_int ((Staged.unstage (Codec.get codec cf_a)) buf 2))

let test_view_set_bool () =
  (* Default [bit_order = Msb_first]: first-declared field lives at bit 7. *)
  let codec, cf_flag =
    let f_flag = Field.v "flag" (bit (bits ~width:1 U8)) in
    let cf_flag = Codec.(f_flag $ fst) in
    let codec =
      let open Codec in
      v "ViewSetBool"
        (fun flag code -> (flag, code))
        [ cf_flag; Field.v "code" (bits ~width:7 U8) $ snd ]
    in
    (codec, cf_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec cf_flag)) buf 0 true;
  Alcotest.(check bool)
    "get flag after set true" true
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Alcotest.(check int) "byte value (MSB-first)" 0x80 (Bytes.get_uint8 buf 0);
  (Staged.unstage (Codec.set codec cf_flag)) buf 0 false;
  Alcotest.(check bool)
    "get flag after set false" false
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Alcotest.(check int) "byte cleared" 0x00 (Bytes.get_uint8 buf 0)

(* -- Field sharing tests -- same field spec used in two codecs -- *)

let test_view_shared_field_spec () =
  (* Two codecs with different layouts, each with their own field "x".
     Codec1: [u16be x] [u16be y]   -> x at offset 0
     Codec2: [u16be pad] [u16be x] -> x at offset 2
     Each codec gets a fresh field object. *)
  let f1_x = Field.v "x" uint16be in
  let cf1_x = Codec.(f1_x $ fun (x, _) -> x) in
  let codec1 =
    let open Codec in
    v "Share1"
      (fun x y -> (x, y))
      [ cf1_x; (Field.v "y" uint16be $ fun (_, y) -> y) ]
  in
  let f2_x = Field.v "x" uint16be in
  let cf2_x = Codec.(f2_x $ fun (x, _) -> x) in
  let codec2 =
    let open Codec in
    v "Share2"
      (fun _pad x -> (x, UInt16.zero))
      [ (Field.v "pad" uint16be $ fun _ -> UInt16.zero); cf2_x ]
  in
  let buf1 = Bytes.create 4 in
  Bytes.set_uint16_be buf1 0 0xAAAA;
  Bytes.set_uint16_be buf1 2 0xBBBB;
  let buf2 = Bytes.create 4 in
  Bytes.set_uint16_be buf2 0 0x0000;
  Bytes.set_uint16_be buf2 2 0xCCCC;
  (* f1_x reads at offset 0, f2_x reads at offset 2 *)
  Alcotest.(check int)
    "codec1 get x" 0xAAAA
    (UInt16.to_int ((Staged.unstage (Codec.get codec1 cf1_x)) buf1 0));
  Alcotest.(check int)
    "codec2 get x" 0xCCCC
    (UInt16.to_int ((Staged.unstage (Codec.get codec2 cf2_x)) buf2 0))

let test_view_shared_bitfield_spec () =
  (* Two codecs with different bitfield layouts using the default
     [bit_order = Msb_first].
     Codec1: [3-bit a] [5-bit b]       -> a is top 3 bits
     Codec2: [5-bit pad] [3-bit a]     -> a is bottom 3 bits *)
  let f1_a = Field.v "a" (bits ~width:3 U8) in
  let cf1_a = Codec.(f1_a $ fun (a, _) -> a) in
  let codec1 =
    let open Codec in
    v "ShareBf1"
      (fun a b -> (a, b))
      [ cf1_a; (Field.v "b" (bits ~width:5 U8) $ fun (_, b) -> b) ]
  in
  let f2_a = Field.v "a" (bits ~width:3 U8) in
  let cf2_a = Codec.(f2_a $ fun (a, _) -> a) in
  let codec2 =
    let open Codec in
    v "ShareBf2"
      (fun _pad a -> (a, 0))
      [ (Field.v "pad" (bits ~width:5 U8) $ fun _ -> 0); cf2_a ]
  in
  (* 0xE3 = 0b_1110_0011
     codec1 reads top 3 bits    -> 0b111 = 7
     codec2 reads bottom 3 bits -> 0b011 = 3 *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xE3;
  Alcotest.(check int)
    "codec1 get a (top 3)" 7
    ((Staged.unstage (Codec.get codec1 cf1_a)) buf 0);
  Alcotest.(check int)
    "codec2 get a (bot 3)" 3
    ((Staged.unstage (Codec.get codec2 cf2_a)) buf 0)

let test_view_shared_set_independent () =
  (* set via one codec's field must not affect the other's interpretation.
     Default [bit_order = Msb_first]: first-declared field at top. *)
  let f1 = Field.v "v" (bits ~width:4 U8) in
  let cf1 = Codec.(f1 $ fun (v, _) -> v) in
  let codec1 =
    let open Codec in
    v "SetShare1"
      (fun v pad -> (v, pad))
      [ cf1; (Field.v "pad" (bits ~width:4 U8) $ fun (_, p) -> p) ]
  in
  let f2 = Field.v "v" (bits ~width:4 U8) in
  let cf2 = Codec.(f2 $ fun (v, _) -> v) in
  let codec2 =
    let open Codec in
    v "SetShare2"
      (fun pad v -> (v, pad))
      [ (Field.v "pad" (bits ~width:4 U8) $ fun (_, p) -> p); cf2 ]
  in
  (* Codec1's v is the top nibble; set to 0xA. *)
  let buf = Bytes.create 1 in
  (Staged.unstage (Codec.set codec1 cf1)) buf 0 0xA;
  Alcotest.(check int) "byte after set1" 0xA0 (Bytes.get_uint8 buf 0);
  (* Codec2's v is the bottom nibble -- should still be 0. *)
  Alcotest.(check int)
    "codec2 get after set1" 0
    ((Staged.unstage (Codec.get codec2 cf2)) buf 0);
  (* Set codec2's v (bottom nibble) to 0x5. *)
  (Staged.unstage (Codec.set codec2 cf2)) buf 0 0x5;
  Alcotest.(check int) "byte after set2" 0xA5 (Bytes.get_uint8 buf 0);
  (* Codec1's v should still be 0xA. *)
  Alcotest.(check int)
    "codec1 get after set2" 0xA
    ((Staged.unstage (Codec.get codec1 cf1)) buf 0)

(* -- byte_slice tests -- *)

let test_view_byte_slice_get () =
  (* A record with a fixed-size byte_slice field returns a sub-slice *)
  let f_payload = Field.v "payload" (byte_slice ~size:(int 4)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let codec =
    let open Codec in
    v "SliceRec"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xABCD;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let payload = (Staged.unstage (Codec.get codec cf_payload)) buf 0 in
  (* payload should be a slice into buf at offset 2, length 4 *)
  Alcotest.(check int) "payload first" 2 (Bs.first payload);
  Alcotest.(check int) "payload length" 4 (Bs.length payload);
  Alcotest.(check bool) "same buffer" true (Bs.bytes payload == buf);
  Alcotest.(check int)
    "payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload));
  Alcotest.(check int)
    "payload[3]" 0x40
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload + 3))

let test_view_byte_slice_decode () =
  (* decode also produces a correct sub-slice *)
  let codec =
    let open Codec in
    v "SliceDec"
      (fun tag payload -> (tag, payload))
      [
        (Field.v "tag" uint8 $ fun (t, _) -> t);
        (Field.v "data" (byte_slice ~size:(int 3)) $ fun (_, p) -> p);
      ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xFF;
  Bytes.set_uint8 buf 1 0xAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let tag, payload = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "tag" 0xFF (UInt8.to_int tag);
  Alcotest.(check int) "payload first" 1 (Bs.first payload);
  Alcotest.(check int) "payload length" 3 (Bs.length payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_view_byte_slice_nested () =
  (* Two-layer nested protocol: get payload slice, then get inner field *)
  let f_val = Field.v "val" uint16be in
  let cf_val = Codec.(f_val $ fun v -> v) in
  let inner_codec = Codec.v "Inner" (fun v -> v) [ cf_val ] in
  let f_payload = Field.v "payload" (byte_slice ~size:(int 2)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let outer_codec =
    let open Codec in
    v "Outer"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x1234;
  let payload_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec cf_payload)) buf 0)
  in
  let inner_val =
    (Staged.unstage (Codec.get inner_codec cf_val)) buf payload_off
  in
  Alcotest.(check int)
    "inner val via zero-copy" 0x1234 (UInt16.to_int inner_val)

(* -- Raw access: get / set / sub -- *)

let test_raw_get_uint () =
  let f_a = Field.v "a" uint16be in
  let f_b = Field.v "b" uint8 in
  let cf_a = Codec.(f_a $ fun (a, _) -> a) in
  let cf_b = Codec.(f_b $ fun (_, b) -> b) in
  let codec = Codec.v "RawU" (fun a b -> (a, b)) [ cf_a; cf_b ] in
  let buf = Bytes.create 3 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint8 buf 2 0xFF;
  Alcotest.(check int)
    "get a" 0x1234
    (UInt16.to_int ((Staged.unstage (Codec.get codec cf_a)) buf 0));
  Alcotest.(check int)
    "get b" 0xFF
    (UInt8.to_int ((Staged.unstage (Codec.get codec cf_b)) buf 0))

let test_raw_get_bitfield () =
  (* Default [bit_order = Msb_first]: [hi] (first declared) is the top nibble,
     matching the natural naming. *)
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let cf_hi = Codec.(f_hi $ fun (h, _) -> h) in
  let cf_lo = Codec.(f_lo $ fun (_, l) -> l) in
  let codec = Codec.v "RawBF" (fun hi lo -> (hi, lo)) [ cf_hi; cf_lo ] in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  Alcotest.(check int)
    "get hi" 0xA
    ((Staged.unstage (Codec.get codec cf_hi)) buf 0);
  Alcotest.(check int)
    "get lo" 0x7
    ((Staged.unstage (Codec.get codec cf_lo)) buf 0)

let test_raw_set_uint () =
  let f_a = Field.v "a" uint16be in
  let f_b = Field.v "b" uint8 in
  let cf_a = Codec.(f_a $ fun (a, _) -> a) in
  let cf_b = Codec.(f_b $ fun (_, b) -> b) in
  let codec = Codec.v "RawSU" (fun a b -> (a, b)) [ cf_a; cf_b ] in
  let buf = Bytes.create 3 in
  Bytes.fill buf 0 3 '\x00';
  (Staged.unstage (Codec.set codec cf_a)) buf 0 (UInt16.v 0xABCD);
  (Staged.unstage (Codec.set codec cf_b)) buf 0 (UInt8.v 0x42);
  Alcotest.(check int) "set a" 0xABCD (Bytes.get_uint16_be buf 0);
  Alcotest.(check int) "set b" 0x42 (Bytes.get_uint8 buf 2)

let test_raw_set_bitfield () =
  (* Default [bit_order = Msb_first]: [hi] goes to the top nibble. *)
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let cf_hi = Codec.(f_hi $ fun (h, _) -> h) in
  let cf_lo = Codec.(f_lo $ fun (_, l) -> l) in
  let codec = Codec.v "RawSBF" (fun hi lo -> (hi, lo)) [ cf_hi; cf_lo ] in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec cf_hi)) buf 0 0xC;
  (Staged.unstage (Codec.set codec cf_lo)) buf 0 0x3;
  Alcotest.(check int) "set bf byte" 0xC3 (Bytes.get_uint8 buf 0)

let test_raw_sub_nested () =
  (* Two-layer nested protocol using sub + get: zero alloc *)
  let f_val = Field.v "val" uint16be in
  let cf_val = Codec.(f_val $ fun v -> v) in
  let inner_codec = Codec.v "Inner" (fun v -> v) [ cf_val ] in
  let f_payload = Field.v "payload" (byte_slice ~size:(int 2)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let outer_codec =
    let open Codec in
    v "Outer"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x5678;
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec cf_payload)) buf 0)
  in
  Alcotest.(check int) "sub offset" 2 inner_off;
  let inner_val =
    (Staged.unstage (Codec.get inner_codec cf_val)) buf inner_off
  in
  Alcotest.(check int) "inner val via sub+get" 0x5678 (UInt16.to_int inner_val)

let test_raw_sub_three_layers () =
  (* Three-layer: outer -> mid -> inner, all zero-alloc via sub+get *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let inner = Codec.v "L3" (fun x -> x) [ cf_x ] in
  let f_mid_payload = Field.v "data" (byte_slice ~size:(int 1)) in
  let cf_mid_payload = Codec.(f_mid_payload $ fun (_, p) -> p) in
  let mid =
    let open Codec in
    v "L2"
      (fun tag payload -> (tag, payload))
      [ (Field.v "tag" uint8 $ fun (t, _) -> t); cf_mid_payload ]
  in
  let f_body = Field.v "body" (byte_slice ~size:(int 2)) in
  let cf_body = Codec.(f_body $ fun (_, b) -> b) in
  let outer =
    let open Codec in
    v "L1"
      (fun hdr body -> (hdr, body))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_body ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let mid_off = Bs.first ((Staged.unstage (Codec.get outer cf_body)) buf 0) in
  Alcotest.(check int) "mid offset" 2 mid_off;
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get mid cf_mid_payload)) buf mid_off)
  in
  Alcotest.(check int) "inner offset" 3 inner_off;
  let x = (Staged.unstage (Codec.get inner cf_x)) buf inner_off in
  Alcotest.(check int) "3-layer get" 0xCC (UInt8.to_int x)

let test_raw_with_offset () =
  (* get / set work correctly with non-zero base offset *)
  let f_v = Field.v "v" uint32be in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec = Codec.v "RawOff" (fun v -> v) [ cf_v ] in
  let buf = Bytes.create 20 in
  Bytes.fill buf 0 20 '\x00';
  (Staged.unstage (Codec.set codec cf_v))
    buf 10
    (Wire.Private.UInt32.of_int32 0xDEADBEEFl);
  Alcotest.(check int32)
    "get at offset 10" 0xDEADBEEFl
    (UInt32.to_int32 ((Staged.unstage (Codec.get codec cf_v)) buf 10))

let suite =
  ( "codec_view",
    [
      Alcotest.test_case "view: get uint" `Quick test_view_get_uint;
      Alcotest.test_case "view: get bool" `Quick test_view_get_bool;
      Alcotest.test_case "view: set uint" `Quick test_view_set_uint;
      Alcotest.test_case "view: set bool" `Quick test_view_set_bool;
      Alcotest.test_case "view: bounds check" `Quick test_view_bounds_check;
      Alcotest.test_case "view: with offset" `Quick test_view_with_offset;
      Alcotest.test_case "view: shared field spec" `Quick
        test_view_shared_field_spec;
      Alcotest.test_case "view: shared bitfield spec" `Quick
        test_view_shared_bitfield_spec;
      Alcotest.test_case "view: shared set independent" `Quick
        test_view_shared_set_independent;
      Alcotest.test_case "view: byte_slice get" `Quick test_view_byte_slice_get;
      Alcotest.test_case "view: byte_slice decode" `Quick
        test_view_byte_slice_decode;
      Alcotest.test_case "view: byte_slice nested" `Quick
        test_view_byte_slice_nested;
      Alcotest.test_case "raw: get uint" `Quick test_raw_get_uint;
      Alcotest.test_case "raw: get bitfield" `Quick test_raw_get_bitfield;
      Alcotest.test_case "raw: set uint" `Quick test_raw_set_uint;
      Alcotest.test_case "raw: set bitfield" `Quick test_raw_set_bitfield;
      Alcotest.test_case "raw: sub nested" `Quick test_raw_sub_nested;
      Alcotest.test_case "raw: sub 3 layers" `Quick test_raw_sub_three_layers;
      Alcotest.test_case "raw: with offset" `Quick test_raw_with_offset;
    ] )
