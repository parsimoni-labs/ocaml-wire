(* Tests of Codec.View: checks, views, fields, layers and folds. *)

open Wire
open Test_helpers
module View = Codec.View

(* [set c f buf off x] writes [x] as the field [f] of the view of the record
   of [c] at [off] in [buf]. *)
let set c f buf off x =
  let f = View.field c f in
  let (Buf.Any b) = Buf.v buf in
  View.set b (View.get (View.check c b ~off)) f x

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
    (UInt16.to_int ((reader codec cf_x) buf 0));
  Alcotest.(check int)
    "get y" 0x5678
    (UInt16.to_int ((reader codec cf_y) buf 0))

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
  Alcotest.(check bool) "get flag=true" true ((reader codec cf_flag) buf 0);
  Bytes.set_uint8 buf 0 0x00;
  Alcotest.(check bool) "get flag=false" false ((reader codec cf_flag) buf 0)

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
  set codec cf_x buf 0 (UInt16.v 0xAAAA);
  Alcotest.(check int)
    "get x after set" 0xAAAA
    (UInt16.to_int ((reader codec cf_x) buf 0));
  Alcotest.(check int)
    "y unchanged" 0x5678
    (UInt16.to_int ((reader codec cf_y) buf 0))

let test_view_bounds_check () =
  let codec =
    let open Codec in
    v "ViewBounds" (fun a -> a) [ (Field.v "a" uint32be $ fun a -> a) ]
  in
  let buf = Bytes.create 2 in
  match Codec.decode codec buf ~off:0 with
  | Error { Error.kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" Error.pp e
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
    (UInt16.to_int ((reader codec cf_a) buf 2))

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
  set codec cf_flag buf 0 true;
  Alcotest.(check bool)
    "get flag after set true" true
    ((reader codec cf_flag) buf 0);
  Alcotest.(check int) "byte value (MSB-first)" 0x80 (Bytes.get_uint8 buf 0);
  set codec cf_flag buf 0 false;
  Alcotest.(check bool)
    "get flag after set false" false
    ((reader codec cf_flag) buf 0);
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
    (UInt16.to_int ((reader codec1 cf1_x) buf1 0));
  Alcotest.(check int)
    "codec2 get x" 0xCCCC
    (UInt16.to_int ((reader codec2 cf2_x) buf2 0))

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
  Alcotest.(check int) "codec1 get a (top 3)" 7 ((reader codec1 cf1_a) buf 0);
  Alcotest.(check int) "codec2 get a (bot 3)" 3 ((reader codec2 cf2_a) buf 0)

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
  set codec1 cf1 buf 0 0xA;
  Alcotest.(check int) "byte after set1" 0xA0 (Bytes.get_uint8 buf 0);
  (* Codec2's v is the bottom nibble -- should still be 0. *)
  Alcotest.(check int) "codec2 get after set1" 0 ((reader codec2 cf2) buf 0);
  (* Set codec2's v (bottom nibble) to 0x5. *)
  set codec2 cf2 buf 0 0x5;
  Alcotest.(check int) "byte after set2" 0xA5 (Bytes.get_uint8 buf 0);
  (* Codec1's v should still be 0xA. *)
  Alcotest.(check int) "codec1 get after set2" 0xA ((reader codec1 cf1) buf 0)

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
  let payload = (reader codec cf_payload) buf 0 in
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
  let tag, payload = decode_ok (Codec.decode codec buf ~off:0) in
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
  let payload_off = Bs.first ((reader outer_codec cf_payload) buf 0) in
  let inner_val = (reader inner_codec cf_val) buf payload_off in
  Alcotest.(check int)
    "inner val via zero-copy" 0x1234 (UInt16.to_int inner_val)

(* -- Reads and writes at an offset, through layers of slices -- *)

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
    (UInt16.to_int ((reader codec cf_a) buf 0));
  Alcotest.(check int) "get b" 0xFF (UInt8.to_int ((reader codec cf_b) buf 0))

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
  Alcotest.(check int) "get hi" 0xA ((reader codec cf_hi) buf 0);
  Alcotest.(check int) "get lo" 0x7 ((reader codec cf_lo) buf 0)

let test_raw_set_uint () =
  let f_a = Field.v "a" uint16be in
  let f_b = Field.v "b" uint8 in
  let cf_a = Codec.(f_a $ fun (a, _) -> a) in
  let cf_b = Codec.(f_b $ fun (_, b) -> b) in
  let codec = Codec.v "RawSU" (fun a b -> (a, b)) [ cf_a; cf_b ] in
  let buf = Bytes.create 3 in
  Bytes.fill buf 0 3 '\x00';
  set codec cf_a buf 0 (UInt16.v 0xABCD);
  set codec cf_b buf 0 (UInt8.v 0x42);
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
  set codec cf_hi buf 0 0xC;
  set codec cf_lo buf 0 0x3;
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
  let inner_off = Bs.first ((reader outer_codec cf_payload) buf 0) in
  Alcotest.(check int) "sub offset" 2 inner_off;
  let inner_val = (reader inner_codec cf_val) buf inner_off in
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
  let mid_off = Bs.first ((reader outer cf_body) buf 0) in
  Alcotest.(check int) "mid offset" 2 mid_off;
  let inner_off = Bs.first ((reader mid cf_mid_payload) buf mid_off) in
  Alcotest.(check int) "inner offset" 3 inner_off;
  let x = (reader inner cf_x) buf inner_off in
  Alcotest.(check int) "3-layer get" 0xCC (UInt8.to_int x)

let test_raw_with_offset () =
  (* get / set work correctly with non-zero base offset *)
  let f_v = Field.v "v" uint32be in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec = Codec.v "RawOff" (fun v -> v) [ cf_v ] in
  let buf = Bytes.create 20 in
  Bytes.fill buf 0 20 '\x00';
  set codec cf_v buf 10 (Wire.Private.UInt32.of_int32 0xDEADBEEFl);
  Alcotest.(check int32)
    "get at offset 10" 0xDEADBEEFl
    (UInt32.to_int32 ((reader codec cf_v) buf 10))

(* -- Checks -- *)

type hdr = { kind : int; len : UInt16.t }

let f_kind = Field.v "kind" ~self_constraint:Expr.(fun k -> k <= int 3) uint8
let hdr_kind = Codec.(f_kind $ fun h -> UInt8.v h.kind)
let hdr_len = Codec.(Field.v "len" uint16be $ fun h -> h.len)

let hdr =
  Codec.v "Hdr"
    (fun kind len -> { kind = UInt8.to_int kind; len })
    Codec.[ hdr_kind; hdr_len ]

let lookalike_len = Codec.(Field.v "len" uint16be $ fun h -> h.len)

let check_at ?len c buf off =
  let (Buf.Any b) = Buf.v buf in
  View.is_ok (View.check ?len c b ~off)

let validates c buf off = Result.is_ok (Codec.validate c buf ~off)

let test_check_agrees () =
  let buf = Bytes.of_string "\x01\x00\x0c\x07\x00\x01\x02" in
  for off = -2 to Bytes.length buf + 1 do
    Alcotest.(check bool)
      (Fmt.str "off %d" off)
      (off >= 0 && validates hdr buf off)
      (check_at hdr buf off)
  done

let test_check_window () =
  let buf = Bytes.of_string "\x01\x00\x0c\x02\x00\x0d" in
  Alcotest.(check bool) "whole window" true (check_at ~len:3 hdr buf 0);
  Alcotest.(check bool) "short window" false (check_at ~len:2 hdr buf 0);
  Alcotest.(check bool) "window past the end" false (check_at ~len:4 hdr buf 3);
  Alcotest.(check bool) "negative window" false (check_at ~len:(-1) hdr buf 0);
  Alcotest.(check bool) "negative offset" false (check_at ~len:3 hdr buf (-1));
  Alcotest.(check bool) "second record" true (check_at ~len:3 hdr buf 3)

(* A record of variable size checked in a window is checked in its bytes
   alone, as [validate] checks a copy of the window. *)
type tlv = { tag : UInt8.t; size : UInt8.t; body : string }

let f_tlv_size = Field.v "size" uint8

let tlv =
  Codec.v "Tlv"
    (fun tag size body -> { tag; size; body })
    Codec.
      [
        (Field.v "tag" uint8 $ fun t -> t.tag);
        (f_tlv_size $ fun t -> t.size);
        ( Field.v "body" (byte_array ~size:(Field.ref f_tlv_size)) $ fun t ->
          t.body );
      ]

let test_check_window_variable () =
  let buf = Bytes.of_string "\x07\x03abc\x08\x01z" in
  let n = Bytes.length buf in
  for off = 0 to n do
    for len = 0 to n - off do
      let window = Bytes.sub buf off len in
      Alcotest.(check bool)
        (Fmt.str "window %d+%d" off len)
        (validates tlv window 0)
        (check_at ~len tlv buf off)
    done
  done

let test_get_refused () =
  let (Buf.Any b) = Buf.v (Bytes.of_string "\x09\x00\x00") in
  let k = View.check hdr b ~off:0 in
  Alcotest.(check bool) "refused" false (View.is_ok k);
  match View.get k with
  | _ -> Alcotest.fail "a view of a refused record"
  | exception Invalid_argument _ -> ()

(* -- Fields -- *)

let test_field_of_another_codec () =
  match View.field hdr lookalike_len with
  | _ -> Alcotest.fail "a field of another codec resolved"
  | exception Invalid_argument _ -> ()

(* A field of another codec of the same record type reads the place it has
   there, and the zero of its type where that place lies outside the
   buffer; a write there is refused. *)
let test_field_same_record_type () =
  let far_len = Codec.(Field.v "len" uint16be $ fun h -> h.len) in
  let far =
    Codec.v "Far"
      (fun _ len -> { kind = 0; len })
      Codec.
        [
          ( Field.v "pad" (byte_array ~size:(int 8)) $ fun _ ->
            String.make 8 '\000' );
          far_len;
        ]
  in
  let far_len = View.field far far_len in
  let (Buf.Any b) = Buf.v (Bytes.of_string "\x01\x00\x0c") in
  let v = View.get (View.check hdr b ~off:0) in
  Alcotest.(check int) "zero" 0 (UInt16.to_int (View.read b v far_len));
  match View.set b v far_len (UInt16.v 1) with
  | () -> Alcotest.fail "a write outside the buffer"
  | exception Invalid_argument _ -> ()

let test_set_refuses () =
  let buf = Bytes.of_string "\x12" in
  let nibble = Codec.(Field.v "n" (bits ~width:4 U8) $ fst) in
  let c =
    Codec.v "Nibbles"
      (fun n m -> (n, m))
      Codec.[ nibble; Field.v "m" (bits ~width:4 U8) $ snd ]
  in
  let (Buf.Any b) = Buf.v buf in
  let v = View.get (View.check c b ~off:0) in
  (match View.set b v (View.field c nibble) 0x1F with
  | () -> Alcotest.fail "a value wider than its field"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string) "unchanged" "\x12" (Bytes.to_string buf)

(* A field after one of variable size reads where the record places it, and
   has no constant place to write. *)
let test_read_after_variable () =
  let trailer = Codec.(Field.v "trailer" uint8 $ snd) in
  let c =
    Codec.v "AfterVar"
      (fun t trailer -> (t, trailer))
      Codec.[ Field.v "tlv" (codec tlv) $ fst; trailer ]
  in
  let trailer_f = View.field c trailer in
  let (Buf.Any b) = Buf.v (Bytes.of_string "\x07\x03abc\x2a") in
  let v = View.get (View.check c b ~off:0) in
  Alcotest.(check int) "trailer" 0x2a (UInt8.to_int (View.read b v trailer_f));
  match View.set b v trailer_f (UInt8.v 1) with
  | () -> Alcotest.fail "a write of a field with no constant place"
  | exception Invalid_argument _ -> ()

let test_read_map () =
  let colour =
    Codec.(
      Field.v "colour"
        (variants "Colour" [ ("red", `Red); ("blue", `Blue) ] uint8)
      $ fst)
  in
  let c =
    Codec.v "Colours"
      (fun c n -> (c, n))
      Codec.[ colour; Field.v "n" uint8 $ snd ]
  in
  let (Buf.Any b) = Buf.v (Bytes.of_string "\x01\x05") in
  let v = View.get (View.check c b ~off:0) in
  Alcotest.(check bool) "blue" true (View.read b v (View.field c colour) = `Blue)

(* -- Layers -- *)

type gated = { gate : UInt8.t; inner : inner option; after : UInt8.t }

let f_gate = Field.v "gate" uint8

let gated_inner =
  Codec.(
    Field.optional "inner"
      ~present:Expr.(Field.ref f_gate <> int 0)
      (codec inner_codec)
    $ fun g -> g.inner)

let gated =
  Codec.v "Gated"
    (fun gate inner after -> { gate; inner; after })
    Codec.
      [
        (f_gate $ fun g -> g.gate);
        gated_inner;
        (Field.v "after" uint8 $ fun g -> g.after);
      ]

let inner_value = Codec.(f_inner_value $ fun (i : inner) -> i.value)
let present = "\x01\x07\x12\x34\x09"
let absent = "\x00\x09"

let test_layer_sub () =
  let l = View.layer gated (Codec.opt_layer gated_inner) in
  let sub s =
    let (Buf.Any b) = Buf.v (Bytes.of_string s) in
    let k = View.check gated b ~off:0 in
    Alcotest.(check bool) "checked" true (View.is_ok k);
    View.is_ok (View.sub b (View.get k) l)
  in
  Alcotest.(check bool) "present" true (sub present);
  Alcotest.(check bool) "absent" false (sub absent)

let test_layer_refused () =
  match Codec.layer Codec.(Field.v "n" uint8 $ Fun.id) with
  | _ -> Alcotest.fail "a layer of a byte"
  | exception Invalid_argument _ -> ()

let test_field_path () =
  let path = View.field gated Codec.(opt_layer gated_inner / inner_value) in
  let read s =
    let (Buf.Any b) = Buf.v (Bytes.of_string s) in
    View.read b (View.get (View.check gated b ~off:0)) path
    |> Option.map UInt16.to_int
  in
  Alcotest.(check (option int)) "present" (Some 0x1234) (read present);
  Alcotest.(check (option int)) "absent" None (read absent)

(* -- Folds -- *)

let lens buf =
  View.fold hdr
    Codec.[ hdr_len ]
    buf ~init:[]
    ~f:(fun acc len -> UInt16.to_int len :: acc)

let test_fold_back_to_back () =
  (match lens (Bytes.of_string "\x01\x00\x0c\x02\x00\x0d") with
  | Ok l -> Alcotest.(check (list int)) "lens" [ 13; 12 ] l
  | Error (e, _) -> Alcotest.failf "%a" Error.pp e);
  (match lens Bytes.empty with
  | Ok l -> Alcotest.(check (list int)) "empty" [] l
  | Error (e, _) -> Alcotest.failf "%a" Error.pp e);
  match lens (Bytes.of_string "\x01\x00\x0c\x09\x00\x0d") with
  | Ok _ -> Alcotest.fail "a refused record folded"
  | Error (e, acc) ->
      Alcotest.(check int) "at" 3 e.at;
      Alcotest.(check (list int)) "acc" [ 12 ] acc

let test_fold_stride () =
  let buf =
    Bytes.of_string "\x01\x00\x0c\xff\x09\x00\x0d\xff\x02\x00\x0e\xff\x00"
  in
  let fold ?invalid () =
    View.fold ~stride:4 ?invalid hdr
      Codec.[ hdr_len ]
      buf ~init:[]
      ~f:(fun acc len -> UInt16.to_int len :: acc)
  in
  (match fold ~invalid:(fun acc -> -1 :: acc) () with
  | Ok l -> Alcotest.(check (list int)) "slots" [ 14; -1; 12 ] l
  | Error (e, _) -> Alcotest.failf "%a" Error.pp e);
  match fold () with
  | Ok _ -> Alcotest.fail "a refused slot folded"
  | Error (e, acc) ->
      Alcotest.(check int) "at" 4 e.at;
      Alcotest.(check (list int)) "acc" [ 12 ] acc

let test_fold_index () =
  let buf = Bytes.of_string "\x01\x00\x0c\x02\x00\x0d" in
  let offs = [| 3; 0; 5 |] and lens = [| 3; 3; 3 |] in
  match
    View.fold ~offs ~lens
      ~invalid:(fun acc -> -1 :: acc)
      hdr
      Codec.[ hdr_len ]
      buf ~init:[]
      ~f:(fun acc len -> UInt16.to_int len :: acc)
  with
  | Ok l -> Alcotest.(check (list int)) "frames" [ -1; 12; 13 ] l
  | Error (e, _) -> Alcotest.failf "%a" Error.pp e

let test_fold_misuse () =
  let buf = Bytes.empty in
  let raises name f = Alcotest.(check bool) name true (raises_invalid f) in
  let f acc _ = acc in
  raises "stride 0" (fun () ->
      View.fold ~stride:0 hdr Codec.[ hdr_len ] buf ~init:() ~f);
  raises "offs alone" (fun () ->
      View.fold ~offs:[||] hdr Codec.[ hdr_len ] buf ~init:() ~f);
  raises "offs and lens" (fun () ->
      View.fold ~offs:[| 0 |] ~lens:[||] hdr Codec.[ hdr_len ] buf ~init:() ~f);
  raises "stride and offs" (fun () ->
      View.fold ~stride:1 ~offs:[||] ~lens:[||] hdr
        Codec.[ hdr_len ]
        buf ~init:() ~f);
  raises "invalid back to back" (fun () ->
      View.fold ~invalid:Fun.id hdr Codec.[ hdr_len ] buf ~init:() ~f);
  raises "a field of another codec" (fun () ->
      View.fold hdr Codec.[ lookalike_len ] buf ~init:() ~f)

let test_fold_fields_in_order () =
  match
    View.fold hdr
      Codec.[ hdr_len; hdr_kind; hdr_len ]
      (Bytes.of_string "\x01\x00\x0c\x02\x00\x0d")
      ~init:[]
      ~f:(fun acc len kind len' ->
        (UInt16.to_int len, UInt8.to_int kind, UInt16.to_int len') :: acc)
  with
  | Ok l ->
      Alcotest.(check (list (triple int int int)))
        "fields"
        [ (13, 2, 13); (12, 1, 12) ]
        l
  | Error (e, _) -> Alcotest.failf "%a" Error.pp e

(* More fields than the saturated calls take. *)
let test_fold_many_fields () =
  match
    View.fold hdr
      Codec.[ hdr_kind; hdr_len; hdr_kind; hdr_len; hdr_kind ]
      (Bytes.of_string "\x01\x00\x0c")
      ~init:0
      ~f:(fun acc a b c d e ->
        acc + UInt8.to_int a + UInt16.to_int b + UInt8.to_int c
        + UInt16.to_int d + UInt8.to_int e)
  with
  | Ok n -> Alcotest.(check int) "sum" 27 n
  | Error (e, _) -> Alcotest.failf "%a" Error.pp e

(* A path reads [None] where its layer is absent. *)
let test_fold_path () =
  let path = Codec.(opt_layer gated_inner / inner_value) in
  match
    View.fold gated
      Codec.[ path ]
      (Bytes.of_string (present ^ absent))
      ~init:[]
      ~f:(fun acc v -> Option.map UInt16.to_int v :: acc)
  with
  | Ok l ->
      Alcotest.(check (list (option int))) "values" [ None; Some 0x1234 ] l
  | Error (e, _) -> Alcotest.failf "%a" Error.pp e

(* A 32-bit field reads its carrier's value, the top bit included. *)
let test_read_32 () =
  let fields =
    Codec.
      [
        (Field.v "a" uint32be $ fun (a, _, _, _) -> a);
        (Field.v "b" uint32 $ fun (_, b, _, _) -> b);
        (Field.v "c" int32be $ fun (_, _, c, _) -> c);
        (Field.v "d" int32 $ fun (_, _, _, d) -> d);
      ]
  in
  let c = Codec.v "Thirty2" (fun a b c d -> (a, b, c, d)) fields in
  let a, b, cc, d = match fields with [ a; b; c; d ] -> (a, b, c, d) in
  let buf =
    Bytes.of_string
      "\xfe\xdc\xba\x98\x98\xba\xdc\xfe\xff\xff\xff\xfe\xfe\xff\xff\xff"
  in
  let (Buf.Any bb) = Buf.v buf in
  let v = View.get (View.check c bb ~off:0) in
  let read f = View.read bb v (View.field c f) in
  Alcotest.(check int32) "uint32be" 0xfedcba98l (UInt32.to_int32 (read a));
  Alcotest.(check int32) "uint32" 0xfedcba98l (UInt32.to_int32 (read b));
  Alcotest.(check int) "int32be" (-2) (SInt32.to_int (read cc));
  Alcotest.(check int) "int32" (-2) (SInt32.to_int (read d))

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
      Alcotest.test_case "check: agrees with validate" `Quick test_check_agrees;
      Alcotest.test_case "check: window" `Quick test_check_window;
      Alcotest.test_case "check: window of a variable record" `Quick
        test_check_window_variable;
      Alcotest.test_case "get: refused" `Quick test_get_refused;
      Alcotest.test_case "field: of another codec" `Quick
        test_field_of_another_codec;
      Alcotest.test_case "field: same record type" `Quick
        test_field_same_record_type;
      Alcotest.test_case "set: refuses" `Quick test_set_refuses;
      Alcotest.test_case "read: after a variable field" `Quick
        test_read_after_variable;
      Alcotest.test_case "read: map" `Quick test_read_map;
      Alcotest.test_case "read: 32-bit scalars" `Quick test_read_32;
      Alcotest.test_case "field: path" `Quick test_field_path;
      Alcotest.test_case "layer: sub" `Quick test_layer_sub;
      Alcotest.test_case "layer: refused" `Quick test_layer_refused;
      Alcotest.test_case "fold: back to back" `Quick test_fold_back_to_back;
      Alcotest.test_case "fold: stride" `Quick test_fold_stride;
      Alcotest.test_case "fold: index" `Quick test_fold_index;
      Alcotest.test_case "fold: misuse" `Quick test_fold_misuse;
      Alcotest.test_case "fold: fields in order" `Quick
        test_fold_fields_in_order;
      Alcotest.test_case "fold: many fields" `Quick test_fold_many_fields;
      Alcotest.test_case "fold: path" `Quick test_fold_path;
    ] )
