(* Tests for codec.ml: dependent sizes and Field.ref expressions. *)

open Wire
open Test_helpers

(* -- Dependent-size byte_slice tests -- *)

type dep_slice_record = { length : UInt16.t; payload : Bs.t }

let f_ds_length = Field.v "Length" uint16be
let f_ds_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_ds_length))
let cf_ds_length = Codec.(f_ds_length $ fun r -> r.length)
let cf_ds_payload = Codec.(f_ds_payload $ fun r -> r.payload)

let dep_slice_codec =
  Codec.v "DepSlice"
    (fun length payload -> { length; payload })
    [ cf_ds_length; cf_ds_payload ]

let test_dep_bslice_decode_empty () =
  (* length=0, no payload bytes *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0;
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "length" 0 (UInt16.to_int r.length);
  Alcotest.(check int) "payload length" 0 (Bs.length r.payload)

let test_dep_bslice_decode_4 () =
  (* length=4, 4 payload bytes *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.set_uint8 buf 3 0xBB;
  Bytes.set_uint8 buf 4 0xCC;
  Bytes.set_uint8 buf 5 0xDD;
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "length" 4 (UInt16.to_int r.length);
  Alcotest.(check int) "payload length" 4 (Bs.length r.payload);
  Alcotest.(check int) "payload first" 2 (Bs.first r.payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload));
  Alcotest.(check int)
    "payload[3]" 0xDD
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload + 3))

let test_dep_bslice_decode_100 () =
  (* length=100, 100 payload bytes *)
  let buf = Bytes.create 102 in
  Bytes.set_uint16_be buf 0 100;
  for i = 0 to 99 do
    Bytes.set_uint8 buf (2 + i) (i land 0xFF)
  done;
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "length" 100 (UInt16.to_int r.length);
  Alcotest.(check int) "payload length" 100 (Bs.length r.payload);
  Alcotest.(check int)
    "payload[50]" 50
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload + 50))

let test_dep_bslice_roundtrip () =
  (* encode then decode: 2 bytes length + 4 bytes payload = 6 total *)
  let payload_data = Bytes.of_string "\x01\x02\x03\x04" in
  let original =
    { length = UInt16.v 4; payload = Bs.make payload_data ~first:0 ~length:4 }
  in
  let buf = Bytes.create 6 in
  Codec.encode dep_slice_codec original buf 0;
  (* Verify encoded length field *)
  Alcotest.(check int) "encoded length field" 4 (Bytes.get_uint16_be buf 0);
  (* Verify wire_size_at reads the buffer correctly *)
  Alcotest.(check int)
    "wire_size_at" 6
    (decode_ok (Codec.wire_size_at dep_slice_codec buf 0));
  let decoded = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "roundtrip length" 4 (UInt16.to_int decoded.length);
  Alcotest.(check int) "roundtrip payload len" 4 (Bs.length decoded.payload);
  Alcotest.(check int)
    "roundtrip payload[0]" 0x01
    (Bytes.get_uint8 (Bs.bytes decoded.payload) (Bs.first decoded.payload))

let test_dep_bslice_get_payload () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let payload = (reader dep_slice_codec cf_ds_payload) buf 0 in
  Alcotest.(check int) "get payload first" 2 (Bs.first payload);
  Alcotest.(check int) "get payload length" 4 (Bs.length payload);
  Alcotest.(check int)
    "get payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_dep_bslice_sub () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  let off = Bs.first ((reader dep_slice_codec cf_ds_payload) buf 0) in
  Alcotest.(check int) "sub offset" 2 off

let test_dep_bslice_set_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  (Staged.unstage (Codec.set dep_slice_codec cf_ds_length)) buf 0 (UInt16.v 8);
  Alcotest.(check int)
    "set length" 8
    (UInt16.to_int ((reader dep_slice_codec cf_ds_length) buf 0))

let test_dep_bslice_get_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 42;
  Alcotest.(check int)
    "get length" 42
    (UInt16.to_int ((reader dep_slice_codec cf_ds_length) buf 0))

(* -- Dependent-size byte_array tests -- *)

type dep_array_record = { length : UInt16.t; payload : string }

let f_da_length = Field.v "Length" uint16be
let f_da_payload = Field.v "Payload" (byte_array ~size:(Field.ref f_da_length))
let cf_da_length = Codec.(f_da_length $ fun r -> r.length)
let cf_da_payload = Codec.(f_da_payload $ fun r -> r.payload)

let dep_array_codec =
  Codec.v "DepArray"
    (fun length payload -> { length; payload })
    [ cf_da_length; cf_da_payload ]

let test_dep_byte_array_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 5;
  Bytes.blit_string "hello" 0 buf 2 5;
  let r = decode_ok (Codec.decode dep_array_codec buf 0) in
  Alcotest.(check int) "length" 5 (UInt16.to_int r.length);
  Alcotest.(check string) "payload is string copy" "hello" r.payload

let test_dep_byte_array_roundtrip () =
  let original = { length = UInt16.v 3; payload = "abc" } in
  let buf = Bytes.create 5 in
  Codec.encode dep_array_codec original buf 0;
  let decoded = decode_ok (Codec.decode dep_array_codec buf 0) in
  Alcotest.(check int) "roundtrip length" 3 (UInt16.to_int decoded.length);
  Alcotest.(check string) "roundtrip payload" "abc" decoded.payload

let test_dep_byte_array_get () =
  let buf = Bytes.create 5 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.blit_string "xyz" 0 buf 2 3;
  let payload = (reader dep_array_codec cf_da_payload) buf 0 in
  Alcotest.(check string) "get payload" "xyz" payload

(* -- Fixed field after variable field tests -- *)

type trailer_record = { length : UInt16.t; payload : Bs.t; checksum : UInt16.t }

let f_tr_length = Field.v "Length" uint16be
let f_tr_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_tr_length))
let f_tr_checksum = Field.v "Checksum" uint16be
let cf_tr_length = Codec.(f_tr_length $ fun r -> r.length)
let cf_tr_payload = Codec.(f_tr_payload $ fun r -> r.payload)
let cf_tr_checksum = Codec.(f_tr_checksum $ fun r -> r.checksum)

let trailer_codec =
  Codec.v "Trailer"
    (fun length payload checksum -> { length; payload; checksum })
    [ cf_tr_length; cf_tr_payload; cf_tr_checksum ]

let test_dep_trailer_get_checksum () =
  (* [length:u16be=3] [payload:3 bytes] [checksum:u16be=0xBEEF] *)
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0xBEEF;
  let checksum = UInt16.to_int ((reader trailer_codec cf_tr_checksum) buf 0) in
  Alcotest.(check int) "get checksum" 0xBEEF checksum

let test_dep_trailer_set_checksum () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0x0000;
  (Staged.unstage (Codec.set trailer_codec cf_tr_checksum))
    buf 0 (UInt16.v 0xCAFE);
  Alcotest.(check int) "set checksum" 0xCAFE (Bytes.get_uint16_be buf 5)

let test_dep_trailer_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.set_uint8 buf 3 0xBB;
  Bytes.set_uint8 buf 4 0xCC;
  Bytes.set_uint16_be buf 5 0xDEAD;
  let r = decode_ok (Codec.decode trailer_codec buf 0) in
  Alcotest.(check int) "length" 3 (UInt16.to_int r.length);
  Alcotest.(check int) "payload length" 3 (Bs.length r.payload);
  Alcotest.(check int) "payload first" 2 (Bs.first r.payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload));
  Alcotest.(check int) "checksum" 0xDEAD (UInt16.to_int r.checksum)

let test_dep_trailer_roundtrip () =
  let payload_data = Bytes.of_string "\x01\x02" in
  let original =
    {
      length = UInt16.v 2;
      payload = Bs.make payload_data ~first:0 ~length:2;
      checksum = UInt16.v 0x1234;
    }
  in
  let buf = Bytes.create 6 in
  Codec.encode trailer_codec original buf 0;
  let decoded = decode_ok (Codec.decode trailer_codec buf 0) in
  Alcotest.(check int) "rt length" 2 (UInt16.to_int decoded.length);
  Alcotest.(check int) "rt payload len" 2 (Bs.length decoded.payload);
  Alcotest.(check int) "rt checksum" 0x1234 (UInt16.to_int decoded.checksum)

(* -- wire_size API for variable codecs -- *)

let test_dep_wire_size_metadata () =
  Alcotest.(check (option int))
    "fixed codec size" (Some 7)
    (Codec.wire_size_opt simple_record_codec);
  Alcotest.(check (option int))
    "dependent slice is variable" None
    (Codec.wire_size_opt dep_slice_codec);
  Alcotest.(check (option int))
    "trailer is variable" None
    (Codec.wire_size_opt trailer_codec);
  (* wire_size raises Invalid_argument for variable-size codecs *)
  (match Codec.wire_size dep_slice_codec with
  | _ -> Alcotest.fail "expected Invalid_argument from wire_size"
  | exception Invalid_argument _ -> ());
  (match Codec.wire_size trailer_codec with
  | _ -> Alcotest.fail "expected Invalid_argument from wire_size"
  | exception Invalid_argument _ -> ());
  (* wire_size succeeds for fixed codecs *)
  Alcotest.(check int) "fixed wire_size" 7 (Codec.wire_size simple_record_codec)

let test_dep_min_wire_size () =
  (* min_wire_size for dep_slice_codec: just uint16be = 2 *)
  Alcotest.(check int) "dep_slice min" 2 (Codec.min_wire_size dep_slice_codec);
  (* min_wire_size for trailer_codec: uint16be + uint16be = 4 (variable payload excluded) *)
  Alcotest.(check int) "trailer min" 4 (Codec.min_wire_size trailer_codec);
  (* min_wire_size for fixed codec equals wire_size *)
  Alcotest.(check int)
    "fixed min"
    (Codec.wire_size simple_record_codec)
    (Codec.min_wire_size simple_record_codec)

let test_dep_compute_wire_size () =
  (* dep_slice: length=4 -> total = 2 + 4 = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Alcotest.(check int)
    "dep_slice compute" 6
    (decode_ok (Codec.wire_size_at dep_slice_codec buf 0));
  (* dep_slice: length=0 -> total = 2 + 0 = 2 *)
  Bytes.set_uint16_be buf 0 0;
  Alcotest.(check int)
    "dep_slice compute 0" 2
    (decode_ok (Codec.wire_size_at dep_slice_codec buf 0));
  (* trailer: length=3 -> total = 2 + 3 + 2 = 7 *)
  let buf2 = Bytes.create 7 in
  Bytes.set_uint16_be buf2 0 3;
  Alcotest.(check int)
    "trailer compute" 7
    (decode_ok (Codec.wire_size_at trailer_codec buf2 0));
  (* fixed codec: compute returns fixed size without reading buffer *)
  Alcotest.(check int)
    "fixed compute"
    (Codec.wire_size simple_record_codec)
    (decode_ok (Codec.wire_size_at simple_record_codec (Bytes.create 7) 0))

(* [wire_size_at] reads a record's sizes from the buffer, so bytes that hold
   no size are input: it answers the end-of-input error of the bytes it reads
   a size from, and raises [Invalid_argument] only on a negative offset. *)
let test_dep_wire_size_at_bad_bytes () =
  let error = Alcotest.testable Error.pp Error.equal in
  let answers name codec buf off ~expected =
    Alcotest.(check (result int error))
      name (Error expected)
      (Codec.wire_size_at codec buf off)
  in
  answers "a length the buffer cuts" dep_slice_codec (Bytes.of_string "\x00") 0
    ~expected:
      { at = 0; field = []; kind = Unexpected_eof { expected = 2; got = 1 } };
  answers "a length past the buffer" trailer_codec
    (Bytes.of_string "\x00\x03abc")
    5
    ~expected:
      { at = 5; field = []; kind = Unexpected_eof { expected = 2; got = 0 } };
  match
    Codec.wire_size_at dep_slice_codec (Bytes.of_string "\x00\x00") (-1)
  with
  | _ -> Alcotest.fail "a negative offset answered a size"
  | exception Invalid_argument _ -> ()

(* -- Field.ref expression tests -- *)

let test_dep_codec_ref () =
  (* Field.ref produces a valid expression used as byte_slice size *)
  let f_len = Field.v "Len" uint8 in
  let f_data = Field.v "Data" (byte_slice ~size:(Field.ref f_len)) in
  let cf_len = Codec.(f_len $ fun (l, _) -> l) in
  let cf_data = Codec.(f_data $ fun (_, d) -> d) in
  let codec =
    Codec.v "RefTest" (fun len data -> (len, data)) [ cf_len; cf_data ]
  in
  (* buf: [len=5] [5 bytes payload] *)
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 5;
  for i = 0 to 4 do
    Bytes.set_uint8 buf (1 + i) (0x10 + i)
  done;
  let len, data = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "ref len" 5 (UInt8.to_int len);
  Alcotest.(check int) "ref data length" 5 (Bs.length data);
  Alcotest.(check int)
    "ref data[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data));
  Alcotest.(check int)
    "ref data[4]" 0x14
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data + 4))

(* A size that can pass 64 bits, go below zero, or divide by zero has no 3D
   projection: 3d.exe cannot verify it, so the codec is refused when it is
   built. *)
let span_codec name size () =
  let f_len = Field.v "Len" uint8 in
  let f_data = Field.v "Data" (byte_array ~size:(size (Field.ref f_len))) in
  Codec.v name (fun len data -> (len, data)) Codec.[ f_len $ fst; f_data $ snd ]

let test_dep_size_arithmetic_overflow () =
  let int = Wire.int in
  List.iter
    (fun (name, mk) ->
      Alcotest.(check bool) (name ^ " refused") true (raises_invalid mk))
    [
      ("past 64 bits", span_codec "Wide" (fun n -> Expr.(n * int max_int * n)));
      ( "negative literal",
        span_codec "Negative" (fun n -> Expr.(n - int min_int + int 1)) );
      ("below zero", span_codec "Below" (fun n -> Expr.(int 1 - n)));
      ("divided by zero", span_codec "DivZero" (fun n -> Expr.(int 8 / n)));
      ("modulo zero", span_codec "ModZero" (fun n -> Expr.(int 8 mod n)));
    ]

(* A [where] that adds a literal near [max_int] to two fields passes OCaml's
   native int: it is computed on 64 bits, as the C computes it. *)
let test_where_arithmetic_overflow_offset () =
  let f_a = Field.v "a" uint8 in
  let f_b = Field.v "b" uint8 in
  let c =
    Codec.v "WhereOverflow"
      ~where:Expr.(int max_int + Field.ref f_b + Field.ref f_a > int max_int)
      (fun a b -> (a, b))
      Codec.[ f_a $ fst; f_b $ snd ]
  in
  let ok s = Result.is_ok (Codec.decode c (Bytes.of_string s) 0) in
  Alcotest.(check bool) "a + b = 0" false (ok "\000\000");
  Alcotest.(check bool) "a + b = 1" true (ok "\000\001");
  Alcotest.(check bool) "a + b = 510" true (ok "\255\255")

let test_dep_ref_size_eval () =
  (* Test that the size expression is evaluated correctly for wire_size_at *)
  let f_sz = Field.v "Size" uint8 in
  let f_body = Field.v "Body" (byte_slice ~size:(Field.ref f_sz)) in
  let cf_sz = Codec.(f_sz $ fun (s, _) -> s) in
  let cf_body = Codec.(f_body $ fun (_, b) -> b) in
  let codec =
    Codec.v "RefSizeEval" (fun sz body -> (sz, body)) [ cf_sz; cf_body ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_uint8 buf 0 10;
  Alcotest.(check int)
    "compute size" 11
    (decode_ok (Codec.wire_size_at codec buf 0));
  Bytes.set_uint8 buf 0 0;
  Alcotest.(check int)
    "compute size 0" 1
    (decode_ok (Codec.wire_size_at codec buf 0))

let signed_magnitude_seek_codec =
  let f_seek =
    Field.v "Seek" uint64be ~self_int64:(fun self ->
        Expr.(self <= int64 Int64.max_int || self > int64 Int64.min_int))
  in
  Codec.v "SignedMagnitudeSeek" Fun.id Codec.[ f_seek $ Fun.id ]

let seek_buf v =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 v;
  b

let test_int64_field_constraint_accepts_signed_magnitude_domain () =
  List.iter
    (fun v ->
      let decoded =
        decode_ok (Codec.decode signed_magnitude_seek_codec (seek_buf v) 0)
      in
      Alcotest.(check int64) "seek" v (UInt64.to_int64 decoded))
    [ 0L; Int64.max_int; Int64.succ Int64.min_int; -1L ]

let test_int64_field_constraint_rejects_negative_zero () =
  match Codec.decode signed_magnitude_seek_codec (seek_buf Int64.min_int) 0 with
  | Ok _ -> Alcotest.fail "expected signed-magnitude negative zero to fail"
  | Error { Error.kind = Constraint_failed _; _ } -> ()
  | Error e -> Alcotest.failf "expected Constraint_failed, got %a" Error.pp e

(* The bsdiff seek: a sign-magnitude uint64 decoded through a map, whose
   magnitude must fit a native int. Exercises an int64 mask ([land64]) over a
   map'd field: the constraint reads the raw pre-map word. *)
let mask_seek_codec =
  let f_seek =
    Field.v "Seek" (map ~decode:Fun.id ~encode:Fun.id uint64be)
      ~self_int64:(fun self ->
        Expr.(
          land64 self (int64 0x7FFF_FFFF_FFFF_FFFFL)
          <= int64 (Int64.of_int max_int)))
  in
  Codec.v "MaskSeek" Fun.id Codec.[ f_seek $ Fun.id ]

let test_int64_mask_constraint_over_map () =
  let accept v =
    Alcotest.(check int64)
      "accept" v
      (UInt64.to_int64
         (decode_ok (Codec.decode mask_seek_codec (seek_buf v) 0)))
  in
  let reject v =
    match Codec.decode mask_seek_codec (seek_buf v) 0 with
    | Ok _ -> Alcotest.failf "expected 0x%Lx rejected" v
    | Error { Error.kind = Constraint_failed _; _ } -> ()
    | Error e -> Alcotest.failf "wrong error: %a" Error.pp e
  in
  (* magnitude fits: small values and sign-bit-only (negative zero magnitude) *)
  List.iter accept [ 0L; 5L; Int64.min_int ];
  (* magnitude too big: bit 62 set, and all-ones *)
  List.iter reject [ 0x4000_0000_0000_0000L; Int64.max_int ]

(* A plain int-kind [self_constraint] on a uint64 field must still read the
   field value, not an unpopulated zero slot: an out-of-range value has to be
   rejected, or the bound is silently vacuous. *)
let test_uint64_int_ref_constraint_enforced () =
  let f =
    Field.v "Len" uint64be ~self_constraint:(fun self -> Expr.(self <= int 10))
  in
  let codec = Codec.v "U64IntRef" Fun.id Codec.[ f $ Fun.id ] in
  let buf v =
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 v;
    b
  in
  Alcotest.(check int64)
    "in-range accepts" 3L
    (UInt64.to_int64 (decode_ok (Codec.decode codec (buf 3L) 0)));
  match Codec.decode codec (buf 20L) 0 with
  | Ok _ -> Alcotest.fail "20 exceeds the bound and must be rejected"
  | Error { Error.kind = Constraint_failed _; _ } -> ()
  | Error e -> Alcotest.failf "expected Constraint_failed, got %a" Error.pp e

(* -- struct_of_codec for variable-size codecs -- *)

let test_struct_of_dep () =
  (* struct_of_codec should produce a valid struct for variable-size codecs *)
  let output = render_3d dep_slice_codec in
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output);
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output)

let test_struct_of_dep_trailer () =
  let output = render_3d trailer_codec in
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output);
  Alcotest.(check bool)
    "contains Checksum" true
    (contains ~sub:"Checksum" output)

(* -- sizeof_this / field_pos in codec -- *)

type pos_record = { pa : UInt8.t; pb : UInt8.t; pc : UInt8.t }

(* [sizeof_this] is only worth checking past a field wider than a byte, so this
   one carries a two-byte middle field of its own. *)
type wide_pos_record = { wa : UInt8.t; wb : UInt16.t; wc : UInt8.t }

let test_codec_sizeof_this () =
  let out = Param.output "out" uint32be in
  let codec =
    let open Codec in
    v "SizeofThisCodec"
      (fun wa wb wc -> { wa; wb; wc })
      [
        (Field.v "a" uint8 $ fun r -> r.wa);
        (Field.v "b" uint16be $ fun r -> r.wb);
        ( Field.v "c"
            ~action:(Action.on_success [ Action.assign out sizeof_this ])
            uint8
        $ fun r -> r.wc );
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x01\x00\x02\x03" in
  let _v = decode_ok (Codec.decode ~env codec buf 0) in
  (* sizeof_this at field c = 1 (uint8) + 2 (uint16be) = 3 *)
  Alcotest.(check int) "sizeof_this at c" 4 (UInt32.to_int (Param.get env out))

let test_codec_field_pos () =
  (* 3D reads the field position only into a [var], which the projection does
     not write: an action assigning it is refused. *)
  let out = Param.output "out" uint32be in
  Alcotest.(check bool)
    "field_pos refused" true
    (raises_invalid (fun () ->
         let open Codec in
         v "FieldPosCodec"
           (fun a b c -> { pa = a; pb = b; pc = c })
           [
             (Field.v "a" uint8 $ fun r -> r.pa);
             (Field.v "b" uint8 $ fun r -> r.pb);
             ( Field.v "c"
                 ~action:(Action.on_success [ Action.assign out field_pos ])
                 uint8
             $ fun r -> r.pc );
           ]))

let test_size_names_mapped_field () =
  let sized typ =
    let f_len = Field.v "len" typ in
    Codec.v "Sized"
      (fun k s -> (k, s))
      Codec.
        [
          f_len $ fst; Field.v "data" (byte_array ~size:(Field.ref f_len)) $ snd;
        ]
  in
  let lossy =
    sized (map ~decode:(fun v -> UInt8.to_int v land 3) ~encode:UInt8.v uint8)
  in
  (match Codec.decode lossy (Bytes.of_string "\x07abcdefg") 0 with
  | Ok (k, s) ->
      Alcotest.(check int) "mapped value" 3 k;
      Alcotest.(check string) "sized by the wire value" "abcdefg" s
  | Error e -> Alcotest.failf "lossy: %a" Error.pp e);
  (match Codec.decode lossy (Bytes.of_string "\x07abcd") 0 with
  | Ok _ -> Alcotest.fail "lossy: a short record decoded"
  | Error _ -> ());
  let refusing = sized (lookup [ 1; 2 ] uint8) in
  (match Codec.decode refusing (Bytes.of_string "\x01abcde") 0 with
  | Ok (k, s) ->
      Alcotest.(check int) "looked up" 2 k;
      Alcotest.(check string) "sized by the index" "a" s
  | Error e -> Alcotest.failf "lookup: %a" Error.pp e);
  match Codec.decode refusing (Bytes.of_string "\x05abcde") 0 with
  | Ok _ -> Alcotest.fail "lookup: a refused index decoded"
  | Error e ->
      Alcotest.(check string)
        "the refusal names the field" "len@0: invalid tag: 5"
        (Fmt.str "%a" Error.pp e)

let suite =
  ( "codec_dependent",
    [
      Alcotest.test_case "dep: byte_slice decode empty" `Quick
        test_dep_bslice_decode_empty;
      Alcotest.test_case "dep: byte_slice decode 4" `Quick
        test_dep_bslice_decode_4;
      Alcotest.test_case "dep: byte_slice decode 100" `Quick
        test_dep_bslice_decode_100;
      Alcotest.test_case "dep: byte_slice roundtrip" `Quick
        test_dep_bslice_roundtrip;
      Alcotest.test_case "dep: byte_slice get payload" `Quick
        test_dep_bslice_get_payload;
      Alcotest.test_case "dep: byte_slice sub" `Quick test_dep_bslice_sub;
      Alcotest.test_case "dep: byte_slice set length" `Quick
        test_dep_bslice_set_length;
      Alcotest.test_case "dep: byte_slice get length" `Quick
        test_dep_bslice_get_length;
      Alcotest.test_case "dep: byte_array decode" `Quick
        test_dep_byte_array_decode;
      Alcotest.test_case "dep: byte_array roundtrip" `Quick
        test_dep_byte_array_roundtrip;
      Alcotest.test_case "dep: byte_array get" `Quick test_dep_byte_array_get;
      Alcotest.test_case "dep: fixed after variable get checksum" `Quick
        test_dep_trailer_get_checksum;
      Alcotest.test_case "dep: fixed after variable set checksum" `Quick
        test_dep_trailer_set_checksum;
      Alcotest.test_case "dep: fixed after variable decode" `Quick
        test_dep_trailer_decode;
      Alcotest.test_case "dep: fixed after variable roundtrip" `Quick
        test_dep_trailer_roundtrip;
      Alcotest.test_case "dep: wire_size metadata" `Quick
        test_dep_wire_size_metadata;
      Alcotest.test_case "dep: min_wire_size" `Quick test_dep_min_wire_size;
      Alcotest.test_case "dep: wire_size_at" `Quick test_dep_compute_wire_size;
      Alcotest.test_case "dep: wire_size_at on bad bytes" `Quick
        test_dep_wire_size_at_bad_bytes;
      Alcotest.test_case "dep: codec ref" `Quick test_dep_codec_ref;
      Alcotest.test_case "dep: size arithmetic 3D cannot verify refused" `Quick
        test_dep_size_arithmetic_overflow;
      Alcotest.test_case "codec: where past the native int on 64 bits" `Quick
        test_where_arithmetic_overflow_offset;
      Alcotest.test_case "dep: codec ref size eval" `Quick
        test_dep_ref_size_eval;
      Alcotest.test_case "dep: struct_of_codec" `Quick test_struct_of_dep;
      Alcotest.test_case "dep: trailer struct_of_codec" `Quick
        test_struct_of_dep_trailer;
      Alcotest.test_case "codec: sizeof_this" `Quick test_codec_sizeof_this;
      Alcotest.test_case "codec: field_pos in an action refused" `Quick
        test_codec_field_pos;
      Alcotest.test_case "constraint: int64 signed-magnitude domain" `Quick
        test_int64_field_constraint_accepts_signed_magnitude_domain;
      Alcotest.test_case "constraint: int64 rejects negative zero" `Quick
        test_int64_field_constraint_rejects_negative_zero;
      Alcotest.test_case "constraint: int64 mask over map'd field" `Quick
        test_int64_mask_constraint_over_map;
      Alcotest.test_case "constraint: int-ref bound on uint64 enforced" `Quick
        test_uint64_int_ref_constraint_enforced;
      Alcotest.test_case "size: names a mapped field" `Quick
        test_size_names_mapped_field;
    ] )
