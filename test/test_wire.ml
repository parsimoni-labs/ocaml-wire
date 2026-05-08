(* Tests for wire.ml: direct decoding, encoding, and streaming semantics. *)

open Wire
open Wire.Everparse.Raw

(* Helper: parse from a string delivered in slices of [chunk_size] bytes.
   Forces multi-byte values to straddle slice boundaries. *)
let parse_chunked ~chunk_size typ s =
  let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:chunk_size s in
  Wire.of_reader typ reader

(* Helper: encode to string via streaming writer with [chunk_size] buffer *)
let encode_chunked ~chunk_size typ v =
  let buf = Buffer.create 64 in
  let writer = Bytesrw.Bytes.Writer.of_buffer ~slice_length:chunk_size buf in
  Wire.to_writer typ v writer;
  Bytesrw.Bytes.Writer.write_eod writer;
  Buffer.contents buf

(* -- Parsing tests -- *)

let test_parse_uint8 () =
  let input = "\x42" in
  match of_string uint8 input with
  | Ok v -> Alcotest.(check int) "uint8 value" 0x42 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_le () =
  let input = "\x01\x02" in
  match of_string uint16 input with
  | Ok v -> Alcotest.(check int) "uint16 le value" 0x0201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint16_be () =
  let input = "\x01\x02" in
  match of_string uint16be input with
  | Ok v -> Alcotest.(check int) "uint16 be value" 0x0102 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_le () =
  let input = "\x01\x02\x03\x04" in
  match of_string uint32 input with
  | Ok v -> Alcotest.(check int) "uint32 le value" 0x04030201 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint32_be () =
  let input = "\x01\x02\x03\x04" in
  match of_string uint32be input with
  | Ok v -> Alcotest.(check int) "uint32 be value" 0x01020304 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_uint64_le () =
  let input = "\x01\x02\x03\x04\x05\x06\x07\x08" in
  match of_string uint64 input with
  | Ok v -> Alcotest.(check int64) "uint64 le value" 0x0807060504030201L v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_array () =
  let input = "\x01\x02\x03" in
  let t = array ~len:(int 3) uint8 in
  match of_string t input with
  | Ok v -> Alcotest.(check (list int)) "array values" [ 1; 2; 3 ] v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_byte_array () =
  let input = "hello" in
  let t = byte_array ~size:(int 5) in
  match of_string t input with
  | Ok v -> Alcotest.(check string) "byte_array value" "hello" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_int8_negative () =
  let buf = Bytes.of_string "\xFE" in
  Alcotest.(check int) "-2" (-2) (of_bytes_exn int8 buf)

let test_int8_full_range () =
  for i = -128 to 127 do
    let s = to_string int8 i in
    Alcotest.(check int) (Fmt.str "%d" i) i (of_string_exn int8 s)
  done

let test_int16be_negative () =
  let buf = Bytes.of_string "\xFF\xFE" in
  Alcotest.(check int) "-2 BE" (-2) (of_bytes_exn int16be buf)

let test_int32be_negative () =
  let buf = Bytes.of_string "\xFF\xFF\xFF\xFE" in
  Alcotest.(check int) "-2 BE" (-2) (of_bytes_exn int32be buf)

let test_int64le_roundtrip () =
  let v = -0x0102_0304_0506_0708L in
  let s = to_string int64 v in
  Alcotest.(check int64) "roundtrip" v (of_string_exn int64 s)

let test_float32be_roundtrip () =
  let v = 1.5 in
  let s = to_string float32be v in
  Alcotest.(check (float 0.0)) "1.5" v (of_string_exn float32be s)

let test_float64le_roundtrip () =
  List.iter
    (fun v ->
      let s = to_string float64 v in
      Alcotest.(check (float 0.0)) "roundtrip" v (of_string_exn float64 s))
    [ 0.0; -0.0; 3.14159; -1e300; Float.infinity; Float.neg_infinity ]

let test_float64_nan_roundtrip () =
  let s = to_string float64be Float.nan in
  let v = of_string_exn float64be s in
  Alcotest.(check bool) "nan preserved" true (Float.is_nan v)

let finite_field name typ =
  let template = Field.v name typ in
  Field.v name ~constraint_:(is_finite template) typ

let finite_codec () =
  let f_v = finite_field "v" float64be in
  Codec.v "Tel" (fun v -> v) Codec.[ (f_v $ fun v -> v) ]

let test_finite_rejects_nan () =
  let buf = Bytes.create 8 in
  Bytes.set_int64_be buf 0 0x7FF8_0000_0000_0001L;
  match Codec.decode (finite_codec ()) buf 0 with
  | Error (Constraint_failed _) -> ()
  | Ok _ -> Alcotest.fail "is_finite should have rejected NaN"
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_finite_accepts () =
  let buf = Bytes.create 8 in
  Bytes.set_int64_be buf 0 (Int64.bits_of_float 3.14);
  match Codec.decode (finite_codec ()) buf 0 with
  | Ok v -> Alcotest.(check (float 0.0)) "finite ok" 3.14 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_finite_rejects_inf () =
  let buf = Bytes.create 8 in
  Bytes.set_int64_be buf 0 (Int64.bits_of_float Float.infinity);
  match Codec.decode (finite_codec ()) buf 0 with
  | Error (Constraint_failed _) -> ()
  | _ -> Alcotest.fail "is_finite should have rejected +inf"

let printable_byte b = Expr.(b >= int 0x20 && b <= int 0x7e)

let test_bawhere_accepts () =
  let t = byte_array_where ~size:(int 5) ~per_byte:printable_byte in
  match of_string t "abc D" with
  | Ok v -> Alcotest.(check string) "printable" "abc D" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_bawhere_rejects () =
  let t = byte_array_where ~size:(int 4) ~per_byte:printable_byte in
  match of_string t "ab\x01c" with
  | Ok _ -> Alcotest.fail "expected per-byte refinement to reject"
  | Error (Constraint_failed _) -> ()
  | Error e ->
      Alcotest.failf "expected Constraint_failed, got %a" pp_parse_error e

let test_bawhere_enc_rejects () =
  let t = byte_array_where ~size:(int 3) ~per_byte:printable_byte in
  match to_string t "a\x01b" with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected encode to reject non-printable byte"

let test_parse_variants_valid () =
  let input = "\x01" in
  let t = variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] uint8 in
  match of_string t input with
  | Ok v -> Alcotest.(check bool) "variants value" true (v = `B)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_variants_invalid () =
  let input = "\xFF" in
  let t = variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] uint8 in
  match of_string t input with
  | Ok _ -> Alcotest.fail "expected error for invalid enum"
  | Error (Invalid_enum { value; _ }) ->
      Alcotest.(check int) "invalid enum value" 255 value
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_all_bytes () =
  let input = "hello world" in
  match of_string all_bytes input with
  | Ok v -> Alcotest.(check string) "all_bytes value" "hello world" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_valid () =
  let input = "\x00\x00\x00" in
  match of_string all_zeros input with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_all_zeros_invalid () =
  let input = "\x00\x01\x00" in
  match of_string all_zeros input with
  | Ok _ -> Alcotest.fail "expected error for non-zero byte"
  | Error (All_zeros_failed { offset }) ->
      Alcotest.(check int) "non-zero offset" 1 offset
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_bitfield () =
  let input = "\xFF\xFF\xFF\xFF" in
  let t = bits ~width:6 U32 in
  match of_string t input with
  | Ok v -> Alcotest.(check int) "bitfield value (6 bits)" 63 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_eof () =
  let input = "\x01" in
  match of_string uint16 input with
  | Ok _ -> Alcotest.fail "expected EOF error"
  | Error (Unexpected_eof { expected; got }) ->
      Alcotest.(check int) "expected bytes" 2 expected;
      Alcotest.(check int) "got bytes" 1 got
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_struct () =
  let input = "\x01\x02\x03" in
  let s =
    struct_ "Test" [ field "a" uint8; field "b" uint8; field "c" uint8 ]
  in
  let t = struct_typ s in
  match of_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_constraint () =
  (* Test struct with constraint that should pass *)
  let input = "\x0A" in
  let f_x = field "x" uint8 in
  let f_x = field "x" ~constraint_:Expr.(field_ref f_x <= int 100) uint8 in
  let s = struct_ "Constrained" [ f_x ] in
  let t = struct_typ s in
  match of_string t input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_constraint_fail () =
  (* Test struct with constraint that should fail *)
  let input = "\xFF" in
  let f_x = field "x" uint8 in
  let f_x = field "x" ~constraint_:Expr.(field_ref f_x <= int 100) uint8 in
  let s = struct_ "Constrained" [ f_x ] in
  let t = struct_typ s in
  match of_string t input with
  | Ok _ -> Alcotest.fail "expected constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_action_return_true () =
  let input = "\x0A" in
  let f_x = field "x" uint8 in
  let s =
    struct_ "ActionOk"
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(field_ref f_x <= int 10) ])
          uint8;
      ]
  in
  match of_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_action_return_false () =
  let input = "\x0B" in
  let f_x = field "x" uint8 in
  let s =
    struct_ "ActionFail"
      [
        field "x"
          ~action:
            (Action.on_success
               [ Action.return_bool Expr.(field_ref f_x <= int 10) ])
          uint8;
      ]
  in
  match of_string (struct_typ s) input with
  | Ok () -> Alcotest.fail "expected action failure"
  | Error (Constraint_failed "field action") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_parse_struct_action_var () =
  let input = "\x04" in
  let f_x = field "x" uint8 in
  let f_twice = field "twice" uint8 in
  let s =
    struct_ "ActionVar"
      [
        field "x"
          ~action:
            (Action.on_success
               [
                 Action.var "twice" Expr.(field_ref f_x * int 2);
                 Action.return_bool Expr.(field_ref f_twice = int 8);
               ])
          uint8;
      ]
  in
  match of_string (struct_typ s) input with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_struct_action_abort () =
  let input = "\x01" in
  let s =
    struct_ "ActionAbort"
      [ field "x" ~action:(Action.on_success [ Action.abort ]) uint8 ]
  in
  match of_string (struct_typ s) input with
  | Ok () -> Alcotest.fail "expected abort"
  | Error (Constraint_failed "field action") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

type bounded_payload = { bp_length : int; bp_data : string }

let test_parse_param_with_params () =
  let max_len = Param.input "max_len" uint16be in
  let out_len = Param.output "out_len" uint16be in
  let f_length_c = field "Length" uint16be in
  let f_length =
    Field.v "Length"
      ~action:
        (Action.on_success [ Action.assign out_len (field_ref f_length_c) ])
      uint16be
  in
  let c =
    Codec.v "BoundedPayload"
      ~where:Expr.(field_ref f_length_c <= Param.expr max_len)
      (fun length data -> { bp_length = length; bp_data = data })
      Codec.
        [
          (f_length $ fun r -> r.bp_length);
          ( Field.v "Data" (byte_array ~size:(Field.ref f_length)) $ fun r ->
            r.bp_data );
        ]
  in
  let env = Codec.env c |> Param.bind max_len 3 in
  let buf = Bytes.of_string "\x00\x03abc" in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.(check int) "out_len" 3 (Param.get env out_len)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_parse_param_where_fail () =
  let max_len = Param.input "max_len" uint16be in
  let out_len = Param.output "out_len" uint16be in
  let f_length_c = field "Length" uint16be in
  let f_length =
    Field.v "Length"
      ~action:
        (Action.on_success [ Action.assign out_len (field_ref f_length_c) ])
      uint16be
  in
  let c =
    Codec.v "BoundedPayload"
      ~where:Expr.(field_ref f_length_c <= Param.expr max_len)
      (fun length data -> { bp_length = length; bp_data = data })
      Codec.
        [
          (f_length $ fun r -> r.bp_length);
          ( Field.v "Data" (byte_array ~size:(Field.ref f_length)) $ fun r ->
            r.bp_data );
        ]
  in
  let env = Codec.env c |> Param.bind max_len 2 in
  let buf = Bytes.of_string "\x00\x03abc" in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.fail "expected where failure"
  | Error (Constraint_failed "where clause") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* -- sizeof / sizeof_this / field_pos -- *)

let test_sizeof () =
  (* sizeof(uint32) = 4, use it in a constraint *)
  let f_x = field "x" uint8 in
  let s =
    struct_ "SizeofTest"
      [ field "x" ~constraint_:Expr.(field_ref f_x = sizeof uint32be) uint8 ]
  in
  (* x=4 => sizeof(uint32be) = 4, constraint passes *)
  (match of_string (struct_typ s) "\x04" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "sizeof pass: %a" pp_parse_error e);
  (* x=3 => sizeof(uint32be) = 4, constraint fails *)
  match of_string (struct_typ s) "\x03" with
  | Ok _ -> Alcotest.fail "expected sizeof constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_sizeof_this () =
  (* sizeof_this tracks bytes consumed: after field a (1 byte) and b (2 bytes),
     sizeof_this should be 3 at the point field c is parsed. *)
  let s =
    struct_ "SizeofThis"
      [
        field "a" uint8;
        field "b" uint16be;
        field "c" ~constraint_:Expr.(sizeof_this = int 3) uint8;
      ]
  in
  (* a=1, b=2, sizeof_this=3 at c, c=0: constraint passes *)
  match of_string (struct_typ s) "\x01\x00\x02\x00" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "sizeof_this: %a" pp_parse_error e

let test_sizeof_this_fail () =
  (* sizeof_this should be 1 at field b, not 2 *)
  let s =
    struct_ "SizeofThisFail"
      [
        field "a" uint8; field "b" ~constraint_:Expr.(sizeof_this = int 2) uint8;
      ]
  in
  (* sizeof_this=1 at b, constraint says =2: fails *)
  match of_string (struct_typ s) "\x01\x02" with
  | Ok _ -> Alcotest.fail "expected sizeof_this constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_field_pos () =
  (* field_pos is 0 for the first field, 1 for the second, etc. *)
  let s =
    struct_ "FieldPos"
      [
        field "a" ~constraint_:Expr.(field_pos = int 0) uint8;
        field "b" ~constraint_:Expr.(field_pos = int 1) uint8;
        field "c" ~constraint_:Expr.(field_pos = int 2) uint8;
      ]
  in
  match of_string (struct_typ s) "\x01\x02\x03" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "field_pos: %a" pp_parse_error e

let test_field_pos_fail () =
  let s =
    struct_ "FieldPosFail"
      [
        field "a" uint8;
        field "b"
          ~constraint_:Expr.(field_pos = int 0) (* wrong: should be 1 *)
          uint8;
      ]
  in
  match of_string (struct_typ s) "\x01\x02" with
  | Ok _ -> Alcotest.fail "expected field_pos constraint failure"
  | Error (Constraint_failed _) -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

type sizeof_action_record = { sa_a : int; sa_b : int; sa_c : int }

let test_sizeof_this_with_action () =
  (* sizeof_this visible to actions: assign out = sizeof_this at field c *)
  let out = Param.output "out" uint8 in
  let c =
    Codec.v "SizeofThisAction"
      (fun a b c -> { sa_a = a; sa_b = b; sa_c = c })
      Codec.
        [
          (Field.v "a" uint8 $ fun r -> r.sa_a);
          (Field.v "b" uint16be $ fun r -> r.sa_b);
          ( Field.v "c"
              ~action:(Action.on_success [ Action.assign out sizeof_this ])
              uint8
          $ fun r -> r.sa_c );
        ]
  in
  let env = Codec.env c in
  let buf = Bytes.of_string "\x01\x00\x02\x00" in
  match Codec.decode ~env c buf 0 with
  | Ok _ -> Alcotest.(check int) "sizeof_this via action" 3 (Param.get env out)
  | Error e -> Alcotest.failf "sizeof_this action: %a" pp_parse_error e

(* -- Encoding tests -- *)

let test_encode_uint8 () =
  let encoded = to_string uint8 0x42 in
  Alcotest.(check string) "uint8 encoding" "\x42" encoded

let test_encode_uint16_le () =
  let encoded = to_string uint16 0x0201 in
  Alcotest.(check string) "uint16 le encoding" "\x01\x02" encoded

let test_encode_uint16_be () =
  let encoded = to_string uint16be 0x0102 in
  Alcotest.(check string) "uint16 be encoding" "\x01\x02" encoded

let test_encode_uint32_le () =
  let encoded = to_string uint32 0x04030201 in
  Alcotest.(check string) "uint32 le encoding" "\x01\x02\x03\x04" encoded

let test_encode_uint32_be () =
  let encoded = to_string uint32be 0x01020304 in
  Alcotest.(check string) "uint32 be encoding" "\x01\x02\x03\x04" encoded

let test_encode_array () =
  let t = array ~len:(int 3) uint8 in
  let encoded = to_string t [ 1; 2; 3 ] in
  Alcotest.(check string) "array encoding" "\x01\x02\x03" encoded

let test_encode_byte_array () =
  let t = byte_array ~size:(int 5) in
  let encoded = to_string t "hello" in
  Alcotest.(check string) "byte_array encoding" "hello" encoded

let test_encode_variants () =
  let t = variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] uint8 in
  let encoded = to_string t `B in
  Alcotest.(check string) "variants encoding" "\x01" encoded

let test_encode_bitfield () =
  (* Default [bit_order] is [Msb_first]: a 6-bit field lives in the top
     6 bits of the base word. On a LE U32 base, writing 63 = 0x3F gives
     word = 0x3F lsl 26 = 0xFC000000, which serialises to [\x00\x00\x00\xFC]. *)
  let t = bits ~width:6 U32 in
  let encoded = to_string t 63 in
  Alcotest.(check string)
    "bitfield encoding (MSB-first)" "\x00\x00\x00\xFC" encoded

let test_encode_bitfield_lsb_first () =
  (* Explicit [~bit_order:Lsb_first] recovers the C bit-field packing: the
     value goes into the low 6 bits of the word. *)
  let t = bits ~bit_order:Lsb_first ~width:6 U32 in
  let encoded = to_string t 63 in
  Alcotest.(check string)
    "bitfield encoding (LSB-first)" "\x3F\x00\x00\x00" encoded

(* Adversarial bit-order tests: hardcoded byte positions anchor the new
   default [bit_order = Msb_first] so a regression in the shift logic
   surfaces as a noisy test failure, not a silent interop bug. *)

let test_bits_single_field_positions () =
  (* A 3-bit field holding value 0b101 = 5. *)
  let t_msb = bits ~width:3 U8 in
  let t_lsb = bits ~bit_order:Lsb_first ~width:3 U8 in
  let s_msb = to_string t_msb 5 in
  let s_lsb = to_string t_lsb 5 in
  (* Msb_first: value at bits 5..7 -> 0b1010_0000 = 0xA0. *)
  Alcotest.(check string) "msb-first single field" "\xA0" s_msb;
  (* Lsb_first: value at bits 0..2 -> 0b0000_0101 = 0x05. *)
  Alcotest.(check string) "lsb-first single field" "\x05" s_lsb

let test_bits_roundtrip_all_combos () =
  let check base width order expected =
    let t = bits ~bit_order:order ~width base in
    let s = to_string t expected in
    match of_string t s with
    | Ok got ->
        Alcotest.(check int) (Fmt.str "roundtrip width=%d" width) expected got
    | Error e -> Alcotest.failf "decode failed: %a" pp_parse_error e
  in
  let bases = [ (U8, 4); (U16, 5); (U16be, 6); (U32, 7); (U32be, 3) ] in
  let orders = [ Msb_first; Lsb_first ] in
  List.iter
    (fun (base, width) ->
      let max_val = (1 lsl width) - 1 in
      let values = [ 0; 1; max_val; max_val / 2 ] in
      List.iter (fun order -> List.iter (check base width order) values) orders)
    bases

(* -- Roundtrip tests -- *)

let test_roundtrip_uint8 () =
  let original = 0x42 in
  let encoded = to_string uint8 original in
  match of_string uint8 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint8" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint16 () =
  let original = 0x1234 in
  let encoded = to_string uint16 original in
  match of_string uint16 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint16" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_uint32 () =
  let original = 0x12345678 in
  let encoded = to_string uint32 original in
  match of_string uint32 encoded with
  | Ok decoded -> Alcotest.(check int) "roundtrip uint32" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_array () =
  let original = [ 1; 2; 3; 4; 5 ] in
  let t = array ~len:(int 5) uint8 in
  let encoded = to_string t original in
  match of_string t encoded with
  | Ok decoded -> Alcotest.(check (list int)) "roundtrip array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_roundtrip_byte_array () =
  let original = "hello" in
  let t = byte_array ~size:(int 5) in
  let encoded = to_string t original in
  match of_string t encoded with
  | Ok decoded ->
      Alcotest.(check string) "roundtrip byte_array" original decoded
  | Error e -> Alcotest.failf "%a" pp_parse_error e

(* -- Streaming: cross-slice boundary tests -- *)

(* Parse roundtrip with every chunk size forcing boundary straddles *)
let test_stream_uint8 () =
  let encoded = to_string uint8 42 in
  match parse_chunked ~chunk_size:1 uint8 encoded with
  | Ok v -> Alcotest.(check int) "uint8 chunk=1" 42 v
  | Error e -> Alcotest.failf "uint8 chunk=1: %a" pp_parse_error e

let test_stream_uint16_chunk1 () =
  let encoded = to_string uint16 0xCAFE in
  match parse_chunked ~chunk_size:1 uint16 encoded with
  | Ok v -> Alcotest.(check int) "uint16 chunk=1" 0xCAFE v
  | Error e -> Alcotest.failf "uint16 chunk=1: %a" pp_parse_error e

let test_stream_uint16_chunk3 () =
  (* chunk=3 means 2-byte value fits in one slice -- fast path *)
  let encoded = to_string uint16be 0xBEEF in
  match parse_chunked ~chunk_size:3 uint16be encoded with
  | Ok v -> Alcotest.(check int) "uint16be chunk=3" 0xBEEF v
  | Error e -> Alcotest.failf "uint16be chunk=3: %a" pp_parse_error e

let test_stream_uint32_chunk1 () =
  let encoded = to_string uint32 0xDEADBEEF in
  match parse_chunked ~chunk_size:1 uint32 encoded with
  | Ok v -> Alcotest.(check int) "uint32 chunk=1" 0xDEADBEEF v
  | Error e -> Alcotest.failf "uint32 chunk=1: %a" pp_parse_error e

let test_stream_uint32_chunk3 () =
  (* chunk=3: 4-byte value straddles at byte 3 *)
  let encoded = to_string uint32be 0x12345678 in
  match parse_chunked ~chunk_size:3 uint32be encoded with
  | Ok v -> Alcotest.(check int) "uint32be chunk=3" 0x12345678 v
  | Error e -> Alcotest.failf "uint32be chunk=3: %a" pp_parse_error e

let test_stream_uint64_chunk1 () =
  let encoded = to_string uint64 0x0102030405060708L in
  match parse_chunked ~chunk_size:1 uint64 encoded with
  | Ok v -> Alcotest.(check int64) "uint64 chunk=1" 0x0102030405060708L v
  | Error e -> Alcotest.failf "uint64 chunk=1: %a" pp_parse_error e

let test_stream_uint64_chunk3 () =
  let encoded = to_string uint64be 0xFEDCBA9876543210L in
  match parse_chunked ~chunk_size:3 uint64be encoded with
  | Ok v -> Alcotest.(check int64) "uint64be chunk=3" 0xFEDCBA9876543210L v
  | Error e -> Alcotest.failf "uint64be chunk=3: %a" pp_parse_error e

let test_stream_uint64_chunk5 () =
  let encoded = to_string uint64 0xAAAABBBBCCCCDDDDL in
  match parse_chunked ~chunk_size:5 uint64 encoded with
  | Ok v -> Alcotest.(check int64) "uint64 chunk=5" 0xAAAABBBBCCCCDDDDL v
  | Error e -> Alcotest.failf "uint64 chunk=5: %a" pp_parse_error e

let test_stream_bitfield_chunk1 () =
  (* Bitfield: 6+10+16 bits packed in a uint32 *)
  let bf = bits ~width:6 U32 in
  let encoded = to_string bf 42 in
  match parse_chunked ~chunk_size:1 bf encoded with
  | Ok v ->
      (* 42 written in top 6 bits of uint32: 42 << 26, then read back as 6-bit *)
      Alcotest.(check int) "bitfield chunk=1" 42 v
  | Error e -> Alcotest.failf "bitfield chunk=1: %a" pp_parse_error e

(* Encode roundtrip through chunked writer *)
let test_stream_encode_chunk1 () =
  let v = 0xDEADBEEF in
  let encoded = encode_chunked ~chunk_size:1 uint32be v in
  match of_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=1" v decoded
  | Error e -> Alcotest.failf "encode chunk=1: %a" pp_parse_error e

let test_stream_encode_chunk3 () =
  let v = 0x12345678 in
  let encoded = encode_chunked ~chunk_size:3 uint32be v in
  match of_string uint32be encoded with
  | Ok decoded -> Alcotest.(check int) "encode chunk=3" v decoded
  | Error e -> Alcotest.failf "encode chunk=3: %a" pp_parse_error e

(* -- Suite -- *)

let suite =
  ( "wire",
    [
      (* parsing *)
      Alcotest.test_case "parse: uint8" `Quick test_parse_uint8;
      Alcotest.test_case "parse: uint16 le" `Quick test_parse_uint16_le;
      Alcotest.test_case "parse: uint16 be" `Quick test_parse_uint16_be;
      Alcotest.test_case "parse: uint32 le" `Quick test_parse_uint32_le;
      Alcotest.test_case "parse: uint32 be" `Quick test_parse_uint32_be;
      Alcotest.test_case "parse: uint64 le" `Quick test_parse_uint64_le;
      Alcotest.test_case "parse: array" `Quick test_parse_array;
      Alcotest.test_case "parse: byte_array" `Quick test_parse_byte_array;
      Alcotest.test_case "parse: int8 negative" `Quick test_int8_negative;
      Alcotest.test_case "parse: int8 full range" `Quick test_int8_full_range;
      Alcotest.test_case "parse: int16be negative" `Quick test_int16be_negative;
      Alcotest.test_case "parse: int32be negative" `Quick test_int32be_negative;
      Alcotest.test_case "parse: int64le roundtrip" `Quick
        test_int64le_roundtrip;
      Alcotest.test_case "parse: float32be roundtrip" `Quick
        test_float32be_roundtrip;
      Alcotest.test_case "parse: float64le roundtrip" `Quick
        test_float64le_roundtrip;
      Alcotest.test_case "parse: float64 nan" `Quick test_float64_nan_roundtrip;
      Alcotest.test_case "is_finite: rejects nan" `Quick test_finite_rejects_nan;
      Alcotest.test_case "is_finite: accepts finite" `Quick test_finite_accepts;
      Alcotest.test_case "is_finite: rejects inf" `Quick test_finite_rejects_inf;
      Alcotest.test_case "parse: byte_array_where accepts" `Quick
        test_bawhere_accepts;
      Alcotest.test_case "parse: byte_array_where rejects" `Quick
        test_bawhere_rejects;
      Alcotest.test_case "encode: byte_array_where rejects" `Quick
        test_bawhere_enc_rejects;
      Alcotest.test_case "parse: variants valid" `Quick
        test_parse_variants_valid;
      Alcotest.test_case "parse: variants invalid" `Quick
        test_parse_variants_invalid;
      Alcotest.test_case "parse: all_bytes" `Quick test_parse_all_bytes;
      Alcotest.test_case "parse: all_zeros valid" `Quick
        test_parse_all_zeros_valid;
      Alcotest.test_case "parse: all_zeros invalid" `Quick
        test_parse_all_zeros_invalid;
      Alcotest.test_case "parse: bitfield" `Quick test_parse_bitfield;
      Alcotest.test_case "parse: eof error" `Quick test_parse_eof;
      Alcotest.test_case "parse: struct" `Quick test_parse_struct;
      Alcotest.test_case "parse: struct constraint" `Quick
        test_parse_struct_constraint;
      Alcotest.test_case "parse: struct constraint fail" `Quick
        test_parse_struct_constraint_fail;
      Alcotest.test_case "parse: struct action return true" `Quick
        test_parse_action_return_true;
      Alcotest.test_case "parse: struct action return false" `Quick
        test_parse_action_return_false;
      Alcotest.test_case "parse: struct action var" `Quick
        test_parse_struct_action_var;
      Alcotest.test_case "parse: struct action abort" `Quick
        test_parse_struct_action_abort;
      Alcotest.test_case "parse: param struct with params" `Quick
        test_parse_param_with_params;
      Alcotest.test_case "parse: param struct where fail" `Quick
        test_parse_param_where_fail;
      (* sizeof / sizeof_this / field_pos *)
      Alcotest.test_case "sizeof" `Quick test_sizeof;
      Alcotest.test_case "sizeof_this" `Quick test_sizeof_this;
      Alcotest.test_case "sizeof_this fail" `Quick test_sizeof_this_fail;
      Alcotest.test_case "field_pos" `Quick test_field_pos;
      Alcotest.test_case "field_pos fail" `Quick test_field_pos_fail;
      Alcotest.test_case "sizeof_this with action" `Quick
        test_sizeof_this_with_action;
      (* encoding *)
      Alcotest.test_case "encode: uint8" `Quick test_encode_uint8;
      Alcotest.test_case "encode: uint16 le" `Quick test_encode_uint16_le;
      Alcotest.test_case "encode: uint16 be" `Quick test_encode_uint16_be;
      Alcotest.test_case "encode: uint32 le" `Quick test_encode_uint32_le;
      Alcotest.test_case "encode: uint32 be" `Quick test_encode_uint32_be;
      Alcotest.test_case "encode: array" `Quick test_encode_array;
      Alcotest.test_case "encode: byte_array" `Quick test_encode_byte_array;
      Alcotest.test_case "encode: variants" `Quick test_encode_variants;
      Alcotest.test_case "encode: bitfield" `Quick test_encode_bitfield;
      Alcotest.test_case "encode: bitfield LSB-first" `Quick
        test_encode_bitfield_lsb_first;
      Alcotest.test_case "bits: single-field positions" `Quick
        test_bits_single_field_positions;
      Alcotest.test_case "bits: roundtrip all (base, bit_order)" `Quick
        test_bits_roundtrip_all_combos;
      (* roundtrip *)
      Alcotest.test_case "roundtrip: uint8" `Quick test_roundtrip_uint8;
      Alcotest.test_case "roundtrip: uint16" `Quick test_roundtrip_uint16;
      Alcotest.test_case "roundtrip: uint32" `Quick test_roundtrip_uint32;
      Alcotest.test_case "roundtrip: array" `Quick test_roundtrip_array;
      Alcotest.test_case "roundtrip: byte_array" `Quick
        test_roundtrip_byte_array;
      (* streaming: cross-slice boundary *)
      Alcotest.test_case "stream: uint8 chunk=1" `Quick test_stream_uint8;
      Alcotest.test_case "stream: uint16 chunk=1" `Quick
        test_stream_uint16_chunk1;
      Alcotest.test_case "stream: uint16 chunk=3" `Quick
        test_stream_uint16_chunk3;
      Alcotest.test_case "stream: uint32 chunk=1" `Quick
        test_stream_uint32_chunk1;
      Alcotest.test_case "stream: uint32 chunk=3" `Quick
        test_stream_uint32_chunk3;
      Alcotest.test_case "stream: uint64 chunk=1" `Quick
        test_stream_uint64_chunk1;
      Alcotest.test_case "stream: uint64 chunk=3" `Quick
        test_stream_uint64_chunk3;
      Alcotest.test_case "stream: uint64 chunk=5" `Quick
        test_stream_uint64_chunk5;
      Alcotest.test_case "stream: bitfield chunk=1" `Quick
        test_stream_bitfield_chunk1;
      Alcotest.test_case "stream: encode chunk=1" `Quick
        test_stream_encode_chunk1;
      Alcotest.test_case "stream: encode chunk=3" `Quick
        test_stream_encode_chunk3;
    ] )
