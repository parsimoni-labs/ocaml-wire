(* Tests for codec.ml: repeat over a byte budget. *)

open Wire
open Test_helpers

(* -- Nested: Repeat typ: parse elements until byte budget exhausted --
   [container], [f_cnt_length], [repeat_codec] live in {!Test_helpers}. *)

let test_repeat_decode_empty () =
  (* length=0 -> no items *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 0 (UInt8.to_int r.length);
  Alcotest.(check int) "item count" 0 (List.length r.items)

let test_repeat_decode_one () =
  (* length=3 -> one inner (tag=1byte, value=2bytes) *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 3;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 3 (UInt8.to_int r.length);
  Alcotest.(check int) "item count" 1 (List.length r.items);
  let item = List.hd r.items in
  Alcotest.(check int) "item.tag" 0x42 (UInt8.to_int item.tag);
  Alcotest.(check int) "item.value" 0x1234 (UInt16.to_int item.value)

let test_repeat_decode_multiple () =
  (* length=9 -> three inner items (3 bytes each) *)
  let buf = Bytes.create 10 in
  Bytes.set_uint8 buf 0 9;
  (* item 0 *)
  Bytes.set_uint8 buf 1 0x01;
  Bytes.set_uint16_be buf 2 0x0001;
  (* item 1 *)
  Bytes.set_uint8 buf 4 0x02;
  Bytes.set_uint16_be buf 5 0x0002;
  (* item 2 *)
  Bytes.set_uint8 buf 7 0x03;
  Bytes.set_uint16_be buf 8 0x0003;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 9 (UInt8.to_int r.length);
  Alcotest.(check int) "item count" 3 (List.length r.items);
  List.iteri
    (fun i (item : inner) ->
      Alcotest.(check int)
        (Fmt.str "item[%d].tag" i) (i + 1) (UInt8.to_int item.tag);
      Alcotest.(check int)
        (Fmt.str "item[%d].value" i)
        (i + 1) (UInt16.to_int item.value))
    r.items

let test_repeat_encode () =
  let v =
    {
      length = UInt8.v 6;
      items =
        [
          { tag = UInt8.v 0x01; value = UInt16.v 0x0001 };
          { tag = UInt8.v 0x02; value = UInt16.v 0x0002 };
        ];
    }
  in
  let buf = Bytes.create 7 in
  Codec.encode repeat_codec v buf 0;
  Alcotest.(check int) "length byte" 6 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "item0.tag" 0x01 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "item0.value" 0x0001 (Bytes.get_uint16_be buf 2);
  Alcotest.(check int) "item1.tag" 0x02 (Bytes.get_uint8 buf 4);
  Alcotest.(check int) "item1.value" 0x0002 (Bytes.get_uint16_be buf 5)

let expect_repeat_encode_error label codec value =
  let buf = Bytes.make 8 '\x7f' in
  match Codec.encode codec value buf 0 with
  | () -> Alcotest.failf "%s: expected a repeat byte-budget error" label
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": names repeat") true
        (contains ~sub:"repeat" msg)

let test_repeat_exact_budget () =
  let f_size = Field.v "size" uint8 in
  let fixed =
    Codec.v "RepeatFixedBudget"
      (fun size items -> (size, items))
      Codec.
        [
          f_size $ fst;
          Field.repeat "items" ~size:(Field.ref f_size) uint16be $ snd;
        ]
  in
  (match Codec.decode fixed (Bytes.of_string "\x01\xaa") 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "fixed-width repeat accepted a remainder");
  expect_repeat_encode_error "fixed underrun" fixed (UInt8.v 6, [ UInt16.v 1 ]);
  expect_repeat_encode_error "fixed overshoot" fixed
    (UInt8.v 2, [ UInt16.v 1; UInt16.v 2 ]);
  let f_var_size = Field.v "size" uint8 in
  let variable =
    Codec.v "RepeatVariableBudget"
      (fun size items -> (size, items))
      Codec.
        [
          f_var_size $ fst;
          Field.repeat "items" ~size:(Field.ref f_var_size) zeroterm $ snd;
        ]
  in
  match Codec.decode variable (Bytes.of_string "\x01A\x00") 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "variable-width repeat crossed its byte budget"

let test_repeat_roundtrip () =
  let items =
    [
      { tag = UInt8.v 0x0A; value = UInt16.v 0x000A };
      { tag = UInt8.v 0x0B; value = UInt16.v 0x000B };
      { tag = UInt8.v 0x0C; value = UInt16.v 0x000C };
    ]
  in
  let original : container = { length = UInt8.v 9; items } in
  let buf = Bytes.create 10 in
  Codec.encode repeat_codec original buf 0;
  let decoded = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int)
    "length"
    (UInt8.to_int original.length)
    (UInt8.to_int decoded.length);
  Alcotest.(check int) "item count" 3 (List.length decoded.items);
  List.iter2
    (fun (orig : inner) (dec : inner) ->
      Alcotest.(check int) "tag" (UInt8.to_int orig.tag) (UInt8.to_int dec.tag);
      Alcotest.(check int)
        "value" (UInt16.to_int orig.value) (UInt16.to_int dec.value))
    original.items decoded.items

(* Repeat with fixed-size primitive elements *)

type int_container = { count : UInt8.t; values : UInt16.t list }

let f_ic_count = Field.v "Count" uint8

let repeat_int_codec =
  Codec.v "IntContainer"
    (fun count values -> { count; values })
    Codec.
      [
        (f_ic_count $ fun r -> r.count);
        ( Field.repeat "Values" ~size:(Field.ref f_ic_count) uint16be $ fun r ->
          r.values );
      ]

let test_repeat_primitive () =
  (* count=6 -> 3 uint16be values *)
  let buf = Bytes.create 7 in
  Bytes.set_uint8 buf 0 6;
  Bytes.set_uint16_be buf 1 0x1111;
  Bytes.set_uint16_be buf 3 0x2222;
  Bytes.set_uint16_be buf 5 0x3333;
  let r = decode_ok (Codec.decode repeat_int_codec buf 0) in
  Alcotest.(check int) "count" 6 (UInt8.to_int r.count);
  Alcotest.(check int) "n values" 3 (List.length r.values);
  Alcotest.(check (list int))
    "values" [ 0x1111; 0x2222; 0x3333 ]
    (List.map UInt16.to_int r.values)

(* [Codec.size_of_value] counts a [Field.repeat]'s elements, including under a
   dynamic byte budget, so a buffer sized from [size_of_value] holds the whole
   encoding. *)
let test_repeat_size_of_value () =
  let v =
    { count = UInt8.v 6; values = List.map UInt16.v [ 0x1111; 0x2222; 0x3333 ] }
  in
  (* Count (1) + 3 * uint16be (6) = 7 *)
  let n = Codec.size_of_value repeat_int_codec v in
  Alcotest.(check int) "size_of_value" 7 n;
  let buf = Bytes.create n in
  Codec.encode repeat_int_codec v buf 0;
  Alcotest.(check int)
    "wire_size_at" 7
    (Codec.wire_size_at repeat_int_codec buf 0);
  let r = decode_ok (Codec.decode repeat_int_codec buf 0) in
  Alcotest.(check (list int))
    "roundtrip"
    (List.map UInt16.to_int v.values)
    (List.map UInt16.to_int r.values)

(* Repeat with trailer after *)

type repeat_trailer = { len : UInt8.t; items : inner list; check : UInt8.t }

let f_rt_len = Field.v "Len" uint8

let repeat_trailer_codec =
  Codec.v "RepeatTrailer"
    (fun len items check -> { len; items; check })
    Codec.
      [
        (f_rt_len $ fun r -> r.len);
        ( Field.repeat "Items" ~size:(Field.ref f_rt_len) (codec inner_codec)
        $ fun r -> r.items );
        (Field.v "Check" uint8 $ fun r -> r.check);
      ]

let test_repeat_with_trailer () =
  (* len=6 -> two inner items (3 bytes each), then 1 byte trailer *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 6;
  Bytes.set_uint8 buf 1 0x01;
  Bytes.set_uint16_be buf 2 0x0001;
  Bytes.set_uint8 buf 4 0x02;
  Bytes.set_uint16_be buf 5 0x0002;
  Bytes.set_uint8 buf 7 0xFF;
  let r = decode_ok (Codec.decode repeat_trailer_codec buf 0) in
  Alcotest.(check int) "len" 6 (UInt8.to_int r.len);
  Alcotest.(check int) "item count" 2 (List.length r.items);
  Alcotest.(check int) "check" 0xFF (UInt8.to_int r.check)

(* Variable-size repeat: codec with dependent-size field *)

type var_inner = { len : UInt8.t; data : string }

let f_vi_len = Field.v "Len" uint8

let var_inner_codec =
  Codec.v "VarInner"
    (fun len data -> { len; data })
    Codec.
      [
        (f_vi_len $ fun r -> r.len);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_vi_len)) $ fun r ->
          r.data );
      ]

type var_container = { size : UInt16.t; items : var_inner list }

let f_vc_size = Field.v "Size" uint16be

let var_repeat_codec =
  Codec.v "VarContainer"
    (fun size items -> { size; items })
    Codec.
      [
        (f_vc_size $ fun r -> r.size);
        ( Field.repeat "Items" ~size:(Field.ref f_vc_size)
            (codec var_inner_codec)
        $ fun r -> r.items );
      ]

let test_repeat_variable_size_elements () =
  (* Two variable-length items: [len=2, "ab"] [len=3, "cde"] = 2+2+3+3 = 7 bytes *)
  let buf = Bytes.create 9 in
  Bytes.set_uint16_be buf 0 7;
  (* item 0: len=2, data="ab" *)
  Bytes.set_uint8 buf 2 2;
  Bytes.blit_string "ab" 0 buf 3 2;
  (* item 1: len=3, data="cde" *)
  Bytes.set_uint8 buf 5 3;
  Bytes.blit_string "cde" 0 buf 6 3;
  let r = decode_ok (Codec.decode var_repeat_codec buf 0) in
  Alcotest.(check int) "size" 7 (UInt16.to_int r.size);
  Alcotest.(check int) "item count" 2 (List.length r.items);
  let i0 = List.nth r.items 0 in
  let i1 = List.nth r.items 1 in
  Alcotest.(check int) "item0.len" 2 (UInt8.to_int i0.len);
  Alcotest.(check string) "item0.data" "ab" i0.data;
  Alcotest.(check int) "item1.len" 3 (UInt8.to_int i1.len);
  Alcotest.(check string) "item1.data" "cde" i1.data

let suite =
  ( "codec_repeat",
    [
      Alcotest.test_case "repeat: decode empty" `Quick test_repeat_decode_empty;
      Alcotest.test_case "repeat: decode one" `Quick test_repeat_decode_one;
      Alcotest.test_case "repeat: decode multiple" `Quick
        test_repeat_decode_multiple;
      Alcotest.test_case "repeat: encode" `Quick test_repeat_encode;
      Alcotest.test_case "repeat: exact byte budget" `Quick
        test_repeat_exact_budget;
      Alcotest.test_case "repeat: roundtrip" `Quick test_repeat_roundtrip;
      Alcotest.test_case "repeat: primitive" `Quick test_repeat_primitive;
      Alcotest.test_case "repeat: size_of_value" `Quick
        test_repeat_size_of_value;
      Alcotest.test_case "repeat: with trailer" `Quick test_repeat_with_trailer;
      Alcotest.test_case "repeat: variable size elements" `Quick
        test_repeat_variable_size_elements;
    ] )
