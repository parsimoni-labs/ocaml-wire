(* Tests for codec.ml: optional fields. *)

open Wire
open Test_helpers

(* -- Nested: Optional typ: conditional field presence --
   [opt_record], [opt_codec], [opt_codec_present], [opt_codec_absent] live
   in {!Test_helpers}. *)

let test_optional_present_decode () =
  (* hdr(1) + payload(2) + trail(1) = 4 bytes *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xBB;
  let r = decode_ok (Codec.decode opt_codec_present buf 0) in
  Alcotest.(check int) "hdr" 0xAA (UInt8.to_int r.hdr);
  Alcotest.(check (option int))
    "payload" (Some 0x1234)
    (Option.map UInt16.to_int r.payload);
  Alcotest.(check int) "trail" 0xBB (UInt8.to_int r.trail)

let test_optional_absent_decode () =
  (* hdr(1) + trail(1) = 2 bytes (no payload) *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  let r = decode_ok (Codec.decode opt_codec_absent buf 0) in
  Alcotest.(check int) "hdr" 0xAA (UInt8.to_int r.hdr);
  Alcotest.(check (option int))
    "payload" None
    (Option.map UInt16.to_int r.payload);
  Alcotest.(check int) "trail" 0xBB (UInt8.to_int r.trail)

let test_optional_present_encode () =
  let v : opt_record =
    {
      hdr = UInt8.v 0xAA;
      payload = Some (UInt16.v 0x1234);
      trail = UInt8.v 0xBB;
    }
  in
  let buf = Bytes.create 4 in
  Codec.encode opt_codec_present v buf 0;
  Alcotest.(check int) "hdr" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "payload" 0x1234 (Bytes.get_uint16_be buf 1);
  Alcotest.(check int) "trail" 0xBB (Bytes.get_uint8 buf 3)

let test_optional_absent_encode () =
  let v : opt_record =
    { hdr = UInt8.v 0xAA; payload = None; trail = UInt8.v 0xBB }
  in
  let buf = Bytes.create 2 in
  Codec.encode opt_codec_absent v buf 0;
  Alcotest.(check int) "hdr" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "trail" 0xBB (Bytes.get_uint8 buf 1)

let test_optional_present_roundtrip () =
  let original : opt_record =
    {
      hdr = UInt8.v 0x11;
      payload = Some (UInt16.v 0x2233);
      trail = UInt8.v 0x44;
    }
  in
  let buf = Bytes.create 4 in
  Codec.encode opt_codec_present original buf 0;
  let decoded = decode_ok (Codec.decode opt_codec_present buf 0) in
  Alcotest.(check int)
    "hdr"
    (UInt8.to_int original.hdr)
    (UInt8.to_int decoded.hdr);
  Alcotest.(check (option int))
    "payload"
    (Option.map UInt16.to_int original.payload)
    (Option.map UInt16.to_int decoded.payload);
  Alcotest.(check int)
    "trail"
    (UInt8.to_int original.trail)
    (UInt8.to_int decoded.trail)

let test_optional_absent_roundtrip () =
  let original : opt_record =
    { hdr = UInt8.v 0x11; payload = None; trail = UInt8.v 0x44 }
  in
  let buf = Bytes.create 2 in
  Codec.encode opt_codec_absent original buf 0;
  let decoded = decode_ok (Codec.decode opt_codec_absent buf 0) in
  Alcotest.(check int)
    "hdr"
    (UInt8.to_int original.hdr)
    (UInt8.to_int decoded.hdr);
  Alcotest.(check (option int))
    "payload"
    (Option.map UInt16.to_int original.payload)
    (Option.map UInt16.to_int decoded.payload);
  Alcotest.(check int)
    "trail"
    (UInt8.to_int original.trail)
    (UInt8.to_int decoded.trail)

(* A byte_array whose ~size reads an optional_or field. The size expression
   reads the optional_or's present-or-default value, so encode and decode agree
   on the span length. *)
let test_bytearray_sized_by_optional_or () =
  let f_gate = Field.v "gate" uint8 in
  let f_len =
    Field.optional_or "len"
      ~present:Expr.(Field.ref f_gate <> int 0)
      ~default:(UInt8.v 3) uint8
  in
  let f_data = Field.v "data" (byte_array ~size:(Field.ref f_len)) in
  let c =
    Codec.v "OoSized"
      (fun gate len data -> (gate, len, data))
      Codec.
        [
          (f_gate $ fun (g, _, _) -> g);
          (f_len $ fun (_, l, _) -> l);
          (f_data $ fun (_, _, d) -> d);
        ]
  in
  let rt v expected =
    let buf = Bytes.create (Codec.size_of_value c v) in
    Codec.encode c v buf 0;
    Alcotest.(check string) "encoded bytes" expected (Bytes.to_string buf);
    match Codec.decode c buf 0 with
    | Ok (g, l, d) ->
        let vg, vl, vd = v in
        Alcotest.(check bool)
          "round-trip" true
          (UInt8.equal g vg && UInt8.equal l vl && String.equal d vd)
    | Error e -> Alcotest.failf "decode: %a" pp_parse_error e
  in
  (* gate set: len present (2), data is 2 bytes *)
  rt (UInt8.v 1, UInt8.v 2, "ab") "\x01\x02ab";
  (* gate clear: len falls back to the default (3), data is 3 bytes *)
  rt (UInt8.zero, UInt8.v 3, "xyz") "\x00\x03xyz"

let test_optional_wire_size_present () =
  Alcotest.(check int) "wire_size present" 4 (Codec.wire_size opt_codec_present)

let test_optional_wire_size_absent () =
  Alcotest.(check int) "wire_size absent" 2 (Codec.wire_size opt_codec_absent)

(* Optional with codec inner type *)

type opt_codec_record = { hdr : UInt8.t; inner : inner option; trail : UInt8.t }

let opt_inner_codec ~present =
  Codec.v "OptCodecRecord"
    (fun hdr inner trail -> { hdr; inner; trail })
    Codec.
      [
        (Field.v "Hdr" uint8 $ fun r -> r.hdr);
        ( Field.optional "Inner" ~present:(bool present) (codec inner_codec)
        $ fun r -> r.inner );
        (Field.v "Trail" uint8 $ fun r -> r.trail);
      ]

let test_optional_codec_present () =
  let c = opt_inner_codec ~present:true in
  (* hdr(1) + inner(3) + trail(1) = 5 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  Bytes.set_uint8 buf 4 0xBB;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAA (UInt8.to_int r.hdr);
  (match r.inner with
  | None -> Alcotest.fail "expected Some"
  | Some inner ->
      Alcotest.(check int) "inner.tag" 0x42 (UInt8.to_int inner.tag);
      Alcotest.(check int) "inner.value" 0x1234 (UInt16.to_int inner.value));
  Alcotest.(check int) "trail" 0xBB (UInt8.to_int r.trail)

let test_optional_codec_absent () =
  let c = opt_inner_codec ~present:false in
  (* hdr(1) + trail(1) = 2 bytes *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAA (UInt8.to_int r.hdr);
  Alcotest.(check (option int))
    "inner" None
    (Option.map (fun (i : inner) -> UInt8.to_int i.tag) r.inner);
  Alcotest.(check int) "trail" 0xBB (UInt8.to_int r.trail)

(* Multiple optional fields (TM frame pattern) *)

type multi_opt = {
  data : UInt16.t;
  ocf : UInt32.t option;
  fecf : UInt16.t option;
}

let multi_opt_codec ~ocf ~fecf =
  Codec.v "MultiOpt"
    (fun data ocf fecf -> { data; ocf; fecf })
    Codec.
      [
        (Field.v "Data" uint16be $ fun r -> r.data);
        ( Field.optional "OCF"
            ~present:(if ocf then Expr.true_ else Expr.false_)
            uint32be
        $ fun r -> r.ocf );
        (Field.optional "FECF" ~present:(bool fecf) uint16be $ fun r -> r.fecf);
      ]

let test_optional_both_present () =
  let c = multi_opt_codec ~ocf:true ~fecf:true in
  (* data(2) + ocf(4) + fecf(2) = 8 *)
  let buf = Bytes.create 8 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_int32_be buf 2 0x22222222l;
  Bytes.set_uint16_be buf 6 0x3333;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 (UInt16.to_int r.data);
  Alcotest.(check (option int))
    "ocf" (Some 0x22222222)
    (Option.map UInt32.to_int r.ocf);
  Alcotest.(check (option int))
    "fecf" (Some 0x3333)
    (Option.map UInt16.to_int r.fecf)

let test_optional_both_absent () =
  let c = multi_opt_codec ~ocf:false ~fecf:false in
  (* data(2) only *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0x1111;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 (UInt16.to_int r.data);
  Alcotest.(check (option int)) "ocf" None (Option.map UInt32.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" None (Option.map UInt16.to_int r.fecf)

let test_optional_mixed () =
  let c = multi_opt_codec ~ocf:true ~fecf:false in
  (* data(2) + ocf(4) = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_int32_be buf 2 0x22222222l;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 (UInt16.to_int r.data);
  Alcotest.(check (option int))
    "ocf" (Some 0x22222222)
    (Option.map UInt32.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" None (Option.map UInt16.to_int r.fecf)

(* Dynamic optional: presence determined by a previously-parsed field. *)

type dyn_opt = { flags : UInt8.t; payload : UInt16.t option; trail : UInt8.t }

let f_do_flags = Field.v "Flags" uint8
let f_do_trail = Field.v "Trail" uint8

let dyn_opt_codec =
  Codec.v "DynOpt"
    (fun flags payload trail -> { flags; payload; trail })
    Codec.
      [
        (f_do_flags $ fun r -> r.flags);
        ( Field.optional "Payload"
            ~present:Expr.(Field.ref f_do_flags <> int 0)
            uint16be
        $ fun r -> r.payload );
        (f_do_trail $ fun r -> r.trail);
      ]

let test_dyn_opt_present () =
  (* flags=1 -> payload present. Layout: [01] [12 34] [FF] *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int) "flags" 1 (UInt8.to_int r.flags);
  Alcotest.(check (option int))
    "payload" (Some 0x1234)
    (Option.map UInt16.to_int r.payload);
  Alcotest.(check int) "trail" 0xFF (UInt8.to_int r.trail)

let test_dyn_opt_absent () =
  (* flags=0 -> payload absent. Layout: [00] [FF] *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0;
  Bytes.set_uint8 buf 1 0xFF;
  let r = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int) "flags" 0 (UInt8.to_int r.flags);
  Alcotest.(check (option int))
    "payload" None
    (Option.map UInt16.to_int r.payload);
  Alcotest.(check int) "trail" 0xFF (UInt8.to_int r.trail)

let test_dyn_opt_get_trail () =
  let cf_trail = Codec.(f_do_trail $ fun r -> r.trail) in
  let get_trail = Staged.unstage (Codec.get dyn_opt_codec cf_trail) in
  (* Present: trail at offset 3. *)
  let buf1 = Bytes.create 4 in
  Bytes.set_uint8 buf1 0 1;
  Bytes.set_uint16_be buf1 1 0x1234;
  Bytes.set_uint8 buf1 3 0xAA;
  Alcotest.(check int) "trail (present)" 0xAA (UInt8.to_int (get_trail buf1 0));
  (* Absent: trail at offset 1. *)
  let buf2 = Bytes.create 2 in
  Bytes.set_uint8 buf2 0 0;
  Bytes.set_uint8 buf2 1 0xBB;
  Alcotest.(check int) "trail (absent)" 0xBB (UInt8.to_int (get_trail buf2 0))

let check_dyn_opt_roundtrip label expected_len expected_bytes original =
  Alcotest.(check int)
    (label ^ " size_of_value") expected_len
    (Codec.size_of_value dyn_opt_codec original);
  let buf = Bytes.create expected_len in
  Codec.encode dyn_opt_codec original buf 0;
  Alcotest.(check string)
    (label ^ " bytes") expected_bytes (Bytes.to_string buf);
  Alcotest.(check int)
    (label ^ " wire_size_at") expected_len
    (Codec.wire_size_at dyn_opt_codec buf 0);
  let decoded = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int)
    (label ^ " flags")
    (UInt8.to_int original.flags)
    (UInt8.to_int decoded.flags);
  Alcotest.(check (option int))
    (label ^ " payload")
    (Option.map UInt16.to_int original.payload)
    (Option.map UInt16.to_int decoded.payload);
  Alcotest.(check int)
    (label ^ " trail")
    (UInt8.to_int original.trail)
    (UInt8.to_int decoded.trail)

let test_field_optional_dynamic_roundtrip () =
  check_dyn_opt_roundtrip "present" 4 "\x01\x12\x34\xFF"
    {
      flags = UInt8.v 1;
      payload = Some (UInt16.v 0x1234);
      trail = UInt8.v 0xFF;
    };
  check_dyn_opt_roundtrip "absent" 2 "\x00\xEE"
    { flags = UInt8.v 0; payload = None; trail = UInt8.v 0xEE }

let test_dyn_opt_reject_gate () =
  let check_reject label v =
    let len = Codec.size_of_value dyn_opt_codec v + 4 in
    match Codec.encode dyn_opt_codec v (Bytes.create len) 0 with
    | () -> Alcotest.failf "%s: encode unexpectedly succeeded" label
    | exception Invalid_argument _ -> ()
  in
  check_reject "gate true / value None"
    { flags = UInt8.v 1; payload = None; trail = UInt8.v 0xEE };
  check_reject "gate false / value Some"
    {
      flags = UInt8.v 0;
      payload = Some (UInt16.v 0x1234);
      trail = UInt8.v 0xEE;
    }

let test_encode_totality () =
  let check_exact label original =
    let len = Codec.size_of_value dyn_opt_codec original in
    let buf = Bytes.create len in
    Codec.encode dyn_opt_codec original buf 0;
    Alcotest.(check int)
      (label ^ " wire_size_at") len
      (Codec.wire_size_at dyn_opt_codec buf 0);
    if len > 0 then
      match Codec.encode dyn_opt_codec original (Bytes.create (len - 1)) 0 with
      | () -> Alcotest.failf "%s: short buffer accepted" label
      | exception Invalid_argument _ -> ()
  in
  check_exact "present"
    {
      flags = UInt8.v 1;
      payload = Some (UInt16.v 0x1234);
      trail = UInt8.v 0xFF;
    };
  check_exact "absent"
    { flags = UInt8.v 0; payload = None; trail = UInt8.v 0xEE }

(* Dynamic optional via Field.ref on a bool field -- the TM frame pattern.
   Field.ref now accepts 'a t, so a bool field created with [bit] can be
   referenced directly in expressions. *)

type tm_opt = {
  ocf_flag : bool;
  data : UInt16.t;
  ocf : UInt32.t option;
  trail : UInt8.t;
}

let f_to_ocf_flag = Field.v "OCFFlag" (bit (bits ~width:1 U8))

let tm_opt_codec =
  Codec.v "TmOpt"
    (fun ocf_flag _pad data ocf trail -> { ocf_flag; data; ocf; trail })
    Codec.
      [
        (f_to_ocf_flag $ fun r -> r.ocf_flag);
        (Field.v "Pad" (bits ~width:7 U8) $ fun _ -> 0);
        (Field.v "Data" uint16be $ fun r -> r.data);
        ( Field.optional "OCF"
            ~present:Expr.(Field.ref f_to_ocf_flag <> int 0)
            uint32be
        $ fun r -> r.ocf );
        (Field.v "Trail" uint8 $ fun r -> r.trail);
      ]

let test_dyn_opt_anyref_present () =
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0x80;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_int32_be buf 3 0xDEADBEEFl;
  Bytes.set_uint8 buf 7 0xFF;
  let r = decode_ok (Codec.decode tm_opt_codec buf 0) in
  Alcotest.(check bool) "ocf_flag" true r.ocf_flag;
  Alcotest.(check int) "data" 0x1234 (UInt16.to_int r.data);
  Alcotest.(check (option int32))
    "ocf" (Some 0xDEADBEEFl)
    (Option.map UInt32.to_int32 r.ocf);
  Alcotest.(check int) "trail" 0xFF (UInt8.to_int r.trail)

let test_dyn_opt_anyref_absent () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x00;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode tm_opt_codec buf 0) in
  Alcotest.(check bool) "ocf_flag" false r.ocf_flag;
  Alcotest.(check int) "data" 0x1234 (UInt16.to_int r.data);
  Alcotest.(check (option int)) "ocf" None (Option.map UInt32.to_int r.ocf);
  Alcotest.(check int) "trail" 0xFF (UInt8.to_int r.trail)

(* -- Predicates with bitwise/shift/mod operators in [optional] --
   Reproduces the silent miscompile where [compile_bool_expr]'s
   [try_compile_int_reader] fell back to [fun _ -> true] for any
   predicate using [Land] / [Lor] / [Lsr] / [Mod] / [Cast] / etc.,
   so a high-bit-gated optional would always read its payload. *)

type bit_gated = { flags : UInt8.t; body : UInt8.t option }

let f_bg_flags = Field.v "Flags" uint8

let bg_codec ~present =
  Codec.v "BgRec"
    (fun flags body -> { flags; body })
    Codec.
      [
        (f_bg_flags $ fun r -> r.flags);
        (Field.optional "Body" ~present uint8 $ fun r -> r.body);
      ]

(* Decode a present-payload buffer (expect [Some 0x2A]) and an absent buffer
   (expect [None]) through a [bg_codec] gated on [present]. *)
let check_bit_gated ~present ~present_buf ~present_label ~absent_buf
    ~absent_label =
  let c = bg_codec ~present in
  let r = decode_ok (Codec.decode c (Bytes.of_string present_buf) 0) in
  Alcotest.(check (option int))
    present_label (Some 0x2A)
    (Option.map UInt8.to_int r.body);
  let r = decode_ok (Codec.decode c (Bytes.of_string absent_buf) 0) in
  Alcotest.(check (option int))
    absent_label None
    (Option.map UInt8.to_int r.body)

let test_optional_land_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags land int 0x80 <> int 0)
    ~present_buf:"\x80\x2A" ~present_label:"high bit set" ~absent_buf:"\x00"
    ~absent_label:"high bit clear"

let test_optional_lsr_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags lsr int 4 <> int 0)
    ~present_buf:"\x10\x2A" ~present_label:"top nibble set" ~absent_buf:"\x0F"
    ~absent_label:"top nibble clear"

let test_optional_mod_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags mod int 2 <> int 0)
    ~present_buf:"\x03\x2A" ~present_label:"odd" ~absent_buf:"\x02"
    ~absent_label:"even"

let test_optional_lor_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags lor int 0x01 = int 0x01)
    ~present_buf:"\x01\x2A" ~present_label:"lor matches" ~absent_buf:"\xFF"
    ~absent_label:"lor no match"

(* [Field.ref] on an [optional] field reads the inner value, not 0: the
   int_array slot is populated for [Optional] fields, so a constraint or size
   expression referring to the optional sees the real value. *)

type ref_opt = { x : UInt8.t option; check : UInt8.t }

let f_ro_x = Field.optional "X" ~present:Expr.true_ uint8

let ref_opt_codec =
  let f_check =
    Field.v "Check" uint8 ~constraint_:Expr.(Field.ref f_ro_x > int 0)
  in
  Codec.v "RefOpt"
    (fun x check -> { x; check })
    Codec.[ (f_ro_x $ fun r -> r.x); (f_check $ fun r -> r.check) ]

let test_field_ref_through_optional () =
  (* x=0x80 (present); the constraint reads ref(x) and asserts it's
     non-zero. With the populate bug, ref(x) read 0 and the constraint
     failed; with the fix it reads 0x80 and the decode succeeds. *)
  let buf = Bytes.of_string "\x80\x7F" in
  let r = decode_ok (Codec.decode ref_opt_codec buf 0) in
  Alcotest.(check (option int)) "x" (Some 0x80) (Option.map UInt8.to_int r.x);
  Alcotest.(check int) "check" 0x7F (UInt8.to_int r.check)

let test_uint64_in_size_expr () =
  let f_len = Field.v "Len" uint64be in
  let codec =
    let open Codec in
    v "U64Ref"
      (fun len data -> (len, data))
      [
        (f_len $ fun (l, _) -> l);
        (Field.v "Data" (byte_array ~size:(Field.ref f_len)) $ fun (_, d) -> d);
      ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_int64_be buf 0 3L;
  Bytes.blit_string "ABC" 0 buf 8 3;
  let len, data = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int64) "len" 3L (UInt64.to_int64 len);
  Alcotest.(check string) "data" "ABC" data

let suite =
  ( "codec_optional",
    [
      Alcotest.test_case "optional: present decode" `Quick
        test_optional_present_decode;
      Alcotest.test_case "optional: absent decode" `Quick
        test_optional_absent_decode;
      Alcotest.test_case "optional: present encode" `Quick
        test_optional_present_encode;
      Alcotest.test_case "optional: absent encode" `Quick
        test_optional_absent_encode;
      Alcotest.test_case "optional: present roundtrip" `Quick
        test_optional_present_roundtrip;
      Alcotest.test_case "optional: absent roundtrip" `Quick
        test_optional_absent_roundtrip;
      Alcotest.test_case "optional: wire_size present" `Quick
        test_optional_wire_size_present;
      Alcotest.test_case "optional: wire_size absent" `Quick
        test_optional_wire_size_absent;
      Alcotest.test_case "byte_array sized by optional_or field" `Quick
        test_bytearray_sized_by_optional_or;
      Alcotest.test_case "optional: codec present" `Quick
        test_optional_codec_present;
      Alcotest.test_case "optional: codec absent" `Quick
        test_optional_codec_absent;
      Alcotest.test_case "optional: both present" `Quick
        test_optional_both_present;
      Alcotest.test_case "optional: both absent" `Quick
        test_optional_both_absent;
      Alcotest.test_case "optional: mixed" `Quick test_optional_mixed;
      Alcotest.test_case "optional: dynamic present" `Quick test_dyn_opt_present;
      Alcotest.test_case "optional: dynamic absent" `Quick test_dyn_opt_absent;
      Alcotest.test_case "optional: dynamic get trail" `Quick
        test_dyn_opt_get_trail;
      Alcotest.test_case "optional: dynamic roundtrip" `Quick
        test_field_optional_dynamic_roundtrip;
      Alcotest.test_case "optional: dynamic rejects inconsistent gate" `Quick
        test_dyn_opt_reject_gate;
      Alcotest.test_case "optional: encode size totality" `Quick
        test_encode_totality;
      Alcotest.test_case "optional: bool ref present" `Quick
        test_dyn_opt_anyref_present;
      Alcotest.test_case "optional: bool ref absent" `Quick
        test_dyn_opt_anyref_absent;
      Alcotest.test_case "optional: land predicate" `Quick
        test_optional_land_predicate;
      Alcotest.test_case "optional: lsr predicate" `Quick
        test_optional_lsr_predicate;
      Alcotest.test_case "optional: mod predicate" `Quick
        test_optional_mod_predicate;
      Alcotest.test_case "optional: lor predicate" `Quick
        test_optional_lor_predicate;
      Alcotest.test_case "optional: Field.ref reads inner value" `Quick
        test_field_ref_through_optional;
      Alcotest.test_case "uint64 in size expr" `Quick test_uint64_in_size_expr;
    ] )
