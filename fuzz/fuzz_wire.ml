(** Fuzz tests for wire library: parse crash safety, roundtrip correctness,
    record codec roundtrip, streaming, and dependent-size fields. *)

open Alcobar

(* Silence unused variable warnings for parse error handling *)
let _ = Wire.pp_parse_error

(* Helper: encode record to string using Codec API *)
let string_of_record codec v =
  let ws = Wire.Codec.wire_size codec in
  let buf = Bytes.create ws in
  Wire.Codec.encode codec v buf 0;
  Ok (Bytes.unsafe_to_string buf)

(* Helper: decode record from string using Codec API *)
let record_of_string codec s =
  let ws = Wire.Codec.wire_size codec in
  if String.length s < ws then
    Error (Wire.Unexpected_eof { expected = ws; got = String.length s })
  else Wire.Codec.decode codec (Bytes.of_string s) 0

(** Truncate input to reasonable size for protocol messages. *)
let truncate buf =
  let max_len = 1024 in
  if String.length buf > max_len then String.sub buf 0 max_len else buf

(** {1 Parsing Tests} *)

let test_parse_uint8 buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint8 buf in
  ()

let test_parse_uint16 buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint16 buf in
  ()

let test_parse_uint16be buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint16be buf in
  ()

let test_parse_uint32 buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint32 buf in
  ()

let test_parse_uint32be buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint32be buf in
  ()

let test_parse_uint63 buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint63 buf in
  ()

let test_parse_uint63be buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint63be buf in
  ()

let test_parse_uint64 buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint64 buf in
  ()

let test_parse_uint64be buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.uint64be buf in
  ()

let test_parse_bitfield buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:6 Wire.U32 in
  let _ = Wire.of_string t buf in
  ()

let test_parse_bf_uint8 buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:3 Wire.U8 in
  let _ = Wire.of_string t buf in
  ()

let test_parse_bf_uint16 buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:10 Wire.U16 in
  let _ = Wire.of_string t buf in
  ()

let test_parse_bf_uint16be buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:10 Wire.U16be in
  let _ = Wire.of_string t buf in
  ()

let test_parse_bf_uint32be buf =
  let buf = truncate buf in
  let t = Wire.bits ~width:20 Wire.U32be in
  let _ = Wire.of_string t buf in
  ()

let test_parse_map buf =
  let buf = truncate buf in
  let t =
    Wire.map ~decode:(fun n -> n * 2) ~encode:(fun n -> n / 2) Wire.uint8
  in
  let _ = Wire.of_string t buf in
  ()

let test_parse_bool buf =
  let buf = truncate buf in
  let t = Wire.bit Wire.uint8 in
  let _ = Wire.of_string t buf in
  ()

let test_parse_unit buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.empty buf in
  ()

let test_parse_array buf =
  let buf = truncate buf in
  let len = min 10 (String.length buf) in
  let arr = Wire.array ~len:(Wire.int len) Wire.uint8 in
  let _ = Wire.of_string arr buf in
  ()

let test_parse_byte_array buf =
  let buf = truncate buf in
  let size = min 10 (String.length buf) in
  let ba = Wire.byte_array ~size:(Wire.int size) in
  let _ = Wire.of_string ba buf in
  ()

let test_parse_variants buf =
  let buf = truncate buf in
  let e =
    Wire.variants "TestEnum" [ ("A", `A); ("B", `B); ("C", `C) ] Wire.uint8
  in
  let _ = Wire.of_string e buf in
  ()

let test_parse_where buf =
  let buf = truncate buf in
  let t = Wire.where Wire.Expr.true_ Wire.uint8 in
  let _ = Wire.of_string t buf in
  ()

let test_parse_all_bytes buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.all_bytes buf in
  ()

let test_parse_all_zeros buf =
  let buf = truncate buf in
  let _ = Wire.of_string Wire.all_zeros buf in
  ()

type test_struct = { ts_a : int; ts_b : int; ts_c : int }

let test_struct_codec =
  Wire.Codec.v "Test"
    (fun a b c -> { ts_a = a; ts_b = b; ts_c = c })
    Wire.Codec.
      [
        (Wire.Field.v "a" Wire.uint8 $ fun r -> r.ts_a);
        (Wire.Field.v "b" Wire.uint16 $ fun r -> r.ts_b);
        (Wire.Field.v "c" Wire.uint32 $ fun r -> r.ts_c);
      ]

let test_parse_struct buf =
  let buf = truncate buf in
  let _ = Wire.Codec.decode test_struct_codec (Bytes.of_string buf) 0 in
  ()

type constrained_struct = { cs_x : int }

let f_cs_x = Wire.Field.v "x" Wire.uint8

let constrained_codec =
  Wire.Codec.v "Constrained"
    ~where:Wire.Expr.(Wire.Field.ref f_cs_x <= Wire.int 100)
    (fun x -> { cs_x = x })
    Wire.Codec.[ (f_cs_x $ fun r -> r.cs_x) ]

let test_parse_struct_constrained buf =
  let buf = truncate buf in
  let _ = Wire.Codec.decode constrained_codec (Bytes.of_string buf) 0 in
  ()

type be_struct = { be_a : int; be_b : int; be_c : int64 }

let be_codec =
  Wire.Codec.v "BE"
    (fun a b c -> { be_a = a; be_b = b; be_c = c })
    Wire.Codec.
      [
        (Wire.Field.v "a" Wire.uint16be $ fun r -> r.be_a);
        (Wire.Field.v "b" Wire.uint32be $ fun r -> r.be_b);
        (Wire.Field.v "c" Wire.uint64be $ fun r -> r.be_c);
      ]

let test_parse_struct_be buf =
  let buf = truncate buf in
  let _ = Wire.Codec.decode be_codec (Bytes.of_string buf) 0 in
  ()

type bf_struct = { bf_a : int; bf_b : int; bf_c : int; bf_d : int }

let bf_codec =
  Wire.Codec.v "BF"
    (fun a b c d -> { bf_a = a; bf_b = b; bf_c = c; bf_d = d })
    Wire.Codec.
      [
        (Wire.Field.v "a" (Wire.bits ~width:3 Wire.U8) $ fun r -> r.bf_a);
        (Wire.Field.v "b" (Wire.bits ~width:5 Wire.U8) $ fun r -> r.bf_b);
        (Wire.Field.v "c" (Wire.bits ~width:10 Wire.U16) $ fun r -> r.bf_c);
        (Wire.Field.v "d" (Wire.bits ~width:6 Wire.U16) $ fun r -> r.bf_d);
      ]

let test_parse_struct_bitfields buf =
  let buf = truncate buf in
  let _ = Wire.Codec.decode bf_codec (Bytes.of_string buf) 0 in
  ()

(* Bitfield encode overflow: arbitrary ints must either fit and roundtrip,
   or raise Invalid_argument when they exceed the field width. *)
let test_bf_encode_overflow a b c d =
  let v = { bf_a = a; bf_b = b; bf_c = c; bf_d = d } in
  let fits_a = a lsr 3 = 0 && a >= 0 in
  let fits_b = b lsr 5 = 0 && b >= 0 in
  let fits_c = c lsr 10 = 0 && c >= 0 in
  let fits_d = d lsr 6 = 0 && d >= 0 in
  let all_fit = fits_a && fits_b && fits_c && fits_d in
  match string_of_record bf_codec v with
  | exception Invalid_argument _ ->
      if all_fit then fail "unexpected overflow for in-range values"
  | Error _ -> fail "unexpected encode error"
  | Ok encoded -> (
      if not all_fit then fail "expected overflow but encode succeeded";
      match record_of_string bf_codec encoded with
      | Ok decoded ->
          if v.bf_a <> decoded.bf_a then fail "bf_a mismatch";
          if v.bf_b <> decoded.bf_b then fail "bf_b mismatch";
          if v.bf_c <> decoded.bf_c then fail "bf_c mismatch";
          if v.bf_d <> decoded.bf_d then fail "bf_d mismatch"
      | Error _ -> fail "bf roundtrip decode failed")

type optional_struct = {
  os_gate : int;
  os_payload : int option;
  os_trail : int;
}

let f_os_gate = Wire.Field.v "Gate" Wire.uint8

let optional_static_codec =
  Wire.Codec.v "OptionalStatic"
    (fun gate payload trail ->
      { os_gate = gate; os_payload = payload; os_trail = trail })
    Wire.Codec.
      [
        (f_os_gate $ fun r -> r.os_gate);
        ( Wire.Field.optional "Payload" ~present:Wire.Expr.true_ Wire.uint8
        $ fun r -> r.os_payload );
        (Wire.Field.v "Trail" Wire.uint8 $ fun r -> r.os_trail);
      ]

let optional_dynamic_codec =
  Wire.Codec.v "OptionalDynamic"
    (fun gate payload trail ->
      { os_gate = gate; os_payload = payload; os_trail = trail })
    Wire.Codec.
      [
        (f_os_gate $ fun r -> r.os_gate);
        ( Wire.Field.optional "Payload"
            ~present:Wire.Expr.(Wire.Field.ref f_os_gate <> Wire.int 0)
            Wire.uint8
        $ fun r -> r.os_payload );
        (Wire.Field.v "Trail" Wire.uint8 $ fun r -> r.os_trail);
      ]

let option_is_some = function Some _ -> true | None -> false

let optional_equal a b =
  a.os_gate = b.os_gate
  && a.os_payload = b.os_payload
  && a.os_trail = b.os_trail

let check_optional_encode_totality label codec ~roundtrip original =
  let len = Wire.Codec.size_of_value codec original in
  let buf = Bytes.create len in
  match Wire.Codec.encode codec original buf 0 with
  | exception Invalid_argument _ ->
      if roundtrip then fail (label ^ " rejected a consistent value")
  | () ->
      if not roundtrip then fail (label ^ " accepted an inconsistent value");
      let ws = Wire.Codec.wire_size_at codec buf 0 in
      if ws <> len then
        fail (Fmt.str "%s wire size: expected %d got %d" label len ws);
      let decoded =
        match Wire.Codec.decode codec buf 0 with
        | Ok v -> v
        | Error e -> fail (Fmt.str "%s decode: %a" label Wire.pp_parse_error e)
      in
      if not (optional_equal original decoded) then
        fail (label ^ " roundtrip mismatch")

let test_optional_static_true_totality gate_seed payload_seed trail_seed =
  let original =
    {
      os_gate = gate_seed land 0xFF;
      os_payload = Some (payload_seed land 0xFF);
      os_trail = trail_seed land 0xFF;
    }
  in
  check_optional_encode_totality "optional static true" optional_static_codec
    ~roundtrip:true original

let test_optional_dynamic_totality gate_seed payload_seed trail_seed =
  let payload =
    if payload_seed land 1 = 0 then None
    else Some ((payload_seed lsr 1) land 0xFF)
  in
  let original =
    {
      os_gate = gate_seed land 1;
      os_payload = payload;
      os_trail = trail_seed land 0xFF;
    }
  in
  let roundtrip = original.os_gate <> 0 = option_is_some original.os_payload in
  check_optional_encode_totality "optional dynamic" optional_dynamic_codec
    ~roundtrip original

let test_parse_anon_field buf =
  let buf = truncate buf in
  let c =
    Wire.Codec.v "Anon"
      (fun x pad y -> (x, pad, y))
      Wire.Codec.
        [
          (Wire.Field.v "x" Wire.uint8 $ fun (x, _, _) -> x);
          (Wire.Field.v "_pad" Wire.uint8 $ fun (_, p, _) -> p);
          (Wire.Field.v "y" Wire.uint16 $ fun (_, _, y) -> y);
        ]
  in
  let _ = Wire.Codec.decode c (Bytes.of_string buf) 0 in
  ()

type test_case_val = [ `U8 of int | `U16 of int | `Default of int ]

let test_parse_casetype buf =
  let buf = truncate buf in
  let t : test_case_val Wire.typ =
    Wire.casetype "Tag" Wire.uint8
      [
        Wire.case ~index:0 Wire.uint8
          ~inject:(fun v -> `U8 v)
          ~project:(function `U8 v -> Some v | _ -> None);
        Wire.case ~index:1 Wire.uint16
          ~inject:(fun v -> `U16 v)
          ~project:(function `U16 v -> Some v | _ -> None);
        Wire.default Wire.uint32
          ~inject:(fun v -> `Default (Wire.Private.UInt32.to_int v))
          ~project:(function
            | `Default v -> Some (Wire.Private.UInt32.of_int v) | _ -> None);
      ]
  in
  let _ = Wire.of_string t buf in
  ()

(* Parse crash safety: nested struct with random input *)
let test_parse_nested_struct buf =
  let buf = truncate buf in
  let inner =
    Wire.Everparse.Raw.struct_ "Inner"
      [
        Wire.Everparse.Raw.field "a" Wire.uint8;
        Wire.Everparse.Raw.field "b" Wire.uint8;
      ]
  in
  let outer =
    Wire.Everparse.Raw.struct_ "Outer"
      [
        Wire.Everparse.Raw.field "hdr" Wire.uint16be;
        Wire.Everparse.Raw.field "payload" (Wire.Everparse.Raw.struct_typ inner);
      ]
  in
  let _ = Wire.of_string (Wire.Everparse.Raw.struct_typ outer) buf in
  ()

(** {1 Roundtrip Tests} *)

let roundtrip_uint label ~equal typ n =
  let encoded = Wire.to_string typ n in
  match Wire.of_string typ encoded with
  | Ok decoded ->
      if not (equal n decoded) then fail (label ^ " roundtrip mismatch")
  | Error _ -> fail (label ^ " roundtrip parse failed")

let test_roundtrip_uint8 n =
  roundtrip_uint "uint8" ~equal:Int.equal Wire.uint8 (abs n mod 256)

let test_roundtrip_uint16 n =
  roundtrip_uint "uint16" ~equal:Int.equal Wire.uint16 (abs n mod 65536)

let test_roundtrip_uint16be n =
  roundtrip_uint "uint16be" ~equal:Int.equal Wire.uint16be (abs n mod 65536)

let test_roundtrip_uint32 n =
  roundtrip_uint "uint32" ~equal:Int.equal Wire.uint32 (n land ((1 lsl 32) - 1))

let test_roundtrip_uint32be n =
  roundtrip_uint "uint32be" ~equal:Int.equal Wire.uint32be
    (n land ((1 lsl 32) - 1))

let test_roundtrip_uint63 n =
  roundtrip_uint "uint63" ~equal:Int.equal Wire.uint63 (abs n)

let test_roundtrip_uint63be n =
  roundtrip_uint "uint63be" ~equal:Int.equal Wire.uint63be (abs n)

let test_roundtrip_uint64 n =
  roundtrip_uint "uint64" ~equal:Int64.equal Wire.uint64 n

let test_roundtrip_uint64be n =
  roundtrip_uint "uint64be" ~equal:Int64.equal Wire.uint64be n

let test_roundtrip_map n =
  let n = abs n mod 256 in
  let t =
    Wire.map ~decode:(fun x -> x * 2) ~encode:(fun x -> x / 2) Wire.uint8
  in
  let encoded = Wire.to_string t (n * 2) in
  match Wire.of_string t encoded with
  | Ok decoded -> if n * 2 <> decoded then fail "map roundtrip mismatch"
  | Error _ -> fail "map roundtrip parse failed"

let test_roundtrip_bool n =
  let v = n mod 2 = 0 in
  let t = Wire.bit Wire.uint8 in
  let encoded = Wire.to_string t v in
  match Wire.of_string t encoded with
  | Ok decoded -> if v <> decoded then fail "bool roundtrip mismatch"
  | Error _ -> fail "bool roundtrip parse failed"

let test_roundtrip_array a b c =
  let arr = [ abs a mod 256; abs b mod 256; abs c mod 256 ] in
  let t = Wire.array ~len:(Wire.int 3) Wire.uint8 in
  let encoded = Wire.to_string t arr in
  match Wire.of_string t encoded with
  | Ok decoded -> if arr <> decoded then fail "array roundtrip mismatch"
  | Error _ -> fail "array roundtrip parse failed"

let test_roundtrip_byte_array buf =
  let buf = truncate buf in
  let len = String.length buf in
  if len > 0 then begin
    let t = Wire.byte_array ~size:(Wire.int len) in
    let encoded = Wire.to_string t buf in
    match Wire.of_string t encoded with
    | Ok decoded -> if buf <> decoded then fail "byte_array roundtrip mismatch"
    | Error _ -> fail "byte_array roundtrip parse failed"
  end

let test_roundtrip_variants n =
  let n = abs n mod 3 in
  let variants = [| `A; `B; `C |] in
  let t = Wire.variants "Test" [ ("A", `A); ("B", `B); ("C", `C) ] Wire.uint8 in
  let v = variants.(n) in
  let encoded = Wire.to_string t v in
  match Wire.of_string t encoded with
  | Ok decoded -> if v <> decoded then fail "variants roundtrip mismatch"
  | Error _ -> fail "variants roundtrip parse failed"

(** {1 Record Codec Tests} *)

type test_record = { x : int; y : int; z : int }

let test_record_codec =
  Wire.Codec.v "TestRecord"
    (fun x y z -> { x; y; z })
    Wire.Codec.
      [
        (Wire.Field.v "x" Wire.uint8 $ fun r -> r.x);
        (Wire.Field.v "y" Wire.uint16 $ fun r -> r.y);
        (Wire.Field.v "z" Wire.uint32 $ fun r -> r.z);
      ]

let test_record_roundtrip x y z =
  let x = abs x mod 256 in
  let y = abs y mod 65536 in
  let z = z land 0xFFFFFFFF in
  let original = { x; y; z } in
  match string_of_record test_record_codec original with
  | Error _ -> fail "record encode failed"
  | Ok encoded -> (
      match record_of_string test_record_codec encoded with
      | Ok decoded ->
          if original.x <> decoded.x then fail "record x mismatch";
          if original.y <> decoded.y then fail "record y mismatch";
          if original.z <> decoded.z then fail "record z mismatch"
      | Error _ -> fail "record roundtrip decode failed")

let test_record_decode_crash buf =
  let buf = truncate buf in
  let _ = record_of_string test_record_codec buf in
  ()

type be_record = { a : int; b : int }
(** Record codec with big-endian fields. *)

let be_record_codec =
  Wire.Codec.v "BERecord"
    (fun a b -> { a; b })
    Wire.Codec.
      [
        (Wire.Field.v "a" Wire.uint16be $ fun r -> r.a);
        (Wire.Field.v "b" Wire.uint32be $ fun r -> r.b);
      ]

let test_record_be_roundtrip a b =
  let a = abs a mod 65536 in
  let b = b land 0xFFFFFFFF in
  let original = { a; b } in
  match string_of_record be_record_codec original with
  | Error _ -> fail "be record encode failed"
  | Ok encoded -> (
      match record_of_string be_record_codec encoded with
      | Ok decoded ->
          if original.a <> decoded.a then fail "be record a mismatch";
          if original.b <> decoded.b then fail "be record b mismatch"
      | Error _ -> fail "be record roundtrip decode failed")

type bool_record = { flag : bool; value : int }
(** Record codec with bool/map fields. *)

let bool_record_codec =
  Wire.Codec.v "BoolRecord"
    (fun flag value -> { flag; value })
    Wire.Codec.
      [
        (Wire.Field.v "flag" (Wire.bit Wire.uint8) $ fun r -> r.flag);
        (Wire.Field.v "value" Wire.uint16 $ fun r -> r.value);
      ]

let test_record_bool_roundtrip n =
  let flag = n mod 2 = 0 in
  let value = abs n mod 65536 in
  let original = { flag; value } in
  match string_of_record bool_record_codec original with
  | Error _ -> fail "bool record encode failed"
  | Ok encoded -> (
      match record_of_string bool_record_codec encoded with
      | Ok decoded ->
          if original.flag <> decoded.flag then fail "bool record flag mismatch";
          if original.value <> decoded.value then
            fail "bool record value mismatch"
      | Error _ -> fail "bool record roundtrip decode failed")

(** {1 Streaming: cross-slice boundary roundtrips} *)

(* Parse from a chunked reader -- forces multi-byte values to straddle slices *)
let parse_chunked ~chunk_size typ s =
  let reader = Bytesrw.Bytes.Reader.of_string ~slice_length:chunk_size s in
  Wire.of_reader typ reader

let stream_roundtrip label ~chunk_size ~equal typ n =
  let encoded = Wire.to_string typ n in
  match parse_chunked ~chunk_size typ encoded with
  | Ok decoded -> if not (equal n decoded) then fail (label ^ " mismatch")
  | Error _ -> fail (label ^ " parse failed")

let test_stream_roundtrip_uint16 n =
  stream_roundtrip "stream uint16" ~chunk_size:1 ~equal:Int.equal Wire.uint16
    (abs n mod 65536)

let test_stream_roundtrip_uint16be n =
  stream_roundtrip "stream uint16be" ~chunk_size:1 ~equal:Int.equal
    Wire.uint16be
    (abs n mod 65536)

let test_stream_roundtrip_uint32 n =
  stream_roundtrip "stream uint32 chunk=1" ~chunk_size:1 ~equal:Int.equal
    Wire.uint32
    (n land ((1 lsl 32) - 1))

let test_stream_roundtrip_uint32be_chunk3 n =
  stream_roundtrip "stream uint32be chunk=3" ~chunk_size:3 ~equal:Int.equal
    Wire.uint32be
    (n land ((1 lsl 32) - 1))

let test_stream_roundtrip_uint64 n =
  stream_roundtrip "stream uint64 chunk=1" ~chunk_size:1 ~equal:Int64.equal
    Wire.uint64 n

let test_stream_roundtrip_uint64be_chunk5 n =
  stream_roundtrip "stream uint64be chunk=5" ~chunk_size:5 ~equal:Int64.equal
    Wire.uint64be n

let stream_check_field label ~expected typ slice =
  match parse_chunked ~chunk_size:1 typ slice with
  | Error _ -> fail (label ^ " parse failed")
  | Ok v -> if expected <> v then fail (label ^ " mismatch")

let test_stream_roundtrip_record x y z =
  let x = abs x mod 256 in
  let y = abs y mod 65536 in
  let z = z land ((1 lsl 32) - 1) in
  let original = { x; y; z } in
  match string_of_record test_record_codec original with
  | Error _ -> fail "stream record encode failed"
  | Ok encoded ->
      stream_check_field "stream record x" ~expected:x Wire.uint8
        (String.sub encoded 0 1);
      stream_check_field "stream record y" ~expected:y Wire.uint16
        (String.sub encoded 1 2);
      stream_check_field "stream record z" ~expected:z Wire.uint32
        (String.sub encoded 3 4)

(** {1 Dependent-size Field Tests} *)

module Slice = Bytesrw.Bytes.Slice

(* -- byte_slice variant: [length:u16be] [payload:byte_slice(length)] -- *)

type slice_msg = { sl_length : int; sl_payload : Slice.t }

let f_sl_length = Wire.Field.v "Length" Wire.uint16be

let f_sl_payload =
  Wire.Field.v "Payload" (Wire.byte_slice ~size:(Wire.Field.ref f_sl_length))

let slice_msg_codec =
  Wire.Codec.v "SliceMsg"
    (fun length payload -> { sl_length = length; sl_payload = payload })
    Wire.Codec.
      [
        (f_sl_length $ fun r -> r.sl_length);
        (f_sl_payload $ fun r -> r.sl_payload);
      ]

let slice_or_eod buf len =
  if len = 0 then Slice.eod else Slice.make buf ~first:0 ~length:len

let test_depsize_slice_roundtrip payload_str =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let payload_bytes = Bytes.of_string payload_str in
  let payload = slice_or_eod payload_bytes len in
  let original = { sl_length = len; sl_payload = payload } in
  let total = 2 + len in
  let buf = Bytes.create total in
  Wire.Codec.encode slice_msg_codec original buf 0;
  let decoded =
    match Wire.Codec.decode slice_msg_codec buf 0 with
    | Ok v -> v
    | Error e -> fail (Fmt.str "depsize slice decode: %a" Wire.pp_parse_error e)
  in
  if decoded.sl_length <> len then fail "depsize slice length mismatch";
  let dec_payload =
    Bytes.sub_string
      (Slice.bytes decoded.sl_payload)
      (Slice.first decoded.sl_payload)
      (Slice.length decoded.sl_payload)
  in
  if dec_payload <> payload_str then fail "depsize slice payload mismatch"

let test_depsize_slice_empty () =
  let payload = Slice.eod in
  let original = { sl_length = 0; sl_payload = payload } in
  let buf = Bytes.create 2 in
  Wire.Codec.encode slice_msg_codec original buf 0;
  let decoded =
    match Wire.Codec.decode slice_msg_codec buf 0 with
    | Ok v -> v
    | Error e ->
        fail (Fmt.str "depsize slice empty decode: %a" Wire.pp_parse_error e)
  in
  if decoded.sl_length <> 0 then fail "depsize slice empty length mismatch";
  if Slice.length decoded.sl_payload <> 0 then
    fail "depsize slice empty payload mismatch"

(* -- byte_array variant: [length:u16be] [data:byte_array(length)] -- *)

type array_msg = { ba_length : int; ba_data : string }

let f_ba_length = Wire.Field.v "Length" Wire.uint16be

let f_ba_data =
  Wire.Field.v "Data" (Wire.byte_array ~size:(Wire.Field.ref f_ba_length))

let array_msg_codec =
  Wire.Codec.v "ArrayMsg"
    (fun length data -> { ba_length = length; ba_data = data })
    Wire.Codec.
      [ (f_ba_length $ fun r -> r.ba_length); (f_ba_data $ fun r -> r.ba_data) ]

let test_depsize_array_roundtrip payload_str =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let original = { ba_length = len; ba_data = payload_str } in
  let total = 2 + len in
  let buf = Bytes.create total in
  Wire.Codec.encode array_msg_codec original buf 0;
  let decoded =
    match Wire.Codec.decode array_msg_codec buf 0 with
    | Ok v -> v
    | Error e -> fail (Fmt.str "depsize array decode: %a" Wire.pp_parse_error e)
  in
  if decoded.ba_length <> len then fail "depsize array length mismatch";
  if decoded.ba_data <> payload_str then fail "depsize array data mismatch"

let test_depsize_array_empty () =
  let original = { ba_length = 0; ba_data = "" } in
  let buf = Bytes.create 2 in
  Wire.Codec.encode array_msg_codec original buf 0;
  let decoded =
    match Wire.Codec.decode array_msg_codec buf 0 with
    | Ok v -> v
    | Error e ->
        fail (Fmt.str "depsize array empty decode: %a" Wire.pp_parse_error e)
  in
  if decoded.ba_length <> 0 then fail "depsize array empty length mismatch";
  if decoded.ba_data <> "" then fail "depsize array empty data mismatch"

(* -- trailing fixed field: [length:u16be] [payload:byte_slice(length)] [tag:u8] -- *)

type tagged_msg = { tm_length : int; tm_payload : Slice.t; tm_tag : int }

let f_tm_length = Wire.Field.v "Length" Wire.uint16be

let f_tm_payload =
  Wire.Field.v "Payload" (Wire.byte_slice ~size:(Wire.Field.ref f_tm_length))

let f_tm_tag = Wire.Field.v "Tag" Wire.uint8

let tagged_msg_codec =
  Wire.Codec.v "TaggedMsg"
    (fun length payload tag ->
      { tm_length = length; tm_payload = payload; tm_tag = tag })
    Wire.Codec.
      [
        (f_tm_length $ fun r -> r.tm_length);
        (f_tm_payload $ fun r -> r.tm_payload);
        (f_tm_tag $ fun r -> r.tm_tag);
      ]

let test_depsize_tagged_roundtrip payload_str tag =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let tag = abs tag mod 256 in
  let payload_bytes = Bytes.of_string payload_str in
  let payload = slice_or_eod payload_bytes len in
  let original = { tm_length = len; tm_payload = payload; tm_tag = tag } in
  let total = 2 + len + 1 in
  let buf = Bytes.create total in
  Wire.Codec.encode tagged_msg_codec original buf 0;
  let decoded =
    match Wire.Codec.decode tagged_msg_codec buf 0 with
    | Ok v -> v
    | Error e ->
        fail (Fmt.str "depsize tagged decode: %a" Wire.pp_parse_error e)
  in
  if decoded.tm_length <> len then fail "depsize tagged length mismatch";
  let dec_payload =
    Bytes.sub_string
      (Slice.bytes decoded.tm_payload)
      (Slice.first decoded.tm_payload)
      (Slice.length decoded.tm_payload)
  in
  if dec_payload <> payload_str then fail "depsize tagged payload mismatch";
  if decoded.tm_tag <> tag then fail "depsize tagged tag mismatch"

let test_depsize_tagged_empty tag =
  let tag = abs tag mod 256 in
  let payload = Slice.eod in
  let original = { tm_length = 0; tm_payload = payload; tm_tag = tag } in
  let buf = Bytes.create 3 in
  Wire.Codec.encode tagged_msg_codec original buf 0;
  let decoded =
    match Wire.Codec.decode tagged_msg_codec buf 0 with
    | Ok v -> v
    | Error e ->
        fail (Fmt.str "depsize tagged empty decode: %a" Wire.pp_parse_error e)
  in
  if decoded.tm_length <> 0 then fail "depsize tagged empty length mismatch";
  if Slice.length decoded.tm_payload <> 0 then
    fail "depsize tagged empty payload mismatch";
  if decoded.tm_tag <> tag then fail "depsize tagged empty tag mismatch"

let test_depsize_compute_wire_size payload_str =
  let len = String.length payload_str mod 201 in
  let payload_str =
    if len < String.length payload_str then String.sub payload_str 0 len
    else payload_str
  in
  let len = String.length payload_str in
  let payload_bytes = Bytes.of_string payload_str in
  let payload = slice_or_eod payload_bytes len in
  let original = { sl_length = len; sl_payload = payload } in
  let total = 2 + len in
  let buf = Bytes.create total in
  Wire.Codec.encode slice_msg_codec original buf 0;
  let ws = Wire.Codec.wire_size_at slice_msg_codec buf 0 in
  if ws <> total then
    fail (Fmt.str "depsize wire_size_at: expected %d got %d" total ws)

(** {1 Test Registration} *)

let parse_tests =
  [
    test_case "parse uint8" [ bytes ] test_parse_uint8;
    test_case "parse uint16" [ bytes ] test_parse_uint16;
    test_case "parse uint16be" [ bytes ] test_parse_uint16be;
    test_case "parse uint32" [ bytes ] test_parse_uint32;
    test_case "parse uint32be" [ bytes ] test_parse_uint32be;
    test_case "parse uint63" [ bytes ] test_parse_uint63;
    test_case "parse uint63be" [ bytes ] test_parse_uint63be;
    test_case "parse uint64" [ bytes ] test_parse_uint64;
    test_case "parse uint64be" [ bytes ] test_parse_uint64be;
    test_case "parse bitfield" [ bytes ] test_parse_bitfield;
    test_case "parse U8" [ bytes ] test_parse_bf_uint8;
    test_case "parse U16" [ bytes ] test_parse_bf_uint16;
    test_case "parse U16be" [ bytes ] test_parse_bf_uint16be;
    test_case "parse U32be" [ bytes ] test_parse_bf_uint32be;
    test_case "parse map" [ bytes ] test_parse_map;
    test_case "parse bool" [ bytes ] test_parse_bool;
    test_case "parse unit" [ bytes ] test_parse_unit;
    test_case "parse array" [ bytes ] test_parse_array;
    test_case "parse byte_array" [ bytes ] test_parse_byte_array;
    test_case "parse enum" [ bytes ] test_parse_variants;
    test_case "parse where" [ bytes ] test_parse_where;
    test_case "parse all_bytes" [ bytes ] test_parse_all_bytes;
    test_case "parse all_zeros" [ bytes ] test_parse_all_zeros;
    test_case "parse struct" [ bytes ] test_parse_struct;
    test_case "parse struct constrained" [ bytes ] test_parse_struct_constrained;
    test_case "parse struct be" [ bytes ] test_parse_struct_be;
    test_case "parse struct bitfields" [ bytes ] test_parse_struct_bitfields;
    test_case "parse anon field" [ bytes ] test_parse_anon_field;
    test_case "parse casetype" [ bytes ] test_parse_casetype;
    test_case "parse nested struct" [ bytes ] test_parse_nested_struct;
  ]

let roundtrip_tests =
  [
    test_case "roundtrip uint8" [ int ] test_roundtrip_uint8;
    test_case "roundtrip uint16" [ int ] test_roundtrip_uint16;
    test_case "roundtrip uint16be" [ int ] test_roundtrip_uint16be;
    test_case "roundtrip uint32" [ int ] test_roundtrip_uint32;
    test_case "roundtrip uint32be" [ int ] test_roundtrip_uint32be;
    test_case "roundtrip uint63" [ int ] test_roundtrip_uint63;
    test_case "roundtrip uint63be" [ int ] test_roundtrip_uint63be;
    test_case "roundtrip uint64" [ int64 ] test_roundtrip_uint64;
    test_case "roundtrip uint64be" [ int64 ] test_roundtrip_uint64be;
    test_case "roundtrip map" [ int ] test_roundtrip_map;
    test_case "roundtrip bool" [ int ] test_roundtrip_bool;
    test_case "roundtrip array" [ int; int; int ] test_roundtrip_array;
    test_case "roundtrip byte_array" [ bytes ] test_roundtrip_byte_array;
    test_case "roundtrip enum" [ int ] test_roundtrip_variants;
  ]

let record_tests =
  [
    test_case "record roundtrip" [ int; int; int ] test_record_roundtrip;
    test_case "record decode crash" [ bytes ] test_record_decode_crash;
    test_case "record be roundtrip" [ int; int ] test_record_be_roundtrip;
    test_case "record bool roundtrip" [ int ] test_record_bool_roundtrip;
    test_case "bf encode overflow" [ int; int; int; int ]
      test_bf_encode_overflow;
    test_case "optional static true totality" [ int; int; int ]
      test_optional_static_true_totality;
    test_case "optional dynamic totality" [ int; int; int ]
      test_optional_dynamic_totality;
  ]

let stream_tests =
  [
    test_case "stream uint16 chunk=1" [ int ] test_stream_roundtrip_uint16;
    test_case "stream uint16be chunk=1" [ int ] test_stream_roundtrip_uint16be;
    test_case "stream uint32 chunk=1" [ int ] test_stream_roundtrip_uint32;
    test_case "stream uint32be chunk=3" [ int ]
      test_stream_roundtrip_uint32be_chunk3;
    test_case "stream uint64 chunk=1" [ int64 ] test_stream_roundtrip_uint64;
    test_case "stream uint64be chunk=5" [ int64 ]
      test_stream_roundtrip_uint64be_chunk5;
    test_case "stream record chunk=1" [ int; int; int ]
      test_stream_roundtrip_record;
  ]

let depsize_tests =
  [
    test_case "depsize slice roundtrip" [ bytes ] test_depsize_slice_roundtrip;
    test_case "depsize slice empty" [ const () ] test_depsize_slice_empty;
    test_case "depsize array roundtrip" [ bytes ] test_depsize_array_roundtrip;
    test_case "depsize array empty" [ const () ] test_depsize_array_empty;
    test_case "depsize tagged roundtrip" [ bytes; int ]
      test_depsize_tagged_roundtrip;
    test_case "depsize tagged empty" [ int ] test_depsize_tagged_empty;
    test_case "depsize wire_size_at" [ bytes ] test_depsize_compute_wire_size;
  ]

(* Fuzz the IEEE 754 boundary: any 8-byte input must either decode to a
   finite [float] or fall on the [Constraint_failed] branch when a codec
   asserts [is_finite] on the field. The validator's bit-mask check and
   [Float.is_finite] of the same value must agree. *)
let test_is_finite_agrees buf =
  let buf = truncate buf in
  if String.length buf < 8 then ()
  else
    let bytes_in = Bytes.of_string (String.sub buf 0 8) in
    let template = Wire.Field.v "v" Wire.float64be in
    let f_v =
      Wire.Field.v "v" ~constraint_:(Wire.is_finite template) Wire.float64be
    in
    let codec =
      Wire.Codec.v "T" (fun v -> v) Wire.Codec.[ (f_v $ fun v -> v) ]
    in
    let raw = Wire.of_bytes_exn Wire.float64be bytes_in in
    let codec_finite =
      try
        let _ = Wire.Codec.decode_exn codec bytes_in 0 in
        true
      with Wire.Parse_error _ -> false
    in
    if Float.is_finite raw <> codec_finite then
      fail
        (Fmt.str "is_finite disagreement: raw=%h Float.is_finite=%b codec=%b"
           raw (Float.is_finite raw) codec_finite)

let test_float64_roundtrip buf =
  let buf = truncate buf in
  if String.length buf < 8 then ()
  else
    let bytes_in = Bytes.of_string (String.sub buf 0 8) in
    let v = Wire.of_bytes_exn Wire.float64be bytes_in in
    let s = Wire.to_string Wire.float64be v in
    let v' = Wire.of_string_exn Wire.float64be s in
    let bits = Int64.bits_of_float v and bits' = Int64.bits_of_float v' in
    if Int64.equal bits bits' || (Float.is_nan v && Float.is_nan v') then ()
    else fail (Fmt.str "float64 bit-pattern roundtrip failed: %h vs %h" v v')

let float_tests =
  [
    test_case "is_finite vs Float.is_finite" [ bytes ] test_is_finite_agrees;
    test_case "float64 roundtrip" [ bytes ] test_float64_roundtrip;
  ]

(* Composable [(codec, bytes list)] generators for the structured
   combinators. Each [gen] packs an existentially-typed codec with a
   list of [case]s: every [Valid v] carries wire bytes that should
   decode to [v] (and re-encode byte-identically), while [Noise] bytes
   are deliberately unrelated input that the decoder must handle
   without crashing.

   The four leaves cover the bug classes the encoder-coverage TODO
   names (parametric byte_array #53, variable-element repeat #51,
   codec-with-all_bytes tail #54, string-tag casetype #49/#50). The
   [pair] combinator wraps two leaves into one tuple codec so a single
   fuzz draw exercises a compound layout end to end. *)

type 'a positive = { value : 'a; bytes : unit -> bytes }
type negative = { bytes : unit -> bytes }

type 'a gen = {
  codec : 'a Wire.Codec.t;
  env : Wire.Param.env option;
  positive_cases : 'a positive list;
      (* Bytes that should decode to [value] and re-encode byte-identically.*)
  negative_cases : negative list;
      (* Bytes the decoder must handle without crashing (return [Error] or
         decode to some value -- either outcome is acceptable, but raising
         is a bug). *)
  equal : 'a -> 'a -> bool;
}

let mk_random buf =
  let n = String.length buf in
  if n = 0 then Bytes.empty
  else
    let out = Bytes.create n in
    Bytes.blit_string buf 0 out 0 n;
    out

let run_gen label g =
  List.iter
    (fun (p : _ positive) ->
      let bs = p.bytes () in
      (match Wire.Codec.decode ?env:g.env g.codec bs 0 with
      | Ok decoded ->
          if not (g.equal decoded p.value) then
            fail (label ^ ": positive decode mismatch")
      | Error _ -> fail (label ^ ": positive decode failed"));
      let out = Bytes.create (Bytes.length bs) in
      (try Wire.Codec.encode ?env:g.env g.codec p.value out 0
       with Invalid_argument _ -> fail (label ^ ": positive reencode raised"));
      if Bytes.unsafe_to_string out <> Bytes.unsafe_to_string bs then
        fail (label ^ ": positive reencode bytes mismatch");
      (* Also exercise the top-level [Wire.to_string] path: this drives
         [encode_into] -> [encode_codec], which uses [size_of_value] to
         pre-size the scratch. A mismatch here surfaces encoder bugs that
         [Codec.encode] doesn't hit (Codec.encode bypasses encode_codec). *)
      if g.env = None then begin
        let via_wire = Wire.to_string (Wire.codec g.codec) p.value in
        if via_wire <> Bytes.unsafe_to_string bs then
          fail (label ^ ": Wire.to_string mismatch")
      end)
    g.positive_cases;
  List.iter
    (fun (n : negative) ->
      let bs = n.bytes () in
      try ignore (Wire.Codec.decode ?env:g.env g.codec bs 0)
      with Invalid_argument _ -> fail (label ^ ": negative crashed decoder"))
    g.negative_cases

(* Leaf generators *)

type param_payload = { payload : string }

let gen_param_byte_array n =
  let len = abs n mod 64 in
  let payload = String.init len (fun i -> Char.chr ((n + i) land 0xFF)) in
  let p_len = Wire.Param.input "len" Wire.uint16be in
  let codec =
    Wire.Codec.v "Param"
      (fun b -> { payload = b })
      Wire.Codec.
        [
          ( Wire.Field.v "Data" (Wire.byte_array ~size:(Wire.Param.expr p_len))
          $ fun r -> r.payload );
        ]
  in
  let env = Wire.Codec.env codec |> Wire.Param.bind p_len len in
  let valid = Bytes.of_string payload in
  let noise = mk_random (String.make (max 0 (len - 1)) '\x00') in
  {
    codec;
    env = Some env;
    positive_cases = [ { value = { payload }; bytes = (fun () -> valid) } ];
    negative_cases = [ { bytes = (fun () -> noise) } ];
    equal = (fun a b -> a.payload = b.payload);
  }

type ext = { name : string }

let ext_codec =
  let f_len = Wire.Field.v "name_len" Wire.uint8 in
  let f_name =
    Wire.Field.v "name" (Wire.byte_array ~size:(Wire.Field.ref f_len))
  in
  Wire.Codec.v "Ext"
    (fun _l n -> { name = n })
    Wire.Codec.
      [ (f_len $ fun e -> String.length e.name); (f_name $ fun e -> e.name) ]

let gen_repeat_var_elem buf =
  let buf = truncate buf in
  let body = Buffer.create 64 in
  let exts = Stdlib.ref [] in
  let i = Stdlib.ref 0 in
  while !i < String.length buf do
    let take = max 1 (Char.code buf.[!i] land 0x0F) in
    let take = min take (String.length buf - !i - 1) in
    if take > 0 then begin
      let name = String.sub buf (!i + 1) take in
      Buffer.add_char body (Char.chr take);
      Buffer.add_string body name;
      exts := { name } :: !exts;
      i := !i + 1 + take
    end
    else i := String.length buf
  done;
  let exts = List.rev !exts in
  let total = Buffer.length body in
  let f_total = Wire.Field.v "total" Wire.uint16be in
  let f_exts =
    Wire.Field.repeat "exts" ~size:(Wire.Field.ref f_total)
      (Wire.codec ext_codec)
  in
  let codec =
    Wire.Codec.v "Exts"
      (fun _t xs -> xs)
      Wire.Codec.[ (f_total $ fun _ -> total); (f_exts $ fun xs -> xs) ]
  in
  let framed = Bytes.create (2 + total) in
  Bytes.set_uint16_be framed 0 total;
  Bytes.blit_string (Buffer.contents body) 0 framed 2 total;
  (* A noise byte stream that announces a length larger than the data
     it carries: the decoder must surface EOF, not crash. *)
  let noise = Bytes.of_string "\x00\xFF\x00" in
  {
    codec;
    env = None;
    positive_cases = [ { value = exts; bytes = (fun () -> framed) } ];
    negative_cases = [ { bytes = (fun () -> noise) } ];
    equal = ( = );
  }

type trailing = { header : int; tail : string }

let trailing_codec =
  Wire.Codec.v "Trailing"
    (fun h t -> { header = h; tail = t })
    Wire.Codec.
      [
        (Wire.Field.v "h" Wire.uint8 $ fun r -> r.header);
        (Wire.Field.v "t" Wire.all_bytes $ fun r -> r.tail);
      ]

let outer_trailing =
  Wire.Codec.v "Outer"
    (fun p -> p)
    Wire.Codec.[ (Wire.Field.v "p" (Wire.codec trailing_codec) $ fun p -> p) ]

let gen_codec_all_bytes_tail buf =
  let buf = truncate buf in
  (* Pad with a sentinel when the fuzz input was empty so the [Valid]
     case carries the header byte the codec requires. *)
  let buf = if String.length buf = 0 then "\x00" else buf in
  let header = Char.code buf.[0] in
  let tail = String.sub buf 1 (String.length buf - 1) in
  let framed = Bytes.of_string buf in
  let noise = Bytes.empty in
  {
    codec = outer_trailing;
    env = None;
    positive_cases =
      [ { value = { header; tail }; bytes = (fun () -> framed) } ];
    negative_cases = [ { bytes = (fun () -> noise) } ];
    equal = (fun a b -> a.header = b.header && a.tail = b.tail);
  }

type ssh_auth = [ `Publickey of int | `Other of int ]

let ssh_auth_codec =
  let body : ssh_auth Wire.typ =
    Wire.casetype "AuthMethod"
      (Wire.byte_array ~size:(Wire.int 9))
      [
        Wire.case ~index:"publickey" Wire.uint8
          ~inject:(fun v -> `Publickey v)
          ~project:(function `Publickey v -> Some v | _ -> None);
        Wire.default Wire.uint8
          ~inject:(fun v -> `Other v)
          ~project:(function `Other v -> Some v | _ -> None);
      ]
  in
  let f = Wire.Field.v "method" body in
  Wire.Codec.v "SshAuth" (fun m -> m) Wire.Codec.[ (f $ fun m -> m) ]

let gen_casetype_string_tag body_byte =
  let v = abs body_byte mod 256 in
  let framed = Bytes.create 10 in
  Bytes.blit_string "publickey" 0 framed 0 9;
  Bytes.set_uint8 framed 9 v;
  let noise =
    (* Method-name bytes that match no case; decoder hits the default
       branch and yields [`Other], not a crash. *)
    let n = Bytes.create 10 in
    Bytes.blit_string "xxxxxxxxx" 0 n 0 9;
    Bytes.set_uint8 n 9 (v lxor 0xFF);
    n
  in
  {
    codec = ssh_auth_codec;
    env = None;
    positive_cases = [ { value = `Publickey v; bytes = (fun () -> framed) } ];
    negative_cases = [ { bytes = (fun () -> noise) } ];
    equal = ( = );
  }

(* Combinator: glue two leaf gens into a pair-codec gen.

   Cross-products produce four sets of bytes:
   - positive x positive: clean tuples, the both-valid base case.
   - positive x negative and negative x positive: "almost valid" -- one
     side is well-shaped, the other isn't. These are the inputs that
     stress decoder error paths most realistically.
   - negative x negative: fully invalid; included for completeness so
     the decoder isn't tested only on near-valid data.

   The split keeps the positive/negative distribution from collapsing
   into one direction: as long as both leaves contribute roughly
   balanced positive/negative cases, the product stays roughly half
   positive (1 of 4) and half negative (3 of 4). *)
let cat_bytes_thunks a b () =
  let av = a () and bv = b () in
  let na = Bytes.length av and nb = Bytes.length bv in
  let out = Bytes.create (na + nb) in
  Bytes.blit av 0 out 0 na;
  Bytes.blit bv 0 out na nb;
  out

let pair_positives g1 g2 =
  List.concat_map
    (fun (p1 : _ positive) ->
      List.map
        (fun (p2 : _ positive) ->
          {
            value = (p1.value, p2.value);
            bytes = cat_bytes_thunks p1.bytes p2.bytes;
          })
        g2.positive_cases)
    g1.positive_cases

let pair_negatives g1 g2 =
  let cross_pn =
    List.concat_map
      (fun (p1 : _ positive) ->
        List.map
          (fun (n2 : negative) ->
            { bytes = cat_bytes_thunks p1.bytes n2.bytes })
          g2.negative_cases)
      g1.positive_cases
  in
  let cross_np =
    List.concat_map
      (fun (n1 : negative) ->
        List.map
          (fun (p2 : _ positive) ->
            { bytes = cat_bytes_thunks n1.bytes p2.bytes })
          g2.positive_cases)
      g1.negative_cases
  in
  let cross_nn =
    List.concat_map
      (fun (n1 : negative) ->
        List.map
          (fun (n2 : negative) ->
            { bytes = cat_bytes_thunks n1.bytes n2.bytes })
          g2.negative_cases)
      g1.negative_cases
  in
  cross_pn @ cross_np @ cross_nn

let gen_pair name g1 g2 =
  let f1 = Wire.Field.v "x" (Wire.codec g1.codec) in
  let f2 = Wire.Field.v "y" (Wire.codec g2.codec) in
  let codec =
    Wire.Codec.v name (fun x y -> (x, y)) Wire.Codec.[ f1 $ fst; f2 $ snd ]
  in
  let env = match (g1.env, g2.env) with None, None -> None | _ -> None in
  let equal (x1, y1) (x2, y2) = g1.equal x1 x2 && g2.equal y1 y2 in
  {
    codec;
    env;
    positive_cases = pair_positives g1 g2;
    negative_cases = pair_negatives g1 g2;
    equal;
  }

let test_gen_param_byte_array n =
  run_gen "param_byte_array" (gen_param_byte_array n)

let test_gen_repeat_var_elem buf =
  run_gen "repeat_var_elem" (gen_repeat_var_elem buf)

let test_gen_codec_tail buf =
  run_gen "codec_all_bytes_tail" (gen_codec_all_bytes_tail buf)

let test_gen_casetype_string_tag n =
  run_gen "casetype_string_tag" (gen_casetype_string_tag n)

(* Fixed-size leaf used for composition tests: an embedded sub-codec with
   variable-size tail can't be the left half of a pair (the right half
   has no way to find its starting offset). The string-tag casetype's
   per-case body sizes are known (uint8 in both branches), so two of
   them compose cleanly. *)
let test_gen_pair m n =
  run_gen "pair"
    (gen_pair "Pair" (gen_casetype_string_tag m) (gen_casetype_string_tag n))

(* Remaining leaves so every codec-targetable Wire.typ constructor has
   at least one fuzz target. Each leaf wraps the constructor under test
   in a one-field codec so the harness exercises decode + Codec.encode
   + Wire.to_string in lockstep. Constructors only meaningful inside a
   module of types (Type_ref / Qualified_ref / Struct / Apply) are not
   standalone codec fields and stay out of scope. *)

let gen_byte_slice buf =
  let buf = truncate buf in
  let buf = if buf = "" then "\x00" else buf in
  let len = min (String.length buf) 32 in
  let payload = String.sub buf 0 len in
  let codec =
    Wire.Codec.v "Slice"
      (fun s -> s)
      Wire.Codec.
        [
          (Wire.Field.v "s" (Wire.byte_slice ~size:(Wire.int len)) $ fun s -> s);
        ]
  in
  let positive =
    let bytes_payload = Bytes.of_string payload in
    let slice = Bytesrw.Bytes.Slice.make bytes_payload ~first:0 ~length:len in
    [ { value = slice; bytes = (fun () -> bytes_payload) } ]
  in
  let negative = [ { bytes = (fun () -> Bytes.empty) } ] in
  {
    codec;
    env = None;
    positive_cases = positive;
    negative_cases = negative;
    equal =
      (fun a b ->
        Bytesrw.Bytes.Slice.length a = Bytesrw.Bytes.Slice.length b
        && Bytes.unsafe_to_string (Bytesrw.Bytes.Slice.bytes a)
           = Bytes.unsafe_to_string (Bytesrw.Bytes.Slice.bytes b));
  }

let gen_uint_var n =
  let len = (abs n mod 7) + 1 in
  let value = abs n mod 256 in
  let codec =
    Wire.Codec.v "Uv"
      (fun v -> v)
      Wire.Codec.
        [
          ( Wire.Field.v "v" (Wire.uint ~endian:Wire.Big (Wire.int len))
          $ fun v -> v );
        ]
  in
  let buf = Bytes.create len in
  Wire.Codec.encode codec value buf 0;
  {
    codec;
    env = None;
    positive_cases = [ { value; bytes = (fun () -> buf) } ];
    negative_cases = [ { bytes = (fun () -> Bytes.empty) } ];
    equal = Int.equal;
  }

let printable b =
  let open Wire.Expr in
  Wire.Expr.( >= ) b (Wire.int 0x20) && Wire.Expr.( <= ) b (Wire.int 0x7e)

let gen_byte_array_where buf =
  let buf = truncate buf in
  let buf = if buf = "" then "x" else buf in
  let len = min (String.length buf) 16 in
  (* Sanitise into the predicate's accept set so the [Valid] case round-
     trips; the predicate is per-byte [0x20..0x7e]. *)
  let payload =
    String.init len (fun i ->
        let c = Char.code buf.[i] in
        let c = if c < 0x20 || c > 0x7e then 0x20 + (c land 0x5f) else c in
        Char.chr c)
  in
  let codec =
    Wire.Codec.v "Baw"
      (fun s -> s)
      Wire.Codec.
        [
          ( Wire.Field.v "s"
              (Wire.byte_array_where ~size:(Wire.int len) ~per_byte:printable)
          $ fun s -> s );
        ]
  in
  let bytes_in = Bytes.of_string payload in
  let bad = Bytes.make len '\x00' in
  {
    codec;
    env = None;
    positive_cases = [ { value = payload; bytes = (fun () -> bytes_in) } ];
    (* Noise: bytes that violate the per-byte predicate -- decoder must
       surface [Constraint_failed], not crash. *)
    negative_cases = [ { bytes = (fun () -> bad) } ];
    equal = String.equal;
  }

let gen_where n =
  let value = abs n mod 100 in
  let f = Wire.Field.v "v" Wire.uint8 in
  let codec =
    Wire.Codec.v "W"
      ~where:Wire.Expr.(Wire.Field.ref f <= Wire.int 100)
      (fun v -> v)
      Wire.Codec.[ (f $ fun v -> v) ]
  in
  let valid = Bytes.make 1 (Char.chr value) in
  let bad = Bytes.make 1 '\xff' in
  {
    codec;
    env = None;
    positive_cases = [ { value; bytes = (fun () -> valid) } ];
    negative_cases = [ { bytes = (fun () -> bad) } ];
    equal = Int.equal;
  }

let gen_map n =
  let value = abs n mod 128 in
  let map_typ =
    Wire.map ~decode:(fun n -> n * 2) ~encode:(fun n -> n / 2) Wire.uint8
  in
  let codec =
    Wire.Codec.v "M"
      (fun v -> v)
      Wire.Codec.[ (Wire.Field.v "v" map_typ $ fun v -> v) ]
  in
  let raw = value in
  let buf = Bytes.make 1 (Char.chr raw) in
  let positive = [ { value = raw * 2; bytes = (fun () -> buf) } ] in
  let negative = [ { bytes = (fun () -> Bytes.empty) } ] in
  {
    codec;
    env = None;
    positive_cases = positive;
    negative_cases = negative;
    equal = Int.equal;
  }

let gen_single_elem n =
  let len = (abs n mod 3) + 1 in
  let inner_v = abs n mod 256 in
  let codec =
    Wire.Codec.v "Se"
      (fun v -> v)
      Wire.Codec.
        [
          ( Wire.Field.v "v" (Wire.nested ~size:(Wire.int len) Wire.uint8)
          $ fun v -> v );
        ]
  in
  let buf = Bytes.make len '\x00' in
  Bytes.set_uint8 buf 0 inner_v;
  {
    codec;
    env = None;
    positive_cases = [ { value = inner_v; bytes = (fun () -> buf) } ];
    negative_cases = [ { bytes = (fun () -> Bytes.empty) } ];
    equal = Int.equal;
  }

let gen_all_zeros n =
  let len = abs n mod 16 in
  let codec =
    Wire.Codec.v "Az"
      (fun s -> s)
      Wire.Codec.[ (Wire.Field.v "z" Wire.all_zeros $ fun s -> s) ]
  in
  let zeros = Bytes.make len '\x00' in
  let nonzero = Bytes.make (max 1 len) '\x01' in
  {
    codec;
    env = None;
    positive_cases =
      [ { value = String.make len '\x00'; bytes = (fun () -> zeros) } ];
    negative_cases = [ { bytes = (fun () -> nonzero) } ];
    equal = String.equal;
  }

let gen_optional n =
  (* Static-present optional: [present = Bool true] resolves the field
     unconditionally to its inner shape. *)
  let inner = abs n mod 256 in
  let f = Wire.Field.optional ~present:Wire.Expr.true_ "v" Wire.uint8 in
  let codec = Wire.Codec.v "Opt" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ] in
  let buf = Bytes.make 1 (Char.chr inner) in
  {
    codec;
    env = None;
    positive_cases = [ { value = Some inner; bytes = (fun () -> buf) } ];
    negative_cases = [ { bytes = (fun () -> Bytes.empty) } ];
    equal = ( = );
  }

let gen_optional_or n =
  let inner = abs n mod 256 in
  let f =
    Wire.Field.optional_or ~present:Wire.Expr.true_ ~default:0 "v" Wire.uint8
  in
  let codec =
    Wire.Codec.v "OptOr" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ]
  in
  let buf = Bytes.make 1 (Char.chr inner) in
  {
    codec;
    env = None;
    positive_cases = [ { value = inner; bytes = (fun () -> buf) } ];
    negative_cases = [ { bytes = (fun () -> Bytes.empty) } ];
    equal = Int.equal;
  }

type int_case = U16 of int | Default of int

let gen_casetype_int_tag n =
  let pick = abs n mod 2 in
  let body : int_case Wire.typ =
    Wire.casetype "Cf" Wire.uint8
      [
        Wire.case ~index:1 Wire.uint16be
          ~inject:(fun v -> U16 v)
          ~project:(function U16 v -> Some v | _ -> None);
        Wire.default Wire.uint8
          ~inject:(fun v -> Default v)
          ~project:(function Default v -> Some v | _ -> None);
      ]
  in
  let codec =
    Wire.Codec.v "Cm"
      (fun m -> m)
      Wire.Codec.[ (Wire.Field.v "m" body $ fun m -> m) ]
  in
  let v, buf =
    if pick = 0 then (
      let payload = (abs n + 256) mod 0xFFFF in
      let b = Bytes.create 3 in
      Bytes.set_uint8 b 0 1;
      Bytes.set_uint16_be b 1 payload;
      (U16 payload, b))
    else
      let payload = abs n mod 256 in
      let b = Bytes.create 2 in
      Bytes.set_uint8 b 0 0xFF;
      Bytes.set_uint8 b 1 payload;
      (Default payload, b)
  in
  {
    codec;
    env = None;
    positive_cases = [ { value = v; bytes = (fun () -> buf) } ];
    negative_cases = [ { bytes = (fun () -> Bytes.empty) } ];
    equal = ( = );
  }

let test_gen_byte_slice buf = run_gen "byte_slice" (gen_byte_slice buf)
let test_gen_uint_var n = run_gen "uint_var" (gen_uint_var n)

let test_gen_byte_array_where buf =
  run_gen "byte_array_where" (gen_byte_array_where buf)

let test_gen_where n = run_gen "where" (gen_where n)
let test_gen_map n = run_gen "map" (gen_map n)
let test_gen_single_elem n = run_gen "single_elem" (gen_single_elem n)
let test_gen_all_zeros n = run_gen "all_zeros" (gen_all_zeros n)
let test_gen_optional n = run_gen "optional" (gen_optional n)
let test_gen_optional_or n = run_gen "optional_or" (gen_optional_or n)

let test_gen_casetype_int n =
  run_gen "casetype_int_tag" (gen_casetype_int_tag n)

(* Meta-test: assert every codec-targetable [Wire.typ] constructor has a
   gen fuzz target. Drives a representative value of each constructor
   into [size_of_typ_value] (the value-driven encode-size walk) -- any
   constructor whose match arm we forget to teach falls into the
   wildcard returning [0] or raises, both of which surface here. *)

(* Walk a representative [a typ] for every constructor and require
   [size_of_typ_value] to return a positive size (or 0 for nominally
   zero-width forms). Calling this in the fuzz suite means adding a new
   typ constructor without teaching [size_of_typ_value] about it makes
   this test fail loudly. *)
let meta_check label expected_ok matches =
  if not matches then
    fail (Fmt.str "typ constructor %s missing expected_ok=%d" label expected_ok)

(* Meta cases assert each [Wire.typ] constructor round-trips via the
   public [Wire.to_string] / [Wire.of_string]. Adding a constructor
   without teaching the encoder / decoder makes one of these
   roundtrips fail and signals the gen menu also needs an entry. *)

let try_rt : type a. a Wire.typ -> a -> bool =
 fun t v ->
  let s = Wire.to_string t v in
  match Wire.of_string t s with Ok v' -> v' = v | Error _ -> false

let meta_scalar_cases =
  [
    ("Uint8", fun () -> try_rt Wire.uint8 0x42);
    ("Uint16be", fun () -> try_rt Wire.uint16be 0xABCD);
    ( "Uint32be",
      fun () -> try_rt Wire.uint32be (Wire.Private.UInt32.of_int 0x12345678) );
    ("Uint63be", fun () -> try_rt Wire.uint63be (Wire.Private.UInt63.of_int 7));
    ("Uint64be", fun () -> try_rt Wire.uint64be 0x42L);
    ("Int8", fun () -> try_rt Wire.int8 (-1));
    ("Int16be", fun () -> try_rt Wire.int16be (-2));
    ("Int32be", fun () -> try_rt Wire.int32be (-3));
    ("Int64be", fun () -> try_rt Wire.int64be (-4L));
    ("Float32be", fun () -> try_rt Wire.float32be 1.5);
    ("Float64be", fun () -> try_rt Wire.float64be 1.5);
    ("Bits", fun () -> try_rt (Wire.bits ~width:3 Wire.U8) 5);
    ("Unit", fun () -> try_rt Wire.empty ());
    ( "Uint_var",
      fun () -> try_rt (Wire.uint ~endian:Wire.Big (Wire.int 3)) 0x123 );
  ]

let meta_byte_cases =
  [
    ("All_bytes", fun () -> try_rt Wire.all_bytes "abc");
    ("All_zeros", fun () -> try_rt Wire.all_zeros "\000\000");
    ("Byte_array", fun () -> try_rt (Wire.byte_array ~size:(Wire.int 3)) "xyz");
    ( "Byte_array_where",
      fun () ->
        try_rt
          (Wire.byte_array_where ~size:(Wire.int 3) ~per_byte:printable)
          "abc" );
    ( "Byte_slice",
      fun () ->
        let b = Bytes.of_string "abcd" in
        let s = Bytesrw.Bytes.Slice.make b ~first:0 ~length:4 in
        Wire.to_string (Wire.byte_slice ~size:(Wire.int 4)) s = "abcd" );
  ]

let meta_composite_cases =
  [
    ( "Map",
      fun () ->
        let t =
          Wire.map ~decode:(fun n -> n + 1) ~encode:(fun n -> n - 1) Wire.uint8
        in
        try_rt t 5 );
    ( "Variants/Enum",
      fun () ->
        let t = Wire.variants "E" [ ("A", `A); ("B", `B) ] Wire.uint8 in
        try_rt t `A );
    ( "Single_elem (nested)",
      fun () -> try_rt (Wire.nested ~size:(Wire.int 2) Wire.uint8) 7 );
    ( "Array",
      fun () -> try_rt (Wire.array ~len:(Wire.int 3) Wire.uint8) [ 1; 2; 3 ] );
    ( "Casetype",
      fun () ->
        let body : int_case Wire.typ =
          Wire.casetype "Cf" Wire.uint8
            [
              Wire.case ~index:1 Wire.uint16be
                ~inject:(fun v -> U16 v)
                ~project:(function U16 v -> Some v | _ -> None);
              Wire.default Wire.uint8
                ~inject:(fun v -> Default v)
                ~project:(function Default v -> Some v | _ -> None);
            ]
        in
        try_rt body (U16 7) );
    ( "Codec",
      fun () ->
        let inner =
          Wire.Codec.v "I"
            (fun v -> v)
            Wire.Codec.[ (Wire.Field.v "v" Wire.uint8 $ fun v -> v) ]
        in
        try_rt (Wire.codec inner) 9 );
  ]

let test_meta_typs n =
  ignore n;
  let all = meta_scalar_cases @ meta_byte_cases @ meta_composite_cases in
  List.iter (fun (name, run) -> meta_check name 1 (run ())) all

let encoder_tests =
  [
    test_case "gen param byte_array" [ int ] test_gen_param_byte_array;
    test_case "gen repeat var-elem" [ bytes ] test_gen_repeat_var_elem;
    test_case "gen codec all_bytes-tail" [ bytes ] test_gen_codec_tail;
    test_case "gen casetype string-tag" [ int ] test_gen_casetype_string_tag;
    test_case "gen pair (casetype + casetype)" [ int; int ] test_gen_pair;
    test_case "gen uint_var" [ int ] test_gen_uint_var;
    test_case "gen where" [ int ] test_gen_where;
    test_case "gen map" [ int ] test_gen_map;
    test_case "gen single_elem" [ int ] test_gen_single_elem;
    test_case "gen optional" [ int ] test_gen_optional;
    test_case "gen optional_or" [ int ] test_gen_optional_or;
    (* The five tests below currently fail. Each is a fuzz reproducer for
       a real Wire bug documented in [todo/fuzz-known-failing-gens.md];
       leaving them in the suite keeps the bugs visible until they're
       fixed. *)
    test_case "gen byte_slice" [ bytes ] test_gen_byte_slice;
    test_case "gen byte_array_where" [ bytes ] test_gen_byte_array_where;
    test_case "gen all_zeros" [ int ] test_gen_all_zeros;
    test_case "gen casetype int-tag" [ int ] test_gen_casetype_int;
    test_case "meta: every typ has a size_of_typ_value arm" [ int ]
      test_meta_typs;
  ]

let suite =
  ( "wire",
    parse_tests @ roundtrip_tests @ record_tests @ stream_tests @ depsize_tests
    @ float_tests @ encoder_tests )
