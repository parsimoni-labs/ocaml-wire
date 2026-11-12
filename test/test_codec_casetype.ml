(* Tests for codec.ml: casetype fields and repeated casetypes. *)

open Wire
open Test_helpers

(* -- Casetype as a trailing variable-size codec field -- *)

type ev_payload =
  [ `Login of UInt16.t | `Logout of UInt32.t | `Other of UInt8.t ]

let casetype_field_event_typ : ev_payload Wire.typ =
  Wire.casetype "EvPayload" Wire.uint8
    [
      Wire.case ~index:(UInt8.v 1) Wire.uint16be
        ~inject:(fun v -> `Login v)
        ~project:(function `Login v -> Some v | _ -> None);
      Wire.case ~index:(UInt8.v 2) Wire.uint32be
        ~inject:(fun v -> `Logout v)
        ~project:(function `Logout v -> Some v | _ -> None);
      Wire.default Wire.uint8
        ~inject:(fun _tag v -> `Other v)
        ~project:(function `Other v -> Some (UInt8.v 0xFF, v) | _ -> None);
    ]

type ev_event = { ts : int64; data : ev_payload }

let casetype_field_codec =
  Codec.v "CasetypeFieldEvt"
    (fun ts data -> { ts; data })
    Codec.
      [
        (Field.v "Timestamp" int64be $ fun e -> e.ts);
        (Field.v "Data" casetype_field_event_typ $ fun e -> e.data);
      ]

let test_casetype_field_login () =
  let buf = Bytes.create 11 in
  Bytes.set_int64_be buf 0 42L;
  Bytes.set_uint8 buf 8 1;
  Bytes.set_uint16_be buf 9 0x1234;
  let r = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check int64) "ts" 42L r.ts;
  Alcotest.(check bool) "Login 0x1234" true (r.data = `Login (UInt16.v 0x1234))

let test_casetype_field_logout () =
  let buf = Bytes.create 13 in
  Bytes.set_int64_be buf 0 99L;
  Bytes.set_uint8 buf 8 2;
  Bytes.set_int32_be buf 9 0x55667788l;
  let r = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check bool)
    "Logout" true
    (r.data = `Logout (UInt32.of_int32 0x55667788l))

let test_casetype_field_default () =
  let buf = Bytes.create 10 in
  Bytes.set_int64_be buf 0 0L;
  Bytes.set_uint8 buf 8 99;
  Bytes.set_uint8 buf 9 7;
  let r = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check bool) "Other 7" true (r.data = `Other (UInt8.v 7))

(* A casetype whose discriminant matches no case fails with a typed
   [Invalid_tag] carrying the tag value, not a stringly constraint failure. *)
let test_casetype_no_match_invalid_tag () =
  let typ =
    Wire.casetype "Closed" Wire.uint8
      [
        Wire.case ~index:(UInt8.v 1) Wire.uint8
          ~inject:(fun v -> `A v)
          ~project:(function `A v -> Some v | _ -> None);
        Wire.case ~index:(UInt8.v 2) Wire.uint8
          ~inject:(fun v -> `B v)
          ~project:(function `B v -> Some v | _ -> None);
      ]
  in
  let c = Codec.v "ClosedCt" Fun.id Codec.[ Field.v "d" typ $ Fun.id ] in
  match Codec.decode c (Bytes.of_string "\x63\x00") 0 with
  | Error { kind = Invalid_tag n; _ } ->
      Alcotest.(check int) "unmatched tag" 99 n
  | Ok _ -> Alcotest.fail "decode accepted an unmatched casetype tag"
  | Error _ -> Alcotest.fail "wrong error for an unmatched casetype tag"

(* A casetype's extent depends on which case the tag selects, so the record's
   bounds walk dispatches on the tag before any field reader runs, and that is
   where an unknown tag surfaces. The failure must still name the field: a
   caller repairing malformed input has only the path to go on. *)
let test_casetype_no_match_names_field () =
  let typ =
    Wire.casetype "Sized" Wire.uint8
      [
        Wire.case ~index:(UInt8.v 1) Wire.uint8
          ~inject:(fun v -> `A v)
          ~project:(function `A v -> Some v | _ -> None);
        Wire.case ~index:(UInt8.v 2) Wire.uint16
          ~inject:(fun v -> `B v)
          ~project:(function `B v -> Some v | _ -> None);
      ]
  in
  let c = Codec.v "Tagged" Fun.id Codec.[ Field.v "body" typ $ Fun.id ] in
  match Codec.decode c (Bytes.of_string "\x63\x00\x00") 0 with
  | Error { kind = Invalid_tag n; field; at } ->
      Alcotest.(check int) "unmatched tag" 99 n;
      Alcotest.(check int) "at the casetype field" 0 at;
      Alcotest.(check (list string)) "names the field" [ "body" ] field
  | Ok _ -> Alcotest.fail "decode accepted an unmatched casetype tag"
  | Error _ -> Alcotest.fail "wrong error for an unmatched casetype tag"

(* The default branch recovers the matched tag and re-encodes it, so an
   arbitrary unclaimed tag round-trips (the DHCP / TCP-options shape). *)
type tlv = Known of UInt16.t | Unknown of (UInt8.t * string)

let tlv_typ : tlv Wire.typ =
  Wire.casetype "Tlv" Wire.uint8
    [
      Wire.case ~index:(UInt8.v 1) Wire.uint16be
        ~inject:(fun v -> Known v)
        ~project:(function Known v -> Some v | _ -> None);
      Wire.default
        (Wire.byte_array ~size:(int 2))
        ~inject:(fun tag body -> Unknown (tag, body))
        ~project:(function Unknown (t, b) -> Some (t, b) | _ -> None);
    ]

let tlv_codec =
  Codec.v "Tlv" (fun x -> x) Codec.[ (Field.v "v" tlv_typ $ fun x -> x) ]

let test_casetype_default_recovers_tag () =
  let v = Unknown (UInt8.v 0x42, "ab") in
  let buf = Bytes.create (Codec.size_of_value tlv_codec v) in
  Codec.encode tlv_codec v buf 0;
  Alcotest.(check int)
    "encode writes the captured tag" 0x42 (Bytes.get_uint8 buf 0);
  match Codec.decode tlv_codec buf 0 with
  | Ok (Unknown (t, b)) ->
      Alcotest.(check int) "decode recovers the tag" 0x42 (UInt8.to_int t);
      Alcotest.(check string) "decode body" "ab" b
  | Ok _ -> Alcotest.fail "expected Unknown"
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_casetype_field_roundtrip () =
  let buf = Bytes.create 11 in
  let original = { ts = 123L; data = `Login (UInt16.v 0xabcd) } in
  Codec.encode casetype_field_codec original buf 0;
  let decoded = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check int64) "ts roundtrip" original.ts decoded.ts;
  Alcotest.(check bool) "data roundtrip" true (original.data = decoded.data)

(* [Codec.size_of_value] counts a casetype field's tag plus its matched-case
   body, so a buffer sized from it holds the whole encoding. *)
let test_casetype_size_of_value () =
  let original = { ts = 123L; data = `Login (UInt16.v 0xabcd) } in
  (* ts (8) + tag (1) + Login body uint16be (2) = 11 *)
  let n = Codec.size_of_value casetype_field_codec original in
  Alcotest.(check int) "size_of_value" 11 n;
  let buf = Bytes.create n in
  Codec.encode casetype_field_codec original buf 0;
  let decoded = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check int64) "ts" original.ts decoded.ts;
  Alcotest.(check bool) "data" true (original.data = decoded.data)

(* Length-prefixed casetype dispatch: [tag][length][body] where length
   bounds the inner casetype's tag + body. *)

type lp_event = { tag : UInt8.t; len : UInt16.t; data : ev_payload }

let lp_event_len = Field.v "Length" uint16be

let lp_event_codec =
  Codec.v "LpEvent"
    (fun tag len data -> { tag; len; data })
    Codec.
      [
        (Field.v "Tag" uint8 $ fun e -> e.tag);
        (lp_event_len $ fun e -> e.len);
        ( Field.v "Data"
            (Wire.nested ~size:(Field.ref lp_event_len) casetype_field_event_typ)
        $ fun e -> e.data );
      ]

let test_length_prefixed_casetype () =
  (* tag=0xAA, len=3 (1 byte casetype tag + 2 bytes uint16be), inner tag=1, body=0x4242 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint16_be buf 1 3;
  Bytes.set_uint8 buf 3 1;
  Bytes.set_uint16_be buf 4 0x4242;
  let r = decode_ok (Codec.decode lp_event_codec buf 0) in
  Alcotest.(check int) "tag" 0xAA (UInt8.to_int r.tag);
  Alcotest.(check int) "len" 3 (UInt16.to_int r.len);
  Alcotest.(check bool) "data" true (r.data = `Login (UInt16.v 0x4242))

(* -- [repeat] of a [casetype] with mixed-shape cases (DHCP-options TLV) --

   Each option dispatches on a [u8] code. PAD (0) and END (255) are a bare tag
   with no body ([unit]); every other code is a length-prefixed body
   ([len:u8] + [data:byte_array(len)]). This exercises a casetype as a repeat
   element with structurally different per-case lengths -- zero-length cases
   beside variable-length ones. *)
type dhcp_opt = Pad | End | Generic of string
type opt_body = { data : string }

let ob_len = Field.v "len" uint8
let ob_data = Field.v "data" (byte_array ~size:(Field.ref ob_len))

let opt_body_codec =
  Codec.v "OptBody"
    (fun _l d -> { data = d })
    Codec.
      [
        (ob_len $ fun b -> UInt8.v (String.length b.data));
        (ob_data $ fun b -> b.data);
      ]

let dhcp_opt_typ : dhcp_opt typ =
  casetype "DhcpOpt" uint8
    [
      case ~index:(UInt8.v 0) empty
        ~inject:(fun () -> Pad)
        ~project:(function Pad -> Some () | _ -> None);
      case ~index:(UInt8.v 255) empty
        ~inject:(fun () -> End)
        ~project:(function End -> Some () | _ -> None);
      case ~index:(UInt8.v 53) (codec opt_body_codec)
        ~inject:(fun b -> Generic b.data)
        ~project:(function Generic d -> Some { data = d } | _ -> None);
    ]

let dhcp_opt_size = function Pad | End -> 1 | Generic d -> 2 + String.length d

(* Trailing options region sized by a leading byte budget. *)
let dhcp_f_total = Field.v "total" uint8

let dhcp_f_opts =
  Field.repeat "opts" ~size:(Field.ref dhcp_f_total) dhcp_opt_typ

let dhcp_codec =
  Codec.v "DhcpOpts"
    (fun _t xs -> xs)
    Codec.
      [
        ( dhcp_f_total $ fun xs ->
          UInt8.v (List.fold_left (fun a o -> a + dhcp_opt_size o) 0 xs) );
        (dhcp_f_opts $ fun xs -> xs);
      ]

let test_repeat_casetype_decode () =
  (* total=10: 35 03 'abc' | 00 | 35 01 'z' | ff *)
  let buf = Bytes.of_string "\x0a\x35\x03abc\x00\x35\x01z\xff" in
  let opts = decode_ok (Codec.decode dhcp_codec buf 0) in
  Alcotest.(check int) "count" 4 (List.length opts);
  match opts with
  | [ Generic "abc"; Pad; Generic "z"; End ] -> ()
  | _ -> Alcotest.fail "unexpected options"

let test_repeat_casetype_roundtrip () =
  let v = [ Generic "abc"; Pad; Generic "z"; End ] in
  let total = List.fold_left (fun a o -> a + dhcp_opt_size o) 0 v in
  let buf = Bytes.create (1 + total) in
  Codec.encode dhcp_codec v buf 0;
  let decoded = decode_ok (Codec.decode dhcp_codec buf 0) in
  Alcotest.(check bool) "roundtrip" true (decoded = v)

let test_repeat_casetype_empty () =
  let buf = Bytes.of_string "\x00" in
  let opts = decode_ok (Codec.decode dhcp_codec buf 0) in
  Alcotest.(check int) "empty" 0 (List.length opts)

(* A casetype case body that is a NUL-terminated string bounded to a fixed
   region ([zeroterm_at_most]), used as a repeat element. read_elem,
   build_field_encoder, and elem_size_of all lacked the zeroterm_at_most case,
   so encode raised "unsupported type" and sizing raised "cannot determine
   element size". *)
type zt_opt = Str of string | Num of UInt8.t

let zt_opt_typ : zt_opt typ =
  casetype "ZtOpt" uint8
    [
      case ~index:(UInt8.v 1)
        (zeroterm_at_most ~size:(int 6))
        ~inject:(fun s -> Str s)
        ~project:(function Str s -> Some s | _ -> None);
      case ~index:(UInt8.v 2) uint8
        ~inject:(fun n -> Num n)
        ~project:(function Num n -> Some n | _ -> None);
    ]

let zt_opt_size = function Str _ -> 1 + 6 | Num _ -> 1 + 1
let zt_f_total = Field.v "total" uint16be
let zt_f_opts = Field.repeat "opts" ~size:(Field.ref zt_f_total) zt_opt_typ

let zt_codec =
  Codec.v "ZtOpts"
    (fun _t xs -> xs)
    Codec.
      [
        ( zt_f_total $ fun xs ->
          UInt16.v (List.fold_left (fun a o -> a + zt_opt_size o) 0 xs) );
        (zt_f_opts $ fun xs -> xs);
      ]

let test_repeat_casetype_zeroterm_at_most () =
  let v = [ Str "hi"; Num (UInt8.v 7); Str "" ] in
  let buf = Bytes.create (Codec.size_of_value zt_codec v) in
  Codec.encode zt_codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (decode_ok (Codec.decode zt_codec buf 0) = v)

(* A casetype case body that is a lone bitfield, as a repeat element.
   read_elem / build_field_encoder / elem_size_of lacked the Bits case, so the
   casetype could not encode/decode as a repeat element. A bitfield case body
   projects to 3D inside a repeated casetype (the casetype packs it into its
   base word); a list of bare bitfields, by contrast, has no projection and is
   rejected. *)
type bf_opt = Bf of int | Raw of UInt8.t

let bf_opt_typ : bf_opt typ =
  casetype "BfOpt" uint8
    [
      case ~index:(UInt8.v 1) (bits ~width:3 U8)
        ~inject:(fun b -> Bf b)
        ~project:(function Bf b -> Some b | _ -> None);
      case ~index:(UInt8.v 2) uint8
        ~inject:(fun v -> Raw v)
        ~project:(function Raw v -> Some v | _ -> None);
    ]

let bf_f_total = Field.v "total" uint16be
let bf_f_opts = Field.repeat "opts" ~size:(Field.ref bf_f_total) bf_opt_typ

(* each case is a 1-byte body after the 1-byte tag *)
let bf_codec =
  Codec.v "BfOpts"
    (fun _t xs -> xs)
    Codec.
      [
        (bf_f_total $ fun xs -> UInt16.v (List.length xs * 2));
        (bf_f_opts $ fun xs -> xs);
      ]

let test_repeat_casetype_bits_case () =
  let v = [ Bf 5; Raw (UInt8.v 9); Bf 0 ] in
  let buf = Bytes.create (Codec.size_of_value bf_codec v) in
  Codec.encode bf_codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (decode_ok (Codec.decode bf_codec buf 0) = v)

(* A casetype is repeatable only when every case body decodes one element at a
   time. A nested region case body has no per-element 3D projection inside a
   repeat (EverParse cannot extract a single-element-array nested in a repeated
   casetype), so the casetype is rejected at construction rather than failing
   at decode. *)
let test_repeat_casetype_unprojectable_case_rejected () =
  let nested_case =
    casetype "NestCaseOpt" uint8
      [
        case ~index:(UInt8.v 1)
          (nested ~size:(int 1) int8)
          ~inject:(fun v -> `N v)
          ~project:(function `N v -> Some v);
      ]
  in
  Alcotest.(check bool)
    "repeat over casetype with nested case body rejected" true
    (raises_invalid (fun () -> Field.repeat "opts" ~size:(int 4) nested_case))

let suite =
  ( "codec_casetype",
    [
      Alcotest.test_case "casetype field: login" `Quick
        test_casetype_field_login;
      Alcotest.test_case "casetype field: logout" `Quick
        test_casetype_field_logout;
      Alcotest.test_case "casetype field: default" `Quick
        test_casetype_field_default;
      Alcotest.test_case "casetype field: no match names the field" `Quick
        test_casetype_no_match_names_field;
      Alcotest.test_case "casetype field: no match is Invalid_tag" `Quick
        test_casetype_no_match_invalid_tag;
      Alcotest.test_case "casetype default recovers matched tag" `Quick
        test_casetype_default_recovers_tag;
      Alcotest.test_case "casetype field: roundtrip" `Quick
        test_casetype_field_roundtrip;
      Alcotest.test_case "casetype field: size_of_value" `Quick
        test_casetype_size_of_value;
      Alcotest.test_case "casetype field: length-prefixed" `Quick
        test_length_prefixed_casetype;
      Alcotest.test_case "repeat: casetype TLV decode" `Quick
        test_repeat_casetype_decode;
      Alcotest.test_case "repeat: casetype TLV roundtrip" `Quick
        test_repeat_casetype_roundtrip;
      Alcotest.test_case "repeat: casetype TLV empty" `Quick
        test_repeat_casetype_empty;
      Alcotest.test_case "repeat: casetype with zeroterm_at_most case" `Quick
        test_repeat_casetype_zeroterm_at_most;
      Alcotest.test_case "repeat: casetype with bitfield case" `Quick
        test_repeat_casetype_bits_case;
      Alcotest.test_case "repeat: casetype with unprojectable case rejected"
        `Quick test_repeat_casetype_unprojectable_case_rejected;
    ] )
