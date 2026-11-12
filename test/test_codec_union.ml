(* Tests for codec.ml: unions dispatched on an earlier field. *)

open Wire
open Test_helpers

(* The shape of the request: a tag, a field that is not the union, then the
   union, then a field after it to pin the offsets. *)

type payload =
  [ `Ping of UInt16.t | `Data of UInt32.t | `Other of UInt8.t * UInt8.t ]

type packet = {
  kind : UInt8.t;
  checksum : UInt32.t;
  body : payload;
  tail : UInt8.t;
}

let f_kind = Field.v "Kind" uint8

let payload_cases =
  [
    case ~index:(UInt8.v 1) uint16be
      ~inject:(fun v -> `Ping v)
      ~project:(function `Ping v -> Some v | _ -> None);
    case ~index:(UInt8.v 2) uint32be
      ~inject:(fun v -> `Data v)
      ~project:(function `Data v -> Some v | _ -> None);
    default uint8
      ~inject:(fun tag v -> `Other (tag, v))
      ~project:(function `Other (tag, v) -> Some (tag, v) | _ -> None);
  ]

let packet_codec =
  Codec.v "UnionPacket"
    (fun kind checksum body tail -> { kind; checksum; body; tail })
    Codec.
      [
        (f_kind $ fun p -> p.kind);
        (Field.v "Checksum" uint32be $ fun p -> p.checksum);
        ( Field.union "Body" ~on:f_kind ~casetype:"UnionBody" payload_cases
        $ fun p -> p.body );
        (Field.v "Tail" uint8 $ fun p -> p.tail);
      ]

let packet kind body =
  {
    kind = UInt8.v kind;
    checksum = UInt32.of_int32 0xdeadbeefl;
    body;
    tail = UInt8.v 0x7e;
  }

let encoded codec v =
  let buf = Bytes.create (Codec.size_of_value codec v) in
  Codec.encode codec v buf 0;
  Bytes.to_string buf

let wire_of = encoded packet_codec

let test_decode () =
  let r =
    decode_ok
      (Codec.decode packet_codec
         (Bytes.of_string "\x02\xde\xad\xbe\xef\x00\x00\x01\x00\x7e")
         0)
  in
  Alcotest.(check bool) "data" true (r.body = `Data (UInt32.of_int 256));
  Alcotest.(check int) "tail" 0x7e (UInt8.to_int r.tail)

let test_roundtrip () =
  List.iter
    (fun (p, expected) ->
      let s = wire_of p in
      Alcotest.(check string) "bytes" expected s;
      let r = decode_ok (Codec.decode packet_codec (Bytes.of_string s) 0) in
      Alcotest.(check bool) "value" true (r = p);
      Codec.validate packet_codec (Bytes.of_string s) 0)
    [
      (packet 1 (`Ping (UInt16.v 0x1234)), "\x01\xde\xad\xbe\xef\x12\x34\x7e");
      ( packet 2 (`Data (UInt32.of_int 7)),
        "\x02\xde\xad\xbe\xef\x00\x00\x00\x07\x7e" );
      (packet 9 (`Other (UInt8.v 9, UInt8.v 5)), "\x09\xde\xad\xbe\xef\x05\x7e");
    ]

(* The value's case and the tag field are two fields of one record: writing a
   record where they disagree would not decode back to itself. *)
let test_encode_mismatch () =
  let p = packet 2 (`Ping (UInt16.v 1)) in
  Alcotest.(check bool)
    "refused" true
    (raises_invalid (fun () -> Codec.encode packet_codec p (Bytes.create 16) 0))

(* The default branch carries its tag, but not one a case claims: those bytes
   would decode as the case. *)
let test_encode_default_claimed () =
  let p = packet 1 (`Other (UInt8.v 1, UInt8.v 5)) in
  Alcotest.(check bool)
    "refused" true
    (raises_invalid (fun () -> Codec.encode packet_codec p (Bytes.create 16) 0))

let closed_codec =
  let f_tag = Field.v "Tag" uint8 in
  Codec.v "UnionClosed"
    (fun tag pad v -> (tag, pad, v))
    Codec.
      [
        (f_tag $ fun (t, _, _) -> t);
        (Field.v "Pad" uint8 $ fun (_, p, _) -> p);
        ( Field.union "V" ~on:f_tag ~casetype:"UnionClosedV"
            [
              case ~index:(UInt8.v 1) uint8 ~inject:Fun.id ~project:Option.some;
            ]
        $ fun (_, _, v) -> v );
      ]

let test_unknown_tag () =
  let buf = Bytes.of_string "\x63\x00\x00" in
  (match Codec.decode closed_codec buf 0 with
  | Error { kind = Invalid_tag n; field; at } ->
      Alcotest.(check int) "tag" 99 n;
      Alcotest.(check int) "at the tag field" 0 at;
      Alcotest.(check (list string)) "field" [ "V" ] field
  | Error e -> Alcotest.failf "expected Invalid_tag, got %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "decoded an unclaimed tag");
  match Codec.validate closed_codec buf 0 with
  | exception Parse_error { kind = Invalid_tag 99; _ } -> ()
  | () -> Alcotest.fail "validated an unclaimed tag"

let test_truncated () =
  expect_kind
    (function Unexpected_eof _ -> true | _ -> false)
    (Codec.decode packet_codec (Bytes.of_string "\x02\xde\xad\xbe\xef\x00") 0)

(* Tags too large or too sparse for the lookup table take the comparing path;
   two unions may read the one tag. *)
let wide_codec =
  let f_ether = Field.v "EtherType" uint16be in
  let cases a b =
    [
      case ~index:(UInt16.v 0x0800) a ~inject:Either.left
        ~project:Either.find_left;
      case ~index:(UInt16.v 0x86dd) b ~inject:Either.right
        ~project:Either.find_right;
    ]
  in
  Codec.v "UnionWide"
    (fun ether first second -> (ether, first, second))
    Codec.
      [
        (f_ether $ fun (e, _, _) -> e);
        ( Field.union "First" ~on:f_ether ~casetype:"UnionWideFirst"
            (cases uint8 uint16be)
        $ fun (_, f, _) -> f );
        ( Field.union "Second" ~on:f_ether ~casetype:"UnionWideSecond"
            (cases uint16be uint8)
        $ fun (_, _, s) -> s );
      ]

let test_wide_tags () =
  let v4 =
    (UInt16.v 0x0800, Either.Left (UInt8.v 4), Either.Left (UInt16.v 6))
  in
  let v6 =
    (UInt16.v 0x86dd, Either.Right (UInt16.v 4), Either.Right (UInt8.v 6))
  in
  List.iter
    (fun (v, expected) ->
      let buf = Bytes.create (Codec.size_of_value wide_codec v) in
      Codec.encode wide_codec v buf 0;
      Alcotest.(check string) "bytes" expected (Bytes.to_string buf);
      let back = decode_ok (Codec.decode wide_codec buf 0) in
      Alcotest.(check string) "decoded" expected (encoded wide_codec back))
    [ (v4, "\x08\x00\x04\x00\x06"); (v6, "\x86\xdd\x00\x04\x06") ]

(* A bitfield tag shares its byte with another field. *)
let test_bitfield_tag () =
  let f_kind = Field.v "Kind" (bits ~width:4 U8) in
  let c =
    Codec.v "UnionBits"
      (fun kind flags v -> (kind, flags, v))
      Codec.
        [
          (f_kind $ fun (k, _, _) -> k);
          (Field.v "Flags" (bits ~width:4 U8) $ fun (_, f, _) -> f);
          ( Field.union "V" ~on:f_kind ~casetype:"UnionBitsV"
              [
                case ~index:3 uint16be ~inject:Either.left
                  ~project:Either.find_left;
                case ~index:5 uint8 ~inject:Either.right
                  ~project:Either.find_right;
              ]
          $ fun (_, _, v) -> v );
        ]
  in
  let v = (5, 0xa, Either.Right (UInt8.v 0x42)) in
  let buf = Bytes.create (Codec.size_of_value c v) in
  Codec.encode c v buf 0;
  Alcotest.(check int) "size" 2 (Bytes.length buf);
  let back = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check string) "decoded" "\x5a\x42" (encoded c back)

let test_non_integer_tag_rejected () =
  let f_name = Field.v "Name" (byte_array ~size:(int 2)) in
  Alcotest.(check bool)
    "refused" true
    (raises_invalid (fun () ->
         Field.union "V" ~on:f_name ~casetype:"UnionStr"
           [ case ~index:"ab" uint8 ~inject:Fun.id ~project:Option.some ]))

let test_projection () =
  let out = render_3d packet_codec in
  List.iter
    (fun sub ->
      if not (contains ~sub out) then
        Alcotest.failf "missing %S in:@.%s" sub out)
    [ "casetype _UnionBody"; "UINT8 tag"; "UnionBody(Kind) Body" ]

(* Dispatch is a table lookup and a call: decoding the union allocates only
   what its value does, validating it nothing. *)
let test_validate_no_alloc () =
  skip_unless_gc_counters ();
  let buf = Bytes.of_string "\x02\xde\xad\xbe\xef\x00\x00\x01\x00\x7e" in
  Codec.validate packet_codec buf 0;
  let before = Gc.minor_words () in
  for _ = 1 to 1000 do
    Codec.validate packet_codec buf 0
  done;
  let after = Gc.minor_words () in
  Alcotest.(check (float 0.)) "words" 0. (after -. before)

let suite =
  ( "codec_union",
    [
      Alcotest.test_case "decode" `Quick test_decode;
      Alcotest.test_case "roundtrip" `Quick test_roundtrip;
      Alcotest.test_case "encode: value and tag disagree" `Quick
        test_encode_mismatch;
      Alcotest.test_case "encode: default with a claimed tag" `Quick
        test_encode_default_claimed;
      Alcotest.test_case "unknown tag is Invalid_tag" `Quick test_unknown_tag;
      Alcotest.test_case "truncated body" `Quick test_truncated;
      Alcotest.test_case "wide tags, two unions on one tag" `Quick
        test_wide_tags;
      Alcotest.test_case "bitfield tag" `Quick test_bitfield_tag;
      Alcotest.test_case "non-integer tag rejected" `Quick
        test_non_integer_tag_rejected;
      Alcotest.test_case "3d projection" `Quick test_projection;
      Alcotest.test_case "validate does not allocate" `Quick
        test_validate_no_alloc;
    ] )
