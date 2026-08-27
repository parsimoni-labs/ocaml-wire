open Wire

type inner = { tag : UInt8.t; value : UInt16.t }

let f_inner_tag = Field.v "Tag" uint8
let f_inner_value = Field.v "Value" uint16be

let inner_codec =
  Codec.v "Inner"
    (fun tag value -> ({ tag; value } : inner))
    Codec.
      [
        (f_inner_tag $ fun (r : inner) -> r.tag);
        (f_inner_value $ fun (r : inner) -> r.value);
      ]

type outer = { header : UInt8.t; inner : inner; trailer : UInt8.t }

let outer_codec =
  Codec.v "Outer"
    (fun header inner trailer -> ({ header; inner; trailer } : outer))
    Codec.
      [
        (Field.v "Header" uint8 $ fun (r : outer) -> r.header);
        (Field.v "Inner" (codec inner_codec) $ fun (r : outer) -> r.inner);
        (Field.v "Trailer" uint8 $ fun (r : outer) -> r.trailer);
      ]

type l2 = { x : UInt8.t }
type l1 = { inner : l2; y : UInt16.t }
type l0 = { inner : l1; z : UInt8.t }

let l2_codec =
  Codec.v "L2"
    (fun x -> ({ x } : l2))
    Codec.[ (Field.v "X" uint8 $ fun (r : l2) -> r.x) ]

let l1_codec =
  Codec.v "L1"
    (fun inner y -> ({ inner; y } : l1))
    Codec.
      [
        (Field.v "Inner" (codec l2_codec) $ fun (r : l1) -> r.inner);
        (Field.v "Y" uint16be $ fun (r : l1) -> r.y);
      ]

let l0_codec =
  Codec.v "L0"
    (fun inner z -> ({ inner; z } : l0))
    Codec.
      [
        (Field.v "Inner" (codec l1_codec) $ fun (r : l0) -> r.inner);
        (Field.v "Z" uint8 $ fun (r : l0) -> r.z);
      ]

type opt_record = { hdr : UInt8.t; payload : UInt16.t option; trail : UInt8.t }

let opt_codec ~present =
  Codec.v "OptRecord"
    (fun hdr payload trail -> ({ hdr; payload; trail } : opt_record))
    Codec.
      [
        (Field.v "Hdr" uint8 $ fun (r : opt_record) -> r.hdr);
        ( Field.optional "Payload" ~present:(bool present) uint16be
        $ fun (r : opt_record) -> r.payload );
        (Field.v "Trail" uint8 $ fun (r : opt_record) -> r.trail);
      ]

let opt_codec_present = opt_codec ~present:true
let opt_codec_absent = opt_codec ~present:false

type container = { length : UInt8.t; items : inner list }

let f_cnt_length = Field.v "Length" uint8

let repeat_codec =
  Codec.v "Container"
    (fun length items -> ({ length; items } : container))
    Codec.
      [
        (f_cnt_length $ fun (r : container) -> r.length);
        ( Field.repeat "Items" ~size:(Field.ref f_cnt_length) (codec inner_codec)
        $ fun (r : container) -> r.items );
      ]

type packet = { id : UInt8.t; data : UInt16.t }

let packet_codec =
  Codec.v "Packet"
    (fun id data -> ({ id; data } : packet))
    Codec.
      [
        (Field.v "Id" uint8 $ fun (r : packet) -> r.id);
        (Field.v "Data" uint16be $ fun (r : packet) -> r.data);
      ]

type multi_record = { x : UInt16.t; y : UInt16.t }

let multi_record_codec =
  Codec.v "MultiRecord"
    (fun x y -> ({ x; y } : multi_record))
    Codec.
      [
        (Field.v "x" uint16be $ fun (r : multi_record) -> r.x);
        (Field.v "y" uint16be $ fun (r : multi_record) -> r.y);
      ]

(* -- Decode assertions -- *)

let decode_ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected parse error: %a" pp_parse_error e

let expect_constraint_fail = function
  | Ok _ -> Alcotest.fail "expected a Constraint_failed parse error"
  | Error { kind = Constraint_failed _; _ } -> ()
  | Error e ->
      Alcotest.failf "expected Constraint_failed, got %a" pp_parse_error e

(* Assert a decode failed with a [kind] satisfying [pred]. *)
let expect_kind pred = function
  | Ok _ -> Alcotest.fail "expected a parse error"
  | Error { kind; _ } when pred kind -> ()
  | Error e -> Alcotest.failf "unexpected parse error: %a" pp_parse_error e

let roundtrip name typ testable v =
  Alcotest.check testable name v (decode_ok (of_string typ (to_string typ v)))
