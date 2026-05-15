open Wire

type inner = { tag : int; value : int }

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

type outer = { header : int; inner : inner; trailer : int }

let outer_codec =
  Codec.v "Outer"
    (fun header inner trailer -> ({ header; inner; trailer } : outer))
    Codec.
      [
        (Field.v "Header" uint8 $ fun (r : outer) -> r.header);
        (Field.v "Inner" (codec inner_codec) $ fun (r : outer) -> r.inner);
        (Field.v "Trailer" uint8 $ fun (r : outer) -> r.trailer);
      ]

type l2 = { x : int }
type l1 = { inner : l2; y : int }
type l0 = { inner : l1; z : int }

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

type opt_record = { hdr : int; payload : int option; trail : int }

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

type container = { length : int; items : inner list }

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

type packet = { id : int; data : int }

let packet_codec =
  Codec.v "Packet"
    (fun id data -> ({ id; data } : packet))
    Codec.
      [
        (Field.v "Id" uint8 $ fun (r : packet) -> r.id);
        (Field.v "Data" uint16be $ fun (r : packet) -> r.data);
      ]

type multi_record = { x : int; y : int }

let multi_record_codec =
  Codec.v "MultiRecord"
    (fun x y -> ({ x; y } : multi_record))
    Codec.
      [
        (Field.v "x" uint16be $ fun (r : multi_record) -> r.x);
        (Field.v "y" uint16be $ fun (r : multi_record) -> r.y);
      ]
