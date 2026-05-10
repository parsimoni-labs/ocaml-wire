open Wire

type inner = { tag : int; value : int }

let f_inner_tag = Field.v "Tag" uint8
let f_inner_value = Field.v "Value" uint16be

let inner_codec =
  Codec.v "Inner"
    (fun tag value -> { tag; value })
    Codec.[ (f_inner_tag $ fun r -> r.tag); (f_inner_value $ fun r -> r.value) ]

type outer = { header : int; inner : inner; trailer : int }

let outer_codec =
  Codec.v "Outer"
    (fun header inner trailer -> { header; inner; trailer })
    Codec.
      [
        (Field.v "Header" uint8 $ fun r -> r.header);
        (Field.v "Inner" (codec inner_codec) $ fun r -> r.inner);
        (Field.v "Trailer" uint8 $ fun r -> r.trailer);
      ]

type l2 = { l2_x : int }
type l1 = { l1_inner : l2; l1_y : int }
type l0 = { l0_inner : l1; l0_z : int }

let l2_codec =
  Codec.v "L2"
    (fun x -> { l2_x = x })
    Codec.[ (Field.v "X" uint8 $ fun r -> r.l2_x) ]

let l1_codec =
  Codec.v "L1"
    (fun inner y -> { l1_inner = inner; l1_y = y })
    Codec.
      [
        (Field.v "Inner" (codec l2_codec) $ fun r -> r.l1_inner);
        (Field.v "Y" uint16be $ fun r -> r.l1_y);
      ]

let l0_codec =
  Codec.v "L0"
    (fun inner z -> { l0_inner = inner; l0_z = z })
    Codec.
      [
        (Field.v "Inner" (codec l1_codec) $ fun r -> r.l0_inner);
        (Field.v "Z" uint8 $ fun r -> r.l0_z);
      ]

type opt_record = { opt_hdr : int; opt_payload : int option; opt_trail : int }

let opt_codec ~present =
  Codec.v "OptRecord"
    (fun hdr payload trail ->
      { opt_hdr = hdr; opt_payload = payload; opt_trail = trail })
    Codec.
      [
        (Field.v "Hdr" uint8 $ fun r -> r.opt_hdr);
        ( Field.optional "Payload" ~present:(bool present) uint16be $ fun r ->
          r.opt_payload );
        (Field.v "Trail" uint8 $ fun r -> r.opt_trail);
      ]

let opt_codec_present = opt_codec ~present:true
let opt_codec_absent = opt_codec ~present:false

type container = { cnt_length : int; cnt_items : inner list }

let f_cnt_length = Field.v "Length" uint8

let repeat_codec =
  Codec.v "Container"
    (fun length items -> { cnt_length = length; cnt_items = items })
    Codec.
      [
        (f_cnt_length $ fun r -> r.cnt_length);
        ( Field.repeat "Items" ~size:(Field.ref f_cnt_length) (codec inner_codec)
        $ fun r -> r.cnt_items );
      ]

type packet = { pkt_id : int; pkt_data : int }

let packet_codec =
  Codec.v "Packet"
    (fun id data -> { pkt_id = id; pkt_data = data })
    Codec.
      [
        (Field.v "Id" uint8 $ fun r -> r.pkt_id);
        (Field.v "Data" uint16be $ fun r -> r.pkt_data);
      ]

type multi_record = { x : int; y : int }

let multi_record_codec =
  Codec.v "MultiRecord"
    (fun x y -> { x; y })
    Codec.
      [
        (Field.v "x" uint16be $ fun r -> r.x);
        (Field.v "y" uint16be $ fun r -> r.y);
      ]
