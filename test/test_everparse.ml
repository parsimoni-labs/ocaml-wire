(* Tests for c.ml: 3D generation and projection-facing syntax. *)

open Wire
open Wire.Everparse.Raw

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

let test_bitfields () =
  let f_y = field "y" (bits ~width:10 U32) in
  let f_z = field "z" (bits ~width:16 U32) in
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 U32);
        field "y"
          ~constraint_:Expr.(field_ref f_y <= int 900)
          (bits ~width:10 U32);
        field "z"
          ~constraint_:Expr.(field_ref f_y + field_ref f_z <= int 60000)
          (bits ~width:16 U32);
      ]
  in
  let m = module_ [ typedef bf ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains BF" true (contains ~sub:"BF" output)

let test_enumerations () =
  let m =
    module_
      [
        enum_decl "Enum8"
          [ ("Enum8_1", 0); ("Enum8_2", 1); ("Enum8_3", 2) ]
          uint8;
      ]
  in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains enum" true (contains ~sub:"enum" output);
  Alcotest.(check bool) "contains Enum8_1" true (contains ~sub:"Enum8_1" output)

let test_field_dependence () =
  let t_struct = param_struct "t" [ param "a" uint32 ] [ field "x" uint32 ] in
  let f_a = field "a" uint32 in
  let s_struct =
    struct_ "s"
      [ field "a" uint32; field "b" (apply (type_ref "t") [ field_ref f_a ]) ]
  in
  let m = module_ [ typedef t_struct; typedef s_struct ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains param" true (contains ~sub:"UINT32 a" output)

let test_casetype () =
  let d_casetype =
    casetype_decl "_D"
      [ param "key" uint32 ]
      uint32
      [ decl_case 1 uint16; decl_case 2 uint32 ]
  in
  let m = module_ [ d_casetype ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool)
    "contains casetype" true
    (contains ~sub:"casetype" output);
  Alcotest.(check bool)
    "contains switch" true
    (contains ~sub:"switch (key)" output);
  Alcotest.(check bool) "public name is D" true (contains ~sub:"} D;" output)

let test_pretty_print () =
  let simple =
    struct_ "Simple" [ field "a" uint8; field "b" uint16be; field "c" uint32 ]
  in
  let m = module_ [ typedef simple ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains typedef" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output)

(* ── Codec definitions for 3D extraction tests ── *)

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
        ( Field.v "Payload" (optional (bool present) uint16be) $ fun r ->
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
        ( Field.v "Items"
            (repeat ~size:(Field.ref f_cnt_length) (codec inner_codec))
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

type tm_like = {
  hdr : int;
  data_len : int;
  packets : packet list;
  ocf : int option;
  fecf : int option;
}

let f_tm_data_len = Field.v "DataLen" uint8

let tm_like_codec ~ocf ~fecf =
  Codec.v "TmLike"
    (fun hdr data_len packets ocf fecf -> { hdr; data_len; packets; ocf; fecf })
    Codec.
      [
        (Field.v "Hdr" uint16be $ fun r -> r.hdr);
        (f_tm_data_len $ fun r -> r.data_len);
        ( Field.v "Packets"
            (repeat ~size:(Field.ref f_tm_data_len) (codec packet_codec))
        $ fun r -> r.packets );
        ( Field.v "OCF"
            (optional (if ocf then Expr.true_ else Expr.false_) uint32be)
        $ fun r -> r.ocf );
        (Field.v "FECF" (optional (bool fecf) uint16be) $ fun r -> r.fecf);
      ]

(* ── 3D extraction tests ── *)

let test_3d_codec_embed () =
  (* Codec embed: field type should reference the sub-codec's struct name *)
  let s_inner = Everparse.struct_of_codec inner_codec in
  let s_outer = Everparse.struct_of_codec outer_codec in
  let m = module_ [ typedef s_inner; typedef s_outer ] in
  let output = to_3d m in
  (* Inner struct must be defined *)
  Alcotest.(check bool)
    "inner struct defined" true
    (contains ~sub:"typedef struct _Inner" output);
  (* Outer struct references Inner by name *)
  Alcotest.(check bool)
    "outer references Inner" true
    (contains ~sub:"Inner Inner" output);
  Alcotest.(check bool)
    "contains Header field" true
    (contains ~sub:"Header" output);
  Alcotest.(check bool)
    "contains Trailer field" true
    (contains ~sub:"Trailer" output)

let test_3d_codec_nested () =
  (* Two-level nesting: L0 -> L1 -> L2, each should reference by name *)
  let s0 = Everparse.struct_of_codec l0_codec in
  let m = module_ [ typedef s0 ] in
  let output = to_3d m in
  (* L0's struct should reference L1 by name *)
  Alcotest.(check bool) "contains L1 ref" true (contains ~sub:"L1" output);
  Alcotest.(check bool) "contains Z field" true (contains ~sub:"Z" output)

let test_3d_optional_present () =
  (* Optional present: field should appear as normal UINT16BE *)
  let s = Everparse.struct_of_codec opt_codec_present in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Hdr" true (contains ~sub:"Hdr" output);
  Alcotest.(check bool) "contains UINT16" true (contains ~sub:"UINT16" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output);
  Alcotest.(check bool) "contains Trail" true (contains ~sub:"Trail" output);
  (* Must not contain invalid 3D syntax *)
  Alcotest.(check bool)
    "no optional() syntax" false
    (contains ~sub:"optional(" output)

let test_3d_optional_absent () =
  (* Optional absent: zero-length field *)
  let s = Everparse.struct_of_codec opt_codec_absent in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Hdr" true (contains ~sub:"Hdr" output);
  Alcotest.(check bool) "contains Trail" true (contains ~sub:"Trail" output);
  Alcotest.(check bool)
    "no optional() syntax" false
    (contains ~sub:"optional(" output)

let test_3d_repeat () =
  (* Repeat: should render as variable-length array with :byte-size *)
  let s_inner = Everparse.struct_of_codec inner_codec in
  let s = Everparse.struct_of_codec repeat_codec in
  let m = module_ [ typedef s_inner; typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Items" true (contains ~sub:"Items" output);
  (* Repeat should render as :byte-size array, referencing Inner type *)
  Alcotest.(check bool)
    "contains :byte-size" true
    (contains ~sub:":byte-size" output);
  Alcotest.(check bool)
    "references Inner elem type" true
    (contains ~sub:"Inner" output);
  (* Must not contain invalid 3D syntax *)
  Alcotest.(check bool)
    "no repeat() syntax" false
    (contains ~sub:"repeat(" output)

let test_3d_tm_like () =
  (* Full TM-like composition: all nested types should project to 3D *)
  let c = tm_like_codec ~ocf:true ~fecf:true in
  let s_pkt = Everparse.struct_of_codec packet_codec in
  let s = Everparse.struct_of_codec c in
  let m = module_ [ typedef s_pkt; typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Hdr" true (contains ~sub:"Hdr" output);
  Alcotest.(check bool) "contains DataLen" true (contains ~sub:"DataLen" output);
  Alcotest.(check bool) "contains Packets" true (contains ~sub:"Packets" output);
  Alcotest.(check bool) "contains OCF" true (contains ~sub:"OCF" output);
  Alcotest.(check bool) "contains FECF" true (contains ~sub:"FECF" output);
  (* Sub-codec should be referenced by name *)
  Alcotest.(check bool)
    "contains Packet typedef" true
    (contains ~sub:"typedef struct _Packet" output);
  (* Packet should be referenced in the TmLike struct *)
  Alcotest.(check bool)
    "Packet type referenced" true
    (contains ~sub:"Packet" output)

let suite =
  ( "everparse",
    [
      Alcotest.test_case "generation: bitfields" `Quick test_bitfields;
      Alcotest.test_case "generation: enumerations" `Quick test_enumerations;
      Alcotest.test_case "generation: field dependence" `Quick
        test_field_dependence;
      Alcotest.test_case "generation: casetype" `Quick test_casetype;
      Alcotest.test_case "generation: pretty print" `Quick test_pretty_print;
      Alcotest.test_case "3d: codec embed" `Quick test_3d_codec_embed;
      Alcotest.test_case "3d: codec nested" `Quick test_3d_codec_nested;
      Alcotest.test_case "3d: optional present" `Quick test_3d_optional_present;
      Alcotest.test_case "3d: optional absent" `Quick test_3d_optional_absent;
      Alcotest.test_case "3d: repeat" `Quick test_3d_repeat;
      Alcotest.test_case "3d: tm-like" `Quick test_3d_tm_like;
    ] )
