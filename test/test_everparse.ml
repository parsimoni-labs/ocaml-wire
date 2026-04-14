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

(* ── Bit-order projection tests ──

   Non-native (base, bit_order) combinations should still emit a valid 3D
   struct by reversing declaration order within the bit group and, if the
   widths don't fill the word, prepending anonymous padding. These tests
   lock in that the reorder actually happens in the emitted 3D text. *)

(* Return the byte offset of the first occurrence of [sub] in [s],
   or -1 if not found. Used to assert relative field ordering. *)
let index_of ~sub s =
  let re = Re.compile (Re.str sub) in
  match Re.exec_opt re s with Some g -> Re.Group.start g 0 | None -> -1

let test_3d_bitorder_u8msb_reorder () =
  (* Non-native: (U8, Msb_first). EverParse native for UINT8 is LSB-first,
     so the projection reverses [a; b] to [b; a] in the emitted 3D text.
     Widths sum to 8 -> no padding. *)
  let codec =
    let open Codec in
    v "ReorderU8"
      (fun a b -> (a, b))
      [
        Field.v "a" (bits ~width:3 U8) $ fst;
        Field.v "b" (bits ~width:5 U8) $ snd;
      ]
  in
  let schema = Everparse.schema codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  let ia = index_of ~sub:"UINT8 a" s in
  let ib = index_of ~sub:"UINT8 b" s in
  Alcotest.(check bool) "a found" true (ia >= 0);
  Alcotest.(check bool) "b found" true (ib >= 0);
  Alcotest.(check bool) "reordered: b before a" true (ib < ia)

let test_3d_bitorder_u8msb_padding () =
  (* Non-native and incomplete: two 3-bit fields in (U8, Msb_first).
     Reversal alone would place the 6 bits at LSB-first positions 0..5;
     the projection prepends 2 bits of anonymous padding so the user's
     fields land at bits 2..7 (matching their Msb_first intent). *)
  let codec =
    let open Codec in
    v "ReorderU8Pad"
      (fun a b -> (a, b))
      [
        Field.v "a" (bits ~width:3 U8) $ fst;
        Field.v "b" (bits ~width:3 U8) $ snd;
      ]
  in
  let schema = Everparse.schema codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "has anonymous padding field" true
    (contains ~sub:"UINT8 _anon_" s);
  Alcotest.(check bool) "padding width = 2" true (contains ~sub:": 2" s)

let test_3d_bitorder_constraint_collapse () =
  (* Non-native: (U8, Msb_first) with a backward-reference constraint.
     Original order [a {a<=5}; b {a+b<=10}]. After reversal to [b; a],
     a constraint [a+b<=10] attached to b would fire before [a] was
     parsed. The projection therefore collapses all group constraints
     onto the last reversed field (here [a]), where every referent is
     available. *)
  let f_a_placeholder = Field.v "a" (bits ~width:4 U8) in
  let f_b_placeholder = Field.v "b" (bits ~width:4 U8) in
  let f_a =
    Field.v "a"
      ~constraint_:Expr.(Field.ref f_a_placeholder <= int 5)
      (bits ~width:4 U8)
  in
  let f_b =
    Field.v "b"
      ~constraint_:
        Expr.(Field.ref f_a_placeholder + Field.ref f_b_placeholder <= int 10)
      (bits ~width:4 U8)
  in
  let codec =
    let open Codec in
    v "Ranged" (fun a b -> (a, b)) [ f_a $ fst; f_b $ snd ]
  in
  let schema = Wire.Everparse.schema codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  (* b appears first in 3D (reversed), without a constraint block. *)
  let ia = index_of ~sub:"UINT8 a : 4" s in
  let ib = index_of ~sub:"UINT8 b : 4" s in
  Alcotest.(check bool) "b before a" true (ib < ia);
  (* The combined constraint lands on [a] (last in reversed order),
     including both the original [a <= 5] and [a + b <= 10]. *)
  Alcotest.(check bool)
    "a still referenced in combined constraint" true (contains ~sub:"a <= 5" s);
  Alcotest.(check bool)
    "b still referenced in combined constraint" true
    (contains ~sub:"(a + b) <= 10" s)

let test_3d_bitorder_native_noreorder () =
  (* Native: (U32be, Msb_first). Fields stay in declared order, no padding. *)
  let codec =
    let open Codec in
    v "NativeU32Be"
      (fun a b -> (a, b))
      [
        Field.v "x" (bits ~width:4 U32be) $ fst;
        Field.v "y" (bits ~width:28 U32be) $ snd;
      ]
  in
  let schema = Everparse.schema codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  let ix = index_of ~sub:"UINT32BE x" s in
  let iy = index_of ~sub:"UINT32BE y" s in
  Alcotest.(check bool) "x found" true (ix >= 0);
  Alcotest.(check bool) "y found" true (iy >= 0);
  Alcotest.(check bool) "x before y (no reorder)" true (ix < iy);
  Alcotest.(check bool) "no padding" false (contains ~sub:"_anon_" s)

(* ── Variable-size schema projection ── *)

type dep_frame = { frame_length : int; data : string }

let f_frame_length = Field.v "FrameLength" uint16be
let header_size = 2

let dep_frame_codec =
  Codec.v "DepFrame"
    (fun frame_length data -> { frame_length; data })
    Codec.
      [
        (f_frame_length $ fun r -> r.frame_length);
        ( Field.v "Data"
            (byte_array ~size:Expr.(Field.ref f_frame_length - int header_size))
        $ fun r -> r.data );
      ]

let test_3d_dep_size_schema () =
  let schema = Everparse.schema dep_frame_codec in
  Alcotest.(check bool) "variable wire_size" true (schema.wire_size = None);
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "contains FrameLength" true
    (contains ~sub:"FrameLength" s);
  Alcotest.(check bool) "contains Data" true (contains ~sub:"Data" s);
  Alcotest.(check bool)
    "contains byte-size expr" true
    (contains ~sub:":byte-size (FrameLength - 2)" s)

let test_3d_dep_size_roundtrip () =
  let original = { frame_length = 7; data = "HELLO" } in
  let buf = Bytes.create 7 in
  Codec.encode dep_frame_codec original buf 0;
  let decoded = Codec.decode dep_frame_codec buf 0 in
  match decoded with
  | Ok v ->
      Alcotest.(check int) "frame_length" 7 v.frame_length;
      Alcotest.(check string) "data" "HELLO" v.data
  | Error e -> Alcotest.failf "%a" pp_parse_error e

type param_frame = { pf_data : string }

let p_len = Param.input "len" uint16be

let param_frame_codec =
  Codec.v "ParamFrame"
    (fun data -> { pf_data = data })
    Codec.
      [
        ( Field.v "Data" (byte_array ~size:(Param.expr p_len)) $ fun r ->
          r.pf_data );
      ]

let test_3d_param_in_size () =
  (* A byte_array whose size is driven by a formal parameter must thread
     the parameter into the 3D typedef signature. *)
  let schema = Everparse.schema param_frame_codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "typedef carries len param" true
    (contains ~sub:"UINT16BE len" s);
  Alcotest.(check bool) "size uses len" true (contains ~sub:":byte-size len" s)

(* ── Reserved word escaping ── *)

let test_reserved_word_escaping () =
  let f_type = field "type" uint8 in
  let f_case = field "case" uint16be in
  let s =
    struct_ "Reserved"
      [ field "type" uint8; field "case" uint16be; field "value" uint32 ]
  in
  let m =
    module_
      [
        typedef ~entrypoint:true
          (param_struct "Reserved"
             [ param "return" uint8 ]
             [
               field "type" uint8;
               field "case"
                 ~constraint_:
                   Expr.(field_ref f_type + field_ref f_case <= int 10)
                 uint16be;
             ]);
      ]
  in
  ignore s;
  let output = to_3d m in
  Alcotest.(check bool) "type escaped" true (contains ~sub:"UINT8 type_" output);
  Alcotest.(check bool)
    "case escaped" true
    (contains ~sub:"UINT16BE case_" output);
  Alcotest.(check bool)
    "return escaped in param" true
    (contains ~sub:"UINT8 return_" output);
  Alcotest.(check bool)
    "type_ in constraint" true
    (contains ~sub:"type_" output);
  Alcotest.(check bool)
    "no bare reserved word as field" false
    (contains ~sub:"UINT8 type;" output)

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
      Alcotest.test_case "3d: bit_order U8 Msb_first reorder" `Quick
        test_3d_bitorder_u8msb_reorder;
      Alcotest.test_case "3d: bit_order U8 Msb_first padding" `Quick
        test_3d_bitorder_u8msb_padding;
      Alcotest.test_case "3d: bit_order native no reorder" `Quick
        test_3d_bitorder_native_noreorder;
      Alcotest.test_case "3d: bit_order constraint collapse" `Quick
        test_3d_bitorder_constraint_collapse;
      Alcotest.test_case "3d: dep-size schema" `Quick test_3d_dep_size_schema;
      Alcotest.test_case "3d: dep-size roundtrip" `Quick
        test_3d_dep_size_roundtrip;
      Alcotest.test_case "3d: param in size" `Quick test_3d_param_in_size;
      Alcotest.test_case "3d: reserved word escaping" `Quick
        test_reserved_word_escaping;
    ] )
