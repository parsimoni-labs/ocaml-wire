(* Tests of the 3D projection: the structs of embedded, optional and repeated
   codecs, variable sizes, parameters and lookups. *)

open Wire
open Wire.Private.Everparse.Raw
open Test_helpers

(* -- Codec definitions for 3D extraction tests --
   The shared codecs ([inner], [outer], [l0]/[l1]/[l2], [opt_record],
   [container]/[repeat_codec], [packet]/[packet_codec]) live in
   {!Test_helpers}. *)

type tm_like = {
  hdr : UInt16.t;
  data_len : UInt8.t;
  packets : packet list;
  ocf : UInt32.t option;
  fecf : UInt16.t option;
}

let f_tm_data_len = Field.v "DataLen" uint8

let tm_like_codec ~ocf ~fecf =
  Codec.v "TmLike"
    (fun hdr data_len packets ocf fecf -> { hdr; data_len; packets; ocf; fecf })
    Codec.
      [
        (Field.v "Hdr" uint16be $ fun r -> r.hdr);
        (f_tm_data_len $ fun r -> r.data_len);
        ( Field.repeat "Packets" ~size:(Field.ref f_tm_data_len)
            (codec packet_codec)
        $ fun r -> r.packets );
        ( Field.optional "OCF"
            ~present:(if ocf then Expr.true_ else Expr.false_)
            uint32be
        $ fun r -> r.ocf );
        (Field.optional "FECF" ~present:(bool fecf) uint16be $ fun r -> r.fecf);
      ]

(* -- 3D extraction tests -- *)

let test_3d_codec_embed () =
  (* Codec embed: field type should reference the sub-codec's struct name *)
  let s_inner = Private.Everparse.Raw.struct_of_codec inner_codec in
  let s_outer = Private.Everparse.Raw.struct_of_codec outer_codec in
  let m = module_ [ typedef s_inner; typedef s_outer ] in
  let output = to_3d m in
  (* Inner struct must be defined *)
  Alcotest.(check bool)
    "inner struct defined" true
    (contains ~sub:"typedef struct WireInner" output);
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
  let s0 = Private.Everparse.Raw.struct_of_codec l0_codec in
  let m = module_ [ typedef s0 ] in
  let output = to_3d m in
  (* L0's struct should reference L1 by name *)
  Alcotest.(check bool) "contains L1 ref" true (contains ~sub:"L1" output);
  Alcotest.(check bool) "contains Z field" true (contains ~sub:"Z" output)

let test_3d_optional_present () =
  (* Optional present: field should appear as normal UINT16BE *)
  let s = Private.Everparse.Raw.struct_of_codec opt_codec_present in
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
  let s = Private.Everparse.Raw.struct_of_codec opt_codec_absent in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Hdr" true (contains ~sub:"Hdr" output);
  Alcotest.(check bool) "contains Trail" true (contains ~sub:"Trail" output);
  Alcotest.(check bool)
    "no optional() syntax" false
    (contains ~sub:"optional(" output)

let test_3d_repeat () =
  (* Repeat: should render as variable-length array with :byte-size *)
  let s_inner = Private.Everparse.Raw.struct_of_codec inner_codec in
  let s = Private.Everparse.Raw.struct_of_codec repeat_codec in
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
  let s_pkt = Private.Everparse.Raw.struct_of_codec packet_codec in
  let s = Private.Everparse.Raw.struct_of_codec c in
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
    (contains ~sub:"typedef struct WirePacket" output);
  (* Packet should be referenced in the TmLike struct *)
  Alcotest.(check bool)
    "Packet type referenced" true
    (contains ~sub:"Packet" output)

(* -- Variable-size schema projection -- *)

type dep_frame = { frame_length : UInt16.t; data : string }

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
  let schema = Everparse.project ~mode:`Ffi dep_frame_codec in
  Alcotest.(check bool)
    "variable wire_size" true
    (Wire.Private.Everparse.wire_size schema = None);
  let s =
    Wire.Private.Everparse.Raw.to_3d (Wire.Private.Everparse.module_ schema)
  in
  Alcotest.(check bool)
    "contains FrameLength" true
    (contains ~sub:"FrameLength" s);
  Alcotest.(check bool) "contains Data" true (contains ~sub:"Data" s);
  Alcotest.(check bool)
    "contains byte-size expr" true
    (contains ~sub:":byte-size (FrameLength - 2)" s)

let test_3d_dep_size_roundtrip () =
  let original = { frame_length = UInt16.v 7; data = "HELLO" } in
  let buf = Bytes.create 7 in
  Codec.encode dep_frame_codec original buf ~off:0;
  let decoded = Codec.decode dep_frame_codec buf ~off:0 in
  match decoded with
  | Ok v ->
      Alcotest.(check int) "frame_length" 7 (UInt16.to_int v.frame_length);
      Alcotest.(check string) "data" "HELLO" v.data
  | Error e -> Alcotest.failf "%a" Error.pp e

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
  let schema = Everparse.project ~mode:`Ffi param_frame_codec in
  let s =
    Wire.Private.Everparse.Raw.to_3d (Wire.Private.Everparse.module_ schema)
  in
  Alcotest.(check bool)
    "typedef carries len param" true
    (contains ~sub:"UINT16BE len" s);
  Alcotest.(check bool) "size uses len" true (contains ~sub:":byte-size len" s)

(* A codec embedding a parametric sub-codec must surface the sub's param as its
   own formal and apply it at the use site, else 3D rejects the reference. *)
let test_3d_param_embed () =
  let lim = Param.input "lim" uint8 in
  let sub =
    Codec.v "PSub"
      ~where:Expr.(Field.ref (Field.v "v" uint8) <= Param.expr lim)
      (fun v -> v)
      Codec.[ Field.v "v" uint8 $ Fun.id ]
  in
  let outer =
    Codec.v "PEmbed"
      (fun s -> s)
      Codec.[ Field.v "s" (Wire.codec sub) $ Fun.id ]
  in
  let s =
    Wire.Private.Everparse.Raw.to_3d
      (Wire.Private.Everparse.module_ (Everparse.project ~mode:`Ffi outer))
  in
  Alcotest.(check bool)
    "sub typedef carries the formal" true
    (contains ~sub:"WirePSub(UINT8 lim)" s);
  Alcotest.(check bool)
    "use site applies the formal" true
    (contains ~sub:"PSub(lim) s" s)

(* A [nested] region over a composite inner (an array) projects through a
   synthesised wrapper struct so the single-element-array element is a named
   type, not a malformed inline array. *)
let test_3d_nested_over_array () =
  let codec =
    Codec.v "NAEmbed"
      (fun xs -> xs)
      Codec.
        [
          Field.v "xs" (nested ~size:(int 16) (array ~len:(int 2) uint64be))
          $ Fun.id;
        ]
  in
  let s =
    Wire.Private.Everparse.Raw.to_3d
      (Wire.Private.Everparse.module_ (Everparse.project ~mode:`Ffi codec))
  in
  Alcotest.(check bool)
    "wrapper struct holds the array" true
    (contains ~sub:"UINT64BE v[:byte-size" s);
  Alcotest.(check bool)
    "use site is a single-element-array of the named wrapper" true
    (contains ~sub:"xs[:byte-size-single-element-array 16]" s)

(* A [nested] region lifts its inner into a synthesised wrapper struct, which is
   a 3D scope of its own: an inner sized from a field of the enclosing struct
   named something the wrapper never declared, and EverParse rejected the schema
   with "Variable n not found". The wrapper takes the name as a formal and the
   use site passes the field on. *)
let test_3d_nested_inner_sized_by_a_field () =
  let f_n = Field.v "n" uint8 in
  let c =
    Codec.v "NestRef"
      (fun n a -> (n, a))
      Codec.
        [
          (Field.v "n" uint8 $ fun (n, _) -> n);
          ( Field.v "a" (nested ~size:(int 4) (byte_array ~size:(Field.ref f_n)))
          $ fun (_, a) -> a );
        ]
  in
  let check mode =
    let out =
      flatten
        (to_3d (Wire.Private.Everparse.module_ (Everparse.project ~mode c)))
    in
    Alcotest.(check bool)
      "wrapper declares the field it is sized by" true
      (contains ~sub:"(UINT32 n)" out);
    Alcotest.(check bool)
      "use site passes the field to the wrapper" true
      (contains ~sub:"(n) a[:byte-size-single-element-array 4]" out)
  in
  check `Ffi;
  check `Standalone

(* Two [nested] inners of different shapes each need a wrapper of their own.
   Both used to be named after the same catch-all, so the second was dropped as
   a repeat of the first and its field then validated against the first's
   length. *)
let test_3d_nested_wrapper_per_shape () =
  let f_n = Field.v "n" uint8 in
  let f_m = Field.v "m" uint8 in
  let c =
    Codec.v "TwoNest"
      (fun n m a b -> (n, m, a, b))
      Codec.
        [
          (Field.v "n" uint8 $ fun (n, _, _, _) -> n);
          (Field.v "m" uint8 $ fun (_, m, _, _) -> m);
          ( Field.v "a" (nested ~size:(int 4) (byte_array ~size:(Field.ref f_n)))
          $ fun (_, _, a, _) -> a );
          ( Field.v "b" (nested ~size:(int 8) (byte_slice ~size:(Field.ref f_m)))
          $ fun (_, _, _, b) -> b );
        ]
  in
  let out =
    flatten
      (to_3d (Wire.Private.Everparse.module_ (Everparse.project ~mode:`Ffi c)))
  in
  Alcotest.(check bool)
    "the first inner keeps its own length" true
    (contains ~sub:"UINT8 v[:byte-size n]" out);
  Alcotest.(check bool)
    "the second inner keeps its own length" true
    (contains ~sub:"UINT8 v[:byte-size m]" out);
  Alcotest.(check bool)
    "each use site passes its own field" true
    (contains ~sub:"(m) b[:byte-size-single-element-array 8]" out)

let test_3d_byte_array_where () =
  (* Synthesises a refinement struct so 3D can apply the per-byte constraint
     to each element of the byte-size array. *)
  let f_len = Field.v "Length" uint16be in
  let f_data =
    Field.v "Data"
      (byte_array_where ~size:(Field.ref f_len)
         ~per_byte:Expr.(fun b -> b >= int 0x20 && b <= int 0x7e))
  in
  let codec =
    Codec.v "Printable"
      (fun len data -> (len, data))
      Codec.[ f_len $ fst; f_data $ snd ]
  in
  let schema = Wire.Everparse.project ~mode:`Ffi codec in
  let s =
    Wire.Private.Everparse.Raw.to_3d (Wire.Private.Everparse.module_ schema)
  in
  Alcotest.(check bool)
    "synth typedef present" true
    (contains ~sub:"struct Wire_RefByte_" s);
  Alcotest.(check bool)
    "field references synth" true
    (contains ~sub:"_RefByte_" s);
  Alcotest.(check bool)
    "constraint inlined" true
    (contains ~sub:">= 32" s && contains ~sub:"<= 126" s);
  let synth_ref =
    Re.execp
      (Re.compile
         (Re.seq
            [ Re.str "_RefByte_"; Re.rep1 Re.digit; Re.str " Data[:byte-size" ]))
      s
  in
  Alcotest.(check bool) "Data references synth not raw UINT8" true synth_ref

(* A lookup / cases field decodes only in-range indices (OCaml raises
   [Invalid_tag] otherwise); the 3D projection must emit the matching [< len]
   refinement so the generated C validator rejects the same out-of-range inputs.
   Without it the verified C is more permissive than the OCaml decoder. *)
let test_lookup_index_bound () =
  let codec =
    Codec.v "Lk"
      (fun v -> v)
      Codec.[ (Field.v "v" (lookup [ 'a'; 'b'; 'c'; 'd' ] uint8) $ fun v -> v) ]
  in
  let output =
    to_3d (module_ [ typedef (Private.Everparse.Raw.struct_of_codec codec) ])
  in
  Alcotest.(check bool)
    "lookup field bounds its index" true
    (contains ~sub:"(v < 4)" output)

let test_array_lookup_element_bound () =
  (* A lookup as an array element must bound every element, not just a scalar
     field, or the verified C validator accepts out-of-range indices the OCaml
     decoder rejects. The element projects through a synthesised refined-byte
     struct, as a byte_array_where does. *)
  let codec =
    Codec.v "ArrLk"
      (fun v -> v)
      Codec.
        [
          ( Field.v "a"
              (array ~len:(int 3) (lookup [ 'a'; 'b'; 'c'; 'd' ] uint8))
          $ fun v -> v );
        ]
  in
  (* The synthesised element struct is emitted by the full schema projection,
     not [struct_of_codec] alone, so render the whole module. *)
  let output =
    to_3d (Wire.Private.Everparse.module_ (Everparse.project ~mode:`Ffi codec))
  in
  Alcotest.(check bool)
    "array element bounds its index" true
    (contains ~sub:"(__elt_lk4 < 4)" output);
  Alcotest.(check bool)
    "element refined via synth struct" true
    (contains ~sub:"_RefByte_lk4 a" output)

(* A lookup is exhaustive when its variant count fills the field's bit width
   exactly ([nvariants = 2 ^ width]): every representable value is then a
   valid index, so the [< nvariants] refinement [test_lookup_index_bound]
   checks for above would always hold -- a dead check EverParse would still
   compile into the generated C. This is the exact shape of ocaml-uslp's
   [src_or_dest] field (a 2-variant lookup over a 1-bit UINT32BE), which
   motivated suppressing the bound (see [lookup_is_exhaustive] in
   lib/types.ml). One variant short of the same field width (3 variants over
   2 bits, i.e. [3 < 4]) must still emit its bound: exhaustiveness, not
   merely "small nvariants", drives the suppression. *)
let test_lookup_index_bound_exhaustive () =
  let exhaustive =
    Codec.v "LkExhaustive"
      (fun v -> v)
      Codec.
        [
          (Field.v "v" (lookup [ 'a'; 'b' ] (bits ~width:1 U32be)) $ fun v -> v);
        ]
  in
  let out =
    to_3d
      (module_ [ typedef (Private.Everparse.Raw.struct_of_codec exhaustive) ])
  in
  Alcotest.(check bool)
    "exhaustive lookup still emits the bitwidth suffix" true
    (contains ~sub:"UINT32BE v : 1" out);
  Alcotest.(check bool)
    "exhaustive lookup emits no range bound" false (contains ~sub:"< 2" out);
  let non_exhaustive =
    Codec.v "LkNonExhaustive"
      (fun v -> v)
      Codec.
        [
          ( Field.v "v" (lookup [ 'a'; 'b'; 'c' ] (bits ~width:2 U32be))
          $ fun v -> v );
        ]
  in
  let out2 =
    to_3d
      (module_
         [ typedef (Private.Everparse.Raw.struct_of_codec non_exhaustive) ])
  in
  Alcotest.(check bool)
    "one variant short of exhaustive still bounds its index" true
    (contains ~sub:"(v < 3)" out2)

let test_optional_scalar_regions () =
  List.iter
    (fun at_most ->
      let tag = Field.v "tag" uint8 and size_f = Field.v "size" uint8 in
      let region =
        (if at_most then nested_at_most else nested)
          ~size:(Field.ref size_f) uint16be
      in
      let c =
        Codec.v "OptionalRegion"
          (fun t s v -> (t, s, v))
          Codec.
            [
              (tag $ fun (t, _, _) -> t);
              (size_f $ fun (_, s, _) -> s);
              ( Field.optional "value"
                  ~present:Expr.(Field.ref tag = int 1)
                  region
              $ fun (_, _, v) -> v );
            ]
      in
      List.iter
        (fun mode ->
          let out =
            flatten
              (to_3d
                 (Wire.Private.Everparse.module_ (Everparse.project ~mode c)))
          in
          Alcotest.(check bool)
            "a gate preserves one optional region" true
            (contains ~sub:"OptRegion_" out);
          Alcotest.(check bool)
            "the scalar region has a guarded size" true
            (contains
               ~sub:
                 (if at_most then "_region_size >= 2" else "_region_size == 2")
               out);
          Alcotest.(check bool)
            "the scalar region has its own parser" true
            (contains
               ~sub:
                 (if at_most then "ScalarRegionAtMost_"
                  else "ScalarRegionExact_")
               out))
        [ `Ffi; `Standalone ])
    [ false; true ]

let suite =
  ( "everparse_extract",
    [
      Alcotest.test_case "3d: optional scalar regions" `Quick
        test_optional_scalar_regions;
      Alcotest.test_case "generation: lookup index bound" `Quick
        test_lookup_index_bound;
      Alcotest.test_case "generation: lookup index bound, exhaustive" `Quick
        test_lookup_index_bound_exhaustive;
      Alcotest.test_case "generation: array lookup element bound" `Quick
        test_array_lookup_element_bound;
      Alcotest.test_case "3d: codec embed" `Quick test_3d_codec_embed;
      Alcotest.test_case "3d: codec nested" `Quick test_3d_codec_nested;
      Alcotest.test_case "3d: optional present" `Quick test_3d_optional_present;
      Alcotest.test_case "3d: optional absent" `Quick test_3d_optional_absent;
      Alcotest.test_case "3d: repeat" `Quick test_3d_repeat;
      Alcotest.test_case "3d: tm-like" `Quick test_3d_tm_like;
      Alcotest.test_case "3d: dep-size schema" `Quick test_3d_dep_size_schema;
      Alcotest.test_case "3d: dep-size roundtrip" `Quick
        test_3d_dep_size_roundtrip;
      Alcotest.test_case "3d: param in size" `Quick test_3d_param_in_size;
      Alcotest.test_case "3d: param embed" `Quick test_3d_param_embed;
      Alcotest.test_case "3d: nested over array" `Quick
        test_3d_nested_over_array;
      Alcotest.test_case "3d: nested inner sized by a field" `Quick
        test_3d_nested_inner_sized_by_a_field;
      Alcotest.test_case "3d: one nested wrapper per shape" `Quick
        test_3d_nested_wrapper_per_shape;
      Alcotest.test_case "3d: byte_array_where synth typedef" `Quick
        test_3d_byte_array_where;
    ] )
