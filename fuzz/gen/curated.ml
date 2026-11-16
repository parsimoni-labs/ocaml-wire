open Gen
open Leaves
open Wrappers
open Record
open Columns
open Compose

(* {1 Canonical registry and EverParse drivers}

   Every fuzz suite drives off [registry] rather than its own hand-written
   list, so a generated codec is exercised on the OCaml round-trip path
   ([test_cases]) and the 3D projection path ([everparse_cases] /
   [everparse_3d_cases]) from one source. *)

type packed = Pack : 'a t -> packed

(* The value equality the generator was built with, so a caller can compare two
   decodes of the same record without knowing its type. *)
let equal g = g.equal
let codec g = g.codec
let binds_env (Pack g) = Option.is_some g.env

let registry_record =
  Codec.v "RegRecord" (fun a b -> (a, b)) Codec.[ uint8 $ fst; uint16be $ snd ]

(* One closed byte-wide enum, used both on its own and as a list element. *)
let registry_enum_color = enum "Color" [ ("Red", 1); ("Green", 2); ("Blue", 3) ]

(* Two bitfields packed into one base word, as a record whose field list the
   fuzzer keeps: the shape where a [Codec.set] that rewrites the whole word
   instead of its own bits destroys the neighbour, and the only shape in the
   registry where two fields share a byte. The hand-built [bitpack_*] gens
   pack the same way but seal their codec, so their slots are not addressable
   one at a time. *)
let registry_bits_u8 =
  Codec.v "RegBitsU8"
    (fun hi lo -> (hi, lo))
    Codec.[ bits ~width:3 Wire.U8 $ fst; bits ~width:5 Wire.U8 $ snd ]

let registry_bits_u16be =
  Codec.v "RegBitsU16be"
    (fun hi lo -> (hi, lo))
    Codec.[ bits ~width:4 Wire.U16be $ fst; bits ~width:12 Wire.U16be $ snd ]

(* {2 A sub-codec reached through a field}

   [array(2,record)] already puts a [Wire.codec] in an element position, but no
   entry put one in a field position, which is the position that makes the
   projection declare the inner struct before the outer names it and makes the
   outer's later fields start at the inner's own width rather than a scalar's.
   The gens below fill it: a sub-codec last, first, twice, two levels deep, and
   one whose width the inner's own contents decide. *)

let registry_subcodec_last =
  Codec.v "RegSubLast"
    (fun tag body -> (tag, body))
    Codec.[ uint8 $ fst; registry_record $ snd ]

(* The sub-codec first: its bitfields must not coalesce with the outer field
   after it, and that field starts at the inner struct's width. *)
let registry_subcodec_first =
  Codec.v "RegSubFirst"
    (fun body tail -> (body, tail))
    Codec.[ registry_bits_u8 $ fst; uint16be $ snd ]

(* The same sub-codec in two slots: one declaration, two references. *)
let registry_subcodec_twice =
  Codec.v "RegSubTwice"
    (fun a b -> (a, b))
    Codec.[ registry_record $ fst; registry_record $ snd ]

let registry_subcodec_leaf =
  Codec.v "RegSubLeaf" (fun a b -> (a, b)) Codec.[ uint8 $ fst; uint8 $ snd ]

let registry_subcodec_mid =
  Codec.v "RegSubMid"
    (fun leaf tail -> (leaf, tail))
    Codec.[ registry_subcodec_leaf $ fst; uint16be $ snd ]

(* Two levels: the leaf has to be declared before the struct that names it, and
   that one before the outer. *)
let registry_subcodec_deep =
  Codec.v "RegSubDeep"
    (fun tag mid -> (tag, mid))
    Codec.[ uint8 $ fst; registry_subcodec_mid $ snd ]

(* A sub-codec whose width its own length prefix decides, with a field after it:
   the trailing field's offset is what the inner consumed, not a constant. *)
let registry_subcodec_var_body =
  repeat_sized "RegSubVarBody"
    (fun ~size typ -> Wire.Field.repeat "items" ~size typ)
    ~bytes:8 uint8

let registry_subcodec_var =
  Codec.v "RegSubVar"
    (fun body tail -> (body, tail))
    Codec.[ registry_subcodec_var_body $ fst; uint8 $ snd ]

(* A valid NUL-terminated sub-codec: its variable field is the only field in the
   inner record, so the containing field can still be projected and extracted. *)
let registry_subcodec_zt_only_body =
  Codec.v "RegSubZtOnlyBody" (fun a -> a) Codec.[ zeroterm $ Fun.id ]

let registry_subcodec_zt_only =
  Codec.v "RegSubZtOnly"
    (fun tag body -> (tag, body))
    Codec.[ uint8 $ fst; registry_subcodec_zt_only_body $ snd ]

let registry_casetype =
  casetype_u8 "RegPayload"
    [
      case ~index:1 uint16be
        ~inject:(fun v -> `A v)
        ~project:(function `A v -> Some v | _ -> None);
      case ~index:2 uint32be
        ~inject:(fun v -> `B v)
        ~project:(function `B v -> Some v | _ -> None);
      default_case ~tag:0xFF uint8
        ~inject:(fun v -> `Other v)
        ~project:(function `Other v -> Some v | _ -> None);
    ]

(* One casetype per integer carrier an enum base can take. Each dispatches the
   same two branches; what differs is what the tag's case index has to be
   projected through, which is where the projection used to assert. *)
let tagged_cases =
  [
    case ~index:1 uint16be
      ~inject:(fun v -> `A v)
      ~project:(function `A v -> Some v | _ -> None);
    case ~index:2 uint32be
      ~inject:(fun v -> `B v)
      ~project:(function `B v -> Some v | _ -> None);
  ]

let registry_casetype_enum_tag =
  casetype_tagged "RegEnumTag"
    (Wire.enum "RegEnumTagKind" [ ("A", 1); ("B", 2) ] Wire.uint8)
    Wire.UInt8.v tagged_cases

let registry_casetype_lookup_tag =
  casetype_tagged "RegLookupTag"
    (Wire.enum "RegLookupTagKind"
       [ ("A", 1); ("B", 2) ]
       (Wire.lookup [ 0; 1; 2 ] Wire.uint8))
    Fun.id tagged_cases

let registry_casetype_where_tag =
  casetype_tagged "RegWhereTag"
    (Wire.enum "RegWhereTagKind"
       [ ("A", 1); ("B", 2) ]
       (Wire.where Wire.Expr.(Wire.int 0 = Wire.int 0) Wire.uint8))
    Wire.UInt8.v tagged_cases

let registry_casetype_u64_tag =
  casetype_tagged "RegU64Tag"
    (Wire.enum "RegU64TagKind" [ ("A", 1); ("B", 2) ] Wire.uint64)
    Wire.UInt64.of_int tagged_cases

(* A casetype dispatching on an enum whose type and case names are 3D and F*
   keywords: each case label is the escaped enum constant. *)
let registry_casetype_keyword_tag =
  casetype_tagged "RegKeywordTag"
    (Wire.enum "end" [ ("in", 1); ("of", 2) ] Wire.uint8)
    Wire.UInt8.v tagged_cases

let unsigned_scalar_gens =
  [
    ("uint8", Pack uint8);
    ("uint16", Pack uint16);
    ("uint16be", Pack uint16be);
    ("uint16(endian_edges)", Pack uint16_endian_edges);
    ("uint16be(endian_edges)", Pack uint16be_endian_edges);
    ("uint32", Pack uint32);
    ("uint32be", Pack uint32be);
    ("uint32(endian_edges)", Pack uint32_endian_edges);
    ("uint32be(endian_edges)", Pack uint32be_endian_edges);
    ("uint64", Pack uint64);
    ("uint64be", Pack uint64be);
    ("uint64(endian_edges)", Pack uint64_endian_edges);
    ("uint64be(endian_edges)", Pack uint64be_endian_edges);
  ]

let signed_scalar_gens =
  [
    ("int8", Pack int8);
    ("int16", Pack int16);
    ("int16be", Pack int16be);
    ("int16(endian_edges)", Pack int16_endian_edges);
    ("int16be(endian_edges)", Pack int16be_endian_edges);
    ("int32", Pack int32);
    ("int32be", Pack int32be);
    ("int32(endian_edges)", Pack int32_endian_edges);
    ("int32be(endian_edges)", Pack int32be_endian_edges);
    ("int64", Pack int64);
    ("int64be", Pack int64be);
    ("int64(endian_edges)", Pack int64_endian_edges);
    ("int64be(endian_edges)", Pack int64be_endian_edges);
  ]

let float_and_empty_gens =
  [
    ("float32", Pack float32);
    ("float32be", Pack float32be);
    ("float64", Pack float64);
    ("float64be", Pack float64be);
    ("empty", Pack empty);
  ]

let variable_width_scalar_gens =
  [
    ("uint_var(1,little)", Pack (uint_var ~endian:Wire.Little 1));
    ("uint_var(2,little)", Pack (uint_var ~endian:Wire.Little 2));
    ("uint_var(3,little)", Pack (uint_var ~endian:Wire.Little 3));
    ("uint_var(3,big)", Pack (uint_var ~endian:Wire.Big 3));
    ("uint_var(5,little)", Pack (uint_var ~endian:Wire.Little 5));
    ("uint_var(5,big)", Pack (uint_var ~endian:Wire.Big 5));
    ("uint_var(7,little)", Pack (uint_var ~endian:Wire.Little 7));
    ("uint_var(7,big)", Pack (uint_var ~endian:Wire.Big 7));
    ("uint_var(3,little_edges)", Pack uint_var3_little_edges);
    ("uint_var(3,big_edges)", Pack uint_var3_big_edges);
    ("uint_var(7,little_edges)", Pack uint_var7_little_edges);
    ("uint_var(7,big_edges)", Pack uint_var7_big_edges);
    ("bit(uint8)", Pack (bit uint8));
    ("bit(uint16)", Pack (bit uint16));
    ("bit(uint16be)", Pack (bit uint16be));
    ("bit(int8)", Pack (bit int8));
    ("bit(int16)", Pack (bit int16));
    ("bit(int16be)", Pack (bit int16be));
  ]

let scalar_gens =
  unsigned_scalar_gens @ signed_scalar_gens @ float_and_empty_gens
  @ variable_width_scalar_gens

let byte_gens =
  [
    ("byte_array(0)", Pack (byte_array 0));
    ("byte_array(5)", Pack (byte_array 5));
    ("byte_slice(0)", Pack (byte_slice 0));
    ("byte_slice(3)", Pack (byte_slice 3));
    ( "byte_array_where(4)",
      Pack
        (byte_array_where 4 ~per_byte:(fun b ->
             Wire.Expr.(b >= Wire.int 0x20 && b <= Wire.int 0x7e))) );
    ("all_bytes", Pack all_bytes);
    ("all_zeros", Pack all_zeros);
    ("zeroterm", Pack zeroterm);
    ("zeroterm_at_most(1)", Pack (zeroterm_at_most 1));
    ("zeroterm_at_most(8)", Pack (zeroterm_at_most 8));
  ]

(* Every [bits] shape the registry draws. On js_of_ocaml (32-bit [int]) and
   wasm_of_ocaml (31-bit) [Wire.bits] refuses a width the native int cannot hold
   without its sign bit, so those shapes are left out there rather than
   aborting the registry at startup. *)
let bits_shapes =
  [
    (1, Wire.U8, Wire.Msb_first);
    (1, Wire.U8, Wire.Lsb_first);
    (3, Wire.U8, Wire.Msb_first);
    (3, Wire.U8, Wire.Lsb_first);
    (7, Wire.U8, Wire.Msb_first);
    (7, Wire.U8, Wire.Lsb_first);
    (8, Wire.U8, Wire.Msb_first);
    (8, Wire.U8, Wire.Lsb_first);
    (1, Wire.U16, Wire.Msb_first);
    (15, Wire.U16, Wire.Msb_first);
    (16, Wire.U16, Wire.Msb_first);
    (16, Wire.U16, Wire.Lsb_first);
    (1, Wire.U16be, Wire.Msb_first);
    (10, Wire.U16be, Wire.Msb_first);
    (4, Wire.U16be, Wire.Msb_first);
    (15, Wire.U16be, Wire.Lsb_first);
    (16, Wire.U16be, Wire.Msb_first);
    (16, Wire.U16be, Wire.Lsb_first);
    (1, Wire.U32, Wire.Msb_first);
    (17, Wire.U32, Wire.Msb_first);
    (17, Wire.U32, Wire.Lsb_first);
    (32, Wire.U32, Wire.Lsb_first);
    (1, Wire.U32be, Wire.Msb_first);
    (31, Wire.U32be, Wire.Msb_first);
    (31, Wire.U32be, Wire.Lsb_first);
    (32, Wire.U32be, Wire.Msb_first);
    (32, Wire.U32be, Wire.Lsb_first);
    (4, Wire.U32, Wire.Lsb_first);
  ]

let bits_label (width, base, bit_order) =
  let base =
    match base with
    | Wire.U8 -> "U8"
    | U16 -> "U16"
    | U16be -> "U16be"
    | U32 -> "U32"
    | U32be -> "U32be"
  in
  let lsb = match bit_order with Wire.Lsb_first -> ",Lsb" | Msb_first -> "" in
  Fmt.str "bits(%d,%s%s)" width base lsb

let int_holds_width width = width <= Sys.int_size - 1

let bits_gens =
  List.filter_map
    (fun ((width, base, bit_order) as shape) ->
      if int_holds_width width then
        Some (bits_label shape, Pack (bits ~bit_order ~width base))
      else None)
    bits_shapes

let bitpack_gens =
  [
    ("bitpack_u8_msb(1,2,5)", Pack bitpack_u8_msb);
    ("bitpack_u8_lsb(1,2,5)", Pack bitpack_u8_lsb);
    ("bitpack_u8_spill(5,5)", Pack bitpack_u8_spill);
    ("bitpack_split_orders", Pack bitpack_split_orders);
    ("bitpack_u16_msb(3,2,11)", Pack bitpack_u16_msb);
    ("bitpack_u16_lsb(3,2,11)", Pack bitpack_u16_lsb);
    ("bitpack_u16be_msb(3,2,11)", Pack bitpack_u16be_msb);
    ("bitpack_u16be_lsb(3,2,11)", Pack bitpack_u16be_lsb);
    ("bitpack_u16be_spill(9,9)", Pack bitpack_u16be_spill);
    ("bitpack_u32_msb(6,10,16)", Pack bitpack_u32_msb);
    ("bitpack_u32_lsb(6,10,16)", Pack bitpack_u32_lsb);
    ("bitpack_u32be_msb(6,10,16)", Pack bitpack_u32be_msb);
    ("bitpack_u32be_lsb(6,10,16)", Pack bitpack_u32be_lsb);
  ]

let wrapper_gens =
  [
    ( "map(uint8)",
      Pack
        (map
           ~decode:(fun n -> Wire.UInt8.to_int n * 2)
           ~encode:(fun n -> Wire.UInt8.v (n / 2))
           uint8) );
    ("variants", Pack (variants "Flag" [ ("A", `A); ("B", `B); ("C", `C) ]));
    ("variants_u16be", Pack variants_u16be);
    ("enum", Pack registry_enum_color);
    ("enum_u16be", Pack enum_u16be);
    ("enum_open", Pack enum_open);
    ("enum_open_u16be", Pack enum_open_u16be);
    ("bounded_u8", Pack (bounded_u8 ~min:10 ~max:100));
    ("bounded_u16be", Pack bounded_u16be);
    ("bounded_u32be", Pack bounded_u32be);
    ("where(uint8)", Pack (where uint8));
    ("lookup", Pack (lookup [ 'a'; 'b'; 'c'; 'd' ] uint8));
    ("optional(uint8)", Pack (optional uint8));
    ("optional(false)", Pack (optional ~present:false uint8));
    ("optional_or(uint8)", Pack (optional_or ~default:Wire.UInt8.zero uint8));
    ( "optional_or(false)",
      Pack (optional_or ~present:false ~default:(Wire.UInt8.v 42) uint8) );
    ("optional_dynamic", Pack optional_dynamic);
    ("optional_or_dynamic", Pack optional_or_dynamic);
    ("optional_dyn_after_span", Pack optional_dyn_after_span);
    ("union_after_span", Pack union_after_span);
    ("nested(0,empty)", Pack (nested 0 empty));
    ("nested(2,uint16be)", Pack (nested 2 uint16be));
    ("nested(4,byte_array4)", Pack (nested 4 (byte_array 4)));
    (* A [nested] region whose inner carries its own array / refinement
       qualifier is lifted into a synthesised [Se*] wrapper struct. Only the
       [SeBytes] arm above was reached; these three reach the rest of the
       family, and the refined-span one also orders the [_RefByte_*] typedef
       the wrapper names against the wrapper itself. *)
    ("nested(3,byte_slice3)", Pack (nested 3 (byte_slice 3)));
    ( "nested(4,byte_array_where4)",
      Pack
        (nested 4
           (byte_array_where 4 ~per_byte:(fun b ->
                Wire.Expr.(b >= Wire.int 0x20 && b <= Wire.int 0x7e)))) );
    ("nested(8,zeroterm_at_most8)", Pack (nested 8 (zeroterm_at_most 8)));
    ("nested_at_most(0,empty)", Pack (nested_at_most 0 empty));
    ("nested_at_most(2,uint16be)", Pack (nested_at_most 2 uint16be));
    ("nested_at_most(4,uint16be)", Pack (nested_at_most 4 uint16be));
  ]

let composite_gens =
  [
    ("array(0,uint8)", Pack (array 0 uint8));
    ("array(3,uint16be)", Pack (array 3 uint16be));
    ("array_seq(3,uint16be)", Pack (array_seq 3 uint16be));
    ("array_seq(0,uint8)", Pack (array_seq 0 uint8));
    ("array(2,record)", Pack (array 2 registry_record));
    ("array(3,bounded_u8)", Pack (array 3 (bounded_u8 ~min:10 ~max:100)));
    (* A closed enum under a byte budget cannot be spelled as a 1-byte enum
       type, so it goes through the synthesised [_EnumElt_] struct that carries
       the membership refinement per element. An [array] takes that route only
       when the base is wider than a byte or big-endian; a [repeat] takes it at
       every width. Neither position had an entry. *)
    ("array(2,enum_u16be)", Pack (array 2 enum_u16be));
    ("repeat(6,enum)", Pack (repeat ~bytes:6 registry_enum_color));
    ("repeat(0,uint8)", Pack (repeat ~bytes:0 uint8));
    ("repeat(8,uint8)", Pack (repeat ~bytes:8 uint8));
    ("repeat(7,uint16be)", Pack (repeat ~bytes:7 uint16be));
    ("repeat_seq(8,uint8)", Pack (repeat_seq ~bytes:8 uint8));
    ("repeat_seq(7,uint16be)", Pack (repeat_seq ~bytes:7 uint16be));
    ("record", Pack registry_record);
    ("subcodec_last(record)", Pack registry_subcodec_last);
    ("subcodec_first(bits)", Pack registry_subcodec_first);
    ("subcodec_twice(record)", Pack registry_subcodec_twice);
    ("subcodec_deep(record)", Pack registry_subcodec_deep);
    ("subcodec_var(repeat)", Pack registry_subcodec_var);
    ("subcodec_ztonly(zeroterm)", Pack registry_subcodec_zt_only);
    ("record_bits(3+5,U8)", Pack registry_bits_u8);
    ("record_bits(4+12,U16be)", Pack registry_bits_u16be);
    ("casetype_u8", Pack registry_casetype);
    ("casetype_u16be_default", Pack casetype_u16be_default);
    ("casetype_enum_tag", Pack registry_casetype_enum_tag);
    ("casetype_lookup_tag", Pack registry_casetype_lookup_tag);
    ("casetype_where_tag", Pack registry_casetype_where_tag);
    ("casetype_u64_tag", Pack registry_casetype_u64_tag);
    ("casetype_keyword_tag", Pack registry_casetype_keyword_tag);
    ("field_anon", Pack field_anon);
  ]

let param_action_gens =
  [
    ("action", Pack action);
    ("action_on_act", Pack action_on_act);
    ("expr_ops", Pack expr_ops);
    ("sizeof", Pack sizeof);
    ("codec_where", Pack codec_where);
    ("typ_where", Pack typ_where);
    ("field_constraint", Pack field_constraint);
    ("field_int", Pack field_int);
    ("self_int64", Pack self_int64);
    ("rest_bytes", Pack rest_bytes);
    ("param_input", Pack param_input);
    ("param_size", Pack param_size);
    ("if_then_else", Pack if_then_else);
    ("finite_float64", Pack finite_float64);
    ("nan_float64", Pack nan_float64);
  ]

(* Flat records shaped for a fast path each: a fixed span after the last
   data-sized one, a range check before an enum check, a byte table, the widest
   staged encode, and getters that answer differently on each call. *)
let flat_gens =
  let gen mks = Pack (snd (flat_of "Flat" mks)) in
  let wide16 = match wide 16 with Any a -> Pack a.g in
  [
    ( "flat_span_tail",
      gen [ span_group len_u8; fixed_bytes_group ~slice:false 4 ] );
    ( "flat_check_order",
      gen [ range8_group ~lo:0 ~hi:10; enum8_group [ ("x", 0) ] ] );
    ( "flat_table",
      gen [ bits_group ~limits:[ 5; 20 ] Wire.U8 [ 3; 5 ]; u8_group ] );
    ("flat_wide16", wide16);
    ( "flat_impure",
      Pack
        (snd
           (flat_of ~impure:true "Flat"
              [
                bits_group Wire.U8 [ 3 ];
                fixed_bytes_group ~slice:false 4;
                u8_group;
              ])) );
  ]

(* A registry label rendered as a unique EverParse struct name: the
   alphanumeric characters with the first capitalized and the rest lowercased,
   e.g. ["uint_var(3,big)"] becomes ["Uintvar3big"]. EverParse lowercases all
   but the first character of a module name when it generates C, so keeping no
   internal capitals makes the generated validator symbol match what the FFI
   stubs reference. *)
let camel_of_label label =
  let buf = Buffer.create (String.length label) in
  let first = ref true in
  String.iter
    (fun c ->
      let alnum =
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
      in
      if alnum then begin
        Buffer.add_char buf
          (if !first then Char.uppercase_ascii c else Char.lowercase_ascii c);
        first := false
      end)
    label;
  let s = Buffer.contents buf in
  if s = "" then "Case" else s

(* Give each registry case a unique 3D struct name derived from its label, so
   every case projects to a distinct schema and the per-codec validators link
   without entrypoint-name collisions. The name is metadata only
   ({!Wire.Codec.rename}): it changes neither the wire encoding nor the
   generators, so the positive / random / adversarial streams are untouched. *)
let rename_case label (Pack g) =
  Pack { g with codec = Wire.Codec.rename (camel_of_label label) g.codec }

let registry : (string * packed) list =
  List.map
    (fun (label, p) -> (label, rename_case label p))
    (scalar_gens @ byte_gens @ bits_gens @ bitpack_gens @ wrapper_gens
   @ composite_gens @ param_action_gens @ flat_gens)
