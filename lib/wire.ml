(* Wire: Dependent Data Descriptions for EverParse 3D *)

module Error = Wire_runtime.Error
module UInt32 = UInt32
module UInt63 = UInt63
module Action = Action
module Param = Param
module Field = Field
include Types

type bitfield = U8 | U16 | U16be | U32 | U32be
type param = Types.param

let param_name (p : param) = p.param_name
let param_is_mutable (p : param) = p.mutable_

let param_c_type (p : param) =
  let (Types.Pack_typ t) = p.param_typ in
  Types.c_type_of t

let _field_ref = Types.ref
let map ~decode ~encode inner = Types.map decode encode inner

let bool (b : Stdlib.Bool.t) : _ Types.expr =
  if b then Types.true_ else Types.false_

let bit = Types.bool
let empty = Types.unit
let fixed_size = Types.field_wire_size
let lookup = Types.cases

(* A string can be viewed as bytes while it is only read, but a mutable slice
   backed by that view must not escape. Codecs hide their result type behind a
   decode closure, so record once, when constructing their typ, whether their
   structural form can retain a slice. A legacy [Struct] produces [unit], hence
   its fields do not escape. *)
let rec typ_returns_slice : type a. a typ -> bool = function
  | Byte_slice _ -> true
  | Where { inner; _ } -> typ_returns_slice inner
  | Single_elem { elem; _ } -> typ_returns_slice elem
  | Map { inner; _ } -> typ_returns_slice inner
  | Apply { typ; _ } -> typ_returns_slice typ
  | Optional { inner; _ } -> typ_returns_slice inner
  | Optional_or { inner; _ } -> typ_returns_slice inner
  | Enum { base; _ } -> typ_returns_slice base
  | Array { elem; _ } -> typ_returns_slice elem
  | Repeat { elem; _ } -> typ_returns_slice elem
  | Casetype { tag; cases; _ } ->
      typ_returns_slice tag
      || List.exists
           (fun (Case_branch { cb_inner; _ }) -> typ_returns_slice cb_inner)
           cases
  | Union { cases; _ } ->
      List.exists
        (fun (Case_branch { cb_inner; _ }) -> typ_returns_slice cb_inner)
        cases
  | Codec { codec_returns_slice; _ } -> codec_returns_slice
  | Uint8 | Uint16 _ | Uint32 _ | Uint64 _ | Int8 | Int16 _ | Int32 _ | Int64 _
  | Float32 _ | Float64 _ | Uint_var _ | Bits _ | Unit | All_bytes | All_zeros
  | Zeroterm | Zeroterm_at_most _ | Byte_array _ | Byte_array_where _ | Struct _
  | Type_ref _ | Qualified_ref _ | Der _ ->
      false

let struct_returns_slice s =
  List.exists
    (fun (Field { field_typ; _ }) -> typ_returns_slice field_typ)
    s.fields

(* IEEE 754 predicates compile to bit-mask checks over the float's bit
   pattern, which [build_populate] stores for float fields: a float32's in
   the int slot (its 31 low bits carry the whole exponent and mantissa on any
   platform), a float64's in the int64 slot, where the exponent bits 52-62
   stay exact even on a narrow-int platform. Both produce the same
   [(v >> N) & M] / [== M] checks on wire's OCaml decoder and EverParse's
   verified C decoder. *)

let is_float64 f =
  match Field.typ f with
  | Types.Float32 _ -> false
  | Types.Float64 _ -> true
  | _ -> invalid_arg "Wire: not a float field"

let is_finite (f : float Field.t) : bool Types.expr =
  if is_float64 f then
    let r = Types.Ref (Types.I64, Field.name f) in
    Ne (Land64 (Lsr64 (r, Int 52), Int64 0x7FFL), Int64 0x7FFL)
  else
    let r = Field.ref f in
    Expr.(Land (Lsr (r, Int 23), Int 0xFF) <> Int 0xFF)

let is_nan (f : float Field.t) : bool Types.expr =
  if is_float64 f then
    let r = Types.Ref (Types.I64, Field.name f) in
    And
      ( Eq (Land64 (Lsr64 (r, Int 52), Int64 0x7FFL), Int64 0x7FFL),
        Ne (Land64 (r, Int64 0xF_FFFF_FFFF_FFFFL), Int64 0L) )
  else
    let r = Field.ref f in
    Expr.(
      Land (Lsr (r, Int 23), Int 0xFF) = Int 0xFF
      && Land (r, Int 0x007F_FFFF) <> Int 0)

let codec (c : 'r Codec.t) : 'r typ =
  let codec_decode = Codec.embed_decode_ctx c in
  let codec_validate = Codec.embed_validate_ctx c in
  let codec_validate_sized = Codec.embed_validate_sized_ctx c in
  let codec_check_end = Codec.embed_check_end c in
  let codec_walk = Codec.embed_walk c in
  let codec_encode = Codec.embed_encode_ctx c in
  let codec_field_readers = Codec.field_readers_ctx c in
  let codec_struct = Codec.to_struct c in
  let codec_returns_slice = struct_returns_slice codec_struct in
  let codec_size_of_value = Codec.encoded_size_ctx c in
  let codec_min_size = Codec.min_size c in
  let codec_ref = Codec.embed_ref c in
  match Codec.wire_size_info_ctx c with
  | `Fixed n ->
      Codec
        {
          codec_name = Codec.name c;
          codec_id = Codec.id c;
          codec_decode;
          codec_validate;
          codec_validate_sized;
          codec_check_end;
          codec_walk;
          codec_encode;
          codec_fixed_size = Some n;
          codec_min_size;
          codec_size_of = (fun _ctx _input_end _buf _off -> n);
          codec_size_of_value;
          codec_field_readers;
          codec_returns_slice;
          codec_struct;
          codec_ref;
        }
  | `Variable size_of ->
      Codec
        {
          codec_name = Codec.name c;
          codec_id = Codec.id c;
          codec_decode;
          codec_validate;
          codec_validate_sized;
          codec_check_end;
          codec_walk;
          codec_encode;
          codec_fixed_size = None;
          codec_min_size;
          codec_size_of = size_of;
          codec_size_of_value;
          codec_field_readers;
          codec_returns_slice;
          codec_struct;
          codec_ref;
        }

type ('elt, 'seq) seq_map = ('elt, 'seq) Types.seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map

(* A per-byte condition is checked when the type is built: it projects to an
   element struct of its own, which sees no parameter and no other field. *)
let byte_array_where ~size ~per_byte =
  let t = Types.byte_array_where ~size ~per_byte in
  (match t with
  | Byte_array_where { elt_var; cond; _ } -> (
      try Types.check_per_byte elt_var cond
      with Invalid_argument m ->
        Fmt.invalid_arg "Wire.byte_array_where: per-byte condition: %s" m)
  | _ -> ());
  t

let seq_list = Types.seq_list
let array_seq = Types.array_seq

let rest_bytes total =
  Types.byte_array ~size:Types.(Sub (Param_ref total, Sizeof_this))

let bits ?(bit_order = Types.Msb_first) ~width bf =
  let base =
    match bf with
    | U8 -> Types.bf_uint8
    | U16 -> Types.bf_uint16
    | U16be -> Types.bf_uint16be
    | U32 -> Types.bf_uint32
    | U32be -> Types.bf_uint32be
  in
  Types.bits ~bit_order ~width base

module Expr = struct
  include Types.Expr

  let true_ = Types.true_
  let false_ = Types.false_
  let bool b = if b then Types.true_ else Types.false_
end

module UInt64 = UInt64
module SInt32 = SInt32
module SInt8 = SInt8
module SInt16 = SInt16
module UInt8 = UInt8
module Buf = Buf
module UInt16 = UInt16
module Asn1 = Asn1
module Ascii = Ascii

module Private = struct
  module UInt32 = UInt32
  module Unchecked = Unchecked
  module UInt63 = UInt63
  module Input_end = Input_end
  module Types = Types
  module Eval = Eval
  module Expr_compiler = Expr_compiler
  module Bitfield = Bitfield
  module Uint_var = Uint_var
  module Shape = Shape
  module Gen = Codec.Gen
  module Testing = Codec.Testing
  module Names = Names
  module Layout = Record_layout
  module Cbor_det = Cbor_det

  type packed = Field.packed =
    | Named : 'a Field.t -> packed
    | Anon : 'a Field.anon -> packed

  let decl_of_packed = Field.decl_of_packed

  module Everparse = struct
    include Everparse

    let module_ (t : t) = t.module_
    let wire_size (t : t) = t.wire_size
    let source (t : t) = t.source
  end

  type validator = Codec.validator

  let validator_of_struct = Codec.validator_of_struct
  let validate_struct = Codec.validate_struct
  let struct_size_of = Codec.struct_size_of
  let struct_min_size = Codec.struct_min_size
  let wire_size_info_of_validator = Codec.wire_size_info_of_validator

  module Ascii = Ascii

  let param_name = param_name
  let param_is_mutable = param_is_mutable
  let param_c_type = param_c_type
  let ml_type_of = Types.ml_type_of
  let c_type_of = Types.c_type_of
end

(* The public [Codec] and [Everparse] over the internal ones. *)

module Everparse = struct
  include Everparse

  let name (t : t) = t.name
end

module Codec = struct
  module C = Codec

  type 'r t = 'r C.t
  type ('a, 'r) field = ('a, 'r) C.def

  type ('f, 'k, 'r) fields = ('f, 'k, 'r) C.Defs.t =
    | [] : ('k, 'k, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'k, 'r) fields -> ('a -> 'f, 'k, 'r) fields

  let ( $ ) f proj = C.Def (C.( $ ) f proj)

  let v name ?where ?doc make fields =
    C.v name ?where ?doc make (C.fields_of_defs fields)

  type ('s, 'r) layer = ('s, 'r) C.layer

  let layer = C.layer
  let opt_layer = C.opt_layer
  let ( / ) = C.path
  let ( // ) = C.under
  let name = C.name
  let rename = C.rename
  let doc = C.doc
  let env = C.env
  let min_size = C.min_size
  let fixed_size = C.size
  let decode ?consume ?env c buf ~off = C.decode ?consume ?env c buf off
  let validate ?env c buf ~off = C.validate ?env c buf off
  let encode ?env c r buf ~off = C.encode ?env c r buf off
  let to_bytes = C.to_bytes
  let to_string = C.to_string
  let size = C.encoded_size
  let size_at ?env c buf ~off = C.size_at ?env c buf off
  let pp_value = C.pp_value

  module View = struct
    include C.View

    type ('a, 'r) field = ('a, 'r) C.View.rfield
    type ('s, 'r) layer = ('s, 'r) C.View.sub_layer
  end
end
