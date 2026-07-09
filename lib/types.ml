type endian = Little | Big
type bit_order = Msb_first | Lsb_first

(* Sequence builder for Array/Repeat -- Jsont-style accumulator pattern.
   Existentially hides the builder type so callers control the output container. *)
type ('elt, 'seq) seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map

(* Param handles -- defined here so expr and action_stmt can reference them *)
type param_input
type param_output

type ('a, 'k) param_handle = {
  name : string;
  typ : 'a typ;
  packed_typ : packed_typ;
  mutable_ : bool;
  cell : int ref;
      (* [cell] is the per-handle backing read by encode/size expressions and
         the value-forwarding vehicle for embedded sub-codecs. Slot resolution
         (decode array index, env index) is NOT stored here: it is per-codec,
         since one handle may be referenced both standalone and from an
         embedding, which would clash on a single mutable field. *)
}

and packed_typ = Pack_typ : 'a typ -> packed_typ

(* Expressions *)
and _ ref_kind = I : int ref_kind | I64 : int64 ref_kind

and _ expr =
  | Int : int -> int expr
  | Int64 : int64 -> int64 expr
  | Bool : bool -> bool expr
  | Ref : 'a ref_kind * string -> 'a expr
  | Param_ref : ('a, 'k) param_handle -> int expr
  | Sizeof : 'a typ -> int expr
  | Sizeof_this : int expr
  | Field_pos : int expr
  | Add : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
  | Div : int expr * int expr -> int expr
  | Mod : int expr * int expr -> int expr
  | Land : int expr * int expr -> int expr
  | Lor : int expr * int expr -> int expr
  | Lxor : int expr * int expr -> int expr
  | Lnot : int expr -> int expr
  | Lsl : int expr * int expr -> int expr
  | Lsr : int expr * int expr -> int expr
  | Eq : 'a expr * 'a expr -> bool expr
  | Ne : 'a expr * 'a expr -> bool expr
  | Lt : 'a expr * 'a expr -> bool expr
  | Le : 'a expr * 'a expr -> bool expr
  | Gt : 'a expr * 'a expr -> bool expr
  | Ge : 'a expr * 'a expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  | Cast : [ `U8 | `U16 | `U32 | `U64 ] * int expr -> int expr
  | If_then_else : bool expr * int expr * int expr -> int expr

(* Bitfield base types - standalone, not mutually recursive *)
and bitfield_base = U8 | U16 of endian | U32 of endian

(* Types *)
and _ typ =
  | Uint8 : int typ
  | Uint16 : endian -> int typ
  | Uint32 : endian -> UInt32.t typ
  | Uint63 : endian -> UInt63.t typ
  | Uint64 : endian -> int64 typ (* boxed, for full 64-bit *)
  | Int8 : int typ
  | Int16 : endian -> int typ
  | Int32 : endian -> int typ (* fits OCaml int on 64-bit hosts *)
  | Int64 : endian -> int64 typ
  | Float32 :
      endian
      -> float typ (* IEEE 754 binary32, widened to OCaml float *)
  | Float64 : endian -> float typ (* IEEE 754 binary64 *)
  | Uint_var : { size : int expr; endian : endian } -> int typ
  | Bits : {
      width : int;
      base : bitfield_base;
      bit_order : bit_order;
    }
      -> int typ
  | Unit : unit typ
  | All_bytes : string typ
  | All_zeros : string typ
  | Zeroterm : string typ
  | Zeroterm_at_most : { size : int expr } -> string typ
  | Where : { cond : bool expr; inner : 'a typ } -> 'a typ
  | Array : {
      len : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ
  | Byte_array : { size : int expr } -> string typ
  | Byte_array_where : {
      size : int expr;
      elt_var : string;
      cond : bool expr;
    }
      -> string typ
  | Byte_slice : { size : int expr } -> Bytesrw.Bytes.Slice.t typ
  | Single_elem : { size : int expr; elem : 'a typ; at_most : bool } -> 'a typ
  | Enum : {
      name : string;
      cases : (string * int) list;
      base : int typ;
      closed : bool;
          (* [true]: only the listed values are valid (decode rejects others, the
             3D projection emits a membership refinement). [false]: an open set,
             the names document known values but any value is accepted. *)
    }
      -> int typ
  | Casetype : {
      name : string;
      tag : 'k typ;
      cases : ('a, 'k) case_branch list;
    }
      -> 'a typ
  | Struct : struct_ -> unit typ
  | Type_ref : string -> 'a typ
  | Qualified_ref : { module_ : string; name : string } -> 'a typ
  | Map : {
      inner : 'w typ;
      decode : 'w -> 'a;
      encode : 'a -> 'w;
      index_bound : int option;
          (* When [Some n] the raw [inner] value is a valid index only when
             [< n] (set by {!cases} / lookup). [decode] enforces this on the
             OCaml side; the 3D projection emits it as a field refinement so the
             generated C validator rejects the same out-of-range inputs. *)
    }
      -> 'a typ
  | Apply : { typ : 'a typ; args : packed_expr list } -> 'a typ
  | Codec : {
      codec_name : string;
      codec_decode : bytes -> int -> 'r;
      codec_encode : 'r -> bytes -> int -> int;
          (* Returns the offset after the bytes the encoder wrote. *)
      codec_fixed_size : int option;
      codec_size_of : bytes -> int -> int;
      codec_size_of_value : 'r -> int;
          (* Encoded byte length of a value, computed from the value rather
             than by re-reading the buffer. The buffer-driven [codec_size_of]
             is wrong for variable-size codecs ending in [all_bytes] /
             [rest_bytes] / [all_zeros]: it reads "remaining buffer space",
             not the value's actual tail length. *)
      codec_field_readers : (string * (bytes -> int -> int)) list;
      codec_struct : struct_;
          (** Structural representation of the codec. Mirrors [codec_decode] /
              [codec_encode] but in a form 3D projection can walk. *)
    }
      -> 'r typ
  | Optional : { present : bool expr; inner : 'a typ } -> 'a option typ
  | Optional_or : {
      present : bool expr;
      inner : 'a typ;
      default : 'a;
    }
      -> 'a typ
  | Repeat : {
      size : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ

and ('a, 'k) case_branch =
  | Case_branch : {
      cb_tag : 'k option;
          (* Decode-match tag: [Some t] for an explicit case, [None] for the
             default branch (matches any unclaimed tag). *)
      cb_inner : 'w typ;
      cb_inject : 'k -> 'w -> 'a;
          (* Receives the matched tag (so a default branch can recover the
             actual tag it caught) and the decoded body. *)
      cb_project : 'a -> ('k * 'w) option;
          (* Yields the tag to encode and the body. An explicit case pairs its
             own fixed tag; the default branch yields the tag carried by the
             value, so an arbitrary unclaimed tag round-trips. *)
    }
      -> ('a, 'k) case_branch

and packed_expr = Pack_expr : 'a expr -> packed_expr

(* Structs *)
and struct_ = {
  name : string;
  params : param list;
  where : bool expr option;
  fields : field list;
}

and field =
  | Field : {
      field_name : string option;
      field_typ : 'a typ;
      constraint_ : bool expr option;
      action : action option;
      field_doc : string option;
    }
      -> field

and param = { param_name : string; param_typ : packed_typ; mutable_ : bool }

(* Actions *)
and action = Success of action_stmt list | Act of action_stmt list

and action_stmt =
  | Assign : ('a, param_output) param_handle * int expr -> action_stmt
  | Field_assign of string * string * int expr
    (* [Field_assign (ptr, field_name, expr)] emits [ptr->field_name = expr;] *)
  | Extern_call of string * string list
    (* [Extern_call (fn, args)] emits [fn(arg1, arg2, ...);] *)
  | Return of bool expr
  | Abort
  | If of bool expr * action_stmt list * action_stmt list option
  | Var of string * int expr

type param_env = {
  codec_id : int;
  names : string array;
      (* Parallel to [slots]: the param name at each slot. [Param.bind] / [get]
         resolve a handle to its slot by name, so resolution is per-env (hence
         per-codec) rather than via a mutable field on the shared handle. *)
  slots : int array;
  bound : bool array;
      (* Parallel to [slots]: one bit per slot, set by [Param.bind] so
         encoders can reject envs that left an input param at its
         default-zero value. *)
}

(* Expression constructors *)
let int n = Int n
let true_ = Bool true
let false_ = Bool false
let ref name = Ref (I, name)
let sizeof t = Sizeof t
let sizeof_this = Sizeof_this
let field_pos = Field_pos

module Expr = struct
  let int64 n = Int64 n
  let ( + ) a b = Add (a, b)
  let ( - ) a b = Sub (a, b)
  let ( * ) a b = Mul (a, b)
  let ( / ) a b = Div (a, b)
  let ( mod ) a b = Mod (a, b)
  let ( land ) a b = Land (a, b)
  let ( lor ) a b = Lor (a, b)
  let ( lxor ) a b = Lxor (a, b)
  let lnot a = Lnot a
  let ( lsl ) a b = Lsl (a, b)
  let ( lsr ) a b = Lsr (a, b)
  let ( = ) a b = Eq (a, b)
  let ( <> ) a b = Ne (a, b)
  let ( < ) a b = Lt (a, b)
  let ( <= ) a b = Le (a, b)
  let ( > ) a b = Gt (a, b)
  let ( >= ) a b = Ge (a, b)
  let ( && ) a b = And (a, b)
  let ( || ) a b = Or (a, b)
  let not a = Not a
  let if_then_else c t e = If_then_else (c, t, e)
  let to_uint8 e = Cast (`U8, e)
  let to_uint16 e = Cast (`U16, e)
  let to_uint32 e = Cast (`U32, e)
  let to_uint64 e = Cast (`U64, e)
end

(* Type constructors *)
let uint8 = Uint8
let uint16 = Uint16 Little
let uint16be = Uint16 Big
let uint32 = Uint32 Little
let uint32be = Uint32 Big
let uint63 = Uint63 Little
let uint63be = Uint63 Big
let uint64 = Uint64 Little
let uint64be = Uint64 Big
let int8 = Int8
let int16 = Int16 Little
let int16be = Int16 Big
let int32 = Int32 Little
let int32be = Int32 Big
let (int64 : int64 typ) = Int64 Little
let (int64be : int64 typ) = Int64 Big
let float32 = Float32 Little
let float32be = Float32 Big
let float64 = Float64 Little
let float64be = Float64 Big

let uint ?(endian = Big) size =
  (match size with
  | Int n when n < 1 || n > 7 ->
      Fmt.invalid_arg "uint: size must be 1-7, got %d" n
  | _ -> ());
  Uint_var { size; endian }

(* Bitfield bases *)
let bf_uint8 = U8
let bf_uint16 = U16 Little
let bf_uint16be = U16 Big
let bf_uint32 = U32 Little
let bf_uint32be = U32 Big

let bitfield_base_bits : bitfield_base -> int = function
  | U8 -> 8
  | U16 _ -> 16
  | U32 _ -> 32

let bits ?(bit_order = Msb_first) ~width base =
  (* A bitfield must fit its base word: a [width] above the base size, or below
     1, has no faithful wire meaning and the OCaml shift and the 3D [base F :
     width] field would read different values. Reject it at construction. *)
  let total = bitfield_base_bits base in
  if width < 1 || width > total then
    Fmt.invalid_arg
      "Wire.bits: width %d does not fit a %d-bit base (must be 1..%d)" width
      total total;
  Bits { width; base; bit_order }

let bit b = Bool.to_int b
let is_set n = n <> 0

(* Field decorations [Optional], [Optional_or], [Repeat] only have a 3D
   projection when they appear at the top of a struct field's type --
   anywhere else (array elem, casetype case body, [where]/[map]/[apply]
   wrapper) there is no 3D shape that captures the semantics. Reject the
   construction at the user's call site so the error points at the
   wrapping combinator rather than surfacing later inside [pp_typ]. *)
let rec is_field_decoration : type a. a typ -> bool = function
  | Optional _ | Optional_or _ | Repeat _ -> true
  | Map { inner; _ } -> is_field_decoration inner
  | Where { inner; _ } -> is_field_decoration inner
  | Apply { typ; _ } -> is_field_decoration typ
  | _ -> false

let reject_decoration ~combinator t =
  if is_field_decoration t then
    Fmt.invalid_arg
      "Wire.%s: [optional]/[optional_or]/[repeat] are field decorations and \
       cannot appear nested inside [%s] -- attach them as the field type \
       directly via [Field.v]."
      combinator combinator

let map decode encode inner =
  reject_decoration ~combinator:"map" inner;
  Map { inner; decode; encode; index_bound = None }

let bool inner =
  Map { inner; decode = is_set; encode = bit; index_bound = None }

(* Parse errors -- moved here so combinators like [cases] can raise them
   directly rather than through intermediate exceptions. *)

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }

exception Parse_error of parse_error

(* Top-level so [variants_lookup] does not allocate a closure per call.
   Returns -1 if [v] is not in [arr] (used as a non-allocating sentinel
   instead of [int option]). Shared with [variants] below. *)
let rec variants_lookup arr v i =
  if i >= Array.length arr then -1
  else if arr.(i) = v then i
  else variants_lookup arr v (i + 1)

let cases variants inner =
  let arr = Array.of_list variants in
  let len = Array.length arr in
  let decode n =
    if n >= 0 && n < len then arr.(n) else raise (Parse_error (Invalid_tag n))
  in
  let encode v =
    let i = variants_lookup arr v 0 in
    if i < 0 then invalid_arg "Wire.lookup: unknown variant" else i
  in
  Map { inner; decode; encode; index_bound = Some len }

let unit = Unit
let all_bytes = All_bytes
let all_zeros = All_zeros
let zeroterm = Zeroterm
let zeroterm_at_most ~size = Zeroterm_at_most { size }

let where cond inner =
  reject_decoration ~combinator:"where" inner;
  Where { cond; inner }

let seq_list : ('a, 'a list) seq_map =
  Seq_map
    {
      empty = [];
      add = (fun acc x -> x :: acc);
      finish = List.rev;
      iter = List.iter;
    }

(* The 3D projection of [array]/[optional]/[optional_or]/[repeat] turns
   their length / predicate / byte budget into a [byte-size] suffix.
   That works as long as the wrapped element exposes a wire size we can
   plumb into the suffix expression -- either a fixed scalar width, an
   already-sized payload like [byte_array {size}], or a codec with a
   fixed [wire_size]. Reject everything else at the smart constructor
   so the error fires at the user's call site. *)
let rec has_wire_size_expr : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ -> true
  | Int8 | Int16 _ | Int32 _ | Int64 _ -> true
  | Float32 _ | Float64 _ -> true
  | Bits _ -> true
  | Unit -> true
  | Uint_var _ -> true
  | Byte_array _ | Byte_slice _ | Byte_array_where _ -> true
  | Single_elem _ -> true
  | Map { inner; _ } -> has_wire_size_expr inner
  | Enum { base; _ } -> has_wire_size_expr base
  | Where { inner; _ } -> has_wire_size_expr inner
  | Apply { typ; _ } -> has_wire_size_expr typ
  | Array { elem; _ } -> has_wire_size_expr elem
  | Codec { codec_fixed_size; _ } -> codec_fixed_size <> None
  | _ -> false

(* EverParse's byte-budget array ([T_nlist], the [array]/[repeat] lowering)
   requires its element parser to consume a positive minimum number of bytes:
   the [nz] index of [parser_kind nz wk]. A byte-budget array, a [nested] region
   ([T_exact]), and the all-bytes / all-zeros tails all have kind
   [parser_kind false _] -- their parser may consume zero bytes -- regardless of
   a positive constant budget, so none of them is [nz]. A struct/codec is [nz]
   iff some field is (sizes add, so one positive-minimum field anchors the rest);
   a casetype is [nz] iff its tag is or every case body is (it parses [tag] then
   a switch, and the tag alone anchors it). [map]/[where]/[enum] are transparent.
   This is what makes a named composite usable as an element: a [T_nlist] over a
   non-[nz] element fails EverParse extraction. *)
let rec nz : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ -> true
  | Int8 | Int16 _ | Int32 _ | Int64 _ -> true
  | Float32 _ | Float64 _ -> true
  | Bits _ -> true
  | Zeroterm -> true
  | Uint_var _ -> false (* lowers to a UINT8 byte-budget array *)
  | Zeroterm_at_most _ -> false (* T_at_most kind: parser_kind false _ *)
  | Byte_array _ | Byte_array_where _ | Byte_slice _ -> false
  | Unit -> false
  | All_bytes | All_zeros -> false
  | Array _ | Repeat _ -> false (* nlist kind *)
  | Single_elem _ -> false (* T_exact kind *)
  | Optional _ | Optional_or _ -> false
  | Where { inner; _ } -> nz inner
  | Map { inner; _ } -> nz inner
  | Enum { base; _ } -> nz base
  | Apply { typ; _ } -> nz typ
  | Codec { codec_struct; _ } -> struct_nz codec_struct
  | Struct s -> struct_nz s
  | Casetype { tag; cases; _ } ->
      nz tag
      || List.for_all (fun (Case_branch { cb_inner; _ }) -> nz cb_inner) cases
  | Type_ref _ | Qualified_ref _ -> false (* opaque ref: be conservative *)

and struct_nz (s : struct_) =
  List.exists (fun (Field f) -> nz f.field_typ) s.fields

(* An [array]/[array_seq] element must be fixed-width AND decodable one element
   at a time by the array loop: a scalar, a fixed-size byte span, or a
   fixed-size sub-codec. [Map]/[Where]/[Enum] are transparent wrappers, so look
   through them. A [nested] region, a refined byte span ([byte_array_where]), a
   nested [array], or a variable / self-delimiting inner has no array
   projection (3D rejects it) and the decoder has no element reader, so they are
   refused at construction. A fixed-size sub-codec must additionally be [nz]:
   EverParse projects the array as a byte-budget list of the codec's named
   struct, and a list over a possibly-empty element does not extract. This is
   stricter than [has_wire_size_expr], which admits sized-but-undecodable
   elements for the [optional] byte-size suffix. *)
let rec is_array_element : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ -> true
  | Int8 | Int16 _ | Int32 _ | Int64 _ -> true
  | Float32 _ | Float64 _ -> true
  (* [Unit] is 0-width: an array of it carries no bytes and projects to a
     zero-size 3D array EverParse rejects, so it is not a valid element. *)
  | Uint_var { size = Int _; _ } -> true
  | Byte_array { size = Int _ } | Byte_slice { size = Int _ } -> true
  | Codec { codec_fixed_size; codec_struct; _ } ->
      codec_fixed_size <> None && struct_nz codec_struct
  | Map { inner; _ } -> is_array_element inner
  | Where { inner; _ } -> is_array_element inner
  | Enum { base; _ } -> is_array_element base
  | _ -> false

let reject_non_array_element ~combinator t =
  if not (is_array_element t) then
    Fmt.invalid_arg
      "Wire.%s: element type is not a fixed-width array element -- 3D projects \
       %s as a count of fixed-size elements (a scalar, a fixed byte span, or a \
       fixed-size sub-codec with at least one fixed-size field); a nested \
       region, refined span, variable inner, or a sub-codec made only of \
       byte-span fields has no array projection."
      combinator combinator

(* A self-delimiting inner carries its own length / structure, so it decodes a
   known span without an external size. These project as a gate-dispatched
   casetype (present case = the inner, absent case = a 0-byte field), unlike the
   byte-size suffix used for [has_wire_size_expr] inners. *)
let rec is_self_delimiting : type a. a typ -> bool = function
  | Codec _ -> true
  | Casetype _ -> true
  | Map { inner; _ } -> is_self_delimiting inner
  | Where { inner; _ } -> is_self_delimiting inner
  | Apply { typ; _ } -> is_self_delimiting typ
  | _ -> false

(* [optional]/[optional_or] accept any inner that projects to 3D: either a
   byte-sized inner (conditional [byte-size] region) or a self-delimiting one
   (gate-dispatched casetype). Everything else has no clean projection. *)
let reject_unprojectable_optional ~combinator t =
  if not (has_wire_size_expr t || is_self_delimiting t) then
    Fmt.invalid_arg
      "Wire.%s: inner type does not project to 3D -- it is neither byte-sized \
       nor self-delimiting."
      combinator combinator

(* A bitfield is packed within an enclosing base word in a record; it has no
   standalone wire form, so it cannot be an [array] / [repeat] element (there is
   no byte boundary per element, and no 3D element type to project to). Reject
   it at construction rather than crash at decode. *)
let rec is_bitfield_element : type a. a typ -> bool = function
  | Bits _ -> true
  | Map { inner; _ } -> is_bitfield_element inner
  | Where { inner; _ } -> is_bitfield_element inner
  | Enum { base; _ } -> is_bitfield_element base
  | _ -> false

let reject_bitfield_element ~combinator t =
  if is_bitfield_element t then
    Fmt.invalid_arg
      "Wire.%s: a bitfield has no standalone element form (bits are packed \
       within an enclosing word); it cannot be an element of %s."
      combinator combinator

(* A bare greedy field ([all_bytes] / [all_zeros]) reads "the rest of the
   buffer". It has no 3D type of its own, so it only projects as the trailing
   field of a struct or sub-codec, never as a casetype case body (the switch
   case needs a determinate type) and never with a determinate element size. *)
let rec is_greedy : type a. a typ -> bool = function
  | All_bytes | All_zeros -> true
  | Map { inner; _ } -> is_greedy inner
  | Where { inner; _ } -> is_greedy inner
  | Enum { base; _ } -> is_greedy base
  | _ -> false

let reject_greedy_case_body t =
  if is_greedy t then
    invalid_arg
      "Wire.casetype: a case body cannot be a bare all_bytes / all_zeros \
       greedy field; it has no determinate type. Wrap it in a sub-codec."

let array ~len elem =
  reject_decoration ~combinator:"array" elem;
  reject_bitfield_element ~combinator:"array" elem;
  reject_non_array_element ~combinator:"array" elem;
  Array { len; elem; seq = seq_list }

let array_seq seq ~len elem =
  reject_decoration ~combinator:"array_seq" elem;
  reject_bitfield_element ~combinator:"array_seq" elem;
  reject_non_array_element ~combinator:"array_seq" elem;
  Array { len; elem; seq }

let byte_array ~size = Byte_array { size }
let byte_array_where_counter = Stdlib.ref 0
let elt_var_prefix = "__elt_"

let byte_array_where ~size ~per_byte =
  let n = !byte_array_where_counter in
  Stdlib.incr byte_array_where_counter;
  let elt_var = elt_var_prefix ^ string_of_int n in
  let cond = per_byte (Ref (I, elt_var)) in
  Byte_array_where { size; elt_var; cond }

(* The 3D synthesized typedef name derived from an elt_var. The element
   field inside the synthesized struct keeps the elt_var as its name so the
   captured [cond] (which references [elt_var] via [Ref]) renders as a
   constraint on that field directly. *)
let synth_name_of_elt_var ev =
  let plen = String.length elt_var_prefix in
  if String.length ev > plen && String.sub ev 0 plen = elt_var_prefix then
    "_RefByte_" ^ String.sub ev plen (String.length ev - plen)
  else "_RefByte_" ^ ev

let byte_slice ~size = Byte_slice { size }

let optional present inner =
  reject_bitfield_element ~combinator:"optional" inner;
  reject_unprojectable_optional ~combinator:"optional" inner;
  Optional { present; inner }

let optional_or present ~default inner =
  reject_bitfield_element ~combinator:"optional_or" inner;
  reject_unprojectable_optional ~combinator:"optional_or" inner;
  Optional_or { present; inner; default }

(* [repeat ~size elem] is more permissive than [array]: 3D's
   [<elem>[:byte-size <size>]] parses elements until the byte budget is
   exhausted, so variable-size elements are fine as long as each one is
   self-bounded by its 3D type (struct, codec, ...). The only thing we
   refuse is a field decoration nested inside [elem]. *)
let repeat ~size elem =
  reject_decoration ~combinator:"repeat" elem;
  reject_bitfield_element ~combinator:"repeat" elem;
  Repeat { size; elem; seq = seq_list }

let repeat_seq seq ~size elem =
  reject_decoration ~combinator:"repeat_seq" elem;
  reject_bitfield_element ~combinator:"repeat_seq" elem;
  Repeat { size; elem; seq }

let nested ~size elem =
  reject_bitfield_element ~combinator:"nested" elem;
  Single_elem { size; elem; at_most = false }

let nested_at_most ~size elem =
  reject_bitfield_element ~combinator:"nested_at_most" elem;
  Single_elem { size; elem; at_most = true }

let enum name cases base = Enum { name; cases; base; closed = true }
let enum_open name cases base = Enum { name; cases; base; closed = false }

let fail_parse_error fmt =
  Fmt.kstr (fun s -> raise (Parse_error (Constraint_failed s))) fmt

let variants name cases base =
  let enum_cases = List.mapi (fun i (s, _) -> (s, i)) cases in
  let arr = Array.of_list (List.map snd cases) in
  let len = Array.length arr in
  let decode n =
    if n >= 0 && n < len then arr.(n)
    else fail_parse_error "Wire.variants %s: unknown value %d" name n
  in
  let encode v =
    let i = variants_lookup arr v 0 in
    if i < 0 then Fmt.invalid_arg "Wire.variants %s: unknown variant" name
    else i
  in
  map decode encode (enum name enum_cases base)

(* Casetype *)
type ('a, 'k) case_def =
  | Case_def : {
      cd_index : 'k option;
      cd_inner : 'w typ;
      cd_inject : 'w -> 'a;
      cd_project : 'a -> 'w option;
    }
      -> ('a, 'k) case_def
  | Default_def : {
      dd_inner : 'w typ;
      dd_inject : 'k -> 'w -> 'a;
      dd_project : 'a -> ('k * 'w) option;
    }
      -> ('a, 'k) case_def

let case ?index inner ~inject ~project =
  Case_def
    {
      cd_index = index;
      cd_inner = inner;
      cd_inject = inject;
      cd_project = project;
    }

let default inner ~inject ~project =
  Default_def { dd_inner = inner; dd_inject = inject; dd_project = project }

(* An enum over a big-endian multi-byte base cannot be a 3D [enum] type:
   EverParse types the integer constants as the native (little-endian) width and
   rejects the declaration ("Expected UINT16BE, got UINT16"). Such an enum is
   projected as its base scalar with a membership refinement (closed) or bare
   base (open) instead, the same shape used outside the doc projection. *)
let enum_base_is_be : type a. a typ -> bool = function
  | Uint16 Big | Uint32 Big | Uint63 Big | Uint64 Big -> true
  | Int16 Big | Int32 Big | Int64 Big -> true
  | Uint_var { endian = Big; _ } -> true
  | _ -> false

(* A casetype tag must project to a 3D type the generated [switch] dispatches on.
   A [uint ~size] (Uint_var) tag renders as [UINTBE(n)], which is not a 3D type;
   an enum over a big-endian base has no 3D enum type to name in the case labels.
   Neither projects, so reject it at construction (seen through the transparent
   [Map] / [Where] wrappers a [variants] tag carries). *)
let rec reject_unprojectable_casetype_tag : type k. k typ -> unit = function
  | Uint_var _ ->
      invalid_arg
        "Wire.casetype: a [uint ~size] tag has no 3D projection; use a \
         fixed-width integer, bitfield, or enum tag."
  | Enum { base; _ } when enum_base_is_be base ->
      invalid_arg
        "Wire.casetype: an enum over a big-endian base has no 3D casetype-tag \
         projection; use a little-endian or 1-byte enum base."
  | Map { inner; _ } -> reject_unprojectable_casetype_tag inner
  | Where { inner; _ } -> reject_unprojectable_casetype_tag inner
  | _ -> ()

let casetype name tag defs =
  reject_unprojectable_casetype_tag tag;
  let resolve = function
    | Case_def { cd_index; cd_inner; cd_inject; cd_project } ->
        reject_decoration ~combinator:"casetype" cd_inner;
        reject_greedy_case_body cd_inner;
        let tag_val =
          match cd_index with
          | Some k -> k
          | None ->
              invalid_arg
                "Wire.casetype: every case must supply an explicit [~index]"
        in
        Case_branch
          {
            cb_tag = Some tag_val;
            (* [case] knows its tag from [~index], so its [inject] takes only the
               body and its [project] yields only the body; pair the fixed tag
               back on for the unified tag-aware branch. *)
            cb_inner = cd_inner;
            cb_inject = (fun _matched body -> cd_inject body);
            cb_project =
              (fun a -> Option.map (fun w -> (tag_val, w)) (cd_project a));
          }
    | Default_def { dd_inner; dd_inject; dd_project } ->
        reject_decoration ~combinator:"casetype" dd_inner;
        reject_greedy_case_body dd_inner;
        Case_branch
          {
            cb_tag = None;
            cb_inner = dd_inner;
            cb_inject = dd_inject;
            cb_project = dd_project;
          }
  in
  Casetype { name; tag; cases = List.map resolve defs }

(* Struct fields *)
let field name ?constraint_ ?action ?doc typ =
  Field
    {
      field_name = Some name;
      field_typ = typ;
      constraint_;
      action;
      field_doc = doc;
    }

let anon_field typ =
  Field
    {
      field_name = None;
      field_typ = typ;
      constraint_ = None;
      action = None;
      field_doc = None;
    }

(* Struct constructors *)
let struct_ name fields = { name; params = []; where = None; fields }
let struct_name (s : struct_) = s.name
let struct_typ s = Struct s
let field_names s = List.filter_map (fun (Field f) -> f.field_name) s.fields

let struct_project s ~name ~keep =
  let keep_names = List.filter_map (fun (Field f) -> f.field_name) keep in
  let fields =
    List.map
      (fun (Field f) ->
        match f.field_name with
        | Some n when List.mem n keep_names -> Field f
        | _ ->
            Field
              { f with field_name = None; constraint_ = None; action = None })
      s.fields
  in
  { s with name; fields }

(* What kind of OCaml value a field produces -- used by Wire_stubs to
   generate the right C-to-OCaml conversion in output stubs. *)
type ocaml_kind = Int | Int64 | Float32 | Float64 | Bool | String | Unit

let rec ocaml_kind_of : type a. a typ -> ocaml_kind = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint_var _ -> Int
  | Uint64 _ -> Int64
  | Int8 | Int16 _ | Int32 _ -> Int
  | Int64 _ -> Int64
  | Float32 _ -> Float32
  | Float64 _ -> Float64
  | Bits _ -> Int
  | Map { inner = Bits _; decode = _; encode = _; _ } ->
      (* bool (bits ~width:1 ...) maps to bool; other maps stay int *)
      (* We can't distinguish bool from other maps here without checking
         the decode function. Use Int as safe default -- the EverParse
         output struct stores the raw int anyway. *)
      Int
  | Map { inner; _ } -> ocaml_kind_of inner
  | Enum { base; _ } -> ocaml_kind_of base
  | Where { inner; _ } -> ocaml_kind_of inner
  | Byte_array _ -> String
  | Byte_array_where _ -> String
  | Byte_slice _ -> String (* approximate: slice becomes string in output *)
  | Zeroterm | Zeroterm_at_most _ -> String
  | Unit | All_bytes | All_zeros -> Unit
  | _ -> Int (* fallback *)

let field_kinds s =
  List.filter_map
    (fun (Field f) ->
      match f.field_name with
      | Some name -> Some (name, ocaml_kind_of f.field_typ)
      | None -> None)
    s.fields

(* Parameters *)
let param name typ =
  { param_name = name; param_typ = Pack_typ typ; mutable_ = false }

let mutable_param name typ =
  { param_name = name; param_typ = Pack_typ typ; mutable_ = true }

let param_struct name params ?where fields = { name; params; where; fields }

let apply typ args =
  reject_decoration ~combinator:"apply" typ;
  Apply { typ; args = List.map (fun e -> Pack_expr e) args }

(* Type references *)
let type_ref name = Type_ref name
let qualified_ref module_ name = Qualified_ref { module_; name }

(* Actions *)
let on_success stmts = Success stmts
let on_act stmts = Act stmts
let assign (p : ('a, param_output) param_handle) e = Assign (p, e)
let return_bool e = Return e
let abort = Abort
let action_if cond then_ else_ = If (cond, then_, else_)
let var name e = Var (name, e)

(* Declarations *)
type decl =
  | Typedef of {
      entrypoint : bool;
      export : bool;
      output : bool;
      extern_ : bool;
      doc : string option;
      struct_ : struct_;
    }
  | Define of { name : string; value : int }
  | Extern_fn of { name : string; params : param list; ret : packed_typ }
  | Extern_probe of { init : bool; name : string }
  | Enum_decl of {
      name : string;
      cases : (string * int) list;
      base : packed_typ;
    }
  | Casetype_decl of {
      name : string;
      params : param list;
      tag : packed_typ;
      cases : (packed_expr option * packed_typ) list;
    }

let typedef ?(entrypoint = false) ?(export = false) ?(output = false)
    ?(extern_ = false) ?doc struct_ =
  Typedef { entrypoint; export; output; extern_; doc; struct_ }

let define name value = Define { name; value }
let extern_fn name params ret = Extern_fn { name; params; ret = Pack_typ ret }
let extern_probe ?(init = false) name = Extern_probe { init; name }
let enum_decl name cases base = Enum_decl { name; cases; base = Pack_typ base }

type decl_case = packed_expr option * packed_typ

let decl_case tag typ = (Some (Pack_expr (Int tag)), Pack_typ typ)
let decl_default typ = (None, Pack_typ typ)

let casetype_decl name params tag cases =
  Casetype_decl { name; params; tag = Pack_typ tag; cases }

(* Module *)
type module_ = { doc : string option; decls : decl list }

(* Extract enum declarations needed by struct fields. Scans field types for
   Enum constructors (including under Map/Where wrappers) and returns the
   corresponding enum_decl entries, deduplicated by name. Enums over bitfield
   bases are skipped: they map to plain bitfields in 3D (the enum/variant
   mapping is OCaml-only). An enum over a big-endian base is skipped too (no 3D
   enum type for it; the field carries a membership refinement instead). *)
let enum_decls (s : struct_) : decl list =
  let seen = Hashtbl.create 4 in
  let decls = Stdlib.ref [] in
  let is_bits : type a. a typ -> bool = function
    | Bits _ -> true
    | _ -> false
  in
  List.iter
    (fun (Field f) ->
      let rec extract : type a. a typ -> unit = function
        | Enum { name; cases; base; _ }
          when (not (Hashtbl.mem seen name))
               && (not (is_bits base))
               && not (enum_base_is_be base) ->
            (* Both open and closed enums declare a 3D enum type so the named
               codes survive in the generated .3d. A closed enum additionally
               constrains its field with a membership refinement; an open enum
               leaves the field as its base scalar (any value accepted) while
               still documenting the known codes through the declaration. *)
            Hashtbl.add seen name ();
            decls := Enum_decl { name; cases; base = Pack_typ base } :: !decls
        | Map { inner; _ } -> extract inner
        | Where { inner; _ } -> extract inner
        (* An enum can sit inside a container (an array or repeat element, an
           optional, a sized region); its declaration is still needed, or the
           schema references an undeclared type. *)
        | Array { elem; _ } -> extract elem
        | Repeat { elem; _ } -> extract elem
        | Optional { inner; _ } -> extract inner
        | Optional_or { inner; _ } -> extract inner
        | Single_elem { elem; _ } -> extract elem
        | _ -> ()
      in
      extract f.field_typ)
    s.fields;
  List.rev !decls

(* A closed enum carries a bounded value set the OCaml decoder enforces on every
   decoded value. As a byte-budget list element (an [array] / [repeat] element)
   or an [optional] inner it would otherwise render as the bare base, dropping
   the membership the generated C validator must also enforce -- so [Codec.decode]
   would reject an out-of-set code that the generated C accepts. Such an element
   is projected through a synthesised single-field struct whose field carries the
   membership refinement. [closed_enum_elem] returns the enum name, its base, and
   its case values, seen through the transparent [Map] / [Where] / [Apply]
   wrappers; [None] for an open enum or a non-enum, which keep their rendering. *)
let rec closed_enum_elem : type a.
    a typ -> (string * packed_typ * int list) option = function
  | Enum { name; base; cases; closed = true } ->
      Some (name, Pack_typ base, List.map snd cases)
  | Map { inner; _ } -> closed_enum_elem inner
  | Where { inner; _ } -> closed_enum_elem inner
  | Apply { typ; _ } -> closed_enum_elem typ
  | _ -> None

(* A closed-enum array element renders as a named 1-byte enum type only when its
   base is a single byte; a wider or big-endian base loses that rendering under a
   byte budget and goes through the refined-element struct instead. (A [repeat]
   element loses it at every width, so the [repeat] path does not gate on this.) *)
let closed_enum_elem_wide : type a. a typ -> bool =
 fun elem ->
  match closed_enum_elem elem with
  | Some (_, Pack_typ Uint8, _) | Some (_, Pack_typ Int8, _) -> false
  | Some _ -> true
  | None -> false

let enum_elem_struct_name name = "_EnumElt_" ^ name

(* The membership refinement [v == c0 || v == c1 || ...] over an element
   variable. [None] for an empty case set (a degenerate enum admitting no
   value), which carries no refinement. *)
let enum_membership_cond var = function
  | [] -> None
  | v0 :: rest ->
      Some
        (List.fold_left
           (fun acc v -> Or (acc, Eq (Ref (I, var), Int v)))
           (Eq (Ref (I, var), Int v0))
           rest)

(* The synthesised refined-element struct typedef for a closed enum used as a
   list element / optional inner, or [None] when there is no membership to
   enforce. The field is typed as the enum's base scalar (not the named enum
   type) and carries the membership refinement, so it projects without the 3D
   enum declaration and works for wide and big-endian bases alike. *)
let enum_elem_struct_decl name (Pack_typ base) cases =
  match enum_membership_cond "v" cases with
  | None -> None
  | Some cond ->
      Some
        (typedef
           (struct_
              (enum_elem_struct_name name)
              [ field "v" ~constraint_:cond base ]))

(* Emit the synthesised refined-element struct for a closed-enum list element /
   optional inner, deduped by name, mirroring where [field_suffix] /
   [optional_suffix] reference it. *)
let emit_enum_elem_struct seen acc elem =
  match closed_enum_elem elem with
  | Some (name, base, (_ :: _ as cases)) ->
      let sname = enum_elem_struct_name name in
      if not (Hashtbl.mem seen sname) then begin
        Hashtbl.add seen sname ();
        Option.iter
          (fun d -> acc := d :: !acc)
          (enum_elem_struct_decl name base cases)
      end
  | _ -> ()

(* The base printer for a byte-budget list element ([array] / [repeat]): a closed
   enum routes through its synthesised refined-element struct, so membership is
   enforced per element as the OCaml decoder does; anything else uses [fallback]
   (its bare base / wrapper rendering). *)
let list_elem_pp : type a.
    a typ -> fallback:(Format.formatter -> unit) -> Format.formatter -> unit =
 fun elem ~fallback ->
  match closed_enum_elem elem with
  | Some (name, _, _ :: _) ->
      fun ppf -> Fmt.string ppf (enum_elem_struct_name name)
  | _ -> fallback

(* True for tag types that the 3D side can dispatch on natively: integer-
   shaped scalars plus enums. String/byte tags use the two-step shape
   (split into adjacent fields, dispatch in caller code) instead. *)
let is_int_dispatch_typ : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint_var _ | Int8 | Int16 _
  | Int32 _ | Bits _ | Enum _ ->
      true
  | _ -> false

(* Project a case-branch discriminator value of type ['k] to a 3D constant
   expression. Only called for int-shaped tags; non-int tags don't reach
   here because their casetype field is rewritten away before
   [casetype_decls_of_struct] walks it. *)
let case_index_to_expr : type k. k typ -> k -> packed_expr =
 fun tag_typ k ->
  match tag_typ with
  | Uint8 -> Pack_expr (Int k)
  | Uint16 _ -> Pack_expr (Int k)
  | Uint32 _ -> Pack_expr (Int k)
  | Uint63 _ -> Pack_expr (Int k)
  | Uint_var _ -> Pack_expr (Int k)
  | Int8 -> Pack_expr (Int k)
  | Int16 _ -> Pack_expr (Int k)
  | Int32 _ -> Pack_expr (Int k)
  | Bits _ -> Pack_expr (Int k)
  | Enum _ -> Pack_expr (Int k)
  | _ -> assert false (* guarded by [is_int_dispatch_typ] *)

(* Auto-emit a dispatch + wrapper for each [Casetype] used in a struct.

   [Wire.casetype "Foo" tag cases] parses the tag inline in OCaml; the 3D
   side has no equivalent (its [casetype_decl] takes the tag as a
   parameter, leaving the caller to supply it). To project cleanly we
   synthesise two declarations per unique casetype name:

   - [Casetype_decl "Foo_Body"] -- the dispatch table, parameterised on
     the tag value.
   - [Typedef "Foo"] -- a small wrapper struct holding [tag; body] where
     [body] is [Foo_Body(tag)].

   The user's [Casetype] typ then prints as the wrapper's name (already
   the existing [pp_typ] behaviour) and references the wrapper. *)
let decl_case_of_branch : type a k. k typ -> (a, k) case_branch -> decl_case =
 fun tag (Case_branch { cb_tag; cb_inner; _ }) ->
  let tag_expr =
    match cb_tag with None -> None | Some k -> Some (case_index_to_expr tag k)
  in
  (tag_expr, Pack_typ cb_inner)

let casetype_pair : type a k.
    string -> k typ -> (a, k) case_branch list -> decl * decl =
 fun name tag cases ->
  let body_name = name ^ "_Body" in
  let decl_cases = List.map (decl_case_of_branch tag) cases in
  let dispatch =
    Casetype_decl
      {
        name = body_name;
        params = [ param "tag" tag ];
        tag = Pack_typ tag;
        cases = decl_cases;
      }
  in
  let tag_field = field "tag" tag in
  let body_field =
    field "body"
      (Apply { typ = Type_ref body_name; args = [ Pack_expr (Ref (I, "tag")) ] })
  in
  let wrapper = typedef (struct_ name [ tag_field; body_field ]) in
  (dispatch, wrapper)

(* A variable-size, self-delimiting [repeat] element with no named 3D type
   (e.g. a NUL-terminated string) is wrapped in a one-field element struct so
   the byte-budget list references a bare name. Fixed-size byte elements use the
   flat-base form instead; named elements (sub-codecs, casetypes) already render
   as a name. Returns the wrapper struct name, or [None]. *)
let repeat_elem_struct : type a. a typ -> string option = function
  | Zeroterm -> Some "ZtElem"
  | _ -> None

(* A [nested ~size] projects to [<elem>[:byte-size-single-element-array n]],
   whose element must be a single type token. A bare scalar or an already-named
   struct ([codec] / [casetype]) works inline, but an element that renders with
   its own array / refinement qualifier ([byte_array], [byte_slice],
   [byte_array_where], [zeroterm_at_most], [uint_var]) or that needs its own
   declaration ([enum]) cannot. Those go through a synthesised wrapper struct,
   named deterministically from the element so the field renderer and the
   typedef emitter agree without shared state. *)
(* A deterministic, collision-free identifier fragment for an inner type, used
   to name its synthesised wrapper struct. Two structurally equal inners get the
   same fragment (so their wrapper is shared); different shapes differ. *)
let rec wrap_tag : type a. a typ -> string = function
  | Uint8 -> "u8"
  | Uint16 Little -> "u16l"
  | Uint16 Big -> "u16b"
  | Uint32 Little -> "u32l"
  | Uint32 Big -> "u32b"
  | Uint63 Little -> "u63l"
  | Uint63 Big -> "u63b"
  | Uint64 Little -> "u64l"
  | Uint64 Big -> "u64b"
  | Int8 -> "i8"
  | Int16 Little -> "i16l"
  | Int16 Big -> "i16b"
  | Int32 Little -> "i32l"
  | Int32 Big -> "i32b"
  | Int64 Little -> "i64l"
  | Int64 Big -> "i64b"
  | Float32 _ -> "f32"
  | Float64 _ -> "f64"
  | Uint_var { size = Int n; _ } -> Fmt.str "uv%d" n
  | Bits { width; _ } -> Fmt.str "bf%d" width
  | Byte_array { size = Int n } -> Fmt.str "ba%d" n
  | Byte_slice { size = Int n } -> Fmt.str "bs%d" n
  | Byte_array_where { size = Int n; _ } -> Fmt.str "rb%d" n
  | Zeroterm -> "zt"
  | Zeroterm_at_most { size = Int n } -> Fmt.str "ztam%d" n
  | Unit -> "unit"
  | Enum { name; _ } -> "e" ^ name
  | Map { inner; _ } -> wrap_tag inner
  | Where { inner; _ } -> wrap_tag inner
  | Array { len = Int n; elem; _ } -> Fmt.str "arr%d_%s" n (wrap_tag elem)
  | Single_elem { size = Int n; elem; _ } -> Fmt.str "se%d_%s" n (wrap_tag elem)
  | Optional { inner; _ } -> "opt_" ^ wrap_tag inner
  | Optional_or { inner; _ } -> "optd_" ^ wrap_tag inner
  | Repeat { elem; _ } -> "rep_" ^ wrap_tag elem
  | Codec { codec_name; _ } -> codec_name
  | Casetype { name; _ } -> name
  | _ -> "x"

(* The synthesised wrapper-struct name for a [nested] inner that has no valid
   bare single-element-array token. A scalar, sub-codec, or casetype renders
   inline ([None]); everything else (byte spans, arrays, optionals, nested
   regions, ...) is lifted into a named [struct { v : inner }]. *)
let some fmt = Fmt.kstr (fun s -> Some s) fmt

let rec single_elem_struct : type a. a typ -> string option = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ | Int8 | Int16 _ | Int32 _
  | Int64 _ | Float32 _ | Float64 _ | Codec _ | Casetype _ ->
      None
  | Map { inner; _ } -> single_elem_struct inner
  | Where { inner; _ } -> single_elem_struct inner
  (* Keep the historical names for the byte-span family so existing schemas and
     tests are unchanged; everything else gets a structural name. *)
  | Byte_array { size = Int n } -> some "SeBytes%d" n
  | Byte_slice { size = Int n } -> some "SeSlice%d" n
  | Byte_array_where { size = Int n; _ } -> some "SeRBytes%d" n
  | Uint_var { size = Int n; _ } -> some "SeUvar%d" n
  | Zeroterm_at_most { size = Int n } -> some "SeZtam%d" n
  | Enum { name; _ } -> Some ("Se_" ^ name)
  | other -> Some ("Se_" ^ wrap_tag other)

(* The synthesised gate-dispatch casetype name for an [optional] whose inner is
   self-delimiting (no wire-size expression). Derived from the inner so two
   optionals over the same inner share one declaration. *)
let optional_casetype_name : type a. a typ -> string =
 fun inner ->
  let rec base : type a. a typ -> string = function
    | Codec { codec_name; _ } -> codec_name
    | Casetype { name; _ } -> name
    | Map { inner; _ } -> base inner
    | Where { inner; _ } -> base inner
    | Apply { typ; _ } -> base typ
    | _ -> "Inner"
  in
  "Opt_" ^ base inner

(* Project a self-delimiting [optional] inner as a casetype dispatched on the
   gate: [case 0] parses a 0-byte field (the gate arg is [present ? 1 : 0], so 0
   means absent), the default case parses the inner. An empty case body is a 3D
   syntax error, hence the 0-byte field. *)
let optional_casetype_decl : type a. string -> a typ -> decl =
 fun name inner ->
  Casetype_decl
    {
      name;
      params = [ param "present" Uint8 ];
      tag = Pack_typ Uint8;
      cases =
        [
          (Some (Pack_expr (Int 0)), Pack_typ (Byte_array { size = Int 0 }));
          (None, Pack_typ inner);
        ];
    }

(* The refined-byte element a wrapper struct holds, if any, seen through the
   transparent [map] / [where] wrappers. Its synthesised [_RefByte_*] typedef
   must be emitted before the wrapper that references it, or the wrapper names an
   undeclared type (a [byte_array_where] inside a [nested] region). *)
let rec wrapper_refined_byte : type a. a typ -> (string * bool expr) option =
  function
  | Byte_array_where { elt_var; cond; _ } -> Some (elt_var, cond)
  | Map { inner; _ } -> wrapper_refined_byte inner
  | Where { inner; _ } -> wrapper_refined_byte inner
  | _ -> None

let rec collect_casetype_decls : type a.
    (string, unit) Hashtbl.t -> decl list Stdlib.ref -> a typ -> unit =
 fun seen acc typ ->
  let extract : type b. b typ -> unit =
   fun t -> collect_casetype_decls seen acc t
  in
  (* A fixed-size element used inside [repeat] / [nested] needs a named wrapper
     struct when it has no usable bare token; emit it once, deduped by name. A
     refined-byte element inside the wrapper needs its own typedef emitted first. *)
  let emit_wrapper name elem =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      (match wrapper_refined_byte elem with
      | Some (elt_var, cond) ->
          let synth = synth_name_of_elt_var elt_var in
          if not (Hashtbl.mem seen synth) then begin
            Hashtbl.add seen synth ();
            acc :=
              typedef (struct_ synth [ field elt_var ~constraint_:cond uint8 ])
              :: !acc
          end
      | None -> ());
      acc := typedef (struct_ name [ field "v" elem ]) :: !acc
    end
  in
  match typ with
  | Casetype { name; tag; cases }
    when is_int_dispatch_typ tag && not (Hashtbl.mem seen name) ->
      Hashtbl.add seen name ();
      (* Visit case inners first so any sub-codecs / nested casetypes they
         reference are declared before the dispatch that names them. *)
      List.iter (fun (Case_branch { cb_inner; _ }) -> extract cb_inner) cases;
      let dispatch, wrapper = casetype_pair name tag cases in
      acc := wrapper :: dispatch :: !acc
  | Casetype { cases; _ } ->
      (* String-tagged casetype: handled by [split_string_casetype_fields].
         Still walk inner case typs for nested int-tagged casetypes. *)
      List.iter (fun (Case_branch { cb_inner; _ }) -> extract cb_inner) cases
  | Codec { codec_name; codec_struct; _ } when not (Hashtbl.mem seen codec_name)
    ->
      (* Embedded sub-codec: emit its struct alongside the parent so 3D
         references to [codec_name] resolve. Recurse into the sub-codec's
         own fields to catch nested casetype / sub-codec dependencies. *)
      Hashtbl.add seen codec_name ();
      acc := typedef codec_struct :: !acc;
      List.iter (fun (Field f) -> extract f.field_typ) codec_struct.fields
  | Codec _ -> ()
  | Map { inner; _ } -> extract inner
  | Where { inner; _ } -> extract inner
  | Optional { inner; _ } when not (has_wire_size_expr inner) ->
      (* Self-delimiting inner: declare its dependencies, then the
         gate-dispatch casetype that references them. *)
      extract inner;
      let opt_name = optional_casetype_name inner in
      if not (Hashtbl.mem seen opt_name) then begin
        Hashtbl.add seen opt_name ();
        acc := optional_casetype_decl opt_name inner :: !acc
      end
  | Optional { inner; _ } ->
      extract inner;
      emit_enum_elem_struct seen acc inner
  | Optional_or { inner; _ } -> extract inner
  | Apply { typ; _ } -> extract typ
  | Repeat { elem; _ } ->
      extract elem;
      emit_enum_elem_struct seen acc elem;
      Option.iter (fun n -> emit_wrapper n elem) (repeat_elem_struct elem)
  | Array { elem; _ } ->
      extract elem;
      (* A 1-byte little-endian closed-enum array element renders as the named
         enum type (declared by [enum_decls]); a wider or big-endian one renders
         through the synthesised refined-element struct, declared here. *)
      if closed_enum_elem_wide elem then emit_enum_elem_struct seen acc elem
  | Single_elem { elem; _ } ->
      extract elem;
      Option.iter (fun n -> emit_wrapper n elem) (single_elem_struct elem)
  | _ -> ()

let casetype_decls_of_struct (s : struct_) : decl list =
  let seen = Hashtbl.create 4 in
  let acc = Stdlib.ref [] in
  List.iter
    (fun (Field f) -> collect_casetype_decls seen acc f.field_typ)
    s.fields;
  List.rev !acc

(* Two-step projection for string-tagged casetype: split the field into
   two adjacent fields, the tag bytes and the body bytes.

   The 3D side validates wire framing only; case dispatch is the caller's
   job (OCaml [parse_casetype] already does this on its side, and C
   consumers receive both spans through the plug and dispatch with their
   own [strcmp] table -- exactly what real protocol implementations like
   OpenSSH do). No 3D-side extern, no scratch slot, no runtime helper.

   The body field uses [All_bytes] ("rest of buffer"), so the casetype
   must be the trailing field of its parent struct -- matching the
   existing codec.ml constraint for variable-size casetype fields. *)
let split_string_casetype_fields (s : struct_) : struct_ =
  let split (Field f) =
    match (f.field_name, f.field_typ) with
    | Some fname, Casetype { tag; _ } when not (is_int_dispatch_typ tag) ->
        let tag_field = Field { f with field_typ = tag } in
        let body_field = field (fname ^ "_body") All_bytes in
        [ tag_field; body_field ]
    | _ -> [ Field f ]
  in
  { s with fields = List.concat_map split s.fields }

let decl_name = function
  | Enum_decl { name; _ } -> Some name
  | Casetype_decl { name; _ } -> Some name
  | Typedef { struct_ = { name; _ }; _ } -> Some name
  | _ -> None

let absorb_candidate (acc_rev, seen) e =
  match decl_name e with
  | None -> (acc_rev, seen)
  | Some n when List.mem n seen -> (acc_rev, seen)
  | Some n -> (e :: acc_rev, n :: seen)

let absorb_typedef_dependencies acc d =
  match d with
  | Typedef { struct_; _ } ->
      let candidates = enum_decls struct_ @ casetype_decls_of_struct struct_ in
      List.fold_left absorb_candidate acc candidates
  | _ -> acc

let module_ ?doc decls =
  (* Auto-prepend enum and casetype declarations needed by typedefs in
     [decls] but not already declared in the module. Output order matches
     the order [casetype_decls_of_struct] returns: dispatch before
     wrapper, since the wrapper applies the dispatch. *)
  let already_declared =
    List.filter_map decl_name decls |> List.fold_left (fun acc n -> n :: acc) []
  in
  (* First pass: split string-tagged casetype fields in every typedef.
     This must happen before [casetype_decls_of_struct] runs so the
     wrapper-and-dispatch projection only sees int-tagged casetypes. *)
  let split_decls =
    List.map
      (function
        | Typedef ({ struct_; _ } as t) ->
            Typedef { t with struct_ = split_string_casetype_fields struct_ }
        | d -> d)
      decls
  in
  let extra_rev, _ =
    List.fold_left absorb_typedef_dependencies ([], already_declared)
      split_decls
  in
  { doc; decls = List.rev extra_rev @ split_decls }

(* 3D and C reserved words that cannot be used as field/param names. *)
module Reserved_3d = Set.Make (String)

let reserved_3d =
  Reserved_3d.of_list
    (String.split_on_char ' '
       (* 3D / EverParse keywords *)
       "typedef struct casetype switch case default enum extern mutable \
        entrypoint export output where if else return abort var unit bool true \
        false sizeof this int char void float double long short unsigned \
        signed static const volatile auto register union while for do break \
        continue goto type inline UINT8 UINT16 UINT16BE UINT32 UINT32BE UINT64 \
        UINT64BE Bool PUINT8 abstract and as assert assume attributes begin by \
        calc class decreases default downto effect eliminate ensures exception \
        exists forall friend fun function ghost include inline_for_extraction \
        instance introduce irreducible layered_effect let logic match module \
        new new_effect noeq noextract opaque_to_smt open polymonadic_bind \
        polymonadic_subcomp private range_of rec reflectable reifiable reify \
        requires restart_solver returns set_range_of sub_effect synth then tot \
        total try unfold unopteq val when with")

(* Append a trailing underscore to names that 3D or F* reserves so the
   projection always produces a parseable identifier even when the user
   picks something like [total]. The same escaping is applied wherever a
   user-chosen name reaches 3D output: param decls, [Param_ref], [Ref],
   field names. The OCaml-side name is unchanged. *)
let escape_3d name =
  if Reserved_3d.mem name reserved_3d then name ^ "_" else name

let pp_endian ppf = function Little -> () | Big -> Fmt.string ppf "BE"

let pp_bitfield_base ppf = function
  | U8 -> Fmt.string ppf "UINT8"
  | U16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | U32 e -> Fmt.pf ppf "UINT32%a" pp_endian e

let pp_cast_type ppf = function
  | `U8 -> Fmt.string ppf "UINT8"
  | `U16 -> Fmt.string ppf "UINT16"
  | `U32 -> Fmt.string ppf "UINT32"
  | `U64 -> Fmt.string ppf "UINT64"

let string_of_uint64 n =
  let ten = 10L in
  let rec loop n acc =
    let q = Int64.unsigned_div n ten in
    let r = Int64.unsigned_rem n ten |> Int64.to_int in
    let acc = Char.chr (Char.code '0' + r) :: acc in
    if q = 0L then acc else loop q acc
  in
  String.of_seq (List.to_seq (loop n []))

let rec pp_expr : type a. a expr Fmt.t =
 fun ppf expr ->
  match expr with
  | Int n when n < 0 ->
      (* 3D has no negative integer literals in any syntax, so an expression with
         one (e.g. comparing an unsigned field to [-1]) has no projection and is
         rejected with a clear error. *)
      Fmt.invalid_arg
        "Wire: negative integer literal %d has no 3D projection (EverParse has \
         no negative literals)"
        n
  | Int n -> Fmt.int ppf n
  | Int64 n -> Fmt.pf ppf "%suL" (string_of_uint64 n)
  | Bool true -> Fmt.string ppf "true"
  | Bool false -> Fmt.string ppf "false"
  | Ref (_, name) -> Fmt.string ppf (escape_3d name)
  | Param_ref p -> Fmt.string ppf (escape_3d p.name)
  | Sizeof t -> Fmt.pf ppf "sizeof (%a)" pp_typ t
  | Sizeof_this -> Fmt.string ppf "sizeof (this)"
  | Field_pos ->
      (* EverParse has no [field_pos] keyword: the field position is not
         available as a 3D expression, so it has no projection and is rejected
         with a clear error. *)
      invalid_arg "Wire: field_pos has no 3D projection"
  | Add (a, b) -> Fmt.pf ppf "(%a + %a)" pp_expr a pp_expr b
  | Sub (a, b) -> Fmt.pf ppf "(%a - %a)" pp_expr a pp_expr b
  | Mul (a, b) -> Fmt.pf ppf "(%a * %a)" pp_expr a pp_expr b
  | Div (a, b) -> Fmt.pf ppf "(%a / %a)" pp_expr a pp_expr b
  | Mod (a, b) -> Fmt.pf ppf "(%a %% %a)" pp_expr a pp_expr b
  | Land (a, b) -> Fmt.pf ppf "(%a & %a)" pp_expr a pp_expr b
  | Lor (a, b) -> Fmt.pf ppf "(%a | %a)" pp_expr a pp_expr b
  | Lxor (a, b) -> Fmt.pf ppf "(%a ^ %a)" pp_expr a pp_expr b
  | Lnot a -> Fmt.pf ppf "(~%a)" pp_expr a
  | Lsl (a, b) -> Fmt.pf ppf "(%a << %a)" pp_expr a pp_expr b
  | Lsr (a, b) -> Fmt.pf ppf "(%a >> %a)" pp_expr a pp_expr b
  | Eq (a, b) -> Fmt.pf ppf "(%a == %a)" pp_expr a pp_expr b
  | Ne (a, b) -> Fmt.pf ppf "(%a != %a)" pp_expr a pp_expr b
  | Lt (a, b) -> Fmt.pf ppf "(%a < %a)" pp_expr a pp_expr b
  | Le (a, b) -> Fmt.pf ppf "(%a <= %a)" pp_expr a pp_expr b
  | Gt (a, b) -> Fmt.pf ppf "(%a > %a)" pp_expr a pp_expr b
  | Ge (a, b) -> Fmt.pf ppf "(%a >= %a)" pp_expr a pp_expr b
  | And (a, b) -> Fmt.pf ppf "(%a && %a)" pp_expr a pp_expr b
  | Or (a, b) -> Fmt.pf ppf "(%a || %a)" pp_expr a pp_expr b
  | Not a -> Fmt.pf ppf "(!%a)" pp_expr a
  | Cast (t, e) -> Fmt.pf ppf "((%a) %a)" pp_cast_type t pp_expr e
  | If_then_else (c, t, e) ->
      Fmt.pf ppf "((%a) ? %a : %a)" pp_expr c pp_expr t pp_expr e

and pp_typ : type a. a typ Fmt.t =
 fun ppf typ ->
  match typ with
  | Uint8 -> Fmt.string ppf "UINT8"
  | Uint16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | Uint32 e -> Fmt.pf ppf "UINT32%a" pp_endian e
  (* 3D has no 63-bit type. Project to the 8-byte UINT64: both decoders then
     accept every 8-byte input (the OCaml reader keeps the low 63 bits), so the
     C validator never accepts more than the OCaml one. *)
  | Uint63 e -> Fmt.pf ppf "UINT64%a" pp_endian e
  | Uint64 e -> Fmt.pf ppf "UINT64%a" pp_endian e
  (* 3D has no native signed types: project to the same-width UINT*. The
     two's-complement reinterpretation lives in the OCaml decoder. *)
  | Int8 -> Fmt.string ppf "UINT8"
  | Int16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | Int32 e -> Fmt.pf ppf "UINT32%a" pp_endian e
  | Int64 e -> Fmt.pf ppf "UINT64%a" pp_endian e
  (* IEEE 754 has no native 3D type; project to the underlying unsigned width.
     Float predicates (is_nan, is_finite) compile to bit-pattern refinements. *)
  | Float32 e -> Fmt.pf ppf "UINT32%a" pp_endian e
  | Float64 e -> Fmt.pf ppf "UINT64%a" pp_endian e
  | Uint_var { size; endian } ->
      Fmt.pf ppf "UINT%a(%a)" pp_endian endian pp_expr size
  | Bits { base; _ } -> pp_bitfield_base ppf base
  | Unit -> Fmt.string ppf "unit"
  | All_bytes -> Fmt.string ppf "all_bytes"
  | All_zeros -> Fmt.string ppf "all_zeros"
  | Zeroterm -> Fmt.string ppf "UINT8[:zeroterm]"
  | Zeroterm_at_most { size } ->
      Fmt.pf ppf "UINT8[:zeroterm-byte-size-at-most %a]" pp_expr size
  | Where { cond; inner } -> Fmt.pf ppf "%a { %a }" pp_typ inner pp_expr cond
  | Array { len; elem; _ } -> Fmt.pf ppf "%a[%a]" pp_typ elem pp_expr len
  | Byte_array { size } | Byte_slice { size } ->
      Fmt.pf ppf "UINT8[:byte-size %a]" pp_expr size
  | Byte_array_where { size; cond; _ } ->
      Fmt.pf ppf "UINT8 { %a }[:byte-size %a]" pp_expr cond pp_expr size
  | Single_elem { size; elem; at_most = false } ->
      Fmt.pf ppf "%a[:byte-size-single-element-array %a]" pp_typ elem pp_expr
        size
  | Single_elem { size; elem; at_most = true } ->
      Fmt.pf ppf "%a[:byte-size-single-element-array-at-most %a]" pp_typ elem
        pp_expr size
  | Enum { name; _ } -> Fmt.string ppf name
  | Casetype { name; _ } -> Fmt.string ppf name
  | Struct { name; _ } -> Fmt.string ppf name
  | Type_ref name -> Fmt.string ppf name
  | Qualified_ref { module_; name } -> Fmt.pf ppf "%s::%s" module_ name
  | Apply { typ; args } ->
      Fmt.pf ppf "%a(%a)" pp_typ typ Fmt.(list ~sep:comma pp_packed_expr) args
  | Map { inner; _ } -> pp_typ ppf inner
  (* A parametric sub-codec is emitted as a typedef with formals (see
     [extract]), so the use site must apply them: [Sub(p1, p2)]. The outer
     codec surfaces the same-named params, so each formal resolves to the
     outer's matching parameter. *)
  | Codec { codec_name; codec_struct; _ } -> (
      match codec_struct.params with
      | [] -> Fmt.string ppf codec_name
      | params ->
          Fmt.pf ppf "%s(%a)" codec_name
            Fmt.(list ~sep:comma string)
            (List.map (fun (p : param) -> p.param_name) params))
  (* [Optional]/[Optional_or]/[Repeat] are field decorations: their
     wrapping combinators ([map], [where], [array], [casetype], [apply])
     reject them at construction, so reaching this branch from a typ
     emitted by 3D projection means a struct field is being printed
     standalone -- caller's job, never our pp_typ. The trivial-predicate
     branches survive because they erase the decoration entirely. *)
  | Optional { present = Bool true; inner } -> pp_typ ppf inner
  | Optional { present = Bool false; _ } -> Fmt.string ppf "UINT8"
  | Optional_or { present = Bool true; inner; _ } -> pp_typ ppf inner
  | Optional_or { present = Bool false; _ } -> Fmt.string ppf "UINT8"
  | Optional _ | Optional_or _ | Repeat _ ->
      assert false (* unreachable: rejected by the wrapping combinator *)

and pp_packed_expr ppf (Pack_expr e) = pp_expr ppf e

(* 3D's [var x = a; p] requires [a] to be an atomic action (extern call,
   field_ptr, ...), not an arbitrary expression. Wire's [Action.var name e]
   binds a name to a pure expression, so we lower it by substituting
   [Ref name -> e] in subsequent stmts and dropping the [var] emission. *)
let rec subst_expr : type a. (string * int expr) list -> a expr -> a expr =
 fun env e ->
  let r e = subst_expr env e in
  match e with
  | Int _ | Int64 _ | Bool _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos
    ->
      e
  | Ref (I, name) -> ( try List.assoc name env with Not_found -> e)
  | Ref (I64, _) -> e
  | Add (a, b) -> Add (r a, r b)
  | Sub (a, b) -> Sub (r a, r b)
  | Mul (a, b) -> Mul (r a, r b)
  | Div (a, b) -> Div (r a, r b)
  | Mod (a, b) -> Mod (r a, r b)
  | Land (a, b) -> Land (r a, r b)
  | Lor (a, b) -> Lor (r a, r b)
  | Lxor (a, b) -> Lxor (r a, r b)
  | Lnot a -> Lnot (r a)
  | Lsl (a, b) -> Lsl (r a, r b)
  | Lsr (a, b) -> Lsr (r a, r b)
  | Eq (a, b) -> Eq (r a, r b)
  | Ne (a, b) -> Ne (r a, r b)
  | Lt (a, b) -> Lt (r a, r b)
  | Le (a, b) -> Le (r a, r b)
  | Gt (a, b) -> Gt (r a, r b)
  | Ge (a, b) -> Ge (r a, r b)
  | And (a, b) -> And (r a, r b)
  | Or (a, b) -> Or (r a, r b)
  | Not a -> Not (r a)
  | Cast (w, x) -> Cast (w, r x)
  | If_then_else (c, a, b) -> If_then_else (r c, r a, r b)

let pp_stmt env ppf stmt =
  let e a = subst_expr env a in
  match stmt with
  | Assign (p, x) -> Fmt.pf ppf "*%s = %a;" (escape_3d p.name) pp_expr (e x)
  | Field_assign (ptr, field_name, x) ->
      Fmt.pf ppf "%s->%s = %a;" ptr field_name pp_expr (e x)
  | Extern_call (fn, args) -> Fmt.pf ppf "%s(%s);" fn (String.concat ", " args)
  | Return x -> Fmt.pf ppf "return %a;" pp_expr (e x)
  | Abort -> Fmt.string ppf "abort;"
  | If _ | Var _ ->
      (* Handled by [pp_stmts] which threads the substitution env. *)
      assert false

let rec pp_stmts env ppf = function
  | [] -> ()
  | Var (name, x) :: rest -> pp_stmts ((name, subst_expr env x) :: env) ppf rest
  | If (cond, then_, else_opt) :: rest ->
      let cond = subst_expr env cond in
      (match else_opt with
      | None -> Fmt.pf ppf "if (%a) { %a }" pp_expr cond (pp_stmts env) then_
      | Some else_ ->
          Fmt.pf ppf "if (%a) { %a } else { %a }" pp_expr cond (pp_stmts env)
            then_ (pp_stmts env) else_);
      if rest <> [] then Fmt.sp ppf ();
      pp_stmts env ppf rest
  | stmt :: rest ->
      pp_stmt env ppf stmt;
      if rest <> [] then Fmt.sp ppf ();
      pp_stmts env ppf rest

let pp_action ppf = function
  | Success stmts -> Fmt.pf ppf "@[<h>{:on-success %a }@]" (pp_stmts []) stmts
  | Act stmts -> Fmt.pf ppf "@[<h>{:act %a }@]" (pp_stmts []) stmts

(* Extract field suffix for arrays - the modifier goes after the field name *)
type field_suffix =
  | No_suffix
  | Bitwidth of int
  | Byte_array of int expr
  | Single_elem of { size : int expr; at_most : bool }
  | Array of int expr
  | Zeroterm
  | Zeroterm_at_most of int expr

let rec inner_wire_size : type a. a typ -> int option = function
  | Uint8 | Int8 -> Some 1
  | Uint16 _ | Int16 _ -> Some 2
  | Uint32 _ | Int32 _ | Float32 _ -> Some 4
  | Uint63 _ | Uint64 _ | Int64 _ | Float64 _ -> Some 8
  | Bits { base = U8; _ } -> Some 1
  | Bits { base = U16 _; _ } -> Some 2
  | Bits { base = U32 _; _ } -> Some 4
  | Unit -> Some 0
  | Map { inner; _ } -> inner_wire_size inner
  | Enum { base; _ } -> inner_wire_size base
  | Where { inner; _ } -> inner_wire_size inner
  | Codec { codec_fixed_size = Some n; _ } -> Some n
  | _ -> None

(* Like [inner_wire_size] but as an [int expr], so explicitly-sized
   types ([byte_array], [byte_slice], [single_elem], etc.) can drive
   the [byte-size] suffix of an enclosing [optional]/[array]. Returns
   [None] only for shapes whose total wire size is genuinely not
   expressible at projection time (variable-size codec without an
   exposed [wire_size_expr], casetype, struct ref, all_bytes/all_zeros). *)
let rec inner_wire_size_expr : type a. a typ -> int expr option = function
  | Byte_array { size } | Byte_slice { size } | Byte_array_where { size; _ } ->
      Some size
  | Single_elem { size; _ } -> Some size
  | Zeroterm_at_most { size } -> Some size
  | Uint_var { size; _ } -> Some size
  | Array { len; elem; _ } -> (
      match inner_wire_size_expr elem with
      | Some s -> Some (Mul (len, s))
      | None -> None)
  | Apply { typ; _ } -> inner_wire_size_expr typ
  (* Transparent wrappers carry the inner's wire size, including the byte-span
     family that [inner_wire_size] does not size. Without this an [array] over a
     [where] / [map] / [enum] of a byte span builds (the element passes
     [is_array_element], which looks through wrappers) but has no projectable
     size. *)
  | Where { inner; _ } -> inner_wire_size_expr inner
  | Map { inner; _ } -> inner_wire_size_expr inner
  | Enum { base; _ } -> inner_wire_size_expr base
  | t -> ( match inner_wire_size t with Some n -> Some (Int n) | None -> None)

(* Strip transparent wrappers to see whether a byte-budget list element renders
   as a named composite (sub-codec / casetype). Those are the only list elements
   whose [nz]-ness matters: a flat scalar / byte / varint element renders as
   [UINT8] / [UINTnn], itself [nz], so the emitted list is always well-kinded. *)
let rec renders_as_named_composite : type a. a typ -> bool = function
  | Codec _ | Casetype _ -> true
  | Map { inner; _ } -> renders_as_named_composite inner
  | Where { inner; _ } -> renders_as_named_composite inner
  | _ -> false

(* 3d-model boundary for a byte-budget list ([T_nlist]) element's base type
   printer. [Nlist_elem.t] is abstract, so the only way to obtain one is
   [Nlist_elem.v], which refuses a named-composite element that is not [nz] --
   EverParse rejects a list whose element parser may consume zero bytes. Routing
   both list emit sites ([array] / [repeat]) through here means the projection
   cannot hand {!pp_field} a base for an ill-kinded list. [is_array_element] /
   [is_repeat_element] reject such elements at construction, so for user-built
   codecs the proof always succeeds; this is the backstop one layer in. *)
module Nlist_elem : sig
  type t

  val v : 'a typ -> (Format.formatter -> unit) -> t
  val pp : t -> Format.formatter -> unit
end = struct
  type t = Format.formatter -> unit

  let v : type a. a typ -> (Format.formatter -> unit) -> t =
   fun elem pp ->
    if (not (renders_as_named_composite elem)) || nz elem then pp
    else
      Fmt.invalid_arg
        "Everparse: byte-budget list over %a, whose parser may consume zero \
         bytes; EverParse rejects a non-[nz] list element"
        pp_typ elem

  let pp t = t
end

(* The index bound a [cases] / lookup typ carries, seen through the transparent
   [Map] / [Where] / [Apply] wrappers. [Some n] means the decoded raw value is a
   valid index only when [< n], which the projection emits as a refinement so
   the generated C validator rejects the same out-of-range inputs the OCaml
   decoder does. *)
let rec index_bound_of : type a. a typ -> int option = function
  | Map { index_bound = Some _ as b; _ } -> b
  | Map { inner; index_bound = None; _ } -> index_bound_of inner
  | Where { inner; _ } -> index_bound_of inner
  | Apply { typ; _ } -> index_bound_of typ
  | _ -> None

(* Bit width of a typ's wire representation, seen through the same transparent
   wrappers as [index_bound_of] -- used only to decide whether a lookup's
   index bound is exhaustive (see [lookup_is_exhaustive], used by [pp_field]).
   [Bits] carries an explicit width; the other scalar bases contribute their
   fixed wire width. [None] for shapes with no fixed bit width (composites,
   byte arrays, ...), which conservatively keeps any bound they carry. *)
let rec bit_width_of : type a. a typ -> int option = function
  | Bits { width; _ } -> Some width
  | Uint8 | Int8 -> Some 8
  | Uint16 _ | Int16 _ -> Some 16
  | Uint32 _ | Int32 _ -> Some 32
  | Uint63 _ -> Some 63
  | Uint64 _ | Int64 _ -> Some 64
  | Map { inner; _ } -> bit_width_of inner
  | Where { inner; _ } -> bit_width_of inner
  | Apply { typ; _ } -> bit_width_of typ
  | Enum { base; _ } -> bit_width_of base
  | _ -> None

(* [nvariants] indices exhaust a field of [width] bits when every representable
   value is a valid index, i.e. [nvariants = 2 ^ width]: the [< nvariants]
   refinement [pp_field] would otherwise emit always holds, a dead check
   EverParse would still compile into the generated C. [width] is capped well
   below the native [int] width ([Sys.int_size] is 63 on a 64-bit host) before
   shifting: [1 lsl 62] already overflows into the sign bit, and no realistic
   variant list is exhaustive over a field that wide anyway, so fields at or
   above that width are simply treated as never exhaustive. *)
let lookup_is_exhaustive ~nvariants ~width =
  width >= 1 && width < 62 && nvariants = 1 lsl width

(* The case values an [enum] field admits, seen through the transparent
   [Map] / [Where] / [Apply] wrappers. The projection emits membership as a
   field refinement so the generated C validator rejects values outside the
   named cases, exactly as the OCaml decoder does (a bare base value otherwise
   slips through C while [Codec.decode] raises [Invalid_enum]). *)
let rec enum_cases_of : type a. a typ -> int list option = function
  (* Only a closed enum bounds its value set; an open enum accepts any value, so
     it emits no membership refinement. *)
  | Enum { cases; closed = true; _ } -> Some (List.map snd cases)
  | Enum { closed = false; _ } -> None
  | Map { inner; _ } -> enum_cases_of inner
  | Where { inner; _ } -> enum_cases_of inner
  | Apply { typ; _ } -> enum_cases_of typ
  | _ -> None

(* A 1-byte array / repeat element that carries an index bound (a lookup index
   into an [n]-entry table) projects like a [byte_array_where]: every byte is
   refined to [< n] through a synthesised element struct. Returns the element
   variable and its constraint, keyed on the bound so equal bounds share a
   single synthesised typedef. *)
let index_bound_elt : type a. a typ -> (string * bool expr) option =
 fun elem ->
  match (inner_wire_size elem, index_bound_of elem) with
  | Some 1, Some bound ->
      let elt_var = Fmt.str "%slk%d" elt_var_prefix bound in
      Some (elt_var, Lt (Ref (I, elt_var), Int bound))
  | _ -> None

(* The bare base an open enum renders as when it is a fixed-size array element.
   A closed enum admits only its named codes, so it projects to a 3D enum type
   that EverParse enforces per element, mirroring the OCaml decoder. An open enum
   admits any value and carries no membership, so it must render as its base
   scalar, not the enum type, or the generated C validator would reject codes the
   OCaml decoder accepts. Returns [None] for a closed enum or a non-enum, which
   keep their existing rendering. *)
let rec open_enum_elem_base : type a. a typ -> (Format.formatter -> unit) option
    = function
  | Enum { closed = false; base; _ } -> Some (fun ppf -> pp_typ ppf base)
  | Map { inner; _ } -> open_enum_elem_base inner
  | Where { inner; _ } -> open_enum_elem_base inner
  | Apply { typ; _ } -> open_enum_elem_base typ
  | _ -> None

let rec optional_suffix : type a.
    bool expr -> a typ -> field_suffix * (Format.formatter -> unit) =
 fun present inner ->
  match inner_wire_size_expr inner with
  | Some s ->
      (* Project the optional as a conditional byte region: [present ? s : 0]
         bytes. The base printer comes from [field_suffix] so the inner's
         element type (e.g. [UINT8] for a byte array) is emitted once, without
         its own [byte-size] suffix duplicating the conditional one. A closed
         enum inner routes through its refined-element struct so the present
         value's membership is enforced, as the OCaml decoder does. *)
      let pp_base =
        match closed_enum_elem inner with
        | Some (name, _, _ :: _) ->
            fun ppf -> Fmt.string ppf (enum_elem_struct_name name)
        | _ -> snd (field_suffix inner)
      in
      (Byte_array (If_then_else (present, s, Int 0)), pp_base)
  | None ->
      (* Self-delimiting inner with no wire-size expression (a variable-size
         sub-codec, casetype, ...). Dispatch on the gate via the synthesised
         casetype: [present ? 1 : 0] selects its absent (0-byte) or inner case. *)
      let opt_name = optional_casetype_name inner in
      let arg = If_then_else (present, Int 1, Int 0) in
      ( No_suffix,
        fun ppf ->
          pp_typ ppf
            (Apply { typ = Type_ref opt_name; args = [ Pack_expr arg ] }) )

and field_suffix : type a. a typ -> field_suffix * (Format.formatter -> unit) =
 fun typ ->
  match typ with
  | Bits { width; base; _ } ->
      (Bitwidth width, fun ppf -> pp_bitfield_base ppf base)
  | Uint_var { size; _ } -> (Byte_array size, fun ppf -> Fmt.string ppf "UINT8")
  | Byte_array { size } | Byte_slice { size } ->
      (Byte_array size, fun ppf -> Fmt.string ppf "UINT8")
  | Byte_array_where { size; elt_var; _ } ->
      let synth = synth_name_of_elt_var elt_var in
      (Byte_array size, fun ppf -> Fmt.string ppf synth)
  | Single_elem { size; elem; at_most } ->
      (* A wrapper-needing element goes through its synthesised struct name; a
         bare scalar or named struct renders inline. *)
      let pp_elem ppf =
        match single_elem_struct elem with
        | Some name -> Fmt.string ppf name
        | None -> pp_typ ppf elem
      in
      (Single_elem { size; at_most }, pp_elem)
  | Array { len; elem; _ } -> array_suffix len elem
  | Map { inner; _ } -> field_suffix inner
  | Enum { base; _ } -> field_suffix base
  | Optional { present = Bool true; inner } -> field_suffix inner
  | Optional { present = Bool false; _ } ->
      (Byte_array (Int 0), fun ppf -> Fmt.string ppf "UINT8")
  | Optional { present; inner } -> optional_suffix present inner
  | Optional_or { present = Bool true; inner; _ } -> field_suffix inner
  | Optional_or { present = Bool false; _ } ->
      (Byte_array (Int 0), fun ppf -> Fmt.string ppf "UINT8")
  | Optional_or { inner; _ } ->
      (* A dynamic [optional_or] is value-driven: the inner always occupies its
         bytes in the wire (the gate only selects decoded vs default on read),
         so it projects unconditionally, never as a [present ? size : 0]
         region. *)
      field_suffix inner
  | Repeat { size; elem; _ } -> repeat_suffix size elem
  | Zeroterm -> (Zeroterm, fun ppf -> Fmt.string ppf "UINT8")
  | Zeroterm_at_most { size } ->
      (Zeroterm_at_most size, fun ppf -> Fmt.string ppf "UINT8")
  | _ -> (No_suffix, fun ppf -> pp_typ ppf typ)

(* 3D's [name[len]] suffix only accepts byte-sized elements; for wider elements
   the size in bytes must be made explicit via [:byte-size]. [inner_wire_size_expr]
   handles fixed scalars, [byte_array]-style sized payloads, codec-with-fixed-size,
   and nested arrays. *)
and array_suffix : type a.
    int expr -> a typ -> field_suffix * (Format.formatter -> unit) =
 fun len elem ->
  match (inner_wire_size elem, inner_wire_size_expr elem) with
  | Some 1, _ -> (
      match index_bound_elt elem with
      | Some (elt_var, _) ->
          (* A bounded byte element (a lookup index) projects like a
             [byte_array_where]: a byte-budget array of the synthesised
             refined-byte struct, so every element carries the [< n] bound the
             OCaml decoder enforces. *)
          ( Byte_array len,
            fun ppf -> Fmt.string ppf (synth_name_of_elt_var elt_var) )
      | None ->
          let pp_elem ppf =
            match open_enum_elem_base elem with
            | Some pp -> pp ppf
            | None -> pp_typ ppf elem
          in
          (Array len, pp_elem))
  | _, Some s ->
      (* A wider element is emitted as its bare base under a byte budget of
         [len * width]; its own [:byte-size] suffix (e.g. for a [byte_array]
         chunk) is dropped so it is not duplicated. A named-composite base must
         be [nz] (see {!nlist_base}). A wider or big-endian closed enum cannot
         carry a 1-byte enum type, so [list_elem_pp] routes it through its
         refined-element struct. *)
      let pp_elem =
        list_elem_pp elem
          ~fallback:
            (Nlist_elem.pp (Nlist_elem.v elem (snd (field_suffix elem))))
      in
      (Byte_array (Mul (len, s)), pp_elem)
  | _ ->
      (* Unreachable: [array] rejects variable-size elem at construction. *)
      assert false

(* Variable-length list within a byte budget. A self-delimiting element with no
   named type goes through its wrapper struct; otherwise the element is emitted
   as its bare base (its own [:byte-size] suffix dropped) so the only suffix is
   the budget: a list of fixed n-byte chunks is just bytes on the wire, like a
   repeat of [UINT8]. A closed enum loses its 1-byte enum-type rendering at every
   width in a byte budget, so [list_elem_pp] routes it through the refined-element
   struct to keep per-element membership. *)
and repeat_suffix : type a.
    int expr -> a typ -> field_suffix * (Format.formatter -> unit) =
 fun size elem ->
  let pp_elem ppf =
    match index_bound_elt elem with
    | Some (elt_var, _) -> Fmt.string ppf (synth_name_of_elt_var elt_var)
    | None ->
        list_elem_pp elem
          ~fallback:(fun ppf ->
            match repeat_elem_struct elem with
            | Some name -> Fmt.string ppf name
            | None ->
                Nlist_elem.pp (Nlist_elem.v elem (snd (field_suffix elem))) ppf)
          ppf
  in
  (Byte_array size, pp_elem)

let anon_counter = Stdlib.ref 0

(* The documentation projection renders an enum field as its named 3D enum type
   (e.g. [Status StatusCode;]), which makes EverParse enforce membership through
   the type. The codegen projection keeps the base type plus an explicit
   membership refinement, because its FFI plug reads the field as the base type.
   Toggled around rendering by [to_3d]/[to_3d_file], mirroring [anon_counter]. *)
let render_enum_as_type = Stdlib.ref false

(* The named 3D enum type a field should render as under [render_enum_as_type],
   seen through the transparent [Map] / [Where] / [Apply] wrappers. Only an enum
   over a scalar base qualifies: an enum over a bitfield base maps to a plain
   bitfield with no enum declaration to reference. *)
let rec enum_type_name : type a. a typ -> string option = function
  | Enum { base = Bits _; _ } -> None
  (* An open enum has no 3D enum type to name (it projects as its base), so the
     doc projection types the field as the base scalar, not the enum. *)
  | Enum { closed = false; _ } -> None
  (* A big-endian base has no valid 3D enum type either; project as base plus a
     membership refinement. *)
  | Enum { base; _ } when enum_base_is_be base -> None
  | Enum { name; _ } -> Some name
  | Map { inner; _ } -> enum_type_name inner
  | Where { inner; _ } -> enum_type_name inner
  | Apply { typ; _ } -> enum_type_name typ
  | _ -> None

let rec extract_where_constraint : type a. a typ -> bool expr option * a typ =
 fun typ ->
  match typ with
  | Where { cond; inner } -> (
      match extract_where_constraint inner with
      | Some c2, inner' -> (Some (And (cond, c2)), inner')
      | None, inner' -> (Some cond, inner'))
  | _ -> (None, typ)

let combine_constraints a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some a, Some b -> Some (And (a, b))

(* Render the [:byte-size ...] / bitwidth / array suffix that follows a field
   name. Shared by [pp_field] and [pp_casetype_cases] so a casetype case body
   gets the suffix after its name (a valid field declaration) rather than
   inline on the type. *)
let pp_field_suffix ppf = function
  | No_suffix -> ()
  | Bitwidth w -> Fmt.pf ppf " : %d" w
  | Byte_array size -> Fmt.pf ppf "[:byte-size %a]" pp_expr size
  | Single_elem { size; at_most = false } ->
      Fmt.pf ppf "[:byte-size-single-element-array %a]" pp_expr size
  | Single_elem { size; at_most = true } ->
      Fmt.pf ppf "[:byte-size-single-element-array-at-most %a]" pp_expr size
  | Zeroterm -> Fmt.pf ppf "[:zeroterm]"
  | Zeroterm_at_most size ->
      Fmt.pf ppf "[:zeroterm-byte-size-at-most %a]" pp_expr size
  | Array len -> Fmt.pf ppf "[%a]" pp_expr len

(* The scalar a field's refinement compares against, seen through transparent
   [Map] / [Enum] / [Where] wrappers. 3D has only unsigned integer types, so a
   signed or float field's ordering refinement needs special projection. *)
let rec scalar_kind : type a. a typ -> [ `Signed of int | `Float | `Other ] =
  function
  | Int8 -> `Signed 8
  | Int16 _ -> `Signed 16
  | Int32 _ -> `Signed 32
  | Int64 _ -> `Signed 64
  | Float32 _ | Float64 _ -> `Float
  | Map { inner; _ } -> scalar_kind inner
  | Enum { base; _ } -> scalar_kind base
  | Where { inner; _ } -> scalar_kind inner
  | _ -> `Other

let rec mentions_field : type a. string -> a expr -> bool =
 fun name e ->
  match e with
  | Ref (_, n) -> String.equal n name
  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Land (a, b)
  | Lor (a, b)
  | Lxor (a, b)
  | Lsl (a, b)
  | Lsr (a, b) ->
      mentions_field name a || mentions_field name b
  | Lnot a -> mentions_field name a
  | Cast (_, a) -> mentions_field name a
  | Eq (a, b) -> mentions_field name a || mentions_field name b
  | Ne (a, b) -> mentions_field name a || mentions_field name b
  | Lt (a, b) -> mentions_field name a || mentions_field name b
  | Le (a, b) -> mentions_field name a || mentions_field name b
  | Gt (a, b) -> mentions_field name a || mentions_field name b
  | Ge (a, b) -> mentions_field name a || mentions_field name b
  | And (a, b) -> mentions_field name a || mentions_field name b
  | Or (a, b) -> mentions_field name a || mentions_field name b
  | Not a -> mentions_field name a
  | If_then_else (c, t, el) ->
      mentions_field name c || mentions_field name t || mentions_field name el
  | Int _ | Int64 _ | Bool _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos
    ->
      false

(* Two's-complement: a signed n-bit value compared with an ordering operator
   equals the unsigned value compared after flipping the sign bit of both
   operands ([a <_s b] iff [(a ^ 2^(n-1)) <_u (b ^ 2^(n-1))]). The field projects
   to an unsigned type, so a [width]-bit signed field's ordering refinement
   ([self OP constant]) is rewritten this way; a comparison with a constant
   outside the signed range folds to a literal, and anything that is not a plain
   comparison of the field with an integer literal is rejected (it cannot be
   projected faithfully). *)
let rebuild_ordering op (l : int expr) (r : int expr) : bool expr =
  match op with
  | `Lt -> Lt (l, r)
  | `Le -> Le (l, r)
  | `Gt -> Gt (l, r)
  | `Ge -> Ge (l, r)

(* Rewrite a signed field's equality refinement. Unlike ordering, equality needs
   no order-preserving flip: the byte that decodes to signed [k] is just its
   two's-complement [k land mask]. A constant outside the signed range can never
   be equal, so it folds to a constant. *)
let rewrite_signed_eq : type x.
    name:string ->
    mask:int ->
    smin:int ->
    smax:int ->
    ne:bool ->
    x expr ->
    x expr ->
    bool expr ->
    bool expr =
 fun ~name ~mask ~smin ~smax ~ne a b orig ->
  let fold k : bool expr =
    match (ne, k >= smin && k <= smax) with
    | false, true -> Eq (Ref (I, name), Int (k land mask))
    | false, false -> Bool false
    | true, true -> Ne (Ref (I, name), Int (k land mask))
    | true, false -> Bool true
  in
  match (a, b) with
  | Ref (I, n), Int k when String.equal n name -> fold k
  | Int k, Ref (I, n) when String.equal n name -> fold k
  | _ ->
      if mentions_field name a || mentions_field name b then
        Fmt.invalid_arg
          "Wire: signed field %S has an equality constraint that is not a \
           plain comparison with an integer literal, so it cannot be projected \
           to 3D faithfully; compare the field directly to a constant"
          name
      else orig

(* Rewrite a signed field's ordering refinement to the two's-complement form the
   unsigned projected field reads ([x < 100] over [int8] becomes [(x ^ 128) <
   228]). A constant outside the signed range folds to a constant. *)
let rewrite_signed_ordering : type x.
    name:string ->
    signbit:int ->
    mask:int ->
    smin:int ->
    smax:int ->
    [ `Lt | `Le | `Gt | `Ge ] ->
    x expr ->
    x expr ->
    bool expr ->
    bool expr =
 fun ~name ~signbit ~mask ~smin ~smax op a b orig ->
  let flip_const k : int expr = Int (k land mask lxor signbit) in
  let flip_self : int expr = Lxor (Ref (I, name), Int signbit) in
  let self_op_const k : bool expr =
    if k > smax then match op with `Lt | `Le -> Bool true | _ -> Bool false
    else if k < smin then
      match op with `Lt | `Le -> Bool false | _ -> Bool true
    else rebuild_ordering op flip_self (flip_const k)
  in
  let const_op_self k : bool expr =
    if k > smax then match op with `Lt | `Le -> Bool false | _ -> Bool true
    else if k < smin then
      match op with `Lt | `Le -> Bool true | _ -> Bool false
    else rebuild_ordering op (flip_const k) flip_self
  in
  match (a, b) with
  | Ref (I, n), Int k when String.equal n name -> self_op_const k
  | Int k, Ref (I, n) when String.equal n name -> const_op_self k
  | _ ->
      if mentions_field name a || mentions_field name b then
        Fmt.invalid_arg
          "Wire: signed field %S has an ordering constraint that is not a \
           plain comparison with an integer literal, so it cannot be projected \
           to 3D faithfully; compare the field directly to a constant"
          name
      else orig

let translate_signed_refinement ~name ~width (cond : bool expr) : bool expr =
  let signbit = 1 lsl (width - 1) in
  let mask = (1 lsl width) - 1 in
  let smax = signbit - 1 and smin = -signbit in
  (* The helpers are called fully applied at each arm: a partial application
     would weaken the polymorphic operand type and let the comparison's
     existential escape. *)
  let ord op a b e =
    rewrite_signed_ordering ~name ~signbit ~mask ~smin ~smax op a b e
  in
  let rec xform (e : bool expr) : bool expr =
    match e with
    | And (a, b) -> And (xform a, xform b)
    | Or (a, b) -> Or (xform a, xform b)
    | Not a -> Not (xform a)
    | Lt (a, b) -> ord `Lt a b e
    | Le (a, b) -> ord `Le a b e
    | Gt (a, b) -> ord `Gt a b e
    | Ge (a, b) -> ord `Ge a b e
    | Eq (a, b) -> rewrite_signed_eq ~name ~mask ~smin ~smax ~ne:false a b e
    | Ne (a, b) -> rewrite_signed_eq ~name ~mask ~smin ~smax ~ne:true a b e
    | _ -> e
  in
  xform cond

(* A float ordering refinement has no faithful unsigned projection (IEEE bit
   patterns do not order as unsigned), so it is rejected; a signed field's
   ordering or equality refinement is rewritten to the two's-complement form the
   unsigned projected field reads. *)
let project_refinement ~name ~kind (cond : bool expr) : bool expr =
  (* Reject any ordering comparison that mentions this field, with [msg]. Each
     comparison constructor is matched in its own arm so its existential operand
     type stays local. *)
  let reject_orderings msg =
    let bad : type x. x expr -> x expr -> unit =
     fun a b ->
      if mentions_field name a || mentions_field name b then
        invalid_arg (msg ^ "; field " ^ name)
    in
    let rec walk : bool expr -> unit = function
      | And (a, b) | Or (a, b) ->
          walk a;
          walk b
      | Not a -> walk a
      | Lt (a, b) -> bad a b
      | Le (a, b) -> bad a b
      | Gt (a, b) -> bad a b
      | Ge (a, b) -> bad a b
      | _ -> ()
    in
    walk cond;
    cond
  in
  match kind with
  | `Other -> cond
  | `Float ->
      reject_orderings
        "Wire: a float field ordering constraint has no 3D projection (IEEE \
         bit patterns compare as unsigned)"
  | `Signed 64 ->
      reject_orderings
        "Wire: a signed 64-bit field ordering constraint cannot be projected \
         to 3D"
  | `Signed width -> translate_signed_refinement ~name ~width cond

(* A field whose value is an unsigned scalar at most 32 bits wide, seen through
   the transparent [Map] / [Enum] / [Where] wrappers. The sum of two such fields
   fits in both OCaml's native int and a 64-bit unsigned, so an [Add] over them
   can be projected without overflow by widening the operands. *)
let rec widenable_unsigned : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ -> true
  | Map { inner; _ } -> widenable_unsigned inner
  | Enum { base; _ } -> widenable_unsigned base
  | Where { inner; _ } -> widenable_unsigned inner
  | _ -> false

(* EverParse computes a refinement at the field's own (narrow) width, so a plain
   [a + b] over [UINT8] fields can overflow and F* refuses to verify it, while
   the OCaml decoder computes the same sum in its wide native int. Widen each
   [Add] operand that names a <=32-bit unsigned field to [UINT64] so the 3D sum
   matches the decoder's. Only [Add] is rewritten: [Sub] can underflow (an
   unsigned wrap the decoder does not do), and a wider or signed operand has no
   sound widening, so those are left untouched and keep their current behaviour. *)
(* The field-reference names an expression mentions. [collect_refs_packed] is the
   same walk through a comparison's existentially-typed operands. *)
let rec collect_refs : type a. a expr -> string list = function
  | Int _ | Int64 _ | Bool _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos
    ->
      []
  | Ref (_, name) -> [ name ]
  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Land (a, b)
  | Lor (a, b)
  | Lxor (a, b)
  | Lsl (a, b)
  | Lsr (a, b) ->
      collect_refs a @ collect_refs b
  | Lnot a -> collect_refs a
  | Lt (a, b) -> collect_refs_packed a @ collect_refs_packed b
  | Le (a, b) -> collect_refs_packed a @ collect_refs_packed b
  | Gt (a, b) -> collect_refs_packed a @ collect_refs_packed b
  | Ge (a, b) -> collect_refs_packed a @ collect_refs_packed b
  | And (a, b) | Or (a, b) -> collect_refs a @ collect_refs b
  | Not a -> collect_refs a
  | Cast (_, a) -> collect_refs a
  | If_then_else (c, a, b) -> collect_refs c @ collect_refs a @ collect_refs b
  | Eq (a, b) -> collect_refs_packed a @ collect_refs_packed b
  | Ne (a, b) -> collect_refs_packed a @ collect_refs_packed b

and collect_refs_packed : type a. a expr -> string list =
 fun e -> collect_refs e

let rec widen_add : type a. string list -> a expr -> a expr =
 fun ws e ->
  let r e = widen_add ws e in
  let operand e =
    match r e with
    | Ref (I, n) as e when List.mem n ws -> Cast (`U64, e)
    | e -> e
  in
  match e with
  | Add (a, b) -> Add (operand a, operand b)
  | Int _ | Int64 _ | Bool _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos
  | Ref _ ->
      e
  | Sub (a, b) -> Sub (r a, r b)
  | Mul (a, b) -> Mul (r a, r b)
  | Div (a, b) -> Div (r a, r b)
  | Mod (a, b) -> Mod (r a, r b)
  | Land (a, b) -> Land (r a, r b)
  | Lor (a, b) -> Lor (r a, r b)
  | Lxor (a, b) -> Lxor (r a, r b)
  | Lnot a -> Lnot (r a)
  | Lsl (a, b) -> Lsl (r a, r b)
  | Lsr (a, b) -> Lsr (r a, r b)
  | Eq (a, b) -> Eq (r a, r b)
  | Ne (a, b) -> Ne (r a, r b)
  | Lt (a, b) -> Lt (r a, r b)
  | Le (a, b) -> Le (r a, r b)
  | Gt (a, b) -> Gt (r a, r b)
  | Ge (a, b) -> Ge (r a, r b)
  | And (a, b) -> And (r a, r b)
  | Or (a, b) -> Or (r a, r b)
  | Not a -> Not (r a)
  | Cast (w, x) -> Cast (w, r x)
  | If_then_else (c, a, b) -> If_then_else (r c, r a, r b)

(* EverParse computes a constraint at the field's own narrow width, so a [Sub] or
   [Mul] naming a field underflows or overflows there: 3d.exe refuses to verify
   it ("cannot verify u8 subtraction / multiplication"), leaving the codec with
   no validator. Unlike [Add], neither has a sound widening: a widened [Sub]
   wraps on underflow where the OCaml decoder goes negative, and a widened [Mul]
   of wide operands overflows the decoder's own native int. Reject such a
   constraint at construction; a [Sub] / [Mul] of constants (which folds) is
   fine, as is additive field arithmetic. *)
let rec reject_field_sub_mul : type a. string -> a expr -> unit =
 fun name e ->
  let both : type x. x expr -> x expr -> unit =
   fun a b ->
    reject_field_sub_mul name a;
    reject_field_sub_mul name b
  in
  match e with
  | (Sub _ | Mul _) when collect_refs e <> [] ->
      let op = match e with Sub _ -> "subtraction" | _ -> "multiplication" in
      Fmt.invalid_arg
        "Wire: field %S has a constraint whose %s over a field has no 3D \
         projection (it under/overflows the field's narrow width); keep field \
         arithmetic in a constraint additive, or move it out of the constraint"
        name op
  | Sub (a, b) | Mul (a, b) -> both a b
  | Add (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Land (a, b)
  | Lor (a, b)
  | Lxor (a, b)
  | Lsl (a, b)
  | Lsr (a, b) ->
      both a b
  | Eq (a, b) -> both a b
  | Ne (a, b) -> both a b
  | Lt (a, b) -> both a b
  | Le (a, b) -> both a b
  | Gt (a, b) -> both a b
  | Ge (a, b) -> both a b
  | And (a, b) | Or (a, b) -> both a b
  | Lnot a -> reject_field_sub_mul name a
  | Not a -> reject_field_sub_mul name a
  | Cast (_, a) -> reject_field_sub_mul name a
  | If_then_else (c, a, b) ->
      reject_field_sub_mul name c;
      both a b
  | Int _ | Int64 _ | Bool _ | Ref _ | Param_ref _ | Sizeof _ | Sizeof_this
  | Field_pos ->
      ()

(* The narrow-width arithmetic a projected field constraint needs: reject a field
   [Sub] / [Mul] (no sound projection), then widen each [Add] operand. *)
let project_field_arith name widenable cond =
  reject_field_sub_mul name cond;
  widen_add widenable cond

(* The generated .3d doubles as protocol documentation, so a [?doc] note (often
   an RFC citation) should not run past ~80 columns as one long comment. Wrap the
   body greedily on word boundaries, opening with [open_] and closing with
   [close] ([/* .. */] for a field, [/*++ .. --*/] for a codec or module).
   Continuation lines align under the opener so the comment reads as a block. *)
let pp_wrapped_comment ppf ~open_ ~close text =
  let width = 72 in
  let words =
    String.split_on_char ' '
      (String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) text)
    |> List.filter (fun w -> w <> "")
  in
  let lines =
    let rec go acc cur = function
      | [] -> List.rev (if cur = "" then acc else cur :: acc)
      | w :: rest ->
          if cur = "" then go acc w rest
          else if String.length cur + 1 + String.length w <= width then
            go acc (cur ^ " " ^ w) rest
          else go (cur :: acc) w rest
    in
    go [] "" words
  in
  match lines with
  | [] -> Fmt.pf ppf "%s %s" open_ close
  | [ one ] -> Fmt.pf ppf "%s %s %s" open_ one close
  | first :: rest ->
      let indent = String.make (String.length open_ + 1) ' ' in
      let last = List.length rest - 1 in
      Fmt.pf ppf "%s %s" open_ first;
      List.iteri
        (fun i line ->
          if i = last then Fmt.pf ppf "@,%s%s %s" indent line close
          else Fmt.pf ppf "@,%s%s" indent line)
        rest

let pp_field_doc ppf d =
  Fmt.pf ppf "@,%t" (fun ppf ->
      pp_wrapped_comment ppf ~open_:"/*" ~close:"*/" d)

(* A lookup / cases field is valid only for in-range indices; the caller emits
   that bound as a refinement (the OCaml decoder enforces it via the [Map]
   decode) -- unless the lookup is exhaustive over the field's bit width, in
   which case every representable value is already a valid index and the
   bound is a dead check (see [lookup_is_exhaustive]). *)
let lookup_bound_cond raw_name field_typ =
  match index_bound_of field_typ with
  | Some len
    when lookup_is_exhaustive ~nvariants:len
           ~width:(Option.value ~default:(-1) (bit_width_of field_typ)) ->
      None
  | Some len -> Some (Lt (Ref (I, raw_name), Int len))
  | None -> None

let pp_field widenable ppf (Field f) =
  let raw_name, name =
    match f.field_name with
    | Some name -> (name, escape_3d name)
    | None ->
        let n = !anon_counter in
        incr anon_counter;
        let s = Fmt.str "_anon_%d" n in
        (s, s)
  in
  (* Extract Where constraints from the type so they appear as field
     constraints in the 3D output, not inline in the type. *)
  let where_cond, typ = extract_where_constraint f.field_typ in
  let bound_cond = lookup_bound_cond raw_name f.field_typ in
  (* The documentation projection types the field as its named enum, so
     EverParse enforces membership through the type. The codegen projection
     instead emits membership as a refinement on the base type, which the OCaml
     decoder mirrors on decode. *)
  let doc_enum =
    if !render_enum_as_type then enum_type_name f.field_typ else None
  in
  let enum_cond =
    match (doc_enum, enum_cases_of f.field_typ) with
    | None, Some (v0 :: rest) ->
        Some
          (List.fold_left
             (fun acc v -> Or (acc, Eq (Ref (I, raw_name), Int v)))
             (Eq (Ref (I, raw_name), Int v0))
             rest)
    | _ -> None
  in
  let constraint_ =
    combine_constraints
      (combine_constraints
         (combine_constraints f.constraint_ where_cond)
         bound_cond)
      enum_cond
  in
  (* Rewrite a signed field's ordering refinement to its two's-complement
     unsigned form (the field projects to an unsigned type), and reject a float
     ordering refinement, which has no faithful unsigned projection. *)
  let constraint_ =
    Option.map
      (project_refinement ~name:raw_name ~kind:(scalar_kind typ))
      constraint_
  in
  let constraint_ =
    Option.map (project_field_arith raw_name widenable) constraint_
  in
  let suffix, pp_base = field_suffix typ in
  let pp_base =
    match doc_enum with
    | Some n -> fun ppf -> Fmt.string ppf n
    | None -> pp_base
  in
  Option.iter (pp_field_doc ppf) f.field_doc;
  Fmt.pf ppf "@,%t %s%a" pp_base name pp_field_suffix suffix;
  Option.iter (Fmt.pf ppf " { %a }" pp_expr) constraint_;
  Option.iter (Fmt.pf ppf " %a" pp_action) f.action;
  Fmt.string ppf ";"

let pp_param ppf p =
  let (Pack_typ t) = p.param_typ in
  let name = escape_3d p.param_name in
  if p.mutable_ then Fmt.pf ppf "mutable %a *%s" pp_typ t name
  else Fmt.pf ppf "%a %s" pp_typ t name

let pp_params ppf params =
  if not (List.is_empty params) then
    Fmt.pf ppf "(%a)" Fmt.(list ~sep:comma pp_param) params

(* Collect every [Ref name] occurring in an expression. Used to detect
   field references in struct-level [where] clauses, which 3D's grammar
   does not allow (the [where] there sees only parameters). *)
(* If a struct-level [where] references fields (rather than only params),
   attach it as the [constraint_] of the last referenced field instead --
   3D's [where] clause only sees parameters. *)
let attach_where_to_target w target fields =
  List.map
    (fun (Field f as field) ->
      if f.field_name = Some target then
        let constraint_ = combine_constraints f.constraint_ (Some w) in
        Field { f with constraint_ }
      else field)
    fields

(* Last [field_name] that occurs in [names], by struct position. *)
let last_referenced_field names fields =
  List.fold_left
    (fun acc (Field f) ->
      match f.field_name with
      | Some n when List.mem n names -> Some n
      | _ -> acc)
    None fields

let lower_where_to_field_constraint where fields =
  match where with
  | None -> (None, fields)
  | Some w ->
      let refs = collect_refs w in
      let field_names =
        List.filter_map (fun (Field f) -> f.field_name) fields
      in
      let referenced = List.filter (fun r -> List.mem r field_names) refs in
      if referenced = [] then (Some w, fields)
      else
        (* Attach to the last referenced field by struct position so every
           name in the constraint is decoded by the time it runs. *)
        let target =
          match last_referenced_field referenced fields with
          | Some n -> n
          | None -> List.hd (List.rev referenced)
        in
        (None, attach_where_to_target w target fields)

(* EverParse cannot prove that a byte span sized [a - sizeof(this)] (as produced
   by [rest_bytes]) does not underflow the u32 subtraction. Emit the guard
   [a >= sizeof(this)] as a refinement on the immediately preceding field;
   EverParse uses it to discharge the subtraction (a non-scalar field cannot be
   refined, so this relies on the span following a scalar field, the usual
   header). This is a 3D-projection concern only: the OCaml decoder already
   knows [a]. *)
let guard_subtraction_sizes fields =
  let guard_of : type a. a typ -> bool expr option = function
    | Byte_array { size = Sub (a, Sizeof_this) } -> Some (Ge (a, Sizeof_this))
    | _ -> None
  in
  let add_guard g (Field f) =
    Field
      {
        f with
        constraint_ =
          (match f.constraint_ with
          | None -> Some g
          | Some c -> Some (And (c, g)));
      }
  in
  let rec go = function
    | f :: (Field nf :: _ as rest) ->
        let f =
          match guard_of nf.field_typ with Some g -> add_guard g f | None -> f
        in
        f :: go rest
    | fs -> fs
  in
  go fields

let pp_struct ppf (s : struct_) =
  anon_counter := 0;
  let name = escape_3d s.name in
  let where, fields = lower_where_to_field_constraint s.where s.fields in
  let fields = guard_subtraction_sizes fields in
  let widenable =
    List.filter_map
      (fun (Field f) ->
        match f.field_name with
        | Some n when widenable_unsigned f.field_typ -> Some n
        | _ -> None)
      fields
  in
  Fmt.pf ppf "typedef struct _%s%a" name pp_params s.params;
  Option.iter (Fmt.pf ppf "@,where (%a)" pp_expr) where;
  Fmt.pf ppf "@,{@[<v 2>";
  List.iter (pp_field widenable ppf) fields;
  Fmt.pf ppf "@]@,} %s" name

let pp_enum_cases ppf cases =
  List.iteri
    (fun i (cname, value) ->
      if not (Int.equal i 0) then Fmt.string ppf ",";
      Fmt.pf ppf "@,%s = %d" cname value)
    cases

let pp_casetype_cases : type k.
    Format.formatter -> k typ -> decl_case list -> unit =
 fun ppf tag cases ->
  (* When the switch tag is an enum, EverParse requires each case label to be the
     enum constant name, not the raw integer it stands for. *)
  let enum_label k =
    match tag with
    | Enum { cases = ecases; _ } ->
        List.find_map
          (fun (n, v) -> if Int.equal v k then Some n else None)
          ecases
    | _ -> None
  in
  List.iteri
    (fun i (tag_opt, Pack_typ typ) ->
      let field_name = Fmt.str "v%d" i in
      let suffix, pp_base = field_suffix typ in
      let pp_body ppf () =
        Fmt.pf ppf "%t %s%a" pp_base field_name pp_field_suffix suffix
      in
      match tag_opt with
      | None -> Fmt.pf ppf "@,default: %a;" pp_body ()
      | Some e -> (
          let label =
            match e with Pack_expr (Int k) -> enum_label k | _ -> None
          in
          match label with
          | Some name -> Fmt.pf ppf "@,case %s: %a;" (escape_3d name) pp_body ()
          | None -> Fmt.pf ppf "@,case %a: %a;" pp_packed_expr e pp_body ()))
    cases

let pp_decl ppf = function
  | Typedef { entrypoint; export; output; extern_; doc; struct_ = st } ->
      Option.iter
        (fun d ->
          pp_wrapped_comment ppf ~open_:"/*++" ~close:"--*/" d;
          Fmt.pf ppf "@,")
        doc;
      if extern_ then
        (* extern typedef struct _Name Name *)
        let n = escape_3d st.name in
        Fmt.pf ppf "extern typedef struct _%s %s@,@," n n
      else begin
        if output then Fmt.pf ppf "output@,";
        if export then Fmt.pf ppf "export@,";
        if entrypoint then Fmt.pf ppf "entrypoint@,";
        Fmt.pf ppf "%a;@,@," pp_struct st
      end
  | Define { name; value } ->
      if value < 0 then Fmt.pf ppf "#define %s (%d)@," name value
      else Fmt.pf ppf "#define %s 0x%x@," name value
  | Extern_fn { name; params; ret = Pack_typ ret } ->
      Fmt.pf ppf "@[<h>extern %a %s(%a)@]@,@," pp_typ ret name
        Fmt.(list ~sep:comma pp_param)
        params
  | Extern_probe { init; name } ->
      (* 3D's [EXTERN PROBE q? IDENT] rule has no terminator. *)
      if init then Fmt.pf ppf "extern probe (INIT) %s@,@," name
      else Fmt.pf ppf "extern probe %s@,@," name
  | Enum_decl { name; cases; base = Pack_typ base } ->
      Fmt.pf ppf "%a enum %s {@[<v 2>" pp_typ base name;
      pp_enum_cases ppf cases;
      Fmt.pf ppf "@]@,}@,@,"
  | Casetype_decl { name; params; tag = Pack_typ tag; cases } ->
      (* First param is the switch discriminant *)
      let disc_name =
        match params with p :: _ -> p.param_name | [] -> "tag"
      in
      (* Internal name has underscore prefix, public name doesn't *)
      let internal_name, public_name =
        if String.length name > 0 && name.[0] = '_' then
          (name, String.sub name 1 (String.length name - 1))
        else ("_" ^ name, name)
      in
      Fmt.pf ppf "casetype %s%a {@[<v 2>@,switch (%s) {" internal_name pp_params
        params disc_name;
      pp_casetype_cases ppf tag cases;
      Fmt.pf ppf "@,}@]@,} %s;@,@," public_name

let pp_module ppf m =
  Option.iter
    (fun d ->
      pp_wrapped_comment ppf ~open_:"/*++" ~close:"--*/" d;
      Fmt.pf ppf "@,@,")
    m.doc;
  List.iter (pp_decl ppf) m.decls

let to_3d ?(enum_as_type = false) m =
  render_enum_as_type := enum_as_type;
  Fun.protect
    ~finally:(fun () -> render_enum_as_type := false)
    (fun () -> Fmt.str "@[<v>%a@]" pp_module m)

let to_3d_file ?(enum_as_type = false) path m =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (to_3d ~enum_as_type m ^ "\n"))

let raise_eof ~expected ~got =
  raise (Parse_error (Unexpected_eof { expected; got }))

let pp_parse_error ppf = function
  | Unexpected_eof { expected; got } ->
      Fmt.pf ppf "unexpected EOF: expected %d bytes, got %d" expected got
  | Constraint_failed msg -> Fmt.pf ppf "constraint failed: %s" msg
  | Invalid_enum { value; valid } ->
      Fmt.pf ppf "invalid enum value %d, valid: [%a]" value
        Fmt.(list ~sep:comma int)
        valid
  | Invalid_tag tag -> Fmt.pf ppf "invalid tag: %d" tag
  | All_zeros_failed { offset } ->
      Fmt.pf ppf "non-zero byte at offset %d" offset

(* Encoded byte size of [v] under [typ], computed from the value rather
   than from a buffer. Mirrors [field_wire_size] for fixed cases and
   reads the value for variable byte fields. Sub-codecs delegate to the
   value-driven [codec_size_of_value] baked in at codec construction.

   Typs whose size depends on a parameter or sibling field ([Uint_var]
   with non-constant size, dynamic [Optional]/[Optional_or], parametric
   [Repeat] / [Single_elem]) are not handled here -- they only appear
   inside a codec, which threads its own value-driven size at seal. *)

(** Compute wire size of a type (None for variable-size types). *)
let rec size_of_typ_value : type a. a typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> 1
  | Uint16 _ -> 2
  | Uint32 _ -> 4
  | Uint63 _ -> 8
  | Uint64 _ -> 8
  | Int8 -> 1
  | Int16 _ -> 2
  | Int32 _ -> 4
  | Int64 _ -> 8
  | Float32 _ -> 4
  | Float64 _ -> 8
  | Bits { base = U8; _ } -> 1
  | Bits { base = U16 _; _ } -> 2
  | Bits { base = U32 _; _ } -> 4
  | Unit -> 0
  | All_bytes -> String.length v
  | All_zeros -> String.length v
  | Zeroterm -> String.length v + 1
  | Zeroterm_at_most { size = Int n } -> n
  | Zeroterm_at_most _ -> 0
  | Byte_array _ -> String.length v
  | Byte_array_where _ -> String.length v
  | Byte_slice _ -> Bytesrw.Bytes.Slice.length v
  | Uint_var { size = Int n; _ } -> n
  | Uint_var _ -> 0
  | Map { inner; encode; _ } -> size_of_typ_value inner (encode v)
  | Where { inner; _ } -> size_of_typ_value inner v
  | Enum { base; _ } -> size_of_typ_value base v
  | Optional { present = Bool true; inner } ->
      size_of_typ_value inner (Option.get v)
  | Optional { present = Bool false; _ } -> 0
  | Optional { inner; _ } -> (
      (* Dynamic gate: value drives presence at encode time. The
         buffer-driven [present_fn] is the decode-side oracle and would
         disagree with [v] here, so consult [v] directly. *)
      match v with
      | Some inner_v -> size_of_typ_value inner inner_v
      | None -> 0)
  | Optional_or { present = Bool true; inner; _ } -> size_of_typ_value inner v
  | Optional_or { present = Bool false; _ } -> 0
  | Optional_or { inner; _ } ->
      (* Dynamic gate: encode always has a value (the default fills in
         for [None]); the encoder writes the inner regardless of what
         the runtime gate would have said at decode. *)
      size_of_typ_value inner v
  | Codec { codec_size_of_value; _ } -> codec_size_of_value v
  | Single_elem { size = Int n; _ } -> n
  | Single_elem _ -> 0
  | Repeat { elem; seq = Seq_map s; _ } ->
      (* Sum the actual element sizes from the value, like [Array]. The byte
         budget ([size]) is only a literal when fixed; a dynamic budget left
         this at 0, so [Codec.size_of_value] under-counted a repeat field. *)
      let total = Stdlib.ref 0 in
      s.iter (fun e -> total := !total + size_of_typ_value elem e) v;
      !total
  | Array { elem; seq = Seq_map s; _ } ->
      let total = Stdlib.ref 0 in
      s.iter (fun e -> total := !total + size_of_typ_value elem e) v;
      !total
  | Apply { typ; _ } -> size_of_typ_value typ v
  | Casetype { tag; cases; _ } ->
      (* Tag bytes plus the matched case's body: find the branch whose
         [project] accepts [v], then size its inner from the projected body.
         Without this the value-driven size dropped the whole casetype to 0,
         so [Codec.size_of_value] under-allocated and [encode] ran off the
         buffer. *)
      let tag_size = Option.value ~default:0 (inner_wire_size tag) in
      let rec find = function
        | [] -> tag_size
        | Case_branch { cb_inner; cb_project; _ } :: rest -> (
            match cb_project v with
            | Some (_tag, body) -> tag_size + size_of_typ_value cb_inner body
            | None -> find rest)
      in
      find cases
  | Struct _ -> 0
  | Type_ref _ -> 0
  | Qualified_ref _ -> 0

let rec field_wire_size : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Uint32 _ -> Some 4
  | Uint63 _ -> Some 8
  | Uint64 _ -> Some 8
  | Int8 -> Some 1
  | Int16 _ -> Some 2
  | Int32 _ -> Some 4
  | Int64 _ -> Some 8
  | Float32 _ -> Some 4
  | Float64 _ -> Some 8
  | Uint_var { size = Int n; _ } -> Some n
  | Uint_var _ -> None
  | Bits { base; _ } -> (
      match base with U8 -> Some 1 | U16 _ -> Some 2 | U32 _ -> Some 4)
  | Unit -> Some 0
  | Byte_array { size = Int n }
  | Byte_array_where { size = Int n; _ }
  | Byte_slice { size = Int n } ->
      Some n
  | Where { inner; _ } -> field_wire_size inner
  | Enum { base; _ } -> field_wire_size base
  | Map { inner; _ } -> field_wire_size inner
  | Codec { codec_fixed_size; _ } -> codec_fixed_size
  | Optional { present = Bool true; inner } -> field_wire_size inner
  | Optional { present = Bool false; _ } -> Some 0
  | Optional _ -> None
  | Optional_or { present = Bool true; inner; _ } -> field_wire_size inner
  | Optional_or { present = Bool false; _ } -> Some 0
  | Optional_or _ -> None
  | Repeat _ -> None
  | Array { len = Int n; elem; _ } -> (
      match field_wire_size elem with Some k -> Some (n * k) | None -> None)
  | _ -> None

let c_type_of : type a. a typ -> string = function
  | Uint8 | Bits { base = U8; _ } -> "uint8_t"
  | Uint16 _ | Bits { base = U16 _; _ } -> "uint16_t"
  | Uint32 _ | Uint63 _ | Bits { base = U32 _; _ } -> "uint32_t"
  | Uint64 _ -> "uint64_t"
  (* Signed types project to UINT* in 3D so EverParse only sees unsigned
     widths; the same width drives the C field type. *)
  | Int8 -> "uint8_t"
  | Int16 _ -> "uint16_t"
  | Int32 _ -> "uint32_t"
  | Int64 _ -> "uint64_t"
  (* Floats are also projected as the same-width UINT* in 3D, since the
     verified parser sees the bit pattern; the float reinterpretation is
     OCaml-side. *)
  | Float32 _ -> "uint32_t"
  | Float64 _ -> "uint64_t"
  | Uint_var _ -> "uint32_t"
  | _ -> "uint32_t"

let ml_type_of : type a. a typ -> string = function
  | Uint8 | Uint16 _ | Uint_var _ | Bits _ -> "int"
  | Uint32 _ | Uint63 _ -> "int"
  | Uint64 _ -> "int64"
  | Int8 | Int16 _ | Int32 _ -> "int"
  | Int64 _ -> "int64"
  | Float32 _ | Float64 _ -> "float"
  | _ -> "int"
