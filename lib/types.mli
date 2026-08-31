(** Core type definitions for the Wire DSL. *)

(** Byte order. *)
type endian = Little | Big

val equal_endian : endian -> endian -> bool
(** [equal_endian a b] is [true] when [a] and [b] are the same byte order. *)

type eval_ctx
(** Parameter bindings for expression evaluation. Internal to the direct and
    compiled interpreters. The buffer is not part of the context: it travels as
    its own argument so that {!unbound_eval_ctx} is a constant, and a
    parameter-free encode or decode allocates no context. *)

val unbound_eval_ctx : eval_ctx
(** The context with no parameter bindings. Internal use. *)

val eval_ctx : ?set_param:(string -> int -> unit) -> (string -> int) -> eval_ctx
(** A context with explicit parameter lookup. Internal use. *)

val eval_param : eval_ctx -> string -> int
(** Look up a parameter, returning 0 in an unbound context. Internal use. *)

val eval_set_param : eval_ctx -> string -> int -> unit
(** Update a bound output parameter, if the context accepts writes. Internal
    use. *)

(* The parse-error types are declared here, before {!typ}, so that {!typ}'s own
   [Where] / [Field] constructors win type-directed disambiguation throughout;
   the {!predicate} constructors are reached only through a [predicate]-typed
   context. See {!section:parse-errors} for the exception and helpers. *)

(** Which predicate a {!Constraint_failed} came from. *)
type predicate = Where | Field | Action | Per_byte

(** Parse failure categories. {!constructor-Unexpected_eof} counts bytes, not
    buffer positions: [expected] is how many the value needed and [got] how many
    were left where it starts, so both are the same whatever offset the frame is
    read at. For {!constructor-Constraint_failed}, [value] is the offending
    field's value for a single-field self-constraint and [None] for a
    cross-field or where predicate. *)
type error_kind =
  | Unexpected_eof of { expected : int; got : int }
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | Missing_terminator
  | Non_zero_padding
  | Value_out_of_range of { value : int64 }
  | Zero_divisor
  | Constraint_failed of { which : predicate; value : int64 option }

type parse_error = { at : int; field : string list; kind : error_kind }
(** [at] is the absolute byte offset of the failing field. Alongside it the
    parse error records the path of field names leading to that field (root to
    leaf, empty at a top-level or anonymous position) and the failure kind. *)

val parse_error : ?at:int -> ?field:string list -> error_kind -> parse_error
(** [parse_error kind] builds a decode error carrying [kind]. The byte offset
    defaults to 0 and the field path to [[]] (a top-level or whole-buffer
    failure), so a caller synthesizing an error outside a codec run, such as a
    length pre-check or a mapping decoder, need only give the kind. *)

val eof :
  ?at:int ->
  ?field:string list ->
  expected:int ->
  got:int ->
  unit ->
  parse_error
(** [eof ~expected ~got ()] is [parse_error (Unexpected_eof { expected; got })]:
    the input ended with [got] bytes where [expected] were needed. *)

(** Which end of a packed base word the first declared bitfield occupies.

    {!constructor-Msb_first} places the first declared field at the most
    significant bit, matching how RFC, CCSDS, and IETF specs draw their bit
    diagrams. {!constructor-Lsb_first} places it at bit 0, matching MSVC C
    bit-field packing.

    Bit order is independent of byte order. Every combination of base word and
    bit order is a valid wire description. When projecting to EverParse 3D, the
    export layer reverses field declaration order within a bit group if the
    user's bit order differs from EverParse's native choice for the base; this
    preserves byte layout in memory and is invisible to the user. *)
type bit_order = Msb_first | Lsb_first

val equal_bit_order : bit_order -> bit_order -> bool
(** [equal_bit_order a b] is [true] when [a] and [b] are the same bit order. *)

(** {1 Sequence builders} *)

(** Builder for sequence accumulation. Existentially hides the builder type. *)
type ('elt, 'seq) seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map

val seq_list : ('a, 'a list) seq_map
(** Default builder: accumulate into a list. *)

val exact_array_elements : ('a, 'seq) seq_map -> expected:int -> 'seq -> 'a list
(** Materialise an array value after checking its declared element count.
    Internal helper shared by direct and compiled encoders. *)

val exact_repeat_elements :
  ('a, 'seq) seq_map ->
  expected:int ->
  size_of:('a -> int) ->
  'seq ->
  ('a * int) list
(** Materialise and size a repeat value after checking its byte budget. Internal
    helper shared by direct and compiled encoders. *)

val check_byte_field_size : expected:int -> actual:int -> unit
(** Check a byte-span value against its declared size. A fixed-size byte field
    is exact, so any mismatch raises [Invalid_argument]. Internal helper shared
    by every encoder path. *)

val check_nested_size : at_most:bool -> expected:int -> actual:int -> unit
(** Check an inner value against an exact or at-most nested region. Internal
    helper shared by sizing and encoders. *)

val raise_no_matching_case : unit -> 'a
(** Raise [Invalid_argument] for a casetype value no case projects. Such a value
    has no encoding, so it has no size either: the size path and both encode
    paths raise through here so all three say the same thing. *)

val enum_values : (string * int) list -> int list
(** The value set of an enum's named cases. *)

val enum_member : int list -> int -> bool
(** [enum_member valid v] is [true] when [v] is one of [valid]. The single
    membership rule decode and encode share, so the two halves agree on what a
    closed enum admits. *)

val check_enum_decode : at:int -> cases:(string * int) list -> int -> unit
(** Check a decoded value against a closed enum's named cases. An unlisted value
    raises {!constructor-Invalid_enum} carrying the case set. Scans the cases in
    place, so a value that is a member costs no allocation however many cases
    there are. *)

val check_enum_encode : name:string -> valid:int list -> int -> unit
(** Check a value against a closed enum's case set before encoding it. An
    unlisted value raises [Invalid_argument]: decode rejects it, so emitting it
    would produce bytes this library cannot read back. *)

val check_all_zeros_encode : string -> unit
(** Check an {!val-all_zeros} padding value is all zero bytes before encoding
    it. A non-zero byte raises [Invalid_argument]. *)

val check_zeroterm_encode : string -> unit
(** Check a {!val-zeroterm} value carries no NUL before encoding it. An embedded
    NUL raises [Invalid_argument]: it terminates the string on the wire, so the
    bytes would decode back short. Internal helper shared by every encoder path.
*)

val check_zeroterm_region : region:int -> len:int -> unit
(** Check a {!val-zeroterm_at_most} value of [len] bytes leaves room for its NUL
    terminator inside a [region]-byte span. Internal helper shared by every
    encoder path. *)

val check_unsigned_encode : bits:int -> int -> unit
(** Check an integer fits an unsigned [bits]-wide field before encoding it. The
    accepted range is [[0, 2^bits - 1]], exactly what the decoder produces, so
    encode stays inverse to decode; anything else raises [Invalid_argument],
    because masking it would put a different, equally legal, number on the wire
    that no later check can tell from the one the caller meant. Used by
    [bits ~width], the one shape still carried in a bare [int]. Where the native
    [int] is no wider than the slice the guard narrows to what is still
    checkable, so it never rejects a value the target can represent. *)

(** {2 Checked scalar writers}

    The signed 32-bit writers, under the names the encode paths use. Every
    fixed-width scalar writes through its own carrier, whose range is already
    the field's. *)

val set_int32_le : bytes -> int -> SInt32.t -> unit
(** [set_int32_le buf off v] writes [v] as four signed little-endian bytes. *)

val set_int32_be : bytes -> int -> SInt32.t -> unit
(** [set_int32_be buf off v] writes [v] as four signed big-endian bytes. *)

(** {1 Param handles} *)

type param_input
type param_output

type ('a, 'k) param_handle = {
  id : int;
  name : string;
  typ : 'a typ;
  packed_typ : packed_typ;
  mutable_ : bool;
}

and packed_typ = Pack_typ : 'a typ -> packed_typ

(** {1 Expressions}

    Typed expression language used in constraints, actions and size
    computations. Arithmetic and bitwise operators mirror OCaml conventions. *)

and _ ref_kind = I : int ref_kind | I64 : int64 ref_kind

(** Typed expressions used in constraints, actions, and sizes. *)
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
  | Land64 : int64 expr * int64 expr -> int64 expr
  | Lsr64 : int64 expr * int expr -> int64 expr
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

(** {1 Types} *)

(** Base storage for bitfield extractions. *)
and bitfield_base = U8 | U16 of endian | U32 of endian

and _ typ =
  | Uint8 : UInt8.t typ
      (** 8-bit unsigned. Carried by {!UInt8.t}, whose range is the field's: a
          number the byte cannot hold is refused where it is built. *)
  | Uint16 : endian -> UInt16.t typ
      (** 16-bit unsigned. Carried by {!UInt16.t}, whose range is the field's: a
          number two bytes cannot hold is refused where it is built. *)
  | Uint32 : endian -> UInt32.t typ  (** 32-bit unsigned. *)
  | Uint64 : endian -> UInt64.t typ
      (** 64-bit unsigned. Carried by {!UInt64.t}, which orders as an unsigned
          number; [int64] would rank the largest value below 1. *)
  | Int8 : SInt8.t typ
      (** 8-bit signed. Carried by {!SInt8.t}, whose range is the field's: a
          number the byte cannot hold is refused where it is built. *)
  | Int16 : endian -> SInt16.t typ
      (** 16-bit signed. Carried by {!SInt16.t}, whose range is the field's: a
          number two bytes cannot hold is refused where it is built. *)
  | Int32 : endian -> SInt32.t typ
      (** 32-bit signed. Carried by {!SInt32.t}, which holds the full range on
          every target; a plain [int] drops the top bits where it is narrower
          than the field. *)
  | Int64 : endian -> int64 typ  (** 64-bit signed. *)
  | Float32 : endian -> float typ
      (** IEEE 754 binary32, widened to OCaml [float]. *)
  | Float64 : endian -> float typ  (** IEEE 754 binary64. *)
  | Uint_var : { size : int expr; endian : endian } -> UInt63.t typ
      (** Variable-width unsigned integer (1-7 bytes). *)
  | Bits : {
      width : int;
      base : bitfield_base;
      bit_order : bit_order;
    }
      -> int typ  (** Bitfield. *)
  | Unit : unit typ  (** Zero-width. *)
  | All_bytes : string typ  (** Remaining bytes as string. *)
  | All_zeros : string typ  (** Remaining bytes, must be zero. *)
  | Zeroterm : string typ  (** NUL-terminated string. *)
  | Zeroterm_at_most : { size : int expr } -> string typ
      (** NUL-terminated string within an [size]-byte region. *)
  | Where : { cond : bool expr; inner : 'a typ } -> 'a typ  (** Guarded. *)
  | Array : {
      len : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ  (** Fixed-count array. *)
  | Byte_array : { size : int expr } -> string typ  (** Byte span as string. *)
  | Byte_array_where : {
      size : int expr;
      elt_var : string;
      cond : bool expr;
    }
      -> string typ
      (** Byte span with a per-byte refinement: each decoded byte must satisfy
          [cond], where [elt_var] is bound to the byte's value. *)
  | Byte_slice : { size : int expr } -> Bytesrw.Bytes.Slice.t typ
      (** Zero-copy byte span. *)
  | Single_elem : { size : int expr; elem : 'a typ; at_most : bool } -> 'a typ
      (** Single element in a sized region. *)
  | Enum : {
      name : string;
      cases : (string * int) list;
      base : 'a typ;
          (** Any type with an integer view: the case values name integers, and
              {!int_of_exn} reads one out of a decoded [base] value. *)
      closed : bool;
          (** [true]: only the listed values are valid. [false]: open set, the
              names document known values but any value is accepted. *)
    }
      -> 'a typ  (** Named enumeration. *)
  | Casetype : {
      name : string;
      tag : 'k typ;
      cases : ('a, 'k) case_branch list;
    }
      -> 'a typ  (** Tag-dispatched union. *)
  | Struct : struct_ -> unit typ  (** Nested struct. *)
  | Type_ref : string -> 'a typ  (** Forward reference by name. *)
  | Qualified_ref : { module_ : string; name : string } -> 'a typ
      (** Qualified reference. *)
  | Map : {
      inner : 'w typ;
      decode : 'w -> 'a;
      encode : 'a -> 'w;
      index_bound : int option;
          (** When [Some n] the raw [inner] value is a valid index only when
              [< n] (set by {!cases}). [decode] enforces it on the OCaml side;
              the 3D projection emits it as a field refinement. *)
    }
      -> 'a typ  (** Mapped type. *)
  | Apply : { typ : 'a typ; args : packed_expr list } -> 'a typ
      (** Parameterised type application. *)
  | Codec : {
      codec_name : string;
      codec_decode : eval_ctx -> Input_end.t -> bytes -> int -> 'r;
      codec_validate : eval_ctx -> Input_end.t -> bytes -> int -> unit;
          (** Everything [codec_decode] checks, none of what it builds. Run at
              every use site of the sub-codec, as the generated C runs the
              nested struct's validator. *)
      codec_encode : 'r -> eval_ctx -> bytes -> int -> int;
      codec_fixed_size : int option;
      codec_min_size : int;
          (** Bytes the codec occupies whatever its variable-size fields hold,
              hence the extent [codec_size_of] has to read before it can resolve
              a span. *)
      codec_size_of : eval_ctx -> Input_end.t -> bytes -> int -> int;
      codec_size_of_value : eval_ctx -> 'r -> int;
          (** Encoded byte length of a value, computed from the value rather
              than by re-reading the buffer. *)
      codec_field_readers :
        (string * (eval_ctx -> Input_end.t -> bytes -> int -> int)) list;
      codec_struct : struct_;
          (** Structural form of the codec, used by the 3D projection. *)
    }
      -> 'r typ  (** Embedded sub-codec. *)
  | Optional : { present : bool expr; inner : 'a typ } -> 'a option typ
      (** Conditionally present field. *)
  | Optional_or : {
      present : bool expr;
      inner : 'a typ;
      default : 'a;
    }
      -> 'a typ  (** Conditionally present field with default. *)
  | Repeat : {
      size : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ  (** Repeated elements filling a byte budget. *)

and ('a, 'k) case_branch =
  | Case_branch : {
      cb_tag : 'k option;
      cb_inner : 'w typ;
      cb_inject : 'k -> 'w -> 'a;
      cb_project : 'a -> ('k * 'w) option;
    }
      -> ('a, 'k) case_branch

(** Existentially packed expression. *)
and packed_expr = Pack_expr : 'a expr -> packed_expr

and struct_ = {
  name : string;
  params : param list;
  where : bool expr option;
  fields : field list;
}
(** Struct declaration. *)

(** A single struct field. *)
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
(** Formal parameter. *)

(** Action attached to a field. *)
and action = Success of action_stmt list | Act of action_stmt list

(** Statements available in field actions. *)
and action_stmt =
  | Assign : ('a, param_output) param_handle * int expr -> action_stmt
  | Field_assign of string * string * int expr
  | Extern_call of string * string list
  | Return of bool expr
  | Abort
  | If of bool expr * action_stmt list * action_stmt list option
  | Var of string * int expr

type param_env = {
  codec_id : int;
  names : string array;
      (** Parallel to {!field-slots}: the param name at each slot, so a handle
          resolves to its slot by name (per-env, hence per-codec). *)
  slots : int array;
  bound : bool array;
      (** Parallel to {!field-slots}; set by [Param.bind] so consumers can
          detect unbound input params. *)
}

(** {1 Expression Constructors} *)

val int : int -> int expr
(** Integer literal. *)

val true_ : bool expr
(** Boolean true. *)

val false_ : bool expr
(** Boolean false. *)

val ref : string -> int expr
(** Reference a field or parameter by name. *)

val sizeof : 'a typ -> int expr
(** Size of a type in bytes. *)

val sizeof_this : int expr
(** Size of the enclosing struct. *)

val field_pos : int expr
(** Byte offset of the current field. *)

(** Infix operators for building expressions. *)
module Expr : sig
  (** {2 Arithmetic and bitwise operators} *)

  val int64 : int64 -> int64 expr
  (** 64-bit integer literal. *)

  val ( + ) : int expr -> int expr -> int expr
  (** Addition. *)

  val ( - ) : int expr -> int expr -> int expr
  (** Subtraction. *)

  val ( * ) : int expr -> int expr -> int expr
  (** Multiplication. *)

  val ( / ) : int expr -> int expr -> int expr
  (** Division. *)

  val ( mod ) : int expr -> int expr -> int expr
  (** Modulo. *)

  val ( land ) : int expr -> int expr -> int expr
  (** Bitwise AND. *)

  val land64 : int64 expr -> int64 expr -> int64 expr
  (** Bitwise AND on full-width 64-bit operands, for masking a 64-bit field
      before a comparison. Projects to 3D as [&]. *)

  val lsr64 : int64 expr -> int expr -> int64 expr
  (** Logical shift right on a full-width 64-bit operand; the shift amount must
      be a constant. Projects to 3D as [>>]. *)

  val ( lor ) : int expr -> int expr -> int expr
  (** Bitwise OR. *)

  val ( lxor ) : int expr -> int expr -> int expr
  (** Bitwise XOR. *)

  val lnot : int expr -> int expr
  (** Bitwise NOT. *)

  val ( lsl ) : int expr -> int expr -> int expr
  (** Logical shift left. *)

  val ( lsr ) : int expr -> int expr -> int expr
  (** Logical shift right. *)

  (** {2 Comparison operators} *)

  val ( = ) : 'a expr -> 'a expr -> bool expr
  (** Equality. *)

  val ( <> ) : 'a expr -> 'a expr -> bool expr
  (** Inequality. *)

  val ( < ) : 'a expr -> 'a expr -> bool expr
  (** Strictly less than, on native-integer or 64-bit expressions. *)

  val ( <= ) : 'a expr -> 'a expr -> bool expr
  (** Less than or equal, on native-integer or 64-bit expressions. *)

  val ( > ) : 'a expr -> 'a expr -> bool expr
  (** Strictly greater than, on native-integer or 64-bit expressions. *)

  val ( >= ) : 'a expr -> 'a expr -> bool expr
  (** Greater than or equal, on native-integer or 64-bit expressions. *)

  (** {2 Boolean operators} *)

  val ( && ) : bool expr -> bool expr -> bool expr
  (** Logical AND. *)

  val ( || ) : bool expr -> bool expr -> bool expr
  (** Logical OR. *)

  val not : bool expr -> bool expr
  (** Logical NOT. *)

  val if_then_else : bool expr -> int expr -> int expr -> int expr
  (** [if_then_else c t e] is [t] when [c] holds, else [e]. Use it for a value
      that depends on another field, e.g. a size where [0] means a maximum:
      [if_then_else Expr.(field = int 0) (int 65536) field]. *)

  (** {2 Integer casts} *)

  val to_uint8 : int expr -> int expr
  (** Cast to 8-bit unsigned. *)

  val to_uint16 : int expr -> int expr
  (** Cast to 16-bit unsigned. *)

  val to_uint32 : int expr -> int expr
  (** Cast to 32-bit unsigned. *)

  val to_uint64 : int expr -> int expr
  (** Cast to 64-bit unsigned. *)
end

val checked_add : int -> int -> int
(** Native-integer addition that raises {!exception-Parse_error} with
    {!constructor-Value_out_of_range} on overflow. *)

val checked_sub : int -> int -> int
(** Native-integer subtraction that raises {!exception-Parse_error} with
    {!constructor-Value_out_of_range} on overflow. *)

val checked_mul : int -> int -> int
(** Native-integer multiplication that raises {!exception-Parse_error} with
    {!constructor-Value_out_of_range} on overflow. *)

val checked_div : int -> int -> int
(** Native-integer division that raises {!exception-Parse_error} with
    {!constructor-Zero_divisor} when its divisor is zero. *)

val checked_mod : int -> int -> int
(** Native-integer modulo that raises {!exception-Parse_error} with
    {!constructor-Zero_divisor} when its divisor is zero. *)

(** {1 Type Constructors} *)

val uint8 : UInt8.t typ
(** 8-bit unsigned, native endian. *)

val uint16 : UInt16.t typ
(** 16-bit unsigned, little-endian. *)

val uint16be : UInt16.t typ
(** 16-bit unsigned, big-endian. *)

val uint32 : UInt32.t typ
(** 32-bit unsigned, little-endian. *)

val uint32be : UInt32.t typ
(** 32-bit unsigned, big-endian. *)

val uint64 : UInt64.t typ
(** 64-bit unsigned, little-endian. *)

val uint64be : UInt64.t typ
(** 64-bit unsigned, big-endian. *)

val int8 : SInt8.t typ
(** 8-bit signed two's-complement integer. *)

val int16 : SInt16.t typ
(** 16-bit signed, little-endian. *)

val int16be : SInt16.t typ
(** 16-bit signed, big-endian. *)

val int32 : SInt32.t typ
(** [int32] is a 32-bit signed little-endian integer, returned as a {!SInt32.t}.
*)

val int32be : SInt32.t typ
(** [int32be] is a 32-bit signed big-endian integer, returned as a {!SInt32.t}.
*)

val int64 : int64 typ
(** 64-bit signed, little-endian. *)

val int64be : int64 typ
(** 64-bit signed, big-endian. *)

val float32 : float typ
(** [float32] is an IEEE 754 binary32 little-endian, widened to OCaml [float].
*)

val float32be : float typ
(** [float32be] is an IEEE 754 binary32 big-endian, widened to OCaml [float]. *)

val float64 : float typ
(** [float64] is an IEEE 754 binary64 little-endian. *)

val float64be : float typ
(** [float64be] is an IEEE 754 binary64 big-endian. *)

val uint : ?endian:endian -> int expr -> UInt63.t typ
(** [uint size] is an unsigned integer of [size] bytes (1-7). Default endian is
    {!Big}. The size may be a dynamic expression for parameter-driven widths. *)

val bf_uint8 : bitfield_base
(** 8-bit bitfield base. *)

val bf_uint16 : bitfield_base
(** 16-bit bitfield base, little-endian. *)

val bf_uint16be : bitfield_base
(** 16-bit bitfield base, big-endian. *)

val bf_uint32 : bitfield_base
(** 32-bit bitfield base, little-endian. *)

val bf_uint32be : bitfield_base
(** 32-bit bitfield base, big-endian. *)

val bits : ?bit_order:bit_order -> width:int -> bitfield_base -> int typ
(** [bits ~width base] extracts [width] bits from a bitfield base. The bit order
    defaults to {!Msb_first}. *)

val map : ('w -> 'a) -> ('a -> 'w) -> 'w typ -> 'a typ
(** Map a wire type to a different OCaml type. *)

(** {1 Integer views}

    A combinator that refines the integer a field decodes to needs nothing else
    from its base, so which OCaml type carries that integer is the base's own
    business. These decide that reading once, for every type that has one, so
    the codec's checks, {!Eval}, {!Param}'s environments and the 3D projection
    all agree on it. *)

val int_of : 'a typ -> 'a -> int option
(** [int_of typ v] converts a typed value to [int]. [None] for a value that does
    not fit the native int (a {!val-uint64} over 2{^ 62}) and for a type with no
    integer view. *)

val relocate_at : at:int -> (unit -> 'a) -> 'a
(** [relocate_at ~at f] runs [f], moving any parse error it raises to byte [at].
    A conversion handed a value alone has no offset to name and reports 0; the
    reader that found the value does. *)

val int_view_is_total : 'a typ -> bool
(** [int_view_is_total typ] is [true] when every value of [typ] fits the native
    int, so taking its integer view cannot fail and needs no relocation. *)

val int_of_exn : 'a typ -> 'a -> int
(** [int_of_exn typ v] is {!val-int_of} without the [option]: it returns the
    [int] directly (no boxing on the numeric path) and raises {!Parse_error}
    ({!constructor-Value_out_of_range}) for a value beyond the native int range,
    [Invalid_argument] for a type with no integer view. *)

val of_int : 'a typ -> int -> 'a
(** [of_int typ n] is the value [typ] carries for the integer [n]. Raises
    [Invalid_argument] when [n] is outside what the type's field can hold, or
    when the type has no integer view. *)

val is_int_representable : 'a typ -> bool
(** [is_int_representable typ] is [true] when {!int_of_exn} and {!of_int} have a
    conversion for [typ]. *)

val reject_non_integer : combinator:string -> 'a typ -> unit
(** [reject_non_integer ~combinator typ] raises [Invalid_argument] naming
    [combinator] unless [typ] {!is_int_representable}. Called at construction,
    so a base a combinator cannot read as an integer fails at the description
    rather than on the first byte decoded. *)

val bool : 'a typ -> bool typ
(** Map an integer-valued type to boolean (0 = false). Raises [Invalid_argument]
    on a base with no integer view. *)

val cases : 'a list -> 'b typ -> 'a typ
(** Map integer values to a list of cases by index. Raises [Invalid_argument] on
    a base with no integer view. *)

val unit : unit typ
(** Zero-width unit type. *)

val all_bytes : string typ
(** Consume all remaining bytes. *)

val all_zeros : string typ
(** Consume remaining bytes, asserting all zero. *)

val zeroterm : string typ
(** [zeroterm] is a NUL-terminated string: bytes up to (but excluding) the first
    [0x00], which is consumed. Projects to the 3D [field[:zeroterm]] form, which
    desugars to the EverParse prelude [cstring]/[parse_string] combinator. This
    3D feature has no [3d-lang.html] section; see [EverParse3d.Prelude.fsti].
    Encoding a string that itself contains a [0x00] raises [Invalid_argument].
*)

val zeroterm_at_most : size:int expr -> string typ
(** [zeroterm_at_most ~size] is a NUL-terminated string occupying a fixed
    [size]-byte region: the terminator must appear within [size] bytes, and the
    field always consumes exactly [size] bytes (trailing bytes after the
    terminator are zero padding). Projects to the 3D
    [field[:zeroterm-byte-size-at-most size]] form ([t_at_most] of [cstring]).
    Undocumented in the manual; see [EverParse3d.Prelude.fsti]. *)

val where : bool expr -> 'a typ -> 'a typ
(** Guard a type with a boolean constraint. *)

val array : len:int expr -> 'a typ -> 'a list typ
(** Fixed-count array of elements. *)

val array_seq : ('a, 'seq) seq_map -> len:int expr -> 'a typ -> 'seq typ
(** Fixed-count array with custom builder. *)

val byte_array : size:int expr -> string typ
(** Byte span as a string. Encoding a value whose length differs from [size]
    raises [Invalid_argument]. *)

val byte_array_where :
  size:int expr -> per_byte:(int expr -> bool expr) -> string typ
(** [byte_array_where ~size ~per_byte] is a byte span of [size] bytes where each
    byte must satisfy [per_byte]. The argument to [per_byte] is an expression
    bound to the current byte's integer value. Decode raises
    {!exception:Parse_error} on the first byte that violates the constraint;
    encode raises [Invalid_argument]. Encoding otherwise follows {!byte_array}:
    the value's length must equal [size]. *)

val synth_name_of_elt_var : string -> string
(** [synth_name_of_elt_var ev] is the 3D-side synthesised refinement-typedef
    name derived from a {!byte_array_where} element variable. Internal: used by
    the EverParse projection. *)

val index_bound_elt : 'a typ -> (string * bool expr) option
(** [index_bound_elt elem] is [Some (elt_var, cond)] when [elem] is a 1-byte
    array / repeat element carrying a lookup index bound: it then projects like
    a {!byte_array_where} whose synthesised element struct is named
    [synth_name_of_elt_var elt_var] and refines its byte with [cond]. Internal:
    used by the EverParse projection. *)

val byte_slice : size:int expr -> Bytesrw.Bytes.Slice.t typ
(** Zero-copy byte span. Encoding a slice whose length differs from [size]
    raises [Invalid_argument]. *)

val optional : bool expr -> 'a typ -> 'a option typ
(** Conditionally present field. *)

val optional_or : bool expr -> default:'a -> 'a typ -> 'a typ
(** Conditionally present field with default when absent. *)

val repeat : size:int expr -> 'a typ -> 'a list typ
(** Repeated elements filling a byte budget. *)

val repeat_seq : ('a, 'seq) seq_map -> size:int expr -> 'a typ -> 'seq typ
(** Repeated elements with custom builder. *)

val nested : size:int expr -> 'a typ -> 'a typ
(** Single element in a sized region (exact fit). *)

val nested_at_most : size:int expr -> 'a typ -> 'a typ
(** Single element in a sized region (may be smaller). *)

val enum : string -> (string * int) list -> 'a typ -> 'a typ
(** Named enumeration over an integer-valued base. Raises [Invalid_argument] on
    a base with no integer view. *)

val enum_open : string -> (string * int) list -> 'a typ -> 'a typ
(** Open enumeration: the named codes are declared in the 3D projection for
    documentation, but any value is accepted (no membership refinement, no
    decode rejection). *)

val variants : string -> (string * 'a) list -> 'b typ -> 'a typ
(** Named variant mapping over an integer-valued base. Raises [Invalid_argument]
    on a base with no integer view. *)

type ('a, 'k) case_def
(** A casetype branch definition. ['k] is the discriminator type. *)

val case :
  ?index:'k ->
  'w typ ->
  inject:('w -> 'a) ->
  project:('a -> 'w option) ->
  ('a, 'k) case_def
(** A branch matching a specific tag value. *)

val default :
  'w typ ->
  inject:('k -> 'w -> 'a) ->
  project:('a -> ('k * 'w) option) ->
  ('a, 'k) case_def
(** [default inner ~inject ~project] is the default branch of a casetype. On
    decode it matches any tag the explicit cases didn't claim, and [inject]
    receives that matched tag together with the decoded body, so the value can
    record which unclaimed tag it caught. On encode, [project] yields the tag to
    write back along with the body, so an arbitrary unclaimed tag round-trips
    (there is no fixed encode tag). *)

val casetype : string -> 'k typ -> ('a, 'k) case_def list -> 'a typ
(** [casetype name tag defs] is a tag-dispatched union. Every case must supply
    an explicit [~index]; the discriminator type ['k] can be an integer, a
    string, or any other typ with decidable equality. *)

val split_string_casetype_fields : struct_ -> struct_
(** [split_string_casetype_fields s] rewrites each {!constructor:Casetype} field
    of [s] whose tag is not an int-shaped typ into two adjacent byte fields: the
    tag bytes and a trailing {!all_bytes} body. Used by the 3D projection so
    string-tagged casetype dispatch becomes caller code instead of parser code,
    matching how real protocol implementations like OpenSSH split the parse and
    dispatch steps. *)

(** {1 Struct Constructors} *)

val field :
  string ->
  ?constraint_:bool expr ->
  ?action:action ->
  ?doc:string ->
  'a typ ->
  field
(** Declare a named field. [?doc] is rendered as a [/* ... */] comment above the
    field in the 3D projection. *)

val anon_field : 'a typ -> field
(** Declare an anonymous (padding) field. *)

val struct_ : string -> field list -> struct_
(** Construct a struct from fields. *)

val struct_name : struct_ -> string
(** Name of the struct declaration. *)

val field_names : struct_ -> string list
(** Named field names in declaration order. *)

val struct_project : struct_ -> name:string -> keep:field list -> struct_
(** [struct_project s ~name ~keep] keeps only the fields in [keep], making all
    others anonymous. *)

type ocaml_kind = Int | Int64 | Float32 | Float64 | Bool | String | Unit

val field_kinds : struct_ -> (string * ocaml_kind) list
(** Return the struct name. *)

val struct_typ : struct_ -> unit typ
(** Return the struct wrapped as a type. *)

val param : string -> 'a typ -> param
(** Declare an immutable parameter. *)

val mutable_param : string -> 'a typ -> param
(** Declare a mutable parameter. *)

val param_struct :
  string -> param list -> ?where:bool expr -> field list -> struct_
(** Construct a parameterised struct. *)

val apply : 'a typ -> int expr list -> 'a typ
(** Apply arguments to a parameterised type. *)

val type_ref : string -> 'a typ
(** Reference a type by name. *)

val qualified_ref : string -> string -> 'a typ
(** Reference a type by module and name. *)

(** {1 Action Constructors} *)

val on_success : action_stmt list -> action
(** Wrap statements as an on-success action. *)

val on_act : action_stmt list -> action
(** Wrap statements as an on-act action. *)

val assign : ('a, param_output) param_handle -> int expr -> action_stmt
(** Assign to a mutable parameter. *)

val return_bool : bool expr -> action_stmt
(** Return a boolean result. *)

val abort : action_stmt
(** Abort parsing. *)

val action_if :
  bool expr -> action_stmt list -> action_stmt list option -> action_stmt
(** Conditional action. *)

val var : string -> int expr -> action_stmt
(** Declare a local variable. *)

(** {1 Module-level Declarations} *)

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

val typedef :
  ?entrypoint:bool ->
  ?export:bool ->
  ?output:bool ->
  ?extern_:bool ->
  ?doc:string ->
  struct_ ->
  decl
(** Create a typedef declaration. *)

val define : string -> int -> decl
(** [define name value] creates a [#define] constant. *)

val extern_fn : string -> param list -> 'a typ -> decl
(** Declare an extern function. *)

val extern_probe : ?init:bool -> string -> decl
(** Declare an extern probe. *)

val enum_decl : string -> (string * int) list -> 'a typ -> decl
(** Declare an enum type. *)

type decl_case = packed_expr option * packed_typ
(** A case branch in a casetype declaration. *)

val decl_case : int -> 'a typ -> decl_case
(** A case branch matching a tag value. *)

val decl_default : 'a typ -> decl_case
(** A default case branch. *)

val casetype_decl : string -> param list -> 'a typ -> decl_case list -> decl
(** Declare a casetype. *)

type module_ = { doc : string option; decls : decl list }
(** A 3D module. *)

val module_ : ?doc:string -> decl list -> module_
(** Build a module from declarations. *)

(** {1 Pretty Printing} *)

val pp_expr : Format.formatter -> 'a expr -> unit
(** Pretty-print an expression. *)

val pp_typ : Format.formatter -> 'a typ -> unit
(** Pretty-print a type. *)

val check_where_encode : bool expr -> bool -> unit
(** [check_where_encode cond ok] raises [Invalid_argument] when [ok] is [false],
    naming [cond] in the message. Used by the encoders to refuse a value its own
    [where] refinement rejects. *)

val pp_action : Format.formatter -> action -> unit
(** Pretty-print an action block. *)

val pp_module : Format.formatter -> module_ -> unit
(** Pretty-print a module as 3D source. *)

val to_3d : ?enum_as_type:bool -> module_ -> string
(** Render a module as a 3D source string. With [~enum_as_type:true] (the
    documentation projection) an enum field renders as its named 3D enum type
    rather than the base type plus a membership refinement. *)

val to_3d_file : ?enum_as_type:bool -> string -> module_ -> unit
(** [to_3d_file path m] writes module [m] to a [.3d] file at [path]. See
    {!to_3d} for [enum_as_type]. *)

val escape_3d : string -> string
(** [escape_3d name] appends [_] if [name] is a 3D or C reserved word. *)

(** {1:parse-errors Parse Errors}

    The {!predicate}, {!error_kind}, and {!parse_error} types are declared near
    the top of this interface. *)

exception Parse_error of parse_error

val raise_error : at:int -> error_kind -> 'a
(** Raise {!Parse_error} with an empty field path. *)

val raise_eof : at:int -> expected:int -> got:int -> 'a
(** Raise {!Parse_error} for unexpected end-of-input. *)

val raise_invalid_tag : at:int -> int -> 'a
(** Raise {!Parse_error} for a tag or lookup index with no matching case. *)

val map_decode : index_bound:int option -> ('w -> 'a) -> at:int -> 'w -> 'a
(** [map_decode ~index_bound decode ~at v] is [decode v], with any
    {!Parse_error} it raises moved to [at]. A {!constructor-Map}'s [decode] sees
    only the value, so the bound a {!cases} map enforces can only be reported at
    offset 0; [at] is the absolute offset the reader took [v] from, which is
    what {!parse_error} promises. Maps with no [index_bound] reject nothing and
    are called directly. *)

val raise_invalid_enum : at:int -> value:int -> valid:int list -> 'a
(** Raise {!Parse_error} for an enum value outside its named set. *)

val raise_missing_terminator : at:int -> 'a
(** Raise {!Parse_error} for a NUL-terminated string with no terminator. *)

val raise_non_zero_padding : at:int -> 'a
(** Raise {!Parse_error} for a non-zero byte where zero padding is required. *)

val raise_out_of_range : at:int -> int64 -> 'a
(** Raise {!Parse_error} for an integer beyond the native integer range. *)

val raise_zero_divisor : at:int -> 'a
(** Raise {!Parse_error} for division or modulo by zero. *)

val raise_constraint : at:int -> which:predicate -> ?value:int64 -> unit -> 'a
(** Raise {!Parse_error} for a violated predicate [which], optionally carrying
    the offending field's [value]. *)

val pp_parse_error : Format.formatter -> parse_error -> unit
(** Pretty-print a parse error. *)

val pp_error_kind : Format.formatter -> error_kind -> unit
(** Pretty-print a parse error kind, without location. *)

val equal_parse_error : parse_error -> parse_error -> bool
(** Structural equality on parse errors. *)

val equal_error_kind : error_kind -> error_kind -> bool
(** Structural equality on parse error kinds, ignoring location. *)

val compare_parse_error : parse_error -> parse_error -> int
(** Total order on parse errors. *)

val field_wire_size : 'a typ -> int option
(** Fixed wire size of a field type, if determinable. *)

val is_greedy : 'a typ -> bool
(** Whether a type reads "the rest of the buffer" (a greedy byte span, through
    transparent wrappers). Such a field is only valid as the last field of a
    struct or codec. *)

val ends_greedy : 'a typ -> bool
(** Like {!is_greedy} but through composition: a sub-codec or casetype case
    whose own tail is greedy reads to the end of the buffer just the same. Such
    a type can only be the last thing in the buffer, so nothing may follow it
    and no container may iterate it. *)

val struct_ends_greedy : struct_ -> bool
(** [struct_ends_greedy s] is {!ends_greedy} of [s]'s last field. *)

val nz : 'a typ -> bool
(** Whether the type's parser always consumes a positive minimum number of bytes
    (EverParse's [nz] parser-kind index). [false] for a byte span, a
    {!val-nested} region, an optional, or an all-bytes / all-zeros tail: their
    parser may consume zero bytes regardless of any positive constant size. A
    struct/codec is [nz] iff one of its fields is; a casetype iff its tag or
    every case body is. EverParse's byte-budget array ([T_nlist]) rejects a
    non-[nz] element, so a sub-codec must be [nz] to be an {!val-array} /
    {!val-repeat} element. *)

val struct_nz : struct_ -> bool
(** [struct_nz s] is [true] iff some field of [s] is {!nz}, i.e. the struct's
    parser has a positive minimum size. *)

val size_of_typ_value : eval_ctx -> 'a typ -> 'a -> int
(** [size_of_typ_value ctx typ v] is the encoded byte size of [v] under [typ],
    computed from the value rather than from a buffer. A size that names an
    input parameter resolves through [ctx]; one that names a sibling field or a
    position does not, since a value on its own does not carry the record around
    it, and a typ measured only by such a size raises [Invalid_argument] rather
    than reporting [0] for a field the encoder will fill. Also raises for a
    casetype value no case projects, which encoding refuses for the same reason.
*)

val c_type_of : 'a typ -> string
(** [c_type_of typ] returns the C type name (e.g., ["uint8_t"], ["uint32_t"]).
*)

val ml_type_of : 'a typ -> string
(** [ml_type_of typ] returns the OCaml type name for FFI stub generation:
    ["int"] for integer types that fit in OCaml integers, ["int64"] for uint64.
*)

(** {1 Seed values} *)

type int_slot = { width : int; endian : endian }
(** Byte width and order of a whole-byte integer field on the wire. *)

type field_seed = { field : string; slot : int_slot; values : int64 list }
(** A field whose own declaration singles out particular wire values. *)

val int_slots : struct_ -> (string * int_slot) list
(** [int_slots s] is the byte slot of every named whole-byte integer field of
    [s], in declaration order. Bitfields (their base word is shared), floats,
    byte spans and composites have no slot of their own and are left out.
    {!field_seeds} is this same set narrowed to the fields whose declaration
    singles out particular values. *)

val field_seeds : struct_ -> field_seed list
(** [field_seeds s] is, for each named whole-byte integer field of [s] whose any
    refinement in the record compares against a value, the byte slot it occupies
    and those values: the constant an equality or inequality names, the values
    either side of an ordering boundary, and the members of a closed
    enumeration. A refinement is credited to the field it compares rather than
    to the field carrying it, and a struct-level [where] is read where the
    projection puts it, so a field's values are the ones the generated validator
    tests it against. A casetype field seeds too: its tag is parsed at the start
    of the field's own bytes, so the slot is the tag's and the values are the
    case indices. Fields with no such value, and fields that are not whole-byte
    integers (bitfields in particular, whose base word is shared), are omitted.

    The list over-approximates: it names candidates worth trying, not values the
    description is guaranteed to admit, so a consumer must run each through the
    codec to learn the verdict. It exists so a generated corpus can reach the
    accepting side of a constraint that drawing bytes never would -- a magic
    number leaves one accepting value in [2^32]. *)
