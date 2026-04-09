(** Core type definitions for the Wire DSL. *)

type endian = Little | Big  (** Byte order. *)

type bit_order =
  | Msb_first
  | Lsb_first
      (** Which end of a packed base word the first declared bitfield occupies.

          [Msb_first] places the first declared field at the most significant
          bit, matching how RFC, CCSDS, and IETF specs draw their bit diagrams.
          [Lsb_first] places it at bit 0, matching MSVC C bit-field packing.

          Bit order is independent of byte order. Every combination of base word
          and bit order is a valid wire description. When projecting to
          EverParse 3D, the export layer reverses field declaration order within
          a bit group if the user's bit order differs from EverParse's native
          choice for the base; this preserves byte layout in memory and is
          invisible to the user. *)

(** {1 Sequence builders} *)

type ('elt, 'seq) seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map
      (** Builder for sequence accumulation. Existentially hides the builder
          type. *)

val seq_list : ('a, 'a list) seq_map
(** Default builder: accumulate into a list. *)

(** {1 Param handles} *)

type param_input
type param_output

type ('a, 'k) param_handle = {
  ph_name : string;
  ph_typ : 'a typ;
  ph_packed_typ : packed_typ;
  ph_mutable : bool;
  ph_cell : int ref;
  mutable ph_slot : int;
  mutable ph_env_idx : int;
}

and packed_typ = Pack_typ : 'a typ -> packed_typ

(** {1 Expressions}

    Typed expression language used in constraints, actions and size
    computations. Arithmetic and bitwise operators mirror OCaml conventions. *)

and _ expr =
  | Int : int -> int expr
  | Int64 : int64 -> int64 expr
  | Bool : bool -> bool expr
  | Ref : string -> int expr
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
  | Lt : int expr * int expr -> bool expr
  | Le : int expr * int expr -> bool expr
  | Gt : int expr * int expr -> bool expr
  | Ge : int expr * int expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  | Cast : [ `U8 | `U16 | `U32 | `U64 ] * int expr -> int expr

(** {1 Types} *)

and bitfield_base =
  | BF_U8
  | BF_U16 of endian
  | BF_U32 of endian  (** Base storage for bitfield extractions. *)

and _ typ =
  | Uint8 : int typ  (** 8-bit unsigned. *)
  | Uint16 : endian -> int typ  (** 16-bit unsigned. *)
  | Uint32 : endian -> UInt32.t typ  (** 32-bit unsigned. *)
  | Uint63 : endian -> UInt63.t typ  (** 63-bit unsigned. *)
  | Uint64 : endian -> int64 typ  (** 64-bit unsigned. *)
  | Bits : {
      width : int;
      base : bitfield_base;
      bit_order : bit_order;
    }
      -> int typ  (** Bitfield. *)
  | Unit : unit typ  (** Zero-width. *)
  | All_bytes : string typ  (** Remaining bytes as string. *)
  | All_zeros : string typ  (** Remaining bytes, must be zero. *)
  | Where : { cond : bool expr; inner : 'a typ } -> 'a typ  (** Guarded. *)
  | Array : {
      len : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ  (** Fixed-count array. *)
  | Byte_array : { size : int expr } -> string typ  (** Byte span as string. *)
  | Byte_slice : { size : int expr } -> Bytesrw.Bytes.Slice.t typ
      (** Zero-copy byte span. *)
  | Single_elem : { size : int expr; elem : 'a typ; at_most : bool } -> 'a typ
      (** Single element in a sized region. *)
  | Enum : {
      name : string;
      cases : (string * int) list;
      base : int typ;
    }
      -> int typ  (** Named enumeration. *)
  | Casetype : {
      name : string;
      tag : 'tag typ;
      cases : ('tag option * 'a typ) list;
    }
      -> 'a typ  (** Tag-dispatched union. *)
  | Struct : struct_ -> unit typ  (** Nested struct. *)
  | Type_ref : string -> 'a typ  (** Forward reference by name. *)
  | Qualified_ref : { module_ : string; name : string } -> 'a typ
      (** Qualified reference. *)
  | Map : { inner : 'w typ; decode : 'w -> 'a; encode : 'a -> 'w } -> 'a typ
      (** Mapped type. *)
  | Apply : { typ : 'a typ; args : packed_expr list } -> 'a typ
      (** Parameterised type application. *)
  | Codec : {
      codec_name : string;
      codec_decode : bytes -> int -> 'r;
      codec_encode : 'r -> bytes -> int -> unit;
      codec_fixed_size : int option;
      codec_size_of : bytes -> int -> int;
      codec_field_readers : (string * (bytes -> int -> int)) list;
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

and packed_expr =
  | Pack_expr : 'a expr -> packed_expr  (** Existentially packed expression. *)

and struct_ = {
  name : string;
  params : param list;
  where : bool expr option;
  fields : field list;
}
(** Struct declaration. *)

and field =
  | Field : {
      field_name : string option;
      field_typ : 'a typ;
      constraint_ : bool expr option;
      action : action option;
    }
      -> field  (** A single struct field. *)

and param = { param_name : string; param_typ : packed_typ; mutable_ : bool }
(** Formal parameter. *)

and action =
  | On_success of action_stmt list
  | On_act of action_stmt list  (** Action attached to a field. *)

and action_stmt =
  | Assign : ('a, param_output) param_handle * int expr -> action_stmt
  | Field_assign of string * string * int expr
  | Extern_call of string * string list
  | Return of bool expr
  | Abort
  | If of bool expr * action_stmt list * action_stmt list option
  | Var of string * int expr  (** Action statement. *)

type param_env = { pe_codec_id : int; pe_slots : int array }

(** {1 Expression Constructors} *)

val int : int -> int expr
(** Integer literal. *)

val int64 : int64 -> int64 expr
(** 64-bit integer literal. *)

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

  val ( < ) : int expr -> int expr -> bool expr
  (** Strictly less than. *)

  val ( <= ) : int expr -> int expr -> bool expr
  (** Less than or equal. *)

  val ( > ) : int expr -> int expr -> bool expr
  (** Strictly greater than. *)

  val ( >= ) : int expr -> int expr -> bool expr
  (** Greater than or equal. *)

  (** {2 Boolean operators} *)

  val ( && ) : bool expr -> bool expr -> bool expr
  (** Logical AND. *)

  val ( || ) : bool expr -> bool expr -> bool expr
  (** Logical OR. *)

  val not : bool expr -> bool expr
  (** Logical NOT. *)

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

(** {1 Type Constructors} *)

val uint8 : int typ
(** 8-bit unsigned, native endian. *)

val uint16 : int typ
(** 16-bit unsigned, little-endian. *)

val uint16be : int typ
(** 16-bit unsigned, big-endian. *)

val uint32 : UInt32.t typ
(** 32-bit unsigned, little-endian. *)

val uint32be : UInt32.t typ
(** 32-bit unsigned, big-endian. *)

val uint63 : UInt63.t typ
(** 63-bit unsigned, little-endian. *)

val uint63be : UInt63.t typ
(** 63-bit unsigned, big-endian. *)

val uint64 : int64 typ
(** 64-bit unsigned, little-endian. *)

val uint64be : int64 typ
(** 64-bit unsigned, big-endian. *)

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
(** [bits ~width base] extracts [width] bits from a bitfield base. [bit_order]
    defaults to {!Msb_first}. *)

val map : ('w -> 'a) -> ('a -> 'w) -> 'w typ -> 'a typ
(** Map a wire type to a different OCaml type. *)

val bool : int typ -> bool typ
(** Map an integer type to boolean (0 = false). *)

val cases : 'a list -> int typ -> 'a typ
(** Map integer values to a list of cases by index. *)

val unit : unit typ
(** Zero-width unit type. *)

val all_bytes : string typ
(** Consume all remaining bytes. *)

val all_zeros : string typ
(** Consume remaining bytes, asserting all zero. *)

val where : bool expr -> 'a typ -> 'a typ
(** Guard a type with a boolean constraint. *)

val array : len:int expr -> 'a typ -> 'a list typ
(** Fixed-count array of elements. *)

val array_seq : ('a, 'seq) seq_map -> len:int expr -> 'a typ -> 'seq typ
(** Fixed-count array with custom builder. *)

val byte_array : size:int expr -> string typ
(** Byte span as a string. *)

val byte_slice : size:int expr -> Bytesrw.Bytes.Slice.t typ
(** Zero-copy byte span. *)

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

val enum : string -> (string * int) list -> int typ -> int typ
(** Named enumeration over an integer base. *)

val variants : string -> (string * 'a) list -> int typ -> 'a typ
(** Named variant mapping over an integer base. *)

type ('tag, 'a) case = 'tag option * 'a typ
(** A casetype branch. *)

val case : 'tag -> 'a typ -> ('tag, 'a) case
(** A branch matching a specific tag value. *)

val default : 'a typ -> ('tag, 'a) case
(** A default branch. *)

val casetype : string -> 'tag typ -> ('tag, 'a) case list -> 'a typ
(** Tag-dispatched union type. *)

(** {1 Struct Constructors} *)

val field :
  string -> ?constraint_:bool expr -> ?action:action -> 'a typ -> field
(** Declare a named field. *)

val anon_field : 'a typ -> field
(** Declare an anonymous (padding) field. *)

val struct_ : string -> field list -> struct_
(** Construct a struct from fields. *)

val struct_name : struct_ -> string

val field_names : struct_ -> string list
(** Named field names in declaration order. *)

val struct_project : struct_ -> name:string -> keep:field list -> struct_
(** [struct_project s ~name ~keep] keeps only the fields in [keep], making all
    others anonymous. *)

type ocaml_kind = K_int | K_int64 | K_bool | K_string | K_unit

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

val pp_action : Format.formatter -> action -> unit
(** Pretty-print an action block. *)

val pp_module : Format.formatter -> module_ -> unit
(** Pretty-print a module as 3D source. *)

val to_3d : module_ -> string
(** Render a module as a 3D source string. *)

val to_3d_file : string -> module_ -> unit
(** [to_3d_file path m] writes module [m] to a [.3d] file at [path]. *)

(** {1 Parse Errors} *)

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }  (** Structured parse error. *)

exception Parse_error of parse_error

val raise_eof : expected:int -> got:int -> 'a
(** Raise {!Parse_error} for unexpected end-of-input. *)

val pp_parse_error : Format.formatter -> parse_error -> unit
(** Pretty-print a parse error. *)

val field_wire_size : 'a typ -> int option
(** Fixed wire size of a field type, if determinable. *)

val c_type_of : 'a typ -> string
(** [c_type_of typ] returns the C type name (e.g., ["uint8_t"], ["uint32_t"]).
*)

val ml_type_of : 'a typ -> string
(** [ml_type_of typ] returns the OCaml type name for FFI stub generation:
    ["int"] for integer types that fit in OCaml [int], ["int64"] for uint64. *)
