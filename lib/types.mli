type endian = Little | Big

type _ expr =
  | Int : int -> int expr
  | Int64 : int64 -> int64 expr
  | Bool : bool -> bool expr
  | Ref : string -> int expr
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

and bitfield_base = BF_U8 | BF_U16 of endian | BF_U32 of endian

and _ typ =
  | Uint8 : int typ
  | Uint16 : endian -> int typ
  | Uint32 : endian -> UInt32.t typ
  | Uint63 : endian -> UInt63.t typ
  | Uint64 : endian -> int64 typ
  | Bits : { width : int; base : bitfield_base } -> int typ
  | Unit : unit typ
  | All_bytes : string typ
  | All_zeros : string typ
  | Where : { cond : bool expr; inner : 'a typ } -> 'a typ
  | Array : { len : int expr; elem : 'a typ } -> 'a list typ
  | Byte_array : { size : int expr } -> string typ
  | Byte_slice : { size : int expr } -> Bytesrw.Bytes.Slice.t typ
  | Single_elem : { size : int expr; elem : 'a typ; at_most : bool } -> 'a typ
  | Enum : {
      name : string;
      cases : (string * int) list;
      base : int typ;
    }
      -> int typ
  | Casetype : {
      name : string;
      tag : 'tag typ;
      cases : ('tag option * 'a typ) list;
    }
      -> 'a typ
  | Struct : struct_ -> unit typ
  | Type_ref : string -> 'a typ
  | Qualified_ref : { module_ : string; name : string } -> 'a typ
  | Map : { inner : 'w typ; decode : 'w -> 'a; encode : 'a -> 'w } -> 'a typ
  | Apply : { typ : 'a typ; args : packed_expr list } -> 'a typ

and packed_expr = Pack_expr : 'a expr -> packed_expr

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
    }
      -> field

and param = { param_name : string; param_typ : packed_typ; mutable_ : bool }
and packed_typ = Pack_typ : 'a typ -> packed_typ
and action = On_success of action_stmt list | On_act of action_stmt list

and action_stmt =
  | Assign of string * int expr
  | Return of bool expr
  | Abort
  | If of bool expr * action_stmt list * action_stmt list option
  | Var of string * int expr

val int : int -> int expr
val int64 : int64 -> int64 expr
val true_ : bool expr
val false_ : bool expr
val ref : string -> int expr
val sizeof : 'a typ -> int expr
val sizeof_this : int expr
val field_pos : int expr

module Expr : sig
  val ( + ) : int expr -> int expr -> int expr
  val ( - ) : int expr -> int expr -> int expr
  val ( * ) : int expr -> int expr -> int expr
  val ( / ) : int expr -> int expr -> int expr
  val ( mod ) : int expr -> int expr -> int expr
  val ( land ) : int expr -> int expr -> int expr
  val ( lor ) : int expr -> int expr -> int expr
  val ( lxor ) : int expr -> int expr -> int expr
  val lnot : int expr -> int expr
  val ( lsl ) : int expr -> int expr -> int expr
  val ( lsr ) : int expr -> int expr -> int expr
  val ( = ) : 'a expr -> 'a expr -> bool expr
  val ( <> ) : 'a expr -> 'a expr -> bool expr
  val ( < ) : int expr -> int expr -> bool expr
  val ( <= ) : int expr -> int expr -> bool expr
  val ( > ) : int expr -> int expr -> bool expr
  val ( >= ) : int expr -> int expr -> bool expr
  val ( && ) : bool expr -> bool expr -> bool expr
  val ( || ) : bool expr -> bool expr -> bool expr
  val not : bool expr -> bool expr
  val to_uint8 : int expr -> int expr
  val to_uint16 : int expr -> int expr
  val to_uint32 : int expr -> int expr
  val to_uint64 : int expr -> int expr
end

val uint8 : int typ
val uint16 : int typ
val uint16be : int typ
val uint32 : UInt32.t typ
val uint32be : UInt32.t typ
val uint63 : UInt63.t typ
val uint63be : UInt63.t typ
val uint64 : int64 typ
val uint64be : int64 typ
val bf_uint8 : bitfield_base
val bf_uint16 : bitfield_base
val bf_uint16be : bitfield_base
val bf_uint32 : bitfield_base
val bf_uint32be : bitfield_base
val bits : width:int -> bitfield_base -> int typ
val map : ('w -> 'a) -> ('a -> 'w) -> 'w typ -> 'a typ
val bool : int typ -> bool typ
val cases : 'a list -> int typ -> 'a typ
val unit : unit typ
val all_bytes : string typ
val all_zeros : string typ
val where : bool expr -> 'a typ -> 'a typ
val array : len:int expr -> 'a typ -> 'a list typ
val byte_array : size:int expr -> string typ
val byte_slice : size:int expr -> Bytesrw.Bytes.Slice.t typ
val single_elem_array : size:int expr -> 'a typ -> 'a typ
val single_elem_array_at_most : size:int expr -> 'a typ -> 'a typ
val enum : string -> (string * int) list -> int typ -> int typ
val variants : string -> (string * 'a) list -> int typ -> 'a typ

type ('tag, 'a) case = 'tag option * 'a typ

val case : 'tag -> 'a typ -> ('tag, 'a) case
val default : 'a typ -> ('tag, 'a) case
val casetype : string -> 'tag typ -> ('tag, 'a) case list -> 'a typ

val field :
  string -> ?constraint_:bool expr -> ?action:action -> 'a typ -> field

val anon_field : 'a typ -> field
val struct_ : string -> field list -> struct_
val struct_name : struct_ -> string
val struct_typ : struct_ -> unit typ
val param : string -> 'a typ -> param
val mutable_param : string -> 'a typ -> param

val param_struct :
  string -> param list -> ?where:bool expr -> field list -> struct_

val apply : 'a typ -> int expr list -> 'a typ
val type_ref : string -> 'a typ
val qualified_ref : string -> string -> 'a typ
val on_success : action_stmt list -> action
val on_act : action_stmt list -> action
val assign : string -> int expr -> action_stmt
val return_bool : bool expr -> action_stmt
val abort : action_stmt

val action_if :
  bool expr -> action_stmt list -> action_stmt list option -> action_stmt

val var : string -> int expr -> action_stmt

type decl =
  | Typedef of {
      entrypoint : bool;
      export : bool;
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

val typedef : ?entrypoint:bool -> ?export:bool -> ?doc:string -> struct_ -> decl
val define : string -> int -> decl
val extern_fn : string -> param list -> 'a typ -> decl
val extern_probe : ?init:bool -> string -> decl
val enum_decl : string -> (string * int) list -> 'a typ -> decl

type decl_case = packed_expr option * packed_typ

val decl_case : int -> 'a typ -> decl_case
val decl_default : 'a typ -> decl_case
val casetype_decl : string -> param list -> 'a typ -> decl_case list -> decl

type module_ = { doc : string option; decls : decl list }

val module_ : ?doc:string -> decl list -> module_
val pp_expr : Format.formatter -> 'a expr -> unit
val pp_typ : Format.formatter -> 'a typ -> unit
val pp_module : Format.formatter -> module_ -> unit
val to_3d : module_ -> string
val to_3d_file : string -> module_ -> unit

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }

exception Parse_error of parse_error

val raise_eof : expected:int -> got:int -> 'a
val pp_parse_error : Format.formatter -> parse_error -> unit
val field_wire_size : 'a typ -> int option
