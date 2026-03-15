(* Wire: Dependent Data Descriptions for EverParse 3D *)

open Result.Syntax

(** Staged computations, following the pattern from Jane Street's Base library.
    Forces users to explicitly unstage functions to make specialization visible.
    See also Irmin's repr which uses the same pattern. *)
module Staged = struct
  type +'a t = { unstage : 'a } [@@unboxed]

  let stage x = { unstage = x }
  let unstage { unstage } = unstage
end

(* UInt32: unboxed on 64-bit (uses int), boxed on 32-bit (uses int32) *)
module UInt32 = struct
  type t = int (* On 64-bit, int is 63 bits - enough for uint32 *)

  let () =
    if Sys.int_size < 32 then
      failwith "Wire.UInt32 requires 64-bit OCaml (int must be >= 32 bits)"

  let get_le buf off =
    let b0 = Bytes.get_uint8 buf off in
    let b1 = Bytes.get_uint8 buf (off + 1) in
    let b2 = Bytes.get_uint8 buf (off + 2) in
    let b3 = Bytes.get_uint8 buf (off + 3) in
    b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

  let get_be buf off =
    let b0 = Bytes.get_uint8 buf off in
    let b1 = Bytes.get_uint8 buf (off + 1) in
    let b2 = Bytes.get_uint8 buf (off + 2) in
    let b3 = Bytes.get_uint8 buf (off + 3) in
    (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

  let mask v = v land ((1 lsl 32) - 1)

  let set_le buf off v =
    let v = mask v in
    Bytes.set_uint8 buf off (v land 0xFF);
    Bytes.set_uint8 buf (off + 1) ((v lsr 8) land 0xFF);
    Bytes.set_uint8 buf (off + 2) ((v lsr 16) land 0xFF);
    Bytes.set_uint8 buf (off + 3) ((v lsr 24) land 0xFF)

  let set_be buf off v =
    let v = mask v in
    Bytes.set_uint8 buf off ((v lsr 24) land 0xFF);
    Bytes.set_uint8 buf (off + 1) ((v lsr 16) land 0xFF);
    Bytes.set_uint8 buf (off + 2) ((v lsr 8) land 0xFF);
    Bytes.set_uint8 buf (off + 3) (v land 0xFF)

  let to_int t = t
  let of_int t = mask t
end

(* UInt63: unboxed on 64-bit (uses int), reads 8 bytes but masks to 63 bits *)
module UInt63 = struct
  type t = int (* 63-bit int on 64-bit platforms *)

  let () =
    if Sys.int_size < 63 then
      failwith "Wire.UInt63 requires 64-bit OCaml (int must be 63 bits)"

  let get_le buf off =
    let b0 = Bytes.get_uint8 buf off in
    let b1 = Bytes.get_uint8 buf (off + 1) in
    let b2 = Bytes.get_uint8 buf (off + 2) in
    let b3 = Bytes.get_uint8 buf (off + 3) in
    let b4 = Bytes.get_uint8 buf (off + 4) in
    let b5 = Bytes.get_uint8 buf (off + 5) in
    let b6 = Bytes.get_uint8 buf (off + 6) in
    let b7 = Bytes.get_uint8 buf (off + 7) in
    b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) lor (b4 lsl 32)
    lor (b5 lsl 40) lor (b6 lsl 48)
    lor ((b7 land 0x7F) lsl 56)

  let get_be buf off =
    let b0 = Bytes.get_uint8 buf off in
    let b1 = Bytes.get_uint8 buf (off + 1) in
    let b2 = Bytes.get_uint8 buf (off + 2) in
    let b3 = Bytes.get_uint8 buf (off + 3) in
    let b4 = Bytes.get_uint8 buf (off + 4) in
    let b5 = Bytes.get_uint8 buf (off + 5) in
    let b6 = Bytes.get_uint8 buf (off + 6) in
    let b7 = Bytes.get_uint8 buf (off + 7) in
    ((b0 land 0x7F) lsl 56)
    lor (b1 lsl 48) lor (b2 lsl 40) lor (b3 lsl 32) lor (b4 lsl 24)
    lor (b5 lsl 16) lor (b6 lsl 8) lor b7

  let set_le buf off v =
    Bytes.set_uint8 buf off (v land 0xFF);
    Bytes.set_uint8 buf (off + 1) ((v lsr 8) land 0xFF);
    Bytes.set_uint8 buf (off + 2) ((v lsr 16) land 0xFF);
    Bytes.set_uint8 buf (off + 3) ((v lsr 24) land 0xFF);
    Bytes.set_uint8 buf (off + 4) ((v lsr 32) land 0xFF);
    Bytes.set_uint8 buf (off + 5) ((v lsr 40) land 0xFF);
    Bytes.set_uint8 buf (off + 6) ((v lsr 48) land 0xFF);
    Bytes.set_uint8 buf (off + 7) ((v lsr 56) land 0x7F)

  let set_be buf off v =
    Bytes.set_uint8 buf off ((v lsr 56) land 0x7F);
    Bytes.set_uint8 buf (off + 1) ((v lsr 48) land 0xFF);
    Bytes.set_uint8 buf (off + 2) ((v lsr 40) land 0xFF);
    Bytes.set_uint8 buf (off + 3) ((v lsr 32) land 0xFF);
    Bytes.set_uint8 buf (off + 4) ((v lsr 24) land 0xFF);
    Bytes.set_uint8 buf (off + 5) ((v lsr 16) land 0xFF);
    Bytes.set_uint8 buf (off + 6) ((v lsr 8) land 0xFF);
    Bytes.set_uint8 buf (off + 7) (v land 0xFF)

  let to_int t = t
  let of_int t = t
end

type endian = Little | Big

(* Expressions *)
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

(* Bitfield base types - standalone, not mutually recursive *)
and bitfield_base = BF_U8 | BF_U16 of endian | BF_U32 of endian

(* Types *)
and _ typ =
  | Uint8 : int typ
  | Uint16 : endian -> int typ
  | Uint32 : endian -> UInt32.t typ
  | Uint63 : endian -> UInt63.t typ
  | Uint64 : endian -> int64 typ (* boxed, for full 64-bit *)
  | Bits : { width : int; base : bitfield_base } -> int typ
  | Unit : unit typ
  | All_bytes : string typ
  | All_zeros : string typ
  | Where : { cond : bool expr; inner : 'a typ } -> 'a typ
  | Array : { len : int expr; elem : 'a typ } -> 'a list typ
  | Byte_array : { size : int expr } -> string typ
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
    }
      -> field

and param = { param_name : string; param_typ : packed_typ; mutable_ : bool }
and packed_typ = Pack_typ : 'a typ -> packed_typ

(* Actions *)
and action = On_success of action_stmt list | On_act of action_stmt list

and action_stmt =
  | Assign of string * int expr
  | Return of bool expr
  | Abort
  | If of bool expr * action_stmt list * action_stmt list option
  | Var of string * int expr

(* Expression constructors *)
let int n = Int n
let int64 n = Int64 n
let true_ = Bool true
let false_ = Bool false
let ref name = Ref name
let sizeof t = Sizeof t
let sizeof_this = Sizeof_this
let field_pos = Field_pos

module Expr = struct
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

(* Bitfield bases *)
let bf_uint8 = BF_U8
let bf_uint16 = BF_U16 Little
let bf_uint16be = BF_U16 Big
let bf_uint32 = BF_U32 Little
let bf_uint32be = BF_U32 Big
let bits ~width base = Bits { width; base }
let bit b = Bool.to_int b
let is_set n = n <> 0
let map decode encode inner = Map { inner; decode; encode }
let bool inner = Map { inner; decode = is_set; encode = bit }

let cases variants inner =
  let arr = Array.of_list variants in
  let decode n =
    if n >= 0 && n < Array.length arr then arr.(n)
    else Fmt.invalid_arg "Wire.cases: unknown value %d" n
  in
  let encode v =
    let rec go i =
      if i >= Array.length arr then invalid_arg "Wire.cases: unknown variant"
      else if arr.(i) = v then i
      else go (i + 1)
    in
    go 0
  in
  Map { inner; decode; encode }

let unit = Unit
let all_bytes = All_bytes
let all_zeros = All_zeros
let where cond inner = Where { cond; inner }
let array ~len elem = Array { len; elem }
let byte_array ~size = Byte_array { size }
let single_elem_array ~size elem = Single_elem { size; elem; at_most = false }

let single_elem_array_at_most ~size elem =
  Single_elem { size; elem; at_most = true }

let enum name cases base = Enum { name; cases; base }

(* Casetype *)
type ('tag, 'a) case = 'tag option * 'a typ

let case tag typ = (Some tag, typ)
let default typ = (None, typ)
let casetype name tag cases = Casetype { name; tag; cases }

(* Struct fields *)
let field name ?constraint_ ?action typ =
  Field { field_name = Some name; field_typ = typ; constraint_; action }

let anon_field typ =
  Field
    { field_name = None; field_typ = typ; constraint_ = None; action = None }

(* Struct constructors *)
let struct_ name fields = { name; params = []; where = None; fields }
let struct_name s = s.name
let struct_typ s = Struct s

(* Parameters *)
let param name typ =
  { param_name = name; param_typ = Pack_typ typ; mutable_ = false }

let mutable_param name typ =
  { param_name = name; param_typ = Pack_typ typ; mutable_ = true }

let param_struct name params ?where fields = { name; params; where; fields }
let apply typ args = Apply { typ; args = List.map (fun e -> Pack_expr e) args }

(* Type references *)
let type_ref name = Type_ref name
let qualified_ref module_ name = Qualified_ref { module_; name }

(* Actions *)
let on_success stmts = On_success stmts
let on_act stmts = On_act stmts
let assign ptr e = Assign (ptr, e)
let return_bool e = Return e
let abort = Abort
let action_if cond then_ else_ = If (cond, then_, else_)
let var name e = Var (name, e)

(* Declarations *)
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

let typedef ?(entrypoint = false) ?(export = false) ?doc struct_ =
  Typedef { entrypoint; export; doc; struct_ }

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
type module_ = { doc : string option; name : string; decls : decl list }

let module_ ?doc name decls = { doc; name; decls }

(* Pretty printing using Fmt *)

let pp_endian ppf = function Little -> () | Big -> Fmt.string ppf "BE"

let pp_bitfield_base ppf = function
  | BF_U8 -> Fmt.string ppf "UINT8"
  | BF_U16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | BF_U32 e -> Fmt.pf ppf "UINT32%a" pp_endian e

let pp_cast_type ppf = function
  | `U8 -> Fmt.string ppf "UINT8"
  | `U16 -> Fmt.string ppf "UINT16"
  | `U32 -> Fmt.string ppf "UINT32"
  | `U64 -> Fmt.string ppf "UINT64"

let rec pp_expr : type a. a expr Fmt.t =
 fun ppf expr ->
  match expr with
  | Int n when n < 0 -> Fmt.pf ppf "(%d)" n
  | Int n -> Fmt.int ppf n
  | Int64 n -> Fmt.pf ppf "%LduL" n
  | Bool true -> Fmt.string ppf "true"
  | Bool false -> Fmt.string ppf "false"
  | Ref name -> Fmt.string ppf name
  | Sizeof t -> Fmt.pf ppf "sizeof (%a)" pp_typ t
  | Sizeof_this -> Fmt.string ppf "sizeof (this)"
  | Field_pos -> Fmt.string ppf "field_pos"
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

and pp_typ : type a. a typ Fmt.t =
 fun ppf typ ->
  match typ with
  | Uint8 -> Fmt.string ppf "UINT8"
  | Uint16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | Uint32 e -> Fmt.pf ppf "UINT32%a" pp_endian e
  | Uint63 e -> Fmt.pf ppf "UINT63%a" pp_endian e
  | Uint64 e -> Fmt.pf ppf "UINT64%a" pp_endian e
  | Bits { base; _ } -> pp_bitfield_base ppf base
  | Unit -> Fmt.string ppf "unit"
  | All_bytes -> Fmt.string ppf "all_bytes"
  | All_zeros -> Fmt.string ppf "all_zeros"
  | Where { cond; inner } -> Fmt.pf ppf "%a { %a }" pp_typ inner pp_expr cond
  | Array { len; elem } -> Fmt.pf ppf "%a[%a]" pp_typ elem pp_expr len
  | Byte_array { size } -> Fmt.pf ppf "UINT8[:byte-size %a]" pp_expr size
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

and pp_packed_expr ppf (Pack_expr e) = pp_expr ppf e

let rec pp_action_stmt ppf = function
  | Assign (ptr, e) -> Fmt.pf ppf "*%s = %a;" ptr pp_expr e
  | Return e -> Fmt.pf ppf "return %a;" pp_expr e
  | Abort -> Fmt.string ppf "abort;"
  | If (cond, then_, None) ->
      Fmt.pf ppf "if (%a) { %a }" pp_expr cond
        Fmt.(list ~sep:sp pp_action_stmt)
        then_
  | If (cond, then_, Some else_) ->
      Fmt.pf ppf "if (%a) { %a } else { %a }" pp_expr cond
        Fmt.(list ~sep:sp pp_action_stmt)
        then_
        Fmt.(list ~sep:sp pp_action_stmt)
        else_
  | Var (name, e) -> Fmt.pf ppf "var %s = %a;" name pp_expr e

let pp_action ppf = function
  | On_success stmts ->
      Fmt.pf ppf "{:on-success %a }" Fmt.(list ~sep:sp pp_action_stmt) stmts
  | On_act stmts ->
      Fmt.pf ppf "{:act %a }" Fmt.(list ~sep:sp pp_action_stmt) stmts

let pp_bitwidth : type a. a typ -> int option = function
  | Bits { width; _ } -> Some width
  | _ -> None

(* Extract field suffix for arrays - the modifier goes after the field name *)
type field_suffix =
  | No_suffix
  | Bitwidth of int
  | Byte_array of int expr
  | Single_elem of { size : int expr; at_most : bool }
  | Array of int expr

let field_suffix : type a. a typ -> field_suffix * (Format.formatter -> unit) =
 fun typ ->
  match typ with
  | Bits { width; base } ->
      (Bitwidth width, fun ppf -> pp_bitfield_base ppf base)
  | Byte_array { size } -> (Byte_array size, fun ppf -> Fmt.string ppf "UINT8")
  | Single_elem { size; elem; at_most } ->
      (Single_elem { size; at_most }, fun ppf -> pp_typ ppf elem)
  | Array { len; elem } -> (Array len, fun ppf -> pp_typ ppf elem)
  | _ -> (No_suffix, fun ppf -> pp_typ ppf typ)

let pp_field ppf (Field f) =
  match f.field_name with
  | Some name ->
      let suffix, pp_base = field_suffix f.field_typ in
      Fmt.pf ppf "@,%t %s" pp_base name;
      (* Print suffix after field name *)
      (match suffix with
      | No_suffix -> ()
      | Bitwidth w -> Fmt.pf ppf " : %d" w
      | Byte_array size -> Fmt.pf ppf "[:byte-size %a]" pp_expr size
      | Single_elem { size; at_most = false } ->
          Fmt.pf ppf "[:byte-size-single-element-array %a]" pp_expr size
      | Single_elem { size; at_most = true } ->
          Fmt.pf ppf "[:byte-size-single-element-array-at-most %a]" pp_expr size
      | Array len -> Fmt.pf ppf "[%a]" pp_expr len);
      Option.iter (Fmt.pf ppf " { %a }" pp_expr) f.constraint_;
      Option.iter (Fmt.pf ppf " %a" pp_action) f.action;
      Fmt.string ppf ";"
  | None -> Fmt.pf ppf "@,%a;" pp_typ f.field_typ

let pp_param ppf p =
  let (Pack_typ t) = p.param_typ in
  if p.mutable_ then Fmt.pf ppf "mutable %a *%s" pp_typ t p.param_name
  else Fmt.pf ppf "%a %s" pp_typ t p.param_name

let pp_params ppf params =
  if not (List.is_empty params) then
    Fmt.pf ppf "(%a)" Fmt.(list ~sep:comma pp_param) params

let pp_struct ppf (s : struct_) =
  Fmt.pf ppf "typedef struct _%s%a" s.name pp_params s.params;
  Option.iter (Fmt.pf ppf "@,where (%a)" pp_expr) s.where;
  Fmt.pf ppf "@,{@[<v 2>";
  List.iter (pp_field ppf) s.fields;
  Fmt.pf ppf "@]@,} %s" s.name

let pp_decl ppf = function
  | Typedef { entrypoint; export; doc; struct_ = st } ->
      Option.iter (Fmt.pf ppf "/*++ %s --*/@,") doc;
      if export then Fmt.pf ppf "export@,";
      if entrypoint then Fmt.pf ppf "entrypoint@,";
      Fmt.pf ppf "%a;@,@," pp_struct st
  | Define { name; value } ->
      if value < 0 then Fmt.pf ppf "#define %s (%d)@," name value
      else Fmt.pf ppf "#define %s 0x%x@," name value
  | Extern_fn { name; params; ret = Pack_typ ret } ->
      Fmt.pf ppf "extern %a %s(%a);@,@," pp_typ ret name
        Fmt.(list ~sep:comma pp_param)
        params
  | Extern_probe { init; name } ->
      if init then Fmt.pf ppf "extern probe (INIT) %s;@,@," name
      else Fmt.pf ppf "extern probe %s;@,@," name
  | Enum_decl { name; cases; base = Pack_typ base } ->
      Fmt.pf ppf "%a enum %s {@[<v 2>" pp_typ base name;
      List.iteri
        (fun i (cname, value) ->
          if not (Int.equal i 0) then Fmt.string ppf ",";
          Fmt.pf ppf "@,%s = %d" cname value)
        cases;
      Fmt.pf ppf "@]@,}@,@,"
  | Casetype_decl { name; params; tag = Pack_typ _; cases } ->
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
      List.iteri
        (fun i (tag_opt, Pack_typ typ) ->
          let field_name = Fmt.str "v%d" i in
          match tag_opt with
          | Some e ->
              Fmt.pf ppf "@,case %a: %a %s;" pp_packed_expr e pp_typ typ
                field_name
          | None -> Fmt.pf ppf "@,default: %a %s;" pp_typ typ field_name)
        cases;
      Fmt.pf ppf "@,}@]@,} %s;@,@," public_name

let pp_module ppf m =
  Option.iter (Fmt.pf ppf "/*++ %s --*/@,@,") m.doc;
  List.iter (pp_decl ppf) m.decls

let to_3d m = Fmt.str "@[<v>%a@]" pp_module m

let to_3d_file path m =
  let oc = open_out path in
  let ppf = Format.formatter_of_out_channel oc in
  Fmt.pf ppf "@[<v>%a@]@." pp_module m;
  close_out oc

(* Binary parsing with bytesrw *)

module Br = Bytesrw.Bytes.Reader
module Bs = Bytesrw.Bytes.Slice

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }

exception Parse_error of parse_error

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

(* Parsing context - tracks field values for dependent types.

   All field values are stored as [int] after conversion via [val_to_int].
   This is sound because constraint expressions (the only consumers of
   context values) operate on integers. *)
module Ctx = Map.Make (String)

type ctx = int Ctx.t

let empty_ctx = Ctx.empty

(* Convert a typed value to [int] for context storage. All types that
   appear in constraint expressions are numeric, so this conversion is
   lossless for practical schemas. Non-numeric types store 0. *)
let rec val_to_int : type a. a typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ ->
      (* Unsigned interpretation — values >= 2^62 don't fit in OCaml int,
         return max_int so constraints [value <= K] fail correctly *)
      Int64.unsigned_to_int v |> Option.value ~default:max_int
  | Bits _ -> v
  | Enum { base; _ } -> val_to_int base v
  | Where { inner; _ } -> val_to_int inner v
  | Single_elem { elem; _ } -> val_to_int elem v
  | Apply { typ; _ } -> val_to_int typ v
  | Map { inner; encode; _ } -> val_to_int inner (encode v)
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Casetype _
  | Struct _ | Type_ref _ | Qualified_ref _ ->
      0

let ctx_get ctx name =
  match Ctx.find_opt name ctx with
  | Some v -> v
  | None -> failwith ("unbound field: " ^ name)

(* Decoder state - tracks position within slices *)
type decoder = {
  reader : Br.t;
  mutable slice : Bs.t;
  mutable slice_pos : int;
  mutable position : int;
}

let decoder reader = { reader; slice = Bs.eod; slice_pos = 0; position = 0 }

let refill dec =
  dec.slice <- Br.read dec.reader;
  dec.slice_pos <- 0

let slice_get_byte slice pos =
  Bytes.get_uint8 (Bs.bytes slice) (Bs.first slice + pos)

let read_byte dec =
  if dec.slice_pos >= Bs.length dec.slice then begin
    refill dec;
    if Bs.is_eod dec.slice then None
    else begin
      let b = slice_get_byte dec.slice dec.slice_pos in
      dec.slice_pos <- dec.slice_pos + 1;
      dec.position <- dec.position + 1;
      Some b
    end
  end
  else begin
    let b = slice_get_byte dec.slice dec.slice_pos in
    dec.slice_pos <- dec.slice_pos + 1;
    dec.position <- dec.position + 1;
    Some b
  end

(* Read exactly n bytes *)
let read_bytes dec n =
  if n = 0 then Ok Bytes.empty
  else
    let buf = Bytes.create n in
    let rec loop off remaining =
      if remaining = 0 then Ok buf
      else begin
        (* Refill if needed *)
        if dec.slice_pos >= Bs.length dec.slice then begin
          refill dec;
          if Bs.is_eod dec.slice then
            Error (Unexpected_eof { expected = n; got = off })
          else loop off remaining
        end
        else
          let available = Bs.length dec.slice - dec.slice_pos in
          let to_copy = min available remaining in
          Bytes.blit (Bs.bytes dec.slice)
            (Bs.first dec.slice + dec.slice_pos)
            buf off to_copy;
          dec.slice_pos <- dec.slice_pos + to_copy;
          dec.position <- dec.position + to_copy;
          loop (off + to_copy) (remaining - to_copy)
      end
    in
    loop 0 n

(* Read all remaining bytes *)
let read_all dec =
  let buf = Buffer.create 256 in
  let rec loop () =
    if dec.slice_pos >= Bs.length dec.slice then begin
      refill dec;
      if Bs.is_eod dec.slice then Buffer.contents buf else loop ()
    end
    else begin
      let slice_bytes = Bs.bytes dec.slice in
      let first = Bs.first dec.slice + dec.slice_pos in
      let len = Bs.length dec.slice - dec.slice_pos in
      Buffer.add_subbytes buf slice_bytes first len;
      dec.position <- dec.position + len;
      dec.slice_pos <- Bs.length dec.slice;
      loop ()
    end
  in
  loop ()

(* Evaluate an expression in context *)
let rec eval_expr : type a. ctx -> a expr -> a =
 fun ctx expr ->
  match expr with
  | Int n -> n
  | Int64 n -> n
  | Bool b -> b
  | Ref name -> ctx_get ctx name
  | Sizeof _ -> 0 (* TODO: compute actual size *)
  | Sizeof_this -> 0
  | Field_pos -> 0
  | Add (a, b) -> eval_expr ctx a + eval_expr ctx b
  | Sub (a, b) -> eval_expr ctx a - eval_expr ctx b
  | Mul (a, b) -> eval_expr ctx a * eval_expr ctx b
  | Div (a, b) -> eval_expr ctx a / eval_expr ctx b
  | Mod (a, b) -> eval_expr ctx a mod eval_expr ctx b
  | Land (a, b) -> eval_expr ctx a land eval_expr ctx b
  | Lor (a, b) -> eval_expr ctx a lor eval_expr ctx b
  | Lxor (a, b) -> eval_expr ctx a lxor eval_expr ctx b
  | Lnot a -> lnot (eval_expr ctx a)
  | Lsl (a, b) -> eval_expr ctx a lsl eval_expr ctx b
  | Lsr (a, b) -> eval_expr ctx a lsr eval_expr ctx b
  | Eq (a, b) -> eval_expr ctx a = eval_expr ctx b
  | Ne (a, b) -> eval_expr ctx a <> eval_expr ctx b
  | Lt (a, b) -> eval_expr ctx a < eval_expr ctx b
  | Le (a, b) -> eval_expr ctx a <= eval_expr ctx b
  | Gt (a, b) -> eval_expr ctx a > eval_expr ctx b
  | Ge (a, b) -> eval_expr ctx a >= eval_expr ctx b
  | And (a, b) -> eval_expr ctx a && eval_expr ctx b
  | Or (a, b) -> eval_expr ctx a || eval_expr ctx b
  | Not a -> not (eval_expr ctx a)
  | Cast (_, e) -> eval_expr ctx e (* TODO: proper casting *)

(* Bitfield accumulator for packed struct parsing.
   Consecutive bitfields sharing the same base type are packed together.
   Bits are extracted from MSB to LSB (big-endian style) per EverParse 3D. *)
type bf_accum = {
  bf_base : bitfield_base;
  bf_word : int;
  bf_bits_used : int;
  bf_total_bits : int;
}

let bf_total_bits = function BF_U8 -> 8 | BF_U16 _ -> 16 | BF_U32 _ -> 32
let bf_base_size = function BF_U8 -> 1 | BF_U16 _ -> 2 | BF_U32 _ -> 4

let bf_compatible base1 base2 =
  match (base1, base2) with
  | BF_U8, BF_U8 -> true
  | BF_U16 e1, BF_U16 e2 -> e1 = e2
  | BF_U32 e1, BF_U32 e2 -> e1 = e2
  | _ -> false

let bf_read_word dec base =
  let size = bf_base_size base in
  match read_bytes dec size with
  | Error e -> Error e
  | Ok buf ->
      let v =
        match base with
        | BF_U8 -> Bytes.get_uint8 buf 0
        | BF_U16 Little -> Bytes.get_uint16_le buf 0
        | BF_U16 Big -> Bytes.get_uint16_be buf 0
        | BF_U32 Little -> Int32.to_int (Bytes.get_int32_le buf 0)
        | BF_U32 Big -> Int32.to_int (Bytes.get_int32_be buf 0)
      in
      Ok v

(* Extract bits from accumulated word (MSB first, big-endian style) *)
let bf_extract accum width =
  let shift = accum.bf_total_bits - accum.bf_bits_used - width in
  let mask = (1 lsl width) - 1 in
  let value = (accum.bf_word lsr shift) land mask in
  let new_accum = { accum with bf_bits_used = accum.bf_bits_used + width } in
  (value, new_accum)

(* Check if accumulator can provide more bits *)
let bf_has_room accum width = accum.bf_bits_used + width <= accum.bf_total_bits

(* Legacy parse_bits for standalone bitfield parsing (non-struct context) *)
let parse_bits dec base width =
  match bf_read_word dec base with
  | Error e -> Error e
  | Ok v -> Ok (v land ((1 lsl width) - 1))

(* Helper: parse fixed-size integer from decoder *)
let parse_int dec n get ctx =
  match read_bytes dec n with
  | Ok buf -> Ok (get buf 0, ctx)
  | Error e -> Error e

(* Parse a type from a decoder *)
let parse_bf_field dec accum_opt base width =
  match accum_opt with
  | Some accum when bf_compatible accum.bf_base base && bf_has_room accum width
    ->
      let v, new_accum = bf_extract accum width in
      let accum_opt' =
        if new_accum.bf_bits_used = new_accum.bf_total_bits then None
        else Some new_accum
      in
      Ok (v, accum_opt')
  | _ ->
      let* word = bf_read_word dec base in
      let total = bf_total_bits base in
      let accum =
        {
          bf_base = base;
          bf_word = word;
          bf_bits_used = 0;
          bf_total_bits = total;
        }
      in
      let v, new_accum = bf_extract accum width in
      let accum_opt' =
        if new_accum.bf_bits_used = new_accum.bf_total_bits then None
        else Some new_accum
      in
      Ok (v, accum_opt')

let check_constraint ctx cond =
  match cond with
  | Some c when not (eval_expr ctx c) ->
      Error (Constraint_failed "field constraint")
  | _ -> Ok ()

let parse_all_zeros dec ctx =
  let s = read_all dec in
  let rec check i =
    if i >= String.length s then Ok (s, ctx)
    else if s.[i] <> '\000' then Error (All_zeros_failed { offset = i })
    else check (i + 1)
  in
  check 0

let rec parse_with_ctx : type a.
    ctx -> a typ -> decoder -> (a * ctx, parse_error) result =
 fun ctx typ dec ->
  match typ with
  | Uint8 -> parse_int dec 1 Bytes.get_uint8 ctx
  | Uint16 Little -> parse_int dec 2 Bytes.get_uint16_le ctx
  | Uint16 Big -> parse_int dec 2 Bytes.get_uint16_be ctx
  | Uint32 Little -> parse_int dec 4 UInt32.get_le ctx
  | Uint32 Big -> parse_int dec 4 UInt32.get_be ctx
  | Uint63 Little -> parse_int dec 8 UInt63.get_le ctx
  | Uint63 Big -> parse_int dec 8 UInt63.get_be ctx
  | Uint64 Little -> parse_int dec 8 Bytes.get_int64_le ctx
  | Uint64 Big -> parse_int dec 8 Bytes.get_int64_be ctx
  | Bits { width; base } -> (
      match parse_bits dec base width with
      | Ok v -> Ok (v, ctx)
      | Error e -> Error e)
  | Unit -> Ok ((), ctx)
  | All_bytes ->
      let s = read_all dec in
      Ok (s, ctx)
  | All_zeros -> parse_all_zeros dec ctx
  | Where { cond; inner } -> (
      match parse_with_ctx ctx inner dec with
      | Ok (v, ctx') ->
          if eval_expr ctx' cond then Ok (v, ctx')
          else Error (Constraint_failed "where clause")
      | Error e -> Error e)
  | Array { len; elem } ->
      let n = eval_expr ctx len in
      let rec loop acc i ctx' =
        if i >= n then Ok (List.rev acc, ctx')
        else
          match parse_with_ctx ctx' elem dec with
          | Ok (v, ctx'') -> loop (v :: acc) (i + 1) ctx''
          | Error e -> Error e
      in
      loop [] 0 ctx
  | Byte_array { size } -> (
      let n = eval_expr ctx size in
      match read_bytes dec n with
      | Ok buf -> Ok (Bytes.to_string buf, ctx)
      | Error e -> Error e)
  | Single_elem { size = _; elem; at_most = _ } -> parse_with_ctx ctx elem dec
  | Enum { cases; base; _ } -> (
      match parse_with_ctx ctx base dec with
      | Ok (v, ctx') ->
          let valid = List.map snd cases in
          if List.mem v valid then Ok (v, ctx')
          else Error (Invalid_enum { value = v; valid })
      | Error e -> Error e)
  | Casetype { cases; tag; _ } -> (
      match parse_with_ctx ctx tag dec with
      | Error e -> Error e
      | Ok (tag_val, ctx') ->
          let rec find_case = function
            | [] -> Error (Invalid_tag (val_to_int tag tag_val))
            | (Some expected, case_typ) :: rest ->
                if expected = tag_val then parse_with_ctx ctx' case_typ dec
                else find_case rest
            | (None, case_typ) :: _ -> parse_with_ctx ctx' case_typ dec
          in
          find_case cases)
  | Struct { fields; _ } ->
      (parse_struct_fields ctx dec fields : (unit * ctx, parse_error) result)
  | Map { inner; decode; _ } ->
      parse_with_ctx ctx inner dec
      |> Result.map (fun (v, ctx') -> (decode v, ctx'))
  | Type_ref _ -> failwith "type_ref requires a type registry"
  | Qualified_ref _ -> failwith "qualified_ref requires a type registry"
  | Apply _ -> failwith "apply requires a type registry"

and parse_struct_fields ctx dec fields =
  let parse_field_with_bf : type a.
      bf_accum option -> a typ -> (a * bf_accum option, parse_error) result =
   fun accum_opt typ ->
    match typ with
    | Bits { width; base } -> parse_bf_field dec accum_opt base width
    | _ ->
        let* v, _ = parse_with_ctx ctx typ dec in
        Ok (v, None)
  in
  let rec go ctx' accum_opt = function
    | [] -> Ok ((), ctx')
    | Field { field_name; field_typ = Bits _ as ft; constraint_; _ } :: rest ->
        let* v, accum_opt' = parse_field_with_bf accum_opt ft in
        let ctx'' =
          match field_name with Some n -> Ctx.add n v ctx' | None -> ctx'
        in
        let* () = check_constraint ctx'' constraint_ in
        go ctx'' accum_opt' rest
    | Field { field_name; field_typ; constraint_; _ } :: rest ->
        let* v, ctx'' = parse_with_ctx ctx' field_typ dec in
        let ctx'' =
          match field_name with
          | Some n -> Ctx.add n (val_to_int field_typ v) ctx''
          | None -> ctx''
        in
        let* () = check_constraint ctx'' constraint_ in
        go ctx'' None rest
  in
  go ctx None fields

let parse typ reader =
  let dec = decoder reader in
  match parse_with_ctx empty_ctx typ dec with
  | Ok (v, _) -> Ok v
  | Error e -> Error e

let parse_string typ s =
  let reader = Br.of_string s in
  parse typ reader

let parse_bytes typ b =
  let reader = Br.of_bytes b in
  parse typ reader

(* Binary encoding with Bytesrw.Bytes.Writer *)

module Bw = Bytesrw.Bytes.Writer

(* Encoder state *)
type encoder = { writer : Bw.t; buf : bytes }

let encoder writer = { writer; buf = Bytes.create 8 }

let write_slice enc len =
  let slice = Bs.make enc.buf ~first:0 ~length:len in
  Bw.write enc.writer slice

let write_byte enc b =
  Bytes.set_uint8 enc.buf 0 b;
  write_slice enc 1

let write_uint16_le enc v =
  Bytes.set_uint16_le enc.buf 0 v;
  write_slice enc 2

let write_uint16_be enc v =
  Bytes.set_uint16_be enc.buf 0 v;
  write_slice enc 2

let write_int32_le enc v =
  Bytes.set_int32_le enc.buf 0 v;
  write_slice enc 4

let write_int32_be enc v =
  Bytes.set_int32_be enc.buf 0 v;
  write_slice enc 4

let write_uint32_le enc v =
  UInt32.set_le enc.buf 0 v;
  write_slice enc 4

let write_uint32_be enc v =
  UInt32.set_be enc.buf 0 v;
  write_slice enc 4

let write_int64_le enc v =
  Bytes.set_int64_le enc.buf 0 v;
  write_slice enc 8

let write_int64_be enc v =
  Bytes.set_int64_be enc.buf 0 v;
  write_slice enc 8

let write_uint63_le enc v =
  UInt63.set_le enc.buf 0 v;
  write_slice enc 8

let write_uint63_be enc v =
  UInt63.set_be enc.buf 0 v;
  write_slice enc 8

let write_string enc s = Bw.write_string enc.writer s

let rec encode_with_ctx : type a. ctx -> a typ -> a -> encoder -> ctx =
 fun ctx typ v enc ->
  match typ with
  | Uint8 ->
      write_byte enc v;
      ctx
  | Uint16 Little ->
      write_uint16_le enc v;
      ctx
  | Uint16 Big ->
      write_uint16_be enc v;
      ctx
  | Uint32 Little ->
      write_uint32_le enc v;
      ctx
  | Uint32 Big ->
      write_uint32_be enc v;
      ctx
  | Uint63 Little ->
      write_uint63_le enc v;
      ctx
  | Uint63 Big ->
      write_uint63_be enc v;
      ctx
  | Uint64 Little ->
      write_int64_le enc v;
      ctx
  | Uint64 Big ->
      write_int64_be enc v;
      ctx
  | Bits { width; base } ->
      let mask = (1 lsl width) - 1 in
      let masked = v land mask in
      (match base with
      | BF_U8 -> write_byte enc masked
      | BF_U16 Little -> write_uint16_le enc masked
      | BF_U16 Big -> write_uint16_be enc masked
      | BF_U32 Little -> write_int32_le enc (Int32.of_int masked)
      | BF_U32 Big -> write_int32_be enc (Int32.of_int masked));
      ctx
  | Unit -> ctx
  | All_bytes ->
      write_string enc v;
      ctx
  | All_zeros ->
      write_string enc v;
      ctx
  | Where { inner; _ } -> encode_with_ctx ctx inner v enc
  | Array { elem; _ } ->
      List.fold_left
        (fun ctx' elem_v -> encode_with_ctx ctx' elem elem_v enc)
        ctx v
  | Byte_array _ ->
      write_string enc v;
      ctx
  | Single_elem { elem; _ } -> encode_with_ctx ctx elem v enc
  | Enum { base; _ } -> encode_with_ctx ctx base v enc
  | Map { inner; encode; _ } -> encode_with_ctx ctx inner (encode v) enc
  | Casetype _ -> failwith "casetype encoding: use Record module"
  | Struct _ -> failwith "struct encoding: use Record module"
  | Type_ref _ -> failwith "type_ref requires a type registry"
  | Qualified_ref _ -> failwith "qualified_ref requires a type registry"
  | Apply _ -> failwith "apply requires a type registry"

let encode typ v writer =
  let enc = encoder writer in
  ignore (encode_with_ctx empty_ctx typ v enc)

let encode_to_bytes typ v =
  let buf = Buffer.create 64 in
  let writer = Bw.of_buffer buf in
  encode typ v writer;
  Buffer.to_bytes buf

let encode_to_string typ v =
  let buf = Buffer.create 64 in
  let writer = Bw.of_buffer buf in
  encode typ v writer;
  Buffer.contents buf

(* ==================== Typed Record DSL (ctypes-like) ==================== *)

type ('a, 'r) field_codec = {
  name : string;
  constraint_ : bool expr option;
  typ : 'a typ;
  get : 'r -> 'a;
  set : 'a -> 'r -> 'r;
}
(** A field codec for field of type ['a] in record of type ['r] *)

(** Specialized encode/decode functions built at codec construction time. This
    avoids interpretation overhead by generating direct operations on
    Bytesrw.Slice, similar to hand-written codecs but integrated with the
    streaming API. *)

type 'r record_codec = {
  record_name : string;
  fields : 'r field_codec_packed list;
  default : 'r;
  wire_size : int option; (* Pre-computed fixed wire size, None if variable *)
}
(** A record codec for type ['r]. Contains only the schema description.
    Specialized encode/decode functions are built by
    [Record.encode]/[Record.decode]. *)

and 'r field_codec_packed =
  | Field_codec : ('a, 'r) field_codec -> 'r field_codec_packed

(** Compute wire size of a field codec's type *)
let rec field_wire_size : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Uint32 _ -> Some 4
  | Uint64 _ -> Some 8
  | Bits { base; _ } -> (
      match base with
      | BF_U8 -> Some 1
      | BF_U16 _ -> Some 2
      | BF_U32 _ -> Some 4)
  | Unit -> Some 0
  | Byte_array { size = Int n } -> Some n
  | Where { inner; _ } -> field_wire_size inner
  | Enum { base; _ } -> field_wire_size base
  | Map { inner; _ } -> field_wire_size inner
  | _ -> None

(** Build a specialized field encoder: writes field value to bytes at offset.
    Returns the new offset. Works directly on the slice's underlying bytes. *)
let rec build_field_encoder : type a. a typ -> bytes -> int -> a -> int =
 fun typ ->
  match typ with
  | Uint8 ->
      fun buf off v ->
        Bytes.set_uint8 buf off v;
        off + 1
  | Uint16 Little ->
      fun buf off v ->
        Bytes.set_uint16_le buf off v;
        off + 2
  | Uint16 Big ->
      fun buf off v ->
        Bytes.set_uint16_be buf off v;
        off + 2
  | Uint32 Little ->
      fun buf off v ->
        UInt32.set_le buf off v;
        off + 4
  | Uint32 Big ->
      fun buf off v ->
        UInt32.set_be buf off v;
        off + 4
  | Uint63 Little ->
      fun buf off v ->
        UInt63.set_le buf off v;
        off + 8
  | Uint63 Big ->
      fun buf off v ->
        UInt63.set_be buf off v;
        off + 8
  | Uint64 Little ->
      fun buf off v ->
        Bytes.set_int64_le buf off v;
        off + 8
  | Uint64 Big ->
      fun buf off v ->
        Bytes.set_int64_be buf off v;
        off + 8
  | Byte_array { size = Int n } ->
      fun buf off v ->
        let len = min n (String.length v) in
        Bytes.blit_string v 0 buf off len;
        if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
        off + n
  | Where { inner; _ } -> build_field_encoder inner
  | Enum { base; _ } -> build_field_encoder base
  | Map { inner; encode; _ } ->
      let enc = build_field_encoder inner in
      fun buf off v -> enc buf off (encode v)
  | Unit -> fun _buf off () -> off
  | _ ->
      (* Fallback for complex types - not specialized *)
      fun _buf _off _v -> failwith "build_field_encoder: unsupported type"

(** Build a specialized field decoder: reads field value from slice's bytes.
    Takes the slice's bytes, first offset, and field offset within the record.
    Returns the value and new field offset. *)
let rec build_field_decoder : type a. a typ -> bytes -> int -> int -> a * int =
 fun typ ->
  match typ with
  | Uint8 -> fun buf base off -> (Bytes.get_uint8 buf (base + off), off + 1)
  | Uint16 Little ->
      fun buf base off -> (Bytes.get_uint16_le buf (base + off), off + 2)
  | Uint16 Big ->
      fun buf base off -> (Bytes.get_uint16_be buf (base + off), off + 2)
  | Uint32 Little ->
      fun buf base off -> (UInt32.get_le buf (base + off), off + 4)
  | Uint32 Big -> fun buf base off -> (UInt32.get_be buf (base + off), off + 4)
  | Uint63 Little ->
      fun buf base off -> (UInt63.get_le buf (base + off), off + 8)
  | Uint63 Big -> fun buf base off -> (UInt63.get_be buf (base + off), off + 8)
  | Uint64 Little ->
      fun buf base off -> (Bytes.get_int64_le buf (base + off), off + 8)
  | Uint64 Big ->
      fun buf base off -> (Bytes.get_int64_be buf (base + off), off + 8)
  | Byte_array { size = Int n } ->
      fun buf base off -> (Bytes.sub_string buf (base + off) n, off + n)
  | Where { inner; _ } -> build_field_decoder inner
  | Enum { base; _ } -> build_field_decoder base
  | Map { inner; decode; _ } ->
      let dec = build_field_decoder inner in
      fun buf base off ->
        let v, off' = dec buf base off in
        (decode v, off')
  | Unit -> fun _buf _base off -> ((), off)
  | _ ->
      (* Fallback for complex types *)
      fun _buf _base _off -> failwith "build_field_decoder: unsupported type"

(** Build a mutable-offset field decoder (avoids tuple allocation). Takes bytes,
    base, mutable offset ref. Returns value, mutates offset. *)
let rec build_field_decoder_mut : type a. a typ -> bytes -> int -> int ref -> a
    =
 fun typ ->
  match typ with
  | Uint8 ->
      fun buf base off ->
        let v = Bytes.get_uint8 buf (base + !off) in
        off := !off + 1;
        v
  | Uint16 Little ->
      fun buf base off ->
        let v = Bytes.get_uint16_le buf (base + !off) in
        off := !off + 2;
        v
  | Uint16 Big ->
      fun buf base off ->
        let v = Bytes.get_uint16_be buf (base + !off) in
        off := !off + 2;
        v
  | Uint32 Little ->
      fun buf base off ->
        let v = UInt32.get_le buf (base + !off) in
        off := !off + 4;
        v
  | Uint32 Big ->
      fun buf base off ->
        let v = UInt32.get_be buf (base + !off) in
        off := !off + 4;
        v
  | Uint63 Little ->
      fun buf base off ->
        let v = UInt63.get_le buf (base + !off) in
        off := !off + 8;
        v
  | Uint63 Big ->
      fun buf base off ->
        let v = UInt63.get_be buf (base + !off) in
        off := !off + 8;
        v
  | Uint64 Little ->
      fun buf base off ->
        let v = Bytes.get_int64_le buf (base + !off) in
        off := !off + 8;
        v
  | Uint64 Big ->
      fun buf base off ->
        let v = Bytes.get_int64_be buf (base + !off) in
        off := !off + 8;
        v
  | Byte_array { size = Int n } ->
      fun buf base off ->
        let v = Bytes.sub_string buf (base + !off) n in
        off := !off + n;
        v
  | Where { inner; _ } -> build_field_decoder_mut inner
  | Enum { base; _ } -> build_field_decoder_mut base
  | Map { inner; decode; _ } ->
      let dec = build_field_decoder_mut inner in
      fun buf base off -> decode (dec buf base off)
  | Unit -> fun _buf _base _off -> ()
  | _ ->
      fun _buf _base _off ->
        failwith "build_field_decoder_mut: unsupported type"

(** CPS-style field decoder: threads constructor through decode chain. This is
    the repr pattern that avoids intermediate record allocations. Type: bytes ->
    int -> int ref -> ('a -> 'b) -> 'b *)
let rec build_field_decoder_cps : type a.
    a typ -> bytes -> int -> int ref -> (a -> 'k) -> 'k =
 fun typ ->
  match typ with
  | Uint8 ->
      fun buf base off k ->
        let v = Bytes.get_uint8 buf (base + !off) in
        off := !off + 1;
        k v
  | Uint16 Little ->
      fun buf base off k ->
        let v = Bytes.get_uint16_le buf (base + !off) in
        off := !off + 2;
        k v
  | Uint16 Big ->
      fun buf base off k ->
        let v = Bytes.get_uint16_be buf (base + !off) in
        off := !off + 2;
        k v
  | Uint32 Little ->
      fun buf base off k ->
        let v = UInt32.get_le buf (base + !off) in
        off := !off + 4;
        k v
  | Uint32 Big ->
      fun buf base off k ->
        let v = UInt32.get_be buf (base + !off) in
        off := !off + 4;
        k v
  | Uint63 Little ->
      fun buf base off k ->
        let v = UInt63.get_le buf (base + !off) in
        off := !off + 8;
        k v
  | Uint63 Big ->
      fun buf base off k ->
        let v = UInt63.get_be buf (base + !off) in
        off := !off + 8;
        k v
  | Uint64 Little ->
      fun buf base off k ->
        let v = Bytes.get_int64_le buf (base + !off) in
        off := !off + 8;
        k v
  | Uint64 Big ->
      fun buf base off k ->
        let v = Bytes.get_int64_be buf (base + !off) in
        off := !off + 8;
        k v
  | Byte_array { size = Int n } ->
      fun buf base off k ->
        let v = Bytes.sub_string buf (base + !off) n in
        off := !off + n;
        k v
  | Where { inner; _ } -> build_field_decoder_cps inner
  | Enum { base; _ } -> build_field_decoder_cps base
  | Map { inner; decode; _ } ->
      let dec = build_field_decoder_cps inner in
      fun buf base off k -> dec buf base off (fun v -> k (decode v))
  | Unit -> fun _buf _base _off k -> k ()
  | _ ->
      fun _buf _base _off _k ->
        failwith "build_field_decoder_cps: unsupported type"

(** Create a field codec *)
let field_codec name ?constraint_ typ ~get ~set =
  { name; constraint_; typ; get; set }

(** Create a record codec schema. This is just data - no specialization yet.
    Specialization happens when you call [Record.encode]/[Record.decode]. *)
let record_codec name ~default fields =
  let wire_size =
    List.fold_left
      (fun acc (Field_codec fc) ->
        match (acc, field_wire_size fc.typ) with
        | Some a, Some b -> Some (a + b)
        | _ -> None)
      (Some 0) fields
  in
  { record_name = name; fields; default; wire_size }

(** [record_wire_size codec] returns the fixed wire size of the codec, or [None]
    if the codec has variable-length fields. Callers should check buffer length
    before calling decode to avoid index-out-of-bounds errors. *)
let record_wire_size codec = codec.wire_size

(** Pack a field codec for storage in record *)
let pack_field fc = Field_codec fc

(* Bitfield encoder accumulator *)
type bf_enc_accum = {
  bfe_base : bitfield_base;
  bfe_word : int;
  bfe_bits_used : int;
  bfe_total_bits : int;
}

let bf_write_word enc base word =
  match base with
  | BF_U8 -> write_byte enc word
  | BF_U16 Little -> write_uint16_le enc word
  | BF_U16 Big -> write_uint16_be enc word
  | BF_U32 Little -> write_int32_le enc (Int32.of_int word)
  | BF_U32 Big -> write_int32_be enc (Int32.of_int word)

(* Insert bits into accumulator at current position (MSB first) *)
let bf_insert accum width value =
  let shift = accum.bfe_total_bits - accum.bfe_bits_used - width in
  let mask = (1 lsl width) - 1 in
  let masked = value land mask in
  let word' = accum.bfe_word lor (masked lsl shift) in
  { accum with bfe_word = word'; bfe_bits_used = accum.bfe_bits_used + width }

(** Encode a record value to a writer with bitfield packing *)
let encode_bf_accum enc flush_accum accum_opt base width field_val =
  let accum_opt' =
    match accum_opt with
    | Some accum
      when bf_compatible accum.bfe_base base
           && accum.bfe_bits_used + width <= accum.bfe_total_bits ->
        Some (bf_insert accum width field_val)
    | _ ->
        flush_accum accum_opt;
        let total = bf_total_bits base in
        let accum =
          {
            bfe_base = base;
            bfe_word = 0;
            bfe_bits_used = 0;
            bfe_total_bits = total;
          }
        in
        Some (bf_insert accum width field_val)
  in
  match accum_opt' with
  | Some a when a.bfe_bits_used = a.bfe_total_bits ->
      bf_write_word enc a.bfe_base a.bfe_word;
      None
  | other -> other

let encode_record : type r.
    r record_codec -> r -> Bw.t -> (unit, parse_error) result =
 fun codec v writer ->
  let enc = encoder writer in
  let flush_accum = function
    | None -> ()
    | Some accum -> bf_write_word enc accum.bfe_base accum.bfe_word
  in
  let rec encode_fields (ctx : int Ctx.t) accum_opt = function
    | [] ->
        flush_accum accum_opt;
        Ok ()
    | Field_codec fc :: rest -> (
        let field_val = fc.get v in
        match fc.typ with
        | Bits { width; base } ->
            let accum_opt' =
              encode_bf_accum enc flush_accum accum_opt base width field_val
            in
            let ctx' = Ctx.add fc.name field_val ctx in
            let* () = check_constraint ctx' fc.constraint_ in
            encode_fields ctx' accum_opt' rest
        | _ ->
            flush_accum accum_opt;
            let ctx' = encode_with_ctx ctx fc.typ field_val enc in
            let ctx'' = Ctx.add fc.name (val_to_int fc.typ field_val) ctx' in
            let* () = check_constraint ctx'' fc.constraint_ in
            encode_fields ctx'' None rest)
  in
  encode_fields empty_ctx None codec.fields

(** Build a staged record encoder: returns a slice with encoded data. Following
    repr's pattern: iteration over fields happens once at staging time, building
    closures that are fast to execute.

    WARNING: The returned slice's underlying buffer may be reused between calls.
    Copy the slice data before the next encode if you need to keep it. *)
let encode_record_to_slice : type r. r record_codec -> (r -> Bs.t) Staged.t =
 fun codec ->
  match codec.wire_size with
  | Some wire_size ->
      (* Build field encoders at staging time - this is the repr pattern *)
      let buf = Bytes.create wire_size in
      let field_encoders =
        List.filter_map
          (fun (Field_codec fc) ->
            match field_wire_size fc.typ with
            | Some _ ->
                let encoder = build_field_encoder fc.typ in
                Some (fun b off v -> encoder b off (fc.get v))
            | None -> None)
          codec.fields
      in
      if List.length field_encoders <> List.length codec.fields then
        (* Not all fields can be specialized - return empty slice *)
        Staged.stage (fun _v -> Bs.eod)
      else
        (* All fields specialized - this closure captures pre-built encoders *)
        Staged.stage (fun v ->
            let _ =
              List.fold_left (fun off enc -> enc buf off v) 0 field_encoders
            in
            Bs.make buf ~first:0 ~length:wire_size)
  | None ->
      (* Variable-size: can't return slice *)
      Staged.stage (fun _v -> Bs.eod)

(** Build a staged record decoder: reads from a slice. Following repr's pattern:
    iteration over fields happens once at staging time, building closures that
    are fast to execute. *)
let decode_record_from_slice : type r. r record_codec -> (Bs.t -> r) Staged.t =
 fun codec ->
  (* Build field decoders using mutable offset to avoid tuple allocation *)
  let field_decoders =
    List.filter_map
      (fun (Field_codec fc) ->
        match field_wire_size fc.typ with
        | Some _ ->
            let decoder = build_field_decoder_mut fc.typ in
            let set = fc.set in
            Some (fun buf base off acc -> set (decoder buf base off) acc)
        | None -> None)
      codec.fields
  in
  let default = codec.default in
  if List.length field_decoders <> List.length codec.fields then
    Staged.stage (fun _slice -> default)
  else
    (* Convert to array for faster iteration *)
    let decoders = Array.of_list field_decoders in
    let n = Array.length decoders in
    Staged.stage (fun slice ->
        let buf = Bs.bytes slice in
        let base = Bs.first slice in
        let off = Stdlib.ref 0 in
        let acc = Stdlib.ref default in
        for i = 0 to n - 1 do
          acc := decoders.(i) buf base off !acc
        done;
        !acc)

(** {2 Zero-allocation encode/decode}

    These functions provide direct bytes access without slice allocation. Use
    when you need maximum performance and can manage buffer lifetime. *)

type 'r encode_context = {
  buffer : bytes;
  wire_size : int;
  encode : 'r -> unit;
}
(** Context for zero-allocation encoding. The [buffer] is shared and reused
    between calls to [encode]. Copy the bytes before the next [encode] if you
    need to keep them. *)

let encode_record_to_bytes : type r. r record_codec -> r encode_context option =
 fun codec ->
  match codec.wire_size with
  | Some wire_size ->
      let buf = Bytes.create wire_size in
      (* Build field encoders at staging time - each captures its getter *)
      let field_encoders =
        List.filter_map
          (fun (Field_codec fc) ->
            match field_wire_size fc.typ with
            | Some _ ->
                let encoder = build_field_encoder fc.typ in
                let get = fc.get in
                (* This function is built once, captures encoder and get *)
                Some (fun v b off -> encoder b off (get v))
            | None -> None)
          codec.fields
      in
      if List.length field_encoders <> List.length codec.fields then None
      else
        (* Convert to array for faster iteration without closure allocation *)
        let encoders = Array.of_list field_encoders in
        let n = Array.length encoders in
        let encode v =
          let off = Stdlib.ref 0 in
          for i = 0 to n - 1 do
            off := encoders.(i) v buf !off
          done
        in
        Some { buffer = buf; wire_size; encode }
  | None -> None

(** Build a direct field reader that reads at a fixed offset. No tuples, no refs
    \- just pure value read. *)
let rec build_field_reader : type a. a typ -> int -> bytes -> int -> a =
 fun typ field_off ->
  match typ with
  | Uint8 -> fun buf base -> Bytes.get_uint8 buf (base + field_off)
  | Uint16 Little -> fun buf base -> Bytes.get_uint16_le buf (base + field_off)
  | Uint16 Big -> fun buf base -> Bytes.get_uint16_be buf (base + field_off)
  | Uint32 Little -> fun buf base -> UInt32.get_le buf (base + field_off)
  | Uint32 Big -> fun buf base -> UInt32.get_be buf (base + field_off)
  | Uint63 Little -> fun buf base -> UInt63.get_le buf (base + field_off)
  | Uint63 Big -> fun buf base -> UInt63.get_be buf (base + field_off)
  | Uint64 Little -> fun buf base -> Bytes.get_int64_le buf (base + field_off)
  | Uint64 Big -> fun buf base -> Bytes.get_int64_be buf (base + field_off)
  | Byte_array { size = Int n } ->
      fun buf base -> Bytes.sub_string buf (base + field_off) n
  | Where { inner; _ } -> build_field_reader inner field_off
  | Enum { base; _ } -> build_field_reader base field_off
  | Map { inner; decode; _ } ->
      let read = build_field_reader inner field_off in
      fun buf base -> decode (read buf base)
  | Unit -> fun _buf _base -> ()
  | _ -> fun _buf _base -> failwith "build_field_reader: unsupported type"

let decode_record_from_bytes : type r.
    r record_codec -> (bytes -> int -> r) Staged.t =
 fun codec ->
  (* Precompute field offsets and build readers at staging time *)
  let field_info =
    let current_off = Stdlib.ref 0 in
    List.filter_map
      (fun (Field_codec fc) ->
        match field_wire_size fc.typ with
        | Some size ->
            let off = !current_off in
            current_off := !current_off + size;
            (* Build a direct reader at this fixed offset *)
            let reader = build_field_reader fc.typ off in
            let set = fc.set in
            Some (fun buf base acc -> set (reader buf base) acc)
        | None -> None)
      codec.fields
  in
  let default = codec.default in
  if List.length field_info <> List.length codec.fields then
    Staged.stage (fun _buf _base -> default)
  else
    (* Convert to array for iteration *)
    let decoders = Array.of_list field_info in
    let n = Array.length decoders in
    Staged.stage (fun buf base ->
        let acc = Stdlib.ref default in
        for i = 0 to n - 1 do
          acc := decoders.(i) buf base !acc
        done;
        !acc)

(** {2 Zero-alloc decode with explicit types}

    For truly zero intermediate allocations, use these typed decode functions
    that build readers at staging time and call a make function directly. Only
    the final record is allocated - no intermediate records or refs. *)

(** Build zero-alloc decoder for 1-field record *)
let decode_make1 : type a1 r.
    a1 typ -> make:(a1 -> r) -> (bytes -> int -> r) Staged.t =
 fun t1 ~make ->
  let read1 = build_field_reader t1 0 in
  Staged.stage (fun buf base -> make (read1 buf base))

(** Build zero-alloc decoder for 2-field record *)
let decode_make2 : type a1 a2 r.
    a1 typ -> a2 typ -> make:(a1 -> a2 -> r) -> (bytes -> int -> r) Staged.t =
 fun t1 t2 ~make ->
  match (field_wire_size t1, field_wire_size t2) with
  | Some s1, Some _ ->
      let read1 = build_field_reader t1 0 in
      let read2 = build_field_reader t2 s1 in
      Staged.stage (fun buf base -> make (read1 buf base) (read2 buf base))
  | _ -> failwith "decode_make2: variable-size fields not supported"

(** Build zero-alloc decoder for 3-field record *)
let decode_make3 : type a1 a2 a3 r.
    a1 typ ->
    a2 typ ->
    a3 typ ->
    make:(a1 -> a2 -> a3 -> r) ->
    (bytes -> int -> r) Staged.t =
 fun t1 t2 t3 ~make ->
  match (field_wire_size t1, field_wire_size t2, field_wire_size t3) with
  | Some s1, Some s2, Some _ ->
      let read1 = build_field_reader t1 0 in
      let read2 = build_field_reader t2 s1 in
      let read3 = build_field_reader t3 (s1 + s2) in
      Staged.stage (fun buf base ->
          make (read1 buf base) (read2 buf base) (read3 buf base))
  | _ -> failwith "decode_make3: variable-size fields not supported"

(** Build zero-alloc decoder for 4-field record *)
let decode_make4 : type a1 a2 a3 a4 r.
    a1 typ ->
    a2 typ ->
    a3 typ ->
    a4 typ ->
    make:(a1 -> a2 -> a3 -> a4 -> r) ->
    (bytes -> int -> r) Staged.t =
 fun t1 t2 t3 t4 ~make ->
  match
    ( field_wire_size t1,
      field_wire_size t2,
      field_wire_size t3,
      field_wire_size t4 )
  with
  | Some s1, Some s2, Some s3, Some _ ->
      let read1 = build_field_reader t1 0 in
      let read2 = build_field_reader t2 s1 in
      let read3 = build_field_reader t3 (s1 + s2) in
      let read4 = build_field_reader t4 (s1 + s2 + s3) in
      Staged.stage (fun buf base ->
          make (read1 buf base) (read2 buf base) (read3 buf base)
            (read4 buf base))
  | _ -> failwith "decode_make4: variable-size fields not supported"

(** {2 Bounds-checked decode with exceptions}

    Same as decode_make* but with bounds checking that raises Parse_error. *)

(** Build bounds-checked decoder for 1-field record *)
let decode_make1_exn : type a1 r.
    a1 typ -> make:(a1 -> r) -> (bytes -> int -> r) Staged.t =
 fun t1 ~make ->
  match field_wire_size t1 with
  | Some total_size ->
      let read1 = build_field_reader t1 0 in
      Staged.stage (fun buf base ->
          let len = Bytes.length buf in
          if base + total_size > len then
            raise_eof ~expected:total_size ~got:(len - base);
          make (read1 buf base))
  | None -> failwith "decode_make1_exn: variable-size fields not supported"

(** Build bounds-checked decoder for 2-field record *)
let decode_make2_exn : type a1 a2 r.
    a1 typ -> a2 typ -> make:(a1 -> a2 -> r) -> (bytes -> int -> r) Staged.t =
 fun t1 t2 ~make ->
  match (field_wire_size t1, field_wire_size t2) with
  | Some s1, Some s2 ->
      let total_size = s1 + s2 in
      let read1 = build_field_reader t1 0 in
      let read2 = build_field_reader t2 s1 in
      Staged.stage (fun buf base ->
          let len = Bytes.length buf in
          if base + total_size > len then
            raise_eof ~expected:total_size ~got:(len - base);
          make (read1 buf base) (read2 buf base))
  | _ -> failwith "decode_make2_exn: variable-size fields not supported"

(** Build bounds-checked decoder for 3-field record *)
let decode_make3_exn : type a1 a2 a3 r.
    a1 typ ->
    a2 typ ->
    a3 typ ->
    make:(a1 -> a2 -> a3 -> r) ->
    (bytes -> int -> r) Staged.t =
 fun t1 t2 t3 ~make ->
  match (field_wire_size t1, field_wire_size t2, field_wire_size t3) with
  | Some s1, Some s2, Some s3 ->
      let total_size = s1 + s2 + s3 in
      let read1 = build_field_reader t1 0 in
      let read2 = build_field_reader t2 s1 in
      let read3 = build_field_reader t3 (s1 + s2) in
      Staged.stage (fun buf base ->
          let len = Bytes.length buf in
          if base + total_size > len then
            raise_eof ~expected:total_size ~got:(len - base);
          make (read1 buf base) (read2 buf base) (read3 buf base))
  | _ -> failwith "decode_make3_exn: variable-size fields not supported"

(** Build bounds-checked decoder for 4-field record *)
let decode_make4_exn : type a1 a2 a3 a4 r.
    a1 typ ->
    a2 typ ->
    a3 typ ->
    a4 typ ->
    make:(a1 -> a2 -> a3 -> a4 -> r) ->
    (bytes -> int -> r) Staged.t =
 fun t1 t2 t3 t4 ~make ->
  match
    ( field_wire_size t1,
      field_wire_size t2,
      field_wire_size t3,
      field_wire_size t4 )
  with
  | Some s1, Some s2, Some s3, Some s4 ->
      let total_size = s1 + s2 + s3 + s4 in
      let read1 = build_field_reader t1 0 in
      let read2 = build_field_reader t2 s1 in
      let read3 = build_field_reader t3 (s1 + s2) in
      let read4 = build_field_reader t4 (s1 + s2 + s3) in
      Staged.stage (fun buf base ->
          let len = Bytes.length buf in
          if base + total_size > len then
            raise_eof ~expected:total_size ~got:(len - base);
          make (read1 buf base) (read2 buf base) (read3 buf base)
            (read4 buf base))
  | _ -> failwith "decode_make4_exn: variable-size fields not supported"

(** Convert record codec to struct_ for 3D generation *)
let record_to_struct codec =
  let fields =
    List.map
      (fun (Field_codec fc) -> field fc.name ?constraint_:fc.constraint_ fc.typ)
      codec.fields
  in
  struct_ codec.record_name fields

(* ==================== EverParse FFI Helpers ==================== *)

(* NOTE: wire does NOT generate C parsing code. C parsers come from EverParse.
   This section provides helpers for generating OCaml FFI stubs that call
   EverParse-generated C code.

   Workflow:
   1. Define schema in OCaml using wire
   2. Generate .3d file with to_3d
   3. Run EverParse to generate C parser (.h with struct + read/write)
   4. Use to_c_stubs to generate OCaml FFI bindings to call EverParse C *)

(** Compute the fixed wire size of a struct (None if variable-length) *)
let rec size_of_typ : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Uint32 _ -> Some 4
  | Uint64 _ -> Some 8
  | Byte_array { size = Int n } -> Some n
  | Enum { base; _ } -> size_of_int_typ base
  | Where { inner; _ } -> size_of_typ inner
  | Map { inner; _ } -> size_of_typ inner
  | _ -> None

and size_of_int_typ : int typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Enum { base; _ } -> size_of_int_typ base
  | Where { inner; _ } -> size_base inner
  | _ -> None

and size_base : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | _ -> None

let size_of_struct (s : struct_) =
  List.fold_left
    (fun acc (Field f) ->
      match (acc, size_of_typ f.field_typ) with
      | Some a, Some b -> Some (a + b)
      | _ -> None)
    (Some 0) s.fields

(** Compute the EverParse-normalized identifier for a struct name.

    EverParse 3D normalizes C identifiers: names that start with two or more
    consecutive uppercase letters are lowercased with only the first letter
    capitalized (e.g., [CLCW] becomes [Clcw], [TMFrame] becomes [Tmframe]).
    Names with standard camelCase are preserved (e.g., [AllInts] stays
    [AllInts]). *)
let everparse_name name =
  let is_upper c = Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c in
  let len = String.length name in
  let rec count_upper i =
    if i < len && is_upper name.[i] then count_upper (i + 1) else i
  in
  if len > 0 && count_upper 0 >= 2 then
    String.init len (fun i ->
        let c = Char.lowercase_ascii name.[i] in
        if i = 0 then Char.uppercase_ascii c else c)
  else name

(** OCaml type name for a wire type (for generated external declarations). *)
let rec ml_type_of : type a. a typ -> string = function
  | Uint8 -> "int"
  | Uint16 _ -> "int"
  | Uint32 _ -> "int32"
  | Uint64 _ -> "int64"
  | Bits _ -> "int"
  | Enum { base; _ } -> ml_type_of_int base
  | Where { inner; _ } -> ml_type_of inner
  | Map { inner; _ } -> ml_type_of inner
  | Apply { typ; _ } -> ml_type_of typ
  | _ -> failwith "ml_type_of: unsupported type"

and ml_type_of_int : int typ -> string = function
  | Uint8 -> "int"
  | Uint16 _ -> "int"
  | Enum { base; _ } -> ml_type_of_int base
  | Where { inner; _ } -> ml_type_of_int inner
  | Map { inner; _ } -> ml_type_of inner
  | _ -> failwith "ml_type_of_int: unsupported type"

(** C expression to store a C struct field into an OCaml value. *)
let c_to_ml : type a. a typ -> string -> string =
 fun typ c_expr ->
  match ml_type_of typ with
  | "int" -> Fmt.str "Val_int(%s)" c_expr
  | "int32" -> Fmt.str "caml_copy_int32(%s)" c_expr
  | "int64" -> Fmt.str "caml_copy_int64(%s)" c_expr
  | _ -> failwith "c_to_ml: unsupported type"

(** C expression to extract an OCaml value into a C value. *)
let ml_to_c : type a. a typ -> string -> string =
 fun typ ml_expr ->
  match ml_type_of typ with
  | "int" -> Fmt.str "Int_val(%s)" ml_expr
  | "int32" -> Fmt.str "Int32_val(%s)" ml_expr
  | "int64" -> Fmt.str "Int64_val(%s)" ml_expr
  | _ -> failwith "ml_to_c: unsupported type"

(** Does this type require a boxed OCaml allocation (int32/int64)? *)
let is_boxed : type a. a typ -> bool =
 fun typ -> match ml_type_of typ with "int32" | "int64" -> true | _ -> false

(** Named fields with existential type hidden. *)
type named_field = Named : string * 'a typ -> named_field

let named_fields (s : struct_) =
  List.filter_map
    (fun (Field f) ->
      match f.field_name with
      | Some name -> Some (Named (name, f.field_typ))
      | None -> None)
    s.fields

(** Generate C check stub: [bytes -> bool]. Calls EverParse-generated
    [FooCheckFoo] validation function. *)
let c_stub_check ppf (s : struct_) =
  let ep = everparse_name s.name in
  let lower = String.lowercase_ascii s.name in
  (* Define a static error handler for this schema *)
  Fmt.pf ppf
    "static void %s_err(const char *t, const char *f, const char *r,@\n" lower;
  Fmt.pf ppf
    "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {@\n";
  Fmt.pf ppf
    "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;@\n";
  Fmt.pf ppf "}@\n";
  (* Call Validate directly instead of going through the Wrapper.
     No OCaml allocation or GC interaction, so skip CAMLparam/CAMLreturn. *)
  Fmt.pf ppf "CAMLprim value caml_wire_%s_check(value v_buf) {@\n" lower;
  Fmt.pf ppf "  uint8_t *data = (uint8_t *)Bytes_val(v_buf);@\n";
  Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf);@\n";
  Fmt.pf ppf "  uint64_t r = %sValidate%s(NULL, %s_err, data, len, 0);@\n" ep ep
    lower;
  Fmt.pf ppf "  return Val_bool(EverParseIsSuccess(r));@\n";
  Fmt.pf ppf "}@\n@\n"

(** Generate C FFI stubs that call EverParse-generated validators.

    For each struct [Foo], generates:
    - Validation stub: [caml_wire_foo_check(v_buf)] calling [Validate] directly

    The generated code expects EverParse headers and sources to be available via
    [-I] include path. EverParse identifier normalization is handled
    automatically (e.g., [CLCW] becomes [ClcwCheckClcw]). *)
let to_c_stubs (structs : struct_ list) =
  let buf = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf
    "/* wire_stubs.c - OCaml FFI stubs for EverParse-generated C */@\n@\n";
  Fmt.pf ppf "#include <caml/mlvalues.h>@\n";
  Fmt.pf ppf "#include <caml/memory.h>@\n";
  Fmt.pf ppf "#include <stdint.h>@\n@\n";
  Fmt.pf ppf "/* EverParse headers and sources */@\n";
  List.iteri
    (fun i (s : struct_) ->
      if i = 0 then Fmt.pf ppf "#include \"EverParse.h\"@\n";
      Fmt.pf ppf "#include \"%s.h\"@\n" s.name;
      Fmt.pf ppf "#include \"%s.c\"@\n" s.name)
    structs;
  Fmt.pf ppf "@\n/* Validation stubs */@\n";
  List.iter (fun (s : struct_) -> c_stub_check ppf s) structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(** Generate OCaml [external] declarations matching the C stubs from
    {!to_c_stubs}. For each struct [Foo], generates:
    {[
      external foo_check : bytes -> bool = "caml_wire_foo_check"
    ]} *)
let to_ml_stubs (structs : struct_ list) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  List.iter
    (fun (s : struct_) ->
      let lower = String.lowercase_ascii s.name in
      Fmt.pf ppf "external %s_check : bytes -> bool@\n" lower;
      Fmt.pf ppf "  = \"caml_wire_%s_check\" [@@@@noalloc]@\n@\n" lower)
    structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(** Module name for a generated per-struct OCaml stub file. Converts CamelCase
    to snake_case, e.g., [SimpleHeader] maps to ["simple_header"]. *)
let to_ml_stub_name (s : struct_) =
  let name = s.name in
  let buf = Buffer.create (String.length name + 4) in
  String.iteri
    (fun i c ->
      if i > 0 && Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c then
        Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c))
    name;
  Buffer.contents buf

(** Generate a flat OCaml stub module for a single struct. Produces a file with
    an [external check] declaration:
    {[
      (* Generated by wire *)
      external check : bytes -> bool = "caml_wire_foo_check"
    ]} *)
let to_ml_stub (s : struct_) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let lower = String.lowercase_ascii s.name in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  Fmt.pf ppf "external check : bytes -> bool@\n";
  Fmt.pf ppf "  = \"caml_wire_%s_check\"@\n" lower;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* ==================== Struct-level read/write ==================== *)

type parsed_value = Parsed : 'a typ * 'a -> parsed_value
type parsed_struct = (string option * parsed_value) list

let read_struct (s : struct_) buf =
  let reader = Br.of_string buf in
  let dec = decoder reader in
  let rec go ctx acc = function
    | [] -> Ok (List.rev acc)
    | Field { field_name; field_typ; constraint_; _ } :: rest -> (
        let* v, ctx' = parse_with_ctx ctx field_typ dec in
        let ctx' =
          match field_name with
          | Some n -> Ctx.add n (val_to_int field_typ v) ctx'
          | None -> ctx'
        in
        match constraint_ with
        | Some cond when not (eval_expr ctx' cond) ->
            Error (Constraint_failed "field constraint")
        | _ -> go ctx' ((field_name, Parsed (field_typ, v)) :: acc) rest)
  in
  go empty_ctx [] s.fields

let write_struct (s : struct_) (ps : parsed_struct) =
  let buf = Buffer.create 64 in
  let writer = Bw.of_buffer buf in
  let enc = encoder writer in
  let fields_with_vals = List.combine s.fields ps in
  let rec go ctx = function
    | [] -> Ok (Buffer.contents buf)
    | (Field { field_name; constraint_; _ }, (_, Parsed (typ, v))) :: rest -> (
        let ctx' = encode_with_ctx ctx typ v enc in
        let ctx' =
          match field_name with
          | Some n -> Ctx.add n (val_to_int typ v) ctx'
          | None -> ctx'
        in
        match constraint_ with
        | Some cond when not (eval_expr ctx' cond) ->
            Error (Constraint_failed "field constraint")
        | _ -> go ctx' rest)
  in
  go empty_ctx fields_with_vals

(* Capture top-level names before Codec shadows them *)
type struct_field = field

let struct_field = field
let struct' = struct_

module Codec = struct
  type view = { v_buf : bytes; v_off : int }

  type ('a, 'r) field = {
    name : string;
    typ : 'a typ;
    get : 'r -> 'a;
    mutable f_reader : bytes -> int -> 'a;
    mutable f_writer : bytes -> int -> 'a -> unit;
  }

  (* GADT snoc-list of typed field readers, built in forward order by |+.
     ('full, 'remaining) readers tracks:
     - 'full:      the original constructor type
     - 'remaining: what's left after consuming the readers in this list

     Snoc appends at the end, so readers are in field order.
     At seal time, pattern-matching reconstructs the full application
     without partial application closures (for up to 6 fields). *)
  type (_, _) readers =
    | Nil : ('f, 'f) readers
    | Snoc :
        ('full, 'a -> 'rest) readers * (bytes -> int -> 'a)
        -> ('full, 'rest) readers

  (* Bitfield group state: tracks the current base word being packed. *)
  type bf_codec_state = {
    bfc_base : bitfield_base;
    bfc_base_off : int; (* byte offset of base word within record *)
    bfc_bits_used : int; (* bits consumed so far in current group *)
    bfc_total_bits : int; (* 8, 16, or 32 *)
  }

  type ('f, 'r) record =
    | Record : {
        r_name : string;
        r_make : 'full;
        r_readers : ('full, 'f) readers;
        r_writers_rev : ('r -> bytes -> int -> unit) list;
        r_wire_size : int;
        r_fields_rev : struct_field list;
        r_bf : bf_codec_state option;
      }
        -> ('f, 'r) record

  type 'r t = {
    t_name : string;
    t_decode : bytes -> int -> 'r;
    t_encode : 'r -> bytes -> int -> unit;
    t_wire_size : int;
    t_struct_fields : struct_field list;
  }

  let record name make =
    Record
      {
        r_name = name;
        r_make = make;
        r_readers = Nil;
        r_writers_rev = [];
        r_wire_size = 0;
        r_fields_rev = [];
        r_bf = None;
      }

  let field name typ get =
    {
      name;
      typ;
      get;
      f_reader = (fun _ _ -> failwith "field: not added to a record yet");
      f_writer = (fun _ _ _ -> failwith "field: not added to a record yet");
    }

  (* Bitfield helpers *)

  let bf_base_byte_size = function BF_U8 -> 1 | BF_U16 _ -> 2 | BF_U32 _ -> 4

  let bf_base_total_bits = function
    | BF_U8 -> 8
    | BF_U16 _ -> 16
    | BF_U32 _ -> 32

  let bf_base_equal a b =
    match (a, b) with
    | BF_U8, BF_U8 -> true
    | BF_U16 e1, BF_U16 e2 -> e1 = e2
    | BF_U32 e1, BF_U32 e2 -> e1 = e2
    | _ -> false

  let bf_read_base base buf off =
    match base with
    | BF_U8 -> Bytes.get_uint8 buf off
    | BF_U16 Little -> Bytes.get_uint16_le buf off
    | BF_U16 Big -> Bytes.get_uint16_be buf off
    | BF_U32 Little -> UInt32.get_le buf off
    | BF_U32 Big -> UInt32.get_be buf off

  let bf_write_base base buf off v =
    match base with
    | BF_U8 -> Bytes.set_uint8 buf off v
    | BF_U16 Little -> Bytes.set_uint16_le buf off v
    | BF_U16 Big -> Bytes.set_uint16_be buf off v
    | BF_U32 Little -> UInt32.set_le buf off v
    | BF_U32 Big -> UInt32.set_be buf off v

  let build_bf_reader base byte_off shift width =
    let mask = (1 lsl width) - 1 in
    fun buf off -> (bf_read_base base buf (off + byte_off) lsr shift) land mask

  let build_bf_writer base byte_off shift width =
    let mask = (1 lsl width) - 1 in
    fun buf off value ->
      let cur = bf_read_base base buf (off + byte_off) in
      bf_write_base base buf (off + byte_off)
        (cur lor ((value land mask) lsl shift))

  let build_bf_accessor_writer base byte_off shift width =
    let mask = (1 lsl width) - 1 in
    let clear_mask = lnot (mask lsl shift) in
    fun buf off value ->
      let cur = bf_read_base base buf (off + byte_off) in
      bf_write_base base buf (off + byte_off)
        (cur land clear_mask lor ((value land mask) lsl shift))

  let build_bf_clear base byte_off =
   fun buf off -> bf_write_base base buf (off + byte_off) 0

  let ( |+ ) : type a f r. (a -> f, r) record -> (a, r) field -> (f, r) record =
   fun (Record r) ({ name; typ; get; _ } as fld) ->
    (* Recursively unwrap Map layers to reach the wire-level type, composing
       encode/decode conversions along the way. *)
    let rec add : type w.
        w typ ->
        (r -> w) ->
        ((bytes -> int -> w) -> bytes -> int -> a) ->
        ((bytes -> int -> w -> unit) -> bytes -> int -> a -> unit) ->
        (f, r) record =
     fun typ get_wire wrap_reader wrap_writer ->
      match typ with
      | Map { inner; decode; encode } ->
          add inner
            (fun v -> encode (get_wire v))
            (fun reader -> wrap_reader (fun buf off -> decode (reader buf off)))
            (fun writer ->
              wrap_writer (fun buf off value -> writer buf off (encode value)))
      | Bits { width; base } ->
          let total = bf_base_total_bits base in
          let need_new_group =
            match r.r_bf with
            | None -> true
            | Some bf ->
                (not (bf_base_equal bf.bfc_base base))
                || bf.bfc_bits_used + width > bf.bfc_total_bits
          in
          let base_off, bits_used, size_delta, extra_writers =
            if need_new_group then
              let base_off = r.r_wire_size in
              let clear = build_bf_clear base base_off in
              ( base_off,
                0,
                bf_base_byte_size base,
                [ (fun _v buf off -> clear buf off) ] )
            else
              let bf = Option.get r.r_bf in
              (bf.bfc_base_off, bf.bfc_bits_used, 0, [])
          in
          let shift = total - bits_used - width in
          let raw_reader = build_bf_reader base base_off shift width in
          let raw_writer = build_bf_writer base base_off shift width in
          let accessor_writer =
            build_bf_accessor_writer base base_off shift width
          in
          fld.f_reader <- wrap_reader raw_reader;
          fld.f_writer <- wrap_writer accessor_writer;
          let new_bf =
            {
              bfc_base = base;
              bfc_base_off = base_off;
              bfc_bits_used = bits_used + width;
              bfc_total_bits = total;
            }
          in
          Record
            {
              r_name = r.r_name;
              r_make = r.r_make;
              r_readers = Snoc (r.r_readers, wrap_reader raw_reader);
              r_writers_rev =
                (fun v buf off -> raw_writer buf off (get_wire v))
                :: (extra_writers @ r.r_writers_rev);
              r_wire_size = r.r_wire_size + size_delta;
              r_fields_rev = struct_field name typ :: r.r_fields_rev;
              r_bf = Some new_bf;
            }
      | _ ->
          let fsize =
            match field_wire_size typ with
            | Some s -> s
            | None -> failwith "Codec.(|+): variable-size fields not supported"
          in
          let field_off = r.r_wire_size in
          let raw_reader = build_field_reader typ field_off in
          let raw_encoder = build_field_encoder typ in
          fld.f_reader <- wrap_reader raw_reader;
          fld.f_writer <-
            wrap_writer (fun buf off value ->
                let _ = raw_encoder buf (off + field_off) value in
                ());
          Record
            {
              r_name = r.r_name;
              r_make = r.r_make;
              r_readers = Snoc (r.r_readers, wrap_reader raw_reader);
              r_writers_rev =
                (fun v buf off ->
                  let _ = raw_encoder buf (off + field_off) (get_wire v) in
                  ())
                :: r.r_writers_rev;
              r_wire_size = r.r_wire_size + fsize;
              r_fields_rev = struct_field name typ :: r.r_fields_rev;
              r_bf = None;
            }
    in
    add typ get (fun reader -> reader) (fun writer -> writer)

  (* Chunked application: peels off up to 6 fields per recursion step.
     Cost: ceil(n/6) - 1 partial applications instead of n - 1. *)
  let rec apply_readers : type full current.
      full -> (full, current) readers -> bytes -> int -> current =
   fun make readers buf off ->
    match readers with
    | Nil -> make
    | Snoc (Nil, r1) -> make (r1 buf off)
    | Snoc (Snoc (Nil, r1), r2) -> make (r1 buf off) (r2 buf off)
    | Snoc (Snoc (Snoc (Nil, r1), r2), r3) ->
        make (r1 buf off) (r2 buf off) (r3 buf off)
    | Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4) ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
    | Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5) ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
    | Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (rest, r1), r2), r3), r4), r5), r6) ->
        let f = apply_readers make rest buf off in
        f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off)

  let seal : type r. (r, r) record -> r t =
   fun (Record r) ->
    let total = r.r_wire_size in
    let writers = Array.of_list (List.rev r.r_writers_rev) in
    let n_writers = Array.length writers in
    let build_decode : type full. full -> (full, r) readers -> bytes -> int -> r
        =
     fun make readers ->
      match readers with
      | Nil -> fun _buf _off -> make
      | Snoc (Nil, r1) -> fun buf off -> make (r1 buf off)
      | Snoc (Snoc (Nil, r1), r2) ->
          fun buf off -> make (r1 buf off) (r2 buf off)
      | Snoc (Snoc (Snoc (Nil, r1), r2), r3) ->
          fun buf off -> make (r1 buf off) (r2 buf off) (r3 buf off)
      | Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
      | Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off)
      | Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off)
      | Snoc
          ( Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6),
            r7 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off)
      | Snoc
          ( Snoc
              ( Snoc
                  ( Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5),
                    r6 ),
                r7 ),
            r8 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off)
      | Snoc
          ( Snoc
              ( Snoc
                  ( Snoc
                      ( Snoc
                          (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5),
                        r6 ),
                    r7 ),
                r8 ),
            r9 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off)
      | Snoc
          ( Snoc
              ( Snoc
                  ( Snoc
                      ( Snoc
                          ( Snoc
                              ( Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4),
                                r5 ),
                            r6 ),
                        r7 ),
                    r8 ),
                r9 ),
            r10 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off)
              (r10 buf off)
      | Snoc
          ( Snoc
              ( Snoc
                  ( Snoc
                      ( Snoc
                          ( Snoc
                              ( Snoc
                                  ( Snoc
                                      (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4),
                                    r5 ),
                                r6 ),
                            r7 ),
                        r8 ),
                    r9 ),
                r10 ),
            r11 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off)
              (r10 buf off) (r11 buf off)
      | Snoc
          ( Snoc
              ( Snoc
                  ( Snoc
                      ( Snoc
                          ( Snoc
                              ( Snoc
                                  ( Snoc
                                      ( Snoc
                                          ( Snoc (Snoc (Snoc (Nil, r1), r2), r3),
                                            r4 ),
                                        r5 ),
                                    r6 ),
                                r7 ),
                            r8 ),
                        r9 ),
                    r10 ),
                r11 ),
            r12 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off)
              (r10 buf off) (r11 buf off) (r12 buf off)
      | Snoc
          ( Snoc
              ( Snoc
                  ( Snoc
                      ( Snoc
                          ( Snoc
                              ( Snoc
                                  ( Snoc
                                      ( Snoc
                                          ( Snoc
                                              ( Snoc
                                                  (Snoc (Snoc (Nil, r1), r2), r3),
                                                r4 ),
                                            r5 ),
                                        r6 ),
                                    r7 ),
                                r8 ),
                            r9 ),
                        r10 ),
                    r11 ),
                r12 ),
            r13 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off) (r9 buf off)
              (r10 buf off) (r11 buf off) (r12 buf off) (r13 buf off)
      | readers -> fun buf off -> apply_readers make readers buf off
    in
    let raw_decode = build_decode r.r_make r.r_readers in
    {
      t_name = r.r_name;
      t_decode =
        (fun buf off ->
          if off + total > Bytes.length buf then
            raise_eof ~expected:total ~got:(Bytes.length buf - off);
          raw_decode buf off);
      t_encode =
        (fun v buf off ->
          for i = 0 to n_writers - 1 do
            writers.(i) v buf off
          done);
      t_wire_size = total;
      t_struct_fields = List.rev r.r_fields_rev;
    }

  let wire_size t = t.t_wire_size
  let decode t buf off = t.t_decode buf off
  let encode t v buf off = t.t_encode v buf off
  let to_struct t = struct' t.t_name t.t_struct_fields

  let view t buf off =
    if off + t.t_wire_size > Bytes.length buf then
      raise_eof ~expected:t.t_wire_size ~got:(Bytes.length buf - off);
    { v_buf = buf; v_off = off }

  let get f v = f.f_reader v.v_buf v.v_off
  let set f v x = f.f_writer v.v_buf v.v_off x
end

module Record = struct
  type ('a, 'r) field = ('a, 'r) field_codec
  type 'r t = 'r record_codec

  let field = field_codec
  let ( @: ) name (typ, get, set) = field_codec name typ ~get ~set

  let ( @:? ) name (constraint_, typ, get, set) =
    field_codec name ~constraint_ typ ~get ~set

  let record name ~default fields =
    record_codec name ~default (List.map pack_field fields)

  let encode codec = encode_record_to_slice codec
  let decode codec = decode_record_from_slice codec
  let encode_bytes codec = encode_record_to_bytes codec
  let decode_bytes codec = decode_record_from_bytes codec
  let to_struct = record_to_struct
end
