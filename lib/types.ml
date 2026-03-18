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
let byte_slice ~size = Byte_slice { size }
let single_elem_array ~size elem = Single_elem { size; elem; at_most = false }

let single_elem_array_at_most ~size elem =
  Single_elem { size; elem; at_most = true }

let enum name cases base = Enum { name; cases; base }

let variants name cases base =
  let enum_cases = List.mapi (fun i (s, _) -> (s, i)) cases in
  let arr = Array.of_list (List.map snd cases) in
  let decode n =
    if n >= 0 && n < Array.length arr then arr.(n)
    else Fmt.invalid_arg "Wire.variants %s: unknown value %d" name n
  in
  let encode v =
    let rec go i =
      if i >= Array.length arr then
        Fmt.invalid_arg "Wire.variants %s: unknown variant" name
      else if arr.(i) = v then i
      else go (i + 1)
    in
    go 0
  in
  map decode encode (enum name enum_cases base)

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
type module_ = { doc : string option; decls : decl list }

(* Extract enum declarations needed by struct fields. Scans field types for
   Enum constructors (including under Map/Where wrappers) and returns the
   corresponding enum_decl entries, deduplicated by name. *)
let enum_decls (s : struct_) : decl list =
  let seen = Hashtbl.create 4 in
  let decls = Stdlib.ref [] in
  List.iter
    (fun (Field f) ->
      let rec extract : type a. a typ -> unit = function
        | Enum { name; cases; base } when not (Hashtbl.mem seen name) ->
            Hashtbl.add seen name ();
            decls := Enum_decl { name; cases; base = Pack_typ base } :: !decls
        | Map { inner; _ } -> extract inner
        | Where { inner; _ } -> extract inner
        | _ -> ()
      in
      extract f.field_typ)
    s.fields;
  List.rev !decls

let module_ ?doc decls =
  (* Auto-prepend enum declarations for any enum types used in typedefs
     that aren't already declared in the module. *)
  let already_declared =
    List.fold_left
      (fun acc d ->
        match d with Enum_decl { name; _ } -> name :: acc | _ -> acc)
      [] decls
  in
  let extra =
    List.fold_left
      (fun acc d ->
        match d with
        | Typedef { struct_; _ } ->
            List.filter
              (fun e ->
                match e with
                | Enum_decl { name; _ } ->
                    not
                      (List.mem name already_declared
                      || List.exists
                           (function
                             | Enum_decl { name = n; _ } -> String.equal n name
                             | _ -> false)
                           acc)
                | _ -> false)
              (enum_decls struct_)
            @ acc
        | _ -> acc)
      [] decls
  in
  { doc; decls = List.rev extra @ decls }

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
  | Byte_array { size } | Byte_slice { size } ->
      Fmt.pf ppf "UINT8[:byte-size %a]" pp_expr size
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
  | Byte_array { size } | Byte_slice { size } ->
      (Byte_array size, fun ppf -> Fmt.string ppf "UINT8")
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

(* Parse errors — shared between streaming parser and Codec *)

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

(** Compute wire size of a type (None for variable-size types). *)
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
  | Byte_array { size = Int n } | Byte_slice { size = Int n } -> Some n
  | Where { inner; _ } -> field_wire_size inner
  | Enum { base; _ } -> field_wire_size base
  | Map { inner; _ } -> field_wire_size inner
  | _ -> None
