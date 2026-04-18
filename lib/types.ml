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
  ph_name : string;
  ph_typ : 'a typ;
  ph_packed_typ : packed_typ;
  ph_mutable : bool;
  ph_cell : int ref;
  mutable ph_slot : int;
  mutable ph_env_idx : int;
}

and packed_typ = Pack_typ : 'a typ -> packed_typ

(* Expressions *)
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
  | If_then_else : bool expr * int expr * int expr -> int expr

(* Bitfield base types - standalone, not mutually recursive *)
and bitfield_base = BF_U8 | BF_U16 of endian | BF_U32 of endian

(* Types *)
and _ typ =
  | Uint8 : int typ
  | Uint16 : endian -> int typ
  | Uint32 : endian -> UInt32.t typ
  | Uint63 : endian -> UInt63.t typ
  | Uint64 : endian -> int64 typ (* boxed, for full 64-bit *)
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
  | Where : { cond : bool expr; inner : 'a typ } -> 'a typ
  | Array : {
      len : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ
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
      tag : int typ;
      cases : 'a case_branch list;
    }
      -> 'a typ
  | Struct : struct_ -> unit typ
  | Type_ref : string -> 'a typ
  | Qualified_ref : { module_ : string; name : string } -> 'a typ
  | Map : { inner : 'w typ; decode : 'w -> 'a; encode : 'a -> 'w } -> 'a typ
  | Apply : { typ : 'a typ; args : packed_expr list } -> 'a typ
  | Codec : {
      codec_name : string;
      codec_decode : bytes -> int -> 'r;
      codec_encode : 'r -> bytes -> int -> unit;
      codec_fixed_size : int option;
      codec_size_of : bytes -> int -> int;
      codec_field_readers : (string * (bytes -> int -> int)) list;
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

and 'a case_branch =
  | Case_branch : {
      cb_tag : int option;
      cb_inner : 'w typ;
      cb_inject : 'w -> 'a;
      cb_project : 'a -> 'w option;
    }
      -> 'a case_branch

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

(* Actions *)
and action = On_success of action_stmt list | On_act of action_stmt list

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

type param_env = { pe_codec_id : int; pe_slots : int array }

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

let uint ?(endian = Big) size =
  (match size with
  | Int n when n < 1 || n > 7 ->
      Fmt.invalid_arg "uint: size must be 1-7, got %d" n
  | _ -> ());
  Uint_var { size; endian }

(* Bitfield bases *)
let bf_uint8 = BF_U8
let bf_uint16 = BF_U16 Little
let bf_uint16be = BF_U16 Big
let bf_uint32 = BF_U32 Little
let bf_uint32be = BF_U32 Big
let bits ?(bit_order = Msb_first) ~width base = Bits { width; base; bit_order }
let bit b = Bool.to_int b
let is_set n = n <> 0
let map decode encode inner = Map { inner; decode; encode }
let bool inner = Map { inner; decode = is_set; encode = bit }

(* Parse errors -- moved here so combinators like [cases] can raise them
   directly rather than through intermediate exceptions. *)

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }

exception Parse_error of parse_error

let cases variants inner =
  let arr = Array.of_list variants in
  let decode n =
    if n >= 0 && n < Array.length arr then arr.(n)
    else raise (Parse_error (Invalid_tag n))
  in
  let encode v =
    let rec go i =
      if i >= Array.length arr then invalid_arg "Wire.lookup: unknown variant"
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

let seq_list : ('a, 'a list) seq_map =
  Seq_map
    {
      empty = [];
      add = (fun acc x -> x :: acc);
      finish = List.rev;
      iter = List.iter;
    }

let array ~len elem = Array { len; elem; seq = seq_list }
let array_seq seq ~len elem = Array { len; elem; seq }
let byte_array ~size = Byte_array { size }
let byte_slice ~size = Byte_slice { size }
let optional present inner = Optional { present; inner }
let optional_or present ~default inner = Optional_or { present; inner; default }
let repeat ~size elem = Repeat { size; elem; seq = seq_list }
let repeat_seq seq ~size elem = Repeat { size; elem; seq }
let nested ~size elem = Single_elem { size; elem; at_most = false }
let nested_at_most ~size elem = Single_elem { size; elem; at_most = true }
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
type 'a case_def =
  | Case_def : {
      cd_index : int option;
      cd_inner : 'w typ;
      cd_inject : 'w -> 'a;
      cd_project : 'a -> 'w option;
    }
      -> 'a case_def
  | Default_def : {
      dd_inner : 'w typ;
      dd_inject : 'w -> 'a;
      dd_project : 'a -> 'w option;
    }
      -> 'a case_def

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

let casetype ?(first = 0) ?(step = 1) name tag defs =
  let counter = Stdlib.ref first in
  let resolve = function
    | Case_def { cd_index; cd_inner; cd_inject; cd_project } ->
        let idx =
          match cd_index with
          | Some i ->
              counter := i + step;
              i
          | None ->
              let i = !counter in
              counter := i + step;
              i
        in
        Case_branch
          {
            cb_tag = Some idx;
            cb_inner = cd_inner;
            cb_inject = cd_inject;
            cb_project = cd_project;
          }
    | Default_def { dd_inner; dd_inject; dd_project } ->
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
let field name ?constraint_ ?action typ =
  Field { field_name = Some name; field_typ = typ; constraint_; action }

let anon_field typ =
  Field
    { field_name = None; field_typ = typ; constraint_ = None; action = None }

(* Struct constructors *)
let struct_ name fields = { name; params = []; where = None; fields }
let struct_name s = s.name
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
type ocaml_kind = K_int | K_int64 | K_bool | K_string | K_unit

let rec ocaml_kind_of : type a. a typ -> ocaml_kind = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint_var _ -> K_int
  | Uint64 _ -> K_int64
  | Bits _ -> K_int
  | Map { inner = Bits _; decode = _; encode = _ } ->
      (* bool (bits ~width:1 ...) maps to bool; other maps stay int *)
      (* We can't distinguish bool from other maps here without checking
         the decode function. Use K_int as safe default -- the EverParse
         output struct stores the raw int anyway. *)
      K_int
  | Map { inner; _ } -> ocaml_kind_of inner
  | Enum { base; _ } -> ocaml_kind_of base
  | Where { inner; _ } -> ocaml_kind_of inner
  | Byte_array _ -> K_string
  | Byte_slice _ -> K_string (* approximate: slice becomes string in output *)
  | Unit | All_bytes | All_zeros -> K_unit
  | _ -> K_int (* fallback *)

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
let apply typ args = Apply { typ; args = List.map (fun e -> Pack_expr e) args }

(* Type references *)
let type_ref name = Type_ref name
let qualified_ref module_ name = Qualified_ref { module_; name }

(* Actions *)
let on_success stmts = On_success stmts
let on_act stmts = On_act stmts
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
   mapping is OCaml-only). *)
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
        | Enum { name; cases; base }
          when (not (Hashtbl.mem seen name)) && not (is_bits base) ->
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

(* 3D and C reserved words that cannot be used as field/param names. *)
module Reserved_3d = Set.Make (String)

let reserved_3d =
  Reserved_3d.of_list
    (String.split_on_char ' '
       "typedef struct casetype switch case default enum extern mutable \
        entrypoint export output where if else return abort var unit bool true \
        false sizeof this int char void float double long short unsigned \
        signed static const volatile auto register union while for do break \
        continue goto type inline UINT8 UINT16 UINT16BE UINT32 UINT32BE UINT64 \
        UINT64BE Bool PUINT8")

let escape_3d name =
  if Reserved_3d.mem name reserved_3d then name ^ "_" else name

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
  | Ref name -> Fmt.string ppf (escape_3d name)
  | Param_ref p -> Fmt.string ppf (escape_3d p.ph_name)
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
  | If_then_else (c, t, e) ->
      Fmt.pf ppf "((%a) ? %a : %a)" pp_expr c pp_expr t pp_expr e

and pp_typ : type a. a typ Fmt.t =
 fun ppf typ ->
  match typ with
  | Uint8 -> Fmt.string ppf "UINT8"
  | Uint16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | Uint32 e -> Fmt.pf ppf "UINT32%a" pp_endian e
  | Uint63 e -> Fmt.pf ppf "UINT63%a" pp_endian e
  | Uint64 e -> Fmt.pf ppf "UINT64%a" pp_endian e
  | Uint_var { size; endian } ->
      Fmt.pf ppf "UINT%a(%a)" pp_endian endian pp_expr size
  | Bits { base; _ } -> pp_bitfield_base ppf base
  | Unit -> Fmt.string ppf "unit"
  | All_bytes -> Fmt.string ppf "all_bytes"
  | All_zeros -> Fmt.string ppf "all_zeros"
  | Where { cond; inner } -> Fmt.pf ppf "%a { %a }" pp_typ inner pp_expr cond
  | Array { len; elem; _ } -> Fmt.pf ppf "%a[%a]" pp_typ elem pp_expr len
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
  | Codec { codec_name; _ } -> Fmt.string ppf codec_name
  | Optional { present = Bool true; inner } -> pp_typ ppf inner
  | Optional { present = Bool false; _ } -> Fmt.string ppf "UINT8"
  | Optional { inner; _ } -> Fmt.pf ppf "optional(%a)" pp_typ inner
  | Optional_or { present = Bool true; inner; _ } -> pp_typ ppf inner
  | Optional_or { present = Bool false; _ } -> Fmt.string ppf "UINT8"
  | Optional_or { inner; _ } -> Fmt.pf ppf "optional(%a)" pp_typ inner
  | Repeat { elem; _ } -> pp_typ ppf elem

and pp_packed_expr ppf (Pack_expr e) = pp_expr ppf e

let rec pp_action_stmt ppf = function
  | Assign (p, e) -> Fmt.pf ppf "*%s = %a;" (escape_3d p.ph_name) pp_expr e
  | Field_assign (ptr, field_name, e) ->
      Fmt.pf ppf "%s->%s = %a;" ptr field_name pp_expr e
  | Extern_call (fn, args) -> Fmt.pf ppf "%s(%s);" fn (String.concat ", " args)
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
  | Var (name, e) -> Fmt.pf ppf "var %s = %a;" (escape_3d name) pp_expr e

let pp_action ppf = function
  | On_success stmts ->
      Fmt.pf ppf "@[<h>{:on-success %a }@]"
        Fmt.(list ~sep:sp pp_action_stmt)
        stmts
  | On_act stmts ->
      Fmt.pf ppf "@[<h>{:act %a }@]" Fmt.(list ~sep:sp pp_action_stmt) stmts

(* Extract field suffix for arrays - the modifier goes after the field name *)
type field_suffix =
  | No_suffix
  | Bitwidth of int
  | Byte_array of int expr
  | Single_elem of { size : int expr; at_most : bool }
  | Array of int expr

let rec inner_wire_size : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Uint32 _ -> Some 4
  | Uint64 _ -> Some 8
  | Bits { base = BF_U8; _ } -> Some 1
  | Bits { base = BF_U16 _; _ } -> Some 2
  | Bits { base = BF_U32 _; _ } -> Some 4
  | Unit -> Some 0
  | Map { inner; _ } -> inner_wire_size inner
  | Enum { base; _ } -> inner_wire_size base
  | Where { inner; _ } -> inner_wire_size inner
  | _ -> None

and optional_suffix : type a.
    bool expr -> a typ -> field_suffix * (Format.formatter -> unit) =
 fun present inner ->
  match inner_wire_size inner with
  | Some n ->
      let size = If_then_else (present, Int n, Int 0) in
      (Byte_array size, fun ppf -> pp_typ ppf inner)
  | None -> (No_suffix, fun ppf -> Fmt.pf ppf "optional(%a)" pp_typ inner)

and field_suffix : type a. a typ -> field_suffix * (Format.formatter -> unit) =
 fun typ ->
  match typ with
  | Bits { width; base; _ } ->
      (Bitwidth width, fun ppf -> pp_bitfield_base ppf base)
  | Uint_var { size; _ } -> (Byte_array size, fun ppf -> Fmt.string ppf "UINT8")
  | Byte_array { size } | Byte_slice { size } ->
      (Byte_array size, fun ppf -> Fmt.string ppf "UINT8")
  | Single_elem { size; elem; at_most } ->
      (Single_elem { size; at_most }, fun ppf -> pp_typ ppf elem)
  | Array { len; elem; _ } -> (Array len, fun ppf -> pp_typ ppf elem)
  | Map { inner; _ } -> field_suffix inner
  | Enum { base; _ } -> field_suffix base
  | Optional { present = Bool true; inner } -> field_suffix inner
  | Optional { present = Bool false; _ } ->
      (Byte_array (Int 0), fun ppf -> Fmt.string ppf "UINT8")
  | Optional { present; inner } -> optional_suffix present inner
  | Optional_or { present = Bool true; inner; _ } -> field_suffix inner
  | Optional_or { present = Bool false; _ } ->
      (Byte_array (Int 0), fun ppf -> Fmt.string ppf "UINT8")
  | Optional_or { present; inner; _ } -> optional_suffix present inner
  | Repeat { size; elem; _ } ->
      (* Variable-length array with byte-size budget *)
      (Byte_array size, fun ppf -> pp_typ ppf elem)
  | _ -> (No_suffix, fun ppf -> pp_typ ppf typ)

let anon_counter = Stdlib.ref 0

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

let pp_field ppf (Field f) =
  let name =
    match f.field_name with
    | Some name -> escape_3d name
    | None ->
        let n = !anon_counter in
        incr anon_counter;
        Fmt.str "_anon_%d" n
  in
  (* Extract Where constraints from the type so they appear as field
     constraints in the 3D output, not inline in the type. *)
  let where_cond, typ = extract_where_constraint f.field_typ in
  let constraint_ = combine_constraints f.constraint_ where_cond in
  let suffix, pp_base = field_suffix typ in
  Fmt.pf ppf "@,%t %s" pp_base name;
  (match suffix with
  | No_suffix -> ()
  | Bitwidth w -> Fmt.pf ppf " : %d" w
  | Byte_array size -> Fmt.pf ppf "[:byte-size %a]" pp_expr size
  | Single_elem { size; at_most = false } ->
      Fmt.pf ppf "[:byte-size-single-element-array %a]" pp_expr size
  | Single_elem { size; at_most = true } ->
      Fmt.pf ppf "[:byte-size-single-element-array-at-most %a]" pp_expr size
  | Array len -> Fmt.pf ppf "[%a]" pp_expr len);
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

let pp_struct ppf (s : struct_) =
  anon_counter := 0;
  let name = escape_3d s.name in
  Fmt.pf ppf "typedef struct _%s%a" name pp_params s.params;
  Option.iter (Fmt.pf ppf "@,where (%a)" pp_expr) s.where;
  Fmt.pf ppf "@,{@[<v 2>";
  List.iter (pp_field ppf) s.fields;
  Fmt.pf ppf "@]@,} %s" name

let pp_decl ppf = function
  | Typedef { entrypoint; export; output; extern_; doc; struct_ = st } ->
      Option.iter (Fmt.pf ppf "/*++ %s --*/@,") doc;
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
  | Uint_var { size = Int n; _ } -> Some n
  | Uint_var _ -> None
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
  | Codec { codec_fixed_size; _ } -> codec_fixed_size
  | Optional { present = Bool true; inner } -> field_wire_size inner
  | Optional { present = Bool false; _ } -> Some 0
  | Optional _ -> None
  | Optional_or { present = Bool true; inner; _ } -> field_wire_size inner
  | Optional_or { present = Bool false; _ } -> Some 0
  | Optional_or _ -> None
  | Repeat _ -> None
  | _ -> None

let c_type_of : type a. a typ -> string = function
  | Uint8 | Bits { base = BF_U8; _ } -> "uint8_t"
  | Uint16 _ | Bits { base = BF_U16 _; _ } -> "uint16_t"
  | Uint32 _ | Uint63 _ | Bits { base = BF_U32 _; _ } -> "uint32_t"
  | Uint64 _ -> "uint64_t"
  | Uint_var _ -> "uint32_t"
  | _ -> "uint32_t"

let ml_type_of : type a. a typ -> string = function
  | Uint8 | Uint16 _ | Uint_var _ | Bits _ -> "int"
  | Uint32 _ | Uint63 _ -> "int"
  | Uint64 _ -> "int64"
  | _ -> "int"
