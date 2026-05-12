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
  | Map : { inner : 'w typ; decode : 'w -> 'a; encode : 'a -> 'w } -> 'a typ
  | Apply : { typ : 'a typ; args : packed_expr list } -> 'a typ
  | Codec : {
      codec_name : string;
      codec_decode : bytes -> int -> 'r;
      codec_encode : 'r -> bytes -> int -> unit;
      codec_fixed_size : int option;
      codec_size_of : bytes -> int -> int;
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
      cb_inner : 'w typ;
      cb_inject : 'w -> 'a;
      cb_project : 'a -> 'w option;
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
let bf_uint8 = BF_U8
let bf_uint16 = BF_U16 Little
let bf_uint16be = BF_U16 Big
let bf_uint32 = BF_U32 Little
let bf_uint32be = BF_U32 Big
let bits ?(bit_order = Msb_first) ~width base = Bits { width; base; bit_order }
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
  Map { inner; decode; encode }

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
  Map { inner; decode; encode }

let unit = Unit
let all_bytes = All_bytes
let all_zeros = All_zeros

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

let reject_variable_size ~combinator t =
  if not (has_wire_size_expr t) then
    Fmt.invalid_arg
      "Wire.%s: inner type must expose a wire size -- 3D projects %s through a \
       [byte-size] suffix that needs the element width."
      combinator combinator

let array ~len elem =
  reject_decoration ~combinator:"array" elem;
  reject_variable_size ~combinator:"array" elem;
  Array { len; elem; seq = seq_list }

let array_seq seq ~len elem =
  reject_decoration ~combinator:"array_seq" elem;
  reject_variable_size ~combinator:"array_seq" elem;
  Array { len; elem; seq }

let byte_array ~size = Byte_array { size }
let byte_array_where_counter = Stdlib.ref 0
let elt_var_prefix = "__elt_"

let byte_array_where ~size ~per_byte =
  let n = !byte_array_where_counter in
  Stdlib.incr byte_array_where_counter;
  let elt_var = elt_var_prefix ^ string_of_int n in
  let cond = per_byte (Ref elt_var) in
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
  reject_variable_size ~combinator:"optional" inner;
  Optional { present; inner }

let optional_or present ~default inner =
  reject_variable_size ~combinator:"optional_or" inner;
  Optional_or { present; inner; default }

(* [repeat ~size elem] is more permissive than [array]: 3D's
   [<elem>[:byte-size <size>]] parses elements until the byte budget is
   exhausted, so variable-size elements are fine as long as each one is
   self-bounded by its 3D type (struct, codec, ...). The only thing we
   refuse is a field decoration nested inside [elem]. *)
let repeat ~size elem =
  reject_decoration ~combinator:"repeat" elem;
  Repeat { size; elem; seq = seq_list }

let repeat_seq seq ~size elem =
  reject_decoration ~combinator:"repeat_seq" elem;
  Repeat { size; elem; seq }

let nested ~size elem = Single_elem { size; elem; at_most = false }
let nested_at_most ~size elem = Single_elem { size; elem; at_most = true }
let enum name cases base = Enum { name; cases; base }

let variants name cases base =
  let enum_cases = List.mapi (fun i (s, _) -> (s, i)) cases in
  let arr = Array.of_list (List.map snd cases) in
  let len = Array.length arr in
  let decode n =
    if n >= 0 && n < len then arr.(n)
    else Fmt.invalid_arg "Wire.variants %s: unknown value %d" name n
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
      dd_inject : 'w -> 'a;
      dd_project : 'a -> 'w option;
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

let casetype name tag defs =
  let resolve = function
    | Case_def { cd_index; cd_inner; cd_inject; cd_project } ->
        reject_decoration ~combinator:"casetype" cd_inner;
        let cb_tag =
          match cd_index with
          | Some _ as k -> k
          | None ->
              invalid_arg
                "Wire.casetype: every case must supply an explicit [~index]"
        in
        Case_branch
          {
            cb_tag;
            cb_inner = cd_inner;
            cb_inject = cd_inject;
            cb_project = cd_project;
          }
    | Default_def { dd_inner; dd_inject; dd_project } ->
        reject_decoration ~combinator:"casetype" dd_inner;
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
type ocaml_kind = Int | Int64 | Float32 | Float64 | Bool | String | Unit

let rec ocaml_kind_of : type a. a typ -> ocaml_kind = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint_var _ -> Int
  | Uint64 _ -> Int64
  | Int8 | Int16 _ | Int32 _ -> Int
  | Int64 _ -> Int64
  | Float32 _ -> Float32
  | Float64 _ -> Float64
  | Bits _ -> Int
  | Map { inner = Bits _; decode = _; encode = _ } ->
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
      (Apply { typ = Type_ref body_name; args = [ Pack_expr (Ref "tag") ] })
  in
  let wrapper = typedef (struct_ name [ tag_field; body_field ]) in
  (dispatch, wrapper)

let casetype_decls_of_struct (s : struct_) : decl list =
  let seen = Hashtbl.create 4 in
  let acc = Stdlib.ref [] in
  let rec extract : type a. a typ -> unit = function
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
    | Codec { codec_name; codec_struct; _ }
      when not (Hashtbl.mem seen codec_name) ->
        (* Embedded sub-codec: emit its struct alongside the parent so 3D
           references to [codec_name] resolve. Recurse into the sub-codec's
           own fields to catch nested casetype / sub-codec dependencies. *)
        Hashtbl.add seen codec_name ();
        acc := typedef codec_struct :: !acc;
        List.iter (fun (Field f) -> extract f.field_typ) codec_struct.fields
    | Codec _ -> ()
    | Map { inner; _ } -> extract inner
    | Where { inner; _ } -> extract inner
    | Optional { inner; _ } -> extract inner
    | Optional_or { inner; _ } -> extract inner
    | Apply { typ; _ } -> extract typ
    | Repeat { elem; _ } -> extract elem
    | Array { elem; _ } -> extract elem
    | Single_elem { elem; _ } -> extract elem
    | _ -> ()
  in
  List.iter (fun (Field f) -> extract f.field_typ) s.fields;
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

let module_ ?doc decls =
  (* Auto-prepend enum and casetype declarations needed by typedefs in
     [decls] but not already declared in the module. Output order matches
     the order [casetype_decls_of_struct] returns: dispatch before
     wrapper, since the wrapper applies the dispatch. *)
  let decl_name = function
    | Enum_decl { name; _ } -> Some name
    | Casetype_decl { name; _ } -> Some name
    | Typedef { struct_ = { name; _ }; _ } -> Some name
    | _ -> None
  in
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
    List.fold_left
      (fun (acc_rev, seen) d ->
        match d with
        | Typedef { struct_; _ } ->
            let candidates =
              enum_decls struct_ @ casetype_decls_of_struct struct_
            in
            List.fold_left
              (fun (acc_rev, seen) e ->
                match decl_name e with
                | None -> (acc_rev, seen)
                | Some n when List.mem n seen -> (acc_rev, seen)
                | Some n -> (e :: acc_rev, n :: seen))
              (acc_rev, seen) candidates
        | _ -> (acc_rev, seen))
      ([], already_declared) split_decls
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
  | Codec { codec_name; _ } -> Fmt.string ppf codec_name
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
  | Ref name -> ( try List.assoc name env with Not_found -> e)
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
  | Assign (p, x) -> Fmt.pf ppf "*%s = %a;" (escape_3d p.ph_name) pp_expr (e x)
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
  | On_success stmts ->
      Fmt.pf ppf "@[<h>{:on-success %a }@]" (pp_stmts []) stmts
  | On_act stmts -> Fmt.pf ppf "@[<h>{:act %a }@]" (pp_stmts []) stmts

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
  | Codec { codec_fixed_size = Some n; _ } -> Some n
  | _ -> None

(* Like [inner_wire_size] but as an [int expr], so explicitly-sized
   types ([byte_array], [byte_slice], [single_elem], etc.) can drive
   the [byte-size] suffix of an enclosing [optional]/[array]. Returns
   [None] only for shapes whose total wire size is genuinely not
   expressible at projection time (variable-size codec without an
   exposed [wire_size_expr], casetype, struct ref, all_bytes/all_zeros). *)
and inner_wire_size_expr : type a. a typ -> int expr option = function
  | Byte_array { size } | Byte_slice { size } | Byte_array_where { size; _ } ->
      Some size
  | Single_elem { size; _ } -> Some size
  | Uint_var { size; _ } -> Some size
  | Array { len; elem; _ } -> (
      match inner_wire_size_expr elem with
      | Some s -> Some (Mul (len, s))
      | None -> None)
  | Apply { typ; _ } -> inner_wire_size_expr typ
  | t -> ( match inner_wire_size t with Some n -> Some (Int n) | None -> None)

and optional_suffix : type a.
    bool expr -> a typ -> field_suffix * (Format.formatter -> unit) =
 fun present inner ->
  match inner_wire_size_expr inner with
  | Some s ->
      ( Byte_array (If_then_else (present, s, Int 0)),
        fun ppf -> pp_typ ppf inner )
  | None ->
      (* Unreachable: [optional]/[optional_or] reject variable-size inner
         at construction (smart constructor uses [has_wire_size_expr]). *)
      assert false

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
      (Single_elem { size; at_most }, fun ppf -> pp_typ ppf elem)
  | Array { len; elem; _ } -> (
      (* 3D's [name[len]] suffix only accepts byte-sized elements; for wider
         elements the size in bytes must be made explicit via [:byte-size].
         [inner_wire_size_expr] handles fixed scalars, [byte_array]-style
         sized payloads, codec-with-fixed-size, and nested arrays. *)
      match (inner_wire_size elem, inner_wire_size_expr elem) with
      | Some 1, _ -> (Array len, fun ppf -> pp_typ ppf elem)
      | _, Some s -> (Byte_array (Mul (len, s)), fun ppf -> pp_typ ppf elem)
      | _ ->
          (* Unreachable: [array] rejects variable-size elem at construction. *)
          assert false)
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

(* Collect every [Ref name] occurring in an expression. Used to detect
   field references in struct-level [where] clauses, which 3D's grammar
   does not allow (the [where] there sees only parameters). *)
let rec collect_refs : type a. a expr -> string list = function
  | Int _ | Int64 _ | Bool _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos
    ->
      []
  | Ref name -> [ name ]
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
  | Lt (a, b) | Le (a, b) | Gt (a, b) | Ge (a, b) ->
      collect_refs a @ collect_refs b
  | And (a, b) | Or (a, b) -> collect_refs a @ collect_refs b
  | Not a -> collect_refs a
  | Cast (_, a) -> collect_refs a
  | If_then_else (c, a, b) -> collect_refs c @ collect_refs a @ collect_refs b
  | Eq (a, b) -> collect_refs_packed a @ collect_refs_packed b
  | Ne (a, b) -> collect_refs_packed a @ collect_refs_packed b

and collect_refs_packed : type a. a expr -> string list =
 fun e -> collect_refs e

(* If a struct-level [where] references fields (rather than only params),
   attach it as the [constraint_] of the last referenced field instead --
   3D's [where] clause only sees parameters. *)
let lower_where_to_field_constraint where fields =
  match where with
  | None -> (None, fields)
  | Some w ->
      let refs = collect_refs w in
      let field_names =
        List.filter_map (fun (Field f) -> f.field_name) fields
      in
      let referenced_field_names =
        List.filter (fun r -> List.mem r field_names) refs
      in
      if referenced_field_names = [] then (Some w, fields)
      else
        (* Attach to the last referenced field by struct position so every
           name in the constraint is decoded by the time it runs. *)
        let last_pos =
          List.fold_left
            (fun acc (Field f) ->
              match f.field_name with
              | Some n when List.mem n referenced_field_names -> Some n
              | _ -> acc)
            None fields
        in
        let target =
          match last_pos with
          | Some n -> n
          | None -> List.hd (List.rev referenced_field_names)
        in
        let fields =
          List.map
            (fun (Field f as field) ->
              match f.field_name with
              | Some n when n = target ->
                  let constraint_ =
                    combine_constraints f.constraint_ (Some w)
                  in
                  Field { f with constraint_ }
              | _ -> field)
            fields
        in
        (None, fields)

let pp_struct ppf (s : struct_) =
  anon_counter := 0;
  let name = escape_3d s.name in
  let where, fields = lower_where_to_field_constraint s.where s.fields in
  Fmt.pf ppf "typedef struct _%s%a" name pp_params s.params;
  Option.iter (Fmt.pf ppf "@,where (%a)" pp_expr) where;
  Fmt.pf ppf "@,{@[<v 2>";
  List.iter (pp_field ppf) fields;
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
      (* 3D's [EXTERN PROBE q? IDENT] rule has no terminator. *)
      if init then Fmt.pf ppf "extern probe (INIT) %s@,@," name
      else Fmt.pf ppf "extern probe %s@,@," name
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
  | Int8 -> Some 1
  | Int16 _ -> Some 2
  | Int32 _ -> Some 4
  | Int64 _ -> Some 8
  | Float32 _ -> Some 4
  | Float64 _ -> Some 8
  | Uint_var { size = Int n; _ } -> Some n
  | Uint_var _ -> None
  | Bits { base; _ } -> (
      match base with
      | BF_U8 -> Some 1
      | BF_U16 _ -> Some 2
      | BF_U32 _ -> Some 4)
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
  | _ -> None

let c_type_of : type a. a typ -> string = function
  | Uint8 | Bits { base = BF_U8; _ } -> "uint8_t"
  | Uint16 _ | Bits { base = BF_U16 _; _ } -> "uint16_t"
  | Uint32 _ | Uint63 _ | Bits { base = BF_U32 _; _ } -> "uint32_t"
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
