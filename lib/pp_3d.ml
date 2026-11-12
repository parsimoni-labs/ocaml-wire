open Dsl
open Lower_3d

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

let struct_tag_name (s : struct_) = "Wire" ^ escape_3d s.name
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
  | Land64 (a, b) -> Fmt.pf ppf "(%a & %a)" pp_expr a pp_expr b
  | Lsr64 (a, b) -> Fmt.pf ppf "(%a >> %a)" pp_expr a pp_expr b
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
  | Land64 (a, b) -> Land64 (r a, r b)
  | Lsr64 (a, b) -> Lsr64 (r a, r b)
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
  | Uint64 _ | Int64 _ | Float64 _ -> Some 8
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
            (Apply
               {
                 typ = Type_ref opt_name;
                 args = Pack_expr arg :: optional_casetype_args inner;
               }) )

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
      (* A wrapper-needing element goes through its synthesised struct, applied
         to the formals that struct lifted out of the enclosing scope; a bare
         scalar or named struct renders inline. *)
      let pp_elem ppf =
        match single_elem_struct elem with
        | Some name -> pp_typ ppf (synth_ref name (wrap_formals elem))
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

(* How a declaration position renders a type: the base to print, its suffix, and
   the refinement that has to follow the name. Shared by [pp_field] and
   [pp_casetype_cases] so a case body carries the same membership, index bound
   and [where] a field of that type would. Rendering a case body through
   [field_suffix] alone dropped every one of them, and the generated validator
   then accepted bodies the codec rejects. *)
let render_typ_at : type a.
    name:string ->
    widenable:string list ->
    own:bool expr option ->
    a typ ->
    (Format.formatter -> unit) * field_suffix * bool expr option =
 fun ~name ~widenable ~own field_typ ->
  (* Extract Where constraints from the type so they appear as field
     constraints in the 3D output, not inline in the type. *)
  let where_cond, typ = extract_where_constraint field_typ in
  let bound_cond = lookup_bound_cond name field_typ in
  (* The documentation projection types the field as its named enum, so
     EverParse enforces membership through the type. The codegen projection
     instead emits membership as a refinement on the base type, which the OCaml
     decoder mirrors on decode. *)
  let doc_enum =
    if !render_enum_as_type then enum_type_name field_typ else None
  in
  let enum_cond =
    match (doc_enum, enum_cases_of field_typ) with
    | None, Some (v0 :: rest) ->
        Some
          (List.fold_left
             (fun acc v -> Or (acc, Eq (Ref (I, name), Int v)))
             (Eq (Ref (I, name), Int v0))
             rest)
    | _ -> None
  in
  let constraint_ =
    combine_constraints
      (combine_constraints (combine_constraints own where_cond) bound_cond)
      enum_cond
  in
  (* Rewrite a signed field's ordering refinement to its two's-complement
     unsigned form (the field projects to an unsigned type), and reject a float
     ordering refinement, which has no faithful unsigned projection. *)
  let constraint_ =
    Option.map (project_refinement ~name ~kind:(scalar_kind typ)) constraint_
  in
  let constraint_ =
    Option.map (project_field_arith name widenable) constraint_
  in
  let suffix, pp_base = field_suffix typ in
  let pp_base =
    match doc_enum with
    | Some n -> fun ppf -> Fmt.string ppf n
    | None -> pp_base
  in
  (pp_base, suffix, constraint_)

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
  let pp_base, suffix, constraint_ =
    render_typ_at ~name:raw_name ~widenable ~own:f.constraint_ f.field_typ
  in
  Option.iter (pp_field_doc ppf) f.field_doc;
  Fmt.pf ppf "@,%t %s%a" pp_base name pp_field_suffix suffix;
  Option.iter (Fmt.pf ppf " { %a }" pp_expr) constraint_;
  Option.iter (Fmt.pf ppf " %a" pp_action) f.action;
  Fmt.string ppf ";"

(* The scalar an enum-shaped type projects to, seen through the transparent
   wrappers. Used where a type has to render without an enum declaration to
   refer to. *)
let rec pp_scalar_of : type a. Format.formatter -> a typ -> unit =
 fun ppf t ->
  match t with
  | Enum { base; _ } -> pp_scalar_of ppf base
  | Map { inner; _ } -> pp_scalar_of ppf inner
  | Where { inner; _ } -> pp_scalar_of ppf inner
  | Apply { typ; _ } -> pp_scalar_of ppf typ
  | t -> pp_typ ppf t

(* A parameter's type renders under the same rule as a field's: the named enum
   only where the module declares enum types, and the base it projects to
   otherwise. Naming an enum unconditionally left the codegen projection
   referring to a type it never declares, and a [where]-wrapped base rendering
   as a refinement, which is not a parameter type at all. *)
let pp_param_typ : type a. Format.formatter -> a typ -> unit =
 fun ppf t ->
  match if !render_enum_as_type then enum_type_name t else None with
  | Some n -> Fmt.string ppf (escape_3d n)
  | None -> pp_scalar_of ppf t

(* [~as_enum] is what separates the two formal positions. A casetype's switch
   discriminant may be the enum type, and reads better as it. A struct's formal
   is used in the size and offset arithmetic of the fields it parameterises, and
   EverParse requires an integer type there: naming the enum got "Unknown
   integer type" even with the enum declared. *)
let pp_param ~as_enum ppf p =
  let (Pack_typ t) = p.param_typ in
  let name = escape_3d p.param_name in
  let pp ppf t = if as_enum then pp_param_typ ppf t else pp_scalar_of ppf t in
  if p.mutable_ then Fmt.pf ppf "mutable %a *%s" pp t name
  else Fmt.pf ppf "%a %s" pp t name

let pp_params ?(as_enum = false) ppf params =
  if not (List.is_empty params) then
    Fmt.pf ppf "(%a)" Fmt.(list ~sep:comma (pp_param ~as_enum)) params

let pp_struct ppf (s : struct_) =
  anon_counter := 0;
  let name = escape_3d s.name in
  let tag = struct_tag_name s in
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
  Fmt.pf ppf "typedef struct %s%a" tag (pp_params ?as_enum:None) s.params;
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
  (* Only where the module declares enum types: the codegen projection renders
     the tag as its base scalar, so a constant name there would refer to a type
     the module never declares. Seen through the transparent wrappers, since the
     tag's enum can sit behind a [lookup]'s map or a [where]. *)
  let rec named_cases : type k. k typ -> (string * int) list option = function
    | Enum { cases; _ } -> Some cases
    | Map { inner; _ } -> named_cases inner
    | Where { inner; _ } -> named_cases inner
    | Apply { typ; _ } -> named_cases typ
    | _ -> None
  in
  let enum_label k =
    (* [enum_type_name] is the same test that decides whether the tag renders as
       the enum: an enum over a bitfield base has no 3D enum type and so no
       declaration, and naming its constant referred to nothing. *)
    if (not !render_enum_as_type) || Option.is_none (enum_type_name tag) then
      None
    else
      match named_cases tag with
      | Some ecases ->
          List.find_map
            (fun (n, v) -> if Int.equal v k then Some n else None)
            ecases
      | None -> None
  in
  List.iteri
    (fun i (tag_opt, Pack_typ typ) ->
      let field_name = Fmt.str "v%d" i in
      let pp_base, suffix, constraint_ =
        render_typ_at ~name:field_name ~widenable:[] ~own:None typ
      in
      let pp_body ppf () =
        Fmt.pf ppf "%t %s%a" pp_base field_name pp_field_suffix suffix;
        Option.iter (Fmt.pf ppf " { %a }" pp_expr) constraint_
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
        (* The tag occupies C's distinct tag namespace; a readable [Wire]
           prefix keeps it separate from the public typedef name. *)
        let n = escape_3d st.name in
        Fmt.pf ppf "extern typedef struct %s %s@,@," (struct_tag_name st) n
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
        Fmt.(list ~sep:comma (pp_param ~as_enum:false))
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
      Fmt.pf ppf "casetype %s%a {@[<v 2>@,switch (%s) {" internal_name
        (pp_params ~as_enum:true) params disc_name;
      pp_casetype_cases ppf tag cases;
      Fmt.pf ppf "@,}@]@,} %s;@,@," public_name

let pp_module ppf m =
  Option.iter
    (fun d ->
      pp_wrapped_comment ppf ~open_:"/*++" ~close:"--*/" d;
      Fmt.pf ppf "@,@,")
    m.doc;
  List.iter (pp_decl ppf) m.decls

(* Two declarations under one name are two different types with one name, and
   3D resolves every reference to whichever came first. [Everparse.write]
   already refuses this across schemas; within one schema nothing did, so a
   second sub-codec of a different width silently gave a validator reading a
   different number of bytes than the codec. Compared by what each renders to,
   because a [decl] holds closures and structural equality raises on them.
   Identical redeclarations are left alone: 3D says so itself, loudly. *)
let check_unique_decl_names decls =
  let seen = Hashtbl.create 16 in
  List.iter
    (fun d ->
      match decl_name d with
      | None -> ()
      | Some n -> (
          let text = Fmt.str "%a" pp_decl d in
          match Hashtbl.find_opt seen n with
          | None -> Hashtbl.add seen n text
          | Some first ->
              if not (String.equal first text) then
                Fmt.invalid_arg
                  "Wire: the schema declares %S twice with different \
                   definitions; every reference to it would resolve to the \
                   first, so rename one of them"
                  n))
    decls

let to_3d ?(enum_as_type = false) m =
  render_enum_as_type := enum_as_type;
  Fun.protect
    ~finally:(fun () -> render_enum_as_type := false)
    (fun () ->
      check_unique_decl_names m.decls;
      Fmt.str "@[<v>%a@]" pp_module m)

let to_3d_file ?(enum_as_type = false) path m =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (to_3d ~enum_as_type m ^ "\n"))
