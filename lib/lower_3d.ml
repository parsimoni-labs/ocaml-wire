open Dsl

(* Extract enum declarations needed by struct fields. Scans field types for
   Enum constructors (including under Map/Where wrappers) and returns the
   corresponding enum_decl entries, deduplicated by name. Enums over bitfield
   bases are skipped: they map to plain bitfields in 3D (the enum/variant
   mapping is OCaml-only). An enum over a big-endian base is skipped too (no 3D
   enum type for it; the field carries a membership refinement instead). *)
(* Both open and closed enums declare a 3D enum type so the named codes survive
   in the generated .3d. A closed enum additionally constrains its field with a
   membership refinement; an open enum leaves the field as its base scalar (any
   value accepted) while still documenting the known codes through the
   declaration.

   A second enum under the same name is a repeat of the first (see
   [claim_name]). *)

(* One name, one declaration. A declaration collected under [name] is emitted
   the first time ([true]) and skipped when the same [shape] claims it again
   ([false]): two references to one type project once. A different [shape]
   under the same name is refused, since every 3D reference to the name would
   resolve to whichever was emitted first. [shape] is a pure structural key,
   as a [decl] holds closures and cannot be compared itself. *)
let claim_name seen ~kind name shape =
  match Hashtbl.find_opt seen name with
  | None ->
      Hashtbl.add seen name shape;
      true
  | Some first when String.equal first shape -> false
  | Some _ ->
      Fmt.invalid_arg
        "Wire: two different %s are named %S; every reference to it resolves \
         to one of them, so rename one"
        kind name

(* The integer an enum is carried by, as part of its shape: two enums with
   the same cases over different bases declare different 3D types. *)
let rec int_base_tag : type a. a typ -> string = function
  | Uint8 -> "u8"
  | Int8 -> "i8"
  | Uint16 Little -> "u16"
  | Uint16 Big -> "u16be"
  | Int16 Little -> "i16"
  | Int16 Big -> "i16be"
  | Uint32 Little -> "u32"
  | Uint32 Big -> "u32be"
  | Int32 Little -> "i32"
  | Int32 Big -> "i32be"
  | Uint64 Little -> "u64"
  | Uint64 Big -> "u64be"
  | Int64 Little -> "i64"
  | Int64 Big -> "i64be"
  | Uint_var { endian = Little; _ } -> "uv"
  | Uint_var { endian = Big; _ } -> "uvbe"
  | Bits { width; base = U8; _ } -> "bits8_" ^ string_of_int width
  | Bits { width; base = U16 _; _ } -> "bits16_" ^ string_of_int width
  | Bits { width; base = U32 _; _ } -> "bits32_" ^ string_of_int width
  | Map { inner; _ } -> int_base_tag inner
  | Where { inner; _ } -> int_base_tag inner
  | Apply { typ; _ } -> int_base_tag typ
  | Enum { base; _ } -> int_base_tag base
  | _ -> invalid_arg "Wire: an enum over a non-integer base"

let enum_shape base cases =
  int_base_tag base ^ "|"
  ^ String.concat "," (List.map (fun (n, v) -> n ^ "=" ^ string_of_int v) cases)

let record_enum seen decls name cases base =
  if claim_name seen ~kind:"enums" name (enum_shape base cases) then
    decls := Enum_decl { name; cases; base = Pack_typ base } :: !decls

let enum_decls (s : struct_) : decl list =
  let seen : (string, string) Hashtbl.t = Hashtbl.create 4 in
  let decls = Stdlib.ref [] in
  let is_bits : type a. a typ -> bool = function
    | Bits _ -> true
    | _ -> false
  in
  List.iter
    (fun (Field f) ->
      let rec extract : type a. a typ -> unit = function
        | Enum { name; cases; base; _ }
          when (not (is_bits base)) && not (enum_base_is_be base) ->
            record_enum seen decls name cases base
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
      let (Pack_typ b) = base in
      if
        claim_name seen ~kind:"enum element structs" sname
          (int_base_tag b ^ "|"
          ^ String.concat "," (List.map string_of_int cases))
      then
        Option.iter
          (fun d -> acc := d :: !acc)
          (enum_elem_struct_decl name base cases)
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
(* Whether a casetype dispatches on an integer tag. This has to admit exactly
   what [case_index_to_expr] can project, which is every carrier with an integer
   view: disagreeing sends the casetype down the string-tag rewrite, where the
   whole union collapses to a tag plus [all_bytes] and the generated validator
   checks no case body at all. A [variants] or [lookup] tag is a [Map] over an
   integer and dispatches like one. *)
let is_int_dispatch_typ : type a. a typ -> bool =
 fun typ -> is_int_representable typ

let int32_case_index k =
  match SInt32.to_int_opt k with
  | Some index -> index
  | None ->
      Fmt.invalid_arg
        "Wire.casetype: case index %ld does not fit this platform's native int"
        (SInt32.to_int32 k)

let uint32_case_index k =
  match UInt32.to_int k with
  | index -> index
  | exception (Failure _ | Invalid_argument _) ->
      let index =
        UInt32.to_int32 k |> Int64.of_int32 |> Int64.logand 0xFFFF_FFFFL
      in
      Fmt.invalid_arg
        "Wire.casetype: case index %Ld does not fit this platform's native int"
        index

let uint_var_case_index k =
  match UInt63.to_int k with
  | index -> index
  | exception (Failure _ | Invalid_argument _) ->
      Fmt.invalid_arg
        "Wire.casetype: case index %a does not fit this platform's native int"
        UInt63.pp k

(* Both widths project to 3D's UINT64, so the case label is the unsigned
   reinterpretation of the eight wire bytes -- which is what [int_of] reads out
   of them, and what the generated [switch] compares against. *)
let uint64_case_index k =
  match UInt64.to_int_opt k with
  | Some index -> index
  | None ->
      Fmt.invalid_arg
        "Wire.casetype: case index %Lu does not fit this platform's native int"
        (UInt64.to_int64 k)

let int64_case_index k =
  match Int64.unsigned_to_int k with
  | Some index -> index
  | None ->
      Fmt.invalid_arg
        "Wire.casetype: case index %Lu does not fit this platform's native int"
        k

(* Project a case-branch discriminator value of type ['k] to a 3D constant
   expression. Only called for int-shaped tags; non-int tags don't reach here
   because their casetype field is rewritten away before
   [casetype_decls_of_struct] walks it.

   [is_int_dispatch_typ] lets an [Enum] through whatever its base, and [enum]
   takes any base [is_int_representable] accepts, so every integer carrier
   reachable that way has to be covered here -- the transparent [map] / [where]
   / [nested] / [apply] wrappers a [variants] or [lookup] base carries
   included. The match is left exhaustive so a new typ constructor is a
   compile error rather than a crash on a schema that uses it. *)
let rec case_index_to_expr : type k. k typ -> k -> packed_expr =
 fun tag_typ k ->
  match tag_typ with
  | Uint8 -> Pack_expr (Int (UInt8.to_int k))
  | Uint16 _ -> Pack_expr (Int (UInt16.to_int k))
  | Uint32 _ -> Pack_expr (Int (uint32_case_index k))
  | Uint64 _ -> Pack_expr (Int (uint64_case_index k))
  | Uint_var _ -> Pack_expr (Int (uint_var_case_index k))
  | Int8 -> Pack_expr (Int (SInt8.to_int k))
  | Int16 _ -> Pack_expr (Int (SInt16.to_int k))
  | Int32 _ -> Pack_expr (Int (int32_case_index k))
  | Int64 _ -> Pack_expr (Int (int64_case_index k))
  | Bits _ -> Pack_expr (Int k)
  | Enum { base; _ } -> case_index_to_expr base k
  | Map { inner; encode; _ } -> case_index_to_expr inner (encode k)
  | Where { inner; _ } -> case_index_to_expr inner k
  | Single_elem { elem; _ } -> case_index_to_expr elem k
  | Apply { typ; _ } -> case_index_to_expr typ k
  | Float32 _ | Float64 _ | Unit | All_bytes | All_zeros | Zeroterm
  | Zeroterm_at_most _ | Array _ | Byte_array _ | Byte_array_where _
  | Byte_slice _ | Casetype _ | Union _ | Struct _ | Type_ref _
  | Qualified_ref _ | Codec _ | Optional _ | Optional_or _ | Repeat _ | Der _
  | Cbor _ ->
      invalid_arg
        "Wire.casetype: this tag type carries no integer case index; use a \
         fixed-width integer, bitfield, or enum tag."

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

(* The dispatch table of a casetype or a union: a 3D casetype parameterised on
   the tag value. *)
let casetype_dispatch : type a k.
    string -> k typ -> (a, k) case_branch list -> decl =
 fun name tag cases ->
  Casetype_decl
    {
      name;
      params = [ param "tag" tag ];
      tag = Pack_typ tag;
      cases = List.map (decl_case_of_branch tag) cases;
    }

let casetype_pair : type a k.
    string -> k typ -> (a, k) case_branch list -> decl * decl =
 fun name tag cases ->
  let body_name = name ^ "_Body" in
  let dispatch = casetype_dispatch body_name tag cases in
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

let endian_tag = function Little -> "l" | Big -> "b"

let bitfield_base_tag = function
  | U8 -> "u8"
  | U16 e -> "u16" ^ endian_tag e
  | U32 e -> "u32" ^ endian_tag e

let bit_order_tag = function Msb_first -> "m" | Lsb_first -> "l"

let cast_tag = function
  | `U8 -> "8"
  | `U16 -> "16"
  | `U32 -> "32"
  | `U64 -> "64"

(* A decimal literal as an identifier fragment: a negative one leads with [m],
   since [-] is no part of an identifier. *)
let literal_tag s =
  if String.length s > 0 && s.[0] = '-' then
    "m" ^ String.sub s 1 (String.length s - 1)
  else s

(* An [elt_var] is bound by the refined-byte struct its span renders through, so
   it is not free here; and the counter that makes it unique would make two
   identically shaped spans look different, so it does not name itself either. *)
let is_elt_var name = String.starts_with ~prefix:elt_var_prefix name

(* A [nested ~size] projects to [<elem>[:byte-size-single-element-array n]],
   whose element must be a single type token. A bare scalar or an already-named
   struct ([codec] / [casetype]) works inline, but an element that renders with
   its own array / refinement qualifier ([byte_array], [byte_slice],
   [byte_array_where], [zeroterm_at_most], [uint_var]) or that needs its own
   declaration ([enum]) cannot. Those go through a synthesised wrapper struct,
   named deterministically from the element so the field renderer and the
   typedef emitter agree without shared state. *)

(* A deterministic, collision-free identifier fragment for an inner type, used
   to name its synthesised wrapper struct, paired with the formals that wrapper
   has to declare. Two structurally equal inners get the same fragment (so their
   wrapper is shared); different shapes differ, which is why the match is total:
   a catch-all names unrelated shapes alike, and the second is then dropped as a
   repeat of the first and its field validated as the first's element.

   A wrapper struct is a 3D scope of its own, so a size expression naming a
   field of the enclosing struct has nothing to resolve against inside it. Those
   names come back as [UINT32] formals, the type 3D gives every byte-size
   expression (it widens a narrower argument to it, and refuses a wider one
   rather than truncating); a bound parameter comes back as its own declared
   type. The use site passes each one on by name. A sub-codec or a casetype
   keeps a scope of its own; a sub-codec is applied to its formals by name
   ([Sub(p1, ...)]), so those come back too, as declared.

   Pure, so it can be compared where the declaration itself cannot. *)
let rec wrap_shape : type a. a typ -> string * param list =
 fun typ ->
  let plain tag = (tag, []) in
  let sized prefix size =
    let tag, formals = expr_shape size in
    (prefix ^ tag, formals)
  in
  let over prefix size elem =
    let stag, sformals = expr_shape size in
    let etag, eformals = wrap_shape elem in
    (Fmt.str "%s%s_%s" prefix stag etag, sformals @ eformals)
  in
  match typ with
  | Uint8 -> plain "u8"
  | Uint16 e -> plain ("u16" ^ endian_tag e)
  | Uint32 e -> plain ("u32" ^ endian_tag e)
  | Uint64 e -> plain ("u64" ^ endian_tag e)
  | Int8 -> plain "i8"
  | Int16 e -> plain ("i16" ^ endian_tag e)
  | Int32 e -> plain ("i32" ^ endian_tag e)
  | Int64 e -> plain ("i64" ^ endian_tag e)
  | Float32 e -> plain ("f32" ^ endian_tag e)
  | Float64 e -> plain ("f64" ^ endian_tag e)
  | Uint_var { size; endian } -> sized ("uv" ^ endian_tag endian) size
  | Bits { width; base; bit_order } ->
      Fmt.kstr plain "bf%d%s%s" width (bitfield_base_tag base)
        (bit_order_tag bit_order)
  | Unit -> plain "unit"
  | All_bytes -> plain "all"
  | All_zeros -> plain "zeros"
  | Zeroterm -> plain "zt"
  | Zeroterm_at_most { size } -> sized "ztam" size
  | Byte_array { size } -> sized "ba" size
  | Byte_slice { size } -> sized "bs" size
  | Byte_array_where { size; cond; _ } ->
      (* The per-byte constraint tells two spans of one size apart, but it
         renders in their own [_RefByte_*] struct and resolves there, so it
         lifts nothing here. *)
      let stag, sformals = expr_shape size in
      let ctag, _ = expr_shape cond in
      (Fmt.str "rb%s_%s" stag ctag, sformals)
  | Where { cond; inner } ->
      let ctag, cformals = expr_shape cond in
      let itag, iformals = wrap_shape inner in
      (Fmt.str "w%s_%s" ctag itag, cformals @ iformals)
  | Array { len; elem; _ } -> over "arr" len elem
  | Single_elem { size; elem; at_most } ->
      over (if at_most then "seam" else "se") size elem
  | Repeat { size; elem; _ } -> over "rep" size elem
  | Optional { present; inner } ->
      let ptag, pformals = expr_shape present in
      let itag, iformals = wrap_shape inner in
      (Fmt.str "opt%s_%s" ptag itag, pformals @ iformals)
  | Optional_or { present; inner; _ } ->
      let ptag, pformals = expr_shape present in
      let itag, iformals = wrap_shape inner in
      (Fmt.str "optd%s_%s" ptag itag, pformals @ iformals)
  | Enum { name; _ } -> plain ("e" ^ name)
  | Casetype { name; _ } -> plain name
  | Union { name; on; _ } ->
      let otag, oformals = expr_shape on in
      (Fmt.str "%s_%s" name otag, oformals)
  | Struct { name; _ } -> plain name
  | Type_ref name -> plain name
  | Qualified_ref { module_; name } -> plain (module_ ^ "_" ^ name)
  | Map { inner; _ } -> wrap_shape inner
  | Apply { typ; args } ->
      let ttag, _ = wrap_shape typ in
      let atags, aformals =
        List.split (List.map (fun (Pack_expr e) -> expr_shape e) args)
      in
      (Fmt.str "ap%s_%s" ttag (String.concat "_" atags), List.concat aformals)
  | Codec { codec_name; codec_struct; _ } -> (codec_name, codec_struct.params)
  | Der _ -> invalid_arg "Wire: an ASN.1 component has no 3D projection"
  | Cbor _ -> invalid_arg "Wire: a CBOR member has no 3D projection"

and expr_shape : type a. a expr -> string * param list =
 fun e ->
  let un op x =
    let tag, formals = expr_shape x in
    (Fmt.str "%s_%s" op tag, formals)
  in
  let bin : type b c. string -> b expr -> c expr -> string * param list =
   fun op x y ->
    let xtag, xformals = expr_shape x in
    let ytag, yformals = expr_shape y in
    (Fmt.str "%s_%s_%s" op xtag ytag, xformals @ yformals)
  in
  match e with
  | Int n -> (literal_tag (string_of_int n), [])
  | Int64 n -> ("l" ^ literal_tag (Int64.to_string n), [])
  | Bool b -> ((if b then "true" else "false"), [])
  | Ref (_, name) when is_elt_var name -> ("elt", [])
  | Ref (_, name) -> (name, [ param name uint32 ])
  | Param_ref p ->
      ( "p" ^ p.name,
        [
          {
            param_name = p.name;
            param_typ = p.packed_typ;
            mutable_ = p.mutable_;
          };
        ] )
  | Sizeof t -> ("sz" ^ fst (wrap_shape t), [])
  | Sizeof_this -> ("szthis", [])
  | Field_pos -> ("fpos", [])
  | Add (a, b) -> bin "add" a b
  | Sub (a, b) -> bin "sub" a b
  | Mul (a, b) -> bin "mul" a b
  | Div (a, b) -> bin "div" a b
  | Mod (a, b) -> bin "mod" a b
  | Land (a, b) -> bin "land" a b
  | Lor (a, b) -> bin "lor" a b
  | Lxor (a, b) -> bin "lxor" a b
  | Lnot a -> un "lnot" a
  | Lsl (a, b) -> bin "lsl" a b
  | Lsr (a, b) -> bin "lsr" a b
  | Land64 (a, b) -> bin "land64" a b
  | Lsr64 (a, b) -> bin "lsr64" a b
  | Eq (a, b) -> bin "eq" a b
  | Ne (a, b) -> bin "ne" a b
  | Lt (a, b) -> bin "lt" a b
  | Le (a, b) -> bin "le" a b
  | Gt (a, b) -> bin "gt" a b
  | Ge (a, b) -> bin "ge" a b
  | And (a, b) -> bin "and" a b
  | Or (a, b) -> bin "or" a b
  | Not a -> un "not" a
  | Cast (w, a) -> un ("cast" ^ cast_tag w) a
  | If_then_else (c, t, f) ->
      let ctag, cformals = expr_shape c in
      let ttag, tformals = expr_shape t in
      let ftag, fformals = expr_shape f in
      (Fmt.str "ite_%s_%s_%s" ctag ttag ftag, cformals @ tformals @ fformals)

let wrap_tag t = fst (wrap_shape t)

(* The formals a synthesised wrapper over [t] declares: first occurrence of each
   name wins, so its declaration and its use sites agree on their order. *)
let wrap_formals t =
  let seen = Hashtbl.create 4 in
  List.filter
    (fun (p : param) ->
      if Hashtbl.mem seen p.param_name then false
      else (
        Hashtbl.add seen p.param_name ();
        true))
    (snd (wrap_shape t))

let formal_args formals =
  List.map (fun (p : param) -> Pack_expr (Ref (I, p.param_name))) formals

(* A reference to a synthesised declaration, applied to the formals it lifted so
   the enclosing scope's values flow into the new one. *)
let synth_ref name = function
  | [] -> Type_ref name
  | formals -> Apply { typ = Type_ref name; args = formal_args formals }

(* The synthesised wrapper-struct name for a [nested] inner that has no valid
   bare single-element-array token. A scalar, sub-codec, or casetype renders
   inline ([None]); everything else (byte spans, arrays, optionals, nested
   regions, ...) is lifted into a named [struct { v : inner }]. *)
let some fmt = Fmt.kstr (fun s -> Some s) fmt

let rec single_elem_struct : type a. a typ -> string option = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint64 _ | Int8 | Int16 _ | Int32 _ | Int64 _
  | Float32 _ | Float64 _ | Codec _ | Casetype _ ->
      None
  | Map { inner; _ } -> single_elem_struct inner
  | Where { inner; _ } -> single_elem_struct inner
  (* Keep the historical names of the byte-span family where the size alone
     pins the shape, so existing schemas and tests are unchanged. A refined span
     and a variable-width integer also differ in their constraint and their
     endianness, so those take a structural name, as everything else does. *)
  | Byte_array { size = Int n } -> some "SeBytes%d" n
  | Byte_slice { size = Int n } -> some "SeSlice%d" n
  | Zeroterm_at_most { size = Int n } -> some "SeZtam%d" n
  | Enum { name; _ } -> Some ("Se_" ^ name)
  | other -> Some ("Se_" ^ wrap_tag other)

(* A single-element region cannot become a byte-budget list when wrapped
   in an optional: the list would parse the element repeatedly and lose the
   exact/at-most contract. Keep it behind a gate and a named wrapper. *)
let rec optional_region : type a. a typ -> bool = function
  | Single_elem _ -> true
  | Map { inner; _ } -> optional_region inner
  | Where { inner; _ } -> optional_region inner
  | _ -> false

let optional_region_name inner = "OptRegion_" ^ wrap_tag inner

let region_bound : type a. a typ -> bool expr option * a typ = function
  | Where { cond; inner = Single_elem _ as region } -> (Some cond, region)
  | Map ({ inner = Where { cond; inner = Single_elem _ as region }; _ } as m) ->
      (Some cond, Map { m with inner = region })
  | t -> (None, t)

(* Primitive scalars have a fixed extent. Lower their sized regions to a
   guarded scalar plus padding instead of retaining a generic exact-parser
   combinator, whose erased parser-kind metadata can leak into C extraction. *)
let primitive_width : type a. a typ -> int option = function
  | Uint8 | Int8 -> Some 1
  | Uint16 _ | Int16 _ -> Some 2
  | Uint32 _ | Int32 _ | Float32 _ -> Some 4
  | Uint64 _ | Int64 _ | Float64 _ -> Some 8
  | _ -> None

let rec field_wire_size : type a. a typ -> int option = function
  | Uint_var { size = Int n; _ } -> Some n
  | Uint_var _ -> None
  | Bits { base; _ } -> (
      match base with U8 -> Some 1 | U16 _ -> Some 2 | U32 _ -> Some 4)
  | Unit -> Some 0
  | Byte_array { size = Int n }
  | Byte_array_where { size = Int n; _ }
  | Byte_slice { size = Int n }
  (* A bounded zero-terminated string spans its whole region whatever the
     terminator's position, as [elem_size_of] already reports it. *)
  | Zeroterm_at_most { size = Int n } ->
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
  | t -> primitive_width t

let rec inner_wire_size : type a. a typ -> int option = function
  | Bits { base = U8; _ } -> Some 1
  | Bits { base = U16 _; _ } -> Some 2
  | Bits { base = U32 _; _ } -> Some 4
  | Unit -> Some 0
  | Map { inner; _ } -> inner_wire_size inner
  | Enum { base; _ } -> inner_wire_size base
  | Where { inner; _ } -> inner_wire_size inner
  | Codec { codec_fixed_size = Some n; _ } -> Some n
  | t -> primitive_width t

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

let backend (s : struct_) =
  List.find_map
    (fun (Field f) ->
      match f.field_typ with
      | Der _ -> Some `Der
      | Cbor _ -> Some `Cbor
      | _ -> None)
    s.fields

let rec backend_value : type a. a typ -> [ `Der | `Cbor ] option = function
  | Codec { codec_struct; _ } -> backend codec_struct
  | Map { inner; _ } -> backend_value inner
  | _ -> None

let byte_region t = Option.is_some (backend_value t)

let primitive_region : type a.
    a typ -> (int expr * packed_typ * int * bool) option = function
  | Single_elem { size; elem; at_most } ->
      Option.map
        (fun n -> (size, Pack_typ elem, n, at_most))
        (primitive_width elem)
  | _ -> None

let primitive_region_name : type a. a typ -> string = function
  | Single_elem { elem; at_most; _ } ->
      (if at_most then "ScalarRegionAtMost_" else "ScalarRegionExact_")
      ^ wrap_tag elem
  | _ -> invalid_arg "Types.primitive_region_name"

let primitive_region_decl typ (_, Pack_typ elem, width, at_most) =
  let size = Ref (I, "_region_size") in
  let where = if at_most then Ge (size, Int width) else Eq (size, Int width) in
  let fields = [ field "_value" elem ] in
  let fields =
    if at_most then
      fields
      @ [ field "_padding" (Byte_array { size = Sub (size, Int width) }) ]
    else fields
  in
  typedef
    (param_struct
       (primitive_region_name typ)
       [ param "_region_size" (Uint32 Little) ]
       ~where fields)

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
  if optional_region inner then "Opt_" ^ wrap_tag inner else "Opt_" ^ base inner

(* The formals the gate casetype has to declare on top of the gate itself. The
   casetype is a 3D scope of its own, and its default case applies the inner: a
   parametric sub-codec renders there as [Sub(p1, ...)], naming formals that only
   the enclosing struct surfaces. Redeclaring them here, and passing them on from
   the use site, puts them back in scope. Walks the transparent wrappers the way
   [optional_casetype_name] does, so a name and its formals describe the same
   inner; an [apply] carries its own arguments and lifts nothing. *)
let rec optional_casetype_formals : type a. a typ -> param list = function
  | inner when optional_region inner -> wrap_formals inner
  | Codec { codec_struct; _ } -> codec_struct.params
  | Map { inner; _ } -> optional_casetype_formals inner
  | Where { inner; _ } -> optional_casetype_formals inner
  | _ -> []

(* What a use site passes to the gate casetype after the gate: one argument per
   lifted formal, resolved against the enclosing struct that surfaces it. *)
let optional_casetype_args : type a. a typ -> packed_expr list =
 fun inner -> formal_args (optional_casetype_formals inner)

(* Project a self-delimiting [optional] inner as a casetype dispatched on the
   gate: [case 0] parses a 0-byte field (the gate arg is [present ? 1 : 0], so 0
   means absent), the default case parses the inner. An empty case body is a 3D
   syntax error, hence the 0-byte field. *)
let optional_casetype_decl : type a. string -> a typ -> decl =
 fun name inner ->
  Casetype_decl
    {
      name;
      params = param "present" Uint8 :: optional_casetype_formals inner;
      tag = Pack_typ Uint8;
      cases =
        [
          (Some (Pack_expr (Int 0)), Pack_typ (Byte_array { size = Int 0 }));
          ( None,
            if optional_region inner then
              Pack_typ
                (synth_ref (optional_region_name inner) (wrap_formals inner))
            else Pack_typ inner );
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

(* The synthesised 1-byte struct a refined span renders as, emitted once per
   name. [refined_byte_typedefs] only walks the entrypoint's own fields, so a
   span reached through a sub-codec, an array element or a case body has no
   other source of a declaration. *)
let emit_refined_byte seen acc elt_var cond =
  let synth = synth_name_of_elt_var elt_var in
  if claim_name seen ~kind:"refined bytes" synth (fst (expr_shape cond)) then
    acc :=
      typedef (struct_ synth [ field elt_var ~constraint_:cond uint8 ]) :: !acc

(* A 1-byte array or repeat element carrying a lookup's index bound renders
   as a refined byte, [_RefByte_lk<n>]. [refined_byte_typedefs] declares it
   for the entrypoint's own fields only, so a sub-codec's is declared here. *)
let emit_index_bound seen acc elem =
  Option.iter
    (fun (elt_var, cond) -> emit_refined_byte seen acc elt_var cond)
    (index_bound_elt elem)

(* A structural key for a sub-codec's struct: its name, its fields' names, and
   each field's shape as [wrap_tag] renders it. Pure, so it can be compared
   where the declaration itself cannot. *)
let codec_shape name (st : struct_) =
  name ^ "|"
  ^ String.concat ","
      (List.map
         (fun (Field f) ->
           Option.value ~default:"_" f.field_name ^ ":" ^ wrap_tag f.field_typ)
         st.fields)

(* A fixed-size element used inside [repeat] / [nested] needs a named wrapper
   struct when it has no usable bare token; emit it once, deduped by name. A
   refined-byte element inside the wrapper needs its own typedef emitted first.
   A name already standing for another shape is refused: sharing it would drop
   one of the two elements and validate its field as the other. *)
let emit_wrapper seen acc name elem =
  if claim_name seen ~kind:"element wrappers" name (wrap_tag elem) then begin
    (match wrapper_refined_byte elem with
    | Some (elt_var, cond) -> emit_refined_byte seen acc elt_var cond
    | None -> ());
    acc :=
      (let bound, region = region_bound elem in
       let sub =
         match region with
         | Single_elem { size = Sub (a, b); _ } -> Some (Ge (a, b))
         | _ -> None
       in
       let where =
         match (bound, sub) with
         | Some b, Some s -> Some (And (b, s))
         | Some c, None | None, Some c -> Some c
         | None, None -> None
       in
       typedef
         (param_struct name (wrap_formals elem) ?where [ field "v" region ]))
      :: !acc
  end

(* A structural key for an int-tagged casetype: its tag and, per case, the
   case tag and the body's shape. *)
let casetype_shape tag cases =
  wrap_tag tag ^ "|"
  ^ String.concat ","
      (List.map
         (fun branch ->
           let tag_opt, Pack_typ body = decl_case_of_branch tag branch in
           (match tag_opt with
             | None -> "default"
             | Some (Pack_expr e) -> fst (expr_shape e))
           ^ ":" ^ wrap_tag body)
         cases)

(* A sized field holding one element: a scalar region, or the wrapper struct
   of an element that needs one. *)
let emit_single_elem seen acc typ elem =
  match primitive_region typ with
  | Some info ->
      let name = primitive_region_name typ in
      if claim_name seen ~kind:"scalar regions" name name then
        acc := primitive_region_decl typ info :: !acc
  | None ->
      Option.iter
        (fun n -> emit_wrapper seen acc n elem)
        (single_elem_struct elem)

(* The synthesised structs a repeat element renders through: a lookup's
   refined byte and a closed enum's refined element. *)
let emit_repeat_elem seen acc elem =
  emit_index_bound seen acc elem;
  emit_enum_elem_struct seen acc elem

let rec collect_casetype_decls : type a.
    (string, string) Hashtbl.t -> decl list Stdlib.ref -> a typ -> unit =
 fun seen acc typ ->
  let extract : type b. b typ -> unit =
   fun t -> collect_casetype_decls seen acc t
  in
  match typ with
  | Casetype { name; tag; cases } when is_int_dispatch_typ tag ->
      if claim_name seen ~kind:"casetypes" name (casetype_shape tag cases) then begin
        (* Visit case inners first so any sub-codecs / nested casetypes they
           reference are declared before the dispatch that names them. *)
        List.iter (fun (Case_branch { cb_inner; _ }) -> extract cb_inner) cases;
        let dispatch, wrapper = casetype_pair name tag cases in
        acc := wrapper :: dispatch :: !acc
      end
  | Union { name; tag; cases; _ } ->
      (* The tag is a field of the enclosing struct, so there is no wrapper to
         hold it: the dispatch is declared alone and the field applies it to
         the tag. *)
      if
        claim_name seen ~kind:"casetypes" name
          ("union|" ^ casetype_shape tag cases)
      then begin
        List.iter (fun (Case_branch { cb_inner; _ }) -> extract cb_inner) cases;
        acc := casetype_dispatch name tag cases :: !acc
      end
  | Casetype { cases; _ } ->
      (* String-tagged casetype: handled by [split_string_casetype_fields].
         Still walk inner case typs for nested int-tagged casetypes. *)
      List.iter (fun (Case_branch { cb_inner; _ }) -> extract cb_inner) cases
  | Codec { codec_name; codec_struct; _ } ->
      if
        (* Embedded sub-codec: emit its struct alongside the parent so 3D
         references to [codec_name] resolve, and only after visiting its own
         fields, so whatever they name is declared ahead of it. Two different
         sub-codecs under one name are refused ([claim_name]): every reference
         would resolve to the first, and a validator read a different number of
         bytes than the codec. *)
        claim_name seen ~kind:"sub-codecs" codec_name
          (codec_shape codec_name codec_struct)
      then begin
        List.iter (fun (Field f) -> extract f.field_typ) codec_struct.fields;
        acc := typedef codec_struct :: !acc
      end
  | Map { inner; _ } -> extract inner
  | Where { inner; _ } -> extract inner
  | Optional { inner; _ }
    when optional_region inner || not (has_wire_size_expr inner) ->
      (* Self-delimiting inner: declare its dependencies, then the
         gate-dispatch casetype that references them. *)
      extract inner;
      if optional_region inner then
        emit_wrapper seen acc (optional_region_name inner) inner;
      let opt_name = optional_casetype_name inner in
      if claim_name seen ~kind:"optional casetypes" opt_name (wrap_tag inner)
      then acc := optional_casetype_decl opt_name inner :: !acc
  | Optional { inner; _ } ->
      extract inner;
      emit_enum_elem_struct seen acc inner
  | Optional_or { inner; _ } -> extract inner
  | Apply { typ; _ } -> extract typ
  | Repeat { elem; _ } ->
      extract elem;
      emit_repeat_elem seen acc elem;
      Option.iter
        (fun n -> emit_wrapper seen acc n elem)
        (repeat_elem_struct elem)
  | Array { elem; _ } ->
      extract elem;
      emit_index_bound seen acc elem;
      (* A 1-byte little-endian closed-enum array element renders as the named
         enum type (declared by [enum_decls]); a wider or big-endian one renders
         through the synthesised refined-element struct, declared here. *)
      if closed_enum_elem_wide elem then emit_enum_elem_struct seen acc elem
  (* Bytes to 3D: everparse-asn1 or evercddl checks what they hold. *)
  | Single_elem { elem; at_most = false; _ } when byte_region elem -> ()
  | Single_elem { elem; _ } as typ ->
      extract elem;
      emit_single_elem seen acc typ elem
  | Byte_array_where { elt_var; cond; _ } ->
      emit_refined_byte seen acc elt_var cond
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
   consumers receive both spans through the visitor and dispatch with their
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

(* [enums] holds the shape of every enum the module declares, so two enums
   that differ are refused under one name wherever they sit: in the record, in
   a sub-codec, or among the declarations the caller gave. *)
let absorb_candidate enums (acc_rev, seen) e =
  (match e with
  | Enum_decl { name; cases; base = Pack_typ base } ->
      ignore (claim_name enums ~kind:"enums" name (enum_shape base cases))
  | _ -> ());
  match decl_name e with
  | None -> (acc_rev, seen)
  | Some n when List.mem n seen -> (acc_rev, seen)
  | Some n -> (e :: acc_rev, n :: seen)

(* A typedef needs the enums its fields name, and so does every sub-codec
   struct it brings in: an enum used only inside a nested record was otherwise
   never declared, and EverParse refused the reference. *)
let absorb_typedef_dependencies enums acc d =
  match d with
  | Typedef { struct_; _ } ->
      let nested = casetype_decls_of_struct struct_ in
      let nested_enums =
        List.concat_map
          (function Typedef { struct_ = s; _ } -> enum_decls s | _ -> [])
          nested
      in
      let candidates = enum_decls struct_ @ nested_enums @ nested in
      List.fold_left (absorb_candidate enums) acc candidates
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
  let enums = Hashtbl.create 4 in
  List.iter
    (function
      | Enum_decl { name; cases; base = Pack_typ base } ->
          ignore (claim_name enums ~kind:"enums" name (enum_shape base cases))
      | _ -> ())
    decls;
  let extra_rev, _ =
    List.fold_left
      (absorb_typedef_dependencies enums)
      ([], already_declared) split_decls
  in
  { doc; decls = List.rev extra_rev @ split_decls }

(* A [where] anywhere in a field's type becomes a field constraint, including
   one under an [enum] or a [map]: 3D takes at most one refinement after a field
   name, so leaving an inner one to render inline on the type produced two
   stacked braces and a syntax error. Rebuilding the wrapper around the
   extracted inner keeps the type it renders as unchanged. *)
let rec extract_where_constraint : type a. a typ -> bool expr option * a typ =
 fun typ ->
  match typ with
  | Where { cond; inner } -> (
      match extract_where_constraint inner with
      | Some c2, inner' -> (Some (And (cond, c2)), inner')
      | None, inner' -> (Some cond, inner'))
  | Enum ({ base; _ } as e) -> (
      match extract_where_constraint base with
      | None, _ -> (None, typ)
      | Some c, base' -> (Some c, Enum { e with base = base' }))
  | Map ({ inner; _ } as m) -> (
      match extract_where_constraint inner with
      | None, _ -> (None, typ)
      | Some c, inner' -> (Some c, Map { m with inner = inner' }))
  | _ -> (None, typ)

let combine_constraints a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some a, Some b -> Some (And (a, b))

(* A field whose value is an unsigned scalar at most 32 bits wide, seen through
   the transparent [Map] / [Enum] / [Where] wrappers. *)
let rec widenable_unsigned : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ -> true
  | Map { inner; _ } -> widenable_unsigned inner
  | Enum { base; _ } -> widenable_unsigned base
  | Where { inner; _ } -> widenable_unsigned inner
  | _ -> false

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
  | Land64 (a, b) -> collect_refs a @ collect_refs b
  | Lsr64 (a, b) -> collect_refs a @ collect_refs b
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
