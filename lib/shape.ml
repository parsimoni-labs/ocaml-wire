open Types

(* A greedy field consumes the rest of the buffer, so it is only meaningful as
   the last field: an earlier one starves every field after it (and 3D's
   [:consume-all] must be last too). This also covers an embedded sub-codec
   ending in a greedy field, whose tail would otherwise swallow the following
   field's bytes. Reject it at construction rather than silently truncating
   later fields at decode. *)
let reject_greedy_not_last name fields =
  let rec check = function
    | [] | [ _ ] -> ()
    | Types.Field f :: rest ->
        if Types.ends_greedy f.field_typ then
          Fmt.invalid_arg
            "Codec.v %s: a field that consumes the rest of the buffer \
             (all_bytes / all_zeros, or a sub-codec ending in one) must be the \
             last field, but %s is followed by more fields"
            name
            (Option.value ~default:"<anon>" f.field_name);
        check rest
  in
  check fields

(* A [Wire.where] is expressible in 3D only as a top-level field refinement
   ([UINT8 g { cond }], which projects and is enforced). Inside a container the
   projection emits invalid 3D that 3d.exe rejects ([UINT8 { cond } vals[N]] for
   an array element, [UINT8 { cond } opt[:byte-size ...]] for an optional inner):
   an element refinement cannot reference the outer field it needs. Such a where
   would ship a codec whose [.3d] does not compile while OCaml decode silently
   drops the constraint, so reject it at construction. *)
let reject_nested_where name fields =
  (* A [where true] is elided by the projection (no refinement is emitted), so it
     is a harmless no-op wrapper even inside a container. Only a non-trivial cond
     emits a refined element, which is what EverParse rejects. *)
  let rec any_real_where : type a. a Types.typ -> bool = function
    | Types.Where { cond = Types.Bool true; inner } -> any_real_where inner
    | Types.Where _ -> true
    | Types.Map { inner; _ } -> any_real_where inner
    | Types.Enum { base; _ } -> any_real_where base
    | Types.Array { elem; _ } -> any_real_where elem
    | Types.Repeat { elem; _ } -> any_real_where elem
    | Types.Optional { inner; _ } -> any_real_where inner
    | Types.Optional_or { inner; _ } -> any_real_where inner
    | Types.Single_elem { elem; _ } -> any_real_where elem
    | _ -> false
  in
  let fail fname =
    Fmt.invalid_arg
      "Codec.v %s: field %s puts a Wire.where inside a container element; a \
       where projects to 3D only as a top-level field refinement, so this \
       shape has no verified validator. Move the constraint to the field \
       itself or a codec ~where."
      name fname
  in
  let rec check_typ : type a. string -> a Types.typ -> unit =
   fun fname t ->
    match t with
    | Types.Where { inner; _ } -> check_typ fname inner
    | Types.Map { inner; _ } -> check_typ fname inner
    | Types.Enum { base; _ } -> check_typ fname base
    | Types.Array { elem; _ } -> if any_real_where elem then fail fname
    | Types.Repeat { elem; _ } -> if any_real_where elem then fail fname
    | Types.Single_elem { elem; _ } -> if any_real_where elem then fail fname
    | Types.Optional { inner; _ } -> if any_real_where inner then fail fname
    | Types.Optional_or { inner; _ } -> if any_real_where inner then fail fname
    | Types.Casetype { cases; _ } ->
        (* A [where] as a casetype case body projects to [case k: T { cond } v;],
           which is not valid 3D (a refinement is not allowed on a case body),
           unlike a top-level field refinement. Reject a non-trivial one. *)
        List.iter
          (fun (Types.Case_branch { cb_inner; _ }) ->
            if any_real_where cb_inner then fail fname)
          cases
    | _ -> ()
  in
  List.iter
    (fun (Types.Field f) ->
      check_typ (Option.value ~default:"<anon>" f.field_name) f.field_typ)
    fields

(* EverParse represents byte sizes as [u32]. Its verifier rejects a product
   [count * width] when the declared upper bound permits 2^32 or more, but the
   resulting F* diagnostic points into generated code. Recognise only the
   deliberately narrow, certain case: a literal coefficient multiplied by a
   field with a simple [field <= K] constraint. Everything else stays with
   EverParse so this diagnostic cannot create false positives. *)
let min_known_bound a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some a, Some b -> Some (Int.min a b)

let rec simple_upper_bound field : bool expr -> int option = function
  | Le (Ref (I, candidate), Int bound) when String.equal field candidate ->
      Some bound
  | And (a, b) ->
      min_known_bound (simple_upper_bound field a) (simple_upper_bound field b)
  | _ -> None

let byte_size_bound_for fields field =
  List.fold_left
    (fun bound (Types.Field f) ->
      match (f.field_name, f.constraint_) with
      | Some candidate, Some constraint_ when String.equal field candidate ->
          min_known_bound bound (simple_upper_bound field constraint_)
      | _ -> bound)
    None fields

let reject_byte_size_product name fields sized_field field coefficient =
  match byte_size_bound_for fields field with
  | Some bound when bound >= 0 && coefficient > 0 ->
      let limit = 0x1_0000_0000L in
      let coefficient64 = Int64.of_int coefficient in
      let first_overflowing_bound =
        let quotient = Int64.div limit coefficient64 in
        if Int64.rem limit coefficient64 = 0L then quotient
        else Int64.succ quotient
      in
      if Int64.compare (Int64.of_int bound) first_overflowing_bound >= 0 then
        Fmt.invalid_arg
          "Codec.v %S: byte-size for field %S multiplies %S (bounded by <= %d) \
           by %d, which permits at least 2^32 bytes; tighten the field bound \
           for EverParse's u32 byte-size limit"
          name sized_field field bound coefficient
  | _ -> ()

let rec check_byte_size_expr name fields sized_field : int expr -> unit =
  function
  | Mul (Ref (I, field), Int coefficient) | Mul (Int coefficient, Ref (I, field))
    ->
      reject_byte_size_product name fields sized_field field coefficient
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
      check_byte_size_expr name fields sized_field a;
      check_byte_size_expr name fields sized_field b
  | Lnot a | Cast (_, a) -> check_byte_size_expr name fields sized_field a
  | If_then_else (_, a, b) ->
      check_byte_size_expr name fields sized_field a;
      check_byte_size_expr name fields sized_field b
  | Int _ | Ref _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos -> ()

let rec check_byte_size_typ : type a.
    string -> Types.field list -> string -> a Types.typ -> unit =
 fun name fields sized_field -> function
  | Uint_var { size; _ }
  | Byte_array { size }
  | Byte_array_where { size; _ }
  | Byte_slice { size }
  | Single_elem { size; _ }
  | Zeroterm_at_most { size }
  | Repeat { size; _ } ->
      check_byte_size_expr name fields sized_field size
  | Map { inner; _ } -> check_byte_size_typ name fields sized_field inner
  | Where { inner; _ } -> check_byte_size_typ name fields sized_field inner
  | Apply { typ; _ } -> check_byte_size_typ name fields sized_field typ
  | Optional { inner; _ } -> check_byte_size_typ name fields sized_field inner
  | Optional_or { inner; _ } ->
      check_byte_size_typ name fields sized_field inner
  | _ -> ()

let reject_certain_byte_size_mul name fields =
  List.iter
    (fun (Types.Field f) ->
      check_byte_size_typ name fields
        (Option.value ~default:"<anon>" f.field_name)
        f.field_typ)
    fields

let reject_duplicate_field_names name fields =
  let seen = Hashtbl.create (List.length fields) in
  List.iter
    (fun (Types.Field f) ->
      match f.field_name with
      | None -> ()
      | Some field when Hashtbl.mem seen field ->
          Fmt.invalid_arg "Codec.v %S: duplicate field name %S" name field
      | Some field -> Hashtbl.add seen field ())
    fields

let reject_invalid_codec_shape name fields =
  reject_duplicate_field_names name fields;
  reject_greedy_not_last name fields;
  reject_nested_where name fields;
  reject_certain_byte_size_mul name fields
