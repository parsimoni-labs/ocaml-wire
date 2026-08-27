open Types

(* Single GADT walker for [a expr] used by both call surfaces (closure
   over bytes for variable-size sizing; int_array lookup for constraint
   evaluation). The two surfaces share every operator dispatch and only
   differ in how leaves ([Ref], [Param_ref], [Sizeof], [Sizeof_this],
   [Field_pos]) resolve. *)

type packed_param = Pack_param : ('a, 'k) param_handle -> packed_param

(* Leaves resolution strategy for a given access layer. The context is
   threaded as three curried arguments rather than a single packed value: the
   closure access layer passes [runtime buf base] with no per-call tuple, and
   the int-array layer passes [arr () ()] with immediate units. *)
type ('c1, 'c2, 'c3) leaves = {
  ref_ : string -> 'c1 -> 'c2 -> 'c3 -> int;
  i64 : string -> 'c1 -> 'c2 -> 'c3 -> int64;
  param_ref : packed_param -> 'c1 -> 'c2 -> 'c3 -> int;
  sizeof_typ : packed_typ -> 'c1 -> 'c2 -> 'c3 -> int;
  sizeof_this : 'c1 -> 'c2 -> 'c3 -> int;
  field_pos : 'c1 -> 'c2 -> 'c3 -> int;
}

(* The shift amount of [Lsr64] is an [int expr]; only constants make sense
   there and project, so compile just that shape. *)
let lsr64_shift_amount : int expr -> int = function
  | Int n -> n
  | _ -> invalid_arg "Wire: Lsr64 shift amount must be a constant"

let rec compile_int64 : type c1 c2 c3.
    (c1, c2, c3) leaves -> int64 expr -> c1 -> c2 -> c3 -> int64 =
 fun l e ->
  match e with
  | Int64 n -> fun _ _ _ -> n
  | Ref (I64, name) -> l.i64 name
  | Land64 (a, b) ->
      let fa = compile_int64 l a and fb = compile_int64 l b in
      fun c1 c2 c3 -> Int64.logand (fa c1 c2 c3) (fb c1 c2 c3)
  | Lsr64 (a, b) ->
      let fa = compile_int64 l a and n = lsr64_shift_amount b in
      fun c1 c2 c3 -> Int64.shift_right_logical (fa c1 c2 c3) n

let try_int64 : type a c1 c2 c3.
    (c1, c2, c3) leaves -> a expr -> (c1 -> c2 -> c3 -> int64) option =
 fun l e ->
  let go e = Some (compile_int64 l e) in
  match e with
  | Int64 _ as e -> go e
  | Ref (I64, _) as e -> go e
  | Land64 _ as e -> go e
  | Lsr64 _ as e -> go e
  | _ -> None

let rec compile_int : type c1 c2 c3.
    (c1, c2, c3) leaves -> int expr -> c1 -> c2 -> c3 -> int =
 fun l e ->
  let rec_ = compile_int l in
  match e with
  | Int n -> fun _ _ _ -> n
  | Ref (I, name) -> l.ref_ name
  | Param_ref p -> l.param_ref (Pack_param p)
  | Sizeof t -> l.sizeof_typ (Pack_typ t)
  | Sizeof_this -> l.sizeof_this
  | Field_pos -> l.field_pos
  | Add (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 + fb c1 c2 c3
  | Sub (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 - fb c1 c2 c3
  | Mul (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 * fb c1 c2 c3
  | Div (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 / fb c1 c2 c3
  | Mod (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 mod fb c1 c2 c3
  | Land (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 land fb c1 c2 c3
  | Lor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 lor fb c1 c2 c3
  | Lxor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 lxor fb c1 c2 c3
  | Lnot a ->
      let fa = rec_ a in
      fun c1 c2 c3 -> lnot (fa c1 c2 c3)
  | Lsl (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 lsl fb c1 c2 c3
  | Lsr (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 -> fa c1 c2 c3 lsr fb c1 c2 c3
  | Cast (w, a) -> (
      let fa = rec_ a in
      match w with
      | `U8 -> fun c1 c2 c3 -> fa c1 c2 c3 land 0xFF
      | `U16 -> fun c1 c2 c3 -> fa c1 c2 c3 land 0xFFFF
      | `U32 -> fun c1 c2 c3 -> fa c1 c2 c3 land UInt32.mask32
      | `U64 -> fa)
  | If_then_else (c, t, e) ->
      let fc = compile_bool l c in
      let ft = rec_ t and fe = rec_ e in
      fun c1 c2 c3 -> if fc c1 c2 c3 then ft c1 c2 c3 else fe c1 c2 c3

(* Typed-GADT projector: refines [a expr] to [int expr] / [bool expr]
   so [Eq] / [Ne] can dispatch to the right compiler at the right type. *)
and try_int : type a c1 c2 c3.
    (c1, c2, c3) leaves -> a expr -> (c1 -> c2 -> c3 -> int) option =
 fun l e ->
  let go e = Some (compile_int l e) in
  match e with
  | Int _ as e -> go e
  | Ref (I, _) as e -> go e
  | Param_ref _ as e -> go e
  | Sizeof _ as e -> go e
  | Sizeof_this as e -> go e
  | Field_pos as e -> go e
  | Add _ as e -> go e
  | Sub _ as e -> go e
  | Mul _ as e -> go e
  | Div _ as e -> go e
  | Mod _ as e -> go e
  | Land _ as e -> go e
  | Lor _ as e -> go e
  | Lxor _ as e -> go e
  | Lnot _ as e -> go e
  | Lsl _ as e -> go e
  | Lsr _ as e -> go e
  | Cast _ as e -> go e
  | If_then_else _ as e -> go e
  | _ -> None

and try_bool : type a c1 c2 c3.
    (c1, c2, c3) leaves -> a expr -> (c1 -> c2 -> c3 -> bool) option =
 fun l e ->
  let go e = Some (compile_bool l e) in
  match e with
  | Bool _ as e -> go e
  | Eq _ as e -> go e
  | Ne _ as e -> go e
  | Lt _ as e -> go e
  | Le _ as e -> go e
  | Gt _ as e -> go e
  | Ge _ as e -> go e
  | And _ as e -> go e
  | Or _ as e -> go e
  | Not _ as e -> go e
  | _ -> None

and compile_bool : type c1 c2 c3.
    (c1, c2, c3) leaves -> bool expr -> c1 -> c2 -> c3 -> bool =
 fun l e ->
  let bool_rec = compile_bool l in
  match e with
  | Bool b -> fun _ _ _ -> b
  | Eq (a, b) -> (
      match
        ( try_int l a,
          try_int l b,
          try_int64 l a,
          try_int64 l b,
          try_bool l a,
          try_bool l b )
      with
      | Some fa, Some fb, _, _, _, _ ->
          fun c1 c2 c3 -> fa c1 c2 c3 = fb c1 c2 c3
      | _, _, Some fa, Some fb, _, _ ->
          fun c1 c2 c3 -> fa c1 c2 c3 = fb c1 c2 c3
      | _, _, _, _, Some fa, Some fb ->
          fun c1 c2 c3 -> fa c1 c2 c3 = fb c1 c2 c3
      | _ -> assert false)
  | Ne (a, b) -> (
      match
        ( try_int l a,
          try_int l b,
          try_int64 l a,
          try_int64 l b,
          try_bool l a,
          try_bool l b )
      with
      | Some fa, Some fb, _, _, _, _ ->
          fun c1 c2 c3 -> fa c1 c2 c3 <> fb c1 c2 c3
      | _, _, Some fa, Some fb, _, _ ->
          fun c1 c2 c3 -> fa c1 c2 c3 <> fb c1 c2 c3
      | _, _, _, _, Some fa, Some fb ->
          fun c1 c2 c3 -> fa c1 c2 c3 <> fb c1 c2 c3
      | _ -> assert false)
  | Lt (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c1 c2 c3 -> fa c1 c2 c3 < fb c1 c2 c3
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 -> Int64.unsigned_compare (fa c1 c2 c3) (fb c1 c2 c3) < 0
      | _ -> assert false)
  | Le (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c1 c2 c3 -> fa c1 c2 c3 <= fb c1 c2 c3
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 ->
            Int64.unsigned_compare (fa c1 c2 c3) (fb c1 c2 c3) <= 0
      | _ -> assert false)
  | Gt (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c1 c2 c3 -> fa c1 c2 c3 > fb c1 c2 c3
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 -> Int64.unsigned_compare (fa c1 c2 c3) (fb c1 c2 c3) > 0
      | _ -> assert false)
  | Ge (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ -> fun c1 c2 c3 -> fa c1 c2 c3 >= fb c1 c2 c3
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 ->
            Int64.unsigned_compare (fa c1 c2 c3) (fb c1 c2 c3) >= 0
      | _ -> assert false)
  | And (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c1 c2 c3 -> fa c1 c2 c3 && fb c1 c2 c3
  | Or (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c1 c2 c3 -> fa c1 c2 c3 || fb c1 c2 c3
  | Not e ->
      let fe = bool_rec e in
      fun c1 c2 c3 -> not (fe c1 c2 c3)
  | Ref _ -> .
