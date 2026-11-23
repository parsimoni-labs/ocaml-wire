open Types

(* Single GADT walker for [a expr] used by both call surfaces (closure
   over bytes for variable-size sizing; int_array lookup for constraint
   evaluation). The two surfaces share every operator dispatch and only
   differ in how leaves ([Ref], [Param_ref], [Sizeof], [Sizeof_this],
   [Field_pos]) resolve. *)

type packed_param = Pack_param : ('a, 'k) param_handle -> packed_param
type wide = Unsigned | Signed

(* Leaves resolution strategy for a given access layer. The context is
   threaded as four curried arguments rather than a single packed value: the
   closure access layer passes [runtime input_end buf base] with no per-call
   tuple, and the int-array layer passes [arr () () ()] with immediate
   units. *)
type ('c1, 'c2, 'c3, 'c4) leaves = {
  ref_ : string -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> int;
  i64 : string -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> int64;
  wide : string -> wide option;
  wide_expr : int expr -> bool;
  param_ref : packed_param -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> int;
  sizeof_typ : packed_typ -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> int;
  sizeof_this : 'c1 -> 'c2 -> 'c3 -> 'c4 -> int;
  field_pos : 'c1 -> 'c2 -> 'c3 -> 'c4 -> int;
}

(* The shift amount of [Lsr64] is an [int expr]; only constants make sense
   there and project, so compile just that shape. *)
let lsr64_shift_amount : int expr -> int = function
  | Int n -> n
  | _ -> invalid_arg "Wire: Lsr64 shift amount must be a constant"

let rec compile_int64 : type c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> int64 expr -> c1 -> c2 -> c3 -> c4 -> int64 =
 fun l e ->
  match e with
  | Int64 n -> fun _ _ _ _ -> n
  | Ref (I64, name) -> l.i64 name
  | Land64 (a, b) ->
      let fa = compile_int64 l a and fb = compile_int64 l b in
      fun c1 c2 c3 c4 -> Int64.logand (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  | Lsr64 (a, b) ->
      let fa = compile_int64 l a and n = lsr64_shift_amount b in
      fun c1 c2 c3 c4 -> Int64.shift_right_logical (fa c1 c2 c3 c4) n

let try_int64 : type a c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> a expr -> (c1 -> c2 -> c3 -> c4 -> int64) option
    =
 fun l e ->
  let go e = Some (compile_int64 l e) in
  match e with
  | Int64 _ as e -> go e
  | Ref (I64, _) as e -> go e
  | Land64 _ as e -> go e
  | Lsr64 _ as e -> go e
  | _ -> None

(* A field a native int cannot always hold is read at its full width, as
   EverParse reads it: an int expression over one is evaluated on 64 bits,
   unsigned when any such field is unsigned, and an overflow, an underflow or a
   negative operand of an unsigned expression is [Value_out_of_range]. So is
   one whose value can leave the native int ([wide_expr]), unsigned, as 3D
   computes it at the type that holds it. *)
let join a b =
  match (a, b) with
  | Some Unsigned, _ | _, Some Unsigned -> Some Unsigned
  | Some Signed, _ | _, Some Signed -> Some Signed
  | None, None -> None

let rec wide_of : type c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> int expr -> wide option =
 fun l e ->
  let w = wide_of l in
  match e with
  | Ref (I, name) -> l.wide name
  | Int _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos -> None
  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Land (a, b)
  | Lor (a, b)
  | Lxor (a, b)
  | Lsl (a, b)
  | Lsr (a, b)
  | If_then_else (_, a, b) ->
      join (w a) (w b)
  | Lnot a | Cast (_, a) -> w a

let width l e =
  match wide_of l e with
  | Some w -> Some w
  | None -> if l.wide_expr e then Some Unsigned else None

let out_of_range v = raise_out_of_range ~at:0 v

let of_int w n =
  match w with
  | Unsigned when n < 0 -> out_of_range (Int64.of_int n)
  | Unsigned | Signed -> Int64.of_int n

let to_int w v =
  match w with
  | Unsigned -> (
      match Int64.unsigned_to_int v with Some n -> n | None -> out_of_range v)
  | Signed ->
      let n = Int64.to_int v in
      if Int64.equal (Int64.of_int n) v then n else out_of_range v

let compare_wide = function
  | Unsigned -> Int64.unsigned_compare
  | Signed -> Int64.compare

let add_wide w a b =
  let s = Int64.add a b in
  let overflows =
    match w with
    | Unsigned -> Int64.unsigned_compare s a < 0
    | Signed -> Int64.(logand (logxor a s) (logxor b s)) < 0L
  in
  if overflows then out_of_range s else s

let sub_wide w a b =
  let d = Int64.sub a b in
  let overflows =
    match w with
    | Unsigned -> Int64.unsigned_compare a b < 0
    | Signed -> Int64.(logand (logxor a b) (logxor a d)) < 0L
  in
  if overflows then out_of_range d else d

let mul_wide w a b =
  let p = Int64.mul a b in
  let overflows =
    (not (Int64.equal a 0L))
    &&
    match w with
    | Unsigned -> not (Int64.equal (Int64.unsigned_div p a) b)
    | Signed ->
        (Int64.equal a (-1L) && Int64.equal b Int64.min_int)
        || not (Int64.equal (Int64.div p a) b)
  in
  if overflows then out_of_range p else p

let div_wide w a b =
  if Int64.equal b 0L then raise_zero_divisor ~at:0
  else
    match w with
    | Unsigned -> Int64.unsigned_div a b
    | Signed ->
        if Int64.equal a Int64.min_int && Int64.equal b (-1L) then
          out_of_range a
        else Int64.div a b

let rem_wide w a b =
  if Int64.equal b 0L then raise_zero_divisor ~at:0
  else
    match w with Unsigned -> Int64.unsigned_rem a b | Signed -> Int64.rem a b

(* [e] as an [int expr] when it is one. *)
let as_int : type a. a expr -> int expr option = function
  | Int _ as e -> Some e
  | Ref (I, _) as e -> Some e
  | Param_ref _ as e -> Some e
  | Sizeof _ as e -> Some e
  | Sizeof_this as e -> Some e
  | Field_pos as e -> Some e
  | Add _ as e -> Some e
  | Sub _ as e -> Some e
  | Mul _ as e -> Some e
  | Div _ as e -> Some e
  | Mod _ as e -> Some e
  | Land _ as e -> Some e
  | Lor _ as e -> Some e
  | Lxor _ as e -> Some e
  | Lnot _ as e -> Some e
  | Lsl _ as e -> Some e
  | Lsr _ as e -> Some e
  | Cast _ as e -> Some e
  | If_then_else _ as e -> Some e
  | _ -> None

let rec compile_int : type c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> int expr -> c1 -> c2 -> c3 -> c4 -> int =
 fun l e ->
  match width l e with
  | None -> compile_narrow l e
  | Some w ->
      let f = compile_wide l w e in
      fun c1 c2 c3 c4 -> to_int w (f c1 c2 c3 c4)

and compile_narrow : type c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> int expr -> c1 -> c2 -> c3 -> c4 -> int =
 fun l e ->
  let rec_ = compile_narrow l in
  match e with
  | Int n -> fun _ _ _ _ -> n
  | Ref (I, name) -> l.ref_ name
  | Param_ref p -> l.param_ref (Pack_param p)
  | Sizeof t -> l.sizeof_typ (Pack_typ t)
  | Sizeof_this -> l.sizeof_this
  | Field_pos -> l.field_pos
  (* An operation with a literal operand, the common shape of a size
     ([ihl * 4 - 20], [len - 8]), applies the literal directly rather than
     calling a closure that returns it: one call per node instead of three. *)
  | Add (a, Int k) ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> checked_add (fa c1 c2 c3 c4) k
  | Sub (a, Int k) ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> checked_sub (fa c1 c2 c3 c4) k
  | Mul (a, Int k) ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> checked_mul (fa c1 c2 c3 c4) k
  | Land (a, Int k) ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 land k
  | Lsl (a, Int k) ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 lsl k
  | Lsr (a, Int k) ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 lsr k
  | Add (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> checked_add (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  | Sub (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> checked_sub (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  | Mul (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> checked_mul (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  | Div (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> checked_div (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  | Mod (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> checked_mod (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  | Land (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 land fb c1 c2 c3 c4
  | Lor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 lor fb c1 c2 c3 c4
  | Lxor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 lxor fb c1 c2 c3 c4
  | Lnot a ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> lnot (fa c1 c2 c3 c4)
  | Lsl (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 lsl fb c1 c2 c3 c4
  | Lsr (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 lsr fb c1 c2 c3 c4
  | Cast (w, a) -> (
      let fa = rec_ a in
      match w with
      | `U8 -> fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 land 0xFF
      | `U16 -> fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 land 0xFFFF
      | `U32 -> fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 land UInt32.mask32
      | `U64 -> fa)
  | If_then_else (c, t, e) ->
      let fc = compile_bool l c in
      let ft = rec_ t and fe = rec_ e in
      fun c1 c2 c3 c4 ->
        if fc c1 c2 c3 c4 then ft c1 c2 c3 c4 else fe c1 c2 c3 c4

and compile_wide : type c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> wide -> int expr -> c1 -> c2 -> c3 -> c4 -> int64
    =
 fun l w e ->
  let rec_ = compile_wide l w in
  let op f a b =
    let fa = rec_ a and fb = rec_ b in
    fun c1 c2 c3 c4 -> f (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  in
  let shift f a b =
    let fa = rec_ a and fb = compile_int l b in
    fun c1 c2 c3 c4 -> f (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)
  in
  match e with
  | Ref (I, name) when Option.is_some (l.wide name) -> l.i64 name
  | Int n ->
      let v = of_int w n in
      fun _ _ _ _ -> v
  | Ref (I, _) | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos ->
      let f = compile_narrow l e in
      fun c1 c2 c3 c4 -> of_int w (f c1 c2 c3 c4)
  | Add (a, b) -> op (add_wide w) a b
  | Sub (a, b) -> op (sub_wide w) a b
  | Mul (a, b) -> op (mul_wide w) a b
  | Div (a, b) -> op (div_wide w) a b
  | Mod (a, b) -> op (rem_wide w) a b
  | Land (a, b) -> op Int64.logand a b
  | Lor (a, b) -> op Int64.logor a b
  | Lxor (a, b) -> op Int64.logxor a b
  | Lnot a ->
      let fa = rec_ a in
      fun c1 c2 c3 c4 -> Int64.lognot (fa c1 c2 c3 c4)
  | Lsl (a, b) -> shift Int64.shift_left a b
  | Lsr (a, b) -> shift Int64.shift_right_logical a b
  | Cast (width, a) -> (
      let fa = rec_ a in
      match width with
      | `U8 -> fun c1 c2 c3 c4 -> Int64.logand (fa c1 c2 c3 c4) 0xFFL
      | `U16 -> fun c1 c2 c3 c4 -> Int64.logand (fa c1 c2 c3 c4) 0xFFFFL
      | `U32 -> fun c1 c2 c3 c4 -> Int64.logand (fa c1 c2 c3 c4) 0xFFFF_FFFFL
      | `U64 -> fa)
  | If_then_else (c, t, e) ->
      let fc = compile_bool l c in
      let ft = rec_ t and fe = rec_ e in
      fun c1 c2 c3 c4 ->
        if fc c1 c2 c3 c4 then ft c1 c2 c3 c4 else fe c1 c2 c3 c4

(* The full width at which a comparison of [a] with [b] is decided, with both
   sides compiled at it, when either reads a wide field. *)
and compare_at : type a c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves ->
    a expr ->
    a expr ->
    ((c1 -> c2 -> c3 -> c4 -> int64)
    * (c1 -> c2 -> c3 -> c4 -> int64)
    * (int64 -> int64 -> int))
    option =
 fun l a b ->
  match (as_int a, as_int b) with
  | Some a, Some b -> (
      match join (width l a) (width l b) with
      | None -> None
      | Some w -> Some (compile_wide l w a, compile_wide l w b, compare_wide w))
  | _ -> None

(* Typed-GADT projector: refines [a expr] to [int expr] / [bool expr]
   so [Eq] / [Ne] can dispatch to the right compiler at the right type. *)
and try_int : type a c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> a expr -> (c1 -> c2 -> c3 -> c4 -> int) option =
 fun l e -> Option.map (compile_int l) (as_int e)

and try_bool : type a c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> a expr -> (c1 -> c2 -> c3 -> c4 -> bool) option =
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

and compile_bool : type c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> bool expr -> c1 -> c2 -> c3 -> c4 -> bool =
 fun l e ->
  let wide a b test =
    match compare_at l a b with
    | None -> None
    | Some (fa, fb, cmp) ->
        Some (fun c1 c2 c3 c4 -> test (cmp (fa c1 c2 c3 c4) (fb c1 c2 c3 c4)))
  in
  let at_full_width =
    match e with
    | Eq (a, b) -> wide a b (fun c -> c = 0)
    | Ne (a, b) -> wide a b (fun c -> c <> 0)
    | Lt (a, b) -> wide a b (fun c -> c < 0)
    | Le (a, b) -> wide a b (fun c -> c <= 0)
    | Gt (a, b) -> wide a b (fun c -> c > 0)
    | Ge (a, b) -> wide a b (fun c -> c >= 0)
    | _ -> None
  in
  match at_full_width with Some f -> f | None -> compile_bool_narrow l e

and compile_bool_narrow : type c1 c2 c3 c4.
    (c1, c2, c3, c4) leaves -> bool expr -> c1 -> c2 -> c3 -> c4 -> bool =
 fun l e ->
  let bool_rec = compile_bool l in
  match e with
  | Bool b -> fun _ _ _ _ -> b
  (* A comparison with a literal, as in a gate ([protocol = 6]) or a
     constraint ([ihl >= 5]), compares with the literal directly. *)
  | Eq (a, Int k) ->
      let fa = compile_int l a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 = k
  | Ne (a, Int k) ->
      let fa = compile_int l a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 <> k
  | Lt (a, Int k) ->
      let fa = compile_int l a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 < k
  | Le (a, Int k) ->
      let fa = compile_int l a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 <= k
  | Gt (a, Int k) ->
      let fa = compile_int l a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 > k
  | Ge (a, Int k) ->
      let fa = compile_int l a in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 >= k
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
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 = fb c1 c2 c3 c4
      | _, _, Some fa, Some fb, _, _ ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 = fb c1 c2 c3 c4
      | _, _, _, _, Some fa, Some fb ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 = fb c1 c2 c3 c4
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
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 <> fb c1 c2 c3 c4
      | _, _, Some fa, Some fb, _, _ ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 <> fb c1 c2 c3 c4
      | _, _, _, _, Some fa, Some fb ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 <> fb c1 c2 c3 c4
      | _ -> assert false)
  | Lt (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 < fb c1 c2 c3 c4
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 c4 ->
            Int64.unsigned_compare (fa c1 c2 c3 c4) (fb c1 c2 c3 c4) < 0
      | _ -> assert false)
  | Le (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 <= fb c1 c2 c3 c4
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 c4 ->
            Int64.unsigned_compare (fa c1 c2 c3 c4) (fb c1 c2 c3 c4) <= 0
      | _ -> assert false)
  | Gt (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 > fb c1 c2 c3 c4
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 c4 ->
            Int64.unsigned_compare (fa c1 c2 c3 c4) (fb c1 c2 c3 c4) > 0
      | _ -> assert false)
  | Ge (a, b) -> (
      match (try_int l a, try_int l b, try_int64 l a, try_int64 l b) with
      | Some fa, Some fb, _, _ ->
          fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 >= fb c1 c2 c3 c4
      | _, _, Some fa, Some fb ->
          fun c1 c2 c3 c4 ->
            Int64.unsigned_compare (fa c1 c2 c3 c4) (fb c1 c2 c3 c4) >= 0
      | _ -> assert false)
  | And (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 && fb c1 c2 c3 c4
  | Or (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c1 c2 c3 c4 -> fa c1 c2 c3 c4 || fb c1 c2 c3 c4
  | Not e ->
      let fe = bool_rec e in
      fun c1 c2 c3 c4 -> not (fe c1 c2 c3 c4)
  | Ref _ -> .
