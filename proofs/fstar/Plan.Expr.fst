module Plan.Expr

(* Integer expressions of a plan, their value in the native int, and the
   interval analysis the planner decides plain arithmetic with
   (Planner.range, Planner.corners). *)

open FStar.Mul

(* OCaml's native int on a 64-bit host. *)
let max_int : int = pow2 62 - 1
let min_int : int = - (pow2 62)
let in_int (x:int) : bool = min_int <= x && x <= max_int

type var = nat
type env = var -> int
let upd (en:env) (v:var) (x:int) : env = fun w -> if w = v then x else en w

(* The variables bound at a point of the plan. *)
type scope = var -> bool
let extend (sc:scope) (v:var) : scope = fun w -> w = v || sc w

type expr =
  | Lit : int -> expr
  | V : var -> expr
  | Add : expr -> expr -> expr
  | Sub : expr -> expr -> expr
  | Mul : expr -> expr -> expr
  | Shl : expr -> nat -> expr

(* The mathematical value. *)
let rec eval (en:env) (e:expr) : int =
  match e with
  | Lit n -> n
  | V v -> en v
  | Add a b -> eval en a + eval en b
  | Sub a b -> eval en a - eval en b
  | Mul a b -> eval en a * eval en b
  | Shl a k -> eval en a * pow2 k

let chk (x:int) : option int = if in_int x then Some x else None

(* The value in the native int: [None] when it or any intermediate value
   leaves it. This is the checked arithmetic of Wire_runtime.Arith; a plain
   operation is only correct where this is [Some]. *)
let rec ev (en:env) (e:expr) : option int =
  match e with
  | Lit n -> chk n
  | V v -> chk (en v)
  | Add a b -> (match ev en a, ev en b with Some x, Some y -> chk (x + y) | _ -> None)
  | Sub a b -> (match ev en a, ev en b with Some x, Some y -> chk (x - y) | _ -> None)
  | Mul a b -> (match ev en a, ev en b with Some x, Some y -> chk (x * y) | _ -> None)
  | Shl a k -> (match ev en a with Some x -> chk (x * pow2 k) | None -> None)

let rec ev_eval (en:env) (e:expr)
  : Lemma (match ev en e with Some x -> x == eval en e /\ in_int x | None -> True)
  = match e with
    | Lit _ | V _ -> ()
    | Add a b | Sub a b | Mul a b -> ev_eval en a; ev_eval en b
    | Shl a _ -> ev_eval en a

let rec closed (sc:scope) (e:expr) : bool =
  match e with
  | Lit _ -> true
  | V v -> sc v
  | Add a b | Sub a b | Mul a b -> closed sc a && closed sc b
  | Shl a _ -> closed sc a

let rec closed_extend (sc:scope) (v:var) (e:expr)
  : Lemma (requires closed sc e) (ensures closed (extend sc v) e)
  = match e with
    | Lit _ | V _ -> ()
    | Add a b | Sub a b | Mul a b -> closed_extend sc v a; closed_extend sc v b
    | Shl a _ -> closed_extend sc v a

(* Binding a variable the expression does not mention leaves its value. *)
let rec eval_upd (sc:scope) (en:env) (v:var) (x:int) (e:expr)
  : Lemma (requires closed sc e /\ not (sc v))
          (ensures eval (upd en v x) e == eval en e /\ ev (upd en v x) e == ev en e)
  = match e with
    | Lit _ | V _ -> ()
    | Add a b | Sub a b | Mul a b -> eval_upd sc en v x a; eval_upd sc en v x b
    | Shl a _ -> eval_upd sc en v x a

(* {1 Ranges} *)

type ranges = var -> option (int & int)

(* Every range holds of the environment and lies in the native int. *)
let sound_ranges (r:ranges) (en:env) : prop =
  forall (v:var). match r v with
             | Some (lo, hi) -> in_int lo /\ in_int hi /\ lo <= en v /\ en v <= hi
             | None -> True

let both (a b:int) : option (int & int) =
  if in_int a && in_int b then Some (a, b) else None

(* The interval of [e], or [None] when it may leave the native int. The
   planner also bounds products by their corners; the model leaves a product
   of two non-literals unbounded, which only makes it decide less. *)
let rec range (r:ranges) (e:expr) : option (int & int) =
  match e with
  | Lit n -> both n n
  | V v -> (match r v with Some (lo, hi) -> both lo hi | None -> None)
  | Add a b ->
    (match range r a, range r b with
     | Some (l1, h1), Some (l2, h2) -> both (l1 + l2) (h1 + h2)
     | _ -> None)
  | Sub a b ->
    (match range r a, range r b with
     | Some (l1, h1), Some (l2, h2) -> both (l1 - h2) (h1 - l2)
     | _ -> None)
  | Mul (Lit c) (Lit d) -> if in_int c && in_int d then both (c * d) (c * d) else None
  | Mul _ _ -> None
  | Shl a k ->
    (match range r a with
     | Some (lo, hi) -> both (lo * pow2 k) (hi * pow2 k)
     | None -> None)

let rec range_sound (r:ranges) (en:env) (e:expr)
  : Lemma (requires sound_ranges r en)
          (ensures (match range r e with
                    | Some (lo, hi) ->
                      ev en e == Some (eval en e) /\ lo <= eval en e /\ eval en e <= hi
                    | None -> True))
  = match e with
    | Lit _ -> ()
    | V v -> ()
    | Add a b | Sub a b -> range_sound r en a; range_sound r en b
    | Mul _ _ -> ()
    | Shl a k ->
      range_sound r en a;
      (match range r a with
       | Some (lo, hi) ->
         FStar.Math.Lemmas.lemma_mult_le_right (pow2 k) lo (eval en a);
         FStar.Math.Lemmas.lemma_mult_le_right (pow2 k) (eval en a) hi
       | None -> ())

(* [e] can be written as plain arithmetic: the ranges keep it, and every
   intermediate value, inside the native int (no [ov] in Emitter.expr). *)
let fits (r:ranges) (e:expr) : bool = Some? (range r e)

let fits_ev (r:ranges) (en:env) (e:expr)
  : Lemma (requires sound_ranges r en /\ fits r e) (ensures ev en e == Some (eval en e))
  = range_sound r en e
