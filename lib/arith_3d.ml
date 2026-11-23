open Dsl

(* The integer types of 3D. *)
type ty = T8 | T16 | T32 | T64

let bits = function T8 -> 8 | T16 -> 16 | T32 -> 32 | T64 -> 64
let wider a b = if bits a >= bits b then a else b
let ones n = if n >= 64 then -1L else Int64.(pred (shift_left 1L n))
let top t = ones (bits t)
let ule a b = Int64.unsigned_compare a b <= 0
let ult a b = Int64.unsigned_compare a b < 0
let umin a b = if ule a b then a else b
let umax a b = if ule a b then b else a
let fits t v = ule v (top t)

let smallest v =
  if fits T8 v then T8
  else if fits T16 v then T16
  else if fits T32 v then T32
  else T64

let type_name t = Fmt.str "UINT%d" (bits t)
let cast_of_ty = function T8 -> `U8 | T16 -> `U16 | T32 -> `U32 | T64 -> `U64
let of_cast = function `U8 -> T8 | `U16 -> T16 | `U32 -> T32 | `U64 -> T64

(* The narrowest type at least [t] whose width exceeds a shift amount [k]. *)
let above k t =
  List.find
    (fun t' ->
      bits t' >= bits t && Int64.compare k (Int64.of_int (bits t')) < 0)
    [ T8; T16; T32; T64 ]

(* What an integer subexpression can be, as F* knows it: its 3D type, its
   unsigned bounds, and whether OCaml evaluates it on 64 bits ([wide]): it
   reads a field a native int cannot hold, or it or a subexpression can leave
   the native int or shifts by an amount the native int cannot. *)
type range = { ty : ty; lo : int64; hi : int64; wide : bool }

let full ?(wide = false) ty = { ty; lo = 0L; hi = top ty; wide }
let exact v = { ty = smallest v; lo = v; hi = v; wide = false }

(* How 3D reads a field. A signed field is its unsigned bit pattern, which only
   a comparison with a literal reads the way OCaml does; a float is its IEEE 754
   bits, which no ordering reads as OCaml does. *)
type leaf =
  | Unsigned of range
  | Signed of int
  | Float of range
  | Opaque
  | Formal

(* A [Formal] is a parameter of a declaration the projection synthesises (an
   optional region, a sized wrapper), through which the struct that applies it
   passes its own fields. The expressions over it are that struct's, checked
   there with the fields' own types, so they project as written. *)

(* A 32-bit value is wide where OCaml's int has 32 bits or fewer, as
   [Codec_repr.wide_of] reads it. *)
let narrow_int = Sys.int_size <= 32

let base_ty : bitfield_base -> ty = function
  | U8 -> T8
  | U16 _ -> T16
  | U32 _ -> T32

let rec leaf : type a. a typ -> leaf = function
  | Uint8 -> Unsigned (full T8)
  | Uint16 _ -> Unsigned (full T16)
  | Uint32 _ -> Unsigned (full ~wide:narrow_int T32)
  | Uint64 _ -> Unsigned (full ~wide:true T64)
  | Int8 -> Signed 8
  | Int16 _ -> Signed 16
  | Int32 _ -> Signed 32
  | Int64 _ -> Signed 64
  | Float32 _ -> Float (full ~wide:narrow_int T32)
  | Float64 _ -> Float (full ~wide:true T64)
  | Bits { width; base; _ } ->
      let ty = base_ty base in
      Unsigned { (full ~wide:(narrow_int && ty = T32) ty) with hi = ones width }
  | Map { inner; _ } -> leaf inner
  | Where { inner; _ } -> leaf inner
  | Enum { base; _ } -> leaf base
  | Optional { inner; _ } -> leaf inner
  | Optional_or { inner; _ } -> leaf inner
  | _ -> Opaque

(* The leaves of a struct, and what the conditions an expression sits under
   prove: [ge] holds the pairs [(a, b)] with [a >= b]; [a > b] is
   [a >= b + 1]. *)
type env = {
  leaves : (string * leaf) list;
  ge : (packed_expr * packed_expr) list;
  formals : bool;
  trusted : bool;
  written : bool;
}

let empty =
  { leaves = []; ge = []; formals = false; trusted = false; written = false }

(* A [trusted] environment computes how wide an expression the struct's own
   environment has checked: it knows no refinement or condition, so it bounds
   what it cannot prove by the type, and refuses nothing. A [written] one checks
   an expression 3D computes as written, at its operands' type: it casts
   nothing. *)

let rec pp : type a. a expr Fmt.t =
 fun ppf e ->
  let bin op a b = Fmt.pf ppf "(%a %s %a)" pp a op pp b in
  match e with
  | Int n -> Fmt.int ppf n
  | Int64 n -> Fmt.pf ppf "%Lu" n
  | Bool b -> Fmt.bool ppf b
  | Ref (_, n) -> Fmt.string ppf n
  | Param_ref p -> Fmt.string ppf p.name
  | Sizeof _ -> Fmt.string ppf "sizeof (_)"
  | Sizeof_this -> Fmt.string ppf "sizeof (this)"
  | Field_pos -> Fmt.string ppf "field_pos"
  | Add (a, b) -> bin "+" a b
  | Sub (a, b) -> bin "-" a b
  | Mul (a, b) -> bin "*" a b
  | Div (a, b) -> bin "/" a b
  | Mod (a, b) -> bin "mod" a b
  | Land (a, b) -> bin "land" a b
  | Lor (a, b) -> bin "lor" a b
  | Lxor (a, b) -> bin "lxor" a b
  | Lnot a -> Fmt.pf ppf "(lnot %a)" pp a
  | Lsl (a, b) -> bin "lsl" a b
  | Lsr (a, b) -> bin "lsr" a b
  | Land64 (a, b) -> bin "land64" a b
  | Lsr64 (a, b) -> Fmt.pf ppf "(%a lsr64 %a)" pp a pp b
  | Eq (a, b) -> bin "=" a b
  | Ne (a, b) -> bin "<>" a b
  | Lt (a, b) -> bin "<" a b
  | Le (a, b) -> bin "<=" a b
  | Gt (a, b) -> bin ">" a b
  | Ge (a, b) -> bin ">=" a b
  | And (a, b) -> bin "&&" a b
  | Or (a, b) -> bin "||" a b
  | Not a -> Fmt.pf ppf "(not %a)" pp a
  | Cast (t, a) -> Fmt.pf ppf "(to_uint%d %a)" (bits (of_cast t)) pp a
  | If_then_else (c, a, b) ->
      Fmt.pf ppf "(if %a then %a else %a)" pp c pp a pp b

(* What a [trusted] environment cannot bound. *)
exception Unknown

let fail e fmt = Fmt.kstr (fun r -> Fmt.invalid_arg "Wire: %a %s" pp e r) fmt
let key e = Fmt.str "%a" pp e

(* {1 What a condition proves} *)

type op = [ `Eq | `Ne | `Lt | `Le | `Gt | `Ge ]

let negate : op -> op = function
  | `Eq -> `Ne
  | `Ne -> `Eq
  | `Lt -> `Ge
  | `Le -> `Gt
  | `Gt -> `Le
  | `Ge -> `Lt

(* [k OP x] is [x (mirror OP) k]. *)
let mirror : op -> op = function
  | `Lt -> `Gt
  | `Le -> `Ge
  | `Gt -> `Lt
  | `Ge -> `Le
  | (`Eq | `Ne) as op -> op

(* [r] where [x OP k] holds, or [None] when nothing in [r] satisfies it. *)
let bound (op : op) k r =
  let k = Int64.of_int k in
  let r =
    match op with
    | `Lt ->
        if Int64.equal k 0L then None
        else Some { r with hi = umin r.hi (Int64.pred k) }
    | `Le -> Some { r with hi = umin r.hi k }
    | `Gt ->
        if Int64.equal k (-1L) then None
        else Some { r with lo = umax r.lo (Int64.succ k) }
    | `Ge -> Some { r with lo = umax r.lo k }
    | `Eq -> Some { r with lo = umax r.lo k; hi = umin r.hi k }
    | `Ne ->
        if Int64.equal k r.lo && ult r.lo r.hi then
          Some { r with lo = Int64.succ k }
        else if Int64.equal k r.hi && ult r.lo r.hi then
          Some { r with hi = Int64.pred k }
        else Some r
  in
  match r with Some r when ule r.lo r.hi -> Some r | _ -> None

let narrow env n op k =
  match List.assoc_opt n env.leaves with
  | Some (Unsigned r) when k >= 0 -> (
      match bound op k r with
      | Some r -> { env with leaves = (n, Unsigned r) :: env.leaves }
      | None -> env)
  | _ -> env

let ge env a b = { env with ge = (Pack_expr a, Pack_expr b) :: env.ge }

let proves_ge env a b =
  env.ge <> []
  &&
  let a = key a and b = key b in
  List.exists
    (fun (Pack_expr x, Pack_expr y) ->
      String.equal (key x) a && String.equal (key y) b)
    env.ge

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

(* [env] once [cond] is known to be [holds]: what 3D's proof knows under the
   left side of [&&], the negated left side of [||] and each branch of [?:]. *)
let rec assume env (cond : bool expr) holds =
  let cmp : type x. op -> x expr -> x expr -> env =
   fun op a b ->
    let op = if holds then op else negate op in
    let rel =
      match (a, b) with
      | Ref (I, n), Int k -> narrow env n op k
      | Int k, Ref (I, n) -> narrow env n (mirror op) k
      | _ -> env
    in
    let gt rel (a : x expr) (b : x expr) =
      match as_int b with
      | Some b' -> ge (ge rel a b) a (Add (b', Int 1))
      | None -> ge rel a b
    in
    match (op, a, b) with
    | `Ge, a, b -> ge rel a b
    | `Gt, a, b -> gt rel a b
    | `Le, a, b -> ge rel b a
    | `Lt, a, b -> gt rel b a
    | `Eq, a, b -> ge (ge rel a b) b a
    | `Ne, _, _ -> rel
  in
  match cond with
  | And (a, b) -> if holds then assume (assume env a true) b true else env
  | Or (a, b) -> if holds then env else assume (assume env a false) b false
  | Not a -> assume env a (not holds)
  | Eq (a, b) -> cmp `Eq a b
  | Ne (a, b) -> cmp `Ne a b
  | Lt (a, b) -> cmp `Lt a b
  | Le (a, b) -> cmp `Le a b
  | Gt (a, b) -> cmp `Gt a b
  | Ge (a, b) -> cmp `Ge a b
  | Bool _ -> env
  | Ref _ -> .

(* {1 Ranges} *)

let add_u a b =
  let s = Int64.add a b in
  if ult s a then None else Some s

let mul_u a b =
  if Int64.equal a 0L then Some 0L
  else
    let p = Int64.mul a b in
    if Int64.equal (Int64.unsigned_div p a) b then Some p else None

let shl_u a k =
  if Int64.equal a 0L then Some 0L
  else if k >= 64 then None
  else
    let r = Int64.shift_left a k in
    if Int64.equal (Int64.shift_right_logical r k) a then Some r else None

(* F* reasons about a shift or a division by a known amount; any other one
   it bounds only by its type. *)
let constant r = Int64.equal r.lo r.hi
let shr_u a k = if k >= 64 then 0L else Int64.shift_right_logical a k
let to_shift k = if ule k 64L then Int64.to_int k else 64
let native = Int64.of_int max_int
let native_shift = Int64.of_int Sys.int_size

(* OCaml evaluates on 64 bits a value that can leave its native int, as the C
   does at the 3D type that holds it. *)
let past_native r = if ule r.hi native then r else { r with wide = true }
let cast ty r e = if r.ty = ty then e else Cast (cast_of_ty ty, e)

(* The operands of [a OP b] in type [ty]. A literal beside a cast operand is
   left as written, since the cast already sets the type of the operation. *)
let operands ty ra (a : int expr) rb (b : int expr) =
  match (a, b) with
  | _, Int _ -> (cast ty ra a, b)
  | Int _, _ -> (a, cast ty rb b)
  | _ -> (cast ty ra a, cast ty rb b)

let read env e n =
  match List.assoc_opt n env.leaves with
  | Some (Unsigned r) | Some (Float r) -> r
  | Some (Signed w) when env.trusted -> full (smallest (ones w))
  | (Some Opaque | None) when env.trusted -> raise Unknown
  | Some (Signed 64) ->
      fail e
        "reads the signed 64-bit field %S other than in an equality with a \
         non-negative literal; 3D reads it as its unsigned bit pattern, so it \
         cannot be projected faithfully"
        n
  | Some (Signed _) ->
      fail e
        "reads the signed field %S other than in a comparison with an integer \
         literal; 3D reads it as its unsigned bit pattern, so it cannot be \
         projected faithfully"
        n
  | Some Opaque ->
      fail e "reads the field %S, which 3D does not read as an integer" n
  | Some Formal -> full T32
  | None -> fail e "reads %S, which is not a field of this struct" n

let param : type a k. (a, k) param_handle -> range =
 fun p ->
  match leaf p.typ with
  | Unsigned r | Float r -> r
  | Signed w -> full (smallest (ones w))
  | Opaque | Formal -> full T64

let formal env e =
  List.exists
    (fun n -> List.assoc_opt n env.leaves = Some Formal)
    (Lower_3d.collect_refs e)

let rec int64_ env (e : int64 expr) : int64 expr * range =
  match e with
  | Int64 v -> (e, { ty = T64; lo = v; hi = v; wide = true })
  | Ref (I64, n) -> (e, read env e n)
  | Land64 (a, b) ->
      let a', ra = int64_ env a in
      let b', rb = int64_ env b in
      (Land64 (a', b'), { (full T64) with wide = ra.wide || rb.wide })
  | Lsr64 (a, (Int k as b)) when k >= 0 && k < 64 ->
      let a', ra = int64_ env a in
      (Lsr64 (a', b), { ra with lo = shr_u ra.lo k; hi = shr_u ra.hi k })
  | Lsr64 _ -> fail e "shifts by something other than a constant below 64"

let is_float env n =
  match List.assoc_opt n env.leaves with Some (Float _) -> true | _ -> false

(* A signed [width]-bit field [n] compared with the literal [k], as 3D reads
   it: ordering flips the sign bit of both sides ([a <_s b] iff
   [(a ^ 2^(w-1)) <_u (b ^ 2^(w-1))]), equality compares the bit pattern, and
   a literal outside the field's range folds to a constant. *)
let signed_literal n width (op : op) k : bool expr =
  let signbit = 1 lsl (width - 1) in
  let mask = (1 lsl width) - 1 in
  let smax = signbit - 1 and smin = -signbit in
  let self : int expr = Ref (I, n) in
  if k > smax then
    match op with `Lt | `Le | `Ne -> Bool true | `Gt | `Ge | `Eq -> Bool false
  else if k < smin then
    match op with `Gt | `Ge | `Ne -> Bool true | `Lt | `Le | `Eq -> Bool false
  else
    let flipped : int expr = Lxor (self, Int signbit) in
    let c : int expr = Int (k land mask lxor signbit) in
    match op with
    | `Eq -> Eq (self, Int (k land mask))
    | `Ne -> Ne (self, Int (k land mask))
    | `Lt -> Lt (flipped, c)
    | `Le -> Le (flipped, c)
    | `Gt -> Gt (flipped, c)
    | `Ge -> Ge (flipped, c)

(* What a [trusted] environment knows of a value past 64 bits, which the
   struct's own environment bounds. *)
let past_64 env e =
  if env.trusted then full ~wide:true T64
  else fail e "can exceed 64 bits, which no 3D type holds"

(* A position in a buffer, as [sizeof (this)] and [field_pos] are. *)
let position =
  { (full T32) with hi = umin (top T32) (Int64.of_int Sys.max_string_length) }

let rec int_ env (e : int expr) : int expr * range =
  match e with
  | Int n when n < 0 && env.trusted -> raise Unknown
  | Int n ->
      if n < 0 then fail e "is a negative literal, which 3D cannot write"
      else (e, exact (Int64.of_int n))
  | Ref (I, n) -> (e, read env e n)
  | Param_ref p when p.mutable_ && not env.trusted ->
      fail e
        "reads the output %S, which 3D holds as a pointer and does not read in \
         an expression"
        p.name
  | Param_ref p -> (e, param p)
  | Sizeof t -> (
      match Lower_3d.inner_wire_size t with
      | Some s -> (e, { (exact (Int64.of_int s)) with ty = T32 })
      | None -> (e, full T32))
  | Sizeof_this -> (e, position)
  | Field_pos when env.trusted -> (e, position)
  | Field_pos ->
      fail e
        "has no 3D projection: 3D reads the field position only into a [var] \
         of an action"
  | Add (a, b) -> grow env e (fun a b -> Add (a, b)) add_u a b
  | Mul (a, b) -> grow env e (fun a b -> Mul (a, b)) mul_u a b
  | Sub (a, b) -> subtract env e a b
  | Div (a, b) ->
      divide env e a b
        (fun a b -> Div (a, b))
        (fun ra rb ->
          (Int64.unsigned_div ra.lo rb.hi, Int64.unsigned_div ra.hi rb.lo))
  | Mod (a, b) ->
      divide env e a b
        (fun a b -> Mod (a, b))
        (fun ra rb -> (0L, umin ra.hi (Int64.pred rb.hi)))
  | Land (a, Lnot m) -> clear env e a m (fun a m -> Land (a, Lnot m))
  | Land (Lnot m, a) -> clear env e a m (fun a m -> Land (Lnot m, a))
  | Land (a, b) -> bitwise env a b (fun a b -> Land (a, b))
  | Lor (a, b) -> bitwise env a b (fun a b -> Lor (a, b))
  | Lxor (a, b) -> bitwise env a b (fun a b -> Lxor (a, b))
  | Lnot a when env.trusted ->
      let a', ra = int_ env a in
      (Lnot a', full ra.ty)
  | Lnot _ ->
      fail e
        "has no faithful 3D projection: 3D complements within the operand's \
         type and OCaml over its native int"
  | Lsl (a, b) -> shift_left env e a b
  | Lsr (a, b) -> shift_right env e a b
  | Cast (w, a) ->
      let a', ra = int_ env a in
      let ty = of_cast w in
      if bits ty < bits ra.ty && (not (fits ty ra.hi)) && not env.trusted then
        fail e "narrows a value that can exceed %s, which 3d.exe cannot verify"
          (type_name ty);
      ( Cast (w, a'),
        { ra with ty; lo = umin ra.lo (top ty); hi = umin ra.hi (top ty) } )
  | If_then_else (c, t, f) ->
      let c' = bool_ env c in
      let t', rt = int_ (assume env c true) t in
      let f', rf = int_ (assume env c false) f in
      ( If_then_else (c', t', f'),
        {
          ty = wider rt.ty rf.ty;
          lo = umin rt.lo rf.lo;
          hi = umax rt.hi rf.hi;
          wide = rt.wide || rf.wide;
        } )

(* A subtraction 3D verifies: its left side is known to be at least its
   right side, from the ranges or from a condition it sits under, or is the
   same expression. *)
and subtract env e a b =
  let a', ra = int_ env a in
  let b', rb = int_ env b in
  let lo =
    if ule rb.hi ra.lo then Int64.sub ra.lo rb.hi
    else if proves_ge env a (Add (b, Int 1)) then 1L
    else if String.equal (key a) (key b) || proves_ge env a b || env.trusted
    then 0L
    else
      fail e
        "can go below zero, which no 3D type holds and 3d.exe cannot verify; \
         guard it with a comparison that rules it out ([a >= b && ...])"
  in
  let hi = if ule rb.lo ra.hi then Int64.sub ra.hi rb.lo else lo in
  (Sub (a', b'), { ty = wider ra.ty rb.ty; lo; hi; wide = ra.wide || rb.wide })

and shift_left env e a b =
  let a', ra = int_ env a in
  let b', rb = int_ env b in
  let k = to_shift rb.hi in
  match shl_u ra.hi k with
  | _ when env.written ->
      if not (ult rb.hi (Int64.of_int (bits ra.ty))) then
        fail e "can shift by the width of its operand or more";
      (Lsl (a', b'), { (full ra.ty) with wide = ra.wide || rb.wide })
  | Some hi when k < 64 ->
      let ty = above rb.hi (wider ra.ty (smallest hi)) in
      let wide = ra.wide || rb.wide || not (ult rb.hi native_shift) in
      let r =
        if constant rb then
          let lo = Option.value ~default:0L (shl_u ra.lo k) in
          { ty; lo; hi; wide }
        else { (full ty) with wide }
      in
      (Lsl (cast ty ra a', b'), past_native r)
  | _ -> (Lsl (a', b'), past_64 env e)

and shift_right env e a b =
  let a', ra = int_ env a in
  let b', rb = int_ env b in
  if not (ult rb.hi 64L) then
    if env.trusted then (Lsr (a', b'), full ~wide:true T64)
    else fail e "can shift by the width of its operand or more"
  else if env.written && not (ult rb.hi (Int64.of_int (bits ra.ty))) then
    fail e "can shift by the width of its operand or more"
  else
    let ty = above rb.hi ra.ty in
    let wide = ra.wide || rb.wide || not (ult rb.hi native_shift) in
    let r =
      if constant rb then
        let k = to_shift rb.hi in
        { ty; lo = shr_u ra.lo k; hi = shr_u ra.hi k; wide }
      else { (full ty) with wide }
    in
    (Lsr (cast ty ra a', b'), r)

(* An addition or multiplication, computed in the narrowest type that holds its
   range: its operands are cast there when their own type does not. *)
and grow env e mk f a b =
  let a', ra = int_ env a in
  let b', rb = int_ env b in
  match f ra.hi rb.hi with
  | None -> (mk a' b', past_64 env e)
  | Some hi ->
      let lo = Option.value ~default:hi (f ra.lo rb.lo) in
      let natural = wider ra.ty rb.ty in
      if env.written && not (fits natural hi) then
        fail e "can exceed %s, which 3d.exe cannot verify" (type_name natural);
      let ty = wider natural (smallest hi) in
      let r = past_native { ty; lo; hi; wide = ra.wide || rb.wide } in
      let a', b' = if ty = natural then (a', b') else operands ty ra a' rb b' in
      (mk a' b', r)

and divide env e a b mk f =
  let a', ra = int_ env a in
  let b', rb = int_ env b in
  let zero = Int64.equal rb.lo 0L in
  if zero && not env.trusted then
    fail e "can divide by zero, which 3d.exe cannot verify";
  let ty = wider ra.ty rb.ty and wide = ra.wide || rb.wide in
  let r =
    if constant rb && not zero then
      let lo, hi = f ra rb in
      { ty; lo; hi; wide }
    else { (full ty) with wide }
  in
  (mk a' b', r)

(* F* knows nothing of a bitwise result beyond its type. *)
and bitwise env a b mk =
  let a', ra = int_ env a in
  let b', rb = int_ env b in
  (mk a' b', { (full (wider ra.ty rb.ty)) with wide = ra.wide || rb.wide })

(* [a land lnot m] clears the bits of [m] in [a]. 3D complements [m] within
   its own type, OCaml over a native int: the two agree on every [a] that type
   holds. *)
and clear env e a m mk =
  let a', ra = int_ env a in
  let m', rm = int_ env m in
  if (not (fits rm.ty ra.hi)) && not env.trusted then
    fail e
      "clears bits with lnot, which 3D complements within %s, narrower than \
       the value it masks"
      (type_name rm.ty);
  (mk a' m', { (full ra.ty) with wide = ra.wide || rm.wide })

and operand : type x. env -> x expr -> x expr =
 fun env e ->
  match e with
  | Int _ -> fst (int_ env e)
  | Ref (I, _) -> fst (int_ env e)
  | Param_ref _ -> fst (int_ env e)
  | Sizeof _ -> fst (int_ env e)
  | Sizeof_this -> fst (int_ env e)
  | Field_pos -> fst (int_ env e)
  | Add _ -> fst (int_ env e)
  | Sub _ -> fst (int_ env e)
  | Mul _ -> fst (int_ env e)
  | Div _ -> fst (int_ env e)
  | Mod _ -> fst (int_ env e)
  | Land _ -> fst (int_ env e)
  | Lor _ -> fst (int_ env e)
  | Lxor _ -> fst (int_ env e)
  | Lnot _ -> fst (int_ env e)
  | Lsl _ -> fst (int_ env e)
  | Lsr _ -> fst (int_ env e)
  | Cast _ -> fst (int_ env e)
  | If_then_else _ -> fst (int_ env e)
  | Int64 _ -> fst (int64_ env e)
  | Ref (I64, _) -> fst (int64_ env e)
  | Land64 _ -> fst (int64_ env e)
  | Lsr64 _ -> fst (int64_ env e)
  | Bool _ -> bool_ env e
  | Eq _ -> bool_ env e
  | Ne _ -> bool_ env e
  | Lt _ -> bool_ env e
  | Le _ -> bool_ env e
  | Gt _ -> bool_ env e
  | Ge _ -> bool_ env e
  | And _ -> bool_ env e
  | Or _ -> bool_ env e
  | Not _ -> bool_ env e

and bool_ env (e : bool expr) : bool expr =
  match e with
  | Bool _ -> e
  | And (a, b) ->
      let a' = bool_ env a in
      And (a', bool_ (assume env a true) b)
  | Or (a, b) ->
      let a' = bool_ env a in
      Or (a', bool_ (assume env a false) b)
  | Not a -> Not (bool_ env a)
  | Eq (a, b) -> compare env e `Eq a b
  | Ne (a, b) -> compare env e `Ne a b
  | Lt (a, b) -> compare env e `Lt a b
  | Le (a, b) -> compare env e `Le a b
  | Gt (a, b) -> compare env e `Gt a b
  | Ge (a, b) -> compare env e `Ge a b
  | Ref _ -> .

and compare : type x. env -> bool expr -> op -> x expr -> x expr -> bool expr =
 fun env e op a b ->
  let rebuild (op : op) (a : x expr) (b : x expr) : bool expr =
    match op with
    | `Eq -> Eq (a, b)
    | `Ne -> Ne (a, b)
    | `Lt -> Lt (a, b)
    | `Le -> Le (a, b)
    | `Gt -> Gt (a, b)
    | `Ge -> Ge (a, b)
  in
  let signed n =
    match List.assoc_opt n env.leaves with
    | Some (Signed w) -> Some w
    | _ -> None
  in
  let literal n op k orig =
    match signed n with
    | Some 64 -> (
        match op with (`Eq | `Ne) when k >= 0 -> Some orig | _ -> None)
    | Some w -> Some (signed_literal n w op k)
    | None -> None
  in
  let rewritten =
    match (a, b) with
    | Ref (I, n), Int k -> literal n op k e
    | Int k, Ref (I, n) -> literal n (mirror op) k e
    | Ref (I64, n), Int64 _ when signed n = Some 64 && (op = `Eq || op = `Ne) ->
        Some e
    | Int64 _, Ref (I64, n) when signed n = Some 64 && (op = `Eq || op = `Ne) ->
        Some e
    | _ -> None
  in
  match rewritten with
  | Some e -> e
  | None ->
      (match op with
      | `Lt | `Le | `Gt | `Ge -> (
          match
            List.find_opt
              (fun n -> is_float env n)
              (Lower_3d.collect_refs a @ Lower_3d.collect_refs b)
          with
          | Some n ->
              fail e
                "orders the float field %S, whose IEEE 754 bit patterns, which \
                 3D compares, do not order as its values do"
                n
          | None -> ())
      | `Eq | `Ne -> ());
      let a' = operand env a in
      rebuild op a' (operand env b)

(* {1 Structs} *)

(* An expression over a [Formal] is the applying struct's, checked there. *)
let checked env e f = if env.formals && formal env e then e else f e

let size env e =
  checked env e @@ fun e ->
  let e', r = int_ env e in
  if r.ty = T64 then
    fail e
      "has the 3D type UINT64, and 3D types a size as a 32-bit integer: keep \
       it below 2^32 and read it from fields of at most 32 bits"
  else e'

let packed env (Pack_expr e) = Pack_expr (checked env e (operand env))

(* A per-byte condition, over the byte [elt_var] alone, as its synthesised
   struct checks it. *)
let per_byte elt_var cond =
  let env = { empty with leaves = [ (elt_var, leaf Uint8) ] } in
  ignore (bool_ env cond : bool expr)

let rec typ : type a. env -> a typ -> a typ =
 fun env t ->
  match t with
  | Byte_array { size = s } -> Byte_array { size = size env s }
  | Byte_slice { size = s } -> Byte_slice { size = size env s }
  | Byte_array_where r ->
      per_byte r.elt_var r.cond;
      Byte_array_where { r with size = size env r.size }
  | Single_elem r -> Single_elem { r with size = size env r.size }
  | Zeroterm_at_most { size = s } -> Zeroterm_at_most { size = size env s }
  | Uint_var r -> Uint_var { r with size = size env r.size }
  | Repeat r -> Repeat { r with size = size env r.size }
  | Array r -> Array { r with len = size env r.len }
  | Map r -> Map { r with inner = typ env r.inner }
  | Where r -> Where { r with inner = typ env r.inner }
  | Enum r -> Enum { r with base = typ env r.base }
  | Apply { typ = t; args } ->
      Apply { typ = typ env t; args = List.map (packed env) args }
  | Optional { present; inner } ->
      let present = checked env present (bool_ env) in
      if Lower_3d.optional_region inner || not (has_wire_size_expr inner) then begin
        region inner;
        Optional { present; inner }
      end
      else Optional { present; inner = typ env inner }
  | Optional_or r -> Optional_or { r with inner = typ env r.inner }
  | t -> t

(* The body of an optional region or casetype is declared on its own, reading
   each field it names through a [UINT32] formal, under the [where] its
   declaration states: the bound {!bounded} gives it, and [a >= b] for a size
   [a - b]. 3D checks its expressions there, as written. *)
and region_check : type a. a typ -> unit =
 fun inner ->
  let bound, body = Lower_3d.region_bound inner in
  let leaves =
    List.map
      (fun (p : param) -> (p.param_name, Unsigned (full T32)))
      (Lower_3d.wrap_formals inner)
  in
  let env = { empty with leaves; written = true } in
  let env =
    match body with
    | Single_elem { size = Sub (a, b); _ } -> ge env a b
    | _ -> env
  in
  let env = match bound with Some c -> assume env c true | None -> env in
  ignore (typ env body : a typ)

and region : type a. a typ -> unit =
 fun inner ->
  try region_check inner
  with Invalid_argument m ->
    Fmt.invalid_arg
      "%s, in an optional field, whose 3D declaration reads each field it \
       names as a UINT32 bounded by what the record proves of it"
      m

(* An action's integer expression: a literal or a field as written, which 3D
   copies, any other as {!int_} checks it. *)
let value env (x : int expr) =
  match x with
  | Int _ | Ref (I, _) -> (
      match int_ env x with
      | r -> r
      | exception Invalid_argument _ -> (x, full T64))
  | _ -> int_ env x

(* The 3D type of an output, when 3D stores an integer there. *)
let output_ty : type a k. (a, k) param_handle -> ty option =
 fun p ->
  match leaf p.typ with
  | Unsigned r | Float r -> Some r.ty
  | Signed w -> Some (smallest (ones w))
  | Opaque | Formal -> None

(* The value an action assigns to the output [p], which the codec stores in a
   native int. 3D stores only a value of at most the output's type: a wider one
   is narrowed with a cast when its range fits the output, and refused
   otherwise. A literal or a field is [copied] as the codec read it, also one
   3D does not read as an integer. *)
let assigned env p (x : int expr) =
  checked env x @@ fun x ->
  let copied = match x with Int _ | Ref (I, _) -> true | _ -> false in
  match int_ env x with
  | exception Invalid_argument _ when copied -> x
  | x', r -> (
      if (not copied) && not (ule r.hi native) then
        fail x
          "assigns a value past OCaml's native int, which the codec cannot \
           store";
      match output_ty p with
      | Some ty when bits r.ty > bits ty ->
          if fits ty r.hi then Cast (cast_of_ty ty, x')
          else
            fail x
              "can exceed %s, the type of the output %S, and 3D stores only a \
               value that fits its output: bound it before the action, or \
               assign it to a wider output"
              (type_name ty) p.name
      | _ -> x')

(* An action's expressions as 3D checks them. A [var] is the range of its
   expression to the statements after it. *)
let rec stmts env = function
  | [] -> []
  | Assign (p, x) :: rest -> Assign (p, assigned env p x) :: stmts env rest
  | Field_assign (ptr, name, x) :: rest ->
      Field_assign (ptr, name, checked env x (fun x -> fst (value env x)))
      :: stmts env rest
  | Return c :: rest -> Return (checked env c (bool_ env)) :: stmts env rest
  | If (c, t, e) :: rest ->
      If
        ( checked env c (bool_ env),
          stmts (assume env c true) t,
          Option.map (stmts (assume env c false)) e )
      :: stmts env rest
  | Var (n, x) :: rest ->
      let x', r = value env x in
      Var (n, x')
      :: stmts { env with leaves = (n, Unsigned r) :: env.leaves } rest
  | ((Extern_call _ | Abort) as s) :: rest -> s :: stmts env rest

let action env = function
  | Success l -> Success (stmts env l)
  | Act l -> Act (stmts env l)

let field env (Field f) =
  match (typ env f.field_typ, Option.map (action env) f.action) with
  | field_typ, action -> Field { f with field_typ; action }
  | exception Invalid_argument m ->
      Fmt.invalid_arg "%s (in field %S)" m
        (Option.value ~default:"_" f.field_name)

(* The bounds the record proves of the fields an optional region reads,
   where its size verifies only under them: [a * 4] over a [uint8] reads [a]
   as a [UINT32], where it verifies under [a <= 255]. *)
let region_bounds env inner =
  List.concat_map
    (fun (p : param) ->
      let n : int expr = Ref (I, p.param_name) in
      match List.assoc_opt p.param_name env.leaves with
      | Some (Unsigned r) ->
          (if ult 0L r.lo && ule r.lo native then
             [ Ge (n, Int (Int64.to_int r.lo)) ]
           else [])
          @
          if ult r.hi (top T32) && ule r.hi native then
            [ Le (n, Int (Int64.to_int r.hi)) ]
          else []
      | _ -> [])
    (Lower_3d.wrap_formals inner)

(* [f] with the region of its optional under a [Where] stating those bounds,
   which the region's declaration takes as its [where] and its name records,
   when the region verifies only under them. *)
let bounded env (Field f as field) =
  match f.field_typ with
  | Optional { present; inner = Single_elem _ as inner } -> (
      match region_check inner with
      | () -> field
      | exception Invalid_argument _ -> (
          match region_bounds env inner with
          | [] -> field
          | b :: bs -> (
              let cond = List.fold_left (fun a c -> And (a, c)) b bs in
              let inner = Where { cond; inner } in
              match region_check inner with
              | () -> Field { f with field_typ = Optional { present; inner } }
              | exception Invalid_argument _ -> field)))
  | _ -> field

let refinement env ~name t cond =
  let env =
    if List.mem_assoc name env.leaves then env
    else { env with leaves = (name, leaf t) :: env.leaves }
  in
  checked env cond (bool_ env)

let condition env cond = checked env cond (bool_ env)

let envs ?(formals = []) ?where fields =
  let leaves =
    List.filter_map
      (fun (Field f) ->
        Option.map (fun n -> (n, leaf f.field_typ)) f.field_name)
      fields
    @ List.map (fun n -> (n, Formal)) formals
  in
  let outer = { empty with leaves; formals = formals <> [] } in
  let first =
    match where with Some w -> assume outer w true | None -> outer
  in
  let _, before =
    List.fold_left
      (fun (env, acc) (Field f) ->
        let acc = env :: acc in
        let own, _ = Lower_3d.extract_where_constraint f.field_typ in
        let env =
          match Lower_3d.combine_constraints f.constraint_ own with
          | Some c -> assume env c true
          | None -> env
        in
        (env, acc))
      (first, []) fields
  in
  (outer, List.rev before)

let leaf_of (Pack_typ t) = leaf t

let wide typ_of e =
  let leaves =
    List.filter_map
      (fun n -> Option.map (fun t -> (n, leaf_of t)) (typ_of n))
      (Lower_3d.collect_refs e)
  in
  match int_ { empty with leaves; trusted = true } e with
  | _, r -> r.wide
  | exception (Unknown | Invalid_argument _) -> false
