(* SMT-LIB terms over bit-vectors, booleans, integers and one byte array. *)

type sort = Bool | Bv of int | Int | Bytes

type t =
  | Const of int * int64
  | True
  | False
  | Name of string * sort
  | App of string * t list * sort
  | Raw of string * sort

let equal (a : t) b = a = b
let compare (a : t) b = Stdlib.compare a b

let sort = function
  | Const (w, _) -> Bv w
  | True | False -> Bool
  | Name (_, s) | App (_, _, s) | Raw (_, s) -> s

let width t =
  match sort t with
  | Bv w -> w
  | Bool -> invalid_arg "Smt.width: a boolean"
  | Int -> invalid_arg "Smt.width: an integer"
  | Bytes -> invalid_arg "Smt.width: the byte array"

let mask w v =
  if w >= 64 then v else Int64.logand v (Int64.pred (Int64.shift_left 1L w))

(* The signed value of the [w]-bit pattern [v]. *)
let signed w v =
  if w >= 64 then v
  else
    let v = mask w v in
    if Int64.logand v (Int64.shift_left 1L (w - 1)) = 0L then v
    else Int64.sub v (Int64.shift_left 1L w)

let bv w v = Const (w, mask w v)
let int63 n = bv 63 (Int64.of_int n)
let bool b = if b then True else False
let name s sort = Name (s, sort)

let pp_sort ppf = function
  | Bool -> Fmt.string ppf "Bool"
  | Bv w -> Fmt.pf ppf "(_ BitVec %d)" w
  | Int -> Fmt.string ppf "Int"
  | Bytes -> Fmt.string ppf "(Array (_ BitVec 64) (_ BitVec 8))"

let rec pp ppf = function
  | Const (w, v) ->
      if w mod 4 = 0 then Fmt.pf ppf "#x%0*Lx" (w / 4) v
      else Fmt.pf ppf "(_ bv%Lu %d)" v w
  | True -> Fmt.string ppf "true"
  | False -> Fmt.string ppf "false"
  | Name (s, _) | Raw (s, _) -> Fmt.string ppf s
  | App (op, args, _) -> Fmt.pf ppf "(%s %a)" op Fmt.(list ~sep:sp pp) args

let to_string t = Fmt.str "%a" pp t
let app op args sort = App (op, args, sort)

(* Constant folding: only for operands that are constants, so the folded
   term means what the unfolded one does. *)

let fold2 w op a b =
  match op with
  | "bvadd" -> Some (Int64.add a b)
  | "bvsub" -> Some (Int64.sub a b)
  | "bvmul" -> Some (Int64.mul a b)
  | "bvand" -> Some (Int64.logand a b)
  | "bvor" -> Some (Int64.logor a b)
  | "bvxor" -> Some (Int64.logxor a b)
  | "bvshl" ->
      if Int64.compare b 0L >= 0 && Int64.compare b (Int64.of_int w) < 0 then
        Some (Int64.shift_left a (Int64.to_int b))
      else None
  | "bvlshr" ->
      if Int64.compare b 0L >= 0 && Int64.compare b (Int64.of_int w) < 0 then
        Some (Int64.shift_right_logical (mask w a) (Int64.to_int b))
      else None
  | "bvashr" ->
      if Int64.compare b 0L >= 0 && Int64.compare b (Int64.of_int w) < 0 then
        Some (Int64.shift_right (signed w a) (Int64.to_int b))
      else None
  | _ -> None

let binop op a b =
  let w = width a in
  if width b <> w then
    Fmt.invalid_arg "Smt.%s: operands of %d and %d bits" op w (width b);
  match (a, b) with
  | Const (_, x), Const (_, y) -> (
      match fold2 w op x y with
      | Some v -> bv w v
      | None -> app op [ a; b ] (Bv w))
  | _ -> app op [ a; b ] (Bv w)

let add = binop "bvadd"
let sub = binop "bvsub"
let mul = binop "bvmul"
let logand = binop "bvand"
let logor = binop "bvor"
let logxor = binop "bvxor"
let shl = binop "bvshl"
let lshr = binop "bvlshr"
let ashr = binop "bvashr"
let sdiv = binop "bvsdiv"
let srem = binop "bvsrem"
let neg a = sub (bv (width a) 0L) a

let cmp op a b =
  if width a <> width b then
    Fmt.invalid_arg "Smt.%s: operands of %d and %d bits" op (width a) (width b);
  match (a, b) with
  | Const (w, x), Const (_, y) -> (
      let sx = signed w x and sy = signed w y in
      match op with
      | "=" -> bool (Int64.equal x y)
      | "bvslt" -> bool (Int64.compare sx sy < 0)
      | "bvsle" -> bool (Int64.compare sx sy <= 0)
      | "bvsgt" -> bool (Int64.compare sx sy > 0)
      | "bvsge" -> bool (Int64.compare sx sy >= 0)
      | "bvult" -> bool (Int64.unsigned_compare x y < 0)
      | "bvule" -> bool (Int64.unsigned_compare x y <= 0)
      | _ -> app op [ a; b ] Bool)
  | _ -> app op [ a; b ] Bool

let eq a b =
  match (sort a, sort b) with
  | Bool, Bool -> (
      match (a, b) with
      | True, x | x, True -> x
      | False, False -> True
      | _ -> app "=" [ a; b ] Bool)
  | _ -> cmp "=" a b

let slt = cmp "bvslt"
let sle = cmp "bvsle"
let sgt = cmp "bvsgt"
let sge = cmp "bvsge"
let ult = cmp "bvult"
let ule = cmp "bvule"

let not_ = function
  | True -> False
  | False -> True
  | App ("not", [ x ], _) -> x
  | x -> app "not" [ x ] Bool

let and_ l =
  let l = List.filter (fun x -> x <> True) l in
  if List.mem False l then False
  else match l with [] -> True | [ x ] -> x | l -> app "and" l Bool

let or_ l =
  let l = List.filter (fun x -> x <> False) l in
  if List.mem True l then True
  else match l with [] -> False | [ x ] -> x | l -> app "or" l Bool

let implies a b =
  match (a, b) with
  | True, b -> b
  | False, _ | _, True -> True
  | _ -> app "=>" [ a; b ] Bool

let ite c a b =
  match c with
  | True -> a
  | False -> b
  | _ -> if a = b then a else app "ite" [ c; a; b ] (sort a)

let extract hi lo a =
  match a with
  | Const (_, v) -> bv (hi - lo + 1) (Int64.shift_right_logical v lo)
  | _ -> app (Fmt.str "(_ extract %d %d)" hi lo) [ a ] (Bv (hi - lo + 1))

let zero_extend n a =
  if n = 0 then a
  else
    match a with
    | Const (w, v) -> bv (w + n) v
    | _ -> app (Fmt.str "(_ zero_extend %d)" n) [ a ] (Bv (width a + n))

let sign_extend n a =
  if n = 0 then a
  else
    match a with
    | Const (w, v) -> bv (w + n) (signed w v)
    | _ -> app (Fmt.str "(_ sign_extend %d)" n) [ a ] (Bv (width a + n))

let concat = function
  | [] -> invalid_arg "Smt.concat: no operand"
  | [ x ] -> x
  | l -> app "concat" l (Bv (List.fold_left (fun n x -> n + width x) 0 l))

(* [resize w a] is [a] at [w] bits: truncated, or sign-extended. *)
let resize w a =
  let v = width a in
  if v = w then a
  else if v > w then extract (w - 1) 0 a
  else sign_extend (w - v) a

let select arr idx = app "select" [ arr; idx ] (Bv 8)
let bv2nat a = app "bv2nat" [ a ] Int
