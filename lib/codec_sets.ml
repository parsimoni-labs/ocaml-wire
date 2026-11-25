(* Sets of ints as sorted, disjoint intervals: the values a constraint on a
   field allows. *)

open Types
open Codec_repr

let set_inter a b =
  let rec go a b =
    match (a, b) with
    | [], _ | _, [] -> []
    | (l1, h1) :: r1, (l2, h2) :: r2 ->
        let rest = if h1 < h2 then go r1 b else go a r2 in
        let lo = max l1 l2 and hi = min h1 h2 in
        if lo <= hi then (lo, hi) :: rest else rest
  in
  go a b

let set_union a b =
  let rec merge = function
    | (l1, h1) :: (l2, h2) :: rest when h1 = max_int || l2 <= h1 + 1 ->
        merge ((l1, max h1 h2) :: rest)
    | x :: rest -> x :: merge rest
    | [] -> []
  in
  merge (List.sort compare (a @ b))

let all_ints = [ (min_int, max_int) ]

(* The values [cmp] keeps when comparing with [n]. *)
let cmp_set cmp n =
  match cmp with
  | Eq -> [ (n, n) ]
  | Ge -> [ (n, max_int) ]
  | Le -> [ (min_int, n) ]
  | Gt -> if n = max_int then [] else [ (n + 1, max_int) ]
  | Lt -> if n = min_int then [] else [ (min_int, n - 1) ]
  | Ne ->
      set_union
        (if n = min_int then [] else [ (min_int, n - 1) ])
        (if n = max_int then [] else [ (n + 1, max_int) ])

(* The values of the field [name] that [e] accepts, when [e] only compares the
   field with literals, through [&&] and [||]. *)
let rec self_set name (e : bool expr) =
  let bound : type a. cmp -> a expr -> a expr -> (int * int) list option =
   fun op x y ->
    match (x, y) with
    | Ref (I, a), Int n when a = name -> Some (cmp_set op n)
    | Int n, Ref (I, a) when a = name -> Some (cmp_set (flip op) n)
    | _ -> None
  in
  let both f a b =
    match (self_set name a, self_set name b) with
    | Some a, Some b -> Some (f a b)
    | _ -> None
  in
  match e with
  | Bool true -> Some all_ints
  | Bool false -> Some []
  | And (a, b) -> both set_inter a b
  | Or (a, b) -> both set_union a b
  | Eq (x, y) -> bound Eq x y
  | Ne (x, y) -> bound Ne x y
  | Lt (x, y) -> bound Lt x y
  | Le (x, y) -> bound Le x y
  | Gt (x, y) -> bound Gt x y
  | Ge (x, y) -> bound Ge x y
  | _ -> None
