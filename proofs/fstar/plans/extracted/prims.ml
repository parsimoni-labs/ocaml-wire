(* What the code F* extracts from the model needs of Prims, over exact
   integers. F*'s own Prims uses zarith, which this repository does not
   depend on; the model's values stay below 2^130, and it divides only in
   [wrap], which the decisions do not call. *)

module Z = struct
  (* A sign in {-1, 0, 1} and base 2^24 digits, low first, with no high
     zero digit; 0 is [{ sign = 0; mag = [] }]. *)
  type t = { sign : int; mag : int list }

  let base = 1 lsl 24
  let zero = { sign = 0; mag = [] }

  let rec trim = function
    | [] -> []
    | d :: r -> (
        match trim r with [] -> if d = 0 then [] else [ d ] | r -> d :: r)

  let make sign mag = match trim mag with [] -> zero | mag -> { sign; mag }

  (* The digits of [n] > 0, or of [-n] for [n] < 0, without negating
     [min_int]. *)
  let rec digits n =
    if n = 0 then []
    else
      let r = n mod base in
      (if r < 0 then -r else r) :: digits (n / base)

  let of_int n =
    if n = 0 then zero else make (if n < 0 then -1 else 1) (digits n)

  let one = of_int 1

  let rec cmp_mag a b =
    match (a, b) with
    | [], [] -> 0
    | _ ->
        let la = List.length a and lb = List.length b in
        if la <> lb then compare la lb
        else
          let ra = List.rev a and rb = List.rev b in
          let rec go = function
            | x :: xs, y :: ys -> if x <> y then compare x y else go (xs, ys)
            | _ -> 0
          in
          go (ra, rb)

  let add_mag a b =
    let rec go c a b =
      match (a, b) with
      | [], [] -> if c = 0 then [] else [ c ]
      | x :: xs, [] | [], x :: xs ->
          let s = x + c in
          (s mod base) :: go (s / base) xs []
      | x :: xs, y :: ys ->
          let s = x + y + c in
          (s mod base) :: go (s / base) xs ys
    in
    go 0 a b

  (* [a - b] for [a >= b]. *)
  let sub_mag a b =
    let rec go c a b =
      match (a, b) with
      | [], _ -> []
      | x :: xs, [] ->
          let s = x - c in
          if s < 0 then (s + base) :: go 1 xs [] else s :: go 0 xs []
      | x :: xs, y :: ys ->
          let s = x - y - c in
          if s < 0 then (s + base) :: go 1 xs ys else s :: go 0 xs ys
    in
    trim (go 0 a b)

  let neg x = { x with sign = -x.sign }
  let abs x = { x with sign = (if x.sign < 0 then 1 else x.sign) }

  let add x y =
    if x.sign = 0 then y
    else if y.sign = 0 then x
    else if x.sign = y.sign then make x.sign (add_mag x.mag y.mag)
    else
      let c = cmp_mag x.mag y.mag in
      if c = 0 then zero
      else if c > 0 then make x.sign (sub_mag x.mag y.mag)
      else make y.sign (sub_mag y.mag x.mag)

  let sub x y = add x (neg y)

  let mul_mag a b =
    let res = Array.make (List.length a + List.length b + 1) 0 in
    List.iteri
      (fun i x ->
        let c = ref 0 in
        List.iteri
          (fun j y ->
            let s = res.(i + j) + (x * y) + !c in
            res.(i + j) <- s mod base;
            c := s / base)
          b;
        let k = ref (i + List.length b) in
        while !c > 0 do
          let s = res.(!k) + !c in
          res.(!k) <- s mod base;
          c := s / base;
          incr k
        done)
      a;
    Array.to_list res

  let mul x y =
    if x.sign = 0 || y.sign = 0 then zero
    else make (x.sign * y.sign) (mul_mag x.mag y.mag)

  let compare x y =
    if x.sign <> y.sign then compare x.sign y.sign
    else if x.sign >= 0 then cmp_mag x.mag y.mag
    else cmp_mag y.mag x.mag

  let leq x y = compare x y <= 0
  let geq x y = compare x y >= 0
  let lt x y = compare x y < 0
  let gt x y = compare x y > 0

  (* For the small values a shift or an index takes. *)
  let to_int x =
    let m = List.fold_right (fun d acc -> (acc * base) + d) x.mag 0 in
    if x.sign < 0 then -m else m

  let shift_left x k =
    let rec zeros n = if n = 0 then [] else 0 :: zeros (n - 1) in
    mul x (make 1 (zeros (k / 24) @ [ 1 lsl (k mod 24) ]))

  (* Floor division of small values; the decisions never divide. *)
  let fits x = List.length x.mag <= 2

  let ediv x y =
    if fits x && fits y then
      let a = to_int x and b = to_int y in
      let q = a / b in
      let q = if a mod b <> 0 && a < 0 <> (b < 0) then q - 1 else q in
      of_int q
    else failwith "Prims: division of a large integer"

  let erem x y = sub x (mul (ediv x y) y)

  let rec to_string x =
    if x.sign = 0 then "0"
    else if x.sign < 0 then "-" ^ to_string (neg x)
    else
      let ten = of_int 10 in
      let rec go x acc =
        if x.sign = 0 then acc
        else
          let q = ediv_big x ten in
          let r = sub x (mul q ten) in
          go q (string_of_int (to_int r) ^ acc)
      and ediv_big x d =
        (* Long division of [x >= 0] by a small [d]. *)
        let d = to_int d in
        let rec go = function
          | [] -> ([], 0)
          | h :: t ->
              let q, r = go t in
              let cur = (r * base) + h in
              ((cur / d) :: q, cur mod d)
        in
        make 1 (fst (go x.mag))
      in
      go x ""

  let of_string s =
    let neg_, s =
      if String.length s > 0 && s.[0] = '-' then
        (true, String.sub s 1 (String.length s - 1))
      else (false, s)
    in
    let v =
      String.fold_left
        (fun acc c -> add (mul acc (of_int 10)) (of_int (Char.code c - 48)))
        zero s
    in
    if neg_ then neg v else v
end

type int = Z.t
type nat = int
type pos = int
type nonrec bool = bool
type nonrec 'a list = 'a list

let of_int = Z.of_int
let int_zero = Z.zero
let int_one = Z.one
let parse_int = Z.of_string
let to_string = Z.to_string
let op_Negation x = not x
let ( + ) = Z.add
let ( - ) = Z.sub
let ( * ) = Z.mul
let ( / ) = Z.ediv
let ( <= ) = Z.leq
let ( >= ) = Z.geq
let ( < ) = Z.lt
let ( > ) = Z.gt
let ( mod ) = Z.erem
let ( ~- ) = Z.neg
let pow2 x = Z.shift_left Z.one (Z.to_int x)
let uu___is_Cons = function [] -> false | _ :: _ -> true
let uu___is_Nil = function [] -> true | _ :: _ -> false
