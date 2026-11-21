(* An obligation restated in linear integer arithmetic, for Z3 queries that
   stall on bit-blasting products of 63-bit vectors.

   Each bit-vector term is read as the signed integer it holds. A sum, a
   difference or a product by a constant is then the integer operation,
   provided it does not wrap: that is a side condition, a bit-vector
   lemma of its own (small: one operation each). A choice between two terms
   on a condition that reads this way is the same choice between their
   integers. Any other term is a leaf,
   an integer in the range of its width. A hypothesis that does not read
   this way is dropped, which only weakens the query. So when the side
   conditions hold under the path condition and the integer query is
   unsat, the obligation holds. *)

open Smt

type t = {
  defs : (string, Smt.t) Hashtbl.t;
  mutable leaves : (Smt.t * string * int) list; (* term, its name, width *)
  mutable sides : Smt.t list; (* bit-vector side conditions *)
}

let v defs =
  let h = Hashtbl.create 97 in
  List.iter (fun (n, t) -> Hashtbl.replace h n t) defs;
  { defs = h; leaves = []; sides = [] }

let pow2 k =
  (* 2^k in decimal, for k up to 130. *)
  let double d =
    let carry, digits =
      List.fold_right
        (fun c (carry, acc) ->
          let v = (2 * (Char.code c - 48)) + carry in
          (v / 10, Char.chr (48 + (v mod 10)) :: acc))
        (List.of_seq (String.to_seq d))
        (0, [])
    in
    (if carry > 0 then "1" else "") ^ String.of_seq (List.to_seq digits)
  in
  let rec go k d = if k = 0 then d else go (k - 1) (double d) in
  go k "1"

let leaf l t w =
  match List.find_opt (fun (t', _, _) -> Smt.equal t t') l.leaves with
  | Some (_, n, _) -> n
  | None ->
      let n = Fmt.str "i%d" (List.length l.leaves) in
      l.leaves <- (t, n, w) :: l.leaves;
      n

let int_literal w v =
  let s = Smt.signed w v in
  if Int64.compare s 0L < 0 then Fmt.str "(- %Lu)" (Int64.neg s)
  else Fmt.str "%Ld" s

(* That [op a b], at [w] bits, does not wrap: computed [k] bits wider, it
   is the sign extension of the [w]-bit result. *)
let no_wrap op a b k =
  let w = width a in
  let wide x = sign_extend k x in
  eq
    (sign_extend k (app op [ a; b ] (Bv w)))
    (app op [ wide a; wide b ] (Bv (w + k)))

let rec int l t =
  match t with
  | Const (w, v) -> int_literal w v
  | Name (n, Bv w) -> (
      match Hashtbl.find_opt l.defs n with
      | Some d -> int l d
      | None -> leaf l t w)
  | App ((("bvadd" | "bvsub") as op), [ a; b ], Bv _) ->
      l.sides <- no_wrap op a b 1 :: l.sides;
      Fmt.str "(%s %s %s)"
        (if op = "bvadd" then "+" else "-")
        (int l a) (int l b)
  | App ("bvmul", [ (Const _ as c); a ], Bv _)
  | App ("bvmul", [ a; (Const _ as c) ], Bv _) ->
      l.sides <- no_wrap "bvmul" c a (width a) :: l.sides;
      Fmt.str "(* %s %s)" (int l c) (int l a)
  | App (op, [ a ], Bv _) when String.starts_with ~prefix:"(_ sign_extend" op ->
      int l a
  | App (op, [ a ], Bv w)
    when String.starts_with ~prefix:"(_ extract" op && width a = w + 1 ->
      (* Truncating one bit keeps the value when it fits. *)
      l.sides <- eq (sign_extend 1 t) a :: l.sides;
      int l a
  | App ("ite", [ c; a; b ], Bv _) -> (
      (* [Int64_u.compare] answers by cases, which a comparison of its
         answer with 0 reads back. *)
      let saved = (l.leaves, l.sides) in
      match formula l c with
      | c -> Fmt.str "(ite %s %s %s)" c (int l a) (int l b)
      | exception Exit ->
          l.leaves <- fst saved;
          l.sides <- snd saved;
          leaf l t (width t))
  | _ -> ( match sort t with Bv w -> leaf l t w | _ -> raise Exit)

and formula l t =
  match t with
  | True -> "true"
  | False -> "false"
  | App ("not", [ a ], _) -> Fmt.str "(not %s)" (formula l a)
  | App ((("and" | "or" | "=>") as op), args, _) ->
      Fmt.str "(%s %s)" op (String.concat " " (List.map (formula l) args))
  | App (op, [ a; b ], Bool) -> (
      let i =
        match op with
        | "bvsle" -> Some "<="
        | "bvslt" -> Some "<"
        | "bvsge" -> Some ">="
        | "bvsgt" -> Some ">"
        | "=" -> Some "="
        | _ -> None
      in
      match (i, sort a) with
      | Some i, Bv _ -> Fmt.str "(%s %s %s)" i (int l a) (int l b)
      | _ -> raise Exit)
  | Name (n, Bool) -> (
      match Hashtbl.find_opt l.defs n with
      | Some d -> formula l d
      | None -> raise Exit)
  | _ -> raise Exit

let rec conjuncts = function
  | App ("and", args, _) -> List.concat_map conjuncts args
  | t -> [ t ]

(* [query ~defs ~pc ~cond] is the integer script whose unsat answer, with
   the side conditions (a bit-vector formula to prove under [pc]), proves
   [pc => cond]; [None] when [cond] does not read as linear arithmetic. *)
let query ~defs ~pc ~cond =
  let l = v defs in
  match formula l cond with
  | exception Exit -> None
  | goal ->
      let hyps =
        List.filter_map
          (fun c ->
            let saved = (l.leaves, l.sides) in
            match formula l c with
            | f -> Some f
            | exception Exit ->
                l.leaves <- fst saved;
                l.sides <- snd saved;
                None)
          (conjuncts pc)
      in
      let b = Buffer.create 4096 in
      let ppf = Format.formatter_of_buffer b in
      List.iter
        (fun (_, n, w) ->
          Fmt.pf ppf
            "(declare-const %s Int)@.(assert (<= (- %s) %s))@.(assert (< %s \
             %s))@."
            n
            (pow2 (w - 1))
            n n
            (pow2 (w - 1)))
        (List.rev l.leaves);
      List.iter (fun h -> Fmt.pf ppf "(assert %s)@." h) hyps;
      Fmt.pf ppf "(assert (not %s))@.(check-sat)@." goal;
      Format.pp_print_flush ppf ();
      Some (Buffer.contents b, and_ l.sides)
