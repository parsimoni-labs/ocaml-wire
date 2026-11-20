module Plan.Sum

(* Planner.sum and its operations, as the O1 lane writes them
   (lib/ocaml/planner.ml, section Sums): an integer expression as terms, each
   an expression that is not a sum, a difference or a literal, times a
   coefficient, plus a constant. Every operation that may overflow returns
   [None], as the planner's handlers do; see defects/O1SumConst for the one
   that does not in the planner. *)

open FStar.Mul
open FStar.List.Tot
open Plan.Expr

type terms = list (expr & int)
type sum = { terms : terms; const : int }

let rec eval_terms (en:env) (ts:terms) : int =
  match ts with
  | [] -> 0
  | (e, c) :: r -> c * eval en e + eval_terms en r

let eval_sum (en:env) (s:sum) : int = eval_terms en s.terms + s.const

let rec unique (ts:terms) : bool =
  match ts with
  | [] -> true
  | (k, _) :: r -> None? (List.Tot.assoc k r) && unique r

let rec closed_terms (sc:scope) (ts:terms) : bool =
  match ts with
  | [] -> true
  | (e, _) :: r -> closed sc e && closed_terms sc r

let closed_sum (sc:scope) (s:sum) : bool = closed_terms sc s.terms

let sum_int (n:int) : sum = { terms = []; const = n }
let sum_atom (e:expr) : sum = { terms = [ (e, 1) ]; const = 0 }

(* {1 Adding one term: List.assoc_opt, then append, remove_assoc or map} *)

let rec remove (k:expr) (ts:terms) : terms =
  match ts with
  | [] -> []
  | (k', c) :: r -> if k' = k then r else (k', c) :: remove k r

let rec replace (k:expr) (c:int) (ts:terms) : terms =
  match ts with
  | [] -> []
  | (k', c') :: r -> (if k' = k then (k, c) else (k', c')) :: replace k c r

let add_term (ts:terms) (t:expr & int) : option terms =
  let (k, c) = t in
  match List.Tot.assoc k ts with
  | None -> Some (ts @ [ (k, c) ])
  | Some c' ->
    (match chk (c + c') with
     | None -> None
     | Some c'' -> if c'' = 0 then Some (remove k ts) else Some (replace k c'' ts))

let rec add_terms (acc:terms) (bs:terms) : Tot (option terms) (decreases bs) =
  match bs with
  | [] -> Some acc
  | t :: r -> (match add_term acc t with None -> None | Some acc' -> add_terms acc' r)

(* The constant is added inside the handler here; Planner.sum_add adds it
   outside, where its overflow escapes. *)
let sum_add (a b:sum) : option sum =
  match add_terms a.terms b.terms with
  | None -> None
  | Some ts -> (match chk (a.const + b.const) with None -> None | Some c -> Some { terms = ts; const = c })

let rec neg_terms (ts:terms) : option terms =
  match ts with
  | [] -> Some []
  | (e, c) :: r ->
    (match chk (0 - c), neg_terms r with Some c', Some r' -> Some ((e, c') :: r') | _ -> None)

let sum_neg (s:sum) : option sum =
  match neg_terms s.terms, chk (0 - s.const) with
  | Some ts, Some c -> Some { terms = ts; const = c }
  | _ -> None

let sum_sub (a b:sum) : option sum =
  match sum_neg b with None -> None | Some nb -> sum_add a nb

let rec sum_of (e:expr) : option sum =
  match e with
  | Lit n -> Some (sum_int n)
  | Add a b -> (match sum_of a, sum_of b with Some x, Some y -> sum_add x y | _ -> None)
  | Sub a b -> (match sum_of a, sum_of b with Some x, Some y -> sum_sub x y | _ -> None)
  | _ -> Some (sum_atom e)

let sum_size (s:sum) : nat = List.Tot.length s.terms + (if s.const = 0 then 0 else 1)

(* {1 Correctness of the operations} *)

let rec eval_append (en:env) (ts us:terms)
  : Lemma (eval_terms en (ts @ us) == eval_terms en ts + eval_terms en us)
  = match ts with [] -> () | _ :: r -> eval_append en r us

let rec eval_remove (en:env) (k:expr) (ts:terms)
  : Lemma (match List.Tot.assoc k ts with
           | Some c -> eval_terms en ts == c * eval en k + eval_terms en (remove k ts)
           | None -> True)
  = match ts with [] -> () | (k', _) :: r -> if k' = k then () else eval_remove en k r

let rec replace_is (en:env) (k:expr) (c:int) (ts:terms)
  : Lemma (requires unique ts /\ Some? (List.Tot.assoc k ts))
          (ensures eval_terms en (replace k c ts) == c * eval en k + eval_terms en (remove k ts))
  = match ts with
    | [] -> ()
    | (k', _) :: r ->
      if k' = k then begin
        (* No later term has the key, so the rest is left alone. *)
        let rec untouched (us:terms)
          : Lemma (requires None? (List.Tot.assoc k us)) (ensures replace k c us == us)
          = match us with [] -> () | _ :: u -> untouched u
        in
        untouched r
      end
      else replace_is en k c r

let rec assoc_append (k:expr) (ts us:terms)
  : Lemma (List.Tot.assoc k (ts @ us) ==
           (match List.Tot.assoc k ts with Some c -> Some c | None -> List.Tot.assoc k us))
  = match ts with [] -> () | _ :: r -> assoc_append k r us

let rec unique_append (ts:terms) (k:expr) (c:int)
  : Lemma (requires unique ts /\ None? (List.Tot.assoc k ts))
          (ensures unique (ts @ [ (k, c) ]))
  = match ts with
    | [] -> ()
    | (k', c') :: r -> assoc_append k' r [ (k, c) ]; unique_append r k c

let rec assoc_remove (j k:expr) (ts:terms)
  : Lemma (requires None? (List.Tot.assoc j ts)) (ensures None? (List.Tot.assoc j (remove k ts)))
  = match ts with [] -> () | (k', _) :: r -> if k' = k then () else assoc_remove j k r

let rec unique_remove (k:expr) (ts:terms)
  : Lemma (requires unique ts) (ensures unique (remove k ts))
  = match ts with
    | [] -> ()
    | (k', _) :: r -> if k' = k then () else (assoc_remove k' k r; unique_remove k r)

let rec assoc_replace (j k:expr) (c:int) (ts:terms)
  : Lemma (None? (List.Tot.assoc j (replace k c ts)) == None? (List.Tot.assoc j ts))
  = match ts with [] -> () | _ :: r -> assoc_replace j k c r

let rec unique_replace (k:expr) (c:int) (ts:terms)
  : Lemma (requires unique ts) (ensures unique (replace k c ts))
  = match ts with
    | [] -> ()
    | (k', _) :: r -> assoc_replace k' k c r; assoc_replace k k c r; unique_replace k c r

let add_term_ok (en:env) (ts:terms) (t:expr & int)
  : Lemma (requires unique ts)
          (ensures (match add_term ts t with
                    | Some ts' -> unique ts' /\ eval_terms en ts' == eval_terms en ts + snd t * eval en (fst t)
                    | None -> True))
  = let (k, c) = t in
    match List.Tot.assoc k ts with
    | None -> eval_append en ts [ (k, c) ]; unique_append ts k c
    | Some c' ->
      eval_remove en k ts;
      (match chk (c + c') with
       | None -> ()
       | Some c'' ->
         if c'' = 0 then unique_remove k ts
         else (replace_is en k c'' ts; unique_replace k c'' ts))

let rec add_terms_ok (en:env) (acc bs:terms)
  : Lemma (requires unique acc)
          (ensures (match add_terms acc bs with
                    | Some ts -> unique ts /\ eval_terms en ts == eval_terms en acc + eval_terms en bs
                    | None -> True))
          (decreases bs)
  = match bs with
    | [] -> ()
    | t :: r ->
      add_term_ok en acc t;
      (match add_term acc t with None -> () | Some acc' -> add_terms_ok en acc' r)

let sum_add_ok (en:env) (a b:sum)
  : Lemma (requires unique a.terms)
          (ensures (match sum_add a b with
                    | Some s -> unique s.terms /\ eval_sum en s == eval_sum en a + eval_sum en b
                    | None -> True))
  = add_terms_ok en a.terms b.terms

let rec neg_terms_ok (en:env) (ts:terms)
  : Lemma (match neg_terms ts with
           | Some us -> eval_terms en us == - (eval_terms en ts)
           | None -> True)
  = match ts with [] -> () | _ :: r -> neg_terms_ok en r

let sum_sub_ok (en:env) (a b:sum)
  : Lemma (requires unique a.terms)
          (ensures (match sum_sub a b with
                    | Some s -> unique s.terms /\ eval_sum en s == eval_sum en a - eval_sum en b
                    | None -> True))
  = neg_terms_ok en b.terms;
    match sum_neg b with None -> () | Some nb -> sum_add_ok en a nb

let rec sum_of_ok (en:env) (e:expr)
  : Lemma (match sum_of e with
           | Some s -> unique s.terms /\ eval_sum en s == eval en e
           | None -> True)
  = match e with
    | Add a b ->
      sum_of_ok en a; sum_of_ok en b;
      (match sum_of a, sum_of b with Some x, Some y -> sum_add_ok en x y | _ -> ())
    | Sub a b ->
      sum_of_ok en a; sum_of_ok en b;
      (match sum_of a, sum_of b with Some x, Some y -> sum_sub_ok en x y | _ -> ())
    | _ -> ()

(* {1 Closedness: a sum over bound variables stays one} *)

let rec closed_terms_append (sc:scope) (ts us:terms)
  : Lemma (closed_terms sc (ts @ us) == (closed_terms sc ts && closed_terms sc us))
  = match ts with [] -> () | _ :: r -> closed_terms_append sc r us

let rec closed_remove (sc:scope) (k:expr) (ts:terms)
  : Lemma (requires closed_terms sc ts) (ensures closed_terms sc (remove k ts))
  = match ts with [] -> () | (k', _) :: r -> if k' = k then () else closed_remove sc k r

let rec closed_replace (sc:scope) (k:expr) (c:int) (ts:terms)
  : Lemma (requires closed_terms sc ts) (ensures closed_terms sc (replace k c ts))
  = match ts with [] -> () | _ :: r -> closed_replace sc k c r

let rec closed_add_terms (sc:scope) (acc bs:terms)
  : Lemma (requires closed_terms sc acc /\ closed_terms sc bs)
          (ensures (match add_terms acc bs with Some ts -> closed_terms sc ts | None -> True))
          (decreases bs)
  = match bs with
    | [] -> ()
    | (k, c) :: r ->
      closed_terms_append sc acc [ (k, c) ];
      closed_remove sc k acc;
      (match chk (c + 0) with _ -> ());
      (match List.Tot.assoc k acc with
       | None -> ()
       | Some c' -> (match chk (c + c') with Some c'' -> closed_replace sc k c'' acc | None -> ()));
      (match add_term acc (k, c) with None -> () | Some acc' -> closed_add_terms sc acc' r)

let rec closed_neg (sc:scope) (ts:terms)
  : Lemma (requires closed_terms sc ts)
          (ensures (match neg_terms ts with Some us -> closed_terms sc us | None -> True))
  = match ts with [] -> () | _ :: r -> closed_neg sc r

let closed_sum_add (sc:scope) (a b:sum)
  : Lemma (requires closed_sum sc a /\ closed_sum sc b)
          (ensures (match sum_add a b with Some s -> closed_sum sc s | None -> True))
  = closed_add_terms sc a.terms b.terms

let closed_sum_sub (sc:scope) (a b:sum)
  : Lemma (requires closed_sum sc a /\ closed_sum sc b)
          (ensures (match sum_sub a b with Some s -> closed_sum sc s | None -> True))
  = closed_neg sc b.terms;
    match sum_neg b with None -> () | Some nb -> closed_sum_add sc a nb

let rec closed_sum_of (sc:scope) (e:expr)
  : Lemma (requires closed sc e)
          (ensures (match sum_of e with Some s -> closed_sum sc s | None -> True))
  = match e with
    | Add a b ->
      closed_sum_of sc a; closed_sum_of sc b;
      (match sum_of a, sum_of b with Some x, Some y -> closed_sum_add sc x y | _ -> ())
    | Sub a b ->
      closed_sum_of sc a; closed_sum_of sc b;
      (match sum_of a, sum_of b with Some x, Some y -> closed_sum_sub sc x y | _ -> ())
    | _ -> ()

let rec eval_terms_upd (sc:scope) (en:env) (v:var) (x:int) (ts:terms)
  : Lemma (requires closed_terms sc ts /\ not (sc v))
          (ensures eval_terms (upd en v x) ts == eval_terms en ts)
  = match ts with [] -> () | (e, _) :: r -> eval_upd sc en v x e; eval_terms_upd sc en v x r

let rec closed_terms_extend (sc:scope) (v:var) (ts:terms)
  : Lemma (requires closed_terms sc ts) (ensures closed_terms (extend sc v) ts)
  = match ts with [] -> () | (e, _) :: r -> closed_extend sc v e; closed_terms_extend sc v r

(* {1 Planner.sum_fails: [d < 0] as [lhs > rhs]} *)

let term_expr (c:int) (e:expr) : expr = if c = 1 then e else Mul (Lit c) e

(* The fold of Planner.sum_fails' [side]: [None] for no term. *)
let rec side_acc (acc:option expr) (ts:list (int & expr)) : Tot (option expr) (decreases ts) =
  match ts with
  | [] -> acc
  | (c, e) :: r ->
    let t = term_expr c e in
    side_acc (match acc with None -> Some t | Some a -> Some (Add a t)) r

let side (ts:list (int & expr)) : option expr = side_acc None ts

let rec eval_cs (en:env) (ts:list (int & expr)) : int =
  match ts with [] -> 0 | (c, e) :: r -> c * eval en e + eval_cs en r

let opt_eval (en:env) (o:option expr) : int = match o with None -> 0 | Some e -> eval en e

let rec side_acc_ok (en:env) (acc:option expr) (ts:list (int & expr))
  : Lemma (ensures opt_eval en (side_acc acc ts) == opt_eval en acc + eval_cs en ts) (decreases ts)
  = match ts with [] -> () | (c, e) :: r ->
    let t = term_expr c e in
    side_acc_ok en (match acc with None -> Some t | Some a -> Some (Add a t)) r

let swap (t:expr & int) : int & expr = (snd t, fst t)

let rec neg_cs (ts:list (int & expr)) : option (list (int & expr)) =
  match ts with
  | [] -> Some []
  | (c, e) :: r -> (match chk (0 - c), neg_cs r with Some c', Some r' -> Some ((c', e) :: r') | _ -> None)

let minus (s:option expr) (n:int) : option expr =
  match s with
  | None -> (match chk (0 - n) with Some m -> Some (Lit m) | None -> None)
  | Some s -> if n = 0 then Some s else Some (Sub s (Lit n))

let value (s:option expr) : expr = match s with Some e -> e | None -> Lit 0

let sum_fails (d:sum) : option (expr & expr) =
  let cs = List.Tot.map swap d.terms in
  let (pos, neg) = List.Tot.partition (fun (t:int & expr) -> fst t > 0) cs in
  match neg_cs neg with
  | None -> None
  | Some neg ->
    let lhs = side neg in
    let rhs = side pos in
    if d.const >= 0 then
      (match minus lhs d.const with Some l -> Some (l, value rhs) | None -> None)
    else
      (match chk (0 - d.const) with
       | None -> None
       | Some m -> (match minus rhs m with Some r -> Some (value lhs, r) | None -> None))

let rec eval_cs_map (en:env) (ts:terms)
  : Lemma (eval_cs en (List.Tot.map swap ts) == eval_terms en ts)
  = match ts with [] -> () | _ :: r -> eval_cs_map en r

let rec eval_cs_partition (en:env) (f:(int & expr) -> bool) (ts:list (int & expr))
  : Lemma (let (a, b) = List.Tot.partition f ts in eval_cs en ts == eval_cs en a + eval_cs en b)
  = match ts with [] -> () | _ :: r -> eval_cs_partition en f r

let rec neg_cs_ok (en:env) (ts:list (int & expr))
  : Lemma (match neg_cs ts with Some us -> eval_cs en us == - (eval_cs en ts) | None -> True)
  = match ts with [] -> () | _ :: r -> neg_cs_ok en r

let sum_fails_ok (en:env) (d:sum)
  : Lemma (match sum_fails d with
           | Some (l, r) -> (eval en l > eval en r <==> eval_sum en d < 0)
           | None -> True)
  = let cs = List.Tot.map swap d.terms in
    eval_cs_map en d.terms;
    let (pos, neg) = List.Tot.partition (fun (t:int & expr) -> fst t > 0) cs in
    eval_cs_partition en (fun (t:int & expr) -> fst t > 0) cs;
    neg_cs_ok en neg;
    match neg_cs neg with
    | None -> ()
    | Some neg' -> side_acc_ok en None neg'; side_acc_ok en None pos

let rec closed_side_acc (sc:scope) (acc:option expr) (ts:list (int & expr))
  : Lemma (requires (match acc with Some a -> closed sc a | None -> True) /\
                    (forall c e. List.Tot.memP (c, e) ts ==> closed sc e))
          (ensures (match side_acc acc ts with Some a -> closed sc a | None -> True))
          (decreases ts)
  = match ts with [] -> () | (c, e) :: r ->
    let t = term_expr c e in
    closed_side_acc sc (match acc with None -> Some t | Some a -> Some (Add a t)) r
