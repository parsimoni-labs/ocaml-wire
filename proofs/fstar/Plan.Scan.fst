module Plan.Scan

(* A plan (Planner: prefixes, spans, where clauses, optional fields) in one
   window, what the codec makes of it ([spec]), and what the code the emitter
   writes for it does ([exec], following Emitter.walk). [exec] is
   parameterised by the O1 rewrites: [cancel] (Emitter.cancelled) and [drop]
   (Emitter.span ~implied with Emitter.checks_next). *)

open FStar.Mul
open FStar.List.Tot
open Plan.Expr
open Plan.Sum

(* A position: a variable holding an offset in the window, plus a constant
   (Emitter's [p], [pos_expr]). *)
type pos = { pv : option var; at : nat }

let posval (en:env) (p:pos) : int = (match p.pv with None -> 0 | Some v -> en v) + p.at

let pos_closed (sc:scope) (p:pos) : bool = match p.pv with None -> true | Some v -> sc v

(* A field of a prefix: its variable, its constant offset in the prefix and
   its width in bytes. *)
type field = { id : var; off : nat; width : w:nat{1 <= w /\ w <= 7} }

(* A where clause or a field constraint: [Ge a b] is [a >= b]. *)
type cond =
  | Ge : expr -> expr -> cond
  | Eq : expr -> expr -> cond

type node =
  | Prefix : fields:list field -> size:nat -> node
  | Span : n:var -> e:var -> size:expr -> node
  | Test : cond -> node
  (* A run of optional fields: the case whose tag is the value of [sel], or
     [dflt]; with no default, a value matching no tag runs no case. *)
  | Gate : sel:var -> cases:list (int & list node) -> dflt:option (list node) -> node
  (* Where the code after a run of optional fields starts. *)
  | Join : node

let rec nsize (x:node) : Tot Prims.pos (decreases x) =
  match x with
  | Gate _ cs d -> 2 + csize cs + osize d
  | _ -> 1
and lsize (l:list node) : Tot nat (decreases l) =
  match l with [] -> 0 | x :: r -> nsize x + lsize r
and csize (cs:list (int & list node)) : Tot nat (decreases cs) =
  match cs with [] -> 0 | (_, b) :: r -> lsize b + csize r
and osize (d:option (list node)) : Tot nat (decreases d) =
  match d with None -> 0 | Some b -> lsize b

let rec lsize_append (a b:list node)
  : Lemma (lsize (a @ b) == lsize a + lsize b)
  = match a with [] -> () | _ :: r -> lsize_append r b

let rec pick (t:int) (cs:list (int & list node)) : option (list node) =
  match cs with [] -> None | (k, b) :: r -> if k = t then Some b else pick t r

let rec pick_size (t:int) (cs:list (int & list node))
  : Lemma (match pick t cs with Some b -> lsize b <= csize cs | None -> True)
  = match cs with [] -> () | (k, _) :: r -> if k = t then () else pick_size t r

let chosen (en:env) (sel:var) (cs:list (int & list node)) (d:option (list node))
  : option (list node) =
  match pick (en sel) cs with Some b -> Some b | None -> d

let chosen_size (en:env) (sel:var) (cs:list (int & list node)) (d:option (list node)) (r:list node)
  : Lemma (match chosen en sel cs d with
           | Some b -> lsize (b @ (Join :: r)) < lsize (Gate sel cs d :: r)
           | None -> True)
  = pick_size (en sel) cs;
    match chosen en sel cs d with Some b -> lsize_append b (Join :: r) | None -> ()

(* {1 Well-formed plans}

   The planner's variables are unique: each is bound once on a path, and an
   expression reads only variables bound before it. *)

let cond_closed (sc:scope) (c:cond) : bool =
  match c with Ge a b | Eq a b -> closed sc a && closed sc b

let rec fields_ok (sc:scope) (fs:list field) (size:nat) : Tot bool (decreases fs) =
  match fs with
  | [] -> true
  | f :: r -> not (sc f.id) && f.off + f.width <= size && fields_ok (extend sc f.id) r size

let rec fields_scope (sc:scope) (fs:list field) : Tot scope (decreases fs) =
  match fs with [] -> sc | f :: r -> fields_scope (extend sc f.id) r

(* Emitter.ahead: the bytes a run of prefixes and tests needs. *)
let rec ahead (l:list node) : nat =
  match l with
  | Prefix _ size :: r -> size + ahead r
  | Test _ :: r -> ahead r
  | _ -> 0

(* A run of prefixes and tests holds fixed fields of one record, which
   Codec_seal.add_min_size keeps below Sys.max_string_length. *)
let rec wf (sc:scope) (l:list node) : Tot bool (decreases %[lsize l; 0; 0]) =
  match l with
  | [] -> true
  | Prefix fs size :: r ->
    ahead l <= max_string_length && fields_ok sc fs size && wf (fields_scope sc fs) r
  | Span n e s :: r -> closed sc s && not (sc n) && not (sc e) && n <> e && wf (extend (extend sc n) e) r
  | Test c :: r -> cond_closed sc c && wf sc r
  | Gate sel cs d :: r ->
    sc sel &&
    wf_cases sc cs r &&
    (match d with None -> true | Some b -> (lsize_append b (Join :: r); wf sc (b @ (Join :: r)))) &&
    wf sc (Join :: r)
  | Join :: r -> wf sc r
and wf_cases (sc:scope) (cs:list (int & list node)) (r:list node)
  : Tot bool (decreases %[(lsize r + csize cs + 1 <: nat); 1; cs]) =
  match cs with
  | [] -> true
  | (_, b) :: cs' -> lsize_append b (Join :: r); wf sc (b @ (Join :: r)) && wf_cases sc cs' r

let rec wf_pick (sc:scope) (t:int) (cs:list (int & list node)) (r:list node)
  : Lemma (requires wf_cases sc cs r)
          (ensures (match pick t cs with Some b -> wf sc (b @ (Join :: r)) | None -> True))
  = match cs with [] -> () | (k, _) :: cs' -> if k = t then () else wf_pick sc t cs' r

(* {1 What the codec accepts} *)

noeq type outcome =
  | Reject
  | Accept : env -> outcome
  | Fault (* a read outside the window, or plain arithmetic that wraps *)

(* The bytes of the window: a read of [w] bytes at an offset. *)
type reader = int -> w:nat -> v:nat{v < pow2 (8 * w)}

let rec read_fields (rd:reader) (en:env) (p:int) (fs:list field) : Tot env (decreases fs) =
  match fs with [] -> en | f :: r -> read_fields rd (upd en f.id (rd (p + f.off) f.width)) p r

let holds (en:env) (c:cond) : option bool =
  match c with
  | Ge a b -> (match ev en a, ev en b with Some x, Some y -> Some (x >= y) | _ -> None)
  | Eq a b -> (match ev en a, ev en b with Some x, Some y -> Some (x = y) | _ -> None)

(* The codec on [len] bytes, from offset [p]: a prefix needs its bytes, a
   span a size that does not overflow, is not negative and fits, a check
   must hold without overflow (Codec.decode since e4be2c6f). *)
let rec spec (len:nat) (rd:reader) (en:env) (p:int) (l:list node)
  : Tot outcome (decreases lsize l) =
  match l with
  | [] -> Accept en
  | Prefix fs size :: r ->
    if p + size > len then Reject else spec len rd (read_fields rd en p fs) (p + size) r
  | Span n e s :: r ->
    (match ev en s with
     | None -> Reject
     | Some k -> if k < 0 || p + k > len then Reject else spec len rd (upd (upd en n k) e (p + k)) (p + k) r)
  | Test c :: r -> (match holds en c with Some true -> spec len rd en p r | _ -> Reject)
  | Gate sel cs d :: r ->
    chosen_size en sel cs d r;
    (match chosen en sel cs d with
     | Some b -> spec len rd en p (b @ (Join :: r))
     | None -> spec len rd en p (Join :: r))
  | Join :: r -> spec len rd en p r

(* {1 What the generated code does} *)

noeq type st = {
  sc : scope;                     (* bound variables *)
  rg : ranges;                    (* Emitter's [ranges] *)
  facts : list (expr & expr);     (* passed checks [a >= b] (Planner.facts) *)
  sums : var -> option sum;       (* Emitter's [sums]: the end of a span as a sum *)
  bounded : list (expr & pos);    (* Emitter's [bounded]: span bounds a dispatch checked *)
  known : int;                    (* bytes known to be in the window after [p] *)
  p : pos;
}

type opts = { cancel : bool; drop : bool }

(* Planner.nonnegative_in: by range, or by a passed check [a >= b] for [a - b]. *)
let nonnegative (w:st) (e:expr) : bool =
  (match range w.rg e with Some (lo, _) -> lo >= 0 | None -> false) ||
  (match e with Sub a b -> mem (a, b) w.facts | _ -> false)

let first_span (b:list node) : option expr =
  match b with Span _ _ s :: _ -> Some s | _ -> None

let rec all_first (s:expr) (cs:list (int & list node)) : bool =
  match cs with [] -> true | (_, b) :: r -> first_span b = Some s && all_first s r

(* Emitter.hoisted: the size of the span every case starts with, when a case
   always runs. *)
let hoisted (cs:list (int & list node)) (d:option (list node)) : option expr =
  match d with
  | None -> None
  | Some b -> (match first_span b with Some s -> if all_first s cs then Some s else None | None -> None)

(* Emitter.checks_next *)
let checks_next (l:list node) : bool =
  match l with
  | Prefix _ _ :: _ -> ahead l > 0
  | Span _ _ _ :: _ -> true
  | Gate _ cs d :: _ -> Some? (hoisted cs d)
  | _ -> false

let add_range (a b:int & int) : option (int & int) =
  let (l1, h1) = a in let (l2, h2) = b in both (l1 + l2) (h1 + h2)

(* Emitter.end_range *)
let end_range (w:st) (nr:int & int) (p:pos) : option (int & int) =
  let pr = match p.pv with
    | None -> both p.at p.at
    | Some v -> add_range (match w.rg v with Some r -> r | None -> (min_int, max_int)) (p.at, p.at)
  in
  match pr with None -> None | Some r -> add_range r nr

(* Emitter.pos_sum *)
let pos_sum (w:st) (expand:bool) (p:pos) : option sum =
  let var = match p.pv with
    | None -> sum_int 0
    | Some v -> (match w.sums v with Some s -> if expand then s else sum_atom (V v) | None -> sum_atom (V v))
  in
  sum_add var (sum_int p.at)

(* Emitter.cancelled: [Some (lhs, rhs)], the test [lhs > rhs]. *)
let cancelled (w:st) (lim:var) (s:expr) (p:pos) : option (expr & expr) =
  let used (expand:bool) =
    match pos_sum w expand p, sum_of s with
    | Some ps, Some n -> sum_add ps n
    | _ -> None in
  let room (u:option sum) = match u with None -> None | Some u -> sum_sub (sum_atom (V lim)) u in
  let plain =
    (match sum_of s with None -> 0 | Some x -> sum_size x) +
    (if None? p.pv then 0 else 1) + (if p.at = 0 then 0 else 1) in
  let best =
    match room (used false), room (used true) with
    | Some a, Some b -> if sum_size b < sum_size a then Some b else Some a
    | Some a, None -> Some a
    | None, b -> b in
  match best with
  | Some d ->
    if sum_size d - 1 < plain then
      (match sum_fails d with
       | Some (l, r) -> if fits w.rg l && fits w.rg r then Some (l, r) else None
       | None -> None)
    else None
  | None -> None

(* [limit - p] as Emitter.outside writes it, plain. *)
let room_expr (lim:var) (p:pos) : expr =
  let r = match p.pv with None -> V lim | Some v -> Sub (V lim) (V v) in
  if p.at = 0 then r else Sub r (Lit p.at)

(* Emitter.outside: [Some true] rejects, [None] is a wrapped operation. *)
let outside (o:opts) (w:st) (lim:var) (en:env) (s:expr) (ov nonneg:bool) (n:int) : option bool =
  if not nonneg && n < 0 then Some true
  else
    match (if o.cancel && not ov then cancelled w lim s w.p else None) with
    | Some (l, r) -> (match ev en l, ev en r with Some x, Some y -> Some (x > y) | _ -> None)
    | None -> (match ev en (room_expr lim w.p) with Some room -> Some (n > room) | None -> None)

(* The size of a span as the code binds it: checked, [-1] on overflow, when
   the ranges do not keep it in the native int. *)
let span_size (w:st) (en:env) (s:expr) : option int =
  if fits w.rg s then ev en s else Some (match ev en s with Some k -> k | None -> -1)

let size_range (w:st) (s:expr) : int & int =
  let (lo, hi) = match range w.rg s with Some r -> r | None -> (min_int, max_int) in
  ((if lo < 0 then 0 else lo), (if hi < 0 then 0 else hi))

let pos_plus (en:env) (p:pos) (k:int) : option int = chk (posval en p + k)

(* Emitter.bind_span *)
let bind_span (w:st) (implied:bool) (n e:var) (s:expr) (ends:option (int & int)) : st =
  let nr = size_range w s in
  let er = match ends with
    | Some r -> if implied then r else ((if fst r < 0 then 0 else fst r), snd r)
    | None -> (0, max_int) in
  let sm = if not (fits w.rg s) then None
           else match pos_sum w true w.p, sum_of s with Some ps, Some x -> sum_add ps x | _ -> None in
  { w with
    sc = extend (extend w.sc n) e;
    rg = (fun v -> if v = e then Some er else if v = n then Some nr else w.rg v);
    sums = (fun v -> if v = e then sm else w.sums v);
    known = 0;
    p = { pv = Some e; at = 0 } }

let bind_field (w:st) (f:field) : st =
  { w with sc = extend w.sc f.id;
           rg = (fun v -> if v = f.id then Some (0, pow2 (8 * f.width) - 1) else w.rg v) }

let rec bind_fields (w:st) (fs:list field) : Tot st (decreases fs) =
  match fs with [] -> w | f :: r -> bind_fields (bind_field w f) r

(* Planner.refined for the checks the model reads: a lower or upper bound on
   a variable, and every [a >= b] as a fact. *)
let refine (w:st) (c:cond) : st =
  match c with
  | Ge a b ->
    let rg = match a, b with
      | V x, Lit c -> (match w.rg x with
                      | Some (lo, hi) -> (fun v -> if v = x then Some ((if lo < c then c else lo), hi) else w.rg v)
                      | None -> w.rg)
      | Lit c, V x -> (match w.rg x with
                      | Some (lo, hi) -> (fun v -> if v = x then Some (lo, (if hi > c then c else hi)) else w.rg v)
                      | None -> w.rg)
      | _ -> w.rg in
    { w with rg = rg; facts = (a, b) :: w.facts }
  | Eq _ _ -> w

(* The reads of a prefix: [Fault] outside the window. *)
let rec reads (len:nat) (rd:reader) (en:env) (p:int) (fs:list field) : Tot (option env) (decreases fs) =
  match fs with
  | [] -> Some en
  | f :: r ->
    if p + f.off < 0 || p + f.off + f.width > len then None
    else reads len rd (upd en f.id (rd (p + f.off) f.width)) p r

(* One run of the code Emitter.walk writes, in a window of [len] bytes whose
   length the variable [lim] holds. *)
let rec exec (o:opts) (len:nat) (lim:var) (rd:reader) (w:st) (en:env) (l:list node)
  : Tot outcome (decreases lsize l) =
  match l with
  | [] -> Accept en
  | Prefix fs size :: r ->
    let need = ahead l in
    let guard : option (option int) =
      (* [None]: a wrapped room; [Some None]: rejected; [Some (Some k)]: known. *)
      if w.known >= need then Some (Some w.known)
      else
        let room = match w.p.pv with None -> V lim | Some v -> Sub (V lim) (V v) in
        match ev en room with
        | None -> None
        (* [lit (p.at + need)]: the emitter adds the two at generation time,
           with OCaml's wrapping [+]. *)
        | Some x -> if x < wrap (w.p.at + need) then Some None else Some (Some need) in
    (match guard with
     | None -> Fault
     | Some None -> Reject
     | Some (Some k) ->
       match reads len rd en (posval en w.p) fs with
       | None -> Fault
       | Some en' ->
         let w' = bind_fields w fs in
         exec o len lim rd ({ w' with known = k - size; p = { w.p with at = w.p.at + size } }) en' r)
  | Span n e s :: r ->
    (match span_size w en s with
     | None -> Fault
     | Some k ->
       let ov = not (fits w.rg s) in
       let nonneg = not ov && nonnegative w s in
       let ends = end_range w (size_range w s) w.p in
       let implied = o.drop && checks_next r && nonneg && Some? ends in
       let skip = implied || mem (s, w.p) w.bounded in
       match (if skip then Some false else outside o w lim en s ov nonneg k) with
       | None -> Fault
       | Some true -> Reject
       | Some false ->
         match pos_plus en w.p k with
         | None -> Fault
         | Some x -> exec o len lim rd (bind_span w implied n e s ends) (upd (upd en n k) e x) r)
  | Test c :: r ->
    (match holds en c with Some true -> exec o len lim rd (refine w c) en r | _ -> Reject)
  | Gate sel cs d :: r ->
    let hoist : option (option st) =
      (* Emitter.hoist_bound: [None] a fault, [Some None] rejected. *)
      match hoisted cs d with
      | Some s ->
        if mem (s, w.p) w.bounded then Some (Some w)
        else
          (match span_size w en s with
           | None -> None
           | Some k ->
             let ov = not (fits w.rg s) in
             let nonneg = not ov && nonnegative w s in
             match outside o w lim en s ov nonneg k with
             | None -> None
             | Some true -> Some None
             | Some false ->
               match pos_plus en w.p k with
               | None -> None
               | Some _ -> Some (Some ({ w with bounded = (s, w.p) :: w.bounded })))
      | None -> Some (Some w) in
    chosen_size en sel cs d r;
    (match hoist with
     | None -> Fault
     | Some None -> Reject
     | Some (Some w1) ->
       match chosen en sel cs d with
       | Some b -> exec o len lim rd w1 en (b @ (Join :: r))
       | None -> exec o len lim rd w1 en (Join :: r))
  | Join :: r -> exec o len lim rd w en r
