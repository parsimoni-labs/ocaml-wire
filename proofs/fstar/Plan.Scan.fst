module Plan.Scan

(* A plan (Planner: prefixes, spans, where clauses, optional fields) in one
   window, what the codec makes of it ([spec]), and what the code the emitter
   writes for it does ([exec], following Emitter.walk). [exec] is
   parameterised by the O1 rewrites: [cancel] (Emitter.cancelled) and [drop]
   (Emitter.span ~implied with Emitter.checks_next). *)

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
  (* A window of [size] bytes at the position, its length bound to [n] and
     its end to [e], whose contents [body] reads; with [at_most] they may
     leave some of it unread. *)
  | Region : n:var -> e:var -> size:expr -> at_most:bool -> body:list node -> node
  (* Where a region's contents end. *)
  | Close : at_most:bool -> node

let rec nsize (x:node) : Tot Prims.pos (decreases x) =
  match x with
  | Gate _ cs d -> 2 + csize cs + osize d
  | Region _ _ _ _ b -> 2 + lsize b
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

(* Emitter.reads_nothing (f0bd4b6b): contents that load no byte, prefixes
   of no field the walk binds and tests, so a region of them may leave its
   bound to the check after it. The model's also needs no more bytes than a
   buffer holds, so that their positions stay in the native int. *)
let rec unread_nodes (b:list node) : bool =
  match b with
  | [] -> true
  | Prefix [] _ :: r -> unread_nodes r
  | Test _ :: r -> unread_nodes r
  | _ -> false

let unread (b:list node) : bool = unread_nodes b && ahead b <= max_string_length

let rec wf (sc:scope) (l:list node) : Tot bool (decreases %[lsize l; 0; 0]) =
  match l with
  | [] -> true
  | Prefix fs size :: r -> fields_ok sc fs size && wf (fields_scope sc fs) r
  | Span n e s :: r -> closed sc s && not (sc n) && not (sc e) && n <> e && wf (extend (extend sc n) e) r
  | Test c :: r -> cond_closed sc c && wf sc r
  | Gate sel cs d :: r ->
    sc sel &&
    wf_cases sc cs r &&
    (match d with None -> true | Some b -> (lsize_append b (Join :: r); wf sc (b @ (Join :: r)))) &&
    wf sc (Join :: r)
  | Join :: r -> wf sc r
  | Region n e s am b :: r ->
    closed sc s && not (sc n) && not (sc e) && n <> e &&
    (lsize_append b (Close am :: r); wf (extend (extend sc n) e) (b @ (Close am :: r)))
  | Close _ :: r -> wf sc r
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

(* The bytes of the buffer: a read of [w] bytes at an offset in it. *)
type reader = int -> w:nat -> v:nat{v < pow2 (8 * w)}

(* The fields of a prefix whose first byte is at [a] in the buffer. *)
let rec read_fields (rd:reader) (en:env) (a:int) (fs:list field) : Tot env (decreases fs) =
  match fs with [] -> en | f :: r -> read_fields rd (upd en f.id (rd (a + f.off) f.width)) a r

let holds (en:env) (c:cond) : option bool =
  match c with
  | Ge a b -> (match ev en a, ev en b with Some x, Some y -> Some (x >= y) | _ -> None)
  | Eq a b -> (match ev en a, ev en b with Some x, Some y -> Some (x = y) | _ -> None)

let region_size (b:list node) (am:bool) (r:list node)
  : Lemma (forall n e s. lsize (b @ (Close am :: r)) < lsize (Region n e s am b :: r))
  = lsize_append b (Close am :: r)

(* The codec in a window of [len] bytes at [wb] in the buffer, from offset
   [p]: a prefix needs its bytes, a span a size that does not overflow, is
   not negative and fits, a check must hold without overflow (Codec.decode
   since e4be2c6f). A region is a window of its own, [fr] the windows around
   it, each with where the code after it resumes. *)
let rec spec (rd:reader) (wb len:int) (fr:list (int & int & int)) (en:env) (p:int) (l:list node)
  : Tot outcome (decreases lsize l) =
  match l with
  | [] -> Accept en
  | Prefix fs size :: r ->
    if p + size > len then Reject else spec rd wb len fr (read_fields rd en (wb + p) fs) (p + size) r
  | Span n e s :: r ->
    (match ev en s with
     | None -> Reject
     | Some k -> if k < 0 || p + k > len then Reject else spec rd wb len fr (upd (upd en n k) e (p + k)) (p + k) r)
  | Test c :: r -> (match holds en c with Some true -> spec rd wb len fr en p r | _ -> Reject)
  | Gate sel cs d :: r ->
    chosen_size en sel cs d r;
    (match chosen en sel cs d with
     | Some b -> spec rd wb len fr en p (b @ (Join :: r))
     | None -> spec rd wb len fr en p (Join :: r))
  | Join :: r -> spec rd wb len fr en p r
  | Region n e s am b :: r ->
    region_size b am r;
    (match ev en s with
     | None -> Reject
     | Some k ->
       if k < 0 || p + k > len then Reject
       else spec rd (wb + p) k ((wb, len, p + k) :: fr) (upd (upd en n k) e (p + k)) 0 (b @ (Close am :: r)))
  | Close am :: r ->
    (match fr with
     | [] -> Reject
     | (wb', len', p') :: fr' -> if not am && p <> len then Reject else spec rd wb' len' fr' en p' r)

(* {1 What the generated code does} *)

noeq type st = {
  sc : scope;                     (* bound variables *)
  rg : ranges;                    (* Emitter's [ranges] *)
  facts : list (expr & expr);     (* passed checks [a >= b] (Planner.facts) *)
  sums : var -> option sum;       (* Emitter's [sums]: the end of a span as a sum *)
  bounded : list (expr & pos);    (* Emitter's [bounded]: span bounds a dispatch checked *)
  known : int;                    (* bytes known to be in the window after [p] *)
  p : pos;
  held : list (int & nat);        (* Emitter's [held]: the words loaded at constant
                                     offsets of the window, by offset, and their values *)
}

(* The rewrites, and the words of the int64 step scanner (057ff551) with
   [keep], what Emitter.shared_words asks of a word besides its bounds (that
   no word holds its field yet and it serves two fields). *)
noeq type opts = {
  cancel : bool;
  drop : bool;
  words : bool;
  keep : int -> int -> nat -> bool;
}

(* Planner.nonnegative_in: by range, or by a passed check [a >= b] for [a - b]. *)
let nonnegative (w:st) (e:expr) : bool =
  (match range w.rg e with Some (lo, _) -> lo >= 0 | None -> false) ||
  (match e with Sub a b -> mem (a, b) w.facts | _ -> false)

let first_span (b:list node) : option expr =
  match b with Span _ _ s :: _ | Region _ _ s _ _ :: _ -> Some s | _ -> None

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
  | Span _ _ _ :: _ | Region _ _ _ _ _ :: _ -> true
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

(* Emitter.pos_expr *)
let pos_expr (p:pos) : expr =
  match p.pv with None -> Lit p.at | Some v -> if p.at = 0 then V v else Add (V v) (Lit p.at)

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

(* {2 Regions} *)

(* The range of a region's size once its span binds it to [n]: the bytes
   its window holds at least and at most (Emitter.step's [lo, hi]). *)
let region_range (w:st) (n:var) : int & int =
  match w.rg n with Some r -> r | None -> (0, max_int)

(* A region's contents end on its size without a test when they end at a
   constant offset that the range [r] of its size pins (95f307ce). *)
let ends_exact (r:int & int) (p:pos) : bool = None? p.pv && r = (p.at, p.at)

(* Emitter.region_start: where a region at [p] starts, as an expression and
   a constant. A position one term plus a positive constant from a span's
   end is the term, and the constant goes into the region's loads
   (b633f225). *)
let region_start (w:st) (p:pos) : expr & int =
  match p.pv with
  | None -> (pos_expr p, 0)
  | Some v ->
    (match w.sums v with
     | Some s ->
       (match s.terms with
        | [ (e, c1) ] ->
          if c1 = 1 && s.const + p.at > 0 && fits w.rg e then (e, s.const + p.at) else (pos_expr p, 0)
        | _ -> (pos_expr p, 0))
     | None -> (pos_expr p, 0))

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

(* The reads of a prefix at [p] in a window of [len] bytes at [wb]: [Fault]
   outside the window, though the bytes may be in the buffer. *)
let rec reads (wb len:int) (rd:reader) (en:env) (p:int) (fs:list field) : Tot (option env) (decreases fs) =
  match fs with
  | [] -> Some en
  | f :: r ->
    if p + f.off < 0 || p + f.off + f.width > len then None
    else reads wb len rd (upd en f.id (rd (wb + p + f.off) f.width)) p r

(* {2 Shared words (057ff551)} *)

let imax (a b:int) : int = if a < b then b else a
let imin (a b:int) : int = if a < b then a else b

(* Emitter.shared_words: for each field, at [off] from the prefix's address
   and [wd] bytes wide, the word of 8 bytes from it moved back to end at
   [hi] but not before [lo], kept when it holds the field, ends by [hi] and
   [keep] takes it. *)
let rec choose (keep:int -> int -> nat -> bool) (lo hi:int) (fs:list (int & nat))
  : Tot (list int) (decreases fs) =
  match fs with
  | [] -> []
  | (off, wd) :: r ->
    let a = imax lo (imin off (hi - 8)) in
    if keep a off wd && a + 8 <= hi && a <= off && off + wd <= a + 8
    then a :: choose keep lo hi r
    else choose keep lo hi r

(* The words [ws], from the prefix's address [v0] in a window of [len] bytes
   at [wb]: each a load of 8 bytes, [None] outside the window. They are kept
   by their offset in the window. *)
let rec load_words (wb len:int) (rd:reader) (v0:int) (ws:list int)
  : Tot (option (list (int & nat))) (decreases ws) =
  match ws with
  | [] -> Some []
  | a :: r ->
    let b = v0 + a in
    if b < 0 || b + 8 > len then None
    else (match load_words wb len rd v0 r with
          | None -> None
          | Some l -> Some ((b, rd (wb + b) 8) :: l))

(* The [m] bytes [d] bytes into a word [v], as Emitter.unsigned takes them:
   Plan.Word.extract at offset 0. *)
let word_field (v:nat) (d:nat) (m:nat{1 <= m /\ m <= 7 /\ d + m <= 8}) : nat =
  let x = (v / pow2 (8 * (8 - d - m))) % pow2 63 in
  if d = 0 then x else x % pow2 (8 * m)

(* The value of the field of [m] bytes at [q] from a word that holds it. *)
let rec from_words (ws:list (int & nat)) (q:int) (m:nat{1 <= m /\ m <= 7})
  : Tot (option nat) (decreases ws) =
  match ws with
  | [] -> None
  | (b, v) :: r -> if 0 <= b && b <= q && q + m <= b + 8 then Some (word_field v (q - b) m) else from_words r q m

(* The reads of a prefix whose fields the words [ws] may hold: a field a
   word holds is taken from it, any other read on its own. *)
let rec reads_w (wb len:int) (rd:reader) (ws:list (int & nat)) (en:env) (p:int) (fs:list field)
  : Tot (option env) (decreases fs) =
  match fs with
  | [] -> Some en
  | f :: r ->
    match from_words ws (p + f.off) f.width with
    | Some v -> reads_w wb len rd ws (upd en f.id v) p r
    | None ->
      if p + f.off < 0 || p + f.off + f.width > len then None
      else reads_w wb len rd ws (upd en f.id (rd (wb + p + f.off) f.width)) p r

(* A window around the current one: its base and length, the variable that
   holds its length, where the code after the region resumes, the bounds
   checked and words held in it, and the range of the current window's
   length, the region's size, as the region opened. *)
noeq type frame = {
  fwb : int; flen : int; flim : var; fp : pos; fbounded : list (expr & pos); fheld : list (int & nat);
  frg : int & int
}

(* The current window: [len] bytes at [wb] in the buffer, the length in
   [lim] (Emitter's [base] and [limit]), and the windows around it. *)
noeq type win = { wb : int; len : int; lim : var; frames : list frame }

(* Emitter.prefix and load_words, once a check made [k] bytes from the
   prefix's address known to be in the window: the words between [lo] and
   [hi], the bytes the check covered, loaded, then the fields read, from the
   words those loaded before at constant offsets of the window, and its
   own, hold. The words held after it, and the fields' values. *)
let prefix_reads (o:opts) (rd:reader) (x:win) (w:st) (en:env) (fs:list field) (k:int)
  : option (list (int & nat) & env) =
  let v0 = match w.p.pv with None -> 0 | Some v -> en v in
  let constant = None? w.p.pv in
  let lo = if constant then 0 else w.p.at in
  let hi = extent w.p.at (if k < 0 then 0 else k) in
  let fresh =
    if o.words then choose o.keep lo hi (map (fun (f:field) -> ((w.p.at + f.off <: int), (f.width <: nat))) fs)
    else [] in
  match load_words x.wb x.len rd v0 fresh with
  | None -> None
  | Some fw ->
    let ws = (if constant then w.held else []) @ fw in
    match reads_w x.wb x.len rd ws en (posval en w.p) fs with
    | None -> None
    | Some en' -> Some (ws, en')

(* One run of the code Emitter.walk writes. *)
let rec exec (o:opts) (rd:reader) (x:win) (w:st) (en:env) (l:list node)
  : Tot outcome (decreases lsize l) =
  let len = x.len in
  let lim = x.lim in
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
        (* [lit (extent p.at need)]: the emitter adds the two at generation
           time with Planner.extent, which stops past any buffer. *)
        | Some x -> if x < extent w.p.at need then Some None else Some (Some need) in
    (match guard with
     | None -> Fault
     | Some None -> Reject
     | Some (Some k) ->
       match prefix_reads o rd x w en fs k with
       | None -> Fault
       | Some (ws, en') ->
         let w' = bind_fields w fs in
         exec o rd x ({ w' with known = k - size; p = { w.p with at = w.p.at + size };
                                held = (if None? w.p.pv then ws else w.held) }) en' r)
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
         | Some y -> exec o rd x (bind_span w implied n e s ends) (upd (upd en n k) e y) r)
  | Test c :: r ->
    (match holds en c with Some true -> exec o rd x (refine w c) en r | _ -> Reject)
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
       | Some b -> exec o rd x w1 en (b @ (Join :: r))
       | None -> exec o rd x w1 en (Join :: r))
  | Join :: r -> exec o rd x w en r
  | Region n e s am b :: r ->
    (* Emitter.step's [Region]: the bound of a span, left to the next check
       only where the contents load no byte (f0bd4b6b), then the contents in
       a window of their own. *)
    region_size b am r;
    (match span_size w en s with
     | None -> Fault
     | Some k ->
       let ov = not (fits w.rg s) in
       let nonneg = not ov && nonnegative w s in
       let ends = end_range w (size_range w s) w.p in
       let implied = o.drop && unread b && checks_next r && nonneg && Some? ends in
       match (if implied || mem (s, w.p) w.bounded then Some false
              else outside o w lim en s ov nonneg k) with
       | None -> Fault
       | Some true -> Reject
       | Some false ->
         match pos_plus en w.p k with
         | None -> Fault
         | Some y ->
           let w1 = bind_span w implied n e s ends in
           let f = { fwb = x.wb; flen = len; flim = lim; fp = w1.p; fbounded = w1.bounded; fheld = w1.held;
                     frg = region_range w1 n } in
           let x' = { wb = x.wb + posval en w.p; len = k; lim = n; frames = f :: x.frames } in
           (* The bounds checked in the window around do not hold in this
              one; the window holds the bytes its size's range proves. *)
           let lo = fst (region_range w1 n) in
           let w' = { w1 with p = { pv = None; at = 0 }; known = lo; bounded = []; held = [] } in
           exec o rd x' w' (upd (upd en n k) e y) (b @ (Close am :: r)))
  | Close am :: r ->
    (match x.frames with
     | [] -> Reject
     | f :: fs ->
       (* [if cp <> n then fail]: a region read exactly, unless its size's
          range pins where its contents end. *)
       match (if am || ends_exact f.frg w.p then Some false else
              match ev en (pos_expr w.p) with Some c -> Some (c <> len) | None -> None) with
       | None -> Fault
       | Some true -> Reject
       | Some false ->
         exec o rd ({ wb = f.fwb; len = f.flen; lim = f.flim; frames = fs })
           ({ w with known = 0; p = f.fp; bounded = f.fbounded; held = f.fheld }) en r)
