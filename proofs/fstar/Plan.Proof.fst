module Plan.Proof

(* The code the emitter writes runs as the codec does, with or without the O1
   rewrites, and never reads outside its window nor wraps an operation. *)

open FStar.Mul
open FStar.List.Tot
open Plan.Expr
open Plan.Sum
open Plan.Scan

#set-options "--fuel 1 --ifuel 1 --z3rlimit 40"

(* {1 The invariant of a walk} *)

let fact_holds (en:env) (a b:expr) : prop =
  match ev en a, ev en b with Some x, Some y -> x >= y | _ -> False

let rec facts_ok (sc:scope) (en:env) (fs:list (expr & expr)) : prop =
  match fs with
  | [] -> True
  | (a, b) :: r -> closed sc a /\ closed sc b /\ fact_holds en a b /\ facts_ok sc en r

let bound_holds (len:int) (en:env) (s:expr) (q:pos) : prop =
  match ev en s with Some k -> k >= 0 /\ posval en q + k <= len | None -> False

let rec bounded_ok (sc:scope) (len:int) (en:env) (bs:list (expr & pos)) : prop =
  match bs with
  | [] -> True
  | (s, q) :: r -> closed sc s /\ pos_closed sc q /\ bound_holds len en s q /\ bounded_ok sc len en r

let sums_ok (w:st) (en:env) : prop =
  forall (v:var). match w.sums v with
             | Some s -> w.sc v /\ closed_sum w.sc s /\ unique s.terms /\ eval_sum en s == en v
             | None -> True

let pos_ok (w:st) (en:env) : prop =
  pos_closed w.sc w.p /\
  (match w.p.pv with Some v -> 0 <= en v /\ en v <= max_int | None -> True)

let inv (len:nat) (lim:var) (w:st) (en:env) (pending:bool) : prop =
  len <= max_string_length /\ len <= max_int /\
  w.sc lim /\ en lim == len /\
  sound_ranges w.rg en /\
  facts_ok w.sc en w.facts /\
  sums_ok w en /\
  bounded_ok w.sc len en w.bounded /\
  pos_ok w en /\
  w.known >= 0 /\
  (pending ==> (w.known == 0 /\ w.p.at == 0 /\ Some? w.p.pv)) /\
  (not pending ==> posval en w.p + w.known <= len)

(* {1 Binding a fresh variable} *)

let rec facts_upd (sc:scope) (en:env) (v:var) (x:int) (fs:list (expr & expr))
  : Lemma (requires facts_ok sc en fs /\ not (sc v))
          (ensures facts_ok (extend sc v) (upd en v x) fs)
  = match fs with
    | [] -> ()
    | (a, b) :: r ->
      eval_upd sc en v x a; eval_upd sc en v x b;
      closed_extend sc v a; closed_extend sc v b;
      facts_upd sc en v x r

let rec bounded_upd (sc:scope) (len:int) (en:env) (v:var) (x:int) (bs:list (expr & pos))
  : Lemma (requires bounded_ok sc len en bs /\ not (sc v))
          (ensures bounded_ok (extend sc v) len (upd en v x) bs)
  = match bs with
    | [] -> ()
    | (s, q) :: r ->
      eval_upd sc en v x s; closed_extend sc v s;
      bounded_upd sc len en v x r

let rec facts_mem (sc:scope) (en:env) (fs:list (expr & expr)) (a b:expr)
  : Lemma (requires facts_ok sc en fs /\ mem (a, b) fs) (ensures fact_holds en a b)
  = match fs with [] -> () | (a', b') :: r -> if (a', b') = (a, b) then () else facts_mem sc en r a b

let rec bounded_mem (sc:scope) (len:int) (en:env) (bs:list (expr & pos)) (s:expr) (q:pos)
  : Lemma (requires bounded_ok sc len en bs /\ mem (s, q) bs) (ensures bound_holds len en s q)
  = match bs with [] -> () | (s', q') :: r -> if (s', q') = (s, q) then () else bounded_mem sc len en r s q

let sums_upd (w:st) (en:env) (v:var) (x:int)
  : Lemma (requires sums_ok w en /\ not (w.sc v))
          (ensures (forall (u:var). match w.sums u with
                      | Some s -> extend w.sc v u /\ closed_sum (extend w.sc v) s /\ unique s.terms /\
                                 eval_sum (upd en v x) s == upd en v x u
                      | None -> True))
  = introduce forall (u:var). (match w.sums u with
                      | Some s -> extend w.sc v u /\ closed_sum (extend w.sc v) s /\ unique s.terms /\
                                 eval_sum (upd en v x) s == upd en v x u
                      | None -> True)
    with (match w.sums u with
          | Some s -> eval_terms_upd w.sc en v x s.terms; closed_terms_extend w.sc v s.terms
          | None -> ())

(* {1 Prefixes} *)

let pow2_width (wd:nat{1 <= wd /\ wd <= 7}) : Lemma (pow2 (8 * wd) - 1 <= max_int) =
  FStar.Math.Lemmas.pow2_le_compat 62 (8 * wd)

(* Binding the fields of a prefix keeps the invariant's parts that do not
   depend on the position. *)
let rec bind_fields_ok (len:nat) (lim:var) (rd:reader) (w:st) (en:env) (p:int) (fs:list field) (size:nat)
  : Lemma (requires fields_ok w.sc fs size /\ len <= max_int /\
                    w.sc lim /\ en lim == len /\
                    sound_ranges w.rg en /\ facts_ok w.sc en w.facts /\ sums_ok w en /\
                    bounded_ok w.sc len en w.bounded)
          (ensures (let w' = bind_fields w fs in
                    let en' = read_fields rd en p fs in
                    w'.sc == fields_scope w.sc fs /\
                    w'.sc lim /\ en' lim == len /\
                    sound_ranges w'.rg en' /\ facts_ok w'.sc en' w'.facts /\ sums_ok w' en' /\
                    bounded_ok w'.sc len en' w'.bounded /\
                    w'.facts == w.facts /\ w'.bounded == w.bounded /\ w'.sums == w.sums /\
                    w'.known == w.known /\ w'.p == w.p /\
                    (forall (u:var). w.sc u ==> (w'.sc u /\ en' u == en u))))
          (decreases fs)
  = match fs with
    | [] -> ()
    | f :: r ->
      let x = rd (p + f.off) f.width in
      pow2_width f.width;
      facts_upd w.sc en f.id x w.facts;
      bounded_upd w.sc len en f.id x w.bounded;
      sums_upd w en f.id x;
      let w1 = bind_field w f in
      bind_fields_ok len lim rd w1 (upd en f.id x) p r size

let rec reads_ok (len:nat) (rd:reader) (en:env) (p:int) (fs:list field) (sc:scope) (size:nat)
  : Lemma (requires fields_ok sc fs size /\ 0 <= p /\ p + size <= len)
          (ensures reads len rd en p fs == Some (read_fields rd en p fs))
          (decreases fs)
  = match fs with [] -> () | f :: r -> reads_ok len rd (upd en f.id (rd (p + f.off) f.width)) p r (extend sc f.id) size

let rec ahead_reject (len:nat) (rd:reader) (en:env) (p:int) (l:list node)
  : Lemma (requires p <= len /\ p + ahead l > len)
          (ensures spec len rd en p l == Reject)
          (decreases l)
  = match l with
    | Prefix fs size :: r -> if p + size > len then () else ahead_reject len rd (read_fields rd en p fs) (p + size) r
    | Test c :: r -> (match holds en c with Some true -> ahead_reject len rd en p r | _ -> ())
    | _ -> ()

(* {1 Spans} *)

#push-options "--fuel 2"
let pos_sum_ok (w:st) (en:env) (expand:bool) (p:pos)
  : Lemma (requires sums_ok w en /\ pos_closed w.sc p)
          (ensures (match pos_sum w expand p with
                    | Some s -> unique s.terms /\ closed_sum w.sc s /\ eval_sum en s == posval en p
                    | None -> True))
  = let var = match p.pv with
      | None -> sum_int 0
      | Some v -> (match w.sums v with Some s -> if expand then s else sum_atom (V v) | None -> sum_atom (V v)) in
    (match p.pv with
     | Some v ->
       assert (match w.sums v with
               | Some s -> w.sc v /\ closed_sum w.sc s /\ unique s.terms /\ eval_sum en s == en v
               | None -> True)
     | None -> ());
    assert (unique var.terms);
    assert (closed_sum w.sc var);
    sum_add_ok en var (sum_int p.at);
    closed_sum_add w.sc var (sum_int p.at)
#pop-options

let used_ok (w:st) (en:env) (expand:bool) (p:pos) (s:expr)
  : Lemma (requires sums_ok w en /\ pos_closed w.sc p)
          (ensures (match pos_sum w expand p, sum_of s with
                    | Some ps, Some n ->
                      (match sum_add ps n with
                       | Some u -> unique u.terms /\ eval_sum en u == posval en p + eval en s
                       | None -> True)
                    | _ -> True))
  = pos_sum_ok w en expand p;
    sum_of_ok en s;
    match pos_sum w expand p, sum_of s with
    | Some ps, Some n -> sum_add_ok en ps n
    | _ -> ()

#push-options "--fuel 2"
let room_ok (en:env) (lim:var) (u:sum)
  : Lemma (requires unique u.terms)
          (ensures (match sum_sub (sum_atom (V lim)) u with
                    | Some d -> eval_sum en d == en lim - eval_sum en u
                    | None -> True))
  = sum_sub_ok en (sum_atom (V lim)) u
#pop-options

(* Emitter.cancelled tests what the plain bound tests. *)
let cancelled_ok (w:st) (en:env) (lim:var) (s:expr)
  : Lemma (requires sums_ok w en /\ pos_closed w.sc w.p /\ sound_ranges w.rg en)
          (ensures (match cancelled w lim s w.p with
                    | Some (l, r) ->
                      ev en l == Some (eval en l) /\ ev en r == Some (eval en r) /\
                      (eval en l > eval en r <==> posval en w.p + eval en s > en lim)
                    | None -> True))
  = used_ok w en false w.p s;
    used_ok w en true w.p s;
    let used (expand:bool) =
      match pos_sum w expand w.p, sum_of s with
      | Some ps, Some n -> sum_add ps n
      | _ -> None in
    (match used false with Some u -> room_ok en lim u | None -> ());
    (match used true with Some u -> room_ok en lim u | None -> ());
    match cancelled w lim s w.p with
    | Some (l, r) ->
      fits_ev w.rg en l; fits_ev w.rg en r;
      let room (u:option sum) = match u with None -> None | Some u -> sum_sub (sum_atom (V lim)) u in
      (match room (used false) with Some d -> sum_fails_ok en d | None -> ());
      (match room (used true) with Some d -> sum_fails_ok en d | None -> ())
    | None -> ()

let nonnegative_ok (w:st) (en:env) (s:expr)
  : Lemma (requires sound_ranges w.rg en /\ facts_ok w.sc en w.facts /\ fits w.rg s /\ nonnegative w s)
          (ensures ev en s == Some (eval en s) /\ eval en s >= 0)
  = range_sound w.rg en s;
    match s with
    | Sub a b ->
      if mem (a, b) w.facts then (facts_mem w.sc en w.facts a b; ev_eval en a; ev_eval en b) else ()
    | _ -> ()

(* The room left after a position, as the code computes it, plain. *)
#push-options "--fuel 3"
let room_value (len:nat) (lim:var) (w:st) (en:env) (pending:bool)
  : Lemma (requires inv len lim w en pending)
          (ensures ev en (room_expr lim w.p) == Some (len - posval en w.p))
  = match w.p.pv with
    | Some v -> assert (en v <= max_int /\ 0 <= en v)
    | None -> ()
#pop-options

let cancelled_value (len:nat) (lim:var) (w:st) (en:env) (pending:bool) (s:expr) (k:int)
  : Lemma (requires inv len lim w en pending /\ ev en s == Some k)
          (ensures (match cancelled w lim s w.p with
                    | Some (l, r) ->
                      (match ev en l, ev en r with
                       | Some x, Some y -> (x > y <==> posval en w.p + k > len)
                       | _ -> False)
                    | None -> True))
  = ev_eval en s; cancelled_ok w en lim s

(* What the bound of a span at the position decides: with the size the code
   binds, it rejects exactly when the codec does, and passing it means the
   span fits. [P > len] only when the bound before was left to this one. *)
let outside_ok (o:opts) (len:nat) (lim:var) (w:st) (en:env) (pending:bool) (s:expr)
  : Lemma (requires inv len lim w en pending /\ closed w.sc s)
          (ensures (let ov = not (fits w.rg s) in
                    let nonneg = not ov && nonnegative w s in
                    match span_size w en s with
                    | Some k ->
                      (match outside o w lim en s ov nonneg k with
                       | Some rej ->
                         (rej <==> (posval en w.p > len \/
                                    (match ev en s with None -> True | Some k' -> k' < 0 \/ posval en w.p + k' > len))) /\
                         (not rej ==> (ev en s == Some k /\ k >= 0))
                       | None -> False)
                    | None -> False))
  = let ov = not (fits w.rg s) in
    let nonneg = not ov && nonnegative w s in
    if not ov then fits_ev w.rg en s;
    if nonneg then nonnegative_ok w en s;
    room_value len lim w en pending;
    match span_size w en s with
    | Some k ->
      if not ov then cancelled_value len lim w en pending s k;
      assert (posval en w.p >= 0)
    | None -> ()

(* The size a span binds when a dispatch already checked its bound. *)
let bounded_size (len:nat) (lim:var) (w:st) (en:env) (pending:bool) (s:expr)
  : Lemma (requires inv len lim w en pending /\ mem (s, w.p) w.bounded)
          (ensures (match ev en s, span_size w en s with
                    | Some k, Some k' -> k == k' /\ k >= 0 /\ posval en w.p + k <= len
                    | _ -> False))
  = bounded_mem w.sc len en w.bounded s w.p;
    if fits w.rg s then fits_ev w.rg en s

let end_range_ok (w:st) (en:env) (s:expr) (k:int)
  : Lemma (requires sound_ranges w.rg en /\ pos_ok w en /\ k >= 0 /\
                    (fits w.rg s ==> ev en s == Some k) /\ in_int k)
          (ensures (let nr = size_range w s in
                    fst nr <= k /\ k <= snd nr /\ in_int (fst nr) /\ in_int (snd nr) /\
                    (match end_range w nr w.p with
                     | Some (lo, hi) -> lo <= posval en w.p + k /\ posval en w.p + k <= hi /\ in_int lo /\ in_int hi
                     | None -> True)))
  = range_sound w.rg en s;
    ev_eval en s

#push-options "--z3rlimit 120 --fuel 1 --ifuel 1"
(* Binding a span keeps the invariant. *)
let bind_span_ok (len:nat) (lim:var) (w:st) (en:env) (pending:bool) (implied:bool) (n e:var) (s:expr) (k:int)
  : Lemma (requires inv len lim w en pending /\ closed w.sc s /\ not (w.sc n) /\ not (w.sc e) /\ n <> e /\
                    k >= 0 /\ in_int k /\ (fits w.rg s ==> ev en s == Some k) /\
                    in_int (posval en w.p + k) /\
                    (not implied ==> posval en w.p + k <= len) /\
                    (implied ==> Some? (end_range w (size_range w s) w.p)))
          (ensures (let ends = end_range w (size_range w s) w.p in
                    let w' = bind_span w implied n e s ends in
                    let en' = upd (upd en n k) e (posval en w.p + k) in
                    inv len lim w' en' (posval en w.p + k > len) /\
                    posval en' w'.p == posval en w.p + k))
  = let x = posval en w.p + k in
    let en1 = upd en n k in
    let en' = upd en1 e x in
    let sc1 = extend w.sc n in
    let ends = end_range w (size_range w s) w.p in
    let w' = bind_span w implied n e s ends in
    end_range_ok w en s k;
    facts_upd w.sc en n k w.facts;
    facts_upd sc1 en1 e x w.facts;
    bounded_upd w.sc len en n k w.bounded;
    bounded_upd sc1 len en1 e x w.bounded;
    sums_upd w en n k;
    sums_upd ({ w with sc = sc1 }) en1 e x;
    pos_sum_ok w en true w.p;
    sum_of_ok en s;
    closed_sum_of w.sc s;
    if fits w.rg s then fits_ev w.rg en s;
    ev_eval en s;
    (match pos_sum w true w.p, sum_of s with
     | Some ps, Some y ->
       sum_add_ok en ps y;
       closed_sum_add w.sc ps y;
       (match sum_add ps y with
        | Some sm ->
          eval_terms_upd w.sc en n k sm.terms;
          closed_terms_extend w.sc n sm.terms;
          eval_terms_upd sc1 en1 e x sm.terms;
          closed_terms_extend sc1 e sm.terms
        | None -> ())
     | _ -> ());
    assert (sums_ok w' en');
    assert (sound_ranges w'.rg en')
#pop-options

(* {1 Where clauses} *)

let refine_ok (len:nat) (lim:var) (w:st) (en:env) (c:cond)
  : Lemma (requires inv len lim w en false /\ cond_closed w.sc c /\ holds en c == Some true)
          (ensures inv len lim (refine w c) en false)
  = match c with
    | Ge a b ->
      ev_eval en a; ev_eval en b;
      let w' = refine w c in
      introduce forall (v:var). (match w'.rg v with
             | Some (lo, hi) -> in_int lo /\ in_int hi /\ lo <= en v /\ en v <= hi
             | None -> True)
      with (match a, b with
            | V x, Lit _ -> assert (match w.rg x with Some (lo, hi) -> lo <= en x /\ en x <= hi | None -> True)
            | Lit _, V x -> assert (match w.rg x with Some (lo, hi) -> lo <= en x /\ en x <= hi | None -> True)
            | _ -> ())
    | Eq _ _ -> ()

(* {1 The theorem} *)

let span_first (sc:scope) (b:list node) (r:list node) (s:expr)
  : Lemma (requires wf sc (b @ (Join :: r)) /\ first_span b == Some s) (ensures closed sc s)
  = ()

let rec all_first_pick (s:expr) (t:int) (cs:list (int & list node))
  : Lemma (requires all_first s cs)
          (ensures (match pick t cs with Some b -> first_span b == Some s | None -> True))
  = match cs with [] -> () | (k, _) :: r -> if k = t then () else all_first_pick s t r

#push-options "--z3rlimit 200 --fuel 2 --ifuel 2"
let rec exec_spec (o:opts) (len:nat) (lim:var) (rd:reader) (w:st) (en:env) (pending:bool) (l:list node)
  : Lemma (requires inv len lim w en pending /\ wf w.sc l /\ (pending ==> checks_next l))
          (ensures exec o len lim rd w en l ==
                   (if posval en w.p > len then Reject else spec len rd en (posval en w.p) l))
          (decreases lsize l)
  = let pv = posval en w.p in
    match l with
    | [] -> ()
    | Prefix fs size :: r ->
      let need = ahead l in
      (match w.p.pv with Some v -> assert (0 <= en v /\ en v <= max_int) | None -> ());
      (* The constant the emitter adds does not wrap: the position's offset
         lies in the window, the run's bytes in a record. *)
      max_string_length_small ();
      assert (w.p.at <= len /\ need <= max_string_length);
      wrap_id (w.p.at + need);
      if w.known >= need then begin
        reads_ok len rd en pv fs w.sc size;
        bind_fields_ok len lim rd w en pv fs size;
        let w' = bind_fields w fs in
        let w2 = { w' with known = w.known - size; p = { w.p with at = w.p.at + size } } in
        let en' = read_fields rd en pv fs in
        assert (posval en' w2.p == pv + size);
        exec_spec o len lim rd w2 en' false r
      end
      else if pv + need > len then begin
        if pv <= len then ahead_reject len rd en pv l
      end
      else begin
        reads_ok len rd en pv fs w.sc size;
        bind_fields_ok len lim rd w en pv fs size;
        let w' = bind_fields w fs in
        let w2 = { w' with known = need - size; p = { w.p with at = w.p.at + size } } in
        let en' = read_fields rd en pv fs in
        assert (posval en' w2.p == pv + size);
        exec_spec o len lim rd w2 en' false r
      end
    | Span n e s :: r ->
      let ov = not (fits w.rg s) in
      let nonneg = not ov && nonnegative w s in
      let ends = end_range w (size_range w s) w.p in
      let implied = o.drop && checks_next r && nonneg && Some? ends in
      if fits w.rg s then fits_ev w.rg en s;
      ev_eval en s;
      if mem (s, w.p) w.bounded then begin
        bounded_size len lim w en pending s;
        let k = Some?.v (ev en s) in
        bind_span_ok len lim w en pending implied n e s k;
        exec_spec o len lim rd (bind_span w implied n e s ends) (upd (upd en n k) e (pv + k)) false r
      end
      else if implied then begin
        nonnegative_ok w en s;
        let k = eval en s in
        end_range_ok w en s k;
        bind_span_ok len lim w en pending implied n e s k;
        exec_spec o len lim rd (bind_span w implied n e s ends) (upd (upd en n k) e (pv + k)) (pv + k > len) r
      end
      else begin
        outside_ok o len lim w en pending s;
        match span_size w en s with
        | Some k ->
          (match outside o w lim en s ov nonneg k with
           | Some false ->
             bind_span_ok len lim w en pending implied n e s k;
             exec_spec o len lim rd (bind_span w implied n e s ends) (upd (upd en n k) e (pv + k)) false r
           | _ -> ())
        | None -> ()
      end
    | Test c :: r ->
      (match holds en c with
       | Some true ->
         refine_ok len lim w en c;
         exec_spec o len lim rd (refine w c) en false r
       | _ -> ())
    | Gate sel cs d :: r ->
      chosen_size en sel cs d r;
      pick_size (en sel) cs;
      wf_pick w.sc (en sel) cs r;
      (match hoisted cs d with
       | Some s ->
         let Some b0 = d in
         span_first w.sc b0 r s;
         all_first_pick s (en sel) cs;
         let Some b = chosen en sel cs d in
         lsize_append b (Join :: r);
         if mem (s, w.p) w.bounded then begin
           bounded_size len lim w en pending s;
           exec_spec o len lim rd w en false (b @ (Join :: r))
         end
         else begin
           outside_ok o len lim w en pending s;
           let ov = not (fits w.rg s) in
           let nonneg = not ov && nonnegative w s in
           match span_size w en s with
           | Some k ->
             (match outside o w lim en s ov nonneg k with
              | Some false ->
                let w1 = { w with bounded = (s, w.p) :: w.bounded } in
                assert (inv len lim w1 en false);
                exec_spec o len lim rd w1 en false (b @ (Join :: r))
              | _ -> ())
           | None -> ()
         end
       | None ->
         (match chosen en sel cs d with
          | Some b -> lsize_append b (Join :: r); exec_spec o len lim rd w en false (b @ (Join :: r))
          | None -> exec_spec o len lim rd w en false (Join :: r)))
    | Join :: r -> exec_spec o len lim rd w en false r
#pop-options

(* {1 Corollaries for a walk from the start of a window} *)

#push-options "--fuel 2 --ifuel 2"
let rec spec_no_fault (len:nat) (rd:reader) (en:env) (p:int) (l:list node)
  : Lemma (ensures ~ (Fault? (spec len rd en p l))) (decreases lsize l)
  = match l with
    | [] -> ()
    | Prefix fs size :: r -> if p + size > len then () else spec_no_fault len rd (read_fields rd en p fs) (p + size) r
    | Span n e s :: r ->
      (match ev en s with
       | Some k -> if k < 0 || p + k > len then () else spec_no_fault len rd (upd (upd en n k) e (p + k)) (p + k) r
       | None -> ())
    | Test c :: r -> spec_no_fault len rd en p r
    | Gate sel cs d :: r ->
      chosen_size en sel cs d r;
      (match chosen en sel cs d with
       | Some b -> spec_no_fault len rd en p (b @ (Join :: r))
       | None -> spec_no_fault len rd en p (Join :: r))
    | Join :: r -> spec_no_fault len rd en p r
#pop-options

(* The walk at the start of a window whose length [lim] holds (Emitter.start). *)
let start (lim:var) : st = {
  sc = (fun v -> v = lim);
  rg = (fun v -> if v = lim then Some (0, max_int) else None);
  facts = [];
  sums = (fun _ -> None);
  bounded = [];
  known = 0;
  p = { pv = None; at = 0 };
}

(* M1 and M2: the generated code accepts exactly what the codec accepts,
   binding the same values, with or without the O1 rewrites, and never reads
   outside the window nor wraps an operation. *)
let scanner_correct (o:opts) (len:nat) (lim:var) (rd:reader) (en:env) (l:list node)
  : Lemma (requires len <= max_string_length /\ en lim == len /\ wf (start lim).sc l)
          (ensures exec o len lim rd (start lim) en l == spec len rd en 0 l /\
                   ~ (Fault? (exec o len lim rd (start lim) en l)))
  = max_string_length_small ();
    exec_spec o len lim rd (start lim) en false l;
    spec_no_fault len rd en 0 l

(* The O1 rewrites change no answer. *)
let o1_preserves (len:nat) (lim:var) (rd:reader) (en:env) (l:list node)
  : Lemma (requires len <= max_string_length /\ en lim == len /\ wf (start lim).sc l)
          (ensures exec ({ cancel = true; drop = true }) len lim rd (start lim) en l ==
                   exec ({ cancel = false; drop = false }) len lim rd (start lim) en l)
  = scanner_correct ({ cancel = true; drop = true }) len lim rd en l;
    scanner_correct ({ cancel = false; drop = false }) len lim rd en l
