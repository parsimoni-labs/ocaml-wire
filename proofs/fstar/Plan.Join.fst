module Plan.Join

(* The join of a dispatch (Decision.Joined, Emitter.define_join): where the
   code after a run of optional fields is written once, every case that ran,
   or none, jumps to a local function that walks it, with the position the
   case ended at as a fresh variable [e]. That walk starts from what the
   walk knew before the cases (Emitter's [inner]: its ranges, facts, sums,
   checked bounds and held words), knowing no byte of the window after [e].
   The model's [exec] copies the code after the dispatch into each case
   instead (Decision.Copied). [join_correct] proves the joined walk accepts
   what the codec accepts from where the case ended. *)

open FStar.List.Tot
open Plan.Expr
open Plan.Sum
open Plan.Scan
open Plan.Proof

(* The walk the join writes, from the walk [w1] before the cases. *)
let join_state (w1:st) (e:var) : st =
  { w1 with sc = extend w1.sc e; known = 0; p = { pv = Some e; at = 0 } }

let rec facts_agree (sc:scope) (en en':env) (fs:list (expr & expr))
  : Lemma (requires facts_ok sc en fs /\ (forall (v:var). sc v ==> en' v == en v))
          (ensures facts_ok sc en' fs)
  = match fs with
    | [] -> ()
    | (a, b) :: r -> ev_agree sc en en' a; ev_agree sc en en' b; facts_agree sc en en' r

let rec terms_agree (sc:scope) (en en':env) (ts:terms)
  : Lemma (requires closed_terms sc ts /\ (forall (v:var). sc v ==> en' v == en v))
          (ensures eval_terms en' ts == eval_terms en ts)
  = match ts with [] -> () | (e, _) :: r -> ev_agree sc en en' e; terms_agree sc en en' r

(* The walk before the cases still holds of the environment after a case,
   which binds only variables that walk had not bound, given that its
   ranges name only variables it bound. *)
let before_holds (len:nat) (lim:var) (w1:st) (en1 enc:env)
  : Lemma (requires inv len lim w1 en1 false /\
                    (forall (v:var). w1.sc v ==> enc v == en1 v) /\
                    (forall (v:var). Some? (w1.rg v) ==> w1.sc v))
          (ensures inv len lim w1 enc false)
  = facts_agree w1.sc en1 enc w1.facts;
    bounded_agree w1.sc w1.sc len en1 enc w1.bounded;
    introduce forall (v:var). (match w1.sums v with
                  | Some s -> w1.sc v /\ closed_sum w1.sc s /\ unique s.terms /\ eval_sum enc s == enc v
                  | None -> True)
    with (match w1.sums v with Some s -> terms_agree w1.sc en1 enc s.terms | None -> ());
    (match w1.p.pv with Some v -> assert (enc v == en1 v) | None -> ())

(* The walk the join writes holds of the environment after a case, with its
   position variable [e] bound to where the case ended. *)
let join_holds (len:nat) (lim:var) (w1:st) (en1 enc:env) (e:var) (q:int)
  : Lemma (requires inv len lim w1 en1 false /\
                    (forall (v:var). w1.sc v ==> enc v == en1 v) /\
                    (forall (v:var). Some? (w1.rg v) ==> w1.sc v) /\
                    not (w1.sc e) /\ 0 <= q /\ q <= len)
          (ensures inv len lim (join_state w1 e) (upd enc e q) false)
  = before_holds len lim w1 en1 enc;
    max_string_length_small ();
    facts_upd w1.sc enc e q w1.facts;
    bounded_upd w1.sc len enc e q w1.bounded;
    sums_upd w1 enc e q;
    assert (forall (v:var). Some? (w1.rg v) ==> v <> e)

(* M1 for a join: the walk of the code after a dispatch, written once and
   jumped to with the position [q] a case ended at, accepts what the codec
   accepts from [q], binding the same values and [e], and never faults. Its
   premises are that the case bound only variables the walk before it had
   not, and that the walk's ranges name only variables it bound. *)
let join_correct (o:opts) (rd:reader) (x:win) (w1:st) (en1 enc:env) (e:var) (q:int) (r:list node)
  : Lemma (requires 0 <= x.len /\ inv x.len x.lim w1 en1 false /\
                    frames_ok w1.sc en1 x.wb x.len x.frames /\ held_ok o rd x w1 /\
                    (forall (v:var). w1.sc v ==> enc v == en1 v) /\
                    (forall (v:var). Some? (w1.rg v) ==> w1.sc v) /\
                    not (w1.sc e) /\ 0 <= q /\ q <= x.len /\
                    wf (extend w1.sc e) r)
          (ensures (let en = upd enc e q in
                    exec o rd x (join_state w1 e) en r ==
                    spec rd x.wb x.len (spec_frames en1 x.frames) en q r))
  = let en = upd enc e q in
    join_holds x.len x.lim w1 en1 enc e q;
    frames_agree w1.sc (extend w1.sc e) en1 en x.wb x.len x.frames;
    exec_spec o rd x (join_state w1 e) en false r
