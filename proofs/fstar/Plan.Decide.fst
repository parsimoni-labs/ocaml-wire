module Plan.Decide

(* The decisions the model's walk takes for a plan's spans and region
   sizes, on every case of every dispatch, and when one of them justifies a
   decision Wire_ocaml.Plan.spans reports the emitter took. Plan.Proof
   proves the walk correct whatever it decides; this module only computes
   what it decides, so the emitter's decisions can be checked against it
   (make plans). *)

open FStar.List.Tot
open Plan.Expr
open Plan.Sum
open Plan.Scan

(* Emitter.bound: left to the next check, covered by a hoisted check, or
   checked here, with the test that the size is negative and with the
   common terms cancelled. *)
type kind =
  | KNext
  | KHoisted
  | KChecked : negative:bool -> cancelled:bool -> kind

(* What the model's walk decides for a span of [s] bytes at [w.p], followed
   by [next]: [exec]'s Span case, without the values. *)
let span_decision (w:st) (lim:var) (s:expr) (next:list node) (may_drop:bool)
  : kind & bool & option (int & int) =
  let ov = not (fits w.rg s) in
  let nonneg = not ov && nonnegative w s in
  let ends = end_range w (size_range w s) w.p in
  let implied = may_drop && checks_next next && nonneg && Some? ends in
  if implied then (KNext, true, ends)
  else if mem (s, w.p) w.bounded then (KHoisted, false, ends)
  else (KChecked (not nonneg) (Some? (if ov then None else cancelled w lim s w.p)), false, ends)

noeq type around = { alim : var; ap : pos; abounded : list (expr & pos) }

(* A decision, with where it is taken: how many regions deep, and the
   position when it is a constant of the window. *)
type decision = { dsize : expr; dkind : kind; depth : nat; dat : option nat }

let at_const (p:pos) : option nat = match p.pv with None -> Some p.at | Some _ -> None

let decide (s:expr) (k:kind) (fr:list around) (p:pos) : decision =
  { dsize = s; dkind = k; depth = length fr; dat = at_const p }

(* Where a prefix may load its shared words (057ff551): how many regions
   deep, whether its position is a constant of the window, that position's
   constant part, and the bytes [wlo] to [whi] from its address the words
   must lie in. *)
type wdecision = { wdepth : nat; wconst : bool; wat : int; wlo : int; whi : int }

type event =
  | ESpan of decision
  | EWords of wdecision

(* The walk takes fuel rather than a measure, so evaluating it calls no
   lemma about the sizes of the lists it builds. *)
let rec walk (fuel:nat) (w:st) (lim:var) (fr:list around) (l:list node)
  : Tot (list event) (decreases %[fuel; 0; 0]) =
  if fuel = 0 then [] else
  let fuel = fuel - 1 in
  match l with
  | [] -> []
  | Prefix fs size :: r ->
    let need = ahead l in
    let k = if w.known >= need then w.known else need in
    let w' = bind_fields w fs in
    let constant = None? w.p.pv in
    EWords ({ wdepth = length fr; wconst = constant; wat = w.p.at;
              wlo = (if constant then 0 else w.p.at); whi = extent w.p.at (if k < 0 then 0 else k) }) ::
    walk fuel ({ w' with known = k - size; p = { w.p with at = w.p.at + size } }) lim fr r
  | Span n e s :: r ->
    let (d, implied, ends) = span_decision w lim s r true in
    ESpan (decide s d fr w.p) :: walk fuel (bind_span w implied n e s ends) lim fr r
  | Test c :: r -> walk fuel (refine w c) lim fr r
  | Gate sel cs d :: r ->
    let hoist : list event & st =
      match hoisted cs d with
      | Some s ->
        if mem (s, w.p) w.bounded then ([], w)
        else (let (k, _, _) = span_decision w lim s [] false in
              ([ ESpan (decide s k fr w.p) ], { w with bounded = (s, w.p) :: w.bounded }))
      | None -> ([], w) in
    let (ds, w1) = hoist in
    let rest : list event =
      match d with
      | Some b -> walk fuel w1 lim fr (b @ (Join :: r))
      | None -> walk fuel w1 lim fr (Join :: r) in
    ds @ walk_cases fuel w1 lim fr cs r @ rest
  | Join :: r -> walk fuel w lim fr r
  | Region n e s am b :: r ->
    let (d, _, ends) = span_decision w lim s [] false in
    let w1 = bind_span w false n e s ends in
    let a = { alim = lim; ap = w1.p; abounded = w1.bounded } in
    ESpan (decide s d fr w.p) :: walk fuel ({ w1 with p = { pv = None; at = 0 }; known = 0; bounded = [] }) n (a :: fr)
                (b @ (Close am :: r))
  | Close _ :: r ->
    (match fr with
     | [] -> walk fuel w lim [] r
     | a :: fs -> walk fuel ({ w with known = 0; p = a.ap; bounded = a.abounded }) a.alim fs r)
and walk_cases (fuel:nat) (w:st) (lim:var) (fr:list around) (cs:list (int & list node)) (r:list node)
  : Tot (list event) (decreases %[fuel; 1; cs]) =
  match cs with
  | [] -> []
  | (_, b) :: cs' -> walk fuel w lim fr (b @ (Join :: r)) @ walk_cases fuel w lim fr cs' r

(* The model's decisions for a plan, from the start of a window whose length
   variable [lim] holds. *)
let events (lim:var) (known:nat) (l:list node) : list event =
  let w : st = {
    sc = (fun v -> v = lim);
    rg = (fun v -> if v = lim then Some (0, max_int) else None);
    facts = [];
    sums = (fun _ -> None);
    bounded = [];
    known = known;
    p = { pv = None; at = 0 };
    held = [];
  } in
  walk 100000 w lim [] l

let rec spans_of (es:list event) : list decision =
  match es with
  | [] -> []
  | ESpan d :: r -> d :: spans_of r
  | _ :: r -> spans_of r

let rec words_of (es:list event) : list wdecision =
  match es with
  | [] -> []
  | EWords d :: r -> d :: words_of r
  | _ :: r -> words_of r

let decisions (lim:var) (l:list node) : list decision = spans_of (events lim 0 l)

(* Where the prefixes may load words, in a window of at least [known]
   bytes. *)
let prefixes (lim:var) (known:nat) (l:list node) : list wdecision = words_of (events lim known l)

(* A model decision [m] justifies an emitter decision [k] for the same size
   when it decides at least as much: the same bound left out or hoisted, a
   negative test left out only where the model leaves it out, terms
   cancelled only where the model cancels them. A full plain check needs no
   justification. *)
let covers (m k:kind) : bool =
  match k, m with
  | KChecked true false, _ -> true
  | KNext, KNext -> true
  | KHoisted, KHoisted -> true
  | KChecked neg c, KChecked neg' c' -> (neg || not neg') && (not c || c')
  | KChecked neg false, KNext -> true
  | _ -> false

(* [m] justifies [e]: the same size, region depth and constant position,
   and a kind that covers [e]'s. *)
let justified (ms:list decision) (e:decision) : bool =
  existsb (fun (m:decision) ->
    m.dsize = e.dsize && m.depth = e.depth && m.dat = e.dat && covers m.dkind e.dkind) ms

(* The emitter's decisions [es] that [ms] does not justify. *)
let rec unjustified (ms:list decision) (es:list decision) : list decision =
  match es with
  | [] -> []
  | e :: r -> if justified ms e then unjustified ms r else e :: unjustified ms r

(* The shared words an emitter's prefix loads, [ewords], with the fields
   of its prefix, [efields], at offsets from its address. *)
noeq type wrecord = { edepth : nat; econst : bool; eat : int; efields : list (int & nat); ewords : list int }

(* [m] justifies the words of [e]: the same prefix, and each word one
   Plan.Scan.choose may keep for one of its fields, so between [m]'s bounds
   and holding the field. A prefix at a position the emitter only knows at
   run time, such as a frame's in the step scanner, loads no word before
   its address. A prefix that loads no word needs no justification. *)
let words_justified (ms:list wdecision) (e:wrecord) : bool =
  Nil? e.ewords ||
  existsb (fun (m:wdecision) ->
    m.wdepth = e.edepth && m.wat = e.eat && (m.wconst || not e.econst) &&
    (let lo = if e.econst then m.wlo else (if m.wlo < e.eat then e.eat else m.wlo) in
     for_all (fun (a:int) -> Cons? (choose (fun a' _ _ -> a' = a) lo m.whi e.efields)) e.ewords)) ms

let rec words_unjustified (ms:list wdecision) (es:list wrecord) : list wrecord =
  match es with
  | [] -> []
  | e :: r -> if words_justified ms e then words_unjustified ms r else e :: words_unjustified ms r
