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

noeq type around = { alim : var; ap : pos; abounded : list (expr & pos); adisp : int }

(* A decision, with where it is taken: how many regions deep, and the
   position when it is a constant of the window. *)
type decision = { dsize : expr; dkind : kind; depth : nat; dat : option nat }

let at_const (p:pos) : option nat = match p.pv with None -> Some p.at | Some _ -> None

let decide (s:expr) (k:kind) (fr:list around) (p:pos) : decision =
  { dsize = s; dkind = k; depth = length fr; dat = at_const p }

(* Where a prefix may load its shared words (057ff551): how many regions
   deep, whether its position is a constant of the window, that position's
   constant part, and the bytes [wlo] to [whi] from its address the words
   must lie in; [wdisp], the bytes from the emitter's base to the window
   (Plan.Scan.region_start, b633f225), which the emitter adds to the
   offsets it reports. *)
type wdecision = { wdepth : nat; wconst : bool; wat : int; wlo : int; whi : int;
                   wneed : int; wtested : bool; wdisp : int }

(* A dispatch: where it is, and the size of the span every case starts with
   when its bound is checked once before it. *)
type gdecision = { gdepth : nat; gat : option nat; ghoisted : option expr }

type event =
  | ESpan of decision
  | EWords of wdecision
  | EGate of gdecision

(* The walk takes fuel rather than a measure, so evaluating it calls no
   lemma about the sizes of the lists it builds. *)
let rec walk (fuel:nat) (w:st) (lim:var) (disp:int) (fr:list around) (l:list node)
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
              wlo = (if constant then 0 else w.p.at); whi = extent w.p.at (if k < 0 then 0 else k);
              wneed = need; wtested = w.known < need; wdisp = disp }) ::
    walk fuel ({ w' with known = k - size; p = { w.p with at = w.p.at + size } }) lim disp fr r
  | Span n e s :: r ->
    let (d, implied, ends) = span_decision w lim s r true in
    ESpan (decide s d fr w.p) :: walk fuel (bind_span w implied n e s ends) lim disp fr r
  | Test c :: r -> walk fuel (refine w c) lim disp fr r
  | Gate sel cs d :: r ->
    let hoist : list event & st =
      match hoisted cs d with
      | Some s ->
        if mem (s, w.p) w.bounded then ([], w)
        else (let (k, _, _) = span_decision w lim s [] false in
              ([ ESpan (decide s k fr w.p) ], { w with bounded = (s, w.p) :: w.bounded }))
      | None -> ([], w) in
    let (ds, w1) = hoist in
    let gate = EGate ({ gdepth = length fr; gat = at_const w.p; ghoisted = hoisted cs d }) in
    let rest : list event =
      match d with
      | Some b -> walk fuel w1 lim disp fr (b @ (Join :: r))
      | None -> walk fuel w1 lim disp fr (Join :: r) in
    ds @ gate :: walk_cases fuel w1 lim disp fr cs r @ rest
  | Join :: r -> walk fuel w lim disp fr r
  | Region n e s am b :: r ->
    let (d, implied, ends) = span_decision w lim s r (unread b) in
    let w1 = bind_span w implied n e s ends in
    let a = { alim = lim; ap = w1.p; abounded = w1.bounded; adisp = disp } in
    ESpan (decide s d fr w.p) ::
    walk fuel ({ w1 with p = { pv = None; at = 0 }; known = fst (region_range w1 n); bounded = [] }) n
      (disp + snd (region_start w w.p)) (a :: fr) (b @ (Close am :: r))
  | Close _ :: r ->
    (match fr with
     | [] -> walk fuel w lim disp [] r
     | a :: fs -> walk fuel ({ w with known = 0; p = a.ap; bounded = a.abounded }) a.alim a.adisp fs r)
and walk_cases (fuel:nat) (w:st) (lim:var) (disp:int) (fr:list around) (cs:list (int & list node)) (r:list node)
  : Tot (list event) (decreases %[fuel; 1; cs]) =
  match cs with
  | [] -> []
  | (_, b) :: cs' -> walk fuel w lim disp fr (b @ (Join :: r)) @ walk_cases fuel w lim disp fr cs' r

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
  walk 100000 w lim 0 [] l

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

let rec gates_of (es:list event) : list gdecision =
  match es with
  | [] -> []
  | EGate d :: r -> d :: gates_of r
  | _ :: r -> gates_of r

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
   of its prefix, [efields], at offsets from its address, and the first
   byte [elo] they may start at. The emitter counts these offsets, and the
   prefix's [eat], from its base, the displacement [wdisp] before the
   window (Plan.Proof.region_start_ok). *)
noeq type wrecord = { edepth : nat; econst : bool; eat : int; elo : int;
                      efields : list (int & nat); ewords : list int }

(* [m] justifies the words of [e]: the same prefix, and each word one
   Plan.Scan.choose may keep for one of its fields, so between [m]'s bounds
   and holding the field, once the window's displacement is taken off the
   emitter's offsets; the emitter's first byte is the window's start for a
   prefix at a constant offset, the prefix's address otherwise. A prefix at a
   position the emitter only knows at run time, such as a frame's in the
   step scanner, loads no word before its address. A prefix that loads no
   word needs no justification. *)
let words_justified (ms:list wdecision) (e:wrecord) : bool =
  Nil? e.ewords ||
  existsb (fun (m:wdecision) ->
    let d = m.wdisp in
    let at = e.eat - d in
    m.wdepth = e.edepth && m.wat = at && (m.wconst || not e.econst) &&
    e.elo = (if e.econst then m.wlo + d else e.eat) &&
    (let lo = if e.econst then m.wlo else (if m.wlo < at then at else m.wlo) in
     let fs = map (fun (f:int & nat) -> ((fst f - d <: int), snd f)) e.efields in
     for_all (fun (a:int) -> Cons? (choose (fun a' _ _ -> a' = a - d) lo m.whi fs)) e.ewords)) ms

let rec words_unjustified (ms:list wdecision) (es:list wrecord) : list wrecord =
  match es with
  | [] -> []
  | e :: r -> if words_justified ms e then words_unjustified ms r else e :: words_unjustified ms r

(* {1 The bytes a run of fields needs, dispatches and the frame loop} *)

(* An emitter's check of the bytes a run of fields at [eat] needs: [ebytes]
   bytes, [etested] when it writes a test of the window's length. *)
type precord = { pdepth : nat; pconst : bool; pat : int; pbytes : int; ptested : bool }

(* [m] justifies [e]: the same prefix needing the same bytes, the length
   left untested only where the model leaves it untested. *)
(* A size or a position as the emitter adds it with Planner.extent, which
   stops past any buffer; the model's walk adds plainly, and its checks go
   through [extent] alike. *)
let saturate (n:int) : int = if n > max_string_length then max_string_length + 1 else n

let prefix_justified (ms:list wdecision) (e:precord) : bool =
  existsb (fun (m:wdecision) ->
    m.wdepth = e.pdepth && saturate m.wneed = e.pbytes &&
    ((m.wconst = e.pconst && saturate m.wat = e.pat && (e.ptested || not m.wtested)) ||
     (* After a join the emitter's position is a variable where the model,
        which copies the code after a dispatch into each case, keeps the
        case's: a tested run of the bytes the model's needs is justified. *)
     (not e.pconst && e.ptested))) ms

let rec prefixes_unjustified (ms:list wdecision) (es:list precord) : list precord =
  match es with
  | [] -> []
  | e :: r -> if prefix_justified ms e then prefixes_unjustified ms r else e :: prefixes_unjustified ms r

(* A dispatch the emitter hoists a bound before is justified by the model's
   dispatch at the same place hoisting the same size; one that hoists none
   needs no justification. *)
let gate_justified (ms:list gdecision) (e:gdecision) : bool =
  None? e.ghoisted ||
  existsb (fun (m:gdecision) ->
    m.gdepth = e.gdepth && m.gat = e.gat && m.ghoisted = e.ghoisted) ms

let rec gates_unjustified (ms:list gdecision) (es:list gdecision) : list gdecision =
  match es with
  | [] -> []
  | e :: r -> if gate_justified ms e then gates_unjustified ms r else e :: gates_unjustified ms r

(* Scan_module.fixed_need: the bytes of the leading runs of fields and of
   the first optional field's, which a frame loop knows a frame holds once
   its stride is at least that. The tests between them, a field's checks,
   read no byte. *)
let rec fixed_need (l:list node) : nat =
  match l with
  | Prefix _ size :: r -> extent size (fixed_need r)
  | Test _ :: r -> fixed_need r
  | Gate _ ((_, b) :: _) _ :: _ -> fixed_need b
  | Gate _ [] (Some b) :: _ -> fixed_need b
  | _ -> 0

(* {1 The fields a frame loop loads ahead} *)

(* The fields at constant offsets of the bytes [fixed_need] counts, each with
   its offset from the frame's start and its width: those
   Scan_module.gated_fields finds, so those the OxCaml frame loop may load
   ahead of a frame's walk. *)
let rec lead_fields (at:nat) (l:list node) : Tot (list (var & (nat & nat))) (decreases l) =
  match l with
  | Prefix fs size :: r ->
    map (fun (f:field) -> (f.id, ((at + f.off <: nat), (f.width <: nat)))) fs @ lead_fields (at + size) r
  | Test _ :: r -> lead_fields at r
  | Gate _ ((_, b) :: _) _ :: _ -> lead_fields at b
  | Gate _ [] (Some b) :: _ -> lead_fields at b
  | _ -> []

(* Every field loaded ahead lies in the [need] bytes the loop knows. *)
let rec ahead_in (need:nat) (ah:list (nat & nat)) : bool =
  match ah with [] -> true | (k, wd) :: r -> k + wd <= need && ahead_in need r

(* Each field loaded ahead, with the offset from the frame's start and the
   bytes the loop loads it from, is one of [lead]: the bytes the walk reads
   it from. *)
let rec ahead_at (lead:list (var & (nat & nat))) (ah:list (var & (nat & nat))) : bool =
  match ah with [] -> true | (v, kw) :: r -> assoc v lead = Some kw && ahead_at lead r

(* A frame loop's read-ahead [ah] is justified when each field is loaded from
   the bytes of the leading fields the walk reads it from, inside the [need]
   bytes the loop knows. *)
let ahead_justified (need:nat) (l:list node) (ah:list (var & (nat & nat))) : bool =
  ahead_at (lead_fields 0 l) ah && ahead_in need (map snd ah)

(* {1 Dispatch trees} *)

(* A dispatch's tree is justified when it runs the first case whose
   condition holds, testing each atom at most once on a path
   (Plan.Tree.tree_first, tested_once), and runs a case on every path when
   the emitter hoists a bound before it (always_runs). *)
let dispatch_justified (cs:list Plan.Tree.formula) (t:Plan.Tree.choice) (hoists:bool) : bool =
  Plan.Tree.tree_ok [] cs t && (not hoists || not (Plan.Tree.runs_none t))

(* {1 Ranges of bitfields} *)

(* The bounds of a range of two bitfields are those of their checks
   (Plan.Range.range_correct). *)
let range_justified (bits shift width shift' width' c d lo hi:nat) : bool =
  Plan.Range.range_ok bits shift width shift' width' c d lo hi
