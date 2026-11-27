module Plan.Index

(* The [_index] loop of a visitor's fold over the frames of an index,
   [lens.(j)] bytes at [offs.(j)] of a buffer of [length] bytes, read once
   before the loop (Scan_module.pp_fold_step_index).

   The plain loop (pp_fold_step_index_plain) walks frame after frame
   through [_within], the walk of one frame, which tests that the frame
   lies in the buffer and knows none of its bytes; on OxCaml it walks four
   frames an iteration, then the rest one by one.

   When the frames need [need > 0] bytes before their first variable offset
   (Scan_module.fixed_need), the loop is pipelined one frame deep, two
   frames an iteration (pp_fold_index_ahead, pp_index_frames):

     <next 0>; c := 0; i := 1;
     while i < n - 1 do
       <next i>; <walk c>; <next (i + 1)>; <walk i>; c := i + 1; i := i + 2
     done;
     if i < n then (<next i>; <walk c>; <walk i>) else <walk c>

   <next j> (pp_index_next) loads [offs.(j)] and [lens.(j)] as [base] and
   [stop], tests

     known_j = base >= 0 && stop >= need && stop <= length - base

   (on OxCaml, the sign of one [logor] of [int64_u] differences), and only
   if it holds loads the fields [ahead] at their constant offsets from
   [base]: those Scan_module.read_aheads ~leading finds, the fields of the
   leading prefixes among them. <walk j> (pp_index_walk) walks frame [j]
   knowing [need] bytes, reading the fields loaded ahead, if [known_j] held,
   and through [_within] otherwise.

   The walk of a frame calls the consumer, which the model assumes writes
   neither the index nor the buffer: the loop has loaded the next frame's
   offset, length and fields before it. *)

open FStar.List.Tot
open Plan.Expr
open Plan.Scan
open Plan.Proof
open Plan.Decide
open Plan.Loop
open Plan.Values

module UI = FStar.UInt

(* An index of [n] frames, each an offset and a length, OCaml ints. *)
noeq type index = { n : nat; offs : nat -> int; lens : nat -> int }

let index_ok (ix:index) : prop =
  forall (j:nat). j < ix.n ==> in_int (ix.offs j) /\ in_int (ix.lens j)

(* {1 The test of a frame} *)

(* The test on OCaml, which evaluates [length - base] only once
   [base >= 0] and [stop >= need] have passed. *)
let known (length:int) (need:nat) (b s:int) : bool = b >= 0 && s >= need && s <= length - b

(* The OCaml test cannot overflow: the one difference it computes lies in
   the native int wherever it is computed. *)
let known_no_overflow (length need b s:int)
  : Lemma (requires 0 <= length /\ length <= max_int /\ in_int b)
          (ensures b >= 0 ==> in_int (length - b))
  = ()

(* The test on OxCaml: [base], [stop], [length] and [need] as [int64_u]
   patterns, and the sign of

     logor (logor base (sub (sub length base) stop)) (sub stop need). *)
let lor64 (a b:pattern) : pattern = UI.logor #64 a b

let known_u (length need b s:int) : bool =
  let pb = of_int b in
  let ps = of_int s in
  signed (lor64 (lor64 pb (sub64 (sub64 (of_int length) pb) ps)) (sub64 ps (of_int need))) >= 0

(* The sign bit of a [logor] is set where one of its operands' is. *)
let lor_sign (a b:pattern)
  : Lemma (signed (lor64 a b) >= 0 <==> signed a >= 0 /\ signed b >= 0)
  = UI.lemma_msb_pow2 #64 a;
    UI.lemma_msb_pow2 #64 b;
    UI.lemma_msb_pow2 #64 (lor64 a b);
    UI.logor_definition #64 a b 0

(* A difference of 64 bits, where it does not wrap, is the difference. *)
let signed_mod (x:int)
  : Lemma (requires - pow2 63 <= x /\ x < pow2 63) (ensures signed (x % pow2 64) == x)
  = bounds ();
    if x >= 0 then FStar.Math.Lemmas.small_mod x (pow2 64)
    else begin
      FStar.Math.Lemmas.lemma_mod_plus x 1 (pow2 64);
      FStar.Math.Lemmas.small_mod (x + pow2 64) (pow2 64)
    end

let sub_of (x y:int) : Lemma (sub64 (x % pow2 64) (y % pow2 64) == (x - y) % pow2 64)
  = FStar.Math.Lemmas.lemma_mod_sub_distr (x % pow2 64) y (pow2 64);
    FStar.Math.Lemmas.lemma_mod_plus_distr_l x (- y) (pow2 64)

(* The OxCaml test is the OCaml one: a negative [base] sets the sign bit,
   and for any other no difference wraps, [int64_u]s holding the
   differences of two native ints and a length no longer than
   [Sys.max_string_length]. *)
let known_u_is (length need b s:int)
  : Lemma (requires 0 <= length /\ length <= max_string_length /\
                    0 <= need /\ need <= max_string_length + 1 /\ in_int b /\ in_int s)
          (ensures known_u length need b s == known length need b s)
  = bounds ();
    max_string_length_small ();
    let pb = of_int b in
    let ps = of_int s in
    let d1 = sub64 (sub64 (of_int length) pb) ps in
    let d2 = sub64 ps (of_int need) in
    lor_sign (lor64 pb d1) d2;
    lor_sign pb d1;
    signed_mod b;
    sub_of s need;
    signed_mod (s - need);
    sub_of length b;
    sub_of (length - b) s;
    if b >= 0 then signed_mod (length - b - s)

(* Both tests of a frame of an index agree. *)
let index_known_u (ix:index) (length need:nat) (j:nat)
  : Lemma (requires index_ok ix /\ j < ix.n /\ length <= max_string_length /\
                    need <= max_string_length + 1)
          (ensures known_u length need (ix.offs j) (ix.lens j) == known length need (ix.offs j) (ix.lens j))
  = known_u_is length need (ix.offs j) (ix.lens j)

(* {1 The loop} *)

(* What the loop does, in order: the test of frame [j] and its answer, a
   load of [w] bytes at [a] ahead of a frame's walk, the walk of frame [j],
   knowing [need] bytes ([true]) or through [_within]. *)
type ev =
  | Test : j:nat -> ok:bool -> ev
  | Load : j:nat -> a:int -> w:nat -> ev
  | Walk : j:nat -> k:bool -> ev

let known_at (ix:index) (length:int) (need:nat) (j:nat) : bool = known length need (ix.offs j) (ix.lens j)

let loads (j:nat) (b:int) (ah:list (nat & nat)) : list ev =
  map (fun (kw:nat & nat) -> Load j (b + fst kw) (snd kw)) ah

(* pp_index_next. *)
let next (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (j:nat) : list ev =
  let k = known_at ix length need j in
  Test j k :: (if k then loads j (ix.offs j) ah else [])

(* The loop from the frame [c], tested with the answer [kc] and not yet
   walked, at [i = c + 1]. *)
let rec pipeline (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (c:nat) (kc:bool)
  : Tot (list ev) (decreases (if c < ix.n then ix.n - c else 0)) =
  let i = c + 1 in
  if i + 1 < ix.n then
    next ix length need ah i @
    (Walk c kc ::
     (next ix length need ah (i + 1) @
      (Walk i (known_at ix length need i) ::
       pipeline ix length need ah (i + 1) (known_at ix length need (i + 1)))))
  else if i < ix.n then next ix length need ah i @ [ Walk c kc; Walk i (known_at ix length need i) ]
  else [ Walk c kc ]

(* pp_fold_index_ahead. *)
let index_loop (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) : list ev =
  if ix.n > 0 then next ix length need ah 0 @ pipeline ix length need ah 0 (known_at ix length need 0)
  else []

(* {1 The frames it visits} *)

let rec upto (i n:nat) : Tot (list nat) (decreases (if i < n then n - i else 0)) =
  if i < n then i :: upto (i + 1) n else []

(* The frames a loop walks, and those it tests, in order. *)
let rec walks (t:list ev) : list nat =
  match t with [] -> [] | Walk j _ :: r -> j :: walks r | _ :: r -> walks r

let rec tests (t:list ev) : list nat =
  match t with [] -> [] | Test j _ :: r -> j :: tests r | _ :: r -> tests r

let rec walks_append (a b:list ev) : Lemma (walks (a @ b) == walks a @ walks b) =
  match a with [] -> () | _ :: r -> walks_append r b

let rec tests_append (a b:list ev) : Lemma (tests (a @ b) == tests a @ tests b) =
  match a with [] -> () | _ :: r -> tests_append r b

let rec loads_none (j:nat) (b:int) (ah:list (nat & nat))
  : Lemma (walks (loads j b ah) == [] /\ tests (loads j b ah) == [])
  = match ah with [] -> () | _ :: r -> loads_none j b r

let next_frames (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (j:nat) (r:list ev)
  : Lemma (walks (next ix length need ah j @ r) == walks r /\
           tests (next ix length need ah j @ r) == j :: tests r)
  = let ls = if known_at ix length need j then loads j (ix.offs j) ah else [] in
    loads_none j (ix.offs j) ah;
    walks_append ls r;
    tests_append ls r

let rec pipeline_frames (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (c:nat) (kc:bool)
  : Lemma (requires c < ix.n)
          (ensures walks (pipeline ix length need ah c kc) == c :: upto (c + 1) ix.n /\
                   tests (pipeline ix length need ah c kc) == upto (c + 1) ix.n)
          (decreases (ix.n - c))
  = let i = c + 1 in
    if i + 1 < ix.n then begin
      let rest = pipeline ix length need ah (i + 1) (known_at ix length need (i + 1)) in
      pipeline_frames ix length need ah (i + 1) (known_at ix length need (i + 1));
      next_frames ix length need ah (i + 1) (Walk i (known_at ix length need i) :: rest);
      next_frames ix length need ah i
        (Walk c kc :: (next ix length need ah (i + 1) @ (Walk i (known_at ix length need i) :: rest)));
      assert (upto i ix.n == i :: upto (i + 1) ix.n);
      assert (upto (i + 1) ix.n == (i + 1) :: upto (i + 2) ix.n)
    end
    else if i < ix.n then begin
      next_frames ix length need ah i [ Walk c kc; Walk i (known_at ix length need i) ];
      assert (upto i ix.n == [ i ])
    end
    else ()

(* The pipelined loop walks the frames the plain loop does, each once, in
   the same order, and loads each one's offset and length once, in that
   order, before its walk: every index it reads is below [n]. *)
let index_loop_frames (ix:index) (length:int) (need:nat) (ah:list (nat & nat))
  : Lemma (walks (index_loop ix length need ah) == upto 0 ix.n /\
           tests (index_loop ix length need ah) == upto 0 ix.n)
  = if ix.n > 0 then begin
      pipeline_frames ix length need ah 0 (known_at ix length need 0);
      next_frames ix length need ah 0 (pipeline ix length need ah 0 (known_at ix length need 0))
    end

(* The plain loop on OxCaml: four frames an iteration while [i < n - 3],
   then one. *)
let rec unrolled (n i:nat) : Tot (list nat) (decreases (if i < n then n - i else 0)) =
  if i + 3 < n then i :: (i + 1) :: (i + 2) :: (i + 3) :: unrolled n (i + 4) else upto i n

let rec unrolled_frames (n i:nat)
  : Lemma (ensures unrolled n i == upto i n) (decreases (if i < n then n - i else 0))
  = if i + 3 < n then begin
      unrolled_frames n (i + 4);
      assert (upto i n == i :: upto (i + 1) n);
      assert (upto (i + 1) n == (i + 1) :: upto (i + 2) n);
      assert (upto (i + 2) n == (i + 2) :: upto (i + 3) n);
      assert (upto (i + 3) n == (i + 3) :: upto (i + 4) n)
    end

(* {1 What each step reads} *)

(* Each test is of its own frame; each walk of a frame is the one its test
   chose; each load ahead is of a frame whose test passed, inside the
   [need] bytes it holds. *)
let ev_ok (ix:index) (length:int) (need:nat) (e:ev) : bool =
  match e with
  | Test j k -> j < ix.n && k = known_at ix length need j
  | Walk j k -> j < ix.n && k = known_at ix length need j
  | Load j a w -> j < ix.n && known_at ix length need j && ix.offs j <= a && a + w <= ix.offs j + need

let rec loads_ok (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (j:nat)
  : Lemma (requires j < ix.n /\ known_at ix length need j /\ ahead_in need ah)
          (ensures for_all (ev_ok ix length need) (loads j (ix.offs j) ah))
  = match ah with [] -> () | _ :: r -> loads_ok ix length need r j

let next_ok (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (j:nat) (r:list ev)
  : Lemma (requires j < ix.n /\ ahead_in need ah /\ for_all (ev_ok ix length need) r)
          (ensures for_all (ev_ok ix length need) (next ix length need ah j @ r))
  = let ls = if known_at ix length need j then loads j (ix.offs j) ah else [] in
    if known_at ix length need j then loads_ok ix length need ah j;
    for_all_append (ev_ok ix length need) ls r

let rec pipeline_ok (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (c:nat)
  : Lemma (requires c < ix.n /\ ahead_in need ah)
          (ensures for_all (ev_ok ix length need) (pipeline ix length need ah c (known_at ix length need c)))
          (decreases (ix.n - c))
  = let i = c + 1 in
    let kc = known_at ix length need c in
    if i + 1 < ix.n then begin
      let rest = pipeline ix length need ah (i + 1) (known_at ix length need (i + 1)) in
      pipeline_ok ix length need ah (i + 1);
      next_ok ix length need ah (i + 1) (Walk i (known_at ix length need i) :: rest);
      next_ok ix length need ah i
        (Walk c kc :: (next ix length need ah (i + 1) @ (Walk i (known_at ix length need i) :: rest)))
    end
    else if i < ix.n then next_ok ix length need ah i [ Walk c kc; Walk i (known_at ix length need i) ]
    else ()

let rec for_all_memP (#a:Type) (f:a -> bool) (l:list a) (x:a)
  : Lemma (requires for_all f l /\ memP x l) (ensures f x)
  = match l with [] -> () | _ :: r -> FStar.Classical.move_requires (for_all_memP f r) x

let index_loop_ok (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (e:ev)
  : Lemma (requires ahead_in need ah /\ memP e (index_loop ix length need ah))
          (ensures ev_ok ix length need e)
  = if ix.n > 0 then begin
      pipeline_ok ix length need ah 0;
      next_ok ix length need ah 0 (pipeline ix length need ah 0 (known_at ix length need 0))
    end;
    for_all_memP (ev_ok ix length need) (index_loop ix length need ah) e

(* {1 The order of the loads} *)

(* Every load of a frame's field comes after the test of that frame passed
   and before the frame's walk. [tested] holds the frames whose test passed
   so far, [walked] those walked. *)
let rec ordered (tested walked:list nat) (t:list ev) : Tot bool (decreases t) =
  match t with
  | [] -> true
  | Test j k :: r -> ordered (if k then j :: tested else tested) walked r
  | Load j _ _ :: r -> mem j tested && not (mem j walked) && ordered tested walked r
  | Walk j _ :: r -> ordered tested (j :: walked) r

let rec below (c:int) (l:list nat) : bool = match l with [] -> true | x :: r -> x <= c && below c r

let rec below_not_mem (c:int) (l:list nat) (x:nat)
  : Lemma (requires below c l /\ x > c) (ensures not (mem x l))
  = match l with [] -> () | _ :: r -> below_not_mem c r x

let rec below_le (c d:int) (l:list nat)
  : Lemma (requires below c l /\ c <= d) (ensures below d l)
  = match l with [] -> () | _ :: r -> below_le c d r

let rec loads_ordered (tested walked:list nat) (j:nat) (b:int) (ah:list (nat & nat)) (r:list ev)
  : Lemma (requires mem j tested /\ not (mem j walked) /\ ordered tested walked r)
          (ensures ordered tested walked (loads j b ah @ r))
  = match ah with [] -> () | _ :: q -> loads_ordered tested walked j b q r

let next_ordered (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (tested walked:list nat) (j:nat) (r:list ev)
  : Lemma (requires not (mem j walked) /\
                    ordered (if known_at ix length need j then j :: tested else tested) walked r)
          (ensures ordered tested walked (next ix length need ah j @ r))
  = if known_at ix length need j then loads_ordered (j :: tested) walked j (ix.offs j) ah r

let rec pipeline_ordered (ix:index) (length:int) (need:nat) (ah:list (nat & nat)) (tested walked:list nat)
  (c:nat) (kc:bool)
  : Lemma (requires c < ix.n /\ below c walked)
          (ensures ordered tested walked (pipeline ix length need ah c kc))
          (decreases (ix.n - c))
  = let i = c + 1 in
    let t1 = if known_at ix length need i then i :: tested else tested in
    below_not_mem c walked i;
    if i + 1 < ix.n then begin
      let t2 = if known_at ix length need (i + 1) then (i + 1) :: t1 else t1 in
      let rest = pipeline ix length need ah (i + 1) (known_at ix length need (i + 1)) in
      below_le c (i + 1) walked;
      pipeline_ordered ix length need ah t2 (i :: c :: walked) (i + 1) (known_at ix length need (i + 1));
      below_not_mem c walked (i + 1);
      next_ordered ix length need ah t1 (c :: walked) (i + 1) (Walk i (known_at ix length need i) :: rest);
      next_ordered ix length need ah tested walked i
        (Walk c kc :: (next ix length need ah (i + 1) @ (Walk i (known_at ix length need i) :: rest)))
    end
    else if i < ix.n then
      next_ordered ix length need ah tested walked i [ Walk c kc; Walk i (known_at ix length need i) ]
    else ()

(* The loop loads a frame's fields only after that frame's test passed, and
   before its walk. *)
let index_loop_ordered (ix:index) (length:int) (need:nat) (ah:list (nat & nat))
  : Lemma (ordered [] [] (index_loop ix length need ah))
  = if ix.n > 0 then begin
      let k0 = known_at ix length need 0 in
      pipeline_ordered ix length need ah (if k0 then [ 0 ] else []) [] 0 k0;
      next_ordered ix length need ah [] [] 0 (pipeline ix length need ah 0 k0)
    end

(* {1 The walk of a frame} *)

let window (b s:int) (lim:var) : win = { wb = b; len = s; lim = lim; frames = [] }

(* [_within]: the walk of one frame, which tests that it lies in the
   buffer and knows none of its bytes; the plain loop's walk of every frame. *)
let within (o:opts) (rd:reader) (length:int) (lim:var) (en:env) (l:list node) (b s:int) : outcome =
  if b < 0 || s < 0 || s > length - b then Reject
  else exec o rd (window b s lim) (start lim) en l

(* The walk the pipelined loop gives a frame: knowing [need] bytes where its
   test passed, [_within] otherwise. *)
let run (o:opts) (rd:reader) (length:int) (need:nat) (lim:var) (en:env) (l:list node) (b s:int) (k:bool)
  : outcome =
  if k then exec o rd (window b s lim) ({ start lim with known = need }) en l
  else within o rd length lim en l b s

(* The walk the pipelined loop gives a frame answers what [_within]
   answers, which is what the codec accepts in the frame when it lies in
   the buffer, and a rejection otherwise; it never faults; and a frame whose
   test passed lies in the buffer and holds [need] bytes. *)
let index_walk_correct (o:opts) (rd:reader) (length need:nat) (lim:var) (en:env) (l:list node) (b s:int)
  : Lemma (requires length <= max_string_length /\ en lim == s /\ wf (start lim).sc l /\
                    (o.words ==> consistent rd))
          (ensures (let k = known length need b s in
                    let r = run o rd length need lim en l b s k in
                    r == within o rd length lim en l b s /\ ~ (Fault? r) /\
                    r == (if 0 <= b && 0 <= s && b + s <= length then spec rd b s [] en 0 l else Reject) /\
                    (k ==> 0 <= b /\ need <= s /\ b + s <= length)))
  = if 0 <= b && 0 <= s && b + s <= length then begin
      frame_walk_correct o rd b s lim en l 0;
      if known length need b s then frame_walk_correct o rd b s lim en l need
    end

(* The pipelined [_index] loop, in a buffer no longer than
   [Sys.max_string_length], with fields loaded ahead inside the [need] bytes
   its test ensures (Plan.Decide.ahead_in, which make plans checks): each
   test reads an index below [n]; each walk of a frame answers what the
   plain loop's walk of it answers, which is what the codec accepts in the
   frame when the frame lies in the buffer, and never faults; each load
   ahead is of a frame whose test passed, so that lies in the buffer, and
   reads inside the [need] bytes the frame holds. *)
let index_loop_correct (o:opts) (rd:reader) (ix:index) (length need:nat) (ah:list (nat & nat))
  (lim:var) (en:env) (l:list node) (e:ev)
  : Lemma (requires length <= max_string_length /\ ahead_in need ah /\ wf (start lim).sc l /\
                    (o.words ==> consistent rd) /\ memP e (index_loop ix length need ah))
          (ensures (match e with
                    | Test j k -> j < ix.n /\ k == known_at ix length need j
                    | Walk j k ->
                      j < ix.n /\
                      (en lim == ix.lens j ==>
                       (let b = ix.offs j in
                        let s = ix.lens j in
                        let r = run o rd length need lim en l b s k in
                        r == within o rd length lim en l b s /\ ~ (Fault? r) /\
                        r == (if 0 <= b && 0 <= s && b + s <= length then spec rd b s [] en 0 l else Reject)))
                    | Load j a w ->
                      j < ix.n /\ known_at ix length need j /\
                      0 <= ix.offs j /\ ix.offs j <= a /\ a + w <= ix.offs j + need /\
                      need <= ix.lens j /\ ix.offs j + ix.lens j <= length))
  = index_loop_ok ix length need ah e;
    match e with
    | Walk j k ->
      let aux () : Lemma (requires en lim == ix.lens j)
                         (ensures (let b = ix.offs j in
                                   let s = ix.lens j in
                                   let r = run o rd length need lim en l b s k in
                                   r == within o rd length lim en l b s /\ ~ (Fault? r) /\
                                   r == (if 0 <= b && 0 <= s && b + s <= length then spec rd b s [] en 0 l
                                         else Reject))) =
        index_walk_correct o rd length need lim en l (ix.offs j) (ix.lens j) in
      FStar.Classical.move_requires aux ()
    | _ -> ()
