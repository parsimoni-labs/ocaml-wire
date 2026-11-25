module Plan.Loop

(* Which walk the frame loop of fold_frames gives each frame, and the fields
   the OxCaml loop loads ahead of a frame's walk (Scan_module.pp_frames,
   pp_frames_loop, 922e2bce).

     if stride < need then <each frame by fold, or by a walk knowing 0 bytes>
     else <each frame by the walk knowing [need] bytes>

   With four frames an iteration, the loop binds for each of its frames, at
   [base_j], the fields [ahead] at their constant offset [k] from the frame
   start, before the walk of its first frame; the frames the loop walks one
   by one after it bind them at their own start. The walk then reads those
   variables instead of loading the fields. *)

open FStar.List.Tot
open Plan.Expr
open Plan.Scan
open Plan.Proof
open Plan.Frames
open Plan.Decide

(* The bytes of its window the walk of a frame of [s] bytes knows. *)
let frame_known (need:nat) (s:int) : nat = if s < need then 0 else need

(* The loads ahead of the walk of a frame at [b], each an offset in the
   buffer and a width in bytes: those of a loop of four frames an iteration,
   in the branch the frames hold [need] bytes. *)
let frame_loads (frames:nat) (need:nat) (ah:list (nat & nat)) (b s:int) : list (int & nat) =
  if frames = 4 && not (s < need) then map (fun (kw:nat & nat) -> ((b + fst kw <: int), snd kw)) ah else []

(* The frames' starts, in the order the loop walks them: four an iteration
   in the branch that knows [need] bytes on OxCaml, one an iteration
   otherwise. *)
let bases (frames:nat) (need:nat) (off len:int) (s:Prims.pos) : list int =
  if frames = 4 && not (s < need) then four_frames off len s else cursor off (off + len - s) s

(* Each frame the loop walks: its start, the bytes its walk knows and the
   loads made ahead of it. *)
let frame_walk (frames:nat) (need:nat) (ah:list (nat & nat)) (s:int) (b:int)
  : int & nat & list (int & nat) =
  (b, frame_known need s, frame_loads frames need ah b s)

let frame_walks (frames:nat) (need:nat) (ah:list (nat & nat)) (off len:int) (s:Prims.pos)
  : list (int & nat & list (int & nat)) =
  map (frame_walk frames need ah s) (bases frames need off len s)

let rec loads_in (need:nat) (ah:list (nat & nat)) (b s:int) (a:int) (wd:nat)
  : Lemma (requires ahead_in need ah /\ need <= s /\
                    memP (a, wd) (map (fun (kw:nat & nat) -> ((b + fst kw <: int), snd kw)) ah))
          (ensures b <= a /\ a + wd <= b + s)
          (decreases ah)
  = match ah with
    | [] -> ()
    | (k, w') :: r -> if a = b + k && wd = w' then () else loads_in need r b s a wd

(* Behind the frame guard, every frame the loop walks lies in the buffer,
   its walk knows no more bytes than the frame has, and every load ahead of
   it reads inside it: a frame shorter than [need] bytes, walked knowing
   none, has nothing loaded ahead, and one of four in an iteration has its
   fields loaded at its own start. *)
let frame_walks_in (buflen:nat) (frames:nat) (need:nat) (ah:list (nat & nat)) (off len:int) (s:Prims.pos)
  (b:int) (kn:nat) (ls:list (int & nat))
  : Lemma (requires buflen <= max_int /\ guard buflen off len s /\ ahead_in need ah /\
                    memP (b, kn, ls) (frame_walks frames need ah off len s))
          (ensures 0 <= b /\ b + s <= buflen /\ kn <= s /\
                   (forall (a:int) (wd:nat). memP (a, wd) ls ==> b <= a /\ a + wd <= b + s))
  = memP_map_elim (frame_walk frames need ah s) (b, kn, ls) (bases frames need off len s);
    if frames = 4 && not (s < need) then four_frames_in_buffer buflen off len s b
    else frames_in_buffer buflen off len s b;
    let aux (a:int) (wd:nat) : Lemma (memP (a, wd) ls ==> b <= a /\ a + wd <= b + s) =
      if frames = 4 && not (s < need) then
        FStar.Classical.move_requires (loads_in need ah b s a) wd
      else () in
    FStar.Classical.forall_intro_2 aux

(* Both branches walk the frames the counted loop did. *)
let frame_walks_frames (frames:nat) (need:nat) (off len:nat) (s:Prims.pos)
  : Lemma (bases frames need off len s == counted off s 0 (len / s))
  = if frames = 4 && not (s < need) then four_frames_loop off len s else cursor_loop off len s

(* The walk of a frame of [s] bytes at [b], knowing [kn <= s] bytes, accepts
   what the codec accepts in that window and never faults: Plan.Proof's
   theorem from a start that knows bytes. *)
let frame_walk_correct (o:opts) (rd:reader) (b:int) (s:nat) (lim:var) (en:env) (l:list node) (kn:nat)
  : Lemma (requires s <= max_string_length /\ en lim == s /\ kn <= s /\ wf (start lim).sc l /\
                    (o.words ==> consistent rd))
          (ensures (let x = { wb = b; len = s; lim = lim; frames = [] } in
                    let w = { start lim with known = kn } in
                    exec o rd x w en l == spec rd b s [] en 0 l /\ ~ (Fault? (exec o rd x w en l))))
  = max_string_length_small ();
    let x = { wb = b; len = s; lim = lim; frames = [] } in
    exec_spec o rd x ({ start lim with known = kn }) en false l;
    spec_no_fault rd b s [] en 0 l

(* The frame loop: behind the frame guard, in a buffer no longer than
   [Sys.max_string_length], every frame it walks lies in the buffer, every
   field it loads ahead lies in that frame, and the walk of the frame, from
   the bytes the loop gives it as known, accepts exactly what the codec
   accepts in the frame, and never faults. *)
let frame_loop_correct (o:opts) (rd:reader) (buflen:nat) (frames:nat) (need:nat) (ah:list (nat & nat))
  (off len:int) (s:Prims.pos) (lim:var) (en:env) (l:list node) (b:int) (kn:nat) (ls:list (int & nat))
  : Lemma (requires buflen <= max_string_length /\ guard buflen off len s /\ ahead_in need ah /\
                    memP (b, kn, ls) (frame_walks frames need ah off len s) /\
                    en lim == s /\ wf (start lim).sc l /\ (o.words ==> consistent rd))
          (ensures (let x = { wb = b; len = s; lim = lim; frames = [] } in
                    let w = { start lim with known = kn } in
                    0 <= b /\ b + s <= buflen /\
                    (forall (a:int) (wd:nat). memP (a, wd) ls ==> b <= a /\ a + wd <= b + s) /\
                    exec o rd x w en l == spec rd b s [] en 0 l /\ ~ (Fault? (exec o rd x w en l))))
  = max_string_length_small ();
    frame_walks_in buflen frames need ah off len s b kn ls;
    frame_walk_correct o rd b s lim en l kn
