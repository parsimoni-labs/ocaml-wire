open Wire.Private.Everparse

let hex_of_bytes b =
  let digits = "0123456789abcdef" in
  let out = Bytes.create (Bytes.length b * 2) in
  Bytes.iteri
    (fun i c ->
      let n = Char.code c in
      Bytes.set out (2 * i) digits.[n lsr 4];
      Bytes.set out ((2 * i) + 1) digits.[n land 0xf])
    b;
  Bytes.unsafe_to_string out

(* The accept/reject decision the validator must reproduce, and -- on rejection
   -- what the codec objected to, which makes an input repairable. The record
   must span the whole buffer because the hardened [Check] wrapper rejects
   trailing bytes. [`Resize n] means that the parsed record occupies [n] bytes. *)
let codec_verdict ?env c buf =
  match Wire.Codec.decode ?env c buf ~off:0 with
  | Error e -> `Reject e
  | Ok _ -> (
      match Wire.Codec.validate ?env c buf ~off:0 with
      | Error e -> `Reject e
      | Ok () -> (
          match Wire.Codec.size_at ?env c buf ~off:0 with
          | Error e -> `Reject e
          | Ok n -> if n = Bytes.length buf then `Accept else `Resize n))

let codec_accepts ?env c buf =
  match codec_verdict ?env c buf with `Accept -> true | _ -> false

(* A small param value, biased around the codec's footprint so a parameter that
   bounds a field size (e.g. a max length) straddles the accept/reject boundary
   rather than always accepting or always rejecting. *)
let fuzz_param_value rng center =
  match Random.State.int rng 6 with
  | 0 -> 0
  | 1 -> center
  | 2 -> center + 1
  | 3 -> Random.State.int rng (max 1 ((2 * center) + 4))
  | _ -> Random.State.int rng (max 1 (center + 1))

(* Structurally-biased lengths: cluster around the codec's minimum size so the
   corpus straddles the accept/reject boundary. The exact size and a little
   over exercise the constraint path; under-size exercises truncation; a few
   far-out lengths add breadth. *)
let fuzz_length rng center =
  match Random.State.int rng 10 with
  | 0 -> 0
  | 1 | 2 -> Random.State.int rng (max 1 (center + 1))
  | 3 -> (2 * center) + Random.State.int rng 4
  | 4 -> center + 1
  | _ -> center

(* [agree.c] reads a line into a fixed 64 KiB buffer. *)
let max_corpus_input = 65536

(* A seed grows no further than this: a line is written as hex and replayed
   through the validator, and 256 lines at the 64 KiB a 16-bit length can ask
   for are megabytes of both. *)
let max_seed_input = 4096

let random_bytes rng n =
  Bytes.init n (fun _ -> Char.chr (Random.State.int rng 256))

(* Keep bytes already repaired when the codec supplies the record's true size. *)
let resize rng buf n =
  let out = random_bytes rng n in
  Bytes.blit buf 0 out 0 (min n (Bytes.length buf));
  out

(* Write the low [width] bytes of [v] without native-int shifts: field seeds may
   describe an eight-byte slot even on wasm_of_ocaml or js_of_ocaml. Negative
   signed values naturally produce their two's-complement wire bytes. *)
let write_slot buf off (slot : Raw.int_slot) v =
  let width = slot.width in
  if off >= 0 && off + width <= Bytes.length buf then
    for i = 0 to width - 1 do
      let shift =
        8
        * match slot.endian with Wire.Big -> width - 1 - i | Wire.Little -> i
      in
      let byte = Int64.(to_int (logand (shift_right_logical v shift) 0xffL)) in
      Bytes.set buf (off + i) (Char.chr byte)
    done

let seed_for seeds (e : Wire.Error.t) =
  match List.rev e.field with
  | leaf :: _ ->
      List.find_opt
        (fun (seed : Raw.field_seed) -> String.equal seed.field leaf)
        seeds
  | [] -> None

(* An EOF can be repaired by growing the input, and a missing terminator by
   writing one where the scan gave up: both are structural, so neither needs a
   value the declaration singles out. Anything else is repairable only when the
   failing field's declaration names candidate values. *)
let repair_for ~seeds ~len (e : Wire.Error.t) =
  match e.kind with
  | Wire.Error.Unexpected_eof { expected; got } when expected > got ->
      `Grow (len + expected - got)
  | Wire.Error.Missing_terminator when e.at < len -> `Terminate e.at
  | Wire.Error.Missing_terminator -> `Grow (e.at + 1)
  | _ -> (
      match seed_for seeds e with
      | Some seed -> `Place (e.at, seed)
      | None -> `Stuck)

(* Construct one accepting input by repeatedly asking the codec how to repair
   the earliest failure. The field offsets come from parse errors rather than a
   static layout, so the same loop works after variable-width prefixes. *)
let seed_input ?env rng ~seeds ~center ~fill c =
  let fuel = ref ((2 * List.length seeds) + 6) in
  let buf = ref (fill rng (max 1 center)) in
  let placed = ref [] in
  let out = ref None in
  let place off (seed : Raw.field_seed) =
    let values = Array.of_list seed.values in
    let value = values.(Random.State.int rng (Array.length values)) in
    write_slot !buf off seed.slot value;
    if not (List.mem_assoc off !placed) then
      placed := (off, (seed, value)) :: !placed
  in
  (* A record asking for more bytes than a seed may carry was sized by a
     length field the byte draw filled with a large value. No parse error names
     that field -- the demand surfaces on the record's whole extent -- so damp
     every length at once: each byte loses its high half, which shrinks any
     length and reaches zero, the smallest, on the second pass. The values
     already settled are written back, so the loop resumes from the progress it
     had made rather than starting over. *)
  let damp () =
    Bytes.iteri
      (fun i c -> Bytes.set !buf i (Char.chr (Char.code c lsr 4)))
      !buf;
    List.iter
      (fun (off, ((seed : Raw.field_seed), value)) ->
        write_slot !buf off seed.slot value)
      !placed
  in
  let grow n =
    if n > max_seed_input then damp ()
    else if n >= 0 && n <> Bytes.length !buf then buf := resize rng !buf n
    else fuel := 0
  in
  while !out = None && !fuel > 0 do
    decr fuel;
    match codec_verdict ?env c !buf with
    | `Accept -> out := Some !buf
    | `Resize n -> grow n
    | `Reject e -> (
        match repair_for ~seeds ~len:(Bytes.length !buf) e with
        | `Grow n -> grow n
        | `Terminate off -> Bytes.set !buf off '\000'
        | `Place (off, seed) -> place off seed
        | `Stuck -> fuel := 0)
  done;
  match !out with Some b -> Some (b, List.rev !placed) | None -> None

let neighbours v =
  let before = if Int64.equal v Int64.min_int then [] else [ Int64.pred v ] in
  let after = if Int64.equal v Int64.max_int then [] else [ Int64.succ v ] in
  before @ (v :: after)

(* Put each named value, and its immediate neighbours, directly into every
   constrained field settled while constructing the seed. These cases expose a
   stale validator whose boundary differs by one without relying on chance. *)
let boundary_inputs seed placed =
  List.concat_map
    (fun (off, ((field_seed : Raw.field_seed), _)) ->
      field_seed.values |> List.concat_map neighbours
      |> List.sort_uniq Int64.compare
      |> List.map (fun value ->
          let b = Bytes.copy seed in
          write_slot b off field_seed.slot value;
          b))
    placed

(* Mutants cross field constraints and the checked wrapper's whole-buffer
   boundary while retaining most of a known-accepted record. *)
let mutate rng seed =
  let len = Bytes.length seed in
  if len = 0 then seed
  else
    match Random.State.int rng 8 with
    | 0 -> Bytes.sub seed 0 (len - 1)
    | 1 when len < max_corpus_input ->
        let b = Bytes.create (len + 1) in
        Bytes.blit seed 0 b 0 len;
        Bytes.set b len (Char.chr (Random.State.int rng 256));
        b
    | kind ->
        let b = Bytes.copy seed in
        let offset = Random.State.int rng len in
        let value = Char.code (Bytes.get b offset) in
        let value' =
          if kind land 1 = 0 then value lxor (1 lsl Random.State.int rng 8)
          else Random.State.int rng 256
        in
        Bytes.set b offset (Char.chr (value' land 0xff));
        b

(* Try a bounded number of fresh byte/parameter combinations for each desired
   accepting seed. An unsolved constraint is reported by the vacuity check
   rather than spinning. *)
(* Where a record's accepting side sits at an extreme, a uniform draw never
   lands on it: a predicate satisfied at the minimum, a terminator in the first
   byte, a float pattern the bytes have to spell out. Start from the extremes as
   well as from noise and let the same repair loop settle the rest, which costs
   a way to solve for the value nothing at all. *)
let extremal_fills =
  [| (fun _rng n -> Bytes.make n '\000'); (fun _rng n -> Bytes.make n '\255') |]

let accepting_seeds ?env_of rng ~seeds ~center ~draw_params ~want c =
  let env_of = match env_of with Some f -> f | None -> fun _ -> None in
  let n_extremal = Array.length extremal_fills in
  (* The extremes are tried first and are deterministic, so they are extra
     attempts rather than a share of the random ones: a shape whose accepting
     side is only ever found by chance keeps every draw it had. *)
  let found = ref []
  and tried = ref 0
  and attempts = ref ((4 * want) + n_extremal) in
  while List.length !found < want && !attempts > 0 do
    decr attempts;
    let fill =
      if !tried < n_extremal then extremal_fills.(!tried) else random_bytes
    in
    incr tried;
    let pvals = draw_params () in
    match seed_input ?env:(env_of pvals) rng ~seeds ~center ~fill c with
    | Some (b, placed) -> found := (pvals, b, placed) :: !found
    | None -> ()
  done;
  Array.of_list !found

let vacuous_corpus name ~accepts ~rejects =
  Fmt.failwith
    "corpus for %s is vacuous: %d accepted, %d rejected. A differential over \
     it cannot tell the validator from one that answers the same way to every \
     input.@\n\
     %s"
    name accepts rejects
    (if accepts = 0 then
       "Nothing reached the accepting side of the codec's constraints. \
        Wire.Private.Everparse.Raw.field_seeds names values only for equality, \
        inequality, ordering and closed-enum constraints on whole-byte integer \
        fields; any other constraint needs a hand-written corpus."
     else "Every input was accepted; the corpus needs a rejected one.")

(* Seeds, boundary cases, seed mutants, then uniform bytes for breadth. Keep the
   boundary cases ahead of random mutants and preserve exactly [count] lines. *)
let emit_streams ~count ~emit ~rng ~center ~draw_params seeded =
  let emitted = ref 0 in
  Array.iter
    (fun (pvals, b, _) ->
      if !emitted < count then begin
        emit (pvals, b);
        incr emitted
      end)
    seeded;
  Array.iter
    (fun (pvals, b, placed) ->
      List.iter
        (fun boundary ->
          if !emitted < count then begin
            emit (pvals, boundary);
            incr emitted
          end)
        (boundary_inputs b placed))
    seeded;
  let mutant_count =
    if Array.length seeded = 0 then 0
    else min (count / 2) (max 0 (count - !emitted))
  in
  for _ = 1 to mutant_count do
    let pvals, b, _ = seeded.(Random.State.int rng (Array.length seeded)) in
    emit (pvals, mutate rng b);
    incr emitted
  done;
  for _ = 1 to max 0 (count - !emitted) do
    let len = min max_corpus_input (fuzz_length rng center) in
    emit (draw_params (), random_bytes rng len)
  done

let v ~count ppf c =
  let rng = Random.State.make [| 0x5eed51 |] in
  let schema = Wire.Everparse.project ~mode:`Standalone c in
  let name = Wire.Everparse.name schema in
  let pnames =
    match Wire.Private.Everparse.source schema with
    | Some source -> Raw.input_param_names source
    | None -> []
  in
  let seeds =
    match Wire.Private.Everparse.source schema with
    | Some source -> Raw.field_seeds source
    | None -> []
  in
  let center = Wire.Codec.min_size c in
  let draw_params () = List.map (fun _ -> fuzz_param_value rng center) pnames in
  let env_of pvals =
    match pnames with
    | [] -> None
    | _ ->
        Some
          (List.fold_left2
             (fun env name value -> Wire.Param.bind_by_name name value env)
             (Wire.Codec.env c) pnames pvals)
  in
  let accepts = ref 0 and rejects = ref 0 in
  let emit (pvals, b) =
    let pfield =
      match pvals with
      | [] -> "-"
      | _ -> String.concat "," (List.map string_of_int pvals)
    in
    let hex = if Bytes.length b = 0 then "-" else hex_of_bytes b in
    let accepted = codec_accepts ?env:(env_of pvals) c b in
    if accepted then incr accepts else incr rejects;
    Fmt.pf ppf "%s %s %s %d@\n" name pfield hex (if accepted then 1 else 0)
  in
  emit_streams ~count ~emit ~rng ~center ~draw_params
    (accepting_seeds ~env_of rng ~seeds ~center ~draw_params
       ~want:(max 1 (count / 8))
       c);
  if !accepts = 0 || !rejects = 0 then
    vacuous_corpus name ~accepts:!accepts ~rejects:!rejects
