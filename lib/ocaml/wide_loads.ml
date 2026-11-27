(* {1 Wide loads}

   The 64-bit words a prefix loads once, each for the fields it holds. *)

open Wire.Private
open Layout
module T = Types
open Planner

(* The 64-bit words of a prefix of [size] bytes worth loading once: those
   holding two or more of [fields] a shift takes out. An [int64_u] position
   reads a byte by a 16-bit load that needs no swap, which a test or an
   offset of the byte has sooner than a shift of a swapped word, so a byte is
   not taken from a word when the position is [unboxed]. *)
let in_word ~unboxed f =
  match f.store with
  | Int { width = 8; _ } | Bits { base = T.U8; _ } -> not unboxed
  | Int { width = 16 | 32; endian = T.Big; _ } -> true
  | Bits { base = T.U16 T.Big | T.U32 T.Big; _ } -> true
  | _ -> false

let covers a f = a <= f.offset && f.offset + width_bytes f.store <= a + 8

(* One 64-bit load serves every field it holds, as the load combining of Go's
   [memcombine] pass turns adjacent narrow loads into a wide one. A word
   starts at a multiple of 8, or ends with the prefix. Candidates are computed
   from the fields, so the cost does not grow with [size]. *)
let words ~unboxed ~size fields =
  if size < 8 then []
  else
    let last = size - 8 in
    List.concat_map
      (fun f ->
        if not (in_word ~unboxed f) then []
        else
          List.filter
            (fun a -> covers a f)
            [ min (f.offset / 8 * 8) last; last ])
      fields
    |> List.sort_uniq Int.compare
    |> List.filter (fun a ->
        let held =
          List.filter_map
            (fun f ->
              if in_word ~unboxed f && covers a f then Some f.offset else None)
            fields
        in
        List.length (List.sort_uniq Int.compare held) > 1)

(* The words a prefix loads with [Boxed], where a word costs one load and a
   field a shift and a mask. A word is loaded when it holds two or more
   fields that no word of [held] holds, counting the fields of the later
   prefixes, [later]. It starts at the first such field, moved back to stay
   between [lo] and [hi], the bytes known to be in the window. The fields are
   sorted by offset and scanned once, so the cost is linear in their number. *)
let shared_words ~held ~lo ~hi ~later live =
  let worded = List.filter (in_word ~unboxed:false) in
  let by_offset = List.sort (fun a b -> Int.compare a.offset b.offset) in
  let live = by_offset (worded live) in
  let candidates = Array.of_list (by_offset (live @ worded later)) in
  let starts = Hashtbl.create 16 in
  List.iter (fun a -> Hashtbl.replace starts a ()) held;
  let unheld g =
    let last = g.offset and first = g.offset + width_bytes g.store - 8 in
    let rec free a =
      a > last || ((not (Hashtbl.mem starts a)) && free (a + 1))
    in
    free (max 0 first)
  in
  let next = ref 0 in
  List.fold_left
    (fun chosen f ->
      let a = max lo (min f.offset (hi - 8)) in
      while !next < Array.length candidates && candidates.(!next).offset < a do
        incr next
      done;
      let rec served i acc =
        if i >= Array.length candidates || candidates.(i).offset >= a + 8 then
          acc
        else
          let g = candidates.(i) in
          served (i + 1)
            (if covers a g && unheld g && not (List.mem g.offset acc) then
               g.offset :: acc
             else acc)
      in
      if
        unheld f
        && a + 8 <= hi
        && covers a f
        && List.compare_length_with (served !next []) 1 > 0
      then begin
        Hashtbl.replace starts a ();
        a :: chosen
      end
      else chosen)
    [] live
  |> List.rev

(* The live fields that start before [until] in the prefixes of [nodes], at
   constant offsets from [at]. The prefix that starts each optional field of
   a following run counts too. *)
let rec following live ~until at nodes =
  if at >= until then []
  else
    match nodes with
    | Prefix (fields, n) :: rest ->
        List.filter_map
          (fun f ->
            let offset = extent at f.offset in
            if Sset.mem f.id live && offset < until then Some { f with offset }
            else None)
          fields
        @ following live ~until (extent at n) rest
    | Test _ :: rest -> following live ~until at rest
    | Gate _ :: _ as gates -> starts live ~until at gates
    | (Span _ | Region _) :: _ | [] -> []

and starts live ~until at = function
  | Gate (_, _, _, body) :: rest ->
      following live ~until at body @ starts live ~until at rest
  | _ -> []
