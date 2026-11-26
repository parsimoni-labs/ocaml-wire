(* Records of at most 7 bytes as the [int] of their bytes, big-endian, for
   [Codec.writer] and [Codec.Word]. *)

open Codec_leaf

(* A record of at most 7 bytes is an [int] of its bytes, big-endian: the
   template's bits with the written fields' cleared, each field's value
   shifted to its place, written in a few stores. A field reaches this when
   it is a big-endian integer or a bitfield of a byte or a big-endian 16- or
   32-bit word; any other writer copies the template and calls [set] per
   field. *)

(* Where a field's value goes in the record's [int]: the proof its value is
   an [int], the bits of a value too wide for a bitfield ([hi], 0 for a field
   whose carrier is its range), the mask of its own bits and its shift. *)
type 'a slot = {
  eq : ('a, int) Type.eq;
  hi : int;
  wm : int;
  pos : int;
  width : int;
}

type 'a wfield = Slot of 'a slot | Setter of (bytes -> int -> 'a -> unit)

let int_of : type a. (a, int) Type.eq -> a -> int = fun Type.Equal x -> x

(* The record's bytes [n] at most 7, as an [int]. *)
let run_of_bytes b n =
  let acc = Stdlib.ref 0 in
  for i = 0 to n - 1 do
    acc := (!acc lsl 8) lor Bytes.get_uint8 b i
  done;
  !acc

let[@inline] store_run buf p n acc =
  match n with
  | 1 -> Unchecked.set_u8 buf p acc
  | 2 -> Unchecked.set_u16be buf p acc
  | 3 ->
      Unchecked.set_u16be buf p (acc lsr 8);
      Unchecked.set_u8 buf (p + 2) acc
  | 4 ->
      Unchecked.set_u16be buf p (acc lsr 16);
      Unchecked.set_u16be buf (p + 2) acc
  | 5 ->
      Unchecked.set_u16be buf p (acc lsr 24);
      Unchecked.set_u16be buf (p + 2) (acc lsr 8);
      Unchecked.set_u8 buf (p + 4) acc
  | 6 ->
      Unchecked.set_u16be buf p (acc lsr 32);
      Unchecked.set_u16be buf (p + 2) (acc lsr 16);
      Unchecked.set_u16be buf (p + 4) acc
  | _ ->
      Unchecked.set_u16be buf p (acc lsr 40);
      Unchecked.set_u16be buf (p + 2) (acc lsr 24);
      Unchecked.set_u16be buf (p + 4) (acc lsr 8);
      Unchecked.set_u8 buf (p + 6) acc

(* The slot of a field at [at] holding [nb] bytes, [w] bits of them above bit
   [s] of its word, in a record of [n] bytes. *)
let slot eq ~n ~at ~nb ~s ~w ~bits =
  let pos = (8 * (n - (at + nb))) + s in
  let wm = if w >= Sys.int_size then -1 else (1 lsl w) - 1 in
  { eq; hi = (if bits then lnot wm else 0); wm; pos; width = w }

(* A value too wide for its bitfield, raised as [set] raises it. *)
let[@inline never] slot_too_wide s x =
  Types.check_unsigned_encode ~bits:s.width x;
  Fmt.invalid_arg "Codec.writer: value %d does not fit %d bits" x s.width

let[@inline] slot_check s x = if x land s.hi <> 0 then slot_too_wide s x
let[@inline] slot_put s x acc = acc lor ((x land s.wm) lsl s.pos)

let[@inline never] writer_too_short name n buf off =
  Errors.encode_too_short ~name ~size:n ~len:(Bytes.length buf) off

type ('f, 'r) wlist =
  | WNil : (unit, 'r) wlist
  | WCons : 'a wfield * ('f, 'r) wlist -> ('a -> 'f, 'r) wlist

(* The arguments past the arities [writer] stages one closure for, one
   closure each, folded into [acc]; the record goes down once the last is in
   and checked. *)
let rec slots_curry : type f r. (f, r) wlist -> int -> (int -> unit) -> f =
 fun ws acc k ->
  match ws with
  | WNil -> k acc
  | WCons (Slot s, rest) ->
      fun a ->
        let x = int_of s.eq a in
        slot_check s x;
        slots_curry rest (slot_put s x acc) k
  | WCons (Setter _, _) -> invalid_arg "Codec.writer: mixed fields"

let rec setters_curry : type f r. (f, r) wlist -> bytes -> int -> f =
 fun ws buf off ->
  match ws with
  | WNil -> ()
  | WCons (Setter set, rest) ->
      fun a ->
        set buf off a;
        setters_curry rest buf off
  | WCons (Slot _, _) -> invalid_arg "Codec.writer: mixed fields"

let rec all_slots : type f r. (f, r) wlist -> bool = function
  | WNil -> true
  | WCons (Slot _, rest) -> all_slots rest
  | WCons (Setter _, _) -> false

(* The bits [ws] write, cleared from [t]. *)
let rec clear_slots : type f r. (f, r) wlist -> int -> int =
 fun ws t ->
  match ws with
  | WNil -> t
  | WCons (Slot s, rest) -> clear_slots rest (t land lnot (s.wm lsl s.pos))
  | WCons (Setter _, rest) -> clear_slots rest t

(* The run writer, one closure per arity up to 6. *)
(* The checks of a run writer once one has failed, in order: the frame, then
   each value. Out of line and called last, so a writer that passes them has
   no frame. *)
let[@inline never] run_failed name n buf off checks =
  if off < 0 || off > Bytes.length buf - n then writer_too_short name n buf off
  else checks ()

let run_writer : type f r.
    name:string -> n:int -> int -> (f, r) wlist -> bytes -> int -> f =
 fun ~name ~n t ws ->
  match ws with
  | WNil ->
      fun buf off ->
        if outside buf off n then run_failed name n buf off ignore
        else store_run buf off n t
  | WCons (Slot s1, WNil) ->
      fun buf off a1 ->
        let x1 = int_of s1.eq a1 in
        if outside buf off n || x1 land s1.hi <> 0 then
          run_failed name n buf off (fun () -> slot_check s1 x1)
        else store_run buf off n (slot_put s1 x1 t)
  | WCons (Slot s1, WCons (Slot s2, WNil)) ->
      fun buf off a1 a2 ->
        let x1 = int_of s1.eq a1 and x2 = int_of s2.eq a2 in
        if outside buf off n || x1 land s1.hi lor (x2 land s2.hi) <> 0 then
          run_failed name n buf off (fun () ->
              slot_check s1 x1;
              slot_check s2 x2)
        else store_run buf off n (slot_put s2 x2 (slot_put s1 x1 t))
  | WCons (Slot s1, WCons (Slot s2, WCons (Slot s3, WNil))) ->
      fun buf off a1 a2 a3 ->
        let x1 = int_of s1.eq a1
        and x2 = int_of s2.eq a2
        and x3 = int_of s3.eq a3 in
        if
          outside buf off n
          || x1 land s1.hi lor (x2 land s2.hi) lor (x3 land s3.hi) <> 0
        then
          run_failed name n buf off (fun () ->
              slot_check s1 x1;
              slot_check s2 x2;
              slot_check s3 x3)
        else
          store_run buf off n
            (slot_put s3 x3 (slot_put s2 x2 (slot_put s1 x1 t)))
  | WCons (Slot s1, WCons (Slot s2, WCons (Slot s3, WCons (Slot s4, WNil)))) ->
      fun buf off a1 a2 a3 a4 ->
        let x1 = int_of s1.eq a1 and x2 = int_of s2.eq a2 in
        let x3 = int_of s3.eq a3 and x4 = int_of s4.eq a4 in
        if
          outside buf off n
          || x1 land s1.hi lor (x2 land s2.hi) lor (x3 land s3.hi)
             lor (x4 land s4.hi)
             <> 0
        then
          run_failed name n buf off (fun () ->
              slot_check s1 x1;
              slot_check s2 x2;
              slot_check s3 x3;
              slot_check s4 x4)
        else
          store_run buf off n
            (slot_put s4 x4
               (slot_put s3 x3 (slot_put s2 x2 (slot_put s1 x1 t))))
  | ws ->
      fun buf off ->
        if outside buf off n then run_failed name n buf off ignore;
        slots_curry ws t (fun acc -> store_run buf off n acc)

(* The writer that copies the template and sets each field, one closure per
   arity up to 4. *)
let set_writer : type f r.
    name:string -> n:int -> bytes -> (f, r) wlist -> bytes -> int -> f =
 fun ~name ~n b ws ->
  let[@inline] frame buf off =
    if off < 0 || off > Bytes.length buf - n then
      writer_too_short name n buf off;
    Bytes.blit b 0 buf off n
  in
  match ws with
  | WCons (Setter s1, WNil) ->
      fun buf off a1 ->
        frame buf off;
        s1 buf off a1
  | WCons (Setter s1, WCons (Setter s2, WNil)) ->
      fun buf off a1 a2 ->
        frame buf off;
        s1 buf off a1;
        s2 buf off a2
  | WCons (Setter s1, WCons (Setter s2, WCons (Setter s3, WNil))) ->
      fun buf off a1 a2 a3 ->
        frame buf off;
        s1 buf off a1;
        s2 buf off a2;
        s3 buf off a3
  | WCons
      (Setter s1, WCons (Setter s2, WCons (Setter s3, WCons (Setter s4, WNil))))
    ->
      fun buf off a1 a2 a3 a4 ->
        frame buf off;
        s1 buf off a1;
        s2 buf off a2;
        s3 buf off a3;
        s4 buf off a4
  | ws ->
      fun buf off ->
        frame buf off;
        setters_curry ws buf off

(* {1 Reads} *)

module Word = struct
  (* A record of [n] bytes, [8 n] bits fewer than an [int] holds, as the [int]
     of its bytes big-endian. One load reads it: an 8-byte one where the buffer
     has 8 bytes from the record on, else the record's bytes one by one; a field
     is then two shifts and a mask, in the caller. *)
  type 'r t = int

  (* A field of a record's word: shifted left by [l], then right by [r] keeping
     its sign, then masked by [m] (all ones for a signed field). *)
  type ('a, 'r) field = { peq : ('a, int) Type.eq; l : int; r : int; m : int }

  external get64u : bytes -> int -> int64 = "%caml_bytes_get64u"
  external swap64 : int64 -> int64 = "%bswap_int64"

  let field eq ~pos ~width ~signed =
    {
      peq = eq;
      l = Sys.int_size - pos - width;
      r = Sys.int_size - width;
      m = (if signed then -1 else (1 lsl width) - 1);
    }

  (* A reader of records of [n] bytes, the shift that leaves them in the low
     bits of the 8 bytes from their offset, and the extent of each of their
     fields, in order: its name, offset and width. *)
  type 'r reader = { n : int; shift : int; fields : (string * int * int) array }

  let reader n ~fields =
    { n; shift = 8 * (8 - n); fields = Array.of_list fields }

  (* A check of a record of [n] bytes is the record's word, which [n] bytes
     leave non-negative ([Codec.Word.reader] takes [8 n] bits fewer than an
     [int] holds), or [-1] for a record that does not lie in the buffer. *)
  type 'r check = int

  (* The record's bytes one by one, where fewer than 8 follow it. *)
  let[@inline never] check_bytes r buf off =
    if off < 0 || off > Bytes.length buf - r.n then -1
    else
      let acc = Stdlib.ref 0 in
      for i = 0 to r.n - 1 do
        acc := (!acc lsl 8) lor Bytes.get_uint8 buf (off + i)
      done;
      !acc

  (* One 8-byte load where the buffer has 8 bytes from the record on, else the
     record's bytes one by one, out of line. A known function, so a caller's
     compiler inlines the load into its loop. *)
  let[@inline] check r buf off =
    if Sys.int_size >= 63 && off >= 0 && off <= Bytes.length buf - 8 then
      let w = get64u buf off in
      let w = if Sys.big_endian then w else swap64 w in
      Int64.to_int (Int64.shift_right_logical w r.shift)
    else check_bytes r buf off

  let[@inline] ok k = k >= 0

  let[@inline never] refused () =
    invalid_arg "Codec.Word.get: the check refused the record"

  let[@inline] get k = if k < 0 then refused () else k

  (* The error [Codec.get] answers for the first field [buf] cuts, or, for a
     record with no field to cut, the end of input [decode] answers for it. *)
  let error r buf off =
    if off < 0 then Errors.negative_offset off;
    let len = Bytes.length buf in
    if off <= len - r.n then
      invalid_arg "Codec.Word.error: the check accepts the record";
    let rec first i =
      if i = Array.length r.fields then
        {
          Types.at = off;
          field = [];
          kind =
            Unexpected_eof
              { expected = r.n; got = (if off > len then 0 else len - off) };
        }
      else
        let name, at, width = r.fields.(i) in
        if off > len - (at + width) then
          Result.get_error
            (short_field name ~base:off ~at:(off + at) ~width buf)
        else first (i + 1)
    in
    first 0

  let of_int : type a. (a, int) Type.eq -> int -> a = fun Type.Equal x -> x

  let[@inline always] read p (w : _ t) =
    of_int p.peq (((w lsl p.l) asr p.r) land p.m)
end
