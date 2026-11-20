module Defect.WordUnchecked.Buggy

(* 6a0fce0b "codec: check a bitfield accessor's word is in the buffer",
   before the fix: Codec.get on a bitfield whose base is a u32 assembled the
   word from unchecked byte reads at the caller's offset [off + byte_off], so
   a frame shorter than the word read the bytes after it. *)


noeq type outcome =
  | Value
  | Raised (* Invalid_argument: a bad offset is API misuse *)
  | Fault  (* a read outside the buffer *)

(* [n] unchecked byte reads at [o] in a buffer of [len] bytes. *)
let unchecked_word (len:nat) (o:int) (n:nat) : outcome =
  if o < 0 || o + n > len then Fault else Value

let get (len:nat) (off byte_off:int) (n:nat) : outcome = unchecked_word len (off + byte_off) n

(* A 4-byte word at offset 2 of a 4-byte buffer. *)
let _ = assert_norm (get 4 2 0 4 == Fault)

(* An accessor never reads outside the buffer. *)
let get_safe (len:nat) (off byte_off:int) (n:nat) : Lemma (~ (Fault? (get len off byte_off n))) = ()
