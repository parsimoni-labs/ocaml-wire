(* Where the bytes handed to a parse stop.

   The end of the buffer at the top level, but a nested region hands its value
   fewer bytes than the buffer holds, and a length field the data places can
   land past the region while still lying inside the buffer. Reading it there
   sizes the value from bytes the parse was never given, so the limit rides
   down every buffer walk instead of being re-derived from [Bytes.length]
   wherever a read is checked. *)

type t = int

let of_bytes = Bytes.length
let narrow t n = Int.max 0 (Int.min t n)
let to_int t = t
let pp = Fmt.int
