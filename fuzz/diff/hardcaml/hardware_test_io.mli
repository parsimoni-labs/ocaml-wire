(** Bounded frame checks and bus packing shared by hardware lanes. *)

val bounded : bytes -> int -> int -> bool
(** Whether a logical frame lies within its physical byte buffer. *)

val word :
  capacity:int -> padding:(int -> int) -> bytes -> int -> int -> Hardcaml.Bits.t
(** [word ~capacity ~padding buf off len] places the bounded frame in the low
    bytes and fills unused physical lanes with [padding]. Invalid physical
    bounds or a length beyond [capacity] raise [Invalid_argument]. *)

val equal_fields : (string * int) list -> (string * int) list -> bool
(** Compare named native scalar snapshots. *)
