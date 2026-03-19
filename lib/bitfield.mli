(** Shared bitfield utilities. *)

val byte_size : Types.bitfield_base -> int
val total_bits : Types.bitfield_base -> int
val equal : Types.bitfield_base -> Types.bitfield_base -> bool
val read_word : Types.bitfield_base -> bytes -> int -> int
val write_word : Types.bitfield_base -> bytes -> int -> int -> unit
val extract : total:int -> bits_used:int -> width:int -> int -> int
