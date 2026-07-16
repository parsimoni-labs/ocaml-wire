(** Unsigned 32-bit integer.

    Backed by {!Optint.t}: an unboxed native [int] on a 64-bit host, a boxed
    [int32] where the native [int] is narrower. This is what lets a value with
    bit 31 set survive decoding on js_of_ocaml / wasm_of_ocaml, where a plain
    [int] would drop it. *)

type t = Optint.t

val pp : Format.formatter -> t -> unit
(** Pretty-printer for unsigned 32-bit values. *)

val le : bytes -> int -> t
(** [le buf off] reads a little-endian value from [buf] at offset [off]. *)

val be : bytes -> int -> t
(** [be buf off] reads a big-endian value from [buf] at offset [off]. *)

val set_le : bytes -> int -> t -> unit
(** [set_le buf off v] writes [v] as little-endian into [buf] at offset [off].
*)

val set_be : bytes -> int -> t -> unit
(** [set_be buf off v] writes [v] as big-endian into [buf] at offset [off]. *)

val to_int : t -> int
(** [to_int t] is the value as a native [int]. Exact on a 64-bit host; on a
    narrower one a value with bit 31 set does not fit and is truncated, so
    prefer {!to_int32} across such a boundary. *)

val of_int : int -> t
(** [of_int n] is the low 32 bits of [n]. *)

val to_int32 : t -> int32
(** [to_int32 t] is the value as an [int32] (its bit pattern, exact on every
    platform). *)

val of_int32 : int32 -> t
(** [of_int32 n] is the [int32] bit pattern as a 32-bit value. *)
