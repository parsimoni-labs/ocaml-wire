(** APID demux: a SpaceWire CCSDS Packet Transfer Protocol router counting the
    packets of each route, every field read by Wire's staged {!Wire.Codec.get}.
    The OCaml rows of [bench/routing] and [bench/apps] run it. *)

val generate_stream : int -> bytes * int * int
(** [generate_stream n] is [(buf, total, payload)]: [n] space packets of
    variable size back to back in [buf], [total] bytes in all, [payload] of them
    in packet data fields. *)

val hdr : int
(** [hdr] is the size of a space packet primary header. *)

type state
(** The read offset and the count of each route. *)

val state : unit -> state
(** [state ()] is a router at offset 0 with every count 0. *)

val reset : state -> unit
(** [reset s] sets the offset and every count of [s] back to 0. *)

val step : buf:bytes -> total_bytes:int -> state -> unit -> unit
(** [step ~buf ~total_bytes s ()] routes the packet at the offset of [s] in the
    first [total_bytes] bytes of [buf], wrapping to 0 when its header does not
    fit, and moves to the next packet. *)

val run_packets :
  buf:bytes -> total_bytes:int -> state -> int -> int * int * int * int
(** [run_packets ~buf ~total_bytes s n] runs {!step} [n] times and returns the
    counts of the four routes: housekeeping, science, diagnostics, idle. *)

val pp_counts : (int * int * int * int) Fmt.t
(** [pp_counts] prints the counts {!run_packets} returns. *)
