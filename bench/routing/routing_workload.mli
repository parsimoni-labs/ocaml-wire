(** APID demux: a SpaceWire CCSDS Packet Transfer Protocol router counting the
    packets of each route, each header read by Wire's {!Wire.Codec.read_word}
    and its fields taken with {!Wire.Codec.of_word}. The OCaml rows of
    [bench/routing] and [bench/apps] run it. *)

val generate_stream : int -> bytes * int * int
(** [generate_stream n] is [(buf, total, payload)]: space packets of variable
    size back to back in [buf], [total] bytes in all, [payload] of them in
    packet data fields. The last packet is truncated, its data field running
    past the end of [buf], and comes after [n - max 1 (n / 64)] whole ones, so
    that reading [n] headers from offset 0 wraps once. *)

val route_of_apid : int -> int
(** [route_of_apid apid] is the route of [apid], 0 to 3: housekeeping, science,
    diagnostics or idle.

    @raise Invalid_argument if [apid] is not an 11-bit APID. *)

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

val stamp : bytes -> int -> apid:int -> seq:int -> dlen:Wire.UInt16.t -> unit
(** [stamp out o ~apid ~seq ~dlen] writes at [o] in [out] the header of a
    telemetry packet with a secondary header, unsegmented, with APID [apid],
    sequence count [seq] and data length [dlen], by Wire's staged
    {!Wire.Codec.writer} of {!Space.packet_default} and those three fields. *)

val restamp : buf:bytes -> total_bytes:int -> out:bytes -> int -> unit
(** [restamp ~buf ~total_bytes ~out n] reads the headers of [n] packets as
    {!run_packets} does and {!stamp}s each at the same offset in [out], with its
    APID and data length and the next sequence count of its route, counting from
    0 modulo 2{^ 14}: the forwarding rewrite of a router that re-numbers the
    packets of each route. *)

val pp_counts : (int * int * int * int) Fmt.t
(** [pp_counts] prints the counts {!run_packets} returns. *)
