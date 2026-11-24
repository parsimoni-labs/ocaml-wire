(** The flow table of the flow benchmark: open addressing with linear probing
    over 64-byte slots, keyed by the 5-tuple, counting packets and bytes per
    flow. Its layout and hash are those of the Rust and C rows. There is one
    table, so that a consumer the scanner calls by name can reach it. *)

val init : capacity:int -> unit
(** [init ~capacity] makes the table: the least power of two at least [capacity]
    slots. *)

val reset : unit -> unit
(** [reset ()] empties the table in constant time, before a pass. *)

val add4 : src:int -> dst:int -> proto:int -> ports:int -> bytes:int -> unit
(** [add4 ~src ~dst ~proto ~ports ~bytes] counts one IPv4 packet of [bytes]
    bytes: [ports] is [(src_port lsl 16) lor dst_port], or 0 without a transport
    header. *)

val add6 :
  s0:int ->
  s1:int ->
  s2:int ->
  s3:int ->
  d0:int ->
  d1:int ->
  d2:int ->
  d3:int ->
  proto:int ->
  ports:int ->
  bytes:int ->
  unit
(** [add6] is {!add4} for IPv6, each address as four 32-bit words. *)

val summary : unit -> int * int
(** [summary ()] is the number of flows and a checksum of every live slot: its
    place, key, packets and bytes. *)
