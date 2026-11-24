(** The sink that records keys, once and untimed, for the rows that time the
    flow table alone: {!Flow_table} takes them from an array there. *)

val stride : int
(** [stride] is the number of ints of a key in {!take}: the IP version, the
    protocol, the ports, the IP length, then the four source and the four
    destination words (an IPv4 address in the first word, the others 0). *)

val add4 : src:int -> dst:int -> proto:int -> ports:int -> bytes:int -> unit
(** [add4] records the key {!Flow_table.add4} takes. *)

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
(** [add6] records the key {!Flow_table.add6} takes. *)

val take : unit -> int array
(** [take ()] is every key recorded since the last [take], {!stride} ints each,
    and forgets them. *)
