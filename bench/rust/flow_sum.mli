(** The sink of the rows that time the parse without the flow table: it folds
    each packet's key, the arguments {!Flow_table.add4} and {!Flow_table.add6}
    take, into one sum, as [Sum] of cargo/src/flow.rs does, so a parse row costs
    its parse and a fold of a few instructions. *)

val reset : unit -> unit
(** [reset ()] starts a pass: no key, a zero sum. *)

val add4 : src:int -> dst:int -> proto:int -> ports:int -> bytes:int -> unit
(** [add4] folds the key {!Flow_table.add4} takes. *)

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
(** [add6] folds the key {!Flow_table.add6} takes. *)

val summary : unit -> int * int
(** [summary ()] is the number of keys folded since {!reset} and their sum. *)
