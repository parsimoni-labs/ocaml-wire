(** The sink of the rows that time the parse without the flow table: it folds
    each packet's key, the arguments {!Flow_table.add4} and {!Flow_table.add6}
    take, into one sum, as [Sum] of cargo/src/flow.rs does, so a parse row costs
    its parse and a fold of a few instructions. *)

val reset : unit -> unit
(** [reset ()] starts a pass: no key, a zero sum. *)

val key4 : src:int -> dst:int -> proto:int -> ports:int -> bytes:int -> int
(** [key4] is the value {!add4} folds into the sum. *)

val key6 :
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
  int
(** [key6] is the value {!add6} folds into the sum. *)

val add_pass : keys:int -> sum:int -> unit
(** [add_pass ~keys ~sum] folds [keys] keys whose values sum to [sum], for a row
    that keeps its fold in its own accumulator. *)

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
