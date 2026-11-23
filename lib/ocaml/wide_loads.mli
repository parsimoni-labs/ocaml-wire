(** The 64-bit words a prefix loads once. Internal to {!Wire_ocaml}. *)

open Planner

val in_word : unboxed:bool -> field -> bool
(** [in_word ~unboxed f] is [true] when [f] is taken out of a 64-bit word that
    holds it; a byte is not, when the position is [unboxed]. *)

val words : unboxed:bool -> size:int -> field list -> int list
(** [words ~unboxed ~size fields] is the offsets of the 64-bit words of a prefix
    of [size] bytes worth loading once: those holding two or more of [fields].
*)

val shared_words :
  held:int list ->
  lo:int ->
  hi:int ->
  later:field list ->
  field list ->
  int list
(** [shared_words ~held ~lo ~hi ~later live] is the words a prefix loads with
    [Boxed]: each holds two or more fields of [live] and [later] that no word of
    [held] holds, and lies between [lo] and [hi]. *)

val following : Sset.t -> until:int -> int -> node list -> field list
(** [following live ~until at nodes] is the [live] fields that start before
    [until] in the prefixes of [nodes], at constant offsets from [at]. *)
