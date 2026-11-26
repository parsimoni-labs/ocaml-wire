(** The visitor the fold [count_tcp] of {!Stack} calls by name. *)

type t = int
(** The number of frames that passed and carry a TCP segment. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints a count. *)

val invalid : t -> t
(** [invalid n] is [n]: a frame that fails is not counted. *)

val valid : t -> ipv4_tcp_present:bool -> t
(** [valid n ~ipv4_tcp_present] is [n + 1] if [ipv4_tcp_present], [n] otherwise.
*)
