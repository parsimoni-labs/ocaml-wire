(** What the tests of generated code embedded without wire check, with nothing
    but wire.runtime. *)

val check : string -> bool -> unit
(** [check what ok] records a failure named [what] unless [ok]. *)

val bytes_of : int list -> bytes
(** [bytes_of l] is the bytes [l]. *)

val parse_error :
  string ->
  ?field:string list ->
  at:int ->
  Wire_runtime.Error.kind ->
  ('a, Wire_runtime.Error.t) result ->
  unit
(** [parse_error what ?field ~at kind r] checks that [r] is an error of [kind]
    at [at], naming [field] (default none). *)

val invalid : string -> (unit -> 'a) -> unit
(** [invalid what f] checks that [f ()] raises [Invalid_argument]. *)

val round_trip :
  string ->
  size:int ->
  decode:(bytes -> int -> ('r, Wire_runtime.Error.t) result) ->
  encode:('r -> bytes -> int -> unit) ->
  int list ->
  unit
(** [round_trip what ~size ~decode ~encode good] checks that the record [good]
    holds decodes and encodes back to [good], and that a buffer one byte short
    or a negative offset is refused. *)

val big_round_trip :
  string ->
  size:int ->
  decode:
    (Wire_runtime.Unchecked.Bigstring.t ->
    int ->
    ('r, Wire_runtime.Error.t) result) ->
  encode:('r -> Wire_runtime.Unchecked.Bigstring.t -> int -> unit) ->
  int list ->
  unit
(** [big_round_trip what ~size ~decode ~encode good] checks that the record
    [good] holds decodes and encodes back to [good] on a bigarray. *)

val sources : unit -> unit
(** [sources ()] checks that no generated source in the current directory
    mentions [Wire.]. *)

val exit : unit -> unit
(** [exit ()] exits with status 1 if a check failed, and prints [ok] if none
    did. *)
