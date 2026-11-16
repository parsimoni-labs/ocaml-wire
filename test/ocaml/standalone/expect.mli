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
  Wire_runtime.Errors.error_kind ->
  (unit -> 'a) ->
  unit
(** [parse_error what ?field ~at kind f] checks that [f ()] raises a parse error
    of [kind] at [at], naming [field] (default none). *)

val invalid : string -> (unit -> 'a) -> unit
(** [invalid what f] checks that [f ()] raises [Invalid_argument]. *)

val round_trip :
  string ->
  size:int ->
  decode:(bytes -> int -> 'r) ->
  encode:('r -> bytes -> int -> unit) ->
  big_decode:(Wire_runtime.Unchecked.Bigstring.t -> int -> 'b) ->
  big_encode:('b -> Wire_runtime.Unchecked.Bigstring.t -> int -> unit) ->
  int list ->
  unit
(** [round_trip what ~size ~decode ~encode ~big_decode ~big_encode good] checks
    that the record [good] holds decodes and encodes back to [good] on bytes and
    on a bigarray, and that a buffer one byte short or a negative offset is
    refused. *)

val sources : unit -> unit
(** [sources ()] checks that no generated source in the current directory
    mentions [Wire.]. *)

val exit : unit -> unit
(** [exit ()] exits with status 1 if a check failed, and prints [ok] if none
    did. *)
