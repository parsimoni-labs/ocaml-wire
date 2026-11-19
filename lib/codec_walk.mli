(** The walk of a record without parameters.

    One pass over a record's fields that checks and reads each once, in field
    order, with a cursor ({!Types.cursor}) that a sub-codec leaves at its end.
    Sizes, gates and conditions are closures of the cursor alone, each one
    direct call. A walk raises {!Types.Unwalked} where it does not accept, and
    the decode then takes the general path for its answer. Internal to {!Codec}.
*)

open Codec_repr

val unwalked : unit -> 'a
(** [unwalked ()] raises {!Types.Unwalked}. *)

val size :
  (string * packed_scalar) list -> int Types.expr -> Types.cursor -> int
(** [size prefix e c] is [e] over the fixed-offset fields [prefix] names, in the
    record at [c], as the layout pass evaluates a size. [size prefix e] raises
    [Exit] when [e] reads anything else. *)

val gate :
  (string * packed_scalar) list -> bool Types.expr -> Types.cursor -> bool
(** [gate prefix e c] is {!size} for a presence condition. *)

val cond :
  (string * packed_scalar) list -> bool Types.expr -> Types.cursor -> bool
(** [cond prefix e c] is a constraint or a [where] clause over the fields
    [prefix] names, in the record at [c]. [cond prefix e] raises [Exit] when [e]
    reads anything else, a [sizeof] included. *)

(** The kind of value a byte span reads as. *)
type _ span =
  | Slice_span : Bytesrw.Bytes.Slice.t span
  | String_span : string span

val span : 'a span -> (Types.cursor -> int) -> Types.cursor -> 'a
(** [span kind size c] reads the span of [size c] bytes at [c]. *)

val gated :
  (Types.cursor -> bool) -> (Types.cursor -> 'a) -> Types.cursor -> 'a option
(** [gated present item c] reads [item c] when [present c] holds; an absent
    field takes no bytes. *)

val region :
  at_most:bool ->
  (Types.cursor -> int) ->
  (Types.cursor -> 'a) ->
  Types.cursor ->
  'a
(** [region ~at_most size sub c] walks the sub-codec [sub] in the region of
    [size c] bytes at [c]: it must end at the region's end, or before it when
    [at_most]. [c] goes on at the region's end. *)

val sub : (Types.cursor -> 'a) -> Types.cursor -> 'a
(** [sub w c] walks the sub-codec [w] at [c], which it leaves at its end. *)

val fixed_sub : int -> (Types.cursor -> 'a) -> Types.cursor -> 'a
(** [fixed_sub off w c] walks the fixed-size sub-codec [w] at offset [off] of
    the record at [c], leaving [c] where it was. *)

(** How the walk reads each field of a record, in field order. *)
type (_, _) readers =
  | Nil : ('r, 'r) readers
  | Cons :
      (Types.cursor -> 'a) * ('rest, 'result) readers
      -> ('a -> 'rest, 'result) readers

val readers : ('full, 'result) walk_readers -> ('full, 'result) readers
(** [readers r] is how the walk reads the fields [r] describes. *)

val stage : 'mid -> ('mid, 'result) readers -> (Types.cursor -> 'result) option
(** [stage f readers] reads the fields of the record at the cursor's [wbase] and
    applies [f] to them, with a direct call per field and one saturated call of
    [f]. [None] past 32 fields. *)

val record :
  n_params:int ->
  min_size:int ->
  prefix:(string * packed_scalar) list ->
  where:bool Types.expr option ->
  'full ->
  ('full, 'r) walk option ->
  (Types.cursor -> 'r) option
(** [record ~n_params ~min_size ~prefix ~where make w] is the walk of a sealed
    record of at least [min_size] bytes, built by [make] from the fields [w]
    reads: its frame, then the checks of its fixed-offset fields and of its
    [where] clause, then each field in order. [None] when the record has
    parameters, no walk, more than 32 fields, or a [where] clause reading
    anything but the fields [prefix] names. *)

val decode : (Types.cursor -> 'r) -> (bytes -> int -> 'r) -> bytes -> int -> 'r
(** [decode walk general buf off] is the record [walk] reads at [off], or
    [general buf off] when [walk] does not accept it. *)
