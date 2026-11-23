(** The walk of a record without parameters.

    One pass over a record's fields that checks and reads each once, in field
    order, with a cursor ({!Types.cursor}) that a sub-codec leaves at its end.
    Sizes, gates and conditions are closures of the cursor alone, each one
    direct call. A walk that does not accept raises the error the check pass
    raises, in the same order. Internal to {!Codec}. *)

open Codec_repr

val unwalked : unit -> 'a
(** [unwalked ()] raises {!Types.Unwalked}. *)

val size :
  size:int ->
  (string * packed_scalar) list ->
  int Types.expr ->
  Types.cursor ->
  int
(** [size ~size prefix e c] is [e] over the fixed-offset fields [prefix] names,
    in the record at [c], whose frame of [size] bytes the walk has checked, as
    the layout pass evaluates a size. An unsigned big-endian field of a frame of
    at least 8 bytes is read as bits of a word of the frame, in the operator
    that reads it. [size ~size prefix e] raises [Exit] when [e] reads anything
    else. *)

type gates = (bool Types.expr * int) list
(** The gates of a record evaluated so far, in field order. *)

val no_gates : gates
(** [no_gates] is the gates of a record before its first. *)

val gate :
  size:int ->
  gates:gates ->
  (string * packed_scalar) list ->
  bool Types.expr ->
  (Types.cursor -> bool) * gates
(** [gate ~size ~gates prefix e] is [(g, gates')]: [g c] is {!size} for the
    presence condition [e], reading the answer of each gate of [gates] it holds
    from the cursor's {!Types.field-wgates} rather than evaluating it again, and
    recording its own answer there; [gates'] is [gates] with [e]. Every gate of
    a record is evaluated in field order, so a gate is evaluated before any
    other reads its answer. *)

val cond :
  size:int ->
  (string * packed_scalar) list ->
  bool Types.expr ->
  Types.cursor ->
  bool
(** [cond ~size prefix e c] is a constraint or a [where] clause over the fields
    [prefix] names, in the record at [c], read as {!size} reads them.
    [cond ~size prefix e] raises [Exit] when [e] reads anything else, a [sizeof]
    included. *)

val cond_at :
  (string * packed_scalar) list ->
  bool Types.expr ->
  size:int ->
  Types.cursor ->
  bool
(** [cond_at prefix e ~size c] is [cond ~size prefix e c], for a record whose
    frame size is known later. [cond_at prefix e] raises [Exit] where {!cond}
    does. *)

(** The kind of value a byte span reads as. *)
type _ span =
  | Slice_span : Bytesrw.Bytes.Slice.t span
  | String_span : string span

val span : 'a span -> (Types.cursor -> int) -> 'a Types.walk
(** [span kind size] walks the span of [size c] bytes at the cursor [c]. Raises
    [Parse_error] when it does not fit before the cursor's [wend]. *)

val gated : (Types.cursor -> bool) -> 'a Types.walk -> 'a option Types.walk
(** [gated present item] walks [item] when [present c] holds; an absent field
    takes no bytes. *)

val region :
  at_most:bool ->
  name:string option ->
  (Types.cursor -> int) ->
  'a Types.walk ->
  'a Types.walk
(** [region ~at_most ~name size sub] walks the sub-codec [sub] in the region of
    [size c] bytes at the cursor [c], against the region's end: it must end
    there, or before it when [at_most]. [c] goes on at the region's end. With
    [name = Some n], an error under it carries [n] in its field path when the
    cursor's [wnamed] asks for names; {!val-sub} and {!val-fixed_sub} name
    theirs alike. *)

val sub : name:string option -> 'a Types.walk -> 'a Types.walk
(** [sub ~name w] walks the sub-codec [w] at the cursor, which it leaves at its
    end. *)

val fixed_sub :
  ?at_most:bool ->
  ?size:int ->
  name:string option ->
  int ->
  'a Types.walk ->
  'a Types.walk
(** [fixed_sub ~at_most ~size ~name off w] walks the sub-codec [w] at offset
    [off] of the record at the cursor, leaving the cursor where it was. With
    [size], [w] walks in a region of [size] bytes, which it must not pass, nor
    fall short of unless [at_most] (default [true]). *)

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
  fail:(Input_end.t -> bytes -> int -> unit) ->
  'full ->
  ('full, 'r) walk option ->
  ('r Types.walk * (string * (Types.cursor -> int)) list) option
(** [record ~n_params ~min_size ~prefix ~where ~fail make w] is the walk of a
    sealed record of at least [min_size] bytes, built by [make] from the fields
    [w] reads: its frame, the checks of its fixed-offset fields, each field in
    order, then its [where] clause. When a check of a fixed-offset field fails,
    or an expression raises, [fail input_end buf off], the record's check pass,
    raises the error. Its [wskip] makes the same checks and reads only the
    fields they read. With it, where the walk finds each field it walks or
    gates, as {!Codec_repr.compiled}'s [places]. [None] when the record has
    parameters, no walk, more than 32 fields, or a [where] clause reading
    anything but the fields [prefix] names. *)

val decode : 'r Types.walk -> (bytes -> int -> 'r) -> bytes -> int -> 'r
(** [decode walk general buf off] is the record [walk] reads at [off], or
    [general buf off] when [walk] raises {!Types.Unwalked}. A walk that fails is
    walked again with names on, for the field path of its error. *)

val accepts : 'r Types.walk -> bytes -> int -> Input_end.t -> bool
(** [accepts walk buf off stop] is [true] iff the [wskip] of [walk] accepts the
    record at [off] in the bytes of [buf] before [stop]. *)
