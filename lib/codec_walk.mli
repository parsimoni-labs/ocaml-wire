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
  (string * packed_scalar) list -> int Types.expr -> Types.cursor -> int
(** [size prefix e c] is [e] over the fixed-offset fields [prefix] names, in the
    record at [c], as the layout pass evaluates a size. [size prefix e] raises
    [Exit] when [e] reads anything else. *)

type gates = (bool Types.expr * int) list
(** The gates of a record evaluated so far, in field order. *)

val no_gates : gates
(** [no_gates] is the gates of a record before its first. *)

val gate :
  gates:gates ->
  (string * packed_scalar) list ->
  bool Types.expr ->
  (Types.cursor -> bool) * gates
(** [gate ~gates prefix e] is [(g, gates')]: [g c] is {!size} for the presence
    condition [e], reading the answer of each gate of [gates] it holds from the
    cursor's {!Types.field-wgates} rather than evaluating it again, and
    recording its own answer there; [gates'] is [gates] with [e]. Every gate of
    a record is evaluated in field order, so a gate is evaluated before any
    other reads its answer. *)

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
(** [span kind size c] reads the span of [size c] bytes at [c]. Raises
    [Parse_error] when it does not fit before the cursor's [wend]. *)

val gated :
  (Types.cursor -> bool) -> (Types.cursor -> 'a) -> Types.cursor -> 'a option
(** [gated present item c] reads [item c] when [present c] holds; an absent
    field takes no bytes. *)

val region :
  at_most:bool ->
  name:string option ->
  (Types.cursor -> int) ->
  (Types.cursor -> 'a) ->
  Types.cursor ->
  'a
(** [region ~at_most ~name size sub c] walks the sub-codec [sub] in the region
    of [size c] bytes at [c], against the region's end: it must end there, or
    before it when [at_most]. [c] goes on at the region's end. With
    [name = Some n], an error under it carries [n] in its field path when the
    cursor's [wnamed] asks for names; {!val-sub} and {!val-fixed_sub} name
    theirs alike. *)

val sub : name:string option -> (Types.cursor -> 'a) -> Types.cursor -> 'a
(** [sub ~name w c] walks the sub-codec [w] at [c], which it leaves at its end.
*)

val fixed_sub :
  name:string option -> int -> (Types.cursor -> 'a) -> Types.cursor -> 'a
(** [fixed_sub ~name off w c] walks the fixed-size sub-codec [w] at offset [off]
    of the record at [c], leaving [c] where it was. *)

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
  (Types.cursor -> 'r) option
(** [record ~n_params ~min_size ~prefix ~where ~fail make w] is the walk of a
    sealed record of at least [min_size] bytes, built by [make] from the fields
    [w] reads: its frame, the checks of its fixed-offset fields, each field in
    order, then its [where] clause. When a check of a fixed-offset field fails,
    or an expression raises, [fail input_end buf off], the record's check pass,
    raises the error. [None] when the record has parameters, no walk, more than
    32 fields, or a [where] clause reading anything but the fields [prefix]
    names. *)

val decode : (Types.cursor -> 'r) -> (bytes -> int -> 'r) -> bytes -> int -> 'r
(** [decode walk general buf off] is the record [walk] reads at [off], or
    [general buf off] when [walk] raises {!Types.Unwalked}. A walk that fails is
    walked again with names on, for the field path of its error. *)
