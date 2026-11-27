(** Why a decoder refuses its input.

    A codec and a module generated from it with [Wire_ocaml] answer the same
    error for the same bytes, a value of type {!t}. [Wire.Error] is this module.
*)

(** The type for the predicates a {!constructor-Constraint_failed} comes from: a
    field's [~self_constraint] or [~constraint_] ([Field]), a codec's or a
    type's [~where] ([Where]), a field's [~action] ([Action]), or a
    [byte_array_where] per-byte refinement ([Per_byte]). *)
type predicate = Where | Field | Action | Per_byte

(** The type for the reasons input is refused. {!constructor-Unexpected_eof}
    counts bytes, not buffer positions: [expected] is how many the value needed
    and [got] how many were left where it starts, so both are the same whatever
    offset the frame is read at. For {!constructor-Constraint_failed}, [value]
    is the offending field's value for a single-field constraint and [None] for
    a cross-field or where predicate. *)
type kind =
  | Unexpected_eof of { expected : int; got : int }
  | Trailing_bytes of int
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | Missing_terminator
  | Non_zero_padding
  | Value_out_of_range of { value : int64 }
  | Zero_divisor
  | Constraint_failed of { which : predicate; value : int64 option }

type t = { at : int; field : string list; kind : kind }
(** The type for errors. [at] is the absolute byte offset of the failing field,
    [field] the path of field names to it, root to leaf, empty at a top-level or
    anonymous position. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] prints [e] with its field path and offset, as in
    [len@4: unexpected EOF: expected 2 bytes, got 1]. *)

val pp_kind : Format.formatter -> kind -> unit
(** [pp_kind ppf k] prints [k] without a location. *)

val equal : t -> t -> bool
(** [equal e e'] is [true] iff [e] and [e'] are the same error. *)

val compare : t -> t -> int
(** [compare] is a total order on errors, compatible with {!equal}. *)
