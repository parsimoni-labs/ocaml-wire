(** Per-field plans of the record codec.

    {!compile_field} turns one field, at the layout reached so far, into the
    reader, writer, scratch populate function and layout step that appending it
    to a record takes. It neither checks constraints nor runs actions:
    {!Codec_seal} folds those in. Internal to {!Codec}. *)

open Codec_repr

type ('a, 'r) compiled_field = {
  raw_reader : runtime -> Input_end.t -> bytes -> int -> 'a;
  raw_writer : 'r -> runtime -> bytes -> int -> int -> int;
  extra_writers : ('r -> runtime -> bytes -> int -> int -> int) list;
  field_access : field_access;
  size_delta : int;
  next_off : next_off;
  bf_after : bf_codec_state option;
  int_reader : runtime -> Input_end.t -> bytes -> int -> int;
  nested_readers : field_reader list;
  validator_off : int;
  populate : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
}
(** The plan for appending one field to a record. [validator_off] is [-1] when
    the field's offset is dynamic. *)

type layout_ctx = {
  next_off : next_off;
  fixed_prefix : int ref;
  bf : bf_codec_state option;
  field_readers : field_reader list;
  field_at : string -> (runtime -> Input_end.t -> bytes -> int -> int) option;
      (** Where an earlier field's bytes start, by name. *)
  n_fields : int;
}
(** The part of a record's state that compiling a field reads. *)

val layout_ctx_of : ('f, 'r) record -> layout_ctx
(** [layout_ctx_of r] is the layout reached by [r]. *)

val next_off_rel : next_off -> runtime -> Input_end.t -> bytes -> int -> int
(** [next_off_rel n runtime input_end buf base] is where the next field starts,
    counted from the record's [base]. *)

val note_fixed_prefix : before:next_off -> after:next_off -> int ref -> unit
(** [note_fixed_prefix ~before ~after cell] updates 3D's [sizeof(this)], the
    size of the fields before the first variable-size one. *)

val var_bytes_check_len :
  ('a -> Input_end.t -> bytes -> 'b -> int) ->
  'a ->
  bytes ->
  'b ->
  actual:int ->
  unit
(** [var_bytes_check_len size_fn runtime buf base ~actual] raises
    [Invalid_argument] unless [actual] is the size [size_fn] gives. *)

val prepend_field :
  string -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
(** [prepend_field name f runtime input_end buf base] is
    [f runtime input_end buf base] with [name] prepended to the field path of a
    parse error. *)

val prepend_field_check :
  string ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a ->
  'b ->
  'c ->
  'd ->
  'e ->
  'f
(** [prepend_field_check] is {!prepend_field} for a check over the scratch. *)

val compile_field : layout_ctx -> ('a, 'r) field -> ('a, 'r) compiled_field
(** [compile_field ctx f] is the plan for appending [f] at [ctx]. Raises
    [Invalid_argument] on a field shape the codec cannot place there. *)
