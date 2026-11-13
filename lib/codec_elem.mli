(** Access below the field level for the record codec.

    Bitfield base words, the elements of arrays, repeats and casetypes, and the
    walks that collect a codec's parameters and action locals. Internal to
    {!Codec}. *)

val bf_base_byte_size : Types.bitfield_base -> int
(** [bf_base_byte_size] is {!Bitfield.byte_size}. *)

val bf_base_total_bits : Types.bitfield_base -> int
(** [bf_base_total_bits] is {!Bitfield.total_bits}. *)

val bf_base_equal : Types.bitfield_base -> Types.bitfield_base -> bool
(** [bf_base_equal] is {!Bitfield.equal}. *)

val equal_bit_order : Types.bit_order -> Types.bit_order -> bool
(** [equal_bit_order a b] is [true] iff [a] and [b] are the same order. *)

val build_bf_reader :
  Types.bitfield_base -> int -> int -> int -> bytes -> int -> int
(** [build_bf_reader base byte_off shift width buf off] reads a bitfield from a
    base word the caller has checked to be in [buf]. *)

val build_bf_accessor_reader :
  Types.bitfield_base -> int -> int -> int -> bytes -> int -> int
(** [build_bf_accessor_reader] is {!build_bf_reader} with the base word's span
    checked first. *)

val build_bf_writer :
  Types.bitfield_base -> int -> int -> int -> bytes -> int -> int -> unit
(** [build_bf_writer base byte_off shift width buf off v] ors the range-checked
    [v] into a base word the encoder has cleared. *)

val build_bf_accessor_writer :
  Types.bitfield_base -> int -> int -> int -> bytes -> int -> int -> unit
(** [build_bf_accessor_writer] replaces a bitfield in place for {!Codec.set}: it
    checks the value's range and the base word's span before writing. *)

val build_bf_clear : Types.bitfield_base -> int -> bytes -> int -> unit
(** [build_bf_clear base byte_off buf off] zeroes the base word at
    [off + byte_off]. *)

val extend_scope :
  string ->
  'a ->
  (string option * 'a) list ->
  (string option * 'a) list ->
  (string option * 'a) list
(** [extend_scope name own nested prev] adds field [name] and the fields its
    sub-codec brings ([nested]) to the scope [prev], newest first. An entry that
    loses its name to a newer one keeps its place, which numbers a slot, and has
    no name. *)

val build_idx : (string option * 'a) list -> string -> int
(** [build_idx readers name] is the slot of [name], in declaration order. Raises
    [Invalid_argument] on an unbound name. *)

val action_vars : string list -> Types.action_stmt -> string list
(** [action_vars acc s] adds the local variables [s] declares to [acc]. *)

val iter_param_refs_fields :
  (Param.packed -> unit) -> Types.field list -> bool Types.expr option -> unit
(** [iter_param_refs_fields f fields where] calls [f] on every parameter the
    fields and [where] refer to, including those of embedded codecs. *)

val param_use : Types.field list -> Codec_repr.param_use
(** [param_use fields] is how the expressions of [fields], and of the codecs
    they embed, read the output parameters their actions assign. *)

val tag_byte_size : 'a Types.typ -> int
(** [tag_byte_size t] is the width of the casetype tag [t]. *)

val read_elem :
  'a Types.typ -> Codec_repr.runtime -> Input_end.t -> bytes -> int -> 'a
(** [read_elem t runtime input_end buf off] reads one element of type [t] at
    [off]. *)

val write_elem :
  'a Types.typ -> Codec_repr.runtime -> bytes -> int -> 'a -> unit
(** [write_elem t runtime buf off v] writes [v] as one element of type [t] at
    [off]. *)

val elem_size_of :
  'a Types.typ -> Codec_repr.runtime -> Input_end.t -> bytes -> int -> int
(** [elem_size_of t runtime input_end buf off] is the wire size of the element
    of type [t] at [off]. *)

val unbound_where_ref : string -> 'a
(** [unbound_where_ref name] raises [Invalid_argument]: a [where] clause refers
    to [name], which no field or parameter binds. *)

val iter_refs :
  param:(Param.packed -> unit) ->
  field:(string -> unit) ->
  'a Types.expr ->
  unit
(** [iter_refs ~param ~field e] calls [param] on every parameter and [field] on
    every field name [e] reads. *)

val unbound_check_ref : string -> 'a
(** [unbound_check_ref name] raises [Invalid_argument]: a field constraint or
    action refers to [name], which no field binds. *)

val used_idx : ('a, unit) Hashtbl.t -> ('b -> 'a) -> 'b -> 'a
(** [used_idx used idx name] is [idx name], after noting the slot in [used]. *)

val mask32 : int
(** [mask32] is the low 32 bits set, built from an [int64] literal so a 31-bit
    [int] build does not truncate it. *)
