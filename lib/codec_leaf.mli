(** Leaf readers and writers of the record codec.

    One field type read or written at a fixed offset: the scalar and byte-span
    readers and encoders, the refinement and enum checks they run, and the
    bytes-only fast paths of {!Codec.get} and {!Codec.set}. Internal to
    {!Codec}. *)

val blit_string_exact : int -> bytes -> int -> string -> int
(** [blit_string_exact n buf off s] writes [s] at [off] and returns [off + n].
    Raises [Invalid_argument] unless [String.length s = n]. *)

val blit_slice_exact : int -> bytes -> int -> Bytesrw.Bytes.Slice.t -> int
(** [blit_slice_exact n buf off src] is {!blit_string_exact} for a slice. *)

val bytes_from : Input_end.t -> int -> int
(** [bytes_from input_end off] is the number of input bytes from [off] on, the
    [got] of an end-of-input error; [0] outside the input. *)

val check_read_bounds : Input_end.t -> at:int -> width:int -> unit
(** [check_read_bounds input_end ~at ~width] raises an end-of-input
    {!Types.Parse_error} unless the [width] bytes at [at] are in the input. *)

val zeroterm_nul_pos : bytes -> first:int -> limit:int -> int
(** [zeroterm_nul_pos buf ~first ~limit] is the index of the first NUL in
    [\[first, limit)]; raises {!Types.Parse_error} when there is none. *)

val refined_span_check :
  elt_var:string ->
  cond:bool Types.expr ->
  bytes ->
  first:int ->
  len:int ->
  unit
(** [refined_span_check ~elt_var ~cond buf ~first ~len] raises a per-byte
    constraint error at the first byte of the span that fails [cond]. *)

val refined_span_reader :
  elt_var:string ->
  cond:bool Types.expr ->
  bytes ->
  first:int ->
  len:int ->
  string
(** [refined_span_reader] runs {!refined_span_check} and copies the span. *)

val all_zeros_check : bytes -> first:int -> len:int -> unit
(** [all_zeros_check buf ~first ~len] raises a non-zero-padding error at the
    first non-NUL byte of the span. *)

val all_zeros_reader : bytes -> first:int -> len:int -> string
(** [all_zeros_reader] runs {!all_zeros_check} and copies the span. *)

val field_shape : 'a Types.typ -> string
(** [field_shape t] names [t] in prose for an "unsupported shape" refusal. *)

val build_field_encoder_ctx :
  'a Types.typ -> Types.eval_ctx -> bytes -> int -> 'a -> int
(** [build_field_encoder_ctx t runtime buf off v] writes [v] as a value of type
    [t] at [off] and returns the offset after it. Raises [Invalid_argument] when
    [t] has no writer. *)

val build_field_encoder : 'a Types.typ -> bytes -> int -> 'a -> int
(** [build_field_encoder t buf off v] is {!build_field_encoder_ctx} with no
    parameter bindings. *)

val enum_check :
  base:'a Types.typ ->
  to_int:('b -> int) ->
  (string * int) list ->
  bool ->
  at:int ->
  'b ->
  'b
(** [enum_check ~base ~to_int cases closed ~at v] is [v] when it names a case of
    a closed enum, or any [v] of an open one. Raises an invalid enum error at
    [at] otherwise. *)

val enum_encode_check :
  name:string ->
  cases:(string * int) list ->
  closed:bool ->
  to_int:('a -> int) ->
  'a ->
  unit
(** [enum_encode_check] is the encode-side twin of {!enum_check}; it raises
    [Invalid_argument] on a value a closed enum does not list. *)

val build_field_reader_ctx :
  'a Types.typ -> int -> Types.eval_ctx -> Input_end.t -> bytes -> int -> 'a
(** [build_field_reader_ctx t off runtime input_end buf base] reads a value of
    type [t] at [base + off] with no bounds check: the caller has checked the
    frame. Raises [Invalid_argument] when [t] has no reader. *)

val build_immediate_reader : 'a Types.typ -> int -> (bytes -> int -> 'a) option
(** [build_immediate_reader t off] is a reader that needs no evaluation context,
    or [None] when the layout of [t] may consult one. *)

val build_immediate_encoder : 'a Types.typ -> (bytes -> int -> 'a -> int) option
(** [build_immediate_encoder t] is the writer twin of {!build_immediate_reader}.
*)
