(** The expressions and the checking walk of a plan, written as Rust.

    The walk follows {!Wire_ocaml.Plan} and the decisions it takes, as the folds
    of [Wire_ocaml] do. Internal to {!Wire_rust}. *)

module P = Wire_ocaml.Plan

exception Unsupported of string
(** Raised with the reason when a codec has a shape Rust cannot spell. *)

val fail_unsupported : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_unsupported fmt] raises {!Unsupported} with the formatted reason. *)

val ident : string -> string
(** [ident name] is {!Wire.Private.Names.rust_ident}. *)

val rust_string : string -> string
(** [rust_string s] is the Rust string literal of [s]. Raises {!Unsupported}
    when [s] is not UTF-8. *)

val doc_string : string -> string
(** [doc_string s] is [s] on one line, its bytes escaped, for a comment. *)

(** {1 Expressions} *)

val int_lit : int -> string
(** [int_lit n] is the [i64] literal of [n]. *)

val expr : P.Scope.t -> 'a Wire.Private.Types.expr -> string
(** [expr scope e] is [e] over the plan's variables, each an [i64]: an integer
    as an [i64], a condition as a [bool], compared on [u64] where [scope]
    decides an operand can leave the native int. Raises {!Unsupported} for an
    expression Rust cannot spell. *)

val size_expr : P.Scope.t -> int Wire.Private.Types.expr -> string
(** [size_expr scope e] is the size [e] as an [i64]: one that can leave the
    native int is computed on [u64] and saturated by [sat] at the largest [i64],
    past any buffer. *)

val shift_prelude : string
(** [shift_prelude] defines [lsl] and [lsr], OCaml's shifts, which {!expr} calls
    where Rust's shifts do not agree with them. *)

(** {1 Types and loads} *)

val endian_suffix : Wire.Private.Types.endian -> string
(** [endian_suffix e] is ["be"] or ["le"]. *)

val base_endian : Wire.Private.Types.bitfield_base -> Wire.Private.Types.endian
(** [base_endian b] is the byte order of the word [b]. *)

val base_type : Wire.Private.Types.bitfield_base -> string
(** [base_type b] is the Rust type of the word [b]. *)

val bits_type : int -> string
(** [bits_type width] is the narrowest unsigned Rust type of [width] bits. *)

val type_bits : int -> int
(** [type_bits width] is the bits of {!bits_type}[ width]. *)

val store_type : Wire.Private.Layout.store -> string
(** [store_type s] is the Rust type of a field stored as [s]. *)

val load : string -> int -> int -> Wire.Private.Types.endian -> string -> string
(** [load b k bytes endian ty] loads the [ty] of [bytes] bytes at [k] of the
    array [b]. *)

val mask : int -> string
(** [mask width] is the mask of the [width] low bits. *)

val read_at : string -> int -> Wire.Private.Layout.store -> string
(** [read_at b k s] is the value of a field stored as [s] whose bytes start at
    [k] of the array [b], in its Rust type. *)

val pp_lines : string list Fmt.t
(** [pp_lines] prints each string on a line. *)

(** {1 The walk} *)

val output_type : P.output -> string
(** [output_type o] is the Rust type of the output [o]. *)

val output_value : P.output -> string
(** [output_value o] is the variable of [o] as its Rust type. *)

val body :
  depth:int ->
  P.t ->
  P.output list ->
  frame:string ->
  finish:string list ->
  string * bool
(** [body ~depth plan outputs ~frame ~finish] is the walk of [plan] over the
    slice [frame], indented [depth] levels, binding [outputs], and whether it
    binds a slot. The walk returns [None] where the record is bad and runs the
    lines [finish] where it is good. Raises {!Unsupported} as {!expr} does. *)
