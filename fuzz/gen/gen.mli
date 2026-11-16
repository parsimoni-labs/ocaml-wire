(** The generator type and the helpers every leaf is built from.

    An ['a t] pairs the codec it tests with the Alcobar streams a property
    reads: values with their canonical bytes, arbitrary bytes, bytes at the
    boundary of each constraint, and hostile values encode has to refuse. *)

type env_strategy = {
  positive : Wire.Param.env -> Wire.Param.env;
      (** Binds every [Param.input] for the positive stream. *)
  fuzz : (Wire.Param.env -> Wire.Param.env) Alcobar.gen;
      (** Binder gen for the random and adversarial streams. *)
}
(** How a codec that references [Param.input] gets its env. *)

(** One addressable field of a record gen, with its field type packed away. *)
type 'r field_check =
  | Field_check : {
      name : string;
      field : ('a, 'r) Wire.Codec.field;
      proj : 'r -> 'a;
      equal : 'a -> 'a -> bool;
    }
      -> 'r field_check

(* Declared last so that, as in the implementation, an unannotated [g.positive]
   resolves to this record rather than to [env_strategy]. *)
type 'a t = {
  codec : 'a Wire.Codec.t;  (** The codec under test. *)
  positive : ('a * bytes) Alcobar.gen;
      (** Values together with bytes that decode to them. *)
  random : bytes Alcobar.gen;
      (** Arbitrary bytes decode must handle without raising. *)
  adversarial : bytes Alcobar.gen;
      (** Bytes at the boundary of every constraint the codec checks. *)
  equal : 'a -> 'a -> bool;  (** The value equality. *)
  typ : 'a Wire.typ;
      (** The typ this gen contributes when it is a field of a parent record:
          the bare typ for a leaf, a sub-codec wrap for a record. *)
  env : env_strategy option;
      (** How to bind the codec's [Param.input]s, when it has any. *)
  adversarial_value : 'a Alcobar.gen option;
      (** Well-typed values that violate a refinement decode enforces, or [None]
          where the shape has nothing left to violate. *)
  fields : 'a field_check list;
      (** The record's addressable fields, [[]] when it has none. *)
}
(** A fuzz generator paired with the codec it tests. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp] prints a gen as the bit diagram of its codec. *)

val bytes_of_string : string -> bytes
(** [bytes_of_string s] is [s] as bytes, without copying. *)

val string_of_bytes : bytes -> string
(** [string_of_bytes b] is [b] as a string, without copying. *)

val corpus_generation_mode : unit -> bool
(** [corpus_generation_mode ()] is [true] when Alcobar is generating an AFL seed
    corpus. *)

val file_input_mode : unit -> bool
(** [file_input_mode ()] is [true] when the executable is processing one
    AFL-provided file input. *)

val afl_mode : unit -> bool
(** [afl_mode ()] is [true] in either of the two AFL modes. *)

val slice_of_string : string -> Bytesrw.Bytes.Slice.t
(** [slice_of_string s] is a slice over a copy of [s]. *)

val string_of_slice : Bytesrw.Bytes.Slice.t -> string
(** [string_of_slice s] is the bytes [s] spans. *)

val slice_equal : Bytesrw.Bytes.Slice.t -> Bytesrw.Bytes.Slice.t -> bool
(** [slice_equal a b] compares the bytes [a] and [b] span. *)

val bytes_of_octets : int list -> bytes
(** [bytes_of_octets xs] is one byte per element of [xs], masked to 8 bits. *)

val bytes_fixed : int -> bytes Alcobar.gen
(** [bytes_fixed n] generates arbitrary byte strings of length [n]. *)

val bytes_any : bytes Alcobar.gen
(** [bytes_any] generates arbitrary byte strings. *)

val leaf_codec_name : string
(** [leaf_codec_name] names the one-field record {!codec_of_typ} builds. *)

val leaf_slot_name : string
(** [leaf_slot_name] names the single field of that record. *)

val leaf_field : 'a Wire.typ -> ('a, 'a) Wire.Codec.field
(** [leaf_field typ] is the single field of that record. *)

val codec_of_typ : 'a Wire.typ -> 'a Wire.Codec.t
(** [codec_of_typ typ] wraps [typ] in a one-field record codec. *)

val slot : string -> 'a Wire.Field.t -> ('r -> 'a) -> 'r field_check
(** [slot name f proj] is the checker for field [f] of a hand-built record,
    projected with [proj]. *)

val pair_fields :
  string * 'a Wire.Field.t ->
  string * 'b Wire.Field.t ->
  ('a * 'b) field_check list
(** [pair_fields a b] are the checkers of a record projected with [fst] and
    [snd]. *)

val triple_fields :
  string * 'a Wire.Field.t ->
  string * 'b Wire.Field.t ->
  string * 'c Wire.Field.t ->
  ('a * 'b * 'c) field_check list
(** [triple_fields a b c] is {!pair_fields} for a three-field record. *)

val encode_via_codec : 'a Wire.Codec.t -> 'a -> bytes
(** [encode_via_codec codec v] is the canonical encoding of [v]: a buffer sized
    by [size_of_value] and filled by [encode]. *)

val unlisted_enum_gen : mask:int -> int list -> int Alcobar.gen
(** [unlisted_enum_gen ~mask valid] generates numbers under [mask] that are not
    in [valid]. *)

val above_bit_width : int -> int Alcobar.gen
(** [above_bit_width width] generates values with a bit set above [width]. *)

val above_byte_width : int -> Wire.Private.UInt63.t Alcobar.gen
(** [above_byte_width size] generates values needing more than [size] bytes. *)

val byte_lengths_around : int -> string Alcobar.gen
(** [byte_lengths_around n] generates byte strings of the lengths around [n],
    only one of which ([n]) a span of declared size [n] can encode. *)

val leaf :
  equal:('a -> 'a -> bool) ->
  typ:'a Wire.typ ->
  value_gen:'a Alcobar.gen ->
  random:bytes Alcobar.gen ->
  adversarial:'a Alcobar.gen ->
  'a t
(** [leaf ~equal ~typ ~value_gen ~random ~adversarial] is the gen of [typ] whose
    positives and adversarial bytes are the encodings of [value_gen] and
    [adversarial] values. *)

val enum_like :
  typ:'a Wire.typ ->
  value_gen:'a Alcobar.gen ->
  random:bytes Alcobar.gen ->
  adversarial:bytes Alcobar.gen ->
  'a t
(** [enum_like ~typ ~value_gen ~random ~adversarial] is {!leaf} with raw
    adversarial bytes, so they can carry tags the encoder never writes. *)

val file_input : bool Lazy.t
(** [file_input] is [true] when the run replays one input file, as AFL and a
    crash replay do. *)

val failure_reported : unit -> bool
(** [failure_reported ()] is [true] once a failure has been printed on a file
    input. *)

(** Alcobar, with a [fail] and a {!val-failf} that print the failure on a file
    input before raising it: Alcobar's AFL runner turns a failed property into a
    bare exception and prints nothing. *)
module Alcobar : module type of struct
  include Alcobar
end

val failf : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt] is {!Alcobar.failf}, which on an AFL file input also prints the
    failure, since Alcobar's AFL runner raises without printing it. *)

val pp_hex : Format.formatter -> string -> unit
(** [pp_hex ppf s] prints the bytes of [s] in hexadecimal. *)
