(** Differential testing: one {!Wire.Codec.t} against external C behaviour.

    A diff test keeps the codec as the single OCaml authority and compares it
    against C read/write functions, typically backed by EverParse-generated
    code. *)

type 'r harness
(** A test harness bundling a codec with external read/write oracles. *)

val harness :
  name:string ->
  codec:'r Wire.Codec.t ->
  read:(string -> 'a option) ->
  write:('a -> string option) ->
  project:('r -> 'a) ->
  equal:('a -> 'a -> bool) ->
  ?ocaml_read:(string -> 'a option) ->
  unit ->
  'r harness
(** [harness ~name ~codec ~read ~write ~project ~equal ?ocaml_read ()] creates a
    test harness.

    The harness compares both implementations in a single semantic domain ['a]:
    [project] maps OCaml-decoded values into that domain, [read] extracts an
    external value from wire bytes, and [write] serializes a value in that same
    domain back to wire bytes. This makes full-record diff tests and
    projected-field diff tests use the same API. [ocaml_read], when provided,
    overrides the default OCaml byte-reading path derived from [codec] and
    [project]. This is useful when the OCaml code under test uses a staged
    accessor such as [Codec.get] instead of full decode. *)

type result =
  | Match
  | Both_failed
  | Value_mismatch of string
  | Only_c_ok of string
  | Only_ocaml_ok of string

val read : 'r harness -> string -> result
(** [read h buf] compares the OCaml-decoded value of [buf] against the external
    value read from [buf]. *)

val write : 'r harness -> 'r -> result
(** [write h v] projects [v], serializes it with the external writer, reads the
    resulting bytes with the OCaml side of the harness, and compares projected
    values. *)

val full_roundtrip : 'r harness -> 'r -> result
(** [full_roundtrip h v] encodes [v] with OCaml, reads and re-writes it through
    the external implementation, then reads the result with the OCaml side of
    the harness and compares projected values. *)

(** {1 Type-erased testing} *)

type packed_test = {
  name : string;
  wire_size : int;
  test_read : string -> result;
  test_write : string -> result;
  test_roundtrip : string -> result;
}
(** A type-erased test for generic test loops. *)

val pack : 'r harness -> packed_test
(** [pack h] creates a type-erased test from a harness. *)
