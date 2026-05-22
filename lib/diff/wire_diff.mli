(** Differential testing: one {!Wire.Codec.t} against external C behaviour.

    A diff test keeps the codec as the single OCaml authority and compares it
    against C read/write functions, typically backed by EverParse-generated
    code. *)

type result =
  | Match
  | Both_failed
  | Value_mismatch of string
  | Only_c_ok of string
  | Only_ocaml_ok of string

type t = {
  name : string;
  wire_size : int;
  test_read : string -> result;
  test_write : string -> result;
  test_roundtrip : string -> result;
}
(** A type-erased diff test. *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a diff test summary. *)

val harness :
  name:string ->
  codec:'r Wire.Codec.t ->
  read:(string -> 'a option) ->
  write:('a -> string option) ->
  project:('r -> 'a) ->
  equal:('a -> 'a -> bool) ->
  ?ocaml_read:(string -> 'a option) ->
  unit ->
  t
(** [harness ~name ~codec ~read ~write ~project ~equal ?ocaml_read ()] creates a
    diff test comparing an OCaml codec against external read/write functions.

    The test compares both implementations in a single semantic domain ['a]:
    [project] maps OCaml-decoded values into that domain, [read] extracts an
    external value from wire bytes, and [write] serializes a value in that same
    domain back to wire bytes. [ocaml_read], when provided, overrides the
    default OCaml byte-reading path derived from [codec] and [project]. *)
