(** Generic differential testing: OCaml codec vs d3t-generated C code. *)

val roundtrip_struct :
  D3t.struct_ -> string -> (string, D3t.parse_error) Stdlib.result
(** [roundtrip_struct s buf] parses [buf] as struct [s] and re-encodes it.
    Equivalent to [write_struct s (read_struct s buf)]. *)

type 'r schema
(** A schema bundles everything needed for differential testing. *)

val schema :
  name:string ->
  codec:'r D3t.Codec.t ->
  c_read:(string -> string option) ->
  c_write:(string -> string option) ->
  equal:('r -> 'r -> bool) ->
  'r schema
(** [schema ~name ~codec ~c_read ~c_write ~equal] creates a schema. *)

type result =
  | Match
  | Both_failed
  | Value_mismatch of string
  | Only_c_ok of string
  | Only_ocaml_ok of string

val read : 'r schema -> string -> result
(** [read s buf] parses [buf] with both OCaml and C, compares results. *)

val write : 'r schema -> 'r -> result
(** [write s v] encodes [v] with OCaml, roundtrips through C, compares. *)

val full_roundtrip : 'r schema -> 'r -> result
(** [full_roundtrip s v] encodes with OCaml, roundtrips through C, decodes with
    OCaml, and compares the final value to the original. *)

(** {1 Type-erased testing} *)

type packed_test = {
  name : string;
  wire_size : int;
  test_read : string -> result;
  test_write : string -> result;
  test_roundtrip : string -> result;
}
(** A type-erased test that can be used in generic test loops without knowing
    the record type. *)

val pack : 'r schema -> wire_size:int -> packed_test
(** [pack schema ~wire_size] creates a type-erased test from a schema.
    [test_write] and [test_roundtrip] decode random bytes via the OCaml codec to
    get a valid value, then run the corresponding diff function. *)
