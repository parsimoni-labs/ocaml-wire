(** The Rust backend of the differential fuzzer: a module {!Wire_rust} generates
    from a codec, compiled into a static library and compared with the codec on
    the same inputs.

    Rust code reports a refusal as a value, not as the codec's exception, so
    both sides are compared on what they decide: the bytes read or written, or
    which kind of error, at which field and offset. *)

(** How a field's value comes back from Rust: an integer, a bitfield (which also
    has its word getter), or a byte array of that length. *)
type kind = Int | Bits | Bytes of int

type field = { name : string; kind : kind }
(** A field of a generated module. *)

type case = { id : int; size : int; fields : field array }
(** A generated module: its number in the library, the record's size and its
    fields in order. *)

val check : string -> Fuzz_gen.packed -> case -> bytes -> unit
(** [check label p case buf] compares [case], generated from [p]'s codec, with
    the codec on [buf], at the offsets of {!Diff_ocaml.check} that are not
    negative (a Rust offset is a [usize]) and at [max_int]: decode and validate,
    every getter and bitfield word, encode into buffers around the record, the
    setters rebuilding the record, and [Valid::at] with its reads. At the last
    offsets a [usize] holds, every read must find the buffer too short. Fails
    with {!Alcobar.failf}. *)
