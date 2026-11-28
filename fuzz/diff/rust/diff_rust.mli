(** The Rust backend of the differential fuzzer: the module {!Wire_rust}
    generates from a codec, compiled into a static library and compared with the
    codec on the same inputs, errors exactly. *)

type case = {
  id : int;  (** Its number in the library. *)
  outputs : string list;  (** The outputs its views read, in order. *)
  full : string list;
      (** The outputs the fold of its module of every field hands out. *)
  selected : string list;
      (** The outputs the fold of its module of every other field hands out. *)
  checks : bool;  (** It has a module whose fold hands out no field. *)
  fixed : bool;  (** Its codec has a constant size. *)
  flat : bool;  (** Its setters write every byte of the record. *)
}
(** The generated modules of a codec. *)

val folds : case -> Diff_ocaml.scan_ops list
(** [folds m] is the fold of each module of [m] as a scanner, through one frame
    of an index, the slots of a stride and an index. *)

val views : 'r Wire.Codec.t -> case -> Diff_ocaml.scan_ops
(** [views c m] is [m]'s check and accessors, read as {!Diff_ocaml.view_scan}
    reads the OCaml module's, against the codec [c] and the outputs
    {!field-full} of [m]'s fold of every field. *)

val values : 'r Wire.Codec.t -> case -> Diff_ocaml.scan_ops
(** [values c m] is [m]'s [decode], [encode], [size] and [validate], compared as
    {!Diff_ocaml.values_scan} compares the OCaml module's, errors exactly. *)

val check : string -> Fuzz_gen.packed -> case -> bytes -> unit
(** [check label p m buf] compares [m], generated from [p]'s codec of constant
    size, with the codec on [buf] at offsets from 0 to past its end: validate
    and decode give the same error or the same record, the check and each
    accessor read what {!Wire.Codec.validate} and the decoder's readers read,
    encode writes the same bytes into buffers around the record or both refuse,
    the setters rebuild a flat record, and at the last offsets a [usize] holds
    every decoder finds the buffer too short and the last setter the field past
    it. Errors compare exactly, offset included. Fails with {!Alcobar.failf}. *)

val raw : string -> 'r Wire.Codec.t -> case -> 'r -> unit
(** [raw label c m v] compares [m], generated from [c] of constant size, with
    [c] on encoding [v], which may break [c]'s constraints or its [where]
    clause: into buffers around the record, at offsets from 0, both write the
    same bytes or refuse with the same error, offset included. [m] reads [v],
    with no check, from the bytes [c] writes before it refuses them. Fails with
    {!Alcobar.failf}. *)
