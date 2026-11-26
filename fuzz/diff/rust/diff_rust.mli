(** The Rust backend of the differential fuzzer: the module {!Wire_rust}
    generates from a codec, compiled into a static library and compared with the
    codec on the same inputs, errors exactly. *)

type case = {
  id : int;  (** Its number in the library. *)
  full : string list;  (** The outputs of its fold [full], in order. *)
  selected : string list;  (** The outputs of its fold [selected]. *)
  checks : bool;  (** It has the fold [checks], of no output. *)
  fixed : bool;  (** Its codec has a constant size. *)
  flat : bool;  (** Its setters write every byte of the record. *)
}
(** A generated module. *)

val folds : case -> Diff_ocaml.scan_ops list
(** [folds m] is each fold of [m] as a scanner, through its one frame, its
    frames of a stride and its index. *)

val views : 'r Wire.Codec.t -> case -> Diff_ocaml.scan_ops
(** [views c m] is [m]'s check and accessors, read as {!Diff_ocaml.view_scan}
    reads the OCaml module's, against the codec [c] and the outputs of [m]'s
    fold {!field-full}. *)

val values : 'r Wire.Codec.t -> case -> Diff_ocaml.scan_ops
(** [values c m] is [m]'s [decode], [encode], [size] and [validate], compared as
    {!Diff_ocaml.values_scan} compares the OCaml module's, errors exactly. *)

val check : string -> Fuzz_gen.packed -> case -> bytes -> unit
(** [check label p m buf] compares [m], generated from [p]'s codec of constant
    size, with the codec on [buf] at offsets from 0 to past its end: validate
    and decode give the same error or the same record, the check and each
    accessor read what {!Wire.Codec.validate} and {!Wire.Codec.get} read, encode
    writes the same bytes into buffers around the record or both refuse, the
    setters rebuild a flat record, and at the last offsets a [usize] holds every
    decoder finds the buffer too short and the last setter the field past it.
    Errors compare exactly, offset included. Fails with {!Alcobar.failf}. *)

val raw : string -> 'r Wire.Codec.t -> case -> 'r -> unit
(** [raw label c m v] compares [m], generated from [c] of constant size, with
    [c] on encoding [v], which may break [c]'s constraints or its [where]
    clause: into buffers around the record, at offsets from 0, both write the
    same bytes or refuse with the same error, offset included. [m] reads [v],
    with no check, from the bytes [c] writes before it refuses them. Fails with
    {!Alcobar.failf}. *)
