(** Rust code generated from a codec.

    [Wire_rust] writes the module {!Wire_ocaml} writes for a codec as one Rust
    module, under the same names, in Rust: every offset a constant, over [&[u8]]
    and [&mut [u8]], with nothing but [core] and no unsafe code, so the module
    builds under [#![no_std]]. No input makes the module panic: a decoder
    answers the codec's error as a value. The checks, their order and their
    arithmetic are the plan's, {!Wire_ocaml.Plan}, which the OCaml module is
    written from: a constraint's arithmetic that can leave the 63-bit ints of
    the 64-bit host that runs the generator is computed on [u64], as the codec
    computes it on 64 bits, so the module's fixed-width integers compute it
    alike on every target.

    A generator executable declares the modules of its codecs with {!v} and
    hands them to {!main}, as with {!Wire_ocaml.v} and {!Wire_ocaml.main}:
    {v
    let () =
      Wire_rust.(
        main
          [
            v Space.clcw_codec;
            v Packet_stack.capture ~folds:[ fold "classify" ~outputs:flow ];
          ])
    v}
    and a rule writes each module, naming the type that visits each fold:
    {v
    (rule
     (targets captured_stack.rs)
     (action (run ./gen.exe %{targets} --visitor classify=crate::Flow)))
    v}

    {b The module.}

    - [check(buf, off, len) -> Check] checks the record at [off] in the [len]
      bytes there, as the OCaml [check] does; a codec of constant size takes no
      window, [check(buf, off)], and checks the record at [off] in [buf].
      [Check::ok] tests it and [Check::get] answers its [View], [None] where it
      failed. A view borrows the buffer, so its bytes cannot change while it
      lives.
    - Each named field of the record is a method of [View], named as the
      record's field, and the presence of an optional field [<field>_present]. A
      field's Rust name is its name lower-cased, any character outside a Rust
      identifier made [_], prefixed with [f_] where it would start with a digit
      or be the lone [_], and suffixed with [_] where it is a Rust keyword of
      any edition, or one followed by underscores ([type] is [type_], [type_] is
      [type__]). An integer field returns its value as the codec's own type,
      [u16] for a 16-bit integer and the narrowest unsigned type for a bitfield,
      0 where the field is absent; the presence of an optional field is a
      [bool]; a field of bytes a [&[u8]] or a [&[u8; N]] of the buffer; a field
      that holds a record a [Check] of that record's module, which passes iff
      the record is present and its fixed bytes lie in the buffer. Each codec a
      field holds has one module, [captured_ipv4] for the OCaml [Captured_ipv4],
      with the same members. A field at a constant offset inside the bytes a
      view holds is one load of an array; any other is placed again from the
      bytes that place it.
    - [set_<field>(buf, at, x)] writes an integer or a bitfield at a constant
      offset inside the bytes a view holds, in place, in the record at [at], the
      [offset(&v)] of a view: a view borrows its buffer, so a write takes the
      buffer and the view's offset. It checks none of the record's constraints,
      and refuses a value its field cannot hold and a record past the end of
      [buf].
    - [MIN_SIZE], the fewest bytes a record takes; [validate(buf, off)], which
      answers the codec's error; [decode(buf, off)], the record as the module's
      [Record], a field of bytes borrowing [buf]; [encode(r, buf, off)]; and
      [size(r)], the bytes [encode] writes. Each record a field holds has its
      [Record] and [size]. A [Record] literal names each field, as the labels of
      the OCaml [write] do, so [encode] takes the place of [write].
    - Each fold [f] the module declares ({!fold}): the trait [f::Visitor], with
      [valid(&mut self, o1, ..., on)], each output of its Rust type, a presence
      a [bool], and [invalid(&mut self)]; [f(v, buf, off, len)], one frame;
      [f_frames(v, buf, off, len, stride)], the [len / stride] frames of
      [stride] bytes at [off]; and [f_index(v, buf, offs, lens)], the frames of
      [lens[i]] bytes at [offs[i]]. The fold is generic over the visitor, so the
      compiler calls it directly and inlines it.
    - [Error], the codec's error ({!Wire.Error.t}): [at], the absolute offset of
      the failing field; [field], the [Path] of field names to it; and [kind], a
      [Kind] with a variant per {!Wire.Error.kind}. A decoder answers the error
      the codec answers on the same bytes. An encoder refuses what the codec's
      encoder refuses: a value its field cannot hold is [Kind::ValueOutOfRange],
      a size, a presence or a where clause the values break a failed constraint
      at the field, a buffer too short [Kind::UnexpectedEof], and bytes that
      fail their own checks once written the error of those checks. [Error]
      implements [Display] and [core::error::Error]. *)

val ident : string -> string
(** [ident name] is the Rust name the module derives from the field [name]
    ({!Wire.Private.Names.rust_ident}): its record field and [Valid] method
    [ident name], its functions [get_<ident name>] and [set_<ident name>]. *)

val digest : 'r Wire.Codec.t -> string
(** [digest c] is the digest of [c]'s description that the module generated from
    [c] holds as [DIGEST]. It changes whenever the bytes [c] accepts or the
    values it reads do. *)

(** {1:module The module of a codec} *)

type fold
(** The type for the folds a module declares. *)

val fold : ?outputs:string list -> string -> fold
(** [fold ?outputs name] is the fold [name], which checks the record of a frame
    and hands [outputs] (default: every one, {!Wire_ocaml.outputs}) to its
    visitor, as {!Wire_ocaml.fold} does; the others are not read unless a check
    needs them, and every check still runs. *)

type t
(** The type for the module generated from a codec. *)

val v : ?name:string -> ?folds:fold list -> 'r Wire.Codec.t -> t
(** [v ?name ?folds c] is the module of [c], named [name] (default: the name
    {!Wire_ocaml.v} gives it), with [folds] (default: one fold [fold] of every
    output). The module never refers to [c].

    Raises [Invalid_argument] with the message of {!Wire_ocaml.v} for what it
    refuses, ["Wire_rust"] in place of ["Wire_ocaml"]; and, naming the reason,
    for a codec the walk of {!Wire_ocaml.v} does not cover, a map (whose
    functions are OCaml code), an expression Rust cannot spell, a name that is
    not UTF-8, two names that would be one Rust name and two folds of one name.
*)

val name : t -> string
(** [name t] is the name of [t]'s module: its file is [name t ^ ".rs"]. *)

val to_rs : ?visitors:(string * string) list -> t -> string
(** [to_rs ?visitors t] is the source of [t]'s module. Each of [visitors] is a
    fold's name and the type that visits it, a Rust path or a file [x_y.rs] of
    the crate, whose type is [crate::x_y::XY]: the module then checks that the
    type implements the fold's visitor, so the crate fails to build where they
    part. Raises [Invalid_argument] for a fold [t] does not declare. *)

val main : t list -> unit
(** [main ts] is a generator's command line,
    [gen.exe [--visitor FOLD=TYPE]... FILE...]. It writes each [FILE],
    [<name>.rs], the module of [ts] named [<name>]; [--visitor FOLD=TYPE] gives
    each written module's fold [FOLD] the visitor [TYPE] ({!to_rs}). It exits
    with status 2 and a message on a file that names no module of [ts] and on a
    visitor of a fold no written module declares. *)

(** {1:flat Scanners and flat modules} *)

val scanner : ?fields:string list -> codec:string -> 'r Wire.Codec.t -> string
(** [scanner ?fields ~codec c] is the source of a Rust module that checks a
    record of [c] in a frame of bytes and returns its scalar fields, written
    from {!Wire_ocaml.Plan}, the plan the folds of [Wire_ocaml] write, with its
    checks in the same order and the same decisions: which values are read,
    which arithmetic is checked and the decision tree of a run of optional
    fields. It covers what a fold of [Wire_ocaml] covers: integers up to 32
    bits, bitfields, byte arrays and slices of any size, nested codecs, optional
    fields, fields in a sized region, exact or at most, and where clauses.
    [codec] names [c] in the module's header and in its [CODEC] constant.

    The module has [scan(frame: &[u8]) -> Option<Fields>], which checks the
    frame as {!Wire.Codec.decode} does on those bytes alone and returns its
    scalar fields, or [None] where the codec rejects it, an overflow of its
    arithmetic included; and [decode(buf, off, len)], which first cuts the frame
    out of [buf], [None] when it does not lie inside. [Fields] has a field per
    output [Wire_ocaml.outputs] names, spelled by {!ident}: an integer of the
    field's width and sign, or a [bool] for an optional field's
    [<path>_present]; an absent optional field's outputs are zero. A field in an
    anonymous one is checked but is no output. [fields] selects the outputs by
    those names; every check stays. Each region is a slice of exactly its bytes,
    so no read leaves it; every load indexes an array [split_first_chunk]
    returned. The module needs nothing but [core] and forbids unsafe code. [n]
    optional fields give code linear in [n], whether they follow each other or
    nest, and whatever outputs are selected.

    Raises [Invalid_argument] for what a fold of [Wire_ocaml] refuses (a map
    among them), for a name in [fields] that is no output, and naming both
    outputs when two would spell one Rust field ({!ident}). *)

val rs : codec:string -> 'r Wire.Codec.t -> string
(** [rs ~codec c] is the source of a module specialised to [c]. [codec] names
    [c] in the module's header and in its [CODEC] constant.

    Raises [Invalid_argument], naming the field and the reason, for a codec it
    cannot specialise: a variable size, parameters, actions, a map (whose
    functions are OCaml code), an expression it cannot write in Rust, or a field
    name that is not UTF-8; and naming both fields when two of them would define
    the same Rust name ({!ident}), or one of them a name the module defines for
    itself, such as [new] on [Valid]. *)
