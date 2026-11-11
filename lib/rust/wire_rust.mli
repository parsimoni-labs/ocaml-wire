(** Rust code generated from a codec.

    [Wire_rust] writes a fixed-size codec out as one Rust module, the sibling of
    what [Wire_ocaml] writes for OCaml: every offset a constant, over [&[u8]]
    and [&mut [u8]], with nothing but [core] and no unsafe code, so the module
    builds under [#![no_std]]. A generator executable prints it with
    [print_string (Wire_rust.rs ~codec:"Ipv4.codec" Ipv4.codec)], and the module
    goes in a file of its own ([mod ipv4;]).

    The module has [SIZE], the record's size; [DIGEST], {!digest} of the codec,
    for a build to check the code against the codec it comes from; a [Record]
    struct of the field values; [validate buf off] and [decode buf off], which
    check the record as {!Wire.Codec.validate} does; [write buf off v1 ... vn]
    and [encode r buf off], which refuse a value that does not fit its field
    before writing and a record that breaks its constraints after; [get_<field>]
    and [set_<field>], which read and write one field from its bytes alone, as
    {!Wire.Codec.get} and {!Wire.Codec.set} do; and for a bitfield,
    [get_<field>_word] and [<field>_of_word], so the fields packed in one word
    share one load.

    [Valid::at(buf, off)] checks the record and returns a view whose [<field>()]
    reads index a [&[u8; SIZE]] at constant offsets: the compiler proves each in
    bounds, and the borrow keeps the bytes from changing while the view lives,
    so the record keeps passing its checks.

    A refusal is an [Error] value: [TooShort], [Constraint] naming the field and
    its offset (the empty name for the codec's [where] clause), [InvalidEnum]
    with the value, [Overflow] when a constraint's arithmetic leaves the 63-bit
    integers the codec computes on, which it rejects too, and [OutOfRange] for a
    write. Errors come in the codec's order. *)

val ident : string -> string
(** [ident name] is the Rust name the module derives from the field [name]
    ({!Wire.Private.Names.rust_ident}): its record field and [Valid] method
    [ident name], its functions [get_<ident name>] and [set_<ident name>]. *)

val digest : 'r Wire.Codec.t -> string
(** [digest c] is the digest of [c]'s description that the module generated from
    [c] holds as [DIGEST]. It changes whenever the bytes [c] accepts or the
    values it reads do. *)

val scanner : ?fields:string list -> codec:string -> 'r Wire.Codec.t -> string
(** [scanner ?fields ~codec c] is the source of a Rust module that checks a
    record of [c] in a frame of bytes and returns its scalar fields, written
    from {!Wire_ocaml.Plan}, the plan [Wire_ocaml.scanner] writes, with its
    checks in the same order and the same decisions: which values are read,
    which arithmetic is checked and the decision tree of a run of optional
    fields. It covers what [Wire_ocaml.scanner] covers: integers up to 32 bits,
    bitfields, byte arrays and slices of any size, nested codecs, optional
    fields, fields in a sized region, exact or at most, and where clauses.
    [codec] names [c] in the module's header and in its [CODEC] constant.

    The module has [scan(frame: &[u8]) -> Option<Fields>], which checks the
    frame as {!Wire.Codec.decode} does on those bytes alone and returns its
    scalar fields, or [None] where the codec rejects it, an overflow of its
    arithmetic included; and [decode(buf, off, len)], which first cuts the frame
    out of [buf], [None] when it does not lie inside. [Fields] has a field per
    output [Wire_ocaml.scanner_fields] names, spelled by {!ident}: an integer of
    the field's width and sign, or a [bool] for an optional field's
    [<path>_present]; an absent optional field's outputs are zero. A field in an
    anonymous one is checked but is no output. [fields] selects the outputs by
    those names; every check stays. Each region is a slice of exactly its bytes,
    so no read leaves it; every load indexes an array [split_first_chunk]
    returned. The module needs nothing but [core] and forbids unsafe code. The
    code after an optional field is written once, so its size grows linearly
    with the number of optional fields.

    Raises [Invalid_argument] for what [Wire_ocaml.scanner] refuses (a map among
    them), for a name in [fields] that is no output, and naming both outputs
    when two would spell one Rust field ({!ident}). *)

val rs : codec:string -> 'r Wire.Codec.t -> string
(** [rs ~codec c] is the source of a module specialised to [c]. [codec] names
    [c] in the module's header and in its [CODEC] constant.

    Raises [Invalid_argument], naming the field and the reason, for a codec it
    cannot specialise: a variable size, parameters, actions, a map (whose
    functions are OCaml code), an expression it cannot write in Rust, or a field
    name that is not UTF-8; and naming both fields when two of them would define
    the same Rust name ({!ident}), or one of them a name the module defines for
    itself, such as [new] on [Valid]. *)
