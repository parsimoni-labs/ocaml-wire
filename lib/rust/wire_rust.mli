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

val rs : codec:string -> 'r Wire.Codec.t -> string
(** [rs ~codec c] is the source of a module specialised to [c]. [codec] names
    [c] in the module's header and in its [CODEC] constant.

    Raises [Invalid_argument], naming the field and the reason, for a codec it
    cannot specialise: a variable size, parameters, actions, a map (whose
    functions are OCaml code), an expression it cannot write in Rust, or a field
    name that is not UTF-8; and naming both fields when two of them would define
    the same Rust name ({!ident}), or one of them a name the module defines for
    itself, such as [new] on [Valid]. *)
