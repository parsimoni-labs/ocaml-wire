(** Rust code generated from a codec.

    [Wire_rust] writes the module {!Wire_ocaml} writes for a codec as one Rust
    module, with the same concepts under the same names, in Rust: every offset a
    constant, over [&[u8]] and [&mut [u8]], with nothing but [core] and no
    unsafe code, so the module builds under [#![no_std]]. No input makes the
    module panic: a decoder answers the codec's error as a value. The checks,
    their order and their arithmetic are the plan's, {!Wire_ocaml.Plan}, which
    the OCaml module is written from: a constraint's arithmetic that can leave
    the 63-bit ints of the 64-bit host that runs the generator is computed on
    [u64], as the codec computes it on 64 bits, so the module's fixed-width
    integers compute it alike on every target.

    A generator executable declares the modules of its codecs with {!v} and
    hands them to {!main}, as with {!Wire_ocaml.v} and {!Wire_ocaml.main}:
    {v
    let () =
      Wire_rust.(
        main [ v ~fold:[ "apid" ] Space.packet_codec; v Space.clcw_codec ])
    v}
    {v
    (rule
     (targets space_packet.rs)
     (action (run ./gen.exe %{targets})))
    v}

    {b The module.} The names of the OCaml module, spelled in Rust:
    {v
    let c = space_packet::check(buf, off);
    if c.is_ok() { let v = c.get(); route(v.apid()); }
    let r = space_packet::fold(buf, space_packet::Records::BackToBack,
                               [0u64; 4], None,
                               |mut n, apid| { n[route(apid)] += 1; n });
    v}
    - [check(buf, off) -> Check] checks the record at [off] in [buf], for a
      codec of constant size, and [check(buf, off, len)] the record at [off] in
      the [len] bytes there, for any other: the checks the codec makes, in its
      order. [Check::is_ok] tests it and [Check::get] answers its [View], and
      panics where the check failed: a refused record is input, test [is_ok]
      first. A view borrows the buffer, so its bytes cannot change while it
      lives.
    - Each named field of the record is a method of [View], its accessor, named
      as the record's field, and the presence of an optional field
      [<field>_present]. A field's Rust name is its name lower-cased, any
      character outside a Rust identifier made [_], prefixed with [f_] where it
      would start with a digit or be the lone [_], and suffixed with [_] where
      it is a Rust keyword of any edition, or one followed by underscores
      ([type] is [type_], [type_] is [type__]). An integer field returns its
      value as the codec's own type, [u16] for a 16-bit integer and the
      narrowest unsigned type for a bitfield, 0 where the field is absent; the
      presence of an optional field is a [bool]; a field of bytes a [&[u8]] or a
      [&[u8; N]] of the buffer; a field that holds a record a [Check] of that
      record's module, which passes iff the record is present and its fixed
      bytes lie in the buffer. Each codec a field holds has one module,
      [captured_ipv4] for the OCaml [Captured_ipv4], with the same members. A
      field at a constant offset inside the bytes a view holds is one load of an
      array; any other is placed again from the bytes that place it.
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
    - With [~fold], [fold(buf, records, init, invalid, f)], the OCaml [fold]:
      [records] places the records, [Records::BackToBack],
      [Records::Slots(stride)] or [Records::Frames(offs, lens)], where the OCaml
      fold takes [?stride] or [?offs ~lens]; [invalid], an
      [Option<&mut dyn FnMut(A) -> A>], is its [?invalid]; and
      [f(acc, x1, ..., xn)] gets the fields [fold] names, positionally in the
      order of that list, each in its Rust type, an [Option] as in the OCaml
      fold; Rust has no labelled arguments, so where the OCaml [f] takes
      [~ipv4_protocol], the Rust closure takes the field at its position and a
      closure over two fields of one type can swap them. It answers [Ok(acc)],
      or [Err((e, acc))] at the first record refused with no [invalid]. [f] is a
      type parameter, so the compiler calls it directly and inlines it. It
      panics on the misuses the OCaml fold raises [Invalid_argument] on: a
      stride of 0, offsets and lengths of two lengths, and [invalid] with
      records back to back.
    - [Error], the codec's error ({!Wire.Error.t}): [at], the absolute offset of
      the failing field; [field], the [Path] of field names to it; and [kind], a
      [Kind] with a variant per {!Wire.Error.type-kind}. A decoder answers the
      error the codec answers on the same bytes. An encoder refuses what the
      codec's encoder refuses: a value its field cannot hold is
      [Kind::ValueOutOfRange], a size, a presence or a where clause the values
      break a failed constraint at the field, a buffer too short
      [Kind::UnexpectedEof], and bytes that fail their own checks once written
      the error of those checks. [Error] implements [Display] and
      [core::error::Error]. *)

(** {1:module The module of a codec} *)

type t
(** The type for the module generated from a codec. *)

val v : ?name:string -> ?fold:string list -> 'r Wire.Codec.t -> t
(** [v ?name ?fold c] is the module of [c], named [name] (default: the name
    {!Wire_ocaml.v} gives it), with a fold of the fields [fold] names by their
    paths, as {!Wire_ocaml.v} names them; without [fold] the module has none.
    The module never refers to [c].

    Raises [Invalid_argument] with the message of {!Wire_ocaml.v} for what it
    refuses, ["Wire_rust"] in place of ["Wire_ocaml"]; and, naming the reason,
    for a codec the walk of {!Wire_ocaml.v} does not cover, a map (whose
    functions are OCaml code), an expression Rust cannot spell, a name that is
    not UTF-8 and two names that would be one Rust name. *)

val name : t -> string
(** [name t] is the name of [t]'s module: its file is [name t ^ ".rs"]. *)

val to_rs : t -> string
(** [to_rs t] is the source of [t]'s module. *)

val main : t list -> unit
(** [main ts] is a generator's command line, [gen.exe FILE...]. It writes each
    [FILE], [<name>.rs], the module of [ts] named [<name>]. It exits with status
    2 and a message on a file that names no module of [ts]. *)
