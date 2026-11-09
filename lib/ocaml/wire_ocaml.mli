(** OCaml code generated from a codec.

    A codec decodes and encodes through closures built when it is compiled.
    [Wire_ocaml] writes the same work out as OCaml source, with every offset a
    constant, so the compiler sees straight-line loads and stores and a direct
    call to the record's constructor. The generated module checks at start-up
    that the codec it was generated from has not changed, and hands any input it
    rejects to the codec, so its errors are the codec's own.

    A generator executable, run by a dune rule, prints the module for a codec
    with [print_string (Wire_ocaml.ml ~codec:"Ipv4.codec" Ipv4.codec)].

    The generated module has [get_<field> buf off] for each field, which reads
    it as {!Wire.Codec.get} does with one bounds-checked load at a constant
    offset, and [get_<field>_int] for a 32-bit field's [int] view. [Valid.v buf]
    validates the record at offset 0, as {!Wire.Codec.valid} does, and
    [read_<field>] on what it returns is one load with no check of its own. As
    for {!Wire.Codec.valid}, the promise is memory safety only: a buffer's
    length never changes, so a read never leaves the buffer, but if its bytes
    change after [Valid.v], a read may return a value that breaks a constraint.
    [(v :> bytes)] is the buffer itself, not a copy. For a bitfield,
    [get_<field>_word] loads the word that holds it and [<field>_of_word] takes
    the field out, so fields packed in one word share one load. A [Bigstring]
    submodule has the same functions, [Valid] included, on a bigarray of bytes.
    It has [size], [decode], [validate] and [write buf off v1 ... vn], which
    stores field values in order. Without [~make] it also defines a record [t]
    of the field values, [make] and [encode]. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. [`OxCaml] adds [[@zero_alloc]]
    checks on [validate], [write_int], [write] and the reads; [decode_with k],
    which takes a continuation for flambda2 to inline, so a [k] that uses a few
    fields reads only those, under one frame check; and, for the generated
    record, [decode_local], which builds it on the stack. *)

(** A {!Wire.typ} of any value type. *)
type typ = Typ : 'a Wire.typ -> typ

val ml :
  ?target:target ->
  ?make:string ->
  ?maps:(string * typ) list ->
  codec:string ->
  'r Wire.Codec.t ->
  string
(** [ml ?target ?make ?maps ~codec c] is the source of a module specialised to
    [c], for [target] (default [`OCaml]). [codec] is the OCaml path of [c] as
    the generated module sees it. [make], the path of a function taking the
    field values in order, builds the decoded value; it defaults to a generated
    record, which a codec with a map field cannot use.

    A field under a {!Wire.map} is specialised when [maps] names the map: a pair
    of its OCaml path and the map itself, as in
    [("Page.u16be_int", Typ Page.u16be_int)]. The generated module calls the
    map's functions by that path, and checks when it loads that the field still
    holds that very map.

    Raises [Invalid_argument], naming the field and the reason, for a codec it
    cannot specialise: a variable size, parameters, actions, or a map [maps]
    does not name; and naming both fields when two of them would define the same
    OCaml name ({!ident}), or one of them a name the module defines for itself,
    such as [get_failed]. *)
