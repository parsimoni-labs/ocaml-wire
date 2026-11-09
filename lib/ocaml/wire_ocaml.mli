(** OCaml code generated from a codec.

    A codec decodes and encodes through closures built when it is compiled.
    [Wire_ocaml] writes the same work out as OCaml source, with every offset a
    constant, so the compiler sees straight-line loads and stores and a direct
    call to the record's constructor. Its code holds no integer literal wider
    than 31 bits, so js_of_ocaml and wasm_of_ocaml compile it as written; a
    module that reads a 32-bit field into an [int] raises [Invalid_argument]
    when it loads on a target whose ints have 32 bits or fewer.

    {b Runtime.} The generated module links [wire.runtime] (and so [optint]),
    not [wire], and needs no codec at run time: it can be embedded in a program
    that does not link [wire]. Its integer types are [Wire_runtime.UInt8.t] and
    the like, which are {!Wire.UInt8.t} and the like where [wire] is linked, and
    it raises [Wire_runtime.Errors.Parse_error], which is {!Wire.Parse_error},
    with the value the codec raises, and the codec's [Invalid_argument]
    messages. The one exception is a module generated with [~maps]: a map's
    functions are a [Wire] value, so that module links [wire]. The module
    carries the digest of its codec's shape; {!bridge} writes a module that
    checks it against the codec when it loads, for a program that links [wire]
    anyway.

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

val ident : string -> string
(** [ident name] is the OCaml name a generated module derives from the field
    [name], for either target: its getter is [get_<ident name>], its read
    [read_<ident name>] and its record field [ident name]. It is [name]
    lower-cased, any character other than a letter, a digit or ['_'] turned into
    ['_'], prefixed with ["f_"] when that is empty or starts with a digit, and
    suffixed with ['_'] when it is a keyword of OCaml or of OxCaml, so the code
    for the default target compiles on an OxCaml compiler too. *)

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
    [c], for [target] (default [`OCaml]). [codec], the OCaml path of [c], names
    it in the module's header; the module itself never refers to [c]. [make],
    the path of a function taking the field values in order, builds the decoded
    value; it defaults to a generated record, which a codec with a map field
    cannot use.

    A field under a {!Wire.map} is specialised when [maps] names the map: a pair
    of its OCaml path and the map itself, as in
    [("Page.u16be_int", Typ Page.u16be_int)]. The generated module calls the
    map's functions by that path; {!bridge} checks that the field still holds
    that very map.

    Raises [Invalid_argument], naming the field and the reason, for a codec it
    cannot specialise: a variable size, parameters, actions, or a map [maps]
    does not name; and naming both fields when two of them would define the same
    OCaml name ({!ident}), or one of them a name the module defines for itself,
    such as [get_failed]. *)

val bridge :
  ?maps:(string * typ) list ->
  codec:string ->
  core:string ->
  'r Wire.Codec.t ->
  string
(** [bridge ?maps ~codec ~core c] is the source of a module that checks, when it
    loads, that the codec [c] at the path [codec] still has the shape the module
    at the path [core] was generated from by {!ml}, and that each map [maps]
    names is still the very map of its field. It links [wire]; link it next to
    [core] wherever [wire] is linked anyway. *)
