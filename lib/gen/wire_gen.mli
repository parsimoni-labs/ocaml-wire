(** OCaml code generated from a codec.

    A codec decodes and encodes through closures built when it is compiled.
    [Wire_gen] writes the same work out as OCaml source, with every offset a
    constant, so the compiler sees straight-line loads and stores and a direct
    call to the record's constructor. The generated module checks at start-up
    that the codec it was generated from has not changed, and hands any input it
    rejects to the codec, so its errors are the codec's own.

    A generator executable, run by a dune rule, prints the module for a codec
    with [print_string (Wire_gen.ml ~codec:"Ipv4.codec" Ipv4.codec)].

    The generated module has [get_<field> buf off] for each field, which reads
    it as {!Wire.Codec.get} does with one bounds-checked load at a constant
    offset, and [get_<field>_int] for a 32-bit field's [int] view. [Valid.v buf]
    validates the record at offset 0 and is the proof of it: a buffer's length
    never changes, so [read_<field>] on it is one load with no check of its own.
    It has [size], [decode], [validate] and [write buf off v1 ... vn], which
    stores field values in order. Without [~make] it also defines a record [t]
    of the field values, [make] and [encode]. A codec it cannot specialise
    (variable size, parameters, actions, maps) gets a module with [decode] and
    [validate] that call the codec. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. [`OxCaml] adds [[@zero_alloc]]
    checks on [validate], [write_int], [write] and the reads; [decode_with k],
    which takes a continuation for flambda2 to inline, so a [k] that uses a few
    fields reads only those, under one frame check; and, for the generated
    record, [decode_local], which builds it on the stack. *)

val ml :
  ?target:target -> ?make:string -> codec:string -> 'r Wire.Codec.t -> string
(** [ml ?target ?make ~codec c] is the source of a module specialised to [c],
    for [target] (default [`OCaml]). [codec] is the OCaml path of [c] as the
    generated module sees it. [make], the path of a function taking the field
    values in order, builds the decoded value; it defaults to a generated
    record. *)
