(** OCaml code generated from a codec.

    A codec decodes and encodes through closures built when it is compiled.
    [Wire_ocaml] writes the same work out as OCaml source, with every offset a
    constant, so the compiler sees straight-line loads and stores and a direct
    call to the record's constructor. The generated module checks at start-up
    that the codec it was generated from has not changed, and hands any input it
    rejects to the codec, so its errors are the codec's own.

    A generator executable, run by a dune rule, prints the module for a codec
    with [print_string (Wire_ocaml.ml ~codec:"Ipv4.codec" Ipv4.codec)].

    The generated module has [size], [decode], [validate] and
    [write buf off v1 ... vn], which stores field values in order. Without
    [~make] it also defines a record [t] of the field values, [make] and
    [encode]. A codec it cannot specialise (variable size, parameters, actions,
    maps) gets a module with [decode] and [validate] that call the codec. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. [`OxCaml] adds [[@zero_alloc]]
    checks on [validate], [write_int] and [write]; [decode_with make], which
    takes the constructor as an argument for flambda2 to inline; and, for the
    generated record, [decode_local], which builds it on the stack. *)

val ml :
  ?target:target -> ?make:string -> codec:string -> 'r Wire.Codec.t -> string
(** [ml ?target ?make ~codec c] is the source of a module specialised to [c],
    for [target] (default [`OCaml]). [codec] is the OCaml path of [c] as the
    generated module sees it. [make], the path of a function taking the field
    values in order, builds the decoded value; it defaults to a generated
    record. *)
