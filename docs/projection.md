# Projection to EverParse 3D

The limits below decide what a codec can project to 3D, and so what a
combinator must reject when it is built. The README's
[Features](../README.md#features) section covers what does project.

## What 3D cannot express

- A 3D list is a byte budget (`[:byte-size E]`). Variable-size elements
  under a byte budget project; a count followed by that many
  variable-size elements does not, and needs a caller-side cursor.
- No recursion, direct or mutual.
- `casetype` reads its tag inline, immediately before the body, or from
  a caller-passed parameter.
- No terminator-delimited list of a non-byte element: the frontend only
  exposes `zeroterm` (UINT8, zero terminator). Stopping a TLV list at a
  sentinel is done by the consumer trimming the slice first.
- A byte-budget array needs a strong-kind element (fixed scalar, fixed
  byte span, fixed-scalar record). `array`/`repeat` over `nested`,
  `byte_array_where` or a nested `array` is rejected.
- KaRaMeL cannot extract a single-element-array (`nested ~size`) case
  body inside a casetype used as a repeat element; a bitfield case body
  works.
- A variable-size `optional` projects as a parameter-dispatched casetype
  whose absent case is a zero-byte field.
- The pointer/probe/specialisation subsystem is out of scope: it
  describes out-of-band data, not a single-buffer codec.
- A struct with one field (after bitfield packing) is read as that field,
  and a field of its type cannot carry an action when the struct's own
  field carries one ("this field cannot be read because it has multiple
  actions"). The FFI plug therefore does not flatten such a sub-codec
  where the field holding it has a setter: as a field of the entrypoint,
  or under a user action.
- A literal has no big-endian type (`(UINT16BE) 0` is a `UINT16`), so an
  action cannot pass a constant to a big-endian setter: the validator
  cannot clear the plug members of an absent optional.

## The FFI plug

`Everparse.project ~mode:`Ffi` gives the entrypoint a `ctx` parameter and
each named field a setter call, and `Wire_3d` writes the default
`<Name>_Fields` plug, one member per setter index. A codec that embeds
another codec (as a field, under an `optional`, or in a `nested` region)
also flattens it: the sub-codec's struct and the optional casetype or
region wrapper around it take `ctx`, and each of its scalar fields calls the
entrypoint's setter with its own index. Members are named by path, the field
names from the entrypoint down joined with underscores (`ipv4_src`,
`ipv4_tcp_src_port`), with a `<path>_present` flag per optional, the names
`Wire_ocaml.scanner` gives its outputs; a path that spells another member's
name is refused when the codec is projected. A sub-codec reached at two
paths is declared once per path (`Leaf_a`, `Leaf_b`), since each copy calls
the setters with its own indices. An embedded byte span has no member, and
a sub-codec inside an array, a repeat or a casetype body is validated but
not flattened.

The validator writes a member only for what it parses. A presence flag is
written on every successful validation, 1 or 0; the fields under an absent
optional are not written and keep the plug's initial value. The generated
stubs, the differential oracle and the benchmark loops zero the plug before
each validation, so those fields read 0, as in `Wire_ocaml.scanner`. A
caller that reuses a plug without zeroing it reads them only when their
flag is 1.

## Generated C

The standalone archive (`lib<name>.a`, from
`Wire_3d.generate_dune_standalone`) is a shipped deliverable linked into
the target (Solo5, bare-metal flight software), so it builds in every
dune context through that context's toolchain:
`%{ocaml-config:c_compiler}`, the `ocaml-config` pack linker, and
`objcopy`/`ar` resolved with `cc -print-prog-name`. Only host-side steps
(regenerating C with `3d.exe`, the `agree` differential) are gated on
`(= %{context_name} default)`.

EverParse emits embedded sub-validators `static inline` and the
entrypoint validator external, named after the codec. Many codecs link
into one library as one translation unit each; two codecs projecting to
the same entrypoint name collide and are separated with `Codec.rename`.
`WIRECTX` collapsing to the first codec's `<Name>Fields` when several
headers share a translation unit is benign: it is only an opaque pointer
cast back in each validator's own translation unit.

## Narrow-int targets

js_of_ocaml (32-bit ints) and wasm_of_ocaml (31-bit) are supported
targets: `test/wasm` runs the unit suite (`test/*.ml`) under both; the
other test directories (`3d`, `stubs`, `diff`, `cram`) run natively
only. Unit tests stay portable: no integer literal above 2^30, `u32`
through `Int32` literals, `u63` and `uint` through `Int64` and
`Optint.Int63.of_int64`, no `Optint.to_int` on values with bit 31 set,
and `Gc.minor_words` checks skipped on `Sys.Other` backends.
`make test-wasm` fails on integer-overflow warnings except optint's
known `0x7fffffff`/`0x40000000`.
