# Projection to EverParse 3D

The limits below decide what a codec can project to 3D, and so what a
combinator must reject when it is built. The README's
[Features](../README.md#features) section covers what does project.

## The 3D ceiling

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
targets: `test/wasm` runs the full suite under both. Tests stay
portable: no integer literal above 2^30, `u32` through `Int32` literals,
`u63` and `uint` through `Int64` and `Optint.Int63.of_int64`, no
`Optint.to_int` on values with bit 31 set, and `Gc.minor_words` checks
skipped on `Sys.Other` backends. `make test-wasm` fails on
integer-overflow warnings except optint's known
`0x7fffffff`/`0x40000000`.
