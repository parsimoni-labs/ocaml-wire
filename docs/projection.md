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

## Arithmetic

3D computes every integer expression in a fixed-width unsigned type and F*
must prove each operation stays inside it, where the OCaml decoder computes
with exact integers. 3d.exe (EverParse v2026.10.27) types an expression as
follows:

- A literal has the smallest of `UINT8`, `UINT16`, `UINT32` and `UINT64`
  that holds it (`300` is a `UINT16`, `4294967296` a `UINT64`). There are no
  negative literals.
- A field has its declared type, read as an unsigned bit pattern: a signed
  field reads as its two's complement, a float as its IEEE 754 bits. F*
  knows a bitfield's bit width (`UINT8 a:4` holds `0..15`).
- `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^` and `?:` have the wider type of
  their operands. `<<`, `>>` and `~` have the type of their left operand. A
  cast `(UINTn) e` has type `UINTn`. A comparison accepts operands of any
  two types.
- F* must prove that `+`, `-` and `*` stay inside the type ("Cannot verify
  u8 addition"), that a divisor is not zero, that a shift amount is less than
  the width of the type, and that a narrowing cast loses no bits. `<<` and
  `~` are not checked: the C masks the result to the type, so `a << 2` over a
  `UINT8` wraps at 256.
- The proof knows each field's type and bounds a shift or a division by a
  constant, `?:`, `+`, `-` and `*`, but nothing of `&`, `|` or `^` beyond
  their type: `(v & 255) + 1` over a `UINT64` does not verify.
- It also knows the refinements of the fields before an expression, the
  `where` clause, and the conditions the expression is guarded by: the left
  side of `&&`, the negated left side of `||` and the condition of `?:` in
  each branch (`a >= 1 && l >= a - 1` verifies; `l >= a - 1` alone does not).
  A strict comparison gives one more (`l > a && l - a - 1 <= 5` and
  `l > a && l - (a + 1) <= 5` verify), as do an equality (`l == a && l - a
  == 0`) and `a - a`.
- A byte size or an array length must have a type of at most 32 bits, a
  constant one too (`[:byte-size 4294967296]` is an error: "has type UINT64
  instead of UInt32").

`Wire.Codec.v` computes the type and the range of every subexpression of a
constraint, `where` clause, size, presence condition, `byte_array_where`
per-byte condition and action by these rules, and
the projection emits what it computed, so the two cannot differ. An addition,
multiplication or left shift whose range does not fit the type 3D would give
it has its operands cast to the narrowest type that holds it (`IHL * 4` over
a 4-bit field stays as written, `a << 2` over a `UINT8` becomes
`((UINT16) a) << 2`), and the C then computes the value OCaml does. The codec
is refused when 3d.exe would reject it: a range past 64 bits, a subtraction
that can go below zero, a divisor that can be zero, a shift amount that can
reach the width of its type, a narrowing cast that can lose bits, a negative
literal, `field_pos`. It is also refused where the C and OCaml would
disagree: `lnot`, which 3D complements within its type and OCaml over a
native int, except in `x land lnot m` with `x` inside the type of `m`; any
use of a signed field but a comparison with a literal, which projects in
two's-complement form (`a < 5` over an `int8` is `(a ^ 128) < 133`); and an
ordering of a float field or of a 64-bit signed one.

An expression whose range passes OCaml's native `int`, or that shifts by
`Sys.int_size` or more, is computed on 64 bits as unsigned integers, as the
C computes it at the `UINT64` it has (`a + 4611686018427387898` over a
`UINT32BE`, `((UINT64) a) * a`): `Wire.Codec`, the OCaml `Wire_ocaml`
generates and the Rust `Wire_rust` generates compare such an expression on 64
bits, and keep every other one in the native int, so a narrow expression
costs what it did. A `byte_array_where` per-byte condition is computed the
same way, over its byte. It projects to a one-byte element struct of its own,
so it reads no parameter and no other field (`byte_array_where` refuses one
that does), and its `sizeof (this)` is 1.

The expressions of a declaration the projection synthesises (an optional
region, a sized wrapper) read the enclosing struct's fields through `UINT32`
parameters, and project as written. The body of an optional region or
casetype is checked there too, as 3D computes it: at the type of its operands,
each field a `UINT32` it knows nothing more of than the `where` its
declaration states: `a >= b` for a size `a - b`, and the bounds the record
proves of the fields it reads, where the body verifies only under them.
`nested ~size:(a * 4)` over a `uint8` projects as `((UINT16) a) * 4` in the
record; under an `optional`, `a * 4` over a `UINT32` verifies only with the
`where (a <= 255)` its declaration then states, and the declaration is named
by that bound as well as its shape, so each use site applies the one it
proves.

An action assigns its value as 3D computes it, widened like a constraint's
(`*out = (((UINT16) a) * 4)`). 3D stores only a value whose type is at most
its output's ("Expected type UINT8"), and an action knows the refinements of
the fields before its own and the condition of each `if` it sits under, not
its own field's refinement: a wider value those bound within the output is
narrowed (`*out = ((UINT8) x)` after `x <= 200`), and any other is refused:
`x + 1` over a `UINT8` into a `UINT8`. `sizeof (this)` is a `UINT32` whose
constant F* knows: it is narrowed the same way when the bytes of the fields
before the first of variable size fit the output (a bitfield counts its whole
word, so this bounds it from above), and refused past them. No
expression reads an output, which 3D holds as a pointer, nor `field_pos`,
which 3D reads only into a `var`. A 3D `if` ends its action, so the
statements after it are written into both of its branches. An output whose
type a native int cannot hold (a `uint64`, or a `uint32` on a 32-bit target)
receives the 64 bits the C stores.

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
the setters with its own indices. An optional integer or float of an
embedded codec has a member after its flag, also under `map` or `enum`. An
embedded byte span has no member, and a sub-codec inside an array, a repeat
or a casetype body is validated but not flattened.

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
(regenerating C with `everparse-3d.exe`, the `agree` differential) are gated on
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
