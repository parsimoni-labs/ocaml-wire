# The scanner, proved against 3d.exe

For each codec opted in, F* proves that the `fold` `Wire_ocaml.scanner`
writes answers what the parser EverParse writes for the codec's 3D
projection answers, on the same bytes. The F* is not a model: it is the
`fold` the emitter writes, printed by the walk that prints the OCaml, its
operations and loads written as calls into the primitives `WireGen.OCaml`.

```
make prove     # needs EverParse in ~/.local/everparse (or EVERPARSE)
```

## The theorem

`<M>.Proof.fold_is_3d`, for every `buf`, `off`, `len`, `invalid` and
`valid`:

```
fold buf off len invalid valid ==
  (if off < 0 || len < 0 || off + len > P.length buf then invalid ()
   else Flat.answer invalid valid (Flat.spec (Seq.slice buf off (off + len))))
```

`Flat.spec s` is `LP.parse (<M>.parser____Wire<S> ()) s`, the LowParse
parser that 3d.exe's own F* output, `<M>.fst`, denotes for the schema, its
value dropped of the bytes it consumed: the record is the prefix of the
window the parser accepts. `Flat.answer` calls `invalid ()` where the
parser fails and otherwise `valid` with each output of the scanner read
from the parser's value: a field as an `int`, a bitfield as `P.bits` of its
word, the presence of an optional field as whether its casetype holds its
second case (`WireGen.Lib3d.ite_case`), whose value holds the field's codec,
or the region it lies in; a field under an optional field that is absent
is 0.

Hence, for the code F* checks:

- the scanner accepts exactly the windows the 3D parser accepts, and hands
  out the values it reads;
- its answer depends on the window's bytes only, so a short frame cannot
  read the next one;
- no load leaves the buffer: each unchecked load of `WireGen.OCaml`
  requires its bytes to lie in the buffer, so the code is typable only if
  they do;
- no `+`, `-` or `lsl` wraps: each requires its exact result to fit an
  OCaml `int`;
- the code terminates and raises nothing: it is `Tot`.

## What is generated

`Wire_ocaml.fstar ~codec:M c` returns five modules, which
`scripts/fstar-gen/gen.exe` writes with `M.3d` (`Wire.Everparse.write
~mode:`Standalone ~name:M`):

| module | what it holds |
|--------|---------------|
| `M.Scan.fst` | `fold`, as the emitter writes it, with in place each function the OCaml walk is cut into: F* checks a function on its own, without the tests on the path to its call |
| `M.Flat.fsti`, `M.Flat.fst` | `flat`, the record read field by field as the 3D projection lays it out (a nested codec, an optional one as the casetype on its condition, a region as a window of its own), and `flat_is_3d`: `flat` is `answer` of 3d.exe's parser. It depends on the schema alone |
| `M.Proof.fsti`, `M.Proof.fst` | one lemma per value of the walk that reads bytes, stating it in bytes of the buffer (`be buf (o + k) n`, `P.bits` of a word) for every position `o` it reads at, as the emitter chose it, which Z3 applies where the value occurs; then `fold == flat`, and `fold_is_3d` |

The library in `lib/` is written once: `WireGen.OCaml` (the primitives,
those whose meaning is more than linear arithmetic opaque to Z3, so that
`M.Scan` is checked with their types alone), `WireGen.Defs` (the meaning
of each opaque primitive as a rewrite rule, which `M.Proof` loads),
`WireGen.Lib` (big-endian reads and the LowParse equations of the
combinators 3d.exe builds from, as SMT patterns), `WireGen.Lib3d` (a
casetype's value, 3D's bitfields most significant bit first, shifts and
masks of its unsigned words), `WireGen.Tables` (the
rules of `WireGen.OCaml` at the literals the emitter prints, which
`scripts/fstar-gen/tables.exe` writes) and `WireGen.Tac` (the tactics
that unfold a 3D denotation and the scanner). It uses `Plan.Word` from
`../fstar`. `WireGen.Vectors.fst` is written by
`scripts/fstar-gen/vectors.exe`, which runs the OCaml primitives
(`Wire_runtime.Unchecked` loads, `Int64.to_int`,
`Int64.shift_right_logical`, `Int32.to_int`, `land`, `lsr`, `lsl`, `+`,
`-`, `*`, the comparisons) on boundary and random operands and states each
result of the primitive of `WireGen.OCaml`: F* checks the arithmetic by
normalisation and the loads and masks by Z3.

## What is trusted

- F*, Z3 and LowParse as EverParse ships them, and the 3D interpreter's
  `as_parser`.
- 3d.exe's front end: that `M.fst` is the meaning of `M.3d`, the trust the
  verified C validator already asks for.
- `WireGen.OCaml`: that each primitive means what the OCaml function means
  on a 64-bit host (an `int` is 63-bit two's complement, an `int64` a 64-bit
  pattern, `Int64.to_int` keeps its low 63 bits, `Int32.to_int`
  sign-extends). `WireGen.Vectors` checks it on the operands it lists, not
  for all.
- The token table: `M.Scan.fst` and the OCaml `fold` are one tree up to
  it, which `test/fstar` checks (below), so of the printer only the table
  is trusted.

- `answer`, which is part of the statement: it reads each output out of the
  3D value as the 3D language lays out a bitfield (from the least
  significant bit for `UINT8` and little-endian words, from the most
  significant for big-endian ones).

## The OCaml and the F* are one tree

`test/fstar` (part of `dune runtest`, no F* needed) reads the `fold` of
`Wire_ocaml.scanner` and the functions it calls, and `M.Scan.fst`, with
the OCaml parser: the bodies of the F* functions keep to the F* that is
also OCaml (`let x = e in`, a local function `let f () = e in`,
`if then else`, application, `&&`, `||`, `not`). It requires the same
functions in the same order, each with the same parameters and the same
body once the OCaml is read through this table, in
`test/fstar/test_tree.ml`:

| OCaml | F* |
|-------|----|
| `a + b`, `-`, `*` | `P.add a b`, `P.sub`, `P.mul` |
| `land`, `lsr`, `lsl` | `P.land`, `P.lsr`, `P.lsl` |
| `<`, `<=`, `>`, `>=`, `=`, `<>` | `P.lt`, `P.le`, `P.gt`, `P.ge`, `P.eq`, `P.ne` |
| `Int64.to_int`, `Int64.shift_right_logical` | `P.to_int`, `P.shift_right_logical` |
| `Int32.to_int`, `Bytes.length` | `P.int32_to_int`, `P.length` |
| `Wire_runtime.Unchecked.f` | `P.f` |
| an `int64` literal `0xFFFF_FFFFL` | `0xFFFFFFFF` |
| `(e : int)` | `e` |
| `f ~x:e` | `f e` |
| `let[@local] f () = e in` | `let f () = e in` |
| `f ~x ~y`, `f` a function the F* does not define | the body of `f` |

A labelled argument becomes positional only where the labels are those
the callee declares, in order: a cut's parameters, or the outputs for
`valid` (`Wire_ocaml.scanner_fields`). A function the emitter cuts out
of the walk, so that merlint accepts the OCaml, is `[@inline always]`
and applied to its parameters, which the call names alike: the F* writes
its body in place, and the test reads it there, after checking that it
reads no variable but its parameters and the definitions around it.
Headers differ and are not compared beyond the parameter names:
`[@inline always]` against none, and the F* parameter types, which F*
checks.

The test runs on every codec of the test corpora the F* rendering accepts:
the proved ones, the packet stack of `examples/net`, the headers of
`bench/rust` and `examples/space`, the schemas of `test/ocaml`, the
fuzzers' registry and the codecs the differential fuzzer samples. Three
mutants of the IPv4 OCaml (a literal, an operator, two labels swapped)
must fail it.

## Scope

The F* rendering covers records of unsigned big-endian fields of up to 32
bits, bitfields of such words, byte arrays of any size, nested codecs,
optional codecs of a variable size (3D reads them as a casetype on their
condition) and codecs in a region of a size the record gives, with their
constraints. `Wire_ocaml.fstar` raises `Invalid_argument` for anything
else: an optional codec of a fixed size, which 3D reads as a list of
elements, signed and little-endian fields, checked arithmetic, a scanner
that holds an output in a mutable local (more than four optional fields
deep), the frame loop, the step and OxCaml scanners, the record module,
and the Rust Wire_rust writes, which has a walk of its own and stays under
`make fuzz-rust`.

## Opting a codec in

Add it to `scripts/fstar-gen/proved.ml`, list its six files in the
targets of `dune` and add a `prove.sh codec M` rule to the `prove` alias.
`make prove` then runs 3d.exe on `M.3d` and F* on `M.Scan`, `M.Flat` and
`M.Proof`. The proved set is Tiny (two fields and a constraint), the
Ethernet and IPv4 headers of `bench/rust/headers.ml` (shared 64-bit loads,
bitfields, a constraint across two bitfields tested as one range of their
word), the CLCW of `examples/space` (fourteen bitfields of a 32-bit word),
Span (a field read at a position a variable-size one gives), Gate (an
optional codec), Dispatch (two on one tag, which the scanner tests as one
tree) and Region (an optional codec in a region of at most a field's
bytes).

`IPv4Mutant` is the IPv4 scanner with `ihl` read with three bits instead
of four: `prove.sh refused` requires its Scan and Flat to verify and its
Proof to fail.

## Cost

`prove.sh` keeps F*'s checked modules in `WIRE_FSTAR_CACHE` (default
`_build/fstar-cache`, one directory per F* version) and 3d.exe's output
there by digest of the `.3d`, so a run checks again only what changed.
The default is inside the tree's build directory, beside `_build/default`,
so each worktree has its own cache and prunes only that one; dune leaves
it alone and `dune clean` removes it.
Each run lists the entries it read or wrote, and once all have run the
`@prove` alias removes the rest: other F* versions, the output of a `.3d`
that changed, modules no codec has any more. The cache so holds what the
current sources need and nothing else.
Each module gets `WIRE_PROVE_TIMEOUT` seconds (default 900); a module that
runs out fails, so the mutant refused by a timeout counts as refused and
the job stays bounded. CI runs `make prove` in the `prove` job of
`.github/workflows/ci.yml`, with the cache kept by `actions/cache`.

Measured on a loaded Mac with EverParse v2026.10.27 and two F* at once:
from an empty cache `make prove` takes 14 minutes, the library about 4 of
them (`WireGen.Tables` 99 s, `WireGen.Vectors` 113 s) and IPv4's `Flat`,
the costliest module, 269 s; from a full cache it takes 3.5 minutes, nearly
all of it the mutant's `Proof` (about 180 s), which fails and so is checked
again on every run. An F* process peaks at 740 MB.

The CPU each module takes is steadier than its wall time on a loaded
machine. Measured with EverParse v2026.10.27 on a Mac whose load stood
between 200 and 470, so that wall time ran ten times the CPU: the
`Proof` of each codec takes 9 to 13 s of CPU (Clcw 51 s, IPv4 44 s), its
`Flat` 3 to 37 s (IPv4 181 s), its `Scan` under 1 s.
