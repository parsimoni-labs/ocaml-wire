# The scanner, proved against 3d.exe

For each codec opted in, F* proves that the `fold` `Wire_ocaml.scanner`
writes, and its step functions `fold_step` and `step_within`, answer what
the parser EverParse writes for the codec's 3D projection answers, on the
same bytes. The F* is not a model: it is the code the emitter writes,
printed by the walk that prints the OCaml, its operations and loads
written as calls into the primitives `WireGen.OCaml`.

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

### The step functions

The step functions hold each value as an `int64` and call the module given
to `Wire_ocaml.scanner ~step` by name. `<M>.StepProof.fold_step_is_3d`,
for every `buf`, `off`, `len` and `init`, and every module `S` they may
call (`<M>.Visitor` declares its `t`, `invalid` and `valid` and defines
none):

```
fold_step buf off len init ==
  (if off < 0 || len < 0 || off + len > P.length buf then S.invalid init
   else Flat.answer (fun () -> S.invalid init) (visit init)
          (Flat.spec (Seq.slice buf off (off + len))))
```

`visit init` is `S.valid init` on the same outputs, each `int` as the
`int64` that holds it (`P.int64_of_int`, a bitfield `P.bits64` of its
word's pattern), so the step walk accepts what `fold` and 3d.exe's parser
accept and hands out the same values. `step_within_is_3d` states the same
of `step_within buf length off len init`, which `fold_index_step` calls
once per frame, for the `length` its callers pass: the type of
`step_within` requires it to be `P.length buf`. The four consequences
above hold of the step walk too, its `int64` operations included: an
`int64` comparison is signed, as OCaml's on `int64` is.

## What is generated

`Wire_ocaml.fstar ~codec:M c` returns nine modules, which
`scripts/fstar-gen/gen.exe` writes with `M.3d` (`Wire.Everparse.write
~mode:`Standalone ~name:M`):

| module | what it holds |
|--------|---------------|
| `M.Scan.fst` | `fold`, as the emitter writes it, with in place each function the OCaml walk is cut into: F* checks a function on its own, without the tests on the path to its call |
| `M.Flat.fsti`, `M.Flat.fst` | `flat`, the record read field by field as the 3D projection lays it out (a nested codec, an optional one as the casetype on its condition, a region as a window of its own), and `flat_is_3d`: `flat` is `answer` of 3d.exe's parser. It depends on the schema alone |
| `M.Proof.fsti`, `M.Proof.fst` | one lemma per value of the walk that reads bytes, stating it in bytes of the buffer (`be buf (o + k) n`, `P.bits` of a word) for every position `o` it reads at, as the emitter chose it, which Z3 applies where the value occurs; then `fold == flat`, and `fold_is_3d` |
| `M.Visitor.fsti` | the module the step functions call by name: an abstract `t`, `invalid` and `valid` (an `int64` per output, a `bool` per presence), none defined, so the proof holds of every module |
| `M.Step.fst` | `step_within` and `fold_step`, as the emitter writes them with `~step`, values held as `int64`, with in place each function the OCaml walk is cut into |
| `M.StepProof.fsti`, `M.StepProof.fst` | `visit`; the lemmas of `M.Proof` for the step walk's values (`P.bits64` of a word); then `step_within == flat` on the buffer's length, `step_within_is_3d` and `fold_step_is_3d`, through the same `M.Flat` |

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
that unfold a 3D denotation and the scanner, and split the scanner's
proof at each of its tests, so that Z3 checks one path at a time). It uses
`Plan.Word` from `../fstar`, and `Plan.Values`, whose `int64` operations
`WireGen.Defs` proves those of `WireGen.OCaml` (`int64_add_is_plan` and
the others), so what is assumed of `Int64` is stated once; a value of the
63-bit `int` held as an `int64` is itself by `Plan.Values.signed_of_int`.
`WireGen.Vectors.fst` is written by `scripts/fstar-gen/vectors.exe`, which
runs the OCaml primitives (`Wire_runtime.Unchecked` loads,
`Int64.to_int`, `Int64.shift_right_logical`, `Int32.to_int`, `land`,
`lsr`, `lsl`, `+`, `-`, `*`, the comparisons, and the `Int64` operations
of the step walk: `of_int`, `of_int32`, `add`, `sub`, `mul`, `shift_left`, `logand` by
a mask, the comparisons on `int64`) on boundary and random operands and
states each
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
  sign-extends, `Int64.of_int` and `Int64.of_int32` sign-extend,
  `Int64.add`, `sub` and `mul` are modulo 2^64, a comparison of two
  `int64` is signed). `WireGen.Vectors` checks it on the operands it
  lists, not for all.
- The token table: `M.Scan.fst` and the OCaml `fold`, `M.Step.fst` and the
  OCaml `step_within` and `fold_step`, are one tree up to it, which
  `test/fstar` checks (below), so of the printer only the table is
  trusted.

- `answer`, which is part of the statement: it reads each output out of the
  3D value as the 3D language lays out a bitfield (from the least
  significant bit for `UINT8` and little-endian words, from the most
  significant for big-endian ones).

## The OCaml and the F* are one tree

`test/fstar` (part of `dune runtest`, no F* needed) reads the `fold` of
`Wire_ocaml.scanner ~step:"S"` and the functions it calls, and
`M.Scan.fst`, then its `fold_step` and the functions it calls, and
`M.Step.fst`, with
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
| `Int64.of_int`, `of_int32`, `add`, `sub`, `mul`, `logand` | `P.int64_of_int`, `P.int64_of_int32`, `P.int64_add`, ... |
| `Int64.shift_left` | `P.shift_left` |
| `(a : int64) < b`, `<=`, `>`, `>=`, `=`, `<>` | `P.int64_lt a b`, `P.int64_le`, ... |
| `S.invalid`, `S.valid` | `S.invalid`, `S.valid` |
| `Wire_runtime.Unchecked.f` | `P.f` |
| an `int64` literal `0xFFFF_FFFFL` | `0xFFFFFFFF` |
| `(e : int)` | `e` |
| `f ~x:e` | `f e` |
| `let[@local] f () = e in` | `let f () = e in` |
| `f ~x ~y`, `f` a function the F* does not define | the body of `f` |

A labelled argument becomes positional only where the labels are those
the callee declares, in order: a cut's parameters, or the outputs for
`valid` (`Wire_ocaml.scanner_fields`), after the accumulator for
`S.valid`. A function the emitter cuts out
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
mutants of the IPv4 OCaml `fold` (a literal, an operator, two labels
swapped) and three of its step walk (a shift, an `int64` comparison, two
labels swapped) must fail it.

## Scope

The F* rendering covers records of unsigned big-endian fields of up to 32
bits, bitfields of such words, byte arrays of any size, nested codecs,
optional codecs of a variable size (3D reads them as a casetype on their
condition) and codecs in a region of a size the record gives, with their
constraints. `Wire_ocaml.fstar` raises `Invalid_argument` for anything
else: an optional codec of a fixed size, which 3D reads as a list of
elements, signed and little-endian fields, checked arithmetic, a scanner
that holds an output in a mutable local (more than four optional fields
deep), a negative `int64` literal and an `int64` normalised to 63 bits in
the step walk. Outside the rendering stay the frame loops (`fold_frames`,
`fold_frames_step`, `fold_index_step`), the OxCaml scanner, the views, the
record module, and the Rust Wire_rust writes, which has a walk of its own
and stays under `make fuzz-rust`.

## Opting a codec in

Add it to `scripts/fstar-gen/proved.ml`, list its ten files in the
targets of `dune` and add a `prove.sh codec M` rule to the `prove` alias.
`make prove` then runs 3d.exe on `M.3d` and F* on `M.Scan`, `M.Flat`,
`M.Proof`, `M.Visitor`, `M.Step` and `M.StepProof`. The proved set is Tiny (two fields and a constraint), the
Ethernet and IPv4 headers of `bench/rust/headers.ml` (shared 64-bit loads,
bitfields, a constraint across two bitfields tested as one range of their
word), the CLCW of `examples/space` (fourteen bitfields of a 32-bit word),
Span (a field read at a position a variable-size one gives), Gate (an
optional codec), Dispatch (two on one tag, which the scanner tests as one
tree) and Region (an optional codec in a region of at most a field's
bytes).

`IPv4Mutant` is the IPv4 scanner with `ihl` read with three bits instead
of four: `prove.sh refused` requires its Scan and Flat to verify and its
Proof to fail. `ClcwStepMutant` is the CLCW step walk with `vcid` shifted
out of its word by 17 bits instead of 18: `prove.sh refused ... Step`
requires its Step and Flat to verify and its StepProof to fail.

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

The step functions add, measured the same way with the load between 110
and 200: a `StepProof` of 10 to 23 s of CPU per codec (IPv4 64 s), a
`Step` and a `Visitor` under 1 s each, and the step mutant's failing
`StepProof` 50 s on every run; `WireGen.Vectors` takes 81 s with the
`Int64` vectors and `WireGen.Tables` 98 s with the `int64` masks. An F*
process peaks at 860 MB.
