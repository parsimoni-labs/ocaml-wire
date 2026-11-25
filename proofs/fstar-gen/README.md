# The scanner, proved against 3d.exe

For each codec opted in, F* proves that the `fold` of the module
`Wire_ocaml` writes, and the fold `fold_step` with a visitor and its
`fold_step_within`, answer what the parser EverParse writes for the
codec's 3D projection answers, on the same bytes. The F* is not a model:
it is the code the emitter writes, printed by the walk that prints the
OCaml, its operations and loads written as calls into the primitives
`WireGen.OCaml`.

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

The step functions hold each value as an `int64` and call the visitor of
the fold `fold_step` by name. `<M>.StepProof.fold_step_is_3d`,
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
accept and hands out the same values. `fold_step_within_is_3d` states the same
of `fold_step_within buf length off len init`, which `fold_step_index` calls
once per frame, for the `length` its callers pass: the type of
`fold_step_within` requires it to be `P.length buf`. The four consequences
above hold of the step walk too, its `int64` operations included: an
`int64` comparison is signed, as OCaml's on `int64` is.

### The OxCaml scanner

The module `Wire_ocaml.to_ml` writes for `` ~target:`OxCaml `` holds values and
positions as `int64_u`. Its `fold` and its step functions are rendered by
the same walk as `M.OxScan` and `M.OxStep`, each `int64_u` as its 64-bit
pattern, an F* `int64`, and each OxCaml primitive as the primitive of
`WireGen.OCaml` that means the same (`U.add` as `P.int64_add`,
`L.u16le_at` as `P.u16le_at`, `B.offset` as `P.offset`, the literal `#5L`
as `5`). `M.OxProof.fold_is_3d` and `M.OxStepProof.fold_step_is_3d` are the
two theorems above, word for word, of the OxCaml code: its `fold` hands
`valid` each output as an `int` (`U.to_int`), its step functions hand
`S.valid` each as an `int64` (`U.to_int64`). So the OxCaml scanner too
accepts exactly what 3d.exe's parser accepts, hands out the same values
and loads nothing outside the buffer (its loads at an `int64_u` position
require the bytes there); its `int64_u` arithmetic is modulo 2^64, so
where it would wrap the answer would differ from the parser's, which the
proof excludes. The sign of one `logor` that tests `off` and `len` in its
`fold_step_within` rejects exactly the windows that leave the buffer.

### The walks of the frame loops

The loops of `fold_step_frames` and `fold_step_index` walk a frame that
holds the bytes they test (`need`, the leading prefixes) with a walk of
their own, which the OCaml cuts out as `step_frames_record` and
`step_index_record`: it knows those bytes, so it checks none of their
bounds, and on the index loop it reads the fields the loop loaded ahead
(the IPv4 protocol, a tag) from variables instead of the buffer. `M.Frames`
and `M.Index`, and `M.OxFrames` and `M.OxIndex` on OxCaml, render each as
a function of the same name and parameters, printed by the same walk, each
parameter typed with what the loop ensures of it: the frame at `base`
lies in the buffer and holds `need` bytes, its length `stop` at least
those, and each field loaded ahead is its load at `base`.
`M.FramesProof.frame_is_3d` (and `M.IndexProof`, `M.OxFramesProof`,
`M.OxIndexProof`), for every such frame:

```
step_frames_record buf base stop current ==
  Flat.answer (fun () -> S.invalid current) (visit current)
    (Flat.spec (Seq.slice buf base (base + stop)))
```

so the walk of a frame answers what 3d.exe's parser answers on the
frame's bytes, reads no byte outside the buffer, and reads none before
it is known. A walk that reads none of `stop` is stated for every frame
length `stride` of at least `need` bytes.

The loops themselves (the cursor, `last`, four frames an iteration, the
batch too short for the loop, the index's offsets and lengths, the test
`known` and the loads ahead they guard) are not rendered: F* has no
`while` or `ref` that the OCaml's tree maps to. `Plan.Frames` and
`Plan.Loop` prove of the plan model of `fold_frames`' control that it
visits the frames `len / stride` counts, in order, each in the buffer,
and that a field it loads ahead lies in its frame; the model of
`fold_step_index`'s pipelined loop is being written apart. What the
frames' theorems above leave to the loop is exactly their premises: that
the frame lies in the buffer and holds `need` bytes when walked with this
walk, and that a field passed in was loaded at the frame's start. The
theorem the loops call for, their result as the fold of the frames'
answers in order, is not stated here.

## What is generated

`Wire_ocaml.fstar ~codec:M c` returns twenty-seven modules, which
`scripts/fstar-gen/gen.exe` writes with `M.3d` (`Wire.Everparse.write
~mode:`Standalone ~name:M`):

| module | what it holds |
|--------|---------------|
| `M.Scan.fst` | `fold`, as the emitter writes it, with in place each function the OCaml walk is cut into: F* checks a function on its own, without the tests on the path to its call |
| `M.Flat.fsti`, `M.Flat.fst` | `flat`, the record read field by field as the 3D projection lays it out (a nested codec, an optional one as the casetype on its condition, a region as a window of its own), and `flat_is_3d`: `flat` is `answer` of 3d.exe's parser. It depends on the schema alone |
| `M.Proof.fsti`, `M.Proof.fst` | one lemma per value of the walk that reads bytes, stating it in bytes of the buffer (`be buf (o + k) n`, `P.bits` of a word) for every position `o` it reads at, as the emitter chose it, which Z3 applies where the value occurs; then `fold == flat`, and `fold_is_3d` |
| `M.Visitor.fsti` | the module the step functions call by name: an abstract `t`, `invalid` and `valid` (an `int64` per output, a `bool` per presence), none defined, so the proof holds of every module |
| `M.Step.fst` | `fold_step_within` and `fold_step`, as the emitter writes them for the visitor `S`, values held as `int64`, with in place each function the OCaml walk is cut into |
| `M.StepProof.fsti`, `M.StepProof.fst` | `visit`; the lemmas of `M.Proof` for the step walk's values (`P.bits64` of a word); then `fold_step_within == flat` on the buffer's length, `fold_step_within_is_3d` and `fold_step_is_3d`, through the same `M.Flat` |
| `M.OxScan.fst`, `M.OxProof.fsti`, `M.OxProof.fst` | the same as `M.Scan` and `M.Proof`, of the OxCaml scanner's `fold` |
| `M.OxStep.fst`, `M.OxStepProof.fsti`, `M.OxStepProof.fst` | the same as `M.Step` and `M.StepProof`, of the OxCaml scanner's step functions, which call the same `M.Visitor` |
| `M.Frames.fst`, `M.FramesProof.fsti`, `M.FramesProof.fst` | `step_frames_record`, the walk of a frame of the loop of `fold_step_frames`, typed with what the loop ensures, and `frame_is_3d` |
| `M.Index.fst`, `M.IndexProof.fsti`, `M.IndexProof.fst` | the same of `step_index_record`, of the loop of `fold_step_index`, with the fields it reads ahead; only for a codec whose frames need bytes, as only then has the loop a walk of its own |
| `M.OxFrames...`, `M.OxIndex...` | the same of the OxCaml scanner |

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
proof at each of its tests, so that Z3 checks one path at a time; for a
walk on `int64` or `int64_u`, at the reading's tests too, each output of a
path that accepts a query of its own, with no fuel and linear arithmetic
only). It uses
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
normalisation and the loads and masks by Z3. `WireGen.VectorsOx.fst` does
the same for the OxCaml primitives the F* of the OxCaml scanner reads as
those of `WireGen.OCaml` (`Int64_u`'s operations and comparisons,
`Target.offset`, `Target.unsigned_compare`, the sign of `Int64_u.logor`,
the loads at an `int64_u` position of `Target.Local`), on the same
operands (`scripts/fstar-gen/operands.ml`): `vectors_ox.exe` runs them on
an OxCaml compiler, and the file is committed, since `make prove` runs on
the stock one. On an OxCaml compiler `dune runtest` (and `make
test-oxcaml`) checks the committed file is what OxCaml computes; `dune
promote` writes it again.

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
- The OxCaml primitives' meaning, read as `WireGen.OCaml`'s on the 64-bit
  pattern of an `int64_u`: that `Int64_u`'s operations, conversions and
  comparisons are those of `Int64` on the same pattern, `Target.offset`
  keeps its low 63 bits, `Target.unsigned_compare` compares two patterns
  as unsigned, `Int64_u.logor` is negative exactly where an operand is,
  and the loads of `Target.Local` at an `int64_u` position read the bytes
  there, a 16-bit or 32-bit little-endian one included.
  `WireGen.VectorsOx` checks it on its operands, as computed on OxCaml.
- The token table: `M.Scan.fst` and the OCaml `fold`, `M.Step.fst` and the
  OCaml `fold_step_within` and `fold_step`, and the same two of the OxCaml
  scanner, are one tree up to it, which `test/fstar` checks (below), so
  of the printer only the table is trusted.

- `answer`, which is part of the statement: it reads each output out of the
  3D value as the 3D language lays out a bitfield (from the least
  significant bit for `UINT8` and little-endian words, from the most
  significant for big-endian ones).

## The OCaml and the F* are one tree

`test/fstar` (part of `dune runtest`, no F* needed) reads the `fold` of
the module `Wire_ocaml.to_ml` writes with the folds `fold` and
`fold_step`, whose visitor is a module `S`: its `fold` and the functions
it calls, and `M.Scan.fst`, then its `fold_step` and the functions it
calls, and `M.Step.fst`, then `step_frames_record` and
`step_index_record` and `M.Frames.fst` and `M.Index.fst`, then the same
of the module for `` ~target:`OxCaml `` and the `M.Ox*.fst`, with
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
| `U.add`, `sub`, `mul`, `logand`, `logor`, `of_int`, `of_int32_u` | `P.int64_add`, ..., `P.int64_of_int32` |
| `U.shift_right_logical`, `shift_left`, `to_int`, `to_int64` | `P.shift_right_logical`, ..., `P.to_int64` |
| `U.compare a b < 0`, `<=`, `>`, `>=`, `=`, `<>` | `P.int64_lt a b`, ... |
| `B.unsigned_compare a b > 0`, `B.offset` | `P.int64_unsigned_gt a b`, `P.offset` |
| `L.f` | `P.f` |
| `#5L`, `(-#5L)` | `5`, `18446744073709551611`: a literal is the decimal of its 64-bit pattern |
| `e [@nontail]`, `(buf @ local)` | `e`, `buf` |
| `Wire_runtime.Unchecked.f` | `P.f` |
| an `int64` literal `0xFFFF_FFFFL` | `0xFFFFFFFF` |
| `(e : int)` | `e` |
| `f ~x:e` | `f e` |
| `let[@local] f () = e in` | `let f () = e in` |
| `f ~x ~y`, `f` a function the F* does not define | the body of `f` |

A labelled argument becomes positional only where the labels are those
the callee declares, in order: a cut's parameters, or the outputs for
`valid` (`Wire_ocaml.outputs`), after the accumulator for `S.valid`. A
function the emitter cuts out
of the walk, so that merlint accepts the OCaml, is `[@inline always]`
and applied to its parameters, which the call names alike: the F* writes
its body in place, and the test reads it there, after checking that it
reads no variable but its parameters and the definitions around it.
Headers differ and are not compared beyond the parameter names:
`[@inline always]` against none, and the F* parameter types, which F*
checks.

The stock parser reads the OxCaml text once each unboxed literal `#5L`
is `5L` and each `@ local` is dropped, one definition at a time: a
definition it cannot read (an unboxed record, a frame loop's `let
mutable`) is one `fold` and `fold_step` never reach.

The test runs on every codec of the test corpora the F* rendering accepts:
the proved ones, the packet stack of `examples/net`, the headers of
`bench/rust` and `examples/space`, the schemas of `test/ocaml`, the
fuzzers' registry and the codecs the differential fuzzer samples. Three
mutants of the IPv4 OCaml `fold` (a literal, an operator, two labels
swapped) and three of its step walk (a shift, an `int64` comparison, two
labels swapped), three of the OxCaml one (a literal, a comparison, an
output given another field's value) and two of the walk of a frame of the
index loop (two labels swapped; on OxCaml, a shift) must fail it.

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
the step walks. The control of the frame loops is not rendered: the cursor,
`last`, the unrolled iterations, the batch too short for the loop, the
index's offsets and lengths, its `known` test and the loads ahead it
guards stay outside F*, and no theorem states a loop's result as the fold
of its frames' answers (above). Outside the rendering stay too the walks
of a frame of `fold_frames` and `fold_frames_unboxed`,
which call closures (the same walk as `fold_step_frames`'s, its consumer a
parameter), the OxCaml `fold_unboxed` and `decode_unboxed`, the views,
the record module, and the Rust Wire_rust writes, which has a walk of its
own and stays under `make fuzz-rust`.

## Opting a codec in

Add it to `scripts/fstar-gen/proved.ml`, list its twenty-eight files in the
targets of `dune` and add a `prove.sh codec M` rule to the `prove` alias.
`make prove` then runs 3d.exe on `M.3d` and F* on each of its
modules. The proved set is Tiny (two fields and a constraint), the
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
`TinyOxMutant` is Tiny's OxCaml `fold` with `a` read as the low byte of
the 16-bit load one byte on, the byte after `a`: `prove.sh refused ...
OxScan` requires its OxScan and Flat to verify and its OxProof to fail.
`RegionIndexMutant` is Region's OxCaml `step_index_record` given its tag
as read ahead one byte on, a 16-bit load ending one byte past the two
bytes the loop knows: `prove.sh refused ... OxIndex walk` requires the
walk itself not to verify, as the load's type refuses it.

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

The OxCaml scanner adds, with the load between 60 and 150: an `OxProof`
of 11 to 18 s of CPU per codec (Clcw 60 s, IPv4 98 s) and an
`OxStepProof` of 11 to 22 s (IPv4 56 s), an `OxScan` and an `OxStep`
under 1 s each, `WireGen.VectorsOx` 103 to 141 s, and the OxCaml
mutant's failing `OxProof` 14 s on every run; `WireGen.Tables` stays near
100 s with the rules of a field shifted out of a word, and an F* process
peaks at 920 MB. IPv4's `Flat`, whose module did not change, takes 360 to
410 s of CPU against this library where it took 160 s against the library
of the step walk alone: its one large query is that sensitive to the rules
in scope, here those of the OxCaml loads, which `WireGen.OCaml` carries
because the OxCaml scanner needs them where it is checked (a size read
through a byte of a 16-bit load is bounded by them). IPv4's `OxProof`
was as sensitive while the path that accepts was one query of all
thirteen outputs: 98 s here, 209 s with `--query_stats`, past 900 s in
CI. `WireGen.Tac.unfold_step` now splits that path through the reading's
tests and its equation into one query per output, with no fuel and
linear arithmetic only, as are the facts of every proof module. With
every `--z3seed` from 0 to 5 its queries take 23 to 27 s of Z3 in all,
none over 3 s or over 2 units of rlimit; the facts that took up to 32 s
take under 1 s. IPv4's `Flat` is not split: its one query uses 359 of
its 400 units of rlimit, and took 1110 s on a Mac loaded to 540.

The walks of a frame add four proofs per codec, `FramesProof`,
`IndexProof` and their OxCaml twins, of 9 to 19 s of CPU each (IPv4 50 to
65 s), their walks under 1 s each; the index mutant is refused in 1 s, its
walk failing to type-check.
