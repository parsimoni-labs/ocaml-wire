# F* model of the scanner's bound checks

An F* model of the part of a `Wire_ocaml` plan that bounds care about,
with proofs that the code the emitter writes reads only inside its window
and accepts exactly what the codec accepts, with and without the O1
rewrites (bf2c2866: cancel common terms in a bound, leave a span's bound
to the next check), the words the int64 step scanner shares between
prefixes (057ff551) and the region rules of the scan chains (b633f225,
bd97743d, 95f307ce). It uses the F*, Z3 and LowParse that ship with
EverParse and is outside the dune build.

```
make -C proofs/fstar verify        # the model and its theorems
make -C proofs/fstar -j4 defects   # past defects and mutants are refused
make -C proofs/fstar plans         # the emitter decides as the model does
make -C proofs/fstar check-extract # plans/extracted is the model
```

`make plans` builds `plans/plans.exe`, which takes the plans
`Wire_ocaml.Plan` makes of the sampled codecs of the differential fuzzer,
its structured scan cases, the `Defect.PrefixWrap` codec, the packet stack
of `examples/net` and the `huge`, `deep` and `nested` schemas of
`test/ocaml`, and the decisions of their module with a fold and its
visitor, on OCaml and on OxCaml, and checks three things of each.

- `wf` holds of the plan: every variable bound once and read after it is
  bound, the premise `scanner_correct` takes of the planner. F* evaluates
  it on the plans up to the packet stack; plans.exe evaluates it, as F*
  extracts it, on all.
- Every bound decision `Wire_ocaml.Plan.decisions` reports (`Span_bound`)
  (left to the next check, hoisted, checked with or without the negative
  test, cancelled) is one the model's walk takes for the same size, region
  depth and constant position (`Plan.Decide.justified`). The walk is
  `Plan.Decide.walk`, run as F* extracts it (`plans/extracted`, over exact
  integers instead of zarith), on two readings of each run of optional
  fields: one that may run no case, one where a case always runs. Each
  walk's decisions are checked against the plan of the fields that walk
  binds (`Wire_ocaml.Plan.live`): every field for the folds, those a check,
  a size or a condition reads for `check`, whose answer has no output. A
  field the walk does not bind is no field of its plan, which must still
  be well formed, so no node reads it.
- Every shared word `Wire_ocaml.Plan.decisions` reports (`Shared_words`)
  a prefix of the step scanner loads is one `Plan.Scan.choose` may keep for
  one of the prefix's fields, between the bounds the model's walk gives the same prefix
  (`Plan.Decide.words_justified`), from a window start of no known bytes or
  of the bytes the frame loop's `stride` test ensures (`fixed_need`). The
  emitter counts a word's offset from the base it gives the window, the
  displacement `Plan.Scan.region_start` takes out of a region's start
  before it (b633f225); the check takes the model's displacement off the
  emitter's offsets, and a prefix at a constant offset must report that
  displacement as its first byte. And a
  prefix at a constant offset reads only words the prefixes before it in
  the same window, in the same walk, loaded: none crosses into a region or
  out of it.
- Every check of the bytes a run of fields needs (`Prefix_bound`) needs
  what the model's guard needs at the same prefix, the sizes saturated as
  `Planner.extent` saturates them, and leaves the window's length untested
  only where the model does (`Plan.Decide.prefix_justified`); after a join,
  where the emitter's position is a variable, a tested check is justified
  by the bytes alone. Every dispatch that checks a bound once before its
  cases (`Dispatch`) hoists the size the model's `hoisted` does at the same
  place. Every walk
  starts with no byte known, or with the bytes its frame loop's `stride`
  test ensures (`Frame_loop`), which are no more than the model's
  `fixed_need`, and walks one frame or four an iteration, the loops
  `Plan.Frames` proves. A frame loop is the one `Plan.Loop.frame_walks`
  models: it walks the frames shorter than its bytes (`short`) by `fold`
  or by a walk of its own that knows no byte, and only when it knows some,
  the others by a walk that knows them all; each field it loads ahead
  (`ahead`), only four frames an iteration, is one of the model's
  `lead_fields`, at the offset and of the width the walk reads it at,
  inside the bytes the loop knows (`Plan.Decide.ahead_justified`). The
  `_index` loop of a visitor's fold is the one `Plan.Index` models: with no leading
  bytes, the plain loop, one frame an iteration or four, every frame
  through `_within` (the walk named after the fold, knowing no byte) and nothing
  loaded ahead; with some, the pipelined loop, two frames an iteration, a
  walk of its own that knows just those bytes, `_within` for the frames
  whose test fails, and each field it loads one frame ahead, those of the
  leading prefixes included, justified as above.
- Every dispatch's tree (`Dispatch.choice`), read over the atoms of the
  conditions it reports it decided (`conditions`), runs the first case whose
  condition the tests on its path decide holds, after those that decide every
  condition before it fails, tests no atom twice on a path, and runs a case
  on every path where the dispatch hoists a bound
  (`Plan.Decide.dispatch_justified`). An atom is any test other than `&&`,
  `||` and `not`, numbered by its text, so two atoms the planner knows to
  depend on each other (`f = 6`, `f = 17`) are independent here.
- Every range of two bitfields a walk tests as one (`Word_range`) is of
  two fields of one word, the first its top bits and the second the bits
  right below, each with a single check `f = c` and `g >= d`, and its
  bounds are `c * 2^shift + d * 2^shift'` and `(c + 1) * 2^shift - 1`
  (`Plan.Decide.range_justified`), which `Plan.Range.range_correct` proves
  test exactly the two checks.
- Every walk holds values as `Plan.Values` models for its run: `int` in
  the scanner, `int64` in its step functions, `int64_u` on OxCaml
  (`Walk.values`); every signed field is at most 64 bits wide and every
  bitfield lies inside its word, the premises of `sign_extend` and
  `view_raw`.

The control claims every bound is left to the next check, moves every word
one byte on and every tested run untested, which the model must refuse for
some plan. Run against
emitters mutated to keep `held` into a region, and to keep a word past the
checked bytes, the check reports each.

`EVERPARSE` defaults to `~/.local/everparse`, an EverParse from v2026.10.27
(F* 2026.10.25~dev, Z3 5.3.0); the sources use its F*'s syntax and
built-in integer `*`, so an older one no longer checks them.

## What is proved

- `Plan.Proof.scanner_correct`: for a well-formed plan (`Plan.Scan.wf`:
  every variable bound once, read after it is bound), the walk `exec`
  returns what the codec's walk `spec` returns (`Reject`, or `Accept` with
  the same bound values) and never returns `Fault` (a read outside the
  innermost window or a plain operation that leaves the 63-bit native int).
  It holds for every combination of the two rewrites (`opts`).
- Regions: a region's size is checked against the window around it, its
  contents read in a window of exactly that size, and an exact region checks
  its contents used all of it. The window is known to hold the low end of
  its size's range from its start (`region_range`, 95f307ce), so a region
  of constant size checks no bound for the prefixes it holds; an exact
  region whose contents end at a constant offset that the range pins at
  both ends (`ends_exact`) skips the test of its end, which `frames_ok`
  justifies by keeping the window's length in that range. A region whose
  contents load no byte (`Plan.Scan.unread`: prefixes of no field the walk
  binds, and tests) leaves its size's bound to the check after it, as a span
  does (f0bd4b6b): where the region fits, the walk is that of a checked
  region; where it runs past the window, its contents read nothing, so they
  reject or end at a constant position, and the check after the region
  rejects (`unread_reject`, `close_pending`).
- `Plan.Proof.region_start_ok` (b633f225): a region's start split into the
  term of its position's sum and a constant (`Plan.Scan.region_start`)
  gives a base and a displacement that add up to the region's window, so
  each load at the displacement plus `k` from that base reads byte `k` of
  the window, the byte `exec` reads.
- `Plan.Proof.negative_rejects` (bd97743d): a walk that starts with the
  check of a prefix of some bytes rejects a negative window length before
  it reads, so the entry of a scan need not test `len < 0`. `frames_ok` in the invariant keeps every
  window inside the one around it and ending where the code after it
  resumes, so contents cannot borrow bytes from beyond their region, as a
  frame of `fold_frames` cannot from the next frame.
- `Plan.Proof.o1_preserves`: with and without O1 and the shared words,
  the same answer.
- Shared words (M5, `Plan.Proof.prefix_words`): each word
  `Emitter.shared_words` keeps for a prefix lies in `[lo, hi]`, `lo` the
  window's start for a prefix at a constant offset and the prefix's address
  otherwise, `hi` the end of the bytes the last check covered
  (`extent at known`), so its 8 bytes are in the window; every field a word
  holds, whether loaded by this prefix or held from one before it in the
  window, is the bytes it covers (`from_words_ok`, given a `consistent`
  reader, which `Plan.Bytes.be_consistent` proves a big-endian buffer is);
  and the words held (`held`) are cleared on entering a region and restored
  on leaving it (`held_ok`, `frames_held`). Which words to keep beyond
  their bounds (unheld, serving two fields) is a parameter `keep` the
  proof quantifies over.
- `Plan.Proof.exec_spec`: the inductive statement behind both, for any
  walk state the invariant `inv` describes, including the state right after
  a span whose bound was left to the next check (`pending`).
- `Plan.Sum`: the planner's sums are sound (`sum_add_ok`, `sum_sub_ok`,
  `sum_of_ok`, `sum_fails_ok`), and `Plan.Proof.cancelled_ok`: the
  cancelled test `lhs > rhs` holds exactly when `pos + size > limit`.
- `Plan.Word.word_field`: a field of `m` bytes read out of a shared 64-bit
  big-endian load, as `Emitter.unsigned` writes it, equals the narrow
  big-endian load; `u16_be_low`, `u16_be_high`, `u16_le_low`, `u16_le_high`
  do the same for `Emitter.u16_in_word`; `word_range` proves the range test
  of c35f9b0e (`version = 4 && ihl >= 5` is the byte in `[0x45, 0x4f]`).
- `Plan.Frames.cursor_loop`: the cursor loop of 73591041 visits the same
  frames as the counted loop it replaced; `frames_in_buffer`: behind the
  frame guard each frame lies in the buffer and the cursor arithmetic stays
  in the native int; each frame is its own window, so a short frame cannot
  borrow bytes from the next.
- `Plan.Loop.frame_loop_correct`: behind the frame guard, every frame
  the loop of `fold_frames` walks, one an iteration or four, lies in the
  buffer; its walk, from the bytes the loop gives it as known (none for a
  frame shorter than the leading fields, `frame_known`), accepts what the
  codec accepts in the frame and never faults; and every field the OxCaml
  loop loads ahead of the walk (922e2bce) lies in that frame: none is
  loaded in the loop over short frames, and each one loaded lies in the
  bytes the loop knows (`ahead_in`). `frame_walks_frames`: both branches
  walk the frames the counted loop did.
- `Plan.Index.index_loop_correct`: the `_index` loop of a visitor's fold over an
  index of frames, pipelined one frame deep and two frames an iteration
  (#558), in a buffer no longer than `Sys.max_string_length`: each test
  reads an index below `n`; each frame's walk, knowing the leading bytes
  where the frame's test `base >= 0 && stop >= need && stop <= length - base`
  passed and through `_within` otherwise, answers what `_within`,
  the plain loop's walk, answers, which is what the codec accepts in the
  frame where the frame lies in the buffer and a rejection elsewhere, and
  never faults (`index_walk_correct`); each field loaded ahead, the leading
  prefixes' among them, is of a frame whose test passed and lies inside the
  bytes that test ensures (`ahead_in`). `index_loop_frames`: the loop walks
  the frames of the plain loop, each once, in order, and tests each once,
  in order; `unrolled_frames`: so does the plain loop of
  four frames an iteration on OxCaml. `index_loop_ordered`: a frame's fields
  are loaded only after its test passed, and before its walk.
  `known_no_overflow`: the OCaml test computes `length - base` only for a
  `base` that is not negative, where it does not overflow; `known_u_is`:
  the OxCaml test, the sign of one `logor` of `int64_u` differences, is the
  OCaml one.
- `Plan.Join.join_correct`: the code after a dispatch written once, in a
  join every case jumps to with the position it ended at (`Joined`), walks
  from the state before the cases, its position a fresh variable `e` and
  no byte known (`join_state`, Emitter.define_join), and accepts what the
  codec accepts from that position, binding the same values and `e`, never
  faulting. Its premises are that the case bound only variables the walk
  before it had not, which `wf` gives, and that the walk's ranges name only
  variables it bound, which `inv` does not state (below). The model's `exec` still copies the code after a dispatch into
  each case (`Copied`); `make plans` counts the joined dispatches, whose
  checks after the join are tested from a variable position (above).
- `Plan.Values`: a value of the 63-bit int held as an `int64` or
  `int64_u` is itself (`signed_of_int`); plain `add`, `sub` and `mul` of
  two such values, where the result lies in the 63-bit int as the
  planner's ranges prove before writing them plain, give the result
  (`plain_ops`); checked arithmetic, through `to_int`, gets back the
  operands the `int` walk has (`checked_operands`), so the step and OxCaml
  walks reject and hold what the `int` walk does. The sign extension
  `shift_right (shift_left v (64 - w)) (64 - w)` of a word whose low `w`
  bits are a field's, whatever it holds above them, is the field's signed
  value (`sign_extend`), and a bitfield read from a word that holds more
  than its bytes above them is the bitfield of its bytes (`view_raw`).
- `Plan.Range.range_correct`: for every word of `bits` bits, bounds that
  `range_ok` accepts hold exactly where the two checks do
  (`Plan.Word.word_range`, here for the bounds the plans use).
- `Plan.Tree.tree_first`: a tree `tree_ok` accepts runs, for every value
  of its atoms, the case the codec's walk of the conditions in order runs
  first, so a case exactly where its condition holds and none before it
  does, and no case exactly where no condition holds (`first_none`);
  `tested_once`: it evaluates each atom at most once; `always_runs`: one
  with no leaf that runs none always runs a case, the premise of the
  bound a dispatch hoists and of the model's reading of a run whose last
  case is its default.
- `Spec.Ipv4.scanner_is_3d` (M3): for a plan of the IPv4 header's first
  four bytes and checks, the generated walk accepts exactly the inputs the
  LowParse parser EverParse builds for the same 3D accepts.

## How the model maps to the code

The functions are those of `lib/ocaml/emitter.ml` and `planner.ml` on the
O1 lane's branch at 114c5dc3; this branch starts before it.

| Model | Code |
| --- | --- |
| `Plan.Scan.node`: `Prefix`, `Span`, `Test`, `Gate`, `Region`, `Join`, `Close` | `Planner` nodes `Prefix`, `Span`, `Test`, `Gate` runs and `Region`; `Join` marks where `Emitter.dispatch` joins, `Close` where a region's contents end |
| `exec` | `Emitter.walk` / `step` |
| `win`, `frame` | the walk's `base` and `limit`, and those `step`'s `Region` case restores |
| `ahead`, the `Prefix` guard, `known` | `Emitter.ahead`, `Emitter.bound`, the `known` field |
| `choose`, `load_words`, `prefix_reads`, `held` | `Emitter.shared_words`, `Emitter.load_words`, `Emitter.prefix`, the `held` field |
| `from_words`, `word_field` | `Emitter.unsigned` with a word (`Boxed`) |
| `span_size`, `outside`, `room_expr` | `Emitter.span`, `Emitter.outside` |
| `cancelled`, `pos_sum` | `Emitter.cancelled`, `Emitter.pos_sum` |
| `checks_next`, `hoisted`, the `Gate` case | `Emitter.checks_next`, `Emitter.hoisted`, `Emitter.hoist_bound` |
| `end_range`, `size_range`, `bind_span` | `Emitter.end_range`, `Emitter.bind_span` |
| `region_range`, `ends_exact`, `region_start` | the `lo, hi` of `Emitter.step`'s `Region` case, `Emitter.close_region`, `Emitter.region_start` |
| `nonnegative`, `refine`, `range` | `Planner.nonnegative_in`, `Planner.refined`, `Planner.range` / `corners` |
| `Plan.Sum` | `Planner.sum_*` |
| `ev` | `Wire_runtime.Arith` (checked); plain code where `fits` |
| `Plan.Index`: `index_loop`, `next`, `known`, `known_u`, `within`, `unrolled` | `Scan_module.pp_index_frames`, `pp_index_next`, its OCaml and OxCaml tests, `_within`, `pp_fold_step_index_plain` |

## What is assumed

- The visitor an `_index` loop calls writes neither the index nor the
  buffer: the pipelined loop loads the next frame's offset, length and
  fields before the walk of the frame before it calls the consumer.
- OCaml on a 64-bit host, from its manual: an `int` is a 63-bit two's
  complement pattern and `land`, `lor`, `lsl` are bitwise on it; an
  `int64` is a 64-bit pattern, `Int64.shift_right_logical` shifts it as
  unsigned, `Int64.to_int` keeps its low 63 bits. `Plan.Word` states these
  as `FStar.UInt` operations (`ocaml_*`) and proves the word extraction and
  `Planner.word_range`'s `lor` on them.
- A window is no longer than `Sys.max_string_length` (it lies in a
  `Bytes`). The constant the emitter adds at generation time, `p.at +
  need`, goes through `Planner.extent` (1883d145), which stops at
  `Sys.max_string_length + 1`: where a run of prefixes needs more, the
  check rejects every frame, as the codec does, so a run needs no bound.
- The top window lies in the buffer, so adding a window's base cannot
  overflow (`Plan.Frames` proves it for `fold_frames`; a region's window
  lies in the one around it).

- OCaml's `Int64` (and `Int64_u`, whose operations are the same on the
  pattern), from its manual: `add`, `sub` and `mul` are modulo 2^64,
  `of_int` sign-extends, `shift_left` multiplies the pattern by a power of
  two modulo 2^64, `shift_right` divides the signed value rounding down,
  `shift_right_logical` divides the pattern and `logand` with `2^w - 1`
  keeps its low `w` bits. `Plan.Values` states them as operations on the
  pattern.

## Where the model departs from the code

- On entering a region the model clears the bounds a dispatch checked
  (`bounded`), and restores those of the window around on leaving it. The
  emitter at b1f6f756 keeps them, which is the defect `region-bounded-leak`
  replays; the model is the fix it proves.

## What is not covered

- That the walk's ranges name only variables it bound, the premise
  `join_correct` takes: `inv` does not carry it, so it is not proved of
  the state `exec` reaches before a dispatch.
- `Planner.nonnegative_in` beyond a range or a passed `a >= b`; products in
  ranges (the planner bounds them by their corners, the model does not).
- That a walk reads a field loaded ahead of it from the variable the loop
  bound instead of loading it: `make plans` checks the loop loads the field
  from the bytes the model's walk reads it at, but `exec` is not run with
  the field bound beforehand.
- That the conditions of a dispatch exclude each other
  (`Dispatch_tree.exclusive`): the tree runs the first that holds, the codec
  each that holds, and they agree only where at most one does; nor that an
  atom the tree leaves untested, which the codec evaluates, does not
  overflow there.
- That the codec accepts what the plan of a walk accepts once the fields
  the walk does not bind are dropped from it: `spec` reads such a field
  only to bind it, and `make plans` checks the walk's plan is well formed,
  so no node reads one, but the model does not state it.
- `off > Bytes.length buf`, which bd97743d leaves to the first check: the
  model assumes the top window lies in the buffer (above), which holds once
  that check passes with a length that is not negative.
- Cuts into functions, the `bounded` key's textual form, OxCaml's unboxed
  integers, bitfield views other than `word_range`, the Rust backend.
- In `make plans`, expressions the model cannot write (division, masks,
  shifts right, casts) become variables it knows nothing of, and signed
  fields keep the model's unsigned range; whether a run of optional fields
  always runs a case is not decided, both readings are walked.
- The int64 form of the step scanner (`Boxed`) runs `exec` over
  mathematical integers: `Plan.Values` proves each operation the emitter
  writes holds the value `exec` computes, operation by operation, but
  `exec` is not run over patterns. The `int` form's sign extension on
  63 bits (`Sys.int_size - w`) and Wire_runtime.Arith's overflow tests
  are not proved; `ev` assumes the latter.

## Next proof targets

`Wire_ocaml.Plan.decisions` hands out decisions the model has no notion of
yet; each is the next thing to model and check against the plans.

- Cuts (`Cut`): a function cut out of a walk keeps its window and the
  bytes known there. `Emitter.cut` walks the rest with the walk's own
  state, so in the model a cut is the identity on `exec`, and what it can
  break lies outside the plan: a variable its body reads that is not one
  of its parameters, or one a parameter shadows, is a property of the
  text. The decision carries no position, so `make plans` cannot tie its
  `known` to the model's walk at the same place; that needs the position
  in `Decision.Cut` and the model's walk to report its state before each
  node.

## Past defects

`make defects` checks that each buggy replay fails and its fix verifies.

| Commit | Defect | Replay | Caught |
| --- | --- | --- | --- |
| e4be2c6f | a span size `len * len` wrapped to 1 and was accepted | `Defect.SpanWrap` | yes |
| e976e0cd | generated `A + (max_int - 5) <> 0` wrapped, accepting `A = 6` | `Defect.ConstraintWrap` | yes |
| 6a0fce0b | `Codec.get` read a u32 bitfield word past a short frame | `Defect.WordUnchecked` | yes |
| 4da4e8ad | validate read fields after an overlong span unguarded | `fields-after-span.sed` on `Plan.Scan` | yes |
| 57535e21 | a region's size walk read past the region's end | `Defect.RegionWalk` | yes |
| 787089f1 | validate accepted a repeat's partial element | `Defect.RepeatBudget` | yes, outside the plan model |
| bf2c2866 (O1 lane) | `Planner.sum_add` raises out of its handler | `Defect.SumConst` | yes |
| b1f6f756, fixed by 1883d145 | 33 nested variable-size codecs fuse into a prefix whose size wraps: no bound, a load far outside the buffer | `prefix-wrap.sed` with `Defect.PrefixWrap.Witness` | yes |
| b1f6f756, fixed by 8902234a (#504) | a dispatch's hoisted bound, keyed by size and position, skips the check of a region of the same size at the same offset inside a case's region: a load past the buffer | `region-bounded-leak.sed` with `Defect.RegionBound.Witness`; `make plans` on the codec | yes |

The last three were found while writing the model. For `Defect.PrefixWrap`
the scanner Wire_ocaml wrote for the codec accepted two bytes the codec
rejects and loaded its last field at `o3 + -4467570830351532362`; the
witness checks the walk that adds with OCaml's wrapping `+` reads outside
the window on the same plan, where the model's, with `extent`, rejects. For
the region defect the scanner accepts six bytes the codec rejects and reads
a 32-bit word whose last two bytes lie past the buffer; the witness checks
the mutant's walk accepts the same plan where the codec's rejects, and
`plans.exe Leak leak` reports the emitter's hoisted decision the model does
not take, until #504 lands.

The plan model expresses the bound and arithmetic defects directly
(4da4e8ad as a mutant of the emitter model itself). 6a0fce0b, 57535e21,
e4be2c6f and e976e0cd are in the codec or in parts the model leaves out
(field accessors, region windows, constraint arithmetic), so each replay
states the property on the piece of model it needs. 787089f1 is a missing
check rather than a bound or an overflow; the replay only compares accept
sets of a repeat, which the plan model does not have. The fold_frames rule
that a short frame cannot borrow bytes from the next came with the design
(7b4a2aa7), not a fix, so it has no buggy version; `Plan.Frames` proves the
current loop keeps it.

The mutant `fields-after-span` keeps the bytes the frame check gave after a
span instead of none: with a 2-byte window, a 1-byte field `a = 1`, a span
of `a` bytes and a 1-byte field, the span fits and the last field is read
at offset 2. `bind_span_ok` then fails: the invariant
`position + known <= len` no longer holds after a span.

The mutants `word-past-check`, `word-before-window` and `held-leak` take
out of `Emitter.shared_words` and the region walk, in turn, the test that
a word ends by the last checked byte, the `max lo` that keeps it from
starting before the window or the prefix, and the clearing of `held` on
entering a region. Each makes `Plan.Proof` fail, and its witness runs the
mutant: two bytes the codec accepts where the first two load a word of
eight, before or past the window; a field at offset 0 of a region at 8
taken from the word held at offset 0 of the buffer, accepting what the
codec rejects.

The mutants `short-known` and `ahead-short` change the frame loop of
`Plan.Loop`: its test of a short frame off by one (`stride + 1 < need`),
and the fields loaded ahead in the loop over short frames too. Each makes
`frame_walks_in` fail, and its witness runs the mutant: a frame of one byte
walked knowing the two its prefix needs faults reading its field at 1; a
field at 1 is loaded ahead of a frame of one byte at 0. Run against an
emitter mutated to load each field eleven bytes past its offset, `make
plans` reports the read-ahead of the packet stack and of two scan cases.

The mutant `region-unread-loads` applies f0bd4b6b's rule to a region whose
contents load a byte, which then reads its fields past the window when the
region runs past it: it makes `exec_spec` fail.

The mutants `index-ahead-untested`, `index-ahead-past` and
`index-known-short` change the pipelined loop of `Plan.Index`: the next
frame's fields loaded before its test, whatever its answer; `ahead_in`
accepting a field that starts in the bytes the test ensures but ends past
them; and the test without `stop >= need`. Each makes `Plan.Index` fail,
and its witness runs the mutant: the byte at 0 loaded from a frame of no
byte, whose test then fails; two bytes at 0 loaded from a frame of one byte
in a buffer of one; a frame of one byte walked knowing the two its leading
fields need, its field at 1 loaded ahead past the buffer.

The mutants `tree-retest` and `tree-case-first` weaken the check of a
dispatch's tree: a path may test an atom again, and a leaf may run a case
without the path deciding the conditions before it fail. The first makes
`tested_once` fail, the second `tree_first`, whose witness is a tree that
runs the second of two cases where the first holds.

The mutant `range-top-bits` accepts a range of two fields below the top
bits of their word, which makes `range_correct` fail; its witness is a byte
whose checks hold outside the bounds, the bits above the fields set.

The mutant `join-known` keeps in the join the bytes the walk before the
cases knew, though the join's position is where a case ended; it makes
`join_holds` fail.

The mutant `sign-shift` writes the int64 sign extension with the shift of
a 63-bit int, which leaves a field's sign bit below the word's: it makes
`sign_extend` fail.

The mutants `region-known-high`, `region-end-unpinned` and
`region-start-off` weaken the region rules: the window known to hold the
high end of its size's range, the end of an exact region left untested
where only the low end pins it, and a region's displacement without its
position's constant. Each makes `Plan.Proof` fail.

The mutants `o1-drop-negative`, `o1-drop-wrap` and `o1-drop-unchecked`
remove one premise each of the O1 rule that leaves a span's bound to the
next check: the size is not negative (a size `a - 5` with `a = 0` moves the
position back, and the next prefix reads before it), the end fits the
native int (`p + n` wraps), and the next node checks bytes (a span before a
where clause or the end of the plan is never bounded). Each makes
`exec_spec` fail, so each premise is needed. They are not past defects.

`make verify` takes a minute and a half of CPU on one core; `make
defects` about fourteen, most of it the mutants, whose failing proofs run
until Z3 gives up (`-j` runs them side by side); `make plans` a few
seconds once `plans.exe` is built.
