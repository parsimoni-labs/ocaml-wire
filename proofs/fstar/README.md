# F* model of the scanner's bound checks

An F* model of the part of a `Wire_ocaml` plan that bounds care about,
with proofs that the code the emitter writes reads only inside its window
and accepts exactly what the codec accepts, with and without the O1
rewrites (bf2c2866: cancel common terms in a bound, leave a span's bound
to the next check). It uses the F*, Z3 and LowParse that ship with
EverParse and is outside the dune build.

```
make -C proofs/fstar verify        # the model and its theorems
make -C proofs/fstar -j4 defects   # past defects and mutants are refused
```

`EVERPARSE` defaults to `~/.local/everparse`.

## What is proved

- `Plan.Proof.scanner_correct`: for a well-formed plan (`Plan.Scan.wf`:
  every variable bound once, read after it is bound), the walk `exec`
  returns what the codec's walk `spec` returns (`Reject`, or `Accept` with
  the same bound values) and never returns `Fault` (a read outside
  `[0, len)` or a plain operation that leaves the 63-bit native int). It
  holds for every combination of the two rewrites (`opts`).
- `Plan.Proof.o1_preserves`: with and without O1, the same answer.
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
- `Spec.Ipv4.scanner_is_3d` (M3): for a plan of the IPv4 header's first
  four bytes and checks, the generated walk accepts exactly the inputs the
  LowParse parser EverParse builds for the same 3D accepts.

## How the model maps to the code

The functions are those of `lib/ocaml/emitter.ml` and `planner.ml` on the
O1 lane's branch at 114c5dc3; this branch starts before it.

| Model | Code |
| --- | --- |
| `Plan.Scan.node`: `Prefix`, `Span`, `Test`, `Gate`, `Join` | `Planner` nodes `Prefix`, `Span`, `Test`, `Gate` runs; `Join` marks where `Emitter.dispatch` joins |
| `exec` | `Emitter.walk` / `step` |
| `ahead`, the `Prefix` guard, `known` | `Emitter.ahead`, `Emitter.bound`, the `known` field |
| `span_size`, `outside`, `room_expr` | `Emitter.span`, `Emitter.outside` |
| `cancelled`, `pos_sum` | `Emitter.cancelled`, `Emitter.pos_sum` |
| `checks_next`, `hoisted`, the `Gate` case | `Emitter.checks_next`, `Emitter.hoisted`, `Emitter.hoist_bound` |
| `end_range`, `size_range`, `bind_span` | `Emitter.end_range`, `Emitter.bind_span` |
| `nonnegative`, `refine`, `range` | `Planner.nonnegative_in`, `Planner.refined`, `Planner.range` / `corners` |
| `Plan.Sum` | `Planner.sum_*` |
| `ev` | `Wire_runtime.Arith` (checked); plain code where `fits` |

## What is assumed

- OCaml on a 64-bit host, from its manual: an `int` is a 63-bit two's
  complement pattern and `land`, `lor`, `lsl` are bitwise on it; an
  `int64` is a 64-bit pattern, `Int64.shift_right_logical` shifts it as
  unsigned, `Int64.to_int` keeps its low 63 bits. `Plan.Word` states these
  as `FStar.UInt` operations (`ocaml_*`) and proves the word extraction and
  `Planner.word_range`'s `lor` on them.
- A window is no longer than `Sys.max_string_length` (it lies in a
  `Bytes`), and a run of prefixes and tests is no longer either (`wf`).
  Then the constant the emitter adds at generation time, `p.at + need`,
  which the model computes with OCaml's wrapping `+`, does not wrap. The
  planner breaks the second premise at b1f6f756 (`Defect.PrefixWrap`).
- A window lies in the buffer, so adding its base cannot overflow
  (`Plan.Frames` proves it for `fold_frames`; regions are not modelled).

## What is not covered

- Regions (nested windows, `at_most`) and their contents' walk.
- The join of a dispatch: the model continues each case into the code after
  it (the emitter's copy mode); the join's state forgets more, which only
  makes it decide less.
- `Planner.nonnegative_in` beyond a range or a passed `a >= b`; products in
  ranges (the planner bounds them by their corners, the model does not).
- Cuts into functions, the `bounded` key's textual form, OxCaml's unboxed
  integers, bitfield views other than `word_range`, the Rust backend.
- A differential run of the model against the planner. F* extracts
  `Plan.Expr`, `Plan.Sum` and `Plan.Scan` to OCaml (`--codegen OCaml`), but
  the result needs F*'s OCaml runtime and zarith (`Prims.int` is `Z.t`),
  which the repository's switch does not have, and the emitter's decisions
  (which bound it cancels or leaves out) exist only as generated text.

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
| b1f6f756 (open) | 33 nested variable-size codecs fuse into a prefix whose size wraps: no bound, a load far outside the buffer | `Defect.PrefixWrap`, `prefix-run-unbounded.sed` | yes |

The last two were found while writing the model. For `Defect.PrefixWrap`
the scanner Wire_ocaml writes for the codec accepts two bytes the codec
rejects and loads its last field at `o3 + -4467570830351532362`; the
replay checks the model's walk faults on the same plan, and the fix side
shows `wf` refuses the plan.

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

The mutants `o1-drop-negative`, `o1-drop-wrap` and `o1-drop-unchecked`
remove one premise each of the O1 rule that leaves a span's bound to the
next check: the size is not negative (a size `a - 5` with `a = 0` moves the
position back, and the next prefix reads before it), the end fits the
native int (`p + n` wraps), and the next node checks bytes (a span before a
where clause or the end of the plan is never bounded). Each makes
`exec_spec` fail, so each premise is needed. They are not past defects.

Verification takes about two minutes (`make verify`, one core); `make
defects` under three minutes with `-j4`, most of it the mutants, whose
failing proofs run until Z3 gives up.
