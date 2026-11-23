# Hardcaml optimization audit

## Measurement protocol replaces core-only tuning

The ECP5 results below are historical diagnostics, not the target acceptance
metric. No further ECP5 micro-tuning is planned before measuring the registered
core on the pinned F2 part under Vivado. That OOC measurement needs an x86
Vivado host, not an FPGA instance. This arm64 workspace has no Vivado host
configured, so no target timing result is reported yet.

The default OOC top now registers every non-clock input and every output. This
makes result-bank selection, rejection masks and length saturation part of
register-to-register timing. The SDE adapter remains a separate OOC configuration
with explicit external delays. The fixture is timing-only; it is not an elastic
transport adapter and is never substituted into the HDK deployment.

Reports after synthesis, placement and routing identify each endpoint class,
its worst path, logic levels, slack and worst-path fanout. Missing anchors are
reported as incomplete coverage, not zero-depth successes. The shared selectors,
4 ns planning allocations, local XCUP structural results and pinned published
profile are in [TIMING.md](../bench/hardcaml-stream/TIMING.md). Those local mapped
depths are conservative structural counts, not Vivado delays or Fmax.

Three-cycle latency describes the current implementation, not an architectural
requirement. Once target timing is available, validation may be split by actual
dependencies and individual failure bits registered before acceptance reduction.
Buffering will be reassessed with complete target timing and area data.

The oracle is frozen independently of RTL: corpus, native reference executable,
case, maximum and framing format determine its cache key. Replay verifies the
receipt and record digest and does not run or rebuild EverParse. Broad campaigns
belong after timing selection; they are not part of each timing iteration.

## Historical ECP5 experiments

The original 102.88 MHz ECP5 result is a measured design point. It does not establish an
architectural limit or an exhaustive search of current techniques. The target
remains 250 MHz, and a comparable state-of-the-art result has not been shown.

The measurements and test runs below were recorded before the backend was
ported onto the current `Wire_ocaml.Plan`; they have not been repeated since.

## Documentation actually consulted

This audit read the official Hardcaml sections on
[combinational logic and widths](https://docs.hardcaml.org/hardcaml-docs/designing-circuits/combinational_logic/),
[registers, pipelines and memories](https://docs.hardcaml.org/hardcaml-docs/designing-circuits/sequential_logic/),
[multiplexers](https://docs.hardcaml.org/hardcaml-docs/designing-circuits/mux4/),
[signal naming](https://docs.hardcaml.org/hardcaml-docs/more-on-circuit-design/naming/)
and [hierarchy](https://docs.hardcaml.org/hardcaml-docs/using-interfaces/module_hierarchies/).
The installed library is v0.17.1; examples from current documentation still need
checking against that API and the inferred RTL.

Jane Street's
[ZPrize implementation notes](https://zprize.hardcaml.com/msm-implementation-details.html)
describe frequency/implementation-strategy experiments and explicit placement
across SLRs. Their automatic-retiming experiment made their design slower.
These are useful engineering examples, not evidence that the same settings
improve this parser. Its
[field-arithmetic notes](https://zprize.hardcaml.com/msm-field-multiplication)
also illustrate parameterized pipeline cuts and width-aware designs. The
cryptographic arithmetic algorithms are not packet-parser optimizations.

AMD's [high-fanout guide](https://docs.amd.com/r/en-US/ug949-vivado-design-methodology/Optimizing-High-Fanout-Nets)
and [practical optimization notes](https://adaptivesupport.amd.com/s/article/High-fanout-net-optimization-techniques?language=en_US)
explain register replication, placement, and inspecting non-clock fanout. Such
changes need routing evidence; a smaller Boolean expression alone is insufficient.

The community [FPGA engineering guide](https://ocamlstreet.gitbook.io/hardcaml-wiki/fpga-engineering)
and [Hardcaml reference](https://ocamlstreet.gitbook.io/hardcaml-wiki/hardcaml/cheatsheet-reference)
were also consulted. They do not replace the official API, width checks or
vendor timing reports. This is a targeted reading list, not a claim to have
read every guide or tried every technique.

## What has been measured

All streaming numbers below use ECP5-85F/CABGA381 speed grade 8, a requested
250 MHz clock, matched placement seeds 1–3 unless stated otherwise, and the complete 512-bit streaming metadata
core. They are out-of-context estimates, excluding physical I/O and system
integration. The earlier complete-frame decoder is a different interface and
cannot serve as this stream's baseline.

| Technique | Evidence or status |
| --- | --- |
| Shared checked-plan lowering | Used by bounded and streaming hardware; differential checks reuse Codec, generated OCaml, and verified EverParse C. |
| Bounded prefix capture | Proven 94-byte scalar read prefix, 128-byte capture, logical maximum 65,549 bytes. |
| Fixed capture lanes for full nonfinal beats | Implemented; flexible contiguous packing remains separately checked. |
| Balanced KEEP population-count tree | Implemented; further topology choices have not been compared. |
| Narrow cursors and conservative output widths | Implemented; native scalar semantics and every acceptance check remain. |
| Three-stage valid/error/data/ID alignment | Implemented and tested with stalls/reset; pipeline placement is not exhaustively explored. |
| Remove saturation from raw-counter feedback | Baseline: 102.88 MHz, 4,901 COMB, 2,485 FF; preceding variant was 99.64 MHz. |
| Custom counter lookahead | Correctness checks passed, but 98.17 MHz and 4,916 COMB; discarded. |
| Move report saturation across the EOF snapshot | Retained after matched seeds 1–3: mean 99.83 → 105.19 MHz; 4,912 COMB / 2,485 FF. |
| Move consumed-value masking after its register | Retained after matched seeds 1–3: mean 105.19 → 109.46 MHz; 4,919 COMB / 2,485 FF. |
| Separate counter feedback from EOF overflow reporting | Retained after matched seeds 1–3: mean 109.46 → 118.32 MHz; 4,915 COMB / 2,482 FF. Full structured C-backed replay passed. |
| Parallel adjacent-bit mask validation | Retained: mean 118.32 → 122.36 MHz; 4,909 COMB / 2,482 FF. One of three seeds regresses; full structured C-backed replay passed. |
| Widened exact bounds arithmetic | Discarded: seed 1 falls from 120.92 to 109.29 MHz, with 29 extra logic cells. Exact equivalence and functional checks passed; no performance benefit was shown. |
| Reserved-credit output buffering | Retained: four slots preserve three-cycle latency and isolate READY. Mean 122.36 → 123.74 MHz; 6,328 COMB / 4,009 FF. Both small control inductions, bounded-pipeline equivalence, 17 C-backed transport tests, both full differential lanes and 15,512 shared-fixture RTL captures pass. |
| Alternative field-selection muxes and one-hot dispatch | Not systematically measured. Changes must preserve absent-field and malformed-input behavior. |
| Additional internal pipeline cuts | Not systematically measured. All control and scalar snapshots must remain aligned. |
| Multiple packet lanes | Not implemented. Requires enough ingress/egress bandwidth and explicit ordering/arbitration. |
| Vivado strategy, floorplan, SLR and primitive selection | Not measured on F2. ECP5 results cannot predict those implementation choices. |
| Matched published parser baseline | Not established; protocol coverage, validation, fields, device and timing constraints must match. |

The user's proposed beat counter, buffered pipeline, narrower arithmetic,
segmented IDs, full-beat-only fast profile, and wider multi-frame interface
remain concrete candidates. A beat counter must retain actual byte counts for
malformed masks, including rejected frames. A 32-bit ID would wrap in about
43 seconds at 100 million frames/second, so the 64-bit contract remains. A wider
bus needs multiple frame boundaries and result slots to improve small-frame
packet rate.

An arithmetic audit of the clamp candidate after Yosys `proc; opt; wreduce;
opt_clean` found that cursor/length additions and subtractions already use at
most 18 bits for this stack. Two separate 64-bit increments compute the frame
ID and `TKEEP + 1` for mask validation. Four comparator cells retain 63-bit ports, but their operands contain
only 6–18 distinct bits and many constant bits; a wide RTL declaration does not
establish a correspondingly wide physical carry chain. A compiler range pass
must be justified by the synthesized paths and preserve signed values and
native shift/bitwise semantics. Header fields also include 32-bit values.

The mask increment has a parallel Boolean alternative: a mask is low-contiguous
exactly when no adjacent pair has a higher one and a lower zero. Combinational
SAT proves the production helper equivalent to the original expression for all
masks at widths 1–64 and 128, including zero/full masks and one-byte buses.
For the isolated 64-bit predicate, ECP5 mapping changes 49 LUT4 / 10 PFUMX /
4 L6MUX21 cells to 48 / 7 / 2; the longest topological path changes from eight
cells to seven. Neither mapping contains CCU2C carry cells: Yosys already
optimizes the old increment expression into a LUT/mux tree. This is a small
measured logic improvement, not removal of a physical 64-bit carry chain.
Whole-core frequencies are 120.92 / 121.46 / 124.70 MHz at seeds 1–3,
respectively: one seed loses 3.41%, two gain 5.65% and 8.61%. The mean gains
3.41%, with six fewer logic cells and unchanged register count. This is
placement-sensitive evidence, not a guaranteed improvement for every seed.
The 15 C-backed transport tests, both small control proofs, 19 structured
streaming cases at 1,000 repeats and 163 emitted-RTL captures also pass.
An input-pin path need not improve the reported register-to-register Fmax, so
external timing needs its own constrained review.

The FPGA family is also material. AMD's
[100G example](https://github.com/Xilinx/xup_vitis_network_example/blob/master/Ethernet/README.md)
documents a 512-bit CMAC interface at 322.265625 MHz. That hardened MAC is not
evidence that this soft parser meets timing. The F2 shell integration is pinned
to its 250 MHz main clock and still requires a Vivado build.

## Baseline design's critical path

The baseline route's worst register-to-register path is 9.72 ns. The timing
JSON decomposes it into 5.811 ns routing, 3.227 ns cell logic, 0.395 ns
clock-to-Q and 0.287 ns setup. The implementation log groups the latter three
as 3.909 ns of logic-related delay. Routing accounts for about 60% of the path.

Tracing the generated RTL identifies the start as the raw 17-bit length
register and the endpoint as the first reported-length snapshot. Between them
are the next-byte sum, overflow comparison and saturation selection. Synthesis
mapped the clamp-to-constant into the snapshot flip-flop's LSR control. This
identifies a concrete place to change pipeline placement; it does not establish
that every high-fanout enable or extraction mux is already optimized.

The retained change captures a sufficiently wide raw sum and the accumulated
overflow flag together, then applies saturation after that register in an
existing downstream stage. The raw feedback counter, overflow detection,
draining behavior, current diagnostics and three-cycle external latency must
retain their semantics. Merely adding a register to the counter feedback would
change the recurrence and could corrupt consecutive beats.

| Placement seed | Original MHz | Retimed report clamp MHz | Retimed consumption MHz | Deferred overflow MHz | Parallel mask MHz | Buffered MHz |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | 102.88 | 104.42 | 108.23 | 125.19 | 120.92 | 120.67 |
| 2 | 95.03 | 105.99 | 107.41 | 114.96 | 121.46 | 125.91 |
| 3 | 101.57 | 105.17 | 112.73 | 114.81 | 124.70 | 124.64 |
| Mean | 99.83 | 105.19 | 109.46 | 118.32 | 122.36 | 123.74 |

The first retiming's mean gain is 5.38%; the target remains unmet. Its seed-1 critical path moves
into decoder cursor/region validation and the zero-on-rejection consumption
register. Its 9.58 ns includes 3.59 ns logic-related delay and 5.99 ns routing.
The consumption experiment applies the established raw-field register
convention: register the raw cursor and acceptance together, then mask the
public result after the register. Rejected consumption remains zero. This adds
4.05% over the retimed clamp's mean, with positive changes in all three seeds.
The result mask now lies on an output path, so a register-to-register estimate
does not establish its complete interface timing. The F2 build must include the
adapter and its external delay constraints before accepting the system clock.
Both small control proofs, the bounded pipeline equivalence proof, the 14
C-backed transport tests, all 19 cases at 1,000 repeats in both bounded and
streaming differential lanes, and 163 actual RTL captures pass. Across these seeds, its remaining worst paths all
involve the raw counter's next sum and overflow flag capture.

The counter experiment freezes from the current raw value exceeding the
maximum, allowing the increment and overflow comparison to run independently.
Raw storage must hold the first overshoot (`maximum + beat_bytes`), and the EOF
snapshot must also hold one further beat (`maximum + 2 * beat_bytes`). Otherwise
a small maximum with a wide beat can wrap before the late check. Two sticky
input/mask error bits remain; final scope supplies the aligned overflow flag.
The published length clamps after the last register. Live length/error views
must still reflect overflow immediately after each accepted beat. This first
experiment retains actual popcounts; a beats-only counter is a separate design.
The final length clamp is another output path requiring the integrated timing
check described above.

This adds 8.10% over the preceding design's mean and 18.52% over the original
three-seed baseline. The 15 C-enabled transport/configuration tests, both small
control proofs and 163 actual RTL captures pass. A focused 131-byte frame test
with maximum three and 64-byte beats protects the EOF carry bit: deliberately
truncating that snapshot made the controller falsely accept the oversized
frame. The correct source was restored and its emitted RTL checked identical
before continuing. All 19 structured streaming cases at 1,000 repeats against
the real EverParse C oracle also pass.

The report-clamp change passes the existing 14 C-backed transport tests, both
small-instance control induction profiles, all 19 structured C-backed scanner
cases at 1,000 repeats, and 163 actual RTL captures checked by the host verifier.
The manifest and three-cycle interface latency remain unchanged.

The discarded bounds experiment replaces `pos <= limit && size <= limit - pos`
with the sign of `zero_extend(limit) - zero_extend(pos) - zero_extend(size)`
at width `cursor_width + 2`. That width represents the entire signed remainder
range without wrapping. The native-width equality check still rejects negative
or truncated DSL sizes. Constant-zero operands retain simpler comparisons.
SAT proved the candidate for widths 1–21, and the 15 C-backed transport tests
and 163 emitted-RTL captures passed. Nevertheless, seed 1 dropped from 120.92
to 109.29 MHz and area grew from 4,909 to 4,938 logic cells, with unchanged
register count. Its critical path contained 4.25 ns logic-related delay and
4.90 ns routing. The original helper was restored; no unused alternative
remains in production. This is an exact arithmetic transformation that made
the measured implementation worse, illustrating why algebra alone is
insufficient.

The buffering experiment reserves one of four result slots at each accepted
EOF and releases it on an output handshake. Registered credits control ingress;
the two decoder-valid stages advance independently of the sink. Four snapshot
banks replace the old final register, preserving the three-cycle first-result
latency. The existing transport driver checks four pending results, complete
drain, mixed rejection and reset of all four results. The actual RTL also passes
15,512 shared packet-stack fixtures and a separate 163-record sample/overflow
corpus, with recurring 32-cycle stalls and pending-traffic reset. The broad
corpus includes all registered stack truncations, option and fragment fixtures
plus the long frames. Its reference exporter compares the existing normal and
generated software implementations and the real EverParse C oracle; no second
packet parser is introduced for the replay.

An output-only temporal property is insufficient for induction over arbitrary
hidden FIFO contents. The strengthened existing proof observes actual occupied
slots and in-flight snapshots, checking their IDs, framing, bounds and count
conservation against its independent EOF queue. These unstable observer views
add no registers or exported production ports: the emitted RTL and manifest
remain byte-identical with the views present. Both packing profiles close
temporal induction at step three for the small elaborated configuration.
The bounded pipeline equivalence proof and all 19 structured cases at 1,000
repeats in both bounded and streaming C-backed differential lanes also pass.
These proofs do not establish arbitrary codec/configuration equivalence.

Mapped-netlist connectivity confirms that output-ready's combinational cone
reaches eight flip-flops, versus all 2,482 in the preceding design. This counts
register endpoints, stopping at the first register; it is not a delay estimate.
Area increases from 4,909 logic cells / 2,482 FF to 6,328 / 4,009. The extra
banks and output mux therefore have a material cost. The read mux lies on an
output path, so integrated adapter/interface timing remains essential.
Across the same seeds, frequencies are 120.67 / 125.91 / 124.64 MHz. The mean
gains 1.13% over the preceding design, with changes of −0.21%, +3.66% and
−0.05% per seed. This is approximately unchanged core timing with much better
control isolation, rather than a large clock-speed win. It brings the complete
campaign's mean gain to about 24% over the original design. When all four
credits are occupied, resuming output leaves a one-cycle ingress restart bubble;
steady unstalled operation still accepts one complete frame per cycle.

The initial four-way read mux exposed nonblocking assignments in combinational
Verilog from Hardcaml v0.17.1. An explicit balanced binary mux tree avoids that
emitter issue; strict Verilator checks remain enabled, with no warning waiver
or generated-Verilog rewriting.

## Acceptance of a change

Use identical tool versions, device, constraints and corpus for each A/B.
Record area, timing endpoints and routing/logic delay, then check additional
placement seeds before treating a small timing change as robust. Keep the
ordinary codec rules, malformed-mask handling, overflow scope, consumption,
scalar values, frame IDs, backpressure and reset behavior unchanged.

Reuse the existing C-backed differential framework, focused transport tests,
small-instance temporal proofs, and emitted-RTL capture verifier. The temporal
proofs cover their elaborated configurations; EverParse's C proof does not
automatically prove the RTL. No optimization may omit checks to improve a
benchmark, and no cloud resource is launched by this audit.

The hardware CI lane installs OCaml 5.4.1 and Hardcaml v0.17.1, uses the existing
pinned EverParse installer, and names both differential suites and the transport
tests explicitly. It also runs the existing mask, streaming-control, bounded
padding and bounded-pipeline proofs, retaining their logs. The workflow's YAML
and shell syntax were checked locally; execution on GitHub remains a separate
CI result.

The maintainability cleanup extracts data-only emitter state and focused node,
bounds and load helpers while retaining one checked-plan traversal. Simulator
setup, cycle observation and proof invariants have shared helpers as well.
The production RTL and all temporal miters remain byte-identical. Typed lint
reports no actionable findings; two existing E724 matcher false positives
remain on the correctly conjunctive AFL/backend-enable guards. Those guards
were preserved, with no exclusions or warning suppressions.
