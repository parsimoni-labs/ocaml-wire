# Packet stack benchmark

The packet traversal is `Packet_stack.codec` in `examples/net`. The
`fused stack` and `selected outputs` rows run the scanner
`Wire_ocaml.scanner` writes from this codec, for the OCaml target on OCaml
and for the OxCaml target on OxCaml, and the scanner `Wire_rust.scanner`
writes from the same plan; `Wire.Codec stack decode` decodes the
same codec. The `linked headers` rows, OCaml and generated Rust, read the
fixed header codecs of `headers.ml` and link the layers in the harness. The
Rust crate rows traverse the same layers. Every row computes the same
checksum.

Run `make bench-rust`. It writes four corpora (64-byte packets without
options and 128-byte slots with IPv4 and TCP options and varied lengths,
each in random and in kind-sorted order; see `bench/README.md`), runs every
row on each, and prints one table per corpus with the best, p10, median and
p90 process CPU time and the best and median wall time per packet, ranked by
best CPU time, then the best CPU time of each row on every corpus. Row names
record the compiler versions. All rows must agree on the checksum of each
corpus family. Keep validation, fragment handling and checksum work
identical when changing an implementation.

`make bench-rust BUILD_EVERPARSE=1` adds the two EverParse C rows
(`everparse/`): the validator EverParse verifies and extracts from the same
`Packet_stack.codec`, called once per slot in a C loop. `EverParse C,
default plug` reads every field from the default `EthernetStack_Fields`
plug, nested IPv4, TCP and UDP members and presence flags included.
`EverParse C, inline plug` (`everparse/stack_inline.c`) reads the same
members from a plug copied from the shipped `EthernetStack_Fields.c` and
trimmed to them, its setters `static inline` in the validator's translation
unit. Both time their passes in C, as the Rust harness does, and print the
same `RESULT` lines.

## Measurements

Both harnesses read the process CPU time (`getrusage`, user and system)
around every timed pass, next to the wall clock, and the report ranks on
the best CPU time. On a loaded host the wall clock of a pass includes the
time the process waited descheduled. CPU time does not remove every effect
of load: on a host with performance and efficiency cores the same pass
takes different CPU times on different cores and clocks, and other
processes still evict caches and train the branch predictors, so read the
p10 and median columns too.

`make bench-rust-count` counts the instructions and cycles each row spends
per packet with `bench/count.sh`, as `make bench-apps-count` does for the
applications: the row alone with 1 and with 21 timed passes, the difference
divided by 20 passes, so the process's fixed costs drop out. The numbers
below are the median of ten runs with its defaults, 200,000 packets per
corpus, measured at commit `aceaeb80` (the parent of the commit that adds
them, which changes only READMEs) on an ARM64 macOS host under a load
average of 145 to 380. The cycles move with the load and the core a row
lands on, the instructions do not. OCaml 5.4.1 with `-inline 50`; OCaml
5.4.1 with flambda (switch `5.4.1-flambda`, `-O2`); OxCaml 5.4.0+ox
(switch `5.4.0-ox9`, `-O4`), each on every unit as `bench/README.md`
(Compiler flags) explains;
rustc 1.94.1 with etherparse 0.21.0, smoltcp 0.14.0 and pnet_packet 0.35.0,
as `cargo/Cargo.lock` pins them. The four OxCaml batch rows and the two
`wire.rust` scanner rows were counted again at commit `922e2bce`, after
the OxCaml frame loop started loading each frame's IPv4 protocol ahead of
its walk: the median of forty runs, the six rows interleaved on each
corpus with their order rotated from run to run, under a load average of
120 to 200. Interleaving makes the rows share the host's load, which ten
runs taken one row after another do not; the medians still move by about
5% from one such session to the next. The corpora are deterministic:
`bench.exe corpus DIR 200000` writes them with these SHA-256 digests.

| Corpus | SHA-256 |
| --- | --- |
| fixed64-random | `c1982f4d5d15777677626271e089525be14ca54d92ca5b6b304974614b0fc439` |
| fixed64-sorted | `d84afd2a272f127b7e4475d8d3db5861755a90f73c14eb0ff17706256bb47053` |
| mix128-random | `eb051b87d495915d8f64fcbc101b021cec549933bce3f31d8eb1a555fd2716f0` |
| mix128-sorted | `931fc45b7033d274f304753455284eacf77f1063d3424221c6de88041680c6d6` |

Instructions / cycles per packet, ordered by the instructions on
`fixed64-random`:

| Row | fixed64 random | fixed64 sorted | mix128 random | mix128 sorted |
| --- | ---: | ---: | ---: | ---: |
| wire.ocaml selected outputs, unboxed batch, OxCaml | 65.4 / 24.9 | 65.3 / 18.4 | 64.5 / 25.6 | 64.2 / 14.2 |
| wire.ocaml selected outputs, batch, OxCaml | 66.7 / 24.2 | 66.7 / 18.1 | 65.5 / 24.9 | 65.5 / 14.2 |
| wire.rust, linked headers | 68.6 / 24.4 | 68.6 / 18.8 | 67.0 / 25.2 | 66.7 / 14.9 |
| wire.ocaml fused stack, unboxed batch, OxCaml | 70.0 / 25.3 | 70.1 / 19.4 | 68.8 / 26.1 | 68.8 / 15.3 |
| wire.ocaml fused stack, batch, OxCaml | 71.4 / 25.2 | 71.4 / 19.1 | 70.2 / 25.6 | 70.0 / 15.3 |
| wire.rust selected outputs | 73.5 / 24.3 | 73.5 / 19.1 | 72.0 / 25.4 | 71.8 / 14.9 |
| wire.rust fused stack | 80.6 / 26.4 | 80.5 / 21.2 | 78.4 / 26.8 | 78.4 / 16.0 |
| smoltcp 0.14 | 82.3 / 30.6 | 82.2 / 24.2 | 80.5 / 30.6 | 80.6 / 17.7 |
| wire.ocaml selected outputs, batch, OCaml flambda | 87.0 / 32.3 | 86.8 / 26.9 | 85.8 / 34.0 | 85.9 / 23.1 |
| wire.ocaml fused stack, batch, OCaml flambda | 87.3 / 32.4 | 87.2 / 27.4 | 86.2 / 35.2 | 86.0 / 21.1 |
| wire.ocaml fused stack, batch, OCaml | 88.2 / 33.9 | 88.8 / 26.4 | 87.5 / 35.8 | 87.5 / 22.6 |
| wire.ocaml selected outputs, batch, OCaml | 88.5 / 33.0 | 88.6 / 27.3 | 87.8 / 35.5 | 87.1 / 22.6 |
| wire.ocaml selected outputs, fold, OxCaml | 88.8 / 32.4 | 88.7 / 25.1 | 87.5 / 34.5 | 87.2 / 20.8 |
| wire.ocaml fused stack, fold, OxCaml | 90.6 / 32.0 | 90.7 / 26.7 | 89.3 / 33.8 | 89.2 / 21.5 |
| pnet_packet 0.35 | 114 / 32.9 | 114 / 25.9 | 110 / 32.8 | 110 / 21.6 |
| wire.ocaml fused stack, fold, OCaml flambda | 122 / 40.0 | 122 / 34.6 | 121 / 42.6 | 121 / 27.1 |
| wire.ocaml selected outputs, fold, OCaml flambda | 122 / 39.7 | 122 / 34.6 | 121 / 41.8 | 121 / 27.4 |
| etherparse 0.21 | 130 / 32.5 | 130 / 26.9 | 126 / 33.0 | 126 / 21.5 |
| wire.ocaml fused stack, fold, OCaml | 136 / 42.5 | 136 / 36.3 | 135 / 43.9 | 135 / 31.4 |
| wire.ocaml fused stack, OxCaml | 140 / 40.0 | 140 / 33.8 | 137 / 43.0 | 138 / 29.4 |
| wire.ocaml fused stack, unboxed API, OxCaml | 141 / 38.1 | 141 / 31.6 | 139 / 39.7 | 139 / 24.5 |
| wire.ocaml selected outputs, fold, OCaml | 143 / 43.0 | 143 / 36.0 | 142 / 44.9 | 142 / 29.4 |
| wire.ocaml View.check/view, linked headers, OxCaml | 164 / 47.7 | 164 / 40.8 | 161 / 51.8 | 161 / 35.5 |
| EverParse C, inline plug | 165 / 36.1 | 165 / 30.7 | 164 / 36.5 | 165 / 25.1 |
| wire.ocaml View.check/view, linked headers, OCaml | 212 / 58.1 | 212 / 50.2 | 209 / 60.5 | 209 / 43.6 |
| wire.ocaml View.check/view, linked headers, OCaml flambda | 224 / 58.1 | 224 / 51.0 | 220 / 61.7 | 220 / 47.5 |
| wire.ocaml fused stack, OCaml flambda | 273 / 65.0 | 272 / 57.9 | 268 / 67.8 | 268 / 50.9 |
| wire.ocaml fused stack, OCaml | 477 / 129 | 477 / 121 | 471 / 130 | 471 / 112 |
| EverParse C, default plug | 678 / 121 | 678 / 115 | 689 / 124 | 688 / 108 |
| Wire.Codec check/read, linked headers, OCaml | 681 / 139 | 681 / 130 | 670 / 136 | 669 / 117 |
| Wire.Codec check/read, linked headers, OxCaml | 713 / 142 | 713 / 133 | 701 / 142 | 700 / 124 |
| Wire.Codec check/read, linked headers, OCaml flambda | 740 / 146 | 740 / 141 | 727 / 148 | 727 / 130 |
| Wire.Codec stack decode, OxCaml | 2943 / 694 | 2943 / 651 | 3013 / 719 | 3013 / 659 |
| Wire.Codec stack decode, OCaml | 3005 / 699 | 3004 / 648 | 3098 / 727 | 3097 / 660 |
| Wire.Codec stack decode, OCaml flambda | 3147 / 705 | 3147 / 659 | 3249 / 735 | 3249 / 670 |

Every row gave checksum `886803207481188` on both `fixed64` corpora and
`836859001324973` on both `mix128` corpora. A row spends the same
instructions on the random and the sorted order of a family; the sorted
order saves cycles where the branches on the kind of packet predict.

The two EverParse C rows are `make bench-rust-count BUILD_EVERPARSE=1`,
counted the same way at commit `5dc37350` with the inline-plug row added,
the median of five runs, under a load average of 310 to 340, with EverParse
3d v2026.10.27; their C was compiled by Apple clang 21.0.0 at `-O2`. The
default-plug row counted within 0.5% of its instructions at `5dc37350`
itself. In the default-plug row each field reaches the plug through an
out-of-line setter in another translation unit, 30 calls for a TCP packet,
and the whole 35-member plug is zeroed per slot. The inline-plug row
compiles the setters into the validator, where each folds to one store or
none, and zeroes a struct of the 19 members it reads: the 513 instructions
per packet between the two rows are the plug, and the inline-plug row's 165
are the validator's own work.

The `fold` and `batch` rows call the scanner's `fold_step` and
`fold_frames_step`, which call the checksum module `Step` by name. Stock
OCaml cannot inline a function passed as an argument. When `fold_step` was
added, on the `fixed64-random` corpus of that time, the stock batch through
`fold_frames` and a closure called `valid` through `caml_apply20`,
allocated a five-word closure per frame and took 31.30 (full outputs) and
20.18 (selected) ns/packet, against 15.38 and 14.86 by name, with no
allocation and no call per frame. The stock `decode` row still builds its
record through `fold`'s closures. `Step` takes the outputs as `int64` and
folds them as `int64`, as the unboxed OxCaml rows fold `int64_u` and the
Rust rows `u64`.
