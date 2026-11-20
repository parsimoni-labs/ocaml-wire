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

`make bench-rust BUILD_EVERPARSE=1` adds the EverParse C row
(`everparse/`): the validator EverParse verifies and extracts from the same
`Packet_stack.codec`, called once per slot in a C loop, with every field
read from the default `EthernetStack_Fields` plug, nested IPv4, TCP and UDP
members and presence flags included. It times its passes in C, as the Rust
harness does, and prints the same `RESULT` lines.

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
below are the median of five runs with its defaults, 200,000 packets per
corpus, measured at commit `86864e63` (the parent of the commit that adds
them, which changes only READMEs) on an ARM64 macOS host under a load
average of 20 to 43. OCaml 5.4.1 with `-inline 50`; OCaml 5.4.1 with flambda
(switch `5.4.1-flambda`, `-O2`); OxCaml 5.4.0+ox (switch `5.4.0-ox9`,
`-O4`), each on every unit as `bench/README.md` (Compiler flags) explains;
rustc 1.94.1 with etherparse 0.21.0, smoltcp 0.14.0 and pnet_packet 0.35.0,
as `cargo/Cargo.lock` pins them. The corpora are deterministic: `bench.exe
corpus DIR 200000` writes them with these SHA-256 digests.

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
| wire.rust, linked headers | 68.7 / 22.9 | 68.7 / 19.5 | 66.8 / 24.3 | 66.7 / 14.0 |
| wire.rust selected outputs | 75.5 / 24.6 | 75.4 / 17.7 | 73.7 / 26.0 | 73.8 / 14.1 |
| wire.ocaml selected outputs, unboxed batch, OxCaml | 75.9 / 23.4 | 75.8 / 21.0 | 74.5 / 28.4 | 75.0 / 15.2 |
| wire.ocaml fused stack, unboxed batch, OxCaml | 77.9 / 24.6 | 77.8 / 16.8 | 77.0 / 26.4 | 76.8 / 13.2 |
| smoltcp 0.14 | 82.1 / 31.0 | 82.2 / 22.4 | 80.5 / 27.3 | 80.3 / 14.7 |
| wire.rust fused stack | 82.5 / 25.3 | 82.6 / 21.8 | 80.2 / 28.0 | 80.1 / 14.6 |
| wire.ocaml selected outputs, batch, OxCaml | 85.8 / 26.1 | 85.7 / 21.4 | 84.8 / 32.5 | 84.2 / 14.5 |
| wire.ocaml fused stack, batch, OxCaml | 87.9 / 26.1 | 87.7 / 20.1 | 86.3 / 30.6 | 86.5 / 18.3 |
| wire.ocaml selected outputs, fold, OxCaml | 104 / 33.1 | 104 / 24.4 | 103 / 35.2 | 102 / 21.0 |
| wire.ocaml fused stack, fold, OxCaml | 106 / 31.4 | 106 / 23.4 | 105 / 34.8 | 104 / 23.4 |
| pnet_packet 0.35 | 114 / 31.5 | 113 / 26.1 | 110 / 31.2 | 110 / 19.5 |
| wire.ocaml selected outputs, batch, OCaml flambda | 127 / 41.8 | 126 / 33.4 | 125 / 45.0 | 124 / 28.4 |
| wire.ocaml fused stack, batch, OCaml flambda | 127 / 39.4 | 126 / 33.1 | 125 / 43.5 | 125 / 22.0 |
| etherparse 0.21 | 129 / 33.9 | 130 / 26.6 | 126 / 33.4 | 126 / 20.1 |
| wire.ocaml fused stack, batch, OCaml | 138 / 42.0 | 138 / 31.3 | 137 / 43.2 | 136 / 27.8 |
| wire.ocaml selected outputs, batch, OCaml | 138 / 40.7 | 138 / 34.0 | 136 / 45.7 | 136 / 25.9 |
| wire.ocaml fused stack, OxCaml | 146 / 41.1 | 147 / 32.1 | 144 / 42.7 | 144 / 27.8 |
| wire.ocaml fused stack, unboxed API, OxCaml | 147 / 37.5 | 147 / 28.5 | 145 / 40.1 | 145 / 22.9 |
| wire.ocaml fused stack, fold, OCaml flambda | 150 / 44.3 | 150 / 38.4 | 149 / 49.3 | 149 / 25.5 |
| wire.ocaml selected outputs, fold, OCaml flambda | 151 / 44.0 | 150 / 36.9 | 149 / 46.3 | 149 / 32.9 |
| wire.ocaml View.check/view, linked headers, OxCaml | 164 / 45.3 | 164 / 36.1 | 161 / 47.0 | 162 / 32.6 |
| wire.ocaml fused stack, fold, OCaml | 168 / 44.9 | 168 / 36.4 | 167 / 47.2 | 166 / 33.3 |
| wire.ocaml selected outputs, fold, OCaml | 168 / 46.4 | 168 / 40.8 | 166 / 47.0 | 167 / 30.5 |
| wire.ocaml View.check/view, linked headers, OCaml | 212 / 48.8 | 212 / 45.5 | 209 / 59.4 | 209 / 34.3 |
| wire.ocaml View.check/view, linked headers, OCaml flambda | 224 / 51.6 | 224 / 48.9 | 220 / 57.1 | 220 / 37.1 |
| wire.ocaml fused stack, OCaml flambda | 277 / 63.9 | 277 / 54.9 | 274 / 65.2 | 273 / 46.6 |
| wire.ocaml fused stack, OCaml | 482 / 111 | 483 / 116 | 476 / 128 | 476 / 103 |
| Wire.Codec check/read, linked headers, OCaml | 698 / 122 | 698 / 104 | 686 / 136 | 687 / 117 |
| Wire.Codec check/read, linked headers, OxCaml | 732 / 124 | 731 / 115 | 719 / 138 | 718 / 125 |
| Wire.Codec check/read, linked headers, OCaml flambda | 757 / 138 | 756 / 135 | 744 / 144 | 744 / 116 |
| EverParse C, default plug | 760 / 128 | 760 / 121 | 775 / 133 | 774 / 116 |
| Wire.Codec stack decode, OxCaml | 2938 / 657 | 2937 / 592 | 3008 / 639 | 3005 / 642 |
| Wire.Codec stack decode, OCaml | 2996 / 616 | 2999 / 550 | 3088 / 668 | 3092 / 576 |
| Wire.Codec stack decode, OCaml flambda | 3140 / 662 | 3141 / 634 | 3239 / 725 | 3238 / 649 |

Every row gave checksum `886803207481188` on both `fixed64` corpora and
`836859001324973` on both `mix128` corpora. A row spends the same
instructions on the random and the sorted order of a family; the sorted
order saves cycles where the branches on the kind of packet predict.

The EverParse C row is `make bench-rust-count BUILD_EVERPARSE=1`, counted
the same way at the same commit with EverParse 3d v2026.08.21; its C was
compiled by Apple clang 21.0.0 at `-O2`.
Each field reaches the plug through an out-of-line setter in another
translation unit, 30 calls for a TCP packet, and the plug is zeroed per
slot; the run does not separate that cost from the validator's own.

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
