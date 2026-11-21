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
below are the median of ten runs with its defaults, 200,000 packets per
corpus, measured at commit `aceaeb80` (the parent of the commit that adds
them, which changes only READMEs) on an ARM64 macOS host under a load
average of 145 to 380. The cycles move with the load and the core a row
lands on, the instructions do not. OCaml 5.4.1 with `-inline 50`; OCaml
5.4.1 with flambda (switch `5.4.1-flambda`, `-O2`); OxCaml 5.4.0+ox
(switch `5.4.0-ox9`, `-O4`), each on every unit as `bench/README.md`
(Compiler flags) explains;
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
| wire.ocaml selected outputs, unboxed batch, OxCaml | 64.2 / 26.6 | 64.2 / 20.4 | 62.9 / 27.0 | 63.1 / 16.1 |
| wire.ocaml selected outputs, batch, OxCaml | 65.0 / 26.4 | 65.0 / 20.0 | 63.8 / 27.6 | 64.0 / 15.2 |
| wire.ocaml fused stack, unboxed batch, OxCaml | 65.8 / 26.3 | 66.0 / 20.0 | 65.1 / 28.0 | 65.1 / 16.2 |
| wire.ocaml fused stack, batch, OxCaml | 66.8 / 26.3 | 66.7 / 21.1 | 65.5 / 27.5 | 65.7 / 16.0 |
| wire.rust, linked headers | 68.6 / 24.5 | 68.9 / 19.4 | 66.8 / 25.2 | 66.8 / 14.9 |
| wire.rust selected outputs | 73.7 / 24.6 | 73.6 / 19.2 | 72.1 / 25.3 | 71.8 / 16.4 |
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
| wire.ocaml View.check/view, linked headers, OCaml | 212 / 58.1 | 212 / 50.2 | 209 / 60.5 | 209 / 43.6 |
| wire.ocaml View.check/view, linked headers, OCaml flambda | 224 / 58.1 | 224 / 51.0 | 220 / 61.7 | 220 / 47.5 |
| wire.ocaml fused stack, OCaml flambda | 273 / 65.0 | 272 / 57.9 | 268 / 67.8 | 268 / 50.9 |
| wire.ocaml fused stack, OCaml | 477 / 129 | 477 / 121 | 471 / 130 | 471 / 112 |
| EverParse C, default plug | 677 / 120 | 676 / 113 | 687 / 121 | 688 / 107 |
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

The EverParse C row is `make bench-rust-count BUILD_EVERPARSE=1`, counted
the same way at the same commit with EverParse 3d v2026.10.27; its C was
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
