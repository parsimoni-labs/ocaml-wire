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
corpus, measured at commit `2ef217b4` (the parent of the commit that adds
them, which changes only READMEs) on an ARM64 macOS host under a load
average of 8 to 90. OCaml 5.4.1 with `-inline 50`; OCaml 5.4.1 with flambda
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
| wire.rust, linked headers | 68.5 / 20.4 | 68.5 / 14.7 | 66.7 / 19.7 | 66.7 / 10.9 |
| wire.rust selected outputs | 75.3 / 20.4 | 75.3 / 16.5 | 73.8 / 20.0 | 73.6 / 10.9 |
| wire.ocaml selected outputs, unboxed batch, OxCaml | 75.7 / 21.8 | 75.7 / 16.0 | 74.9 / 27.2 | 74.8 / 11.4 |
| wire.ocaml fused stack, unboxed batch, OxCaml | 77.7 / 21.5 | 77.7 / 15.7 | 76.7 / 23.4 | 76.8 / 11.4 |
| smoltcp 0.14 | 81.9 / 24.4 | 82.0 / 23.9 | 80.4 / 28.1 | 80.3 / 11.4 |
| wire.rust fused stack | 82.5 / 23.1 | 82.4 / 19.8 | 80.2 / 22.3 | 80.1 / 12.0 |
| wire.ocaml selected outputs, batch, OxCaml | 85.8 / 24.3 | 85.6 / 16.3 | 84.3 / 23.0 | 84.2 / 13.7 |
| wire.ocaml fused stack, batch, OxCaml | 87.7 / 22.5 | 87.7 / 17.3 | 86.2 / 27.5 | 86.2 / 12.3 |
| wire.ocaml selected outputs, fold, OxCaml | 104 / 30.1 | 104 / 20.8 | 102 / 31.9 | 102 / 16.2 |
| wire.ocaml fused stack, fold, OxCaml | 106 / 28.1 | 106 / 20.5 | 104 / 30.9 | 104 / 15.6 |
| pnet_packet 0.35 | 113 / 29.3 | 113 / 21.8 | 110 / 27.3 | 110 / 17.0 |
| wire.ocaml fused stack, batch, OCaml flambda | 126 / 35.0 | 126 / 26.0 | 124 / 33.1 | 124 / 18.6 |
| wire.ocaml selected outputs, batch, OCaml flambda | 126 / 32.7 | 126 / 24.5 | 125 / 38.0 | 124 / 20.2 |
| etherparse 0.21 | 129 / 29.2 | 129 / 25.0 | 126 / 31.2 | 126 / 17.2 |
| wire.ocaml fused stack, batch, OCaml | 138 / 34.1 | 138 / 26.6 | 136 / 38.0 | 136 / 23.8 |
| wire.ocaml selected outputs, batch, OCaml | 138 / 37.6 | 138 / 26.3 | 136 / 36.4 | 136 / 21.1 |
| wire.ocaml fused stack, OxCaml | 146 / 36.8 | 146 / 29.8 | 144 / 39.0 | 144 / 20.7 |
| wire.ocaml fused stack, unboxed API, OxCaml | 147 / 35.2 | 147 / 27.6 | 145 / 36.4 | 146 / 21.6 |
| wire.ocaml fused stack, fold, OCaml flambda | 150 / 41.3 | 150 / 28.5 | 148 / 37.9 | 148 / 21.7 |
| wire.ocaml selected outputs, fold, OCaml flambda | 150 / 40.3 | 150 / 28.6 | 148 / 38.2 | 148 / 23.9 |
| wire.ocaml View.check/view, linked headers, OxCaml | 164 / 38.4 | 163 / 30.3 | 161 / 46.8 | 161 / 23.2 |
| wire.ocaml fused stack, fold, OCaml | 168 / 43.2 | 168 / 31.6 | 166 / 44.7 | 166 / 30.0 |
| wire.ocaml selected outputs, fold, OCaml | 168 / 40.9 | 168 / 32.2 | 166 / 44.7 | 167 / 24.4 |
| wire.ocaml View.check/view, linked headers, OCaml | 212 / 43.8 | 212 / 35.4 | 209 / 51.9 | 209 / 34.1 |
| wire.ocaml View.check/view, linked headers, OCaml flambda | 223 / 47.1 | 223 / 36.1 | 220 / 45.3 | 220 / 28.9 |
| wire.ocaml fused stack, OCaml flambda | 277 / 55.7 | 277 / 46.7 | 273 / 53.8 | 273 / 34.6 |
| wire.ocaml fused stack, OCaml | 482 / 109 | 482 / 99.1 | 476 / 115 | 475 / 105 |
| EverParse C, default plug | 759 / 127 | 759 / 124 | 771 / 128 | 771 / 117 |
| Wire.Codec valid/read, linked headers, OCaml | 952 / 144 | 952 / 125 | 870 / 136 | 870 / 129 |
| Wire.Codec valid/read, linked headers, OxCaml | 988 / 148 | 988 / 134 | 905 / 144 | 905 / 113 |
| Wire.Codec valid/read, linked headers, OCaml flambda | 1036 / 171 | 1036 / 167 | 946 / 131 | 946 / 112 |
| Wire.Codec stack decode, OxCaml | 7647 / 1333 | 7649 / 1319 | 7312 / 1354 | 7312 / 1185 |
| Wire.Codec stack decode, OCaml | 8504 / 1389 | 8505 / 1306 | 8138 / 1453 | 8138 / 1234 |
| Wire.Codec stack decode, OCaml flambda | 9399 / 1516 | 9399 / 1371 | 8998 / 1417 | 8996 / 1227 |

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
record through `fold`'s closures.
