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
below are the median of three runs with its defaults, 200,000 packets per
corpus, measured at commit `0974d978` (the parent of the commit that adds
them, which changes only READMEs) on an ARM64 macOS host under a load
average near 8. OCaml 5.4.1 with `-inline 50`; OCaml 5.4.1 with flambda
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
| wire.rust, linked headers | 68.5 / 18.4 | 68.5 / 14.2 | 66.7 / 19.6 | 66.7 / 10.0 |
| wire.rust selected outputs | 75.3 / 19.4 | 75.3 / 15.3 | 73.6 / 20.4 | 73.6 / 8.8 |
| smoltcp 0.14 | 81.9 / 23.3 | 81.9 / 18.4 | 80.3 / 23.6 | 80.4 / 10.2 |
| wire.rust fused stack | 82.4 / 20.2 | 82.3 / 15.7 | 80.2 / 20.4 | 80.2 / 10.0 |
| wire.ocaml fused stack, unboxed batch, OxCaml | 97.7 / 27.7 | 97.7 / 21.5 | 96.5 / 27.8 | 96.4 / 13.6 |
| wire.ocaml selected outputs, unboxed batch, OxCaml | 97.7 / 27.4 | 97.7 / 20.1 | 96.4 / 27.0 | 96.3 / 14.5 |
| wire.ocaml fused stack, batch, OxCaml | 109 / 28.6 | 109 / 21.7 | 107 / 30.0 | 107 / 15.8 |
| wire.ocaml selected outputs, batch, OxCaml | 109 / 28.1 | 109 / 21.8 | 107 / 30.1 | 107 / 15.8 |
| pnet_packet 0.35 | 113 / 26.0 | 114 / 20.5 | 110 / 26.2 | 110 / 14.9 |
| wire.ocaml fused stack, fold, OxCaml | 123 / 32.7 | 123 / 24.4 | 121 / 34.9 | 121 / 18.8 |
| wire.ocaml selected outputs, fold, OxCaml | 123 / 34.7 | 123 / 24.1 | 121 / 34.9 | 121 / 19.1 |
| etherparse 0.21 | 129 / 27.8 | 129 / 24.5 | 126 / 27.6 | 126 / 17.4 |
| wire.ocaml fused stack, batch, OCaml flambda | 131 / 32.5 | 131 / 25.7 | 130 / 35.2 | 130 / 18.8 |
| wire.ocaml selected outputs, batch, OCaml flambda | 131 / 32.9 | 131 / 25.8 | 130 / 34.9 | 130 / 18.8 |
| wire.ocaml selected outputs, fold, OCaml flambda | 150 / 36.3 | 150 / 29.3 | 149 / 38.0 | 149 / 21.7 |
| wire.ocaml fused stack, fold, OCaml flambda | 150 / 35.9 | 150 / 29.3 | 149 / 38.1 | 149 / 21.7 |
| wire.ocaml fused stack, batch, OCaml | 153 / 36.0 | 154 / 29.3 | 152 / 38.3 | 152 / 20.8 |
| wire.ocaml selected outputs, batch, OCaml | 153 / 34.6 | 154 / 29.2 | 152 / 38.2 | 152 / 20.7 |
| wire.ocaml fused stack, unboxed API, OxCaml | 159 / 40.1 | 159 / 31.1 | 158 / 41.3 | 158 / 21.2 |
| wire.ocaml fused stack, OxCaml | 165 / 40.5 | 165 / 32.3 | 162 / 42.5 | 162 / 22.5 |
| wire.ocaml fused stack, fold, OCaml | 170 / 40.0 | 170 / 33.2 | 168 / 43.3 | 168 / 24.0 |
| wire.ocaml selected outputs, fold, OCaml | 170 / 38.1 | 170 / 33.6 | 169 / 42.7 | 168 / 23.8 |
| wire.ocaml View.check/view, linked headers, OxCaml | 173 / 39.4 | 173 / 31.6 | 170 / 40.8 | 170 / 23.6 |
| wire.ocaml View.check/view, linked headers, OCaml | 212 / 43.4 | 212 / 34.8 | 209 / 44.3 | 209 / 28.4 |
| wire.ocaml View.check/view, linked headers, OCaml flambda | 223 / 45.0 | 223 / 36.0 | 220 / 45.8 | 220 / 28.9 |
| wire.ocaml fused stack, OCaml flambda | 279 / 52.6 | 279 / 45.4 | 275 / 53.8 | 275 / 35.5 |
| wire.ocaml fused stack, OCaml | 480 / 105 | 480 / 98.7 | 475 / 108 | 474 / 94.1 |
| EverParse C, default plug | 760 / 118 | 760 / 113 | 774 / 121 | 774 / 109 |
| Wire.Codec valid/read, linked headers, OCaml | 952 / 137 | 952 / 125 | 870 / 122 | 870 / 100 |
| Wire.Codec valid/read, linked headers, OxCaml | 987 / 145 | 988 / 135 | 905 / 133 | 905 / 113 |
| Wire.Codec valid/read, linked headers, OCaml flambda | 1036 / 151 | 1036 / 139 | 947 / 136 | 946 / 112 |
| Wire.Codec stack decode, OxCaml | 7650 / 1305 | 7651 / 1272 | 7309 / 1224 | 7309 / 1144 |
| Wire.Codec stack decode, OCaml | 8502 / 1300 | 8504 / 1229 | 8144 / 1302 | 8145 / 1143 |
| Wire.Codec stack decode, OCaml flambda | 9398 / 1444 | 9401 / 1373 | 8998 / 1384 | 8998 / 1244 |

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
