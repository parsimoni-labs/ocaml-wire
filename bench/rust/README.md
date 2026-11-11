# Packet stack benchmark

The packet traversal is `Packet_stack.codec` in `examples/net`. The
`fused stack` and `selected outputs` rows run the scanner
`Wire_ocaml.scanner` writes from this codec, for the OCaml target on OCaml
and for the OxCaml target on OxCaml; `Wire.Codec stack decode` decodes the
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

## Measurements

Both harnesses read the process CPU time (`getrusage`, user and system)
around every timed pass, next to the wall clock, and the report ranks on
the best CPU time. On a loaded host the wall clock of a pass includes the
time the process waited descheduled. CPU time does not remove every effect
of load: on a host with performance and efficiency cores the same pass
takes different CPU times on different cores and clocks, and other
processes still evict caches and train the branch predictors, so read the
p10 and median columns too.

One run of `make bench-rust` with its defaults, 200,000 packets per corpus and 21
timed passes per row, on an ARM64 macOS host whose load average fell from
68 to 52 during the run. OCaml 5.4.1 (no flambda); OxCaml 5.4.0+ox (switch
`5.4.0-ox9`); rustc 1.94.1 with etherparse 0.21.0, smoltcp 0.14.0 and
pnet_packet 0.35.0, as `cargo/Cargo.lock` pins them. The corpora are
deterministic: `bench.exe corpus DIR 200000` writes them with these SHA-256
digests.

| Corpus | SHA-256 |
| --- | --- |
| fixed64-random | `c1982f4d5d15777677626271e089525be14ca54d92ca5b6b304974614b0fc439` |
| fixed64-sorted | `d84afd2a272f127b7e4475d8d3db5861755a90f73c14eb0ff17706256bb47053` |
| mix128-random | `eb051b87d495915d8f64fcbc101b021cec549933bce3f31d8eb1a555fd2716f0` |
| mix128-sorted | `931fc45b7033d274f304753455284eacf77f1063d3424221c6de88041680c6d6` |

Best CPU ns/packet:

| Row | fixed64 random | fixed64 sorted | mix128 random | mix128 sorted |
| --- | ---: | ---: | ---: | ---: |
| wire.rust, linked headers | 6.60 | 5.52 | 7.78 | 3.87 |
| OxCaml selected outputs, unboxed batch | 7.90 | 9.02 | 11.74 | 4.88 |
| OxCaml fused stack, unboxed batch | 8.15 | 8.93 | 11.70 | 4.82 |
| smoltcp 0.14 | 8.54 | 6.93 | 8.94 | 4.68 |
| OxCaml selected outputs, batch | 8.65 | 10.36 | 13.27 | 5.39 |
| OxCaml fused stack, batch | 9.07 | 10.18 | 13.11 | 5.40 |
| pnet_packet 0.35 | 9.34 | 7.62 | 9.54 | 6.13 |
| etherparse 0.21 | 9.36 | 7.79 | 8.96 | 5.76 |
| OxCaml selected outputs, fold | 9.77 | 10.55 | 14.35 | 6.50 |
| OxCaml fused stack, unboxed API | 9.81 | 11.54 | 14.57 | 7.03 |
| OxCaml fused stack, fold | 10.48 | 11.14 | 14.24 | 6.33 |
| OxCaml fused stack, decode | 11.45 | 12.96 | 16.13 | 7.71 |
| OCaml selected outputs, batch | 12.34 | 10.53 | 15.30 | 10.49 |
| OCaml fused stack, batch | 12.38 | 10.41 | 15.57 | 8.50 |
| OCaml selected outputs, fold | 13.41 | 11.88 | 16.83 | 9.75 |
| OxCaml View.check/view, linked headers | 13.96 | 15.29 | 20.00 | 10.44 |
| OCaml fused stack, fold | 14.24 | 11.80 | 16.44 | 12.05 |
| OCaml View.check/view, linked headers | 20.40 | 17.34 | 23.22 | 16.93 |
| OCaml fused stack, decode | 36.24 | 32.47 | 41.67 | 33.97 |
| OCaml `Wire.Codec` valid/read, linked headers | 46.02 | 42.09 | 48.49 | 38.56 |
| OxCaml `Wire.Codec` valid/read, linked headers | 47.95 | 62.50 | 61.29 | 46.92 |
| OxCaml `Wire.Codec` stack decode | 716.49 | 749.28 | 979.71 | 874.90 |
| OCaml `Wire.Codec` stack decode | 806.86 | 825.16 | 801.03 | 722.45 |

Every row gave checksum `886803207481188` on both `fixed64` corpora and
`836859001324973` on both `mix128` corpora. On sorted corpora the best of
the OxCaml rows often lies well below their median, so one run does not
order rows within about 1 ns/packet there.

The `fold` and `batch` rows call the scanner's `fold_step` and
`fold_frames_step`, which call the checksum module `Step` by name. Stock
OCaml cannot inline a function passed as an argument. When `fold_step` was
added, on the `fixed64-random` corpus of that time, the stock batch through
`fold_frames` and a closure called `valid` through `caml_apply20`,
allocated a five-word closure per frame and took 31.30 (full outputs) and
20.18 (selected) ns/packet, against 15.38 and 14.86 by name, with no
allocation and no call per frame. The stock `decode` row still builds its
record through `fold`'s closures.
