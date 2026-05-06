## unreleased

- Fix C stub generator: use the EverParse-normalised `<Name>Fields`
  type name. Schemas with a name segment of 2+ leading capitals
  (e.g. `IPv4`, `EP_Header`, `MC_Status_Reply`) previously emitted C
  that referenced an undefined struct.
- `Codec.bitfield`: dispatch on the bitfield base type at construction
  time rather than via `Bitfield.read_word base` partial application.
  The resulting word reader is a direct read of the right width with
  `byte_off` already baked in, eliminating one indirect closure call
  and a runtime `match` per `load_word` invocation. On the CLCW
  polling loop (4 bitfield extracts per 4-byte word) this lifts pure
  OCaml throughput from ~195 to ~205 Mword/s (-0.2 ns/word, +5%) on
  Apple M-series. Existing bitfield tests cover the unchanged
  behaviour. Pure refactor, no API change.
- Expose the byte-level word readers (`u16_le`, `u16_be`, `u32_le`,
  `u32_be`) in `Bitfield.mli` so the dispatch above can reach them.
- Add `bench/bitfield/`: stock-OCaml microbench comparing the old
  partial-application word reader against the post-refactor closure,
  for each of the five `bitfield_base` arms. Run with
  `make bench-bitfield` -- no EverParse required.
- `Wire.variants`: lift the lookup loop in `encode` to a top-level
  function. The previous `let rec go i = ... in go 0` closed over `v`
  and `arr`, allocating a fresh closure on every encode (~6w/op on the
  demo `CasesDemo.type` write case). Calling a top-level function
  takes 0w/op. No behavioural change.

## 0.9.0

Initial release.
