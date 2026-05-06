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
- `Wire.variants`: lift the lookup loop in `encode` to a top-level
  function. The previous `let rec go i = ... in go 0` closed over `v`
  and `arr`, allocating a fresh closure on every encode (~6w/op on the
  demo `CasesDemo.type` write case). Calling a top-level function
  takes 0w/op. No behavioural change.
- `Wire.cases`: same closure-capture fix as `variants` above; the two
  combinators shared the same `let rec go` anti-pattern.
- **Single decode/encode codepath.** `Wire.decode_string` /
  `Wire.decode_bytes` / `Wire.decode (Reader)` previously dispatched
  through `parse_with`, a streaming reader-based decoder that threaded
  an `Eval.ctx` (a `Map.Make (String)`) for cross-field references.
  The decoder is now a single `parse_direct : 'a typ -> bytes -> int ->
  int -> 'a * int` kernel handling all DSL types directly; streaming
  becomes a thin buffering layer. Struct types validate via the new
  `Codec.validator_of_struct` (the same int-array validation kernel as
  `Codec.decode`). No more parallel implementation, no more String
  Map. Same migration on the encode side: `Wire.encode` (Writer-based)
  goes through a single `encode_to_writer` kernel.
- `Codec.validator` / `Codec.validator_of_struct` / `Codec.validate_struct`
  / `Codec.struct_size_of` / `Codec.struct_min_size` /
  `Codec.wire_size_info_of_validator`: new public API exposing the
  int-array validation kernel for arbitrary `Types.struct_` (no record
  constructor required). Used internally by `Wire.decode_string` for
  `Struct` types.
- `Eval` module slimmed: `ctx` is now `unit`; `bind`, `get`, `set_pos`,
  and the streaming `action` interpreter are gone. Only `expr`
  (top-level, fails on `Ref`) and `int_of` remain.
- Deleted from `lib/wire.ml`: `parse_with`, `parse_struct_fields`,
  `parse_codec`, `parse_all_zeros`, `parse_bits`, `parse_bf_field`,
  `parse_int`, the buffered `decoder` type and its `refill`/`read_*`
  helpers, the `bf_accum` accumulator, `check_constraint`, and
  `apply_action`. The legacy reader-based decoder has no remaining
  surface area.
- `Codec.slice_offset` / `Codec.slice_length`: zero-allocation access
  to the offset/length of a `byte_slice` field. Replaces
  `Slice.first (Codec.get c f buf base)` (4w/op alloc per call) with
  a staged reader that returns the absolute byte offset directly
  (0w/op). On the demo `Eth->TCP.dst_port (3 layers)` write case the
  full chain drops from 8w/op to 0w/op. Type-restricted to
  `(Slice.t, _) field`, so passing a non-slice field is a compile-time
  error.
- **Single C entry point per FFI schema.** `Wire_stubs` previously
  emitted two C functions per schema: `caml_wire_<name>_parse`
  (validate, build record in C) and `caml_wire_<name>_parse_k`
  (validate, callback into OCaml continuation). Both did the same
  validation; only the result delivery differed. Drop the non-CPS C
  function -- only `_parse_k` is emitted now. The OCaml-side
  record-returning `<name>_parse` becomes a thin wrapper that calls
  `_parse_k` with a record-constructor continuation. Single C
  codepath per schema; record / CPS ergonomics both preserved on the
  OCaml side. Hot-loop callers should use `_parse_k` directly to
  avoid the extra C-to-OCaml callback hop the wrapper pays.
- `Codec.validator_of_struct`: now fires field actions (uses
  `validate_arr` with a pre-allocated scratch int array) and handles
  nested struct fields recursively. A `Struct`-typed field inside
  another struct previously tripped `compile_var_bytes` because
  `compile_field` doesn't accept `Struct` types directly; the
  validator now recognises that case, builds a sub-validator
  recursively, and inlines its `vt_validate` at the right offset.
  Inner field references stay scoped to the inner struct.
- Total demo-bench allocations on the consolidated FFI path drop from
  ~3.7 GB to ~1.2 GB (-67%) over the standard 10M-iteration workload.
  The remaining hits are the inherent OCaml `int64` box for `uint64`
  fields (boxed at the Bytes-read site and at the C-to-OCaml callback
  boundary).

## 0.9.0

Initial release.
