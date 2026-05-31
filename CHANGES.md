## unreleased

### Added

- `Field.optional` and `Field.optional_or` now accept a variable-size inner,
  so an optional field can be a length-prefixed string or a whole sub-message,
  not just a fixed-width value. Building such a codec previously raised
  `Invalid_argument` (#88, @samoht)
- Add `Wire.zeroterm` and `Wire.zeroterm_at_most ~size` for
  NUL-terminated strings: the bytes up to a terminator, or the same
  within a fixed-size region. They project to the 3D `field[:zeroterm]`
  and `field[:zeroterm-byte-size-at-most n]` forms (#77, @samoht)
- `Wire.casetype` now accepts any tag typ (`'k typ`, not just `int`) and
  projects cleanly to 3D: int-tagged casetypes get an auto-emitted
  `casetype_decl` + wrapper typedef; byte-tagged casetypes (e.g.
  `byte_array ~size`) project as two adjacent byte spans so dispatch
  happens in caller code, matching how OpenSSH and similar protocol
  parsers handle string-discriminated messages (#49, @samoht)
- Project a casetype case whose body is an embedded sub-codec to a
  separate 3D struct. Sub-codecs may end in `all_bytes`; the
  declaration order places the sub-codec before the dispatch decl
  that names it (#50, @samoht)
- `Field.repeat` now projects to 3D for variable-size elements (e.g.
  a sub-codec with its own length-prefixed bytes), emitting
  `<Elem> name[:byte-size budget]` (#51, @samoht)
- `Wire.Codec.size_of_value` (#58, @samoht)
- Support `Wire.casetype` and `Wire.nested ~size` as `Codec` fields
  (#47, @samoht)
- Add `Field.optional` / `Field.optional_or` / `Field.repeat` /
  `Field.repeat_seq` (#46, @samoht)
- Add `Wire.rest_bytes` for trailing "rest of buffer" fields, plus
  direct `all_bytes` / `all_zeros` support as `Codec` fields
  (#44, @samoht)
- Add signed integers `int8` / `int16(be)` / `int32(be)` / `int64(be)`
  (#42, @samoht)
- Add IEEE 754 floats `float32(be)` / `float64(be)` and `Wire.is_finite`
  / `Wire.is_nan` predicates (#42, @samoht)
- Add `Wire.byte_array_where ~size ~per_byte` for byte spans with a
  per-byte refinement (#40, @samoht)
- Add `Codec.validator_of_struct` / `validate_struct` / `struct_size_of`
  / `struct_min_size` (#37, @samoht)
- Add `Codec.slice_offset` / `Codec.slice_length` (#37, @samoht)
- Add `Wire.codec` type alias for `'r Codec.t` and `Wire.pp_value`
  (#39, @samoht)

### Changed

- Decoding a struct with variable-size fields (`byte_slice`, `byte_array`
  or `repeat` whose size is a cross-field expression) no longer allocates
  on each field access: the size/offset evaluator now takes `buf` and
  `base` as separate arguments instead of a per-call pair, and reads
  integer fields without boxing an `int option`. Pure speedup, no API
  change (#81, @samoht)
- Remove `Wire.optional` / `Wire.optional_or` / `Wire.repeat` /
  `Wire.repeat_seq` from the typ-level surface; use the matching
  `Field.*` combinators instead (#46, @samoht)
- Rename `Wire.decode_*` / `Wire.encode_*` to `of_string` / `of_bytes` /
  `of_reader` / `to_string` / `to_bytes` / `to_writer`; add `_exn` twins
  that raise on parse error (#39, @samoht)
- Fold `Codec.decode_with` into `Codec.decode` via `?env`; split into
  `Codec.decode` (result) and `Codec.decode_exn` (raises) (#39, @samoht)
- Speed up `Codec.bitfield` ~5% (#37, @samoht)
- Drop the `pe_` prefix on `Types.param_env` fields: they are now
  `codec_id`, `slots`, `bound`. Callers using `Param.bind` / `Param.get`
  are unaffected; code that pattern-matches the record or reads fields
  directly needs to update (#63, @samoht)
- `Codec.raw_encode` and the `Codec` typ constructor's `codec_encode` now
  return the offset after the written bytes (`int`) instead of `unit`.
  `Codec.encode` is unaffected. Callers that bound `raw_encode` and
  threaded its result need to discard the returned offset or use it as
  the new running position (#65, @samoht)

### Documentation

- Add odoc cross-reference links and the doc comments that were missing
  across the public interfaces, and expose a `pp` printer on `UInt32`,
  `UInt63`, `Param`, and `Wire.Diff` (#73, @samoht)
- Type-check `README.md` and every public `.mli` under `mdx`
  (#39, @samoht)

### Fixed

- `Field.repeat` over a `zeroterm` element (a list of NUL-terminated strings
  within a byte budget) now encodes, decodes, and generates a verified
  EverParse validator. It previously raised `Failure` when decoding (#93, @samoht)
- A `Field.optional_or` with a dynamic gate now generates an EverParse C
  validator that accepts the bytes `Codec.encode` produces. The two previously
  disagreed about the field's layout when the gate was false, so a value
  written by the OCaml encoder was rejected by the generated validator
  (#88, @samoht)
- `Field.repeat` over a fixed `byte_array` / `byte_slice` element (a list of
  n-byte chunks) now encodes, decodes, and generates a verified EverParse
  validator. It previously raised `Failure` when decoding (#89, @samoht)
- `Wire.array` whose element is a fixed `byte_array` / `byte_slice` (e.g. an
  array of fixed-size addresses) now generates a valid EverParse validator;
  the generated 3D schema was previously malformed for such arrays (#92, @samoht)
- An embedded variable-size sub-codec (`Wire.codec`, e.g. a length-prefixed
  string) used as a field no longer makes EverParse reject the schema with
  `Parse_with_dep_action: tag not readable`; the field is handed to its
  `WireSet*` callback by offset, like a byte slice or casetype (#87, @samoht)
- A cross-field size/offset/`present` expression that reads an integer field
  whose value exceeds the native int range (a `uint64`/`int64` length beyond
  `max_int`), or that references a non-integer field, now raises
  `Parse_error` instead of silently evaluating to 0. An out-of-range length
  was being read as an empty field, masking malformed input (#82, @samoht)
- `Codec.size_of_value` now counts a `Field.repeat`'s elements instead of
  reporting zero for a dynamic byte budget. Like the casetype case in #78, a
  buffer sized from `Codec.size_of_value` for a codec with a repeat field
  under-allocated, so `Codec.encode` ran off the end with
  `Invalid_argument "index out of bounds"` (#79, @samoht)
- `Codec.size_of_value` now counts a `Wire.casetype` field's tag and
  matched-case body instead of reporting zero for it. Sizing a buffer with
  `Codec.size_of_value` for a codec containing a casetype field used to
  under-allocate, so `Codec.encode` then ran off the end with
  `Invalid_argument "index out of bounds"` (#78, @samoht)
- `Field.repeat` over a `Wire.casetype` element now encodes and decodes
  instead of raising `Failure "unsupported element type in repeat"`. The
  repeat element path had no case for a tag-dispatched union, which ruled
  out a DHCP-style options TLV whose cases mix bare single-byte tags (PAD,
  END) with length-prefixed bodies (#75, @samoht)

- `Codec.size_of_value` no longer over-counts a packed bitfield reached
  through a value wrapper such as `Wire.bit` or an enum/map over `bits`.
  The per-field size was keyed off the field's logical type, so a wrapped
  bit sharing a base byte (e.g. a 7-bit reserved field followed by a 1-bit
  `bool` flag packed into one byte) reported an extra byte; `Codec.encode`
  then raised a spurious `Invalid_argument` even though the writer emitted
  the correct bytes (#72, @samoht)
- `Codec.encode` now raises `Invalid_argument` when the writer emits fewer
  bytes than `Codec.size_of_value v` promised, so a buggy writer that
  leaves trailing bytes uninitialised fails loudly at the encode site
  instead of shipping silently-corrupted bytes that a decoder reads as
  part of the value (#62, @samoht)
- `Codec.encode` checks the buffer against `Codec.size_of_value v` instead
  of the static `min_size` lower bound, so a variable-size codec encoded
  into a too-small buffer fails loudly with a precise byte count instead
  of writing past the end. Also fixes a latent overcount in
  `Codec.size_of_value` where each bitfield in a packed group reported the
  full base size, returning `base_count * base_size` for codecs that
  actually fit in one base word (#61, @samoht)
- `Field.optional` with a dynamic gate no longer ships a silent phantom
  byte or overruns the buffer on inconsistent input; the encoder raises
  `Invalid_argument` when the gate and the option value disagree
  (#58, @samoht)
- Dynamic `Field.optional_or` now sizes and writes from the value instead
  of using the dynamic gate as an encode-side size oracle; the gate
  remains the decode-side selector between the decoded inner and the
  default (#58, @samoht)
- Decoding an `all_zeros` field that contains a non-zero byte returns a
  `Constraint_failed` decode error instead of raising `Invalid_argument`.
  The old behaviour broke the decoder's "never raise on adversarial
  input" contract (@samoht)
- `Wire.encode_into` on a `Single_elem` (`Wire.nested ~size:n inner`)
  now pads zero bytes up to the declared `size` when `inner` writes
  fewer than `n` bytes. `encode_direct` already padded; `encode_into`
  silently dropped the padding, so `Wire.to_string` produced shorter
  bytes than `Wire.of_string` expected (@samoht)
- `Wire.default` now takes a required `~tag:'k`: the discriminator
  the encoder writes when projecting the default branch back to bytes.
  Previously encoding any default-branch value crashed with
  `Failure "casetype encoding: cannot encode default case"`; the
  decoder side still uses the default branch as the match-anything
  fallback (@samoht)
- `Wire.encode_into` on a `codec` field whose inner ends in `all_bytes`
  / `rest_bytes` / `all_zeros` no longer ships a 4096-byte scratch
  tail. The encoded size is now computed from the value via
  `Codec.size_of_value`; the buffer-driven `size_of` mismeasured
  variable tails as "remaining buffer space" (#54, @samoht)
- `Codec.encode` / `Codec.raw_encode` accept `?env:Param.env`, mirroring
  `Codec.decode`. Encoding a parametric codec without an env (or with an
  env that left an input param unbound) now raises `Invalid_argument`
  naming the offending param instead of silently writing zero-sized
  regions. A parametric byte field whose value length disagrees with its
  env-bound size also raises rather than truncating (#53, @samoht)
- Fix silent always-true compilation of `Field.optional` /
  `Field.optional_or` predicates that use bitwise / shift / mod
  operators, and fix `Field.ref` on an `optional` field reading 0
  instead of the decoded inner value (#48, @samoht)
- Allow variable-size sub-codecs and `Field.repeat` after a
  variable-size field (#38, @samoht)
- Fix C stub generator for schema names with 2+ leading capitals
  (e.g. `IPv4`, `EP_Header`) (#36, @samoht)

## 0.9.0

Initial release.
