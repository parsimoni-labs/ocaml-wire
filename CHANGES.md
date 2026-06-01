## unreleased

### Added

- `Field.optional` and `Field.optional_or` now accept a variable-size inner,
  so an optional field can be a length-prefixed string or a whole sub-message,
  not just a fixed-width value. Building such a codec previously raised
  `Invalid_argument` (#88, @samoht)
- `Wire.zeroterm` and `Wire.zeroterm_at_most ~size` for NUL-terminated
  strings: the bytes up to a terminator, optionally bounded to a
  fixed-size region (#77, @samoht)
- `Wire.casetype` now accepts any tag type (`'k typ`, not just `int`), so a
  string-discriminated union (a `byte_array`-tagged casetype, as in many
  SSH messages) is expressible (#49, @samoht)
- A `Wire.casetype` case body can be an embedded sub-codec, including one
  that ends in `all_bytes` (#50, @samoht)
- `Field.repeat` now supports variable-size elements, e.g. a list of
  length-prefixed sub-messages (#51, @samoht)
- `Wire.Codec.size_of_value`: the encoded byte length of a value (#58, @samoht)
- `Wire.casetype` and `Wire.nested ~size` can be used as `Codec` fields
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

- Decoding a struct with variable-size fields (`byte_slice`, `byte_array`,
  or a `repeat` sized by a cross-field expression) no longer allocates on
  each field access. Pure speedup, no API change (#81, @samoht)
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
- `Codec.raw_encode` now returns the offset after the written bytes instead
  of `unit`. `Codec.encode` is unaffected (#65, @samoht)

### Documentation

- Add odoc cross-reference links and the doc comments that were missing
  across the public interfaces, and expose a `pp` printer on `UInt32`,
  `UInt63`, `Param`, and `Wire.Diff` (#73, @samoht)
- Type-check `README.md` and every public `.mli` under `mdx`
  (#39, @samoht)

### Fixed

- `Codec.encode` no longer requires an `?env` for a codec whose only
  parameters are decode-side outputs (a field with an `Action.assign` into a
  `Param.output`). Output params are never read when encoding, so demanding an
  env raised `Invalid_argument` spuriously, and an output-param sub-codec
  embedded as a field could not be encoded at all (#95, @samoht)
- `Field.repeat` / `Field.repeat_seq` now reject an element type that has no
  clean per-element 3D projection (a sub-byte `bits` field, a refined or
  at-most byte span, `all_zeros`, or a nested `array` / `nested`) at
  construction with a clear `Invalid_argument`, instead of building a codec
  that raised `Failure` deep in decode or emitted a schema EverParse could not
  verify. Supported elements are unchanged: fixed-width scalars and byte spans,
  `zeroterm`, sub-codecs, and casetypes (#97, @samoht)
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
- `Field.repeat` / `Wire.array` over a bitfield element (`Wire.bits` or
  `Wire.bit`) now raises `Invalid_argument` when the codec is built, instead of
  crashing at decode. A bitfield only exists packed inside a record, so it
  cannot be a repeat or array element (#90, @samoht)
- A codec that embeds a variable-size sub-codec (`Wire.codec`, e.g. a
  length-prefixed string) as a field is now accepted by EverParse; it
  previously failed schema generation (#87, @samoht)
- A cross-field length / offset / `present` expression that reads an integer
  beyond the native int range (a `uint64`/`int64` length over `max_int`), or
  reads a non-integer field, now fails the parse instead of silently reading
  0. The old behaviour masked malformed input (#82, @samoht)
- `Codec.size_of_value` under-counted a `Field.repeat` with a dynamic byte
  budget, so `Codec.encode` overran the buffer. It now counts the elements
  (#79, @samoht)
- `Codec.size_of_value` under-counted a `Wire.casetype` field, so
  `Codec.encode` overran the buffer. It now counts the tag and matched case
  (#78, @samoht)
- `Field.repeat` over a `Wire.casetype` element now encodes and decodes
  instead of raising. This covers DHCP-style options whose cases mix bare
  single-byte tags with length-prefixed bodies (#75, @samoht)
- `Codec.size_of_value` no longer over-counts a packed bitfield wrapped by
  `Wire.bit` or an enum/map, which made `Codec.encode` raise a spurious
  `Invalid_argument` (#72, @samoht)
- `Codec.encode` now raises `Invalid_argument` when the writer emits fewer
  bytes than `size_of_value` promised, instead of shipping a value with
  uninitialised trailing bytes (#62, @samoht)
- `Codec.encode` into a too-small buffer now fails with a precise byte count
  instead of writing past the end (#61, @samoht)
- `Field.optional` with a dynamic gate no longer writes a phantom byte or
  overruns the buffer when the gate and the value disagree; it raises
  `Invalid_argument` (#58, @samoht)
- `Field.optional_or` with a dynamic gate now encodes from the value; the
  gate selects the decoded value or the default on decode (#58, @samoht)
- Decoding an `all_zeros` field that contains a non-zero byte now returns a
  `Constraint_failed` error instead of raising (@samoht)
- `Wire.to_string` on a `Wire.nested ~size:n` field now zero-pads to `n`
  bytes when the inner writes fewer, so it agrees with `Wire.of_string`
  (@samoht)
- `Wire.default` now takes a required `~tag`: the discriminator written when
  encoding the default branch. Encoding a default-branch value previously
  crashed (@samoht)
- `Wire.to_string` on a `codec` field whose inner ends in `all_bytes` /
  `rest_bytes` / `all_zeros` no longer appends a 4 KB scratch tail; the size
  is computed from the value (#54, @samoht)
- `Codec.encode` / `Codec.raw_encode` accept `?env:Param.env`, like
  `Codec.decode`. Encoding a parametric codec with a missing param binding
  now raises `Invalid_argument` naming the param instead of writing
  zero-sized regions (#53, @samoht)
- `Field.optional` / `Field.optional_or` predicates that use bitwise / shift
  / mod operators are no longer silently treated as always-true, and
  `Field.ref` on an `optional` field now reads the decoded value instead of 0
  (#48, @samoht)
- A variable-size sub-codec or `Field.repeat` may now follow a variable-size
  field (#38, @samoht)
- Fix C stub generation for schema names with two or more leading capitals
  (e.g. `IPv4`, `EP_Header`) (#36, @samoht)

## 0.9.0

Initial release.
