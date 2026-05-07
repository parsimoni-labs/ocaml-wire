## unreleased

### Added

- `Codec.validator_of_struct` / `validate_struct` / `struct_size_of` /
  `struct_min_size`: build and run a validator from any `Types.struct_`,
  no record constructor needed (#37, @samoht)
- `Codec.slice_offset` / `Codec.slice_length`: zero-allocation access to
  the offset/length of a `byte_slice` field, replacing the
  `Slice.first (Codec.get ...)` pattern (#37, @samoht)
- `Wire.codec` type alias for `'r Codec.t`, and `Wire.pp_value` to
  print a record field-by-field through its codec (@samoht)
- `Wire.byte_array_where ~size ~per_byte`: byte span with a per-byte
  refinement. Decode raises `Parse_error` on the first byte that
  violates the constraint; encode raises `Invalid_argument`. The 3D
  projection synthesises a 1-byte refinement struct per use and
  references it from the parent field, so both wire and EverParse C
  enforce the same per-element constraint. Motivating shape:
  printable-ASCII bodies (SSH name-list, RFC 4251 sec 5) (@samoht)

### Changed

- `Codec.bitfield`: ~5% faster on bitfield-heavy workloads (CLCW polling
  +5%) (#37, @samoht)
- Rename `Wire.decode_*` / `Wire.encode_*` to `of_string` / `of_bytes` /
  `of_reader` / `to_string` / `to_bytes` / `to_writer`. Add `_exn`
  twins that raise `Parse_error` instead of returning a result
  (@samoht)
- Fold `Codec.decode_with` into `Codec.decode` via an optional `?env`.
  Split decode into `Codec.decode` (result) and `Codec.decode_exn`
  (raises) (@samoht)

### Documentation

- Type-check `README.md` and every public `.mli` example under `mdx`
  (`(using mdx 0.4)`). Stale snippets now break `dune runtest`. (@samoht)

### Fixed

- Allow `Wire.codec` sub-codecs and `Wire.repeat` after a variable-size
  field. Previously raised `add_field: variable-size codec after
  variable-size field not supported` (#38, @samoht)
- Fix C stub generator: use the EverParse-normalised `<Name>Fields`
  type name. Schemas with a name segment of 2+ leading capitals (e.g.
  `IPv4`, `EP_Header`) previously emitted C that referenced an
  undefined struct (#36, @samoht)

## 0.9.0

Initial release.
