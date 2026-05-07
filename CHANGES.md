## unreleased

### Added

- `Codec.validator_of_struct` / `validate_struct` / `struct_size_of` /
  `struct_min_size`: build and run a validator from any `Types.struct_`,
  no record constructor needed (#37, @samoht)
- `Codec.slice_offset` / `Codec.slice_length`: zero-allocation access to
  the offset/length of a `byte_slice` field, replacing the
  `Slice.first (Codec.get ...)` pattern (#37, @samoht)

### Changed

- `Codec.bitfield`: ~5% faster on bitfield-heavy workloads (CLCW polling
  +5%) (#37, @samoht)
- Rename `Wire.encode_to_string` to `Wire.encode_string` and
  `Wire.encode_to_bytes` to `Wire.encode_bytes` for symmetry with
  `Wire.decode_string` / `Wire.decode_bytes` (@samoht)

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
