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
- Rename direct-IO entry points to the standard `of_string` /
  `to_string` / `of_bytes` / `to_bytes` / `of_reader` / `to_writer`
  vocabulary, matching the encoding-library convention. Each
  decoder ships an `_exn` twin that raises `Parse_error` instead
  of returning a result, for hot paths where `Ok _` allocation
  matters (@samoht)
- Merge `Codec.decode_with` into `Codec.decode` via an optional
  `?env`, and split into `Codec.decode` (returns result) and
  `Codec.decode_exn` (raises). Internal callers use the [_exn]
  variant (@samoht)

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
