## unreleased

### Added

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

- Remove `Wire.optional` / `Wire.optional_or` / `Wire.repeat` /
  `Wire.repeat_seq` from the typ-level surface; use the matching
  `Field.*` combinators instead (#46, @samoht)
- Rename `Wire.decode_*` / `Wire.encode_*` to `of_string` / `of_bytes` /
  `of_reader` / `to_string` / `to_bytes` / `to_writer`; add `_exn` twins
  that raise on parse error (#39, @samoht)
- Fold `Codec.decode_with` into `Codec.decode` via `?env`; split into
  `Codec.decode` (result) and `Codec.decode_exn` (raises) (#39, @samoht)
- Speed up `Codec.bitfield` ~5% (#37, @samoht)

### Documentation

- Type-check `README.md` and every public `.mli` under `mdx`
  (#39, @samoht)

### Fixed

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
