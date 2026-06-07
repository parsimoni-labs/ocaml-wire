## unreleased

### Added

- `Wire.Codec.rename` returns a codec with a new name, leaving its wire
  encoding and field constraints unchanged. The name only determines the
  generated 3D struct name, so it lets a generically built codec be given a
  unique, meaningful name before projection or code generation (@samoht)
- `Wire.nested` / `Wire.nested_at_most` now accept a composite inner (a
  `Wire.array`, or another nested region), and a `Wire.casetype` field's case
  body may be such a region. Both round-trip and generate a verified EverParse
  validator; previously they crashed at decode or failed schema generation
  (#109, @samoht)
- An embedded sub-codec (`Wire.codec c` used as a field or `Field.repeat` /
  `array` element) that takes `Param.input` parameters now works: the outer
  codec surfaces the sub-codec's input params as its own, so `Codec.env` /
  `Param.bind` reach them and the values are threaded into the sub-codec on
  encode, decode, and 3D projection. Previously the sub-codec's params were
  invisible to the outer codec and binding them raised `Invalid_argument`
  (#108, @samoht)
- A `Wire.casetype` used as a `Field.repeat` element may now have a case body
  that is a bitfield, alongside the scalar, byte-span, NUL-terminated, and
  sub-codec bodies already allowed. Such a casetype previously failed at
  decode (#105, @samoht)
- `Field.optional` and `Field.optional_or` now accept a variable-size inner,
  so an optional field can be a length-prefixed string or a whole sub-message,
  not just a fixed-width value, and generate a verified EverParse validator.
  Building such a codec previously raised `Invalid_argument`; afterwards a
  statically-present optional over a byte-span, refined (`byte_array_where`), or
  composite inner still projected to C that EverParse rejected, because the
  optional was not treated as transparent in the projection (#88, #133, @samoht)
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

- `Field.repeat` and `Wire.array` over a `Wire.casetype` now raise
  `Invalid_argument` at construction when a case body has no per-element
  projection (a nested region, array, or optional), instead of building a
  codec that fails later at decode (#105, @samoht)
- `Wire.default` (a casetype's default branch) no longer takes a fixed `~tag`;
  instead it threads the matched discriminator through `inject` and `project`,
  so an arbitrary unclaimed tag round-trips. `inject` is now `'k -> 'w -> 'a`
  (it receives the matched tag along with the body) and `project` is now
  `'a -> ('k * 'w) option` (it returns the tag to write back). The decoder used
  to hand `inject` only the body and the encoder always wrote the one fixed
  `~tag`, so a default branch could not recover or re-emit the actual tag it
  caught (e.g. a DHCP or TCP option code). Migrate by taking the tag in
  `inject` (`fun _tag body -> ...` to ignore it) and pairing it in `project`
  (#100, @samoht)
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

- An `Action.on_act` whose body ends in `Action.return_bool` now generates a
  verified EverParse validator. It projected to an `:act` block (which is unit
  in 3D) ending in a Bool `return`, with the auto field setter emitted after the
  return, so EverParse rejected it; the trailing return is now dropped (a no-op
  success, matching how OCaml evaluates it) and the setter runs last (@samoht)
- A statically-absent `Field.optional` / `Field.optional_or` (`~present:false`)
  now generates a verified EverParse validator. It projected to a zero-length
  byte-size list EverParse refused to name ("Expected a named type, got
  Parse_nlist"), leaving the codec with no validator; it now projects as a
  zero-byte `unit` field, the same form `Wire.empty` uses (@samoht)
- The generated dune rule now compiles the EverParse C under strict C11
  (`-std=c11 -D_DEFAULT_SOURCE`, without `-Wextra`) instead of `-std=c99`, so
  the verified validators build on Linux glibc: the BSD endian helpers
  (`be16toh`, ...) the C uses need `_DEFAULT_SOURCE` declared, and `-Wextra`'s
  `-Wtype-limits` rejected the always-true bound check EverParse emits for an
  optional's absent zero-byte case (@samoht)
- A `Wire.enum` field now projects its membership to 3D, so the
  EverParse-generated C validator rejects values outside the named cases,
  exactly as `Codec.decode` does (raising `Invalid_enum`). The field previously
  projected to its bare base integer with no refinement, so the verified C
  accepted out-of-range values the OCaml decoder rejects, including for an enum
  nested inside a sub-codec or record (#131, @samoht)
- A `Wire.byte_array_where` inside a `Wire.nested` / `nested_at_most` region now
  generates a verified EverParse validator. Its synthesised refined-byte typedef
  was emitted only for a top-level field, so the nested region referenced an
  undeclared type and EverParse rejected the schema. The typedef is now emitted
  (before the wrapper that references it) wherever the refinement is nested
  (#132, @samoht)
- A `Wire.lookup` field now projects its index bound to 3D, so the
  EverParse-generated C validator rejects out-of-range indices exactly as the
  OCaml decoder does. Previously the projection emitted the underlying integer
  with no bound, so the verified C accepted indices the OCaml decoder rejects
  (@samoht)
- A `Wire.lookup` used as a `Wire.array` or `Field.repeat` element now projects
  its index bound to every element, not just to a scalar field. Previously the
  verified C validator accepted out-of-range indices in array or repeat elements
  that the OCaml decoder rejects (#126, @samoht)
- A codec with a `Wire.uint63` or `Wire.uint63be` field now generates a
  verified EverParse validator. It projected to an invalid `UINT63` type that
  EverParse has no notion of, so schema generation failed and the codec silently
  had no verified C parser at all. It now projects to the 8-byte `UINT64`, which
  both decoders accept identically (#125, @samoht)
- A codec mixing signed-integer or float fields of different widths (e.g. a
  `float32` then a `float64`, or an `int8` then an `int32`) now generates a
  verified EverParse validator. Such fields shared one extraction callback whose
  value type was fixed to the first field's width, so EverParse rejected the
  schema on the width mismatch and the codec had no verified C parser at all.
  Each field width now routes to its own callback (#127, @samoht)
- A `Wire.enum` used as a `Wire.array` or `Field.repeat` element (or inside an
  optional or sized region) now generates a verified EverParse validator. Its
  enumeration declaration was only emitted for scalar fields, so the schema
  referenced an undeclared type and EverParse rejected it, leaving the codec
  with no verified C parser at all (#128, @samoht)
- Decoding a `Wire.enum` through the `Codec` API now rejects a value that is not
  one of the named cases (raising `Invalid_enum`), on a scalar field and on
  every array or repeat element, matching the EverParse-generated validator and
  the `Wire.of_string` path. The `Codec` decoder previously stripped the enum to
  its base integer and accepted any value (#130, @samoht)
- Decoding no longer crashes with `Invalid_argument` on adversarial input where
  a `Field.repeat` byte budget, or a variable field's cross-field size, exceeds
  the buffer (an out-of-range `Bytes.sub`). Such an oversized length now fails
  with a clean `Parse_error` (#117, @samoht)
- `Wire.rest_bytes` now generates a verified EverParse validator. Its
  `total - sizeof(this)` byte-size failed EverParse verification ("cannot verify
  u32 subtraction"), so any codec with a `rest_bytes` field failed schema
  generation regardless of width. The projection now emits a `total >=
  sizeof(this)` guard on the preceding field, which discharges the subtraction
  (#117, @samoht)
- `Wire.array` / `Wire.array_seq` / `Field.repeat` / `Field.repeat_seq` over a
  sub-codec built only from byte-span fields (`byte_array`, `byte_slice`, a
  varint) now raise `Invalid_argument` at construction. EverParse projects such
  an element as a byte-budget list whose entries may be empty, which its
  validator rejects, so the codec previously built but failed schema
  generation. A sub-codec with at least one fixed-size field is accepted as
  before (#115, @samoht)
- `Wire.array` / `Wire.array_seq` over a float, a signed integer, a `uint63`, or
  a `Wire.where` / `Wire.map` wrapping a fixed byte span no longer crashes 3D
  projection with an assertion failure. Such an array built and round-tripped
  but the projector could not size its element, so generating a schema raised.
  All fixed-width scalars and wrapped byte spans now project (#116, @samoht)
- A greedy field (`all_bytes` / `all_zeros`) reads the rest of the buffer, so it
  is now rejected with `Invalid_argument` anywhere it is not the final field: a
  non-last field of a codec, a `Field.repeat` / `Wire.array` element (or a
  sub-codec ending in one), or a `Wire.casetype` case body. Such a codec
  previously built but failed to decode. It remains valid as the last field,
  the supported way to consume the rest (#107, #110, #111, @samoht)
- An embedded sub-codec's `where` clause and field constraints are now enforced
  when the codec is decoded as a field or element. They were silently dropped
  on the embedded path (only the buffer-size and field readers ran), so a
  value the sub-codec would reject standalone was accepted when embedded
  (#108, @samoht)
- `Wire.array` / `Wire.array_seq` now reject a non-fixed-width element (a
  `Wire.nested` region, a `Wire.byte_array_where` refined span, or a nested
  `Wire.array`) at construction with `Invalid_argument`, instead of building a
  codec that failed schema generation or raised `Failure` at decode. An array
  projects as a count of fixed-size elements, so the element must be a scalar,
  a fixed byte span, or a fixed-size sub-codec, matching `Field.repeat`'s
  element rule (#107, @samoht)
- A `Wire.casetype` whose case body is a NUL-terminated string (`zeroterm` or
  `zeroterm_at_most`) now encodes, decodes, and sizes correctly as a
  `Field.repeat` element. The element encoder had no `zeroterm` case (so encode
  raised `Failure "unsupported type"`), the element decoder no
  `zeroterm_at_most` case, and the element sizer reported "cannot determine
  element size", so a list of such tag-dispatched options could not round-trip
  (#103, @samoht)
- A `byte_array` / `byte_slice` (or any field) whose `~size` reads a
  `Field.optional_or` field no longer resolves that size to 0. The optional_or
  field exposed a const-0 reader to cross-field size/offset expressions, so the
  span decoded as empty (silent truncation) and `Codec.encode` raised a length
  mismatch. It now reads the present-or-default value (#101, @samoht)
- A `Wire.nested` / `Wire.nested_at_most` field now generates a verified
  EverParse validator. Previously any codec with a `nested` field failed schema
  generation, regardless of the inner type (#99, @samoht)
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
- `Wire.array` over a fixed-size sub-record (a `Wire.codec` element, e.g. an
  array of `{ x; y }` points) now decodes instead of raising `Failure`. The
  schema already projected and verified as a sub-struct under a `[:byte-size]`
  budget; only the OCaml decoder was missing the case (#96, @samoht)
- `Field.repeat` over a `zeroterm` element (a list of NUL-terminated strings
  within a byte budget) now encodes, decodes, and generates a verified
  EverParse validator. It previously raised `Failure` when decoding (#93, @samoht)
- A `Field.optional_or` with a dynamic gate now generates an EverParse C
  validator that accepts the bytes `Codec.encode` produces. The two previously
  disagreed about the field's layout when the gate was false, so a value
  written by the OCaml encoder was rejected by the generated validator
  (#88, @samoht)
- `Field.repeat` and `Wire.array` over a fixed `byte_array` / `byte_slice`
  element (a list of n-byte chunks, e.g. fixed-size addresses) now encode,
  decode, and generate a verified EverParse validator. Decoding previously
  raised `Failure` and the generated schema was malformed (#89, #92, @samoht)
- A bitfield (`Wire.bits` / `Wire.bit`) is now rejected with `Invalid_argument`
  at construction as an element of `Field.repeat` / `Wire.array` / `Wire.nested`
  or as an `Field.optional` inner. A bitfield only exists packed inside a
  record, with no standalone wire form, so it cannot be an element or a
  conditionally-present field; previously such a codec built and crashed at
  decode (#90, #98, #107, @samoht)
- A codec that embeds a variable-size sub-codec (`Wire.codec`, e.g. a
  length-prefixed string) as a field is now accepted by EverParse; it
  previously failed schema generation (#87, @samoht)
- A cross-field length / offset / `present` expression that reads an integer
  beyond the native int range (a `uint64`/`int64` length over `max_int`), or
  reads a non-integer field, now fails the parse instead of silently reading
  0. The old behaviour masked malformed input (#82, @samoht)
- `Codec.size_of_value` now sizes a `Field.repeat` with a dynamic budget, a
  `Wire.casetype` field, and a packed bitfield (wrapped by `Wire.bit` or an
  enum / map) correctly. The first two were under-counted (so `Codec.encode`
  overran the buffer) and the bitfield over-counted (so `encode` raised a
  spurious `Invalid_argument`) (#72, #78, #79, @samoht)
- `Field.repeat` over a `Wire.casetype` element now encodes and decodes
  instead of raising. This covers DHCP-style options whose cases mix bare
  single-byte tags with length-prefixed bodies (#75, @samoht)
- `Codec.encode` now raises `Invalid_argument` when the writer emits fewer
  bytes than `size_of_value` promised, instead of shipping a value with
  uninitialised trailing bytes (#62, @samoht)
- `Codec.encode` into a too-small buffer now fails with a precise byte count
  instead of writing past the end (#61, @samoht)
- `Field.optional` / `Field.optional_or` with a dynamic gate now encode from
  the value (the gate selects the decoded value or the default on decode);
  `optional` raises `Invalid_argument` rather than writing a phantom byte or
  overrunning the buffer when the gate and value disagree (#58, @samoht)
- Decoding an `all_zeros` field that contains a non-zero byte now returns a
  `Constraint_failed` error instead of raising (@samoht)
- `Wire.to_string` on a `Wire.nested ~size:n` field now zero-pads to `n`
  bytes when the inner writes fewer, so it agrees with `Wire.of_string`
  (@samoht)
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
