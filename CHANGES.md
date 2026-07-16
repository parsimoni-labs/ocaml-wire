## unreleased

### Added

- `Wire.Expr.land64` is an int64 bitwise AND for masking a full-width `uint64`
  field inside a constraint, and `~self_int64` / `Field.int64` now accept a
  `map`-decoded uint64 field. A sign-magnitude offset bound such as a bsdiff
  seek (`land64 self mask <= max`) is now expressible and projects to a 3D
  refinement EverParse verifies (#227, @samoht)

### Changed

- `uint32`/`uint32be` now decode to `Optint.t` and `uint63`/`uint63be` to
  `Optint.Int63.t` rather than a native `int`, so a value with bit 31 (or the
  high half) set is no longer silently truncated on a target whose `int` is
  narrower than 63 bits (js_of_ocaml, wasm_of_ocaml); a TCP sequence number now
  round-trips there. Read such a field with `Optint.to_int` / `Optint.to_int32`;
  a `uint32` used as a size parameter must become `int32be` (#226, @samoht)

- Decode errors are redesigned. `parse_error` is now a `{ at; field; kind }`
  record instead of a flat variant: `at` is the failing field's byte offset,
  `field` the root-to-leaf path of field names to it, and `kind` a closed
  `error_kind` (`Unexpected_eof`, `Invalid_enum`, `Invalid_tag`,
  `Missing_terminator`, `Non_zero_padding`, `Value_out_of_range`,
  `Constraint_failed`), so a failure deep in a nested struct is both locatable
  and matchable. `Constraint_failed of string` becomes
  `Constraint_failed { which; value }`, naming the predicate that failed and
  carrying the offending field's value. `Validation_error` is removed
  (`Codec.validate` and the `_exn` decoders raise the single `Parse_error`), and
  the type gains `equal_parse_error` / `compare_parse_error`, `pp_error_kind`,
  and public `parse_error` / `eof` constructors. Match on `e.kind`, read `e.at`,
  replace `Validation_error` with `Parse_error`, and wrap `Wire.parse_error` in
  your own variant for domain errors (#219, @samoht)

- Decoding a record codec with 17 to 32 fields no longer allocates a
  short-lived closure on each decode: the closure-free decode ceiling that
  1.0.0 raised to 16 fields now extends to 32. Real protocol headers cross the
  old limit routinely, such as an 18-field TCP header with its flag bits broken
  out, or telemetry and IPv4 headers in the high twenties, which now allocate
  only the record they return. A codec wider than 32 fields keeps the recursive
  fallback (#214, @samoht)

### Fixed

- Codec values are now safe to share across domains: their validation scratch
  and parameter backing are per-domain, so encoding, decoding, or validating one
  codec concurrently no longer corrupts the result (#223, #224, @samoht)

- A generated FFI parser (`parse buf off`) now raises `Invalid_argument` when
  `off` is negative or past the end of `buf`, instead of reading out of bounds.
  The C stub computed its length as `Bytes.length buf - off` in unsigned
  arithmetic, so an out-of-range `off` (which can carry a length or offset field
  parsed from untrusted input) underflowed the length into a huge span and
  pushed the read pointer past the buffer before the validator ran. An in-range
  `off` is unaffected (#222, @samoht)

- A non-zero byte in an `all_zeros` padding field now reports the same error
  whether the field is decoded through a struct (`Codec.decode` /
  `Codec.validate`) or directly (`Wire.of_string`). The struct path used to
  report a stringly `Constraint_failed` that dropped the byte offset while the
  direct path reported the typed `All_zeros_failed` with the offset; both now
  report `All_zeros_failed` carrying the offset (#220, @samoht)

- A `casetype` whose discriminant matches no case now fails with the typed
  `Invalid_tag` carrying the tag value, the same class as an out-of-range lookup
  index, instead of a stringly `Constraint_failed` (#220, @samoht)

- A non-integer field referenced where an integer is required (a schema mistake,
  such as a float used as a length) now raises `Invalid_argument` rather than a
  parse error, keeping schema errors distinct from malformed input (#220,
  @samoht)

## 1.0.0

### Added

- `Field.action` returns a field's action (the `?action` passed to `Field.v`),
  completing the field accessor set alongside `Field.constraint_` and
  `Field.doc`. `Action.pp` and `Param.pp` pretty-print an action block and a
  parameter, matching the existing `Field.pp` (#189, @samoht)

- `Wire.enum_open name cases base` is an open enumeration: it names the known
  values for documentation but accepts any value. Unlike `Wire.enum` /
  `Wire.variants`, it does not reject an unlisted value (no decode
  `Invalid_enum`, and the field projects as its base scalar with no membership
  refinement), which is what an open value set (a protocol field that may carry
  unknown or future codes) needs. The known codes are still emitted as a 3D
  enum declaration, so they stay documented in the generated `.3d` (#166,
  @samoht)

- The doc pipeline's differential `agree.c` is now derived from the codecs
  alone: it computes each validator's name and parameter types from the Wire
  definitions instead of reading the EverParse-generated `<Name>Wrapper.h`. The
  self-check therefore regenerates as pure OCaml, with no EverParse needed and
  no chance of drifting from the codec, and its build rule is split out from the
  committed C. `Wire.Everparse.Raw.input_param_c_types` exposes the per-parameter
  C types this relies on (#167, @samoht)

- `Wire.Expr.if_then_else cond t e` builds a conditional value expression (the
  3D `? :` ternary), so a size or constraint can depend on another field, e.g. a
  16-bit length where 0 means 65536: `if_then_else Expr.(len = int 0) (int
  65536) len`. The underlying constructor was previously reachable only through
  the wrapped internal module (#164, @samoht)

- The doc pipeline's differential harness now covers parameterized codecs: the
  corpus oracle binds each codec's `Param.input` values and the generated
  `agree.c` passes the same values to the EverParse validator, so a
  length-bound or otherwise parameter-dependent frame (e.g. a CCSDS TC / AOS /
  TM / USLP layout) is checked end to end rather than skipped. `Wire.Param`
  gains `bind_by_name`, which binds an input parameter by name without its typed
  handle (#163, @samoht)
- `Wire.Field.v` takes an optional `?self_int64`, and `Wire.Field.int64` /
  `Wire.Expr.int64` build full-width 64-bit field constraints. This lets
  schemas constrain domains such as signed-magnitude `uint64` values without
  truncating the field through OCaml's native `int` (@samoht)
- `Wire.Field.v` takes an optional `?doc` (read back with `Wire.Field.doc`):
  a free-text note, such as an RFC section, that the documentation projection
  renders as a `/* ... */` comment above the field in the generated 3D. A
  protocol spec can now cite the standard each individual field comes from, not
  just the struct as a whole, and EverParse accepts the comment (#157, @samoht)
- The standalone projection pipeline (`Wire_3d.main ~mode:`Standalone`) now
  auto-generates a differential self-check: `dune runtest` fuzzes inputs,
  records whether the
  OCaml codec accepts each, and replays them through the EverParse-generated C
  validator, failing on any input the two decide differently. This catches a
  doc projection that drifts from the codec (a wrong bit order, a constraint
  that means something else over the wire type), which nothing checked before
  since the doc validator carries no FFI. The build also produces an installed
  `lib<name>.a` archive of the validator. New `Wire_3d.generate_corpus` and
  `Wire_3d.generate_agree` expose the two halves (@samoht)
- `Wire.Codec.v` takes an optional `?doc` (read back with `Wire.Codec.doc`):
  a free-text note, such as an RFC citation, that the documentation projection
  renders as a `/*++ ... --*/` comment on the codec's 3D typedef. The generated
  spec then documents which standard each protocol struct comes from, and
  EverParse accepts the comment (#155, @samoht)
- `Wire_3d`'s documentation helpers (`generate_doc`, `generate_dune_doc`, and
  `main ~mode:`Standalone`) take an optional `?name` that sets the generated
  `<Name>.3d` / `<Name>.c` file base independently of the opam `~package`, so a
  package like `ocaml-tcp` can emit a `Tcp.3d` spec while still installing under
  its own name (#154, @samoht)
- `Wire.Everparse.project ~mode:`Standalone` and `Wire.Everparse.write` project
  a codec, or a whole family of codecs, to a clean `.3d` with no FFI
  scaffolding: enums render as named 3D enum types, types shared across codecs
  are emitted once, and a protocol family lands in one readable `<Name>.3d`. The
  result doubles as a protocol specification and as input to EverParse, which
  compiles it to a standalone verified C parser with no FFI (#151, @samoht)
- `Wire.Codec.rename` returns a codec with a new name, leaving its wire
  encoding and field constraints unchanged, so a generically built codec can
  be given a unique, meaningful name before code generation (@samoht)
- `Wire.nested` / `Wire.nested_at_most` now accept a composite inner (a
  `Wire.array`, or another nested region), and a `Wire.casetype` field's case
  body may be such a region; both round-trip and generate a verified EverParse
  validator (#109, @samoht)
- An embedded sub-codec (`Wire.codec c` used as a field or `Field.repeat` /
  `array` element) that takes `Param.input` parameters now works: the outer
  codec exposes the sub-codec's input params as its own, so `Codec.env` /
  `Param.bind` reach them and the values are threaded into the sub-codec on
  encode, decode, and projection (#108, @samoht)
- A `Wire.casetype` used as a `Field.repeat` element may now have a bitfield
  case body, alongside the scalar, byte-span, NUL-terminated, and sub-codec
  bodies already allowed (#105, @samoht)
- `Field.optional` and `Field.optional_or` now accept a variable-size inner,
  so an optional field can be a length-prefixed string or a whole sub-message,
  not just a fixed-width value, and generate a verified EverParse validator
  (#88, #133, @samoht)
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

- Encoding a variable-length byte field (`byte_slice`, `byte_array`,
  `zeroterm`, `all_bytes`, ...) no longer allocates: on a flambda-off switch
  the writer rebuilt two short-lived closures per field on every encode.
  Encoding a record made of scalars and var-bytes fields is now
  allocation-free (#213, @samoht)

- Decoding a record codec with more than 8 fields no longer allocates a
  short-lived closure on each decode: the constructor is now applied in one
  saturated call for codecs of up to 16 fields. On a flambda-off switch this
  partial application was the dominant cost of decoding a wide header (CLCW at
  13 fields, CFDP at 14), which now allocates only the record it returns (#212,
  @samoht)

- The EverParse projection API is consolidated to two entry points. The two
  projections, previously `Wire.Everparse.schema` and `Wire.Everparse.doc`, are
  now `Wire.Everparse.project ?mode`, where `` `Standalone `` (the default)
  emits a clean `.3d` that EverParse compiles to a standalone verified C parser
  (the production output, which also reads as a spec) and `` `Ffi `` emits the
  OCaml-callable bridge with `WireCtx` extern callbacks. Writing is likewise one
  `Wire.Everparse.write ?mode` (replacing `write_3d` and `write_doc`). The
  struct-level entry points `struct_of_codec` and `schema_of_struct` move under
  `Wire.Everparse.Raw` (as `struct_of_codec` and `project_struct`), and
  `Wire_3d.main`'s mode is now `` `Standalone `` rather than `` `Doc `` (#210,
  @samoht)

- A long `?doc` note on a field or codec (an RFC citation, say) now wraps across
  several comment lines in the generated `.3d` instead of rendering as one line
  past 80 columns, so the generated spec stays readable (#191, @samoht)

- A package generated by the `Wire_3d` pipeline now treats its EverParse C
  (`<Name>.c`, the wrappers, `EverParse.h`) as committed source. A plain `dune
  build` or `dune test` uses the committed C and never invokes `3d.exe`, and
  fails loudly if the C was never committed; set `BUILD_EVERPARSE=1` to
  regenerate it (the rule then promotes the result back into the tree).
  Previously the rule regenerated whenever the C was missing, which silently
  required EverParse on an ordinary build and let committed C drift unnoticed.
  The pure-OCaml `.3d` and `agree.c` are unaffected and still regenerate on
  demand (#168, @samoht)

- `Wire_3d.main` now takes packed codecs (`Wire_3d.pack codec`) and a
  mandatory `~mode:[`Ffi | `Standalone]`, so every `gen.ml` states what it
  emits. `` `Ffi `` keeps the per-codec FFI parsers; `` `Standalone `` emits one
  FFI-free `<Package>.3d` specification and a single standalone `<Package>.c`
  parser for the whole package, through the new `Wire_3d.generate_doc` and
  `Wire_3d.generate_dune_doc`. Migrate a `gen.ml` by replacing
  `[schema c; ...]` with `~mode:`Ffi [pack c; ...]`, or `~mode:`Standalone` for
  the single-file output (#152, @samoht)
- Reading or writing a `uint32` or `uint63` field now stays in the native
  `int` instead of round-tripping through a boxed `Int32` or `Int64`. The
  boxing surfaced as per-field allocation in tight decode and encode loops;
  field access is now allocation-free regardless of how the compiler optimises
  the surrounding code. Pure speedup, no API change (#150, @samoht)
- `Wire.Codec.decode` no longer allocates a fresh validation buffer on every
  call: each codec reuses a single buffer across decodes, so decoding the same
  codec in a loop allocates a constant amount instead of growing with the
  number of decodes. Pure speedup, no API change (#149, @samoht)
- Decoding a `Field.repeat` over a `Wire.casetype` (the DHCP / TCP
  option-list shape) no longer allocates a closure and a boxed length per
  element, so decode allocation no longer grows with the number of elements.
  Pure speedup, no API change (#148, @samoht)
- The build rules `wire.3d` generates (the `dune.inc` and the wrapper in its
  setup example) now use a `3d` alias instead of the generic `gen`, so
  `dune build @3d` regenerates the `.3d` files and EverParse C parsers.
  Update any `dune build @gen` invocation accordingly (#146, @samoht)
- `Wire.of_reader` now rewinds on failure: every byte consumed by a failed
  decode is pushed back, restoring the reader to its position before the
  call, so the caller can retry with another description or after more
  input arrives (#145, @samoht)
- `Wire.of_reader` now consumes only the bytes of the decoded value and
  leaves the rest on the reader, so several values can be decoded
  back-to-back from the same reader. Previously the first call drained the
  whole reader. Types that extend to the end of input (`all_bytes`,
  `all_zeros`) still consume the whole stream (#144, @samoht)
- The `Wire.Everparse.plug_field` record fields lose their `pf_` prefix
  (`pf_name` is now `name`, `pf_idx` is `idx`, and so on). Update custom
  plug generators accordingly (#144, @samoht)
- Codecs that share a synthesised type (an `enum`, or a refined-byte or
  element-wrapper struct) can now be linked into one binary, so full protocol
  stacks built from per-codec parsers (Ethernet, IPv4, TCP, ...) link cleanly.
  The `Wire_stubs`-generated FFI now compiles each codec's C as its own unit
  and links them; use the new `Wire_stubs.build_codec_archive` to build the
  link archive (#135, @samoht)
- `Field.repeat` and `Wire.array` over a `Wire.casetype` now raise
  `Invalid_argument` at construction when a case body has no per-element
  projection (a nested region, array, or optional), instead of building a
  codec that fails later at decode (#105, @samoht)
- `Wire.default` (a casetype's default branch) no longer takes a fixed `~tag`;
  instead it threads the matched discriminator through `inject` and `project`,
  so an arbitrary unclaimed tag round-trips. `inject` is now `'k -> 'w -> 'a`
  (it receives the matched tag along with the body) and `project` is now
  `'a -> ('k * 'w) option` (it returns the tag to write back), so a default
  branch can recover and re-emit the tag it caught (e.g. a DHCP or TCP option
  code). Migrate by taking the tag in `inject` (`fun _tag body -> ...` to
  ignore it) and pairing it in `project` (#100, @samoht)
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

- The generated C `<Name>Check<Codec>` wrapper now validates the whole buffer:
  it returns `FALSE` unless the validator consumed every input byte, so a valid
  record followed by trailing bytes is rejected instead of accepted as a valid
  prefix. The differential corpus oracle applies the same whole-buffer rule, so
  the OCaml and C verdicts still agree. The raw `<Name>Validate<Codec>` entry
  point keeps its prefix semantics and returns the consumed position. `wire`
  now depends on `re` at runtime, previously a test-only dependency (#215,
  @samoht)

- `Wire.Everparse.project` now rejects a codec that cannot project to 3D when
  the schema is built, not later when it is rendered. A constraint with no 3D
  projection (a `field_pos`, or a subtraction or multiplication over a field)
  used to build a schema that only raised once passed to `to_3d`, so the
  projection was not a reliable projectability check (#209, @samoht)

- `Codec.validate` now enforces every check `Codec.decode` does, for any codec.
  It used to skip a field's own decode-side checks (enum and variant membership,
  a lookup index bound, a refined or NUL-terminated span, an embedded codec or
  array element's constraints) whenever the struct had no top-level constraint
  or `where`, so validating untrusted input accepted values decode rejects and a
  following zero-copy `Codec.get` trusted them. A codec with nothing to check (a
  header of plain scalars and byte spans) still validates without allocating, so
  validating before a batch of `get` calls on a hot path stays cheap (#207, #211, @samoht)

- A field constraint that subtracts or multiplies a field (such as
  `Expr.(a * b = int 0)` or `Expr.(a - b >= int 5)`) is now rejected at
  projection. Such an expression under- or overflows the field's narrow width,
  which EverParse refuses to verify, leaving the codec with no validator; unlike
  addition (widened to avoid overflow), neither has a sound projection. A
  constant `Sub` / `Mul` and additive field arithmetic are unaffected
  (#206, @samoht)

- `Codec.validate` now enforces an `all_zeros` padding field, rejecting a
  non-zero byte exactly as `Codec.decode` does. The zero check lived only in the
  decode reader, so validating a frame with tampered padding succeeded and a
  following zero-copy read accepted it (#205, @samoht)

- `Codec.validate` now runs decode's structural bounds check for every codec,
  including one with no field constraints or `where`. It used to be a no-op for
  a constraint-free codec, so validating a truncated buffer succeeded and a
  following zero-copy `Codec.get` read out of bounds; the documented safety gate
  now rejects a short buffer (#204, @samoht)

- A signed field equality constraint (an `int8` field whose `~self_constraint`
  is `Expr.(s = int k)`) now projects to the two's-complement byte the unsigned
  validator reads, and folds to a constant when the target is outside the signed
  range. The raw constant was compared against the unsigned projection, so the
  generated C and `Codec.decode` disagreed on the same byte (#203, @samoht)

- A `byte_slice` whose resolved size is negative, for example a `Sub` on a
  length field that underflows on crafted input, now fails with a `Codec` parse
  error instead of escaping `Codec.decode` with a raw `Invalid_argument`. The
  slice read skipped the bounds check the other byte spans run and crashed
  (#202, @samoht)

- A `Wire.casetype` whose tag is a `uint ~size` value, or an enum over a
  big-endian base, is now rejected at construction. Neither projects to a 3D
  type the dispatch can name, so the codec built without a verified validator.
  A fixed-width integer, bitfield, or little-endian / 1-byte enum tag is
  unaffected (#201, @samoht)

- A non-trivial `Wire.where` used as a `Wire.casetype` case body is now rejected
  at construction. Such a refinement projects to `case k: T { cond } v;`, which
  is not valid 3D (a case body takes no refinement), unlike a top-level field
  `where`, so the codec had no verified validator (#200, @samoht)

- `Wire.bits ~width` is now validated against its base word: a width above the
  base size (e.g. `bits ~width:9 U8`) or below 1 is rejected at construction.
  Such a field had no faithful wire meaning, and the OCaml shift and the
  EverParse-generated validator read different values from the same bytes
  (#199, @samoht)

- `Field.repeat` over a zero-width element (`Wire.empty`) is now rejected at
  construction, like `Wire.array` already was. A byte-budget list of a 0-width
  element does not extract through EverParse, so the codec had no verified C
  validator; the error now fires when the schema is built rather than producing
  an unverifiable spec (#198, @samoht)

- A closed `enum` (or `variants`) used as an `array` element, a `repeat`
  element, or an `optional` inner now enforces its value set in the
  EverParse-generated C validator, matching `Codec.decode`. Previously only a
  byte-wide enum array element carried the membership check; a wider or
  big-endian array element, a repeat element of any width, and an optional inner
  accepted codes outside the named set that the OCaml decoder rejects with
  `Invalid_enum`, so the two disagreed on the same input (#197, @samoht)

- The documentation pipeline's differential `agree.c` now predicts each
  EverParse validator-wrapper symbol with `Wire_3d.pascal_case`, a transcription
  of EverParse's own `pascal_case` mangling, computed as
  `pascal_case (module ^ "_check_" ^ codec)`. The old code glued
  `everparse_name`-normalized parts, which kept a capital after a digit
  (`TPM2B` gave `TpmCheckTpm2B` where EverParse emits `TpmCheckTpm2b`), so
  `agree.c` referenced a symbol that did not exist and failed to compile under
  `-Werror`. Regenerate a package's `agree.c` to pick this up (@samoht)

- The `dune.inc` rules generated by the `Wire_3d` pipeline now invoke the
  generator through `%{exe:gen.exe}` and use plain dune actions instead of shell
  `(system ...)` commands. The old rules ran `./gen.exe`, which relied on dune
  placing a `gen.exe` symlink in the action's working directory; under
  sandboxing that is not guaranteed, so the build could fail with `gen.exe: not
  found`. The corpus, the differential `agree` driver, and the validator
  archive are now built as ordinary targets and run directly. Regenerate a
  package's `dune.inc` (`dune build @<pkg>/c/3d`) to pick this up (@samoht)

- A codec whose name has a lower-case segment after an underscore (such as
  `Grpc_message`) now generates C identifiers that match the ones EverParse
  emits. `Wire_3d.everparse_name` capitalized only the first segment, yielding
  `Grpcmessage` where EverParse produces `GrpcMessage`, so the generated FFI
  stubs and the documentation differential harness referenced a name that did
  not exist and failed to link. Every underscore-separated segment is now
  capitalized (@samoht)

- A field constraint that adds narrow unsigned fields (such as `a + b <= 10`
  over two `uint8` fields) now projects to a `.3d` EverParse verifies. The sum
  was emitted at the field's own width, which EverParse refuses to verify because
  it can overflow, while `Wire.Codec.decode` computes the same sum in OCaml's
  wide native int. The addition's operands are now widened to 64-bit so the
  generated C validator and the decoder agree (#188, @samoht)
- An `array` of an open `enum` (`Wire.enum_open`) now validates identically in
  the OCaml decoder and the EverParse-generated C validator: both accept any
  element value. The C validator used to reject element values outside the named
  codes (it constrained each element to the enum's named set), while
  `Wire.Codec.decode` accepted them, so the two disagreed on which buffers are
  valid (#187, @samoht)
- An `enum` / `variants` over a big-endian base (e.g. `enum ... uint16be`) now
  projects to a `.3d` EverParse accepts. It was emitted as a `UINT16BE enum`
  declaration, which EverParse rejects (it types the integer constants as the
  native width: "Expected UINT16BE, got UINT16"). A big-endian-based enum now
  projects as its base scalar with a membership refinement (closed) or bare base
  (open), with no enum declaration (#185, @samoht)
- `Wire.array` / `Wire.array_seq` now reject a zero-width element (`empty` /
  unit) at construction. Such an array carries no bytes and projected to a
  zero-size 3D array EverParse rejects; it is a degenerate shape and is refused
  up front (#184, @samoht)
- `Wire.Codec.v` now rejects, at construction, a non-last field that is a
  `Wire.casetype` with a case body ending in a greedy field (`all_bytes` /
  `all_zeros`). If that case is selected the greedy tail consumes the rest of the
  buffer, starving the following field, so the record failed to round-trip while
  construction silently accepted it. The greedy-must-be-last check now looks
  through casetype case bodies, as it already does through an embedded sub-codec
  (#183, @samoht)
- `Wire.Codec.validate` on a buffer too short to hold the fields a check reads
  now fails cleanly instead of raising `Invalid_argument`. A `where` or field
  constraint may read a field whose offset depends on a length read from the
  buffer; `decode` bounds-checks the buffer first, but `validate` ran the check
  kernel directly, so a short buffer read out of bounds and crashed (#181,
  @samoht)
- A zero-length `Wire.byte_slice` now decodes to an empty slice instead of
  raising `Invalid_argument`. The slice constructor rejects a zero length, so a
  `byte_slice` whose size resolved to 0 crashed the decoder rather than yielding
  the empty slice (#180, @samoht)
- `Wire.Codec.v` now rejects, at construction, a non-last field whose type is an
  embedded sub-codec ending in a greedy field (`all_bytes` / `all_zeros`). Such a
  tail consumes the rest of the buffer with no boundary, so it silently swallowed
  the following field's bytes and the record failed to decode. The greedy-must-be-
  last check already rejected a bare greedy field that is not last; it now looks
  through an embedded sub-codec too (#179, @samoht)
- `Wire.of_string` (and the other typ-level entry points) now return a clean
  `Error` on a truncated input to a variable-size codec, instead of raising
  `Invalid_argument`. Computing the codec's span reads its length and gate fields
  up front; on a buffer too short to hold them, that read ran off the end and
  escaped as an out-of-bounds exception rather than a parse error. `Codec.decode`
  already guarded this; the typ-level path now does too (#178, @samoht)
- `Wire.of_string` (and the other typ-level entry points) now accept an unlisted
  code in a `Wire.enum_open` field, matching `Wire.Codec.decode`. The typ-level
  decoder kept the closed-enum membership check regardless of the `enum_open`
  flag, so it raised `Invalid_enum` on a value the codec accepts, an open enum
  that behaved as closed on that path (#177, @samoht)
- `Wire.Codec.decode` now rejects a parametric codec whose env is missing or
  leaves an input param unbound, raising `Invalid_argument` (naming the param)
  the way `Codec.encode` already does. Decoding without binding a parameter used
  to resolve a parameter-driven field size to 0 and silently truncate the field;
  the binding precondition is now enforced up front on both sides (#176, @samoht)
- `Wire.Param.bind_by_name` now drives a parameter-dependent field size on
  decode, not only `where` clauses and constraints. A field whose size comes from
  a parameter (a `byte_array`, `byte_slice`, or `uint_var` sized by
  `Param.expr`) read as zero bytes when its parameter was bound by name, silently
  truncating the field and misaligning everything after it; only the typed
  `Param.bind` worked. Both binders now resolve parametric sizes identically
  (#175, @samoht)
- A signed integer field's ordering constraint (e.g. `int8 x` with `x < 100`) now
  projects soundly. A signed field becomes an unsigned `UINT*` in 3D, so the
  refinement was emitted as an unsigned comparison and the verified C validator
  disagreed with the OCaml decoder on bytes whose sign bit is set (byte 200 is
  the signed value -56: accepted by OCaml, rejected by C). The ordering is now
  rewritten to its two's-complement unsigned form. A float field ordering
  constraint, which has no faithful unsigned projection (IEEE bit patterns do not
  order as unsigned), is rejected when the codec is projected (#174, @samoht)
- A `Wire.where` placed on a container element (an array or repeat element, or an
  optional inner) is now rejected at codec construction with a clear error.
  EverParse cannot express a refinement on an array or optional element that
  references an outer field, so such a `where` produced a codec whose generated
  `.3d` did not compile while OCaml decode silently ignored the constraint. A
  `where` is supported as a top-level field refinement; move the constraint onto
  the field itself or a codec `~where` (#173, @samoht)
- The doc pipeline's differential self-check (`agree.c`) now links for a codec
  whose name has interior consecutive capitals. EverParse normalizes such a name
  in the validator symbol (`SpaceOSFrame -> SpaceOsframe`), but the harness built
  the symbol from the raw name, so the generated check called an undeclared
  function. The name now goes through the same normalization, which also collapses
  a consecutive-capital run anywhere in a name, not only at the start (#171,
  @samoht)
- The doc pipeline's differential self-check (`agree.c`) no longer false-reports
  a mismatch for a codec with a large payload. The generated reader held each
  corpus line in a buffer one char short of two hex digits per input byte, so an
  8 KB payload (16384 hex chars) truncated the line and misparsed the verdict.
  The buffer is now sized to the input width (#172, @samoht)
- `Wire.Codec.decode` and `Wire.Codec.validate` now enforce a constraint written
  as `Wire.where cond t` on a field, and any field `~action`. Such a `where` was
  projected into the generated `.3d` (so the EverParse C validator rejected
  violating input) but was silently dropped on the OCaml side, so OCaml accepted
  what the verified C rejects; and `Codec.validate` skipped field actions that
  `decode` ran, so the two disagreed. Decode and validate now share a single
  validation path and enforce identical semantics (#169, @samoht)
- A `Wire.casetype` that switches on a `Wire.enum` tag now projects to a 3D
  schema EverParse accepts: each case label is emitted as the enum constant name
  (`case InteriorIndex:`) instead of the raw integer (`case 2:`), which EverParse
  rejected as not a member of the enumerated type (#162, @samoht)
- `Wire.Expr.( = )` and `Wire.Expr.( <> )` are explicitly re-exported from the
  expression language, so equality in a local `Expr.(...)` open builds `Eq` /
  `Ne` constraints rather than depending on the surrounding equality binding
  (@samoht)
- A codec whose name contains a capital `V` (e.g. `VeritySuperblock`) now
  generates its C parser. EverParse names the validator `<Name>Validate<Name>`,
  and the name reader stopped at the first `V`, so C generation failed for any
  such name (#143, @samoht)
- A `Wire.enum` field now enforces its membership in the EverParse-generated C
  validator, rejecting values outside the named cases exactly as `Codec.decode`
  does (raising `Invalid_enum`), including for an enum nested inside a sub-codec
  or record. Previously the verified C accepted out-of-range values the OCaml
  decoder rejects (#131, @samoht)
- Decoding a `Wire.enum` through the `Codec` API now rejects a value that is
  not one of the named cases (raising `Invalid_enum`), on a scalar field and on
  every array or repeat element, matching the EverParse validator and the
  `Wire.of_string` path. The `Codec` decoder previously accepted any value
  (#130, @samoht)
- A `Wire.lookup` field now enforces its index bound in the EverParse-generated
  C validator, on a scalar field and on every `Wire.array` / `Field.repeat`
  element, so the validator rejects out-of-range indices exactly as the OCaml
  decoder does. Previously the verified C accepted indices the decoder rejects
  (#126, @samoht)
- A `Wire.nested` / `nested_at_most` field, and a `Wire.byte_array_where` span
  inside one, now generate a verified EverParse validator. Previously any codec
  with such a field failed schema generation (#99, #132, @samoht)
- `Wire.rest_bytes` now generates a verified EverParse validator. Previously
  any codec with a `rest_bytes` field failed schema generation regardless of
  width (#117, @samoht)
- A codec with a `Wire.uint63` / `uint63be` field now generates a verified
  EverParse validator (projecting to the 8-byte `UINT64`). Previously schema
  generation failed and the codec had no verified C parser at all (#125, @samoht)
- A codec mixing signed-integer or float fields of different widths (e.g. a
  `float32` then a `float64`, or an `int8` then an `int32`) now generates a
  verified EverParse validator. Previously such a codec had no verified C parser
  (#127, @samoht)
- A `Wire.enum` used as a `Wire.array` / `Field.repeat` element (or inside an
  optional or sized region) now generates a verified EverParse validator.
  Previously such a codec had no verified C parser (#128, @samoht)
- A statically-absent `Field.optional` / `Field.optional_or` (`~present:false`)
  now generates a verified EverParse validator. Previously the codec had no
  validator at all (@samoht)
- An `Action.on_success` ending in a conditional `Action.return_bool` (an
  `Action.if_` with a `return` branch), and an `Action.on_act` whose body ends
  in `Action.return_bool`, now generate a verified EverParse validator (@samoht)
- Projecting an expression (a `~where` / field constraint / `~self_constraint`)
  that uses a construct with no projectable form (a negative integer literal or
  `field_pos`) now raises a clear `Invalid_argument` instead of emitting C that
  EverParse rejects with a cryptic error. Every other operator (shifts, bitwise,
  casts, mod, div, comparisons, `sizeof`, `sizeof_this`) projects (@samoht)
- The generated dune rule now compiles the EverParse C under strict C11
  (`-std=c11 -D_DEFAULT_SOURCE`) instead of `-std=c99`, so the verified
  validators build on Linux glibc (the BSD endian helpers the C uses need
  `_DEFAULT_SOURCE`) (@samoht)
- Decoding no longer raises `Invalid_argument` on adversarial input where a
  `Field.repeat` byte budget, or a variable field's cross-field size, exceeds
  the buffer; an oversized length now fails with a clean `Parse_error`
  (#117, @samoht)
- A cross-field length / offset / `present` expression that reads an integer
  beyond the native int range (a `uint64` / `int64` length over `max_int`), or
  reads a non-integer field, now fails the parse instead of silently reading 0.
  The old behaviour masked malformed input (#82, @samoht)
- A `byte_array` / `byte_slice` (or any field) whose `~size` reads a
  `Field.optional_or` field no longer resolves that size to 0, decoding the span
  as empty (silent truncation) and raising a length mismatch on `Codec.encode`;
  it now reads the present-or-default value (#101, @samoht)
- A greedy field (`all_bytes` / `all_zeros`) reads the rest of the buffer, so it
  is now rejected with `Invalid_argument` anywhere it is not the final field: a
  non-last field of a codec, a `Field.repeat` / `Wire.array` element (or a
  sub-codec ending in one), or a `Wire.casetype` case body. It remains valid as
  the last field, the supported way to consume the rest (#107, #110, #111, @samoht)
- An embedded sub-codec's `where` clause and field constraints are now enforced
  when the codec is decoded as a field or element. They were silently dropped on
  the embedded path, so a value the sub-codec would reject standalone was
  accepted when embedded (#108, @samoht)
- `Wire.array` / `array_seq` now reject a non-fixed-width element (a
  `Wire.nested` region, a `Wire.byte_array_where` refined span, or a nested
  `Wire.array`) at construction with `Invalid_argument`. An element must be a
  scalar, a fixed byte span, or a fixed-size sub-codec, matching `Field.repeat`
  (#107, @samoht)
- `Wire.array` / `array_seq` / `Field.repeat` / `repeat_seq` over a sub-codec
  built only from byte-span fields (`byte_array`, `byte_slice`, a varint) now
  raise `Invalid_argument` at construction. A sub-codec with at least one
  fixed-size field is accepted as before (#115, @samoht)
- `Wire.array` / `array_seq` over a float, a signed integer, a `uint63`, or a
  `Wire.where` / `Wire.map` wrapping a fixed byte span no longer raise during 3D
  projection; all fixed-width scalars and wrapped byte spans now project
  (#116, @samoht)
- `Field.repeat` / `repeat_seq` now reject an element type that has no clean
  per-element projection (a sub-byte `bits` field, a refined or at-most byte
  span, `all_zeros`, or a nested `array` / `nested`) at construction with a
  clear `Invalid_argument`. Supported elements are unchanged: fixed-width
  scalars and byte spans, `zeroterm`, sub-codecs, and casetypes (#97, @samoht)
- A bitfield (`Wire.bits` / `Wire.bit`) is now rejected with `Invalid_argument`
  at construction as an element of `Field.repeat` / `Wire.array` / `Wire.nested`
  or as a `Field.optional` inner: a bitfield only exists packed inside a record,
  with no standalone wire form (#90, #98, #107, @samoht)
- A `Wire.casetype` whose case body is a NUL-terminated string (`zeroterm` or
  `zeroterm_at_most`) now encodes, decodes, and sizes correctly as a
  `Field.repeat` element, so a list of such tag-dispatched options round-trips
  (#103, @samoht)
- `Field.repeat` over a `Wire.casetype` element now encodes and decodes instead
  of raising. This covers DHCP-style options whose cases mix bare single-byte
  tags with length-prefixed bodies (#75, @samoht)
- `Field.repeat` / `Wire.array` over a fixed `byte_array` / `byte_slice` element
  (a list of n-byte chunks, e.g. fixed-size addresses) now encode, decode, and
  generate a verified EverParse validator; decoding previously raised `Failure`
  (#89, #92, @samoht)
- `Field.repeat` over a `zeroterm` element (a list of NUL-terminated strings
  within a byte budget) now encodes, decodes, and generates a verified EverParse
  validator; it previously raised `Failure` when decoding (#93, @samoht)
- `Wire.array` over a fixed-size sub-record (a `Wire.codec` element, e.g. an
  array of `{ x; y }` points) now decodes instead of raising `Failure`
  (#96, @samoht)
- A `Field.optional_or` with a dynamic gate now generates an EverParse C
  validator that accepts the bytes `Codec.encode` produces; the two previously
  disagreed on the field's layout when the gate was false (#88, @samoht)
- A codec that embeds a variable-size sub-codec (`Wire.codec`, e.g. a
  length-prefixed string) as a field is now accepted by EverParse; it previously
  failed schema generation (#87, @samoht)
- A variable-size sub-codec or `Field.repeat` may now follow a variable-size
  field (#38, @samoht)
- `Codec.encode` no longer requires an `?env` for a codec whose only parameters
  are decode-side outputs (a field with an `Action.assign` into a
  `Param.output`). Output params are never read when encoding, so demanding an
  env raised `Invalid_argument` spuriously, and an output-param sub-codec
  embedded as a field could not be encoded at all (#95, @samoht)
- `Codec.size_of_value` now sizes a `Field.repeat` with a dynamic budget, a
  `Wire.casetype` field, and a packed bitfield (wrapped by `Wire.bit` or an
  enum / map) correctly. The first two were under-counted (so `Codec.encode`
  overran the buffer) and the bitfield over-counted (so `encode` raised a
  spurious `Invalid_argument`) (#72, #78, #79, @samoht)
- `Codec.encode` now raises `Invalid_argument` when the writer emits fewer
  bytes than `size_of_value` promised, instead of shipping a value with
  uninitialised trailing bytes (#62, @samoht)
- `Codec.encode` into a too-small buffer now fails with a precise byte count
  instead of writing past the end (#61, @samoht)
- `Field.optional` / `Field.optional_or` with a dynamic gate now encode from
  the value (the gate selects the decoded value or the default on decode);
  `optional` raises `Invalid_argument` rather than writing a phantom byte or
  overrunning the buffer when the gate and value disagree (#58, @samoht)
- `Field.optional` / `Field.optional_or` predicates that use bitwise / shift
  / mod operators are no longer silently treated as always-true, and
  `Field.ref` on an `optional` field now reads the decoded value instead of 0
  (#48, @samoht)
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
- Fix C stub generation for schema names with two or more leading capitals
  (e.g. `IPv4`, `EP_Header`) (#36, @samoht)

## 0.9.0

Initial release.
