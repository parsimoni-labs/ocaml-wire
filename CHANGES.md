## unreleased

### Fixed

- `Wire.Everparse.write` no longer refuses a family in which a sub-codec is both
  packed as a codec of its own and reached through another codec's field,
  reporting the two as conflicting definitions of one type. They differ only in
  how the type is used, not in what it is, so they now collapse into a single
  declaration that still gets a validator of its own and keeps the codec's doc
  comment, whichever schema was projected first (#346, @samoht)

- A casetype tag whose enum sits over a `lookup`, a `where` or a 64-bit base no
  longer fails the 3D projection with an assertion failure. Every integer
  carrier an enum base accepts now yields a case label (#347, @samoht)

- A parse error from an unmatched casetype tag now names the casetype field. It
  used to arrive with an empty field path, leaving a caller no way to tell which
  field the bad tag belonged to (#348, @samoht)

- `Wire.Everparse.Raw.field_seeds` now names a casetype's case indices, so
  `Wire_3d.generate_corpus` can reach a tagged record instead of reporting the
  codec as vacuous (#348, @samoht)

- `Wire_3d.generate_corpus` no longer gives up on a record whose length field
  the byte draw filled with a huge value. A draw asking for more bytes than a
  corpus line can carry is brought back into range rather than the codec being
  abandoned, which covers both dependent and unconstrained length fields
  (#349, @samoht)

- `Wire.Codec.wire_size_at` takes `?env` and refuses without one when the codec
  has input params, as `decode` already did. An unbound param reads 0, so a
  param-sized field measured as empty and the reported extent was shorter than
  the record; `Wire_3d.generate_corpus` recorded a reject verdict for input its
  own codec accepts, and a differential over that corpus blamed the generated C
  validator for the harness's answer (#350, @samoht)

- `Field.repeat` now explains itself when the encoded elements do not fill the
  declared byte budget. The budget is the region size the description declares,
  which encode holds the values to as well, so a caller who does not know that
  size where the codec is built should take it as a `Param.input` rather than
  baking in a literal. The documentation also records that there is no sizeless
  repeat and why: 3D's only list construct is the byte budget, so a repeat that
  consumed whatever remained would have no projection (#351, @samoht)

- `Wire.Codec.size_of_value` takes `?env` and resolves a field whose byte extent
  is an input parameter, refusing when it has none rather than reporting 0. It
  used to under-report such a field by its whole width, so a caller sizing a
  buffer from the answer got one too short and `encode` ran off the end with a
  raw out-of-bounds instead of a wire error (#353, @samoht)

## 1.2.0

### Added

- Support wasm_of_ocaml (31-bit int) and js_of_ocaml (32-bit int). Codecs no
  longer silently truncate values that exceed the target's native integer range
  (#232, @samoht)

- Add `Wire.Expr.lsr64`, an int64 logical shift right by a constant amount, for
  isolating the high bits of a full-width field inside a constraint. It projects
  to a 3D refinement EverParse verifies (#232, @samoht)

- Add `Wire.Codec.field_readers` and `Wire.Everparse.Raw.int_slots`.
  `field_readers` is the decoder's own reader for every named int-valued field,
  keyed by name, so a caller holding a type-erased codec can read a field
  without a field handle; `int_slots` is the byte slot of every named whole-byte
  integer field (#318, @samoht)

- Add `Wire.Everparse.Raw.field_seeds`, exposing constrained whole-byte integer
  values, and seed `Wire_3d.generate_corpus` from them and from accepted inputs,
  so a generated corpus reaches values uniform bytes never would. A one-sided
  corpus, which cannot tell a validator from a constant answer, is now refused
  (#314, @samoht)

- Add `Wire.equal_error_kind`, structural equality on error kinds ignoring
  location, and `Wire.Everparse.equal_field_action_form` (#299, #302, @samoht)

- Document how to make a whole field group conditional with `Field.optional`
  over a sub-codec, keeping the group's byte-length prefix and dependent
  `Field.repeat` local to the group while later fields stay in the parent codec
  (#236, @samoht)

### Changed

- **Breaking:** A fixed-width integer field decodes to a carrier whose range is
  the field's, so a value it cannot hold is refused where it is built instead of
  being masked into a legal-looking one on the way to the wire: `uint8` to
  `Wire.UInt8.t`, `int8` to `SInt8.t`, `uint16` and `uint16be` to `UInt16.t`,
  `int16` and `int16be` to `SInt16.t`, `uint32` and `uint32be` to `UInt32.t`
  (was `Optint.t`), `int32` and `int32be` to `SInt32.t`, `uint64` and `uint64be`
  to `UInt64.t` (was `int64`). The 8- and 16-bit carriers are a `private int`,
  read with `to_int` and built with the range-checked `v`; the wider ones are
  abstract, read with `to_int32` / `to_int64` and built with `of_int32` /
  `of_int64` or the range-checked `of_int`. `SInt32` and `UInt32` are
  deliberately not interchangeable: they share a representation, so a value read
  with the wrong signedness would be a silent misparse rather than a type error
  (#321, #322, #323, #330, #333, #335, #336, @samoht)

- **Breaking:** `Wire.uint` decodes to `Optint.Int63.t` rather than a native
  `int`, spelled as that type rather than the `Wire.Private` alias: a 7-byte
  value needs 56 bits, which used to truncate on a narrow-int target. Read it
  with `Optint.Int63.to_int` / `to_int64` (#232, #270, @samoht)

- Integer-valued combinators no longer insist on a native `int` base.
  `Wire.enum`, `enum_open`, `variants`, `lookup` and `bit` sit over `uint32`,
  `int32` or `uint64` as readily as over `uint8`, and project to EverParse the
  same way; `Wire.rest_bytes` takes a size parameter of any integer type, and
  `Wire.Field.int64` a field of any type carrying a 64-bit slot (#321, #323,
  #328, @samoht)

- **Breaking:** Require OCaml 5.3, up from 5.2 (#320, @samoht)

- **Breaking:** `Codec.encode` and `Wire.to_string` raise `Invalid_argument` on
  a value their own decoder rejects, rather than writing bytes that fail to read
  back: an unlisted value in a closed `enum`, a non-zero byte in an `all_zeros`
  padding field, a `byte_array_where` byte that fails its `~per_byte`
  refinement, and a record whose `where` clause or field `~constraint_` does not
  hold. Encoding a well-formed value is unchanged (#263, @samoht)

- **Breaking:** Encoding an integer a field cannot hold raises
  `Invalid_argument` instead of dropping the bits that do not fit, so `int8`
  refuses `200` rather than writing it back as `-56`, and `bits ~width` and
  `uint ~size` accept exactly their declared width. A masked value is itself
  legal at that width, so nothing downstream could tell it from a value meant
  that way. Applies to `Wire.to_string`, `Codec.encode` and `Codec.set` alike
  (#275, @samoht)

- **Breaking:** Encoding a `byte_array`, `byte_array_where` or `byte_slice`
  whose length differs from its declared `~size` raises `Invalid_argument`. It
  used to truncate a longer value, zero-pad a shorter one, or, through the
  streaming writer, desynchronise every following field, so the bytes on the
  wire could differ from the value a caller had signed, hashed or
  length-prefixed. For a short value in a fixed-size region use
  `zeroterm_at_most`, which is NUL-terminated and round-trips (#259, @samoht)

- Require `bytesrw` >= 0.4.0 (#297, @samoht)

- `Codec.load_word` reads the bitfield base word as an `Optint.t` (was a native
  `int`) and `Codec.extract` takes it, so the word-at-a-time batch API is exact
  for 32-bit bases on every platform; on a 64-bit host the word stays an unboxed
  int (#232, @samoht)

- Change the exception raised for a description no decoder or encoder can
  handle, such as a casetype value no case projects or an unresolved type
  reference, from `Failure` to the `Invalid_argument` documented in `wire.mli`.
  The buffer is untouched on those paths, so only the exception type changes
  (#291, @samoht)

- Change the `zeroterm` and `zeroterm_at_most` encode errors to report as
  `Wire.encode` on every path. `Codec.encode` reported the same two faults, a
  NUL in the value and a value too long for its region, under its own name
  (#269, @samoht)

- Change the generated `.3d` C struct tags to `Wire<Name>` rather than
  `_<Name>`. Public typedef names are unchanged, and standalone entrypoints
  follow as `<Base>CheckWire<Name>` (#235, @samoht)

### Fixed

- A codec that embeds a sub-codec now projects to a schema EverParse accepts
  whatever that sub-codec holds. The types a sub-codec's own fields name were
  declared after it rather than before, and a refined byte span reached only
  through a sub-codec was never declared at all, so EverParse rejected the
  schema with a "not found" naming a type the codec does use (#344, @samoht)

- Generating C for a codec with a long name no longer fails with
  `Invalid_argument "List.iter2"`. Past a certain length the generated callback
  declarations wrap onto a second line, which the field-plug generator read as
  no declarations at all; both layouts are read now, and a header that still
  cannot be read is named in the error rather than surfacing as a crash
  somewhere else (#344, @samoht)

- A value with no fixed wire size read through `Wire.of_reader` no longer costs
  time and memory quadratic in its length. The reader rebuilt the whole
  accumulated buffer and re-parsed it from offset zero after every slice, so a
  64 KiB `zeroterm` arriving one byte at a time took 1.7 s where it now takes a
  millisecond, at the top level and inside a codec alike (#325, #331, @samoht)

- `Codec.validate` allocates a constant amount whatever the sender sends, and
  nothing at all for a codec with no parameters. It used to run the field
  readers for their checks and drop what they built, so a refined span, a
  `zeroterm_at_most`, an `all_zeros`, a `repeat`'s elements and a closed
  `enum`'s named cases each cost memory in proportion to lengths and counts the
  sender chooses. Sub-codecs reached through an `array`, a `nested` region or a
  casetype case body are checked in place too, and `Codec.decode` no longer
  keeps a second copy of a span it returns (#286, #289, #290, #292, #293, #294,
  #295, #329, @samoht)

- A fixed-offset `Codec.get` or `Codec.set` with no parameters, and
  `Wire.of_string` / `Wire.of_bytes` on a struct type, no longer allocate per
  call: direct scalars represented as immediate OCaml values, bitfields, enums
  and immediate-valued maps measure zero words on a non-Flambda release build,
  and the validator cache lookup no longer builds a closure and an option. Boxed
  results such as `int64`, and allocations performed by a `map` callback itself,
  remain visible to callers. The same accessors are also faster than before
  (#239, #269, @samoht)

- A fresh domain's first decode or validate no longer costs about a word per
  compiled validator the program ever built, which reached roughly 1 MB at
  50,000 validators and was charged even to domains that never decode
  (#320, @samoht)

- `Codec.decode` and `Codec.validate` no longer accept bytes that
  `Wire.of_string` and the EverParse validator built from the same schema
  reject. A sub-codec field's constraints, `where`, closed-enum membership,
  `~per_byte` refinements and actions were skipped, a `byte_array_where`'s
  `~per_byte` ran on the direct parser alone, and a `Field.repeat` whose byte
  budget does not split into whole elements passed (#268, #305, #311, @samoht)

- `Codec.validate` no longer raises `Invalid_argument` where a parse error was
  due. It reaches a field at whatever offset the layout computes and those reads
  were unguarded, so an overlong variable-size field pushing its successors past
  the end of the buffer crashed the documented gate for untrusted input. Same
  for `Wire.of_bytes` and `Wire.of_string` on a struct, an `optional` gated on a
  runtime expression, and a `uint` of runtime width. All now report end of input
  at the field's own offset (#271, #276, @samoht)

- `Codec.get` and `Codec.set` no longer read or write past the field they name.
  A bitfield's base word is assembled from byte reads that went unchecked, so a
  truncated frame read at an application-chosen offset answered with the bytes
  that followed it and `set` overwrote them, leaving half a word behind when it
  raised; and a `byte_array`, `byte_array_where` or `byte_slice` whose `~size`
  is known only at decode time wrote the value's own length, running over the
  fields after it with no error. Both spans are now checked first (#262, #288,
  @samoht)

- `Codec.set` on a sub-codec, casetype or array field no longer leaves a refused
  value in the buffer. The writer laid the value down and only then checked it,
  against the documented promise to leave the buffer untouched; those writers
  now restore the bytes they replaced. `Codec.encode` is not transactional and
  now says so: after a failed encode the destination holds a partial record
  (#280, @samoht)

- `Codec.get` and `Codec.set` handle an `optional` or `optional_or` field whose
  gate is fixed at construction, a shape `Codec.decode` reads without trouble
  and `get` used to raise `Failure "build_field_reader: unsupported type"` on. A
  runtime gate stays unsupported and refuses with `Invalid_argument` naming the
  shape (#280, @samoht)

- `Codec.size_of_value` no longer answers for a write that cannot happen or
  under-reports a field's width: a casetype value no case projects raises
  `Invalid_argument` as encode does, and `Codec.set` refuses before writing,
  while a fixed-size `byte_array`, `byte_array_where` or `byte_slice` reports
  its declared width, so a buffer allocated from the answer exactly fits the
  bytes written (#238, #296, @samoht)

- `Unexpected_eof` reports byte counts, not buffer positions, and counts only
  bytes the parse was handed. The `expected` and `got` fields carried absolute
  positions at several sites, so the pair shifted with the offset the frame was
  read at and could come out equal to each other; and a value inside a `nested`
  region or a `repeat` budget was sized from whatever the buffer held past the
  region, so the reported shortfall named bytes the parse never claimed
  (#285, #326, #332, @samoht)

- A parse error names the field's own offset, so a caller locating the field
  from `at` no longer reads or rewrites the wrong bytes. A failing field
  `~constraint_`, an `enum` membership failure and a staged read through a
  bitfield reported the enclosing record's base, and a `lookup` tag out of range
  reported byte 0 whatever offset the frame was read at. A dynamic layout still
  reports the record base (#306, @samoht)

- A failure under a size or offset expression reports its own error. A
  validation error or an exception from a `map` callback came out as an end of
  input on a buffer that was long enough, and a byte span whose size expression
  went negative escaped as `Invalid_argument "String.sub"`; truncation is now
  raised by the reads themselves, naming the missing span (#267, @samoht)

- `Field.repeat`, `array`, `array_seq` and `nested` hold to their declared size
  on decode and encode. A repeat consumed or emitted more or less than its byte
  budget, the array encoders accepted a short value and left zero-filled phantom
  elements, and `nested` let its inner value consume less than the region. Use
  `nested_at_most` where trailing region padding is intentional; it remains
  permissive and zero-pads on encode (#238, @samoht)

- `Codec.v` rejects duplicate field names, and distinct parameter handles with
  the same name. Name-based field access and parameter environments silently
  aliased the first declaration (#238, @samoht)

- A constant size or length expression such as `Expr.(int 2 + int 2)` behaves
  exactly like the literal `int 4`. It used to slip past every check and fast
  path that looks for a literal size, so such a field encoded without its
  exact-length check, reported its codec as variable-size, and bypassed the
  `uint` 1-7 size guard (#259, @samoht)

- Operations on the same parametric codec are safe to re-enter and interleave
  across fibers with different `Param.env` values: embedded codecs, casetype
  bodies, fixed-size wrappers and repeated elements no longer read another
  operation's field sizes. An env built for a different codec is rejected first,
  on encode, decode, validation and staged getters alike, including where the
  parameter counts happen to match (#237, #239, @samoht)

- A field wider than the target's native `int` no longer decodes or encodes
  wrong under wasm_of_ocaml and js_of_ocaml. A bitfield touching bit 31 of its
  base word dropped that bit, a 4-byte signed field dropped the top bits on the
  way in and sent the frame back out as different bytes with the encode-side
  range check disabled, and a NaN passed a float64 `is_finite` check. The
  emitted 3D refinements are unchanged (#232, #321, @samoht)

- A value too wide for the native `int` reports a typed `Value_out_of_range`, or
  a contextual `Invalid_argument` naming the case index or parameter, where
  reading a `uint32`, `uint63` or `uint` field, projecting a casetype and
  `Param.bind` used to leak a bare Optint `Failure`. The validation slots mirror
  only values the int can hold, as `uint64` already did (#232, #237, @samoht)

- `uint32` and `uint64` values order as unsigned numbers on every target. Their
  carriers compared the top bit as a sign, so the largest value of each ranked
  below 1 (#322, #323, @samoht)

- A shape with no valid encoding or no EverParse projection is refused at
  construction with a clear error, rather than building and misbehaving later: a
  `Field.repeat` element whose greedy tail sits inside a sub-codec and so
  swallows every element after it, a zero-size byte span as an `array` or
  `Field.repeat` element, an `optional` field that can expose a consume-rest
  payload with another field after it, and a `field * constant` byte size whose
  `field <= bound` constraint lets the maximum reach EverParse's `2^32` limit.
  Products without that conclusive shape stay deferred to EverParse, avoiding
  speculative rejections (#237, #239, #256, #266, #280, @samoht)

- A `Field.repeat` element that turns out to consume no bytes reports an
  end-of-input error from `Wire.of_string` and `Wire.of_bytes` instead of
  looping forever (#256, @samoht)

- `Wire.Ascii` diagrams render every expression a size or constraint can
  contain. Bitmasks, shifts, division, casts, conditionals, `sizeof` and
  parameter references used to print as a bare `?`, so a trailing payload sized
  by `Wire.rest_bytes` showed up as `(? - ? bytes)` (#257, @samoht)

- Generated C no longer enforces the wrong specification when names collide. Two
  codecs whose artifacts differ only in the leading capital or in an uppercase
  run, and two codecs of a family that declare different types under the same
  name, are refused with a clear error instead of the second write winning and
  leaving the shadowed codec's stubs validating against another codec's spec;
  and two domains building descriptions at once can no longer mint the same
  synthesised `byte_array_where` element name, whose 3D typedef was then emitted
  twice. Types declared identically by several codecs still collapse to a single
  declaration (#255, #266, #272, @samoht)

- The generated FFI stubs no longer lose field values. The parse callback
  collected the boxed arguments in a bare `value` array, which is not a GC root,
  so a minor collection triggered by a later box could move or reclaim an
  earlier one and the continuation silently received garbage. `CAMLlocalN` now
  roots them (#272, @samoht)

- The emitted `EverParseEndianness.h` compiles on a target that is neither
  Linux/glibc nor Apple. The `__BYTE_ORDER__` branch was spliced only when the
  header was absent, which it never is, so the header a cross-compiled
  standalone archive gets stopped at `#error "Unsupported platform"`
  (#272, @samoht)

- EverParse generation no longer interprets output paths, executable paths and
  schema filenames that contain spaces or shell metacharacters as commands
  (#239, @samoht)

- A package's committed `.3d` specs, `dune.inc` and EverParse C can no longer go
  stale. `dune runtest` diffs the specs and rules against freshly generated ones
  and rechecks an installed `<Name>.provenance` stamp holding the stdlib
  BLAKE2b-256 digest of the `.3d` the C was built from, no EverParse needed. The
  committed schema is a plain source rather than a promoted build target, so
  drift fails `runtest` with a promotable report and `dune promote` writes the
  new bytes after an intended codec change (#235, #317, @samoht)

### Removed

- **Breaking:** Remove `uint63` and `uint63be`. An 8-byte field spans more
  values than their `Optint.Int63.t` carrier holds, and decoding masked the
  difference away instead of failing, on both the OCaml and the generated C
  path. Use `uint64` / `uint64be` instead, whose carrier is exactly the eight
  bytes on the wire, and convert an existing value with `Optint.Int63.to_int64`.
  `Wire.uint ~size` is unaffected: its carrier is exact for the one to seven
  bytes it accepts (#316, @samoht)

- Delete the dependency on `eio`. Nothing in the library used it, it was only
  ever linked, unused, into `wire.diff` (#233, @samoht)

## 1.1.0

### Added

- `Wire.Expr.land64` is an int64 bitwise AND for masking a full-width `uint64`
  field inside a constraint, and `~self_int64` / `Field.int64` now accept a
  `map`-decoded uint64 field. A sign-magnitude offset bound such as a bsdiff
  seek (`land64 self mask <= max`) is now expressible and projects to a 3D
  refinement EverParse verifies (#227, @samoht)

### Changed

- The standalone `c/` archive (`3d` mode) now builds and installs in every dune
  context through that context's own toolchain, so a cross build produces and
  installs a target-native verified parser instead of skipping the C. The build
  uses `%{ocaml-config:c_compiler}`, the `ocaml-config` partial linker, and the
  binutils that compiler resolves (`-print-prog-name`), so a cross `cc` finds
  the target's `objcopy`/`ar`. `EverParseEndianness.h` gains a freestanding
  `__BYTE_ORDER__`/`__builtin_bswap*` branch so it compiles for an OS-less
  target (a unikernel defines neither `__linux__` nor `__APPLE__`). Only C
  regeneration and the `agree` differential test stay host-gated (#231, @samoht)

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

- A standalone `Wire_3d` archive no longer exposes its raw `<Base>Validate*`
  validators to C callers. Those entrypoints skip a `StartPosition <=
  InputLength` bound check, so a direct caller passing `StartPosition >
  InputLength` underflowed the read span and went out of bounds. The install now
  ships only the checked `<Base>Wrapper.h` header (not `<Base>.h`), and the
  archive build localizes every symbol except the `<Base>Check<Codec>` wrappers,
  so the raw validators are neither declared nor linkable; the wrapper validates
  from position 0 and is the safe public API (#228, #229, @samoht)

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
  truncating the field through OCaml's native `int` (#160, @samoht)
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
  `Wire_3d.generate_agree` expose the two halves (#158, @samoht)
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
  be given a unique, meaningful name before code generation (#121, @samoht)
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
  `-Werror`. Regenerate a package's `agree.c` to pick this up (#196, @samoht)

- The `dune.inc` rules generated by the `Wire_3d` pipeline now invoke the
  generator through `%{exe:gen.exe}` and use plain dune actions instead of shell
  `(system ...)` commands. The old rules ran `./gen.exe`, which relied on dune
  placing a `gen.exe` symlink in the action's working directory; under
  sandboxing that is not guaranteed, so the build could fail with `gen.exe: not
  found`. The corpus, the differential `agree` driver, and the validator
  archive are now built as ordinary targets and run directly. Regenerate a
  package's `dune.inc` (`dune build @<pkg>/c/3d`) to pick this up (#195,
  @samoht)

- A codec whose name has a lower-case segment after an underscore (such as
  `Grpc_message`) now generates C identifiers that match the ones EverParse
  emits. `Wire_3d.everparse_name` capitalized only the first segment, yielding
  `Grpcmessage` where EverParse produces `GrpcMessage`, so the generated FFI
  stubs and the documentation differential harness referenced a name that did
  not exist and failed to link. Every underscore-separated segment is now
  capitalized (#193, @samoht)

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
  (#159, @samoht)
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
  validator at all (#137, @samoht)
- An `Action.on_success` ending in a conditional `Action.return_bool` (an
  `Action.if_` with a `return` branch), and an `Action.on_act` whose body ends
  in `Action.return_bool`, now generate a verified EverParse validator (#139,
  @samoht)
- Projecting an expression (a `~where` / field constraint / `~self_constraint`)
  that uses a construct with no projectable form (a negative integer literal or
  `field_pos`) now raises a clear `Invalid_argument` instead of emitting C that
  EverParse rejects with a cryptic error. Every other operator (shifts, bitwise,
  casts, mod, div, comparisons, `sizeof`, `sizeof_this`) projects (#140,
  @samoht)
- The generated dune rule now compiles the EverParse C under strict C11
  (`-std=c11 -D_DEFAULT_SOURCE`) instead of `-std=c99`, so the verified
  validators build on Linux glibc (the BSD endian helpers the C uses need
  `_DEFAULT_SOURCE`) (#134, @samoht)
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
  `Constraint_failed` error instead of raising (#60, @samoht)
- `Wire.to_string` on a `Wire.nested ~size:n` field now zero-pads to `n`
  bytes when the inner writes fewer, so it agrees with `Wire.of_string`
  (#60, @samoht)
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
