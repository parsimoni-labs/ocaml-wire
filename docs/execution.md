# How a codec runs

A codec is compiled on its first use into a plan for one record in a
buffer. Domains that use a new codec at the same time may each compile it:
the compile is pure and one plan is kept. Every operation runs part of that
plan, and the plan reads untrusted bytes, so the rules below say which reads
are checked and why the others cannot go wrong.

## The three phases

`decode c buf off` runs three phases in order, and the other operations run
a prefix of them.

1. **Frame.** Check `0 <= off` and `off + min_size <= Bytes.length buf`,
   where `min_size` is the sum of the fixed sizes of the record's fields. A
   variable-size record then reads its length fields, with checked reads, and
   checks that its end lies inside the input. After the frame, every byte at
   a fixed offset below `min_size` is in the buffer.
2. **Check.** Evaluate the field constraints, the `where` clause, closed enum
   membership and `map` refusals, in field order. The first failure is the
   one reported. A scalar inside the frame is read into its slot only when a
   constraint, an action or the `where` clause uses it, and a comparison of a
   field with a literal or another field is tested directly. Any other field
   runs its general validator, so the choice is made field by field.
3. **Read.** Build the value. A field at a fixed offset inside `min_size` is
   read with no bounds check; any other field is read with one. A size that
   reads an output parameter gets the value the check pass left, which is the
   one the check used when every field that assigns an output parameter comes
   before every field that reads one. In any other order the read keeps the
   caller's values and each sub-codec checks itself where it is read.

`validate` runs the frame and the checks. `get` frames one field and reads
it. `encode` writes the fields and, when the codec has constraints, runs the
checks on what it wrote.

## Invariants

These are what the unchecked reads rely on. Each one is enforced where the
codec is built, so no input can break it.

- **Unchecked reads stay in the frame.** A read without a bounds check is
  created only for a field whose offset and width are constants of the
  schema, with `0 <= offset` and `offset + width <= min_size` at the point
  the field is added (`Codec.field_read`).
- **The frame only grows.** `min_size` never decreases as fields are added,
  and never exceeds `Sys.max_string_length`, so the frame check cannot
  overflow (`Codec.add_min_size`). A negative or oversized literal size is
  refused when its type is built (`Types.literal_size`).
- **Bytes never choose an unchecked offset.** An offset that depends on the
  data (after a variable-size field, or set by a parameter) is always read
  with a check.
- **Input never raises `Invalid_argument`.** Bad bytes give a parse error;
  `Invalid_argument` is for misuse of the API (a negative offset, a missing
  `env`).
- **Field order.** Reads and checks run in declaration order, so the error
  names the first bad field.
- **One answer.** `decode` fails exactly when `validate` does, and both agree
  with the EverParse C validator; the differential fuzzer checks this.

The `paranoid` build profile swaps the unchecked reads for checked ones, so
the test suite and the fuzzers report any read outside the frame as an
exception instead of reading past the buffer.

## Generated code

`Wire_ocaml` writes a codec out as OCaml with the same three phases. It reads
the codec once into a plan: runs of fields at constant offsets (prefixes),
spans whose size an expression gives, optional fields (gates), sized windows
(regions) and where clauses, in the order the codec checks them, over
variables unique in the plan. One walk over the plan writes every function
that checks a record: `is_valid`, the decoders and the function that finds
the codec's error for a fixed record, and `fold`, `fold_frames` and `decode`
(and with `~step`, `fold_step` and `fold_frames_step`) for a codec of any
shape in scope. OCaml without flambda never inlines a function passed as an
argument, nor a function whose body defines a closure, so `fold_frames`
rejects a frame with no closure of its own, and the step functions call
their consumer by name, with no closure call and no allocation per frame.
The invariants above hold for it as follows.

- **Unchecked reads stay in the frame.** A prefix is read only after one
  check that the bytes it spans, and those of the prefixes and where clauses
  right after it, lie in the current window: the record's frame, or the
  window a region gives its contents, whose size is itself checked to lie in
  the enclosing one. A span's size is checked against the window too, and
  checked not to be negative unless a range or a passed check proves it.
  Positions are relative to the window, so adding the window's offset to
  reach a byte cannot overflow, and a short frame in `fold_frames` cannot
  borrow bytes from the next.
- **Arithmetic agrees with the codec.** An addition, subtraction or product
  whose operands' ranges keep it inside the native int is written as plain
  arithmetic; any other is checked, and an overflow rejects the record, or
  in the error-finding function raises the codec's error at the same place.
  Code that holds a value of more than 31 bits refuses to load where ints
  have 32 bits or fewer; the rest runs on js_of_ocaml and wasm_of_ocaml as
  written.
- **Branches only drop what they decide.** A run of optional fields whose
  conditions exclude each other, the cases of a tag, is one decision tree
  that tests each atom of their conditions at most once on a path. What a
  branch learns (a tag's value, a passed check) simplifies only the code on
  that branch. The code after an optional field is copied into its
  branches only while it is short, holds no optional field and the copies
  along a path stay under 16; otherwise it is written once, in a local
  function the compiler turns into a jump. Either way the checks grow
  linearly with the number of optional fields. The outputs do not when
  optional fields nest: that function takes every output under it, so `n`
  optional fields nested inside each other that hand out every output give
  code quadratic in `n`, about 3,500 lines at 32 and 12,000 at 64. Selecting
  outputs with `~fields` shrinks it, down to the checks alone.
- **Selecting outputs keeps every check.** A field is read when an output
  needs it or a check, a size or a condition reads it; every check runs
  either way.

The differential lanes compare every generated module with the codec on
random bytes: the record modules on the sampled fixed records, the scanners
on every sampled shape and on hand-written structured cases, and, with
`BUILD_EVERPARSE=1`, those cases with the EverParse C validator for
acceptance and the bytes it consumes. On OxCaml the scanners hold integers as
`int64_u`; the paranoid profile checks each of their loads too, on the full
64-bit index.
