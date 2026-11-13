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
   read with no bounds check; any other field is read with one.

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
