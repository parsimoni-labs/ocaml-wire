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
   where `min_size` is the sum of the bytes each field always holds. A
   fixed-size field counts for its size and a sub-codec for its own
   `min_size`; an optional field and a span sized by the input count for
   nothing. A variable-size record then reads its length fields, with
   checked reads, and
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
  overflow (`Codec.add_min_size`). A codec whose fields, sub-codecs
  included, always hold more bytes than a buffer can is refused when it is
  compiled. A negative or oversized literal size is
  refused when its type is built (`Types.literal_size`).
- **Bytes never choose an unchecked offset.** An offset that depends on the
  data (after a variable-size field, or set by a parameter) is always read
  with a check.
- **Offsets do not wrap.** The start of a field after a variable-size one
  and the end of the record are sums of sizes read from the input. They are
  added with saturating arithmetic (`Codec_repr.offset_add`), so a sum past
  `max_int` is `max_int`, past the end of any buffer, and the frame check
  rejects it. A span is tested as `first > len - size`, which cannot
  overflow.
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
their consumer by name, with no closure call. They allocate nothing per
frame where the compiler inlines the consumer and the runtime's loads.

On OxCaml their loop over frames is unrolled four times, then walks the
remaining frames one by one. OCaml polls for signals and other domains once
per loop iteration, so the unrolled loop polls once per four frames. An
iteration computes where its four frames start and loads, for each, the
fields in its leading bytes that choose a nested optional field (the IPv4
protocol of a packet stack), before the first frame's checks: on input whose
kind varies from frame to frame, the branch on that field mispredicts often,
and the next frame's choice then waits for no load. On
OCaml the loop walks one frame per iteration, because four inlined walks
cost the stock compiler more in spilled registers than the polls they save.
The loop over frames that hold the leading prefixes comes last in the
function, where the compiler closes it with one branch. On OCaml the loop
over frames too short for it is a function of its own, called once per
batch, so that its variables take no register from the main loop.

The invariants above hold for the generated code as follows.

- **Unchecked reads stay in the frame.** A prefix is read only after one
  check that the bytes it spans, and those of the prefixes and where clauses
  right after it, lie in the current window: the record's frame, or the
  window a region gives its contents, whose size is itself checked to lie in
  the enclosing one. A span's size is checked against the window too, and
  checked not to be negative unless a range or a passed check proves it.
  When every case of a dispatch starts with the same span, its size is
  checked once before the dispatch. The check is remembered with its window,
  so a region, which opens a new window, checks its own spans.
  A span's check is dropped when the next check, which runs before any read,
  covers the span's bytes too. This needs ranges to prove the span's size not
  negative and its end inside the native int. A bound is compared with the
  terms shared by its position and its size cancelled, so the IPv4 payload
  check is `total_length <= stop - 14`.
  Positions are relative to the window, so adding the window's offset to
  reach a byte cannot overflow, and a short frame in `fold_frames` cannot
  borrow bytes from the next. The generator adds the codec's constant
  offsets and sizes with saturating arithmetic, capped at
  `Sys.max_string_length + 1`. No buffer is that long, so a prefix that ends
  there fails its bound check, and no sum wraps.
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
  function the compiler turns into a jump.
- **Outputs are bound once.** An output under at most four optional fields
  nested inside each other (`Plan.slot_depth`) is immutable: bound where its
  field is read, to its absent value where the field is absent, and passed
  to the function after the field. Realistic codecs nest no deeper, and
  OCaml without flambda specialises each path over such bindings. A deeper
  output is a mutable local: bound once to its absent value where the walk
  starts, in each frame for `fold_frames`, and assigned where its field is
  present, so an absent field writes nothing for it and the function after
  one does not take it. An output is thus written by at most four optional
  fields, and the code grows linearly with their number, nested or not,
  whatever outputs it hands out: `n` of them nested inside each other that
  hand out every output give about 1,400 lines at 32 and 2,700 at 64.
- **Mutable locals are never allocated.** The generated code only reads and
  assigns such a local in place, in the walk or in one of its local
  functions, and only applies those in tail position. OCaml without flambda
  then makes the `ref` a mutable variable, in a register or a stack slot,
  and the function a jump. On OxCaml an `int64_u` has no `ref` and a `let
  mutable` cannot be read in a local function, so the local is a one-field
  record of `Wire_runtime.Target`, which flambda2 unboxes; `[@zero_alloc]`
  checks it.
- **Selecting outputs keeps every check.** A field is read when an output
  needs it or a check, a size or a condition reads it; every check runs
  either way. A scanner names no failed field, so it merges an equality on
  the top bits of a word and a lower bound on the bits right below into one
  range test of the word: the IPv4 version and header length are one byte in
  `[0x45, 0x4f]`.

On OCaml, `fold_step` and `fold_frames_step` hold each field value as an
`int64` and each position as an `int`. The other functions hold both as
`int`, and on OxCaml every function holds both as `int64_u`.

The stock compiler tags an `int`, so each field taken out of a word and each
operation on it costs an instruction or two for the tag. An `int64` that is
bound with `let` and passed only to primitives and inlined functions stays
in a register, neither boxed nor tagged. The consumer's `valid` takes its
integer outputs as `int64` on both targets, so one consumer compiles for
either. They are unboxed where the compiler inlines `valid`, and boxed, so
allocated, where it calls `valid` out of line. A word the runtime loads is unboxed where the load is inlined.
Dune's dev profile compiles `wire.runtime` with `-opaque`, which prevents
that, so each word allocates there.

A position stays an `int` because a load takes one. A size stays the `int64`
it is computed as, and so does a window whose length is a size. Only the end
of a span narrows it to an `int`. The `int64` holds the codec's 63-bit value
sign-extended, with the arithmetic of the `int64_u` on OxCaml, so the step
functions accept, reject and hand out what `fold` does.

Where ints have 32 bits or fewer (js_of_ocaml, wasm_of_ocaml), an `int64` is
a heap value. The step functions allocate at each operation there and run
slower than `fold`, with the same answers, which the node tests check.

The step functions on OCaml also share one word load between prefixes. A
64-bit word loaded at a constant offset of the window serves every later
prefix whose fields it holds, such as the Ethernet type and the start of the
IPv4 header. A word is loaded only inside the bytes the last bound check
covered, so the check that covers the fields covers the word too.

A region that starts at one term plus a constant, such as the TCP header at
`ihl * 4 - 20` bytes from 34, takes the term as its base and the constant in
each of its loads, so the term reaches a load by one addition. A load that
reads a byte through a wider one never starts before the region's first byte.

`Wire_rust.scanner` writes the same plan out as Rust, from `Wire_ocaml.Plan`,
which hands it the same decisions. A window is a slice of exactly its bytes:
a prefix is checked by `split_first_chunk`, whose array its loads index at
constant offsets, and a size is compared as `u64` with the slice's length,
which also rejects a negative one. The slice's length is the room left, so
a bound needs no cancelling and every span keeps its check. Two bitfields of
one word are one range test where `Plan.word_range` finds one. An operation
the plan checks returns
`None` where the 63-bit int overflows, which rejects the record. A run of
optional fields binds the tail it leaves and its immutable outputs in one
`let`, so the code after it is written once; an output under more than
`Plan.slot_depth` optional fields is a `let mut` bound to its absent value
where `scan` starts and assigned where its field is present, so the code is
linear in the number of optional fields, as the OCaml is.

The differential lanes compare every generated module with the codec on
random bytes: the record modules on the sampled fixed records, the scanners
on every sampled shape and on hand-written structured cases, and, with
`BUILD_EVERPARSE=1`, those cases with the EverParse C validators of their
standalone and FFI projections for acceptance and the bytes consumed, and
with every scalar member of the FFI projection's default plug, nested fields
and presence flags included ([the FFI plug](projection.md#the-ffi-plug)), an
absent field as zero. The sampled shapes are compared with those validators
too, on acceptance, the bytes consumed and their top-level integer members.
On OxCaml the scanners hold integers as `int64_u`; the
paranoid profile checks each of their loads too, on the full 64-bit index.
