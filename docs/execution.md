# How a codec runs

A codec is compiled on its first use into a plan for one record in a
buffer. Domains that use a new codec at the same time may each compile it:
the compile is pure and one plan is kept. Every operation runs part of that
plan, and the plan reads untrusted bytes, so the rules below say which reads
are checked and why the others cannot go wrong.

## The three phases

`decode c buf off` runs three phases in order, and the other operations run
a prefix of them.

1. **Frame.** Check `0 <= off` and `off + min_size <= input_end`, where
   `min_size` is the sum of the bytes each field always holds and
   `input_end` is the end of the buffer at the top level, and the end of the
   region that holds a nested record. A fixed-size field counts for its size
   and a sub-codec for its own `min_size`; an optional field and a span sized
   by the input count for nothing. After the frame, every byte at a fixed
   offset below `min_size` is in the input. A record that does not fit
   reports what the validator 3D generates reports, which checks each field
   as it reaches it: the first field in order that fails its checks within
   the input, else the end of input. Only then are its checks run again,
   each field read once it fits, and its `where` clause with the last field
   it reads, or before every field when it reads none, so a record that fits
   pays for none of it.
2. **Check.** Evaluate each field in field order: its bounds (a span, a
   region and a sub-codec, each against `input_end`, a sub-codec against its
   own frame first), its constraints, closed enum membership and `map`
   refusals; then the `where` clause. The first failure is the one reported.
   A variable-size record last checks that its end, from its length fields,
   lies inside the input, which only fails for a field the checks do not
   read. A scalar inside the frame is read into its slot only when a
   constraint, an action or the `where` clause uses it, and a comparison of a
   field with a literal or another field is tested directly. On a target
   whose `int` holds 63 bits, the tests of fields that one 8-byte word of the
   frame holds are one test of that word: exact values anywhere in it, and a
   range below them, whose subtraction cannot borrow from them unless it
   fails, so the IPv4 version and header length are one range test, and the
   TCP gate of IPv4 one masked comparison. Any other field runs its general
   validator, so the choice is made field by field.
3. **Read.** Build the value. A field at a fixed offset inside `min_size` is
   read with no bounds check; any other field is read with one. A size that
   reads an output parameter gets the value the check pass left, which is the
   one the check used when every field that assigns an output parameter comes
   before every field that reads one. In any other order the read keeps the
   caller's values and each sub-codec checks itself where it is read.

`validate` runs the frame and the checks. `get` frames one field and reads
it. `encode` writes the fields and, when the codec has constraints, runs the
checks on what it wrote.

`View.check` runs the frame and the checks, as `validate` does, and keeps
the record's offset instead of its error. A field of a view is then read
with no bounds check when it lies at a fixed offset inside `min_size` and
has no action. On a target whose `int` holds 63 bits, an unsigned
big-endian integer or bitfield in a frame of at least 8 bytes is read inline,
one 8-byte load, a shift and a mask; in a frame of 4 to 7 bytes it is read
from a 4-byte word by one call. Any other field is one call, and a field that
`get` reads with a check is read with that check.

A codec of variable size, or one whose checks are more than word tests, is
checked by its walk in check mode: the same plan as the walk that decodes
it, the same frame, tests, spans, gates and regions in field order, with no
value built and no field read but those a check, a size or a gate reads.
`View.enter` then reaches a sub-record of a view without checking it again:
the walk of the record around it finds where it starts, or that its gate
leaves it absent, and `enter` tests only that the sub-record's frame lies in
the buffer, so its fields read in place as those of any view. The walk goes
past a span or a region by its size, with no test of its bounds, which the
check made, and walks only a sub-codec that measures itself. A record whose
bytes changed after its check can move a sub-record, never out of the
buffer: a negative size refuses it, and a sum past `max_int` is `max_int`.

## Invariants

These are what the unchecked reads rely on. Each one is enforced where the
codec is built, so no input can break it.

- **Unchecked reads stay in the frame.** A read without a bounds check is
  created only for a field whose offset and width are constants of the
  schema, with `0 <= offset` and `offset + width <= min_size` at the point
  the field is added (`Codec.field_read`, `Codec.View.field`), and a view
  is made only of a record whose frame lies in the buffer (`Codec.View.check`,
  `Codec.View.enter`).
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
  names the first bad field. The walk `decode` runs on a record without
  parameters reports the same error as the check pass: it checks the frame,
  then the fields in order, then the `where` clause, and leaves a failed
  constraint of a fixed-offset field to the record's check pass, which names
  the first one.
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
the codec's error for a fixed record, and `fold`, `fold_frames` and
`check` (and with `~step`, `fold_step`, `fold_frames_step` and
`fold_index_step`) for a codec of any shape in scope. One writer
(`Writer`) puts them in one module per codec, whatever its shape: the walks,
the check and the views of every codec the walk covers, and the members of a
fixed record of named scalars (its record, decoder, writes, `Get`, `Valid`,
`View`, `Field`, `Global`, `Bigstring`) where the codec is one;
`Wire_ocaml.ml` and `Wire_ocaml.scanner` write the same module, with the
folds their arguments select. OCaml without flambda never inlines a function passed as an
argument, nor a function whose body defines a closure, so `fold_frames`
rejects a frame with no closure of its own, and the step functions call
their consumer by name, with no closure call. They allocate nothing per
frame where the compiler inlines the consumer and the runtime's loads. OCaml
without flambda inlines the consumer's `valid` only when its body defines no
function, a partial application or a primitive used as a value included;
otherwise each call boxes the `int64` values it is passed.

Their loop over frames is unrolled four times, then walks the remaining
frames one by one. OCaml polls for signals and other domains once per loop
iteration, so the unrolled loop polls once per four frames. An iteration
computes where its four frames start and loads, for each, the fields in its
leading bytes that choose a nested optional field (the IPv4 protocol of a
packet stack), before the first frame's checks: on input whose kind varies
from frame to frame, the branch on that field mispredicts often, and the
next frame's choice then waits for no load. The loop over frames that hold
the leading prefixes comes last in the function, where the compiler closes
it with one branch. On OCaml the loop over frames too short for it is a
function of its own, called once per batch, so that its variables take no
register from the main loop.

`fold_index_step` cannot compute where a frame starts: each start is a load
from the index, and only the frame's length says which of its bytes may be
read. Its loop is pipelined one frame deep instead, two frames an iteration.
Before it walks a frame, it loads the next frame's offset and length, tests
that the next frame lies in the buffer and holds the bytes the frame loop
above knows, and if so loads the fields in those bytes that choose an
optional field: those the frame loop reads ahead, and the fields of the
leading prefixes too, the Ethernet type and the IPv4 protocol of a packet
stack, of which OCaml reads only the fields of one byte ahead. A frame
so tested is walked as the frame loop walks one, knowing those bytes, with
those fields as variables; any other goes through the walk of one frame. On
a trace whose kind of packet changes from frame to frame, a mispredicted
branch of one frame then leaves the next frame's choices waiting for no load
of the index or of the frame.

A scanner also writes the views of its codec, beside the folds. `check b
length ~off ~len` is the walk of `fold` with no output: it answers `off` for
a record it accepts and a negative `int` otherwise, so it accepts exactly
what `fold` accepts, and `Codec.View.check` on the frame's bytes; `ok` tests
the answer and `get` makes it a `'b view`, an `int` only the accessors of the
buffer named `'b` take. Each named integer field is an accessor named after
it, each optional field that holds no record a presence `<name>_present`,
and each field that holds a record an accessor that answers a check of that
record's module, one module per codec, named after it (`CapturedIPv4` is
`Captured_ipv4`, written once for the IPv4 and IPv6 fields that hold
`StackTCP`). Each record is planned on its own (`Planner.plan_apart`), each
record a field holds kept as a span of a variable for its size, so an
accessor walks its record's own nodes only. A field at a constant offset
inside the bytes a view of its record is known to hold, the leading prefix of
its plan, is one load from the view. Anything else is placed again: the
accessor reads the fields that place it, tests the conditions of the
optional fields around it, and checks each span not to be negative and each
read to lie in the buffer, but makes none of the record's own checks, which
the check that gave the view made. Bytes written after the check are input
the accessors read again: a sub-record they move is refused, an optional
field they turn off is absent (0), and a field they move out of the buffer
raises `Parse_error`, as `Codec.View.read` does; no read leaves the buffer.
The conditions read their bitfields through the bitfield's word, so that
tests of bits of one word are one masked test, as in the check; fields that
share a 64-bit word at a constant offset are read from one load of it, and
an unsigned 32-bit field is the top half of the word that starts on it when
that word is known to be there. A span followed by a read tests its size
against the room the read needs, so one test covers both. On OxCaml the
values the tests read are `int64_u`, as in the check, and the value an
accessor returns is read as an `int`. A record placed after a record of
variable size is placed by that record's `end_at`, the walk of its spans to
its end. The views of a run of optional fields, or of optional fields nested
inside each other, cost about 55 lines and 370 words of code per field, on
either target: a module per level, each accessor walking only its record.
An accessor returns the field's value as the codec decodes it: a 16-bit
integer is a `UInt16.t`, typed at the accessor's end from the `int` it reads
by `cast` and the carrier's `int_eq`, which the compilers inline to nothing,
a 32-bit one a `UInt32.t` through a proof the module binds once where ints
have more than 32 bits, a bitfield an `int`; a named field of bytes is a
slice of the buffer or a copy. An integer or a bitfield in the leading prefix
also has `set_<field>`, one store, or a load and a store of a bitfield's
word, with no check of the record's constraints, as `Codec.set` makes none.

A codec the walk covers whose record is not one of named scalars at
constant offsets has its values written beside the views (`Value_module`):
each record its module holds has a record type and a printer, `of_view`,
which reads each field through its accessor, and `encode_at ~write`, which
walks the plan from the values, writing them with `write`, and refuses a
record whose values break a size, a presence or a check; `size` is the same
walk with no write. The codec's own adds `decode`, `validate` and `encode`.
Each optional field of a record is a function of its own in `encode_at` and
in the walk that finds the error, named with a prime that no accessor's
name has, so each function stays small. `decode` is the check, then `of_view`; where the check fails,
`error_at` walks the plan as the codec walks the record and answers the
codec's error, field path and offset included: the checks of the fields at
constant offsets of the record decoded (through optional fields and records
held there) come before its fixed bytes, a record entered at a variable
offset checks its bitfields before its fixed bytes, a short field is named
only when it is checked, a short span names the record that holds it, and a
region is measured first. Each record's check pass calls those of the
records it holds, so the code stays linear in them; the views and values of
a level of nested optional fields cost about 133 lines.

The invariants above hold for the generated code as follows.

- **Unchecked reads stay in the frame.** A prefix is read only after one
  check that the bytes it spans, and those of the prefixes and where clauses
  right after it, lie in the current window: the record's frame, or the
  window a region gives its contents, whose size is itself checked to lie in
  the enclosing one. A span's size is checked against the window too, and
  checked not to be negative unless a range or a passed check proves it.
  When every case of a dispatch starts with the same span, its size is
  checked once before the dispatch. The check is remembered with its window,
  so a region, which opens a new window, checks its own spans. The window
  holds at least the bytes the range of its size proves, so a region of
  constant size checks no bound for the prefixes it holds, nor, when it is
  exact and they fill that size, where they end.
  A span's check, or that of a region whose contents load no byte, is dropped
  when the next check, which runs before any read, covers its bytes too. This
  needs ranges to prove the size not negative and its end inside the native
  int. A bound is compared with the terms shared by its position and its size
  cancelled, so the IPv4 payload check is `total_length <= stop - 14`.
  Positions are relative to the window, so adding the window's offset to
  reach a byte cannot overflow, and a short frame in `fold_frames` cannot
  borrow bytes from the next. The generator adds the codec's constant
  offsets and sizes with saturating arithmetic, capped at
  `Sys.max_string_length + 1`. No buffer is that long, so a prefix that ends
  there fails its bound check, and no sum wraps. The entry of `fold` and
  `decode` checks `off` and `len` against the buffer. With `len` not
  negative, `len > Bytes.length buf - off` rejects an `off` past the buffer,
  so no entry tests `off > Bytes.length buf`. Where the walk starts with the
  bound check of a prefix, that check rejects a negative `len`, so the entry
  tests `off < 0` and that bound only; the frame loops test `len < 0` too,
  and raise `Invalid_argument` instead of rejecting. `fold_index_step` makes the
  same test for each frame of its index, against the buffer's length read
  once before its loop, and checks once that its two arrays have the same
  length, so that the loop reads both without a bound check. It loads a
  frame's fields ahead only once that test, and a length of at least the
  bytes those fields end in, has passed.
  An accessor of a view reads with no check only the leading prefix of its
  record, which the check, or the accessor that placed the record, tested
  to lie in the buffer; a field it places again is read after a test of
  its position against the buffer's length, and positions stay at or after
  the view, since a span that may be negative is tested.
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

On OCaml, the step functions hold each field value as an
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
random bytes: the fixed records' members on the sampled fixed records, the
walks and views on every sampled shape and on hand-written structured cases,
the values of the modules with no fixed record on the same frames (decode
written back by encode, validate and its error exactly), and, with
`BUILD_EVERPARSE=1`, those cases with the EverParse C validators of their
standalone and FFI projections for acceptance and the bytes consumed, and
with every scalar member of the FFI projection's default visitor, nested fields
and presence flags included ([the FFI visitor](projection.md#the-ffi-visitor)), an
absent field as zero. The sampled shapes are compared with those validators
too, on acceptance, the bytes consumed and their top-level integer members.
On OxCaml the scanners hold integers as `int64_u`; the
paranoid profile checks each of their loads too, on the full 64-bit index.

With `BUILD_EVERPARSE=1` and z3, `verify/` executes the packet-stack
scanner symbolically and has Z3 prove every unchecked read in bounds and,
in `@verify/equivalence`, every answer equal to the EverParse
specification's. It also shows that the scanner makes no check it could do
without: each check is taken out in turn, and the code without it must read
out of bounds or raise on some input (the check is needed for safety), or
answer differently from the code with it (needed for semantics). `fold` and
`frames_record` answer accept or reject; `fold_frames`, given any
arguments, answers whether it raises `Invalid_argument`, which is what its
argument checks decide. Runtest classifies every check of these three on
OCaml, 28 in all, and fails if one is redundant or undecided, or if it
finds none. `@verify/classify` does the same for the step scanner, and on
OxCaml for `fold`, `frames_record` and their step forms; the argument
checks of the OxCaml frame loops, written by the same code as on OCaml, are
not classified there. The classifier does not show that a loop ends, so
`stride <= 0` is needed because a negative stride must raise, not because
a zero one would loop.
