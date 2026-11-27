# The end-state API

`wire` describes a binary format once, as an OCaml value, and derives from
that value everything that reads and writes the bytes: a decoder and an
encoder, in-place views of untrusted buffers, folds over many records,
generated OCaml, OxCaml and Rust, and a verified C validator.

The model and the surface live with the code: the running example is
[doc/api.mld](../doc/api.mld), the interface of codecs, values, views and
folds is [lib/wire.mli](../lib/wire.mli), and the signature every generated
module meets is in [lib/ocaml/wire_ocaml.mli](../lib/ocaml/wire_ocaml.mli)
(`RECORD`, `S`, `FIXED`, `VARIABLE`, `FOLD`). This document keeps what the
implementation lanes still need: the surface of the libraries those files do
not cover yet, what goes, the open questions and the decisions the API rests
on. Paths and line numbers are those of `ed6be0fc`.

## The surface not yet in an interface

### `wire.runtime`

```ocaml
module Buf : sig
  type 'b t = private bytes                          (** A buffer named ['b]. *)
  type any = Any : 'b t -> any [@@unboxed]
  val v : bytes -> any                               (** [v buf] names [buf]. *)
  val pp : Format.formatter -> 'b t -> unit
end
module Error : sig (* lib/runtime/error.mli, unchanged *) end
module UInt8, UInt16, UInt32, SInt8, SInt16, SInt32 (* v, v_opt, to_int, zero,
  min_int, max_int, equal, compare, pp; lib/runtime/uInt16.mli:13-41 *)
module Vec (* the SIMD lanes the [~simd] modules use *)
```

### The generators

```ocaml
(* Wire_rust *)
type t
val v : ?name:string -> ?fold:string list -> 'r Wire.Codec.t -> t
val name : t -> string
val to_rs : t -> string
val main : t list -> unit
```

The Rust module has the names of the OCaml one on a view that borrows its
buffer:

```rust
let c = space_packet::check(buf, off);
if c.is_ok() { let v = c.get(); route(v.apid()); }
let r = space_packet::fold(buf, [0u64; 4], |mut n, apid| { n[route(apid)] += 1; n });
```

### Verified backends (f)

```ocaml
(* Wire_3d *)
type packed
val pack : 'a Wire.Codec.t -> packed
val run : ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
val main : ?name:string -> mode:[ `Ffi | `Standalone ] -> package:string -> packed list -> unit
val parse_3d : ?batch:bool -> outdir:string -> string -> (unit, string) result
val locate_3d_exe : unit -> string option

(* Wire_stubs *)
val generate : schema_dir:string -> outdir:string -> Wire_3d.packed list -> unit
val build_codec_archive : ?cflags:string list -> schema_dir:string -> archive:string -> unit -> unit
```

## What goes

`lib/wire.mli` at `ed6be0fc` has 360 declarations (`val`, `type`, `module`,
`module type`, read outside comments, the four local `:=` substitutions not
counted). 171 go or become internal and 189 stay, 7 of them renamed.

| Group | Declarations (wire.mli line) | Count |
|---|---|---:|
| In-place access besides `View` | `Codec.get` 1310, `set` 1545, `writer` 1557, `args` 1141, `field_readers` 1604; `Codec.Word` 1652-1705 (the module and its 11 members) | 17 |
| `View`'s codec names and second field kinds | `codec` (type) 1391, `CODEC` and its 3 members 1395-1402, `codec` 1406, `checked_field` 1478, `sub_option` 1523: a view is indexed by its record type, and `View.field` and `View.layer` cover every field | 8 |
| Typ-level I/O | `of_reader` 1049, `of_string` 1072, `of_bytes` 1080, `to_writer` 1097, `to_bytes` 1104, `to_string` 1107: a codec is the unit | 6 |
| Struct validators | `validator` 1716, `validator_of_struct` 1719, `validate_struct` 1722, `struct_size_of` 1726, `struct_min_size` 1729, `wire_size_info_of_validator` 1732 | 6 |
| EverParse plumbing | `Everparse`'s `struct_`, `decl`, `decl_case`, `module_`, `validate_output_name`, `uses_wire_ctx`, `visitor_field`, `visitor_member`, `visitor_fields`, `visitor_setters`, `entrypoint_struct`, `extern_fn_names`, `field_action_form`, `equal_field_action_form`, `field_action_forms` (1769-1856); `Everparse.Raw` and its 47 members (1877-2053) | 63 |
| `Ascii` of a struct | `of_struct` 2064, `pp_struct` 2070 | 2 |
| `Private` | the module and its 66 members (2083-2362) | 67 |
| Packed fields | `Field.packed` 431, `Field.decl_of_packed` 612 | 2 |

Renamed: `View.ok` (1443) to `is_ok`; `Codec.size` (1203) and `size` (1019)
to `fixed_size`; `Codec.encoded_size` (1218) to `size`; `View.sub` (type
1505 and value 1509) to `layer`, and `View.enter` (1530) to `sub`.
`View.check` (1431) takes `~off` and no `'b Buf.length`; `View.field`
(1464) and `View.layer` take the codec itself; `View.read` (1491) and
`View.sub` take the buffer, the view and the copy. The fold's `Codec.fields`
gains the parameter of its result.

Beyond `wire.mli`:

- `Wire_ocaml` (lib/ocaml/wire_ocaml.mli): 59 of 68 go. `fold` (type and
  value, 99-102) and `?folds` give way to `?fold:string list`; `outputs` 213,
  `view`/`view_member` 257-262, `views` 275, `fstar` 279 and `Plan` with its
  51 members (321-687) become internal. `to_ml`/`to_mli` lose `?twin` and
  `?visitors`, `main` its `--visitor`: the interpreted twin (`<name>_codec`),
  the visitor module and the per-fold functions `n`, `n_frames`, `n_index` go.
- `Wire_rust` (lib/rust/wire_rust.mli): `fold` (type and value, 87-90) go,
  `?fold` replaces `?folds`, `to_rs` loses `?visitors`.
- `Wire_3d` (lib/3d/wire_3d.mli): 30 of 36 become internal, `pool` and
  `max_jobs` among them; `packed`, `pack`, `run`, `main`, `parse_3d` and
  `locate_3d_exe` stay.
- `Wire_stubs` (lib/stubs/wire_stubs.mli): 10 of 12 become internal;
  `packed_codec` gives way to `Wire_3d.packed`.
- `wire.runtime`: `Buf.length` (type and value) and `Buf.local`
  (lib/runtime/buf.std.mli:12-24) go; `Unchecked` (16), `Errors` (19),
  `Arith` (10), `Target` and each carrier's `le`, `be`, `set_*`, `unsafe_*`
  and `int_eq` are hidden, since only generated code calls them.

## Open questions

Each with the measurement that settles it. The counts run as
`bench/count.sh` does: `make bench-flow`, then
`sh bench/count.sh 1000000 view _build/default/bench/rust/bench.exe flow _build/bench-flow/mawi.pcap RUNS "<row>"`,
and `make bench-apps-count` for the routing, gateway and CLCW rows, on OCaml
5.4.1 and OxCaml.

1. **Whether a view could carry its buffer**, in both forms, so that
   `Space_packet.apid v` and `View.read v apid_f` drop the `b`. Such a view is
   a block, allocated per record unless the compiler unboxes it, where
   `check`, `is_ok` and `get` now allocate nothing. Settled by the row
   `wire.ocaml check/view, accessors, parse only` and the `wire.ocaml` rows
   of `bench-apps-count`, on a branch whose views carry their buffer: the
   buffer stays an argument unless that branch counts within noise on OCaml
   5.4.1 and OxCaml.
2. **Whether binding a field before `Codec.v`** (`f $ (fun p -> p.apid)`)
   can be lighter. Not a cost: the binding runs once. Settled by writing
   examples/space/space.ml and examples/net in the lighter form, with `make
   test` and the 3D projections unchanged.
3. **Whether `check` takes `~len` for a codec of variable size.** The ruling
   on the window of a check stands (below). Once `Buf.length` goes, `check`
   reads the buffer's length itself, where a loop now reads it once; recount
   the rows of 1 with `check` reading it, and a cost beyond noise brings the
   length back as an argument.
4. **Positional visitor fields swap silently** when two of them share a type:
   `~f:(fun acc seq_count apid -> ...)` over `[ apid; seq_count ]` type-checks.
   Labelled arguments would catch it but cannot be shared with the
   interpreted fold, whose field list is a value. Settled by counting the
   folds of examples/ and bench/ whose fields share a type, and the rows of 1
   for a generated fold taking labelled arguments.
5. **`View.get` against `View.read`.** Ruled: the proposal stands.
   `View.get` turns a check into a view and only that, as `Option.get`; a
   field is read by `View.read b v field` (the tip's name, wire.mli:1491).
   Using `get` for both, as the decision of 2026-11-27 wrote `View.get v
   apid`, would clash.
6. **Nested fields** (proposals, from the shape shown with the nested
   example):
   - A field under an absent optional layer reaches `f` as `None`. On stock
     OCaml each present value allocates a `Some` per record; the
     alternative, today's generated form, hands `f` a presence `bool` beside
     a zero value (`<path>_present`, wire_ocaml.mli:213-218). Settled by the
     MAWI rows of `bench/count.sh` for a fold over `ipv4 / protocol` and
     `ipv4 / tcp / dst_port` both ways.
   - The typed path combinator `Field.( / )`, and the type `Codec.layer` of
     a field that holds a record, so that a path through a field that holds
     none is a type error, and an optional layer under another gives one
     `option`, not two. Settled by its typing in `wire.mli` and a cram test
     of the rejected path. The typing in lib/wire.mli puts the combinators in
     `Codec`, since `Field` comes before `Codec` and cannot name a bound
     field, and splits them as fpath does: `l / f` is a field under a layer
     and `l // l'` a layer under a layer. One left-associative `/` cannot
     return a layer or a field depending on its right operand, so the path
     `ipv4 / tcp / dst_port` is written `ipv4 // tcp / dst_port`. A layer is
     made from its bound field by `Codec.layer` or, for an optional one,
     `Codec.opt_layer`.
7. **A copy from another codec with the same record type.** Ruled: option
   (a). Interpreted reads are loads bounds-checked against the buffer, so a
   misuse is memory-safe and answers a wrong value; both forms keep one API
   without naming codecs. (b) narrows views on 31-bit targets, and (c) puts
   back the naming step the model removes. The misuse is documented in
   `View.field`.

   A copy and a view are both indexed by the record type, so when two codecs
   share one, a copy from one (`View.field c1 apid`) given with a view of the
   other passes the types. The three options were:
   - (a) Interpreted reads are loads bounds-checked against the buffer. The
     mismatch is then memory-safe, but reads the copy's place in the other
     record and answers a wrong value on misuse. Cost: one bound test per
     interpreted read; the generated form is unaffected.
   - (b) The codec's id packed into the view beside its offset, and one
     compare per read raising `Invalid_argument`. Cost: offset bits, which on
     31-bit ints (js_of_ocaml, wasm_of_ocaml) narrow the buffers a view can
     address, against principle 5.
   - (c) A type-level brand per codec, `('c, 'r) Codec.t`, which a
     generative functor or an extra type parameter on `Codec.t` would give
     (the tip names a codec with a first-class module for this,
     wire.mli:1391-1423). Cost: every codec is named before its views are
     used, the step the model removes.

   How often it arises, at `ed6be0fc`, read from the 279 `.cmt` files of
   `dune build` (821 applications of `Codec.v`): 488 of them fall in 65
   groups of two or more codecs with one record type, record types with a
   type variable left out. All are in test/ and fuzz/, or share a tuple type
   with a test from another program (scripts/fstar-gen/proved.ml,
   bench/rust/headers.ml:8); no file of examples/, bench/, lib/ or scripts/
   holds two codecs of one record type, and the 34 codecs of examples/ have
   34 record types. `Codec.rename` has 7 callers (fuzz/gen/api_checks.ml:391,
   fuzz/gen/curated.ml:509, test/diff/gen_diff.ml:313 and 358,
   test/test_codec.ml:1104, 1120 and 1132); a renamed codec keeps the id and
   the layout of its source (lib/dsl.ml:341), so its copies and views agree.
   The counts say how rarely the case arises outside the tests.

## Decisions it rests on

**No backward compatibility** (Thomas, 2026-11-26): "don't worry about
backward compat. Just design a safe and efficient and clean and beautiful
API."

**One generated module per codec** (Thomas, 2026-11-25): "No record/scanner
split: one generated module per codec." "No generic `enter` verb: a field's
accessor is named after the field; a scalar reads its value, a sub-record its
view, an optional sub-record a check. A sub-record's module is declared once
per codec."

**Names** (Thomas): "`View` (checked record), `Visitor` (the module receiving
fields, serde's term [...]). Don't invent new names." Since 2026-11-27 the
visitor is the fold's function `~f`, and every new name has a precedent
(below).

**Shapes** (orchestrator, 2026-11-25): "Every shape gets decode, encode, size
and set_<field> on a view; labelled write only where every field has a
constant size."

**The window of a check** (orchestrator, measured): "A codec of constant size:
`check b ~off` [...]; variable size: `check b ~off ~len`. Reason: the uniform
`~len` cost 5-6 instr per check with the natural window [...] (OxCaml apps
clcw 41.7 -> 47.9, gateway 627 -> 735 per frame)."

**Deleted beyond v1** (agreed): "`Buf.length`, `Buf.local` hidden"; "DSL
unchanged".

**The project's rules**, CLAUDE.md: Feature shippability, Untrusted input
("Bad input makes a decoder return `Error`, never raise
`Invalid_argument`"), Scalar type names, Tagged unions and the Benchmarking
principles ("Every optimisation lives in the code generator or the runtime,
never in the harness"). docs/execution.md, Invariants and Generated code
("OCaml without flambda never inlines a function passed as an argument",
execution.md:159-161).

**The model** (Thomas, 2026-11-27): the running example, the concepts in the
order of doc/api.mld, and the two forms. "Interpreted" and "generated" name an
interpreter against a code generator, as in Carette, Kiselyov and Shan,
"Finally Tagless, Partially Evaluated", MetaOCaml and the Futamura
projections; never "twin", "dynamic" or "compiled", since ocamlopt compiles
both. What goes is decision 5 of that day: `Codec.get`/`set`/`writer`,
`Codec.Word` and `field_readers`; the EverParse plumbing, `Everparse.Raw`,
`Private` and the struct validators; `Wire_ocaml`'s `outputs`, `views`,
`Plan` and `fstar`; `Buf.length`/`Buf.local` and the runtime's helpers; the
typ-level I/O. `Codec.View` stays as the one interpreted in-place API.

**Names and their precedents** (Thomas, 2026-11-27; cited from memory, not
checked against the sources): `is_ok` as `Result.is_ok`; `get` as
`Option.get`, raising on the empty case; view as C++ `std::string_view`;
visitor as serde's `Visitor`; fold as `List.fold_left`, labelled `~init ~f`
as `ListLabels.fold_left` and Base's `List.fold`; snake_case accessors as
prost spells protobuf fields; `set_<field>` as protobuf's C++ `set_foo()`;
`check` and `validate` as EverParse's `<Base>Check<T>` wrapper (a yes or no,
lib/3d/check_wrapper.mli:1-6) and `<Mod>Validate<T>` validator; `sub` as
`Bytes.sub`; `size` the encoded size of a value and `fixed_size` the constant
size.

**The fold takes `~f` everywhere** (Thomas, 2026-11-27: "The alternative is
~f everywhere. -> yes"), and is called `fold` in both forms ("Space_packet.route
-> why `route` and not `fold` ?"). A generated module has at most one fold,
its fields declared in the generator. Measured on `bench/count.sh`, MAWI, 1M
packets, instructions per packet (branches `lane/fold-f`, `f01541ec`, and
`lane/fused-f`, `fb57955e`):

| Row | OCaml 5.4.1 classify | OCaml 5.4.1 parse | OxCaml classify | OxCaml parse |
|---|---:|---:|---:|---:|
| fused fold, visitor module named in the dune rule | 240 | 164 | 203 | 77 |
| fused fold taking `~f` (closure, fields positional): **chosen** | 503 | 409 | 205 | 88 |
| `fold ~f` where `f` gets a view and re-reads its fields | 359 | 261 | 258 | 172 |
| hand-written check/view loop | 326 | 238 | 259 | 174 |

On OxCaml the closure is inlined, and the fold costs what the fused fold with
a named visitor costs (classify 205 against 203, parse 88 against 77). On
stock OCaml 5.4.1 `f` is never inlined: each record is an indirect call
(`caml_apply29`, 22 arguments through the domain state), and the fold takes
about twice the instructions (classify 503 against 240, parse 409 against
164). This trades stock OCaml's fold speed for one API across both forms and
both compilers, and relaxes principle 3 for stock-OCaml folds. A visitor
given a view loses fusion on both compilers, so `f` gets the fields.

**The principles** the API is checked against:

1. **Bad input is a value**: a check, a decoder or an accessor answers a
   refused check or an `Error.t` for any bytes and never raises;
   `Invalid_argument` is misuse. Check: `make test`, `fuzz/` and the
   differential tests against the EverParse validators.
2. **An unchecked read is proved in bounds**: only a view, which only a check
   makes, reads without a bounds check, and only inside its record. Check:
   the `paranoid` profile and `verify/`.
3. **The obvious loop is the fast loop**: `check`, `is_ok`, `get` and the
   accessors, or a fold, are what a user writes and what the benchmarks time,
   on OCaml, flambda and OxCaml. On stock OCaml, a fold calls its `~f`
   indirectly per record and costs about twice a fused fold with a named
   visitor (the table above); that cost is accepted for one fold API. Check:
   `make bench-apps-count` and `bench/count.sh` on the MAWI rows.
4. **One way to do each thing**: one check, one accessor and one setter per
   field, one record write, one fold, one name per concept. Check: § What
   goes leaves no second path.
5. **Names state wire widths, and nothing assumes a 63-bit `int`**: `uint16`
   is two bytes, a carrier holds its field, and a module that needs more than
   31 bits refuses to load where ints are narrower. Check: `make test-wasm`.
