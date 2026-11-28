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
buffer. Rust has no optional arguments, so the fold takes the placement of
the records as a value, `Records::BackToBack`, `Records::Slots(stride)` or
`Records::Frames(offs, lens)`, and `invalid` as an `Option`:

```rust
let c = space_packet::check(buf, off);
if c.is_ok() { let v = c.get(); route(v.apid()); }
let r = space_packet::fold(buf, Records::BackToBack, [0u64; 4], None,
  |mut n, apid| { n[route(apid)] += 1; n });
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
- `wire.runtime`: `Buf.local` (lib/runtime/buf.std.mli:21-22) goes, and
  `Buf.length` stays, which the generated `check` takes (question 3);
  `Unchecked` (16), `Errors` (19),
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

   Counted (lane api-gen, `sh bench/count.sh 1000000 ROW
   _build/default/bench/rust/bench.exe flow _build/bench-flow/mawi.pcap RUNS
   ROW`, MAWI, 1M packets, instructions per packet, two runs each): the
   generated module of `Packet_stack.capture` as written, and the same views
   carrying their buffer, a block `{ b; k }` that `check` and `get` build and
   every accessor and layer takes alone (one-line delegates over the module,
   as a generator writing such views writes them, on a throwaway branch):

   | Row, parse only | OCaml 5.4.1 | OxCaml 5.4.0-ox9 |
   |---|---:|---:|
   | `wire.ocaml check/view, accessors`, buffer an argument | 269.4, 271.1 | 193.0, 191.8 |
   | the same, the view carrying its buffer | 379.0, 377.8 | 204.5, 210.8 |

   A view carrying its buffer costs 40% on OCaml 5.4.1, which allocates the
   block per record, and 7% on OxCaml: outside noise on both, so the buffer
   stays an argument. The `wire.ocaml` rows of `bench-apps-count` were not
   taken: bench/apps links the routing, gateway and CLCW workloads, which
   still call `Codec.set` and `Codec.Word` and build only once the
   interpreted API lands.
2. **Whether binding a field before `Codec.v`** (`f $ (fun p -> p.apid)`)
   can be lighter. Not a cost: the binding runs once. Settled by writing
   examples/space/space.ml and examples/net in the lighter form, with `make
   test` and the 3D projections unchanged.
3. **Whether `check` takes `~len` for a codec of variable size.** The ruling
   on the window of a check stands (below). Once `Buf.length` goes, `check`
   reads the buffer's length itself, where a loop now reads it once; recount
   the rows of 1 with `check` reading it, and a cost beyond noise brings the
   length back as an argument.

   **Measured (2026-11-28).** The rows of 1 on main (`8334c9ec`), built with
   `--profile=bench` twice: as it is, the generated check taking the length
   the loop read once (a), and with the generator patched so that every
   function that took the length reads `Bytes.length` itself, the length
   argument left unread (b: lib/ocaml/view_module.ml, scan_module.ml and
   record_module.ml). The branch of this lane cannot run them: it removes
   `Buf.length` before the generator lane rewrites the modules that take it.
   Counted by `sh bench/count.sh 100000 LABEL bench.exe MODE FILE 100000 RUNS
   wire.ocaml` on the inputs `bench/apps/bench.exe inputs DIR 100000 100000
   100000` writes, and `sh bench/count.sh 1000000 LABEL bench.exe flow
   mawi.pcap RUNS ROW` on the first 1,000,000 MAWI packets. Instructions
   (cycles) per item, OCaml 5.4.1:

   | Row | (a) length an argument | (b) check reads it |
   |---|---:|---:|
   | routing run, `wire.ocaml` | 44.5 (9.6) | 56.4 (11.0) |
   | routing write, `wire.ocaml` | 111.3 (21.3) | 116.0 (18.6) |
   | gateway run, `wire.ocaml` | 751.7 (220.9) | 880.8 (180.1) |
   | gateway write, `wire.ocaml` | 991.2 (168.8) | 983.7 (167.6) |
   | clcw run, `wire.ocaml` | 41.9 (7.7) | 54.1 (13.0) |
   | clcw write, `wire.ocaml` | 74.1 (15.3) | 73.6 (9.8) |
   | `wire.ocaml check/view, accessors` | 325.3 (337.5) | 360.9 (391.5) |
   | `wire.ocaml check/view, accessors, parse only` | 237.5 (70.6) | 271.6 (78.5) |

   On OxCaml 5.4.0-ox9, the same builds:

   | Row | (a) length an argument | (b) check reads it |
   |---|---:|---:|
   | routing run, `wire.ocaml` | 34.9 (8.7) | 33.4 (6.1) |
   | routing write, `wire.ocaml` | 87.0 (36.6) | 80.2 (19.4) |
   | gateway run, `wire.ocaml` | 602.6 (189.9) | 610.2 (201.1) |
   | gateway write, `wire.ocaml` | 597.0 (65.4) | 611.4 (139.7) |
   | clcw run, `wire.ocaml` | 37.3 (4.9) | 40.9 (10.9) |
   | clcw write, `wire.ocaml` | 61.1 (8.4) | 61.9 (18.3) |
   | `wire.ocaml check/view, accessors` | 257.9 (272.3) | 277.6 (303.3) |
   | `wire.ocaml check/view, accessors, parse only` | 173.7 (47.7) | 191.0 (54.9) |

   Each pair gives the same result. On OCaml 5.4.1, reading the length in the
   check costs the reading rows 11 to 29% more instructions (12 per item for
   routing and CLCW, 34 for the flow rows) and the writing rows nothing; on
   OxCaml, 10% for CLCW and the flow rows (4, 17 and 20 instructions), 1% for
   gateway, and nothing for routing. Beyond noise on both compilers,
   which by the rule above brings the length back as an argument of the
   generated check. The decision is the orchestrator's. The interpreted
   `View.check` reads the length itself, with `?len` for a window, as
   lib/wire.mli states; no interpreted row was counted for this question.

   **Measured on the merged tree (lane api-merge, 2026-11-28).** The same rows
   on main (`8334c9ec`) as it is and on `api-2.0`, where every generated
   `check` reads `Bytes.length` itself and no accessor on a view reads it,
   each built with `--profile=bench` (OCaml 5.4.1, and OxCaml 5.4.0-ox9 in a
   build directory of its own), both on the inputs `api-2.0`'s
   `bench/apps/bench.exe inputs DIR 100000 100000 100000` writes and on the
   first 1,000,000 MAWI packets. Counted once each by `sh bench/count.sh` as
   above, while other builds ran on the machine, so the cycles are noisy and
   the instructions are what to read. Main and `api-2.0` differ in more than
   the length: the loops of bench/apps and bench/rust call each module's new
   names. Every row gives the same result on both. Instructions (cycles) per
   item:

   | Row | OCaml 5.4.1, main | OCaml 5.4.1, `api-2.0` | OxCaml, main | OxCaml, `api-2.0` |
   |---|---:|---:|---:|---:|
   | routing run, `wire.ocaml` | 45.3 (10.0) | 58.3 (10.8) | 41.2 (12.0) | 36.7 (8.8) |
   | routing write, `wire.ocaml` | 110.3 (25.0) | 119.6 (24.9) | 82.8 (25.0) | 84.9 (23.4) |
   | gateway run, `wire.ocaml` | 744.2 (187.2) | 936.1 (205.4) | 598.0 (155.8) | 664.3 (201.9) |
   | gateway write, `wire.ocaml` | 979.3 (142.2) | 993.4 (179.8) | 593.0 (120.9) | 598.1 (102.0) |
   | clcw run, `wire.ocaml` | 45.5 (8.1) | 55.7 (12.5) | 41.8 (8.6) | 44.0 (7.4) |
   | clcw write, `wire.ocaml` | 72.6 (14.3) | 71.6 (7.8) | 60.0 (9.0) | 60.5 (10.7) |
   | `wire.ocaml check/view, accessors` | 326.2 (294.0) | 355.6 (326.6) | 259.2 (248.7) | 277.2 (283.9) |
   | `wire.ocaml check/view, accessors, parse only` | 238.8 (67.0) | 270.3 (69.9) | 173.8 (47.7) | 192.5 (55.7) |

   On OCaml 5.4.1 the reading rows take 9 to 29% more instructions on
   `api-2.0` (routing run 29%, gateway run 26%, CLCW run 22%, the flow rows 9
   and 13%) and the writing rows 1 to 8%; on OxCaml gateway run and the flow
   rows take 7 to 11% more, CLCW run 5%, the writing rows 1 to 3%, and routing
   run 11% fewer. This agrees with the patched count above, save gateway run
   on OCaml (936 against 881), and is beyond noise on both compilers. The
   interface is unchanged by this count: the decision is the orchestrator's.

   **Cause (lane perf-q3).** The generated modules and the loops differ only
   in `check`: the accessors, `is_ok` and `get` are the same text on both.
   In the routing loop on arm64 (`otool -tV` of `Loops.routing`, OCaml
   5.4.1), the path of one accepted packet is 56 instructions on main and 66
   on `api-2.0`: 8 for the length, as `Bytes.length` in each `check` takes 9
   that ocamlopt does not hoist out of the loop where main reloaded the
   length the loop read once, and 2 for a test `off < 0` that main left to
   the sign of the answer. The test came back because the generator decided
   whether a walk reads its buffer by looking for `buf` in a header that now
   always names it; it is gone again for a check that reads nothing (space
   packet, TM frame, CLCW, Ethernet), which leaves 64. Only `check` taking
   the length, the column (a) above, removes the other 8.

   **Ruled (orchestrator, 2026-11-28):** the generated `check` takes the
   buffer's length again, `check b length ~off` for a codec of constant size
   and `check b length ~off ~len` for one of variable size, where `length`
   is `Wire.Buf.length b`, of type `'b Buf.length`, which a loop reads once
   per buffer. `'b` names one buffer and bytes never shrink, so a
   `'b Buf.length` cannot be wrong, and taking it is as memory-safe as
   reading `Bytes.length` in each check. The interpreted `View.check` still
   reads the length itself. With it, the routing loop's path of one accepted
   packet is 56 instructions again, the same code as main's but for the
   offset of one global; the gateway and CLCW loops are main's instruction
   for instruction (`otool -tV` of `Loops`, OCaml 5.4.1, arm64). The
   dynamic count of this shape is column (a) of the first table above, taken
   on main; it was not retaken on this tree, since counting needs
   `/usr/bin/time -l` outside the sandbox. The Rust `check` takes a slice,
   whose length is part of the reference, so it is unchanged.

4. **Positional visitor fields swap silently** when two of them share a type:
   `~f:(fun acc seq_count apid -> ...)` over `[ apid; seq_count ]` type-checks.
   Labelled arguments would catch it but cannot be shared with the
   interpreted fold, whose field list is a value. Settled by counting the
   folds of examples/ and bench/ whose fields share a type, and the rows of 1
   for a generated fold taking labelled arguments.

   Counted (lane api-gen): examples/ and bench/ hold three folds. That of
   examples/generated/stack.ml hands an `UInt8.t option` and an
   `UInt16.t option`, no type twice; that of bench/rust's `g_selected` hands
   16 fields and that of `g_flow` 22, each with several fields of one type
   (`UInt16.t option`, `UInt32.t option`). Two of three can swap fields
   silently. On the row `wire.ocaml fold, selected fields, parse only` (the
   fold of `g_flow` over an index, its `f` handing each key to `Flow_sum`),
   counted as for question 1, with `f` taking its fields positionally and,
   on a throwaway branch, labelled by their paths (`~ipv4_protocol:`):

   | `f` takes its fields | OCaml 5.4.1 | OxCaml 5.4.0-ox9 |
   |---|---:|---:|
   | positionally | 397.0, 396.6 | 92.4, 94.0 |
   | labelled | 397.1, 396.6 | 94.0, 93.6 |

   Labels cost nothing on either compiler: they are erased at compile time,
   and on stock OCaml the call through `caml_applyN` is the same. The
   interpreted fold's fields are a value and cannot be labelled.

   **Ruled (orchestrator, 2026-11-28):** the generated fold's `f` takes its
   fields labelled by their paths, the accessor names joined with `_`
   (`~ipv4_protocol`, `~ipv4_tcp_dst_port`), on OCaml and OxCaml, since the
   labels catch the swaps of two of the three folds at no measured cost; two
   paths that give one label are refused by `Wire_ocaml.v`. The interpreted
   fold stays positional, its fields a list. The Rust fold's closure takes
   them positionally, in the order of `?fold`, as Rust has no labels.
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

   - **Measured for the interpreted fold (OCaml 5.4.1, 2026-11-28).**
     `View.fold Packet_stack.codec` over the index (`~offs ~lens`) of the
     first 1,000,000 packets of the MAWI trace, refused frames passed to
     `~invalid`, built with `--profile=bench` and counted by `bench/count.sh
     1000000 ROW q6.exe mawi.pcap RUNS ROW`. `bench/rust/bench.exe` does not
     build on this branch until the generated modules move, so the rows ran
     in a harness of their own over the same capture, with the alternative
     added to a scratch copy of the library: a path read as its value or a
     zero, beside a `bool` field for the layer's presence. Instructions
     (cycles) per packet:

     | Fold over | `option` (chosen) | presence `bool` and zero | zero only |
     |---|---:|---:|---:|
     | `ipv4 / protocol` | 722.3 (178.5) | 799.1 (200.3) | 721.2 (177.2) |
     | `ipv4 // tcp / dst_port` | 763.4 (192.7) | 880.7 (214.1) | |

     On OxCaml 5.4.0-ox9, the same harness:

     | Fold over | `option` (chosen) | presence `bool` and zero | zero only |
     |---|---:|---:|---:|
     | `ipv4 / protocol` | 678.2 (166.7) | 751.9 (180.8) | 678.3 (171.2) |
     | `ipv4 // tcp / dst_port` | 722.3 (170.4) | 834.1 (193.0) | |

     The `Some` costs about one instruction per packet over a zero on OCaml
     5.4.1 and none on OxCaml, and the
     presence form costs more, since its `bool` and its value are two fields
     and each enters the layers. The interpreted fold keeps the `option`.
     The generated form's rows are the generator lane's to count.

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
