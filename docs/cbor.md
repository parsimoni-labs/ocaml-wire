# Deterministic CBOR

**Experimental in 2.0.0.** `Wire.Cbor` ships in 2.0.0 as an experimental
module: its API will certainly break in 2.1.0. The module preamble and the
CHANGES entry say so; no `[@@alert]` marks it.

`wire` describes deterministic CBOR (RFC 8949 section 4.2.1) in the same
description as binary formats and ASN.1 DER. Each part projects to the
verified parser technology that expresses it and is checked there (CLAUDE.md,
Feature shippability): EverParse 3D for binary layouts, everparse-asn1 for DER
(docs/asn1.md), and evercddl, EverParse's CDDL front end, for CBOR. 3D reads a
CBOR head, but not a map whose entries come in any order, nor the bytewise
key order of deterministic encoding; evercddl takes a CDDL description (RFC
8610) and produces C validators, parsers and serialisers verified against a
specification of deterministic CBOR.

EverParse paths are from its root at tag `v2026.11.13` (`3eaa0aa22`), with
`Gen.fst` for `src/cddl/tool/CDDL.Tool.Gen.fst`, `Main.ml` for
`src/cddl/tool/ocaml/evercddl-gen/Main.ml`, `Parser.ml` for
`src/cddl/tool/ocaml/evercddl-lib/CDDLParser.ml`, `spec/` for
`lib/everparse/cddl/spec/`, `cbor/` for `lib/everparse/cbor/`, and
`cose-sign.cddl` for `src/cose/cose-sign.cddl`. `codec.mli` and `binary.ml`
are ocaml-cbor's `lib/`; `cose/` and `suit/` are ocaml-cose's and ocaml-suit's
`lib/`.

## Concepts

- A CBOR type `'a Cbor.t` describes the deterministic CBOR bytes of OCaml values `'a`, once, as a CDDL rule does.
- A map record or an array record is a record: its members are `Field.t` values bound with `Codec.( $ )`, as in `Codec.v`, and `Cbor.codec` gives its `Codec.t`.
- Values: `Codec.decode` and `Codec.encode` copy the whole value; bad bytes are an `Error`, never an exception. A well-formed but non-deterministic encoding is bad bytes.
- Views: `Codec.View.check` runs the CBOR validator once and never raises, then `is_ok` and `get`; a member is read in place with `View.read` of a field made once by `View.field`. Heads move every offset, so nothing is set in place.
- One description, three backends: a CBOR part nests in a binary or DER codec, and a binary part in CBOR, as a byte region the other backend validates. A DER part in CBOR is refused in 2.0 (Backend selection).
- A shape evercddl does not express is refused when it is built, with `Invalid_argument`.

## Running example: COSE_Sign1 (RFC 9052 section 4.2)

```ocaml
open Wire
type label = Int of Cbor.integer | Text of string
type iv = Iv of string | Partial_iv of string
type header = {
  alg : label option;
  crit : label list option;
  content_type : label option;
  kid : string option;
  iv : iv option;
  others : (label * string) list;
}
type sign1 = {
  protected : (header * string) option;
  unprotected : header;
  payload : string option;
  signature : string;
}
let label =
  Cbor.(choice [
    case "int" int ~inject:(fun i -> Int i) ~project:(function Int i -> Some i | _ -> None);
    case "tstr" text ~inject:(fun s -> Text s) ~project:(function Text s -> Some s | _ -> None) ])
let f_alg = Cbor.mem_opt "alg" (`Int 1) label
let f_crit = Cbor.mem_opt "crit" (`Int 2) (Cbor.array ~non_empty:true label)
let f_cty = Cbor.mem_opt "content_type" (`Int 3) label
let f_kid = Cbor.mem_opt "kid" (`Int 4) Cbor.bytes
let f_iv =
  Cbor.(exclusive "iv" [
    member (`Int 5) bytes ~inject:(fun s -> Iv s) ~project:(function Iv s -> Some s | _ -> None);
    member (`Int 6) bytes ~inject:(fun s -> Partial_iv s)
      ~project:(function Partial_iv s -> Some s | _ -> None) ])
let f_others = Cbor.keep_unknown "others" label Cbor.any
let header_map =
  Cbor.record "header_map"
    (fun alg crit content_type kid iv others -> { alg; crit; content_type; kid; iv; others })
    Codec.[ (f_alg $ fun h -> h.alg); (f_crit $ fun h -> h.crit);
            (f_cty $ fun h -> h.content_type); (f_kid $ fun h -> h.kid);
            (f_iv $ fun h -> h.iv); (f_others $ fun h -> h.others) ]
let protected =
  Cbor.(choice [
    case "serialized" (keep_bytes (encoded header_map)) ~inject:Option.some ~project:Fun.id;
    case "empty" (size ~min:0 ~max:0 bytes) ~inject:(fun _ -> None)
      ~project:(function None -> Some "" | Some _ -> None) ])
let f_protected = Cbor.elem "protected" protected
let f_unprotected = Cbor.elem "unprotected" header_map
let f_payload = Cbor.elem "payload" (Cbor.nullable Cbor.bytes)
let f_signature = Cbor.elem "signature" Cbor.bytes
let cose_sign1 =
  Cbor.tuple "COSE_Sign1"
    (fun protected unprotected payload signature -> { protected; unprotected; payload; signature })
    Codec.[ (f_protected $ fun s -> s.protected); (f_unprotected $ fun s -> s.unprotected);
            (f_payload $ fun s -> s.payload); (f_signature $ fun s -> s.signature) ]
let codec = Cbor.codec (Cbor.tag 18 cose_sign1)
```

`Cbor.project "COSE_Sign1_Tagged" codec` prints the module evercddl reads; the
unnamed root takes the module name. It is `cose-sign.cddl:1-52` with
`Generic_Headers` inlined into `header_map`, and the mutual exclusion of keys
5 and 6 written the way `cose-sign.cddl:21-30` writes it, with
`everparse-no-match` (`spec/CDDL.Spec.AST.Driver.fst:86-89`):

```
header_map = {
  ? 1 => int / tstr,
  ? 2 => [+ int / tstr],
  ? 3 => int / tstr,
  ? 4 => bstr,
  ((5 => bstr, ? 6 : everparse-no-match) //
   (6 => bstr, ? 5 : everparse-no-match) //
   (? 5 : everparse-no-match, ? 6 : everparse-no-match)),
  * (int / tstr) => any
}
COSE_Sign1 = [
  protected : bstr .everparse-det-cbor header_map / bstr .size 0,
  unprotected : header_map,
  payload : bstr / nil,
  signature : bstr
]
COSE_Sign1_Tagged = #6.18(COSE_Sign1)
```

The printer names a rule for a tuple, a record and the root, and for no
other type: `label`, a choice, is written out where it is used, in
parentheses as the key of `* k => v`, which the grammar reads as a `type1`
(`Parser.ml:309-327`). `test/cbor/test.ml` pins this text, and evercddl
verifies it (Running everparse-cddl from the prefix).

RFC 9052 writes the protected bucket as `bstr .cbor header_map`; evercddl has
no `.cbor` (`Parser.ml:289-296`, "TODO: (non-injective) cbor") and offers
`.everparse-det-cbor` instead, which `cose-sign.cddl:14-19` justifies by RFC
9052 section 9 requiring deterministic encoding. `Cbor.encoded` projects to it.

ocaml-cose reads the same structure through `Cbor.Value.t` and hand-written
matches (`cose/sign1.mli:177-210`, `cose/header.mli:51-63`); the protected
bytes it signs over are what `keep_bytes` keeps.

## Where the DSL lives

In `wire`, as `Wire.Cbor`, for the reason `Wire.Asn1` is there
(docs/asn1.md, Where the DSL lives): a CBOR value is a field of a binary or
DER record (`Field.v "msg" (nested ~size (codec (Cbor.codec t)))`), and a
binary record is the content of a CBOR byte string (`Cbor.containing`), so the combinators must produce the `typ`, `Field.t` and
`Codec.t` that `Codec.decode`, `Codec.View` and the projections consume, and
`typ` is abstract. `'a Cbor.t` is a type of its own because a record needs the
key of each member, a choice needs the major types and literals of its
alternatives to decide disjointness, and a map key needs a CBOR type, none of
which an arbitrary `typ` carries. `wire.cbor` (`Wire_cbor`, `lib/cbor/`) is to
`everparse-cddl.exe` what `wire.asn1` is to `everparse-asn1.exe`: it runs the
tool, and links `unix`. The dune rules and C harnesses that bind the C come
with the stubs, after 2.0.

Names are ocaml-cbor's where it has one (`codec.mli`: `uint` 39, `int` 54,
`bytes` 66, `text` 84, `array` 89, `assoc` 115, `mem` and `mem_opt` 135-139,
`tag` 219, `null` 236, `bool` 239, `nullable` 259); `record` and `tuple` are
the two record shapes ocaml-cbor names ("Records as text-keyed maps",
`codec.mli:124`; `tuple2` to `tuple5`, `codec.mli:95-105`), taking fields as
`Codec.v` does rather than a curried pipeline, so a view addresses each member
by its field. A CDDL map refuses a key no member matches, where ocaml-cbor
skips it by default (`codec.mli:158`), so the open case is the member that
keeps those entries, jsont's `keep_unknown` (`jsont.mli:1188`, 0.4.0).
`case`, `choice`, `containing` and `keep_bytes` are `Wire.Asn1`'s, `map` is
both libraries'. `encoded` is RFC 8949's name for a byte string holding one
CBOR item (section 3.4.5.1, tag 24), which RFC 8610 section 3.8.4 writes
`.cbor`; `Cbor.cbor` would repeat the module name. The rest are CDDL's: `nint`,
`range` (`min..max`), `size` (`.size`), `literal`, `any`, `elem` (an array
entry). Two departures from ocaml-cbor follow the
scalar rule (CLAUDE.md, Scalar type names): ocaml-cbor's `uint` and `int` are
`int t`, narrower than the 64-bit argument; here `uint` and `nint` are
`UInt64.t t` and `int` is `integer t`, the tag and argument evercddl's C
carries (`src/cose/c/COSE_Format.h:340-349`). `exclusive` has no precedent
(Q5).

## Constructs and their projections

| `Wire.Cbor` | `.cddl` | interpreted OCaml |
|---|---|---|
| `uint`, `nint` | `uint` (`#0`), `nint` (`#1`) | the argument, `UInt64.t`; `nint` stands for `-1 - n` |
| `int` | `int`, `uint / nint` | `Uint n` or `Nint n` |
| `range ~min ~max` | `min..max` (`Parser.ml:282-287`) | `int64` in range |
| `bytes`, `text` | `bstr` (`#2`), `tstr` (`#3`) | octets; text valid UTF-8 |
| `bool`, `null` | `bool`, `nil` | `true`/`false`, `()` |
| `literal v` | the literal `1`, `-1`, `"Signature"` | `()` |
| `any` | `any` (`#`) | the bytes of one data item |
| `size ~min ~max t` | `t .size (min..max)` | length in bytes |
| `tag n t` | `#6.n(t)` | `t`'s value under tag `n` |
| `array ?non_empty t` | `[* t]`, `[+ t]` | list |
| `elem n t`, `elem_opt n t` | `n : t`, `? n : t` in an array | value, option |
| `tuple n make fields` | `n = [ ... ]` | record |
| `assoc k v` | `{ * k => v }` | pairs in deterministic key order |
| `mem n k t`, `mem_opt n k t` | `k => t`, `? k => t` | value, option |
| `exclusive n members` | `(k1 => t1, ? k2 : everparse-no-match) // ... // (? k1 : everparse-no-match, ...)` | at most one member |
| `keep_unknown n k v` | `* k => v` | the entries no other member matches |
| `record n make fields` | `n = { ... }` | record |
| `choice cases` | `t1 / t2 / ...` | the one case that matches |
| `nullable t` | `t / nil` | option |
| `encoded t` | `bstr .everparse-det-cbor t` | content decoded by `t` |
| `containing c` | `bstr` | content decoded by the binary codec `c` |
| `keep_bytes t` | `t` | value and its bytes |
| `map` | the type mapped | converted |

An anonymous type that `.cddl` must name is named by its path, as evercddl
names C types after the rule (`Gen.fst:36-56`: lower-cased, filtered to
`[A-Za-z0-9_]`, `evercddl_` before a reserved name).

A record reads a map as the C evercddl generates from `cose-sign.cddl`
does, which lane C3 measured on that C (the probes of `test/test_cbor.ml`
that "COSE: vectors" replays, each with the C's verdict):

- an entry under the key of a `mem` whose value does not match the member's
  type is not refused there: as for `k => t` in a CDDL group, it falls
  through to `keep_unknown`, which takes it if its key and value match
  `* k => v`, else the map is refused. `{ 4: 7 }` (`kid_int`) and
  `{ 1: h'' }` (`alg_bstr`) are accepted as COSE headers, with no `kid` and
  no `alg`;
- an entry under a key of an `exclusive` must match that member: the
  `? k : everparse-no-match` of the other alternatives leaves it nowhere
  else to go, so `{ 5: 7 }` (`iv_int`) is refused, although
  `keep_unknown` would take `5 => any`.

## Backend selection

A codec built by `Cbor` is a CBOR codec. Each codec projects to its own
backend, and one of one kind nested in another is handed over as a byte
region, as DER is (docs/asn1.md, Backend selection):

- **CBOR in binary.** A field `codec (Cbor.codec t)` of a `Codec.v` record
  sits under `nested ~size:e` (else `Codec.v` raises). `Everparse.project`
  emits it as `UINT8 f[:byte-size e]`, as it does DER (docs/asn1.md, Backend
  selection), so 3D checks the framing, and `Cbor.project` of the binary
  codec is the module of `t`, so evercddl checks the region. A `where true`
  between `nested` and the CBOR codec constrains nothing and projects to
  nothing; any other `where` there is refused when the codec is built. The C
  that joins the two runs `M_validate_and_parse_T(slice)` and requires `Some`
  with an empty remainder, the full-consumption check; it comes with the
  stubs (Q2 of docs/asn1.md decides whether an `:on-success` extern or glue
  after validation carries it, for both formats).
- **CBOR in DER.** `Asn1.containing (Wire.codec (Cbor.codec t))` is an OCTET
  STRING to everparse-asn1. `Cbor.project` of the DER codec is the module of
  the CBOR types under its containings, each named by its path,
  `Cbor.contained` of it the 3D schemas under those, and `Asn1.contained`
  leaves the CBOR codec to them. The glue runs the CBOR validator on the
  content slice.
- **Binary in CBOR.** `Cbor.containing c` projects to `bstr`, since CDDL has
  no extern hook (`Parser.ml:289-296` lists the two controls). The check runs
  `M_validate_and_parse_T`, then the 3D validator of `c` on exactly the
  content of each such byte string, reached through the parsed C struct, and
  accepts iff all accept.
- **DER in CBOR** is refused when built: `Cbor.containing` raises on a DER
  codec. evercddl sees a `bstr`, and `Asn1.project` would have to read a
  CBOR codec, which it cannot: `Cbor` reads DER codecs through `Asn1`, so
  `Asn1` reading `Cbor` would make the two modules depend on each other. A
  CBOR codec under `Cbor.containing` is refused too; `Cbor.encoded` is that
  byte string, which evercddl checks.
- **CBOR in CBOR.** `Cbor.encoded t` projects to `.everparse-det-cbor`, which
  evercddl checks itself (`spec/CDDL.Spec.AST.Elab.fst:1246-1256`).
- **Interpreted.** `Codec.validate` on the outer codec calls the inner codec's
  validator on the region; one answer either way.

x5chain (RFC 9360 section 2, label 33) is the case that crosses all three:
`COSE_X509 = bstr / [ 2*certs: bstr ]`, each `bstr` a DER certificate. It
would be

```ocaml
let cert = Cbor.containing (Wire.codec (Asn1.codec certificate))
let x5chain =
  Cbor.(choice [
    case "one" cert ~inject:(fun c -> [ c ]) ~project:(function [ c ] -> Some c | _ -> None);
    case "chain" (array ~non_empty:true cert) ~inject:Fun.id ~project:Option.some ])
```

so that evercddl checks the CBOR, everparse-asn1 each certificate; in 2.0
`cert` raises, as DER in CBOR does, and a certificate is a `Cbor.bytes` the
DER codec decodes after (Q11). evercddl has no bounded occurrence (`Parser.ml:269-270` accepts `*` only with no bounds), so
`2*` cannot be stated: the array accepts one certificate, which RFC 9360 says
is written as a bare `bstr` (`cose/header.mli:132-134` writes it so). Q4.

`Everparse.project c` raises on a CBOR codec, as on a DER one. `Cbor.project
m c` is the CDDL module of a CBOR codec, or of the CBOR types a binary or DER
codec holds; `Cbor.contained c` is the 3D schemas of the binary codecs under
the containings of those types. The handover glue is unverified C, which the
differential test is to cover.

## Deterministic encoding and who enforces it

evercddl's generated parsers sit on EverCBOR's deterministic API only:
`Gen.fst:157,218` instantiate every validator and parser with
`Det.cbor_det_impl`, `Main.ml:233-238` roots the extraction at
`CBOR.Pulse.API.Det.C`, and
`lib/everparse/cddl/pulse/CDDL.Pulse.AST.Det.C.fst:21` binds
`cbor_det_parse_full` with "TODO: support mixtures where non-deterministic
CBOR objects contain .det-cbor deterministically serialized CBOR objects".
EverCBOR also has a non-deterministic API (`CBORNondet.h` in the prefix,
`cbor/pulse/CBOR.Pulse.API.Nondet.C.fsti`) that evercddl does not use. The
validator accepts exactly the prefixes that are `cbor_det_serialize` of some
value (`cbor/pulse/CBOR.Pulse.API.Base.fst:5363-5370`), and `cbor_det_parse`
is the raw parser followed by the optimal-heads and sorted-keys checks
(`cbor/spec/raw/CBOR.Spec.API.Format.fst:28-32`).

The interpreted codec enforces what EverCBOR does, so `decode` fails exactly
when the C does (docs/execution.md, Invariants, "One answer"). Encoders write
deterministic CBOR only.

| RFC 8949 rule | EverCBOR | ocaml-cbor `Deterministic` |
|---|---|---|
| shortest head argument (4.2.1, 4.1) | `cbor/spec/raw/CBOR.Spec.Raw.Optimal.fst:10-16` | `binary.ml:825-827,853-858,876` |
| definite lengths only (4.2.1) | `cbor/spec/raw/everparse/CBOR.Spec.Raw.EverParse.fst:80-81` | `binary.ml:986-988` |
| keys sorted bytewise by encoding (4.2.1) | `cbor/spec/raw/CBOR.Spec.Raw.Format.fsti:87-89` | `binary.ml:1221-1234` |
| no duplicate key (5.6) | strict order, `CBOR.Spec.Raw.Format.fsti:116-120` | `binary.ml:1233` |
| text is UTF-8 (5.3.1) | `cbor/spec/raw/CBOR.Spec.Raw.Base.fsti:30` | `binary.ml:1006` |
| two-byte simple value at least 32 (3.3) | `cbor/spec/CBOR.Spec.Constants.fst:67-70` | `binary.ml:1124` |
| floats (4.2.1 shortest form) | refused outright, `CBOR.Spec.Raw.EverParse.fst:79` | shortest, `binary.ml:1074-1080` |
| bignum without leading zero (3.4.3) | not checked: tags are opaque (`postlude.cddl:4-9` comments bignums out) | `binary.ml:1310-1314` |

Wire refuses a float anywhere, in `any` too, and does not check bignums, so it
agrees with the C; both differences from ocaml-cbor are classes the oracle
allows. Text literals in a description are ASCII and shorter than 2^16 bytes
(`spec/CDDL.Spec.AST.Base.fst:156`, "FIXME: support utf8").

EverCBOR states its UTF-8 check in `cbor/spec/CBOR.Spec.API.UTF8.fsti`, and
the prefix ships only that interface, so wire checks plain RFC 3629: no
overlong form, no surrogate, nothing above U+10FFFF. The 206 UTF-8 rows of
EverCBOR's vectors agree with it.

Nesting has no bound: EverCBOR's deterministic validator has none and
accepts its vector of 2200 nested arrays, so wire reads, writes and prints
any depth in loops, with memory linear in the input.

`cbor_det_validate` answers the length of the one data item at the start of
its input and looks no further, and `M_validate_and_parse_T` hands back the
rest. Full consumption is checked above them, in two places: the codec
refuses bytes after the item in its region, as the stubs refuse a non-empty
remainder, and `encoded` refuses a byte string whose content is not exactly
one item, as `.everparse-det-cbor` requires an empty remainder
(`lib/everparse/cddl/pulse/CDDL.Pulse.Misc.fst:508`). Wire's deterministic
core reads one item and answers where it ends, as `cbor_det_validate` does.

## The C API the stubs bind

For a rule `T` in a module `M` (`--mname M`), evercddl emits, as
`src/cose/c/COSE_Format.h:1363-1430` shows for `cose_sign1`:

- `bool M_validate_T(cbor_det_t c)`: whether a parsed CBOR item matches `T`;
- `M_T M_parse_T(cbor_det_t c)`: a C struct of slices and tagged unions into the input, copying nothing;
- `size_t M_serialize_T(M_T c, Pulse_Lib_Slice_slice__uint8_t out)`: the bytes written to `out`, `0` when it fails (the generated `COSE_Format_serialize_cose_sign1` ends on `return (size_t)0U`);
- `M_validate_and_parse_T(Pulse_Lib_Slice_slice__uint8_t s)`: an option of the struct and the remaining slice, from bytes.

Under them, EverCBOR's `cbor_det_validate`, `cbor_det_parse`,
`cbor_det_serialize` and the array and map iterators
(`include/everparse/CBORDet.h:104-202` in the prefix); `src/cose/interop/common.c:4-19`
uses `validate_and_parse` on a COSE_Key. The stubs bind `Codec.View.check`
and `Codec.decode` to `M_validate_and_parse_T` with an empty remainder,
`View.read` to the members of `M_T`, and `Codec.encode` to
`M_serialize_T`. A `* k => v` member is an iterator type in `M_T`
(`COSE_Format.h:1199`), which the stubs walk; the C type names are krml's
monomorphisations and change with the grammar, so the stubs name them from
the generated header, never by hand.

## The differential oracle

Three readers of one input: wire's interpreted codec, the evercddl C
(through `Wire_cbor`'s harness), and ocaml-cbor's codec under
`Encoding.Deterministic` (`encoding.mli`), over the same grammar written in
ocaml-cbor's combinators. Wire and the C agree exactly: verdict, consumed
length, value, and the bytes of re-encoding. Wire and ocaml-cbor agree on
every input both accept; a reject only one side makes is allowed in the two
classes the table marks (floats, bignums), each with a vector proving it.

Inputs:

- the COSE_Sign1 messages of ocaml-cose's tests: its cose-wg examples and
  its pycose traces; `test/test_cbor.ml:cose_traces` carries the cose-wg
  example ECDSA-01 and the 14 pycose messages;
- EverParse's COSE interop (`src/cose/interop/test.py`) generates its
  messages with pycose at test time and commits none, so it contributes the
  recipe, not vectors;
- EverCBOR's own vectors: the 363 inputs
  `share/everparse/tests/cbor/cbor_tests.c` writes when built with
  `-DEVERCBOR_DET` against the prefix's `CBORDet.o` (363 of 363 pass), 35
  of them `invalid` and 24 `_nondet`, refused by the deterministic API, and
  206 UTF-8 rows, with verdicts from the C and from Python cbor2
  (`cbor_tests.py:1-20`); `test/cbor_vectors` carries them;
- RFC 8949 Appendix A, as ocaml-cbor's test vectors carry it, copied in
  `test/cbor_vectors/cbor_vectors.ml:appendix_a`: every item is valid
  CBOR, and the deterministic readers must refuse exactly the floats and the
  indefinite lengths among them, 34 of the 81 items. No item of Appendix A
  has a head in a non-preferred form.

## Refused when built

The combinators raise `Invalid_argument`, each refusal unit-tested, on:

- recursion: no `fix`. A rule that names itself never becomes bounded, so
  the topological sort ends on "contains an undefined name"
  (`spec/CDDL.Spec.AST.Driver.fst:48-82`), and a recursive group is refused
  by name (`Gen.fst:760`);
- a `choice` of two cases whose types meet: every type choice is elaborated
  with `guard_choices` true (`spec/CDDL.Spec.AST.Elab.fst:1683`), which
  requires disjoint alternatives (`:1232-1233`,
  `spec/CDDL.Spec.AST.Elab.Disjoint.Type.fst`), and two maps are never proved
  disjoint (`Disjoint.Type.fst:205`, "map: not supported");
- `nullable t` where `t` matches `nil`, for the same reason;
- a `record` whose members' keys meet, or with two `keep_unknown`
  (`spec/CDDL.Spec.AST.Elab.fst:1480-1485` requires disjoint footprints);
- a `tuple` whose entries evercddl cannot prove to split one way: the array
  elaborator requires a unique concatenation
  (`spec/CDDL.Spec.AST.Elab.fst:1372,1413`). `cose-sign.cddl:54-80` shows the
  greedy `?` that makes an `elem_opt` followed by an entry of the same type
  wrong, and its rewrite. Wire refuses an `elem_opt` that can also start the
  entries after it, by evercddl's own test (Q9); and a `tuple` with no entry,
  an empty group evercddl does not elaborate (`:1402`);
- `range` with `min > max` (`spec/CDDL.Spec.AST.Elab.fst:1257-1264`); a range
  that crosses zero and leaves `int64` cannot be built, since the bounds are
  `int64` (`:1265-1266`);
- `size` on anything but `bytes` or `text`, or with `min > max`
  (`spec/CDDL.Spec.AST.Elab.fst:1300-1334`);
- a text `literal` or key that is not ASCII or reaches 2^16 bytes
  (`spec/CDDL.Spec.AST.Base.fst:156`);
- a negative `tag`;
- an `exclusive` with fewer than two members, or with a repeated key;
- a `choice` with no case or a repeated case name, a `tuple` or `record` with
  a repeated member name or a field another combinator made;
- a member of a `tuple` or `record` in a binary record (`Codec.v`), or given a
  constraint or an action (`Field.v`), as for an ASN.1 component;
- a CBOR codec in a binary record outside `nested ~size` (with `Cbor.codec`,
  lane C3);
- a DER or a CBOR codec under `containing`, through `map` too (Backend
  selection).

Wire decides "two types meet" and "an array splits one way" with a port of
evercddl's own checks to the shapes the combinators build: `typ_disjoint`
(`spec/CDDL.Spec.AST.Elab.Disjoint.Type.fst`) and `array_group_disjoint`
(`spec/CDDL.Spec.AST.Elab.Disjoint.Array.fst`), in `lib/cbor.ml`. evercddl
reruns them with more fuel until they answer
(`src/cddl/tool/CDDL.Tool.Gen.fst:134-149`); wire doubles the fuel up to a
bound and refuses past it.

Floats, bounded occurrences (`n*m`), generics, sockets and plugs, `~`, `&`,
and every control other than `.size` and `.everparse-det-cbor` have no
combinator: evercddl's parser has no float literal (`Parser.ml:236-238`), no
generic parameter (`:223-228`), no `n*m` (`:269-270`), no `~` or `&` group
choice (`:342-345`, only `&(name: type)` parses) and two controls
(`:289-296`). It does parse sockets (`$`, `$$`, `/=`, `//=`, `:130-131`,
`:175-220`); wire has no use for them, since a description is closed.

## Running everparse-cddl from the prefix

The prefix built from `v2026.11.13`, `$EVERPARSE_HOME` below, holds
`$EVERPARSE_HOME/bin/everparse-cddl.exe` with `fstar.exe` and `krml` beside
it, Z3 5.3.0 under `$EVERPARSE_HOME/lib/fstar/z3-5.3.0`, the checked CBOR and
CDDL libraries and `CBORDet.o` under `$EVERPARSE_HOME/lib/everparse`, and the
headers under `$EVERPARSE_HOME/include/everparse`. The tool parses the
`.cddl` files given, writes an F* module, verifies it with F*, extracts it
through KaRaMeL to C, and compiles `M.c` (`Main.ml:283-425`). The postlude,
which defines `int`, `tstr`, `any` and the other prelude names, is not
implicit: it is passed as an input (`src/cose/Makefile:13-16`):

```
everparse-cddl.exe --mname M --odir OUT --tmpdir TMP \
  grammar.cddl $EVERPARSE_HOME/lib/everparse/cddl/spec/postlude.cddl
```

`--fstar_only` stops after writing the F* (`Main.ml:286`), `--skip_compilation`
before `cc`, `--rust` emits Rust instead (`Main.ml:261`), `--admit` admits the
SMT queries of the generated module (`Main.ml:298`).

The prefix the feasibility run used was built from source with `make ADMIT=1
package-subset`, whose log passes `--admit_smt_queries true` to the 583
library modules, `CDDL.Spec.AST.Elab.fst` among them. The library proofs are
therefore taken on trust from upstream, not rechecked here; the generated
module's proofs are checked in full, since the run passes no `--admit`. A
prefix built without `ADMIT` rechecks the libraries too.

### Feasibility run

Run on 2026-11-27 on macOS arm64, with `P` the prefix above and the
grammar EverParse ships, `src/cose/cose-sign.cddl` unchanged:

```
$P/bin/everparse-cddl.exe --mname COSE_Format --odir out --tmpdir tmp \
  cose-sign.cddl $P/lib/everparse/cddl/spec/postlude.cddl
```

rc 0, "EverCDDL succeeded!", 2148 s wall, while a `dune build` and `dune
test` of this repository ran beside it at `-j 8`, so the times are upper
bounds. From the files' timestamps:

| Step | Wall |
|---|---|
| parse the CDDL, write `COSE_Format.fst` (554 KB) | 13 s |
| F* verifies the module (`COSE_Format.fst.checked`, 34 MB) | 2037 s |
| F* extracts to `Custard.json` | 87 s |
| KaRaMeL writes `COSE_Format.c` (640 KB) and `.h` (57 KB) | 20 s |
| `cc -Wall -Werror -std=c11` compiles `COSE_Format.o` | 4 s |

The generated `COSE_Format.h` differs from the one EverParse commits
(`src/cose/c/COSE_Format.h`) only by two typedefs that the committed one has
and this one lacks (`COSE_Format_spect_evercddl_uint`,
`COSE_Format_spect_nint`).

A 50-line harness reads hex messages, calls
`COSE_Format_validate_and_parse_cose_sign1_tagged` and accepts when it answers
`Some` with an empty remainder; it links `COSE_Format.o` with the prefix's
`lib/everparse/CBORDet.o` (`cc` rc 0, 1 s). Over 20 inputs (one run,
under 1 s in all):

- the cose-wg example ECDSA-01 from ocaml-cose's `test/fixtures/ecdsa-sig-01.json`: accepted, 64-byte signature;
- 11 of the 14 pycose messages of ocaml-cose's `test/interop/pycose/traces/sign1.csv` (ES256, ES384, ES512, EdDSA, empty to 1 KB payloads): accepted;
- the other 3, `es256_full`, `eddsa_full` and `es256_sigstore`: **refused**. Their protected header is `a3 01 .. 04 .. 03 ..`, keys 1, 4, 3: pycose writes the map in insertion order, which is valid CBOR but not deterministic (keys sort 1, 3, 4). evercddl's parser is deterministic only, so it refuses them, and so will wire (Q10);
- 5 variants of ECDSA-01, each well-formed CBOR, all refused: the signature's length in a two-byte head (`59 0040`), tag 18 in a one-byte head (`d8 12`), the array as indefinite length (`9f .. ff`), the protected map with keys 3 then 1, and one trailing byte.

No step failed. Not taken: a run with `--rust`, and the COSE_Key and
COSE_Sign entry points, which the same object provides.

### The verified test

`make cbor` (`BUILD_EVERPARSE=1 dune build @test/cbor/runtest`) writes the
module of every codec of `test/cbor/cbor_modules.ml` with `print.exe` and
runs `Wire_cbor.run` on them: evercddl in full on each, with no `--admit`, the
postlude of its prefix as one more input, in `Wire_3d.pool` jobs. A module
passes when the tool answers 0, F* prints "All verification conditions
discharged" and evercddl "EverCDDL succeeded!", so its C compiled with
`-Wall -Werror`. The rule fails on any other answer.

Run on 2026-11-28 on macOS arm64 with the `v2026.11.13` prefix first in
`PATH`, four jobs at once, beside a full `dune test` of this repository: rc 0
in 27 minutes, all 26 modules verified. The times are each job's wall time,
upper bounds under that load.

| Module | Wall | Module | Wall |
|---|---|---|---|
| `COSE_Sign1_Tagged` | 354 s | `Cbor_keep_unknown` | 180 s |
| `COSE_Key` | 251 s | `Cbor_literal` | 160 s |
| `Cbor_any` | 162 s | `Cbor_map` | 252 s |
| `Cbor_array` | 219 s | `Cbor_nullable` | 179 s |
| `Cbor_assoc` | 207 s | `Cbor_range` | 146 s |
| `Cbor_choice` | 181 s | `Cbor_record` | 191 s |
| `Cbor_containing` | 223 s | `Cbor_simple` | 145 s |
| `Cbor_encoded` | 223 s | `Cbor_size` | 167 s |
| `Cbor_exclusive` | 222 s | `Cbor_strings` | 144 s |
| `Cbor_int` | 221 s | `Cbor_tag` | 178 s |
| `Cbor_keep_bytes` | 253 s | `Cbor_tuple` | 209 s |
| `Cbor_uint` | 221 s | `Clash` | 247 s |
| `Framed` | 461 s | `Enveloped` | 176 s |

CI's EverParse release, `v2026.10.27` (`.github/ci-dependencies.env`), ships
`bin/everparse-cddl.exe` and `lib/everparse/cddl/spec/postlude.cddl` in its
Linux archive, but no CI job sets `BUILD_EVERPARSE` for `test/cbor`, and no
run against that release was taken.

## The experimental scope for 2.0

The bar is ASN.1's: an interpreted codec that agrees with the C, and a
`.cddl` printer whose every emitted module evercddl accepts and verifies in a
`BUILD_EVERPARSE` test. Lanes C1 to C5 meet it; C6 and the rest come after
2.0.

| Lane | Files | State |
|---|---|---|
| C1 deterministic core | `lib/cbor_det.ml(i)` (`Wire.Private`: heads, the table's rules, key order, UTF-8, skipping `any`), `test/test_cbor_det.ml` with Appendix A and the 363 EverCBOR rows | done |
| C2 construction and refusals | `lib/cbor.ml` constructors, a `Cbor` node beside `Der` in `lib/types.ml(i)`, `test/test_cbor.ml` with one test per refusal | done |
| C3 interpreted codec | `lib/cbor.ml` decode, validate, encode, `containing` handover, views; `examples/cose/` with the running example and COSE_Key; the COSE probes with the verdicts of evercddl's C | done |
| C4 CDDL printer | `lib/cbor_cddl.ml` behind `Cbor.project`, `pp`, `write`; `test/cbor` pins the text of COSE_Sign1, COSE_Key and one module per construct | done |
| C5 evercddl pipeline | `lib/cbor/wire_cbor.ml(i)` (locate the tool, pass the postlude), the `BUILD_EVERPARSE` rule of `test/cbor` running every module of `Cbor_modules` in full (`make cbor`) | done |
| C6 differential and fuzz | `test/diff/` three readers, an Alcobar fuzz test of arbitrary and mutated COSE_Sign1, and the C harness replaying the COSE vectors | after 2.0 |

Also after 2.0: stubs binding the C (the API above is their target) and
the handover glue they carry, so `Codec.View.check` and `Codec.decode` run
the verified C; a member read in O(1) once a view is checked, where
`View.read` today walks the record to find the member at each read; DER in
CBOR (Q11); `Wire_ocaml` and `Wire_rust` generation; a fold over arrays in a
view; and SUIT, whose command sequences recurse (Q3).

Every `BUILD_EVERPARSE` run pays F*'s verification of each module, so the
rule checks one small module per construct plus the COSE ones, in
`Wire_3d.pool` jobs (`WIRE_3D_JOBS`), and dune caches the result by content.

## Open questions

- **Q1** Whether `Codec.View.check` binds `M_validate_and_parse_T`, which
  builds the C struct, or `cbor_det_validate` plus `M_validate_T`, which do
  not. Settled by the instructions per COSE_Sign1 of each through the stubs.
- **Q2** How evercddl names the C types of anonymous wire types and of
  `exclusive`, which the stubs need. Settled by reading the header the C5
  rule writes for the running example (`test/cbor/out/COSE_Sign1_Tagged.h`
  under `_build`), when the stubs are written.
- **Q3** SUIT: `SUIT_Command_Sequence` recurses through `try-each` and
  `run-sequence` (`suit/command.mli:26-53`), which evercddl refuses. Settled
  by either a depth-unrolled grammar evercddl accepts, or treating a nested
  sequence as `bstr` checked by a second pass; decided on the SUIT Appendix B
  vectors.
- **Q4** x5chain accepts a one-certificate array that RFC 9360 says is a bare
  `bstr`. Settled when evercddl takes bounded occurrences (`Parser.ml:269`,
  "TODO: bounds"), or by a check outside the grammar that the oracle covers.
- **Q5** The name `exclusive`: no OCaml CBOR library has the group, and RFC
  8610 has no name for "at most one of these members". Settled at interface
  review.
- **Q6** Literals and keys are `int`, narrower than CBOR's 65-bit integers and
  32-bit on js_of_ocaml. Settled by `make test-wasm` with a key above 2^31,
  as Q7 of docs/asn1.md settles OID arcs.
- **Q7** `.cbor` without determinism: evercddl only checks
  `.everparse-det-cbor` (`Parser.ml:294`), so a byte string holding
  non-deterministic CBOR has no projection. Settled when evercddl adds
  `.cbor` over EverCBOR's nondet API.
- **Q8** Whether ocaml-cbor can be a test dependency of wire, released or
  vendored, for the third reader. Settled by its opam status at C6.
- **Q9** The predicate `tuple` uses to refuse an ambiguous array: evercddl's
  `array_group_concat_unique_weak` and `_strong`
  (`spec/CDDL.Spec.AST.Elab.Base.fst`) over `array_group_disjoint`. C2 ports
  them to the groups `tuple` and `array` build: an `elem_opt` must be
  disjoint from the closed rest of its tuple
  (`spec/CDDL.Spec.AST.Elab.Base.fst:1153-1155`). Half settled: evercddl
  verifies every module the C5 rule runs, so it accepts what `tuple`
  accepts there. The other half, the grammars `tuple` refuses written by
  hand and given to evercddl, is not run.
- **Q10** Deterministic only refuses COSE that other implementations write:
  3 of ocaml-cose's 14 pycose messages put protected keys in insertion order
  (feasibility run), and evercddl, so wire, refuses them, where ocaml-cose's
  interop test decodes and verifies every row
  (`test/interop/pycose/test.ml:197`; that test was not run here). A verifier
  checks the protected bytes as sent, so it cannot re-encode them first.
  Settled by whether 2.0 offers wire's COSE as a reader for profiles that
  mandate deterministic encoding and leaves interop reading to ocaml-cose, or
  waits for evercddl on the nondet API (Q7).
- **Q11** DER in CBOR: `Cbor.containing` refuses a DER codec, so x5chain
  cannot be written, because `Asn1.project` cannot read a CBOR codec while
  `Cbor` reads DER codecs through `Asn1`. Settled by one walk of a codec
  over both backends that each projection calls, outside both modules.
