# ASN.1 DER

`wire` describes ASN.1 DER (X.690) in the same description as binary
formats. A wire codec projects each part to the verified parser technology
that expresses it, and is checked there: EverParse 3D for binary layouts,
everparse-asn1 for DER (CLAUDE.md, Feature shippability). 3D has no base-128
tags or OID arcs, no OPTIONAL decided by peeking at a tag and no SET OF
ordering; everparse-asn1 is EverParse's verified front end for exactly those.

EverParse paths are from its root at `49875441c`, with `README` for
`src/ASN1/tool/README.md`, `asn1.rst` for `doc/asn1.rst` and `spec/` for
`lib/everparse/asn1/spec/`; `asn*.ml*` are asn1-combinators' `src/`, `x509/`
ocaml-x509's `lib/`.

## Concepts

- An ASN.1 type `'a Asn1.t` describes the DER bytes of OCaml values `'a`, once.
- A SEQUENCE or SET is a record: its components are `Field.t` values bound with `Codec.( $ )`, as in `Codec.v`, and `Asn1.codec` gives its `Codec.t`.
- Values: `Codec.decode` and `Codec.encode` copy the whole value; bad bytes are an `Error`, never an exception.
- Views: `Codec.View.check` runs the DER validator once (never raises, allocates nothing), then `ok` and `get`; a component is read in place with `checked_field`, a SEQUENCE OF folded with `Asn1.fold`. DER lengths move every offset, so nothing is set in place.
- One description, two backends: a DER part nests in a binary codec, and a binary part in DER, as a byte region the other backend validates.
- A shape no backend expresses is refused when it is built, with `Invalid_argument`.

## Running example: RFC 5280 SubjectPublicKeyInfo

```ocaml
open Wire
type params = Null | Curve of Asn1.Oid.t | Absent
type alg = { oid : Asn1.Oid.t; params : params }
type spki = { alg : alg; key : int * string }
let rsa = Asn1.Oid.(base 1 2 <|| [ 840; 113549; 1; 1; 1 ])
let ec = Asn1.Oid.(base 1 2 <|| [ 840; 10045; 2; 1 ])
let ed25519 = Asn1.Oid.(base 1 3 <|| [ 101; 112 ])
let f_oid = Asn1.required "algorithm" Asn1.oid
let f_params =
  Asn1.(defined_by "parameters" ~on:f_oid [
    case rsa null ~inject:(fun () -> Null) ~project:(function Null -> Some () | _ -> None);
    case ec oid ~inject:(fun c -> Curve c) ~project:(function Curve c -> Some c | _ -> None);
    absent ed25519 ~inject:Absent ~project:(function Absent -> true | _ -> false) ])
let alg_id =
  Asn1.sequence "AlgorithmIdentifier" (fun oid params -> { oid; params })
    Codec.[ (f_oid $ fun a -> a.oid); (f_params $ fun a -> a.params) ]
let f_alg = Asn1.required "algorithm" alg_id
let f_key = Asn1.required "subjectPublicKey" Asn1.bit_string
let spki =
  Asn1.sequence "SubjectPublicKeyInfo" (fun alg key -> { alg; key })
    Codec.[ (f_alg $ fun s -> s.alg); (f_key $ fun s -> s.key) ]
let codec = Asn1.codec spki
```

`Asn1.project "SPKI" codec` prints the module everparse-asn1 reads, in the
`--#DEFINED-BY` form of `README:286-326`, the shape of
`share/everparse/tests/asn1/tool/X509Lite.asn:11-31`:

```
SPKI DEFINITIONS EXPLICIT TAGS ::= BEGIN
--#DEFINED-BY AlgorithmIdentifier.parameters {
--#  rsa : NULL, ec : OBJECT IDENTIFIER, ed25519 : ABSENT
--#} FALLBACK NONE
rsa OBJECT IDENTIFIER ::= { 1 2 840 113549 1 1 1 }
ec OBJECT IDENTIFIER ::= { 1 2 840 10045 2 1 }
ed25519 OBJECT IDENTIFIER ::= { 1 3 101 112 }
AlgorithmIdentifier ::= SEQUENCE {
  algorithm OBJECT IDENTIFIER, parameters ANY DEFINED BY algorithm OPTIONAL }
SubjectPublicKeyInfo ::= SEQUENCE {
  algorithm AlgorithmIdentifier, subjectPublicKey BIT STRING }
END
```

ocaml-x509 writes it with `sequence2` and a recursive `fix` over a `choice4`
of parameters (`x509/algorithm.ml:439-456`, `x509/public_key.ml:91-100`);
everparse-asn1 rejects recursion (`README:494`), so wire keys the parameters
by the OID, as RFC 5280 does.

## Where the DSL lives

In `wire`, as `Wire.Asn1`, not in a separate library. A DER value is a field
of a binary record (`Field.v "cert" (nested ~size (codec (Asn1.codec cert)))`)
and a binary record is the content of a DER OCTET STRING
(`Asn1.containing`), so the combinators must produce the `typ`, `Field.t` and
`Codec.t` that `Codec.decode`, `Codec.View` and `Everparse.project` consume.
`typ` is abstract, so a separate library could not extend it, and a DSL that
cannot nest in a wire codec would contradict the rule. `'a Asn1.t` stays a
type of its own because tagging, OPTIONAL and CHOICE need the tag set of their
argument, which an arbitrary `typ` (`uint16be`) does not have: the type rules
that out statically. `wire.asn1` (`Wire_asn1`) is to `everparse-asn1.exe`
what `wire.3d` is to `everparse-3d.exe`: it runs the tool and writes the dune
rules and C harnesses, so it will link `unix`, which `wire` does not.

Names are asn1-combinators' (`asn.mli:80-342`: `required`, `optional`,
`sequence`, `set`, `sequence_of`, `set_of`, `choice`, `implicit`, `explicit`,
the primitives, `cls`, `Oid.base`, `<|`, `<||`), but a record binds fields as
`Codec.v` does rather than as `sequence2..6` tuples, so a view addresses each
component by its field, and `map` is `Wire.map`'s `~decode ~encode`. The
rest are X.680's or EverParse's: `default`, `size`, `alternative` (X.680),
`bounded_integer` (`spec/ASN1.Syntax.fst:224`), `defined_by`, `fallback`
(`README:286-320`), `keep_bytes` (`README:207`), `containing` (`README:271`).

## Constructs and their projections

| `Wire.Asn1` | `.asn` | interpreted OCaml |
|---|---|---|
| `bool` | `BOOLEAN` | `0x00`/`0xFF` only |
| `integer` | `INTEGER` | minimal content octets, as a string |
| `bounded_integer ~min ~max` | `INTEGER (min..max)`, C `int64_t` (`README:254`) | `int64` in range |
| `enumerated ns` | `ENUMERATED { n(i), ... }` | `int` in `ns` |
| `null`, `oid`, `octet_string` | `NULL`, `OBJECT IDENTIFIER`, `OCTET STRING` | `()`, arcs below 2^32, bytes |
| `bit_string` | `BIT STRING` | unused count, octets (`spec/ASN1.Spec.Content.BITSTRING.fst:32`) |
| `utf8_string` ... `bmp_string` | the string type | content octets, alphabet checked |
| `utc_time`, `generalized_time` | `UTCTime`, `GeneralizedTime` | characters, format checked |
| `any` | `ANY` | one whole TLV |
| `size ~min ~max t` | `t (SIZE (min..max))` | length in characters |
| `implicit`/`explicit ?cls n t` | `[cls n] IMPLICIT/EXPLICIT t` | tag replaced / wrapped |
| `required`, `optional`, `default v` | component, `OPTIONAL`, `DEFAULT v` | absent if the next tag is not its own |
| `sequence n make fields` | `n ::= SEQUENCE { ... }` | record |
| `set n make fields` | `n ::= SET { ... }` | record, components in tag order |
| `sequence_of`/`set_of ?non_empty t` | `SEQUENCE/SET [SIZE (1..MAX)] OF t` | list, SET OF in DER order |
| `choice alts` | `CHOICE { name t, ... }` | the alternative whose tag is next |
| `defined_by n ~on cases` | `ANY DEFINED BY on` + `--#DEFINED-BY` table | type chosen by the key |
| `keep_bytes t` | `--#KEEP-BYTES T` (`README:207-240`) | value and its DER bytes |
| `containing t` | `OCTET STRING (CONTAINING T)` if `t` is DER, else `OCTET STRING` | content decoded by `t` |
| `map` | the type mapped | converted |

An anonymous type that `.asn` must name is named by its path, as
everparse-asn1 names C types (`README:244-245`); every tag carries its keyword.

## Backend selection

A codec built by `Asn1` is a DER codec; any other is a binary codec. Each
projects to its own backend, and a codec of one kind nested in the other is
handed over as a byte region:

- **DER in binary.** A field `codec (Asn1.codec t)` of a `Codec.v` record
  must sit under `nested ~size:e` (else `Codec.v` raises): 3D sees
  `UINT8 f[:byte-size e]` whose `:on-success` action passes `field_ptr` and
  `e` to an extern (`doc/3d-lang.rst:558,587-597`) that `Wire_asn1` writes as
  `validate_T(slice, &off) && off == e`, the full-consumption idiom of
  `share/everparse/tests/asn1/tool/small_test.c:139-143`.
- **Binary in DER.** `containing c` projects to `OCTET STRING`, since
  everparse-asn1 has no extern hook (`README:437-481` lists none). The
  generated check runs `validate_T`, then the 3D validator of `c` on exactly
  the content slice of each such component (reached with `T_seq_*`,
  `README:174-205`), and accepts iff all accept.
- **Interpreted.** `Codec.validate` on the outer codec calls the inner
  codec's own validator on the region; one answer either way.

`Everparse.project c` is a binary codec's 3D schema, DER regions handed over;
it raises `Invalid_argument` on a DER codec. `Asn1.project m c` is the ASN.1
module of a DER codec, or of the DER types a binary codec holds;
`Asn1.contained c` is the 3D schemas of the binary codecs a DER codec holds.
3d.exe and F* check each 3D schema (`make 3d`), everparse-asn1's F* proofs
each ASN.1 module, tag ambiguity included (`README:8-27`). The handover glue
is unverified C, which the differential test covers.

## DER rules and who enforces them

The interpreted codec enforces what the everparse-asn1 specification does,
no more, so `decode` fails exactly when `validate_T` does (docs/execution.md,
Invariants, "One answer"). Encoders write DER only.

| X.690 rule | everparse-asn1 | asn1-combinators `der` |
|---|---|---|
| definite, minimal length (10.1) | `spec/ASN1.Spec.LengthU32.fst:9-18` | `asn_ber_der.ml:70,102` |
| minimal tag number (8.1.2) | `spec/ASN1.Spec.IdentifierU32.fst:16` | `asn_ber_der.ml:55,91` |
| primitive strings only (10.2) | `spec/ASN1.Syntax.fst:26-35` | `asn_ber_der.ml:185` |
| minimal INTEGER (8.3.2) | `spec/ASN1.Spec.Content.INTEGER.fst:682-692` | `asn_prim.ml:98-99` |
| BOOLEAN `00`/`FF` (11.1) | `spec/ASN1.Spec.Content.BOOLEAN.fst:13-22` | not checked (`asn_prim.ml:64-68`) |
| BIT STRING unused 0-7, zero (11.2.1) | `spec/ASN1.Spec.Content.BITSTRING.fst:13-24` | range only (`asn_prim.ml:168-172`) |
| named bits, no trailing 0 (11.2.2) | not checked (`asn1.rst:622-625`) | n/a |
| DEFAULT omitted (11.5) | parser refuses (`spec/ASN1.Spec.Sequence.fst:32-36`), writer omits (`README:105-106`) | no DEFAULT |
| SET components in tag order (10.3) | `spec/ASN1.Spec.SetComponents.fst:4-10` | sorts on encode (`asn_ber_der.ml:422-431`) |
| SET OF sorted (11.6) | `spec/ASN1.Spec.Raw.fst:13-14,655-668` | sorts on encode (`asn_ber_der.ml:433-441`), not on decode (`:222`) |
| minimal OID subidentifiers (8.19) | `spec/ASN1.Spec.Content.OIDU32.fst:20-21` | `asn_prim.ml:230` |

Wire does not check 11.2.2 either, so it agrees with the C; it offers no
named-bit type until that is settled (Q6).

## The C API the stubs bind

Per root `T` (`README:83-126`): `validate_T(slice, &off)` succeeds exactly
when the spec parser does; `parse_T` copies nothing; `write_T(x, out, &pos)`
writes right to left, answering `JIT_OK`, `JIT_INVALID` or `JIT_NO_SPACE`;
`size_T` (`src/ASN1/tool/ocaml/everparse-asn1/Gen.ml:580-581`, not in the
README). Lists: `L_iter_start`, `L_iter_next`, `L_length`, `L_nth`
(`README:128-166`); components: `T_seq_start`, `T_seq_next`, `T_c_get`
(`README:174-205`). The stubs bind `Codec.View.check` to `validate_T`,
`checked_field` to `T_seq_*`, `Asn1.fold` to `L_iter_*`, `Codec.decode` to
`parse_T`, `Codec.encode` to `write_T`.

## The differential oracle

Three readers of one input: wire's interpreted codec, the everparse-asn1 C
(through `Wire_asn1.generate_agree`) and asn1-combinators' `der` codec over
the ocaml-x509 grammar. Wire and the C must agree exactly: verdict, consumed
length, value, and the bytes of re-encoding. Wire and asn1-combinators must
agree on every input both accept; a reject only asn1-combinators misses is
allowed in the classes the table above marks (BOOLEAN, BIT STRING padding,
SET OF order, DEFAULT present), each with a vector proving it. Inputs: the
certificates of ocaml-x509's tests; the corpus EverParse ran (14620
certificates and 6154 CRLs, `asn1.rst:688-689`), likely
`src/ASN1/evaluation/data.zip`, not opened here; and EverParse's vectors in
`share/everparse/tests/asn1/difftest/vectors/`, whose verdicts cite X.690 and
never come from a parser (`src/ASN1/difftest/README.md:16-26`).

## Refused when built

The combinators raise `Invalid_argument`, each refusal unit-tested, on:

- recursion: no `fix` (`README:494`, `asn1.rst:619-621`);
- `implicit` on a CHOICE or ANY (`spec/ASN1.Syntax.fst:136`); asn1-combinators
  silently makes it explicit (`asn.mli:124-129`), wire does not;
- a CHOICE with two alternatives of one tag (`spec/ASN1.Syntax.fst:149`) or an
  ANY alternative (`README:493`); a SET with two components of one tag
  (`spec/ASN1.Syntax.fst:281`) or over 64 (`README:463`);
- a run of OPTIONAL/DEFAULT components whose tags meet each other or the next
  (`spec/ASN1.Base.fst:479-496`), which everparse-asn1 only finds in F*
  (`README:502-504`);
- DEFAULT on a non-primitive, an OID or under a constrained EXPLICIT INTEGER
  (`README:464-467,496-498`);
- SIZE on a SEQUENCE OF other than `1..MAX`, which `?non_empty` is
  (`README:499-500`); SIZE that is not one range or reaches 65536
  (`README:460-461,487`);
- a `defined_by` key that is OPTIONAL, constrained, after its component, in
  another SEQUENCE, or a second table in one SEQUENCE (`asn1.rst:648-652`);
- an OID arc or tag number of 2^32 or more (`spec/ASN1.Spec.Content.OIDU32.fst`,
  `spec/ASN1.Spec.IdentifierU32.fst`), a UNIVERSAL 0 tag
  (`spec/ASN1.Syntax.fst:10-12`);
- `keep_bytes` on a CHOICE alternative, a DEFAULT component or a
  `defined_by` key or component (`README:234-240`);
- a DER codec in a binary record outside `nested ~size`.

REAL, extension markers and BER have no combinator at all.

## Building everparse-asn1

Not built here. `make asn1-tool` at the EverParse root builds
`bin/everparse-asn1.exe` (`README:31-34`, `Makefile:117-119`) with OCaml and
dune from EverParse's opam switch (`src/ASN1/tool/Makefile:2`). Running it
also needs F*, Z3 5.3.0 (`lib/everparse/z3-version.Makefile:1`), `cc`, and
the verified ASN.1 and LowParse libraries under `$EVERPARSE_HOME`
(`README:69-78`), which `make asn1` verifies, Pulse included
(`Makefile:104-115`). `~/.local/everparse` has F* but neither.

## Open questions

- **Q1** `Asn1.fold` hands each element as a value; the model's fold hands a
  view. Settled by allocations per certificate on the x509 corpus walking
  extensions, against `L_iter_next`'s zero-copy element parse.
- **Q2** Whether a 3D byte array's `:on-success` can call a `UINT8` extern on
  `field_ptr`; if not, DER-in-binary uses post-validation glue too. Settled by
  a ten-line `.3d` through 3d.exe.
- **Q3** Time carriers: strings here, Ptime in asn1-combinators
  (`asn.mli:304-307`). Settled by whether `wire` may depend on ptime under
  `make test-wasm`, and by what `spec/ASN1.Spec.Content.TIME.fst` accepts.
- **Q4** The `--#DEFINED-BY` syntax of a typed or OPTIONAL `FALLBACK`:
  `README:302` shows only `FALLBACK NONE`. Settled by the front end's parser
  (`src/ASN1/tool/ocaml/everparse-asn1/`) or one run of the tool.
- **Q5** Whether `Wire_ocaml` and `Wire_rust` generate DER modules. Settled by
  the bench of the interpreted codec against the C through the stubs.
- **Q6** Named-bit BIT STRINGs (`asn.mli:342`, X.509 KeyUsage): neither side
  checks 11.2.2, so KeyUsage decodes through `bit_string`. Settled when
  everparse-asn1 checks 11.2.2 (`asn1.rst:622-625` lists it as not checked).
- **Q7** OID arcs are `int` in `Oid.base` and `<|`, narrower than 2^32 on
  wasm. Settled by `make test-wasm` with a 2^31 arc through `Oid.of_string`.
- **Q8** Per-key OPTIONAL (`README:314`) and same-length in-place `set`
  wait for a protocol that needs them.
