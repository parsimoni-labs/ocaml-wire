# Registered-boundary timing measurements

The stream core has stable semantic signal names. Naming uses Hardcaml v0.17.1
`Signal.Type.add_name`, including the existing feedback wire when a register is
returned through one. It adds no registers, logic, KEEP or DONT_TOUCH attributes.
Primary input aliases and constants are excluded. See the official
[Hardcaml naming guide](https://docs.hardcaml.org/hardcaml-docs/more-on-circuit-design/naming/).

The authoritative selectors and budgets are in
`bench/hardcaml-cloud/wire_hardcaml_tools/timing.py`. The package's `measure`
command imports that module instead of maintaining another classification.
When Yosys removes a named bus alias, the analyzer also resolves actual FF `src`
provenance against the named nonblocking-assignment target in the exact emitted
RTL. Source locations are discovered from each netlist, never hardcoded. Both
netlist and RTL hashes are recorded; provenance coverage is reported separately.
Selectors identify named nets and their actual FF Q drivers; they do not require
synthesis-generated FF cell names to retain a source name. In particular,
`wire_result_bank_0_q_accept` through `_3_q_accept` identify validation endpoints.
An absent required stage is an error. The stage classes overlap and describe
measurement endpoints, not additional pipeline stages.

| Endpoint class | Semantic name selector |
| --- | --- |
| Registered input boundary | `*wire_boundary_input_*` |
| KEEP/count/sequence ingress | `*wire_ingress_*` |
| Captured prefix and EOF snapshot | `*wire_capture_*`, `*wire_frame_*` |
| Decode/extraction register | `*wire_decode_extract_q_*` |
| Validation acceptance register | `*wire_result_bank_*_q_accept*` |
| Result banks and controls | `*wire_result_bank_*` |
| Registered output boundary | `*wire_boundary_output_*` |

Every measured register-to-register path has the following **planning budget**.
These allocations are not achieved measurements and do not establish 250 MHz.

| Path class | Period | Setup uncertainty | CQ/setup/skew allowance | Logic allocation | Routing allocation | Achieved delay |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| Input/ingress to ingress | 4 ns | 0.2 ns | 0.4 ns | 2.0 ns | 1.4 ns | Not measured |
| Input/ingress/capture to prefix | 4 ns | 0.2 ns | 0.4 ns | 2.0 ns | 1.4 ns | Not measured |
| Prefix to decode/extraction | 4 ns | 0.2 ns | 0.4 ns | 2.0 ns | 1.4 ns | Not measured |
| Decode to validation/result banks | 4 ns | 0.2 ns | 0.4 ns | 2.0 ns | 1.4 ns | Not measured |
| Result banks to output boundary | 4 ns | 0.2 ns | 0.4 ns | 2.0 ns | 1.4 ns | Not measured |

## Reproduction

Generate the normal core and manifest using the existing Dune generator. The
`--unnamed OUTPUT.v` mode exports exactly the same circuit after clearing internal
names, retaining public port names. Comparing the unnamed exports from before and
after the naming change checks graph identity without depending on RTL aliases.
The exports are byte-identical; the manifest is unchanged. The existing 17
transport/configuration tests also pass with the verified EverParse C oracle.

Create a bundle with `python3 bench/hardcaml-cloud/tools.py bundle CORE.json CORE.v BUNDLE`, then
run `python3 bench/hardcaml-cloud/tools.py timing BUNDLE BOUNDARY.v`. This is the existing fully
registered timing fixture, not a replacement AXI transport component. Both input
and output boundary registers are preserved by the fixture; the core is not.
The output timing measurement includes result-bank selection, acceptance masking
and length saturation before the registered boundary.

For a local structural XCUP proxy, use the installed Yosys and ABC9 flow:

```sh
yosys -p 'read_verilog CORE.v BOUNDARY.v; setattr -unset keep_hierarchy; setattr -mod -unset keep_hierarchy; synth_xilinx -family xcup -top wire_timing_boundary -flatten -noiopad -noclkbuf -json core.json'
python3 bench/hardcaml-cloud/tools.py measure core.json --rtl CORE.v BOUNDARY.v > stages.json
```

Only hierarchy preservation is removed for flattening. Boundary register
`keep`/`dont_touch` attributes remain. This analysis requires actual stage FFs;
it does not substitute primary ports for missing boundary registers.

The JSON reports mapped-cell dependency depth and input-pin fanout. Traversal
stops at every FF boundary and includes data, enable and reset endpoints.
Multi-output arithmetic cells conservatively depend on their inputs, so these
are structural upper bounds, not physical critical paths. A CARRY4 and a LUT do
not have equal delay. There is no conversion from logic levels to nanoseconds.
Vivado post-synthesis/place/route reports with the complete boundary fixture are
required for achieved timing. Existing ECP5 core-only results do not replace them.

## Published-profile scope audit

The read-only comparison artifact is pinned to
[`ptp_v1`, commit 23c7c1a29008677637764c2bdf07ebcd505b0e42](https://github.com/boutros-lab/patterns-to-parsers/tree/23c7c1a29008677637764c2bdf07ebcd505b0e42).
Its [simple P4](https://github.com/boutros-lab/patterns-to-parsers/blob/23c7c1a29008677637764c2bdf07ebcd505b0e42/p4/simple_parser.p4)
contains Ethernet, IPv4/IPv6, TCP/UDP/ICMP and up to two fixed eight-byte IPv6
extension headers. The [full P4](https://github.com/boutros-lab/patterns-to-parsers/blob/23c7c1a29008677637764c2bdf07ebcd505b0e42/p4/full_parser.p4)
adds up to two VLAN tags and two MPLS labels. Both mark five-tuple fields using
`__sip__`, `__dip__`, `__sp__`, `__dp__`, `__prot__`; IPv6 addresses are 128 bits.
The tagged-field mechanism is explicit in
[`engine/tools/p4_to_ir.py`](https://github.com/boutros-lab/patterns-to-parsers/blob/23c7c1a29008677637764c2bdf07ebcd505b0e42/engine/tools/p4_to_ir.py).

These profiles differ from the current Wire IPv4/TCP/UDP stack. In this artifact,
ordinary IPv4 transport dispatch requires IHL=5; other IHL values default to P4
accept without extracting transport. The P4 does not validate IPv4 total length,
fragment-dependent transport presence, TCP data offset or UDP length. Checksum
controls are empty. IPv6 extension dispatch recognizes next-header 0x2b and ignores
extension length. The ICMP constant is literally 0x01 for both IP versions;
a matched experiment must not silently substitute ICMPv6's usual 0x3a. The
post-MPLS IPv4 path does not apply the ordinary path's IHL=5 condition.
These observations describe source semantics; they are not a claim that generated
artifact RTL has been independently checked for equivalence to that P4.

The artifact's [Table 1 reproduction instructions](https://github.com/boutros-lab/patterns-to-parsers/blob/23c7c1a29008677637764c2bdf07ebcd505b0e42/artifact_evaluation/README.md)
explicitly pass `PART=xc7vx690tffg1761-2`. Its
[generic Vivado script](https://github.com/boutros-lab/patterns-to-parsers/blob/23c7c1a29008677637764c2bdf07ebcd505b0e42/run_vivado.tcl)
otherwise defaults to a different Versal device. It requests OOC synthesis then
implementation. The
[parser template](https://github.com/boutros-lab/patterns-to-parsers/blob/23c7c1a29008677637764c2bdf07ebcd505b0e42/engine/templates/parser_top_rtl_tmpl.sv)
uses input skid buffering, latched flits, registered accumulation and registered
field outputs, with DONT_TOUCH on several explicit state declarations. This is
not the current core's three-cycle pipeline. Match field set, full protocol graph,
validation scope, device, IO fixture and preservation policy before comparing
frequency or area. No extraction-only Wire implementation or relaxed validator
has been introduced by this audit.

## Local XCUP structural observations

Yosys 0.70+368 (`7cf88aca6`), `synth_xilinx -family xcup` with ABC9, mapped the
complete registered fixture to 5,099 FFs, 3,897 LUT2–LUT6 cells, 96 CARRY4 and
64 INV cells. All 5,099 FFs resolve to the stage classes using aliases or actual
source provenance; validation FFs are also members of the result-bank class.
This is a generic XCUP mapping, not a Vivado result for a placed VU47P.

| Class | Actual FFs | Maximum input-pin loads on one Q bit |
| --- | ---: | ---: |
| Input boundary | 581 | 759 (reset) |
| Ingress | 86 | 4 |
| Prefix/EOF snapshot | 1,399 | 602 |
| Decode/extraction | 493 | 168 |
| Validation accept subset | 4 | 1 |
| Result banks/control | 2,031 | 509 (read index) |
| Output boundary | 509 | 0 |

| Register path class | Conservative maximum mapped-cell depth | Cells on that dependency chain |
| --- | ---: | --- |
| Input/ingress to ingress | 17 | 16 CARRY4 + INV (64-bit sequence increment) |
| Input/ingress/capture to prefix | 15 | 6 CARRY4 + 9 LUTs (KEEP-to-snapshot path) |
| Prefix to extraction | 8 | CARRY4 + INV + 6 LUTs |
| Extraction to validation | 11 | 5 CARRY4 + INV + 5 LUTs |
| Extraction to result banks | 11 | Same validation dependency |
| Result banks to registered output | 2 | 2 LUTs, including output masking/control |

These chains identify structural candidates for later target timing inspection.
They do not establish that the sequence increment is physically slower than the
validation chain. Carry cells have dedicated routing; the analyzer deliberately
uses a conservative cell-level dependency model. The explicit registered output
boundary now includes logic excluded from the earlier core-only ECP5 clock report.
No architecture, latency, validation rule or buffer capacity was changed here.
