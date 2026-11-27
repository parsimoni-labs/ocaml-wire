.PHONY: build test test-wasm test-oxcaml fuzz fuzz-paranoid fuzz-afl fuzz-afl-diff \
       fuzz-nightly fuzz-rust 3d asn1 bench bench-demo bench-routing bench-gateway \
       bench-clcw bench-rust bench-flow bench-flow-swap bench-apps-rust bench-apps-count \
       prof memtrace memtrace-demo memtrace-routing memtrace-gateway memtrace-clcw \
       cppcheck prove verify clean

build:
	dune build

test:
	dune runtest

# Runs test/wasm and a short smoke of the fuzz properties (fuzz/wasm) under
# node, built with wasm_of_ocaml (31-bit ints) and js_of_ocaml (32-bit), then
# fails on any integer-overflow truncation warning from wire's own code or the
# fuzz generators. optint's two
# known literals (0x7fffffff, 0x40000000) are filtered: its 32-bit emulation
# modules truncate to the bit patterns they intend on a 31-bit target, and
# the runtime checks prove the values survive. Warnings only surface for
# freshly compiled units, so the grep is meaningful on a cold build (CI).
# Remove the filter once https://github.com/mirage/optint/pull/31 is released.
test-wasm:
	@command -v wasm_of_ocaml >/dev/null || \
	  { echo "wasm_of_ocaml not found: opam install wasm_of_ocaml-compiler"; exit 1; }
	@command -v node >/dev/null || { echo "node not found"; exit 1; }
	@mkdir -p _build
	dune build @test/wasm/runtest @fuzz/wasm/runtest 2> _build/wasm-build.log; \
	status=$$?; cat _build/wasm-build.log >&2; \
	test $$status -eq 0 || exit $$status; \
	! grep "integer-overflow" _build/wasm-build.log \
	  | grep -v -e 0x7fffffff -e 0x40000000 | grep -q . \
	  || { echo "error: wasm_of_ocaml truncated an integer literal"; exit 1; }

# The wire.ocaml tests and its differential lane on the code it writes for
# OxCaml, with every [@zero_alloc] checked: run from an OxCaml switch that has
# the test dependencies (alcobar included). The dev profile, which compiles
# libraries -opaque, so each check relies on the [@zero_alloc] the runtime's
# interfaces state; the release one, which inlines across them; then the
# paranoid one, which checks every unchecked load as well.
OXCAML_LANES = @test/ocaml-ox/runtest @fuzz/diff/ocaml-ox/runtest \
  @test/runtime-ox/runtest @proofs/fstar-gen/runtest
test-oxcaml:
	@test "$$(ocamlopt -config-var ox)" = true || \
	  { echo "not an OxCaml compiler"; exit 1; }
	dune build $(OXCAML_LANES)
	dune build --profile release $(OXCAML_LANES)
	dune build --profile paranoid $(OXCAML_LANES)

# The wire fuzz properties alone; WIRE_FUZZ_SEED, WIRE_FUZZ_REPEAT and
# WIRE_FUZZ_BUDGET widen the search (fuzz/dune).
fuzz:
	dune build @@fuzz/runtest

# Every test and property with the unchecked reads checked (lib/dune).
fuzz-paranoid:
	dune runtest --profile paranoid

# AFL campaigns, WIRE_AFL_SECONDS each (default 60); fuzz-afl-diff needs
# EverParse. A crash is replayed with _build/default/<dir>/fuzz.exe <file>,
# which prints the divergence. The Rust lane's campaign needs rustc:
# BUILD_RUST=1 dune build --profile afl @fuzz/diff/rust/fuzz.
FUZZ_AFL_DIFF = @@fuzz/diff/fuzz @@fuzz/diff/ocaml/fuzz @@fuzz/diff/ocaml-ox/fuzz

fuzz-afl:
	dune build --profile afl @@fuzz/fuzz @@fuzz/3d/fuzz

fuzz-afl-diff:
	BUILD_EVERPARSE=1 dune build --profile afl $(FUZZ_AFL_DIFF)

# The sampled codecs against the Rust Wire_rust generates from them (needs
# rustc).
fuzz-rust:
	BUILD_RUST=1 dune build @fuzz/diff/rust/runtest

# What the nightly workflow runs: long property runs, then long campaigns.
fuzz-nightly:
	WIRE_FUZZ_REPEAT=1000000 WIRE_FUZZ_BUDGET=20 dune build @@fuzz/runtest
	WIRE_FUZZ_REPEAT=1000000 WIRE_FUZZ_BUDGET=20 \
	  dune build @@fuzz/runtest --profile paranoid
	WIRE_AFL_SECONDS=3600 $(MAKE) fuzz-afl
	WIRE_AFL_SECONDS=7200 $(MAKE) fuzz-afl-diff

# The scanners of the codecs proofs/fstar-gen/dune lists, proved by F* equal
# to the parser 3d.exe writes for each, and the mutant that must not verify
# (needs EverParse, whose F* and Z3 it runs, at most two at once).
# WIRE_FSTAR_CACHE holds the checked modules (proofs/fstar-gen/README.md).
prove:
	BUILD_EVERPARSE=1 dune build -j 2 @proofs/fstar-gen/prove

# The generated scanners, on OCaml and on OxCaml, and record modules
# executed symbolically, their reads proved in bounds and the records'
# answers equal to 3d.exe's specification by Z3, the planted mutants
# refused and every check of the scanners classified (needs EverParse and
# z3; verify/dune says what each rule proves). One rule at a time, as each
# runs four Z3 processes.
verify:
	BUILD_EVERPARSE=1 dune build -j 1 @verify/runtest @verify/classify

3d:
	dune exec examples/validate_3d.exe

# everparse-asn1 parses and elaborates every module of test/asn1: the tool is
# $(EVERPARSE_ASN1), else everparse-asn1.exe in PATH or ~/.local/everparse/bin.
asn1:
	dune build @test/asn1/asn1

bench: bench-demo bench-routing bench-gateway bench-clcw

bench-demo:
	BUILD_EVERPARSE=1 dune exec --profile=bench bench/demo/bench.exe

bench-routing:
	BUILD_EVERPARSE=1 dune exec --profile=bench bench/routing/bench.exe

bench-gateway:
	BUILD_EVERPARSE=1 dune exec --profile=bench bench/gateway/bench.exe

bench-clcw:
	BUILD_EVERPARSE=1 dune exec --profile=bench bench/clcw/bench.exe

# The compilers the cross-language benchmarks run the OCaml rows on besides
# the one on PATH, as name:switch: stock OCaml with flambda and OxCaml, each
# skipped when its opam switch is absent.
FLAMBDA_SWITCH ?= 5.4.1-flambda
OXCAML_SWITCH ?= 5.4.0-ox9
BENCH_SWITCHES = flambda:$(FLAMBDA_SWITCH) oxcaml:$(OXCAML_SWITCH)
# $(1) a harness, $(2) commands: builds the harness $(1) on each switch of
# BENCH_SWITCHES, into _build/NAME, then runs $(2) with $$n the switch's name
# and $$h the harness.
on_switches = for s in $(BENCH_SWITCHES); do n=$${s%%:*}; sw=$${s\#*:}; \
	  if opam switch list --short 2>/dev/null | grep -qx "$$sw"; then \
	    opam exec --switch "$$sw" -- dune build --profile=bench \
	      --build-dir $(CURDIR)/_build/$$n $(1) || exit 1; \
	    h=_build/$$n/default/$(1); $(2); \
	  else echo "switch $$sw not found: skipping $$n"; fi; done

# Ethernet/IPv4/TCP-or-UDP headers: Wire.Codec and wire.ocaml on OCaml, and on
# each switch of BENCH_SWITCHES, wire.rust against etherparse, smoltcp and
# pnet_packet, on each corpus of BENCH_RUST_CORPORA (name:stride, written by
# bench.exe corpus), and with BUILD_EVERPARSE=1 the EverParse C validator of
# the same codec. cargo fetches the crates the first time. Results land in
# _build/bench-rust/.
BENCH_RUST_PACKETS ?= 200000
BENCH_RUST_RUNS ?= 21
BENCH_RUST_CORPORA ?= fixed64-random:64 fixed64-sorted:64 \
                      mix128-random:128 mix128-sorted:128
BENCH_RUST = _build/bench-rust
# $(1) CORPUS STRIDE RUNS ROW, for each corpus and each row $(1) lists, each
# row in a process of its own (bench/rows.sh), appended to
# $(BENCH_RUST)/$(2).txt.
bench_rust_each = for c in $(BENCH_RUST_CORPORA); do \
	  sh bench/rows.sh $(1) $(BENCH_RUST)/$${c%:*}.bin $${c\#*:} \
	    $(BENCH_RUST_RUNS) >> $(BENCH_RUST)/$(2).txt || exit 1; done
bench-rust:
	@mkdir -p $(BENCH_RUST) bench/rust/cargo/src/gen
	dune build --profile=bench bench/rust/bench.exe bench/rust/gen.exe
	_build/default/bench/rust/gen.exe --rust bench/rust/cargo/src/gen
	_build/default/bench/rust/bench.exe corpus $(BENCH_RUST) \
	  $(BENCH_RUST_PACKETS)
	cargo build --quiet --release --manifest-path bench/rust/cargo/Cargo.toml
	rm -f $(BENCH_RUST)/*.txt
	$(call bench_rust_each,bench/rust/cargo/target/release/wire-bench-rust,rust)
	$(call bench_rust_each,_build/default/bench/rust/bench.exe run,ocaml)
	@$(call on_switches,bench/rust/bench.exe,$(call bench_rust_each,$$h run,$$n))
	@if [ "$(BUILD_EVERPARSE)" = 1 ]; then \
	  BUILD_EVERPARSE=1 dune build --profile=bench \
	    bench/rust/everparse/everparse.exe && \
	  $(call bench_rust_each,_build/default/bench/rust/everparse/everparse.exe,everparse); \
	else echo "BUILD_EVERPARSE unset: skipping the EverParse C rows"; fi
	@_build/default/bench/rust/bench.exe report $(BENCH_RUST)/*.txt

# A MAWI trace (samplepoint-F, 2025-11-15 14:00, 15 minutes), its first
# BENCH_FLOW_PACKETS packets held in memory, through a flow classifier on one
# core: Wire.Codec and wire.ocaml on OCaml and on each switch of
# BENCH_SWITCHES, wire.rust against etherparse, smoltcp and pnet_packet, and
# with BUILD_EVERPARSE=1 the EverParse C validator of the same codec; then
# each parse without the table (corpus mawi parse) and the table without a
# parse (corpus mawi table). The trace is downloaded into _build/bench-flow
# once and checked against its SHA-256; the packets are cut from it into
# _build/bench-flow/mawi.pcap.
BENCH_FLOW_URL = http://mawi.wide.ad.jp/mawi/samplepoint-F/2025/202511151400.pcap.gz
BENCH_FLOW_SHA256 = a99b91648ec296c1611558878f11828d2cb02d434e77f05153fa0020a6ce266c
BENCH_FLOW_PACKETS ?= 1000000
BENCH_FLOW_RUNS ?= 21
BENCH_FLOW = _build/bench-flow
BENCH_FLOW_TRACE = $(BENCH_FLOW)/202511151400.pcap.gz
BENCH_FLOW_PCAP = $(BENCH_FLOW)/mawi.pcap

$(BENCH_FLOW_TRACE):
	@mkdir -p $(BENCH_FLOW)
	curl -fL -o $@.part $(BENCH_FLOW_URL)
	@echo "$(BENCH_FLOW_SHA256)  $@.part" | shasum -a 256 -c - \
	  || { echo "$(BENCH_FLOW_URL): unexpected SHA-256" >&2; exit 1; }
	mv $@.part $@

# The harnesses built, the trace checked and its packets cut, for bench-flow
# and bench-flow-swap.
define bench_flow_prepare
	@mkdir -p bench/rust/cargo/src/gen
	dune build --profile=bench bench/rust/bench.exe bench/rust/gen.exe
	_build/default/bench/rust/gen.exe --rust bench/rust/cargo/src/gen
	@echo "$(BENCH_FLOW_SHA256)  $(BENCH_FLOW_TRACE)" | shasum -a 256 -c - >/dev/null \
	  || { echo "$(BENCH_FLOW_TRACE): unexpected SHA-256" >&2; exit 1; }
	gzip -dc < $(BENCH_FLOW_TRACE) | _build/default/bench/rust/bench.exe \
	  flow-extract $(BENCH_FLOW_PACKETS) $(BENCH_FLOW_PCAP)
	_build/default/bench/rust/bench.exe flow-mix $(BENCH_FLOW_PCAP)
	cargo build --quiet --release --manifest-path bench/rust/cargo/Cargo.toml
endef

bench-flow: $(BENCH_FLOW_TRACE)
	$(bench_flow_prepare)
	rm -f $(BENCH_FLOW)/*.txt
	sh bench/rows.sh bench/rust/cargo/target/release/wire-bench-rust flow \
	  $(BENCH_FLOW_PCAP) $(BENCH_FLOW_RUNS) >> $(BENCH_FLOW)/rust.txt
	sh bench/rows.sh bench/rust/cargo/target/release/wire-bench-flow-parse \
	  $(BENCH_FLOW_PCAP) $(BENCH_FLOW_RUNS) >> $(BENCH_FLOW)/rust.txt
	sh bench/rows.sh _build/default/bench/rust/bench.exe flow \
	  $(BENCH_FLOW_PCAP) $(BENCH_FLOW_RUNS) >> $(BENCH_FLOW)/ocaml.txt
	@$(call on_switches,bench/rust/bench.exe,sh bench/rows.sh $$h flow \
	  $(BENCH_FLOW_PCAP) $(BENCH_FLOW_RUNS) >> $(BENCH_FLOW)/$$n.txt)
	@if [ "$(BUILD_EVERPARSE)" = 1 ]; then \
	  BUILD_EVERPARSE=1 dune build --profile=bench \
	    bench/rust/everparse/everparse.exe && \
	  sh bench/rows.sh _build/default/bench/rust/everparse/everparse.exe flow \
	    $(BENCH_FLOW_PCAP) $(BENCH_FLOW_RUNS) >> $(BENCH_FLOW)/everparse.txt; \
	else echo "BUILD_EVERPARSE unset: skipping the EverParse C row"; fi
	@_build/default/bench/rust/bench.exe report $(BENCH_FLOW)/*.txt

# The swap rows of bench-flow alone: one flow counter (bench/rust/flow_swap.ml)
# written against the header modules wire.ocaml generates or their
# interpreted twins, on OCaml and on each switch of
# BENCH_SWITCHES, with smoltcp making the same checks, each with the table
# (corpus mawi swap) and without it (mawi swap parse). Each row is timed as
# in bench-flow, into _build/bench-flow/swap/, then counted by bench/count.sh,
# into _build/bench-flow/swap/counts.tsv: label, instructions and cycles per
# packet, and the row's result.
BENCH_FLOW_SWAP = $(BENCH_FLOW)/swap
# $(1) a harness and its arguments up to the capture, $(2) a label prefix,
# $(3) a results file: each swap row $(1) lists, timed into $(3), then
# counted.
bench_flow_swap_each = $(1) $(BENCH_FLOW_PCAP) 1 --list \
	  | grep -e '^swap=' -e ', IPv4 flows' | while IFS= read -r r; do \
	    $(1) $(BENCH_FLOW_PCAP) $(BENCH_FLOW_RUNS) "$$r" >> $(3) || exit 1; \
	    sh bench/count.sh $(BENCH_FLOW_PACKETS) "$(2)$$r" \
	      $(1) $(BENCH_FLOW_PCAP) RUNS "$$r" \
	      >> $(BENCH_FLOW_SWAP)/counts.tsv || exit 1; \
	  done

bench-flow-swap: $(BENCH_FLOW_TRACE)
	$(bench_flow_prepare)
	@mkdir -p $(BENCH_FLOW_SWAP)
	rm -f $(BENCH_FLOW_SWAP)/*.txt $(BENCH_FLOW_SWAP)/counts.tsv
	@$(call bench_flow_swap_each,bench/rust/cargo/target/release/wire-bench-rust flow,rust ,$(BENCH_FLOW_SWAP)/rust.txt)
	@$(call bench_flow_swap_each,bench/rust/cargo/target/release/wire-bench-flow-parse,rust ,$(BENCH_FLOW_SWAP)/rust.txt)
	@$(call bench_flow_swap_each,_build/default/bench/rust/bench.exe flow,ocaml ,$(BENCH_FLOW_SWAP)/ocaml.txt)
	@$(call on_switches,bench/rust/bench.exe,$(call bench_flow_swap_each,$$h flow,$$n ,$(BENCH_FLOW_SWAP)/$$n.txt))
	@_build/default/bench/rust/bench.exe report $(BENCH_FLOW_SWAP)/*.txt
	@cat $(BENCH_FLOW_SWAP)/counts.tsv

# The instructions and cycles each row of bench-rust spends per packet, as
# bench-apps-count counts them below, on each corpus of BENCH_RUST_CORPORA
# with BENCH_RUST_PACKETS packets, and with BUILD_EVERPARSE=1 the EverParse C
# rows too. Results in _build/bench-rust-count/counts.tsv: label, instructions
# and cycles per packet, and the row's checksum.
BENCH_RUST_COUNT_DIR = _build/bench-rust-count
# $(1) a harness and its arguments up to the corpus, $(2) a label prefix: each
# row it lists, on each corpus, appended to counts.tsv.
bench_rust_count_rows = for c in $(BENCH_RUST_CORPORA); do \
	  f=$(BENCH_RUST_COUNT_DIR)/$${c%:*}.bin; z=$${c\#*:}; \
	  $(1) $$f $$z 1 --list > $(BENCH_RUST_COUNT_DIR)/rows || exit 1; \
	  while IFS= read -r r; do \
	    sh bench/count.sh $(BENCH_RUST_PACKETS) "$(2)$${c%:*} $$r" \
	      $(1) $$f $$z RUNS "$$r" \
	      >> $(BENCH_RUST_COUNT_DIR)/counts.tsv || exit 1; \
	  done < $(BENCH_RUST_COUNT_DIR)/rows; \
	done
BENCH_RUST_BIN = bench/rust/cargo/target/release/wire-bench-rust

bench-rust-count:
	@mkdir -p $(BENCH_RUST_COUNT_DIR) bench/rust/cargo/src/gen
	dune build --profile=bench bench/rust/bench.exe bench/rust/gen.exe
	_build/default/bench/rust/gen.exe --rust bench/rust/cargo/src/gen
	_build/default/bench/rust/bench.exe corpus $(BENCH_RUST_COUNT_DIR) \
	  $(BENCH_RUST_PACKETS)
	cargo build --quiet --release --manifest-path bench/rust/cargo/Cargo.toml
	rm -f $(BENCH_RUST_COUNT_DIR)/counts.tsv
	@$(call bench_rust_count_rows,$(BENCH_RUST_BIN),)
	@$(call bench_rust_count_rows,_build/default/bench/rust/bench.exe run,)
	@$(call on_switches,bench/rust/bench.exe,$(call bench_rust_count_rows,$$h run,$$n ))
	@if [ "$(BUILD_EVERPARSE)" = 1 ]; then \
	  BUILD_EVERPARSE=1 dune build --profile=bench \
	    bench/rust/everparse/everparse.exe && \
	  $(call bench_rust_count_rows,_build/default/bench/rust/everparse/everparse.exe,); \
	else echo "BUILD_EVERPARSE unset: skipping the EverParse C rows"; fi
	@cat $(BENCH_RUST_COUNT_DIR)/counts.tsv

# The application benchmarks (bench/routing, bench/gateway, bench/clcw) across
# languages: each OCaml workload, and the same application logic over the
# OCaml Wire_ocaml generates, on OCaml and on each switch of BENCH_SWITCHES,
# against that logic over the Rust Wire_rust generates
# from the same codecs and over the released crates that read and write the
# same headers (spacepackets, ccsds_primary_header, gaia-ccsds-c2a,
# chronus-gateway), on the inputs their generators write; cargo fetches the
# crates the first time. Each workload runs reading, then writing. Their
# EverParse C rows stay in bench-routing, bench-gateway and bench-clcw.
# Results land in _build/bench-apps/.
BENCH_APPS_PACKETS ?= 10000000
BENCH_APPS_FRAMES ?= 1000000
BENCH_APPS_WORDS ?= 1000000
BENCH_APPS_RUNS ?= 21
BENCH_APPS = _build/bench-apps
# $(1) FILE ITEMS RUNS ROW, for each workload and each row $(1) lists, each row
# in a process of its own (bench/rows.sh), appended to $(BENCH_APPS)/$(2).txt.
bench_apps_each = for w in routing:$(BENCH_APPS_PACKETS) \
	  gateway:$(BENCH_APPS_FRAMES) clcw:$(BENCH_APPS_WORDS); do \
	  sh bench/rows.sh $(1) $(BENCH_APPS)/$${w%:*}.bin $${w\#*:} \
	    $(BENCH_APPS_RUNS) >> $(BENCH_APPS)/$(2).txt || exit 1; done
# The instructions and cycles each row of bench-apps-rust spends per item, on
# inputs of BENCH_APPS_COUNT items, for every row each harness lists:
# bench/count.sh runs the row with 1 and with 21 timed passes, each after its
# warm-up pass, and divides the difference by 20 passes, so starting the
# process, reading the input and printing the result never count. Results in
# _build/bench-apps-count/counts.tsv: label, instructions and cycles per item,
# and the row's result.
BENCH_APPS_COUNT ?= 100000
BENCH_APPS_COUNT_DIR = _build/bench-apps-count
bench_count = sh bench/count.sh $(BENCH_APPS_COUNT)
# $(1) a harness, $(2) a label prefix: each row it lists, on each workload,
# reading and writing, appended to counts.tsv.
bench_count_rows = for w in routing gateway clcw; do \
	  f=$(BENCH_APPS_COUNT_DIR)/$$w.bin; \
	  for m in run write; do \
	    $(1) $$m $$f $(BENCH_APPS_COUNT) 1 --list \
	      > $(BENCH_APPS_COUNT_DIR)/rows || exit 1; \
	    while IFS= read -r r; do \
	      $(bench_count) "$(2)$$w $$m $$r" $(1) $$m $$f $(BENCH_APPS_COUNT) \
	        RUNS "$$r" >> $(BENCH_APPS_COUNT_DIR)/counts.tsv || exit 1; \
	    done < $(BENCH_APPS_COUNT_DIR)/rows; \
	  done; \
	done
BENCH_APPS_RUST = bench/apps/cargo/target/release/wire-bench-apps

bench-apps-count:
	@mkdir -p $(BENCH_APPS_COUNT_DIR) bench/apps/cargo/src/gen
	dune build --profile=bench bench/apps/bench.exe bench/apps/gen.exe
	_build/default/bench/apps/gen.exe --rust bench/apps/cargo/src/gen
	_build/default/bench/apps/bench.exe inputs $(BENCH_APPS_COUNT_DIR) \
	  $(BENCH_APPS_COUNT) $(BENCH_APPS_COUNT) $(BENCH_APPS_COUNT)
	cargo build --quiet --release --manifest-path bench/apps/cargo/Cargo.toml
	rm -f $(BENCH_APPS_COUNT_DIR)/counts.tsv
	@$(call bench_count_rows,$(BENCH_APPS_RUST),)
	@$(call bench_count_rows,_build/default/bench/apps/bench.exe,)
	@$(call on_switches,bench/apps/bench.exe,$(call bench_count_rows,$$h,$$n ))
	@cat $(BENCH_APPS_COUNT_DIR)/counts.tsv

bench-apps-rust:
	@mkdir -p $(BENCH_APPS) bench/apps/cargo/src/gen
	dune build --profile=bench bench/apps/bench.exe bench/apps/gen.exe
	_build/default/bench/apps/gen.exe --rust bench/apps/cargo/src/gen
	_build/default/bench/apps/bench.exe inputs $(BENCH_APPS) \
	  $(BENCH_APPS_PACKETS) $(BENCH_APPS_FRAMES) $(BENCH_APPS_WORDS)
	cargo build --quiet --release --manifest-path bench/apps/cargo/Cargo.toml
	rm -f $(BENCH_APPS)/*.txt
	$(call bench_apps_each,$(BENCH_APPS_RUST) run,rust)
	$(call bench_apps_each,$(BENCH_APPS_RUST) write,rust)
	$(call bench_apps_each,_build/default/bench/apps/bench.exe run,ocaml)
	$(call bench_apps_each,_build/default/bench/apps/bench.exe write,ocaml)
	@$(call on_switches,bench/apps/bench.exe,$(call bench_apps_each,$$h run,$$n) && \
	  $(call bench_apps_each,$$h write,$$n))
	@_build/default/bench/apps/bench.exe report $(BENCH_APPS)/*.txt

PROF_EXE ?= bench/clcw/bench.exe

prof:
	BUILD_EVERPARSE=1 dune build --profile=bench $(PROF_EXE)
	xctrace record --template 'Time Profiler' --output prof.trace \
		--launch -- _build/default/$(PROF_EXE)
	@echo "Profile written to prof.trace — open with: open prof.trace"

memtrace: memtrace-routing memtrace-gateway memtrace-clcw

memtrace-demo:
	BUILD_EVERPARSE=1 MEMTRACE=demo.ctf dune exec --profile=bench bench/demo/bench.exe
	memtrace_hotspots demo.ctf

memtrace-routing:
	BUILD_EVERPARSE=1 MEMTRACE=routing.ctf dune exec --profile=bench bench/routing/bench.exe
	memtrace_hotspots routing.ctf

memtrace-gateway:
	BUILD_EVERPARSE=1 MEMTRACE=gateway.ctf dune exec --profile=bench bench/gateway/bench.exe
	memtrace_hotspots gateway.ctf

memtrace-clcw:
	BUILD_EVERPARSE=1 MEMTRACE=clcw.ctf dune exec --profile=bench bench/clcw/bench.exe
	memtrace_hotspots clcw.ctf

# Static analysis over the project's hand-written C: the benchmark application
# loops and the header they share. EverParse proves the parsers it generates;
# nothing covered the C written by hand around them. The file list comes from
# git rather than a literal so a new hand-written file cannot escape it, and
# every tracked .c/.h is hand-written, as the generated parsers are produced
# into _build and never committed. -I bench resolves bench_common.h, so a
# finding inside it surfaces too. The headers that stay unresolved are external
# (OCaml's caml/*, EverParse's generated schemas): pointing cppcheck at OCaml's
# headers multiplies the #ifdef configuration space by sixty and finds nothing.
# unusedFunction fires on every CAMLprim, whose only caller is the OCaml
# runtime. cppcheck prints the name of each file it opens, which is the proof
# that the list reached it.
cppcheck:
	@if ! command -v cppcheck >/dev/null 2>&1; then \
	  echo "cppcheck not found: skipping (brew install cppcheck)"; \
	  exit 0; \
	fi; \
	files=$$(git ls-files '*.c' '*.h'); \
	if [ -z "$$files" ]; then \
	  echo "error: found no hand-written C to check"; exit 1; \
	fi; \
	cppcheck --enable=all --check-level=exhaustive --inconclusive \
	  --std=c11 --language=c -I bench \
	  --suppress=missingIncludeSystem --suppress=missingInclude \
	  --suppress=unusedFunction --suppress=checkersReport \
	  --suppress=normalCheckLevelMaxBranches --suppress=unmatchedSuppression \
	  --inline-suppr --error-exitcode=1 $$files

clean:
	dune clean
