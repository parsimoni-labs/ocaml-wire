.PHONY: build test test-wasm test-oxcaml fuzz fuzz-paranoid fuzz-afl fuzz-afl-diff \
       fuzz-nightly fuzz-rust 3d bench bench-demo bench-routing bench-gateway \
       bench-clcw bench-rust \
       prof memtrace memtrace-demo memtrace-routing memtrace-gateway memtrace-clcw \
       cppcheck clean

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
# the test dependencies (alcobar included). The release profile, because the
# dev one compiles libraries -opaque and the checks cannot pass there, then the
# paranoid one, which checks every unchecked load as well.
OXCAML_LANES = @test/ocaml-ox/runtest @fuzz/diff/ocaml-ox/runtest \
  @test/runtime-ox/runtest
test-oxcaml:
	@test "$$(ocamlopt -config-var ox)" = true || \
	  { echo "not an OxCaml compiler"; exit 1; }
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

3d:
	dune exec examples/validate_3d.exe

bench: bench-demo bench-routing bench-gateway bench-clcw

bench-demo:
	BUILD_EVERPARSE=1 dune exec --profile=release bench/demo/bench.exe

bench-routing:
	BUILD_EVERPARSE=1 dune exec --profile=release bench/routing/bench.exe

bench-gateway:
	BUILD_EVERPARSE=1 dune exec --profile=release bench/gateway/bench.exe

bench-clcw:
	BUILD_EVERPARSE=1 dune exec --profile=release bench/clcw/bench.exe

# Ethernet/IPv4/TCP-or-UDP headers: Wire.Codec and wire.ocaml on OCaml (and on
# OxCaml when the switch OXCAML_SWITCH exists), wire.rust against etherparse
# and smoltcp. cargo fetches the two crates the first time. Results land in
# _build/bench-rust/.
OXCAML_SWITCH ?= 5.4.0-ox9
BENCH_RUST_PACKETS ?= 200000
BENCH_RUST_RUNS ?= 21
BENCH_RUST = _build/bench-rust
bench-rust:
	@mkdir -p $(BENCH_RUST) bench/rust/cargo/src/gen
	dune build --profile=release bench/rust/bench.exe bench/rust/gen.exe
	_build/default/bench/rust/gen.exe --rust bench/rust/cargo/src/gen
	_build/default/bench/rust/bench.exe corpus $(BENCH_RUST)/corpus.bin \
	  $(BENCH_RUST_PACKETS)
	cargo run --quiet --release --manifest-path bench/rust/cargo/Cargo.toml \
	  -- $(BENCH_RUST)/corpus.bin 64 $(BENCH_RUST_RUNS) > $(BENCH_RUST)/rust.txt
	_build/default/bench/rust/bench.exe run $(BENCH_RUST)/corpus.bin \
	  $(BENCH_RUST_RUNS) > $(BENCH_RUST)/ocaml.txt
	@if opam switch list --short 2>/dev/null | grep -qx '$(OXCAML_SWITCH)'; then \
	  opam exec --switch $(OXCAML_SWITCH) -- dune build --profile=release \
	    --build-dir $(CURDIR)/_build/oxcaml bench/rust/bench.exe && \
	  _build/oxcaml/default/bench/rust/bench.exe run $(BENCH_RUST)/corpus.bin \
	    $(BENCH_RUST_RUNS) > $(BENCH_RUST)/oxcaml.txt; \
	else rm -f $(BENCH_RUST)/oxcaml.txt; \
	  echo "OxCaml switch $(OXCAML_SWITCH) not found: skipping OxCaml"; fi
	@_build/default/bench/rust/bench.exe report $(BENCH_RUST)/*.txt

PROF_EXE ?= bench/clcw/bench.exe

prof:
	BUILD_EVERPARSE=1 dune build --profile=release $(PROF_EXE)
	xctrace record --template 'Time Profiler' --output prof.trace \
		--launch -- _build/default/$(PROF_EXE)
	@echo "Profile written to prof.trace — open with: open prof.trace"

memtrace: memtrace-routing memtrace-gateway memtrace-clcw

memtrace-demo:
	BUILD_EVERPARSE=1 MEMTRACE=demo.ctf dune exec --profile=release bench/demo/bench.exe
	memtrace_hotspots demo.ctf

memtrace-routing:
	BUILD_EVERPARSE=1 MEMTRACE=routing.ctf dune exec --profile=release bench/routing/bench.exe
	memtrace_hotspots routing.ctf

memtrace-gateway:
	BUILD_EVERPARSE=1 MEMTRACE=gateway.ctf dune exec --profile=release bench/gateway/bench.exe
	memtrace_hotspots gateway.ctf

memtrace-clcw:
	BUILD_EVERPARSE=1 MEMTRACE=clcw.ctf dune exec --profile=release bench/clcw/bench.exe
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
