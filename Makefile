.PHONY: build test test-wasm 3d bench bench-demo bench-routing bench-gateway bench-clcw \
       prof memtrace memtrace-demo memtrace-routing memtrace-gateway memtrace-clcw \
       cppcheck clean

build:
	dune build

test:
	dune runtest

# Runs test/wasm under node with wasm_of_ocaml (31-bit ints), then fails on
# any integer-overflow truncation warning from wire's own code. optint's two
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
	dune build @test/wasm/runtest 2> _build/wasm-build.log; \
	status=$$?; cat _build/wasm-build.log >&2; \
	test $$status -eq 0 || exit $$status; \
	! grep "integer-overflow" _build/wasm-build.log \
	  | grep -v -e 0x7fffffff -e 0x40000000 | grep -q . \
	  || { echo "error: wasm_of_ocaml truncated an integer literal"; exit 1; }

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
