.PHONY: build test test-wasm 3d bench bench-demo bench-routing bench-gateway bench-clcw \
       prof memtrace memtrace-demo memtrace-routing memtrace-gateway memtrace-clcw clean

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

clean:
	dune clean
