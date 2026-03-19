.PHONY: build test bench bench-demo bench-routing bench-gateway bench-clcw prof memtrace clean

build:
	dune build

test:
	dune runtest

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
	dune build --profile=release $(PROF_EXE)
	xctrace record --template 'Time Profiler' --output prof.trace \
		--launch -- _build/default/$(PROF_EXE)
	@echo "Profile written to prof.trace — open with: open prof.trace"

memtrace:
	MEMTRACE=trace.ctf dune exec --profile=release bench/memtrace/bench.exe
	memtrace_hotspots trace.ctf

clean:
	dune clean
