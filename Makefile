.PHONY: build test bench bench-routing bench-gateway bench-clcw prof memtrace clean

build:
	dune build

test:
	dune runtest

bench:
	BUILD_EVERPARSE=1 dune exec --profile=release bench/bench.exe

bench-routing:
	dune exec --profile=release bench/bench_routing.exe

bench-gateway:
	dune exec --profile=release bench/bench_gateway.exe

bench-clcw:
	dune exec --profile=release bench/bench_clcw.exe

PROF_EXE ?= bench/bench_clcw.exe

prof:
	dune build --profile=release $(PROF_EXE)
	xctrace record --template 'Time Profiler' --output prof.trace \
		--launch -- _build/default/$(PROF_EXE)
	@echo "Profile written to prof.trace — open with: open prof.trace"

memtrace:
	MEMTRACE=trace.ctf dune exec --profile=release bench/memtrace.exe
	memtrace_hotspots trace.ctf

clean:
	dune clean
