.PHONY: build test bench prof memtrace clean

build:
	dune build

test:
	dune runtest

bench:
	BUILD_EVERPARSE=1 dune exec bench/bench.exe

prof:
	dune build bench/memtrace.exe
	xctrace record --template 'Time Profiler' --output prof.trace \
		--launch -- _build/default/bench/memtrace.exe
	@echo "Profile written to prof.trace — open with: open prof.trace"

memtrace:
	MEMTRACE=trace.ctf dune exec bench/memtrace.exe
	memtrace_hotspots trace.ctf

clean:
	dune clean
