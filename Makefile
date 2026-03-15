.PHONY: build test bench bench-sample memtrace clean

build:
	dune build

test:
	dune runtest

bench:
	dune exec bench/bench_alloc.exe

bench-everparse:
	BUILD_EVERPARSE=1 dune exec bench/bench_perf.exe

bench-sample:
	BUILD_EVERPARSE=1 dune build bench/bench_perf.exe
	@echo "Starting bench (50M iters), sampling for 5s..."
	@_build/default/bench/bench_perf.exe 50000000 & \
	  PID=$$!; \
	  sleep 1; \
	  sudo sample $$PID 5 -file /tmp/bench_sample.txt; \
	  wait $$PID; \
	  echo "Sample saved to /tmp/bench_sample.txt"

memtrace:
	MEMTRACE=trace.ctf dune exec bench/bench_wire_memtrace.exe
	memtrace_hotspots trace.ctf

clean:
	dune clean
