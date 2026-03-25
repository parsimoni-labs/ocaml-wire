.PHONY: build test bench bench-demo bench-routing bench-gateway bench-clcw \
       prof memtrace memtrace-demo memtrace-routing memtrace-gateway memtrace-clcw clean

build:
	dune build

test:
	dune runtest
	@if [ -x "$$HOME/.local/everparse/bin/3d.exe" ] || command -v 3d.exe >/dev/null 2>&1; then \
		PATH="$$HOME/.local/everparse/bin:$$PATH" BUILD_EVERPARSE=1 dune runtest; \
	else \
		echo "Skipping EverParse-gated tests (3d.exe not found)"; \
	fi

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

memtrace: memtrace-routing memtrace-gateway memtrace-clcw

memtrace-demo:
	BUILD_EVERPARSE=1 MEMTRACE=demo.ctf dune exec --profile=release bench/demo/bench.exe
	memtrace_hotspots demo.ctf

memtrace-routing:
	MEMTRACE=routing.ctf dune exec --profile=release bench/routing/bench.exe
	memtrace_hotspots routing.ctf

memtrace-gateway:
	MEMTRACE=gateway.ctf dune exec --profile=release bench/gateway/bench.exe
	memtrace_hotspots gateway.ctf

memtrace-clcw:
	MEMTRACE=clcw.ctf dune exec --profile=release bench/clcw/bench.exe
	memtrace_hotspots clcw.ctf

clean:
	dune clean
