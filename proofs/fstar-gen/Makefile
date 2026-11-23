# Spike: prove the scanner Wire_ocaml writes for a codec equal to the parser
# 3d.exe writes for the same codec's .3d. Outside the dune build.
#
#   make OUT=dir        3d.exe on each codec's .3d (gen/gen.exe), then F*
#
# Needs EverParse (3d.exe, fstar.exe, z3) from v2026.10.27 in EVERPARSE.

EVERPARSE ?= $(HOME)/.local/everparse
OUT ?= $(or $(TMPDIR),/tmp)/wire-fstar-gen
ROOT := $(abspath ../..)
CODECS = Tiny Ethernet IPv4
LIB = ../fstar/Plan.Word.fst WireGen.Lib.fst WireGen.Lib3d.fst WireGen.OCaml.fst WireGen.Tables.fst WireGen.Tac.fst

.PHONY: all $(CODECS)
all: $(CODECS)

$(OUT)/gen/%.3d:
	dune build --root $(ROOT) ./proofs/fstar-gen/gen/gen.exe
	mkdir -p $(OUT)/gen
	$(ROOT)/_build/default/proofs/fstar-gen/gen/gen.exe $(OUT)/gen

# 3d.exe --batch writes and checks <Name>.fst, the parser the proof is about.
$(OUT)/%.3d.done: $(OUT)/gen/%.3d
	mkdir -p $(OUT)/$*
	cp $< $(OUT)/$*/
	$(EVERPARSE)/bin/3d.exe --batch --odir $(OUT)/$* $(OUT)/$*/$*.3d > $(OUT)/$*/3d.log 2>&1
	touch $@

$(CODECS): %: $(OUT)/%.3d.done
	@for m in $(LIB) $*.Scan.fst $*.Flat.fst $*.Proof.fst; do \
	  echo "$$m"; \
	  python3 -I ./measure.py ./fstar.sh $(OUT)/$* $$m 2>&1 | \
	    grep -E '^\* Error|wall' || exit 1; \
	done
