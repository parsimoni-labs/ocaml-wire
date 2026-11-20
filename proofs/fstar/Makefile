# Verify the F* model of the scanner planner with the F* and Z3 that ship
# with EverParse. Nothing in the dune build depends on this directory.
#
#   make verify    the model and its theorems

EVERPARSE ?= $(HOME)/.local/everparse
FSTAR ?= $(EVERPARSE)/bin/fstar.exe
Z3 ?= $(EVERPARSE)/lib/fstar/z3-4.13.3/bin/z3
CACHE ?= $(or $(TMPDIR),/tmp)/wire-fstar-cache

FLAGS = --z3version 4.13.3 --smt $(Z3) --already_cached 'Prims FStar'
FSTAR_FLAGS = $(FLAGS) --cache_checked_modules --cache_dir $(CACHE) --include .

MODEL = Plan.Expr.fst Plan.Sum.fst Plan.Scan.fst Plan.Proof.fst
MODULES = $(MODEL) Plan.Word.fst Plan.Frames.fst

.PHONY: verify clean

# One module at a time, in dependency order, so each one's checked file is
# written before the next reads it.
verify:
	@mkdir -p $(CACHE)
	@for m in $(MODULES); do \
	  $(FSTAR) $(FSTAR_FLAGS) $$m || exit 1; \
	done

clean:
	rm -rf $(CACHE)
