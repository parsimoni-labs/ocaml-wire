EVERPARSE ?= $(HOME)/.local/everparse
FSTAR ?= $(EVERPARSE)/bin/fstar.exe
Z3 ?= $(EVERPARSE)/lib/fstar/z3-4.13.3/bin/z3
CACHE ?= $(or $(TMPDIR),/tmp)/wire-fstar-cache

FSTAR_FLAGS = --z3version 4.13.3 --smt $(Z3) --cache_checked_modules \
  --cache_dir $(CACHE) --already_cached 'Prims FStar'

MODULES = Plan.Expr.fst Plan.Sum.fst Plan.Scan.fst Plan.Proof.fst

.PHONY: verify clean

verify:
	@mkdir -p $(CACHE)
	$(FSTAR) $(FSTAR_FLAGS) $(MODULES)

clean:
	rm -rf $(CACHE)
