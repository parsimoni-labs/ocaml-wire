# Verify the F* model of the scanner planner with the F* and Z3 that ship
# with EverParse. Nothing in the dune build depends on this directory.
#
#   make verify    the model and its theorems
#   make defects   each past defect's buggy replay fails, its fix verifies;
#                  each mutant of the model fails (slow: use -j)

EVERPARSE ?= $(HOME)/.local/everparse
FSTAR ?= $(EVERPARSE)/bin/fstar.exe
Z3 ?= $(EVERPARSE)/lib/fstar/z3-4.13.3/bin/z3
CACHE ?= $(or $(TMPDIR),/tmp)/wire-fstar-cache

LOWPARSE ?= $(EVERPARSE)/src/lowparse

FLAGS = --z3version 4.13.3 --smt $(Z3) --already_cached 'Prims FStar LowParse' \
  --include $(LOWPARSE)
FSTAR_FLAGS = $(FLAGS) --cache_checked_modules --cache_dir $(CACHE) --include .

MODEL = Plan.Expr.fst Plan.Sum.fst Plan.Scan.fst Plan.Proof.fst
MODULES = $(MODEL) Plan.Word.fst Plan.Frames.fst Spec.Ipv4.fst

# defects/Defect.<name>.Buggy.fst fails, defects/Defect.<name>.Fixed.fst
# verifies.
DEFECTS = SpanWrap ConstraintWrap WordUnchecked RegionWalk RepeatBudget SumConst

# defects/<name>.sed rewrites the module its "# mutates:" line names, and
# the module its "# fails:" line names then fails to verify.
MUTANTS = fields-after-span o1-drop-negative o1-drop-wrap o1-drop-unchecked \
  frames-quad-overlap frames-quad-guard

.PHONY: verify defects clean $(DEFECTS:%=defect-%) $(MUTANTS:%=mutant-%)

# One module at a time, in dependency order, so each one's checked file is
# written before the next reads it.
verify:
	@mkdir -p $(CACHE)
	@for m in $(MODULES); do \
	  $(FSTAR) $(FSTAR_FLAGS) $$m || exit 1; \
	done

defects: $(DEFECTS:%=defect-%) $(MUTANTS:%=mutant-%)

$(DEFECTS:%=defect-%): defect-%:
	@mkdir -p $(CACHE)
	@out=$$($(FSTAR) $(FSTAR_FLAGS) defects/Defect.$*.Fixed.fst 2>&1) || \
	  { echo "$$out"; echo "FAIL $*: the fixed replay does not verify"; exit 1; }
	@out=$$($(FSTAR) $(FSTAR_FLAGS) defects/Defect.$*.Buggy.fst 2>&1); \
	  if [ $$? -eq 0 ]; then echo "FAIL $*: the buggy replay verifies"; exit 1; fi; \
	  echo "$$out" | grep -q '^1 error was reported' || \
	    { echo "$$out"; echo "FAIL $*: not the one expected error"; exit 1; }; \
	  echo "caught $*: $$(echo "$$out" | grep -m1 -A1 '^\* Error' | tr -s ' \n' ' ')"

$(MUTANTS:%=mutant-%): mutant-%:
	@d=$(CACHE)/mutant-$*; s=defects/$*.sed; \
	  f=$$(sed -n 's/^# mutates: //p' $$s); c=$$(sed -n 's/^# fails: //p' $$s); \
	  rm -rf $$d; mkdir -p $$d; cp $(MODULES) $$d/; sed -f $$s $$f > $$d/$$f; \
	  if cmp -s $$f $$d/$$f; then echo "FAIL $*: the mutation does not apply"; exit 1; fi; \
	  out=$$(cd $$d && $(FSTAR) $(FLAGS) $$c 2>&1); \
	  if [ $$? -eq 0 ]; then echo "FAIL $*: the mutant verifies"; exit 1; fi; \
	  echo "caught $*: $$(echo "$$out" | grep -m1 -A1 '^\* Error' | sed "s|$$d/||" | tr -s ' \n' ' ')"

clean:
	rm -rf $(CACHE)
