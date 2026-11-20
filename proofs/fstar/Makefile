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
MODULES = $(MODEL) Plan.Word.fst Plan.Frames.fst Spec.Ipv4.fst Plan.Decide.fst

# defects/Defect.<name>.Buggy.fst fails, defects/Defect.<name>.Fixed.fst
# verifies.
DEFECTS = SpanWrap ConstraintWrap WordUnchecked RegionWalk RepeatBudget SumConst


# defects/<name>.sed rewrites the module its "# mutates:" line names, and
# the module its "# fails:" line names then fails to verify.
MUTANTS = fields-after-span o1-drop-negative o1-drop-wrap o1-drop-unchecked \
  frames-quad-overlap frames-quad-guard word-lor-overlap word-mask-dropped \
  prefix-wrap region-bounded-leak region-unchecked region-inexact

.PHONY: verify defects plans extract check-extract clean $(DEFECTS:%=defect-%) $(MUTANTS:%=mutant-%)

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
	  t=$$(sed -n 's/^# witness: //p' $$s); \
	  if [ -n "$$t" ]; then cp defects/$$t $$d/; \
	    wout=$$(cd $$d && $(FSTAR) $(FLAGS) $$t 2>&1) || \
	      { echo "$$wout"; echo "FAIL $*: the witness does not hold of the mutant"; exit 1; }; fi; \
	  echo "caught $*: $$(echo "$$out" | grep -m1 -A1 '^\* Error' | sed "s|$$d/||" | tr -s ' \n' ' ')"

# The planner's plans of the sampled codecs, the structured scan cases, the
# packet stack and the PrefixWrap codec meet what Plan.Proof assumes of a
# plan (F* checks wf), and every bound decision Wire_ocaml.Plan.spans
# reports the emitter took is one the model's walk takes (Plan.Decide as F*
# extracts it). The control claims every bound is left to the next check,
# and must be refused.
ROOT = $(abspath ../..)
PLANS = $(ROOT)/_build/default/proofs/fstar/plans/plans.exe

plans:
	@mkdir -p $(CACHE)/plans
	dune build --root $(ROOT) ./proofs/fstar/plans/plans.exe
	@$(PLANS) Plans > $(CACHE)/plans/Plans.fst 2> $(CACHE)/plans/decisions.txt; \
	  cat $(CACHE)/plans/decisions.txt; \
	  grep -q ' 0 plans with one the model does not justify' $(CACHE)/plans/decisions.txt || \
	    { echo "FAIL: an emitter decision the model does not justify"; exit 1; }
	$(FSTAR) $(FSTAR_FLAGS) $(CACHE)/plans/Plans.fst
	@$(PLANS) Control control > /dev/null 2> $(CACHE)/plans/control.txt; \
	  if grep -q ' 0 plans with one the model does not justify' $(CACHE)/plans/control.txt; then \
	    echo "FAIL: the control's fabricated decisions are justified"; exit 1; \
	  else echo "control refused: $$(tail -1 $(CACHE)/plans/control.txt)"; fi

# The model as F* extracts it, which plans/plans.exe runs: extract writes it
# again, check-extract fails when the checked-in copy differs.
EXTRACTED = Plan.Expr Plan.Sum Plan.Scan Plan.Decide

extract:
	@mkdir -p $(CACHE)/extract
	$(FSTAR) $(FSTAR_FLAGS) --codegen OCaml --extract '$(EXTRACTED)' \
	  --odir $(CACHE)/extract Plan.Decide.fst
	cp $(CACHE)/extract/Plan_*.ml plans/extracted/

check-extract:
	@mkdir -p $(CACHE)/extract
	@$(FSTAR) $(FSTAR_FLAGS) --codegen OCaml --extract '$(EXTRACTED)' \
	  --odir $(CACHE)/extract Plan.Decide.fst > /dev/null
	@for f in $(CACHE)/extract/Plan_*.ml; do \
	  diff -q $$f plans/extracted/$$(basename $$f) || exit 1; done
	@echo "plans/extracted matches the model"

clean:
	rm -rf $(CACHE)
