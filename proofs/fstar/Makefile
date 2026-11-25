# Verify the F* model of the scanner planner with the F* and Z3 that ship
# with EverParse. Nothing in the dune build depends on this directory.
#
#   make verify    the model and its theorems
#   make defects   each past defect's buggy replay fails, its fix verifies;
#                  each mutant of the model fails (slow: use -j)

EVERPARSE ?= $(HOME)/.local/everparse
FSTAR ?= $(EVERPARSE)/bin/fstar.exe

# EverParse from v2026.10.27 (F* 2026.10.25~dev): the Z3 its F* ships
# with, and LowParse.
Z3DIR := $(lastword $(wildcard $(EVERPARSE)/lib/fstar/z3-*))
Z3VERSION ?= $(patsubst z3-%,%,$(notdir $(Z3DIR)))
Z3 ?= $(Z3DIR)/bin/z3
LOWPARSE ?= $(EVERPARSE)/lib/everparse/lowparse

# Checked files are F*'s own: one cache per F*.
FSTAR_VERSION := $(shell $(FSTAR) --version 2>/dev/null | sed -n 's/^F\* //p')
CACHE ?= $(or $(TMPDIR),/tmp)/wire-fstar-cache/$(FSTAR_VERSION)

FLAGS = --z3version $(Z3VERSION) --smt $(Z3) --already_cached 'Prims FStar LowParse' \
  --include $(LOWPARSE)
# F* takes a module's name from its file's path, so each runs from the
# directory that holds it.
CACHED = --cache_checked_modules --cache_dir $(CACHE)
FSTAR_FLAGS = $(FLAGS) $(CACHED) --include .

MODEL = Plan.Expr.fst Plan.Sum.fst Plan.Scan.fst Plan.Proof.fst
MODULES = $(MODEL) Plan.Word.fst Plan.Bytes.fst Plan.Frames.fst Spec.Ipv4.fst Plan.Tree.fst \
  Plan.Range.fst Plan.Decide.fst Plan.Loop.fst Plan.Join.fst \
  Plan.Values.fst Plan.Index.fst

# defects/Defect.<name>.Buggy.fst fails on one line (F* may report a
# conjunction there as several errors), defects/Defect.<name>.Fixed.fst
# verifies.
DEFECTS = SpanWrap ConstraintWrap WordUnchecked RegionWalk RepeatBudget SumConst


# defects/<name>.sed rewrites the module its "# mutates:" line names, and
# the module its "# fails:" line names then fails to verify.
MUTANTS = fields-after-span o1-drop-negative o1-drop-wrap o1-drop-unchecked \
  frames-quad-overlap frames-quad-guard word-lor-overlap word-mask-dropped \
  prefix-wrap region-bounded-leak region-unchecked region-inexact \
  short-known ahead-short tree-retest tree-case-first \
  range-top-bits join-known sign-shift \
  word-past-check word-before-window held-leak \
  region-known-high region-end-unpinned region-start-off \
  index-ahead-untested index-ahead-past index-known-short

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
	@out=$$(cd defects && $(FSTAR) $(FLAGS) $(CACHED) --include .. Defect.$*.Fixed.fst 2>&1) || \
	  { echo "$$out"; echo "FAIL $*: the fixed replay does not verify"; exit 1; }
	@out=$$(cd defects && $(FSTAR) $(FLAGS) $(CACHED) --include .. Defect.$*.Buggy.fst 2>&1); \
	  if [ $$? -eq 0 ]; then echo "FAIL $*: the buggy replay verifies"; exit 1; fi; \
	  n=$$(echo "$$out" | sed -n 's/^\* Error [0-9]* at \([^(]*([0-9]*\),.*/\1/p' | sort -u | wc -l); \
	  [ $$n -eq 1 ] || \
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
	  [ $$(grep -c ' 0 plans with one the model does not justify' $(CACHE)/plans/decisions.txt) -eq 3 ] || \
	    { echo "FAIL: an emitter decision the model does not justify"; exit 1; }; \
	  grep -q ' 0 held across windows' $(CACHE)/plans/decisions.txt || \
	    { echo "FAIL: a word held across windows"; exit 1; }; \
	  grep -q ' 0 not well formed' $(CACHE)/plans/decisions.txt || \
	    { echo "FAIL: a plan is not what Plan.Proof assumes"; exit 1; }
	cd $(CACHE)/plans && $(FSTAR) $(FLAGS) $(CACHED) --include $(CURDIR) Plans.fst
	@$(PLANS) Control control > /dev/null 2> $(CACHE)/plans/control.txt; \
	  if grep -q ' 0 plans with one the model does not justify' $(CACHE)/plans/control.txt; then \
	    echo "FAIL: the control's fabricated decisions are justified"; exit 1; \
	  else echo "control refused: $$(grep -E '^(decisions|checks|words):' $(CACHE)/plans/control.txt | tr '\n' ' ')"; fi

# The model as F* extracts it, which plans/plans.exe runs: extract writes it
# again, check-extract fails when the checked-in copy differs.
# What plans.exe runs: Plan.Decide, Plan.Scan.wf and what they reach, one
# OCaml file per module.
EXTRACT = --codegen OCaml --custard_entry_module Plan.Decide --custard_entry Plan.Scan.wf \
  --custard_split

# Custard extracts from checked files only.
extract:
	@mkdir -p $(CACHE)/extract
	$(FSTAR) $(FSTAR_FLAGS) Plan.Decide.fst
	$(FSTAR) $(FSTAR_FLAGS) $(EXTRACT) --odir $(CACHE)/extract Plan.Decide.fst
	cp $(CACHE)/extract/*.ml plans/extracted/

check-extract:
	@mkdir -p $(CACHE)/extract
	@$(FSTAR) $(FSTAR_FLAGS) Plan.Decide.fst > /dev/null
	@$(FSTAR) $(FSTAR_FLAGS) $(EXTRACT) --odir $(CACHE)/extract Plan.Decide.fst > /dev/null
	@for f in $(CACHE)/extract/*.ml; do \
	  diff -q $$f plans/extracted/$$(basename $$f) || exit 1; done
	@echo "plans/extracted matches the model"

clean:
	rm -rf $(CACHE)
