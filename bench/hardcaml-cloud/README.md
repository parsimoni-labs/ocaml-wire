# Wire hardware host tools

These experimental tools package generated RTL, replay existing differential
reference records and prepare Vivado build inputs. They contain no packet parser.
The package has no Python runtime dependencies and requires Python 3.9 or newer.
Vivado, Verilator, Yosys and the native EverParse-backed reference executable are
external tools, needed only by their corresponding commands.

From a checkout, run `python3 bench/hardcaml-cloud/tools.py --help`. To use the
same implementations on a build host without the OCaml checkout:

```sh
python3 -m pip wheel --no-deps --wheel-dir artifacts/wheels bench/hardcaml-cloud
# Copy the wheel and prepared build inputs to the host, then activate a venv.
python3 -m pip install --no-index --find-links artifacts/wheels wire-hardcaml-tools
wire-hardcaml-tools --help
```

The commands are `bundle`, `verify`, `prepare`, `rtl-test`, `oracle-cache`,
`timing` and `measure`; each has `--help`. The source entry point and installed
entry point dispatch to the same package modules. The wheel includes the Tcl
timing reporter, so `prepare` does not need files from the source checkout.

Run the existing unit and boundary-equivalence tests from this directory with
`python3 -m unittest -v test_bundle.py`, or use the Dune test alias. See
[the production checklist](../../docs/hardcaml-production.md) for scope and open
readiness blockers. Packaging these tools does not validate FPGA timing.
