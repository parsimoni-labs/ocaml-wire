"""Run the same host tools from an installed wheel or the source checkout."""

import argparse
import importlib
import sys


COMMANDS = {
    "bundle": "bundle",
    "verify": "verify",
    "prepare": "prepare",
    "rtl-test": "rtl_test",
    "oracle-cache": "oracle_cache",
    "timing": "timing",
    "measure": "measure",
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=COMMANDS)
    if len(sys.argv) < 2 or sys.argv[1] in ("-h", "--help"):
        parser.print_help()
        return
    command = parser.parse_args([sys.argv.pop(1)]).command
    sys.argv[0] += " " + command
    importlib.import_module("." + COMMANDS[command], __package__).main()


if __name__ == "__main__":
    main()
