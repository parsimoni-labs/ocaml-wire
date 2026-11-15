#!/usr/bin/env python3
"""Compare captured SDE records with the existing differential driver's reference."""

import argparse
import json
from pathlib import Path

from . import bundle


def verify(directory, reference, records):
    schema = bundle.read_schema(directory)
    count = 0
    for count, line in enumerate(reference, 1):
        expected = json.loads(line)
        bundle.encode_record(schema, expected)
        actual = bundle.decode_record(schema, records.read(schema["record_bytes"]))
        if actual != expected:
            different = [name for name in actual if actual[name] != expected[name]]
            raise ValueError(f"record {count - 1} differs on {', '.join(different)}")
    if not count:
        raise ValueError("empty reference is not a hardware validation")
    if records.read(1):
        raise ValueError("unexpected extra device records")
    return count


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bundle", type=Path)
    parser.add_argument("reference", type=Path)
    parser.add_argument("records", type=Path)
    args = parser.parse_args()
    try:
        with args.reference.open() as reference, args.records.open("rb") as records:
            count = verify(args.bundle, reference, records)
    except (OSError, ValueError, TypeError, KeyError) as error:
        parser.exit(1, str(error) + "\n")
    print(f"Verified {count} ordered device records against the reference")


if __name__ == "__main__":
    main()
