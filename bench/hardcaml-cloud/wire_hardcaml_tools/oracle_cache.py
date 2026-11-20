"""Cache the existing C-enabled differential reference, independently of RTL."""

import argparse
import hashlib
import json
import os
import subprocess
import tempfile
from pathlib import Path


# The prefix [Diff_driver.report_oracle] prints only when the C oracle ran.
ORACLE_MARKER = "structured oracle: EverParse C, verified .3d"


def digest(path):
    sha = hashlib.sha256()
    with Path(path).open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            sha.update(chunk)
    return sha.hexdigest()


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode()


def request(reference, corpus, case, maximum, slot):
    if not case or maximum < 0 or slot < 0:
        raise ValueError("invalid oracle configuration")
    return dict(version=1, corpus_sha256=digest(corpus),
                reference_sha256=digest(reference), case=case,
                max_length=maximum, slot=slot, oracle="everparse-3d-required")


def validate(receipt, corpus, records, *, maximum=None, slot=None):
    receipt = Path(receipt)
    saved = json.loads(receipt.read_text())
    config = saved["request"]
    key = hashlib.sha256(canonical(config)).hexdigest()
    if saved["key"] != key or saved.get("oracle_confirmed") is not True:
        raise ValueError("invalid oracle receipt")
    if config.get("version") != 1 or config.get("oracle") != "everparse-3d-required":
        raise ValueError("unsupported oracle receipt")
    if config["corpus_sha256"] != digest(corpus):
        raise ValueError("corpus differs from cached oracle")
    if saved["records_sha256"] != digest(records):
        raise ValueError("cached oracle records were modified")
    if type(saved["record_count"]) is not int or saved["record_count"] < 1:
        raise ValueError("empty cached oracle")
    if maximum is not None and config["max_length"] != maximum:
        raise ValueError("cached oracle maximum differs from circuit")
    if slot is not None and config["slot"] != slot:
        raise ValueError("cached oracle slot differs from replay")
    return saved


def build(reference, corpus, cache, case, maximum, slot):
    reference, corpus, cache = (Path(p).resolve() for p in (reference, corpus, cache))
    config = request(reference, corpus, case, maximum, slot)
    key = hashlib.sha256(canonical(config)).hexdigest()
    destination = cache / key
    cache.mkdir(parents=True, exist_ok=True)
    if destination.exists():
        saved = validate(destination / "receipt.json", corpus, destination / "expected.jsonl")
        if saved["request"] != config:
            raise ValueError("oracle cache request differs")
        return destination
    with tempfile.TemporaryDirectory(prefix=".oracle-", dir=cache) as temporary:
        temporary = Path(temporary)
        records = temporary / "expected.jsonl"
        command = [str(reference), case, str(maximum), str(slot), str(corpus), str(records)]
        environment = dict(os.environ, BUILD_EVERPARSE="1")
        result = subprocess.run(command, env=environment, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True)
        (temporary / "reference.log").write_text(result.stdout)
        if result.returncode or ORACLE_MARKER not in result.stdout:
            raise ValueError("reference failed or verified EverParse C oracle was absent: "
                             + result.stdout[-2000:])
        count = 0
        with records.open() as source:
            for count, line in enumerate(source, 1):
                if json.loads(line).get("frame_id") != count - 1:
                    raise ValueError("cached reference frame IDs are not ordered")
        if not count:
            raise ValueError("reference exported no records")
        # Refuse inputs/executable changed during generation.
        if request(reference, corpus, case, maximum, slot) != config:
            raise ValueError("oracle inputs changed during generation")
        saved = dict(key=key, request=config, oracle_confirmed=True,
                     records_sha256=digest(records), record_count=count)
        (temporary / "receipt.json").write_text(json.dumps(saved, indent=2) + "\n")
        try:
            temporary.rename(destination)
        except OSError:
            if not destination.is_dir():
                raise
            existing = validate(destination / "receipt.json", corpus,
                                destination / "expected.jsonl")
            if existing != saved:
                raise ValueError("concurrent oracle builders disagree")
        # TemporaryDirectory cleans up the old name, which is now absent.
    return destination


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("reference", type=Path, help="native C-enabled reference.exe")
    parser.add_argument("corpus", type=Path)
    parser.add_argument("cache", type=Path)
    parser.add_argument("--case", default="scan_packet")
    parser.add_argument("--max-length", type=int, default=65549)
    parser.add_argument("--slot", type=int, default=0)
    args = parser.parse_args()
    try:
        print(build(args.reference, args.corpus, args.cache, args.case,
                    args.max_length, args.slot))
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        parser.exit(1, str(error) + "\n")


if __name__ == "__main__":
    main()
