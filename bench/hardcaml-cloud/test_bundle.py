"""ABI tests independent of the parser implementation."""

import copy
import io
import json
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch
from pathlib import Path

from wire_hardcaml_tools import bundle, oracle_cache, prepare, timing, verify


def manifest():
    def port(name, width, compact, signed=False, direction="output"):
        return dict(name=name, width=width, wire_width=compact,
                    signed=signed, direction=direction)
    return dict(module="test_decoder", capture_bytes=64, max_length=255,
                read_prefix=64, latency_cycles=3, packing="full_nonfinal",
                ports=[port(n, w, w, direction="input") for n, w in bundle.INPUTS.items()]
                + [port("input_ready", 1, 1), port("output_valid", 1, 1),
                   port("frame_id", 64, 64), port("frame_length", 9, 9),
                   port("transport_error", 3, 3), port("accepted", 1, 1),
                   port("in_scope", 1, 1), port("consumed", 63, 8),
                   port("field_negative", 63, 8, True), port("field_flag", 1, 1)])


class MetadataTests(unittest.TestCase):
    def setUp(self):
        self.schema = bundle.layout(manifest(), b"test RTL")
        self.values = dict(frame_id=0x1122334455667788, frame_length=42,
                           transport_error=0, accepted=1, in_scope=1,
                           consumed=20, field_negative=-3, field_flag=1)

    def test_golden_little_endian_record(self):
        # Fixed externally readable ABI vector, including signed extension.
        expected = bytes.fromhex("88776655443322112a30457f") + bytes(52)
        self.assertEqual(bundle.encode_record(self.schema, self.values), expected)
        self.assertEqual(bundle.decode_record(self.schema, expected), self.values)

    def test_padding_and_truncation_are_not_silently_accepted(self):
        data = bytearray(bundle.encode_record(self.schema, self.values))
        data[-1] = 128
        with self.assertRaisesRegex(ValueError, "padding"):
            bundle.decode_record(self.schema, data)
        with self.assertRaisesRegex(ValueError, "truncated"):
            bundle.decode_record(self.schema, data[:-1])

    def test_out_of_range_and_wrong_field_sets(self):
        for key, value in [("field_negative", -129), ("field_negative", 128),
                           ("frame_id", -1), ("frame_id", 1 << 64),
                           ("accepted", True)]:
            with self.subTest(key=key, value=value), self.assertRaises(ValueError):
                bundle.encode_record(self.schema, dict(self.values, **{key: value}))
        with self.assertRaisesRegex(ValueError, "field set"):
            bundle.encode_record(self.schema, {})

    def test_manifest_rejects_unsafe_contracts(self):
        for change in [lambda m: m.update(module="bad;module"),
                       lambda m: m.update(read_prefix=65),
                       lambda m: m.update(max_length=1 << 32),
                       lambda m: m.update(packing="contiguous"),
                       lambda m: m["ports"].append(copy.deepcopy(m["ports"][0])),
                       lambda m: m["ports"].append(dict(name="unaccounted", width=1,
                                                        wire_width=1, signed=False,
                                                        direction="output"))]:
            value = manifest()
            change(value)
            with self.assertRaises(ValueError):
                bundle.layout(value, b"test RTL")

    def test_metadata_cannot_overrun_sde_record(self):
        value = manifest()
        for index in range(8):
            value["ports"].append(dict(name=f"field_extra_{index}", width=63,
                                       wire_width=63, signed=False, direction="output"))
        with self.assertRaisesRegex(ValueError, "one SDE beat"):
            bundle.layout(value, b"test RTL")

    def test_digest_binds_rtl_and_contract(self):
        other = bundle.layout(manifest(), b"different RTL")
        self.assertNotEqual(self.schema["layout_sha256"], other["layout_sha256"])
        changed = manifest()
        changed["max_length"] = 254
        self.assertNotEqual(self.schema["layout_sha256"],
                            bundle.layout(changed, b"test RTL")["layout_sha256"])

    def packaged(self, root):
        source = root / "manifest.json"
        source.write_text(json.dumps(manifest()))
        rtl = root / "core.v"
        rtl.write_bytes(b"test RTL")
        destination = root / "bundle"
        bundle.package(source, rtl, destination)
        return destination

    def test_stale_or_modified_artifacts_are_refused(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            directory = self.packaged(root)
            self.assertEqual(bundle.read_schema(directory), self.schema)
            adapter = directory / "cl_sde_srm.sv"
            original = adapter.read_bytes()
            adapter.write_bytes(original + b"// modified\n")
            with self.assertRaisesRegex(ValueError, "adapter differs"):
                bundle.read_schema(directory)
            with self.assertRaisesRegex(ValueError, "refusing to replace"):
                bundle.package(root / "manifest.json", root / "core.v", directory)
            adapter.write_bytes(original)
            schema = directory / "metadata.json"
            changed = json.loads(schema.read_text())
            changed["fields"][0]["offset"] += 1
            schema.write_text(json.dumps(changed))
            with self.assertRaisesRegex(ValueError, "sealed ABI"):
                bundle.read_schema(directory)

    def test_capture_rejects_missing_extra_or_different_results(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = self.packaged(Path(temporary))
            record = bundle.encode_record(self.schema, self.values)
            reference = json.dumps(self.values) + "\n"
            self.assertEqual(verify.verify(directory, io.StringIO(reference),
                                           io.BytesIO(record)), 1)
            cases = [("", record, "empty reference"),
                     (reference, record[:-1], "truncated"),
                     (reference, record * 2, "extra device"),
                     (reference, bytes([record[0] ^ 1]) + record[1:], "frame_id")]
            for expected, captured, error in cases:
                with self.subTest(error=error), self.assertRaisesRegex(ValueError, error):
                    verify.verify(directory, io.StringIO(expected), io.BytesIO(captured))

    def test_timing_violations_cannot_be_published_as_success(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            with self.assertRaisesRegex(ValueError, "missing routed"):
                prepare.check_checkpoints(directory, "wire_checked")
            (directory / "cl_sde.wire_checked.post_route.dcp").write_bytes(b"DCP")
            (directory / "wire_checked.Developer_CL.tar").write_bytes(b"tar")
            prepare.check_checkpoints(directory, "wire_checked")
            (directory / "cl_sde.wire_checked.post_route.VIOLATED.dcp").write_bytes(b"bad")
            with self.assertRaisesRegex(ValueError, "timing-violated"):
                prepare.check_checkpoints(directory, "wire_checked")

    def test_hdk_staging_excludes_old_results_and_resolves_script_links(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            checkout = root / "hdk"
            checkout.mkdir()
            def git(*args):
                return subprocess.check_output(["git", "-c", "core.hooksPath=/dev/null",
                                                "-c", "commit.gpgsign=false",
                                                "-C", str(checkout), *args],
                                               stderr=subprocess.STDOUT).decode().strip()
            git("init", "-q")
            example = checkout / "hdk/cl/examples/cl_sde"
            scripts = example / "build/scripts"
            scripts.mkdir(parents=True)
            design = example / "design"
            design.mkdir()
            (design / "cl_sde_srm.sv").write_text("// original example\n")
            common = checkout / "hdk/common/scripts/build.py"
            common.parent.mkdir(parents=True)
            common.write_text("# pinned build script\n")
            (scripts / "build.py").symlink_to(common)
            git("add", ".")
            git("-c", "user.name=Test", "-c", "user.email=test@example.invalid",
                "commit", "-qm", "fixture")
            old = example / "build/checkpoints/old.post_route.dcp"
            old.parent.mkdir()
            old.write_bytes(b"stale checkpoint")
            # Use a real minimal git checkout as the pinned source fixture.
            with patch.object(bundle, "HDK_REVISION", git("rev-parse", "HEAD")):
                directory = self.packaged(root)
                destination = root / "job/cl_sde"
                prepare.hdk(directory, checkout, destination)
            copied = destination / "build/scripts/build.py"
            self.assertFalse(copied.is_symlink())
            self.assertEqual(copied.read_bytes(), common.read_bytes())
            self.assertFalse((destination / "build/checkpoints").exists())

    def test_registered_ooc_and_adapter_are_distinct_measurements(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            directory = self.packaged(root)
            for adapter in (False, True):
                output = root / ("adapter" if adapter else "registered")
                prepare.ooc(directory, output, adapter=adapter)
                provenance = json.loads((output / "provenance.json").read_text())
                self.assertEqual(provenance["registered_boundary"], not adapter)
                self.assertEqual(provenance["layout_sha256"], self.schema["layout_sha256"])
                budget = json.loads((output / "stage-budgets.json").read_text())["allocation"]
                self.assertAlmostEqual(sum(budget[k] for k in
                                           ("uncertainty_ns", "register_allowance_ns",
                                            "logic_ns", "routing_ns")), budget["period_ns"])

    def test_bare_timing_core_accepts_published_width_without_sde_packaging(self):
        value = manifest()
        for port in value["ports"]:
            if port["name"] == "input_data":
                port["width"] = port["wire_width"] = 256
            elif port["name"] == "input_keep":
                port["width"] = port["wire_width"] = 32
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source, rtl = root / "manifest.json", root / "core.v"
            source.write_text(json.dumps(value))
            rtl.write_bytes(b"fixture RTL")
            prepare.core_ooc(source, rtl, root / "job", part="xc7vx690tffg1761-2")
            provenance = json.loads((root / "job/provenance.json").read_text())
            self.assertEqual(provenance["part"], "xc7vx690tffg1761-2")
            self.assertTrue(provenance["registered_boundary"])
            next(p for p in value["ports"] if p["name"] == "input_data")["width"] = 257
            with self.assertRaisesRegex(ValueError, "KEEP"):
                timing.core_schema(value, b"fixture RTL")

    @unittest.skipUnless(shutil.which("yosys"), "Yosys is required for boundary equivalence")
    def test_boundary_registers_every_input_and_output(self):
        # A combinational core makes a missing input/output register observable.
        interface = timing.ports(self.schema)
        inputs = [(n, w) for n, w, d in interface if d == "input"]
        outputs = [(n, w) for n, w, d in interface if d == "output"]
        declaration = ",\n".join(f"{d} [{w - 1}:0] {n}" for n, w, d in interface)
        core = [f"module test_decoder({declaration});"]
        parity = " ^ ".join(f"(^{name})" for name, _ in inputs if name != "clock")
        for name, width in outputs:
            core.append(f"assign {name} = {{{width}{{{parity}}}}};")
        core.append("endmodule")
        miter = ["module miter(" + ",".join(f"input [{w - 1}:0] {n}"
                                           for n, w in inputs) + ",output proof_ok);"]
        for name, width in outputs:
            miter += [f"wire [{width - 1}:0] actual_{name}, gold_{name};",
                      f"reg [{width - 1}:0] first_{name}, second_{name};",
                      f"always @(posedge clock) begin first_{name} <= gold_{name}; "
                      f"second_{name} <= first_{name}; end"]
        for module, instance, prefix in [(timing.TOP, "dut", "actual"),
                                          ("test_decoder", "gold", "gold")]:
            bindings = [f".{n}({n})" for n, _ in inputs]
            bindings += [f".{n}({prefix}_{n})" for n, _ in outputs]
            miter.append(f"{module} {instance}(" + ",".join(bindings) + ");")
        miter += ["assign proof_ok = " + " & ".join(
            f"(actual_{n} == second_{n})" for n, _ in outputs) + ";", "endmodule"]
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "fixture.v"
            path.write_text("\n".join(core) + "\n" + timing.boundary(self.schema)
                            + "\n" + "\n".join(miter))
            result = subprocess.run(["yosys", "-Q", "-p", "read_verilog fixture.v; "
                                     "setattr -unset keep_hierarchy; "
                                     "prep -top miter -flatten; sat -seq 4 -prove-skip 2 "
                                     "-prove proof_ok 1 -verify"], cwd=temporary,
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            self.assertEqual(result.returncode, 0, result.stdout.decode(errors="replace")[-4000:])


class OracleCacheTests(unittest.TestCase):
    def reference(self, command, **kwargs):
        self.assertEqual(kwargs["env"]["BUILD_EVERPARSE"], "1")
        Path(command[-1]).write_text('{"frame_id":0}\n')
        return subprocess.CompletedProcess(command, 0, oracle_cache.ORACLE_MARKER)

    def test_replay_reuses_frozen_records_and_refuses_mutation(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            executable, corpus = root / "reference.exe", root / "frames.bin"
            executable.write_bytes(b"native reference fixture")
            corpus.write_bytes(b"corpus")
            with patch.object(oracle_cache.subprocess, "run", side_effect=self.reference) as run:
                entry = oracle_cache.build(executable, corpus, root / "cache", "scan_packet", 255, 0)
                self.assertEqual(entry, oracle_cache.build(executable, corpus, root / "cache",
                                                          "scan_packet", 255, 0))
                self.assertEqual(run.call_count, 1)
            with self.assertRaisesRegex(ValueError, "maximum"):
                oracle_cache.validate(entry / "receipt.json", corpus, entry / "expected.jsonl",
                                      maximum=256)
            (entry / "expected.jsonl").write_text('{"frame_id":1}\n')
            with self.assertRaisesRegex(ValueError, "modified"):
                oracle_cache.validate(entry / "receipt.json", corpus, entry / "expected.jsonl")

    def test_changed_oracle_or_corpus_gets_another_cache_key(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            executable, corpus = root / "reference.exe", root / "frames.bin"
            executable.write_bytes(b"first reference")
            corpus.write_bytes(b"first corpus")
            with patch.object(oracle_cache.subprocess, "run", side_effect=self.reference) as run:
                first = oracle_cache.build(executable, corpus, root / "cache", "scan_packet", 255, 0)
                executable.write_bytes(b"second reference")
                second = oracle_cache.build(executable, corpus, root / "cache", "scan_packet", 255, 0)
                corpus.write_bytes(b"second corpus")
                third = oracle_cache.build(executable, corpus, root / "cache", "scan_packet", 255, 0)
                self.assertEqual(len({first, second, third}), 3)
                self.assertEqual(run.call_count, 3)

    def test_reference_without_verified_c_is_not_cached(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            executable, corpus = root / "reference.exe", root / "frames.bin"
            executable.write_bytes(b"reference")
            corpus.write_bytes(b"corpus")
            result = subprocess.CompletedProcess([], 0, "C skipped")
            with patch.object(oracle_cache.subprocess, "run", return_value=result):
                with self.assertRaisesRegex(ValueError, "oracle was absent"):
                    oracle_cache.build(executable, corpus, root / "cache", "scan_packet", 255, 0)
            self.assertEqual(list((root / "cache").iterdir()), [])


if __name__ == "__main__":
    unittest.main()
