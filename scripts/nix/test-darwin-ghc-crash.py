#!/usr/bin/env python3
"""Focused crash-report and canonical build-status regressions."""

import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("crash_reports", ROOT / "collect-darwin-ghc-crash.py")
reports = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reports)


def ips(path="/nix/store/abc-ghc914-sh-compiler/lib/bin/ghc-9.14", **extra):
    body = {
        "procPath": path, "pid": 1234, "cpuType": "X86-64",
        "faultingThread": 1, "exception": {"type": "EXC_BAD_ACCESS", "signal": "SIGBUS"},
        "termination": {"namespace": "SIGNAL", "code": 10},
        "threads": [{"frames": [{"symbol": "other_thread"}]}, {
            "triggered": True, "threadState": {"flavor": "x86_THREAD_STATE", "rip": {"value": 4160}},
            "frames": [{"imageIndex": 2, "imageOffset": 64, "symbol": "crashed_here"}]}],
        "usedImages": [{"path": "unused-one"}, {"path": "unused-two"}, {
            "name": "ghc-9.14", "path": path, "base": 4096, "size": 1024,
            "arch": "x86_64", "uuid": "fixture-uuid"}],
        "crashReporterKey": "PRIVATE-HEADER", "environment": "PRIVATE-ENVIRONMENT",
    }
    body.update(extra)
    return (json.dumps({"bug_type": "309", "incident_id": "PRIVATE-INCIDENT"})
            + "\n" + json.dumps(body)).encode()


class CrashReports(unittest.TestCase):
    def test_faulting_thread_and_original_image_index(self):
        result = reports.crash_fields(ips())
        self.assertEqual(result["frames"][0]["symbol"], "crashed_here")
        self.assertEqual(result["threadState"]["rip"]["value"], 4160)
        self.assertEqual(result["images"][0]["index"], 2)
        self.assertEqual(result["images"][0]["base"] + result["frames"][0]["imageOffset"], 4160)
        self.assertNotIn("PRIVATE", json.dumps(result))
        self.assertNotIn("other_thread", json.dumps(result))

    def test_rejects_unrelated_process_and_non_crash_reports(self):
        self.assertIsNone(reports.crash_fields(ips("/Applications/Other.app/Other")))
        self.assertIsNone(reports.crash_fields(ips().replace(b'"309"', b'"288"')))
        self.assertIsNone(reports.crash_fields(ips(faultingThread=99)))

    def test_only_recent_regular_bounded_files(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "ghc-current.ips").write_bytes(ips())
            old = root / "ghc-old.ips"
            old.write_bytes(ips())
            os.utime(old, (1, 1))
            (root / "ghc-link.ips").symlink_to(root / "ghc-current.ips")
            (root / "unrelated.ips").write_bytes(ips())
            (root / "ghc-incomplete.ips").write_text('{"bug_type":"309"}\n{')
            (root / "ghc-large.ips").write_bytes(b"x" * (reports.MAX_INPUT + 1))
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                self.assertEqual(reports.collect([root], time.time() - 30), 1)
            self.assertEqual(output.getvalue().count("Darwin GHC crash report:"), 1)

    def test_report_frame_and_output_bounds(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for index in range(5):
                (root / f"ghc-{index}.ips").write_bytes(ips())
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(reports.collect([root], 0), reports.MAX_REPORTS)
            frames = [{"symbol": "x" * 1000} for _ in range(100)]
            (root / "ghc-huge-fields.ips").write_bytes(ips(threads=[{"frames": frames}], faultingThread=0))
            self.assertEqual(len(reports.crash_fields(ips(threads=[{"frames": frames}], faultingThread=0))["frames"]), 80)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "ghc-huge-fields.ips").write_bytes(ips(threads=[{"frames": frames}], faultingThread=0))
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                self.assertEqual(reports.collect([root], 0), 0)
            self.assertIn("technical-output bound", output.getvalue())
            self.assertLess(len(output.getvalue()), reports.MAX_OUTPUT)

    def test_deadline_cannot_be_swallowed_as_filesystem_error(self):
        with patch.object(reports, "recent_reports", side_effect=reports.CollectionDeadline("bound")):
            with self.assertRaises(reports.CollectionDeadline):
                reports.collect([], 0)


class BuildStatus(unittest.TestCase):
    def run_driver(self, status, system="x86_64-darwin", github="true", arguments=None):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            script = root / "scripts/nix/build.sh"
            script.parent.mkdir(parents=True)
            shutil.copy2(ROOT / "build.sh", script)
            bin_dir = root / "bin"
            bin_dir.mkdir()
            for name, body in {
                "nix": 'printf "%s\\n" "$*" >> "$NIX_TEST_LOG"\nexit "$NIX_TEST_STATUS"\n',
                "python3": 'printf "COLLECTOR %s\\n" "$*"\nexit 99\n',
            }.items():
                executable = bin_dir / name
                executable.write_text("#!/bin/sh\n" + body)
                executable.chmod(0o755)
            env = dict(os.environ, PATH=str(bin_dir) + os.pathsep + os.environ["PATH"],
                       NIX_SYSTEM=system, GITHUB_ACTIONS=github, NIX_TEST_STATUS=str(status),
                       NIX_TEST_LOG=str(root / "calls"))
            result = subprocess.run(["sh", str(script), *(arguments or ["native"])],
                                    env=env, capture_output=True, text=True, timeout=5)
            calls = (root / "calls").read_text().splitlines() if (root / "calls").exists() else []
            return result, calls

    def test_failed_intel_ci_build_keeps_exact_status_when_collector_fails(self):
        result, calls = self.run_driver(42)
        self.assertEqual(result.returncode, 42)
        self.assertEqual(len(calls), 1)
        self.assertIn("COLLECTOR", result.stdout)
        self.assertIn("--since", result.stdout)

    def test_success_builds_both_original_goals_without_collection(self):
        result, calls = self.run_driver(0)
        self.assertEqual(result.returncode, 0)
        self.assertEqual(len(calls), 2)
        self.assertIn("hydraJobs.x86_64-darwin.lib:simplex-chat", calls[0])
        self.assertIn("hydraJobs.x86_64-darwin.exe:simplex-chat", calls[1])
        self.assertNotIn("COLLECTOR", result.stdout)

    def test_failed_ios_ci_build_preserves_status_when_both_diagnostics_fail(self):
        result, calls = self.run_driver(42, arguments=["ios"])
        self.assertEqual(result.returncode, 42)
        self.assertEqual(len(calls), 1)
        self.assertIn("collect-darwin-ghc-crash.py", result.stdout)
        self.assertIn("trace-intel-ghc.py", result.stdout)
        self.assertIn("--attribute hydraJobs.x86_64-darwin.x86_64-darwin-ios:lib:simplex-chat", result.stdout)
        self.assertIn("--log", result.stdout)

    def test_native_failure_does_not_launch_compiler_replay(self):
        result, _ = self.run_driver(42)
        self.assertEqual(result.returncode, 42)
        self.assertNotIn("trace-intel-ghc.py", result.stdout)

    def test_non_intel_and_non_ci_failures_keep_status_without_collection(self):
        for system, github in [("x86_64-linux", "true"), ("aarch64-darwin", "true"),
                               ("x86_64-darwin", "false")]:
            with self.subTest(system=system, github=github):
                result, _ = self.run_driver(42, system, github)
                self.assertEqual(result.returncode, 42)
                self.assertNotIn("COLLECTOR", result.stdout)

    def test_list_does_not_build_or_collect(self):
        result, calls = self.run_driver(42, arguments=["--list", "all"])
        self.assertEqual(result.returncode, 0)
        self.assertEqual(len(result.stdout.splitlines()), 4)
        self.assertEqual(calls, [])
        self.assertNotIn("COLLECTOR", result.stdout)


if __name__ == "__main__":
    unittest.main()
