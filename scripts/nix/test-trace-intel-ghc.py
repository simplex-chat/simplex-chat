#!/usr/bin/env python3
"""Target, failure and timeout controls for the compiler diagnostic launcher."""

import contextlib
import importlib.util
import io
import os
from pathlib import Path
import signal
import sys
import tempfile
import time
import unittest
from unittest.mock import patch


SPEC = importlib.util.spec_from_file_location("intel_trace", Path(__file__).with_name("trace-intel-ghc.py"))
TRACE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(TRACE)
DRV = "/nix/store/" + "a" * 32 + "-simplex-chat-lib-simplex-chat-7.1.0.11.drv"
FAILURE = ("Failed to build simplex-chat-7.1.0.11-unit. The build process terminated with exit code -10\n"
           + "error: Cannot build '" + DRV + "'.\n")


class LauncherTests(unittest.TestCase):
    def test_unique_observed_failure(self):
        self.assertEqual(TRACE.select_failure(FAILURE + FAILURE), DRV)
        self.assertIsNone(TRACE.select_failure(FAILURE.replace("exit code -10", "exit code 1")))
        self.assertIsNone(TRACE.select_failure(FAILURE.replace("simplex-chat-lib-simplex-chat", "dependency-lib-dependency")))
        with self.assertRaises(ValueError):
            TRACE.select_failure(FAILURE + FAILURE.replace("a" * 32, "b" * 32))

    def invoke(self, folder, *, actions="true", machine="x86_64", target=None, failure=FAILURE,
               reject_build=False, reject_cache=False):
        log = Path(folder) / "nix-build.log"
        log.write_text(failure)
        calls = []
        def step(arguments, deadline, seconds, capture=False):
            values = list(map(str, arguments))
            calls.append((values, seconds, capture))
            if values[0] == "git":
                return "c" * 40
            if values[:3] == ["xcrun", "--find", "clang"]:
                return "/usr/bin/clang"
            if values[0] == "xcrun":
                return "/existing/SDK"
            if (reject_build and "--file" in values) or (reject_cache and "--expr" in values):
                raise RuntimeError("Diagnostic command returned 1")
            return None
        argv = ["trace-intel-ghc.py", "--repo", folder, "--attribute", target or TRACE.ATTRIBUTE, "--log", str(log)]
        with patch.dict(os.environ, {"GITHUB_ACTIONS": actions}), patch.object(TRACE.platform, "system", return_value="Darwin"), \
                patch.object(TRACE.platform, "machine", return_value=machine), \
                patch.object(TRACE.shutil, "which", side_effect=lambda tool: None if tool in ("timeout", "gtimeout") else "/existing/tool"), \
                patch.object(TRACE, "run_step", side_effect=step), patch("sys.argv", argv), \
                contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
            result = TRACE.main()
        return result, calls

    def test_excluded_contexts_never_launch(self):
        with tempfile.TemporaryDirectory() as folder:
            for options in ({"actions": "false"}, {"machine": "arm64"}, {"target": "hydraJobs.x86_64-darwin.lib:simplex-chat"},
                            {"failure": FAILURE.replace("exit code -10", "exit code 1")}):
                result, calls = self.invoke(folder, **options)
                self.assertEqual((result, calls), (0, []))

    def test_one_scoped_replay_with_bounds_and_no_sandbox_override(self):
        with tempfile.TemporaryDirectory() as folder:
            result, calls = self.invoke(folder)
            self.assertEqual(result, 0)
            builds = [call for call in calls if call[0][:2] == ["nix", "build"]]
            self.assertEqual(len(builds), 2)
            preparation = builds[0][0]
            self.assertIn("--expr", preparation)
            self.assertEqual(preparation[preparation.index("--max-jobs") + 1], "0")
            self.assertEqual(preparation[preparation.index("--builders") + 1], "")
            self.assertEqual(preparation[preparation.index("external-builders") + 1], "[]")
            self.assertEqual(builds[0][1], 30)
            arguments, seconds, _ = builds[1]
            self.assertEqual(seconds, 300)
            self.assertEqual(arguments[arguments.index("expectedDrv") + 1], DRV)
            self.assertIn("--timeout", arguments)
            self.assertEqual(arguments[-1], "300")
            self.assertNotIn("--option", arguments)
            self.assertIn("--keep-failed", arguments)
            original = arguments[arguments.index("original") + 1]
            self.assertIn('f.hydraJobs.x86_64-darwin."x86_64-darwin-ios:lib:simplex-chat"', original)
            self.assertNotIn(".project.hsPkgs", original)
            self.assertEqual(sum(call[0][:1] == ["/usr/bin/clang"] for call in calls), 1)

    def test_failed_diagnostic_returns_without_replay_loop(self):
        with tempfile.TemporaryDirectory() as folder:
            result, calls = self.invoke(folder, reject_build=True)
            self.assertEqual(result, 0)
            self.assertEqual(sum("--file" in call[0] for call in calls), 1)

    def test_cache_miss_never_launches_replay(self):
        with tempfile.TemporaryDirectory() as folder:
            result, calls = self.invoke(folder, reject_cache=True)
            self.assertEqual(result, 0)
            self.assertEqual(sum("--expr" in call[0] for call in calls), 1)
            self.assertFalse(any("--file" in call[0] for call in calls))

    def test_actual_metadata_output_is_bounded(self):
        with self.assertRaises(ValueError):
            TRACE.run_step([sys.executable, "-c", "print('x' * 65536)"], time.monotonic() + 10, 5, True)
        self.assertEqual(TRACE.run_step([sys.executable, "-c", "print('revision')"],
                                        time.monotonic() + 10, 5, True), "revision")

    def test_actual_timeout_kills_owned_command_and_its_child(self):
        with tempfile.TemporaryDirectory() as folder:
            pidfile = Path(folder) / "owned-pids"
            grandchild = "import signal,time; signal.signal(signal.SIGTERM,signal.SIG_IGN); time.sleep(60)"
            program = ("import os,signal,subprocess,sys,time; "
                       "signal.signal(signal.SIGTERM,signal.SIG_IGN); "
                       "p=subprocess.Popen([sys.executable,'-c',sys.argv[2]]); "
                       "open(sys.argv[1],'w').write(str(os.getpid())+' '+str(p.pid)); time.sleep(60)")
            before = time.monotonic()
            with self.assertRaisesRegex(TimeoutError, "wall-clock limit"):
                TRACE.command_status([sys.executable, "-c", program, str(pidfile), grandchild], 1)
            self.assertLess(time.monotonic() - before, 7)
            parent, child = map(int, pidfile.read_text().split())
            # The direct child is reaped. An orphaned grandchild can briefly
            # remain a zombie on Linux; it must not be running in either case.
            for pid in (parent, child):
                stopped = False
                until = time.monotonic() + 3
                while time.monotonic() < until:
                    try:
                        os.kill(pid, 0)
                    except ProcessLookupError:
                        stopped = True
                        break
                    stat = Path("/proc") / str(pid) / "stat"
                    if stat.exists() and stat.read_text().split(")", 1)[1].split()[0] == "Z":
                        stopped = True
                        break
                    time.sleep(0.02)
                self.assertTrue(stopped, "Owned fixture PID still running: " + str(pid))

    def test_nonzero_actual_command_remains_a_failure(self):
        with self.assertRaisesRegex(RuntimeError, "returned 7"):
            TRACE.run_step([sys.executable, "-c", "raise SystemExit(7)"], time.monotonic() + 10, 5)

    def test_deadline_and_nix_literal(self):
        with patch.object(TRACE.time, "monotonic", return_value=2), patch.object(TRACE.subprocess, "Popen") as run:
            with self.assertRaises(TimeoutError):
                TRACE.run_step(["nix", "build"], 1, 300)
            run.assert_not_called()
        self.assertEqual(TRACE.nix_string('path${value}\\"'), '"path\\${value}\\\\\\\""')


if __name__ == "__main__":
    unittest.main()
