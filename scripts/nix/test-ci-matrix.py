#!/usr/bin/env python3
"""Check CI selections against the actual Make/build.sh output lists."""
import importlib.util
import os
import re
from pathlib import Path
import subprocess
import unittest

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("matrix", HERE / "ci-matrix.py")
matrix = importlib.util.module_from_spec(spec)
spec.loader.exec_module(matrix)


def outputs(group):
    result = []
    for row in matrix.select(group)["include"]:
        env = dict(os.environ, NIX_SYSTEM=row["system"], NIX_TARGET=row["target"])
        result += subprocess.check_output(["make", "--no-print-directory", "nix-list"],
                                          cwd=HERE.parent.parent, env=env, text=True).splitlines()
    return result


class Matrix(unittest.TestCase):
    def test_default_covers_all_21_outputs_once(self):
        selected = outputs("all")
        self.assertEqual(len(matrix.select("all")["include"]), 7)
        self.assertEqual(len(selected), 21)
        self.assertEqual(len(set(selected)), 21)

    def test_static_dispatch_is_exactly_the_existing_three_outputs(self):
        self.assertEqual(outputs("linux-static"), [
            "hydraJobs.x86_64-linux.x86_64-linux-static:exe:simplex-chat",
            "hydraJobs.x86_64-linux.i686-linux-static:exe:simplex-chat",
            "hydraJobs.x86_64-linux.aarch64-linux-static:exe:simplex-chat",
        ])

    def test_legacy_pr_jobs_do_not_cancel_active_runs(self):
        source = (HERE.parent.parent / ".github/workflows/build.yml").read_text()
        expressions = re.findall(r"^      cancel-in-progress: \$\{\{ (.*) \}\}$", source, re.MULTILINE)
        self.assertEqual(len(expressions), 3)
        prior = "!startsWith(github.ref, 'refs/tags/v') && !needs.maybe-release.outputs.libs_tag"
        for expression in expressions:
            self.assertEqual(expression, "github.event_name != 'pull_request' && " + prior)

            def evaluate(event, ref, libs_tag):
                # Evaluate these actual, narrowly constrained workflow
                # expressions with GitHub's relevant boolean/string semantics.
                code = expression.replace("github.event_name", "event").replace("github.ref", "ref")
                code = code.replace("needs.maybe-release.outputs.libs_tag", "libs_tag")
                code = re.sub(r"!(?!=)", "not ", code).replace("&&", "and")
                return bool(eval(code, {"__builtins__": {}}, {
                    "event": event, "ref": ref, "libs_tag": libs_tag,
                    "startsWith": lambda value, prefix: value.startswith(prefix),
                }))

            for event in ["pull_request", "push", "release", "workflow_dispatch", "schedule"]:
                for ref in ["refs/pull/7729/merge", "refs/heads/master", "refs/tags/v7.0", "refs/tags/other"]:
                    for libs_tag in [None, False, 0, "", "false", "v7.0-libs"]:
                        with self.subTest(event=event, ref=ref, libs_tag=libs_tag):
                            expected = False if event == "pull_request" else not ref.startswith("refs/tags/v") and not libs_tag
                            self.assertEqual(evaluate(event, ref, libs_tag), expected)

    def test_unknown_group_rejected(self):
        with self.assertRaises(ValueError):
            matrix.select("windows")
        result = subprocess.run(["python3", str(HERE / "ci-matrix.py"), "--group", "windows"],
                                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
