#!/usr/bin/env python3
"""Check CI selections against the actual Make/build.sh output lists."""
import importlib.util
import json
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

    def test_android_dispatch_is_exactly_the_existing_four_outputs(self):
        self.assertEqual(outputs("android"), [
            "hydraJobs.x86_64-linux.aarch64-android:lib:simplex-chat",
            "hydraJobs.x86_64-linux.aarch64-android:lib:support",
            "hydraJobs.x86_64-linux.armv7a-android:lib:simplex-chat",
            "hydraJobs.x86_64-linux.armv7a-android:lib:support",
        ])

    def test_windows_dispatch_is_exactly_the_existing_two_outputs(self):
        self.assertEqual(outputs("windows"), [
            "hydraJobs.x86_64-linux.x86_64-windows:exe:simplex-chat",
            "hydraJobs.x86_64-linux.x86_64-windows:lib:simplex-chat",
        ])

    def test_intel_dispatch_retains_all_four_existing_outputs(self):
        selected = matrix.select("x86_64-darwin")
        self.assertEqual(selected, {"include": [dict(
            system="x86_64-darwin", runner="macos-15-intel", target="all")]})
        self.assertEqual(outputs("x86_64-darwin"), [
            "hydraJobs.x86_64-darwin.lib:simplex-chat",
            "hydraJobs.x86_64-darwin.exe:simplex-chat",
            "hydraJobs.x86_64-darwin.x86_64-darwin-ios:lib:simplex-chat",
            "hydraJobs.x86_64-darwin.x86_64-darwin:lib:simplex-chat",
        ])
        cli = subprocess.check_output([
            "python3", str(HERE / "ci-matrix.py"), "--group", "x86_64-darwin"], text=True)
        self.assertEqual(json.loads(cli), selected)

    def test_manual_groups_have_distinct_queues_without_changing_pr_or_push_keys(self):
        source = (HERE.parent.parent / ".github/workflows/nix.yml").read_text()
        groups = re.findall(r"^  group: (.*)$", source, re.MULTILINE)
        self.assertEqual(len(groups), 1)
        prefix = "nix-${{ github.workflow }}-${{ github.ref }}"
        self.assertTrue(groups[0].startswith(prefix))
        suffix = groups[0][len(prefix):]
        self.assertEqual(suffix,
                         "${{ github.event_name == 'workflow_dispatch' && format('-{0}', inputs.group || 'all') || '' }}")
        expression = suffix[4:-3].replace("github.event_name", "event").replace("inputs.group", "group")
        expression = expression.replace("&&", "and").replace("||", "or")

        def queue_key(event, group):
            suffix_value = eval(expression, {"__builtins__": {}}, {
                "event": event, "group": group,
                "format": lambda template, value: template.format(value),
            })
            return "nix-Nix Haskell builds-refs/heads/branch" + suffix_value

        for event in ["pull_request", "push"]:
            for group in [None, "all", "linux-static", "android", "windows", "x86_64-darwin"]:
                self.assertEqual(queue_key(event, group), "nix-Nix Haskell builds-refs/heads/branch")
        keys = [queue_key("workflow_dispatch", group)
                for group in ["all", "linux-static", "android", "windows", "x86_64-darwin"]]
        self.assertEqual(len(set(keys)), 5)
        self.assertEqual(queue_key("workflow_dispatch", ""), keys[0])
        self.assertEqual(queue_key("workflow_dispatch", None), keys[0])

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
            matrix.select("unknown")
        result = subprocess.run(["python3", str(HERE / "ci-matrix.py"), "--group", "unknown"],
                                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
