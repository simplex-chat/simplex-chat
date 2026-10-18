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


def listed_outputs(system, target):
    env = dict(os.environ, NIX_SYSTEM=system, NIX_TARGET=target)
    return subprocess.check_output(["make", "--no-print-directory", "nix-list"],
                                   cwd=HERE.parent.parent, env=env, text=True).splitlines()


def outputs(group):
    return [output for row in matrix.select(group)["include"]
            for output in listed_outputs(row["system"], row["target"])]


def workflow_step(name):
    source = (HERE.parent.parent / ".github/workflows/nix.yml").read_text()
    match = re.search(r"^      - name: " + re.escape(name) + r"\n(.*?)(?=^      - |\Z)",
                      source, re.MULTILINE | re.DOTALL)
    if match is None:
        raise AssertionError(f"missing workflow step: {name}")
    return match.group(1)


def step_enabled(name, row, failed=False, build_outcome="success"):
    match = re.search(r"^        if: \$\{\{ (.*) \}\}$", workflow_step(name), re.MULTILINE)
    if match is None:
        raise AssertionError(f"missing workflow condition: {name}")
    expression = match.group(1).replace("matrix.system", "system").replace("matrix.target", "target")
    expression = expression.replace("steps.haskell_build.outcome", "build_outcome")
    expression = expression.replace("failure()", "failed").replace("&&", "and").replace("||", "or")
    return bool(eval(expression, {"__builtins__": {}}, {
        **row, "failed": failed, "build_outcome": build_outcome,
    }))


class Matrix(unittest.TestCase):
    def test_default_covers_all_21_outputs_once(self):
        selected = outputs("all")
        self.assertEqual(len(matrix.select("all")["include"]), 8)
        self.assertEqual(len(selected), 21)
        self.assertEqual(len(set(selected)), 21)
        self.assertEqual(selected, [output for system in [
            "x86_64-linux", "aarch64-linux", "aarch64-darwin", "x86_64-darwin",
        ] for output in listed_outputs(system, "all")])

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

    def test_intel_dispatch_splits_all_four_existing_outputs_in_order(self):
        selected = matrix.select("x86_64-darwin")
        self.assertEqual(selected, {"include": [
            dict(system="x86_64-darwin", runner="macos-15-intel", target="native"),
            dict(system="x86_64-darwin", runner="macos-15-intel", target="darwin-libs"),
        ]})
        native = [
            "hydraJobs.x86_64-darwin.lib:simplex-chat",
            "hydraJobs.x86_64-darwin.exe:simplex-chat",
        ]
        mobile = [
            "hydraJobs.x86_64-darwin.x86_64-darwin-ios:lib:simplex-chat",
            "hydraJobs.x86_64-darwin.x86_64-darwin:lib:simplex-chat",
        ]
        self.assertEqual(listed_outputs("x86_64-darwin", "native"), native)
        self.assertEqual(listed_outputs("x86_64-darwin", "darwin-libs"), mobile)
        self.assertEqual(outputs("x86_64-darwin"), native + mobile)
        cli = subprocess.check_output([
            "python3", str(HERE / "ci-matrix.py"), "--group", "x86_64-darwin"], text=True)
        self.assertEqual(json.loads(cli), selected)

    def test_intel_mobile_dispatch_is_only_the_two_existing_library_outputs(self):
        selected = matrix.select("x86_64-darwin-libs")
        self.assertEqual(selected, {"include": [dict(
            system="x86_64-darwin", runner="macos-15-intel", target="darwin-libs")]})
        self.assertEqual(outputs("x86_64-darwin-libs"), [
            "hydraJobs.x86_64-darwin.x86_64-darwin-ios:lib:simplex-chat",
            "hydraJobs.x86_64-darwin.x86_64-darwin:lib:simplex-chat",
        ])
        cli = subprocess.check_output([
            "python3", str(HERE / "ci-matrix.py"), "--group", "x86_64-darwin-libs"], text=True)
        self.assertEqual(json.loads(cli), selected)

    def test_workflow_manual_choices_match_the_selector(self):
        source = (HERE.parent.parent / ".github/workflows/nix.yml").read_text()
        choices = re.search(r"^        options: \[(.*)\]$", source, re.MULTILINE)
        self.assertIsNotNone(choices)
        expected = ["all", "linux-static", "android", "windows", "x86_64-darwin", "x86_64-darwin-libs"]
        self.assertEqual(choices.group(1).split(", "), expected)
        for group in expected:
            with self.subTest(group=group):
                cli = subprocess.check_output([
                    "python3", str(HERE / "ci-matrix.py"), "--group", group], text=True)
                self.assertEqual(json.loads(cli), matrix.select(group))

    def test_intel_policy_and_info_table_checks_cover_both_groups(self):
        for row in matrix.select("all")["include"]:
            for step in ["Check Intel Darwin build policies",
                         "Check native Intel Darwin GHC info-table placement"]:
                with self.subTest(row=row, step=step):
                    self.assertEqual(step_enabled(step, row), row["system"] == "x86_64-darwin")
        for target in ["native", "darwin-libs"]:
            row = dict(system="x86_64-darwin", target=target)
            self.assertTrue(step_enabled("Check Intel Darwin build policies", row))
            self.assertTrue(step_enabled("Check native Intel Darwin GHC info-table placement", row))

    def test_intel_setup_diagnostic_keeps_exact_failure_gate_in_both_groups(self):
        for row in matrix.select("all")["include"]:
            for failed in [False, True]:
                for outcome in ["success", "failure", "cancelled", "skipped"]:
                    with self.subTest(row=row, failed=failed, outcome=outcome):
                        self.assertEqual(step_enabled("Capture Intel Darwin ghc-boot Setup failure", row,
                                                      failed, outcome),
                                         row["system"] == "x86_64-darwin" and failed and outcome == "failure")

    def test_native_cli_smoke_matches_the_actual_output_group(self):
        for row in matrix.select("all")["include"]:
            native_cli = f'hydraJobs.{row["system"]}.exe:simplex-chat'
            with self.subTest(row=row):
                self.assertEqual(step_enabled("Check native CLI encryption and profile passwords", row),
                                 native_cli in listed_outputs(row["system"], row["target"]))

    def test_split_artifacts_and_existing_execution_bounds(self):
        source = (HERE.parent.parent / ".github/workflows/nix.yml").read_text()
        template = "nix-${{ matrix.system }}-${{ matrix.target }}-${{ github.sha }}"
        self.assertEqual(source.count("name: " + template), 1)
        names = [template.replace("${{ matrix.system }}", row["system"])
                 .replace("${{ matrix.target }}", row["target"])
                 .replace("${{ github.sha }}", "same-source-sha")
                 for row in matrix.select("all")["include"]]
        self.assertEqual(len(set(names)), len(names))
        self.assertRegex(source, r"(?m)^    timeout-minutes: 360$")
        self.assertRegex(source, r"(?m)^      NIX_BUILD_TIMEOUT: 7200$")
        self.assertRegex(source, r"(?m)^      fail-fast: false$")
        self.assertIn("timeout-minutes: 1", workflow_step("Check Intel Darwin build policies"))
        self.assertIn("timeout-minutes: 10", workflow_step("Check native Intel Darwin GHC info-table placement"))
        self.assertIn("timeout-minutes: 120", workflow_step("Capture Intel Darwin ghc-boot Setup failure"))

    def test_manual_groups_have_distinct_queues_without_changing_pr_or_push_keys(self):
        source = (HERE.parent.parent / ".github/workflows/nix.yml").read_text()
        groups = re.findall(r"^  group: (.*)$", source, re.MULTILINE)
        self.assertEqual(len(groups), 1)
        self.assertRegex(source, r"(?m)^  cancel-in-progress: false$")
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
            for group in [None, "all", "linux-static", "android", "windows", "x86_64-darwin", "x86_64-darwin-libs"]:
                self.assertEqual(queue_key(event, group), "nix-Nix Haskell builds-refs/heads/branch")
        keys = [queue_key("workflow_dispatch", group)
                for group in ["all", "linux-static", "android", "windows", "x86_64-darwin", "x86_64-darwin-libs"]]
        self.assertEqual(len(set(keys)), 6)
        self.assertEqual(queue_key("workflow_dispatch", ""), keys[0])
        self.assertEqual(queue_key("workflow_dispatch", None), keys[0])

    def test_legacy_jobs_preserve_running_pr_and_non_pr_builds(self):
        source = (HERE.parent.parent / ".github/workflows/build.yml").read_text()
        values = re.findall(r"^      cancel-in-progress: (.*)$", source, re.MULTILINE)
        self.assertEqual(len(values), 3)
        # Master queues non-PR jobs; this migration also preserves running PR
        # builds. The ARM Linux push job shares the Linux concurrency policy.
        self.assertRegex(source, r"(?m)^    concurrency: &linux-concurrency$")
        push_job = re.search(r"(?ms)^  build-linux-push:\n(.*?)(?=^  \S|\Z)", source)
        self.assertIsNotNone(push_job)
        self.assertRegex(push_job.group(1), r"(?m)^    concurrency: \*linux-concurrency$")
        for value in values:

            def evaluate(event, ref, libs_tag):
                if value in ("true", "false"):
                    return value == "true"
                self.assertTrue(value.startswith("${{ ") and value.endswith(" }}"))
                # Evaluate these actual, narrowly constrained workflow
                # expressions with GitHub's relevant boolean/string semantics.
                code = value[4:-3].replace("github.event_name", "event").replace("github.ref", "ref")
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
                            self.assertFalse(evaluate(event, ref, libs_tag))

    def test_unknown_group_rejected(self):
        with self.assertRaises(ValueError):
            matrix.select("unknown")
        result = subprocess.run(["python3", str(HERE / "ci-matrix.py"), "--group", "unknown"],
                                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
