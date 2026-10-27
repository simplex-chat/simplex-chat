import argparse
import datetime
import tempfile
import unittest
from pathlib import Path

import flakes

FIXTURES = Path(__file__).resolve().parent / "fixtures"
CHAT = "simplex-chat/simplex-chat"
SMP = "simplex-chat/simplexmq"
LINUX = "ubuntu-22.04-x86_64 (CLI,Desktop), GHC: 9.6.3"
LINUX_GHC8 = "ubuntu-22.04-x86_64 (CLI,Desktop), GHC: 8.10.7"
LINUX_SMP = "ubuntu-22.04-x86-64, GHC: 8.10.7"
MASTER = {"id": 1, "event": "push", "head_branch": "master", "head_sha": "a" * 40, "created_at": "2026-10-20T12:44:00Z"}
DISPATCH = {**MASTER, "event": "workflow_dispatch", "head_branch": "ep/ci-flakes"}
TIME = datetime.datetime(2026, 10, 26, tzinfo=datetime.timezone.utc)


def fixture(name: str) -> str:
    return (FIXTURES / name).read_text(encoding="utf-8")


def lines(name: str) -> list[flakes.Line]:
    return flakes.log_lines(fixture(name))


def job(name, test, conclusion, earlier=(("Build CLI", "success"),), step="Run tests", status="completed", job_id=2):
    steps = [{"name": n, "number": i, "conclusion": c} for i, (n, c) in enumerate(earlier, 1)]
    if test != "absent":
        steps.append({"name": step, "number": len(steps) + 1, "conclusion": test})
    return {
        "id": job_id,
        "run_attempt": 1,
        "name": name,
        "status": status,
        "conclusion": conclusion,
        "html_url": f"https://github.com/simplex-chat/simplex-chat/actions/runs/1/job/{job_id}",
        "steps": steps,
    }


def record(repository, job, log=None, run=MASTER):
    def read_log():
        if log is None:
            raise AssertionError("the log is not needed")
        return fixture(log) if log else None

    return flakes.job_record(repository, run, job, read_log)[0]


class OutcomeTest(unittest.TestCase):
    def test_failed_tests_of_both_hspec_versions(self):
        failed = record(CHAT, job(LINUX, "failure", "failure"), "hspec-2.11-failures.log")
        self.assertEqual((failed["outcome"], len(failed["failures"]), failed["summaries"]), ("failed", 3, [3]))
        self.assertEqual(failed["job_key"], "ubuntu-22.04-x86_64 GHC 9.6.3")
        failed = record(CHAT, job(LINUX_GHC8, "failure", "failure"), "hspec-2.7-failures.log")
        self.assertEqual((failed["outcome"], len(failed["failures"]), failed["summaries"]), ("failed", 12, [12]))

    def test_step_timeouts_of_both_hspec_versions(self):
        for log in ("hspec-2.11-timeout.log", "hspec-2.7-timeout.log"):
            timeout = record(CHAT, job(LINUX, "failure", "failure"), log)
            self.assertEqual(timeout["outcome"], "timeout")
            self.assertEqual(timeout["hang"]["description"], "verify .lint fkey-indexes")

    def test_build_failures(self):
        expected = {
            "build-ghc-error.log": (
                "src/Simplex/Chat/View.hs:2873:20: error: [-Wincomplete-patterns, -Werror=incomplete-patterns]"
            ),
            "build-sigkill.log": "build process killed (SIGKILL)",
            "build-fatal.log": "fatal: could not get a repository handle for submodule 'cbits/libsecp256k1'",
        }
        for log, raw in expected.items():
            failed = record(CHAT, job(LINUX, "skipped", "failure", earlier=(("Build CLI", "failure"),)), log)
            self.assertEqual(failed["outcome"], "build failed")
            self.assertEqual((failed["build"]["step"], failed["build"]["raw"]), ("Build CLI", raw))

    def test_test_step_failure_without_failed_tests_or_timeout_is_a_crash(self):
        empty = fixture("no-tests-selected.log").replace("No tests selected", "0 examples, 0 failures")
        for log in (fixture("build-fatal.log"), empty):
            crashed = flakes.job_record(CHAT, MASTER, job(LINUX, "failure", "failure"), lambda: log)[0]
            self.assertEqual((crashed["outcome"], crashed["build"]["step"]), ("crashed", "Run tests"))

    def test_simplexmq_retries_are_read_from_passed_and_failed_jobs(self):
        pattern = "user error (Pattern match failure in do expression at tests/AgentTests/NotificationTests.hs:N:N-N)"
        expected = [
            {"label": None, "signature": pattern},
            {"label": None, "signature": "no notification"},
            {"label": None, "signature": pattern},
        ]
        for test, outcome in (("failure", "failed"), ("success", "passed")):
            tested = record(SMP, job(LINUX_SMP, test, test, step="Test"), "simplexmq-retries.log")
            self.assertEqual((tested["outcome"], tested["retries"]), (outcome, expected))

    def test_simplexmq_retries_are_keyed_by_test_label(self):
        tested = record(SMP, job(LINUX_SMP, "success", "success", step="Test"), "simplexmq-retry-labels.log")
        expected = [{"label": "should expire one message", "signature": "inAnyOrder timeout"}]
        self.assertEqual((tested["outcome"], tested["retries"]), ("passed", expected))

    def test_job_without_selected_tests_is_not_tested_in_manual_and_pull_request_runs(self):
        for run in (DISPATCH, PULL_RUN):
            untested = record(CHAT, job(LINUX, "success", "success"), "no-tests-selected.log", run)
            self.assertEqual(untested["outcome"], "not tested")

    def test_pull_request_body_lines_are_not_test_output(self):
        body = [
            "No tests selected",
            "Failures:",
            "  1) Agent, should expire one message",
            '  To rerun use: --match "/Agent/should expire one message/"',
            'Test exception: "should expire one message", inAnyOrder timeout',
            "1443 examples, 1 failure",
        ]
        log = fixture("simplexmq-retry-labels.log")
        lines = log.split("\n")
        after = next(i for i, line in enumerate(lines, 1) if line.endswith("  PR_BODY: "))
        with_body = "\n".join(lines[:after] + body + lines[after:])
        for test in ("success", "failure"):
            tested = job(LINUX_SMP, test, test, step="Test")
            plain = flakes.job_record(SMP, PULL_RUN, tested, lambda: log)[0]
            self.assertEqual(flakes.job_record(SMP, PULL_RUN, tested, lambda: with_body)[0], plain)

    def test_passed_job_on_stable_or_a_tag_keeps_failures_of_earlier_attempts(self):
        for branch in ("stable", "v6.4.1"):
            run = {**MASTER, "head_branch": branch}
            passed = record(CHAT, job(LINUX, "success", "success"), "hspec-2.11-failures.log", run)
            self.assertEqual((passed["outcome"], len(passed["failures"])), ("passed", 3))

    def test_outcomes_without_logs(self):
        cases = [
            (job(LINUX, "success", "success"), "passed"),
            (job(LINUX, None, "failure"), "runner lost"),
            (job(LINUX, "absent", "cancelled", earlier=()), "cancelled"),
            (job(LINUX, "cancelled", "cancelled"), "cancelled"),
            (job(LINUX, "skipped", "success"), "not tested"),
            (job(LINUX, None, None, status="in_progress"), "in progress"),
        ]
        for tested, outcome in cases:
            self.assertEqual(record(CHAT, tested)["outcome"], outcome)

    def test_missing_log(self):
        self.assertEqual(record(CHAT, job(LINUX, "failure", "failure"), "")["outcome"], "log missing")

    def test_cancelled_job_without_steps_is_counted_when_its_job_key_is_tested(self):
        cancelled = job(LINUX, "absent", "cancelled", earlier=())
        self.assertTrue(flakes.is_counted(cancelled, "Run tests", {flakes.job_key(LINUX)}))
        self.assertFalse(flakes.is_counted(cancelled, "Run tests", set()))

    def test_job_keys(self):
        self.assertEqual(flakes.job_key("macos-latest (CLI,Desktop), GHC: 9.6.3"), "macos-latest GHC 9.6.3")
        self.assertEqual(flakes.job_key(LINUX_SMP), "ubuntu-22.04-x86-64 GHC 8.10.7")
        self.assertIsNone(flakes.job_key("resolver (python)"))
        self.assertIsNone(flakes.job_key("macos-latest (iOS)"))
        self.assertIsNone(flakes.job_key("ubuntu-${{ matrix.os }} (CLI,Desktop), GHC: ${{ matrix.ghc }}"))


class FailureTest(unittest.TestCase):
    def test_failures_equal_the_summary_count(self):
        counts = {"hspec-2.11-failures.log": 3, "hspec-2.7-failures.log": 12, "simplexmq-retries.log": 1}
        for log, count in counts.items():
            output = flakes.test_output(lines(log))
            self.assertEqual((len(output.failures), sum(output.summaries)), (count, count))

    def test_failure_records(self):
        failure = flakes.test_output(lines("hspec-2.7-failures.log")).failures[1]
        self.assertEqual(
            failure["test"], "/SimpleX chat client/direct tests/maintenance mode/export/import chat with files/"
        )
        self.assertEqual(failure["location"], "tests/ChatTests/Direct.hs:114:40")
        self.assertEqual(
            failure["reason"],
            [
                "uncaught exception: ErrorCall",
                'alice: no output for 20 seconds, expected: "completed receiving file 1 (test.jpg) from bob"',
            ],
        )
        self.assertEqual(
            failure["signature"],
            'uncaught exception: ErrorCall | alice: no output for N seconds, expected: "completed receiving file ',
        )

    def test_byte_order_marks_and_carriage_returns_are_removed(self):
        log = fixture("hspec-2.11-failures.log")
        marked = "\n".join(chr(0xFEFF) + line if "To rerun use:" in line else line for line in log.split("\n"))
        for variant in (marked, log.replace("\n", "\r\n")):
            self.assertEqual(flakes.log_lines(variant), flakes.log_lines(log))

    def test_reason_keeps_3_lines_and_the_signature_all_lines(self):
        texts = ["Failures:", "  1) a", *(f"       line {n}" for n in range(1, 5)), '  To rerun use: --match "/a/"']
        failure = flakes.test_output([flakes.Line(TIME, text) for text in texts]).failures[0]
        self.assertEqual(failure["reason"], ["line 1", "line 2", "line 3"])
        self.assertEqual(failure["signature"], "line N | line N | line N | line N")

    def test_both_hspec_versions_give_one_signature(self):
        current = flakes.test_output(lines("hspec-2.11-failures.log")).failures[2]
        older = flakes.test_output(lines("hspec-2.7-failures.log")).failures[0]
        self.assertTrue(current["signature"].startswith('expected: "Your new chat address is created!" | but got: '))
        self.assertEqual(current["signature"], older["signature"])

    def test_signature_replaces_identifiers_digits_and_spaces(self):
        self.assertEqual(flakes.signature("queue AbCdEfGhIjKlMnOp1  failed after 30 s"), "queue X failed after N s")
        self.assertEqual(flakes.signature("x" * 120), "x" * 100)

    def test_haskell_string(self):
        self.assertEqual(flakes.haskell_string('"/a \\"b\\"/\\955\\&1/"'), '/a "b"/λ1/')

    def test_retry_location_is_its_file(self):
        retry = (
            'Test exception: HUnitFailure (Just (SrcLoc {srcLocPackage = "p", srcLocModule = "M", '
            'srcLocFile = "tests/CoreTests/CryptoTests.hs", srcLocStartLine = 407, srcLocStartCol = 10, '
            'srcLocEndLine = 407, srcLocEndCol = 20})) (ExpectedButGot Nothing "False" "True")'
        )
        signature = 'HUnitFailure tests/CoreTests/CryptoTests.hs (ExpectedButGot Nothing "False" "True")'
        self.assertEqual(flakes.retries([flakes.Line(TIME, retry)]), [{"label": None, "signature": signature}])

    def test_failed_retry_lines_with_and_without_label(self):
        lines = [
            'Test failed: location Nothing, reason: Reason "no notification"',
            'Test failed: "sends \\"hello\\"", location Nothing, reason: ExpectedButGot Nothing "1" "2"',
        ]
        self.assertEqual(
            flakes.retries([flakes.Line(TIME, line) for line in lines]),
            [
                {"label": None, "signature": 'location Nothing, reason: Reason "no notification"'},
                {"label": 'sends "hello"', "signature": 'location Nothing, reason: ExpectedButGot Nothing "N" "N"'},
            ],
        )

    def test_build_signature_includes_the_error_message(self):
        _, key = flakes.failed_step_signature(lines("build-ghc-error.log"))
        self.assertEqual(
            key,
            "src/Simplex/Chat/View.hs:N:N: error: [-Wincomplete-patterns, -Werror=incomplete-patterns] | Pattern ",
        )

    def test_build_signature_is_the_last_line_of_the_failed_step(self):
        texts = [
            "##[group]Run git describe --tags",
            "##[endgroup]",
            "fatal: No names found, cannot describe anything.",
            "##[group]Run cabal build",
            "##[endgroup]",
            "Error: cabal: Failed to build simplexmq.",
            "",
            "##[error]Process completed with exit code 1.",
        ]
        raw, _ = flakes.failed_step_signature([flakes.Line(TIME, text) for text in texts])
        self.assertEqual(raw, "Error: cabal: Failed to build simplexmq.")


class HangTest(unittest.TestCase):
    def hang(self, log):
        parsed = lines(log)
        return flakes.hang(parsed, flakes.step_timeout(parsed, "Run tests"))

    def test_last_reported_item_and_silence(self):
        current = self.hang("hspec-2.11-timeout.log")
        self.assertEqual((current["item"], current["silence"]), ("/Schema dump/verify .lint fkey-indexes/", 7211))
        older = self.hang("hspec-2.7-timeout.log")
        self.assertEqual((older["item"], older["silence"]), ("verify .lint fkey-indexes", 7137))

    def next_item(self, timeout, spec_log):
        with tempfile.TemporaryDirectory() as directory:
            collector = flakes.Collector(None, flakes.Cache(Path(directory)))
            collector.update_spec(CHAT, {"job": 3, "job_key": timeout["job_key"]}, flakes.spec_items(lines(spec_log)))
            return collector.next_item(timeout)

    def test_next_item_is_read_from_the_spec_of_the_job_key(self):
        timeout = record(CHAT, job(LINUX, "failure", "failure"), "hspec-2.11-timeout.log")
        self.assertEqual(self.next_item(timeout, "hspec-2.11-failures.log"), "verify schema down migrations")

    def test_next_item_is_found_by_description_when_the_group_path_differs(self):
        timeout = record(CHAT, job(LINUX, "failure", "failure"), "hspec-2.11-timeout.log")
        timeout["hang"]["item"] = "/Stray output/verify .lint fkey-indexes/"
        self.assertEqual(self.next_item(timeout, "hspec-2.11-failures.log"), "verify schema down migrations")

    def test_failed_hspec_2_7_item_is_found_in_the_spec(self):
        log = fixture("hspec-2.7-timeout.log").replace(
            "  verify .lint fkey-indexes", "        should send files from CLI without /store FAILED [1]"
        )
        timeout = flakes.job_record(CHAT, MASTER, job(LINUX_GHC8, "failure", "failure"), lambda: log)[0]
        self.assertEqual(timeout["hang"]["item"], "should send files from CLI without /store")
        self.assertEqual(self.next_item(timeout, "hspec-2.7-failures.log"), "switches remote hosts")

    def test_timeout_before_the_first_item_of_the_test_step(self):
        log = "\n".join(line for line in fixture("hspec-2.7-timeout.log").split("\n") if "Z   verify " not in line)
        timeout = flakes.job_record(CHAT, MASTER, job(LINUX_GHC8, "failure", "failure"), lambda: log)[0]
        self.assertEqual((timeout["outcome"], timeout["hang"]["item"]), ("timeout", None))

    def test_spec_is_kept_from_the_newest_job(self):
        key = flakes.job_key(LINUX)
        with tempfile.TemporaryDirectory() as directory:
            collector = flakes.Collector(None, flakes.Cache(Path(directory)))
            for job_id in (3, 5, 4):
                collector.update_spec(CHAT, {"job": job_id, "job_key": key}, [[f"/{job_id}/", str(job_id)]])
            self.assertEqual(collector.cache.read(CHAT, "spec", key)["job"], 5)

    def test_spec_is_saved_from_push_runs_only(self):
        logs = {PUSH_RUN["id"]: "hspec-2.11-failures.log", PULL_RUN["id"]: "hspec-2.11-failures.log"}
        with tempfile.TemporaryDirectory() as directory:
            collector = flakes.Collector(FakeGitHub(logs), flakes.Cache(Path(directory)))
            collector.runs(CHAT, [PUSH_RUN])
            collector.runs(CHAT, [PULL_RUN])
            self.assertEqual(collector.cache.read(CHAT, "spec", flakes.job_key(LINUX))["job"], 1000)

    def test_pr_reads_the_next_item_from_the_window_on_a_cold_cache(self):
        logs = {PUSH_RUN["id"]: "hspec-2.11-failures.log", PULL_RUN["id"]: "hspec-2.11-timeout.log"}
        with tempfile.TemporaryDirectory() as directory:
            collector = flakes.Collector(FakeGitHub(logs), flakes.Cache(Path(directory)))
            report = flakes.pr_command(collector, argparse.Namespace(url=PULL["html_url"], window=30)).text
            self.assertIn("next item: verify schema down migrations", report)


def workflow_run(run_id, event, branch, created):
    return {
        "id": run_id,
        "run_attempt": 1,
        "status": "completed",
        "event": event,
        "head_branch": branch,
        "head_sha": "c" * 40,
        "created_at": created,
        "html_url": f"https://github.com/{CHAT}/actions/runs/{run_id}",
        "head_repository": {"full_name": "fork/simplex-chat"},
    }


PUSH_RUN = workflow_run(100, "push", "master", "2026-10-21T00:00:00Z")
PULL_RUN = workflow_run(200, "pull_request", "ep/x", "2026-10-22T00:00:00Z")
PULL = {
    "html_url": f"https://github.com/{CHAT}/pull/1",
    "head": {"repo": {"full_name": "fork/simplex-chat"}, "ref": "ep/x"},
    "base": {"ref": "master"},
}


class FakeGitHub:
    def __init__(self, logs):
        self.logs = logs

    def get(self, path, **params):
        if path.endswith("/pulls/1"):
            return PULL
        return {"workflow_runs": [run for run in (PULL_RUN, PUSH_RUN) if run["event"] == params["event"]]}

    def pages(self, path, key, **params):
        run_id = int(path.split("/")[-2])
        return [job(LINUX, "failure", "failure", job_id=run_id * 10)]

    def log(self, repository, job_id):
        return fixture(self.logs[job_id // 10])


def entry(run_id, created, jobs):
    run = {
        "id": run_id,
        "run_attempt": 1,
        "head_sha": "b" * 40,
        "created_at": created,
        "html_url": f"https://github.com/{CHAT}/actions/runs/{run_id}",
    }
    entries = []
    for number, (name, attempt, tests) in enumerate(jobs):
        job_id = run_id * 100 + number
        tested = job(name, "failure" if tests else "success", "failure" if tests else "success", job_id=job_id)
        failures = [{"test": t, "signature": "s", "reason": ["s"], "location": None, "step_attempt": 1} for t in tests]
        entries.append(({**tested, "run_attempt": attempt}, {
            "repository": CHAT,
            "run": run_id,
            "run_attempt": attempt,
            "job": job_id,
            "url": tested["html_url"],
            "job_key": flakes.job_key(name),
            "outcome": "failed" if tests else "passed",
            "build": None,
            "failures": failures,
            "hang": None,
            "retries": [],
            "summaries": [len(tests)],
        }))
    return flakes.RunRecords(run, entries)


class ReportTest(unittest.TestCase):
    def test_labels_compare_failures_with_the_window(self):
        window = flakes.Window(CHAT, "master", [
            entry(10, "2026-10-21T00:00:00Z", [(LINUX, 1, ["/a/"]), (LINUX_GHC8, 1, [])]),
            entry(11, "2026-10-20T00:00:00Z", [(LINUX, 1, []), (LINUX_GHC8, 1, ["/b/"])]),
        ])
        analysed = entry(20, "2026-10-22T00:00:00Z", [(LINUX, 1, ["/a/", "/b/", "/c/"]), (LINUX, 2, [])])
        report = flakes.run_report("CI failures", analysed, window, [analysed]).text
        self.assertIn("| /a/ | s | 1 of 1 | 1 of 2 | 0 | on master, passed on re-run |", report)
        self.assertIn("| /b/ | s | 1 of 1 | 0 of 2 | 1 | on master, other jobs, passed on re-run |", report)
        self.assertIn("| /c/ | s | 1 of 1 | 0 of 2 | 0 | new, passed on re-run |", report)
        self.assertIn("master, other jobs: ubuntu-22.04-x86_64 GHC 8.10.7 on 2026-10-20", report)
        self.assertIn("Parser check: 6 of 6 logs with summaries match their failure counts", report)

    def test_test_passed_on_re_run_inside_the_test_step(self):
        window = flakes.Window(CHAT, "master", [entry(10, "2026-10-21T00:00:00Z", [(LINUX, 1, [])])])
        analysed = entry(20, "2026-10-22T00:00:00Z", [(LINUX, 1, ["/a/", "/b/"])])
        tested = analysed.entries[0][1]
        tested["failures"].append({**tested["failures"][1], "step_attempt": 2})
        tested["summaries"] = [2, 1]
        report = flakes.run_report("CI failures", analysed, window, None).text
        self.assertIn("| /a/ | s | 0 of 1 | 0 | new, passed on re-run |", report)
        self.assertIn("| /b/ | s | 0 of 1 | 0 | new |", report)

    def test_build_failure_notes_other_signatures_of_the_same_job(self):
        window = flakes.Window(CHAT, "master", [entry(10, "2026-10-21T00:00:00Z", [(LINUX, 1, [])])])
        analysed = entry(20, "2026-10-22T00:00:00Z", [(LINUX, 1, [])])
        for run, raw in ((window.runs[0], "build process killed (SIGKILL)"), (analysed, "fatal: x")):
            run.entries[0][1].update(outcome="build failed", build={"step": "Build CLI", "raw": raw, "signature": raw})
        report = flakes.run_report("CI failures", analysed, window, None).text
        self.assertIn("master, same job, other signature: build process killed (SIGKILL), 1 run, 2026-10-21", report)

    def test_branch_summary_counts_outcomes_and_tests(self):
        window = flakes.Window(CHAT, "master", [
            entry(10, "2026-10-21T00:00:00Z", [(LINUX, 1, ["/a/"]), (LINUX_GHC8, 1, ["/a/"])]),
            entry(11, "2026-10-20T00:00:00Z", [(LINUX, 1, []), (LINUX_GHC8, 1, [])]),
        ])
        report = flakes.branch_report(window).text
        self.assertIn("simplex-chat/simplex-chat master: 2 runs, 2026-10-20 to 2026-10-21", report)
        self.assertIn("| 1 | ubuntu-22.04-x86_64 GHC 8.10.7 | 2 | 1 | 1 |", report)
        self.assertIn("| 1 | /a/ | 2 | 1 | 1 | 2026-10-21 |", report)

    def test_branch_summary_counts_retries_by_test(self):
        runs = [entry(run_id, "2026-10-21T00:00:00Z", [(LINUX, 1, [])]) for run_id in (10, 11)]
        runs[0].entries[0][1]["retries"] = [{"label": "a", "signature": "s1"}, {"label": None, "signature": "s2"}]
        runs[1].entries[0][1]["retries"] = [{"label": "a", "signature": "s3"}, {"label": None, "signature": "s4"}]
        report = flakes.branch_report(flakes.Window(CHAT, "master", runs)).text
        self.assertIn("Retries: 4", report)
        self.assertIn("| 1 | a | 2 | s1; s3 |", report)
        self.assertIn("| 2 | unknown | 1 | s2 |", report)
        self.assertIn("| 3 | unknown | 1 | s4 |", report)

    def test_branch_summary_leaves_out_job_keys_that_were_not_tested(self):
        window = flakes.Window(CHAT, "master", [
            entry(10, "2026-10-21T00:00:00Z", [(LINUX, 1, []), (LINUX_GHC8, 1, [])]),
        ])
        window.runs[0].entries[1][1]["outcome"] = "not tested"
        report = flakes.branch_report(window).text
        self.assertIn("| 1 | ubuntu-22.04-x86_64 GHC 9.6.3 | 1 | 1 |", report)
        self.assertNotIn("GHC 8.10.7", report)

    def test_pr_runs_exclude_runs_without_selected_tests(self):
        window = flakes.Window(CHAT, "master", [entry(10, "2026-10-21T00:00:00Z", [(LINUX, 1, [])])])
        analysed = entry(20, "2026-10-22T00:00:00Z", [(LINUX, 1, ["/a/"])])
        untested = entry(19, "2026-10-21T12:00:00Z", [(LINUX, 1, [])])
        untested.entries[0][1]["outcome"] = "not tested"
        report = flakes.run_report("CI failures", analysed, window, [analysed, untested]).text
        self.assertIn("| /a/ | s | 1 of 1 | 0 of 1 | 0 | new |", report)

    def test_test_report_includes_the_parser_check(self):
        window = flakes.Window(CHAT, "master", [entry(10, "2026-10-21T00:00:00Z", [(LINUX, 1, ["/a/"])])])
        report = flakes.test_report(window, "/a/").text
        self.assertIn("Parser check: 1 of 1 logs with summaries match their failure counts", report)

    def test_base_of_a_run(self):
        other = {"base": {"ref": "dev", "repo": {"id": 2}}}
        own = {"base": {"ref": "stable", "repo": {"id": 1}}}

        def base(event, pulls):
            run = {"event": event, "head_branch": "master", "repository": {"id": 1}, "pull_requests": pulls}
            return flakes.run_base(run, "release")

        self.assertEqual(base("push", [other]), "master")
        self.assertEqual(base("pull_request", [other, own]), "stable")
        self.assertEqual(base("workflow_dispatch", [other]), "release")

    def test_urls(self):
        pull = "https://github.com/simplex-chat/simplex-chat/pull/7703"
        job_url = "https://github.com/simplex-chat/simplexmq/actions/runs/5/job/6"
        self.assertEqual(flakes.parse_url(pull, "pull"), (CHAT, 7703))
        self.assertEqual(flakes.parse_url(job_url, "actions/runs"), (SMP, 5))
        self.assertEqual(flakes.parse_url("https://github.com/simplex-chat/simplexmq", None), (SMP, None))


if __name__ == "__main__":
    unittest.main()
