#!/usr/bin/env python3

# Prints the results in the JUnit XML reports of a Gradle test task, and adds them to the job summary on GitHub.
# Usage: scripts/ci/desktop-linux/test-summary.py <test-results dir>

import glob
import os
import sys
import xml.etree.ElementTree as ET

results_dir = sys.argv[1]
suites = [ET.parse(path).getroot() for path in sorted(glob.glob(os.path.join(results_dir, "*.xml")))]
if not suites:
    sys.exit(f"no test results in {results_dir}")

rows = ["| Suite | Tests | Failed | Skipped | Seconds |", "|---|---|---|---|---|"]
failures = []
totals = [0, 0, 0]
for suite in suites:
    tests, failed, skipped = (int(suite.get(k, 0)) for k in ("tests", "failures", "skipped"))
    failed += int(suite.get("errors", 0))
    totals = [totals[0] + tests, totals[1] + failed, totals[2] + skipped]
    rows.append(f"| {suite.get('name')} | {tests} | {failed} | {skipped} | {suite.get('time')} |")
    for case in suite.iter("testcase"):
        for problem in list(case.findall("failure")) + list(case.findall("error")):
            failures.append(f"{case.get('classname')}.{case.get('name')}: {problem.get('message')}")

summary = [f"### desktopTest: {totals[0]} tests, {totals[1]} failed, {totals[2]} skipped", ""] + rows
if failures:
    summary += ["", "Failures:", ""] + [f"- {f}" for f in failures]
print("\n".join(summary))
if os.environ.get("GITHUB_STEP_SUMMARY"):
    with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as f:
        f.write("\n".join(summary) + "\n")
