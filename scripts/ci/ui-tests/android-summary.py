#!/usr/bin/env python3

# Prints the results of an Android UI test run of apps/multiplatform/scripts/uitest-android.sh (its results.txt,
# known-issues.txt and the GAVE_UP and ABNORMAL markers), and adds them to the job summary on GitHub.
# Usage: scripts/ci/ui-tests/android-summary.py <run dir>

import collections
import glob
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import known_issues

run_dir = sys.argv[1]
results = os.path.join(run_dir, "results.txt")
if not os.path.isfile(results):
    sys.exit(f"no results.txt in {run_dir}")

outcomes = {"0": "passed", "-2": "failed", "-3": "skipped", "-4": "skipped", "crashed": "crashed", "flaky": "flaky"}
columns = ["passed", "failed", "crashed", "flaky", "skipped"]
classes = collections.defaultdict(collections.Counter)
problems = []
flaky = []
with open(results) as f:
    for line in f:
        test, _, code = line.strip().rpartition(" ")
        if not test:
            continue
        outcome = outcomes.get(code, f"code {code}")
        cls = test.split("#")[0].removeprefix("chat.simplex.app.ui.")
        classes[cls][outcome if outcome in columns else "failed"] += 1
        name = test.removeprefix("chat.simplex.app.ui.")
        if outcome == "flaky":
            flaky.append(name)
        elif outcome not in ("passed", "skipped"):
            problems.append(f"{name}: {outcome}")


def marker(name):
    path = os.path.join(run_dir, name)
    return open(path).read().strip() if os.path.isfile(path) else None


totals = collections.Counter()
for counts in classes.values():
    totals.update(counts)
rows = ["| Class | " + " | ".join(c.capitalize() for c in columns) + " |", "|---" * (len(columns) + 1) + "|"]
for cls in sorted(classes):
    rows.append(f"| {cls} | " + " | ".join(str(classes[cls][c]) for c in columns) + " |")

summary = [f"### Android UI tests: {sum(totals.values())} tests, " + ", ".join(f"{totals[c]} {c}" for c in columns), ""]
gave_up = marker("GAVE_UP")
if gave_up is not None:
    summary += [f"**The run gave up after {gave_up or 4} crashed passes; the remaining tests did not run.**", ""]
abnormal = marker("ABNORMAL")
if abnormal is not None:
    summary += [f"**The instrumentation ended abnormally ({abnormal}); the remaining tests did not run.**", ""]
summary += rows if classes else ["No test finished."]
if problems:
    summary += ["", "Failed or crashed:", ""] + [f"- {p}" for p in problems]
if flaky:
    summary += ["", "Flaky (running when the app process died, then passed when re-run; not a failure):", ""] + [f"- {t}" for t in flaky]
summary += known_issues.section(os.path.join(run_dir, "known-issues.txt"))
shots = glob.glob(os.path.join(run_dir, "failures", "*"))
if shots:
    summary += ["", f"{len(shots)} failure screenshots are in the `failures` directory of the run's artifact."]
print("\n".join(summary))
if os.environ.get("GITHUB_STEP_SUMMARY"):
    with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as f:
        f.write("\n".join(summary) + "\n")
