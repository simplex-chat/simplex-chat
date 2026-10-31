#!/usr/bin/env python3

# Lists the known app issues of a UI test run: the tests skipped for a known issue, and the intermittent known issues
# that did not reproduce, from the run's known-issues.txt (one line per test: <Class.test> TAB <SKIPPED|NOT-REPRODUCED>
# TAB <finding> TAB <message>). Prints nothing when the file is missing or empty. android-summary.py imports it.
# Usage: scripts/ci/ui-tests/known_issues.py <known-issues.txt>

import os
import sys

PREFIX = "chat.simplex.app.ui."


def section(path):
    if not path or not os.path.isfile(path):
        return []
    groups = {"SKIPPED": [], "NOT-REPRODUCED": [], "other": []}
    with open(path, errors="replace") as f:
        for line in dict.fromkeys(l.rstrip("\r\n") for l in f):
            fields = line.split("\t")
            if len(fields) >= 4 and fields[1] in groups:
                test, kind, finding, message = fields[0], fields[1], fields[2], "\t".join(fields[3:])
                groups[kind].append(f"{test.removeprefix(PREFIX)} (finding {finding}): {message}")
            elif line.strip():
                groups["other"].append(line.strip())
    titles = {
        "SKIPPED": "Skipped for a known app issue",
        "NOT-REPRODUCED": "Intermittent known issues that did not reproduce (passed)",
        "other": "Unrecognised known-issue lines",
    }
    out = []
    for kind, items in groups.items():
        if items:
            out += ["", f"{titles[kind]} ({len(items)}):", ""] + [f"- {i}" for i in items]
    return out


if __name__ == "__main__":
    lines = section(sys.argv[1])
    if lines:
        print("\n".join(lines))
        if os.environ.get("GITHUB_STEP_SUMMARY"):
            with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as f:
                f.write("\n".join(lines) + "\n")
