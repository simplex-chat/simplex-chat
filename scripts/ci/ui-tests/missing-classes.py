#!/usr/bin/env python3

# Fails when test classes a desktop shard should have run have no JUnit XML report: a hard timeout (Runtime.halt) ends
# a test JVM, and the classes it had not run yet are then missing from the reports, not failed.
# The expected classes are the test classes under the desktop UI test roots that the shard's --tests patterns select.
# Usage: scripts/ci/ui-tests/missing-classes.py <test-results dir> <pattern>...

import fnmatch
import glob
import os
import re
import sys
import xml.etree.ElementTree as ET

repo = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
src = os.path.join(repo, "apps/multiplatform/common/src")
roots = [os.path.join(src, "uiTestShared/kotlin/chat/simplex/app/ui"), os.path.join(src, "desktopTest/kotlin/chat/simplex/app/ui")]
package_re = re.compile(r"^package\s+([\w.]+)", re.M)
class_re = re.compile(r"^(?:public\s+|internal\s+|open\s+)*class\s+(\w+)", re.M)
test_re = re.compile(r"^\s*@Test\b", re.M)


def test_classes():
    classes = set()
    for root in roots:
        for path in glob.glob(os.path.join(root, "**", "*.kt"), recursive=True):
            text = open(path, errors="replace").read()
            package = package_re.search(text)
            if test_re.search(text) and package:
                classes.update(f"{package.group(1)}.{name}" for name in class_re.findall(text))
    return classes


def selected(cls, patterns):
    simple = cls.rsplit(".", 1)[-1]
    for p in patterns:
        method_stripped = p.rsplit(".", 1)[0]
        for q in (p, method_stripped) if "*" not in method_stripped else (p,):
            if fnmatch.fnmatchcase(cls, q) or (q[:1].isupper() and fnmatch.fnmatchcase(simple, q)):
                return True
    return False


results_dir, patterns = sys.argv[1], sys.argv[2:]
expected = {c for c in test_classes() if selected(c, patterns)}
reported = set()
for path in glob.glob(os.path.join(results_dir, "*.xml")):
    try:
        reported.add(ET.parse(path).getroot().get("name"))
    except ET.ParseError:
        pass
missing = sorted(expected - reported)
if not missing:
    print(f"All {len(expected)} expected test classes have results.")
    sys.exit(0)
summary = ["", f"**{len(missing)} expected classes missing** of {len(expected)}: no results, likely lost to a hard timeout of their test JVM (see the step log):", ""]
summary += [f"- {c.removeprefix('chat.simplex.app.ui.')}" for c in missing]
print("\n".join(summary))
print(f"::error::{len(missing)} expected classes missing from the test results")
if os.environ.get("GITHUB_STEP_SUMMARY"):
    with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as f:
        f.write("\n".join(summary) + "\n")
sys.exit(1)
