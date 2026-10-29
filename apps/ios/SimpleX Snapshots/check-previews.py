#!/usr/bin/env python3

import re
import sys
from pathlib import Path

EXCLUDED = {
    "AppearanceSettings_Previews",
    "UserProfilesView_Previews",
}

tests = Path(__file__).with_name("SnapshotTests.swift")
ios = tests.parents[1]
listed = set(re.findall(r"SnapshotScene\((\w+)\.self", tests.read_text()))
errors = []
for path in sorted(ios.rglob("*.swift")):
    source = path.read_text()
    name = path.relative_to(ios.parent.parent)
    for preview in re.findall(r"^[ \t]*struct[ \t]+(\w+)[ \t]*:[ \t]*PreviewProvider\b", source, re.M):
        if preview not in listed and preview not in EXCLUDED:
            errors.append(f"{name}: {preview} is not in previewScenes() of {tests.name}")
    if re.search(r"^[ \t]*#Preview\b", source, re.M):
        errors.append(f"{name}: #Preview is not rendered, write a PreviewProvider and add it to previewScenes() of {tests.name}")
print("\n".join(errors) or f"{len(listed)} previews are rendered, {len(EXCLUDED)} are excluded")
sys.exit(1 if errors else 0)
