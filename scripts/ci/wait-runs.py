#!/usr/bin/env python3

# Waits until every Actions run for a commit has completed, then prints each run and, for a run that did not succeed,
# its jobs that did not succeed.
# If the commit has no runs, it waits for the first one, without a time limit.
# The repository is GITHUB_REPOSITORY, or origin's on GitHub; GH_TOKEN is needed for a private one.
# Usage: scripts/ci/wait-runs.py [<commit>] [<poll secs>]

import json
import os
import re
import subprocess
import sys
import time
import urllib.request


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout.strip()


def api(path: str) -> dict:
    headers = {"Accept": "application/vnd.github+json"}
    if token := os.environ.get("GH_TOKEN"):
        headers["Authorization"] = f"Bearer {token}"
    request = urllib.request.Request(f"https://api.github.com/repos/{repo}/{path}", headers=headers)
    with urllib.request.urlopen(request, timeout=60) as response:
        return json.load(response)


def succeeded(conclusion: str | None) -> bool:
    return conclusion in ("success", "skipped")


repo = os.environ.get("GITHUB_REPOSITORY") or re.sub(r"^(https://github\.com/|git@github\.com:)|\.git$", "", git("remote", "get-url", "origin"))
commit = git("rev-parse", "--verify", f"{sys.argv[1] if len(sys.argv) > 1 else 'HEAD'}^{{commit}}")
secs = int(sys.argv[2]) if len(sys.argv) > 2 else 120

while not (runs := api(f"actions/runs?head_sha={commit}&per_page=100")["workflow_runs"]) or any(r["status"] != "completed" for r in runs):
    time.sleep(secs)

for run in runs:
    print(run["name"], run["conclusion"], run["html_url"])
    if not succeeded(run["conclusion"]):
        for job in api(f"actions/runs/{run['id']}/jobs?per_page=100")["jobs"]:
            if not succeeded(job["conclusion"]):
                print("  ", job["conclusion"], job["name"], job["html_url"])
