#!/usr/bin/env python3

import io
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.parse
import urllib.request
import zipfile
from datetime import datetime, timedelta, timezone
from pathlib import Path

WORKFLOW = "ios-sim.yml"
ARTIFACT = "ios-sim-smoke-test"
REFERENCES = "apps/ios/SimpleX Snapshots/References"


def git(*args):
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout.strip()


def github_token():
    token = os.environ.get("GITHUB_TOKEN") or os.environ.get("GH_TOKEN")
    if token:
        return token
    try:
        return subprocess.run(["gh", "auth", "token"], check=True, capture_output=True, text=True).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        sys.exit("set GITHUB_TOKEN, or log in with gh")


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args):
        return None


class GitHub:
    def __init__(self):
        self.repo = git("remote", "get-url", "origin").split("github.com", 1)[1].lstrip(":/").removesuffix(".git")
        self.token = github_token()

    def request(self, url, body=None):
        return urllib.request.Request(
            url if url.startswith("https://") else f"https://api.github.com/repos/{self.repo}{url}",
            data=None if body is None else json.dumps(body).encode(),
            headers={"Authorization": f"Bearer {self.token}", "Accept": "application/vnd.github+json"},
        )

    def get(self, path):
        with urllib.request.urlopen(self.request(path)) as r:
            return json.loads(r.read())

    def post(self, path, body):
        urllib.request.urlopen(self.request(path, body)).close()

    def download(self, url):
        try:
            urllib.request.build_opener(NoRedirect).open(self.request(url)).close()
            sys.exit(f"no redirect from {url}")
        except urllib.error.HTTPError as e:
            if e.code not in (301, 302, 303, 307, 308):
                raise
            location = e.headers["Location"]
        with urllib.request.urlopen(location) as r:
            return r.read()


def run_workflow(github, branch):
    head = git("rev-parse", "HEAD")
    quoted = urllib.parse.quote(branch, safe="")
    remote_head = github.get(f"/branches/{quoted}")["commit"]["sha"]
    if remote_head != head:
        sys.exit(f"{branch} is at {remote_head[:9]} on GitHub and at {head[:9]} here: push it first")
    dispatched = datetime.now(timezone.utc) - timedelta(minutes=1)
    github.post(f"/actions/workflows/{WORKFLOW}/dispatches", {"ref": branch})
    run = None
    while run is None:
        time.sleep(15)
        runs = github.get(f"/actions/workflows/{WORKFLOW}/runs?branch={quoted}&event=workflow_dispatch&per_page=10")["workflow_runs"]
        run = next((r for r in runs if r["head_sha"] == head and datetime.fromisoformat(r["created_at"].replace("Z", "+00:00")) >= dispatched), None)
    print(f"run {run['html_url']}", flush=True)
    while run["status"] != "completed":
        time.sleep(60)
        run = github.get(f"/actions/runs/{run['id']}")
    print(f"run {run['id']}: {run['conclusion']}")
    return run


def download_snapshots(github, run):
    artifacts = github.get(f"/actions/runs/{run['id']}/artifacts?name={ARTIFACT}")["artifacts"]
    if not artifacts:
        sys.exit(f"run {run['id']} has no artifact {ARTIFACT}")
    target = Path(tempfile.mkdtemp(prefix="ios-snapshots-"))
    with zipfile.ZipFile(io.BytesIO(github.download(artifacts[0]["archive_download_url"]))) as z:
        for name in z.namelist():
            if name.startswith(("snapshots/", "snapshots-diff/")):
                z.extract(name, target)
    return target


def record(downloaded):
    renders = downloaded / "snapshots"
    files = sorted({e["file"] for index in renders.glob("index-*.json") for e in json.loads(index.read_text())})
    if not files:
        sys.exit(f"no snapshots in {renders}")
    references = Path(git("rev-parse", "--show-toplevel")) / REFERENCES
    references.mkdir(parents=True, exist_ok=True)
    for old in references.glob("*.png"):
        if old.name not in files:
            old.unlink()
    for name in files:
        shutil.copyfile(renders / name, references / name)
    size = sum((references / name).stat().st_size for name in files)
    print(f"{len(files)} references, {size / 1e6:.1f} MB, in {references}")
    print(git("status", "--short", "--", str(references)))


def check(downloaded):
    result = downloaded / "snapshots" / "compare.json"
    if not result.exists():
        sys.exit(f"the run compared no snapshots; its renders are in {downloaded / 'snapshots'}")
    compared = json.loads(result.read_text())
    for name in compared["failed"]:
        print(f"{name}: {compared['results'][name]}")
    print(f"{len(compared['results'])} snapshots, {len(compared['failed'])} differ; diff images in {downloaded / 'snapshots-diff'}")
    sys.exit(1 if compared["failed"] else 0)


def main():
    if len(sys.argv) not in (2, 3) or sys.argv[1] not in ("record", "check"):
        sys.exit("usage: snapshots-remote.py record|check [branch]")
    branch = sys.argv[2] if len(sys.argv) == 3 else git("rev-parse", "--abbrev-ref", "HEAD")
    github = GitHub()
    downloaded = download_snapshots(github, run_workflow(github, branch))
    if sys.argv[1] == "record":
        record(downloaded)
    else:
        check(downloaded)


main()
