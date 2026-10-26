#!/usr/bin/env python3
from __future__ import annotations

import argparse
import concurrent.futures
import datetime
import json
import os
import re
import sys
import tempfile
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter, defaultdict
from collections.abc import Callable
from dataclasses import dataclass, field
from email.message import Message
from pathlib import Path

API = "https://api.github.com"
USER_AGENT = "simplex-ci-flakes"
WORKFLOW = "build.yml"
MAIN_OWNER = "simplex-chat"
TEST_STEPS = {"simplex-chat": "Run tests", "simplexmq": "Test"}
RETRIED_REPOSITORIES = {"simplexmq"}
WINDOW = 30
PR_RUNS = 10
WORKERS = 4
REQUEST_ATTEMPTS = 5
TRANSIENT_STATUSES = {500, 502, 503, 504}
SIGNATURE_LENGTH = 100
REASON_LINES = 3
REASON_INDENT = " " * 7
DEFAULT_CACHE = "~/.cache/simplex-ci-flakes"
OUTCOMES = [
    "passed",
    "failed",
    "timeout",
    "crashed",
    "build failed",
    "runner lost",
    "cancelled",
    "log missing",
    "in progress",
    "not tested",
]
STEP_RAN = ("success", "failure")

STAMP = re.compile(r"^(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d)(?:\.\d+)?Z ")
COLOUR = re.compile(r"\x1b\[[0-9;]*m")
JOB_NAME = re.compile(r"^(.+?)(?: \(CLI,Desktop\))?, GHC: (\S+)$")
ITEM = re.compile(r"^( *)(.*) \[[✔✘‐]\]$")
TOP_GROUP = re.compile(r"^[A-Za-z][^:=\"\[\]{}_]*$")
FAILED_SUFFIX = re.compile(r" FAILED \[\d+\]$")
LOCATION = re.compile(r"^  (\S+:\d+:\d+): $")
NUMBERED = re.compile(r"^  \d+\) ")
RERUN = re.compile(r'^  To rerun use: --match (".*")(?: --seed -?\d+)?$')
SUMMARY = re.compile(r"^\d+ examples?, (\d+) failures?(?:, \d+ pending)?$")
GHC_ERROR = re.compile(r"^\S+:\d+:\d+: error:")
RETRY = re.compile(r"^Test (?:exception: |failed: location )(.*)$")
SRC_LOC = re.compile(r'\(?Just \(SrcLoc \{[^}]*srcLocFile = "([^"]*)"[^}]*\}\)\)?')
IDENTIFIER = re.compile(r"[A-Za-z0-9_+/=-]{16,}")
DIGITS = re.compile(r"\d+")
SPACES = re.compile(r"\s+")
HASKELL_ESCAPE = re.compile(r"\\(\d+|&|.)")
NEXT_PAGE = re.compile(r'<([^>]+)>;\s*rel="next"')
GITHUB_URL = re.compile(r"^https://github\.com/([^/]+)/([^/]+?)(?:\.git)?(?:/(pull|actions/runs)/(\d+))?(?:[/?#].*)?$")


@dataclass(frozen=True)
class Line:
    time: datetime.datetime | None
    text: str


def log_lines(text: str) -> list[Line]:
    lines = []
    for raw in text.removeprefix("\ufeff").split("\n"):
        raw = raw.removesuffix("\r")
        stamp = STAMP.match(raw)
        time = datetime.datetime.fromisoformat(stamp[1]).replace(tzinfo=datetime.timezone.utc) if stamp else None
        lines.append(Line(time, COLOUR.sub("", raw[stamp.end() :] if stamp else raw)))
    return lines


def signature(text: str) -> str:
    text = IDENTIFIER.sub(lambda identifier: "X" if DIGITS.search(identifier[0]) else identifier[0], text)
    return SPACES.sub(" ", DIGITS.sub("N", text))[:SIGNATURE_LENGTH]


def haskell_string(literal: str) -> str:
    def unescape(escape: re.Match) -> str:
        code = escape[1]
        if code.isdigit():
            return chr(int(code))
        return {"&": "", "n": "\n", "t": "\t"}.get(code, code)

    return HASKELL_ESCAPE.sub(unescape, literal[1:-1])


def reason_lines(reason: list[str]) -> list[str]:
    kept = []
    for line in reason:
        if line.strip().startswith("CallStack (from HasCallStack)"):
            break
        if line.strip():
            kept.append(line.strip())
    return kept


def failure(location: str | None, reason: list[str], literal: str, step_attempt: int) -> dict:
    lines = reason_lines(reason)
    return {
        "test": haskell_string(literal),
        "signature": signature(" | ".join(lines)),
        "reason": lines[:REASON_LINES],
        "location": location,
        "step_attempt": step_attempt,
    }


@dataclass
class TestOutput:
    failures: list[dict] = field(default_factory=list)
    summaries: list[int] = field(default_factory=list)


def test_output(lines: list[Line]) -> TestOutput:
    output = TestOutput()
    in_failures = False
    location = None
    pending = None
    for line in lines:
        text = line.text
        if text == "Failures:":
            in_failures = True
        elif in_failures and text.startswith("Randomized with seed"):
            in_failures = False
        elif in_failures:
            if match := LOCATION.match(text):
                location = match[1]
            elif NUMBERED.match(text):
                pending = (location, [])
                location = None
            elif (match := RERUN.match(text)) and pending:
                output.failures.append(failure(*pending, match[1], len(output.summaries) + 1))
                pending = None
            elif pending and text.startswith(REASON_INDENT):
                pending[1].append(text[len(REASON_INDENT) :])
        elif match := SUMMARY.match(text):
            output.summaries.append(int(match[1]))
    return output


def output_start(lines: list[Line], end: int) -> int:
    return max((i for i in range(end) if lines[i].text == "##[endgroup]"), default=-1) + 1


def indentation(text: str) -> int:
    return len(text) - len(text.lstrip(" "))


def joined_path(names: list[str]) -> str:
    return "/" + "/".join(names) + "/"


def checks_items(region: list[Line]) -> list[tuple[Line, str, str]]:
    groups: list[tuple[int, str]] = []
    items = []
    follows_report = True
    for line in region:
        text = line.text
        indent = indentation(text)
        if item := ITEM.match(text):
            path = joined_path([name for depth, name in groups if depth < indent] + [item[2]])
            items.append((line, path, item[2]))
            follows_report = True
        elif not text.strip() or text.lstrip().startswith("# PENDING"):
            follows_report = True
        elif follows_report and is_group_line(text, indent):
            groups = [group for group in groups if group[0] < indent] + [(indent, text.strip())]
        else:
            follows_report = False
    return items


def is_group_line(text: str, indent: int) -> bool:
    if indent == 0:
        return bool(TOP_GROUP.match(text))
    return indent % 2 == 0 and not text.lstrip().startswith(("[", "#"))


def specdoc_items(region: list[Line]) -> list[tuple[Line, str, str]]:
    items = []
    for line in region:
        if indentation(line.text) >= 2 and line.text.lstrip()[:1].isalpha():
            text = FAILED_SUFFIX.sub("", line.text.strip())
            items.append((line, text, text))
    return items


def reported_items(lines: list[Line], end: int) -> list[tuple[Line, str, str]]:
    region = lines[output_start(lines, end) : end]
    if any(ITEM.match(line.text) for line in region):
        return checks_items(region)
    return specdoc_items(region)


def spec_items(lines: list[Line]) -> list[list[str]] | None:
    end = next((i for i, line in enumerate(lines) if line.text == "Failures:" or SUMMARY.match(line.text)), None)
    if end is None:
        return None
    return [[item, description] for _, item, description in reported_items(lines, end)]


def step_timeout(lines: list[Line], step: str) -> int | None:
    marker = f"##[error]The action '{step}' has timed out after "
    return next((i for i, line in enumerate(lines) if line.text.startswith(marker)), None)


def iso(time: datetime.datetime | None) -> str | None:
    return time.strftime("%Y-%m-%dT%H:%M:%SZ") if time else None


def hang(lines: list[Line], timeout: int) -> dict:
    items = reported_items(lines, timeout)
    if not items:
        return {"item": None, "description": None, "time": None, "silence": None, "next": None}
    line, item, description = items[-1]
    end = lines[timeout].time
    silence = int((end - line.time).total_seconds()) if end and line.time else None
    return {"item": item, "description": description, "time": iso(line.time), "silence": silence, "next": None}


def failed_step_signature(lines: list[Line]) -> tuple[str, str]:
    error = next((i for i, line in enumerate(lines) if line.text.startswith("##[error]")), len(lines))
    start = max((i for i in range(error) if lines[i].text.startswith("##[group]Run ")), default=0)
    section = [line.text for line in lines[start:error]]
    for i, text in enumerate(section):
        if GHC_ERROR.match(text):
            following = next((rest.strip() for rest in section[i + 1 :] if rest.strip()), "")
            return text.strip(), signature(f"{text.strip()} | {following}")
    if any("killed (i.e. SIGKILL)" in text for text in section):
        return "build process killed (SIGKILL)", signature("build process killed (SIGKILL)")
    if fatal := next((text for text in section if text.startswith("fatal: ")), None):
        return fatal, signature(fatal)
    last = next((text.strip() for text in reversed(section) if text.strip()), "")
    return last, signature(last)


def build_failure(lines: list[Line], step: str) -> dict:
    raw, key = failed_step_signature(lines)
    return {"step": step, "raw": raw, "signature": key}


def retries(lines: list[Line]) -> list[str]:
    return [signature(SRC_LOC.sub(r"\1", match[1])) for line in lines if (match := RETRY.match(line.text))]


def job_key(name: str) -> str | None:
    match = JOB_NAME.match(name)
    if match is None or "${{" in name:
        return None
    return f"{match[1]} GHC {match[2]}"


def repository_name(repository: str) -> str:
    return repository.split("/")[1]


def main_repository(repository: str) -> str:
    return f"{MAIN_OWNER}/{repository_name(repository)}"


def test_step(repository: str) -> str:
    name = repository_name(repository)
    if name not in TEST_STEPS:
        sys.exit(f"{repository}: the test step of {name} is unknown")
    return TEST_STEPS[name]


def step_of(job: dict, name: str) -> dict | None:
    return next((step for step in job["steps"] if step["name"] == name), None)


def step_ran(job: dict, name: str) -> bool:
    step = step_of(job, name)
    return step is not None and step["conclusion"] in STEP_RAN


def is_counted(job: dict, test: str, tested: set[str]) -> bool:
    key = job_key(job["name"])
    if key is None:
        return False
    return step_of(job, test) is not None or (not job["steps"] and job["conclusion"] == "cancelled" and key in tested)


def retried_attempts(run: dict) -> bool:
    branch = run["head_branch"] or ""
    return run["event"] == "push" and (branch == "stable" or branch.startswith("v"))


def job_record(
    repository: str, run: dict, job: dict, read_log: Callable[[], str | None]
) -> tuple[dict, list[list[str]] | None]:
    record = {
        "repository": repository,
        "run": run["id"],
        "run_attempt": job["run_attempt"],
        "job": job["id"],
        "url": job["html_url"],
        "event": run["event"],
        "branch": run["head_branch"],
        "sha": run["head_sha"],
        "created": run["created_at"],
        "name": job["name"],
        "job_key": job_key(job["name"]),
        "outcome": None,
        "build": None,
        "failures": [],
        "hang": None,
        "retries": [],
        "summaries": [],
    }
    name = test_step(repository)
    step = step_of(job, name)
    conclusion = step["conclusion"] if step else None
    earlier_failure = next(
        (s for s in job["steps"] if s["conclusion"] == "failure" and step and s["number"] < step["number"]), None
    )
    retried = repository_name(repository) in RETRIED_REPOSITORIES
    if job["status"] != "completed":
        record["outcome"] = "in progress"
    elif conclusion == "success":
        record["outcome"] = "passed"
    elif conclusion == "failure":
        record["outcome"] = "failed"
    elif conclusion == "cancelled" or job["conclusion"] == "cancelled":
        record["outcome"] = "cancelled"
    elif conclusion is None and job["conclusion"] == "failure":
        record["outcome"] = "runner lost"
    elif conclusion == "skipped" and earlier_failure:
        record["outcome"] = "build failed"
    else:
        record["outcome"] = "not tested"
    log_needed = record["outcome"] in ("failed", "build failed") or (
        record["outcome"] == "passed" and (retried or retried_attempts(run))
    )
    if not log_needed:
        return record, None
    text = read_log()
    if text is None:
        if record["outcome"] != "passed":
            record["outcome"] = "log missing"
        return record, None
    lines = log_lines(text)
    if record["outcome"] == "build failed":
        record["build"] = build_failure(lines, earlier_failure["name"])
        return record, None
    output = test_output(lines)
    record["failures"] = output.failures
    record["summaries"] = output.summaries
    timeout = step_timeout(lines, name)
    if record["outcome"] == "failed":
        if timeout is not None:
            record["outcome"] = "timeout"
            record["hang"] = hang(lines, timeout)
        elif not any(output.summaries):
            record["outcome"] = "crashed"
            record["build"] = build_failure(lines, name)
    if retried and record["outcome"] in ("passed", "failed"):
        record["retries"] = retries(lines)
    return record, spec_items(lines) if output.summaries else None


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def fetch(
    opener: urllib.request.OpenerDirector, request: urllib.request.Request, timeout: int
) -> tuple[bytes, Message]:
    for attempt in range(1, REQUEST_ATTEMPTS + 1):
        try:
            with opener.open(request, timeout=timeout) as response:
                return response.read(), response.headers
        except urllib.error.HTTPError as error:
            if error.code not in TRANSIENT_STATUSES or attempt == REQUEST_ATTEMPTS:
                raise
        except (urllib.error.URLError, TimeoutError, ConnectionError):
            if attempt == REQUEST_ATTEMPTS:
                raise
        time.sleep(2**attempt)


class GitHub:
    def __init__(self, token: str):
        self.token = token
        self.opener = urllib.request.build_opener(NoRedirect)

    def request(self, url: str) -> tuple[bytes, Message]:
        headers = {
            "Authorization": f"Bearer {self.token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
            "User-Agent": USER_AGENT,
        }
        return fetch(self.opener, urllib.request.Request(url, headers=headers), 60)

    def get(self, path: str, **params) -> dict:
        query = f"?{urllib.parse.urlencode(params)}" if params else ""
        return json.loads(self.request(f"{API}/{path}{query}")[0])

    def pages(self, path: str, key: str, **params) -> list[dict]:
        url = f"{API}/{path}?{urllib.parse.urlencode(params)}"
        items = []
        while url:
            body, headers = self.request(url)
            items += json.loads(body)[key]
            link = NEXT_PAGE.search(headers.get("Link") or "")
            url = link[1] if link else None
        return items

    def log(self, repository: str, job_id: int) -> str | None:
        try:
            return self.request(f"{API}/repos/{repository}/actions/jobs/{job_id}/logs")[0].decode("utf-8", "replace")
        except urllib.error.HTTPError as error:
            if error.code == 410:
                return None
            if error.code != 302:
                raise
            location = error.headers["Location"]
        try:
            blob = urllib.request.Request(location, headers={"User-Agent": USER_AGENT})
            return fetch(urllib.request.build_opener(), blob, 300)[0].decode("utf-8", "replace")
        except urllib.error.HTTPError as error:
            if error.code == 404:
                return None
            raise


class Cache:
    def __init__(self, root: Path):
        self.root = root

    def read(self, repository: str, kind: str, name: str):
        try:
            return json.loads((self.root / repository / kind / name).read_text())
        except FileNotFoundError:
            return None

    def write(self, repository: str, kind: str, name: str, value) -> None:
        directory = self.root / repository / kind
        directory.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile("w", dir=directory, delete=False) as file:
            json.dump(value, file)
        os.replace(file.name, directory / name)


def trimmed_job(job: dict) -> dict:
    return {
        "id": job["id"],
        "run_attempt": job["run_attempt"],
        "name": job["name"],
        "status": job["status"],
        "conclusion": job["conclusion"],
        "html_url": job["html_url"],
        "steps": [
            {"name": step["name"], "number": step["number"], "conclusion": step["conclusion"]}
            for step in job.get("steps") or []
        ],
    }


@dataclass
class RunRecords:
    run: dict
    entries: list[tuple[dict, dict]]

    def latest(self) -> dict[str, tuple[dict, dict]]:
        latest = {}
        for job, record in sorted(self.entries, key=lambda entry: entry[1]["run_attempt"]):
            latest[record["job_key"]] = (job, record)
        return latest

    def date(self) -> str:
        return self.run["created_at"][:10]


class Collector:
    def __init__(self, github: GitHub, cache: Cache):
        self.github = github
        self.cache = cache

    def run_jobs(self, repository: str, run: dict) -> list[dict]:
        name = f"{run['id']}-{run['run_attempt']}"
        jobs = self.cache.read(repository, "runs", name)
        if jobs is None:
            path = f"repos/{repository}/actions/runs/{run['id']}/jobs"
            jobs = [trimmed_job(job) for job in self.github.pages(path, "jobs", filter="all", per_page=100)]
            if run["status"] == "completed":
                self.cache.write(repository, "runs", name, jobs)
        return jobs

    def record(self, repository: str, run: dict, job: dict) -> tuple[dict, list[list[str]] | None]:
        record = self.cache.read(repository, "jobs", str(job["id"]))
        if record is not None:
            return record, None
        record, spec = job_record(repository, run, job, lambda: self.github.log(repository, job["id"]))
        if job["status"] == "completed":
            self.cache.write(repository, "jobs", str(job["id"]), record)
        return record, spec

    def runs(self, repository: str, runs: list[dict]) -> list[RunRecords]:
        name = test_step(repository)
        with concurrent.futures.ThreadPoolExecutor(WORKERS) as pool:
            listings = list(pool.map(lambda run: self.run_jobs(repository, run), runs))
            tested = {job_key(job["name"]) for jobs in listings for job in jobs if step_of(job, name)}
            counted = [(run, job) for run, jobs in zip(runs, listings) for job in jobs if is_counted(job, name, tested)]
            results = list(pool.map(lambda pair: self.record(repository, *pair), counted))
        for (_, job), (record, spec) in zip(counted, results):
            if spec is not None:
                self.update_spec(repository, record, spec)
        entries = defaultdict(list)
        for (run, job), (record, _) in zip(counted, results):
            entries[run["id"]].append((job, record))
        return [RunRecords(run, entries[run["id"]]) for run in runs]

    def update_spec(self, repository: str, record: dict, items: list[list[str]]) -> None:
        main = main_repository(repository)
        current = self.cache.read(main, "spec", record["job_key"])
        if current is None or current["job"] < record["job"]:
            self.cache.write(main, "spec", record["job_key"], {"job": record["job"], "items": items})

    def next_item(self, record: dict) -> str | None:
        spec = self.cache.read(main_repository(record["repository"]), "spec", record["job_key"])
        item = record["hang"]["item"]
        if spec is None or item is None:
            return None
        items = spec["items"]
        position = next((i for i, (path, _) in enumerate(items) if path == item), None)
        if position is None:
            description = record["hang"]["description"]
            position = next((i for i, (_, text) in enumerate(items) if text == description), None)
        if position is None or position + 1 == len(items):
            return None
        return items[position + 1][1]

    def resolve_hangs(self, runs: list[RunRecords]) -> None:
        for entry in runs:
            for _, record in entry.entries:
                if record["hang"]:
                    record["hang"]["next"] = self.next_item(record)


@dataclass
class Window:
    repository: str
    branch: str
    runs: list[RunRecords]

    def header(self) -> str:
        if not self.runs:
            return f"{self.repository} {self.branch}: 0 runs"
        dates = f"{self.runs[-1].date()} to {self.runs[0].date()}"
        return f"{self.repository} {self.branch}: {len(self.runs)} runs, {dates}"


def load_window(collector: Collector, repository: str, branch: str, before: str | None, size: int) -> Window:
    params = {
        "branch": branch,
        "event": "push",
        "status": "completed",
        "per_page": size,
        "exclude_pull_requests": "true",
    }
    if before:
        params["created"] = f"<{before}"
    path = f"repos/{repository}/actions/workflows/{WORKFLOW}/runs"
    runs = collector.github.get(path, **params)["workflow_runs"][:size]
    window = Window(repository, branch, collector.runs(repository, runs))
    collector.resolve_hangs(window.runs)
    return window


def record_keys(record: dict) -> set[tuple[str, str, str]]:
    name = test_step(record["repository"])
    keys = {("test", name, failure["test"]) for failure in record["failures"]}
    if record["hang"] and record["hang"]["description"]:
        keys.add(("hang", name, record["hang"]["description"]))
    if record["build"]:
        keys.add(("step", record["build"]["step"], record["build"]["signature"]))
    return keys


@dataclass
class Occurrence:
    job_key: str
    run: RunRecords
    record: dict


@dataclass
class Index:
    occurrences: dict[tuple, list[Occurrence]]
    step_runs: Counter


def index(runs: list[RunRecords]) -> Index:
    occurrences = defaultdict(list)
    step_runs = Counter()
    for entry in runs:
        for key, (job, record) in entry.latest().items():
            for failure_key in record_keys(record):
                occurrences[failure_key].append(Occurrence(key, entry, record))
            for step in job["steps"]:
                if step["conclusion"] in STEP_RAN:
                    step_runs[(key, step["name"])] += 1
    return Index(occurrences, step_runs)


@dataclass
class Row:
    record: dict
    subject: str
    shown: str
    key: tuple | None
    step_attempt: int | None = None


def rows(entry: RunRecords) -> list[Row]:
    result = []
    for _, record in sorted(entry.entries, key=lambda e: (e[1]["run_attempt"], e[1]["job_key"])):
        name = test_step(record["repository"])
        for failure in record["failures"]:
            shown = " | ".join(failure["reason"])[:SIGNATURE_LENGTH]
            key = ("test", name, failure["test"])
            result.append(Row(record, failure["test"], shown, key, failure["step_attempt"]))
        if record["outcome"] == "timeout":
            hang = record["hang"]
            silence = f"silent {hang['silence'] // 60} min" if hang["silence"] is not None else "silence unknown"
            shown = f"{silence}, next item: {hang['next'] or 'unknown'}"
            key = ("hang", name, hang["description"]) if hang["description"] else None
            result.append(Row(record, hang["item"] or "none reported", shown, key))
        elif record["build"]:
            build = record["build"]
            result.append(Row(record, build["step"], build["raw"], ("step", build["step"], build["signature"])))
        elif record["outcome"] not in ("passed", "failed", "not tested") or (
            record["outcome"] == "failed" and not record["failures"]
        ):
            result.append(Row(record, name, "", None))
    return result


def passed_on_rerun(row: Row, entry: RunRecords) -> bool:
    record = row.record
    later = [
        r for _, r in entry.entries if r["job_key"] == record["job_key"] and r["run_attempt"] > record["run_attempt"]
    ]
    if any(r["outcome"] == "passed" for r in later):
        return True
    if row.step_attempt is None or len(record["summaries"]) <= row.step_attempt:
        return False
    return not any(
        f["test"] == row.key[2] and f["step_attempt"] > row.step_attempt for f in record["failures"]
    )


def pr_runs_mark(row: Row, pr_runs: list[RunRecords]) -> str:
    job = row.record["job_key"]
    ran = [entry for entry in pr_runs if any(r["job_key"] == job and step_ran(j, row.key[1]) for j, r in entry.entries)]
    failed = [
        entry for entry in ran if any(r["job_key"] == job and row.key in record_keys(r) for _, r in entry.entries)
    ]
    return f"{len(failed)} of {len(ran)}"


def date_range(dates: list[str]) -> str:
    first, last = min(dates), max(dates)
    return first if first == last else f"{first} to {last}"


def runs_text(dates: list[str]) -> str:
    count = len(dates)
    return f"{count} run{'s' if count != 1 else ''}, {date_range(dates)}"


def cell(value) -> str:
    return str(value).replace("|", "\\|").replace("\n", " ")


def table(header: list[str], body: list[list]) -> str:
    lines = [header, ["---"] * len(header), *body]
    return "\n".join("| " + " | ".join(cell(value) for value in line) + " |" for line in lines)


@dataclass
class Report:
    text: str
    data: dict


def parser_check(records: list[dict]) -> str:
    parsed = [r for r in records if r["summaries"]]
    differ = [r for r in parsed if len(r["failures"]) != sum(r["summaries"])]
    matching = len(parsed) - len(differ)
    lines = [f"Parser check: {matching} of {len(parsed)} logs with summaries match their failure counts"]
    lines += [f"- {r['url']}: {len(r['failures'])} failures parsed, {sum(r['summaries'])} in summaries" for r in differ]
    return "\n".join(lines)


def all_records(*groups: list[RunRecords]) -> list[dict]:
    seen = {}
    for runs in groups:
        for entry in runs:
            for _, record in entry.entries:
                seen[record["job"]] = record
    return list(seen.values())


@dataclass
class Comparison:
    same: str = ""
    other: str = ""
    label: str = ""
    pr_runs: str = ""
    notes: list[str] = field(default_factory=list)


def compare(row: Row, entry: RunRecords, found: Index, base: str, pr_runs: list[RunRecords] | None) -> Comparison:
    if row.key is None:
        return Comparison()
    job = row.record["job_key"]
    same = [o for o in found.occurrences[row.key] if o.job_key == job]
    other = [o for o in found.occurrences[row.key] if o.job_key != job]
    label = f"on {base}" if same else f"on {base}, other jobs" if other else "new"
    if passed_on_rerun(row, entry):
        label += ", passed on re-run"
    notes = []
    if other:
        seen = sorted({(o.job_key, o.run.date()) for o in other}, key=lambda o: o[1])
        notes.append(f"   {base}, other jobs: " + "; ".join(f"{key} on {date}" for key, date in seen))
    if row.key[0] == "step":
        notes += other_signatures(row, found, base)
    return Comparison(
        same=f"{len(same)} of {found.step_runs[(job, row.key[1])]}",
        other=str(len(other)),
        label=label,
        pr_runs=pr_runs_mark(row, pr_runs) if pr_runs is not None else "",
        notes=notes,
    )


def run_report(title: str, entry: RunRecords, window: Window, pr_runs: list[RunRecords] | None) -> Report:
    found = index(window.runs)
    base = window.branch
    attempts = len({record["run_attempt"] for _, record in entry.entries})
    lines = [
        title,
        f"Run: {entry.run['html_url']}, attempt {entry.run['run_attempt']}, commit {entry.run['head_sha'][:9]}",
        f"Window: {window.header()}",
        "",
    ]
    header = ["#", "Job", "Outcome", "Test or step", "Signature"]
    header += ["PR runs"] if pr_runs is not None else []
    header += [f"{base}, same job", f"{base}, other jobs", "Label"]
    body = []
    notes = []
    data_rows = []
    for number, row in enumerate(rows(entry), 1):
        record = row.record
        comparison = compare(row, entry, found, base, pr_runs)
        job = record["job_key"] + (f", attempt {record['run_attempt']}" if attempts > 1 else "")
        cells = [number, job, record["outcome"], row.subject, row.shown]
        cells += [comparison.pr_runs] if pr_runs is not None else []
        body.append(cells + [comparison.same, comparison.other, comparison.label])
        notes += [f"{number}. {record['url']}", *comparison.notes]
        data_rows.append(
            {
                "job": record["job"],
                "subject": row.subject,
                "signature": row.shown,
                "label": comparison.label,
                "pr_runs": comparison.pr_runs,
            }
        )
    if body:
        lines.append(table(header, body))
        lines += ["", "```", *notes]
    else:
        lines += ["No failures.", "", "```"]
    passed = sorted(key for key, (_, record) in entry.latest().items() if record["outcome"] == "passed")
    lines.append(f"Passed: {', '.join(passed) if passed else 'none'}")
    lines.append("```")
    lines.append(parser_check(all_records([entry], window.runs, pr_runs or [])))
    data = {"run": [record for _, record in entry.entries], "rows": data_rows, "window": all_records(window.runs)}
    return Report("\n".join(lines), data)


def other_signatures(row: Row, found: Index, base: str) -> list[str]:
    _, step, current = row.key
    by_signature = defaultdict(list)
    for key, occurrences in found.occurrences.items():
        if key[0] == "step" and key[1] == step and key[2] != current:
            for o in occurrences:
                if o.job_key == row.record["job_key"]:
                    by_signature[key[2]].append(o)
    return [
        f"   {base}, same job, other signature: {o[0].record['build']['raw']}, {runs_text([x.run.date() for x in o])}"
        for o in by_signature.values()
    ]


def branch_report(window: Window) -> Report:
    lines = [window.header(), ""]
    counts = defaultdict(Counter)
    tests = defaultdict(list)
    timeouts = []
    builds = defaultdict(list)
    others = []
    retried = Counter()
    for entry in window.runs:
        for key, (_, record) in entry.latest().items():
            counts[key][record["outcome"]] += 1
            for test in {failure["test"] for failure in record["failures"]}:
                tests[test].append((key, entry.date()))
            if record["outcome"] == "timeout":
                timeouts.append((key, entry, record["hang"]))
            elif record["outcome"] == "build failed":
                builds[(key, record["build"]["step"], record["build"]["signature"])].append((entry, record))
            elif record["outcome"] in ("crashed", "runner lost", "log missing"):
                others.append((key, entry, record))
            retried.update(record["retries"])
    shown = sorted(
        (key for key, c in counts.items() if set(c) - {"not tested"}),
        key=lambda key: (-sum(n for o, n in counts[key].items() if o not in ("passed", "not tested")), key),
    )
    columns = [o for o in OUTCOMES if any(counts[key][o] for key in shown)]
    lines.append(table(["#", "Job", "Jobs", *columns], [
        [n, key, sum(counts[key].values()), *(counts[key][o] for o in columns)] for n, key in enumerate(shown, 1)
    ]))
    if tests:
        keys = [key for key in shown if any(k == key for occurrences in tests.values() for k, _ in occurrences)]
        ordered = sorted(tests.items(), key=lambda t: (-len(t[1]), t[0]))
        lines += ["", f"Tests by failures, {len(ordered)}:"]
        lines.append(table(["#", "Test", "Failures", *keys, "Last"], [
            [n, test, len(o), *(sum(1 for k, _ in o if k == key) for key in keys), max(d for _, d in o)]
            for n, (test, o) in enumerate(ordered, 1)
        ]))
    if timeouts:
        lines += ["", "Timeouts:"]
        lines.append(table(["#", "Job", "Run", "Last reported item", "Next item"], [
            [n, key, entry.run["html_url"], hang["item"] or "none reported", hang["next"] or "unknown"]
            for n, (key, entry, hang) in enumerate(sorted(timeouts, key=lambda t: t[1].run["created_at"]), 1)
        ]))
    if builds:
        lines += ["", "Build failures:"]
        lines.append(table(["#", "Job", "Step", "Signature", "Runs"], [
            [n, key, step, o[0][1]["build"]["raw"], f"{len(o)}, {date_range([e.date() for e, _ in o])}"]
            for n, ((key, step, _), o) in enumerate(sorted(builds.items(), key=lambda b: -len(b[1])), 1)
        ]))
    if others:
        lines += ["", "Other outcomes:"]
        lines.append(table(["#", "Job", "Outcome", "Job link", "Signature"], [
            [n, key, record["outcome"], record["url"], record["build"]["raw"] if record["build"] else ""]
            for n, (key, entry, record) in enumerate(others, 1)
        ]))
    if retried:
        lines += ["", f"Retries: {sum(retried.values())}"]
        lines.append(table(["#", "Retry signature", "Retries"], [
            [n, text, count] for n, (text, count) in enumerate(retried.most_common(), 1)
        ]))
    lines += ["", parser_check(all_records(window.runs))]
    return Report("\n".join(lines), {"window": all_records(window.runs)})


def test_report(window: Window, path: str) -> Report:
    found = []
    for entry in window.runs:
        for key, (_, record) in sorted(entry.latest().items()):
            for failure in record["failures"]:
                if failure["test"] == path:
                    found.append((key, entry, record, failure))
    lines = [window.header(), f"Test: {path}", ""]
    if found:
        lines.append(table(["#", "Job", "Date", "Signature", "Job link"], [
            [n, key, entry.date(), " | ".join(failure["reason"])[:SIGNATURE_LENGTH], record["url"]]
            for n, (key, entry, record, failure) in enumerate(found, 1)
        ]))
    else:
        lines.append("No failures in the window.")
    return Report("\n".join(lines), {"failures": [dict(f, job=r["job"], url=r["url"]) for _, _, r, f in found]})


def parse_url(url: str, kind: str | None) -> tuple[str, int | None]:
    match = GITHUB_URL.match(url)
    if match is None or match[3] != kind:
        sys.exit(f"{url}: expected a {kind or 'repository'} URL of github.com")
    return f"{match[1]}/{match[2]}", int(match[4]) if match[4] else None


def pr_command(collector: Collector, args: argparse.Namespace) -> Report:
    repository, number = parse_url(args.url, "pull")
    pull = collector.github.get(f"repos/{repository}/pulls/{number}")
    head = (pull["head"]["repo"] or {}).get("full_name")
    path = f"repos/{repository}/actions/workflows/{WORKFLOW}/runs"
    listed = collector.github.get(path, event="pull_request", branch=pull["head"]["ref"], per_page=100)["workflow_runs"]
    runs = [run for run in listed if (run["head_repository"] or {}).get("full_name") == head][:PR_RUNS]
    if not runs:
        sys.exit(f"{args.url}: no {WORKFLOW} runs")
    pr_runs = collector.runs(repository, runs)
    collector.resolve_hangs(pr_runs)
    base = pull["base"]["ref"]
    window = load_window(collector, main_repository(repository), base, runs[0]["created_at"], args.window)
    return run_report(f"CI failures: {pull['html_url']}", pr_runs[0], window, pr_runs)


def run_base(run: dict, base: str) -> str:
    if run["pull_requests"]:
        return run["pull_requests"][0]["base"]["ref"]
    return run["head_branch"] if run["event"] == "push" else base


def run_command(collector: Collector, args: argparse.Namespace) -> Report:
    repository, run_id = parse_url(args.url, "actions/runs")
    run = collector.github.get(f"repos/{repository}/actions/runs/{run_id}")
    entry = collector.runs(repository, [run])[0]
    collector.resolve_hangs([entry])
    base = run_base(run, args.base)
    window = load_window(collector, main_repository(repository), base, run["created_at"], args.window)
    return run_report(f"CI failures: {run['html_url']}", entry, window, None)


def branch_command(collector: Collector, args: argparse.Namespace) -> Report:
    repository, _ = parse_url(args.url, None)
    return branch_report(load_window(collector, repository, args.branch, None, args.window))


def test_command(collector: Collector, args: argparse.Namespace) -> Report:
    repository, _ = parse_url(args.url, None)
    path = "/" + args.path.strip("/") + "/"
    return test_report(load_window(collector, repository, args.base, None, args.window), path)


def token(args: argparse.Namespace) -> str:
    value = Path(args.token_file).read_text().strip() if args.token_file else os.environ.get("GH_TOKEN", "")
    if not value:
        sys.exit("a GitHub token is required: --token-file <path> or the environment variable GH_TOKEN")
    return value


def parser() -> argparse.ArgumentParser:
    options = argparse.ArgumentParser(add_help=False)
    options.add_argument("--window", type=int, default=WINDOW, help=f"base branch runs, {WINDOW} by default")
    options.add_argument("--base", default="master", help="base branch of test and of runs without a branch to compare")
    options.add_argument("--json", action="store_true", help="job records and labels as JSON")
    options.add_argument("--token-file", help="file with a GitHub token; the environment variable GH_TOKEN by default")
    options.add_argument("--cache", default=DEFAULT_CACHE, help=f"cache directory, {DEFAULT_CACHE} by default")
    description = f"Failures of the test jobs of {WORKFLOW} runs, compared with the base branch"
    root = argparse.ArgumentParser(description=description)
    commands = root.add_subparsers(dest="command", required=True)
    pr = commands.add_parser("pr", parents=[options], help="newest run of a pull request, every attempt")
    pr.add_argument("url", help="pull request URL")
    run = commands.add_parser("run", parents=[options], help="one run, every attempt")
    run.add_argument("url", help="run URL")
    branch = commands.add_parser("branch", parents=[options], help="window summary of a branch")
    branch.add_argument("url", help="repository URL")
    branch.add_argument("branch", nargs="?", default="master")
    test = commands.add_parser("test", parents=[options], help="every failure of one test in the window")
    test.add_argument("url", help="repository URL")
    test.add_argument("path", help="hspec match path, e.g. /SimpleX chat client/group tests/")
    return root


COMMANDS = {"pr": pr_command, "run": run_command, "branch": branch_command, "test": test_command}


def main(argv: list[str]) -> None:
    args = parser().parse_args(argv)
    collector = Collector(GitHub(token(args)), Cache(Path(args.cache).expanduser()))
    report = COMMANDS[args.command](collector, args)
    print(json.dumps(report.data, indent=1) if args.json else report.text)


if __name__ == "__main__":
    main(sys.argv[1:])
