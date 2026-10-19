#!/usr/bin/env python3
"""Print bounded technical fields from recent Nix GHC crash reports."""

import argparse
import json
import os
from pathlib import Path
import re
import signal
import stat
import time


MAX_INPUT = 2 * 1024 * 1024
MAX_OUTPUT = 64 * 1024
MAX_REPORTS = 3
PROCESS = re.compile(r"(?:ghc(?:-[\w.+-]+)?|iserv(?:-(?:prof|dyn))?)\Z")


class CollectionDeadline(Exception):
    """Keep the deadline distinct from recoverable filesystem errors."""


def select_fields(value, keys):
    return {key: value[key] for key in keys if key in value}


def crash_fields(data):
    # Apple documents two JSON objects: a metadata line, then the report.
    # https://developer.apple.com/documentation/xcode/interpreting-the-json-format-of-a-crash-report
    header, body = data.decode("utf-8").split("\n", 1)
    if str(json.loads(header).get("bug_type")) != "309":
        return None
    report = json.loads(body)
    executable = report.get("procPath", "")
    if not executable.startswith("/nix/store/") or not PROCESS.fullmatch(Path(executable).name):
        return None
    threads = report.get("threads", [])
    index = report.get("faultingThread")
    if not isinstance(index, int) or isinstance(index, bool) or not 0 <= index < len(threads):
        return None
    thread = threads[index]
    frames = [select_fields(frame, ("imageIndex", "imageOffset", "symbol",
                                   "symbolLocation", "sourceFile", "sourceLine"))
              for frame in thread.get("frames", [])[:80]]
    images = report.get("usedImages", [])
    image_indexes = sorted({frame["imageIndex"] for frame in frames
                            if isinstance(frame.get("imageIndex"), int)
                            and 0 <= frame["imageIndex"] < len(images)})
    # Keep indexes so each frame remains attributable after filtering images.
    return {
        "procPath": executable,
        "pid": report.get("pid"),
        "cpuType": report.get("cpuType"),
        "exception": select_fields(report.get("exception", {}),
                                   ("type", "signal", "subtype", "codes", "rawCodes")),
        "termination": select_fields(report.get("termination", {}),
                                     ("namespace", "code", "indicator", "flags")),
        "faultingThread": index,
        "frames": frames,
        "framesTruncated": len(thread.get("frames", [])) > len(frames),
        "threadState": thread.get("threadState"),
        "images": [{"index": i, **{k: images[i].get(k) for k in
                    ("name", "path", "base", "size", "arch", "uuid")}}
                   for i in image_indexes],
    }


def recent_reports(directories, since):
    candidates = []
    for directory in directories:
        try:
            with os.scandir(directory) as entries:
                # Do not recurse into archives or inspect unrelated filenames.
                for count, entry in enumerate(entries):
                    if count >= 4096:
                        break
                    if entry.name.endswith(".ips") and entry.name.startswith(("ghc", "iserv")):
                        info = entry.stat(follow_symlinks=False)
                        if stat.S_ISREG(info.st_mode) and info.st_mtime >= since:
                            candidates.append((info.st_mtime, Path(entry.path)))
        except OSError:
            continue
    return [path for _, path in sorted(candidates, reverse=True)[:MAX_REPORTS * 4]]


def collect(directories, since):
    found = 0
    for path in recent_reports(directories, since):
        try:
            # Reject symlinks and non-regular files again at open time.
            fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
            with os.fdopen(fd, "rb") as source:
                info = os.fstat(source.fileno())
                if not stat.S_ISREG(info.st_mode) or info.st_mtime < since or info.st_size > MAX_INPUT:
                    continue
                data = source.read(MAX_INPUT + 1)
            if len(data) > MAX_INPUT:
                continue
            fields = crash_fields(data)
            if fields is None:
                continue
            encoded = json.dumps(fields, ensure_ascii=True)
            if len(encoded) > MAX_OUTPUT:
                print("Darwin GHC crash report exceeds the technical-output bound", flush=True)
                continue
            print("Darwin GHC crash report: " + encoded, flush=True)
            found += 1
            if found == MAX_REPORTS:
                break
        except (OSError, ValueError, TypeError, AttributeError, KeyError):
            # ReportCrash can still be writing; a later bounded scan may succeed.
            continue
    return found


def deadline_expired(_signum, _frame):
    raise CollectionDeadline("Darwin crash-report collection reached its 15-second bound")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--since", type=float, required=True, help="Build start as Unix seconds")
    parser.add_argument("--directory", type=Path, action="append", help="Override report directories")
    args = parser.parse_args()
    directories = args.directory or [Path.home() / "Library/Logs/DiagnosticReports",
                                     Path("/Library/Logs/DiagnosticReports")]
    signal.signal(signal.SIGALRM, deadline_expired)
    signal.alarm(15)
    try:
        for attempt in range(3):
            if collect(directories, args.since):
                return
            if attempt < 2:
                time.sleep(2)
        print("No recent readable Nix GHC .ips crash report was published", flush=True)
    except CollectionDeadline as error:
        print(str(error), flush=True)
    finally:
        signal.alarm(0)


if __name__ == "__main__":
    main()
