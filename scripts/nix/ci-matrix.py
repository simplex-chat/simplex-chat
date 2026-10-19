#!/usr/bin/env python3
"""Select existing Nix CI groups; PRs and pushes use the complete matrix."""
import argparse
import json

ROWS = (
    ("x86_64-linux", "ubuntu-24.04", "native"),
    ("x86_64-linux", "ubuntu-24.04", "android"),
    ("x86_64-linux", "ubuntu-24.04", "windows"),
    ("x86_64-linux", "ubuntu-24.04", "linux-static"),
    ("aarch64-linux", "ubuntu-24.04-arm", "native"),
    ("aarch64-darwin", "macos-15", "all"),
    # Cold native compiler/application builds left too little of GitHub's
    # six-hour job limit for the two mobile libraries. Keep their budgets separate.
    ("x86_64-darwin", "macos-15-intel", "native"),
    ("x86_64-darwin", "macos-15-intel", "darwin-libs"),
)


def select(group):
    if group not in ("all", "linux-static", "android", "windows", "x86_64-darwin", "x86_64-darwin-libs"):
        raise ValueError(f"unknown Nix CI group: {group}")
    if group == "all":
        rows = ROWS
    elif group == "x86_64-darwin":
        rows = tuple(row for row in ROWS if row[0] == group)
    elif group == "x86_64-darwin-libs":
        rows = tuple(row for row in ROWS if row[0] == "x86_64-darwin" and row[2] == "darwin-libs")
    else:
        rows = tuple(row for row in ROWS if row[2] == group)
    return {"include": [dict(system=system, runner=runner, target=target)
                        for system, runner, target in rows]}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--group", choices=["all", "linux-static", "android", "windows", "x86_64-darwin", "x86_64-darwin-libs"], default="all")
    print(json.dumps(select(parser.parse_args().group), separators=(",", ":")))
