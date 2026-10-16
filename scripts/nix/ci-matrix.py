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
    ("x86_64-darwin", "macos-15-intel", "all"),
)


def select(group):
    if group not in ("all", "linux-static"):
        raise ValueError(f"unknown Nix CI group: {group}")
    rows = ROWS if group == "all" else tuple(row for row in ROWS if row[2] == group)
    return {"include": [dict(system=system, runner=runner, target=target)
                        for system, runner, target in rows]}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--group", choices=["all", "linux-static"], default="all")
    print(json.dumps(select(parser.parse_args().group), separators=(",", ":")))
