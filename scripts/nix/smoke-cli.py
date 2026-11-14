#!/usr/bin/env python3
"""Check CLI encryption and profile passwords using temporary local stores.

Usage: python3 scripts/nix/smoke-cli.py /path/to/simplex-chat
No server is needed. All profiles, database keys, and passwords are test fixtures.
"""

import argparse
import os
from pathlib import Path
import subprocess
import sys
import tempfile


def smoke(cli, timeout):
    with tempfile.TemporaryDirectory(prefix="simplex-cli-smoke-") as directory:
        root = Path(directory)
        home = root / "home"
        home.mkdir()
        environment = dict(os.environ, HOME=str(home))
        for name in ("DATA", "CONFIG", "CACHE"):
            environment[f"XDG_{name}_HOME"] = str(root / name.lower())

        def run(label, arguments, expected_exit=0, line=None, fragment=None):
            print(f"Checking {label}...", flush=True)
            result = subprocess.run(
                [str(cli)] + arguments,
                cwd=directory,
                env=environment,
                stdin=subprocess.DEVNULL,
                capture_output=True,
                encoding="utf-8",
                timeout=timeout,
                check=False,
            )
            output = result.stdout + result.stderr
            if (
                result.returncode != expected_exit
                or (line is not None and line not in output.splitlines())
                or (fragment is not None and fragment not in output)
            ):
                raise RuntimeError(
                    f"{label} failed: exit {result.returncode}, "
                    f"expected exit {expected_exit}, line {line!r}, "
                    f"fragment {fragment!r}\n{output}"
                )

        run("CLI version", ["--version"], fragment="SimpleX Chat v")
        prefix = str(root / "probe")
        key = "simplex-cli-fixture-key"

        def options(database_key):
            return [
                "--database", prefix,
                "--key", database_key,
                "--execute-log", "none",
                "--time", "0",
                "--yes-migrate",
            ]

        common = options(key)
        run(
            "fresh encrypted stores",
            common + ["--user-display-name", "cli-smoke", "--execute", "/u"],
            line="Current user: cli-smoke",
        )
        for suffix in ("_chat.db", "_agent.db"):
            database = Path(prefix + suffix)
            with database.open("rb") as stream:
                header = stream.read(16)
            if len(header) != 16 or header == b"SQLite format 3\x00":
                raise RuntimeError(f"{database.name} does not have an encrypted header")

        run(
            "correct database key after reopening",
            common + ["--maintenance", "--execute", "/db test key " + key],
            line="ok",
        )
        run(
            "wrong database key through the command API",
            common + ["--maintenance", "--execute", "/db test key wrong-fixture-key"],
            line="error opening database after encryption: wrong passphrase or invalid database file",
        )
        run(
            "second local profile",
            common + ["--execute", "/create user cli-smoke-hidden"],
            line="user profile: cli-smoke-hidden",
        )
        # These commands require a started chat. Fresh profiles have no connections.
        run(
            "stored profile password hash and salt",
            common + ["--execute", "/hide user fixture_password"],
            line="profile is hidden",
        )
        run(
            "wrong profile password after reopening",
            common + ["--execute", "/unhide user wrong_fixture_password"],
            line="user does not exist or incorrect password",
        )
        run(
            "cleared profile password hash and salt",
            common + ["--execute", "/unhide user fixture_password"],
            line="profile is visible",
        )
        run(
            "cleared profile password after reopening",
            common + ["--execute", "/unhide user fixture_password"],
            line="user is not hidden",
        )
        run(
            "wrong database key at startup",
            options("wrong-fixture-key") + ["--maintenance", "--execute", "/u"],
            expected_exit=1,
            fragment="ErrorNotADatabase",
        )
    print("CLI encryption and profile password smoke passed.", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cli", type=Path, help="built simplex-chat executable")
    parser.add_argument("--timeout", type=int, default=60, help="seconds per CLI call (default: 60)")
    arguments = parser.parse_args()
    if arguments.timeout <= 0:
        parser.error("--timeout must be positive")
    try:
        smoke(arguments.cli.expanduser().resolve(strict=True), arguments.timeout)
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
        print(f"CLI smoke failed: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
