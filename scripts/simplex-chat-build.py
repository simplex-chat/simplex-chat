#!/usr/bin/env python3

"""Builds SimpleX Chat release files, installs prebuilt libraries and reproduces releases: see simplex_build/cli.py."""

import sys

if sys.version_info < (3, 12):  # noqa: UP036 # runs on the older Pythons it rejects
    sys.exit("simplex-chat-build.py needs Python 3.12 or newer")

from pathlib import Path

# The package next to this script, also when Python runs isolated (-I) and leaves this directory off sys.path
sys.path.insert(0, str(Path(__file__).resolve().parent))

from simplex_build.cli import main  # noqa: E402

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
