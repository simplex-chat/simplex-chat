#!/usr/bin/env python3
"""Check the real patched RTS caller with scripted API boundary controls.

This native unit does not run Wine or qualify Windows API/ABI behavior.
"""

import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rts-source", type=Path, required=True,
                        help="configured GHC rts directory, containing linker/PEi386.c")
    parser.add_argument("--cc", default="cc", help="native C11 compiler executable")
    args = parser.parse_args()
    scripts = Path(__file__).resolve().parent
    source = args.rts_source / "linker/PEi386.c"
    if not source.is_file():
        parser.error(f"missing RTS source: {source}")

    with tempfile.TemporaryDirectory(prefix="windows-dll-search-path-") as directory:
        root = Path(directory)
        (root / "linker").mkdir()
        target = root / "linker/PEi386.c"
        shutil.copyfile(source, target)
        subprocess.run(
            ["patch", "-p1", "-F0", "-i",
             str(scripts / "ghc914-rts-windows-dll-search-path.patch")],
            cwd=root, check=True, timeout=30,
        )
        text = target.read_text()
        start = "HsPtr addLibrarySearchPath_PEi386(pathchar* dll_path)\n"
        end = "\nbool removeLibrarySearchPath_PEi386"
        if text.count(start) != 1 or text.count(end) != 1:
            raise RuntimeError("expected one exact RTS caller boundary")
        begin = text.index(start)
        finish = text.index(end, begin)
        (root / "candidate-body.inc").write_text(text[begin:finish])
        shutil.copyfile(scripts / "windows-dll-search-path/CallerBoundary.c",
                        root / "CallerBoundary.c")
        executable = root / "caller-boundary"
        subprocess.run(
            [args.cc, "-std=c11", "-Werror=vla", "-pedantic", "-Wall", "-Wextra",
             "-O1", str(root / "CallerBoundary.c"), "-o", str(executable)],
            check=True, timeout=30,
        )
        subprocess.run([str(executable)], check=True, timeout=10)


if __name__ == "__main__":
    main()
