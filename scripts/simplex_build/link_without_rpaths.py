"""The linker of the macOS packages: cc without the absolute rpaths that Cabal and GHC pass, whose room in the load
commands would make the libraries' layout depend on where GHC and Xcode are."""

import os
import shlex
from collections.abc import Iterator, Sequence
from pathlib import Path


def expand(args: Sequence[str]) -> Iterator[str]:
    """args with each @file replaced by the arguments in it, as GHC passes the link's arguments."""
    for arg in args:
        if arg.startswith("@") and Path(arg[1:]).is_file():
            yield from expand(shlex.split(Path(arg[1:]).read_text()))
        else:
            yield arg


def without_absolute_rpaths(args: Sequence[str]) -> list[str]:
    """args without -Wl,-rpath,/dir, -Wl,-rpath -Wl,/dir, -Xlinker -rpath -Xlinker /dir and -rpath /dir."""
    kept = []
    i = 0
    while i < len(args):
        rest = list(args[i : i + 4])
        if rest[0].startswith("-Wl,-rpath,/"):
            i += 1
        elif rest[:1] == ["-Wl,-rpath"] and rest[1:2] and rest[1].startswith("-Wl,/"):
            i += 2
        elif rest[:3] == ["-Xlinker", "-rpath", "-Xlinker"] and rest[3:] and rest[3].startswith("/"):
            i += 4
        elif rest[:1] == ["-rpath"] and rest[1:2] and rest[1].startswith("/"):
            i += 2
        else:
            kept.append(rest[0])
            i += 1
    return kept


def link(argv: Sequence[str]) -> None:
    args = without_absolute_rpaths(list(expand(argv)))
    responses = [arg[1:] for arg in argv if arg.startswith("@") and Path(arg[1:]).is_file()]
    if responses:
        # the arguments that GHC put in a response file can be too many for a command line
        rewritten = Path(f"{responses[0]}.without-rpaths")
        rewritten.write_text("\n".join(shlex.quote(arg) for arg in args) + "\n")
        args = [f"@{rewritten}"]
    os.execvp("cc", ["cc", *args])
