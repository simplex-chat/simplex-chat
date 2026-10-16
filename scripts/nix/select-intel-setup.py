"""Select the failed GHC 9.14 ghc-boot component for the Intel diagnostic."""

import pathlib
import re
import sys


def select_failed_component(log):
    component = (
        r"/nix/store/[0123456789abcdfghijklmnpqrsvwxyz]{32}"
        r"-ghc-boot-lib-ghc-boot-9\.14\.drv"
    )
    failures = re.findall(
        rf"error: Cannot build '({component})'|builder for '({component})' failed",
        log,
    )
    paths = {path for match in failures for path in match if path}
    if len(paths) > 1:
        raise ValueError("multiple GHC 9.14 ghc-boot components failed")
    return next(iter(paths), None)


def main():
    try:
        if len(sys.argv) != 2:
            raise ValueError("expected one canonical build log")
        selected = select_failed_component(
            pathlib.Path(sys.argv[1]).read_text(errors="replace")
        )
        if selected is None:
            raise ValueError("no GHC 9.14 ghc-boot component failure found")
    except (OSError, ValueError) as error:
        print(f"Skipping Intel Setup diagnostic: {error}.", file=sys.stderr)
        return 1
    print(selected)
    return 0


if __name__ == "__main__":
    sys.exit(main())
