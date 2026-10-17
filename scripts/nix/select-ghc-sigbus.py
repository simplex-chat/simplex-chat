#!/usr/bin/env python3
"""Select one normal iOS library compilation without changing its argv."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import stat
import sys
import unicodedata


MAX_BYTES = 1024 * 1024
MAX_FILES = 16


def ghc_space(char):
    # GHC.Internal.Unicode.isSpace, including its Latin-1 fast path.
    code = ord(char)
    if code <= 0x377:
        return code == 32 or 9 <= code <= 13 or code == 0xA0
    return unicodedata.category(char) == "Zs"


def unescape_response(text):
    # Match pinned GHC.Internal.ResponseFile.unescapeArgs. In particular,
    # backslash escapes the next character even inside single quotes, and
    # empty fields, a final backslash and unmatched quote delimiters vanish.
    result, current = [], []
    quote, escaped = None, False
    for char in text:
        if escaped:
            current.append(char)
            escaped = False
        elif char == "\\":
            escaped = True
        elif quote is not None:
            if char == quote:
                quote = None
            else:
                current.append(char)
        elif char in ("'", '"'):
            quote = char
        elif ghc_space(char):
            if current:
                result.append("".join(current))
                current = []
        else:
            current.append(char)
    if current:
        result.append("".join(current))
    return result


def expand_for_selection(arguments):
    expanded, files, total = [], [], 0
    for argument in arguments:
        if argument.startswith("@"):
            if len(files) >= MAX_FILES:
                raise ValueError("Too many compiler response files")
            path = Path(argument[1:])
            if not path.is_absolute():
                raise ValueError("Expected an absolute compiler response-file path")
            descriptor = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
            with os.fdopen(descriptor, "rb") as stream:
                if not stat.S_ISREG(os.fstat(stream.fileno()).st_mode):
                    raise ValueError("Compiler response file must be regular")
                data = stream.read(MAX_BYTES - total + 1)
            total += len(data)
            if total > MAX_BYTES:
                raise ValueError("Compiler response files exceed the selection limit")
            # GHC expands only the original argv's @files, never recursively.
            expanded.extend(unescape_response(data.decode("utf-8")))
            files.append({"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()})
        else:
            expanded.append(argument)
    return expanded, files


def selected(arguments, version):
    if not {"--make", "-fbuilding-cabal-package", "-DIOS"}.issubset(arguments) or "-prof" in arguments:
        return False
    units = [arguments[index + 1] for index, value in enumerate(arguments[:-1])
             if value == "-this-unit-id"]
    return len(units) == 1 and units[0].startswith("simplex-chat-" + version + "-")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--package-version", required=True)
    parser.add_argument("arguments", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    arguments = args.arguments[1:] if args.arguments[:1] == ["--"] else args.arguments
    try:
        expanded, files = expand_for_selection(arguments)
        if not selected(expanded, args.package_version):
            return 1
        print(json.dumps({"intelGhcSignalSelection": True, "argumentCount": len(expanded),
                          "responseFiles": files, "originalArgvUnchanged": True}), flush=True)
        return 0
    except (OSError, UnicodeError, ValueError) as error:
        print("Intel GHC diagnostic selection failed: " + type(error).__name__, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
