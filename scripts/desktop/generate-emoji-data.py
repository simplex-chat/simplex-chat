#!/usr/bin/env python3
# Generates the desktop emoji picker data from Unicode emoji-test.txt.
# Emoji version must not exceed the bundled NotoColorEmoji font (2.038 covers Emoji 15.0):
#   curl -O https://unicode.org/Public/emoji/15.0/emoji-test.txt
#   python3 scripts/desktop/generate-emoji-data.py emoji-test.txt apps/multiplatform/common/src/desktopMain/resources/emoji/emoji.txt
# Output: "@<group>" lines followed by "<emoji>\t<name>" lines, in CLDR order.

import sys

SKIPPED_GROUPS = {"Component"}
SKIN_TONE_MODIFIERS = {0x1F3FB, 0x1F3FC, 0x1F3FD, 0x1F3FE, 0x1F3FF}


def parse(lines):
    group = None
    for line in lines:
        line = line.rstrip("\n")
        if line.startswith("# group: "):
            group = line[len("# group: "):]
            if group not in SKIPPED_GROUPS:
                yield "@" + group
        elif line and not line.startswith("#") and group not in SKIPPED_GROUPS:
            codes, rest = line.split(";", 1)
            status, comment = rest.split("#", 1)
            if status.strip() != "fully-qualified":
                continue
            code_points = [int(c, 16) for c in codes.split()]
            if SKIN_TONE_MODIFIERS.intersection(code_points):
                continue
            # comment is "<emoji> E<version> <name>"
            name = comment.strip().split(" ", 2)[2]
            yield "".join(map(chr, code_points)) + "\t" + name


def main():
    src, dst = sys.argv[1], sys.argv[2]
    with open(src, encoding="utf-8") as f:
        out = list(parse(f))
    with open(dst, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
