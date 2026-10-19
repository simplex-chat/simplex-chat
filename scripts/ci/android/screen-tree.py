#!/usr/bin/env python3

# Reads a uiautomator dump and prints the elements that have a text or a content description:
#   screen-tree.py <dump .xml> lines         one per line, "x,y text" with the element's centre
#   screen-tree.py <dump .xml> find <text>   "x y left top right bottom" of the element, or exits 1
# Text is compared as the desktop ui helper compares it: letters and digits only, any case. An element that is exactly
# the text wins over one that contains it, and then one inside a clickable element (Compose puts a button's text in a
# child of the clickable view) over a title with the same text.

import re
import sys
import xml.etree.ElementTree as ET

BOUNDS = re.compile(r"\[(-?\d+),(-?\d+)\]\[(-?\d+),(-?\d+)\]")


def normalise(text):
    return "".join(c for c in text.lower() if c.isalnum())


def elements(node, in_clickable=False):
    clickable = in_clickable or node.get("clickable") == "true"
    label = node.get("text") or node.get("content-desc") or ""
    bounds = BOUNDS.fullmatch(node.get("bounds", ""))
    if label.strip() and bounds:
        yield (" ".join(label.split()), clickable, *map(int, bounds.groups()))
    for child in node:
        yield from elements(child, clickable)


def main():
    path, command = sys.argv[1], sys.argv[2]
    found = list(elements(ET.parse(path).getroot()))
    if command == "lines":
        for label, _, left, top, right, bottom in found:
            print(f"{(left + right) // 2},{(top + bottom) // 2} {label}")
        return
    target = normalise(sys.argv[3])
    matches = [e for e in found if target and target in normalise(e[0])]
    if not matches:
        sys.exit(1)
    _, _, left, top, right, bottom = min(matches, key=lambda e: (normalise(e[0]) != target, not e[1]))
    print(f"{(left + right) // 2} {(top + bottom) // 2} {left} {top} {right} {bottom}")


main()
