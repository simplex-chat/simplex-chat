#!/usr/bin/env python3

# Reads a uiautomator dump and prints the elements that have a text or a content description:
#   screen-tree.py <dump .xml> lines         one per line, "x,y text" with the element's centre
#   screen-tree.py <dump .xml> find <text>   "x y left top right bottom" of the element, or exits 1
# Text is compared as the desktop ui helper compares it: letters and digits only, any case; an element that is exactly
# the text wins over the first that contains it.

import re
import sys
import xml.etree.ElementTree as ET

BOUNDS = re.compile(r"\[(-?\d+),(-?\d+)\]\[(-?\d+),(-?\d+)\]")


def normalise(text):
    return "".join(c for c in text.lower() if c.isalnum())


def elements(path):
    for node in ET.parse(path).iter("node"):
        label = node.get("text") or node.get("content-desc") or ""
        bounds = BOUNDS.fullmatch(node.get("bounds", ""))
        if label.strip() and bounds:
            left, top, right, bottom = map(int, bounds.groups())
            yield " ".join(label.split()), left, top, right, bottom


def main():
    path, command = sys.argv[1], sys.argv[2]
    found = list(elements(path))
    if command == "lines":
        for label, left, top, right, bottom in found:
            print(f"{(left + right) // 2},{(top + bottom) // 2} {label}")
        return
    target = normalise(sys.argv[3])
    exact = [e for e in found if normalise(e[0]) == target]
    partial = [e for e in found if target and target in normalise(e[0])]
    match = (exact or partial or [None])[0]
    if match is None:
        sys.exit(1)
    _, left, top, right, bottom = match
    print(f"{(left + right) // 2} {(top + bottom) // 2} {left} {top} {right} {bottom}")


main()
