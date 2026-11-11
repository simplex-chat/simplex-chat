#!/usr/bin/env python3

# Reads a uiautomator dump of the screen:
#   screen-tree.py <dump .xml> lines         the elements with a text or a content description, one per line: "x,y text", the element's centre
#   screen-tree.py <dump .xml> find <text>   "x y left top right bottom" of the element with the text, or exits 1
#   screen-tree.py <dump .xml> tree          the elements that have a text or can be used: class, text, bounds and states
#   screen-tree.py <dump .xml> focused [<text>]   exits 1 unless a text field has the focus, and shows the text exactly
# Text is compared as the desktop ui helper compares it: letters and digits only, any case. An element that is exactly
# the text wins over one that contains it, and then one inside a clickable element (Compose puts a button's text in a
# child of the clickable view) over a title with the same text.

import json
import re
import sys
import xml.etree.ElementTree as ET

BOUNDS = re.compile(r"\[(-?\d+),(-?\d+)\]\[(-?\d+),(-?\d+)\]")
STATES = ["clickable", "checkable", "checked", "focused", "scrollable", "selected"]
FIELD = "android.widget.EditText"
# listed by tree even without a text: fields, and the progress indicators of screens that are busy
WIDGETS = [FIELD, "android.widget.ProgressBar"]


def normalise(text):
    return "".join(c for c in text.lower() if c.isalnum())


def label(node):
    return " ".join((node.get("text") or node.get("content-desc") or "").split())


# Elements off the screen, such as the chat list while a chat covers it, have empty bounds.
def nodes(node, in_clickable=False):
    clickable = in_clickable or node.get("clickable") == "true"
    bounds = BOUNDS.fullmatch(node.get("bounds", ""))
    if bounds:
        left, top, right, bottom = map(int, bounds.groups())
        if right > left and bottom > top:
            yield node, clickable, (left, top, right, bottom)
    for child in node:
        yield from nodes(child, clickable)


def tree_line(node, bounds):
    left, top, right, bottom = bounds
    states = [s for s in STATES if node.get(s) == "true"] + (["disabled"] if node.get("enabled") == "false" else [])
    kind = node.get("class", "").rsplit(".", 1)[-1]
    return f"{kind} {json.dumps(label(node))} at {left},{top} size {right - left}x{bottom - top}{''.join(' ' + s for s in states)}"


def main():
    path, command = sys.argv[1], sys.argv[2]
    found = list(nodes(ET.parse(path).getroot()))
    labelled = [(label(n), clickable, bounds) for n, clickable, bounds in found if label(n)]
    if command == "lines":
        for text, _, (left, top, right, bottom) in labelled:
            print(f"{(left + right) // 2},{(top + bottom) // 2} {text}")
    elif command == "tree":
        for node, _, bounds in found:
            if label(node) or node.get("class") in WIDGETS or any(node.get(s) == "true" for s in ["clickable", "checkable", "scrollable"]):
                print(tree_line(node, bounds))
    elif command == "focused":
        text = " ".join(sys.argv[3].split()) if len(sys.argv) > 3 else ""
        if not any(n.get("class") == FIELD and n.get("focused") == "true" and text in label(n) for n, _, _ in found):
            sys.exit(1)
    elif command == "find":
        target = normalise(sys.argv[3])
        matches = [e for e in labelled if target and target in normalise(e[0])]
        if not matches:
            sys.exit(1)
        _, _, (left, top, right, bottom) = min(matches, key=lambda e: (normalise(e[0]) != target, not e[1]))
        print(f"{(left + right) // 2} {(top + bottom) // 2} {left} {top} {right} {bottom}")
    else:
        sys.exit(f"unknown command: {command}")


main()
