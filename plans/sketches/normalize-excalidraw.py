#!/usr/bin/env python3
"""Shrink an .excalidraw file without changing what it renders.

Excalidraw writes every element with its full schema, pretty-printed, so a
354-element sketch costs 14k lines and ~90k tokens to read, and a one-field
edit shows up as a 27-line diff block. This drops the fields Excalidraw
re-derives on load and puts one element per line, so a changed element is
one changed line.

Kept on purpose: roughness, fillStyle, fontFamily and strokeColor, which
this drawing sets away from the Excalidraw default; strokeWidth, whose
default is 2, so dropping a 1 doubles the stroke; and lineHeight, which
Excalidraw recomputes from font metrics when it is absent.

Usage: normalize-excalidraw.py FILE...   rewrites each file in place

Runs from .git/hooks/pre-commit on every staged .excalidraw file.
"""
import json
import sys

# Rewritten by Excalidraw on every save; carries no meaning for the drawing.
DROP = {"version", "versionNonce", "updated", "seed", "baseline"}

# Dropped only where the element already holds the default value.
DEFAULT = {
    "isDeleted": False, "locked": False, "link": None, "frameId": None,
    "angle": 0, "opacity": 100, "strokeStyle": "solid",
    "groupIds": [], "boundElements": [], "roundness": None,
    "backgroundColor": "transparent", "autoResize": True,
    "textAlign": "left", "verticalAlign": "top", "containerId": None,
    "startBinding": None, "endBinding": None, "startArrowhead": None,
    "endArrowhead": None, "lastCommittedPoint": None,
    "startIsSpecial": None, "endIsSpecial": None,
}


def lean(element):
    return {k: v for k, v in element.items()
            if k not in DROP
            and not (k in DEFAULT and v == DEFAULT[k])
            and not (k == "originalText" and v == element.get("text"))}


def dump(scene):
    head = [f"{json.dumps(k)}: {json.dumps(v)}" for k, v in scene.items()
            if k not in ("elements", "appState", "files")]
    body = ",\n".join("    " + json.dumps(lean(e), separators=(",", ":"))
                      for e in scene["elements"])
    tail = [f"{json.dumps(k)}: {json.dumps(scene[k])}"
            for k in ("appState", "files") if k in scene]
    return ("{\n  " + ",\n  ".join(head) + ',\n  "elements": [\n' + body
            + "\n  ],\n  " + ",\n  ".join(tail) + "\n}\n")


def main(paths):
    for path in paths:
        with open(path) as f:
            before = f.read()
        text = dump(json.loads(before))
        with open(path, "w") as f:
            f.write(text)
        print(f"{path}: {len(before)} -> {len(text)} bytes, "
              f"{before.count(chr(10))} -> {text.count(chr(10))} lines")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    main(sys.argv[1:])
