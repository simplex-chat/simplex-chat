#!/usr/bin/env python3

# Prints the height in pixels of the ink in a box of a screenshot: from the first to the last row with a pixel darker than
# halfway between the box's lightest and darkest. OCR's boxes are padded, so they hardly tell text sizes apart.
#
#   screen-ink.py <gray.pgm> <left> <top> <right> <bottom>
#
# <gray.pgm> is the screenshot in 8-bit grayscale; the box is in its pixels, edges included.

import re
import sys


def main():
    path, left, top, right, bottom = sys.argv[1], *map(int, sys.argv[2:6])
    with open(path, "rb") as f:
        data = f.read()
    header = re.match(rb"P5\s+(\d+)\s+(\d+)\s+(\d+)\s", data)
    width, height, pixels = int(header[1]), int(header[2]), data[header.end():]
    columns = range(max(left, 0), min(right, width - 1) + 1)
    rows = [[pixels[y * width + x] for x in columns] for y in range(max(top, 0), min(bottom, height - 1) + 1)]
    threshold = (max(map(max, rows)) + min(map(min, rows))) / 2
    ink = [i for i, row in enumerate(rows) if min(row) < threshold]
    print(ink[-1] - ink[0] + 1 if ink else 0)


main()
