#!/usr/bin/env python3

# Prints how many pixels of an image stand out from its background (its most common colour), and their mean colour.
# With a colour, fails unless enough pixels stand out and that channel of their mean exceeds the other two by a margin.
# Usage: screen-colour.py <image.ppm> [red|green|blue]

import re
import sys
from collections import Counter

# A pixel stands out when one of its channels differs from the background's by more than this.
DIFFERENCE = 48
# A 42 px profile image has about 1200 pixels; fewer than this standing out is no image.
MIN_PIXELS = 300
DOMINANCE = 50
CHANNELS = {"red": 0, "green": 1, "blue": 2}


def read_ppm(path):
    with open(path, "rb") as f:
        data = f.read()
    header = re.match(rb"P6\s+(\d+)\s+(\d+)\s+(\d+)\s", data)
    if int(header[3]) != 255:
        sys.exit(f"{path}: expected 8-bit samples, got maximum {int(header[3])}")
    samples = data[header.end():]
    return [tuple(samples[i:i + 3]) for i in range(0, len(samples) - 2, 3)]


def main():
    pixels = read_ppm(sys.argv[1])
    colour = sys.argv[2] if len(sys.argv) > 2 else None
    if colour is not None and colour not in CHANNELS:
        sys.exit(f"unknown colour: {colour}")
    background = Counter(pixels).most_common(1)[0][0]
    standing_out = [p for p in pixels if max(abs(a - b) for a, b in zip(p, background)) > DIFFERENCE]
    n = len(standing_out)
    mean = [round(sum(p[c] for p in standing_out) / n) if n else 0 for c in range(3)]
    print(f"{n} px, mean {mean[0]},{mean[1]},{mean[2]}")
    if colour is not None:
        c = CHANNELS[colour]
        if n < MIN_PIXELS or any(mean[c] - mean[o] < DOMINANCE for o in range(3) if o != c):
            sys.exit(1)


main()
