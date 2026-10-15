#!/usr/bin/env python3

# Reads the words tesseract found on a screenshot and prints the phrases on screen, or the centre of one phrase.
#
#   screen-text.py <scale> <ocr dir> lines         one phrase per line: centre x,y and its words
#   screen-text.py <scale> <ocr dir> find <text>   "x y" of the first phrase containing the text; fails if none does
#
# <ocr dir> has tesseract's TSV of the dark-text and the light-text pass (dark.tsv, light.tsv), in pixels of the
# screenshot enlarged by <scale>, and background.pgm, the background luminance at screen size. Printed coordinates are
# screen pixels. Words are compared ignoring case and punctuation.

import csv
import os
import re
import sys

# Luminance (0-255) below which a background is dark enough for light text: the blue buttons are about 115.
LIGHT_BACKGROUND = 160


def key(text):
    return re.sub(r"[^0-9a-z]", "", text.lower())


def read_words(path, scale):
    words = []
    with open(path, newline="") as f:
        for row in csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE):
            text = (row["text"] or "").strip()
            if row["level"] != "5" or not key(text):
                continue
            left, top, width, height = (int(row[k]) / scale for k in ("left", "top", "width", "height"))
            words.append({"text": text, "key": key(text), "l": left, "t": top, "r": left + width, "b": top + height})
    return words


def read_background(path):
    with open(path, "rb") as f:
        data = f.read()
    header = re.match(rb"P5\s+(\d+)\s+(\d+)\s+(\d+)\s", data)
    if int(header[3]) != 255:
        sys.exit(f"{path}: expected 8-bit samples, got maximum {int(header[3])}")
    return int(header[1]), int(header[2]), data[header.end():]


def centre(words):
    return ((min(w["l"] for w in words) + max(w["r"] for w in words)) / 2,
            (min(w["t"] for w in words) + max(w["b"] for w in words)) / 2)


# Each pass also reads noise where its polarity doesn't apply (the gaps between dark letters look like light text),
# so a word counts only from the pass that matches the background under it.
def screen_words(ocr_dir, scale):
    width, height, luminance = read_background(os.path.join(ocr_dir, "background.pgm"))

    def on_light_background(w):
        x, y = centre([w])
        return luminance[min(int(y), height - 1) * width + min(int(x), width - 1)] >= LIGHT_BACKGROUND

    dark = read_words(os.path.join(ocr_dir, "dark.tsv"), scale)
    light = read_words(os.path.join(ocr_dir, "light.tsv"), scale)
    return [w for w in dark if on_light_background(w)] + [w for w in light if not on_light_background(w)]


# A word continues a phrase when it is on the same line as the phrase's last word and at most 1.5 of their height after it;
# wider gaps separate UI elements.
def follows(a, b):
    h = min(a["b"] - a["t"], b["b"] - b["t"])
    return min(a["b"], b["b"]) - max(a["t"], b["t"]) >= 0.5 * h and -0.5 * h <= b["l"] - a["r"] <= 1.5 * h


def phrases(words):
    result = []
    for w in sorted(words, key=lambda w: w["l"]):
        phrase = next((p for p in result if follows(p[-1], w)), None)
        if phrase:
            phrase.append(w)
        else:
            result.append([w])
    return sorted(result, key=lambda p: (min(w["t"] for w in p), p[0]["l"]))


# OCR can glue an icon to the first word, or punctuation or the text cursor to the last,
# so long edge words match by suffix or prefix.
def word_matches(word, k, first, last):
    if word == k:
        return True
    if len(k) < 4:
        return False
    return (first and word.endswith(k)) or (last and word.startswith(k))


def find(all_phrases, text):
    keys = [k for k in map(key, text.split()) if k]
    n = len(keys)
    for phrase in all_phrases:
        for i in range(len(phrase) - n + 1):
            if all(word_matches(phrase[i + j]["key"], k, j == 0, j == n - 1) for j, k in enumerate(keys)):
                return centre(phrase[i:i + n])
    return None


def main():
    scale, ocr_dir, command, args = float(sys.argv[1]), sys.argv[2], sys.argv[3], sys.argv[4:]
    on_screen = phrases(screen_words(ocr_dir, scale))
    if command == "lines":
        for phrase in on_screen:
            x, y = centre(phrase)
            print(f"{round(x)},{round(y)} {' '.join(w['text'] for w in phrase)}")
    elif command == "find":
        found = find(on_screen, args[0])
        if not found:
            sys.exit(1)
        print(f"{round(found[0])} {round(found[1])}")
    else:
        sys.exit(f"unknown command: {command}")


main()
