#!/usr/bin/env python3

# Reads the words tesseract found on a screenshot and prints the phrases on screen, or where one text is.
#
#   screen-text.py <scale> <ocr dir> lines         one phrase per line: centre x,y and its words
#   screen-text.py <scale> <ocr dir> find <text>   "x y left top right bottom": centre and box of the text, from the phrase
#                                                  that is exactly the text, else the first one containing it; fails if none does
#
# <ocr dir> has tesseract's TSV of the dark-text and the light-text pass (dark.tsv, light.tsv), in pixels of the
# screenshot enlarged by <scale>, and gray.pgm, the screenshot in 8-bit grayscale. Printed coordinates are
# screen pixels. Text is compared ignoring case, spaces and punctuation.

import csv
import os
import re
import statistics
import sys

# Luminance (0-255) below which a background is dark enough for light text: blue buttons are about 115, disabled grey
# ones about 137, light ones (window, fields, cards) over 230.
LIGHT_BACKGROUND = 190


# In the app's font a capital I looks like a lowercase l, and OCR reads it as one, so the two compare equal.
def key(text):
    return re.sub(r"[^0-9a-z]", "", text.lower()).replace("l", "i")


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


def read_gray(path):
    with open(path, "rb") as f:
        data = f.read()
    header = re.match(rb"P5\s+(\d+)\s+(\d+)\s+(\d+)\s", data)
    if int(header[3]) != 255:
        sys.exit(f"{path}: expected 8-bit samples, got maximum {int(header[3])}")
    return int(header[1]), int(header[2]), data[header.end():]


# The median of the rows 2 to 4 pixels above and below the word: its background, free of its own letters.
def background(gray, w):
    width, height, luminance = gray
    left, right = max(int(w["l"]), 0), min(int(w["r"]), width - 1)
    rows = [int(w["t"]) - d for d in (2, 3, 4)] + [int(w["b"]) + d for d in (2, 3, 4)]
    return statistics.median(luminance[y * width + x] for y in rows if 0 <= y < height for x in range(left, right + 1))


def box(words):
    return min(w["l"] for w in words), min(w["t"] for w in words), max(w["r"] for w in words), max(w["b"] for w in words)


def centre(words):
    left, top, right, bottom = box(words)
    return (left + right) / 2, (top + bottom) / 2


# Each pass also reads noise where its polarity doesn't apply (the gaps between dark letters look like light text),
# so a word counts only from the pass that matches the background under it.
def screen_words(ocr_dir, scale):
    gray = read_gray(os.path.join(ocr_dir, "gray.pgm"))

    def on_light_background(w):
        return background(gray, w) >= LIGHT_BACKGROUND

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


# OCR can split or join words ("#ci_group" read as "#ci_ group"), so words are compared by their joined characters.
# It can also glue an icon to the first word, or punctuation or the text cursor to the last, so the text may start
# inside the first word and end inside the last when at least 4 of its characters are in that word.
def span_matches(words, target):
    keys = [w["key"] for w in words]
    joined = "".join(keys)
    start = joined.find(target)
    while start >= 0:
        end = start + len(target)
        first_ok = start == 0 or len(keys[0]) - start >= 4
        last_ok = end == len(joined) or len(keys[-1]) - (len(joined) - end) >= 4
        if start < len(keys[0]) and end > len(joined) - len(keys[-1]) and first_ok and last_ok:
            return True
        start = joined.find(target, start + 1)
    return False


def find(all_phrases, text):
    target = key(text)
    if not target:
        return None
    for phrase in all_phrases:
        if "".join(w["key"] for w in phrase) == target:
            return phrase
    for phrase in all_phrases:
        for i in range(len(phrase)):
            for j in range(i + 1, len(phrase) + 1):
                if span_matches(phrase[i:j], target):
                    return phrase[i:j]
    return None


def main():
    scale, ocr_dir, command, args = float(sys.argv[1]), sys.argv[2], sys.argv[3], sys.argv[4:]
    on_screen = phrases(screen_words(ocr_dir, scale))
    if command == "lines":
        for phrase in on_screen:
            x, y = centre(phrase)
            print(f"{round(x)},{round(y)} {' '.join(w['text'] for w in phrase)}")
    elif command == "find":
        words = find(on_screen, args[0])
        if not words:
            sys.exit(1)
        print(" ".join(str(round(v)) for v in centre(words) + box(words)))
    else:
        sys.exit(f"unknown command: {command}")


main()
