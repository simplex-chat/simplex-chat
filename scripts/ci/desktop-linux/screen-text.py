#!/usr/bin/env python3

# Reads the words tesseract found on a screenshot (TSV output, one file per OCR pass) and
# prints the phrases on screen, or the centre of one phrase.
#
#   screen-text.py <scale> lines <tsv>...        one phrase per line: centre x,y and its words
#   screen-text.py <scale> find <text> <tsv>...  "x y" of the first phrase containing the text; fails if none does
#
# <scale> is how much the screenshot was enlarged for OCR; printed coordinates are screen pixels.
# Words are compared ignoring case and punctuation.

import csv
import re
import sys

MIN_CONFIDENCE = 10


def key(text):
    return re.sub(r"[^0-9a-z]", "", text.lower())


def read_words(path, scale):
    words = []
    with open(path, newline="") as f:
        for row in csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE):
            text = (row["text"] or "").strip()
            if row["level"] != "5" or not key(text) or float(row["conf"]) < MIN_CONFIDENCE:
                continue
            left, top, width, height = (int(row[k]) / scale for k in ("left", "top", "width", "height"))
            words.append({"text": text, "key": key(text), "conf": float(row["conf"]),
                          "l": left, "t": top, "r": left + width, "b": top + height})
    return words


def area(w):
    return (w["r"] - w["l"]) * (w["b"] - w["t"])


def same_place(a, b):
    dx = min(a["r"], b["r"]) - max(a["l"], b["l"])
    dy = min(a["b"], b["b"]) - max(a["t"], b["t"])
    return dx > 0 and dy > 0 and dx * dy >= 0.5 * min(area(a), area(b))


# The passes read the same text twice where both can; keep the more confident reading of each place.
def merge(passes):
    kept = []
    for w in sorted((w for words in passes for w in words), key=lambda w: -w["conf"]):
        if not any(same_place(w, k) for k in kept):
            kept.append(w)
    return kept


def centre(words):
    return ((min(w["l"] for w in words) + max(w["r"] for w in words)) / 2,
            (min(w["t"] for w in words) + max(w["b"] for w in words)) / 2)


# Words on one row that are closer than 1.5 of their height form a phrase; wider gaps separate UI elements.
def phrases(words):
    rows = []
    for w in sorted(words, key=lambda w: (w["t"] + w["b"]) / 2):
        cy, h = (w["t"] + w["b"]) / 2, w["b"] - w["t"]
        row = next((r for r in rows if abs(r["cy"] - cy) <= 0.5 * min(r["h"], h)), None)
        if row:
            row["words"].append(w)
        else:
            rows.append({"cy": cy, "h": h, "words": [w]})
    result = []
    for row in rows:
        phrase = []
        for w in sorted(row["words"], key=lambda w: w["l"]):
            if phrase and w["l"] - phrase[-1]["r"] > 1.5 * (w["b"] - w["t"]):
                result.append(phrase)
                phrase = []
            phrase.append(w)
        result.append(phrase)
    return sorted(result, key=lambda p: (min(w["t"] for w in p), p[0]["l"]))


# OCR can glue an icon to the first word or punctuation to the last, so long edge words match by suffix or prefix.
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
    scale, command, args = float(sys.argv[1]), sys.argv[2], sys.argv[3:]
    if command == "lines":
        for phrase in phrases(merge(read_words(p, scale) for p in args)):
            x, y = centre(phrase)
            print(f"{round(x)},{round(y)} {' '.join(w['text'] for w in phrase)}")
    elif command == "find":
        found = find(phrases(merge(read_words(p, scale) for p in args[1:])), args[0])
        if not found:
            sys.exit(1)
        print(f"{round(found[0])} {round(found[1])}")
    else:
        sys.exit(f"unknown command: {command}")


main()
