#!/usr/bin/env python3

import json
import os
import struct
import sys
import zlib
from pathlib import Path

CHANNEL_TOLERANCE = 8


def unfilter(kind, line, prev, bpp):
    if kind == 1:
        for i in range(bpp, len(line)):
            line[i] = (line[i] + line[i - bpp]) & 0xFF
    elif kind == 2:
        for i in range(len(line)):
            line[i] = (line[i] + prev[i]) & 0xFF
    elif kind == 3:
        for i in range(len(line)):
            left = line[i - bpp] if i >= bpp else 0
            line[i] = (line[i] + ((left + prev[i]) >> 1)) & 0xFF
    elif kind == 4:
        for i in range(len(line)):
            a = line[i - bpp] if i >= bpp else 0
            b = prev[i]
            c = prev[i - bpp] if i >= bpp else 0
            p = a + b - c
            pa, pb, pc = abs(p - a), abs(p - b), abs(p - c)
            line[i] = (line[i] + (a if pa <= pb and pa <= pc else b if pb <= pc else c)) & 0xFF


def read_rgb(path):
    data = path.read_bytes()
    pos, idat = 8, []
    while pos < len(data):
        length, kind = struct.unpack(">I4s", data[pos:pos + 8])
        body = data[pos + 8:pos + 8 + length]
        pos += 12 + length
        if kind == b"IHDR":
            width, height, depth, color, _, _, interlace = struct.unpack(">IIBBBBB", body)
        elif kind == b"IDAT":
            idat.append(body)
    if depth not in (8, 16) or color not in (2, 6) or interlace:
        raise ValueError(f"{path.name}: bit depth {depth}, colour type {color}, interlace {interlace}")
    channels = 3 if color == 2 else 4
    bpp = channels * depth // 8
    stride = width * bpp
    raw = zlib.decompress(b"".join(idat))
    rows, prev = [], bytearray(stride)
    for y in range(height):
        start = y * (stride + 1)
        line = bytearray(raw[start + 1:start + 1 + stride])
        unfilter(raw[start], line, prev, bpp)
        prev = line
        high = line[0::2] if depth == 16 else line
        rgb = bytearray(width * 3)
        for c in range(3):
            rgb[c::3] = high[c::channels]
        rows.append(bytes(rgb))
    return width, height, rows


def write_rgb(path, width, rows):
    def chunk(kind, body):
        return struct.pack(">I", len(body)) + kind + body + struct.pack(">I", zlib.crc32(kind + body))
    raw = b"".join(b"\0" + row for row in rows)
    path.write_bytes(
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", struct.pack(">IIBBBBB", width, len(rows), 8, 2, 0, 0, 0))
        + chunk(b"IDAT", zlib.compress(raw, 6))
        + chunk(b"IEND", b"")
    )


def changed_pixels(ref_rows, out_rows, width):
    changed, max_delta = {}, 0
    for y, (a, b) in enumerate(zip(ref_rows, out_rows)):
        if a == b:
            continue
        for x in range(width):
            i = x * 3
            delta = max(abs(a[i] - b[i]), abs(a[i + 1] - b[i + 1]), abs(a[i + 2] - b[i + 2]))
            max_delta = max(max_delta, delta)
            if delta > CHANNEL_TOLERANCE:
                changed.setdefault(y, []).append(x)
    return changed, max_delta


def diff_image(path, ref, out, changed):
    (rw, rh, ref_rows), (ow, oh, out_rows) = ref, out
    height = max(rh, oh)
    grey = bytes([128]) * 3
    rows = []
    for y in range(height):
        a = ref_rows[y] if y < rh else grey * rw
        b = out_rows[y] if y < oh else grey * ow
        row = a + grey * 8 + b
        if (rw, rh) == (ow, oh):
            mask = bytearray(255 - (255 - v) // 4 for v in a)
            for x in changed.get(y, []):
                mask[x * 3:x * 3 + 3] = b"\xff\0\0"
            row += grey * 8 + mask
        rows.append(bytes(row))
    write_rgb(path, len(rows[0]) // 3, rows)


def compare(ref_path, out_path, diff_dir):
    if ref_path.read_bytes() == out_path.read_bytes():
        return {"status": "equal"}
    ref, out = read_rgb(ref_path), read_rgb(out_path)
    if ref[:2] != out[:2]:
        diff_image(diff_dir / out_path.name, ref, out, {})
        return {"status": "size", "reference": list(ref[:2]), "render": list(out[:2])}
    changed, max_delta = changed_pixels(ref[2], out[2], ref[0])
    if not changed:
        return {"status": "within tolerance", "maxDelta": max_delta}
    diff_image(diff_dir / out_path.name, ref, out, changed)
    return {"status": "changed", "changedPixels": sum(len(xs) for xs in changed.values()), "maxDelta": max_delta}


def main():
    if len(sys.argv) != 4:
        sys.exit("usage: snapshots-compare.py <references dir> <renders dir> <diff dir>")
    references, renders, diff_dir = map(Path, sys.argv[1:])
    diff_dir.mkdir(parents=True, exist_ok=True)
    rendered = {e["file"] for index in sorted(renders.glob("index-*.json")) for e in json.loads(index.read_text())}
    expected = {p.name for p in references.glob("*.png")}
    results = {}
    for name in sorted(expected | rendered):
        if name not in rendered:
            results[name] = {"status": "missing"}
        elif name not in expected:
            results[name] = {"status": "extra"}
        else:
            results[name] = compare(references / name, renders / name, diff_dir)
    failed = {n: r for n, r in results.items() if r["status"] not in ("equal", "within tolerance")}
    (renders / "compare.json").write_text(json.dumps({"results": results, "failed": sorted(failed)}, indent=2, sort_keys=True))
    lines = [f"{len(results)} snapshots, {len(failed)} differ from the references in {references}"]
    lines += [f"- {n}: {', '.join(f'{k} {v}' for k, v in sorted(r.items()))}" for n, r in failed.items()]
    print("\n".join(lines))
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a") as f:
            f.write("\n".join(["## Snapshots", ""] + lines) + "\n")
    sys.exit(1 if failed else 0)


main()
