#!/usr/bin/env python3
import argparse
import bz2
import os
import plistlib
import struct
import sys
import tempfile
import urllib.request
import zlib

BEZEL_DMG_URL = "https://devimages-cdn.apple.com/design/resources/download/Bezel-iPhone-17.dmg"
BEZEL_PNG_PATH = "PNG/iPhone 17 Pro/iPhone 17 Pro - Silver - Portrait.png"
BEZEL_SCALE = 3

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
REFERENCES = os.path.join(REPO, "apps/ios/SimpleX Snapshots/References")
GUIDE_IMAGES = os.path.join(REPO, "docs/guide/images")

FRAMED = {
    "app-settings-advanced-network-settings.png": "AdvancedNetworkSettings",
    "app-settings-settings.png": "SettingsView",
    "audio-video-calls-settings.png": "CallSettings",
    "channels-create.png": "AddChannelView",
    "chat-profiles-create-profile.png": "CreateProfile",
    "chat-profiles-hide-profile.png": "ProfilePrivacyView",
    "groups-create.png": "AddGroupView",
    "groups-info.png": "GroupChatInfoView",
    "groups-member-info.png": "GroupMemberInfoView",
    "groups-preferences.png": "GroupPreferencesView",
    "groups-profile.png": "GroupProfileView",
    "linked-devices-connect-to-desktop.png": "ConnectDesktopView",
    "making-connections-contact-info.png": "ChatInfoView",
    "managing-data-chat-data.png": "DatabaseView",
    "managing-data-migrate-here.png": "MigrateToDevice",
    "managing-data-passphrase.png": "DatabaseEncryptionView",
    "notifications-ios-modes.png": "NotificationsModeView",
    "privacy-security-verify-code.png": "VerifyCodeView",
    "readme-get-started.png": "SimpleXInfo",
    "send-messages-chat.png": "ChatView",
    "send-messages-contact-preferences.png": "ContactPreferencesView",
    "supporter-badges-redeem-code.png": "BadgesRedeemCodeView",
    "supporter-badges-support-simplex.png": "BadgesSupportSimplexView",
}


def main():
    parser = argparse.ArgumentParser(description="Places the iOS renders used by the user guide into Apple's iPhone 17 Pro bezel.")
    parser.add_argument("--cache", default=os.path.join(tempfile.gettempdir(), "simplex-guide-bezel"), help="directory for the downloaded bezel, outside the repository")
    parser.add_argument("--references", default=REFERENCES, help="directory of the snapshot references")
    args = parser.parse_args()
    bezel_w, bezel_h, bezel = downscale(*read_png(bezel_png(args.cache)), BEZEL_SCALE)
    left, top, screen_w, screen_h = screen_area(bezel_w, bezel_h, bezel)
    for image, view in FRAMED.items():
        w, h, render = read_png(os.path.join(args.references, f"{view}-light-large.png"))
        if (w, h) != (screen_w, screen_h):
            sys.exit(f"{view}: render is {w}x{h}, the bezel screen is {screen_w}x{screen_h}")
        framed = bytearray(bezel_w * bezel_h * 4)
        for y in range(h):
            start = ((top + y) * bezel_w + left) * 4
            framed[start:start + w * 4] = render[y * w * 4:(y + 1) * w * 4]
        write_png(os.path.join(GUIDE_IMAGES, image), bezel_w, bezel_h, composite_over(framed, bezel))
        print(f"{image}: {view}")


def bezel_png(cache):
    png = os.path.join(cache, os.path.basename(BEZEL_PNG_PATH))
    if not os.path.exists(png):
        os.makedirs(cache, exist_ok=True)
        dmg = os.path.join(cache, os.path.basename(BEZEL_DMG_URL))
        if not os.path.exists(dmg):
            urllib.request.urlretrieve(BEZEL_DMG_URL, dmg)
        with open(png, "wb") as f:
            f.write(hfs_file(udif_image(dmg), BEZEL_PNG_PATH))
    return png


def udif_image(dmg):
    with open(dmg, "rb") as f:
        f.seek(-512, 2)
        koly = f.read(512)
        data_fork = struct.unpack(">Q", koly[0x18:0x20])[0]
        xml_offset, xml_length = struct.unpack(">QQ", koly[0xD8:0xE8])
        f.seek(xml_offset)
        blocks = plistlib.loads(f.read(xml_length))["resource-fork"]["blkx"]
        image = bytearray()
        for block in blocks:
            mish = block["Data"]
            first_sector = struct.unpack(">Q", mish[8:16])[0]
            data_offset = struct.unpack(">Q", mish[24:32])[0]
            chunk_count = struct.unpack(">I", mish[0xC8:0xCC])[0]
            for i in range(chunk_count):
                kind, _, sector, sectors, offset, length = struct.unpack(">IIQQQQ", mish[0xCC + 40 * i:0xCC + 40 * (i + 1)])
                if kind in (0, 2, 0x7FFFFFFE, 0xFFFFFFFF):
                    continue
                f.seek(data_fork + data_offset + offset)
                chunk = f.read(length)
                data = {1: lambda c: c, 0x80000005: zlib.decompress, 0x80000006: bz2.decompress}[kind](chunk)
                position = (first_sector + sector) * 512
                if len(image) < position + len(data):
                    image.extend(bytes(position + len(data) - len(image)))
                image[position:position + len(data)] = data
    return image


def hfs_file(image, wanted_path):
    partition = next(
        struct.unpack(">I", image[block * 512 + 8:block * 512 + 12])[0] * 512
        for block in range(1, struct.unpack(">I", image[512 + 4:512 + 8])[0] + 1)
        if image[block * 512 + 48:block * 512 + 57] == b"Apple_HFS"
    )
    header = image[partition + 1024:partition + 1536]
    block_size = struct.unpack(">I", header[40:44])[0]

    def fork(record):
        size = struct.unpack(">Q", record[0:8])[0]
        extents = [struct.unpack(">II", record[16 + 8 * i:24 + 8 * i]) for i in range(8)]
        data = b"".join(image[partition + start * block_size:partition + (start + count) * block_size] for start, count in extents if count)
        return data[:size]

    catalog = fork(header[272:352])
    node_size = struct.unpack(">H", catalog[32:34])[0]
    node = struct.unpack(">I", catalog[24:28])[0]
    folders = {}
    files = {}
    while node:
        data = catalog[node * node_size:(node + 1) * node_size]
        next_node = struct.unpack(">I", data[0:4])[0]
        for i in range(struct.unpack(">H", data[10:12])[0]):
            offset = struct.unpack(">H", data[node_size - 2 * (i + 1):node_size - 2 * i])[0]
            key_length, parent, name_length = struct.unpack(">HIH", data[offset:offset + 8])
            name = data[offset + 8:offset + 8 + 2 * name_length].decode("utf-16-be")
            record = data[offset + 2 + key_length:]
            kind = struct.unpack(">h", record[0:2])[0]
            if kind == 1:
                folders[struct.unpack(">I", record[8:12])[0]] = (parent, name)
            elif kind == 2:
                files[(parent, name)] = record[88:168]
        node = next_node

    def path(parent, name):
        parts = [name]
        while parent in folders and folders[parent][0] != 1:
            parent, folder = folders[parent]
            parts.append(folder)
        return "/".join(reversed(parts))

    return next(fork(record) for (parent, name), record in files.items() if path(parent, name) == wanted_path)


def downscale(w, h, rgba, factor):
    out_w, out_h = w // factor, h // factor
    out = bytearray(out_w * out_h * 4)
    for y in range(out_h):
        for x in range(out_w):
            r = g = b = a = 0
            for dy in range(factor):
                row = ((y * factor + dy) * w + x * factor) * 4
                for dx in range(factor):
                    p = row + dx * 4
                    alpha = rgba[p + 3]
                    r += rgba[p] * alpha
                    g += rgba[p + 1] * alpha
                    b += rgba[p + 2] * alpha
                    a += alpha
            p = (y * out_w + x) * 4
            if a:
                out[p:p + 4] = bytes((round(r / a), round(g / a), round(b / a), round(a / (factor * factor))))
    return out_w, out_h, out


def screen_area(w, h, rgba):
    start = (h // 2) * w + w // 2
    seen = {start}
    stack = [start]
    while stack:
        p = stack.pop()
        x = p % w
        for q in (p - 1 if x > 0 else -1, p + 1 if x < w - 1 else -1, p - w, p + w):
            if 0 <= q < w * h and q not in seen and rgba[q * 4 + 3] < 255:
                seen.add(q)
                stack.append(q)
    xs = [p % w for p in seen]
    ys = [p // w for p in seen]
    return min(xs), min(ys), max(xs) - min(xs) + 1, max(ys) - min(ys) + 1


def composite_over(below, above):
    out = bytearray(len(below))
    for p in range(0, len(below), 4):
        top_alpha = above[p + 3]
        bottom_alpha = below[p + 3] * (255 - top_alpha) // 255
        alpha = top_alpha + bottom_alpha
        if alpha:
            for c in range(3):
                out[p + c] = round((above[p + c] * top_alpha + below[p + c] * bottom_alpha) / alpha)
            out[p + 3] = alpha
    return out


def read_png(path):
    data = open(path, "rb").read()
    position = 8
    compressed = bytearray()
    while position < len(data):
        length = struct.unpack(">I", data[position:position + 4])[0]
        kind = data[position + 4:position + 8]
        body = data[position + 8:position + 8 + length]
        if kind == b"IHDR":
            w, h, depth, color, _, _, interlace = struct.unpack(">IIBBBBB", body)
            if depth != 8 or interlace or color not in (2, 6):
                sys.exit(f"{path}: unsupported PNG format")
        elif kind == b"IDAT":
            compressed += body
        position += 12 + length
    channels = 4 if color == 6 else 3
    raw = zlib.decompress(bytes(compressed))
    stride = w * channels
    pixels = bytearray(h * stride)
    previous = bytearray(stride)
    for y in range(h):
        line = unfilter(raw[y * (stride + 1)], bytearray(raw[y * (stride + 1) + 1:(y + 1) * (stride + 1)]), previous, channels)
        pixels[y * stride:(y + 1) * stride] = line
        previous = line
    if channels == 4:
        return w, h, pixels
    rgba = bytearray(w * h * 4)
    for c in range(3):
        rgba[c::4] = pixels[c::3]
    rgba[3::4] = b"\xff" * (w * h)
    return w, h, rgba


def unfilter(kind, line, previous, channels):
    for x in range(len(line)):
        left = line[x - channels] if x >= channels else 0
        up = previous[x]
        up_left = previous[x - channels] if x >= channels else 0
        if kind == 1:
            line[x] = (line[x] + left) & 255
        elif kind == 2:
            line[x] = (line[x] + up) & 255
        elif kind == 3:
            line[x] = (line[x] + ((left + up) >> 1)) & 255
        elif kind == 4:
            estimate = left + up - up_left
            nearest = min((abs(estimate - left), 0, left), (abs(estimate - up), 1, up), (abs(estimate - up_left), 2, up_left))[2]
            line[x] = (line[x] + nearest) & 255
    return line


def write_png(path, w, h, rgba):
    stride = w * 4
    rows = bytearray()
    for y in range(h):
        line = rgba[y * stride:(y + 1) * stride]
        rows += b"\x01" + bytes(line[:4]) + bytes((line[x] - line[x - 4]) & 255 for x in range(4, stride))

    def chunk(kind, body):
        return struct.pack(">I", len(body)) + kind + body + struct.pack(">I", zlib.crc32(kind + body))

    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n")
        f.write(chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 6, 0, 0, 0)))
        f.write(chunk(b"IDAT", zlib.compress(bytes(rows), 9)))
        f.write(chunk(b"IEND", b""))


if __name__ == "__main__":
    main()
