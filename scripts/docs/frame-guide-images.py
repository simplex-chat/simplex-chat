#!/usr/bin/env python3
import argparse
import base64
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

PIXEL_FRAME = "pixel_7"
PIXEL_FRAME_URL = "https://android.googlesource.com/platform/tools/adt/idea/+/ab04bfbfaaedfcb539b3768c2e8ef1368ea947a4/artwork/resources/device-art-resources/{}/{}?format=TEXT"
PIXEL_SCREEN_LEFT = 59
PIXEL_SCREEN_TOP = 58
PIXEL_SCALE = 2

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
    "groups-chat.png": "ChatView_Group",
    "groups-create.png": "AddGroupView",
    "groups-info.png": "GroupChatInfoView",
    "groups-member-info.png": "GroupMemberInfoView",
    "groups-preferences.png": "GroupPreferencesView",
    "groups-profile.png": "GroupProfileView",
    "linked-devices-connect-to-desktop.png": "ConnectDesktopView",
    "making-connections-address.png": "UserAddressCreated",
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

PIXEL_FRAMED = {
    "notifications-android-modes.png": "notifications-mode",
}

CODE_LENGTH_ORDER = (17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

DISTANCE_MAP = (
    (0, 1), (1, 0), (1, 1), (-1, 1), (0, 2), (2, 0), (1, 2),
    (-1, 2), (2, 1), (-2, 1), (2, 2), (-2, 2), (0, 3), (3, 0),
    (1, 3), (-1, 3), (3, 1), (-3, 1), (2, 3), (-2, 3), (3, 2),
    (-3, 2), (0, 4), (4, 0), (1, 4), (-1, 4), (4, 1), (-4, 1),
    (3, 3), (-3, 3), (2, 4), (-2, 4), (4, 2), (-4, 2), (0, 5),
    (3, 4), (-3, 4), (4, 3), (-4, 3), (5, 0), (1, 5), (-1, 5),
    (5, 1), (-5, 1), (2, 5), (-2, 5), (5, 2), (-5, 2), (4, 4),
    (-4, 4), (3, 5), (-3, 5), (5, 3), (-5, 3), (0, 6), (6, 0),
    (1, 6), (-1, 6), (6, 1), (-6, 1), (2, 6), (-2, 6), (6, 2),
    (-6, 2), (4, 5), (-4, 5), (5, 4), (-5, 4), (3, 6), (-3, 6),
    (6, 3), (-6, 3), (0, 7), (7, 0), (1, 7), (-1, 7), (5, 5),
    (-5, 5), (7, 1), (-7, 1), (4, 6), (-4, 6), (6, 4), (-6, 4),
    (2, 7), (-2, 7), (7, 2), (-7, 2), (3, 7), (-3, 7), (7, 3),
    (-7, 3), (5, 6), (-5, 6), (6, 5), (-6, 5), (8, 0), (4, 7),
    (-4, 7), (7, 4), (-7, 4), (8, 1), (8, 2), (6, 6), (-6, 6),
    (8, 3), (5, 7), (-5, 7), (7, 5), (-7, 5), (8, 4), (6, 7),
    (-6, 7), (7, 6), (-7, 6), (8, 5), (7, 7), (-7, 7), (8, 6),
    (8, 7),
)


def main():
    parser = argparse.ArgumentParser(description="Places the app screens used by the user guide into device frames: iOS renders into Apple's iPhone 17 Pro bezel, Android emulator screenshots into Google's Pixel 7 frame.")
    parser.add_argument("--cache", default=os.path.join(tempfile.gettempdir(), "simplex-guide-bezel"), help="directory for the downloaded frames, outside the repository")
    parser.add_argument("--references", default=REFERENCES, help="directory of the snapshot references")
    parser.add_argument("--android-screens", help="directory of the android-smoke-test artifact of an android-emu run, taken with the emulator's Pixel 7 display overlays; Android screens are framed only when it is given")
    args = parser.parse_args()
    bezel_w, bezel_h, bezel = downscale(*read_png(bezel_png(args.cache)), BEZEL_SCALE)
    left, top, screen_w, screen_h = screen_area(bezel_w, bezel_h, bezel)
    for image, view in FRAMED.items():
        w, h, render = read_png(os.path.join(args.references, f"{view}-light-large.png"))
        if (w, h) != (screen_w, screen_h):
            sys.exit(f"{view}: render is {w}x{h}, the bezel screen is {screen_w}x{screen_h}")
        write_png(os.path.join(GUIDE_IMAGES, image), bezel_w, bezel_h, composite_over(place(render, w, h, bezel_w, bezel_h, left, top), bezel))
        print(f"{image}: {view}")
    if args.android_screens:
        (back_w, back_h, back), (mask_w, mask_h, mask) = (read_webp(pixel_layer(args.cache, layer)) for layer in ("back.webp", "mask.webp"))
        for image, shot in PIXEL_FRAMED.items():
            screens = [name for name in os.listdir(args.android_screens) if name.endswith(f"-{shot}.png")]
            if len(screens) != 1:
                sys.exit(f"{shot}: {len(screens)} screenshots in {args.android_screens}")
            w, h, screen = read_png(os.path.join(args.android_screens, screens[0]))
            if (w, h) != (mask_w, mask_h):
                sys.exit(f"{shot}: screenshot is {w}x{h}, the Pixel 7 screen is {mask_w}x{mask_h}")
            framed = composite_over(back, place(composite_over(screen, mask), w, h, back_w, back_h, PIXEL_SCREEN_LEFT, PIXEL_SCREEN_TOP))
            write_png(os.path.join(GUIDE_IMAGES, image), *downscale(back_w, back_h, framed, PIXEL_SCALE))
            print(f"{image}: {screens[0]}")


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


def pixel_layer(cache, layer):
    path = os.path.join(cache, f"{PIXEL_FRAME}-{layer}")
    if not os.path.exists(path):
        os.makedirs(cache, exist_ok=True)
        with urllib.request.urlopen(PIXEL_FRAME_URL.format(PIXEL_FRAME, layer)) as response:
            data = base64.b64decode(response.read())
        with open(path, "wb") as f:
            f.write(data)
    return path


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


def place(rgba, w, h, canvas_w, canvas_h, left, top):
    canvas = bytearray(canvas_w * canvas_h * 4)
    for y in range(h):
        start = ((top + y) * canvas_w + left) * 4
        canvas[start:start + w * 4] = rgba[y * w * 4:(y + 1) * w * 4]
    return canvas


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


def read_webp(path):
    data = open(path, "rb").read()
    if data[0:4] != b"RIFF" or data[8:12] != b"WEBP":
        sys.exit(f"{path}: not a WebP image")
    position = 12
    while data[position:position + 4] != b"VP8L":
        if position >= len(data):
            sys.exit(f"{path}: not a lossless WebP image")
        position += 8 + (struct.unpack("<I", data[position + 4:position + 8])[0] + 1 & ~1)
    bits = Bits(data[position + 8:])
    if bits.read(8) != 0x2F:
        sys.exit(f"{path}: invalid lossless WebP signature")
    w = bits.read(14) + 1
    h = bits.read(14) + 1
    bits.read(1)
    if bits.read(3):
        sys.exit(f"{path}: unsupported lossless WebP version")
    transforms = []
    xsize = w
    while bits.read(1):
        kind = bits.read(2)
        if kind in (0, 1):
            size_bits = bits.read(3) + 2
            transforms.append((kind, xsize, size_bits, read_image(bits, subsample(xsize, size_bits), subsample(h, size_bits), False)))
        elif kind == 2:
            transforms.append((kind, xsize, 0, None))
        else:
            size = bits.read(8) + 1
            table = read_image(bits, size, 1, False)
            for i in range(1, size):
                table[i] = add_pixels(table[i], table[i - 1])
            width_bits = 3 if size <= 2 else 2 if size <= 4 else 1 if size <= 16 else 0
            transforms.append((kind, xsize, width_bits, table))
            xsize = subsample(xsize, width_bits)
    pixels = read_image(bits, xsize, h, True)
    for kind, width, size_bits, transform_data in reversed(transforms):
        pixels = (inverse_predictor, inverse_color, inverse_subtract_green, inverse_color_indexing)[kind](pixels, width, h, size_bits, transform_data)
    argb = struct.pack(f">{w * h}I", *pixels)
    rgba = bytearray(w * h * 4)
    for c in range(4):
        rgba[c::4] = argb[(c + 1) % 4::4]
    return w, h, rgba


class Bits:
    def __init__(self, data):
        self.data = data
        self.position = 0

    def read(self, n):
        value = 0
        for i in range(n):
            value |= (self.data[self.position >> 3] >> (self.position & 7) & 1) << i
            self.position += 1
        return value


def subsample(size, bits):
    return (size + (1 << bits) - 1) >> bits


def read_image(bits, w, h, argb_image):
    cache_bits = 0
    if bits.read(1):
        cache_bits = bits.read(4)
        if not 1 <= cache_bits <= 11:
            sys.exit(f"invalid color cache size {cache_bits}")
    cache_size = 1 << cache_bits if cache_bits else 0
    prefix_bits = 0
    groups_image = None
    if argb_image and bits.read(1):
        prefix_bits = bits.read(3) + 2
        groups_image = [p >> 8 & 0xFFFF for p in read_image(bits, subsample(w, prefix_bits), subsample(h, prefix_bits), False)]
    groups = [[read_code(bits, size) for size in (280 + cache_size, 256, 256, 256, 40)] for _ in range(max(groups_image) + 1 if groups_image else 1)]
    groups_width = subsample(w, prefix_bits)
    cache = [0] * cache_size
    pixels = [0] * (w * h)
    i = cached = 0
    while i < len(pixels):
        codes = groups[groups_image[(i // w >> prefix_bits) * groups_width + (i % w >> prefix_bits)]] if groups_image else groups[0]
        symbol = decode(bits, codes[0])
        if symbol < 256:
            red, blue, alpha = (decode(bits, code) for code in codes[1:4])
            pixels[i] = alpha << 24 | red << 16 | symbol << 8 | blue
            i += 1
        elif symbol < 280:
            length = prefix_value(bits, symbol - 256)
            code = prefix_value(bits, decode(bits, codes[4]))
            if code > 120:
                offset = code - 120
            else:
                dx, dy = DISTANCE_MAP[code - 1]
                offset = max(dx + dy * w, 1)
            if offset > i or i + length > len(pixels):
                sys.exit("invalid backward reference")
            for _ in range(length):
                pixels[i] = pixels[i - offset]
                i += 1
        else:
            pixels[i] = cache[symbol - 280]
            i += 1
        while cached < i and cache_size:
            cache[(0x1E35A7BD * pixels[cached] & 0xFFFFFFFF) >> 32 - cache_bits] = pixels[cached]
            cached += 1
    return pixels


def read_code(bits, alphabet):
    lengths = [0] * alphabet
    if bits.read(1):
        count = bits.read(1) + 1
        lengths[bits.read(8 if bits.read(1) else 1)] = 1
        if count == 2:
            lengths[bits.read(8)] = 1
        return prefix_code(lengths)
    code_length_lengths = [0] * 19
    for i in range(bits.read(4) + 4):
        code_length_lengths[CODE_LENGTH_ORDER[i]] = bits.read(3)
    code_length_code = prefix_code(code_length_lengths)
    max_symbol = bits.read(2 + 2 * bits.read(3)) + 2 if bits.read(1) else alphabet
    if max_symbol > alphabet:
        sys.exit("invalid prefix code size")
    symbol = 0
    previous = 8
    while symbol < alphabet and max_symbol:
        max_symbol -= 1
        length = decode(bits, code_length_code)
        if length < 16:
            lengths[symbol] = length
            symbol += 1
            if length:
                previous = length
        else:
            extra, offset = ((2, 3), (3, 3), (7, 11))[length - 16]
            repeat = bits.read(extra) + offset
            if symbol + repeat > alphabet:
                sys.exit("invalid prefix code lengths")
            lengths[symbol:symbol + repeat] = [previous if length == 16 else 0] * repeat
            symbol += repeat
    return prefix_code(lengths)


def prefix_code(lengths):
    used = [symbol for symbol, length in enumerate(lengths) if length]
    if len(used) == 1:
        return used[0]
    counts = [0] * 16
    for symbol in used:
        counts[lengths[symbol]] += 1
    return counts, sorted(used, key=lambda symbol: (lengths[symbol], symbol))


def decode(bits, code):
    if isinstance(code, int):
        return code
    counts, symbols = code
    value = first = index = 0
    for length in range(1, 16):
        value |= bits.read(1)
        if value - first < counts[length]:
            return symbols[index + value - first]
        index += counts[length]
        first = (first + counts[length]) << 1
        value <<= 1
    sys.exit("invalid prefix code")


def prefix_value(bits, code):
    if code < 4:
        return code + 1
    extra = (code - 2) >> 1
    return ((2 + (code & 1)) << extra) + bits.read(extra) + 1


def add_pixels(a, b):
    return (a & 0xFF00FF00) + (b & 0xFF00FF00) & 0xFF00FF00 | (a & 0x00FF00FF) + (b & 0x00FF00FF) & 0x00FF00FF


def average2(a, b):
    return (((a ^ b) & 0xFEFEFEFE) >> 1) + (a & b)


def per_channel(f, *pixels):
    return sum(f(*(p >> shift & 255 for p in pixels)) << shift for shift in (0, 8, 16, 24))


def clamp(c):
    return min(max(c, 0), 255)


def select(left, top, top_left):
    to_left = sum(abs((top >> shift & 255) - (top_left >> shift & 255)) for shift in (0, 8, 16, 24))
    to_top = sum(abs((left >> shift & 255) - (top_left >> shift & 255)) for shift in (0, 8, 16, 24))
    return left if to_left < to_top else top


PREDICTORS = (
    lambda left, top, top_right, top_left: 0xFF000000,
    lambda left, top, top_right, top_left: left,
    lambda left, top, top_right, top_left: top,
    lambda left, top, top_right, top_left: top_right,
    lambda left, top, top_right, top_left: top_left,
    lambda left, top, top_right, top_left: average2(average2(left, top_right), top),
    lambda left, top, top_right, top_left: average2(left, top_left),
    lambda left, top, top_right, top_left: average2(left, top),
    lambda left, top, top_right, top_left: average2(top_left, top),
    lambda left, top, top_right, top_left: average2(top, top_right),
    lambda left, top, top_right, top_left: average2(average2(left, top_left), average2(top, top_right)),
    lambda left, top, top_right, top_left: select(left, top, top_left),
    lambda left, top, top_right, top_left: per_channel(lambda a, b, c: clamp(a + b - c), left, top, top_left),
    lambda left, top, top_right, top_left: per_channel(lambda a, b: clamp(a + int((a - b) / 2)), average2(left, top), top_left),
    lambda left, top, top_right, top_left: 0xFF000000,
    lambda left, top, top_right, top_left: 0xFF000000,
)


def inverse_predictor(pixels, w, h, size_bits, modes):
    blocks_width = subsample(w, size_bits)
    for i in range(len(pixels)):
        y, x = divmod(i, w)
        if y == 0:
            prediction = pixels[i - 1] if x else 0xFF000000
        elif x == 0:
            prediction = pixels[i - w]
        else:
            mode = modes[(y >> size_bits) * blocks_width + (x >> size_bits)] >> 8 & 15
            prediction = PREDICTORS[mode](pixels[i - 1], pixels[i - w], pixels[i - w + 1], pixels[i - w - 1])
        pixels[i] = add_pixels(pixels[i], prediction)
    return pixels


def color_delta(t, c):
    return (t - (t >> 7 << 8)) * (c - (c >> 7 << 8)) >> 5


def inverse_color(pixels, w, h, size_bits, elements):
    blocks_width = subsample(w, size_bits)
    for i, p in enumerate(pixels):
        y, x = divmod(i, w)
        element = elements[(y >> size_bits) * blocks_width + (x >> size_bits)]
        green = p >> 8 & 255
        red = (p >> 16 & 255) + color_delta(element & 255, green) & 255
        blue = (p & 255) + color_delta(element >> 8 & 255, green) + color_delta(element >> 16 & 255, red) & 255
        pixels[i] = p & 0xFF00FF00 | red << 16 | blue
    return pixels


def inverse_subtract_green(pixels, w, h, size_bits, data):
    for i, p in enumerate(pixels):
        green = p >> 8 & 255
        pixels[i] = p & 0xFF00FF00 | ((p >> 16) + green & 255) << 16 | (p + green & 255)
    return pixels


def inverse_color_indexing(pixels, w, h, width_bits, table):
    packed_width = subsample(w, width_bits)
    index_bits = 8 >> width_bits
    out = [0] * (w * h)
    for y in range(h):
        for x in range(w):
            index = (pixels[y * packed_width + (x >> width_bits)] >> 8 + (x & (1 << width_bits) - 1) * index_bits) & (1 << index_bits) - 1
            out[y * w + x] = table[index] if index < len(table) else 0
    return out


if __name__ == "__main__":
    main()
