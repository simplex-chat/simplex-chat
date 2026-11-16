#!/usr/bin/env python3

# The name resolver that the test smp-server's [NAMES] section points at, as simplexmq's Server/Names/HttpResolver.hs
# calls it: GET /v2/resolve/<query> answers a NameResponse for the query "[<keccak-256 of the lowercased label, hex>]<.tld>"
# (or the plain name, which older clients send), GET /health answers {}. Registered names come from a JSON file, read on every request, so names can be added while it runs:
#   {"cinews.testing": {"simplexChannel": ["https://..."], "simplexContact": []}, ...}
# Every other name is answered as available.
# Usage: names-resolver.py <port> <names json>

import json
import sys
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

ROUND_CONSTANTS = [
    0x0000000000000001, 0x0000000000008082, 0x800000000000808A, 0x8000000080008000,
    0x000000000000808B, 0x0000000080000001, 0x8000000080008081, 0x8000000000008009,
    0x000000000000008A, 0x0000000000000088, 0x0000000080008009, 0x000000008000000A,
    0x000000008000808B, 0x800000000000008B, 0x8000000000008089, 0x8000000000008003,
    0x8000000000008002, 0x8000000000000080, 0x000000000000800A, 0x800000008000000A,
    0x8000000080008081, 0x8000000000008080, 0x0000000080000001, 0x8000000080008008,
]
ROTATIONS = [[0, 36, 3, 41, 18], [1, 44, 10, 45, 2], [62, 6, 43, 15, 61], [28, 55, 25, 21, 56], [27, 20, 39, 8, 14]]
MASK = (1 << 64) - 1


def rol(v, n):
    return ((v << n) | (v >> (64 - n))) & MASK if n else v


def keccak_f(a):
    for rc in ROUND_CONSTANTS:
        c = [a[x][0] ^ a[x][1] ^ a[x][2] ^ a[x][3] ^ a[x][4] for x in range(5)]
        d = [c[(x - 1) % 5] ^ rol(c[(x + 1) % 5], 1) for x in range(5)]
        a = [[a[x][y] ^ d[x] for y in range(5)] for x in range(5)]
        b = [[0] * 5 for _ in range(5)]
        for x in range(5):
            for y in range(5):
                b[y][(2 * x + 3 * y) % 5] = rol(a[x][y], ROTATIONS[x][y])
        a = [[b[x][y] ^ (~b[(x + 1) % 5][y] & b[(x + 2) % 5][y]) for y in range(5)] for x in range(5)]
        a[0][0] ^= rc
    return a


# Keccak-256 as Ethereum uses it (padding 0x01, not SHA3's 0x06), which hashlib does not have
def keccak256(data):
    rate = 136
    padded = bytearray(data) + b"\x01" + bytes(-(len(data) + 1) % rate)
    padded[-1] |= 0x80
    a = [[0] * 5 for _ in range(5)]
    for offset in range(0, len(padded), rate):
        for i in range(rate // 8):
            a[i % 5][i // 5] ^= int.from_bytes(padded[offset + 8 * i:offset + 8 * i + 8], "little")
        a = keccak_f(a)
    return b"".join(a[i % 5][i // 5].to_bytes(8, "little") for i in range(4))


def name_query(name):
    label, tld = name.lower().split(".", 1)
    return "[" + keccak256(label.encode()).hex() + "]." + tld


def name_response(names_file, query):
    with open(names_file) as f:
        names = json.load(f)
    for name, links in names.items():
        # hashed from SMP v22 (NQHash), the plain name before (NQDomain)
        if query.lower() in (name_query(name), name.lower()):
            record = {
                "name": name,
                "nickname": "",
                "website": "",
                "location": "",
                "simplexContact": links.get("simplexContact", []),
                "simplexChannel": links.get("simplexChannel", []),
                "eth": None,
                "btc": None,
                "xmr": None,
                "dot": None,
                "owner": "",
                "resolver": "",
            }
            return name, {"registration": {"type": "registered", "nameRecord": record}}
    return None, {"registration": {"type": "available", "pricing": {"registrationPrices": {}, "basePrice": 1000, "minLabelLength": 1}}}


class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        path = urllib.parse.unquote(self.path)
        if path == "/health":
            self.reply(200, {})
        elif path.startswith("/v2/resolve/"):
            query = path[len("/v2/resolve/"):]
            name, response = name_response(self.server.names_file, query)
            print(f"resolve {query}: {name or 'not registered'}", flush=True)
            self.reply(200, response)
        else:
            self.reply(404, {})

    def reply(self, status, body):
        data = json.dumps(body).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def log_message(self, format, *args):
        pass


if __name__ == "__main__":
    server = ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1])), Handler)
    server.names_file = sys.argv[2]
    print(f"names resolver on 127.0.0.1:{sys.argv[1]}, names from {sys.argv[2]}", flush=True)
    server.serve_forever()
