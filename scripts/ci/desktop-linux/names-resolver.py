#!/usr/bin/env python3

# A SimpleX name resolver for the test SMP server's [NAMES] section, as simplexmq's test resolver answers:
# GET /v2/resolve/<query> with a NameResponse for every name, registered or not, and GET /health.
# Names are read from the registry file on each request: {"<label>": {"simplexContact": [...], "simplexChannel": [...]}},
# all under .testing. A v22 router asks by label hash, "[<keccak-256 hex>].testing", so names are matched by hash.
# Usage: names-resolver.py <port> <registry .json>

import json
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import unquote

MASK = (1 << 64) - 1
ROTATIONS = [[0, 36, 3, 41, 18], [1, 44, 10, 45, 2], [62, 6, 43, 15, 61], [28, 55, 25, 21, 56], [27, 20, 39, 8, 14]]


def round_constants():
    r, constants = 1, []
    for _ in range(24):
        c = 0
        for j in range(7):
            if r & 1:
                c |= 1 << ((1 << j) - 1)
            r = ((r << 1) ^ 0x71) & 0xFF if r & 0x80 else r << 1
        constants.append(c)
    return constants


ROUND_CONSTANTS = round_constants()


def rotate(x, n):
    return ((x << n) | (x >> (64 - n))) & MASK if n else x


def keccak_f(a):
    for rc in ROUND_CONSTANTS:
        c = [a[x][0] ^ a[x][1] ^ a[x][2] ^ a[x][3] ^ a[x][4] for x in range(5)]
        d = [c[x - 1] ^ rotate(c[(x + 1) % 5], 1) for x in range(5)]
        a = [[a[x][y] ^ d[x] for y in range(5)] for x in range(5)]
        b = [[0] * 5 for _ in range(5)]
        for x in range(5):
            for y in range(5):
                b[y][(2 * x + 3 * y) % 5] = rotate(a[x][y], ROTATIONS[x][y])
        a = [[b[x][y] ^ (~b[(x + 1) % 5][y] & b[(x + 2) % 5][y]) for y in range(5)] for x in range(5)]
        a[0][0] ^= rc
    return a


# Keccak-256 as Ethereum uses it (padding 0x01, not SHA3-256's 0x06), which labelHash in SimplexName.hs is
def keccak256(data):
    rate = 136
    padded = bytearray(data) + b"\x01" + bytes(-(len(data) + 1) % rate)
    padded[-1] |= 0x80
    a = [[0] * 5 for _ in range(5)]
    for block in range(0, len(padded), rate):
        for i in range(rate // 8):
            a[i % 5][i // 5] ^= int.from_bytes(padded[block + 8 * i:block + 8 * i + 8], "little")
        a = keccak_f(a)
    return b"".join(a[i % 5][i // 5].to_bytes(8, "little") for i in range(4))


def query_key(label):
    return "[" + keccak256(label.lower().encode()).hex() + "].testing"


def record(label, links):
    return {
        "name": label + ".testing",
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


def name_response(query, registry_file):
    with open(registry_file) as f:
        registry = json.load(f)
    for label, links in registry.items():
        if query in (query_key(label), label.lower() + ".testing"):
            return {"registration": {"type": "registered", "nameRecord": record(label, links)}}
    return {"registration": {"type": "available", "pricing": {"registrationPrices": {}, "basePrice": 1000, "minLabelLength": 1}}}


class Resolver(BaseHTTPRequestHandler):
    def do_GET(self):
        prefix = "/v2/resolve/"
        if self.path == "/health":
            self.reply(200, {})
        elif self.path.startswith(prefix):
            query = unquote(self.path[len(prefix):])
            response = name_response(query, self.server.registry_file)
            print(f"resolve {query}: {response['registration']['type']}", flush=True)
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


def main():
    server = ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1])), Resolver)
    server.registry_file = sys.argv[2]
    print(f"names resolver on 127.0.0.1:{sys.argv[1]}, registry {sys.argv[2]}", flush=True)
    server.serve_forever()


if __name__ == "__main__":
    main()
