#!/usr/bin/env python3
"""Save the relay address on first start, then exec the relay.

The address is printed only when it is created, and is not stored in the
database in its displayed form, so it is captured from the relay's output.
"""
import os
import shlex
import subprocess
import sys

BIN = "simplex-chat-relay"
WEB_ROOT = "/var/www/relay-web-channels"
ADDR_FILE = "/out/relay-address.txt"
CAPTURE_TIMEOUT = 180  # seconds
DEFAULT_RTS_OPTS = "-N -F1.2 -A16m -I0.01 -Iw15"
DEFAULT_POOL_SIZE = "4"  # the binary defaults to a single connection
DEFAULT_QUEUE_SIZE = "65536"


def require(name):
    value = os.environ.get(name)
    if not value:
        sys.exit(f"{name} is required")
    return value


def rts_args():
    """RELAY_RTS_OPTS holds bare options; the +RTS/-RTS markers are added here."""
    opts = [
        o
        for o in shlex.split(os.environ.get("RELAY_RTS_OPTS") or DEFAULT_RTS_OPTS)
        if o not in ("+RTS", "-RTS")
    ]
    return ["+RTS", *opts, "-RTS"] if opts else []


def find_address(text):
    for token in text.split():
        if token.startswith("https://") or token.startswith("simplex:"):
            return token
    return None


def capture_address(oneshot):
    """Create the address if needed and save it. Runs before the relay starts."""
    cmd = oneshot + ["--create-schema", "-t", "0", "-e", "/sa"]
    try:
        out = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            errors="replace",
            timeout=CAPTURE_TIMEOUT,
        ).stdout
    except subprocess.TimeoutExpired as exc:
        out = exc.stdout or ""
    sys.stdout.write(out)
    sys.stdout.flush()

    address = find_address(out)
    if address:
        with open(ADDR_FILE, "w") as f:
            f.write(address + "\n")
    else:
        sys.stderr.write("entrypoint: relay address not captured; will retry next start\n")


def main():
    name = require("RELAY_NAME")
    domain = require("RELAY_WEB_DOMAIN")
    conn = require("DB_CONN")
    image_file = os.environ.get("RELAY_IMAGE_FILE")

    os.makedirs(f"{WEB_ROOT}/channel", exist_ok=True)
    os.makedirs("/out", exist_ok=True)

    common = [BIN, "--relay", "--headless", "--user-display-name", name]

    # Only applied when the address is created, which the one-shot below does.
    address_server = os.environ.get("RELAY_ADDRESS_SERVER")
    if address_server:
        common += ["--relay-address-server", address_server]

    # The image is applied only when the profile is created.
    if not os.path.exists(ADDR_FILE):
        oneshot = common + (["--user-image-file", image_file] if image_file else []) + ["-d", conn]
        capture_address(oneshot)

    relay = common + [
        "--relay-web-domain", domain,
        "--relay-web-dir", f"{WEB_ROOT}/channel",
        "--relay-web-cors-file", f"{WEB_ROOT}/cors.conf",
        "--relay-web-interval", "30",
        "-d", conn,
        "--create-schema",
        "--pool-size", os.environ.get("RELAY_POOL_SIZE") or DEFAULT_POOL_SIZE,
        "--queue-size", os.environ.get("RELAY_QUEUE_SIZE") or DEFAULT_QUEUE_SIZE,
    ] + rts_args()
    os.execvp(relay[0], relay)


if __name__ == "__main__":
    main()
