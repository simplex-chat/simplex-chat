#!/usr/bin/env python3
"""Record the relay address, then run the relay with no wrapper in the way.

On the first start (or whenever /out/relay-address.txt is missing) a short
one-shot invocation creates the profile and address if needed, prints the
address, and saves it. Then the real relay is exec'd, so it runs as PID 1 with
no Python left in the process tree. Steady-state starts skip straight to the
exec.

The address is not stored in the database as a ready-to-use string (it is a
binary conn-req blob plus short-link data, re-encoded on display), so the
relay's own output is the canonical source.
"""
import os
import subprocess
import sys

BIN = "simplex-chat-relay"
WEB_ROOT = "/var/www/relay-web-channels"
ADDR_FILE = "/out/relay-address.txt"
CAPTURE_TIMEOUT = 180  # seconds; first address creation involves an SMP round-trip


def require(name):
    value = os.environ.get(name)
    if not value:
        sys.exit(f"{name} is required")
    return value


def find_address(text):
    for token in text.split():
        if token.startswith("https://") or token.startswith("simplex:"):
            return token
    return None


def capture_address(oneshot):
    """One-shot: create the profile/address if needed, print it, and save it.

    Runs before the real relay starts, so there is never a second agent
    subscribing to the same queues. If it can't capture (e.g. SMP briefly
    unreachable), the relay still creates and serves the address, and the next
    start retries because the file is still missing.
    """
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
    sys.stdout.write(out)  # keep it in the container logs
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

    # First start (or the file was removed): bootstrap and capture the address,
    # then the real relay reuses it. The avatar only applies at profile
    # creation, so it goes on the one-shot, not the long-running relay.
    if not os.path.exists(ADDR_FILE):
        oneshot = common + (["--user-image-file", image_file] if image_file else []) + ["-d", conn]
        capture_address(oneshot)

    # Replace this process with the relay: PID 1, native signals, no wrapper.
    relay = common + [
        "--relay-web-domain", domain,
        "--relay-web-dir", f"{WEB_ROOT}/channel",
        "--relay-web-cors-file", f"{WEB_ROOT}/cors.conf",
        "--relay-web-interval", "30",
        "-d", conn,
        "--create-schema",
    ]
    os.execvp(relay[0], relay)


if __name__ == "__main__":
    main()
