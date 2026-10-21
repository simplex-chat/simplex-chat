#!/bin/bash

# Carries the test backend's stopped work dir, its server keys and databases, from the app job to the Claude job as an
# artifact, which anyone signed in to GitHub can download from this public repository, so it is encrypted to
# backend-state.crt, whose private key only the Claude job holds (the secret BACKEND_STATE_KEY in app-sim-claude).
# Usage:
#   scripts/ci/backend-state.sh pack <work dir> <file>     writes the work dir without its binaries, encrypted, to <file>
#   scripts/ci/backend-state.sh unpack <file> <work dir>   decrypts <file> into <work dir> with the key in BACKEND_STATE_KEY

set -euo pipefail

cert=$(dirname "$0")/backend-state.crt

# OpenSSL 3 on both runners: macOS's own openssl is LibreSSL
if [ "$(uname)" = Darwin ]; then
  openssl=$(brew --prefix openssl@3)/bin/openssl
else
  openssl=openssl
fi

pack() {
  tar -czf - --exclude=./bin -C "$1" . |
    "$openssl" cms -encrypt -binary -aes-256-gcm -recip "$cert" -keyopt rsa_padding_mode:oaep -outform DER -out "$2"
}

unpack() {
  : "${BACKEND_STATE_KEY:?not set: the environment app-sim-claude needs this secret}"
  mkdir -p "$2"
  "$openssl" cms -decrypt -binary -inform DER -in "$1" -inkey <(printf '%s\n' "$BACKEND_STATE_KEY") |
    tar -xzf - -C "$2"
}

case "${1:-}" in
  pack) pack "$2" "$3" ;;
  unpack) unpack "$2" "$3" ;;
  *)
    echo "usage: $0 pack <work dir> <file> | unpack <file> <work dir>"
    exit 1
    ;;
esac
