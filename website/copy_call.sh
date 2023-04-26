#!/usr/bin/env bash
# Safety measures
set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

mkdir -p ./src/call
cp ./node_modules/@simplex-chat/webrtc/dist/call.js          ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/call.js.map      ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/ui.js            ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/style.css        ./src/call/
cp ./node_modules/@simplex-chat/webrtc/dist/webcall.html     ./src/call/index.html
cp ./node_modules/@simplex-chat/webrtc/dist/lz-string.min.js ./src/call/
