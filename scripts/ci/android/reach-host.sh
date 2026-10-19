#!/bin/bash

# Checks how the emulator reaches a server on the runner's 127.0.0.1, as the test backend's SMP server listens:
# through 10.0.2.2, the emulator's alias for the host's loopback, and through 127.0.0.1 after adb reverse.
# Usage: scripts/ci/android/reach-host.sh <port>

set -euo pipefail

port=$1
dir=$(mktemp -d)
echo "runner ok" > "$dir/index.html"
python3 -m http.server "$port" --bind 127.0.0.1 --directory "$dir" > "$dir/server.log" 2>&1 &
server=$!
trap 'kill "$server"' EXIT
until (exec 3<> "/dev/tcp/127.0.0.1/$port") 2> /dev/null; do
  sleep 0.5
done

get() {
  adb shell "printf 'GET / HTTP/1.0\r\n\r\n' | nc -w 5 $1 $port" | tr -d '\r' | tail -n 1
}

echo "10.0.2.2:$port from the emulator: $(get 10.0.2.2 || true)"
adb reverse "tcp:$port" "tcp:$port"
echo "127.0.0.1:$port from the emulator after adb reverse: $(get 127.0.0.1 || true)"
adb reverse --remove "tcp:$port"
