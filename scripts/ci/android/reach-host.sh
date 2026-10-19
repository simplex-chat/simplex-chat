#!/bin/bash

# Checks how the emulator reaches a server on the runner's 127.0.0.1, as the test backend's SMP server listens:
# through 10.0.2.2, the emulator's alias for the host's loopback, and through 127.0.0.1 after adb reverse.
# Each is tried 10 times, a second apart, and every attempt's answer is printed.
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

try() {
  local host=$1 answer
  for attempt in $(seq 10); do
    answer=$(adb shell "printf 'GET / HTTP/1.0\r\n\r\n' | nc -w 5 $host $port" 2>&1 | tr -d '\r' | tail -n 1 || true)
    echo "$host:$port from the emulator, attempt $attempt: ${answer:-no answer}"
    [ "$answer" != "runner ok" ] || return 0
    sleep 1
  done
}

try 10.0.2.2
adb reverse "tcp:$port" "tcp:$port"
adb reverse --list
try 127.0.0.1
adb reverse --remove "tcp:$port"
cat "$dir/server.log"
