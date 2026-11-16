#!/bin/bash

# Boots the AVD "ci" (install-sdk.sh) without a window, with KVM, and waits until Android has booted and its
# package manager answers; then turns animations off and keeps the screen on and unlocked.
# Usage: scripts/ci/android/emulator.sh start <log dir>
#        scripts/ci/android/emulator.sh reverse <port>   forwards the emulator's 127.0.0.1:<port> to the runner's,
#                                                        and waits until the server there answers
# The emulator keeps running for later steps of the job.

set -euo pipefail

boot_secs=600

start() {
  local log=$1/emulator.log started deadline missing=0
  mkdir -p "$1"
  echo 'KERNEL=="kvm", GROUP="kvm", MODE="0666", OPTIONS+="static_node=kvm"' | sudo tee /etc/udev/rules.d/99-kvm4all.rules
  sudo udevadm control --reload-rules
  sudo udevadm trigger --name-match=kvm
  ls -l /dev/kvm
  emulator -accel-check

  local flags=(-avd ci -no-window -no-audio -no-boot-anim -no-snapshot -no-metrics -gpu swiftshader_indirect -camera-back none -camera-front none -memory 4096 -cores 4)
  echo "emulator ${flags[*]}"
  started=$SECONDS
  nohup emulator "${flags[@]}" < /dev/null > "$log" 2>&1 &

  deadline=$((SECONDS + boot_secs))
  until [ "$(adb shell getprop sys.boot_completed 2> /dev/null)" = 1 ]; do
    # the launcher starts qemu with the same arguments; just after the start neither may be running yet
    if pgrep -f -- '-avd ci' > /dev/null; then
      missing=0
    else
      missing=$((missing + 1))
    fi
    if [ "$missing" -ge 5 ] || [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: Android did not boot in $((SECONDS - started)) s$([ "$missing" -ge 5 ] && echo ", the emulator is gone")"
      cat "$log"
      return 1
    fi
    sleep 2
  done
  echo "booted in $((SECONDS - started)) s"
  until adb shell pm path android > /dev/null 2>&1; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: no package manager $((SECONDS - started)) s after the start"
      return 1
    fi
    sleep 2
  done
  echo "package manager ready after $((SECONDS - started)) s"

  adb shell settings put global window_animation_scale 0
  adb shell settings put global transition_animation_scale 0
  adb shell settings put global animator_duration_scale 0
  adb shell svc power stayon true
  adb shell wm dismiss-keyguard
  adb shell getprop | grep -E 'ro.build.version.(sdk|release)|ro.product.cpu.abilist|native.bridge|ro.kernel.qemu' || true
  adb shell wm size
}

# The test SMP server speaks TLS, which answers a plain-text request with an alert: any bytes back mean the request
# reached it. The forward can take a while to answer after the boot.
reverse() {
  local port=$1 deadline=$((SECONDS + 60)) bytes
  if ! [[ "$port" =~ ^[0-9]+$ ]]; then
    echo "error: \"$port\" is not a port"
    return 1
  fi
  adb reverse "tcp:$port" "tcp:$port"
  adb reverse --list
  while :; do
    bytes=$(adb shell "printf 'GET / HTTP/1.0\r\n\r\n' | nc -w 5 127.0.0.1 $port | wc -c" || true)
    bytes=${bytes//[!0-9]/}
    echo "127.0.0.1:$port from the emulator: ${bytes:-no} bytes back"
    [ "${bytes:-0}" -eq 0 ] || return 0
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: the emulator does not reach the runner's 127.0.0.1:$port"
      return 1
    fi
    sleep 2
  done
}

case "${1:-}" in
  start) start "$2" ;;
  reverse) reverse "$2" ;;
  *)
    echo "usage: $0 start <log dir> | reverse <port>"
    exit 1
    ;;
esac
