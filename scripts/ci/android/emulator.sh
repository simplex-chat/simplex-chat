#!/bin/bash

# Boots the AVD "ci" (install-sdk.sh) without a window, with KVM, and waits until Android has booted and its
# package manager answers; then turns animations off and keeps the screen on and unlocked.
# Usage: scripts/ci/android/emulator.sh start <log dir> | stop

set -euo pipefail

boot_secs=600

start() {
  local log=$1/emulator.log started deadline
  mkdir -p "$1"
  echo 'KERNEL=="kvm", GROUP="kvm", MODE="0666", OPTIONS+="static_node=kvm"' | sudo tee /etc/udev/rules.d/99-kvm4all.rules
  sudo udevadm control --reload-rules
  sudo udevadm trigger --name-match=kvm
  ls -l /dev/kvm
  emulator -accel-check

  local flags=(-avd ci -no-window -no-audio -no-boot-anim -no-snapshot -gpu "${EMULATOR_GPU:-swiftshader_indirect}" -camera-back none -camera-front none -memory 4096 -cores 4)
  local help
  help=$(emulator -help 2>&1 || true)
  if grep -q -- '-no-metrics' <<< "$help"; then
    flags+=(-no-metrics)
  fi
  echo "emulator ${flags[*]}"
  started=$SECONDS
  nohup emulator "${flags[@]}" < /dev/null > "$log" 2>&1 &
  echo $! > "$RUNNER_TEMP/emulator.pid"

  deadline=$((SECONDS + boot_secs))
  until [ "$(adb shell getprop sys.boot_completed 2> /dev/null | tr -d '\r')" = 1 ]; do
    # the launcher starts qemu with the same arguments
    if ! pgrep -f -- '-avd ci' > /dev/null || [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: Android did not boot in $boot_secs s"
      cat "$log"
      return 1
    fi
    sleep 2
  done
  echo "booted in $((SECONDS - started)) s"
  until adb shell pm path android > /dev/null 2>&1; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
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

stop() {
  adb emu kill || true
  if [ -f "$RUNNER_TEMP/emulator.pid" ]; then
    local pid
    pid=$(cat "$RUNNER_TEMP/emulator.pid")
    for _ in $(seq 30); do
      kill -0 "$pid" 2> /dev/null || return 0
      sleep 1
    done
    kill -9 "$pid" 2> /dev/null || true
  fi
}

case "${1:-}" in
  start) start "$2" ;;
  stop) stop ;;
  *)
    echo "usage: $0 start <log dir> | stop"
    exit 1
    ;;
esac
