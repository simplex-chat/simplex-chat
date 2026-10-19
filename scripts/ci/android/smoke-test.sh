#!/bin/bash

# Installs the APK in the booted emulator (emulator.sh), starts the app and waits for its first onboarding screen;
# with "onboarding" it goes on through onboarding to the chat list.
# Usage: scripts/ci/android/smoke-test.sh <apk> <output dir> [start|onboarding]
# Each step saves a numbered screenshot, the accessibility tree and the text on screen to the output dir; the app's
# logcat is in app.log there. Later steps of the job get UI_OUT, with the emulator still running.

set -euo pipefail

apk=$1
export UI_OUT=$2
cases=${3:-start}
package=chat.simplex.app
ui=$(dirname "$0")/ui
mkdir -p "$UI_OUT"
if [ -n "${GITHUB_ENV:-}" ]; then
  echo "UI_OUT=$UI_OUT" >> "$GITHUB_ENV"
fi

finish() {
  local status=$?
  if [ "$status" -ne 0 ]; then
    "$ui" shot final || true
  fi
  kill "$logcat_pid" 2> /dev/null || true
  echo "::group::the core and crashes in logcat"
  grep -E 'ndk_translation|SIMPLEX *: (start|chat)|FATAL EXCEPTION|Fatal signal|has died|Process: chat.simplex' "$UI_OUT/app.log" | head -n 100 || true
  echo "::endgroup::"
  exit "$status"
}

unzip -l "$apk" | grep -E ' lib/' || true
started=$SECONDS
adb install -r -g "$apk"
echo "installed in $((SECONDS - started)) s"
adb shell pm dump "$package" | grep -E 'primaryCpuAbi|versionName|versionCode' || true

adb logcat -c
adb logcat -v threadtime > "$UI_OUT/app.log" 2>&1 &
logcat_pid=$!
trap finish EXIT

started=$SECONDS
"$ui" launch
"$ui" wait "Get started" 180
echo "first screen after $((SECONDS - started)) s"
"$ui" shot first-screen
adb shell pidof "$package"
if [ "$cases" = start ]; then
  echo "smoke test: the app shows its first screen"
  exit 0
fi

"$ui" click-text "Get started"
"$ui" click-text "Enter profile name" 30
"$ui" type citester
"$ui" wait citester 10
"$ui" shot profile-name
"$ui" click-text "Create profile"

"$ui" wait "Continue" 60
"$ui" shot your-network
"$ui" click-text "Continue"

"$ui" wait "Accept" 30
"$ui" shot network-commitments
"$ui" click-text "Accept"

"$ui" wait "Talk to someone" 60
"$ui" shot chat-list
echo "smoke test: onboarding reached the chat list in $((SECONDS - started)) s"
