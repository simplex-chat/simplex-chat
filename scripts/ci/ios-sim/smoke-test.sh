#!/bin/bash

# Installs the app in a new iPhone simulator and drives it with AXe: onboarding, chat list, typing in the search field.
# Usage: scripts/ci/ios-sim/smoke-test.sh <path to SimpleX.app> <output dir>
# Each step saves a screenshot and an accessibility tree to the output dir and prints the tree to the log.

set -euo pipefail

app=$1
out=$2
bundle_id=chat.simplex.app
search_text="ci smoke"
mkdir -p "$out"

axe --version
runtime=com.apple.CoreSimulator.SimRuntime.iOS-$(xcrun --sdk iphonesimulator --show-sdk-version | tr . -)
udid=$(xcrun simctl create ci-iphone com.apple.CoreSimulator.SimDeviceType.iPhone-17 "$runtime")
echo "simulator $udid, runtime $runtime"
xcrun simctl boot "$udid"
xcrun simctl bootstatus "$udid" -b
open -a Simulator --args -CurrentDeviceUDID "$udid"

xcrun simctl spawn "$udid" log stream --level debug --style compact --predicate 'process == "SimpleX"' > "$out/app.log" 2>&1 &
log_pid=$!

step=0
snap() {
  step=$((step + 1))
  local f
  f=$(printf '%s/%02d-%s' "$out" "$step" "$1")
  xcrun simctl io "$udid" screenshot "$f.png" > /dev/null
  axe describe-ui --udid "$udid" > "$f.json"
  echo "::group::accessibility tree: $1"
  cat "$f.json"
  echo "::endgroup::"
}

wait_for() {
  local deadline=$((SECONDS + $2))
  local tree="$out/wait.json"
  until axe describe-ui --udid "$udid" > "$tree" && grep -qF "$1" "$tree"; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: \"$1\" not shown after $2 s"
      return 1
    fi
    sleep 2
  done
}

tap_label() {
  axe tap --label "$1" --wait-timeout "$2" --post-delay 2 --udid "$udid"
}

# SwiftUI text fields have no accessibility label; an empty field exposes its placeholder as the value
tap_field() {
  axe tap --value "$1" --wait-timeout "$2" --post-delay 2 --udid "$udid"
}

finish() {
  local status=$?
  snap final || true
  kill "$log_pid" || true
  echo "::group::core start in the app log"
  grep -E 'chatMigrateInit|startChat' "$out/app.log" || true
  echo "::endgroup::"
  echo "::group::app log, last 300 lines"
  tail -n 300 "$out/app.log"
  echo "::endgroup::"
  find ~/Library/Logs/DiagnosticReports -name 'SimpleX*' -newer "$app" 2>/dev/null | while read -r report; do
    echo "::group::crash report $report"
    head -n 200 "$report"
    echo "::endgroup::"
  done || true
  exit "$status"
}
trap finish EXIT

xcrun simctl install "$udid" "$app"
xcrun simctl launch "$udid" "$bundle_id"

wait_for "Get started" 180
snap first-screen
tap_label "Get started" 10

wait_for "Create profile" 30
tap_field "Enter profile name..." 10
axe type 'citester' --udid "$udid"
snap profile-name
tap_label "Create profile" 10

wait_for "Continue" 60
snap your-network
tap_label "Continue" 10

wait_for "Accept" 30
snap network-commitments
tap_label "Accept" 10

sleep 5
snap after-onboarding
tap_label "Allow" 20 || echo "no notifications alert to allow"

wait_for "Search or paste SimpleX link" 60
snap chat-list
tap_field "Search or paste SimpleX link" 10
axe type "$search_text" --udid "$udid"
sleep 2
snap search-typed

# the keyboard capitalizes the first letter
tree=$(ls "$out"/*-search-typed.json)
if ! grep -qiF "$search_text" "$tree"; then
  echo "error: \"$search_text\" is not in the accessibility tree after typing"
  exit 1
fi
if ! grep -qF "Search in Directory" "$tree"; then
  echo "error: no \"Search in Directory\" row after typing"
  exit 1
fi
echo "smoke test passed: the chat list search field shows \"$search_text\""
