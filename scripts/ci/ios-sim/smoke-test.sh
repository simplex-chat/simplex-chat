#!/bin/bash

# Installs the app in a new iPhone simulator and runs the scripted UI cases with the ui helper:
# onboarding to the chat list, typing in the search field, and searching the test directory.
# Usage: scripts/ci/ios-sim/smoke-test.sh <path to SimpleX.app> <output dir>
# Each step saves a screenshot and an accessibility tree to the output dir and prints the tree to the log.
# Later steps of the job get SIM_UDID and UI_OUT, with the simulator still running.

set -euo pipefail

app=$1
export UI_OUT=$2
bundle_id=chat.simplex.app
search_text="ci seed"
ui=$(dirname "$0")/ui
mkdir -p "$UI_OUT"

axe --version
runtime=com.apple.CoreSimulator.SimRuntime.iOS-$(xcrun --sdk iphonesimulator --show-sdk-version | tr . -)
SIM_UDID=$(xcrun simctl create ci-iphone com.apple.CoreSimulator.SimDeviceType.iPhone-17 "$runtime")
export SIM_UDID
echo "simulator $SIM_UDID, runtime $runtime"
if [ -n "${GITHUB_ENV:-}" ]; then
  echo "SIM_UDID=$SIM_UDID" >> "$GITHUB_ENV"
  echo "UI_OUT=$UI_OUT" >> "$GITHUB_ENV"
fi
xcrun simctl boot "$SIM_UDID"
xcrun simctl bootstatus "$SIM_UDID" -b
open -a Simulator --args -CurrentDeviceUDID "$SIM_UDID"

xcrun simctl spawn "$SIM_UDID" log stream --level debug --style compact --predicate 'process == "SimpleX"' > "$UI_OUT/app.log" 2>&1 &
log_pid=$!

finish() {
  local status=$?
  "$ui" shot final || true
  kill "$log_pid" || true
  echo "::group::core start in the app log"
  grep -E 'chatMigrateInit|startChat' "$UI_OUT/app.log" || true
  echo "::endgroup::"
  echo "::group::app log, last 300 lines"
  tail -n 300 "$UI_OUT/app.log"
  echo "::endgroup::"
  find ~/Library/Logs/DiagnosticReports -name 'SimpleX*' -newer "$app" 2>/dev/null | while read -r report; do
    echo "::group::crash report $report"
    head -n 200 "$report"
    echo "::endgroup::"
  done || true
  exit "$status"
}
trap finish EXIT

expect_in_tree() {
  local tree
  tree=$("$ui" tree)
  if ! grep -q "$@" <<< "$tree"; then
    echo "error: ${*: -1} is not in the accessibility tree"
    exit 1
  fi
}

xcrun simctl install "$SIM_UDID" "$app"
xcrun simctl launch "$SIM_UDID" "$bundle_id"

# onboarding to the chat list
"$ui" wait "Get started" 180
"$ui" shot first-screen
"$ui" tap "Get started"

"$ui" wait "Create profile" 30
"$ui" tap-value "Enter profile name..."
"$ui" type citester
"$ui" shot profile-name
"$ui" tap "Create profile"

"$ui" wait "Continue" 60
"$ui" shot your-network
"$ui" tap "Continue"

"$ui" wait "Accept" 30
"$ui" shot network-commitments
"$ui" tap "Accept"

sleep 5
"$ui" shot after-onboarding
"$ui" tap "Allow" 20 || echo "no notifications alert to allow"

"$ui" wait "Search or paste SimpleX link" 60
"$ui" shot chat-list

# typed text that is not a SimpleX name offers the directory
"$ui" tap-value "Search or paste SimpleX link"
"$ui" type "$search_text"
sleep 2
"$ui" shot search-typed
# the keyboard capitalizes the first letter
expect_in_tree -iF "\"$search_text\""
expect_in_tree -F '"Search in Directory"'

# the directory finds the seeded group and channel, after the first-search consent
"$ui" tap "Search in Directory"
"$ui" wait "Search in Directory?" 10
"$ui" shot search-consent
"$ui" tap "Search"
if ! "$ui" wait "Join channel" 60; then
  "$ui" shot search-failed
  "$ui" tap "Search failed, tap to retry"
  "$ui" wait "Join channel" 60
fi
"$ui" shot directory-results
# the header alone is matched exactly: "Search in Directory" stays shown above the field
expect_in_tree -F '"Directory"'
expect_in_tree -F '#ci_group'
expect_in_tree -F 'Join group'
expect_in_tree -F '#ci_channel'
expect_in_tree -F 'Join channel'
echo "smoke test passed: the directory search for \"$search_text\" shows #ci_group (Join group) and #ci_channel (Join channel)"
