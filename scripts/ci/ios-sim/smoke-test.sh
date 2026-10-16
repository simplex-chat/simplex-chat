#!/bin/bash

# Installs the app in a new iPhone simulator and runs the scripted UI cases with the ui helper: onboarding to the
# chat list with the one-hand UI (the default), then the chat list again with the top bar. It leaves the app in the one-hand UI.
# With BACKEND_DIR, the work dir of the running test backend, it then searches the test directory, whose results
# must show their avatars and have the Directory header above them in both layouts; then, with a joined channel,
# the chat list's cards must show while typing and hide under the results; and "Show more" must be disabled while
# its page loads (a screenshot shows its colour).
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

# iOS asks once whether the app may send notifications. The tree sometimes shows the app under this alert rather than
# the alert (run 38895987656), so its Allow button is also looked for at its place.
allow_notifications() {
  local deadline=$((SECONDS + 30)) element
  while [ "$SECONDS" -lt "$deadline" ]; do
    if "$ui" tap Allow 1; then
      return
    fi
    element=$("$ui" point 275 518 || true)
    if grep -qF '"Allow"' <<< "$element"; then
      "$ui" tap-xy 275 518
      return
    fi
    sleep 2
  done
  echo "no notifications alert to allow"
}

# a profile without conversations sees the onboarding pages in place of the list
expect_chat_list() {
  expect_in_tree -F '"Talk to someone"'
  expect_in_tree -F 'Button "Let someone connect to you"'
  expect_in_tree -F 'Button "Connect via link or QR code"'
  expect_in_tree -F 'Button "Compose"'
  echo "in the $1 layout the chat list shows the onboarding pages and the toolbar"
}

# the y of the first element with exactly this label
element_y() {
  local tree
  tree=$("$ui" tree)
  grep -F "\"$1\" value=" <<< "$tree" | head -n 1 | sed -E 's/.* at [0-9-]+,([0-9-]+) .*/\1/'
}

expect_header_above_results() {
  local header group channel
  header=$(element_y Directory)
  group=$(element_y "#ci_group")
  channel=$(element_y "#ci_channel")
  if [ -z "$header" ] || [ -z "$group" ] || [ -z "$channel" ] || [ "$header" -ge "$group" ] || [ "$header" -ge "$channel" ]; then
    echo "error: in the $1 layout the Directory header (y=$header) is not above its results (#ci_group y=$group, #ci_channel y=$channel)"
    exit 1
  fi
  echo "in the $1 layout the Directory header (y=$header) is above its results (#ci_group y=$group, #ci_channel y=$channel)"
}

# a result without an avatar shows a placeholder icon labelled with its symbol name; an avatar image has no label
expect_avatars() {
  local tree
  tree=$("$ui" tree)
  if grep -qE '"(person\.2|antenna\.radiowaves\.left\.and\.right)\.circle\.fill"' <<< "$tree"; then
    echo "error: in the $1 layout a directory result shows the placeholder icon, not its avatar"
    exit 1
  fi
  echo "in the $1 layout #ci_group and #ci_channel show their avatars"
}

type_search() {
  "$ui" tap-value "Search or paste SimpleX link"
  "$ui" type "$1"
  sleep 2
}

# the alert asks before every search, as the first search chose "Search", not "Search and don't show again"
search_directory() {
  "$ui" tap "Search in Directory"
  if "$ui" wait "Search in Directory?" 10; then
    "$ui" tap "Search"
  fi
  "$ui" wait "$1" 60
}

# "Cancel" exists only while the field is focused; the keyboard may have capitalized the typed text
clear_search() {
  local text
  text=$("$ui" tree | sed -nE 's/^TextField null value="(.*)" at .*/\1/p' | head -n 1)
  "$ui" tap-value "$text"
  "$ui" tap Cancel
}

# the first launch with more than two chats asks to turn on SimpleX Lock (laNoticeAlert in ContentView.swift)
dismiss_lock_notice() {
  if "$ui" wait "SimpleX Lock" 10; then
    "$ui" tap Cancel
  fi
}

cards=("New 1-time link" "Paste link / Scan" "Bottom bar")
# shown only in the US
banner="Invest in SimpleX Chat!"

shown_cards() {
  local tree label
  tree=$("$ui" tree)
  for label in "${cards[@]}" "$banner"; do
    if grep -qF "\"$label\"" <<< "$tree"; then
      echo "$label"
    fi
  done
}

expect_cards_hidden_by_results() {
  local typing results cleared label
  type_search "$search_text"
  "$ui" shot "cards-typing-$1"
  typing=$(shown_cards)
  for label in "${cards[@]}"; do
    if ! grep -qxF "$label" <<< "$typing"; then
      echo "error: in the $1 layout the chat list does not show \"$label\" while typing"
      exit 1
    fi
  done
  search_directory "#ci_channel"
  "$ui" shot "cards-results-$1"
  results=$(shown_cards)
  if [ -n "$results" ]; then
    echo "error: in the $1 layout the directory results are shown with cards: $(paste -sd ',' - <<< "$results")"
    exit 1
  fi
  expect_header_above_results "$1 (chat list)"
  expect_avatars "$1 (chat list)"
  clear_search
  sleep 2
  "$ui" shot "cards-cleared-$1"
  cleared=$(shown_cards)
  if [ "$cleared" != "$typing" ]; then
    echo "error: in the $1 layout clearing the search brought back \"$(paste -sd ',' - <<< "$cleared")\", not \"$(paste -sd ',' - <<< "$typing")\""
    exit 1
  fi
  echo "in the $1 layout the cards ($(paste -sd ',' - <<< "$typing")) show while typing, are hidden under the results and come back when the search is cleared"
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
allow_notifications
"$ui" shot notifications-allowed

"$ui" wait "Talk to someone" 60
"$ui" shot chat-list
expect_chat_list reachable

# ui layout checks that the toolbar moved
"$ui" layout top
"$ui" shot chat-list-top
expect_chat_list top
"$ui" layout reachable
if [ -z "${BACKEND_DIR:-}" ]; then
  echo "smoke test passed: onboarding reaches the chat list, which shows its onboarding pages and toolbar with the one-hand UI and with the top bar"
  exit 0
fi

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
expect_header_above_results reachable
expect_avatars reachable

"$ui" layout top
type_search "$search_text"
search_directory "Join channel"
"$ui" shot directory-results-top
expect_header_above_results top
expect_avatars top
"$ui" layout reachable

# with a conversation the chat list replaces the onboarding pages, and has cards around the chats
type_search "ci cards"
search_directory "#ci_cards"
"$ui" tap "#ci_cards"
"$ui" tap "Open channel" 30
"$ui" wait "Tap Join channel" 30
"$ui" tap "Join channel"
"$ui" wait "you are subscriber" 60
"$ui" shot joined-channel
"$ui" tap Back
clear_search
expect_cards_hidden_by_results reachable
"$ui" layout top
dismiss_lock_notice
expect_cards_hidden_by_results top
"$ui" layout reachable

# with the directory stopped, the next page waits 10 s before it fails: long enough to see "Show more" while it loads
type_search "ci avatar"
search_directory "Show more"
"$ui" directory stop
"$ui" tap "Show more"
"$ui" shot show-more-loading
expect_in_tree -E '^Button "Show more" .* disabled$'
"$ui" directory start
clear_search
echo "smoke test passed: onboarding reaches the chat list in both layouts; the directory search for \"$search_text\" shows #ci_group (Join group) and #ci_channel (Join channel) with their avatars and the Directory header above them; with a chat, the list's cards show while typing and hide under the results; \"Show more\" is disabled while its page loads"
