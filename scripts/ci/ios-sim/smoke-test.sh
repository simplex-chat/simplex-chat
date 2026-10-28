#!/bin/bash

# Installs the app in a new iPhone simulator and runs the scripted UI cases with the ui helper: onboarding to the
# chat list with the one-hand UI (the default), then the chat list again with the top bar. It leaves the app in the one-hand UI.
# For an app in the derived data of a build, it then renders the previews of the SimpleX Snapshots tests to the snapshots
# folder of the output dir (snapshots.sh).
# With BACKEND_DIR, the work dir of the running test backend, it then relaunches the app and searches the test directory: the first search
# asks to enable Directory search (Cancel sends nothing, Enable searches), and no later search, the Search key's included,
# asks again; the results show Open and their avatars, with no header, in both layouts; Open prepares a channel and
# opens it without an alert, with its subscriber count; once joined, the channel is its chat row among the results, on
# the chat list in both layouts and in New chat; the chat list's cards show while typing and hide under the results;
# a text that no chat and no listing matches offers the directory, and "No chats found" shows only once it found nothing;
# a page whose only result is that channel adds nothing; the load row says how many results are left, is disabled while
# its page loads (a screenshot shows its colour) and offers a retry when that fails; and a long press on a result
# shows a menu with Open (not fatal: the gesture is new to this harness; a screenshot shows it).
# Usage: scripts/ci/ios-sim/smoke-test.sh <path to SimpleX.app> <output dir>
# Each step saves a screenshot and an accessibility tree to the output dir and prints the tree to the log.
# Later steps of the job get SIM_UDID and UI_OUT, with the simulator still running.

set -euo pipefail

app=$1
export UI_OUT=$2
bundle_id=chat.simplex.app
search_text="ci seed"
# matches no chat and no listing
no_match_text=zqxvjk
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

# the first element whose label contains the text
labelled_containing() {
  local tree
  tree=$("$ui" tree)
  grep -F -- "$1" <<< "$tree" | head -n 1 || true
}

# The "Open" on the row of the result labelled exactly $1, or nothing: the row's texts are stacked beside its avatar
# and Open is centred on the row, so Open's top is within a row's height of the name's top.
open_in_row() {
  local tree name top line y
  tree=$("$ui" tree)
  name=$(grep -F "\"$1\" value=" <<< "$tree" | head -n 1 || true)
  if [ -z "$name" ]; then
    return 0
  fi
  top=$(sed -E 's/.* at [0-9-]+,([0-9-]+) .*/\1/' <<< "$name")
  while IFS= read -r line; do
    y=$(sed -E 's/.* at [0-9-]+,([0-9-]+) .*/\1/' <<< "$line")
    if [ "$y" -ge $((top - 10)) ] && [ "$y" -le $((top + 60)) ]; then
      echo "$line"
      return 0
    fi
  done < <(grep -F '"Open" value=' <<< "$tree" || true)
}

# #ci_group and #ci_channel each with Open on its row and its avatar; no header, and no "Search in Directory" while results show
expect_seed_results() {
  local tree name
  for name in "#ci_group" "#ci_channel"; do
    if [ -z "$(open_in_row "$name")" ]; then
      echo "error: in the $1 layout $name is not a result with Open on its row"
      exit 1
    fi
  done
  tree=$("$ui" tree)
  # matched exactly: "Search in Directory" also contains the word
  if grep -qF '"Directory" value=' <<< "$tree"; then
    echo "error: in the $1 layout the results have a Directory header"
    exit 1
  fi
  if grep -qF '"Search in Directory"' <<< "$tree"; then
    echo "error: in the $1 layout \"Search in Directory\" is shown while the results are"
    exit 1
  fi
  expect_avatars "$1"
  echo "in the $1 layout #ci_group and #ci_channel are results with Open, with no header and no \"Search in Directory\""
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

# the first search turned Directory search on, so no later one may ask to enable it
expect_no_consent() {
  local tree
  tree=$("$ui" tree)
  if grep -qF "Enable Directory search?" <<< "$tree"; then
    "$ui" shot consent-asked-again
    echo "error: $1 asked to enable Directory search again"
    exit 1
  fi
}

tap_search_in_directory() {
  "$ui" tap "Search in Directory"
  expect_no_consent "\"Search in Directory\""
}

search_directory() {
  tap_search_in_directory
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
  expect_seed_results "$1 (chat list)"
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

# with a chat in the list, a text that matches no chat offers the directory; "No chats found" waits for its search to find nothing
expect_no_chats_found_after_search() {
  local tree retry
  type_search "$no_match_text"
  "$ui" shot no-match-typed
  tree=$("$ui" tree)
  if ! grep -qF '"Search in Directory"' <<< "$tree" || grep -qF '"No chats found"' <<< "$tree"; then
    echo "error: for \"$no_match_text\" the chat list doesn't offer \"Search in Directory\", or shows \"No chats found\" before searching"
    exit 1
  fi
  tap_search_in_directory
  if ! "$ui" wait '"No chats found"' 30; then
    retry=$(labelled_containing "Search failed, tap to retry")
    if [ -z "$retry" ]; then
      "$ui" shot no-match-searched-missing
      echo "error: \"No chats found\" not shown after the directory search for \"$no_match_text\" found nothing"
      exit 1
    fi
    tap_element "$retry"
    "$ui" wait '"No chats found"' 30
  fi
  "$ui" shot no-match-searched
  clear_search
  echo "for \"$no_match_text\" the chat list offers \"Search in Directory\" without \"No chats found\", which shows once the directory found nothing"
}

# ci_cards is the only result for "ci cards": once joined, it must be its chat row, not a result to open
expect_no_join_row() {
  local tree
  tree=$("$ui" tree)
  if grep -qF '"#ci_cards"' <<< "$tree" || grep -qF '"Open" value=' <<< "$tree"; then
    echo "error: in $1 the joined ci_cards is a result row (#ci_cards, Open), not its chat"
    exit 1
  fi
}

# with the space the local filter does not match ci_cards, so the list does not show it, and the directory finds it by its description
open_known_chat_from_results() {
  type_search "ci cards"
  search_directory "ci_cards"
  "$ui" shot "known-chat-$1"
  expect_in_tree -F 'StaticText "ci_cards"'
  expect_no_join_row "the $1 layout (chat list)"
  "$ui" tap ci_cards
  "$ui" wait "you are subscriber" 30
  "$ui" shot "known-chat-opened-$1"
  "$ui" tap Back
  clear_search
  echo "in the $1 layout the directory result for the joined ci_cards is its chat row, which opens the channel"
}

# the elements labelled exactly $1, one per line
labelled() {
  "$ui" tree | grep -F "\"$1\" value=" || true
}

# the elements labelled $1 that are not among the lines $2
added_labelled() {
  local now
  now=$(labelled "$1")
  if [ -z "$2" ]; then
    printf '%s' "$now"
  else
    grep -vxF -f <(printf '%s\n' "$2") <<< "$now" || true
  fi
}

# the topmost of these tree lines
topmost() {
  sed -E 's/.* at [0-9-]+,([0-9-]+) .*/\1 &/' <<< "$1" | sort -n | head -n 1 | cut -d ' ' -f 2-
}

# taps the centre of the element on this tree line, or long-presses it with long-press
tap_element() {
  local x y w h
  read -r x y w h <<< "$(sed -nE 's/.* at ([0-9]+),([0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/p' <<< "$1")"
  if [ -z "$h" ]; then
    echo "error: no on-screen element to tap in: $1"
    exit 1
  fi
  "$ui" "${2:-tap-xy}" $((x + w / 2)) $((y + h / 2))
}

# New chat lists only contacts, so there the seeded results are rows with Open and the joined channel is a contact-style
# chat row. The chat list behind the sheet may be in the tree too, with its own search field (at the bottom with the
# one-hand UI) and ci_cards row, so the sheet's field is the topmost one and its row is the ci_cards element the search adds.
search_in_new_chat() {
  local before row deadline tree
  before=$(labelled ci_cards)
  "$ui" tap Compose
  "$ui" wait "Create 1-time link" 10
  tap_element "$(topmost "$("$ui" tree | grep -F 'TextField null value="Search or paste SimpleX link"')")"
  "$ui" type "$search_text"
  sleep 2
  search_directory '"#ci_group"'
  "$ui" shot new-chat-results
  if [ -z "$(open_in_row "#ci_group")" ]; then
    echo "error: in New chat #ci_group is not a result with Open on its row"
    exit 1
  fi
  tap_element "$(topmost "$("$ui" tree | grep -F 'TextField null value=')")"
  "$ui" tap Cancel
  # the profile has no contacts, so New chat's empty state is "No contacts"
  tap_element "$(topmost "$("$ui" tree | grep -F 'TextField null value="Search or paste SimpleX link"')")"
  "$ui" type "$no_match_text"
  sleep 2
  "$ui" shot new-chat-no-match-typed
  tree=$("$ui" tree)
  if ! grep -qF '"Search in Directory"' <<< "$tree" || grep -qF -e '"No contacts"' -e '"No filtered contacts"' <<< "$tree"; then
    echo "error: for \"$no_match_text\" New chat doesn't offer \"Search in Directory\", or shows its empty state before searching"
    exit 1
  fi
  tap_search_in_directory
  "$ui" wait '"No contacts"' 30
  "$ui" shot new-chat-no-match-searched
  tap_element "$(topmost "$("$ui" tree | grep -F 'TextField null value=')")"
  "$ui" tap Cancel
  tap_element "$(topmost "$("$ui" tree | grep -F 'TextField null value="Search or paste SimpleX link"')")"
  "$ui" type "ci cards"
  sleep 2
  tap_search_in_directory
  deadline=$((SECONDS + 60))
  until row=$(added_labelled ci_cards "$before") && [ -n "$row" ]; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      "$ui" shot known-chat-new-chat-missing
      echo "error: New chat shows no ci_cards row after searching the directory for \"ci cards\""
      exit 1
    fi
    sleep 2
  done
  "$ui" shot known-chat-new-chat
  expect_no_join_row "New chat"
  tap_element "$row"
  "$ui" wait "you are subscriber" 30
  "$ui" shot known-chat-opened-new-chat
  "$ui" tap Back
  echo "in New chat #ci_group is a result with Open, \"No contacts\" shows only once the search for \"$no_match_text\" found nothing, and the directory result for the joined ci_cards is its chat row ($row), which closes the sheet and opens the channel"
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

products=$(dirname "$app")
if [[ "$products" == */Build/Products/Debug-iphonesimulator ]]; then
  "$(dirname "$0")/snapshots.sh" "${products%/Build/Products/Debug-iphonesimulator}" "$UI_OUT/snapshots"
  if "$ui" wait "would like to access the Camera" 10; then
    "$ui" tap "Don’t Allow"
  fi
fi

if [ -z "${BACKEND_DIR:-}" ]; then
  echo "smoke test passed: onboarding reaches the chat list, which shows its onboarding pages and toolbar with the one-hand UI and with the top bar"
  exit 0
fi

# private routing to unknown servers only: the preset servers that would proxy name lookups can't reach the test SMP
# server on localhost
xcrun simctl terminate "$SIM_UDID" "$bundle_id" 2> /dev/null || true
prefs=$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" group.chat.simplex.app)/Library/Preferences/group.chat.simplex.app
xcrun simctl spawn "$SIM_UDID" defaults write "$prefs" networkSMPProxyMode -string unknown
"$ui" relaunch

# typed text that is not a SimpleX name offers the directory
"$ui" tap-value "Search or paste SimpleX link"
"$ui" type "$search_text"
sleep 2
"$ui" shot search-typed
# the keyboard capitalizes the first letter
expect_in_tree -iF "\"$search_text\""
expect_in_tree -F '"Search in Directory"'

# Directory search is off until enabled: the first search asks, and Cancel sends nothing
"$ui" tap "Search in Directory"
"$ui" wait "Enable Directory search?" 10
"$ui" shot search-consent
"$ui" tap Cancel
sleep 2
"$ui" shot search-cancelled
tree=$("$ui" tree)
if grep -qF '"Open" value=' <<< "$tree" || ! grep -qF '"Search in Directory"' <<< "$tree"; then
  echo "error: Cancel in \"Enable Directory search?\" searched, or \"Search in Directory\" is gone"
  exit 1
fi
echo "Cancel in \"Enable Directory search?\" searched nothing, and \"Search in Directory\" is still shown"

# Enable turns Directory search on and searches: the directory finds the seeded group and channel
"$ui" tap "Search in Directory"
"$ui" wait "Enable Directory search?" 10
"$ui" tap Enable
if ! "$ui" wait '"Open"' 60; then
  "$ui" shot search-failed
  tap_element "$(labelled_containing "Search failed, tap to retry")"
  "$ui" wait '"Open"' 60
fi
"$ui" shot directory-results
expect_seed_results reachable

# the Search key searches the directory too, and never resolves names: "Connect to …" stays for the name typed
clear_search
type_search cinews
"$ui" key return
expect_no_consent "The Search key"
"$ui" wait '"#cinews"' 60
"$ui" shot search-key-results
# the keyboard capitalizes the first letter
expect_in_tree -iF '"Connect to cinews.testing"'
tree=$("$ui" tree)
if grep -qF '"Names"' <<< "$tree"; then
  echo "error: the Search key shows a Names part"
  exit 1
fi
echo "the Search key shows the directory's results for cinews, with \"Connect to cinews.testing\" and no Names part"
clear_search

"$ui" layout top
type_search "$search_text"
search_directory '"Open"'
"$ui" shot directory-results-top
expect_seed_results top
"$ui" layout reachable

# Open prepares the channel and opens it without asking, so no tree read until it opens may show the "Open channel" alert;
# with a conversation the chat list then replaces the onboarding pages, and has cards around the chats
type_search "ci cards"
search_directory '"#ci_cards"'
"$ui" shot ci-cards-result
open=$(open_in_row "#ci_cards")
if [ -z "$open" ]; then
  echo "error: #ci_cards is not a result with Open on its row"
  exit 1
fi
tap_element "$open"
deadline=$((SECONDS + 60))
until tree=$("$ui" tree) && grep -qF "Tap Join channel" <<< "$tree"; do
  if grep -qF 'Button "Open channel"' <<< "$tree"; then
    "$ui" shot open-channel-alert
    echo "error: Open on #ci_cards showed the \"Open channel\" alert"
    exit 1
  fi
  if [ "$SECONDS" -ge "$deadline" ]; then
    "$ui" shot prepared-channel-missing
    echo "error: Open on #ci_cards did not open the channel with \"Tap Join channel\" after 60 s"
    exit 1
  fi
  sleep 1
done
"$ui" shot prepared-channel
# the toolbar and the banner
subscribers=$(grep -cE '[0-9]+ subscribers?' <<< "$tree" || true)
if [ "$subscribers" -lt 2 ]; then
  echo "error: the prepared channel shows its subscriber count $subscribers times, not in both the toolbar and the banner"
  exit 1
fi
echo "Open on #ci_cards opened the prepared channel without an alert, with its subscriber count in the toolbar and the banner"
"$ui" tap "Join channel"
"$ui" wait "you are subscriber" 60
"$ui" shot joined-channel
"$ui" tap Back
# "ci cards" doesn't match the chat's name, so the list doesn't show it: the results show it as its chat row
"$ui" shot joined-channel-search
expect_in_tree -F 'StaticText "ci_cards"'
expect_no_join_row "the chat list after joining"
echo "after joining through Open, ci_cards is its chat row among the results and no longer a result to open"
clear_search
# the page's only result is that channel, which the list shows, so the search adds no row and no note; nothing marks
# its end, so the step waits it out: once the directory's link is fetched, the request fails after 10 s
type_search "ci_cards"
tap_search_in_directory
sleep 20
retry=$(labelled_containing "Search failed, tap to retry")
if [ -n "$retry" ]; then
  tap_element "$retry"
  sleep 20
fi
"$ui" shot page-in-chats
expect_in_tree -F 'StaticText "ci_cards"'
expect_no_join_row "the chat list listing ci_cards"
tree=$("$ui" tree)
# "Search in Directory" hides once a search for the text runs
if grep -qF -e '"Search in Directory"' -e "All results on this page" -e "Search failed, tap to retry" <<< "$tree"; then
  echo "error: the search whose only result is the listed ci_cards didn't run, failed or shows a note"
  exit 1
fi
echo "the search whose only result is the listed ci_cards adds no row and no note"
clear_search
expect_no_chats_found_after_search
expect_cards_hidden_by_results reachable
open_known_chat_from_results reachable
"$ui" layout top
dismiss_lock_notice
expect_cards_hidden_by_results top
open_known_chat_from_results top
"$ui" layout reachable
search_in_new_chat

# with the directory stopped, the next page waits 10 s before it fails: long enough to see the load row while it loads
type_search "ci avatar"
search_directory "Load more"
"$ui" shot load-more
expect_in_tree -E 'Load more · [0-9]+ left'
loaded=$("$ui" tree | grep -cF '"#ci_avatar_' || true)
"$ui" directory stop
tap_element "$(labelled_containing "Load more")"
# read before the screenshot, which takes long enough for the 10 s request to fail
expect_in_tree -E '^Button ".*Load more.*" .* disabled$'
"$ui" shot load-more-loading
"$ui" wait "Search failed, tap to retry" 30
"$ui" shot load-more-failed
"$ui" directory start
# the directory takes a while to accept requests again, so the retry is repeated until the next page comes
deadline=$((SECONDS + 120))
until [ "$("$ui" tree | grep -cF '"#ci_avatar_' || true)" -gt "$loaded" ]; do
  if [ "$SECONDS" -ge "$deadline" ]; then
    "$ui" shot load-more-retry-failed
    echo "error: retrying the failed page did not load more results after 120 s"
    exit 1
  fi
  retry=$(labelled_containing "Search failed, tap to retry")
  if [ -n "$retry" ]; then
    tap_element "$retry"
  fi
  sleep 5
done
"$ui" shot load-more-retried
echo "the load row says how many results are left, is disabled while its page loads, offers a retry when it fails, and the retry loads the page"
clear_search

# not fatal: the long press is new to this harness, and the screenshot shows the menu's preview over the flipped list
type_search "$search_text"
search_directory '"#ci_group"'
opens=$(labelled Open)
tap_element "$(labelled_containing '"#ci_group" value=')" long-press
"$ui" shot long-press-menu
if [ -n "$(added_labelled Open "$opens")" ]; then
  echo "a long press on #ci_group shows a menu with Open"
  "$ui" tap-xy 200 100
else
  echo "WARN: a long press on #ci_group shows no menu with Open; see the long-press-menu screenshot"
fi
clear_search || true
echo "smoke test passed: onboarding reaches the chat list in both layouts; the first directory search asks to enable Directory search, Cancel sends nothing and Enable searches, and no later search asks; the search for \"$search_text\" shows #ci_group and #ci_channel with Open and their avatars, with no header; the Search key searches the directory and keeps \"Connect to …\"; Open opens a prepared channel without an alert, with its subscriber count; with a chat, the list's cards show while typing and hide under the results, and \"No chats found\" waits for a directory search that finds nothing; a page of chats already listed adds nothing; the joined channel found by its description is its chat row on the chat list in both layouts and in New chat, and opens it; the load row says how many results are left, is disabled while its page loads and retries a failed page"
