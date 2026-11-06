#!/bin/bash

# Installs the app in a new iPhone simulator and runs the scripted UI cases with the ui helper: onboarding to the
# chat list with the one-hand UI (the default), then the chat list again with the top bar. It leaves the app in the one-hand UI.
# For an app in the derived data of a build, it then renders the previews of the SimpleX Snapshots tests to the snapshots
# folder of the output dir (snapshots.sh).
# With "images" it then turns off the encryption of local files, creates a group and sends tests/fixtures/test.jpg and
# logo-large-rgba.png picked in the photo library, and checks the pixel sizes of the sent files.
# Usage: scripts/ci/ios-sim/smoke-test.sh <path to SimpleX.app> <output dir> [images]
# Each step saves a screenshot and an accessibility tree to the output dir and prints the tree to the log.
# Later steps of the job get SIM_UDID and UI_OUT, with the simulator still running.

set -euo pipefail

app=$1
export UI_OUT=$2
cases=${3:-}
bundle_id=chat.simplex.app
search_text="ci seed"
no_match_text=zqxvjk
text_only_in_ci_cards_description="ci cards"
ui=$(dirname "$0")/ui
fixtures=$(cd "$(dirname "$0")/../../../tests/fixtures" && pwd)
test_card_x=66
logo_x=200
photo_y=412
picker_done_x=363
picker_done_y=144
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

labelled_containing() {
  local tree
  tree=$("$ui" tree)
  grep -F -- "$1" <<< "$tree" | head -n 1 || true
}

retry_failed_search() {
  local retry
  retry=$(labelled_containing "Search failed, tap to retry")
  [ -n "$retry" ] || return 1
  tap_line "$retry"
}

open_in_row() {
  local label=$1 tree name top line y
  tree=$("$ui" tree)
  name=$(grep -F "\"$label\" value=" <<< "$tree" | head -n 1 || true)
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

expect_seed_results() {
  local tree name
  for name in "#ci_group" "#ci_channel"; do
    if [ -z "$(open_in_row "$name")" ]; then
      fail "in the $1 layout $name is not a result with Open on its row"
    fi
  done
  tree=$("$ui" tree)
  if grep -qF '"Directory" value=' <<< "$tree"; then
    fail "in the $1 layout the results have a Directory header"
  fi
  if grep -qF '"Search in Directory"' <<< "$tree"; then
    fail "in the $1 layout \"Search in Directory\" is shown while the results are"
  fi
  expect_avatars "$1"
  echo "in the $1 layout #ci_group and #ci_channel are results with Open, with no header and no \"Search in Directory\""
}

expect_avatars() {
  local tree placeholder_icons='"(person\.2|antenna\.radiowaves\.left\.and\.right)\.circle\.fill"'
  tree=$("$ui" tree)
  if grep -qE "$placeholder_icons" <<< "$tree"; then
    fail "in the $1 layout a directory result shows the placeholder icon, not its avatar"
  fi
  echo "in the $1 layout #ci_group and #ci_channel show their avatars"
}

type_search() {
  "$ui" tap-value "Search or paste SimpleX link"
  "$ui" type "$1"
  sleep 2
}

expect_no_consent() {
  local tree
  tree=$("$ui" tree)
  if grep -qF "Enable Directory search?" <<< "$tree"; then
    "$ui" shot consent-shown-again
    fail "$1 showed the \"Enable Directory search?\" alert again"
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

clear_search() {
  local text
  text=$("$ui" tree | sed -nE 's/^TextField null value="(.*)" at .*/\1/p' | head -n 1)
  "$ui" tap-value "$text"
  "$ui" tap Cancel
}

focus_new_chat_search() {
  tap_line "$(topmost "$("$ui" tree | grep -F 'TextField null value="Search or paste SimpleX link"')")"
}

new_chat_type_search() {
  focus_new_chat_search
  "$ui" type "$1"
  sleep 2
}

clear_new_chat_search() {
  tap_line "$(topmost "$("$ui" tree | grep -F 'TextField null value=')")"
  "$ui" tap Cancel
}

dismiss_simplex_lock_notice() {
  if "$ui" wait "SimpleX Lock" 10; then
    "$ui" tap Cancel
  fi
}

cards=("New 1-time link" "Paste link / Scan" "Bottom bar")
us_only_banner="Invest in SimpleX Chat!"

shown_cards() {
  local tree label
  tree=$("$ui" tree)
  for label in "${cards[@]}" "$us_only_banner"; do
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
      fail "in the $1 layout the chat list does not show \"$label\" while typing"
    fi
  done
  search_directory "#ci_channel"
  "$ui" shot "cards-results-$1"
  results=$(shown_cards)
  if [ -n "$results" ]; then
    fail "in the $1 layout the directory results are shown with cards: $(paste -sd ',' - <<< "$results")"
  fi
  expect_seed_results "$1 (chat list)"
  clear_search
  sleep 2
  "$ui" shot "cards-cleared-$1"
  cleared=$(shown_cards)
  if [ "$cleared" != "$typing" ]; then
    fail "in the $1 layout clearing the search brought back \"$(paste -sd ',' - <<< "$cleared")\", not \"$(paste -sd ',' - <<< "$typing")\""
  fi
  echo "in the $1 layout the cards ($(paste -sd ',' - <<< "$typing")) show while typing, are hidden under the results and come back when the search is cleared"
}

expect_no_chats_found_after_search() {
  local tree
  type_search "$no_match_text"
  "$ui" shot no-match-typed
  tree=$("$ui" tree)
  if ! grep -qF '"Search in Directory"' <<< "$tree" || grep -qF '"No chats found"' <<< "$tree"; then
    fail "for \"$no_match_text\" the chat list doesn't offer \"Search in Directory\", or shows \"No chats found\" before searching"
  fi
  tap_search_in_directory
  if ! "$ui" wait '"No chats found"' 30; then
    if ! retry_failed_search; then
      "$ui" shot no-match-searched-missing
      fail "\"No chats found\" not shown after the directory search for \"$no_match_text\" found nothing"
    fi
    "$ui" wait '"No chats found"' 30
  fi
  "$ui" shot no-match-searched
  clear_search
  echo "for \"$no_match_text\" the chat list offers \"Search in Directory\" without \"No chats found\", which shows once the directory found nothing"
}

expect_ci_cards_is_not_a_result() {
  local tree
  tree=$("$ui" tree)
  if grep -qF '"#ci_cards"' <<< "$tree" || grep -qF '"Open" value=' <<< "$tree"; then
    fail "in $1 the joined ci_cards is a result row (#ci_cards, Open), not its chat"
  fi
}

open_known_chat_from_results() {
  type_search "$text_only_in_ci_cards_description"
  search_directory "ci_cards"
  "$ui" shot "known-chat-$1"
  expect_in_tree -F 'StaticText "ci_cards"'
  expect_ci_cards_is_not_a_result "the $1 layout (chat list)"
  "$ui" tap ci_cards
  "$ui" wait "you are subscriber" 30
  "$ui" shot "known-chat-opened-$1"
  "$ui" tap Back
  clear_search
  echo "in the $1 layout the directory result for the joined ci_cards is its chat row, which opens the channel"
}

labelled() {
  local label=$1
  "$ui" tree | grep -F "\"$label\" value=" || true
}

added_labelled() {
  local label=$1 before=$2 now
  now=$(labelled "$label")
  if [ -z "$before" ]; then
    printf '%s' "$now"
  else
    grep -vxF -f <(printf '%s\n' "$before") <<< "$now" || true
  fi
}

topmost() {
  local lines=$1
  sed -E 's/.* at [0-9-]+,([0-9-]+) .*/\1 &/' <<< "$lines" | sort -n | head -n 1 | cut -d ' ' -f 2-
}

tap_line() {
  local line=$1 gesture=${2:-tap-xy} x y w h
  read -r x y w h <<< "$(sed -nE 's/.* at ([0-9]+),([0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/p' <<< "$line")"
  if [ -z "$h" ]; then
    fail "no on-screen element to tap in: $line"
  fi
  "$ui" "$gesture" $((x + w / 2)) $((y + h / 2))
}

new_chat_results() {
  new_chat_type_search "$search_text"
  search_directory '"#ci_group"'
  "$ui" shot new-chat-results
  if [ -z "$(open_in_row "#ci_group")" ]; then
    fail "in New chat #ci_group is not a result with Open on its row"
  fi
  echo "in New chat #ci_group is a result with Open"
}

new_chat_no_match() {
  local deadline tree
  new_chat_type_search "$no_match_text"
  "$ui" shot new-chat-no-match-typed
  tree=$("$ui" tree)
  if ! grep -qF '"Search in Directory"' <<< "$tree" || grep -qF -e '"No contacts"' -e '"No filtered contacts"' <<< "$tree"; then
    fail "for \"$no_match_text\" New chat doesn't offer \"Search in Directory\", or shows its empty state before searching"
  fi
  tap_search_in_directory
  deadline=$((SECONDS + 30))
  until tree=$("$ui" tree) && grep -qF -e '"No contacts"' -e '"No filtered contacts"' <<< "$tree"; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      "$ui" shot new-chat-no-match-missing
      fail "New chat's empty state not shown 30 s after the search for \"$no_match_text\" found nothing"
    fi
    sleep 1
  done
  "$ui" shot new-chat-no-match-searched
  echo "in New chat the empty state shows only once the search for \"$no_match_text\" found nothing"
}

new_chat_known_chat() {
  local before=$1 row deadline
  new_chat_type_search "$text_only_in_ci_cards_description"
  tap_search_in_directory
  deadline=$((SECONDS + 60))
  until row=$(added_labelled ci_cards "$before") && [ -n "$row" ]; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      "$ui" shot known-chat-new-chat-missing
      fail "New chat shows no ci_cards row after searching the directory for \"$text_only_in_ci_cards_description\""
    fi
    sleep 2
  done
  "$ui" shot known-chat-new-chat
  expect_ci_cards_is_not_a_result "New chat"
  tap_line "$row"
  "$ui" wait "you are subscriber" 30
  "$ui" shot known-chat-opened-new-chat
  "$ui" tap Back
  echo "in New chat the directory result for the joined ci_cards is its chat row ($row), which closes the sheet and opens the channel"
}

search_in_new_chat() {
  local before
  before=$(labelled ci_cards)
  "$ui" tap Compose
  "$ui" wait "Create 1-time link" 10
  new_chat_results
  clear_new_chat_search
  new_chat_no_match
  clear_new_chat_search
  new_chat_known_chat "$before"
}

route_privately_to_unknown_servers() {
  local prefs
  xcrun simctl terminate "$SIM_UDID" "$bundle_id" 2> /dev/null || true
  prefs=$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" group.chat.simplex.app)/Library/Preferences/group.chat.simplex.app
  xcrun simctl spawn "$SIM_UDID" defaults write "$prefs" networkSMPProxyMode -string unknown
  "$ui" relaunch
}

typed_text_offers_directory() {
  type_search "$search_text"
  "$ui" shot search-typed
  expect_in_tree -iF "\"$search_text\""
  expect_in_tree -F '"Search in Directory"'
}

cancel_consent_searches_nothing() {
  local tree
  "$ui" tap "Search in Directory"
  "$ui" wait "Enable Directory search?" 10
  "$ui" shot search-consent
  "$ui" tap Cancel
  sleep 2
  "$ui" shot search-cancelled
  tree=$("$ui" tree)
  if grep -qF '"Open" value=' <<< "$tree" || ! grep -qF '"Search in Directory"' <<< "$tree"; then
    fail "Cancel in \"Enable Directory search?\" searched, or \"Search in Directory\" is gone"
  fi
  echo "Cancel in \"Enable Directory search?\" searched nothing, and \"Search in Directory\" is still shown"
}

enable_and_search() {
  "$ui" tap "Search in Directory"
  "$ui" wait "Enable Directory search?" 10
  "$ui" tap Enable
  if ! "$ui" wait '"Open"' 60; then
    "$ui" shot search-failed
    retry_failed_search || fail "no \"Open\" and no \"Search failed, tap to retry\" after Enable"
    "$ui" wait '"Open"' 60
  fi
  "$ui" shot directory-results
  expect_seed_results reachable
  clear_search
}

search_key_keeps_connect_to() {
  local tree
  type_search cinews
  "$ui" key return
  expect_no_consent "The Search key"
  "$ui" wait '"#cinews"' 60
  "$ui" shot search-key-results
  expect_in_tree -iF '"Connect to cinews.testing"'
  tree=$("$ui" tree)
  if grep -qF '"Names"' <<< "$tree"; then
    fail "the Search key shows a Names part"
  fi
  echo "the Search key shows the directory's results for cinews, with \"Connect to cinews.testing\" and no Names part"
  clear_search
}

seed_results_in_top_layout() {
  "$ui" layout top
  type_search "$search_text"
  search_directory '"Open"'
  "$ui" shot directory-results-top
  expect_seed_results top
  "$ui" layout reachable
}

open_and_join_ci_cards() {
  local open deadline tree subscribers
  type_search "$text_only_in_ci_cards_description"
  search_directory '"#ci_cards"'
  "$ui" shot ci-cards-result
  open=$(open_in_row "#ci_cards")
  if [ -z "$open" ]; then
    fail "#ci_cards is not a result with Open on its row"
  fi
  tap_line "$open"
  deadline=$((SECONDS + 60))
  until tree=$("$ui" tree) && grep -qF "Tap Join channel" <<< "$tree"; do
    if grep -qF 'Button "Open channel"' <<< "$tree"; then
      "$ui" shot open-channel-alert
      fail "Open on #ci_cards showed the \"Open channel\" alert"
    fi
    if [ "$SECONDS" -ge "$deadline" ]; then
      "$ui" shot prepared-channel-missing
      fail "Open on #ci_cards did not open the channel with \"Tap Join channel\" after 60 s"
    fi
    sleep 1
  done
  "$ui" shot prepared-channel
  subscribers=$(grep -cE '[0-9]+ subscribers?' <<< "$tree" || true)
  if [ "$subscribers" -lt 2 ]; then
    fail "the prepared channel shows its subscriber count $subscribers times, not in both the toolbar and the banner"
  fi
  echo "Open on #ci_cards opened the prepared channel without an alert, with its subscriber count in the toolbar and the banner"
  "$ui" tap "Join channel"
  "$ui" wait "you are subscriber" 60
  "$ui" shot joined-channel
  "$ui" tap Back
  "$ui" shot joined-channel-search
  expect_in_tree -F 'StaticText "ci_cards"'
  expect_ci_cards_is_not_a_result "the chat list after joining"
  echo "after joining through Open, ci_cards is its chat row among the results and no longer a result to open"
  clear_search
}

search_listed_chat_adds_nothing() {
  local tree
  type_search "ci_cards"
  tap_search_in_directory
  sleep 20
  if retry_failed_search; then
    sleep 20
  fi
  "$ui" shot page-in-chats
  expect_in_tree -F 'StaticText "ci_cards"'
  expect_ci_cards_is_not_a_result "the chat list listing ci_cards"
  tree=$("$ui" tree)
  if grep -qF -e '"Search in Directory"' -e "All results on this page" -e "Search failed, tap to retry" <<< "$tree"; then
    fail "the search whose only result is the listed ci_cards didn't run, failed or shows a note"
  fi
  echo "the search whose only result is the listed ci_cards adds neither row nor note"
  clear_search
}

load_row_states() {
  local loaded deadline
  type_search "ci avatar"
  search_directory "Load more"
  "$ui" shot load-more
  expect_in_tree -E 'Load more · [0-9]+ left'
  loaded=$("$ui" tree | grep -cF '"#ci_avatar_' || true)
  "$ui" directory stop
  tap_line "$(labelled_containing "Load more")"
  expect_in_tree -E '^Button ".*Load more.*" .* disabled$'
  "$ui" shot load-more-loading
  "$ui" wait "Search failed, tap to retry" 30
  "$ui" shot load-more-failed
  "$ui" directory start
  deadline=$((SECONDS + 120))
  until [ "$("$ui" tree | grep -cF '"#ci_avatar_' || true)" -gt "$loaded" ]; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      "$ui" shot load-more-retry-failed
      fail "retrying the failed page did not load more results after 120 s"
    fi
    retry_failed_search || true
    sleep 5
  done
  "$ui" shot load-more-retried
  echo "the load row shows how many results are left, is disabled while its page loads, offers a retry when it fails, and the retry loads the page"
  clear_search
}

report_long_press_menu() {
  local opens
  type_search "$search_text"
  search_directory '"#ci_group"'
  opens=$(labelled Open)
  tap_line "$(labelled_containing '"#ci_group" value=')" long-press
  "$ui" shot long-press-menu
  if [ -n "$(added_labelled Open "$opens")" ]; then
    echo "a long press on #ci_group shows a menu with Open"
    "$ui" tap-xy 200 100
  else
    echo "WARN: a long press on #ci_group shows no menu with Open; see the long-press-menu screenshot"
  fi
}

tap_element() {
  local line
  line=$("$ui" tree | grep -iE "^$1 \"$2\"" | tail -n 1)
  [ -n "$line" ] || return 1
  tap_line "$line"
}

sent_files() {
  find "$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" group.chat.simplex.app)" "$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" data)" -name "IMG_*.$1" 2> /dev/null | wc -l
}

sent_file() {
  local deadline=$((SECONDS + 30))
  until [ "$(sent_files "$1")" -gt "$2" ]; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 2
  done
}

sent_size() {
  local file
  file=$(find "$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" group.chat.simplex.app)" "$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" data)" -name "IMG_*.$1" -exec ls -t {} + 2> /dev/null | sed -n 1p)
  sips -g pixelWidth -g pixelHeight "$file" | awk '/pixelWidth/ {w = $2} /pixelHeight/ {h = $2} END {print w "x" h}'
}

disable_file_encryption() {
  xcrun simctl terminate "$SIM_UDID" "$bundle_id" 2> /dev/null || true
  xcrun simctl spawn "$SIM_UDID" defaults write "$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" group.chat.simplex.app)/Library/Preferences/group.chat.simplex.app" privacyEncryptLocalFiles -bool false
  "$ui" relaunch
}

fail() {
  echo "error: $*"
  exit 1
}

create_group() {
  "$ui" tap Compose
  "$ui" tap "Create group" 30
  "$ui" tap-value "Enter group name…" 30
  "$ui" type "image notes"
  "$ui" shot new-group
  tap_element Button "Create group" || fail "no \"Create group\" button"
  "$ui" tap Skip 10 || true
  "$ui" shot group-created
}

app_resized_images() {
  grep -cE "resizeImageTo(Str|Data)Size final" "$UI_OUT/app.log" || true
}

send_image() {
  local ext=$1 photo_x=$2 size=$3 sent resized
  sent=$(sent_files "$ext")
  resized=$(app_resized_images)
  tap_element Button "Attachements" || fail "no attachment button"
  "$ui" tap "Choose from library"
  sleep 3
  "$ui" shot "picker-$ext"
  "$ui" tap-xy "$photo_x" "$photo_y"
  "$ui" shot "picker-selected-$ext"
  "$ui" tap-xy "$picker_done_x" "$picker_done_y"
  sleep 3
  "$ui" shot "preview-$ext"
  "$ui" find multiply || fail "$ext: no image preview in the compose area"
  tap_element Button "[^\"]*arrow[^\"]*" || fail "no send button"
  sent_file "$ext" "$sent" || fail "$ext: no .$ext file sent"
  "$ui" shot "sent-$ext"
  [ "$(sent_size "$ext")" = "$size" ] || fail "$ext: the sent image is $(sent_size "$ext"), not $size"
  [ "$(app_resized_images)" -eq "$resized" ] || fail "$ext: the app resized the image in place of the core"
  echo "$ext: the picked image was previewed and sent as a $size .$ext file"
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
echo "smoke test passed: onboarding reaches the chat list, which shows its onboarding pages and toolbar with the one-hand UI and with the top bar"

products=$(dirname "$app")
if [[ "$products" == */Build/Products/Debug-iphonesimulator ]]; then
  "$(dirname "$0")/snapshots.sh" "${products%/Build/Products/Debug-iphonesimulator}" "$UI_OUT/snapshots"
  if "$ui" wait "would like to access the Camera" 10; then
    "$ui" tap "Don’t Allow"
  fi
fi

if [ -n "${BACKEND_DIR:-}" ]; then
  route_privately_to_unknown_servers
  typed_text_offers_directory
  cancel_consent_searches_nothing
  enable_and_search
  search_key_keeps_connect_to
  seed_results_in_top_layout
  open_and_join_ci_cards
  search_listed_chat_adds_nothing
  expect_no_chats_found_after_search
  expect_cards_hidden_by_results reachable
  open_known_chat_from_results reachable
  "$ui" layout top
  dismiss_simplex_lock_notice
  expect_cards_hidden_by_results top
  open_known_chat_from_results top
  "$ui" layout reachable
  search_in_new_chat
  load_row_states
  report_long_press_menu
  echo "smoke test passed: directory search"
fi

if [ "$cases" = images ]; then
  "$ui" tap "Don’t Allow" 10 || true
  disable_file_encryption
  xcrun simctl addmedia "$SIM_UDID" "$fixtures/test.jpg" "$fixtures/logo-large-rgba.png"
  create_group
  send_image jpg "$test_card_x" 1280x720
  send_image png "$logo_x" 1630x466
  echo "smoke test passed: images"
fi
