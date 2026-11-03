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

tap_element() {
  local line x y w h
  line=$("$ui" tree | grep -iE "^$1 \"$2\"" | tail -n 1)
  [ -n "$line" ] || return 1
  read -r x y w h < <(sed -E 's/.* at (-?[0-9]+),(-?[0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/' <<< "$line")
  "$ui" tap-xy $((x + w / 2)) $((y + h / 2))
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
