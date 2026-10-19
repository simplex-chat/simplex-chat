#!/bin/bash

# Installs the APK in the booted emulator (emulator.sh) and runs the scripted UI cases with the ui helper: onboarding to
# the chat list, a local group, then the chat list with the reachable toolbar (the default) and with the top bar.
# Usage: scripts/ci/android/smoke-test.sh <apk> <output dir> [start|onboarding|layouts|backend]
# With "start" it stops at the first screen, for an agent to go on from; with "onboarding", at the chat list. "backend"
# also adds the test SMP server (NAMES_SERVER, from backend.sh, reached through emulator.sh reverse) under Your servers
# and tests it, before both layouts.
# Each step saves a numbered screenshot, the accessibility tree and the text on screen to the output dir, prints the
# text to the log, and adds a line to steps.log there; logcat is in app.log. What the screens show is checked without
# stopping: every check is listed in checks.txt and the job summary, and the test fails at the end if any failed.
# The app keeps running for later steps of the job, which get UI_OUT.

set -euo pipefail

apk=$1
export UI_OUT=$2
cases=${3:-layouts}
case "$cases" in
  start | onboarding | layouts | backend) ;;
  *)
    echo "error: unknown case \"$cases\": start, onboarding, layouts (the default) or backend"
    exit 1
    ;;
esac
package=chat.simplex.app
local_chat="seeded notes"
search_field="Search or paste SimpleX link"
ui=$(dirname "$0")/ui
checks=$UI_OUT/checks.txt
mkdir -p "$UI_OUT"
: > "$checks"
if [ -n "${GITHUB_ENV:-}" ]; then
  echo "UI_OUT=$UI_OUT" >> "$GITHUB_ENV"
fi

report_checks() {
  [ -s "$checks" ] || return 0
  echo "::group::checks"
  cat "$checks"
  echo "::endgroup::"
  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    {
      echo "### Smoke test checks"
      echo
      sed 's/^/- /' "$checks"
    } >> "$GITHUB_STEP_SUMMARY"
  fi
}

finish() {
  local status=$?
  if [ "$status" -ne 0 ]; then
    "$ui" shot final || true
  fi
  echo "::group::the core and crashes in logcat"
  grep -E 'ndk_translation|SIMPLEX *: (start|App crashed)|AndroidRuntime: (FATAL EXCEPTION|Process: )|Fatal signal' "$UI_OUT/app.log" | head -n 100 || true
  echo "::endgroup::"
  report_checks
  exit "$status"
}
trap finish EXIT

fail() {
  echo "error: $*"
  exit 1
}

check() {
  local what=$1
  shift
  if "$@"; then
    echo "PASS $what" | tee -a "$checks"
  else
    echo "FAIL $what" | tee -a "$checks"
    echo "::error::$what"
  fi
}

# A crash: the app logs its own under SIMPLEX ("App crashed"), Android's FATAL EXCEPTION covers one before the app's
# handler is set, and a native fatal signal names the process by the last 15 characters of its name. Not any line
# with "error" or from AndroidRuntime, which logs every uiautomator dump.
app_running() {
  ! grep -q 'SIMPLEX *: App crashed' "$UI_OUT/app.log" &&
    ! grep -A 1 'FATAL EXCEPTION' "$UI_OUT/app.log" | grep -q "Process: $package," &&
    ! grep -q "Fatal signal .*(${package: -15})" "$UI_OUT/app.log" &&
    adb shell pidof "$package" > /dev/null
}

y_of() {
  local pos
  pos=$("$ui" find "$1") || return 1
  cut -d ' ' -f 2 <<< "$pos"
}

above() {
  local a b
  a=$(y_of "$1") || return 1
  b=$(y_of "$2") || return 1
  [ "$a" -lt "$b" ]
}

screen_height() {
  adb shell wm size | sed -n 's/^Physical size: [0-9]*x//p'
}

# Back closes one screen at a time, here between the screens of Settings, which cover the chat list without removing it
# from the tree: a restart is what returns to the chat list.
back_to() {
  for _ in 1 2 3 4 5; do
    "$ui" find "$1" > /dev/null 2>&1 && return 0
    "$ui" key BACK
  done
  "$ui" find "$1" > /dev/null
}

# On a first start the chat list can show for a moment before onboarding, long enough to open its What's new sheet over
# the first screen: it is closed and noted, not failed.
first_screen() {
  local deadline=$((SECONDS + 180))
  until "$ui" find "Get started" > /dev/null 2>&1; do
    app_running || return 1
    if "$ui" find "New in" > /dev/null 2>&1; then
      echo "NOTE the What's new sheet covered the first onboarding screen" | tee -a "$checks"
      "$ui" shot whats-new-over-onboarding
      "$ui" click-text OK
    fi
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
}

# After Your network Android asks about instant notifications, in front of the conditions: the app's alert, then the
# system's battery dialog, each with "Allow". The conditions show for a moment between the two.
allow_notifications_and_accept() {
  local deadline=$((SECONDS + 120))
  until "$ui" find "Talk to someone" > /dev/null 2>&1; do
    if "$ui" find "Allow" > /dev/null 2>&1; then
      "$ui" shot notifications
      "$ui" click-text "Allow"
    elif "$ui" find "Accept" > /dev/null 2>&1; then
      "$ui" shot network-commitments
      "$ui" click-text "Accept"
    fi
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
}

# The centre "x y" of the first text field below the text, which may have neither a text nor a placeholder.
field_below() {
  local y
  y=$(y_of "$1") || return 1
  "$ui" tree | sed -n -E 's/^EditText .* at ([0-9]+),([0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/p' |
    awk -v y="$y" '$2 > y { print $1 + int($3 / 2), $2 + int($4 / 2); exit }'
}

# The centre "x y" of the switch in the row of the text.
switch_of() {
  local y
  y=$(y_of "$1") || return 1
  "$ui" tree | sed -n -E 's/.* at ([0-9]+),([0-9]+) size ([0-9]+)x([0-9]+) .*checkable.*/\1 \2 \3 \4/p' |
    awk -v y="$y" '$2 <= y && y <= $2 + $4 { print $1 + int($3 / 2), $2 + int($4 / 2); exit }'
}

# The user picker's button in the toolbar, before the list has chats: chats show the same profile image placeholder.
open_settings() {
  "$ui" click-text "profile image placeholder" 10
  "$ui" click-text "Settings" 10
}

# Green pixels of the test status left of a server's host name: SimplexGreen, the tick of a server that passed its test.
green_before() {
  local x y l t r b png
  read -r x y l t r b < <("$ui" find "$1") || return 1
  "$ui" shot "$2" > /dev/null || return 1
  png=$(find "$UI_OUT" -maxdepth 1 -name "*-$2.png" | sort | tail -n 1)
  convert "$png" -alpha off -crop "${l}x$((b - t))+0+$t" +repage -fuzz 20% -fill black +opaque 'rgb(77,218,103)' \
    -fill white -opaque 'rgb(77,218,103)' -colorspace Gray -format '%[fx:round(mean*w*h)]' info:
}

# Waits up to 60 s for Test servers to finish: the test status turns green, or the app shows that the test failed.
server_tested() {
  local deadline=$((SECONDS + 60)) green
  while [ "$SECONDS" -lt "$deadline" ]; do
    if "$ui" find "Server test failed" > /dev/null 2>&1; then
      "$ui" shot server-test-failed
      return 1
    fi
    green=$(green_before 127.0.0.1 server-test) || return 1
    echo "green pixels before the test server's host: $green"
    if [ "$green" -ge 50 ]; then
      return 0
    fi
    sleep 2
  done
  return 1
}

# Settings > Network & servers > Your servers: the test SMP server added, tested and saved. Its address has 127.0.0.1,
# which the emulator forwards to the runner's.
test_server() {
  local x y
  open_settings
  "$ui" click-text "Network & servers" 10
  "$ui" click-text "Your servers" 10
  "$ui" click-text "Add server" 10
  "$ui" click-text "Enter server manually" 10
  "$ui" wait "Your server address" 10
  read -r x y < <(field_below "Your server address") || fail "no address field for the new server"
  "$ui" click "$x" "$y"
  "$ui" type "$NAMES_SERVER"
  # the keyboard can show the last characters a moment later, and a tap before that lost one
  "$ui" wait "$NAMES_SERVER" 15
  # a server entered by hand is off, and Test servers tests only the servers that are on
  read -r x y < <(switch_of "Use for new connections") || fail "no \"Use for new connections\" switch"
  "$ui" click "$x" "$y"
  "$ui" shot new-server
  back_to "Test servers" || fail "no Your servers after adding the server"
  "$ui" wait 127.0.0.1 10
  "$ui" click-text "Test servers"
  check "the test SMP server passes Test servers" server_tested
  back_to "Save servers" || fail "no Network & servers after Your servers"
  "$ui" click-text "Save servers"
  "$ui" shot servers-saved
  "$ui" relaunch
}

# A group of the user's own: without chats the chat list shows the onboarding pages, not the search field. The new
# group's name field has the focus.
create_local_chat() {
  "$ui" click-text "Start new chat" 10
  "$ui" click-text "Create group" 10
  "$ui" wait "Enter group name" 10
  "$ui" type "$local_chat"
  "$ui" wait "$local_chat" 10
  "$ui" shot new-group
  "$ui" click-text "Create group" 10
  # the app offers to invite members once it has created the group; without contacts it has no "Skip inviting members"
  "$ui" wait "Invite members" 30
  "$ui" shot group-created
  "$ui" relaunch
  "$ui" wait "$search_field" 30
  "$ui" shot local-chat
}

# The search field and "Add list" next to it: at the bottom of the chat list, "Add list" above the field; at the top, below it.
check_layout() {
  local layout=$1 field half
  half=$(($(screen_height) / 2))
  field=$(y_of "$search_field") || field=""
  "$ui" shot "$layout-layout"
  check "$layout: \"$local_chat\" listed" "$ui" find "$local_chat"
  if [ "$layout" = reachable ]; then
    check "reachable: the search field is in the bottom half" [ "${field:-0}" -gt "$half" ]
    check "reachable: \"Add list\" above the search field" above "Add list" "$search_field"
  else
    check "top: the search field is in the top half" [ "${field:-$((half * 2))}" -lt "$half" ]
    check "top: \"Add list\" below the search field" above "$search_field" "Add list"
  fi
}

# private routing to unknown servers only: the preset servers that would proxy to the test SMP server can't reach the
# runner's 127.0.0.1; developer tools at the debug level put the app's debug log in logcat
started=$SECONDS
"$ui" start "$apk" NetworkSMPProxyMode=Unknown DeveloperTools=true LogLevel=DEBUG
first_screen || fail "no first onboarding screen 180 s after the start, or the app stopped: its crashes in logcat are below"
echo "first screen after $((SECONDS - started)) s"
"$ui" shot first-screen
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

allow_notifications_and_accept || fail "no chat list 120 s after Your network"
"$ui" shot chat-list
echo "onboarding reached the chat list $((SECONDS - started)) s after the start"

if [ "$cases" != onboarding ]; then
  if [ "$cases" = backend ]; then
    : "${NAMES_SERVER:?NAMES_SERVER must be the test SMP server address}"
    test_server
  fi
  create_local_chat
  check_layout reachable
  "$ui" layout top
  check_layout top
fi
check "the app is running and logged no crash" app_running

if grep -q '^FAIL' "$checks"; then
  fail "the smoke test failed checks: $(grep -c '^FAIL' "$checks") of $(grep -cE '^(PASS|FAIL)' "$checks")"
fi
echo "smoke test passed: $cases"
