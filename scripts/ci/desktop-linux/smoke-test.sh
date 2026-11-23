#!/bin/bash

# Starts the desktop app on a virtual display and runs the scripted UI cases with the ui helper: onboarding to the
# chat list, a local group, then the chat list with the toolbar at the bottom (the default) and at the top, then
# tests/fixtures/test.jpg and logo-large-rgba.png picked in the GTK file dialog and sent in the group.
# Usage: scripts/ci/desktop-linux/smoke-test.sh <app dir> <output dir> [start|onboarding|layouts|setup|backend|tour]
# With "start" it stops at the first screen, for an agent to go on from; with "onboarding", at the chat list.
# "tour" sizes the window's content to 1366x768, creates the group and opens each screen reachable from the chat list,
# saved as tour-<name>.
# "setup" creates the group, makes the test SMP server (backend.sh) the one that resolves SimpleX names, and stops;
# "backend" does the same, connects to the name that backend.sh registered, and goes on to both layouts.
# Both need NAMES_SERVER, the test SMP server's address.
# Each step saves a numbered screenshot and the text read from it to the output dir, prints the text to the log,
# and adds a line to steps.log there. What the screens show is checked without stopping: every check is listed in
# checks.txt and the job summary, and the test fails at the end if any failed. The app keeps running for later
# steps of the job, which get UI_OUT.

set -euo pipefail

app_dir=$1
export UI_OUT=$2
cases=${3:-layouts}
case "$cases" in
  start | onboarding | layouts | setup | backend | tour) ;;
  *)
    echo "error: unknown case \"$cases\": start, onboarding, layouts (the default), setup, backend or tour"
    exit 1
    ;;
esac
# backend.sh registered seeded.testing for ci_contact
name_label=seeded
local_chat="seeded notes"
ui=$(dirname "$0")/ui
checks=$UI_OUT/checks.txt
# switches are at the right of the left pane, where Settings open
switch_offset=335
fixtures=$(cd "$(dirname "$0")/../../../tests/fixtures" && pwd)
state=${UI_STATE:-${RUNNER_TEMP:-/tmp}/ui}
bottom_bar_offset=30
avatar_offset=27
pencil_offset=363
pane_centre_offset=195
pane_right_offset=390
paperclip_offset=415
send_offset=27
tour_settle=1
tour_scroll=10
tour_label_scrolls=4
tour_label_wait=5
tour_window_width=1366
tour_window_height=768
app_version=$(sed -n 's/^desktop.version_name=//p' "$(dirname "$0")/../../../apps/multiplatform/gradle.properties")
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
  echo "::group::app log, last 300 lines"
  tail -n 300 "$UI_OUT/app.log" || true
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

# text compared as ui compares it: letters and digits, lowercase, a lowercase l read as i
normalise() {
  tr -cd '[:alnum:]' <<< "$1" | tr '[:upper:]' '[:lower:]' | tr l i
}

# Prints "x y" of the text in the chat list pane, left of x 390: a phrase equal to it, else, unless the second argument
# is "exact", the first containing it. The chats, results and Settings are there; the right pane shows the open chat,
# which can have the same names.
left_pane_position() {
  local target line x y found=""
  target=$(normalise "$1")
  while IFS= read -r line; do
    x=${line%%,*}
    y=${line#*,}
    y=${y%% *}
    [ "$x" -lt "$pane_right_x" ] || continue
    case "$(normalise "${line#* }")" in
      "$target")
        echo "$x $y"
        return 0
        ;;
      *"$target"*)
        found=${found:-"$x $y"}
        ;;
    esac
  done < <("$ui" text)
  [ -n "$found" ] && [ "${2:-}" != exact ] || return 1
  echo "$found"
}

on_screen() {
  left_pane_position "$1" > /dev/null
}

not_on_screen() {
  ! left_pane_position "$1" > /dev/null
}

y_of() {
  local pos
  pos=$(left_pane_position "$1") || return 1
  cut -d ' ' -f 2 <<< "$pos"
}

above() {
  local a b
  a=$(y_of "$1") || return 1
  b=$(y_of "$2") || return 1
  [ "$a" -lt "$b" ]
}

# the value is on the same line as its setting's title
in_line_with() {
  local value title
  value=$(y_of "$1") || return 1
  title=$(y_of "$2") || return 1
  [ $((value - title)) -lt 10 ] && [ $((title - value)) -lt 10 ]
}

# Scrolls the left pane down until the text shows above its bottom toolbar.
scroll_to() {
  local y
  for _ in 1 2 3 4 5 6 7 8; do
    if y=$(y_of "$1") && [ "$y" -lt $((bottom_y - 90)) ]; then
      return 0
    fi
    "$ui" scroll "$pane_centre_x" $((window_y + 380)) down
  done
  return 1
}

# Escape closes one screen at a time, and clears the search field
back_to_chat_list() {
  local attempt
  for attempt in 1 2 3 4 5 6; do
    on_chat_list && return 0
    if [ $((attempt % 2)) -eq 0 ]; then
      "$ui" click "$avatar_x" "$bottom_y"
    else
      "$ui" key Escape
    fi
  done
  on_chat_list
}

left_pane_phrases() {
  local line
  while IFS= read -r line; do
    if [ "${line%%,*}" -lt "$pane_right_x" ]; then
      normalise "${line#* }"
      echo
    fi
  done < <("$ui" text)
}

# New chat has the same search field
on_chat_list() {
  local phrases title
  phrases=$(left_pane_phrases)
  for title in "Settings" "Chat preferences" "Network" "New chat"; do
    [[ "$phrases" == *"$(normalise "$title")"* ]] && return 1
  done
  [[ "$phrases" == *"$(normalise "Search or paste SimpleX link")"* ]]
}

# Opens Settings from the user menu, behind the profile picture at the left of the bottom toolbar.
open_settings() {
  "$ui" click "$avatar_x" "$bottom_y"
  "$ui" click-text "Settings" 10
}

# Turns "Reachable app toolbars" off in Settings > Appearance, which moves the toolbar and the search field to the top.
use_top_toolbar() {
  local y
  "$ui" key Escape
  open_settings
  "$ui" click-text "Appearance" 10
  "$ui" wait "Reachable app toolbars" 10
  "$ui" shot appearance
  y=$(y_of "Reachable app toolbars")
  "$ui" click "$switch_x" "$y"
  "$ui" shot appearance-toolbar-switched
  back_to_chat_list || true
  "$ui" shot top-chat-list
  y=$(y_of "Search or paste SimpleX link") || fail "no search field after closing Settings"
  [ "$y" -lt $((window_y + 180)) ] || fail "the search field is still at the bottom (y $y) after switching Reachable app toolbars"
}

# The search field and "Add list" next to it: at the bottom of the chat list, "Add list" above the field; at the top, below it.
check_layout() {
  local layout=$1 field
  field=$(y_of "Search or paste SimpleX link") || field=""
  "$ui" shot "$layout-layout"
  check "$layout: \"$local_chat\" listed" on_screen "$local_chat"
  if [ "$layout" = bottom ]; then
    check "bottom: the search field is at the bottom" [ "${field:-0}" -gt $((bottom_y - 130)) ]
    check "bottom: \"Add list\" above the search field" above "Add list" "Search or paste SimpleX link"
  else
    check "top: the search field is at the top" [ "${field:-900}" -lt $((window_y + 180)) ]
    check "top: \"Add list\" below the search field" above "Search or paste SimpleX link" "Add list"
  fi
}

# A group of the user's own: without chats the chat list shows no search field.
create_local_chat() {
  local y
  # the pencil at the right of the bottom toolbar opens New chat
  "$ui" click "$pencil_x" "$bottom_y"
  "$ui" click-text "Create group" 10
  "$ui" wait "Enter group name" 10
  y=$(y_of "Enter group name")
  "$ui" click "$pane_centre_x" $((y + 40))
  "$ui" type "$local_chat"
  "$ui" shot new-group
  "$ui" click-text "Create group" 10
  "$ui" click-text "Skip inviting members" 10 || true
  "$ui" shot group-created
  "$ui" key Escape
  back_to_chat_list || fail "no chat list after creating the group"
  # the group stays open in the right pane: Escape doesn't close a chat on desktop
  on_screen "$local_chat" || fail "\"$local_chat\" is not in the chat list"
  "$ui" shot local-chat
}

# Clicks a server role's value until its dropdown shows. The menu is open when an item reads exactly "yes", which no
# role's value does ("default (yes)", "default (no)", "no"); it can take longer to show than OCR takes to read the screen.
open_role_menu() {
  for _ in 1 2 3; do
    "$ui" click-text "$1" 10 || return 1
    for _ in 1 2 3; do
      left_pane_position yes exact > /dev/null && return 0
      sleep 1
    done
  done
  return 1
}

# Settings > Network & servers: the test SMP server added with the names role, and names off for the preset
# operators, so that only the test server resolves names.
use_names_server() {
  local x y operator
  open_settings
  # OCR reads "Network & servers" as two phrases
  "$ui" click-text "Network" 10
  "$ui" click-text "Your servers" 10
  "$ui" click-text "Add server" 10
  "$ui" click-text "Enter server manually" 10
  "$ui" wait "Your server address" 10
  y=$(y_of "Your server address")
  "$ui" click "$pane_centre_x" $((y + 50))
  "$ui" type "$NAMES_SERVER"
  y=$(y_of "Use for new connections")
  "$ui" click "$switch_x" "$y"
  "$ui" shot new-server
  "$ui" key Escape
  # the New server screen shows the address with "127.0.0.1" too, until it closes
  "$ui" wait "Add server" 10
  "$ui" wait "Test servers" 10
  "$ui" click-text "127.0.0.1" 10
  "$ui" wait "SMP server" 10
  scroll_to "To resolve names" || fail "no \"To resolve names\" in the test server's settings"
  # A dropdown opens from its value; a custom server's roles default to yes, yes and no, so the first "default (yes)"
  # is the receiving one. A directory service can't reply to queues on this server, so it only resolves names.
  open_role_menu "default (yes)" || fail "no menu for the test server's \"To receive\""
  # OCR misreads the short "no", the item after "yes"
  read -r x y < <(left_pane_position yes exact) || fail "the \"To receive\" menu closed"
  "$ui" click "$x" $((y + 48))
  open_role_menu "default (no)" || fail "no menu for the test server's \"To resolve names\""
  read -r x y < <(left_pane_position yes exact) || fail "the \"To resolve names\" menu closed"
  "$ui" click "$x" "$y"
  "$ui" shot names-server-roles
  # the first "default (yes)" and the first "yes" are on the lines after "To receive", so it is no
  in_line_with "default (yes)" "For private routing" || fail "the test server's \"To receive\" is still its default"
  in_line_with "yes" "To resolve names" || fail "the test server's \"To resolve names\" is not yes, or \"To receive\" is"
  "$ui" key Escape
  "$ui" wait "Add server" 10
  "$ui" key Escape
  for operator in "SimpleX Chat" Flux; do
    "$ui" click-text "$operator" 10
    # the operator's own toggle: the title "<operator> servers" can wrap
    "$ui" wait "Use servers" 10
    scroll_to "To resolve names" || fail "no \"To resolve names\" in the $operator settings"
    y=$(y_of "To resolve names")
    "$ui" click "$switch_x" "$y"
    "$ui" shot "names-off-$(tr -cd 'a-zA-Z' <<< "$operator")"
    "$ui" key Escape
    "$ui" wait "Your servers" 10
  done
  scroll_to "Save servers" || fail "no \"Save servers\" in Network & servers"
  "$ui" click-text "Save servers" 10
  sleep 2
  "$ui" shot servers-saved
  back_to_chat_list || fail "no chat list after saving the servers"
}

# A bare name in the search field is offered with the .testing domain; the test SMP server resolves it to ci_contact's
# address, and the app shows that profile before connecting.
connect_by_name() {
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$name_label"
  "$ui" wait "Connect to $name_label.testing" 15
  "$ui" shot name-typed
  "$ui" click-text "Connect to $name_label.testing"
  check "$name_label.testing resolves to ci_contact" "$ui" wait ci_contact 60
  check "the app offers to open a new chat with ci_contact" "$ui" wait "Open new chat" 10
  "$ui" shot name-resolved
  "$ui" click-text Cancel 10 || "$ui" key Escape
  back_to_chat_list || fail "no chat list after the name's alert"
}

sent_files() {
  find -H "$state/data" -name "IMG_*.$1" | wc -l
}

sent_file() {
  local deadline=$((SECONDS + 30))
  until [ "$(sent_files "$1")" -gt "$2" ]; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
}

latest_shot() {
  find "$UI_OUT" -maxdepth 1 -name "[0-9][0-9]-$1.png" | sort | tail -n 1
}

compose_changed() {
  local changed
  local compose_area=976x200+$pane_right_x+$((bottom_y - 230))
  convert "$(latest_shot "$1")" -crop "$compose_area" +repage "$UI_OUT/compose-$1.png"
  convert "$(latest_shot "$2")" -crop "$compose_area" +repage "$UI_OUT/compose-$2.png"
  changed=$(compare -metric AE -fuzz 10% "$UI_OUT/compose-$1.png" "$UI_OUT/compose-$2.png" null: 2>&1 || true)
  changed=${changed%% *}
  echo "pixels changed above the message field: $changed"
  [ "${changed%%.*}" -gt 2000 ]
}

core_returned_no_image() {
  grep -c "core returned no image" "$UI_OUT/app.log" || true
}

send_image() {
  local file=$1 ext=$2 sent no_image
  sent=$(sent_files "$ext")
  no_image=$(core_returned_no_image)
  "$ui" shot "before-$ext"
  "$ui" click "$paperclip_x" "$bottom_y"
  "$ui" click-text Image 10
  "$ui" wait "Other Locations" 60
  "$ui" shot "file-dialog-$ext"
  "$ui" type /
  sleep 1
  "$ui" type "${fixtures#/}/$file"
  "$ui" shot "file-path-$ext"
  "$ui" key Return
  sleep 2
  if "$ui" find "Other Locations" > /dev/null 2>&1; then
    "$ui" click-text Open 5
  fi
  sleep 3
  "$ui" shot "preview-$ext"
  check "$ext: the compose area shows the picked image" compose_changed "before-$ext" "preview-$ext"
  "$ui" click "$send_x" "$bottom_y"
  check "$ext: the image is sent as a .$ext file" sent_file "$ext" "$sent"
  "$ui" shot "sent-$ext"
  check "$ext: the core resized the image" [ "$(core_returned_no_image)" -eq "$no_image" ]
}

send_images() {
  local x y
  read -r x y < <(left_pane_position "$local_chat") || fail "no \"$local_chat\" in the chat list"
  "$ui" click "$x" "$y"
  send_image test.jpg jpg
  send_image logo-large-rgba.png png
}

open_user_menu() {
  "$ui" click "$avatar_x" "$bottom_y"
  "$ui" wait "Settings" 10
}

open_new_chat() {
  "$ui" click "$pencil_x" "$bottom_y"
  "$ui" wait "New chat" 10
}

open_local_chat() {
  local x y
  read -r x y < <(left_pane_position "$local_chat") || return 1
  "$ui" click "$x" "$y"
}

open_group_info() {
  local target line
  open_local_chat || return 1
  sleep "$tour_settle"
  target=$(normalise "$local_chat")
  while IFS= read -r line; do
    [ "${line%%,*}" -ge "$pane_right_x" ] || continue
    if [ "$(normalise "${line#* }")" = "$target" ]; then
      line=${line%% *}
      "$ui" click "${line%%,*}" "${line#*,}"
      return 0
    fi
  done < <("$ui" text)
  return 1
}

click_scrolled() {
  for _ in $(seq "$tour_label_scrolls"); do
    "$ui" click-text "$1" "$tour_label_wait" && return 0
    "$ui" scroll "$pane_centre_x" $((window_y + 380)) down
  done
  return 1
}

visit() {
  local name=$1 opener=$2 label
  shift 2
  back_to_chat_list || true
  if ! "$opener"; then
    check "tour: $name opens" false
    return
  fi
  for label in "$@"; do
    if ! click_scrolled "$label"; then
      check "tour: $name opens" false
      return
    fi
  done
  sleep "$tour_settle"
  "$ui" shot "tour-$name"
  "$ui" scroll "$pane_centre_x" $((window_y + 380)) down "$tour_scroll"
  sleep "$tour_settle"
  "$ui" shot "tour-$name-end"
  check "tour: $name opens" true
}

tour() {
  visit user-menu open_user_menu
  visit settings open_settings
  visit appearance open_settings "Appearance"
  visit privacy open_settings "Your privacy"
  visit help open_settings "Help & support"
  visit how-to-use open_settings "Help & support" "How to use it"
  visit whats-new open_settings "Help & support" "What's new"
  visit about open_settings "Help & support" "About SimpleX Chat"
  visit chat-data open_settings "Chat data"
  visit support open_settings "Support SimpleX"
  visit network open_settings "Network"
  visit your-servers open_settings "Network" "Your servers"
  visit advanced-network open_settings "Network" "Advanced network settings"
  visit calls open_settings "Audio"
  visit version open_settings "$app_version"
  visit developer-tools open_settings "$app_version" "Developer tools"
  visit address open_user_menu "SimpleX address"
  visit chat-preferences open_user_menu "Chat preferences"
  visit chat-profiles open_user_menu "Your chat profiles"
  visit link-mobile open_user_menu "Link a mobile"
  visit new-chat open_new_chat
  visit one-time-link open_new_chat "Create 1-time link"
  visit local-chat open_local_chat
  visit group-info open_group_info
  back_to_chat_list || true
  if open_local_chat; then
    "$ui" click "$paperclip_x" "$bottom_y"
    sleep "$tour_settle"
    "$ui" shot tour-attachment-menu
    "$ui" key Escape
  fi
  visit migrate open_settings "Migrate to another device"
}

# private routing to unknown servers only: the preset servers that would proxy name lookups can't reach the test SMP
# server on 127.0.0.1; developer tools at the debug level put the app's debug log in app.log
read -r -a extra_settings <<< "${SMOKE_SETTINGS:-}"
"$ui" start "$app_dir" NetworkSMPProxyMode=Unknown DeveloperTools=true LogLevel=DEBUG "${extra_settings[@]}"
if [ "$cases" = tour ]; then
  "$ui" size "$tour_window_width" "$tour_window_height"
fi
read -r window_x window_y window_width window_height < "$state/window-rect"
bottom_y=$((window_y + window_height - bottom_bar_offset))
switch_x=$((window_x + switch_offset))
avatar_x=$((window_x + avatar_offset))
pencil_x=$((window_x + pencil_offset))
pane_centre_x=$((window_x + pane_centre_offset))
pane_right_x=$((window_x + pane_right_offset))
paperclip_x=$((window_x + paperclip_offset))
send_x=$((window_x + window_width - send_offset))

# onboarding to the chat list
"$ui" wait "Create your profile" 180
"$ui" shot first-screen
if [ "$cases" = start ]; then
  echo "smoke test: the app shows its first screen"
  exit 0
fi
"$ui" click-text "Create your profile"

"$ui" wait "Create profile" 30
"$ui" click-text "Enter profile name"
"$ui" type citester
"$ui" shot profile-name
"$ui" click-text "Create profile"

# a new desktop database gets a random passphrase, which onboarding offers to replace
"$ui" wait "Use random passphrase" 60
"$ui" shot database-passphrase
"$ui" click-text "Use random passphrase"
"$ui" wait "OK" 10
"$ui" shot random-passphrase-alert
"$ui" click-text "OK"

"$ui" wait "Continue" 60
"$ui" shot your-network
"$ui" click-text "Continue"

"$ui" wait "Accept" 30
"$ui" shot network-commitments
"$ui" click-text "Accept"

"$ui" wait "Talk to someone" 60
sleep 3
"$ui" shot chat-list
if [ "$cases" = onboarding ]; then
  echo "smoke test passed: onboarding reached the chat list"
  exit 0
fi

create_local_chat
if [ "$cases" = tour ]; then
  tour
  if grep -q '^FAIL' "$checks"; then
    fail "the tour did not open $(grep -c '^FAIL' "$checks") of $(grep -cE '^(PASS|FAIL)' "$checks") screens"
  fi
  echo "tour done: $(grep -c '^PASS' "$checks") screens saved as tour-<name>"
  exit 0
fi
if [ "$cases" = setup ] || [ "$cases" = backend ]; then
  : "${NAMES_SERVER:?NAMES_SERVER must be the test SMP server address}"
  use_names_server
  if [ "$cases" = setup ]; then
    echo "smoke test setup done: \"$local_chat\" created, $NAMES_SERVER resolves names"
    exit 0
  fi
  connect_by_name
fi

check_layout bottom
use_top_toolbar
check_layout top
send_images

if grep -q '^FAIL' "$checks"; then
  fail "the smoke test failed checks: $(grep -c '^FAIL' "$checks") of $(grep -cE '^(PASS|FAIL)' "$checks")"
fi
echo "smoke test passed: onboarding reached the chat list, which shows \"$local_chat\" with the toolbar at the bottom and at the top"
