#!/bin/bash

# Starts the desktop app on a virtual display and runs the scripted UI cases with the ui helper:
# onboarding to the chat list; setup, which creates a local group and makes the test SMP server (backend.sh) the one
# that resolves SimpleX names; then, with the toolbar at the bottom (the default) and then at the top, a search of
# the test directory with "Search in Directory" and the full search with Enter, which also resolves a SimpleX name.
# Usage: scripts/ci/desktop-linux/smoke-test.sh <app dir> <output dir> [onboarding|setup]
# With "onboarding" it stops at the chat list: releases before directory search show no search field without chats.
# With "setup" it stops after the setup. Setup needs NAMES_SERVER, the test SMP server's address.
# Each step saves a numbered screenshot and the text read from it to the output dir, prints the text to the log,
# and adds a line to steps.log there. What the searches show is checked without stopping: every check is listed in
# checks.txt and the job summary, and the test fails at the end if any failed. The app keeps running for later
# steps of the job, which get UI_OUT.

set -euo pipefail

app_dir=$1
export UI_OUT=$2
cases=${3:-all}
search_text="ci seed"
# a SimpleX name candidate: Enter resolves seeded.testing, which backend.sh registered for ci_contact,
# and searches the directory, whose entries are "CI seeded ..."
name_text=seeded
local_chat="seeded notes"
ui=$(dirname "$0")/ui
checks=$UI_OUT/checks.txt
# switches are at the right of the left pane, where Settings open
switch_x=335
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
      echo "### Directory search checks"
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

on_screen() {
  "$ui" find "$1" > /dev/null
}

not_on_screen() {
  ! "$ui" find "$1" > /dev/null 2>&1
}

y_of() {
  local pos
  pos=$("$ui" find "$1") || return 1
  cut -d ' ' -f 2 <<< "$pos"
}

height_of() {
  local x y l t r b
  read -r x y l t r b < <("$ui" find "$1") || return 1
  echo $((b - t))
}

above() {
  local a b
  a=$(y_of "$1") || return 1
  b=$(y_of "$2") || return 1
  [ "$a" -lt "$b" ]
}

# each text is above the next
in_order() {
  local text y previous=-1
  for text in "$@"; do
    y=$(y_of "$text") || return 1
    [ "$y" -gt "$previous" ] || return 1
    previous=$y
  done
}

# the label is in the row headed by the name: below the name, by less than a row's height
in_row_of() {
  local label name
  label=$(y_of "$1") || return 1
  name=$(y_of "$2") || return 1
  [ "$label" -ge "$name" ] && [ $((label - name)) -lt 45 ]
}

# the count's text is smaller than the description's above it: both have capitals and ascenders, no descenders
count_text_smaller() {
  local count description
  count=$(height_of "1 subscriber") || return 1
  description=$(height_of "CI seeded channel") || return 1
  echo "NOTE text heights: \"1 subscriber\" $count px, \"CI seeded channel\" $description px" | tee -a "$checks"
  [ "$count" -lt "$description" ]
}

# Shows more results until both seeded entries are listed: their avatars make each response hold one of them.
# Sets pages and retries.
show_all_results() {
  local layout=$1 missing
  pages=1
  retries=0
  while :; do
    "$ui" wait "Join" 60 || true
    missing=""
    on_screen "#ci_channel" || missing="#ci_channel"
    on_screen "#ci_group" || missing="#ci_group"
    "$ui" shot "$layout-page-$pages"
    [ -n "$missing" ] || break
    if on_screen "Search failed, tap to retry"; then
      retries=$((retries + 1))
      [ "$retries" -le 2 ] || fail "$layout: the search failed $retries times"
      "$ui" click-text "Search failed, tap to retry"
    else
      [ "$pages" -lt 5 ] || fail "$layout: $missing is not in $pages pages of results"
      "$ui" click-text "Show more" 10 || fail "$layout: $missing is not in the results, and there is no Show more"
      pages=$((pages + 1))
      "$ui" wait "$missing" 60 || true
    fi
  done
  echo "NOTE $layout: the results came in $pages pages, after $retries retries" | tee -a "$checks"
}

check_directory_rows() {
  local layout=$1
  check "$layout: the first page held one result, \"Show more\" loaded the other" [ "$pages" -eq 2 ]
  check "$layout: no more results offered" not_on_screen "Show more"
  check "$layout: \"Directory\" header above #ci_group" above "Directory" "#ci_group"
  check "$layout: \"Directory\" header above #ci_channel" above "Directory" "#ci_channel"
  check "$layout: #ci_group shows its red avatar" "$ui" avatar "#ci_group" red
  check "$layout: #ci_channel shows its green avatar" "$ui" avatar "#ci_channel" green
  check "$layout: #ci_group row has \"CI seeded group\"" in_row_of "CI seeded group" "#ci_group"
  check "$layout: #ci_channel row has \"CI seeded channel\"" in_row_of "CI seeded channel" "#ci_channel"
  check "$layout: #ci_group row has \"Join group\"" in_row_of "Join group" "#ci_group"
  check "$layout: #ci_channel row has \"Join channel\"" in_row_of "Join channel" "#ci_channel"
  check "$layout: member count shown" on_screen "2 members"
  check "$layout: subscriber count shown" on_screen "1 subscriber"
}

# Searches the directory for the text in the search field with the "Search in Directory" row.
search_directory() {
  local layout=$1 consent=$2
  "$ui" click-text "Search in Directory"
  "$ui" wait "Search and don't show again" 10
  "$ui" shot "$layout-search-consent"
  "$ui" click-text "$consent"
  show_all_results "$layout-directory"
  check_directory_rows "$layout"
  check "$layout: the count's text is smaller than the description's" count_text_smaller
}

# Types a SimpleX name and presses Enter, the desktop's Search key: the local chat that matches, then the Names part
# with the name's contact, then the Directory part, in the list's order outward from the search field.
full_search() {
  local layout=$1 consent=$2
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$name_text"
  "$ui" wait "Connect to $name_text.testing" 15
  "$ui" shot "$layout-name-typed"
  "$ui" key Return
  if [ -n "$consent" ]; then
    "$ui" wait "Search and don't show again" 10
    "$ui" click-text "$consent"
  fi
  "$ui" wait "ci_contact" 60 || true
  show_all_results "$layout-full"
  check_directory_rows "$layout full search"
  check "$layout full search: \"$local_chat\" listed" on_screen "$local_chat"
  check "$layout full search: \"Names\" header shown" on_screen "Names"
  check "$layout full search: ci_contact found by its name" in_row_of "@$name_text.testing" "ci_contact"
  check "$layout full search: ci_contact row has \"CI seeded contact\"" in_row_of "CI seeded contact" "ci_contact"
  check "$layout full search: ci_contact shows its blue avatar" "$ui" avatar "ci_contact" blue
  check "$layout full search: \"Connect to $name_text.testing\" gone once the name is found" not_on_screen "Connect to $name_text.testing"
  check "$layout full search: \"Names\" header above ci_contact" above "Names" "ci_contact"
  if [ "$layout" = top ]; then
    check "$layout full search: local chat, Names, Directory, from the search field down" \
      in_order "Search in Directory" "$local_chat" "Names" "ci_contact" "Directory" "#ci_channel"
  else
    check "$layout full search: local chat, Names, Directory, from the search field up" \
      in_order "Directory" "#ci_channel" "Names" "ci_contact" "$local_chat" "Search in Directory"
  fi
}

# Escape closes one screen at a time, and clears the search field
back_to_chat_list() {
  for _ in 1 2 3 4 5 6; do
    if not_on_screen "Settings" && not_on_screen "Chat preferences" && not_on_screen "Network & servers" && on_screen "Search or paste SimpleX link"; then
      return 0
    fi
    "$ui" key Escape
  done
  on_screen "Search or paste SimpleX link"
}

# Opens Settings from the user menu, behind the profile picture at the left of the bottom toolbar.
open_settings() {
  "$ui" click 27 730
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
  [ "$y" -lt 200 ] || fail "the search field is still at the bottom (y $y) after switching Reachable app toolbars"
}

# A group of the user's own, so the chat list has a chat that the full search matches and the directory doesn't list.
create_local_chat() {
  local y
  # the pencil at the right of the bottom toolbar opens New chat
  "$ui" click 363 730
  "$ui" click-text "Create group" 10
  "$ui" wait "Enter group name" 10
  y=$(y_of "Enter group name")
  "$ui" click 195 $((y + 40))
  "$ui" type "$local_chat"
  "$ui" shot new-group
  "$ui" click-text "Create group" 10
  "$ui" click-text "Skip inviting members" 20 || true
  "$ui" shot group-created
  "$ui" key Escape
  "$ui" key Escape
  back_to_chat_list || fail "no chat list after creating the group"
  "$ui" wait "$local_chat" 10 || fail "\"$local_chat\" is not in the chat list"
  "$ui" shot local-chat
}

# Settings > Network & servers: the test SMP server added with the names role, and names off for the preset
# operators, so that only the test server resolves names.
use_names_server() {
  local y operator
  open_settings
  "$ui" click-text "Network & servers" 10
  "$ui" click-text "Your servers" 10
  "$ui" click-text "Add server" 10
  "$ui" click-text "Enter server manually" 10
  "$ui" wait "Your server address" 10
  y=$(y_of "Your server address")
  "$ui" click 195 $((y + 50))
  "$ui" type "$NAMES_SERVER"
  y=$(y_of "Use for new connections")
  "$ui" click "$switch_x" "$y"
  "$ui" shot new-server
  "$ui" key Escape
  "$ui" click-text "127.0.0.1" 10
  "$ui" wait "To resolve names" 10
  "$ui" click-text "To resolve names"
  "$ui" click-text "yes" 10
  "$ui" shot names-server-roles
  "$ui" wait "To resolve names" 10
  in_row_of "yes" "To resolve names" || fail "the test server's \"To resolve names\" is not yes"
  "$ui" key Escape
  "$ui" key Escape
  for operator in "SimpleX Chat" Flux; do
    "$ui" click-text "$operator" 10
    "$ui" wait "To resolve names" 10
    y=$(y_of "To resolve names")
    "$ui" click "$switch_x" "$y"
    "$ui" shot "names-off-$(tr -cd 'a-zA-Z' <<< "$operator")"
    "$ui" key Escape
  done
  "$ui" click-text "Save servers" 10
  sleep 2
  "$ui" shot servers-saved
  back_to_chat_list || fail "no chat list after saving the servers"
}

"$ui" start "$app_dir"

# onboarding to the chat list
"$ui" wait "Create your profile" 180
"$ui" shot first-screen
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

: "${NAMES_SERVER:?NAMES_SERVER must be the test SMP server address}"
create_local_chat
use_names_server
if [ "$cases" = setup ]; then
  echo "smoke test setup done: \"$local_chat\" created, $NAMES_SERVER resolves names"
  exit 0
fi

# typed text that is not a SimpleX link offers the directory; the default layout has the toolbar and the search field at the bottom
"$ui" click-text "Search or paste SimpleX link"
"$ui" type "$search_text"
"$ui" wait "$search_text" 15
"$ui" wait "Search in Directory" 15
"$ui" shot bottom-search-typed
search_directory bottom "Search"
"$ui" key Escape
full_search bottom "Search"

# the alert was answered with "Search" both times, so it asks again
use_top_toolbar
"$ui" click-text "Search or paste SimpleX link"
"$ui" type "$search_text"
"$ui" wait "$search_text" 15
"$ui" wait "Search in Directory" 15
"$ui" shot top-search-typed
search_directory top "Search and don't show again"
"$ui" key Escape
full_search top ""

if grep -q '^FAIL' "$checks"; then
  fail "the searches failed checks: $(grep -c '^FAIL' "$checks") of $(grep -cE '^(PASS|FAIL)' "$checks")"
fi
echo "smoke test passed: \"Search in Directory\" for \"$search_text\" and the full search for \"$name_text\" show their parts in order, with the toolbar at the bottom and at the top"
