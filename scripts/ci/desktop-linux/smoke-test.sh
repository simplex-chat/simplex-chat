#!/bin/bash

# Starts the desktop app on a virtual display and runs the scripted UI cases with the ui helper: onboarding to the
# chat list, a local group, then the chat list with the toolbar at the bottom (the default) and at the top.
# Usage: scripts/ci/desktop-linux/smoke-test.sh <app dir> <output dir> [start|onboarding|layouts|setup|backend|search]
# With "start" it stops at the first screen, for an agent to go on from; with "onboarding", at the chat list.
# "setup" creates the group, makes the test SMP server (backend.sh) the one that resolves SimpleX names, and stops;
# "backend" does the same, connects to the name that backend.sh registered, and goes on to both layouts.
# "search" also searches in both layouts: the test directory with "Search in Directory", and the full search with
# Enter, which also resolves a SimpleX name; with the toolbar at the bottom, also the full search in the New chat sheet.
# Then it joins ci_channel from its Directory row and checks, in both layouts, that a page of chats already listed
# says so and that ci_channel, when the list doesn't show it, is its chat's row in the Directory part, opening it
# without an alert; and, with the toolbar at the bottom, that it is a chat row in the New chat sheet too.
# These three need NAMES_SERVER, the test SMP server's address.
# Each step saves a numbered screenshot and the text read from it to the output dir, prints the text to the log,
# and adds a line to steps.log there. What the screens show is checked without stopping: every check is listed in
# checks.txt and the job summary, and the test fails at the end if any failed. The app keeps running for later
# steps of the job, which get UI_OUT.

set -euo pipefail

app_dir=$1
export UI_OUT=$2
cases=${3:-layouts}
case "$cases" in
  start | onboarding | layouts | setup | backend | search) ;;
  *)
    echo "error: unknown case \"$cases\": start, onboarding, layouts (the default), setup, backend or search"
    exit 1
    ;;
esac
search_text="ci seed"
# backend.sh registered seeded.testing for ci_contact; as a SimpleX name candidate, Enter also searches the directory,
# whose entries are "CI seeded ..."
name_label=seeded
local_chat="seeded notes"
ui=$(dirname "$0")/ui
checks=$UI_OUT/checks.txt
# the Wefunder banner, the reachable toolbars card and the connect cards, which the chat list hides while it shows online results
cards=("Invest in SimpleX Chat" "Top bar" "New 1-time link")
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
    [ "$x" -lt 390 ] || continue
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
    if y=$(y_of "$1") && [ "$y" -lt 640 ]; then
      return 0
    fi
    "$ui" scroll 195 400 down
  done
  return 1
}

# the count's text is smaller than the description's above it: both have capitals and ascenders, no descenders
count_text_smaller() {
  local count description
  count=$("$ui" ink "1 subscriber") || return 1
  description=$("$ui" ink "CI seeded channel") || return 1
  echo "NOTE ink heights: \"1 subscriber\" $count px, \"CI seeded channel\" $description px" | tee -a "$checks"
  [ "$count" -lt "$description" ]
}

# the phrases in the chat list pane, left of x 390, one per line, normalised; the right pane can show a banner too
left_pane_text() {
  local line
  "$ui" text | while IFS= read -r line; do
    if [ "${line%%,*}" -lt 390 ]; then
      normalise "${line#* }"
      echo
    fi
  done
}

not_in() {
  ! grep -qF "$(normalise "$2")" <<< "$1"
}

is_in() {
  grep -qF "$(normalise "$2")" <<< "$1"
}

# Records which of the cards the chat list shows while the typed text only filters it, in shown_cards.
cards_while_typing() {
  local layout=$1 screen card
  screen=$(left_pane_text)
  shown_cards=()
  for card in "${cards[@]}"; do
    if is_in "$screen" "$card"; then
      shown_cards+=("$card")
    fi
  done
  echo "NOTE $layout: while typing, the chat list shows: ${shown_cards[*]:-none of the cards}" | tee -a "$checks"
  check "$layout: banner or cards shown while typing" [ ${#shown_cards[@]} -gt 0 ]
}

cards_hidden() {
  local what=$1 screen card
  screen=$(left_pane_text)
  for card in "${shown_cards[@]}"; do
    check "$what: \"$card\" hidden" not_in "$screen" "$card"
  done
}

# Escape clears the search field
clear_search_and_check_cards() {
  local what=$1 screen card
  "$ui" key Escape
  "$ui" wait "Search or paste SimpleX link" 10 || true
  "$ui" shot "$(tr -cd 'a-z-' <<< "${what// /-}")-cleared"
  screen=$(left_pane_text)
  for card in "${shown_cards[@]}"; do
    check "$what: \"$card\" back after clearing the search" is_in "$screen" "$card"
  done
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
  cards_hidden "$layout, directory results"
  clear_search_and_check_cards "$layout directory search"
}

# Types a SimpleX name and presses Enter, the desktop's Search key: the local chat that matches, then the Names part
# with the name's contact, then the Directory part, in the list's order outward from the search field.
full_search() {
  local layout=$1 consent=$2
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$name_label"
  "$ui" wait "Connect to $name_label.testing" 15
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
  check "$layout full search: ci_contact found by its name" in_row_of "@$name_label.testing" "ci_contact"
  check "$layout full search: ci_contact row has \"CI seeded contact\"" in_row_of "CI seeded contact" "ci_contact"
  check "$layout full search: ci_contact shows its blue avatar" "$ui" avatar "ci_contact" blue
  check "$layout full search: \"Connect to $name_label.testing\" gone once the name is found" not_on_screen "Connect to $name_label.testing"
  check "$layout full search: \"Names\" header above ci_contact" above "Names" "ci_contact"
  if [ "$layout" = top ]; then
    check "$layout full search: local chat, Names, Directory, from the search field down" \
      in_order "Search in Directory" "$local_chat" "Names" "ci_contact" "Directory" "#ci_channel"
  else
    check "$layout full search: local chat, Names, Directory, from the search field up" \
      in_order "Directory" "#ci_channel" "Names" "ci_contact" "$local_chat" "Search in Directory"
  fi
  cards_hidden "$layout, full search results"
  clear_search_and_check_cards "$layout full search"
}

# The same full search from the New chat sheet, whose list is reversed too with the toolbar at the bottom.
new_chat_sheet_search() {
  local consent=$1
  # the pencil at the right of the bottom toolbar opens New chat; the chat list behind it has the same search field
  "$ui" click 363 730
  "$ui" wait "Create group" 10
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$name_label"
  "$ui" wait "Connect to $name_label.testing" 15
  "$ui" key Return
  "$ui" wait "Search and don't show again" 10
  "$ui" click-text "$consent"
  "$ui" wait "ci_contact" 60 || true
  show_all_results new-chat
  check "New chat: the first page held one result, \"Show more\" loaded the other" [ "$pages" -eq 2 ]
  check "New chat: no more results offered" not_on_screen "Show more"
  check "New chat: \"Names\" header above ci_contact" above "Names" "ci_contact"
  check "New chat: \"Directory\" header above #ci_group" above "Directory" "#ci_group"
  check "New chat: \"Directory\" header above #ci_channel" above "Directory" "#ci_channel"
  check "New chat: Names, then Directory, from the search field up" \
    in_order "Directory" "#ci_channel" "Names" "ci_contact" "Search in Directory"
  # with the toolbar at the bottom Escape only clears the sheet's search; its back arrow closes it
  "$ui" key Escape
  "$ui" wait "Create group" 10
  "$ui" click 27 730
  back_to_chat_list || fail "no chat list after the New chat sheet"
}

# OCR reads a Directory row's name with its "#", which normalise drops, and a chat's row without it
no_ci_channel_directory_row() {
  local line
  while IFS= read -r line; do
    if [ "${line%%,*}" -lt 390 ] && grep -qE '#ci_ ?chan' <<< "${line#* }"; then
      return 1
    fi
  done < <("$ui" text)
}

# the alert of a directory entry the user has, which a chat's own row doesn't show
no_subscriber_alert() {
  ! "$ui" find "You are a subscriber" > /dev/null 2>&1
}

# waits up to 60 s for the text in the chat list pane
wait_in_list() {
  local deadline=$((SECONDS + 60))
  until on_screen "$1"; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
}

# clicks the text in the chat list pane
click_in_list() {
  local pos
  pos=$(left_pane_position "$1") || fail "\"$1\" is not in the chat list pane"
  "$ui" click "${pos% *}" "${pos#* }"
}

# Clicks the lowest text in the right pane, where the open chat is: its bars are at the bottom, below its messages.
click_in_chat() {
  local target line x y found=""
  target=$(normalise "$1")
  while IFS= read -r line; do
    x=${line%%,*}
    y=${line#*,}
    y=${y%% *}
    [ "$x" -ge 390 ] || continue
    case "$(normalise "${line#* }")" in
      *"$target"*) found="$x $y" ;;
    esac
  done < <("$ui" text)
  [ -n "$found" ] || return 1
  "$ui" click "${found% *}" "${found#* }"
}

# Selects the search field's text and deletes it: Escape would go to the open chat when it has the keyboard focus.
clear_search_field() {
  local y=104
  [ "$1" = bottom ] && y=674
  "$ui" click 200 "$y"
  "$ui" key ctrl+a BackSpace
  "$ui" wait "Search or paste SimpleX link" 10 || fail "$1: the search field is not empty"
}

# Joins ci_channel from its Directory row, with the toolbar at the top, so that it is a chat the later searches find.
join_directory_channel() {
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  show_all_results top-join
  "$ui" click-text "#ci_channel"
  "$ui" click-text "Open channel" 30
  "$ui" wait "Tap Join channel" 30
  click_in_chat "Join channel" || fail "no \"Join channel\" in the opened channel"
  check "top: ci_channel joined from its Directory row" "$ui" wait "you are subscriber" 60
  "$ui" shot top-joined-channel
  clear_search_field top
}

# With ci_channel joined, "ci seed" finds it on the first page, as its chat row, and #ci_group on the second.
show_known_results() {
  local what=$1
  if ! wait_in_list "Show more" && on_screen "Search failed, tap to retry"; then
    "$ui" click-text "Search failed, tap to retry"
    wait_in_list "Show more" || true
  fi
  "$ui" shot "$what-page-1"
  "$ui" click-text "Show more" 10 || fail "$what: no \"Show more\" after the first page"
  "$ui" wait "#ci_group" 60 || true
  "$ui" shot "$what-page-2"
}

# A directory result that is a chat the user has. Typed `ci_channel`, it is listed as the chat open in the right pane,
# so the page added no row and the Directory part says so. With "seeded notes" open and `ci seed` typed, the list
# doesn't show it, so it is its chat row in the Directory part, and clicking it opens it.
known_channel_searches() {
  local layout=$1
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type ci_channel
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  # the note is a long line, which OCR can split after its first words
  wait_in_list "All results on this page" || true
  "$ui" shot "$layout-known-listed"
  check "$layout: a page of chats already listed says so" on_screen "All results on this page"
  check "$layout: no #ci_channel Directory row next to the listed chat" no_ci_channel_directory_row
  check "$layout: no \"Join channel\" next to the listed chat" not_on_screen "Join channel"
  clear_search_field "$layout"
  click_in_list "$local_chat"
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  show_known_results "$layout-known"
  check "$layout: ci_channel is a row of the Directory part" above "Directory" "ci_channel"
  check "$layout: ci_channel is its chat's row, not #ci_channel" no_ci_channel_directory_row
  check "$layout: ci_channel has no \"Join channel\"" not_on_screen "Join channel"
  check "$layout: #ci_group row keeps \"Join group\"" in_row_of "Join group" "#ci_group"
  click_in_list ci_channel
  sleep 2
  "$ui" shot "$layout-known-opened"
  check "$layout: clicking ci_channel's row shows no alert" no_subscriber_alert
  check "$layout: clicking ci_channel's row opens the channel" "$ui" wait "you are subscriber" 15
  clear_search_field "$layout"
}

# New chat lists only contacts, so there too the joined channel is a chat row in the Directory part.
new_chat_known_channel() {
  "$ui" click 363 730
  "$ui" wait "Create group" 10
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  show_known_results new-chat-known
  check "New chat: ci_channel is a row of the Directory part" above "Directory" "ci_channel"
  check "New chat: ci_channel is its chat's row, not #ci_channel" no_ci_channel_directory_row
  check "New chat: ci_channel has no \"Join channel\"" not_on_screen "Join channel"
  check "New chat: #ci_group row keeps \"Join group\"" in_row_of "Join group" "#ci_group"
  # with the toolbar at the bottom Escape only clears the sheet's search; its back arrow closes it
  "$ui" key Escape
  "$ui" wait "Create group" 10
  "$ui" click 27 730
  back_to_chat_list || fail "no chat list after the New chat sheet"
}

# Escape closes one screen at a time, and clears the search field
back_to_chat_list() {
  for _ in 1 2 3 4 5 6; do
    on_chat_list && return 0
    "$ui" key Escape
  done
  on_chat_list
}

# New chat has the same search field
on_chat_list() {
  not_on_screen "Settings" && not_on_screen "Chat preferences" && not_on_screen "Network" && not_on_screen "New chat" && on_screen "Search or paste SimpleX link"
}

# Opens Settings from the user menu, behind the profile picture at the left of the toolbar: at the bottom (y 730) unless
# the toolbar is at the top (y 48).
open_settings() {
  "$ui" click 27 "${1:-730}"
  "$ui" click-text "Settings" 10
}

# Switches "Reachable app toolbars" in Settings > Appearance, which moves the toolbar and the search field to the top
# (off) or the bottom (on).
switch_toolbar() {
  local to=$1 y
  "$ui" key Escape
  if [ "$to" = top ]; then
    open_settings 730
  else
    open_settings 48
  fi
  "$ui" click-text "Appearance" 10
  "$ui" wait "Reachable app toolbars" 10
  "$ui" shot appearance
  y=$(y_of "Reachable app toolbars")
  "$ui" click "$switch_x" "$y"
  "$ui" shot appearance-toolbar-switched
  back_to_chat_list || true
  "$ui" shot "$to-chat-list"
  y=$(y_of "Search or paste SimpleX link") || fail "no search field after closing Settings"
  if [ "$to" = top ]; then
    [ "$y" -lt 200 ] || fail "the search field is still at the bottom (y $y) after switching Reachable app toolbars"
  else
    [ "$y" -gt 600 ] || fail "the search field is still at the top (y $y) after switching Reachable app toolbars"
  fi
}

# The search field and "Add list" next to it: at the bottom of the chat list, "Add list" above the field; at the top, below it.
check_layout() {
  local layout=$1 field
  field=$(y_of "Search or paste SimpleX link") || field=""
  "$ui" shot "$layout-layout"
  check "$layout: \"$local_chat\" listed" on_screen "$local_chat"
  if [ "$layout" = bottom ]; then
    check "bottom: the search field is at the bottom" [ "${field:-0}" -gt 600 ]
    check "bottom: \"Add list\" above the search field" above "Add list" "Search or paste SimpleX link"
  else
    check "top: the search field is at the top" [ "${field:-900}" -lt 200 ]
    check "top: \"Add list\" below the search field" above "Search or paste SimpleX link" "Add list"
  fi
}

# A group of the user's own: without chats the chat list shows no search field.
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
# The first item of the dropdown opened from the line at y $2, which repeats that line's value below it.
menu_first_item() {
  local target line x y deadline=$((SECONDS + 10))
  target=$(normalise "$1")
  while [ "$SECONDS" -lt "$deadline" ]; do
    while IFS= read -r line; do
      x=${line%%,*}
      y=${line#*,}
      y=${y%% *}
      if [ "$x" -lt 390 ] && [ "$y" -gt $(($2 + 20)) ] && [ "$(normalise "${line#* }")" = "$target" ]; then
        echo "$x $y"
        return 0
      fi
    done < <("$ui" text)
    sleep 1
  done
  return 1
}

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
  "$ui" click 195 $((y + 50))
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
  # this menu opens over the address's QR code, where OCR can miss "yes", so it is the row under the menu's first item
  y=$(y_of "To resolve names")
  "$ui" click-text "default (no)" 10
  read -r x y < <(menu_first_item "default (no)" "$y") || fail "no menu for the test server's \"To resolve names\""
  "$ui" click "$x" $((y + 48))
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

# private routing to unknown servers only: the preset servers that would proxy name lookups can't reach the test SMP
# server on 127.0.0.1; developer tools at the debug level put the app's debug log in app.log
"$ui" start "$app_dir" NetworkSMPProxyMode=Unknown DeveloperTools=true LogLevel=DEBUG

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
if [ "$cases" = setup ] || [ "$cases" = backend ] || [ "$cases" = search ]; then
  : "${NAMES_SERVER:?NAMES_SERVER must be the test SMP server address}"
  use_names_server
  if [ "$cases" = setup ]; then
    echo "smoke test setup done: \"$local_chat\" created, $NAMES_SERVER resolves names"
    exit 0
  fi
  connect_by_name
fi

check_layout bottom
# typed text that is not a SimpleX link offers the directory
if [ "$cases" = search ]; then
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "$search_text" 15
  "$ui" wait "Search in Directory" 15
  "$ui" shot bottom-search-typed
  cards_while_typing bottom
  search_directory bottom "Search"
  full_search bottom "Search"
  new_chat_sheet_search "Search"
fi
switch_toolbar top
check_layout top
# the alert was answered with "Search" both times, so it asks again
if [ "$cases" = search ]; then
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "$search_text" 15
  "$ui" wait "Search in Directory" 15
  "$ui" shot top-search-typed
  cards_while_typing top
  search_directory top "Search and don't show again"
  full_search top ""
  join_directory_channel
  known_channel_searches top
  switch_toolbar bottom
  known_channel_searches bottom
  new_chat_known_channel
fi

if grep -q '^FAIL' "$checks"; then
  fail "the smoke test failed checks: $(grep -c '^FAIL' "$checks") of $(grep -cE '^(PASS|FAIL)' "$checks")"
fi
if [ "$cases" = search ]; then
  echo "smoke test passed: onboarding reached the chat list with the toolbar at the bottom and at the top, the directory and name searches showed their parts, and the joined ci_channel is a chat row in the Directory part (chat list in both layouts, New chat), with the note for a page of chats already listed"
  exit 0
fi
echo "smoke test passed: onboarding reached the chat list, which shows \"$local_chat\" with the toolbar at the bottom and at the top"
