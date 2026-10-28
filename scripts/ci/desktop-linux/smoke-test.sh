#!/bin/bash

# Starts the desktop app on a virtual display and runs the scripted UI cases with the ui helper: onboarding to the
# chat list, a local group, then the chat list with the toolbar at the bottom (the default) and at the top.
# Usage: scripts/ci/desktop-linux/smoke-test.sh <app dir> <output dir> [start|onboarding|layouts|setup|backend|search]
# With "start" it stops at the first screen, for an agent to go on from; with "onboarding", at the chat list.
# "setup" creates the group, makes the test SMP server (backend.sh) the one that resolves SimpleX names, and stops;
# "backend" does the same, connects to the name that backend.sh registered, and goes on to both layouts.
# "search" also searches in both layouts: the test directory with "Search in Directory", after the alert that enables
# Directory search (Cancel searches nothing, Enable searches; no later search asks), and with Enter, the Search key,
# which searches the directory too and leaves "Connect to …" for the typed name; with the toolbar at the bottom, also
# with Enter in the New chat sheet, and a right click on a result opens its menu with Open. Then it joins ci_channel
# through its result's Open, which opens the prepared channel without an alert, and checks, in both layouts, that a page
# of chats already listed adds nothing and that ci_channel, when the list doesn't show it, is its chat's row among the
# results, opening it without an alert; and, with the toolbar at the bottom, that it is a chat row in New chat too.
# Restarted with no chat open, a text that matches nothing offers the directory, and "No chats found" shows only once
# the directory found nothing too.
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
# matches no chat and no listing
no_match_text=zqxvjk
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

not_on_screen_exact() {
  ! left_pane_position "$1" exact > /dev/null
}

# Prints "x y" of the "Open" in the result row headed by the name, as in_row_of places a label: each result has one,
# so the first "Open" on screen can be another row's.
open_in_row_of() {
  local name line x y
  name=$(y_of "$1") || return 1
  while IFS= read -r line; do
    x=${line%%,*}
    y=${line#*,}
    y=${y%% *}
    if [ "$x" -lt 390 ] && [ "$(normalise "${line#* }")" = open ] && [ "$y" -ge "$name" ] && [ $((y - name)) -lt 45 ]; then
      echo "$x $y"
      return 0
    fi
  done < <("$ui" text)
  return 1
}

# how many phrases are the text in the chat list pane (left), the open chat's pane (right) or anywhere (all)
count_phrases() {
  local pane=$1 target line x n=0
  target=$(normalise "$2")
  while IFS= read -r line; do
    x=${line%%,*}
    if [ "$pane" = all ] || { [ "$pane" = left ] && [ "$x" -lt 390 ]; } || { [ "$pane" = right ] && [ "$x" -ge 390 ]; }; then
      if [ "$(normalise "${line#* }")" = "$target" ]; then
        n=$((n + 1))
      fi
    fi
  done < <("$ui" text)
  echo "$n"
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

# Loads more results until both seeded entries are listed: their avatars make each response hold one of them.
# The load row's text after the first page is recorded before its shot, in load_more_shown and one_left_shown:
# OCR may misread its middle dot, so "Load more" and "1 left" are read separately. Sets pages and retries.
show_all_results() {
  local layout=$1 missing
  pages=1
  retries=0
  load_more_shown=false
  one_left_shown=false
  while :; do
    "$ui" wait "Open" 60 || true
    missing=""
    on_screen "#ci_channel" || missing="#ci_channel"
    on_screen "#ci_group" || missing="#ci_group"
    if [ "$pages" -eq 1 ] && [ -n "$missing" ]; then
      if on_screen "Load more"; then
        load_more_shown=true
      fi
      if on_screen "1 left"; then
        one_left_shown=true
      fi
    fi
    "$ui" shot "$layout-page-$pages"
    [ -n "$missing" ] || break
    if on_screen "Search failed, tap to retry"; then
      retries=$((retries + 1))
      [ "$retries" -le 2 ] || fail "$layout: the search failed $retries times"
      "$ui" click-text "Search failed, tap to retry"
    else
      [ "$pages" -lt 5 ] || fail "$layout: $missing is not in $pages pages of results"
      "$ui" click-text "Load more" 10 || fail "$layout: $missing is not in the results, and there is no Load more"
      pages=$((pages + 1))
      "$ui" wait "$missing" 60 || true
    fi
  done
  echo "NOTE $layout: the results came in $pages pages, after $retries retries" | tee -a "$checks"
}

check_directory_rows() {
  local layout=$1
  check "$layout: the first page held one result, \"Load more\" loaded the other" [ "$pages" -eq 2 ]
  check "$layout: after the first page the load row said \"Load more\"" [ "$load_more_shown" = true ]
  check "$layout: after the first page the load row said \"1 left\"" [ "$one_left_shown" = true ]
  check "$layout: no more results offered" not_on_screen "Load more"
  check "$layout: no \"Directory\" header" not_on_screen_exact "Directory"
  check "$layout: #ci_group shows its red avatar" "$ui" avatar "#ci_group" red
  check "$layout: #ci_channel shows its green avatar" "$ui" avatar "#ci_channel" green
  check "$layout: #ci_group row has \"CI seeded group\"" in_row_of "CI seeded group" "#ci_group"
  check "$layout: #ci_channel row has \"CI seeded channel\"" in_row_of "CI seeded channel" "#ci_channel"
  check "$layout: #ci_group row has \"Open\"" open_in_row_of "#ci_group"
  check "$layout: #ci_channel row has \"Open\"" open_in_row_of "#ci_channel"
  check "$layout: member count shown" on_screen "2 members"
  check "$layout: subscriber count shown" on_screen "1 subscriber"
}

# The first search asks to enable Directory search: Cancel searches nothing, Enable turns it on and searches.
enable_directory_search() {
  "$ui" click-text "Search in Directory"
  "$ui" wait "Enable Directory search" 10
  "$ui" shot search-consent
  "$ui" click-text "Cancel"
  sleep 2
  check "Cancel in the alert searches nothing" not_on_screen_exact "Open"
  check "\"Search in Directory\" still shown after Cancel" on_screen "Search in Directory"
  "$ui" shot search-cancelled
  "$ui" click-text "Search in Directory"
  "$ui" wait "Enable Directory search" 10
  "$ui" click-text "Enable"
}

# Directory search was enabled at the first search, so no later one may ask
no_consent_alert() {
  ! "$ui" find "Enable Directory search" > /dev/null 2>&1
}

# A right click on a result opens its menu, which offers Open; a click elsewhere closes it (Escape doesn't close the
# chat list's menus on desktop).
result_menu() {
  local layout=$1 pos opens
  pos=$(left_pane_position "#ci_group") || { check "$layout: #ci_group shown for its menu" false; return; }
  opens=$(count_phrases all Open)
  "$ui" right-click "${pos% *}" "${pos#* }"
  check "$layout: a right click on #ci_group opens a menu with \"Open\"" [ "$(count_phrases all Open)" -gt "$opens" ]
  "$ui" shot "$layout-result-menu"
  "$ui" click 900 450
  sleep 1
  check "$layout: a click elsewhere closes the result's menu" [ "$(count_phrases all Open)" -eq "$opens" ]
}

# Searches the directory for the text in the search field with the "Search in Directory" row; the first search
# enables Directory search.
search_directory() {
  local layout=$1
  if [ "$layout" = bottom ]; then
    enable_directory_search
  else
    "$ui" click-text "Search in Directory"
    sleep 2
    check "$layout: no alert before the search" no_consent_alert
  fi
  show_all_results "$layout-directory"
  check_directory_rows "$layout"
  check "$layout: no \"Search in Directory\" while the results show" not_on_screen "Search in Directory"
  check "$layout: the count's text is smaller than the description's" count_text_smaller
  cards_hidden "$layout, directory results"
  if [ "$layout" = bottom ]; then
    result_menu "$layout"
  fi
  clear_search_and_check_cards "$layout directory search"
}

# Types a SimpleX name and presses Enter, the desktop's Search key: it searches the directory only, so the local chat
# that matches is nearest the search field, then the directory's results; "Connect to …" stays and nothing is resolved.
full_search() {
  local layout=$1
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$name_label"
  "$ui" wait "Connect to $name_label.testing" 15
  "$ui" shot "$layout-name-typed"
  "$ui" key Return
  sleep 2
  check "$layout full search: no alert before the search" no_consent_alert
  show_all_results "$layout-full"
  check_directory_rows "$layout full search"
  check "$layout full search: \"$local_chat\" listed" on_screen "$local_chat"
  check "$layout full search: no \"Names\" header" not_on_screen_exact "Names"
  check "$layout full search: no ci_contact row, as the name is not resolved" not_on_screen "ci_contact"
  check "$layout full search: \"Connect to $name_label.testing\" stays" on_screen "Connect to $name_label.testing"
  if [ "$layout" = top ]; then
    check "$layout full search: \"Connect to …\", the local chat, then the results, from the search field down" \
      in_order "Connect to $name_label.testing" "$local_chat" "#ci_group"
    check "$layout full search: #ci_channel below the local chat" above "$local_chat" "#ci_channel"
  else
    check "$layout full search: the results, the local chat, then \"Connect to …\", from the search field up" \
      in_order "#ci_group" "$local_chat" "Connect to $name_label.testing"
    check "$layout full search: #ci_channel above the local chat" above "#ci_channel" "$local_chat"
  fi
  cards_hidden "$layout, full search results"
  clear_search_and_check_cards "$layout full search"
}

# With chats in the list and none open, a text that matches no chat offers the directory; "No chats found" waits for
# its search to find nothing.
no_match_search() {
  local layout=$1
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$no_match_text"
  "$ui" wait "Search in Directory" 15 || true
  "$ui" shot "$layout-no-match-typed"
  check "$layout: \"Search in Directory\" offered for \"$no_match_text\"" on_screen "Search in Directory"
  check "$layout: no \"No chats found\" before the directory search" not_on_screen "No chats found"
  "$ui" click-text "Search in Directory"
  if ! "$ui" wait "No chats found" 30 && on_screen "Search failed, tap to retry"; then
    "$ui" click-text "Search failed, tap to retry"
    "$ui" wait "No chats found" 30 || true
  fi
  "$ui" shot "$layout-no-match-searched"
  check "$layout: \"No chats found\" once the directory search found nothing" on_screen "No chats found"
  clear_search_field "$layout"
}

# The same Search key from the New chat sheet: one group of results, the contacts that match first, with Open on the results.
new_chat_sheet_search() {
  # the pencil at the right of the bottom toolbar opens New chat; the chat list behind it has the same search field
  "$ui" click 363 730
  "$ui" wait "Create group" 10
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$name_label"
  "$ui" wait "Connect to $name_label.testing" 15
  "$ui" key Return
  sleep 2
  check "New chat: no alert before the search" no_consent_alert
  show_all_results new-chat
  check "New chat: the first page held one result, \"Load more\" loaded the other" [ "$pages" -eq 2 ]
  check "New chat: no more results offered" not_on_screen "Load more"
  check "New chat: no \"Names\" header" not_on_screen_exact "Names"
  check "New chat: no \"Directory\" header" not_on_screen_exact "Directory"
  check "New chat: #ci_group row has \"Open\"" open_in_row_of "#ci_group"
  check "New chat: #ci_channel row has \"Open\"" open_in_row_of "#ci_channel"
  check "New chat: \"Connect to $name_label.testing\" stays" on_screen "Connect to $name_label.testing"
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

# Opens the prepared channel, read before its shot: it has "Tap Join channel" and its subscriber count in the toolbar and
# the banner. Sets alert_shown when a read on the way found the "Open channel" alert, which Open must not show.
wait_prepared_channel() {
  local deadline=$((SECONDS + 30)) screen
  alert_shown=false
  prepared_subscribers=0
  while :; do
    screen=$("$ui" text)
    if grep -qiF "open channel" <<< "$screen"; then
      alert_shown=true
    fi
    if grep -qiF "tap join channel" <<< "$screen"; then
      local line
      while IFS= read -r line; do
        if [ "${line%%,*}" -ge 390 ] && grep -qiE '[0-9]+ subscribers?' <<< "$line"; then
          prepared_subscribers=$((prepared_subscribers + 1))
        fi
      done <<< "$screen"
      return 0
    fi
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
}

# Joins ci_channel through its result's Open, with the toolbar at the top, so that it is a chat the later searches find.
join_directory_channel() {
  local pos
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  show_all_results top-join
  pos=$(open_in_row_of "#ci_channel") || fail "no \"Open\" in #ci_channel's row"
  "$ui" click "${pos% *}" "${pos#* }"
  check "top: Open on #ci_channel opens the prepared channel" wait_prepared_channel
  check "top: Open on #ci_channel shows no \"Open channel\" alert" [ "$alert_shown" = false ]
  check "top: the prepared channel shows its subscriber count in the toolbar and the banner" [ "$prepared_subscribers" -ge 2 ]
  "$ui" shot top-prepared-channel
  click_in_chat "Join channel" || fail "no \"Join channel\" in the opened channel"
  check "top: ci_channel joined through Open" "$ui" wait "you are subscriber" 60
  "$ui" shot top-joined-channel
  clear_search_field top
}

# With ci_channel joined, "ci seed" finds it on the first page, as its chat row, and #ci_group on the second.
show_known_results() {
  local what=$1
  if ! wait_in_list "Load more" && on_screen "Search failed, tap to retry"; then
    "$ui" click-text "Search failed, tap to retry"
    wait_in_list "Load more" || true
  fi
  "$ui" shot "$what-page-1"
  "$ui" click-text "Load more" 10 || fail "$what: no \"Load more\" after the first page"
  "$ui" wait "#ci_group" 60 || true
  "$ui" shot "$what-page-2"
}

# A directory result that is a chat the user has. Typed `ci_channel`, it is listed as the chat open in the right pane,
# so the page adds no row and no note. With "seeded notes" open and `ci seed` typed, the list doesn't
# show it, so it is its chat row among the results, and clicking it opens it.
known_channel_searches() {
  local layout=$1
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type ci_channel
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  # nothing marks the end of a search that adds no row: wait it out (once the directory's link is fetched, the request
  # fails after 10 s); "Search in Directory" hides once a search for the text runs
  sleep 20
  if on_screen "Search failed, tap to retry"; then
    "$ui" click-text "Search failed, tap to retry"
    sleep 20
  fi
  "$ui" shot "$layout-known-listed"
  check "$layout: the search for the listed chat ran" not_on_screen "Search in Directory"
  check "$layout: the search for the listed chat didn't fail" not_on_screen "Search failed, tap to retry"
  check "$layout: a page of chats already listed adds no note" not_on_screen "All results on this page"
  check "$layout: no #ci_channel result next to the listed chat" no_ci_channel_directory_row
  check "$layout: no \"Open\" next to the listed chat" not_on_screen_exact "Open"
  clear_search_field "$layout"
  click_in_list "$local_chat"
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  show_known_results "$layout-known"
  check "$layout: ci_channel is listed among the results" on_screen "ci_channel"
  check "$layout: ci_channel is its chat's row, not #ci_channel" no_ci_channel_directory_row
  check "$layout: only #ci_group has \"Open\", not ci_channel" [ "$(count_phrases left Open)" -eq 1 ]
  check "$layout: #ci_group row has \"Open\"" open_in_row_of "#ci_group"
  click_in_list ci_channel
  sleep 2
  "$ui" shot "$layout-known-opened"
  check "$layout: clicking ci_channel's row shows no alert" no_subscriber_alert
  check "$layout: clicking ci_channel's row opens the channel" "$ui" wait "you are subscriber" 15
  clear_search_field "$layout"
}

# New chat lists only contacts, so there too the joined channel is a chat row among the results.
new_chat_known_channel() {
  "$ui" click 363 730
  "$ui" wait "Create group" 10
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "Search in Directory" 15
  "$ui" click-text "Search in Directory"
  show_known_results new-chat-known
  check "New chat: ci_channel is listed among the results" on_screen "ci_channel"
  check "New chat: ci_channel is its chat's row, not #ci_channel" no_ci_channel_directory_row
  check "New chat: only #ci_group has \"Open\", not ci_channel" [ "$(count_phrases left Open)" -eq 1 ]
  check "New chat: #ci_group row has \"Open\"" open_in_row_of "#ci_group"
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
  search_directory bottom
  full_search bottom
  new_chat_sheet_search
fi
switch_toolbar top
check_layout top
if [ "$cases" = search ]; then
  "$ui" click-text "Search or paste SimpleX link"
  "$ui" type "$search_text"
  "$ui" wait "$search_text" 15
  "$ui" wait "Search in Directory" 15
  "$ui" shot top-search-typed
  cards_while_typing top
  search_directory top
  full_search top
  join_directory_channel
  known_channel_searches top
  switch_toolbar bottom
  known_channel_searches bottom
  new_chat_known_channel
  # the chat list always shows the chat open in the right pane, and only a restart closes it
  "$ui" stop
  "$ui" start "$app_dir"
  "$ui" wait "Search or paste SimpleX link" 120 || fail "no chat list after restarting the app"
  no_match_search bottom
fi

if grep -q '^FAIL' "$checks"; then
  fail "the smoke test failed checks: $(grep -c '^FAIL' "$checks") of $(grep -cE '^(PASS|FAIL)' "$checks")"
fi
if [ "$cases" = search ]; then
  echo "smoke test passed: onboarding reached the chat list with the toolbar at the bottom and at the top, the first search enabled Directory search, the searches showed one list of results with Open and the load row, \"No chats found\" waited for a directory search that found nothing, Open opened the prepared ci_channel without an alert, and the joined ci_channel is a chat row among the results (chat list in both layouts, New chat), and a page of chats already listed adds nothing"
  exit 0
fi
echo "smoke test passed: onboarding reached the chat list, which shows \"$local_chat\" with the toolbar at the bottom and at the top"
