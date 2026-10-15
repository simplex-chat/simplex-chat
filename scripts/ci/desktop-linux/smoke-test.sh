#!/bin/bash

# Starts the desktop app on a virtual display and runs the scripted UI cases with the ui helper:
# onboarding to the chat list, and a search of the test directory (backend.sh) from the chat list search field.
# Usage: scripts/ci/desktop-linux/smoke-test.sh <app dir> <output dir> [onboarding]
# With "onboarding" it stops at the chat list: releases before directory search show no search field without chats.
# Each step saves a numbered screenshot and the text read from it to the output dir, prints the text to the log,
# and adds a line to steps.log there. What the search shows is checked without stopping: every check is listed in
# checks.txt and the job summary, and the test fails at the end if any failed. The app keeps running for later
# steps of the job, which get UI_OUT.

set -euo pipefail

app_dir=$1
export UI_OUT=$2
cases=${3:-all}
search_text="ci seed"
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

above() {
  local a b
  a=$(y_of "$1") || return 1
  b=$(y_of "$2") || return 1
  [ "$a" -lt "$b" ]
}

# the label is in the row headed by the name: below the name, by less than a row's height
in_row_of() {
  local label name
  label=$(y_of "$1") || return 1
  name=$(y_of "$2") || return 1
  [ "$label" -ge "$name" ] && [ $((label - name)) -lt 45 ]
}

# Searches the directory for the text in the search field, and shows more results until both seeded ones are there:
# their avatars make each response hold one of them.
search_directory() {
  local layout=$1 consent=$2 pages=1 retries=0 missing
  "$ui" click-text "Search in Directory"
  "$ui" wait "Search and don't show again" 10
  "$ui" shot "$layout-search-consent"
  "$ui" click-text "$consent"
  while :; do
    "$ui" wait "Join" 60 || true
    missing=""
    on_screen "#ci_channel" || missing="#ci_channel"
    on_screen "#ci_group" || missing="#ci_group"
    "$ui" shot "$layout-directory-page-$pages"
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
  check "$layout: the first page held one result, \"Show more\" loaded the other" [ "$pages" -eq 2 ]
  check "$layout: no more results offered" not_on_screen "Show more"
  check "$layout: \"Directory\" header above #ci_group" above "Directory" "#ci_group"
  check "$layout: \"Directory\" header above #ci_channel" above "Directory" "#ci_channel"
  check "$layout: #ci_group shows its red avatar" "$ui" avatar "#ci_group" red
  check "$layout: #ci_channel shows its green avatar" "$ui" avatar "#ci_channel" green
  check "$layout: #ci_group row has \"CI seed group\"" in_row_of "CI seed group" "#ci_group"
  check "$layout: #ci_channel row has \"CI seed channel\"" in_row_of "CI seed channel" "#ci_channel"
  check "$layout: #ci_group row has \"Join group\"" in_row_of "Join group" "#ci_group"
  check "$layout: #ci_channel row has \"Join channel\"" in_row_of "Join channel" "#ci_channel"
  check "$layout: member count shown" on_screen "members"
  check "$layout: subscriber count shown" on_screen "subscriber"
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

# typed text that is not a SimpleX link offers the directory; the default layout has the toolbar and the search field at the bottom
"$ui" click-text "Search or paste SimpleX link"
"$ui" type "$search_text"
"$ui" wait "$search_text" 15
"$ui" wait "Search in Directory" 15
"$ui" shot bottom-search-typed
search_directory bottom "Search"

if grep -q '^FAIL' "$checks"; then
  fail "the search for \"$search_text\" failed checks: $(grep -c '^FAIL' "$checks") of $(grep -cE '^(PASS|FAIL)' "$checks")"
fi
echo "smoke test passed: the directory search for \"$search_text\" shows #ci_group and #ci_channel with their avatars"
