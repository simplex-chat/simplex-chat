#!/bin/bash

# Starts the desktop app on a virtual display and runs the scripted UI cases with the ui helper:
# onboarding to the chat list, and typing in the chat list search field, which offers the directory.
# Usage: scripts/ci/desktop-linux/smoke-test.sh <app dir> <output dir> [onboarding]
# With "onboarding" it stops at the chat list: releases before directory search show no search field without chats.
# Each step saves a numbered screenshot and the text read from it to the output dir, prints the text to the log,
# and adds a line to steps.log there. The app keeps running for later steps of the job, which get UI_OUT.

set -euo pipefail

app_dir=$1
export UI_OUT=$2
cases=${3:-all}
search_text="ci smoke"
ui=$(dirname "$0")/ui
mkdir -p "$UI_OUT"
if [ -n "${GITHUB_ENV:-}" ]; then
  echo "UI_OUT=$UI_OUT" >> "$GITHUB_ENV"
fi

finish() {
  local status=$?
  "$ui" shot final || true
  echo "::group::app log, last 300 lines"
  tail -n 300 "$UI_OUT/app.log" || true
  echo "::endgroup::"
  exit "$status"
}
trap finish EXIT

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

# typed text that is not a SimpleX link offers the directory
"$ui" click-text "Search or paste SimpleX link"
"$ui" type "$search_text"
"$ui" wait "$search_text" 15
"$ui" wait "Search in Directory" 15
"$ui" shot search-typed
echo "smoke test passed: onboarding reached the chat list, and its search field shows \"$search_text\" and the \"Search in Directory\" row"
