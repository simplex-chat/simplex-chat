#!/bin/bash

# The iOS app's shared steps over the ui helper, for the smoke test and the feature tests in tests/.
# Sourced with UI_OUT set; it sets ui and sources ../app-tests/checks.sh. Runs under macOS's bash 3.2.

here=$(dirname "${BASH_SOURCE[0]}")
ui=$here/ui
bundle_id=chat.simplex.app
. "$here/../app-tests/checks.sh"

in_tree() {
  local tree
  tree=$("$ui" tree) || return 1
  grep -q "$@" <<< "$tree"
}
