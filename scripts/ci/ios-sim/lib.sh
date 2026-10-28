#!/bin/bash

here=$(dirname "${BASH_SOURCE[0]}")
ui=$here/ui
bundle_id=chat.simplex.app
. "$here/../app-tests/checks.sh"

in_tree() {
  local tree
  tree=$("$ui" tree) || return 1
  grep -q "$@" <<< "$tree"
}
