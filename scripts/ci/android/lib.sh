#!/bin/bash

# The Android app's shared steps over the ui helper, for the smoke test and the feature tests in tests/: positions of
# texts, fields and switches in the accessibility tree, going back to a screen and opening Settings.
# Sourced with UI_OUT set; it sets ui and sources ../app-tests/checks.sh.

here=$(dirname "${BASH_SOURCE[0]}")
ui=$here/ui
search_field="Search or paste SimpleX link"
. "$here/../app-tests/checks.sh"

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
