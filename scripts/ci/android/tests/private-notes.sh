#!/bin/bash

# Private notes: a note sent in "Private notes" shows in the chat and as the chat's preview in the chat list, with the
# reachable toolbar and with the top bar.
# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/*
# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/*
# covers: apps/multiplatform/common/src/androidMain/kotlin/chat/simplex/common/platform/PlatformTextField.android.kt

set -euo pipefail
. "$(dirname "$0")/../lib.sh"

note="scripted note"

# An empty compose field has its hint as its text; Send and Record voice message both show either way.
compose_empty() {
  local tree
  for _ in 1 2 3 4 5 6 7 8 9 10; do
    tree=$("$ui" tree) && grep -q '^EditText "Message" at' <<< "$tree" && return 0
    sleep 1
  done
  return 1
}

# A row's preview is the line under its name; rows are about 230 px apart.
note_is_preview() {
  local name_y note_y
  name_y=$(y_of "Private notes") || return 1
  note_y=$(y_of "$note") || return 1
  [ "$note_y" -gt "$name_y" ] && [ "$note_y" -lt $((name_y + 115)) ]
}

"$ui" click-text "Private notes" 10
"$ui" click-text "Message" 10
"$ui" type "$note"
"$ui" click-text "Send Message" 10
compose_empty || fail "the compose field still has the note after Send"
"$ui" shot note-sent
check "the note shows in Private notes" "$ui" find "$note"

# the first Back can close the keyboard
back_to "Start new chat" || fail "no chat list after leaving Private notes"
"$ui" shot preview-reachable
check "reachable: Private notes shows the note as its preview" note_is_preview

"$ui" layout top
"$ui" shot preview-top
check "top: Private notes shows the note as its preview" note_is_preview
