#!/bin/bash

# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/*
# covers: apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/*
# covers: apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/PlatformTextField.desktop.kt

set -euo pipefail
. "$(dirname "$0")/../lib.sh"

note="scripted note"
row_spacing=90

in_open_chat() {
  local target line
  target=$(normalise "$1")
  while IFS= read -r line; do
    [ "${line%%,*}" -ge 390 ] || continue
    case "$(normalise "${line#* }")" in
      *"$target"*) return 0 ;;
    esac
  done < <("$ui" text)
  return 1
}

note_is_preview() {
  local name_y note_y
  name_y=$(y_of "Private notes") || return 1
  note_y=$(y_of "$note") || return 1
  [ "$note_y" -gt "$name_y" ] && [ "$note_y" -lt $((name_y + row_spacing / 2)) ]
}

read -r x y < <(left_pane_position "Private notes") || fail "no \"Private notes\" in the chat list"
"$ui" click "$x" "$y"
"$ui" click-text "Message" 10
"$ui" type "$note"
"$ui" key Return
"$ui" wait "Message" 10
"$ui" shot note-sent
check "the note shows in Private notes" in_open_chat "$note"

"$ui" shot preview-reachable
check "reachable: Private notes shows the note as its preview" note_is_preview

"$ui" layout top
"$ui" shot preview-top
check "top: Private notes shows the note as its preview" note_is_preview
