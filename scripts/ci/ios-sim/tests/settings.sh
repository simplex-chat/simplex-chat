#!/bin/bash

# Settings: the toolbar's profile image opens the user picker, its "Settings" opens Settings, and a swipe down closes
# it, with the one-hand UI and with the top bar.
# covers: apps/ios/Shared/Views/ChatList/UserPicker.swift
# covers: apps/ios/Shared/Views/UserSettings/SettingsView.swift
# covers: apps/ios/Shared/Views/ChatList/ChatListView.swift

set -euo pipefail
. "$(dirname "$0")/../lib.sh"

# The toolbar's profile image is 32x32; the picker's own Image "Account" (44x44) is in the tree even while it is closed.
tap_profile_image() {
  local x y
  read -r x y < <("$ui" tree | sed -n -E 's/^Image "Account" .* at ([0-9]+),([0-9]+) size 32x32.*/\1 \2/p' | head -n 1) ||
    fail "$1: no profile image in the toolbar"
  "$ui" tap-xy $((x + 16)) $((y + 16))
}

# The sheet takes a moment to slide away.
settings_closed() {
  local i
  for i in 1 2 3 4 5; do
    in_tree -F 'Heading "Your settings"' || return 0
    sleep 1
  done
  return 1
}

open_and_close_settings() {
  tap_profile_image "$1"
  "$ui" tap "Settings"
  check "$1: Settings opens with \"Your settings\"" "$ui" wait "Your settings" 10
  check "$1: Settings lists \"Network & servers\"" in_tree -F 'Button "Network & servers"'
  "$ui" shot "settings-$1"
  "$ui" swipe 201 150 201 850
  check "$1: a swipe down closes Settings" settings_closed
  check "$1: the chat list shows \"Talk to someone\"" in_tree -F '"Talk to someone"'
  "$ui" shot "closed-$1"
}

open_and_close_settings reachable
"$ui" layout top
open_and_close_settings top
