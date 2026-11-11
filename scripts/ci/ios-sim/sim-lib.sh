# shellcheck shell=bash disable=SC2034,SC2154
# Shared by smoke-test.sh and call-test.sh: a new simulator and onboarding to the chat list, driven with the ui helper.
# Source it after setting ui to the helper's path; each function acts on the simulator in SIM_UDID.
# SC2034, SC2154: bundle_id and app_group are for the scripts that source it, which set ui.

bundle_id=chat.simplex.app
app_group=group.chat.simplex.app

# Creates and boots an iPhone 17 simulator named $1, opens its window and prints its UDID.
new_simulator() {
  local runtime udid
  runtime=com.apple.CoreSimulator.SimRuntime.iOS-$(xcrun --sdk iphonesimulator --show-sdk-version | tr . -)
  udid=$(xcrun simctl create "$1" com.apple.CoreSimulator.SimDeviceType.iPhone-17 "$runtime")
  echo "simulator $1 $udid, runtime $runtime" >&2
  xcrun simctl boot "$udid"
  xcrun simctl bootstatus "$udid" -b > /dev/null
  # the keyboard's swipe-typing tip covers the lower half of the screen, with its own Continue button (run 41248278756)
  xcrun simctl spawn "$udid" defaults write com.apple.keyboard.preferences DidShowContinuousPathIntroduction -bool true
  open -a Simulator --args -CurrentDeviceUDID "$udid"
  echo "$udid"
}

expect_in_tree() {
  local tree
  tree=$("$ui" tree)
  if ! grep -q "$@" <<< "$tree"; then
    echo "error: ${*: -1} is not in the accessibility tree"
    exit 1
  fi
}

# iOS asks once whether the app may send notifications. The tree sometimes shows the app under this alert rather than
# the alert (run 38895987656), so its Allow button is also looked for at its place.
allow_notifications() {
  local deadline=$((SECONDS + 30)) element
  while [ "$SECONDS" -lt "$deadline" ]; do
    if "$ui" tap Allow 1; then
      return
    fi
    element=$("$ui" point 275 518 || true)
    if grep -qF '"Allow"' <<< "$element"; then
      "$ui" tap-xy 275 518
      return
    fi
    sleep 2
  done
  echo "no notifications alert to allow"
}

# Runs a ui command up to 3 times: AXe sometimes fails on a simulator that is still settling, such as with "No translation
# object returned for simulator" (run 41256820483).
ui_retry() {
  local attempt
  for attempt in 1 2; do
    "$ui" "$@" && return
    echo "retrying: ui $*"
    sleep 5
  done
  "$ui" "$@"
}

# From the app's first screen to the chat list, creating the profile $1; shot names start with $2.
# Waits are long: a runner with two simulators read no accessibility tree for over 30 s (runs 41229483743, 41248278756),
# and a second simulator's first screen took over 3 min (run 42709556460).
onboard() {
  local name=$1 prefix=${2:-}
  "$ui" wait "Get started" 600
  "$ui" shot "${prefix}first-screen"
  ui_retry tap "Get started" 60

  "$ui" wait "Create profile" 90
  ui_retry tap-value "Enter profile name..." 60
  "$ui" type "$name"
  "$ui" shot "${prefix}profile-name"
  ui_retry tap "Create profile" 60

  "$ui" wait "Continue" 90
  "$ui" shot "${prefix}your-network"
  ui_retry tap "Continue" 60

  "$ui" wait "Accept" 90
  "$ui" shot "${prefix}network-commitments"
  ui_retry tap "Accept" 60

  sleep 5
  "$ui" shot "${prefix}after-onboarding"
  allow_notifications
  "$ui" shot "${prefix}notifications-allowed"

  "$ui" wait "Talk to someone" 90
  "$ui" shot "${prefix}chat-list"
}
