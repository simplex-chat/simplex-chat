#!/bin/bash

# The Android side of the iOS-Android call test (scripts/ci/ios-sim/call-android-test.sh, on a macOS runner): the app,
# onboarded by smoke-test.sh, connects with the iPhone's 1-time link over the preset servers. The iPhone calls and hangs
# up: the Android app accepts, and its call must end on the end message, without reconnecting first. Then the Android
# app calls the iPhone, keeps the call for a while for the iPhone's media checks, and hangs up.
# Usage: scripts/ci/android/call-peer.sh <link> <output dir>

set -euo pipefail

link=$1
export UI_OUT=$2
ui=$(dirname "$0")/ui
package=chat.simplex.app
peer=iPhone
call_time=60
log=$UI_OUT/app.log
mkdir -p "$UI_OUT"

fail() {
  echo "error: $*"
  "$ui" shot failed || true
  exit 1
}

log_lines() {
  wc -l < "$log"
}

call_log() {
  tail -n +"$1" "$log" | grep -E "received from WebRTCView: Connection|CallManager.endCall|callEnded|CallView: webview" || true
}

wait_call_end() {
  local from=$1 deadline=$((SECONDS + $2))
  until call_log "$from" | grep -q "CallManager.endCall"; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
}

adb shell am start -a android.intent.action.VIEW -d "$link" "$package" > /dev/null
"$ui" wait "Open new chat" 60 || fail "the app did not offer to open the iPhone's link"
"$ui" shot link-opened
"$ui" click-text "Open new chat"
"$ui" click-text Connect 60 || fail "the new chat with $peer has no Connect button"
"$ui" shot connecting
"$ui" wait "$peer" 600 || fail "no contact $peer 10 min after opening the link"
"$ui" shot connected

call=iphone-calls-android
"$ui" wait Accept 1800 || fail "$call: no incoming call from $peer"
"$ui" shot "$call-incoming"
from=$(($(log_lines) + 1))
"$ui" click-text Accept
"$ui" wait "e2e encrypted" 300 || fail "$call: the call is not e2e encrypted"
"$ui" shot "$call-connected"
wait_call_end "$from" 600 || fail "$call: the call did not end 10 min after it connected"
"$ui" shot "$call-ended"
echo "::group::$call: the Android app's call log"
call_log "$from"
echo "::endgroup::"
if call_log "$from" | grep -q "Connection reconnecting"; then
  fail "$call: the Android app reconnected before the iPhone's end of the call arrived"
fi
echo "$call: the call ended without reconnecting"

call=android-calls-iphone
sleep 15
"$ui" click-text "$peer" 30
"$ui" wait "Message" 30
"$ui" click-text "$peer" 10
"$ui" wait video 30 || fail "$call: no video call button in $peer's info"
read -r x _ _ top _ _ < <("$ui" find video)
"$ui" click "$x" $((top - 70))
from=$(($(log_lines) + 1))
"$ui" wait "e2e encrypted" 600 || fail "$call: $peer did not accept the call"
"$ui" shot "$call-connected"
sleep "$call_time"
"$ui" shot "$call-before-hang-up"
"$ui" click-text "Hang up" 10
wait_call_end "$from" 60 || fail "$call: the call did not end when the Android app ended it"
"$ui" shot "$call-ended"
echo "::group::$call: the Android app's call log"
call_log "$from"
echo "::endgroup::"
adb shell pidof "$package" > /dev/null || fail "the app is not running"
echo "call test passed on Android: $peer called, the Android app called back, each ended its call"
