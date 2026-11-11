#!/bin/bash

# The iOS side of the iOS-Android call test (scripts/ci/android/call-peer.sh, on a Linux runner), in two steps.
# "link" starts the iPhone and prints its 1-time link to <output dir>/link, for the job to pass it to the Android job;
# the simulator keeps running for "calls". "calls" waits for the Android app to connect, calls it and hangs up, then
# accepts its call, checks the media, and checks that its call ends on the Android app's end message, without
# reconnecting first.
# Usage: scripts/ci/ios-sim/call-android-test.sh link <SimpleX.app> <output dir>
#        scripts/ci/ios-sim/call-android-test.sh calls <output dir>

set -euo pipefail

phase=$1
here=$(dirname "$0")
ui=$here/ui
# shellcheck source=scripts/ci/ios-sim/sim-lib.sh
. "$here/sim-lib.sh"
# shellcheck source=scripts/ci/ios-sim/call-lib.sh
. "$here/call-lib.sh"

# SC2034: get reads them by name
# shellcheck disable=SC2034
name_alice=iPhone name_android=citester
peers=(alice)
fixed_peers=alice

case "$phase" in
  link)
    app_alice=$2
    out=$3
    mkdir -p "$out"
    udid_alice='' log_pid_alice=''
    check_test_video alice
    swiftc -O -o "$out/call-frame" "$here/call-frame.swift"
    stop_on_failure() {
      local status=$?
      [ "$status" -eq 0 ] && return
      (exit "$status")
      finish
    }
    trap stop_on_failure EXIT
    start_app alice
    SIM_UDID=$udid_alice UI_OUT=$out/alice onboard "$name_alice"
    create_link alice > "$out/link"
    printf 'app_alice=%q\nudid_alice=%q\nlog_pid_alice=%q\n' "$app_alice" "$udid_alice" "$log_pid_alice" > "$out/state"
    echo "the iPhone's link: $(cat "$out/link")"
    exit 0
    ;;
  calls)
    out=$2
    # shellcheck source=/dev/null
    . "$out/state"
    trap finish EXIT
    ;;
  *)
    fail "usage: $0 link <SimpleX.app> <output dir> | calls <output dir>"
    ;;
esac

on alice wait "$name_android" 3600 || fail "the Android app did not connect"
on alice shot connected-to-android
close_invitation alice

call=iphone-calls-android
from=$(($(log_lines alice) + 1))
start_video_call alice android "$call"
on alice wait "e2e encrypted" 600 || fail "$call: the Android app did not accept the call"
expect_call alice "$call"
wait_media alice "$from"
report_screen alice "$call"
check_media alice "$call" "$from"
end_from=$(($(log_lines alice) + 1))
on alice tap-xy "$end_call_x" "$end_call_y"
if ! call_ended alice; then
  sample_app alice
  fail "$call: the iPhone's call did not end when it ended it"
fi
print_hang_up_log "$call" alice "$end_from"
echo "$call: connected, e2e encrypted, audio and video both ways, ended by the iPhone"

call=android-calls-iphone
ensure_running alice
accept_call alice android "$call" 900
from=$(($(log_lines alice) + 1))
expect_call alice "$call"
wait_media alice "$from"
report_screen alice "$call"
check_media alice "$call" "$from"
if ! ended=$(wait_log_line alice "$from" "WebRTCClient: ending the call" 300); then
  sample_app alice
  fail "$call: the iPhone's call did not end when the Android app ended it"
fi
call_ended alice || fail "$call: the iPhone's call screen stayed after its call ended"
print_hang_up_log "$call" alice "$from"
received=$(log_line alice "$from" "processReceivedMsg: callEnded")
reconnect=$(log_line alice "$from" "WebRTCClient: reconnecting")
echo "$call: the iPhone received the end at ${received%% Df*}, ended its call at ${ended%% Df*}, reconnecting first: $([ -n "$reconnect" ] && echo yes || echo no)"
[ -z "$reconnect" ] || fail "$call: the iPhone reconnected before the Android app's end of the call arrived"
echo "call test passed: the iPhone and the Android app called each other, e2e encrypted, and each call ended without reconnecting"
