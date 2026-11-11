#!/bin/bash

# Video calls between two apps, each in its own iPhone simulator, over the preset servers: Alice and Bob onboard,
# connect with Alice's 1-time link, Alice calls Bob, then Bob calls Alice; the callee accepts in the app (CallKit off).
# alice and bob can run different builds, such as the app with a new and with an old WebRTC.
# Each app sends sounds/video.mp4 (test-video.swift) in place of the camera, a color that changes every half second.
# A call passes when both screens show it connected and e2e encrypted and, by the media stats a debug build logs, each
# app decoded the other's VP8 video, received its audio, and encrypted and decrypted frames without errors. Whether the screen draws the peer's video is reported, not checked: in run 41256820483 the
# runner's simulators drew no video at all, not even the own camera's.
# Each call is ended by one peer. The other peer's call must end on the end message, without reconnecting first,
# also with its app in the background. A failure is reported, not failed, when the peer whose app has the fix is not
# in the fixed peers.
# Usage: scripts/ci/ios-sim/call-test.sh <alice's SimpleX.app> <bob's SimpleX.app> <output dir> [fixed peers]

set -euo pipefail

out=$3
app_alice=$1
app_bob=$2
fixed_peers=${4-alice bob}
here=$(dirname "$0")
ui=$here/ui
# shellcheck source=scripts/ci/ios-sim/sim-lib.sh
. "$here/sim-lib.sh"
# shellcheck source=scripts/ci/ios-sim/call-lib.sh
. "$here/call-lib.sh"
mkdir -p "$out"

# SC2034: get reads them by name
# shellcheck disable=SC2034
udid_alice='' udid_bob='' log_pid_alice='' log_pid_bob='' name_alice=Alice name_bob=Bob
peers=(alice bob)

check_test_video alice
check_test_video bob
swiftc -O -o "$out/call-frame" "$here/call-frame.swift"
trap finish EXIT

# one simulator at a time: with both starting, the runner read no accessibility tree for minutes (run 41229483743)
for peer in "${peers[@]}"; do
  start_app "$peer"
  SIM_UDID=$(get udid "$peer") UI_OUT=$out/$peer onboard "$(get name "$peer")"
done

video_call() {
  local caller=$1 callee=$2 ender=$3 background=${4:-} call="$1-calls-$2-$3-ends${4:+-$4}" caller_from callee_from receiver
  receiver=$([ "$ender" = "$caller" ] && echo "$callee" || echo "$caller")
  ensure_running "$caller"
  ensure_running "$callee"
  caller_from=$(($(log_lines "$caller") + 1))
  callee_from=$(($(log_lines "$callee") + 1))
  start_video_call "$caller" "$callee" "$call"
  accept_call "$callee" "$caller" "$call"
  expect_call "$caller" "$call"
  expect_call "$callee" "$call"
  wait_media "$caller" "$caller_from"
  wait_media "$callee" "$callee_from"
  report_screen "$caller" "$call"
  report_screen "$callee" "$call"
  check_media "$caller" "$call" "$caller_from" reported
  check_media "$callee" "$call" "$callee_from" reported
  hang_up "$ender" "$receiver" "$call" "$background"
  on "$caller" shot "$call-ended"
  on "$callee" shot "$call-ended"
  echo "$call: connected, e2e encrypted, audio both ways, ended"
}

link=$(create_link alice)
echo "Alice's link: $link"
join_link bob "$link"
on alice wait Bob 180 || fail "Bob did not connect to Alice"
on bob wait Alice 60 || fail "Bob has no contact Alice"
on alice shot connected-to-bob
close_invitation alice

video_call alice bob alice
video_call bob alice bob
video_call alice bob bob background

[ "$hang_up_failures" -eq 0 ] || fail "$hang_up_failures hang-up checks failed"

echo "call test passed: alice ($app_alice) and bob ($app_bob) called each other, e2e encrypted, with audio both ways, and each hang-up ended the other call"
