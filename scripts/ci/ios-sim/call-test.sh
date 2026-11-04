#!/bin/bash

# Video calls between two apps, each in its own iPhone simulator, over the preset servers: Alice and Bob onboard,
# connect with Alice's 1-time link, Alice calls Bob, then Bob calls Alice; the callee accepts in the app (CallKit off).
# alice and bob can run different builds, such as the app with a new and with an old WebRTC.
# Each app sends sounds/video.mp4 (test-video.swift) in place of the camera, a color that changes every half second.
# A call passes when both screens show it connected and e2e encrypted and, by the media stats a debug build logs, each
# app decoded the other's VP8 video, received its audio, and encrypted and decrypted frames without errors. Whether the screen draws the peer's video is reported, not checked: in run 41256820483 the
# runner's simulators drew no video at all, not even the own camera's.
# Usage: scripts/ci/ios-sim/call-test.sh <alice's SimpleX.app> <bob's SimpleX.app> <output dir>

set -euo pipefail

out=$3
app_alice=$1
app_bob=$2
here=$(dirname "$0")
ui=$here/ui
# shellcheck source=scripts/ci/ios-sim/sim-lib.sh
. "$here/sim-lib.sh"
# what each side must receive before the media checks, within media_timeout seconds (as CallTests.swift)
video_frames=30
audio_packets=100
media_timeout=180
# The call screen's buttons are not in the accessibility tree: the red end call button is tapped where it is drawn, at
# the bottom centre of the 402 x 874 point screen.
end_call_x=201
end_call_y=794
# the middle of the screen in screenshot pixels (402 x 874 points at 3x): the peer's video, clear of the own video,
# the call state at the top and the buttons at the bottom
frame_region="300 1000 600 600"
mkdir -p "$out"

# SC2034: get reads them by name
# shellcheck disable=SC2034
udid_alice='' udid_bob='' log_pid_alice='' log_pid_bob='' name_alice=Alice name_bob=Bob

# macOS's bash 3.2 has no associative arrays: per-peer values are variables named <name>_<peer>
get() {
  local var=$1_$2
  echo "${!var}"
}

for peer in alice bob; do
  [ -f "$(get app "$peer")/sounds/video.mp4" ] || {
    echo "error: $(get app "$peer") has no sounds/video.mp4 to send as the camera"
    exit 1
  }
done
swiftc -O -o "$out/call-frame" "$here/call-frame.swift"

peers=(alice bob)

on() {
  SIM_UDID=$(get udid "$1") UI_OUT=$out/$1 "$ui" "${@:2}"
}

finish() {
  local status=$? peer
  for peer in "${peers[@]}"; do
    [ -n "$(get udid "$peer")" ] || continue
    on "$peer" shot final || true
    kill "$(get log_pid "$peer")" 2> /dev/null || true
    xcrun simctl shutdown "$(get udid "$peer")" || true
    echo "::group::$peer: app log, last 200 lines"
    tail -n 200 "$out/$peer/app.log" || true
    echo "::endgroup::"
  done
  find ~/Library/Logs/DiagnosticReports -name 'SimpleX*' -newer "$app_alice" 2> /dev/null | while read -r report; do
    echo "::group::crash report $report"
    head -n 200 "$report"
    echo "::endgroup::"
  done || true
  exit "$status"
}
trap finish EXIT

fail() {
  echo "error: $*"
  exit 1
}

start_app() {
  local peer=$1 container udid
  udid=$(new_simulator "ci-$peer")
  printf -v "udid_$peer" '%s' "$udid"
  mkdir -p "$out/$peer"
  xcrun simctl install "$udid" "$(get app "$peer")"
  xcrun simctl privacy "$udid" grant microphone "$bundle_id"
  xcrun simctl privacy "$udid" grant camera "$bundle_id"
  # CallKit's incoming call screen is iOS's, not the app's: with it off the app shows the call in its own views
  container=$(xcrun simctl get_app_container "$udid" "$bundle_id" "$app_group")
  xcrun simctl spawn "$udid" defaults write "$container/Library/Preferences/$app_group" callKitEnabled -bool false
  xcrun simctl spawn "$udid" log stream --level debug --style compact --predicate 'process == "SimpleX"' > "$out/$peer/app.log" 2>&1 &
  printf -v "log_pid_$peer" '%s' "$!"
  xcrun simctl launch "$udid" "$bundle_id"
}

# one simulator at a time: with both starting, the runner read no accessibility tree for minutes (run 41229483743)
for peer in "${peers[@]}"; do
  start_app "$peer"
  SIM_UDID=$(get udid "$peer") UI_OUT=$out/$peer onboard "$(get name "$peer")"
done

# The peers have one contact each, so a shown call button is the chat with it; tapping the name there would open
# the contact's info.
open_chat() {
  local peer=$1 contact
  contact=$(get name "$2")
  if on "$peer" tree | grep -qE '^PopUpButton "Call"'; then
    return
  fi
  on "$peer" tap "Close" 5 || true
  on "$peer" wait "$contact" 30
  on "$peer" tap "$contact" 10 || on "$peer" tap-value "$contact" 10
  on "$peer" shot "chat-with-$2"
}

tap_line() {
  local x y w h
  read -r x y w h < <(sed -E 's/.* at (-?[0-9]+),(-?[0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/' <<< "$2")
  on "$1" tap-xy $((x + w / 2)) $((y + h / 2))
}

# taps the first element of the types (default: a button or a pop-up button, the chat's "Call" menu) whose label
# matches the pattern, waiting up to secs for it
tap_button() {
  local peer=$1 pattern=$2 secs=$3 types=${4:-Button|PopUpButton} line
  local deadline=$((SECONDS + secs))
  while [ "$SECONDS" -lt "$deadline" ]; do
    line=$(on "$peer" tree | grep -E "^($types) \"($pattern)\"" | head -n 1 || true)
    if [ -n "$line" ]; then
      tap_line "$peer" "$line"
      return
    fi
    sleep 3
  done
  return 1
}

expect_call() {
  local peer=$1 tree
  on "$peer" wait "e2e encrypted" 90 || fail "$peer: the call is not e2e encrypted"
  local deadline=$((SECONDS + 90))
  until tree=$(on "$peer" tree) && grep -qE '"connected"' <<< "$tree"; do
    [ "$SECONDS" -lt "$deadline" ] || fail "$peer: the call did not connect"
    sleep 2
  done
  on "$peer" shot "$2-connected"
}

# The call screen covers the chat, whose tree stays readable under it: the call ended when the chat's composer is in
# the tree and the call's state is not. An unreadable tree is not an ended call (run 41311546712).
call_ended() {
  local deadline=$((SECONDS + 60)) tree
  while [ "$SECONDS" -lt "$deadline" ]; do
    tree=$(on "$1" tree || true)
    if grep -qF 'Button "Attachements"' <<< "$tree" && ! grep -qE '^StaticText "connected"' <<< "$tree"; then
      return
    fi
    sleep 3
  done
  return 1
}

app_pid() {
  pgrep -f "$(get udid "$1")/.*/SimpleX.app/SimpleX$" | head -n 1 || true
}

# Two simulators share the Mac's audio server: an app stopping its audio after a call aborted with "AURemoteIO: RPC
# timeout. Apparently deadlocked" (run 41333783822). That app is started again; any other exit fails the test.
ensure_running() {
  local peer=$1
  [ -z "$(app_pid "$peer")" ] || return 0
  grep -q "AURemoteIO: RPC timeout" "$out/$peer/app.log" || fail "$peer's app is not running"
  echo "::warning::$peer's app aborted in the simulator's audio server after its call ended; starting it again"
  xcrun simctl launch "$(get udid "$peer")" "$bundle_id" > /dev/null
  on "$peer" wait Compose 90
}

# Saves the stacks of the app's threads, for a call that does not end: in run 41311546712 the app that ended a call
# froze, its main thread logging nothing after WebRTCClient's "ending the call".
sample_app() {
  local peer=$1 pid
  pid=$(app_pid "$peer")
  if [ -z "$pid" ]; then
    echo "$peer's app is not running"
    return
  fi
  sample "$pid" 5 -file "$out/$peer/hang-sample.txt" > /dev/null 2>&1 || true
  echo "::group::$peer's app threads while its call did not end ($out/$peer/hang-sample.txt)"
  cat "$out/$peer/hang-sample.txt" || true
  echo "::endgroup::"
}

# The peer ends its call; the other app ends it when the chat delivers the end.
end_call() {
  local peer=$1 other=$2 call=$3
  on "$peer" tap-xy "$end_call_x" "$end_call_y"
  if ! call_ended "$peer"; then
    sample_app "$peer"
    fail "$call: $peer's call did not end when $peer ended it"
  fi
  if ! call_ended "$other"; then
    sample_app "$other"
    fail "$call: $other's call did not end when $peer ended it"
  fi
}

# the peer's video: a saturated color, different in two screenshots a few seconds apart
frame_color() {
  local peer=$1 shot
  shot=$(find "$out/$peer" -maxdepth 1 -name "*-$2.png" | sort | tail -n 1)
  # shellcheck disable=SC2086
  "$out/call-frame" "$shot" $frame_region
}

saturated() {
  local r g b
  read -r r g b <<< "$1"
  local max=$((r > g ? (r > b ? r : b) : (g > b ? g : b)))
  local min=$((r < g ? (r < b ? r : b) : (g < b ? g : b)))
  [ "$max" -ge 150 ] && [ $((max - min)) -ge 100 ]
}

report_screen() {
  local peer=$1 call=$2 first second shown=no
  on "$peer" shot "$call-video-1"
  sleep 3
  on "$peer" shot "$call-video-2"
  first=$(frame_color "$peer" "$call-video-1")
  second=$(frame_color "$peer" "$call-video-2")
  if saturated "$first" && saturated "$second" && [ "$first" != "$second" ]; then
    shown=yes
  fi
  echo "$call: $peer's screen shows the peer's video: $shown (rgb $first, then $second)"
}

# The app's last media stats line after line $2 of its log (WebRTCClient.logMediaStats), such as "audio packets
# received 812, video frames decoded 301, encoded 290, frames encrypted 1102, decrypted 1093, crypto errors 0,
# codecs audio/opus video/VP8".
media_stats() {
  tail -n +"$2" "$out/$1/app.log" | grep -oE "call media stats: .*" | tail -n 1 | sed 's/call media stats: //'
}

# a count from a media stats line, such as "video frames decoded"
stat_count() {
  grep -oE "$2 [0-9]+" <<< "$1" | grep -oE "[0-9]+$" || echo 0
}

wait_media() {
  local peer=$1 from=$2 stats deadline=$((SECONDS + media_timeout))
  while [ "$SECONDS" -lt "$deadline" ]; do
    stats=$(media_stats "$peer" "$from")
    if [ "$(stat_count "$stats" "video frames decoded")" -ge "$video_frames" ] &&
      [ "$(stat_count "$stats" "audio packets received")" -ge "$audio_packets" ]; then
      return
    fi
    sleep 5
  done
}

check_media() {
  local peer=$1 call=$2 from=$3 stats
  stats=$(media_stats "$peer" "$from")
  echo "$call: $peer's media: ${stats:-no stats logged}"
  [ "$(stat_count "$stats" "audio packets received")" -gt 0 ] || fail "$call: $peer received no audio"
  [ "$(stat_count "$stats" "video frames decoded")" -gt 0 ] || fail "$call: $peer decoded no video"
  [ "$(stat_count "$stats" "frames encrypted")" -gt 0 ] || fail "$call: $peer encrypted no frames"
  [ "$(stat_count "$stats" "decrypted")" -gt 0 ] || fail "$call: $peer decrypted no frames"
  [ "$(stat_count "$stats" "crypto errors")" -eq 0 ] || fail "$call: $peer had frame encryption errors"
  grep -qE "codecs .*video/VP8" <<< "$stats" || fail "$call: $peer's video is not VP8"
}

video_call() {
  local caller=$1 callee=$2 call="$1-calls-$2" caller_from callee_from
  ensure_running "$caller"
  ensure_running "$callee"
  caller_from=$(($(wc -l < "$out/$caller/app.log") + 1))
  callee_from=$(($(wc -l < "$out/$callee/app.log") + 1))
  open_chat "$caller" "$callee"
  tap_button "$caller" Call 120 || fail "$caller's chat with $callee has no call button"
  on "$caller" tap "Video call" 10
  on "$caller" shot "$call-calling"
  on "$callee" wait Accept 120 || fail "$callee got no incoming call"
  on "$callee" shot "$call-incoming"
  on "$callee" tap Accept
  expect_call "$caller" "$call"
  expect_call "$callee" "$call"
  wait_media "$caller" "$caller_from"
  wait_media "$callee" "$callee_from"
  report_screen "$caller" "$call"
  report_screen "$callee" "$call"
  check_media "$caller" "$call" "$caller_from"
  check_media "$callee" "$call" "$callee_from"
  # Alice, this branch's app, ends both calls: before this branch's fix, ending a call closed it twice at once and
  # froze the app (run 41333783822).
  end_call alice bob "$call"
  on "$caller" shot "$call-ended"
  on "$callee" shot "$call-ended"
  echo "$call: connected, e2e encrypted, audio and video both ways"
}

# Alice's 1-time link, read from the tree, reaches Bob's clipboard; Bob opens it as a new chat and connects
on alice tap "Let someone connect to you" 30
on alice tap "Invite someone privately" 30
on alice wait "simplex" 60
on alice shot invitation
link=$(on alice tree | grep -oE 'https://[^" ]+/(i|invitation)#[^" ]+|simplex:/invitation#[^" ]+' | head -n 1)
[ -n "$link" ] || fail "Alice's 1-time link is not in the tree"
echo "Alice's link: $link"
# copying to the clipboard timed out once in both call tests of a run (run 41299700451)
SIM_UDID=$(get udid bob) UI_OUT=$out/bob ui_retry clipboard "$link"

on bob tap "Connect via link or QR code" 30
on bob wait "Tap to paste link" 30
on bob shot connect-via-link
on bob tap "Tap to paste link"
# iOS asks before an app reads the clipboard
on bob tap "Allow Paste" 10 || true
on bob shot link-pasted
on bob tap "Open new chat" 60 || on bob tap "Use current profile" 10 || fail "Bob's app did not offer to open Alice's link"
tap_button bob Connect 60 || fail "Bob's new chat with Alice has no Connect button"
on bob shot connecting

on alice wait Bob 180 || fail "Bob did not connect to Alice"
on bob wait Alice 60 || fail "Bob has no contact Alice"
on alice shot connected-to-bob
# the invitation sheet stays open over the chat list
on alice swipe 201 150 201 800
on alice shot invitation-closed

video_call alice bob
video_call bob alice

echo "call test passed: alice ($app_alice) and bob ($app_bob) called each other, e2e encrypted, with audio and video both ways"
