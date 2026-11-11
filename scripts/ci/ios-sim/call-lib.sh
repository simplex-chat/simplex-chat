# shellcheck shell=bash disable=SC2034,SC2154
# Shared by call-test.sh and call-android-test.sh: the peers' simulators, call screens, media stats and hang-up checks.
# Source it after sim-lib.sh, with out, peers, fixed_peers, app_<peer> and name_<peer> set.

# what each side must receive before the media checks, within media_timeout seconds (as CallTests.swift)
video_frames=30
audio_packets=100
media_timeout=180
# a call that waits for its reconnection to time out (WebRTCClient.reconnectTimeout, 30 s) ends later than this
max_end_delay=20
# The call screen's buttons are not in the accessibility tree: the red end call button is tapped where it is drawn, at
# the bottom centre of the 402 x 874 point screen.
end_call_x=201
end_call_y=794
# the middle of the screen in screenshot pixels (402 x 874 points at 3x): the peer's video, clear of the own video,
# the call state at the top and the buttons at the bottom
frame_region="300 1000 600 600"

# macOS's bash 3.2 has no associative arrays: per-peer values are variables named <name>_<peer>
get() {
  local var=$1_$2
  echo "${!var}"
}

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

fail() {
  echo "error: $*"
  exit 1
}

check_test_video() {
  [ -f "$(get app "$1")/sounds/video.mp4" ] || fail "$(get app "$1") has no sounds/video.mp4 to send as the camera"
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

# Prints the peer's 1-time link, read from the tree; the invitation sheet stays open over the chat list.
create_link() {
  local peer=$1 link
  on "$peer" tap "Let someone connect to you" 30 > /dev/null
  on "$peer" tap "Invite someone privately" 30 > /dev/null
  on "$peer" wait "simplex" 60 > /dev/null
  on "$peer" shot invitation > /dev/null
  link=$(on "$peer" tree | grep -oE 'https://[^" ]+/(i|invitation)#[^" ]+|simplex:/invitation#[^" ]+' | head -n 1)
  [ -n "$link" ] || fail "$peer's 1-time link is not in the tree"
  echo "$link"
}

close_invitation() {
  on "$1" swipe 201 150 201 800
  on "$1" shot invitation-closed
}

# The link reaches the peer's clipboard; the peer opens it as a new chat and connects.
join_link() {
  local peer=$1 link=$2
  # copying to the clipboard timed out once in both call tests of a run (run 41299700451)
  SIM_UDID=$(get udid "$peer") UI_OUT=$out/$peer ui_retry clipboard "$link"
  on "$peer" tap "Connect via link or QR code" 30
  on "$peer" wait "Tap to paste link" 30
  on "$peer" shot connect-via-link
  on "$peer" tap "Tap to paste link"
  # iOS asks before an app reads the clipboard
  on "$peer" tap "Allow Paste" 10 || true
  on "$peer" shot link-pasted
  on "$peer" tap "Open new chat" 60 || on "$peer" tap "Use current profile" 10 || fail "$peer's app did not offer to open the link"
  tap_button "$peer" Connect 60 || fail "$peer's new chat has no Connect button"
  on "$peer" shot connecting
}

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

start_video_call() {
  local caller=$1 callee=$2 call=$3
  open_chat "$caller" "$callee"
  tap_button "$caller" Call 120 || fail "$caller's chat with $callee has no call button"
  on "$caller" tap "Video call" 10
  on "$caller" shot "$call-calling"
}

accept_call() {
  local callee=$1 caller=$2 call=$3 secs=${4:-120}
  if ! on "$callee" wait Accept "$secs"; then
    [ -z "$(get udid "$caller")" ] || sample_app "$caller"
    fail "$callee got no incoming call"
  fi
  on "$callee" shot "$call-incoming"
  on "$callee" tap Accept
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

# Saves the stacks of the app's threads, for a call that does not end or start: in run 41311546712 the app that ended
# a call froze, its main thread logging nothing after WebRTCClient's "ending the call".
sample_app() {
  local peer=$1 pid
  pid=$(app_pid "$peer")
  if [ -z "$pid" ]; then
    echo "$peer's app is not running"
    return
  fi
  sample "$pid" 5 -file "$out/$peer/hang-sample.txt" > /dev/null 2>&1 || true
  echo "::group::$peer's app threads ($out/$peer/hang-sample.txt)"
  cat "$out/$peer/hang-sample.txt" || true
  echo "::endgroup::"
}

fixed() {
  [[ " $fixed_peers " == *" $1 "* ]]
}

# fails the test at its end when the peer's app has the fix, otherwise warns
hang_up_failures=0
expect_fixed() {
  local peer=$1 message=$2
  if fixed "$peer"; then
    echo "::error::$message"
    hang_up_failures=$((hang_up_failures + 1))
    return
  fi
  echo "::warning::$message (not checked: $peer's app is not the fixed one)"
}

log_lines() {
  wc -l < "$out/$1/app.log"
}

log_line() {
  tail -n +"$2" "$out/$1/app.log" | grep -m 1 -F "$3" || true
}

# seconds since midnight of a compact log line, such as "2026-11-09 12:34:56.789 Df SimpleX[...] ..."
log_seconds() {
  awk '{ split($2, t, ":"); printf "%.3f", t[1] * 3600 + t[2] * 60 + t[3] }' <<< "$1"
}

wait_log_line() {
  local peer=$1 from=$2 text=$3 secs=$4 line deadline=$((SECONDS + $4))
  until line=$(log_line "$peer" "$from" "$text") && [ -n "$line" ]; do
    [ "$SECONDS" -lt "$deadline" ] || return 1
    sleep 1
  done
  echo "$line"
}

print_hang_up_log() {
  local call=$1 peer from
  shift
  echo "::group::$call: hang-up in the apps' logs"
  while [ $# -gt 0 ]; do
    peer=$1 from=$2
    shift 2
    tail -n +"$from" "$out/$peer/app.log" |
      grep -E "chatSendCmd apiEndCall|CallManager.endCall|processReceivedMsg: callEnded|WebRTC: processed cmd|WebRTCClient: (reconnecting|ending the call|ice connection state|peer connection state)" |
      sed "s/^\([^ ]* [^ ]*\).*SimpleX\[[^]]*\] /\1 $peer: /"
  done | sort
  echo "::endgroup::"
}

# The ender ends the call; the receiver's app ends it when the chat delivers the end. With background, the receiver's
# app is in the background from before the end until its call ended.
hang_up() {
  local ender=$1 receiver=$2 call=$3 background=$4 ender_from receiver_from ended sent reconnect delay
  ender_from=$(($(log_lines "$ender") + 1))
  receiver_from=$(($(log_lines "$receiver") + 1))
  if [ -n "$background" ]; then
    axe button home --udid "$(get udid "$receiver")"
    sleep 5
    on "$receiver" shot "$call-background"
  fi
  on "$ender" tap-xy "$end_call_x" "$end_call_y"
  if ! call_ended "$ender"; then
    sample_app "$ender"
    fail "$call: $ender's call did not end when $ender ended it"
  fi
  if ! ended=$(wait_log_line "$receiver" "$receiver_from" "WebRTCClient: ending the call" 90); then
    sample_app "$receiver"
    fail "$call: $receiver's call did not end when $ender ended it"
  fi
  if [ -n "$background" ]; then
    xcrun simctl launch "$(get udid "$receiver")" "$bundle_id" > /dev/null
  fi
  if ! call_ended "$receiver"; then
    sample_app "$receiver"
    fail "$call: $receiver's call screen stayed after its call ended"
  fi
  sent=$(log_line "$ender" "$ender_from" "CallManager.endCall: ending call")
  [ -n "$sent" ] || fail "$call: $ender's log has no CallManager.endCall"
  delay=$(awk -v a="$(log_seconds "$sent")" -v b="$(log_seconds "$ended")" 'BEGIN { printf "%.1f", b - a }')
  reconnect=$(log_line "$receiver" "$receiver_from" "WebRTCClient: reconnecting")
  print_hang_up_log "$call" "$ender" "$ender_from" "$receiver" "$receiver_from"
  local reconnected=no
  [ -z "$reconnect" ] || reconnected=yes
  echo "$call: $ender ended the call, $receiver's call ended $delay s later, $receiver reconnecting first: $reconnected"
  if [ -n "$reconnect" ]; then
    expect_fixed "$ender" "$call: $receiver reconnected before $ender's end of the call arrived"
  fi
  if [ -n "$background" ] && awk -v d="$delay" -v m="$max_end_delay" 'BEGIN { exit !(d > m) }'; then
    expect_fixed "$receiver" "$call: $receiver's app in the background ended its call $delay s after $ender ended it, more than $max_end_delay s"
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
