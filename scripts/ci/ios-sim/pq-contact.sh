#!/bin/bash

# Connects the app that smoke-test.sh left at its chat list with a CLI peer, both on the preset servers: the app pastes
# the peer's one-time link, they exchange messages, and the peer's and the app's contact info must show quantum
# resistant encryption, whose sntrup761 KEM hashes with the core's OpenSSL.
# Usage: scripts/ci/ios-sim/pq-contact.sh <simplex-chat CLI> <work dir>
# Needs SIM_UDID and UI_OUT from smoke-test.sh; the peer's commands and their output go to UI_OUT/peer.log.

set -euo pipefail

cli=$1
work=$2
ui=$(dirname "$0")/ui
bundle_id=chat.simplex.app
peer=pq_peer
peer_pq="quantum resistant end-to-end encryption"
app_pq="Quantum resistant"
peer_message="peer-to-app"
app_message="app-to-peer"
connect_secs=300
# the y above which a row is on the iPhone 17 screen, clear of the home indicator
visible_bottom=780
mkdir -p "$work"
chmod +x "$cli"

# smoke-test.sh stopped its stream of the app's log when it ended
xcrun simctl spawn "$SIM_UDID" log stream --level debug --style compact --predicate 'process == "SimpleX"' > "$UI_OUT/pq-app.log" 2>&1 &
log_pid=$!

finish() {
  local status=$?
  if [ "$status" -ne 0 ]; then
    "$ui" shot pq-failed || true
  fi
  kill "$log_pid" || true
  exit "$status"
}
trap finish EXIT

# Runs one command in the peer CLI, which stays online for 5 s after it to exchange messages; the output is in $work/out.
chat() {
  local status=0
  "$cli" -d "$work/$peer" --user-display-name "$peer" -e "$1" -t 5 < /dev/null > "$work/out" 2>&1 || status=$?
  {
    echo "> $1"
    cat "$work/out"
  } >> "$UI_OUT/peer.log"
  return "$status"
}

until_peer_shows() {
  local pattern=$1 cmd=$2 deadline=$((SECONDS + connect_secs))
  until chat "$cmd" && grep -qiE "$pattern" "$work/out"; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      echo "error: \"$pattern\" not shown by the peer's \"$cmd\" after $connect_secs s"
      cat "$work/out"
      return 1
    fi
  done
}

# "x y width height" of each element line of ui tree
frames() {
  sed -E 's/.* at (-?[0-9]+),(-?[0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/'
}

tap_centre() {
  local x y w h
  read -r x y w h
  "$ui" tap-xy $((x + w / 2)) $((y + h / 2))
}

# the full simplex: link opens in the app without asking the server for the link's data; the CLI prints only the
# short link with -e, so the link is read from its database
chat /c
link=$(sqlite3 "$work/${peer}_chat.db" "SELECT CAST(conn_req_inv AS TEXT) FROM connections WHERE conn_req_inv IS NOT NULL ORDER BY connection_id DESC LIMIT 1")
case "$link" in
  simplex:/invitation#*) ;;
  *)
    echo "error: the peer made no one-time link"
    cat "$work/out"
    exit 1
    ;;
esac
# pasted, as the core rejected this link opened with simctl openurl (run 41240723750)
echo "the peer's link: ${#link} characters"
"$ui" clipboard "$link"
# the sheet's QR scanner would ask for the camera in an alert over the paste button; iOS stops an app whose
# permissions change
xcrun simctl privacy "$SIM_UDID" grant camera "$bundle_id"
"$ui" relaunch
"$ui" tap "Connect via link or QR code"
"$ui" tap "Tap to paste link" 20
deadline=$((SECONDS + 60))
until "$ui" find "Use current profile" > /dev/null 2>&1; do
  "$ui" tap "Allow Paste" 1 > /dev/null 2>&1 || true
  if [ "$SECONDS" -ge "$deadline" ]; then
    echo "error: the app offered no connection to the peer's link 60 s after it was pasted"
    exit 1
  fi
  sleep 2
done
"$ui" shot connect-via-link
"$ui" tap "Use current profile"
"$ui" wait "Connection request sent" 60
"$ui" tap OK

# the app's profile is the peer's only contact: smoke-test.sh typed its name, which the keyboard may have autocorrected
contact_line='^[^ ]+( \(.*\))?$'
until_peer_shows "$contact_line" /contacts
contact=$(grep -m 1 -E "$contact_line" "$work/out" | cut -d ' ' -f 1)
until_peer_shows "$peer_pq" "/i @$contact"
chat "@$contact $peer_message"

"$ui" wait "$peer" "$connect_secs"
"$ui" find "$peer" | head -n 1 | frames | tap_centre
# the chat's messages have no text in the accessibility tree: the screenshots show them
"$ui" wait "Your contact" 30
"$ui" shot pq-chat

# the message field has no label: it is right of the attachments button, and the send button appears at its right end
read -r x y w h < <("$ui" tree | grep -E '^Button "Attachements" ' | frames)
row=$((y + h / 2))
"$ui" tap-xy $((x + w + 100)) "$row"
"$ui" type "$app_message"
send=$("$ui" tree | grep -E '^Button ' | grep -vE '^Button "(Attachements|Flash)" ' | frames | awk -v row="$row" \
  '{ x = $1 + int($3 / 2); y = $2 + int($4 / 2) } y > row - 30 && y < row + 30 && x > 250 { print x, y }' | sort -n | tail -n 1)
if [ -z "$send" ]; then
  echo "error: no send button right of the message field"
  exit 1
fi
read -r x y <<< "$send"
"$ui" tap-xy "$x" "$y"

until_peer_shows "$app_message" "/tail @$contact 5"
chat "/i @$contact"
cp "$work/out" "$UI_OUT/peer-contact.txt"
if ! grep -qF "$peer_pq" "$work/out"; then
  echo "error: the peer's contact $contact is not quantum resistant after the messages"
  exit 1
fi
"$ui" shot pq-messages

# the contact's info shows the connection's encryption in its "E2E encryption" row, below the first screen; the tree
# lists the row before it scrolls into view, so it is scrolled until the screenshot shows it
"$ui" tree | grep -E "^Button \"$peer\" " | frames | tap_centre
row_top() {
  "$ui" find "$app_pq" 2> /dev/null | grep ' at ' | frames | head -n 1 | cut -d ' ' -f 2
}
for _ in 1 2 3 4 5; do
  top=$(row_top || true)
  if [ -n "$top" ] && [ "$top" -lt "$visible_bottom" ]; then
    break
  fi
  "$ui" swipe 201 700 201 300
done
top=$(row_top || true)
if [ -z "$top" ] || [ "$top" -ge "$visible_bottom" ]; then
  echo "error: the app's contact info does not show \"$app_pq\" encryption"
  exit 1
fi
"$ui" shot pq-contact-info
echo "the app and the peer exchanged messages, and both show quantum resistant encryption"
