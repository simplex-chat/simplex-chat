#!/bin/bash

# Watches the badges screen while the core credits a prepaid badge and lets it lapse, over and over, and checks that the
# screen always shows one of its two pages: "Your badge" or the Support SimpleX page. The core starts the cycles on the
# chat console's /badge test_cycles <cycles> <seconds without a badge> <seconds with one> (test builds only); this script
# clears the app's data, takes it through onboarding, sends that from the console and opens Settings > Supporter perks.
# Every reading keeps its screenshot, accessibility tree and text in <output dir>/readings. A reading is blank when the
# tree has neither page, or when the middle of the screenshot is nearly one colour (ink below MIN_INK): a page whose
# drawing is lost stays in the tree. Blank that lasts BLANK_SECS is kept as a numbered screenshot with its tree, and the
# screen is closed and opened again.
# Usage: scripts/ci/android/badge-cycles.sh <apk> <output dir> [cycles] [seconds without] [seconds with]

set -euo pipefail

apk=$1
out=$2
cycles=${3:-40}
hidden_secs=${4:-10}
shown_secs=${5:-10}
blank_secs=${BLANK_SECS:-5}
min_ink=${MIN_INK:-0.02}
package=chat.simplex.app
here=$(dirname "$0")
ui=$here/ui
readings=$out/readings
mkdir -p "$readings"

fresh_onboarding() {
  UI_OUT=$out "$ui" stop
  adb shell pm clear "$package"
  "$here/smoke-test.sh" "$apk" "$out/$1" onboarding
}

# the ARM translation can crash the app at start (README, "Known limits"), so onboarding gets a second start
fresh_onboarding onboarding || fresh_onboarding onboarding-again
export UI_OUT=$out

normalise() {
  tr -cd '[:alnum:]' <<< "$1" | tr '[:upper:]' '[:lower:]'
}

# the page the screen shows, from the text of one reading: badge, support or blank
page_of() {
  local text
  text=$(normalise "$(cut -d ' ' -f 2- "$1")")
  case "$text" in
    *"$(normalise "Your badge")"* | *"$(normalise "shown on your profile")"*) echo badge ;;
    *"$(normalise "Get a badge to send larger files")"*) echo support ;;
    *) echo blank ;;
  esac
}

# the spread of grey levels in the middle of the screen, between its bars: near 0 when nothing is drawn there
ink_of() {
  local w h
  read -r w h < <(identify -format '%w %h' "$1")
  convert "$1" -crop "${w}x$((h * 6 / 10))+0+$((h * 15 / 100))" +repage -colorspace Gray -format '%[fx:standard_deviation]' info:
}

open_settings() {
  "$ui" click-text "profile image placeholder" 10
  "$ui" click-text "Settings" 10
}

scroll_to_click() {
  for _ in 1 2 3 4 5 6; do
    "$ui" click-text "$1" 3 && return 0
    "$ui" scroll 540 1400 down 1
  done
  return 1
}

back_to_chat_list() {
  for _ in 1 2 3 4 5; do
    "$ui" find "Start new chat" > /dev/null 2>&1 && return 0
    "$ui" key BACK
  done
  "$ui" find "Start new chat" > /dev/null
}

start_cycles() {
  local x y
  open_settings
  scroll_to_click "Developer tools"
  "$ui" click-text "Chat console" 10
  read -r x y < <("$ui" tree | sed -n -E 's/^EditText .* at ([0-9]+),([0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/p' | awk '{ print $1 + int($3 / 2), $2 + int($4 / 2); exit }')
  "$ui" click "$x" "$y"
  "$ui" type "/badge test_cycles $cycles $hidden_secs $shown_secs"
  "$ui" click-text "Send Message" 10
  "$ui" wait "test_cycles" 10
  "$ui" shot cycles-started
  back_to_chat_list
}

open_badges() {
  open_settings
  scroll_to_click "Supporter perks"
}

close_badges() {
  "$ui" key BACK
}

start_cycles
open_badges
"$ui" shot badges-open

n=0
credits=0
lapses=0
blanks=0
last=""
last_page=none
blank_since=""
deadline=$((SECONDS + cycles * (hidden_secs + shown_secs) + 60))
while [ "$SECONDS" -lt "$deadline" ]; do
  n=$((n + 1))
  reading=$readings/$(printf '%04d' "$n")
  UI_OUT=$reading "$ui" shot reading > /dev/null
  page=$(page_of "$reading/01-reading.txt")
  ink=$(ink_of "$reading/01-reading.png")
  if [ "$page" != blank ] && awk -v ink="$ink" -v min="$min_ink" 'BEGIN { exit !(ink < min) }'; then
    page=blank
  fi
  mv "$reading" "$reading-$page"
  echo "$(date -u +%H:%M:%S) reading $n: $page, ink $ink" >> "$out/readings.log"
  case "$last>$page" in
    support\>badge) credits=$((credits + 1)) ;;
    badge\>support) lapses=$((lapses + 1)) ;;
  esac
  if [ "$page" = blank ]; then
    blank_since=${blank_since:-$SECONDS}
    if [ $((SECONDS - blank_since)) -ge "$blank_secs" ]; then
      blanks=$((blanks + 1))
      echo "BLANK $blanks at reading $n, after $credits credits and $lapses lapses, the last page shown: $last_page" | tee -a "$out/blanks.txt"
      "$ui" shot "blank-$blanks"
      close_badges
      scroll_to_click "Supporter perks" || open_badges || true
      blank_since=""
    fi
  else
    blank_since=""
    last=$page
    last_page=$page
  fi
done
close_badges

summary="badge cycles: $credits credits and $lapses lapses seen on the open screen in $n readings, $blanks blank"
echo "$summary"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "### Badge screen cycles"
    echo
    echo "$summary"
    if [ -s "$out/blanks.txt" ]; then sed 's/^/- /' "$out/blanks.txt"; fi
  } >> "$GITHUB_STEP_SUMMARY"
fi
[ "$blanks" -eq 0 ]
