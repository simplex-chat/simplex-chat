#!/bin/bash

# Watches the badges screen while the core credits a prepaid badge and lets it lapse, over and over, and checks that the
# screen always shows one of its two pages: "Your badge" or "Support SimpleX". The core does this only when
# SIMPLEX_TEST_BADGE_CYCLES is set ("<cycles> <seconds without a badge> <seconds with one>"), which this script sets for a
# fresh app that it takes through onboarding, after stopping the app of earlier steps.
# Every reading keeps its text and the left pane's image in <output dir>/readings. A reading with neither page that lasts
# BLANK_SECS is the blank screen: it is kept as a numbered screenshot, and the screen is closed and opened again.
# Usage: scripts/ci/desktop-linux/badge-cycles.sh <app dir> <output dir> [cycles] [seconds without] [seconds with]

set -euo pipefail

app_dir=$1
out=$2
cycles=${3:-40}
hidden_secs=${4:-10}
shown_secs=${5:-10}
blank_secs=${BLANK_SECS:-5}
here=$(dirname "$0")
ui=$here/ui
readings=$out/readings
mkdir -p "$readings"

UI_OUT=$out "$ui" stop
export UI_STATE=${RUNNER_TEMP:-/tmp}/ui-badge-cycles
export SIMPLEX_TEST_BADGE_CYCLES="$cycles $hidden_secs $shown_secs"
"$here/smoke-test.sh" "$app_dir" "$out/onboarding" onboarding
export UI_OUT=$out

normalise() {
  tr -cd '[:alnum:]' <<< "$1" | tr '[:upper:]' '[:lower:]' | tr l i
}

# the page the left pane shows, from the text of one reading: badge, support or blank
page_of() {
  local text
  text=$(normalise "$(awk -F'[, ]' '$1 < 390' "$1" | cut -d ' ' -f 2-)")
  case "$text" in
    *"$(normalise "Your badge")"* | *"$(normalise "shown on your profile")"*) echo badge ;;
    *"$(normalise "Support SimpleX")"*) echo support ;;
    *) echo blank ;;
  esac
}

open_badges() {
  "$ui" click 27 730
  "$ui" click-text "Settings" 10
  for _ in 1 2 3 4 5 6; do
    "$ui" click-text "Supporter perks" 3 && return 0
    "$ui" scroll 195 400 down
  done
  return 1
}

close_badges() {
  for _ in 1 2 3; do
    "$ui" key Escape
  done
}

open_badges
"$ui" wait "Support SimpleX" 20 || "$ui" wait "Your badge" 5
"$ui" shot badges-open

n=0
credits=0
lapses=0
blanks=0
last=""
blank_since=""
deadline=$((SECONDS + cycles * (hidden_secs + shown_secs) + 60))
while [ "$SECONDS" -lt "$deadline" ]; do
  n=$((n + 1))
  reading=$readings/$(printf '%04d' "$n")
  "$ui" text > "$reading.txt"
  page=$(page_of "$reading.txt")
  convert "$UI_STATE/ocr/screen.png" -crop 390x900+0+0 +repage "$reading-$page.png"
  mv "$reading.txt" "$reading-$page.txt"
  echo "$(date -u +%H:%M:%S) reading $n: $page" >> "$out/readings.log"
  case "$last>$page" in
    support\>badge) credits=$((credits + 1)) ;;
    badge\>support) lapses=$((lapses + 1)) ;;
  esac
  if [ "$page" = blank ]; then
    blank_since=${blank_since:-$SECONDS}
    if [ $((SECONDS - blank_since)) -ge "$blank_secs" ]; then
      blanks=$((blanks + 1))
      echo "BLANK $blanks at reading $n, after $credits credits and $lapses lapses, the last page shown: ${last_page:-none}" | tee -a "$out/blanks.txt"
      "$ui" shot "blank-$blanks"
      close_badges
      open_badges || true
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
    [ -s "$out/blanks.txt" ] && sed 's/^/- /' "$out/blanks.txt"
  } >> "$GITHUB_STEP_SUMMARY"
fi
[ "$blanks" -eq 0 ]
