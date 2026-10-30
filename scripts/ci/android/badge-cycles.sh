#!/bin/bash

# Watches the badges screen while the core credits a prepaid badge and lets it lapse, over and over, and checks that the
# screen draws the page its accessibility tree lists: "Your badge" or the Support SimpleX page. The core starts the
# cycles on the chat console's /badge test_cycles <cycles> <seconds without a badge> <seconds with one> (test builds
# only); this script reinstalls the app, takes it through onboarding, sends that from the console and opens Settings >
# Supporter perks. BADGE_CYCLES_SETTINGS, settings as ui start takes them, restarts the app with them first.
# Every reading keeps its screenshot, tree and text in <output dir>/readings, and readings.log has the page the tree
# lists and the page drawn: support when the Support page's wide blue button is drawn, blank when the middle of the
# screen is one colour (ink below MIN_INK), badge otherwise. A change of the listed page is a transition; it fails when
# a reading after its first (which can be taken mid-change) draws another page or nothing. transitions.log has each one.
# Usage: scripts/ci/android/badge-cycles.sh <apk> <output dir> [cycles] [seconds without] [seconds with]

set -euo pipefail

apk=$1
out=$2
cycles=${3:-40}
hidden_secs=${4:-10}
shown_secs=${5:-10}
min_ink=${MIN_INK:-0.02}
package=chat.simplex.app
here=$(dirname "$0")
ui=$here/ui
readings=$out/readings
mkdir -p "$readings"

# uninstalled rather than cleared: a cleared app loses the permissions install granted, and onboarding then asks differently
fresh_onboarding() {
  UI_OUT=$out "$ui" stop
  adb uninstall "$package"
  "$here/smoke-test.sh" "$apk" "$out/$1" onboarding
}

# the ARM translation can crash the app at start (README, "Known limits"), so onboarding gets a second start
fresh_onboarding onboarding || fresh_onboarding onboarding-again
export UI_OUT=$out
if [ -n "${BADGE_CYCLES_SETTINGS:-}" ]; then
  "$ui" stop
  # shellcheck disable=SC2086
  "$ui" start "$apk" $BADGE_CYCLES_SETTINGS
  "$ui" wait "Start new chat" 90
fi

normalise() {
  tr -cd '[:alnum:]' <<< "$1" | tr '[:upper:]' '[:lower:]'
}

# the page the tree lists, from the text of one reading: badge, support or none
listed_page_of() {
  local text
  text=$(normalise "$(cut -d ' ' -f 2- "$1")")
  case "$text" in
    *"$(normalise "Your badge")"* | *"$(normalise "shown on your profile")"*) echo badge ;;
    *"$(normalise "Get a badge to send larger files")"*) echo support ;;
    *) echo none ;;
  esac
}

# Prints "<page> <ink> <blue>": ink is the spread of grey levels between the bars, which ImageMagick reads as nan when
# there is none; blue is how much bluer than red the place of the Support page's button is.
drawn_page_of() {
  local w h ink blue page=badge
  read -r w h < <(identify -format '%w %h\n' "$1")
  ink=$(convert "$1" -alpha off -crop "${w}x$((h * 6 / 10))+0+$((h * 15 / 100))" +repage -colorspace Gray -format '%[fx:standard_deviation]' info:)
  blue=$(convert "$1" -alpha off -crop "$((w * 55 / 100))x$((h * 4 / 100))+$((w * 22 / 100))+$((h * 78 / 100))" +repage -format '%[fx:mean.b-mean.r]' info:)
  case "$ink" in *nan*) ink=0 ;; esac
  if awk -v ink="$ink" -v min="$min_ink" 'BEGIN { exit !(ink < min) }'; then
    page=blank
  elif awk -v blue="$blue" 'BEGIN { exit !(blue > 0.4) }'; then
    page=support
  fi
  echo "$page $ink $blue"
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

# Settings lists the app's version and code at its end, which open the screen with Developer tools; that screen shows
# the version too, but not followed by the code
gradle_properties=$here/../../../apps/multiplatform/gradle.properties
app_version="$(sed -n 's/^android.version_name=//p' "$gradle_properties") ($(sed -n 's/^android.version_code=//p' "$gradle_properties"))"

back_to_settings() {
  for _ in 1 2 3 4 5 6; do
    "$ui" find "$app_version" > /dev/null 2>&1 && return 0
    "$ui" key BACK
  done
  "$ui" find "$app_version" > /dev/null
}

scroll_up_to_click() {
  for _ in 1 2 3 4 5 6 7 8; do
    "$ui" click-text "$1" 3 && return 0
    "$ui" scroll 540 1000 up 1
  done
  return 1
}

start_cycles() {
  local x y
  open_settings
  scroll_to_click "$app_version"
  "$ui" click-text "Developer tools" 10
  "$ui" click-text "Chat console" 10
  read -r x y < <("$ui" tree | sed -n -E 's/^EditText .* at ([0-9]+),([0-9]+) size ([0-9]+)x([0-9]+).*/\1 \2 \3 \4/p' | awk '{ print $1 + int($3 / 2), $2 + int($4 / 2); exit }')
  "$ui" click "$x" "$y"
  "$ui" type "/badge test_cycles $cycles $hidden_secs $shown_secs"
  "$ui" click-text "Send Message" 10
  "$ui" wait "test_cycles" 10
  "$ui" shot cycles-started
  back_to_settings
}

start_cycles
scroll_up_to_click "Supporter perks"
"$ui" shot badges-open

pages=0
transitions=0
failed=0
blank=0
listed_now=""
since=""
drawn_since_first=""

# Judges the page listed since the last change: the first is the screen as it opened, not a transition, and one read
# once has nothing to judge.
judge_transition() {
  local verdict=ok drawn
  [ -n "$listed_now" ] || return 0
  pages=$((pages + 1))
  [ "$listed_now" != none ] || verdict=unlisted
  for drawn in $drawn_since_first; do
    if [ "$drawn" = blank ]; then
      verdict=blank
    elif [ "$drawn" != "$listed_now" ] && [ "$verdict" = ok ]; then
      verdict="drew another page"
    fi
  done
  echo "$since to $listed_now: $verdict, drawn:$drawn_since_first" >> "$out/transitions.log"
  [ "$pages" -gt 1 ] || return 0
  transitions=$((transitions + 1))
  [ "$verdict" = ok ] || failed=$((failed + 1))
  [ "$verdict" != blank ] || blank=$((blank + 1))
}

n=0
deadline=$((SECONDS + cycles * (hidden_secs + shown_secs) + 60))
while [ "$SECONDS" -lt "$deadline" ]; do
  n=$((n + 1))
  reading=$readings/$(printf '%04d' "$n")
  UI_OUT=$reading "$ui" shot reading > /dev/null
  listed=$(listed_page_of "$reading/01-reading.txt")
  read -r drawn ink blue <<< "$(drawn_page_of "$reading/01-reading.png")"
  mv "$reading" "$reading-$listed-$drawn"
  now=$(date -u +%H:%M:%S)
  echo "$now reading $n: listed $listed, drawn $drawn, ink $ink, blue $blue" >> "$out/readings.log"
  if [ "$listed" != "$listed_now" ]; then
    judge_transition
    listed_now=$listed
    since=$now
    drawn_since_first=""
  else
    drawn_since_first+=" $drawn"
  fi
done
judge_transition
"$ui" key BACK

summary="badge cycles: $transitions transitions on the open screen in $n readings; $failed drew another page or nothing, $blank of them nothing"
echo "$summary"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    echo "### Badge screen cycles${BADGE_CYCLES_SETTINGS:+ with $BADGE_CYCLES_SETTINGS}"
    echo
    echo "$summary"
  } >> "$GITHUB_STEP_SUMMARY"
fi
[ "$failed" -eq 0 ]
