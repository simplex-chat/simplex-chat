#!/bin/sh
# Usage: scripts/uitest-android.sh <app.apk> <androidTest.apk> [am instrument -e args...]
# Needs scripts/uitest.sh to have run once, or `scripts/uitest.sh --prepare-only` (it builds the CLI and test servers and writes build/uitest/env.properties).
# Env:
#   UITEST_SERIAL  adb serial of the device or emulator to use; when unset, a Waydroid instance is claimed with scripts/waydroid/wd.sh
#   WD_SH          another wd.sh to use (default scripts/waydroid/wd.sh; see scripts/waydroid/README.md)
#   WD_INSTANCE    Waydroid instance (default 2), WD_OWNER claim owner (default uitest-$USER)
#   UITEST_APP_ID  application id of the app APK (default chat.simplex.app.debug)
#   ADB, JAVA_HOME tool locations (default: the adb wd.sh uses, `wd.sh adb-path`; java from JAVA_HOME or PATH)
set -eu
mp="$(dirname "$(readlink -f "$0")")/.."
wd="${WD_SH:-$mp/scripts/waydroid/wd.sh}"
ADB="${ADB:-$("$wd" adb-path)}"
adb() { "$ADB" "$@"; }
app_apk=$1; test_apk=$2; shift 2
export WD_INSTANCE="${WD_INSTANCE:-2}" WD_OWNER="${WD_OWNER:-uitest-$(id -un)}"
pkg="${UITEST_APP_ID:-chat.simplex.app.debug}"
runner=$pkg.test/androidx.test.runner.AndroidJUnitRunner
out="$(dirname "$(readlink -f "$0")")/../build/uitest-android/run-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$out"
t0=$(date +%s)
step() { echo "uitest-android: [$(( $(date +%s) - t0 )) s] $*"; }

bridge_log="$out/bridge.log"
avail_mb=$(awk '/^MemAvailable:/ {print int($2 / 1024)}' /proc/meminfo)
[ "$avail_mb" -ge 4500 ] || { echo "uitest-android: only $avail_mb MB available, need 4500 (bridge + Waydroid); free memory first" >&2; exit 1; }
java="${JAVA_HOME:+$JAVA_HOME/bin/}java"
[ -s "$mp/build/uitest/env.properties" ] || { echo "uitest-android: run scripts/uitest.sh (or scripts/uitest.sh --prepare-only) once first; it builds the CLI and test servers and writes build/uitest/env.properties" >&2; exit 1; }
cp_file="$mp/build/uitest/bridge.classpath"
h="$mp/common/src/desktopTest/kotlin/chat/simplex/app/ui/harness"
bridge_srcs="$h/UiBridge.kt $h/CliPeer.kt $h/VtScreen.kt $h/TestServers.kt $h/Socks5Stub.kt $h/UiEnv.kt $h/UiJvm.kt $mp/common/src/uiTestShared/kotlin/chat/simplex/app/ui/harness/ServersConfig.kt $mp/common/src/uiTestShared/kotlin/chat/simplex/app/ui/harness/PeerLines.kt"
wd_pid=
cleanup() {
  [ -z "${bridge_pid:-}" ] || kill -TERM "$bridge_pid" 2>/dev/null || true
  [ -z "$wd_pid" ] || wait "$wd_pid" 2>/dev/null || true
  [ -n "${UITEST_SERIAL:-}" ] || "$wd" release >/dev/null 2>&1 || true
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
if [ ! -s "$cp_file" ] || [ -n "$(find $bridge_srcs -newer "$cp_file" 2>/dev/null)" ]; then
  step "compiling the bridge"
  (cd "$mp" && ./gradlew -g "$mp/.gradle/uitest-home" --no-daemon -q -Pkotlin.compiler.execution.strategy=in-process :common:uitestBridgeClasspath) > "$out/bridge-build.log" 2>&1 \
    || { echo "uitest-android: bridge build failed; see $out/bridge-build.log" >&2; exit 1; }
fi
if [ -z "${UITEST_SERIAL:-}" ]; then
  "$wd" claim "$WD_OWNER"
  if ! "$wd" status | grep -q "Session:.*RUNNING"; then
    "$wd" up > "$out/waydroid-up.log" 2>&1 &
    wd_pid=$!
  fi
fi
setsid "$java" -Djava.awt.headless=true -Dsimplex.uitest.env="$mp/build/uitest/env.properties" -cp "$(cat "$cp_file")" chat.simplex.app.ui.harness.UiBridge 7777 > "$bridge_log" 2>&1 &
bridge_pid=$!
i=0
until grep -q "^uitest-bridge: smp" "$bridge_log" 2>/dev/null; do
  i=$((i + 1)); [ "$i" -lt 1200 ] || { echo "uitest-android: bridge did not start; see $bridge_log" >&2; exit 1; }
  kill -0 "$bridge_pid" 2>/dev/null || { echo "uitest-android: bridge exited; see $bridge_log" >&2; exit 1; }
  sleep 0.1
done
base=$(sed -n 's/^uitest-bridge: .*port base \([0-9]*\),.*/\1/p' "$bridge_log")
step "bridge up, server port base $base"

if [ -n "$wd_pid" ]; then
  wait "$wd_pid" || { echo "uitest-android: wd.sh up failed; see $out/waydroid-up.log" >&2; wd_pid=; exit 1; }
  wd_pid=
fi
serial="${UITEST_SERIAL:-$("$wd" serial)}"
step "device $serial ready"
echo "uitest-android: note: pm clear removes the app's data on $serial"
app_sum=$(sha256sum "$app_apk" | cut -c1-16)
stamp="$mp/build/uitest-android/installed-app-$(printf %s "$serial" | tr -c "A-Za-z0-9" _)"
installed_ver=$(adb -s "$serial" shell dumpsys package "$pkg" 2>/dev/null | tr -d '\r' | sed -n 's/^ *lastUpdateTime=//p' | head -1)
if [ -n "$installed_ver" ] && [ "$(cat "$stamp" 2>/dev/null)" = "$app_sum $installed_ver" ]; then
  echo "uitest-android: app APK unchanged on $serial, skipping its install"
else
  if [ -n "${UITEST_SERIAL:-}" ]; then adb -s "$serial" install -r -t -g "$app_apk" >/dev/null; else "$wd" install "$app_apk"; fi
  installed_ver=$(adb -s "$serial" shell dumpsys package "$pkg" | tr -d '\r' | sed -n 's/^ *lastUpdateTime=//p' | head -1)
  echo "$app_sum $installed_ver" > "$stamp"
fi
adb -s "$serial" install -r -t "$test_apk"
step "APKs installed"
for p in 7777 $((base + 1)) $((base + 2)) $((base + 11)) $((base + 12)) $((base + 20)) $((base + 90)); do adb -s "$serial" reverse "tcp:$p" "tcp:$p" >/dev/null; done
prepare_app() {
  adb -s "$serial" shell pm clear "$pkg" >/dev/null
  adb -s "$serial" shell settings put global heads_up_notifications_enabled 0
  for perm in android.permission.POST_NOTIFICATIONS android.permission.CAMERA android.permission.RECORD_AUDIO; do
    adb -s "$serial" shell pm grant "$pkg" "$perm" 2>/dev/null || true
  done
}
# Runs `am instrument` into $1. If its output does not change for UITEST_STALL_S seconds (default 420, longer than the
# in-process 300 s watchdog), the app process is force-stopped so the pass ends as "Process crashed" and the run resumes:
# under arm64 translation the native allocator can deadlock (scudo::HybridMutex::lockSlow in several threads), which
# blocks every thread of the process, the in-process watchdog included (scripts/waydroid/README.md).
instrument() {
  file=$1; shift
  adb -s "$serial" shell am instrument -w -r "$@" "$runner" > "$file" 2>&1 &
  ipid=$!
  last=-1; idle=0; stall="${UITEST_STALL_S:-420}"
  while kill -0 "$ipid" 2>/dev/null; do
    sleep 15
    size=$(wc -c < "$file")
    if [ "$size" = "$last" ]; then idle=$((idle + 15)); else idle=0; last=$size; fi
    if [ "$idle" -ge "$stall" ]; then
      step "no instrumentation output for $idle s; force-stopping $pkg (stalled process)"
      adb -s "$serial" shell am force-stop "$pkg" >/dev/null 2>&1
      idle=0
    fi
  done
  wait "$ipid"
}
# Prints one line per test in an `am instrument -r` output: "<class>#<test> <code>" for finished tests (0 pass, -2 fail, -3/-4 skipped)
# and "<class>#<test> crashed" for a test that started without finishing.
results() {
  tr -d '\r' < "$1" | awk '
    /^INSTRUMENTATION_STATUS: class=/ { c = substr($0, 31) }
    /^INSTRUMENTATION_STATUS: test=/ { t = substr($0, 30) }
    /^INSTRUMENTATION_STATUS_CODE: / { code = $2; if (code == 1) { running = c "#" t } else { print c "#" t, code; running = "" } }
    END { if (running != "") print running, "crashed" }'
}
shots=/sdcard/Android/data/$pkg/files/failures
adb -s "$serial" shell rm -rf "$shots" || true
adb -s "$serial" logcat -G 16M >/dev/null 2>&1 || true
adb -s "$serial" logcat -c || true
: > "$out/results.txt"
pass=1
gave_up=
while :; do
  prepare_app
  skip=$(cut -d' ' -f1 "$out/results.txt" | paste -sd, -)
  set +e
  if [ -n "$skip" ]; then
    instrument "$out/instrument-$pass.txt" "$@" -e notClass "$skip"
  else
    instrument "$out/instrument-$pass.txt" "$@"
  fi
  rc=$?
  set -e
  results "$out/instrument-$pass.txt" >> "$out/results.txt"
  adb -s "$serial" pull "$shots" "$out/" >/dev/null 2>&1 || true
  step "instrumentation pass $pass finished (am instrument exit $rc)"
  tr -d '\r' < "$out/instrument-$pass.txt" | grep -q "^INSTRUMENTATION_RESULT: shortMsg=" || break
  [ "$pass" -lt 4 ] || { echo "uitest-android: process crashed in 4 passes, giving up" >&2; gave_up=1; break; }
  pass=$((pass + 1))
  echo "uitest-android: the app process crashed; resuming with the remaining tests (pass $pass)"
done
cat "$out"/instrument-*.txt > "$out/instrument.txt"
adb -s "$serial" logcat -d > "$out/logcat.txt" 2>&1 || true
grep -E "INSTRUMENTATION_STATUS: stack=" "$out/instrument.txt" | cut -c1-1500 || true
grep -E "UITEST|TestRunner: (started|finished|failed)" "$out/logcat.txt" | sed "s/^[0-9-]* //" > "$out/timings.txt" || true
echo "uitest-android: results ($out/results.txt; 0 pass, -2 fail, -3/-4 skipped):"
sed 's/^chat\.simplex\.app\.ui\.//' "$out/results.txt"
echo "uitest-android: $(grep -c ' 0$' "$out/results.txt") passed, $(grep -c ' -2$' "$out/results.txt") failed, $(grep -c ' crashed$' "$out/results.txt") crashed; results in $out"
[ -z "$gave_up" ] && ! grep -qE ' (-2|crashed)$' "$out/results.txt" && [ -s "$out/results.txt" ]
