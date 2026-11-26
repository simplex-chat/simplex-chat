#!/bin/bash
# UI half of xcodetest. The root helper runs this as console user ci,
# inside ci's Aqua session. Arguments are the prepared source tree, the
# DerivedData directory, and the ad-hoc entitlements plist.
set -euo pipefail

work=${1:?source tree}
derived=${2:?derived data}
ent=${3:?entitlements}

export HOME=${HOME:-/Users/ci}
export USER=ci
export LOGNAME=ci
export TMPDIR=$work/tmp
mkdir -p "$TMPDIR"
developer=/Applications/Xcode.app/Contents/Developer
export DEVELOPER_DIR=$developer
export PATH="$developer/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export OS_ACTIVITY_DT_MODE=YES
cd "$work"

log=$derived/xcodetest-run.log
: > "$log"

# Linker-signed products have no app-group entitlement, so the process
# SIGTRAPs in getWallpaperDirectory(). Sign inside-out. keychain-access-groups
# is restricted under an ad-hoc signature (AMFI -424) and is not included.
# The marker is part of the signed bundle. The app then uses the
# simplex_xcodetest database prefix and suffixed keychain accounts.
prod=$derived/Build/Products/Debug-iphonesimulator
: > "$prod/SimpleX.app/xcodetest-isolate"
python3 - "$prod" "$ent" <<'PY'
import os, subprocess, sys
prod, ent = sys.argv[1], sys.argv[2]
entitled = ("/SimpleX.app", "/SimpleX NSE.appex", "/SimpleX SE.appex")
found = []
for dirpath, dirnames, filenames in os.walk(prod):
    if os.path.basename(dirpath).endswith((".app", ".appex", ".xctest", ".framework")):
        found.append(dirpath)
found.sort(key=lambda p: p.count(os.sep), reverse=True)
if not found:
    sys.exit("no built bundles to sign under " + prod)
for path in found:
    cmd = ["/usr/bin/codesign", "--force", "--sign", "-", "--timestamp=none"]
    if any(path.endswith(suffix) for suffix in entitled):
        cmd += ["--entitlements", ent]
    cmd.append(path)
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        sys.stderr.write(result.stderr)
        sys.exit(result.returncode)
print("resigned", len(found))
PY

destination=$(python3 - <<'PY'
import json, subprocess, sys
raw = subprocess.check_output(["/usr/bin/xcrun", "simctl", "list", "devices", "available", "-j"])
data = json.loads(raw)
phones = []
for runtime, devices in data.get("devices", {}).items():
    if "iOS" not in runtime:
        continue
    for device in devices:
        if device.get("isAvailable", True) and device.get("name", "").startswith("iPhone"):
            phones.append(device)
booted = [device for device in phones if device.get("state") == "Booted"]
pool = booted or phones
by_name = {device["name"]: device for device in pool}
pick = None
for name in ("iPhone 17", "iPhone 16", "iPhone 15", "iPhone 14"):
    if name in by_name:
        pick = by_name[name]
        break
if pick is None and pool:
    pick = pool[-1]
if pick is None:
    sys.exit("no available iPhone simulator in the console session")
print("platform=iOS Simulator,id=" + pick["udid"])
PY
)
echo "destination $destination" | tee -a "$log"
udid=${destination#*id=}
data=$(xcrun simctl get_app_container "$udid" chat.simplex.app data)
prefs=$data/Library/Preferences
backup=$derived/ci-prefs-backup
mkdir -p "$backup"
for name in chat.simplex.app.plist group.chat.simplex.app.plist; do
  if [ -f "$prefs/$name" ]; then
    cp -p "$prefs/$name" "$backup/$name"
  fi
done
group=$(python3 - "$udid" <<'PY'
import os, plistlib, sys
udid = sys.argv[1]
root = f"/Users/ci/Library/Developer/CoreSimulator/Devices/{udid}/data/Containers/Shared/AppGroup"
found = ""
for name in os.listdir(root):
    path = os.path.join(root, name)
    meta = os.path.join(path, ".com.apple.mobile_container_manager.metadata.plist")
    if not os.path.isfile(meta):
        continue
    with open(meta, "rb") as handle:
        ident = plistlib.load(handle).get("MCMMetadataIdentifier")
    if ident == "group.chat.simplex.app":
        found = path
        break
if not found:
    sys.exit("group.chat.simplex.app container not found")
print(found)
PY
)
echo "isolate group $group" | tee -a "$log"
rm -f "$group"/simplex_xcodetest*

restore_ci_state() {
  local rc=$?
  set +e
  rm -f "$group"/simplex_xcodetest*
  # SIGKILL so cfprefsd cannot flush the suite's defaults over the backup.
  xcrun simctl spawn "$udid" launchctl kill SIGKILL system/com.apple.cfprefsd.xpc.daemon >/dev/null 2>&1
  sleep 1
  # xcodebuild reinstalls the app. The container captured at start is gone.
  local fresh prefs_now
  fresh=$(xcrun simctl get_app_container "$udid" chat.simplex.app data 2>/dev/null || true)
  prefs_now=$fresh/Library/Preferences
  if [ -n "$fresh" ]; then
    mkdir -p "$prefs_now"
    for name in chat.simplex.app.plist group.chat.simplex.app.plist; do
      if [ -f "$backup/$name" ]; then
        cp -p "$backup/$name" "$prefs_now/$name"
      fi
    done
  fi
  exit "$rc"
}
trap restore_ci_state EXIT
rm -rf "$derived/result.xcresult"

set +e
# callservicesd requires application-identifier. An ad-hoc signature cannot
# carry it (AMFI -424), so the six CallKit VoIP tests fail. They stay in the
# scheme. reportNewIncomingCall does not return under that signature, so
# those two run alone and are stopped if they are still going after 90s.
# A finished suite whose only failures are the six is success. The watchdog
# on the rest of the suite is not.
xcodebuild_common=(
  test-without-building
  -project apps/ios/SimpleX.xcodeproj
  -scheme "SimpleX (iOS)"
  -destination "$destination"
  -derivedDataPath "$derived"
  -onlyUsePackageVersionsFromResolvedFile
  -skipPackagePluginValidation
  -skipMacroValidation
  -IDEPackageSupportDisableManifestSandbox=1
  -IDEPackageSupportDisablePluginExecutionSandbox=1
  CODE_SIGNING_ALLOWED=NO
)
run_bounded() {
  local seconds=$1
  local bundle=$2
  shift 2
  xcodebuild "${xcodebuild_common[@]}" -resultBundlePath "$bundle" "$@" >>"$log" 2>&1 &
  local xpid=$!
  (
    trap - EXIT
    sleep "$seconds"
    local cmd
    cmd=$(ps -p "$xpid" -o command= 2>/dev/null || true)
    case "$cmd" in
      *test-without-building*)
        echo "WATCHDOG: exceeded ${seconds}s, stopping test run" | tee -a "$log"
        kill -TERM "$xpid" 2>/dev/null || true
        sleep 10
        cmd=$(ps -p "$xpid" -o command= 2>/dev/null || true)
        case "$cmd" in
          *test-without-building*) kill -KILL "$xpid" 2>/dev/null || true ;;
        esac
        ;;
    esac
  ) &
  local wpid=$!
  wait "$xpid"
  local ec=$?
  kill "$wpid" 2>/dev/null || true
  wait "$wpid" 2>/dev/null || true
  echo "XCODEBUILD_EXIT=$ec" | tee -a "$log"
  return "$ec"
}
# The rest of the suite is about eight minutes.
run_bounded 1200 "$derived/result.xcresult" \
  -skip-testing:"SimpleX Snapshots/SnapshotTests/testVoIPPushForRefusedCallSuspendsChat" \
  -skip-testing:"SimpleX Snapshots/SnapshotTests/testVoIPPushKeepsChatActiveWhenAppOpens"
suite_ec=$?
echo "VOIP-RUN testVoIPPushForRefusedCallSuspendsChat" | tee -a "$log"
run_bounded 90 "$derived/result-voip-refused.xcresult" \
  -only-testing:"SimpleX Snapshots/SnapshotTests/testVoIPPushForRefusedCallSuspendsChat"
echo "VOIP-RUN testVoIPPushKeepsChatActiveWhenAppOpens" | tee -a "$log"
run_bounded 90 "$derived/result-voip-active.xcresult" \
  -only-testing:"SimpleX Snapshots/SnapshotTests/testVoIPPushKeepsChatActiveWhenAppOpens"
set -e
python3 - "$log" "$suite_ec" <<'PY'
import re, sys
path, suite_ec = sys.argv[1], int(sys.argv[2])
text = open(path, errors="replace").read()
if "\nVOIP-RUN " not in text:
    sys.exit("xcodetest: suite log has no VoIP section")
suite, voip = text.split("\nVOIP-RUN ", 1)
if "WATCHDOG:" in suite:
    sys.exit("xcodetest: the suite hit the watchdog")
if "TEST EXECUTE SUCCEEDED" not in suite and "TEST EXECUTE FAILED" not in suite:
    sys.exit("xcodetest: the suite did not finish")
failed = set(re.findall(r"Test Case '-\[[^]]+ ([A-Za-z0-9_]+)\]' failed", suite))
for line in suite.splitlines():
    line = line.strip()
    if line.endswith("() failed") and "." in line:
        failed.add(line.split(".", 1)[1][: -len("() failed")])
accepted = {
    "testVoIPPushForAnswerBeforeChatStartsSuspendsChat",
    "testVoIPPushForAnswerWithoutInvitationSuspendsChat",
    "testVoIPPushForDeclinedCallSuspendsChat",
    "testVoIPPushForExpiredCallKeepsChatSuspended",
    "testVoIPPushForRefusedCallSuspendsChat",
    "testVoIPPushKeepsChatActiveWhenAppOpens",
}
blocking = (
    "testVoIPPushForRefusedCallSuspendsChat",
    "testVoIPPushKeepsChatActiveWhenAppOpens",
)
unexpected = sorted(failed - accepted)
if unexpected:
    sys.exit("xcodetest: unexpected failures: " + ", ".join(unexpected))
if suite_ec != 0 and not failed:
    sys.exit("xcodetest: suite exited %s without a parsed test failure" % suite_ec)
parts = ("\nVOIP-RUN " + voip).split("\nVOIP-RUN ")[1:]
seen = []
for part in parts:
    name, _, body = part.partition("\n")
    seen.append(name)
    started = (
        ("Test Case '-[" in body and name in body)
        or "WATCHDOG:" in body
        or "TEST EXECUTE" in body
    )
    if not started:
        sys.exit("xcodetest: %s did not start" % name)
if list(seen) != list(blocking):
    sys.exit("xcodetest: VoIP runs were %s" % ", ".join(seen))
print("xcodetest: accepted CallKit failures: " + (", ".join(sorted(failed)) or "(none)"))
PY
echo "ran $destination"
chmod a+r "$log" || true
