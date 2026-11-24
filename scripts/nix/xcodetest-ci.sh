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
xcodebuild test-without-building \
  -project apps/ios/SimpleX.xcodeproj \
  -scheme "SimpleX (iOS)" \
  -destination "$destination" \
  -derivedDataPath "$derived" \
  -resultBundlePath "$derived/result.xcresult" \
  -onlyUsePackageVersionsFromResolvedFile \
  -skipPackagePluginValidation \
  -skipMacroValidation \
  -IDEPackageSupportDisableManifestSandbox=1 \
  -IDEPackageSupportDisablePluginExecutionSandbox=1 \
  CODE_SIGNING_ALLOWED=NO >>"$log" 2>&1 &
xpid=$!
# The one-pass suite is about eight minutes. A UI-configuration regression
# is not allowed to occupy the builder for hours.
(
  trap - EXIT
  sleep 1200
  cmd=$(ps -p "$xpid" -o command= 2>/dev/null || true)
  case "$cmd" in
    *test-without-building*)
      echo "WATCHDOG: exceeded 20 minutes, stopping test run" | tee -a "$log"
      kill -TERM "$xpid" 2>/dev/null || true
      sleep 10
      cmd=$(ps -p "$xpid" -o command= 2>/dev/null || true)
      case "$cmd" in
        *test-without-building*) kill -KILL "$xpid" 2>/dev/null || true ;;
      esac
      ;;
  esac
) &
wpid=$!
wait "$xpid"
ec=$?
kill "$wpid" 2>/dev/null || true
wait "$wpid" 2>/dev/null || true
set -e
echo "XCODEBUILD_EXIT=$ec" | tee -a "$log"
echo "ran $destination"
chmod a+r "$log" || true
exit "$ec"
