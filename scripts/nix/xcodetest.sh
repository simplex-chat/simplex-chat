#!/bin/bash
# Link the rolled-up Haskell core and run the SimpleX (iOS) XCTest scheme once.
# Stub archives are not a substitute: chat_migrate_init_key must be the real
# function. testLaunch runs once; the UI-configuration matrix is hours of the
# same case.
#
# SwiftPM compiles each Package.swift under /usr/bin/sandbox-exec. nixbld
# cannot apply that profile (sandbox_apply: EPERM). The one-argument
# IDEPackageSupportDisable* defaults skip it. Xcode 26 rejects the two-word
# YES form.
#
# aarch64-darwin requires the xcode-home system feature so the nixbld passwd
# home is writable. CoreSimulator follows that home, not $HOME. A host that
# cannot reach a simulator fails this derivation.
set -euo pipefail

src=${1:?source tree}
out=${2:?output}
ios_lib=${3:?ios library output}
mac2ios=${4:?mac2ios}

work=$(mktemp -d)
cp -a "$src/." "$work/"
chmod -R u+w "$work"
cd "$work"

python3 "apps/ios/SimpleX Snapshots/check-previews.py"

developer=${DEVELOPER_DIR:-/Applications/Xcode.app/Contents/Developer}
export DEVELOPER_DIR=$developer
# xcodebuild lives in Xcode.app. xcrun is the /usr/bin trampoline, and a Nix
# builder's PATH does not include /usr/bin. Nix tools stay after the host paths.
export PATH="$developer/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
# xcodebuild and SwiftPM follow HOME. CoreSimulator follows the passwd entry.
export HOME=$work/home
export CFFIXED_USER_HOME=$HOME
mkdir -p "$HOME/Library/Developer/Xcode" "$HOME/Library/Caches" "$HOME/Library/Logs"

if [ ! -x "$developer/usr/bin/xcodebuild" ]; then
  echo "xcodebuild is not available at $developer/usr/bin/xcodebuild" >&2
  exit 1
fi
if [ ! -x "$mac2ios" ]; then
  echo "mac2ios is not available at $mac2ios" >&2
  exit 1
fi

case $(uname -m) in
  arm64|aarch64) archs=arm64 ;;
  x86_64) archs=x86_64 ;;
  *) echo "Unsupported Xcode host architecture: $(uname -m)" >&2; exit 1 ;;
esac
export ARCH=$archs

shopt -s nullglob
zips=("$ios_lib"/*.zip)
if [ "${#zips[@]}" -ne 1 ]; then
  echo "expected one ios library zip in $ios_lib, found ${#zips[@]}" >&2
  exit 1
fi
stage=$work/ios-lib
mkdir -p "$stage"
unzip -q "${zips[0]}" -d "$stage"
libs=("$stage"/*.a)
if [ "${#libs[@]}" -eq 0 ]; then
  echo "ios library zip contains no static archives" >&2
  exit 1
fi

mkdir -p apps/ios/Libraries apps/ios/Libraries/ios apps/ios/Libraries/sim
# The zip is already retargeted to device iOS. The simulator copies need
# platform 7. Do not mac2ios the device copies again.
cp -a "${libs[@]}" apps/ios/Libraries/ios/
cp -a "${libs[@]}" apps/ios/Libraries/sim/
for f in apps/ios/Libraries/sim/*.a; do
  chmod u+w "$f"
  "$mac2ios" -s "$f"
done
# File references live in Libraries/. The simulator link reads those paths.
# Libraries/sim is the -lcrypto search path. Both are the simulator archives.
cp -a apps/ios/Libraries/sim/*.a apps/ios/Libraries/
bash scripts/ios/update-pbxproj.sh

derived=$work/DerivedData
xcodebuild build-for-testing \
  -project apps/ios/SimpleX.xcodeproj \
  -scheme "SimpleX (iOS)" \
  -configuration Debug \
  -destination "generic/platform=iOS Simulator" \
  -derivedDataPath "$derived" \
  -onlyUsePackageVersionsFromResolvedFile \
  -skipPackagePluginValidation \
  -skipMacroValidation \
  -IDEPackageSupportDisableManifestSandbox=1 \
  -IDEPackageSupportDisablePluginExecutionSandbox=1 \
  ARCHS="$archs" \
  CODE_SIGNING_ALLOWED=NO

mkdir -p "$out"
mkdir -p "$out/xctest"
find "$derived" -name '*.xctest' -type d -exec cp -R {} "$out/xctest/" \;
find "$out/xctest" -name '*.xctest' -type d | tee "$out/built.txt"
if [ ! -s "$out/built.txt" ]; then
  echo "xcodebuild produced no XCTest bundles" >&2
  exit 1
fi

if ! simctl list devices available -j >"$out/simulators.json" 2>"$out/simulators.err"; then
  echo "CoreSimulatorService is not reachable. The iOS suite did not run." >&2
  cat "$out/simulators.err" >&2 || true
  exit 1
fi

destination=$(python3 - "$out/simulators.json" <<'PY'
import json
import sys
data = json.load(open(sys.argv[1]))
phones = []
for runtime, devices in data.get("devices", {}).items():
    if "iOS" not in runtime:
        continue
    for device in devices:
        if device.get("isAvailable", True) and device.get("name", "").startswith("iPhone"):
            phones.append(device)
by_name = {device["name"]: device for device in phones}
pick = None
for name in ("iPhone 17", "iPhone 16", "iPhone 15", "iPhone 14"):
    if name in by_name:
        pick = by_name[name]
        break
if pick is None and phones:
    pick = phones[-1]
if pick is None:
    sys.exit("CoreSimulatorService is reachable, but no available iPhone simulator exists")
print("platform=iOS Simulator,id=" + pick["udid"])
PY
)

# Linker-signed products have no app-group entitlement, so the process
# SIGTRAPs in getWallpaperDirectory(). Sign inside-out. keychain-access-groups
# is restricted under an ad-hoc signature (AMFI -424) and is not included.
ent=$work/scripts/nix/adhoc-group-task.entitlements
prod=$derived/Build/Products/Debug-iphonesimulator
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

set +e
xcodebuild test-without-building \
  -project apps/ios/SimpleX.xcodeproj \
  -scheme "SimpleX (iOS)" \
  -destination "$destination" \
  -derivedDataPath "$derived" \
  -onlyUsePackageVersionsFromResolvedFile \
  -skipPackagePluginValidation \
  -skipMacroValidation \
  -IDEPackageSupportDisableManifestSandbox=1 \
  -IDEPackageSupportDisablePluginExecutionSandbox=1 \
  CODE_SIGNING_ALLOWED=NO &
xpid=$!
# The one-pass suite is about eight minutes. A UI-configuration regression
# is not allowed to occupy the builder for hours.
(
  sleep 1200
  cmd=$(ps -p "$xpid" -o command= 2>/dev/null || true)
  case "$cmd" in
    *test-without-building*)
      echo "WATCHDOG: exceeded 20 minutes, stopping test run"
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
echo "XCODEBUILD_EXIT=$ec"
if [ "$ec" -ne 0 ]; then
  exit "$ec"
fi
echo "ran $destination" > "$out/simulator-tests.txt"
