#!/bin/bash
# Build the iOS XCTest bundles, then run them when CoreSimulatorService is
# reachable. The Nix build user's directory-services home is /var/empty, so
# CoreSimulator cannot subscribe and that run is skipped.
#
# SwiftPM compiles each Package.swift under /usr/bin/sandbox-exec with a
# profile that imports system.sb. On this OS that import is permitted for a
# normal login user and refused for nixbld (sandbox_apply: EPERM). The job
# still attempts the build; a builder whose build users can apply the profile
# produces the bundles.
set -euo pipefail

src=${1:?source tree}
out=${2:?output}

work=$(mktemp -d)
cp -a "$src/." "$work/"
chmod -R u+w "$work"
cd "$work"

python3 "apps/ios/SimpleX Snapshots/check-previews.py"

developer=${DEVELOPER_DIR:-/Applications/Xcode.app/Contents/Developer}
export DEVELOPER_DIR=$developer
# xcodebuild lives in Xcode.app. xcrun is the /usr/bin trampoline, and a Nix
# builder's PATH does not include /usr/bin.
export PATH="$developer/usr/bin:/usr/bin:$PATH"
# xcodebuild and SwiftPM follow HOME. CoreSimulator follows the passwd entry,
# which stays /var/empty for a Nix build user even after HOME is set.
export HOME=$work/home
export CFFIXED_USER_HOME=$HOME
mkdir -p "$HOME/Library/Developer/Xcode" "$HOME/Library/Caches" "$HOME/Library/Logs"

if [ ! -x "$developer/usr/bin/xcodebuild" ]; then
  echo "xcodebuild is not available at $developer/usr/bin/xcodebuild" >&2
  exit 1
fi

case $(uname -m) in
  arm64|aarch64) archs=arm64 ;;
  x86_64) archs=x86_64 ;;
  *) echo "Unsupported Xcode host architecture: $(uname -m)" >&2; exit 1 ;;
esac
export ARCH=$archs
bash scripts/ci/ios-stub-libs.sh

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
  ARCHS="$archs" \
  CODE_SIGNING_ALLOWED=NO

mkdir -p "$out/xctest"
find "$derived" -name '*.xctest' -type d -exec cp -R {} "$out/xctest/" \;
find "$out/xctest" -name '*.xctest' -type d | tee "$out/built.txt"
if [ ! -s "$out/built.txt" ]; then
  echo "xcodebuild produced no XCTest bundles" >&2
  exit 1
fi

if simctl list devices available -j >"$out/simulators.json" 2>"$out/simulators.err"; then
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
  xcodebuild test-without-building \
    -project apps/ios/SimpleX.xcodeproj \
    -scheme "SimpleX (iOS)" \
    -destination "$destination" \
    -derivedDataPath "$derived" \
    -onlyUsePackageVersionsFromResolvedFile \
    -skipPackagePluginValidation \
    -skipMacroValidation \
    CODE_SIGNING_ALLOWED=NO \
    | tee "$out/xcodebuild-test.txt"
  echo "ran $destination" > "$out/simulator-tests.txt"
else
  echo "CoreSimulatorService is not reachable from this Nix build. The XCTest bundles were built and the snapshot preview list was checked." \
    | tee "$out/simulator-tests.txt"
fi
