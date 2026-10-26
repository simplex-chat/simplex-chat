#!/bin/bash

set -euo pipefail

derived=$1
out=$2
mkdir -p "$out"

opts=(
  -project apps/ios/SimpleX.xcodeproj
  -scheme "SimpleX (iOS)"
  -configuration Debug
  -derivedDataPath "$derived"
  -onlyUsePackageVersionsFromResolvedFile
  -skipPackagePluginValidation
  -skipMacroValidation
)
settings=(
  ARCHS=arm64
  CODE_SIGN_STYLE=Manual
  CODE_SIGN_IDENTITY=-
  DEVELOPMENT_TEAM=
  PROVISIONING_PROFILE_SPECIFIER=
)

xcodebuild build-for-testing -quiet "${opts[@]}" -destination "id=$SIM_UDID" -only-testing:"SimpleX Snapshots" "${settings[@]}"
xcrun simctl shutdown "$SIM_UDID" || true

runtime=com.apple.CoreSimulator.SimRuntime.iOS-$(xcrun --sdk iphonesimulator --show-sdk-version | tr . -)
margins="$out/margins"
mkdir -p "$margins"

for type in iPhone-16 iPhone-16-Plus iPhone-12-Pro-Max iPhone-SE--2nd-generation- iPhone-11-Pro iPhone-11-Pro-Max iPhone-11; do
  if ! udid=$(xcrun simctl create "$type" "com.apple.CoreSimulator.SimDeviceType.$type" "$runtime"); then
    echo "$type: not created"
    continue
  fi
  printf '%s\t%s\n' "$udid" "$type" >> "$margins/created.tsv"
  if ! xcrun simctl boot "$udid"; then
    echo "$type: boot failed"
    continue
  fi
  xcrun simctl bootstatus "$udid" -b || true
  TEST_RUNNER_SNAPSHOT_DIR="$margins" xcodebuild test-without-building "${opts[@]}" -destination "id=$udid" -only-testing:"SimpleX Snapshots/SnapshotTests/testListMargins" "${settings[@]}" > "$margins/xcodebuild-$udid.log" 2>&1 || echo "$type: margins test failed"
  xcrun simctl shutdown "$udid" || true
  xcrun simctl delete "$udid" || true
done

ls -l "$out" "$margins"
