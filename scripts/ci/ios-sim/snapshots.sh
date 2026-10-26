#!/bin/bash

set -euo pipefail

derived=$1
out=$2
mkdir -p "$out"

xcode=(
  -project apps/ios/SimpleX.xcodeproj
  -scheme "SimpleX (iOS)"
  -configuration Debug
  -destination "id=$SIM_UDID"
  -derivedDataPath "$derived"
  -onlyUsePackageVersionsFromResolvedFile
  -skipPackagePluginValidation
  -skipMacroValidation
  -only-testing:"SimpleX Snapshots"
  ARCHS=arm64
  CODE_SIGN_STYLE=Manual
  CODE_SIGN_IDENTITY=-
  DEVELOPMENT_TEAM=
  PROVISIONING_PROFILE_SPECIFIER=
)

xcodebuild build-for-testing -quiet "${xcode[@]}"

for size in large extra-extra-extra-large accessibility-medium; do
  xcrun simctl ui "$SIM_UDID" content_size "$size"
  TEST_RUNNER_SNAPSHOT_DIR="$out" TEST_RUNNER_SNAPSHOT_SIZE="$size" xcodebuild test-without-building "${xcode[@]}"
done
xcrun simctl ui "$SIM_UDID" content_size large

ls -l "$out"
