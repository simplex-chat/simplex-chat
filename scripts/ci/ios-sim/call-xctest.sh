#!/bin/bash

# Runs the SimpleX Calls tests (apps/ios/SimpleX Calls) in a new simulator: two call clients in the app's process make
# an encrypted video call. The app needs sounds/video.mp4 (test-video.swift) in place of the camera.
# Usage: scripts/ci/ios-sim/call-xctest.sh <derived data of the app's build> <output dir>

set -euo pipefail

derived=$1
out=$2
# shellcheck source=scripts/ci/ios-sim/sim-lib.sh
. "$(dirname "$0")/sim-lib.sh"
mkdir -p "$out"
sim=$(new_simulator ci-calls)
trap 'xcrun simctl shutdown "$sim" || true' EXIT

xcode=(
  -project apps/ios/SimpleX.xcodeproj
  -scheme "SimpleX (iOS)"
  -configuration Debug
  -destination "id=$sim"
  -derivedDataPath "$derived"
  -onlyUsePackageVersionsFromResolvedFile
  -skipPackagePluginValidation
  -skipMacroValidation
  -only-testing:"SimpleX Calls"
  ARCHS=arm64
  CODE_SIGN_STYLE=Manual
  CODE_SIGN_IDENTITY=-
  DEVELOPMENT_TEAM=
  PROVISIONING_PROFILE_SPECIFIER=
)

xcodebuild build-for-testing -quiet "${xcode[@]}"
xcrun simctl install "$sim" "$derived/Build/Products/Debug-iphonesimulator/SimpleX.app"
xcrun simctl privacy "$sim" grant microphone "$bundle_id"
xcrun simctl privacy "$sim" grant camera "$bundle_id"
touch "$out/started"
status=0
# a hung test fails after 10 minutes, rather than with the job's limit (run 41295737388)
xcodebuild test-without-building -resultBundlePath "$out/CallTests.xcresult" \
  -test-timeouts-enabled YES -default-test-execution-time-allowance 600 -maximum-test-execution-time-allowance 600 \
  "${xcode[@]}" || status=$?

# the test aborts the app when ending a call hangs: its crash report has every thread's stack
find ~/Library/Logs/DiagnosticReports -name 'SimpleX*' -newer "$out/started" 2> /dev/null | while read -r report; do
  cp "$report" "$out/"
  echo "::group::crash report $report"
  cat "$report"
  echo "::endgroup::"
done
exit "$status"
