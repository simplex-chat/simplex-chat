#!/bin/bash

# Builds the iOS app for the simulator into <derived data>/Build/Products/Debug-iphonesimulator/SimpleX.app.
# With a git ref, it builds that ref's apps/ios, with its WebRTC, against the core library this branch prepared,
# then restores the branch's apps/ios. The call test calls between this app and the branch's, so a branch
# that changes the core library can't use it: the ref's Swift code would run against the branch's core.
# Usage: scripts/ci/ios-sim/build-app.sh <derived data> [git ref]

set -euo pipefail

derived_data=$1
ref=${2:-}

if [ -n "$ref" ]; then
  git fetch --depth 1 origin "$ref"
  echo "building apps/ios of $ref ($(git rev-parse --short FETCH_HEAD))"
  trap 'git checkout HEAD -- apps/ios && scripts/ios/update-pbxproj.sh' EXIT
  git checkout FETCH_HEAD -- apps/ios
  scripts/ios/update-pbxproj.sh
fi

xcodebuild build -quiet \
  -project apps/ios/SimpleX.xcodeproj \
  -scheme "SimpleX (iOS)" \
  -configuration Debug \
  -destination 'generic/platform=iOS Simulator' \
  -derivedDataPath "$derived_data" \
  -onlyUsePackageVersionsFromResolvedFile \
  -skipPackagePluginValidation \
  -skipMacroValidation \
  ARCHS=arm64 \
  CODE_SIGN_STYLE=Manual \
  CODE_SIGN_IDENTITY=- \
  DEVELOPMENT_TEAM= \
  PROVISIONING_PROFILE_SPECIFIER=
