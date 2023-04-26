#!/usr/bin/env bash
# Safety measures
set -euo pipefail
# shellcheck disable=SC2154
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

langs=( en cs de es fr it nl pl ru zh-Hans )

for lang in "${langs[@]}"; do
  echo "***"
  echo "***"
  echo "***"
  echo "*** Importing $lang"
  xcodebuild -importLocalizations \
            -project ./apps/ios/SimpleX.xcodeproj \
            -localizationPath ./apps/ios/SimpleX\ Localizations/"$lang".xcloc \
            -disableAutomaticPackageResolution \
            -skipPackageUpdates
  sleep 10
done
