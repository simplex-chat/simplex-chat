#!/bin/sh

set -e

langs=( en cs de es fr it ja nl pl ru th zh-Hans )

for lang in "${langs[@]}"; do
  echo "***"
  echo "***"
  echo "***"
  echo "*** Importing $lang"
  xcodebuild -importLocalizations \
            -project ./apps/ios/SimpleX.xcodeproj \
            -localizationPath ./apps/ios/SimpleX\ Localizations/$lang.xcloc \
            -disableAutomaticPackageResolution \
            -skipPackageUpdates
  sleep 10
done
