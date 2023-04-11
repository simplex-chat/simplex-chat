#!/bin/sh

set -e

langs=( cs de es fr it nl ru zh-Hans )

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
  sleep 2
done
