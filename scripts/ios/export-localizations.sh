#!/bin/sh

set -e

langs=( en bg cs de es fi fr it ja nl pl ru uk zh-Hans )

for lang in "${langs[@]}"; do
  echo "***"
  echo "***"
  echo "***"
  echo "*** Exporting $lang"
  xcodebuild -exportLocalizations \
            -project ./apps/ios/SimpleX.xcodeproj \
            -localizationPath ./apps/ios/SimpleX\ Localizations \
            -exportLanguage $lang
  sleep 2
done
