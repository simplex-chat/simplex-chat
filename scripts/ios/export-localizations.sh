#!/bin/sh

set -e

langs=( cs de es fr it nl ru zh-Hans )

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
