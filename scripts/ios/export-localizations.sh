#!/usr/bin/env bash
# Safety measures
set -euo pipefail
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

langs=( en cs de es fr it nl pl ru zh-Hans )

for lang in "${langs[@]}"; do
  echo "***"
  echo "***"
  echo "***"
  echo "*** Exporting $lang"
  xcodebuild -exportLocalizations \
            -project ./apps/ios/SimpleX.xcodeproj \
            -localizationPath ./apps/ios/SimpleX\ Localizations \
            -exportLanguage "$lang"
  sleep 2
done
