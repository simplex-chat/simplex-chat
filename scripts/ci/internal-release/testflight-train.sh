#!/bin/bash

set -e

version=$(scripts/ci/internal-release/build-numbers.sh show HEAD | jq -r .ios.version_name)
released=$(curl -fsS --retry 3 "https://itunes.apple.com/lookup?bundleId=chat.simplex.app" | jq -r '.results[0].version // ""')
if [ "$version" != "$released" ] && [ "$(printf '%s\n' "$released" "$version" | sort -V | tail -n 1)" = "$version" ]; then
  echo "open=true" >> "$GITHUB_OUTPUT"
else
  echo "::notice title=TestFlight::the App Store has $released, so the TestFlight train of $version is closed: the iOS upload is skipped until MARKETING_VERSION is above $released"
fi
