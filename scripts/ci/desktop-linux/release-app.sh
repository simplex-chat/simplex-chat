#!/bin/bash

# Downloads the released desktop app for this runner's Ubuntu at the branch's latest release tag, and unpacks it.
# Usage: scripts/ci/desktop-linux/release-app.sh <dir>
# The app is then in <dir>/app/opt/simplex, laid out as release/main/app/simplex of a branch build.

set -euo pipefail

dir=$1
tag=$(git describe --tags --abbrev=0 --match 'v*' --exclude '*-armv7a' --exclude '*-fdroid')
. /etc/os-release
asset=simplex-desktop-ubuntu-${VERSION_ID/./_}-$(uname -m).deb
echo "release $tag, asset $asset"

mkdir -p "$dir"
curl -fsSL -o "$dir/$asset" "https://github.com/simplex-chat/simplex-chat/releases/download/$tag/$asset"
dpkg-deb -x "$dir/$asset" "$dir/app"
rm "$dir/$asset"
ls -la "$dir/app/opt/simplex/bin"
du -sh "$dir/app/opt/simplex"
