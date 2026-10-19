#!/bin/bash

# Downloads the released arm64 APK at the branch's latest release tag and checks that SimpleX signed it.
# Usage: scripts/ci/android/release-apk.sh <dir>
# Prints the APK's path.

set -euo pipefail

# the release signing certificate, as scripts/simplex-chat-reproduce-builds-android.sh checks it
simplex_key=3C:52:C4:FD:3C:AD:1C:07:C9:B0:0A:70:80:E3:58:FA:B9:FE:FC:B8:AF:5A:EC:14:77:65:F1:6D:0F:21:AD:85
dir=$1
tag=$(git describe --tags --abbrev=0 --match 'v*' --exclude '*-armv7a' --exclude '*-fdroid')
apk=$dir/simplex-aarch64.apk
echo "release $tag" >&2

mkdir -p "$dir"
curl -fsSL -o "$apk" "https://github.com/simplex-chat/simplex-chat/releases/download/$tag/simplex-aarch64.apk"
apksigner=$(find "$ANDROID_HOME/build-tools" -name apksigner | sort -V | tail -n 1)
signer=$("$apksigner" verify --print-certs "$apk" | sed -n 's/.*certificate SHA-256 digest: //p' | head -n 1 | fold -w2 | paste -sd: | tr '[:lower:]' '[:upper:]')
if [ "$signer" != "$simplex_key" ]; then
  echo "error: $apk is signed by $signer, not SimpleX" >&2
  exit 1
fi
echo "signed by SimpleX: $signer" >&2
echo "$apk"
