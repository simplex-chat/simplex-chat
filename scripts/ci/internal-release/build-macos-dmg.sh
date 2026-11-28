#!/bin/bash

set -e

arch=$1
compose=$2
core_hash=$3
libs=apps/multiplatform/common/src/commonMain/cpp/desktop/libs/mac-$arch
mkdir -p "$libs" apps/multiplatform/build/links
tar -C "$libs" -xzf core/desktop-core.tar.gz
ln -sfn "../../common/src/commonMain/cpp/desktop/libs/mac-$arch/" "apps/multiplatform/build/links/macos-$compose"
scripts/desktop/prepare-vlc-mac.sh
echo "$core_hash" > "$libs/core-hash"
if [ -n "$APPLE_SIMPLEX_SIGNING_KEYCHAIN" ]; then
  scripts/ci/build-desktop-mac.sh
else
  echo "::notice title=macOS::APPLE_SIMPLEX_* are not set: the dmg is not signed"
  (cd apps/multiplatform && ./gradlew -Psimplex.assets.dir="$ASSETS_DIR" packageDmg)
fi
mkdir out
cp apps/multiplatform/release/main/dmg/SimpleX-*.dmg "out/simplex-desktop-macos-$arch.dmg"
app=apps/multiplatform/release/main/app/SimpleX.app/Contents
python3 scripts/ci/internal-release/ui-zip.py "$app/app" "$app/runtime/Contents/Home/release" out
