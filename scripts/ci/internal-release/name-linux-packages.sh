#!/bin/bash

set -e

mkdir out
cp "apps/multiplatform/release/main/deb/simplex_$ARCH.deb" "out/simplex-desktop-ubuntu-${OS//./_}-$ARCH.deb"
if [ "$OS" = 22.04 ]; then
  cp apps/multiplatform/release/main/*imple*.AppImage "out/simplex-desktop-$ARCH.AppImage"
fi
app=apps/multiplatform/extracted/opt/simplex
python3 scripts/ci/internal-release/ui-zip.py "$app/lib/app" "$app/lib/runtime/release" out
