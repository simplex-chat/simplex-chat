#!/bin/bash

# Unpacks the core package from build-core.sh where scripts/desktop/build-lib-linux.sh leaves the libraries,
# and links them as the desktop app's resources, as that script does.
# Usage: scripts/ci/desktop-linux/install-core.sh <core .tar.gz>

set -euo pipefail

arch=$(uname -m)
case "$arch" in
  aarch64) compose_arch=arm64 ;;
  *) compose_arch=x64 ;;
esac
libs_dir=apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-$arch

mkdir -p "$libs_dir"
tar -C "$libs_dir" -xzf "$1"
ls -la "$libs_dir"

mkdir -p apps/multiplatform/build/links
ln -sfT "../../common/src/commonMain/cpp/desktop/libs/linux-$arch/" "apps/multiplatform/build/links/linux-$compose_arch"
ls -la apps/multiplatform/build/links
