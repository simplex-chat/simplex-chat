#!/bin/sh

set -eu

os=$1
arch=$2
core_hash=$3
scripts/ci/desktop-linux/install-core.sh core/desktop-core.tar.gz
scripts/desktop/prepare-vlc-linux.sh
echo "$core_hash" > "apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-$arch/core-hash"
SKIP_LIB=1 ASSETS_DIR=../../assets scripts/desktop/make-deb-linux.sh
[ "$os" != 22.04 ] || ASSETS_DIR=../../assets scripts/desktop/make-appimage-linux.sh
