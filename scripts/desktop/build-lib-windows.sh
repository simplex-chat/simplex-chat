#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"

OS=windows
ARCH="x86_64"
JOB_REPO=${1:-$SIMPLEX_CI_REPO_URL}

if [ "$ARCH" == "aarch64" ]; then
    COMPOSE_ARCH=arm64
else
    COMPOSE_ARCH=x64
fi

cd $root_dir

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
scripts/desktop/download-lib-windows.sh $JOB_REPO
scripts/desktop/prepare-vlc-windows.sh

links_dir=apps/multiplatform/build/links
mkdir -p $links_dir
cd $links_dir
rm -rf $OS-$COMPOSE_ARCH
ln -sfT ../../common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/ $OS-$COMPOSE_ARCH
