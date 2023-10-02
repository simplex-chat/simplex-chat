#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"

OS=windows
ARCH=`uname -a | rev | cut -d' ' -f2 | rev`
JOB_REPO=${1:-$SIMPLEX_CI_REPO_URL}

cd $root_dir

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/src/jvmMain/resources/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
scripts/desktop/download-lib-windows.sh $JOB_REPO
scripts/desktop/prepare-vlc-windows.sh
