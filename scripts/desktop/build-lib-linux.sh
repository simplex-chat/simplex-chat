#!/usr/bin/env bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

OS=linux
ARCH="$(uname -m)"
DATABASE_BACKEND="${1:-sqlite}"
GHC_VERSION=$(ghc --numeric-version)

if [ "$ARCH" == "aarch64" ]; then
    COMPOSE_ARCH=arm64
else
    COMPOSE_ARCH=x64
fi

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
cd $root_dir
BUILD_DIR=dist-newstyle/build/$ARCH-$OS/ghc-${GHC_VERSION}/simplex-chat-*

scripts/desktop/check-exports.sh

#rm -rf $BUILD_DIR
if [[ "$DATABASE_BACKEND" == "postgres" ]]; then
    echo "Building with postgres backend..."
    cabal build lib:simplex-chat --ghc-options='-optl-Wl,-rpath,$ORIGIN -optl-Wl,-soname,libsimplex.so -flink-rts -threaded' --constraint 'simplexmq +client_library +client_postgres' --constraint 'simplex-chat +client_library +client_postgres'
else
    echo "Building with sqlite backend..."
    cabal build lib:simplex-chat --ghc-options='-optl-Wl,-rpath,$ORIGIN -optl-Wl,-soname,libsimplex.so -flink-rts -threaded' --constraint 'simplexmq +client_library' --constraint 'simplex-chat +client_library'
fi
cd $BUILD_DIR/build
mv libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so libsimplex.so 2> /dev/null || true
#patchelf --add-needed libHSrts_thr-ghc${GHC_VERSION}.so libsimplex.so
#patchelf --add-rpath '$ORIGIN' libsimplex.so

# GitHub's Ubuntu 20.04 runner started to set libffi.so.7 as a dependency while Ubuntu 20.04 on user's devices may not have it
# but libffi.so.8 is shipped as an external library with other libs
patchelf --replace-needed "libffi.so.7" "libffi.so.8" libsimplex.so

mkdir deps 2> /dev/null || true
ldd libsimplex.so | grep "ghc" | cut -d' ' -f 3 | xargs -I {} cp {} ./deps/

cd -

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp -r $BUILD_DIR/build/deps/* apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp $BUILD_DIR/build/libsimplex.so apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
[ -n "$SKIP_VLC" ] || scripts/desktop/prepare-vlc-linux.sh

links_dir=apps/multiplatform/build/links
mkdir -p $links_dir
cd $links_dir
ln -sfT ../../common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/ $OS-$COMPOSE_ARCH
