#!/usr/bin/env bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

OS=linux
ARCH="$(uname -m)"
DATABASE_BACKEND="${1:-sqlite}"
GHC_VERSION=9.6.3

if [ "$ARCH" == "aarch64" ]; then
    COMPOSE_ARCH=arm64
else
    COMPOSE_ARCH=x64
fi

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
cd $root_dir
DIST_DIR="${DIST_DIR:-dist-newstyle}"
BUILD_DIR=$DIST_DIR/build/$ARCH-$OS/ghc-${GHC_VERSION}/simplex-chat-*

exports=( $(sed 's/foreign export ccall "chat_migrate_init_key"//' src/Simplex/Chat/Mobile.hs | sed 's/foreign export ccall "chat_reopen_store"//' |grep "foreign export ccall" | cut -d '"' -f2) )
for elem in "${exports[@]}"; do count=$(grep -R "$elem$" libsimplex.dll.def | wc -l); if [ $count -ne 1 ]; then echo Wrong exports in libsimplex.dll.def. Add \"$elem\" to that file; exit 1; fi ; done
for elem in "${exports[@]}"; do count=$(grep -R "\"$elem\"" flake.nix | wc -l); if [ $count -ne 1 ]; then echo Wrong exports in flake.nix. Add \"$elem\" to aarch64-android:lib:simplex-chat; exit 1; fi ; done

#rm -rf $BUILD_DIR
if [[ "$DATABASE_BACKEND" == "postgres" ]]; then
    echo "Building with postgres backend..."
    cabal build --builddir="$DIST_DIR" lib:simplex-chat --ghc-options='-optl-Wl,-rpath,$ORIGIN -optl-Wl,-soname,libsimplex.so -flink-rts -threaded' --constraint 'simplexmq +client_library +client_postgres' --constraint 'simplex-chat +client_library +client_postgres' --constraint 'jpeg-turbo +static-gcc'
else
    echo "Building with sqlite backend..."
    cabal build --builddir="$DIST_DIR" lib:simplex-chat --ghc-options='-optl-Wl,-rpath,$ORIGIN -optl-Wl,-soname,libsimplex.so -flink-rts -threaded' --constraint 'simplexmq +client_library' --constraint 'simplex-chat +client_library' --constraint 'jpeg-turbo +static-gcc'
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
