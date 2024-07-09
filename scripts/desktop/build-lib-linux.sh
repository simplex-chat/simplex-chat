#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

OS=linux
ARCH=${1:-`uname -a | rev | cut -d' ' -f2 | rev`}
GHC_VERSION=9.6.3

if [ "$ARCH" == "aarch64" ]; then
    COMPOSE_ARCH=arm64
else
    COMPOSE_ARCH=x64
fi

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
cd $root_dir
BUILD_DIR=dist-newstyle/build/$ARCH-$OS/ghc-${GHC_VERSION}/simplex-chat-*

exports=( $(sed 's/foreign export ccall "chat_migrate_init_key"//' src/Simplex/Chat/Mobile.hs | sed 's/foreign export ccall "chat_reopen_store"//' |grep "foreign export ccall" | cut -d '"' -f2) )
for elem in "${exports[@]}"; do count=$(grep -R "$elem$" libsimplex.dll.def | wc -l); if [ $count -ne 1 ]; then echo Wrong exports in libsimplex.dll.def. Add \"$elem\" to that file; exit 1; fi ; done
for elem in "${exports[@]}"; do count=$(grep -R "\"$elem\"" flake.nix | wc -l); if [ $count -ne 2 ]; then echo Wrong exports in flake.nix. Add \"$elem\" in two places of the file; exit 1; fi ; done

rm -rf $BUILD_DIR
cabal build lib:simplex-chat --ghc-options='-optl-Wl,-rpath,$ORIGIN -flink-rts -threaded'
cd $BUILD_DIR/build
#patchelf --add-needed libHSrts_thr-ghc${GHC_VERSION}.so libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so
#patchelf --add-rpath '$ORIGIN' libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so

# GitHub's Ubuntu 20.04 runner started to set libffi.so.7 as a dependency while Ubuntu 20.04 on user's devices may not have it
# but libffi.so.8 is shipped as an external library with other libs
patchelf --replace-needed "libffi.so.7" "libffi.so.8" libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so

mkdir deps 2> /dev/null || true
ldd libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so | grep "ghc" | cut -d' ' -f 3 | xargs -I {} cp {} ./deps/

cd -

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp -r $BUILD_DIR/build/deps/* apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp $BUILD_DIR/build/libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
scripts/desktop/prepare-vlc-linux.sh

links_dir=apps/multiplatform/build/links
mkdir -p $links_dir
cd $links_dir
ln -sfT ../../common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/ $OS-$COMPOSE_ARCH
