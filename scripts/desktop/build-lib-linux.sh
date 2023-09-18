#!/bin/bash

OS=linux
ARCH=${1:-`uname -a | rev | cut -d' ' -f2 | rev`}
GHC_VERSION=9.6.2

BUILD_DIR=dist-newstyle/build/$ARCH-$OS/ghc-${GHC_VERSION}/simplex-chat-*

rm -rf $BUILD_DIR
cabal build lib:simplex-chat --ghc-options='-optl-Wl,-rpath,$ORIGIN -flink-rts -threaded'
cd $BUILD_DIR/build
#patchelf --add-needed libHSrts_thr-ghc${GHC_VERSION}.so libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so
#patchelf --add-rpath '$ORIGIN' libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so
mkdir deps
ldd libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so | grep "ghc" | cut -d' ' -f 3 | xargs -I {} cp {} ./deps/

cd -

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/src/jvmMain/resources/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp -r $BUILD_DIR/build/deps apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp $BUILD_DIR/build/libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
scripts/desktop/prepare-vlc-linux.sh
