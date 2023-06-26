#!/bin/bash

OS=linux
ARCH=${1:-`uname -a | rev | cut -d' ' -f2 | rev`}
GHC_VERSION=8.10.7

rm -rf dist-newstyle/build/$ARCH-$OS/ghc-${GHC_VERSION}/simplex-chat-*
cabal build lib:simplex-chat --ghc-options='-optl-Wl,-rpath,$ORIGIN' --ghc-options="-optl-L$(ghc --print-libdir)/rts -optl-Wl,--as-needed,-lHSrts_thr-ghc$GHC_VERSION"
cd dist-newstyle/build/$ARCH-$OS/ghc-${GHC_VERSION}/simplex-chat-*/build
#patchelf --add-needed libHSrts_thr-ghc${GHC_VERSION}.so libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so
#patchelf --add-rpath '$ORIGIN' libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so
mkdir deps
ldd libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so | grep "ghc" | cut -d' ' -f 3 | xargs -I {} cp {} ./deps/
mkdir -p ../../../../../../apps/android/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp -r deps ../../../../../../apps/android/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so ../../../../../../apps/android/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
