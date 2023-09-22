#!/bin/bash

OS=windows
ARCH=${1:-`uname -a | rev | cut -d' ' -f2 | rev`}
GHC_VERSION=9.6.2

BUILD_DIR=dist-newstyle/build/$ARCH-$OS/ghc-${GHC_VERSION}/simplex-chat-*

#rm -rf $BUILD_DIR
rm cabal.project.local
echo "ignore-project: False" > cabal.project.local
#echo "package direct-sqlcipher" >> cabal.project.local
#echo "    flags: +openssl" >> cabal.project.local
#echo "    extra-include-dirs: `pwd`/openssl-3/x64/include" >> cabal.project.local
#echo "    extra-lib-dirs: `pwd`/openssl-3/x64/lib" >> cabal.project.local
echo "package simplex-chat" >> cabal.project.local
echo "    ghc-options: -shared -o libsimplex.dll -optl-LC:\Users\me\Documents\openssl-1.1.1w -flink-rts -threaded -optl-lffi" >> cabal.project.local
cabal build lib:simplex-chat --enable-shared --ghc-options="-shared"
#cd $BUILD_DIR/build
#mkdir deps
#ldd libHSsimplex-chat-*-inplace-ghc${GHC_VERSION}.so | grep "ghc" | cut -d' ' -f 3 | xargs -I {} cp {} ./deps/

#cd -

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/src/jvmMain/resources/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
#cp -r $BUILD_DIR/build/deps apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp libsimplex.dll apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
