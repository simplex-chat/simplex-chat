#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"

OS=windows
ARCH=${1:-x86_64}
if [ "$ARCH" == "aarch64" ]; then
    COMPOSE_ARCH=arm64
else
    COMPOSE_ARCH=x64
fi

BUILD_DIR=dist-newstyle/build/$ARCH-$OS/ghc-*/simplex-chat-*

# IMPORTANT: in order to get a working build you should use x86_64 MinGW with make, cmake, gcc.
# 100% working MinGW is https://github.com/brechtsanders/winlibs_mingw/releases/download/13.1.0-16.0.5-11.0.0-ucrt-r5/winlibs-x86_64-posix-seh-gcc-13.1.0-mingw-w64ucrt-11.0.0-r5.zip
# Many other distributions I tested don't work in some cases or don't have required tools.
# Also, standalone Cmake installed globally via .msi package does not produce working library, you should use MinGW's Cmake.
# Example of export:
# export PATH=/c/MinGW/bin:/c/ghcup/bin:/c/Program\ Files/Amazon\ Corretto/jdk17.0.9_8/bin/:$PATH
# If you use Msys2, use UCRT64 (NOT Mingw64, because it will crash on launch because of non-posix threads), install these packages:
# pacman -S perl make mingw-w64-ucrt-x86_64-cmake mingw-w64-ucrt-x86_64-gcc
# and export path to ghcup/bin and java

cd $root_dir
mkdir dist-newstyle 2>/dev/null || true

if [ ! -f dist-newstyle/openssl-1.1.1w/libcrypto-1_1-x64.dll ]; then
	cd dist-newstyle
    curl https://www.openssl.org/source/openssl-1.1.1w.tar.gz -o openssl.tar.gz
    $WINDIR\\System32\\tar.exe -xvzf openssl.tar.gz
	cd openssl-1.1.1w
	./Configure mingw64
	make
	cd ../../
fi
openssl_windows_style_path=$(echo `pwd`/dist-newstyle/openssl-1.1.1w | sed 's#/\([a-z]\)#\1:#' | sed 's#/#\\#g')
rm -rf $BUILD_DIR 2>/dev/null || true
# Existence of this directory produces build error: cabal's bug
rm -rf dist-newstyle/src/direct-sq* 2>/dev/null || true
rm cabal.project.local 2>/dev/null || true
echo "ignore-project: False" >> cabal.project.local
echo "package direct-sqlcipher" >> cabal.project.local
echo "    flags: +openssl" >> cabal.project.local
echo "    extra-include-dirs: $openssl_windows_style_path\include" >> cabal.project.local
echo "    extra-lib-dirs: $openssl_windows_style_path" >> cabal.project.local
echo "package simplex-chat" >> cabal.project.local
echo "    ghc-options: -shared -threaded -optl-L$openssl_windows_style_path -optl-lcrypto-1_1-x64 -o libsimplex.dll libsimplex.dll.def" >> cabal.project.local
# Very important! Without it the build fails on linking step since the linker can't find exported symbols.
# It looks like GHC bug because with such random path the build ends successfully
sed -i "s/ld.lld.exe/abracadabra.exe/" `ghc --print-libdir`/settings
cabal build lib:simplex-chat

rm -rf apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
rm -rf apps/multiplatform/desktop/build/cmake

mkdir -p apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp dist-newstyle/openssl-1.1.1w/libcrypto-1_1-x64.dll apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/
cp libsimplex.dll apps/multiplatform/common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/

scripts/desktop/prepare-vlc-windows.sh

links_dir=apps/multiplatform/build/links
mkdir -p $links_dir
cd $links_dir
rm -rf $OS-$COMPOSE_ARCH
ln -sfT ../../common/src/commonMain/cpp/desktop/libs/$OS-$ARCH/ $OS-$COMPOSE_ARCH
