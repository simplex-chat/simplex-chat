#!/bin/bash

set -e

ARCH="${1:-`uname -a | rev | cut -d' ' -f1 | rev`}"
if [ "$ARCH" == "arm64" ]; then
    ARCH=aarch64
    vlc_arch=arm64
else
    vlc_arch=intel64
fi
vlc_version=3.0.19

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
vlc_dir=$root_dir/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/mac-$ARCH/vlc
#rm -rf $vlc_dir
mkdir -p $vlc_dir/vlc || exit 0

cd /tmp
mkdir tmp 2>/dev/null || true
cd tmp
curl --tlsv1.2 https://github.com/simplex-chat/vlc/releases/download/v$vlc_version/vlc-macos-$ARCH.zip -L -o vlc
unzip -oqq vlc
install_name_tool -add_rpath "@loader_path/VLC.app/Contents/MacOS/lib" vlc-cache-gen
cd VLC.app/Contents/MacOS/lib
for lib in $(ls *.dylib); do install_name_tool -add_rpath "@loader_path" $lib 2> /dev/null || true; done
cd ../plugins
for lib in $(ls *.dylib); do
    install_name_tool -add_rpath "@loader_path/../../" $lib 2> /dev/null || true
done
cd ..
../../../vlc-cache-gen plugins
cp lib/* $vlc_dir/
cp -r -p plugins/ $vlc_dir/vlc/plugins
cd ../../../../
rm -rf tmp
