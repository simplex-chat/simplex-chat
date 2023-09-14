#!/bin/bash

set -e

ARCH="${1:-`uname -a | rev | cut -d' ' -f1 | rev`}"
if [ "$ARCH" == "arm64" ]; then
    ARCH=aarch64
fi
if [ "$ARCH" == "arm64" ]; then
    vlc_arch=arm64
else
    vlc_arch=intel64
fi
vlc_version=3.0.18

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
vlc_dir=$root_dir/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/mac-$ARCH/deps/vlc
#rm -rf $vlc_dir
mkdir -p $vlc_dir/vlc || exit 0

cd /tmp
mkdir tmp 2>/dev/null || true
cd tmp
export PATH=$PATH:$(pwd)
7zz -h > /dev/null 2>/dev/null || (curl https://7-zip.org/a/7z2301-mac.tar.xz -L -o 7z.tar.xz && tar -xvf 7z.tar.xz 2>/dev/null)
curl https://mirror.freedif.org/videolan/vlc/$vlc_version/macosx/vlc-$vlc_version-$vlc_arch.dmg -L -o vlc
7zz -y -xr!"VLC media player/Applications" x vlc > /dev/null

cp VLC\ media\ player/VLC.app/Contents/MacOS/lib/* $vlc_dir/
cp -r VLC\ media\ player/VLC.app/Contents/MacOS/plugins/ $vlc_dir/vlc/plugins
cd ../
rm -rf tmp

cd $vlc_dir
for lib in $(ls *.dylib); do install_name_tool -add_rpath "@loader_path" $lib 2> /dev/null; done
cd vlc/plugins
for lib in $(ls *.dylib); do install_name_tool -add_rpath "@loader_path/../../" $lib 2> /dev/null; done
