#!/bin/bash

set -e

ARCH="${1:-$(uname -m)}" 
if [ "$ARCH" == "arm64" ]; then
    ARCH=aarch64
    vlc_arch=arm64
else
    vlc_arch=intel64
fi

vlc_tag='v3.0.24-simplex'
vlc_url="https://github.com/simplex-chat/vlc/releases/download/${vlc_tag}/vlc-macos-${ARCH}.zip"
case "$ARCH" in
  aarch64) vlc_sha256='69f72b2bb250c07e44a58bbaa6b80b48641d11f5cfe92b9b45656296852a8e47' ;;
  x86_64) vlc_sha256='a74b895d194472249b24eeb27af235ac12d2d7d6c1f9c4e442aa7d6934f03abd' ;;
esac

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
curl --tlsv1.2 "${vlc_url}" -L -o vlc
echo "${vlc_sha256}  vlc" | shasum -a 256 -c -
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
