#!/usr/bin/env bash

set -e

ARCH="$(uname -m)"

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
vlc_dir=$root_dir/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-${ARCH}/vlc

mkdir $vlc_dir || exit 0

vlc_tag='v3.0.24-simplex'
vlc_url="https://github.com/simplex-chat/vlc/releases/download/${vlc_tag}/vlc-linux-${ARCH}.appimage"
case "$ARCH" in
  x86_64) vlc_sha256='cdcd71c6bbd35bb4fddadf7cd9aeff25078df79d5d9c6a5e03b8fb196f82ee91' ;;
  aarch64) vlc_sha256='8a4ca4617cd1a16e09f09f0a1e34162cf5fbfd3048e67cccb738a1a476441bbc' ;;
esac

cd /tmp
mkdir tmp 2>/dev/null || true
cd tmp
curl --tlsv1.2 "${vlc_url}" -L -o appimage
echo "${vlc_sha256}  appimage" | sha256sum -c -
chmod +x appimage
./appimage --appimage-extract
chmod -R a+rX squashfs-root/usr/lib
cp -r squashfs-root/usr/lib/* $vlc_dir
cd ../
rm -rf tmp
exit 0


# This is currently unneeded
cd /tmp
(
mkdir tmp
cd tmp
curl --tlsv1.2 https://archive.ubuntu.com/ubuntu/pool/universe/v/vlc/libvlc5_3.0.9.2-1_amd64.deb -o libvlc
ar p libvlc data.tar.xz > data.tar.xz
tar -xvf data.tar.xz
mv usr/lib/x86_64-linux-gnu/libvlc.so{.5,}
cp usr/lib/x86_64-linux-gnu/libvlc.so* $vlc_dir
cd ../
rm -rf tmp
)

(
mkdir tmp
cd tmp
curl --tlsv1.2 https://archive.ubuntu.com/ubuntu/pool/universe/v/vlc/libvlccore9_3.0.9.2-1_amd64.deb -o libvlccore
ar p libvlccore data.tar.xz > data.tar.xz
tar -xvf data.tar.xz
cp usr/lib/x86_64-linux-gnu/libvlccore.so* $vlc_dir
cd ../
rm -rf tmp
)

(
mkdir tmp
cd tmp
curl --tlsv1.2 https://mirrors.edge.kernel.org/ubuntu/pool/universe/v/vlc/vlc-plugin-base_3.0.9.2-1_amd64.deb -o plugins
ar p plugins data.tar.xz > data.tar.xz
tar -xvf data.tar.xz
find usr/lib/x86_64-linux-gnu/vlc/plugins/ -name "lib*.so*" -exec patchelf --set-rpath '$ORIGIN/../../' {} \;
cp -r usr/lib/x86_64-linux-gnu/vlc/{libvlc*,plugins} $vlc_dir
cd ../
rm -rf tmp
)

(
mkdir tmp
cd tmp
curl --tlsv1.2 https://archive.ubuntu.com/ubuntu/pool/main/libi/libidn/libidn11_1.33-2.2ubuntu2_amd64.deb -o idn
ar p idn data.tar.xz > data.tar.xz
tar -xvf data.tar.xz
cp lib/x86_64-linux-gnu/lib* $vlc_dir
cd ../
rm -rf tmp
)

find $vlc_dir -maxdepth 1 -name "lib*.so*" -exec patchelf --set-rpath '$ORIGIN' {} \;
