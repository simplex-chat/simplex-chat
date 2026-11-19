#!/usr/bin/env bash

set -e

ARCH="$(uname -m)"

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
vlc_dir=$root_dir/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-${ARCH}/vlc

mkdir $vlc_dir || exit 0

vlc_tag='v3.0.23-2'
vlc_url="https://github.com/simplex-chat/vlc/releases/download/${vlc_tag}/vlc-linux-${ARCH}.appimage"
case "$ARCH" in
  x86_64) vlc_sha256=594fc3a603167a977cc0de519e1d5f785037df0db60c57518f88e46606d5bb98 ;;
  aarch64) vlc_sha256=697e3d1d0eaad0729529826021fd78b10eee411ba413866c5ab24ac4ac754ba4 ;;
  *) echo "No VLC bundle for $ARCH"; exit 1 ;;
esac

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT
cd "$tmp_dir"
curl -fsSL --tlsv1.2 "${vlc_url}" -o appimage
echo "${vlc_sha256}  appimage" | sha256sum -c -
chmod +x appimage
./appimage --appimage-extract
cp -r squashfs-root/usr/lib/* $vlc_dir
