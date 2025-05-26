#!/bin/bash

set -e

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
vlc_dir=$root_dir/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/windows-x86_64/vlc
rm -rf $vlc_dir
mkdir -p $vlc_dir/vlc || exit 0

vlc_tag='v3.0.21-1'
vlc_url="https://github.com/simplex-chat/vlc/releases/download/${vlc_tag}/vlc-win-x86_64.zip"

cd /tmp
mkdir tmp 2>/dev/null || true
cd tmp
curl --tlsv1.2 "${vlc_url}" -L -o vlc
$WINDIR\\System32\\tar.exe -xf vlc
cd vlc-*
# Setting the same date as the date that will be on the file after extraction from JAR to make VLC cache checker happy
find plugins | grep ".dll" | xargs touch -m -d "1970-01-01T00:00:00Z"
./vlc-cache-gen plugins
cp *.dll $vlc_dir/
cp -r -p plugins/ $vlc_dir/vlc/plugins
cd ../../
rm -rf tmp
