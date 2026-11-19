#!/bin/bash

set -euo pipefail

sdk=$1
version=23.1.7779620
archive=android-ndk-r23b-linux.zip
root=android-ndk-r23b
sha1=f47ec4c4badd11e9f593a8450180884a927c330d
sha256=c6e97f9c8cfe5b7be0a9e6c15af8e7a179475b7ded23e2d1c1fa0945d6fb4382
dest=$sdk/ndk/$version

if [ -f "$dest/source.properties" ]; then
  echo "NDK $version is installed in $dest"
  exit 0
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
curl --proto '=https' --tlsv1.2 -sSfL --retry 3 -o "$work/$archive" "https://dl.google.com/android/repository/$archive"
echo "$sha1  $work/$archive" | sha1sum -c
echo "$sha256  $work/$archive" | sha256sum -c
unzip -q "$work/$archive" -d "$work"
grep -qx "Pkg.Revision = $version" "$work/$root/source.properties"
mkdir -p "$sdk/ndk"
mv "$work/$root" "$dest"
echo "NDK $version installed in $dest"
