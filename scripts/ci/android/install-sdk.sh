#!/bin/bash

# Installs a pinned emulator, platform-tools and Google APIs x86_64 system image from Google's repository, each
# checked against the SHA-1 its repository manifest lists, and creates the AVD "ci" for that image.
# Usage: scripts/ci/android/install-sdk.sh <api level> <sdk dir>
# Puts ANDROID_SDK_ROOT, ANDROID_HOME and ANDROID_AVD_HOME in GITHUB_ENV and the emulator and adb on GITHUB_PATH.

set -euo pipefail

api=$1
mkdir -p "$2"
sdk=$(cd "$2" && pwd)
repo=https://dl.google.com/android/repository

case "$api" in
  30) image=x86_64-30_r16.zip image_sha1=6ae21030eaadc041078444d3798e4b399f3e787d ;;
  34) image=x86_64-34_r14.zip image_sha1=e0f6c9a0691aa27bd597d0deb1bcfdc943ac8ca7 ;;
  35) image=x86_64-35_r09.zip image_sha1=0103e6dab21290c4b9d16550a3ce99476f884eef ;;
  *)
    echo "error: no pinned system image for API $api"
    exit 1
    ;;
esac

fetch() {
  local url=$1 sha1=$2 file
  file=$sdk/downloads/$(basename "$url")
  mkdir -p "$sdk/downloads"
  curl --proto '=https' --tlsv1.2 -sSfL --retry 3 -o "$file" "$url"
  echo "$sha1  $file" | sha1sum -c >&2
  echo "sha256 $(sha256sum "$file" | cut -d ' ' -f 1) $(basename "$url")" >&2
  echo "$file"
}

install() {
  local dest=$1 zip
  zip=$(fetch "$2" "$3")
  mkdir -p "$dest"
  unzip -q "$zip" -d "$dest"
  rm "$zip"
}

# emulator 37.2.12 and platform-tools 37.0.1, the stable channel on 2026-10-19
install "$sdk" "$repo/emulator-linux_x64-16428233.zip" cd7362ea55dfb86a418958138dc396e74165dd01
install "$sdk" "$repo/platform-tools_r37.0.1-linux.zip" 477254aa5f903c15cf51001717bdf347fb6b53e0
install "$sdk/system-images/android-$api/google_apis" "$repo/sys-img/google_apis/$image" "$image_sha1"

avd_home=$sdk/avd
mkdir -p "$avd_home/ci.avd"
cat > "$avd_home/ci.ini" << EOF
avd.ini.encoding=UTF-8
path=$avd_home/ci.avd
target=android-$api
EOF
cat > "$avd_home/ci.avd/config.ini" << EOF
avd.ini.encoding=UTF-8
AvdId=ci
abi.type=x86_64
hw.cpu.arch=x86_64
hw.cpu.ncore=4
hw.ramSize=4096
disk.dataPartition.size=6G
hw.lcd.width=1080
hw.lcd.height=2400
hw.lcd.density=420
hw.keyboard=yes
hw.gpu.enabled=yes
hw.gpu.mode=swiftshader_indirect
image.sysdir.1=system-images/android-$api/google_apis/x86_64/
tag.id=google_apis
PlayStore.enabled=false
EOF

if ldd "$sdk/emulator/qemu/linux-x86_64/qemu-system-x86_64" | grep 'not found'; then
  echo "error: the emulator needs these libraries"
  exit 1
fi
"$sdk/emulator/emulator" -version | head -n 1
"$sdk/platform-tools/adb" version | head -n 2
grep -E '^(ro.build.version.sdk|ro.product.cpu.abilist|ro.system.product.cpu.abilist)=' "$sdk/system-images/android-$api/google_apis/x86_64/build.prop" || true
du -sh "$sdk"

if [ -n "${GITHUB_ENV:-}" ]; then
  {
    echo "ANDROID_SDK_ROOT=$sdk"
    echo "ANDROID_HOME=$sdk"
    echo "ANDROID_AVD_HOME=$avd_home"
  } >> "$GITHUB_ENV"
  echo "$sdk/emulator" >> "$GITHUB_PATH"
  echo "$sdk/platform-tools" >> "$GITHUB_PATH"
fi
