#!/bin/bash

# Installs a pinned emulator, platform-tools and the Google APIs x86_64 system image of API 34 from Google's repository,
# each checked against the SHA-1 its repository manifest lists and the SHA-256 recorded when it was pinned, and creates
# the AVD "ci" for that image.
# Usage: scripts/ci/android/install-sdk.sh <sdk dir>
# Puts ANDROID_SDK_ROOT, ANDROID_HOME and ANDROID_AVD_HOME in GITHUB_ENV and the emulator and adb on GITHUB_PATH.

set -euo pipefail

mkdir -p "$1"
sdk=$(cd "$1" && pwd)
repo=https://dl.google.com/android/repository
api=34

install() {
  local dest=$1 url=$2 sha1=$3 sha256=$4 zip
  zip=$sdk/downloads/$(basename "$url")
  mkdir -p "$sdk/downloads"
  curl --proto '=https' --tlsv1.2 -sSfL --retry 3 -o "$zip" "$url"
  echo "$sha1  $zip" | sha1sum -c
  echo "$sha256  $zip" | sha256sum -c
  mkdir -p "$dest"
  unzip -q "$zip" -d "$dest"
  rm "$zip"
}

# emulator 37.2.12 and platform-tools 37.0.1, the stable channel on 2026-10-19
install "$sdk" "$repo/emulator-linux_x64-16428233.zip" \
  cd7362ea55dfb86a418958138dc396e74165dd01 b08fc43d8608d2955f607f1b287beb525041e730086bdca3af152accac3af9c1
install "$sdk" "$repo/platform-tools_r37.0.1-linux.zip" \
  477254aa5f903c15cf51001717bdf347fb6b53e0 d230f13842f60f782a8645f9c813f8f845bf36089ea7289f28c48f17979313f1
install "$sdk/system-images/android-$api/google_apis" "$repo/sys-img/google_apis/x86_64-34_r14.zip" \
  e0f6c9a0691aa27bd597d0deb1bcfdc943ac8ca7 783a40134baf4f3012d4464fbe1571b1612a0dbd2e7a44d14bd8328923443833

# avdmanager needs the package.xml that only sdkmanager writes
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

# the emulator's launcher finds its own libraries under lib64; only system libraries can be missing
if LD_LIBRARY_PATH=$(find "$sdk/emulator/lib64" -type d | paste -sd :) ldd "$sdk/emulator/qemu/linux-x86_64/qemu-system-x86_64" | grep 'not found'; then
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
