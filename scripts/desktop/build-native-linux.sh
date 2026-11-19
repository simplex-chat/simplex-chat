#!/usr/bin/env bash

set -euo pipefail

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}

ARCH="$(uname -m)"
case "$ARCH" in
  x86_64) NATIVE_ARCH=linux-x64 ;;
  aarch64) NATIVE_ARCH=linux-aarch64 ;;
  *) echo "Unsupported architecture $ARCH"; exit 1 ;;
esac

root_dir="$(dirname "$(dirname "$(readlink "$0")")")"
external_dir="$root_dir/apps/multiplatform/external"
nucleus_dir="$external_dir/nucleus/upstream"
tray_dir="$external_dir/composenativetray/upstream"
libs_dir="$root_dir/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-$ARCH"

nucleus_modules=(
  decorated-window-core
  decorated-window-tao
  energy-manager
  darkmode-detector
  notification-linux
  launcher-linux
)

if [ ! -f "$nucleus_dir/settings.gradle.kts" ] || [ ! -f "$tray_dir/build.gradle.kts" ]; then
  echo "Native module sources are missing, run: git submodule update --init --recursive"
  exit 1
fi

mkdir -p "$libs_dir"

for module in "${nucleus_modules[@]}"; do
  out_dir="$nucleus_dir/$module/src/main/resources/nucleus/native"
  bash "$nucleus_dir/$module/src/main/native/linux/build.sh"
  cp "$out_dir/$NATIVE_ARCH/"*.so "$libs_dir/"
  rm -rf "$out_dir"
done

java_home="${JAVA_HOME:-$(dirname "$(dirname "$(realpath "$(command -v javac)")")")}"
gcc -shared -fPIC -O2 -Wall -Wextra \
  -I"$java_home/include" -I"$java_home/include/linux" \
  $(pkg-config --cflags gtk+-3.0 libsecret-1) \
  "$root_dir/apps/multiplatform/common/src/desktopMain/native/linux/simplex_linux.c" \
  -o "$libs_dir/libsimplex_linux.so" \
  $(pkg-config --libs gtk+-3.0) -ldl

tray_out_dir="$(mktemp -d)"
trap 'rm -rf "$tray_out_dir"' EXIT
NATIVE_LIBS_OUTPUT_DIR="$tray_out_dir" bash "$tray_dir/src/native/linux/build.sh"
cp "$tray_out_dir/$NATIVE_ARCH/"*.so "$libs_dir/"

ls -la "$libs_dir"/libnucleus_*.so "$libs_dir"/libLinuxTray.so "$libs_dir"/libsimplex_linux.so
