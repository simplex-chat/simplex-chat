#!/usr/bin/env sh
# Builds the Haskell core library for one target and puts it where the app
# build expects it. Run from the repo root.
#
#   ./scripts/build-core.sh android-aarch64
#   ./scripts/build-core.sh android-armv7a
#   ./scripts/build-core.sh desktop-linux [sqlite|postgres]
#   ./scripts/build-core.sh desktop-mac [x86_64|arm64] [sqlite|postgres]
#   ./scripts/build-core.sh desktop-windows [x86_64]
#   ./scripts/build-core.sh ios                                  # macOS only
#
# nix targets (android-*, ios) need Nix >= 2.22 with flakes enabled and
# https://cache.iog.io as a substituter, otherwise the cross-GHC is compiled
# from source (12-24 hours). Desktop targets need ghcup's GHC 9.6.3 + cabal
# and a cabal.project.local, see docs/CONTRIBUTING.md.
set -eu

android_libs="apps/multiplatform/common/src/commonMain/cpp/android/libs"

# $1 = flake attribute path after .#hydraJobs., $2 = zip in ./result, $3 = destination
nix_unzip() {
  nix build ".#hydraJobs.$1"
  mkdir -p "$3"
  unzip -o "result/$2" -d "$3"
}

build_android() {
  arch="$1"
  abi="$2"
  nix_unzip "x86_64-linux.\"${arch}-android:lib:simplex-chat\"" \
            "pkg-${arch}-android-libsimplex.zip" "$android_libs/$abi"
  nix_unzip "x86_64-linux.\"${arch}-android:lib:support\"" \
            "pkg-${arch}-android-libsupport.zip" "$android_libs/$abi"
}

target="${1:-}"
if [ $# -gt 0 ]; then shift; fi

case "$target" in
  android-aarch64|arm64-v8a)
    build_android aarch64 arm64-v8a
    ;;
  android-armv7a|armeabi-v7a)
    build_android armv7a armeabi-v7a
    ;;
  desktop-linux)
    scripts/desktop/build-lib-linux.sh "$@"
    ;;
  desktop-mac)
    scripts/desktop/build-lib-mac.sh "$@"
    ;;
  desktop-windows)
    scripts/desktop/build-lib-windows.sh "$@"
    ;;
  ios)
    nix build '.#hydraJobs.aarch64-darwin."aarch64-darwin-ios:lib:simplex-chat"'
    cp result/pkg-ios-aarch64-swift-json.zip "$HOME/Downloads/"
    scripts/ios/prepare.sh
    ;;
  *)
    printf 'usage: %s <android-aarch64|android-armv7a|desktop-linux|desktop-mac|desktop-windows|ios>\n' "$0"
    exit 1
    ;;
esac
