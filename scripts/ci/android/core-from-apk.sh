#!/bin/bash

# Puts the core libraries of a released arm64 APK where scripts/android/download-libs.sh puts the Nix-built ones,
# for a spike build of the app before the core is built on the runner.
# Usage: scripts/ci/android/core-from-apk.sh <apk>

set -euo pipefail

libs_dir=apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a
tmp=$(mktemp -d)
unzip -q "$1" 'lib/arm64-v8a/*' -d "$tmp"
mkdir -p "$libs_dir"
find "$tmp/lib/arm64-v8a" -name '*.so' ! -name 'libapp-lib.so' -exec cp {} "$libs_dir" \;
ls -la "$libs_dir"
readelf -d "$libs_dir/libsimplex.so" | grep -E 'NEEDED|SONAME'
