#!/bin/bash

# Unpacks the core package from build-core.sh where scripts/android/download-libs.sh puts the Hydra builds.
# Usage: scripts/ci/android/install-core.sh <core .tar.gz> [arm64-v8a|armeabi-v7a]

set -euo pipefail

libs_dir=apps/multiplatform/common/src/commonMain/cpp/android/libs/${2:-arm64-v8a}
mkdir -p "$libs_dir"
tar -C "$libs_dir" -xzf "$1"
ls -la "$libs_dir"
readelf -d "$libs_dir/libsimplex.so" | grep -E 'NEEDED|SONAME'
