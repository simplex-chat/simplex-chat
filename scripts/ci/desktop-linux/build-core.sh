#!/bin/bash

# Builds the desktop core library for Linux with scripts/desktop/build-lib-linux.sh, as build.yml does, and packs its libs directory.
# Usage: scripts/ci/desktop-linux/build-core.sh <output .tar.gz>

set -euo pipefail

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
arch=$(uname -m)
ghc_ver=$(ghc --numeric-version)
version=$(sed -n 's/^version: *//p' simplex-chat.cabal)
libs_dir=apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-$arch

ghc --version
cabal --version
du -sh ~/.local/state/cabal/store ~/.cabal/store dist-newstyle 2> /dev/null || true

cat > cabal.project.local << EOF
ignore-project: False
jobs: \$ncpus

package direct-sqlcipher
    flags: +openssl
EOF

# a restored dist-newstyle can hold an older version's build directory, which breaks the script's simplex-chat-* glob
find "dist-newstyle/build/$arch-linux/ghc-$ghc_ver" -maxdepth 1 -name 'simplex-chat-*' ! -name "simplex-chat-$version" -exec rm -rf {} + 2> /dev/null || true

SKIP_VLC=1 scripts/desktop/build-lib-linux.sh

ls -la "$libs_dir"
readelf -d "$libs_dir/libsimplex.so" | grep -E 'SONAME|RUNPATH|NEEDED'
echo "Selected symbols defined in libsimplex.so:"
nm -D --defined-only "$libs_dir/libsimplex.so" | awk '$NF ~ /^(chat_migrate_init_key|chat_send_cmd_retry|chat_recv_msg_wait)$/ {print "  " $NF}'
du -sh "$libs_dir"

tar -C "$libs_dir" -czf "$out" .
ls -la "$out"
