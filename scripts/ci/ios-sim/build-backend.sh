#!/bin/bash

# Builds the CLI and the directory service that the app job runs as its test backend.
# Usage: scripts/ci/ios-sim/build-backend.sh <output zip>
# They need the server-capable configuration (no client_library), as build.yml's macOS job builds the CLI.

set -euo pipefail

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
openssl_dir=$(brew --prefix openssl@3)

ghc --version
cabal --version
du -sh ~/.cabal/store dist-newstyle || true

cat > cabal.project.local <<EOF
ignore-project: False

package simplexmq
    extra-include-dirs: $openssl_dir/include
    extra-lib-dirs: $openssl_dir/lib

package direct-sqlcipher
    flags: +openssl
    extra-include-dirs: $openssl_dir/include
    extra-lib-dirs: $openssl_dir/lib
EOF

cabal build -j exe:simplex-chat exe:simplex-directory-service

pkg=$(mktemp -d)
for exe in simplex-chat simplex-directory-service; do
  cp "$(cabal list-bin "exe:$exe")" "$pkg"
done
otool -L "$pkg"/*

rm -f "$out"
(cd "$pkg" && zip -9 "$out" *)
ls -la "$out"
