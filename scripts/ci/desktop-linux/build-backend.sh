#!/bin/bash

# Builds the CLI and the directory service that the app job runs as its test backend.
# Usage: scripts/ci/desktop-linux/build-backend.sh <output .tar.gz>
# They need the server-capable configuration (no client_library), as build.yml's Linux job builds the CLI.

set -euo pipefail

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"

ghc --version
cabal --version
du -sh ~/.cabal/store dist-newstyle 2> /dev/null || true

cat > cabal.project.local << EOF
ignore-project: False
jobs: \$ncpus

package direct-sqlcipher
    flags: +openssl
EOF

cabal build exe:simplex-chat exe:simplex-directory-service

pkg=$(mktemp -d)
for exe in simplex-chat simplex-directory-service; do
  cp "$(cabal list-bin "exe:$exe")" "$pkg"
done
ldd "$pkg"/*

tar -C "$pkg" -czf "$out" .
ls -la "$out"
