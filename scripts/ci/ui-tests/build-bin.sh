#!/bin/bash

# Builds the CLI and uitest-servers, the UI tests' peers and local servers, as apps/multiplatform/scripts/uitest.sh
# builds them (-O0, flag uitest), and packs the two executables.
# Usage: scripts/ci/ui-tests/build-bin.sh <output .tar.gz>
# The configuration is build-core.sh's but without client_library, which makes uitest-servers unbuildable.

set -euo pipefail

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
flags=(-O0 -f uitest)
exes=(simplex-chat uitest-servers)

ghc --version
cabal --version

cat > cabal.project.local << EOF
ignore-project: False
jobs: \$ncpus

package direct-sqlcipher
    flags: +openssl
EOF

cabal build "${flags[@]}" "${exes[@]/#/exe:}"

pkg=$(mktemp -d)
for exe in "${exes[@]}"; do
  # in a fresh project list-bin first prints git's output from syncing the source-repository-packages
  cp "$(cabal list-bin "${flags[@]}" "exe:$exe" | tail -n 1)" "$pkg"
done
ldd "$pkg"/*
if ldd "$pkg"/* | grep 'not found'; then
  echo "error: the executables need these libraries"
  exit 1
fi
du -sh "$pkg"/*

tar -C "$pkg" -czf "$out" .
ls -la "$out"
