#!/bin/bash

# Builds the CLI, the directory service and the SMP server that the app job runs as its test backend.
# Usage: scripts/ci/desktop-linux/build-backend.sh <output .tar.gz>
# They need the server-capable configuration (no client_library), as build.yml's Linux job builds the CLI.
# The SMP server, which resolves the test's SimpleX names, comes from a clone of the simplexmq commit that
# cabal.project pins: cabal builds only the libraries of a source-repository-package.

set -euo pipefail

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"

ghc --version
cabal --version

cat > cabal.project.local << EOF
ignore-project: False
jobs: \$ncpus

package direct-sqlcipher
    flags: +openssl
EOF

jpeg_turbo_dir=$(scripts/desktop/prepare-libjpeg-turbo.sh)
export PKG_CONFIG_PATH=$jpeg_turbo_dir/lib/pkgconfig

cabal build exe:simplex-chat exe:simplex-directory-service

simplexmq_tag=$(sed -n '/simplex-chat\/simplexmq.git/{n;s/^ *tag: *//p}' cabal.project)
smq=$(mktemp -d)
git -C "$smq" init -q
git -C "$smq" fetch -q --depth 1 https://github.com/simplex-chat/simplexmq.git "$simplexmq_tag"
git -C "$smq" checkout -q FETCH_HEAD
git -C "$smq" submodule update -q --init --recursive
cp cabal.project.local "$smq"
(cd "$smq" && cabal build exe:smp-server)

pkg=$(mktemp -d)
for exe in simplex-chat simplex-directory-service; do
  cp "$(cabal list-bin "exe:$exe")" "$pkg"
done
# in a fresh project list-bin first prints git's output from syncing the source-repository-packages
cp "$(cd "$smq" && cabal list-bin exe:smp-server | tail -n 1)" "$pkg"
ldd "$pkg"/*

tar -C "$pkg" -czf "$out" .
ls -la "$out"
