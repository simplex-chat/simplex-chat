#!/bin/bash

# Builds the CLI, the directory service and the SMP server (for SimpleX names) that the app job runs as its test backend.
# Usage: scripts/ci/ios-sim/build-backend.sh <output zip>
# They need the server-capable configuration (no client_library), as build.yml's macOS job builds the CLI.

set -euo pipefail

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
openssl_dir=$(brew --prefix openssl@3)

ghc --version
cabal --version

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

targets="exe:simplex-chat exe:simplex-directory-service"
cabal build -j $targets

# simplexmq's sdist leaves out apps/common/Web/static/.well-known, which smp-server embeds, so it is built from a clone
simplexmq=$(mktemp -d)/simplexmq
git clone --quiet --filter=blob:none https://github.com/simplex-chat/simplexmq.git "$simplexmq"
git -C "$simplexmq" checkout --quiet "$(awk '/simplexmq.git/ {getline; print $2; exit}' cabal.project)"
git -C "$simplexmq" submodule update --init --quiet
git -C "$simplexmq" log -1 --format='simplexmq %H %s'
git -C "$simplexmq" submodule status
cp cabal.project.local "$simplexmq/"
(cd "$simplexmq" && cabal build -j exe:smp-server)

pkg=$(mktemp -d)
for target in $targets; do
  cp "$(cabal list-bin "$target")" "$pkg"
done
# in the clone, list-bin first prints git's output from syncing the source packages
cp "$(cd "$simplexmq" && cabal list-bin exe:smp-server | tail -n 1)" "$pkg"
otool -L "$pkg"/*

rm -f "$out"
(cd "$pkg" && zip -9 "$out" *)
ls -la "$out"
