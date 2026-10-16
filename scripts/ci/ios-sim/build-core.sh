#!/bin/bash

# Builds the iOS core library package with cabal, as flake.nix builds aarch64-darwin-ios:lib:simplex-chat.
# Usage: scripts/ci/ios-sim/build-core.sh <output zip>
# The zip has the same files as the Hydra package that scripts/ios/prepare.sh expects.

set -euo pipefail

mkdir -p "$(dirname "$1")"
out="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
ghc_ver=$(ghc --numeric-version)
version=$(sed -n 's/^version: *//p' simplex-chat.cabal)
openssl_dir=$(brew --prefix openssl@3)
ffi_dir=$(brew --prefix libffi)
gmp_dir=$(brew --prefix gmp)

ghc --version
cabal --version
echo "rts extra-libraries: $(ghc-pkg field rts extra-libraries --simple-output)"
echo "ghc-bignum extra-libraries: $(ghc-pkg field ghc-bignum extra-libraries --simple-output)"
find "$(ghc --print-libdir)" -name 'lib*ffi*.a' -o -name 'libgmp*.a' -o -name 'libHSrts*_thr.a'
du -sh ~/.cabal/store dist-newstyle || true

cat > cabal.project.local <<EOF
ignore-project: False

package simplex-chat
    flags: +swift +client_library
    static: True
    shared: False
    ghc-options: -threaded -DIOS

package simplexmq
    flags: +swift +client_library +commoncrypto
    extra-include-dirs: $openssl_dir/include
    extra-lib-dirs: $openssl_dir/lib

package direct-sqlcipher
    flags: +commoncrypto

package entropy
    flags: +DoNotGetEntropy
EOF

cabal build -j lib:simplex-chat

build_dir=dist-newstyle/build/aarch64-osx/ghc-$ghc_ver/simplex-chat-$version/build
ls -la "$build_dir"/libHSsimplex-chat-*

ghc_lib=$(ls "$build_dir"/libHSsimplex-chat-*-ghc"$ghc_ver".a)
echo "Selected symbols defined in $ghc_lib:"
{ nm -gU "$ghc_lib" 2>/dev/null || true; } \
  | awk '$NF ~ /^_(ffi_call|__gmpz_init|hs_init_with_rtsopts|chat_migrate_init_key|EVP_CIPHER_CTX_new|sqlite3_key)$/ {print "  " $NF}' \
  | sort -u

pkg=$(mktemp -d)
cp "$build_dir"/libHSsimplex-chat-*.a "$ffi_dir"/lib/libffi.a "$gmp_dir"/lib/libgmp.a "$gmp_dir"/lib/libgmpxx.a "$pkg"
chmod u+w "$pkg"/*.a
for f in "$pkg"/libHSsimplex-chat-*.a; do
  strip -x "$f"
done
ls -la "$pkg"

rm -f "$out"
(cd "$pkg" && zip -9 "$out" *.a)
ls -la "$out"
