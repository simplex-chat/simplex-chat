#!/bin/bash

set -euo pipefail

function readlink() {
  echo "$(cd "$(dirname "$1")"; pwd -P)"
}
root_dir="$(dirname "$(dirname "$(readlink "$0")")")"

version=3.1.4.1
build=20260327
prefix=${LIBJPEG_TURBO_PREFIX:-$HOME/.cache/libjpeg-turbo-$version}

if [ ! -f "$prefix/lib/libturbojpeg.a" ]; then
    if command -v cygpath > /dev/null; then
        export CC=$(cygpath -m "$(ghc --print-libdir)/../mingw/bin/clang.exe")
        export LDFLAGS="-fuse-ld=lld --rtlib=compiler-rt"
    fi
    tmp=$(mktemp -d)
    cmake -S "$root_dir/external/libjpeg-turbo" -B "$tmp/build" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$prefix" -DCMAKE_INSTALL_LIBDIR:PATH=lib -DBUILD=$build -DENABLE_SHARED=OFF -DCMAKE_POSITION_INDEPENDENT_CODE=ON >&2
    cmake --build "$tmp/build" --target install --parallel >&2
    rm -rf "$tmp"
fi

if command -v cygpath > /dev/null; then
    cygpath -m "$prefix"
else
    echo "$prefix"
fi
