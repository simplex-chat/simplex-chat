#!/bin/bash

set -euo pipefail

version=2.1.5.1
build=20230208
sha256=2fdc3feb6e9deb17adec9bafa3321419aa19f8f4e5dea7bf8486844ca22207bf
prefix=${LIBJPEG_TURBO_PREFIX:-$HOME/.cache/libjpeg-turbo-$version}

if [ ! -f "$prefix/lib/libturbojpeg.a" ]; then
    tmp=$(mktemp -d)
    curl --proto '=https' --tlsv1.2 -sSfL -o "$tmp/src.tar.gz" "https://github.com/libjpeg-turbo/libjpeg-turbo/releases/download/$version/libjpeg-turbo-$version.tar.gz"
    if command -v sha256sum > /dev/null; then
        echo "$sha256  $tmp/src.tar.gz" | sha256sum -c - >&2
    else
        echo "$sha256  $tmp/src.tar.gz" | shasum -a 256 -c - >&2
    fi
    tar -xzf "$tmp/src.tar.gz" -C "$tmp"
    cmake -S "$tmp/libjpeg-turbo-$version" -B "$tmp/build" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$prefix" -DCMAKE_INSTALL_LIBDIR:PATH=lib -DBUILD=$build -DENABLE_SHARED=OFF -DCMAKE_POSITION_INDEPENDENT_CODE=ON >&2
    cmake --build "$tmp/build" --target install --parallel >&2
    rm -rf "$tmp"
fi

if command -v cygpath > /dev/null; then
    cygpath -m "$prefix"
else
    echo "$prefix"
fi
