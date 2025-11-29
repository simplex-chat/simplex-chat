#!/usr/bin/env sh
set -eu

export SOURCE_DATE_EPOCH=0

CLI_VERSION="$1"
CLI_PATH_TO_BIN="${2:-/out/simplex-chat}"
BUILD_FOLDER="${3:-/out/deb-build}"

size=$(stat -c '%s' "$CLI_PATH_TO_BIN" | awk '{printf "%.0f\n", ($1+1023)/1024}')
arch=$(case "$(uname -m)" in x86_64) printf "amd64" ;; aarch64) printf "arm64" ;; *) printf "unknown" ;; esac)
package='simplex-chat'

mkdir "$BUILD_FOLDER"
cd "$BUILD_FOLDER"

mkdir -p ./${package}/DEBIAN
mkdir -p ./${package}/usr/bin
cat > ./${package}/DEBIAN/control << EOF
Package: ${package}
Version: ${CLI_VERSION}
Section: Messenger
Priority: optional
Architecture: ${arch}
Maintainer: SimpleX Chat <chat@simplex.chat>
Description: SimpleX - the first messaging platform that has no user identifiers of any kind - 100% private by design! (CLI)
Installed-Size: ${size}
EOF

cp "$CLI_PATH_TO_BIN" ./${package}/usr/bin/simplex-chat
chmod +x ./${package}/usr/bin/simplex-chat

find ./${package} -exec touch -d "@${SOURCE_DATE_EPOCH}" {} +

dpkg-deb --build --root-owner-group --uniform-compression ./${package}

strip-nondeterminism "./${package}.deb"
