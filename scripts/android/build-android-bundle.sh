#!/usr/bin/env bash
# Safety measures
set -euo pipefail
trap 's=$?; echo >&2 "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

[ -n "$1" ] || exit 1

tmp=$(mktemp -d -t)
libsim=$(<<< "$1" grep libsimplex)
libsup=$(<<< "$1" grep libsupport)
commit="${2:-nix-android}"

# Clone simplex
git clone --depth 1 https://github.com/simplex-chat/simplex-chat "$tmp/simplex-chat"

# Switch to nix-android branch
git -C "$tmp/simplex-chat" checkout "$commit"

# Create missing folders
mkdir -p "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"

curl -sSf "$libsim" -o "$tmp/libsimplex.zip"
unzip -o "$tmp/libsimplex.zip" -d "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"

curl -sSf "$libsup" -o "$tmp/libsupport.zip"
unzip -o "$tmp/libsupport.zip" -d "$tmp/simplex-chat/apps/android/app/src/main/cpp/libs/arm64-v8a"

gradle -p "$tmp/simplex-chat/apps/android/" clean build
cp "$tmp/simplex-chat/apps/android/app/build/outputs/apk/release/app-release-unsigned.apk" "$PWD/simplex-chat.apk"
