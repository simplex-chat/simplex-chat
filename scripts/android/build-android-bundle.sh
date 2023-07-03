#!/usr/bin/env sh
# Safety measures
[ -n "$1" ] || exit 1
set -eu

tmp=$(mktemp -d -t)
libsim=$(cat "$1" | grep libsimplex)
libsup=$(cat "$1" | grep libsupport)
commit="${2:-nix-android}"

# Clone simplex
git clone https://github.com/simplex-chat/simplex-chat "$tmp/simplex-chat"

# Switch to nix-android branch
git -C "$tmp/simplex-chat" checkout "$commit"

# Create missing folders
mkdir -p "$tmp/simplex-chat/apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a"

curl -sSf "$libsim" -o "$tmp/libsimplex.zip"
unzip -o "$tmp/libsimplex.zip" -d "$tmp/simplex-chat/apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a"

curl -sSf "$libsup" -o "$tmp/libsupport.zip"
unzip -o "$tmp/libsupport.zip" -d "$tmp/simplex-chat/apps/multiplatform/common/src/commonMain/cpp/android/libs/arm64-v8a"

gradle -p "$tmp/simplex-chat/apps/multiplatform/" clean build
cp "$tmp/simplex-chat/apps/multiplatform/android/build/outputs/apk/release/android-release-unsigned.apk" "$PWD/simplex-chat.apk"
