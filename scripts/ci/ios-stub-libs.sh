#!/bin/sh

# Creates stub static libraries in apps/ios/Libraries/sim so that the iOS app
# links for the simulator without the Haskell core.
# Run from repo root on macOS.

set -eu

IOS_DIR=apps/ios
PBXPROJ=$IOS_DIR/SimpleX.xcodeproj/project.pbxproj
OUT_DIR=$IOS_DIR/Libraries/sim
ARCH=arm64
MIN_IOS=15.0

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# The linker resolves symbols by name only, so the stub signatures are not checked.
sed -n 's/^extern [^(]*[ *]\([a-z_]*\)(.*/\1/p' "$IOS_DIR/SimpleXChat/SimpleX.h" "$IOS_DIR/SimpleXChat/hs_init.c" \
    | sort -u | sed 's/.*/void &(void) {}/' > "$tmp/stubs.c"

if [ ! -s "$tmp/stubs.c" ]; then
    echo "Error: no extern functions found in SimpleX.h and hs_init.c" >&2
    exit 1
fi

xcrun --sdk iphonesimulator clang -arch "$ARCH" -mios-simulator-version-min="$MIN_IOS" -c "$tmp/stubs.c" -o "$tmp/stubs.o"
xcrun ar rcs "$tmp/libstubs.a" "$tmp/stubs.o"

libs=$(sed -n 's/.*isa = PBXFileReference; lastKnownFileType = archive\.ar; path = "\{0,1\}\(lib[^";]*\.a\)"\{0,1\};.*/\1/p' "$PBXPROJ")
if [ -z "$libs" ]; then
    echo "Error: no static library references found in $PBXPROJ" >&2
    exit 1
fi

mkdir -p "$OUT_DIR"
echo "$libs" | while IFS= read -r lib; do
    cp "$tmp/libstubs.a" "$OUT_DIR/$lib"
    echo "$OUT_DIR/$lib"
done
