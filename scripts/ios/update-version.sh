#!/bin/sh

# Bumps CURRENT_PROJECT_VERSION (build number) and MARKETING_VERSION in
# apps/ios/SimpleX.xcodeproj/project.pbxproj. Each appears in 10 places.
#
# Usage: ./scripts/ios/update-version.sh <build_number> <marketing_version>
# Example: ./scripts/ios/update-version.sh 333 6.5.3

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <build_number> <marketing_version>" >&2
    echo "Example: $0 333 6.5.3" >&2
    exit 1
fi

NEW_BUILD=$1
NEW_MARKETING=$2

if ! echo "$NEW_BUILD" | grep -qE '^[0-9]+$'; then
    echo "Error: build_number must be a positive integer (got: $NEW_BUILD)." >&2
    exit 1
fi
if ! echo "$NEW_MARKETING" | grep -qE '^[0-9]+(\.[0-9]+)+$'; then
    echo "Error: marketing_version must be like 6.5.3 (got: $NEW_MARKETING)." >&2
    exit 1
fi

PBXPROJ=./apps/ios/SimpleX.xcodeproj/project.pbxproj
if [ ! -f "$PBXPROJ" ]; then
    echo "Error: $PBXPROJ not found. Run from repo root." >&2
    exit 1
fi

# Detect current values; head -1 covers the (unexpected) mixed-values case,
# which the 10-line sanity check below will reject.
OLD_BUILD=$(grep -hoE 'CURRENT_PROJECT_VERSION = [^;]+;' "$PBXPROJ" \
    | sed -E 's/^.*= ([^;]+);$/\1/' | sort -u | head -1)
OLD_MARKETING=$(grep -hoE 'MARKETING_VERSION = [^;]+;' "$PBXPROJ" \
    | sed -E 's/^.*= ([^;]+);$/\1/' | sort -u | head -1)
if [ -z "$OLD_BUILD" ] || [ -z "$OLD_MARKETING" ]; then
    echo "Error: CURRENT_PROJECT_VERSION or MARKETING_VERSION not found in $PBXPROJ." >&2
    exit 1
fi

if [ "$OLD_BUILD" = "$NEW_BUILD" ] && [ "$OLD_MARKETING" = "$NEW_MARKETING" ]; then
    echo "Already up to date: build $NEW_BUILD, version $NEW_MARKETING"
    exit 0
fi

# Each field must appear in exactly 10 lines with a single uniform value.
OLD_BUILD_LINES=$(grep -cF "CURRENT_PROJECT_VERSION = $OLD_BUILD;" "$PBXPROJ" || true)
OLD_MARKETING_LINES=$(grep -cF "MARKETING_VERSION = $OLD_MARKETING;" "$PBXPROJ" || true)
if [ "$OLD_BUILD_LINES" -ne 10 ] || [ "$OLD_MARKETING_LINES" -ne 10 ]; then
    echo "Error: expected 10 + 10 lines, found $OLD_BUILD_LINES CURRENT_PROJECT_VERSION and $OLD_MARKETING_LINES MARKETING_VERSION (mixed values?)." >&2
    exit 1
fi

echo "Bumping in $PBXPROJ:"
if [ "$OLD_BUILD" != "$NEW_BUILD" ]; then
    echo "  CURRENT_PROJECT_VERSION: $OLD_BUILD -> $NEW_BUILD"
fi
if [ "$OLD_MARKETING" != "$NEW_MARKETING" ]; then
    echo "  MARKETING_VERSION:       $OLD_MARKETING -> $NEW_MARKETING"
fi

# Escape '.' in OLD_MARKETING so version dots match literally.
OLD_MARKETING_RE=$(printf '%s' "$OLD_MARKETING" | sed 's/\./\\./g')

TMP=$(mktemp "$PBXPROJ.XXXXXX")
trap 'rm -f "$TMP"' EXIT
sed \
    -e "s|CURRENT_PROJECT_VERSION = $OLD_BUILD;|CURRENT_PROJECT_VERSION = $NEW_BUILD;|g" \
    -e "s|MARKETING_VERSION = $OLD_MARKETING_RE;|MARKETING_VERSION = $NEW_MARKETING;|g" \
    "$PBXPROJ" > "$TMP"
mv "$TMP" "$PBXPROJ"

NEW_BUILD_LINES=$(grep -cF "CURRENT_PROJECT_VERSION = $NEW_BUILD;" "$PBXPROJ" || true)
NEW_MARKETING_LINES=$(grep -cF "MARKETING_VERSION = $NEW_MARKETING;" "$PBXPROJ" || true)
if [ "$NEW_BUILD_LINES" -ne 10 ] || [ "$NEW_MARKETING_LINES" -ne 10 ]; then
    echo "Error: post-replacement: $NEW_BUILD_LINES CURRENT_PROJECT_VERSION + $NEW_MARKETING_LINES MARKETING_VERSION (expected 10+10)." >&2
    exit 1
fi
echo "Updated 20 lines (10 + 10)."
