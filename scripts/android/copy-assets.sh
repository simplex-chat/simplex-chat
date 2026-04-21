#!/bin/sh
set -eu

# Copies generated multiplatform assets into the SimpleX assets directory.
# Called by Gradle build when simplex.assets.dir property is set.
#
# Usage: copy-assets.sh <source-dir> <dest-dir>

SRC_DIR="$1/multiplatform/resources/MR/images"
DEST_DIR="$2/MR/images"

if [ ! -d "$SRC_DIR" ]; then
  echo "Error: source assets not found: $SRC_DIR (run resize.sh first)" >&2
  exit 1
fi

rm -rf "$DEST_DIR"
mkdir -p "$DEST_DIR"

cp "$SRC_DIR"/* "$DEST_DIR/"
echo "Copied multiplatform assets to $DEST_DIR"
