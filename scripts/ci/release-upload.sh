#!/bin/bash

set -euo pipefail

[ $# -ge 3 ] || { echo "usage: $0 <repository> <tag> <file>..." >&2; exit 1; }
repo=$1
tag=$2
shift 2

published() {
  [ "$(gh release view "$tag" --repo "$repo" --json isDraft --jq .isDraft)" = false ] \
    && gh release download "$tag" --repo "$repo" --pattern "$(basename "$1")" --dir "$(mktemp -d)"
}

for file in "$@"; do
  gh release upload "$tag" "$file" --clobber --repo "$repo" || published "$file"
done
