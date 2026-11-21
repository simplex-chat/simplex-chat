#!/bin/bash

# Pairs the tour-<name> screenshots of two runs of the same tour by name, and compares them with snapshots-compare.py.
# Usage: scripts/ci/tour-compare.sh <base tour dir> <head tour dir> <result dir>
# The result dir gets base/ and head/ with one <name>.png each, diff/ with the differing pairs side by side, and
# head/compare.json.

set -euo pipefail

base_dir=$1
head_dir=$2
result=$3
here=$(cd "$(dirname "$0")" && pwd)

for side in base head; do
  src=$base_dir
  [ "$side" = head ] && src=$head_dir
  mkdir -p "$result/$side"
  for shot in "$src"/[0-9][0-9]-tour-*.png; do
    [ -e "$shot" ] || continue
    name=${shot##*/}
    cp "$shot" "$result/$side/${name#*-}"
  done
done
for shot in "$result/head"/*.png; do
  [ -e "$shot" ] && echo "${shot##*/}"
done | sort | jq -R '{file: .}' | jq -s . > "$result/head/index-tour.json"
python=$(command -v python3 || command -v python)
"$python" "$here/snapshots-compare.py" "$result/base" "$result/head" "$result/diff" || true
