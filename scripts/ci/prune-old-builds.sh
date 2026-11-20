#!/bin/sh
set -eu

version=$(awk '/^version:/ {print $2}' simplex-chat.cabal | tr -d '\r')
for dir in "$@"; do
  if [ -d "$dir/build" ]; then
    find "$dir/build" -mindepth 3 -maxdepth 3 -type d -name 'simplex-chat-*' ! -name "simplex-chat-$version" -exec rm -rf {} +
  fi
  for src in "$dir"/src/*/; do
    [ -e "$src.git" ] || continue
    rev=$(git -C "$src" rev-parse HEAD 2>/dev/null || true)
    if [ -z "$rev" ] || ! grep -q "tag: $rev" cabal.project; then
      rm -rf "$src"
    fi
  done
done
