#!/bin/sh
# Copies the built sites in /srv into the folders mounted at /out, which Caddy serves.
set -eu

export_site() {
  src=$1
  dst=$2
  cd "$src"
  find . -type d -exec mkdir -p "$dst/{}" \;
  # The shells name the build's assets, so they are replaced last, each in one rename.
  find . -type f ! -path ./index.html ! -path ./sw.js -exec cp --preserve=mode,timestamps {} "$dst/{}" \;
  for shell in sw.js index.html; do
    cp --preserve=mode,timestamps "$shell" "$dst/$shell.new"
    mv "$dst/$shell.new" "$dst/$shell"
  done
  cd "$dst"
  find . -type f -exec sh -c 'for f; do [ -e "$0/$f" ] || rm "$f"; done' "$src" {} +
  find . -mindepth 1 -type d -empty -delete
}

export_site /srv/badges /out/badges
export_site /srv/names /out/names
