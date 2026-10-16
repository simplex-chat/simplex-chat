#!/bin/sh
set -eu

command=$1
hashes=$2

sources() {
  git ls-files -z '*.hs' '*.hsc'
}

case "$command" in
  record)
    sources | xargs -0 sha256sum > "$hashes"
    ;;
  restore)
    sources | xargs -0 touch
    while read -r hash file; do
      if [ -f "$file" ] && [ "$(sha256sum "$file" | cut -d' ' -f1)" = "$hash" ]; then
        touch -d 2000-01-01 "$file"
      fi
    done < "$hashes"
    ;;
  *)
    echo "usage: $0 record|restore <hashes file>" >&2
    exit 1
    ;;
esac
