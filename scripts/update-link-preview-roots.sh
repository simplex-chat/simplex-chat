#!/bin/sh

set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

curl -fsSL -o "$dir/cacert.pem" https://curl.se/ca/cacert.pem
curl -fsSL -o "$dir/cacert.pem.sha256" https://curl.se/ca/cacert.pem.sha256
(cd "$dir" && shasum -a 256 -c cacert.pem.sha256)
cp "$dir/cacert.pem" "$root/src/Simplex/Chat/LinkPreview/roots.pem"
