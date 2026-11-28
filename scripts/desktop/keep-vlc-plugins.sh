#!/bin/bash

set -euo pipefail

plugins_dir=$1
allowed=$(tr -d '\r' < "$(dirname "$0")/vlc-plugins.txt")

find "$plugins_dir" -type f | while read -r file; do
  name=$(basename "$file")
  module=${name#lib}
  module=${module%%_plugin.*}
  if [[ "$name" != lib*_plugin.* ]] || ! grep -qxF "$module" <<< "$allowed"; then
    rm "$file"
  fi
done
find "$plugins_dir" -type d -empty -delete
