#!/bin/bash

# Unpacks the CLI and uitest-servers from build-bin.sh into <dir>, checks that the runner has their libraries, and puts
# UITEST_CLI and UITEST_SERVERS in GITHUB_ENV, where apps/multiplatform/scripts/uitest.sh uses them instead of building.
# Usage: scripts/ci/ui-tests/install-bin.sh <bin .tar.gz> <dir>

set -euo pipefail

mkdir -p "$2"
dir=$(cd "$2" && pwd)
tar -C "$dir" -xzf "$1"
ls -la "$dir"
if ldd "$dir/simplex-chat" "$dir/uitest-servers" | grep 'not found'; then
  echo "error: the executables need these libraries"
  exit 1
fi

if [ -n "${GITHUB_ENV:-}" ]; then
  {
    echo "UITEST_CLI=$dir/simplex-chat"
    echo "UITEST_SERVERS=$dir/uitest-servers"
  } >> "$GITHUB_ENV"
fi
