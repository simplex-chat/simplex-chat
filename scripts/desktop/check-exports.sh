#!/usr/bin/env bash

set -euo pipefail

# Run from the repository root, as the desktop build scripts do.
manifest=scripts/nix/chat-exports.txt
awk '
  { sub(/\r$/, "") }
  $0 !~ /^[A-Za-z_][A-Za-z_0-9]*$/ || seen[$0]++ {
    print "Invalid or duplicate export in " FILENAME ": " $0 > "/dev/stderr"
    exit 1
  }
  END { if (NR == 0) exit 1 }
' "$manifest"

# Keep the historical exclusions shared by the desktop DLL/export guards.
exports=( $(sed 's/foreign export ccall "chat_migrate_init_key"//' src/Simplex/Chat/Mobile.hs | sed 's/foreign export ccall "chat_reopen_store"//' | grep "foreign export ccall" | cut -d '"' -f2) )
for symbol in "${exports[@]}"; do
  count=$(awk -v symbol="$symbol" '
    { sub(/\r$/, ""); sub(/^[[:space:]]+/, ""); if ($0 == symbol) count++ }
    END { print count + 0 }
  ' libsimplex.dll.def)
  if [ "$count" -ne 1 ]; then
    echo "Wrong exports in libsimplex.dll.def. Expected exactly one $symbol" >&2
    exit 1
  fi
  count=$(awk -v symbol="$symbol" '
    { sub(/\r$/, ""); if ($0 == symbol) count++ }
    END { print count + 0 }
  ' "$manifest")
  if [ "$count" -ne 1 ]; then
    echo "Wrong exports in $manifest. Expected exactly one $symbol" >&2
    exit 1
  fi
done
