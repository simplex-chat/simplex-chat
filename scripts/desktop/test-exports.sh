#!/usr/bin/env bash

set -eu

repo=$(cd "$(dirname "$0")/../.." && pwd)
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/bin"
cat > "$fixture/bin/ghc" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  --numeric-version) echo 9.6.3 ;;
  --print-libdir) echo /unused-ghc-libdir ;;
  *) exit 99 ;;
esac
STUB
cat > "$fixture/bin/cabal" <<'STUB'
#!/usr/bin/env bash
echo BUILD_REACHED
exit 77
STUB
chmod +x "$fixture/bin/ghc" "$fixture/bin/cabal"

for scenario in current missing duplicate similar-name crlf dll-missing dll-duplicate excluded; do
  for platform in linux mac windows; do
    work="$fixture/$scenario-$platform"
    mkdir -p "$work/scripts/desktop" "$work/scripts/nix" "$work/src/Simplex/Chat"
    cp "$repo/scripts/desktop/build-lib-$platform.sh" "$repo/scripts/desktop/check-exports.sh" "$work/scripts/desktop/"
    cp "$repo/scripts/nix/chat-exports.txt" "$work/scripts/nix/"
    cp "$repo/src/Simplex/Chat/Mobile.hs" "$work/src/Simplex/Chat/"
    cp "$repo/libsimplex.dll.def" "$work/"
    cp "$fixture/bin/cabal" "$work/scripts/desktop/prepare-openssl-windows.sh"
    manifest="$work/scripts/nix/chat-exports.txt"
    case "$scenario" in
      missing) sed '/^chat_migrate_init$/d' "$manifest" > "$manifest.new"; mv "$manifest.new" "$manifest" ;;
      duplicate) echo chat_migrate_init >> "$manifest" ;;
      similar-name) sed 's/^chat_migrate_init$/chat_migrate_init_extra/' "$manifest" > "$manifest.new"; mv "$manifest.new" "$manifest" ;;
      crlf) awk '{ printf "%s\r\n", $0 }' "$manifest" > "$manifest.new"; mv "$manifest.new" "$manifest" ;;
      dll-missing) sed '/^    chat_migrate_init$/d' "$work/libsimplex.dll.def" > "$work/def.new"; mv "$work/def.new" "$work/libsimplex.dll.def" ;;
      dll-duplicate) echo '    chat_migrate_init' >> "$work/libsimplex.dll.def" ;;
      excluded)
        for file in "$manifest" "$work/libsimplex.dll.def"; do
          sed '/chat_migrate_init_key/d; /chat_reopen_store/d' "$file" > "$file.new"; mv "$file.new" "$file"
        done ;;
    esac
    if output=$(cd "$work" && PATH="$fixture/bin:$PATH" bash "scripts/desktop/build-lib-$platform.sh" 2>&1); then
      status=0
    else
      status=$?
    fi
    case "$scenario" in
      current|crlf|excluded)
        [ "$status" -eq 77 ] && [[ "$output" == *BUILD_REACHED* ]] || { echo "$platform/$scenario did not reach the build stub: $output" >&2; exit 1; } ;;
      *)
        [ "$status" -eq 1 ] && [[ "$output" != *BUILD_REACHED* ]] || { echo "$platform/$scenario did not stop at the export guard: $output" >&2; exit 1; } ;;
    esac
    echo "PASS $platform/$scenario"
  done
done
