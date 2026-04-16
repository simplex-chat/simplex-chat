#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TYPES_PKG="$REPO_ROOT/packages/simplex-chat-client/types/typescript"
NODEJS_PKG="$REPO_ROOT/packages/simplex-chat-nodejs"

# npm's `file:` deps are copied into node_modules, so when the source
# package changes (e.g. after `git pull`) consumers keep seeing the old
# copy — this is what produced the TS2345 "Property 'comments' is missing
# in type ... FullGroupPreferences" errors. Replacing those copies with
# symlinks removes the possibility of drift: there is one physical package
# and every consumer sees the current version.
link_workspace_dep() {
  local host="$1" name="$2" target="$3"
  local dest="$host/node_modules/$name"
  if [ -L "$dest" ] && [ "$(readlink -f "$dest")" = "$target" ]; then
    return
  fi
  mkdir -p "$(dirname "$dest")"
  rm -rf "$dest"
  ln -s "$target" "$dest"
}

# Only run `npm install` when something it cares about actually changed
# — missing node_modules, or a package manifest newer than the installed
# lockfile marker. Avoids the multi-minute reinstall on every build.
ensure_installed() {
  local dir="$1"
  local marker="$dir/node_modules/.package-lock.json"
  if [ ! -d "$dir/node_modules" ] \
    || [ ! -f "$marker" ] \
    || [ "$dir/package.json" -nt "$marker" ] \
    || { [ -f "$dir/package-lock.json" ] && [ "$dir/package-lock.json" -nt "$marker" ]; }; then
    (cd "$dir" && npm install)
  fi
}

echo "Building @simplex-chat/types..."
(cd "$TYPES_PKG" && npm run build)

echo "Building simplex-chat..."
ensure_installed "$NODEJS_PKG"
link_workspace_dep "$NODEJS_PKG" "@simplex-chat/types" "$TYPES_PKG"
(cd "$NODEJS_PKG" && npm run build)

echo "Building simplex-support-bot..."
ensure_installed "$SCRIPT_DIR"
link_workspace_dep "$SCRIPT_DIR" "@simplex-chat/types" "$TYPES_PKG"
link_workspace_dep "$SCRIPT_DIR" "simplex-chat" "$NODEJS_PKG"
(cd "$SCRIPT_DIR" && npm run build)

echo "Build complete. Output in $SCRIPT_DIR/dist/"
