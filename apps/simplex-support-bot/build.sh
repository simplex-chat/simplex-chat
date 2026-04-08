#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "Building simplex-support-bot..."

# Build @simplex-chat/types (local dependency)
echo "Building @simplex-chat/types..."
cd "$REPO_ROOT/packages/simplex-chat-client/types/typescript"
npm run build

# Build simplex-chat (local dependency — native addon + TypeScript)
echo "Building simplex-chat..."
cd "$REPO_ROOT/packages/simplex-chat-nodejs"
npm run build

# Install and build the bot
echo "Building simplex-support-bot..."
cd "$SCRIPT_DIR"
npm install

# npm install copies the file: dependency but doesn't run its build script,
# so simplex.js/simplex.d.ts (native addon loader) are missing from dist/.
cp node_modules/simplex-chat/src/simplex.js node_modules/simplex-chat/dist/
cp node_modules/simplex-chat/src/simplex.d.ts node_modules/simplex-chat/dist/

npm run build

echo "Build complete. Output in $SCRIPT_DIR/dist/"
