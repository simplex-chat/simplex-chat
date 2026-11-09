#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")/../.."

case "${1:-}" in
  badge-web)
    (cd apps/simplex-badge-service/web && npm ci && npm test)
    ;;
  ci-scripts)
    python3 scripts/ci/test_packages_release.py
    ;;
  ci-failures)
    python3 scripts/ci-failures/test_ci_failures.py
    ;;
  website)
    (cd website && npm test)
    ;;
  mcp)
    (cd packages/simplex-chat-client/types/typescript && npm install --no-audit --no-fund && npx tsc)
    (cd packages/simplex-chat-nodejs && npm install --ignore-scripts --no-save --no-audit --no-fund ../simplex-chat-client/types/typescript && npx tsc && cp src/simplex.* dist)
    cd packages/simplex-chat-mcp
    npm ci --no-audit --no-fund
    npm install --no-save --no-audit --no-fund ../simplex-chat-nodejs ../simplex-chat-client/types/typescript
    npx tsc
    npx tsc -p test/tsconfig.json --noEmit
    npm test
    npm run test:e2e
    ;;
  *)
    echo "usage: $0 badge-web | ci-scripts | ci-failures | website | mcp" >&2
    exit 1
    ;;
esac
