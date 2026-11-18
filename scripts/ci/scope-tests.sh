#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")/../.."

case "${1:-}" in
  badge-web)
    (cd apps/simplex-store-service/web && npm ci && npm test)
    ;;
  ci-scripts)
    python3 scripts/ci/test_packages_release.py
    ;;
  website)
    (cd website && npm test)
    ;;
  mcp)
    cd packages/simplex-chat-mcp
    npm ci --no-audit --no-fund
    npx tsc
    npx tsc -p test/tsconfig.json --noEmit
    npm test
    npm run test:e2e
    ;;
  support-bot)
    (cd apps/simplex-support-bot && npm ci --no-audit --no-fund && npm run build && npm test)
    ;;
  support-bot-light)
    cd apps/simplex-support-bot-light
    python3 -m venv .venv
    .venv/bin/pip install -e ../../packages/simplex-chat-python -e '.[dev]'
    .venv/bin/ruff check
    .venv/bin/ruff format --check src tests
    .venv/bin/pyright
    .venv/bin/pytest tests
    ;;
  names-page)
    fixtures="$PWD/tests/fixtures/api/store"
    cd apps/simplex-domains/webapp
    npm ci --no-audit --no-fund
    STORE_FIXTURES="$fixtures" npm test
    npm run build
    ;;
  *)
    echo "usage: $0 badge-web | ci-scripts | website | mcp | support-bot | support-bot-light | names-page" >&2
    exit 1
    ;;
esac
