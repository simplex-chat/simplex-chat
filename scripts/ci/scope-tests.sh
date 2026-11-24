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
    fixtures="$PWD/tests/fixtures/store-service/rest-api"
    cd apps/simplex-domains/webapp
    npm ci --no-audit --no-fund
    STORE_FIXTURES="$fixtures" npx vitest run test/contract test/terms-vectors.test.ts
    ;;
  names-e2e)
    export TMPDIR="${RUNNER_TEMP:-${TMPDIR:-/tmp}}"
    tools="$(mktemp -d)"
    mkdir -p "$tools/ens-contracts" "$tools/test/fixtures"
    for stage in contracts anvil; do
      docker build -q --target "$stage" -t "names-$stage" -f apps/simplex-domains/webapp/dev/Dockerfile apps/simplex-domains
    done
    contracts="$(docker create names-contracts)"
    docker cp "$contracts:/src/ens-contracts/artifacts" "$tools/ens-contracts/artifacts"
    docker cp "$contracts:/src/test/fixtures/terms" "$tools/test/fixtures/terms"
    docker rm "$contracts"
    anvil="$(docker create names-anvil)"
    docker cp "$anvil:/usr/local/bin/anvil" "$tools/anvil"
    docker rm "$anvil"
    (cd apps/simplex-domains/webapp && npm ci --no-audit --no-fund && npx playwright install --with-deps chromium)
    if [ -x simplex-chat-test ]; then export STORE_TEST_BIN="$PWD/simplex-chat-test"; fi
    ulimit -c 0
    ENS_CONTRACTS_DIR="$tools/ens-contracts" ANVIL="$tools/anvil" scripts/store-service/dev/contract-run.sh
    ;;
  build-tool)
    python3 -m unittest discover -s scripts/simplex_build -t scripts
    python3 -m pip install --quiet mypy==1.18.2 ruff==0.14.0
    python3 -m mypy --strict --exclude '(^|/)(test_.*|testing)\.py$' scripts/simplex_build
    python3 -m ruff check scripts/simplex-chat-build.py scripts/simplex_build
    ;;
  *)
    echo "usage: $0 badge-web | ci-scripts | website | mcp | support-bot | support-bot-light | names-page | names-e2e | build-tool" >&2
    exit 1
    ;;
esac
