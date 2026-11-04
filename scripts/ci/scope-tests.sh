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
  names-web)
    webapp=apps/simplex-domains/webapp
    (cd "$webapp" && npm ci && npm test && npm run build && npx playwright install --with-deps chromium)
    tools="$(mktemp -d)"
    docker buildx build --load -t simplex-names-dev -f "$webapp/dev/Dockerfile" apps/simplex-domains
    container="$(docker create simplex-names-dev)"
    docker cp "$container:/contracts" "$tools/contracts"
    docker cp "$container:/usr/local/bin/anvil" "$tools/anvil"
    docker rm "$container"
    ENS_CONTRACTS_DIR="$tools/contracts" ANVIL="$tools/anvil" \
      STORE_SERVICE_BIN="$(find dist-newstyle -name simplex-store-service -type f -executable -print -quit)" \
      scripts/store-service/dev/contract-run.sh
    ;;
  *)
    echo "usage: $0 badge-web | ci-scripts | names-web" >&2
    exit 1
    ;;
esac
