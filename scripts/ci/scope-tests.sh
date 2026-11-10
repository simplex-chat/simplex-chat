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
  names-exporter)
    (cd apps/simplex-names-exporter && python3 -m pip install -e '.[dev]' && python3 -m pytest -q)
    ;;
  *)
    echo "usage: $0 badge-web | ci-scripts | names-exporter" >&2
    exit 1
    ;;
esac
