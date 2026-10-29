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
  *)
    echo "usage: $0 badge-web | ci-scripts | ci-failures" >&2
    exit 1
    ;;
esac
