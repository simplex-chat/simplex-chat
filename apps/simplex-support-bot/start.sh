#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# --- Required ---
# GROK_API_KEY      xAI API key (env var)
# --team-group      Team group display name

# --- Optional ---
# --db-prefix       Database file prefix (default: ./data/simplex)
# --auto-add-team-members (-a)  Comma-separated ID:name pairs (e.g. 1:Alice,2:Bob)
# --group-links     Public group link(s) shown in welcome message
# --timezone        IANA timezone for weekend detection (default: UTC)
# --complete-hours  Hours of inactivity before auto-complete (default: 3)

if [ -z "${GROK_API_KEY:-}" ]; then
  echo "Error: GROK_API_KEY environment variable is required" >&2
  exit 1
fi

if [ ! -f dist/index.js ]; then
  echo "Error: dist/index.js not found. Run ./build.sh first." >&2
  exit 1
fi

exec node dist/index.js "$@"
