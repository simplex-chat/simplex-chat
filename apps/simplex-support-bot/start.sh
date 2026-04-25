#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# --- Required ---
# GROK_API_KEY           xAI API key (env var)
# --team-group           Team group display name
# --pg-conn              PostgreSQL connection string. Required only when
#                        simplex-chat-nodejs was installed with the postgres
#                        backend (SIMPLEX_BACKEND=postgres, .npmrc, or
#                        --simplex_backend=postgres at install time).

# --- Optional ---
# --state-file           Path to the bot's state JSON (default: ./data/state.json)
# --sqlite-file-prefix   SQLite DB file prefix (default: ./data/simplex)
# --sqlite-key           SQLCipher encryption key (default: unencrypted)
# --pg-schema            PostgreSQL schema prefix (default: simplex_v1)
# --auto-add-team-members (-a)  Comma-separated ID:name pairs (e.g. 1:Alice,2:Bob)
# --group-links          Public group link(s) shown in welcome message
# --timezone             IANA timezone for weekend detection (default: UTC)
# --complete-hours       Hours of inactivity before auto-complete (default: 3)

if [ -z "${GROK_API_KEY:-}" ]; then
  echo "Error: GROK_API_KEY environment variable is required" >&2
  exit 1
fi

if [ ! -f dist/index.js ]; then
  echo "Error: dist/index.js not found. Run ./build.sh first." >&2
  exit 1
fi

# Run via `npm start` so npm exposes `.npmrc` values (e.g. simplex_backend=postgres)
# as `npm_config_*` env vars to the bot.
exec npm --silent start -- "$@"
