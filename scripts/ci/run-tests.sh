#!/bin/bash

set -euo pipefail

if [ "$#" -eq 0 ]; then
  echo "usage: $0 <test command>" >&2
  exit 1
fi

if [ -n "${PR_BODY:-}" ]; then
  TESTS=$(printf '%s\n' "$PR_BODY" | tr -d '\r' | awk '/^```ci-tests[ \t]*$/ {f = 1; next} /^```[ \t]*$/ {f = 0} f')
fi
args=()
while read -r p; do
  if [ -n "$p" ]; then args+=(-m "$p"); fi
done <<< "${TESTS:-}"
if [ "${#args[@]}" -gt 0 ]; then
  args+=(--fail-on=empty)
elif [ "${REDUCED:-}" = true ]; then
  echo "No tests selected"
  exit 0
fi

i=1
attempts=${ATTEMPTS:-1}
while [ "$i" -le "$attempts" ]; do
  if "$@" "${args[@]}"; then
    break
  elif [ "$i" -lt "$attempts" ]; then
    echo "Attempt $i failed, retrying..."
    sleep 1
  fi
  i=$((i + 1))
done
if [ "$i" -gt "$attempts" ]; then
  echo "Attempt $attempts failed."
  exit 1
fi
