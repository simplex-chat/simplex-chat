#!/bin/bash

# Runs Claude Code to repeat the smoke test with the ui helper, as claude-test.md describes, and passes when
# its final message ends with "CLAUDE-TEST: PASS".
# Usage: scripts/ci/desktop-linux/claude-test.sh <output dir> <transcript .jsonl>
# Expects the app started (smoke-test.sh ... start) with UI_OUT under <output dir>, claude and ui on PATH,
# CLAUDE_MODEL and CLAUDE_CODE_OAUTH_TOKEN. Claude works in <output dir>: the screenshots, its report.md and the
# prompt it was given end up there. The transcript, Claude Code's stderr (<transcript>.stderr) and the guard hook's
# log (<transcript>.guard.log) are kept outside it, where Claude can't write. If any of them holds the token, all are
# deleted and the run fails.

set -euo pipefail

: "${CLAUDE_CODE_OAUTH_TOKEN:?CLAUDE_CODE_OAUTH_TOKEN must be set}"
here=$(cd "$(dirname "$0")" && pwd)
out=$(cd "$1" && pwd)
transcript=$2
stderr=$transcript.stderr
settings=$(mktemp)
export CLAUDE_GUARD_LOG=$transcript.guard.log
: > "$CLAUDE_GUARD_LOG"

{
  cat "$here/claude-test.md"
  echo
  echo '```'
  "$here/ui" || true
  echo '```'
} > "$out/prompt.md"

cat > "$settings" << EOF
{"hooks": {"PreToolUse": [{"matcher": "Bash", "hooks": [{"type": "command", "command": "$here/claude-guard.py"}]}]}}
EOF

claude --version
cd "$out"
status=0
claude -p "$(cat prompt.md)" \
  --model "$CLAUDE_MODEL" \
  --max-turns 60 \
  --max-budget-usd 3 \
  --output-format stream-json \
  --verbose \
  --tools Bash,Read,Write \
  --permission-mode dontAsk \
  --allowedTools "Bash(ui *)" "Read(/$out/**)" "Edit(/$out/**)" \
  --disallowedTools "mcp__*" \
  --strict-mcp-config \
  --disable-slash-commands \
  --no-session-persistence \
  --settings "$settings" \
  < /dev/null > "$transcript" 2> "$stderr" || status=$?

# A second safeguard to the file tools' limits: what the session wrote is printed and uploaded only when it is known
# not to hold the token. grep reads the token from a pipe, so it is in no process's arguments.
leak=0
printf '%s\n' "$CLAUDE_CODE_OAUTH_TOKEN" | grep -rqF -f - -- "$out" "$transcript" "$stderr" "$CLAUDE_GUARD_LOG" || leak=$?
if [ "$leak" -ne 1 ]; then
  rm -rf -- "$out" "$transcript" "$stderr" "$CLAUDE_GUARD_LOG"
  echo "error: the token is in the Claude session's output, or checking for it failed (grep exited $leak); the output, transcript, stderr and guard log are deleted"
  exit 1
fi

echo "::group::claude stderr"
cat "$stderr"
echo "::endgroup::"
echo "::group::report.md"
cat report.md 2> /dev/null || echo "no report.md"
echo "::endgroup::"
echo "::group::guard hook decisions"
cat "$CLAUDE_GUARD_LOG"
echo "::endgroup::"
result=$(jq -r 'select(.type == "result") | .result // empty' "$transcript" 2> /dev/null | tail -n 20 || true)
echo "final message, last lines:"
echo "$result"
if [ "$status" -ne 0 ]; then
  echo "error: claude exited with $status"
  exit 1
fi
last=$(grep -v '^[[:space:]]*$' <<< "$result" | tail -n 1 || true)
grep -qxE '[*`]*CLAUDE-TEST: PASS[*`]*' <<< "$last"
