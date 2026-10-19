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

token=${CLAUDE_CODE_OAUTH_TOKEN:?CLAUDE_CODE_OAUTH_TOKEN must be set}
# only the claude process gets the token, nothing else this script starts
unset CLAUDE_CODE_OAUTH_TOKEN
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
CLAUDE_CODE_OAUTH_TOKEN=$token claude -p \
  --model "$CLAUDE_MODEL" \
  --effort high \
  --restricted \
  --tools Bash,Read,Write,Edit \
  --allowedTools "Bash(ui)" "Bash(ui *)" "Read(/$out/**)" "Edit(/$out/**)" \
  --permission-mode dontAsk \
  --permission-prompts none \
  --strict-mcp-config \
  --max-turns 60 \
  --max-budget-usd 3 \
  --autocompact 200k \
  --no-session-persistence \
  --settings "$settings" \
  --output-format stream-json \
  --verbose \
  < prompt.md > "$transcript" 2> "$stderr" || status=$?

# A second safeguard to the file tools' limits: what the session wrote is printed and uploaded only when it is known
# not to hold the token. grep reads the token from a pipe, so it is in no process's arguments.
leak=0
printf '%s\n' "$token" | grep -rqF -f - -- "$out" "$transcript" "$stderr" "$CLAUDE_GUARD_LOG" || leak=$?
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
grep -qxE '[*`]*CLAUDE-TEST: PASS[*`.]*[[:space:]]*' <<< "$last"
