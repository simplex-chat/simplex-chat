#!/bin/bash

# Runs the device test plan with Claude Code on this runner after the smoke test, one session per layout,
# as claude-scope says. A session drives the app only through the ui helper, and reads and writes only its
# own directory <output dir>/<layout>: prompt.md, report.md, shots/, transcript.jsonl (stream-json).
# Usage: scripts/ci/ios-sim/claude-test.sh <output dir>
# Needs CLAUDE_CODE_OAUTH_TOKEN, SIM_UDID (the booted simulator with the app) and BACKEND_DIR (the running test backend).

set -euo pipefail

token=${CLAUDE_CODE_OAUTH_TOKEN:?CLAUDE_CODE_OAUTH_TOKEN must be set}
# only the claude processes get the token, nothing else this script starts
unset CLAUDE_CODE_OAUTH_TOKEN

mkdir -p "$1"
out=$(cd "$1" && pwd)
dir=$(cd "$(dirname "$0")" && pwd)
: "${SIM_UDID:?SIM_UDID must be the simulator UDID}"
: "${BACKEND_DIR:?BACKEND_DIR must be the work dir of the test backend}"

scope() {
  sed -n "s/^$1: *//p" "$dir/claude-scope"
}

model=$(scope model)
layouts=$(scope layouts)
cases=$(scope cases)
max_turns=$(scope max-turns)
max_budget=$(scope max-budget-usd)

export PATH="$dir:$PATH" BACKEND_DIR APP_LOG=$out/app.log CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1
cp "$BACKEND_DIR/seed.md" "$out/seed.md"
settings=$out/settings.json
cat > "$settings" << EOF
{"hooks": {"PreToolUse": [{"matcher": "Bash", "hooks": [{"type": "command", "command": "$dir/claude-guard.py"}]}]}}
EOF

xcrun simctl spawn "$SIM_UDID" log stream --level debug --style compact --predicate 'process == "SimpleX"' > "$APP_LOG" 2>&1 &
log_pid=$!
trap 'kill "$log_pid" 2> /dev/null || true' EXIT

layout_name() {
  case "$1" in
    reachable) echo "the one-hand UI (reachable toolbar)" ;;
    top) echo "the top bar" ;;
  esac
}

write_prompt() {
  local layout=$1 previous=$2
  cat "$dir/claude-prompt.md"
  echo
  echo "## The ui commands"
  echo
  echo '```'
  ui || true
  echo '```'
  echo
  echo "## This session"
  echo
  echo "- Layout: $(layout_name "$layout")."
  echo "- Cases: $cases."
  if [ -z "$previous" ]; then
    echo "- At the start: the smoke test onboarded the profile citester, which has no conversations, and searched the directory once, choosing \"Search\" in the alert."
  else
    echo "- At the start: an earlier session ran the cases in $(layout_name "$previous") on this app, so the profile may have chats, other profiles may exist and hints may be changed; look at the state first."
  fi
  echo "- Limits: $max_turns turns and \$$max_budget (estimated at API prices). Keep report.md current as you go."
  echo
  cat "$out/seed.md"
  echo
  cat "$dir/device-test-plan.md"
}

status=0
previous=""
for layout in $layouts; do
  session=$out/$layout
  mkdir -p "$session/shots"
  ui layout "$layout"
  write_prompt "$layout" "$previous" > "$session/prompt.md"
  echo "::group::Claude session: $layout, cases $cases, $model, $max_turns turns, \$$max_budget"
  session_status=0
  # outside the session's directory, so the session cannot change it
  export CLAUDE_GUARD_LOG=$out/$layout-guard.log
  : > "$CLAUDE_GUARD_LOG"
  (
    cd "$session"
    UI_OUT=$session/shots CLAUDE_CODE_OAUTH_TOKEN=$token claude -p \
      --model "$model" \
      --effort high \
      --restricted \
      --tools Bash,Read,Write,Edit \
      --allowedTools "Bash(ui)" "Bash(ui *)" Read Write Edit \
      --permission-mode dontAsk \
      --permission-prompts none \
      --strict-mcp-config \
      --max-turns "$max_turns" \
      --max-budget-usd "$max_budget" \
      --autocompact 200k \
      --no-session-persistence \
      --settings "$settings" \
      --output-format stream-json \
      --verbose \
      < prompt.md > transcript.jsonl 2> stderr.log
  ) || session_status=$?
  echo "claude exited with $session_status"
  tail -n 20 "$session/stderr.log"
  if [ -f "$session/report.md" ]; then
    cat "$session/report.md"
  else
    echo "no report.md"
  fi
  echo "guard hook: $(grep -c '^allow' "$CLAUDE_GUARD_LOG" || true) allowed, $(grep -c '^refuse' "$CLAUDE_GUARD_LOG" || true) refused"
  grep '^refuse' "$CLAUDE_GUARD_LOG" || true
  echo "::endgroup::"
  # the token must never be uploaded, whatever the session did; grep reads it from a pipe, not from its arguments
  if grep -rqF -f <(printf '%s\n' "$token") "$session" "$CLAUDE_GUARD_LOG"; then
    rm -rf "$session" "$CLAUDE_GUARD_LOG"
    echo "::error::the $layout session's files contained the token, so they were deleted before the upload"
    session_status=1
  fi
  if [ "$session_status" -ne 0 ]; then
    status=$session_status
  fi
  previous=$layout
done
exit "$status"
