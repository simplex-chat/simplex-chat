#!/bin/bash

# Runs the device test plan with Claude Code on this runner after the smoke test, one session per layout,
# as claude-scope says. A session drives the app only through the ui helper, and reads and writes only its
# own directory, which ends up in <output dir>/<layout>: prompt.md, report.md, shots/, transcript.jsonl (stream-json).
# It passes when every session's final message ends with "CLAUDE-TEST: PASS".
# Usage: scripts/ci/ios-sim/claude-test.sh <output dir>
# Needs CLAUDE_CODE_OAUTH_TOKEN, SIM_UDID (the booted simulator with the app) and, when the test backend runs, BACKEND_DIR.

set -euo pipefail

token=${CLAUDE_CODE_OAUTH_TOKEN:?CLAUDE_CODE_OAUTH_TOKEN must be set}
# only the claude processes get the token, nothing else this script starts
unset CLAUDE_CODE_OAUTH_TOKEN

mkdir -p "$1"
out=$(cd "$1" && pwd)
# sessions run here and move to the output dir, which is uploaded, only after their files are checked for the token
staging=$out.sessions
mkdir -p "$staging"
dir=$(cd "$(dirname "$0")" && pwd)
: "${SIM_UDID:?SIM_UDID must be the simulator UDID}"
BACKEND_DIR=${BACKEND_DIR:-}

scope() {
  sed -n "s/^$1: *//p" "$dir/claude-scope"
}

model=$(scope model)
layouts=$(scope layouts)
max_turns=$(scope max-turns)
max_budget=$(scope max-budget-usd)

export PATH="$dir:$PATH" BACKEND_DIR APP_LOG=$out/app.log CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1
if [ -n "$BACKEND_DIR" ]; then
  cp "$BACKEND_DIR/seed.md" "$out/seed.md"
fi
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
  echo "- Cases: $(scope "cases-$layout")."
  local entries
  entries=$(scope "entries-$layout")
  if [ -n "$entries" ]; then
    echo "- Entries to use: $entries."
  fi
  if [ -n "$BACKEND_DIR" ]; then
    echo "- Link to paste (C4, W3): $(cat "$BACKEND_DIR/directory-link") (the test directory's address)."
    echo "- Names server to add in the app: $(cat "$BACKEND_DIR/names-server")."
  else
    echo "- No test backend runs in this session."
  fi
  if [ -z "$previous" ] && [ -n "$BACKEND_DIR" ]; then
    echo "- At the start: the smoke test onboarded a profile (typed as \"citester\"; the keyboard may have autocorrected the name), searched the directory in both layouts, choosing \"Search\" in the alert each time, and joined the channel ci_cards; it is the profile's only conversation."
  elif [ -z "$previous" ]; then
    echo "- At the start: the smoke test onboarded a profile (typed as \"citester\"; the keyboard may have autocorrected the name) and checked the chat list in both layouts; the profile has no conversations."
  else
    echo "- At the start: an earlier session ran the cases in $(layout_name "$previous") on this app, so the profile may have chats, other profiles may exist and hints may be changed; look at the state first."
  fi
  echo "- Limits: $max_turns turns and \$$max_budget (estimated at API prices). Keep report.md current as you go."
  echo
  if [ -n "$BACKEND_DIR" ]; then
    cat "$out/seed.md"
    echo
  fi
  cat "$dir/device-test-plan.md"
}

status=0
previous=""
for layout in $layouts; do
  session=$staging/$layout
  mkdir -p "$session/shots"
  echo "$layout" >> "$out/sessions"
  ui layout "$layout"
  write_prompt "$layout" "$previous" > "$session/prompt.md"
  echo "::group::Claude session: $layout, $model, $max_turns turns, \$$max_budget"
  session_status=0
  # outside the session's directory, so the session cannot change it
  export CLAUDE_GUARD_LOG=$staging/$layout-guard.log
  : > "$CLAUDE_GUARD_LOG"
  (
    cd "$session"
    UI_OUT=$session/shots CLAUDE_CODE_OAUTH_TOKEN=$token claude -p \
      --model "$model" \
      --effort high \
      --restricted \
      --tools Bash,Read,Write,Edit \
      --allowedTools "Bash(ui)" "Bash(ui *)" "Read(/$session/**)" "Edit(/$session/**)" \
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
  # the token must never be printed or uploaded, whatever the session did; grep reads it from a pipe, not from its arguments
  leak=0
  grep -rqF -f <(printf '%s\n' "$token") "$session" "$CLAUDE_GUARD_LOG" || leak=$?
  if [ "$leak" -ne 1 ]; then
    rm -rf "$session" "$CLAUDE_GUARD_LOG"
    echo "::error::the $layout session's files contain the token, or checking for it failed (grep exited $leak), so they were deleted before the upload"
    session_status=1
  else
    mv "$session" "$CLAUDE_GUARD_LOG" "$out/"
    session=$out/$layout
    CLAUDE_GUARD_LOG=$out/$layout-guard.log
    tail -n 20 "$session/stderr.log"
    if [ -f "$session/report.md" ]; then
      cat "$session/report.md"
    else
      echo "no report.md"
    fi
    echo "guard hook: $(grep -c '^allow' "$CLAUDE_GUARD_LOG" || true) allowed, $(grep -c '^refuse' "$CLAUDE_GUARD_LOG" || true) refused"
    grep '^refuse' "$CLAUDE_GUARD_LOG" || true
    result=$(jq -r -R 'fromjson? | select(.type == "result") | .result // empty' "$session/transcript.jsonl" | tail -n 20 || true)
    echo "final message, last lines:"
    echo "$result"
    last=$(grep -v '^[[:space:]]*$' <<< "$result" | tail -n 1 || true)
    if ! grep -qxE '[*`]*CLAUDE-TEST: PASS[*`.]*[[:space:]]*' <<< "$last"; then
      echo "::error::the $layout session's final message does not end with CLAUDE-TEST: PASS"
      session_status=1
    fi
  fi
  echo "::endgroup::"
  if [ "$session_status" -ne 0 ]; then
    status=$session_status
  fi
  previous=$layout
done
exit "$status"
