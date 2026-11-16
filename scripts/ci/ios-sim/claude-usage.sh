#!/bin/bash

# Reports what each Claude session of claude-test.sh spent, from the final result event of its transcript,
# to the job summary and to <output dir>/claude-usage.json. total_cost_usd is Claude Code's own estimate
# at API prices, not a bill.
# Usage: scripts/ci/ios-sim/claude-usage.sh <output dir>

set -euo pipefail

out=$1
summary=${GITHUB_STEP_SUMMARY:-/dev/stdout}
sessions=0
mkdir -p "$out"

{
  echo "## Claude sessions"
  echo
  echo "| Session | Result | Turns | Duration | Input | Output | Cache read | Cache write | Per model, estimated at API prices | Total, estimated at API prices | Permission denials |"
  echo "|---|---|---|---|---|---|---|---|---|---|---|"
} >> "$summary"

# claude-test.sh lists each session it starts in <output dir>/sessions; a session's files reach <output dir> only once
# they are checked for the token
while read -r session; do
  sessions=$((sessions + 1))
  transcript=$out/$session/transcript.jsonl
  if [ ! -f "$transcript" ]; then
    if [ -d "$out.sessions/$session" ]; then
      reason="cut short before its files were checked for the token, so they are not uploaded"
    else
      reason="its files held the token or could not be checked, so they were deleted"
    fi
    echo "| $session | $reason: usage unknown | | | | | | | | | |" >> "$summary"
    mkdir -p "$out/$session"
    jq -n --arg s "$session" '{session: $s, result: "missing"}' > "$out/$session/usage.json"
    continue
  fi
  result=$(jq -c -R 'fromjson? | select(.type == "result")' "$transcript" | tail -n 1)
  if [ -z "$result" ]; then
    echo "| $session | no result event: the session timed out or crashed | | | | | | | | | |" >> "$summary"
    jq -n --arg s "$session" '{session: $s, result: "missing"}' > "$out/$session/usage.json"
    continue
  fi
  jq --arg s "$session" '{session: $s, subtype, is_error, num_turns, duration_ms, usage: (.usage | {input_tokens, output_tokens, cache_read_input_tokens, cache_creation_input_tokens}), modelUsage, total_cost_usd, permission_denials: (.permission_denials // [] | length)}' <<< "$result" > "$out/$session/usage.json"
  jq -r --arg s "$session" '"| \($s) | \(.subtype)\(if .is_error then " (error)" else "" end) | \(.num_turns) | \(.duration_ms / 60000 | floor) min | \(.usage.input_tokens) | \(.usage.output_tokens) | \(.usage.cache_read_input_tokens) | \(.usage.cache_creation_input_tokens) | \([.modelUsage // {} | to_entries[] | "\(.key) $\(.value.costUSD // 0 | . * 100 | round / 100)"] | join(", ")) | $\(.total_cost_usd // 0 | . * 100 | round / 100) | \(.permission_denials // [] | length) |"' <<< "$result" >> "$summary"
done < <(cat "$out/sessions" 2> /dev/null)

if [ "$sessions" -eq 0 ]; then
  echo "No Claude session ran." >> "$summary"
  echo "[]" > "$out/claude-usage.json"
else
  jq -s '.' "$out"/*/usage.json > "$out/claude-usage.json"
fi
cat "$out/claude-usage.json"
