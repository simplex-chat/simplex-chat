#!/usr/bin/env python3

# Reports what a Claude Code run spent, from the result event at the end of its stream-json transcript: prints it,
# adds it to the job summary, and writes it as JSON. Without a result event (a timeout or a crash) it says so
# rather than reporting zero.
# Usage: scripts/ci/desktop-linux/claude-usage.py <transcript .jsonl> <output .json>

import json
import os
import sys

FIELDS = ["subtype", "is_error", "num_turns", "duration_ms", "total_cost_usd", "usage", "modelUsage"]


def last_result(path):
    result = None
    try:
        with open(path) as f:
            for line in f:
                try:
                    event = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if isinstance(event, dict) and event.get("type") == "result":
                    result = event
    except FileNotFoundError:
        pass
    return result


def summary(result):
    lines = ["### Claude run usage", ""]
    if result is None:
        return lines + ["No result event in the transcript (timeout or crash): usage unknown, not zero."]
    usage = result.get("usage") or {}
    lines += [
        f"- result: {result.get('subtype')}{' (error)' if result.get('is_error') else ''}",
        f"- turns: {result.get('num_turns')}",
        f"- duration: {(result.get('duration_ms') or 0) / 1000:.0f} s",
        f"- tokens: input {usage.get('input_tokens')}, output {usage.get('output_tokens')}, "
        f"cache read {usage.get('cache_read_input_tokens')}, cache write {usage.get('cache_creation_input_tokens')}",
        f"- total_cost_usd: {result.get('total_cost_usd')}, estimated at API prices "
        "(computed by Claude Code; on a subscription it is not a bill)",
    ]
    for model, model_usage in (result.get("modelUsage") or {}).items():
        lines.append(f"- {model}: {json.dumps(model_usage)}")
    return lines


def main():
    transcript, out = sys.argv[1], sys.argv[2]
    result = last_result(transcript)
    with open(out, "w") as f:
        json.dump({k: result.get(k) for k in FIELDS} if result else {"result": None}, f, indent=2)
    text = "\n".join(summary(result))
    print(text)
    if os.environ.get("GITHUB_STEP_SUMMARY"):
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as f:
            f.write(text + "\n")


main()
