#!/usr/bin/env python3

# PreToolUse hook for Claude on the runner: lets through only Bash commands that run the ui helper, with plain
# arguments, so the session has no general shell. Claude Code runs read-only commands such as cat, echo and env without
# asking in every permission mode, and Bash inherits CLAUDE_CODE_OAUTH_TOKEN, so permission rules alone are not enough.
# Exits 2 for anything else; Claude sees the message as the reason. Each decision is appended to CLAUDE_GUARD_LOG
# when it is set, as evidence that the hook ran.

import json
import os
import re
import sys

# nothing that chains, pipes, redirects, substitutes, expands or escapes
UI_COMMAND = re.compile(r"ui( [^;&|<>$`()\\\r\n]*)?")

call = json.load(sys.stdin)
command = (call.get("tool_input") or {}).get("command", "").strip()
allowed = call.get("tool_name") != "Bash" or UI_COMMAND.fullmatch(command)
if os.environ.get("CLAUDE_GUARD_LOG"):
    with open(os.environ["CLAUDE_GUARD_LOG"], "a") as log:
        log.write(f"{'allow' if allowed else 'refuse'} {call.get('tool_name')}: {command[:200]!r}\n")
if not allowed:
    print(f"Only the ui helper runs here, as one command: ui <command> [args]. Refused: {command[:200]}", file=sys.stderr)
    sys.exit(2)
