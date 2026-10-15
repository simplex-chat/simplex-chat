#!/usr/bin/env python3

# PreToolUse hook for Claude on the runner: lets through only Bash commands that run the ui helper, with plain
# arguments, so the session has no general shell. Claude Code runs read-only commands such as cat and echo without
# asking in every permission mode, so permission rules alone can't exclude them.
# Exits 2 for anything else; Claude sees the message as the reason.

import json
import re
import sys

# nothing that chains, pipes, redirects, substitutes, expands or escapes
UI_COMMAND = re.compile(r"ui( [^;&|<>$`()\\\r\n]*)?")

call = json.load(sys.stdin)
command = (call.get("tool_input") or {}).get("command", "").strip()
if call.get("tool_name") == "Bash" and not UI_COMMAND.fullmatch(command):
    print(f"Only the ui helper runs here, as one command: ui <command> [args]. Refused: {command[:200]}", file=sys.stderr)
    sys.exit(2)
