#!/usr/bin/env node
process.stdout.write(JSON.stringify({hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "allow"}}))
