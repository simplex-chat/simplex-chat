---
name: reset-owners
description: Replace the SimpleX Main group and its owners at once, and show where to see the link to the new one.
disable-model-invocation: true
allowed-tools: Bash(node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" reset-owners)
---

!`node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" reset-owners`

Show the output above to the user exactly as it is.
