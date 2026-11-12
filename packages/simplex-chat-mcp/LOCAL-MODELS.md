# SimpleX MCP server: local models

The agent is run here with opencode as the MCP host and a llama.cpp model served by llama-swap. The general setup of other hosts is in [REFERENCE.md](REFERENCE.md#other-mcp-hosts). Statements about opencode were checked against its source, version 1.19.5.

## opencode as host

Add the agent to `mcp` in opencode's config, with `timeout`:

```json
{
  "mcp": {
    "simplex-my-agent": {
      "type": "remote",
      "url": "http://127.0.0.1:5226/mcp",
      "headers": {"Authorization": "Bearer <token>"},
      "timeout": 169800000
    }
  }
}
```

- opencode applies `timeout`, in milliseconds, to the connection, the tool list and each tool call. Without it, opencode allows 30 seconds for the connection and the tool list, and its MCP client ends a tool call after 60 seconds; the 5-second default stated in opencode's documentation is not applied.
- opencode's MCP client restarts the tool-call limit at each progress notification. The server sends one every 60 seconds during a wait, so with the 60-second default, opencode can end a longer wait with a timeout.
- 169800000 is the `timeout` of the Claude Code entry made by `register` at the default expiry: the expiry plus 24 hours and 10 minutes.
- `experimental.mcp_timeout` is used for the tool calls of entries without `timeout`.

opencode reads the tool list of the server at each connection and keeps it until the next one. When the server was not running at opencode's start, or was upgraded since, reconnect:
- in the TUI: open `/mcps` and press space on `simplex-my-agent`, twice when it is shown as Enabled;
- with `opencode serve`: `POST /mcp/simplex-my-agent/connect`.

opencode connects anew at each `opencode run` without `--attach`.

### Context limits

Set `limit` in each local model's entry of opencode's provider config; opencode requires both fields:

```json
"models": {
  "MODEL": {"limit": {"context": 32768, "output": 4096}}
}
```

- Without `limit`, unless models.dev lists the same provider and model ID, opencode compacts a session only after llama-server rejects a request as too long; opencode recognizes llama-server's error text.
- With `limit`, opencode starts compaction at `context` minus `output` tokens and sends `output` as the answer limit. An `output` of 0, or above 32000, is taken as 32000; with a 32768-token context, the threshold is then about 768 tokens.

### Sessions and compaction

- With `opencode run --continue`, opencode resumes the top-level session updated last, not the one created last.
- After a switch to a model with a smaller context, start a new session: opencode can fail to compact the earlier history with that model. The error is `Conversation history too large to compact - exceeds model context limit` for a model without `limit`, otherwise `Session too large to compact - context exceeds model limit even after stripping media`. Alternatively, set a model with a larger context for the `compaction` agent: `"agent": {"compaction": {"model": "PROVIDER/MODEL"}}`.
- Into the summary request, opencode copies the compacted tool calls with their arguments, and their results up to 2000 characters each; it instructs the model to keep exact commands and identifiers. Keep secrets in files and refer to them in commands, as `$(cat FILE)` or `curl --netrc-file FILE`.

## Prompts for smaller models

opencode sends its default system prompt unless one of `gpt`, `o1`, `o3`, `gemini-`, `claude`, `trinity`, `kimi` and `muse` is found in the model ID. In that prompt, the model is described as a CLI tool for software engineering tasks, and answers are limited to fewer than 4 lines. An agent's `prompt` is sent in its place:

```json
{
  "agent": {
    "simplex": {"prompt": "{file:./simplex-agent.md}"}
  }
}
```

```
opencode run --agent simplex < task.md
```

- `{file:PATH}` is read relative to the config file.
- For an unknown agent name, or a subagent, opencode prints a warning and runs its default agent.
- opencode joins the arguments of `opencode run` with spaces and wraps each argument with a space in double quotes, escaping inner ones: a prompt with spaces given as one argument is sent quoted. Text from stdin is appended as it is.
- opencode ends the session loop at the model's first answer without a tool call, and `opencode run` exits then. To keep the agent in a chat, ask the model in the prompt to answer only with `send` and to call `receive` after each answer.
- At compaction, opencode replaces earlier messages with a summary, the first prompt included, unless it is among the recent messages kept. opencode sends the agent's `prompt` at every step: put standing rules there.

opencode adds instruction files to every system prompt, after an agent's `prompt` too:
- `AGENTS.md` in opencode's config folder, `~/.config/opencode`, or else `~/.claude/CLAUDE.md`;
- the `AGENTS.md` files from the working folder up to the project root; without them, the `CLAUDE.md` files.

Remove instructions written for interactive work from these files. With `OPENCODE_DISABLE_CLAUDE_CODE_PROMPT=1`, opencode leaves out the `CLAUDE.md` files.

## llama-swap

### Groups

llama-swap puts the models outside `groups` into one group. When llama-swap loads a member of a group, it unloads, by default:
- the other members of that group (`swap: true`);
- the models of other groups (`exclusive: true`), except groups with `persistent: true`.

With these defaults, llama-swap runs one model at a time. For a model loaded beside the main one, use a group such as:

```yaml
groups:
  beside:
    swap: false
    exclusive: false
    persistent: true
    members:
      - small-model
```

llama-swap keeps the members of a group with `swap: false` loaded together.

### Captures

llama-swap keeps the latest model requests in a memory buffer, 5 MB by default (`captureBuffer`): for chat requests, the request and response bodies, with the conversation and the messages of the agent's chats in them. It returns them at `GET /api/captures/ID`, checks a key only when `apiKeys` is set, and listens on all interfaces by default. With private chats, set `captureBuffer: 0` or `apiKeys`, or start llama-swap with `-listen 127.0.0.1:8080`.

## Containers

With the server and the agent in separate containers:
- The server accepts only loopback connections with a localhost `Host` ([REFERENCE.md](REFERENCE.md#security)). Run both containers in one network namespace, such as one pod, and use the URL `http://127.0.0.1:5226/mcp`.
- The server opens the path given in `file` of `send` in its own container and needs read permission on it. Mount the agent's workspace at the same path in both containers, readable by the server's user, outside the server's data folder and `~/.simplex-mcp`.
- The server returns the path of a downloaded file under `files/` of its data folder, in a folder with mode 0700. Mount that `files/` folder read-only into the agent's container at its server path, and run the agent with the server's user ID.
