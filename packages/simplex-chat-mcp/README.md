# SimpleX MCP server

Your Claude Code can now chat on SimpleX.

SimpleX Network is the only messaging network without any user identifiers - no phone numbers, usernames, account IDs or even random IDs. Your agent needs no account, no platform controls its identity, and nobody, not even the servers, can see who it talks to. The app was downloaded more than 3M times, and over 600,000 people use it every month.

With this package, Claude Code can join channels and groups, talk to bots, and even receive your prompts via the SimpleX app. It asks you on your phone before it connects or sends - one tap to approve.

<p>
  <img src="https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/packages/simplex-chat-mcp/images/approval.png" alt="The agent asks in its chat before it sends a message; one tap approves it" width="320">
  <img src="https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/packages/simplex-chat-mcp/images/prompts.png" alt="Prompting the agent from the SimpleX app" width="320">
</p>

## Try it

You need Node.js 22 or later, curl, Claude Code and the [SimpleX Chat app](https://simplex.chat/downloads/).

1. Start your agent and keep it running:

   ```
   npx -y @simplex-chat/mcp serve my-agent --display-name "My Agent"
   ```

   Scan the QR code it prints with the SimpleX app, and tap Join. The chat "My Agent" appears: your agent asks there before it acts. Ctrl-C stops it; when requests were waiting or status messages were shown, it first marks the requests cancelled in "My Agent" and deletes the status messages, waiting up to 10 seconds (`--stop-wait`), and a second Ctrl-C stops it at once.

2. In another terminal, add it to Claude Code:

   ```
   npx -y @simplex-chat/mcp register my-agent
   ```

   It also adds status hooks to `~/.claude/settings.json`: with them, Claude Code sends its events to the agent with curl.

3. Add `"mcp__simplex-my-agent"` to `permissions.allow` in `~/.claude/settings.json`. You approve in the app, so Claude Code doesn't need to ask too.

4. Start `claude` and ask:

   ```
   Search the SimpleX Directory for security groups: https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok
   ```

   Approve its cards in "My Agent" with one tap.

## Prompt your agent from the app

Give Claude Code your SimpleX address:

```
Connect to me as yourself at <your SimpleX address>, with the message:
"I'm your agent, connected as you asked in Claude Code. I'll act on your prompts here, except security-sensitive ones."
Then take my prompts from that chat and answer there: wait for my messages with receive (wait 3600, typing true),
answer each one in that chat with send (wait 0), and call receive again after every answer, until I say stop.
```

Or give it the link of a group of yours, where every member prompts it:

```
Connect as yourself to my group at <your group link>.
Then take the prompts of its members from that group and answer there: wait for their messages with receive (wait 3600, typing true),
answer each one in that group with send (wait 0), and call receive again after every answer, until I say stop.
```

Approve the connection in "My Agent", and accept the request in the app if it asks. On the card of the agent's first answer, tap `all`: its later answers in that chat need no approval.

Once you tap `all`, the agent shows what it does in a live message in that chat, for example `◐ thinking · 0:12`, and deletes that message when its answer is sent; in a group, every member sees it. With the status hooks, the agent also shows Claude Code's work there, for example running a command, reading or editing a file, searching, running a subagent, or waiting for your permission in Claude Code. To have the status message removed rather than marked deleted, set "Delete for everyone" to "always" in the agent contact's preferences, or turn it on in the group's preferences. Keep that chat in your active chat profile: in other profiles, the Android and desktop apps notify of every update. With a local model behind llama-swap, start the agent with `--inference http://127.0.0.1:8080`: the agent then shows the model loading and its tokens per second.

## Safety

- The agent asks in "My Agent" before it connects, sends a message or a file, downloads a file or leaves; you approve or reject. On a download card, `/receive` gives the agent the file, and `/forward` sends it only to you.
- `all` approves every message to one chat; `/revoke` ends it. `/receive CHAT N all` lets the agent receive that file and later files from one chat on the agent's servers; `/receive CHAT off` ends it.
- After `all`, the agent's status message in that chat and its notes there are sent without asking, as its answers are; in a group, every member sees them. Each note is printed in the terminal where `serve` runs, in place of a copy in "My Agent". In the status message, everyone in that chat sees the names of the files Claude Code reads and edits, without their folders, also for its work outside that chat.
- Messages in your prompt chat can become prompts for your Claude Code session: use a chat where only you and people you trust write.
- The agent runs on your computer and has no address in SimpleX Network, so nobody can contact it first.

## Only Claude Code, for now

The agent is tested with Claude Code. Tell us which other apps and models you want to use in [an issue](https://github.com/simplex-chat/simplex-chat/issues), or contribute.

## Reference

Approval modes, owners, several agents, other MCP hosts, recovery and all options: [REFERENCE.md](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-mcp/REFERENCE.md).

## Own a part of SimpleX

Business is moving into messages, and AI agents follow: an agent can ask a company's agent in a chat instead of opening the company's website, and buy there too. These chats hold plans, purchases and accounts, so with AI everyone feels the need for private and secure communication.

SimpleX Chat, the company that builds SimpleX Network, is raising a community round on Wefunder: [wefunder.com/simplex.chat](https://wefunder.com/simplex.chat?utm_source=mcp)
