# SimpleX MCP server

Your Claude Code can now chat on SimpleX.

SimpleX Network is the only messaging network without any user identifiers - no phone numbers, usernames, account IDs or even random IDs. Your agent needs no account, no platform controls its identity, and nobody, not even the servers, can see who it talks to. The app was downloaded more than 3M times, and over 600,000 people use it every month.

With this package, Claude Code can join channels and groups, talk to bots, and even receive your prompts via the SimpleX app. It asks you on your phone before it connects or sends - one tap to approve.

<p>
  <img src="https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/packages/simplex-chat-mcp/images/session.png" alt="The group of a Claude Code session in the SimpleX app: a prompt, Claude's answer, and its requests to run commands, which one tap approves" width="320">
</p>

## Run Claude Code from the app

You need Node.js 22 or later, Claude Code, and the [SimpleX Chat app](https://simplex.chat/downloads/) 7.1 or later.

1. Install the plugin in Claude Code:

   ```
   /plugin marketplace add https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/.claude-plugin/marketplace.json
   /plugin install simplex@simplex-chat
   ```

2. Start Claude Code with the SimpleX channel:

   ```
   claude --dangerously-load-development-channels plugin:simplex@simplex-chat
   ```

   Scan the QR code with the SimpleX app and tap Join. The group "SimpleX MCP" appears.

3. Each Claude Code session gets its own group, such as "1 · my-project". Join it from the invitation in "SimpleX MCP".

4. Send prompts in the session's group, and Claude answers there. Prompts you type in the terminal appear there too. Before Claude acts in other chats or uses a tool such as Bash, it asks in the group, and one tap approves.

## Safety

- Everyone in "SimpleX MCP" can join every session's group as an admin. Invite there only people you trust with all your sessions.
- In a session's group, rights follow the member's role. Admins and owners send prompts and the group's commands, and approve Claude's actions. Members and moderators send prompts. Observers only read. Claude posts these rights when someone joins or their role changes.
- Claude Code never runs a command from the group, even one that starts with `!` or `/`. Claude receives each prompt as text.
- `all` approves every message to one chat; `/revoke` ends it. `/receive CHAT N all` lets the agent receive that file and later files from one chat on the agent's servers; `/receive CHAT off` ends it.
- Claude sends files to the group only while the group allows "Files and media".
- Any program running as you can replace "SimpleX MCP" and its owners, without their approval, with the plugin's `reset-owners` command.
- The agent runs on your computer and has no address in SimpleX Network, so nobody can contact it first.

## Only Claude Code, for now

The agent is tested with Claude Code. Tell us which other apps and models you want to use in [an issue](https://github.com/simplex-chat/simplex-chat/issues), or contribute.

## Reference

[REFERENCE.md](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-mcp/REFERENCE.md) covers the plugin's options, recovery and security. It also explains shared settings for several machines or containers, and running the agent as an MCP server without the channel.

What comes next: [Roadmap](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-mcp/ROADMAP.md).

## Own a part of SimpleX

Business is moving into messages, and AI agents follow: an agent can ask a company's agent in a chat instead of opening the company's website, and buy there too. These chats hold plans, purchases and accounts, so with AI everyone feels the need for private and secure communication.

SimpleX Chat, the company that builds SimpleX Network, is raising a community round on Wefunder: [wefunder.com/simplex.chat](https://wefunder.com/simplex.chat?utm_source=mcp)
