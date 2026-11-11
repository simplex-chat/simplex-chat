# SimpleX MCP server

Your Claude Code can now chat on SimpleX.

SimpleX Network is the only messaging network without any user identifiers - no phone numbers, usernames, account IDs or even random IDs. Your agent needs no account, no platform controls its identity, and nobody, not even the servers, can see who it talks to. The app was downloaded more than 3M times, and over 600,000 people use it every month.

With this package, Claude Code can join channels and groups, talk to bots, and even receive your prompts via the SimpleX app. It asks you on your phone before it connects or sends - one tap to approve.

<p>
  <img src="https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/packages/simplex-chat-mcp/images/approval.png" alt="The agent asks in its chat before it sends a message; one tap approves it" width="320">
  <img src="https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/packages/simplex-chat-mcp/images/prompts.png" alt="Prompting the agent from the SimpleX app" width="320">
</p>

## Run Claude Code from the app

You need Node.js 22 or later, Claude Code and the [SimpleX Chat app](https://simplex.chat/downloads/) 7.1 or later.

1. Install the plugin in Claude Code:

   ```
   /plugin marketplace add https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/.claude-plugin/marketplace.json
   /plugin install simplex@simplex-chat
   ```

2. Start Claude Code with the SimpleX channel:

   ```
   claude --dangerously-load-development-channels plugin:simplex@simplex-chat
   ```

   Scan the QR code it shows with the SimpleX app, and tap Join. The group "SimpleX MCP" appears.

3. Each Claude Code session gets its own group, such as "1 · my-project", and you get its invitation in "SimpleX MCP". Join it.

4. Send a prompt in the session's group. Claude answers there. It asks there before it connects, sends, downloads a file or leaves a chat, and before it uses a tool such as Bash: one tap approves. Your prompts typed in the terminal appear in the group too.

## Safety

- Everyone in "SimpleX MCP" can join each session's group as an admin, prompt the session and approve its actions and tool use: invite there only people you trust with all your sessions. In a session's group, rights follow the role: admins and owners prompt the session, send its `/` commands, and approve its actions and tool use; moderators and members prompt it; observers only read. Claude Code runs no command from a prompt, also one that starts with `!`: Claude gets the text, and its tool use follows Claude Code's permissions. Claude posts a member's rights at their join and at each change of their role; the post for a member who can send names the command that makes them an observer. Claude's files reach the group at once, while the group's preference "Files and media" allows them.
- `all` approves every message to one chat; `/revoke` ends it. `/receive CHAT N all` lets the agent receive that file and later files from one chat on the agent's servers; `/receive CHAT off` ends it.
- `npx -y @simplex-chat/mcp@VERSION reset-owners` in a terminal, VERSION being the plugin's version, replaces "SimpleX MCP" and its owners at once, without their approval: any program running as you on your computer can do it.
- The agent runs on your computer and has no address in SimpleX Network, so nobody can contact it first.

## Only Claude Code, for now

The agent is tested with Claude Code. Tell us which other apps and models you want to use in [an issue](https://github.com/simplex-chat/simplex-chat/issues), or contribute.

## Reference

Settings shared by several machines or containers, and the agent as an MCP server without the channel, by `serve` and `register`, are in [REFERENCE.md](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-mcp/REFERENCE.md), with the plugin's options, recovery and security.

## Own a part of SimpleX

Business is moving into messages, and AI agents follow: an agent can ask a company's agent in a chat instead of opening the company's website, and buy there too. These chats hold plans, purchases and accounts, so with AI everyone feels the need for private and secure communication.

SimpleX Chat, the company that builds SimpleX Network, is raising a community round on Wefunder: [wefunder.com/simplex.chat](https://wefunder.com/simplex.chat?utm_source=mcp)
