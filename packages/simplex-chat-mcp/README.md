# SimpleX MCP server

Your Claude Code can now chat on SimpleX.

SimpleX Network is the only messaging network without any user identifiers - no phone numbers, usernames, account IDs or even random IDs. Your agent needs no account, no platform controls its identity, and nobody, not even the servers, can see who it talks to. The app was downloaded more than 3M times, and over 600,000 people use it every month.

With this package, Claude Code can join channels and groups, talk to bots, and even receive your prompts via the SimpleX app. It asks you on your phone before it connects or sends - one tap to approve.

<p>
  <img src="https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/packages/simplex-chat-mcp/images/approval.png" alt="The agent asks in its chat before it sends a message; one tap approves it" width="320">
  <img src="https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/packages/simplex-chat-mcp/images/prompts.png" alt="Prompting the agent from the SimpleX app" width="320">
</p>

## Try it

You need Node.js 22 or later, Claude Code and the [SimpleX Chat app](https://simplex.chat/downloads/).

1. In Claude Code, add the plugin:

   ```
   /plugin marketplace add https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/.claude-plugin/marketplace.json
   /plugin install simplex@simplex-chat
   ```

2. Start a new session. Scan the QR code it shows with the SimpleX app and tap Join. The group "SimpleX MCP" appears: your sessions ask there before they act. To see the link again, run `/simplex:link` in Claude Code, or `npx -y @simplex-chat/mcp link` in a terminal for a QR code you can scan.

3. Ask:

   ```
   Search the SimpleX Directory for security groups: https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok
   ```

   Approve its card in "SimpleX MCP" with one tap.

Each Claude Code session has its own SimpleX profile, "SimpleX MCP - <session name>"; name a session with `claude -n NAME` or `/rename`. Change the group name, the approval mode and the expiry in `/plugin` > Installed > simplex > Configure options, or in `/config`.

## Prompt your agent from the app

Give Claude Code your SimpleX address:

```
Connect to me as yourself at <your SimpleX address>, with the message:
"I'm your agent, connected as you asked in Claude Code. I'll act on your prompts here, except security-sensitive ones."
Then take my prompts from that chat and answer there: wait for my messages with receive (wait 3600),
answer each one in that chat, and call receive again after every answer, until I say stop.
```

Approve the connection in "SimpleX MCP", and accept the request in the app if it asks. On the card of the agent's first answer, tap `all`: its later answers to you need no approval.

## Safety

- The agent asks in "SimpleX MCP" before it connects, sends or leaves; you approve or reject.
- `all` approves every message to one chat; `/revoke` ends it.
- Messages in your prompt chat can become prompts for your Claude Code session: use a chat where only you write.
- The agent runs on your computer and has no address in SimpleX Network, so nobody can contact it first.

## Only Claude Code, for now

The agent is tested with Claude Code. Tell us which other apps and models you want to use in [an issue](https://github.com/simplex-chat/simplex-chat/issues), or contribute.

## Reference

Options, the Main group and its owners, owner commands, files, recovery and security: [REFERENCE.md](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-mcp/REFERENCE.md).

## Own a part of SimpleX

Business is moving into messages, and AI agents follow: an agent can ask a company's agent in a chat instead of opening the company's website, and buy there too. These chats hold plans, purchases and accounts, so with AI everyone feels the need for private and secure communication.

SimpleX Chat, the company that builds SimpleX Network, is raising a community round on Wefunder: [wefunder.com/simplex.chat](https://wefunder.com/simplex.chat?utm_source=mcp)
