# SimpleX Chat bot API

- [Why create a bot](#why-create-a-bot)
- [How to create a bot](#how-to-create-a-bot)
- [Sending commands](#sending-commands)
- [Processing events](#processing-events)
- [Security considerations](#security-considerations)
- [Useful bots](#useful-bots)
- [API types reference](./api/README.md) (another page)


## Why create a bot

You can implement SimpleX Chat for these and many other scenarios:
- customer support - both as a single- and a multi-agent support chat (using SimpleX Chat [business address]() feature),
- information search and retrieval bots, with or without LLM integration,
- broadcast bot, when messages from your trusted users are forwarded to all connected contacts - e.g., see our SimpleX Status bot in the app ([source code](../apps/simplex-broadcast-bot/)),
- feedback bot, when messages from connected contacts are forwarded to a preset list of your trusted users,
- P2P trading bots, connecting buyers and sellers,
- etc.

We will share all useful bots you create in the bottom of this page - please submit a PR to add it.


## How to create a bot

[SimpleX Chat CLI](../docs/CLI.md) can be run as a local WebSockets server on any port:

```bash
simplex-chat -p 5225
```

To see all supported parameters:

```bash
simplex-chat -h
```

Your bot must run as a standalone process connecting to CLI via WebSockets on the chosen port. See [Security considerations](#security-considerations) about connecting your bot process to CLI.

All communication between your bot process and CLI happens via JSON-encoded WebSocket text messages.

To connect to other SimpleX Chat users and to send messages the bot must send commands to CLI. The command WebSocket message contains correlation ID and commands as strings.

CLI will respond to command messages with command processing results. The response WebSocket message contains the same correlation ID as was sent in the command and JSON-encoded response record.

See [Sending commands](#sending-commands) about message formats and types for commands and responses.

CLI will also send chat events to your bot process. These events represent information about connecting SimpleX Chat users, received messages, etc.

See [Processing events](#processing-events) about event message format and types.

In most cases, the bot needs to have a pre-configured user profile and SimpleX address, configured to automatically accept incoming contact requests from all users. It is simpler to do it manually via desktop client and then use this chat database with your bot. But it can also be done programmatically when bot starts.

In the simplest case, your bot must process [NewChatItems](./api/EVENTS.md#newchatitems) event to receive messages from connected users and use [APISendMessages](./api/COMMANDS.md#apisendmessages) command to respond to them.


## Sending commands

CLI WebSockets API allows to:

- send and receive messages and files.
- create and change user profile - you also can do it manually, via SimpleX Chat desktop app or CLI.
- create and accept invitations or connect with the contacts.
- create and manage long-term user address, accepting connection requests automatically or via code.
- create, join and manage group.

Each command your bot sends to CLI should have this JSON format:

```json
{
  "corrId": "<any unique string>",
  "cmd": "<command string>"
}
```

You can use sequential numbers, UUIDs or some other unique strings in `corrId` field.

Command strings are the same commands you can can see in `Settings / Developer tools / Chat Console` of mobile and desktop apps. You can test these commands via SimpleX Chat CLI.

When command is processed, CLI will send a response as a WebSockets message in this format:

```json
{
  "corrId": "<corrId sent with a command>",
  "resp": {
    "type": "<response record tag>",
    "other response fields": null
  }
}
```

`corrId` will be the same as you used in commands. Your bot must maintain the map of pending commands responses, and can implement an internal callback or async API for convenience. See our [TypeScript bot library](../packages/simplex-chat-client/typescript/README.md) for an example. TypeScript library sends commands sequentially, via a queue, but your bot can send commands concurrently.


`resp` field is a command-specific response in JSON format. All command responses form a discriminated union with `type` field as a tag.

See [API Commands and Responses](./api/COMMANDS.md) reference about specific command strings and JSON types for command responses. As CLI has the same API as used by mobile and desktop apps, it supports other commands not included in the reference.


## Processing events

Chat event is a WebSocket message in this format:

```json
{
 "resp": {
    "type": "<event record tag>",
    "other event fields": null
  }
}
```

While it uses the same `resp` property as responses for backward compatibility, the event type is a different discriminated union. Some record types are used both as a command response and an event. The most important example is `NewChatItems` that can be sent both as a response to [APISendMessages](./api/COMMANDS.md#apisendmessages) with correlation ID (when message is scheduled for delivery) and as [events](./api/EVENTS.md#newchatitems) when messages are received.

See [API Events](./api/EVENTS.md) reference about specific JSON types for chat events. CLI can send other events not included in the reference.

*Please note*: Your bot must allow and ignore all events it does not process, it should not fail when it encounters undocumented event types. Your bot JSON parser must allow additional properties in all types, and must allow and ignore records with unknown union tags and unknown enum strings.


## Security considerations

WebSockets API of SimpleX Chat CLI does not support any authentication. CLI binds only to localhost to prevent accidental access from public network, in case you did not close this port in firewall. The messages in WebSocket API are not encrypted in any way, and must not be sent via the public networks.

It is usually simpler to run your bot process on the same machine where you run SimpleX Chat CLI, and to close CLI port in firewall. That makes connection between your bot and CLI secure. It also simplifies sending and receiving files via bot, as they are stored on the file system accessible to SimpleX Chat CLI.

If you have to run your bot on another machine, you need to secure access to bot CLI via any web proxy that supports WebSockets, e.g. Caddy or Nginx. You must configure TLS termination in the proxy and connect CLI process from bot via a secure TLS connection. If you connect to bot via a public network, you also must configure HTTP basic auth with sufficiently large credentials (we recommend 256 bits) to prevent unauthorized access. You can validate TLS security of your proxy via a free test at [SSLLabs.com](https://www.ssllabs.com/ssltest/). You can also configure firewall on the machine where you run bot process to only allow connections from the IP address(es) where you run your bot.


## Useful bots

TODO
