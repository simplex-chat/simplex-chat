# SimpleX Chat bot API

- [Why create a bot](#why-create-a-bot)
- [What is SimpleX bot](#what-is-simplex-bot)
- [How to configure bot profile](#how-to-configure-bot-profile)
- [How to create a bot](#how-to-create-a-bot)
- [Sending commands](#sending-commands)
- [Processing events](#processing-events)
- [Security considerations](#security-considerations)
- [Useful bots](#useful-bots)
- [API types reference](./api/README.md) (another page)


## Why create a bot

You can implement SimpleX Chat for these and many other scenarios:
- customer support - both as a single- and a multi-agent support chat (using SimpleX Chat [business address](https://simplex.chat/docs/business.html) feature),
- information search and retrieval bots, with or without LLM integration,
- moderation bots, to moderate your group and communities.
- broadcast bot, when messages from your trusted users are forwarded to all connected contacts - e.g., see our SimpleX Status bot in the app ([source code](../apps/simplex-broadcast-bot/)),
- feedback bot, when messages from connected contacts are forwarded to a preset list of your trusted users,
- P2P trading bots, connecting buyers and sellers,
- etc.

We will share all useful bots you create in the bottom of this page - please submit a PR to add it.


## What is SimpleX bot

SimpleX bot is a participant of SimpleX network. Theoretically, bot can do everything that a usual SimpleX Chat user can do – send and receive messages and files, connect to addresses and join groups, etc. But to be useful, a bot should distinguish itself as a bot, and to provide an interface for the users to interact with it.

## How to configure bot profile

Starting from v6.4.3, SimpleX Chat apps support bot configuration to distinguish bots, to highlight commands in messages, and to show command menus.

### Set up bot profile

To distinguish SimpleX user profile as a bot, set its `peerType` property to `"bot"`. It can be done in one of these ways:
- using CLI options `--create-bot-display-name` and `create-bot-allow-files` when first starting CLI to create bot profile,
- using command `/create bot [files=on] <name>[ <bio>]` (if name contains spaces, it must be in single quotes), when creating additional bot profiles in the same database,
- by configuring bot commands that the users will see in the UI when they type `/` character or tap `//` button with `/set bot commands ...` CLI command (see syntax below),
- by using [APIUpdateProfile](./api/COMMANDS.md#apiupdateprofile) bot command to set `peerType` and configure bot commands at the same time.

### Configure bot commands

Bot commands are messages that start from `/` character. Normally, they would consist of lowercase latin letters, but commands can use any letters, digits and underscores. Commands can have parameters.

All commands in messages will be highlighted in the chats with the bot, and when users tap them, they will be instantly sent. If the message has a single line and starts from `/` character, the whole message will be highlighted. Otherwise, if command is included as part of the message, it will be highlighted until the first space after `/` character: e.g., `/list` command in Directory service shows user's groups.

*Please note*: commands in messages will be highlighted based purely on `/` character, regardless of whether they are supported by the bot or included in bot configuration. It allows bots to have "hidden" commands that bot would support, but that won't be shown in the menu. But it may also lead to mistakes if bot sends incorrect commands in the instructions to the users.

Bots can also send highlighted commands with parameters. To do that, bots should surround both command and its parameters in single quotes: e.g., `/'role 2'`. Quotes won't show in the apps UI, and if the user taps this command, it will be sent as `/role 2`.

Configured bot commands will be be offered to the users as a menu, and for quick lookup as the user types.

Bot commands configuration is a property in `preferences` object in bot profile received by the user. These preferences can be configured both on the bot user profile level, to offer the same commands to all connected users, and as overrides for specific contacts, to offer different commands to different bot contacts.

Configuring commands in bot user can be done either with [APIUpdateProfile](./api/COMMANDS.md#apiupdateprofile) or with `/set bot commands` CLI command:

```
/set bot commands <commands>
```

where:

```
commands = <commandOrMenu>[,<commandOrMenu>...]
commandOrMenu = command | menu
command = '<label>':/'<keyword>[ <params>]'
menu = '<label>':{<commands>}
```

This syntax allows creating nested menus of commands with and without parameters. You must enclose parameter names with the characters `<` and `>`. Currently, users have to edit command templates to set actual parameters, but in the future there will be UI support to fill in parameters based on this syntax. For example, some of SimpleX Directory service commands could be configured with this command:

```
/set bot commands 'How to use bot':/help,'Show your groups':/list,'Your group settings':{'Set default role':/'role <ID>','Set anti-spam filter':/'filter <ID>'}
```

Configuring commands for specific contacts can be done with [APISetContactPrefs](./api/COMMANDS.md#apisetcontactprefs) command.

### Bots and business addresses

A useful scenario would be when bot is used to accept requests to bot's business address, so that a new business chat is created for every connecting user, and then invites other people from the business, as appropriate.

Business chat is a special group chat under the hood, but the connected customer sees business avatar, and all users that bot would add to the group later see customer' avatar. This chat will inherits preferences from the bot profile, so if you want to allow customers to send files, you need to allow them in the bot. And if bot has any commands configured, they will also be available to customers in the menu.

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

*Please note*: CLI uses network connection for most API commands. Command network usage is included in the reference:
- "no" - command doesn't use network,
- "interactive" - all or some network requests will complete before command response is sent to the bot,
- "background" - command response will be sent to the bot before scheduled network requests are sent.

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

WebSockets API of SimpleX Chat CLI does not support any authentication. CLI binds only to localhost to prevent accidental access from public network, in case you did not close this port in firewall. The messages in WebSocket API are not encrypted in any way, and must not be sent via public networks.

It is usually simpler to run your bot process on the same machine where you run SimpleX Chat CLI, and to close CLI port in firewall. That makes connection between your bot and CLI secure. It also simplifies sending and receiving files via bot, as they are stored on the file system accessible to SimpleX Chat CLI.

If you have to run your bot on another machine, you need to secure access to bot CLI via any web proxy that supports WebSockets, e.g. Caddy or Nginx. You must configure TLS termination in the proxy and connect CLI process from bot via a secure TLS connection. If you connect to bot via a public network, you also must configure HTTP basic auth to prevent unauthorized access. You can validate TLS security of your proxy via a free test at [SSLLabs.com](https://www.ssllabs.com/ssltest/). You can also configure firewall on the machine where you run SimpleX CLI to only allow connections from the IP address of your bot.


## Useful bots

- [Broadcast bot](../apps/simplex-broadcast-bot/) (Haskell) - we use it to send [status and release updates](https://status.simplex.chat/status/public).
- [Moderation bot](https://github.com/NCalex42/simplex-bot) (Java)
- [Matterbridge bot](https://github.com/UnkwUsr/matterbridge-simplex) (JavaScript)
