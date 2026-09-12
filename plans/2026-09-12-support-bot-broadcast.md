# Support bot broadcast

A feed broadcast sent from the support bot's team group.

## Decisions

- Trigger: `/broadcast <text>` in the team group, accepted only from contacts listed in `--broadcasters`.
- Audience: unrestricted. Every customer group and every direct contact receives it, including the Grok contact and team member DMs.
- Content: text only.
- Command: the bot sends the CLI command `/feed <json text>` through `sendChatCmd`. `APISendFeedMessage` stays undocumented and no lookup command is added until the feed API stabilises.
- Reporting: the bot acknowledges on send, and posts again when the feed item reaches `sndSent` `complete` or an error status.

## Bot

`apps/simplex-support-bot/src/config.ts`:

- `--broadcasters <list>`: comma-separated `ID:name` pairs, parsed with `parseIdName`
- `Config.broadcasters: IdName[]`

`apps/simplex-support-bot/src/index.ts`:

- broadcasters are validated at startup as team members are: read the contact by id, then compare the display name
- `teamGroupPreferences.commands` gains
  `{type: "command", keyword: "broadcast", label: "Broadcast to all chats", params: "text"}`
- the `events` map gains `chatItemsStatusesUpdated`

`apps/simplex-support-bot/src/bot.ts`:

- `processTeamGroupMessage` routes the `broadcast` keyword to `handleBroadcastCommand` and keeps the `join` route
- `handleBroadcastCommand`:
  - the sender's `memberContactId` must be in `config.broadcasters`, otherwise the bot replies in the team group and stops
  - the text is the trimmed message with the `/broadcast` prefix and one following whitespace character removed; `ciBotCommand`'s `params` stops at the first newline, so it is only used for the keyword
  - empty text is an error reply
  - the command sent is `"/feed " + JSON.stringify(text)`; `msgTextP` decodes the JSON string, so newlines and quotes survive
  - the response must be `newChatItems`; its first item id is held in a pending map, and the bot replies that the broadcast is queued
- `onChatItemsStatusesUpdated` matches items whose chat info type is `feed` and whose id is in the pending map:
  - `sndSent` with `sndProgress` `complete` replies that the broadcast is delivered, and drops the entry
  - `sndError` replies with the error text, and drops the entry
- a restart drops pending reports; delivery continues in the feed workers

## Status

Written: config, startup validation, command routing, the broadcast handler, the status handler, the README section, and nine tests in `bot.test.ts` with a `feed` chat item factory and a `/feed` response in the mock API.

Not run: `tsc` and `vitest`, since the bot's `node_modules` is absent and installing it was declined.

## Not changed

- The feed audience. Restricting a broadcast to customer groups would need a scope on the feed itself.
- The API docs, the TypeScript client and the Python client.
- `updateChatSettings`. It still rejects `*` and `%`, so `/feed drop %` is not accepted.
