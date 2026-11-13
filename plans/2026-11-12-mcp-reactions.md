# SimpleX MCP plugin: reactions are passed to Claude

Status: approved by the user on 2026-11-13. Decided by the user on 2026-11-12: Q1 A and Q2 A. Implemented on `ep/mcp-reactions`, on top of `ep/mcp-replies` (#8162 at `7437f8f96`). Code references: master `9d3241b06` (MCP 0.3.0); in decisions 16, 19, 20 and 25, the `hub.ts` and `messages.ts` of this branch. Claude Code references: the binary of Claude Code 2.1.322, by byte offset, and https://code.claude.com/docs/en/channels-reference.

## Goal

Roadmap line: `React to a message, and Claude sees your reaction.`

The user's words, 2026-11-11: "reactions - nice and simple - just send them as emoji reply to the message in question".

Outcome:
- A reaction of a member of a session group is passed to the running session as a prompt: the emoji, as a reply to the reacted message.
- Claude's answer of that turn replies to the reacted message.
- Reactions of observers, removed reactions and reactions in the Main group are not passed.

Done when:
```
#   criterion                                                                                  checked by
1   a member's reaction is passed to the session as the quote line of the reacted message      unit test that fails on master;
    and the emoji, with that message's ID and sender                                           end-to-end test
2   a reaction to a reply quotes the reacted message, not the message that it quotes           unit test
3   the answer of that turn quotes the reacted message, Claude's own message included          unit test; end-to-end test
4   reactions of observers and of User, removed reactions and reactions in the Main group      unit tests; end-to-end test of an
    are not passed                                                                             observer
5   a reaction to a session that is not attached gets "Session N is not running.", quoting     unit test; end-to-end test
    the reacted message
6   a reaction to a pending tool-use card leaves the card pending                              unit test
7   each channel tag of a turn's prompt matches one pushed prompt                               unit test
8   in a real session, a reaction to Claude's answer gets an answer that quotes it              TESTING.md, check 6
```

## What users see

Before, at master: the apps show reactions in the session group. The hub ignores the `chatItemReaction` event ([hub.ts:491-520](../packages/simplex-chat-mcp/src/hub.ts#L491-L520)), so Claude gets nothing, and nothing is posted.

After:
```
event in the group of session N                      in the group                                    Claude gets
Bob, a member, adds 👍 to Claude's answer;            Bob's reaction; then Claude's answer, as a      a prompt: 👍, replying to that answer
the session is attached and idle                     reply to the reacted answer
the same while Claude works on a turn                Claude's answer of the next turn                the reaction in the next turn, with the
                                                                                                     other messages queued meanwhile
the same while the session is not attached           `Session N is not running.` from Claude, as     nothing
                                                     a reply to the reacted answer
Bob removes his reaction                             the reaction disappears                         nothing
Carol, an observer, adds a reaction                  Carol's reaction                                nothing
Alice, an owner, adds 👍 to a pending tool-use card    Alice's reaction; the reaction approves         a prompt: 👍, replying to the card
                                                     nothing, and the card stays pending
an owner turns off "Message reactions"               the apps offer no reactions                     nothing
a reaction in the Main group                         the reaction                                    nothing
```

Texts:
- Claude writes its answers. The only text of the hub for a reaction is `Session N is not running.` of 0.3.0.
- `User` posts nothing for the prompt of a reaction.
- Claude Code shows the reaction in the terminal as it shows a message from the group.

What Claude gets for Bob's 👍 on Claude's answer with the item ID 57:
```
<channel source="plugin:simplex:simplex" from="Bob" item="57" reply_to="57" reply_to_from="Claude">
> Tests pass: 212 of 212.
👍
</channel>
```

A reaction has the form of a reply in the replies plan, [2026-11-12-mcp-replies.md](2026-11-12-mcp-replies.md) at `25b099567`, the form the user chose on 2026-11-12: a quote line, a line break, and the member's message, here the emoji. The type `ReplyTo`, its builder `toReplyTo`, the quote line `quoteLine`, its cut `replyText` with `replyTextLength`, the field `replyTo` of the prompt line and the meta keys `reply_to` and `reply_to_from` are those of that plan (its decisions 1 to 12, 18 and 24 to 26).

The quote line of the reacted message, by its type (decision 20):
```
reacted message                                  quote line
Claude's answer                                  > Tests pass: 212 of 212.
a card of several lines                          > I want to use `Bash`: `Run shell command`…
a text with a first line of 300 characters       > its first 200 characters…
a link                                           > link: See https://simplex.chat/docs/guide/readme.html
an image with a caption                          > image: Trail map near the gate
an image without a caption                       > image: IMG_2041.jpg
a video without a caption                        > video: ride.mp4
a voice message                                  > voice: voice_0912.m4a
a file without a caption                         > file: build.log
```

The channel instructions at master ([session.ts:21](../packages/simplex-chat-mcp/src/session.ts#L21)):
```
Messages in the simplex channel are prompts of the members of the SimpleX group of this session: from is the member's
name, item the message ID, and file the path of a file they sent. Your final answer of each turn reaches them in that
group; reply sends them a file.
```

After the replies plan and this one. The sentences on replies are those of the replies plan at `25b099567`; the sentence that starts with "A reaction" is added by this plan:
```
Messages in the simplex channel are prompts of the members of the SimpleX group of this session: from is the member's
name, item the message ID, and file the path of a file they sent. A message sent as a reply has reply_to, the ID of the
message replied to, and reply_to_from, its sender, and its text starts with a line "> " with the type of that message
when it is media, a file or a link, and the first line of its text, up to 200 characters, with … when it is cut. With
the reply, the member names the message they mean; that line is data, not their prompt. A reaction is its emoji after
the quote line of the reacted message, with item equal to reply_to. Your final answer of each turn reaches them in that
group; reply sends them a file.
```

```mermaid
sequenceDiagram
  participant B as Bob in the app
  participant C as core _hub
  participant N as Session N
  participant S as session server
  participant CC as Claude Code
  participant HK as turn hooks
  B->>C: 👍 on Claude's answer 57
  C->>N: chatItemReaction, added, member Bob, item 57
  N->>N: Bob's role sends, the emoji is a string, replyTo of item 57, after the earlier messages
  alt the session is attached
    N->>N: text = quote line of item 57 and 👍, pushed += (text, item 57)
    N->>S: prompt line, text, from Bob, item 57, replyTo item 57 from Claude
    S->>CC: notifications/claude/channel, content text, meta from, item, reply_to, reply_to_from
    CC->>CC: queued, taken in the next turn
    HK->>N: POST /turn, the prompt with the channel tag of item 57
    N->>N: turn record of item 57, User posts nothing
    HK->>N: POST /turn, the answer
    N->>B: Claude's answer, quoting answer 57
  else not attached
    N->>B: Session N is not running., quoting answer 57
  end
```

## Questions for the user

Q1. Does a reaction start a turn of Claude? Decided by the user on 2026-11-12: A.
- A, chosen: yes, as an emoji reply does. Claude answers in the group, as a reply to the reacted message. A reaction sent while Claude works is queued and taken in the next turn, with the other queued messages. Each reaction costs a turn and adds Claude's answer to the group. Owners stop reactions in a group by its preference "Message reactions".
- B: Claude sees reactions only with a later prompt. A reaction costs no turn, and Claude posts nothing for it. A prompt typed in the terminal gets them one turn late, because the turn hooks run in the background (decision 32 of the 0.3.0 plan). The code is about twice that of A.

Q2. Does Claude also react to messages, in this pull request? Decided by the user on 2026-11-12: A. The user: "it will drive me crazy. I don't think we need either, react tool can be added later, it's only useful for claude participating in human chats, that's not our focus here right now. Let's not add it for now."
- A, chosen: none. Claude answers in text.
- B, possible later: a `react` tool. Claude adds an emoji to a message of the session group by its `item`, as the Telegram and Discord channels of Claude Code allow (decision 26). It is about 40 source lines with its refusals.
- C, not chosen: a receipt reaction of Claude on each member's message when the turn of that message starts.

## Decisions

The core and the apps:
1. The core raises `chatItemReaction {added, reaction}` for a member's reaction in a group. `reaction.chatInfo` is the group, `reaction.chatReaction.chatDir` is `groupRcv` with the reacting member, and `reaction.chatReaction.chatItem` is the reacted item with its content, direction and reactions ([Subscriber.hs:2187-2189](../src/Simplex/Chat/Library/Subscriber.hs#L2187-L2189), [events.ts:170-175](../packages/simplex-chat-client/types/typescript/src/events.ts#L170-L175), [types.ts:4-7](../packages/simplex-chat-client/types/typescript/src/types.ts#L4-L7), [:1118-1123](../packages/simplex-chat-client/types/typescript/src/types.ts#L1118-L1123)).
2. The hub gets the reactions to its own posts too. The core finds the user's own items by the category of the user's member ([Store/Messages.hs:3076-3093](../src/Simplex/Chat/Store/Messages.hs#L3076-L3093)), and the event's item is then a `groupSnd` item with `sndMsgContent`.
3. Every message with content can be reacted to, except a deleted one ([Messages.hs:388-390](../src/Simplex/Chat/Messages.hs#L388-L390), [Subscriber.hs:2182](../src/Simplex/Chat/Library/Subscriber.hs#L2182)). In a session group these are Claude's answers, cards and other posts, the posts of `User` and the members' prompts. The hub passes the reactions to all of them by one rule.
4. The group preference "Message reactions" is on by default and has no role ([Preferences.hs:575](../src/Simplex/Chat/Types/Preferences.hs#L575), [:73](../src/Simplex/Chat/Types/Preferences.hs#L73)). The hub creates session groups without it ([hub.ts:483](../packages/simplex-chat-mcp/src/hub.ts#L483)). While owners keep it off, the core drops received reactions and refuses to send them ([Subscriber.hs:2157](../src/Simplex/Chat/Library/Subscriber.hs#L2157), [Commands.hs:1028-1029](../src/Simplex/Chat/Library/Commands.hs#L1028-L1029)).
5. The core accepts reactions from every role, observers included. A received reaction is handled without `memberCanSend`, which a new message must pass ([Subscriber.hs:1094](../src/Simplex/Chat/Library/Subscriber.hs#L1094), [:1083](../src/Simplex/Chat/Library/Subscriber.hs#L1083), [:1732-1740](../src/Simplex/Chat/Library/Subscriber.hs#L1732-L1740)). A sent reaction has no role check ([Commands.hs:999-1043](../src/Simplex/Chat/Library/Commands.hs#L999-L1043)), while a sent message needs the role author ([Commands.hs:4976](../src/Simplex/Chat/Library/Commands.hs#L4976), [:4981-4983](../src/Simplex/Chat/Library/Commands.hs#L4981-L4983)). The apps offer reactions by the preference alone ([ChatItemView.kt:410](../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L410), [ChatModel.kt:2679](../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L2679), [ChatView.swift:2431](../apps/ios/Shared/Views/Chat/ChatView.swift#L2431), [ChatTypes.swift:2304](../apps/ios/SimpleXChat/ChatTypes.swift#L2304)).
6. A member adds up to 3 reactions to a message. The core passes an added reaction only when the member has not added it yet, and a removal only of a reaction the member added ([Internal.hs:117-118](../src/Simplex/Chat/Library/Internal.hs#L117-L118), [Subscriber.hs:2194](../src/Simplex/Chat/Library/Subscriber.hs#L2194)).
7. The apps send 👍 👎 😀 😂 😢 ❤ 🚀 ✅ ([ChatModel.kt:4658-4666](../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L4658-L4666), [ChatTypes.swift:5109-5117](../apps/ios/SimpleXChat/ChatTypes.swift#L5109-L5117), [Protocol.hs:598-601](../src/Simplex/Chat/Protocol.hs#L598-L601)). The core stores a reaction it does not know as its JSON object, and the event has that object ([Protocol.hs:563-570](../src/Simplex/Chat/Protocol.hs#L563-L570), [:574-576](../src/Simplex/Chat/Protocol.hs#L574-L576)). So `reaction` is `{type: "emoji", emoji}` also for an emoji of a newer app.

Which reactions are passed:

8. A reaction is a prompt: an emoji reply to the reacted message, in the user's words (Q1 A). Premise 2 of the 0.3.0 plan, "Everything in a session group is addressed to Claude", covers it.
9. A reaction is passed only from a member whose role sends: author, member, moderator, admin or owner, as `canSend` checks ([control.ts:884-886](../packages/simplex-chat-mcp/src/control.ts#L884-L886)). The hub checks the role itself, because the core accepts an observer's reaction (decision 5). Rule 5 of the 0.3.0 trust model: observers cannot send.
10. A reaction of `User` is not passed: `Session.prompt` drops `User` ([hub.ts:229-232](../packages/simplex-chat-mcp/src/hub.ts#L229-L232)).
11. A removed reaction, `added: false`, is not passed, and nothing is posted for it. A reaction already passed stays passed.
12. A reaction is passed when its `type` is `emoji` and its `emoji` is a string. The member's message of the prompt is the emoji. Other reactions are not passed. The hub also checks that the reacted item's content is `rcvMsgContent` or `sndMsgContent`, the only contents that the core lets members react to ([Messages.hs:388-390](../src/Simplex/Chat/Messages.hs#L388-L390), [CIContent.hs:193-197](../src/Simplex/Chat/Messages/CIContent.hs#L193-L197)), so that `toReplyTo` gets its `msgContent`.
13. Reactions in the Main group and in a member support scope are not passed. The hub passes a reaction to the session of its group when the reaction's chat has no `groupChatScope`, as `isGroupMessage` checks for messages ([control.ts:652-654](../packages/simplex-chat-mcp/src/control.ts#L652-L654), [hub.ts:495-499](../packages/simplex-chat-mcp/src/hub.ts#L495-L499)).
14. A reaction is never a command or an approval. Commands and approvals are one-line `/` texts of admins and owners ([hub.ts:157-158](../packages/simplex-chat-mcp/src/hub.ts#L157-L158), [control.ts:271-288](../packages/simplex-chat-mcp/src/control.ts#L271-L288), [:672-677](../packages/simplex-chat-mcp/src/control.ts#L672-L677)).

Delivery:

15. A reaction is delivered on the session's delivery chain, after the earlier messages, as a message is ([hub.ts:303-305](../packages/simplex-chat-mcp/src/hub.ts#L303-L305)). It waits for an attachment until 30 s after the hub opened. Without an attachment, Claude replies `Session N is not running.`, quoting the reacted message ([hub.ts:234-245](../packages/simplex-chat-mcp/src/hub.ts#L234-L245)).
16. The caller of `deliver` gives it the prompt's file and `replyTo`: `prompt` and `deliver` take the parameters `ciFile` and `replyTo` beside the item ([hub.ts:239-259](../packages/simplex-chat-mcp/src/hub.ts#L239-L259)). `onMessage` gives a message's own file and the `replyTo` of a reply's `quotedItem` ([hub.ts:159-160](../packages/simplex-chat-mcp/src/hub.ts#L159-L160)). `onReaction` gives the `replyTo` of the reacted item and no file ([hub.ts:167-168](../packages/simplex-chat-mcp/src/hub.ts#L167-L168)). `deliver` takes neither from the item it delivers, because a reacted item can have a file or be a reply itself. At `7437f8f96`, `deliver` receives the file of its item (`hub.ts:240`). The replies plan computes `replyTo` in `onMessage` by the same rule (its decision 12).
17. `deliver` writes `quoteLine(replyTo)` and a line break before the member's message, as the replies plan specifies it (its decisions 12 and 18). The prompt line of a reaction is `{prompt: {text: "> BODY\nEMOJI", from: REACTOR, item: REACTED, replyTo: {item: REACTED, from: SENDER}}}` ([protocol.ts:59-62](../packages/simplex-chat-mcp/src/protocol.ts#L59-L62)). `item` is the reacted message's ID for two reasons. The hub's channel tags, its turn records and the answer's quote use `item` ([hub.ts:27](../packages/simplex-chat-mcp/src/hub.ts#L27), [:266-275](../packages/simplex-chat-mcp/src/hub.ts#L266-L275), [:297-301](../packages/simplex-chat-mcp/src/hub.ts#L297-L301)). A session server of 0.3.0 maps `text`, `from` and `item` and ignores other fields ([session.ts:198](../packages/simplex-chat-mcp/src/session.ts#L198)). `item` equal to `reply_to` marks a reaction for Claude.
18. The session server maps `replyTo` to the meta `reply_to` and `reply_to_from`, as for a reply (decisions 1 to 4 and 25 of the replies plan). Claude Code keeps meta keys that match `^[a-zA-Z_][a-zA-Z0-9_]*$` (binary offset 308443090). The docs: "Keys must be identifiers: letters, digits, and underscores only. Keys containing hyphens or other characters are silently dropped."
19. The `replyTo` of a reaction is the `ReplyTo` `{item, from, type?, text}` that `toReplyTo(chatInfo, {itemId, chatDir, content, fileName}, self)` computes from the reacted item: its `meta.itemId`, `chatDir`, `content.msgContent` and `file.fileName` (decisions 18 and 24 of the replies plan; [messages.ts:206-215](../packages/simplex-chat-mcp/src/messages.ts#L206-L215), [hub.ts:167](../packages/simplex-chat-mcp/src/hub.ts#L167)). Its sender is named by `itemSender` ([messages.ts:294-307](../packages/simplex-chat-mcp/src/messages.ts#L294-L307)), with the hub's profile name, `Claude`, as the agent's own name, `{name: this.hub.profile.profile.displayName}`, as `onMessage` names it for a reply ([hub.ts:159](../packages/simplex-chat-mcp/src/hub.ts#L159); decisions 8 and 12 of the replies plan). Every session group is created with that profile and not incognito ([hub.ts:495](../packages/simplex-chat-mcp/src/hub.ts#L495), [control.ts:626-627](../packages/simplex-chat-mcp/src/control.ts#L626-L627), [api.ts:658-659](../packages/simplex-chat-nodejs/src/api.ts#L658-L659)). The posts of `User` get `User`, and a member's messages the member's display name. `onReaction` computes `replyTo` when it queues the prompt, without an await, so the order of the delivery chain is kept (0.3.0 decision 45).
20. The quote line of a reaction is `quoteLine` of its `replyTo`, by the rule of the replies plan (its decisions 5 to 7 and 26, and its mechanics of `replyText` and `quoteLine`; [messages.ts:206-230](../packages/simplex-chat-mcp/src/messages.ts#L206-L230)). `toReplyTo` sets `type` to `content.type` for a message other than a text and other than `unknown`, and `text` to the message's text, or to its file name when the text is empty. `quoteLine` is `> ` and `replyText` of a body: the text for a text; `TYPE: TEXT`, with the text trimmed, such as `image: Trail map near the gate`, for a link, image, video, voice message, file, report or chat; the type alone when the trimmed text is empty. An empty body gives `>` alone. `replyText` takes the first line of the trimmed body, ended by a line break of any kind, trimmed. Over `replyTextLength`, 200, grapheme clusters, the type included, it takes the first 200 without the trailing spaces. `…` follows when the line is longer or more lines follow. A line break ends the body, so the quote takes one line of the content, and the emoji follows on the next. A reacted item has its file name, so a reacted image, video, voice message or file without text is quoted with its type and its file name. A reply's quote names an image or a file without text by its file name too, which the replier's core writes into the quote ([Internal.hs:374-378](../src/Simplex/Chat/Library/Internal.hs#L374-L378), [:398-399](../src/Simplex/Chat/Library/Internal.hs#L398-L399)). For a video or voice message without text, a reply's quote has no file name, and its line is the type alone, such as `> video` (decision 7 of the replies plan).
21. Claude Code queues a channel notification as a prompt with priority `next`, origin `channel` and `skipSlashCommands` (binary offsets 308443184 and 314212088). The docs: "Events queue into the session and are processed in order. If several notifications arrive while Claude is busy, they're delivered together on the next turn and Claude handles them as a group."
22. `User` posts nothing for the prompt of a reaction. Its transcript entry has the origin `channel`, so the hook sends `typed: false` ([hook.ts:124-129](../packages/simplex-chat-mcp/src/hook.ts#L124-L129), decision 121 of the 0.3.0 plan, [flow 21](../packages/simplex-chat-mcp/spec/flows.md#21-a-terminal-prompt-and-the-answer): "Task notifications, messages of other sessions and channel prompts have other origins."). Its channel tag also matches the pushed reaction, whose text is the quote line and the emoji ([hub.ts:195](../packages/simplex-chat-mcp/src/hub.ts#L195); decision 14 of the replies plan).

The answer:

23. The answer of a reaction's turn quotes the reacted message, as the answer of a message quotes the message. `Session.answer` quotes an item with `sndMsgContent` too; today it quotes only `rcvMsgContent` ([hub.ts:278](../packages/simplex-chat-mcp/src/hub.ts#L278)). The core quotes the user's own group messages ([Internal.hs:236](../src/Simplex/Chat/Library/Internal.hs#L236)). The rules of decisions 33 and 50 of the 0.3.0 plan apply to the quote.
24. Each channel tag of a turn's prompt matches one pushed prompt: the oldest one with its item and text that no other tag of the prompt matched. Today one tag matches every equal entry ([hub.ts:300](../packages/simplex-chat-mcp/src/hub.ts#L300)). Two members' equal reactions to one message are equal entries, and each one's turn quotes the reacted message. A message has a unique item, so its matching is unchanged.

Shared with `ep/mcp-replies`, and Claude's own reactions:

25. `ep/mcp-replies` (#8162) adds `ReplyTo`, `toReplyTo`, `quoteLine`, `replyText` and `replyTextLength` in `messages.ts`, the quote line that `deliver` writes, the field `replyTo` of the prompt line, its two meta keys and the instructions' sentences on replies. This branch is made on top of it, at `7437f8f96`, and reuses them. Its pull request opens after #8162 is merged.
26. Claude's own reactions are not in this pull request (Q2 A). A later `react` tool would follow the Telegram and Discord channels of Claude Code, in https://github.com/anthropics/claude-plugins-official at `23c304a6`. Their `react` tool adds an emoji to a message by the chat's and the message's IDs (`external_plugins/telegram/server.ts:484-495`, `:599-604`; `external_plugins/discord/server.ts:545-556`, `:680-685`). Those servers pass no reactions of their users to Claude: the Telegram server handles messages and button presses only (telegram:738-882), and the Discord server handles new messages and interactions only, without reaction events (discord:83-86, :747, :805). The hub would send Claude's reactions with `apiChatItemReaction` of `simplex-chat` 7.1.3 ([api.ts:530-540](../packages/simplex-chat-nodejs/src/api.ts#L530-L540)).

## Design

Structurally different representations of a reaction, with their cost:
```
option   representation                       producer, the hub          consumers                   0.3.0 session server     wire
A        the prompt line: text the quote      onReaction, the replyTo    the session server maps     the quote line and the   one optional field
         line and the emoji, item the         by toReplyTo; deliver      replyTo to two meta keys    emoji with item;         of the prompt line
         reacted message, replyTo of it       writes the quote line                                  replyTo ignored
B        as A, without item                   as A                       as A; the tag pattern of    meta item "undefined"    as A
                                                                         the hub also takes reply_to
C        a new line type {reaction}           as A, and a second line    a second branch in          the reaction dropped,    a new line type
                                              writer                     channelNotification         its pushed entry kept
D        a sentence of the plugin as the      a text builder             none                        the sentence             none
         content: "reacted 👍 to ..."
E        a list per session, merged into the  the list, its removals,    the merge into the next     as A                     as A
         next prompt from the group (Q1 B)    the merge                  channel prompt
```

A is chosen. B costs a second tag pattern and gives 0.3.0 servers a wrong `item`. C duplicates the fields of a prompt, and a 0.3.0 server drops the reaction while its pushed entry waits for a match. D mixes the plugin's words into the member's message. E is option B of Q1.

A uses the form of a reply. `ep/mcp-replies` passes a reply's quoted message to Claude, so both pull requests use one `replyTo` field, one builder, one quote line and one set of meta keys (decision 25, [Conflict areas](#conflict-areas-with-the-other-items)).

The prompt line of `/attach`, for a reaction:
```
prompt
  text       the quote line of the reacted message, a line break, the emoji
  from       the reactor's display name
  item       the reacted message's ID
  file       absent
  replyTo    of the reacted message, as the replies plan writes it on the line
replyTo
  item       its ID, always present for a reaction
  from       its sender: Claude, User or the member's display name
```

The channel event, for a reaction, by decisions 1 to 4 and 25 of the replies plan:
```
content         prompt.text
from            prompt.from
item            prompt.item
reply_to        replyTo.item
reply_to_from   replyTo.from
```

Changes per file and function. The rows marked `first` are made by `ep/mcp-replies`, as the replies plan specifies them:
```
file                 function                    change
src/control.ts       isGroupMessage              takes {chatInfo}, so that it accepts an ACIReaction
src/messages.ts      ReplyTo, toReplyTo,         first: as in the replies plan; ReplyTo is {item?, from?, type?, text};
                     quoteLine, replyText,       toReplyTo takes {itemId?, chatDir?, content, fileName?}
                     replyTextLength
src/protocol.ts      AttachLine.prompt           first: replyTo?: {item?, from?}
src/session.ts       channelNotification         first: meta reply_to and reply_to_from from replyTo
src/session.ts       channelInstructions         first: the sentences on replies; this one: the sentence on reactions
src/hub.ts           Hub.onEvent                 chatItemReaction of a session group: Session.onReaction
src/hub.ts           Session.onReaction          new: the checks of decisions 9 to 12; the prompt with its replyTo on the delivery chain
src/hub.ts           Session.onMessage           passes the prompt with the message's own file
src/hub.ts           Session.prompt, deliver     take ciFile, the file to receive; first: replyTo, and quoteLine of it
src/hub.ts           Session.answer              quotes sndMsgContent items too
src/hub.ts           Session.pushedIn            one pushed prompt per channel tag
```

Mechanics:
```
on chatItemReaction e, group of session N, no groupChatScope:   N.onReaction(e)
onReaction({added, reaction}):
  added
  reaction.chatReaction.chatDir is groupRcv, its member m
  canSend(role of m)
  reaction.chatReaction.reaction.type = emoji, its emoji a string
  reacted.content is rcvMsgContent or sndMsgContent   (decision 12)
  all true:   replyTo = toReplyTo(chatInfo, {itemId, chatDir, content, fileName} of reacted, {name: the hub's profile name})
              delivery = delivery then prompt(reacted, emoji, m, no file, replyTo)
onMessage(m), another message:   replyTo = toReplyTo of m.quotedItem, when present   (the replies plan)
                                 delivery = delivery then prompt(m, text, member, m.file, replyTo)
prompt(item, text, member, ciFile, replyTo):   member is User: nothing; else deliver(the same)
deliver(item, text, member, ciFile, replyTo):  as at 7437f8f96, with ciFile in place of the item's file
             text = replyTo present: quoteLine(replyTo) + "\n" + text; else text   (the replies plan)
             pushed += (text, item, now)
             the line {prompt: {text, from: name of member, item: id of item, file?, replyTo: {item?, from?} of replyTo}}
answer(a, q):  quote q when its content is rcvMsgContent or sndMsgContent
pushedIn(prompt):  each channel tag (item, content) takes the oldest pushed entry with that item and text that is not taken yet
                   an entry equal to the raw prompt, pushed in the last 60 s: matched, as at 0.3.0
                   the matched entries in pushed order
```

Two equal reactions to one message, each taken in its own turn (decision 24):
```mermaid
sequenceDiagram
  participant HK as UserPromptSubmit hooks
  participant N as Session N
  Note over N: pushed: (quote line and 👍, item 57) of Bob, then the same of Carol
  HK->>N: turn 1, one tag of item 57
  N->>N: takes Bob's entry, turn record of item 57
  HK->>N: turn 2, one tag of item 57
  N->>N: takes Carol's entry, turn record of item 57
  Note over N: each answer quotes message 57. At master, turn 1 takes both entries, and turn 2 has no quote.
```

Which reactions are passed:
```mermaid
flowchart LR
  E[chatItemReaction] --> G{group of a session, no scope}
  G -- no --> X[not passed]
  G -- yes --> A{added}
  A -- no --> X
  A -- yes --> R{the reactor's role sends}
  R -- no --> X
  R -- yes --> M{type emoji, the emoji a string}
  M -- no --> X
  M -- yes --> U{the reactor is User}
  U -- yes --> X
  U -- no --> P[prompt on the delivery chain]
```

Data flow:
```mermaid
flowchart LR
  APP[member's app] -- x.msg.react --> CORE[core _hub]
  CORE -- chatItemReaction --> HUB[Hub.onEvent]
  HUB --> SES[Session.onReaction]
  SES -- the reacted item, the hub's profile name --> RT[toReplyTo]
  RT --> SES
  SES --> DEL[Session.deliver: hold, not running, quote line, pushed]
  DEL -- prompt line with text and replyTo --> SRV[session server]
  SRV -- content, meta from, item, reply_to, reply_to_from --> CC[Claude Code]
  CC -- UserPromptSubmit and Stop hooks --> TURN[Session.turn: tag match, turn record, quote]
  TURN -- the answer, quoting the reacted message --> CORE
```

## Compatibility

- SimpleX apps: every app that joins a session group, 7.1 or later, sends and shows reactions. An emoji of a newer app is passed as its text (decision 7). The apps are unchanged.
- Claude Code: the meta keys match its key pattern (decision 18). The channel contract of 2.1.322 is the one of 0.3.0.
- A 0.3.0 session server attached to a newer service: the protocol version stays 1, and the new field is optional. The server passes the quote line and the emoji with `item`, without the meta keys ([session.ts:197-198](../packages/simplex-chat-mcp/src/session.ts#L197-L198)).
- A newer session server and a 0.3.0 service: the session server stops the older service and starts its own ([client.ts:35-46](../packages/simplex-chat-mcp/src/client.ts#L35-L46)).
- Plugin data folders: unchanged. Pushed prompts are kept in memory, as at 0.3.0.
- `serve`: unchanged. `AgentTools.onEvent` ignores reactions ([tools.ts:665-691](../packages/simplex-chat-mcp/src/tools.ts#L665-L691)).

## Acceptance and tests

Unit tests, `test/unit/sessions.test.ts`, "session channel":
1. `passes a reaction of a member whose role sends as the quote line of the reacted message and the emoji, with the message's ID and sender, and nothing for an observer, User, a removed reaction, another kind of reaction or a reaction in the Main group`. It fails at `7437f8f96`: no line is written. It compiles there, because it passes the event to `Hub.onEvent` as a `ChatEvent` and compares the lines with `toEqual`.
2. `quotes in the prompt of a reaction to a reply the reacted message, not the message that the reply quotes`.
3. `quotes a reacted image or video without a caption by its type and file name, and receives no file of the reacted message`: `> image: IMG_2041.jpg\n❤` and `> video: ride.mp4\n🚀`.
4. `quotes the reacted message in the answer of a reaction's turn, Claude's own answer included`.
5. `replies that the session is not running to a reaction, quoting the reacted message`.
6. `leaves a pending tool-use card pending when an approver reacts to it, and passes the reaction as a prompt`.
7. `matches each channel tag of a turn's prompt to one pushed prompt, so two equal reactions to one message are quoted in their own turns`.

The unit tests of `toReplyTo`, of the quote line and of the meta keys in `session.test.ts` are those of the replies plan, made by `ep/mcp-replies`. This one adds to `is a channel` of `session.test.ts` that the instructions describe a reaction.

End-to-end tests, `test/e2e/service.test.ts`:
- In the test of the hooks: Alice reacts 👍 to Claude's answer `Fixed.` before the session attaches. Claude replies `Session 1 is not running.`, quoting `Fixed.` After the attachment, Alice reacts 👍 to `Tests pass.`, and the stream gets `{prompt: {text: "> Tests pass.\n👍", from: "Alice", item, replyTo: {item, from: "Claude"}}}`. The hooks pass the channel tag and an answer, and Alice gets the answer quoting `Tests pass.`. Alice removes the reaction and adds 🚀, and the next line of the stream is the one of 🚀. This test fails at `7437f8f96`.
- In the test of the roles: Carol, an observer, reacts to Claude's file message, and Alice gets the reaction. Then Bob reacts to the tool-use card, and the stream gets Bob's reaction. At the test's end, the stream has no reaction of Carol.

CI: the reduced build of the fork branch runs scope `mcp` in build-linux, `scripts/ci/scope-tests.sh mcp`, with the unit and end-to-end tests ([scripts/ci/README.md](../scripts/ci/README.md), section "mcp"). Only `packages/simplex-chat-mcp` changes, so the app test workflows are not run.

Real session, `TESTING.md`, a new check 6: react 👍 to Claude's answer in the group. Claude answers as a reply to that answer, and `User` posts nothing. In the transcript, the prompt's content is `> ` with the start of that answer, and 👍 on the next line. Remove the reaction, and nothing happens. React while Claude counts to 300, and the reaction is answered in the next turn.

## Docs to change

- `README.md`, step 4: `Claude also gets your reactions, as emoji replies.`
- `REFERENCE.md`, "Prompts and answers": one item on reactions, decisions 8 to 23 in users' terms.
- `ROADMAP.md`: the line `React to a message, and Claude sees your reaction.` is removed.
- `spec/api.md`: 4.3, the rules of the session group, and the post `Session N is not running.` for a reaction; 4.5, the content and the meta of a reaction and the instructions; 4.6, the prompt line of `/attach` for a reaction.
- `spec/architecture.md`: 8.2, the rows of `hub.ts`, `session.ts` and `protocol.ts`; 8.3, the pushed prompts with reactions; 8.4, the event dispatch of `chatItemReaction`; 8.6, the prompt line and the mechanics.
- `spec/flows.md`: a new flow 32, "A reaction in the session group", with the table of contents. Flow 31 is the one of #8157.
- `TESTING.md`: the automated tests and the real-session check 6; later checks are renumbered.

## Conflict areas with the other items

- `ep/mcp-replies`, the strongest overlap. The replies plan owns the form of a replied-to message: `ReplyTo` with its `type`, `toReplyTo`, `quoteLine`, `replyText` with `replyTextLength`, the quote line that `deliver` writes, the field `replyTo` of the prompt line and the meta keys `reply_to` and `reply_to_from`. The pull request merged first adds them, and the second reuses them (decision 25). Both plans compute `replyTo` in the caller of `deliver`, with the hub's profile name as Claude's name (decisions 16 and 19; decision 12 of the replies plan). `AgentTools.selfSender` of the replies plan serves tool results only, and this item does not use it.

  Shared code: `messages.ts`; `hub.ts` `Session.onMessage`, `prompt`, `deliver`, `answer`, `pushedIn`, `Hub.onEvent`; `protocol.ts` `AttachLine`; `session.ts` `channelNotification`, `channelInstructions`; the tests "session channel" in `sessions.test.ts`, `is a channel` in `session.test.ts`, and the end-to-end test of the hooks in `service.test.ts`, which both items extend; `spec/api.md` 4.3 to 4.6; `spec/architecture.md` 8.2, 8.6; `spec/flows.md` 20 and 21; `REFERENCE.md` "Prompts and answers".
- `ep/mcp-harness-notes`: Claude posts notes for Claude Code's own prompts, which have `typed: false`. The prompt of a reaction is a channel prompt with `typed: false` too, so the notes must leave out channel prompts. Shared code: `hub.ts` `Session.turn`, `post`; `hook.ts` `promptTyped`; `REFERENCE.md` "Prompts and answers"; `sessions.test.ts`.
- `ep/mcp-agent-profile`: `reply_to_from` of Claude's posts is the hub's profile name, `Claude`, which that item may change. The shared files are the docs below.
- `ep/mcp-flat-sessions`: the shared files are the docs below.
- Every item: `ROADMAP.md`, `REFERENCE.md`, `TESTING.md` with its numbered checks, `spec/*.md`, and the version in `package.json` and `.claude-plugin/plugin.json`.

## Estimate

```
part                                     lines
source: hub.ts, control.ts               about 30
source of the rows marked first          about 25, when this pull request is merged first
unit tests                               about 115
end-to-end tests                         about 35
docs                                     about 80
```

About 3 hours of work and one CI run of about 40 minutes, without the merges of the other items.
