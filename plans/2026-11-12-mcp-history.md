# SimpleX MCP server 0.2.0: receive with history

Status: draft for review. Decided on 2026-11-12: history in `receive`, at most 50 messages. Code references: master `c6e7549b12`.

## Decisions

1. `receive` takes `history`, an integer from 1 to 50.
2. With it, the result has `history`: the last message items of the chat before the first item of `messages`, or the last ones when `messages` is empty, in chat order.
3. Message items are received and sent messages (`rcvMsgContent`, `sndMsgContent`); new and live items, and items with `itemDeleted` (deleted, blocked or moderated), are left out.
4. The agent's own items are named as in `connect`: its name in the chat; `from_type` only outside incognito chats.
5. History items are not marked read and do not change `more`.
6. History is read when the call returns, after any wait, before the returned messages are marked read.

## receive

Input:
```
field     value
history   integer from 1 to 50; optional
```

Output:
```
field     value
history   Message[]; only with the input
```

`historyItems(chat, ref, before, count)`:
```
pagination = before=<before> count=100, or count=100 without before
loop:
  page = /_get chat <ref> <pagination>
  items = the message items of page that are not rcvNew or live and have no itemDeleted, then items
  items holds at least count, or page holds fewer than 100: return the last count of items
  pagination = before=<the first item ID of page> count=100
```

Changes:
```
file          function            change
messages.ts   historyItems        new
tools.ts      ReceiveInput        history
              receiveSchema       history
              receiveOutput       history
              receiveDescription  one sentence on history
              receive             history read after the wait, before markRead; in the result only with the input
              historyMessages     new: chatFields, selfSender, historyItems, toMessage
              selfSender          new: the self Sender of connect
              connectedChat       selfSender
```

## Tests

```
file                         test
test/unit/messages.test.ts   items in order before the returned new ones; own ones under the agent's name; live, deleted and other items left out; only the new ones marked read
                             the latest items when none is new; no history without the input
                             history read after the wait
                             a new item whose event has not arrived left out of history; the next call returns it
                             pages of 100 items before the first new message, and from the end
                             own items under the incognito name
test/unit/http.test.ts       history 51 refused by the schema
```

Docs:
```
file                   change
README.md              the agent recalls the conversation in a new session
REFERENCE.md           receive with history; a recovery row for a lost session
TESTING.md             a step: history in a new session
PLAN.md                receive input and result; receive steps; tests; decisions of 2026-11-12
spec/api.md            1.4: history input and output; 1.8: from, from_type and sent items; anchors remapped
spec/architecture.md   historyItems; limits; anchors remapped
spec/flows.md          5: history in the diagram and the notes; anchors remapped
spec/README.md         the role of messages.ts; anchors remapped
```
