# Channels forwarding

This expands on the previous [channels rfc](./2025-07-30-channels.md), specifically on message forwarding by chat relays.

## Problem

Current implementation of groups uses forwarding mechanism for improved message delivery between connecting members. From perspective of reusing it for channels it has following limitations:
- Messages are forwarded only for the duration of members establishing connection, until they report x.grp.mem.con to forwarding admin.
- For a given pair of members only a single admin forwards messages - inviting admin (host) forwards messages between its invitees and members introduced to them. It means:
  - This admin can be a single point of failure and/or can cause arbitrary delays in message delivery based on its availability when forwarding messages between these member pairs.
  - Member pairs fully trust their single forwarding admin in forwarded content.

Not limitations of protocol, but other weak points of current implementation:
- Forward operations are synchronous to message reception (by forwarding admin), so they're not resumable on failure.
- Forward operations are carried out as "group sends" to all required members (invitees/introduced members in respect to sending member), number of which is expected to grow very large in channels and can potentially have a big load on the database.

## Solution

Chat relays will serve as forwarding agents instead of inviting admins, with following modifications to protocol and implementation.

### Forwarding not limited to establishing connection

- In channels members will not connect directly to other members and channel owners.
- Chat relays will not take into consideration status of introductions.
- Chat relays will forward messages between owners and all members (from members to owners, for example, for reactions, comments).

### All chat relays forward all messages

- Instead of inviting admins all chat relays will forward all messages.
- Members will deduplicate messages (already implemented).
- Members should highlight differences in deduplicated messages. This would solve trust issue.
  - Question: If messages are to be signed by owners, what differences can chat relays introduce? Does this point in initial doc imply owner sending different versions of message to chat relays? Losses in delivery?
  - TBC prototype UI, backend.

### Persisted forwarding instructions

- For batch of received messages a forwarding instruction has to be persisted.
- Special worker would pick up forwarding instructions for sending.
- Forwarding instruction can be split into smaller batches by members to make smaller group sends one at a time.

How to present a forwarding instruction in storage?

Currently we build ad-hoc forwarding instruction for each message based on its scope (GroupForwardScope). Then for each scope message batches are sent separately to different member lists.

Question: should we persist all forwarding instructions of or only for main (GFSMain - for regular messages) scope? For main and all (GFSAll), and don't persist for support scope (GFSMemberSupport)?

We roughly need following data to be persisted for a forwarding instruction:
- group id,
- forwarding scope - TBC as above,
- sending member - to include memberId in XGrpMsgForward,
- broker timestamp of received messages batch to include in XGrpMsgForward,
- list of messages (events).

Restoring list of messages for a forwarding instruction:
- Currently for each message full MsgBody is saved, which is just a ByteString. Also if received messages were batched, full batch body is saved for each message record. For example, here is message body of batched deletion of 2 chat items, which is saved on 2 message records:
  ```
  msg_body = [{"v":"1-16","msgId":"ZUd3bzVDTXFHWmc3Z29YSQ==","event":"x.msg.del","params":{"msgId":"QVVuN2RkaXg0aVJJdTZXSA=="}},{"v":"1-16","msgId":"amM5dzV2RkRjNmNYSDRLQQ==","event":"x.msg.del","params":{"msgId":"NzFlV3pNRStpQTRrQjRYUg=="}}]
  ```
  On the other hand for forwarding operation we require ChatMessage 'Json (for XGrpMsgForward).
- For a forwarding instruction we can persist list of messages ids from messages table (message_id), and save ChatMessage 'Json on records to restore.
- Alternatively, we can read full msg_body (it can even be duplicated on forwarding instruction), and restore chat messages (ChatMessage 'Json) via parseChatMessages. However, this conflicts with possibility that a single batch body will have messages of different scopes, which current processing logic allows, although there's no legitimate case as of now. So, persisting ChatMessage' Json on each record seems preferable. Also it avoids repeated parsing.
- As a side note, it seems we can stop saving msg_body for received messages, as it's not used for any purpose. This field is only used for retrieving and sending pending group messages.

Schema draft:

```sql
ALTER TABLE messages ADD COLUMN chat_message_json TEXT;

CREATE TABLE forwarding_instructions (
  forwarding_instruction_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  forward_scope TEXT NOT NULL, -- save as JSON/text? add field for scope group member id for GFSMemberSupport scope?
  sending_group_member_id INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE, -- or sending_member_id BLOB without fkey
  broker_ts TEXT NOT NULL,
  message_ids TEXT NOT NULL, -- comma separated list; normalize via many-to-many table? doesn't seem necessary
  failed INTEGER DEFAULT 0, -- for worker marking forwarding instruction as failed, to be able to proceed to next one
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
```

Forwarding worker will read forwarding instructions sorted by created_at, retrieve chat messages corresponding to message_ids, retrieve group members based on group_id and forward_scope and send batched forward events to them.

How to split forwarding to large number of group members into smaller batches?

One option is to determine forwarding lists at the moment of creating forwarding instruction and persist them, possibly already split into predefined batches. However, this unnecessarily excludes newly joined members (those that joined between creating instruction and worker picking it up for forwarding operation).

Another approach would be to split retrieved members into batches at time of processing instruction, and persist group member ids that instruction was processed for after sending one batch at a time. Group member retrieval should then filter out already processed members in case of operation failure leading to repeat processing.

```sql
CREATE TABLE forwarding_instructions_members (
  forwarding_instruction_member_id INTEGER PRIMARY KEY,
  forwarding_instruction_id INTEGER NOT NULL REFERENCES forwarding_instructions ON DELETE CASCADE,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE
);
```

Overall forwarding worker algorithm would then be:
1. Retrieve next forwarding instruction.
  - Retrieve chat messages corresponding to message_ids, build forward events (XGrpMsgForward).
  - Retrieve member list based on group_id and forward_scope, filtering out already processed members via forwarding_instructions_members.
2. Split member list into smaller batches. TBC how to choose optimal batch size.
3. For each batch of members:
  1. Group send (sendGroupMessages_) batch of XGrpMsgForward events.
  2. Persist processed member ids to forwarding_instructions_members.
    - Persist after save is ok because receiving client can deduplicate in case chat relay fails in-between.
  3. Possibly small delay to avoid overloading database.
    - Question: Since chat relays will be working on postgres with multiple connections, do we really need this splitting into member batches? Group send is already batched in itself.
4. Once forwarding instruction is fully processed, delete it.

## Other considerations

### Hiding sending owner id

Initial doc mentions "messages sent from channel name" in minimal testable scope. This would be a feature allowing owners to send messages to channel without it being clear for members which owner sent it. For this, we'd also like to hide sending owner's member id from forwarding operation.

This means MemberId in XGrpMsgForward should become optional.

```haskell
XGrpMsgForward :: Maybe MemberId -> ChatMessage 'Json -> UTCTime -> ChatMsgEvent 'Json
```

Receiving members would then save this message as received from group. We already have necessary machinery for chat items persistence - see ShowGroupAsSender. However, message processing logic would have to be reworked to allow for absence of group member. Also, sending owner would have to indicate it to chat relays in XMsgNew (TBC - other events?).

```haskell
XMsgNew :: ShowGroupAsSender -> MsgContainer -> ChatMsgEvent 'Json
```

Question: Should this be resolved in the same scope, or separately?

Question: Should chat relays also hide sending owner from other owners or not? Probably not, then forwarding instruction would have to be split into separate sends with different sets of XGrpMsgForward events (with and without sending owner's member id).

### Events to forward

Forwarding regular messages, reactions, updates and many other events is straightforward. However, naive processing of some events currently breaks forwarding logic, specifically member and group deletion.

As it is now, forwarding operation follows event processing. So, processing of XGrpMemDel, XGrpDel first deletes member connection (or connections with all members in case of group deletion), then attempts to forward these events, which in reality never works as at this point connection is already deleted.

Currently we ignore this problem (there are TODOs), as all members according to protocol connect with each other, and forwarding serves only as message delivery improvement/backup, and not a main route, and so members in at least supposedly most cases receive these events from the original sender.

With chat relays, however, no messages are sent directly from owners to members, so if logic is kept as is these events would never be received. So, their processing should be special cased to delay until after forwarding operation.

Question: Should this be resolved in the same scope, or separately? This is already an issue in current group design, so it could be a separate improvement.
