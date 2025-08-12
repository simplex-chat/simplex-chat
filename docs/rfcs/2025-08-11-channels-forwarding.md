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
  - Question: If messages are to be signed by owners, what differences can chat relays introduce? Does this point in initial doc imply owner sending different versions of message to chat relays? Losses in delivery? Answer: Not all messages will be signed.

#### Highlighting deduplicated messages differences

Currently we simply ignore duplicate messages - see createNewRcvMessage.

Some options on how to highlight difference in messages from chat relays:
1. Show only the fact that there is difference.
    - Persist duplicate error (SEDuplicateGroupMessage) as flag on chat item to then display warning sign in UI.
2. Save message hashes as some additional entity linked to message or chat item for each chat relay.
    - If any new hash differs, set flag on chat item for warning sign in UI.
    - Add ability to load additional info (with other chat item info via APIGetChatItemInfo) showing which relays sent different hashes (e.g. 1 differs from 2 others - show 1 as warning, all differ - show all as warning).
3. As previous, but save content. Possibly save content additionally to hashes, for faster comparison.
    - Saving content means converting duplicate messages to chat item content, or only saving text, but latter will fail to show difference in files.

Service events will not have content. We still can/should compare them and indicate difference, e.g. by creating a special chat item (similar to integrity violation).

How to compute hashes. Problem is with file descriptions, as it is legitimate case that they will be different. Hash could be computed as hash of message without file invitation + file digest from file invitation (FileInvitation.fileDigest).

Files can also cause problem with showing difference in content if file preview is the same, but file hash is different. So showing difference in content does not exclude necessity of showing difference in hashes as in option 2.

As a note, this whole section discusses an edge case of chat relays maliciously changing messages, and countermeasures to prevent them doing so undetectably. May be not worth implementing overly complex solution as MVP (option 3), and option 2 or even 1 can suffice. TBC further.

### Persisted forwarding jobs

- For batch of received messages a forwarding job has to be persisted.
- Special worker would pick up forwarding jobs for sending.
- Forwarding job can be split into smaller batches by members to make smaller group sends one at a time.

How to present a forwarding job in storage?

Currently we build ad-hoc forwarding instruction for each message based on its scope (GroupForwardScope). Then for each scope message batches are sent separately to different member lists.

Question: should we persist all forwarding jobs or only for main (GFSMain - for regular messages) and all (GFSAll - for all current and pending members scopes), and don't persist for support scope (GFSMemberSupport)?

Pros/cons of persisting jobs for all scopes:
- Possibly more uniform processing. However, for support scope group member id is required (see question in schema below).
- Requires forwarding worker to have more logic.

Pros/cons of persisting jobs only for GFSMain and GFSAll scopes:
- Forwarding worker and persistence are simpler, but logic lives in 2 places (synchronous as now + worker).
- Support scopes have small number of members to forward to, so optimizing forward for them is not a necessity.

We roughly need following data to be persisted for a forwarding job:
- group id,
- forwarding scope - TBC as above,
- sending member - to include memberId in XGrpMsgForward,
- broker timestamp of received messages batch to include in XGrpMsgForward,
- "message from channel" flag from sending owner - see "Hiding sending owner id" section below,
- list of messages (events).

Restoring list of messages for a forwarding job:
- Currently for each message full MsgBody is saved, which is just a ByteString. Also if received messages were batched, full batch body is saved for each message record. For example, here is message body of batched deletion of 2 chat items, which is saved on 2 message records:
  ```
  msg_body = [{"v":"1-16","msgId":"ZUd3bzVDTXFHWmc3Z29YSQ==","event":"x.msg.del","params":{"msgId":"QVVuN2RkaXg0aVJJdTZXSA=="}},{"v":"1-16","msgId":"amM5dzV2RkRjNmNYSDRLQQ==","event":"x.msg.del","params":{"msgId":"NzFlV3pNRStpQTRrQjRYUg=="}}]
  ```
  On the other hand for forwarding operation we require ChatMessage 'Json (for XGrpMsgForward).
- For a forwarding job we can persist list of messages ids from messages table (message_id), and save ChatMessage 'Json on records to restore.
- Alternatively, we can read full msg_body (it can even be duplicated on forwarding job), and restore chat messages (ChatMessage 'Json) via parseChatMessages. However, this conflicts with possibility that a single batch body will have messages of different scopes, which current processing logic allows, although there's no legitimate case as of now. So, persisting ChatMessage' Json on each record seems preferable. Also it avoids repeated parsing.
- As a side note, it seems we can stop saving msg_body for received messages, as it's not used for any purpose. This field is only used for retrieving and sending pending group messages.

#### Splitting forwarding job into delivery batches

Chat relay should be able to handle channels consisting of hundreds of thousands of members - loading all members for forwarding in memory will not scale.

Approach 1.

Forwarding job will read required members in loop using a cursor (use group_member_id for cursor? member_id?) to split into further delivery jobs (group sends). Forwarding job can launch group sends in new threads so they start concurrently with job processing further member records. Possibly cursor position can be remembered on the job record after successful scheduling of group send, for faster recovery on failure.

It's unclear if there's a need for a separate lower level abstraction for delivery jobs, that could be reused for feeds and other purposes, as they would require different logic of building delivery lists, and scheduling a delivery for agent already serves a similar purpose.

Optimizing admin forwarding in regular groups would require different queries for member filtering.

Schema draft:

```sql
ALTER TABLE messages ADD COLUMN chat_message_json TEXT;

CREATE TABLE forwarding_jobs (
  forwarding_job_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  forward_scope TEXT NOT NULL, -- save as JSON/text? add field for scope group member id for GFSMemberSupport scope?
  sending_group_member_id INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE, -- or sending_member_id BLOB without fkey
  broker_ts TEXT NOT NULL,
  group_as_sender INTEGER NOT NULL DEFAULT 0, -- for "message from channel" flag from owner
  message_ids TEXT NOT NULL, -- comma separated list; normalize via many-to-many table? doesn't seem necessary
  failed INTEGER DEFAULT 0, -- for worker marking forwarding job as failed, to be able to proceed to next one
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
```

Approach 2.

Pre-allocate group member ids for the forwarding job in a separate table at the moment of job creation. It may be not as taxing to read only member ids without cursor, and persist them for the job in the same transaction.

Same applies - optimizing for admin forwarding would require different queries.

```sql
CREATE TABLE forwarding_jobs_members (
  forwarding_job_member_id INTEGER PRIMARY KEY,
  forwarding_job_id INTEGER NOT NULL REFERENCES forwarding_jobs ON DELETE CASCADE,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE
)
```

Then forwarding job would then use a simpler query to load members in loop using a cursor - without complex filters for member selection. Each scheduled group send can delete its forwarding_jobs_members records, or mark them as complete (requires new field).

Forwarding jobs for different groups can be concurrent, inside group should be sequential to follow order of messages. One approach could be to create a dedicated forwarding worker for each group.

Forwarding worker(s) should use low priority db pool.

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

Question: Should chat relays also hide sending owner from other owners or not? Probably not, then forwarding job would have to be split into separate sends with different sets of XGrpMsgForward events (with and without sending owner's member id).

### Connection deleting events

Forwarding regular messages, reactions, updates and many other events is straightforward. However, naive processing of some events currently breaks forwarding logic, specifically member and group deletion.

As it is now, forwarding operation follows event processing. So, processing of XGrpMemDel, XGrpDel first deletes member connection (or connections with all members in case of group deletion), then attempts to forward these events, which in reality never works as at this point connection is already deleted.

Currently we ignore this problem (there are TODOs), as all members according to protocol connect with each other, and forwarding serves only as message delivery improvement/backup, and not a main route, and so members in at least supposedly most cases receive these events from the original sender.

With chat relays, however, no messages are sent directly from owners to members, so if logic is kept as is these events would never be received. So, their processing should be special cased to delay until after forwarding operation.

Question: Should this be resolved in the same scope, or separately? This is already an issue in current group design, so it could be a separate improvement.
