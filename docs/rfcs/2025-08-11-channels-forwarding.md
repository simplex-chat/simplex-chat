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
4. Create new versions as "different version of another message" chat items.
    - Timestamp would have to be made the same as on original chat item, as it's not guaranteed to be the same - each chat relay will forward a broker ts based on its own metadata of receiving message from sender.
    - It could be done via special replies, to re-use reply machinery. However, this won't work for messages being replies themselves then.
    - Alternatively, it could say "different message from X relay".

Service events will not have content. We still can/should compare them and indicate difference, e.g. by creating a special chat item (similar to integrity violation).

How to compute hashes. Problem is with file descriptions, as it is legitimate case that they will be different. Hash could be computed as hash of message, excluding file description from FileInvitation. File digest and names should be the same, the problem of different names needs to be fixed.

Files can also cause problem with showing difference in content if file preview is the same, but file hash is different. So showing difference in content does not exclude necessity of showing difference in hashes as in option 2.

As a note, this whole section discusses an edge case of chat relays maliciously changing messages, and countermeasures to prevent them doing so undetectably. May be not worth implementing overly complex solution as MVP (options 3, 4).

For MVP from UI standpoint this should suffice: a marker on item, that would show in info which relays delivered it with a sign which relay delivered different hash. Possibly also special event item next to marked item (with same timestamp) - for service events that don't create a chat item, or in case we don't account it for some chat items.

### Forwarding jobs

- Received messages will be marked for forwarding and picked up by a forwarding worker to form forwarding jobs.
- Forwarding job can be split into smaller batches by members to make smaller group sends one at a time.

**How forwarding works now:**

Currently we build ad-hoc forwarding instruction for each message based on its scope (GroupForwardScope). The output of processing a batch of messages is `Map GroupForwardScope (NonEmpty (ChatMessage 'Json))`. Then for each scope message batches are sent separately to different member lists (to clarify, "sent" here means scheduled for sending in agent). All this is done synchronously in receive loop.

**How forwarding will work:**

Chat relay should be able to handle channels consisting of hundreds of thousands of members. Synchronous forwarding will be replaced by asynchronous forwarding jobs to reduce load in the receive loop, and make forwarding resumable on failure. Also loading all members in memory will not scale, so these jobs have to be split into smaller batches.

Question: Should synchronous processing be changed to forwarding jobs for all types of scopes, or only for Main (GFSMain - for regular messages) and All (GFSAll - for all current members and for all pending members scopes), and not for Support scope (GFSMemberSupport)?
  - Pros/cons of persisting jobs for all types of scopes:
    - Possibly more uniform processing. However, for support scope group member id is required. Either encode scope as json or add field for scope group member id for persistence (see schema below).
    - Requires forwarding worker to have more logic ("if"), or different worker types.
  - Pros/cons of persisting jobs only for Main and All scopes:
    - Forwarding worker and persistence are simpler, but logic lives in 2 places (synchronous as now + worker).
    - As a note, support scopes have small number of members to forward to, so optimizing forward for them is not a necessity.

Answer: All forwarding logic will become asynchronous. This will reduce load in the receive loop for inviting admins in regular groups as well.

Forwarding jobs for different groups can be concurrent, inside group should be sequential to follow order of messages. One approach could be to create a dedicated forwarding worker for each group. Different scope types can also use dedicated workers (possibly: 1 worker for all + main scopes, 1 - for all support scopes), so heavy full group forwarding wouldn't slow down forwarding in much smaller support scopes.

Forwarding workers will re-use worker abstraction from agent.

Forwarding workers should use low priority db pool.

We roughly need following data for a forwarding job:
- group id,
- forwarding scope (forward_scope),
- sending member - to include memberId in XGrpMsgForward,
- broker timestamp of received messages batch to include in XGrpMsgForward,
- "message from channel" flag from sending owner - see "Hiding sending owner id" section below (group_as_sender),
- list of messages (events).

**How to build message batches for a forwarding job:**

Currently for each message a full MsgBody is saved on `messages` table record, which is just a ByteString. Also if received messages were batched, full batch body is saved for each message record (it's a questionable design). For example, here is message body of batched deletion of 2 chat items, which is saved on 2 message records:

```
msg_body = [{"v":"1-16","msgId":"ZUd3bzVDTXFHWmc3Z29YSQ==","event":"x.msg.del","params":{"msgId":"QVVuN2RkaXg0aVJJdTZXSA=="}},{"v":"1-16","msgId":"amM5dzV2RkRjNmNYSDRLQQ==","event":"x.msg.del","params":{"msgId":"NzFlV3pNRStpQTRrQjRYUg=="}}]
```

On the other hand for forwarding operation we require ChatMessage 'Json (for XGrpMsgForward), which could be saved on message records instead.

As a side note, it seems we can stop saving full batch body as msg_body for received messages, as it's not used for any purpose. This field is only used for retrieving and sending pending group messages (sent, not received).

Alternatively, we could read full msg_body, and repeatedly parse list of ChatMessage 'Json. However, this conflicts with possibility that a single batch body will have messages of different scopes, which current processing logic allows, although there's no legitimate case as of now. This means we'd have make jobs forward to different lists of members, or have some supervisor to launch different jobs, which further complicates the processing. Also, this limits forwarding to the same batch sizes (in terms of number of messages) as those batches that received messages came in, and instead we could possibly fit more messages into batches if available. Overall, persisting ChatMessage' Json on each message record, and then retrieving available messages for the job, seems preferable.

**How to split forwarding jobs into smaller delivery batches:**

For simplicity, this will be done only for channels, and not for inviting admins in regular groups, at least initially. Inviting admins have more complex logic, involving multiple queries and filters to retrieve less (only necessary) members, as we're trying to limit load on them, as they can be mobile clients. They will be moved to asynchronous processing too, though.

Forwarding job will read required members in loop using a cursor (e.g., use group_member_id as a cursor) to split into further delivery jobs (group sends) by members. As a reminder, group send in terms of chat logic is not a network operation, but a request to agent. We can consider this part a black box for now, and consider if groups sends should be made concurrently later. For now though, for simplicity we can consider that group sends are done in a synchronous loop with moving cursor for member retrieval. Cursor position can be remembered on the job record after each group send, for recovery.

It's unclear if there's a need for a separate lower level abstraction for delivery jobs, that could be reused for feeds and other purposes, as they would require different logic of building delivery lists, and scheduling a delivery for agent already serves a similar purpose. For now, it seems this additional abstraction is unnecessary.

**How a forwarding job will work overall:**

0. There is some process on start that launches necessary workers for each group/scope client serves as a forwarding agent (for those that have messages that need forwarding). Also message receive loop "kicks" necessary workers. Below we start with a worker for some group/scope trying to retrieve next work item, that is being next message(s) to forward.
1. Worker retrieves next message from `messages` table that was marked for forwarding, that matches the worker's group/scope.
    - We may need a separate field on message record to track "forward pending" state, to know what to forward. Setting forward_scope as a task with addition of forward_complete flag to mark forward completion may be enough (see schema below).
2. Worker checks, if this message is already attached to some forwarding job, and the state of the job.
    1. Worker gets job record.
        - If message is not attached to any job yet, worker creates a new job record, sets job id on the message record (normal execution path).
        - Otherwise worker gets existing job attached to the message (recovery path).
    2. Worker determines forwarding batch encoding for the job.
        - If encoding is not saved on the job, it means it wasn't determined before (normal execution):
          1. Worker retrieves more messages that match its group/scope.
              - Could be in loop or in bulk.
              - We won't know how much we'd need. Some heuristic could be read 100, then mark those that are used with jobId, or load more if needed.
              - Also message list has to preserve reception order.
          2. Worker prepares encoding (wraps messages in XGrpMsgForward events with metadata) and saves it on the job record.
              - It's important to put encoding into job and mark all relevant messages in one transaction.
        - Otherwise worker uses saved encoding (recovery).
3. With moving cursor on group member id for member retrieval, forward encoded batch. In loop:
    1. Retrieve members up to some limit and filtering according to a cursor. In normal execution path on first iteration job would not have a previously saved cursor yet.
        - For Main scope all current members will be retrieved.
        - For Support scopes - moderators and above + scope member. For support scope job can be done without a cursor, instead simply loading all its members and forwarding.
        - For All scope - all current and pending members (In channels there is no need for member review. So possibly we don't have to account for All scope).
        - Main difference from forwarding in regular groups here is that for inviting admins we retrieve only introduced and invited members for the message's sending member, that are not yet connected. For channels there will be no such filtering.
    2. Group send encoded batch to retrieved members.
    3. Update cursor on the job.
4. Marks all messages attached to the job as forwarded (forward_complete), deletes the job record.
    - Possibly do in cleanup manager and delete after say a week.

Schema draft:

```sql
CREATE TABLE forwarding_jobs (
  forwarding_job_id INTEGER PRIMARY KEY,
  msg_batch_encoding TEXT,
  cursor_group_member_id INTEGER
)

ALTER TABLE messages ADD COLUMN chat_message_json TEXT;
ALTER TABLE messages ADD COLUMN forward_scope TEXT;
ALTER TABLE messages ADD COLUMN group_as_sender INTEGER NOT NULL DEFAULT 0; -- for "message from channel" flag from owner
ALTER TABLE messages ADD COLUMN forward_complete INTEGER NOT NULL DEFAULT 0;
ALTER TABLE messages ADD COLUMN forwarding_job_id INTEGER REFERENCES forwarding_jobs ON DELETE SET NULL;

-- indexes for fkey, search based on group/scope and order;
```

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

ShowGroupAsSender can be in MsgContainer, not separate.

Chat relays should not hide sending owner from other owners: sending owner should be visible to them, but message shown as from channel. This means forwarding job has to be split into separate sends with different sets of XGrpMsgForward events (with and without sending owner's member id).

### Connection deleting events

Forwarding regular messages, reactions, updates and many other events is straightforward. However, naive processing of some events currently breaks forwarding logic, specifically member and group deletion.

As it is now, forwarding operation follows event processing. So, processing of XGrpMemDel, XGrpDel first deletes member connection (or connections with all members in case of group deletion), then attempts to forward these events, which in reality never works as at this point connection is already deleted.

Currently we ignore this problem (there are TODOs), as all members according to protocol connect with each other, and forwarding serves only as message delivery improvement/backup, and not a main route, and so members in at least supposedly most cases receive these events from the original sender.

With chat relays, however, no messages are sent directly from owners to members, so if logic is kept as is these events would never be received. So, their processing should be special cased to delay until after forwarding operation.

## Further considerations, ideas (Update)

- Sending member profiles.
  - Profiles of owners (and admins, moderators) should be sent to all members on joining.
  - Profiles of other members should be sent on interaction with them.
  - For MVP: we can show counts on reactions and avoid solving it, but we should have a design to solve this problem, as it would be necessary for comments and later for large groups.
  - Solution draft - partition members based on join timestamp and sender last interaction time:
    - Track last interaction timestamp on each member.
    - When forwarding from this member partition members into two parts: include profile for those who joined after interaction, don't include for those who joined before interaction.
    - This suggests that job would be divided into two parts.
    - Protocol to request member profile as a fallback.
    - This becomes more complex in case batch has multiple members - for n required profiles to send, recipients need to be partitioned into n + 1 parts.
  - Better solution - schedule profile deliveries separately from batch on first post-join delivery per sender.
    - Track last_profile_delivery_ts (for sender), join_ts (for recipient) on member records.
    - On the sender's first overall interaction (last_profile_delivery_ts is null), first create a special task to deliver profile to all (in scope All).
    - On following sends on batching, for senders whose last_profile_delivery_ts < any member's join_ts (i.e., some member missed the initial broadcast), create a profile delivery task for those specific senders.
    - Message task should have a flag whether profile should be delivered to anyone (set to false on first profile delivery). Checking last_profile_delivery_ts is null seems to be sufficient.
- Don't partition for owners based on "message from channel" flag to simplify delivery - no need for two separate jobs/cursors.
- When chat relay receives group deletion event, or event removing it [chat relay itself] from the group:
  - Chat relay should kill all forwarding workers for the group -> delete all jobs -> create one new job to forward group deletion.
  - It could be a special type of job.
- Sending each reaction (and in future comment) won't scale well.
  - Instead periodically send reaction and comment counts.
  - Send reactions and comments themselves on request.
  - This implies that instead of message records, special entity for forwarding tasks should be added, for worker to search for next work items - see more below.
- Connections with priority.
  - Client could have 2 connections/queues with relay, and relay - 2 subscribers.
  - Separation of responsibilities between connections/queues:
    - Normal queue to be used for regular forwarding of messages, reactions, etc.
    - High-priority queue to be used for serving client requests and sending important service events (e.g. ownership changes, group deletion).
  - Possibly special case of connection redundancy.
- Design to be reworked to use special entity for forwarding tasks instead of relying on messages.
  - Points for this:
    - Batched reactions/comments counts.
    - Special logic on group deletion/relay removal.
    - Possibly special logic on sending member profiles, as it's not needed for all types of jobs.
  - Sum type of task types.
  - Some tasks may simply point to message records.
  - Some tasks may be created for further updating their metadata / scheduling, e.g. "send reactions/comments count update", and the information itself may be taken from chat items.
