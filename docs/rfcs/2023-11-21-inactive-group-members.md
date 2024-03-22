# Inactive group members

## Problem

Group traffic is higher than necessary due to lack of diagnosis of inactive group members. By inactive we understand group members who went offline for indefinitely long time, uninstalled application without leaving group, failed to send x.grp.leave message before deleting connection, or in any other way failed to explicitly communicate further inactivity.

Currently other group members continue to identify such members as active and to send messages to their connections until exceeding receiving SMP queues quotas, with pending messages being slowly retried even after that.

## Solution

Identify inactive members and don't send messages to their connections. Silent periodically online members should continue to receive messages, so decision to mark member as inactive should be made conservatively.

Agent:
- on SMP.QUOTA error notify client with ERR CONN QUOTA (new ConnectionErrorType QUOTA).
- on receiving QCONT notify client (new event).

Chat, on sending side, per member:
- unanswered_snd_msg_count - number of messages that were sent consecutively without receiving a message from member.
- last_rcv_ts - timestamp of last received message.
- inactive flag.
- set inactive if:
  - agent reports QUOTA error.
  - on sending message: (unanswered_snd_msg_count > K) && (last_rcv_ts earlier than Ddiff days ago), Ddiff = 1/2/3 days?
- reset inactive:
  - on receiving QCONT.
  - on receiving message or receipt. Also reset unanswered_snd_msg_count, last_rcv_ts.
- don't send to member if inactive.
  - don't send only content messages (x.msg.new, etc.) and always send messages altering group state?
- unanswered_snd_msg_count, last_rcv_ts to be tracked, checked, reset only for members with compatible version.

Chat, on receiving side, per member:
- unanswered_rcv_msg_count - number of messages that were received consecutively without sending a message to member.
- send non-optional receipt / another (new) protocol message if:
  - on receiving message: unanswered_rcv_msg_count > M, M < K.
- on sending a message or receipt to member reset unanswered_rcv_msg_count.
- unanswered_rcv_msg_count to be tracked, checked, reset only for members with compatible version.

\***

Consider above condition:

> (unanswered_snd_msg_count > K) && (last_rcv_ts earlier than Ddiff days ago)

It still doesn't account for following situation:

1. Sending member sends a few (N1, N1 < M) messages to silent member on day D1.
2. Sending member doesn't send messages for several days.
3. Sending member sends more messages (N2, N1 + N2 > K) to silent member on day DI (DI - D1 > diff in days in above condition), while silent member is offline.
  - Sending member checks above condition and evaluates it to be true, marks silent member as inactive.
  - Simply remembering last_snd_ts on sending side and adding check for it not being from several days ago to above condition is not enough, as it will be overwritten by current day sends and will only evaluate false for the first send. What could work is remembering prev_session_last_snd_ts or prev_day_last_snd_ts, but it further complicates logic, and still probably wouldn't account for some time zone differences.
4. Sending member sends yet more messages, which will not be queued for silent member marked inactive.
5. Silent member comes online, sends receipt upon receiving message fulfilling above condition: `unanswered_rcv_msg_count > M`, and will lose following messages.
  - If sending member created messages from 4 as pending, and sent them upon receiving receipt from silent member, silent member would only receive them after sending member coming online. If they are in different time zones it may happen on next day.

Same situation can occur even without step 1, simply by sending many messages while other member is offline.

The problem is less acute the greater the difference between K and M, but making K >> M renders this whole mechanism obsolete, as we could then simply rely on QUOTA errors to mark group members inactive (and don't slow retry in agent?).

Perhaps an acceptable way to solve this problem is to add a task to cleanup manager that would send receipts to all members on condition: (unanswered_rcv_msg_count > 0) && (last_reply_ts earlier than 1 day ago). (Adds last_reply_ts to tracking on receiving side). Perhaps it should be a task separate from cleanup manager that only occurs once per start, or with longer interval.

\***

Additionally we could consider group member connection as disabled with smaller AUTH error count. Currently it's 10 messages, could be 1.

### Delivery suspension notice

When receiving side comes back online, replies and continues to receive messages, it has no way of knowing there was a gap in messages from sending member. To notify receiving member about delivery suspension, sending member should send notice containing shared message id of the last sent message (new protocol event) to them:

```haskell
XGrpMemSuspended :: SharedMsgId -> ChatMsgEvent 'Json
```

Sending side additionally tracks:
- xgrpmemsuspended_sent flag - to only send it once.

When processing it, receiving member creates a "gap" chat item (e.g. event saying "member x suspended delivery to you due to your inactivity, there may be a gap in messages").

After receiving member signals activity by sending any reply, sending member may send message history before continuing normal delivery.

Starting point for message history: either receiving member could request history starting from specific shared message id (received in XGrpMemSuspended) with another new protocol event, or sending member can remember it instead of just flag.

### Sending message history

New protocol event:

```haskell
XGrpMsgHistory :: [ChatMessage 'Json] -> ChatMsgEvent 'Json
```

Sending member builds messages history starting starting from requested/remembered shared message id:
- `messages` table is periodically cleaned up, so messages would be retrieved from `chat_items`.
- if chat item for starting shared message id is not found (it may have been deleted manually or as a disappearing message), abort?
  - sending member could track number of skipped messages per member, but again if any chat items were deleted, older (previously successfully sent) chat items would be retrieved, resulting in duplicate messages. If receiving member has also cleaned up records in `messages` table, they wouldn't be deduplicated.
  - sending member could track timestamp of first unsent message instead of shared msg id.
- sending member should probably limit maximum number of messages sent as history (100?).
- only XMsgNew events should be sent in XGrpMsgHistory (chat items to be transformed back into text messages).
  - updates, deletions would be reflected in chat item list.
  - reactions would be omitted.
  - files would be likely expired by the time of sending history, so only file name and size may be sent in FileInvitation, with invitation being practically not acceptable.
    - add new flag to CIFile "expired" for receiving member to mark chat items created based on such invitations.
    - FileInvitation in MsgContainer could also contain this flag as optional to explicitly communicate that only file metadata is sent.
    - alternatively sending member could re-upload files, but this seems excessive.
  - XMsgNew events don't include message timestamps (instead usually broker ts is retrieved from agent message meta), so receiving member wouldn't be able to restore them from history. Perhaps history should include XGrpMsgForward events containing XMsgNew events instead.
- XGrpMsgHistory is likely to exceed message block limit.
  - either multiple messages comprising a history can be batched as a single message on chat level until the block size is exceeded.
  - or large history messages could be batched on agent level.

\***

Same XGrpMsgHistory protocol event could be sent by host to new members, after sending introductions.

---

Update 2024-02-12:

### Group "pings"

Alternatively to tracking unanswered messages counts per member, which is complex and in some cases as discussed above ineffective, group members could periodically send group wide pings indicating their active presence.

```haskell
XGrpPing :: ChatMsgEvent 'Json
```

Members track:

- inactive flag (as above - set on QUOTA errors as well)
- last_snd_ts on group
- last_rcv_ts on group member

Clients run a worker process for checking last_snd_ts in each of their groups, and send pings to groups on a periodic basis.

- part of cleanup manager or separate process?
- on each worker step, for each group matching criteria to send ping, send ping with a random delay to reduce correlation between groups (spawn a separate thread with a random delay for each group)
- criteria for sending ping: last_snd_ts earlier than group_ping_interval ago
- configure group_ping_interval to, for example, 23 hours (so that if user opens app each day at same time client will match criteria to send pings daily)

Clients receiving pings:

- update last_rcv_ts
- when sending a message to group, check only for timestamp difference (no unanswered snd msg count logic as above)
