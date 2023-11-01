# Groups integrity

## Problems

- Inconsistency of group state:
  - group profile including group wide preferences,
  - list of members and their roles.
- Lack of group messages integrity - group member can send different messages to different members.

Lack of group consistency leads to group federation both in terms of members list and content visible to different members, which leads to user frustration and lack of trust.

Improvements to group design should provide:

- Consistent group state.
- Group messages integrity:
  - integrity violations (different message sent to different members) should be identified and shown to users,
  - missed messages should be requested to fill in gaps.

## Design ideas and questions

### Group messages integrity

A message container to include member's message ID (ordered?), and list of IDs and hashes of parent messages.

```haskell
data MsgParentId = MsgParentId
  { memberId :: MemberId,
    msgId :: Int64, -- sequential message ID for parent message (among memberId member messages)
    msgHash :: ByteString
  }

data MsgIds = MsgIds
  { msgId :: Int64, -- sequential message ID for member's message
    parentIds :: [MsgParentId]
  }
```

Questions:
  - What level of protocol should include MsgIds, and what messages should be included into integrity graph?
    - Having it on AppMessage level would allow to include all protocol messages. But some protocol messages are sent with different content per member (XGrpMemIntro, XGrpMemFwd, probe messages) and would have different hash. Also they contain sensitive data such as invitation links and should not be forwarded anyway.
    - If MsgIds is MsgContainer level, only XMsgNew would have it. This excludes other content messages such as updates, deletes, etc.
    - Include it into specific "content" chat events - XMsgNew, XMsgFileCancel (unused), XMsgUpdate, XMsgDel, XMsgReact, XFile (not used anymore but was never fully deprecated), XFileCancel.
    - Some new protocol level container, uniting above events?
  - Should msgId be sequential integer? (It leaks metadata about member's previous activity in the group) Can SharedMsgId be used instead?
  - Depending on number of parent messages, parentIds can become arbitrarily long and not fit into 16KB block, especially for messages containing profiles pictures.

When receiving a message with unknown parent identifiers, client should request missing messages from the sender by sending XGrpRequestSkipped, including last seen message reference for each missing parent. When receiving XGrpRequestSkipped, member should forward requested messages up to last seen parent using XGrpRequested.

```haskell
-- include received parentId?
XGrpRequestSkipped :: [MsgParentId] -> ChatMsgEvent 'Json

data MsgRequestedParent = MsgRequested
  { parentId :: MsgParentId,
    msg :: MsgContainer -- content TBD based on scope of messages included into integrity graph. Full event?
  }

XGrpRequested :: MsgRequestedParent -> ChatMsgEvent 'Json
```

Questions:
  - Depending on number of missing parents, XGrpRequestSkipped may not fit into 16KB block.
  - There may be multiple skipped messages for a given member, should they be sent sequentially from oldest (following the one known to requesting member) to newest?
  - XGrpRequested may not fit into 16KB block even if original MsgContainer / chat event did fit. On the other hand multiple XGrpRequested messages can be batched.
  - Malicious group member may arbitrarily request (at any time or in response to a new message) any number of skipped messages by sending parentIds from the past and trigger receiving member to send a lot of traffic. There are already some automatic response events in protocol, but they are harder to abuse: XGrpMemFwd - requires cooperation with other member, or creating connection; receipts - can be turned off; probes - requires member having matching contact and being non incognito in group. Should the member receiving XGrpRequestSkipped protect from such abuse by limiting number of requested messages? Limiting number or requests from a specific member in time?
  - By the time member requests skipped messages, sender may be offline. Should the requester send XGrpRequestSkipped to other members?
    - together with the request to sender or after some period?
    - to which members? - fraction of admins? all admins?
  - Member receiving XGrpRequestSkipped may not have requested messages, for example:
    - request is for the older parent id, and member never received it himself (was not part of the group then or has gap in place of this message), or has gap between sent message parent and requested parent.
    - member deleted parent(s), e.g. via periodic cleanup, or by deleting specific messages.
    - don't fully delete group message records while in group? instead only overwrite content?

Message integrity is computed for received messages, can be updated on receiving requested message parents.

```haskell
data GroupMsgIntegrity
  = GMIOk
  | GMISkippedParents {skippedParents :: [MsgParentId]}
  | GMIBadParentHash {knownParent :: MsgParentId, badParent :: MsgParentId} -- list?
```

```sql
CREATE TABLE message_integrity_records( -- message_hashes? group_messages?
  message_integrity_record_id INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES messages ON DELETE CASCADE, -- SET NULL?
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  member_id BLOB NOT NULL,
  member_msg_id INTEGER NOT NULL, -- shared_msg_id?
  msg_hash BLOB NOT NULL,
  msg_integrity TEXT NOT NULL, -- computed for received messages, for sent always Ok?
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

-- many to many table for message_integrity_records table
-- (parent can have multiple children, child can have multiple parents)
-- parent can be null if it wasn't received
CREATE TABLE message_parents(
  message_parent_id INTEGER PRIMARY KEY,
  message_integrity_record_id INTEGER NOT NULL REFERENCES message_integrity_record_id ON DELETE CASCADE,
  message_parent_integrity_record_id INTEGER REFERENCES message_integrity_record_id ON DELETE CASCADE,
  msg_parent_member_id BLOB NOT NULL,
  msg_parent_member_msg_id INTEGER NOT NULL,
  msg_hash BLOB NOT NULL,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);
```

How should message integrity errors be displayed in UI?
  - Displaying skipped parent errors would clutter UI due to delays in delivery. Probably they shouldn't be displayed.
  - Integrity violations (hashes not matching) should be displayed on respective chat items.
    - if integrity is on AppMessage level for all chat events - not all messages have corresponding chat items, create internal chat items?
    - if it's on the level of content messages, updates / etc. can be high above in message history, deletes can be not visible at all (full delete).
    - how to get reference to message via chat item when loading chat items? Integrity violation can be on a message different than chat item's created_by_msg_id message. For each chat item load integrity of all messages via chat_item_messages?
  - If integrity errors are only displayed on integrity violations, for malicious member to work around it and send different message to different group members could he specify unknown (far into future or past) message id, instead of incorrect one? Sender then wouldn't respond with skipped parents (and other members wouldn't be able to) - how to differentiate between this case and skipper parent error that is to be ignored in UI?
  - Should it be prohibited to not send MsgIds (to avoid message integrity check) if member protocol version supports it? Should it be prohibited at all and group with integrity be separated? How to distinguish between messages sent without integrity fields and messages with skipped parents in UI?
  - Not showing skipped parents integrity error in UI would lead user to believe integrity is preserved, and integrity violation can be revealed later. If conversation is time sensitive member may react to message considering it conversation integrity wasn't breached, and integrity violation may be revealed later. Having eventual integrity may not be better than having no integrity at all, and may even be worse because it produces false assumptions regarding conversation integrity. The goal can be narrowed to only restoring missed messages (gaps), without calculating integrity.

### Consistent group state

TODO
