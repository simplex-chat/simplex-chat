# Feed: a broadcast chat

A feed is a per-user chat. Each item in it is a broadcast: one message sent to
every contact and, for a business, to every customer group. Each recipient chat
holds an instance of the broadcast as an ordinary sent item, marked as a feed
message. Recipients see the message marked as a feed message and may drop feed
messages from a chat with a local flag. An edit or a deletion in the feed is
applied to every instance. Reactions received on instances are summed in the
feed item.

Delivery, edits, deletions and file descriptions are `delivery_jobs` rows
with a feed scope, processed by the existing delivery job worker
(`runDeliveryJobWorker`, `Subscriber.hs:4130-4327`) in buckets. Recipients
are read by cursor; the client never loads all contacts. Jobs are created
directly, without delivery tasks, as `leaveChannelRelay` does
(`Commands.hs:3200-3208`).

The TODO at `Commands.hs:2635` ("Broadcast rework") describes the shared
message id used here.

## Decisions

- One feed per user, created with the user (`feeds` row; the `note_folders`
  pattern).
- The feed contains sent items only. `CIDirection 'CTFeed` has the single
  constructor `CIFeedSnd`.
- One `SharedMsgId` per broadcast: the broadcast identity. The feed item and
  all instances store it. Every event of the broadcast (`XMsgNew`,
  `XMsgFileDescr` parts, `XMsgUpdate`, `XMsgDel`) is one `messages` row,
  encoded once with this id and linked to the feed item through
  `chat_item_messages`. Every recipient receives the same body
  (`VRValue (Just i)` / `VRRef i`, as `buildMsgReqs` at `Subscriber.hs:4244`).
  Bodies contain no `ttl`.
- The shared id lets recipients correlate the sender across recipients: two
  recipients comparing ids can establish a common sender. The UI shows a
  privacy notice in the feed chat before the first broadcast. Incognito
  connections and incognito memberships are excluded from recipients.
- Per-recipient rows: one `msg_deliveries` row (from `deliverMessagesB`) and
  one `chat_items` instance. Jobs and delivery events address instances by
  `chat_items.feed_item_id`: `msg_deliveries -> chat_item_messages -> feed
  item -> instance with feed_item_id = feed item AND the contact_id /
  group_id of the connection`.
- Instances are ordinary sent items of their chats: per-chat editing,
  deletion, reactions, expiration, disappearing messages.
  `chat_items.feed_item_id` (`ON DELETE SET NULL`) links an instance to the
  feed item and is the file join; `CIMeta.itemFeed` exposes it.
- A feed message takes the TTL of the chat on both sides: the sender's
  instance from `sndContactCITimed False ct Nothing` (`Internal.hs:174`), the
  recipient's item from the chat's own TTL in place of the absent message
  `ttl`.
- Deletion: `CIDMInternalMark` marks the feed item (`CIDeleted`) at command
  time. `CIDMInternal` and `CIDMBroadcast` set it to `CIDeleting`, a
  `CIDeleted 'CTFeed` state; the item stays in the feed until the job has
  processed every instance, and the job removes it at the end. `CIDeleting`
  rejects edits through `editable`; a delete of a `CIDeleting` item enqueues
  the job again (a retry after a failed job).
- Every job read is one range query per bucket over the cursor: contacts
  with their connection, their tags, customer groups, their members,
  instances with their chats. Every job write is one transaction per bucket.
  The worker never queries per recipient.
- Recipients: contacts passing `directOrUsed`, with a sendable connection
  (`contactSendConn_`) and a non-incognito connection (`contactConnIncognito`
  is False); groups with `business_chat = 'customer'` (the sender is the
  business), with non-incognito membership, a current and active membership,
  and their current members (the `getGroupRecipients` rule for the `Nothing`
  scope, `Internal.hs:1861-1865`, applied to batch-loaded members).
- Recipient chat setting `dropFeed`: a feed message (`feed = Just True`) for a
  chat with the flag set is acknowledged and discarded — no chat item, no
  event, no file transfer. A feed update for a missing item is discarded
  likewise. A delete for a missing item in such a chat is discarded without
  `CEvtChatItemDeletedNotFound`.
- Wire marker: `feed :: Maybe Bool` in `MsgContainer` and in `XMsgUpdate`,
  serialized only as `true`. Older clients ignore the field. The chat version
  is unchanged.
- Received feed messages are stored with `chat_items.item_feed = 1`;
  `CIMeta.itemFeed = Just CIFeedRcv`.
- Content: text, link, image, video, voice, file. One XFTP upload per
  broadcast; one recipient description for everyone. Quotes, mentions, live
  messages and the `ttl` parameter are rejected.
- Feed item status is set by the job: `CISSndNew` on creation,
  `CISSndSent SSPPartial` after the first bucket, `CISSndSent SSPComplete` on
  completion, `CISSndError` on job failure. Receipts are visible per instance.
- Feed item reactions are computed on read from received instance reactions
  (`reaction_sent = 0`) by `shared_msg_id`, with `userReacted = False`.
  Reactions sent from the feed are not supported.
- Commands: `/feed <text>` (CLI) and `/_feed <feedId> json|text ...` (API) send
  a broadcast. `%` is the feed chat type in `ChatRef` and `ChatName` contexts
  only; `SendRef` is unchanged.
- `CRBroadcastSent` is removed: the broadcast is a job, so counts are not
  known at command time. `/feed` returns `CRNewChatItems` with the feed item.
- Commands on the feed hold a `CLFeed` entity lock and only write the feed
  item and a job. The worker holds no entity locks (as relay job workers).
- Feed items are outside global chat item expiration (as notes).

## Types

`Types.hs`, after `NoteFolder` (:2208):

```haskell
data Feed = Feed
  { feedId :: FeedId,
    userId :: UserId,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    chatTs :: UTCTime,
    favorite :: Bool,
    unread :: Bool
  }
  deriving (Eq, Show)

type FeedId = Int64
```

`$(JQ.deriveJSON defaultJSON ''Feed)` next to `''NoteFolder` (:2411).

`ChatSettings` (`Types.hs:601`): field `dropFeed :: BoolDef`;
`defaultChatSettings` sets `BoolDef False`. `omittedField` of `BoolDef` keeps
older UI payloads valid.

`ContactOrGroup` (`Types.hs:591`): `CGFeed Feed`; `contactAndGroupIds`
returns `(Maybe ContactId, Maybe GroupId, Maybe FeedId)`.

`Messages.hs`:

- `ChatType` (:61): `CTFeed` after `CTLocal`; JSON tag `"feed"`.
  `chatTypeStr CTFeed = "%"` (:155).
- `SChatType` (:66): `SCTFeed :: SChatType 'CTFeed`; `TestEquality`,
  `ChatTypeI 'CTFeed`, `toChatType`, `aChatType` (:75-112).
- `ChatInfo` (:169): `FeedChat :: Feed -> ChatInfo 'CTFeed`.
  `chatInfoToRef` (:190): `FeedChat Feed {feedId} -> Just $ ChatRef CTFeed feedId Nothing`.
  `JSONChatInfo` (:204): `JCInfoFeed {feed :: Feed}`; `jsonChatInfo` (:229),
  `jsonAChatInfo` (:242).
- `CIDirection` (:293): `CIFeedSnd :: CIDirection 'CTFeed 'MDSnd`.
  `JSONCIDirection` (:308): `JCIFeedSnd`; `jsonCIDirection` (:318),
  `jsonACIDirection` (:328). `jsonACIQDirection` (:667):
  `JCIFeedSnd -> Left "unquotable"`.
- `ChatDirection` (:391): `CDFeedSnd :: Feed -> ChatDirection 'CTFeed 'MDSnd`;
  `toCIDirection` (:400), `toChatInfo` (:410).
- `ChatTypeQuotable 'CTFeed` resolves to the existing `TypeError` case (:646).
- `deletable'` (:542): the default branch applies to `SCTFeed`.
- `CIDeleted` (:1282): `CIDeleting :: Maybe UTCTime -> CIDeleted 'CTFeed`;
  `JSONCIDeleted` (:1292): `JCIDDeleting {deletedTs}`; `jsonCIDeleted`,
  `jsonACIDeleted`, `itemDeletedTs` (:1299-1318). `deletable'` and
  `editable` (`mkCIMeta` :538) are False for it through `isNothing
  itemDeleted`. Stored as `item_deleted = 4` (`DBCIDeleting`, next to
  `DBCIBlockedByAdmin` at `Store/Messages.hs:2942`). `chatItemDeletedText`
  (`View.hs:624-628`) matches every constructor and gains
  `CIDeleting _ -> "deleting"`.
- `ConnOrGroupId` (:1190): `FeedId Int64`, the entity of feed messages
  (`messages.feed_id`). `createNewRcvMessage` (`Store/Messages.hs:309-317`)
  matches the constructors and gains a `FeedId` branch returning
  `SEInternalError`: received messages have a connection or a group.
- `CIMeta` (:509): field `itemFeed :: Maybe CIFeed` after `msgVerified`;
  `mkCIMeta` (:535) gains the parameter; `dummyMeta` (:551) sets `Nothing`.

  ```haskell
  data CIFeed
    = CIFeedSnd {feedItemId :: Maybe ChatItemId}
    | CIFeedRcv
  ```

  JSON `sumTypeJSON $ dropPrefix "CIFeed"`. Stored as
  `chat_items.item_feed` (1 for both) and `chat_items.feed_item_id`
  (`CIFeedSnd` only; `Nothing` after the feed item is removed or the feed is
  cleared). Row mapping: `item_feed = 1` with `item_sent = 1` ->
  `CIFeedSnd feed_item_id`; with `item_sent = 0` -> `CIFeedRcv`.

`Protocol.hs`:

- `MsgContainer` (:678): `feed :: Maybe Bool` after `forwardLink`; `mcSimple`
  (:705) sets `Nothing`; the sender sets `Just True`.
- `XMsgUpdate` (:450): field `feed :: Maybe Bool`; parser (:1403) reads
  `opt "feed"`; encoder (:1486) adds `("feed" .=? feed)`. Positional
  patterns and constructions gain the argument: `Subscriber.hs:554`, `:730`,
  `:1039`, `:1286`, `:3882`; `Commands.hs:780`, `:814`, `:1443`.
- `cmFeed :: AChatMsgEvent -> Bool` next to `cmToQuotedMsg` (:622):
  `ACME _ (XMsgNew MsgContainer {feed = Just True}) -> True`.

`Controller.hs`:

- `ChatCommand`: `APISendFeedMessage {feedId :: FeedId, composedMessage :: ComposedMessage}`;
  `SetDropFeed ChatName Bool`. `SendMessageBroadcast MsgContent` stays as the
  CLI command. `CRBroadcastSent` (:824) is removed.
- `ChatConfig` (:141): `feedBucketSize :: Int` (1000 in `Chat.hs`).
  `deliveryJobWorkers` (:309) serves feed jobs; no new worker map.

`Delivery.hs`, extending the existing scopes (the intent recorded at
`M20250813_delivery_tasks.hs:45`):

```haskell
data DeliveryEntity = DEGroup GroupId | DEFeed FeedId
  deriving (Eq, Ord, Show)

type DeliveryWorkerKey = (DeliveryEntity, DeliveryWorkerScope)

data DeliveryWorkerScope = DWSGroup | DWSMemberSupport | DWSFeed

data DeliveryJobScope
  = DJSGroup {jobSpec :: DeliveryJobSpec}
  | DJSMemberSupport {supportGMId :: GroupMemberId}
  | DJSFeed {feedItemId :: ChatItemId, feedJobSpec :: FeedJobSpec}

data DeliveryJobSpec
  = DJDeliveryJob {includePending :: Bool}
  | DJRelayRemoved
  | DJFeed FeedJobSpec

data FeedJobSpec
  = FJNew
  | FJFileDescr
  | FJUpdate
  | FJDelete CIDeleteMode
```

- `toWorkerScope (DJSFeed {}) = DWSFeed`; `jobScopeImpliedSpec (DJSFeed
  {feedJobSpec}) = DJFeed feedJobSpec` (:96); `jobSpecImpliedPending (DJFeed
  _) = False` (:101); `DeliveryJobSpecTag` (:65) gains `DJSTFeedNew`,
  `DJSTFeedFileDescr`, `DJSTFeedUpdate`, `DJSTFeedDelete` for
  `job_scope_spec_tag`; `DWSFeed` encodes as `"feed"` (:33).
- `MessageDeliveryJob` (:161) gains `messageIds :: [MessageId]`,
  `cursorContactId_ :: Maybe ContactId`, `cursorGroupId_ :: Maybe GroupId`;
  `senderGMIds` is `[]` and `cursorGMId_` is `Nothing` for feed jobs.
- `DeliveryWorkerKey` construction sites: `Subscriber.hs:1125`, `:1130`,
  `:4056`, `:4143`; `Commands.hs:3208` (`DEGroup gId`).
- The task worker's `case jobScopeImpliedSpec jobScope of`
  (`Subscriber.hs:4067`) gains `DJFeed _ -> throwChatError $ CEInternalError
  "delivery task worker: feed spec"` — tasks are group-scoped.

`Store/Shared.hs`:

- `ChatLockEntity` (:68): `CLFeed FeedId`; `withFeedLock` next to
  `withContactLock` (`Internal.hs:142`); the lock name in `DebugLocks`
  (`Commands.hs:3652`).
- `StoreError` (:113): `SEFeedAlreadyExists {feedId :: FeedId}`,
  `SEFeedNotFound {feedId :: FeedId}`, `SEUserFeedNotFound`. `View.hs:2835`:
  `SEUserFeedNotFound -> ["no feed"]`. Job errors reuse
  `SEDeliveryJobNotFound` and `SEInvalidDeliveryJob`.

## Schema

SQLite `M20260904_feeds`; Postgres with `BIGINT GENERATED ALWAYS AS IDENTITY`,
`TIMESTAMPTZ`, `SMALLINT` and named constraints, as `M20241220_initial.hs:619-635`.

```sql
CREATE TABLE feeds(
  feed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  chat_ts TEXT NOT NULL DEFAULT(datetime('now')),
  favorite INTEGER NOT NULL DEFAULT 0,
  unread_chat INTEGER NOT NULL DEFAULT 0
) STRICT;
PRAGMA writable_schema=1;
UPDATE sqlite_master
SET sql = replace(sql, 'group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE', 'group_id INTEGER REFERENCES groups ON DELETE CASCADE')
WHERE type = 'table' AND name = 'delivery_jobs';
PRAGMA writable_schema=RESET;
ALTER TABLE delivery_jobs ADD COLUMN feed_id INTEGER REFERENCES feeds ON DELETE CASCADE;
ALTER TABLE delivery_jobs ADD COLUMN chat_item_id INTEGER REFERENCES chat_items ON DELETE CASCADE;
ALTER TABLE delivery_jobs ADD COLUMN delete_mode TEXT;
ALTER TABLE delivery_jobs ADD COLUMN message_ids TEXT;
ALTER TABLE delivery_jobs ADD COLUMN cursor_contact_id INTEGER;
ALTER TABLE delivery_jobs ADD COLUMN cursor_group_id INTEGER;
CREATE INDEX idx_delivery_jobs_feed_next ON delivery_jobs(feed_id, worker_scope, failed, job_status);
CREATE INDEX idx_delivery_jobs_chat_item_id ON delivery_jobs(chat_item_id);
ALTER TABLE chat_items ADD COLUMN feed_id INTEGER DEFAULT NULL REFERENCES feeds ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN feed_item_id INTEGER DEFAULT NULL REFERENCES chat_items ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN item_feed INTEGER NOT NULL DEFAULT 0;
ALTER TABLE messages ADD COLUMN feed_id INTEGER DEFAULT NULL REFERENCES feeds ON DELETE CASCADE;
ALTER TABLE files ADD COLUMN feed_id INTEGER DEFAULT NULL REFERENCES feeds ON DELETE CASCADE;
ALTER TABLE contacts ADD COLUMN drop_feed INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN drop_feed INTEGER NOT NULL DEFAULT 0;
CREATE INDEX idx_feeds_user_id ON feeds(user_id);
CREATE INDEX idx_chat_items_feed_id ON chat_items(feed_id);
CREATE INDEX idx_chat_items_feeds_created_at ON chat_items(user_id, feed_id, created_at);
CREATE INDEX idx_chat_items_feed_item_contact ON chat_items(feed_item_id, contact_id);
CREATE INDEX idx_chat_items_feed_item_group ON chat_items(feed_item_id, group_id);
CREATE INDEX idx_messages_feed_id ON messages(feed_id);
CREATE INDEX idx_files_feed_id ON files(feed_id);
INSERT INTO feeds (user_id) SELECT user_id FROM users;
```

- `feed_item_id` self-reference precedent: `fwd_from_chat_item_id`
  (`chat_schema.sql:504`).
- `delivery_jobs.group_id` becomes nullable through the `sqlite_master`
  edit of `M20251230_strict_tables.hs:18-26`; Postgres uses
  `ALTER COLUMN group_id DROP NOT NULL`. A feed job has `feed_id`,
  `chat_item_id` (the feed item), `message_ids` and the feed cursors; a
  group job has `group_id` and `cursor_group_member_id`.
- `message_ids`: comma-separated decimal ids, the encoding of
  `delivery_jobs.sender_group_member_ids` (`Store/Delivery.hs:266-270, :329`).
- Registration: `SQLite/Migrations.hs`, `Postgres/Migrations.hs`, the module
  list in `simplex-chat.cabal` (:85), regenerated `chat_schema.sql` dumps, the
  down-migration round-trip.

## Store

New module `Store/Feeds.hs`:

- Feed entity, the shape of `Store/NoteFolders.hs`: `createFeed`,
  `getUserFeedId`, `getFeed`, `updateFeedUnreadChat`, `deleteFeedCIs`
  (`DELETE FROM messages WHERE feed_id = ?` first — `deleteContactCIs`
  deletes messages by connection at `Store/Messages.hs:202` — then the
  items; pending group messages of the feed cascade with the messages).
- Bucket readers, one range query each over the cursor (`id > ? ORDER BY id LIMIT ?`):
  - `getFeedContactsByCursor db cxt user cursor_ count :: IO [Contact]` —
    the `getContact` SELECT (`Store/Direct.hs:973-991`) with
    `WHERE ct.user_id = ? AND ct.deleted = 0 AND ct.is_user = 0 AND ct.contact_id > ? ORDER BY ct.contact_id LIMIT ?`;
    one row per contact (`idx_connections_contact_id` is UNIQUE,
    `chat_schema.sql:1304`). The SELECT list and joins of `getContact` are
    extracted into `contactQueryFields` / `contactQueryFrom` in
    `Store/Shared.hs`, the shape of `groupInfoQueryFields` /
    `groupInfoQueryFrom` (:796-826); `getContact` (`Store/Direct.hs:975`),
    the identical lists at `Store/Direct.hs:324` and
    `Store/ContactRequest.hs:117` use them.
  - `getContactsTagsByRange db user fromId toId :: IO (Map ContactId [ChatTagId])` —
    `chat_tags_chats JOIN contacts USING (contact_id) WHERE user_id = ? AND contact_id > ? AND contact_id <= ?`;
    `toContact` receives the contact's tags from the map.
  - `getFeedCustomerGroupsByCursor db cxt user cursor_ count :: IO [GroupInfo]` —
    `groupInfoQuery <> " WHERE g.user_id = ? AND mu.contact_id = ? AND g.business_chat = ? AND g.group_id > ? ORDER BY g.group_id LIMIT ?"`
    (`BCCustomer`); `getGroupsTagsByRange` as for contacts.
  - `getCustomerGroupsMembersByRange db cxt user fromId toId :: IO (Map GroupId [GroupMember])` —
    `groupMemberQuery <> " JOIN groups g ON g.group_id = m.group_id WHERE m.user_id = ? AND g.business_chat = ? AND m.group_id > ? AND m.group_id <= ? AND (m.contact_id IS NULL OR m.contact_id != ?)"`
    (the `getGroupMembers` condition, `Store/Groups.hs:1236`;
    `idx_group_members_group_id (user_id, group_id)`); the `groups` join
    keeps members of other groups in the id range out.
  - `getFeedContactInstancesByCursor db cxt user feedItemId cursor_ count :: IO [(Contact, CChatItem 'CTDirect)]` —
    the `getDirectChatItem` SELECT (`Store/Messages.hs:2700-2716`) composed
    with `contactQueryFields` (`:.` rows), `FROM chat_items i JOIN contacts ct ON ct.contact_id = i.contact_id`
    + `contactQueryFrom` joins,
    `WHERE i.user_id = ? AND i.feed_item_id = ? AND i.contact_id > ? ORDER BY i.contact_id, c.connection_id LIMIT ?`
    (`idx_chat_items_feed_item_contact`).
  - `getFeedGroupInstancesByCursor db cxt user feedItemId cursor_ count :: IO [(GroupInfo, CChatItem 'CTGroup)]` —
    the `getGroupChatItem` SELECT (:3094-3139) composed with
    `groupInfoQueryFields`, `WHERE i.user_id = ? AND i.feed_item_id = ? AND i.group_id > ? ORDER BY i.group_id LIMIT ?`
    (`idx_chat_items_feed_item_group`); members from `getCustomerGroupsMembersByRange`.
  - `getFeedInstanceContactIdsByRange db user feedItemId fromId toId :: IO [ContactId]`
    (and groups) — the instance guard of a repeated `FJNew` bucket.
  - `getDeliveredContactIdsByRange db msgId fromId toId :: IO [ContactId]` —
    `SELECT c.contact_id FROM msg_deliveries d JOIN connections c ON c.connection_id = d.connection_id WHERE d.message_id = ? AND c.contact_id > ? AND c.contact_id <= ?`
    (`idx_msg_deliveries_message_id`, `chat_schema.sql:1085`); the group
    variant joins `group_members gm ON gm.group_member_id = c.group_member_id`
    and returns `gm.group_id` — the delivery guard of a repeated bucket for
    every job type.
`Store/Delivery.hs`, parameterized by the entity:

- `DeliveryJobScopeRow` (:62) gains `(Maybe ChatItemId, Maybe CIDeleteMode)`
  from `chat_item_id`, `delete_mode`; `jobScopeRow_` (:64) and
  `toJobScope_` (:71) map `DJSFeed`; `createMsgDeliveryTask` (:78) writes
  the first four fields (tasks are group-scoped).
- `createMsgDeliveryJob` (:251): `DeliveryEntity` in place of `GroupInfo`,
  and `[MessageId]`; writes `group_id` or `feed_id`, `chat_item_id` from
  the scope, `message_ids`. Callers: `Subscriber.hs:4077`, `:4090`,
  `Commands.hs:3207` (`DEGroup`, `[]`).
- `getPendingDeliveryJobScopes` (:272): `SELECT DISTINCT group_id, feed_id, worker_scope`
  mapped to the key. `getNextDeliveryJob` (:285): the key condition is
  `group_id = ?` or `feed_id = ?` by entity
  (`idx_delivery_jobs_next`, `idx_delivery_jobs_feed_next`); the row adds
  `message_ids`, `cursor_contact_id`, `cursor_group_id`.
- `updateDeliveryJobFeedCursor db jobId contactId_ groupId_` next to
  `updateDeliveryJobCursor` (:392).
- `deleteGroupDeliveryJobs` (:100), `deleteDoneDeliveryJobs` (:400),
  `updateDeliveryJobStatus` (:337), `setDeliveryJobErrStatus` (:340):
  unchanged.
- Bucket writers, one statement or one `executeMany` each, in one
  transaction per bucket: `updateFeedInstances` (`item_content`, `item_text`,
  `item_edited = 1`, `has_link`, `updated_at` for
  `user_id = ? AND feed_item_id = ? AND contact_id > ? AND contact_id <= ?`,
  and the group range), `deleteFeedInstances` and `markFeedInstancesDeleted`
  by `executeMany` over the ids of each subset (the full-delete split is
  decided in Haskell from `mergedPreferences`), reaction deletion by
  `executeMany` over `(contact_id, shared_msg_id)`, instance statuses by
  `executeMany`, `createPendingGroupMessage` and `createGroupSndStatus` by
  `executeMany`.
- `getFeedCIReactions db user sharedMsgId :: IO [CIReactionCount]`:

  ```sql
  SELECT r.reaction, COUNT(1)
  FROM chat_item_reactions r
  LEFT JOIN contacts ct ON ct.contact_id = r.contact_id
  LEFT JOIN groups g ON g.group_id = r.group_id
  LEFT JOIN group_members mu ON mu.group_id = r.group_id AND mu.member_category = ?
  WHERE r.shared_msg_id = ? AND r.reaction_sent = 0
    AND ((r.group_id IS NULL AND ct.user_id = ?) OR (g.user_id = ? AND r.item_member_id = mu.member_id))
  GROUP BY r.reaction
  ```

  `mu` is the membership (`GCUserMember`): reactions to the user's own group
  items are stored with `item_member_id` equal to the membership id
  (`chatItemMember`, `Messages.hs:369`; `setGroupReaction`, `Store/Messages.hs:3500`).
  The query uses `idx_chat_item_reactions_shared_msg_id`.

`Store/Profiles.hs`: `getUserByFeedId` next to `getUserByNoteFolderId` (:269).

`Store/Direct.hs:1080` `updateContactSettings` and `Store/Groups.hs:3169`
`updateGroupSettings` write `drop_feed`. `ContactRow'` (`Store/Shared.hs:491`)
and `GroupInfoRow` (:688) gain the column, read into `ChatSettings` at
`Store/Shared.hs:501`, `:699` and `Store/Connections.hs:130`; the SELECT
lists producing these rows add it: `Store/Direct.hs:324`, `:975`,
`Store/ContactRequest.hs:117`, `Store/Connections.hs:115`, `:150`,
`Store/Shared.hs:803`. `ChatSettings` constructions at
`Store/Groups.hs:424`, `:503` set `BoolDef False`.

`Store/Files.hs`: `createSndFileTransferXFTP` (:182) writes `feed_id` from
`contactAndGroupIds`; `toFileRef` (:295) and both callers' SELECT lists
(:276, :288) add `feed_id` -> `ChatRef CTFeed`. `getFeedFileInfo` and
`deleteFeedFiles` by `chat_item_id IN (SELECT chat_item_id FROM chat_items WHERE feed_id = ?)`,
the `note_folders` shape (`Store/NoteFolders.hs:61`).

`Store/Messages.hs`:

- `createNewSndMessage` (:237): parameter `Maybe SharedMsgId` after
  `connOrGroupId`. `Just smId` inserts with it; `Nothing` keeps
  `createWithRandomId'`. `SharedMsgId` has a `ToField` instance
  (`Types.hs:256`). `FeedId feedId` writes `messages.feed_id`.
- `createNewChatItem_` (:590): parameter `Maybe CIFeed`; `idsRow` gains
  `Maybe FeedId` from `CDFeedSnd Feed {feedId}`; the INSERT adds `feed_id`,
  `feed_item_id`, `item_feed`. `createNewSndChatItem` (:548) passes the
  parameter through; `createNewRcvChatItem` (:564) passes
  `if cmFeed chatMsgEvent then Just CIFeedRcv else Nothing`;
  `createNewChatItemNoMsg` (:583) and `createLocalChatItems`
  (`Internal.hs:3185`) pass `Nothing`.
- `ChatItemModeRow` (:2281) gains `(BoolInt, Maybe ChatItemId)` from
  `i.item_feed, i.feed_item_id`; `toLocalChatItem` (:1096),
  `toDirectChatItem` (:2307), `toGroupChatItem` (:2375) build `Maybe CIFeed`
  and pass it to `mkCIMeta`; SELECT lists at :2706, :3100, :3213.
- The file join in `getDirectChatItem` (:2712) and `getGroupChatItem`
  (:3129) becomes
  `LEFT JOIN files f ON f.chat_item_id = COALESCE(i.feed_item_id, i.chat_item_id)`:
  an instance renders the feed item's single file.
- `getChatItemIdsByAgentMsgId` (:2517) gains a second id source, resolved
  through the delivery's connection, so all three callers are covered:
  `getDirectChatItemsByAgentMsgId` (:2672), `getGroupChatItemsByAgentMsgId`
  (:3080) and `updateGroupItemsErrorStatus` (`Subscriber.hs:1271-1274`,
  the `MWARN`/`MERR`/`MERRS` path of group connections, which calls it
  directly):

  ```sql
  SELECT cim.chat_item_id
  FROM msg_deliveries d
  JOIN chat_item_messages cim ON cim.message_id = d.message_id
  WHERE d.connection_id = ? AND d.agent_msg_id = ?
  UNION
  SELECT i.chat_item_id
  FROM msg_deliveries d
  JOIN chat_item_messages cim ON cim.message_id = d.message_id
  JOIN connections c ON c.connection_id = d.connection_id
  LEFT JOIN group_members gm ON gm.group_member_id = c.group_member_id
  JOIN chat_items i ON i.feed_item_id = cim.chat_item_id
    AND ((c.contact_id IS NOT NULL AND i.contact_id = c.contact_id)
      OR (gm.group_id IS NOT NULL AND i.group_id = gm.group_id))
  WHERE d.connection_id = ? AND d.agent_msg_id = ?
  ```

  (`idx_chat_items_feed_item_contact`, `idx_chat_items_feed_item_group`).
  The feed item id from the first source is dropped by the callers: a
  direct or group item lookup with the feed item id returns nothing
  (`eitherToMaybe` at :2675, :3083) and `updateGroupMemSndStatus'`
  (`Subscriber.hs:3977`) returns `False` without a status row. `SENT`,
  `RCVD`, `MWARN`, `MERR` then update instance statuses through the existing
  `updateDirectItemsStatus'` (:3964) and `updateGroupItemsStatus` (:3986).
- `updateChatTsStats` (:409): `FeedChat` branch updating `feeds.chat_ts`.
- `getChatPreviews` (:746): `FeedChatPD`, `findFeedChatPreviews_`,
  `getFeedChatPreview_` — copies of the local trio (:1009-1092) over `feeds`.
- `getChatContentTypes` (:1222): `CTFeed -> getTypes " feed_id = ? " ()`.
- `getChatItemIDs` (:1531): `FeedChat Feed {feedId}` branch,
  `" user_id = ? AND feed_id = ? "`, ordered by `created_at`.
- `getFeedChat`: the shape of `getLocalChat` (:1826-1924); `CPInitial`
  returns the last items with `NavigationInfo 0 0`.
- `getFeedChatItem db user feedId itemId`: the row query of
  `getLocalChatItem` (:3198) keyed by `feed_id`; `toFeedChatItem` accepts
  `SMDSnd` rows only and maps `item_deleted` 1 to `CIDeleted` and 4 to
  `CIDeleting`; `reactions` from `getFeedCIReactions` by the item's
  `shared_msg_id`. `safeGetFeedItem`, `safeToFeedItem` as for local items.
- `toChatItemRef` (:2009): 7-tuple with `feed_id` -> `ChatRef CTFeed`; SELECT
  lists at :2460, :2471, :2484, :2496, :3292, :3314.
- `getAChatItem` (:3334): `CTFeed` branch.
- `updateFeedChatItem'`, `updateFeedChatItemStatus`, `deleteFeedChatItem`,
  `markFeedChatItemDeleted` (with the `CIDeleted 'CTFeed` value to set:
  `CIDeleted` or `CIDeleting`), `getFeedChatItemIdByText`,
  `getFeedChatItemIdByText'`: copies of the local and direct functions
  (:3222-3283, :2533, :2653) keyed by `feed_id`. `deleteFeedChatItem` also
  runs `deleteChatItemMessages_` and `deleteChatItemVersions_` (:2635, :2649).

## Sending

`Internal.hs`:

- `createSndMessages` (:2237) becomes `createSndMessages_ Nothing`;
  `createFeedSndMessages smId = createSndMessages_ (Just smId)`.
- `sendGroupSignedMessages_` (:2531): the shuffle and the `memberSendAction`
  fold (`addMember`, :2560-2571) are extracted as
  `groupSendActions :: GroupInfo -> NonEmpty (ChatMsgEvent e) -> [GroupMember] -> IO ([(GroupMember, Connection)], [GroupMember], [GroupMember])`
  (to send, pending, forwarded), and `foldMembers` of `prepareMsgReqs`
  (:2580-2597, the `VRValue (Just i)` / `VRRef i` body-sharing fold) as
  `sharedBodyReqs :: MsgFlags -> NonEmpty (Either ChatError MsgBatch) -> [(r, Connection)] -> ([r], [Either ChatError ChatMsgReq])`,
  generic in the recipient; `sendGroupSignedMessages_` calls both and is
  otherwise unchanged. The feed job uses `groupSendActions` per group and
  `sharedBodyReqs` once per bucket over contacts and members together.
- `createMemberSndStatuses` (`Commands.hs:4866`) moves to the top level of
  `Internal.hs` unchanged.
- `mkChatItem_` (:2859) gains the `Maybe CIFeed` parameter for `mkCIMeta`;
  `mkChatItem` (:2853), `saveRcvChatItem'` (:2825) and
  `saveSndChatItems.createItem` (:2789) pass through (`NewSndChatItemData`
  :2761 gains `itemFeed :: Maybe CIFeed`, `Nothing` at existing sites).
- The file-info lambda repeated in `deleteDirectCIs` (:521),
  `deleteGroupCIs` (:533), `deleteLocalCIs` (:597), `markDirectCIsDeleted`
  (:616), `markGroupCIsDeleted` (:628) becomes `itemsFilesInfo`, which skips
  items with `itemFeed = Just (CIFeedSnd _)`: an instance's file is the feed
  item's file and is not cancelled or deleted with the instance.

`Commands.hs`, `APISendFeedMessage feedId cm`, under `withFeedLock "sendFeed" feedId`:

1. `assertAllowedContent'`, `assertNoMentions`; `quotedItemId` must be absent.
2. `feed <- getFeed`; `sharedMsgId <- getSharedMsgId` (:5030); `createdAt`.
3. File: `checkSndFile`; `xftpSndFileTransfer_ user file fileSize 1 (Just $ CGFeed feed)`
   (`Internal.hs:438`; `roundedFDCount 1` yields 4 descriptions, the first is
   used); `xftpSndFileTransfer` (:4911) adds `CGFeed _ -> pure ()` — no
   `snd_files` rows.
4. Feed item: `updateChatTsStats db cxt user (CDFeedSnd feed) createdAt Nothing`;
   `createNewChatItemNoMsg db user (CDFeedSnd feed) False (CISndMsgContent mc) (Just sharedMsgId) hasLink Nothing createdAt createdAt`;
   `updateFileTransferChatItemId` for the file.
5. Message: `createFeedSndMessages sharedMsgId (Identity (FeedId feedId, Nothing, XMsgNew (mcSimple mc) {file = fInv_, feed = Just True}))`
   (`fInv_ :: Maybe FileInvitation` from step 3);
   `insertChatItemMessage_ db feedItemId msgId createdAt`
   (`Store/Messages.hs:669`, added to the module's export list).
6. Job: `createMsgDeliveryJob db (DEFeed feedId) (DJSFeed feedItemId FJNew) [] [msgId] msgBody`;
   `getDeliveryJobWorker True (DEFeed feedId, DWSFeed)` — the shape of
   `leaveChannelRelay` (`Commands.hs:3204-3208`).
7. Response `CRNewChatItems user [feedItem]` (`CISSndNew`).

`SendMessageBroadcast mc` (:2625): `feedId <- getUserFeedId`;
`APISendFeedMessage feedId (composedMessage Nothing mc)`.

## Delivery worker

`Subscriber.hs`, `runDeliveryJobWorker` (:4130): the worker reads its
entity once — `(user, gInfo)` by `groupId` as today for `DEGroup`, `user`
by `getUserByFeedId` and the feed for `DEFeed` — and `processDeliveryJob`
(:4152) gains the branch `DJFeed spec -> processFeedJob user feed job spec`
in its `case jobScopeImpliedSpec jobScope of`. Startup
(`startDeliveryJobWorkers`, :4094), the `withWork_` loop, the error path
(:4148, `setDeliveryJobErrStatus`) and cleanup (`cleanupDeliveryJobs`,
`Commands.hs:5315`) are unchanged; feed keys come out of
`getPendingDeliveryJobScopes` with the group keys.

`processFeedJob` uses the job's `body` and `messageIds`, then
runs the contact loop and the group loop from the persisted cursors,
`feedBucketSize` recipients per bucket. Each bucket begins by re-reading the
feed item (`getFeedChatItem`). `FJNew`, `FJFileDescr` and `FJUpdate` stop
when the item is absent (removed by `APIClearChat`, which cascaded the job
row) or has `itemDeleted` set (a delete was issued; the `FJDelete` job that
follows handles the instances created so far); `FJDelete` stops only when
the item is absent.

A failing job is set to `DJSError` by the existing error path and, for
`FJNew`, the feed item status to `CISSndError (SndErrOther err)` in the feed
branch; jobs are not retried, as relay jobs are not; the user sends or
deletes again.

Bodies. One `body` per job, stored at creation as for group jobs
(`createMsgDeliveryJob`): `msg_body` of the message for `FJNew`, `FJUpdate`
and `FJDelete`; one job per JSON batch of the description parts
(`batchSndMessagesJSON BMJson`, `maxEncodedMsgLength`) for `FJFileDescr`,
with the batch's `message_ids`; `FJDelete CIDMInternal` and `FJDelete
CIDMInternalMark` have an empty body and deliver nothing. Within a bucket
the first request of the body sends `VRValue (Just 1) body`, the rest
`VRRef 1` (`sharedBodyReqs`). Requests of one connection are adjacent
(`toAgent`, `Internal.hs:2374`). `deliverMessagesB` compresses a body above
`maxCompressedMsgLength` when any connection of the batch has PQ
(`compressBodies`, `Internal.hs:2365`); the compressed body is then shared
by every connection, which every peer decodes: `initialChatVersion` is 9
(`Types.hs:2253`) and compression arrived in version 8 (`Protocol.hs:78`).

Guards for a bucket repeated after a restart: instances are not created
for contacts in `getFeedInstanceContactIdsByRange`; requests are not built
for connections in `getDeliveredContactIdsByRange` (a `msg_deliveries` row
exists once the agent accepted the message); the cursor is written last.
A crash between instance creation and delivery is therefore resumed by
delivery, and a crash between delivery and the cursor sends nothing twice.

Contact bucket, one range read: `FJNew` reads
`getFeedContactsByCursor` and `getContactsTagsByRange`; the other types read
`getFeedContactInstancesByCursor` (contact and instance together) and the
tags. Then, in memory:

1. Eligible for `FJNew`: `directOrUsed`, not `contactConnIncognito`,
   `contactSendConn_` returns a connection. For the other types every
   loaded instance is changed locally; the message is delivered only to
   contacts whose `contactSendConn_` returns a connection.
2. `timed_ = sndContactCITimed False ct Nothing` (`Internal.hs:174`): the
   chat's own TTL, with nothing in the body.
3. `FJNew`: instances first, one transaction: contacts in
   `getFeedInstanceContactIdsByRange` are skipped; for the rest
   `updateChatTsStats` and `createNewChatItem_` with `CDDirectSnd ct`, no
   message id, the shared id, `CISndMsgContent` from the container,
   `Just (CIFeedSnd (Just feedItemId))`, `timed_`; the items are built with
   `mkChatItem_`.
4. Delivery: one `deliverMessagesB` over
   `(conn, MsgFlags {notification = hasNotification tag}, (vor, messageIds))`
   for the connections of the bucket outside `getDeliveredContactIdsByRange`;
   a `Left` result for `FJNew` sets the instance to `CISSndError` (statuses
   by `executeMany`); `createContactPQSndItem` for contacts whose
   `pqSndEnabled` changed, as `sendDirectContactMessages`
   (`Internal.hs:2170`).
5. `FJUpdate`: `updateFeedInstances` for the bucket range; the loaded
   instances are updated in memory with `updatedChatItem`
   (`Store/Messages.hs:2564`) and emitted as `CEvtChatItemUpdated`.
   `FJDelete mode`: `CIDMBroadcast` delivers `XMsgDel`, then deletes the
   instances of contacts with `featureAllowed SCFFullDelete forUser ct` and
   marks the others (the rule of `APIDeleteChatItem`, `Commands.hs:857`);
   `CIDMInternal` deletes; `CIDMInternalMark` marks; marked items are
   updated in memory as `markDirectChatItemDeleted` does (:2665); the
   deletions are emitted as `CEvtChatItemsDeleted` with `byUser = True`.
   `FJFileDescr`: no instance change.
6. Timed instances: `startProximateTimedItemThread`.
7. `FJNew`: `CEvtNewChatItems user instances`.
8. `updateDeliveryJobFeedCursor` with the last contact id read; after the
   first bucket of `FJNew` the feed item status becomes
   `CISSndSent SSPPartial` (`updateFeedChatItemStatus`,
   `CEvtChatItemsStatusesUpdated`).
9. Repeat while the bucket is full.

Group bucket, after the contact loop, two range reads: `FJNew` reads
`getFeedCustomerGroupsByCursor`; the other types read
`getFeedGroupInstancesByCursor`; both read `getCustomerGroupsMembersByRange` and
the tags. Then, in memory, per group: skip `incognitoMembership`; require
`memberCurrent membership && memberActive membership` and take
`filter memberCurrent members` (the `getGroupRecipients` rule for
`Nothing` scope, `Internal.hs:1861-1865`); `groupSendActions gInfo events
members` gives the members to send to, to keep pending and forwarded. The
bucket's request list holds the broadcast body (shared as above) per member
connection to send to. One `deliverMessagesB` for the bucket;
`createPendingGroupMessage` by `executeMany` for pending members (the
message row is the feed's; `sendPendingGroupMessages`, `Internal.hs:2683`,
delivers it on connection); `FJNew`: instances with `CDGroupSnd g Nothing`,
`updateChatTsStats` and `timed_ = sndGroupCITimed False g Nothing`, then
`createMemberSndStatuses` from the per-group results; instance changes as
for contacts; cursor by group id. The job sends no profile update
(`sendGroupProfileUpdate`, `Internal.hs:2496`): `redactedMemberProfile`
(:1311) depends on each group's preferences and `presentUserBadge` (:2176)
produces a proof per presentation, so one body cannot serve all groups; a
pending profile update reaches the customer with the next ordinary message
in that chat.

Completion: `updateDeliveryJobStatus db jobId DJSComplete` (as the group
branch, `Subscriber.hs:4161`); `FJNew` sets `CISSndSent SSPComplete` and
emits `CEvtChatItemsStatusesUpdated` for the feed item; `FJDelete
CIDMInternal` and `FJDelete CIDMBroadcast` then `deleteFeedChatItem` (its
messages, versions and file) and emit `CEvtChatItemsDeleted` with the
`CIDeleting` item and `toChatItem = Nothing`; the cascade removes the job
row. Completed feed jobs are removed by `cleanupDeliveryJobs`
(`Commands.hs:5315`) with the group jobs.

Jobs of one feed run in id order (`getNextDeliveryJob` takes the lowest
pending id of the key), so per recipient `XMsgNew` precedes
`XMsgFileDescr`, `XMsgUpdate` and `XMsgDel` on the connection.

## Files

`processAgentMsgSndFile` (`Subscriber.hs:183`): `withEntityLock_` gains
`Just (ChatRef CTFeed feedId _) -> withFeedLock`. In the `SFDONE` branch
(:209), `lookupChatItemByFileId` returns the feed item; new case
`(rfd : _, _, SMDSnd, FeedChat feed)`:

1. `splitFileDescr partSize (fileDescrText rfd)` (as :298) into
   `XMsgFileDescr {msgId = sharedMsgId, fileDescr, fileExpires}` events;
   `createFeedSndMessages sharedMsgId` with `FeedId feedId`;
   `insertChatItemMessage_` per part.
2. `createMsgDeliveryJob db (DEFeed feedId) (DJSFeed feedItemId FJFileDescr) [] partIds batchBody`
   per batch of `batchSndMessagesJSON`, and
   `getDeliveryJobWorker True (DEFeed feedId, DWSFeed)`.
3. `updateCIFileStatus db user fileId CIFSSndComplete`,
   `xftpDeleteSndFileInternal`, `CEvtSndFileCompleteXFTP`, as the group case
   (:253-257).

Every recipient downloads with the same description. The agent has no caller
of `ackXFTPChunk` (`simplexmq/src/Simplex/FileTransfer/Client.hs:311`), so
chunk replicas stay until server expiry; channels rely on this already (one
description per relay, forwarded to all subscribers).

## Editing

`APIUpdateChatItem (ChatRef CTFeed feedId) itemId live (UpdatedMessage mc mentions)`
(:767), under `withFeedLock`:

1. `live` -> "not supported"; mentions -> "mentions are not supported in this
   chat"; `assertAllowedContent mc`.
2. `feed`, `CChatItem SMDSnd ci <- getFeedChatItem`; requires
   `CISndMsgContent oldMC`, `itemSharedMsgId = Just smId`, `editable`
   (False for `CIDeleted` and `CIDeleting` items).
3. `mc == oldMC` -> `CRChatItemNotChanged`.
4. Message `XMsgUpdate smId mc M.empty Nothing Nothing Nothing Nothing (Just True)`
   via `createFeedSndMessages smId`, linked to the feed item; job
   `createMsgDeliveryJob db (DEFeed feedId) (DJSFeed itemId FJUpdate) [] [msgId] msgBody`
   and `getDeliveryJobWorker True (DEFeed feedId, DWSFeed)`.
5. Feed item: `addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)`;
   `updateFeedChatItem' db user feedId ci (CISndMsgContent mc) True`;
   response `CRChatItemUpdated`. Instance versions are not recorded; the
   feed item holds the history.

## Deleting

`APIDeleteChatItem (ChatRef CTFeed feedId) itemIds mode` (:843), under
`withFeedLock`: `getCommandFeedChatItems` (next to `getCommandLocalChatItems`
:4966); `CIDMHistory` -> `CEInvalidChatItemDelete`; per item:

1. An item with `itemDeleted = Just (CIDeleted _)` -> `CEInvalidChatItemDelete`.
   `CIDMBroadcast` on an item without `itemDeleted`: `assertDeletable` (:891)
   and the message `XMsgDel smId Nothing Nothing False` via
   `createFeedSndMessages smId`, linked to the feed item. On a `CIDeleting`
   item the mode of the earlier delete is kept and no message is created:
   the job is enqueued again.
2. Job `createMsgDeliveryJob db (DEFeed feedId) (DJSFeed itemId (FJDelete mode)) [] msgIds body`
   (`msgIds` and `body` from the `XMsgDel` message for `CIDMBroadcast`,
   `[]` and empty otherwise) and `getDeliveryJobWorker True (DEFeed feedId, DWSFeed)`.
3. `CIDMInternalMark`: `markFeedChatItemDeleted` with `CIDeleted`
   (`toChatItem = Just` the marked item). `CIDMInternal` and `CIDMBroadcast`:
   `markFeedChatItemDeleted` with `CIDeleting` (`toChatItem = Just` the
   item in `CIDeleting`); the job removes the item at the end.
4. Response `CRChatItemsDeleted user deletions True False`.

## Receiving

- `newContentMessage` (`Subscriber.hs:1870`): when
  `feed == Just True && isTrue (dropFeed chatSettings)` the message is
  discarded before file processing; the caller's acknowledgement and receipt
  are unchanged. For a feed message the item TTL is the chat's own:
  `rcvContactCITimed ct (join $ contactTimedTTL ct)` in place of
  `rcvContactCITimed ct itemTTL` (:1892); groups use
  `join $ groupTimedTTL gInfo` in `timed_` (:2147).
- `messageUpdate` (:1979): the not-found branch is skipped under the same
  condition (`XMsgUpdate {feed}`), and otherwise applies the chat TTL rule
  above for a feed update; the found branch is unchanged
  (`updatedChatItem` keeps `itemTimed` when the update has no `ttl`,
  `Store/Messages.hs:2571-2577`).
- `messageDelete` (:2021): the not-found event is skipped when the contact
  has `dropFeed` set.
- `messageFileDescription` (:1911) and `groupMessageFileDescription`
  (:1919): `getFileIdBySharedMsgId` fails for a dropped feed message with
  `SEFileIdNotFoundBySharedMsgId`, which `processEvent` reports as an error
  event (:534); when the chat has `dropFeed` set, that error is discarded.
- `newGroupContentMessage` (:2121) and `groupMessageUpdate` (:2209): the same
  checks against `GroupInfo {chatSettings}`; a discarded message returns
  `Nothing` (no delivery task).
- Received feed items get `itemFeed = Just CIFeedRcv` through
  `createNewRcvChatItem` (`cmFeed`).
- `APISetChatSettings` (:1898) stores `dropFeed`; `SetDropFeed cName on`
  uses `updateChatSettings` (:4347). The command replaces the whole record,
  so a client without the field (an older remote controller) resets
  `dropFeed` to False on any settings change; accepted.
- A voice or file broadcast is checked against each recipient's
  preferences on the recipient side only (`newContentMessage` :1887,
  `prohibitedGroupContent` :2134); the sender does not run
  `assertVoiceAllowed` (`Commands.hs:4743`) per recipient, so some
  recipients record `CIRcvChatFeatureRejected` instead of the message.

## Reading and other commands

- `APIGetChat` (:658): `CTFeed` via `getFeedChat`.
- `APIGetChats` (:654): the feed preview from `getChatPreviews`; the feed is
  always returned, as the notes folder is.
- `APIGetChatItemInfo` (:699): through `getAChatItem`;
  `memberDeliveryStatuses = Nothing`.
- `APIChatRead` (:1231): `CTFeed -> getUserByFeedId; ok user`.
  `APIChatItemsRead`: "not supported". `APIChatUnread` (:1296):
  `updateFeedUnreadChat`. `APIClearChat` (:1386): `deleteFeedFiles`,
  `deleteFeedCIs`; instances stay in their chats, detached by the FK.
- `APIChatItemReaction` (:943): `CTFeed -> throwCmdError "not supported"`.
- `APIDeleteChat`, `APISetChatTags`, `APISetChatSettings`,
  `APISetChatUIThemes`, `APISetChatTTL`: existing "not supported" branches.
- `APIPlanForwardChatItems` (:1004) and `APIForwardChatItems` (:1052) from
  `CTFeed`: `getCommandFeedChatItems`; `ciComposeMsgReq` as the local branch
  (:1119). Forwarding to `CTFeed`: "not supported".
- `getChatRef` (:3699): `CTFeed | name == "" -> getUserFeedId`.
  `getSentChatItemIdByText` (:3763), `getChatItemIdByText` (:3769):
  `getFeedChatItemIdByText` variants.
- `CreateActiveUser` (:443): `createFeed db user` after `createNoteFolder`.

## CLI and view

- Parsers (:5860-5999): `chatTypeP` gains `A.char '%' $> CTFeed`; `chatNameP`
  gains `CTFeed -> pure $ ChatName CTFeed ""`; `sendRefP` is unchanged.
  `"/_feed " *> (APISendFeedMessage <$> A.decimal <*> (" json " *> jsonP <|> " text " *> (composedMessage Nothing <$> mcTextP)))`;
  `"/feed drop " *> (SetDropFeed <$> chatNameP' <* A.space <*> onOffP)`,
  placed before `/feed <text>` (:5744) in the `choice` list: that
  alternative consumes any text through `msgTextP`, so the order decides.
- `View.hs`: `toChatView` (:359) `FeedChat _ -> ("%", ...)`; `viewChatItem`
  (:748) `FeedChat _ -> case chatDir of CIFeedSnd -> ...` with `to = "% "`;
  `viewItemUpdate` (:856) likewise; `viewChatCleared` (:1088)
  `FeedChat _ -> ["feed: all messages are removed"]`; `CRBroadcastSent`
  (:170) and `viewSentBroadcast` (:2360) removed.
- Broadcast bot (`apps/simplex-broadcast-bot/src/Broadcast/Bot.hs:51`):
  matches `CRNewChatItems` and replies that the broadcast is queued.

## Apps and API docs

- Kotlin `ChatModel.kt` (:1472) and the iOS mirrors: `ChatInfo.Feed(feed)`
  with `@SerialName("feed")`, `ChatType.Feed`, `CIDirection.FeedSnd`,
  `CIDeleted.Deleting`, `CIMeta.itemFeed: CIFeed?`, `ChatSettings.dropFeed`,
  the `StoreError` constructors. Chat list row and chat view for the feed;
  a `CIDeleting` item rendered as deletion in progress; a feed marker on
  instances and on received feed items; "edit" on an instance opens the feed
  item when `feedItemId` is present; a "Drop feed messages" toggle in contact
  and group settings; a privacy notice in the feed chat before the first
  broadcast: every recipient receives the same message id, so recipients can
  establish a common sender by comparing messages.
- `bots/src/API/Docs/Types.hs` (:233): `("feed", "%")`; `Feed`, `CIFeed`
  entries; `APISendFeedMessage` in `Docs/Commands.hs`; `CRBroadcastSent`
  removed from `Docs/Responses.hs:132`; regenerated bindings, markdown and
  JSON fixtures.
- `apps/multiplatform/spec/state.md` (:287), `apps/ios/spec/state.md`.

## Follow-ups

- Delivery counts for a feed item in `APIGetChatItemInfo`
  (`GROUP BY item_status` over instances).
- `XFileCancel` job on upload failure.
- The list of reacting contacts for a feed item.
- A live feed-item reaction update on `CEvtChatItemReaction` of an instance;
  the UI refreshes the feed item meanwhile.
- `getChatRefViaItemId` (`Store/Messages.hs:3323`) for local and feed items.
- Forwarding into the feed.

## Tests

`ChatTests`, with `feedBucketSize = 2` in the test config:

1. `/feed` to three contacts and a customer group: instances in each chat
   with `itemFeed = CIFeedSnd`; recipients' items with `CIFeedRcv`; the feed
   item reaches `CISSndSent SSPComplete`.
2. Instance statuses after `SENT` and receipts.
3. A contact with `dropFeed` set receives nothing; a later feed edit and
   delete are silent.
4. A contact's reaction is counted in the feed item.
5. A feed edit updates every instance and every recipient; the feed item has
   two versions.
6. A broadcast delete sets the feed item to `CIDeleting`, removes instances
   of full-delete contacts and marks the others, then removes the feed
   item; an edit of a `CIDeleting` item is rejected; a second delete
   re-enqueues the job; an internal-mark delete marks the feed item and
   every instance and keeps them.
7. Query count per bucket is independent of the bucket size: the SQLite
   statement counts (`trackQueries = DB.TQAll`, `tests/ChatClient.hs:147`;
   `SlowQueryStats.count` per query text) for a bucket of 2 and a bucket of
   4 recipients differ only in `executeMany` row counts.
8. A contact with disappearing messages enabled: the sender's instance and
   the recipient's item are timed with the chat's TTL; the body has no
   `ttl`; a contact without the setting receives an untimed item.
9. A file broadcast: one upload, every recipient receives the file from the
   same description; deleting one instance keeps the file.
10. Incognito contact excluded; `BCBusiness` group excluded; a business
    group's customer receives the message.
11. Restart mid-job: the job resumes from the cursor; a repeated bucket
    creates no duplicate instance and sends nothing twice (the recipient
    receives one message); a job interrupted between instance creation and
    delivery delivers on resume.
12. A delete issued while `FJNew` runs stops the send; the recipients
    reached so far receive `XMsgDel`; the others receive nothing.
13. A group member connection error (`MERR`) marks the group instance's
    member status (`updateGroupItemsErrorStatus`).
14. A dropped feed message with a file: the recipient records no error on
    the following file description.
15. `Direct.hs:995`, `Groups.hs:6731`, `Bots/BroadcastTests.hs` updated to
    `CRNewChatItems`.
