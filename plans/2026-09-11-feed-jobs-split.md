# Split feed delivery from group delivery jobs

Feed delivery is moved out of `delivery_jobs` into its own table, worker key and code path.
`delivery_jobs` returns to its pre-feed definition.
The feed migration is not released, so it is amended in place rather than superseded.

## 1. Schema

### delivery_jobs — revert

Remove from `M20260905_feeds` (SQLite and Postgres):

- `ALTER TABLE delivery_jobs ADD COLUMN feed_id`
- `ALTER TABLE delivery_jobs ADD COLUMN chat_item_id`
- `ALTER TABLE delivery_jobs ADD COLUMN message_ids`
- `ALTER TABLE delivery_jobs ADD COLUMN feed_cursor_id`
- `CREATE INDEX idx_delivery_jobs_feed_next`
- `CREATE INDEX idx_delivery_jobs_chat_item_id`
- the `PRAGMA writable_schema` rewrite of `group_id` (SQLite)
- `ALTER TABLE delivery_jobs ALTER COLUMN group_id DROP NOT NULL` (Postgres)
- the matching statements in both down migrations

`delivery_jobs.group_id` stays `NOT NULL`.
`delivery_jobs.job_scope_spec_tag` holds `DeliveryJobSpecTag` only.
`delivery_jobs.worker_scope` holds `DeliveryWorkerScope` only.

### feed_jobs — new table

SQLite:

```
CREATE TABLE feed_jobs(
  feed_job_id INTEGER PRIMARY KEY AUTOINCREMENT,
  feed_id INTEGER NOT NULL REFERENCES feeds ON DELETE CASCADE,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  worker_scope TEXT NOT NULL,
  action_tag TEXT NOT NULL,
  message_ids TEXT,
  cursor_id INTEGER,
  job_status TEXT NOT NULL,
  job_err_reason TEXT,
  failed INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
) STRICT;
CREATE INDEX idx_feed_jobs_next ON feed_jobs(feed_id, worker_scope, failed, job_status, feed_job_id);
CREATE INDEX idx_feed_jobs_chat_item_id ON feed_jobs(chat_item_id, action_tag);
```

Postgres uses `BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY`, `BIGINT`, `TIMESTAMPTZ NOT NULL DEFAULT (now())`, `SMALLINT NOT NULL DEFAULT 0` for `failed`.

Down migration: `DROP INDEX idx_feed_jobs_chat_item_id`, `DROP INDEX idx_feed_jobs_next`, `DROP TABLE feed_jobs`.

`worker_scope` holds `FeedWorkerScope`.
`action_tag` holds `FeedJobActionTag`.
`cursor_id` holds a contact id under `FWSContacts` and a group id under `FWSGroups`.

## 2. Types

`src/Simplex/Chat/Delivery.hs`:

- delete `DeliveryJobKey`
- delete `DeliveryJobWork`, `DJWGroup`, `DJWFeed`
- restore `DeliveryJob` as the pre-feed record with `jobId`, `cursorId_`, `jobScope`, `senderGMIds`, `body`
- add `FeedJobKey = (FeedId, FeedWorkerScope)`
- add `FeedJob {feedJobId, feedItemId, feedAction, cursorId_}`
- keep `FeedWorkerScope`, `feedWorkerScopes`, `FeedJobAction`, `FeedJobActionTag`, `FeedInstanceSpec` and the action projections

`src/Simplex/Chat/Controller.hs`:

- `deliveryJobWorkers` reverts to `TMap DeliveryWorkerKey Worker`
- add `feedJobWorkers :: TMap FeedJobKey Worker`

## 3. Store

`src/Simplex/Chat/Store/Delivery.hs` reverts to its pre-feed content, with `getPendingDeliveryJobScopes` restored to the single `SELECT DISTINCT group_id, worker_scope` form.

Feed job statements move to `src/Simplex/Chat/Store/Feeds.hs`:

- `createFeedJobs`
- `getNextFeedJob`
- `updateFeedJobCursor`
- `completeFeedJob`
- `getFeedJobMessages`
- `setFeedJobErrStatus`
- `getPendingFeedJobScopes`
- `deleteDoneFeedJobs`

`updateDeliveryJobStatus_` is duplicated as a feed-table statement rather than parameterised by table name.

## 4. Worker

`src/Simplex/Chat/Library/Subscriber.hs`:

- `runDeliveryJobWorker` loses its `case deliveryKey` and handles groups only
- add `runFeedJobWorker` with the same `jobLoop`/`withWork_` shape
- extract the shared shape as `jobWorkerLoop :: Int64 -> Worker -> CM () -> CM ()` plus `jobOperation`, parameterised by the read, the processor and the error writer
- `getDeliveryJobWorker` handles `DeliveryWorkerKey`; add `getFeedJobWorker` for `FeedJobKey`
- `startDeliveryJobWorkers` reads group scopes; add `startFeedJobWorkers` reading feed scopes
- `startFeedWorkers` uses `getFeedJobWorker`

## 5. Review items to resolve in the same change

### Already resolved in the working tree

- `DB.executeMany` with UPDATE and DELETE templates — replaced by `itemIdsStmt` (`IN ?` under `dbPostgres`, a loop otherwise) and by `forM_ … DB.execute`. The only remaining `executeMany` is the `INSERT` in `createFeedJobs`.
- `completeFeedJob` counts `job_status != DJSComplete`, so an errored sibling job prevents finalisation.
- `processFeedJob` sets `CISSndError` on the feed item when a creating action fails.
- The `CIDeleting` retry no longer downgrades to `FJADeleteInternal`; `createFeedJobs` supersedes errored jobs of the same action tag.
- The broadcast deletion window is enforced in the `CTFeed` branch as `diffUTCTime deletedTs itemTs < deleteMsgInterval`.
- `notification` derives from `feedActionEventTag action`.
- `messageDelete`, `groupMessageDelete` and the two file-description paths are silent under `chatDropsFeed`.
- `delGroupChatItems` is one `if` over `fullDelete`.
- `getChatItemIdsByAgentMsgId` is split into `getDirectChatItemIdsByAgentMsgId` and `getGroupChatItemIdsByAgentMsgId`, each probing both columns of the instance index.
- `getPendingDeliveryJobScopes` no longer drops rows whose scope fails to decode.
- The delivered-recipient filters use `Set` membership.
- The cursor and the first-bucket item status are written in one transaction.
- `ProtocolTests` covers `feed` on `x.msg.new` and `x.msg.update`.

### To resolve

1. **Errored feed job leaves the item unfinished.** `finishFeedEvent` never runs while a sibling is `DJSError`. Set the feed item to `CISSndError` and emit `CEvtChatItemsStatusesUpdated` when a job errors. Emit the same event for the first `SSPPartial` write.
2. **No retry of an errored feed job.** Decide between a bounded retry in the worker and a documented manual retry through the command. The current behaviour requires the user to repeat the command.
3. **A deleted contact aborts a whole bucket.** The `FJANew` bucket creates every instance in one `withStore'`, so one foreign key violation loses the rest of the broadcast. Use `withStoreBatch'` per recipient and drop the failing recipient.
4. **`finishFeedEvent` runs outside the feed lock.** Extend `withFeedLock` over `completeFeedJob` and `finishFeedEvent`.
5. **Group recipients.** Members that `memberSendAction` would treat as `MSAPending` or `MSAForwarded` receive nothing and get no pending message. Every member is recorded `GSSNew`, and one member's failure sets the whole instance to `CISSndError`. Record per-member `GSSError` as `createMemberSndStatuses` does, and decide whether pending members are stored.
6. **`APISendFeedMessage` is not atomic.** The item, the message, the message link and the jobs are four transactions. A crash between them leaves an undelivered item in `CISSndNew`. Create all four in one transaction.
7. **A locally marked feed item cannot be removed.** `(_, Just (CIDeleted _))` rejects every later deletion. Accept `CIDMInternal` on a marked item, as direct and group chats do.
8. **`dropFeed` suppression is too broad.** `XMsgDel` and `XMsgFileDescr` carry no feed marker, so the guards silence the not-found delete event and the missing-file error for every message in a chat with `dropFeed` set. Restrict the suppression to items whose shared id belongs to a dropped broadcast, or accept it and record the decision.
9. **Feed messages to groups are unsigned.** `createFeedMessage` passes no `MsgSigning` and `getFeedJobMessages` sets `signedMsg_ = Nothing`, so in a customer group with `SGFSignMessages` the broadcast arrives unverified. Decide between signing per group and excluding signing groups from the broadcast.
10. **`feeds.favorite` has no writer.** `APISetChatSettings` rejects `CTFeed`, and the favourite filter in `findFeedChatPreviews_` reads the column. Either accept `CTFeed` in `APISetChatSettings` or drop the column and the filter.
11. **`setUserChatsRead` skips `feeds`.** A feed marked unread stays unread after `APIUserRead`.
12. **`getChatRefViaItemId` rejects feed items.** `getAllChatItems` returns them, so `APIGetChatItems` after or around a feed item and `ShowChatItem` fail. Add `feed_id` to the query and the mapping.
13. **`SendFile` rejects `%`.** `APISendFeedMessage` accepts `fileSource`, and `/file %` does not reach it.
14. **`SEUserFeedNotFound` and `SEFeedNotFound` have no `viewStoreError` case.**
15. **`unzipMaybe3` unzips a pair.** Rename.
16. **`chat_query_plans.txt` is stale.** Regenerate it from a full test run. The file is owned by root in the current environment.
17. **`plans/2026-09-04-feed-broadcast-chat.md` contradicts the code.** Update it or drop it from the change.
18. **Tests.** Add coverage for `dropFeed`, a file broadcast, a restart in the middle of a job, incognito exclusion, a member send error, and a per-chat detach. `testFeedEditDelete` reads the instance immediately after the recipient prints the edit, before the worker writes it.
19. **`createContactPQSndItem` is not called on the feed path.** `deliverMessagesB` updates the stored PQ state, so only the chat item announcing the change is missing.
20. **Documentation.** A dropped broadcast still stores its body in `messages` for 30 days, and still returns a delivery receipt.

## 6. Order of work

1. Amend the migrations.
2. Split the types.
3. Move the store statements.
4. Split the worker.
5. Resolve items 1 to 9.
6. Resolve items 10 to 15.
7. Regenerate `chat_schema.sql`, `chat_lint.sql` and `chat_query_plans.txt`.
8. Add the tests of item 18.
9. Run the feed, group, direct, bot, schema and API-docs suites, and the Postgres target.
