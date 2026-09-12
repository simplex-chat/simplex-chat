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
- add `runFeedJobWorker` with the same `forever`/`withWork_` shape; `runDeliveryJobWorker` keeps its master shape
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
- `notification` derives from `feedActionCreates action`; `feedActionEventTag` is removed.
- `messageDelete`, `groupMessageDelete` and the two file-description paths are silent under `chatDropsFeed`.
- `delGroupChatItems` is one `if` over `fullDelete`.
- `getChatItemIdsByAgentMsgId` is split into `getDirectChatItemIdsByAgentMsgId` and `getGroupChatItemIdsByAgentMsgId`, each probing both columns of the instance index.
- `getPendingDeliveryJobScopes` no longer drops rows whose scope fails to decode.
- The delivered-recipient filters use `Set` membership.
- The cursor and the first-bucket item status are written in one transaction.
- `ProtocolTests` covers `feed` on `x.msg.new` and `x.msg.update`.

### To resolve

1. **An inner exception must not fail a job.** The `FJANew` bucket creates every instance in one
   `withStore'`, so a foreign key violation from a contact, group or member deleted between the bucket's
   read and its insert loses the rest of the broadcast. Write instances with `withStoreBatch'` per
   recipient and drop the failing recipient. Same for `createGroupSndStatus`.
2. **An errored feed job resumes after restart.** `resumeFeedJobsOnStart` resets
   `failed = 0 AND job_status = DJSError` to `DJSPending` before the workers are started, and the job
   continues from its stored cursor. Within a session an errored job is not retried, so the worker does
   not spin. `failed = 1` stays terminal: it marks a row that failed to decode.
3. **A job error raises a critical error.** `CRITICAL {offerRestart = True}`, as at `Subscriber.hs:153`
   and `Commands.hs:5316`, so the app offers a restart.
4. **The feed item's status is reported on every change.** `CISSndError` on a job error and
   `CISSndSent SSPPartial` on the first bucket each emit `CEvtChatItemsStatusesUpdated`.
5. **`finishFeedEvent` runs inside the feed lock**, before `completeFeedJob` writes `DJSComplete`.
6. **Group instance statuses are recorded per member.** A member's send failure writes `GSSError` against
   that member instead of `CISSndError` on the whole instance. Members without a ready connection are
   still skipped, and no pending messages are stored.
7. **`APISendFeedMessage` creates the item, the message, the message link and the jobs in one transaction.**
8. **`CIDMInternal` is accepted on a marked feed item**, as in direct and group chats.
9. **`feed :: Maybe Bool` is added to `XMsgDel` and `XMsgFileDescr`**, as it was to `XMsgUpdate`. The
   guards in `messageDelete`, `groupMessageDelete` and `skipDroppedFeedFile` become
   `dropsFeed chatSettings feed_`.
10. **`CTFeed` is accepted by `SendFile` and `SendImage`**, as `CTLocal` already is.
11. **`setUserChatsRead` clears `feeds.unread_chat`.**
12. **`SEUserFeedNotFound` and `SEFeedNotFound` get `viewStoreError` cases.**
13. **`unzipMaybe3` is renamed** — it unzips a pair.
14. **`chat_query_plans.txt` is regenerated** from a full test run.
15. **`plans/2026-09-04-feed-broadcast-chat.md` is updated or dropped** — it contradicts the code.
16. **Tests** for `dropFeed`, a file broadcast, a restart in the middle of a job, incognito exclusion, a
    member send error, and a per-chat detach. `testFeedEditDelete` reads the instance immediately after
    the recipient prints the edit, before the worker writes it.
17. **Feed job cleanup** is wired into the cleanup manager as `deleteDoneFeedJobs`.
18. **The file description path is one transaction** — `createFeedFileDescrJobs` creates every part
    message, its link to the feed item and the jobs together.
19. **`deleteFeedInstances` no longer deletes `chat_item_messages`** — an instance is created without a
    message link, and the row would cascade with the item.

### Not changed

- `feeds.favorite` stays, mirroring `NoteFolder`: `note_folders.favorite` has no writer either, so the
  unwritten column is pre-existing rather than introduced here.
- `getChatRefViaItemId` stays as it is. `APIGetChatItems` across all chats is used only from the CLI, and
  its pagination is unused.
- Signing in customer groups: `signMessages` defaults to `FEOff` and feed groups are filtered to
  `business_chat = BCCustomer`, so an unsigned broadcast never reaches a signing channel.
- Retry and the critical error for group delivery jobs: the same gap exists there, outside this change.
- `createContactPQSndItem` on the feed path: `deliverMessagesB` already updates the stored PQ state, so
  only the chat item announcing the change is missing.
- A dropped broadcast still stores its body in `messages` for 30 days and still returns a delivery
  receipt — documented, not changed.

## 6. Done

Sections 1 to 4 are implemented, and items 1 to 13, 15, and 17 to 19 of section 5.

Item 16: tests cover `dropFeed`, a file broadcast and a per-chat detach. A restart in the middle of a
job, incognito exclusion and a member send error remain uncovered — none of the three can be made
deterministic without an induced failure.

Item 14 needs a full test run with write access to `chat_query_plans.txt`.

The Postgres schema dump is stale: `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql` still
holds the feed columns on `delivery_jobs` and no `feed_jobs`. Only the Postgres client binaries are
installed here, so `postgresSchemaDumpTest` cannot be run.

## 7. Order of work

1. Amend the migrations.
2. Split the types.
3. Move the store statements.
4. Split the worker.
5. Resolve items 1 to 9.
6. Resolve items 10 to 15.
7. Regenerate `chat_schema.sql`, `chat_lint.sql` and `chat_query_plans.txt`.
8. Add the tests of item 18.
9. Run the feed, group, direct, bot, schema and API-docs suites, and the Postgres target.
