# Directory Service PostgreSQL Performance Analysis

**Date:** 2026-02-16
**Scope:** Full codepath analysis of `simplex-directory-service` with PostgreSQL backend

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [High-Level Architecture](#2-high-level-architecture)
3. [Slow Query Analysis](#3-slow-query-analysis)
4. [Codepath Performance Issues](#4-codepath-performance-issues)
5. [Member Join / Introduction Path](#5-member-join--introduction-path---the-critical-bottleneck)
6. [Detailed Issue Catalog](#6-detailed-issue-catalog)
7. [Recommendations](#7-recommendations)

---

## 1. Executive Summary

The directory service operates as a bot within the SimpleX Chat core, processing events through `directoryServiceEvent` (`Service.hs:286`). It manages large groups (17k+ members) where the core group messaging infrastructure becomes the primary bottleneck.

**Three categories of issues, by impact:**

| Category | DB Time | Description |
|----------|---------|-------------|
| **Core hot paths** | 2,348s (51%) | `getGroupCurrentMembersCount`, `member_relations_vector`, `getGroupMembers` |
| **External analytics** | 1,067s (23%) | Queries not in codebase, missing indexes |
| **Directory-specific** | ~200s (est.) | N+1 patterns in `getAllListedGroups_`, `getOwnersInfo`, per-admin sends |

The single largest win is replacing `getGroupCurrentMembersCount` (831s / 93,751 calls) with the already-cached `GroupInfo.groupSummary.currentMembers` — a 4-line change.

The most architecturally significant issue is the member introduction path: joining a 17k-member group triggers ~51,000 DB operations per new member.

---

## 2. High-Level Architecture

### DB Access Patterns

All DB access goes through the same `chatStore` connection pool:

| Wrapper | Location | Used By |
|---------|----------|---------|
| `withDB` / `withDB'` | `Directory/Util.hs:22-31` | Directory store operations |
| `withStore` / `withFastStore` | `Controller.hs:1536-1541` | Chat core operations |
| `withStoreBatch` | `Controller.hs:1549-1552` | Batched INSERTs |

Each call is a separate `BEGIN`/`COMMIT` transaction. Directory operations and chat core operations compete for the same pool.

### Event Flow

```
Network event -> Agent -> Subscriber.hs -> ChatEvent -> outputQ/eventQ
                                                            |
                                                  directoryServiceEvent
                                                            |
                                                    sendChatCmd (APISendMessages, etc.)
                                                            |
                                                    Commands.hs -> Internal.hs -> Store
```

Every `sendMessage`/`sendComposedMessages_` call from the directory service (`Bot.hs:86-91`) invokes `sendChatCmd cc (APISendMessages ...)`, which triggers the full chat command processing pipeline in `Commands.hs`.

---

## 3. Slow Query Analysis

Source: `slow_queries.csv` — 100 queries from `pg_stat_statements`, 4,624s total.

### Time Breakdown

| Category | Time (s) | % | Calls | Actionable |
|----------|----------|---|-------|------------|
| External analytics queries | 1,067 | 23.1% | ~712 | Indexes (not in repo) |
| `member_relations_vector` UPDATEs | 1,021 | 22.1% | 39 | plpgsql / batching |
| `getGroupCurrentMembersCount` | 831 | 18.0% | 93,751 | **Trivial fix** |
| `INSERT INTO messages` | 555 | 12.0% | 863k | Unavoidable |
| `getGroupMembers` full loads | 496 | 10.7% | 113 | Architectural |
| Other | 497 | 10.7% | — | Mostly unavoidable |
| `DELETE FROM messages` | 67 | 1.4% | 41 | Batch deletes |
| `pending_group_messages` | 74 | 1.6% | — | Unavoidable |

### Top Queries

| # | Query | Total (s) | Avg (ms) | Calls |
|---|-------|-----------|----------|-------|
| 1 | `SELECT member_status FROM group_members WHERE group_id=? AND user_id=?` | 831 | 8.87 | 93,751 |
| 2 | `UPDATE group_members SET member_relations_vector = set_member_vector_new_relation(...)` (17k IN) | 722 | 27,763 | 26 |
| 3 | External: `chat_items` direction counts | 514 | 2,886 | 178 |
| 4 | `INSERT INTO messages` (group send) | 467 | 0.61 | 769k |
| 5 | `groupInfoQuery` (getGroupInfo/member lookup) | 455 | 1.80 | 253k |
| 6 | External: `chat_items` period counts | 412 | 2,313 | 178 |
| 7 | `UPDATE group_members SET member_relations_vector...` (variant) | 165 | 27,492 | 6 |
| 8 | External: `chat_items` by chat type | 138 | 776 | 178 |
| 9 | `UPDATE group_members SET member_relations_vector...` (variant) | 112 | 27,889 | 4 |
| 10 | `INSERT INTO pending_group_messages` | 64 | 0.10 | 657k |

---

## 4. Codepath Performance Issues

### 4.1 `getGroupCurrentMembersCount` — 831s, trivial fix

**Source:** `Store/Groups.hs:1103-1115`

```haskell
getGroupCurrentMembersCount db User {userId} GroupInfo {groupId} = do
  statuses <- DB.query db
    "SELECT member_status FROM group_members WHERE group_id = ? AND user_id = ?"
    (groupId, userId)
  pure $ length $ filter memberCurrent' statuses
```

Fetches ALL member statuses for a group, filters in Haskell. For 17k-member groups, returns 17k rows per call. Called 93,751 times.

**The result is already cached.** Migration `M20250919_group_summary` added `groups.summary_current_members_count` maintained by INSERT/UPDATE/DELETE triggers on `group_members`. Loaded into `GroupInfo.groupSummary.currentMembers` by every `getGroupInfo` call (`Shared.hs:674`).

**Call sites** (all have `gInfo :: GroupInfo` in scope):

| Location | Context | Frequency |
|----------|---------|-----------|
| `Subscriber.hs:945` | `checkSendRcpt` — every group message receive | **Dominant** |
| `Subscriber.hs:703` | `sendGrpInvitation` — group invitation | Low |
| `Commands.hs:3481` | `sendGrpInvitation` — command path | Low |
| `Internal.hs:940` | Joining via group link | Per join |

**Fix:** Replace `withStore' $ \db -> getGroupCurrentMembersCount db user gInfo` with `pure $ fromIntegral $ currentMembers $ groupSummary gInfo` at all 4 sites. Remove the function.

### 4.2 N+1 in `getAllListedGroups_` — runs every 5 minutes

**Source:** `Directory/Store.hs:336-341`

```haskell
getAllListedGroups_ db vr' user =
  DB.query db (groupReqQuery <> " AND r.group_reg_status = ?") (userId, userContactId, GRSActive)
    >>= mapM (withGroupLink . toGroupInfoReg vr' user)
  where
    withGroupLink (g, gr) = (g,gr,) . eitherToMaybe <$> runExceptT (getGroupLink db user g)
```

After fetching all listed groups in one query, calls `getGroupLink` (`Groups.hs:273-276`) **individually per group**:

```sql
SELECT ... FROM user_contact_links WHERE user_id = ? AND group_id = ? LIMIT 1
```

For N listed groups → N+1 queries in a single transaction.

**Called from:** `updateGroupListingFiles` (`Service.hs:1236-1239`) via the 5-minute background thread (`Service.hs:180-186`).

**Fix:** JOIN `user_contact_links` into the main `groupReqQuery`.

### 4.3 N+1 in `getOwnersInfo`

**Source:** `Service.hs:1171-1174`

```haskell
getOwnersInfo gs =
  withDB' "getOwnersInfo" cc $ \db ->
    mapM (\g@(_, gr) -> ... getContact db (vr cc) user $ dbContactId gr) gs
```

For each `(GroupInfo, GroupReg)`, a separate `getContact` query (which itself calls `getDirectChatTags` — so 2 queries per group).

**Called from:** `sendGroupsInfo` (`Service.hs:1176`) when admin runs `/list`, `/last`, `/pending`.

**Fix:** Batch contact lookups into a single `WHERE contact_id IN (...)` query.

### 4.4 Per-admin sequential message sends

**Source:** `Service.hs:313-317`

```haskell
withAdminUsers action = void . forkIO $ do
  forM_ superUsers $ \KnownContact {contactId} -> action contactId
  forM_ adminUsers $ \KnownContact {contactId} -> action contactId
notifyAdminUsers s = withAdminUsers $ \contactId -> sendMessage' cc contactId s
```

Each `sendMessage'` triggers a full `APISendMessages` pipeline: `getContact` → `createSndMessage` → `updateChatTsStats` → `createSndChatItem` — **4 DB transactions per admin**.

Used in: `deGroupUpdated`, `deContactRoleChanged`, `deServiceRoleChanged`, `deContactRemovedFromGroup`, `deContactLeftGroup`, `deServiceRemovedFromGroup`, `deGroupDeleted`.

### 4.5 Double message send in `sendToApprove`

**Source:** `Service.hs:702-712`

```haskell
sendToApprove ... = do
  ct_ <- getContact' cc user dbContactId           -- 1 DB transaction
  withAdminUsers $ \cId -> do
    sendComposedMessage' cc cId Nothing msg         -- 4 DB transactions
    sendMessage' cc cId $ "/approve ..."            -- 4 DB transactions
```

**8 DB transactions per admin** for every group approval notification. With 3 admins → 25 transactions.

**Fix:** Combine messages using `sendComposedMessages_` with both messages in a single `NonEmpty` list.

### 4.6 Redundant `getGroupReg_` in status updates

**Source:** `Store.hs:258-264`

```haskell
setGroupStatusStore cc gId grStatus' =
  withDB "setGroupStatusStore" cc $ \db -> do
    gr <- getGroupReg_ db gId    -- SELECT (redundant — caller often has it)
    DB.execute db "UPDATE ..."
    pure (groupRegStatus gr, gr {groupRegStatus = grStatus'})
```

The caller (`withGroupReg` at `Service.hs:320-327`) already fetched `GroupReg` via `getGroupReg`. Then `setGroupStatusStore` re-fetches it.

Same pattern in `setGroupStatusPromoStore` (`Store.hs:266-272`) and `setGroupPromotedStore` (`Store.hs:274-280`).

**Fix:** Pass old `GroupReg` to avoid re-querying, or use `UPDATE ... RETURNING`.

### 4.7 Dual queries in `searchListedGroups`

**Source:** `Store.hs:343-396`

```haskell
gs <- groups $ DB.query db (listedGroupQuery <> searchCond <> orderBy <> " LIMIT ?") ...
n  <- count  $ DB.query db (countQuery'      <> searchCond) ...
```

Both the data query and count query scan similar rows. For `LIKE '%...%'` text searches, both do full scans.

**Fix:** Use `COUNT(*) OVER()` window function, or return `has_more` flag instead of total count.

### 4.8 External analytics queries — 1,067s, missing indexes

Queries #3, #6, #8, #69 in slow_queries.csv. Not in this repository. They scan `chat_items` filtering by `item_content_tag`, `item_sent`, `contact_id IS NOT NULL`, and `item_ts`.

**No index covers these access patterns.** Existing indexes lead with `(user_id, contact_id, ...)` or `(user_id, group_id, ...)`.

**Needed indexes:**
```sql
CREATE INDEX idx_chat_items_user_item_content_tag_contact
  ON chat_items (user_id, item_content_tag, item_ts)
  WHERE contact_id IS NOT NULL;
```

---

## 5. Member Join / Introduction Path — The Critical Bottleneck

When a member joins a 17k-member directory group (with captcha), the full sequence is:

### Phase 1: Group Link Join (Subscriber.hs:1293-1306)

| Step | DB Operations | Source |
|------|---------------|--------|
| `getGroupInfo` | 1 query | `Subscriber.hs:1295` |
| `acceptMemberHook` (directory) | 0 (reads `customData` from `GroupInfo`) | `Service.hs:245-258` |
| `createJoiningMember` | ~7 ops (display_name, profile, member INSERT, index increment) | `Groups.hs:1216-1257` |
| `getGroupCurrentMembersCount` | **1 query returning 17k rows** | `Internal.hs:940`, `Groups.hs:1103` |
| `createJoiningMemberConnection` | 1 INSERT | `Groups.hs:1264` |
| `getGroupMemberById` | 1 query | `Groups.hs:940` |
| `createInternalChatItem` | ~2 ops | `Subscriber.hs:1305` |
| **Subtotal** | **~13 ops** | |

### Phase 2: Connection + Pending Status (Subscriber.hs:804-858)

Member status is `GSMemPendingApproval` → **no introductions yet**.

| Step | DB Operations | Source |
|------|---------------|--------|
| `increaseGroupMembersRequireAttention` | 1 UPDATE | `Subscriber.hs:811` |
| `mkGroupChatScope` | ~2 ops | `Subscriber.hs:813` |
| `memberConnectedChatItem` | ~2 ops | `Subscriber.hs:814` |
| **Subtotal** | **~5 ops** | |

### Phase 3: Captcha Exchange (Service.hs:564-696)

| Step | DB Operations | Source |
|------|---------------|--------|
| Send captcha (2 messages via `APISendMessages`) | ~4 ops | `Service.hs:575-587` |
| Receive response + verify | ~2 ops | `Service.hs:636-670` |
| Send confirmation message | ~4 ops | `Service.hs:665` |
| **Subtotal** | **~10 ops** | |

### Phase 4: Member Acceptance — THE BOTTLENECK (Commands.hs:2281-2339)

`approvePendingMember` → `sendChatCmd cc (APIAcceptMember groupId gmId role)`:

| Step | DB Operations | Source |
|------|---------------|--------|
| `getGroupInfo` + `getGroupMemberById` | 2 queries | `Commands.hs:2282` |
| `sendDirectMemberMessage` (XGrpLinkAcpt) | ~3 ops | `Commands.hs:2298` |
| **`getGroupMembers`** (full scan) | **1 query → 17k rows with 30+ columns, 3-table JOIN** | `Internal.hs:1060`, `Groups.hs:1035` |
| `getMemberRelationsVector` | 1 query | `Internal.hs:1060`, `Groups.hs:1657` |
| **`sendGroupMessage'` XGrpMemNew** to 17k members | 1 msg INSERT + **~17k `msg_deliveries` INSERTs** | `Internal.hs:1073` |
| `setMemberVectorNewRelations` (new member's vector) | 2 ops (SELECT FOR UPDATE + UPDATE) | `Groups.hs:1593-1613` |
| **`setMembersVectorsNewRelation`** (all 17k members) | **1 massive UPDATE with plpgsql per-row** | `Groups.hs:1618-1623` |
| `sendGroupMemberMessages` (17k XGrpMemIntro) | **~17k msg INSERTs + ~17k delivery INSERTs** | `Internal.hs:1780-1787` |
| `sendHistory` (up to 100 items) | ~200 ops (per-item: `prepareGroupMsg`, file descr lookups) | `Internal.hs:1138-1228` |
| `updateGroupMemberAccepted` | 1 UPDATE | `Commands.hs:2301` |
| `updateGroupMembersRequireAttention` | 1 UPDATE | `Commands.hs:2303` |
| 2x `createInternalChatItem` | ~4 ops | `Commands.hs:2306-2309` |
| **Subtotal** | **~51,000+ ops** | |

### Total: ~51,030 DB operations for 1 member joining a 17k group

The dominant costs:
- **17k `messages` INSERTs** for XGrpMemIntro (introduction messages to new member)
- **17k `msg_deliveries` INSERTs** for XGrpMemIntro delivery tracking
- **17k `msg_deliveries` INSERTs** for XGrpMemNew (announcement to all existing members)
- **1 massive UPDATE** on 17k `group_members` rows with plpgsql function
- **1 full `getGroupMembers` scan** loading 17k rows with 30+ columns

### `member_relations_vector` plpgsql function detail

**Source:** `M20251117_member_relations_vector.hs:28-54`

```sql
CREATE FUNCTION set_member_vector_new_relation(v BYTEA, idx BIGINT, direction INT, status INT)
RETURNS BYTEA AS $$
  -- Per-row: get_byte, conditional generate_series for extension, set_byte
$$ LANGUAGE plpgsql IMMUTABLE;
```

Called via:
```sql
UPDATE group_members
SET member_relations_vector = set_member_vector_new_relation(member_relations_vector, ?, ?, ?),
    updated_at = ?
WHERE group_member_id IN ($5, $6, ..., $17089)
```

**Bottlenecks:**
1. plpgsql interpreted per-row (17k executions per UPDATE)
2. `generate_series` allocation when extending vector
3. 17k-parameter IN clause → planning overhead
4. Single long-held row-level lock across all 17k rows

---

## 6. Detailed Issue Catalog

### Priority 1 — Quick wins (high impact, low effort)

| # | Issue | Impact | Fix |
|---|-------|--------|-----|
| P1.1 | `getGroupCurrentMembersCount` uses full scan instead of cached `summary_current_members_count` | 831s / 93k calls eliminated | Replace with `currentMembers (groupSummary gInfo)` at 4 call sites |
| P1.2 | `sendToApprove` sends 2 separate messages per admin | 8 txns/admin → 4 txns/admin | Combine into single `sendComposedMessages_` call |
| P1.3 | Redundant `getGroupReg_` in `setGroupStatusStore` | 1 extra SELECT per status change | Pass old `GroupReg` or use `UPDATE ... RETURNING` |

### Priority 2 — N+1 query patterns (moderate impact, moderate effort)

| # | Issue | Impact | Fix |
|---|-------|--------|-----|
| P2.1 | `getAllListedGroups_` N+1 `getGroupLink` | N+1 queries every 5 min | JOIN `user_contact_links` into main query |
| P2.2 | `getOwnersInfo` N+1 `getContact` | 2N queries on admin `/list` | Batch contact lookup with `IN (...)` |
| P2.3 | `searchListedGroups` dual query (data + count) | 2x scan per search | Use `COUNT(*) OVER()` or drop total count |

### Priority 3 — Core architecture (high impact, high effort)

| # | Issue | Impact | Fix |
|---|-------|--------|-----|
| P3.1 | `getGroupMembers` loads ALL members (17k rows, 30+ cols) | 496s / 113 calls | Requires lazy loading / cursor-based approach |
| P3.2 | `setMembersVectorsNewRelation` plpgsql on 17k rows | 1,021s / 39 calls | Rewrite as SQL expression; batch IN clause |
| P3.3 | 34k message INSERTs per member join (17k intros + 17k announcements) | Fundamental to protocol | Potential: defer intros, batch more aggressively |
| P3.4 | `sendHistory` per-item DB calls for file descriptors | ~200 ops per join | Batch file descriptor lookups |

### Priority 4 — External / index issues

| # | Issue | Impact | Fix |
|---|-------|--------|-----|
| P4.1 | Analytics queries missing indexes on `chat_items(item_content_tag)` | 1,067s | Add partial indexes (see below) |
| P4.2 | `LIKE '%...%'` search in `searchListedGroups` | Full scan per search | Consider `pg_trgm` GIN index |

---

## 7. Recommendations

### Immediate (P1 — deploy this week)

**P1.1: Replace `getGroupCurrentMembersCount` with cached value**

At each of the 4 call sites, replace:
```haskell
currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
```
with:
```haskell
let currentMemCount = fromIntegral $ currentMembers $ groupSummary gInfo
```

Then remove `getGroupCurrentMembersCount` from `Groups.hs`.

**Saves: 831s (18% of total DB time), eliminates 93,751 queries.**

### Short-term (P1 + P2 — this sprint)

**P1.2: Combine `sendToApprove` messages**

```haskell
-- Before (Service.hs:710-712):
sendComposedMessage' cc cId Nothing msg
sendMessage' cc cId $ "/approve ..."

-- After:
sendComposedMessages_ cc (SRDirect cId) [(Nothing, msg), (Nothing, MCText $ "/approve ...")]
```

**P2.1: Fix `getAllListedGroups_` N+1**

Add `LEFT JOIN user_contact_links ucl ON ucl.user_id = g.user_id AND ucl.group_id = g.group_id` to `groupReqQuery` variant, return link columns inline.

### Medium-term (P3 — requires design)

**P3.2: Optimize `set_member_vector_new_relation`**

Options:
- Rewrite plpgsql as pure SQL using `overlay()` for fixed-position byte modification
- Batch the IN clause into chunks of ~1000 to reduce lock duration
- Consider moving vector update to async background job

**P3.1: Paginated `getGroupMembers`**

For the introduction path, members could be fetched in cursor-based batches and introduced incrementally, reducing peak memory and query size.

### Index additions

```sql
-- For external analytics queries (P4.1)
CREATE INDEX idx_chat_items_user_item_content_tag_contact
  ON chat_items (user_id, item_content_tag, item_ts)
  WHERE contact_id IS NOT NULL;

-- For getGroupCurrentMembersCount (interim, until P1.1 is deployed)
CREATE INDEX idx_group_members_group_user_status
  ON group_members (group_id, user_id)
  INCLUDE (member_status);
```
