# PostgreSQL Slow Query Analysis — Directory Service

**Date:** 2026-02-16
**Source:** `slow_queries.csv` (100 queries, pg_stat_statements)
**Total time across all queries:** 4,624s

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Time Breakdown by Category](#time-breakdown-by-category)
3. [Actionable Issues](#actionable-issues)
   - [Q#1: getGroupCurrentMembersCount — 831s](#1-getgroupcurrentmemberscount--831s-18-trivial-fix)
   - [Q#2: member_relations_vector UPDATEs — 1,021s](#2-member_relations_vector-updates--1021s-22)
   - [Q#3: External analytics queries — 1,067s](#3-external-analytics-queries--1067s-23)
   - [Q#4: getGroupMembers — 496s](#4-getgroupmembers--496s-11)
   - [Q#5: DELETE FROM messages — 48s](#5-delete-from-messages--48s)
4. [Non-Actionable Hot Paths](#non-actionable-hot-paths)
5. [Proposed Indexes](#proposed-indexes)
6. [Full Query Ranking](#full-query-ranking)

## Executive Summary

Three issues account for 63% of total DB time:

| Issue | Time | Fix |
|-------|------|-----|
| `getGroupCurrentMembersCount` | 831s (18%) | **Trivial** — use existing cached value from `groups.summary_current_members_count` |
| `member_relations_vector` UPDATEs | 1,021s (22%) | Rewrite plpgsql function or batch |
| External analytics queries | 1,067s (23%) | Add missing indexes on `chat_items` |

The first fix is a 4-line code change eliminating 93,751 queries. The second and third require more involved changes.

## Time Breakdown by Category

| Category | Time (s) | % | Calls | Actionable |
|----------|----------|---|-------|------------|
| External analytics queries | 1,067 | 23.1% | ~712 | Indexes only (not in repo) |
| `member_relations_vector` UPDATEs | 1,021 | 22.1% | 39 | plpgsql optimization |
| `getGroupCurrentMembersCount` | 831 | 18.0% | 93,751 | **Trivial fix** |
| `INSERT INTO messages` (2 variants) | 555 | 12.0% | Unavoidable (index overhead) |
| `getGroupMembers` (full member list) | 496 | 10.7% | 113 | Partially addressable |
| Other (90+ queries) | 497 | 10.7% | — | Mostly unavoidable |
| `pending_group_messages` ops | 74 | 1.6% | — | Unavoidable |
| `DELETE FROM messages` | 67 | 1.4% | 41 | Batch optimization possible |

## Actionable Issues

### 1. `getGroupCurrentMembersCount` — 831s (18%), trivial fix

**Query:**
```sql
SELECT member_status FROM group_members WHERE group_id = $1 AND user_id = $2
```

**Stats:** 93,751 calls, 8.87ms avg, 261ms max, 601M rows returned total.

**Source:** `src/Simplex/Chat/Store/Groups.hs:1103`

```haskell
getGroupCurrentMembersCount :: DB.Connection -> User -> GroupInfo -> IO Int
getGroupCurrentMembersCount db User {userId} GroupInfo {groupId} = do
  statuses :: [GroupMemberStatus] <-
    map fromOnly
      <$> DB.query db
        [sql| SELECT member_status FROM group_members WHERE group_id = ? AND user_id = ? |]
        (groupId, userId)
  pure $ length $ filter memberCurrent' statuses
```

This fetches ALL member statuses for a group, then filters in Haskell. For directory-scale groups (17k+ members), this is a full scan per call.

**Root cause:** The result is already cached. Migration `M20250919_group_summary` added `groups.summary_current_members_count` maintained by INSERT/UPDATE/DELETE triggers on `group_members`. This value is loaded into `GroupInfo.groupSummary.currentMembers` by every `getGroupInfo` call (`Shared.hs:674`).

**Call sites** (all have `gInfo :: GroupInfo` in scope):

| File | Line | Context |
|------|------|---------|
| `Library/Subscriber.hs` | 703 | `sendGrpInvitation` — group invitation |
| `Library/Subscriber.hs` | 945 | `checkSendRcpt` — every group message receive |
| `Library/Commands.hs` | 3481 | `sendGrpInvitation` — command path |
| `Library/Internal.hs` | 940 | Joining via group link |

The dominant call site is `Subscriber.hs:945` (`checkSendRcpt`), which fires on every received group message to decide whether to send read receipts for small groups.

**Fix:** At all 4 call sites, replace:
```haskell
currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
```
with:
```haskell
let currentMemCount = fromIntegral $ currentMembers $ groupSummary gInfo
```
Then remove the function.

**Impact:** Eliminates 93,751 queries / 831s (18% of total DB time).

---

### 2. `member_relations_vector` UPDATEs — 1,021s (22%)

**Query:**
```sql
UPDATE group_members
SET member_relations_vector = set_member_vector_new_relation(member_relations_vector, $1, $2, $3),
    updated_at = $4
WHERE group_member_id IN ($5, $6, ..., $17089)
```

**Stats (aggregated across 5 query variants):**

| Calls | Total (ms) | Avg (ms) | Max (ms) | Rows |
|-------|-----------|---------|---------|------|
| 26 | 721,846 | 27,763 | 28,618 | 444,288 |
| 6 | 164,952 | 27,492 | 27,620 | 102,546 |
| 4 | 111,557 | 27,889 | 29,474 | 68,356 |
| 1 | 5,092 | 5,092 | 5,092 | 7,968 |
| 1 | 2,271 | 2,271 | 2,271 | 3,735 |
| 1 | 1,688 | 1,688 | 1,688 | 4,013 |

**Source:** `src/Simplex/Chat/Store/Groups.hs:1615`

```haskell
setMembersVectorsNewRelation :: DB.Connection -> [GroupMember] -> Int64 -> IntroductionDirection -> MemberRelation -> IO ()
```

Called from `Library/Internal.hs:1094` during member introductions.

**The plpgsql function** (`M20251117_member_relations_vector.hs:28`):
```sql
CREATE FUNCTION set_member_vector_new_relation(v BYTEA, idx BIGINT, direction INT, status INT)
RETURNS BYTEA AS $$
  -- per-row: get_byte, set_byte, optional generate_series for extension
$$ LANGUAGE plpgsql IMMUTABLE;
```

**Bottlenecks:**
1. plpgsql function executed per-row (17k+ rows per UPDATE)
2. `generate_series` allocation when extending the vector
3. Massive IN clause (17k+ parameters) causes planning overhead
4. Long lock hold time on `group_members` rows

**Fix options:**
- **Batch the IN clause** into chunks of ~1000 members per UPDATE to reduce lock duration and memory
- **Rewrite as pure SQL** using `overlay()` instead of plpgsql byte manipulation
- **Accept frequency** (39 calls total) and focus on other fixes first

---

### 3. External analytics queries — 1,067s (23%)

These queries are **not in this repository**. They use schema-qualified names (`simplex_v1_chat_schema.`) and run exactly 178 times each, suggesting an external dashboard or cron job.

**Q#3 — 514s (2,886ms avg, 178 calls):**
```sql
SELECT CASE item_sent WHEN $1 THEN $2 WHEN $3 THEN $4 END AS direction,
  count(*) as count
FROM simplex_v1_chat_schema.chat_items
WHERE contact_id IS NOT NULL AND user_id = $5
  AND (item_content_tag = $6 OR item_content_tag = $7)
GROUP BY direction
```

**Q#6 — 412s (2,313ms avg, 178 calls):**
```sql
SELECT $1 as period, count(*) as count
FROM simplex_v1_chat_schema.chat_items
WHERE contact_id IS NOT NULL AND user_id = $2
  AND (item_content_tag = $3 OR item_content_tag = $4)
  AND item_ts > now() - interval $5
UNION ...  -- 3 period buckets
```

**Q#8 — 138s (776ms avg, 178 calls):**
```sql
SELECT $1 AS chat, CASE item_sent ... count(*) as count
FROM simplex_v1_chat_schema.chat_items
WHERE contact_id IS NOT NULL
GROUP BY direction
UNION
SELECT ... FROM simplex_v1_chat_schema.chat_items
WHERE group_id IS NOT NULL
GROUP BY direction
```

**Q#69 — 3s (18ms avg, 178 calls):**
```sql
SELECT COUNT(*) as count FROM simplex_v1_chat_schema.groups g
WHERE EXISTS (SELECT $1 FROM simplex_v1_chat_schema.chat_items i
  WHERE i.group_id = g.group_id AND i.user_id = $2)
```

**Missing indexes:** No index covers `(user_id, item_content_tag)` or `(item_content_tag, item_ts)` for contact-scoped queries. Existing indexes on `chat_items` lead with `(user_id, contact_id, ...)` or `(user_id, group_id, ...)` — none are usable when filtering by `contact_id IS NOT NULL` broadly.

See [Proposed Indexes](#proposed-indexes) section.

---

### 4. `getGroupMembers` — 496s (11%)

**Q#17 — 26s (298ms avg, 88 calls, 4.6M rows):**
```sql
SELECT m.group_member_id, ... FROM group_members m
JOIN contact_profiles p ON ...
LEFT JOIN connections c ON c.group_member_id = m.group_member_id
WHERE m.user_id = $1 AND m.group_id = $2 AND (m.contact_id IS NULL OR m.contact_id != $3)
```

88 calls returning 53k members on average. This is `getGroupMembers` at `Groups.hs:1035`, called from multiple sites in Subscriber.hs, Commands.hs, and Internal.hs.

**Q#5 — 455s (1.80ms avg, 253k calls):**
The `groupInfoQuery` + member lookup, a hot-path query called for every group operation.

**Q#62 — 4s (162ms avg, 25 calls, 427k rows):**
Batch member fetch from `Delivery.hs:342` with huge IN clause (17k+ member IDs).

Not trivially fixable without architectural changes (member caching, pagination).

---

### 5. `DELETE FROM messages` — 48s

**Q#11 — 48s (7,925ms avg, 6 calls, 589k rows):**
```sql
DELETE FROM messages WHERE created_at <= $1
```

Source: `Messages.hs:373` (`deleteOldMessages`). Deletes ~98k rows per call. The `messages` table has 7 indexes, making bulk deletes expensive.

**Q#21 — 19s (544ms avg, 35 calls):**
```sql
DELETE FROM messages WHERE (conn_id, internal_id) IN (
  SELECT conn_id, internal_id FROM messages WHERE internal_ts < $1 ...
```

**Fix options:**
- Batch deletes with LIMIT (e.g., delete 10k rows per iteration)
- Add partial index on `messages(created_at)` WHERE conditions match cleanup patterns

## Non-Actionable Hot Paths

| Query | Time (s) | Calls | Avg (ms) | Notes |
|-------|----------|-------|----------|-------|
| `INSERT INTO messages` (group) | 467 | 769k | 0.61 | 7 indexes per insert |
| `INSERT INTO messages` (rcv) | 43 | 94k | 0.45 | Same table |
| `INSERT INTO pending_group_messages` | 64 | 657k | 0.10 | High volume, low cost |
| `INSERT INTO msg_deliveries` | 40 | 704k | 0.06 | High volume, low cost |
| `getGroupInfo` (Q#5) | 455 | 253k | 1.80 | Fundamental hot path |
| FK checks (`FOR KEY SHARE`) | 37 | 4.7M | 0.01 | PostgreSQL FK enforcement |
| `BEGIN`/`COMMIT` | 4 | 6.2M | 0.00 | Transaction overhead |

## Proposed Indexes

For the external analytics queries (Q#3, Q#6, Q#8):

```sql
-- Covers: WHERE contact_id IS NOT NULL AND user_id = ? AND item_content_tag IN (...)
-- Used by Q#3 (direction counts), Q#6 (period counts)
CREATE INDEX idx_chat_items_user_item_content_tag_contact
  ON chat_items (user_id, item_content_tag, item_ts)
  WHERE contact_id IS NOT NULL;

-- Covers: WHERE group_id IS NOT NULL (for Q#8 group-side UNION)
CREATE INDEX idx_chat_items_item_sent_group
  ON chat_items (item_sent)
  WHERE group_id IS NOT NULL;
```

For the `getGroupCurrentMembersCount` query (until the code fix is deployed):

```sql
-- Covering index to avoid heap fetches
CREATE INDEX idx_group_members_group_user_status
  ON group_members (group_id, user_id) INCLUDE (member_status);
```

## Full Query Ranking

| # | Calls | Total (s) | Avg (ms) | Description |
|---|-------|-----------|----------|-------------|
| 1 | 93,751 | 831 | 8.87 | `SELECT member_status FROM group_members` (getGroupCurrentMembersCount) |
| 2 | 26 | 722 | 27,763 | `UPDATE group_members SET member_relations_vector` (17k members) |
| 3 | 178 | 514 | 2,886 | External: chat_items direction counts |
| 4 | 769,448 | 467 | 0.61 | `INSERT INTO messages` (group send) |
| 5 | 253,087 | 455 | 1.80 | `getGroupInfo` + member lookup |
| 6 | 178 | 412 | 2,313 | External: chat_items period counts |
| 7 | 6 | 165 | 27,492 | `UPDATE group_members SET member_relations_vector` (variant) |
| 8 | 178 | 138 | 776 | External: chat_items by chat type |
| 9 | 4 | 112 | 27,889 | `UPDATE group_members SET member_relations_vector` (variant) |
| 10 | 657,495 | 64 | 0.10 | `INSERT INTO pending_group_messages` |
| 11 | 6 | 48 | 7,925 | `DELETE FROM messages WHERE created_at <= $1` |
| 12 | 93,660 | 43 | 0.45 | `INSERT INTO messages` (rcv) |
| 13 | 703,627 | 40 | 0.06 | `INSERT INTO msg_deliveries` |
| 14 | 93,660 | 32 | 0.35 | `SELECT author/forwarded FROM messages` (dedup check) |
| 15 | 60,620 | 30 | 0.49 | `INSERT INTO messages` (agent snd) |
| 16 | 254,320 | 28 | 0.11 | `SELECT user` (getUser) |
| 17 | 88 | 26 | 298 | `getGroupMembers` (full member list, ~53k members/call) |
| 18 | 60,709 | 24 | 0.39 | `SELECT FROM snd_message_bodies FOR UPDATE` |
| 19 | 435,733 | 21 | 0.05 | `SELECT rcv_queue` |
| 20 | 60,510 | 20 | 0.33 | `INSERT INTO snd_messages` |
