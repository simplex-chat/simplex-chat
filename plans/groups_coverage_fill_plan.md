# Plan: Filling Group/Channel Test Coverage Gaps

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [Test File Organization](#test-file-organization)
3. [Priority 0: Critical Channel Paths](#priority-0-critical-channel-paths)
4. [Priority 1: Error and Fallback Paths](#priority-1-error-and-fallback-paths)
5. [Priority 2: Scope-Related Features](#priority-2-scope-related-features)
6. [Priority 3: Feature Restrictions](#priority-3-feature-restrictions)

---

## Executive Summary

This plan addresses the coverage gaps identified in `groups_test_coverage.md`, focusing exclusively on DSL-based scenario tests using the existing test infrastructure. All tests follow patterns established in `tests/ChatTests/Groups.hs`.

**Excluded from scope:** JSON serialization tests (per user request).

**Key gap categories:**
- Non-channel-owner members sending in channel groups
- Moderation/delete paths in channels (`memberDelete`)
- Error fallback paths (`catchCINotFound`)
- Member support scope (`GCSIMemberSupport`)
- Full-delete feature, live updates, mentions

---

## Test File Organization

All new tests go in `tests/ChatTests/Groups.hs` under existing or new `describe` blocks.

### New `describe` blocks to add:

```haskell
describe "channel moderation" $ do
  -- Tests for memberDelete path, channel moderation errors

describe "channel error paths" $ do
  -- Tests for catchCINotFound, invalid sender, etc.

describe "channel mentions" $ do
  -- Tests for mentions in channel messages

describe "group full delete feature" $ do
  -- Tests for SGFFullDelete enabled
```

---

## Priority 0: Critical Channel Paths

### Test 1: `testChannelMemberModerate`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "channel moderation"`

**Objective:** Cover `memberDelete` path in `groupMessageDelete` (lines 2016-2076) - moderation of channel messages by admin/owner.

**Scenario:**
1. Create channel with owner (alice) + relay (bob) + members (cath, dan)
2. Owner sends channel message
3. Admin/owner moderates (deletes) the channel message
4. Verify message marked deleted for all members
5. Verify moderation event is forwarded

**Coverage targets:**
- `memberDelete` function execution
- `moderate` helper with role checks
- `delete` with `delMember_` populated

---

### Test 2: `testChannelMemberDeleteError`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "channel error paths"`

**Objective:** Cover error path `CIChannelRcv -> messageError "x.msg.del: unexpected channel message in member delete"` (line 2036).

**Scenario:**
1. Create channel with owner + relay + member
2. Attempt to trigger memberDelete on CIChannelRcv item (malformed delete request)
3. Verify error is logged/handled correctly

**Coverage targets:**
- Line 2036: `CIChannelRcv` error case in `memberDelete`

---

### Test 3: `testChannelUpdateNotFound`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "channel error paths"`

**Objective:** Cover `catchCINotFound` fallback in `groupMessageUpdate` (lines 1950-1969) - update arrives for locally deleted item.

**Scenario:**
1. Create channel with owner + relay + member
2. Owner sends message, member receives
3. Member locally deletes the message
4. Owner updates the message
5. Verify member creates new item from update (fallback path)

**Coverage targets:**
- Line 1960: `Nothing -> pure (CDChannelRcv gInfo Nothing, M.empty, Nothing)`
- Lines 1951-1969: create-from-update fallback

---

### Test 4: `testChannelReactionNotFound`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "channel error paths"`

**Objective:** Cover `catchCINotFound` fallback in `groupMsgReaction` (lines 1823-1837) - reaction on locally deleted item.

**Scenario:**
1. Create channel with owner + relay + member
2. Owner sends message, member receives
3. Member locally deletes the message
4. Owner adds reaction
5. Verify reaction is handled without crash

**Coverage targets:**
- Lines 1835-1837: channel reaction fallback

---

### Test 5: `testChannelForwardedMessages`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "relay delivery"` (existing)

**Objective:** Cover `FwdChannel` branch in delivery task (line 3311) and forwarded message parameters.

**Scenario:**
1. Create channel with owner + 2 relays + members
2. Send various message types (new, update, delete, reaction)
3. Verify all are forwarded through relay chain
4. Check forwarded parameters are correctly passed

**Coverage targets:**
- Line 3311: `FwdChannel -> (Nothing, Nothing)`
- Lines 3139-3145: forwarded message handlers

---

## Priority 1: Error and Fallback Paths

### Test 6: `testGroupDeleteNotFound`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "channel error paths"` or existing moderation tests

**Objective:** Cover delete error when message not found (line 2039).

**Scenario:**
1. Create group with alice, bob
2. Bob sends message
3. Alice locally deletes it
4. Bob broadcasts delete for the same message
5. Verify error path is handled

**Coverage targets:**
- Line 2039: `messageError ("x.msg.del: message not found, " <> tshow e)`

---

### Test 7: `testGroupInvalidSenderUpdate`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "channel error paths"`

**Objective:** Cover `validSender _ _ = False` (line 1874) and update from wrong member error (line 1980).

**Scenario:**
1. Create group with alice, bob, cath
2. Bob sends message
3. Cath (with spoofed member ID) attempts to update bob's message
4. Verify error is thrown

**Coverage targets:**
- Line 1874: `validSender _ _ = False`
- Line 1980: `messageError "x.msg.update: group member attempted to update..."`

---

### Test 8: `testGroupReactionDisabled`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** existing `describe "group message reactions"`

**Objective:** Cover reaction disabled path (line 1839).

**Scenario:**
1. Create group with reactions feature disabled
2. Member attempts to add reaction
3. Verify reaction is rejected

**Coverage targets:**
- Line 1839: `otherwise = pure Nothing` when reactions not allowed

---

### Test 9: `testChannelItemNotChanged`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "channel message operations"` (existing)

**Objective:** Cover `CEvtChatItemNotChanged` path (lines 2001-2002) - update with same content.

**Scenario:**
1. Create channel with owner + relay + member
2. Owner sends message
3. Owner "updates" message with identical content
4. Verify no change event is emitted

**Coverage targets:**
- Lines 2001-2002: `CEvtChatItemNotChanged` path

---

## Priority 2: Scope-Related Features

### Test 10: `testScopedSupportMentions`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "group scoped messages"` (existing)

**Objective:** Cover mentions in scoped support messages (`getRcvCIMentions` with non-empty mentions).

**Scenario:**
1. Create group with alice (owner), bob (member), dan (moderator)
2. Bob sends support message mentioning @alice
3. Alice receives with mention highlighted
4. Verify `userMention` flag is set correctly

**Coverage targets:**
- Line 2316: `getRcvCIMentions` with actual mentions
- Line 2319: `sameMemberId mId membership` in userReply check
- Lines 279-281: `uniqueMsgMentions` path

---

### Test 11: `testMemberChatStats`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "group scoped messages"` (existing)

**Objective:** Cover `memberChatStats` function (lines 2323-2330) for both `CDGroupRcv` and `CDChannelRcv` with scope.

**Scenario:**
1. Create group with support enabled
2. Member sends support message
3. Verify unread stats are updated
4. Verify `memberAttentionChange` is computed

**Coverage targets:**
- Lines 2325-2329: `memberChatStats` branches
- Line 2621: `memberAttentionChange`

**Note:** Tests `testScopedSupportUnreadStatsOnRead` and `testScopedSupportUnreadStatsOnDelete` exist but may not cover all branches.

---

### Test 12: `testMkGetMessageChatScope`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "group scoped messages"` (existing)

**Objective:** Cover `mkGetMessageChatScope` branches (lines 1599-1617).

**Scenario:**
1. Create group with pending member (knocking)
2. Pending member sends message with scope
3. Verify correct scope resolution
4. Test with `isReport mc` content type

**Coverage targets:**
- Line 1601: `Just _scopeInfo` return
- Line 1604: `isReport mc` branch
- Lines 1610-1617: `sameMemberId` and `otherwise` branches

---

## Priority 3: Feature Restrictions

### Test 13: `testGroupFullDelete`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** new `describe "group full delete feature"`

**Objective:** Cover `groupFeatureAllowed SGFFullDelete` = True path (line 2067) - `deleteGroupCIs` instead of `markGroupCIsDeleted`.

**Scenario:**
1. Create group with full delete enabled: `/set delete #team on`
2. Bob sends message
3. Alice (or bob) deletes message
4. Verify message is fully deleted (not just marked)

**Coverage targets:**
- Line 2067: `deleteGroupCIs` path
- `groupFeatureAllowed SGFFullDelete` returns True

---

### Test 14: `testGroupLiveMessage`
**File:** `tests/ChatTests/Groups.hs`
**Note:** `testGroupLiveMessage` exists but may not cover update path.

**Objective:** Cover live message update path (line 830 in View.hs, `itemLive == Just True`).

**Scenario:**
1. Create group
2. Send live message
3. Update live message content
4. Verify live update is processed

**Coverage targets:**
- Line 830: `itemLive == Just True && not liveItems -> []`
- Live update in `groupMessageUpdate`

---

### Test 15: `testGroupVoiceDisabled`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** existing tests or new `describe "group feature restrictions"`

**Objective:** Cover voice message rejection (line 342 in Internal.hs).

**Scenario:**
1. Create group with voice disabled: `/set voice #team off`
2. Member attempts to send voice message
3. Verify rejection

**Coverage targets:**
- Line 342: `isVoice mc && not (groupFeatureMemberAllowed SGFVoice m gInfo)`

---

### Test 16: `testGroupReportsDisabled`
**File:** `tests/ChatTests/Groups.hs`
**Add to:** `describe "group member reports"` (existing)

**Objective:** Cover reports disabled path (line 344 in Internal.hs).

**Scenario:**
1. Create group with reports disabled
2. Member attempts to send report
3. Verify rejection

**Coverage targets:**
- Line 344: `isReport mc && ... not (groupFeatureAllowed SGFReports gInfo)`

---

## Implementation Order

1. **Phase 1 (P0):** Tests 1-5 - Critical channel paths
2. **Phase 2 (P1):** Tests 6-9 - Error and fallback paths
3. **Phase 3 (P2):** Tests 10-12 - Scope-related features
4. **Phase 4 (P3):** Tests 13-16 - Feature restrictions

Each test should:
- Use existing DSL operators (`##>`, `<#`, `#$>`, etc.)
- Follow naming convention `test<Feature><Scenario>`
- Include `HasCallStack` constraint
- Use appropriate test helpers (`createGroup2`, `createChannel1Relay`, etc.)

---

## Dependencies

- Existing test infrastructure in `ChatTests.Utils`
- Helper functions: `createChannel1Relay`, `createGroup2`, `createGroup3`, etc.
- DSL operators for assertions

## Estimated New Tests: 16

## Files Modified: 1
- `tests/ChatTests/Groups.hs`
