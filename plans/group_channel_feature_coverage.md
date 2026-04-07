# Group & Channel Feature Test Coverage Plan

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Feature Coverage Matrix](#feature-coverage-matrix)
3. [Gap Analysis by Category](#gap-analysis-by-category)
4. [Recommended New Tests](#recommended-new-tests)
5. [Implementation Roadmap](#implementation-roadmap)

---

## Executive Summary

**Current State:** The test suite in `Groups.hs` provides comprehensive coverage across 120+ scenarios in 14 categories. Core functionality (group CRUD, messaging, member management) is well-tested.

**Key Gaps Identified:**
- Business/contact card group links (untested invitation flow)
- Legacy group link auto-accept path
- Permission enforcement for `SGFFullDelete`
- Error recovery paths (file transfers, database busy, duplicate forwarding)
- Moderator-only scoped message delivery (`DJSMemberSupport`)
- Edge cases in channel message deletion

**Risk Assessment:**
| Priority | Gap Count | Impact |
|----------|-----------|--------|
| Critical | 3 | Production failures in business flows |
| High | 5 | Feature regressions possible |
| Medium | 4 | Edge case handling incomplete |

**Recommendation:** Add 12 new test scenarios in 3 phases over 2 sprints.

---

## Feature Coverage Matrix

### Legend
- ✅ Tested (comprehensive)
- ⚠️ Partial (some paths covered)
- ❌ Untested

### Core Group Operations

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| Group creation | ✅ | `testGroup` | Basic + edge cases |
| Group deletion | ✅ | `testGroupDelete*` | Multiple scenarios |
| Group naming/description | ✅ | `testUpdateGroupProfile` | |
| Group preferences | ✅ | `testGroupPreferences` | Voice, files, etc. |
| Group link creation | ✅ | `testGroupLink*` | |
| Group link via contact card | ❌ | - | Business links untested |
| Legacy auto-accept | ❌ | - | Deprecated path |

### Message Operations

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| XMsgNew (send) | ✅ | Multiple | Core flow |
| XMsgUpdate (edit) | ✅ | `testGroupMessageUpdate` | |
| XMsgDel (delete) | ✅ | `testGroupMessageDelete` | |
| XMsgReact | ✅ | `testGroupMsgReaction` | |
| XMsgFileDescr | ✅ | `testGroupFileTransfer` | |
| Batch messages | ✅ | `testBatch*` | |
| Live messages | ✅ | `testGroupLiveMessage` | |
| Quote messages | ✅ | `testGroup*Quote*` | |
| Duplicate forwarding | ❌ | - | De-dup logic untested |

### Member Management

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| Member add | ✅ | `testGroupAddMember*` | |
| Member remove | ✅ | `testGroupRemoveMember*` | |
| Member roles | ✅ | `testGroupMemberRole*` | |
| Member blocking | ✅ | `testGroupBlock*` | |
| Member merging | ✅ | `testMergeMemberContact*` | |
| Member deletion errors | ❌ | - | Error paths missing |
| Contact from member | ✅ | `testCreateMemberContact*` | |

### Moderation & Full Delete

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| Moderate message | ✅ | `testGroupModerate*` | |
| Block for all | ✅ | `testGroupBlockForAll*` | |
| SGFFullDelete enabled | ✅ | `testFullDeleteGroup*` | |
| SGFFullDelete restricted | ❌ | - | Permission checks |

### Channels & Relays

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| 1-relay delivery | ✅ | `testChannel1Relay*` | |
| 2-relay delivery | ✅ | `testChannel2Relay*` | |
| Owner-only sending | ✅ | `testChannel*Message*` | |
| Identity protection | ✅ | `testChannel*Incognito*` | |
| Channel msg delete errors | ❌ | - | Invalid state handling |

### Scoped Messages (Support Chats)

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| Single moderator | ✅ | `testSupportChat*` | |
| Multi moderator | ✅ | `testSupportChat*Multi*` | |
| Member reports | ✅ | `testReportMessage*` | |
| Forwarding in scope | ✅ | `testSupportChatForward*` | |
| Stats | ✅ | `testSupportChatStats` | |
| DJSMemberSupport delivery | ❌ | - | Moderator-only path |

### Group Links & Invitations

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| Create/delete link | ✅ | `testGroupLink*` | |
| Join via link | ✅ | `testGroupLink*` | |
| Link screening | ✅ | `testGroupLink*Screening*` | |
| Connection plans | ✅ | `testPlanGroupLink*` | |
| Short links | ✅ | `testGroupShortLink*` | |
| Business link invitation | ❌ | - | Contact card flow |

### Error Handling

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| CEGroupNotJoined | ⚠️ | Implicit | Some coverage |
| CEGroupMemberNotFound | ⚠️ | Implicit | Some coverage |
| File transfer errors | ❌ | - | Recovery paths |
| Database busy | ❌ | - | Retry logic |
| Simplex link warnings | ❌ | - | Feature gate |

### History & Disappearing

| Feature | Status | Test Location | Notes |
|---------|--------|---------------|-------|
| History on join | ✅ | `testGroupHistory*` | |
| File history | ✅ | `testGroupHistoryFiles` | |
| Disappearing messages | ✅ | `testGroupHistoryDisappear*` | |

---

## Gap Analysis by Category

### Critical Priority (Production Impact)

#### 1. Business Group Link via Contact Card
**Location:** `APIAddMember` with `InvitationContact` path
**Risk:** Business users cannot invite via contact cards
**Current State:** Only `InvitationMember` path tested
**Missing Coverage:**
- `processGroupInvitation` with `CTContactRequest`
- Auto-accept flow for business links
- Profile merge on business join

#### 2. SGFFullDelete Permission Enforcement
**Location:** `canFullDelete`, `checkFullDeleteAllowed`
**Risk:** Non-admins might delete others' messages
**Missing Coverage:**
- `SGFFullDelete` set to `FAAdmins` restriction
- Error `CECommandError` when non-admin attempts full delete
- Role-based permission matrix

#### 3. DJSMemberSupport Delivery Path
**Location:** `deliverGroupMessages`, `groupMsgDeliveryJobs`
**Risk:** Support messages not reaching moderators correctly
**Missing Coverage:**
- `DJSMemberSupport` job creation
- Moderator-only broadcast logic
- Scope isolation verification

### High Priority (Feature Regressions)

#### 4. Channel Message Deletion Errors
**Location:** `apiDeleteMemberChatItem`, `deleteGroupChatItemInternal`
**Missing Coverage:**
- Delete non-existent channel message
- Delete by non-owner in channel
- `CEInvalidChatItemDelete` error path

#### 5. Member Deletion Error Paths
**Location:** `removeMemberDeleteItem`, `deleteGroupChatItem`
**Missing Coverage:**
- Delete item for already-removed member
- Concurrent deletion race condition
- `CEGroupMemberNotFound` specific handling

#### 6. File Transfer Error Recovery
**Location:** `rcvFileError`, `sndFileError`
**Missing Coverage:**
- Partial transfer resume
- `CEFileTransferError` handling
- Cleanup on failed transfers

#### 7. Legacy Group Link Auto-Accept
**Location:** `processGroupInvitation`, `autoAcceptGroupLink`
**Risk:** Breaking change for older clients
**Missing Coverage:**
- V1 protocol compatibility
- Auto-accept timing

#### 8. Duplicate Message Forwarding
**Location:** `forwardGroupMessage`, `checkDuplicateForward`
**Missing Coverage:**
- Same message forwarded twice
- De-duplication by `sharedMsgId`
- UI state consistency

### Medium Priority (Edge Cases)

#### 9. Simplex Links Feature Warnings
**Location:** `simplexLinkWarning`, `SGFSimplexLinks`
**Missing Coverage:**
- Warning when feature disabled
- Link detection in messages
- User preference override

#### 10. Database Busy Error Handling
**Location:** `withTransaction`, `retryOnBusy`
**Missing Coverage:**
- Concurrent group operations
- Retry exhaustion
- State consistency after retry

#### 11. Invalid Channel/Member Scope Errors
**Location:** `validateGroupChatScope`, `scopeNotAllowed`
**Missing Coverage:**
- Member sending to wrong scope
- Scope mismatch on receive
- `CECommandError "scope not allowed"` path

#### 12. Contact Card Profile Merge
**Location:** `mergeMemberContactProfile`, `updateContactProfile`
**Missing Coverage:**
- Profile conflict resolution
- Image merge logic
- Display name precedence

---

## Recommended New Tests

### Phase 1: Critical (Sprint 1)

```haskell
-- Test 1: Business Group Link Invitation
testBusinessGroupLinkInvitation :: HasCallStack => TestParams -> IO ()
-- Covers: InvitationContact path, CTContactRequest, auto-accept

-- Test 2: Full Delete Permission Restriction
testFullDeletePermissionRestricted :: HasCallStack => TestParams -> IO ()
-- Covers: SGFFullDelete FAAdmins, non-admin rejection, CECommandError

-- Test 3: Moderator-Only Support Delivery
testSupportChatModeratorOnlyDelivery :: HasCallStack => TestParams -> IO ()
-- Covers: DJSMemberSupport, moderator broadcast, scope isolation
```

### Phase 2: High (Sprint 1-2)

```haskell
-- Test 4: Channel Message Delete Errors
testChannelMessageDeleteErrors :: HasCallStack => TestParams -> IO ()
-- Covers: non-existent delete, non-owner delete, CEInvalidChatItemDelete

-- Test 5: Member Deletion Error Paths
testMemberDeletionErrorPaths :: HasCallStack => TestParams -> IO ()
-- Covers: removed member delete, concurrent delete, CEGroupMemberNotFound

-- Test 6: File Transfer Error Recovery
testGroupFileTransferErrorRecovery :: HasCallStack => TestParams -> IO ()
-- Covers: partial resume, CEFileTransferError, cleanup

-- Test 7: Legacy Group Link Compatibility
testLegacyGroupLinkAutoAccept :: HasCallStack => TestParams -> IO ()
-- Covers: V1 protocol, auto-accept timing

-- Test 8: Duplicate Forward Prevention
testDuplicateMessageForwardPrevention :: HasCallStack => TestParams -> IO ()
-- Covers: duplicate detection, sharedMsgId, UI consistency
```

### Phase 3: Medium (Sprint 2)

```haskell
-- Test 9: Simplex Links Feature Warning
testSimplexLinksFeatureWarning :: HasCallStack => TestParams -> IO ()
-- Covers: disabled feature warning, link detection

-- Test 10: Database Busy Retry
testGroupOperationsDatabaseBusy :: HasCallStack => TestParams -> IO ()
-- Covers: concurrent ops, retry logic, state consistency

-- Test 11: Scope Validation Errors
testGroupChatScopeValidationErrors :: HasCallStack => TestParams -> IO ()
-- Covers: wrong scope send, scope mismatch, CECommandError

-- Test 12: Contact Card Profile Merge
testMemberContactProfileMerge :: HasCallStack => TestParams -> IO ()
-- Covers: conflict resolution, image merge, name precedence
```

---

## Implementation Roadmap

### Sprint 1 (Week 1-2)

| Day | Task | Owner | Deliverable |
|-----|------|-------|-------------|
| 1-2 | Test 1: Business link | - | PR ready |
| 3-4 | Test 2: Full delete perms | - | PR ready |
| 5 | Test 3: Moderator delivery | - | PR ready |
| 6-7 | Test 4: Channel delete errors | - | PR ready |
| 8-9 | Test 5: Member delete errors | - | PR ready |
| 10 | Integration + Review | - | Merged |

### Sprint 2 (Week 3-4)

| Day | Task | Owner | Deliverable |
|-----|------|-------|-------------|
| 1-2 | Test 6: File error recovery | - | PR ready |
| 3-4 | Test 7: Legacy link compat | - | PR ready |
| 5-6 | Test 8: Duplicate forward | - | PR ready |
| 7-8 | Tests 9-12: Medium priority | - | PR ready |
| 9-10 | Final integration + CI | - | Release |

### Dependencies

```
Test 1 (Business Link) ─┬─> Test 12 (Profile Merge)
                        │
Test 3 (Moderator) ─────┴─> Test 11 (Scope Validation)

Test 4 (Channel Delete) ──> Test 5 (Member Delete)

Test 6 (File Error) ──────> (standalone)

Test 7 (Legacy Link) ─────> Test 1 (Business Link)

Test 8 (Duplicate) ───────> (standalone)

Tests 9, 10 ──────────────> (standalone)
```

### Success Criteria

1. **Coverage Target:** 95%+ of identified gaps covered
2. **CI Integration:** All tests in nightly suite
3. **Documentation:** Test rationale in docstrings
4. **No Regressions:** Existing 120+ tests still pass

### Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Test flakiness | Use explicit waits, avoid timing assumptions |
| Database state leaks | Ensure proper cleanup in each test |
| Protocol version issues | Test both V1 and V2 where applicable |
| CI timeout | Parallelize independent tests |

---

## Appendix: Test File Locations

| Test Category | Primary File | Secondary |
|---------------|--------------|-----------|
| Group Core | `tests/ChatTests/Groups.hs` | - |
| Channels | `tests/ChatTests/Groups.hs` | `Channels/` if split |
| Support Chats | `tests/ChatTests/Groups.hs` | `ScopedMessages/` if split |
| File Transfers | `tests/ChatTests/Files.hs` | `Groups.hs` |
| Error Handling | Inline with feature tests | - |

---

*Generated: 2026-02-06*
*Branch: ep/channel-messages-2*
*Coverage baseline: 120+ scenarios, 14 categories*
