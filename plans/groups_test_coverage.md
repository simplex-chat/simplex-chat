# Group/Channel Test Coverage Analysis

Coverage run: `cabal test simplex-chat-test --enable-coverage --ghc-options=-O0 --test-options="-m group"`

Full 164 group tests executed (151 passed, 13 failed due to unrelated issues).

## Coverage Summary

After running all group tests:
- Expressions: 48%
- Alternatives: 33%
- Local declarations: 64%
- Top-level: 34%

---

## What IS Covered (Channel-Specific Paths)

- `createNewRcvChatItem` with `CDChannelRcv` - channel message creation
- `toGroupChatItem` with `showGroupAsSender = True` - channel message reading
- `validSender Nothing CIChannelRcv = True` - channel sender validation
- `getGroupChatItemBySharedMsgId` with `Nothing` memberId (`IS NOT DISTINCT FROM`)
- `toCIDirection CDChannelRcv -> CIChannelRcv`
- `toChatInfo CDChannelRcv g s -> GroupChat g s`
- `chatItemMember CIChannelRcv -> Nothing`
- `viewChatItem` for both `CIGroupRcv` and `CIChannelRcv`
- `viewItemReaction` dispatch to `groupReaction` for both constructors
- Channel delete happy path (`channelDelete` -> `delete Nothing`)

---

## Uncovered Code Paths

### 1. Subscriber.hs

#### `processGroupMessage` dispatch (lines 935-972)

| Line | Code | Status |
|------|------|--------|
| 956 | `asGroup == Just True && memberRole' m'' < GROwner` | tickonlyfalse - rejecting non-owner sending as group never tested |
| 963 | `ttl` parameter in `groupMessageUpdate` | nottickedoff |
| 965 | `scope_` parameter in `groupMsgReaction` | nottickedoff |
| 967 | `XFile` handler | nottickedoff |
| 970 | `XFileAcptInv` handler | nottickedoff |
| 987 | `XGrpPrefs` handler | nottickedoff |
| 993 | `BFileChunk` handler | nottickedoff |
| 994 | Catch-all `_` for unsupported messages | nottickedoff |

#### `memberCanSend` / `memberCanSend'` (lines 1446-1454)

| Line | Code | Status |
|------|------|--------|
| 1449 | `memberPending m` part of condition | tickonlytrue - never false |
| 1450 | `otherwise` branch (error "member is not allowed to send messages") | nottickedoff |

#### `newGroupContentMessage` (lines 1876-1940)

| Line | Code | Status |
|------|------|--------|
| 1879 | `vr` parameter in `mkGetMessageChatScope` | nottickedoff |
| 1882 | `ft_` and `False` parameters to `prohibitedGroupContent` | nottickedoff |
| 1883 | `rejected` helper invocation | nottickedoff |
| 1895 | `mentions` parameter for channel messages | nottickedoff |
| 1896 | `pure []` for reactions when `sharedMsgId_` is Nothing | nottickedoff |
| 1901 | `rejected` function body | nottickedoff |
| 1902 | `Just Nothing` for timed_ when forwarded | nottickedoff |
| 1910 | `M.empty` for mentions when blocked | tickonlyfalse |
| 1914 | `gInfo'` and `m'` params to `processFileInv` | nottickedoff |

#### `groupMessageUpdate` (lines 1943-2002)

| Line | Code | Status |
|------|------|--------|
| 1960 | `Nothing -> pure (CDChannelRcv gInfo Nothing, M.empty, Nothing)` | nottickedoff - channel catchCINotFound |
| 1967 | `CDChannelRcv {} -> pure ci'` | nottickedoff |
| 1977 | `mentions' = if memberBlocked m then []` | tickonlyfalse |
| 1980 | `otherwise -> messageError "x.msg.update: group member attempted to update..."` | nottickedoff |
| 1984 | `messageError "x.msg.update: invalid message update"` | nottickedoff |
| 2001-2002 | `CEvtChatItemNotChanged` path | nottickedoff |

#### `groupMessageDelete` (lines 2004-2076)

**channelDelete path:**
| Line | Code | Status |
|------|------|--------|
| 2013 | `messageError "x.msg.del: invalid channel message delete"` | nottickedoff |
| 2015 | `messageError ("x.msg.del: channel message not found, " <> tshow e)` | nottickedoff |

**memberDelete path:**
| Line | Code | Status |
|------|------|--------|
| 2028 | `messageError "x.msg.del: member attempted invalid message delete"` | tickonlyfalse |
| 2036 | `CIChannelRcv -> messageError "x.msg.del: unexpected channel message..."` | nottickedoff |
| 2039 | `messageError ("x.msg.del: message not found, " <> tshow e)` | tickonlyfalse |
| 2041-2042 | `messageError "...message of another member with insufficient..."` | tickonlyfalse |
| 2044-2047 | `createCIModeration` scoped moderation path | nottickedoff |

**moderate helper:**
| Line | Code | Status |
|------|------|--------|
| 2058 | `messageError "x.msg.del: message of another member with incorrect memberId"` | nottickedoff |
| 2059 | `messageError "x.msg.del: message of another member without memberId"` | nottickedoff |
| 2062 | `messageError "...insufficient member permissions"` | tickonlyfalse |

#### `groupMsgReaction` (lines 1818-1860)

| Line | Code | Status |
|------|------|--------|
| 1823-1837 | Entire `catchCINotFound` fallback | nottickedoff |
| 1825-1831 | Scoped reaction path for member with scope | nottickedoff |
| 1832-1834 | Regular group reaction when item not found | nottickedoff |
| 1835-1837 | Channel reaction when item not found | nottickedoff |
| 1839 | `otherwise = pure Nothing` when reactions not allowed | tickonlyfalse |
| 1859 | `Nothing` return for channel (`isJust m_` is False) | nottickedoff |
| 1860 | `pure Nothing` when `ciReactionAllowed` is False | nottickedoff |

#### `validSender` (lines 1871-1874)

| Line | Code | Status |
|------|------|--------|
| 1872 | `validSender (Just mId) (CIGroupRcv m) = sameMemberId mId m` | nottickedoff |
| 1873 | `validSender Nothing CIChannelRcv = True` | **covered** |
| 1874 | `validSender _ _ = False` | nottickedoff |

#### `processForwardedMsg` / `xGrpMsgForward` (lines 3127-3153)

| Line | Code | Status |
|------|------|--------|
| 3133 | `(const Nothing)` wrapper | nottickedoff |
| 3139 | `mentions`, `msgScope`, `ttl`, `live`, `True` to `groupMessageUpdate` | nottickedoff |
| 3141 | `scope_` and `rcvMsg` to `groupMessageDelete` | nottickedoff |
| 3143 | `scope_` to `groupMsgReaction` | nottickedoff |
| 3145 | `XInfo` handler when `author_` is Just | nottickedoff |
| 3152 | `XGrpPrefs` forwarding | nottickedoff |
| 3153 | Catch-all error for unsupported forwarded event | nottickedoff |
| 3311 | `FwdChannel -> (Nothing, Nothing)` | nottickedoff |

---

### 2. View.hs

#### `viewChatItem` (line 646)

| Line | Code | Status |
|------|------|--------|
| 555 | `groupNtf user g mention` - `mention` parameter for channel | nottickedoff |
| 673 | `showSndItemProhibited to` for `CISndGroupInvitation` | nottickedoff |
| 674 | `showSndItem to` fallback for GroupChat | nottickedoff |
| 682 | `CIRcvIntegrityError` in group context | nottickedoff |
| 683 | `CIRcvGroupInvitation` with `isJust m_` guard | nottickedoff |
| 684 | `CIRcvModerated` in group context | nottickedoff |
| 685 | `CIRcvBlocked` in group context | nottickedoff |
| 686 | `showRcvItem from` fallback | nottickedoff |
| 691 | `forwardedFrom` in context computation | nottickedoff |

#### `viewItemUpdate` (line 798)

| Line | Code | Status |
|------|------|--------|
| 819 | `CIGroupRcv m -> updGroupItem (Just m)` | nottickedoff |
| 822 | `CIGroupSnd _ -> []` fallback | nottickedoff |
| 825 | `ttyToGroup g scopeInfo` (non-edited send path) | nottickedoff |
| 830 | `itemLive == Just True && not liveItems -> []` | tickonlyfalse |
| 832 | `_ -> []` fallback for non-message content | nottickedoff |
| 834 | `ttyFromGroup g scopeInfo m_` (non-edited receive path) | nottickedoff |
| 837 | `forwardedFrom` in context | nottickedoff |
| 838 | `groupQuote g` in context | nottickedoff |

#### `viewItemReaction` (line 890)

| Line | Code | Status |
|------|------|--------|
| 898-899 | `sentByMember' g itemDir` in both CIGroupRcv and CIChannelRcv | nottickedoff |
| 913 | `groupReaction _ -> []` (non-message-content fallback) | nottickedoff |
| 917 | `else sentBy` branch when `showGroupAsSender` is False | nottickedoff |
| 958 | `sentByMember'` function | **entirely nottickedoff** |
| 962 | `CIChannelRcv -> Nothing` in sentByMember' | nottickedoff |

#### `viewItemDelete` (line 869)

| Line | Code | Status |
|------|------|--------|
| 880 | `_ -> prohibited` in GroupChat branch | nottickedoff |

#### `viewGroupChatItemsDeleted` (line 866)

| Line | Code | Status |
|------|------|--------|
| 158 | `member_` parameter | nottickedoff |
| 866 | `maybe "" (\m -> " " <> ttyMember m) member_` - empty string fallback | nottickedoff |
| - | Entire function | **entirely nottickedoff** |

#### `groupScopeInfoStr` (line 2785)

| Line | Code | Status |
|------|------|--------|
| - | `Just (GCSIMemberSupport {groupMember_}) -> ...` | nottickedoff |
| - | `Nothing -> "(support)"` sub-branch | nottickedoff |
| - | `Just m -> "(support: " <> viewMemberName m <> ")"` sub-branch | nottickedoff |

#### Scope info display

| Line | Code | Status |
|------|------|--------|
| 2768 | `groupScopeInfoStr scopeInfo` in `ttyToGroup` | nottickedoff |
| 2779 | `groupScopeInfoStr scopeInfo` in `ttyToGroupEdited` | nottickedoff |
| 2782 | `groupScopeInfoStr scopeInfo` in `fromGroupAttention_` | nottickedoff |

#### Other display functions

| Line | Code | Status |
|------|------|--------|
| 625 | `GroupChat g scopeInfo -> ["      " <> ttyToGroup g scopeInfo]` | nottickedoff |
| 766 | `(SMDSnd, GroupChat gInfo _scopeInfo) -> Just $ "you #" <> ...` | nottickedoff |
| 767 | `(SMDRcv, GroupChat gInfo _scopeInfo) -> Just $ "#" <> ...` | nottickedoff |
| 936 | `viewReactionMembers` | **entirely nottickedoff** |
| 1020 | `viewChatCleared` GroupChat branch | nottickedoff |

---

### 3. Internal.hs

#### `saveRcvChatItem'` (lines 2294-2340)

| Line | Code | Status |
|------|------|--------|
| 2288 | `M.empty` for non-group mentions | nottickedoff |
| 2299 | `groupMentions` parameters `db` and `membership` | nottickedoff |
| 2300 | `_ -> pure (M.empty, False)` for non-group | nottickedoff |
| 2303 | `contactChatDeleted cd` | tickonlyfalse |
| 2303 | `vr` parameter in `updateChatTsStats` | nottickedoff |
| 2304 | `else pure $ toChatInfo cd` | nottickedoff |
| 2316 | `getRcvCIMentions` - `db`, `user`, `mentions` parameters | nottickedoff |
| 2319 | `sameMemberId mId membership` in userReply check | nottickedoff |
| 2320 | `\CIMention {memberId} -> sameMemberId memberId membership` | nottickedoff |
| 2311 | `createGroupCIMentions db g ci mentions'` | nottickedoff (mentions always empty) |

#### `memberChatStats` (line 2323)

| Line | Code | Status |
|------|------|--------|
| 2325-2327 | `CDGroupRcv _g (Just scope) m -> ...` | nottickedoff |
| 2328-2329 | `CDChannelRcv _g (Just scope) -> ...` | nottickedoff |
| 2330 | `_ -> Nothing` | nottickedoff |
| - | Entire function | **entirely nottickedoff** |

#### `memberAttentionChange` (line 2621)

| Line | Code | Status |
|------|------|--------|
| - | Entire function | **entirely nottickedoff** |

#### `getRcvCIMentions` (line 277)

| Line | Code | Status |
|------|------|--------|
| 279 | `not (null ft) && not (null mentions) -> ...` | nottickedoff |
| 280 | `uniqueMsgMentions maxRcvMentions mentions $ mentionedNames ft` | nottickedoff |
| 281 | `mapM (getMentionedMemberByMemberId db user groupId) mentions'` | nottickedoff |

#### `uniqueMsgMentions` (line 286)

| Line | Code | Status |
|------|------|--------|
| - | Entire function | **entirely nottickedoff** |

#### `prepareGroupMsg` / `quoteData` (line 204)

| Line | Code | Status |
|------|------|--------|
| 209 | `MCForward $ ExtMsgContent ...` forward branch | nottickedoff |
| 227 | `CIGroupSnd` with `showGroupAsSender` False | nottickedoff |
| 228 | `CIGroupRcv m -> pure (qmc, CIQGroupRcv $ Just m, False, Just m)` | nottickedoff |

#### `mkGetMessageChatScope` (lines 1599-1617)

| Line | Code | Status |
|------|------|--------|
| 1601 | `groupScope@(_gInfo', _m', Just _scopeInfo) -> pure groupScope` | nottickedoff |
| 1604 | `isReport mc -> ...` | tickonlyfalse |
| 1610 | `sameMemberId mId membership -> ...` | nottickedoff |
| 1614 | `otherwise -> do referredMember <- ...` | nottickedoff |
| 1614 | `vr` parameter in `getGroupMemberByMemberId` | nottickedoff |

#### `mkGroupSupportChatInfo` (line 1620)

| Line | Code | Status |
|------|------|--------|
| - | Entire function | **entirely nottickedoff** |

#### Feature checks (tickonlyfalse - never true)

| Line | Code | Status |
|------|------|--------|
| 342 | `isVoice mc && not (groupFeatureMemberAllowed SGFVoice m gInfo)` | tickonlyfalse |
| 344 | `isReport mc && ... not (groupFeatureAllowed SGFReports gInfo)` | tickonlyfalse |
| 485 | `isACIUserMention deletedChatItem` | tickonlyfalse |
| 1593 | `memberPending m` | tickonlyfalse |

#### `sendGroupMessages` (line 1986)

| Line | Code | Status |
|------|------|--------|
| 1989 | `sendProfileUpdate catchAllErrors eToView` | nottickedoff |
| 1995 | `isJust scope = False` branch | nottickedoff |

---

### 4. Messages.hs

#### JSON direction functions - ALL ENTIRELY UNTESTED

**`jsonCIDirection` (lines 314-321):**
| Line | Code | Status |
|------|------|--------|
| 315 | `CIDirectSnd -> JCIDirectSnd` | nottickedoff |
| 316 | `CIDirectRcv -> JCIDirectRcv` | nottickedoff |
| 317 | `CIGroupSnd -> JCIGroupSnd` | nottickedoff |
| 318 | `CIGroupRcv m -> JCIGroupRcv m` | nottickedoff |
| 319 | `CIChannelRcv -> JCIChannelRcv` | nottickedoff |
| 320 | `CILocalSnd -> JCILocalSnd` | nottickedoff |
| 321 | `CILocalRcv -> JCILocalRcv` | nottickedoff |

**`jsonACIDirection` (lines 324-331):**
| Line | Code | Status |
|------|------|--------|
| 325-331 | All branches including `JCIChannelRcv -> ACID SCTGroup SMDRcv CIChannelRcv` | nottickedoff |

**`jsonCIQDirection` (lines 646-651):**
| Line | Code | Status |
|------|------|--------|
| 647 | `CIQDirectSnd -> JCIDirectSnd` | nottickedoff |
| 648 | `CIQDirectRcv -> JCIDirectRcv` | nottickedoff |
| 649 | `CIQGroupSnd -> JCIGroupSnd` | nottickedoff |
| 650 | `CIQGroupRcv (Just m) -> JCIGroupRcv m` | nottickedoff |
| 651 | `CIQGroupRcv Nothing -> JCIChannelRcv` | nottickedoff |

**`jsonACIQDirection` (lines 654-661):**
| Line | Code | Status |
|------|------|--------|
| 655-659 | All branches including `JCIChannelRcv -> Right $ ACIQDirection SCTGroup $ CIQGroupRcv Nothing` | nottickedoff |
| 660 | `JCILocalSnd -> Left "unquotable"` | nottickedoff |
| 661 | `JCILocalRcv -> Left "unquotable"` | nottickedoff |

**ToJSON/FromJSON instances:**
| Line | Code | Status |
|------|------|--------|
| 1469-1470 | `CIDirection` ToJSON | nottickedoff |
| 1473 | `CCIDirection` FromJSON | nottickedoff |
| 1476 | `ACIDirection` FromJSON | nottickedoff |
| 1479 | `CIQDirection` FromJSON | nottickedoff |
| 1482-1483 | `CIQDirection` ToJSON | nottickedoff |

#### Other Messages.hs functions

| Line | Code | Status |
|------|------|--------|
| 372-375 | `chatItemRcvFromMember` | partially covered - `_ -> Nothing` nottickedoff |
| 403 | `toCIDirection CDLocalRcv _ -> CILocalRcv` | nottickedoff |
| 413 | `toChatInfo CDLocalRcv l -> LocalChat l` | nottickedoff |
| 486 | `aChatItemRcvFromMember` | nottickedoff |
| 665 | `quoteMsgDirection CIQDirectSnd -> MDSnd` | nottickedoff |
| 666 | `quoteMsgDirection CIQDirectRcv -> MDRcv` | nottickedoff |

---

### 5. Store/Messages.hs

#### Scope-filtered query functions - ALL ENTIRELY UNTESTED

| Function | Lines | Status |
|----------|-------|--------|
| `findGroupChatPreviews_` | 862-900 | nottickedoff |
| `getChatContentTypes` | 1183-1197 | nottickedoff |
| `getChatItemIDs` | 1476-1505 | nottickedoff |
| `queryUnreadGroupItems` | 1686-1707 | nottickedoff |
| `updateSupportChatItemsRead` | 2038-2077 | nottickedoff |
| `getGroupUnreadTimedItems` | 2080-2102 | nottickedoff |
| `getGroupMemberCIBySharedMsgId` | 2950-2960 | nottickedoff |

#### `toGroupChatItem` (lines 2327-2337)

| Line | Code | Status |
|------|------|--------|
| 2329 | `CIChannelRcv` with file | **covered** |
| 2332 | `CIChannelRcv` without file | **covered** |
| 2334 | `CIGroupRcv member` with file | nottickedoff |
| 2336 | `CIGroupRcv member` without file | nottickedoff |
| 2337 | `badItem` fallback | nottickedoff |
| 2321 | `deletedByGroupMember_` parsing | nottickedoff |

#### `getChatItemQuote_` CDChannelRcv (lines 648-653)

| Line | Code | Status |
|------|------|--------|
| 651 | `mId == userMemberId` check | nottickedoff |
| 651 | `getUserGroupChatItemId_` call | nottickedoff |
| 652 | `otherwise` fallback | nottickedoff |
| 653 | `_ -> pure . ciQuote Nothing $ CIQGroupRcv Nothing` | **covered** |

#### Reaction functions

| Line | Code | Status |
|------|------|--------|
| 3275 | `getGroupCIReactions` | **covered** |
| 3328 | `deleteGroupCIReactions_` | nottickedoff |

---

## Summary

### Well-tested channel paths:
- Channel message create/read/delete happy paths
- Basic channel reactions
- Channel quote creation (quoting nothing)
- `validSender Nothing CIChannelRcv`
- `getGroupChatItemBySharedMsgId` with `Nothing` memberId

### Major gaps:

1. **Non-channel-owner member in channel groups** - `isChannelOwner` always True, `memberForChannel = Just m''` never executed

2. **All JSON serialization for CI directions** - `jsonCIDirection`, `jsonACIDirection`, `jsonCIQDirection`, `jsonACIQDirection` and all `ToJSON`/`FromJSON` instances entirely untested

3. **Member support scope (`GCSIMemberSupport`)** - `mkGroupSupportChatInfo`, `groupScopeInfoStr`, `memberChatStats` entirely untested

4. **Mentions in channel/group messages** - `getRcvCIMentions` with non-empty mentions, `uniqueMsgMentions`, `createGroupCIMentions` never called

5. **Error/fallback paths** - `catchCINotFound` in update/delete/reaction, invalid sender validation, permission errors

6. **Full-delete feature** - `groupFeatureAllowed SGFFullDelete` always false, `deleteGroupCIs` never called

7. **Live message updates** - `itemLive == Just True` always false

8. **Forwarded message handling** - Most parameters to forwarded handlers untested, `FwdChannel` branch untested

9. **View functions** - `sentByMember'`, `viewGroupChatItemsDeleted`, `viewReactionMembers` entirely untested

10. **Scope-filtered store queries** - 7 functions entirely untested

11. **Feature restriction checks** - Voice messages (`SGFVoice`), reports (`SGFReports`) feature checks never triggered
