# Invite member via group

## Table of contents

1. Summary
2. Problem
3. Design
4. Implementation plan
5. Tests
6. Later

## 1. Summary

An admin of group A invites a member of group A to group B.
The invitation is sent as `x.grp.group.inv` over the member connection in group A.
The invited member sees the invitation as a group invitation card in group A, visible to this member only.
Group B is added to the chat list of the invited member.
Sending: chat API and bots API.
Receiving: existing invitation card and join flow.

## 2. Problem

Current path from group A to group B:

1. `APICreateMemberContact` (Commands.hs:3349).
2. `APISendMemberContactInvitation`, event `x.grp.direct.inv` (Commands.hs:3366).
3. Contact connection is established.
4. `APIAddMember`, event `x.grp.inv` (Commands.hs:2810).
5. `APIJoinGroup` (Commands.hs:2840).

Steps 1-3 are gated by the direct messages preference of group A.
A direct chat is created for each invited member.

## 3. Design

### 3.1. Protocol

Event:

```
x.grp.group.inv    XGrpGroupInv
```

Params:

```
groupInvitation    GroupInvitation    same type as in x.grp.inv
```

Delivery:

```
sending             sendDirectMemberMessage, member connection in group A
forwarding          excluded (isForwardedGroupMsg, xGrpMsgForward allowlist)
history             excluded (includeInHistory = False for CIRcvGroupInvitation)
hasNotification     True
hasDeliveryReceipt  True
```

Version:

```
memberGroupInvVersion    VersionChat 22
currentChatVersion       VersionChat 22
```

Version 22 is also used in branch `ep/profile-badges` (`signedRelayInvVersion`).
Version 23 is assigned to the branch merged second.

### 3.2. Checks

Sender, in `APIInviteMember`:

| Condition | Error |
|---|---|
| group A ≠ group B | `CEGroupDuplicateMember` |
| group B: `useRelays' = False` | `CECommandError "can't invite member to channel"` |
| group A: `useRelays' = False` | `CECommandError "can't invite member from channel"` |
| user role in group B ≥ `max GRAdmin memberRole` | `assertUserGroupRole` errors |
| user role in group A ≥ `GRAdmin` | `assertUserGroupRole` errors |
| membership in group A: main profile | `CEContactIncognitoCantInvite` |
| membership in group B: main profile | `CEGroupIncognitoCantInvite` |
| member: `memberCurrent`, connection present | `CEGroupMemberNotActive` |
| member version range includes `memberGroupInvVersion` | `CEPeerChatVRangeIncompatible` |

Receiver, in `xGrpGroupInv`:

| Condition | Action on failure |
|---|---|
| group A: `useRelays' = False` | `messageError` |
| group B profile: `publicGroup = Nothing` | `messageError` |
| sender role in group A ≥ `GRAdmin` | `messageError` |
| sender: `memberBlocked = False` | `messageWarning`, invitation ignored |
| `fromMember` role ≥ `GRAdmin` and ≥ `invitedMember` role | `messageError` |
| `fromMember` id ≠ `invitedMember` id | `messageError` |

### 3.3. Data

Sender, invitee row in group B (`group_members`):

```
member_category              invitee
member_status                invited
contact_id                   NULL
contact_profile_id           copy of the group A member profile, badge removed
local_display_name           allocated with the profile copy
invited_by                   user
invited_by_group_member_id   user membership in group B
sent_inv_queue_info          invitation link
peer_chat_min_version        group A member connection
peer_chat_max_version        group A member connection
invited_via_group_member_id  group A member
```

Receiver, host row in group B:

```
member_category              host
member_status                invited
contact_id                   NULL
contact_profile_id           copy of the group A sender profile, badge removed
local_display_name           allocated with the profile copy
invited_by                   unknown
member_pub_key               fromMemberKey
peer_chat_min_version        group A member connection, adjusted (adjustedMemberVRange)
peer_chat_max_version        group A member connection, adjusted (adjustedMemberVRange)
```

Receiver, membership row in group B:

```
invited_by                   unknown
invited_by_group_member_id   host row
member_profile_id            incognito profile of the membership in group A, when incognito
```

Profile copies: `createNewMemberProfile_` (Groups.hs:2437).

New column:

```
group_members.invited_via_group_member_id   REFERENCES group_members ON DELETE SET NULL
idx_group_members_invited_via_group_member_id
```

### 3.4. Sender flow

```
APIInviteMember groupId groupMemberId memberRole
  checks (3.2)
  invitee <- group B row with invited_via_group_member_id = groupMemberId
  invitee:
    none                -> create connection
                           create invitee row and connection row (3.3)
                           send x.grp.group.inv
    status invited      -> update role
                           send x.grp.group.inv with stored sent_inv_queue_info
    other status        -> CEGroupDuplicateMember
  save CISndGroupInvitation in group A, main scope
  respond CRSentGroupInvitationViaGroup
```

After the invited member joins, the inviting client runs the existing invitee flow: `x.grp.acpt` on CONF (Subscriber.hs:786), introductions on CON.

### 3.5. Receiver flow

```
x.grp.group.inv from member m in group A
  checks (3.2)
  group B <- createGroupInvitation, inviter = m
             existing group, found by inv_queue_info
  auto-accept on, membership invited:
    join group B (joinGroupAsync)
    save CIRcvGroupInvitation in group A, status accepted
  auto-accept on, other membership status:
    ignored
  auto-accept off:
    save CIRcvGroupInvitation in group A, status pending
    emit CEvtReceivedGroupInvitationViaGroup
  groups.chat_item_id of group B = saved item
```

Item location: group A, main scope, sender m.

### 3.6. Join and invitation status

`APIJoinGroup`, host row with `contact_id` NULL: peer version range = `memberChatVRange'` of the host row.
`APIJoinGroup`, host row with a contact: unchanged.

Group items are updated by `updateCIGroupInvitationStatus`:

```
status     trigger
pending    invitation received, auto-accept off
accepted   APIJoinGroup succeeded
rejected   group B deleted while pending
```

### 3.7. Clients

Card in group A: `CIGroupInvitationView`, unchanged.

- Kotlin: `CIContent.RcvGroupInvitation` in `ChatItemView.kt:722`.
- iOS: `.rcvGroupInvitation` in `ChatItemView.swift:150`.

Text while pending: "You are invited to group", "Tap to join".

Chat list: group B is added with `updateGroup(groupInfo)` in a new event handler, as in the `receivedGroupInvitation` handler.

- Kotlin: `SimpleXAPI.kt:3122`.
- iOS: `SimpleXAPI.swift:2708`.

Sending: API only.

### 3.8. API

Command:

```
APIInviteMember
  groupId        GroupId          group B
  groupMemberId  GroupMemberId    member of group A
  memberRole     GroupMemberRole  role in group B

/_invite #<groupId> <groupMemberId> <memberRole>
```

Response:

```
CRSentGroupInvitationViaGroup
  user           User
  groupInfo      GroupInfo        group B
  member         GroupMember      invitee row in group B
  viaGroupInfo   GroupInfo        group A
  viaMember      GroupMember      member of group A
```

Event:

```
CEvtReceivedGroupInvitationViaGroup
  user            User
  groupInfo       GroupInfo        group B
  viaGroupInfo    GroupInfo        group A
  viaMember       GroupMember      sender in group A
  fromMemberRole  GroupMemberRole  sender role in group B
  memberRole      GroupMemberRole  invited role in group B
```

## 4. Implementation plan

### 4.1. Protocol.hs

**File:** `src/Simplex/Chat/Protocol.hs`

History line after line 89:

```
-- 22 - group invitations via group member connection (2026-10-10)
```

Line 95:

```haskell
currentChatVersion = VersionChat 22
```

After `anyTextCommandsVersion` (line 144):

```haskell
memberGroupInvVersion :: VersionChat
memberGroupInvVersion = VersionChat 22
```

Event constructor after line 496:

```haskell
  XGrpGroupInv :: GroupInvitation -> ChatMsgEvent 'Json
```

Tag after line 1113:

```haskell
  XGrpGroupInv_ :: CMEventTag 'Json
```

`strEncode` after line 1176:

```haskell
    XGrpGroupInv_ -> "x.grp.group.inv"
```

`strP` after line 1240:

```haskell
        "x.grp.group.inv" -> XGrpGroupInv_
```

`toCMEventTag` after line 1300:

```haskell
  XGrpGroupInv _ -> XGrpGroupInv_
```

`hasNotification` (line 1334) and `hasDeliveryReceipt` (line 1345):

```haskell
  XGrpGroupInv_ -> True
```

`appJsonToCM` after line 1472:

```haskell
      XGrpGroupInv_ -> XGrpGroupInv <$> p "groupInvitation"
```

`chatToAppMessage` after line 1548:

```haskell
      XGrpGroupInv groupInv -> o ["groupInvitation" .= groupInv]
```

### 4.2. Migrations

**Files:**

- `src/Simplex/Chat/Store/SQLite/Migrations/M20261010_invite_member_via_group.hs`
- `src/Simplex/Chat/Store/Postgres/Migrations/M20261010_invite_member_via_group.hs`

SQLite up:

```sql
ALTER TABLE group_members ADD COLUMN invited_via_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE SET NULL;
CREATE INDEX idx_group_members_invited_via_group_member_id ON group_members(invited_via_group_member_id);
```

Postgres up:

```sql
ALTER TABLE group_members ADD COLUMN invited_via_group_member_id BIGINT REFERENCES group_members(group_member_id) ON DELETE SET NULL;
CREATE INDEX idx_group_members_invited_via_group_member_id ON group_members(invited_via_group_member_id);
```

Down, both:

```sql
DROP INDEX idx_group_members_invited_via_group_member_id;
ALTER TABLE group_members DROP COLUMN invited_via_group_member_id;
```

Registration:

- `src/Simplex/Chat/Store/SQLite/Migrations.hs`: import after line 178, list entry after line 354.
- `src/Simplex/Chat/Store/Postgres/Migrations.hs`: import after line 55, list entry after line 108.
- `simplex-chat.cabal`: modules after lines 170 and 347.

Regenerated by tests:

- `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql`
- `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql`
- `src/Simplex/Chat/Store/SQLite/Migrations/chat_query_plans.txt`

### 4.3. Controller.hs

**File:** `src/Simplex/Chat/Controller.hs`

`ChatCommand`, after line 467:

```haskell
  | APIInviteMember {groupId :: GroupId, groupMemberId :: GroupMemberId, memberRole :: GroupMemberRole}
```

`ChatResponse`, after line 879:

```haskell
  | CRSentGroupInvitationViaGroup {user :: User, groupInfo :: GroupInfo, member :: GroupMember, viaGroupInfo :: GroupInfo, viaMember :: GroupMember}
```

`ChatEvent`, after line 1030:

```haskell
  | CEvtReceivedGroupInvitationViaGroup {user :: User, groupInfo :: GroupInfo, viaGroupInfo :: GroupInfo, viaMember :: GroupMember, fromMemberRole :: GroupMemberRole, memberRole :: GroupMemberRole}
```

### 4.4. Store/Groups.hs

**File:** `src/Simplex/Chat/Store/Groups.hs`

#### 4.4.1. `createGroupInvitation` (line 457)

Inviter parameter: `Contact` → `Either Contact GroupMember`.

```
line 458   pattern: Left Contact {localDisplayName, activeConn = Nothing}
line 459   peerChatVRange:
             Left ct  -> connection of ct (unchanged)
             Right m  -> memberChatVRange' m
line 503   host row:
             Left ct  -> createContactMemberInv_ (unchanged)
             Right m  -> createNewMemberProfile_ with (fromLocalProfile $ memberProfile m) {badge = Nothing},
                         INSERT as insertHost_ in createGroupViaLink' (line 887),
                         status GSMemInvited, member_pub_key, peer_chat_min_version, peer_chat_max_version
line 504   membership invitedBy:
             Left ct  -> IBContact contactId
             Right _  -> IBUnknown
```

Existing-group branch (`getInvitationGroupId_`): unchanged.

#### 4.4.2. `createNewMemberViaGroup` (new, after `createNewContactMember`, line 1377)

```haskell
createNewMemberViaGroup :: DB.Connection -> TVar ChaChaDRG -> StoreCxt -> User -> GroupInfo -> GroupMember -> Connection -> GroupMemberRole -> ConnId -> ConnReqInvitation -> SubscriptionMode -> ExceptT StoreError IO GroupMember
createNewMemberViaGroup db gVar cxt user@User {userId} gInfo@GroupInfo {membership} viaMember Connection {connChatVersion, peerChatVRange} memberRole agentConnId connRequest subMode =
  createWithRandomId' db gVar $ \memId -> runExceptT $ do
    createdAt <- liftIO getCurrentTime
    let profile = (fromLocalProfile $ memberProfile viaMember) {badge = Nothing}
    (localDisplayName, memProfileId, badgeVerified) <- createNewMemberProfile_ db cxt user profile createdAt
    let newMember =
          NewGroupMember
            { memInfo = MemberInfo (MemberId memId) memberRole (Just $ ChatVersionRange peerChatVRange) profile Nothing,
              memCategory = GCInviteeMember,
              memStatus = GSMemInvited,
              memRestriction = Nothing,
              memInvitedBy = IBUser,
              memInvitedByGroupMemberId = Just $ groupMemberId' membership,
              localDisplayName,
              memContactId = Nothing,
              memProfileId
            }
    member@GroupMember {groupMemberId} <- createNewMember_ db user gInfo newMember badgeVerified createdAt
    liftIO $ do
      DB.execute
        db
        "UPDATE group_members SET sent_inv_queue_info = ?, invited_via_group_member_id = ? WHERE group_member_id = ?"
        (connRequest, groupMemberId' viaMember, groupMemberId)
      void $ createMemberConnection_ db userId groupMemberId agentConnId connChatVersion peerChatVRange Nothing 0 createdAt subMode
    pure member
```

#### 4.4.3. `getMemberInvitedVia` (new)

```haskell
getMemberInvitedVia :: DB.Connection -> StoreCxt -> User -> GroupInfo -> GroupMember -> IO (Maybe GroupMember)
getMemberInvitedVia db cxt user@User {userId} GroupInfo {groupId} viaMember = do
  ts <- getCurrentTime
  maybeFirstRow (toContactMember ts cxt user) $
    DB.query
      db
      (groupMemberQuery <> " WHERE m.user_id = ? AND m.group_id = ? AND m.invited_via_group_member_id = ?")
      (userId, groupId, groupMemberId' viaMember)
```

### 4.5. Commands.hs

**File:** `src/Simplex/Chat/Library/Commands.hs`

#### 4.5.1. `APIInviteMember` handler (after `APIAddMember`, line 2839)

```haskell
  APIInviteMember groupId gmId memRole -> withUser $ \user -> withGroupLock "inviteMember" groupId $ do
    (GIK gInfo gks, viaGInfo, viaMember) <- withFastStore $ \db -> do
      g <- getGroupInfoKeys db cxt user groupId
      m@GroupMember {groupId = viaGroupId} <- getGroupMemberById db cxt user gmId
      (g,,m) <$> getGroupInfo db cxt user viaGroupId
    let GroupMember {groupId = viaGroupId, localDisplayName = mName} = viaMember
    when (viaGroupId == groupId) $ throwChatError $ CEGroupDuplicateMember mName
    when (useRelays' gInfo) $ throwCmdError "can't invite member to channel"
    when (useRelays' viaGInfo) $ throwCmdError "can't invite member from channel"
    assertUserGroupRole gInfo $ max GRAdmin memRole
    assertUserGroupRole viaGInfo GRAdmin
    when (incognitoMembership viaGInfo) $ throwChatError CEContactIncognitoCantInvite
    when (incognitoMembership gInfo) $ throwChatError CEGroupIncognitoCantInvite
    mConn <- case memberConn viaMember of
      Just conn | memberCurrent viaMember -> pure conn
      _ -> throwChatError CEGroupMemberNotActive
    unless (viaMember `supportsVersion` memberGroupInvVersion) $ throwChatError CEPeerChatVRangeIncompatible
    let sendInvitation = sendGrpInvitationViaGroup user viaGInfo mConn (GIK gInfo gks)
    withFastStore' (\db -> getMemberInvitedVia db cxt user gInfo viaMember) >>= \case
      Nothing -> do
        gVar <- asks random
        subMode <- chatReadVar subscriptionMode
        (agentConnId, CCLink cReq _) <- withAgent $ \a -> createConnection a nm (aUserId user) True False SCMInvitation Nothing Nothing IKPQOff True subMode
        member <- withFastStore $ \db -> createNewMemberViaGroup db gVar cxt user gInfo viaMember mConn memRole agentConnId cReq subMode
        sendInvitation member cReq
        pure $ CRSentGroupInvitationViaGroup user gInfo member viaGInfo viaMember
      Just member@GroupMember {groupMemberId, memberStatus, memberRole = mRole}
        | memberStatus == GSMemInvited -> do
            unless (mRole == memRole) $ withFastStore' $ \db -> updateGroupMemberRole db user member memRole
            withFastStore' (\db -> getMemberInvitation db user groupMemberId) >>= \case
              Just cReq -> do
                sendInvitation member {memberRole = memRole} cReq
                pure $ CRSentGroupInvitationViaGroup user gInfo member {memberRole = memRole} viaGInfo viaMember
              Nothing -> throwChatError $ CEGroupCantResendInvitation gInfo mName
        | otherwise -> throwChatError $ CEGroupDuplicateMember mName
```

#### 4.5.2. `sendGrpInvitation` (line 4292)

The `GroupInvitation` record construction is moved to `mkGroupInvitation`.
`mkGroupInvitation` is used by `sendGrpInvitation` and `sendGrpInvitationViaGroup`.

```haskell
    mkGroupInvitation :: GroupInfoKeys -> GroupMember -> ConnReqInvitation -> GroupInvitation
    mkGroupInvitation (GIK GroupInfo {groupProfile, membership, businessChat, groupSummary} gks) GroupMember {memberId, memberRole = memRole} cReq =
      let GroupMember {memberRole = userRole, memberId = userMemberId} = membership
       in GroupInvitation
            { fromMember = MemberIdRole userMemberId userRole,
              fromMemberKey = Just $ groupMemberKey gks,
              invitedMember = MemberIdRole memberId memRole,
              connRequest = cReq,
              groupProfile,
              business = businessChat,
              groupLinkId = Nothing,
              groupSize = Just $ fromIntegral $ currentMembers groupSummary
            }
```

#### 4.5.3. `sendGrpInvitationViaGroup` (new, after `sendGrpInvitation`)

```haskell
    sendGrpInvitationViaGroup :: User -> GroupInfo -> Connection -> GroupInfoKeys -> GroupMember -> ConnReqInvitation -> CM ()
    sendGrpInvitationViaGroup user viaGInfo mConn g@(GIK GroupInfo {groupId, groupProfile} _) m@GroupMember {groupMemberId, localDisplayName, memberRole = memRole} cReq = do
      (msg, _, _) <- sendDirectMemberMessage mConn (XGrpGroupInv $ mkGroupInvitation g m cReq) (groupId' viaGInfo)
      let content = CISndGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
      ci <- saveSndChatItem user (CDGroupSnd viaGInfo Nothing) msg content
      toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat viaGInfo Nothing) ci]
```

#### 4.5.4. `APIJoinGroup` (line 2840)

```haskell
      (invitation, ct_) <- withFastStore $ \db -> do
        inv@ReceivedGroupInvitation {fromMember} <- getGroupInvitation db cxt user groupId
        (inv,) <$> forM (memberContactId fromMember) (\_ -> getContactViaMember db cxt user fromMember)
      ...
      let peerChatVRange_ = case ct_ of
            Nothing -> Right $ memberChatVRange' fromMember
            Just ct@Contact {activeConn} -> maybe (Left ct) (\Connection {peerChatVRange} -> Right peerChatVRange) activeConn
      case peerChatVRange_ of
        Right peerChatVRange -> do
          ...
        Left ct -> throwChatError $ CEContactNotActive ct
```

The body of the `Right` branch: unchanged.

#### 4.5.5. `updateCIGroupInvitationStatus` (line 4763)

Item binding: `AChatItem _ _ cInfo ci@ChatItem {content, meta = CIMeta {itemId}}`.
New alternative:

```haskell
        (GroupChat g scopeInfo, CIRcvGroupInvitation ciGroupInv@CIGroupInvitation {status} memRole)
          | status == CIGISPending -> do
              let content' = CIRcvGroupInvitation (ciGroupInv {status = newStatus} :: CIGroupInvitation) memRole
              ci' <- withFastStore' $ \db -> updateGroupChatItem db user (groupId' g) ci content' False False Nothing
              toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat g scopeInfo) ci')
```

#### 4.5.6. Parser (after line 6161)

```haskell
      "/_invite #" *> (APIInviteMember <$> A.decimal <* A.space <*> A.decimal <*> memberRole),
```

### 4.6. Subscriber.hs

**File:** `src/Simplex/Chat/Library/Subscriber.hs`

Group event dispatch, after line 1110:

```haskell
              XGrpGroupInv gInv -> Nothing <$ xGrpGroupInv gInfo' m'' conn' gInv msg brokerTs
```

`joinGroupAsync` (line 2695) is moved from `processGroupInvitation` to the `where` block of `processAgentMessageConn` (line 454).
Parameters:

```
GroupInfoKeys        group B
GroupMemberId        host row
Connection           inviter connection
ConnReqInvitation    invitation link
Maybe Contact        host contact, for group links
Bool                 same group link
```

`joinGroupAsync` is used by `processGroupInvitation` and `xGrpGroupInv`.

New handler, after `xGrpDirectInv` (line 3844):

```haskell
    xGrpGroupInv :: GroupInfo -> GroupMember -> Connection -> GroupInvitation -> RcvMessage -> UTCTime -> CM ()
    xGrpGroupInv viaGInfo m mConn inv msg brokerTs
      | useRelays' viaGInfo = messageError "x.grp.group.inv: not allowed in channels"
      | isJust publicGroup = messageError "x.grp.group.inv: can't invite to channel"
      | memberRole' m < GRAdmin = messageError "x.grp.group.inv: sender role"
      | memberBlocked m = messageWarning "x.grp.group.inv: member is blocked (ignoring)"
      | fromRole < GRAdmin || fromRole < memRole = messageError "x.grp.group.inv: inviting member role"
      | fromMemId == memId = messageError "x.grp.group.inv: duplicate member ID"
      | otherwise = do
          memberKeys <- atomically . C.generateKeyPair =<< asks random
          let incognitoProfileId = localProfileId <$> incognitoMembershipProfile viaGInfo
          (g@(GIK gInfo@GroupInfo {groupId, localDisplayName, groupProfile, membership} _), hostId) <-
            withStore $ \db -> createGroupInvitation db cxt user (Right m) inv incognitoProfileId memberKeys
          void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing Nothing (Just epochStart)
          let GroupMember {groupMemberId} = membership
              createInvitationItem invStatus = do
                let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = invStatus}) memRole
                (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv viaGInfo Nothing m) msg brokerTs content
                withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
                toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDRcv cInfo ci]
          if isTrue (autoAcceptGroupInvitations user)
            then when (memberStatus membership == GSMemInvited) $ do
              joinGroupAsync g hostId mConn connRequest Nothing False
              createInvitationItem CIGISAccepted
            else do
              createInvitationItem CIGISPending
              toView CEvtReceivedGroupInvitationViaGroup {user, groupInfo = gInfo, viaGroupInfo = viaGInfo, viaMember = m, fromMemberRole = fromRole, memberRole = memRole}
      where
        GroupInvitation {fromMember = MemberIdRole fromMemId fromRole, invitedMember = MemberIdRole memId memRole, connRequest, groupProfile = GroupProfile {publicGroup}} = inv
```

### 4.7. View.hs

**File:** `src/Simplex/Chat/View.hs`

Responses and events, after lines 210 and 517:

```haskell
  CRSentGroupInvitationViaGroup u g _ viaG viaM -> ttyUser u ["invitation to join the group " <> ttyGroup' g <> " sent to " <> ttyMember viaM <> " in " <> ttyGroup' viaG]
  CEvtReceivedGroupInvitationViaGroup {user = u, groupInfo = g, viaGroupInfo = viaG, viaMember = m, memberRole = r} -> ttyUser u $ viewReceivedGroupInvitationViaGroup g viaG m r
```

`viewReceivedGroupInvitationViaGroup`, after `viewReceivedGroupInvitation` (line 1405):

```
#club: alice in #team invites you to join the group as member
use /j club to accept
```

Join hint lines: shared with `viewReceivedGroupInvitation`.

Group chat items:

- line 735: `CISndGroupInvitation {} -> showSndItemProhibited to` removed.
- line 746: `CIRcvGroupInvitation {} | isJust m_ -> showRcvItemProhibited from` removed.

### 4.8. Bots API

**Files:** `bots/src/API/Docs/*.hs`

`Commands.hs`, after line 121:

```haskell
        ("APIInviteMember", [], "Invite member of another group to group. Requires bot to have Admin role in both groups.", ["CRSentGroupInvitationViaGroup", "CRChatCmdError"], [TD "CEPeerChatVRangeIncompatible" "Member's client version is older than required"], Just UNInteractive, "/_invite #" <> Param "groupId" <> " " <> Param "groupMemberId" <> " " <> Param "memberRole"),
```

`Responses.hs`, after line 94:

```haskell
    ("CRSentGroupInvitationViaGroup", "Group invitation sent to member of another group"),
```

`Events.hs`, after line 87:

```haskell
        ("CEvtReceivedGroupInvitationViaGroup", "Received group invitation from member of another group."),
```

Generated by `tests/APIDocs.hs`:

- `bots/api/COMMANDS.md`
- `bots/api/EVENTS.md`
- `bots/api/TYPES.md`
- `packages/simplex-chat-client/types/typescript/src/*.ts`
- `packages/simplex-chat-python/src/simplex_chat/types/_*.py`

Wrappers, after `apiAddMember`:

- `packages/simplex-chat-nodejs/src/api.ts:565`: `apiInviteMember(groupId, groupMemberId, memberRole): Promise<T.GroupMember>`, response type `sentGroupInvitationViaGroup`.
- `packages/simplex-chat-python/src/simplex_chat/api.py:352`: `api_invite_member(group_id, group_member_id, member_role)`.
- `packages/simplex-chat-client/typescript/src/client.ts:262`: `apiInviteMember`.

### 4.9. Apps

Kotlin, `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`:

- class after line 6817:

```kotlin
  @Serializable @SerialName("receivedGroupInvitationViaGroup") class ReceivedGroupInvitationViaGroup(val user: UserRef, val groupInfo: GroupInfo, val viaGroupInfo: GroupInfo, val viaMember: GroupMember, val memberRole: GroupMemberRole): CR()
```

- `responseType` after line 7017, `details` after line 7208.
- handler after line 3130: `chatModel.chatsContext.updateGroup(rhId, r.groupInfo)` for the active user.

iOS:

- `apps/ios/Shared/Model/AppAPITypes.swift`: `ChatEvent` case after line 1193, `responseType` after line 1274, `details` after line 1358.

```swift
    case receivedGroupInvitationViaGroup(user: UserRef, groupInfo: GroupInfo, viaGroupInfo: GroupInfo, viaMember: GroupMember, memberRole: GroupMemberRole)
```

- `apps/ios/Shared/Model/SimpleXAPI.swift`: handler after line 2714: `m.updateGroup(groupInfo)` for the active user.

### 4.10. Protocol docs

- `docs/protocol/simplex-chat.md`, after line 265:

```
`x.grp.group.inv` message is sent to a group member via the member connection, to invite the member to another group. Params: `groupInvitation`, as in `x.grp.inv`. This message MUST only be sent by members with `admin` or `owner` role in both groups. Receiving clients MUST ignore this message if the sender role in the group of the member connection is below `admin`.
```

- `docs/protocol/simplex-chat.schema.json`: `x.grp.group.inv` entry, params as `x.grp.inv` (line 555).

## 5. Tests

`tests/ProtocolTests.hs`, after line 352:

- `x.grp.group.inv` encoding and parsing, payload as the `x.grp.inv` test (line 349).

`tests/ChatTests/Groups.hs`, new `describe "invite member via group"`:

| Test | Setup | Expected |
|---|---|---|
| `testInviteMemberViaGroup` | alice admin of #team and #club; bob and cath in #team | bob: invitation card in #team; `cath <// 50000`; bob joins #club; #team item accepted; messages in #club |
| `testInviteMemberViaGroupAutoAccept` | bob: `/set accept group invitations on` | bob joins #club on receipt; #team item accepted |
| `testInviteMemberViaGroupRepeat` | alice invites bob twice before join, then after join | bob: one #club; after join: `CEGroupDuplicateMember` |
| `testInviteMemberViaGroupIncognito` | bob in #team incognito | bob joins #club with the #team incognito profile |
| `testInviteMemberViaGroupReject` | bob deletes #club while invited | #team item rejected |
| `testInviteMemberViaGroupProhibited` | alice member role in #team; bob with chat version 21; #club incognito | errors from 3.2 |

`tests/APIDocs.hs`: run to regenerate the files listed in 4.8.

## 6. Later

- `APIMembersRole` for invitees with `invited_via_group_member_id` (Commands.hs:2996): resend `x.grp.group.inv`.
- Card label "Only you can see it" in group chats.
- Card action "Open" for accepted invitations.
- Invitations to members pending approval in group A.
- Sending UI.
