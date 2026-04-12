# Public Group Invitations & Directory Listing

## Goal

Enable public group (channel) subscribers to invite new subscribers via DM (sharing group link + profile), with owners able to prove ownership via a signed invitation. This unblocks directory service support for public groups alongside regular groups.

## Context

### Current state
- Public groups have `PublicGroupProfile {groupType = GTChannel, groupLink, publicGroupId}` and `useRelays = True`
- Public group invitations are explicitly blocked: `Commands.hs:2475` and `Subscriber.hs:2376`
- Users join public groups via link → `APIPrepareGroup` → `APIConnectPreparedGroup`
- Group invitations (`XGrpInv`) carry `connRequest :: ConnReqInvitation` — public groups don't use this
- Directory bot registers groups via group invitation (owner invites bot as admin) — public groups need a different flow

### Owner keys in public group links
- `FixedLinkData.rootKey :: PublicKeyEd25519` — genesis root key
- `UserContactData.owners :: [OwnerAuth]` — chain of authorized owner keys, each signed by root or previous owner
- Public group creator stores `GroupKeys {groupRootKey = GRKPrivate rootPrivKey, memberPrivKey}`
- `memberPrivKey`'s public key = `ownerKey` in the `OwnerAuth` entry (created via `newOwnerAuth` in ShortLink.hs:80)
- `publicGroupId = sha256(rootPubKey)` — immutable group identity
- Verification chain: `validateLinkOwners` (Protocol.hs:1821) validates each `OwnerAuth` signature against root or previous owner

### DR connection shared secret
- Each direct connection has `rcAD` (Associated Data) derived from X3DH key exchange — both sides can independently compute it
- `getConnectionRatchetAdHash` returns `sha256(rcAD)` — available via agent API
- This is the binding for replay protection in owner signatures

### How group invitations work (for comparison)
1. `APIAddMember` (Commands.hs:2470): creates agent connection, creates member record, calls `sendGrpInvitation`
2. `sendGrpInvitation` (Commands.hs:3769): builds `GroupInvitation`, sends `XGrpInv`, creates `CISndGroupInvitation` chat item
3. Receiver `processGroupInvitation` (Subscriber.hs:2374): calls `createGroupInvitation` (Store/Groups.hs:453) which **creates a local group stub** — THEN creates `CIRcvGroupInvitation` referencing the new `groupId`
4. `APIJoinGroup` (Commands.hs:2501): retrieves stored `connRequest`, joins via agent connection
5. `updateCIGroupInvitationStatus` (Commands.hs:4077): finds chat item via `getChatItemByGroupId`, updates status

## Design

### New protocol message: `XGrpInvPub`

```haskell
data PublicGroupInvitation = PublicGroupInvitation
  { groupProfile :: GroupProfile,         -- contains PublicGroupProfile {groupLink, publicGroupId}
    groupOwner :: Maybe GroupOwnerSig,     -- present when sender is group owner
    groupSize :: Maybe Int                -- current subscriber count
  }

data GroupOwnerSig = GroupOwnerSig
  { memberId :: MemberId,                      -- owner's memberId (= ownerId in OwnerAuth list)
    bindingData :: ByteString,                 -- ratchetAdHash (direct) or smpEncode (publicGroupId, memberId) (public group)
    ownerSig :: C.Signature 'C.Ed25519         -- sig(smpEncode chatBinding <> bindingData <> invitationBody)
    -- chatBinding tag is NOT sent — determined from context by both sides
  }
```

Group link and `publicGroupId` come from `groupProfile.publicGroup`.

#### What is signed

Uses the same `MsgSigning`/`ChatBinding` mechanism as relay group message signatures (Protocol.hs:407), extended to support direct chats and non-public groups.

Current binding: `CBGroup` with `bindingData = smpEncode (publicGroupId, memberId)`.

For owner signatures on invitations, the binding depends on where the invitation is sent:

- **Direct chat**: new `CBDirect` with `bindingData = ratchetAdHash` (`sha256(rcAD)` from `getConnectionRatchetAdHash`). Both sides can independently compute it. Prevents replay: Bob can't reuse Alice's signed proof with the bot because `ratchetAdHash` differs per connection.

- **Public group (relay group)**: existing `CBGroup` with `bindingData = smpEncode (publicGroupId, memberId)` — the sender's identity in the group where the invitation is posted.

- **Group without public identity** (groups without `publicGroupId`): no strong binding available. MemberIds are admin-assigned, so an admin could forge a matching memberId in another group and replay a signed invitation. Owner signatures treated as failed verification (`OSSFailed "no group identity for signature verification"`). Invitations can still be shared unsigned.

The signed content is: `smpEncode chatBinding <> bindingData <> invitationBody` signed with `memberPrivKey`.

#### Verification

The verifier (in `LDATA` continuation — link already resolved):
1. From `LDATA` response: `FixedLinkData {rootKey, linkEntityId}` and `UserContactData {owners}`
2. Cross-validates `publicGroupId` from invitation profile matches `linkEntityId` from resolved link → `OSSFailed "group identity mismatch"` if not
3. Determines `chatBinding` from context (direct → `CBDirect`, public group → `CBGroup`, group without identity → `OSSFailed`)
4. Computes expected binding data and compares against `bindingData` from message:
   - Direct: expected = `getConnectionRatchetAdHash` for the contact connection
   - Public group: expected = `smpEncode (publicGroupId, memberId)`
   - Mismatch → `OSSFailed "binding data mismatch"` (possible MitM in direct, or wrong group context)
4. Finds `OwnerAuth` entry where `ownerId == groupOwner.memberId` → `OSSFailed "unknown member ID"` if not found
5. Reconstructs signed content: `smpEncode chatBinding <> bindingData <> invitationBody`
6. Verifies `ownerSig` against that entry's `ownerKey` → `OSSFailed "signature verification failed"` if invalid

Only the owner key matching `memberId` is accepted — not root key, not other owner keys.

**Signing** always uses `memberPrivKey` (whose public key matches one `ownerKey` entry).

#### Signature verification status

Separate type — `MsgSigStatus` doesn't apply (needs pending state, different context):

```haskell
data OwnerSigStatus
  = OSSPending
  | OSSVerified
  | OSSFailed {reason :: Text}  -- e.g. "unknown member ID", "signature failed"
```

Used as `Maybe OwnerSigStatus` on `CIGroupInvitation`:
- `Nothing` — not signed (regular group invitation or unsigned public group invitation)
- `Just OSSPending` — signed, async link resolution in progress
- `Just OSSVerified` — signed and verified, confirmed owner
- `Just OSSFailed {reason}` — signed but verification failed, reason for debugging

#### Async verification flow

Subscriber is single-threaded — cannot block on link resolution.

1. Receive `XGrpInvPub` with `groupOwner = Just sig`
2. Create invisible prepared group + `CIRcvGroupInvitation` with `ownerSigStatus = Just OSSPending`
3. Fire `CEvtNewChatItems`
4. Call `getAgentConnShortLinkAsync` with a new `CommandFunction` (e.g. `CFGetPublicGroupLinkData`) to start async link resolution
5. Agent resolves short link, returns `LDATA` through subscriber
6. `LDATA` handler matches `CFGetPublicGroupLinkData` in `withCompletedCommand` continuation:
   - Gets `FixedLinkData {rootKey, linkEntityId}` and `UserContactData {owners}`
   - Cross-validates `publicGroupId` from profile matches `linkEntityId`
   - Gets `ratchetAdHash` via `getConnectionRatchetAdHash`
   - Finds `OwnerAuth` entry where `ownerId == sig.memberId`
   - Verifies signature → `OSSVerified` or `OSSFailed {reason}`
   - Updates group data from resolved link data (via store)
   - Updates chat item `ownerSigStatus` (via store + `toView`)

This follows the same pattern as `CFGetRelayDataJoin` / `CFGetRelayDataAccept` in Subscriber.hs:1124-1171.

UI presentation based on `isChannel`/group type:
- `Nothing` → neutral invitation display
- `Just OSSPending` → invitation with pending verification indicator
- `Just OSSVerified` → "Invited by channel owner" with verification indicator
- `Just OSSFailed _` → warning indicator

### Protocol wire format

```
event: "x.grp.inv.pub"
params: { "groupInvitation": PublicGroupInvitation }
```

Added to `ChatMsgEvent` GADT, `CMEventTag`, parser (`appJsonToCM`), encoder (`chatToAppMessage`). Same pattern as `XGrpRelayInv`.

### Chat item types: reuse `CIGroupInvitation` with invisible prepared group

On receiving a public group invitation, create an **invisible prepared group** immediately. This gives us a `groupId` so `CIGroupInvitation` works as-is. The group is not visible in the chat list until the user taps the invitation and confirms.

**Extended `CIGroupInvitation`:**
```haskell
data CIGroupInvitation = CIGroupInvitation
  { groupId :: GroupId,
    groupMemberId :: GroupMemberId,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    status :: CIGroupInvitationStatus,
    ownerSigStatus :: Maybe OwnerSigStatus  -- NEW: Nothing for regular group invitations and unsigned public group invitations
  }
```

Existing `CIRcvGroupInvitation` / `CISndGroupInvitation` constructors are reused — no new CIContent cases needed.

Only `CIGISPending` and `CIGISAccepted` statuses are used for public group invitations.

**All platforms** (Swift, Kotlin) add optional `ownerSigStatus` field to `CIGroupInvitation` — backward compatible (`null`/`nil` for regular group invitations).

#### Invisible prepared group lifecycle

**On receive:**
1. Create invisible prepared group from invitation profile data (similar to `APIPrepareGroup` but without network calls)
2. Store group link in the group record
3. Create `CIRcvGroupInvitation` referencing the new `groupId`, with `ownerSigStatus`
4. Group is NOT visible in chat list

**On tap:**
1. Make the group visible (set `hidden = 0`)
2. If link was already resolved (LDATA continuation ran): group data is up-to-date, navigate to group
3. If link was NOT resolved (unsigned invitation, or LDATA failed): resolve link now, update group data
4. User sees group preview, taps Join → `APIConnectPreparedGroup` → update status to `CIGISAccepted`

**Cleanup:**
- Cleanup manager periodically picks up stale invisible groups that were never opened
- If invitation deleted and then a new one arrives for the same group (same `publicGroupId`), reuse the existing invisible group if cleanup hasn't picked it up yet
- On deletion of last referencing invitation item, group cleaned up (unless it was opened and became visible)

**Status tracking:**
- `groups.chat_item_id` points to the latest invitation item — updated on each new invitation
- `updateCIGroupInvitationStatus` / `getChatItemByGroupId` work because we have a local group record
- Only the latest invitation item is updated to `CIGISAccepted` on join; older items stay `CIGISPending`

### Sending flow: `APISharePublicGroup`

New command — `APIAddMember` stays for regular groups only.

```haskell
APISharePublicGroup :: GroupId -> ContactId -> ChatCommand
```

Only subscribers/owners of the public group can call it.

`sendPublicGroupInvitation`:
1. Load `GroupInfo`, verify `useRelays' gInfo` and user is a current member
2. Get `PublicGroupProfile {groupLink, publicGroupId}` from `groupProfile gInfo`
3. If user has `GroupKeys {memberPrivKey}` (is owner):
   - Get `ratchetAdHash` via `getConnectionRatchetAdHash` for the contact's connection (same as `getConnectionCode` at Commands.hs:3340)
   - Build invitation body (JSON-encoded `PublicGroupInvitation` without `groupOwner`)
   - Sign `smpEncode CBDirect <> ratchetAdHash <> invitationBody` with `memberPrivKey`
   - Build `GroupOwnerSig {memberId = membership.memberId, bindingData = ratchetAdHash, ownerSig}`
4. Build `PublicGroupInvitation {groupProfile, groupOwner, groupSize}`
5. Send via `sendDirectContactMessage` as `XGrpInvPub`
6. Create `CISndGroupInvitation` chat item (sender has the group, so `groupId` is available)
7. Return `CRSentPublicGroupInvitation user gInfo contact`

Duplicate invitations to the same contact are allowed — it's just a message.

### Receiving flow

**Handler in Subscriber.hs:**

```haskell
XGrpInvPub inv -> processPublicGroupInvitation ct inv msg msgMeta
```

`processPublicGroupInvitation`:
1. Parse `PublicGroupInvitation {groupProfile, groupOwner, groupSize}`
2. Verify `groupProfile.publicGroup` is present with `groupType == GTChannel`
3. Create invisible prepared group from profile data
4. Create `CIRcvGroupInvitation` chat item with `ownerSigStatus`:
   - `groupOwner = Nothing` → `Nothing`
   - `groupOwner = Just sig` → `Just OSSPending`
5. Fire `CEvtNewChatItems` event
6. If signed: `getAgentConnShortLinkAsync user CFGetPublicGroupLinkData ...` — continuation handles verification and updates (see "Async verification flow" above)

### UI rendering

`CIGroupInvitationView` (both platforms) checks `groupProfile.publicGroup` for group type and `ownerSigStatus` for owner verification:

**Icon:** Based on `groupProfile.publicGroup != nil` — antenna icon for public groups, group icon for regular. Same logic as `chatIconName` / `isChannel`.

**Invitation text** based on `ownerSigStatus`:
- `Nothing` → "You are invited to group/channel" (neutral)
- `Just OSSPending` → "You are invited to group/channel" with pending verification indicator
- `Just OSSVerified` → "Invited by channel owner" (text based on group type) with verification indicator
- `Just OSSFailed _` → warning indicator for failed owner claim

**Tap handler:** Branches on `groupProfile.publicGroup` (NOT `ownerSigStatus` — unsigned public group invitations also need the link-based flow):
- Regular group (`publicGroup == nil`): existing `joinGroup(groupId)` → `APIJoinGroup`
- Public group (`publicGroup != nil`): make invisible group visible, navigate to group, user taps Join → `APIConnectPreparedGroup`

## Directory bot changes

### Public group registration flow

The bot receives `CEvtNewChatItems` with `CIRcvGroupInvitation` that has `ownerSigStatus`. In `crDirectoryEvent_`:

- Check `ownerSigStatus == Just OSSVerified` — reject if not verified owner
- Check `groupProfile.publicGroup` is present (it's a public group)
- Extract group link from `groupProfile.publicGroup.groupLink`
- Check for duplicate groups (by `publicGroupId` or display name)
- Create `GroupReg` (no separate listing category — profile is the source of truth)
- Join public group as subscriber via the link
- Set status `GRSPendingApproval` (skip `GRSPendingUpdate`)
- Notify admin for approval

**Simplified status flow for public groups:**
- `GRSPendingConfirmation` → (if duplicate name) → confirm
- `GRSProposed` → bot joins group
- `GRSPendingApproval` → admin approves
- `GRSActive` → listed
- No `GRSPendingUpdate` step (no welcome message link check)
- No `GRSSuspendedBadRoles` (bot is subscriber, no role to check)

**Profile monitoring:** Bot as subscriber receives `XGrpInfo` instantly when owner updates. On profile change: compare received profile against what the link actually contains (re-resolve link). If they differ (possible modified client attack), do not approve. Periodic re-verification of link data for listed public groups.

**Commands not applicable to public groups:** `/filter`, `/role`, `/link` — bot is subscriber, cannot manage members.

### Search and listing

**Search results:** Include public groups in same query as regular groups. Mark public groups in result text based on `groupProfile.publicGroup` (group type).

**JSON listing:** No changes to `DirectoryEntryType` — `DETGroup` works for both. Consumers distinguish by checking `groupProfile.publicGroup` in the `DirectoryEntry`.

## Adversarial review: coding decisions

### 1. CIGroupInvitation JSON backward compatibility

`$(JQ.deriveJSON defaultJSON ''CIGroupInvitation)` at CIContent.hs:719 — TH-derived `defaultJSON` requires ALL fields present. Adding `ownerSigStatus` breaks parsing of old JSON without this field.

**Decision:** Use `omitNothingField` / `omittedField` pattern so `ownerSigStatus` defaults to `Nothing` when absent. Or use custom JSON instance. Same applies to `DBJSONCIContent` DB serialization.

Swift (`Decodable`): optional fields decode as `nil` when absent — naturally backward compatible.
Kotlin (`@Serializable`): add `val ownerSigStatus: OwnerSigStatus? = null` — default value handles missing field.

### 2. CIGISExpired is dead code

`CIGISExpired` is defined but never used anywhere. `CIGISRejected` is used when deleting/leaving a group (`Commands.hs:1210`). For public group invitations, only `CIGISPending` and `CIGISAccepted` are needed.

### 3. Invisible prepared group: createPreparedGroup vs createGroupInvitation

Two existing patterns:
- `createGroupInvitation` (Groups.hs:415): creates group + host member + user member, stores `inv_queue_info` (connRequest). Used for `XGrpInv`.
- `createPreparedGroup` (Groups.hs ~line 607): creates group with `PreparedGroup` metadata, user member with `GSMemUnknown`, optional host member. Used for `APIPrepareGroup`.

For invisible public group invitations, `createPreparedGroup` is closer — it creates a group from link data without a connRequest. But we need:
- A new flag/state to mark the group as invisible (not shown in chat list)
- No chat banner items on creation (those make it visible)
- The group link stored for later use when tapping

**Decision:** Chat list query filters on `creating_in_progress = 0` (Messages.hs:918). Prepared groups are NOT hidden — they appear as soon as `creating_in_progress = 0`. Options:
- Keep `creating_in_progress = 1` for invisible groups, flip to `0` on tap. Risk: other code may treat `creating_in_progress = 1` as "delete on failure" (see `deleteInProgressGroup`).
- Add new column (e.g. `hidden INTEGER NOT NULL DEFAULT 0`) to groups table, add `AND g.hidden = 0` to chat list query. Cleaner but requires migration.
- Use existing `PreparedGroup` state with `connLinkPreparedConnection = 0` and `connLinkStartedConnection = 0` and filter on these. But these are not currently in the chat list query.

Recommended: new `hidden` column — cleanest separation, no risk of conflating with `creating_in_progress` semantics.

### 4. Tap action: joinGroup vs connect flow

Current `joinGroup` calls `apiJoinGroup(groupId)` → `APIJoinGroup` (Commands.hs:2501), which uses stored `connRequest` from `inv_queue_info`. This does NOT work for public groups — there's no `connRequest`.

**Decision:** Tapping a public group invitation must:
1. Make the invisible group visible
2. Navigate to the group (now visible in chat list as prepared)
3. User sees the group preview and taps "Join" → `APIConnectPreparedGroup`

This is a different UI flow from regular group invitations (which join inline). The `CIGroupInvitationView` tap handler must branch based on `groupProfile.publicGroup` (public group vs regular group) — NOT on `ownerSigStatus`, since unsigned public group invitations also need the link-based flow.

### 5. Icon in CIGroupInvitationView

Currently hardcoded: iOS `"person.2.circle.fill"`, Kotlin `MR.images.ic_supervised_user_circle_filled`.

**Decision:** Check `groupProfile.publicGroup != nil` to select public group icon (antenna: `"antenna.radiowaves.left.and.right.circle.fill"` / equivalent Kotlin resource). Same logic as `chatIconName` in ChatTypes.swift.

### 6. Directory bot: event detection

Bot detects group invitations via `CEvtReceivedGroupInvitation` (Events.hs:78), NOT through `CEvtNewChatItems`. For public group invitations, there is no `CEvtReceivedGroupInvitation` — the chat item arrives through `CEvtNewChatItems`.

Currently `CEvtNewChatItems` in the bot only handles `CIRcvMsgContent (MCText t)` from direct chats (Events.hs:96). Detecting `CIRcvGroupInvitation` with `ownerSigStatus` requires a new pattern match case.

**Decision:** Add a new case in `crDirectoryEvent_` matching `CEvtNewChatItems` with `CIRcvGroupInvitation` content that has `ownerSigStatus /= Nothing`. Map to a new `DEPublicGroupSubmission` event.

Or: add a new `CEvt` event type (`CEvtReceivedPublicGroupInvitation`) emitted alongside `CEvtNewChatItems` in the subscriber, similar to how `CEvtReceivedGroupInvitation` is emitted for regular invitations.

### 7. ChatBinding extension

`ChatBinding` (Protocol.hs:321) currently has single constructor `CBGroup`. Encoding: `"G"` (single char). Adding `CBDirect` = `"D"`.

**Decision:** Add `CBDirect` constructor. Encoding follows single-char pattern. `ChatBinding` is NOT included in the protocol message (`GroupOwnerSig`) — it's determined from context by both sides. It IS used in the signed content for domain separation.

### 8. GroupOwnerSig JSON encoding

`GroupOwnerSig` has `ByteString` field (`bindingData`). Need JSON encoding for the protocol message.

**Decision:** `bindingData` uses base64url encoding (`B64UrlByteString` pattern). `C.Signature` already has JSON instances. `ChatBinding` is NOT in the JSON — determined from context.

### 9. CommandFunction for async link resolution

Add `CFGetPublicGroupLinkData` to `CommandFunction` enum (Types.hs:1871). Expected response: `LDATA_` (same as `CFGetRelayDataJoin`).

**Decision:** The `LDATA` handler in Subscriber.hs needs a new case matching `CFGetPublicGroupLinkData`. This handler needs access to the chat item ID and contact connection to update the item and verify the binding. The command's associated connection (`conn_` parameter in `getAgentConnShortLinkAsync`) provides this context.

### 10. getAgentConnShortLinkAsync connection parameter

`getAgentConnShortLinkAsync user cmdFunc conn_ shortLink` takes `Maybe Connection`. For relay data, `conn_` is `Just conn` (the relay member connection). For public group invitation verification, what connection do we pass?

**Decision:** Pass the contact's connection (`Just conn` from the direct message handler). The `LDATA` response arrives on this connection's subscriber handler. The `withCompletedCommand` pattern recovers the `CommandData` which can carry additional context (but currently only carries `cmdFunction`).

**Confirmed issue:** `LDATA` is ONLY handled inside `processGroupMessage` (Subscriber.hs:1124). There is NO `LDATA` handler in `processDirectMessage`. For public group invitation verification, the async response arrives on the contact's direct connection → `processDirectMessage`. Must add `LDATA` handling to `processDirectMessage` with `CFGetPublicGroupLinkData` case.

### 11. Sending: GroupOwnerSig construction

`sendPublicGroupInvitation` needs `ratchetAdHash`. `getConnectionRatchetAdHash` is accessed via `withAgent`. The contact's `Connection.connId` maps to the agent `ConnId`.

**Decision:** Extract `agentConnId` from contact's `activeConn`, call `withAgent (\a -> getConnectionRatchetAdHash a agentConnId)` to get the hash. This is the same pattern used in `getConnectionCode` (Commands.hs:3340).

### 12. Sending: chat item creation for CISndGroupInvitation

Sender creates `CISndGroupInvitation` with the group's own `groupId`. The `groupMemberId` refers to the user's membership. `ownerSigStatus` on the sent item: set to `Just OSSVerified` if owner (we know our own signature is valid), or `Nothing` if not owner.

### 13. updateCIGroupInvitationStatus for public groups

`updateCIGroupInvitationStatus` (Commands.hs:4077) finds the chat item via `getChatItemByGroupId` which JOINs `groups.chat_item_id`. This only finds ONE item. For multiple invitations to the same group, only the item linked via `chat_item_id` would be updated.

**Decision:** `setGroupInvitationChatItemId` sets the LATEST invitation's chat_item_id on the group. When the user joins, `updateCIGroupInvitationStatus` updates that one item. Older invitation items for the same group stay as `CIGISPending` — acceptable, they still show the correct profile and link. Alternatively, find all invitation items for this group and update them all, but this adds complexity for a minor UX issue.

### 14. Multiple invitations to same public group

If a user receives multiple invitations to the same public group (same `publicGroupId`), reuse the existing invisible group — just create a new chat item referencing the same `groupId`. Update `chat_item_id` on the group to point to the latest item.

**Decision:** Look up invisible group by `publicGroupId` (`public_group_id` column in `group_profiles` table). If found, reuse its `groupId` for the new chat item and update `chat_item_id`. If not found (or cleaned up), create new invisible group.

## Implementation plan

Conceptual — will be grounded into detailed code-level plan once types are approved.

### Step 1: Protocol types
- `PublicGroupInvitation`, `GroupOwnerSig`, `OwnerSigStatus` types with JSON derivation
- `XGrpInvPub` protocol message, tag `"x.grp.inv.pub"`
- Extend `ChatBinding` with `CBDirect` (`"D"`)

### Step 2: Chat item extension
- Add `ownerSigStatus :: Maybe OwnerSigStatus` to `CIGroupInvitation` (Haskell, Swift, Kotlin)
- Haskell: custom JSON handling or `omittedField` for backward compat (defaultJSON breaks)
- Swift/Kotlin: optional field with default nil/null — naturally backward compatible

### Step 3: Invisible prepared group creation
- New store function: create invisible prepared group from `GroupProfile` + link
- `hidden` flag on groups table (or use PreparedGroup state filtered from chat list)
- Lookup by `publicGroupId` to reuse existing invisible group
- Call `setGroupInvitationChatItemId` to link group ↔ chat item
- Cleanup manager for stale invisible groups

### Step 4: Sending — `APISharePublicGroup`
- New `APISharePublicGroup GroupId ContactId` command
- `sendPublicGroupInvitation`: build invitation, sign if owner (using `getConnectionRatchetAdHash` + `memberPrivKey`), send `XGrpInvPub`
- Create `CISndGroupInvitation` with `ownerSigStatus = Just OSSVerified` if owner, `Nothing` if subscriber

### Step 5: Receiving
- `XGrpInvPub` handler in `processDirectMessage`
- Create invisible group, create `CIRcvGroupInvitation` with `OSSPending` if signed
- `getAgentConnShortLinkAsync user CFGetPublicGroupLinkData (Just conn) groupLink`
- Add `CFGetPublicGroupLinkData` to `CommandFunction`
- Add `LDATA` handling in direct connection context (currently only in group member context)

### Step 6: Joining via invitation
- Tap on public group invitation: make group visible, navigate to group, user taps Join
- Different from regular invitation tap (which calls `apiJoinGroup` inline)
- `APIConnectPreparedGroup` flow, then update status to `CIGISAccepted`

### Step 7: UI (iOS + Kotlin)
- `CIGroupInvitationView`: check `ownerSigStatus` + `groupProfile.publicGroup` for rendering
- Icon: antenna for public groups, group icon for regular
- Tap handler: branch on public group vs regular group
- `OSSPending` state rendering
- `AddGroupMembersView`: `apiSharePublicGroup` for public groups

### Step 8: CLI view

### Step 9: Directory bot — public group registration
- New pattern match in `crDirectoryEvent_` for `CIRcvGroupInvitation` with `ownerSigStatus`
- Or new `CEvt` event emitted from subscriber

### Step 10: Directory bot — search and listing
- Mark public groups in search results based on `groupProfile.publicGroup`

### Step 11: Tests

