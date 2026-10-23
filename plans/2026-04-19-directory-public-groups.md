# Directory Service — Public Group Registration via Chat Cards

## Goal

Enable directory registration of public groups (channels and future group types) via MCChat cards shared in DM with the bot. Replaces the admin-invitation flow with a signature-verified card flow.

## Background

### Current group registration flow
1. Owner invites bot as admin member
2. Bot joins, creates group link, asks owner to add link to welcome message
3. Owner updates profile with link → bot sends for admin approval
4. Admin approves → group listed

This requires the bot to be admin. Public groups don't need this — they already have a public link, and ownership is proven via `ownerSig` on the MCChat card.

### Public group identity
- `PublicGroupProfile {groupType :: GroupType, groupLink :: ShortLinkContact, publicGroupId :: B64UrlByteString}`
- `publicGroupId = sha256(rootKey)` — immutable identity
- `GroupType`: currently `GTChannel`, adding `GTGroup` for forward compatibility
- `GroupKeys {publicGroupId, groupRootKey, memberPrivKey}` — owner's signing keys
- `ownerId` in `LinkOwnerSig` = `B64UrlByteString (unMemberId memberId)` — the owner's MemberId bytes

### ownerId-to-member mapping
- `LinkOwnerSig.ownerId = Just (B64UrlByteString unMemberId)` — same raw bytes as `MemberId`
- `createLinkOwnerMember` (called during `APIConnectPreparedGroup`, Commands.hs:2129) creates a member record with `memberRole = GROwner`, `memberStatus = GSMemUnknown`, `memberContactId = Nothing`
- `GroupMemberId` is available immediately after `APIConnectPreparedGroup`
- `getGroupMemberIdViaMemberId db user gInfo (MemberId ownerId)` looks up `GroupMemberId` from `MemberId`

### Owner member activation
When a relay announces the pre-created `GSMemUnknown` member, `CEvtUnknownMemberAnnounced` fires (Subscriber.hs:2872, via `xGrpMemNew`). The member's profile and role are updated from the announcement's `MemberInfo` (via `updateUnknownMemberAnnounced`, Groups.hs:3010) — the role reflects the member's actual current role, not the pre-created `GROwner`. This event is not currently handled in directory Events.hs.

### connectPlan and known groups
`apiConnectPlan` with `linkOwnerSig` returns:
- `GLPOk {groupSLinkData_, ownerVerification}` — new group
- `GLPKnown {groupInfo}` — bot already a member
- `GLPOwnLink` / `GLPConnectingProhibit` / `GLPConnectingConfirmReconnect` / `GLPNoRelays`

**Gap**: For `GLPKnown`, `groupShortLinkPlan` short-circuits via `knownLinkPlans` — never resolves link data, never verifies signature.

**Fix**: Add an optional parameter to `APIConnectPlan` (before `sig=`, since JSON must be last) that forces link data re-resolution even for known groups. With this parameter, `GLPKnown` includes `ownerVerification` and freshly loaded `groupSLinkData`. The loaded profile may differ from stored — the bot treats the server's current data as authoritative and updates its stored profile accordingly.

**Future**: Add a signed version counter to link data to detect rollback attacks (malicious server serving old signed profiles). The bot would store the highest version seen and reject/flag version reductions. For now, the server is treated as authoritative.

### Owner-contact association via APIConnectPreparedGroup
`createLinkOwnerMember` (called during `APIConnectPreparedGroup`) currently creates owner members with `memberContactId = Nothing`. Add an optional `(contactId, ownerId)` paired parameter to `APIConnectPreparedGroup`: when the link was received in a DM, pass the sender's `contactId` and the `ownerId` from `LinkOwnerSig`. The core sets `memberContactId` on the specific owner member whose `memberId` matches `ownerId`.

This makes ALL existing directory event routing work: `DEContactRoleChanged`, `DEContactRemovedFromGroup`, `DEContactLeftGroup` all resolve via `memberContactId` — no new event types needed for owner tracking.

Also benefits regular UI: when a user taps an owner's link in a DM, the contact association is created, improving the experience (e.g., showing the contact in the group member list).

## Registration flow for public groups

1. Owner taps "Share via chat" on their public group → sends MCChat card to bot in DM
2. Bot receives `CEvtNewChatItems` with `MCChat` content in direct chat → `DEChatLinkReceived`
3. Bot validates card (see validation matrix)
4. Bot calls `apiConnectPlan` with `connLink`, `linkOwnerSig`, and force-resolve flag
5. On `GLPOk` + `Verified`: bot replies "Joining {channel/group} {name}..." and joins via `APIPrepareGroup` then `APIConnectPreparedGroup` (passing owner's `contactId` and `ownerId`). On error: replies "Error joining {channel/group} {name}, please re-send the link!" (same pattern as existing group flow, Service.hs:368-370).
6. After `APIConnectPreparedGroup`, bot stores `dbOwnerMemberId` (via `getGroupMemberIdViaMemberId` — `createLinkOwnerMember` created the record during connect). Registration status: `GRSProposed`.
7. When `CEvtUnknownMemberAnnounced` fires for the owner member → `DEOwnerMemberAnnounced` → bot transitions to `GRSPendingApproval`, replies "Joined {channel/group} {name}. Registration is pending approval — it may take up to 48 hours.", sends to admins for approval
8. Admin approves → `GRSActive`

## Scenario matrix: card received in DM

### Event

One event: `DEChatLinkReceived { contact :: Contact, chatItemId :: ChatItemId, chatLink :: MsgChatLink, ownerSig :: Maybe LinkOwnerSig }`.

Handler validates and replies based on content.

### Card validation (handler level)

| Condition | Action |
|---|---|
| `chatLink` is not MCLGroup, or MCLGroup but no `publicGroup` in profile | Reply: "Only channels can be added to directory via link." |
| MCLGroup + publicGroup but `ownerSig` is `Nothing` | Reply: "To add a {channel/group} to directory you must be the owner." |
| MCLGroup + publicGroup + `ownerSig` is `Just` | Proceed to connectPlan |

### connectPlan results

| Plan result | ownerVerification | Action |
|---|---|---|
| `GLPOk` + sLinkData | `Verified` | Reply "Joining {channel/group} {name}...", join (with contactId + ownerId), register as `GRSProposed` |
| `GLPOk` + sLinkData | `Failed reason` | Reply: "Link signature verification failed: {reason}.\nYou must be the {channel/group} owner to register it." |
| `GLPOk` + sLinkData | `Nothing` | Reply: "Error: could not verify {channel/group} ownership. Please report it to directory admins." |
| `GLPOk` no sLinkData | — | Reply: "Error: no {channel/group} information available via the link." |
| `GLPKnown` | `Verified` | Bot already member — handle as re-registration (see below) |
| `GLPKnown` | `Failed reason` | Reply: "Link signature verification failed: {reason}.\nYou must be the {channel/group} owner to register it." |
| `GLPKnown` | `Nothing` | Reply: "Error: could not verify ownership." |
| `GLPConnectingProhibit` | — | Reply: "Already connecting to this {channel/group}." |
| `GLPConnectingConfirmReconnect` | — | Reply: "Already connecting to this {channel/group}." |
| `GLPOwnLink` | — | Log error. Reply: "Unexpected error. Please report it to directory admins." |
| `GLPNoRelays` | — | Reply: "{Channel/Group} has no active relays. Please try again later." |

### Owner member activation after joining

Bot is in `GRSProposed`. The pre-created owner member has `GSMemUnknown` status. When the relay announces this member, `CEvtUnknownMemberAnnounced` fires → mapped to `DEOwnerMemberAnnounced` in directory events.

| Condition | Action |
|---|---|
| `CEvtUnknownMemberAnnounced` for member matching `dbOwnerMemberId`, announced role is `GROwner` | Transition to `GRSPendingApproval`, notify submitting contact, send for admin approval |
| `CEvtUnknownMemberAnnounced` for member matching `dbOwnerMemberId`, announced role < `GROwner` | Reply: "The signing key does not belong to a current owner. Registration cancelled." Set `GRSRemoved`. |
| Owner member never announced | Registration stays in `GRSProposed`. No timeout — manual cleanup via admin. |

### Re-registration (GLPKnown — bot already member, signature verified at plan)

With the `connectPlan` fix, `GLPKnown` now includes `ownerVerification` and fresh `groupSLinkData`. Only proceed if `Verified`.

Bot extracts `ownerId`, looks up member via `getGroupMemberIdViaMemberId`, confirms `memberRole >= GROwner` AND `memberStatus` is active (not `GSMemUnknown`). The pre-created member has `GROwner` role from creation, so role alone is insufficient — the member must have been announced by a relay to confirm actual presence in the group.

Look up existing `GroupReg` by `groupId`:

| Existing registration | Ownership verified | Action |
|---|---|---|
| No GroupReg found | Yes | Create new registration as `GRSPendingApproval` |
| GroupReg exists, same owner contact | Yes | Handle based on current status (see status matrix) |
| GroupReg exists, different contact | Sender is verified owner AND previous registrant no longer owner (check `dbOwnerMemberId` member's current role) | Transfer: update `dbContactId` and `dbOwnerMemberId`, proceed as same-owner case |
| GroupReg exists, different contact | Sender is verified owner BUT previous registrant still owner | Reply: "This {channel/group} is registered by another owner." |
| GroupReg exists, different contact | Sender NOT verified owner | Reply: "You must be the {channel/group} owner to register it." Additionally: check if previous registrant (via `dbOwnerMemberId`) is still owner. If not → suspend (`GRSSuspendedBadRoles`). |

### Re-registration by same owner — status matrix

| Current status | Action |
|---|---|
| `GRSProposed` | Only if owner member is active (not `GSMemUnknown`): transition to `GRSPendingApproval`, send for approval. If still `GSMemUnknown`: reply "Waiting for owner to connect to the {channel/group}." |
| `GRSPendingConfirmation` | Transition to `GRSPendingApproval`, send for approval (only if previously registered via admin-invitation flow) |
| `GRSPendingUpdate` | Transition to `GRSPendingApproval`, send for approval (only if previously registered via admin-invitation flow) |
| `GRSPendingApproval n` | Check if profile changed (fresh profile from connectPlan vs bot's current DB). If yes: increment approval ID, re-send. If no: reply "Already pending approval." |
| `GRSActive` | Check if profile changed. If yes: transition to `GRSPendingApproval`, re-send. If no: reply "Already listed in the directory." |
| `GRSSuspended` | Reply: "{Channel/Group} is suspended by admin. Contact support." |
| `GRSSuspendedBadRoles` | Ownership re-verified at plan. Transition to `GRSPendingApproval`, send for approval. |
| `GRSRemoved` | Re-register as `GRSPendingApproval` |

### Profile change detection

For re-registration: compare the freshly loaded profile (from connectPlan's re-resolved `groupSLinkData`) against the group's current profile in the bot's database.

For XGrpInfo updates: re-resolve the link via `apiConnectPlan` with `resolve=on`, compare freshly loaded link profile against bot's stored profile.

Uses the same `sameProfile` comparison as existing group flow (Service.hs:491-494), extended with `publicGroup` field: `displayName`, `fullName`, `shortDescr`, `image`, `description`, `memberAdmission`, `publicGroup` — any difference triggers re-approval. The `publicGroup` field includes `groupLink` (ShortLinkContact), so link regeneration by the owner also triggers re-approval.

## Profile updates via XGrpInfo (bot is subscriber)

Bot receives `DEGroupUpdated` when any member updates the group profile. Works for subscribers.

For public groups: skip "link in welcome message" check. First check if the profile actually changed using the same `sameProfile` comparison as for regular groups (`displayName`, `fullName`, `shortDescr`, `image`, `description`, `memberAdmission`). Only if changed, call `apiConnectPlan` with `resolve=on` to re-resolve the link data. Compare the resolved link profile against the bot's stored profile.

Note: `xGrpInfo` (Subscriber.hs:3172) prevents `publicGroup` removal and `publicGroupId` changes for channels — these cases can never occur. The `groupLink` (ShortLinkContact) CAN change if the owner regenerates the link; the bot's DB is updated via XGrpInfo and subsequent re-resolution uses the current link.

| Current status | Profile changed (link data vs stored) | Action |
|---|---|---|
| `GRSProposed` | Any | No action (waiting for owner activation) |
| `GRSPendingApproval n` | Yes | Increment approval ID, re-send for approval |
| `GRSPendingApproval n` | No | No action |
| `GRSActive` | Yes | Transition to `GRSPendingApproval`, notify owner, re-send |
| `GRSActive` | No | No action |
| `GRSSuspended` | Any | No action |
| `GRSSuspendedBadRoles` | Any | No action |
| `GRSRemoved` | Any | No action |

## Owner tracking

### Owner-contact association

When the bot connects via `APIConnectPreparedGroup` with the submitting contact's `contactId` and `ownerId`, the core sets `memberContactId` on the specific pre-created owner member whose `memberId` matches `ownerId`. This makes all existing event routing work: `DEContactRoleChanged`, `DEContactRemovedFromGroup`, `DEContactLeftGroup` resolve via `memberContactId`.

### Owner changes

| Event | Detection | Action |
|---|---|---|
| Owner loses owner role | `DEContactRoleChanged` (works via `memberContactId` set at connect time) | Transition to `GRSSuspendedBadRoles`, notify |
| Owner leaves group | `DEContactLeftGroup` | Transition to `GRSRemoved`, notify, leave group |
| Owner removed from group | `DEContactRemovedFromGroup` | Transition to `GRSRemoved`, notify, leave group |
| Non-owner sends card, current registrant no longer owner | Re-registration flow detects stale ownership | Suspend (`GRSSuspendedBadRoles`). Non-owner's card also checked: if their `ownerId` resolves to a non-owner member, and the current registrant is also not owner → suspend. |
| New owner sends card, current registrant no longer owner | Re-registration flow, verified | Transfer registration |

## Commands for public group registrations

Bot is subscriber (not admin):
- `/filter` — Reply: "This command is not available for public groups."
- `/role` — Reply: "This command is not available for public groups."
- `/link` — Show `PublicGroupProfile.groupLink` with appropriate message.
- `/delete` — Remove registration, bot leaves group (`APILeaveGroup`).
- `/list` — Works as before, includes public group registrations.

## De-registration

| Event | Action |
|---|---|
| Owner sends `/delete ID:NAME` | Delete registration, reply confirmation, leave group |
| Bot removed (`DEServiceRemovedFromGroup`) | Set `GRSRemoved`, notify |
| Group deleted (`DEGroupDeleted`) | Set `GRSRemoved`, notify |
| Owner leaves (`DEContactLeftGroup`) | Set `GRSRemoved`, notify, leave group |
| Owner removed (`DEContactRemovedFromGroup`) | Set `GRSRemoved`, notify, leave group |
| Admin sends `/suspend ID:NAME` | Set `GRSSuspended`, notify, do NOT leave group |

Bot leaves group only for public group registrations (regular groups preserve existing behavior).

## Code changes

### 1. GroupType — add GTGroup

`Types.hs`:
```haskell
data GroupType = GTChannel | GTGroup | GTUnknown Text
```

### 2. connectPlan — force-resolve parameter

Add optional parameter to `APIConnectPlan` (before `sig=`): `resolve=on`. When present, `groupShortLinkPlan` skips the `knownLinkPlans` shortcut and always resolves link data. `GLPKnown` extended with `ownerVerification` and `groupSLinkData_`:
```haskell
GLPKnown {groupInfo :: GroupInfo, ownerVerification :: Maybe OwnerVerification, groupSLinkData_ :: Maybe GroupShortLinkData}
```

Parser: `/_connect plan <userId> [resolve=on] <link> [sig=<json>]`

### 3. APIConnectPreparedGroup — optional (contactId, ownerId)

Add optional paired `(contactId, ownerId)` parameter to `APIConnectPreparedGroup`. When present, `createLinkOwnerMember` (called during connect, Commands.hs:2129) sets `memberContactId` on the specific owner member whose `memberId` matches the provided `ownerId`.

Current parser (Commands.hs:5045): `/_connect group #<groupId> [incognito=on] [<msgContent>]`
New parser: `/_connect group #<groupId> [contact=<contactId> owner=<ownerId>] [incognito=on] [<msgContent>]`

`contact` and `owner` are paired — both required together. `ownerId` identifies which pre-created owner member gets the `memberContactId` set (multiple owners possible via OwnerAuth chain).

Current type (Controller.hs:479): `APIConnectPreparedGroup GroupId IncognitoEnabled (Maybe MsgContent)`
New type: `APIConnectPreparedGroup GroupId (Maybe (ContactId, B64UrlByteString)) IncognitoEnabled (Maybe MsgContent)`

This also benefits the UI: when tapping an owner's link in a DM, the contactId is threaded through the connect alert to `APIConnectPreparedGroup`, creating the association.

### 4. Events.hs — new events

`DEChatLinkReceived` — fires for ALL MCChat messages in DM (any `MsgChatLink` variant, signed or unsigned):
```haskell
| DEChatLinkReceived
    { contact :: Contact,
      chatItemId :: ChatItemId,
      chatLink :: MsgChatLink,
      ownerSig :: Maybe LinkOwnerSig
    }
```

`DEOwnerMemberAnnounced` (from `CEvtUnknownMemberAnnounced`):
```haskell
| DEOwnerMemberAnnounced GroupInfo GroupMember GroupMember
    -- ^ groupInfo, unknownMember, announcedMember
```

In `crDirectoryEvent_`, extend `CEvtNewChatItems` for direct chat:
```haskell
(MCChat {chatLink, ownerSig}, Nothing) -> DEChatLinkReceived ct ciId chatLink ownerSig
```

Add `CEvtUnknownMemberAnnounced` handler:
```haskell
CEvtUnknownMemberAnnounced {groupInfo, unknownMember, announcedMember} ->
  Just $ DEOwnerMemberAnnounced groupInfo unknownMember announcedMember
```

### 5. Service.hs — public group link handler

`deChatLinkReceived`: validates card, calls `apiConnectPlan` (with `resolve=on`), handles per scenario matrix. The link string comes from `MCLGroup.connLink` (`ShortLinkContact`) formatted as URI — passed via command string, parsed inside the handler. For `GLPOk` + `Verified`: joins (with contactId + ownerId), stores `dbOwnerMemberId`, registers as `GRSProposed`. On join error: replies to owner (same pattern as Service.hs:368-370). For `GLPKnown` + `Verified`: re-registration flow.

### 6. Service.hs — owner member announced handler

`deOwnerMemberAnnounced`: checks if the announced member's `GroupMemberId` matches `dbOwnerMemberId` of any `GRSProposed` registration. If yes and role is `GROwner`: transition to `GRSPendingApproval`, notify, send for approval. If role < `GROwner`: cancel.

### 7. Service.hs — deGroupUpdated changes

For public groups (`groupProfile.publicGroup` present), skip "link in welcome message" check. On profile change, call `apiConnectPlan` with `resolve=on` to get authoritative link data. Compare resolved profile against stored. If different, trigger re-approval.

### 8. Service.hs — command restrictions and de-registration

Check `groupProfile.publicGroup` for `/filter`, `/role`. On `/delete` for public groups, call `APILeaveGroup`. Same for owner departure/removal events.

### 9. Help message update

```
To register a channel, share its link with this bot using the "Share via chat" button.
To register a group, invite this bot as admin.
```

### 10. Approval message for admins

Include: group name, description, image, member count, "Registered via link sharing (signed by owner)", publicGroupId.

### 11. Tests

**Registration:**
- Share signed card → bot joins, owner announced, pending approval
- Share unsigned card → "must be owner" reply
- Share non-MCLGroup / non-public-group card → "only channels" reply
- Share card with invalid signature → rejection with reason
- Share card, owner never announced → stays GRSProposed
- Share card, owner announced but role < GROwner → cancelled

**Re-registration (GLPKnown, verified):**
- Same owner re-shares, active → "already listed"
- Same owner re-shares, pending → "already pending"
- Same owner re-shares with changed profile → re-approval
- Different contact, verified owner, previous no longer owner → transfer
- Different contact, verified owner, previous still owner → "registered by another owner"
- Different contact, not owner → rejection + stale ownership check
- Same owner re-shares while GRSProposed, owner still GSMemUnknown → "waiting for owner"

**Profile updates:**
- XGrpInfo on active public group → re-approval
- XGrpInfo on pending public group → increment approval ID
- XGrpInfo on public group skips link-in-welcome check

**Owner tracking (via contactId association):**
- Owner role changed → suspension
- Owner leaves → removal, bot leaves
- Owner removed → removal, bot leaves

**De-registration:**
- `/delete` by owner → removal, bot leaves
- Bot removed → removal
- Admin `/suspend` → suspension, bot stays

**Commands:**
- `/filter` on public group → disabled
- `/role` on public group → disabled
- `/link` on public group → shows public link
