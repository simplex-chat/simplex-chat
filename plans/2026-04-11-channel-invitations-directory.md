# Public Group Invitations & Directory Listing

## Goal

Enable public group (channel) subscribers to invite new subscribers by sharing a channel card in any chat where they can send messages. Channel owners can prove ownership via a signed card. This unblocks directory service support for public groups alongside regular groups.

Sharing channels should be as simple as forwarding — share button on channel opens chat picker, sends a channel card as a regular message. Old clients show the text; new clients show a rich card with profile and join button.

## Context

### Current state
- Public groups have `PublicGroupProfile {groupType = GTChannel, groupLink, publicGroupId}` and `useRelays = True`
- Users join public groups via link → `APIPrepareGroup` → `APIConnectPreparedGroup`
- `MCChat` message content exists with `MsgChatLink` variants for contacts, invitations, and groups (`MCLGroup`)
- Group invitations (`XGrpInv`) carry `connRequest :: ConnReqInvitation` — public groups don't use this mechanism
- Directory bot registers groups via group invitation (owner invites bot as admin) — public groups need a different flow

### Owner keys in public group links
- `FixedLinkData.rootKey :: PublicKeyEd25519` — genesis root key
- `UserContactData.owners :: [OwnerAuth]` — chain of authorized owner keys, each signed by root or previous owner
- Public group creator stores `GroupKeys {groupRootKey = GRKPrivate rootPrivKey, memberPrivKey}`
- `memberPrivKey`'s public key = `ownerKey` in the `OwnerAuth` entry (created via `newOwnerAuth`)
- `publicGroupId = sha256(rootPubKey)` — immutable group identity

### DR connection shared secret
- Each direct connection has `rcAD` (Associated Data) from X3DH key exchange
- `getConnectionRatchetAdHash` returns `sha256(rcAD)` — binding for replay protection

## Design

### Channel cards as MCChat messages

Channel invitations are sent as regular `XMsgNew` with `MCChat` content. No new protocol messages.

```haskell
data MsgContent
  = ...
  | MCChat {text :: Text, chatLink :: MsgChatLink, ownerSig :: Maybe ChatOwnerSig}
  | ...
```

`ownerSig` is optional. Old clients ignore it (missing field) and show `text` as a regular message.

```haskell
data ChatOwnerSig = ChatOwnerSig
  { memberId :: MemberId,
    binding :: B64UrlByteString,
    ownerSig :: B64UrlByteString
  }
```

Works for all `MsgChatLink` types:
- `MCLGroup`: verified against owner keys in the group link
- `MCLContact`: verified against root key in the contact link
- `MCLInvitation`: verified against root key in the invitation link

### What is signed

`smpEncode chatBinding <> bindingData <> smpEncode chatLink` signed with `memberPrivKey`.

Binding depends on where the card is sent:
- **Direct chat**: `CBDirect` with `ratchetAdHash`
- **Public group**: `CBGroup` with `smpEncode (publicGroupId, memberId)`
- **Group without public identity**: signature treated as failed at verification time

### Sending flow

1. User presses "Share" on channel → API call `APIPrepareChatOwnerSig GroupId` returns `Maybe ChatOwnerSig`
2. Opens chat picker (same as forwarding) — chats with disabled simplex links greyed out
3. Sends `XMsgNew` with `MCChat {text = displayName, chatLink = MCLGroup {connLink, groupProfile}, ownerSig}`
4. Creates regular `CISndMsgContent` chat item — no new item types, no new response types

### Receiving flow

Regular `XMsgNew` processing. Creates `CIRcvMsgContent (MCChat ...)`. No hidden groups, no async verification, no special events.

UI renders channel card with profile, member count, join button. If `ownerSig` present, shows "signed by owner" indicator (unverified until join).

### Verification at join time

When user taps "Join" on a channel card:

1. `connectPlan` resolves short link from `MCLGroup.connLink`
2. If `ownerSig` present:
   - Gets `FixedLinkData` and `UserContactData {owners}` from resolved link
   - Determines `chatBinding` from context where card was received
   - Verifies binding data matches expected value
   - Finds `OwnerAuth` entry matching `memberId`
   - Verifies signature
3. Plan result includes verification status: verified / failed / not signed
4. User sees result in plan alert before confirming
5. `APIPrepareGroup` → `APIConnectPreparedGroup` — existing join flow, no changes

### Forwarding

When `MCChat` is forwarded, `ownerSig` is dropped — signature is bound to original context and won't verify in forwarded context.

### Simplex link permission

On send: check `groupFeatureUserAllowed SGFSimplexLinks` for destination group.
On receive: check `groupFeatureMemberAllowed SGFSimplexLinks` for sender in group context.

### CLI view

`MCChat` with `MCLGroup` renders as channel card with display name. If `ownerSig` present, shows "(signed)" indicator.

## Directory bot changes

### Registration flow

Bot receives regular `CIRcvMsgContent (MCChat ...)` messages in direct chat from channel owners. Bot checks `ownerSig` is present. Verifies at join time via `connectPlan`. No special events needed.

- Owner sends channel card to bot in DM (signed)
- Bot resolves link, verifies owner signature
- Bot joins channel as subscriber
- Simplified approval flow: `GRSProposed` → `GRSPendingApproval` → `GRSActive`

### Profile monitoring

Bot as subscriber receives `XGrpInfo` when owner updates profile. On profile change: re-resolve link, compare. Periodic re-verification.

### Search and listing

Search includes both groups and public groups. No separate listing category — `groupProfile.publicGroup` is the source of truth. `DETGroup` works for both in JSON listing.

## Implementation plan (diff from master)

### Step 1: ChatOwnerSig type

- `ChatOwnerSig` type in Types.hs (or Protocol.hs alongside `MCChat`)
- `ownerSig :: Maybe ChatOwnerSig` field on `MCChat`
- JSON derivation with backward compat (optional field)

### Step 2: CBDirect

- Add `CBDirect` to `ChatBinding` in Protocol.hs (already done on master via refactoring PR)

### Step 3: Signature preparation API

- `APIPrepareChatOwnerSig GroupId` command in Controller.hs
- Implementation: load `GroupKeys`, compute `ratchetAdHash` for the target chat, sign, return `ChatOwnerSig`
- CLI: `/_prepare_sig #<groupId>` or integrate with share flow

### Step 4: connectPlan verification

- Extend `connectPlan` / `groupJoinRequestPlan` to verify `ownerSig` from the originating `MCChat` message
- Plan result includes owner verification status
- Verification logic: resolve link → check binding → find owner key → verify signature

### Step 5: Forwarding — drop ownerSig

- When forwarding `MCChat`, set `ownerSig = Nothing`

### Step 6: Permission check

- On send in group context: check `groupFeatureUserAllowed SGFSimplexLinks`
- On receive in group context: check `groupFeatureMemberAllowed SGFSimplexLinks`

### Step 7: CLI view

- Render `MCChat` with `MCLGroup` as channel card in `viewChatItem`

### Step 8: groupLinkData owners preservation

- Fix `groupLinkData` (Internal.hs) to reconstruct `OwnerAuth` from `GroupKeys` instead of hardcoding `owners = []`

### Step 9: Tests

- Share channel card in direct chat (owner signed)
- Share channel card in group (unsigned — no binding for groups without identity)
- Share channel card in channel
- Join via channel card — verify `connectPlan` shows verification result
- Non-public group share rejected
- Forwarded card has no signature
- Old client compatibility (text field shown)

### Step 10: Directory bot

- Handle `MCChat` with `MCLGroup` in `crDirectoryEvent_`
- Channel registration flow
- Profile monitoring

## What stays from refactoring PR (already on master)

- `CBDirect` in `ChatBinding`
- `HasShortLink` typeclass with `connShortLink'`
- `setShortLinkType` / `setShortLinkType_`
