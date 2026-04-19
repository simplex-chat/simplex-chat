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
  | MCChat {text :: Text, chatLink :: MsgChatLink, ownerSig :: Maybe LinkOwnerSig}
  | ...
```

`ownerSig` is optional. Old clients ignore it (missing field) and show `text` as a regular message.

```haskell
data LinkOwnerSig = LinkOwnerSig
  { ownerId :: Maybe OwnerId,     -- Nothing = root key, Just = owner key from OwnerAuth chain
    binding :: B64UrlByteString,
    ownerSig :: B64UrlByteString
  }
```

Sending is supported for channel cards only (for now). Verification is generic for all `MsgChatLink` types:
- `ownerId = Just id`: verified against matching `OwnerAuth.ownerKey` in the link's owner chain (channels)
- `ownerId = Nothing`: verified against `rootKey` from `FixedLinkData` (contacts, invitations)

The sender proves control over the link regardless of type.

### What is signed

`smpEncode chatBinding <> bindingData <> smpEncode chatLink` signed with `memberPrivKey`.

Binding depends on where the card is sent:
- **Direct chat**: `CBDirect` with `ratchetAdHash`
- **Public group**: `CBGroup` with `smpEncode (publicGroupId, memberId)`
- **Group without public identity**: signature treated as failed at verification time

Binding is to chat, not to message (`sharedMsgId` is not included). This allows the sender to forward their own signed card within the same chat (e.g., re-sharing a channel link as a reminder) without invalidating the signature. Message-level binding would prevent this since forwarded messages get new `sharedMsgId`s.

### Sending flow

1. User presses "Share" on channel → API call `APIPrepareLinkOwnerSig GroupId` returns `Maybe LinkOwnerSig`
2. Opens chat picker (same as forwarding) — chats with disabled simplex links greyed out
3. Sends `XMsgNew` with `MCChat {text = displayName, chatLink = MCLGroup {connLink, groupProfile}, ownerSig}`
4. Creates regular `CISndMsgContent` chat item — no new item types, no new response types

### Receiving flow

Regular `XMsgNew` processing. Creates `CIRcvMsgContent (MCChat ...)`. No hidden groups, no async verification, no special events.

UI renders channel card with profile, member count, join button. If `ownerSig` present, shows "signed by owner" indicator (unverified until join).

### Verification at join time

When user taps "Join" on a channel card:

1. UI extracts `connLink` and `ownerSig` from `MCChat` message content
2. UI calls `APIConnectPlan` with the link and signature. `APIConnectPlan` extended:
   ```haskell
   APIConnectPlan {userId :: UserId, connectionLink :: Maybe AConnectionLink, linkOwnerSig :: Maybe LinkOwnerSig}
   ```
   Parser: `/_connect plan <userId> <link> [sig=<json>]`
3. Inside `connectPlan`, if `linkOwnerSig` is present:
   - Gets `FixedLinkData {rootKey}` and `UserContactData {owners}` from resolved link
   - Finds verification key: `ownerId = Nothing` → `rootKey`, `ownerId = Just id` → matching `OwnerAuth.ownerKey`
   - Verifies binding data against expected value from context
   - Verifies signature
4. Each "OK" plan variant extended with verification result:
   ```haskell
   data LinkSigVerification = LSVVerified | LSVFailed {reason :: Text}
   
   ILPOk {contactSLinkData_, linkSigVerification :: Maybe LinkSigVerification}
   CAPOk {contactSLinkData_, linkSigVerification :: Maybe LinkSigVerification}
   GLPOk {groupSLinkInfo_, groupSLinkData_, linkSigVerification :: Maybe LinkSigVerification}
   -- Nothing = not signed, Just LSVVerified = verified, Just LSVFailed = failed with reason
   ```
   Reasons: "unknown owner ID", "binding data mismatch", "signature verification failed", "no group identity for verification"
5. UI shows verification result in join/connect alert for the OK plan variants
6. User confirms → `APIPrepareGroup` → `APIConnectPreparedGroup` — existing join flow, no changes

Pasted links (no message context) pass `linkOwnerSig = Nothing` — plan shows "not signed."

### Forwarding

When `MCChat` is forwarded, `ownerSig` is dropped — UNLESS forwarded by sender in the same chat (re-sharing own card as reminder). Signature is bound to chat context, so forwarding in the same chat preserves validity.

Implementation: in forwarding code, drop `ownerSig` unless `fromChatRef == toChatRef` and sender is the same user.

### Simplex link permission

`MCChat` IS a simplex link — if `SGFSimplexLinks` is prohibited for the sender's role, `MCChat` should be prohibited regardless of content.

Currently `prohibitedSimplexLinks` (Internal.hs:363) only checks formatted text. Fix: also check `MsgContent` type — if it's `MCChat` and simplex links are not allowed, prohibit it. This covers both send and receive via existing `prohibitedGroupContent` calls.

For backward compatibility, the current text-level check is sufficient since the link is included in `text`. But the `MCChat` type check is the correct long-term fix.

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

### Step 1: LinkOwnerSig type

- `LinkOwnerSig` type in Types.hs (or Protocol.hs alongside `MCChat`)
- `ownerSig :: Maybe LinkOwnerSig` field on `MCChat`
- JSON derivation with backward compat (optional field)

### Step 2: CBDirect

- Add `CBDirect` to `ChatBinding` in Protocol.hs (already done on master via refactoring PR)

### Step 3: Share chat message content API

New command that constructs the complete `MCChat` content for sharing:
```haskell
-- Controller.hs
APIShareChatMsgContent {shareChatRef :: ChatRef, toChatRef :: ChatRef}
-- returns CRChatMsgContent {user :: User, msgContent :: MsgContent}
```

Implementation in Commands.hs:
1. Load shared chat info from `shareChatRef` — initially only `CTGroup` with public groups supported
2. Get `PublicGroupProfile {groupLink}` and `groupProfile` from group
3. Determine if user is owner (has `GroupKeys {memberPrivKey}`)
4. If owner, compute binding based on `toChatRef`:
   - `ChatRef CTDirect contactId` → `getConnectionRatchetAdHash` on contact's connection → `CBDirect`
   - `ChatRef CTGroup groupId` → `smpEncode (publicGroupId, memberId)` if group has identity → `CBGroup`
   - Group without identity → `Nothing` (can't sign)
5. If owner and binding available, sign `smpEncode chatBinding <> bindingData <> smpEncode chatLink` with `memberPrivKey`
6. Return `MCChat {text = displayName, chatLink = MCLGroup {connLink = groupLink, groupProfile}, ownerSig}`

Parser: `/_share_chat <chatRef> <chatRef>`

UI flow: press Share on channel → chat picker → select destination → call `APIShareChatMsgContent` → get `MsgContent` → send via existing `APISendMessages`

All business logic (ownership check, signing decision, link extraction, profile inclusion) stays in core. UI only passes two chat refs and sends the returned content.

### Step 4: connectPlan verification

Extend `APIConnectPlan` (Controller.hs:472):
```haskell
APIConnectPlan {userId :: UserId, connectionLink :: Maybe AConnectionLink, linkOwnerSig :: Maybe LinkOwnerSig}
```

Parser (Commands.hs:4945): extend to accept optional JSON `LinkOwnerSig` parameter.

In `connectPlan` (Commands.hs), pass `linkOwnerSig` to `groupShortLinkPlan` / `groupJoinRequestPlan`.

In `groupShortLinkPlan` (Commands.hs ~line 3944): after resolving the link via `getShortLinkConnReq`, if `linkOwnerSig` is present:
1. Extract `FixedLinkData {rootKey}` and `UserContactData {owners}`
2. If `ownerId = Nothing`: verify against `rootKey`
3. If `ownerId = Just id`: find `OwnerAuth` where `ownerId == id`, verify against `ownerKey`
4. Check binding data matches expected
5. Verify signature

Extend `GroupLinkPlan` (Controller.hs:1025):
```haskell
GLPOk {groupSLinkInfo_, groupSLinkData_, ownerVerified :: Maybe Bool}
```
`Nothing` = not signed, `Just True` = verified, `Just False` = failed.

`CRConnectionPlan` response carries this through to UI — shown in plan alert.

### Step 5: Forwarding — drop ownerSig

In message forwarding code (Commands.hs, `APIForwardChatItems`), when forwarding `MCChat` content, set `ownerSig = Nothing`.

Location: Commands.hs where forwarded message content is constructed — find where `MCChat` is handled in forwarding and strip the signature.

### Step 6: Permission check

Fix `prohibitedSimplexLinks` (Internal.hs:363) to also check `MsgContent` type — if `MCChat`, treat as simplex link. Covers both send and receive paths via existing `prohibitedGroupContent` calls.

For backward compatibility, the link is also in `text` field, so existing text-level check catches it. The type check is the correct fix.

### Step 7: CLI view

In `viewChatItem` (View.hs), `MCChat` content already renders via `ttyMsgContent`. Extend to show channel card format and "(signed)" indicator when `ownerSig` is present.

### Step 8: groupLinkData owners preservation

Fix `groupLinkData` (Internal.hs:1330) to reconstruct `OwnerAuth` from `GroupKeys` instead of hardcoding `owners = []`. This ensures the resolved link data has the owner keys needed for verification.

Implementation: when `GroupKeys` has `GRKPrivate rootPrivKey` and `memberPrivKey`, reconstruct `OwnerAuth` with `ownerId = unMemberId memberId`, `ownerKey = publicKey memberPrivKey`, `authOwnerSig = sign rootPrivKey (ownerId <> encodePubKey ownerKey)`.

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
