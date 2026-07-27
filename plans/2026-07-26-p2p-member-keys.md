# p2p group member keys - generation and distribution

## Goal

Give every member of a p2p (non-relay) group an Ed25519 signing key, and distribute each member's public key to the other members, so p2p group messages can be signed and verified. New members are keyed at join; existing members are keyed on upgrade and their keys are distributed through the existing profile-update path.

## Design (agreed)

- Own key: private in `groups.member_priv_key` (via `GroupKeys.memberPrivKey`), public in the membership's `group_members.member_pub_key`. `groupKeys = Just (GroupKeys {publicGroupKeys = Nothing, memberPrivKey})` marks a p2p member key.
- Distribution: the public key is included in `XInfo` (and in `XContact` at join). `XInfo` is sent by the existing profile-update send (`sendGroupProfileUpdate`); the key is included whenever `XInfo` is sent, and a per-member flag records delivery to version-compatible members. One `XInfo` per send - if the profile is sent because it changed, the key is included in that message rather than a second one.
- Version: a new chat version decides who is marked and who can read the key. A member between version 7 and the new version receives `XInfo` for the profile and ignores the unknown key field.
- No acknowledgement in groups: the flag is set on send. A lost message means the member cannot verify until the next send re-delivers the key; whether an unverifiable claim is hidden or shown is a per-claim decision.

## Current state (last commit `261d09ba4`)

Field plumbing is done: `memberKey :: Maybe MemberKey` added to `XInfo` and `XContact`, full encode/decode, all call sites pass `Nothing`/`_`. Four `TODO [member keys]` markers remain at the fill-in points: `Commands.hs:3911` (XContact join), `Internal.hs:2489` (profile-update send `sendGroupProfileUpdate`), `Subscriber.hs:836` (join-confirmation allow), `xInfoMember` (receive/store).

## Changes

### 1. Version - `Protocol.hs`

- Add `groupMemberKeyVersion :: VersionChat = VersionChat 20` with a comment.
- `currentChatVersion = VersionChat 20` (from 19).
- Add changelog line `-- 20 - p2p group member keys for signing (2026-07-26)`.
- No new binary-floor constant: reuse `relayWebCapVersion` (18) as the reliable binary-batch floor for partitioning signed sends (item 7). Binary parsing was added in #6597 at version 17 with no constant; 18 is the first guaranteed.

### 2. Schema + type + row parsing

- New migration `M20260726_member_key_sent.hs` (mirror `M20260720_server_roles.hs`): `ALTER TABLE group_members ADD COLUMN user_member_key_sent INTEGER NOT NULL DEFAULT 0`. Update `chat_schema.sql`.
- `GroupMember` (`Types.hs:1119`): add `userMemberKeySent :: Bool` after `memberPubKey`.
- `GroupMemberRow` / `MaybeGroupMemberRow` (`Groups.hs:263`): add `BoolInt` / `Maybe BoolInt` to the last tuple group, next to `member_pub_key`.
- `toGroupMember` / `toMaybeGroupMember`: parse the column.
- Every `SELECT` that builds a `GroupMemberRow` adds `user_member_key_sent` (shared column list - several sites; grep the existing `member_pub_key, relay_link` list).

### 3. Own key generation + storage (key exists before signing)

- New store fn `setUserMemberKey :: DB.Connection -> GroupId -> GroupMemberId -> C.PrivateKeyEd25519 -> IO ()`: two writes, both required - the private key to `groups.member_priv_key` (the user's own signing key for this group) and its derived public key to the user's own membership row (`group_members.member_pub_key`), as `createNewGroup:429` does.
- New helper `ensureUserMemberKey :: User -> GroupInfo -> CM GroupInfo`: if `groupKeys` already has a key, return `gInfo` unchanged; for a p2p group with `groupKeys = Nothing`, generate an Ed25519 key, store it via `setUserMemberKey`, and return `gInfo` with `groupKeys = Just (GroupKeys {publicGroupKeys = Nothing, memberPrivKey})`. Idempotent (check-and-set in one transaction so concurrent sends cannot create two keys).
- Generation points:
  - Create group - `APINewGroup` (`Commands.hs:2642`): generate the key and pass `Just GroupKeys {publicGroupKeys = Nothing, memberPrivKey}` to `newGroup` (was `Nothing`). `createNewGroup` (`Groups.hs:393-430`) already stores both columns.
  - Join - in `joinContact`'s p2p-group branch (item 5): `gInfo' <- ensureUserMemberKey user gInfo`, take the public key from `gInfo'` `groupKeys` for `XContact`.
  - Send - call `ensureUserMemberKey` at the top of the send entry (`sendGroupMessages` `:2458`, `sendGroupSignedMessages` `:2464`) and thread the returned `gInfo'` to BOTH `sendGroupProfileUpdate` and `sendGroupMessages_`, so `groupMsgSigning` signs the very first message after generation - the key must not lag one message behind. This is also the lazy path for groups created before this change.

### 4. `sendGroupProfileUpdate` - one `XInfo` with profile and/or key (`Internal.hs:2468`)

Key and signing are independent of incognito status: the member key is per group and needed in every p2p group. Only the badge may depend on incognito, and that is handled by the existing profile/badge logic, not the key path. `shouldSendProfileUpdate` gates only the profile part; the key part runs regardless. Restructure `sendGroupProfileUpdate` so a single `XInfo` serves both purposes (never two messages), for non-relay groups, using `gInfo'` from `ensureUserMemberKey`:

- `profileMembers = if shouldSendProfileUpdate then filter (\`supportsVersion\` memberProfileUpdateVersion) members else []` (unchanged trigger; still skips incognito, scope, asGroup).
- `keyMembers` = members with `supportsVersion groupMemberKeyVersion` and `not (userMemberKeySent m)` - runs regardless of incognito.
- recipients = union of the two.
- Send one `XInfo profile (Just ownKey)` to recipients via `sendGroupMessages_` (`:2500`), which returns the `GroupSndResult` needed for marking - `sendGroupMessage'` (`:2374`) discards it (the `_` at `:2377`), so the current `sendGroupProfileUpdate` send call must change. `profile` and its badge are the existing profile logic (unchanged); `ownKey = MemberKey (C.publicKey memberPrivKey)` from `gInfo'` `groupKeys`.
- After send, from that `GroupSndResult`: set `userMemberKeySent = True` for a key recipient whose `sentTo` delivery result (the third tuple element, `:2495`) is `Right`, or that is in `pending` or `forwarded` (enqueued, stored for delivery on connect, or forwarded). A `sentTo` `Left` (enqueue failure) stays `False`. `updateUserMemberProfileSentAt` only when `shouldSendProfileUpdate`.
- Retry stays but is uncapped: any `False` member is re-included on the next send. No cap is needed because `memberSendAction` (`Internal.hs:2608`) returns `Nothing` for a disabled/deleted/failed/rejected connection, so `addMember` (`:2542`) skips it - re-inclusion re-filters it in memory, it is never actually re-sent. The only un-marked-but-attempted case is a `sentTo` failure on a *ready* connection (a rare enqueue error), which retries next send and fails identically for the content message; a truly broken connection transitions to disabled/failed and is then skipped. So retry is cheap and self-limiting. `user_member_key_sent` is a plain boolean.
- New store fn `setMembersMemberKeySent :: DB.Connection -> [GroupMemberId] -> IO ()`.
- Relay groups keep current behaviour (no key here; key comes from the roster).

The member list is already in memory and `userMemberKeySent` is a field on the record, so both filters are in-memory with no extra query.

### 5. Fill the TODO send points

- `joinContact` (`Commands.hs:3900`): the key belongs only in the `Just (Just gInfo) | not (useRelays' gInfo)` case (p2p group join). Split that out of the current `_` branch: `gInfo' <- ensureUserMemberKey user gInfo`, then `XContact profileToSend (Just ownKey) (Just xContactId) welcomeSharedMsgId msg_`. The `Just Nothing` (unknown group) and `Nothing` (direct contact) cases keep `XContact ... Nothing ...`. `XContact` is `encodeConnInfoPQ` (JSON), so this delivery is **unsigned** - the initial trust-on-first-use key. The membership row exists in `gInfo` here, so `setUserMemberKey` writes `member_pub_key` (#5 confirmed).
- `Subscriber.hs:836` (joiner's allow-reply to the host): `XInfo profileToSend (Just ownKey)`, **signed** with the joiner's key when the host version allows (item 6). `XInfo` is `requiresSignature`, so it is signed like any other `XInfo`; this gives the host a signed confirmation of the joiner's key at join.
- `Subscriber.hs:1626` (host accepting the join): pass the parsed `XContact.memberKey` to `acceptGroupJoinRequestAsync` instead of `Nothing`; it flows to `createJoiningMember` (`Groups.hs:2070`, `:2112`), which stores `member_pub_key` (unsigned TOFU).
- Host key to the joiner: `XGrpLinkMem` (`Protocol.hs:503`, currently `Profile` only) needs a `Maybe MemberKey` field added, like the commit added to `XInfo`/`XContact`. The host already sends it during the join in `sendXGrpLinkMem` (`Subscriber.hs:974`), fired on the joiner's `CON` (`:958`); include the host's key, and store it in `xGrpLinkMem` (`:2760`). This is the host->joiner counterpart of the joiner's `XContact`. Add `XGrpLinkMem` to `requiresSignature` (safe - p2p-only, relay groups never send/receive it). But `sendXGrpLinkMem` currently uses `sendDirectMemberMessage` -> `sendDirectMessage_` -> `createSndMessage` (`:2197`), which hardcodes `Nothing` signing and sends via `deliverMessage` (no `groupMsgSigning`, no mode partition) - so `requiresSignature` alone would not sign it. Switch `sendXGrpLinkMem` to `sendGroupMemberMessages` (`:2228`), which computes `groupMsgSigning` (`:2231`) and uses the mode at `:2232` (item-7 partition site: binary-signed to a v20+ joiner, unsigned JSON to a pre-20 joiner), and run `ensureUserMemberKey` first so the host has a key to sign with. `xGrpLinkMem` (`:2760`) must `verifyGroupSig` against the key delivered in the message (self-certifying, like `:868`), since `withVerifiedMsg` has no stored host key yet.

### 6. Receive + confirm the key

The key is confirmed cryptographically wherever the `XInfo` is signed. Two receive points:

- Handshake allow-reply - `Subscriber.hs:868` (`XInfo _ _`). The joiner's reply can be signed: `encodeSignedConnInfo` already produces a signed connInfo (used by `encodeXMemberConnInfo`), and the peer version is known by `INFO` (`updatePeerChatVRange`), so sign the allow-reply (item 5) with the joiner's key when the host version supports it. `parseChatMessage` here is `parseChatMessage'` with the signature discarded (`Internal.hs:1793`); switch to `parseChatMessage'`, verify the signature against the key in the `XInfo`, then read and confirm the key. This confirms the joiner's key at join.
- Group-message `XInfo` - `xInfoMember` (`Subscriber.hs:2755`), signed via item 7, for ongoing profile/key updates.

Store/confirm rule at both points, new store fn `setMemberPubKey :: DB.Connection -> GroupMemberId -> C.PublicKeyEd25519 -> IO ()` (the key-only, no-role counterpart of the existing `setGroupMemberKeyRole`):
- `memberPubKey m = Nothing`, `mKey = Just k` -> store `k`.
- `memberPubKey m = Just k0` -> accept only `Nothing` or `Just k0`; a different key is rejected (immutable).

This is the same pin-or-reject rule as the existing `applyMemberKeyRole` (`Subscriber.hs`, used by the roster): `Nothing` -> pin, `Just k` with `k /= pubKey` -> `Left` reject. Reuse or mirror it.

Signing is uniform: `XInfo` is `requiresSignature`, so every group `XInfo` (the 836 allow-reply, group profile updates) and `XGrpLinkMem` is signed with the member's key when the recipient version allows (binary). The one unconditionally-unsigned delivery is `XContact` - a JSON connInfo that cannot be signed; the host holds that key as trust-on-first-use, confirmed by the joiner's signed `XInfo` (the 836 reply, then later updates).

### 7. Signed send - partition recipients by binary capability

Once `groupKeys = Just`, `groupMsgSigning` (`Internal.hs:2214`) produces a `MsgSigning` for p2p messages, and `createNewSndMessage` (`Store/Messages.hs:236`) stores each `SndMessage` with both `msgBody` (plain encoded message) and `signedMsg_ :: Maybe SignedMsg` (signature over that body). A signed element cannot sit in a JSON batch: `encodeBatchElement (Just sm) body = "/" <> smpEncode (chatBinding, signatures) <> body` (binary), `encodeBatchElement Nothing body = body` (plain JSON), and `encodeBatch` wraps them as `=...` (binary) or `[...]` (JSON) (`Batch.hs:70`, `:130-134`). So the same `SndMessage` yields either form with no re-encoding: keep `signedMsg_` for the signed element, set it to `Nothing` for the unsigned one.

The send path currently picks one mode for the whole group: `mode = if useRelays' gInfo then BMBinary else BMJson` (`Internal.hs:2232`, `:2549`, `:2676`), so p2p is always `BMJson`. Change, for a p2p group when any message is signed (`any (isJust . signedMsg_) msgs`):

- Partition the recipients (`toSendSeparate` and `toSendBatched`) by `\`supportsVersion\` relayWebCapVersion` (18, the binary-batch floor).
- Binary-capable members -> `batchSndMessagesJSON BMBinary msgs` (signed `/` elements).
- Binary-incapable members -> `batchSndMessagesJSON BMJson (map (fmap dropSig) msgs)`, `dropSig m = m {signedMsg_ = Nothing}` (unsigned JSON).
- Fold each partition over its own batch (`foldMembers` already runs per list) and concatenate; body references (`VRRef`) are naturally per-partition.

Relay groups (`BMBinary` for all) and unsigned p2p sends (`BMJson` for all) are unchanged.

A binary-capable member below `groupMemberKeyVersion` (18-19) receives the signed form, stores it unverified (no key), and can forward it intact via `encodeFwdElement` (`Batch.hs:125`), which preserves `signedMsg_` - which is why the partition is by binary capability, not key possession. A member below 18 receives the unsigned JSON form; in a p2p group `signatureOptional` is true, so it accepts the unsigned message rather than rejecting it.

Three mode sites to update: `prepareMsgReqs` (`:2549`, main group send), `sendGroupMemberMessages` (`:2232`, member-to-member / introductions), and `:2676`.

## Implementation status (2026-07-27)

All seven items implemented:

1. Version - `groupMemberKeyVersion = 20`, `currentChatVersion = 20`, `XGrpLinkMem` in `requiresSignature`, `XGrpLinkMem`/`XInfo`/`XContact` carry `Maybe MemberKey`.
2. Schema/type/rows - `user_member_key_sent` column (both backends, migration `M20260727_member_key_sent`), `userMemberKeySent :: Bool` on `GroupMember`, all `GroupMemberRow`/`MaybeGroupMemberRow` SELECTs and parses.
3. Generation - `setUserMemberKey`, `ensureUserMemberKey`, `groupMemberKey`; keyed at `APINewGroup` (create), `joinContact` (contact-link join), `sendXGrpLinkMem` and `XGrpLinkInv` handshake (prepared-group join), and lazily at the send entries.
4. `sendGroupProfileUpdate` - one `XInfo profile (Just ownKey)` to `profileMembers ∪ keyMembers` via `sendGroupMessages_`, marks `setMembersMemberKeySent` for delivered v20+ recipients (`sentTo` Right / `pending` / `forwarded`), `updateUserMemberProfileSentAt` only when the profile changed.
5. Distribution - `XContact` (joiner->host, TOFU) threaded through `profileContactRequest` to `acceptGroupJoinRequestAsync`; `XGrpLinkMem` (host->joiner) signed via `sendGroupMemberMessages`; `XInfo` allow-reply (joiner->host) signed via `encodeSignedGroupConnInfo` + `allowAgentConnectionInfo`, gated on `not (useRelays' gInfo) && maxVersion chatVRange >= relayWebCapVersion`.
6. Receive - `storeMemberKey` (pin-or-reject, self-certified by `verifyGroupSig`) at `xInfoMember`, `xGrpLinkMem`, and the `INFO`/`XInfo` allow-reply (switched to `parseChatMessage'`).
7. Signed send - `prepareMsgReqs` partitions recipients by `relayWebCapVersion` (binary-signed to v18+, sig-stripped JSON below); `memberBatch` does the single-connection equivalent for `sendGroupMemberMessages` and `sendPendingGroupMessages`.

## Open decisions

None outstanding.

Resolved: names finalized during implementation. Both key writes required (`groups.member_priv_key` and own-row `member_pub_key`). `XContact` includes the unsigned key at member creation and the signed allow-reply confirms it. Reuse `relayWebCapVersion` (18) as the binary floor. `user_member_key_sent` is a boolean, set `True` for key recipients in `sentTo`/`pending`/`forwarded`; no cap or error classification - `memberSendAction` skips dead connections (`Internal.hs:2608`), so they are re-filtered, not re-sent, until ready. Key change on receipt - reject any change, immutable. Signed send (item 7) - partition by binary capability. Key distribution runs in all p2p groups including incognito. First send after generation is signed. Sign criteria unchanged. Handshake allow-reply signed, confirms the key at join.
