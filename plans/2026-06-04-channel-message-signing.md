# Plan: optional signing of channel content messages (`XMsgNew` / `XMsgUpdate`)

Grounded on `f/public-groups` (re-confirm anchors by symbol — they drift as the branch advances). Prerequisites are already merged here (#7048 roster-over-inline-file, #7089 roster transfers, #7058 p2p signature verification): `GroupKeys`/`publicGroupId`/`memberPrivKey` (`Types.hs:465`) and `verifyGroupSig` (`Subscriber.hs:117`) exist, and the roster distributes each member's public key.

This is **PR 1**: a member can optionally sign their own channel posts and edits so recipients holding the signed roster can verify authorship + integrity. Signed deletes with recipient enforcement are **PR 2** (sketched at the end).

## Goal / user problem

In relay channels, content (`XMsgNew`) is forwarded by relays and is not signed today — only group-state events are (`requiresSignature`, `Protocol.hs:1328`). A relay can therefore forge or alter content attributed to a member. This feature lets a member optionally attach their member signature; recipients with the roster verify it.

## Decisions

- UI: **per-send long-press override only** — no device-stored default preference. Signing is opt-in for each send.
- Default off, with an in-UI explanation of the tradeoff (a signature is transferable, non-repudiable proof of authorship).
- Recipient indicator in scope (iOS + Kotlin), **chat view only** — not the conversation list. Glyph: `checkmark.seal`.
- Scope this PR: `XMsgNew` + `XMsgUpdate`, **including as-channel posts** (see §5 — signing an as-channel post is verifiable and de-anonymizing, a deliberate team-accepted tradeoff). Edits reuse the original's setting. Reactions stay unsigned. Deletes → PR 2.

## Threat model

Actors: the sending member, recipients, and untrusted **chat relays** that forward content + roster.

- **Forgery of member content** — closed for signed messages: the relay lacks the Ed25519 key, and the signature binds `(publicGroupId, memberId, body)`, so no forgery, cross-bind, or alteration.
- **Downgrade / stripping** (residual, by design) — optional signing lets a relay strip a signature and deliver unsigned. Absence of a badge is *not* proof of forgery; only the presence of a verified badge is a guarantee. A future "required signing" group setting would close this (out of scope).
- **As-channel posts: anonymity for unsigned, accepted de-anonymization for signed.** An owner can "publish as the channel"; Design Objective 6 (`docs/protocol/channels-overview.md:214`) hides *which* owner authored a post from subscribers, and owners are "cryptographically indistinguishable to subscribers" (`:159`). This anonymity holds for **unsigned** as-channel posts: they forward via `FwdChannel` (no `memberId`), and a relay revealing the owner is only a deniable, detectable leak (`:237`). **Signing** an as-channel post is opt-in and deliberately gives it up: to be verifiable it forwards via `FwdMember` (§5), so every subscriber's device receives, verifies, and holds non-repudiable proof of the authoring owner. The owner who signs an as-channel post is trading anonymity + deniability (`:198`, `:221`, `:103`) for verifiability on that post; the UI must say so. Verifiable-*and*-anonymous (ring signature / channel-level key) is out of scope.
- **Non-repudiation** (tradeoff, by design) — a verified signature is transferable proof of authorship; hence opt-in/off.
- **What "verified" proves** — the signed input is `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, and `msgBody` embeds `sharedMsgId`, `MsgScope`, `asGroup`, content. It proves authorship + integrity + group/member/scope/message binding, and nothing else — not `fwdBrokerTs` (relay-controlled), ordering, or completeness. Surface in help.
- **Bad signature is fail-closed** — a signature that fails to verify drops the message and creates an `RGEMsgBadSignature` item (`Subscriber.hs:3798-3800`). New consequence for content: a signed message whose author key doesn't match the recipient's pinned roster key (key rotation, stale/lagging roster, TOFU mismatch) is **dropped**, not shown unsigned. State events already behave this way, but content is higher-volume and user-visible. Needs an edge-case test and a help note.
- **As-channel spoofing** — because signed as-channel posts now arrive as `FwdMember`, the recipient MUST verify the (verified) author is an owner before rendering as-channel (§5); otherwise a non-owner's signed `asGroup=True` post would display as "from the channel".
- **Replay** — the binding covers `sharedMsgId` + `MsgScope`; cross-scope/group replay is blocked, same-message replay is a dedup duplicate.

## What already exists (reused unchanged)

- **Send / sign**: `groupMsgSigning` (`Internal.hs:2099`) → `createSndMessages` threads `Maybe MsgSigning` → `createNewSndMessage` Ed25519-signs `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, storing `signedMsg_` in `SndMessage` (`Messages.hs:1156`).
- **Wire**: `batchMessages` prepends the signature via `encodeBatchElement` (`Batch.hs:45,69`); relay groups always batch.
- **Forward**: live delivery preserves the original signed bytes by reconstructing `VMSigned` from the stored `msg_chat_binding`/`msg_signatures` (`Store/Delivery.hs:155-165`); `fwdSender` is derived from the stored `showGroupAsSender` (`:158`).
- **Receive / verify**: `withVerifiedMsg` (`Subscriber.hs:3794`) runs for all group messages; `XMsgNew_`/`XMsgUpdate_` are not in `requiresSignature` ⇒ `signatureOptional` (`:3819`), so signed → `MSSVerified`/`MSSSignedNoKey`, unsigned → accepted. `FwdMember` verifies against the author's key (`:3746,3808`); `FwdChannel` is received as `VMUnsigned` (`:3758`). No protocol-version bump.
- **Persistence**: own item — `createNewSndChatItem` sets `MSSVerified <$ signedMsg_` (`Store/Messages.hs:548`); received item — `RcvMessage.msgSigned` (`Messages.hs:1174`) is stored by `createNewRcvChatItem` (`Store/Messages.hs:563`); `CIMeta.msgSigned` (`Messages.hs:520`).
- **CLI**: `sigStatusStr` (`View.hs:389`) renders "(signed)" / "(signed, no key to verify)".

Missing: (1) the decision to sign content; (2) per-send plumbing from the API; (3) as-channel forward/display/guard (§5); (4) edit reuse; (5) the badge fix (§7); (6) the apps.

## Core changes (Haskell)

### 1. `signableContent` predicate

Next to `requiresSignature` (`Protocol.hs:1328`):

```haskell
-- | Content events whose authorship a member may optionally prove by signing.
signableContent :: CMEventTag e -> Bool
signableContent = \case
  XMsgNew_ -> True
  XMsgUpdate_ -> True
  _ -> False
```

### 2. Signing decision takes the opt-in flag

`groupMsgSigning` (`Internal.hs:2099`) gains a leading `Bool` — it stays blind to `showGroupAsSender` (as-channel posts sign with the owner's `CBGroup` binding like any member post):

```haskell
groupMsgSigning :: Bool -> GroupInfo -> ChatMsgEvent e -> Maybe MsgSigning
groupMsgSigning sign gInfo@GroupInfo {membership = GroupMember {memberId}, groupKeys = Just GroupKeys {publicGroupId, memberPrivKey}} evt
  | useRelays' gInfo && shouldSign =
      Just $ MsgSigning CBGroup (smpEncode (publicGroupId, memberId)) KRMember memberPrivKey
  where
    tag = toCMEventTag evt
    shouldSign = requiresSignature tag || (sign && signableContent tag)
groupMsgSigning _ _ _ = Nothing
```

Three call sites — all but the content/edit chain pass `False`: `sendGroupMemberMessages` (`Internal.hs:2108`), `sendGroupMessages_` (`:2337`, passes its threaded flag), and the direct `XGrpLeave` send (`Commands.hs:3019`).

### 3. Thread `sign :: Bool` through the send functions

Add the flag to `sendGroupMessages` (`Internal.hs:2302`), `sendGroupMessages_` (`:2335`), `sendGroupMessage` (`:2244`), and the content wrappers `sendGroupContentMessages` / `sendGroupContentMessages_` (`Commands.hs:4430` / `:4439`). Keep `sendGroupMessage'` (`Internal.hs:2250`) and `sendGroupMemberMessages` (`:2105`) unchanged by hardcoding `False`.

Behavior-preserving (every existing caller passes `False`) ⇒ its own commit. The only two variable-flag sites are content send (`Commands.hs:4469`) and edit (`:751`). Other callers pass `False`: `sendGroupMessages` — `Commands.hs:812,819,2821,2958` (and via `sendGroupMessage`); `sendGroupMessages_` — `Commands.hs:2869,3912`; `sendGroupMessage` — `Commands.hs:908,2721,3327,3875,3878,3882`.

`Bool`-choice note: `sign` joins `showGroupAsSender :: ShowGroupAsSender (= Bool)` and `live :: Bool` in `sendGroupContentMessages`/`_`. Place `sign` away from the other two flags in each signature (e.g. after `itemTTL`) to reduce silent transposition.

### 4. API: per-send `sign` flag

Add a field to `APISendMessages` (`Controller.hs:382`):

```haskell
| APISendMessages {sendRef :: SendRef, liveMessage :: Bool, ttl :: Maybe Int, signMessages :: Bool, composedMessages :: NonEmpty ComposedMessage}
```

Parser (`Commands.hs:5104`), defaulting off so old command strings still parse:

```haskell
"/_send " *> (APISendMessages <$> sendRefP <*> liveMessageP <*> sendMessageTTLP <*> signMessagesP <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP))
-- signMessagesP = " sign=" *> onOffP <|> pure False   (after sendMessageTTLP)
```

The ~8 internal positional constructors of `APISendMessages` must gain the field (`False`): `Commands.hs:2394,2403,2423,2443,2451,2496,3238,3279,3288`. Compiler-caught, part of the behavior-preserving commit.

The handler (`Commands.hs:654-667`) flows `signMessages` to `sendGroupContentMessages` for group sends and ignores it for direct sends. `APIReportMessage` (`:693-698`) passes `False`.

### 5. As-channel signing: forward with member id, display from `asGroup`, enforce owner

An owner may sign an as-channel post (no gate on `showGroupAsSender`). Three pieces make it verifiable while displaying as the channel:

- **Forward (`Store/Delivery.hs:158`)**: route signed as-channel posts via `FwdMember` so subscribers can verify; keep unsigned as-channel anonymous via `FwdChannel`:
  ```haskell
  fwdSender = if showGroupAsSender && isNothing chatBinding_ then FwdChannel else FwdMember senderMemberId senderMemberName
  ```
  (`chatBinding_`/`sigs_` are already read here for `verifiedMsg`; `isNothing chatBinding_` ⇔ unsigned. Non-as-channel posts already use `FwdMember`, unchanged.)
- **Display**: the recipient already derives "as channel" from the signed `asGroup` flag in `MsgContainer` / `XMsgUpdate`, independent of `fwdSender` — `newGroupContentMessage` `sentAsGroup = asGroup_ == Just True` (`Subscriber.hs:2154`), `groupMessageUpdate` `showGroupAsSender = fromMaybe (isNothing m_) asGroup_` (`:2212`). So a `FwdMember` + `asGroup=True` post verifies against the owner and renders as the channel with the verified badge.
- **Owner guard (security, MUST)**: the forwarded `XMsgNew` path (`Subscriber.hs:3766 → newGroupContentMessage`) MUST reject as-channel display unless the verified author is an owner — parity with `groupMessageUpdate` (`:2213`) and the direct path (`:1047`); reuse the `validSender … CIChannelRcv == GROwner` pattern (`:2112`). Without it a non-owner's signed `asGroup=True` post renders as "from the channel".
- **Invariant**: `FwdChannel` never carries a signature (signed posts always route via `FwdMember`). Assert this in `encodeFwdElement` (`Batch.hs:108`) as a regression guard.

`sendGroupContentMessages_` passes the API `sign` straight through (no `&& not showGroupAsSender` gate). The owner's own as-channel item is marked signed/verified like any signed send.

### 6. Edit reuse (`XMsgUpdate`)

Group edit, `Commands.hs:739-757`. The own sent item is loaded with `CIMeta` (`:739`); add `msgSigned` to the pattern and reuse it:

```haskell
let reuseSign = isJust msgSigned
SndMessage {msgId, signedMsg_} <- sendGroupMessage user gInfo scope recipients reuseSign event
```

For own sent items, `msgSigned` is `Just MSSVerified` iff signed (`createNewSndChatItem`, `Store/Messages.hs:548`), so `isJust` is the right test. An edit is signed exactly when the original was. Direct/local edits need no change. (`signedMsg_` feeds the §7 badge update.)

### 7. Badge fix: refresh `msg_signed` on in-place content update (security)

Finding: `updateGroupChatItem_` (`Store/Messages.hs:2758`) updates content/status/timed but not `msg_signed` (the `UPDATE` at `:2766`); `updateGroupChatItem` (`:2749`) builds `ci'` via the shared `updatedChatItem` (`:2547`), which carries the original `meta.msgSigned`. Invisible today (content never signed); once content is signed and badged, an in-place edit from an unsigned, relay-forged `XMsgUpdate` would keep a stale `MSSVerified` badge over attacker content.

Why pass it in: the `MSSVerified`/`MSSSignedNoKey` outcome is computed at receive by `withVerifiedMsg` and lives on the chat item; the `messages` row holds signature bytes but not the verification outcome. So it must come from receive-time `RcvMessage.msgSigned`.

Fix (contained to the group helper): add a `Maybe MsgSigStatus` param to `updateGroupChatItem` (`:2749`); after `updatedChatItem` builds `ci'`, override `ci'`'s `meta.msgSigned`, and add `msg_signed = ?` to `updateGroupChatItem_`'s `UPDATE`. `updateGroupChatItem_` is called only from `updateGroupChatItem`. Leave the shared `updatedChatItem` (`:2547`) unchanged — it serves the unsigned direct/local paths.

All five callers pass an explicit value:
- `Commands.hs:757` (sender edit): `MSSVerified <$ signedMsg_` from the returned `SndMessage`.
- `Subscriber.hs:2275` (recipient in-place edit, `updateCI` — the main spoof path): `msgSigned` from the handler's `RcvMessage`. Unsigned forged edit ⇒ `Nothing` ⇒ badge removed; verified ⇒ kept.
- `Subscriber.hs:2235` (recipient edit, `catchCINotFound` restore branch): same `msgSigned`.
- `Subscriber.hs:1190` (`mdeUpdatedCI` decryption-error marker): `Nothing`.
- `Subscriber.hs:1554` (`upsertBusinessRequestItem`): `Nothing` (never a relay channel).

Re-grep `updateGroupChatItem\b` before implementing — a missed caller silently reintroduces the spoof.

### 8. Paths deliberately left unsigned

- Reactions (`XMsgReact`, `Commands.hs:908`) and deletes (`XMsgDel`, `Commands.hs:811/818/3911`): pass `False` this PR (deletes → PR 2).
- Auto-reply welcome content via `sendGroupMessage'` ⇒ `False`.

## App changes (iOS + Kotlin)

Locate by symbol — app line numbers drift independently.

- **A. Decode the status.** JSON tags come from `enumJSON (dropPrefix "MSS")`: `MSSVerified → "verified"`, `MSSSignedNoKey → "signedNoKey"` — not the DB strings "verified"/"no_key". Add an optional `msgSigned: MsgSigStatus?` to `CIMeta` on both platforms (iOS `ChatTypes.swift`; Kotlin `ChatModel.kt`), decoding `verified`/`signedNoKey`. Optional ⇒ old core JSON decodes safely.
- **B. Composer long-press option + thread `sign` to the API.** No device preference — the long-press is the only entry. Add `sign: Bool` to the send closure (default off) and a long-press item next to "Disappearing message" ("Sign message" / "Send without signing", iOS `SendMessageView.swift`; Kotlin `SendMsgView.kt`). Show it for a relay channel where the membership has a signing key (add a derived `memberSigningAvailable` to app `GroupInfo` JSON if needed). It is shown for as-channel sends too, with an explicit note that signing an as-channel post reveals you as the author (§5 tradeoff). Append `sign=on|off` in `apiSendMessages` on both platforms.
- **C. Recipient indicator.** In the message meta row (`CIMetaView`, chat view only), show `checkmark.seal` when `meta.msgSigned == verified`, placed in the trust cluster next to the `lock` and before the timestamp. iOS: append `statusIconText("checkmark.seal", color)` in `ciMetaText`. Kotlin: add an `Icon` branch in `CIMetaText` **and** the matching `iconSpace` branch in `reserveSpaceForMeta` (the in-file contract requires the two to match); add the matching seal vector to `MR.images`. Omit `signedNoKey` (only `.verified` is badged). Own signed items use the same glyph. Conversation list (`ChatPreviewView`) unchanged. Surface the "verified ≠ timestamp/ordering/completeness" caveat in help.

## Compatibility

- Wire format unchanged (the batch-element signature prefix already exists); no `chatVRange` bump; pre-feature relay-capable peers verify/accept.
- API command `sign=` is additive with a default; app + core ship together.
- No DB migration — `chat_items.msg_signed` already exists (written by `createNewChatItem_`, `Store/Messages.hs:614`; read by `mkCIMeta`).
- The new optional app `msgSigned` decodes as absent on older cores.

## Edge cases, races, correctness

- **Key mismatch → drop** (threat model): a signed content message whose author key doesn't match the recipient's roster key is dropped + `RGEMsgBadSignature`, not shown unsigned. Test it; note it in help.
- **Member without keys** (`groupKeys = Nothing`): `groupMsgSigning` returns `Nothing` even with `sign` ⇒ silent unsigned send. The UI gate prevents offering it; document the degrade.
- **Non-relay groups**: the `useRelays'` guard ⇒ never signed; UI must not offer it.
- **Live messages**: each `XMsgUpdate` reuses the item's `msgSigned`, so every increment is signed if the original was. Acceptable cost.
- **Non-batched path**: `sndMessageMBR` uses raw `msgBody`, never reached in relay groups (`memberSendAction → MSASendBatched`). Add a test-asserted invariant; optionally route it through `encodeBatchElement signedMsg_`.
- **History downgrade (posts, by design this PR)**: relay history catch-up rebuilds content unsigned and as-channel via `FwdChannel` (`sendHistory` / `processContentItem`, `Internal.hs:1269` / `:1340/1363`; the relay re-encodes from `MsgContent` and has no author key). So a signed post (channel or member) is delivered unsigned on catch-up — no badge, and an as-channel post re-anonymizes. Graceful (absence ≠ forgery); document and test. PR 2 preserves signatures through history.
- **Concurrency**: signing/verification are pure given keys; no new shared state. Send holds `withGroupLock`; receive runs under existing serialization. No new races.

## Tests

- Protocol (`tests/ProtocolTests.hs`): round-trip signed `XMsgNew`/`XMsgUpdate`; assert binding `CBGroup <> (publicGroupId, memberId)`; verify accepts the right key, rejects wrong key / altered body / altered binding.
- Integration (`tests/ChatTests/`, relay/channel setup in `Groups.hs`): sign+verify ⇒ "(signed)"; off/default ⇒ none; missing roster key ⇒ "(signed, no key to verify)"; edit reuse keeps/omits the badge; **edit downgrade** — unsigned forged `XMsgUpdate` over a signed item ⇒ badge removed (§7); **as-channel signed** — owner `as_group=on sign=on` ⇒ recipient verifies and shows "(signed)" while displaying as the channel; **as-channel unsigned** — forwards via `FwdChannel`, no member id on the wire; **as-channel spoof** — non-owner `asGroup=on sign=on` ⇒ rejected (§5 guard); **history downgrade** — live recipient "(signed)", catch-up recipient not; **key-mismatch drop**; forgery rejection ⇒ `RGEMsgBadSignature`.
- App: minimal decode test that `"verified"` / `"signedNoKey"` parse to the right enum on both platforms.

## Commit plan (PR 1)

1. **Structural (behavior-preserving)**: add `signableContent`, parameterize `groupMsgSigning` + send/content functions with `sign :: Bool`, update all callers and `APISendMessages` constructors with `False`.
2. **Badge fix (independent, no-op today)**: add `Maybe MsgSigStatus` to `updateGroupChatItem`, override `meta.msgSigned`, add `msg_signed` to the `UPDATE`, update all five callers (§7), with a regression test.
3. **Feature (core)**: `APISendMessages` field + parser; content send and edit pass the real flag; as-channel forward/display/owner-guard (§5).
4. **App**: decode + recipient indicator.
5. **App**: composer long-press option + `apiSendMessages` wiring.
6. **Tests** (may accompany 2/3).

Each commit builds and passes tests independently.

### Pre-implementation gates

- **MUST**: re-grep `updateGroupChatItem\b` and confirm every caller passes an explicit `Maybe MsgSigStatus` (§7). Baseline: `Commands.hs:757`; `Subscriber.hs:1190,1554,2235,2275`.
- **MUST**: the as-channel owner guard (§5) is on the forwarded `XMsgNew` path, and `FwdChannel` carries no signature.
- **SHOULD**: re-grep the `sendGroupMessages` / `sendGroupMessage` / `sendGroupMessages_` / `groupMsgSigning` callers; only content-send and edit pass a variable `sign`.
- **SHOULD**: the "verified" caveats (no timestamp/ordering; history downgrade; key-mismatch drop) and the as-channel de-anonymization warning are surfaced in UI/help, and those tests exist.

## Deferred to PR 2: signed deletes + recipient enforcement

Goal: stop a relay forging an owner-attributed delete to censor a signed post. Worth doing only with recipient enforcement, which needs all three of:

1. **Signable deletes**: add `XMsgDel_` to `signableContent`; sign each delete **per item** (keyed off the target item's stored `msgSigned`), because the delete send sites build multi-item batches (`Commands.hs:811/818/3911`).
2. **Recipient enforcement** in `groupMessageDelete` (`Subscriber.hs:2284`): reject an unsigned/unverified delete of a locally-`MSSVerified` item. Works for self-delete and moderation (a moderation delete verifies against the moderator's key; the role check still applies). Live-path only — deletes are not replayed in history.
3. **History signature preservation for posts**: so catch-up members hold posts as verified and (2) covers them. `processContentItem` re-encodes from `MsgContent`; preserving the signature requires the original signed bytes (re-encoding invalidates it — `Store/Delivery.hs:160`). First design question: does the `messages` row survive long enough to forward on catch-up, or must signed bytes be persisted on the chat item (migration)?

Honest limit: enforcement protects a post a recipient already holds verified; a relay that delivers the original post unsigned sidesteps it (visible as a missing badge, not prevented).

## Out of scope / future

- Group-level "required signing" owner setting — rejects unsigned messages group-wide, closing the optional-downgrade gap.
- **Verifiable-anonymous as-channel** — a channel-level signature (ring signature over the owner set, or a shared/authorization-chain channel key) that proves "a valid owner" without revealing which, so an as-channel post is verifiable *and* keeps sender anonymity. This PR's per-member signing cannot do both; signing an as-channel post reveals the owner (§5).
- Signing reactions; signing auto-reply content; verifiable reports (signed `MCReport`).
