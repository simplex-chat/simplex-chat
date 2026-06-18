# Plan: optional signing of channel content messages (`XMsgNew` / `XMsgUpdate`)

Grounded on `f/public-groups` @ `b6bd4a4d1`. The prerequisites are already merged here (#7048 roster-over-inline-file, #7089 roster transfers, #7058 p2p signature verification), so the roster distributes each member's public key and verification works. Anchors below are from this branch — re-confirm by symbol, they drift as the branch advances.

This is **PR 1**: a member can optionally sign their own channel posts and edits so recipients holding the signed roster can verify authorship + integrity. Signed deletes with recipient enforcement are **PR 2** (sketched at the end).

## Goal / user problem

In relay channels, content (`XMsgNew`) is forwarded by relays and is not signed today — only group-state events are (`requiresSignature`, `Protocol.hs:1328`). A relay can therefore forge or alter content attributed to a member. This feature lets a member optionally attach their member signature; recipients with the roster verify it.

## Decisions

- UI: both a device default ("sign my channel messages", off) and a per-send long-press override (mirrors custom disappearing-message TTL).
- Default off, with an in-UI explanation of the tradeoff: a signature is transferable, non-repudiable proof of authorship.
- Recipient indicator in scope (iOS + Kotlin) — signing is useless if invisible to readers.
- Scope this PR: `XMsgNew` + `XMsgUpdate`. Edits reuse the original's setting. Reactions stay unsigned. Deletes → PR 2.

## Threat model

Actors: the sending member, recipients, and untrusted **chat relays** that forward content + roster.

- **Forgery of member content** — closed for signed messages: the relay lacks the Ed25519 key, and the signature binds `(publicGroupId, memberId, body)`, so no forgery, cross-bind, or alteration.
- **Downgrade / stripping** (residual, by design) — optional signing lets a relay strip a signature and deliver unsigned. Absence of a badge is *not* proof of forgery; only the presence of a verified badge is a guarantee. A future "required signing" group setting would close this (out of scope).
- **Publish-as-channel de-anonymization** (structurally prevented — §5) — channels let an owner "publish as the channel": subscribers see a post as from the channel, not the owner. `groupMsgSigning` (`Internal.hs:2099`) is blind to `showGroupAsSender`, so without a guard it would sign as-channel content with binding `(publicGroupId, ownerMemberId)`. That signature is emitted on the wire (`encodeFwdElement`, `Batch.hs:108`), and a malicious relay re-attributes the forward as `fwdSender = FwdMember <owner>` (it controls `fwdSender`, derived from the stored `showGroupAsSender`, `Store/Delivery.hs:158`). Recipients then reconstruct the binding and verify it as `MSSVerified` — turning a deniable leak into non-repudiable proof of who authored an intentionally anonymous post. Because any relay can re-attribute a signature once it exists, the only defense is to never create it for as-channel sends (§5). Receiving `FwdChannel` as `VMUnsigned` (`Subscriber.hs:3758`) does not help — the attack uses `FwdMember`.
- **Non-repudiation** (tradeoff, by design) — a verified signature is transferable proof of authorship; hence opt-in/off. For as-channel posts the loss is unacceptable, not a tradeoff — hence the §5 exclusion.
- **What "verified" proves** — the signed input is `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, and `msgBody` embeds `sharedMsgId`, `MsgScope`, content. It proves authorship + integrity + group/member/scope/message binding, and nothing else — not `fwdBrokerTs` (relay-controlled), ordering, or completeness. Surface this in help.
- **Bad signature is fail-closed** — a signature that fails to verify drops the message and creates an `RGEMsgBadSignature` item (`Subscriber.hs:3798-3800`). New consequence for content: a signed message whose author key does not match the recipient's pinned roster key (key rotation, stale/lagging roster, TOFU mismatch) is dropped, not shown unsigned — a stronger failure mode than for unsigned content. State events already behave this way, but content is higher-volume and user-visible. Needs an edge-case test and a note in help.
- **Replay** — the binding covers `sharedMsgId` + `MsgScope`; cross-scope/group replay is blocked, same-message replay is a dedup duplicate.

## What already exists (reused unchanged)

- **Send / sign**: `groupMsgSigning` (`Internal.hs:2099`) → `createSndMessages` threads `Maybe MsgSigning` → `createNewSndMessage` Ed25519-signs `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, storing `signedMsg_` in `SndMessage` (`Messages.hs:1156`).
- **Wire**: `batchMessages` prepends the signature via `encodeBatchElement` (`Batch.hs:45,69`); relay groups always batch.
- **Receive / verify**: `withVerifiedMsg` (`Subscriber.hs:3794`) runs for all group messages; `XMsgNew_`/`XMsgUpdate_` are not in `requiresSignature` ⇒ `signatureOptional` (`:3819`), so signed → `MSSVerified`/`MSSSignedNoKey`, unsigned → accepted. No protocol-version bump.
- **Persistence**: own item — `createNewSndChatItem` sets `MSSVerified <$ signedMsg_` (`Store/Messages.hs:548`); received item — `RcvMessage.msgSigned` (`Messages.hs:1174`) is stored by `createNewRcvChatItem` (`Store/Messages.hs:563`); `CIMeta.msgSigned` (`Messages.hs:520`).
- **CLI**: `sigStatusStr` (`View.hs:389`) renders "(signed)" / "(signed, no key to verify)".

Missing: (1) the decision to sign content; (2) per-send plumbing from the API; (3) edit reuse; (4) the §7 badge fix; (5) the §5 anonymity gate; (6) the apps.

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

`groupMsgSigning` (`Internal.hs:2099`) gains a leading `Bool`:

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

Mandatory state-event signing is unchanged (the `requiresSignature` branch). In non-relay groups or for keyless members, `sign` is a no-op (`Nothing`).

`groupMsgSigning` has three call sites — all but the threaded content/edit chain pass `False`: `sendGroupMemberMessages` (`Internal.hs:2108`), `sendGroupMessages_` (`Internal.hs:2337`, passes its threaded flag), and the direct `XGrpLeave` send (`Commands.hs:3019`).

### 3. Thread `sign :: Bool` through the send functions

Add the flag to `sendGroupMessages` (`Internal.hs:2302`), `sendGroupMessages_` (`:2335`), `sendGroupMessage` (`:2244`), and the content wrappers `sendGroupContentMessages` / `sendGroupContentMessages_` (`Commands.hs:4430` / `:4439`). Keep `sendGroupMessage'` (`Internal.hs:2250`) and `sendGroupMemberMessages` (`:2105`) unchanged by hardcoding `False` internally.

Behavior-preserving (every existing caller passes `False`) ⇒ its own commit. The only two variable-flag sites are content send (`Commands.hs:4469`) and edit (`Commands.hs:751`). Other callers pass `False`: `sendGroupMessages` — `Commands.hs:812,819,2821,2958` (and via `sendGroupMessage`); `sendGroupMessages_` — `Commands.hs:2869,3912` (and via `sendGroupMessage'`/`sendGroupMessages`); `sendGroupMessage` — `Commands.hs:908,2721,3327,3875,3878,3882`.

`Bool`-choice mitigation: `sign` joins `showGroupAsSender :: ShowGroupAsSender (= Bool)` and `live :: Bool` in `sendGroupContentMessages`/`_` — three same-typed flags, where transposing `sign` and `showGroupAsSender` is the §5 de-anonymization. Keep the §5 gate (below) the single place that combines `sign` with `showGroupAsSender`, and place `sign` away from the other two flags in each signature (e.g. after `itemTTL`) to reduce silent transposition.

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

Wire: `/_send <ref> live=.. ttl=.. sign=on|off json ...`. Per-send granularity (like `ttl`); this is the app↔core boundary, not a protocol-compat concern. The ~8 internal positional constructors of `APISendMessages` must gain the field (`False`): `Commands.hs:2394,2403,2423,2443,2451,2496,3238,3279,3288`. Compiler-caught, but part of the behavior-preserving commit's scope.

### 5. Content send path + as-channel gate (HIGH)

`sendGroupContentMessages` / `sendGroupContentMessages_` (`Commands.hs:4430` / `:4439`) gain `sign :: Bool`. `showGroupAsSender` is in scope at the send site (`:4469`); as-channel posts are never signed:

```haskell
let sign' = sign && not showGroupAsSender
(msgs_, gsr) <- sendGroupMessages user gInfo Nothing showGroupAsSender recipients sign' chatMsgEvents
```

This gate is structural and must live in core (not only UI). It also keeps the sender's own as-channel item unsigned and keeps §6 edit-reuse consistent. The `APISendMessages` handler (`Commands.hs:654-667`) flows `signMessages` to `sendGroupContentMessages` for group sends and ignores it for direct sends; `APIReportMessage` (`:693-698`) passes `False` (reports unsigned this PR).

### 6. Edit reuse (`XMsgUpdate`)

Group edit, `Commands.hs:739-757`. The own sent item is loaded with `CIMeta` (`:739`); add `msgSigned` to the pattern and reuse it:

```haskell
let reuseSign = isJust msgSigned
SndMessage {msgId, signedMsg_} <- sendGroupMessage user gInfo scope recipients reuseSign event
```

For own sent items, `msgSigned` is `Just MSSVerified` iff signed (`createNewSndChatItem`, `Store/Messages.hs:548`), so `isJust` is the right test. An edit is signed exactly when the original was, automatically consistent with §5 (as-channel originals are never signed ⇒ edits stay unsigned). Direct/local edits need no change. (`signedMsg_` from the returned `SndMessage` feeds the §7 badge update below.)

### 7. Badge fix: refresh `msg_signed` on in-place content update (security)

Finding: `updateGroupChatItem_` (`Store/Messages.hs:2758`) updates content/status/timed but not `msg_signed` (the `UPDATE` at `:2766`); `updateGroupChatItem` (`:2749`) builds `ci'` via the shared `updatedChatItem` (`:2547`), which carries the original `meta.msgSigned`. Invisible today (content is never signed); once content is signed and badged, an in-place edit from an unsigned, relay-forged `XMsgUpdate` would keep a stale `MSSVerified` badge over attacker content.

Why pass it in: the `MSSVerified`/`MSSSignedNoKey` outcome is computed at receive by `withVerifiedMsg` and lives on the chat item; the `messages` row holds signature bytes but not the verification outcome. So it must come from receive-time `RcvMessage.msgSigned`.

Fix (contained to the group helper): add a `Maybe MsgSigStatus` param to `updateGroupChatItem` (`:2749`); after `updatedChatItem` builds `ci'`, override `ci'`'s `meta.msgSigned`, and add `msg_signed = ?` to `updateGroupChatItem_`'s `UPDATE`. `updateGroupChatItem_` is called only from `updateGroupChatItem`, so this is self-contained. Leave the shared `updatedChatItem` (`:2547`) unchanged — it serves the unsigned direct/local paths.

All five callers pass an explicit value (no implicit "preserve"):

- `Commands.hs:757` (sender edit): `MSSVerified <$ signedMsg_` from the returned `SndMessage` (= the reused setting).
- `Subscriber.hs:2275` (recipient in-place edit, `updateCI` — the main spoof path): `msgSigned` from the handler's `RcvMessage`. Unsigned forged edit ⇒ `Nothing` ⇒ badge removed; verified ⇒ kept.
- `Subscriber.hs:2235` (recipient edit, `catchCINotFound` restore branch): same `msgSigned` from the `RcvMessage`.
- `Subscriber.hs:1190` (`mdeUpdatedCI` decryption-error marker): `Nothing`.
- `Subscriber.hs:1554` (`upsertBusinessRequestItem`): `Nothing` (never a relay channel).

Net: signed status is set explicitly from the source of current content in every group update path, so a stale badge cannot exist. Re-grep `updateGroupChatItem\b` before implementing — a missed caller silently reintroduces the spoof.

### 8. Paths deliberately left unsigned

- Reactions (`XMsgReact`, `Commands.hs:908`) and deletes (`XMsgDel`, `Commands.hs:811/818/3911`): pass `False` this PR (deletes → PR 2).
- Auto-reply welcome content via `sendGroupMessage'` ⇒ `False`.

## App changes (iOS + Kotlin)

Locate by symbol — app line numbers drift independently.

- **A. Decode the status.** JSON tags come from `enumJSON (dropPrefix "MSS")`: `MSSVerified → "verified"`, `MSSSignedNoKey → "signedNoKey"` — not the DB strings "verified"/"no_key". Add an optional `msgSigned: MsgSigStatus?` to `CIMeta` on both platforms (iOS `ChatTypes.swift`; Kotlin `ChatModel.kt`), decoding `verified`/`signedNoKey`. Optional ⇒ old core JSON decodes safely.
- **B. Device preference (default off).** iOS: `@AppStorage` toggle in `PrivacySettings.swift` (pattern: `protectScreen`) with a non-repudiation footer. Kotlin: `mkBoolPreference` + `SettingsPreferenceItem` in `PrivacySettings.kt`. App-side only (like `customDisappearingMessageTime`), not core `AppSettings`.
- **C. Composer override + thread `sign` to the API.** Add `sign: Bool?` to the send closure (`nil` ⇒ device default), and a long-press item next to "Disappearing message" ("Sign message" / "Send without signing"). Gate visibility on: relay channel + membership has a signing key + not as-channel (the UI half of §5). If app `GroupInfo` lacks relay/key state, add a derived `memberSigningAvailable` boolean to its JSON. Append `sign=on|off` in `apiSendMessages` on both platforms.
- **D. Recipient indicator.** Show a "signed by author" indicator when `meta.msgSigned == verified` in the meta row (iOS `CIMetaView.swift`; Kotlin `CIMetaView.kt`, including `reserveSpaceForMeta`). Render `signedNoKey` muted or not at all so it isn't read as verified. Own signed items use the same indicator. Surface the "verified ≠ timestamp/ordering/completeness" caveat in help.

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
- **Non-batched path**: `sndMessageMBR` uses raw `msgBody`, never reached in relay groups (`memberSendAction → MSASendBatched`). Add a test-asserted invariant; optionally route it through `encodeBatchElement signedMsg_` so a routing change can't silently drop signatures.
- **`FwdChannel` regression-guard (defense-in-depth, not a relay defense)**: `encodeFwdElement` (`Batch.hs:108`) emits the signature unconditionally; §5 makes it absent for as-channel at source. Add an assertion that `encodeFwdElement` carries no signature when `fwdSender = FwdChannel` — this only guards our own code against a §5 regression; it does not constrain a malicious relay, which re-attributes via `FwdMember`.
- **History downgrade (posts, by design this PR)**: relay history catch-up rebuilds content unsigned (`sendHistory` / `processContentItem`, `Internal.hs:1269` / `:1340`, re-encode from `MsgContent`; the relay has no author key). So for the same post a live-forward recipient sees a badge while a catch-up recipient does not. Graceful (absence ≠ forgery); document and test. (PR 2 preserves signatures through history.)
- **Concurrency**: signing/verification are pure given keys; no new shared state. Send holds `withGroupLock`; receive runs under existing receive-loop serialization. No new races.

## Tests

- Protocol (`tests/ProtocolTests.hs`): round-trip signed `XMsgNew`/`XMsgUpdate`; assert binding `CBGroup <> (publicGroupId, memberId)`; verify accepts the right key, rejects wrong key / altered body / altered binding.
- Integration (`tests/ChatTests/`, relay/channel setup in `Groups.hs`): sign+verify ⇒ "(signed)"; off/default ⇒ none; missing roster key ⇒ "(signed, no key to verify)"; edit reuse keeps/omits the badge; edit downgrade — unsigned forged `XMsgUpdate` over a signed item ⇒ badge removed (§7); as-channel never signed — `as_group=on sign=on` ⇒ no "(signed)" and no signature on the wire/stored message (§5); history downgrade — live recipient "(signed)", catch-up recipient not; key-mismatch drop; forgery rejection ⇒ `RGEMsgBadSignature`.
- App: minimal decode test that `"verified"` / `"signedNoKey"` parse to the right enum on both platforms (guards §A).

## Commit plan (PR 1)

1. **Structural (behavior-preserving)**: add `signableContent`, parameterize `groupMsgSigning` + the send/content functions with `sign :: Bool`, update all callers and `APISendMessages` constructors with `False`. Reviewable as "no behavior change".
2. **Badge fix (independent, no-op today)**: add `Maybe MsgSigStatus` to `updateGroupChatItem`, override `meta.msgSigned`, add `msg_signed` to the `UPDATE`, update all five callers (§7), with a regression test that bites once signing exists.
3. **Feature (core)**: `APISendMessages` field + parser; content send and edit pass the real flag (with the §5 gate).
4. **App**: decode + recipient indicator.
5. **App**: device preference + composer override + `apiSendMessages` wiring.
6. **Tests** (may accompany 2/3).

Each commit builds and passes tests independently (bisect/rollback).

### Pre-implementation gates

- **MUST**: the §5 as-channel gate lives in core (`sign && not showGroupAsSender`), and the app option is hidden for as-channel sends — not UI-only.
- **MUST**: re-grep `updateGroupChatItem\b` and confirm every caller passes an explicit `Maybe MsgSigStatus` (§7). Baseline set: `Commands.hs:757`; `Subscriber.hs:1190,1554,2235,2275`.
- **SHOULD**: re-grep the `sendGroupMessages` / `sendGroupMessage` / `sendGroupMessages_` / `groupMsgSigning` callers; only content-send and edit pass a variable `sign`, all others `False`.
- **SHOULD**: the "verified" caveats (no timestamp/ordering; history downgrade; key-mismatch drop) are surfaced in help, and the history-downgrade and key-mismatch tests exist.

## Deferred to PR 2: signed deletes + recipient enforcement

Goal: stop a relay forging an owner-attributed delete to censor a signed post. This is worth doing only with recipient enforcement, which needs all three of:

1. **Signable deletes**: add `XMsgDel_` to `signableContent`; sign each delete per item (keyed off the target item's stored `msgSigned`), because the delete send sites build multi-item batches (`Commands.hs:811/818/3911`) — so signing is per-event, not the per-send flag this PR uses.
2. **Recipient enforcement** in `groupMessageDelete` (`Subscriber.hs:2284`): reject an unsigned/unverified delete of a locally-`MSSVerified` item. Works for self-delete and moderation (a moderation delete verifies against the moderator's key; the existing role check still applies). Live-path only — deletes are not replayed in history.
3. **History signature preservation for posts**: so catch-up members hold posts as verified and (2) covers them. `sendHistory` / `processContentItem` re-encode from `MsgContent`; preserving the signature requires the original signed bytes (re-encoding invalidates the signature — see the comment at `Store/Delivery.hs:160`). First design question: does the relay's `messages` row (which already carries `msg_chat_binding`/`msg_signatures`, used by the live forward at `Store/Delivery.hs:155-165`) survive long enough to forward on catch-up, or must signed bytes be persisted on the chat item (migration)?

Honest limit: enforcement protects a post a recipient already holds verified; a relay that delivers the original post unsigned (live-strip, or pre-(3) catch-up) sidesteps it — visible as a missing badge, but not prevented. So this hardens, not eliminates, relay suppression.

## Out of scope / future

- Group-level "required signing" owner setting — rejects unsigned messages group-wide, closing the optional-downgrade gap holistically.
- Signing reactions; signing auto-reply content; verifiable reports (signed `MCReport`).
