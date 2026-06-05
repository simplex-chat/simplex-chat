# Plan: optional signing of channel content messages (`XMsgNew` / `XMsgUpdate`)

## Goal / user problem

In relay-based channels, content (`XMsgNew`) is forwarded by relays and is **not** signed today (only group-state events are — `requiresSignature`, `Protocol.hs:1251`), so a relay can forge or alter content attributed to a member. This feature lets a member *optionally* attach their member signature, so recipients holding the (signed) roster can verify authorship + integrity.

Decisions:
- **UI: both** — device-stored default ("sign my channel messages", off) + per-send long-press override (mirrors custom disappearing-message TTL).
- **Default: off**, with an in-UI tradeoff explanation (signing = non-repudiable, transferable proof of authorship).
- **Recipient indicator: in scope** (iOS + Kotlin) — signing is useless if invisible to readers.
- **Event scope: `XMsgNew` + `XMsgUpdate` only**; edits reuse the original's setting. `XMsgReact`/`XMsgDel` stay unsigned in v1.

## Prerequisites / sequencing

Lands after #7017 (signed roster) and #7048 (roster over inline files; `GRMember` role). Neither merged yet (branch `f/allow-sign-new-msg`; `git log` tops at #7043). Dependency is specific: *verification* needs the sender's member public key, distributed via the roster; without it a signed message degrades to `MSSSignedNoKey` rather than `MSSVerified`. Integration tests must use the roster/channel setup from those PRs.

**Line numbers are pre-rebase** (grounded against #7043); #7017/#7048 shift every anchor, so **re-locate by symbol**. The dependency PRs add no 6th `updateGroupChatItem` caller, but other branches are queued (`f/channel-comments`, `f/public-groups-members-in-roster`) — hence the caller re-check gate below.

## What already exists (so the change stays small)

Wire format, signing, verification, DB persistence, and CLI display are present and reused unchanged:
- **Send signing:** `groupMsgSigning` (`Internal.hs:1963`) → `createSndMessages` threads `Maybe MsgSigning` (`:1950`) → `createNewSndMessage` Ed25519-signs `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, storing `SignedMsg` in `SndMessage.signedMsg_` (`Store/Messages.hs:234`; `Messages.hs:1156`).
- **Wire:** `batchMessages` prepends the signature via `encodeBatchElement` (`Batch.hs:46,65`); relay groups always batch (`memberSendAction` → only `MSASendBatched` under `useRelays'`, `Internal.hs:2222,2228`).
- **Receive verify:** `withVerifiedMsg` (`Subscriber.hs:3469`) runs for all group messages (`:1004`, forwarded `:3431`); `XMsgNew_`/`XMsgUpdate_` ∉ `requiresSignature` ⇒ `signatureOptional` (`:3491`), so signed → `MSSVerified`/`MSSSignedNoKey`, unsigned → accepted. **No protocol-version bump.**
- **Sent-item persistence:** `createNewSndChatItem` sets `msgSigned = MSSVerified <$ signedMsg_` (`Store/Messages.hs:550`) — own item auto-marked, readable by the edit path.
- **Received-item persistence:** `createNewRcvChatItem` records `RcvMessage.msgSigned` (`Store/Messages.hs:565,567`); `CIMeta.msgSigned :: Maybe MsgSigStatus` (`Messages.hs:520`).
- **CLI:** `sigStatusStr` (`View.hs:388`) appends `" (signed)"` / `" (signed, no key to verify)"`.

Missing: (1) the *decision* to sign content (`groupMsgSigning` returns `Nothing` for content today); (2) per-send plumbing from the API; (3) reuse on edit; (4) the §7 stale-badge fix; (5) the §5 anonymity gate (HIGH); (6) the apps.

## Threat model

Actors: member (sender), recipients, and **chat relays** that forward content + roster. Relays are untrusted for content authenticity.

- **Forgery of member content.** Signing closes it for signed messages: relay lacks the Ed25519 key; signature binds `(publicGroupId, memberId, body)` — no forgery, cross-bind, or alteration.
- **Downgrade / stripping (residual, by design).** Optional signing lets a relay strip a signature and deliver unsigned. Absence of a badge is **not** proof of forgery — only *presence* of a verified badge is a guarantee. A future "required signing" group setting would close it; out of scope.
- **Stale-badge spoof on edits (fixed — §7).** An in-place edit must not keep a `verified` badge over content from an unsigned, relay-forged `XMsgUpdate`.
- **Publish-as-channel de-anonymization (structurally prevented — §5).** Channels allow "publish as the channel" (`showGroupAsSender`/`asGroup`): subscribers see a post as *from the channel*, not the specific owner (Design Objective 6, `docs/protocol/channels-overview.md:214`); today a relay revealing the owner is only a *deniable* leak (`channels-overview.md:~237`). `groupMsgSigning` (`Internal.hs:1963-1967`) is blind to `showGroupAsSender`, so it would sign with binding `(publicGroupId, ownerMemberId)`, broadcast on the wire even for `FwdChannel` (`encodeFwdElement` → `encodeBatchElement signedMsg_`, `Batch.hs:108`). A malicious relay sets the live-forward `fwdSender` freely (it is derived from stored `sentAsGroup`, `Store/Delivery.hs:158`), so every subscriber verifies it as `MSSVerified` — turning the deniable leak into **non-repudiable proof** of which owner authored an intentionally anonymous post; the device-default toggle would trigger this silently. For an anonymity property this must be structurally impossible: signing is never applied to as-channel content (§5), the app option is hidden for as-channel sends (§C), and a defense-in-depth guard keeps `encodeFwdElement` signature-free for `FwdChannel` (Edge cases). (`processContentItem:1302` is the *history* path and rebuilds content unsigned — not the vector.)
- **Non-repudiation (tradeoff, by design).** A verified signature is transferable proof of authorship — a deniability loss; hence opt-in/off-by-default with UI explanation. For *as-channel* posts the loss is unacceptable, not a tradeoff — hence the §5 exclusion.
- **What "verified" means.** Signed input is `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, with `msgBody` embedding `sharedMsgId`, `MsgScope`, content (`Store/Messages.hs:242`). It proves **authorship + integrity + group/member/scope/message binding** — and nothing else: not `fwdBrokerTs` (relay-controlled, `Protocol.hs:382-387`), ordering, or completeness. Surface this in UI/help.
- **Signed content is still relay-suppressible.** `XMsgDel_` ∉ `requiresSignature` (`Protocol.hs:1252-1262`), so an unsigned relay-forged owner-attributed delete is accepted (role-based check vs. the relay-chosen author, `Subscriber.hs:~2269`). Pre-existing, within the relay's drop power; bounds signing's value (proves *what was said*, not that all is delivered).
- **Replay.** Binding covers `sharedMsgId` + `MsgScope`; cross-scope/group replay is blocked, same-message replay is a dedup duplicate.
- **Bad-signature spam (fail-closed, pre-existing).** Failed verification drops content with an `RGEMsgBadSignature` item per occurrence (`Subscriber.hs:3473-3475,3483`); a tampering relay can spam these. Inherited from state-event behavior.

## Core changes (Haskell)

### 1. Signable-content predicate

`Protocol.hs`, next to `requiresSignature` (`:1251`):
```haskell
-- | Content events whose authorship a member may optionally prove by signing.
signableContent :: CMEventTag e -> Bool
signableContent = \case
  XMsgNew_ -> True
  XMsgUpdate_ -> True
  _ -> False
```

### 2. Signing decision carries the opt-in

Named type near `MsgSigning` (`Protocol.hs:426`) — not a bare `Bool`:
```haskell
-- | Whether opt-in content signing applies to this group send.
-- Independent of mandatory state-event signing (requiresSignature),
-- which always applies in relay groups regardless of this value.
data ContentSig = SignContent | DontSignContent
  deriving (Eq, Show)
```
Extend `groupMsgSigning` (`Internal.hs:1963`):
```haskell
groupMsgSigning :: ContentSig -> GroupInfo -> ChatMsgEvent e -> Maybe MsgSigning
groupMsgSigning csig gInfo@GroupInfo {membership = GroupMember {memberId}, groupKeys = Just GroupKeys {publicGroupId, memberPrivKey}} evt
  | useRelays' gInfo && shouldSign =
      Just $ MsgSigning CBGroup (smpEncode (publicGroupId, memberId)) KRMember memberPrivKey
  where
    tag = toCMEventTag evt
    shouldSign = requiresSignature tag || (csig == SignContent && signableContent tag)
groupMsgSigning _ _ _ = Nothing
```
- `useRelays'`/`groupKeys = Just` guards unchanged: in non-relay groups or keyless members, `SignContent` is a no-op (`Nothing`).
- Mandatory state-event signing unaffected (`requiresSignature` branch preserved).

### 3. Thread `ContentSig` through the send functions

`groupMsgSigning` is called only in `sendGroupMessages_` (`Internal.hs:2134`) and `sendGroupMemberMessages` (`:1972`). Add a `ContentSig` param to `sendGroupMessages_` (`:2132`, used in `idsEvts`), `sendGroupMessages` (`:2100`, pass-through), `sendGroupMessage` (`:2088`, pass-through). Keep `sendGroupMessage'` (`:2094`) and `sendGroupMemberMessages` (`:1969`) unchanged by hardcoding `DontSignContent` internally.

Behavior-preserving (all existing callers pass `DontSignContent`) ⇒ its own commit. Call sites to pass `DontSignContent` (grep-verified):
- `sendGroupMessages`: `Subscriber.hs:1370`; `Commands.hs:793,800,2778,2909`.
- `sendGroupMessage`: `Commands.hs:889,2690,3272,3812,3815,3819`.
- `sendGroupMessages_` direct: `Commands.hs:2826,3849`.

The two variable-`ContentSig` sites are the feature (next commit): content send (`Commands.hs:4405`) and group edit (`Commands.hs:732`).

### 4. API: per-send `sign` flag

Add a field to `APISendMessages` (`Controller.hs:332`):
```haskell
| APISendMessages {sendRef :: SendRef, liveMessage :: Bool, ttl :: Maybe Int, signMessages :: Bool, composedMessages :: NonEmpty ComposedMessage}
```
Parser (`Commands.hs:5006`), mirroring `liveMessageP`/`sendMessageTTLP`, defaulting off so old command strings still parse:
```haskell
"/_send " *> (APISendMessages <$> sendRefP <*> liveMessageP <*> sendMessageTTLP <*> signMessagesP <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP))
-- with:  signMessagesP = " sign=" *> onOffP <|> pure False   (place after sendMessageTTLP, before " json ")
```
Wire: `/_send <ref> live=.. ttl=.. sign=on|off json ...`. Per-send granularity (like `ttl`), not per-`ComposedMessage`. API boundary (app↔core, same bundle) ⇒ not a protocol-compat concern.

### 5. Content send path

`sendGroupContentMessages` (`Commands.hs:4366`) and `sendGroupContentMessages_` (`:4375`) gain a `ContentSig` param. `showGroupAsSender` is in scope at the send site (`:4405`); **as-channel posts are never signed** (anonymity gate — see threat model):
```haskell
let csig' = if showGroupAsSender then DontSignContent else csig
(msgs_, gsr) <- sendGroupMessages user gInfo Nothing showGroupAsSender recipients csig' chatMsgEvents
```
This gate is structural (must live here, not only in UI); it also keeps the sender's own as-channel item unsigned and keeps §6 edit-reuse consistent.

- `APISendMessages` handler (`:637-650`): `signMessages` → `SignContent`/`DontSignContent`, passed down (both `SRGroup` and `SRDirect`; direct ignores it — `sendContactContentMessages` doesn't sign). The `:4405` gate then forces `DontSignContent` for as-channel sends regardless of the flag.
- `APIReportMessage` (`:679`): `DontSignContent` (reports unsigned in v1).

### 6. Edit / restore reuse (the `XMsgUpdate` requirement)

Group edit, `Commands.hs:710-742`. Own sent item loaded with `CIMeta` at `:720`; add `msgSigned` to the pattern and reuse it:
```haskell
... meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable, showGroupAsSender, msgSigned}
...
let reuseSig = if isJust msgSigned then SignContent else DontSignContent
SndMessage {msgId} <- sendGroupMessage user gInfo scope recipients reuseSig event
```
`msgSigned` is loaded via `mkCIMeta`/`toGroupChatItem` (`Store/Messages.hs:2412`); for own items it is `Just MSSVerified` iff signed (`createNewSndChatItem` stores only `MSSVerified <$ signedMsg_`, `:550`), so `isJust` is the right test. This makes an edit (including the recipient-deleted-restore case) signed exactly when the original was; it is automatically consistent with §5 (as-channel originals are never signed ⇒ edits stay unsigned).

Direct edit (`:697-704`) and local edit (`:745`) need no change (never signed).

### 7. Security fix: refresh `msg_signed` on in-place content update

**Finding:** `updateGroupChatItem_` (`Store/Messages.hs:2755`) updates content/status/timed fields but **not `msg_signed`** (`UPDATE` at `:2760-2767`); `updatedChatItem` (`:2749`) carries the original `meta.msgSigned`. Today invisible (content never signed); once content is signed and badged, an in-place edit from an **unsigned, relay-forged `XMsgUpdate`** would keep a stale `MSSVerified` badge over attacker content.

**Why pass it in:** the `MSSVerified` vs `MSSSignedNoKey` outcome is computed at receive by `withVerifiedMsg` and lives only on the chat item; the stored `messages` row holds signature bytes but not the verification *outcome*. So the status must come from receive-time `RcvMessage.msgSigned`, not be re-derived.

**Fix (contained to the group helper):** add a `Maybe MsgSigStatus` param to `updateGroupChatItem` (`:2746`); after `let ci' = updatedChatItem …` (`:2749`) override `ci'`'s `meta.msgSigned`, and add `msg_signed = ?` to `updateGroupChatItem_`'s `UPDATE` (`:2755`/`:2760-2767`). `updateGroupChatItem_` is called *only* from `updateGroupChatItem` (grep), so this is self-contained. **Leave `updatedChatItem` (`:2544`) unchanged** — it serves the unsigned direct/local paths (`:2540`, `:3210`).

All **five** callers pass an explicit value (no implicit "preserve"):
- `Commands.hs:738` (sender edit): `MSSVerified <$ signedMsg_` from the returned `SndMessage` (mirrors `:550`; equals the reused setting).
- `Subscriber.hs:2212` (recipient in-place edit — *the spoof path*): `msgSigned` from the handler's `RcvMessage msg`. Unsigned forged edit ⇒ `Nothing` ⇒ badge removed; verified ⇒ kept.
- `Subscriber.hs:2172` (recipient restore in-place, after `saveRcvChatItem'`): same `msgSigned` from `msg`.
- `Subscriber.hs:1152` (`mdeUpdatedCI` decryption-error marker): `Nothing` — local marker, badge correctly cleared.
- `Subscriber.hs:1509` (`upsertBusinessRequestItem` business-chat welcome): `Nothing` — never a relay channel, safely preserves `Nothing`. (Sibling direct path `:1480` uses `updateDirectChatItem'`, unaffected.)

Net: signed status is set explicitly from the source of current content in every group create/update path, so a stale badge cannot exist.

### 8. Paths deliberately left unsigned

- Auto-reply welcome content (`Subscriber.hs:1267` `XMsgUpdate`, `:1269` `XMsgNew`) via `sendGroupMessage'` ⇒ `DontSignContent`.
- `XMsgReact` (`Commands.hs:889`), `XMsgDel` (`Commands.hs:792-799`): unsigned in v1. Asymmetry: a post is verifiable, its reactions/deletes are not — and a signed post is still relay-suppressible (threat model). Later, extending `signableContent` could let recipients reject unsigned deletes of signed posts.

## App changes (iOS + Kotlin)

### A. Decode the signature status
- **JSON tags:** core uses `enumJSON (dropPrefix "MSS")` ⇒ `MSSVerified → "verified"`, `MSSSignedNoKey → "signedNoKey"` (lower-cases first letter). **Not** the DB/text strings (`"verified"`/`"no_key"`).
- iOS: `enum MsgSigStatus: String, Decodable { case verified, signedNoKey }`; add `public var msgSigned: MsgSigStatus?` to `CIMeta` (`apps/ios/SimpleXChat/ChatTypes.swift:3721-3737`).
- Kotlin: `@Serializable enum class MsgSigStatus { @SerialName("verified") Verified, @SerialName("signedNoKey") SignedNoKey }`; add `val msgSigned: MsgSigStatus? = null` to `CIMeta` (`apps/multiplatform/.../model/ChatModel.kt:3434-3450`).
- Optional field ⇒ backward-safe decode of old core JSON.

### B. Device preference (default off)
- iOS: `@AppStorage(DEFAULT_PRIVACY_SIGN_CHANNEL_MESSAGES) private var signChannelMessages = false` + toggle in `PrivacySettings.swift` (pattern: `protectScreen`, `:68-70`) with a non-repudiation footer.
- Kotlin: `val privacySignChannelMessages = mkBoolPreference(SHARED_PREFS_PRIVACY_SIGN_CHANNEL_MESSAGES, false)` (`SimpleXAPI.kt:314`; declarations near `:122-125`) + `SettingsPreferenceItem` in `PrivacySettings.kt` with explanation.
- App-side only (like `customDisappearingMessageTime`), not core `AppSettings`.

### C. Composer option (per-send override) + thread `sign` to the API
- Change the send closure to `(_ ttl: Int?, _ sign: Bool?)` (iOS `SendMessageView.swift:21`; Kotlin `SendMsgView.kt:54`), `sign == nil` ⇒ use device default; composer passes effective `sign = override ?? default`.
- Long-press item next to "Disappearing message" (iOS `SendMessageView.swift:224-247`; Kotlin `SendMsgView.kt:198-209`): "Sign message" (default off) / "Send without signing" (default on).
- **Gate visibility** on relay channel + membership has a signing key + **not as-channel** (the UI half of §5 — never offer it for as-channel publication). If app `GroupInfo` lacks relay/key state, add a derived `memberSigningAvailable` boolean to its JSON; AND it with the composer's as-channel state. Mirror `timedMessageAllowed`.
- `apiSendMessages`: add `sign: Bool`, append `sign=on|off` — iOS `ChatCommand.apiSendMessages` (`AppAPITypes.swift:48`, encode `:239`) + `SimpleXAPI.swift:545`; Kotlin `CC.ApiSendMessages` (`SimpleXAPI.kt:3676`, encode `:3867`) + `SimpleXAPI.kt:1097`.

### D. Recipient indicator
- Show a "signed by author" indicator when `meta.msgSigned == .verified` in the meta row: iOS `CIMetaView.swift` `ciMetaText` (`:93-160`); Kotlin `CIMetaView.kt` `CIMetaText` (`:67-115`) + update `reserveSpaceForMeta` (`:118-175`) for icon width.
- `signedNoKey`: show muted or nothing so it isn't read as `verified` (design). Surface the "verified ≠ timestamp/ordering/completeness" caveat (threat model) in help.
- Own signed items use the same indicator (core sets `MSSVerified` on signed sends).

## Compatibility analysis
- **Protocol wire format:** unchanged; existing batch-element signature prefix. No `chatVRange` bump; pre-feature relay-capable peers verify/accept correctly.
- **API command:** `sign=` additive with default; app+core ship together.
- **DB:** no migration. `chat_items.msg_signed` exists (added `M20260222_chat_relays`; in both schema files; written by `createNewChatItem_:603`).
- **App JSON:** new optional `msgSigned` decodes as absent on older cores.

## Edge cases, races, correctness
- **Member without keys** (`groupKeys = Nothing`): `groupMsgSigning` returns `Nothing` even with `SignContent` ⇒ silent unsigned send. UI gate should prevent offering it; document the silent degrade.
- **Non-relay groups:** `useRelays'` guard ⇒ never signed; UI must not offer it.
- **Live messages:** initial `XMsgNew` then repeated `XMsgUpdate`, each reusing the item's `msgSigned` ⇒ every increment signed. Extra cost per keystroke-batch; acceptable.
- **Separate (non-batched) path drops signatures** (`sndMessageMBR` uses raw `msgBody`, `Internal.hs:2199`, vs the batched path's `encodeBatchElement`). Never reached in relay groups (`memberSendAction` → `MSASendBatched`). Add a test-asserted invariant; optionally make `sndMessageMBR` use `encodeBatchElement signedMsg_` too, so routing changes can't silently drop channel signatures.
- **Defense-in-depth: no signature on `FwdChannel`.** `encodeFwdElement` (`Batch.hs:108`) includes `signedMsg_` unconditionally; §5 makes it `Nothing` for `FwdChannel` in normal flow. Add a guard/assertion that `encodeFwdElement` carries no signature when `fwdSender = FwdChannel`, so no future upstream path can reintroduce the de-anonymization.
- **History re-send strips signatures (badge non-determinism, by design).** Relay history catch-up rebuilds content via `prepareGroupMsg` into plain `XGrpMsgForward` events (`processContentItem`, `Internal.hs:1279-1305`) and lacks the private key ⇒ unsigned. So for the same message, a live-forward recipient sees a badge while a history-catch-up recipient does not. Graceful (absence ≠ forgery); document in UI/help and test.
- **Concurrency:** signing/verification are pure given keys; no new shared state. Send holds `withGroupLock`; receive update runs under existing receive-loop serialization. No new races.

## Tests

Protocol (`tests/ProtocolTests.hs`, extending `:112-312`):
- Round-trip signed `XMsgNew`/`XMsgUpdate` through `SignedMsg`; assert binding `CBGroup <> (publicGroupId, memberId)`; `verify` accepts the right key, rejects wrong key / altered body / altered binding.

Integration (`tests/ChatTests/`, using `setupRelay`/`prepareChannel1Relay`/`createChannel1Relay`/`memberJoinChannel`, `Groups.hs:8621-8750`):
- **Sign + verify:** `sign=on` ⇒ recipient and sender items are `(signed)` (`sigStatusStr`).
- **Off / opt-out:** `sign=off`/default ⇒ no `(signed)`.
- **No key:** missing roster key ⇒ `(signed, no key to verify)` (`MSSSignedNoKey`).
- **Edit reuse:** signed message edit stays `(signed)`; unsigned stays unsigned.
- **Edit downgrade (security):** unsigned `XMsgUpdate` for a previously-signed item (forging-relay, cf. `ChatRelays.hs:220-230`) ⇒ badge **removed** (§7).
- **As-channel never signed (anonymity):** owner posts `as_group=on sign=on` ⇒ no item is `(signed)` and no signature on the wire/stored message (guards §5).
- **History downgrade:** live-forward recipient sees `(signed)`; later history-catch-up recipient sees the same message without it (Edge cases).
- **Forgery rejection:** mismatched-binding replay/fabrication ⇒ signature stripped / `RGEMsgBadSignature`.

App: minimal decode test that `"verified"`/`"signedNoKey"` parse to the right enum on both platforms (guards the §A tag mismatch).

## Commit / diff plan

1. **Structural (behavior-preserving):** add `ContentSig`, `signableContent`, parameterize `groupMsgSigning` + the three send functions, update all callers with `DontSignContent`. Reviewable as "no behavior change".
2. **Security fix (independent, behavioral no-op today):** add `Maybe MsgSigStatus` to `updateGroupChatItem`, override `meta.msgSigned` after `updatedChatItem`, add `msg_signed` to `updateGroupChatItem_`'s `UPDATE`, update all five callers (§7). Until commit 3 every call passes `Nothing`/unchanged, so no observable change yet — but correct on its own, with a regression test that bites once signing exists.
3. **Feature behavior (core):** `APISendMessages` field + parser; content send and edit pass the real `ContentSig` (with the §5 as-channel gate); report path `DontSignContent`.
4. **App — decode + recipient indicator.**
5. **App — device preference + composer option + `apiSendMessages` wiring.**
6. **Tests** (protocol + integration) — may accompany commits 2/3.

Each commit builds and passes tests independently (bisect/rollback).

### Pre-implementation gates (after rebasing onto #7017 + #7048)
- **MUST:** the as-channel gate (`showGroupAsSender ⇒ DontSignContent`, §5) lives in the *core* send path, and the app option is hidden for as-channel sends (§C) — not UI-only.
- **MUST:** re-run `grep -rn 'updateGroupChatItem\b'` and confirm **every** caller passes an explicit `Maybe MsgSigStatus` — a missed caller silently re-introduces the §7 spoof. (Pre-rebase set: `Commands.hs:738`; `Subscriber.hs:1152,1509,2172,2212`.)
- **SHOULD:** re-run the `sendGroupMessages`/`sendGroupMessage`/`sendGroupMessages_` caller greps; only content-send and edit pass a variable `ContentSig`, all others `DontSignContent`.
- **SHOULD:** the three "verified"-meaning caveats (no timestamp/ordering; history downgrade; relay-suppressible) are surfaced in UI/help, and the history-downgrade test exists.

## Out of scope / future
- Group-level "expected/required signing" owner setting (closes the optional-downgrade gap).
- Signing reactions/deletes; signing auto-reply content; verifiable reports (signed `MCReport`).

## Open assumptions to confirm during implementation
- App `GroupInfo` exposes relay+key state for the UI gate, or a derived boolean is added to its JSON.
- Visual treatment of `signedNoKey` vs `verified`, and how to surface the "verified ≠ timestamp/ordering/completeness" caveat (threat model) in help.
