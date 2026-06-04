# Plan: optional signing of channel content messages (`XMsgNew` / `XMsgUpdate`)

## Goal / user problem

In relay-based channels, content messages are forwarded by chat relays. A relay can today forge or alter content attributed to a member, because content messages (`XMsgNew`) are not signed — only group-state events are (`requiresSignature`, `Protocol.hs:1251`). This feature lets a channel member *optionally* attach their member signature to the content they post, so recipients holding the (signed) roster can cryptographically verify authorship and integrity of that post.

Per the up-front decisions:

- **UI: both.** A device-stored default ("sign my channel messages", off by default) plus a per-send long-press override — mirroring the existing custom disappearing-message TTL.
- **Default: off, with an explicit tradeoff explanation.** Signing creates *non-repudiable, transferable* proof of authorship; this is a deliberate deniability tradeoff and must be opt-in for at-risk users.
- **Recipient indicator: in scope.** iOS and Kotlin show a "signed by author" indicator on received channel messages, because signing is meaningless to readers if invisible.
- **Event scope: `XMsgNew` + `XMsgUpdate` only.** Edits/restorations reuse the original message's signing setting. Reactions (`XMsgReact`) and deletes (`XMsgDel`) stay unsigned in v1 (documented asymmetry).

## Prerequisites / sequencing

Lands after PR #7017 (signed roster) and PR #7048 (roster delivery over inline files; `GRMember` role in roster). Neither is merged on this branch yet (`f/allow-sign-new-msg`; `git log` tops out at #7043). The dependency is real and specific: content-signature *verification* requires the recipient to hold the sender's member public key, which is distributed via the roster. Until the signed roster is reliably delivered, signed content from a member whose key the recipient lacks degrades to `MSSSignedNoKey` ("signed, no key to verify") rather than `MSSVerified`. This is graceful, but the user-facing value depends on roster delivery being solid — so this plan must merge on top of those PRs and its integration tests must use their channel/roster setup.

## What already exists (so the change stays small)

The signing machinery is fully present and is reused unchanged on the wire:

- **Send-side signing.** `groupMsgSigning :: GroupInfo -> ChatMsgEvent e -> Maybe MsgSigning` (`Internal.hs:1963`) decides whether to sign; `createSndMessages` (`Internal.hs:1950`) threads `Maybe MsgSigning`; `createNewSndMessage` (`Store/Messages.hs:234`) performs the Ed25519 signature over `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody` and stores `SignedMsg` in `SndMessage.signedMsg_` (`Messages.hs:1156`).
- **Wire encoding.** `batchMessages` reads `SndMessage.signedMsg_` and prepends the signature via `encodeBatchElement` (`Messages/Batch.hs:46,65`). In relay groups every send is batched (`memberSendAction` returns only `MSASendBatched` under `useRelays'`, `Internal.hs:2222,2228`), so signatures reliably reach the wire on the channel path.
- **Receive-side verification.** `withVerifiedMsg` (`Subscriber.hs:3469`) runs for *all* group messages (`Subscriber.hs:1004`, and forwarded at `:3431`). Because `XMsgNew_`/`XMsgUpdate_` are not in `requiresSignature`, `signatureOptional = True` (`Subscriber.hs:3491`): a signed content message is verified and tagged `MSSVerified` (key known) / `MSSSignedNoKey` (key unknown); an unsigned one is accepted as `VMUnsigned`. **No new protocol version is required.**
- **Sent-item persistence.** `createNewSndChatItem` already records `msgSigned = MSSVerified <$ signedMsg_` (`Store/Messages.hs:550`). So once an `XMsgNew` is signed, the sender's own chat item is automatically marked signed — and the edit path can read it back.
- **Received-item persistence.** `createNewRcvChatItem` records `RcvMessage.msgSigned` (`Store/Messages.hs:565,567`); `CIMeta.msgSigned :: Maybe MsgSigStatus` (`Messages.hs:520`).
- **CLI rendering.** `sigStatusStr` (`View.hs:388`) already appends `" (signed)"` / `" (signed, no key to verify)"` to both sent and received item text. So CLI/test output reflects signed content with no extra work.

The net effect: the wire format, signing, verification, DB persistence (both directions), and CLI display are already in place. What is missing is (1) the *decision* to sign content (currently `groupMsgSigning` returns `Nothing` for content), (2) the per-send plumbing of that decision from the API, (3) reuse of the setting on edit, (4) a security fix so in-place edits don't keep a stale signed badge, and (5) the apps (decode the field, the option, the recipient indicator).

## Threat model

Actors: a member (sender), recipients, and one or more **chat relays** that forward content and the roster. Relays are not trusted for content authenticity.

- **Forgery of content attributed to a member.** Without signing, a relay can inject `XMsgNew` attributed to any member. Signing closes this *for signed messages*: the relay lacks the member's Ed25519 private key, and the signature binds `(publicGroupId, memberId, body)` — so it cannot forge a valid signature, cannot cross-bind to another group/member, and cannot alter the body.
- **Downgrade / signature stripping (residual, by design).** Because content signing is *optional*, a relay can strip a member's signature and deliver the message as unsigned. The recipient then sees no badge. Absence of a badge is therefore **not** proof of forgery — only *presence* of a verified badge is a positive guarantee. This is inherent to per-message optional signing; a future group-level "expected/required signing" setting (owner-configurable) would close it. Documented as a limitation; out of scope for v1.
- **Stale-badge spoof on edits (MUST fix — see below).** A signed message that is edited with an *unsigned, relay-forged* `XMsgUpdate` must not retain its "verified" badge over attacker-controlled edited content. The current in-place update path does not refresh `msg_signed`, so this feature would introduce exactly this spoof unless fixed.
- **Non-repudiation (privacy tradeoff, by design).** A verified signature is transferable proof that a specific member authored specific content. For journalists/activists this is a deniability loss. Hence opt-in, off by default, with an in-UI explanation.
- **Replay.** The signed body contains the `sharedMsgId` and `MsgScope`, both covered by the signature; cross-scope/cross-group replay is prevented by the binding, and same-message replay is a duplicate handled by existing dedup.

## Core changes (Haskell)

### 1. Name the signable-content predicate

`Protocol.hs`, next to `requiresSignature` (`:1251`):

```haskell
-- | Content events whose authorship a member may optionally prove by signing.
signableContent :: CMEventTag e -> Bool
signableContent = \case
  XMsgNew_ -> True
  XMsgUpdate_ -> True
  _ -> False
```

### 2. Make signing decision carry the opt-in (no boolean blindness)

Add a named two-state type near `MsgSigning` (`Protocol.hs:426`), per the "name the proposition" rule — `Bool` here would be an unnamed premise threaded through ~13 sites:

```haskell
-- | Whether opt-in content signing applies to this group send.
-- Independent of mandatory state-event signing (requiresSignature),
-- which always applies in relay groups regardless of this value.
data ContentSig = SignContent | DontSignContent
  deriving (Eq, Show)
```

Extend `groupMsgSigning` (`Internal.hs:1963`) to take it as its first argument:

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

Notes that keep this safe:
- The `useRelays'` and `groupKeys = Just` guards are unchanged: in a non-relay group, or a member without signing keys, `SignContent` is a harmless no-op (returns `Nothing`). The UI must still gate the option (see apps) but core degrades silently rather than failing.
- Mandatory state-event signing is unaffected (`requiresSignature` branch preserved).

### 3. Thread `ContentSig` through the group send functions

`groupMsgSigning` is called in exactly two places: `sendGroupMessages_` (`Internal.hs:2134`) and `sendGroupMemberMessages` (`:1972`). Only `sendGroupMessages_` needs the value to be variable; `sendGroupMemberMessages` only sends state/intro events and passes `DontSignContent` internally (its own signature unchanged).

Add a `ContentSig` parameter to:
- `sendGroupMessages_` (`:2132`) — uses it in the `idsEvts` map.
- `sendGroupMessages` (`:2100`) — passes through.
- `sendGroupMessage` (`:2088`) — passes through.

Keep these signatures **unchanged** by hardcoding `DontSignContent` internally:
- `sendGroupMessage'` (`:2094`) → `sendGroupMessages_ ... DontSignContent ...` (state-event senders; no caller churn).
- `sendGroupMemberMessages` (`:1969`).

This is a structural, behavior-preserving change: every existing caller passes `DontSignContent` and produces identical output. Per the diff-discipline rule it is **its own commit**, reviewed purely for "behavior unchanged". Call sites to update with `DontSignContent` (verified by grep):
- `sendGroupMessages`: `Subscriber.hs:1370`; `Commands.hs:793,800,2778,2909`.
- `sendGroupMessage`: `Commands.hs:889,2690,3272,3812,3815,3819`.
- `sendGroupMessages_` direct: `Commands.hs:2826,3849`.

The two sites that pass a *variable* `ContentSig` are the feature behavior (next commit): the content send (`Commands.hs:4405`) and the group edit (`Commands.hs:732`).

Rationale for parameter threading over alternatives: the signing decision is genuinely per-send and must reach `groupMsgSigning`; encoding it on `gInfo` would be a hidden premise, and a parallel "signed send" function would duplicate the `sendGroupMessages_` body (forbidden by `CODE.md`). A named type localizes churn to the three public send entry points and keeps each diff hunk trivial.

### 4. API: per-send `sign` flag

Add a field to `APISendMessages` (`Controller.hs:332`):

```haskell
| APISendMessages {sendRef :: SendRef, liveMessage :: Bool, ttl :: Maybe Int, signMessages :: Bool, composedMessages :: NonEmpty ComposedMessage}
```

Parser (`Commands.hs:5006`), mirroring `liveMessageP`/`sendMessageTTLP` and defaulting off so any command without `sign=` still parses:

```haskell
"/_send " *> (APISendMessages <$> sendRefP <*> liveMessageP <*> sendMessageTTLP <*> signMessagesP <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP))
-- with:  signMessagesP = " sign=" *> onOffP <|> pure False   (place after sendMessageTTLP, before " json ")
```

Wire string becomes `/_send <ref> live=.. ttl=.. sign=on|off json ...`. The field is at the API boundary (app↔core, same bundle), so this is not a protocol-compatibility concern; the `pure False` default also keeps it forward-safe for any stored/old command string.

Place the flag at `APISendMessages` (per-send) level rather than per-`ComposedMessage`, matching `ttl`'s granularity. All messages in one send share the choice.

### 5. Content send path

`sendGroupContentMessages` (`Commands.hs:4366`) and `sendGroupContentMessages_` (`:4375`) gain a `ContentSig` parameter, passed into the `sendGroupMessages user gInfo Nothing showGroupAsSender recipients <csig> chatMsgEvents` call (`:4405`).

- `APISendMessages` handler (`:637-650`): convert the new `signMessages :: Bool` to `if signMessages then SignContent else DontSignContent` and pass it down (both `SRGroup` and `SRDirect`; for direct it is ignored since `sendContactContentMessages` doesn't sign).
- `APIReportMessage` (`:679`): passes `DontSignContent` (reports stay unsigned in v1). Documented; revisit if moderators need verifiable reporters.

### 6. Edit / restore reuse (the `XMsgUpdate` requirement)

Group edit, `Commands.hs:710-742`. The user's own sent item is loaded with its `CIMeta` at `:720`; add `msgSigned` to that record pattern and derive the reuse decision:

```haskell
... meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable, showGroupAsSender, msgSigned}
...
let reuseSig = if isJust msgSigned then SignContent else DontSignContent
SndMessage {msgId} <- sendGroupMessage user gInfo scope recipients reuseSig event
```

`msgSigned` is loaded by `getGroupCIWithReactions` (built via `mkCIMeta` in `toGroupChatItem`, `Store/Messages.hs:2412`); for the user's own item it is `Just MSSVerified` iff the original was signed, `Nothing` otherwise (including all pre-feature messages). This satisfies "the XMsgUpdate that restores a recipient-deleted message reuses the original sender setting": the sender always sends the edit signed exactly when the original was, regardless of whether the recipient still has the item.

Direct edit (`:697-704`) and local edit (`:745`) need no change (never signed).

### 7. Security fix: refresh `msg_signed` on in-place content update

**Finding (grounded):** `updateGroupChatItem_` (`Store/Messages.hs:2755`) updates `item_content, item_text, item_status, item_deleted, item_edited, item_live, updated_at, timed_ttl, timed_delete_at` — but **not `msg_signed`** (`UPDATE` at `:2760-2767`). `updatedChatItem` (called at `:2749`) carries the *original* item's `meta.msgSigned` unchanged. Today this is invisible because content is never signed; the moment this feature signs content and shows a recipient badge, an in-place edit applied from an **unsigned, relay-forged `XMsgUpdate`** would keep the original `MSSVerified` badge over attacker-controlled edited content — a spoof.

**Fix:** make the item's signed status always reflect the *latest* content's actual signature.

- Add a `Maybe MsgSigStatus` parameter to `updateGroupChatItem` (`Store/Messages.hs:2746`) and `updateGroupChatItem_`; include `msg_signed = ?` in the `UPDATE`; have `updatedChatItem` set `meta.msgSigned` to the passed value.
- Receive path `updateCI` (`Subscriber.hs:2201`, call at `:2212`): pass `msgSigned` from the update's `RcvMessage` (already in scope as `RcvMessage {msgSigned}`). An unsigned forged edit ⇒ `Nothing` ⇒ badge correctly disappears; a properly signed-and-verified edit ⇒ `Just MSSVerified` ⇒ badge kept.
- Receive "not-found / restore" path (`Subscriber.hs:2169-2172`): `saveRcvChatItem'` already sets `msgSigned` from the message; pass the same `msgSigned` to the subsequent `updateGroupChatItem` (`:2172`) so it is not clobbered.
- Send edit path (`Commands.hs:733-738`): bind `signedMsg_` from the returned `SndMessage` and pass `MSSVerified <$ signedMsg_`, mirroring `createNewSndChatItem` (`Store/Messages.hs:550`). For the sender this equals the reused setting (consistent), and it keeps the one definition of "item signed = its message carried a signature".

This is the "eliminate the class" step: signed status is derived from the message that produced the current content, in every create/update path, so a stale badge cannot exist.

### 8. Paths deliberately left unsigned (documented)

- Auto-reply welcome content (`Subscriber.hs:1267` `XMsgUpdate`, `:1269` `XMsgNew`) goes through `sendGroupMessage'` ⇒ `DontSignContent`. Automated replies are not signed in v1.
- `XMsgReact` (`Commands.hs:889`), `XMsgDel` (`Commands.hs:792-799`): unsigned in v1 (event scope decision). Note the asymmetry: a member's *post* can be verified, but their reactions/deletes cannot. Acceptable; candidates for a later extension of `signableContent`.

## App changes (iOS + Kotlin)

### A. Decode the signature status

Add `MsgSigStatus` to each app and an optional `msgSigned` field to `CIMeta`.

- **Exact JSON tags (gotcha):** core encodes `MsgSigStatus` via `enumJSON (dropPrefix "MSS")` (`Types/Shared.hs`), i.e. `MSSVerified → "verified"`, `MSSSignedNoKey → "signedNoKey"` (drop `MSS`, lower-case first letter). These are **not** the DB/text-encoding strings (`"verified"` / `"no_key"`). The app enums must match the JSON tags exactly: `"verified"` and `"signedNoKey"`.
- iOS: `enum MsgSigStatus: String, Decodable { case verified, signedNoKey }`; add `public var msgSigned: MsgSigStatus?` to `CIMeta` (`apps/ios/SimpleXChat/ChatTypes.swift:3721-3737`).
- Kotlin: `@Serializable enum class MsgSigStatus { @SerialName("verified") Verified, @SerialName("signedNoKey") SignedNoKey }`; add `val msgSigned: MsgSigStatus? = null` to `CIMeta` (`apps/multiplatform/.../model/ChatModel.kt:3434-3450`).
- Optional field with `nil`/`= null` default ⇒ backward-safe decode of old core JSON.

### B. Device preference (default), off by default

- iOS: add `@AppStorage(DEFAULT_PRIVACY_SIGN_CHANNEL_MESSAGES) private var signChannelMessages = false` and a toggle in `apps/ios/Shared/Views/UserSettings/PrivacySettings.swift` (pattern: the `protectScreen` toggle, `:68-70`). Add a footer/explanation string about non-repudiation.
- Kotlin: `val privacySignChannelMessages = mkBoolPreference(SHARED_PREFS_PRIVACY_SIGN_CHANNEL_MESSAGES, false)` (`SimpleXAPI.kt:314` pattern; declarations near `:122-125`) and a `SettingsPreferenceItem` in `PrivacySettings.kt` with explanation text.
- Both stored purely app-side (like `customDisappearingMessageTime`), not in core `AppSettings`.

### C. Composer option (per-send override) + thread `sign` to the API

- Thread `sign` alongside `ttl`: change the send closure from `(Int?) -> ...` to `(_ ttl: Int?, _ sign: Bool?) -> ...` (iOS `SendMessageView.swift:21`; Kotlin `SendMsgView.kt:54`), where `sign == nil` means "use device default". The composer computes the effective `sign = override ?? signChannelMessagesDefault` and passes a concrete `Bool` to `apiSendMessages`.
- Add a long-press/context-menu item next to "Disappearing message" (iOS `SendMessageView.swift:224-247`; Kotlin `SendMsgView.kt:198-209`): "Sign message" when the default is off, or "Send without signing" when the default is on (symmetric override).
- **Gate visibility** to channels where signing is meaningful and possible: relay channel + the user's membership has a signing key — the same predicate core uses (`useRelays'` + `groupKeys`). If app `GroupInfo` does not already expose this, add a small derived boolean to the `GroupInfo` JSON (e.g. `memberSigningAvailable = useRelays && membership has member key`) so the apps can gate without re-deriving relay/key state. Mirror how `timedMessageAllowed` gates the disappearing option.
- `apiSendMessages`: add `sign: Bool` and append `sign=on|off` to the `/_send` string — iOS `ChatCommand.apiSendMessages` (`AppAPITypes.swift:48`, encode `:239`) and `SimpleXAPI.swift:545`; Kotlin `CC.ApiSendMessages` (`SimpleXAPI.kt:3676`, encode `:3867`) and `SimpleXAPI.kt:1097`.

### D. Recipient indicator

- Render a "signed by author" indicator when `meta.msgSigned == .verified` in the message meta row: iOS `CIMetaView.swift` `ciMetaText` (`:93-160`, alongside the timer/edited marks); Kotlin `CIMetaView.kt` `CIMetaText` (`:67-115`) and update `reserveSpaceForMeta` (`:118-175`) to account for the icon width.
- Decide treatment of `signedNoKey`: show a distinct/dimmed indicator or none, so users don't mistake "signed but unverifiable" for "verified". Recommend: show the indicator only for `verified`; optionally a muted variant for `signedNoKey`. (Confirm visual with design.)
- Do **not** show the indicator for the user's own sent items differently than core does; the core already sets `msgSigned = MSSVerified` on signed sends, so "you signed this" can use the same indicator.

## Compatibility analysis

- **Protocol wire format:** unchanged. Signed content uses the existing batch-element signature prefix already understood by relay-capable clients. No `chatVRange` bump. A relay-capable peer that predates this feature still runs `withVerifiedMsg`, accepts optional content signatures, and sets `msgSigned` correctly — receiving "just works".
- **API command:** `sign=` is additive with a default; app and core ship together.
- **DB:** no migration. `chat_items.msg_signed` already exists (added in `M20260222_chat_relays`, present in both `SQLite/.../chat_schema.sql` and `Postgres/.../chat_schema.sql`; written by `createNewChatItem_:603`).
- **App JSON:** new optional `msgSigned` field decodes as absent on older cores.

## Edge cases, races, correctness

- **Member without signing keys** (`groupKeys = Nothing`): `groupMsgSigning` returns `Nothing` even with `SignContent`; the message goes unsigned. The UI gate (memberSigningAvailable) should prevent offering the option here; if it slips through, behavior is a silent unsigned send. Consider surfacing "couldn't sign" only if product wants it — otherwise document the silent degrade.
- **Non-relay groups:** `useRelays'` guard ⇒ never signed; the UI must not show the option outside relay channels.
- **Live messages:** initial `XMsgNew` (signed iff requested) then repeated `XMsgUpdate`; each update reuses the item's `msgSigned` ⇒ every increment is signed. Extra signing cost per keystroke-batch; acceptable, noted.
- **Separate (non-batched) send path drops signatures** (`sndMessageMBR` uses raw `msgBody`, `Internal.hs:2199`, vs the batched path's `encodeBatchElement`). This path is never taken in relay groups (`memberSendAction` only yields `MSASendBatched` under `useRelays'`). Add a test-asserted invariant so a future change to member-send routing cannot silently ship unsigned channel content; consider making `sndMessageMBR` also use `encodeBatchElement signedMsg_` defensively (small, isolated, keeps signing robust regardless of routing).
- **Edit downgrade:** covered by §7 — refreshing `msg_signed` from the update message.
- **Reaction/delete asymmetry:** documented limitation.
- **Concurrency:** signing/verification are pure given the keys; no new shared mutable state. The send path already holds the group lock (`withGroupLock`), and the receive update path runs under the receive-loop serialization that other `msgSigned` consumers rely on. No new races introduced.

## Tests

Protocol (`tests/ProtocolTests.hs`, extending `:112-312`):
- Round-trip a signed `XMsgNew` and `XMsgUpdate` through `SignedMsg` encode/decode; assert binding bytes `CBGroup <> (publicGroupId, memberId)` and that `verify` accepts the correct key and rejects a wrong key / altered body / altered binding.

Integration (`tests/ChatTests/`, using `setupRelay` / `prepareChannel1Relay` / `createChannel1Relay` / `memberJoinChannel`, `Groups.hs:8621-8750`):
- **Sign + verify:** member sends with `sign=on`; another member receives and the item is `(signed)` (`sigStatusStr`, `View.hs:388`); sender's own item is `(signed)`.
- **Off by default / opt-out:** `sign=off` (or default) ⇒ received item has no `(signed)` suffix.
- **No key:** recipient lacking the sender's roster key ⇒ `(signed, no key to verify)` (`MSSSignedNoKey`).
- **Edit reuse:** edit a signed message ⇒ the `XMsgUpdate` is signed, item stays `(signed)`; edit an unsigned message ⇒ stays unsigned.
- **Edit downgrade (security):** simulate an unsigned `XMsgUpdate` for a previously-signed item (forging-relay scenario in the spirit of `ChatRelays.hs:220-230`) ⇒ the badge is **removed** (`msg_signed` refreshed to `Nothing`), not retained.
- **Forgery rejection:** a relay/member replaying or fabricating a signed message with a mismatched binding ⇒ signature stripped/`RGEMsgBadSignature` per existing `withVerifiedMsg` behavior.

App: a minimal decode test that `msgSigned: "verified"`/`"signedNoKey"` parses to the right enum on both platforms (guards the JSON-tag gotcha in §A).

## Commit / diff plan

1. **Structural (behavior-preserving):** add `ContentSig`, `signableContent`, parameterize `groupMsgSigning` + the three send functions, update all callers with `DontSignContent`. Reviewable as "no behavior change". (Principle: separate structure from behavior.)
2. **Security fix (independent value):** thread `Maybe MsgSigStatus` through `updateGroupChatItem`/`updateGroupChatItem_` + receive/send edit sites; add `msg_signed` to the `UPDATE`. Has its own regression test. Lands even though content isn't signed yet (it is correct on its own).
3. **Feature behavior (core):** `APISendMessages` field + parser; content send and edit pass the real `ContentSig`; report path explicit `DontSignContent`.
4. **App — decode + recipient indicator.**
5. **App — device preference + composer option + `apiSendMessages` wiring.**
6. **Tests** (protocol + integration) — may accompany commits 2/3.

Each commit builds and passes tests independently (enables bisect/rollback).

## Out of scope / future

- Group-level "expected/required signing" owner setting (would close the optional-downgrade gap).
- Signing reactions/deletes.
- Signing automated welcome/auto-reply content.
- Verifiable reports (signed `MCReport`).

## Open assumptions to confirm during implementation

- App `GroupInfo` either already exposes relay+key state for the UI gate, or a small derived boolean is added to its JSON.
- Exact JSON tag for `MSSSignedNoKey` is `"signedNoKey"` (verify by encoding once; the app decode test guards it).
- Visual treatment of `signedNoKey` vs `verified` (design).

---

## Adversarial self-review log

Method: re-read the plan as a hostile reviewer against the codebase facts and the 12 principles; fix; repeat until two consecutive passes find nothing.

**Pass 1 — found and fixed:**
- *Missing security issue.* Initial draft relied on `createNewSndChatItem`/`createNewRcvChatItem` for signed status and assumed edits were fine. Grounding `updateGroupChatItem_` (`Store/Messages.hs:2760-2767`) showed `msg_signed` is **not** updated in place ⇒ stale-badge spoof on forged unsigned edits. Added §7 and commit 2 and the downgrade test. (Principles 10/11.)
- *Boolean blindness.* Draft threaded a bare `Bool signContent`. Replaced with named `ContentSig` (`SignContent`/`DontSignContent`). (Principle 02.)
- *Wrong app enum tags.* Draft assumed `"verified"`/`"no_key"` (the text/DB encoding). Verified core JSON uses `enumJSON (dropPrefix "MSS")` ⇒ `"verified"`/`"signedNoKey"`. Corrected §A and added an app decode test. (Principles 02/04 — value reconstructed differently downstream.)

**Pass 2 — found and fixed:**
- *Unsigned separate-send path.* Noted `sndMessageMBR` uses raw `msgBody` (no signature) while the batched path includes it; confirmed relay groups always batch, so safe today, but added a defensive invariant/test and an optional hardening so future routing changes can't silently drop channel signatures. (Principles 09/11.)
- *Diff discipline.* Made the structural threading (commit 1) explicitly behavior-preserving and separate from the security fix (commit 2) and feature (commit 3), each independently buildable/testable. (Principle 06.)
- *Restore sub-path clobber.* Verified the receive "not-found/restore" branch (`Subscriber.hs:2169-2172`) sets `msgSigned` via `saveRcvChatItem'` then immediately `updateGroupChatItem`; specified passing the same `msgSigned` so the new parameter doesn't clobber it.

**Pass 3 — clean.** Re-read against principles 01–12 and call-graph facts; no new issues. All cited sites re-checked against the files.

**Pass 4 — clean.** Final read for stray claims and unverified file:line; none found. Two consecutive clean passes reached.
