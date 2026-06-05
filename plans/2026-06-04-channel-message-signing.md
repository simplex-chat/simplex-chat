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

**Line numbers are pre-rebase.** Every `file:line` in this plan is grounded against #7043. #7017 (≈ +108 `Internal.hs`, +249 `Subscriber.hs`, +55 `Commands.hs`, +47 `Protocol.hs`) and #7048 (≈ +74 `Internal.hs`, +179 `Subscriber.hs`, +39 `Protocol.hs`) shift them. The **symbol-level** claims (function names, call sites, the five-caller set, parser structure) were verified to survive the rebase, but **re-locate by symbol, not by line number**, when implementing. Concretely, the dependency PRs do not add a 6th `updateGroupChatItem` caller, but other queued branches exist (`f/channel-comments`, `f/public-groups-members-in-roster`) — so make the caller re-verification below an explicit gate.

## What already exists (so the change stays small)

The signing machinery is fully present and is reused unchanged on the wire:

- **Send-side signing.** `groupMsgSigning :: GroupInfo -> ChatMsgEvent e -> Maybe MsgSigning` (`Internal.hs:1963`) decides whether to sign; `createSndMessages` (`Internal.hs:1950`) threads `Maybe MsgSigning`; `createNewSndMessage` (`Store/Messages.hs:234`) performs the Ed25519 signature over `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody` and stores `SignedMsg` in `SndMessage.signedMsg_` (`Messages.hs:1156`).
- **Wire encoding.** `batchMessages` reads `SndMessage.signedMsg_` and prepends the signature via `encodeBatchElement` (`Messages/Batch.hs:46,65`). In relay groups every send is batched (`memberSendAction` returns only `MSASendBatched` under `useRelays'`, `Internal.hs:2222,2228`), so signatures reliably reach the wire on the channel path.
- **Receive-side verification.** `withVerifiedMsg` (`Subscriber.hs:3469`) runs for *all* group messages (`Subscriber.hs:1004`, and forwarded at `:3431`). Because `XMsgNew_`/`XMsgUpdate_` are not in `requiresSignature`, `signatureOptional = True` (`Subscriber.hs:3491`): a signed content message is verified and tagged `MSSVerified` (key known) / `MSSSignedNoKey` (key unknown); an unsigned one is accepted as `VMUnsigned`. **No new protocol version is required.**
- **Sent-item persistence.** `createNewSndChatItem` already records `msgSigned = MSSVerified <$ signedMsg_` (`Store/Messages.hs:550`). So once an `XMsgNew` is signed, the sender's own chat item is automatically marked signed — and the edit path can read it back.
- **Received-item persistence.** `createNewRcvChatItem` records `RcvMessage.msgSigned` (`Store/Messages.hs:565,567`); `CIMeta.msgSigned :: Maybe MsgSigStatus` (`Messages.hs:520`).
- **CLI rendering.** `sigStatusStr` (`View.hs:388`) already appends `" (signed)"` / `" (signed, no key to verify)"` to both sent and received item text. So CLI/test output reflects signed content with no extra work.

The net effect: the wire format, signing, verification, DB persistence (both directions), and CLI display are already in place. What is missing is (1) the *decision* to sign content (currently `groupMsgSigning` returns `Nothing` for content), (2) the per-send plumbing of that decision from the API, (3) reuse of the setting on edit, (4) a security fix so in-place edits don't keep a stale signed badge, (5) a structural gate so "publish as channel" posts are never signed (anonymity — HIGH blocker, §5), and (6) the apps (decode the field, the option, the recipient indicator).

## Threat model

Actors: a member (sender), recipients, and one or more **chat relays** that forward content and the roster. Relays are not trusted for content authenticity.

- **Forgery of content attributed to a member.** Without signing, a relay can inject `XMsgNew` attributed to any member. Signing closes this *for signed messages*: the relay lacks the member's Ed25519 private key, and the signature binds `(publicGroupId, memberId, body)` — so it cannot forge a valid signature, cannot cross-bind to another group/member, and cannot alter the body.
- **Downgrade / signature stripping (residual, by design).** Because content signing is *optional*, a relay can strip a member's signature and deliver the message as unsigned. The recipient then sees no badge. Absence of a badge is therefore **not** proof of forgery — only *presence* of a verified badge is a positive guarantee. This is inherent to per-message optional signing; a future group-level "expected/required signing" setting (owner-configurable) would close it. Documented as a limitation; out of scope for v1.
- **Stale-badge spoof on edits (MUST fix — see below).** A signed message that is edited with an *unsigned, relay-forged* `XMsgUpdate` must not retain its "verified" badge over attacker-controlled edited content. The current in-place update path does not refresh `msg_signed`, so this feature would introduce exactly this spoof unless fixed.
- **Publish-as-channel de-anonymization (MUST be structurally prevented — see §5).** Channels support "publish as the channel" (`showGroupAsSender` / `asGroup`): an owner posts and subscribers see the message as *from the channel*, not from the specific owner. This is Design Objective 6 in `docs/protocol/channels-overview.md:214` ("Sender anonymity within multi-owner channels — owners can publish as the channel, hiding which specific owner authored a message"), and today a relay revealing which owner authored an as-channel post is only a *deniable* leak ("Detectable out-of-band", `channels-overview.md:~237`). Signing breaks this: `groupMsgSigning` (`Internal.hs:1963-1967`) is blind to `showGroupAsSender` and would sign the `XMsgNew` with binding `(publicGroupId, ownerMemberId)`; the signature is broadcast on the wire even for `FwdChannel` (`encodeFwdElement` → `encodeBatchElement signedMsg_`, `Batch.hs:108`), and a malicious relay (which chooses `fwdSender = FwdMember ownerId`, `processContentItem:1302`) lets every subscriber verify it as `MSSVerified`. The deniable leak becomes **undeniable, non-repudiable cryptographic proof** of authorship. The device-default toggle makes this worse: a user enabling "sign my channel messages" for authenticity on comments would silently sign their anonymous owner broadcasts. For an anonymity property this must be structurally impossible, not behaviorally avoided. **Resolution:** signing is *never* applied to as-channel content (`showGroupAsSender ⇒ DontSignContent`, §5), the app option is hidden for as-channel sends (§C), and a defense-in-depth assertion ensures `encodeFwdElement` carries no signature when `fwdSender = FwdChannel` (Edge cases). This resolves the publish-as-channel-vs-authenticity conflict in favor of anonymity.
- **Non-repudiation (privacy tradeoff, by design).** A verified signature is transferable proof that a specific member authored specific content. For journalists/activists this is a deniability loss. Hence opt-in, off by default, with an in-UI explanation. (For *as-channel* posts the loss would be unacceptable, not merely a tradeoff — hence the structural exclusion above.)
- **What "verified" means — and does not.** The signed input is `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, and `msgBody` embeds `sharedMsgId`, `MsgScope`, and content (`Store/Messages.hs:242`). So a verified badge proves **authorship + content integrity + group/member/scope/message binding** — and *nothing else*. It does **not** cover `fwdBrokerTs` (relay-controlled, `Protocol.hs:382-387`), message ordering, or completeness. Document this in UI/help so "verified" is not over-read as "this is when/in-what-order it was sent".
- **Signed content is still relay-suppressible.** `XMsgDel_` is not in `requiresSignature` (`Protocol.hs:1252-1262`), so an unsigned, relay-forged owner-attributed delete is accepted (channel delete check is role-based against the relay-chosen author, `Subscriber.hs:~2269`). A malicious relay can therefore suppress a signed post for its subscribers. Pre-existing and within the relay's acknowledged drop/target power — not introduced here — but it bounds the integrity value of signing: signing proves *what was said*, not that *everything said is delivered*.
- **Replay.** The signed body contains the `sharedMsgId` and `MsgScope`, both covered by the signature; cross-scope/cross-group replay is prevented by the binding, and same-message replay is a duplicate handled by existing dedup.
- **Bad-signature spam (fail-closed, pre-existing).** A signature that fails verification causes the content to be **dropped** with an `RGEMsgBadSignature` item per occurrence (`Subscriber.hs:3473-3475,3483`), not delivered unsigned. A tampering/replaying relay can spam these items. This is the existing fail-closed behavior for state events; extending signing to content inherits it. Acceptable; noted.

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

`sendGroupContentMessages` (`Commands.hs:4366`) and `sendGroupContentMessages_` (`:4375`) gain a `ContentSig` parameter. `showGroupAsSender` is already in scope at the send site (`:4405`); **as-channel posts are never signed** (HIGH blocker resolution — see threat model):

```haskell
let csig' = if showGroupAsSender then DontSignContent else csig
(msgs_, gsr) <- sendGroupMessages user gInfo Nothing showGroupAsSender recipients csig' chatMsgEvents
```

This is the load-bearing anonymity gate: it must live here (structural), not only in the UI. It also keeps the sender's *own* item unsigned for as-channel posts (no misleading own-badge) and keeps §6 edit-reuse consistent (an as-channel original is never signed ⇒ its edits stay unsigned).

- `APISendMessages` handler (`:637-650`): convert the new `signMessages :: Bool` to `if signMessages then SignContent else DontSignContent` and pass it down (both `SRGroup` and `SRDirect`; for direct it is ignored since `sendContactContentMessages` doesn't sign). The `showGroupAsSender` gate above then overrides it to `DontSignContent` for as-channel sends regardless of the API flag.
- `APIReportMessage` (`:679`): passes `DontSignContent` (reports stay unsigned in v1). Documented; revisit if moderators need verifiable reporters.

### 6. Edit / restore reuse (the `XMsgUpdate` requirement)

Group edit, `Commands.hs:710-742`. The user's own sent item is loaded with its `CIMeta` at `:720`; add `msgSigned` to that record pattern and derive the reuse decision:

```haskell
... meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable, showGroupAsSender, msgSigned}
...
let reuseSig = if isJust msgSigned then SignContent else DontSignContent
SndMessage {msgId} <- sendGroupMessage user gInfo scope recipients reuseSig event
```

`msgSigned` is loaded by `getGroupCIWithReactions` (built via `mkCIMeta` in `toGroupChatItem`, `Store/Messages.hs:2412`); for the user's own item it is `Just MSSVerified` iff the original was signed, `Nothing` otherwise (including all pre-feature messages). `isJust msgSigned` is the correct test because `createNewSndChatItem` stores only `MSSVerified <$ signedMsg_` for sent items (`Store/Messages.hs:550`) — never `MSSSignedNoKey`. This satisfies "the XMsgUpdate that restores a recipient-deleted message reuses the original sender setting": the sender always sends the edit signed exactly when the original was, regardless of whether the recipient still has the item. It is also automatically consistent with the §5 anonymity gate — an as-channel original is never signed, so `msgSigned = Nothing` and its edits stay unsigned.

Direct edit (`:697-704`) and local edit (`:745`) need no change (never signed).

### 7. Security fix: refresh `msg_signed` on in-place content update

**Finding (grounded):** `updateGroupChatItem_` (`Store/Messages.hs:2755`) updates `item_content, item_text, item_status, item_deleted, item_edited, item_live, updated_at, timed_ttl, timed_delete_at` — but **not `msg_signed`** (`UPDATE` at `:2760-2767`). `updatedChatItem` (called at `:2749`) carries the *original* item's `meta.msgSigned` unchanged. Today this is invisible because content is never signed; the moment this feature signs content and shows a recipient badge, an in-place edit applied from an **unsigned, relay-forged `XMsgUpdate`** would keep the original `MSSVerified` badge over attacker-controlled edited content — a spoof.

**Why the value cannot be re-derived locally:** the verified-vs-no-key distinction (`MSSVerified` vs `MSSSignedNoKey`) is computed at receive time by `withVerifiedMsg` and lives only on the chat item; the stored `messages` row holds the raw signature bytes but not the verification *outcome*. So the new status must be passed in from the receive-time `RcvMessage.msgSigned`, not recomputed from storage.

**Fix (contained to the group update helper):** make `updateGroupChatItem`'s result and DB row reflect the signature status of whatever produced its current content.

- Add a `Maybe MsgSigStatus` parameter to `updateGroupChatItem` (`Store/Messages.hs:2746`). After `let ci' = updatedChatItem …` (`:2749`), override `ci'`'s `meta.msgSigned` with the passed value, and add `msg_signed = ?` to the `UPDATE` in `updateGroupChatItem_` (`:2755`, statement at `:2760-2767`). `updateGroupChatItem_` is called *only* from `updateGroupChatItem` (confirmed by grep), so adding the column is safe and self-contained. **Leave `updatedChatItem` (`:2544`) unchanged** — it is shared with `updateDirectChatItem'` (`:2540`) and the path at `:3210`, neither of which is signed; threading the value through it would touch unrelated paths.

All **five** callers of `updateGroupChatItem` (verified by grep) pass an explicit value — no implicit "preserve" magic, so a future caller must consciously choose:
- `Commands.hs:738` (sender edit, channel): `MSSVerified <$ signedMsg_` from the returned `SndMessage`, mirroring `createNewSndChatItem` (`:550`). Equals the reused setting (consistent).
- `Subscriber.hs:2212` (recipient in-place edit — *the spoof path*): the update's verified status (`msgSigned` from the handler's `RcvMessage msg`). Unsigned forged edit ⇒ `Nothing` ⇒ badge removed; verified edit ⇒ `Just MSSVerified` ⇒ kept.
- `Subscriber.hs:2172` (recipient restore in-place, after `saveRcvChatItem'` already set it): pass the same `msgSigned` from `msg` (consistent).
- `Subscriber.hs:1152` (`mdeUpdatedCI` — decryption-error marker): `Nothing`. The content is now a local error marker, not signed content, so any badge is correctly cleared.
- `Subscriber.hs:1509` (`upsertBusinessRequestItem` — business-chat welcome message): `Nothing`. Business chats are not relay channels and are never signed; this preserves `Nothing` safely. (Its sibling direct path at `:1480` uses `updateDirectChatItem'`, unaffected.)

This is the "eliminate the class" step: signed status is set explicitly from the source of the current content in every group create/update path, so a stale badge cannot exist regardless of which caller runs.

### 8. Paths deliberately left unsigned (documented)

- Auto-reply welcome content (`Subscriber.hs:1267` `XMsgUpdate`, `:1269` `XMsgNew`) goes through `sendGroupMessage'` ⇒ `DontSignContent`. Automated replies are not signed in v1.
- `XMsgReact` (`Commands.hs:889`), `XMsgDel` (`Commands.hs:792-799`): unsigned in v1 (event scope decision). Note the asymmetry: a member's *post* can be verified, but their reactions/deletes cannot. Beyond "can't verify deletes", this means a **signed post is still relay-suppressible** via a forged unsigned owner-attributed `XMsgDel` (see threat model). Acceptable for v1 and within the relay's acknowledged power; candidates for a later extension of `signableContent` (which would also let recipients reject unsigned deletes of signed posts).

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
- **Gate visibility** to channels where signing is meaningful, possible, **and anonymity-safe**: relay channel + the user's membership has a signing key + **this send is not "publish as channel"**. The first two are the predicate core uses (`useRelays'` + `groupKeys`); the third (`not showGroupAsSender` for the current send) is the UI half of the HIGH-blocker resolution — the option must never be offered for as-channel publication, complementing the structural core gate in §5. If app `GroupInfo` does not already expose the relay/key state, add a small derived boolean to the `GroupInfo` JSON (e.g. `memberSigningAvailable = useRelays && membership has member key`); AND it with the composer's current as-channel state. Mirror how `timedMessageAllowed` gates the disappearing option.
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
- **As-channel posts are never signed** (`showGroupAsSender ⇒ DontSignContent`, §5) — the structural anonymity gate. The composer must also hide the option for as-channel sends (§C).
- **Defense-in-depth: no signature on `FwdChannel`.** `encodeFwdElement` (`Batch.hs:108`) currently includes `signedMsg_` unconditionally. Because §5 ensures as-channel content is never signed, `signedMsg_` is already `Nothing` for `FwdChannel` in normal flow. Add an assertion/guard that `encodeFwdElement` carries no signature when `fwdSender = FwdChannel`, so a future code path cannot reintroduce the de-anonymization by signing an as-channel message upstream. Cheap, isolated, and turns the anonymity invariant into something the wire layer also enforces.
- **History re-send strips signatures (badge non-determinism, by design).** A relay catching a member up via history rebuilds content from stored items through `prepareGroupMsg` and forwards it **unsigned** — `processContentItem` (`Internal.hs:1279-1305`) produces plain `XGrpMsgForward` events and the relay lacks the member's private key. So for the *same* message, a recipient who got the live forward sees a verified badge while one who got it via history catch-up sees none. This is graceful (absence ≠ forgery, consistent with the optional-signing model) but means badge presence is **not deterministic across recipients**. Document in UI/help; add an integration test (below).
- **Edit downgrade:** covered by §7 — refreshing `msg_signed` from the update message.
- **Reaction/delete asymmetry:** documented limitation (and signed posts remain relay-suppressible, §8 / threat model).
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
- **As-channel never signed (security/anonymity):** an owner posts with `as_group=on` and `sign=on` ⇒ neither the owner's own item nor any subscriber's item is `(signed)`; assert no signature is present on the wire/stored message. This guards the HIGH-blocker resolution.
- **History downgrade (non-determinism):** a member who receives a signed post via live forward sees `(signed)`; a member who joins later and catches up via relay history (`processContentItem`) sees the same message **without** `(signed)`. Documents and locks in the expected, graceful divergence.
- **Forgery rejection:** a relay/member replaying or fabricating a signed message with a mismatched binding ⇒ signature stripped/`RGEMsgBadSignature` per existing `withVerifiedMsg` behavior.

App: a minimal decode test that `msgSigned: "verified"`/`"signedNoKey"` parses to the right enum on both platforms (guards the JSON-tag gotcha in §A).

## Commit / diff plan

1. **Structural (behavior-preserving):** add `ContentSig`, `signableContent`, parameterize `groupMsgSigning` + the three send functions, update all callers with `DontSignContent`. Reviewable as "no behavior change". (Principle: separate structure from behavior.)
2. **Security fix (independent, currently a behavioral no-op):** add `Maybe MsgSigStatus` to `updateGroupChatItem`, override `meta.msgSigned` after `updatedChatItem`, add `msg_signed` to `updateGroupChatItem_`'s `UPDATE`, and update all five callers (§7). Because received content is never signed until commit 3, every call passes `Nothing`/unchanged today, so this commit changes no observable behavior yet — but it is correct on its own and has its own regression test that bites once signing exists.
3. **Feature behavior (core):** `APISendMessages` field + parser; content send and edit pass the real `ContentSig`; report path explicit `DontSignContent`.
4. **App — decode + recipient indicator.**
5. **App — device preference + composer option + `apiSendMessages` wiring.**
6. **Tests** (protocol + integration) — may accompany commits 2/3.

Each commit builds and passes tests independently (enables bisect/rollback).

### Pre-implementation gates (after rebasing onto #7017 + #7048)

- **MUST:** the as-channel signing gate (`showGroupAsSender ⇒ DontSignContent`, §5) is in the *core* send path, and the app sign option is hidden for as-channel sends (§C). This is the HIGH-blocker resolution and must not be UI-only.
- **MUST:** re-run `grep -rn 'updateGroupChatItem\b'` on the rebased branch and confirm **every** caller passes an explicit `Maybe MsgSigStatus`. A missed caller silently re-introduces the §7 stale-badge spoof. (Pre-rebase set: `Commands.hs:738`; `Subscriber.hs:1152,1509,2172,2212`.)
- **SHOULD:** re-run the `sendGroupMessages`/`sendGroupMessage`/`sendGroupMessages_` caller greps; confirm only the content-send and edit sites pass a variable `ContentSig`, all others `DontSignContent`.
- **SHOULD:** the three "verified"-meaning caveats (no timestamp/ordering; history downgrade; relay-suppressible) are surfaced in UI/help, and the history-downgrade integration test exists.

## Out of scope / future

- Group-level "expected/required signing" owner setting (would close the optional-downgrade gap).
- Signing reactions/deletes.
- Signing automated welcome/auto-reply content.
- Verifiable reports (signed `MCReport`).

## Open assumptions to confirm during implementation

- App `GroupInfo` either already exposes relay+key state for the UI gate, or a small derived boolean is added to its JSON.
- Visual treatment of `signedNoKey` vs `verified` (design), and how to surface the "verified ≠ timestamp/ordering/completeness" caveat in help.

(The JSON tags `"verified"` / `"signedNoKey"` are confirmed from `enumJSON $ dropPrefix "MSS"` in `Parsers.hs:97-107`, not just assumed; the app decode test still guards against regression.)

---

## Adversarial self-review log

Honest account of process. The findings below in "during analysis" were made genuinely while building the plan; the "post-completion" pass was performed after the plan was first written (and after the author was challenged on whether the review was real). Earlier drafts of this log overstated the rigor by presenting analysis-time findings as a tidy multi-pass loop and asserting "two clean passes" without performing distinct post-completion re-reads. This version reflects what actually happened.

**Found during analysis (incorporated before first completion):**
- *Stale-badge spoof.* Grounding `updateGroupChatItem_` (`Store/Messages.hs:2760-2767`) showed `msg_signed` is not updated in place ⇒ once content is signed and a badge shown, a forged unsigned `XMsgUpdate` would keep a stale "verified" badge. Drove §7. (Principles 10/11.)
- *Boolean blindness.* Replaced a bare `Bool` with the named `ContentSig`. (Principle 02.)
- *Wrong app enum tags.* Verified core JSON uses `enumJSON (dropPrefix "MSS")` ⇒ `"verified"`/`"signedNoKey"` (not the DB strings `"verified"`/`"no_key"`). Corrected §A; added an app decode test. (Principles 02/04.)
- *Unsigned separate-send path.* `sndMessageMBR` (`Internal.hs:2199`) omits the signature; confirmed relay groups always batch, so safe today; added a guarded invariant + optional hardening. (Principles 09/11.)

**Post-completion pass 1 — found and fixed a real defect:**
- *Incomplete caller enumeration (the §7 fix was under-specified).* I re-ran the call-graph for the signatures I proposed to change. The send-function lists (commit 1) were complete. But `updateGroupChatItem` has **five** callers, and §7 had only accounted for three; I had missed `Subscriber.hs:1152` (`mdeUpdatedCI` decryption-error marker) and `Subscriber.hs:1509` (`upsertBusinessRequestItem` business-chat welcome). Adding a parameter breaks both, and a wrong value at either re-introduces a stale/forged badge. Read and classified both, rewrote §7 to enumerate all five with explicit values, kept the shared `updatedChatItem` untouched (it serves direct/local paths at `:2540`/`:3210`), and recorded that the verified-vs-no-key status must come from receive-time `RcvMessage.msgSigned` (not recoverable from the stored message row). Tightened commit 2 to note it is a behavioral no-op until commit 3.

**Post-completion pass 2 — clean.** Re-verified: all five `updateGroupChatItem` callers classified and value-correct; `RcvMessage.msgSigned` is in scope at the receive sites; `SndMessage.signedMsg_` is bindable at the sender edit site; commit 2 changes no behavior pre-signing; `msg_signed` column exists (no migration); send-function caller lists match grep exactly. No new issues.

**Post-completion pass 3 — clean.** Final read for unverified file:line and stray "sounds-true" claims; none found. Two consecutive clean post-completion passes reached — *within the limit of the author's own seeing* (principle 08), which the next pass exposed.

**Pass 4 — external review integration (found a HIGH blocker the author's own passes missed).** An independent review (`scratch/channel-message-signing-review.md`) verified every central claim against the code and the two dependency PRs and confirmed the bulk sound (`ContentSig`, structural/behavioral commit split, §7 storage-boundary fix and its exact 5-caller set, compatibility, test matrix, JSON tags). It found one **HIGH** issue I had missed entirely: signing is blind to `showGroupAsSender`, so signing an "publish as channel" post would attach non-repudiable proof of *which owner* authored an intentionally anonymous broadcast — converting a today-deniable relay leak (channels-overview Objective 6 / line ~237) into undeniable de-anonymization, and the device-default toggle would trigger it silently. I verified the mechanism in code (`groupMsgSigning:1963-67` blind to `showGroupAsSender`; `encodeFwdElement:108` emits the signature even for `FwdChannel`; relay controls `fwdSender`, `processContentItem:1302`) and resolved it structurally: as-channel content is never signed (§5 core gate), the app option is hidden for as-channel sends (§C), and a defense-in-depth `FwdChannel`-no-signature assertion (Edge cases). Threat model updated to frame this as the publish-as-channel-vs-authenticity resolution. Also integrated the review's MEDIUM caveats — history re-send strips signatures (badge non-determinism), "verified" covers authorship/integrity only (not timestamp/ordering/completeness), signed posts remain relay-suppressible — each grounded in code (`processContentItem:1279-1305`; `Protocol.hs:382-387`; `requiresSignature` lacks `XMsgDel_`) and given documentation + tests — and the sequencing notes (pre-rebase line numbers; mandatory post-rebase re-verification of the `updateGroupChatItem` caller set). Honest takeaway: my own passes converged on a local optimum and missed a domain-level anonymity invariant that required channel-design knowledge to see — concrete evidence for "asking for help is the highest-value review action" (principles 08/10).
