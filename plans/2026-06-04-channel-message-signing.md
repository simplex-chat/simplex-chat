# Plan: optional signing of channel content messages (`XMsgNew` / `XMsgUpdate`)

Anchored on branch `f/msg-signing` (master merged in); all symbols re-verified 2026-06-25 — see **§Verification status** at the end for the current line map and the few mislabeled anchors. Re-confirm by symbol before editing; line numbers are advisory. The public-groups roster already provides everything this builds on: `GroupKeys {publicGroupId, memberPrivKey}` (`Types.hs:465`), `verifyGroupSig` (`Subscriber.hs:116`), per-member public-key distribution via the signed roster, and optional signing of group-state events (`requiresSignature`, `Protocol.hs:1334`). Content messages are *not* signed today.

**PR 1** (this plan): a member can optionally sign their own channel posts and edits so recipients holding the signed roster can verify authorship + integrity; and — once an item is held signed — its edits and deletes are **enforced** to be signed (an unsigned mutation of a signed item is rejected at receive), closing the edit/delete-downgrade spoof at the source. The remaining hard part, history signature preservation (so catch-up members also hold posts signed), is **PR 2** (at the end).

## Goal / user problem

In relay channels, content (`XMsgNew`) is forwarded by relays and is not signed — only group-state events are. A relay can therefore forge or alter content attributed to a member. This feature lets a member optionally attach their member signature; recipients with the roster verify it.

## Decisions

- UI: **per-send long-press override only** — no device-stored default preference. Signing is opt-in for each send.
- Default off, with an in-UI explanation of the tradeoff (a signature is transferable, non-repudiable proof of authorship).
- Recipient indicator in scope (iOS + Kotlin), **chat view only** — not the conversation list. Glyph: `checkmark.seal`.
- Scope: `XMsgNew` + `XMsgUpdate` + `XMsgDel`, **including as-channel posts** (see §5 — signing an as-channel post is verifiable and de-anonymizing, a deliberate team-accepted tradeoff). Edits sign iff the original was signed; deletes sign per-item (self-delete iff the target was signed; moderation/admin delete always-signs — §7). Mutations of a held-signed item are enforced to be signed (§7). Reactions stay unsigned.

## Threat model

Actors: the sending member, recipients, and untrusted **chat relays** that forward content + roster.

- **Forgery of member content** — closed for signed messages: the relay lacks the Ed25519 key, and the signature binds `(publicGroupId, memberId, body)`, so no forgery, cross-bind, or alteration.
- **Downgrade / stripping** (residual, narrowed) — optional signing lets a relay strip the signature from an *original* post and deliver it unsigned (the recipient never holds it signed, so nothing is enforced). Absence of a badge is *not* proof of forgery; only the presence of a verified badge is a guarantee. Once a recipient holds an item signed, its edits/deletes are enforced (next bullet), so the residual is now only the original-delivery case. A future "required signing" group setting would close even that (out of scope).
- **Signed-mutation enforcement** (fail-closed) — an `XMsgUpdate`/`XMsgDel` targeting an item the recipient holds signed (`msgSigned = Just _`) MUST itself carry a verifying signature; an unsigned mutation is rejected (drop + `RGEMsgBadSignature`). Closes the edit/delete-downgrade spoof (a relay forging an unsigned edit/delete to overwrite or censor a signed post) at the source. The legitimate sender produces the required signature: edits sign iff the original was signed; self-deletes iff the target was signed; moderation deletes always sign (§7). Coverage is for items the recipient already holds signed — catch-up members holding the post unsigned are outside it until PR 2 history preservation.
- **As-channel posts: anonymity for unsigned, accepted de-anonymization for signed.** An owner can "publish as the channel"; Design Objective 6 (`docs/protocol/channels-overview.md:214`) hides *which* owner authored a post from subscribers, and owners are "cryptographically indistinguishable to subscribers" (`:159`). This anonymity holds for **unsigned** as-channel posts: they forward via `FwdChannel` (no `memberId`), and a relay revealing the owner is only a deniable, detectable leak (`:237`). **Signing** an as-channel post is opt-in and deliberately gives it up: to be verifiable it forwards via `FwdMember` (§5), so every subscriber's device receives, verifies, and holds non-repudiable proof of the authoring owner. The owner is trading anonymity + deniability (`:198`, `:221`, `:103`) for verifiability on that post; the UI must say so. Verifiable-*and*-anonymous (ring signature / channel-level key) is out of scope.
- **Non-repudiation** (tradeoff, by design) — a verified signature is transferable proof of authorship; hence opt-in/off.
- **What "verified" proves** — the signed input is `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, and `msgBody` embeds `sharedMsgId`, `MsgScope`, `asGroup`, content. It proves authorship + integrity + group/member/scope/message binding, and nothing else — not `fwdBrokerTs` (relay-controlled), ordering, or completeness. Surface in help.
- **Bad signature is fail-closed** — a signature that fails to verify drops the message and creates an `RGEMsgBadSignature` item (`Subscriber.hs:3828`). Member keys do **not** rotate (communicated once on join, no rotation planned), so a have-the-key-but-mismatch can only mean forgery, tampering, or corruption — a genuine signal, and dropping is correct. New consequence for content (higher-volume, user-visible): such a message is **dropped**, not shown unsigned, exactly as state events already behave. The lagging-roster case is *not* a drop — if the recipient's roster lacks the author's key, that is the `MSSSignedNoKey` path (accepted, shown without badge), not the mismatch path. So there is no honest false-positive for the drop. Needs an edge-case test and a help note.
- **As-channel spoofing** — because signed as-channel posts arrive as `FwdMember`, the recipient MUST verify the (verified) author is an owner before rendering as-channel (§5); otherwise a non-owner's signed `asGroup=True` post would display as "from the channel".
- **Replay** — the binding covers `sharedMsgId` + `MsgScope`; cross-scope/group replay is blocked, same-message replay is a dedup duplicate.

## What already exists (reused unchanged)

- **Send / sign**: `groupMsgSigning` (`Internal.hs:2110`) → `createSndMessages` threads `Maybe MsgSigning` → `createNewSndMessage` Ed25519-signs `encodeChatBinding CBGroup (publicGroupId, memberId) <> msgBody`, storing `signedMsg_` in `SndMessage` (`Messages.hs:1156`).
- **Wire**: `batchMessages` prepends the signature via `encodeBatchElement` (`Batch.hs:45,69`); relay groups always batch.
- **Forward**: live delivery preserves the original signed bytes by reconstructing `VMSigned` from the stored `msg_chat_binding`/`msg_signatures` (`Store/Delivery.hs:155-165`); `fwdSender` is derived from the stored `showGroupAsSender` (`:158`).
- **Receive / verify**: `withVerifiedMsg` (`Subscriber.hs:3819`) wraps member-authored messages (non-forwarded path `:1037`, forwarded `FwdMember` path `:3780`). `XMsgNew_`/`XMsgUpdate_`/`XMsgDel_` are not in `requiresSignature` ⇒ `signatureOptional` (`:3848`): signed ⇒ `MSSVerified` (key present) / `MSSSignedNoKey` (no roster key), unsigned ⇒ accepted (§7 adds the held-signed enforcement on top). `FwdMember` verifies against the author's key (`verifyGroupSig`, `:3833`); `FwdChannel` is delivered as `VMUnsigned` (`:3783`). No protocol-version bump.
- **Persistence**: own item — `createNewSndChatItem` sets `MSSVerified <$ signedMsg_` (`Store/Messages.hs:548`); received item — `RcvMessage.msgSigned` (`Messages.hs:1174`) is stored by `createNewRcvChatItem` (`Store/Messages.hs:563`); `CIMeta.msgSigned` (`Messages.hs:520`).
- **CLI**: `sigStatusStr` (`View.hs:389`) renders "(signed)" / "(signed, no key to verify)".

Missing: (1) the decision to sign content; (2) per-send plumbing from the API; (3) as-channel forward/display/guard (§5); (4) edit reuse; (5) the badge fix (§7); (6) the apps.

## Core changes (Haskell)

### 1. `signableContent` predicate

Next to `requiresSignature` (`Protocol.hs:1334`):

```haskell
-- | Content events a member may sign (XMsgNew opt-in; XMsgUpdate/XMsgDel when the target was signed).
signableContent :: CMEventTag e -> Bool
signableContent = \case
  XMsgNew_ -> True
  XMsgUpdate_ -> True
  XMsgDel_ -> True
  _ -> False
```

### 2. Signing decision takes the opt-in flag

`groupMsgSigning` (`Internal.hs:2110`) gains a leading `Bool` — it stays blind to `showGroupAsSender` (as-channel posts sign with the owner's `CBGroup` binding like any member post):

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

Three call sites — all but the content/edit chain pass `False`: `sendGroupMemberMessages` (`Internal.hs:2119`, hardcode `False`), `sendGroupMessages_` (`:2364`, passes its threaded flag), and the direct `XGrpLeave` send (`Commands.hs:3019`, `False`).

### 3. Thread `sign :: Bool` through the send functions

Add the flag to `sendGroupMessages` (`Internal.hs:2329`), `sendGroupMessages_` (`:2362`), `sendGroupMessage` (`:2255`), and the content wrappers `sendGroupContentMessages` / `sendGroupContentMessages_` (`Commands.hs:4430` / `:4439`). Keep `sendGroupMessage'` (`Internal.hs:2261`) and `sendGroupMemberMessages` (`:2116`) unchanged by hardcoding `False`.

Behavior-preserving (every existing caller passes `False`) ⇒ its own commit. The only two variable-flag sites are content send (`Commands.hs:4469`) and edit (`:751`). Other callers pass `False`: `sendGroupMessages` — `Commands.hs:812,819,2821,2958`; `sendGroupMessages_` — `Commands.hs:2869,3912`; `sendGroupMessage` — `Commands.hs:908,2721,3327,3875,3878,3882`.

`sendGroupContentMessages` has three callers, all reached via §4 except the first: the content-send handler (`Commands.hs:667`, real flag), `APIReportMessage` (`:698`, `False`), and `APIForwardChatItems` (`:994`, `False`).

`Bool`-choice note: `sign` joins `showGroupAsSender :: ShowGroupAsSender (= Bool)` and `live :: Bool` in `sendGroupContentMessages`/`_`. Place `sign` away from the other two in each signature (e.g. after `itemTTL`) to reduce silent transposition.

### 4. API: per-send `sign` flag

Add a field to `APISendMessages` (`Controller.hs:382`):

```haskell
| APISendMessages {sendRef :: SendRef, liveMessage :: Bool, ttl :: Maybe Int, signMessages :: Bool, composedMessages :: NonEmpty ComposedMessage}
```

Parser (`Commands.hs:5094`), defaulting off so old command strings still parse:

```haskell
"/_send " *> (APISendMessages <$> sendRefP <*> liveMessageP <*> sendMessageTTLP <*> signMessagesP <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP))
-- signMessagesP = " sign=" *> onOffP <|> pure False   (after sendMessageTTLP)
```

The nine internal positional constructors of `APISendMessages` must gain the field (`False`): `Commands.hs:2394,2403,2423,2443,2451,2496,3238,3279,3288`. Compiler-caught, part of the behavior-preserving commit.

The handler (`Commands.hs:654-667`) flows `signMessages` to `sendGroupContentMessages` for group sends and ignores it for direct sends. `APIReportMessage` (`:693-698`) passes `False`.

### 5. As-channel signing: forward with member id, display from `asGroup`, enforce owner

An owner may sign an as-channel post (no gate on `showGroupAsSender`). Three pieces make it verifiable while displaying as the channel:

- **Forward (`Store/Delivery.hs:158`)**: route signed as-channel posts via `FwdMember` so subscribers can verify; keep unsigned as-channel anonymous via `FwdChannel`:
  ```haskell
  fwdSender = if showGroupAsSender && isNothing chatBinding_ then FwdChannel else FwdMember senderMemberId senderMemberName
  ```
  (`chatBinding_`/`sigs_` are already in scope here for `verifiedMsg`; `isNothing chatBinding_` ⇔ unsigned. Non-as-channel posts already use `FwdMember`, unchanged.)
- **Display**: the recipient already derives "as channel" from the signed `asGroup` flag, independent of `fwdSender` — `newGroupContentMessage` `sentAsGroup = asGroup_ == Just True` (`Subscriber.hs:2177`), `groupMessageUpdate` `showGroupAsSender = fromMaybe (isNothing m_) asGroup_` (`:2235`). So a `FwdMember` + `asGroup=True` post verifies against the owner and renders as the channel with the verified badge.
- **Owner guard (security, MUST)**: the forwarded `XMsgNew` path (`xGrpMsgForward` `Subscriber.hs:3771` → `FwdMember` branch `:3775` → `withVerifiedMsg` `:3784` → `newGroupContentMessage` `:3795`) MUST reject as-channel display unless the verified author is an owner — parity with `groupMessageUpdate`'s owner guards (`:2236` send-time, `:2282` store-time) and the direct send path's `checkSendAsGroup` (`Subscriber.hs:1057`, `asGroup == Just True && memberRole' m'' < GROwner ⇒ messageError`); reuse the `validSender … CIChannelRcv ⇒ GROwner` pattern (`:2135`). Without it a non-owner's signed `asGroup=True` post renders as "from the channel". The guard is reliable for *legitimate* owner posts: `GROwner` identity is established from root-key-verifiable link data at connect time (`createLinkOwnerMember`, `Store/Groups.hs:3381`), and `isRosterRole` (`Internal.hs:1257`) excludes `GROwner` — so owner identity is never carried by the roster or `XGrpMemRole` and is therefore independent of the known role-propagation issue. Single owner today.
- **Invariant**: `FwdChannel` never carries a signature (signed posts always route via `FwdMember`). Assert this in `encodeFwdElement` (`Batch.hs:108`) as a regression guard.

`sendGroupContentMessages_` passes the API `sign` straight through (no `&& not showGroupAsSender` gate). The owner's own as-channel item is marked signed/verified like any signed send.

### 6. Edit reuse (`XMsgUpdate`)

Group edit, `Commands.hs:739-757`. The own sent item is loaded with `CIMeta` (`:739`); add `msgSigned` to the pattern and reuse it:

```haskell
let reuseSign = isJust msgSigned
SndMessage {msgId, signedMsg_} <- sendGroupMessage user gInfo scope recipients reuseSign event
```

For own sent items, `msgSigned` is `Just MSSVerified` iff signed (`createNewSndChatItem`, `Store/Messages.hs:548`), so `isJust` is the right test. An edit is signed exactly when the original was — now **mandatory**, not optional: recipients enforce it (§7). The author's own item is authoritative, so the author always emits a signature exactly when recipients require one (no divergence). Direct/local edits need no change.

### 7. Enforcement: a held-signed item's mutations must be signed (security)

**Replaces the earlier "badge refresh" approach.** Principle: if a recipient holds an item as signed (`msgSigned = Just _`), any subsequent `XMsgUpdate` or `XMsgDel` targeting it MUST itself carry a verifying signature; an unsigned mutation of a signed item is **rejected** (drop, reusing `RGEMsgBadSignature`), not applied. This closes the edit/delete-downgrade spoof at the source (fail-closed) instead of applying attacker content and relabelling the badge.

`updateGroupChatItem` / `updateGroupChatItem_` (`Store/Messages.hs:2749`/`:2758`) and their five callers are left **unchanged** — no `Maybe MsgSigStatus` param, no `msg_signed` in the `UPDATE`, no override of `ci'`. A legitimate enforced-signed edit keeps the original `MSSVerified`, so the badge stays correct without a refresh. (The one thing a refresh would have added — reflecting a rare `MSSSignedNoKey → MSSVerified` transition after a lagging roster catches up — is deliberately dropped.)

**Receive check (both paths; the data is already in scope):**

- **Update** — `groupMessageUpdate` (`Subscriber.hs:2225`): it already loads the target item (`:2277`) and has the incoming `RcvMessage` (`:2226`). Before applying the update, if the target's `meta.msgSigned` is `Just _` and the incoming `rcvMsg.msgSigned` is `Nothing`, reject (`messageError` → drop → `RGEMsgBadSignature`).
- **Delete** — `groupMessageDelete` (`Subscriber.hs:2307`): the target `CChatItem` from `findItem` (`:2357`) carries `meta.msgSigned`; add `msgSigned` to the `RcvMessage` pattern (`:2356`) to get the incoming verdict. Same check. **One site covers self-delete, moderation, and forwarded delivery** (all route through `groupMessageDelete`). It composes with — does not touch — the existing `checkRole` WHO-gate (`:2382`): a moderation delete verifies against the *moderator* (its sender) via `withVerifiedMsg`, and `checkRole` independently enforces authority.

"Signed" for the requirement means `MSSVerified` **or** `MSSSignedNoKey` (the sender signed); only `Nothing` (unsigned) is rejected. Since keys do not rotate, an item held `MSSVerified` verifies its mutations too, so this never spuriously rejects a legitimate live-held mutation.

**Send side — produce the required signature so legitimate mutations are accepted:**

- **Edits** (`XMsgUpdate`): §6 already signs the edit iff the original was signed (`reuseSign = isJust msgSigned`); under enforcement this is mandatory. The author's own item is authoritative, so the author emits a signature exactly when recipients require one — no divergence.
- **Deletes** (`XMsgDel`) — per-item, because deletes batch a heterogeneous set of targets (`Commands.hs:811/818/3909`) and signing is currently keyed off event *type*, not instance. Compute the signer per delete at the send sites (the target `items` and their `msgSigned` are already in scope) and thread it in. **Do not** add `XMsgDel_` to `requiresSignature` (over-signs, ignores the per-item condition). Generalize the batch send to carry a per-event `Maybe MsgSigning` rather than recomputing one `groupMsgSigning` per batch — parameterize the existing body, do **not** duplicate it; keep a uniform-sign wrapper so non-delete callers are unchanged (this generalization lands in commit 1, behavior-preserving). Two cases:
  - **Self-delete** (`memberId = Nothing`, `Commands.hs:811/818`): sign iff the deleter's own copy of the target was signed. The self-deleter's view is authoritative, so signed-holders and catch-up-unsigned-holders both accept — no divergence; and it preserves the deniability choice (an unsigned, deniable post's self-delete stays unsigned/deniable).
  - **Moderation/admin delete** (`memberId = Just`, `Commands.hs:3909`): **always sign** in relay channels. A moderator who holds the target unsigned (joined late, caught up via unsigned history) would otherwise emit an unsigned delete that members holding the post signed would reject — silently failing moderation. Moderation deletes already carry the target `memberId` (attributable), so always-signing costs no deniability and removes the divergence.

`signableContent` (§1) includes `XMsgDel_` so `groupMsgSigning sign …` produces a signer when the per-item/per-action decision is to sign.

### 8. Paths deliberately left unsigned

- Reactions (`XMsgReact`, `Commands.hs:908`): pass `False` — never signed (a reaction does not mutate the post's content/integrity and is not enforced).
- Auto-reply welcome content via `sendGroupMessage'` ⇒ `False`.
- Deletes are signed conditionally and enforced (§7) — no longer left unsigned.

## App changes (iOS + Kotlin)

Locate by symbol — app line numbers drift independently.

- **A. Decode the status.** JSON tags come from `enumJSON (dropPrefix "MSS")`: `MSSVerified → "verified"`, `MSSSignedNoKey → "signedNoKey"` — not the DB strings "verified"/"no_key". Add an optional `msgSigned: MsgSigStatus?` to `CIMeta` on both platforms (iOS `ChatTypes.swift`; Kotlin `ChatModel.kt`), decoding `verified`/`signedNoKey`. Optional ⇒ old core JSON decodes safely.
- **B. Composer long-press option + thread `sign` to the API.** No device preference — the long-press is the only entry. Add `sign: Bool` to the send closure (default off) and a long-press item next to "Disappearing message" ("Sign message" / "Send without signing", iOS `SendMessageView.swift`; Kotlin `SendMsgView.kt`). Show it for any relay channel, gated on the app's existing relay-channel indicator (`useRelays'` — the same signal that drives the as-channel composer). No key-derived flag is needed: every sendable member holds a signing key (the only keyless state, prepared/`GSMemUnknown`, is non-current and cannot send, so the composer is not available there). Confirm the app's `GroupInfo` exposes a member-agnostic relay-channel boolean (the as-channel toggle is owner-scoped; signing is offered to all members) — if the only existing signal is owner-scoped, add a plain non-secret relay-channel boolean, not a `memberPrivKey`-derived one. It is shown for as-channel sends too, with an explicit note that signing an as-channel post reveals you as the author (§5 tradeoff). Append `sign=on|off` in `apiSendMessages` on both platforms.
- **C. Recipient indicator.** In the message meta row (`CIMetaView`, chat view only), show `checkmark.seal` when `meta.msgSigned == verified`, in the trust cluster next to `lock` and before the timestamp. iOS: append `statusIconText("checkmark.seal", color)` in `ciMetaText`. Kotlin: add an `Icon` branch in `CIMetaText` **and** the matching `iconSpace` branch in `reserveSpaceForMeta` (the in-file contract requires the two to match); add the matching seal vector to `MR.images`. Omit `signedNoKey` (only `.verified` is badged). Own signed items use the same glyph. Conversation list (`ChatPreviewView`) unchanged. Surface the "verified ≠ timestamp/ordering/completeness" caveat in help.

## Compatibility

- Wire format unchanged (the batch-element signature prefix already exists); no `chatVRange` bump; pre-feature relay-capable peers verify/accept.
- API command `sign=` is additive with a default; app + core ship together.
- No DB migration — `chat_items.msg_signed` already exists (written by `createNewChatItem_`, `Store/Messages.hs:614`; read by `mkCIMeta`).
- The new optional app `msgSigned` decodes as absent on older cores.

## Edge cases, races, correctness

- **Bad signature → drop** (threat model): a signed content message whose signature doesn't verify against the recipient's roster key (forgery/tamper — keys don't rotate) is dropped + `RGEMsgBadSignature`, not shown unsigned. Test it; note it in help. Distinct from §7 enforcement, which rejects an *unsigned* mutation of a *signed* item.
- **Member without keys** (`groupKeys = Nothing`): only the prepared/`GSMemUnknown` relay-channel state is keyless (`createPreparedGroup`, `Store/Groups.hs:654`, with the `TODO [member keys]` marker), and that state is non-current/non-active — it cannot send content, so the composer is unavailable. Any sendable membership has `groupKeys = Just` (key written before the membership becomes current). `groupMsgSigning` returning `Nothing` for `groupKeys = Nothing` is thus a harmless backstop, never reached on a real content send.
- **Non-relay groups**: the `useRelays'` guard ⇒ never signed; UI must not offer it.
- **Live messages**: each `XMsgUpdate` reuses the item's `msgSigned`, so every increment is signed iff the original was — exactly what §7 enforcement requires. Acceptable cost.
- **Mutation enforcement (§7)**: a forged *unsigned* `XMsgUpdate`/`XMsgDel` of a held-signed item is rejected (`RGEMsgBadSignature`), content/visibility unchanged — not applied-then-unbadged. A legitimate signed edit/delete of a signed item is accepted. Test both, on both paths.
- **Moderation-delete divergence**: a moderator holding the target *unsigned* (caught up via unsigned history) must still emit a *signed* delete so members holding the post signed accept it — hence moderation always-sign (§7). Without it, moderation of a signed post silently fails for those members. Test a moderation delete from a catch-up moderator.
- **Non-batched path**: `sndMessageMBR` (`Internal.hs:2428`) uses raw `msgBody`, never reached in relay groups (`memberSendAction → MSASendBatched`). Add a test-asserted invariant; optionally route it through `encodeBatchElement signedMsg_`.
- **History downgrade (posts, by design this PR)**: relay history catch-up rebuilds content unsigned and as-channel via `FwdChannel` (`sendHistory` / `processContentItem`, `Internal.hs:1278` / `:1349`; the relay re-encodes from `MsgContent` and has no author key). So a signed post (channel or member) is delivered unsigned on catch-up — no badge, and an as-channel post re-anonymizes. Graceful (absence ≠ forgery); document and test. Consequently PR 1's mutation enforcement (§7) does not fire for catch-up members (they hold the post unsigned), so a forged unsigned edit/delete still lands for them — the documented honest limit. PR 2 preserves signatures through history.
- **Concurrency**: signing/verification are pure given keys; no new shared state. Send holds `withGroupLock`; receive runs under existing serialization. No new races.

## Tests

- Protocol (`tests/ProtocolTests.hs`): round-trip signed `XMsgNew`/`XMsgUpdate`/`XMsgDel`; assert binding `CBGroup <> (publicGroupId, memberId)`; verify accepts the right key, rejects wrong key / altered body / altered binding.
- Integration (`tests/ChatTests/Groups.hs`, relay/channel setup): sign+verify ⇒ "(signed)"; off/default ⇒ none; missing roster key ⇒ "(signed, no key to verify)"; edit reuse keeps/omits the badge; **edit enforcement** — unsigned forged `XMsgUpdate` over a signed item ⇒ rejected (`RGEMsgBadSignature`), content unchanged (§7), and a legitimate signed edit of a signed item ⇒ accepted; **delete enforcement** — unsigned forged `XMsgDel` of a signed item ⇒ rejected, item not deleted (§7); signed self-delete of a signed item ⇒ deletes; unsigned self-delete of an unsigned item ⇒ deletes (no requirement); **moderation delete** — moderator's signed delete of a signed post ⇒ accepted + role-checked, and moderation always-sign holds even when the moderator holds the target unsigned; **as-channel signed** — owner `as_group=on sign=on` ⇒ recipient verifies and shows "(signed)" while displaying as the channel; **as-channel unsigned** — forwards via `FwdChannel`, no member id on the wire; **as-channel spoof** — non-owner `asGroup=on sign=on` ⇒ rejected (§5 guard); **history downgrade** — live recipient "(signed)", catch-up recipient not, and enforcement does not fire for the catch-up recipient; **bad-signature drop**; forgery rejection ⇒ `RGEMsgBadSignature`.
- App: minimal decode test that `"verified"` / `"signedNoKey"` parse to the right enum on both platforms.

## Commit plan (PR 1)

1. **Structural (behavior-preserving)**: add `signableContent` (`XMsgNew_`, `XMsgUpdate_`, `XMsgDel_`); parameterize `groupMsgSigning` with `sign :: Bool`; thread `sign` through the content/edit send chain; generalize the batch send to carry a per-event `Maybe MsgSigning` (uniform-sign wrapper so existing callers are unchanged); add `signMessages :: Bool` to `APISendMessages` + its nine positional constructors. Every caller passes `False`/`Nothing` ⇒ no behavior change.
2. **Content signing + update enforcement (core)**: `APISendMessages` parser; content send passes the real flag; edit signs iff the original was signed (§6); reject an unsigned `XMsgUpdate` of a signed item in `groupMessageUpdate` (§7); as-channel forward/display/owner-guard (§5). After this commit the update path is complete and spoof-free.
3. **Delete signing + delete enforcement (core)**: per-item delete signing at the three send sites — self-delete conditional, moderation always-sign (§7); reject an unsigned `XMsgDel` of a signed item in `groupMessageDelete` (§7). After this commit the delete path is complete.
4. **App**: decode `msgSigned` + recipient indicator (§C).
5. **App**: composer long-press option + `apiSendMessages` wiring (§B).
6. **Tests** (may accompany 2/3).

Each commit builds and passes tests independently. §7's earlier `updateGroupChatItem` plumbing is dropped — that helper and its five callers are untouched, so there is no badge-fix commit.

### Pre-implementation gates

- **MUST**: the mutation-enforcement check is on **both** `groupMessageUpdate` and `groupMessageDelete` — an unsigned `XMsgUpdate`/`XMsgDel` of a `Just`-signed target is rejected via `RGEMsgBadSignature`. A missed path reopens the spoof.
- **MUST**: per-item delete signing is wired at all three delete send sites (`Commands.hs:811/818` self-delete conditional; `:3909` moderation always-sign), and `XMsgDel_` is **not** added to `requiresSignature`.
- **MUST**: the as-channel owner guard (§5) is on the forwarded `XMsgNew` path, and `FwdChannel` carries no signature.
- **SHOULD**: re-grep `groupMsgSigning` / `sendGroupMessage` / `sendGroupMessages` / `sendGroupMessages_` callers; the batch send is generalized (per-event `Maybe MsgSigning`) without duplicating its body; only content-send, edit, and delete pass a variable signer.
- **SHOULD**: the "verified" caveats (no timestamp/ordering; history downgrade; bad-signature drop) and the as-channel de-anonymization warning are surfaced in UI/help, and those tests exist.

## Deferred to PR 2: history signature preservation

Signable deletes and recipient enforcement moved into PR 1 (§7). What remains is the hard part PR 1's enforcement degrades around.

**History signature preservation for posts.** On catch-up the relay rebuilds content unsigned (`processContentItem` re-encodes from `MsgContent`, has no author key; re-encoding invalidates the original signature — `Store/Delivery.hs:162`). So a catch-up member holds an originally-signed post as `Nothing`, and PR 1's enforcement does not fire for them (target held unsigned ⇒ no signature required). Two residual gaps that only preservation closes:

- a relay can forge an unsigned delete/edit of a signed post for catch-up members (they hold it unsigned, so nothing is enforced);
- the deeper inconsistency behind the moderation-divergence handling — members disagreeing on a post's signed status — is fully resolved only when all members hold the post signed.

Design question (unchanged): does the `messages` row survive long enough to forward the original signed bytes on catch-up, or must signed bytes be persisted on the chat item (migration)?

Honest limit until then: enforcement protects a post a recipient already holds verified; a relay that delivered the original unsigned sidesteps it (visible as a missing badge, not prevented).

## Out of scope / future

- Group-level "required signing" owner setting — rejects unsigned messages group-wide, closing the optional-downgrade gap.
- **Verifiable-anonymous as-channel** — a channel-level signature (ring signature over the owner set, or a shared/authorization-chain channel key) that proves "a valid owner" without revealing which, so an as-channel post is verifiable *and* keeps sender anonymity. This PR's per-member signing cannot do both; signing an as-channel post reveals the owner (§5).
- Signing reactions; signing auto-reply content; verifiable reports (signed `MCReport`).

## Verification status (2026-06-25, branch `f/msg-signing`)

Every symbol the plan names was located and its logic re-checked against current code. **No logic drift** was found in any anchored function — the plan's described behavior still holds everywhere. Three classes of correction below: mislabeled anchors (fix before relying on them), one structural clarification (threading is slightly larger than the prose implies), and a current line map (most anchors moved a little; re-confirm by symbol regardless).

### Mislabeled / materially-moved anchors

- `MsgSigStatus` is defined in **`src/Simplex/Chat/Types/Shared.hs:134`** (`MSSVerified | MSSSignedNoKey`), not in `Types.hs`/`Messages.hs`. DB encoding: `verified` / `no_key`; JSON tags (`enumJSON $ dropPrefix "MSS"`): `verified` / `signedNoKey`. The JSON/DB divergence the app section (§A) relies on is confirmed. Any core edit to the type itself targets `Types/Shared.hs`.
- §5 "the direct path (`:1064`)" is wrong: the non-forwarded as-channel owner enforcement lives in **`checkSendAsGroup` (`Subscriber.hs:1057`)**; `:1064` is just the `XMsgNew → newGroupContentMessage` dispatch (fixed inline in §5).
- §6 wording "add `msgSigned` to the pattern" — confirmed the own-item `CIMeta` pattern at `Commands.hs:739` does **not** bind `msgSigned` today; it must be added to the record pattern (the field exists on `CIMeta`, `Messages.hs:520`).
- §3 "`sendGroupMessage` … 6 callers" — there are **7** `sendGroupMessage` sites; the 7th is the edit path itself (`Commands.hs:751`, the only variable-`sign` one). The six that pass `False`: `Commands.hs:908,2719,3325,3873,3876,3880`.
- `signatureOptional` is at `Subscriber.hs:3848` (plan said ~:3844); `RGEMsgBadSignature` creation at `:3828` (plan said :3824). Behavior unchanged.

### Structural clarification (affects §2/§3 threading)

`groupMsgSigning` is invoked at **three** sites, two of them inside functions that do **not** currently receive any as-channel/sign flag:

- `Internal.hs:2122` — inside `sendGroupMemberMessages` (hardcode `False`).
- `Internal.hs:2367` — inside `sendGroupMessages_`.
- `Commands.hs:3017` — direct `XGrpLeave` via `createSndMessages` (hardcode `False`).

`sendGroupMessages_` has **no** `ShowGroupAsSender`/`asGroup` parameter, and `sendGroupMessages` (`:2332`) consumes its `ShowGroupAsSender` only for `shouldSendProfileUpdate` — it does **not** pass it down. So threading `sign` to the `groupMsgSigning` call inside `sendGroupMessages_` is genuinely new wiring: `sendGroupContentMessages_`/`sendGroupMessage` → `sendGroupMessages` → **(new param)** → `sendGroupMessages_` → `groupMsgSigning sign …`. The plan's §3 list already includes all these functions; this note just flags that the param add to `sendGroupMessages_` is load-bearing, not a pass-through that already exists.

### Current line map (re-confirm by symbol)

| Symbol | File:line |
|---|---|
| `requiresSignature` (insert `signableContent` next to it) | Protocol.hs:1334 |
| `MsgSigning` (4-field record, applied positionally) | Protocol.hs:469 |
| `encodeChatBinding` / `CBGroup` / `signChatMsgBody` | Protocol.hs:476 / 442 / 479 |
| `FwdSender` (`FwdMember MemberId ContactName` / `FwdChannel`) | Protocol.hs:372 |
| `groupMsgSigning` | Internal.hs:2113 (calls: 2122, 2367, Commands.hs:3017) |
| `sendGroupMessages` / `_` | Internal.hs:2332 / 2365 |
| `sendGroupMessage` / `'` | Internal.hs:2258 / 2264 |
| `sendGroupMemberMessages` | Internal.hs:2119 |
| `sndMessageMBR` (non-batched; never hit in relay groups) | Internal.hs:2431 (`memberSendAction` 2455, `MSASendBatched` 2453) |
| `sendHistory` / `processContentItem` (as-channel → `FwdChannel` at 1375) | Internal.hs:1281 / 1352 |
| `createNewSndMessage` (`signedMsg_`) | Store/Messages.hs:235 |
| `APISendMessages` ctor | Controller.hs:382 |
| `/_send` parser / `onOffP` | Commands.hs:5111 / 5490 |
| 9 positional `APISendMessages` ctors (add `False`; `:2449` passes `live=True`) | Commands.hs:2392,2401,2421,2441,2449,2494,3236,3277,3286 |
| `APISendMessages` handler / group send | Commands.hs:654 / 667 |
| `sendGroupContentMessages` / `_` | Commands.hs:4447 / 4456 (callers 667, 698, 994) |
| group edit (own-item pattern / send) | Commands.hs:739 / 751 |
| `XGrpLeave` group send (`sendGroupMessage'`) | Commands.hs:3027 |
| `verifyGroupSig` / `withVerifiedMsg` / `signatureOptional` | Subscriber.hs:116 / 3823 / 3848 |
| `RGEMsgBadSignature` | Subscriber.hs:3828 |
| forwarded `XMsgNew`: `xGrpMsgForward` / `FwdMember` / `withVerifiedMsg` / `newGroupContentMessage` / `FwdChannel→VMUnsigned` | Subscriber.hs:3771 / 3775 / 3784 / 3795 / 3787 |
| `newGroupContentMessage` (`sentAsGroup` 2177) / `groupMessageUpdate` (owner guards 2236, 2282) | Subscriber.hs:2142 / 2225 |
| `validSender` (`CIChannelRcv ⇒ GROwner`) / `checkSendAsGroup` | Subscriber.hs:2135 / 1057 |
| `updateGroupChatItem` / `_` / `UPDATE` / `updatedChatItem` | Store/Messages.hs:2749 / 2758 / 2766 / 2547 |
| §7 `updateGroupChatItem` callers (all 5) | Commands.hs:757; Subscriber.hs:1200,1566,2258,2298 |
| `createNewSndChatItem` (`MSSVerified <$ signedMsg_`) / `createNewRcvChatItem` / `createNewChatItem_` (`msg_signed`) / `mkCIMeta` | Store/Messages.hs:548 / 562 / 614 / Messages.hs:528 |
| `Store/Delivery.hs` `fwdSender` / `VMSigned` reconstruction | Delivery.hs:158 / 162–164 |
| `sigStatusStr` | View.hs:389 |
| iOS `CIMeta` / composer menu / `ciMetaText` / `/_send` build | ChatTypes.swift:3825 / SendMessageView.swift:238 / CIMetaView.swift:93 / AppAPITypes.swift:243 |
| Kotlin `CIMeta` / composer menu / `CIMetaText` + `reserveSpaceForMeta` / `/_send` build | ChatModel.kt:3529 / SendMsgView.kt:198 / CIMetaView.kt:67 + 118 / SimpleXAPI.kt:3870 |
| channels-overview.md anchors (:103,:159,:198,:214,:221,:237) | all confirmed |

## Open design questions (2026-06-25)

Status as of 2026-06-29: all resolved (Q1/Q3/Q5 by the team; Q2/Q4 by code verification). **Design change (2026-06-29):** §7's "badge refresh" was replaced by receive-time **enforcement** — a held-signed item's `XMsgUpdate`/`XMsgDel` must be signed or is rejected (`RGEMsgBadSignature`); signed deletes + enforcement moved from PR 2 into PR 1 (update *and* delete); §7's `updateGroupChatItem` plumbing dropped. One sub-rule baked in pending confirmation: moderation/admin deletes **always** sign in relay channels (self-deletes sign conditionally) to avoid the catch-up-moderator divergence — see §7.

1. **RESOLVED — keep drop + `RGEMsgBadSignature`.** Member keys do not rotate (one key per member, communicated on join), so a have-the-key-but-mismatch is a genuine forgery/tamper signal, and a downgrade-to-unsigned would buy nothing against a malicious relay (which can simply drop the whole message). Behavior stays identical to existing signed events; the lagging-roster case is the `MSSSignedNoKey` accept path, not the drop path, so there is no honest false-positive. Threat-model bullet updated accordingly. Action: edge-case test + help note only.

2. **RESOLVED — gate on `useRelays'` alone, no key-derived flag.** Verified: the only relay-channel state with `groupKeys = Nothing` is prepared/`GSMemUnknown` (`createPreparedGroup`, `Store/Groups.hs:654`), which is non-current/non-active and cannot send — the key is written before the membership becomes sendable. So every sendable member has `groupKeys = Just`. §B simplified to a `useRelays'` gate; §B asks the app task to confirm a member-agnostic relay-channel boolean exists (the as-channel toggle is owner-scoped), adding a plain non-secret one if not. The "member without keys" edge case is now documented as a harmless backstop.

3. **RESOLVED — keep raw `Bool`.** No `SignMessages` newtype. Retain §3's placement guidance (put `sign` away from `showGroupAsSender`/`live` in each signature) as the transposition mitigation.

4. **RESOLVED — guard reads stable, link-data owner identity; no false-negative.** Verified: `GROwner` is established from root-key-verifiable link data at connect time (`createLinkOwnerMember`, `Store/Groups.hs:3381`), and `isRosterRole` (`Internal.hs:1257`) excludes `GROwner`, so owner identity is never carried by the roster or `XGrpMemRole`. The as-channel guard is therefore independent of the known role-propagation bug, and (single owner today) a legitimate owner post never fails it. The §5 propagation note is retracted. The guard stays a MUST — it blocks a non-owner's signed `asGroup=True` post from rendering as the channel.

5. **WITHDRAWN — non-issue.** A send's `sign` flag applies to its whole composed-message batch; `signableContent` filters to `XMsgNew`/`XMsgUpdate` and content sends do not interleave non-signable events.
