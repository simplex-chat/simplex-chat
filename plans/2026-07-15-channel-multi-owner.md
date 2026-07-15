# Channel multi-owner

A channel can have N equal owners, each acting independently, per `docs/protocol/channels-overview.md` §Governance ("Near-term (v7): Multiple owners, any-owner-decides"). Owners are fully equal: any owner can take any administrative action, including destroying the channel. No coordination between owners is required for any action.

## 1. What an owner must be able to do

1. Publish content, and publish as the channel.
2. Sign administrative messages that subscribers accept — profile, prefs, member removal, role changes, relay announcements, channel deletion.
3. Change member/moderator/admin roles, which means signing the roster.
4. Write the channel's link data — add/remove relays, update the profile and member count, and add the next owner.
5. Add another owner.

Today only the creator can do 2, 4 and 5, and 3 works only for one owner at a time. Each is enforced in a different place, so each needs its own change; item 4 is the one that reaches into simplexmq.

## 2. Current state

**The protocol layer is already built for this; the client is not.** `UserContactData.owners :: [OwnerAuth]`, `validateLinkOwners` (chain: each entry signed by root *or an earlier entry*), and `decryptLinkData` (accepts data signed by root *or any listed owner*) exist in simplexmq and are multi-owner-correct. `ShortLinkCreds.linkRootSigKey` exists with the comment *"in case the current user is not the original owner"*.

**SMP already supports multi-writer links.** `RKEY :: NonEmpty RcvPublicAuthKey -> Command Recipient` and `recipientKeys :: NonEmpty` landed in commit `b7a95422` *"smp server: short links and owners for channels"* — "support multiple rcv keys", with tests — at `shortLinksSMPClientVersion = 4 = currentSMPClientVersion`, so every server that can host a channel already accepts it. **The agent never calls RKEY.** This is the mechanism for item 4.

**Two blockers in simplexmq:**
- `AgentStore.hs:2514` — `linkRootSigKey = Nothing` on every load, with `-- TODO linkRootSigKey should be stored in a separate field`. There is no column, so the non-root branch of `validateOwners` is dead code.
- `Crypto/ShortLink.hs:80` — `newOwnerAuth` *generates the key pair locally*. Adding a remote owner must sign an externally supplied public key. The formula `sign(signingKey, ownerId <> encodePubKey ownerKey)` is duplicated in chat at `Internal.hs:1519`.

**The single-owner assumption is concentrated in one function.** `groupLinkData` (`Internal.hs:1509`, carrying `-- TODO [relays] owner: set owners on updating link data (multi-owner)`) rebuilds `owners` on every publish as a singleton containing *only the publishing user*, and as `[]` unless the user holds `GRKPrivate`. So (a) a second owner is never published; (b) once two owners can publish, each publish evicts the other; (c) a non-root owner would publish an empty owners list, making the link data unreadable to everyone.

**Owners are deliberately not in the roster.** `isRosterRole = GRMember | GRModerator | GRAdmin` (`Internal.hs:1281`: *"owners are on the link, not the roster"*); link data is the sole authority for owner keys (`Groups.hs:3533`: *"Role and key stay owner-authoritative … so taking either from an in-band relayed intro would let a compromised relay substitute them"*). This is preserved. Since `updateGroupFromLinkData` carries only profile + member count and runs only on connect/preview, a new owner goes into link data **and** is announced by a signed `x.grp.mem.role`, so existing subscribers learn it promptly rather than at their next join.

**`APIMembersRole … GROwner` on a channel is currently not rejected by the backend** — only by the iOS/Kotlin pickers. Via CLI it half-works: it skips the owner-only guard (`isRosterRole GROwner == False`), sends no key, publishes nothing to the link, and leaves the promoted device throwing `SEGroupLinkNotFound` from `updatePublicGroupData` on every `x.grp.mem.new`. The flow in §3 replaces that path.

## 3. Protocol

`APIMembersRole … GROwner` on a channel triggers this flow instead of performing a plain role change. It is two-phase and asynchronous: the command returns "invitation sent" and the role lands later.

Owner 2 must already be a channel member. Their member key is then already known to relays and subscribers, and the relay connections they will publish through already exist — a subscriber creates `GRRelay`/`GCHostMember` rows, so `getGroupRelayMembers` already returns them for a promoted subscriber and the transport works unchanged.

Three events, all `requiresSignature`, all over the existing relay connections to the invitee:

```
x.grp.owner.inv     { memberRole }                -- O1 -> M
x.grp.owner.acpt    { invitationId, ownerKey, linkRcvKey }   -- M  -> O1
x.grp.owner.creds   { linkRcvId }                 -- O1 -> M, after RKEY + LSET succeed
```

- `memberRole` rides in the invitation even though acceptance is automatic, so a later release can let the invitee accept or reject a named role without a wire change.
- `ownerKey` is the invitee's existing channel member public key — already signing their messages, already known to relays and subscribers. No new signing key.
- `linkRcvKey` is a fresh SMP recipient auth **public** key the invitee generates, so it can write link data.
- `invitationId` binds the acceptance to one invitation, so a relay cannot replay an old acceptance.
- `linkRcvId` is the link queue's recipient ID — the only non-public datum in the exchange. It is sent last, so a declining invitee never receives it.

Everything else owner 2 needs in order to write link data is public or derivable: `shortLinkKey` and `shortLinkId` from the channel link, and the fixed-data plaintext (hence `rootPubKey`) via `LGET`, re-encrypted under a fresh nonce — readers check `sha3_256(fd) == linkKey` and the root signature over the *plaintext*, not the ciphertext.

**Routing this over relays is safe.** The relay sees `linkRcvKey` (a public key) and `linkRcvId`. Neither is exploitable: `Server.hs:1249` verifies *every* recipient command against `recipientKeys`, so `linkRcvId` alone authorises nothing, and the matching private key never leaves owner 2's device. The relay cannot forge an acceptance — it is signed by owner 2's key, which the relay does not hold — and `invitationId` closes replay. A relay can drop the exchange, which is a liveness failure, not an escalation.

O1 on acceptance, driven by a durable worker (mirroring `runRelayRequestWorker`: multi-step, cross-network, must survive a crash mid-way):

1. `RKEY` — add `linkRcvKey` to the link queue's recipient keys.
2. `LSET` — publish `owners = [oa1, …, oaN, mkOwnerAuth memberId2 ownerKey signingKey]`.
3. `x.grp.owner.creds` to M; `x.grp.mem.role memberId2 GROwner (Just ownerKey)` to relays → forwarded to subscribers.

LSET precedes the role broadcast so a joiner in the window learns owner 2 from the link — the authority — rather than by TOFU. Because LSET is a blind overwrite (§4.2), the worker must verify its entry survived and re-publish if not; LSET returning success does not mean the entry is still published.

## 4. Concurrency

### 4.1 Roster: same version from two owners, multiple relays

With O1 and O2 both at version N-1 and both making a change:

```
O1 -> R1,R2 : roster N (content A)
O2 -> R1,R2 : roster N (content B)
R1 sees A then B: fresh = N >= N -> applies B    -> R1 = B
R2 sees B then A: fresh = N >= N -> applies A    -> R2 = A
```

Both relays accepted both, in different orders, and now serve different rosters to different subscribers, permanently. The cause is that `fresh = maybe True (v >=) gate` (`Subscriber.hs:3302`, and `notBelowRoster` at `:3430` for the blob) is not a total order — at equal versions the outcome depends on arrival order.

**Make the version a total order by tie-breaking on the author's `memberId`.** Accept iff `(v, authorMemberId) >= (gate_v, gate_authorMemberId)` lexicographically. `memberId` is 12 random bytes and identical on every device, so every node computes the same winner. `roster_sending_owner_gm_id` already exists on `groups` but is a **local row id**, not comparable across devices; add `roster_version_owner_id BLOB` beside `roster_version`.

The same interleaving, with `memberId(O2) > memberId(O1)`:

```
R1 sees A -> gate (N,O1); B: (N,O2) >= (N,O1) -> accept -> R1 = B
R2 sees B -> gate (N,O2); A: (N,O1) >= (N,O2) -> reject -> R2 = B
```

Both relays converge on B regardless of order, as do subscribers and O1. `>=` is retained rather than `>` to preserve the existing same-owner behaviour, which is deliberate (`Subscriber.hs:3288-3291`: a preceding signed event may already have advanced `roster_version` to this blob's version) — an equal tuple from the same owner still accepts.

**Rosters must reach owners.** Today `sendRosterBlob` targets `getGroupRelayMembers` only, and the relay→subscriber broadcast was deliberately removed, so an owner never sees another owner's roster and its counter goes stale. **Relays forward `XGrpRoster` and its `BFileChunk`s to members with role `GROwner`, excluding the author** — a bounded fan-out, owners only, not all subscribers. This keeps every owner's version current so collisions stay rare, and it is what lets O1 receive (N,O2), lose the tie-break, and self-correct to the network's state.

The losing owner's change is dropped. O1 adopts B and the change it made reverts; the UI must surface this rather than let it happen silently. See §11 for the alternative.

The gap/frontier logic is unaffected: the counter remains a single shared sequence (each owner computes `local + 1`), so the tie-break introduces no gaps in `nextCompleteVersion`.

### 4.2 Link data: concurrent LSET

There is one link queue on one server, so writes serialise and the stored state never diverges — but LSET is a blind whole-object overwrite with no CAS, so O1 adding a relay concurrently with O2 changing the profile silently drops one of them. Neither owner finds out: `LINK` fires only in response to one's own `setConnShortLink` call (`Subscriber.hs:1451`).

**The published link is the source of truth for the relay set, and every non-publishing owner reconciles against it.** `syncSubscriberRelays` already does exactly this for subscribers and is currently skipped for *all* owners (`Commands.hs:1918`); it should be skipped only for the owner that just published. Combined with the periodic `LGET` the relay worker already performs (`checkRelayServedGroups`, `Commands.hs:5254`), owners converge on the published set.

Relay membership is a set with meaningful removals, so an owner republishing a stale snapshot can resurrect a relay another owner just removed. Last-writer-wins converges, but to the wrong value. See §11.

### 4.3 Owner chain ordering

`validateLinkOwners` requires each entry to be signed by root or by an entry **earlier in the list**. Since owner 2 can add owner 3 using its own key, order is load-bearing and must be persisted, not derived. Two owners concurrently appending an owner is an LSET race (§4.2): one append is lost and its invitee is left having accepted but never published — hence the worker's verify-and-republish step in §3.

## 5. Backend work (simplex-chat)

**Persist the owners chain.** Add `owner_auth_sig BLOB NULL` and `owner_auth_index INTEGER NULL` to `group_members` (migration + cabal + SQLite and Postgres). The public key is already there as `member_pub_key`, so this cannot drift from the member record by construction, and the index carries the order §4.3 requires. `groupLinkData` then **reads** the stored chain rather than deriving a singleton from `groupKeys`, which makes "each publish evicts the other owner" structurally impossible rather than merely unlikely.

- `Internal.hs:1509` `groupLinkData` — read the chain; delete the `GRKPrivate`-derived singleton and the `_ -> []` fallback.
- `Internal.hs:1458` `updatePublicGroupData`, `Commands.hs:4011` `runUpdateGroupProfile`, `Commands.hs:2713` `APIAddGroupRelays` — all gate on `memberRole' membership == GROwner` but need "can write link data" (an owner mid-handover cannot). One predicate, used at all three.
- `Commands.hs:3268` `APIAddGroupShortLink` asserts no role at all — add the same gate.
- `Commands.hs:2872` `APIMembersRole` — route `newRole == GROwner` on a channel into the §3 flow.
- `Subscriber.hs:3345` `allowCreate` — widen so an owner-signed `x.grp.mem.role` carrying a key can TOFU-create a `GROwner`; `isRosterRole GROwner == False` blocks it today, so subscribers can never materialise a new owner.
- `Commands.hs:2936` `mKey` — send the key on owner promotion, not only when a roster version is present.
- `Subscriber.hs:4195` — the "owners are already known to every member" skip is false for a newly promoted owner; disseminate their profile.
- `Subscriber.hs:1422` — `publicMemberCount > 1` assumes exactly one owner. Use a subscriber count or `> ownerCount`.
- `Commands.hs:1918` `syncSubscriberRelays` — §4.2.
- `Commands.hs:1192` — share-link owner signing requires `GRKPrivate`, so a promoted owner silently emits `ownerSig = Nothing`. Sign with the member key of any listed owner; `verifyLinkOwner` already resolves by `ownerId`.
- `Store/Groups.hs:2320` `updateRelayGroupKeys` — the `forM_ owners` loop exists but `getGroupMemberIdViaMemberId` throws for an owner the relay has no row for, aborting the transaction. Create the record — reuse `createLinkOwnerMember`, which the subscriber path already uses for exactly this.
- `Commands.hs:5254` `checkRelayServedGroups` — the relay's periodic worker already re-reads link data but binds only `relays`, discarding `owners` from the same `UserContactData`. Refresh owners here; it is the relay's only path to learning of owner changes.
- `Store/Groups.hs:1117` `getHostMember` — `firstRow` on `GCHostMember`; relays model exactly one owner.
- Roster: tie-break gate (`Subscriber.hs:3302`, `:3430`), `roster_version_owner_id` column, relays forward `XGrpRoster` + chunks to owners (§4.1).
- New durable owner-request worker + table, mirroring the relay-request worker.

## 6. simplexmq work

- `Crypto/ShortLink.hs` — extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth`; redefine `newOwnerAuth` on top of it; export. Removes the formula duplicated across the two repos.
- Persist `linkRootSigKey` — new `rcv_queues` column + migration (`AgentStore.hs:2514`). Without it the non-root branch of `validateOwners` cannot work.
- RKEY agent API — add a recipient key to a contact-link queue.
- A link-write API for an owner who does not own the queue. Prefer a standalone `setForeignLinkData` taking explicit creds over materialising a fake `RcvQueue`/`ContactConnection` on owner 2's device: the latter needs an `rcvDhSecret` owner 2 does not have, and risks owner 2 subscribing to owner 1's queue and racing it on inbound requests.

## 7. UI

The UI already pluralises owners (`ownersContributorsCountStr`, `ChatInfoToolbar.swift:134` / `ChatView.kt:1550`), already checks `members.contains { $0.wrapped.memberRole >= .owner }` for the members button (`GroupChatInfoView.swift:128` / `.kt:650`), and already has a `hasOtherOwner` escape hatch on leave (`GroupChatInfoView.swift:250` / `.kt:822`). The gap: `isOwner` is self-relative and count-blind (`membership.memberRole == .owner && membership.memberCurrent`) and is used ~30× as if it meant *the* owner.

**The UI has no model of link-write capability.** `grep groupKeys|GRKPrivate|rootKey` across `apps/ios` and `apps/multiplatform` returns zero hits — no key material on `GroupInfo` or `GroupMember`. It uses two proxies, both wrong once owners are equal:

- **`isOwner && groupLink != nil`** (`GroupChatInfoView.swift:111` — the site of `// TODO [relays] allow other owners to manage channel link (requires protocol changes to share link ownership)`; `.kt:638`). The `groupLink != nil` half is the accidental single-owner gate: only the creator has a `user_contact_links` row, so only they reach the link-management screen.
- **`isOwner`** alone for relay add/remove and relay status fetch (`ChannelRelaysView.swift:26/48/83`, `.kt:39/90/109`).

Both should be replaced by a `canManageLink` flag on `GroupInfo`, supplied by the backend. It is not equivalent to `isOwner`: an owner mid-handover, or one whose `x.grp.owner.creds` never arrived, cannot write the link. Gate link, relay and domain-claim UI on it.

- `canChangeRoleTo` (`ChatTypes.swift:3060`, `ChatModel.kt:2652`) hard-codes `[.observer, .member]` for channels. Add `.owner`, and handle that selecting it starts an async two-phase flow (pending → owner, or failed), not an immediate role change.
- New: owners section in channel info; pending-owner state.
- A role change an owner made can revert when it loses a tie-break (§4.1). Surface it.
- **iOS-only:** `GroupChatInfoView.swift:303` skips the `apiGetGroupLink` fetch unless `isOwner`; Kotlin fetches unconditionally (`ChatView.kt:403/428`). Reconcile.
- RULE-19 (`apps/ios/product/rules.md:112`) says a *sole* owner must not be able to leave. The info view honours it via `hasOtherOwner`, but the chat-list swipe/context menus drop the clause — `showLeaveGroup = … && !(groupInfo.useRelays && groupInfo.isOwner)` (`ChatListNavLink.swift:272`, `:247`; `ChatListNavLinkView.kt:319`, `:341`) — so any channel owner loses Leave in the list even when other owners exist.

## 8. Security

**Any owner can destroy the channel.** `Server.hs:1249` — `vc SRecipient _ = verifyQueue $ \q -> verifiedWithKeys $ recipientKeys (snd q)`: any recipient key authorises any recipient command, including `DEL` (destroy the queue) and `LDEL` (destroy the link data), with no per-command scoping. This is consistent with any-owner-decides, and is recorded because it is stronger than any chat-level action — the link address is embedded in the published profile, so its loss cannot be repaired by re-publishing.

**RKEY revocation is a race.** `updateKeys` replaces the whole list, so O1 can evict O2 and O2 can evict O1; last writer wins. Owner removal at the SMP level is not a clean operation.

**Chain order is load-bearing.** `validateLinkOwners` rejects an entry not signed by root or an earlier entry, so a reordered or partially-published chain makes the link data unreadable to every client and bricks joins. Publish in `owner_auth_index` order and validate the chain locally before `LSET` — the agent rejects a bad chain anyway, but as `CMD PROHIBITED`, not a legible chat error.

**Creator anonymity is weaker than the overview states.** It claims all owners are indistinguishable "provided multiple owners were signed by the root key". Once owner 2 signs owner 3 with its own key, the chain reveals who delegated to whom, and root-signed entries identify the creator's cohort. Qualify the claim in the docs.

## 9. Tests

Channel tests live under `describe "channels"` (`tests/ChatTests/Groups.hs:254`, ~70 tests); `-m "channels"` selects all of them but **not** `chatRelayTests` (`tests/ChatTests/ChatRelays.hs:33`), where the owner-signature suite ("share channel card", `:243-380`) lives.

Infrastructure is close: `memberJoinChannel'` already takes `owners :: [TestCC]` and fans out the "introduced … in the channel" assertion; `Groups.hs:9840` already passes a 2-element list. Gaps: `promoteChannelMember` (`Groups.hs:8773`) hardcodes one owner and one relay; every `prepareChannel*` helper takes one `owner` and registers relays under that owner's `/relays` (precedent for a second registration at `ChatRelays.hs:351`, which is why `setupRelay` returns the link); named profiles run out at `frank`, so **2 owners + 1 relay + 3 subscribers** is the ceiling without adding profiles; the custom `it` imposes a **90s timeout** (`Utils.hs:94`) that 6 clients plus the customary `threadDelay 1000000` between roster steps will approach.

Patterns to reuse: `<###` (`getInAnyOrder`, `Utils.hs:449`) with `EndsWith` for lines whose actor prefix is nondeterministic (an unresolved profile renders by member-id hash); `withCCTransaction` SQL for state that produces no console output (`checkMemberRole`, `Groups.hs:9846`) — the served roster arrives async and silently. `testChannelOwnerKeyAfterLinkUpdate` (`Groups.hs:9545`) is the owner-key template, and its assertion (`COUNT(1) … member_role = 'owner' AND member_pub_key IS NOT NULL` → `[[1]]`) is what must become `[[2]]`.

Cases: owner 2 promoted, verified by an existing subscriber (TOFU path) and by a later joiner (link path); owner 2 posts as the channel; owner 2's admin message verified by a subscriber that only ever saw owner 1; owner 2 adds a relay; owner 2 adds owner 3 (chain order); owner 1 offline throughout. The §4.1 race needs a deterministic test: two owners, two relays, same version, forced interleaving, asserting both relays and all subscribers converge on the same roster and the losing owner self-corrects. Adversarial, modelled on `ChatRelays.hs:277-287`: replaying an owner's `OwnerAuth`; a relay substituting an owner key; a replayed `x.grp.owner.acpt`; a chain published out of order (must fail closed, and fail *before* `LSET`).

## 10. Docs

`channels-overview.md` §Governance — move v7 to current; qualify the creator-anonymity claim; record the §4.1 tie-break and the §4.2 residual. `channels-protocol.md` — new §Owner addition, and the `x.grp.owner.*` events in the signing table. Closes four TODOs: `Internal.hs:1508`, `Subscriber.hs:1426`, `Subscriber.hs:1451`, `Groups.hs:2320`.

## 11. Open questions

**Owner removal.** Out of scope here, and not yet designed. Three things make it harder than it looks: removing a chain entry invalidates every owner it transitively signed (§4.3); SMP-level revocation is a mutual-eviction race (§8); and there is **no last-owner protection anywhere** in the backend — `APIMembersRole` blocks only `selfSelected`, so two owners can already demote each other into an ownerless, unrecoverable channel, and RULE-19 is UI-only. The backend last-owner guard is worth adding regardless of how removal is designed.

**Lost roster updates (§4.1).** Either the losing owner accepts the loss and adopts the winning roster, or it re-applies its pending delta on top and broadcasts at N+1. The latter converges to the union and terminates — the winner does not retry, because its change survived — at the cost of persisting the pending delta.

**Relay resurrection (§4.2).** Either accept that a stale republish can restore a removed relay, or make relay removal element-wise (tombstones, or a version on the link data). Owner removal has the identical shape, so the two are best decided together.
