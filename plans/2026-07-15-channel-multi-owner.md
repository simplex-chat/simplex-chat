# Channel multi-owner (v7: any-owner-decides)

Status: plan for review. Nothing implemented.

Goal: a channel can have N owners, each able to act independently, per `docs/protocol/channels-overview.md` §Governance ("Near-term (v7): Multiple owners, any-owner-decides").

## 1. What a channel owner actually is — three independent authorities

The word "owner" conflates three capabilities that live in different places. Every design question below is really "which of these does owner 2 get?"

| # | Authority | Enforced by | Held today by |
|---|---|---|---|
| A | **Sign as owner** — subscribers accept your admin messages | `OwnerAuth` chain in link data, verified against root key | creator only (published as a 1-element list) |
| B | **Write link data** — relays, profile, member count, owners list | SMP: `LSET` is `Command Recipient`, authorised by the link queue's recipient key | creator only (owns the queue) |
| C | **Sign the roster** — member/moderator/admin roles | single per-group `roster_version` counter | any `GROwner` (already), but see D3 |

A is the identity. B is the link. C is the roster. They are separable, and B is the expensive one.

## 2. Findings that shape the design

**The protocol layer is already built for this; the client is not.** `UserContactData.owners :: [OwnerAuth]`, `validateLinkOwners` (chain: each entry signed by root *or an earlier entry*), and `decryptLinkData` (accepts data signed by root *or any listed owner*) all exist in simplexmq and are multi-owner-correct. `ShortLinkCreds.linkRootSigKey` exists with the comment *"in case the current user is not the original owner"* — i.e. designed for exactly this.

**SMP already supports multi-writer links.** `RKEY :: NonEmpty RcvPublicAuthKey -> Command Recipient` and `recipientKeys :: NonEmpty` landed in commit `b7a95422` *"smp server: short links and owners for channels"* — "support multiple rcv keys", with tests. It is at `shortLinksSMPClientVersion = 4 = currentSMPClientVersion`, so every server that can host a channel already accepts it. **The agent never uses RKEY** (`grep RKEY` in `Agent.hs`/`Client.hs` → nothing). This is the intended mechanism for authority B, shipped server-side and unwired client-side.

**Two blockers in simplexmq:**
- `AgentStore.hs:2514` — `linkRootSigKey = Nothing` on every load, with `-- TODO linkRootSigKey should be stored in a separate field`. There is no column. So the non-root branch of `validateOwners` is currently dead code.
- `Crypto/ShortLink.hs:80` — `newOwnerAuth` *generates the key pair locally*. Adding a remote owner needs to sign an externally supplied public key. The signing formula `sign(signingKey, ownerId <> encodePubKey ownerKey)` is **duplicated** in chat at `Internal.hs:1519`; extract `mkOwnerAuth` and use it from both.

**The single-owner assumption is concentrated in one function.** `groupLinkData` (`Internal.hs:1509`, already carries `-- TODO [relays] owner: set owners on updating link data (multi-owner)`) rebuilds `owners` on every publish as a singleton containing *only the publishing user*, and as `[]` unless the user holds `GRKPrivate`. Consequences: (a) a second owner is never published; (b) if two owners could both publish, **each publish evicts the other**; (c) a non-root owner would publish an empty owners list, making the link data unreadable to everyone.

**Owners are deliberately not in the roster.** `isRosterRole = GRMember | GRModerator | GRAdmin` (`Internal.hs:1281`, comment: *"owners are on the link, not the roster"*). Link data is the sole authority for owner keys (`Groups.hs:3533`: *"Role and key stay owner-authoritative … so taking either from an in-band relayed intro would let a compromised relay substitute them"*). This is correct and must be preserved — but it means **existing subscribers have no way to learn a new owner**, because `updateGroupFromLinkData` only carries profile + member count (never `owners`) and is only called on connect/preview paths. Hence the explicit role-broadcast step.

**Live bug, independent of this feature.** `APIMembersRole gId [m] GROwner` on a channel is **not rejected by the backend** — it is blocked only in the iOS/Kotlin pickers (`ChatTypes.swift:3060`, `ChatModel.kt:2652`). Via CLI/API it produces a broken state: `isRosterRole GROwner == False`, so the observer→owner path skips the owner-only guard, sends no key (`mKey m = if isJust rosterVer then …` — `Commands.hs:2936`, and `rosterVer` is `Nothing` for a non-roster role), publishes nothing to the link, and leaves the promoted device with `membership.memberRole == GROwner` so `updatePublicGroupData` (`Internal.hs:1458`) calls `getGroupLink` on every `x.grp.mem.new` and throws `SEGroupLinkNotFound`. Fix now, regardless of the rest.

## 3. Decisions for you

**D1 — Include authority B (link writes) for owner 2?** *Recommend: yes, staged (§8).* Without it, "multiple owners" does not deliver the property the overview claims for it: *"Loss of all owner devices is the catastrophic event … Multiple owners and backups mitigate this risk"* and design objective 5 (relay transience). If owner 1 is lost, no relay can ever be added again and the channel dies with its last relay. Owner 2 would be an owner who cannot save the channel.

**D2 — Must owner 2 already be a channel member?** *Recommend: yes.* Their key must already be known to relays and subscribers, and the relay connections they will publish through only exist for members (a subscriber creates `GRRelay`/`GCHostMember` rows, so `getGroupRelayMembers` already returns them for a promoted subscriber — the transport works unchanged). Non-member → invitation carries the channel link, they join, then accept. This mirrors the existing relay-invitation flow exactly.

**D3 — Roster concurrency (authority C).** Two owners both compute `rosterVersion + 1` (`Internal.hs:2338`) and recipients accept `v >= gate` (`Subscriber.hs:3302`), so equal versions silently clobber. Worse, owner 2 never *sees* owner 1's versions: `sendRosterBlob` targets `getGroupRelayMembers` only, so rosters go owner→relays and are served to joiners, never to other owners. The overview already defers the real fix ("A planned extension will record role changes as a linearly ordered signed roster log with consistent sequencing across all owners"). Options:
  - **(a) Root owner only signs rosters** in v1; other owners cannot change member/mod/admin roles. Simple, honest, but visibly undercuts "any-owner-decides".
  - **(b) Forward rosters to owners + `max(seen)+1` + tie-break by ownerId.** Converges deterministically; concurrent edits still lose one update silently. *Recommended* — it keeps the v7 promise and the residual race is the one the overview already says is deferred.
  - (c) Full linearised log — out of scope.

**D4 — Can owner 2 add owner 3?** *Recommend: yes* (implied by any-owner-decides). Cost: `validateLinkOwners` is **order-dependent** (an entry must be signed by root or an *earlier* entry), so the chain order must be persisted, not derived. If only the root owner could add owners, every entry would be root-signed and order would be free.

**D5 — Owner removal: in or out of v1?** *Recommend: out, and blocked.* Removing an owner from the middle of the chain invalidates every owner it transitively signed. Also note there is **no last-owner protection anywhere** today (`APIMembersRole` blocks only `selfSelected`), so two owners can mutually demote each other into an ownerless, unrecoverable channel. At minimum add the last-owner guard in v1.

## 4. Protocol

Two new events, both `requiresSignature`, both **sent over the direct owner↔owner contact connection — never through a relay**. This is a hard requirement, not a preference: support scope is not E2E encrypted (the overview lists "E2E encrypted support scope" as future work), so routing owner onboarding through a relay would hand the relay the material it needs to attack the link, directly contradicting design objective 3 ("No possibility for a relay to impersonate an owner").

```
x.grp.owner.inv   { groupLink, memberRole, ... }
x.grp.owner.acpt  { ownerKey, linkRcvKey? }
```

- `memberRole` is carried in the invitation even though v1 auto-accepts, so a future release can let the invitee accept/reject a named role without a wire change (your requirement).
- `ownerKey` is the invitee's existing channel member public key — the same key that already signs their messages and is already known to relays and subscribers. No new signing key.
- `linkRcvKey` is a fresh SMP recipient auth public key the invitee generates for authority B. Define and populate it from day 1 even if RKEY lands in stage 2 (optional field → no protocol revision later).

Owner 1 on acceptance, in this order, driven by a durable worker (mirror `runRelayRequestWorker` — the flow is multi-step, cross-network and must survive a crash mid-way):

1. `RKEY` — add `linkRcvKey` to the link queue's recipient keys. *(stage 2)*
2. `LSET` — publish `owners = [oa1, …, oaN, mkOwnerAuth memberId2 ownerKey rootPrivKey]`.
3. `x.grp.mem.role memberId2 GROwner (Just ownerKey) …` to relays → forwarded to subscribers.

LSET before the role broadcast, so any joiner in the window learns owner 2 from the link (the authority) rather than by TOFU.

## 5. Backend work (simplex-chat)

**Persist the owners chain — the core change.** Add `owner_auth_sig BLOB NULL` and `owner_auth_index INTEGER NULL` to `group_members` (migration + cabal + both SQLite and Postgres). The public key is already there as `member_pub_key`, so this cannot drift out of sync with the member record by construction, and the index gives `validateLinkOwners` its required order. Then rewrite `groupLinkData` to **read** the stored chain instead of deriving a singleton from `groupKeys`. This is what makes "each publish evicts the other owner" structurally impossible rather than merely unlikely, and it is required even in stage 1 (owner 1 must publish both entries).

- `Internal.hs:1509` `groupLinkData` — read chain; remove the `GRKPrivate`-derived singleton and its `_ -> []` fallback.
- `Internal.hs:1458` `updatePublicGroupData`, `Commands.hs:4011` `runUpdateGroupProfile`, `Commands.hs:2713` `APIAddGroupRelays` — these gate on `memberRole' membership == GROwner`, which is authority A, but what they need is authority B. Introduce one predicate (`canWriteLinkData`) and use it at all three, so the proposition being asserted is the one that is actually required. In stage 1 it is "holds the root key"; in stage 2 "has link creds".
- `Commands.hs:3268` `APIAddGroupShortLink` asserts **no role at all** — add the same gate.
- `Subscriber.hs:3345` `allowCreate` — widen so an owner-signed `x.grp.mem.role` carrying a key can TOFU-create a `GROwner` (currently `isRosterRole GROwner == False` blocks it, so subscribers can never materialise a new owner).
- `Commands.hs:2936` `mKey` — send the key on owner promotion, not only when a roster version is present.
- `Subscriber.hs:4195` — the "owners are already known to every member" skip is false for a *newly promoted* owner; disseminate their profile.
- `Subscriber.hs:1422` — `publicMemberCount > 1` assumes exactly one owner (self-documented TODO). Use a subscriber count or `> ownerCount`.
- `Commands.hs:1918` `syncSubscriberRelays` — skipped for all owners; must be skipped only for the *publishing* owner, else a non-publishing owner's relay list drifts.
- `Commands.hs:1192` — share-link owner signing requires `GRKPrivate`, so owner 2 silently emits `ownerSig = Nothing`. Sign with the member key of any listed owner (`verifyLinkOwner` already resolves by `ownerId`).
- `Store/Groups.hs:2320` `updateRelayGroupKeys` — the `forM_ owners` loop already exists but `getGroupMemberIdViaMemberId` **throws** for an owner the relay has no row for, aborting the whole transaction (self-documented TODO). Create the record instead — reuse `createLinkOwnerMember`, which the subscriber path already uses for exactly this.
- `Commands.hs:5254` `checkRelayServedGroups` — the relay's periodic worker already re-reads link data but binds only `relays` and discards `owners` from the very same `UserContactData`. Refresh owners here; this is the relay's only path to learning about owner changes.
- `Store/Groups.hs:1117` `getHostMember` — `firstRow` on `GCHostMember`; relays model exactly one owner. Needed for D4/D5 only.
- Reject `newRole == GROwner` in `APIMembersRole` for channels (§2 live bug) and route owner promotion exclusively through the new flow. Add last-owner protection (D5).

## 6. simplexmq work

Stage 1:
- `Crypto/ShortLink.hs` — extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth`; redefine `newOwnerAuth` on top of it; export. Kills the formula duplicated across the two repos.

Stage 2 (authority B):
- Persist `linkRootSigKey` — new `rcv_queues` column + migration; fixes `AgentStore.hs:2514`. Without it the non-root owner branch of `validateOwners` cannot work.
- RKEY agent API — add the recipient key to a contact-link queue.
- A **least-privilege** link-write API for an owner who does not own the queue. Prefer a standalone `setForeignLinkData` taking explicit creds over materialising a fake `RcvQueue`/`ContactConnection` on owner 2's device: the latter needs an `rcvDhSecret` owner 2 must not have, and risks owner 2 subscribing to owner 1's queue and racing it on inbound requests.

## 7. UI

The UI is further along than the backend: it already pluralises owners (`ownersContributorsCountStr`, `ChatInfoToolbar.swift:134` / `ChatView.kt:1550`), already checks `members.contains { $0.wrapped.memberRole >= .owner }` to decide whether to show the members button (`GroupChatInfoView.swift:128` / `.kt:650`), and already has a `hasOtherOwner` escape hatch on leave (`GroupChatInfoView.swift:250` / `.kt:822`). The gap is that `isOwner` is **self-relative and count-blind** (`membership.memberRole == .owner && membership.memberCurrent`) and is used ~30× as if it meant *the* owner.

**The structural problem: the UI has no model of authority B.** Grep for `groupKeys|GRKPrivate|rootKey` across `apps/ios` and `apps/multiplatform` returns **zero hits** — neither `GroupInfo` nor `GroupMember` carries any key material. The UI cannot distinguish a root-key holder from a promoted owner, so it currently uses two different proxies for it, both wrong for multi-owner:

- **`isOwner && groupLink != nil`** (`GroupChatInfoView.swift:111` — the site of `// TODO [relays] allow other owners to manage channel link (requires protocol changes to share link ownership)`; `.kt:638`). The `groupLink != nil` half is the accidental single-owner gate: only the creator has a `user_contact_links` row, so only they get the link-management screen; everyone else falls to a read-only QR + share sheet.
- **`isOwner`** alone for relay add/remove and relay status fetch (`ChannelRelaysView.swift:26/48/83`, `.kt:39/90/109`), which a co-owner would pass while the backend call fails.

So: **add an explicit `canManageLink` (authority B) to `GroupInfo` from the backend** and gate link/relay/domain-claim UI on it, leaving `isOwner` to mean authority A. That replaces both proxies with the proposition actually being asserted, and it is what lets stage 1 ship (co-owner sees the read-only path, no error) and stage 2 flip one flag.

Also:

- `canChangeRoleTo` (`ChatTypes.swift:3060`, `ChatModel.kt:2652`) hard-codes `[.observer, .member]` for channels — and even the commented-out "restore" line excludes `.owner`. Owner promotion must **not** be added to this picker; it is a distinct flow (invite → accept), not a role change.
- New: invite-owner entry point (from a contact or a member's profile), pending/accepted/failed state, owners section in channel info.
- **iOS-only:** `GroupChatInfoView.swift:303` skips the `apiGetGroupLink` fetch entirely unless `isOwner`; Kotlin fetches unconditionally (`ChatView.kt:403/428`). Reconcile when adding `canManageLink`.
- **Existing bug this will expose:** RULE-19 (`apps/ios/product/rules.md:112`) says a *sole* owner must not be able to leave. The info view honours it via `hasOtherOwner`, but the chat-list swipe/context menus drop that clause entirely — `showLeaveGroup = … && !(groupInfo.useRelays && groupInfo.isOwner)` (`ChatListNavLink.swift:272`, `:247`; `ChatListNavLinkView.kt:319`, `:341`) — so *any* channel owner loses Leave in the list even when other owners exist. Fix alongside; and note RULE-19 is UI-only, with no backend last-owner guard (D5).

## 8. Staging

**Stage 1 — identity.** Owners chain persisted and published; owner invite/accept over the direct connection; role propagation to relays and subscribers; relay creates owner records and refreshes them; live-bug fix; last-owner guard. Owner 2 can publish, post as the channel, moderate, and change roles. Link writes stay with owner 1, explicitly gated with a clear error rather than a silent empty-owners publish. All chat-side; no simplexmq schema change.

**Stage 2 — link authority.** simplexmq: `linkRootSigKey` persistence, RKEY, foreign-link write. Owner 2 gains relay and profile-in-link management. Delivers the loss-of-owner-1 survivability that motivates the feature.

## 9. Security notes

**Granting link-write grants link-destroy.** `Server.hs:1249` — `vc SRecipient _ = verifyQueue $ \q -> verifiedWithKeys $ recipientKeys (snd q)`: *any* recipient key authorises *any* recipient command, including `DEL` (destroy the queue) and `LDEL` (destroy the link data). There is no per-command key scoping. Stage 2 therefore gives every owner the power to irrecoverably destroy the channel link, which is strictly worse than any chat-level action because the link address is embedded in the published profile. Consistent with "any owner can independently make any administrative decision", but it should be a conscious choice. A scoped link-writer key class would be an SMP-server change; flagging as possible future work.

**RKEY revocation is a race.** `updateKeys` replaces the whole list, so owner 1 can evict owner 2 and owner 2 can evict owner 1; last writer wins. Owner removal at the SMP level is therefore not a clean operation (reinforces D5).

**Owner onboarding must not traverse a relay** — see §4.

**Chain order is load-bearing.** `validateLinkOwners` rejects an entry not signed by root or an earlier entry. A reordered or partially-published chain makes the whole link data unreadable to every client, which bricks joins. The stored `owner_auth_index` must be published in order, and the publish path should validate the chain locally before `LSET` (the agent will reject it anyway via `validateOwners`, but as `CMD PROHIBITED`, not a legible chat error).

**Creator anonymity.** The overview claims all owners are indistinguishable "provided multiple owners were signed by the root key". If owner 2 adds owner 3 with owner 2's key (D4), the chain reveals who delegated to whom, and root-signed entries identify the creator's cohort. Worth stating explicitly in the docs rather than leaving the claim unqualified.

## 10. Tests

Channel tests live under `describe "channels"` (`tests/ChatTests/Groups.hs:254`, ~70 tests); `-m "channels"` selects all of them. Note `-m "channels"` does **not** cover `chatRelayTests` (`tests/ChatTests/ChatRelays.hs:33`) — the owner-signature suite ("share channel card", `ChatRelays.hs:243-380`) lives there and must be run separately.

The infrastructure is closer than expected: `memberJoinChannel'` already takes `owners :: [TestCC]` and fans out the "introduced … in the channel" assertion, and `Groups.hs:9840` already passes a 2-element list. Gaps to close first:

- `promoteChannelMember` (`Groups.hs:8773`) hardcodes a single owner and single relay.
- Every `prepareChannel*` helper takes one `owner` and registers relays under that owner's `/relays`. A second owner needs its own registration — precedent at `ChatRelays.hs:351` (re-registering a relay short link under another user), which is why `setupRelay` returns the link.
- Named profiles run out at `frank`, so **2 owners + 1 relay + 3 subscribers** is the practical ceiling without adding profiles.
- The custom `it` imposes a **90s timeout** (`tests/ChatTests/Utils.hs:94`); multi-owner tests with 6 clients and the customary `threadDelay 1000000` between roster steps will approach it.

Assertion patterns to reuse: `<###` (`getInAnyOrder`, `Utils.hs:449`) with `EndsWith` for lines whose actor prefix is nondeterministic (a subscriber that hasn't resolved a profile renders by member-id hash); and direct `withCCTransaction` SQL on the receiver's DB for state that produces no console output (`checkMemberRole`, `Groups.hs:9846`) — the served roster arrives async and is silent. `testChannelOwnerKeyAfterLinkUpdate` (`Groups.hs:9545`) is the existing owner-key template and its DB assertion (`COUNT(1) … member_role = 'owner' AND member_pub_key IS NOT NULL` → `[[1]]`) is exactly the thing that must become `[[2]]`.

Cases: owner 2 promoted, verified by an existing subscriber (TOFU path) and by a later joiner (link path); owner 2 posts as the channel; owner 2's admin message verified by a subscriber that only ever saw owner 1; concurrent link publishes by both owners; concurrent roster edits (D3); chain order after owner 3 is added by owner 2; owner 1 offline (stage 2); rejection of the direct `APIMembersRole … GROwner` path. Adversarial, modelled on `ChatRelays.hs:277-287`: a non-owner replaying an owner's `OwnerAuth`; a relay substituting an owner key; a chain published out of order (must fail closed, and must fail *before* `LSET`).

## 11. Docs

`channels-overview.md` §Governance — move v7 from "planned" to current, and qualify the creator-anonymity claim. `channels-protocol.md` — new §Owner addition, and the `x.grp.owner.*` events in the signing table. Remove the four stale TODOs this closes: `Internal.hs:1508`, `Subscriber.hs:1426`, `Subscriber.hs:1451`, `Groups.hs:2320`.
