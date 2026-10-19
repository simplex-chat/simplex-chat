# Plan: Public groups via relays (unified)

Date: 2026-05-26

This plan is self-contained. It supersedes `2026-05-08-public-groups-via-relays.md` and folds in the privileged-roster mechanism understood since. Implementers should work from this document alone. File:line anchors are current as of this date — confirm before editing.

## Overview

Channels (shipped) are relay-mediated groups where the relay forwards content from owners only; subscribers are pinned to `GRObserver` and cannot post. Public groups are the second value of the same two-axis design: same wire, same transport, but every member can post, and there are moderators/admins who can act.

| `useRelays` | `groupType` | Name | Posting | Notes |
|---|---|---|---|---|
| `false` | (none) | Secret group | all members | today's P2P full-mesh group |
| `true` | `GTChannel` | Channel | owners only | shipped; subscribers anonymous to each other |
| `true` | `GTGroup` | **Public group** | every member | **new**; member-to-member DMs deferred |
| `true` | `GTUnknown _` | (refuse) | — | newer-client link seen by older client → refuse to join |

Three concepts, kept distinct: **transport** = `useRelays` (topology, batch, signatures, delivery); **governance** = `groupType` (who may post, member affordances); **joiner role** = the default role new joiners get, set by the owner on the signed profile.

Two things make public groups work and neither exists today:

1. **The joiner role must come from the owner-signed profile**, not a relay-side global config — otherwise relays disagree on the default role. (Section 2.)
2. **Members must learn who the moderators/admins are — their identity, signing key, and role — in a way a relay cannot forge.** Today a relay can fabricate a moderator (Section 1, Problem). This is the load-bearing piece and ships first.

`GTGroup`, `PublicGroupProfile`, `useRelays'`, and the relay/signing/forwarding machinery already exist (anchors below). The work is additive.

---

# Section 1 — Privileged roster (`XGrpRoster`)

This is a **general relay-group mechanism**: public groups use it now; channels inherit it for their multi-owner/moderator future. It is the first, self-contained task.

## 1.1 Problem and trust model

Owners are trusted because their keys come from the **link**, never the relay: on join, `createLinkOwnerMember` (`Store/Groups.hs:3072`) writes each owner's `member_pub_key` from the link's `OwnerAuth` chain, validated against `publicGroupId == sha256(rootKey)`. `xGrpMemIntro` even nulls the key when `mRole == GROwner` (`Subscriber.hs:3029`): *"owner key must only come from link data, not from relay intro."*

Non-owner privileged members have no such anchor. Today `xGrpMemIntro` **keeps** the relay-asserted key for `GRModerator`/`GRAdmin`, and `introduceInChannel` (`Internal.hs:1165`) introduces all of `getGroupModerators` (which returns mod+admin+owner, `Store/Groups.hs:1190`). So a malicious relay can assert "X is a moderator, here is X's key," and the subscriber will then trust the relay-chosen key to verify X's signed administrative actions (`XGrpMemDel`, `XGrpMemRestrict`, `XGrpMemRole`). The `when (memRole > GRMember)` gate in `xGrpMemNew` (`Subscriber.hs:2957`) blocks the *dissemination* path but not the *join-time intro* path — the protection is half-applied. Dormant for channels (single-owner broadcast), activated by public groups.

**Conclusion:** a non-owner privileged member's `(memberId, name, key, role)` must be **owner-signed**, exactly like owners are link-signed. That is the roster.

## 1.2 Wire event and signing

New event `XGrpRoster` (add to `ChatMsgEvent`, `Protocol.hs:422`), JSON-encoded, carrying:

- `version :: Word32` — monotonic, from 0.
- `roster :: [{ memberId, name, key, role }]` — the complete current privileged set, `role ∈ {GRModerator, GRAdmin}`. `name` is a display name only (to avoid ugly "unknown member" records, as `XGrpMsgForward` already carries one). Owners are **not** in the roster.

Add `XGrpRoster_` to `requiresSignature` (`Protocol.hs:1231`) ⇒ `True`. This makes the owner sign it via the existing `groupMsgSigning` (`Internal.hs:1962`, binding `CBGroup <> (publicGroupId, ownerMemberId)`, key `groups.member_priv_key`) and makes recipients require a valid owner signature via `withVerifiedMsg` (`Subscriber.hs:3461`). No new crypto.

**The handler MUST assert the resolved author is an owner** (`memberRole' author == GROwner`). `withVerifiedMsg` verifies the signature against the *author's* key, and the relay chooses `fwdSender` — so without this assertion a relay could route a roster as a member whose key it controls and the signature would verify. Owners exist on recipients only via the link `OwnerAuth` chain, so a relay can neither fabricate an owner nor sign as one. This assertion is the crux of the roster's integrity.

## 1.3 Authoritative model — versioned snapshot, latest-wins, TOFU keys

Each `XGrpRoster` is the complete current privileged set. Recipients treat the highest-version valid roster as authoritative for *who is privileged and their keys*; absence from the newest accepted roster means *not privileged* (reverts to the joiner default unless an accompanying `XGrpMemRole` sets a specific role — see 1.6). This is self-healing: a member who missed one change gets the full current state on the next roster.

**Key handling is trust-on-first-use, pinned per `memberId`** (per entry):

- `memberId` unknown, or known without a key → store the key (first sight, from the owner). Set name/role.
- `memberId` already has a key:
  - same key → fine; update name/role.
  - **different key → error.** Never overwrite; keep the old key; surface a suspicious-roster event.

There is **no in-place key rotation**: a genuine re-key is modeled as a *new member* — the owner removes the old `memberId` (`XGrpMemDel` + roster drop) and adds a new `memberId` with the new key. Consistent with SimpleX's no-mutable-identity stance, and with the `xGrpMemNew` rule in 1.5.

**Anti-replay / rollback.** The relay cannot forge a signed roster but can replay an older one.

- The current roster `version` is anchored in the owner-controlled link mutable data (which already holds `OwnerAuth`, profile, subscriber count). The relay cannot forge it. A roster change that bumps the version also updates link data. **Status:** the write side is implemented; the join-time **read/detect** is deferred — comparing the anchor against the relay-served roster at join is racy (the forwarded roster may not have arrived yet → false positives), so correct staleness detection must be triggered by roster arrival, not at join. The residual new-joiner rollback gap below stands; the hard anti-replay (member + relay) is in place.
- **Existing members** reject any roster with `version` below the highest already accepted — full anti-replay for them.
- **New joiners** process the latest version the relay actually serves, even if it lags the link anchor, so honest relay propagation lag never blocks a join. The anchor is used for staleness *detection*, not a hard gate, in v1.
- **Documented residual gap:** a stale/malicious relay can serve an old-but-valid roster to a brand-new joiner. Documented in `channels-overview.md` with future mitigations: escalate verification to an owner, or have the client compare the relay's version to the link anchor and refuse/retry above a staleness threshold.

## 1.4 Cap on the privileged set

Bound the privileged set so the signed roster always fits one message — never paginate. A hard **cap on moderators + admins** (owners are on the link, not counted), enforced **at promotion time** on the owner: refuse to elevate beyond the cap with a clear error, so the roster is always constructible as one signed message.

Derive the number from the single encoded-message budget (verify the exact constant — the encoded-message-length limit minus signature + JSON overhead) divided by worst-case entry size. With `{memberId, name, key, role}` entries this is comfortably in the tens-to-~100 range; pick the final value from the measured worst-case entry.

## 1.5 Remove the dissemination gates; gate on the roster instead

Because the relay forwards the roster on join **before anything else**, a privileged member's key/role is owner-established before any relay-asserted introduction arrives. So the relay may now disseminate privileged members' full profiles like any other member, and the gates come out:

- **Remove** the `when (memRole > GRMember)` throw in `xGrpMemNew` (`Subscriber.hs:2957`).
- **Remove** the forward-side `memberRole' s <= GRMember` filter in `sendBodyToMembers` (confirm exact site in `Subscriber.hs`).
- **Replace** with a roster check in `xGrpMemNew`: for an announcement of a privileged role, require that a member record with that `memberId` already exists **with that privileged role** (roster-established). If found → accept the **profile** update only; **never overwrite `key`, `memberId`, or `role`** (roster-authoritative). If not found → reject (a relay conjuring a privileged member not in the roster).

`introduceInChannel` forwards the cached roster to the new member first, then proceeds; it may still announce the newcomer to moderators and maintain the relations vector. The per-mod `XGrpMemIntro` carrying keys is no longer the trust path for privileged members.

## 1.6 Delivery — what is sent, when, to whom

Two orthogonal axes, and the roster owns only one:

- **Axis A — privileged set + keys** (who is mod/admin, their key/name/role): owned by the roster.
- **Axis B — group-membership lifecycle** (removed / restricted / left): owned by `XGrpMemDel` / `XGrpMemRestrict` / `XGrpLeave`, unchanged, applies to everyone.

Dispatch by whether an operation touches the {moderator, admin} set (the *roster roles*). Owner is not a roster role — promotion to/from owner uses `XGrpMemRole` (+ link `OwnerAuth`), never the roster.

**`APIMembersRole` (role change, possibly batched):**

- Emit `XGrpMemRole(M, target)` for each affected member exactly as today — this conveys the exact target role for any role, including owner and specific ≤member roles.
- **Additionally** build and **broadcast** the full signed roster (version++) **iff the {mod, admin} set changed** (any member entered, left, or moved within mod/admin).
- A mixed batch fires both. Example: target=member over `[moderator M, observer O]` → `XGrpMemRole` for both (exact roles) **and** a roster (M left the set). The promotion case `XGrpMemRole(M, mod)` + roster is mildly redundant on the role field and harmless; the key comes only from the roster.

The broadcast reuses the existing owner-admin-event forwarding (`shouldForward = isUserGrpFwdRelay gInfo && not forwarded`, `Subscriber.hs:3191`). Privileged-set changes are rare administrative events, so this is on the order of an `XGrpInfo`/`XGrpPrefs` broadcast — not per-message. The broadcast roster is the **self-healing** mechanism: a member who missed a prior `XGrpMemRole` is corrected by the snapshot.

**`XGrpMemDel` (removal):** broadcast `XGrpMemDel` as today (it neutralizes the member for existing members). If the removed member was privileged, the owner sends a refreshed roster (version++), which the relay broadcasts like any other version bump (see below).

**Relay broadcast rule — always broadcast on a strict version bump.** A newer-version roster is applied, cached, and broadcast to current members, uniformly — promotion, key/role change, demotion, or privileged removal. We do **not** try to make removal cache-only: a demotion (member stays in the group) is indistinguishable from a deletion at the roster-diff level, so suppressing the broadcast would silently drop the self-healing the spec requires for role changes. The only cost is one redundant roster broadcast alongside `XGrpMemDel` on the rare deletion of a privileged member — and even there the broadcast is not waste, since it self-heals the privileged-set side if the `XGrpMemDel` was lost. (This supersedes an earlier "cache-only on deletion" idea, which could not be implemented without either a wire flag or a fragile demotion-vs-deletion diff.)

**On relay add:** the owner sends the current roster to the new relay so it can serve joiners.

**Joiners:** the relay forwards the cached roster at join (1.5).

**Short offline gaps** are covered by ordinary queued delivery: the role-change roster broadcast sits in the member's SMP queue (FIFO) ahead of any later moderator events, so it is processed first on reconnect.

**Quota-blocked catch-up.** A member offline long enough to fill its queue causes the relay to be quota-blocked — the broadcast may never have been enqueued, so naive queueing would leave the member without the current roster, rejecting moderator events indefinitely. Fix: when the queue drains, the relay **sends the current cached roster ahead of the resumed backlog**, so the member holds the current privileged set before processing the events it couldn't verify.

The hook is confirmed: QCONT is delivered to the **sender** when the recipient drains (`simplexmq Agent.hs:3402`), and the relay receives it per subscriber in the group-member connection handler (`Subscriber.hs:1215` — `continueSending` + `sendPendingGroupMessages user gInfo m conn`, with `gInfo`/`m`/`conn` in scope; a relay→subscriber connection is a group-member connection). Implementation must ensure **roster-first ordering** relative to both the agent-level `continueSending` flush and the re-driven delivery tasks, and gate the extra send on a per-member "delivered roster version" so it fires only when the member is behind.

The roster is **never** delivered through the profile-dissemination prepend. That path (`member_relations_vector` → `XGrpMemNew`) carries **profiles** only; with the gate removed (1.5), a privileged member's profile disseminates through it like any other member's, but only after the roster has established their key/role. Profile via prepend, key/role via roster — orthogonal, no double-prepend.

## 1.7 Relay-side cache (the one new storage pattern)

Relays already forward signed bytes verbatim (`encodeFwdElement` `Batch.hs:106`, `verifiedMsgParts` `Protocol.hs:1445`; `messages.msg_chat_binding` + `msg_signatures`; reconstructed in `toTask` `Delivery.hs:154`). What does **not** exist is "store the latest roster and re-emit to joiners" — `sendHistory` (`Internal.hs:1207`) reconstructs content and does *not* preserve signatures, so it is not a template.

Add a small per-group cache holding the latest signed roster message bytes, plus the roster `version` as a **separate column** alongside them (so the relay compares versions without re-parsing the blob). On receiving `XGrpRoster` from an owner the relay: verifies the owner signature; **checks `version` strictly greater than the cached version** (lower → reject as rollback; equal → idempotent no-op); then updates its own member-role records, overwrites the cache + stored version, and (for a role-change-origin roster) creates a delivery task to all current members. On join, it forwards the cached bytes verbatim.

The relay-side version check protects an **honest** relay's cache from being rolled back by a replayed signed roster — which in turn protects every joiner that relay serves. It does not constrain a **malicious** relay (it controls its own cache); that remains the documented new-joiner residual gap (1.3), bounded by the member-side check (1.3) and the link version anchor.

## 1.8 Races and tests

- **Promotion vs. action ordering.** A newly-promoted mod could act before its roster reaches a recipient ⇒ `RGEMsgBadSignature`. Covered for MVP by causal ordering (the roster is broadcast at promotion, before the mod learns of and acts on it), QCONT catch-up (item 7), and recipient tolerance (a rejected first action is re-sent; the next roster repairs trust). The fuller fix — the **roster-specific prepend** (prepend the cached signed roster ahead of a privileged member's forwarded action for recipients below the current version, reusing the item-7 delivered-version tracker, distinct from the `XGrpMemNew` profile prepend) — touches the hot per-recipient delivery loop that carries every forwarded message, so it is **deferred to a focused, separately-tested pass** rather than shipped untested.
- **Multi-owner roster signed by an unknown owner** (owner added after the recipient fetched the link): recipient cannot verify ⇒ buffer/refetch link. Cannot occur for single-owner MVP; flag for v7.
- **Roster vs. profile-update concurrency:** benign (different fields); verify the roster's relations-vector handling does not clobber the profile `MRIntroduced` semantics they share.

Tests: relay-fabricated moderator key is rejected (forgery); promotion delivers a verifiable key; demotion via roster + `XGrpMemRole` reconciles; removed privileged member does not reappear for a new joiner; replayed older roster rejected by existing members; TOFU key-change rejected; batch `APIMembersRole` emits roster + `XGrpMemRole` correctly; self-healing after a dropped role event.

## 1.9 Key anchors for Section 1

`ChatMsgEvent` `Protocol.hs:422`; `requiresSignature` `Protocol.hs:1231`; `groupMsgSigning` `Internal.hs:1962`; `withVerifiedMsg` `Subscriber.hs:3461`; `xGrpMemNew` `Subscriber.hs:2957`; `xGrpMemIntro` `Subscriber.hs:3015`; `introduceInChannel` `Internal.hs:1165`; `getGroupModerators` `Store/Groups.hs:1190`; `memberInfo` `Internal.hs:1187`; `createLinkOwnerMember` `Store/Groups.hs:3072`; `GroupKeys` `Types.hs:462`; `member_relations_vector` machinery in `Types/MemberRelations.hs` (`MemberRelation`, `MRIntroduced`, `IDSubjectIntroduced`, `setNewRelations`); forwarding `Batch.hs:106` / `Protocol.hs:1445` / `Delivery.hs:154`. New columns go in a **new tail migration** (`M20260222_chat_relays` is the *pattern* for relay group columns but is not the tail — never edit an applied migration; `M20260525_member_removed_at` is the current tail).

---

# Section 2 — Joiner role on the signed profile

Today the relay derives a joiner's role from `channelSubscriberRole` (`Controller.hs:161`, default `GRObserver` `Chat.hs:119`), a global config — so relays can disagree and the owner cannot set it per group. Move it onto the owner-signed profile.

## 2.1 Types and helpers

- Add `joinerRole :: Maybe GroupMemberRole` to `PublicGroupProfile` (`Types.hs:798`). No migration: JSON derives via `deriveJSON defaultJSON` with `omitNothingFields = True`, so `Nothing` is omitted on encode and a missing field decodes to `Nothing`.
- Add a `groupType` accessor and `isChannel` on `GroupInfo`/`GroupProfile`, and a resolver `joinerRoleFor :: GroupInfo -> GroupMemberRole` = `joinerRole` if set, else type-keyed default (`GTChannel → GRObserver`, `GTGroup → GRMember`, `GTUnknown _ → GRObserver`). Reuse the existing `publicGroupEditor`/`memberRole'` (`Types.hs:499`/`506`); do **not** introduce a profile-side `memberRole'` (name collision).

## 2.2 Replace the global config

Switch every `channelSubscriberRole` reader to `joinerRoleFor gInfo` and delete the config: `Controller.hs:161`, `Chat.hs:119`, `Commands.hs:2053`, `Commands.hs:2546`, `Subscriber.hs:3248`, `Subscriber.hs:4019`. Verify no out-of-tree consumer reads it.

## 2.3 Command, preferences, defensive refusal

- `APINewPublicGroup` (`Controller.hs:526`, handler `Commands.hs:2495`) gains `groupType` (default `GTChannel`) and optional `joinerRole` (default `joinerRoleFor` of the type); both written onto the constructed profile (today hardcodes `groupType = GTChannel` at `Commands.hs:2538`).
- Parameterize the channel-prefs parser by `GroupType`: Channel keeps its override (`support = OFF`); Public group and `GTUnknown` use secret-group defaults (member-to-moderator escalation is expected). Do not duplicate the parser — parameterize it.
- `directMessages` stays ON by inheritance but is dormant in any relay-mediated group; hide its toggle when `useRelays` and refuse `xGrpDirectInv` defensively when `useRelays'` (`Subscriber.hs:3321`, currently ungated): emit `messageError`, create no contact.

## 2.4 Compatibility

Existing channels: no `joinerRole` ⇒ falls back to `GRObserver` for `GTChannel`. No data migration. Older relays without this change resolve the joiner role from their global config — warn the owner at create time if a selected relay's chat version is below the public-groups version (soft warning, not a block).

---

# Section 3 — Backend tests

Public-group helpers paralleling the channel helpers, plus:

1. Member posts; all members receive it (no "unknown member" lines). 2. Multi-author session: no "unknown member" anywhere. 3. Member edit/delete/react forwarded to all. 4. `xGrpDirectInv` refused under `useRelays` (no contact created); repeat for Channel. 5. Blocked member's messages not forwarded. 6. Multi-relay delivery with cross-relay dedup. 7. History on join. 8. `asGroup=true` from a non-owner rejected. 9. Receipts disabled above the member limit. 10. Older client refuses `groupType = "group"` (needs-newer-version). 11. Incognito member posting attributes the incognito profile. 12. `joinerRole` propagates and defaults correctly (Channel→observer, Public group→member; absent→type default). Plus the roster tests in 1.8.

---

# Section 4 — Clients (iOS, then Kotlin)

## 4.1 Audit `useRelays` vs `isChannel` (structural commit, on its own)

~70–75 sites per platform branch on `useRelays` as a proxy for "is a channel." Split per a mechanical rule and land as a pure structural commit (no behavior change in the same diff):

- **Transport** (keep `useRelays`): link/relay management, owner-can't-leave-own-relay-group, relay-status indicator, incognito flag, typing-state gating, member-DM-affordance suppression.
- **Governance** (switch to `isChannel`): titles, "subscribers" vs "members" framing, "Channel preferences" labels, channel-style vs group-style member display.

## 4.2 Model and behavior

- Model: add `group` arm to `GroupType` (with serializers); `joinerRole`, `groupType`, `isChannel` accessors. Authoritative role resolution stays in Haskell; clients use it for display.
- **Narrow the existing refusal:** PR #7009 (merged to `stable`) added `GLPUpdateRequired` for `groupType /= GTChannel` (`Controller.hs:1051`, `Commands.hs` `unsupportedGroupType`). Change it to refuse only `GTUnknown _`; `GTGroup` proceeds to a public-group join.
- Create flow: one view with a Channel / Public-group segmented control (default Channel) driving the title, link-step label, success screen, and two API params (`groupType`, `joinerRole` = observer for Channel, member for Public group — no role picker in MVP). Hide `directMessages` in create prefs when `useRelays`. Render the threat-model note below the title for Public groups (text in Section 5).
- Suppress the member-tap "send direct message" affordance in any relay-mediated group.
- Members view shows the relay-known roster; header "subscribers" (channel) vs "members" (public group). No filtered view in MVP.
- Strings/icons: ~5–10 `_public_group` string keys mirroring channel forms; reuse `group_members_*` for "members" framing; a distinct Public-group icon (pending design). Kotlin-only: chat-list filter chips place Public groups in the "groups" bucket.

Platforms ship independently (API defaults are backward compatible).

---

# Section 5 — Threat model, docs, release

Fold into `channels-overview.md` (public groups inherit the entire channel threat model; deltas only):

- **A relay can fabricate content as any member** (channels: only as owners). Content (`XMsgNew`/`Update`/`Del`/`React`) is unsigned by design for deniability (`requiresSignature` lists roster/admin events only); broader blast radius in public groups. Detectable via cross-relay consistency. Mitigation is the future opt-in content signing on the channel roadmap; the create-flow note states the trade-off ("a malicious relay could change or fabricate messages from any member — pick relays you trust, or use a secret group for peer-to-peer integrity").
- **Roster rollback for new joiners** (1.3): documented bounded delta + future mitigations.
- Unchanged: relay cannot impersonate an owner or substitute the profile (signed events, validated entity ID); `joinerRole` and the privileged roster are owner-signed, so the relay cannot unilaterally change a joiner's default role or fabricate a moderator.

Document `XGrpRoster` (event, signing, versioning, TOFU, delivery) in `channels-protocol.md`. Bump the chat protocol version (the public-groups version that gates `GTGroup` and `joinerRole`). Release notes include the relay-fabrication line.

---

# Sequencing

1. **Section 1 — privileged roster** (backend). The core; gates the rest of the value. Land the gate-removal/roster-check and the event together so no half-applied trust window exists.
2. **Section 2 — joiner role on profile** (backend). Independent of Section 1.
3. **Section 3 — backend tests** (alongside 1–2).
4. **Section 4 — clients**: audit (structural) first, then iOS, then Kotlin.
5. **Section 5 — docs/version/release** with the backend release.

Backend (1–3) gates the clients. iOS and Kotlin are independent of each other.

---

# Out of scope (deferred)

- **Member-to-member DMs in relay-mediated groups.** Prohibited here (client affordance suppressed, receive-path refusal, relay does not forward `XGrpDirectInv` — so no relay-visible DM graph). A future plan must re-derive the threat model: relay-forwarded DMs would expose (sender, target, time) metadata; relay-blind rendezvous via per-member queues is the privacy-preserving alternative.
- **`memberAdmission` on relay-mediated join** (hardcoded `GAAccepted` bypasses review/captcha — generic relay-groups gap).
- **Roster filter/pagination in the members view** for very large groups.
- **Multi-owner** roster signing/verification and owner promotion via link `OwnerAuth` (v7); **opt-in content signing** (v7 roadmap); **full anti-rollback for new joiners** (link-version hard gate).
