# Channel multi-owner

A channel can have N equal owners, per `docs/protocol/channels-overview.md` §Governance ("v7: any-owner-decides"). Any owner can take any administrative action, including destroying the channel; no coordination between owners. Owner *removal* (handing over ownership, dropping an owner) is in scope — it is the point of the feature: a channel must survive losing any single owner.

Only the creator can do this today. simplexmq already models most of it and leaves it unwired: `UserContactData.owners :: [OwnerAuth]` (a signed chain — each entry signed by root or an earlier owner), `decryptLinkData`/`validateLinkOwners` accept data signed by any listed owner, and `RKEY`/`recipientKeys :: NonEmpty` let a queue have many writers. The single-owner assumption lives almost entirely in `groupLinkData` (`Internal.hs:1509`), which republishes only *self* as the owner.

## 1. The frame: every shared state needs a sequencer

The whole problem is ordering concurrent writes, and the answer is fixed by **where the state physically lives**:

| State | Lives on | Sequencer | How it converges |
|---|---|---|---|
| **Link data** (profile, relay list, owners chain) | one blob on the SMP link queue | the **SMP server** | version-CAS: write-with-expected-version, rejection returns current state, re-merge |
| **Owner queue keys** (`recipientKeys`) | one set on the SMP link queue | the **SMP server** | version-CAS, same shape; + an owner-held key→owner map |
| **Roster** (member/mod/admin roles) | a signed copy on every relay; no trusted central copy | **none** | deterministic tie-break + owner-side auto-retry |
| **Subscriber count** | derived, cosmetic | a **leading owner** (for efficiency, not correctness) | relay-sourced; chain-order handover |

Link data and keys already *have* a sequencer — the server — so they need only optimistic concurrency, no consensus. The roster is the only state with no sequencer, which is exactly why it is the hard part. The count needs a soft leader only to avoid write contention.

## 2. Link data

**What:** the mutable link blob — channel profile, relay list, owners chain, subscriber count. Read by every joiner; written by any owner. The server stores it encrypted and cannot read it.

**Sequencer — a content hash for the server CAS, a version inside the data for event ordering.** These are two tokens serving two layers. The server's CAS token is the **hash of the stored encrypted user-data blob** — the only thing the server can key on, since it cannot read the content. `LSET` carries the expected hash; the server accepts iff `hash(stored) == expected`, stores the new blob, and a mismatch **rejects and returns the current blob**. The owner learns the hash without a round-trip: it hashes the bytes it uploaded on success, or the returned blob on a rejection — never assuming it. Separately, a **monotonic `version` lives inside the mutable data** (invisible to the server); it exists only so recipients can **reject stale events** — an event whose version is below what a recipient already holds is ignored. So: hash = server CAS token (server-visible, unordered); in-content version = client-side ordering (server-blind). A rejected owner re-merges its one change onto the returned blob and retries.

**Both tokens travel in events, so writers rarely pre-read.** The events that change link data (`XGrpInfo`, `XGrpRelayNew`, the owner add/remove/revoke events, the count publish) carry the new `(version, hash)`. Owners hold current state + version + hash from those events, so a write CASes on the hash it holds and succeeds — the only rejections are genuinely simultaneous writes. **Order: write first (`LSET`-CAS), then broadcast the event with the hash of what was written** — a rejected-then-retried write would otherwise announce the wrong hash. **`LGET` is never needed to write** — a stale writer is simply rejected and gets the current blob back.

**One reconcile routine keeps local state in sync; `groupLinkData` always builds from local.** "Reconcile" merges published link data into local state per field: sets (relays, owners, revoked-owners) merge and keep the owner's own pending intent; a whole-value field (profile/prefs) with a genuine concurrent same-field edit surfaces a conflict. Because local state thus stays "published + my pending intents", `groupLinkData` builds the write from local state — no separate delta step. Reconcile runs in three places, all the same code: **on channel open** (owners run `APIGetUpdatedGroupLinkData` like subscribers — the owner-exclusion is removed — to adopt others' changes, connect to newly-added relays, and correct revocations), **on a write rejection** (adopt the returned blob, then rebuild), and as the missed-event backstop. The `owners` list is **sorted by `memberId`** (a canonical order giving a stable leader index and deterministic serialization); a write inserts my entry in place. This is clean when every entry is root-signed (§6 default, order-independent); a *delegated* entry must currently follow its signer (`validateLinkOwners` accepts "signed by root or an earlier entry"), so a memberId sort is only fully compatible if that validation is made order-independent (validate by anchoring each entry to root through the set) — a simplexmq change flagged in §11. `revokedOwners` is a list of `memberId` in the mutable data, append-only (the remover writes it on removal, the leaver on leave, any owner corrects an inconsistency it sees on reconcile). The count comes from relays (§5).

**One more existing defect:** relay activation must key off an *accepted* write, not the `LINK` echo (`Agent.hs:1813` returns the client's own sent data, so today an owner marks a relay active because it asked to — nothing reads back the truth).

## 3. Owner queue keys

**What:** the set of recipient auth public keys allowed to write the link queue = the owners' link-write keys. A promoted owner generates one (`linkRcvKey`) and it is added via `RKEY`.

**Sequencer — the server, hash-CAS.** The CAS token is the hash of the canonically-sorted key set. Unlike link data, the key set is *not* nonce-encrypted — the server holds the keys in the clear and any owner knows them (via the map below) — so **both sides compute the same hash from the content, and an owner rarely needs a rejection or an event to learn it**: it just hashes the set it believes is current. `RKEY` carries the expected hash and the full new set; a mismatch rejects and returns the current set. Two concurrent additions: one wins, the other re-merges its key onto the returned set and retries, so both land. CAS is needed because `RKEY` *replaces* — without it, two concurrent adds each drop the other's key.

**Correlation for removal — a signed map, cached on relays, never on the link server.** To remove owner X you must know *which* key is X's, but the server's set is deliberately **anonymous**: any public or link-server key→owner map lets a joining SMP operator (who reads the OwnerAuth chain and sees which key writes) tie an owner's link-queue activity to their identity. So the `memberId → linkRcvKey` map lives elsewhere: a new owner announces its key (own protocol event, delivered owner-scoped via `getGroupOwners`/a `DJSOwners` scope), and — critically — **relays cache the signed map and serve it on request**. Each entry is owner-signed, so a relay cannot forge one; a relay sees the map but is not the link-queue operator, so it cannot correlate link-queue writes. Because relays are always-on, an owner can always obtain the map without waiting for a peer owner. The map is also what lets an owner compute the key-set hash locally (above).

**So removal never blocks and self-heals:** it is an **async command** whose steps chain by continuation (fetch the map from a relay if the local copy lacks the target → `RKEY` out the key → `LSET` the revocation), CAS rejections retrying inline — no new durable-worker type. A crash mid-removal is corrected by reconcile-on-open (§2): any owner that sees an `OwnerAuth` whose key is no longer in the set treats it as revoked and completes the marker. A missing *addition* self-heals via CAS (a stale set hashes wrong → rejection → returned set repairs it). A relay withholding the map is handled by asking another relay.

## 4. Roster

**What:** the owner-signed set of member/moderator/admin roles (owners are on the link chain, not the roster). It has **two representations**: individual signed `x.grp.mem.role` events sent to members, and a versioned blob sent to relays and served to joiners. These can diverge under concurrency — which is the whole difficulty.

**No sequencer exists** (a copy sits on each relay; relays are untrusted and cannot sign), so convergence is owner-side, in three parts:

1. **Deterministic winner.** Tie-break on `(version, authorMemberId)` — `memberId` is identical on every device, so every node picks the same blob regardless of arrival order. This must be applied at **all three** version gates: the signed-event gate (`Subscriber.hs:3302`), the blob header (`:3430`), and the blob-completion gate (`:3461`, today version-only and storing a device-local id) — the owner↔owner path is the blob, applied at completion. Add `roster_version_owner_id` (the cross-device author) and store it there.

2. **Auto-retry to the union.** An owner keeps its **pending role-deltas** durably. On adopting a newer winning blob that lacks one, it re-applies the delta *if the winner did not also touch that member* — re-broadcasting at a higher version. O1 (X→admin) and O2 (Y→admin) thus converge to *both*, not one. Verified: the re-emitted `x.grp.mem.role` fans out to the full membership (`DJSGroup` → all members, not just relays/joiners), so an already-joined affected member receives it and updates — this is the only channel by which an existing member learns a role change (the blob goes only to relays/joiners).

3. **True conflict.** If both owners changed the *same* member, the tie-break winner stands, that member heals to the winner (both owners emitted its event), and the losing owner surfaces "your change to X was overridden." No silent revert.

**Why no leader:** a leading owner that sequences the roster reintroduces the single-owner dependency multi-owner exists to remove — if it is asleep, all role changes stall. Auto-retry needs no one to be online but the acting owners. Races are rare (concurrent role edits within seconds), so the extra broadcast is acceptable. Bulk changes (one call, one version bump, N events) re-send the bulk on retry.

## 5. Subscriber count

**What:** the "subscribers: N" display — cosmetic, best-effort. Owners have no subscriber connections (subscribers connect to relays), so the count can only come from relays.

**Source — absolute per-relay counts, take the max.** Each relay states its own *current absolute* subscriber count as a distinct per-relay event **batched with the join/leave/removal event it already sends to owners** — so it costs no extra SMP block, and it need only be restated when the count changes, which is exactly on a join/leave/removal (no periodic reporting). The owner stores the latest **per relay** (a field on the relay's member row); the published count is the **max** across relays. Absolute-plus-max makes the hard case correct: a newly added relay that subscribers are still slowly connecting to reports a low number that stays below the max and is ignored until it catches up; a subscriber leaving that relay drops only its entry, not the max; a real departure (from all relays) lowers every entry and so the max. **Deltas fail here** — a leave is forwarded by several relays and *deduplicated*, so an owner cannot attribute a decrease to a relay, and a low new relay's decrease would wrongly crash the total. The count event, being a separate per-relay event, is not deduplicated even though the join/leave it batches with is. (Relays need not talk to each other; the max of independent reports suffices.)

**Sequencer — a leading owner** (the lowest `memberId` among current owners — a stable order every device computes the same way) publishes the count to link data. Not for correctness (relay-sourcing already fixed the promoted-owner-undercount) but for **efficiency**: the count writes the shared link blob, so N owners publishing per join would each rewrite it and starve human writes; one publisher is one stream. **Handover follows that order on a membership change** (tentative — see §11): when the leader is removed or leaves, the next-lowest `memberId` owner becomes the publisher, no liveness detection. A leader that is merely *offline* is not handed over; the count just goes stale until it returns, which is fine because the count is cosmetic. A new owner seeds from the published value and does not publish until it has its own relay reports.

## 6. Owner promotion (adding an owner)

**Trigger:** `APIMembersRole … GROwner` on a channel starts an async, single-target invite flow instead of a plain role change. The UI warns the promoter to verify the invitee's key first (§8).

**Events** (role-generic, so a future non-owner promotion reuses them), all `requiresSignature`, delivered owner→relay→M in M's support scope:
```
x.grp.promote.inv     { invitationId, memberRole, roleData? }   -- O1 -> M
x.grp.promote.acpt    { invitationId, memberKey, roleData? }    -- M  -> O1
x.grp.promote.reject  { invitationId }                          -- M  -> O1
x.grp.promote.cancel  { invitationId }                          -- O1 -> M
```
`memberRole` is the offered role; `roleData` is an optional role-scoped block (owner: `linkRcvId` in inv, fresh `linkRcvKey` in acpt); `memberKey` is M's existing key, which O1 must check **equals the key it already holds for M** — a consistency check so the acceptance cannot introduce a new key (distinct from the OOB verification in §8, which is advisory). A future role adds a `roleData` variant; the parser must decode an unknown tag to an opaque/ignored value (not error), since `omittedField` only covers an *absent* field.

**Consent:** acceptance is not automatic — M confirms in the UI. A pending record on each side; reject or cancel clears it. On acceptance O1 chains, as an async command resumable from the pending record (no new worker type — the record is the durable state, re-driven on receipt and on startup): `RKEY` (add M's key, §3) → `LSET` (insert M's `OwnerAuth` in the sorted `owners`, §2) → `x.grp.mem.role … GROwner` (the **commit point** — M treats itself as owner only on receiving this). O1 also announces M's key to the other owners and sends M the current key-map (§3).

**Sign new owners with the root key when the adder holds it** (i.e. the creator, who keeps the root private key). Root-signed entries are each independently valid — `validateLinkOwners` accepts any entry signed by root — so all owners sit at one level and the list has no delegation dependencies. A non-creator owner has no root key, so it must sign with its member key (delegation), and *those* entries depend on their signer; the creator being the usual adder keeps the list flat, which is what makes removal (§7) simple.

**Transport is not confidential, and that is fine.** Support scope reaches M plus all moderators, so a moderator sees the invitation — but nothing secret is in it (`linkRcvId`/`linkRcvKey` are inert public data), and a moderator that *injects* a fake `x.grp.promote.inv` achieves nothing: completion needs an owner holding a matching pending record, which no one does, so no key or link write ever happens. M additionally verifies the `.inv` is owner-signed and ignores others.

## 7. Owner removal

Drop the leaver's key from `recipientKeys` (`RKEY`, targeting it via the owner-held map §3) and add its `memberId` to **`revokedOwners`** in the mutable link data. **Keep the `OwnerAuth` entry** rather than deleting it — a revoked entry stays valid for verifying the owner's *past* signed messages, `revokedOwners` marks it as no longer current (so a new joiner does not treat it as an owner), and its key is gone so it can no longer write. This is uniform whether the entry is root-signed or delegated; for a delegated entry, keeping it also avoids breaking any owner it signed (root-signed entries — the common case, §6 — have no such dependency). Enforcement is at the SMP layer (no key = no authority), not in the chain. **Leave** is self-sequenced: an owner removing itself writes the revocation first and drops its own key last; if it crashes, a remaining owner finishes. Removal is not a defence against a malicious owner (any owner can already destroy the channel) — it is administrative hand-off.

## 8. Security

- **Promotion is only as safe as the verified key.** An owner's copy of a subscriber's key is relay-asserted (from unsigned `XGrpMemNew`), so a relay that substituted it could otherwise be promoted. The defence is out-of-band verification, which is **already implemented** for channels (`verifyChannelMemberCode`, `Commands.hs:2021`, hashes both members' keys, sorted, so comparing detects substitution). It is **advisory**: the promoter is warned in the UI to verify before promoting (matching the existing channel model), not hard-blocked in the backend. Residual risk: an owner who ignores the warning and promotes an unverified member could sign a relay-substituted key into the chain, making that relay an owner. The backend still enforces the weaker consistency check (§6) that the accepted key equals the one O1 already holds.
- **Any owner can destroy the channel** (`Server.hs:1249`: any recipient key authorises `DEL`/`LDEL`) and can RKEY the key set down to itself, evicting the others. Both accepted under any-owner-decides; recorded because they exceed any chat-level action.
- **Owner-key map is owner-only** (§3), so an SMP operator cannot tie an owner's link-queue writes to their identity.
- **Creator anonymity** is weaker than the overview claims once owner 2 signs owner 3 with its own key (the chain shows who delegated to whom). Qualify it.

## 9. Data model

- **SMP link queue (simplexmq server):** the CAS token per mutable object — the **hash of the stored user-data blob** for link data, and the hash of the sorted key set for `recipientKeys` (the server computes each on write; no version counter). These wire changes gate on a new SMP **relay** version (`currentServerSMPRelayVersion`, `VersionSMP` 18 → 19 — *not* `currentSMPClientVersion`, which is the client↔client envelope and gates nothing server-facing). **Multi-owner requires the link queue's server ≥ v19**: below it, blind `LSET` is merely lossy, but blind `RKEY` *evicts* other owners (replace with no CAS), so promotion must **fail closed** on an old link server, not degrade.
- **Agent store (owner device, both SQLite and Postgres trees):** the last-known link-data hash + in-content version cached next to the link credentials; `linkRootSigKey` persisted (`AgentStore.hs:2514`).
- **Chat DB (owner device):** owners chain (`owner_auth_sig`/`owner_auth_index` on `group_members`, threaded through `createLinkOwnerMember`/`updateRelayGroupKeys`); pending-promotion record (member-row columns); pending role-deltas; `roster_version_owner_id`; per-relay absolute subscriber count (a field on each relay's member row); the `memberId → linkRcvKey` map. A `DJSOwners` owner-only delivery scope for the key announcement.
- **Relay:** a cached signed `memberId → linkRcvKey` map, served on request (for owner removal).
- **Apps (iOS + Kotlin):** `canManageLink` on `GroupInfo`; `MemberRoleProposal` (`MRProposed`/`MRRejected`) + `promotionPending` on `GroupMember`; a new `RcvGroupEvent` case for the promotion service item — all optional/forward-compatible so remote-desktop parsing across versions holds.

## 10. UI/UX

- **Promoter:** keep the role picker, add `.owner`; selecting it sends the invitation and shows a proposed-role row (proposed → invitation-sent + Cancel; rejected → declined + re-invite). Warn if the invitee's key is unverified and route to the verification screen — the consequence is the channel's trust chain, not one conversation.
- **Invitee:** a service chat item (unread) plus an accept/reject banner in M's support chat; accept goes through a confirmation stating any owner can delete the channel. (The banner is new code, not a reuse — the existing pending-member bar is gated on a non-null scope member, and M's own support scope has none.)
- **`canManageLink`** gates link/relay *management* (an owner mid-handover cannot write yet); relay *status display* stays on `isOwner`.
- **Conflict surfaces** (both rare): the losing owner is told "profile edit superseded" (link-data same-field conflict) or "your role change to X was overridden" (roster same-member conflict). No special affordance — the owner re-applies through the normal picker/edit if they still want to.

## 11. Open decisions

- **Count-leader handover** is tentative: chain-order takeover on a membership change (§5). Since owners are often-offline phones, the lowest-memberId leader may be absent much of the time, leaving the count stale — so a staleness-triggered takeover (an online owner publishes when the count lags its own relay-derived value, staggered by memberId order so they don't all write at once, CAS resolving any overlap) is likely needed for the count to actually track. To brainstorm.
- **`validateLinkOwners` order** (simplexmq): a memberId sort of `owners` (§2) coexists with delegated (non-root-signed) entries only if validation anchors each entry to root through the set rather than requiring "signed by an earlier entry". Decide whether to make it order-independent, or to require all owners root-signed (creator-only adds).

Settled this round: multi-owner hard-blocks on a pre-v19 link server (§9, no degraded mode); the roster approach is tie-break + auto-retry with no leader (§4); a same-member conflict is surfaced and re-applied through normal UI (§10).

A detailed change-list (per-file edits, migrations, test cases) follows once this design is locked — deliberately omitted here to keep the design reviewable in one pass.

---

## Appendix A — Why not a consensus protocol (owners or relays)

§1 fixes the answer before any protocol is chosen: the coordination a state needs is set by where it physically lives. Three of the four states already sit on one SMP server (link data, owner keys) or are cosmetic (count), so a server-side compare-and-swap — "consensus with one acceptor", the cheapest sequencer there is — suffices (§2, §3, §5). Only the roster has no sequencer (§4). Under CAP a replicated store is either consistent or available during a partition; multi-owner exists to stay available when owners are absent, so this design takes the available side and converges rather than blocking on a quorum. A real quorum consensus (Paxos/Raft/PBFT/Simplex-style propose → majority-vote → commit) has two candidate homes here — the owners, or the relays — and both fail.

### A.1 Consensus between owners fails on liveness

- **Owners are offline for unbounded time, with no direct connections.** Every owner-to-owner message crosses relays (peer owners are stored as keys, not connections), and a phone can be off for a week, so there is no partial-synchrony bound. By FLP a deterministic quorum cannot both stay safe and terminate here except during network windows nothing can guarantee.
- **A quorum inverts the feature.** Majority-quorum needs a majority *present* to act — a 2-owner channel would need both phones online, and losing an owner would freeze governance. Multi-owner exists to *survive* losing an owner; a quorum makes losing one *harder* to tolerate.
- **A timer that "applies if owners are offline" is not consensus.** A consensus protocol never commits without a quorum; the moment a change is applied on timeout it becomes optimistic replication — which is what this design already is — so the timer needs the same convergence layer underneath and only adds latency plus a relay round-trip. The cases collapse: a lone writer never conflicts; concurrent edits to *different* members converge with or without it; a concurrent same-member edit still resolves by a deterministic tie-break; a partitioned timeout applies on both sides and then reconciles by that same tie-break. Nothing is removed.
- **Reconfiguration is the hardest part, and it is the headline feature.** Changing a consensus group's membership is the most error-prone part of every such protocol, and "add an owner" *is* reconfiguration.

### A.2 Consensus among relays fails on trust, fault-tolerance, and bootstrap

Relays are the stronger candidate: they are always-on, so A.1's liveness objection does not apply — a relay quorum commits without any owner online — and administrative content stays owner-signed, so relays would order operations, not author them. It still fails:

- **Fault-tolerance needs a relay count channels do not have.** Tolerating one *malicious* relay needs 4 relays (BFT, n ≥ 3f+1); one *offline* honest relay needs 3 (CFT, n ≥ 2f+1). Channels run 1–3 relays, so Byzantine safety is unreachable and the design collapses to assuming every relay orders *honestly* — the exact trust the architecture refuses to grant, since a relay may be operated by anyone.
- **Ordering power alone is an attack, even on signed content.** A dishonest relay inside the agreement can equivocate (show different orders to different members → a forked roster), censor (stall an operation it is party to), or reorder to exploit semantics ("remove O2" before "O2 promotes X"). Signatures stop forgery, not reordering, and CFT gives no protection at all. Today no single relay can fork governance — the owner's latest signed state wins through any honest relay — so this is a strict regression.
- **It is a large new subsystem for a small gain.** Relays do not talk to each other today; consensus requires an all-to-all inter-relay mesh plus a full BFT/CFT protocol — security-critical code whose net effect raises complexity while only the roster gets simpler.
- **Relay reconfiguration is circular and can veto itself.** The relay set is mutable governance the owners control, so every relay add/remove/rotate is a consensus-cluster reconfiguration; the worst case is a relay stalling the operation that would *remove it*, defeating the relay-transience objective (owners must be able to remove a bad relay, including the last one).
- **It cannot cover the bootstrap root, so it does not unify.** Link data (profile, relay list, owners chain) and owner keys live on the SMP link-queue server — the entry point a joiner reads with only the short link, before it knows any relay, and where the relay list itself lives. That server already sequences those writes censorship-free via CAS; the actual commit is the owner's LSET, which relays cannot and must not perform (a link-queue key also authorizes `LDEL`). Routing link-data changes through relay-consensus would be redundant with the SMP sequencer and would hand relays a veto over governance — including their own removal. So relay-consensus could cover only the roster, leaving link data and keys on CAS: two sequencing systems, not one.

The general rule: a relay-provided sequencer is only worth considering for state that has *no* sequencer *and* that relays have no conflict of interest about. Link data fails both, owner keys live on the SMP server, the count is cosmetic — leaving only the roster, where the costs above outweigh the gain.

### A.3 A single designated relay as the roster sequencer — also rejected

The lightest form — one designated relay holding the roster under version-CAS (no inter-relay consensus, reusing the link-data pattern) — is rejected too: it concentrates trust in a single relay that can then fork the roster by serving different states to different members, or freeze roster writes whenever it is offline. That worsens the current threat model, in which no single relay can fork governance. The deterministic tie-break (§4) deliberately needs zero trusted parties; a single-relay sequencer trades that property away for a sequencer the roster already approximates without trust.

### The coordination the design does use

The right-sized coordination is already in place: single-acceptor CAS for the server-backed states (§2, §3); a two-party propose → accept → commit handshake for owner promotion (§6), where the action is inherently bilateral (consent) rather than a quorum; and deterministic convergence for the roster (§4), which needs no trusted sequencer. A future M-of-N multisig (governance roadmap) is an *authorization* threshold — who must approve — a different axis that still rides this same convergence substrate and does not need a classical consensus protocol for race resolution.
