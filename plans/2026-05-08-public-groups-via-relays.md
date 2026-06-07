# Plan: Public groups via relays

Date: 2026-05-08

## 1. Overview

Channels (shipped) are relay-mediated groups in which the relay forwards
content from any sender, but subscribers are pinned to `GRObserver` and
cannot post. Public groups are the second value of the same two-axis design:
same wire, same transport, members can post. The blocker is narrow — no
path produces `groupType = GTGroup`, and the relay's joiner-role default
comes from a global config instead of the owner-signed channel profile.
Add a `memberRole` field to the profile, plumb it (with `groupType`)
through the create command, derive the relay's joiner role from it, audit
clients for sites that conflate transport with governance, ride on the
approved member-profile dissemination plan. Member-to-member DMs in
relay-mediated groups are deferred (§10).

## 2. Concept summary: the `useRelays × groupType` matrix

| `useRelays` | `groupType`             | Name              | Wire shape                                                                                | UX                                                                                                              |
|-------------|-------------------------|-------------------|-------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------|
| `false`     | (no `publicGroup`)      | **Secret group**  | P2P `x.grp.inv` invitations; full mesh between members; JSON array batch.                 | Today's group: all members can post; profiles known eagerly; admins moderate.                                   |
| `true`      | `GTChannel`             | **Channel**       | Relay-mediated; subscribers join via channel link; binary signed-batch format; profile carries `memberRole` (default `GRObserver` at creation). | Today's channel: only owners post; subscribers anonymous to each other.                                          |
| `true`      | `GTGroup`               | **Public group**  | Same wire as channel; profile carries `memberRole` (default `GRMember` at creation); profile dissemination on demand. | New: every member can post; member-to-member DMs prohibited (deferred); member roster grown lazily via on-demand profile send. |
| `true`      | `GTUnknown _` (decode)  | (refuse to join)  | Channel link from a newer client; older client sees unknown discriminator.                | New clients reject with a clear "needs newer version" message; pre-existing channels unaffected.                  |

Three axes: **transport** = `useRelays` (topology, batch, signatures,
delivery); **governance model** = `groupType` (profile dissemination,
member affordances; member DMs prohibited in any relay-mediated group);
**joiner role** = `memberRole` on the owner-signed profile, set at
creation, type-keyed default. Today's client sites branch on `useRelays`
as a proxy for `isChannel` — that's the audit work (§4.2, §5.2).

## 3. Backend changes

### 3.1 Wire format / protocol

New optional `memberRole :: Maybe GroupMemberRole` on `PublicGroupProfile`
(owner-signed; relays read from cache). New chat-protocol version
`publicGroupsVersion` signals understanding of `groupType = "group"` and
`memberRole`. Older peers decode unknown `groupType` as `GTUnknown`
(lossless tag preservation already exists) and ignore unknown JSON fields
— §7 covers behavior. Channel-protocol docs gain a paragraph naming
`groupType` the discriminator and `memberRole` the owner-set joiner role.

### 3.2 Type changes

`PublicGroupProfile` gains `memberRole :: Maybe GroupMemberRole`. New
single-line helpers in the same module as `useRelays'`: `groupType'` /
`memberRole'` (accessors); `isPublicGroup'` (`useRelays' && groupType'
== Just GTGroup`); `defaultMemberRoleFor` (`GTChannel → GRObserver`,
`GTGroup → GRMember`, `GTUnknown _ → GRObserver` defensive);
`joinerRoleFor` (canonical resolver — `memberRole'` if present, else
`defaultMemberRoleFor groupType'`). `requiresSignature` unchanged for
MVP; opt-in content signing is future-work mitigation per §6.

### 3.3 API / command changes

`APINewPublicGroup` / `/public group` gain `groupType` (default channel)
and optional `memberRole` (default `defaultMemberRoleFor groupType`); both
are written onto the constructed profile. The subscriber-side prepare-group
flow reads `memberRole` from the resolved link with the same fallback. The
`channelSubscriberRole` config is removed (no callers after §3.4); tests
that flipped it migrate to Public groups or explicit `memberRole`.

#### 3.3.1 Default group preferences

Public-group defaults equal secret-group defaults — the channel override
(`support = OFF`) does not apply, since member-to-moderator escalation is
expected. Parameterize the existing channel-prefs parser by `GroupType`
(Channel keeps its path; Public group and `GTUnknown` use secret-group).
`directMessages` stays ON by inheritance but is **dormant** in any relay-
mediated group (relay doesn't forward `XGrpDirectInv`; clients hide the
toggle); keeping the wire ON lets a future plan re-enable DMs without a
profile-shape change.

### 3.4 Message processing

- **Relay joiner-role derivation** (today reads `channelSubscriberRole`):
  switch to `joinerRoleFor gInfo`. Eliminates cross-relay disparity.
- **Member-DM defensive refusal** (`xGrpDirectInv`): when `useRelays'`,
  emit `messageError` and create no contact. Belt-and-suspenders with the
  §4/§5 client suppression; unreachable today (no forwarding, no P2P).
- **Legacy `x.grp.inv`**: existing channel rejection covers Public groups.
- **`unverifiedAllowed`**: unchanged. Tightening becomes possible once
  the dissemination plan distributes member keys; existing TODO is
  updated to name that precondition.
- **Inherited unchanged** (add a test each): `checkSendAsGroup`
  (role-based), receipts cutoff (count-based), introduce-in-channel +
  history (`useRelays`-keyed).
- **`memberAdmission` on relay-mediated join**: hardcoded `GAAccepted`
  bypasses review/captcha. Generic relay-mediated-groups gap; §8.

### 3.5 Database migrations

No schema migration: `groupType` and `memberRole` ride the existing
JSON-serialized profile; absent fields resolve via `defaultMemberRoleFor`.
The dissemination plan's `sent_profile_vector BLOB` migration is a hard
prerequisite owned by that plan.

### 3.6 Test scenarios

Add Public-group helpers paralleling the channel helpers, plus:

1. Member sends content; all members receive it (no "unknown member" lines).
2. Multi-author session: no "unknown member" lines anywhere.
3. Member edit / delete / react forwarded by relay to all members.
4. Member-DM refused on receive: inject `XGrpDirectInv`, expect `messageError`, no contact created; repeat for Channel.
5. Role changes propagate through signed forwarding.
6. Blocked member's subsequent messages not forwarded.
7. Multi-relay delivery with cross-relay deduplication.
8. History on join.
9. `asGroup=true` from a non-owner member rejected with existing error.
10. Receipts disabled above the 20-member limit.
11. Older-client refusal on `groupType = "group"` shows needs-newer-version.
12. Incognito member posting attributes the incognito profile to others.
13. `memberRole` propagates: explicit `GRAuthor` at creation → joiners get `GRAuthor`; resolved link data carries the value.
14. `memberRole` defaults: Channel → `GRObserver`; Public group → `GRMember`.
15. Old-profile fallback: `memberRole = Nothing` → `defaultMemberRoleFor groupType` (`GRObserver` for Channel).

## 4. iOS changes

### 4.1 Model

Add `case group` to `GroupType` (with serializer arms);
`memberRole: GroupMemberRole?` on `PublicGroupProfile`; `isPublicGroup`,
`groupType`, `memberRole` accessors on `GroupProfile`/`GroupInfo`. Client
uses `memberRole` for display only; authoritative resolution stays on
Haskell.

### 4.2 Audit `useRelays` vs `isChannel` (≈73 sites)

Per-site rule: **transport** (link/relay management, owner-can't-leave-own-
relay-group, relay-status indicator, incognito flag display, typing-state
gating, member-DM-affordance suppression) → keep `useRelays`. **Governance**
(titles, "subscribers" vs "members" framing, "Channel preferences" labels,
channel-style vs group-style member display) → switch to `isChannel`.
Roughly 70% flip to `isChannel`. Visually compare Public / Channel / Secret
after.

### 4.3 Create flow

Unified view with a "Channel / Public group" segmented control above the
display-name field, defaulting to Channel. The toggle drives the screen
title, link-step label, success screen, and two API parameters: `groupType`
and `memberRole` (`.observer` for Channel, `.member` for Public group —
no separate role picker in MVP). Default `groupPreferences` builder is
`groupType`-keyed per §3.3.1. The `directMessages` toggle is hidden in
the create-flow prefs section when `useRelays`. When `groupType = .group`,
render below the title:

> "In a Public group, every member can post. Messages are delivered through
> relays you choose, which means a malicious relay could change or
> fabricate messages from any member. Pick relays you trust."

### 4.4 Strings, views, icons, connect-plan

- **Strings:** ~5–10 keys mirroring channel forms with `_public_group`
  suffixes (create/add/leave/delete/link/temporarily-unavailable/no-
  relays), plus `create_public_group_threat_model_note`. Reuse
  `group_members_*` for "members" framing; channels keep `_subscriber*`.
- **Compose:** existing role-based gates allow members to post; **suppress
  the member-tap "send direct message" affordance in any relay-mediated
  group** (client side of the DM prohibition; receive gate at §3.4).
- **Views:** `GroupChatInfoView` and the link view branch three ways at
  §4.2 sites; the link view takes `groupInfo` and derives variant inside.
  `GroupPreferencesView` hides `directMessages` when `useRelays`.
- **Icon:** `chatIconName` gains a Public-group arm with a distinct icon
  (different from channel-antenna and secret-group-people — §8).
- **Members view:** show the relay-known roster; header "subscribers" for
  channels, "members" for Public groups. No filtered view in MVP.
- **Connect-plan:** wording keyed on resolved `groupType` — "ok to
  subscribe via relays" (channel) vs "ok to join via relays" (Public
  group). CLI string changes alongside; tests follow.

## 5. Kotlin changes

Mirror of §4 across the Compose surface. Subsections parallel §4 and
note divergences only.

### 5.1 Model

`GroupType` gains `Group`; `memberRole` / `isPublicGroup` accessors on
`GroupInfo` / `GroupProfile`.

### 5.2 Audit `useRelays` vs `isChannel` (≈74 sites)

Same transport-vs-governance rule as §4.2; ~70% flip to `isChannel`.

### 5.3 Create flow

Single-view create with Channel / Public-group toggle driving
`groupType`+`memberRole`; threat-model note below the title;
`directMessages` toggle hidden under `useRelays`.

### 5.4 Strings, views, icons, ConnectPlan

Strings, views, icons, and ConnectPlan mirror §4.4. **Kotlin-only:**
chat-list filter chips place Public groups in the "groups" bucket
(mental model: "things I can post in"), not "channels".

## 6. Threat model: changes from channels

This section assumes the channel threat model
(`docs/protocol/channels-overview.md` §"Threat model"). Public groups
inherit every property listed there. One threat is *broader* (channels
have a narrower form of the same threat). The relay's "can / cannot"
framing matches the existing doc style; the items below are written so
they can be folded directly into a future revision of
`channels-overview.md` once Public groups ship.

### 6.A.1 A relay can fabricate content as any member

Content messages (`XMsgNew`, `XMsgUpdate`, `XMsgDel`, `XMsgReact`,
`XFileCancel`) are unsigned in both channels and Public groups
(`Protocol.hs:1221`, `requiresSignature` lists only roster /
administrative events). In channels this gives a compromised relay
the ability to fabricate content attributed to owners — already
documented in `channels-overview.md` §"Threat model" ("Substitute
unsigned content or selectively drop messages for its subscribers").
In Public groups, the same property has a **broader blast radius**:
the relay can fabricate content attributed to *any* member, not just
to owners.

This matches the channel deniability property by design (see
`channels-overview.md` §"Signing scope: roster only, content
optional"): unsigned content is precisely what enables cryptographic
deniability — no third party can prove a member authored anything.
The trade-off is that the operator on the delivery path cannot be
prevented from forging in the same channel.

**A single compromised relay**

*can:*

- Fabricate content messages attributed to any member, not just to
  owners. Detectable by other members through cross-relay
  consistency (same TODO as the channel case: difference detection
  not yet implemented).
- Modify the text or content of messages in transit and re-attribute
  the modified message to its original author.
- Drop content messages selectively — same property as channels.

*cannot:*

- Forge signed administrative events: `XGrpInfo`, `XGrpPrefs`,
  `XGrpMemRole`, `XGrpMemRestrict`, `XGrpMemDel`, `XGrpDel`,
  `XGrpLeave`, `XInfo` (`Protocol.hs:1221`). Roster manipulation,
  profile changes, and member-attributed leave / profile-update
  events all require valid signatures.
- Substitute the channel profile or impersonate an owner — the
  channel's entity ID and owner authorization chain are validated
  by every recipient against the channel link. The new `memberRole`
  field is part of the (owner-signed) channel profile, so a
  compromised relay also cannot fabricate a different joiner role
  than the owner configured.
- Alter authoritative state on owner devices.

**Mitigation.** No code change for the MVP. The future-work fix is
opt-in content signing per the channel roadmap
(`channels-overview.md` §"Future work" / "Transcript integrity" /
"Opt-in content signing"). When that ships, owners of Public groups
will be able to require all content (member or owner) to carry a
signature; member keys are already disseminated to other members
via the prior plan (`2026-04-29-member-profile-sending-channels.md`),
so verification on the recipient side is not a separate effort.

In the meantime, the create-flow help text for "Public group" on
both platforms (§4.3, §5.3) carries this trade-off framing: "In a
Public group, the relay forwards messages on behalf of every member.
A compromised relay could change message text or attribute fabricated
messages to any member. Use a secret group if you need non-
repudiable peer-to-peer messaging." This is the same trade-off
channels make for owner posts; making it explicit at create time
lets users choose Public-group-via-relay vs secret-group based on
whether they value scale or content integrity.

### 6.A.2 What is unchanged from channels

Every other property of the channel threat model carries over
without change. In particular:

- A relay cannot impersonate an owner or substitute the channel
  profile (signed events, validated entity ID). The configured
  `memberRole` is part of the signed profile, so the relay cannot
  unilaterally elevate or demote joiners relative to what the owner
  specified.
- A relay cannot determine subscriber / member real identity or
  network address (inherited from SMP transport).
- All-relays-compromised-and-colluding cannot forge signed events
  or alter owner-authoritative state.
- A passive network observer cannot determine which Public group a
  member is in, or correlate Public-group activity with other
  SimpleX activity.

Public-group members get the same participant-privacy guarantees as
channel subscribers, and Public-group owners get the same key-loss
risk profile as channel owners (see `channels-overview.md`
§"Compromise of owner keys" and §"Loss of all owner devices").

**Out of scope for now: member-to-member DMs in relay-mediated
groups.** In channels, members do not DM each other today. In Public
groups, this plan prohibits the affordance (client-side and
defensively on the receive path) and the relay does not forward
`XGrpDirectInv`. The relay therefore does not see a "member DM
graph" — that threat (which a forwarded-DM design would have
introduced) does not exist under this plan. A future plan can
re-introduce member-to-member DMs and revisit the metadata trade-off
explicitly; the design space is sketched in §10.

### 6.A.3 Release-notes line

For the Public-groups release notes, include a one-line summary of
the new property:

> "In a Public group, the relay you choose could in principle alter
> or fabricate group messages attributed to any member. Pick relays
> you trust, or use a secret group if you need peer-to-peer message
> integrity."

## 7. Migration / compatibility

- **Existing channels unaffected.** Pre-upgrade profiles have no
  `memberRole`; readers fall back to `defaultMemberRoleFor GTChannel =
  GRObserver`. No data migration.
- **Older clients** decode `groupType = "group"` as `GTUnknown` and must
  refuse to join with a "needs newer version" alert; they ignore unknown
  fields on channel profiles otherwise.
- **Older relays** forward Public-group traffic but resolve joiner role
  from their global config — joiners via un-upgraded relays get the legacy
  role and cannot post. Mitigation: warn the owner at create time if any
  selected relay's chat version is below `publicGroupsVersion`. Soft
  warning, not a hard block.

## 8. Open questions

1. **Future member-DM design.** (i) Relay-forwarded `XGrpDirectInv`
   (simple; leaks DM-graph metadata); (ii) relay-blind rendezvous via
   per-member queues on the profile (privacy-preserving; new protocol).
   Either re-derives §6.
2. **`memberAdmission` on relay-mediated join.** Hardcoded `GAAccepted`
   bypasses review/captcha; generic relay-mediated-groups gap; defer.
3. **Distinct icon for Public groups.** Visually different from channel-
   antenna and secret-group-people metaphors. Pending design review.
4. **`channelSubscriberRole` removal.** Verify no out-of-tree consumer
   reads it before deleting.
5. **`memberRole` on profile edit.** MVP exposes no UI; Haskell accepts
   the edit but role-rebase of existing members is undefined. Deferred.
6. **Roster filter in members view.** Paginate/filter for 100K+ members?
   Generic relay-roster question; defer.
7. **Connect-plan wording.** "subscribe / join / connect via relays" —
   pick per `groupType`; update tests when CLI string changes.

## 9. Sequencing

1. **Prerequisite:** member-profile dissemination plan lands first.
2. **Backend:** types, `memberRole` field, command parameters, profile-
   based role derivation, removal of `channelSubscriberRole`, defensive
   `XGrpDirectInv` refusal, tests 1–15.
3. **iOS** then **Kotlin** (independent of each other; API defaults are
   backward compatible): model, audit, create flow, strings; then views,
   icons, ConnectPlan, DM-affordance suppression.
4. **Older-client refusal, version-bump release notes, channel-docs
   updates** ship with the backend release.

## 10. Adjacent work (not planned here)

- **Owner→relay communication of rejection rules.** Joiner-role side is
  fixed here (travels on the signed profile); rejection-rule side
  (admission/captcha) is still relay-side config. Future plan: carry it
  on the profile too.
- **Owner-signature verification on the channel profile by relays.**
  Affects channels equally; does not gate this plan.
- **Member-to-member DMs in relay-mediated groups.** Deferred (§1, §8 Q1).
  A future plan must re-derive §6 — relay-forwarded DMs would re-introduce
  (sender, target, time) metadata exposure this plan
  avoids.
