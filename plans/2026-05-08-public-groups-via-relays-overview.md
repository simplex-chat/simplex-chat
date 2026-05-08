# Public groups via relays — plan summary

Companion overview for `2026-05-08-public-groups-via-relays.md`.

## What

Add a third kind of group: relay-mediated like channels, but every member can post like a
secret group. Resolves the scale ceiling of full-mesh groups without the broadcast-only
governance of channels.

## Concept

Two orthogonal axes, already in the model:

| `useRelays` | `groupType`  | Name         |
|-------------|--------------|--------------|
| false       | (none)       | Secret group |
| true        | `GTChannel`  | Channel      |
| true        | `GTGroup`    | Public group | ← new
| true        | `GTUnknown`  | refuse       | ← older client sees this for `"group"`

`useRelays` = transport (topology, batch format, signing). `groupType` = governance (joiner
role, profile dissemination, member-DM affordances).

## Code blocker (narrow)

No path produces `groupType = GTGroup` (`Commands.hs:2514` always writes `GTChannel`), and
the relay's joiner role comes from `channelSubscriberRole` config instead of `groupType`.

## Backend changes (Haskell)

- **Wire**: bump `currentChatVersion` and add `publicGroupsVersion`. No new fields, no new
  messages — `groupType = "group"` already round-trips through `textEncode`/`textDecode`.
- **Types** (`Types.hs`): add helpers `groupType'`, `isPublicGroup'`, `subscriberRoleFor`
  alongside `useRelays'`. Used everywhere the audit touches a `groupType` decision.
- **Command** (`Commands.hs`): add `groupType` field to `APINewPublicGroup`/`NewPublicGroup`;
  `/public group` parser gains `type=channel|group` (default `channel`). Substitute
  `subscriberRoleFor gType` for `channelSubscriberRole` reads at four sites.
- **Prefs defaults** (`Commands.hs:5380` area): `relayGroupProfile :: GroupType → Parser
  GroupProfile` collapses to `GTChannel → channelProfile, GTGroup → groupProfile`. Public
  group prefs equal secret-group prefs (only channel deviates with `support = OFF`).
- **Role derivation** (`Subscriber.hs`): `unknownMemberRole` and `createRelayLink` read
  `subscriberRoleFor (groupType' gInfo)` instead of config. Each relay derives the same
  default from the channel's immutable `groupType`.
- **Member-to-member DMs**: forward `XGrpDirectInv` through the relay scoped to a single
  recipient. (1) add to `isForwardedGroupMsg`; (2) add dispatch arm in `processForwardedMsg`;
  (3) introduce new `DJSDirectInv` job scope (parallel to `DJSMemberSupport` but no
  moderator broadcast — reusing support-scope would leak DM intent to mods, defeating the
  point); (4) gate on `directMessages` group preference. `xGrpDirectInv` parameterized via
  `DirectInvSource = DISDirect Connection | DISForwarded ForwardedMeta`.
- **Migrations**: none for type plumbing. The `sent_profile_vector` migration from the
  prerequisite dissemination plan is required.
- **Tests**: 13 new cases covering member posting, dissemination integration, member
  edits/deletes/reactions, member-DM creation, role/block changes, multi-relay,
  history-on-join, `asGroup=true` rejection, receipts above-limit, older-client refusal,
  incognito posting, incognito DM.

## iOS + Kotlin changes (mirror each other)

- **Model**: add `Group` variant to `GroupType`; add `isPublicGroup` and `groupType`
  accessors. iOS already has `isChannel`; Kotlin has it too — both stay channel-only.
- **Audit**: ~73 sites iOS, ~74 Kotlin where `useRelays` is used as a proxy for `isChannel`.
  Per-site rule: transport (topology/relay-management/icon) → keep `useRelays`; governance
  (titles, "subscribers", "channel preferences", member-vs-channel UX) → switch to
  `isChannel`. Concrete picks listed per-file with line numbers.
- **Create flow**: unified `AddChannelView` with a "Channel / Public group" segmented
  toggle; default to Channel. `groupType` parameter threads to `apiNewPublicGroup`. Help
  text on the create screen surfaces both threat-model trade-offs (relay can fabricate any
  member's content; relay sees DM-graph metadata).
- **Strings**: ~10 new keys per platform (`add_public_group`, `create_public_group`,
  `public_group_link`, threat-model + DM-metadata help text, etc.). Reuse `group_members_*`
  for "members" framing; channels keep `_subscriber*`.
- **Icons**: distinct icon for Public groups (separate from channel-antenna and
  secret-group-people). Pending design review.
- **Members view**: same relay-known list channels use; section header says "subscribers"
  for channels, "members" for Public groups.
- **ConnectPlan** (Kotlin): `ConnectPlan.kt:634` currently equates "uses relays" with "is
  channel" — fix to branch on `groupType` (Channel / Group / null / Unknown→reject).

## Threat model deltas (new §6)

Two changes from the channel threat model:

1. **Relay sees member DM graph** (new property). Relay learns (sender, target, time) on
   every `XGrpDirectInv`. Cannot read DM content (post-acceptance is P2P), cannot observe
   DM activity after invitation. Mitigations: incognito join; owner can disable
   `directMessages` preference. Default ON; create-flow help text surfaces the implication.
2. **Relay can fabricate content as any member** (broader than channels, where it could
   only fabricate as owners). Same deniability property as channels by design (unsigned
   content); future fix is opt-in content signing. Help text surfaces the trade-off.

Everything else (signed events, owner impersonation, participant privacy) carries over
unchanged.

## Migration & sequencing

- Existing channels untouched. Older clients decode `"group"` as `GTUnknown` and refuse to
  join with a "needs newer version" alert. Owner client and member clients need
  `publicGroupsVersion`. Older relays forward the wire correctly but assign `GRObserver`
  from config until upgraded — owner is warned at create time if any selected relay is
  pre-`publicGroupsVersion`.
- **Hard prerequisite**: member-profile dissemination plan
  (`2026-04-29-member-profile-sending-channels.md`) must land first.
- Ship order: backend types/command/role derivation → relay-forwarded `XGrpDirectInv` →
  iOS plumbing/audit/create → iOS views/icons/ConnectPlan → Kotlin plumbing/audit/create →
  Kotlin views/icons/ConnectPlan → older-client refusal + version-bump release notes.
  Platforms ship independently.

## Adjacent work, not planned

Owner→relay protocol-level role/rejection-rule communication, and owner-signature
verification on the channel profile by relays. Both real disparities, both apply to
channels equally, neither blocks Public groups.
