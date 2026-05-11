# Plan: Public groups via relays

Date: 2026-05-08

## 1. Overview

The previous release shipped **Channels** — relay-mediated groups where owners
publish and subscribers consume. The relay-forwarding pipeline is general:
`Subscriber.hs` already dispatches `XMsgNew`, edits, deletes, reactions, file
descriptors, profile updates, and leaves from any sender, and gates posting by
`memberRole > GRObserver` (`memberCanSend`, `Subscriber.hs:1551`). Subscribers
in channels are pinned to `GRObserver`, so they cannot post — but the machinery
to forward content from non-owners is present and exercised today
(`testChannels1RelayDeliver`, `tests/ChatTests/Groups.hs:8523`, where a
non-owner reaction reaches every member through the relay).

**Public groups** are the second value of the existing two-axis design: same
relay transport as channels, but every member can post like a regular "secret"
group. They give us scale (relay topology) without the broadcast governance.
The actual code blocker is narrow: there is no path that produces
`groupType = GTGroup`, `Commands.hs:2514` always writes `GTChannel`, and the
relay's joiner-role default is read from a global config (`channelSubscriberRole`)
instead of from a per-group value carried on the (owner-signed) channel profile.
Plumb the type through the create command, add a `memberRole` field to the
channel profile, populate it at create time, derive the relay's joiner role from
that field, integrate with the already-approved member-profile dissemination plan,
and the feature works.

Out of scope for this plan: **member-to-member DMs in relay-mediated groups**.
Member-to-member DM creation in any group with `useRelays = true` (Channel or
Public group) is prohibited at the client (no UI affordance) and defensively
refused on the receive path. The relay already does not forward `XGrpDirectInv`
(`Protocol.hs:484-503`, not in `isForwardedGroupMsg`), so no new server gate is
needed. This is deferred, not killed — see §10 for the design space.

## 2. Concept summary: the `useRelays × groupType` matrix

| `useRelays` | `groupType`             | Name              | Wire shape                                                                                | UX                                                                                                              |
|-------------|-------------------------|-------------------|-------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------|
| `false`     | (no `publicGroup`)      | **Secret group**  | P2P `x.grp.inv` invitations; full mesh between members; JSON array batch.                 | Today's group: all members can post; profiles known eagerly; admins moderate.                                   |
| `true`      | `GTChannel`             | **Channel**       | Relay-mediated; subscribers join via channel link; binary signed-batch format; profile carries `memberRole` (default `GRObserver` at creation). | Today's channel: only owners post; subscribers anonymous to each other.                                          |
| `true`      | `GTGroup`               | **Public group**  | Same wire as channel; profile carries `memberRole` (default `GRMember` at creation); profile dissemination on demand. | New: every member can post; member-to-member DMs prohibited (deferred); member roster grown lazily via on-demand profile send. |
| `true`      | `GTUnknown _` (decode)  | (refuse to join)  | Channel link from a newer client; older client sees unknown discriminator.                | New clients reject with a clear "needs newer version" message; pre-existing channels unaffected.                  |

Three derivations follow from this matrix:

- **Transport** is `useRelays` — affects connection topology, binary batch
  format, signatures, delivery pipeline, identity binding.
- **Governance model** is `groupType` (when `useRelays = true`) — affects
  profile dissemination strategy and member-to-member affordances. Member-to-
  member DMs are prohibited in any relay-mediated group regardless of
  `groupType` (deferred — see §10).
- **Joiner role** is a separate `memberRole` field on the channel profile, set
  by the owner at creation. Independent of `groupType`; the create flow picks
  the default from the chosen type (`GTChannel → GRObserver`,
  `GTGroup → GRMember`). The value travels with the (owner-signed) profile, so
  every relay derives the same role for the same group.

The existing iOS/Kotlin `isChannel = publicGroup?.groupType == channel`
distinguishes governance model (channel only). The existing `useRelays` boolean
distinguishes transport (channel or public group). Many call sites today use
`useRelays` as a proxy for `isChannel` because `GTGroup` doesn't exist yet —
those sites are the audit work.

## 3. Backend changes (Haskell, in `src/Simplex/Chat`)

The order below is the order the changes touch the wire/types/commands/runtime
pipeline. Within each subsection, files are listed in approximate touch order.

### 3.1 Wire format / protocol

The `groupType` discriminator does not need a wire change — `groupType = "group"`
is already a valid encoded value for the existing `GroupType` field on
`PublicGroupProfile` (`Types.hs:767-781`); today's encode/decode round-trips it
(`textEncode GTGroup = "group"`).

What does change:

- **New `memberRole :: Maybe GroupMemberRole` field on `PublicGroupProfile`.**
  Optional in the JSON encoding; `Nothing` means "no explicit value — fall back
  to the type-based default" at the relay and at the joiner. Existing channel
  profiles in the wild have no `memberRole` field, and decode to `Nothing`
  (preserving today's behavior — the type-based default for `GTChannel` is
  `GRObserver`). New profiles from the upgraded create flow carry an explicit
  value (`GRObserver` for `GTChannel`, `GRMember` for `GTGroup` at the defaults,
  or whatever the owner specified). The relay reads `memberRole` from its
  cached channel profile when deciding the joiner's role (§3.4) — every relay
  arrives at the same value because the value travels with the owner-signed
  profile, not from a relay-side config.

- **Chat protocol version bump.** Add `publicGroupsVersion :: VersionChat`
  set to the next available version (one higher than the current
  `currentChatVersion`) in `src/Simplex/Chat/Protocol.hs`. The new version
  signals that the peer understands `groupType = "group"` and the new
  `memberRole` field semantically. `Protocol.hs` already has the version-bump
  idiom (`shortLinkDataVersion`, `memberSupportVoiceVersion`).

- **Older-client / older-relay behaviour.** Older clients decode the
  channel link's `groupType` field as `GTUnknown "group"` (lossless tag
  preservation, `Types.hs:778-781`) and must refuse to join (§3.3). They
  ignore the unknown `memberRole` field on newer channel profiles
  (`aeson` default) and still treat `groupType = "channel"` correctly.
  Older relays forward Public-group traffic (pipeline is type-agnostic)
  but assign their global-config role to joiners — relay-upgrade
  ordering covered in §7.

No protocol doc changes are wire-mandatory, but `docs/protocol/simplex-chat.md`
("Channels: relay-mediated groups", line 269-273) and `channels-overview.md` /
`channels-protocol.md` should grow a brief paragraph explaining the
`groupType` discriminator, the new `memberRole` field, and that "Public groups"
is the second value. This is a docs task that pairs with the implementation,
not a separate gating step.

### 3.2 Type changes

The `GroupType` ADT (`Types.hs:767-771`) already has `GTGroup` — no change.
`PublicGroupProfile` (`Types.hs:787-804`) gains the new field:

```haskell
data PublicGroupProfile = PublicGroupProfile
  { groupType :: GroupType,
    memberRole :: Maybe GroupMemberRole,  -- NEW: owner-configured joiner role
    ...
  }
```

`Maybe` keeps existing rows decodable as `Nothing`; no data migration. The
encoder omits the field when `Nothing`.

Helper additions (all in `Types.hs`, alongside `useRelays'` at line 494 —
single-line definitions, no new module):

- `groupType' :: GroupInfo -> Maybe GroupType` — reads through
  `groupProfile.publicGroup?.groupType`. `Nothing` for P2P secret groups.
- `isPublicGroup' :: GroupInfo -> Bool` —
  `useRelays' && groupType' == Just GTGroup`. Used at audit sites and the
  defensive DM-refusal gate.
- `memberRole' :: GroupInfo -> Maybe GroupMemberRole` — reads through
  `groupProfile.publicGroup?.memberRole`.
- `defaultMemberRoleFor :: GroupType -> GroupMemberRole` — type-based
  default. `GTChannel → GRObserver`, `GTGroup → GRMember`, `GTUnknown _ →
  GRObserver` (defensive). Used at creation when the API caller omits the
  role, and at read when a profile has no `memberRole` field.
- `joinerRoleFor :: GroupInfo -> GroupMemberRole` — the canonical resolver:
  `fromMaybe (defaultMemberRoleFor <type>) memberRole'`. Used at every
  relay-side derivation site (§3.4).

`Types/Shared.hs` (the `GroupMemberRole` ADT, lines 18-51) is unchanged.
`Protocol.hs` `requiresSignature` (line 1221) is unchanged for the MVP — see
§3.4 for `unverifiedAllowed` tightening, which is a follow-up.

### 3.3 API/command changes

Goal: extend the existing `/public group` and `APINewPublicGroup` command with
a `groupType` parameter AND an optional `memberRole` parameter. Two parsers,
two constructor fields, two downstream substitutions.

**`src/Simplex/Chat/Controller.hs`**

- Line 525, `APINewPublicGroup` constructor — add `groupType :: GroupType`
  and `memberRole :: Maybe GroupMemberRole` fields. Owner specifies the kind
  (and optionally the joiner role) at creation time. `Nothing` for the role
  means "use `defaultMemberRoleFor groupType`".
- Line 528, `NewPublicGroup` command — same fields, parsed from the CLI form.
- Update `View.hs` and `Library/Commands.hs` to consume the new fields.
- Line 161 — **remove** `channelSubscriberRole :: GroupMemberRole`. After
  §3.4, no consumer remains. Removing is cleaner than leaving dead config;
  pick this over deprecation since the field is not exposed in any settings
  UI and no out-of-tree code is known to read it (verify in §8).

**`src/Simplex/Chat/Library/Commands.hs`**

- Line 2471-2527, `APINewPublicGroup` handler. Three substitutions:
  - Line 2514: `groupType = GTChannel` becomes `groupType = gType` (the new
    parameter).
  - Add population of `memberRole` on the constructed `PublicGroupProfile`:
    `memberRole = Just (fromMaybe (defaultMemberRoleFor gType) mRole)` where
    `mRole` is the new optional `memberRole` API parameter. The field is
    always populated with `Just` on profiles created by upgraded code (only
    pre-existing rows have `Nothing`); this gives readers a clear "new code"
    vs "old code" signal without breaking decoding.
  - Line 2522: `subRole <- asks $ channelSubscriberRole . config` becomes
    `let subRole = joinerRoleFor gInfo` (where `gInfo` is the just-created
    record carrying the new profile). The relay-link create now derives the
    role from the profile it is about to send, not from a side-channel
    config.
- Line 2024-2040, `APIPrepareGroup` handler. Line 2029:
  `subRole <- if useRelays then asks $ channelSubscriberRole . config else
  pure GRMember` becomes:
  ```haskell
  let subRole
        | useRelays = joinerRoleForLinkData groupSLinkData
        | otherwise = GRMember
  ```
  with `joinerRoleForLinkData :: GroupShortLinkData -> GroupMemberRole`
  doing the same resolve-or-default as `joinerRoleFor` but on the resolved
  link's profile. The subscriber sees the role directly from the channel
  link's `GroupShortLinkData`, so what they expect matches what the relay
  will give them.
- Line 5111-5112, parser. Extend `/public group` and `/_public group`
  parsers to accept an optional `type=channel|group` token AND an optional
  `member_role=observer|member|author|admin|owner` token; both default
  according to the `groupType` (member_role defaults to
  `defaultMemberRoleFor gType`). Existing scripts and tests continue to
  work without edits when both are omitted (gType defaults to `channel`).
- Line 4111-4116, `groupShortLinkPlan`. The `entityId == publicGroupId`
  check is unchanged; we already have the resolved `groupType` and
  `memberRole` from `groupSLinkData_` for downstream UX.

**`src/Simplex/Chat.hs`**

- Line 119: `channelSubscriberRole = GRObserver` — remove with the field.

**`tests/ChatClient.hs`**

- Line 214: `channelSubscriberRole = GRMember, -- starting role is GRMember
  to test members sending messages` — remove. Tests that need members to
  post will create Public groups (`GTGroup`) explicitly, or pass an
  explicit `memberRole` at creation; tests that exercise channels keep
  defaults.

#### 3.3.1 Default group preferences for Public groups

The CLI / API create handler builds the initial `GroupPreferences` that
ship in the channel link's `groupProfile`. Today there are two parsers:
`groupProfile` at `Library/Commands.hs:5371-5379` (used by `/group`,
secret-group default) and `channelProfile` at `Library/Commands.hs:5380-
5383` (used by `/public group`, channel default — overrides
`support = OFF` on top of the secret-group base). Public groups need a
third resolution.

Implementation: extend the existing parser by parameterization rather
than introducing a third sibling. `channelProfile` becomes
`relayGroupProfile :: GroupType -> Parser GroupProfile`, and the
overrides are decided from `gType`. The `/_public group` API path
threads the same `GroupType` from the new `APINewPublicGroup` field
(§3.3) into the prefs-resolution helper.

The table below names every preference, gives the secret-group, channel,
and Public-group defaults, and (where Public-group deviates from
secret-group) the reason. "Secret group" = output of the `groupProfile`
parser; "Channel" = output of `channelProfile`; both are grounded in
`Types/Preferences.hs:479-495` (`defaultGroupPrefs`) overlaid with the
parser-level overrides.

| Preference         | Secret group | Channel | **Public group** | Notes                                                                                          |
|--------------------|--------------|---------|------------------|------------------------------------------------------------------------------------------------|
| `timedMessages`    | OFF          | OFF     | **OFF**          | Match secret group. Owners can turn on per-channel.                                            |
| `directMessages`   | ON           | ON      | **ON**           | Inherited from secret-group default; **dormant in relay-mediated groups** — the relay does not forward `XGrpDirectInv` (`Protocol.hs:484-503`) and the client suppresses the DM affordance (§4.5, §5.2). Preference value is preserved on the wire so future plans can flip it on without a profile migration. No UI toggle in relay-mediated groups for the MVP. |
| `fullDelete`       | OFF          | OFF     | **OFF**          | Match secret group.                                                                            |
| `reactions`        | ON           | ON      | **ON**           | Match.                                                                                         |
| `voice`            | ON           | ON      | **ON**           | Match.                                                                                         |
| `files`            | ON           | ON      | **ON**           | Match.                                                                                         |
| `simplexLinks`     | ON           | ON      | **ON**           | Match.                                                                                         |
| `reports`          | ON           | ON      | **ON**           | Match secret group. Public groups can grow to thousands of members and need member-flagging from day one — disabling reports here would force owners to add them later. |
| `history`          | ON           | ON      | **ON**           | Match. New members joining via relay get recent history (relay path in `Subscriber.hs:888`).  |
| `support`          | ON           | OFF     | **ON**           | **Deviates from channel.** Channels disable support because broadcast publishers do not want a subscriber-to-owner side channel by default. Public groups are governed groups with members; member-to-moderator escalation is expected. Match secret group. |
| `sessions`         | OFF          | OFF     | **OFF**          | Match.                                                                                         |
| `comments`         | OFF          | OFF     | **OFF**          | Channels-specific feature; not part of the Public-group MVP.                                   |

Concretely: Public-group defaults equal secret-group defaults
(`directMessages`/`history` ON via parser overlay, `support` ON from
`defaultGroupPrefs`). The only Public-group-specific override needed at
the parser level is **none** — the existing `groupProfile` parser at
`Library/Commands.hs:5371-5379` already produces the correct shape.
The `relayGroupProfile` parameterization above collapses to:

```haskell
relayGroupProfile :: GroupType -> Parser GroupProfile
relayGroupProfile = \case
  GTChannel -> channelProfile  -- existing: support = OFF
  GTGroup   -> groupProfile    -- existing: support = ON
  GTUnknown _ -> groupProfile  -- defensive
```

This is a one-binding change to the parser; the `APINewPublicGroup`
JSON path is unchanged because the caller (mobile client, §4.3 / §5.3)
constructs `groupPreferences` explicitly. Mobile callers must construct
the right preferences for their chosen `groupType`; the iOS / Kotlin
create-flow sub-sections (§4.3, §5.3) gain a `groupType`-aware default-
prefs builder that mirrors this table.

### 3.4 Message processing changes

**`src/Simplex/Chat/Library/Subscriber.hs`**

- Line 3181-3184, `unknownMemberRole`. Currently:
  ```haskell
  unknownMemberRole gInfo
    | useRelays' gInfo = asks $ channelSubscriberRole . config
    | otherwise = pure GRAuthor
  ```
  becomes:
  ```haskell
  unknownMemberRole gInfo
    | useRelays' gInfo = pure $ joinerRoleFor gInfo
    | otherwise = pure GRAuthor
  ```
  The relay derives the joiner default from the channel profile's
  `memberRole` (with the type-based fallback for old profiles), not from a
  global config. Each relay arrives at the same default for the same group,
  eliminating the cross-relay disparity that motivated the existing TODO at
  line 3183.

- Line 3850-3852, `createRelayLink`. Same substitution:
  `subRole <- asks $ channelSubscriberRole . config` becomes
  `let subRole = joinerRoleFor gi`. The relay knows the group's profile
  (including `memberRole`) because `getLinkDataCreateRelayLink` (line 3814+)
  has just resolved and validated the channel profile's `PublicGroupProfile`.

- Line 2429, `processGroupInvitation`. The current check
  `isJust publicGroup = messageError "x.grp.inv: can't invite to channel"`
  is correct for channels and public groups alike — `x.grp.inv` is the
  legacy P2P group invitation; it has no place in relay-mediated groups
  regardless of `groupType`. **No change.**

- Line 3249+, `xGrpDirectInv` handler. Public groups (and channels) do not
  support member-to-member DMs in this plan. Defensive refusal at the top
  of the handler:
  ```haskell
  xGrpDirectInv gInfo m conn' connReq mContent_ msg msgTs = do
    when (useRelays' gInfo) $
      messageError "x.grp.direct.inv: member DMs are not supported in relay-mediated groups"
    -- existing body unchanged for secret groups
  ```
  In practice this arm is never reached in a relay-mediated group today,
  because (a) the relay does not forward `XGrpDirectInv`
  (`Protocol.hs:484-503`, not in `isForwardedGroupMsg`), and (b) there are
  no peer-to-peer connections between members of a relay-mediated group on
  which a direct `XGrpDirectInv` could arrive. The refusal is purely a
  defense against future code paths or peers with custom builds. Belt and
  suspenders: client-side suppression of the DM affordance (§4.5, §5.2),
  plus this receive-side gate.

- Line 1240-1249, `unverifiedAllowed`. Current behaviour: subscribers may
  pass unsigned `XGrpLeave` and `XInfo` between each other when the
  sender's key is not yet known. Once member-key dissemination ships
  (the sidecar `XGrpMemNew` carries `memberKey`, see
  `2026-04-29-member-profile-sending-channels.md` step 6), every
  subscriber-to-subscriber message has a known sender key, and the
  unverified path can be tightened. **For the Public groups MVP, leave
  `unverifiedAllowed` as-is** — tightening it is gated on the
  dissemination plan landing first.

  Concrete handoff: as part of this MVP, update the existing TODO
  comment at `Protocol.hs:1233-1235` to reference both this plan and
  the dissemination plan, naming the precondition for tightening:

  ```haskell
  -- TODO [public-groups]: tighten unverifiedAllowed for GTGroup once
  -- 2026-04-29-member-profile-sending-channels.md lands — keys for
  -- all members will be known via XGrpMemNew sidecar (step 6),
  -- making MSSSignedNoKey unnecessary for XGrpLeave/XInfo between
  -- subscribers. See plans/2026-05-08-public-groups-via-relays.md
  -- §3.4 and the dissemination plan §6.
  ```

  No separate plan file is created — the precondition and the action
  are both small, and the inline TODO with cross-references is enough
  for a future contributor to pick up.

- Line 985-989 / 2087-2089, `checkSendAsGroup`. Already restricts
  `asGroup = True` to `GROwner`. Public group members are typically
  `GRMember` (the default), so the existing gate naturally blocks them
  from sending as the group. **No change**, but add a test asserting that
  a Public-group member attempting `asGroup = True` sees the existing
  error message.

- Line 103-104 / 1042, `smallGroupsRcptsMemLimit`. The receipt gate is
  membership-count based, not transport-based — it already disables
  receipts above 20 members regardless of `useRelays`. Public groups
  inherit this. **No change.** Add a test that creates a Public group
  with > 20 members and asserts receipts are not requested.

- Line 887-897, channel join introduction. The existing path
  (`introduceInChannel` -> `sendHistory`) is independent of `groupType`
  — it runs whenever `useRelays' gInfo`. For Public groups, history
  default is ON (matching channels) and the same code path runs. No
  change required, but verify with a test that joins a Public group
  with two existing messages and asserts the joiner sees both.

**`src/Simplex/Chat/Library/Internal.hs`**

- Line 949, `acceptanceToStatus (memberAdmission groupProfile) gAccepted`
  is already evaluated for any group — channel or otherwise. The relay's
  `memberJoinRequestViaRelay` (`Subscriber.hs:1530-1541`) currently calls
  `acceptGroupJoinRequestAsync` with a hardcoded `GAAccepted`, bypassing
  any review/captcha admission. **For the Public groups MVP, do not
  honor `memberAdmission` on the relay side.** Reasons:
  (1) admission needs an owner-side decision in the current design, and
  the protocol path for a relay-routed pending-review handoff is
  unspecified;
  (2) it is not on the critical path for shipping;
  (3) it applies to channels equally — it is a generic "admission in
  relay-mediated groups" gap, not a Public-groups gap.
  Document this as a known limitation in the release notes and surface
  in §8 Open questions. The first review-sensitive Public group
  deployment will force the design conversation.

### 3.5 Database migrations

**None for the type plumbing.** `groupType` is already a column of
`group_profile.public_group` JSON (or wherever `PublicGroupProfile` is
serialised); existing rows have `"channel"` and new rows can have `"group"`.

**None for the `memberRole` field either.** It lives in the same
`PublicGroupProfile` JSON; absent on pre-upgrade rows, decoded as
`Nothing`, falls back to `defaultMemberRoleFor groupType` at read
(§3.2). Effective role for pre-existing channels is `GRObserver`,
matching today's global-config default.

**One migration arrives via the prior plan** (`sent_profile_vector BLOB`
column on `group_members`,
`2026-04-29-member-profile-sending-channels.md` §1). That migration is
required for Public groups (the "unknown member" UX is unacceptable when
every member is expected to post). It is not duplicated here — adopt the
prior plan as a hard prerequisite of this one.

### 3.6 Test additions

In `tests/ChatTests/Groups.hs`, mirror the channel helpers
(`prepareChannel'`, line 8583; `memberJoinChannel'`, line 8654) with
`preparePublicGroup'` and `memberJoinPublicGroup'`. The bodies are
near-identical, differing in (i) the create command's `type=group`
parameter, (ii) the joiner's role assertion (`GRMember` not
`GRObserver`), and (iii) the connect-plan message ("ok to join via
relays" vs "ok to connect via relays" — see §4 for the wording
decision).

Test cases (each new, all in the existing `describe "channels"` block or
a new sibling `describe "public groups"`):

1. **Member sends content; all members receive it.** Mirror
   `testChannels1RelayDeliver` (line 8523), but with cath as a Public
   group member, not a channel subscriber. Verify dan and eve receive
   cath's message attributed to cath (no "unknown member record" line
   — profile dissemination is active per §3.5 prerequisite).
2. **Profile dissemination integration.** Assert that no "unknown
   member" lines appear at any point in a multi-author Public-group
   session.
3. **Member edit / delete / react.** Each forwarded by the relay,
   each visible to all members.
4. **Member-to-member DM is rejected on receive.** Inject an
   `XGrpDirectInv` into the receive path of a Public-group member
   (bypassing the forwarder, which would not forward such a message
   anyway); verify the `messageError "x.grp.direct.inv: member DMs
   are not supported in relay-mediated groups"` is raised and no
   Contact is created. Repeat for a Channel. Mirrors the existing
   `XGrpInvitation` rejection test.
5. **Role changes on members.** Owner promotes a member to moderator;
   a moderator-only event verifies role propagation through the
   relay's signed forwarding.
6. **Blocking a member.** Owner blocks; verify the blocked member's
   subsequent messages are not forwarded (`blockedByAdmin m` gate at
   `Subscriber.hs:933`).
7. **Multi-relay delivery.** Mirror `testChannels2RelaysDeliver` for
   Public groups; verify cross-relay deduplication.
8. **History on join.** Send messages, then join a new member; verify
   history events arrive.
9. **`asGroup=true` rejection from member.** Member sends a message
   with `asGroup=true`; verify the existing "member is not allowed to
   send as group" error, and that no message item is created.
10. **Receipts disabled above limit.** Create a Public group with 21
    members; verify `aChatMsgHasReceipt` does not produce a receipt
    request.
11. **Older-client refusal.** Channel link with `groupType = "group"`;
    older client (chat version below `publicGroupsVersion`) sees
    `GTUnknown "group"` and refuses to join with a clear message.
12. **Incognito member posting.** Create a Public group; have a member
    join with `incognito = on`; member posts a content message;
    verify other members receive it attributed to the incognito
    profile name (not the member's real profile). Mirror the
    incognito-join helper used in `memberJoinChannelIncognito`
    (`tests/ChatTests/Groups.hs:8690`).
13. **`memberRole` propagates to relay.** Create a Public group with
    explicit `memberRole = GRAuthor`. A peer joining via the channel
    link gets `GRAuthor` (not the type default, not a config value).
    Also assert the channel link's resolved `GroupShortLinkData`
    carries the value.
14. **`memberRole` defaults.** Creating a Public group without
    specifying `memberRole` yields `GRMember`; creating a Channel
    without specifying `memberRole` yields `GRObserver`. Regression
    guard for both the type-default helper and the channel-migration
    story.
15. **Old-profile fallback.** Construct a `GroupShortLinkData` with
    `memberRole = Nothing` (simulating a pre-upgrade channel
    profile); peer joins; role derivation falls back to
    `defaultMemberRoleFor groupType` (`GRObserver` for `GTChannel`).

## 4. iOS changes (Swift, in `apps/ios/Shared`)

Order: model → audit → create flow → views.

### 4.1 Model

`apps/ios/SimpleXChat/ChatTypes.swift`:

- Line 2512-2532, `GroupType` enum. Add `case group`. Update
  `init(from:)` and `encode(to:)`. (`GroupInfo.isChannel` at line 2447
  and `GroupProfile.isChannel` at line 2576 keep their current
  semantics — channel only.)
- Add `memberRole: GroupMemberRole?` field on `PublicGroupProfile`.
  Mirror the Haskell wire: optional, omitted from JSON when nil.
- Add `var isPublicGroup: Bool { publicGroup?.groupType == .group }`
  on `GroupProfile` and `GroupInfo`.
- Add `var groupType: GroupType?` accessor on `GroupInfo` reading
  through `groupProfile.publicGroup?.groupType`.
- Add `var memberRole: GroupMemberRole?` accessor on `GroupInfo`
  reading through `groupProfile.publicGroup?.memberRole`. The client
  uses this only for display ("New members join as: Member") and for
  the create-flow plumbing; the authoritative resolution is on the
  Haskell side.

### 4.2 Audit `useRelays` vs `isChannel`

73 sites. The audit is mechanical but per-site:

For each `useRelays` site, ask: "is this branch about transport (uses
relays for delivery) or governance (channel-only — no member posts, no
member DMs, etc.)?" Choose `useRelays` for transport, `isChannel` for
governance. The list below names the tricky sites and the right
answer. Mostly the heuristic is: titles, subscriber/member labels,
and "channel preferences"-style strings are governance (use
`isChannel`); link-management, relay-management, "delete the
group/channel" prompts on the host side are transport-or-both — keep
`useRelays`. Member-to-member DM affordance suppression is
transport (any relay-mediated group), so **use `useRelays`** at sites
that gate DM creation.

Concrete picks (file names + line numbers verified at the time of
writing; verify with grep before editing):

- `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift`
  - Line 110, 172, 232, 240, 287, 376, 378 — these branch on
    "channel-style member display" vs "group-style member display".
    **Use `isChannel`.**
  - Line 95 (`useRelays && membership.memberIncognito`) — incognito
    flag display only matters for transport; **keep `useRelays`**.
  - Line 658, 662 — `GroupLinkView(isChannel:)` and "Channel link"
    title. The link is shared the same way for channels and Public
    groups (both are relay links), but the title and helper text
    differ. **Use `isChannel`** for the title; the underlying view
    already takes the boolean.
  - Line 784, 806, 825, 836, 873, 875 — "Edit channel profile",
    "Delete channel?", "Leave channel?". Wording follows governance,
    **use `isChannel`**.
  - Line 928, 944, 1004, 1026 — "subscribers" framing,
    "Channel preferences". Governance, **use `isChannel`**.
  - Member-tap "send direct message" affordance — suppress in any
    relay-mediated group; see §4.5.
- `apps/ios/Shared/Views/ChatList/ChatListNavLink.swift` — lines 247,
  272, 568, 623, 625. The "owner can't leave own relayed group"
  rule applies to both channels and Public groups (transport
  constraint — only the owner can rotate relays). **Keep
  `useRelays`** for the leave-suppression branch (247, 272). For
  the title strings (568, 623, 625), **use `isChannel`** — Public
  group owners see "Leave group?" / "Delete group?".
- `apps/ios/Shared/Views/Chat/ChatView.swift` — line 263, 596, 739,
  1087, 1090, 1091, 1092, 1120, 1446. All UX strings —
  **use `isChannel`** except line 596 (`incognito || useRelays`,
  which is about whether typing-indicator-style state can leak
  identity — that is transport, **keep `useRelays`**).
- `apps/ios/Shared/Views/Chat/Group/GroupPreferencesView.swift` —
  line 30, 33. Wording, **use `isChannel`**. Also: the
  `directMessages` preference row is suppressed entirely when
  `useRelays` (the preference is dormant in relay-mediated groups
  per §3.3.1).
- `apps/ios/Shared/Views/Chat/Group/GroupLinkView.swift` — line 20.
  This is a parameter; rename to `isChannel: Bool = false` and pass
  governance from callers. (Already named correctly — verify call
  sites pass `isChannel` not `useRelays`.)
- `apps/ios/Shared/Views/Chat/ChatInfoToolbar.swift` — line 86. The
  toolbar shows the relay-status indicator, which is transport,
  **keep `useRelays`**.

The audit is straightforward in aggregate: ~70% of sites should switch
to `isChannel`; the rest stay on `useRelays`. Rebuild after the audit
and visually compare a Public-group chat against a channel and a
secret group on a small test corpus.

### 4.3 Create flow

`apps/ios/Shared/Views/NewChat/AddChannelView.swift`. Two options:

- (A) Unified create flow with a "Channel / Public group" segmented
  control at the top. The two paths differ only in the `groupType`
  and `memberRole` parameters passed to `apiNewPublicGroup`.
- (B) Add `AddPublicGroupView` as a sibling, navigated to from the
  same "+" menu.

**Pick (A).** The two flows are 95% identical (relay selection,
profile entry, link generation). Duplicating the view doubles the
surface area for relay-management bugs. The toggle is a 3-line
addition. Place the toggle above the display-name field; default to
"Channel" so users who don't change anything keep current behavior.
Reflect the choice in the title ("Create channel" / "Create public
group"), the link-step screen ("Channel link" / "Public group link"),
and the success-screen wording.

The toggle drives two API parameters:
- `groupType = .channel` or `.group`
- `memberRole = .observer` or `.member` — derived from the toggle at
  the client (the type-default). For the MVP the create flow does not
  expose a separate role picker; power users may pass a non-default
  via direct API call.

`apps/ios/Shared/Model/AppAPITypes.swift` — `apiNewPublicGroup` (the
existing call, around `Model/SimpleXAPI.swift:1880-1882`) gains
`groupType: GroupType` and `memberRole: GroupMemberRole?` parameters;
default `.channel` and `nil` for one-line diffs at unaffected call
sites.

The Public-group create-flow screen carries one piece of help text
surfacing the threat-model trade-off (§6.A.1):

- Beneath the "Create public group" title, in the same position the
  "Create channel" screen uses for its description: *"In a Public
  group, every member can post. Messages are delivered through relays
  you choose, which means a malicious relay could change or fabricate
  messages from any member. Pick relays you trust."*

This string is listed in §4.4 as a new entry.

`groupPreferences` defaults builder: extend the existing builder used
by `AddChannelView.swift` to take a `groupType` and produce the
preferences from the table in §3.3.1 (`directMessages = ON`,
`history = ON`, `support = ON` for `groupType = .group`; existing
`support = OFF` override stays for `.channel`). The `directMessages`
toggle is not exposed in the create flow's prefs section when
`useRelays` (the preference is dormant per §3.3.1).

### 4.4 Strings

`apps/ios/Shared/en.lproj/Localizable.strings` — for each existing
`_channel` string that has a logical `_group` analog, audit whether
the existing `_group` form is correct for Public groups (usually it
is). For relay-specific strings ("relay", "subscribers" framed
neutrally), no Public-group variant is needed. Net: add ~5-10 new
strings, not 50. Strings to add (illustrative, names only):

- `add_public_group`, `create_public_group`, `your_public_group`
- `delete_public_group`, `leave_public_group`
- `public_group_link`
- `public_group_no_active_relays_try_later`
  (parallel to `channel_no_active_relays_try_later`)
- `public_group_temporarily_unavailable`
- `create_public_group_threat_model_note` — the create-flow paragraph
  from §4.3 (relay-can-fabricate framing).

The connect-plan-resolved message ("ok to connect via relays") needs
a Public-group form. See §4.6.

### 4.5 Compose / post permissions

Public group members are `GRMember` (or the configured `memberRole`).
Existing client-side gates check `memberRole > .observer` — these
naturally let members post. Audit
`apps/ios/Shared/Views/Chat/ComposeView.swift` for any
`useRelays && !isOwner` branch that suppresses composition; swap to
`isChannel && !isOwner` if it gates governance, leave alone if it
gates transport (e.g., owner-only relay-management hooks).

**Suppress the member-tap "send direct message" affordance** in any
relay-mediated group: the member context menu / profile view hides
the "Send direct message" entry when the containing group has
`useRelays`. Client-side half of the DM-prohibition decision (§1,
§3.4); receive-side gate is on the Haskell side.

### 4.6 Group info / link views / icons

- `GroupChatInfoView.swift` — three-way branching at the sites flagged
  in §4.2 (channel / public group / secret).
- `GroupProfileView.swift` — the profile-edit form already does not
  branch on `isChannel`; verify nothing in it assumes "no member
  posts".
- `GroupLinkView.swift` — title + helper text now needs three
  variants. Two changes: (i) extend `isChannel: Bool` parameter to
  an enum `LinkVariant { secret, publicGroup, channel }`, or (ii)
  pass `groupInfo` and read variant inside. Pick (ii) — fewer call
  sites to update.
- `GroupPreferencesView.swift` — the `directMessages` row is
  suppressed for any relay-mediated group (`useRelays`). The
  preference is dormant in those groups (§3.3.1); the toggle does
  nothing and would mislead users about what the relay sees.
- `chatIconName` (`ChatTypes.swift:2472-2482`):
  - `useRelays && isChannel` → existing antenna icon
    (`antenna.radiowaves.left.and.right.circle.fill`).
  - `useRelays && !isChannel` (Public group) → distinct icon. SF
    Symbols candidate: `person.3.sequence.fill` or
    `person.2.wave.2.fill`. Pick one in design review; the constraint
    is "visually distinct from both channel-antenna and
    secret-group-people". Mirror choice on Kotlin.
  - `!useRelays && businessChat == nil` → existing person.2.
  - business cases unchanged.
- Members view in `GroupChatInfoView`. Channels show the relay-known
  list which is thin (post-dissemination it grows). Public groups
  use the same list — once dissemination ships, the list is
  populated for any member who has interacted in the group.
  **Decision**: show all members the relay has announced (the same
  list as channels post-dissemination). Show "subscribers" for
  channels, "members" for Public groups in the section header. No
  separate "owner+moderators only" filter for the MVP; defer
  filtered-views to a follow-up.

### 4.7 Connect-plan messaging

`View.hs:2103-2105` produces "ok to connect via relays" for any
relay-mediated link. iOS reads this into the connect-plan flow
(`apps/ios/Shared/Views/NewChat/NewChatView.swift:1316`-area). For
Public groups, change wording to "ok to join via relays" (channel
phrasing is "ok to subscribe via relays"). The backend sends the
resolved `GroupShortLinkData`; the client decides wording from
`groupSLinkData.groupProfile.publicGroup?.groupType`.

## 5. Kotlin changes (Compose, in `apps/multiplatform/common`)

Mirrors §4. Same order: model → audit → create flow → views.

### 5.1 Model

`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`:

- Line 2186-2210, `GroupType` sealed class. Add
  `@Serializable @SerialName("group") object Group: GroupType()`. Add
  `"group" -> GroupType.Group` arm to the deserializer. Add the
  reverse arm to the serializer.
- Add `val memberRole: GroupMemberRole?` field on `PublicGroupProfile`,
  mirroring the Haskell wire: optional, omitted from JSON when null.
- Line 2231 — `isChannel` reads `groupType == GroupType.Channel`.
  Add `val isPublicGroup: Boolean get() = publicGroup?.groupType ==
  GroupType.Group` next to it.
- Line 2110 — `GroupInfo.isChannel` reads through to
  `GroupProfile.isChannel`. Add `val isPublicGroup` analogously.
- Add `val memberRole: GroupMemberRole?` accessor on `GroupInfo`
  reading through `groupProfile.publicGroup?.memberRole` (display
  only — see §4.1 rationale).

### 5.2 Audit `useRelays` vs `isChannel`

74 sites. Same per-site rule as iOS. Notable Kotlin-specific sites:

- `views/chatlist/ChatListNavLinkView.kt` — line 319, 341. Same
  leave-suppression as iOS. **Keep `useRelays`** (transport).
- `views/chatlist/ChatListView.kt` — line 1363, 1367. The
  classification of "group / channel / business" for the chat-list
  filter chips already uses `isChannel`. Add `isPublicGroup` to
  whichever bucket Public groups belong in (recommend: include in
  the "groups" bucket alongside secret groups, not in the "channels"
  bucket — the user mental model is "things I can post in").
- `views/chat/ComposeView.kt` — line 1444, 1541, 1621, 1623, 1696,
  2005. Mostly governance — switch to `isChannel`. Line 1696
  (`useRelays && memberRole >= GRMember.Owner`) gates the broadcast
  button; that's owner-channel-only. Switch to `isChannel && isOwner`.
- `views/chat/ChatView.kt` — line 206, 867, 1283, 1293, 1294, 1578,
  2242, 2244, 2247, 2248, 2249, 3207. Each is wording or icon.
  **Use `isChannel`** except line 1578 which is "subscriber count
  for relay-mediated groups" — that should also display for Public
  groups (member count from relay-side dissemination), so generalise
  to `useRelays`; the field name `publicMemberCount` stays as-is;
  the display label varies — "subscribers" for channels, "members"
  for Public groups.
- Member-tap "send direct message" affordance — suppress in any
  relay-mediated group (mirror iOS §4.5).
- `model/ChatModel.kt` line 4617, 4624, 4631 — group icons in
  `chatIconName`/`chatLinkText`. Add a third arm for Public groups
  with the chosen distinct icon (mirror iOS).
- `model/ChatModel.kt` lines 1621, 1628, 1693 — relay-broken /
  no-relays alerting. **Keep `useRelays`**, transport-level.
- `views/chat/group/GroupPreferences.kt` — lines 47, 60, 179, 183,
  229. Wording, **use `isChannel`**. The "save and notify
  subscribers" string should switch to "save and notify members" for
  Public groups. The `directMessages` row is suppressed for any
  relay-mediated group (mirror iOS §4.6).
- `views/chat/group/GroupLinkView.kt` — line 35, 175, 194, 196,
  217, 231, 236, 250, 274. Same pattern as iOS — pass `groupInfo`
  and derive wording/branches inside.

### 5.3 Create flow

`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/AddChannelView.kt`:

- Add a `groupType` state variable (default `GroupType.Channel`).
- Add a segmented toggle at the top of `ProfileStepView` (between
  the title and the name field).
- Pass `groupType` and `memberRole` to `apiNewPublicGroup`. The
  Haskell command parser defaults to channel and to the type-default
  role if either is absent (§3.3), so the Kotlin call site passes
  the chosen values directly.
- The `memberRole` value is derived from the toggle at the client:
  `GroupType.Channel → GRMember.Observer`,
  `GroupType.Group → GRMember.Member`. No separate role picker for
  the MVP.
- `groupPreferences` defaults: drive from the table in §3.3.1.
  Today's channel defaults are at line 115-117. Replace with a
  `groupType`-keyed builder:
  - `GroupType.Channel` → `directMessages = ON, history = ON,
    support = OFF` (today's behaviour, unchanged).
  - `GroupType.Group` → `directMessages = ON, history = ON,
    support = ON`.
- Title string and progress messages: thread through the choice.
- **Help text on the create screen**, mirroring iOS (§4.3):
  - Below the screen title, when `groupType = GroupType.Group`:
    *"In a Public group, every member can post. Messages are
    delivered through relays you choose, which means a malicious
    relay could change or fabricate messages from any member. Pick
    relays you trust."* (See §6.A.1.)
  This string is listed in §5.5 as a new MR key.

Either rename `AddChannelView` to `AddRelayedGroupView` or keep the
name and let it cover both kinds. Recommend keep the name to
minimise churn; the title varies inside.

### 5.4 ConnectPlan

`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/ConnectPlan.kt`:

- Line 634, `showPrepareGroupAlert`: `val isChannel =
  !(groupShortLinkInfo?.direct ?: true)` is **wrong for Public
  groups** — it equates "uses relays" with "is a channel". Fix:
  `val groupType = groupShortLinkData.groupProfile.publicGroup?.groupType`,
  branch:
  - `GroupType.Channel` → channel icon, "subscribers" subtitle,
    "Open new channel" confirm.
  - `GroupType.Group` → public-group icon, "members" subtitle,
    "Open new public group" confirm.
  - `null` (direct link, no `publicGroup` field) → secret-group icon
    and wording.
  - `GroupType.Unknown` → reject the link with a "needs newer
    version" alert and `cleanup()` (do not proceed to prepare).
- Line 472, `ownGroupLinkConfirmConnect` — same three-way branch on
  `groupInfo.groupType` (or `isChannel` / `isPublicGroup`).
- Line 295-318, `GroupLinkPlan.NoRelays` — wording is currently
  "channel temporarily unavailable". Generalise based on
  `groupSLinkData.groupProfile.publicGroup?.groupType`.

### 5.5 Strings

`apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`. Same
strategy as iOS: ~5-10 new keys, mostly mirroring the channel ones with a
`public_group` suffix:

- `chat_link_public_group`, `add_public_group`, `error_creating_public_group`
- `connect_plan_open_new_public_group`, `connect_plan_open_public_group`
- `you_will_join_public_group`
- `public_group_temporarily_unavailable`,
  `public_group_no_active_relays_try_later`
- `public_group_link`,
  `you_can_share_public_group_link_anybody_will_be_able_to_connect`
- `create_public_group_threat_model_note` — the create-flow
  paragraph from §5.3 (relay-can-fabricate framing).

For "subscribers" → "members" framing, prefer reusing the existing
`group_members_*` strings rather than introducing new public-group
variants. Channels keep their `_subscriber*` strings.

### 5.6 Icons

`ChatModel.kt:4617, 4624, 2128`. Add a third arm:

- `useRelays && isChannel` → existing `ic_bigtop_updates_circle_filled`.
- `useRelays && !isChannel` → new icon. Candidate:
  `ic_groups_2_filled` or a new asset that visually combines a
  "people" silhouette with a "broadcast" wedge. Pick in design
  review; mirror to iOS SF Symbol choice.
- `!useRelays && businessChat == null` → existing
  `ic_supervised_user_circle_filled`.

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

- **Existing channels are unaffected.** Pre-upgrade channel profiles
  have no `memberRole` field; readers decode the absent field as
  `Nothing` and `joinerRoleFor` falls back to `GRObserver`, matching
  today's behavior. No data migration.
- **Older clients** (chat version below `publicGroupsVersion`) decode
  `groupType = "group"` as `GTUnknown "group"`. They must refuse to
  join with a "this group requires a newer version of SimpleX Chat"
  alert (in `ConnectPlan.kt` / `NewChatView.swift`, §4.7/§5.4) —
  silently treating as channel would let owners post but block
  members. Older clients reading newer channel profiles (`groupType
  = "channel"` with explicit `memberRole`) ignore the unknown field
  and proceed as with today's channels.
- **Minimum versions.** `publicGroupsVersion` is the new floor.
  Owner client must be at least `publicGroupsVersion` to *create* a
  Public group. Member clients must be at least `publicGroupsVersion`
  to *join*. Older clients that are already members of a channel are
  unaffected.
  Older relays — currently relays accept any `groupType` and forward
  by `useRelays`, so they will forward Public-group traffic
  correctly without an upgrade. The relay-side profile-driven
  joiner-role derivation (§3.4) does require a relay upgrade for
  Public groups to function (a relay running old code would assign
  `GRObserver` from its config, blocking member posts even though
  the channel profile says otherwise). State this explicitly in
  release notes.
- **Relay upgrade ordering.** Owner upgrades first, then relays,
  then members. If an owner creates a Public group while one of
  their relays is still on old code, that relay assigns its config
  default (`GRObserver`) to joiners — members joining via that
  relay cannot post, but members joining via an upgraded relay can.
  The owner sees a partial-functionality state. Mitigation: warn at
  create time if any selected relay's chat version is below
  `publicGroupsVersion` (`Commands.hs` already has access to relay
  versions via the relay request flow). The warning is not a hard
  block — the owner may proceed knowing that some relays will
  reject member posts.

## 8. Open questions

1. **Future member-DM design.** Two directions: (i) forward
   `XGrpDirectInv` through the relay (simple, exposes member-DM
   graph metadata); (ii) relay-blind rendezvous (per-member SMP
   queue advertised on the profile; members initiate directly,
   relay never sees the pair) — more privacy-preserving, requires
   new protocol design. Out of scope here; either option must
   re-derive the §6 threat model.
2. **`memberAdmission` (review/captcha) on relay-mediated join.**
   Today, relay-side join short-circuits `GAAccepted` regardless of
   the channel's `memberAdmission` setting. This is a generic
   relay-side gap (channels and Public groups equally), out of scope
   for this feature. Surface in release notes; defer until a
   separate plan.
3. **Distinct icon for Public groups.** Pending design review on
   both platforms. The set must distinguish Public groups from
   channels (which use the broadcast/antenna metaphor) and from
   secret groups (which use a plain people metaphor). A "people +
   wedge" composite is the obvious candidate.
4. **Removing `channelSubscriberRole` from config.** The field has no
   callers after §3.3. Tests at `tests/ChatClient.hs:214` already
   override it for member-posting scenarios; those tests should
   become Public-group tests (or pass explicit `memberRole` at
   creation). Confirm that no out-of-tree consumer (CLI scripts,
   embedded clients) reads this config.
5. **`memberRole` on profile edit.** The field lives on a profile the
   owner can edit (`XGrpInfo`). MVP: no UI to change `memberRole`
   post-creation; the Haskell side accepts edits but role-rebase of
   existing members is undefined (new joiners get the new role,
   existing members keep theirs). Deferred until a UI need arises.
6. **Subscribed/unsubscribed roster filter in members view.** With
   100K+ Public-group members the relay-known list grows large.
   Should the client paginate / filter (e.g., "recently active
   only")? Out of scope for the MVP — the existing channel members
   view already handles this case for subscribers.
7. **Wording for connect plan**: "ok to join via relays" (Public group)
   vs "ok to subscribe via relays" (channel) vs "ok to connect via
   relays" (current, ambiguous). The CLI string in `View.hs:2105`
   is read by tests — update test expectations alongside the new
   wording. Mobile clients can derive their own.

## 9. Sequencing

1. **Prerequisite: member-profile dissemination plan**
   (`2026-04-29-member-profile-sending-channels.md`). Lands first,
   independently. Hard prerequisite — Public groups do not ship
   until dissemination has landed.
2. **Backend types + `memberRole` field + command + role derivation**
   (§3.1, §3.2, §3.3, §3.4). Single PR. Adds `GTGroup`-producing
   path, adds `memberRole` field to `PublicGroupProfile`, plumbs
   through create command, replaces config-based role lookup with
   profile-based derivation, removes `channelSubscriberRole` config,
   adds defensive `XGrpDirectInv` refusal in relay-mediated groups.
   Tests 1-15 pass at this stage.
3. **iOS plumbing + audit + create flow** (§4.1-§4.4). Single PR.
   No backend coupling — the `groupType` and `memberRole` parameters
   at the API level are already optional.
4. **iOS views, icons, ConnectPlan messaging, DM affordance
   suppression** (§4.5-§4.7). Independent of Kotlin.
5. **Kotlin plumbing + audit + create flow** (§5.1-§5.3). Mirror
   iOS.
6. **Kotlin views, icons, ConnectPlan, DM affordance suppression**
   (§5.4-§5.6).
7. **Older-client refusal, version bump release notes, docs
   updates** (§3.1, §7).

Steps 3-4 and 5-6 ship independently per platform — iOS can ship
Public groups without waiting on Kotlin and vice versa, as long as
the create-side defaults to channel for older clients (§3.3).

## 10. Adjacent work (one paragraph, not planned here)

Three pre-existing or deferred items are deliberately untouched.
(1) **Owner→relay communication of rejection rules**
(`Controller.hs:524`; `Commands.hs:2521`; `Subscriber.hs:1528-1529,
3850`). The joiner-role side is fixed by §3.2/§3.4 (the role now
travels on the owner-signed channel profile); the rejection-rule
side (admission/captcha) is still relay-side config. Future plan:
carry rejection rules on the channel profile too.
(2) **Owner signature verification on the channel profile by relays**
(`Subscriber.hs:3829`). Benefits channels equally; does not gate
this plan.
(3) **Member-to-member DMs in relay-mediated groups.** Deferred.
Design directions in §8 Q1; a future plan picking this up must
re-evaluate the §6 threat model, since relay-forwarded DMs would
re-introduce (sender, target, time) metadata exposure that this
plan avoids.
