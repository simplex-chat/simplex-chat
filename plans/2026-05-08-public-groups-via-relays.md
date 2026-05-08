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
instead of being derived from the channel's `groupType`. Plumb the type through
the create command, branch the joiner-role derivation on `groupType`, integrate
with the already-approved member-profile dissemination plan, and the feature
works.

## 2. Concept summary: the `useRelays × groupType` matrix

| `useRelays` | `groupType`             | Name              | Wire shape                                                                                | UX                                                                                                              |
|-------------|-------------------------|-------------------|-------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------|
| `false`     | (no `publicGroup`)      | **Secret group**  | P2P `x.grp.inv` invitations; full mesh between members; JSON array batch.                 | Today's group: all members can post; profiles known eagerly; admins moderate.                                   |
| `true`      | `GTChannel`             | **Channel**       | Relay-mediated; subscribers join via channel link; binary signed-batch format; `GRObserver`. | Today's channel: only owners post; subscribers anonymous to each other.                                          |
| `true`      | `GTGroup`               | **Public group**  | Same wire as channel; subscribers join with `GRMember`; profile dissemination on demand.  | New: every member can post; member-to-member DMs allowed; member roster grown lazily via on-demand profile send. |
| `true`      | `GTUnknown _` (decode)  | (refuse to join)  | Channel link from a newer client; older client sees unknown discriminator.                | New clients reject with a clear "needs newer version" message; pre-existing channels unaffected.                  |

Two derivations follow from this matrix:

- **Transport** is `useRelays` — affects connection topology, binary batch
  format, signatures, delivery pipeline, identity binding.
- **Governance** is `groupType` (when `useRelays = true`) — affects the joiner
  default role, profile dissemination, and member-to-member affordances.

The existing iOS/Kotlin `isChannel = publicGroup?.groupType == channel`
distinguishes governance (channel only). The existing `useRelays` boolean
distinguishes transport (channel or public group). Many call sites today use
`useRelays` as a proxy for `isChannel` because `GTGroup` doesn't exist yet —
those sites are the audit work.

## 3. Backend changes (Haskell, in `src/Simplex/Chat`)

The order below is the order the changes touch the wire/types/commands/runtime
pipeline. Within each subsection, files are listed in approximate touch order.

### 3.1 Wire format / protocol

The wire format does not change — `groupType = "group"` is already a valid
encoded value for the existing `GroupType` field on `PublicGroupProfile`
(`Types.hs:767-781`); today's encode/decode round-trips it (`textEncode GTGroup
= "group"`). No new fields, no new messages.

What does change:

- **Chat protocol version bump.** Add `publicGroupsVersion :: VersionChat =
  VersionChat 18` in `src/Simplex/Chat/Protocol.hs` (currently
  `currentChatVersion = VersionChat 17`, line 90). The new version signals
  that the peer understands `groupType = "group"` semantically. `Protocol.hs`
  already has the version-bump idiom (`shortLinkDataVersion = 16`,
  `memberSupportVoiceVersion = 17`).
- **Older-client behaviour.** Older clients decode the channel link's
  `groupType` field as `GTUnknown "group"` (lossless tag preservation in
  `textDecode`, `Types.hs:778-781`). They must refuse to join with a clear
  message; see §3.3.

No protocol doc changes are wire-mandatory, but `docs/protocol/simplex-chat.md`
("Channels: relay-mediated groups", line 269-273) and `channels-overview.md` /
`channels-protocol.md` should grow a brief paragraph explaining the
`groupType` discriminator and that "Public groups" is the second value. This
is a docs task that pairs with the implementation, not a separate gating step.

### 3.2 Type changes

Mostly inert. The `GroupType` ADT (`Types.hs:767-771`) already has `GTGroup` —
no change. The `PublicGroupProfile`/`GroupProfile` definitions
(`Types.hs:787-804`) are unchanged.

Two helper additions, both included in this MVP and used everywhere the
audit touches a `groupType` decision (no inline `groupProfile.publicGroup
.groupType` walks anywhere in the new code):

- `Types.hs` — `groupType' :: GroupInfo -> Maybe GroupType`, alongside
  `useRelays' :: GroupInfo -> Bool` (line 494). Returns `Nothing` for P2P
  secret groups (no `publicGroup` field).
- `Types.hs` — `isPublicGroup' :: GroupInfo -> Bool` (true iff
  `useRelays' && groupType' == Just GTGroup`). Used at the joiner-role
  derivation site and the DM-invite gate.

Both helpers live in `Types.hs` (not `Subscriber/Roles.hs` or any new
module) — same file as `useRelays'`, same single-line definitions,
zero new module dependencies.

`Types/Shared.hs` (the `GroupMemberRole` ADT, lines 18-51) is unchanged.
`Protocol.hs` `requiresSignature` (line 1221) is unchanged for the MVP — see
§3.4 for `unverifiedAllowed` tightening, which is a follow-up.

### 3.3 API/command changes

Goal: extend the existing `/public group` and `APINewPublicGroup` command with
a `groupType` parameter (option (a) in the brief). This is the smallest diff:
two parsers, one constructor field, one downstream substitution.

**`src/Simplex/Chat/Controller.hs`**

- Line 525, `APINewPublicGroup` constructor — add `groupType :: GroupType`
  field. Owner specifies the kind at creation time.
- Line 528, `NewPublicGroup` command — same field, parsed from the CLI form.
- Update `View.hs` and `Library/Commands.hs` to consume the new field.
- Remove or rename the `channelSubscriberRole :: GroupMemberRole` config field
  at line 161. See §3.4 — the config-driven default is replaced by a
  `groupType`-driven derivation, so the config has no callers after this
  change. Removing is cleaner than leaving dead config; if removal feels
  scary, mark it deprecated and stop reading it (no consumer remains).

**`src/Simplex/Chat/Library/Commands.hs`**

- Line 2471-2527, `APINewPublicGroup` handler. Two substitutions:
  - Line 2514: `groupType = GTChannel` becomes `groupType = gType` (the new
    parameter).
  - Line 2522: `subRole <- asks $ channelSubscriberRole . config` becomes
    `let subRole = subscriberRoleFor gType` where `subscriberRoleFor GTChannel
    = GRObserver; subscriberRoleFor GTGroup = GRMember; subscriberRoleFor
    (GTUnknown _) = GRObserver` (defensive: unknown means do not let posting
    happen). `subscriberRoleFor` lives in `Types.hs`, alongside `useRelays'`
    and the new `groupType'` / `isPublicGroup'` helpers — single canonical
    site, no new module.
- Line 2024-2040, `APIPrepareGroup` handler. Line 2029:
  `subRole <- if useRelays then asks $ channelSubscriberRole . config else
  pure GRMember` becomes `subRole = if useRelays then subscriberRoleFor (case
  groupSLinkData of GroupShortLinkData {groupProfile = GroupProfile
  {publicGroup = Just PublicGroupProfile {groupType = t}}} -> t; _ ->
  GTChannel) else GRMember`. The subscriber sees the `groupType` directly
  from the channel link's resolved `GroupShortLinkData`.
- Line 5111-5112, parser. Extend `/public group` and `/_public group`
  parsers to accept an optional `type=channel|group` token; default
  `channel` to keep all current scripts and tests working without edits.
- Line 4111-4116, `groupShortLinkPlan`. The `entityId == publicGroupId`
  check is unchanged; we already have the resolved `groupType` from
  `groupSLinkData_` if needed for downstream UX.

**`src/Simplex/Chat.hs`**

- Line 119: `channelSubscriberRole = GRObserver` — remove with the field
  (or stop passing it).

**`tests/ChatClient.hs`**

- Line 214: `channelSubscriberRole = GRMember, -- starting role is GRMember
  to test members sending messages` — remove. Tests that need members to
  post will create Public groups (`GTGroup`) explicitly; tests that exercise
  channels keep `GTChannel` defaults.

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
| `directMessages`   | ON           | ON      | **ON**           | Default ON. Members expect to DM each other in a "group". Help text surfaces the metadata implication (see threat-model §6.A.1). |
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
    | useRelays' gInfo = pure $ subscriberRoleFor (groupType' gInfo)
    | otherwise = pure GRAuthor
  ```
  The relay derives the joiner default from the channel's immutable
  `groupType` rather than its global config. Each relay arrives at the same
  default for the same group, eliminating the cross-relay disparity that
  motivated the existing TODO at line 3183.

- Line 3850-3852, `createRelayLink`. Same substitution:
  `subRole <- asks $ channelSubscriberRole . config` becomes
  `let subRole = subscriberRoleFor (groupType' gi)`. The relay knows the
  group's `groupType` because `getLinkDataCreateRelayLink` (line 3814+)
  has just resolved and validated the channel profile's
  `PublicGroupProfile`.

- Line 2429, `processGroupInvitation`. The current check
  `isJust publicGroup = messageError "x.grp.inv: can't invite to channel"`
  is correct for channels and public groups alike — `x.grp.inv` is the
  legacy P2P group invitation; it has no place in relay-mediated groups
  regardless of `groupType`. **No change.**

- §3.5 Member-to-member DM forwarding. `XGrpDirectInv` is currently NOT in
  `isForwardedGroupMsg` (`Protocol.hs:484-503`), and the forwarded-message
  dispatch in `processForwardedMsg` (`Subscriber.hs:3357-3378`) does not
  handle it. For Public groups, member A's DM invitation must reach
  member B via the relay, since A and B have no direct connection. The
  resulting direct contact (after B accepts) uses an ordinary SMP queue
  pair — peer-to-peer, relay not in the data path.

  Approach: forward `XGrpDirectInv` through the existing pipeline,
  scoped to a single recipient. Steps:

  1. Add `XGrpDirectInv {} -> True` to `isForwardedGroupMsg`
     (`Protocol.hs:484-503`).
  2. Add a dispatch arm in `processForwardedMsg` (`Subscriber.hs:3357+`):
     `XGrpDirectInv connReq mContent_ msgScope -> withAuthor
     XGrpDirectInv_ $ \author -> void $ memberCanSend (Just author)
     msgScope $ Nothing <$ xGrpDirectInv gInfo author conn connReq
     mContent_ rcvMsg msgTs`. Note that `xGrpDirectInv` (line 3249+)
     already gates on `groupFeatureMemberAllowed SGFDirectMessages`, so
     the DM preference is honored.
  3. The sender-side currently produces `XGrpDirectInv` with
     `msgScope = Just (MSMember recipientMemberId)`. Verify that the
     relay's delivery-task creation (`infoToDeliveryContext`,
     `Subscriber.hs:1811`/`2394`/`2115`) yields a `DJSMemberSupport`
     scope so the relay routes only to the target member, not to all
     members. If the existing support-scope path is the right substrate,
     reuse it; if support-scope semantics conflict, introduce a sibling
     `DJSDirectInv` scope. Decide during implementation by tracing one
     end-to-end DM flow with logging.
  4. Relay-side gate: only forward `XGrpDirectInv` when
     `groupFeatureMemberAllowed SGFDirectMessages senderMember gInfo`
     holds. The DM preference is already in `groupPreferences`; the
     relay reads it from its cached `groupProfile`. Add this check
     before creating the delivery task (in the dispatch at
     `Subscriber.hs:990-1027`, `XGrpDirectInv` arm).

  Edge case: `xGrpDirectInv` (line 3249+) currently writes a `Connection`
  record and creates a contact, using the `conn'` argument as the
  member-side connection on which the invitation arrived. The
  forwarded path has no such direct member connection — the message
  arrived through the relay. The two paths share ~95% of their body
  (preference gate, blocked-member check, contact creation, item
  rendering), differing only in (i) the source of the `Connection`
  record persisted with the new contact and (ii) which member-record
  lookup applies.

  Approach: **parameterize the existing handler.** Per `good-code-v4`
  §`<good-diff>` ("extend existing functions by parameterization
  rather than parallel implementations") — duplicating the body
  doubles the surface for blocked-member, preference, and contact-
  creation bugs. Introduce a discriminator parameter and route the
  two cases at the one site that actually differs:

  ```haskell
  data DirectInvSource
    = DISDirect Connection            -- existing path: A→B over group conn
    | DISForwarded ForwardedMeta      -- new path: A→relay→B
                                      -- ForwardedMeta carries what
                                      -- the forwarded path knows
                                      -- instead of the direct conn
                                      -- (sender memberId, broker ts,
                                      -- relay member record, etc.)

  xGrpDirectInv
    :: GroupInfo -> GroupMember -> DirectInvSource -> ConnReqInvitation
    -> Maybe MsgContent -> RcvMessage -> UTCTime -> CM ()
  ```

  Existing callers at `Subscriber.hs:1026` pass `DISDirect conn'`;
  the new arm in `processForwardedMsg` passes `DISForwarded
  forwardedMeta`. The body branches once at the contact-creation
  site (where `conn'` is consumed), and the rest of the function is
  unchanged. If during implementation the `DirectInvSource` split
  produces more conditionals than a duplicated body would (more than
  ~3 case-arms outside the contact-creation site), document the
  discovery and split — but the default is parameterization.

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
  for a future contributor to pick up. If the team later wants a full
  plan instead, name it `plans/{date-after-dissemination-lands}-
  tighten-unverified-allowed.md` and update the comment to point to
  it.

- Line 985-989 / 2087-2089, `checkSendAsGroup`. Already restricts
  `asGroup = True` to `GROwner`. Public group members are `GRMember`, so
  the existing gate naturally blocks them from sending as the group.
  **No change**, but add a test asserting that a Public-group member
  attempting `asGroup = True` sees the existing error message.

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
   cath's message attributed to cath (no "unknown member record"
   line — assumes the dissemination plan has landed).
2. **Profile dissemination integration.** With dissemination on, assert
   no "unknown member" lines appear.
3. **Member edit / delete / react.** Each forwarded by the relay,
   each visible to all members.
4. **Member-to-member DM creation.** Member A sends `/_create direct
   contact with @bob` (the existing `XGrpDirectInv` flow). Verify
   the resulting Contact, exchange a direct message, and assert the
   relay is not in the data path (no relay forwarding line for the
   direct message).
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
    older client (chat version 17) sees `GTUnknown "group"` and
    refuses to join with a clear message.
12. **Incognito member posting.** Create a Public group; have a member
    join with `incognito = on`; member posts a content message;
    verify other members receive it attributed to the incognito
    profile name (not the member's real profile). Mirror the
    incognito-join helper used in `memberJoinChannelIncognito`
    (`tests/ChatTests/Groups.hs:8690`).
13. **Incognito member-to-member DM.** With member-DMs enabled on the
    Public group, member A (joined incognito) creates a direct contact
    with member B via `XGrpDirectInv` (the test 4 path); verify the
    resulting P2P connection presents A's incognito profile to B and
    that A's subsequent direct messages preserve the incognito
    profile (no leak of the real user profile through the new direct
    contact, even after the connection moves off the relay).

## 4. iOS changes (Swift, in `apps/ios/Shared`)

Order: model → audit → create flow → views.

### 4.1 Model

`apps/ios/SimpleXChat/ChatTypes.swift`:

- Line 2512-2532, `GroupType` enum. Add `case group`. Update
  `init(from:)` and `encode(to:)`. (`GroupInfo.isChannel` at line 2447
  and `GroupProfile.isChannel` at line 2576 keep their current
  semantics — channel only.)
- Add `var isPublicGroup: Bool { publicGroup?.groupType == .group }`
  on `GroupProfile` and `GroupInfo`.
- Add `var groupType: GroupType?` accessor on `GroupInfo` reading
  through `groupProfile.publicGroup?.groupType`.

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
`useRelays`.

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
  line 30, 33. Wording, **use `isChannel`**.
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
  parameter passed to `apiNewPublicGroup`.
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

`apps/ios/Shared/Model/AppAPITypes.swift` — `apiNewPublicGroup` (the
existing call, around `Model/SimpleXAPI.swift:1880-1882`) gains a
`groupType: GroupType` parameter; default `.channel` for a one-line
diff at unaffected call sites.

The Public-group create-flow screen carries two pieces of help text
that surface the threat-model trade-offs (§6.A.1, §6.A.2):

- Beneath the "Create public group" title, in the same position the
  "Create channel" screen uses for its description: *"In a Public
  group, every member can post and DM. Messages are delivered through
  relays you choose, which means a malicious relay could change or
  fabricate messages from any member. Pick relays you trust."*
- On the `directMessages` preference toggle (in the prefs section of
  the create flow and in `GroupPreferencesView.swift`, see §4.6), as
  the off-state hint: *"If members can DM each other, your relay can
  see who started a conversation with whom — but not what they say.
  Turn off to keep DM-graph metadata private."*

Both strings are listed in §4.4 as new entries.

`groupPreferences` defaults builder: extend the existing builder used
by `AddChannelView.swift` to take a `groupType` and produce the
preferences from the table in §3.3.1 (`directMessages = ON`,
`history = ON`, `support = ON` for `groupType = .group`; existing
`support = OFF` override stays for `.channel`).

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
- `direct_messages_metadata_note` — the off-state hint on the
  `directMessages` toggle from §4.3 (DM-graph metadata framing).

The connect-plan-resolved message ("ok to connect via relays") needs
a Public-group form. See §4.6.

### 4.5 Compose / post permissions

Public group members are `GRMember`. Existing client-side gates check
`memberRole > .observer` (or equivalent) — these naturally let
members post. Audit `apps/ios/Shared/Views/Chat/ComposeView.swift`
for any `useRelays && !isOwner` branch that suppresses composition;
swap to `isChannel && !isOwner` if it gates governance, leave alone
if it gates transport (e.g., owner-only relay-management hooks).

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
- `GroupPreferencesView.swift` — preferences UI is unchanged for
  Public groups; the directMessages preference, which is
  channel-default-off, channel-Public-group-default-on, is already
  controlled by `groupPreferences`. Verify that creating a Public
  group sets `directMessages` to its default-on value (the existing
  default for non-channel groups). Pick this default at the
  `Commands.hs` create site (§3.3).
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
  populated for any member who has interacted in the group. **MVP
  decision**: show all members the relay has announced (the same
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
- Line 2231 — `isChannel` reads `groupType == GroupType.Channel`.
  Add `val isPublicGroup: Boolean get() = publicGroup?.groupType ==
  GroupType.Group` next to it.
- Line 2110 — `GroupInfo.isChannel` reads through to
  `GroupProfile.isChannel`. Add `val isPublicGroup` analogously.

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
  to `useRelays` and rename the field `publicMemberCount` → display
  it as "subscribers" for channels and "members" for Public groups.
- `model/ChatModel.kt` line 4617, 4624, 4631 — group icons in
  `chatIconName`/`chatLinkText`. Add a third arm for Public groups
  with the chosen distinct icon (mirror iOS).
- `model/ChatModel.kt` lines 1621, 1628, 1693 — relay-broken /
  no-relays alerting. **Keep `useRelays`**, transport-level.
- `views/chat/group/GroupPreferences.kt` — lines 47, 60, 179, 183,
  229. Wording, **use `isChannel`**. The "save and notify
  subscribers" string should switch to "save and notify members" for
  Public groups.
- `views/chat/group/GroupLinkView.kt` — line 35, 175, 194, 196,
  217, 231, 236, 250, 274. Same pattern as iOS — pass `groupInfo`
  and derive wording/branches inside.

### 5.3 Create flow

`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/AddChannelView.kt`:

- Add a `groupType` state variable (default `GroupType.Channel`).
- Add a segmented toggle at the top of `ProfileStepView` (between
  the title and the name field).
- Pass `groupType` to `apiNewPublicGroup`. The Haskell command parser
  defaults to channel if absent (§3.3), so the Kotlin call site
  passes the chosen value directly.
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
    *"In a Public group, every member can post and DM. Messages are
    delivered through relays you choose, which means a malicious
    relay could change or fabricate messages from any member. Pick
    relays you trust."* (See §6.A.2.)
  - On the `directMessages` toggle in the prefs section and in
    `views/chat/group/GroupPreferences.kt`: as the off-state hint,
    *"If members can DM each other, your relay can see who started
    a conversation with whom — but not what they say. Turn off to
    keep DM-graph metadata private."* (See §6.A.1.)
  Both strings are listed in §5.5 as new MR keys.

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
- `direct_messages_metadata_note` — the off-state hint on the
  `directMessages` toggle from §5.3 (DM-graph metadata framing).

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
inherit every property listed there. Two threats are *new* (channels do
not have them) and one is *broader* (channels have a narrower form of
the same threat). The relay's "can / cannot" framing matches the
existing doc style; the items below are written so they can be folded
directly into a future revision of `channels-overview.md` once Public
groups ship.

### 6.A.1 A relay observes the member DM graph

When a Public-group member initiates a DM with another member, the
client emits `XGrpDirectInv` and the relay forwards it (§3.4 sub-section
on member-to-member DM forwarding). The relay sees the (sender memberId,
target memberId) pair on every initial DM invitation. The resulting
Contact establishes a peer-to-peer SMP connection — the *content* of
subsequent direct messages never crosses the relay — but the *fact that
A wanted to talk to B* does. Over time, the relay accumulates a partial
DM graph of the Public group.

This is metadata the relay does not see in channels (members do not DM
in channels) and that no operator sees in secret groups (DM
invitations travel between members directly, no relay in the path).

**A single compromised relay**

*can:*

- Build a partial DM graph of the Public group from forwarded
  `XGrpDirectInv` events: every member who initiated a DM, every
  target member, and the time of initiation.
- Correlate that DM-initiation graph with the content authorship the
  relay already sees, deriving who-talks-to-whom signals beyond the
  group's public messages.
- Drop or selectively forward DM invitations, partitioning members
  who attempt to coordinate off-channel.

*cannot:*

- Read DM content. Once the recipient accepts the invitation, the
  resulting Contact uses an ordinary SMP queue pair — end-to-end
  encrypted at the agent layer, relay not in the data path.
- Observe DM activity after the initial invitation: subsequent
  messages, edits, reactions on the direct contact pass through SMP
  routers, not the relay.
- Determine the real-world identities of A or B. Each carries only
  their group-member profile (or an incognito profile if the member
  joined incognito — see Test 13 in §3.6). The relay sees member
  IDs, not user identities.
- Forge a DM invitation as if from a different member. The forwarded
  `XGrpDirectInv` is delivered with the original sender's memberId,
  and the recipient's client validates the in-group membership before
  accepting the contact.

**Mitigations.** Members who care about DM-graph privacy can join the
group incognito (the relay then sees only the incognito profile's
memberId, not anything correlatable across groups). Owners can
disable the `directMessages` group preference, removing the
forwarding path entirely (the relay rejects `XGrpDirectInv` at the
DM-preference gate, §3.4 sub-section step 4).

The owner-side default for `directMessages` in a new Public group is
**ON** (matches secret groups, §3.3.1). The create-flow help text
on iOS (§4.3, §4.4) and Kotlin (§5.3, §5.5) surfaces the metadata
implication in plain language: "If members can DM each other, your
relay can see who started a conversation with whom — but not what
they say. To prevent the relay from seeing this, turn off member-to-
member messages."

### 6.A.2 A relay can fabricate content as any member

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
  by every recipient against the channel link.
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
both platforms (§4.3, §5.3) includes a one-line trade-off framing:
"In a Public group, the relay forwards messages on behalf of every
member. A compromised relay could change message text or attribute
fabricated messages to any member. Use a secret group if you need
non-repudiable peer-to-peer messaging." This is the same trade-off
that channels make for owner posts; making it explicit at create
time lets users choose Public-group-via-relay vs secret-group based
on whether they value scale or content integrity.

### 6.A.3 What is unchanged from channels

Every other property of the channel threat model carries over
without change. In particular:

- A relay cannot impersonate an owner or substitute the channel
  profile (signed events, validated entity ID).
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

### 6.A.4 Release-notes line

For the Public-groups release notes, include a one-line summary of
both new properties:

> "In a Public group, the relay you choose can see who initiates
> direct conversations between members (but not message content),
> and could in principle alter or fabricate group messages
> attributed to any member. Pick relays you trust, or use a secret
> group if you need peer-to-peer message integrity."

## 7. Migration / compatibility

- **Existing channels are unaffected.** Channel profiles continue to
  carry `groupType = "channel"`; the new code path produces
  `GTGroup` only when explicitly requested.
- **Older clients** (chat version ≤ 17) decode `groupType = "group"`
  as `GTUnknown "group"`. They should not silently treat it as a
  channel — that would let owners post but block members and break
  the UX. Required client behavior: when about to join a link
  whose `publicGroup.groupType` is not recognised, show a clear
  "this group requires a newer version of SimpleX Chat" alert and
  block the join. Add this alert in `ConnectPlan.kt` /
  `NewChatView.swift` as part of §4.7/§5.4. The Haskell side does
  not need to refuse — the client decides.
- **Minimum versions.** Chat protocol version 18 (`publicGroupsVersion`)
  is the new floor. Owner client must be at least 18 to *create* a
  Public group. Member clients must be at least 18 to *join*. Older
  clients that are already members of a channel are unaffected.
  Older relays — currently relays accept any `groupType` and forward
  by `useRelays`, so they will forward Public-group traffic
  correctly without an upgrade. The relay-side type-driven
  joiner-role derivation (§3.4) does require a relay upgrade for
  Public groups to function (a relay running old code would assign
  `GRObserver` from its config, blocking member posts). State this
  explicitly in release notes.
- **Relay upgrade ordering.** Owner upgrades first, then relays,
  then members. If an owner creates a Public group while one of
  their relays is still on old code, that relay assigns `GRObserver`
  to joiners — members joining via that relay cannot post, but
  members joining via an upgraded relay can. The owner sees a
  partial-functionality state. Mitigation: warn at create time if
  any selected relay's chat version is < 18 (`Commands.hs` already
  has access to relay versions via the relay request flow). The
  warning is not a hard block — the owner may proceed knowing that
  some relays will reject member posts.

## 8. Open questions

1. **Forwarding scope for `XGrpDirectInv`.** The relay needs to deliver a
   single-target message. Reuse `MSMember` / `DJSMemberSupport`, or
   introduce a sibling scope for direct-invite delivery? The two
   semantics overlap (deliver to a specific member) but support-scope
   is also delivered to all moderators, which would leak a DM
   intention. Likely answer: introduce a new `DJSDirectInv` scope
   that delivers only to the target member. **Decide before
   implementing §3.4 step 3.**
2. **Member-DM consent.** P2P groups gate `XGrpDirectInv` by the
   `directMessages` group preference. Public groups inherit the
   same gate. Should owners get a per-channel additional toggle ("DMs
   between members allowed") or should the existing preference
   suffice? Recommend: existing preference is enough for MVP.
3. **`memberAdmission` (review/captcha) on relay-mediated join.**
   Today, relay-side join short-circuits `GAAccepted` regardless of
   the channel's `memberAdmission` setting. This is a generic
   relay-side gap (channels and Public groups equally), out of scope
   for this feature. Surface in release notes; defer until a
   separate plan.
4. **Distinct icon for Public groups.** Pending design review on
   both platforms. The set must distinguish Public groups from
   channels (which use the broadcast/antenna metaphor) and from
   secret groups (which use a plain people metaphor). A "people +
   wedge" composite is the obvious candidate.
5. **Removing `channelSubscriberRole` from config.** The field has no
   callers after §3.3. Tests at `tests/ChatClient.hs:214` already
   override it for member-posting scenarios; those tests should
   become Public-group tests. Confirm that no out-of-tree consumer
   (CLI scripts, embedded clients) reads this config.
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
8. **Adopt-the-prior-plan timing.** `2026-04-29-member-profile-sending-channels.md`
   is approved but unmerged at the time of writing. Public groups
   are *usable* without it but feel broken ("unknown member" lines
   on every member-authored message). Two ship orders:
   - (A) Land the dissemination plan first, then ship Public groups
     when it is in.
   - (B) Ship Public groups behind a feature flag while dissemination
     is in flight; flip the flag once dissemination lands.
   Recommend (A) — fewer states to support, less user confusion.

## 9. Sequencing

1. **Prerequisite: member-profile dissemination plan**
   (`2026-04-29-member-profile-sending-channels.md`). Lands first,
   independently. Public groups are a soft dependency: usable without
   it but UX-poor.
2. **Backend types + command + role derivation** (§3.2, §3.3, §3.4
   except DM forwarding). Single PR. Adds `GTGroup`-producing path,
   replaces config with `groupType`-based derivation, removes
   `channelSubscriberRole` config. Tests 1, 5, 8, 9, 10, 11 pass at
   this stage. Member DMs (test 4) and dissemination (test 2) are
   not yet in.
3. **Relay-forwarded `XGrpDirectInv`** (§3.4 sub-section 3.5, the
   member-to-member DM path). Single PR. Test 4 passes.
4. **iOS plumbing + audit + create flow** (§4.1-§4.4). Single PR.
   No backend coupling — the `groupType` parameter at the API level
   is already optional.
5. **iOS views, icons, ConnectPlan messaging** (§4.5-§4.7).
   Independent of Kotlin.
6. **Kotlin plumbing + audit + create flow** (§5.1-§5.3). Mirror
   iOS.
7. **Kotlin views, icons, ConnectPlan** (§5.4-§5.6).
8. **Older-client refusal, version bump release notes, docs
   updates** (§3.1, §7).

Steps 4-5 and 6-7 ship independently per platform — iOS can ship
Public groups without waiting on Kotlin and vice versa, as long as
the create-side defaults to channel for older clients (§3.3).

Steps 2 and 3 ship in either order; step 3 has no dependency on
step 2 other than the existence of `GTGroup` rows in the wild,
which step 2 enables.

## 10. Adjacent work (one paragraph, not planned here)

Two pre-existing channel-protocol disparities are deliberately
untouched. (1) **Owner→relay communication of joiner role and
rejection rules** (`Controller.hs:161, 524`; `Commands.hs:2521`;
`Subscriber.hs:1528-1529, 3850`). The cleaner long-term fix is to
carry the joiner role on the channel link analogously to
`GroupLink.acceptMemberRole` (`Types.hs:554`, default `GRMember` at
`Store/Groups.hs:316`) for regular groups, or include it in the
`x.grp.relay.inv` owner→relay message. Both are protocol extensions;
both benefit channels just as much as Public groups. (2) **Owner
signature verification on the channel profile by relays**
(`Subscriber.hs:3829`). Both are real and worth doing, but neither
gates Public groups: the `groupType`-derived joiner role makes the
config disparity moot for the only two values that matter
(`channel → GRObserver`, `group → GRMember`), and signature
verification of the profile is independent of `groupType`.
