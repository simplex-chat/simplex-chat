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

Two small additions:

- `Types.hs` — derive `groupType' :: GroupInfo -> Maybe GroupType` helper
  alongside `useRelays' :: GroupInfo -> Bool` (line 494). Lets call sites read
  governance without inlining the `groupProfile.publicGroup.groupType` walk.
  Optional but reduces site-by-site verbosity in `Subscriber.hs`/`Commands.hs`.
- `Types.hs` — derive `isPublicGroup' :: GroupInfo -> Bool` (true iff
  `useRelays' && groupType' == Just GTGroup`). One helper, used at the
  joiner-role derivation site and the DM-invite gate.

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
    happen). Place `subscriberRoleFor` next to the helpers in `Types.hs` or in
    a small `Subscriber/Roles.hs` if you want a single canonical site.
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
  record and creates a contact. When the message is forwarded and
  arrives via `processForwardedMsg`, the `conn'` argument the existing
  handler expects is the relay's connection — wrong. Either pass a
  marker/None and adapt `xGrpDirectInv` to handle the forwarded case,
  or introduce `xGrpDirectInvForwarded` that diverges only where it
  needs to. Prefer the latter — keeps the existing direct path
  unchanged.

- Line 1240-1249, `unverifiedAllowed`. Current behaviour: subscribers may
  pass unsigned `XGrpLeave` and `XInfo` between each other when the
  sender's key is not yet known. Once member-key dissemination ships
  (the sidecar `XGrpMemNew` carries `memberKey`, see
  `2026-04-29-member-profile-sending-channels.md` step 6), every
  subscriber-to-subscriber message has a known sender key, and the
  unverified path can be tightened. **For the Public groups MVP, leave
  `unverifiedAllowed` as-is** — tightening it is gated on the
  dissemination plan landing first. Track it as a follow-up.

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
  in §7 Open questions. The first review-sensitive Public group
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
- `groupPreferences` defaults: for Public groups,
  `directMessages = ON`, `history = ON`, `support = OFF`. Today's
  channel defaults are at line 115-117 — add a `when (groupType)`.
- Title string and progress messages: thread through the choice.

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

## 6. Migration / compatibility

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

## 7. Open questions

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

## 8. Sequencing

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
   updates** (§3.1, §6).

Steps 4-5 and 6-7 ship independently per platform — iOS can ship
Public groups without waiting on Kotlin and vice versa, as long as
the create-side defaults to channel for older clients (§3.3).

Steps 2 and 3 ship in either order; step 3 has no dependency on
step 2 other than the existence of `GTGroup` rows in the wild,
which step 2 enables.

## 9. Adjacent work (one paragraph, not planned here)

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
