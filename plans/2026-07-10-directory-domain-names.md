# Directory service: SimpleX domain names

Add SimpleX domain names (`#name`) of listed channels to the directory: show them in the
bot's chat output, include them in the generated web listing and on the directory web page,
and make both bot search and web search match them.

Prerequisites (part A, core):

1. `updateGroupFromLinkData` must stop marking unresolved claims as verified — today the
   link-refresh paths mark self-claimed names verified, which would make the directory an
   impersonation amplifier. It must also NOT resolve names itself: automatic resolution
   from a member's device is a metadata leak (the resolver learns which names the
   device's channels claim — membership metadata, on a refresh cadence).
2. `APIVerifyGroupDomain` must be fixed to do the verification actually needed:
   validate that the group name and the group link are **consistent** — resolving the
   claimed name leads to the link declared in the profile (`publicGroup.groupLink`).
   The link may differ from the one we joined through, and that is fine: link rotation
   is a supported direction, so no join-link comparison. Verification is explicit:
   users trigger it manually (or via the client's own policy); the directory — a public
   bot whose resolver queries reveal nothing personal — calls it directly.

Line refs are to the working tree at planning time; re-check before editing.

## 1. Current state (facts, with references)

### Names in core

- A channel's name is a claim on its group profile:
  `PublicGroupAccess.groupDomainClaim :: Maybe SimplexDomainClaim` (`Types.hs:847-853`),
  reachable as `publicGroup >>= publicGroupAccess >>= groupDomainClaim`. Only public,
  relay-backed groups (channels) have `publicGroup`; legacy p2p groups cannot have names.
- `SimplexDomainClaim = { domain :: StrJSON SimplexDomain, proof :: Maybe SimplexDomainProof }`
  (`Names.hs:49-53`). `SimplexDomain` (simplexmq `SimplexName.hs:40`) stores as TEXT via
  `fullDomainName` — e.g. `team.simplex` — in `group_profiles.group_domain`
  (row mapping `Store/Shared.hs:688,720-727`).
- Display form: `shortNameInfoStr (SimplexNameInfo NTPublicGroup domain)` → `#team` for a
  plain `.simplex` name, otherwise `#`+full domain (simplexmq `SimplexName.hs:118-125`).
  `groupSimplexDomain :: GroupInfo -> Maybe SimplexDomain` exists (`View.hs:1131-1133`;
  the module has no export list, so it is importable).
- Local status: `GroupInfo.groupDomainVerified :: Maybe Bool` (`Types.hs:502`), column
  `groups.group_domain_verified`, included in `groupInfoQueryFields`
  (`Store/Shared.hs:799-805`) — every directory store query already returns claim + status.
  `setGroupDomainVerified` (`Store/Groups.hs:2754`) writes it; `updateGroupProfile`
  already resets it to NULL when (and only when) the claim changes
  (`Store/Groups.hs:2712-2731`).
- The owner sets the name with `/public group access #g domain=...`
  (`Commands.hs:3143-3152`): the client resolves the domain and requires
  `nameResolvesTo groupLink` (`Commands.hs:4837-4838`) before storing. Client-side
  enforcement only — a modified client can claim any name.
- Core's own by-name lookup criterion: claim + `group_domain_verified = 1`
  (`getGroupToConnect`, `Store/Groups.hs:1086-1104`).
- Connect-by-name creates the group with the domain set and marked verified
  (`Store/Groups.hs:2759-2765`) — sound: the user's explicit action resolved the name.

### Defect 1: link-data refresh marks claims verified without resolution

`updateGroupFromLinkData` (`Internal.hs:1466-1488`) marks **any** claim present in
fetched link data as verified (`verifyChanged`, `:1488`). Call sites:

| # | Site | Reached by | Name resolved first? | Claim=name checked? |
|---|------|-----------|----------------------|---------------------|
| 1 | `APIGetUpdatedGroupLinkData`, `Commands.hs:1858` | refresh of the channel's own link data | NO (pure link fetch, `:1855`) | n/a |
| 2 | name-plan branch, `Commands.hs:4326` | `APIConnectPlan`, NAME target, group known by link | YES (`resolveNameLink` `:4303,4357-4363`) — explicit user action | yes, but AFTER the mark — `:4335` throws after `:4326` wrote the flag |
| 3 | `resolveKnownGroup` `Commands.hs:4346-4355`, LINK target | known group + `PRMAllGroups` — the directory's `deGroupLinkCheck` (`Service.hs:820-821`) and app link taps | NO (`resolveSLink` = `pure l'`, `:4290-4292`) | n/a |
| 4 | `resolveKnownGroup`, NAME target | reachable only under `PRMAllGroups`, which no caller sends with a name target (the directory sends link targets only, `Service.hs:563,821,957`; user connects use `PRMUnknown`, `Commands.hs:2288`) — currently unexercised | YES | n/a — the group was found by the verified-name lookup, so it is already verified |

The **contact** twin has sound semantics (`Commands.hs:4246-4253`):
`updateContactFromLinkData` runs only on the name path (`planDomain` guard, `:4249-4250`)
and the claim-equality check throws **before** it runs (`:4252-4253`). Note sites 1 and 3
are still needed for profile / member-count refresh — the fix is the verification rule
inside, not the calls.

### Defect 2: `APIVerifyGroupDomain` does not do the needed verification

`APIVerifyGroupDomain` (`Commands.hs:2301-2307`) delegates to `verifyEntityDomain`
(`:4840-4864`), which requires `claim.proof` and verifies a proof signature. For a public
group this is the wrong check: the claim arrives owner-signed (link data or signed
`XGrpInfo`), so claim authenticity is already given; what needs verification is
**consistency** — that resolving the claimed name leads to the link the profile
declares (not to the link we joined through, which may have rotated). The proof requirement
also makes the command inoperative today (nothing in this tree generates
`SimplexDomainProof`; it always returns "no name proof to verify").
`APIVerifyContactDomain` (`:2294-2300`) is a separate case (contact claims are not
link-bound the same way) and is out of scope.

### Directory service integration points

- Bot output text: `groupInfoText` (`Service.hs:364-370`) — used by `sendToApprove`
  (`:802-813`, admin approval message), `sendFoundGroups` (`:1287-1297`, search results),
  and `sendGroupsInfo` (`:1478-1495`, `/list`, `/last`, `/pending`). All call sites also
  have the `GroupReg`. The `/link` command's public-group branch prints the channel's
  link — the profile-declared `groupLink` (`:1204-1207`).
- Bot search: `searchListedGroups` (`Store.hs:357-411`); `searchCond` (`:404-411`) matches
  `gp.display_name`, `gp.full_name`, `gp.short_descr`, `gp.description` with `LIKE`.
- Web listing: `generateListing` (`Listing.hs:146-168`) writes `listing.json` /
  `promoted.json`; entries built by `groupDirectoryEntry` (`:99-144`) into
  `DirectoryEntry` (`:70-81`), JSON via `defaultJSON` — an optional field is
  wire-compatible.
- Web page: `website/src/js/directory.jsc` — display in `displayEntries` (`:212-341`),
  client-side search in `filterEntries` (`:98-112`); search input
  `website/src/directory.html:273`.
- Periodic link check: `linkCheckThread_` (`Service.hs:199-212`), default 1800s
  (`Options.hs:175-182`); `deGroupLinkCheck` (`:816-854`) sends a link-target
  `APIConnectPlan … PRMAllGroups` per active/pending group — targeting the
  profile-declared `groupLink` (`:818-820`) — and triggers `listingsUpdated` when
  `groupUpdated || summary /= groupSummary g'` (`:825`).

## 2. Design decisions (alternatives considered)

1. **Verification = name ↔ profile-link consistency.** Resolving the claimed name must
   lead to the link the profile declares (`nameResolvesTo (publicGroup.groupLink)`,
   helper at `Commands.hs:4837-4838`) — the same check the owner's client runs at set
   time (`APISetPublicGroupAccess`, `:3149-3150`), so the rule is uniform. The join
   link is NOT compared: link rotation is supported, a name may legitimately resolve to
   a link we never joined through. This is also exactly the property the directory
   publishes — the listing's join link IS the profile-declared link
   (`Listing.hs:124-126`), so a verified name is one that leads users to the same place
   as the entry itself. The profile is trusted (owner-signed provenance: link data or
   signed `XGrpInfo`; a profile update carrying a different `publicGroupId` is rejected —
   `Subscriber.hs:3684-3685` for `XGrpInfo`, extended to link-data-borne updates by
   A1's mirror guard). No proof is involved. Rejected alternatives:
   (a) comparing against the join link — breaks on every rotation; (b) fetching the
   resolved link's data and comparing `publicGroupId` — an extra network round-trip
   for a binding the consistency rule does not need.
2. **Verification is explicit, never automatic on a user's device.** The distinction
   is the request payload, not the destination — both operations are served via SMP
   servers: a link-data fetch references an opaque link ID (the serving host learns
   only that some client fetched that link — nothing name-shaped), while a resolution
   query carries the **readable name**, so whoever serves it learns which name this
   device is interested in. Automatic resolution on profile/link refresh would
   therefore leak membership metadata on a cadence; scheduled link-data fetches (the
   UI's chat-open refresh, the directory's link checks) are fine. So: claim change
   drops the status locally (already the case, `Groups.hs:2717`); resolution happens
   only when the user acts (verify command / client's own verify policy, or connecting
   by name, where resolution is the action itself). The directory, a public service,
   calls the verify API explicitly — its resolver queries reveal nothing personal.
3. **Connect-by-name paths still record verified** (sites 2 and 4 with resolved-domain
   context): the resolution already happened as the user's explicit action; storing the
   result adds no resolver call and no leak, and matches the existing
   created-as-verified behavior (`Groups.hs:2759-2765`).
4. **The directory displays claim + `group_domain_verified = 1`** — the identical
   criterion core uses for by-name lookup (`Groups.hs:1103`). User-facing surfaces
   (search results, `/list`, `/link`, listing JSON, web page) show only verified names;
   the admin approval message additionally shows an unverified claim, marked, because
   admins decide approval.
5. **Directory triggers**: call `APIVerifyGroupDomain` from the existing periodic link
   check for **every group with a claim, every cycle** — the directory re-publishes the
   name, so it must know the name is still working, not only that it worked once. One
   trigger covers the initial claim, claim changes, resolver-failure retries, an owner
   fixing the registry after a failed check, and a registry entry lapsing or being
   re-pointed (the name disappears within one link-check interval). No staleness state
   or timestamps needed. Clients do NOT re-verify on a schedule — only the directory.
   Plus a synchronous check when an approval request is composed (`sendToApprove`, B2)
   so admins always see the actual state. Cost: one resolver query per claimed group
   per cycle (default 30 min), plus one per approval request.
6. **No directory schema change, no directory-log change.**
7. **`groupInfoText` gains a `Maybe Text` name parameter** (the already-formatted line
   content), keeping the admin/user display policy visible at the call sites.
   Alternative: pass `(GroupInfo, GroupReg)` and decide inside — rejected: hides the
   policy in a text builder.

## 3. Part A — core fixes

### A1. `updateGroupFromLinkData` (`Internal.hs:1466-1488`): no marking without resolution, no resolving

The contract, on any group profile update:

1. name unchanged → verification status retained (including `Just False`);
2. name changed or removed → status reset to unknown (NULL);
3. name added → status remains unknown (NULL).

The store layer already enforces all three: `updateGroupProfile` compares old vs new
claim domain and resets to NULL exactly when it changed (`Groups.hs:2712-2717`,
in-memory copy at `:2714`). Every caller is clean — `xGrpInfo` (`Subscriber.hs:3690`,
with `publicGroupId` immutability guards `:3684-3686`), the relay accept path
(`Subscriber.hs:4403`, `publicGroupId` check `:4396-4399`), `runUpdateGroupProfile`
(`Commands.hs:3913`), `createGroupInvitation` (`Groups.hs:474`), the member-update path
(`Groups.hs:815-816`) — **except** `updateGroupFromLinkData` (`Internal.hs:1478`), which
overrides the reset and marks `Just True` whenever a claim is present (`verifyChanged`
`:1488`). It thereby violates all three invariants; the worst case is invariant 1: an
unchanged claim with status `Just False` is overwritten to `Just True` — apps call
`APIGetUpdatedGroupLinkData` on every chat open (`ChatView.swift:773`,
`ChatView.kt:216`), so a failed manual verification is silently erased to "verified"
the next time the user opens the channel.

Fix: add a parameter `resolvedDomain_ :: Maybe SimplexDomain` — the domain the caller
just resolved (by explicit user action) to reach this link data; `Nothing` otherwise.
Rule:

- set verified (`Just True`) exactly when `resolvedDomain_` equals the incoming claim's
  domain (the connect-by-name exception, decision 3 — same class as prepared-group
  creation, `Groups.hs:671-674`). Do NOT also gate on the current status being
  not-`Just True`: `updateGroupProfile` runs first and, on a claim rotation, resets the
  status to NULL, so a stale pre-update `Just True` would wrongly suppress the re-mark of
  the newly-resolved name — leaving it NULL. The write is idempotent, so re-marking an
  already-verified same-name group is a harmless no-op;
- this marking condition is part of the function's **entry guard**
  (`profileChanged || countChanged || verifyResolved`) — a name-plan hit on an unchanged
  profile with a NULL status must still mark, even though nothing else changed;
- otherwise never touch the status — the store layer's three invariants then hold on
  every path;
- **no resolver calls in this function** (decision 2 — metadata leak).

Call sites:

The whole call-site diff is one added argument in three places:

- `Commands.hs:1858` (`APIGetUpdatedGroupLinkData`): pass `Nothing` — pure refresh.
- `Commands.hs:4326` (name-plan branch): pass `Just nameDomain` (bind it in the case
  alternative that currently discards it, `(Just _, …)` → `(Just nameDomain, …)`).
  No reordering: the existing claim check at `:4328-4335` stays where it is — the
  marking rule itself prevents the status write when the fresh claim differs from the
  resolved name, so a mismatched claim throws with no status written. The profile
  refresh that runs before the throw is the group's own link data and is legitimate.
- `Commands.hs:4353` (`resolveKnownGroup`): pass `Nothing` — a link-refresh path.
  (With a name target it is reachable only under `PRMAllGroups`, which no caller sends
  with a name — see §6; and a group found by name is already verified,
  `Groups.hs:1103`, so marking would be a no-op there anyway.)

Nothing moves, nothing reorders; lookup semantics (name → local claim+verified match,
link → stored link match, miss → resolve → OK plan) are untouched.

Behavior change: members who joined via link no longer see
"SimpleX name: #x (verified)" (`simplexDomainLine`, `View.hs:1145-1154`) from the
self-claim alone; the status is NULL (shown unverified) until the user verifies — or
the client's verify policy does on opening group info (`SimplexNameView.kt:62-63`).

### A2. Fix `APIVerifyGroupDomain` (`Commands.hs:2301-2307`)

Applies to public groups (channels) only — claims exist only on `publicGroup` profiles;
p2p groups cannot carry one (`xGrpInfo` rejects it, `Subscriber.hs:3686`); business
groups use the separate address-anchored mechanism (`setPreparedGroupDomain`,
`Groups.hs:2758-2769`, domain on `businessChat.businessDomain`); contacts keep
`APIVerifyContactDomain` untouched.

The check is name ↔ profile-link consistency (decision 1); no join-link comparison and
no `preparedGroup` involvement — the flow works identically for the owner, a member,
and the directory bot, before or after link rotation:

1. Require a claim (`publicGroup >>= publicGroupAccess >>= groupDomainClaim`) — as now.
2. `resolveSimplexName` (agent) on the claimed domain →
   `nameResolvesTo (publicGroup.groupLink) nrSimplexChannel`
   (helper at `Commands.hs:4837-4838`; `groupLink` is a mandatory field of
   `PublicGroupProfile`, `Types.hs:855-861`) — the same consistency check the owner's
   client runs at set time (`:3149-3150`).
3. Holds → `Just True`, else `Just False`, via `setGroupDomainVerified`; respond
   `CRGroupDomainVerified user g' reason` as now. A definite NAME NOT_FOUND resolver
   answer (cf. `tests/ChatTests/Names.hs:100-102`) is `Just False`; network/transport
   errors remain thrown `ChatErrorAgent` (retryable, status unchanged).
4. No proof involvement for groups. (`verifyEntityDomain`'s proof logic remains for
   `APIVerifyContactDomain`, untouched.)

Division of labor — the verify API itself performs only the resolver query; it does
NOT fetch link data, because the profile it checks against is kept fresh by the
existing link-data fetches, which continue on their own cadence:

- the directory fetches link data on every link-check cycle (`deGroupLinkCheck` →
  connect plan, `Service.hs:820-821`), refreshing the profile (claim and `groupLink`)
  before the verify call runs in the same cycle;
- the UI fetches on chat open (`APIGetUpdatedGroupLinkData` — `ChatView.swift:773`,
  `ChatView.kt:216`), so by the time group info's auto/manual verify fires
  (`SimplexNameView.kt:62-63`) the profile is current;
- per A1, none of those fetches touch the verification status — they only refresh the
  data that verification is checked against.

The verify API is called only on user interaction (manual, or the client's verify
policy) and from the directory (B2) — never on a schedule from a user's device.

### A3. Owner set-name marks verified (`APISetPublicGroupAccess`, `Commands.hs:3143-3152`)

Setting a new/changed domain already resolves it and requires
`nameResolvesTo groupLink` as a precondition (`:3147-3150`) — but the subsequent
`runUpdateGroupProfile` resets the status to NULL (claim changed), so the owner's own
channel reads as unverified right after a successful set. Fix: in the
`APISetPublicGroupAccess` handler, after the profile update, set the status `Just True`
— only on the branch where the resolution check ran (new/changed domain). Setting the
same domain skips resolution and retains the status (invariant 1); clearing the domain
leaves NULL (invariant 2). This is the same trust event as connect-by-name: resolution
by explicit user action, persisted immediately. The UI sets channel names through this
command (`GroupChatInfoView.kt:192`), so the fix covers the app flow. Note the returned
`CRGroupUpdated` must carry the updated status (the UI reads the flag off the returned
`GroupInfo`) — set the flag after `runUpdateGroupProfile` and patch or re-read the
group for the response.

### A4. Core tests (`tests/ChatTests/Names.hs`, resolver stub `tests/NameResolver.hs`)

1. Link-target plan / link-data refresh does NOT change the verification status and does
   NOT query the resolver (register a counter in the `NameResolver` stub registry, or
   assert status stays NULL after refresh) — regression for sites 1 and 3 and for the
   no-auto-resolution rule.
2. The three invariants across a profile update cycle: unchanged claim retains the
   status — specifically `Just False` survives a link-data refresh / chat open
   (regression for the erased-failure defect); changed claim resets to NULL; added
   claim stays NULL.
3. `/_verify domain #<gId>`: name resolving to the profile-declared link → verified
   (this is also the rotation case — the check never touches the join link); resolving
   to a different link → failed; NOT_FOUND → failed; resolver unreachable → error,
   status unchanged.
4. Claim change resets the status; the by-name lookup (`getGroupToConnect`) stops
   matching until re-verified.
5. Name-plan mismatch: link data claiming a different name than the resolved one throws
   `CESimplexDomainNotReady` and the status is not written (the marking rule requires
   the fresh claim to equal the resolved name).
6. Owner sets the name (`/public group access ... domain=`): status is `Just True`
   after a successful set of a new domain; re-sending the same domain retains the
   status; clearing it resets to NULL.

## 4. Part B — directory service

### B1. Verified-name accessor

`verifiedGroupDomain :: GroupInfo -> Maybe SimplexDomain` — the claim
(`groupSimplexDomain`) when `groupDomainVerified == Just True`; in `Directory.Store`
(used by both `Service.hs` and `Listing.hs`). Display string:
`shortNameInfoStr . SimplexNameInfo NTPublicGroup`.

### B2. Verification trigger (`Service.hs`)

- In `deGroupLinkCheck` (`:816-854`), after the existing owner check: when the group
  profile has a claim, send `APIVerifyGroupDomain groupId` — every cycle, regardless of
  the current status (decision 5: the directory must know the name is still working).
  The ordering matters: the verify call runs after the cycle's connect-plan call has
  fetched fresh link data, so consistency is checked against the current claim and
  `groupLink`, not day-old state.
  On `CRGroupDomainVerified {groupInfo = g'}` with a status different from before and
  the group listed → `listingsUpdated env`. Errors (`Left` network/timeout) → log,
  status unchanged, retry next cycle.
- Also extend the existing `listingsUpdated` condition (`:825`) with
  `groupDomainVerified g' /= groupDomainVerified gInfo` for the plan-driven updates.
- Verify **synchronously in `sendToApprove`** (`:802-813`), before composing the admin
  message: when the group has a claim and the status is not `Just True`, run
  `APIVerifyGroupDomain` and use the returned `GroupInfo` for the message. This is the
  single choke point through which every approval request flows — registration
  (`deMemberUpdated` `:1067-1070`), re-registration (`deReregistration` →
  `pendingApprovalTransition` `:1051-1056`), profile changes
  (`publicGroupProfileChange` `:552-577`), and link-check reapprovals (`:843-853`) — so
  admins always see the actual verification state. A registration-time
  `DEGroupLinkCheck` nudge would NOT work: at `joinAndRegisterPublicGroup` the group is
  still connecting (the link-target plan returns `GLPConnectingProhibit`, and
  `deGroupLinkCheck` only covers `GRSActive`/pending-approval states, `:819`), and an
  async verify would race the approval message. The event loop is already sequential
  and network-bearing; one added round-trip per approval is in pattern.

### B3. Bot output (`Service.hs`)

- `groupInfoText` (`:364`): add `Maybe Text` name parameter; when present, a line
  `SimpleX name: #team` after `groupNameDescr` (wording mirrors core `View.hs:1175`).
- User-facing call sites pass the display string of `verifiedGroupDomain`:
  `sendFoundGroups` (`:1292-1296`), `sendGroupsInfo` (`:1486-1494`).
- Admin call site `sendToApprove` (`:802-813`): verified → `#team`; claimed but not
  verified → `#team (NOT verified - will not be shown)`.
- `/link` command public branch (`:1204-1207`): add the verified-name line under the link.

### B4. Bot search (`Store.hs`)

`searchListedGroups` `STSearch` (`:386-398`): normalize the query — after `T.toLower`,
strip one leading `#` or `@` (aligning with the web page's normalization) — and extend
`searchCond` (`:404-411`) with a parenthesized branch:
`OR (LOWER(gp.group_domain) LIKE '%' || ? || '%' AND g.group_domain_verified = 1)`.
The stored form is the full domain (`team.simplex`), so `team`, `#team`, and
`team.simplex` all match. Parameter tuples in all four query/count variants gain one
param (the `STSearch` count query already joins `group_profiles`; `g` is in scope in
both — the main query via `groupInfoQueryFrom`, `Store/Shared.hs:816-823`). Guard the
edge case: when the normalized domain query is empty (the search was just `#`),
`LIKE '%%'` would match every named group — pass a value that cannot match (or skip the
branch) instead.

### B5. Web listing (`Listing.hs`)

- `DirectoryEntry` (`:70-81`): add `simplexName :: Maybe Text` (display form `#team`);
  `defaultJSON` omits `Nothing` (simplexmq `Parsers.hs:153`), so the field is
  wire-compatible with existing consumers. `promoted.json` inherits.
- `groupDirectoryEntry` (`:99`): no new parameter — it already takes `GroupInfo`, and
  `verifiedGroupDomain` needs only `GroupInfo` (claim and status both live there);
  compute the field inside. `Listing.hs` already imports `Directory.Store` (`:34`).

### B6. Web page (`website/src/js/directory.jsc`, `website/src/directory.html`)

- `displayEntries` (`:225-228`): when `entry.simplexName` is set, render it under the
  `h2` display name (small accent-styled line, same pattern as other meta lines). Use
  `textContent`, not `innerHTML` — same as `displayName` (`:226`); domains are
  parser-constrained to ASCII (`nameLabelP`, simplexmq `SimplexName.hs:59-71`), but the
  listing file is an external input to the page.
- `filterEntries` (`:98-112`): add a `simplexName` match, normalizing both sides:
  lowercase, strip leading `#`/`@`, strip trailing `.simplex` — so `#team`, `team`, and
  `team.simplex` all match an entry displaying `#team`.
- Optional: `#search` placeholder copy in `directory.html`.

### B7. Directory tests (`tests/Bots/DirectoryTests.hs`)

Directory name tests run under `withSmpServerAndNames` (`tests/ChatClient.hs:596`) — the
directory harness uses the ambient SMP server, so they are a **separate spec**
(`directoryNameTests`) wired in `Test.hs` under the names-capable `tmpTestBracket`
(mirroring `chatNamesTests`), not folded into `directoryServiceTests` (which runs under a
plain SMP server). Channel setup follows `testRegisterChannelViaCard` (`:2042`).

Implemented:
1. **Verified name shown** (`testDirectoryChannelName`): resolver maps `news` to the
   channel's link → owner sets `domain=news.simplex` → register via card → the directory
   verifies name↔link consistency and the admin approval message shows `SimpleX name: #news`.
2. **Unverified claim marked** (`testDirectoryChannelNameNotVerified`): the registry entry
   is re-pointed to a different link after the owner set the name → the directory's
   verification fails → the admin sees `SimpleX name: #news (NOT verified - will not be shown)`.

These exercise the security-critical properties end-to-end: verification (A2), the
`sendToApprove` trigger (B2), the display line (B3), and the verified-only display gating.

Not yet written (lower-risk; the underlying code is covered by the core tests A4 and the
two above): search-by-name (B4, `#team`/`team`/`team.simplex`), a `listing.json`
`simplexName` assertion (B5, extend `checkListings` `:1208-1217`), and re-verification
across a `deGroupLinkCheck` cycle (B2).

## 5. Sequencing (one logical change per commit)

1. Core: `updateGroupFromLinkData` — resolved-domain parameter (one added argument at
   three call sites), no marking without it, no auto-resolution
   (+ core tests A4 1, 2, 5).
2. Core: `APIVerifyGroupDomain` — name ↔ profile-link consistency (resolve +
   `nameResolvesTo (publicGroup.groupLink)`) (+ core tests A4 3, 4).
3. Core: owner set-name marks verified (A3) (+ core test A4 6).
4. Directory: every-cycle verification in `deGroupLinkCheck` + approval-time
   verification in `sendToApprove` (B2).
5. Directory: bot output (B3) + test 1 bot assertions.
6. Directory: search (B4) + tests 2, 3.
7. Directory: listing JSON (B5) + `checkListings` extension, test 4.
8. Website (B6) — manual verification; no JS test harness exists.

## 6. Open points / follow-ups

- Client UX for manual verification (verify button / client-side auto-verify policy per
  `plans/2026-06-27-namespace-ui-display-set.md`) is separate UI work; part A only makes
  the API do the right check. The UI in this tree already renders the 3-state indicator
  and auto-verifies on open when the status is NULL (`SimplexNameView.kt:62-63`,
  toggle default ON) — so after A1, link-joined members get a real verification on
  opening channel info instead of today's spurious "(verified)".
- `APIVerifyContactDomain` and contact names (`@name`) are out of scope — the directory
  lists only groups, and contact claims are not bound to a joined link the same way.
- Observed but NOT in scope (no change requested; decide separately if ever relevant):
  (a) `resolveKnownGroup` with a name target would apply the registry-resolved link's
  data to the locally-found group without checking the resolved link is that group's —
  the path is currently unexercised (defect table row 4); (b) link-data-borne profile
  updates lack the `publicGroupId` mismatch rejection that `xGrpInfo` applies
  (`Subscriber.hs:3684-3685`).

Resolved decisions (were open points):

- Owner's own status after setting the name — fix in scope (A3): mark verified after a
  successful set, since resolution ran as its precondition.
- Invariant 3 ("added → unknown") admits exactly one class of exception — resolution by
  explicit user action, persisted where the group happens to live: at creation for an
  unknown group (`APIPrepareGroup domain=`, `Groups.hs:671-674`), on the existing row
  for a known group (A1's `resolvedDomain_` on the name-plan paths), and at set time
  for the owner (A3). All confirmed.
- Stale verified names: the directory re-verifies every claimed group on every link
  check (decision 5), so a lapsed or re-pointed registry entry drops the name within
  one interval. Clients never re-verify on a schedule — the freshness requirement
  belongs to the publisher, not the member.
- **Link rotation is supported** — no link-immutability invariants anywhere: joining
  does not require the profile link to equal the joining link, `xGrpInfo` does not
  freeze `groupLink`, and verification never compares against the join link. The
  earlier rule "verification fails when the profile link differs from the join link"
  is superseded: verification is name ↔ profile-link consistency only. The only link
  ever compared is the profile-declared one; the join link is never used. Incoming
  profile updates remain subject to the existing `publicGroupId` mismatch rejection
  (`xGrpInfo`), mirrored for link data by A1.
