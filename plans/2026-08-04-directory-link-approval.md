# Directory: group link creation at approval

Date: 2026-08-04

## Goal

- The directory creates the group join link at first approval.
- The directory issues every link data update; the automatic refresh in core is disabled by config.
- The welcome message link requirement is replaced by a post-approval recommendation.
- A link sent to the directory is resolved to its registered group.

Existing functions are amended; the diff is kept minimal, in code and in tests.

## 1. Core (simplex-chat library)

1.1. `ChatConfig`: add `updateGroupLinksFromApp :: Bool`, default `False`. The directory service sets `True` in `directoryService` and `directoryServiceCLI`.

1.2. `xGrpInfo` (Subscriber.hs ~3750): condition before the fork:

```haskell
ChatConfig {updateGroupLinksFromApp} <- asks config
unless (useRelays' g'' || updateGroupLinksFromApp) $
  void $ forkIO $ void $ setGroupLinkData' NRMBackground user g''
```

`setGroupLinkData'` stays unchanged. The call in `runUpdateGroupProfile` (Commands.hs ~4043) stays unconditional.

1.3. Link data sync from the directory: a Service.hs helper reads the link with `getGroupLink` and runs `setGroupLinkData NRMBackground user gInfo gLink` (Internal.hs ~1461, exported) via `runReaderT (runExceptT …) cc`; the `GroupInfo` argument supplies the profile.

## 2. Registration flow (Service.hs)

2.1. `deServiceJoinedGroup`: after `setGroupRegOwner` — set `GRSPendingApproval 1`, notify the owner ("Joined the group X. Registration is pending approval — it may take up to 48 hours."), send `recommendedSettingsNotice`, call `verifyAndSendToApprove`. The `APICreateGroupLink` call and the `GRSPendingUpdate` transition are removed. This mirrors the channel flow in `deMemberUpdated`.

2.2. `DCApproveGroup`, after the duplicate and roles checks, before `setGroupStatusPromo`:

- link record present (legacy registration or re-approval): the §1.3 sync with the `GroupInfo` from `getGroupAndReg`;
- link record absent: `APICreateGroupLink groupId GRMember`; on failure reply with the error and keep the status.

Owner notification: approved, the link, "We recommend adding this link to the group welcome message."

## 3. Profile update handling (`deGroupUpdated`, non-public groups)

3.1. `GroupProfileUpdate` and `groupProfileUpdate` are replaced by one check — link-only change: fields other than description equal, descriptions equal after removal of the service link and the recommended phrase "Link to join the group <name>:", with `T.words` normalization. The link is read with `APIGetGroupLink`; on `SEGroupLinkNotFound` the comparison runs without link removal; on other failures — log, no action (as today). The description-contains-link check (`profileGroupLinkText`, Service.hs ~641) moves to a helper shared with §6.

3.2. Transitions. `n'` — n+1 when the status is `GRSPendingApproval n`, 1 otherwise. "Send to approve" — `checkRolesSendToApprove` as today.

| status | change | status' | actions |
|---|---|---|---|
| GRSActive | link-only | GRSActive | notify owner; §1.3 sync with the event `toGroup` |
| GRSPendingApproval n | link-only | unchanged | — (the sent approval code stays valid) |
| GRSSuspended, GRSSuspendedBadRoles | link-only | unchanged | — |
| GRSPendingUpdate (legacy data only) | any | GRSPendingApproval 1 | notify owner; send to approve |
| any of the above | other change | GRSPendingApproval n' | notify owner and admins; send to approve |

The `GRSPendingUpdate` branch of the `deGroupUpdated` dispatch (~533) is removed; the status is routed through `processProfileChange`. Link removal while active keeps the group listed; `GRSPendingUpdate` is unreachable for new registrations. Channel handling (`publicGroupProfileChange`) stays unchanged.

## 4. Command replies (Service.hs)

- `DCMemberRole`, group without a link: "The group link is created when the group is approved."
- `DCShowUpgradeGroupLink`: the `SEGroupLinkNotFound` reply mentions approval; the `APIAddGroupShortLink` upgrade branch requires `GRSActive`.
- `DCResumeGroup`, `DCSuspendGroup`, `DCApproveGroup` fallback replies include `groupRegStatusText`.
- `DCHelp DHSRegistration`: the welcome message step is replaced by approval; link inclusion is described as a post-approval recommendation.

## 5. Search by link

5.1. Detection: in `DCSearchGroup`, when the formatted text holds a `SimplexLink` of type `XLGroup` or `XLChannel`, the first such `simplexUri`, wrapped with `aConnectTarget`, is the lookup target.

5.2. Lookup: `APIConnectPlan userId (Just target) PRMNever Nothing`:

- `CPGroupLink (GLPOwnLink g)`, `CPGroupLink (GLPKnown {groupInfo})` → `getGroupReg` by group id;
- other plans, `CENotResolvedLocally` → unknown link.

5.3. Replies:

- user, `GRSActive` → the found-group message (single entry, existing format);
- user, other status or unknown link → the current not-found reply;
- admin, registered → group info with `groupRegStatusText` and owner, as in `sendGroupsInfo` admin format;
- admin, unknown link → "This link is not registered in the directory."

5.4. Card path: `deChatLinkReceived` branches without a valid owner signature run the §5.2 lookup on `connLink`; `GLPKnown` → reply per §5.3; otherwise the current replies.

## 6. Link in listings and search results

6.1. Bot search results: `sendFoundGroups` appends the join-link line for non-public groups when the description omits the link; result rows are extended with the group link via `getGroupLink`.

6.2. `groupDirectoryEntry` (Listing.hs): the join-link line, currently appended for public groups, is appended for non-public groups too when the description omits the link.

## 7. Legacy registrations

Registrations created before deployment keep their links. Their updates follow §3.2. Their approval syncs the link data (§2.2, first branch).

## 8. Tests (DirectoryTests.hs)

- Amend `submitGroup`, `groupAccepted`, `completeRegistrationId`, `updateProfileWithLink`, `notifySuperUser`, `approveRegistrationId` to the new sequence — submit → pending approval → approve → link in the approval notification — changing only the affected expected lines.
- New cases: link-only description change keeps the listing and syncs link data; content change requires re-approval; link-only change while pending keeps the approval code valid; profile change while suspended; legacy waiting-for-link registration moves to approval on profile change; `/resume` reply with status; `/role` and `/link` replies before approval; search by link as user and as admin; card from a non-owner for a listed group.

## 9. Docs

- `apps/simplex-directory-service/README.md`: registration steps and the state machine section.
- Bot `/help` text is covered by §4.
