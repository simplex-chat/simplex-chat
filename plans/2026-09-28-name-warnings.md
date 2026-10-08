# Name warnings in connection plans

## Table of contents

1. Summary
2. Terms
3. Scenarios and answers
4. Types
5. Plan logic
6. CLI
7. Apps
8. Name verification
9. Decisions
10. Tests

## 1. Summary

- A name's registration is classified in core by `nameRecordOrWarning`: the record of an unexpired name, or a `NameWarning`.
- With something local, the warning is in the local plan: `nameChange = Just (NCLapsed w)` on `CPContactAddress` or `CPGroupLink`, except `NWNotRegistered` (N11).
- With nothing local, the warning is the error `CESimplexDomainNotReady d (SDENameWarning w)` (N1).
- The apps show an alert exactly for a warning. They do not compare dates, lengths or registrations.
- A typed name (`@d`, `#d`) is planned for its kind only. A bare name (`d`) is resolved once and planned for one kind.
- The name is resolved on every lookup, except with `resolve=never` (N28).
- A local chat whose name leads to another link is answered with that link's plan and `NCMoved` (3c, N35).

## 2. Terms

- **name:** `d`, e.g. `bakery.simplex`. It is typed as `@d` (contact name), `#d` (channel name), or bare `d`.
- **kind:** contact or channel. `@d` has the contact kind, `#d` the channel kind. **The other kind** is the channel for `@d` and the contact for `#d`.
- **the name's link of a kind:** the first entry in the registry record's list for that kind (`nrSimplexContact`, `nrSimplexChannel`) that parses as a short link of that kind's type. Entries that do not parse are ignored.
- **links of local things:**
  - **a chat's link:** the short link stored when the user connected or joined (`conn_short_link_to_connect`);
  - **the own address's or own channel's link:** its `short_link_contact`;
  - **the same link:** the same type, server host and port, and link key (`sameShortLinkContact`); the server's key hash is not compared (N27);
  - **a new link:** the name's link differs from the local one;
  - **leads to:** "the name leads to X" means the name's link of X's kind is X's link.
- **registry answers:**
  - **registered:** `NRRegistered`;
  - **unexpired:** registered, and `expires` is absent or not earlier than now;
  - **expired:** registered, and `expires` is earlier than now;
  - **available:** `NRAvailable`, and the label (the part before the TLD, without subnames) has at least `minLabelLength` characters;
  - **reserved for community:** `NRReserved community`;
  - **not registered:** `NRReserved` for another reason, or `NRAvailable` with a shorter label;
  - **request failed:** the resolver request returns an error.
- **address:** a short link of the contact type, including business addresses.
- **own address:** the user's contact address. It counts for `d` when the user's profile claims `d`.
- **channel:** a public group of the channel type.
- **own channel:** a group the user has the group link for, whose profile claims `d`.
- **chat** (for a name, of a kind), not counting the own address or channel:
  - contact kind: a contact that is not deleted, or a business chat the user is still a member of;
  - channel kind: a joined or prepared channel the user is still a member of.

  In both cases the name `d` is verified, and a short link is stored.
- **local:** a chat, the own address or the own channel, of the kind, for the name. **Nothing local:** none of them.
- **from search, from a message:** the canvas entry points 1a and 1b.

## 3. Scenarios and answers

From `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`.

**A typed name `@d` or `#d`, of kind K**

| Name | Local of kind K | Answer | Warning | Screen |
|---|---|---|---|---|
| unexpired, link L | nothing | L's plan | — | 2a |
| unexpired, link L at a contact or channel the user has, not verified for `d` | nothing | that chat, verified | — | 3a |
| unexpired, link L | chat or own at L | the local one | — | 3a, 4a, 4a′ |
| unexpired, link L | chat at another link | L's plan, `NCMoved` | — | 3c |
| unexpired, link L | own address at another link | L's plan | — | 2a |
| unexpired, link L | own channel at another link | L's plan, `NCMoved` | — | 3c |
| unexpired, link L whose profile does not claim `d` | nothing | error `SDEUnknownDomain` | — | 2g, 4b |
| unexpired, link L whose profile does not claim `d` | chat or own | the local one | — | 3a, 4a, 4a′ |
| unexpired, link L whose data cannot be fetched | nothing | error | — | 2h |
| unexpired, link L whose data cannot be fetched | chat or own at another link | the local one (N19) | — | 3a, 4a, 4a′ |
| unexpired, link L that cannot be joined yet (no relays, needs an app update, connecting) | chat or own channel at another link | L's plan, `NCMoved` (N35) | — | L's alert |
| unexpired, link L at another chat the user has | chat or own channel at another link | that chat, verified, `NCMoved` with the local chat moved (N35, N37) | — | 3a |
| unexpired, no link of kind K | nothing | error `SDENoValidLink` | — | 2f |
| unexpired, no link of kind K | chat or own | the local one | — | 3a, 4a, 4a′ |
| expired | nothing | error `SDENameWarning` | `NWExpired` | 2b |
| expired | chat | the chat | `NWExpired` | 3b, 1d |
| expired | own | own | `NWExpired` | 4c |
| available | nothing | error `SDENameWarning` | `NWAvailable` | 2c |
| available | chat | the chat | `NWAvailable` | 3d |
| available | own | own | `NWAvailable` | 4d |
| reserved for community | nothing | error `SDENameWarning` | `NWReservedForCommunity` | 2d |
| reserved for community | chat | the chat | `NWReservedForCommunity` | 3d |
| reserved for community | own | own | `NWReservedForCommunity` (N13) | 2d's alert |
| not registered | nothing | error `SDENameWarning` | `NWNotRegistered` | 2e |
| not registered | chat or own | the local one | — | 3a, 4a, 4a′ |
| request failed | chat or own | the local one | — | 3a, 4a, 4a′ |
| request failed | nothing | error | — | 2h |
| not asked: `resolve=never` | chat or own | the local one | — | 3a, 4a, 4a′ |
| not asked: `resolve=never` | nothing | error `CENotResolvedLocally` | — | — |

`otherSimplexName` is set for bare names only.

**A bare name `d`**

The name is resolved once, and the record or the error is passed to each kind's plan.

| Registry answer | Answer | `otherSimplexName` |
|---|---|---|
| unexpired, with a channel link | the channel's, as `#d`; the contact's, as `@d`, on an error in the channel's; the channel's error on errors in both | the other kind, when the name has a link of it |
| unexpired, with only a contact link | the contact's, as `@d` | — |
| unexpired, with no link | the local channel, else the local contact, else error `SDENoValidLink` | — |
| a warning | the local channel, else the local contact, else error `SDENameWarning` | — |
| request failed | the local channel, else the local contact, else the error | — |
| not asked: `resolve=never` | the local channel, else the local contact, else `CENotResolvedLocally` | — |

For a channel without relays, or one that needs an app update, the channel's plan is the answer (N29).

`otherSimplexName` is shown as the second button of 2a, 3c, 4a, 4a′, the "Repeat join request?" sheet, and a chat's, prepared contact's or own channel's alert. It is omitted from the no-relays, app-update and "You are already joining the group" alerts. With the button, the other kind's name is planned as a typed name.

## 4. Types

```haskell
data ConnectionPlan
  = CPInvitationLink {invitationLinkPlan :: InvitationLinkPlan}
  | CPContactAddress {contactAddressPlan :: ContactAddressPlan, nameChange :: Maybe NameChange}
  | CPGroupLink {groupLinkPlan :: GroupLinkPlan, nameChange :: Maybe NameChange}
  | CPError {chatError :: ChatError}

data NameChange
  = NCLapsed {nameWarning :: NameWarning}
  | NCMoved {knownChat :: AChatInfo}

data NameWarning
  = NWExpired {expiredAt :: UTCTime, graceUntil :: Maybe UTCTime}
  | NWAvailable {price :: NamePrice}
  | NWReservedForCommunity
  | NWNotRegistered

data NamePrice = NamePrice {amount :: USDCents, years :: Int}

data SimplexDomainError
  = SDENoValidLink
  | SDEUnknownDomain
  | SDENameWarning {nameWarning :: NameWarning}

data DomainVerification
  = DVFailed
  | DVVerified
  | DVMoved
```

- `nameChange` is how the name changed since the local chat was found by it:
  - `NCLapsed`: the registration lapsed; the plan is the local one;
  - `NCMoved`: the name leads to another link; the plan is that link's, and `knownChat` is the local chat (3c, N35, N37).
- `contactDomainVerified` and `groupDomainVerified` are `Maybe DomainVerification`, stored as 0 (failed), 1 (verified) and 2 (moved) (N38).
- `expiredAt` is present whenever a name is expired. A missing `graceUntil` is the dateless variant; a passed `graceUntil` is dropped (N30).
- `NamePrice` is the price of the 2-year term (N3).
- The domain of a lapsed name is `planSimplexName`. The error has it as `simplexDomain`.

```haskell
nameRecordOrWarning :: SystemSeconds -> SimplexDomain -> NameRegistration -> Either NameWarning NameRecord
resolveConnectableName :: User -> NetworkRequestMode -> SimplexDomain -> CM NameRecord
```

- `nameRecordOrWarning` classifies the registration. The label length check is in it.
- `resolveConnectableName` resolves a name for a plan, and throws `CESimplexDomainNotReady d (SDENameWarning w)` for a warning. Other callers use `resolveNameRecord`.

## 5. Plan logic

**The local lookups.** `knownLinkPlans`, one per kind, in the `where` of `connectPlan`'s short link branches:
- **the contact kind:** the own address, else a contact, else a business chat;
- **the channel kind:** the own channel, else a joined or prepared channel.

The result is the link and the local plan without its change. The known chat is the plan's chat (none for the own address). Only verified chats are found by name.

**A typed name `@d` or `#d`**

1. The name is resolved, except with `resolve=never`. The result is the name's link of the kind, or an error: a warning (`SDENameWarning`), no link of the kind (`SDENoValidLink`), a failed request, or `CENotResolvedLocally`.
2. The name's kind is looked up locally.
3. With something local, the answer from `knownNamePlan` is:
   - **the same link:** the local plan;
   - **another link L:** the plan for L, with `NCMoved` for a chat or own channel (N35), without a change for the own address (N9); the local plan on an error in L's plan (N12, N19); L's plan without a change when it is the local chat (N39);
   - **a warning:** the local plan with `NCLapsed w`, except `NWNotRegistered` (N11);
   - **another error:** the local plan.
4. With nothing local, the answer is the plan for the link, or the error.

In the plan for a link L, the claim of L's profile is checked (`SDEUnknownDomain`), and a chat the user has at L is refreshed and verified. `knownChat` is then marked moved (N37).

**A link target:** the local plan. With `resolve=allGroups`, a joined group found by the link is refreshed from its link data.

**A bare name `d`**

1. With `resolve=never`: `#d`, then `@d`, from the local lookups, else `CENotResolvedLocally`.
2. The name is resolved once by `resolveConnectableName`, and the record or the error is passed to each kind's plan.
3. An unexpired name with a channel link: `#d`; on an error, `@d`; on a second error, the channel's error.
4. An unexpired name with only a contact link: `@d`.
5. An unexpired name with no link, a warning, or a failed request: `#d`, then `@d`, from the local lookups, else the channel's error.
6. For an unexpired name, `otherSimplexName` is set to the other kind's name when the name has a link of it (`addOther`, N8, N22).

`connectionPlanProceed` is false for a plan with `NCLapsed` (N18).

## 6. CLI

`viewConnectionPlan` takes `planSimplexName`. A `CPContactAddress` or `CPGroupLink` plan is followed by its `viewNameChange` line:

- `NCLapsed`, by `viewNameWarning`:
  - `SimpleX name bakery.simplex expired on 2027-06-24, its owner can renew it until 2027-09-22`
  - `your SimpleX name alice.simplex expired on 2027-06-24, renew it before 2027-09-22`
  - `SimpleX name bakery.simplex is no longer registered, available: $20 for 2 years`
  - `your SimpleX name alice.simplex is no longer registered, available: $20 for 2 years`
  - `SimpleX name privacy.simplex is reserved for community`
- `NCMoved`:
  - `known contact @alice`
  - `known group #club`
  - `known channel #team`
  - `known business #acme`

The `your` lines are printed for `CAPOwnLink` and `GLPOwnLink` plans. A missing `graceUntil` drops the second clause of the expiry lines.

A chat's name line shows its `DomainVerification`:
- `SimpleX name: @alice.simplex (verified)`
- `SimpleX name: @alice.simplex (verification failed)`
- `SimpleX name: @alice.simplex (moved)`
- `SimpleX name: @alice.simplex (unverified)`, for no status with a proof

Errors are printed by `viewChatError`:
- `SimpleX name sunflower.simplex is available: $20 for 2 years` (`viewNameNotConnectable`)
- `SimpleX name bakery.simplex expired on 2027-06-24, its owner can renew it until 2027-09-22`
- `SimpleX name privacy.simplex is reserved for community`
- `SimpleX name acme.simplex is not registered`
- `SimpleX name boogaloo.simplex has no valid connection link`
- `SimpleX name acme.simplex is not included in the connection link's profile`
- `no matching chat found, name resolution is disabled`

## 7. Apps

- **Alert.** `showNameWarningAlert` maps `NameWarning` and `own` to title, message and action:
  - actions: Renew, Register, Re-register, Connect to SimpleX team;
  - Open chat when the plan has a local chat (1d), with OK;
  - an available name: "Name no longer registered" with Open chat, "Name not registered" without it.
- **Where the alert is shown:**
  - `NCLapsed`: by `planAndConnect`, with `own` from the plan's `isOwnLink`;
  - `SDENameWarning`: by the connect error handler (`apiConnectResponseAlert`), with `own = false` and no Open chat.
- **Other plans.** A chat, prepared contact or own channel (`CAPKnown`, `CAPContactViaAddress`, `GLPKnown`, `GLPOwnLink`) shows its alert with the other kind's button (N14). For a pasted link, the list filter is shown instead of the alert. Every other plan shows its alert, with the other kind's button where §3 lists it.
- **`NCMoved` (3c, N10).** The plan's alert is shown, with Cancel. On a tap from the chat list search, `knownChat` is added to the list filter; Cancel keeps the filter.
- **Local chats (N32).**
  - `planChat`: the plan's contact or group;
  - `knownChat`: the chat of `NCMoved`;
  - `localChats`: both.

  `localChats` are added to or updated in the chat list, and passed to `showLocalChats`. Open chat opens `planChat`.
- **Name status.** A moved name is shown with a cross in the secondary colour. A tap re-verifies it, as for a failed name.
- **Name search (N17, N31).**
  - Chat list: while the text is a name, a debounced `resolve=never` lookup is made for its kind, or for `@d` and `#d` for a bare name. The list is filtered to the plans' local chats, which are added to or updated in the chat list.
  - New chat sheet: nothing is looked up while typing.
  - In both, the "Connect to …" button is shown while the text is a name. The name is planned with no filter.
  - Chat list: the local chats of the button's plan are added to the list filter. The other kind's button leaves the filter unchanged.
  - The search is cleared when a chat is opened or a connection is started from the button's flow. The new chat sheet is closed then.
- **Pasted links.** `filterKnownContact` and `filterKnownGroup` are passed.
- **Strings.** Prices: "%s for %d years". Dates: one long localized format (N25).

## 8. Name verification

One chat per name and kind is verified (N36).

- `setOtherNameChatsMoved user chatInfo` is called after every write that sets a chat verified. The name is the chat's claim (`chatSimplexName`). The calls are in:
  - `APIPrepareContact`, for a contact or a business chat;
  - `APIPrepareGroup`;
  - `APIVerifyContactDomain`;
  - `APIVerifyGroupDomain`;
  - `APISetPublicGroupAccess`;
  - `APIChangePreparedContactUser` and `APIChangePreparedGroupUser`, for the new user, when the moved chat is verified;
  - `updateContactFromLinkData` and `updateGroupFromLinkData`, when the chat is verified from the link data;
  - the contact kind's plan, for a business chat at the name's link (`setPreparedGroupDomain`).
- The user's other verified chats of the name's kind are set moved (2):
  - `setNameContactsMoved`: contacts, for a contact name;
  - `setNameGroupsMoved`: business chats for a contact name, channels for a channel name.
- The ids of the chats set moved are returned by the two queries. `CEvtNameMoved {user, contactIds, groupIds}` is emitted when they are not empty. The apps set these chats moved, changing no other field.
- A chat is set moved only when another chat is verified for the name, not when a plan finds the name moved (N38).

## 9. Decisions

| # | Question | Answer |
|---|---|---|
| N1 | Where the warning is | with something local: `NCLapsed` in `nameChange` on `CPContactAddress`/`CPGroupLink`; with nothing local: error `CESimplexDomainNotReady d (SDENameWarning w)` |
| N2 | Own and chat variants | the wording is chosen from the plan: the own name (`CAPOwnLink`, `GLPOwnLink`) and a chat say "no longer registered" for an available name; the error says "available" |
| N3 | Price | the registry's per-year price for the label's length, times 2 years |
| N4 | An unexpired name with `reservedReason_ = community` | no warning |
| N5 | 3c | `NCMoved` in `nameChange`; a lapsed and a moved name exclude each other |
| N7 | The name no longer has a link of a chat's or own's kind | not reported |
| N8 | The other kind | offered for bare names only, whenever the name has a link of that kind |
| N9 | Own address or channel at another link than the name's | own channel: 3c, as for a chat; own address: the new link's plan without a change |
| N10 | 3c | the new link's alert, with Cancel and no move text; from the chat list search, `knownChat` is in the list filter, and Cancel keeps it |
| N11 | Not registered, with a chat or own | not reported |
| N12 | The new link does not claim the name, with a chat or own | the local one, no alert |
| N13 | Own name reserved for community after its registration ended | `NWReservedForCommunity`, 2d's alert |
| N14 | 3e | dropped: the other kind is offered by the second button of the chat's, prepared contact's or own channel's alert |
| N15 | The request failed | with a chat or own: the local one, no alert; with nothing: the error alert |
| N16 | Where the bare name's lookups are | the two local lookups (`knownLinkPlans`) are in the `where` of `connectPlan`'s short link branches; each kind of a bare name is planned through `connectPlan`, with the resolution already made |
| N17 | Name search (the "Connect to …" button) | no filter is passed: an alert is shown for every answer, with Open chat for a local chat; in the chat list, the plan's local chats are added to the list filter; the search is cleared when a chat is opened or a connection is started |
| N18 | `/c` with `nameChange` | `NCLapsed`: the plan is shown instead of connecting; `NCMoved`: connects when the plan allows |
| N19 | A chat, and the name's link data cannot be fetched | the chat, no alert |
| N22 | Bare name, the channel failed and the contact kind is planned | the channel is offered as the other kind |
| N24 | Expired without a grace date | "expired on <date>", without a renew clause |
| N25 | Dates in the alerts | one long localized format in both apps |
| N26 | Bot clients | `SimplexDomainError` has `nameWarning`; `contactDomainVerified` and `groupDomainVerified` change from a boolean to `DomainVerification` |
| N27 | Comparing the name's link with a local one | `sameShortLinkContact`: the server's key hash is not compared |
| N28 | When to resolve a name | on every lookup, except with `resolve=never` |
| N29 | Bare name, the channel has no relays or needs an app update | the channel's plan; the contact kind is offered when the name has a contact link |
| N30 | Expired, and the grace date has passed | the dateless variant (N24) |
| N31 | Name search | a debounced `resolve=never` lookup of the typed kind, or of both kinds for a bare name; the list is filtered to their local chats; the "Connect to …" button is shown while the text is a name |
| N32 | The local chat in the apps | `localChats` are added or updated; Open chat opens `planChat`; "You are already joining the group" keeps its alert and is not filtered |
| N34 | iOS against Kotlin | the name line in alerts, the own channel's "Your channel" title, a contact prepared at the name's address opened as in Kotlin, and the other kind's button on the connect and "Repeat join request?" sheets |
| N35 | A local chat's name leads to a link that cannot be joined yet, or to another chat the user has | that link's plan, or that chat's, with `NCMoved` |
| N36 | Chats verified for the same name and kind | one per user: `setOtherNameChatsMoved` sets the others moved and emits `CEvtNameMoved` with their ids |
| N37 | `knownChat` when the new link's chat is verified while planning | marked moved, as in the database |
| N38 | The moved status | set only when another chat is verified for the name; a moved chat is not found by name search |
| N39 | The name's link differs from the local chat's link, and L's plan is that chat | L's plan with the local link and without a change: the chat is found by L's connection request, and a changed link does not require joining again |

## 10. Tests

- **Unit tests:** `nameRecordOrWarning`, one per warning, plus the dateless, grace-passed, too-short, 2-year price and unexpired-community cases (`testNameRecordOrWarning`).
- **CLI tests**, in "connection plan: the name lookup answers":
  - warnings with nothing local, as errors (`testConnectByNameNotFound`, `testPlanNameReservedOther`, `testPlanNameResolvedEveryCall`);
  - a chat and own with a lapsed, reserved or unregistered name (`testPlanKnownNameAvailable`, `testPlanKnownNameReserved`, `testPlanOwnNameExpired`, `testPlanOwnNameAvailable`);
  - a business chat (`testConnectByNameBusinessAndChannel`, `testPlanNameOtherKindBusiness`);
  - every-lookup resolution (`testPlanKnownNameStale`, `testPlanNameResolvedEveryCall`);
  - a moved name: 3c, another known chat, a known business chat, a new channel, a channel without relays (`testPlanKnownNameAddressChanged`, `testPlanKnownNameMovedToKnownChat`, `testPlanKnownNameMovedToBusinessChat`, `testPlanChannelNameMoved`, `testPlanChannelNameMovedNoRelays`);
  - a new link whose data cannot be fetched (`testPlanKnownNameLinkFailed`);
  - a bare name whose channel has no relays (`testPlanNameChannelNoRelays`);
  - a link differing only in its key hash, or only in its server address (`testPlanNameLinkKeyHash`, `testPlanNameLinkServer`);
  - a verified prepared contact moved to another user (`testPrepareNameChangeUser`).
