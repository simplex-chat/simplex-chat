# Name warnings in core (#7525 review, 2026-09-28)

EP's review: the connection plan should say in core which name scenario applies, as a type with the minimal data it needs. The CLI renders it, and each app only maps its constructors to strings. The freshness check should sit where each kind's chat is found, not in one place for both. Bare names (kind unknown) and business chats should be visible in the logic.

The canvas was reviewed against this model on 2026-09-28, story by story (§4).

## Table of contents

1. Executive summary
2. Terms
3. Scenarios and answers
4. Changes against today
5. The type
6. Plan logic, as a story per target
7. CLI
8. Apps
9. Canvas changes
10. What goes away
11. Decisions
12. Tests
13. Order of work
14. Done means

## 1. Executive summary

- **`NameWarning` replaces `NameRegistration` in the plan.** `CPContactAddress` and `CPGroupLink` have `nameChange :: Maybe NameChange`, with the warning in `NCLapsed`, and `CPNameNotConnectable` has `nameWarning :: NameWarning`. An app shows an alert exactly when the plan has a warning. It does not compare dates, lengths or registrations.
- **The warning is the registry's answer.** The record of an unexpired name, or the warning, is computed by `nameRecordOrWarning`, which is tested directly. A local plan is built with that warning in `NCLapsed`, except `NWNotRegistered` (N11). The own name and a chat are read from the plan (`CAPOwnLink`, `GLPOwnLink`, and `NCLapsed` on other plans), not from the warning.
- **A typed name (`@d`, `#d`) is planned for its kind only.**
  - The name is resolved before the local lookup. A chat, own address or own channel is compared with the name's link.
  - The same link: the local plan is confirmed.
  - Another link: the new link's plan is built with `NCMoved` and the local chat (3c, N35), or without a change for the own address (N9). The local plan is returned when the new link's profile does not claim the name, or its data cannot be fetched (N12, N19).
  - Nothing local: the plan for the name's link (2a), or `SDEUnknownDomain` if that link's profile does not claim the name (2g).
- **A bare name (`d`) is resolved once and planned for one kind.**
  - The channel is planned as `#d` when the name has a channel link. The contact is planned as `@d` on an error in the channel's plan, or when the name has only a contact link (N22).
  - For a channel without relays, or one that needs an app update, the channel's plan is returned (N29).
  - The other kind is offered whenever the name has a link of it (N8).
- **The name is resolved on every lookup**, except with `resolve=never` (N28, §9 of the lookup plan).
- **Accepted:**
  - Removing the link of a chat's kind from the name is not reported.
  - Reservations for reasons other than community, and label lengths, are not reported for a chat or the own name.

## 2. Terms

- **name:** `d`, e.g. `bakery.simplex`. It is typed as `@d` (contact name), `#d` (channel name), or bare `d`.
- **kind:** contact or channel. `@d` has the contact kind, `#d` the channel kind. **The other kind** is the channel for `@d` and the contact for `#d`.
- **the name's link of a kind:** the first entry in the registry record's list for that kind (`nrSimplexContact`, `nrSimplexChannel`) that parses as a short link of that kind's type. Entries that do not parse are ignored. **No link of a kind:** no such entry.
- **links of local things:**
  - **a chat's link:** the short link stored when the user connected or joined (`conn_short_link_to_connect`);
  - **the own address's or own channel's link:** its `short_link_contact`;
  - **the same link:** the same type, server host and port, and link key (`sameShortLinkContact`); the server's key hash is not compared (N27);
  - **a new link:** the name's link differs from the local one;
  - **leads to:** "the name leads to X" means the name's link of X's kind is X's link.
- **registry answers:**
  - **registered:** the registry answers `NRRegistered`;
  - **unexpired:** registered, and `expires` is absent or not earlier than now;
  - **expired:** registered, and `expires` is earlier than now, whether or not the grace period (`graceUntil`) has passed;
  - **available:** `NRAvailable`, and the label (the part before the TLD, without subnames) has at least `minLabelLength` characters;
  - **reserved for community:** `NRReserved community`;
  - **not registered:** `NRReserved` for another reason (internal, trademark, unknown), or `NRAvailable` with a shorter label;
  - **request failed:** the resolver request returns an error (network, server or resolver).
- **address:** a SimpleX contact address, i.e. a short link of the contact type, including business addresses.
- **own address:** the user's contact address (not a group link). It counts for `d` when the user's profile claims `d`.
- **channel:** a public group of the channel type, joined through a short link of the channel type.
- **own channel:** a group the user has the group link for, whose profile claims `d`.
- **chat** (for a name, of a kind), not counting the own address or channel:
  - contact kind: a contact that is not deleted, or a business chat the user is still a member of;
  - channel kind: a joined or prepared channel (not a business chat) the user is still a member of.

  In both cases the name `d` is verified, and a short link is stored. A contact connected by a one-time invitation, or with an unverified name, is not a chat for the name.
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
| unexpired, link L at another chat the user has | chat or own channel at another link | that chat, `NCMoved` (N35) | — | 3a |
| unexpired, no link of kind K | nothing | error `SDENoValidLink` | — | 2f |
| unexpired, no link of kind K | chat or own | the local one | — | 3a, 4a, 4a′ |
| expired | nothing | not connectable | `NWExpired` | 2b |
| expired | chat | the chat | `NWExpired` | 3b, 1d |
| expired | own | own | `NWExpired` | 4c |
| available | nothing | not connectable | `NWAvailable` | 2c |
| available | chat | the chat | `NWAvailable` | 3d |
| available | own | own | `NWAvailable` | 4d |
| reserved for community | nothing or chat | not connectable, or the chat | `NWReservedForCommunity` | 2d, 3d |
| reserved for community | own | own | `NWReservedForCommunity` (N13) | 2d's alert |
| not registered | nothing | not connectable | `NWNotRegistered` | 2e |
| not registered | chat or own | the local one | — | 3a, 4a, 4a′ |
| request failed | chat or own | the local one | — | 3a, 4a, 4a′ |
| request failed | nothing | error | — | 2h |
| not asked: `resolve=never` | chat or own | the local one | — | 3a, 4a, 4a′ |
| not asked: `resolve=never` | nothing | error `CENotResolvedLocally` | — | — |

The name is resolved in every row except `resolve=never`.

A typed name has no `otherSimplexName`: `@` and `#` state the user's intent.

**A bare name `d`**

The name is resolved once, and the result is passed to each kind's plan.

| Registry answer | Answer | `otherSimplexName` |
|---|---|---|
| unexpired, with a channel link | the channel's, as the typed name `#d`; the contact's, as `@d`, on an error in the channel's; the channel's error on errors in both | the other kind, when the name has a link of it |
| unexpired, with only a contact link | the contact's, as `@d` | — |
| unexpired, with no link | the local channel, else the local contact, else error `SDENoValidLink` | — |
| a warning | the channel's, as `#d`; the contact's, as `@d`, when the channel's is not connectable | — |
| request failed | the local channel, else the local contact, else the error | — |
| not asked: `resolve=never` | the local channel, else the local contact, else `CENotResolvedLocally` | — |

For a channel without relays, or one that needs an app update, the channel's plan is the answer (N29).

`otherSimplexName` is shown as the second button of 2a, 3c, 4a, 4a′, the "Repeat join request?" sheet, and a chat's, prepared contact's or own channel's alert (a name in a message always has `@` or `#`).

It is omitted from the no-relays, app-update and "You are already joining the group" alerts. With the button, the other kind's name is planned as a typed name, as in the table above.

## 4. Changes against today

"Today" is #7525 as of `b78d76ae6`, with its apps; the canvas tags instead compare with master before #7525, `643a9afad`. Prices are the registry's per-year price for the label's length, times 2 years.

| # | Story | Today | New |
|---|---|---|---|
| 1 | The name is unexpired and also reserved for community, whatever is local | the community alert instead of the plan | the plan, as for any unexpired name |
| 4 | Bare name; a chat; the name also leads to the other kind at a link the user has no chat at (e.g. channel `#bakery`, the name now has only a contact link) | the channel's plan if the name has a channel link, else the contact's; the local chat is shown only as the other kind's button, if at all | the contact's plan; the local channel is shown only when the name has a channel link |
| 7 | A chat; the name leads to a new link whose profile does not claim the name | the "Unconfirmed name" error alert | the chat, no alert |
| 8 | A chat; the request fails | the "SimpleX name error" alert | the chat, no alert |
| 9 | A chat; the name leads to a new link; from a message | 3c with Open new chat, Cancel | 3c with Open new chat, Open existing chat (opens `knownChat`), and no Cancel |
| 10 | A chat; the name is available | "Name no longer registered", "from $X per year" | the same alert, "$Y for 2 years" |
| 11 | Own address or channel; the name leads to another link | "Connect to yourself?", or the own channel | own address: the new link's plan, as with nothing local (N9); own channel: 3c, as for a chat, including 9 |
| 14 | Own; the name is available | "Your name has expired", "from $X per year" | the same alert, "$Y for 2 years" |
| 15 | Bare name; own address; the name also has a channel link the user has no chat at | the channel's join sheet | the channel's join sheet, or its no-relays or app-update alert; "Connect to yourself?" (4a) with Join channel when the channel's plan fails |
| 16 | `#d`, or a bare name whose contact kind fails too; nothing local; the channel's link has no relays or needs an app update, and its profile does not claim the name | the no-relays or app-update alert | "Unconfirmed name" (2g) |
| 17 | Own address or channel; the request fails | the error alert | the own plan, no alert |

Unchanged, and checked in the review:
- typed names never offer the other kind;
- nothing local and no link of kind K: 2f, with no other kind;
- a chat or own, and the name is not registered: no warning;
- own, no link of its kind: no warning.

## 5. The type

```haskell
data NameChange
  = NCLapsed {nameWarning :: NameWarning}
  | NCMoved {knownChat :: AChatInfo}

data NameWarning
  = NWExpired {expiredAt :: UTCTime, graceUntil :: Maybe UTCTime}
  | NWAvailable {price :: NamePrice}
  | NWReservedForCommunity
  | NWNotRegistered

data NamePrice = NamePrice {amount :: USDCents, years :: Int}
```

- `nameChange` on `CPContactAddress` and `CPGroupLink` is how the name changed since the local chat was found by it:
  - `NCLapsed`: the registration lapsed; the plan is the local one;
  - `NCMoved`: the name leads to another link; the plan is that link's, and `knownChat` is the local chat (3c, N35).
- `CPNameNotConnectable` has a `NameWarning`: nothing is local, so nothing changed.
- `expiredAt` is present whenever a name is expired, because a registration without `expires` counts as unexpired. A missing `graceUntil` is the canvas's dateless variant, and a `graceUntil` that has passed is dropped (N30).
- **The chat to open.** The apps compute `localChats` from the plan: its contact or group, then `knownChat` (N32).
- `NamePrice` is the price of the 2-year term (N3): the registry's per-year price for the label's length (or its base price), times `years = 2`.
- The domain is not repeated. It is `planSimplexName`, or `simplexDomain` on `CPNameNotConnectable`.

**Deciding the warning.**

```haskell
nameRecordOrWarning :: SystemSeconds -> SimplexDomain -> NameRegistration -> Either NameWarning NameRecord
```

- **`nameRecordOrWarning`:** the record of an unexpired name, or the warning (the "nothing" rows of §3). The label length check is in it.
- **`localChange`:** the change a local plan is built with: `NCLapsed` with the registry's warning, except `NWNotRegistered` (N11).
- The wording for the own name, a chat and nothing local is chosen by the CLI and the apps from the plan (N2):
  - the own name: `CAPOwnLink` or `GLPOwnLink` with `NCLapsed`;
  - a chat: another plan with `NCLapsed`;
  - nothing local: `CPNameNotConnectable`.

## 6. Plan logic, as a story per target

**The local lookups.** `knownLinkPlans`, one per kind, in the `where` of `connectPlan`'s short link branches, used by the name and link paths:
- **the contact kind:** the own address, else a contact, else a business chat (`getGroupToConnect`, which matches `business_chat IS NOT NULL` for `@` names);
- **the channel kind:** the own channel, else a joined or prepared channel.

The result is the link and the plan for what is found.

**A typed name `@d` or `#d`**

1. Resolve the name, except with `resolve=never`. The result of `resolveNameLink` is the name's link of the kind, or a warning; an error is returned on a request failure, or when the name has no link of the kind (`SDENoValidLink`). With `resolve=never`, the result is the error `CENotResolvedLocally`.
2. Look up the name's kind locally. The local plan is built with `localChange`.
3. With something local, the answer from `knownNamePlan` is:
   - **the same link:** the local plan;
   - **another link L:** the plan for L, built with `NCMoved` for a chat or own channel (N35), without a change for the own address (N9); the local plan on an error in L's plan (N12, N19);
   - **a warning or an error:** the local plan.
4. With nothing local, the answer is the plan for the link, `CPNameNotConnectable d` with the warning, or the error.

In the plan for a link L, the claim of L's profile is checked (`SDEUnknownDomain`), and a chat the user has at L is refreshed and verified.

**A link target:** a local chat is answered, and with `resolve=all`, a joined group is refreshed from its link data.

**A bare name `d`**

1. With `resolve=never`, plan `#d`, then `@d`, from the local lookups, or fail with `CENotResolvedLocally`.
2. Resolve the name once, and pass the result to each kind's plan.
3. An unexpired name with a channel link: plan `#d`. On an error, plan `@d`. On a second error, fail with the channel's error.
4. An unexpired name with only a contact link: plan `@d`.
5. An unexpired name with no link, or a failed request: plan `#d`, then `@d`, from the local lookups, or fail with the channel's error.
6. A warning: plan `#d`. When its plan is `CPNameNotConnectable`, plan `@d`.
7. For an unexpired name, set `otherSimplexName` to the other kind's name when the name has a link of it (`addOther`, N8, N22).

## 7. CLI

`viewNameWarning` prints one line after the plan, replacing today's `registered …` / `available …` / `reserved …` lines:

- `SimpleX name bakery.simplex expired on 2027-06-24, its owner can renew it until 2027-09-22`
- `your SimpleX name alice.simplex expired on 2027-06-24, renew it before 2027-09-22`
- `SimpleX name sunflower.simplex is available: $20 for 2 years`
- `SimpleX name bakery.simplex is no longer registered, available: $20 for 2 years`
- `your SimpleX name alice.simplex is no longer registered, available: $20 for 2 years`
- `SimpleX name privacy.simplex is reserved for community`
- `SimpleX name acme.simplex is not registered`

`viewKnownChat` prints `NCMoved` after the plan:
- `known contact @alice`
- `known group #club`
- `known channel #team`
- `known business #acme`

`CPNameNotConnectable` is printed as `SimpleX name sunflower.simplex: nothing to connect to`, before its warning line. Errors are printed as:
- `SimpleX name boogaloo.simplex has no valid connection link`
- `SimpleX name acme.simplex is not included in the connection link's profile`
- `no matching chat found, name resolution is disabled`

The `your` lines are printed for `CAPOwnLink` and `GLPOwnLink` plans. `is available` is printed for `CPNameNotConnectable`, and `is no longer registered` for a chat or the own name. A missing `graceUntil` drops the second clause of the expiry lines. `otherSimplexNameNote` is unchanged. The prices are the test registry's ($10 per year).

## 8. Apps

- **Alert.** `showNameRegistrationAlert` becomes `showNameWarningAlert`, a plain `case` from `NameWarning` and `own` to title, message and action (Renew, Register, Re-register, Connect to SimpleX team). `own` is the plan's `isOwnLink`. Open existing chat is shown when the plan has a local chat (1d), with OK. For an available name, the title is "Name no longer registered" with Open existing chat, and "Name not registered" without it.
- **Flow.** The alert is shown by `planAndConnect` when the plan has a warning. Otherwise:
  - for a chat, prepared contact or own channel (`CAPKnown`, `CAPContactViaAddress`, `GLPKnown`, `GLPOwnLink`), the chat's, prepared contact's or own channel's alert is shown, with the other kind's button (N14); for a pasted link, it is replaced by the list filter, as before this change;
  - every other plan is handled as before this change, with the other kind's button where §3 lists it.

  Dates, lengths and the choice between own and chat warnings are decided in core.
- **Flow for `NCMoved`.** The plan's alert is shown with Open existing chat (`knownChat`). The name warning alert is shown for `NCLapsed` only.
- **3c.** In the `Ok` alert with Open existing chat, "<d> now leads to a new address." (contact) or "<d> now leads to a new channel." (channel) is shown, with Open new chat (Open new channel). Cancel is replaced by Open existing chat.
- **No-relays and app-update alerts:** OK, and Open existing chat when the plan has `knownChat` (N35).
- **Name search.**
  - Chat list: while the text is a name, a debounced `resolve=never` lookup is made for its kind, or for `@d` and `#d` for a bare name. The list is filtered to the plans' local chats, which are added to or updated in the chat list (N31).
  - New chat sheet: nothing is looked up while typing.
  - In both, the "Connect to …" button is shown while the text is a name. The name is planned with no filter, so an alert is shown for every answer (N17).
  - Chat list: the local chats of the button's plan are added to the list filter (`showLocalChats`), so a chat found or verified on the tap is listed with the typing lookup's chats. Only the button's own plan is added; the filter is left unchanged by the other kind's button.
  - The search is cleared when a chat is opened or a connection is started from the button's flow. It is kept after errors, warnings without a chat, Cancel and OK. The new chat sheet is closed when a chat is opened or a connection is started.
- **Pasted links.** `filterKnownContact` and `filterKnownGroup` are passed, as in master: both in the chat list, only `filterKnownContact` in the new chat sheet. A known contact or group is then shown in the filtered list in place of an alert.
- **Local chats.** Each app computes `localChats` from the plan: its contact or group, then `knownChat`. Both apps add them to or update them in the chat list, and the first is opened by Open existing chat (N32).
- **Types.** Kotlin and Swift get `NameWarning` and `NamePrice` in place of `NameRegistration` and `NamePricing`. The hand-written Swift decoder for `NameRegistration` goes away: `NameWarning` is chat's own type and derives like its neighbours.
- **Strings.** The price strings change from "from %s per year" to "%s for %d years".

## 9. Canvas changes

- **2a, 3c, 4a:** the other kind's button is shown for bare names only.
- **3c:** also applies to the own channel. From a message, it shows Open new chat and Open existing chat, with no Cancel. For the own address, the new link's plan is shown as with nothing local (N9).
- **3e, 3e′:** dropped. When a bare name's plan is a chat, prepared contact or own channel, and the name also leads to the other kind, the other kind's button is shown in that chat's, prepared contact's or own channel's alert (N14, N17).
- **Prices:** "$X for 2 years", computed from the registry's price. The amounts on the canvas are examples.
- **3a:** "Still leads to your chat, or not found, no valid link, another name, its new link fails, or the request failed" is consistent with §3 and N19; another chat of the user's at the new link is shown instead (N35).
- **1a, 3f, band 3, footer:** the chat list is filtered to the device's answers, and "Connect to …" is shown while the text is a name; the name is resolved on a tap, and the tap's local chats are added to the list (N28, N31).
- **The other kind's button:** that kind is planned as a typed name, so 3c is shown for a moved chat of that kind.
- **4a, 4a′:** 4a′ is the own channel from a message or the new chat sheet (N34). For a new link that cannot be joined yet, its no-relays or app-update alert is shown, with Open existing chat (N35).

## 10. What goes away

- `nameRegistration_` on `CPContactAddress`/`CPGroupLink`, `nameRegistration` on `CPNameNotConnectable`, and `setPlanRegistration`.
- The `PRMUnknown` equation.
- The pre-resolve branch of `CTShortContact`, `nameHasLink` and `nameExpired`.
- `viewNameRegistration`, replaced by `viewNameWarning`.
- In both apps: the decisions in `showNameRegistrationAlert`, `NameRegistration.expired`, `reservedForCommunity`, `centsPerYear`, `nameCentsPerYear`, and the Swift `NameRegistration` decoder.

## 11. Decisions

Decided:

| # | Question | Answer |
|---|---|---|
| N1 | Where the warning is | `NCLapsed` in `nameChange :: Maybe NameChange` on `CPContactAddress`/`CPGroupLink`, `nameWarning :: NameWarning` on `CPNameNotConnectable` |
| N2 | Own and chat variants | none in the warning: the wording is chosen from the plan; the own name (`CAPOwnLink`, `GLPOwnLink`) and a chat say "no longer registered" for an available name, nothing local says "available" |
| N3 | Price | the registry's per-year price for the label's length, times 2 years |
| N4 | An unexpired name with `reservedReason_ = community` | no warning |
| N5 | 3c | `NCMoved` in `nameChange`; a lapsed and a moved name exclude each other; `addressChanged` is removed, `CAPOk` and `GLPOk` are unchanged |
| N6 | Answers up to a day old for a chat | superseded by N28: the name is resolved on every lookup |
| N7 | The name no longer has a link of a chat's or own's kind | not reported |
| N8 | The other kind | offered for bare names only, whenever the name has a link of that kind |
| N9 | Own address or channel at another link than the name's | own channel: 3c, as for a chat; own address: the new link's plan without a change, as with nothing local |
| N10 | 3c from a message | Open new chat, and Open existing chat (`knownChat`), no Cancel |
| N11 | Not registered (reserved for another reason, or too short), with a chat or own | not reported |
| N12 | The new link does not claim the name, with a chat or own | the local one, no alert |
| N15 | The request failed | with a chat or own: the local one, no alert; with nothing: the error alert |
| N16 | Where the bare name's lookups are | the two local lookups (`knownLinkPlans`) are in the `where` of `connectPlan`'s short link branches; each kind of a bare name is planned through `connectPlan`, with the resolution already made |
| N17 | Name search (the "Connect to …" button) | no filter is passed: an alert is shown for every answer, with Open chat or Open existing chat for a local chat; in the chat list, the plan's local chats are added to the list filter; the search is cleared when a chat is opened or a connection is started |
| N18 | `/c` with `nameChange` | `NCLapsed`: the plan is shown instead of connecting, as only resolvable names are connected; `NCMoved`: connects when the plan allows, as `/c` is not planning (`connectionPlanProceed`) |
| N19 | A chat, and the name's link data cannot be fetched | the chat, no alert, as N15 |
| N20 | 3c for the own address from a message | superseded by N9: the own address has no 3c |
| N21 | 3c for a channel | "… now leads to a new channel." in the information line; Open new channel, and Open existing chat from a message |
| N22 | Bare name, the channel failed and the contact kind is planned | the channel is offered as the other kind |
| N23 | A chat answered without a warning (N7, N11) | superseded by N28: the name is resolved on every lookup |
| N24 | Expired without a grace date | "expired on <date>", without a renew clause, as the CLI |
| N25 | Dates in the alerts | one long localized format in both apps |
| N26 | Bot clients | `connLink` is optional in Python and Node; `resolve=allGroups` still parses |
| N13 | Own name reserved for community after its registration ended | `NWReservedForCommunity`, 2d's alert |
| N14 | 3e | dropped: the other kind is offered by the second button of the chat's, prepared contact's or own channel's alert |
| N27 | Comparing the name's link with a local one | `sameShortLinkContact`: the server's key hash is not compared |
| N28 | When to resolve a name | on every lookup, except with `resolve=never`; the local chat is compared with the name's link on every lookup |
| N29 | Bare name, the channel has no relays or needs an app update | the channel's plan; the contact kind is offered when the name has a contact link |
| N30 | Expired, and the grace date has passed | the dateless variant (N24) |
| N31 | Name search | a debounced `resolve=never` lookup of the typed kind, or of both kinds for a bare name; the list is filtered to their local chats; the "Connect to …" button is shown while the text is a name |
| N32 | The local chat in the apps | `localChats`, computed from the plan: its contact or group, then `knownChat`; added to the list or updated in it, and opened by Open existing chat. "You are already joining the group" keeps its alert and is not filtered |
| N33 | The other kind's button with a chats filter | superseded by N14: filters are passed for pasted links only, and a link has no other kind |
| N34 | iOS against Kotlin | the name line in alerts, the own channel's "Your channel" text, a contact prepared at the name's address opened as in Kotlin, and the other kind's button on the "Repeat join request?" sheet |
| N35 | A local chat's name leads to a link that cannot be joined yet (no relays, needs an app update, connecting), or to another chat the user has | that link's plan, or that chat's, with `NCMoved` |
| N36 | Chats whose name verification changes while planning or preparing | core emits `CEvtContactUpdated` / `CEvtGroupUpdated` for each of the user's other chats of the name's kind whose verification it clears (`unverifyOtherNameChats`); the verified chat is in the command's response |

## 12. Tests

- **Unit tests:** `nameRecordOrWarning`, one per "nothing" row of §3, plus the dateless, grace-passed, too-short, 2-year price and unexpired-community cases (`testNameRecordOrWarning`).
- **CLI tests**, in "connection plan: the name lookup answers", with the warning lines asserted:
  - §4 stories 4, 7, 8, 11, 15, 16 and 17, and 10 and 14 through the price in the warning lines; story 1 is covered by a unit test;
  - a chat and own with a name reserved for community, and with a name not registered (`testPlanKnownNameReserved`);
  - a business chat (`testConnectByNameBusinessAndChannel`, `testPlanNameOtherKindBusiness`);
  - every-lookup resolution, with and without a chat (`testPlanKnownNameStale`, `testPlanNameResolvedEveryCall`);
  - a moved name: 3c, another known chat, and a channel without relays (`testPlanKnownNameAddressChanged`, `testPlanKnownNameMovedToKnownChat`, `testPlanChannelNameMovedNoRelays`);
  - a new link whose data cannot be fetched (`testPlanKnownNameLinkFailed`);
  - a bare name whose channel has no relays (`testPlanNameChannelNoRelays`).

## 13. Order of work

1. Core:
   - `NameWarning`, `NamePrice` and `nameRecordOrWarning`, with its unit tests;
   - `knownNamePlan` after the local lookups;
   - the typed name and bare name paths;
   - the View;
   - the CLI tests.
2. Regenerate the bot API types.
3. Kotlin, then Swift: the `case`, 3c from a message, the strings.
4. Update the lookup plan (`plans/2026-09-22-name-lookup-core-api.md`) §2–§4, and the canvas (§9).

## 14. Done means

- No app reads a `NameRegistration`, compares dates or lengths, or decides whether to alert.
- The typed name and bare name paths read as §6. The local chat is compared with the name's link only in `knownNamePlan`.
- Every row of §3, and every story of §4 except 9 (apps only), has a unit test or a CLI assertion, with these exceptions:
  - a chat or own at a new link that needs an app update or is still connecting, covered by the tested no-relays path through `knownNamePlan`;
  - the app-update half of N29, covered by the tested no-relays branch;
  - the own channel at another link, covered by the joined channel's tested path;
  - a bare name with no link, covered by the tested failed-request path.
