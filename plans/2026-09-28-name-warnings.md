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

- **`NameWarning` replaces `NameRegistration` in the plan.** `CPContactAddress` and `CPGroupLink` have `nameWarning_ :: Maybe NameWarning`, and `CPNameNotConnectable` has `nameWarning :: NameWarning`. An app shows an alert exactly when the plan has a warning. It does not compare dates, lengths or registrations.
- **Two pure functions decide the warning.** One computes a registration's link, or the warning when nothing is local. The other maps that warning to the one for the user's own name or for a chat. Both are tested directly.
- **A typed name (`@d`, `#d`) is planned for its kind only.**
  - A chat at the name's live link is confirmed.
  - A chat, own address or own channel at another link gives the new link's plan with `addressChanged` (3c), unless the new link does not claim the name, cannot be joined yet, or, for a chat, cannot be fetched (N12, N19, N35); another chat the user has at the new link is answered as that chat (N35).
  - Nothing local gives the plan for the name's live link (2a), or `SDEUnknownDomain` if that link's profile does not claim the name (2g).
- **A bare name (`d`) is looked up for both kinds and planned for one.**
  - It plans the channel as `#d`, and the contact as `@d` when the channel has nothing to connect to, fails or cannot be joined (N29).
  - The name is resolved once, through the name cache (N28).
  - It offers the other kind when the name has a live link of it at which nothing is local (N22, N29).
- **Resolved registrations are cached per name** (§9 of the lookup plan). A registration is reused for a day when a chat of the planned kind is local, for 5 minutes when nothing is local, and never for the own address or channel.
- **Accepted:**
  - An answer for a chat can be up to a day old.
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
  - **live:** registered, and `expires` is absent or not earlier than now;
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
- **cached:** the name's registration is in the name cache within its maximum age: a day with a chat of the kind, 5 minutes with nothing local. The own address and channel always resolve, so an owner sees a change on every lookup.
- **from search, from a message:** the canvas entry points 1a and 1b.

## 3. Scenarios and answers

From `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`.

**A typed name `@d` or `#d`, of kind K**

| Name | Local of kind K | Answer | Warning | Screen |
|---|---|---|---|---|
| live, link L | nothing | L's plan | — | 2a |
| live, link L at a contact or channel the user has, not verified for `d` | nothing | that chat, verified and stored as resolved | — | 3a |
| live, link L | chat at L | the chat, confirmed | — | 3a |
| live, link L | chat at another link | L's plan, `addressChanged` | — | 3c |
| live, link L | own at L | own | — | 4a, 4a′ |
| live, link L | own at another link | L's plan, `addressChanged` | — | 3c |
| live, link L whose profile does not claim `d` | nothing | error `SDEUnknownDomain` | — | 2g, 4b |
| live, link L whose profile does not claim `d` | chat or own | the local one | — | 3a, 4a, 4a′ |
| live, link L whose data cannot be fetched | chat at another link | the chat (N19) | — | 3a |
| live, link L whose data cannot be fetched | own at another link | error | — | 2h |
| live, link L whose data cannot be fetched | nothing | error | — | 2h |
| live, link L that cannot be joined yet (no relays, needs an app update, connecting) | chat or own at another link | the local one (N35) | — | 3a, 4a, 4a′ |
| live, link L at another chat the user has | chat or own at another link | that chat (N35) | — | 3a |
| live, no link of kind K | nothing | not connectable | `NWNoValidLink` | 2f |
| live, no link of kind K | chat or own | the local one | — | 3a, 4a, 4a′ |
| expired | nothing | not connectable | `NWExpired` | 2b |
| expired | chat | the chat | `NWExpired` | 3b, 1d |
| expired | own | own | `NWOwnExpired` | 4c |
| available | nothing | not connectable | `NWAvailable` | 2c |
| available | chat | the chat | `NWNoLongerRegistered` | 3d |
| available | own | own | `NWOwnAvailable` | 4d |
| reserved for community | nothing or chat | not connectable, or the chat | `NWReservedForCommunity` | 2d, 3d |
| reserved for community | own | own | `NWReservedForCommunity` (N13) | 2d's alert |
| not registered | nothing | not connectable | `NWNotRegistered` | 2e |
| not registered | chat or own | the local one | — | 3a, 4a, 4a′ |
| request failed | chat | the chat | — | 3a |
| request failed | own or nothing | error | — | 2h |
| not asked: `resolve=never` | chat or own | the local one | — | 3a, 4a, 4a′ |

The registration in these rows is the cached one when it is within its maximum age.

A typed name has no `otherSimplexName`: `@` and `#` state the user's intent.

**A bare name `d`**

The channel is planned first, as the typed name `#d`.

| Channel's answer | Answer |
|---|---|
| can be joined, including a local channel | the channel's |
| nothing to connect to | the contact's, as the typed name `@d` |
| failed, or cannot be joined (no relays, needs an app update, connecting) | the contact's if it can be joined, otherwise the channel's (N29) |

With `resolve=never` both answers are local lookups. A local contact, business chat or own address is answered only when the channel's answer cannot be joined (§4, stories 4 and 15).

When the name is resolved and has a live link of the kind not planned, `otherSimplexName` is that kind's name, unless the user has a chat, own address or own channel of that kind at that link (N22, N29). The plan's screen shows it:
- the second button of 2a, 3c, 4a, 4a′, the "Repeat join request?" sheet, and, from the new chat sheet, of a chat's, prepared contact's or own channel's alert (a name in a message always has `@` or `#`);
- 3e for a chat, prepared contact or own channel, from search (new).

On "You are already joining the group" (a channel being joined through a pasted link) the other kind is not offered. When the other kind's chat is offered because it moved, the button leads to that kind's own lookup: 3c, or the chat when the new link does not claim the name, cannot be fetched or cannot be joined yet (N12, N19, N35), or another chat of the user's at the new link (N35). A button offered for a kind with nothing local leads to that kind's own answer: 2a, 2g, 2h, or the channel's no-relays or app-update alert.

## 4. Changes against today

"Today" is #7525 as of `b78d76ae6`, with its apps; the canvas tags instead compare with master before #7525, `643a9afad`. Prices are the registry's per-year price for the label's length, times 2 years.

| # | Story | Today | New |
|---|---|---|---|
| 1 | The name is live and also reserved for community, whatever is local | the community alert instead of the plan | the plan, as for any live name |
| 4 | Bare name; a chat; the name also leads to the other kind at a link the user has no chat at (e.g. channel `#bakery`, the name now has only a contact link) | the channel's plan if the name has a channel link, else the contact's; the local chat is shown only as the other kind's button, if at all | the local chat; from search, 3e: "bakery.simplex also leads to …", with Join channel or Connect; from the new chat sheet, the chat's alert with that button. A contact `@bakery` is answered only when the channel cannot be joined |
| 7 | A chat; the name leads to a new link whose profile does not claim the name | the "Unconfirmed name" error alert | the chat, no alert |
| 8 | A chat; the request fails | the "SimpleX name error" alert | the chat, no alert |
| 9 | A chat; the name leads to a new link; from a message | 3c with Open new chat, Cancel | 3c with Open new chat, Open existing chat (opens the first of `localChats`), and no Cancel |
| 10 | A chat; the name is available | "Name no longer registered", "from $X per year" | the same alert, "$Y for 2 years" |
| 11 | Own address or channel; the name leads to another link | "Connect to yourself?", or the own channel | 3c: "alice.simplex now leads to a new address", as for a chat, including 9 for the own channel (N20) |
| 14 | Own; the name is available | "Your name has expired", "from $X per year" | the same alert, "$Y for 2 years" |
| 15 | Bare name; own address; the name also has a channel link the user has no chat at | the channel's join sheet | the channel's join sheet when the channel can be joined; otherwise "Connect to yourself?" (4a) with Join channel |
| 16 | `#d`, or a bare name whose contact kind fails too; nothing local; the channel's link has no relays or needs an app update, and its profile does not claim the name | the no-relays or app-update alert | "Unconfirmed name" (2g) |

Unchanged, and checked in the review:
- typed names never offer the other kind;
- nothing local and no link of kind K: 2f, with no other kind;
- a chat or own, and the name is not registered: no warning;
- own, no link of its kind: no warning;
- own and the request failed: the error alert.

## 5. The type

```haskell
data NameWarning
  = NWExpired {expiredAt :: UTCTime, graceUntil :: Maybe UTCTime}
  | NWOwnExpired {expiredAt :: UTCTime, graceUntil :: Maybe UTCTime}
  | NWAvailable {price :: NamePrice}
  | NWNoLongerRegistered {price :: NamePrice}
  | NWOwnAvailable {price :: NamePrice}
  | NWReservedForCommunity
  | NWNotRegistered
  | NWNoValidLink

data NamePrice = NamePrice {amount :: USDCents, years :: Int}
```

- `expiredAt` is present whenever a name is expired, because a registration without `expires` counts as live. A missing `graceUntil` is the canvas's dateless variant, and a `graceUntil` that has passed is dropped (N30).
- **Local chats.** `CPContactAddress` and `CPGroupLink` have `localChats :: [AChatInfo]`: the chats the plan is about, the planned one first, and for a name every local chat of the looked-up kinds. `CRConnectionPlan` has `offerLookup :: Bool`, false only when the planned kind is a chat whose registration is cached, and for a link, where nothing is looked up (N31, N32).
- `NamePrice` is the price of the 2-year term (N3): the registry's per-year price for the label's length (or its base price), times `years = 2`.
- The domain is not repeated. It is `planSimplexName`, or `simplexDomain` on `CPNameNotConnectable`.

**Deciding the warning.** Two pure functions, next to `setAddressChanged`:

```haskell
nameLinkOrWarning :: SystemSeconds -> SimplexNameInfo -> NameRegistration -> Either NameWarning ShortLinkContact
setNameWarning :: NameWarning -> ConnectionPlan -> ConnectionPlan
```

- **`nameLinkOrWarning`** returns the name's link of the kind, or the warning for nothing local (the "nothing" rows of §3). The label length check moves from the apps into it.
- **`setNameWarning`** sets the warning for the local plan, as one `case` over the plan constructors:
  - own: `NWExpired` becomes `NWOwnExpired`, `NWAvailable` becomes `NWOwnAvailable`, and `NWReservedForCommunity` stays (N13); the others become no warning;
  - a chat: `NWAvailable` becomes `NWNoLongerRegistered`, and `NWExpired` and `NWReservedForCommunity` stay; the others become no warning.

## 6. Plan logic, as a story per target

**The local lookups.** Two functions, one per kind, in the `where` of `connectPlan`'s short link branches, used by the name and link paths:
- **the contact kind:** the own address, else a contact, else a business chat (`getGroupToConnect`, which matches `business_chat IS NOT NULL` for `@` names);
- **the channel kind:** the own channel, else a joined or prepared channel.

Each returns the plan for what it finds.

**A typed name `@d` or `#d`**

1. Look up the name's kind locally.
2. With `resolve=never`, answer with what was found, or fail with `CENotResolvedLocally`.
3. Take the registration from the name cache when it is within its maximum age; otherwise resolve it and store it. If the request fails, answer with a chat, or fail.
4. `nameLinkOrWarning` gives either:
   - **a link L:**
     - own or a chat at L is answered as found;
     - otherwise the plan for L, with `addressChanged` if something was local; if L's profile does not claim the name, or L's channel has no relays or needs an app update, or a connection via L is in progress, or, for a chat, L's data cannot be fetched, and something was local, answer with it instead (N12, N19, N35); a chat the user has at L is answered as that chat (N35);
   - **a warning:** the local plan with `setNameWarning`, or `CPNameNotConnectable d` with the warning.

**A link target** keeps today's steps: a local chat is answered, and `resolve=all` refreshes a known channel from its link data.

**A bare name `d`**

1. Plan the channel as the typed name `#d`. Answer with it when it can be joined, including a local channel.
2. When the channel has nothing to connect to, answer with the contact's plan as the typed name `@d`.
3. When the channel failed or cannot be joined, answer with the contact's plan if it can be joined, otherwise with the channel's (N29).
4. Set `otherSimplexName` to the other kind's name if the cached registration has a live link of it, unless the local lookup of that kind found something at that link (N22, N29). Add that kind's local chat to `localChats`.

## 7. CLI

`viewNameWarning` prints one line after the plan, replacing today's `registered …` / `available …` / `reserved …` lines:

- `SimpleX name bakery.simplex expired on 2027-06-24, its owner can renew it until 2027-09-22`
- `your SimpleX name alice.simplex expired on 2027-06-24, renew it before 2027-09-22`
- `SimpleX name sunflower.simplex is available: $20 for 2 years`
- `SimpleX name bakery.simplex is no longer registered, available: $20 for 2 years`
- `your SimpleX name alice.simplex is no longer registered, available: $20 for 2 years`
- `SimpleX name privacy.simplex is reserved for community`
- `SimpleX name acme.simplex is not registered`
- `SimpleX name boogaloo.simplex has no valid link`

A missing `graceUntil` drops the second clause of the expiry lines. `otherSimplexNameNote` is unchanged. The prices are the test registry's ($10 per year).

## 8. Apps

- **Alert.** `showNameRegistrationAlert` becomes `showNameWarningAlert`, a plain `case` from `NameWarning` to title, message and action (Renew, Register, Re-register, Connect to SimpleX team). It keeps "Open existing chat" when the plan has a chat (1d).
- **Flow.** `planAndConnect` shows the alert when the plan has a warning. Otherwise:
  - a plan for a chat, prepared contact or own channel (`CAPKnown`, `CAPContactViaAddress`, `GLPKnown`, `GLPOwnLink`) with `otherSimplexName` shows 3e from search (N14, N17); from the new chat sheet, the chat's, prepared contact's or own channel's alert carries the other kind's button;
  - every other plan proceeds as today.

  There is no `isOwn`, `notConnectable`, `hasLocalChat`, expiry or length logic in either app.
- **3c from a message.** The buttons are Open new chat (Open new channel) and Open existing chat, with no Cancel. Open existing chat opens the first of the plan's `localChats`, which has no chat for the own address, so it gets Cancel (N20).
- **After Open new chat, Open new channel or Join from search.** The search and its filter stay (N17); the prepared chat is not added to the filter.
- **Name search.** The chat list's "Connect to" row passes the filters, as a pasted link does (N17). The new chat sheet's row passes none, so it behaves as from a message; it does no lookup while typing, so it always shows. For a name without `@` or `#` whose kinds are both local at the name's links, it shows the channel's alert alone, since a kind local at its link is not offered; `@d` reaches the contact. The search makes one `resolve=never` lookup of the typed text, filters to its `localChats` and shows the row while `offerLookup` holds (N31); a tap replaces the filter with its own answer's `localChats` and leaves the row to the next search.
- **Local chats.** Both apps filter to, add to or update in the chat list, and open only the plan's `localChats`, also for the own channel with a warning. The other kind's button keeps the first answer's chats and adds its own (N32, N33).
- **Types.** Kotlin and Swift get `NameWarning` and `NamePrice` in place of `NameRegistration` and `NamePricing`. The hand-written Swift decoder for `NameRegistration` goes away: `NameWarning` is chat's own type and derives like its neighbours.
- **Strings.** The price strings change from "from %s per year" to "%s for %d years". 3e needs a title; its buttons reuse "Join channel %s" / "Connect to %s" and OK.

## 9. Canvas changes

- **2a, 3c, 4a:** the other kind's button is shown for bare names only.
- **3c:** also applies to the own address and channel. From a message, it shows Open new chat and Open existing chat, with no Cancel; for the own address, Cancel (N20).
- **3e (new):** from search, a bare name matches a chat, prepared contact or own channel, and the name also leads to the other kind: "bakery.simplex also leads to channel #bakery", Join channel #bakery, OK. From the new chat sheet, the chat's, prepared contact's or own channel's alert shows the other kind's button instead.
- **Prices:** "$X for 2 years", computed from the registry's price. The amounts on the canvas are examples.
- **3a:** "Still leads to your chat, or not found, no valid link, another name, its new link fails or can't be joined yet, or the request failed" matches §3, N19 and N35; another chat of the user's at the new link shows instead (N35).
- **1a, 3f, band 3, footer:** the device answers and "Connect to" hides while the planned kind is a chat whose name was resolved within a day, never for the own name (N28, N31).
- **3e′:** also when the user's contact or own address moved; its own lookup shows 3c (N28).
- **4a, 4a′:** also when the name's new link can't be joined yet (N35). 4a′ is the own channel from a message or the new chat sheet (N34).

## 10. What goes away

- `nameRegistration_` on `CPContactAddress`/`CPGroupLink`, `nameRegistration` on `CPNameNotConnectable`, and `setPlanRegistration`.
- The `PRMUnknown` equation.
- The pre-resolve branch of `CTShortContact`, `resolveNameLink`, `nameHasLink` and `nameExpired`.
- `viewNameRegistration`, replaced by `viewNameWarning`.
- In both apps: the decisions in `showNameRegistrationAlert`, `NameRegistration.expired`, `reservedForCommunity`, `centsPerYear`, `nameCentsPerYear`, and the Swift `NameRegistration` decoder.

## 11. Decisions

Decided:

| # | Question | Answer |
|---|---|---|
| N1 | Where the warning is | `nameWarning_ :: Maybe NameWarning` on `CPContactAddress`/`CPGroupLink`, `nameWarning :: NameWarning` on `CPNameNotConnectable` |
| N2 | Own and chat variants | separate constructors |
| N3 | Price | the registry's per-year price for the label's length, times 2 years |
| N4 | A live name with `reservedReason_ = community` | no warning |
| N5 | 3c | `addressChanged :: Bool` stays |
| N6 | Answers up to a day old for a chat | accepted |
| N7 | The name no longer has a link of a chat's or own's kind | not reported |
| N8 | The other kind | offered for bare names only, unless the user has a chat, own address or own channel of that kind at its link |
| N9 | Own address or channel at another link than the name's | 3c, as for a chat |
| N10 | 3c from a message | Open new chat, and Open existing chat (the first of the plan's `localChats`), no Cancel |
| N11 | Not registered (reserved for another reason, or too short), with a chat or own | not reported |
| N12 | The new link does not claim the name, with a chat or own | the local one, no alert |
| N15 | The request failed | with a chat: the chat, no alert; with own or nothing: the error alert |
| N16 | Where the bare name's lookups and freshness are | the two local lookups are in the `where` of `connectPlan`'s short link branches; the bare name path plans each kind through `connectPlan`, and looks up the other kind with `resolve=never`; freshness is the name cache's |
| N17 | Name search (the chat list's "Connect to" row) | behaves as the canvas's search (1c): it passes the filters, so found chats stay filtered, dismissing keeps the search, 3c shows Cancel and 3e is the alert |
| N18 | `/c` when the name moved (3c) or the own name has a warning | the plan is shown instead of connecting (`connectionPlanProceed`) |
| N19 | A chat, and the name's link data cannot be fetched | the chat, no alert, as N15 |
| N20 | 3c for the own address from a message | Cancel: there is no chat to open |
| N21 | 3c for a channel | "… now leads to a new channel." in the information line; Open new channel, and Open existing chat from a message |
| N22 | Bare name, the channel failed and the contact kind is planned | the channel is offered as the other kind when the name has a live channel link |
| N23 | A chat answered without a warning (N7, N11) | its registration is cached, so the name is not resolved again for a day |
| N24 | Expired without a grace date | "expired on <date>", without a renew clause, as the CLI |
| N25 | Dates in the alerts | one long localized format in both apps |
| N26 | Bot clients | `connLink` is optional in Python and Node; `resolve=allGroups` still parses |
| N13 | Own name reserved for community after its registration ended | `NWReservedForCommunity`, 2d's alert |
| N14 | What shows 3e | the app, for a chat's, prepared contact's or own channel's plan with `otherSimplexName` |
| N27 | Comparing the name's link with a local one | `sameShortLinkContact`: the server's key hash is not compared |
| N28 | When to resolve a name | through the name cache: a registration is reused for a day while the planned kind is a local chat, for 5 minutes with nothing local, and never for the own address or channel; every lookup compares the local chat with the registration |
| N29 | Bare name, nothing local, the channel has no relays or needs an app update | the contact kind is planned, as when the channel fails (N22); if the contact fails too, the channel's plan |
| N30 | Expired, and the grace date has passed | the dateless variant (N24) |
| N31 | Name search | one `resolve=never` lookup; the list shows its `localChats`; the row shows while `offerLookup` holds, so the own name never hides it |
| N32 | The local chat in the apps | only the plan's `localChats`: filtered, added to the list or updated in it, and opened by Open existing chat; `existingChat_` is removed. "You are already joining the group" keeps its alert and is not filtered |
| N33 | The other kind's button from search | keeps the first answer's chats in the filter and adds its own, so its 3c shows Cancel (N17) |
| N34 | iOS against Kotlin | the name line in alerts, the own channel's "Your channel" text, a contact prepared at the name's address opened as in Kotlin, and the other kind's button on the "Repeat join request?" sheet |
| N35 | A local chat's name leads to a link that cannot be joined yet (no relays, needs an app update, connecting), or to another chat the user has | the local chat for the first, the other chat for the second |
| N36 | Chats whose name verification changes while planning or preparing | core emits `CEvtContactUpdated` / `CEvtGroupUpdated` for each of the user's other chats of the name's kind whose verification it clears (`unverifyNameChats`); the verified chat is in the command's response |

## 12. Tests

- **Unit tests:**
  - `nameLinkOrWarning`: one per "nothing" row of §3, plus the dateless, too-short, 2-year price and live-community cases;
  - `setNameWarning` for the own address (`CPContactAddress CAPOwnLink`).

  The chat rows need a `Contact` or `GroupInfo`, and are covered by CLI tests.
- **CLI tests:** "connection plan: the name lookup answers" asserts the warning lines instead of the registration lines. Tests are added for:
  - §4 stories 4, 7, 8, 11 and 15, and 10 and 14 through the price in the warning lines; story 1 is a unit test;
  - a chat with a name reserved for community (3d);
  - a chat and own with a name not registered (no warning);
  - a business chat, with a cached registration and resolved again;
  - a name with nothing local, resolved again after 5 minutes.
- **Changed:** the freshness tests (§9 of the lookup plan) assert the warning lines; `testPlanKnownNameStale` detects re-resolution by an expired registration.

## 13. Order of work

1. Core:
   - `NameWarning`, `NamePrice`, `nameLinkOrWarning` and `setNameWarning`, with their unit tests;
   - the local lookups;
   - the typed name and bare name paths;
   - the View;
   - the CLI tests.
2. Regenerate the bot API types.
3. Kotlin, then Swift: the `case`, 3e, 3c from a message, the strings.
4. Update the lookup plan (`plans/2026-09-22-name-lookup-core-api.md`) §2–§4, and the canvas (§9).

## 14. Done means

- No app reads a `NameRegistration`, compares dates or lengths, or decides whether to alert.
- The typed name and bare name paths read as §6. Freshness is computed only by the local lookups.
- Every row of §3, and every story of §4 except 9 (apps only), has a unit test or a CLI assertion, except the own address or channel at a link that cannot be joined yet, and a chat at a link that needs an app update or is still connecting, which take the tested no-relays path through `setAddressChanged`; the app-update half of N29, which shares the tested no-relays branch; and story 11 for the own channel, which follows the own address's tested path.
