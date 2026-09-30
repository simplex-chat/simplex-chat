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
  - A chat at the name's link is confirmed.
  - A chat, own address or own channel at another link gives the new link's plan with `addressChanged` (3c).
  - Nothing local gives the new link's plan (2a).
- **A bare name (`d`) is planned for both kinds.**
  - It looks up both kinds locally and resolves the name once.
  - It plans the kind that matches locally (the channel first), otherwise the kind the name has a link for.
  - It offers the other kind when the name has a link of it that the user does not have.
- **The local lookup of each kind also returns whether the chat's name was resolved within a day.** It reads this where it finds the chat: the contact's resolution for a contact, the group's for a business chat or channel. No function checks freshness for both kinds.
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
  - **the same link:** equal after the server address is normalized (`serverShortLink`);
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
  - channel kind: a joined channel (not a business chat) the user is still a member of.

  In both cases the name `d` is verified, and a short link is stored. A contact connected by a one-time invitation, or with an unverified name, is not a chat for the name.
- **local:** a chat, the own address or the own channel, of the kind, for the name. **Nothing local:** none of them.
- **fresh:** the chat's name was resolved within the last 24 hours, and the expiry stored with it has not passed. The own address and channel are never fresh, so an owner sees a change on every lookup.
- **from search, from a message:** the canvas entry points 1a and 1b.

## 3. Scenarios and answers

From `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`.

**A typed name `@d` or `#d`, of kind K**

| Name | Local of kind K | Answer | Warning | Screen |
|---|---|---|---|---|
| live, link L | nothing | L's plan | — | 2a |
| live, link L | chat at L | the chat, confirmed | — | 3a |
| live, link L | chat at another link | L's plan, `addressChanged` | — | 3c |
| live, link L | own at L | own | — | 4a |
| live, link L | own at another link | L's plan, `addressChanged` | — | 3c |
| live, link L whose profile does not claim `d` | nothing | error `SDEUnknownDomain` | — | 2g, 4b |
| live, link L whose profile does not claim `d` | chat or own | the local one | — | 3a, 4a |
| live, no link of kind K | nothing | not connectable | `NWNoValidLink` | 2f |
| live, no link of kind K | chat or own | the local one | — | 3a, 4a |
| expired | nothing | not connectable | `NWExpired` | 2b |
| expired | chat | the chat | `NWExpired` | 3b, 1d |
| expired | own | own | `NWOwnExpired` | 4c |
| available | nothing | not connectable | `NWAvailable` | 2c |
| available | chat | the chat | `NWNoLongerRegistered` | 3d |
| available | own | own | `NWOwnAvailable` | 4d |
| reserved for community | nothing or chat | not connectable, or the chat | `NWReservedForCommunity` | 2d, 3d |
| reserved for community | own | own | `NWReservedForCommunity` (N13) | 2d's alert |
| not registered | nothing | not connectable | `NWNotRegistered` | 2e |
| not registered | chat or own | the local one | — | 3a, 4a |
| request failed | chat | the chat | — | 3a |
| request failed | own or nothing | error | — | 2h |
| not asked: a fresh chat in the default mode, or `resolve=never` | chat or own | the local one | — | 3a, 4a |

A typed name has no `otherSimplexName`: `@` and `#` state the user's intent.

**A bare name `d`**

The match is what the channel kind's local lookup found, else what the contact kind's found.

| Local | Answer |
|---|---|
| `resolve=never` | the match, or `CENotResolvedLocally` |
| default mode, and the match is a fresh chat | the chat, from the store |
| any other match: own, a chat that is not fresh, or `resolve=all` | the resolved registration, planned as the typed name of the match's kind (the rows above) |
| nothing local | the channel's plan if the name has a live channel link, falling back to the contact kind if it fails; otherwise the contact kind's plan if the name has a live contact link; otherwise not connectable with the "nothing" row's warning; a failed request is an error |

When the name is resolved and has a live link of the kind not planned, `otherSimplexName` is that kind's name, unless the user has a chat, own address or own channel of that kind at that link. The plan's screen shows it:
- the second button of 2a, 3c and 4a;
- 3e for a chat (new).

## 4. Changes against today

"Today" is #7525 as of `b78d76ae6`, with its apps. Prices are the registry's per-year price for the label's length, times 2 years.

| # | Story | Today | New |
|---|---|---|---|
| 1 | The name is live and also reserved for community, whatever is local | the community alert instead of the plan | the plan, as for any live name |
| 4 | Bare name; a chat that is not fresh; the name also leads to the other kind at a link the user has no chat at (e.g. channel `#bakery`, the name now has only a contact link; or contact `@bakery`, the name has both) | the channel's plan if the name has a channel link, else the contact's; the local chat is shown only as the other kind's button, if at all | the local chat, and 3e: "bakery.simplex also leads to …", with Join channel or Connect |
| 7 | A chat; the name leads to a new link whose profile does not claim the name | the "Unconfirmed name" error alert | the chat, no alert |
| 8 | A chat that is not fresh; the request fails | the "SimpleX name error" alert | the chat, no alert |
| 9 | A chat; the name leads to a new link; from a message | 3c with Open new chat, Cancel | 3c with Open new chat, Open existing chat (opens the plan's `existingChat_`), and no Cancel |
| 10 | A chat; the name is available | "Name no longer registered", "from $X per year" | the same alert, "$Y for 2 years" |
| 11 | Own address or channel; the name leads to another link | "Connect to yourself?", or the own channel | 3c: "alice.simplex now leads to a new address", as for a chat, including 9 |
| 14 | Own; the name is available | "Your name has expired", "from $X per year" | the same alert, "$Y for 2 years" |
| 15 | Bare name; own address; the name also has a channel link the user has no chat at | the channel's join sheet | "Connect to yourself?" (4a) with Join channel |

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

- `expiredAt` is present whenever a name is expired, because a registration without `expires` counts as live. A missing `graceUntil` is the canvas's dateless variant.
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

**The local lookups.** Two functions, one per kind, moved from the kind branches to the `where` of the `SCMContact` equation, so the bare name path can use them:
- **the contact kind:** the own address, else a contact, else a business chat (`getGroupToConnect`, which matches `business_chat IS NOT NULL` for `@` names);
- **the channel kind:** the own channel, else a joined channel.

Each returns the plan for what it finds, and whether it is fresh. It reads the contact's resolution for a contact, and the group's for a business chat or channel. The own address and channel are not fresh.

**A typed name `@d` or `#d`**

1. Look up the name's kind locally.
2. With `resolve=never`, answer with what was found, or fail with `CENotResolvedLocally`.
3. In the default mode, answer with a fresh chat.
4. Resolve the registration, unless the bare name path passed it. If the request fails, answer with a chat, or fail.
5. `nameLinkOrWarning` gives either:
   - **a link L:**
     - own at L is answered as found, and a chat at L is confirmed (`setContactDomainVerified`, `setGroupDomainVerified`, or the channel's refresh from its link data);
     - otherwise the plan for L, with `addressChanged` and `existingChat_` (the local chat or own channel) if something was local; if L's profile does not claim the name and something was local, answer with it instead;
   - **a warning:** the local plan with `setNameWarning`, or `CPNameNotConnectable d` with the warning.

**A link target** keeps today's steps: a local chat is answered, and `resolve=all` refreshes a known channel from its link data.

**A bare name `d`**

1. Look up both kinds locally. The match is what the channel kind's lookup found, else what the contact kind's found.
2. With `resolve=never`, answer with the match, or fail with `CENotResolvedLocally`.
3. In the default mode, answer with the match if it is a fresh chat.
4. Resolve the registration once. If the request fails, answer with the match if it is a chat, or fail.
5. Plan the kind:
   - the match's kind, if there is a match;
   - otherwise the channel if the name has a live channel link, falling back to the contact kind if that fails;
   - otherwise the contact kind if the name has a live contact link.

   It is planned as the typed name, passing the registration. With no kind to plan, answer `CPNameNotConnectable d` with the "nothing" warning.
6. Set `otherSimplexName` to the other kind's name if the name has a live link of it, unless the local lookup of that kind found something at that link.

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

- **Alert.** `showNameRegistrationAlert` becomes a plain `case` from `NameWarning` to title, message and action (Renew, Register, Re-register, Connect to SimpleX team). It keeps "Open existing chat" when the plan has a chat (1d).
- **Flow.** `planAndConnect` shows the alert when the plan has a warning. Otherwise:
  - a plan for a chat (`CAPKnown`, `GLPKnown`) with `otherSimplexName` shows 3e (N14);
  - every other plan proceeds as today.

  There is no `isOwn`, `notConnectable`, `hasLocalChat`, expiry or length logic in either app.
- **3c from a message.** The buttons are Open new chat (Open new channel) and Open existing chat, with no Cancel. Open existing chat opens the plan's `existingChat_`, which core leaves empty for the own address, so it gets Cancel (N20).
- **Name search.** The "Connect to" row passes the filters, as a pasted link does (N17).
- **Types.** Kotlin and Swift get `NameWarning` and `NamePrice` in place of `NameRegistration` and `NamePricing`. The hand-written Swift decoder for `NameRegistration` goes away: `NameWarning` is chat's own type and derives like its neighbours.
- **Strings.** The price strings change from "from %s per year" to "%s for %d years". 3e needs a title, a message and its buttons.

## 9. Canvas changes

- **2a, 3c, 4a:** the other kind's button is shown for bare names only.
- **3c:** also applies to the own address and channel. From a message, it shows Open new chat and Open existing chat, with no Cancel.
- **3e (new):** a bare name matches a chat, and the name also leads to the other kind: "bakery.simplex also leads to channel #bakery.simplex", Join channel, OK. From a message it also shows Open existing chat.
- **Prices:** "$X for 2 years", computed from the registry's price. The amounts on the canvas are examples.
- **3a:** unchanged: "Still leads to your chat, or not found, no valid link, another name, or the request failed" matches §3.

## 10. What goes away

- `nameRegistration_` on `CPContactAddress`/`CPGroupLink`, `nameRegistration` on `CPNameNotConnectable`, and `setPlanRegistration`.
- The `PRMUnknown` equation with `resolvedRecently`.
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
| N10 | 3c from a message | Open new chat, and Open existing chat (the plan's `existingChat_`), no Cancel |
| N11 | Not registered (reserved for another reason, or too short), with a chat or own | not reported |
| N12 | The new link does not claim the name, with a chat or own | the local one, no alert |
| N15 | The request failed | with a chat: the chat, no alert; with own or nothing: the error alert |
| N16 | Where the bare name's lookups and freshness are | the two local lookups move to the equation's `where` and return freshness; the bare name path uses both |
| N17 | Name search ("Connect to" row) | behaves as the canvas's search (1c): it passes the filters, so found chats stay filtered, dismissing keeps the search, 3c shows Cancel and 3e is the alert |
| N18 | `/c` when the name moved (3c) or the own name has a warning | the plan is shown instead of connecting (`connectionPlanProceed`) |
| N19 | A chat, and the name's link data cannot be fetched | the chat, no alert, as N15 |
| N20 | 3c for the own address from a message | Cancel: there is no chat to open |
| N21 | 3c for a channel | "… now leads to a new channel." in the information line; Open new channel, and Open existing chat from a message |
| N22 | Bare name, the channel failed and the contact kind is planned | the channel is not offered as the other kind |
| N23 | A chat answered without a warning (N7, N11) | stored as resolved, so it is not re-resolved for a day |
| N24 | Expired without a grace date | "expired on <date>", without a renew clause, as the CLI |
| N25 | Dates in the alerts | one long localized format in both apps |
| N26 | Bot clients | `connLink` is optional in Python and Node; `resolve=allGroups` still parses |
| N13 | Own name reserved for community after its registration ended | `NWReservedForCommunity`, 2d's alert |
| N14 | What shows 3e | the app, for a chat's plan with `otherSimplexName` |

## 12. Tests

- **Unit tests:**
  - `nameLinkOrWarning`: one per "nothing" row of §3, plus the dateless, too-short, 2-year price and live-community cases;
  - `setNameWarning` for the own address (`CPContactAddress CAPOwnLink`).

  The chat rows need a `Contact` or `GroupInfo`, and are covered by CLI tests.
- **CLI tests:** "connection plan: the name lookup answers" asserts the warning lines instead of the registration lines. Tests are added for:
  - §4 stories 4, 7, 8, 11 and 15, and 10 and 14 through the price in the warning lines; story 1 is a unit test;
  - a chat with a name reserved for community (3d);
  - a chat and own with a name not registered (no warning);
  - a business chat, fresh and not fresh;
  - a bare name with a fresh chat (no resolution).
- **Unchanged:** the freshness tests (§9 of the lookup plan) pass as they are.

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
- The typed name and bare name paths read as §6. Freshness is read only by the local lookups.
- Every row of §3, and every story of §4 except 9 (apps only), has a unit test or a CLI assertion.
