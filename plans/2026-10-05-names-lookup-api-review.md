I read `git diff master -w` in full (6,074 lines, 46 files) and traced the call chains it touches: `Commands.hs`, `Internal.hs`, `Store/Direct.hs`, `Store/Groups.hs`, `Store/Messages.hs`, `View.hs`, `Controller.hs`, and both apps' event handlers and `ChatModel`. I did not build anything or run the tests.

Findings are listed most severe first.

### 1. Opening a channel or business chat found by name can put the chat in the list twice (likely crash on Android and desktop)

- **What triggers it:** `nameChatsUpdated` is now called in `APIPrepareGroup` (`Commands.hs:2230`) and in the business branch of `APIPrepareContact` (`Commands.hs:2199`), after the new chat is created.
- **Why the new chat gets an event:** `getNameChats` (`Store/Groups.hs:2760`) selects every group whose `group_domain` matches, so the new chat is included. A `CEvtGroupUpdated` for it is queued at once by `toView_` (`Controller.hs:1785`). Several store transactions run after that, so the app normally processes the event before the command's response.
- **First add:** on that event, iOS `updateGroup` (`ChatModel.swift:634`) and Kotlin `updateGroup` (`ChatModel.kt:493`) add the chat if it is missing.
- **Second add:** on the response, `addChat` is called unconditionally (`NewChatView.swift:1142`, `1209`; `ConnectPlan.kt:855`, `947`). Neither `addChat` checks for an existing entry.
- **Consequence:** Kotlin keys the chat list by `remoteHostId to id` (`ChatListView.kt:1100`). A `LazyColumn` throws `IllegalArgumentException` on a duplicate key.
- On master, `APIPrepareGroup` produces no event, so this is new with the branch.
- **Suggested fix:** send events only for the chats whose flag `unverifyNameChats` actually changed (read their ids before the `UPDATE`). This also removes the second event for the same chat in `updateContactFromLinkData` and `updateGroupFromLinkData` (`Internal.hs:1659-1660`, `1639-1640`).

### 2. The local lookups return a `ConnectionPlan`, so "what is local" is re-matched in eight places

- `knownContactPlans` and `knownGroupPlans` return `Maybe ((ACreatedConnLink, ConnectionPlan), Bool)`.
- The same constructors are then matched again in:
  - `knownChat` (4642)
  - `setKnownVerified` (4536)
  - `confirmOther` (4471)
  - the stale branch of `otherKindResolved` (4465)
  - the group `confirmKnown` (4587)
  - `planChat` (4691)
  - `setNameWarning`
  - `setAddressChanged`
- `setKnownVerified` and `confirmOther` are the same operation (verify with expiry, then `nameChatsUpdated`), written twice in two styles.
- Reaching the fields takes `fst . fst`, `snd . fst`, `maybe False snd`, and positional rewrites of the 4-tuple (`\(l, planName, _, p) -> (l, planName, other, p)`, `unjoinable (_, _, _, p)`).
- **Suggested:** a dedicated type for the local entry (own address, own channel, contact, group), holding its link and freshness. One verify function would replace both copies, and the plan would be built from it once.

### 3. The same work is repeated within one call

- **Store lookups run three times for a bare name:**
  - once in `CTDomain` (4434-4435);
  - again for the planned kind in the recursive `CTShortContact` call (4494, 4585);
  - a third time in `planLocalChats` (4687).
- **`planLocalChats` repeats work `connectPlan` already did:**
  - it switches on the connect target again (4686-4690);
  - it re-matches the plan just produced (`planChat`);
  - `offerLookup` re-evaluates the `allFresh` predicate (4437 vs 4683).
- **`nameLinkOrWarning` runs twice for the planned kind**, each time with its own `getSystemSeconds` reading (4447, 4562). At an expiry boundary the two readings can disagree.
- **Wasted queries on connect:** for `Connect`, `planLocalChats` runs before `connectWithPlan` (2417), but its result is used only when the plan is shown instead of executed.

### 4. `/_verify domain` sets the resolution time and clears it in the next statement

- At `Commands.hs:2424` and `2439` the code is `setXDomainVerified … <* setXDomainStale …`. `setContactDomainVerified` and `setGroupDomainVerified` always store `resolved_at = now`, so this caller has to undo it.
- **Suggested:** pass the resolution as `Maybe (UTCTime, Maybe UTCTime)`, the same type `getContactDomainResolution` returns.

### 5. The bare-name branch is hard to read

- **One name, two meanings:** `unjoinable` is a predicate on the result tuple at 4458 and an action that checks the claim and builds a plan at 4594, both inside `connectPlan`.
- **Order of definitions:** the `Right reg` branch (4446-4484) has eleven local definitions before a two-statement body.
- **Dense expression:** lines 4480-4481 combine a lambda, an `if`, a nested `catchAllErrors` and a fallback in one expression.
- **Unnamed rule:** the rule "a chat keeps this warning" is computed by building a plan with `setNameWarning` and testing `nameWarning_` (4469, 4568). It deserves a name.

### 6. Existing helpers are not reused

- **Group claim expression:** `claimDomain <$> (publicGroup >>= publicGroupAccess >>= groupDomainClaim)` is added inline at `Commands.hs:2227`, `4128` and `4593`. It already exists as `groupClaim` (local to `Internal.hs:1648`) and `groupSimplexDomain` (`View.hs:1162`).
- **Business-chat SQL condition:** the `business_chat IS [NOT] NULL` fragment, chosen by name type, now appears three times: `Groups.hs:1100`, `Groups.hs:2770`, `Direct.hs:613-616`.
- **Date formatting:** Kotlin `nameDate` (`ConnectPlan.kt:63`) repeats the body of the private `badgeDateText` (`ChatModel.kt:2438`).

### 7. In both apps, `filterChats` changes state and also decides the branch

- The callback sets the search filter and returns whether the alert is suppressed. It is called inside conditions:
  - iOS: `if let chatInfo = localChats.first, filterChats?(localChats) != true` (`NewChatView.swift:1485`, `1563`, `1659`)
  - Kotlin: `takeIf { filterChats?.invoke(localChats) != true }` (`ConnectPlan.kt:193`, `274`, `364`)
- Whether the filter changes therefore depends on short-circuit evaluation.
- `_ = filterChats(result.localChats)` (`ChatListView.swift:863`) discards the result.
- **Suggested:** separate the decision from the filtering.

### 8. The warning is extracted by a second switch over the plan, leaving unreachable branches

- Both apps switch on the plan to extract the warning (`NewChatView.swift:1477`, `ConnectPlan.kt:186`), then switch on it again.
- `CPNameNotConnectable` always has a warning, so these branches never run:
  - `guard let connectionLink` (`NewChatView.swift:1492`)
  - `if (connectionLink == null)` (`ConnectPlan.kt:197`)
  - `.nameNotConnectable` (`NewChatView.swift:1796`, `ConnectPlan.kt:494`)
- `View.hs:217` has the same double switch: `viewConnectionPlan` followed by `viewNameWarning`.
- A `nameWarning` field on `CRConnectionPlan`, next to `planSimplexName`, would remove all three second switches. This reverses decision N1 in `plans/2026-09-28-name-warnings.md`, so the call is yours.

### 9. Changes outside the feature

- **iOS behaviour change for all addresses:** the `.contactViaAddress` case (`NewChatView.swift:1640`) no longer offers "Use current profile / Use new incognito profile"; it opens the contact instead. This applies to every pasted address, not only names, and deserves its own commit.
- **Unrelated tooling:** `plans/sketches/README.md` and `normalize-excalidraw.py` are tooling. The README says the script runs from `.git/hooks/pre-commit`, but that hook is not in the repository.

### 10. Minor

- `localChats` and `offerLookup` are optional in `AppAPITypes.swift:856`, although iOS only decodes its own core's responses. `addressChanged` in the same response is required.
- `unverifyNameChats` takes a `UserId`, while the neighbouring store functions take a `User`.
- `gPlan` (4667) has no type signature, and `knownContactPlans`/`knownGroupPlans` each return a single plan despite the plural names.
- `nameChatsUpdated` uses `withStore'`, while the surrounding planning code uses `withFastStore'`.
- Tests:
  - `testPlanKnownNameStale` and `testPlanNameOtherKindBusiness` write their backdating SQL inline, next to the existing `setContactNamesStale` helper;
  - `testPlanNameOtherKindBusiness` repeats most of `withTeamChats`.
- `plans/2026-09-22-name-lookup-core-api.md` records history (§1), rejected alternatives (§7.7) and outdated line references (`Commands.hs:5082-5088`).
