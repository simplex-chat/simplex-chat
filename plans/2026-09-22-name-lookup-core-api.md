# SimpleX name lookup — the core API the alert canvas needs

Defines the core half of #7525 (`ab/names-lookup-api`). Model: `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`, states 1a–4d. Sibling canvas `plans/sketches/2026-09-18-names-phase3a-registration.excalidraw` on `ab/names-actions-api` (#7530), §5. Registry types from simplexmq `ea43df2349f6d3dedd60d5e4aed21fd99316cad9`, `src/Simplex/Messaging/Names/Record.hs`.

The only UI work here is removing the name cache both apps kept (§9). This is the contract the Kotlin and Swift apps code against, and nothing in it depends on the wallet (#7475) or on either name-actions API.

---

## Table of contents

1. What #7525 declares, and what it does not produce
2. Type delta
3. Producer
4. The state → plan contract
5. Resolve modes, and the once-a-day rule
6. What stays an error
7. Decisions taken
8. Compile fixes and regeneration
9. Re-resolution in core
10. Order of work
11. Done means

---

## Executive summary

#7525 is a declaration-only sketch: it adds `nameRegistration_`, `CPNameNotConnectable` and `PRMAll` to `Controller.hs` and changes two arities, but no producer was written and no consumer was updated, so **the branch does not compile**. `resolveNameRecord` (`Commands.hs:5082-5088`) collapses every non-`NRRegistered` answer into `NAME NOT_FOUND`, so expiry, price and reserved-reason — the substance of nine of the sixteen states — never leave core.

The declared types are close to right. Twelve states map onto them as they stand, and of the four that do not, three are settled by wording or by a client-side reading rather than by the API (§4, §7). The work is therefore mostly **producer, not type**: stop discarding the registration, consult the existing by-name store lookups when the registry yields no usable link, and attach the registration to whichever plan comes out. One field is genuinely missing (`addressChanged`, for 3c) and one constructor needs its domain (`CPNameNotConnectable`, so the UI can print the bare name the canvas shows).

`getContactToConnect` / `getGroupToConnect` (`Direct.hs:802`, `Groups.hs:1090`) and `getUserContactLinkViaTarget` already accept `CTName` and query by domain. The by-name lookup that `CPNameNotConnectable`'s own precondition needs therefore exists — it is simply never reached, because the `CTDomain` branch throws first.

---

## 1. What #7525 declares, and what it does not produce

The diff against `fea9f482c` is eleven added lines in one file. It gives `PlanResolveMode` a `PRMAll` (`Controller.hs:714`, parser `:725`), makes `CRConnectionPlan.connLink` a `Maybe` (`:887`), hangs `nameRegistration_ :: Maybe NameRegistration` off `CPContactAddress` and `CPGroupLink`, adds `CPNameNotConnectable`, and updates `connectionPlanProceed` (`:1212-1233`).

None of it is reachable:

- **`CPNameNotConnectable` is never constructed.** `grep -rn CPNameNotConnectable src/` matches only the declaration, the `connectionPlanProceed` case and a comment.
- **`nameRegistration_` is never populated.** All 26 `CPContactAddress` / `CPGroupLink` occurrences in `Commands.hs` — constructions and patterns alike — still use the old arity.
- **`PRMAll` is parsed and never read.** `Commands.hs` tests only `== PRMNever` and `== PRMAllGroups`, so `resolve=all` silently behaves as `unknown`.
- **`connLink :: Maybe` has no producer** — `Commands.hs:2178` and `:4586` still pass a bare `ACreatedConnLink`.
- **It does not compile.** `View.hs:2234` and `:2252` pattern-match `CPContactAddress cap` / `CPGroupLink glp` at the old arity, `viewConnectionPlan` (`View.hs:2214`) takes a non-`Maybe` link and has no `CPNameNotConnectable` case, and `Commands.hs:2178`/`:4586` type-error on the response field.

**Scope.** Core: types, producer, CLI rendering, generated client types, tests, and one migration per backend (§9); in the apps, only removing the name cache. No new chat command.

**Not in scope, deliberately.** Registration, renewal and pricing actions — those are #7530 and are reached from the UI, not from a connection plan (§7).

---

## 2. Type delta

Four changes: two to the types in `Controller.hs`, two to `connectPlan` in `Commands.hs`.

**`CPNameNotConnectable` carries its domain.**

```haskell
| CPNameNotConnectable {simplexDomain :: SimplexDomain, nameRegistration :: NameRegistration}
```

`planSimplexName` cannot serve here. It is a `SimplexNameInfo`, which needs a `nameType`, and an unregistered name has none — today's code invents one by trying `NTPublicGroup` then `NTContact` (`Commands.hs:4432-4434`), which is arbitrary and becomes visible the moment the UI renders it. The canvas writes every band-2 body as a bare name (`sunflower.simplex is available…`, `bakery.simplex expired on…`), never `@`/`#`, so the UI wants `fullDomainName`, not `shortStr`.

**`CAPOk` and `GLPOk` gain `addressChanged :: Bool`.**

```haskell
| CAPOk {contactSLinkData_ :: Maybe ContactShortLinkData, ownerVerification :: Maybe OwnerVerification, addressChanged :: Bool}
| GLPOk {groupSLinkInfo_ :: Maybe GroupShortLinkInfo, groupSLinkData_ :: Maybe GroupShortLinkData, ownerVerification :: Maybe OwnerVerification, addressChanged :: Bool}
```

True when the name resolved to a link that differs from the one held by the local chat that claims this name. This is state 3c and nothing else expresses it: both "a link you do not have" and "a link that replaced yours" are `CAPOk` today. It states that the address moved and not who moved it, which is exactly what the canvas claims ("Only the address change is known, not who made it") and is the narrow form of the `ownerChanged` field dropped in `f3bcd4a16` — no owner identity is carried, stored or compared.

**`connectPlan` returns an optional link and threads the registration.**

```haskell
connectPlan :: User -> AConnectTarget -> PlanResolveMode -> Maybe LinkOwnerSig
            -> Maybe (Either ChatError NameRegistration)
            -> CM (Maybe ACreatedConnLink, Maybe SimplexNameInfo, Maybe SimplexNameInfo, ConnectionPlan)
```

The fifth parameter is `NameRecord` today (`Commands.hs:4392`); widening it to `NameRegistration` is what lets the recursion carry expiry, pricing and reserved-reason down to the plan that is finally built. `resolveNameLink` (`:4568-4574`) pattern-matches `NRRegistered {nameRecord}` and throws `SDENoValidLink` otherwise, as it effectively does now.

**`PRMAll` gets its meaning, and replaces `PRMAllGroups`.** It re-resolves a chat that is already known, instead of short-circuiting in `knownLinkPlans`. `PRMAllGroups` did this for groups only, so it is removed and the directory service uses `PRMAll`.

Nothing else is removed. `SimplexDomainError` keeps both constructors (§6).

---

## 3. Producer

**Split the registry call.** `resolveNameRecord` (`Commands.hs:5082-5088`) throws away everything the canvas needs. Add the honest one beside it and keep the old name as a wrapper, so the five callers that only ever want a record (`:1607`, `:2421`, `:3298`, `:5094`, `:5650`) do not change:

```haskell
resolveNameRegistration :: User -> NetworkRequestMode -> SimplexDomain -> CM NameRegistration
resolveNameRegistration user nm domain =
  registration <$> withAgent (\a -> resolveSimplexName a nm (aUserId user) domain)

resolveNameRecord :: User -> NetworkRequestMode -> SimplexDomain -> CM NameRecord
resolveNameRecord user nm domain =
  resolveNameRegistration user nm domain >>= \case
    NRRegistered {nameRecord} -> pure nameRecord
    _ -> throwError $ chatErrorAgent $ SMP "" (NAME SMP.NOT_FOUND)
```

**Rewrite the `CTDomain` branch** (`Commands.hs:4415-4442`). Today it resolves, and on anything but a live record with a usable link it calls `connectPlanNoName`, which recurses with a `Left` error and ends in `CPError`. It must instead fall through to the local lookup:

1. `PRMNever` — unchanged, `CENotResolvedLocally`.
2. Otherwise `resolveNameRegistration`. On a network or protocol failure, unchanged: the error becomes `CPError` (2h).
3. If the answer is `NRRegistered`, **not past `expires`**, and has a usable channel or contact link — today's path: pick the type, recurse as `CTName`, attach the registration to the plan that comes back.
4. Otherwise — `NRAvailable`, `NRReserved`, past `expires`, or no usable link — recurse as `CTName` anyway but **only as far as `knownLinkPlans`**. If it returns an own link or a known chat, that plan is the answer, with the registration attached and `connLink` carrying the stored link. If it returns nothing, the answer is `CPNameNotConnectable d registration` with `connLink = Nothing`.

Step 4 is the whole of 2b–2f, 3d, 4c and 4d, and it is why `connLink` had to become optional. It needs no new store query: `knownLinkPlans` (`:4482`, `:4553`) already resolves a `CTName` through `getUserContactLinkViaTarget`, `getContactToConnect` and `getGroupToConnect`, all of which match on `cp.contact_domain` / `gp.group_domain` with `*_verified = 1`.

**Expiry is a producer rule, not a UI rule.** An expired name must not yield a connectable plan — "It does not connect while only its owner can renew it". `expires` absent (a v20/v21 router sent the record alone) means expiry is unknown, so the name is treated as live — the only safe reading. The dateless fallback in §4 covers the other case: `expires` known and past, `graceUntil` absent.

**`addressChanged`.** Under `PRMAll`, or under `PRMUnknown` when the stored resolution is stale (§9), when the target is a `CTName` and `knownLinkPlans` returns a known contact or group, resolve the link anyway and compare it with the stored one. Equal, or fresh under `PRMUnknown`: today's `CAPKnown` / `GLPKnown`, which is 3a. Different: return the `Ok` plan **for the new link**, with `addressChanged = True` — the canvas draws 3c as a profile card for the new address with *Open new chat*, not as a known chat. Every other construction site passes `False`.

**Attach on every name target.** `nameRegistration_` is `Just` whenever the connect target was a name, `Nothing` for a link target, regardless of state — so 2a and 3a carry it too and the UI simply ignores it. One rule beats nine. The exception is a known chat answered from the store (§9): the registry is not asked, so it is `Nothing`.

---

## 4. The state → plan contract

`d` is the resolved `SimplexDomain`, `nr` the `NameRegistration`. "local" is what `knownLinkPlans` finds for `CTName`.

| state | registry answer | local | plan | `connLink` |
|---|---|---|---|---|
| 2a | `NRRegistered`, live, link usable | none | `CPContactAddress (CAPOk … False) (Just nr)` or `CPGroupLink (GLPOk … False) (Just nr)` | `Just` resolved |
| 2b | `NRRegistered`, `expires` past | none | `CPNameNotConnectable d nr` | `Nothing` |
| 2c | `NRAvailable {pricing}` | none | `CPNameNotConnectable d nr` | `Nothing` |
| 2d | `NRReserved NRRCommunity` | none | `CPNameNotConnectable d nr` | `Nothing` |
| 2e | `NRReserved` other, or `NRAvailable` with `minLabelLength` > label | none | `CPNameNotConnectable d nr` | `Nothing` |
| 2f | `NRRegistered`, live, no usable link | none | `CPNameNotConnectable d nr` | `Nothing` |
| 2g | link resolved, its profile claims another name or none | — | `CPError (… SDEUnknownDomain)` | unchanged |
| 2h | network or protocol failure | — | `CPError` | unchanged |
| 3a | any, or not asked when fresh (§9) | known chat | `CPContactAddress (CAPKnown ct) (Just nr)` / `CPGroupLink (GLPKnown …) (Just nr)`, with `Nothing` when not asked | `Just` stored |
| 3b | `NRRegistered`, `expires` past | known chat | `CPContactAddress (CAPKnown ct) (Just nr)` | `Just` stored |
| 3c | `NRRegistered`, live, link ≠ stored | known chat | `CPContactAddress (CAPOk … True) (Just nr)` | `Just` resolved |
| 3d | `NRAvailable` or `NRReserved` | known chat | `CPContactAddress (CAPKnown ct) (Just nr)` | `Just` stored |
| 4a | `NRRegistered`, live | own link | `CPContactAddress CAPOwnLink (Just nr)` | `Just` own |
| 4b | registered, leads nowhere of yours | own claim only | `CPError (… SDEUnknownDomain)` | unchanged |
| 4c | `NRRegistered`, `expires` past | own link | `CPContactAddress CAPOwnLink (Just nr)` | `Just` own |
| 4d | `NRAvailable` | own link | `CPContactAddress CAPOwnLink (Just nr)` | `Just` own |

The two `CPError` rows leave `connLink` exactly as today's code produces it: this change does not reshape the error path.

Four readings are left to the UI, because core cannot make them without guessing:

- **2c vs 2e** — `NRAvailable` is 2c when `minLabelLength <= length label`, and 2e otherwise. The registry prices a name it would still refuse; the length check is the client's. The canvas also folds "a subname" into 2e, and how the registry answers for one (`support.acme.simplex`, a non-empty `subDomain`) is not settled by `Record.hs` — B below must establish it against the mock and, if the answer is not already one of these rows, it is a gap to raise on #7525 rather than to paper over here.
- **Band 3 vs band 4** — both are `CAPKnown`/`CAPOwnLink`; which band an alert belongs to is whether the chat is the current user's own. 4b is 2g's error read against the user's own claimed domain, which is why both are `SDEUnknownDomain` and why 4b needs no core change.
- **Dateless variants** — `expires` present and past with `graceUntil` absent means the name is known to have expired but the grace end is unknown. 2b, 3b and 4c then drop the dates ("`bakery.simplex` has expired. Its owner can still renew it for a limited time.") rather than suppressing the alert. `expires` absent entirely never reaches these rows: §3 treats it as live.
- **Price wording** — `NamePricing` is US cents **per year** plus `minLabelLength`; it has no term. The lookup alerts therefore say *from $X per year* and never "for 2 years". The term exists only as `NameCredit.years` on #7530 and belongs on the registration screen, which is the only place it is authoritative.

---

## 5. Resolve modes, and the once-a-day rule

The canvas asks that a name you have a chat for resolve at most once a day, or when past its expiry, and that any other name resolve on every tap. Core applies this under the default mode, from state it stores beside the verification flags (§9):

| caller state | mode | result |
|---|---|---|
| known chat, resolved within a day and not past expiry | `PRMUnknown` | `CAPKnown` from the store, no network — 3a |
| known chat, stale or past expiry | `PRMUnknown` | registry + link resolve + compare — 3b, 3c, 3d |
| no known chat | `PRMUnknown` | full resolution — band 2 |
| forced refresh (directory service) | `PRMAll` | always resolves |
| per keystroke in search | `PRMNever` | local hit, or `CENotResolvedLocally` swallowed |

Link targets keep today's `PRMUnknown` meaning: a known chat is answered from the store.

`CENotResolvedLocally` continues to be returned for a `PRMNever` miss and continues to be swallowed by both apps.

---

## 6. What stays an error

`SDEUnknownDomain` stays, for 2g and 4b. It is a mismatch between a resolved link's profile and the name asked for, not a property of the registry answer, so it cannot become a `NameRegistration`. Per the review thread it stays nullary — which name was claimed is not carried, because it is not shown.

`SDENoValidLink` stays. It becomes unreachable from the plan path, since 2f is now `CPNameNotConnectable` and its band-3 twin is `CAPKnown`, but `resolveNameLink`, `verifyEntityDomain` (`:5094`) and `/_set domain` (`:1607`) still raise it.

Registry and network failures stay `CPError` (2h). The canvas shows the resolver's own text, which `chatErrorAgent` already carries.

---

## 7. Decisions taken

1. **The term is dropped from lookup wording, not added to the API.** Adding a term to `NamePricing` means a simplexmq change and a re-pin for one string; hardcoding "2 years" in three clients means three silent lies the day the term moves. Per-year pricing is what the registry actually returns.
2. **3c is a `Bool` on the `Ok` plans, not a revived `ownerChanged`.** It carries no owner and needs nothing persisted, so it does not reopen `f3bcd4a16`, and it is all the canvas claims.
3. **Dateless alerts rather than suppressed ones.** A user on an old router still learns the name expired; only the two dates go.
4. **Registration does not go through `ConnectionPlan`.** On the sibling canvas, 5a and 5b are the only registration states that would need one — 5a is `CPContactAddress (CAPKnown ct) (Just NRRegistered)`, 5b is `CPContactAddress (CAPOk …) (Just NRRegistered)` — and both are proposed for dropping (`b898b991d`, "suggest to drop 5a & 5b"). With them gone the registration check is a plain name-status call, this API keeps one consumer, and the two surfaces stop competing. Nothing here becomes removable as a result: every field 5a and 5b would have used is independently required by 3b and 3d.
5. **Consequently the lookup canvas's footer line "registration will always resolve" is obsolete** and should come off the sketch with this change.
6. **Reversed on review (2026-09-24): the freshness rule is in core, not the UIs.** Kept in the apps it was duplicated, lost on export, and keyed by domain name alone, so it was shared across user profiles and, under remote access, across hosts. §9.
7. **The constructor is `CPNameNotConnectable`, renamed from `CPSimplexName`.** All five states it carries share one invariant — you cannot connect — and the attached `NameRegistration` says why. Rejected, with reasons, so they are not re-litigated: *`CPUnregisteredSimplexName`* is false for 2b and 2f, which are registered; *`CPNonResolvingName`* is false for 2b, where both `resolveNameRecord` and `resolveNameLink` succeed and the refusal is policy, and it collides with `CENotResolvedLocally` and with 2h, the cases that genuinely do not resolve; *`CPSimplexName`* reads as a sibling of `CPContactAddress` / `CPGroupLink` naming the target kind, but a name that resolves produces those instead. `CPSimplexDomain`, asked for in review on `ea721d33f`, is vague rather than wrong and remains the fallback if the thread is reopened. The ordering follows the file's dominant negation pattern — `<Noun>Not<Predicate>`, 20 constructors including the close sibling `CESimplexDomainNotReady`; a `Non` prefix appears nowhere in `src/`.

---

## 8. Compile fixes and regeneration

- `View.hs:2234`, `:2252` — match the new arities; `viewConnectionPlan` (`:2214`) takes `Maybe ACreatedConnLink` and gains a `CPNameNotConnectable` case rendering domain, kind, and expiry or price under `testView`.
- `View.hs:217` — pass the now-optional `connLink` through.
- `Commands.hs` — the 26 `CPContactAddress` / `CPGroupLink` occurrences take the second argument; `:2178` and `:4586` build `CRConnectionPlan` with `Maybe`.
- `CAPOk` / `GLPOk` construction sites take `addressChanged`.
- Regenerate the client types: `bots/api/TYPES.md:1903-1922`, `packages/simplex-chat-client/types/typescript/src/types.ts`, `packages/simplex-chat-python/src/simplex_chat/types/_types.py` all still describe the three-constructor plan. Note that `bots/src/API/Docs/Commands.hs:142` and both generated clients never emit `resolve=`, which is fine and stays.

**Encoding note for the app work that follows.** `ConnectionPlan` derives through `sumTypeJSON`, which is `_owsf`-tagged on iOS and `type`-tagged elsewhere, but `NameRegistration` derives through `taggedObjectJSON` unconditionally — deliberately, since it is the RNAME payload. So one iOS response mixes both forms: `{"_owsf":"contactAddress","contactAddress":{…,"nameRegistration_":{"type":"registered",…}}}`. The Swift decoder for `NameRegistration` is therefore hand-written for the `type` form, as `MsgChatLink`'s is (#6821) — a protocol type in the same position — and follows it: `private enum CodingKeys`, `forKey: .type`, a `container` binding, and an `"Unknown … type"` error. simplexmq cannot switch to `sumTypeJSON`: that is platform-conditional in the core too, so an iOS build would expect `_owsf` while the relay forwards the resolver's `type` form. Its `reservedReason` is a bare string that may hold anything up to 32 characters (`NRRUnknown`), so both clients need a default branch.

---

## 9. Re-resolution in core

Review on 2026-09-24 reversed decision 6. Each decision below was taken by the author, not the implementer.

1. **Storage.** `contact_profiles.contact_domain_resolved_at` and `contact_domain_expires_at`; `groups.group_domain_resolved_at` and `group_domain_expires_at` — beside `contact_domain_verified` and `group_domain_verified`, which record whether the name checked out; these record when it was last resolved and when its registration expires. `TEXT` in SQLite, `TIMESTAMPTZ` in Postgres, `_at` as in `badge_purchases.expires_at`. One migration per backend.
2. **Rule.** Under `PRMUnknown`, a known chat reached by a name is re-resolved when `resolved_at` is `NULL` or over a day old, or `expires_at` has passed. `PRMAll` always resolves; `PRMNever` never does. A name with no chat is resolved on every call and nothing is stored; the user's own address is not a chat, so it too is resolved on every call.
3. **Writes.** Wherever core sets a verification flag after a resolution, it also sets `resolved_at` to now and `expires_at` to the registration's expiry, or `NULL` when the caller does not have it — then only the one-day limit applies until the next resolution. That is `setContactDomainVerified`, `setGroupDomainVerified`, and `createPreparedContact`, which inserts a chat created by name already verified. To supply the expiry, `resolveNameRecord` returns it beside the record, for `/_verify domain` (both kinds) and `APISetPublicGroupAccess` to pass on, and `updateGroupFromLinkData` takes it from its callers, since it must not resolve. One path gains a write: a re-resolution confirming the name still resolves to the known chat, which writes nothing today. It re-sets the flag to `True`, a no-op, since only verified chats are found by name.
4. **Moved name.** When re-resolution finds the name resolves elsewhere (3c), nothing is written to the old chat. It stays stale, so each default lookup re-resolves and reports the new address; `resolve=never` still returns the old chat.
5. **Reading.** A store function reads the two columns for the one chat being planned. `LocalProfile` and `GroupInfo` do not change, so the columns never reach the UIs; loading them there would touch 19 queries in 6 store files and both types' JSON.
6. **UIs.** Both apps lose the cache — the preference, `SimplexNameResolved`, the local probe before resolving, and the invalidation in `UserAddressView` — and plan a name with the default mode.
7. **iOS decoding.** Unchanged in substance, per §8.

**Tests**, in core. A name re-pointed with `registerName` shows whether core queried the registry; a stored time is backdated with `withCCTransaction … DB.execute "UPDATE …"`, as `tests/ChatTests/Groups.hs:8825` does:
- a fresh known chat is answered from the store: after re-pointing, the default plan still returns the old contact;
- with `resolved_at` backdated, the default plan re-resolves and reports the new address;
- with `expires_at` in the past, likewise;
- a name with no chat resolves on every call and stores nothing;
- a moved name stays stale: two default lookups both report the new address;
- `resolve=all` and `resolve=never` are unchanged, and existing tests using them stay as they are.

---

## Order of work

**A. Types.** The changes in §2, plus the `connectionPlanProceed` and derivation updates. Done when the constructors, fields and `deriveJSON` calls are in place and `Controller.hs` has no remaining arity error.

**B. Producer.** `resolveNameRegistration`, the rewritten `CTDomain` branch, the `knownLinkPlans` fall-through, expiry gating, and `addressChanged` under `PRMAll`. Done when every row of §4 can be produced.

**C. Consumers.** `View.hs`, the `Commands.hs` construction sites, the response builders. Done when `cabal build` is clean.

**D. Tests.** The harness comes first: `tests/NameResolver.hs:49-50` can only answer `NRRegistered` with `expires = Nothing`, or `NRAvailable` at a fixed price. Give `registerName` a way to set `expires`, `graceUntil` and `reservedReason_`, and add a way to register a name as `NRReserved` — without it, nine of the sixteen rows cannot be reached at all. Then extend `tests/ChatTests/Names.hs` (which already drives `/_connect plan` at `:236`, `:261`, `:282`, `:292`): one case per §4 row, plus a `PRMNever` hit and miss, plus `addressChanged` both ways. Done when all sixteen rows are asserted.

**E. Regeneration.** §8. Done when the generated types describe five constructors.

**F. Re-resolution in core.** §9: the migration and store functions, the rule and its writes, the core tests, then removing the cache from both apps and aligning the Swift decoder with `MsgChatLink` (§8). Done when the §9 tests and the full names suite pass and neither app keeps a name cache.

---

## Done means

- `tests/NameResolver.hs` can answer expired, reserved and available, not only registered-without-dates
- every row of §4 is produced by core and asserted by a test
- `CPNameNotConnectable` carries a domain and is returned only when no local chat claims the name
- `nameRegistration_` is `Just` for every name target the registry was asked about, and `Nothing` for every link target and every known chat answered from the store
- `PRMAll` re-resolves a known chat, `PRMAllGroups` is removed, `PRMNever` is unchanged, and `PRMUnknown` applies the rule in §9
- an expired name never yields a connectable plan, and an absent `expires` is treated as live
- `resolve=all` on a known chat whose name moved returns an `Ok` plan with `addressChanged = True`
- `cabal build` and `cabal test` are clean, and the generated client types match
- one migration per backend, no new chat command, and no name cache left in either app
