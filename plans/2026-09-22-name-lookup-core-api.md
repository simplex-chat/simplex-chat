# SimpleX name lookup — the core API the alert canvas needs

Defines the core half of #7525 (`ab/names-lookup-api`). Model: `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`, states 1a–4d. Sibling canvas `plans/sketches/2026-09-18-names-phase3a-registration.excalidraw` on `ab/names-actions-api` (#7530), §5. Registry types from simplexmq `ea43df2349f6d3dedd60d5e4aed21fd99316cad9`, `src/Simplex/Messaging/Names/Record.hs`.

No UI work here. This is the contract the Kotlin and Swift apps code against, and nothing in it depends on the wallet (#7475) or on either name-actions API.

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
9. Order of work
10. Done means

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

**Scope.** Core only: types, producer, CLI rendering, generated client types, tests. No Kotlin, no Swift, no migration, no new chat command.

**Not in scope, deliberately.** Registration, renewal and pricing actions — those are #7530, which consumes this API for its own name search rather than duplicating it (§7). No name-resolution cache or TTL in core: the once-a-day rule is expressible with the modes that already exist (§5).

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

**`PRMAll` gets its meaning.** It is the contact-side counterpart of `PRMAllGroups`: re-resolve a name whose chat is already known, instead of short-circuiting in `knownLinkPlans`. `PRMAllGroups` covers only groups (`Commands.hs:4509`); contacts have had no escape at all.

Nothing is removed. `SimplexDomainError` keeps both constructors (§6).

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

**`addressChanged`.** Under `PRMAll`, when the target is a `CTName` and `knownLinkPlans` returns a known contact or group, resolve the link anyway and compare it with the stored one. Equal, or `PRMUnknown`: today's `CAPKnown` / `GLPKnown`, which is 3a. Different: return the `Ok` plan **for the new link**, with `addressChanged = True` — the canvas draws 3c as a profile card for the new address with *Open new chat*, not as a known chat. Every other construction site passes `False`.

**Attach on every name target.** `nameRegistration_` is `Just` whenever the connect target was a name, `Nothing` for a link target, regardless of state — so 2a and 3a carry it too and the UI simply ignores it. One rule beats nine.

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
| 3a | any | known chat | `CPContactAddress (CAPKnown ct) (Just nr)` / `CPGroupLink (GLPKnown …) (Just nr)` | `Just` stored |
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

The canvas asks that a name you have a chat for resolve at most once a day, or when past its expiry, and that any other name resolve on every tap. No new mode and no core state is needed, because a bare name is already resolved on every call except `PRMNever` (`Commands.hs:4415-4418`), and `PRMNever` already means *use my chat if I have one, otherwise tell me nothing*:

| caller state | mode | result |
|---|---|---|
| known chat, answer fresh | `PRMNever` | `CAPKnown` from the store, no network — 3a |
| known chat, stale or past expiry | `PRMAll` | registry + link resolve + compare — 3b, 3c, 3d |
| no known chat | `PRMAll` | full resolution — band 2 |
| per keystroke in search | `PRMNever` | local hit, or `CENotResolvedLocally` swallowed |

Core therefore stays stateless and needs no change for the rule at all. **Where the client keeps the freshness timestamp is left to the UI plan** — it is not a core decision, and the modes above serve it either way. One coupling comes with that choice: #7530's `CEvtNameChanged {user, nameState}` fires when a registration or renewal moves on, and `NameState` carries the `domain`, so the client must drop that domain's timestamp on the event. Without it a user registers a name and the lookup keeps calling it available for up to a day, and 4c's *Renew* alert returns after a successful renewal. `PRMUnknown` keeps today's meaning and stays the default for link targets and for every other caller; nothing existing changes behaviour.

`CENotResolvedLocally` continues to be returned for a `PRMNever` miss and continues to be swallowed by both apps.

---

## 6. What stays an error

`SDEUnknownDomain` stays, for 2g and 4b. It is a mismatch between a resolved link's profile and the name asked for, not a property of the registry answer, so it cannot become a `NameRegistration`. Per the review thread it stays nullary — which name was claimed is not carried, because it is not shown.

`SDENoValidLink` stays. It becomes unreachable from the plan path, since 2f is now `CPNameNotConnectable` and its band-3 twin is `CAPKnown`, but `resolveNameLink`, `verifyEntityDomain` (`:5094`) and `/_set domain` (`:1607`) still raise it.

Registry and network failures stay `CPError` (2h). The canvas shows the resolver's own text, which `chatErrorAgent` already carries.

---

## 7. Decisions taken

1. **The term is dropped from lookup wording, not added to the API.** Adding a term to `NamePricing` means a simplexmq change and a re-pin for one string; hardcoding "2 years" in three clients means three silent lies the day the term moves. Per-year pricing is what the registry actually returns. The registration canvas confirms the term is not the registry's to give: its 5e (desktop and F-Droid) shows "YOU PAY — A code, 2 years" with **no amount**, while 5 shows "$20, 2 years" on a store build. The amount is the store product's and the term comes with it, or with `NameCredit.years` on the code path. Registration 1d draws lookup 2c as "$20 for 2 years" and takes that rewording with it.
2. **3c is a `Bool` on the `Ok` plans, not a revived `ownerChanged`.** It carries no owner and needs nothing persisted, so it does not reopen `f3bcd4a16`, and it is all the canvas claims.
3. **Dateless alerts rather than suppressed ones.** A user on an old router still learns the name expired; only the two dates go.
4. **Registration's search *is* this API.** #7530 has nine name commands and not one of them asks the registry about a name, so the search on registration §3 has no other source. It needs free / taken / reserved, and on a free name the pricing — exactly `CPNameNotConnectable` with `NRAvailable {pricing}`, `NRReserved`, or a `CAPOk` / `CAPKnown` plan when the name is taken. Registration 3b renders that answer inline instead of as the former 5a–5c alerts: a taken name shows its address, channel or both, and a tap opens the app's standard alert from the plan already in hand — lookup row 3a for `CAPKnown ct`, row 2a otherwise — so no second lookup is needed. Nothing here becomes removable, since every field 3b uses is independently required by lookup 3b and 3d.
5. **The lookup canvas's footer line "registration will always resolve" is therefore accurate**, not obsolete, and stays on the sketch. It is the sentence that records the coupling in 4.
6. **No cache in core.** §5 shows the rule is expressible with existing modes; a TTL column would be a migration bought for nothing.
7. **One registry answer, one shape.** `ea721d33f` ("embed status. reduce") dropped the `NameResponse` import and replaced `NameStatus {response :: NameResponse, …}` with a bare `NameRegistration`, answering the review's "it would complicate things a lot". #7530 had re-added the import for `NameState {registration :: Maybe NameResponse}`, so the same registry answer reached the clients through two shapes for the sake of `lastBlockTs`, which nothing in chat-core reads. Reduced there to `Maybe NameRegistration`; `BSPName` keeps `NameResponse`, which is right — that is the service wire, not the app API.
8. **The constructor is `CPNameNotConnectable`, renamed from `CPSimplexName`.** All five states it carries share one invariant — you cannot connect — and the attached `NameRegistration` says why. Rejected, with reasons, so they are not re-litigated: *`CPUnregisteredSimplexName`* is false for 2b and 2f, which are registered; *`CPNonResolvingName`* is false for 2b, where both `resolveNameRecord` and `resolveNameLink` succeed and the refusal is policy, and it collides with `CENotResolvedLocally` and with 2h, the cases that genuinely do not resolve; *`CPSimplexName`* reads as a sibling of `CPContactAddress` / `CPGroupLink` naming the target kind, but a name that resolves produces those instead. `CPSimplexDomain`, asked for in review on `ea721d33f`, is vague rather than wrong and remains the fallback if the thread is reopened. The ordering follows the file's dominant negation pattern — `<Noun>Not<Predicate>`, 20 constructors including the close sibling `CESimplexDomainNotReady`; a `Non` prefix appears nowhere in `src/`.

---

## 8. Compile fixes and regeneration

- `View.hs:2234`, `:2252` — match the new arities; `viewConnectionPlan` (`:2214`) takes `Maybe ACreatedConnLink` and gains a `CPNameNotConnectable` case rendering domain, kind, and expiry or price under `testView`.
- `View.hs:217` — pass the now-optional `connLink` through.
- `Commands.hs` — the 26 `CPContactAddress` / `CPGroupLink` occurrences take the second argument; `:2178` and `:4586` build `CRConnectionPlan` with `Maybe`.
- `CAPOk` / `GLPOk` construction sites take `addressChanged`.
- Regenerate the client types: `bots/api/TYPES.md:1903-1922`, `packages/simplex-chat-client/types/typescript/src/types.ts`, `packages/simplex-chat-python/src/simplex_chat/types/_types.py` all still describe the three-constructor plan. Note that `bots/src/API/Docs/Commands.hs:142` and both generated clients never emit `resolve=`, which is fine and stays.

**Encoding note for the app work that follows.** `ConnectionPlan` derives through `sumTypeJSON`, which is `_owsf`-tagged on iOS and `type`-tagged elsewhere, but `NameRegistration` derives through `taggedObjectJSON` unconditionally — deliberately, since it is the RNAME payload. So one iOS response mixes both forms: `{"_owsf":"contactAddress","contactAddress":{…,"nameRegistration_":{"type":"registered",…}}}`. The Swift decoder for `NameRegistration` must be written for the `type` form. Its `reservedReason` is a bare string that may hold anything up to 32 characters (`NRRUnknown`), so both clients need a default branch.

---

## Order of work

**A. Types.** The changes in §2, plus the `connectionPlanProceed` and derivation updates. Done when the constructors, fields and `deriveJSON` calls are in place and `Controller.hs` has no remaining arity error.

**B. Producer.** `resolveNameRegistration`, the rewritten `CTDomain` branch, the `knownLinkPlans` fall-through, expiry gating, and `addressChanged` under `PRMAll`. Done when every row of §4 can be produced.

**C. Consumers.** `View.hs`, the `Commands.hs` construction sites, the response builders. Done when `cabal build` is clean.

**D. Tests.** The harness comes first: `tests/NameResolver.hs:49-50` can only answer `NRRegistered` with `expires = Nothing`, or `NRAvailable` at a fixed price. Give `registerName` a way to set `expires`, `graceUntil` and `reservedReason_`, and add a way to register a name as `NRReserved` — without it, nine of the sixteen rows cannot be reached at all. Then extend `tests/ChatTests/Names.hs` (which already drives `/_connect plan` at `:236`, `:261`, `:282`, `:292`): one case per §4 row, plus a `PRMNever` hit and miss, plus `addressChanged` both ways. Done when all sixteen rows are asserted.

**E. Regeneration.** §8. Done when the generated types describe five constructors.

---

## Done means

- `tests/NameResolver.hs` can answer expired, reserved and available, not only registered-without-dates
- every row of §4 is produced by core and asserted by a test
- `CPNameNotConnectable` carries a domain and is returned only when no local chat claims the name
- `nameRegistration_` is `Just` for every name target and `Nothing` for every link target
- `PRMAll` re-resolves a known contact, `PRMAllGroups` is unchanged, `PRMNever` is unchanged
- an expired name never yields a connectable plan, and an absent `expires` is treated as live
- `resolve=all` on a known chat whose name moved returns an `Ok` plan with `addressChanged = True`
- `cabal build` and `cabal test` are clean, and the generated client types match
- no migration, no new chat command, no UI file touched
