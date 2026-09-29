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
| CPNameNotConnectable {simplexDomain :: SimplexDomain, nameWarning :: NameWarning}
```

`planSimplexName` cannot serve here. It is a `SimplexNameInfo`, which needs a `nameType`, and an unregistered name has none — today's code invents one by trying `NTPublicGroup` then `NTContact` (`Commands.hs:4432-4434`), which is arbitrary and becomes visible the moment the UI renders it. The canvas writes every band-2 body as a bare name (`sunflower.simplex is available…`, `bakery.simplex expired on…`), never `@`/`#`, so the UI wants `fullDomainName`, not `shortStr`.

**`CAPOk` and `GLPOk` gain `addressChanged :: Bool`.**

```haskell
| CAPOk {contactSLinkData_ :: Maybe ContactShortLinkData, ownerVerification :: Maybe OwnerVerification, addressChanged :: Bool}
| GLPOk {groupSLinkInfo_ :: Maybe GroupShortLinkInfo, groupSLinkData_ :: Maybe GroupShortLinkData, ownerVerification :: Maybe OwnerVerification, addressChanged :: Bool}
```

True when the name resolved to a link that differs from the one held by the local chat that claims this name. This is state 3c and nothing else expresses it: both "a link you do not have" and "a link that replaced yours" are `CAPOk` today. It states that the address moved and not who moved it, which is exactly what the canvas claims ("Only the address change is known, not who made it") and is the narrow form of the `ownerChanged` field dropped in `f3bcd4a16` — no owner identity is carried, stored or compared.

**`connectPlan` returns an optional link and takes the registration already resolved.**

```haskell
connectPlan :: User -> AConnectTarget -> PlanResolveMode -> Maybe LinkOwnerSig
            -> Maybe NameRegistration
            -> CM (Maybe ACreatedConnLink, Maybe SimplexNameInfo, Maybe SimplexNameInfo, ConnectionPlan)
```

The fifth parameter is the registration the bare name path resolved, so the path of the kind it plans does not resolve the name again. `CPContactAddress` and `CPGroupLink` have `nameWarning_ :: Maybe NameWarning` rather than a registration: see `plans/2026-09-28-name-warnings.md` §5.

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

**The plan logic** is described in `plans/2026-09-28-name-warnings.md` §6. For each kind, the path compares the local chat with the name's link. The bare name path looks up both kinds locally, resolves the name once, and plans one kind. When nothing is local and the name has no link of the kind, the answer is `CPNameNotConnectable d warning` with `connLink = Nothing`, which is why `connLink` had to become optional. The local lookups (`getUserContactLinkViaTarget`, `getContactToConnect`, `getGroupToConnect`) match on `cp.contact_domain` / `gp.group_domain` with `*_verified = 1`.

**Expiry is a producer rule, not a UI rule.** An expired name must not yield a connectable plan — "It does not connect while only its owner can renew it". `expires` absent (a v20/v21 router sent the record alone) means expiry is unknown, so the name is treated as live — the only safe reading. The dateless fallback in §4 covers the other case: `expires` known and past, `graceUntil` absent.

**`addressChanged`.** Under `PRMAll`, or under `PRMUnknown` when the stored resolution is stale (§9), when the target is a `CTName` and the local lookup returns a known contact, a group, or the user's own address or channel, resolve the link anyway and compare it with the stored one. Equal, or fresh under `PRMUnknown`: today's `CAPKnown` / `GLPKnown`, which is 3a. Different: return the `Ok` plan **for the new link**, with `addressChanged = True` — the canvas draws 3c as a profile card for the new address with *Open new chat*, not as a known chat. Every other construction site passes `False`.

**Warnings, not registrations.** `nameWarning_` is `Just` exactly when the canvas shows an alert. The registration stays in core (`plans/2026-09-28-name-warnings.md` §5).

---

## 4. The state → plan contract

The contract is the scenario table in `plans/2026-09-28-name-warnings.md` §3. It lists every combination of target (typed or bare name), local chat, and registry answer, with the plan and warning for each. Core now makes the four readings this section used to leave to the UI:
- the label length check (2c vs 2e);
- own vs chat, as separate warnings;
- the dateless variants (`graceUntil` absent);
- the price, as the 2-year term.

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

`SDEUnknownDomain` stays, for 2g and 4b. It is a mismatch between a resolved link's profile and the name asked for, not a property of the registry answer, so it cannot become a `NameWarning`. Per the review thread it stays nullary — which name was claimed is not carried, because it is not shown.

`SDENoValidLink` stays for `verifyEntityDomain` and `/_set domain`. The plan path no longer raises it: 2f is `CPNameNotConnectable` with `NWNoValidLink`, and a chat or own name is answered without a warning.

Registry and network failures stay `CPError` (2h). The canvas shows the resolver's own text, which `chatErrorAgent` already carries.

---

## 7. Decisions taken

1. **Reversed on 2026-09-28: the price is the 2-year term, computed in core.** `NamePrice {amount, years}` is the registry's per-year price for the label's length times `years = 2`, so the term is in one place and no client hardcodes it (`plans/2026-09-28-name-warnings.md`, N3).
2. **3c is a `Bool` on the `Ok` plans, not a revived `ownerChanged`.** It carries no owner and needs nothing persisted, so it does not reopen `f3bcd4a16`, and it is all the canvas claims.
3. **Dateless alerts rather than suppressed ones.** A user on an old router still learns the name expired; only the two dates go.
4. **Registration does not go through `ConnectionPlan`.** On the sibling canvas, 5a and 5b are the only registration states that would need one — 5a is `CPContactAddress (CAPKnown ct) (Just NRRegistered)`, 5b is `CPContactAddress (CAPOk …) (Just NRRegistered)` — and both are proposed for dropping (`b898b991d`, "suggest to drop 5a & 5b"). With them gone the registration check is a plain name-status call, this API keeps one consumer, and the two surfaces stop competing. Nothing here becomes removable as a result: every field 5a and 5b would have used is independently required by 3b and 3d.
5. **Consequently the lookup canvas's footer line "registration will always resolve" is obsolete** and should come off the sketch with this change.
6. **Reversed on review (2026-09-24): the freshness rule is in core, not the UIs.** Kept in the apps it was duplicated, lost on export, and keyed by domain name alone, so it was shared across user profiles and, under remote access, across hosts. §9.
7. **The constructor is `CPNameNotConnectable`, renamed from `CPSimplexName`.** All five states it carries share one invariant — you cannot connect — and the attached `NameWarning` says why. Rejected, with reasons, so they are not re-litigated: *`CPUnregisteredSimplexName`* is false for 2b and 2f, which are registered; *`CPNonResolvingName`* is false for 2b, where both `resolveNameRecord` and `resolveNameLink` succeed and the refusal is policy, and it collides with `CENotResolvedLocally` and with 2h, the cases that genuinely do not resolve; *`CPSimplexName`* reads as a sibling of `CPContactAddress` / `CPGroupLink` naming the target kind, but a name that resolves produces those instead. `CPSimplexDomain`, asked for in review on `ea721d33f`, is vague rather than wrong and remains the fallback if the thread is reopened. The ordering follows the file's dominant negation pattern — `<Noun>Not<Predicate>`, 20 constructors including the close sibling `CESimplexDomainNotReady`; a `Non` prefix appears nowhere in `src/`.

---

## 8. Compile fixes and regeneration

- `View.hs:2234`, `:2252` — match the new arities; `viewConnectionPlan` (`:2214`) takes `Maybe ACreatedConnLink` and gains a `CPNameNotConnectable` case rendering domain, kind, and expiry or price under `testView`.
- `View.hs:217` — pass the now-optional `connLink` through.
- `Commands.hs` — the 26 `CPContactAddress` / `CPGroupLink` occurrences take the second argument; `:2178` and `:4586` build `CRConnectionPlan` with `Maybe`.
- `CAPOk` / `GLPOk` construction sites take `addressChanged`.
- Regenerate the client types: `bots/api/TYPES.md:1903-1922`, `packages/simplex-chat-client/types/typescript/src/types.ts`, `packages/simplex-chat-python/src/simplex_chat/types/_types.py` all still describe the three-constructor plan. Note that `bots/src/API/Docs/Commands.hs:142` and both generated clients never emit `resolve=`, which is fine and stays.

**Encoding note.** Superseded on 2026-09-28: the plan has `NameWarning`, which is chat's own type and derives through `sumTypeJSON`, so the Swift decoder is synthesized like its neighbours and the hand-written `NameRegistration` decoder is gone.

---

## 9. Re-resolution in core

Review on 2026-09-24 reversed decision 6. Each decision below was taken by the author, not the implementer.

1. **Storage.** `contact_profiles.contact_domain_resolved_at` and `contact_domain_expires_at`; `groups.group_domain_resolved_at` and `group_domain_expires_at` — beside `contact_domain_verified` and `group_domain_verified`, which record whether the name checked out; these record when it was last resolved and when its registration expires. `TEXT` in SQLite, `TIMESTAMPTZ` in Postgres, `_at` as in `badge_purchases.expires_at`. One migration per backend.
2. **Rule.** Under `PRMUnknown`, a known chat reached by a name is re-resolved when `resolved_at` is `NULL` or over a day old, or `expires_at` has passed. `PRMAll` always resolves; `PRMNever` never does. A name with no chat is resolved on every call and nothing is stored; the user's own address is not a chat, so it too is resolved on every call.
3. **Writes.** Wherever core sets a verification flag after a resolution, it also sets `resolved_at` to now and `expires_at` to the registration's expiry, or `NULL` when the caller does not have it — then only the one-day limit applies until the next resolution. That is `setContactDomainVerified`, `setGroupDomainVerified`, and `createPreparedContact`, which inserts a chat created by name already verified. To supply the expiry, `resolveNameRecord` returns it beside the record, for `/_verify domain` (both kinds) and `APISetPublicGroupAccess` to pass on, and `updateGroupFromLinkData` takes it from its callers, since it must not resolve. Two paths gain a write: a re-resolution confirming the name still resolves to the known chat, and one that answers the chat without a warning because the name no longer has a link of its kind or is not registered (`plans/2026-09-28-name-warnings.md`, N7 and N11). Both re-set the flag to `True`, a no-op, since only verified chats are found by name.
4. **Moved name.** When re-resolution finds the name resolves elsewhere (3c), nothing is written to the old chat. It stays stale, so each default lookup re-resolves and reports the new address; `resolve=never` still returns the old chat.
5. **Reading.** A store function reads the two columns for the one chat being planned. `LocalProfile` and `GroupInfo` do not change, so the columns never reach the UIs; loading them there would touch 19 queries in 6 store files and both types' JSON.
6. **UIs.** Both apps lose the cache — the preference, `SimplexNameResolved`, the local probe before resolving, and the invalidation in `UserAddressView` — and plan a name with the default mode.
7. **iOS decoding.** Superseded, per the note in §8.

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
- `nameWarning_` is `Just` exactly when the lookup canvas shows an alert (`plans/2026-09-28-name-warnings.md` §3)
- `PRMAll` re-resolves a known chat and replaces `PRMAllGroups` (`allGroups` still parses), `PRMNever` is unchanged, and `PRMUnknown` applies the rule in §9
- an expired name never yields a connectable plan, and an absent `expires` is treated as live
- `resolve=all` on a known chat whose name moved returns an `Ok` plan with `addressChanged = True`
- `cabal build` and `cabal test` are clean, and the generated client types match
- one migration per backend, no new chat command, and no name cache left in either app
