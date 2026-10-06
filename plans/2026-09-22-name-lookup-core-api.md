# SimpleX name lookup — the core API the alert canvas needs

Defines the core half of #7525 (`ab/names-lookup-api`). Model: `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`, states 1a–4d. Sibling canvas `plans/sketches/2026-09-18-names-phase3a-registration.excalidraw` on `ab/names-actions-api` (#7530), §5. Registry types from simplexmq `ea43df2349f6d3dedd60d5e4aed21fd99316cad9`, `src/Simplex/Messaging/Names/Record.hs`.

The app changes are in `plans/2026-09-28-name-warnings.md` §8. This is the contract the Kotlin and Swift apps code against, and nothing in it depends on the wallet (#7475) or on either name-actions API.

---

## Table of contents

1. What #7525 declares, and what it does not produce
2. Type delta
3. Producer
4. The state → plan contract
5. Resolve modes
6. What stays an error
7. Decisions taken
8. Compile fixes and regeneration
9. Resolution and verification in core
10. Order of work
11. Done means

---

## Executive summary

As of `db779eff4`, #7525 was a declaration-only sketch: it added `nameRegistration_`, `CPSimplexName` (renamed `CPNameNotConnectable` in `54bc83d80`) and `PRMAll` to `Controller.hs` and changed two arities, but no producer was written and no consumer was updated, so **the branch did not compile**. `resolveNameRecord` (`Commands.hs:5082-5088`) collapsed every non-`NRRegistered` answer into `NAME NOT_FOUND`, so expiry, price and reserved-reason — the substance of nine of the sixteen states — never left core.

The declared types were close to right. Twelve states mapped onto them as they stood, and of the four that did not, three are settled by wording or by `NameWarning` (§4, §7). The work was mostly in the producer:
- the registration is classified into a record or a `NameWarning`;
- for each kind, the local chat is looked up first, and its link is compared with the name's link;
- the warning is attached to the local plan, or returned as `CPNameNotConnectable` when nothing is local.

`CPContactAddress` and `CPGroupLink` have `nameChange :: Maybe NameChange` in place of the declared `nameRegistration_`: how the name changed since the local chat was found by it. `CPNameNotConnectable` has its domain and the registry's warning, so the UI can print the bare name the canvas shows.

The by-name store lookups take `CTName` and query by domain:
- `getContactToConnect` (`Direct.hs:803`);
- `getGroupToConnect` (`Groups.hs:1092`);
- `getUserContactLinkViaTarget` (`Profiles.hs:575`);
- `getGroupInfoViaUserTarget` (`Groups.hs:2844`).

They are queried before the name is resolved.

---

## 1. What #7525 declares, and what it does not produce

This section records the sketch as of `db779eff4`, under today's constructor name: `CPNameNotConnectable` was then `CPSimplexName`. The diff against `fea9f482c` is eleven added lines in one file. It gives `PlanResolveMode` a `PRMAll` (`Controller.hs:714`, parser `:725`), makes `CRConnectionPlan.connLink` a `Maybe` (`:887`), hangs `nameRegistration_ :: Maybe NameRegistration` off `CPContactAddress` and `CPGroupLink`, adds `CPNameNotConnectable`, and updates `connectionPlanProceed` (`:1212-1233`).

None of it is reachable:

- **`CPNameNotConnectable` is never constructed.** `grep -rn CPNameNotConnectable src/` matches only the declaration, the `connectionPlanProceed` case and a comment.
- **`nameRegistration_` is never populated.** All 26 `CPContactAddress` / `CPGroupLink` occurrences in `Commands.hs` — constructions and patterns alike — still use the old arity.
- **`PRMAll` is parsed and never read.** `Commands.hs` tests only `== PRMNever` and `== PRMAllGroups`, so `resolve=all` silently behaves as `unknown`.
- **`connLink :: Maybe` has no producer** — `Commands.hs:2178` and `:4586` still pass a bare `ACreatedConnLink`.
- **It does not compile.** `View.hs:2234` and `:2252` pattern-match `CPContactAddress cap` / `CPGroupLink glp` at the old arity, `viewConnectionPlan` (`View.hs:2214`) takes a non-`Maybe` link and has no `CPNameNotConnectable` case, and `Commands.hs:2178`/`:4586` type-error on the response field.

**Scope.**
- Core: types, the producer, CLI rendering, generated client types and tests.
- Apps: the warning alert, 3c, the other kind's button and name search.
- Chat commands: `resolve=all` is added.

**Not in scope, deliberately.** Registration, renewal and pricing actions — those are #7530 and are reached from the UI, not from a connection plan (§7).

---

## 2. Type delta

The changes are to the types in `Controller.hs`, including `PlanResolveMode`, and to `connectPlan` in `Commands.hs`.

**`CPNameNotConnectable` has its domain.**

```haskell
| CPNameNotConnectable {simplexDomain :: SimplexDomain, nameWarning :: NameWarning}
```

`planSimplexName` cannot serve here. It is a `SimplexNameInfo`, which needs a `nameType`, and an unregistered name has none — the code as of `db779eff4` invented one by trying `NTPublicGroup` then `NTContact`, which is arbitrary and becomes visible the moment the UI renders it. The canvas writes every band-2 body as a bare name (`sunflower.simplex is available…`, `bakery.simplex expired on…`), never `@`/`#`, so the UI uses `fullDomainName`, not `shortStr`.

**`CPContactAddress` and `CPGroupLink` gain `nameChange`.**

```haskell
| CPContactAddress {contactAddressPlan :: ContactAddressPlan, nameChange :: Maybe NameChange}
| CPGroupLink {groupLinkPlan :: GroupLinkPlan, nameChange :: Maybe NameChange}

data NameChange
  = NCLapsed {nameWarning :: NameWarning}
  | NCMoved {knownChat :: AChatInfo}
```

- `NCLapsed`: the local plan, with the registry's warning (`plans/2026-09-28-name-warnings.md` §5).
- `NCMoved`: the new link's plan; the name's link differs from the link of a local chat or own channel. This is state 3c. Only the move is expressed, as in the canvas's 3c ("bakery.simplex now leads to a new address"). It is the narrow form of the `nameOwnerChanged` field dropped in `f3bcd4a16`: owner identity is neither included, stored nor compared.
- `knownChat` is the contact, the business chat, the channel or the own channel.
- A name that moved from the own address has no change: the new link's plan is returned as for a name with nothing local.

**`connectPlan` returns an optional link, and takes the bare name's resolution.**

```haskell
connectPlan :: User -> AConnectTarget -> PlanResolveMode -> Maybe LinkOwnerSig
            -> Maybe (Either ChatError (Either NameWarning NameRecord))
            -> CM (Maybe ACreatedConnLink, Maybe SimplexNameInfo, Maybe SimplexNameInfo, ConnectionPlan)
```

The bare name's resolution is passed to each kind's plan, so the name is resolved once. `connLink` is `Nothing` only for `CPNameNotConnectable`.

**`PRMAll` replaces `PRMAllGroups`, with the same meaning.** A joined group known by link is refreshed from its link data. The directory service uses `PRMAll`. `all`, `allGroups` and `on` parse as `PRMAll`.

`SimplexDomainError` keeps both constructors (§6).

---

## 3. Producer

**Resolving a name.** The name is resolved by `resolveNameRecordOrWarning` (`Commands.hs:5147`), and the registration is classified by `nameRecordOrWarning` (`:5153`):

```haskell
resolveNameRecordOrWarning :: User -> NetworkRequestMode -> SimplexDomain -> CM (Either NameWarning NameRecord)
nameRecordOrWarning :: SystemSeconds -> SimplexDomain -> NameRegistration -> Either NameWarning NameRecord
```

`resolveNameRecord` (`:5140`) is unchanged for its five other callers:
- `APISetUserDomain`;
- `APIVerifyContactDomain`, through `verifyEntityDomain`;
- `APIVerifyGroupDomain`;
- `APISetPublicGroupAccess`;
- service requests by name.

**The plan logic** is described in `plans/2026-09-28-name-warnings.md` §6. When nothing is local and the name has a warning, the answer is `CPNameNotConnectable d warning` with `connLink = Nothing`. Lookup criteria:
- chats (`getContactToConnect`, `getGroupToConnect`): `cp.contact_domain` / `gp.group_domain`, with `*_verified = 1`;
- the own address (`getUserContactLinkViaTarget`): the user's profile claim;
- the own channel (`getGroupInfoViaUserTarget`): `gp.group_domain`.

**Expiry is a producer rule, not a UI rule.** An expired name must not yield a connectable plan — "It does not connect while only its owner can renew it". `expires` absent (a v20/v21 router sent the record alone) means expiry is unknown, so the name is treated as unexpired — the only safe reading. The other case, `expires` known and past with `graceUntil` absent, is the dateless variant in §4.

**`nameChange`.** The name is resolved before the local lookup, so a local plan is built with `NCLapsed` (`localChange`). When the target is a `CTName` and a chat, the own address or the own channel is found locally, the name's link is compared with the stored one by `sameShortLinkContact`, in `knownNamePlan`:
- equal: the local plan (3a, 4a);
- different: the plan for the new link, built with `NCMoved` (3c, N35 of `plans/2026-09-28-name-warnings.md`), or without a change for the own address;
- different, and the new link's profile does not claim the name, or its data cannot be fetched: the local plan (N12, N19).

The change is a parameter of the link plan functions:
- `contactLinkPlan`;
- `groupLinkPlan`;
- `contactRequestPlan`;
- `groupJoinRequestPlan`;
- `groupPlan`.

`Nothing` is passed by full links, `ConnectSimplex` and a name with nothing local.

**Warnings, not registrations.** `NCLapsed` or `CPNameNotConnectable` is returned exactly when the canvas shows a name alert. The registration stays in core (`plans/2026-09-28-name-warnings.md` §5).

---

## 4. The state → plan contract

The contract is the scenario table in `plans/2026-09-28-name-warnings.md` §3. It lists every combination of target (typed or bare name), local chat, and registry answer, with the plan and warning for each. Core now makes the four readings this section used to leave to the UI:
- the label length check (2c vs 2e);
- own vs other, read from the plan (`CAPOwnLink`, `GLPOwnLink`);
- the dateless variants (`graceUntil` absent);
- the price, as the 2-year term.

---

## 5. Resolve modes

| target | mode | answer |
|---|---|---|
| name | `PRMUnknown`, `PRMAll` | resolved on every lookup, and compared with the local chat (§3) |
| name | `PRMNever` | the local chat, or `CENotResolvedLocally` |
| contact or group short link, local | `PRMUnknown`, `PRMNever` | the local plan |
| contact short link, local | `PRMAll` | the local plan |
| group short link of a joined group | `PRMAll` | the group, refreshed from its link data |
| group short link of the own group | `PRMAll` | the local plan |
| contact or group short link, nothing local | `PRMUnknown`, `PRMAll` | the link's plan |
| contact or group short link, nothing local | `PRMNever` | `CENotResolvedLocally` |

The mode is ignored for invitation links. A bare name is resolved once for both kinds.

`PRMNever` is used by the apps' name search, `PRMUnknown` by a tap on "Connect to …", and `PRMAll` by the directory service.

`CENotResolvedLocally` is returned for a `PRMNever` miss, and both apps discard it.

---

## 6. What stays an error

`SDEUnknownDomain` stays, for 2g and 4b. It is a mismatch between a resolved link's profile and the name asked for, not a property of the registry answer, so it cannot become a `NameWarning`. Per the review thread it stays nullary — which name was claimed is not included, because it is not shown. It is also raised for a channel without relays, or one that needs an app update, when its profile does not claim the name.

`SDENoValidLink` stays for `/_set domain`, `APISetPublicGroupAccess` and service requests by name. In the plan path, it is raised for an unexpired name without a link of the kind when nothing is local (2f). With a chat or own name, the local plan is returned without a warning.

Registry and network failures stay command errors when nothing is local (2h), with the resolver's text from `chatErrorAgent`. With a chat or own name, the local plan is returned.

---

## 7. Decisions taken

1. **Reversed on 2026-09-28: the price is the 2-year term, computed in core.** `NamePrice {amount, years}` is the registry's per-year price for the label's length times `years = 2`, so the term is in one place and no client hardcodes it (`plans/2026-09-28-name-warnings.md`, N3).
2. **3c is `NCMoved`, not a revived `nameOwnerChanged`.** No owner is included and nothing is persisted, so `f3bcd4a16` is not reopened.
3. **Dateless alerts rather than suppressed ones.** A registration whose `expires` has passed, without `graceUntil`, is shown with the expiry alert; only the renew-by date is omitted (`plans/2026-09-28-name-warnings.md`, N24). An absent `expires` is treated as unexpired.
4. **Registration does not go through `ConnectionPlan`.** On the sibling canvas, 5a and 5b are the only registration states that would need one, and they cannot be expressed by `nameChange`: an unexpired name has no warning. Both are proposed for dropping (`b898b991d`, "suggest to drop 5a & 5b"). With them dropped, the registration check is a plain name-status call, and this API has one consumer.
5. **Consequently the lookup canvas's footer line "registration will always resolve" is obsolete** and came off the sketch with this change.
6. **Reversed on review (2026-09-24), then simplified: core resolves a name on every plan, and the apps keep no name state.** §9.
7. **The constructor is `CPNameNotConnectable`, renamed from `CPSimplexName`.** It covers four states (2b, 2c, 2d, 2e): a connection is impossible, and the reason is given by the attached `NameWarning`. Rejected names:
   - *`CPUnregisteredSimplexName`* is false for 2b, which is registered;
   - *`CPNonResolvingName`* is false for 2b, where the name is resolved and the refusal is policy, and it is ambiguous with `CENotResolvedLocally` and with 2h, where the name is not resolved;
   - *`CPSimplexName`* would be read as a sibling of `CPContactAddress` / `CPGroupLink` that names the target kind, while those are returned for a resolved name.

   `CPSimplexDomain`, asked for in review on `ea721d33f`, is vague rather than wrong, and is kept as the fallback if the thread is reopened. The name is formed by the file's dominant negation pattern, `<Noun>Not<Predicate>`: 20 constructors, including the close sibling `CESimplexDomainNotReady`. A `Non` prefix is used nowhere in `src/`.

---

## 8. Compile fixes and regeneration

- `View.hs:2234`, `:2252` — match the new arities; `viewConnectionPlan` (`:2214`) takes `Maybe ACreatedConnLink` and gains a `CPNameNotConnectable` case rendering the domain; the warning line is `viewNameWarning` (`plans/2026-09-28-name-warnings.md` §7).
- `View.hs:217` — pass the now-optional `connLink` through.
- `Commands.hs` — `nameChange` is added at the 26 `CPContactAddress` / `CPGroupLink` occurrences; `CRConnectionPlan` is built with `Maybe` at `:2178` and `:4586`.
- Regenerate the client types. Before regeneration, the four-constructor plan was described in `bots/api/TYPES.md:1903-1922`, `packages/simplex-chat-client/types/typescript/src/types.ts` and `packages/simplex-chat-python/src/simplex_chat/types/_types.py`. `resolve=` is omitted by `bots/src/API/Docs/Commands.hs:150` and both generated clients.
- Regeneration is pending. The removed `localChats`, `offerLookup` and `NWNoValidLink` are still described in `bots/api/COMMANDS.md`, `bots/api/TYPES.md` and both generated clients.

**Encoding note.** Superseded on 2026-09-28: the plan has `NameWarning`, which is chat's own type and derives through `sumTypeJSON`, so the Swift decoder is synthesized like its neighbours and the hand-written `NameRegistration` decoder is gone.

---

## 9. Resolution and verification in core

Decision 6 (§7) is implemented as follows.

1. **Resolution.** A name is resolved on every plan, except with `resolve=never`. A bare name is resolved once for both kinds. The registration is kept only within the plan.
2. **Moved name.** On every lookup, the local chat is compared with the name's link, so 3c is shown on the first lookup after the name moved. With `resolve=never`, the old chat is returned until a chat at the new link is verified.
3. **One verified chat per name and kind.** `unverifyOtherNameChats` (`Internal.hs:1669`) is called after every write that sets a chat verified:
   - `APIPrepareContact`, for a contact or a business chat;
   - `APIPrepareGroup`;
   - `APIVerifyContactDomain`;
   - `APIVerifyGroupDomain`;
   - `APISetPublicGroupAccess`;
   - `updateContactFromLinkData` and `updateGroupFromLinkData`, when the chat is verified from the link data.

   The flag is cleared on the user's other chats of the name's kind: contacts and business chats for a contact name, channels for a channel name. Core emits `CEvtContactUpdated` or `CEvtGroupUpdated` for each (N36 of `plans/2026-09-28-name-warnings.md`). After "Open new chat" in 3c, a lookup of the name returns the new chat.
4. **Prepared chats.** A prepared chat is set verified only when the link's profile claims the name (`APIPrepareContact`, `APIPrepareGroup`).
5. **UIs.** Both apps plan a name with the default mode, and keep no name state.
6. **iOS decoding.** Superseded, per the note in §8.

**Tests**, in core. A name is re-pointed with `registerName`, or registered as expired with `registerExpiredName`; a registry query is detected by the next plan's output:
- a known chat: a later expiry is reported, and cleared after a renewal (`testPlanKnownNameStale`);
- nothing local: the name is resolved on every lookup (`testPlanNameResolvedEveryCall`);
- the name moved: 3c, then the new chat after it is prepared (`testPlanKnownNameAddressChanged`, `testPlanKnownNameNewChatOpened`);
- the name moved to another known chat: that chat (`testPlanKnownNameMovedToKnownChat`);
- a chat prepared with a name its profile does not claim is left unverified (`testPrepareNameNotClaimed`);
- with `resolve=never`, the answer is taken from the store (`testPlanNameResolveNever`).

---

## Order of work

**A. Types.** The changes in §2, plus the `connectionPlanProceed` and derivation updates. Done when the constructors, fields and `deriveJSON` calls are in place and `Controller.hs` has no remaining arity error.

**B. Producer.** `resolveNameRecordOrWarning` and `nameRecordOrWarning`, the `CTDomain` branch, `knownNamePlan` after the local lookups (`knownLinkPlans`), expiry gating, and `NCMoved`. Done when every row of §4 can be produced.

**C. Consumers.** `View.hs`, the `Commands.hs` construction sites, the response builders. Done when `cabal build` is clean.

**D. Tests.** The harness comes first: `tests/NameResolver.hs:49-50` could only answer `NRRegistered` with `expires = Nothing`, or `NRAvailable` at a fixed price. Add `registerExpiredName` (expired a day ago, renewable for 30 days), `registerReservedName`, `unregisterName` and `failNameResolution`; the dateless and unexpired-community cases are unit tests of `nameRecordOrWarning` — without these, nine of the sixteen rows cannot be reached at all. Then extend `tests/ChatTests/Names.hs` (which already drives `/_connect plan` at `:236`, `:261`, `:282`, `:292`): one case per §4 row, plus a `PRMNever` hit and miss, plus `NCMoved` both ways. Done when all sixteen rows are asserted.

**E. Regeneration.** §8. Done when the generated types describe five constructors.

**F. Resolution and verification in core.** §9: `unverifyOtherNameChats` after each verifying write, and the core tests. Done when the §9 tests and the full names suite pass.

---

## Done means

- `tests/NameResolver.hs` can answer expired, reserved and available, not only registered-without-dates
- every row of §4 is produced by core and asserted by a test
- `CPNameNotConnectable` has a domain and is returned only when nothing of the planned kind is local for the name
- `NCLapsed` or `CPNameNotConnectable` is returned exactly when the lookup canvas shows a name alert (`plans/2026-09-28-name-warnings.md` §3)
- `PRMAll` replaces `PRMAllGroups` (`allGroups` and `on` still parse), a name is resolved on every lookup except with `PRMNever`, and `PRMNever` is unchanged
- an expired name never yields a connectable plan, and an absent `expires` is treated as unexpired
- the new link's plan with `NCMoved` is returned for a known chat whose name moved to a link that claims it
- `cabal build` and `cabal test` are clean, and the generated client types match
- core and the apps keep a name's registration only within one plan, and the chat commands are unchanged except `resolve=all`
