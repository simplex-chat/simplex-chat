# Badge ledger: one entry type

This branch introduced seven types to describe ledger rows. Three hold nothing the wire's own `StatementEntry` does not already carry. Two describe a plan the caller can hold directly. One pairs a balance with a row id used for a single equality test. One holds three timestamps that two consecutive entries already determine. `StatementEntry` becomes the single representation, and nothing replaces the rest.

| introduced | what it holds beyond `StatementEntry` |
| --- | --- |
| `LedgerBalance` | nothing — its four fields are `StatementEntry`'s |
| `ServiceLedgerEntry` | nothing — `statementEntry` exists only to rename `entryUuid`, flatten the balance, and fill in a `wasPausedSince` the service never sets |
| `LedgerRow` | nothing — a change, a balance and a type, all fields of `StatementEntry` |
| `LedgerPlan` | the rows to write, and which is the issuance |
| `SignedPlan` | the same, with the credential |
| `LedgerTip` | a numeric row id beside a balance |
| `BadgePeriod` | three timestamps, all derivable from two consecutive entries |

## The entry is the state

The ledger's central property is that the last row *is* the balance. `StatementEntry` already carries it — `balanceMonths`, `balanceStartTs`, `balanceAnchorTs`, `balanceBadgeType` — alongside the change that produced it and the type of operation that made it. So an operation is a function from the last entry to the next. The three keep the shapes they have today, with `LedgerBalance` replaced by `StatementEntry`, a uuid added, and the credit type moving in from `ledgerPlan` because a grant now writes its own entry type:

```haskell
lapseEntry :: UTCTime -> Text -> StatementEntry -> Maybe StatementEntry   -- was advanceBalance
issueEntry :: UTCTime -> Text -> StatementEntry -> Maybe StatementEntry   -- was issueMonth
grantEntry :: UTCTime -> Text -> Int -> StatementCreditType -> StatementEntry -> StatementEntry  -- was grantMonths
```

Each sets its own `changeMonths` and `entryType` — `SEDebit SDLapse`, `SEDebit SDBadge`, the caller's credit — so the count and the type it is recorded under travel together and cannot be mismatched. `Nothing` means the operation does not apply: nothing has elapsed, or no month is due. `grantEntry` is total, because a grant always applies.

The arithmetic inside them does not change. `entryId` and `createdAt` are supplied and never read, and `wasPausedSince` stays `Nothing` as the service sets it today. The price is that uuid generation moves from the store to the caller — which is also what lets an entry be identified before it is written, and be the value the credential is stored against.

The client is already here: `getBadgeLedgerLastEntry` returns a `StatementEntry`, because that is what the wire sends and what `badge_ledger` stores. This makes the service match rather than inventing a shape of its own.

## No plan, just composition

`ledgerPlan` goes too. A request lapses first, as its own entry, and the caller chains the rest, keeping whichever came back:

```haskell
let lapsed = lapseEntry now uuid1 tip
    current = fromMaybe tip lapsed
    issued = issueEntry now uuid2 current
    rows = catMaybes [lapsed, issued]
```

A redemption is the same chain with a grant between the two: `lapseEntry`, `grantEntry`, `issueEntry`. It grants the months and issues the first of them at once, which is what `ledgerPlan` does today.

Do not skip the lapse before a grant. `grantEntry` restarts the run only when the balance is empty, so elapsed months still on the books get added to instead: three months bought on 10 January and another code redeemed on 10 June gives five months running from January, every one already spent, where lapsing first gives two running from June.

The caller generates one uuid per entry it might write. These two are the only callers, so a function to compose them would serve two sites that differ by one step.

This disposes of `LedgerPlan` and `SignedPlan` without replacing them: the rows are a list the caller already holds, and the credential belongs to `issued`, a local in scope rather than something to find by position or by type.

Two store functions simplify with them. `getLedgerEntries` returns `[StatementEntry]`, so `credentialResponse`'s `map statementEntry` disappears. And `appendLedgerPlan` loses both its `TVar ChaChaDRG` and its `now`, since the entries now carry their own ids and timestamps — it gains only the issuance's predecessor, which the caller has as `current`.

## The period is derived

`BadgePeriod` carried what an issuance writes to `badge_issuances`. Given the issuance entry and the one before it, all three values are already there:

- `periodStart` is the previous entry's `balanceStartTs`
- `periodEnd` is the issuance entry's own `balanceStartTs`, because issuing moves the start to the period end
- the expiry is a pure function of `periodEnd`

The client already derives the two bounds this way in `getIssuedPeriod`, and takes the expiry from the credential, which is the service's own computation arriving back. On the service, compute the expiry at both points it is needed — signing, then writing the issuance row — from the same entry through the same function.

The predecessor is not always one of the written rows — when nothing lapsed, the entry before the issue is the tip. Composing at the call site names it anyway: it is `current`, the value the issue was computed from, so the writer has it without having to look for it.

## The tip is just an entry

`LedgerTip` is the newest row's id and balance. The id has one use — checking that no row was written while the plan was being signed — and that only asks whether it is still the same row, which `StatementEntry`'s uuid answers. So `getLedgerTip` returns a `StatementEntry`.

Reading a row as an entry means decoding its type, which `entryTypeFromColumns` does only partially — but totally over `code`, `badge` and `lapse`, which is everything the service writes. So `Nothing` still means no rows.

## What stays shared

The client authors no entries — it stores what the service sends and reads the last one back. So `lapseEntry`, `issueEntry` and `grantEntry` are the service's alone after this change, though `Ledger.hs` stays one module and the client simply imports less of it.

The rest of the module changes only in its argument: `paidThrough`, `elapsedMonths`, `monthsFromAnchor` and `monthAfter` take a `StatementEntry` where they took a `LedgerBalance`. `addMonths`, the tag functions and the column helpers are untouched; the week-boundary function is shifted a day and renamed by the worker plan, not here. `elapsedMonths` need no longer be exported, since `lapseEntry` is the only caller. The client keeps `paidThrough` and gains the check below.

The client's remaining use, `ledgerPlan` inside `badgeWorkDue`, is removed by the worker plan.

## Verifying what the service sends

The client authors nothing, so it takes the service's arithmetic on trust — while holding everything needed to check it.

**Where.** In `applyBadgeStatement`, as the rows are stored — the one place holding both the arriving entries and the stored tip, in one transaction. The tip is read only when `previousEntryId` is present: `redeemCode` always sends the whole ledger without one, and checking its opening row against a tip already held would mark a good row bad.

**What.** Re-run the operation the entry claims and compare, rather than restate its arithmetic as rules. Each operation is a total function of a predecessor and a timestamp, and the entry carries the timestamp it was computed with. The predecessor is the previous entry as received, the tip for the first, or `emptyEntry` when there is no tip — what `redeemCode` grants onto, so the opening is not a rule of its own.

Re-running is what catches over-lapsing: writing off three months when one elapsed adds up against its predecessor, and it empties `balanceMonths` while leaving `paidThrough` untouched, so the badge stops renewing while the ledger still reads as paid up. Two values cannot be re-derived and are bounded instead — a credit's months, of which only the sign is checkable, and `createdAt`, which every recompute is anchored on, held below the client's clock and at or after its predecessor's.

**What happens when it fails: store the row and mark it.** Not refuse. Perks do not depend on the ledger — the credential is signed independently and a receiver verifies that signature — so rejecting a statement would strand a badge the service considers paid while proving nothing. The ledger is the user's record of what was spent, and the useful response to arithmetic that does not add up is to keep it and be able to point at the line.

**The column.** `balance_checked`, per entry — `1` when re-running reproduced the row, `0` when something contradicted it, and null when this version has no operation to re-run: an unknown tag, or a debit declared but unimplemented. Marking those `0` would report a newer service's correct row as broken, so they are held only to what is true of any operation — the months add up, the balance is not negative, coverage does not move backwards. Not `verified`, which already means signature verification on profiles and would read as the same thing.

The check belongs in `Ledger.hs`, beside the arithmetic it verifies — `monthsFromAnchor` is internal there and would otherwise have to be exported. The column ships with the rest of the ledger schema, keeping it out of a migration of its own.

## The tests move with the types

The eight ledger-transition tests in `BadgeTests.hs` and their helpers move to `StatementEntry`. Assertions on `balanceMonths`, `balanceStartTs` and `paidThrough` survive as they are. The ones on periods do not: `pass` no longer returns a `BadgePeriod`, so `testTwelveMonths` and `testLapseAfterGap` state their period bounds as consecutive `balanceStartTs` values instead.

## Not in scope

The wire format does not change: `StatementEntry` is what the service already sends and the client already stores, and every column `badge_ledger` has today is still written. The only schema change is `balance_checked`, on the client's `badge_ledger` alone — the service has nothing to check, since it is the side that computes.
