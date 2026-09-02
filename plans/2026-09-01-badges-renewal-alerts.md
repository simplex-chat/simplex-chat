# Badge renewal and alerts — code-redeemed badges

Continues `plans/2026-08-27-badges-mvp-streams.md` stream 1. Model `plans/2026-07-30-supporter-badges-v3-ux.md` §2.4, §2.9, §2.11, §3; engine sketch `plans/2026-07-31-badges-core-implementation.md` §6. Protocol `docs/protocol/badges-rpc.{md,schema.json}`.

Those two older plans predate the code. Where they disagree with what is built, what is built wins — see §7.

---

## 1. What this fixes

A code says how many months it is worth. Nothing reads it. `getBadgeCode` does not select `months`, and `redeemCode` hardcodes `addMonths 1 now`, so a twelve-month code buys one month and nothing renews it.

The ledger is the mechanism that fixes it: a redemption credits the months, each issuance debits one, and the client asks for the next one when it comes due.

**Scope.** Badges redeemed by code. No subscriptions, invoices, store payments, upgrades, transfers or pause — those tables and error paths stay untouched and unwritten. No app UI.

**Not in scope, deliberately.** Investor badges (the type stays, unused) and lifetime badges (being removed in `stable`, merged here later). Every badge in this slice has a balance and an expiry.

---

## 2. The ledger, minimally

Three operations, against the tables as built.

| op | entry_type | change | writes |
|---|---|---|---|
| grant, on redemption | `credit` / `payment` | +N months | one row |
| issue a month | `debit` / `badge` | −1 | one row + one `badge_issuances` row, same transaction |
| lapse elapsed months | `debit` / `lapse` | −k | one row |

Every row carries the state after it: `balance_months`, `balance_start_ts`, `balance_badge_type`. **The last row is the state** — nothing derives a balance by summing, on either side.

- coverage is `[balance_start_ts, addMonths balance_months balance_start_ts)`
- `paidThrough` = `addMonths balance_months balance_start_ts`, read from the last row alone
- credential expiry = `endOfSundayAfter (addMonths 1 balance_start_ts)` — **anchored on the balance, not on `now`**, or the schedule drifts a little further from the purchase every month

`advance t` runs before every grant and issue: lapse the fully elapsed unissued months in one row, if any. Issue requires `balance_months > 0`, `balance_start_ts <= t`, and no issuance already covering `[balance_start_ts, +1 month)` — that last check is what makes a repeated `issueBadge` return the stored credential and write nothing.

The columns this slice never writes stay NULL: `payment_id`, `charge_id`, `from_purchase_id`, `to_purchase_id`, `was_paused_since`. A code grant has no invoice, which the wire type already allows (`payment {invoiceId?}`, absent for codes) and the storage type does not — see §7.

`entry_uuid` is the service's identity for a row and the client's replication key. The client stores rows **verbatim** and never authors one; `entry_type_unknown` / `entry_type_value` keep a row a newer service wrote.

---

## 3. Service

- `getBadgeCode` selects `months` again; redemption grants that many.
- Redemption becomes: `advance` → grant → issue, and the response's `statement` carries the rows it wrote instead of being empty.
- `issueBadge` (`BSCIssueBadge`, already in the protocol) gets its handler: `advance`, then issue if a month is due, then reply. The `balance` the client asserts names its last held entry; the service returns the entries after it, or the whole ledger with an `opening` restatement when it names nothing the service holds.
- A repeat within an issued period replies with the **stored** credential and writes nothing. Not a re-sign: the same period signed twice yields a different signature and churns the client's credential for nothing.
- An exhausted balance is not an error: the reply carries no `credential` and a statement that shows why.
- Signing stays before every write, as in redemption. A signing failure writes nothing and the month is still due.

---

## 4. Client worker

One worker per user, on the agent `Worker` framework already used for delivery (`getAgentWorker` / `hasWorkToDo'` / `cancelWorker`, and the `TMap _ Worker` fields on `ChatController`). Per user because badge state is per profile and one profile must not stall another.

The worker holds no queue. A trigger only signals it; each pass reads stored state and derives the work. Signals are therefore free to be lost or duplicated.

**Pass**, per badge: is a month due, or has one lapsed, or is an alert derivable? If a request is needed, take the user's badge lock, send it signed with the purchase key, apply the response, then release. Applying a response means: copy the statement's entries verbatim, verify the credential, store the issuance, update the purchase, and re-present the badge to contacts.

**Triggers.** Chat start and chat activate. Network restore and profile switch belong here too and cost nothing to add, but no caller sends them in this slice. Redemption is *not* a trigger: it is one round trip that returns the credential and the statement, and the redeem command already stores the rows, sets the badge and broadcasts — there is no follow-up work.

**Scheduling.** No timer thread and no boundary map. Each pass ends by scheduling its own next wake-up, in the shape of `rescheduleWork` (`simplexmq` `NtfSubSupervisor.hs:478`): clear `doWork`, fork a sleeper that signals it at the next boundary, then block as usual. Two boundaries only, both day-granularity — the next month falling due, and `paidThrough`.

Unlike its original, this sleeper is **tracked per user and replaced**, following `deleteTimedItem` (`Internal.hs:1687`): cancel the previous before forking, cancel with the worker at chat stop, and re-check `waitChatStartedAndActivated` on waking. Badge horizons are a month where NtfSubSupervisor's are minutes, so untracked sleepers would accumulate — one per activate — instead of retiring.

**Expiry.** When the balance is exhausted and the last period ends, the shown badge is cleared and the profile update broadcast — the removal update of UX 2.11. This is the visible half of "the badge expired".

**Locking.** Add a `ChatLockEntity` constructor for the badge user and use `withEntityLock`, rather than the separate `badgeLocks` map §6 sketches. Same discipline, one lock map, and the `chatLock` ordering comes for free.

Nothing here is load-bearing for correctness: a pass derives its work from stored state, so chat start alone gives correct behaviour. It is what lets a client left running renew without a restart, and with `badgeGraceInterval` at 7 days a renewal hours late is invisible to contacts.

Timeouts are retried as the identical signed envelope at the next signal, never on a poll timer.

---

## 5. Alerts

One alert: `BASupportEnded`, at `paidThrough` with the balance exhausted, once.

`BAPrepaidEnding` — the 3-days-out warning — is **not** implemented, because a user cannot act on it. Topping up before the current period ends needs the service to credit a balance without issuing a credential, which is a change to the redemption path that beta does not carry; today a prepaid badge can only be continued after it has elapsed. Record that with a `TODO [badges]` in the alert derivation, where the missing branch is. The other three `BadgeAlertKind` constructors need subscriptions and stay unemitted.

Derived from state at the end of each pass, not stored as pending: compare the derived alert with `alert_acked_kind` / `alert_acked_episode` on the purchase, emit if they differ. The episode is the value that makes this occurrence distinct — `paidThrough`. Acknowledging writes the pair; snoozing sets `alert_snooze_until`, after which the alert is emitted once more. All three columns already exist.

---

## 6. Command and event surface

None of this exists — the names in §6 of the older plan have no code behind them. The minimum that makes the slice observable and testable from the terminal:

- `APIGetBadgeState` → the user's badges, balance, `paidThrough`, and current alert
- `APIAckBadgeAlert` — acknowledge or snooze
- `CEvtBadgeChanged` — state changed, including a renewal that arrived without a command
- `CEvtBadgeAlert` — an alert became derivable

Each needs its `chatCommandP` parser, `View.hs` rendering, and registration in `bots/src/API/Docs/{Commands,Responses}.hs`, which `tests/APIDocs.hs` enforces.

---

## 7. Types to correct first

Declared during design, never exercised — `Badges.Types`' `BadgePurchase`, `BadgeIssuance`, `CTPayment` and `CTCharge` have no users, so none of this costs a migration or a call site. (`Simplex.Chat.Badges` has a different `BadgePurchase`, the payment-proof sum, which `Badges.Types` hides; that one is in use and unaffected.)

- **Add a `code` credit type, on the wire and in storage.** The wire currently models a code grant as `payment` with `invoiceId` absent. Storing it as anything else would mean the client rewriting a row it is meant to replicate verbatim, so both sides gain the constructor together. It also makes the absence structural: a code's own invoice belongs to the buyer and lives in `badge_code_invoices` on the service, and the redeemer's ledger must never reference it. Cheap now, expensive once stream 2 ships.
- `CTPayment.invoiceId` becomes `InvoiceId`, not `Int64` — `invoices.invoice_id` is `TEXT`.
- `CTCharge.chargeId` becomes `Text`, matching `subscription_charges.charge_id` and its wire twin `SCCharge`, corrected in milestone A. Unused in this slice; changed so the twins stop disagreeing.
- `BadgePurchase` gains a funding sum — payment or code — rather than a mandatory `Int64` `paymentId`. Exactly one is set and the schema cannot say so, so the type should.
- `BadgeIssuance` loses its `Maybe` period, expiry and entry id. Lifetime is gone, the columns are `NOT NULL`, and every issuance is written beside exactly one `consume` row.
- `UserBadgeState`'s subscription fields stay: `renewsAt` is `Nothing` and `willRenew` is `False` until subscriptions exist.

Out of scope but worth knowing: `badge_ledger.payment_id` references a **payment** while the wire's `payment` credit names an **invoice**. Both are NULL for a code grant, so this slice never has to resolve it.

## 8. Order of work

**A — ledger on the service.** The injectable clock (§9), reading `months`, the three transitions, redemption grants and issues, the `issueBadge` handler, the statement in both responses. Done when a three-month code redeems and a second `issueBadge` a month later returns a second credential, asserted against the service's own rows.

**B — client replica.** Store the statement verbatim, read the balance from the last row, resolve §7's types. Done when the client's rows equal the service's row for row after a redemption.

**C — worker and renewal.** Worker, lock, self-scheduling, the pass, re-presentation. Done when a badge whose month has elapsed renews with no command, and one whose balance is exhausted loses its shown badge and broadcasts the removal.

**D — alerts and surface.** The ended alert, ack and snooze, the four commands and events. Done when the terminal shows it at `paidThrough` and acknowledging silences it.

## 9. Testing time

Nothing about the badge service is mocked: the harness already runs the real one in-process, so a fake would be less faithful and no faster. What is mocked is the clock.

**An injectable `now`** — `IO UTCTime` in config, defaulting to `getCurrentTime`. No badge code calls the clock directly: the transitions already take `now` as a parameter, and so do their two callers, the service handler and the worker pass. In tests both sides read one source — real time plus a test-controlled offset — so a test can redeem a twelve-month code, jump the offset a month, signal the worker, and assert, twelve times over, in milliseconds and against the real service, real signing and real rows.

The offset tracks real time rather than freezing it, which is what keeps the sleeper honest: `rescheduleWork` computes `actionTs - now` in shifted time and still sleeps the right real duration.

This is preferred over making the issuance period configurable. The period is baked into the credential's signed expiry and interacts with the Sunday rounding, so shortening it to seconds means disabling the rounding too — two knobs, and production arithmetic that no test exercises. A clock offset leaves every production computation exactly as shipped and only lies about the date.

It also reaches what waiting cannot: eight months offline, a boundary on the 31st, a leap day. Those hold the bugs.

Real elapsed time is then needed for one thing only — that the sleeper wakes the worker at all. With the offset set a second short of a boundary that is a one-second test.

Tests land with each step, extending `tests/Bots/BadgeServiceTests.hs`.

## 10. Done means

- a twelve-month code yields twelve monthly credentials, one per month, and a thirteenth request yields none
- a second request inside an issued month returns the credential already stored, unchanged
- re-issue happens without a command, from the worker's own wake-up alone
- an app offline across several months lapses exactly the elapsed ones and issues the current one
- redeeming the same code twice still yields one badge and one set of ledger rows
- client and service ledgers match row for row, and the client authored none of them
- the ended alert fires once, survives a restart, and stays silent once acknowledged
- an expired badge disappears from contacts' view without the user acting
