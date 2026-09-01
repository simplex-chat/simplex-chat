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
- An exhausted balance is not an error: the reply carries no `credential` and a statement that shows why.
- Signing stays before every write, as in redemption. A signing failure writes nothing and the month is still due.

---

## 4. Client worker

One worker per user, on the agent `Worker` framework already used for delivery (`getAgentWorker` / `hasWorkToDo'` / `cancelWorker`, and the `TMap _ Worker` fields on `ChatController`). Per user because badge state is per profile and one profile must not stall another.

The worker holds no queue. A trigger only signals it; each pass reads stored state and derives the work. Signals are therefore free to be lost or duplicated.

**Pass**, per badge: is a month due, or has one lapsed, or is an alert derivable? If a request is needed, take the user's badge lock, send it signed with the purchase key, apply the response, then release. Applying a response means: copy the statement's entries verbatim, verify the credential, store the issuance, update the purchase, and re-present the badge to contacts.

**Triggers.** Chat start, chat activate, the timer, and immediately after a redemption. Network restore and profile switch belong here too and cost nothing to add, but no caller sends them in this slice.

**Timer.** One thread. Each pass records its user's next boundary; the thread sleeps to the earliest and signals whoever is due. Boundaries: the next month due, `paidThrough − 3d`, `paidThrough`, and any snooze expiry.

**Expiry.** When the balance is exhausted and the last period ends, the shown badge is cleared and the profile update broadcast — the removal update of UX 2.11. This is the visible half of "the badge expired".

**Locking.** Add a `ChatLockEntity` constructor for the badge user and use `withEntityLock`, rather than the separate `badgeLocks` map §6 sketches. Same discipline, one lock map, and the `chatLock` ordering comes for free.

Timeouts are retried as the identical signed envelope at the next signal, never on a poll timer.

---

## 5. Alerts

Without subscriptions only two of the five `BadgeAlertKind` constructors are reachable. Implement these; leave the others unemitted.

| alert | when |
|---|---|
| `BAPrepaidEnding` | 3 days before `paidThrough`, balance 0, once |
| `BASupportEnded` | at `paidThrough`, balance 0, once |

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

## 7. Resolve before writing the worker

Types declared during design that no code has exercised, and which this slice is the first to need. Each is a small decision, and all of them are cheaper to settle up front than to discover mid-implementation.

- `LedgerCreditType.CTPayment {invoiceId :: Int64}` cannot represent a code grant, which has no invoice — the wire type says `Maybe InvoiceId`. It is also `Int64` where `payments.payment_id` is `TEXT`.
- `CTCharge {chargeId :: Int64}` has the same `Int64`-vs-`TEXT` mismatch its wire twin `SCCharge` already had; that one was corrected to `Text` in milestone A and this one was left, deliberately, for whoever needed it first.
- `BadgePurchase.paymentId :: Int64` is neither optional nor `TEXT`; a code purchase has no payment.
- `BadgeIssuance` has `Maybe` period and expiry fields for lifetime badges, while the table is `NOT NULL` and lifetime is going away.
- `UserBadgeState` carries subscription fields (`renewsAt`, `willRenew`) that nothing in this slice can populate.

---

## 8. Order of work

**A — ledger on the service.** Read `months`, the three transitions, redemption grants and issues, `issueBadge` handler, statement in both responses. Done when a three-month code redeems and a second `issueBadge` a month later returns a second credential, asserted against the service's own rows.

**B — client replica.** Store the statement verbatim, read the balance from the last row, resolve §7's types. Done when the client's rows equal the service's row for row after a redemption.

**C — worker and renewal.** Worker, lock, timer, the pass, re-presentation. Done when a badge whose month has elapsed renews with no command, and one whose balance is exhausted loses its shown badge and broadcasts the removal.

**D — alerts and surface.** The two alerts, ack and snooze, the four commands and events. Done when the terminal shows the ending alert three days out, the ended alert at `paidThrough`, and acknowledging silences each once.

Tests land with each step, extending `tests/Bots/BadgeServiceTests.hs`. Time is the hard part: the transitions take the current time as an argument, so a test can issue twelve months of one badge without waiting — keep every date decision in a function that takes `now` rather than reading the clock.

---

## 9. Done means

- a twelve-month code yields twelve monthly credentials, one per month, and a thirteenth request yields none
- re-issue happens without a command, from the timer alone
- an app offline across several months lapses exactly the elapsed ones and issues the current one
- redeeming the same code twice still yields one badge and one set of ledger rows
- client and service ledgers match row for row, and the client authored none of them
- the ending and ended alerts each fire once, survive a restart, and stay silent once acknowledged
- an expired badge disappears from contacts' view without the user acting
