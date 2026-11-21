# Names service: changes against `names`

Principle (the user): a prompt, excellent service to people who pay, defended from people who try to exploit it. Technically: correct decisions, bounded retries, no retries of lost causes, every error tracked and debuggable.

Each item gives `names` (the base branch: service code of b13968b61, page 9a16ca996; line references are to it) and the final state, decided by the user on 17–18 Nov.

## Contents

1. [Paid orders: the name delivered, or the money returned](#1-paid-orders-the-name-delivered-or-the-money-returned)
2. [Failures and retries: wait only for what can resolve](#2-failures-and-retries-wait-only-for-what-can-resolve)
3. [Prompt service: the operators learn first](#3-prompt-service-the-operators-learn-first)
4. [Defence against exploitation](#4-defence-against-exploitation)
5. [The page and the service: one typed interface](#5-the-page-and-the-service-one-typed-interface)
6. [Stored data](#6-stored-data)
7. [Operators: commands and configuration](#7-operators-commands-and-configuration)
8. [Tests and CI](#8-tests-and-ci)
9. [Internal structure, no behaviour change](#9-internal-structure-no-behaviour-change)

## 1. Paid orders: the name delivered, or the money returned

The implemented states and transitions; apps/simplex-store-service/docs/names.md section 4 gives each transition's trigger and effect.

```mermaid
stateDiagram-v2
  state "closed: expired, cancelled or reversed" as closed
  state "committed: awaiting, signed or lapsed" as committed
  state "failed: registry refused, signature refused or unread" as failed
  state "failed: month ended" as monthEnded
  [*] --> unpaid: one request with the commitment, the signed registration and the terms acceptance; invoice opened, holds its price only
  [*] --> queued: a code order
  unpaid --> queued: payment settled, the name held, other buyers' open invoices of the name cancelled
  unpaid --> taken: payment settled while another paid order holds the name
  unpaid --> closed: invoice expired or cancelled; payment settled after a recorded reversal
  queued --> committing: commit sent
  committing --> queued: commitment lapsed, or commit mined with status 0 below its cap
  queued --> committed: commitment on chain
  committing --> committed: commit mined, signing window 60 s to 24 h
  committed --> committed: signature stored or cleared, window passed unsigned
  committed --> queued: the page asks to commit again past the window; CommitmentTooOld or CommitmentNotFound
  committed --> registering: commitment 60 s old, acceptance held, register sent with the stored signature
  registering --> committed: register mined with status 0 below its cap, the name free
  committed --> registered: register landed
  registering --> registered: register landed
  committed --> taken: the name gone at the confirmation depth
  registering --> taken: the name gone at the confirmation depth
  queued --> failed: permanent reason at its cap
  committing --> failed: permanent reason at its cap
  committed --> failed: permanent reason at its cap; signature refused by the configuration; no read by its page for 3 days
  registering --> failed: permanent reason at its cap
  failed --> queued: Continue of an unread order
  taken --> queued: a move to another name within the month, a paid order's of its price spent from its balance, a code order's on its code reserved again
  failed --> registered: a close finds the register landed
  failed --> taken: support's close of a paid order
  queued --> monthEnded: 1 month from payment, no register pending, money to the balance
  committed --> monthEnded: 1 month from payment, no register pending, money to the balance
  failed --> monthEnded: 1 month from payment, money to the balance
  queued --> closed: dispute or reversal, no register of the current name sent
  committing --> closed: dispute or reversal, no register of the current name sent
  committed --> closed: dispute or reversal, no register of the current name sent
  failed --> closed: dispute or reversal, no register of the current name sent
```

### 1.1 Payment holds the name
- **names:** another buyer's order is refused only while an order of the name has its register in flight (`Names/Orders.hs:295-302`, `Names/Store.hs:864-869`); search shows the name as available otherwise (`Web/Names/Search.hs:59, 69-70`). Two buyers can pay for one name; the loser ends `taken`, its price kept as a balance (`Names/Worker.hs:352-356, 369-383`).
- **Final:** a paid order holds its name against every order through the service, from payment until it is registered, cancelled, or the month from payment ends (1.3); a failure inside the month does not end the hold. Search answers such a name `taken`, and the page shows "Taken". An open invoice holds only its price. A payment that settles while another paid order holds the name ends its own order `taken`, its price kept as its balance, which pays that order's move to another name of its price within the month from payment (1.3), or is refunded.
- **Why:** money from a second buyer for a name already paid for must not be taken; opening an invoice costs nothing, so it cannot hold a name.

### 1.2 Other invoices of a paid name are cancelled
- **names:** they stay open.
- **Final:** when an order's payment is fully settled, the name's other open invoices are cancelled at their providers (`pCancelInvoice`, `Providers.hs:237`). Money that still reaches one is late money, credited to that order's balance (`Names/LateCredits.hs`).

### 1.3 One month from payment
- **names:** a failed order holds its place with no end; it stays live, refusing only orders of the same owner code (`Names/Store.hs:760-772`).
- **Final:** the name stays reserved for the buyer for 1 month from payment acceptance. Only an order whose name was taken moves to another name, as the same order, within the month from payment or its code's placement, up to `max_order_moves`; the view gives the month's end in the taken phase's `heldUntil`. A paid order moves at exactly its price, its balance spent on it; each take credits its price again, so it moves again after another take. A dearer or a cheaper name is refused `409 price_too_high` with the name's price. A code order moves on the same name code: its code is released at the take and reserved again by the move as a code the order holds, which a code another order reserved meanwhile refuses `409 code_reserved`, a spent one `409 code_used`, and a name the code does not cover `409 code_not_covering`; the code's expiry and revocation do not refuse it. Failed and lapsed orders are refunded by support. An order not registered by then fails, the name returns to sale, and the price goes to the order's balance; refunds from the balance are manual. An order with a register transaction sent and not settled is ended only after it settles.
- **Why:** a hold extends the name's registration period; a month resolves every case that can be resolved.

### 1.4 Card disputes and payment reversals
- **names:** the service acts on three Stripe events only: succeeded, failed, cancelled (`Providers/Stripe.hs:104`); a dispute changes nothing and nobody is told.
- **Final:** every dispute and reversal alerts the operators in the log and the group, with the order's evidence (payment, signed order, registration transaction) for the answer to Stripe, whatever the order's state. In addition:
  - an order of which no register transaction of its current name was sent is cancelled: the hold released, nothing credited; a support refund of part of its balance does not prevent it, nor a register of a moved order's earlier name;
  - an order of which a register transaction of its current name was sent is treated as registered for the dispute: not cancelled, its registration continues;
  - a disputed order's price is never credited to its balance, a late credit concurrent with the dispute included; an order whose price is already in its balance (taken, month ended) has it taken back from the balance, as far as the balance holds it; an order not cancelled whose refund support recorded is alerted only;
  - a balance pays no other order, so disputed money pays for no other name.
  - Events: `charge.dispute.created` and `charge.refunded` as reversals, `radar.early_fraud_warning.created` alerted only; badge card payments unchanged.
  - Records: the order's invoice row counts the reversals and keeps the total reversed and the last event's Stripe id and cents; a repeated delivery of the last event changes nothing; single events are read in Stripe.
- **Facts from Stripe's documentation:**
  - a buyer can usually dispute within 120 days of the payment; card networks allow longer in some cases, and for a future event or service the window starts at its date;
  - when a dispute opens, Stripe takes the disputed amount and a dispute fee from the balance at once; outside Mexico the fee is not refunded;
  - the merchant has usually 7–21 days to answer with evidence; the card issuer then decides in usually 60–75 days; the whole dispute takes 2–3 months;
  - a payment already refunded can still be disputed;
  - an early fraud warning from Visa, Mastercard or JCB leads to a fraud dispute in about 40% of cases; a refund prevents the fraud report only when processed as a reversal, usually within 2 hours of capture;
  - events: `charge.dispute.created`, `charge.dispute.updated`, `charge.dispute.closed`, `charge.dispute.funds_withdrawn`, `charge.dispute.funds_reinstated`, `radar.early_fraud_warning.created`.
  - Sources: https://docs.stripe.com/disputes/how-disputes-work, https://support.stripe.com/questions/disputes-on-a-refunded-transaction-faq, https://docs.stripe.com/api/events/types.
- **In-app payments, when added for names:** a store refund is a payment reversal under this rule. Apple notifies a granted refund (`REFUND`) and, for a consumable, first asks for consumption data within 12 hours (`CONSUMPTION_REQUEST`); Apple's pages state no deadline for a refund request, and most come within 30 days of purchase (WWDC23). Google refunds within 48 hours of purchase, then the developer decides; unauthorised charges can be reported within 120 days; refunds and chargebacks appear in real-time developer notifications and, for 30 days, in the Voided Purchases API. Sources: https://developer.apple.com/news/?id=tv3bhra6, https://developer.apple.com/videos/play/wwdc2023/10141/, https://support.google.com/googleplay/answer/15574897, https://developers.google.com/android-publisher/voided-purchases.

### 1.5 Prepaid-code orders
- **names:** a code order failed for 30 days gives its code back; the order stays failed (`Names/Worker.hs:108-110, 564-579`).
- **Final:** a code order not registered within the month from reservation releases its code and is closed, freeing the name, as paid orders.

### 1.6 Prepaid codes expire
- **names:** codes never expire: the expiry is stored and checked, but every code is issued without one (`Names/Codes.hs`, `issueNameCode`). No code has been issued yet.
- **Final:** a code can reserve a name for 3 months from its issue. A taken order's move reserves its own code again, which is not a new reservation, so the code's expiry does not refuse it (1.3).
- **Why:** some time pressure; it bounds how long an old price stays valid.

### 1.7 One request before the invoice
- **names:** the order request holds the name's hash, the commitment and a copy of the name and links sealed with the owner's key (page `flow.ts:67-71`); the name, links and signature come at the signing step, after the commit has aged (page `screens/order.ts:300`), so the page must be open then.
- **Final:** the order request, for paid and code orders alike, holds the name, links, commitment, the owner's signed registration and the terms acceptance; links are checked before the invoice. After payment the service commits, and registers once the commitment is 60 s old, with no page online. The page polls the order's state. The owner signs again only in the rare cases of 2.2.
- **Why:** the page cannot check the chain itself, so a second request protects nothing against the service, while it creates every waiting state; third parties stay unable to front-run, as the chain sequence (commit, at least 60 s, register) is unchanged. The service learns the names of unpaid orders too; the privacy policy's amendment states it.

## 2. Failures and retries: wait only for what can resolve

### 2.1 One rule per failure reason
- **names:** four places decide. Errors without a revert, and every commit error, are retried every 15 s with no end (`Names/Worker.hs:178, 237-243, 300, 315`). A register or relay revert goes to `judge`, which fails the order at once for any reason it does not name (`:336-362`). The service's own signature check fails it the same way (`:305-309`). 5 sends of one kind fail it (`:449-458`).
- **Final:** one function classifies every reason. Permanent is what the operators cannot fix without the owner signing again or without a code change; every code error is permanent.
  - Temporary: retried, logged, reported to the group once, with the latest reason, after 10 minutes of consecutive temporary failures of any reasons, until the month from payment ends (1.3); progress ends the streak. An exception in a step is a temporary failure; the step is run again in 15 s.
  - Permanent: retried up to its cap (2.5), then the order fails, keeps its commit and register transactions, and is logged as an error, reported to the group and counted in Prometheus by reason.
  - Waiting for the owner: what the owner's page resolves (2.2).
- **Why:** retrying a lost cause hides it; failing a fixable one loses a sale.

| Reason | Raised at | Final class | names |
|---|---|---|---|
| node or read failure | — | temporary | retried |
| gas price over `max_fee_per_gas`, gas over `gas_limit_cap`, key without funds, signing failure | service | temporary, operator alert | retried |
| `InsufficientAllowance` | `SimplexSponsor.sol:122` | temporary, operator alert | retried, error log |
| `UnexpiredCommitmentExists` (commit) | `SimplexController.sol:394` | expected: the commitment is recorded; not counted | retried |
| `CommitmentTooNew` | `:509` | wait 1 s | same |
| `CommitmentTooOld` | `:517` | commit again; logged and reported | commit again |
| `CommitmentNotFound` | `:516` | temporary: landed check, then commit again | failed |
| `NameNotAvailable`, `NameReserved`, gone at confirmation depth | `:503`; `SimplexNamePolicy.sol:403` | taken | same |
| the same while the name reads free, or gone only above depth | — | temporary | retried |
| `InvalidNonce` | `SimplexSponsor.sol:140` | owner signs again | same |
| `TermsNotAccepted`, `TermsMismatch` | `SimplexNamePolicy.sol:449, 478-482` | owner accepts and signs again | same |
| `SignatureExpired` | `SimplexSponsor.sol:138` | owner signs again | failed |
| `InvalidSignature` | `:163-164` | permanent: code error | failed |
| `DurationTooLong` (not in the service's list) | `SimplexController.sol:484, 612-616` | permanent | failed |
| `DurationTooShort`, `ResolverRequiredWhenDataSupplied`, `ReverseRecordNotSupported` | `:377-388, 406` | permanent: code error | failed |
| `NameTooShort` | `:377-388, 406` | permanent | failed |
| `LabelTooLong` | `BaseRegistrarImplementation.sol:151-152` | permanent unless operators raise the limit | failed |
| `NotSponsor`, policy `NotController`, `NotProxy` | `SimplexController.sol:169`; `SimplexNamePolicy.sol:400-401` | temporary, operator alert: wiring | failed |
| `ReentrantCall` | `SimplexController.sol:183` | permanent: a contract bug | failed |
| `Error(string)`, unknown code, no data | — | temporary, operator alert | failed |
| errors of admin, public paid, transfer and subname functions (25 names) | contract settings and other paths | temporary, operator alert: unexpected on these paths | failed |

Contracts: brenzi/simplex-namespace-contract e3ba0a5, `ens-contracts/contracts`.

### 2.2 Waiting for the owner
- **names:** an order whose signing window (60 s to 24 h after the commit; the contracts' deploy script, `deploy/simplex/00_deploy_simplex_controller.ts:46-47`) passes unsigned fails "The name was not signed in time" (`Names/Worker.hs:257-259`); Try again commits again (`:426-500`).
- **Final:** rare with the single request (1.7). The stored signature is cleared when it can no longer register: the terms changed (the page asks again and never signs changed terms silently); the owner's counter moved; the name policy changed, so the owner signs the new terms; the signature's week ended. The owner signs again; the service commits again only if the signing window passed meanwhile, when the page asks for it by itself, once per window. A link refused at signing again waits for the owner to replace or remove it (2.4). The order fails when its page has not read it for 3 days, logged, reported to the group and counted in Prometheus; the last read is stored at most once an hour; the name stays held until the month ends.
- **Why:** waiting helps only while someone can act; 3 days covers a buyer away for a weekend.

### 2.3 The owner's signature is valid one week
- **names:** the deadline is the later of the window's end and the signing + 7 days (page `flow.ts:13, 89`).
- **Final:** at most one week. Within it, the service commits the same commitment again as often as needed and registers with the stored signature: the register recomputes the commitment from the registration (`SimplexController.sol:505-520`), and the signature covers the registration without its secret (`SimplexSponsor.sol:70-72`).

### 2.4 Refused links
- **names:** a link that is not a short link of its key's kind, or a second link for one key, is refused with `invalid_link`, without saying which (`Names/Reveal.hs:107-116, 142-145`; `Names/Links.hs:32-47`); the order fails "not signed in time" at the window's end; the page offers another name, or none of the links.
- **Final:** links are checked at the order request, before the invoice (1.7), which names the refused link. The commitment fixes the links, so the owner's signing again checks no link. A link accepted at the order is accepted as stored wherever it is read again: a move's link equal to the stored link of its kind is not checked again.

### 2.5 Counting attempts
- **names:** only sent transactions count, 5 per kind; a dry run that always reverts is retried forever.
- **Final:** up to 5 dry-run failures for a permanent reason; up to 5 transactions mined with status 0; both counted per commitment, for commits, acceptance relays and registers together, and kept apart; temporary reasons have no count limit. A commit that succeeds counts nothing, so new commits of a waiting order have no cap of their own (section 4 bounds them). The caps are settings in `store_service.ini`.

### 2.6 A signature refused after a configuration change
- **Final:** a stored signature, within its deadline, refused because a setting it covers changed (chain id, sponsor, resolver or TLD) is a code error: the order fails at once, before its acceptance relay or register is sent, visibly, logged and reported. The service refuses to sign such an order again from its page; support resolves it.

## 3. Prompt service: the operators learn first

### 3.1 The 3-hour alert
- **names:** the queue alert fires after 20 minutes, but only for queued, committing, registering and committed with a stored signature (`README.md:912, 951`); an order waiting for its owner is not covered.
- **Final:** every paid order not registered within 3 hours of payment, failed orders and orders waiting for the owner included, raises one alert in the log and the managed group; the Prometheus gauge gives the oldest payment of a live order not registered, so a month-ended order leaves it. Badges: a later pull request.
- **Why:** a customer once complained after a week, when the browser state needed to investigate was gone.

### 3.2 Failure reasons are stored
- **names:** the transactions table keeps the signed transaction, hash, nonce, send time, mined block and status, without a reason; a dry run leaves no record.
- **Final:** the order's row counts its counted dry-run failures and its sent failures, and keeps its last failed attempt: its kind, whether a dry run or a sent transaction failed, its decoded reason and its time, for the caps, the logs and the reports; the order's view does not show it. A failure not counted toward a cap is written only when its kind, stage or reason changes. A move, which gives the order a new commitment, clears the counts and the last attempt in the same transaction.

### 3.3 Metrics never stop the service
- **names:** a failing metrics listener leaves the service running (`Metrics.hs:266-267`).
- **Final:** as `names`, and the failure is also reported to the group.

### 3.4 Alert rules
- **Final:** README alert rules on the registrar key's balance and on the fee cap, beside the existing ones (`README.md:951-958`).

## 4. Defence against exploitation

- An open invoice holds no name (1.1).
- Gas spent on reverts is bounded by the attempt counts (2.5).
- Committing again by the order's link: the link holds a 16-byte random id (`Store/Invoices.hs`, `newInvoiceId`); 5 requests a minute per client; only for an order whose window passed unsigned, once per window. A commit costs at most 0.0003 ETH at the 5-gwei cap (`README.md` gas table): at most about 30 commits, 0.009 ETH, per paid order within its month.

## 5. The page and the service: one typed interface

Answers are the service's types encoded as derived JSON, and the page reads those shapes; on `names` they are hand-built objects that the page validates by hand. The page (simplex.domains) changes in the same release.

| Area | Final |
|---|---|
| Search | one case per name state: available, taken (held by a paid order), reserved, registered with the store resolver's records or "other" |
| Refusals | every endpoint answers a refusal as `{"type": "<code>"}` with flat snake_case codes, such as `{"type": "order_exists"}`, and data beside the code, such as `{"type": "catalog_changed", "price": 1500}`; prepaid-code refusals flat, such as `{"type": "code_used"}`; a `bad_request` reason is logged, not sent (the user's A1-28: the reason as text); `edit_limit` is 409 with an ISO time. On `names`: `{"error": "<code>"}` |
| Order view | one shape per phase, the payment kind, the balance, the terms as a record, the failure reason; the name only once the order is paid |
| Order requests | order, code order and move nest their fields, terms as a record; the payment method stays the text the badge request sends |
| Find, link edits, signing | decoded by derived types; find answers an empty list for an owner with no orders, and each order's phase as stored |
| Replaced orders | as `names`: an order's view names no other order; the browser holding the name code finds the newer order through find |
| Links | one link per kind in a signing request; edits name their record as contact or channel; the clear-records edit goes |
| Config | `/api/config` answers chain, resolver, sponsor, TLD and term start once |
| Link preview | as `names`: the chat kind, the display and full names, a picture and a channel's subscribers |
| Terms | the terms answer and the `terms_changed` refusal encoded from types |
| Deleted orders | a held read of a deleted order answers not found at once |

## 6. Stored data

- **Order phase:** one value per phase, decoded from the columns; a row that fits none is refused; a closed order (expired or cancelled) is its own phase; failure reasons are stored as tags. On `names`: a phase tag beside optional fields (`Names/Store.hs:198-232`), failure reasons as English sentences.
- **32-byte hashes:** one type, stored as 32 bytes, written as `0x` lowercase hex.
- **Transactions:** one state field, pending or settled.
- **Late credits:** the query computes the two values the code needs.
- **Edit reverts:** the contract error's name, stored by its tag.
- **Times:** one whole-second clock for every time names reads, in place of truncations at 25 writes; one time per order request.
- **Registration keys:** `name_registration_id`, an integer primary key on both backends, and `registration_id`, the order's public id, TEXT NOT NULL UNIQUE, which the other tables reference. Orders of one second are listed, and an exact resend's order found, by `created_at DESC, name_registration_id DESC`. Every reference in the names migration names its column.
- **Schema boundaries:** the names product uses the generic tables and its own, never a badge table.
  - **names:** names code reads and writes the generic `invoices` and `payments` and its own `name_*` tables (`Names/Store.hs`); none of the names modules mentions a badge table. The names migration creates only `name_*` tables and adds five columns to the generic `payments` (`confirmed_amount`, `received_units`, `confirmed_units`, `invoiced_units`, `invoice_rate`: a payment's crypto amounts and rate). The generic invoice read joins both products' tables to tell an invoice's product (`Store/Invoices.hs:212-222`). Every table carries the badge service's prefix `sx_badge_service_`, the names tables included.
  - **Final:** as `names`: the names migration changes no badge table. It also adds `product` and `provider_ref` to the generic `invoices`, the reference copied from `badge_code_invoices`; it creates only names' own tables and is the service's only names migration.
- **Invoice product:** the generic `invoices` table names its product.
  - **names:** the generic invoice read left-joins both products' tables, `badge_code_invoices` and `name_invoices`, and reads `badge_type`, `months` and `registration_id` to tell an invoice's product and its product data (`Store/Invoices.hs:212-222`).
  - **Final:** `invoices` gets a `product` column, `badge` or `name`, NOT NULL; the names migration adds it with `badge` as the default for existing rows, and a name invoice is written with `name`. Code that does not know the product reads the invoice first, cases on its product, and reads the product's own table, two reads in one transaction; code that knows the product reads the invoice joined with that product's table, one read. No query reads both product tables. Precedent: a connection and the entity tables it points to (`getConnectionEntityKeys`, `src/Simplex/Chat/Store/Connections.hs:84-97`).
- **Provider reference:** in the generic `invoices` table, NOT NULL with a unique index, added by the names migration and filled for existing badge invoices from `badge_code_invoices`; the payment events, the poller and the overdue close find an invoice by it in `invoices` alone. `badge_code_invoices.provider_ref` stays as deployed, so new badge invoices write the reference in both tables; `name_invoices` holds none.

## 7. Operators: commands and configuration

- **Console grammar:** `//issue supporter months 12` in place of `//issue supporter 12`; each command answers its own usage; `//revoke` answers `<code>: Revoked.`; `//issue name` is refused without `[names]` (the user: "ok, to option 1").
- **Service key:** one parser that accepts both cases, as `names` does (`Config.hs:754-759`).
- **Console commands:** one chain connection made at start, shared by the console and the group.
- **Configuration:** strict numbers with bounded messages; `credit_test_purchases` with `accept_unverified_store_receipts` refused at start; a new `--execute` option.
- **Loop failures:** at each of the 12 sites, in 8 lanes, the failure is handled with `catchOwn'` and a handler that logs "… failed; the next pass will run". Where the lane uses the outcome (the metrics' chain read and registered names read, and the cleanup's read of unpaid orders), its handler returns the failure. No shared helper logs and discards an error: whether logging is enough is decided at each site (the user). The month pass and the late credit pass each set a gauge to the start time of their last completed pass, the service's start until then; README alert rules cover the age of both, refresh failures of the registered names and failed scrapes (`up == 0`). An invoice the sweep gives up stays among the read failures until it is expired, so a pass that did not expire it expires it at the next pass; the give-up is logged only once the invoice is expired. The chain RPC and the provider calls turn an exception of the request, a connection or TLS failure while the answer is read included, into their error values by simplexmq's `AnyError` and `tryAllOwnErrors'`, as the core does for `ChatError`; the terms fetch and the store requests do it by `catchThrow`. An exception in an order's step is an operator failure of its streak.
- **Configuration `Show`:** on `names`, a hand-written `Show` hides the chain configuration's RPC URL and key (`Names/Chain.hs:126-128`). Final: a configuration that holds a secret (the key, an RPC URL that can hold a provider key) has no `Show` instance; its tests compare with `Eq`; a log line renders only the parts it needs, locally (the user's A1-35: "either use stock Show, or parse and log locally").
- No comments in code.

## 8. Tests and CI

- The service's tests record the page's answers as fixtures; the page's contract tests read them.
- Signature, code and label test values in one fixture set, read by both sides.
- A `names-page` CI scope runs the page's tests and build.
- Two production-build layout tests stay skipped: they need the dev server's stand-in card form.

## 9. Internal structure, no behaviour change

From the review findings the user approved (plans/2026-11-16-names-review-fixes.md): shared maps as `TMap`; one function where code repeated; fewer modules; derived JSON in place of hand-built objects; simplexmq encodings for signatures and addresses (simplexmq #2083); stored tags decoded by their own cases.
