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

```mermaid
stateDiagram-v2
  [*] --> unpaid: invoice opened, holds its price only
  unpaid --> queued: payment settled, the name held, the name's other open invoices cancelled
  unpaid --> taken: payment settled while another paid order holds the name
  queued --> committing: commit sent
  committing --> committed: commit mined, signing window 60 s to 24 h
  committed --> registering: owner signed, register sent
  registering --> registered
  committed --> waiting: window passed unsigned, terms or policy changed, counter moved, signature week ended
  waiting --> queued: the order's link asks to commit again, once per window
  waiting --> failed: no read by its page for 3 days
  committed --> failed: permanent reason
  registering --> failed: permanent reason
  queued --> cancelled: card dispute or payment reversal
  committed --> cancelled: card dispute or payment reversal
  queued --> failed: 1 month from payment, price to the balance
  committed --> failed: 1 month from payment, price to the balance
```

### 1.1 Payment holds the name
- **names:** another buyer's order is refused only while an order of the name has its register in flight (`Names/Orders.hs:295-302`, `Names/Store.hs:864-869`); search shows the name as available otherwise (`Web/Names/Search.hs:59, 69-70`). Two buyers can pay for one name; the loser ends `taken`, its price kept as a balance (`Names/Worker.hs:352-356, 369-383`).
- **Final:** a paid order holds its name against every order through the service, from payment until it is registered, cancelled, or the month from payment ends (1.3); a failure inside the month does not end the hold. Search shows the name as being registered. An open invoice holds only its price. A payment that settles while another paid order holds the name ends its own order `taken`, its price kept as its balance.
- **Why:** money from a second buyer for a name already paid for must not be taken; opening an invoice costs nothing, so it cannot hold a name.

### 1.2 Other invoices of a paid name are cancelled
- **names:** they stay open.
- **Final:** when an order's payment is fully settled, the name's other open invoices are cancelled at their providers (`pCancelInvoice`, `Providers.hs:237`). Money that still reaches one is late money, credited to that order's balance (`Names/LateCredits.hs`).

### 1.3 One month from payment
- **names:** a failed order holds its place with no end; it stays live, refusing only orders of the same owner code (`Names/Store.hs:760-772`).
- **Final:** the name stays reserved for the buyer for 1 month from payment acceptance, moves to other links or another name included. An order not registered by then fails, the name returns to sale, and the price goes to the order's balance; refunds from the balance are manual.
- **Why:** a hold extends the name's registration period; a month resolves every case that can be resolved.

### 1.4 Card disputes and payment reversals
- **names:** the service acts on three Stripe events only: succeeded, failed, cancelled (`Providers/Stripe.hs:104`); a dispute changes nothing and nobody is told.
- **Final:** a dispute or a reversal cancels the order while its register has not been sent; on a registered order it alerts the operators, with the order's evidence (payment, signed order, registration transaction) for the answer to Stripe.
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
- **Final:** a code can reserve a name for 3 months from its issue.
- **Why:** some time pressure; it bounds how long an old price stays valid.

### 1.7 When the service learns the name: as now
- The service learns the name only at the signing step, after the commit has aged (page `screens/order.ts:300`); until then it holds the name's hash and a copy sealed with the owner's key (page `flow.ts:49, 67-69`).

## 2. Failures and retries: wait only for what can resolve

### 2.1 One rule per failure reason
- **names:** four places decide. Errors without a revert, and every commit error, are retried every 15 s with no end (`Names/Worker.hs:178, 237-243, 300, 315`). A register or relay revert goes to `judge`, which fails the order at once for any reason it does not name (`:336-362`). The service's own signature check fails it the same way (`:305-309`). 5 sends of one kind fail it (`:449-458`).
- **Final:** one function classifies every reason. Permanent is what the operators cannot fix without the owner signing again or without a code change; every code error is permanent.
  - Temporary: retried, logged, reported to the group at about the 5th failure, until the month from payment ends (1.3).
  - Permanent: the order fails at once, keeps its commit and register transactions, and is logged and reported to the group.
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
| `DurationTooLong` (not in the service's list) | `SimplexController.sol:484, 612-616` | permanent; a move stays offered | failed |
| `DurationTooShort`, `ResolverRequiredWhenDataSupplied`, `ReverseRecordNotSupported`, `NameTooShort` | `:377-388, 406` | permanent: code error | failed |
| `LabelTooLong` | `BaseRegistrarImplementation.sol:151-152` | permanent unless operators raise the limit | failed |
| `NotSponsor`, policy `NotController`, `NotProxy` | `SimplexController.sol:169`; `SimplexNamePolicy.sol:400-401` | temporary, operator alert: wiring | failed |
| `ReentrantCall` | `SimplexController.sol:183` | permanent: a contract bug | failed |
| `Error(string)`, unknown code, no data | — | temporary, operator alert | failed |
| errors of admin, public paid, transfer and subname functions (25 names) | contract settings and other paths | temporary, operator alert: unexpected on these paths | failed |

Contracts: brenzi/simplex-namespace-contract e3ba0a5, `ens-contracts/contracts`.

### 2.2 Waiting for the owner
- **names:** an order whose signing window (60 s to 24 h after the commit; the contracts' deploy script, `deploy/simplex/00_deploy_simplex_controller.ts:46-47`) passes unsigned fails "The name was not signed in time" (`Names/Worker.hs:257-259`); Try again commits again (`:426-500`).
- **Final:** the order waits while the owner's page can resolve it: the window passed unsigned; the terms changed (the page asks again and never signs changed terms silently); the owner's counter moved; the name policy changed, so the owner signs the new terms; the signature's week ended. The page holding the owner code asks the service to commit again by itself; Try again goes. The order fails when its page has not read it for 3 days; the last read is stored at most once an hour.
- **Why:** waiting helps only while someone can act; 3 days covers a buyer away for a weekend.

### 2.3 The owner's signature is valid one week
- **names:** the deadline is the later of the window's end and the signing + 7 days (page `flow.ts:13, 89`).
- **Final:** at most one week. Within it, the service commits the same commitment again as often as needed and registers with the stored signature: the register recomputes the commitment from the registration (`SimplexController.sol:505-520`), and the signature covers the registration without its secret (`SimplexSponsor.sol:70-72`).

### 2.4 Refused links
- **names:** a link that is not a short link of its key's kind, or a second link for one key, is refused with `invalid_link`, without saying which (`Names/Reveal.hs:107-116, 142-145`; `Names/Links.hs:32-47`); the order fails "not signed in time" at the window's end; the page offers another name, or none of the links.
- **Final:** the buyer is told which link was refused, contact or channel, and asked to replace or remove it; the name stays reserved; the operators are alerted in the log and the group.

### 2.5 Counting attempts
- **names:** only sent transactions count, 5 per kind; a dry run that always reverts is retried forever.
- **Final:** up to 5 dry-run failures; once a dry run passes, up to 5 sent transactions failing for a permanent reason; temporary reasons have no count limit; the two counts are kept apart. New commits of a waiting order have no cap of their own (section 4 bounds them). The caps are settings in `store_service.ini`.

### 2.6 A signature refused after a configuration change
- **Final:** a stored signature refused because the service's chain id or sponsor address changed is a code error: the order fails at once, visibly, logged and reported.

## 3. Prompt service: the operators learn first

### 3.1 The 3-hour alert
- **names:** the queue alert fires after 20 minutes, but only for queued, committing, registering and committed with a stored signature (`README.md:912, 951`); an order waiting for its owner is not covered.
- **Final:** every paid order not registered within 3 hours of payment raises an alert in the log, the managed group and Prometheus. Badges: a later pull request.
- **Why:** a customer once complained after a week, when the browser state needed to investigate was gone.

### 3.2 Failure reasons are stored
- **names:** the transactions table keeps the signed transaction, hash, nonce, send time, mined block and status, without a reason; a dry run leaves no record.
- **Final:** every failed attempt is stored with its kind, whether a dry run or a sent transaction failed, its decoded reason and its time; the order shows the last one.

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
| Search | one case per name state: available, registering, reserved, registered with the store resolver's records or "other" |
| Refusals | every endpoint answers a refusal as `{"type": "<code>"}` with flat snake_case codes, such as `{"type": "order_exists"}`, and data beside the code, such as `{"type": "catalog_changed", "price": 1500}`; prepaid-code refusals flat, such as `{"type": "code_used"}`; a `bad_request` reason is logged, not sent (the user's A1-28: the reason as text); `edit_limit` is 409 with an ISO time. On `names`: `{"error": "<code>"}` |
| Order view | one shape per phase, the payment kind, the balance hold, the terms as a record, the failure reason |
| Order requests | order, code order, move and spend nest their fields, terms as a record; the payment method stays the text the badge request sends |
| Find, link edits, signing | decoded by derived types; find answers an empty list for an owner with no orders, and each order's phase as stored |
| Replaced orders | a closed order names the order that replaced it; a deleted closed order's link answers "This link does not work" |
| Links | one link per kind in a signing request; edits name their record as contact or channel; the clear-records edit goes |
| Config | `/api/config` answers chain, resolver, sponsor, TLD and term start once |
| Link preview | the chat core's own link data |
| Terms | the terms answer and the `terms_changed` refusal encoded from types |
| Deleted orders | a held read of a deleted order answers not found at once |

## 6. Stored data

- **Order phase:** one value per phase, decoded from the columns; a row that fits none is refused; a closed order (expired or cancelled) is its own phase; failure reasons are stored as tags. On `names`: a phase tag beside optional fields (`Names/Store.hs:198-232`), failure reasons as English sentences.
- **32-byte hashes:** one type, stored as 32 bytes, written as `0x` lowercase hex.
- **Transactions:** one state field, pending or settled.
- **Late credits:** the query computes the two values the code needs.
- **Edit reverts:** the contract error's name, stored by its tag.
- **Times:** one whole-second clock for every time names reads, in place of truncations at 25 writes; one time per order request.
- **Schema boundaries:** the names product uses the generic tables and its own, never a badge table.
  - **names:** names code reads and writes the generic `invoices` and `payments` and its own `name_*` tables (`Names/Store.hs`); none of the names modules mentions a badge table. The names migration creates only `name_*` tables and adds five columns to the generic `payments` (`confirmed_amount`, `received_units`, `confirmed_units`, `invoiced_units`, `invoice_rate`: a payment's crypto amounts and rate). The generic invoice read joins both products' tables to tell an invoice's product (`Store/Invoices.hs:212-222`). Every table carries the badge service's prefix `sx_badge_service_`, the names tables included.
  - **Final:** as `names`: the names migration changes no badge table.
- **Invoice product:** the generic `invoices` table names its product.
  - **names:** the generic invoice read left-joins both products' tables, `badge_code_invoices` and `name_invoices`, and reads `badge_type`, `months` and `registration_id` to tell an invoice's product and its product data (`Store/Invoices.hs:212-222`).
  - **Final:** `invoices` gets a `product` column, `badge` or `name`; the migration sets `badge` as the default for existing rows and `name` for the rows `name_invoices` refers to. Generic code reads the product from it and dispatches to the product's own code, which reads its own table; no generic query joins product tables.
- **Provider reference:** as `names`: each product's table holds the provider's invoice id, `name_invoices.provider_ref` and the deployed badge service's `badge_code_invoices.provider_ref`, both NOT NULL, each with its own unique index. Moving it into the generic `invoices` table would have the names migration change a badge table; a generic column for both products would be a change of its own.

## 7. Operators: commands and configuration

- **Console grammar:** `//issue supporter months 12` in place of `//issue supporter 12`; each command answers its own usage; `//revoke` answers `<code>: Revoked.`; `//issue name` is refused without `[names]` (the user: "ok, to option 1").
- **Service key:** one parser that accepts both cases, as `names` does (`Config.hs:754-759`).
- **Console commands:** one chain connection made at start, shared by the console and the group.
- **Configuration:** strict numbers with bounded messages; `credit_test_purchases` with `accept_unverified_store_receipts` refused at start; a new `--execute` option.
- **Loop failures:** one helper in simplexmq `Util` logs "… failed; the next pass will run" at 11 sites (the user's A3-7).
- **Configuration `Show`:** on `names`, a hand-written `Show` hides the chain configuration's RPC URL and key (`Names/Chain.hs:126-128`). Final: a configuration that holds a secret (the key, an RPC URL that can hold a provider key) has no `Show` instance; its tests compare with `Eq`; a log line renders only the parts it needs, locally (the user's A1-35: "either use stock Show, or parse and log locally").
- No comments in code.

## 8. Tests and CI

- The service's tests record the page's answers as fixtures; the page's contract tests read them.
- Signature, code and label test values in one fixture set, read by both sides.
- A `names-page` CI scope runs the page's tests and build.
- Two production-build layout tests stay skipped: they need the dev server's stand-in card form.

## 9. Internal structure, no behaviour change

From the review findings the user approved (plans/2026-11-16-names-review-fixes.md): shared maps as `TMap`; one function where code repeated; fewer modules; derived JSON in place of hand-built objects; simplexmq encodings for signatures and addresses (simplexmq #2083); stored tags decoded by their own cases.
