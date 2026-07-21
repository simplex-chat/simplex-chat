# Supporter Badges v2 — Implementation Plan

**Date:** 2026-07-21
**Status:** implementation-ready
**Companion:** [Product and UX plan](2026-07-20-supporter-badges-v2-product.md)

Payment verification creates a provider-neutral `PaymentCredit`. Badge issuance consumes it. Payment and badge are separate state machines on client and bot.

![Architecture](assets/badge-v2-roles.svg)

## Contents

- [1. Architecture](#1-architecture)
- [2. State machines](#2-state-machines)
- [3. Contracts](#3-contracts)
- [4. Provider flows](#4-provider-flows)
- [5. Persistence and `CallState` pattern](#5-persistence-and-callstate-pattern)
- [6. Reconciliation and errors](#6-reconciliation-and-errors)
- [7. Provider rules](#7-provider-rules)
- [8. Security and concurrency](#8-security-and-concurrency)
- [9. Delivery and tests](#9-delivery-and-tests)
- [10. API references](#10-api-references)

## 1. Architecture

### Responsibilities

```mermaid
flowchart LR
  C[Client] -->|one request| R[Service RPC]
  R --> O[Bot orchestrator]
  O --> P[Payment service]
  P --> V[Provider adapter]
  P --> X[PaymentCredit]
  O --> B[Badge service]
  B -->|credential| R
  R -->|one final response| C
  C --> K[Core verify and install]
```

| Component | Owns | Must not own |
|---|---|---|
| Client payment | capability, purchase UI, cached status, retry schedule | bot/provider truth |
| Client badge | credential receipt and installation | billing state |
| Payment service | proof verification, billing state, credit schedule | master key, credential |
| Payment credit | product and eligible monthly slot | provider proof, credential |
| Badge service | signing and idempotent credential cache | provider/billing logic |
| Core | signature verification and installed badge | payment status |

Treat these as separate programs with typed interfaces.

### Invariants

1. Provider verification changes payment state only.
2. Only verified entitlement creates a credit.
3. Only `CreditAvailable` plus a master key enters badge signing.
4. Credit consumption and cached issuance result are atomic/idempotent.
5. Payment never activates perks; verified credential does.
6. RPC has no caller identity or bot push. Capability authorizes each payment request.
7. Duplicate RPCs/events return the same result. Unknown states preserve prior state.
8. Provider dates create eligibility; retry/request time never changes badge expiry.

### Time and credit

```haskell
data PaymentCreditState = CreditAvailable | CreditConsumed | CreditVoided

data PaymentCredit = PaymentCredit
  { creditId :: CreditId
  , paymentId :: PaymentId
  , productId :: ServiceProductId
  , slotStart :: UTCTime
  , creditState :: PaymentCreditState
  }
```

- One-time: one credit at verified purchase time. Reject another one-time prepare while its badge is active.
- Subscription: `slotStart(n) = addCalendarMonths n verifiedAnchor` when `slotStart <= now < paidThrough`.
- Monthly and yearly plans both expose one credit per eligible month.
- Badge service computes expiry as the start of the month two months after `slotStart`.
- Unique credit: `(payment_id, product_id, slot_start)`.
- Example: 21 July slot → badge expires 1 September; monthly billing renews 21 August.

Credit eligibility by payment state:

| State | New credit |
|---|---|
| `BPPaidOneTime` | its single unissued credit |
| `BPActive` | current due slot through `paidThrough` |
| `BPGrace` | only while the provider explicitly reports entitlement |
| `BPCancelAtEnd` | due slots until `paidThrough` |
| all other states | none |

![State machines](assets/badge-v2-states.svg)

## 2. State machines

These names are canonical. Every transition is validated against the current constructor.

### Client payment

| State | Meaning |
|---|---|
| `CPNone` | no payment |
| `CPPreparing` | prepare RPC running |
| `CPStoreReady` | Apple/Google binding ready |
| `CPCheckoutReady` | Stripe URL ready |
| `CPProviderPending` | payment/approval pending |
| `CPVerifying` | evidence/status RPC running |
| `CPEntitled` | last bot status is paid |
| `CPCanceling` | management/cancel operation running |
| `CPCancelAtEnd` | renewal off; paid time remains |
| `CPProblem` | typed error + prior snapshot + retry time |
| `CPExpired` | no entitlement remains |

### Bot payment

| State | Meaning |
|---|---|
| `BPPrepared` | payment/capability/binding stored |
| `BPCheckoutOpen` | Stripe Session stored |
| `BPPendingProvider` | provider not complete |
| `BPVerifying` | reconciliation lease active |
| `BPPaidOneTime` | verified one-time payment |
| `BPActive` | paid subscription, renewal on |
| `BPGrace` | provider grants grace |
| `BPOnHold` | failed payment; no new credit |
| `BPPaused` | provider paused entitlement |
| `BPCancelAtEnd` | renewal off; paid time remains |
| `BPExpired` | paid time ended |
| `BPRefunded` | verified refund/chargeback |
| `BPRevoked` | provider revoked entitlement |

`BPVerifying` stores prior state, lease owner, and lease expiry.

### Client badge

| State | Meaning |
|---|---|
| `CBNone` | no usable local badge |
| `CBNeeded` | credit available |
| `CBRequesting` | issue RPC running |
| `CBReceived` | response cached, not installed |
| `CBInstalling` | core verification/install running |
| `CBInstalled` | verified and installed |
| `CBRetryableFailure` | retry while retaining old badge |
| `CBFinalFailure` | update/support required |

### Bot badge

| State | Meaning |
|---|---|
| `BBRequested` | credit/key idempotency row created |
| `BBSigning` | signing lease active |
| `BBIssued` | credential cached; credit consumed |
| `BBRetryableFailure` | same request can retry |
| `BBFinalFailure` | invalid/permanently unsupported request |

Credit states are `CreditAvailable`, `CreditConsumed`, and `CreditVoided`. There is no bot “installed” state.

## 3. Contracts

### RPC payload

```haskell
data ServiceCall = ServiceCall
  { requestId :: RequestId
  , payment :: PaymentInput
  , request :: Maybe ServiceRequest
  }

data PaymentInput
  = Prepare Provider ServiceProductId PurchaseKind
  | AppleEvidence PaymentId Capability SignedTransactionJWS
  | GoogleEvidence PaymentId Capability PurchaseToken
  | ExistingPayment PaymentId Capability
  | CancelSubscription PaymentId Capability
  | CreatePortal PaymentId Capability

data ServiceRequest = IssueBadge MasterKey (Maybe CreditId)

data ServiceResponse = ServiceResponse
  { requestId :: RequestId
  , payment :: PaymentSnapshot
  , credit :: Maybe PaymentCreditSummary
  , service :: Maybe (Either ServiceError BadgeCredential)
  , retryAfter :: Maybe NominalDiffTime
  }
```

Rules:

- `Prepare` cannot issue a badge.
- Apple/Google evidence may include `IssueBadge`.
- Stripe prepare returns Checkout data; a later `ExistingPayment + IssueBadge` issues.
- Pending response has no credit/result and includes `retryAfter`.
- Capability never enters Stripe metadata or a return URL.
- Store capability in the encrypted profile database and include it in supported profile transfer/backup. If it is lost, RPC identity cannot recover it; require explicit provider-bound restore/support and never silently reassign payment.
- `creditId` selects only; bot rechecks ownership/product/eligibility.

### Internal interface

```haskell
resolvePayment :: PaymentInput -> Transaction PaymentDecision
fulfillBadge  :: PaymentCredit -> BadgeRequest -> Transaction BadgeResult
```

Order:

1. authorize capability;
2. resolve/verify payment;
3. commit payment and create/load due credit;
4. pass only credit + request to badge service;
5. cache issuance and consume credit atomically;
6. return one final response.

### Idempotency and audit

- `requestId` binds to canonical request hash. Same body returns stored response; different body returns `idempotency_mismatch`.
- Transport replay dedupe is separate and shorter-lived.
- Stripe mutation idempotency key derives from request ID + operation.
- Developer Tools → Chat Console records start/result, request ID, method, payment suffix, before/after states, retry class, and duration.
- Redact capability, JWS/token, Checkout query/return token, master key, credential, and provider/customer IDs.

## 4. Provider flows

Product outcomes are in the Product Plan. These diagrams show implementation boundaries only.

### Common credit → badge path

```mermaid
sequenceDiagram
  participant C as Client
  participant RPC as RPC
  participant O as Orchestrator
  participant P as Payment service
  participant X as Credit
  participant B as Badge service
  participant K as Core
  C->>RPC: ServiceCall(payment, IssueBadge)
  RPC->>O: Authorized request
  O->>P: Resolve payment
  Note over P: Paid / active
  P->>X: Create or load slot
  Note over X: Available
  O->>B: Create request
  Note over B: Requested
  O->>B: Claim and sign
  Note over B: Issued
  B->>X: Commit result
  Note over X: Consumed
  O-->>RPC: Final status + credential
  RPC-->>C: Final response
  C->>K: Verify and install
  Note over C: Installed
```

### Apple initial verification

```mermaid
sequenceDiagram
  participant C as Client
  participant RPC as RPC
  participant O as Orchestrator
  participant A as Apple adapter
  participant P as Payment store
  C->>RPC: Apple evidence
  RPC->>O: Authorized request
  O->>A: Verify signed transaction
  A->>A: Verify signature, app, product,<br/>binding, dates, revocation
  A-->>O: Normalized result
  O->>P: Apply result
  Note over P: Paid / active
```

This path is offline. Status/restore uses App Store Server API; Notifications V2 only trigger reconciliation.

### Google verification

```mermaid
sequenceDiagram
  participant C as Client
  participant RPC as RPC
  participant O as Orchestrator
  participant G as Google adapter
  participant API as Publisher API
  participant P as Payment store
  C->>RPC: Google evidence
  RPC->>O: Authorized request
  O->>G: Verify token
  G->>API: productsv2.get / subscriptionsv2.get
  API-->>G: Canonical purchase
  G-->>O: Normalized result
  O->>P: Apply result
  Note over P: Paid / active
```

Commit entitlement before outbox acknowledgement/consume. RTDN triggers provider GET; never grant from notification payload.

### Stripe Checkout and webhook

```mermaid
sequenceDiagram
  participant C as Client
  participant RPC as RPC
  participant P as Payment service
  participant S as Stripe
  participant W as Webhook endpoint
  C->>RPC: Prepare Stripe
  RPC->>P: Authorized request
  Note over P: Prepared
  P->>S: Create Checkout Session
  S-->>P: Session ID + URL
  Note over P: Checkout open
  P-->>RPC: payment ID + capability + URL
  RPC-->>C: Final response
  Note over C: Checkout ready, start polling
  S-->>W: Signed webhook
  W->>W: Verify and persist event
  W-->>S: 2xx
  W->>P: Reconcile event
  P->>S: Retrieve current objects
  S-->>P: Canonical payment
  Note over P: Paid / active, credit available
```

Webhook handler verifies the raw body, persists/deduplicates event ID, returns `2xx`, then workers reconcile. Client remains pending until its next RPC.

### Stripe status while pending

```mermaid
sequenceDiagram
  participant C as Client
  participant RPC as RPC
  participant P as Payment service
  participant S as Stripe
  C->>RPC: ExistingPayment
  RPC->>P: Authorized request
  Note over P: Verifying
  P->>S: Retrieve Checkout
  S-->>P: Pending / unpaid
  Note over P: Pending provider
  P-->>RPC: Pending + retryAfter
  RPC-->>C: Final response
  Note over C: Poll later
```

Poll from Checkout open and on return/foreground at 5, 15, 30, 60, 120 seconds. Then use normal reconciliation. Deep links are optional; no localhost listener.

### Cancellation

| Provider | Client action | Bot action | Confirmed state |
|---|---|---|---|
| Apple | open Apple management UI; status RPC on return | App Store Server API status | `BPCancelAtEnd` |
| Google | open Play management UI; status RPC on return | `subscriptionsv2.get` | `BPCancelAtEnd` |
| Stripe | `CancelSubscription` RPC | set `cancel_at_period_end=true`, retrieve Subscription | `BPCancelAtEnd` |

Failure preserves previous state; client shows Retry and still says **Renews on**. “Already canceled” is success. Stripe Portal cancellation is disabled.

## 5. Persistence and `CallState` pattern

Mirror existing `data CallState` machinery:

- closed sums with state-specific fields;
- separate tag projection for queries;
- `deriveJSON (singleFieldJSON fstToLower)`;
- explicit SQL `TEXT` `ToField`/`FromField`;
- typed store reconstruction with inconsistent-row failure;
- controller `TMap` + per-payment locks;
- transition pattern matching + typed invalid-state errors;
- migrations before emitting new tags.

References: `Simplex.Chat.Call`, `Store.Profiles`, `Library.Commands`, `Library.Subscriber`, and `Controller`.

Define five separate sums: client payment, client badge, bot payment, credit, bot badge. Do not encode state as one nullable record.

### Client tables

`badge_payments`: provider/product/plan, payment state payload, encrypted capability/master key, binding/proof reference, `paidThrough`, `willRenew`, checked/retry time, version.

`badges`: payment/credit/slot/key hash, badge state payload, cached credential, expiry, attempt/error, version.

Join by payment/credit ID only. Update active profile only after core installation.

### Bot tables

| Table | Unique key / purpose |
|---|---|
| `payments` | provider-object ownership; canonical payment sum |
| `payment_credits` | payment + product + slot |
| `badge_issuances` | credit + master-key hash; cached credential |
| `rpc_requests` | request ID; request hash + final response |
| `provider_events` | provider event ID; dedupe/result |
| `outbox` | acknowledge, consume, reconciliation, cleanup |

Provider calls/signing run outside long transactions. Leases and compare-and-swap versions recover crashes.

## 6. Reconciliation and errors

### Client reconciliation

Triggers: launch, foreground, profile switch, network restore, store update, Stripe browser return, manual retry, six-hour jittered timer, and date boundaries.

```text
reconcile(paymentId):
  coalesce to one worker
  render cached payment + installed badge
  submit unseen Apple/Google evidence
  request status for nonterminal payment
  if current credit exists and badge is absent: request IssueBadge
  if credential returned: cache -> verify -> install
  schedule next check
```

Never infer provider entitlement from the local clock. Keep an active badge during payment errors.

### Total handling rule

Every input is one of:

1. apply legal transition;
2. return idempotent success;
3. preserve state and retry;
4. preserve state and reject/quarantine.

| Input/result | Class | Client | Bot |
|---|---|---|---|
| payment pending | retry | pending; schedule | keep pending; no credit |
| timeout/429/5xx | retry | prior snapshot; backoff | retain state; `retryAfter` |
| lost response | retry | repeat same ID/body | return cached result |
| duplicate event/request | idempotent | accept same result | dedupe/re-fetch |
| ID reused with new body | reject | new ID only for new action | preserve; telemetry |
| invalid capability/binding | reject | restore/support | preserve; rate-limit |
| invalid proof/product | reject | no blind retry | quarantine/alert |
| unknown provider state | quarantine | stale + retry later | re-fetch; do not guess |
| credit available | apply | request issuance | fulfill if requested |
| credit consumed | idempotent | install cached credential | return same issuance |
| signing unavailable | retry | keep old badge | retryable badge state |
| invalid key/credential/protocol | reject | update/support | final badge failure |
| install crash | local retry | resume cached response | no bot change |
| cancel timeout | retry | still show Renews | preserve canonical state |
| already canceled | idempotent | show end date | return cancel-at-end |
| user cancels store | exit | prior state | expire prepared row later |
| Stripe Checkout expired | final attempt | new checkout on user action | close attempt; no credit |
| refund/revocation | apply | payment ended; signed badge survives to expiry | void unused credits |
| webhook DB failure | retry delivery | no change | non-2xx; provider retries |

Stable codes: `bad_request`, `unsupported_version`, `payment_pending`, `payment_not_entitled`, `ownership_conflict`, `proof_invalid`, `provider_rate_limited`, `provider_unavailable`, `idempotency_mismatch`, `badge_already_issued`, `signing_failed`, `internal_error`.

### Crash recovery

- Before provider call: repeat request.
- Provider succeeds before commit: retrieve by idempotency key/object binding.
- Payment committed before issuance: credit remains available.
- Credential cached before response loss: repeat returns it.
- Response cached before install: resume local installation.
- Duplicate/out-of-order event: dedupe, re-fetch, monotonic transition.

## 7. Provider rules

| Provider | Verify | Identity/period | Notifications | Cancel |
|---|---|---|---|---|
| Apple | offline signed initial transaction; server API later | subscription: original transaction + renewal transaction | Notifications V2 → re-fetch | store UI |
| Google | products v2 / subscriptions v2 GET | linked token chain + order/period | RTDN → re-fetch | Play UI |
| Stripe | retrieve Session/Intent/Invoice/Subscription | one-time intent/session; subscription paid invoice | signed webhook → re-fetch | bot RPC |

Required mappings:

- Apple: active, grace, billing retry, cancel-at-end, expired, refunded/revoked.
- Google: pending, active, grace, on-hold, paused, canceled, expired, linked-token replacement.
- Stripe: Checkout open/expired, async pending/success/failure, invoice paid/failed, subscription active/past-due/unpaid/paused/cancel-at-end/deleted, refund/dispute.

Rules:

- Google initial subscription acknowledgement and one-time consumption run from durable outbox.
- Stripe uses server-selected Price, mode, Customer, `client_reference_id=paymentId`, metadata, and redirect URLs.
- Stripe subscription credit requires a paid invoice, not merely active Subscription status.
- Webhook/status/completion page use one reconciliation function; redirects never fulfill.
- Portal is for invoices/payment methods only. Apple/Google normal cancellation is store UI.

## 8. Security and concurrency

- Verify provider signatures/objects server-side; never trust decoded client/redirect fields.
- Hash capabilities; encrypt retained proofs/provider IDs; rotate keys.
- Keep raw master key client-encrypted and bot-memory-only during signing; persist its hash.
- Allowlist product, app/package, environment, currency/price, and account binding.
- Rate-limit operation/payment and cap payload sizes.
- Serialize payment mutations with lock/version; events and RPC use the same transitions.
- Use outbox for provider actions/events. Alert on stale leases, acknowledgement deadline, webhook lag, and signing failures.
- Trust client-shipped issuer keys; unknown key/protocol requires update.

## 9. Delivery and tests

1. **Schema/protocol:** five sums/codecs, migrations, credit boundary, request ledger, Chat Console audit, core install API.
2. **Apple/Google:** bindings, verification/status, Notifications V2/RTDN, acknowledge/consume, native UI.
3. **Stripe:** Checkout, completion page, webhook, reconciliation, cancel RPC, restricted Portal.
4. **UX/hardening:** scheduler, all Product states, migration, telemetry, cleanup.

Tests:

- JSON/SQL roundtrip and invalid-row tests for every constructor;
- legal/illegal transition properties for all five machines;
- message tests proving only the named owner changes state;
- Apple JWS/status/notification and Google pending/renewal/grace/hold/cancel cases;
- Stripe async payment, invoice renewal, cancellation, closed app/browser, delayed/duplicate/reordered webhook;
- monthly/yearly slots and 21 July → 31 August expiry;
- crash/replay at every side-effect boundary;
- capability/credit/master-key isolation;
- Chat Console coverage and redaction snapshots.

Release gates: provider sandbox E2E, webhook signature/replay, schema rollback, store-policy review, complete error handling, operational dashboards.

### Code locations

| Location | Change |
|---|---|
| new `Simplex.Chat.Badges.Lifecycle` | client sums/transitions/reconciliation |
| `Library.Commands.addUserBadge` | non-CLI verified install API |
| RPC/controller/console | calls, response handling, redacted audit |
| client store/migrations | separate payment and badge stores |
| Kotlin/Swift | derive Product UX state |
| bot payment repository | replace `customData`; providers, credits, outbox |
| bot badge repository | credit-only signing/cache; no provider imports |
| `badge-service/apple.py` | proof + subscription status |
| `badge-service/google.py` | full mapping + acknowledge/consume |
| `badge-service/stripe_api.py` | Checkout/webhook/status/cancel/Portal |
| `badge-service/wire.py` | versioned call/response; retain v1 migration path |

## 10. API references

| Provider | References |
|---|---|
| Apple | [StoreKit](https://developer.apple.com/storekit/), [subscription statuses](https://developer.apple.com/documentation/appstoreserverapi/get-all-subscription-statuses), Notifications V2 |
| Google | [Play Billing](https://developer.android.com/google/play/billing/integrate), [`productsv2.getproductpurchasev2`](https://developers.google.com/android-publisher/api-ref/rest/v3/purchases.productsv2/getproductpurchasev2), [`subscriptionsv2.get`](https://developers.google.com/android-publisher/api-ref/rest/v3/purchases.subscriptionsv2/get), RTDN |
| Stripe | [Checkout](https://docs.stripe.com/api/checkout/sessions/create), [fulfillment](https://docs.stripe.com/checkout/fulfillment), [webhooks](https://docs.stripe.com/webhooks), [subscription events](https://docs.stripe.com/billing/subscriptions/webhooks), [cancel](https://docs.stripe.com/billing/subscriptions/cancel), [Portal](https://docs.stripe.com/customer-management/integrate-customer-portal) |
| RPC | [`simplexmq` service RPC RFC](https://github.com/simplex-chat/simplexmq/blob/rpc/rfcs/2026-07-11-service-rpc.md) |
