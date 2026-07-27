# Supporter Badges v2 — Product and UX Plan

**Date:** 2026-07-21
**Status:** implementation-ready
**Companion:** [Implementation plan](2026-07-20-supporter-badges-v2-implementation.md)

An **order** carries a plan (the product) and a payment. A verified payment issues one badge per eligible month (a row in the badge ledger); core verifies and installs it. Order, payment, and badge are separate.

![Lifecycle](assets/badge-v2-e2e.svg)

## Contents

- [1. Rules](#1-rules)
- [2. UX states](#2-ux-states)
- [3. Badge screen](#3-badge-screen)
- [4. Payment flows](#4-payment-flows)
- [5. Refresh and errors](#5-refresh-and-errors)
- [6. Acceptance criteria](#6-acceptance-criteria)

## 1. Rules

### Plans and providers

| Build | Payment | Cancel/manage |
|---|---|---|
| iOS | StoreKit | Apple subscription UI |
| Android Play | Play Billing | Google Play subscription UI |
| F-Droid / desktop | our own Stripe payment page (Payment Element, no email) | browser Customer Portal — authenticated session via order key or saved recovery handle |

Choices: **One-time**, **Monthly**, **Yearly**. There is no Extend action.

- One-time does not stack and is available again after badge expiry.
- Subscribing from one-time starts a new payment; there is no conversion API.
- Monthly and yearly plans issue one badge per eligible month.
- Cancellation stops renewal. It does not shorten an issued badge.
- Stripe uses the system browser. No localhost service is required.

### Tiers

Supporter and investor grant the same functional perk (2 GB file uploads); legend raises it to 5 GB; investor is otherwise cosmetic (badge color). The tier is fixed by the plan purchased (its SKU), not chosen afterward. Badge expiry is a shared end-of-month boundary, so everyone renewing in a month shares one anonymity set.

### Dates

| Payment event | Billing | Badge |
|---|---|---|
| Paid 21 July | monthly renews 21 August; yearly renews 21 July next year | valid through 31 August |
| Eligible period 21 August | monthly renews 21 September; yearly billing unchanged | new badge valid through 30 September |
| Canceled before renewal | subscription remains paid to provider period end | issued badge remains valid to signed expiry |

Show **Badge valid until** separately from **Renews on** or **Subscription ends on**.

### Sources of truth

- Core signature and expiry decide badge validity.
- Bot/provider verification decides payment status and issuance eligibility.
- Store state and Stripe redirects are hints only.
- Each client request receives exactly one bot response; the bot never initiates a call.
- Before buying, the client creates an order: an Ed25519 order key that both identifies and authenticates it (a new key is a new order), and one 32-byte BBS `BadgeMasterKey` for the badge. Renewals reuse the same order and keys.
- There is no bot-issued token and no caller identity. Every request is signed by the order key; payment additionally carries a fresh store proof (Apple/Google) or is confirmed by webhook (Stripe). The order key, BBS key, and provider proofs are redacted.

## 2. UX states

| State | Condition | Display | Actions |
|---|---|---|---|
| No badge | no entitlement or active badge | plans and prices | Buy once; Monthly; Yearly |
| Payment pending | provider not complete | old badge if valid | Continue; Check again |
| Issuing | paid; badge request/install running | old badge + progress | automatic retry; Retry |
| Active one-time | one-time badge active | tier; badge expiry | Monthly; Yearly |
| Active subscription | paid, renewing, badge active | interval; badge expiry; renewal | Cancel; Manage |
| Canceled, active | renewal off; paid/badge time remains | badge expiry; subscription end | Resubscribe |
| Payment issue | grace/on-hold/provider error | active badge until expiry | Fix payment; Check again |
| Badge missing | paid, but no usable badge | issuance error/progress | Retry |
| Expired | no entitlement or active badge | expired state | Buy once; Monthly; Yearly |
| Needs update | unknown issuer/protocol | unavailable | Update app |
| Offline/stale | refresh failed | cached state + check time | Retry |

An active installed badge remains visible during payment and network errors. These are display states the client computes from its order state plus the last bot answer; the persisted machines are in the implementation plan.

## 3. Badge screen

Display, in order:

1. badge, tier, proof status;
2. **Badge valid until**;
3. payment type and **Renews on** / **Subscription ends on**;
4. primary action, then manage/recovery action;
5. error and **Last checked** only when needed.

![No badge](assets/badge-v2-screen-s0.svg)
![One-time badge](assets/badge-v2-screen-s1.svg)
![Subscription badge](assets/badge-v2-screen-s2.svg)

Cancellation copy: **“Cancel renewal? Your subscription stays active until {date}. You won’t be charged again.”**

## 4. Payment flows

Every diagram is one outcome. Client and bot state are labeled separately.

### Apple

#### Success

```mermaid
sequenceDiagram
  participant C as Client
  participant A as StoreKit
  participant B as Bot
  C->>B: Purchase (Apple), signed order
  B-->>C: Order created + binding
  Note over C: Store ready
  C->>A: Purchase
  A-->>C: Signed transaction
  Note over C: Verifying
  C->>B: Purchase (Apple receipt)
  Note over B: Payment entitled, badge issued
  B-->>C: Status + badge
  Note over C: Entitled, badge installed
```

#### Pending

```mermaid
sequenceDiagram
  participant C as Client
  participant A as StoreKit
  participant B as Bot
  C->>B: Purchase (Apple), signed order
  B-->>C: Order created + binding
  C->>A: Purchase
  A-->>C: Pending
  Note over C: Payment pending, badge unchanged
  Note over B: Requesting, no badge
```

#### Canceled

```mermaid
sequenceDiagram
  participant C as Client
  participant A as StoreKit
  participant B as Bot
  C->>B: Purchase (Apple), signed order
  B-->>C: Order created + binding
  C->>A: Purchase
  A-->>C: User canceled
  Note over C: Previous state
  Note over B: Requesting row expires later
```

Apple initial proof is verified offline. Later status uses App Store Server API.

### Google

#### Success

```mermaid
sequenceDiagram
  participant C as Client
  participant G as Google Play
  participant B as Bot
  C->>B: Purchase (Google), signed order
  B-->>C: Order created + binding
  C->>G: Purchase
  G-->>C: Purchase token
  Note over C: Verifying
  C->>B: Purchase (Google receipt)
  B->>G: Verify with Publisher API
  G-->>B: Paid period
  Note over B: Payment entitled, badge issued
  B-->>C: Status + badge
  Note over C: Entitled, badge installed
```

#### Pending

```mermaid
sequenceDiagram
  participant C as Client
  participant G as Google Play
  participant B as Bot
  C->>B: Purchase (Google), signed order
  B-->>C: Order created + binding
  C->>G: Purchase
  G-->>C: Pending
  Note over C: Payment pending, badge unchanged
  Note over B: Requesting, no badge
```

#### Canceled

```mermaid
sequenceDiagram
  participant C as Client
  participant G as Google Play
  participant B as Bot
  C->>B: Purchase (Google), signed order
  B-->>C: Order created + binding
  C->>G: Purchase
  G-->>C: User canceled
  Note over C: Previous state
  Note over B: Requesting row expires later
```

### Stripe

#### Success

```mermaid
sequenceDiagram
  participant C as Client
  participant B as Bot
  participant S as Stripe
  C->>B: Purchase (StripeInvoice), create order
  B->>S: Create Customer (no email) + payment
  B-->>C: RspInvoice (our pay-page URL)
  C->>S: Open our pay page (Payment Element, no email)
  Note over C: Paid, awaiting confirmation
  C->>B: Purchase (StripePaid), same order
  Note over B: Hold call, no response yet
  S-->>B: Signed webhook
  B->>S: Retrieve and verify payment
  S-->>B: Paid
  Note over B: Payment entitled, badge issued
  B-->>C: RspCredential
  Note over C: Entitled, badge received
  C->>C: Verify and install
  Note over C: Badge installed
```

#### RPC ends before payment

```mermaid
sequenceDiagram
  participant C as Client
  participant B as Bot
  C->>B: Purchase (StripePaid)
  B->>B: Persisted payment is still pending
  Note over B: Hold call, no response
  Note over C: RPC deadline / app closes
  C-xB: Cancel waiting call
  Note over C: Paid — claim to retry
  C->>B: Repeat same Purchase on foreground
  Note over C: Retrying claim
  Note over B: Respond immediately if webhook already completed<br/>otherwise wait again
```

#### Payment page expired

```mermaid
sequenceDiagram
  participant C as Client
  participant B as Bot
  participant S as Stripe
  C->>B: Purchase (StripePaid)
  Note over B: Hold call
  S-->>B: Signed intent/session expired event
  Note over B: Payment page expired, no badge
  B-->>C: RspError payment expired
  Note over C: Expired — new payment page requires user action
```

There is no payment polling. The pending `Purchase` call is the completion signal. A deep link may return the user to the app but is not required and is never payment proof. Stripe payment is on our own page (Stripe Payment Element) so no email is collected — the hosted Stripe Checkout, which forces an email field, is not used.

### Cancel subscription

#### Apple

```mermaid
sequenceDiagram
  participant C as Client
  participant UI as Apple UI
  participant B as Bot
  participant API as Apple API
  C->>UI: Manage subscription
  UI-->>C: Return
  C->>B: Status request
  B->>API: Read status
  API-->>B: Renewal off + end date
  B-->>C: Updated status
  Note over C: Canceled, active until end date
```

#### Google

```mermaid
sequenceDiagram
  participant C as Client
  participant UI as Google UI
  participant B as Bot
  participant API as Google API
  C->>UI: Manage subscription
  UI-->>C: Return
  C->>B: Status request
  B->>API: Read status
  API-->>B: Renewal off + end date
  B-->>C: Updated status
  Note over C: Canceled, active until end date
```

#### Stripe

```mermaid
sequenceDiagram
  participant C as Client
  participant B as Bot
  participant P as Stripe Portal
  participant S as Stripe API
  C->>B: Request cancel link (signed, or with saved recovery handle)
  B-->>C: Authenticated portal session URL
  C->>P: Open portal, confirm cancel
  P->>S: Cancel at period end
  S-->>B: Signed webhook (renewal off + end date)
  B-->>C: Updated status on next check
  Note over C: Canceled, active until end date
```

Never show canceled until the bot confirms renewal is off.

Cancellation also works after the app is removed: Apple/Google via the store subscription UI; Stripe **only if the user saved their recovery handle** (we collect no email, so there is no email login). The bot reconciles the resulting cancellation from the provider webhook/status.

Stripe cancellation always happens in the browser Customer Portal (the portal cancels; the bot reconciles from the webhook). The bot always returns an authenticated per-customer session, sourced from whatever the client can still prove:

| Client still holds | Cancel link the bot sends |
|---|---|
| the order (can sign with its order key) | authenticated portal session — opens straight to the cancel flow, no login |
| only the saved recovery handle (order lost with the app) | same authenticated session, via handle → customer |
| neither | no cancel path — nothing binds them to the customer (no email was collected) |

## 5. Refresh and errors

Refresh on launch, foreground, profile switch, network restore, store update, Stripe browser return, manual retry, six-hour jittered timer, and payment/badge date boundaries.

If a paid period has no current badge, request issuance. Cache the response before core verification/install. There are no bot-initiated client events.

| Condition | Client action |
|---|---|
| Store canceled | restore previous screen |
| Payment pending | keep `Purchase` waiting; retry the same signed call after deadline/restart |
| Network/provider failure | keep cached state and active badge; retry |
| Paid, issuance failed | show “Payment confirmed. Badge is being prepared”; retry |
| Cancel failed | keep **Renews on**; retry |
| Payment issue | show Fix payment / Manage |
| Order signature / proof failure | restore/support; no sensitive details |
| Unknown issuer/protocol | require update |
| Invalid credential | reject; retain old badge; retry/support |
| Duplicate/lost response | repeat same request; no duplicate charge/badge |

Errors preserve the last payment snapshot and installed badge. The implementation plan defines retry/final handling.

## 6. Acceptance criteria

- No badge, one-time badge, and subscription badge UX is complete.
- Choices are One-time, Monthly, Yearly; no Extend.
- Payment on 21 July shows badge through 31 August while billing keeps its provider date.
- Apple, Google, and Stripe have separate linear outcomes.
- Payment verification issues into a provider-neutral badge ledger; badge service has no provider logic.
- Client and bot order/badge state are separate.
- Requests are client-signed with the order key (which also identifies the order), one bot response each; the per-period badge ledger dedups repeats.
- Tier and billing period are fixed by the plan (SKU); the client cannot request a higher tier or longer life than it paid for.
- Stripe payment is on our own page (Payment Element, no email), not hosted Checkout; needs no localhost/deep-link success. Cancellation is always an authenticated per-customer Customer Portal session, reached via the order key or a saved recovery handle (no email login).
- Every error category has an owner, state-preserving action, and retry/final result.
- RPC attempts/results appear redacted in Developer Tools → Chat Console.
