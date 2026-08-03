# Badge service RPC protocol

Schema: `badges-rpc.schema.json`, definitions `request` and `response`. Types: `Simplex.Chat.Badges.Service`. Model: `plans/2026-07-30-supporter-badges-v3-ux.md` §3 — cited below as "model".

## Transport

Service RPC (`plans/2026-07-22-service-rpc-chat.md`, branch `rpc`): the request travels in `APISendServiceRequest.request`, the response in `CRServiceResponse.responseData`; one response per request; per-call timeout.

A request is an envelope: `version` — the client's protocol version; `purchaseKey`; `request` — the command, discriminated on `type`. Responses are discriminated on `type`. The service is deployed ahead of app releases, answers within the client's `version`, and rejects clients older than it supports with `unsupported_version`.

## Identity

Each purchase runs under a fresh Ed25519 key pair; `purchaseKey` is its public part and identifies the badge. The service cannot link purchases of one user; the exceptions are the declared upgrades below. `getBadgeCatalog` may omit `purchaseKey`: unsigned, it returns the catalog alone; signed, its response adds the purchase's `badgeStatement` — a client holding a lapsed badge checks for grants in the same request that prices a new purchase, and buys under a fresh key only when the statement shows none. Every other command requires the key and is signed with it. The agent delivers the verified signer key alongside the request; the service rejects a `purchaseKey` that differs from it with `bad_request`, and a key it holds no record of with `unknown_purchase_key`.

A purchase record is created by `getBadgeInvoice`, or by `purchaseBadge` funded with `apple`, `google`, or `code`; `transferBadge` creates the receiving one (post-MVP).

## Idempotency

A timeout hides the outcome, so the client repeats the identical signed request at its next trigger, never on a poll timer.

- `getBadgeInvoice` — returns the open invoice again; a new invoice is created only when none is open.
- `purchaseBadge` — a payment already granted returns the same `badgeCredential` and writes nothing.
- `upgradeBadgeSubscription` — evidence already applied returns the same result and writes nothing.
- `issueBadge` — repeated within an issued period, returns the cached credential and writes nothing.
- `transferBadge` — a receipt used by the same key returns the same result; used by another key, `receipt_used`.

## Commands

`purchaseBadge`, `upgradeBadgeSubscription`, and `issueBadge` carry `badgeRequest`, the signer's input (`BadgeRequest`, `Simplex.Chat.Badges`): the service signs exactly this content or rejects the command. The proposed `badgeExpiry` is capped by the funded coverage (`sundayAfter`, model §3); its absence requests a lifetime credential; `badgeExtra` is reserved and must be empty.

- `getBadgeCatalog` → `badgeCatalog` — the products and offers; signed, also the purchase's `badgeStatement`.
- `getBadgeInvoice` → `badgeInvoice` — prices the offer for `badgeInfo` and `paymentVia` (`card` — Stripe; `crypto` — btc, xmr): `badgeType`, `months`, `price`, `discount`, the upgrade `credit`, `amount` = price − discount − credit, `expiresAt`, and `paymentTo` — `url` for card; `address` and `cryptoAmount` for crypto. Offer state is checked here only: `deprecated` is still accepted, `disabled` is rejected, an inactive product yields `product_unavailable`.
- `purchaseBadge` → `badgeCredential` — verifies the funding (`apple` JWS offline; `google` token via the Publisher API; `invoice` against webhook-confirmed settlement, `payment_pending` until it lands; `code`), records the grant, and issues the first credential, in one round trip. `receipt` is the recovery bearer secret (model § recovery); the service stores its hash; lifetime badges receive none.
- `upgradeBadgeSubscription` → `badgeCredential` — the app-led store subscription change, on the same key: verifies the store evidence of the replaced subscription and records the new plan; an immediate upgrade returns the new credential, a deferred change returns none.
- `issueBadge` → `badgeCredential` — issues the next period from the balance, the only source of issuance. The ledger is advanced first; the credential is signed before the consume and issuance rows are written, in one transaction. An exhausted balance yields no `credential`; the `statement` shows why. Issuing on a paused badge resumes it (model 2.13).
- `pauseBadge` (post-MVP) → `badgeCredential` — suspends issuance and lapse (model 2.13).
- `transferBadge` (post-MVP) → `badgeCredential` — the receipt moves the remaining balance and the provider binding to the signing key; the transferred period's re-issue consumes a month, and the response carries a fresh receipt, retiring the presented one. Lifetime badges are not transferable by the op — support handles them.

## Upgrades

Always a new purchase under a new key, except store subscriptions, where the store owns the change.

- Non-store: `getBadgeInvoice.upgrade` — `fromPurchaseKey`, the old purchase's `receipt`, `receiptSignature` binding the old key to the new, and the asserted old `balance`. The invoice returns the conversion `credit`; settlement records `debit(conversion)` on the old purchase and the grant on the new.
- Store one-time: an upgrade SKU at a fixed discounted price; `purchaseBadge.upgrade` — `fromPurchaseKey`, `receipt`, `receiptSignature` — proves eligibility (an unexpired cheaper badge), because the store cannot gate who buys the SKU.
- Store subscription, app-led: the native subscription-group flow (Apple — immediate, with the store's prorated refund; Google — per replacement mode), then `upgradeBadgeSubscription` with the new evidence.
- Store subscription, sheet-led, and every downgrade: the client sends nothing — the service discovers the change from provider state and notifications, and each renewal grants months of the charged product.

## Statement and balance

Undefined, together with `catalog`, pending design. Intent: `balance` asserts the months the client believes it holds; `statement` returns the opening balance, the operations since, and the closing balance; the service confirms the assertion or corrects it with the full history. The ledger is written only by the service (model §3).

## Errors

`retryAfter` marks the transient codes: `payment_pending`, `provider_unavailable`, `rate_limited`. `offer_disabled` calls for a catalog refresh. `code_invalid` covers unknown and revoked codes; `code_used` — redeemed under another key. `receipt_invalid` covers unknown receipts. All other codes are terminal for the attempted command.
