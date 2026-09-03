# Badge service RPC protocol

Schema: `badges-rpc.schema.json`, definitions `request` and `response`. Types: `Simplex.Chat.Badges.Service`. Model: `plans/2026-07-30-supporter-badges-v3-ux.md` §3 — cited below as "model".

## Transport

Service RPC (`plans/2026-07-22-service-rpc-chat.md`, branch `rpc`): the request travels in `APISendServiceRequest.request`, the response in `CRServiceResponse.responseData`; one response per request; per-call timeout.

A request is an envelope: `version` — the client's protocol version; `purchaseKey`; `request` — the command, discriminated on `type`. Responses are discriminated on `type`. The service is deployed ahead of app releases, answers within the client's `version`, and rejects clients older than it supports with `unsupported_version`.

## Identity

Each purchase runs under a fresh Ed25519 key pair; `purchaseKey` is its public part and identifies the badge. The service cannot link purchases of one user; the exceptions are the declared upgrades below. `getBadgeCatalog` may omit `purchaseKey`: unsigned, it returns the catalog alone; signed, its response adds the purchase's `badgeStatement` — a client holding a lapsed badge checks for credits in the same request that prices a new purchase, and buys under a fresh key only when the statement shows none. Every other command requires the key and is signed with it. The agent delivers the verified signer key alongside the request; the service rejects a `purchaseKey` that differs from it with `bad_request`.

A purchase record is created by `redeemBadgeCode`, by `getBadgeInvoice`, or by `purchaseBadge` funded with `apple`, `google`, or `receipt`. Those commands accept a key the service holds no record of — on a first purchase it always will. Every other command answers `unknown_purchase_key` for such a key.

## Idempotency

A timeout hides the outcome, so the client repeats the identical signed request at its next trigger, never on a poll timer.

- `getBadgeInvoice` — returns the open invoice again; a new invoice is created only when none is open.
- `redeemBadgeCode` — a code already redeemed by the signing key returns the same `badgeCredential` and writes nothing; redeemed by another key, `code_used`. The client must therefore keep the key it first signed with, or a retry cannot be recognised.
- `purchaseBadge` — a payment already credited returns the same `badgeCredential` and writes nothing.
- `upgradeBadgeSubscription` — evidence already applied returns the same result and writes nothing.
- `issueBadge` — repeated within an issued period, returns the cached credential and writes nothing.
- `purchaseBadge` with a `receipt` — presented again by the same key it returns the same result; presented by another key, `receipt_used`.

## Commands

`purchaseBadge`, `upgradeBadgeSubscription`, and `issueBadge` carry `badgeRequest`, the signer's input (`BadgeRequest`, `Simplex.Chat.Badges`): the service signs exactly this content or rejects the command. The proposed `badgeExpiry` is capped by the funded coverage (`sundayAfter`, model §3) and is required — a credential always expires, and a badge that does not is expressed as a long finite term; `badgeExtra` is reserved and must be empty.

- `getBadgeCatalog` → `badgeCatalog` — the prices and offers; signed, also the purchase's `badgeStatement`. Store builds never send it: prices come from the store and SKUs from app config.
- `getBadgeInvoice` → `badgeInvoice` — prices the purchase for `badgeInfo` and `paymentVia` (`card` — Stripe; `crypto` — btc, xmr). The response holds the generic `invoice` — `invoiceId`, `price`, `discount`, the upgrade `credit`, `amount` = price − discount − credit, `currency`, `expiresAt`, and `paymentTo` (`url` for card; `address` and `cryptoAmount` for crypto) — beside the badge part, `badgeType` and `months`. `priceId` pins the price the client displayed; `offerId` selects a discounted duration, and its absence buys one month at that price. Price and offer status is checked here only: `deprecated` is still accepted, `disabled` is rejected; a badge type with no active price yields `product_unavailable`.
- `redeemBadgeCode` → `badgeCredential` — redeems a code, records the credit, and issues the first credential, in one round trip. It carries `masterKey` and `code` and no `badgeRequest`: a code states no tier and no expiry, so the credential is what reports them. Errors: `code_invalid` for an unknown or malformed code, `code_used` when another key redeemed it, `code_expired` past a redemption deadline.
- `purchaseBadge` → `badgeCredential` — verifies the funding (`apple` JWS offline; `google` token via the Publisher API; `invoice` against webhook-confirmed settlement, `payment_pending` until it lands; `receipt`), records the credit, and issues the first credential, in one round trip. The response `receipt` is the recovery bearer secret (model § recovery); the service stores its hash; lifetime badges receive none.
  - Funding by `receipt` is a transfer (post-MVP): the unissued months of the purchase that receipt belongs to move to the signing key, recorded as `debit(transferOut)` on the source and `credit(transferIn)` on the new purchase, and the presented receipt is retired for a fresh one. The transferred period's issuance debits a month like any other. Lifetime badges hold no receipt, so support handles them.
- `upgradeBadgeSubscription` → `badgeCredential` — the app-led store subscription change, on the same key: verifies the store evidence of the replaced subscription and records the new plan; an immediate upgrade returns the new credential, a deferred change returns none.
- `issueBadge` → `badgeCredential` — issues the next period from the balance, the only source of issuance. The ledger is advanced first; the credential is signed before the `debit(badge)` and issuance rows are written, in one transaction. An exhausted balance yields no `credential`; the `statement` shows why. Issuing on a paused badge resumes it (model 2.13).
- `pauseBadge` (post-MVP) → `badgeCredential` — suspends issuance and lapse (model 2.13).

## Upgrades

Always a new purchase under a new key, except store subscriptions, where the store owns the change.

- Non-store: `getBadgeInvoice.upgrade` — `fromPurchaseKey`, the old purchase's `receipt`, `receiptSignature` binding the old key to the new, and the asserted old `balance`. The invoice returns the conversion `credit`; settlement records `debit(upgrade)` on the old purchase and the credit on the new.
- Store one-time: an upgrade SKU at a fixed discounted price; `purchaseBadge.upgrade` — `fromPurchaseKey`, `receipt`, `receiptSignature` — proves eligibility (an unexpired cheaper badge), because the store cannot gate who buys the SKU.
- Store subscription, app-led: the native subscription-group flow (Apple — immediate, with the store's prorated refund; Google — per replacement mode), then `upgradeBadgeSubscription` with the new evidence.
- Store subscription, sheet-led, and every downgrade: the client sends nothing — the service discovers the change from provider state and notifications, and each renewal credits months of the charged badge type.

## Catalog

`prices` — `priceId`, `badgeType`, `monthPrice`, `currency`, `status`, `createdAt`; `offers` — `offerId`, `priceId`? (absent applies to any price), `months`, `discount`, `status`, `createdAt`. An offer states a discount, as free months or a percentage; a duration without one is priced at `months × monthPrice`. Repricing appends a price and deprecates the old, which is still accepted at invoice creation; deprecated prices and offers are sent so that a refresh cannot remove what the client pinned, and disabled ones are omitted. Rendering is app-driven — tiers and durations come from app resources, and one without a price is shown disabled.

## Statement and balance

The ledger is written by the service alone (model §3); the client keeps a verbatim replica and computes the effective balance from its last entry and the time.

`statement` — `entries`, and `previousEntryId` when they attach after an entry the client holds; its absence marks entries that attach to nothing. Each entry states `entryId`, the signed `changeMonths`, the resulting `balanceMonths`, `balanceStartTs`, and `balanceBadgeType`, `wasPausedSince` on the entry ending a pause, `createdAt`, and `entryType` — `credit`: `payment {invoiceId?}`, `code`, `charge {chargeId}`, `support`, `transferIn {fromPurchaseKey}`, `opening`; `debit`: `refund`, `upgrade {toPurchaseKey}`, `transferOut {toPurchaseKey}`, `support`, `badge`, `lapse`. A code grant is `code` rather than `payment` with no `invoiceId`: the invoice of a code belongs to whoever bought it, and the redeemer's ledger must never reference it. An unknown type is stored as received and decoded after an app upgrade.

`balance` — `lastEntry`, the client's last entry, asserting the position and the months it believes it holds.

An assertion that names an entry the service holds is a prefix: the service proceeds and returns what follows it. Otherwise the service heals its own ledger first — provider evidence for charges, time for lapses — proceeds, and returns either the complete history or one `opening` credit. An `opening` entry is an absolute restatement: the ledger is reset to the amount it states, without relation to the preceding entry, which also serves a new device and, later, the discarding of old history into a brought-forward balance.

## Errors

`retryAfter` marks the transient codes: `payment_pending`, `provider_unavailable`, `rate_limited`. `offer_disabled` calls for a catalog refresh. `code_invalid` covers unknown, malformed and revoked codes alike, so a guesser learns nothing from the difference; `code_used` — redeemed under another key; `code_expired` — past its redemption deadline. `receipt_invalid` covers unknown receipts. All other codes are terminal for the attempted command.
