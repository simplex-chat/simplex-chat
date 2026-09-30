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
- `purchaseBadge` — a payment already credited returns the same `badgeCredential` and writes nothing; a store payment credited to another key, `receipt_used`.
- `upgradeBadgeSubscription` — evidence already applied returns the same result and writes nothing.
- `issueBadge` — repeated within an issued period, returns the cached credential and writes nothing.
- `purchaseBadge` with the recovery `receipt` — presented again by the same key it returns the same result; presented by another key, `receipt_used`.

## Commands

The client never states what is signed. The tier is that of whatever funded the purchase; the expiry is the `sundayAfter` of the period issued (`endOfMondayAfter` in code), shared by every credential of that week; `badgeExtra` is reserved and empty. A client-proposed expiry, even one capped by the funded coverage, would fall off that shared boundary and single its holder out of the week's anonymity set — so no command carries `badgeRequest`. `upgradeBadgeSubscription` still has the field in the schema; that is wrong and it must be dropped when the command is implemented.

- `getBadgeCatalog` → `badgeCatalog` — the prices and offers; signed, also the purchase's `badgeStatement`. Store builds never send it: prices come from the store and SKUs from app config.
- `getBadgeInvoice` → `badgeInvoice` — prices the purchase for `badgeInfo` and `paymentVia` (`card` — Stripe; `crypto` — btc, xmr). The response holds the generic `invoice` — `invoiceId`, `price`, `discount`, the upgrade `credit`, `amount` = price − discount − credit, `currency`, `expiresAt`, and `paymentTo` (`url` for card; `address` and `cryptoAmount` for crypto) — beside the badge part, `badgeType` and `months`. `priceId` pins the price the client displayed; `offerId` selects a discounted duration, and its absence buys one month at that price. Price and offer status is checked here only: `deprecated` is still accepted, `disabled` is rejected; a badge type with no active price yields `product_unavailable`.
- `redeemBadgeCode` → `badgeCredential` — redeems a code, records the credit, and issues the first credential, in one round trip. It carries `masterKey` and `code` and no `badgeRequest`: a code states no tier and no expiry, so the credential is what reports them. Errors: `code_invalid` for an unknown or malformed code, `code_used` when another key redeemed it, `code_expired` past a redemption deadline.
- `purchaseBadge` → `badgeCredential` — verifies the funding (`apple` JWS offline; `google` product id and token via the Publisher API; `invoice` against webhook-confirmed settlement, `payment_pending` until it lands; `receipt`), records the credit, and issues the first credential, in one round trip. It carries `masterKey` and no `badgeRequest`. The response `receipt` is the recovery bearer secret (model § recovery); the service stores its hash; lifetime badges receive none.
  - A store transaction is claimed by its own id, read from the evidence before verification, and credited only when the verified transaction carries that same id. A receipt already credited to the signing key is answered from the record, without asking the store.
  - Errors: `receipt_invalid`, `payment_pending`, `provider_unavailable` — see the classes below; `receipt_used` when another key was credited with it; `product_unavailable` for a product this service does not price; `provider_not_configured` when this deployment has no verifier for the store, terminal for the request since retrying cannot deploy one. `receipt_invalid` and `receipt_used` are the only answers on which the client drops the keys it signed with and finishes the store transaction. Every other answer records nothing and leaves the purchase with its keys, to be presented again at the next trigger.
  - **Which class an answer belongs to.** The two mistakes do not cost the same, so when it is not certain, answer unreachable. `receipt_invalid` tells the client the purchase is dead: it drops the keys it signed with and finishes the store transaction — consumed on Play, finished on StoreKit — and nothing can present it again, so a buyer answered this by mistake has paid for a badge they can never receive. Answering unreachable by mistake costs one more verification request each time the app presents the purchase again, and a Play purchase left unacknowledged for three days is refunded to the buyer.
    - **Terminal**, `receipt_invalid` — the store answered about this purchase and cannot change its mind: an Apple signature or certificate chain that does not verify, another app's bundle id, a test purchase (Apple Sandbox, Play `purchaseType` test), a refunded or revoked purchase, Play `purchaseState` canceled.
    - **Pending**, `payment_pending` — Play `purchaseState` pending.
    - **Unreachable**, `provider_unavailable` — the store was not asked or did not answer: network, timeout, 5xx, quota, this service's own auth or permission failures, and every Play error response, a 404 from `purchases.products.get` or `purchases.subscriptionsv2.get` and a 400 or 410 calling the token invalid included. A 404 decides nothing: Play answers it both for a token it has just issued and not yet recorded, and for one that was never a purchase. Only a purchase record is a verdict. A token whose characters this service will not send is never sent, so Play never answers about it — and Play documents no token grammar, so that check is our guess, not its verdict. A product id outside the characters Play does document is terminal, since no product of this app can have one.
    - Apple is verified offline, so it has no "not yet" and a negative answer about the signed evidence is terminal. Two things are not the store's answer and leave the client holding the purchase: the verifier's own failure, such as a root certificate it could not load, which is `internal`; and a product this service does not price, which is `product_unavailable` — the verifier vouched for the transaction either way.
  - Funding by `receipt` is a transfer (post-MVP): the unissued months of the purchase that receipt belongs to move to the signing key, recorded as `debit(transferOut)` on the source and `credit(transferIn)` on the new purchase, and the presented receipt is retired for a fresh one. The transferred period's issuance debits a month like any other. Lifetime badges hold no receipt, so support handles them.
- `upgradeBadgeSubscription` → `badgeCredential` — the app-led store subscription change, on the same key: verifies the store evidence of the replaced subscription and records the new plan; an immediate upgrade returns the new credential, a deferred change returns none. Its `badgeRequest` is to be dropped (see above).
- `issueBadge` → `badgeCredential` — issues the next period from the balance, the only source of issuance. It carries `balance` alone: the credential is signed with the purchase's stored master key, for the type the balance funds, expiring at the `sundayAfter` of the period issued. The ledger is advanced first; the credential is signed before the `debit(badge)` and issuance rows are written, in one transaction. An exhausted balance yields no `credential`; the `statement` shows why. Issuing on a paused badge resumes it (model 2.13).
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

The ledger is written by the service alone (model §3); the client keeps a verbatim replica and computes the effective balance from its last entry and the time. Month boundaries are counted from `balanceAnchorTs`, the start of the current run of months, and not from `balanceStartTs` — counting from the moving start would compound the day-of-month clipping of a short month, so a run beginning 31 January would reach 28 February and never return to the 31st. The service sets a new anchor only where a lapsed run restarts; months granted while coverage still runs extend it on its existing anchor.

`statement` — `entries`, and `previousEntryId` when they attach after an entry the client holds; its absence marks entries that attach to nothing. Each entry states `entryId`, the signed `changeMonths`, the resulting `balanceMonths`, `balanceStartTs`, `balanceAnchorTs`, and `balanceBadgeType`, `wasPausedSince` on the entry ending a pause, `createdAt`, and `entryType` — `credit`: `payment {invoiceId?}`, `code`, `charge {chargeId}`, `support`, `transferIn {fromPurchaseKey}`, `opening`; `debit`: `refund`, `upgrade {toPurchaseKey}`, `transferOut {toPurchaseKey}`, `support`, `badge`, `lapse`. A code grant is `code` rather than `payment` with no `invoiceId`: the invoice of a code belongs to whoever bought it, and the redeemer's ledger must never reference it. An unknown type is stored as received and decoded after an app upgrade.

`balance` — `lastEntry`, the client's last entry, asserting the position and the months it believes it holds.

An assertion that names an entry the service holds is a prefix: the service proceeds and returns what follows it. Otherwise the service heals its own ledger first — provider evidence for charges, time for lapses — proceeds, and returns either the complete history or one `opening` credit. An `opening` entry is an absolute restatement: the ledger is reset to the amount it states, without relation to the preceding entry, which also serves a new device and, later, the discarding of old history into a brought-forward balance.

## Errors

`retryAfter` marks the transient codes: `payment_pending`, `provider_unavailable`, `rate_limited`. `offer_disabled` calls for a catalog refresh. `code_invalid` covers unknown, malformed and revoked codes alike, so a guesser learns nothing from the difference; `code_used` — redeemed under another key; `code_expired` — past its redemption deadline. `receipt_invalid` covers forged, malformed, another app's, refunded and test receipts alike, so a guesser learns nothing from the difference; `receipt_used` — credited to another key; `provider_not_configured` — no verifier for that store is deployed, so the receipt is neither credited nor refused. All other codes are terminal for the attempted command.
