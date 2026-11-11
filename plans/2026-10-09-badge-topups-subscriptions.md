# Badge top-ups and store subscriptions — design space

Branch `badge-in-app-checkout` builds a store lane for one-time badge purchases. It is not released. Two things are not built:

- **Top-up:** buying more months while a badge is shown.
- **Subscriptions:** the stores' monthly and annual plans.

This document compares two top-up designs, sets out how top-up and subscriptions depend on each other, lists the work and external waits of each step, and lists what the product lead must decide. It is a design space, not an implementation plan.

## Summary

**Does one need the other?**

- Subscriptions do not need top-up, if Subscribe is offered only when no badge is shown and tier changes use a tier-changing grant.
- Top-up A needs nothing from subscriptions.
- Top-up B shares machinery with same-ledger subscriptions: lapse-before-grant and the late-landing problem.

**Smallest correct top-up: design A.** Store Buy opens in a short window before the shown badge ends. The payment is credited at once as a new purchase that becomes the shown badge. Nothing changes in the service or the ledger. The buyer gives up the overlap, at most the window.

**Subscriptions on top cost:**

- service: a charges table, a ledger reader that understands charges, renewal discovery, a database lock, a tier-changing grant, and a verify path for Play subscriptions;
- client core: asking the service at zero months, keeping the badge on until it answers, and the store's expiry on the wire;
- apps: sale, restore, manage and cancel screens;
- external: store subscription setup, notifications, Pub/Sub, quotas, translations.

**Late-presented renewals.** A grant added before unused months are written off loses the paid month. Writing off first fixes that, but coverage then restarts at presentation, 0–7 days late by default. Dating the grant to the store's billing period removes the drift.

**Recommendations** (judgement): A for top-up, if subscriptions are far off; same-ledger subscriptions over A-style ones.

**Decisions for the lead:** review test purchases; A, B, both or neither; A's window and downgrades; subscribing while a badge is shown; Family Sharing; refund clawback in the first release; Play App Signing custody.

**Top risks:** the store product type; Play's owned-product rule and product identity; refunds while a payment is held; synchronised renewal requests.

**Biggest open engineering question:** same-ledger subscriptions need a design for charges that land after the badge came off. The only worked fix is broad.

## Reading notes

**Labels.**

- **Fact:** read in code at the place cited, or traced through it.
- **Model:** stated in the draft model, cited by section or line.
- **Judgement:** a conclusion drawn here.
- **Inference:** a consequence of cited facts, not run.
- **Unverified:** Apple or Google behaviour, and intent read from code comments.

**Sources.**

- Code is `badge-in-app-checkout` unless marked "vb", the unmerged `f/badge-store-verifiers`. "mq" is simplexmq at `e24d537`.
- The service is under `apps/simplex-badge-service/src/BadgeService/`. Its deployed backend is Postgres, so its `Migrations.hs` is `Store/Postgres/Migrations.hs`.
- Core is under `src/Simplex/Chat/`: `Library/Commands.hs`, `Badges/Ledger.hs`, `Store/Badges.hs`. `Chat.hs` is `src/Simplex/Chat.hs`.
- `M20260915_user_badges.hs` lines are the SQLite variant; the Postgres one is a line earlier.
- `badges-rpc.md` and `badges-overview.md` are under `docs/protocol/`.
- Swift files are under `apps/ios/Shared/`, Kotlin ones under `apps/multiplatform/`. `PlayStore.kt` is the `google` flavour's.
- **Model** is the draft `plans/2026-07-30-supporter-badges-v3-ux.md`.

**Terms.**

- **Shown badge:** the purchase in `users.shown_badge_id`.
- **Retirement:** taking the shown badge off at paid-through.
- **Held payment:** a store payment reaching a profile with a shown badge. It waits until retirement.
- **Hand-over:** the app passing a store receipt to core (`APIPurchaseBadge`). A **sweep** hands over every unfinished store purchase.
- **Tip:** a purchase's last ledger entry. **Run:** the months since the ledger's anchor. **Coverage** ends at paid-through.
- **Re-bind:** moving a purchase to new keys after the old keys are lost.
- **Step 1** is the one-time lane's release, which both features build on.

## How it works today

**Only the newest purchase is managed** (fact).

- The badge worker and badge state read the newest purchase (`Store/Badges.hs:398-411`; `Commands.hs:5549`, `:5634`).
- Requests need a shown credential (`Commands.hs:5687-5691`).
- Retirement acts only on a shown newest purchase (`Commands.hs:5827-5835`).

**While a badge is shown, nothing creates a purchase** (fact). Four guards refuse:

- invoice creation (`Commands.hs:3587`);
- code redemption (`Commands.hs:5326`);
- store-receipt selection (`Store/Badges.hs:227`);
- store-receipt attempt (`Commands.hs:5529`).

**When a held payment is credited** (fact unless marked).

- Only retirement clears the shown badge.
- On iOS, retirement waits for a chat run; background refresh counts.
- On Kotlin, chat is never suspended (`Chat.hs:211`), so retirement happens at paid-through (`Commands.hs:5699-5707`).
- Credit then waits for chat start (`Commands.hs:269`), iOS activation (`:619`) or a hand-over (`:5298`).
- At start and activation, retirement runs asynchronously after the held-receipt check, so credit likely slips to a later trigger (inference).

**A held payment while it waits.**

- Its store transaction stays unfinished (fact, `BadgeStore.swift:240-261`).
- On Play it is acknowledged at hand-over but not consumed (fact, `BadgeStore.kt:281-283`), so Play's 3-day auto-refund does not apply.
- Play verification is online; a canceled `purchaseState` is refused (fact, vb `Google.hs:122-128`). Whether a refund sets that state is unverified.
- Apple verification is offline, over the stored transaction (fact, vb `Apple.hs:39-49`; `Store/Badges.hs:136`). A refund during the hold is credited unless the app re-hands a fresh transaction.
- Its product is mapped only at credit (fact, `Service.hs:446-448`; `Catalog.hs:148-154`). An unmapped product is retried daily and never finished (`Commands.hs:5507-5511`, `:5534-5541`).
- Another device on the same Play account sweeps unconsumed purchases (fact, `BadgeStore.kt:353-361`; `PlayStore.kt:103-116`). It records the purchase under its own profile and can credit it first (`Store/Badges.hs:144-148`). The buyer then gets `receipt_used` (`Service.hs:479`). Whether Play lists the purchase there is unverified.
- Whether Apple delivers another device's unfinished consumable is unverified.

**The ledger** (fact, `Ledger.hs:93-101`).

- `grantEntry` adds to the balance and keeps the anchor and tier.
- It restarts the run only when the balance is zero.

**A grant must follow a lapse** (fact, traced).

- A 2-month code is redeemed on 2 Oct, leaving 1 month unissued from 2 Nov. The app is away until 15 Feb.
- A bare +1 grant gives 2 months from 2 Nov. The next issue writes off min(2, 3) = 2, so the new month is lost.
- Lapsing first writes off 1, and the grant restarts the run on 15 Feb.

**The service never grants onto an existing purchase** (fact).

- A purchase starts from an empty ledger (`Service.hs:447-463`, `:489`).
- A receipt under a known key fails on `UNIQUE(purchase_key)` (`M20260915_user_badges.hs:92`) and is answered `internal`. The Play purchase is already acknowledged, so it is paid and never credited.
- Every refusal is answered before any database write (`Service.hs:420-463`).

**Other facts.**

- Store one-time months are issued at credit (`Catalog.hs:150-153`; `Service.hs:489-491`). An issued credential stays valid after a refund (model §3, O6).
- A Play quantity other than 1 is answered `internal` (`Service.hs:431-432`, `:471`) and retried forever (`Commands.hs:5732`). It is paid and never credited.
- With vb, Sandbox and licence-tester purchases are refused as `receipt_invalid` (`Service.hs:433`).
- Without vb, a well-formed receipt not credited to the presenting key is answered `provider_not_configured` (`Service.hs:424-427`). A malformed one is refused first, except a Play token failing the grammar check, which is retried as `provider_unavailable` (`StoreReceipts.hs:71-80`).
- vb will not start its dev mock beside a real verifier (vb `Config.hs:231-232`).

## Top-up A: a new purchase from inside a window

**What it is** (judgement; recommended).

- Store Buy is allowed in a short window before the shown badge's paid-through, for example its last 7 days, until retirement.
- The payment is credited at once as a new purchase under new keys. It becomes the shown badge and is presented at once, as every credit is today (`Commands.hs:5865`, `:5515-5520`) and as the model's MVP says (model 2.5, line 486).
- Today Buy works only after retirement (fact, `Commands.hs:3587`; `Store/Badges.hs:227`). A's gain: the buyer pays before the badge ends, and the badge never goes off.

**Why credit at hand-over, not at retirement** (judgement).

- At hand-over, the new month starts at credit (`Service.hs:489`). The buyer loses the overlap, bounded by the window.
- At retirement, no days are lost, but every payment is held with the exposures above.
- Contacts would also see the badge removed and re-added.
- A warm iOS background refresh (`BGManager.swift:129`) and a backgrounded Kotlin app need new trigger plumbing to credit at retirement.

**Changes.**

- The invoice and receipt guards admit a purchase inside the window (`Commands.hs:3587`, `:5529`).
- The selection guard is SQL on `shown_badge_id IS NULL` (`Store/Badges.hs:227`), but paid-through is computed in Haskell, so that query is restructured.
- The query should filter by window, and the worker should wake when the window opens (judgement). Otherwise each refused attempt shows the buyer a `BIFUnexpected` error until a daily retry succeeds (`Commands.hs:5537-5541`, `:5725`, `:5625-5628`).
- The purchase is credited as its own tier (`Service.hs:447-449`), so no tier rule is needed for correctness. Downgrades are decision 3.
- The guards live in client core, so they bind honest clients only.
- The badge screens on both platforms rework their state. A shown badge replaces the purchase flow on iOS (`BadgesView.swift:26-31`) and closes purchase screens on Kotlin (`BadgesView.kt:28-32`, `:51-57`).

**Recommended, cosmetic: tolerate a credit landing during retirement** (judgement).

- Credit takes the badge lock; retirement takes the profile lock (`Commands.hs:5504`, `:5830`).
- Retirement reads the purchase once (`:5549`), then clears the profile badge unguarded (`:5833`).
- A credit committing in between makes contacts see the badge removed, then re-added on the next pass (`:5824`, `:5869`) (inference).
- The stale pass also raises "support ended" for the old purchase (`:5567`, `:5573`, `:5592`). The app may keep it until its next reload.
- Fix: clear the profile badge conditionally in retirement's transaction, and derive the alert from the shown purchase.

**Optional** (judgement).

- A prepaid-ending alert three days before paid-through. Without it the window is found only on the badge screen, because the chat-list pitch is hidden while a badge is shown (`BadgesView.swift:67-69`; `OnboardingCards.kt:66-67`). The apps render only two alert kinds today (`ChatListView.swift:389`, `:393`), so it is new UI.
- Deferring presentation until the old credential expires, as renewals do (`Commands.hs:5822-5824`). It keeps the request and the profile update on different days (objective 3, `badges-overview.md:205`). It must skip downgrades, and delays a higher tier's perks by up to 8 days.

**Limits.**

- The apps allow one pending purchase per profile (`BadgeStore.swift:120-135`, `BadgeStore.kt:167-183`).
- A is store-only. Codes still wait for retirement, so foss, desktop and link-out buyers get no top-up.
- A sold code expires 365 days after sale (`Orders.hs:21-22`, `:112`; `Service.hs:413`). A code bought d days into a 12-month badge stays redeemable on that profile for about d days after retirement. It can be redeemed on another profile at any time.
- Admitting codes in the window would fix both, at the cost of the code-screen rework (`BadgesRedeemCodeView.swift:288-310`, `BadgesRedeemCodeView.kt:369-377`).

**What it links.** No payments by key. Timing links them: the new purchase starts near the old paid-through.

## Top-up B: credit onto the shown badge

**What it is** (judgement). The payment's months extend the shown badge's ledger, under its key. The badge is never retired and re-added.

**Service.**

- The request carries a store payment and the client's last ledger entry, signed by the purchase key.
- `upgradeBadgeSubscription` already has that shape (`Badges/Service.hs:94-98`; `badges-rpc.md:44`), less its `badgeRequest` field, which must be dropped (`badges-rpc.md:30`). It is unimplemented (`Service.hs:341-348`).
- The service lapses the tip, then grants, then issues if due. Lapsing first is required (see the grant trace above).
- Today's issue path writes only if the tip has not moved, but replies normally either way (`Service.hs:519-532`). B must answer a skipped write as an error. Otherwise the client marks the payment credited with nothing credited.
- Payments must be looked up through the ledger. Today's lookup joins only `badge_purchases.payment_id` (`Store.hs:204-218`), so a top-up re-presented by another device would get `internal` forever (`Service.hs:457-461`).
- A SKU of another tier is refused before any write. `grantEntry` keeps the ledger's tier, so crediting it would grant months at the wrong tier: legend months for a supporter payment, or the reverse.
- B is store-only. Admitting codes would also need the code lookup moved to the ledger, or a retried multi-use code would grant twice (`Store.hs:185-202`).

**Client.**

- The invoice and receipt guards admit a top-up in the window. The code guard stays: a code redeemed while a badge is shown would become newest and strand the stacked months (`Store/Badges.hs:303`, `:398-411`).
- The receipt row is marked as a top-up of the shown purchase at Buy, and signs with that purchase's keys.
- The row can fall back to A until its first request is sent, not after (`Service.hs:474-482`).
- A top-up during coverage usually issues nothing (`Ledger.hs:106`), but `requestBadgeStash` rejects a reply without a credential (`Commands.hs:5344-5346`). That path changes.
- A credited row with no purchase reads as refused today (`Store/Badges.hs:185-187`, `:289`). A credited mark fixes that.
- Retirement must re-read the tip in its own transaction; today it uses a stale one (`Commands.hs:5552`, `:5556`).

**Late landing.**

- Suppose the target retires and another purchase becomes newest before the credit lands.
- Re-showing the target strands the newer purchase and locks out later Buys and codes (`Store/Badges.hs:446-452`). Not re-showing it strands the new months.
- Fix one: defer retirement while a sent request's outcome is unknown. It has no bound while the device is offline.
- Fix two: manage every purchase with unissued months or an active store charge. That is broad: newest-only is assumed by `getUserBadgePurchase`, the single `BadgeState` (`Badges/Types.hs:270-284`), alert derivation (`Commands.hs:5590-5612`), retirement and both apps.

**Costs.**

- Stacked months are lost with the chat database. Re-bind cannot recover a finished one-time purchase if the store does not re-deliver it (unverified).
- A topped-up month lapses if the app does not run during it. Today every store month is issued at credit.
- Unissued months keep their value after a refund, unless a stacking cap or refund handling exists.
- It links payments by key, including a store payment to a code-funded badge's key. That goes beyond the exceptions at `badges-rpc.md:13` and against `badges-overview.md:206`.

## How top-up and subscriptions depend on each other

**Top-up A needs nothing from subscriptions** (judgement).

- Exception: if Apple requires a month of access to be a non-renewing subscription (unverified), restore work moves into step 1 (risk 1).

**Subscriptions need nothing from top-up, under conditions** (judgement).

- The first charge: offer Subscribe only when no badge is shown, as one-time Buy works today. Then existing holders wait for retirement to subscribe.
- Tier changes always reach a profile showing that subscription's badge, and Apple's store UI can change tier without the app (unverified). Same-ledger subscriptions need a tier-changing grant anyway (Subscriptions), which handles this.
- Subscribing while a badge is shown would reuse A's guards and screens.

**Two ways to credit renewals.**

- **Same-ledger:** each renewal credits the subscription's own purchase.
- **A-style:** each renewal is credited as a new purchase under fresh keys.

**What same-ledger shares with B** (judgement).

- Lapse-before-grant on an existing ledger.
- Keeping the badge on until the service answers whether a charge landed.
- The late-landing problem: a subscription revived after grace (Apple billing retry, Play account hold, resubscription; unverified) can land after another purchase became newest. B's broad fix two solves it.
- If the app also hands renewals over: B's client half, its ledger payment lookup, and the tip re-read at retirement.
- Not shared: B's cross-tier refusal, its top-up rows, and stacking.

**What A-style costs** (judgement; not recommended).

- On iOS, a renewal handed over at launch races retirement, so the badge can come off and on (inference). Avoiding that needs the same retirement deferral as same-ledger.
- Each renewal's month starts at its credit (`Service.hs:489-491`). A backlog handed over together forfeits all but one month.
- On Apple, a renewal is filed under whichever profile is active at hand-over (`Store/Badges.hs:139-157`).
- On Play, renewals share one token (unverified), so a renewal resolves to the old record and is never credited (`Commands.hs:5299-5302`).
- Fresh keys buy no unlinkability: the stores link renewals anyway (unverified).

**Either way.**

- Step 4 must keep subscription badges out of A's window, or a top-up and a renewal compete for the next credit (`Store/Badges.hs:228`).
- That needs a way to tell a subscription-funded badge apart. The client keeps no product column (`Store/Badges.hs:289`, `:296-301`), though ledger rows store their credit type.

**The model is split** (judgement).

- Its ledger example tops up on the same ledger (model line 379). Its same-row upgrade (§2.10) is B's credit, deferred to phase 2 (§7).
- But it creates a row per purchase (model line 417).
- On subscription tier changes, the model makes a new row with new keys (§2.10), while the protocol keeps the key (`badges-rpc.md:50`, `:54`).

## Subscriptions: the work

**Crediting time** (judgement, traced through `Ledger.hs:93-116`).

- A charge learned while coverage runs is added, and issued at the next request.
- A charge learned after coverage ended restarts the run, so coverage drifts by the gap.
- Today there is no request at zero months (`Commands.hs:5689`). An added one would naturally fall a day before the credential expires, 0–7 days after paid-through (`Ledger.hs:240-243`), so drift is the default (inference).
- So the grant should be dated to the store's period start (model O2).
- On an existing ledger, the client's check marks such a grant backdated if it predates the tip (`Ledger.hs:164`). That check changes.
- For a new ledger, the service seeds at now (`Service.hs:489`). Seeding at the period start also needs `signFirstMonth` to lapse first (`Service.hs:489-491`), or it issues an elapsed month.

**Tier changes.**

- Apple: a subscription group lets the store's UI change tier (unverified). Play: tiers are separate products (fact, `BadgeStore.kt:31`, `:36`).
- The protocol has the service discover changes (`badges-rpc.md:55`). No such path exists (`Service.hs:341-348`).
- `grantEntry` keeps the tier, so a tier-changing grant is needed. The service signs from the tip's tier (`Service.hs:358`).
- The client stores the tier once (`Store/Badges.hs:297`) and must update it.
- An immediate Apple upgrade refunds the old plan pro rata (unverified), so it needs refund handling.

**Charges.**

- Use the commented draft schema (`M20260915_user_badges.hs:155-227`).
- The ledger reader has no charge case (fact, `Ledger.hs:214-225`). A charge row breaks the statement read (`Store.hs:272-290`), so the purchase stops issuing and every reply fails.

**Discovery.**

- Use Apple's App Store Server API and Play's `subscriptionsv2` (unverified).
- Reading them when the client asks avoids a background writer. But renewal requests arrive together each week (`Ledger.hs:236-243`; `Commands.hs:5696-5707`), and each store call takes up to 10 s (`StoreReceipts.hs:65`) against a 30 s client deadline.
- A background lane is a second ledger writer. The deployed Postgres runs today's check-then-append at default isolation (`Service.hs:527-533`), so it needs a lock before reading the tip.
- A renewal the app hands over is claimed by its own transaction id (`Commands.hs:5316`), and must be mapped to the original.
- Handed-over Play subscriptions need a verify path: vb checks only one-time purchases (vb `Google.hs:105-107`).
- Store API quotas and raising them are external (unverified).

**Client core.**

- At zero months, ask the service whether a charge landed, and keep the badge on until it answers. That needs the store's expiry on the client; no response carries it today (`Badges/Service.hs:121-125`).
- Raise the subscription alerts the code names but never raises (`Badges/Types.hs:120`).

**Re-bind and restore.**

- The model's store re-bind is unbuilt. Apple requires a restore mechanism (unverified).
- The ledger reader has no transfer or upgrade case either (`Ledger.hs:212-219`), so re-bind rows would break it like charge rows.

**Apps.** Subscriptions are not for sale today (fact, `BadgeStore.swift:35`, `BadgeStore.kt:49`). Selling needs the sale screens, hand-over, restore, manage and cancel, and store disclosures.

## Order, work and waits

**Order** (judgement), assuming refund clawback is not in the first release (decision 6):

1. Release the one-time lane.
2. Top-up A, if subscriptions are far off (decision 2).
3. Refund handling.
4. Subscriptions, same-ledger, after the late-landing design.

Every external item below is unverified: external state cannot be checked from the repo.

### Step 1: the one-time lane

**Ours.**

- Merge `badge-in-app-checkout` and vb. Revert the TEST ONLY URLs (`BadgeStore.swift:42-44`, `BadgeStore.kt:56-61`).
- Deploy the verifiers with a production issuer key at index 1–8 (mq `Crypto/Entitlement.hs:132-143`) and the Apple root certificate.
- Deploy the production service holding the keys of the address compiled into core (`Chat.hs:74`), or replace that address before the release build. It cannot change without an app release.
- Publish a stable simplexmq server release with entitlement support, upgrade our XFTP servers, and configure retention, which is opt-in per server (mq `FileTransfer/Server/Main.hs:166-170`).
- Build the Google Play bundle the repo documents (`apps/multiplatform/README.md:27-28`). No script or CI job builds it (`scripts/android/build-android.sh:137`).
- Spread the weekly renewal requests across their day-long lead, as the TODO at `Commands.hs:5696-5698` designs.
- Drop queued requests older than the client's deadline. The queue records no receive time today (`Service.hs:82`).
- If a load test shows it is needed, take store calls off the serial request loop, where one Play credit can block it for about 20 s (vb `Service.hs:444`, `:468`). Concurrent requests then need the Postgres tip lock.
- A test deployment: a separate service with its own address and the index-9 key, both compiled into core (`Chat.hs:73-74`). That means separate core builds per platform. XFTP retention cannot be tested, since servers trust keys 1–8 only (mq `Agent/Env/SQLite.hs:234`).
- A badge-service section in `PRIVACY.md`; terms of sale and a refund policy for the web checkout.
- The model's prerequisites: a release with the 7-day recipient display grace before purchases ship (model line 518). It is in 7.1 (`Badges.hs:166-182`) but not stable 7.0.4. The model's Monday removal updates are not assumed here; the code removes at paid-through (`Commands.hs:5829-5834`).

**External.**

- How 7.0.x reached Play. The `stable` branch already carries the Play Billing flavour (`origin/stable:apps/multiplatform/android/build.gradle.kts:38-42`). If it shipped as a bundle, Play App Signing and a billing build on a track already exist.
- Play App Signing, if not already enrolled (decision 7). The F-Droid recipe must build `assembleFossRelease` (`apps/multiplatform/README.md:33`).
- The 7.1 release with display grace, its store reviews, and recipients updating.
- Apple's Paid Applications Agreement and Google's payments profile, first, since they likely gate all test purchases.
- Products, created only after the product type is settled (risk 1). Play's multi-quantity off. Some products may already exist (`BadgeStore.kt:31`; `BadgeStore.swift:19-23`), and ids cannot be reused (unverified).
- Subscription products created now fix their group and base-plan structure now. Testing subscriptions (risk 6) can use throwaway ids in a separate group.
- Any existing subscription SKUs deactivated. The app finishes or acknowledges a subscription without crediting it (`BadgeStore.swift:196-199`; `BadgeStore.kt:238-241`), so a buyer would be charged and get nothing.
- Play: a Cloud project, a service-account key and two Console permissions (vb README). The order permission likely also allows refunds (unverified), so the test deployment's key needs production custody.
- Store policy and consumer law on holding a paid purchase behind a shown badge, which step 1 already does.
- App Privacy and Data safety declarations, and EU trader status.
- TestFlight beta review and Play testing-track review for test builds.
- Translations by volunteers on Weblate (`README.md:109`).
- Flux: six preset XFTP servers, three chosen at random (`Operators/Presets.hs:118-127`; `Chat.hs:320-337`). Retention must be configured on all six, and they must accept 2 and 5 GB files, which clients allow from launch (`Badges.hs:259-267`).
- Store review and policy: the first in-app purchase, the US link-out, a web-bought code unlocking a perk, and test purchases in review (decision 1).
- The web checkout `badges.simplex.chat`: DNS and TLS, Stripe activation (needs published terms), a BTCPay Server with Bitcoin and Monero (`badges-rpc.md:33`), merchant and tax registration. Deploying the site itself is ours (`scripts/badge-service/Dockerfile:12-18`).
- Sandbox tests: an account-stable `appTransactionId` (risk 3) and a refund of an unfinished purchase (risk 4).
- A Play licence-tester check that a supporter token under the legend product is refused (risk 2).
- Staff purchases on both stores' production paths after approval, since test purchases are refused (vb `Google.hs:140-145`).

### Step 2: top-up A

- Store review, with a demo profile near paid-through or a screen recording, if the reviewer tries the new Buy entry.
- Store policy and consumer law on forfeiting the overlap (unverified).
- Translations.

### Step 3: refunds

**Ours, all new** (fact: the service has no refund code; the client already decodes refund debits, `Ledger.hs:144`, `:229`).

- A refund debit write path, an Apple notification endpoint, and a voided-purchase poller.
- They write beside the request loop (`Service.hs:160`, `:181`), so they need the Postgres lock.
- A record of refunded transactions, checked at credit. A refund during a hold has no ledger yet (`Service.hs:455-456`), and Apple's JWS shows a refund only if re-delivered (vb `Apple.hs:122`, `:131`).
- A per-payment debit rule for ledgers holding several payments (B, same-ledger subscriptions). The model's refund debits the whole balance (model lines 297, 347), which would wipe months other payments bought.

**External.** Play's Voided Purchases API; Apple's Server Notifications V2 with an API key; Stripe disputes.

### Step 4: subscriptions

- The App Store subscription group: SKUs, ranking, prices, localization, review, grace period.
- Play subscriptions with grace and account-hold settings.
- Family Sharing (decision 5).
- Pub/Sub for push discovery.
- Terms and privacy links, and translations.

## Decisions for the lead

**1. Test purchases in store review.** App Review likely buys with test money (unverified), which is refused, and the app shows "Purchase error" (`BadgeStore.swift:272-276`). Options:

- Accept test purchases. That gives public TestFlight users free badges (inference, `StoreReceipts.hs:34`).
- Accept them, and restrict public TestFlight meanwhile.
- Keep refusing, with clearer copy and a note to reviewers. Whether review accepts that is unverified; a rejection blocks step 1.

**2. A, B, both, or neither before subscriptions?**

- Recommended same-ledger subscriptions need nothing from A, and give store buyers continuity on their own. A is worth building first only if subscriptions are far off.
- B adds to A: one key per badge, no lost overlap, and the model's same-row upgrade.
- B costs: the late-landing fix, a stacking cap or refund handling, months that lapse if the app does not run, and linked payments.

**3. A's window length, and downgrades.**

- A longer window is more convenient, but costs more overlap.
- A legend holder buying a supporter month in a 7-day window gives up about $16 of legend for a $7 month (web prices, `Catalog.hs:109-110`). Refuse, warn, or allow?

**4. Subscribing while a badge is shown?**

- Not at all: subscriptions need nothing from A, and existing holders wait for retirement.
- Store-funded holders: supersede the badge as A does, losing up to a month.
- Multi-month code holders: forfeit the code's remaining months, use B, or wait.

**5. Family Sharing for subscriptions?** It cannot be turned off once on (unverified), and creates claimants on other devices.

**6. Refund clawback in the first release?**

- The draft model requires it (§7).
- It saves nothing on store one-time months, which are issued at credit.
- It saves a month refunded during a hold, or a web purchase of 3 or 12 months.
- The model's other first-release items are also unbuilt: a Stripe statement descriptor, and receipts (`Service.hs:367`).

**7. Play App Signing custody**, if not already enrolled: hand Google the signing key, or reverse the documented bundle path.

## Risks and open questions

Ranked by cost if wrong.

**1. The product type** (unverified).

- Both stores fix it when a product is created. Ids are hard-coded (`Catalog.hs:150-153`; `BadgeStore.kt:28-38`).
- If a month of access must be a non-renewing subscription, restore moves into step 1.
- Settle from the stores' guidance, and check which products exist.

**2. Owned-product rules and product identity** (unverified).

- A held Play purchase may block buying its tier on other devices of the same account (`PlayStore.kt:234`). The same question applies to an unfinished iOS consumable.
- vb trusts the client's product id when Play's record omits it (vb `Google.hs:105-107`, `:124`), so cross-tier integrity rests on Play refusing a mismatched token.
- Settle with a licence tester and a Sandbox account.

**3. Account-stable `appTransactionId`** (unverified). If present, every Apple purchase is linkable anyway, so A's timing linkage matters on Play only.

**4. Refunds during a hold.**

- On Apple, a modified client can withhold the revoked transaction.
- An honest one can also lose a race at launch: the receipt worker may present the stored JWS before the sweep replaces it (`Commands.hs:269`; `SimpleXAPI.swift:2397`).
- Only step 3 closes this. The loss equals today's refund-after-credit baseline.

**5. Synchronised renewal requests** (inference).

- Every request fires a day before the shared weekly expiry, at the same instant on Android and desktop.
- The service answers one at a time (`Service.hs:267-272`) against a 30 s client deadline. Per-request time is unmeasured.
- The queue has no age check, so a backlog serves clients that have already gone.
- Spreading and dropping stale requests (step 1) is the fix; a load test sizes it.

**6. Renewal visibility and revival** (unverified). Whether Play renewals reach the app, which Apple fields persist, and how subscriptions are revived. Settle with test subscriptions.

**7. The late-landing design for same-ledger subscriptions.** The only worked fix is broad. A narrower one, crediting a revival after grace as a fresh purchase, is unexamined.

**8. Open checks.**

- How 7.0.x reached Play.
- Whether a subscription can move between groups.
- Whether App Review accepts "Purchase error".
- Store API quotas.
- Binding a store payment to its purchase key, against another device's claim. The invoice id the apps send is a random UUID any device can read back (`Commands.hs:3590`; `BadgeStore.swift:372`), so this needs a key-derived token and a protocol change.
