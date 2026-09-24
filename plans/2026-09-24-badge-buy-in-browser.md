# Buying a badge in the browser

The web page already sells badge codes. This makes the app the way in without making the app a shop: the app opens the page, the page runs the checkout it already has, and at the end hands the code back through a deep link instead of putting it on screen. The buyer never sees a code, and the code purchase itself is untouched.

Wireframes: `plans/sketches/2026-09-23-badge-in-app-checkout.excalidraw`. Panel ids below (A1, B3, D2…) refer to it, and the screens are drawn there rather than described again here.

## Scope

**Part 1** is everything in the wireframes except recurring payments: the browser lane on both builds, and the store lane's one-time product. Monthly and Annual stay on B3 and keep exactly the behaviour they have now — a real store transaction followed by a diagnostic alert, nothing sent anywhere. They must not reach the service, which cannot yet add months to a badge that already exists.

The two lanes of part 1 are independent and unequal: the browser lane needs nothing from the service and can ship on its own, while the store lane cannot start until `purchaseBadge` accepts store receipts.

**Part 2** is subscriptions, outlined at the end.

Everything in the app sections below is both platforms unless it says otherwise.

## Order of work

1. **The renames**, on their own. Both screens are unreachable today, so nothing visible changes and the diff is strings and two titles.
2. **The page** — the `app` parameter, the second-screen start, both endings. Nothing sends the flag yet, so this is inert whenever it lands. It sits on the unmerged badge-service branch, which decides where it gets committed.
3. **The app's browser lane** — Buy in browser, the link handler, A6, and the desktop pair. End to end once 2 is deployed; before that it degrades to today's flow, where the buyer carries the code by hand.
4. **`purchaseBadge` with store receipts**, which depends on none of the above and can run alongside it.
5. **The app's store lane**, last, because nothing in it completes without 4.

(*) **Desktop deep links** sit outside this order entirely — any time or never, blocking nothing. Own section below.

## The page

Three changes, all in `apps/simplex-badge-service/web`.

**Read an `app` flag.** `#app=true` for mobile apps, `#app=desktop` for desktop, on the page's only path, `/`. It goes in the fragment rather than the query because the fragment is never sent: the service learns nothing about which visitors came from the app, and it is stripped from the `Referer` when the page hands off to a payment provider. Nothing server-side needs it — both things it decides are rendering, from data the browser already holds.

**Start at the second screen when it is set.** The app has just shown Support SimpleX, so showing the page's own version of it is a duplicate. Preserve the fragment when the page rewrites the URL for an order, so a reload keeps it; an order resumed in some other browser loses it and falls back to the plain page, which is fine.

**Replace the final screen for `app=true` (A5).** Try the deep link on load, then offer *Return to SimpleX* with *Show code* under it. `Show code` reveals the existing `codeIssued` content, unchanged — which means the code must still be minted and saved in the browser exactly as now. For `app=desktop` (D2), keep `codeIssued` as it is and change only the "Redeem it in the app" line to say the screen is already open. With no flag, nothing changes.

## The link

`simplexchat:/badge/code/<code>` — a scheme of its own, not a path added to `simplex:`. That costs a registration on every platform, including the two where `simplex:` was already free, and buys a badge link that cannot be mistaken for a connection link and an existing dispatch left alone: the URL arrives at the same entry point the app already has, branches on scheme before the path switch that feeds `planAndConnect`, and never reaches it.

Two things the code must get right:

The link is a bearer token. Anything that can read it can redeem the badge — no worse than a code on screen, but it must not be logged. iOS logs every incoming URL in `SimpleXApp.onOpenURL` today and Kotlin should be checked for the same; those lines have to stop short of the code. Any web page can send one, so the screen it opens should do nothing but redeem.

A link may arrive when no profile is active, or for a different profile than the one that bought it. Redeem into the active profile, as the redeem screen does, and let the existing errors speak if the code is spent.

## The app: non-store (A1, A6)

On Support SimpleX, *Get your code* becomes the primary button *Buy in browser* and *Redeem badge code* drops to the text button. The tap must open the browser directly, not through `openExternalLink`, which alerts for anything that is not a SimpleX connection link. The info button stays as it is: the TODO in that file proposing to swap it for *Why SimpleX is built* should not be followed. `chooseLevelButton()` is never called in these builds — there is no in-app purchase here at all.

A6 is the one new screen: a title, a spinner, and a call to the existing redeem API with the code from the link. It is reachable only from the link, never from navigation, and a link can arrive from anywhere in the app — present it the way the badge sheet is already presented from an alert. Its failures are the redeem screen's failures and should use the same alerts. On success the app lands on Your badge by the path it already takes.

## The app: store builds (B1–B5)

B1's primary becomes *Choose your badge* — `chooseLevelButton()`, which already exists and is called by nothing. Its text button is *Buy in browser* where the store rules allow it and *Redeem code* where they do not, so the slot is never empty and codes are always reachable. Define that predicate once and use it for both the button and B2's — the rule is expected to be wider than the crowdfunding banner's and is not settled, so it should be one thing to change later.

B2 (`BadgesYourLevelView`) gains *Redeem code* only when B1's slot is taken by *Buy in browser*. The privacy link stays on B1 in both builds and does not move here.

B3 (`BadgesPayView`) loses its Pay button to *Continue*; `purchase()` moves to B4, which is new. Three of B5's outcomes need building: a verified transaction becomes a funded-not-issued state, `.pending` becomes an awaiting-the-store state, and an unverified one is refused with an alert and never sent. Both of those states belong on `BadgeState` and both must hold the Support SimpleX slot, because a consumable can otherwise be bought twice. Cancelled does nothing, and other billing errors keep today's alert.

Two invariants that are easy to miss. A store transaction must not be finished until the credential is stored — `BadgeStore` says so in a comment already and does the opposite because nothing is delivered yet. And both platforms need reconciliation at launch (`Transaction.updates`, `queryPurchasesAsync`), because an outcome can arrive while the app is closed.

## Desktop (D1, D2)

No deep link. *Buy in browser* opens the page with `app=desktop` and the Redeem code screen at the same time, so the code has somewhere to go the moment it appears. This is also the fallback for any platform where the scheme turns out not to work: nothing else in the app depends on the link existing.

## Desktop deep links (*)

Optional, off the critical path, and blocking nothing — D1 works without it. Worth investigating on its own; if it lands, desktop joins lane A and D1 becomes the fallback rather than the design.

Nothing registers a scheme on desktop today. `appOpenUrl` is wired in commonMain with no desktop implementation behind it, and `desktop/build.gradle.kts` declares Deb, Dmg, Msi and Exe with no protocol entry. Registration is per-OS, three separate small jobs: `CFBundleURLTypes` in the bundle plist plus `Desktop.setOpenURIHandler` on macOS, registry keys under `HKCU\Software\Classes` that the installer would have to write on Windows, and a `.desktop` entry with `MimeType=x-scheme-handler/simplexchat` on Linux. None of them covers an unpackaged run, so the fallback stays either way.

Getting the URL to an app that is already running is the part that half exists. `SingleInstance.kt` takes a file lock and uses a watched file to bring the running instance forward on a second launch; carrying a URL means giving that channel a payload rather than inventing IPC for it.

## The service

The browser lane needs nothing. The store lane needs `purchaseBadge` to accept a store receipt and verify it — Apple's JWS offline, Google's token through the Publisher API, per `docs/protocol/badges-rpc.md`. No provider adapter for either exists today.

Two properties matter more than the plumbing. It must be idempotent per signing key, since the app will retry. And an unknown or invalid token must be a plain refusal, not a retryable error — Android has no local verification, so tampered clients will send junk tokens as a matter of course, and a retryable answer would leave the worker grinding on one that can never become valid.

`badges-rpc.md` also specifies `getBadgeCatalog` and `getBadgeInvoice`. Neither is needed by anything here and neither should be built: the page owns the catalog and the invoice.

## Naming

Two renames: *Your level* becomes *Choose your badge* with the page's lede, and the duration screen's title stops being the level name and becomes *How long?* with the chosen badge on the line below. This is the page's vocabulary; the app adopts it so the two halves of one purchase read the same.

No type or file is renamed — `BadgesYourLevelView` keeps its name. Kotlin resource ids do change along with their values: leaving `badges_choose_your_level` in place would keep twelve translations that still say the old thing, and they would go on being shown, since a changed base value does not invalidate them. Renaming the id retires those translations and gets the string translated again, which is also what iOS does for free, its literals being their own keys. An id removed from the base resources has to be removed from each translation file too, or the build fails formatting them — though no `badges_*` string has been through a translation round yet, so today `base/strings.xml` is the only file that holds any and this costs nothing. It stops being free the moment they are translated.

## Tests

The page has screen and routing tests; extend those rather than starting a parallel suite, and cover all three cases, including no flag at all. No UI tests on the app side — verify those by review.

The store purchase needs end-to-end coverage and `BadgeServiceTests` is already the place: it runs a real client against a real service over SMP and covers redeeming, issuing, renewal and the worker. Everything from `APIPurchaseBadge` rightwards belongs there, with the receipt an opaque blob and verification behind a test provider that accepts one known token — the same seam the crypto and card providers already sit behind. Worth covering: a valid receipt issues a credential; the same receipt twice returns that same credential; an unknown token is refused and the refusal is not retryable; a receipt whose purchase key is not the signer is refused, as the code path is already tested for.

The store SDK call itself cannot be automated. StoreKit needs a configuration file on a device, Play needs a real billing connection, and `.pending` needs a sandbox family account with Ask to Buy. That part is QA, and `useBadgeTestProducts` already exists for walking the Kotlin flow without Play Console products.

## Comments

Default to none. Add one only where behaviour or a consequence cannot be carried by names and structure — the transaction-finish ordering, the unlogged code, the retryability rule above are the kind of thing that earns one. Do not comment each function, do not restate the code, and do not reference this plan, a panel id or a ticket in the source.

## Open questions

- **Will a browser follow the scheme without a gesture?** Safari and Chrome both block some automatic scheme navigations, and a blocked one can show an error page rather than nothing. *Return to SimpleX* exists for this; the question is only whether the automatic attempt is worth making.
- **Where the browser button is allowed.** Which markets, and whether the same answer governs both builds.

## Part 2: subscriptions

Monthly and Annual become real purchases. The service side is the work: a subscription renews without the app present, so the badge has to gain months from a provider notification rather than from a `purchaseBadge` call, which means Apple's server notifications and Google's Pub/Sub, and a way to add months to a badge that already exists. The app side is smaller but not only copy: `BadgeState.renewsAt` and `willRenew` are hardcoded to nothing today and have to become real, and `BadgesYourBadgeView`'s Ends section says prepaid months have no billing date, which stops being true and needs a second wording.

The browser lane never gains subscriptions: the page sells prepaid months and says so.
