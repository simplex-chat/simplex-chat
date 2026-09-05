# SimpleX badge codes — web

The buyer-facing page for badge codes: choose a tier and duration, pay by
card, BTC or XMR, and receive a code. Design and rationale are in
[`plans/badges-codes/2026-08-27-badge-codes.md`](../../../plans/badges-codes/2026-08-27-badge-codes.md);
the implementation plan for this app is
[`plans/badges-codes/2026-08-28-web-implementation.md`](../../../plans/badges-codes/2026-08-28-web-implementation.md).
This file covers how to build, test and run it.

No runtime or dev dependencies beyond `typescript` and `@types/node`.
Stripe.js is loaded at runtime from `js.stripe.com`, never bundled or
installed.

## Build and test

```
npm install
npm run build
npm test
```

`npm run build` clears `build/` and runs `tsc` into it (gitignored), then
`build.js`, which content-hashes the compiled modules, `styles.css` and the images,
copies them to `dist/assets/<hash>/`, and writes that hash into
`public/index.html` and `public/sw.js`. `dist/` (gitignored) is the deploy
tree: `index.html`, `sw.js` and `assets/<hash>/`, nothing else.

`npm test` compiles and runs the suite (`node --test` over
compiled output) but does **not** run `build.js`. One of those tests is a
tripwire: it recomputes the hash from the freshly compiled modules and fails
if the hash named in `public/index.html` or `public/sw.js` does not match.

**If you edit anything under `src/`, run `npm run build` and commit the
regenerated `public/index.html` and `public/sw.js`.** Skipping this is the
most likely way to break the suite — `npm test` will tell you, but the fix is
`npm run build`, not the test.

Both scripts clear `build/` first. `tsc` leaves the output of a module you rename or delete
behind, `build.js` hashes every `.js` it finds there, and the result is a phantom module in the
build that the tripwire would tell you to commit.

## Layout

Only `src/main.ts` and `src/stripe.ts` touch `window`, `location`, `history`
or `localStorage`. `src/screens.ts` builds the DOM, and `src/qr.ts` and
`src/icons.ts` reach for `document` only through `createElementNS`, to build
inline SVG. Nothing else outside those five modules touches the DOM. Every other module
is plain TypeScript, tested directly in Node.

| Module | Owns |
|---|---|
| `domain.ts` | The words the app is written in: steps, themes, order statuses, methods, chains |
| `catalog.ts` | Prices and offers compiled into the page at build time |
| `codes.ts` | The badge code alphabet and check character |
| `parse.ts` | Readers for data the page did not produce, each answering the value or undefined |
| `format.ts` | Money, countdowns and elapsed time as the words a screen prints |
| `store.ts` | The three `localStorage` keys — session, orders and the chosen theme |
| `order.ts` | What an order's state means: what to keep from a response, which screen it selects, and its history row |
| `routing.ts` | Reading `?order=` and the store into what to render |
| `api.ts` | The three routes — create, cancel and read — and the long-poll wait loop, with `fetch`, sleep and the clock injected |
| `flow.ts` | Payment flow control logic — pure, no DOM, no globals |
| `stripe.ts` | Loading Stripe.js, mounting the Payment Element, confirming, and the no-key stand-in |
| `icons.ts` | The badge art, the payment marks and the hamburger, built with `createElementNS`. The brand mark is NOT here: it is a served file |
| `qr.ts` | A QR encoder written for this page, with no dependency, no network and no raster |
| `screens.ts` | Every screen of the spec, and the header and its menu, built node by node — markup is never assigned from a string |
| `main.ts` | Wiring: DOM events in, `flow.ts` calls out, `screens.ts` renders |

## Running the real service against this build

**`mock/server.py` is not the service.** The service is the Haskell
executable `simplex-badge-service`, in `apps/simplex-badge-service/`, and
`dist/` is the directory its listener serves. Nothing about the page changes
between the two — the service matches this build, never the reverse.

```
# 1. build the page. dist/ is the deploy tree: index.html, sw.js, assets/<hash>/
cd apps/simplex-badge-service/web
npm install && npm run build
cd ../../..

# 2. configuration, from the committed example. Copy it IN
#    PLACE, beside the example: that path is the one .gitignore covers, and this
#    file is about to hold a real api_key and webhook_secret.
cd apps/simplex-badge-service
cp badge_service.ini.example badge_service.ini
cd ../..
#   static_dir = ./apps/simplex-badge-service/web/dist   <- the build above
#   [btcpay]   = a real store's host, api_key, store_id and webhook_secret,
#                or delete the whole section to disable BTC and XMR (the provider-unavailable screen)
#   [issuer]   = uncomment the section and put a real issuer secret in key_1, with
#                default = key_1. It has to be a key whose public half already ships in
#                the apps: startup checks the secret against `badgePublicKeys` at that
#                index, so a fresh `simplex-chat badge keygen` pair is refused.

# 3. run it. Without --service-config the web listener does not start at all.
#    An issuer key is required even though nothing in the checkout path uses it:
#    the same process also answers redemption requests, and it refuses to start
#    without a key it could sign a credential with. Put it in the ini's [issuer]
#    section, or pass --issuer-key-idx and --issuer-secret, which override it.
#    `simplex-chat badge keygen` prints a pair.
cabal run simplex-badge-service -- \
  --service-config apps/simplex-badge-service/badge_service.ini
```

Then open the `[listener]` host and port — `http://127.0.0.1:8080/` as the
example ships. `static_dir` is relative to the working directory, so run from
the repository root or make it absolute.

**The executable is the whole badge service, not just this listener.**
`badgeService` starts `simplexChatCore` unconditionally and the web listener
is one lane beside the chat one, so a local run opens or creates a chat
database (`printDbOpts` names the path at startup) and does agent network
work. `--no-address` skips creating the service's contact address on first
start, which a run of the web listener alone does not need.

*These three steps are read out of `Options.hs`, `Service.hs`, `Config.hs` and
`badge_service.ini.example` rather than from a run — driving them end to end
needs a chat database and a real BTCPay store.*

What the mock cannot stand in for:

| | `mock/server.py` | `simplex-badge-service` |
|---|---|---|
| Invoices | invented in memory | created at BTCPay over Greenfield |
| Payment detected by | `POST /control/settle/:id`, an endpoint that exists nowhere else | a poller reading the provider, which is the only thing that carries authority |
| Webhooks | none | `POST /webhooks/btcpay`, signature-verified, a latency hint and nothing more |
| Persistence | none | the chat database, under `sx_badge_service_*` |
| Codes | invented | written unpaid at checkout, marked paid by settlement |

The Haskell side's own end-to-end coverage of that lane — checkout, payment,
polling, partial payment, expiry, late settlement and replay, all against a
fake Greenfield — is `tests/Bots/BadgeWebTests.hs`:

```
cabal test --test-options='-m "Supporter badges"'
```

## Running the mock

`mock/server.py` stands in for the Haskell service, Stripe and BTCPay, so the
whole browser flow can be driven with no real backend. **It is a browser-only
test fixture and not the service**: no signatures, no persistence, no real
money, no provider, and not a specification of what ships. Standard library
only.

```
python3 mock/server.py --port 8099
```

It serves `public/` and `dist/`, and adds:

| Endpoint | Behaviour |
|---|---|
| `POST /api/invoice` | Creates an invoice. Rejects a repeated `codeHash` with `409 code_conflict`. |
| `GET /api/invoice/:id?wait=<status>&seenPaid=<figure>&seenFull=<0\|1>` | Long-polls: holds while the invoice's status is still `<status>` **and** its payment is the one the page says it has rendered, up to `MOCK_HOLD_SECONDS` (default 30). A payment the page has not seen answers at once — the provider's verdict counts as much as the figure, since Monero reports an invoice as confirming while its figures are still zero. A request that omits `seenPaid` holds on the status alone. |
| `POST /api/invoice/:id/cancel` | Expires an open invoice. Refuses a settled one with `409 not_open` and a funded one with `409 funded`, as the service does. |
| `POST /control/settle/:id` | Marks the invoice paid — stands in for a provider webhook. |
| `POST /control/expire/:id` | Marks it expired. |
| `POST /control/partial/:id` | Records a partial payment, half the amount due, and the remainder the provider would still ask for. |
| `POST /control/confirming/:id` | Records the full amount as arrived with the invoice still open — the screen that waits for confirmations. |
| `POST /control/verdict/:id` | Records the provider's verdict alone, with no figure — how Monero reports a payment it is still confirming. |

### Driving a purchase by hand

```
python3 mock/server.py --port 8099 &

curl -s -X POST http://localhost:8099/api/invoice -H 'content-type: application/json' \
  -d '{"codeHash":"<43 base64url chars>","priceId":"price_supporter","offerId":"offer_3m","method":"btc"}'
# => {"invoiceId": "...", "status": "open", ...}

curl -s -X POST http://localhost:8099/control/settle/<invoiceId>
# => {"ok": true, "status": "paid"}

curl -s "http://localhost:8099/api/invoice/<invoiceId>?wait=open"
# returns immediately once settled, with status "paid"
```

A GET with `wait=open` held while `open` is still current unblocks the
instant `/control/settle` (or `/control/expire`, `/control/partial`) fires,
which is what the page's own wait loop relies on — no polling on a timer.

Card invoices (`"method":"card"`) get a `clientSecret` in the response
instead of an address; BTC and XMR get `address` and `cryptoAmount`.

### The Stripe key

The publishable key lives in a `<meta id="stripe-publishable-key">` element
in `public/index.html`, committed empty. `mock/server.py` substitutes
`$STRIPE_PUBLISHABLE_KEY` into the served page and refuses to start if it is
set to anything but a `pk_`-prefixed key (a secret or restricted key would
otherwise be baked into a page anyone can read).

With no key set, the card path renders a labelled development stand-in
instead of a Stripe Payment Element: its button does what a successful
confirm does, and settling it calls the mock's `/control/settle` directly.
**This stand-in cannot appear when a key is set** — the code path that
builds it is unreachable once Stripe.js has actually loaded. To see the real
Payment Element, set a test key:

```
STRIPE_PUBLISHABLE_KEY=pk_test_... python3 mock/server.py --port 8099
```

## What is not verified here

The test suite runs in Node, so it asserts structure rather than rendering:
CSS is checked by parsing `styles.css`, `inert` by the attribute, the QR by
decoding the path the encoder produced, and the service worker by driving
`public/sw.js` in a Node `vm`.

The screens themselves **have** been rendered and reviewed, in both themes at
desktop and phone widths, using headless Chromium driven by Playwright
installed outside this package — the whole purchase was walked through,
settled against the mock, and photographed. Nothing in `package.json`
changed; do the same rather than trusting the suite for anything visual.

What still cannot be checked anywhere here:

- A real QR scan with a phone camera.
- Stripe.js loading, mounting a Payment Element and confirming a payment,
  which need a browser **and** a Stripe account. The SDK method names come
  from the spec and every test drives a fake, so a wrong name stays green here
  and fails in production.
- A real service worker installing and serving the precache with the network
  genuinely off.
- System fonts: headless Chromium substitutes DejaVu, so type metrics differ
  from a real machine.

Before shipping a change that touches any of these, check it by hand in a
real browser: the wizard's two panels travel together for the length of a
step and neither one blinks out, panels ahead of the buyer are unreachable by
Tab, the menu closes on Escape and hands focus back to its button, both
themes are drawn from the menu's own control as well as from the operating
system, offline reload actually serves the cached build, and (with a test
key) the Payment Element mounts and confirms.

## What is not implemented

The web manifest is absent. The design calls it optional: offline support
needs the service worker, not the manifest.

The asset table is otherwise built. The brand mark is the difference worth
knowing about: `public/img/wordmark-*.svg` and `public/img/symbol-*.svg` are
the official files copied out of `website/` and `media-logos/`, served under
the build hash and drawn by the stylesheet — NOT path data transcribed into a
module. `design.test.ts` compares each one against its source byte for byte,
so a hand-edited mark fails the suite. The one edit is the dark wordmark,
which is the light file with its `#030749` lettering set to white, exactly as
simplex.chat's own dark header shows it.

## Content Security Policy

This repository has no CSP — it belongs on the listener that serves the app
in production, not in this app. Without one, in
particular without `script-src https://js.stripe.com`, card payments break
silently and nothing in this test suite would catch it. The policy the spec
specifies:

```
default-src 'self';
script-src  'self' https://js.stripe.com https://*.js.stripe.com;
frame-src   https://js.stripe.com https://*.js.stripe.com https://hooks.stripe.com;
connect-src 'self' https://api.stripe.com;
img-src     'self' https://*.stripe.com
```
