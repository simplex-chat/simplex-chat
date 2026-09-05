# Badge codes service — invoice API, BTCPay and settlement

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers-extended-cc:subagent-driven-development (recommended) or superpowers-extended-cc:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the service half of §5 and §6 — the two API endpoints, the provider interface, the BTCPay adapter, `settleOrder`, the poller and the webhook route — so that a Bitcoin payment made at a real BTCPay Server marks a browser-drawn code paid, end to end.

**Architecture:** A Warp listener inside the badge service process serves the committed web build and two JSON endpoints. `POST /api/invoice` prices the request from the catalog tables, creates the invoice at the provider, and writes `invoices`, `badge_code_invoices` and an unpaid `badge_codes` row in one transaction. A poller lists open invoices at the provider and is the only thing that ever settles one; a verified webhook merely queues an immediate read. `GET /api/invoice/:id?wait=` holds on an STM waiter that settlement wakes after its transaction commits.

**Tech Stack:** Haskell (GHC 9.6.3, cabal 3.10), `warp`/`wai`/`http-types` for the listener, `http-client`/`http-client-tls` for Greenfield, `aeson`, `crypton`+`memory` for HMAC, `ini` for configuration, `sqlcipher-simple`/`postgresql-simple` through `DBStore`, hspec for tests.

**Spec:** `plans/badges-codes/2026-08-27-badge-codes.md` — §4 (data model), §5 (service API), §6 (providers), §9 (configuration), §10 (operator CLI). Read it beside this plan; where the two disagree the spec wins and this plan is wrong.

## Global Constraints

1. **The browser is fixed. The service matches it.** `apps/simplex-badge-service/web/` is complete and has 407 passing tests. Field names, casing and shapes come from `web/src/api.ts` (`CreateRequest`, `CreatedInvoice`, `InvoiceView`) and from `web/mock/server.py`'s `public_view`. No task may change the web client to suit the service. `cd apps/simplex-badge-service/web && npm test` must still report `pass 407` at the end of every task.
2. **`cryptoCurrency` is lowercase `"btc"` or `"xmr"`** on the wire, matching `InvoiceView` in `web/src/api.ts:104`. BTCPay's own payment-method ids (`BTC-CHAIN`, `XMR-CHAIN`) never reach the browser.
3. **The shared schema block is untouched.** `src/Simplex/Chat/Store/{SQLite,Postgres}/Migrations/M20261001_user_badges.hs` is upstream's and is edited by nobody here. Service-only columns go in a new badge-service migration, which is the only place this plan writes DDL.
4. **Every new service module is added to BOTH `other-modules` lists** in `simplex-chat.cabal`: `executable simplex-badge-service` (`:409`) and `test-suite simplex-chat-test` (`:676`). The test suite compiles the service sources directly (`hs-source-dirs` includes `apps/simplex-badge-service/src`, `:710`), so a module missing from the test stanza fails to link the tests.
5. **Every new test module is added to the test stanza's `other-modules` AND imported into `tests/Test.hs`** under a path containing `Supporter badges` or `Badge service`. A spec needing no chat controller goes under `describe "Supporter badges"` (`tests/Test.hs:66`), which CI runs. A spec that starts the badge service bot goes under `xdescribe'' "SimpleX Badge service bot"` (`:92`), which CI skips (`xdescribe''` skips when `CI=true`). A module that compiles but is in no hspec tree never runs and its Verify line passes vacuously — confirm the new spec name appears in the runner output.
6. **Amounts are minor units end to end** (`CurrencyAmount` is a `Word32` of minor units). The only conversion to a decimal major-unit string is at the BTCPay boundary, and it is integer arithmetic — never a `Double`.
7. **`currency` is stored and served lowercase** (`"usd"`, as `web/src/catalog.ts:32` compiles in). BTCPay is sent the uppercase form.
8. **Timestamps are `UTCTime` in Haskell.** SQLite stores ISO-8601 UTC to the second; Postgres stores `TIMESTAMPTZ`. No query may depend on lexicographic text comparison of timestamps, because that is true on one backend only.
9. **No secret is committed.** `badge_service.ini` holds the BTCPay API key and webhook secret; only `badge_service.ini.example` is tracked, and `.gitignore` covers the real file.
10. **Commit style:** subject line alone, `<area>: <subject>`, lowercase, no body. `core:` for Haskell, `plan:` for this file, `web:` for anything under `web/`, comma-separated when a commit spans areas.
11. **Build and test commands:**
    ```bash
    cabal build simplex-badge-service
    cabal test --test-options='-m "Supporter badges" -m "Badge service"'
    ```
    Both `-m` filters are needed: badge specs live under two hspec paths and `--match` is a case-sensitive `isInfixOf` over the full path, OR-ed across repeats. Service-bot specs are skipped when `CI=true`, so CI green is not evidence for them — run them locally.
12. **Stripe is out of scope.** This plan implements the crypto lane only. `POST /api/invoice` with `method: "card"` answers `503 {"error":"provider_unavailable"}`, which is precisely §5.1's designed response for a method whose provider section is absent, and lands the browser on B4b. A test asserts it, so the gap cannot be forgotten.

**User decisions (already made):**
- "sh/badges-codes is deprecated. only sh/badges-codes-new is relevant" — Phases A–D of `plans/badges-codes/2026-08-21-badges-web-checkout.md` are not carried forward; that plan and that branch are dead. Nothing in this plan reads or cherry-picks from them.
- "Start the Haskell service now" — the standing "no Haskell changes" constraint of the web milestone is lifted.
- "You register, I wire it up" — the user supplies a BTCPay instance URL, store id, API key and webhook secret. Every task except Task 15 runs without them.
- "Priority is end-to-end btcpay test" — Task 15 is the point of the plan; Tasks 1–14 exist to make it runnable.

## Spec amendments this plan makes

Each is committed to the spec file in the same commit as the task that needs it.

**A1 (Task 2) — §4.1 gains a fifth service-only column, `provider_ref` on `badge_code_invoices`.** §6.4 orders settlement as *resolve `provider_ref` to an invoice* (step 3) **before** *read the invoice at the provider* (step 5), and §6.5's poller lists provider invoices and must map each back to ours. Neither is possible: `invoices` has no `provider_ref` column (it is upstream's shared block, and §4.1 forbids touching it), and `payments.provider_ref` does not exist until settlement writes the row — the very step that needs the lookup. Without the column a webhook could only be resolved by reading the provider first, which inverts §6.4's order and spends a provider call on every unknown id. `provider_ref TEXT NOT NULL UNIQUE` on the service-only `badge_code_invoices`, written in the creation transaction, is the smallest fix that keeps the shared block untouched.

**A2 (Task 10) — §6.3's payment-method ids are code constants.** §9's configuration table exposes no key for them, so `BTC-CHAIN` and `XMR-CHAIN` are constants in the adapter, logged at startup beside the ids the store actually reports, so a mismatch is visible in the first line of the log rather than in a failed checkout.

---

## File structure

Everything new is under `apps/simplex-badge-service/src/BadgeService/`, except tests.

| File | Responsibility |
|---|---|
| `Config.hs` | `badge_service.ini` (§9): the record, the parser, boot validation. Absent provider section disables its methods; present-but-incomplete is a boot failure |
| `Catalog.hs` | `offerTotal` (§5.1) and the catalog read — the one place a price becomes an amount |
| `Store.hs` | Every SQL statement this service runs: invoice creation, lookups, the settlement writes, the expiry sweep, the operator reads. Statements settlement runs are exported at `DB.Connection` level, so `Orders.hs` can run them inside the one transaction it owns without a second module issuing SQL |
| `Waiters.hs` | The STM structure of §5.2: `publish`, `awaitStatus`, and the count the poller reads |
| `Providers.hs` | The provider-agnostic types of §6.1 — `ProviderError`, `Received`, `PaymentSignal`, `ProviderInvoice`, `OrderDraft` — and the record of operations an adapter fills |
| `Providers/BTCPay.hs` | The BTCPay adapter (§6.3): Greenfield HTTP, decimal handling, status mapping, `BTCPay-Sig` |
| `Orders.hs` | `settleOrder` (§6.4) — the one settlement transaction, called by the poller and by nothing else |
| `Poller.hs` | The list pass and the expiry sweep (§6.5) |
| `Web/Server.hs` | The Warp application: routing, static files, both endpoints, both webhook routes, rate limits |
| `CLI/Codes.hs` | `codes status` and `codes revoke` (§10) |

| Test file | Runs under | Covers |
|---|---|---|
| `tests/Bots/BadgeCatalogTests.hs` | `Supporter badges` (CI) | `offerTotal`, and agreement with the committed `web/src/catalog.ts` |
| `tests/Bots/BadgeWaitersTests.hs` | `Supporter badges` (CI) | `publish`/`awaitStatus`, subscribe-then-read ordering, refcounts |
| `tests/Bots/BadgeWebTests.hs` | `Supporter badges` (CI) | The Warp app over a temporary database, both endpoints, both webhook routes |
| `tests/Bots/BadgeBTCPayTests.hs` | `Supporter badges` (CI) | The adapter against the fake Greenfield, and the fake against the recorded corpus |
| `tests/Bots/BadgeServiceTests.hs` (existing) | `SimpleX Badge service bot` (local) | The full flow with the service bot running |
| `tests/Bots/FakeBTCPay.hs` | — (library module) | The fake Greenfield server the two above drive |

---

## Progress tracker

| Task | Title | Deps | Status |
|---|---|---|---|
| 1 | Dependencies and `badge_service.ini` | — | ☐ |
| 2 | Service migration: the five service-only columns | — | ☐ |
| 3 | `Catalog.hs`: `offerTotal` and catalog reads | 2 | ☐ |
| 4 | `Store.hs`: invoice creation and lookups | 2, 3 | ☐ |
| 5 | `Waiters.hs`: the STM hold | — | ☐ |
| 6 | `Providers.hs`: the adapter interface and a test double | 1 | ☐ |
| 7 | `Web/Server.hs`: listener, routing, `GET /api/invoice/:id` | 1, 4, 5 | ☐ |
| 8 | `POST /api/invoice` and every refusal | 3, 4, 6, 7 | ☐ |
| 9 | `Orders.hs`: `settleOrder` | 4, 5, 6 | ☐ |
| 10 | `Providers/BTCPay.hs`: the Greenfield adapter | 6 | ☐ |
| 11 | `FakeBTCPay.hs` and adapter tests | 10 | ☐ |
| 12 | `Poller.hs`: the list pass and the expiry sweep | 9, 10 | ☐ |
| 13 | `POST /webhooks/btcpay` | 9, 10, 12 | ☐ |
| 14 | Scenario tests: the whole crypto lane | 8, 11, 12, 13 | ☐ |
| 15 | **User gate:** a real BTCPay payment, end to end | 14 | ☐ |

---

## Task 1: Dependencies and `badge_service.ini`

**Goal:** The service executable can link a Warp listener and an HTTP client, and reads its deployment configuration from an ini file, failing at boot on an incomplete section.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Config.hs`
- Create: `apps/simplex-badge-service/badge_service.ini.example`
- Create: `tests/Bots/BadgeConfigTests.hs`
- Modify: `simplex-chat.cabal` (`executable simplex-badge-service` build-depends and `other-modules`; `test-suite simplex-chat-test` the same)
- Modify: `tests/Test.hs` (register `badgeConfigTests` under `describe "Supporter badges"`)
- Modify: `.gitignore`

**Acceptance Criteria:**
- [ ] `cabal build simplex-badge-service` succeeds with `warp`, `wai`, `http-types`, `http-client`, `http-client-tls`, `ini`, `crypton`, `memory`, `case-insensitive`, `containers`, `time`, `scientific` and `base64-bytestring` available to it
- [ ] A complete ini parses to `ServiceConfig` with every §9 default applied: `host` `127.0.0.1`, `port` `8080`, `trust_forwarded_for` off, `expiry_minutes` 60, `speed_policy` `MediumSpeed`, `payment_tolerance` 0.5, `waiting_seconds` 3, `idle_seconds` 60
- [ ] A missing `[btcpay]` section parses successfully and yields `btcpay = Nothing`
- [ ] A `[btcpay]` section missing any one of `host`, `api_key`, `store_id`, `webhook_secret` is `Left`, and the message names the missing key
- [ ] A missing `static_dir` or a missing `base_url` is `Left`; a `base_url` that is not absolute https is `Left`
- [ ] A `[stripe]` section present is `Left`, naming card payments as unimplemented in this build (Global Constraint 12)
- [ ] `speed_policy` accepts the four names and rejects an integer, because §6.3's numeric order is not the speed order
- [ ] `badge_service.ini.example` is tracked; `badge_service.ini` is ignored

**Verify:** `cabal test --test-options='-m "badge service config"'` → the new examples pass, 0 failures

**Steps:**

- [ ] **Step 1: add the dependencies**

In `simplex-chat.cabal`, `executable simplex-badge-service` (`:409`), extend `build-depends` to:

```
  build-depends:
      aeson ==2.2.*
    , base >=4.7 && <5
    , base64-bytestring >=1.0 && <1.3
    , case-insensitive ==1.2.*
    , containers ==0.6.*
    , crypton ==0.34.*
    , directory ==1.3.*
    , filepath ==1.4.*
    , http-client ==0.7.*
    , http-client-tls ==0.3.*
    , http-types ==0.12.*
    , ini ==0.4.1
    , memory ==0.18.*
    , optparse-applicative >=0.15 && <0.17
    , scientific ==0.3.*
    , simple-logger ==0.1.*
    , simplex-chat
    , simplexmq >=6.3
    , stm ==2.5.*
    , time ==1.12.*
    , wai ==3.2.*
    , warp ==3.3.*
```

These exact bounds resolved against this repo's pinned index before, so do not go hunting for versions; if the solver refuses, report it rather than relaxing a bound. The test suite (`:727`) already has `crypton`, `http-types`, `memory`, `wai` and `warp`; add `http-client`, `http-client-tls`, `ini`, `scientific` and `case-insensitive` there too.

Add `BadgeService.Config` to `other-modules` in both stanzas, and `Bots.BadgeConfigTests` to the test stanza's.

- [ ] **Step 2: write the failing test**

```haskell
-- tests/Bots/BadgeConfigTests.hs
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeConfigTests where

import BadgeService.Config
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

badgeConfigTests :: Spec
badgeConfigTests = describe "badge service config" $ do
  it "applies every documented default" testDefaults
  it "disables a provider whose section is absent" testAbsentSection
  it "refuses an incomplete provider section, naming the key" testIncompleteSection
  it "refuses a base_url that is not absolute https" testBaseUrl
  it "refuses a stripe section in this build" testStripeRefused
  it "refuses a numeric speed policy" testSpeedPolicyName

fullIni :: T.Text
fullIni =
  T.unlines
    [ "[listener]",
      "static_dir = /srv/badges",
      "[site]",
      "base_url = https://badges.simplex.chat",
      "[btcpay]",
      "host = https://btcpay.example.org",
      "api_key = token-value",
      "store_id = store-value",
      "webhook_secret = secret-value"
    ]

withIni :: T.Text -> (FilePath -> IO a) -> IO a
withIni t f = withSystemTempDirectory "badge-ini" $ \d -> do
  let p = d </> "badge_service.ini"
  T.writeFile p t
  f p

testDefaults :: IO ()
testDefaults = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  let ListenerConfig {lHost, lPort, lStaticDir, lTrustForwardedFor} = listener cfg
  lHost `shouldBe` "127.0.0.1"
  lPort `shouldBe` 8080
  lStaticDir `shouldBe` "/srv/badges"
  lTrustForwardedFor `shouldBe` False
  let PollConfig {pWaitingSeconds, pIdleSeconds} = poll cfg
  pWaitingSeconds `shouldBe` 3
  pIdleSeconds `shouldBe` 60
  case btcpay cfg of
    Nothing -> expectationFailure "the btcpay section was present"
    Just BTCPayConfig {bExpiryMinutes, bSpeedPolicy, bPaymentTolerance} -> do
      bExpiryMinutes `shouldBe` 60
      bSpeedPolicy `shouldBe` MediumSpeed
      bPaymentTolerance `shouldBe` 0.5

testAbsentSection :: IO ()
testAbsentSection = withIni (T.unlines (take 4 (T.lines fullIni))) $ \p -> do
  Right cfg <- readServiceConfig p
  btcpay cfg `shouldBe` Nothing

testIncompleteSection :: IO ()
testIncompleteSection =
  withIni (T.replace "webhook_secret = secret-value" "" fullIni) $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "webhook_secret"
      Right _ -> expectationFailure "an incomplete btcpay section must fail at boot"

testBaseUrl :: IO ()
testBaseUrl =
  withIni (T.replace "https://badges.simplex.chat" "badges.simplex.chat" fullIni) $ \p ->
    readServiceConfig p >>= (`shouldSatisfy` isLeft)

testStripeRefused :: IO ()
testStripeRefused =
  withIni (fullIni <> "[stripe]\nsecret_key = rk_live_x\n") $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "card payments"
      Right _ -> expectationFailure "a stripe section must fail this build"

testSpeedPolicyName :: IO ()
testSpeedPolicyName =
  withIni (fullIni <> "speed_policy = 2\n") $ \p ->
    readServiceConfig p >>= (`shouldSatisfy` isLeft)
```

Register it in `tests/Test.hs`: `import Bots.BadgeConfigTests` beside the other bot imports, and `describe "Supporter badges" badgeConfigTests` next to `describe "Supporter badges" badgeTests` at `:66`.

- [ ] **Step 3: run it and watch it fail**

Run: `cabal test --test-options='-m "badge service config"'`
Expected: a compile failure — `BadgeService.Config` does not exist.

- [ ] **Step 4: write `Config.hs`**

```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Deployment configuration, read once at boot from badge_service.ini (§9).
-- An absent provider section disables that provider's methods, which the browser
-- renders as B4b; a section present but incomplete stops the service instead of
-- starting one that refuses every payment for a reason nobody can see.
module BadgeService.Config
  ( ListenerConfig (..),
    SiteConfig (..),
    BTCPayConfig (..),
    SpeedPolicy (..),
    PollConfig (..),
    ServiceConfig (..),
    readServiceConfig,
  )
where

import Data.Ini (Ini, lookupValue, readIniFile, sections)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

data ListenerConfig = ListenerConfig
  { lHost :: Text,
    lPort :: Int,
    lStaticDir :: FilePath,
    lTrustForwardedFor :: Bool
  }
  deriving (Eq, Show)

newtype SiteConfig = SiteConfig {sBaseUrl :: Text}
  deriving (Eq, Show)

-- | §6.3: send the NAME, never the integer — the numeric order is not the speed order,
-- so an encoded 2 means six confirmations rather than two.
data SpeedPolicy = HighSpeed | MediumSpeed | LowMediumSpeed | LowSpeed
  deriving (Eq, Show)

data BTCPayConfig = BTCPayConfig
  { bHost :: Text,
    bApiKey :: Text,
    bStoreId :: Text,
    bWebhookSecret :: Text,
    bExpiryMinutes :: Int,
    bSpeedPolicy :: SpeedPolicy,
    bPaymentTolerance :: Double
  }
  deriving (Eq, Show)

data PollConfig = PollConfig {pWaitingSeconds :: Int, pIdleSeconds :: Int}
  deriving (Eq, Show)

data ServiceConfig = ServiceConfig
  { listener :: ListenerConfig,
    site :: SiteConfig,
    btcpay :: Maybe BTCPayConfig,
    poll :: PollConfig
  }
  deriving (Eq, Show)

readServiceConfig :: FilePath -> IO (Either String ServiceConfig)
readServiceConfig path = fmap (>>= parseConfig) (readIniFile path)

parseConfig :: Ini -> Either String ServiceConfig
parseConfig ini = do
  refuseStripe
  lStaticDir <- T.unpack <$> required "listener" "static_dir"
  lHost <- optional "listener" "host" "127.0.0.1"
  lPort <- num "listener" "port" 8080
  lTrustForwardedFor <- bool "listener" "trust_forwarded_for" False
  baseUrl <- required "site" "base_url"
  _ <- checkBaseUrl baseUrl
  btc <- btcpaySection
  pWaitingSeconds <- num "poll" "waiting_seconds" 3
  pIdleSeconds <- num "poll" "idle_seconds" 60
  pure
    ServiceConfig
      { listener = ListenerConfig {lHost, lPort, lStaticDir, lTrustForwardedFor},
        site = SiteConfig {sBaseUrl = baseUrl},
        btcpay = btc,
        poll = PollConfig {pWaitingSeconds, pIdleSeconds}
      }
  where
    hasSection s = s `elem` sections ini
    look s k = either (const Nothing) Just (lookupValue s k ini)
    required s k = case look s k of
      Just v | not (T.null (T.strip v)) -> Right (T.strip v)
      _ -> Left (T.unpack s <> "." <> T.unpack k <> " is required")
    optional s k d = Right (maybe d T.strip (look s k))
    num s k d = case look s k of
      Nothing -> Right d
      Just v -> maybe (Left (T.unpack k <> " must be a whole number")) Right (readMaybe (T.unpack (T.strip v)))
    bool s k d = case fmap (T.toLower . T.strip) (look s k) of
      Nothing -> Right d
      Just "on" -> Right True
      Just "off" -> Right False
      Just other -> Left (T.unpack k <> " must be on or off, not " <> T.unpack other)
    -- Absolute and https, because it is the origin a provider is told to return to.
    checkBaseUrl u
      | "https://" `T.isPrefixOf` u = Right u
      | otherwise = Left "site.base_url must be an absolute https URL"
    refuseStripe
      | hasSection "stripe" =
          Left "the [stripe] section is not supported: card payments are not implemented in this build"
      | otherwise = Right ()
    btcpaySection
      | not (hasSection "btcpay") = Right Nothing
      | otherwise = do
          bHost <- required "btcpay" "host"
          bApiKey <- required "btcpay" "api_key"
          bStoreId <- required "btcpay" "store_id"
          bWebhookSecret <- required "btcpay" "webhook_secret"
          bExpiryMinutes <- num "btcpay" "expiry_minutes" 60
          bSpeedPolicy <- speedPolicy
          bPaymentTolerance <- tolerance
          pure (Just BTCPayConfig {bHost, bApiKey, bStoreId, bWebhookSecret, bExpiryMinutes, bSpeedPolicy, bPaymentTolerance})
    speedPolicy = case look "btcpay" "speed_policy" of
      Nothing -> Right MediumSpeed
      Just v -> case T.strip v of
        "HighSpeed" -> Right HighSpeed
        "MediumSpeed" -> Right MediumSpeed
        "LowMediumSpeed" -> Right LowMediumSpeed
        "LowSpeed" -> Right LowSpeed
        other -> Left ("btcpay.speed_policy must be one of HighSpeed, MediumSpeed, LowMediumSpeed, LowSpeed, not " <> T.unpack other)
    tolerance = case look "btcpay" "payment_tolerance" of
      Nothing -> Right 0.5
      Just v -> case readMaybe (T.unpack (T.strip v)) of
        Just d | d >= 0 && d <= 100 -> Right d
        _ -> Left "btcpay.payment_tolerance must be a percentage between 0 and 100"
```

- [ ] **Step 5: write the example ini and ignore the real one**

`apps/simplex-badge-service/badge_service.ini.example`:

```ini
[listener]
; bind locally; the reverse proxy terminates TLS and drops the query string
host = 127.0.0.1
port = 8080
; required: the directory holding the committed web build
static_dir = ./apps/simplex-badge-service/web/dist
trust_forwarded_for = off

[site]
; required: absolute https origin this service is served from
base_url = https://badges.simplex.chat

; omit this whole section to disable Bitcoin and Monero (the browser shows B4b)
[btcpay]
host = https://btcpay.example.org
api_key = replace-me
store_id = replace-me
webhook_secret = replace-me
expiry_minutes = 60
speed_policy = MediumSpeed
payment_tolerance = 0.5

[poll]
waiting_seconds = 3
idle_seconds = 60
```

Add to `.gitignore`:

```
apps/simplex-badge-service/badge_service.ini
```

- [ ] **Step 6: run the tests**

Run: `cabal test --test-options='-m "badge service config"'`
Expected: 6 examples, 0 failures, and the runner prints `badge service config`.

- [ ] **Step 7: commit**

```bash
git add simplex-chat.cabal tests/Test.hs tests/Bots/BadgeConfigTests.hs \
  apps/simplex-badge-service/src/BadgeService/Config.hs \
  apps/simplex-badge-service/badge_service.ini.example .gitignore
git commit -m "core: read the badge service deployment configuration"
```

---

## Task 2: Service migration — the six service-only columns

**Goal:** The service schema carries everything §4.1 needs that upstream's shared block does not: the support reference, the link from an invoice to its code, the provider's own invoice id, the crypto amount received, and a code's expiry and revocation.

**Files:**
- Modify: `apps/simplex-badge-service/src/BadgeService/Store/SQLite/Migrations.hs`
- Modify: `apps/simplex-badge-service/src/BadgeService/Store/Postgres/Migrations.hs`
- Modify: `plans/badges-codes/2026-08-27-badge-codes.md` (§4.1 — record amendment A1)
- Test: `tests/Bots/BadgeWebTests.hs` (created here with the schema assertion; extended by later tasks)
- Modify: `simplex-chat.cabal`, `tests/Test.hs` — `Bots.BadgeWebTests` is registered here, under `describe "Supporter badges"` (Global Constraint 5), or the test suite does not link

**Acceptance Criteria:**
- [ ] A new migration `20260831_badge_service_web` adds, all with the `sx_badge_service_` prefix: `badge_code_invoices.support_ref` (`NOT NULL UNIQUE`), `badge_code_invoices.code_hash`, `badge_code_invoices.provider_ref` (`NOT NULL UNIQUE`), `payments.crypto_amount`, `badge_codes.expires_at`, `badge_codes.revoked_at`
- [ ] SQLite declares storage classes on every added column, because `badge_codes` and `badge_code_invoices` are `STRICT`
- [ ] The SQLite and Postgres migrations add the same columns with corresponding types (`TEXT`/`TEXT`, `BLOB`/`BYTEA`, `TEXT`/`TIMESTAMPTZ`)
- [ ] A test opens a migrated temporary database and asserts each of the six columns exists and each unique index refuses a duplicate
- [ ] The shared block in `M20261001_user_badges.hs` is byte-identical to `origin/badges`
- [ ] §4.1 of the spec records amendment A1 with its reason

**Verify:** `cabal test --test-options='-m "badge service schema"'` → passes; `git diff origin/badges -- src/Simplex/Chat/Store/SQLite/Migrations/M20261001_user_badges.hs` → empty

**Steps:**

- [ ] **Step 1: write the failing schema test**

```haskell
-- tests/Bots/BadgeWebTests.hs
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeWebTests where

import Test.Hspec

badgeWebTests :: Spec
badgeWebTests = describe "badge service schema" $
  it "carries the five service-only columns of §4.1" testServiceColumns

-- withServiceStore opens a temporary database, runs badgeServiceSchemaMigrations, and
-- hands back the DBStore. It is written here and reused by every later task in this file.
testServiceColumns :: IO ()
testServiceColumns = withServiceStore $ \st -> do
  columnsOf st "sx_badge_service_badge_code_invoices"
    >>= (`shouldSatisfy` \cs -> all (`elem` cs) ["support_ref", "code_hash", "provider_ref"])
  columnsOf st "sx_badge_service_payments" >>= (`shouldSatisfy` elem "crypto_amount")
  columnsOf st "sx_badge_service_badge_codes"
    >>= (`shouldSatisfy` \cs -> all (`elem` cs) ["expires_at", "revoked_at"])
```

`columnsOf` reads the backend's own catalogue — `PRAGMA table_info` on SQLite, `information_schema.columns` on Postgres — behind one CPP switch, so the assertion is about the shipped schema rather than about the migration text.

- [ ] **Step 2: run it, expect failure**

Run: `cabal test --test-options='-m "badge service schema"'`
Expected: FAIL — the columns do not exist.

- [ ] **Step 3: add the SQLite migration**

In `apps/simplex-badge-service/src/BadgeService/Store/SQLite/Migrations.hs`, append to `schemaMigrations` and add the two definitions:

```haskell
schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("20260806_badge_service_schema", m20260806_badge_service_schema, Just down_m20260806_badge_service_schema),
    ("20260831_badge_service_web", m20260831_badge_service_web, Just down_m20260831_badge_service_web)
  ]

m20260831_badge_service_web :: Query
m20260831_badge_service_web =
  withPrefix
    servicePrefix
    [sql|
ALTER TABLE @badge_code_invoices ADD COLUMN support_ref TEXT;

ALTER TABLE @badge_code_invoices ADD COLUMN code_hash BLOB;

ALTER TABLE @badge_code_invoices ADD COLUMN provider_ref TEXT;

CREATE UNIQUE INDEX @idx_badge_code_invoices_support_ref ON @badge_code_invoices(support_ref);

CREATE UNIQUE INDEX @idx_badge_code_invoices_provider_ref ON @badge_code_invoices(provider_ref);

ALTER TABLE @payments ADD COLUMN crypto_amount TEXT;

ALTER TABLE @badge_codes ADD COLUMN expires_at TEXT;

ALTER TABLE @badge_codes ADD COLUMN revoked_at TEXT;
|]

down_m20260831_badge_service_web :: Query
down_m20260831_badge_service_web =
  withPrefix
    servicePrefix
    [sql|
DROP INDEX @idx_badge_code_invoices_support_ref;
DROP INDEX @idx_badge_code_invoices_provider_ref;
ALTER TABLE @badge_code_invoices DROP COLUMN provider_ref;
ALTER TABLE @badge_code_invoices DROP COLUMN code_hash;
ALTER TABLE @badge_code_invoices DROP COLUMN support_ref;
ALTER TABLE @payments DROP COLUMN crypto_amount;
ALTER TABLE @badge_codes DROP COLUMN revoked_at;
ALTER TABLE @badge_codes DROP COLUMN expires_at;
|]
```

`ALTER TABLE … ADD COLUMN` on SQLite cannot add a `NOT NULL` column without a default, so the three that §4.1 calls `NOT NULL` are declared nullable and made not-null by the unique indexes plus the single writer: `createInvoiceRows` (Task 4) is the only statement that inserts a `badge_code_invoices` row and always supplies all three. Say so in a comment above the migration — a reader must not think the constraint was forgotten.

- [ ] **Step 4: add the Postgres migration**

The same columns in `apps/simplex-badge-service/src/BadgeService/Store/Postgres/Migrations.hs`, with `BYTEA` for `code_hash` and `TIMESTAMPTZ` for `expires_at` and `revoked_at`:

```haskell
m20260831_badge_service_web :: Text
m20260831_badge_service_web =
  withPrefix
    servicePrefix
    [r|
ALTER TABLE @badge_code_invoices ADD COLUMN support_ref TEXT;
ALTER TABLE @badge_code_invoices ADD COLUMN code_hash BYTEA;
ALTER TABLE @badge_code_invoices ADD COLUMN provider_ref TEXT;
CREATE UNIQUE INDEX @idx_badge_code_invoices_support_ref ON @badge_code_invoices(support_ref);
CREATE UNIQUE INDEX @idx_badge_code_invoices_provider_ref ON @badge_code_invoices(provider_ref);
ALTER TABLE @payments ADD COLUMN crypto_amount TEXT;
ALTER TABLE @badge_codes ADD COLUMN expires_at TIMESTAMPTZ;
ALTER TABLE @badge_codes ADD COLUMN revoked_at TIMESTAMPTZ;
|]
```

with the matching `down_` in reverse order.

- [ ] **Step 5: record the amendment in the spec**

In §4.1 of `plans/badges-codes/2026-08-27-badge-codes.md`, change "**Four columns are added in the service migration**" to five and add the row:

```
| `provider_ref` | `badge_code_invoices` | `NOT NULL UNIQUE`. The provider's own invoice id, written at creation. §6.4 resolves a webhook to an invoice *before* reading the provider, and §6.5's poller maps a listed provider invoice back to ours; `invoices` is upstream's block and `payments.provider_ref` does not exist until settlement writes it, which is the step that needs the lookup |
```

- [ ] **Step 6: run the test and the whole badge suite**

Run: `cabal test --test-options='-m "badge service schema" -m "Supporter badges"'`
Expected: the new example passes and no existing badge example regresses.

- [ ] **Step 7: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Store tests/Bots/BadgeWebTests.hs \
  tests/Test.hs simplex-chat.cabal plans/badges-codes/2026-08-27-badge-codes.md
git commit -m "core, plan: add the service-only invoice and code columns"
```

---

## Task 3: `Catalog.hs` — `offerTotal` and catalog reads

**Goal:** One function turns a price and an offer into months, gross and amount, and it agrees exactly with the copy compiled into the browser.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Catalog.hs`
- Create: `tests/Bots/BadgeCatalogTests.hs`
- Modify: `simplex-chat.cabal`, `tests/Test.hs`

**Acceptance Criteria:**
- [ ] `offerTotal` is §5.1's function verbatim in behaviour, including `OfferInvalid`'s four arms
- [ ] The literal totals hold: supporter 700 / 1400 / 4200 and legend 7000 / 14000 / 42000 for 1, 3 and 12 months, against the catalog `web/src/catalog.ts` compiles in
- [ ] `ODFreeMonths 1` on a 3-month offer delivers 3 months and charges 2 × `monthPrice`; `ODDiscount 50` on 12 charges `gross * 50 div 100`
- [ ] The gross is formed in `Word64` and a pair whose charged amount exceeds 100000000 minor units is `Left OIAmountUnsellable` rather than a wrapped `Word32`
- [ ] A test parses the committed `web/src/catalog.ts` and asserts every price, offer and computed total matches what `offerTotal` produces — so the two implementations cannot drift
- [ ] `priceOffer` names which of §5.1's five `catalog_changed` conditions it refused on, so the arm taken can be logged

**Verify:** `cabal test --test-options='-m "badge catalog"'` → passes, including the drift check

**Steps:**

- [ ] **Step 1: write the failing tests**

```haskell
-- tests/Bots/BadgeCatalogTests.hs
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeCatalogTests where

import BadgeService.Catalog
import Data.Word (Word8)
import Simplex.Chat.Badges.Service (BadgeOffer (..))
import Simplex.Chat.Badges.Types (BadgeItemStatus (..), BadgeOfferId (..), OfferDiscount (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Test.Hspec

badgeCatalogTests :: Spec
badgeCatalogTests = describe "badge catalog" $ do
  it "prices the six sellable pairs at their literal totals" testLiteralTotals
  it "delivers the free months it does not charge for" testFreeMonths
  it "truncates a percentage discount in the buyer's favour" testDiscountTruncates
  it "refuses a pair it cannot price" testRefusals
  it "agrees with the catalog compiled into the page" testNoDriftFromWeb

offer :: Word8 -> OfferDiscount -> BadgeOffer
offer months discount =
  BadgeOffer {offerId = BadgeOfferId "o", priceId = Nothing, months, discount, status = BISActive, createdAt = epoch}

testLiteralTotals :: IO ()
testLiteralTotals = do
  amountOf 700 Nothing `shouldBe` Right 700
  amountOf 700 (Just (offer 3 (ODFreeMonths 1))) `shouldBe` Right 1400
  amountOf 700 (Just (offer 12 (ODDiscount 50))) `shouldBe` Right 4200
  amountOf 7000 Nothing `shouldBe` Right 7000
  amountOf 7000 (Just (offer 3 (ODFreeMonths 1))) `shouldBe` Right 14000
  amountOf 7000 (Just (offer 12 (ODDiscount 50))) `shouldBe` Right 42000
  where
    amountOf p o = (\(_, _, CurrencyAmount a) -> a) <$> offerTotal (CurrencyAmount p) o

testFreeMonths :: IO ()
testFreeMonths =
  offerTotal (CurrencyAmount 7000) (Just (offer 3 (ODFreeMonths 1)))
    `shouldBe` Right (3, CurrencyAmount 21000, CurrencyAmount 14000)

testDiscountTruncates :: IO ()
testDiscountTruncates =
  -- 333 * 3 = 999; 999 * 67 div 100 = 669, not 669.33 — the fraction goes to the buyer
  offerTotal (CurrencyAmount 333) (Just (offer 3 (ODDiscount 33)))
    `shouldBe` Right (3, CurrencyAmount 999, CurrencyAmount 669)

testRefusals :: IO ()
testRefusals = do
  offerTotal (CurrencyAmount 700) (Just (offer 0 (ODDiscount 10))) `shouldBe` Left OIZeroMonths
  offerTotal (CurrencyAmount 700) (Just (offer 3 (ODFreeMonths 3))) `shouldBe` Left OIFreeMonthsExceedTerm
  offerTotal (CurrencyAmount 700) (Just (offer 3 (ODDiscount 100))) `shouldBe` Left OIDiscountTooLarge
  offerTotal (CurrencyAmount 0) Nothing `shouldBe` Left OIAmountUnsellable
  -- above the $1,000,000 bound: 200000 * 12 = 2400000... in minor units, 240000000 > 100000000
  offerTotal (CurrencyAmount 20000000) (Just (offer 12 (ODDiscount 50))) `shouldBe` Left OIAmountUnsellable
```

`testNoDriftFromWeb` reads `apps/simplex-badge-service/web/src/catalog.ts`, extracts the `CATALOG` literal with a small parser (the file's shape is fixed: one object literal with `prices` and `offers` arrays of flat records), and for every price/offer pair asserts `offerTotal` returns the amount the browser would render. It fails loudly if the literal cannot be parsed, so a reshaped `catalog.ts` is a failing test rather than a silently skipped one.

- [ ] **Step 2: run, expect failure**

Run: `cabal test --test-options='-m "badge catalog"'`
Expected: compile failure — `BadgeService.Catalog` does not exist.

- [ ] **Step 3: write `Catalog.hs`**

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pricing (§5.1). This is the only place a catalog row becomes an amount, and the
-- same arithmetic is compiled into the page, so the figure shown on B3 and the figure
-- charged cannot drift — a test compares the two.
module BadgeService.Catalog
  ( OfferInvalid (..),
    CatalogRefusal (..),
    offerTotal,
    PricedOffer (..),
    priceOffer,
  )
where

import Data.Word (Word64, Word8)
import Simplex.Chat.Badges (BadgeType)
import Simplex.Chat.Badges.Service (BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types (BadgeItemStatus (..), BadgeOfferId, BadgePriceId, OfferDiscount (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))

-- | Why a catalog row cannot be priced. Logged, never rendered: every arm lands on B4c.
data OfferInvalid = OIZeroMonths | OIFreeMonthsExceedTerm | OIDiscountTooLarge | OIAmountUnsellable
  deriving (Eq, Show)

-- | Months delivered, gross price, and amount charged, in minor units.
-- invoices.discount_amount is price - amount. Integer arithmetic throughout: the gross
-- is formed first and the division is last, so nothing intermediate is rounded.
offerTotal :: CurrencyAmount -> Maybe BadgeOffer -> Either OfferInvalid (Word8, CurrencyAmount, CurrencyAmount)
offerTotal (CurrencyAmount p) = \case
  Nothing -> charge 1 (gross 1)
  Just BadgeOffer {months, discount}
    | months == 0 -> Left OIZeroMonths
    | otherwise -> case discount of
        ODFreeMonths f
          | f >= months -> Left OIFreeMonthsExceedTerm
          | otherwise -> charge months (gross (months - f))
        ODDiscount d
          | d >= 100 -> Left OIDiscountTooLarge
          | otherwise -> charge months (gross months * (100 - fromIntegral d) `div` 100)
  where
    -- formed in Word64, above CurrencyAmount's Word32, so it cannot wrap
    gross :: Word8 -> Word64
    gross m = fromIntegral p * fromIntegral m
    maxAmount :: Word64 -- $1,000,000 in minor units
    maxAmount = 100000000
    -- the only bound, and it is on the amount charged rather than on the gross
    charge :: Word8 -> Word64 -> Either OfferInvalid (Word8, CurrencyAmount, CurrencyAmount)
    charge m c
      | c == 0 || c > maxAmount = Left OIAmountUnsellable
      | otherwise = Right (m, CurrencyAmount (fromIntegral (gross m)), CurrencyAmount (fromIntegral c))

-- | What POST /api/invoice needs after pricing: the derived facts a request may not state.
data PricedOffer = PricedOffer
  { poBadgeType :: BadgeType,
    poMonths :: Word8,
    poPrice :: CurrencyAmount,
    poAmount :: CurrencyAmount,
    poCurrency :: Text
  }
  deriving (Eq, Show)

-- | The five conditions §5.1 answers with catalog_changed. Distinct arms because the arm
-- taken is logged: "catalog_changed" on its own cannot tell a stale page from a bad request.
data CatalogRefusal
  = CRUnknownPrice
  | CRDisabledPrice
  | CRUnknownOffer
  | CRDisabledOffer
  | CROfferNotForPrice
  | CRUnsoldBadgeType BadgeType
  | CRUnpriced OfferInvalid
  deriving (Eq, Show)

-- | Resolve a request's priceId and optional offerId against the catalog rows, or say why not.
-- Refuses a disabled price, a disabled offer, an unsold badge type, an offer belonging to
-- another price, and any pair offerTotal cannot price. All of these are §5.1's catalog_changed.
priceOffer :: [BadgePrice] -> [BadgeOffer] -> BadgePriceId -> Maybe BadgeOfferId -> Either CatalogRefusal PricedOffer
```

`priceOffer` resolves the ids first and calls `offerTotal` last, so each refusal reports the condition that actually fired. Only `BTSupporter` and `BTLegend` are sold here — `BTInvestor` and `BTUnknown` are `CRUnsoldBadgeType`, matching the two levels `web/src/catalog.ts:5` compiles in. The rows themselves are read by `readCatalogRows` in `Store.hs` (Task 4), because every SQL statement lives there.

- [ ] **Step 4: run the tests**

Run: `cabal test --test-options='-m "badge catalog"'`
Expected: 5 examples, 0 failures.

- [ ] **Step 5: prove the drift check bites**

Temporarily change `web/src/catalog.ts`'s legend `monthPrice` from `7000` to `7001`, re-run `-m "badge catalog"`, and confirm `testNoDriftFromWeb` fails naming the price. Revert. A drift check that cannot fail is not a check; record the observed failure message in the commit's test run.

- [ ] **Step 6: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Catalog.hs tests/Bots/BadgeCatalogTests.hs \
  tests/Test.hs simplex-chat.cabal
git commit -m "core: price a badge offer once, for the page and the charge"
```

---

## Task 4: `Store.hs` — invoice creation and lookups

**Goal:** One module holds every statement this service runs, and the creation of an invoice, its `badge_code_invoices` row and its unpaid `badge_codes` row is one transaction that either happens completely or not at all.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Store.hs`
- Modify: `tests/Bots/BadgeWebTests.hs`
- Modify: `simplex-chat.cabal`

**Acceptance Criteria:**
- [ ] `createInvoiceRows` writes `invoices`, `badge_code_invoices` and `badge_codes` in a single `withTransaction`, and a failure at any statement leaves no row in any of the three
- [ ] `newInvoiceId` is 128 CSPRNG bits, base64url, and two calls differ
- [ ] `newSupportRef` is five characters from `23456789ABCDEFGHJKMNPQRSTUVWXYZ`, and a collision is retried rather than surfaced
- [ ] `codeHashExists` is true for a hash already in `badge_codes` — this is the `code_conflict` test of §5.1 and is asked before any provider call
- [ ] `getInvoice`, `getInvoiceByProviderRef` and `getInvoiceBySupportRef` each return the same `InvoiceRow`
- [ ] `listOpenInvoices` returns only `ISOpen` rows for the named provider
- [ ] `expireOverdue` sets `expired` only where `status = 'open' AND expires_at < cutoff`, writes `status` alone, and returns how many rows moved
- [ ] `readCatalogRows` returns `badge_prices` and `badge_offers` with the service prefix, dropping every `BISDisabled` row, so a disabled row cannot be sold even if `priceOffer` were wrong
- [ ] Every timestamp round-trips: a value written and read back equals the original to the second on both backends

**Verify:** `cabal test --test-options='-m "badge service store"'` → passes

**Steps:**

- [ ] **Step 1: write the failing tests**

Add to `tests/Bots/BadgeWebTests.hs` a `describe "badge service store"` covering each criterion. The transactionality test is the one that matters most, so it must be adversarial rather than incidental:

```haskell
testCreationIsAtomic :: IO ()
testCreationIsAtomic = withServiceStore $ \st -> do
  let ni = sampleInvoice {niCodeHash = duplicateHash}
  -- plant the code hash first, so the third statement of the transaction is the one that fails
  _ <- createInvoiceRows st sampleInvoice {niInvoiceId = "other", niSupportRef = "AAAAA", niProviderRef = "p-other"}
  r <- createInvoiceRows st ni
  r `shouldSatisfy` isLeft
  getInvoice st (niInvoiceId ni) >>= (`shouldBe` Nothing)
  codeInvoiceRow st (niInvoiceId ni) >>= (`shouldBe` Nothing)
```

and the support-ref collision test drives the generator with a stub source of randomness that yields the same five characters twice and a different five on the third call, asserting the row written carries the third value and that no exception escaped.

- [ ] **Step 2: run, expect failure**

Run: `cabal test --test-options='-m "badge service store"'`
Expected: compile failure — `BadgeService.Store` does not exist.

- [ ] **Step 3: write `Store.hs`**

The module's shape:

```haskell
-- | Every SQL statement the badge service runs (§4.1). Nothing else in this service
-- opens a transaction, so the atomicity claims of §5.1 and §6.4 are decided here.
module BadgeService.Store
  ( InvoiceRow (..),
    NewInvoice (..),
    CreateError (..),
    createInvoiceRows,
    getInvoice,
    getInvoiceByProviderRef,
    getInvoiceBySupportRef,
    codeHashExists,
    listOpenInvoices,
    expireOverdue,
    newInvoiceId,
    newSupportRef,
    readCatalogRows,
    revokeCodeByHash,
    codeRowForInvoice,
  )
where

data NewInvoice = NewInvoice
  { niInvoiceId :: InvoiceId,
    niSupportRef :: Text,
    niProviderRef :: Text,
    niCodeHash :: ByteString,
    niPriceId :: BadgePriceId,
    niOfferId :: Maybe BadgeOfferId,
    niBadgeType :: BadgeType,
    niMonths :: Word8,
    niPrice :: CurrencyAmount,
    niAmount :: CurrencyAmount,
    niCurrency :: Text,
    niProvider :: PaymentProvider,
    niDestination :: ServicePaymentDestination,
    niExpiresAt :: UTCTime,
    niCreatedAt :: UTCTime
  }

data CreateError = CECodeConflict | CERefConflict | CEOther Text
  deriving (Eq, Show)

-- | §5.1: "one transaction then writes invoices, badge_code_invoices and the unpaid
-- badge_codes row". The provider call precedes this, so provider_ref is known before
-- any row exists (§4.1 amendment A1).
createInvoiceRows :: DBStore -> NewInvoice -> IO (Either CreateError ())
```

`InvoiceRow` carries what both endpoints and the operator CLI read: invoice id, support ref, provider, provider ref, badge type, months, price, amount, currency, destination, expiry, status, created/updated, and the payment row's `amount`/`crypto_amount`/`status` where one exists.

`newSupportRef :: DBStore -> IO Text` draws five characters from the §4.2 alphabet with `getRandomBytes`, rejecting bytes outside a whole multiple of 31 so the distribution is uniform, and retries on the unique index. Cap the retries at 16 and return `CEOther` beyond that rather than looping forever.

`expireOverdue` is §6.5's sweep, verbatim in intent:

```sql
UPDATE sx_badge_service_invoices SET status = 'expired', updated_at = ?
 WHERE status = 'open' AND expires_at < ?
   AND invoice_id IN (SELECT invoice_id FROM sx_badge_service_badge_code_invoices)
```

- [ ] **Step 4: run the tests**

Run: `cabal test --test-options='-m "badge service store"'`
Expected: every example passes.

- [ ] **Step 5: mutate to prove the atomicity test**

Replace `withTransaction` with three separate `withTransaction` calls and re-run. `testCreationIsAtomic` must go red naming the orphaned `badge_code_invoices` row. Restore. A transaction test that stays green without the transaction proves nothing.

- [ ] **Step 6: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Store.hs tests/Bots/BadgeWebTests.hs simplex-chat.cabal
git commit -m "core: write an invoice, its code and its reference in one transaction"
```

---

## Task 5: `Waiters.hs` — the STM hold

**Goal:** A held `GET /api/invoice/:id?wait=` is woken by settlement rather than by re-reading the database, and the map holds exactly the invoices someone is waiting on.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Waiters.hs`
- Create: `tests/Bots/BadgeWaitersTests.hs`
- Modify: `simplex-chat.cabal`, `tests/Test.hs`

**Acceptance Criteria:**
- [ ] `awaitStatus` subscribes, *then* reads the database, then blocks — and a test proves the order by publishing between the subscribe and the read and asserting the call returns the new status instead of hanging
- [ ] A publish for a different invoice does not wake a waiter
- [ ] The timeout returns the current status rather than an error
- [ ] `subscribe`/`release` refcount, and the map is empty again after every waiter leaves — including when the waiting action throws
- [ ] `waitingCount` reports how many invoices have at least one waiter, which is what the poller reads to choose its cadence (§6.5)
- [ ] An unknown `wait` value answers immediately (the handler's concern, but the type makes it representable: `awaitStatus` takes the status to compare against)

**Verify:** `cabal test --test-options='-m "badge service waiters"'` → passes, no example takes longer than the 200 ms test timeout except the one asserting the timeout path

**Steps:**

- [ ] **Step 1: write the failing tests**

```haskell
-- tests/Bots/BadgeWaitersTests.hs
testSubscribeBeforeRead :: IO ()
testSubscribeBeforeRead = do
  w <- newWaiters
  ref <- newIORef ISOpen
  -- readStatus runs INSIDE awaitStatus, after subscribing. Publishing here models a
  -- settlement landing between the subscribe and the read: the wait must not miss it.
  let readStatus = do
        atomically $ publish w iid ISPaid
        readIORef ref
  s <- awaitStatus w iid readStatus ISOpen 5_000_000
  s `shouldBe` ISPaid

testMapEmptiesOnException :: IO ()
testMapEmptiesOnException = do
  w <- newWaiters
  _ <- try @SomeException $ awaitStatus w iid (throwIO Boom) ISOpen 1_000_000
  waitingCount w >>= (`shouldBe` 0)
```

- [ ] **Step 2: run, expect failure** — the module does not exist.

- [ ] **Step 3: write `Waiters.hs`**

§5.2's code, with the database read passed in so the module has no store dependency and the test above is possible:

```haskell
{-# LANGUAGE NamedFieldPuns #-}

-- | The waiters of §5.2, doing two jobs: waking held requests, and telling the poller
-- which invoices to list fast (§6.5). One TVar per watched invoice rather than one
-- broadcast, so a settlement wakes only the browsers on that invoice.
module BadgeService.Waiters (Waiters, newWaiters, publish, awaitStatus, waitingCount) where

data Watch = Watch {wStatus :: TVar InvoiceStatus, wRefs :: TVar Int}

newtype Waiters = Waiters (TVar (Map InvoiceId Watch))

-- | Publish after the settling transaction commits. Publishing inside it would let a
-- woken reader query a world where the write has not landed.
publish :: Waiters -> InvoiceId -> InvoiceStatus -> STM ()

-- | Subscribe, then read the database, then block. That order is the correctness
-- argument: subscribing after the read drops a settlement landing between the two,
-- and the request hangs for the full timeout with the answer already committed.
awaitStatus :: Waiters -> InvoiceId -> IO InvoiceStatus -> InvoiceStatus -> Int -> IO InvoiceStatus
awaitStatus w iid readStatus seen usec =
  bracket (atomically $ subscribe w iid) (const . atomically $ release w iid) $ \Watch {wStatus} -> do
    current <- readStatus -- after subscribing, never before
    atomically $ writeTVar wStatus current
    timer <- registerDelay usec
    atomically $
      (do s <- readTVar wStatus; check (s /= seen); pure s)
        `orElse` (do readTVar timer >>= check; readTVar wStatus)
```

`publish` writes the status into the `Watch` if one exists and does nothing otherwise. `subscribe` creates the `Watch` on first use and increments `wRefs`; `release` decrements and deletes at zero.

- [ ] **Step 4: run the tests** — all pass.

- [ ] **Step 5: mutate the ordering**

Move `readStatus` above `bracket` (subscribe after read) and re-run: `testSubscribeBeforeRead` must hang until its 5 s timeout and then fail on `ISOpen`. Note the observed wall-clock in the commit message's test run so the next reader knows the assertion has teeth. Restore.

- [ ] **Step 6: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Waiters.hs tests/Bots/BadgeWaitersTests.hs \
  tests/Test.hs simplex-chat.cabal
git commit -m "core: hold a status request on an stm waiter"
```

---

## Task 6: `Providers.hs` — the adapter interface and a test double

**Goal:** Everything above the provider boundary is provider-agnostic, and a test can drive the whole service with a provider that never touches the network.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Providers.hs`
- Modify: `simplex-chat.cabal`

**Acceptance Criteria:**
- [ ] The types are §6.1's: `ProviderError`, `WebhookError`, `Received`, `PaymentSignal` with `SigFunded`/`SigSettled`/`SigClosed`, and `ProviderInvoice`
- [ ] `Provider` is a record of the three operations plus the list pass the poller needs
- [ ] `Received.rcvAmount` is documented as the absolute total in the invoice currency, never a delta — the property §6.4 depends on
- [ ] `readInvoice` returns `Either ProviderError (Maybe PaymentSignal)`, so "no change" and "could not ask" are different answers
- [ ] `verifyWebhook` returns `Either WebhookError (Maybe Text)`, so a valid signature over an event we ignore is distinct from a bad signature
- [ ] A `StubProvider` in the test tree fills the record from a mutable map and records every call

**Verify:** `cabal build simplex-badge-service` → succeeds; the stub is exercised by Task 8's tests

**Steps:**

- [ ] **Step 1: write `Providers.hs`**

```haskell
-- | The provider boundary (§6.1). An adapter is a module in this service, not a library
-- and not a process: it holds one provider's HTTP calls, its signature scheme, and the
-- mapping from its vocabulary to PaymentSignal. Everything above this line is
-- provider-agnostic, which is what lets the poller and settleOrder be written once.
module BadgeService.Providers where

newtype ProviderError = ProviderError Text deriving (Eq, Show)

newtype WebhookError = WebhookError Text deriving (Eq, Show)

-- | What a provider reports as received: the ABSOLUTE total in the minor units of the
-- invoice currency, never a delta (§6.4), and the provider's own decimal string for
-- the same receipt, which is what B5c prints.
data Received = Received {rcvAmount :: CurrencyAmount, rcvCrypto :: Maybe Text}
  deriving (Eq, Show)

data PaymentSignal
  = SigFunded Received -- something arrived, the invoice is not settled
  | SigSettled Received UTCTime -- paid in full
  | SigClosed Received -- the window closed
  deriving (Eq, Show)

-- | What createInvoice returns: the provider's own id, and where to pay.
data ProviderInvoice = ProviderInvoice
  { piProviderRef :: Text,
    piDestination :: ServicePaymentDestination
  }
  deriving (Show)

-- | What a provider is told at creation. It carries no code and no code hash.
data OrderDraft = OrderDraft
  { odAmount :: CurrencyAmount,
    odCurrency :: Text,
    odSupportRef :: Text,
    odExpiresAt :: UTCTime
  }
  deriving (Eq, Show)

data Provider = Provider
  { pProvider :: PaymentProvider,
    pCreateInvoice :: ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice),
    pReadInvoice :: Text -> IO (Either ProviderError (Maybe PaymentSignal)),
    -- | §6.5: one filtered request covers every open invoice. Returns the signal per
    -- provider_ref for those whose state moved; an invoice not in the result has not moved.
    pListOpen :: IO (Either ProviderError [(Text, PaymentSignal)]),
    pVerifyWebhook :: [Header] -> ByteString -> Either WebhookError (Maybe Text)
  }
```

- [ ] **Step 2: write the stub in the test tree**

In `tests/Bots/BadgeWebTests.hs`, a `stubProvider :: IORef StubState -> Provider` whose `pCreateInvoice` returns a canned `ProviderInvoice`, whose `pReadInvoice`/`pListOpen` read the map the test writes, and whose calls are appended to a log the test asserts on — Task 8 needs "the provider was not called" as an assertion, which only a recording stub can give.

- [ ] **Step 3: build and commit**

```bash
cabal build simplex-badge-service
git add apps/simplex-badge-service/src/BadgeService/Providers.hs tests/Bots/BadgeWebTests.hs simplex-chat.cabal
git commit -m "core: define the payment provider boundary"
```

---

## Task 7: `Web/Server.hs` — listener, routing and `GET /api/invoice/:id`

**Goal:** A Warp application serves the committed web build and answers the read endpoint, holding when asked to.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`
- Modify: `apps/simplex-badge-service/src/BadgeService/Service.hs` (start the listener beside the bot)
- Modify: `tests/Bots/BadgeWebTests.hs`
- Modify: `simplex-chat.cabal`

**Acceptance Criteria:**
- [ ] `GET /` serves `static_dir/index.html`; `GET /assets/<hash>/<file>` serves that file; a path escaping `static_dir` (`..`, an absolute path, a URL-encoded separator, a symlink out) is 404 and never reads outside the directory
- [ ] `GET /api/invoice/:id` returns §5.2's 200 body with exactly the field names of `InvoiceView` in `web/src/api.ts`, `cryptoCurrency` lowercase
- [ ] An unknown id is `404 {"error":"not_found"}` with no other field, so it is indistinguishable from a guess
- [ ] A known path with the wrong method is 405
- [ ] Every API response carries `Cache-Control: no-store`
- [ ] `?wait=open` holds while the stored status is `open` and returns within 30 s; `?wait=paid` on a paid invoice answers at once; an unparseable `wait` answers at once
- [ ] The hold is woken by `publish` in under 100 ms in a test, proving it is not polled
- [ ] Over 60 requests per minute from one IP is `429 {"error":"rate_limited"}` with `Retry-After`; the IP comes from `X-Forwarded-For` only when `trust_forwarded_for` is on
- [ ] An exception inside a handler is a 500 with the `{"error":"internal"}` body and no detail, and does not kill the listener

**Verify:** `cabal test --test-options='-m "badge service web"'` → passes

**Steps:**

- [ ] **Step 1: write the failing tests**

Drive a real listener with `Warp.testWithApplication`, which binds a free port — never a fixed one, because a fixed port collides with a stray server from an earlier run and the failure mode is a test passing against the wrong server. Assert over real HTTP with `http-client`.

The wake-latency test is the load-bearing one:

```haskell
testHoldIsWokenNotPolled :: IO ()
testHoldIsWokenNotPolled = withWebApp $ \env base -> do
  iid <- seedOpenInvoice env
  started <- getCurrentTime
  held <- async $ getJSON (base <> "/api/invoice/" <> iid <> "?wait=open")
  threadDelay 100_000
  markPaidAndPublish env iid
  view <- wait held
  elapsed <- (`diffUTCTime` started) <$> getCurrentTime
  statusOf view `shouldBe` "paid"
  -- the hold is 30 s; anything near that means the answer came from a timeout, not a wake
  elapsed `shouldSatisfy` (< 1)
```

- [ ] **Step 2: run, expect failure** — the module does not exist.

- [ ] **Step 3: write the application**

```haskell
-- | The listener (§5). Routing is a single case over pathInfo so every reachable path is
-- visible in one place; anything not listed is 404 and any listed path reached with the
-- wrong method is 405.
webApp :: WebEnv -> Application
webApp env req respond = case (requestMethod req, pathInfo req) of
  ("GET", []) -> serveStatic env "index.html" respond
  ("GET", "assets" : rest) -> serveAsset env rest respond
  ("POST", ["api", "invoice"]) -> limited env createLimit $ createInvoiceHandler env req respond
  ("GET", ["api", "invoice", iid]) -> limited env readLimit $ readInvoiceHandler env iid req respond
  ("POST", ["webhooks", "btcpay"]) -> btcpayWebhookHandler env req respond
  (_, []) -> respond methodNotAllowed
  (_, ["api", "invoice"]) -> respond methodNotAllowed
  (_, ["api", "invoice", _]) -> respond methodNotAllowed
  (_, ["webhooks", "btcpay"]) -> respond methodNotAllowed
  _ -> respond (jsonError notFound404 "not_found")
```

with `WebEnv` holding the `DBStore`, the `ServiceConfig`, the `Waiters`, the provider registry and the rate-limit buckets — constructible in a test without a `ChatController`, which is what makes every assertion above possible.

Constants, named rather than inlined:

```haskell
holdMicros :: Int
holdMicros = 30 * 1000000 -- §5.2, under the idle timeout of every proxy in front (§9)

readLimit, createLimit :: Limit
readLimit = Limit {perMinute = 60} -- §5.2
createLimit = Limit {perMinute = 5} -- §5.1: each request reaches a provider
```

Static serving resolves the requested path against `static_dir` and refuses anything whose canonical path is not inside it — canonicalise both and compare, rather than pattern-matching on `".."`, which twelve spellings get past.

- [ ] **Step 4: start it beside the bot**

In `Service.hs`, `badgeService` currently runs one `forever` loop over `outputQ`. Wrap it and the listener in `raceAny_`, so either dying stops the process rather than leaving half a service running.

- [ ] **Step 5: run the tests, then mutate**

Run: `cabal test --test-options='-m "badge service web"'`

Then mutate three things and confirm each turns a specific example red: (a) drop the `Cache-Control` header; (b) return the invoice's fields in the 404 body; (c) replace the wake with a 1-second poll loop — `testHoldIsWokenNotPolled` must fail on elapsed time. Restore each.

- [ ] **Step 6: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Web tests/Bots/BadgeWebTests.hs \
  apps/simplex-badge-service/src/BadgeService/Service.hs simplex-chat.cabal
git commit -m "core: serve the site and the invoice status endpoint"
```

---

## Task 8: `POST /api/invoice` and every refusal

**Goal:** A priced, provider-backed invoice exists after a valid request, and every refusal §5.1 names is decided before the provider is called.

**Files:**
- Modify: `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`
- Modify: `tests/Bots/BadgeWebTests.hs`

**Acceptance Criteria:**
- [ ] A valid request returns 200 with exactly `CreatedInvoice`'s fields (`web/src/api.ts:79`): `invoiceId`, `supportRef`, `badgeType`, `months`, `amount`, `currency`, `expiresAt`, plus `address`, `cryptoAmount`, `cryptoCurrency` for crypto
- [ ] The 200 carries **no** code and no code hash — asserted over the raw response bytes, not over a parsed object
- [ ] `badgeType`, `months` and `amount` are derived from `priceId`/`offerId`, so a request naming a legend price with a supporter amount is charged the legend amount
- [ ] `code_conflict` (409), `catalog_changed` (400), `bad_request` (400), `provider_unavailable` (503) and `rate_limited` (429) each occur for their §5.1 condition
- [ ] **Every refusal writes no row and makes no provider call** — asserted against the recording stub's call log, which is what B4c's "Nothing was charged" claims
- [ ] `method: "card"` is 503 `provider_unavailable` while Stripe is unimplemented (Global Constraint 12)
- [ ] A malformed `codeHash` (not 64 lowercase hex characters) is `bad_request`
- [ ] Six requests in a minute from one IP: the sixth is 429 with `Retry-After`

**Verify:** `cabal test --test-options='-m "badge service checkout"'` → passes

**Steps:**

- [ ] **Step 1: write the failing tests**

The no-leak assertion works on bytes, because a parsed object cannot see a field nobody decoded:

```haskell
testNoCodeInTheResponse :: IO ()
testNoCodeInTheResponse = withWebApp $ \env base -> do
  let code = "SXBYDC8AYGQTMPUYZ92TUXP"
      codeHash = sha256Hex code
  raw <- postRaw (base <> "/api/invoice") (createBody codeHash)
  raw `shouldNotContain` code
  raw `shouldNotContain` codeHash
```

and the refusal tests each assert the stub's call log is empty afterwards:

```haskell
testRefusalCostsNothing :: IO ()
testRefusalCostsNothing = withWebApp $ \env base -> do
  _ <- post (base <> "/api/invoice") (createBody' "price_nonexistent")
  calls <- providerCalls env
  calls `shouldBe` []
  invoiceCount env >>= (`shouldBe` 0)
```

- [ ] **Step 2: run, expect failure.**

- [ ] **Step 3: write the handler**

Order matters and is the whole point: parse and validate the body, resolve the catalog, check the code hash, *then* pick the provider and call it, *then* write the rows.

```haskell
createInvoiceHandler env req respond = do
  body <- strictRequestBody req
  case parseCreateRequest body of
    Left _ -> respond (jsonError badRequest400 "bad_request")
    Right CreateRequest {priceId, offerId, method, codeHash} -> do
      (prices, offers) <- readCatalogRows (store env)
      case priceOffer prices offers priceId offerId of
        Left reason -> logInfo (refusalLine reason) >> respond (jsonError badRequest400 "catalog_changed")
        Right priced -> do
          conflict <- codeHashExists (store env) codeHash
          if conflict
            then respond (jsonError conflict409 "code_conflict")
            else case providerFor env method of
              Nothing -> respond (jsonError serviceUnavailable503 "provider_unavailable")
              Just provider -> createAtProvider env provider method priced codeHash respond
```

`createAtProvider` draws the invoice id and support reference, calls `pCreateInvoice`, and on `Right` writes the rows in Task 4's single transaction; a `Left ProviderError` is logged and answered `provider_unavailable` with no row written.

- [ ] **Step 4: run the tests and mutate**

Move the `codeHashExists` check to after the provider call and re-run: `testRefusalCostsNothing`'s conflict case must go red on a non-empty call log. Restore.

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Web/Server.hs tests/Bots/BadgeWebTests.hs
git commit -m "core: create a priced invoice at the provider"
```

---

## Task 9: `Orders.hs` — `settleOrder`

**Goal:** One function, one transaction, decides what a provider signal does to an invoice — and a replay of any signal changes nothing it should not.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Orders.hs`
- Modify: `tests/Bots/BadgeWebTests.hs`
- Modify: `simplex-chat.cabal`

**Acceptance Criteria:**
- [ ] `settleOrder` implements §6.4's table exactly: `SigSettled` against `open`/`expired` → `paid` + settled payment row + code `unpaid`→`paid`; `SigFunded` against `open`/`expired` → status unchanged, payment row `pending`; `SigClosed` against `open` → `expired`; `SigClosed` against `expired` → unchanged, payment row written; any signal against `paid` → nothing
- [ ] The payment row is an upsert keyed on the invoice id, and its `amount` takes the larger of stored and reported (`MAX` on SQLite, `GREATEST` on Postgres) — replaying a receipt of 40000 twice leaves 40000, and an out-of-order smaller event cannot lower it
- [ ] `crypto_amount` is written only when the fiat figure increases, because it is not comparable as text
- [ ] The status write is guarded by the status observed at the start of the transaction; zero rows affected means another transaction moved first, so the call stops, changes nothing, commits and reports success
- [ ] Settlement sets `badge_codes.code_payment_status = 'paid'` and `expires_at` in one statement guarded by `code_payment_status = 'unpaid'`, so a second settlement does not move `expires_at`
- [ ] `publish` runs **after** the commit, and a test proves it by making the woken reader assert the row is already visible
- [ ] `expired → paid` is legal; late on-chain settlement is routine

**Verify:** `cabal test --test-options='-m "badge service settlement"'` → passes

**Steps:**

- [ ] **Step 1: write the failing tests**

One example per row of §6.4's table, plus:

```haskell
testAmountIsMonotonic :: IO ()
testAmountIsMonotonic = withServiceStore $ \st -> do
  iid <- seedOpen st
  _ <- settleOrder st waiters iid (SigFunded (Received 40000 (Just "0.734"))) now
  _ <- settleOrder st waiters iid (SigFunded (Received 10000 (Just "0.180"))) now
  p <- paymentRow st iid
  amountOf p `shouldBe` 40000
  cryptoOf p `shouldBe` Just "0.734" -- not overwritten by the smaller receipt

testPublishIsAfterCommit :: IO ()
testPublishIsAfterCommit = withServiceStore $ \st -> do
  iid <- seedOpen st
  seen <- newEmptyMVar
  -- the waiter reads the DB the moment it is woken; if publish ran inside the
  -- transaction this read would find the old status or block on the writer
  _ <- forkIO $ awaitStatus waiters iid (statusOf st iid) ISOpen 5_000_000 >>= putMVar seen
  threadDelay 50_000
  _ <- settleOrder st waiters iid (SigSettled (Received 42000 Nothing) now) now
  takeMVar seen >>= (`shouldBe` ISPaid)
  statusOf st iid >>= (`shouldBe` ISPaid)
```

- [ ] **Step 2: run, expect failure.**

- [ ] **Step 3: write `Orders.hs`**

```haskell
-- | Settlement (§6.4). One function, called by the poller and by nothing else, doing one
-- transaction. The guards here — the status observed at the start, the monotonic amount,
-- the unpaid code — are what make a replayed provider event safe without a provider_events
-- table. Any future code that treats a provider amount as a delta reintroduces the need for one.
settleOrder :: DBStore -> Waiters -> InvoiceId -> PaymentSignal -> UTCTime -> IO (Either Text InvoiceStatus)
```

Structure: `withTransaction` performs steps 1–5 and returns the status to publish (or `Nothing`), then `publish` runs outside it. Put a comment on the `publish` line saying it must stay outside, because the next reader's instinct will be to move it in.

- [ ] **Step 4: run, then mutate three ways**

(a) Move `publish` inside the transaction — `testPublishIsAfterCommit` must fail or deadlock. (b) Replace `MAX` with the reported amount — `testAmountIsMonotonic` red. (c) Drop the `WHERE status = ?` guard — the concurrent-settlement example red. Restore each and record the observed failures.

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Orders.hs tests/Bots/BadgeWebTests.hs simplex-chat.cabal
git commit -m "core: settle an invoice in one guarded transaction"
```

---

## Task 10: `Providers/BTCPay.hs` — the Greenfield adapter

**Goal:** The service can create, read and list invoices at a BTCPay Server, and verify its webhook signature.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Providers/BTCPay.hs`
- Modify: `simplex-chat.cabal`

**Acceptance Criteria:**
- [ ] `createInvoice` POSTs to `{host}/api/v1/stores/{storeId}/invoices` with `Authorization: token {api_key}`, the amount as a decimal string in **major** units, the currency uppercased, `metadata` carrying the support reference under a key of ours (not `orderId`, not `posData`, which BTCPay gives meaning to), `checkout.expirationMinutes`, `checkout.speedPolicy` as a **name**, `checkout.paymentTolerance`, and `checkout.paymentMethods` restricted to the one chain chosen
- [ ] `checkout.redirectURL` is not set — the crypto path stays on one URL (§6.3)
- [ ] The minor-to-major conversion is integer arithmetic: 4200 → `"42.00"`, 700 → `"7.00"`, 42000 → `"420.00"`, and no `Double` appears in the module
- [ ] `getPaymentMethods` reads the address and crypto amount for the chosen method, and `createProviderInvoice` returns them in `ProviderInvoice`; a payment-methods failure after a successful create is a `ProviderError` and writes nothing
- [ ] `readInvoice` maps §6.3's table: `Settled` → `SigSettled` timed by the latest `Settled` entry in `payments` by `receivedDate`; `Processing` → `SigFunded`; `Expired`/`Invalid` → `SigClosed`; `New` with something received → `SigFunded`; `New` with nothing → `Nothing`
- [ ] There is no `Complete` case, and a status string the adapter does not know is a `ProviderError` naming it rather than a silent `Nothing`
- [ ] Amounts are parsed as decimals from JSON **strings** via `Scientific`, never `Double`; `paymentMethodPaid` is read, never `totalPaid`
- [ ] `receivedDate` is parsed as a Unix timestamp
- [ ] `verifyWebhook` checks `BTCPay-Sig: sha256=<hex>` as `HMAC-SHA256(secret, rawBody)` in constant time, returns `Right (Just invoiceId)` for the four acted-on types, `Right Nothing` for a valid signature over any other type, and `Left` for a missing, malformed or wrong signature
- [ ] `BTC-CHAIN` and `XMR-CHAIN` are named constants, logged at startup beside the ids the store reports (amendment A2)

**Verify:** `cabal build simplex-badge-service` → succeeds; behaviour is proved in Task 11

**Steps:**

- [ ] **Step 1: write the module**

Keep every wire type in this module — nothing above the boundary sees a Greenfield field name. Decoders ignore unknown fields, which is what lets a real instance carry more than the fixture does.

```haskell
-- | The BTCPay Greenfield adapter (§6.3). Every amount on this wire is a JSON string and
-- every timestamp a Unix second; both are parsed as exact values, because a decimal read
-- through Double is a figure a buyer checks against their wallet and finds wrong.
module BadgeService.Providers.BTCPay (btcpayProvider) where

btcMethodId, xmrMethodId :: Text
btcMethodId = "BTC-CHAIN"
xmrMethodId = "XMR-CHAIN"

-- | Minor units to the provider's decimal string, by integer arithmetic only.
-- 4200 -> "42.00". Two decimal places, which is what the currencies here use.
minorToDecimal :: CurrencyAmount -> Text
minorToDecimal (CurrencyAmount a) =
  T.pack (show (a `div` 100)) <> "." <> T.justifyRight 2 '0' (T.pack (show (a `mod` 100)))
```

- [ ] **Step 2: verify the conversion by hand before trusting it**

In `ghci`, check `minorToDecimal` on 0, 5, 99, 100, 700, 4200, 42000 and 100000000, and confirm `"0.00"`, `"0.05"`, `"0.99"`, `"1.00"`, `"7.00"`, `"42.00"`, `"420.00"`, `"1000000.00"`. Task 11 pins these as tests; do the manual pass first, because a wrong pad here charges the wrong amount and no later test in this plan would notice if it were written to match the bug.

- [ ] **Step 3: build and commit**

```bash
cabal build simplex-badge-service
git add apps/simplex-badge-service/src/BadgeService/Providers/BTCPay.hs simplex-chat.cabal
git commit -m "core: add the btcpay greenfield adapter"
```

---

## Task 11: `FakeBTCPay.hs` and adapter tests

**Goal:** A fake Greenfield server the adapter can be driven against, and tests that prove the adapter's mapping rather than its ability to compile.

**Files:**
- Create: `tests/Bots/FakeBTCPay.hs`
- Create: `tests/Bots/BadgeBTCPayTests.hs`
- Create: `apps/simplex-badge-service/test-fixtures/btcpay/*.json`
- Modify: `simplex-chat.cabal`, `tests/Test.hs`

**Acceptance Criteria:**
- [ ] The fake serves `POST /api/v1/stores/{id}/invoices`, `GET …/invoices/{id}`, `GET …/invoices/{id}/payment-methods` and `GET …/invoices` (list), binding a free port
- [ ] Its responses are read from committed JSON fixtures, so the shapes are data a later task can replace with recorded reality rather than code
- [ ] Control endpoints, all prefixed `_` so the adapter cannot reach one by accident: `POST /_state/{id}` sets status, `additionalStatus` and `paymentMethodPaid`; `POST /_fail {calls,status}` makes the next N calls answer with that status
- [ ] The fake **rejects a request whose `Authorization` header is not `token <key>`**, so a broken auth header fails a test instead of passing silently
- [ ] Adapter tests: create returns an address and crypto amount for both `btc` and `xmr`; each of §6.3's status rows maps to its signal; an unknown status is a `ProviderError` naming it; a 500 is a `ProviderError`; `paymentMethodPaid` is what is read, proved by a fixture whose `totalPaid` differs
- [ ] `minorToDecimal` is pinned at the eight values checked in Task 10
- [ ] `verifyWebhook` accepts a body signed with the secret, rejects the same body under a different secret, rejects a re-serialised body (BTCPay indents its payload, so re-serialising changes the bytes and the HMAC), and returns `Right Nothing` for a valid signature over an unhandled type

**Verify:** `cabal test --test-options='-m "badge btcpay"'` → passes

**Steps:**

- [ ] **Step 1: write the fixtures**

One JSON file per response shape, from BTCPay's documented Greenfield schema: `invoice-new.json`, `invoice-processing.json`, `invoice-settled.json`, `invoice-expired.json`, `invoice-invalid.json`, `payment-methods-btc.json`, `payment-methods-xmr.json`, `invoice-list.json`. Give each a header comment field (`"_fixture"`) naming where it came from; Task 15 replaces the contents with captured reality and that field records the change.

- [ ] **Step 2: write the fake and the tests, run them**

Run: `cabal test --test-options='-m "badge btcpay"'`
Expected: every example passes.

- [ ] **Step 3: mutate the signature check**

Replace the constant-time comparison with `==` — the tests stay green, which is the point: note in the module that timing is not what these tests prove, and keep the constant-time call. Then mutate the secret used in `verifyWebhook` to a constant and confirm the wrong-secret example goes red.

- [ ] **Step 4: commit**

```bash
git add tests/Bots/FakeBTCPay.hs tests/Bots/BadgeBTCPayTests.hs \
  apps/simplex-badge-service/test-fixtures tests/Test.hs simplex-chat.cabal
git commit -m "core: drive the btcpay adapter against a fake greenfield"
```

---

## Task 12: `Poller.hs` — the list pass and the expiry sweep

**Goal:** Payment is detected without any webhook, which is the property §6.1 rests on.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Poller.hs`
- Modify: `apps/simplex-badge-service/src/BadgeService/Service.hs`
- Modify: `tests/Bots/BadgeWebTests.hs`
- Modify: `simplex-chat.cabal`

**Acceptance Criteria:**
- [ ] A pass is **one list request per provider**, not one read per invoice — asserted against the fake's request log with three open invoices
- [ ] Every signal the pass returns goes to `settleOrder`
- [ ] The cadence is `waiting_seconds` when `waitingCount > 0` and `idle_seconds` otherwise, read fresh each pass so a waiter arriving speeds up the next one
- [ ] A `Left ProviderError` leaves every invoice alone and the next pass runs — a provider outage delays detection and loses nothing
- [ ] The expiry sweep calls no provider and moves only `open` invoices past `expires_at + 10 minutes` grace
- [ ] The sweep writes `status` alone, so a later `SigClosed` still records what was received
- [ ] An invoice whose window closed more than 72 hours ago is not listed again
- [ ] **A settlement is detected with webhooks never configured** — the test that matters: create, settle at the fake, run the poller, assert the code row is `paid`

**Verify:** `cabal test --test-options='-m "badge service poller"'` → passes

**Steps:**

- [ ] **Step 1: write the failing tests**, the webhook-free one first:

```haskell
testSettlesWithNoWebhookAtAll :: IO ()
testSettlesWithNoWebhookAtAll = withFakeBTCPay $ \fake -> withWebApp' fake $ \env base -> do
  Created {invoiceId} <- postCreate base xmrRequest
  setFakeState fake (providerRefOf env invoiceId) "Settled" "42000"
  runOnePass env
  codePaymentStatus env invoiceId >>= (`shouldBe` CPSPaid)
  invoiceStatus env invoiceId >>= (`shouldBe` ISPaid)
```

- [ ] **Step 2: run, expect failure.**

- [ ] **Step 3: write the poller**, as a loop of `onePass` separated from the scheduling so `runOnePass` is directly testable — a loop that can only be tested by waiting is a loop nobody tests.

Constants:

```haskell
expiryGrace :: NominalDiffTime
expiryGrace = 600 -- §6.5: ten minutes, covering a read racing the deadline and clock skew

closedWindow :: NominalDiffTime
closedWindow = 72 * 3600 -- §6.5: past this the scan stops; must stay under BTCPay's monitoringExpiration
```

- [ ] **Step 4: run it under the service**, wiring the poller into `Service.hs`'s `raceAny_`.

- [ ] **Step 5: mutate the cadence**

Force `waitingCount` to 0 always and confirm the waiting-cadence example goes red; force the pass to read invoices one at a time and confirm the one-request example goes red. Restore.

- [ ] **Step 6: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Poller.hs \
  apps/simplex-badge-service/src/BadgeService/Service.hs tests/Bots/BadgeWebTests.hs simplex-chat.cabal
git commit -m "core: detect payment by listing open invoices"
```

---

## Task 13: `POST /webhooks/btcpay`

**Goal:** A verified webhook shortens the wait and can do nothing else.

**Files:**
- Modify: `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`
- Modify: `tests/Bots/BadgeWebTests.hs`

**Acceptance Criteria:**
- [ ] The raw body is read before any parsing and capped at 64 KB; a larger body is 413 and is not parsed
- [ ] A missing, malformed or wrong signature is 400 with an empty body and no detail
- [ ] A valid signature over an acted-on type queues an immediate read of that `provider_ref` and answers 200 empty
- [ ] A valid signature over an unhandled type, an unknown `provider_ref`, or a `provider_ref` belonging to the other provider is 200 empty and logged
- [ ] **The route never reads the provider, never opens a transaction and never waits** — it verifies, enqueues and answers, asserted by timing the handler against a fake whose reads block for a second
- [ ] There is no 5xx from this route: an internal failure after the 200 is corrected by the next poll
- [ ] The queued read reaches `settleOrder` by the same path the poller uses, so there is one settlement lane, not two

**Verify:** `cabal test --test-options='-m "badge service webhook"'` → passes

**Steps:**

- [ ] **Step 1: write the failing tests**, including the one that pins the route's cheapness:

```haskell
testWebhookDoesNotWaitOnTheProvider :: IO ()
testWebhookDoesNotWaitOnTheProvider = withSlowFake 1_000_000 $ \fake -> withWebApp' fake $ \env base -> do
  ref <- seedOpenWithProviderRef env "inv-1"
  started <- getCurrentTime
  status <- postSigned (base <> "/webhooks/btcpay") (settledBody ref)
  elapsed <- (`diffUTCTime` started) <$> getCurrentTime
  status `shouldBe` 200
  elapsed `shouldSatisfy` (< 0.2) -- the read is queued, not awaited
```

- [ ] **Step 2: run, expect failure.**

- [ ] **Step 3: write the handler**, pushing the `provider_ref` onto the poller's queue and answering. The queue is a bounded `TBQueue`; a full queue drops the hint and logs, because the poll will find it anyway and blocking here would make a provider wait.

- [ ] **Step 4: run and mutate** — make the handler call `pReadInvoice` inline and confirm the timing example goes red. Restore.

- [ ] **Step 5: commit**

```bash
git add apps/simplex-badge-service/src/BadgeService/Web/Server.hs tests/Bots/BadgeWebTests.hs
git commit -m "core: queue a read from a verified btcpay webhook"
```

---

## Task 14: Scenario tests — the whole crypto lane

**Goal:** One suite drives a purchase from `POST /api/invoice` to a paid code against the fake Greenfield, covering the states a real payment moves through.

**Files:**
- Modify: `tests/Bots/BadgeWebTests.hs`
- Modify: `apps/simplex-badge-service/web/README.md` (how to run the service against the web build)

**Acceptance Criteria:**
- [ ] Create → settle at the fake → poll → `GET /api/invoice/:id` reports `paid`, and the `badge_codes` row is `CPSPaid` with `expires_at` set
- [ ] A held `?wait=open` request opened before the settlement returns `paid` within a second of it
- [ ] Partial payment leaves the invoice `open` with `amountPaid` and `cryptoAmountPaid` reported — B5c's inputs
- [ ] An expiry with something received reports the amount and keeps the invoice `expired`; the code stays unpaid
- [ ] **Late settlement works**: an `expired` invoice that then settles becomes `paid` and writes the code, because on-chain confirmation after expiry is routine
- [ ] A replayed settlement writes no second payment row, does not move `expires_at`, and answers the same
- [ ] `InvoiceInvalid` closes the invoice as `expired` (§6.3: the remedy and the screen are identical)
- [ ] Two invoices with the same `codeHash` — the second is `code_conflict` and no invoice exists at the provider for it
- [ ] The whole suite runs with **no webhook delivered at all** in at least one example, proving §6.1's claim that the service works with webhooks switched off

**Verify:** `cabal test --test-options='-m "Supporter badges" -m "Badge service"'` → all examples pass, and the run reports the new ones

**Steps:**

- [ ] **Step 1: write the scenarios** as one `describe "badge service scenarios"`, each an independent example with its own database and its own fake.

- [ ] **Step 2: run the whole badge suite**

Run: `cabal test --test-options='-m "Supporter badges" -m "Badge service"'`
Expected: 0 failures, and the count is higher than before this task by the number of scenarios added.

- [ ] **Step 3: run the web suite too**

Run: `cd apps/simplex-badge-service/web && npm test`
Expected: `pass 407` — Global Constraint 1.

- [ ] **Step 4: document the run**

In `web/README.md`, add a section showing how to serve the real service against the built site: build the web, write `badge_service.ini` from the example with `static_dir` pointing at `web/dist`, run `cabal run simplex-badge-service`, and note that `mock/server.py` remains the browser-only fixture and is not the service.

- [ ] **Step 5: commit**

```bash
git add tests/Bots/BadgeWebTests.hs apps/simplex-badge-service/web/README.md
git commit -m "core, web: cover the crypto lane end to end against the fake"
```

---

## Task 15: A real BTCPay payment, end to end

**Goal:** A Bitcoin payment made at a real BTCPay Server, against a real Greenfield API, moves a browser-drawn code to paid and shows B6 in a real browser — with the captured evidence of both the unpaid and the paid state.

> **USER-ORDERED GATE — NON-SKIPPABLE.** This task was requested by the user in the current conversation. It MUST NOT be closed by walking around it, by declaring it "verified inline", or by substituting a cheaper check. Close only after every item in `acceptanceCriteria` has been re-validated independently, with output captured.

**Files:**
- Modify: `apps/simplex-badge-service/test-fixtures/btcpay/*.json` (replaced with captured real responses, secrets redacted)
- Modify: `tests/Bots/BadgeBTCPayTests.hs` (whatever the captured shapes prove wrong)
- Create: `plans/badges-codes/2026-08-31-btcpay-e2e-evidence.md` (the captured run)

**Prerequisite the user supplies:** the BTCPay instance URL, store id, Greenfield API key and webhook secret, written by hand into an untracked `apps/simplex-badge-service/badge_service.ini`. Do not attempt to register an account.

**Acceptance Criteria:**
- [ ] `POST /api/invoice` against the real instance returns a 200 carrying a real Bitcoin address and crypto amount, and the raw response bytes contain neither the code nor its hash
- [ ] The real Greenfield responses for create, read and payment-methods are captured verbatim and committed as the fixtures, with the API key and any store-identifying value redacted
- [ ] **Unpaid state captured:** before payment, `GET /api/invoice/:id` reports `status: "open"` and the `badge_codes` row is `CPSUnpaid` — captured output
- [ ] **Paid state captured:** after a real payment to the address, the poller alone (webhooks not configured, or configured and then disabled for one run) moves the invoice to `paid`, and `GET /api/invoice/:id` reports `status: "paid"` — captured output
- [ ] The elapsed time between the payment confirming at BTCPay and the endpoint reporting `paid` is recorded, and is within one `waiting_seconds` interval plus one confirmation
- [ ] The browser, served by the service rather than by `mock/server.py`, moves from B5 to B6 and shows the code it drew — captured as a screenshot of each
- [ ] `cabal test --test-options='-m "badge btcpay"'` passes against the replaced fixtures; any adapter change the real shapes forced is committed with them
- [ ] The evidence file records the instance, the network, the invoice id (truncated), both captured states, the timings, and every discrepancy found between BTCPay's documented shapes and its real ones

**Verify:** `cabal run simplex-badge-service` against the real instance, then the captured before/after of `curl -s http://127.0.0.1:8080/api/invoice/$ID | jq .status` → `"open"` then `"paid"`, plus `cabal test --test-options='-m "badge btcpay"'` → 0 failures against the captured fixtures

**Steps:**

- [ ] **Step 1: confirm the credentials work before anything else**

```bash
curl -s -H "Authorization: token $BTCPAY_KEY" \
  "$BTCPAY_HOST/api/v1/stores/$BTCPAY_STORE/invoices?take=1" | head -c 400
```

A 401 or 403 here stops the task: report it and ask the user to check the key's permissions (`btcpay.store.cancreateinvoice` and `btcpay.store.canviewinvoices`). Do not proceed against a fake and call it done.

- [ ] **Step 2: capture the real shapes**

Create one invoice through the real adapter and save the three responses to the fixture files, redacting secrets. Diff each against the committed fixture from Task 11 and write every difference into the evidence file — this is the only step in the plan that can find out whether the adapter's reading of the documentation was right.

- [ ] **Step 3: fix whatever the real shapes broke**, in `Providers/BTCPay.hs` and in the fake, and re-run `-m "badge btcpay"` until green against the captured bytes.

- [ ] **Step 4: run the real purchase**

Serve the built site through the service, open it in the browser, buy one month at the supporter price, capture `status: "open"` and the unpaid code row, pay the invoice, and capture the transition to `paid` with the poller as the only detector.

- [ ] **Step 5: capture the screens**

Screenshot B5 while waiting and B6 after settlement, served by the service. The browser at `/project/container/claude/jobs/9b4e6eac/tmp/shot` renders them without a display.

- [ ] **Step 6: write the evidence file and commit**

```bash
git add apps/simplex-badge-service/test-fixtures/btcpay tests/Bots/BadgeBTCPayTests.hs \
  apps/simplex-badge-service/src/BadgeService/Providers/BTCPay.hs \
  plans/badges-codes/2026-08-31-btcpay-e2e-evidence.md
git commit -m "core, plan: settle a real btcpay invoice end to end"
```

---

## Self-review

**Spec coverage.** §4.1 tables and the service-only columns → Task 2. §4.2's `codeHash`-only contract → Tasks 4 and 8 (the service never receives a code; asserted on raw bytes). §5.1 endpoint, pricing and every refusal → Tasks 3 and 8. §5.2 read endpoint, the hold and the waiters → Tasks 5 and 7. §6.1 provider boundary → Task 6. §6.3 BTCPay → Tasks 10 and 11. §6.4 settlement → Task 9. §6.5 polling and the expiry sweep → Task 12. Webhook routes → Task 13. §9 configuration → Task 1. Whole-lane proof → Tasks 14 and 15.

**Deliberately out of scope, and why.** §6.2 Stripe and the card lane: the browser half is built and the service half is a plan of its own; here `card` answers `provider_unavailable`, which is §5.1's designed response and lands on B4b. §10's operator CLI (`codes status`, `codes revoke`): it needs no part of the payment lane and its absence blocks no test here — it is the natural first task of the follow-up plan, and `revokeCodeByHash` is already in Task 4's store so the CLI is a command parser over an existing statement. §4.3 retention. §11's open questions, all three of which are decisions rather than code.

**Known gaps carried forward.** The fixtures in Task 11 are written from BTCPay's documentation rather than from a running instance, so until Task 15 replaces them the adapter is proved against our reading of the docs and not against BTCPay; Task 15's acceptance criteria make replacing them mandatory rather than optional. Monero cannot be exercised against a demo instance, which runs no Monero plugin, so the XMR path stays proved by the fake alone and that limit belongs in the evidence file. And the service is bound to one process by the in-memory waiter map (§11.11), which no task here changes.
