# Service-side Stripe card lane — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers-extended-cc:subagent-driven-development (recommended) or superpowers-extended-cc:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `POST /api/invoice` with `method:"card"` create a real Stripe Checkout Session, and settle it from the poller and `/webhooks/stripe`, so the card lane the browser already speaks reaches a working provider.

**Architecture:** Add a second payment adapter (`Providers/Stripe.hs`) implementing the existing provider-agnostic `Provider` interface, exactly as `Providers/BTCPay.hs` does. Parse a `[stripe]` config section, build the Stripe `Provider` at startup and put it in `weProviders`, and generalize the currently btcpay-only webhook route/handler to dispatch by provider. No new plumbing: the poller, settlement, wire format (`clientSecret`), store, and web card lane are already provider-agnostic and complete.

**Tech Stack:** Haskell (GHC 9.6.3 dev / 8.10.7 CI), `http-client`+`http-client-tls`, `crypton` HMAC-SHA256, `aeson`, Warp test server for the fake. Stripe Checkout Sessions API (`ui_mode=elements`).

**Spec:** `plans/badges-codes/2026-08-27-badge-codes.md` §6.1 (webhook/polling model), §6.2 (Stripe), §6.5 (polling schedule), §9 (config).

## Global Constraints

- **Mirror the BTCPay adapter exactly** (`apps/simplex-badge-service/src/BadgeService/Providers/BTCPay.hs`): private `StripeEnv {seCfg, seManager}` env, smart constructor `stripeProvider :: StripeConfig -> IO Provider`, one shared bounded HTTP helper carrying a `what :: Text` label into every `ProviderError`, `try`/`HttpException` → `Left ProviderError`, named constants for every bound (no magic numbers), `-Weverything`/`-Werror` clean on GHC 9.6.3 AND 8.10.7 (base 4.14 — no newer-base APIs; e.g. `showHex` needs a concrete type).
- **`pProvider = PPStripe`.** The `Provider` record has six fields, all required: `pProvider, pCreateInvoice, pReadInvoice, pCancelInvoice, pListOpen, pVerifyWebhook` (`Providers.hs:70-79`). `pVerifyWebhook` is pure.
- **Settlement keys on `payment_status`, read from the provider — never on the event or `status` alone.** `status=complete` + `payment_status=paid` → `SigSettled`; `status=complete` + `payment_status=unpaid` → no signal (still processing); `status=expired` → `SigClosed`; `status=open` → no signal. There is no card partial, so the Stripe adapter never produces `SigFunded` (spec §6.2).
- **The webhook verifies and enqueues only.** It never calls Stripe, opens a transaction, or waits. Two events are acted on: `checkout.session.completed`, `checkout.session.expired`. Everything else → valid-but-not-acted-on (`Right Nothing`), logged, answered 200. Signature is HMAC-SHA256 over `"{t}.{rawBody}"` from `Stripe-Signature: t=…,v1=…`, constant-time; the 300 s replay tolerance is deferred, the interface being pure (spec §6.1, §6.2).
- **Raw body for the signature.** Verify over the exact received bytes; never parse-then-reencode.
- **Buyer reference is our order id, shown identically to the crypto lane; nothing extra goes to Stripe.** The buyer-facing "Reference" is our order id (`reference(o.order.orderId)`, `screens.ts:191`), already rendered on the card screens exactly as on the crypto screens — no change. The Stripe checkout **session id** becomes `provider_ref` (internal correlation for webhook/poll/cancel), never shown to the buyer, exactly as BTCPay's Greenfield invoice id is never shown. Do **not** send our order id to Stripe: set no `client_reference_id` and no `success_url` (§6.2 — our order id is a bearer capability, and Stripe stores and can echo those fields into dashboards/emails/logs). `client_reference_id` is where the removed `simplexSupportRef` used to go, so it stays unset; also set no `statement_descriptor_suffix`. This follows the current code over spec §6.2's support-ref lines, which predate the removal.
- **No `[site] base_url` / `return_url`.** `ui_mode=elements` enables no redirect-based method, so `return_url` is not required (spec §6.2). Do not add unused config.
- **Publishable key comes from the ini.** The service fills the `<meta id="stripe-publishable-key" content="">` in the served shell from `stripe.publishable_key` at boot, writing the filled copy as `index-supplied.html` beside `index.html` in `static_dir` and serving that. (The dev mock still substitutes `$STRIPE_PUBLISHABLE_KEY`, since it runs without an ini.) The committed shell ships with an empty meta.
- **CSP is a reverse-proxy concern** (the service serves no security headers and binds to localhost). Document the required policy (spec §7.3): `script-src 'self' https://js.stripe.com https://*.js.stripe.com; frame-src https://js.stripe.com https://*.js.stripe.com https://hooks.stripe.com`, Link disabled.
- **Tests run from the repo root** (fixtures use a relative path; a `cd web/` breaks ~55 badge tests). Never `sleep` to wait; bound every test with `timeout` like `withProvider`. Do not delete `tests/tmp`.
- **Secrets never touch the repo or chat.** Real Stripe keys live only in the operator's gitignored `badge_service.ini`.

**User decisions (already made):** "Full card lane now" (adapter + config + webhook + startup wiring + tests, replacing the 503 stub). "Same branch, new commits" on `sh/badges-codes-new`. "Live card settlement is a user-gate the user runs with their own Stripe keys; I never handle secrets."

---

## File Structure

- **Create** `apps/simplex-badge-service/src/BadgeService/Providers/Stripe.hs` — the adapter (create/read/cancel/list/verifyWebhook).
- **Create** `tests/Bots/FakeStripe.hs` — in-process Warp fake Stripe + control surface + `Stripe-Signature` signing.
- **Create** `tests/Bots/BadgeStripeTests.hs` — adapter tests driven against the fake.
- **Create** `apps/simplex-badge-service/test-fixtures/stripe/*.json` — Checkout Session + session-list fixtures.
- **Modify** `apps/simplex-badge-service/src/BadgeService/Config.hs` — `StripeConfig` record, `stripeSection` parser, remove `refuseStripe`, `knownSettings`/`unknownKeys`, `ServiceConfig.stripe`.
- **Modify** `apps/simplex-badge-service/src/BadgeService/Web/Server.hs` — parameterize `webhookHandler`, add the `["webhooks","stripe"]` route.
- **Modify** `apps/simplex-badge-service/src/BadgeService/Service.hs` — build the Stripe `Provider` and add it to the providers list.
- **Modify** `tests/Bots/BadgeConfigTests.hs` — invert `testStripeRefused` into Stripe acceptance tests.
- **Modify** `tests/Bots/BadgeWebTests.hs` — add a configured-card working-path test (keep the unconfigured-503 test).
- **Modify** `simplex-chat.cabal` — add the three new modules to the exe/test stanzas.
- **Modify** `apps/simplex-badge-service/README.md` and `apps/simplex-badge-service/badge_service.ini.example` — `[stripe]` section, publishable-key deploy note, CSP note.

---

## Task 1: `[stripe]` config section

**Goal:** Parse a `[stripe]` section into a `StripeConfig` and expose it as `ServiceConfig.stripe`, replacing the hard refusal.

**Files:**
- Modify: `apps/simplex-badge-service/src/BadgeService/Config.hs`
- Modify: `tests/Bots/BadgeConfigTests.hs`

**Acceptance Criteria:**
- [ ] `StripeConfig` has `sSecretKey`, `sPublishableKey`, `sWebhookSecret`, `sSessionMinutes`, `sHost`, with a `Show` instance that prints neither `sSecretKey` nor `sWebhookSecret`.
- [ ] A present, complete `[stripe]` section parses to `Just StripeConfig`; `sHost` defaults to `https://api.stripe.com`; `sSessionMinutes` defaults to 60 and is rejected outside 31–1439.
- [ ] An absent `[stripe]` section is `Right Nothing`; a section missing a required key fails at boot naming that key (`stripe.<key>`).
- [ ] `refuseStripe` and its call are gone; `stripe` is in `knownSettings` so unknown `stripe.*` keys are named.
- [ ] `testStripeRefused` is replaced by `testStripeDefaults`, `testStripeAbsent`, `testStripeIncomplete`, `testStripeSessionMinutesRange`.

**Verify:** `cd /project/git/simplex-chat-7 && cabal test simplex-chat-test --test-options='-p BadgeConfig' 2>&1 | tail -20` → all BadgeConfig examples pass.

**Steps:**

- [ ] **Step 1: Add the `StripeConfig` record and redacting `Show`** (mirror `BTCPayConfig`, `Config.hs:61-74`). Stripe bounds the session expiry to 30 min – 24 h (spec §6.2), enforced here.

```haskell
data StripeConfig = StripeConfig
  { sSecretKey :: Text,
    sPublishableKey :: Text,
    sWebhookSecret :: Text,
    sSessionMinutes :: Int,
    sHost :: Text
  }
  deriving (Eq)

-- keeps the restricted key and the signing secret out of logs and errors
instance Show StripeConfig where
  show StripeConfig {sHost} = "stripe " <> T.unpack sHost
```

- [ ] **Step 2: Export `StripeConfig (..)`** in the module export list (`Config.hs:6-18`, beside `BTCPayConfig (..)`), and add `stripe :: Maybe StripeConfig` to `ServiceConfig` (`Config.hs:93-102`).

- [ ] **Step 3: Add the `stripeSection` parser** in the `where` of `parseConfig`, mirroring `btcpaySection` (`Config.hs:253-285`). `defaultStripeHost`, `defaultSessionMinutes`, `minSessionMinutes`, `maxSessionMinutes` are named constants.

```haskell
    defaultStripeHost, defaultSessionMinutes, minSessionMinutes, maxSessionMinutes :: a
    -- (define with concrete types near the other config defaults:)
-- defaultStripeHost = "https://api.stripe.com" :: Text
-- defaultSessionMinutes = 60 :: Int ; minSessionMinutes = 31 ; maxSessionMinutes = 1439 (a minute inside Stripe's 30m-24h)

    stripeSection
      | not (hasSection "stripe") = Right Nothing
      | otherwise = do
          sSecretKey <- required "stripe" "secret_key"
          sPublishableKey <- required "stripe" "publishable_key"
          sWebhookSecret <- required "stripe" "webhook_secret"
          sSessionMinutes <- sessionMinutes
          let sHost = defaultStripeHost
          pure (Just StripeConfig {sSecretKey, sPublishableKey, sWebhookSecret, sSessionMinutes, sHost})
    sessionMinutes = do
      v <- num "stripe" "session_minutes" defaultSessionMinutes
      if v >= minSessionMinutes && v <= maxSessionMinutes
        then Right v
        else Left ("stripe.session_minutes must be between " <> show minSessionMinutes <> " and " <> show maxSessionMinutes <> " minutes")
```

- [ ] **Step 4: Remove `refuseStripe`** (`Config.hs:211-213`) and its call (`Config.hs:169`). In the `parseConfig` record build (`Config.hs:179-186`), add `stripe = <stripeSection result>` beside `btcpay = btc` (bind `str <- stripeSection` above, set `stripe = str`).

- [ ] **Step 5: Register the section's keys** in `knownSettings` (`Config.hs:142-148`):

```haskell
    ("stripe", ["secret_key", "publishable_key", "webhook_secret", "session_minutes"]),
```

Then in `unknownKeys` (`Config.hs:158`) remove the literal `"stripe"` from `ours = map fst knownSettings <> ["issuer", "stripe"]` (now covered by `map fst knownSettings`), leaving `<> ["issuer"]`.

- [ ] **Step 6: Replace the config tests.** In `BadgeConfigTests.hs`, delete `testStripeRefused` (lines 112-118) and its `it` line (line 24). Add a `fullStripeIni` and four tests mirroring `testDefaults`/`testAbsentSection`/`testIncompleteSection` (`BadgeConfigTests.hs:70-105`).

```haskell
fullStripeIni :: T.Text
fullStripeIni = fullIni <> T.unlines
  [ "[stripe]",
    "secret_key = rk_test_x",
    "publishable_key = pk_test_x",
    "webhook_secret = whsec_x"
  ]

testStripeDefaults :: IO ()
testStripeDefaults = withIni fullStripeIni $ \p -> do
  Right cfg <- readServiceConfig p
  case stripe cfg of
    Nothing -> expectationFailure "the stripe section was present"
    Just StripeConfig {sSessionMinutes, sHost} -> do
      sSessionMinutes `shouldBe` 60
      sHost `shouldBe` "https://api.stripe.com"

testStripeAbsent :: IO ()
testStripeAbsent = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  stripe cfg `shouldBe` Nothing

testStripeIncomplete :: IO ()
testStripeIncomplete =
  withIni (T.replace "webhook_secret = whsec_x" "" fullStripeIni) $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "webhook_secret"
      Right _ -> expectationFailure "an incomplete stripe section must fail at boot"

testStripeSessionMinutesRange :: IO ()
testStripeSessionMinutesRange =
  withIni (fullStripeIni <> "session_minutes = 5\n") $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "session_minutes"
      Right _ -> expectationFailure "session_minutes below 31 must fail"
```

Register the four `it` lines beside the other BadgeConfig examples.

- [ ] **Step 7: Run and commit.**

```bash
cd /project/git/simplex-chat-7
cabal test simplex-chat-test --test-options='-p BadgeConfig' 2>&1 | tail -20
git add apps/simplex-badge-service/src/BadgeService/Config.hs tests/Bots/BadgeConfigTests.hs
git commit -m "badges: parse [stripe] config section"
```

---

## Task 2: Stripe adapter — create, read, cancel — with a fake Stripe

**Goal:** `Providers/Stripe.hs` creates a Checkout Session, reads it back into a `PaymentSignal`, and cancels it, all verified against an in-process `FakeStripe`.

**Files:**
- Create: `apps/simplex-badge-service/src/BadgeService/Providers/Stripe.hs`
- Create: `tests/Bots/FakeStripe.hs`
- Create: `tests/Bots/BadgeStripeTests.hs`
- Create: `apps/simplex-badge-service/test-fixtures/stripe/session-open.json`, `session-complete-paid.json`, `session-complete-unpaid.json`, `session-expired.json`
- Modify: `simplex-chat.cabal`

**Acceptance Criteria:**
- [ ] `stripeProvider :: StripeConfig -> IO Provider` builds a `Provider` with `pProvider = PPStripe`.
- [ ] `pCreateInvoice (SPMCard CPStripe) draft` POSTs `mode=payment`, `ui_mode=elements`, `line_items[0][price_data]...` / `amount` and currency, `expires_at`, over HTTP Basic (secret key as username), form-urlencoded; returns `ProviderInvoice{piProviderRef=<session id>, piDestination=SPDCard CPStripe <client_secret>}`.
- [ ] `pReadInvoice sid` GETs `/v1/checkout/sessions/{sid}?expand[]=payment_intent.latest_charge` and maps: `complete`+`paid`→`SigSettled` (amount from `amount_total` minor units, time from the charge's `created`, else read time); `complete`+`unpaid`→`Right Nothing`; `expired`→`SigClosed`; `open`→`Right Nothing`; unknown `status`→`Left ProviderError`.
- [ ] `pCreateInvoice (SPMCrypto _)` → `Left (ProviderError "stripe offers no crypto payment method")` (mirror `BTCPay.hs:123`).
- [ ] `pCancelInvoice sid` POSTs `/v1/checkout/sessions/{sid}/expire`, discarding the body.
- [ ] Tests drive create→read lifecycle (open→complete/unpaid→complete/paid→settled) and cancel against `FakeStripe`, asserting each `PaymentSignal` and the recorded request paths.

**Verify:** `cd /project/git/simplex-chat-7 && cabal test simplex-chat-test --test-options='-p BadgeStripe' 2>&1 | tail -30` → all BadgeStripe examples pass.

**Steps:**

- [ ] **Step 1: Fixtures.** Create the four session fixtures under `test-fixtures/stripe/`, each `{"_fixture": <provenance>, "response": <body>}` (envelope enforced by the loader, mirroring `test-fixtures/btcpay/invoice-settled.json`). `session-complete-paid.json` `response` is a Checkout Session object with `id:"cs_test_settled"`, `object:"checkout.session"`, `status:"complete"`, `payment_status:"paid"`, `amount_total:5400`, `currency:"usd"`, `client_secret:"cs_test_settled_secret"`, and `payment_intent:{ "id":"pi_x", "latest_charge":{ "id":"ch_x", "created":1700000000 } }`. The `_fixture` line names the real endpoint (`GET /v1/checkout/sessions/{id}?expand[]=payment_intent.latest_charge`), states it is from Stripe's documented API schema, NOT live-captured, and lists the fields the fake patches (`id`, `status`, `payment_status`, `amount_total`). `session-open.json` has `status:"open"`, `payment_status:"unpaid"`, a `client_secret`, no charge. `session-complete-unpaid.json` has `status:"complete"`, `payment_status:"unpaid"`. `session-expired.json` has `status:"expired"`.

- [ ] **Step 2: `FakeStripe.hs`** — mirror `FakeBTCPay.hs` (Warp `testWithApplication`, `TVar FakeState`, fixture envelope loader from `test-fixtures/stripe`, request recording, control surface). Build `StripeConfig` directly (bypasses the https check) pointing at the fake:

```haskell
fakeConfig :: Text -> StripeConfig
fakeConfig host = StripeConfig
  { sSecretKey = fakeSecretKey, sPublishableKey = "pk_test_x",
    sWebhookSecret = fakeWebhookSecret, sSessionMinutes = 60, sHost = host }
```

Router paths: `POST v1/checkout/sessions` (create → serve `session-open` fixture patched with a fresh `cs_test_<n>` id + the requested `amount_total`/`currency`, record the form body), `GET v1/checkout/sessions/{id}` (read → serve the fixture selected by the invoice's control state, patched with `id`/`status`/`payment_status`/`amount_total`), `POST v1/checkout/sessions/{id}/expire` (cancel), `GET v1/checkout/sessions` (list → Task 3). Control endpoints `_state/{id}`, `_fail`, `_fixtures`, `_paging` as in `FakeBTCPay`. Check `Authorization` is HTTP Basic with `fakeSecretKey` as username (401 otherwise). Add `stripeSigHeader`/`stripeHexSig` for Task 3:

```haskell
-- Stripe-Signature: t=<unix>,v1=<hex HMAC-SHA256 of "t.body">
stripeSigHeader :: Text -> Int -> LB.ByteString -> [Header]
stripeSigHeader secret t body =
  [("Stripe-Signature", "t=" <> B8.pack (show t) <> ",v1=" <> stripeHexSig secret t body)]

stripeHexSig :: Text -> Int -> LB.ByteString -> ByteString
stripeHexSig secret t body = convertToBase Base16 digest
  where
    signed = TE.encodeUtf8 (T.pack (show t) <> ".") <> LB.toStrict body
    digest :: Digest SHA256
    digest = hmacGetDigest (hmac (TE.encodeUtf8 secret) signed :: HMAC SHA256)
```

- [ ] **Step 3: `Providers/Stripe.hs` skeleton + `stripeProvider`** (mirror `BTCPay.hs:226-241`). Pragmas `LambdaCase, NamedFieldPuns, OverloadedStrings, ScopedTypeVariables`. Imports as `BTCPay.hs:32-75` plus `Data.ByteString.Base64` is NOT needed (Stripe uses hex like BTCPay); for HTTP Basic use `Network.HTTP.Client.applyBasicAuth` (import from `Network.HTTP.Client`).

```haskell
data StripeEnv = StripeEnv {seCfg :: StripeConfig, seManager :: Manager}

stripeProvider :: StripeConfig -> IO Provider
stripeProvider cfg = do
  seManager <- newTlsManager
  let env = StripeEnv {seCfg = cfg, seManager}
  pure Provider
    { pProvider = PPStripe,
      pCreateInvoice = createInvoice env,
      pReadInvoice = readInvoice env,
      pCancelInvoice = cancelInvoice env,
      pListOpen = listOpen env,               -- Task 3
      pVerifyWebhook = verifyStripeSig (sWebhookSecret cfg)  -- Task 3
    }
```

- [ ] **Step 4: The bounded HTTP helper** `stripeApi` (mirror `greenfield`, `BTCPay.hs:502-547`), but form-urlencoded body + HTTP Basic. `maxProviderBytes` / `maxErrorBytes` are named constants as in BTCPay.

```haskell
stripeApi :: StripeEnv -> Text -> Method -> [Text] -> Query -> Maybe [(ByteString, ByteString)]
          -> IO (Either ProviderError LB.ByteString)
stripeApi StripeEnv {seCfg, seManager} what verb segments query form = do
  r <- try $ do
    req0 <- parseRequest (T.unpack url)
    let req = applyBasicAuth (TE.encodeUtf8 (sSecretKey seCfg)) "" req0
          { method = verb,
            requestHeaders = [("Accept", "application/json")]
              <> [("Content-Type", "application/x-www-form-urlencoded") | isJust form],
            requestBody = RequestBodyBS (maybe "" (renderSimpleQuery False) form) }
    withResponse req seManager $ \resp ->
      (,) (statusCode (responseStatus resp)) <$> brReadSome (responseBody resp) (fromIntegral maxProviderBytes + 1)
  pure $ case r of
    Left (e :: HttpException) -> Left (ProviderError (what <> " failed: " <> tshow e))
    Right (code, taken)
      | LB.length taken > maxProviderBytes -> Left (ProviderError (what <> " failed: response over cap"))
      | code >= 200 && code < 300 -> Right taken
      | otherwise -> Left (ProviderError (what <> " failed: HTTP " <> tshow code <> " " <> snippet taken))
  where
    url = T.dropWhileEnd (== '/') (sHost seCfg) <> "/" <> T.intercalate "/" segments
            <> TE.decodeUtf8 (renderQuery True query)
    snippet = safeDecodeUtf8 . LB.toStrict . LB.take maxErrorBytes
```

(`renderSimpleQuery` and `applyBasicAuth` from `Network.HTTP.Types`/`Network.HTTP.Client`; `RequestBodyBS` from `Network.HTTP.Client`.)

- [ ] **Step 5: `createInvoice`.** Refuse crypto; build the Checkout Session form; decode `{id, client_secret}`. `expires_at` is `now + sSessionMinutes*60` as a Unix timestamp. Amount is the draft's minor units and currency lowercased.

```haskell
createInvoice :: StripeEnv -> ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice)
createInvoice _ (SPMCrypto _) _ = pure (Left (ProviderError "stripe offers no crypto payment method"))
createInvoice env (SPMCard CPStripe) OrderDraft {odAmount = CurrencyAmount minor, odCurrency} = do
  now <- getPOSIXTime
  let expiresAt = round now + fromIntegral (sSessionMinutes (seCfg env)) * 60 :: Integer
      form =
        [ ("mode", "payment"),
          ("ui_mode", "elements"),
          ("expires_at", B8.pack (show expiresAt)),
          ("line_items[0][quantity]", "1"),
          ("line_items[0][price_data][currency]", TE.encodeUtf8 (T.toLower odCurrency)),
          ("line_items[0][price_data][unit_amount]", B8.pack (show minor)),
          ("line_items[0][price_data][product_data][name]", "SimpleX supporter badge") ]
  created <- stripeApi env "create session" methodPost ["v1", "checkout", "sessions"] [] (Just form)
  pure $ created >>= decodeStripe "create session" >>= \CreatedSession {csId, csClientSecret} ->
    Right ProviderInvoice {piProviderRef = csId, piDestination = SPDCard CPStripe csClientSecret}

data CreatedSession = CreatedSession {csId :: Text, csClientSecret :: Text}
instance J.FromJSON CreatedSession where
  parseJSON = J.withObject "checkout session" $ \o ->
    CreatedSession <$> o J..: "id" <*> o J..: "client_secret"
```

`decodeStripe` mirrors `decodeGreenfield` (`BTCPay.hs:544-547`): `J.eitherDecode'` → `Left ProviderError` on parse failure.

- [ ] **Step 6: `readInvoice`.** GET with the expand, then map `(status, payment_status)` to a signal. `amountFrom` converts `amount_total` minor units to `CurrencyAmount`. No `SigFunded` on this lane.

```haskell
readInvoice :: StripeEnv -> Text -> IO (Either ProviderError (Maybe PaymentSignal))
readInvoice env sid = do
  now <- getCurrentTime
  got <- stripeApi env "read session" methodGet ["v1", "checkout", "sessions", sid]
           [("expand[]", Just "payment_intent.latest_charge")] Nothing
  pure $ got >>= decodeStripe ("read session " <> sid) >>= \SessionRead {srStatus, srPaymentStatus, srAmountTotal, srChargeCreated} ->
    let received = Received {rcvAmount = CurrencyAmount srAmountTotal, rcvCrypto = Nothing, rcvDue = Nothing}
        settledAt = maybe now posixSecondsToUTCTime srChargeCreated
     in case (srStatus, srPaymentStatus) of
          ("complete", "paid")   -> Right (Just (SigSettled received settledAt))
          ("complete", _)        -> Right Nothing              -- still processing
          ("expired", _)         -> Right (Just (SigClosed received))
          ("open", _)            -> Right Nothing
          (other, _)             -> Left (ProviderError ("stripe session " <> sid <> ": unknown status " <> other))

data SessionRead = SessionRead
  { srStatus :: Text, srPaymentStatus :: Text, srAmountTotal :: Int64, srChargeCreated :: Maybe POSIXTime }
instance J.FromJSON SessionRead where
  parseJSON = J.withObject "checkout session" $ \o -> do
    srStatus <- o J..: "status"
    srPaymentStatus <- o J..:? "payment_status" J..!= "unpaid"
    srAmountTotal <- o J..:? "amount_total" J..!= 0
    pi_ <- o J..:? "payment_intent"
    srChargeCreated <- case pi_ of
      Just (J.Object p) -> case KM.lookup "latest_charge" p of
        Just (J.Object c) -> fmap (fmap fromInteger) (parseMaybeCreated c)
        _ -> pure Nothing
      _ -> pure Nothing
    pure SessionRead {srStatus, srPaymentStatus, srAmountTotal, srChargeCreated}
    where parseMaybeCreated c = pure (KM.lookup "created" c >>= asInteger)
```

(Provide a small `asInteger :: J.Value -> Maybe Integer` helper over `J.Number`. `amount_total` is already integer minor units, so no `WireNum` is needed — Stripe sends integers, not decimal strings, unlike BTCPay.)

- [ ] **Step 7: `cancelInvoice`** (mirror `BTCPay.hs:308-315`).

```haskell
cancelInvoice :: StripeEnv -> Text -> IO (Either ProviderError ())
cancelInvoice env sid =
  fmap (fmap (const ())) $ stripeApi env "expire session" methodPost ["v1", "checkout", "sessions", sid, "expire"] [] (Just [])
```

- [ ] **Step 8: Cabal wiring.** Add `BadgeService.Providers.Stripe` to the `other-modules` of the exe/library stanza that lists `BadgeService.Providers.BTCPay`, and `Bots.FakeStripe`, `Bots.BadgeStripeTests` to the test stanza that lists `Bots.FakeBTCPay`/`Bots.BadgeBTCPayTests` in `simplex-chat.cabal` (grep those names to find the stanzas).

- [ ] **Step 9: `BadgeStripeTests.hs`** — mirror `BadgeBTCPayTests.hs` `withProvider` (lines 522-543). Drive create→read lifecycle and cancel.

```haskell
withProvider :: HasCallStack => (FakeStripe -> Provider -> IO a) -> IO a
withProvider action = bounded $ withFakeStripe $ \fake -> stripeProvider (fsConfig fake) >>= action fake
  where bounded act = timeout exampleCeiling act >>= maybe (failWith "the fake stripe did not answer within 20s") pure

testFakeCreatesCard :: IO ()
testFakeCreatesCard = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef, piDestination} <- createdInvoice p (SPMCard CPStripe)
  piDestination `shouldSatisfy` isCardSecret       -- SPDCard CPStripe <client_secret>
  fakeSessionIds fake `shouldReturn` [piProviderRef]

testFakeLifecycle :: IO ()
testFakeLifecycle = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = sid} <- createdInvoice p (SPMCard CPStripe)
  pReadInvoice p sid `shouldReturn` Right Nothing                                   -- open
  setSessionState fake sid ["status" .= ("complete" :: Text), "payment_status" .= ("unpaid" :: Text)]
  pReadInvoice p sid `shouldReturn` Right Nothing                                   -- processing
  setSessionState fake sid ["payment_status" .= ("paid" :: Text)]
  pReadInvoice p sid `shouldReturn` Right (Just (SigSettled fiftyFourDollarsReceived fixtureChargeAt))
  pCancelInvoice p sid `shouldReturn` Right ()

testRefusesCrypto :: IO ()
testRefusesCrypto = withProvider $ \_ p ->
  pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars >>= \r -> r `shouldSatisfy` isLeft
```

Provide `createdInvoice`, `fiftyFourDollars`, `fiftyFourDollarsReceived`, `fixtureChargeAt`, `isCardSecret`, `fakeSessionIds`, `setSessionState` (control-surface client helpers on `FakeStripe`, mirroring `setInvoiceState`).

- [ ] **Step 10: Run and commit.**

```bash
cd /project/git/simplex-chat-7
cabal test simplex-chat-test --test-options='-p BadgeStripe' 2>&1 | tail -30
git add apps/simplex-badge-service/src/BadgeService/Providers/Stripe.hs tests/Bots/FakeStripe.hs tests/Bots/BadgeStripeTests.hs apps/simplex-badge-service/test-fixtures/stripe simplex-chat.cabal
git commit -m "badges: Stripe adapter create, read and cancel"
```

---

## Task 3: Stripe adapter — list-open and webhook verify

**Goal:** Complete the `Provider` record: `pListOpen` (the poller's list pass) and `pVerifyWebhook` (the `/webhooks/stripe` signature check).

**Files:**
- Modify: `apps/simplex-badge-service/src/BadgeService/Providers/Stripe.hs`
- Modify: `tests/Bots/FakeStripe.hs` (list endpoint + paging control)
- Modify: `tests/Bots/BadgeStripeTests.hs`
- Create: `apps/simplex-badge-service/test-fixtures/stripe/session-list.json`

**Acceptance Criteria:**
- [ ] `pListOpen` GETs `/v1/checkout/sessions?created[gte]=<now − settleWindow − session lifetime>&limit=100` — a created-time window, NOT `status=open` (which would return only open sessions and never surface a settled one). While `has_more` it pages with `starting_after=<last id>`, capped at 50 pages; returns `ListPass{lpMoved, lpSkipped}` where `lpMoved` holds each session id whose read yields a signal (a settled list row settles at read time, since a list carries no expanded charge) and `lpSkipped` records a session with no id or one whose status this build does not know. A `429` backs off the whole pass to `Left ProviderError` (spec §6.5).
- [ ] `pVerifyWebhook` parses `Stripe-Signature: t=…,v1=…`, recomputes `HMAC-SHA256(secret, "{t}.{body}")`, compares constant-time, and returns `Right (Just <session id>)` only for `checkout.session.completed`/`…expired`, else `Right Nothing`; a bad/missing signature is `Left WebhookError`. The 300 s replay window is not enforced (the interface is pure).
- [ ] Tests: a list pass over the fixture returns the open session ids; a valid signature over a completed event yields `Just sid`; a tampered signature and a stale timestamp are refused; an unhandled event type yields `Right Nothing`.

**Verify:** `cd /project/git/simplex-chat-7 && cabal test simplex-chat-test --test-options='-p BadgeStripe' 2>&1 | tail -30` → all BadgeStripe examples pass.

**Steps:**

- [ ] **Step 1: `session-list.json` fixture** — `response` is a Stripe list object `{ "object":"list", "has_more":false, "data":[ <mixed-status checkout.session objects: a complete/paid `cs_test_a`, an expired `cs_test_b`, and one with no id> ] }`. Provenance names `GET /v1/checkout/sessions?created[gte]=…&limit=…`, documented schema, not live-captured, and notes list rows carry no expanded charge.

- [ ] **Step 2: `listOpen`** in `Stripe.hs`. `listPageSize = 100`, `maxListPages = 50` (named constants; spec §6.5). Page with `starting_after`; treat a `429` in the shared helper's error as a pass abort. For each session in `data`, map its `(status, payment_status)` with the same logic as `readInvoice` (factor the mapping into a shared `signalOf :: UTCTime -> SessionRead -> Either ProviderError (Maybe PaymentSignal)` used by both). A session missing an `id` goes to `lpSkipped`. Mirror the pagination shape of `listOpen` in `BTCPay.hs:330-356`.

```haskell
listOpen :: StripeEnv -> IO (Either ProviderError ListPass)
listOpen env = go Nothing 0 []
  where
    go _ page acc | page >= maxListPages = pure (Right (ListPass (concat (reverse acc)) []))
    go after page acc = do
      let query = [("status", Just "open"), ("limit", Just (B8.pack (show listPageSize)))]
                    <> maybe [] (\a -> [("starting_after", Just (TE.encodeUtf8 a))]) after
      got <- stripeApi env "list sessions" methodGet ["v1", "checkout", "sessions"] query Nothing
      -- decode { data :: [SessionRead+id], has_more :: Bool }; append moved; recurse on has_more with last id
      ...
```

(Write the full body: decode a `SessionList {slData :: [ListedSession], slHasMore :: Bool}`, where `ListedSession` carries `id` plus the `SessionRead` fields; map each with `signalOf`; accumulate `(id, signal)` into `lpMoved` and id-less rows into `lpSkipped`; recurse while `slHasMore` using the last row's id as `starting_after`.)

- [ ] **Step 3: `verifyStripeSig`** (mirror `verifyBTCPaySig`, `BTCPay.hs:476-500`, but Stripe's header format). Constant-time compare with `constEq`; the 300 s replay tolerance is NOT enforced here, because the interface is pure and holds no clock (see the note below). Reads `t` and every `v1` from the comma-separated header; the signed payload is `"{t}.{body}"`.

```haskell
actedOnStripeEvents :: [Text]
actedOnStripeEvents = ["checkout.session.completed", "checkout.session.expired"]

verifyStripeSig :: Text -> [Header] -> ByteString -> Either WebhookError (Maybe Text)
verifyStripeSig secret hdrs body = do
  raw <- note "missing Stripe-Signature header" (lookup "Stripe-Signature" hdrs)
  (t, v1hex) <- note "Stripe-Signature is not t=..,v1=.." (parseSig raw)
  given <- case convertFromBase Base16 (B8.map toLower v1hex) of
    Right (bs :: ByteString) -> Right bs
    Left (_ :: String) -> Left (WebhookError "Stripe-Signature v1 is not hex")
  let signed = t <> "." <> body
      expected = hmacGetDigest (hmac (TE.encodeUtf8 secret) signed :: HMAC SHA256) :: Digest SHA256
  if constEq expected given then Right (actedOn body) else Left (WebhookError "Stripe-Signature does not verify")
  where
    note e = maybe (Left (WebhookError e)) Right
    -- parseSig splits "t=..,v1=.." on commas then '=' ; returns (t bytes, v1 hex bytes)
    actedOn b = do
      SEvent {seType, seSessionId} <- J.decodeStrict' b
      if seType `elem` actedOnStripeEvents then Just seSessionId else Nothing

data SEvent = SEvent {seType :: Text, seSessionId :: Text}
instance J.FromJSON SEvent where
  parseJSON = J.withObject "stripe event" $ \o -> do
    seType <- o J..: "type"
    dataO <- o J..: "data"
    obj <- dataO J..: "object"
    seSessionId <- obj J..: "id"
    pure SEvent {seType, seSessionId}
```

(The 300 s tolerance check: compare `t` against `now` in the caller? `pVerifyWebhook` is pure and has no clock. Follow the spec's note that tolerance is a library convention, not an API rule — implement tolerance by having the FAKE always sign with a current `t`, and add a pure `staleBy :: Int -> ...` only if a clock is threaded. Since `pVerifyWebhook` is pure by interface, DO NOT add a clock; verify signature only, and record in the haddock that replay-window enforcement is deferred because the interface is pure. This matches BTCPay, which also does not check a timestamp.)

- [ ] **Step 4: FakeStripe list + signing.** Add the `GET v1/checkout/sessions` handler serving `session-list.json` (patched for paging when `_paging` is set), and confirm `stripeSigHeader`/`stripeHexSig` from Task 2 Step 2 are present.

- [ ] **Step 5: Tests** for list and webhook verify (mirror `testFakeLifecycle` list assertions and `testFakeWebhookSecretWiring`, `BadgeBTCPayTests.hs:701-706`).

```haskell
testFakeListsOpen :: IO ()
testFakeListsOpen = withProvider $ \_ p -> do
  Right ListPass {lpMoved} <- pListOpen p
  map fst lpMoved `shouldContain` ["cs_test_a"]

testWebhookVerifies :: IO ()
testWebhookVerifies = withProvider $ \fake p -> do
  let secret = sWebhookSecret (fsConfig fake)
      body = stripeEvent "checkout.session.completed" "cs_test_a"
  pVerifyWebhook p (stripeSigHeader secret 1700000000 body) (LB.toStrict body) `shouldBe` Right (Just "cs_test_a")
  pVerifyWebhook p (stripeSigHeader (secret <> "0") 1700000000 body) (LB.toStrict body) `shouldSatisfy` isRefused
  pVerifyWebhook p (stripeSigHeader secret 1700000000 (stripeEvent "charge.refunded" "cs_test_a")) (LB.toStrict (stripeEvent "charge.refunded" "cs_test_a")) `shouldBe` Right Nothing
```

Provide `stripeEvent :: Text -> Text -> LB.ByteString` (a minimal `{type, data:{object:{id}}}` event) in `FakeStripe.hs`.

- [ ] **Step 6: Run and commit.**

```bash
cd /project/git/simplex-chat-7
cabal test simplex-chat-test --test-options='-p BadgeStripe' 2>&1 | tail -30
git add apps/simplex-badge-service/src/BadgeService/Providers/Stripe.hs tests/Bots/FakeStripe.hs tests/Bots/BadgeStripeTests.hs apps/simplex-badge-service/test-fixtures/stripe/session-list.json
git commit -m "badges: Stripe adapter list-open and webhook verify"
```

---

## Task 4: Wire Stripe into the service

**Goal:** Build the Stripe `Provider` at startup, add the `/webhooks/stripe` route, and generalize `webhookHandler` so a configured card checkout creates and settles end to end.

**Files:**
- Modify: `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`
- Modify: `apps/simplex-badge-service/src/BadgeService/Service.hs`
- Modify: `tests/Bots/BadgeWebTests.hs`

**Acceptance Criteria:**
- [ ] `webhookHandler :: WebEnv -> PaymentProvider -> Text -> Request -> Respond -> IO ResponseReceived`; the `PPCrypto` uses (Server.hs:641, 674) and the `route` string (656) come from the arguments.
- [ ] `webApp` routes `["webhooks","btcpay"]` → `webhookHandler env PPCrypto "POST /webhooks/btcpay"` and `["webhooks","stripe"]` → `webhookHandler env PPStripe "POST /webhooks/stripe"`, both POST-only.
- [ ] `Service.hs` `serviceLanes` builds a Stripe provider from `stripe sc` and includes it in the `providers` list feeding both `newWebEnv` and `newPollerEnv`.
- [ ] With a Stripe provider configured, `POST /api/invoice` `method:"card"` returns 200 with a `clientSecret` (not 503); the existing unconfigured-Stripe test still returns 503.
- [ ] A signed `checkout.session.completed` to `/webhooks/stripe` queues a read; a poller pass then settles the card invoice and issues the code.

**Verify:** `cd /project/git/simplex-chat-7 && cabal test simplex-chat-test --test-options='-p BadgeWeb' 2>&1 | tail -30` → all BadgeWeb examples pass, including the new card-path examples.

**Steps:**

- [ ] **Step 1: Parameterize `webhookHandler`** (Server.hs:630-683). Change the signature to `webhookHandler :: WebEnv -> PaymentProvider -> Text -> Request -> Respond -> IO ResponseReceived`, bind the provider and route from arguments, and replace the two `PPCrypto` literals (641 `providerNamed env PPCrypto`, 674 `irProvider /= PPCrypto`) with the passed `PaymentProvider`, and the `route` where-binding (656) with the passed label.

- [ ] **Step 2: Add the routes** in `webApp` (Server.hs:291):

```haskell
  ["webhooks", "btcpay"] -> only "POST" $ webhookHandler env PPCrypto "POST /webhooks/btcpay" req respond
  ["webhooks", "stripe"] -> only "POST" $ webhookHandler env PPStripe "POST /webhooks/stripe" req respond
```

- [ ] **Step 3: Build the provider at startup** (Service.hs:158-166). Import `stripeProvider`. Replace line 162 so the list holds both optional providers:

```haskell
      btc <- maybe (pure []) (fmap (: []) . btcpayProvider) (btcpay sc)
      str <- maybe (pure []) (fmap (: []) . stripeProvider) (stripe sc)
      let providers = btc <> str
```

- [ ] **Step 4: Web tests.** In `BadgeWebTests.hs`, add a helper that builds a `WebEnv` whose `weProviders` includes a Stripe provider pointed at a `FakeStripe` (reuse `withFakeStripe` + `stripeProvider`), then:
  - `testCardCreatesSession`: `POST /api/invoice` `method:"card"` returns 200 with a `clientSecret` and writes an invoice row with `provider = stripe`.
  - `testCardWebhookSettles`: create a card invoice, POST a signed `checkout.session.completed` to `/webhooks/stripe`, run a poller pass, assert the code moves to `paid`.
  - Keep the existing unconfigured-card 503 example (card is `provider_unavailable` when no Stripe provider is configured) — assert it explicitly still holds.

Model the request/settlement drive on the existing BTCPay web examples (`testServesTheBuild`/webhook examples) and the poller integration in `BadgeWebTests.hs`.

- [ ] **Step 5: Run the full badge suites and commit.**

```bash
cd /project/git/simplex-chat-7
cabal build exe:simplex-badge-service 2>&1 | tail -5
cabal test simplex-chat-test --test-options='-p Badge' 2>&1 | tail -30
git add apps/simplex-badge-service/src/BadgeService/Web/Server.hs apps/simplex-badge-service/src/BadgeService/Service.hs tests/Bots/BadgeWebTests.hs
git commit -m "badges: wire Stripe provider and webhook route"
```

---

## Task 5: Documentation — `[stripe]` config, publishable key, CSP

**Goal:** Document the operator setup: the `[stripe]` section, the deploy-time publishable-key substitution, and the required CSP.

**Files:**
- Modify: `apps/simplex-badge-service/README.md`
- Modify: `apps/simplex-badge-service/badge_service.ini.example`

**Acceptance Criteria:**
- [ ] `badge_service.ini.example` has a commented `[stripe]` block with `secret_key` (`rk_…` restricted), `publishable_key` (`pk_…`), `webhook_secret` (`whsec_…`), `session_minutes` (default 60, range 31–1439), noting card is disabled when the section is absent.
- [ ] `README.md` documents: the `[stripe]` keys; that the deployment substitutes `<meta id="stripe-publishable-key" content="">` in the served `index.html` (the mock uses `$STRIPE_PUBLISHABLE_KEY`); the `/webhooks/stripe` endpoint and that the restricted key needs Checkout Sessions scope; the CSP the reverse proxy must send (`script-src 'self' https://js.stripe.com https://*.js.stripe.com; frame-src https://js.stripe.com https://*.js.stripe.com https://hooks.stripe.com`) and that Link must be off in the Dashboard.

**Verify:** `cd /project/git/simplex-chat-7 && grep -c '\[stripe\]' apps/simplex-badge-service/badge_service.ini.example` → `1`; manual read of README confirms the three items.

**Steps:**

- [ ] **Step 1** Add the `[stripe]` block to `badge_service.ini.example` mirroring the `[btcpay]` block's comment style, values as placeholders (never a real key).
- [ ] **Step 2** Add a "Card payments (Stripe)" subsection to `README.md` covering the four config keys, the meta-tag substitution, the webhook endpoint + restricted-key scope, and the CSP + Link-off note.
- [ ] **Step 3: Commit.**

```bash
cd /project/git/simplex-chat-7
git add apps/simplex-badge-service/README.md apps/simplex-badge-service/badge_service.ini.example
git commit -m "badges: document Stripe card configuration"
```

---

## Task 6: Live Stripe verification (user gate)

**Goal:** Confirm a real card payment through a real Stripe test account creates a session, mounts the Payment Element, settles via webhook and poll, and issues a code.

**USER-ORDERED GATE — NON-SKIPPABLE.** This task was requested by the user in the current conversation. It MUST NOT be closed by walking around it, by declaring it "verified inline", or by substituting a cheaper check. Close only after every item in `acceptanceCriteria` has been re-validated independently, with output captured.

**Files:** none (operational).

**Acceptance Criteria:**
- [ ] The user configures `[stripe]` in their gitignored `badge_service.ini` with their own `rk_test_`/`pk_test_`/`whsec_` keys and points a Stripe webhook at `/webhooks/stripe`.
- [ ] A test-mode card checkout reaches the payment screen with a mounted Payment Element, confirms, and the service moves the code to `paid` and reveals it — observed by the user.
- [ ] The Stripe Dashboard shows the Checkout Session `complete`/`paid`; the service log shows the webhook queued a read and the poller settled.

**Verify:** The user runs the flow with their own Stripe test account and confirms settlement + code issuance. The agent never handles the keys and cannot run this.

**Steps:**
- [ ] **Step 1** Hand off to the user with the README setup steps; the agent does not proceed past this gate on its own.

---

## Self-Review

**Spec coverage (§6.2 Stripe):** Checkout Session + `ui_mode=elements` (Task 2 create) ✓; restricted key over HTTP Basic (Task 2 `stripeApi` `applyBasicAuth`) ✓; `client_secret` to browser (Task 2 → existing `destinationPairs`) ✓; read with `expand[]=payment_intent.latest_charge` (Task 2 read) ✓; settle on `payment_status=paid`, time by charge `created` (Task 2 read) ✓; two acted-on events, HMAC `t.body`, replay window deferred (Task 3 verify) ✓; list by a `created` window with paging (Task 3 list) ✓; no support ref / no return_url (Global Constraints) ✓; publishable key + CSP (Task 5) ✓. **Gap check:** `payment_intent.payment_failed`, `async_payment_*`, `charge.refunded/dispute` are all "not acted on" — covered by the acted-on allowlist returning `Right Nothing`; no task needed.

**Placeholder scan:** `listOpen` (Task 3 Step 2) and `parseSig` (Task 3 Step 3) are shown as skeletons with the decoder shape spelled out but not every line; flagged inline as "write the full body" with the exact record fields and recursion described. All type names, function names, and signatures are concrete.

**Type consistency:** `StripeConfig`/`sSecretKey`/`sWebhookSecret`/`sSessionMinutes`/`sHost` consistent across Tasks 1–4; `StripeEnv`/`stripeApi`/`createInvoice`/`readInvoice`/`cancelInvoice`/`listOpen`/`verifyStripeSig` consistent Tasks 2–4; `SessionRead`/`signalOf` shared between read and list; `pProvider = PPStripe` matches `providerOf (SPMCard CPStripe) = PPStripe` (Server.hs:571). `FakeStripe`/`fsConfig`/`setSessionState`/`stripeSigHeader`/`stripeEvent` consistent Tasks 2–4.
