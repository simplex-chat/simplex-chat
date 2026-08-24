{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.BadgeServiceTests where

import BadgeService.Catalog (catalogTotals, defaultCatalog, offerTotal, seedCatalog)
import BadgeService.Config
  ( BadgeServiceConfig (..),
    BadgeServiceEnv (..),
    BucketLimits (..),
    -- 'CodesConfig'/'IssuerConfig' import only their constructors, not '(..)': their field
    -- 'issuerKeyFile' is already a pervasive local variable name below (writeTestBadgeServiceSecrets
    -- and its many callers), so importing the field selector too would shadow it (Werror).
    CodesConfig (CodesConfig),
    IssuerConfig (IssuerConfig),
    SignerBucketFamily (..),
    ThrottleConfig (..),
    checkFailureBuckets,
    debitFailureBuckets,
    newBadgeServiceEnv,
    readBadgeServiceConfig,
    sweepSignerBuckets,
  )
import BadgeService.Credentials (issueSignedBadge, loadIssuerKey)
import BadgeService.Options
import BadgeService.Service
import BadgeService.Store
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (replicateM, void)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.List (find, isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (DiffTime, UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime, nominalDay, secondsToDiffTime)
import Data.Word (Word32)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeMasterKey (..), BadgeRequest (..), BadgeType (..), verifyCredential)
import Simplex.Chat.Badges.Service
  ( -- 'BadgeBalance', 'StatementEntry' and 'StatementEntryType' import only their
    -- constructors, not '(..)': their field names (entryId, changeMonths, balanceMonths,
    -- createdAt, ...) duplicate 'BadgeLedgerEntry''s (Badges.Types), which the existing B1
    -- ledger tests below already use as bare selectors -- importing the field selectors here
    -- too would make those pre-existing, untouched uses ambiguous.
    BadgeBalance (BadgeBalance),
    BadgeCatalog (..),
    BadgeOffer (..),
    BadgePrice (..),
    BadgeServiceCommand (..),
    BadgeServiceErrorCode (..),
    BadgeServiceRequest (..),
    BadgeServiceResponse (..),
    StatementCreditType (SCOpening),
    StatementEntry (StatementEntry),
    StatementEntryType (SECredit),
    pattern VersionBadgeService,
  )
import Simplex.Chat.Badges.Types
  ( BadgeItemStatus (..),
    BadgeLedgerEntry (..),
    BadgeOfferId (..),
    BadgePurchaseStatus (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (..),
    OfferDiscount (..),
  )
import Simplex.Chat.Controller (ChatConfig, ChatController (chatStore))
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.PaymentService (ServicePayment (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withConnection)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, createDBStore)
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationConfig (..), MigrationConfirmation (..), MigrationsToRun (..), toDownMigration)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSPublicKey (..), BBSSecretKey (..), bbsKeyGen)
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec hiding (it)
#if defined(dbPostgres)
import BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations)
import Database.PostgreSQL.Simple (Only (..))
import qualified Simplex.Messaging.Agent.Store.Postgres.Migrations as Migrations
#else
import BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations)
import Database.SQLite.Simple (Only (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
#endif

badgeServiceTests :: SpecWith TestParams
badgeServiceTests = do
  it "should respond unsupported_version to a request below minSupportedBadgeVersion" testBadgeServiceUnsupportedVersion
  it "should respond bad_request to a request that fails to decode" testBadgeServiceMalformedRequest
  it "should respond bad_request when purchaseKey differs from the verified signer" testBadgeServiceSignerMismatch
  it "should respond unknown_purchase_key to issueBadge from an unknown key" testBadgeServiceIssueBadgeUnknownKey
  it "should never respond unknown_purchase_key to purchaseBadge from an unknown key" testBadgeServicePurchaseBadgeUnknownKeyIsNotUnknownPurchaseKey
  it "should respond bad_request to pauseBadge from a signer with an existing purchase" testBadgeServicePauseBadgeKnownSignerBadRequest
  it "should respond unknown_purchase_key to pauseBadge from an unknown key" testBadgeServicePauseBadgeUnknownKey
  it "should respond bad_request to purchaseBadge funded by apple" testBadgeServicePurchaseBadgeAppleBadRequest
  it "should reject purchaseBadge{code} with rate_limited before processing when the per-signer bucket is drained" testBadgeServicePurchaseCodeThrottlePreCheck
  it "should turn a pure exception forced only during response encoding into internal, without escaping runHandler" testBadgeServiceCatchAllContainsPureException
  it "should not grow the per-signer bucket map from checks alone, across many distinct keys" testBadgeServiceThrottlePeekDoesNotGrowMap
  it "should add exactly one bucket entry per distinct key that actually fails, and let a sweep evict recovered ones" testBadgeServiceThrottleDebitBoundedAndSweepEvicts
  it "should reject a [throttle] capacity of 0 at config-parse time, naming the key" testBadgeServiceConfigThrottleZeroCapacity
  it "should migrate web_orders, codes and provider_events up and down" testBadgeServiceWebOrderSchemaMigration
  it "should seed the catalog idempotently and preserve a deprecated price" testBadgeServiceCatalogSeeding
  it "should price 3 months at 2x and 12 months at 6x the monthly price" testBadgeCatalogOfferTotal
  it "should fill total for every seeded offer" testBadgeCatalogTotalsFillsSeededOffers
  it "should reject an offer with freeMonths >= months instead of wrapping" testBadgeCatalogOfferTotalRejectsBadFreeMonths
  it "should encode BadgeItemStatus on the wire as active/deprecated/disabled" testBadgeItemStatusJsonWireFormat
  it "should fail to start on a missing config file, naming the file" testBadgeServiceConfigMissingFile
  it "should fail to start on an unparsable value, naming the key" testBadgeServiceConfigUnparsableValue
  it "should fail to start on an unknown key in a known section" testBadgeServiceConfigUnknownKey
  it "should fail to start on a missing [issuer] section" testBadgeServiceConfigMissingIssuerSection
  it "should fail to start on a missing [codes] section" testBadgeServiceConfigMissingCodesSection
  it "should fail to start on a half-configured [btcpay] section" testBadgeServiceConfigHalfConfiguredBtcPay
  it "should fail to start on a half-configured [stripe] section" testBadgeServiceConfigHalfConfiguredStripe
  it "should fail to start when a provider is configured without [web]" testBadgeServiceConfigProviderRequiresWeb
  it "should start with just [issuer] and [codes], no provider section" testBadgeServiceConfigMinimalStarts
  it "should start the service from a complete config with web and both providers" testBadgeServiceCompleteConfigStarts
  it "should create a purchase and append ledger entries readable back in order" testBadgeStorePurchaseAndLedger
  it "should disable a price out of the active catalog while both stay reachable by id" testBadgeStoreSetPriceStatusDisabled
  it "should return the redeeming purchase key from getCodeByHash" testBadgeStoreGetCodeByHashRedeemer
  it "should clear both redemption columns and set unredeemed_at" testBadgeStoreUnredeemCode
  it "should sign a credential that verifies with the matching public key, and fail with a different one" testBadgeCredentialSignAndVerify
  it "should set badgeExpiry to the next Sunday at 23:59:59 UTC" testBadgeCredentialExpiryIsSundayEndOfDay
  it "should roll a periodEnd already on a Sunday to the following Sunday" testBadgeCredentialExpirySundayRollsToFollowingSunday
  it "should reject a badgeRequest with non-empty badgeExtra as bad_request" testBadgeCredentialRejectsNonEmptyBadgeExtra
  it "should load a valid issuer key file" testBadgeIssuerKeyLoadsValidFile
  it "should fail fast on a missing issuer key file" testBadgeIssuerKeyMissingFile
  it "should fail fast on an issuer key file without a 'secret' line" testBadgeIssuerKeyMalformedFile
  it "should fail fast on a non-positive key_idx" testBadgeIssuerKeyNonPositiveIdx

badgeProfile :: Profile
badgeProfile = Profile {displayName = "SimpleX Badges", fullName = "", shortDescr = Nothing, description = Nothing, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences = Nothing, badge = Nothing, contactDomain = Nothing}

serviceDbPrefix :: FilePath
serviceDbPrefix = "badge_service"

mkBadgeServiceOpts :: TestParams -> BadgeServiceOpts
mkBadgeServiceOpts TestParams {tmpPath = ps} =
  BadgeServiceOpts
    { coreOptions =
        testCoreOpts
          { dbOptions =
              (dbOptions testCoreOpts)
#if defined(dbPostgres)
                {dbSchemaPrefix = "client_" <> serviceDbPrefix}
#else
                {dbFilePrefix = ps </> serviceDbPrefix}
#endif
          },
      serviceName = "SimpleX Badges",
      clientService = True,
      noAddress = False,
      runCLI = False,
      testing = True,
      configFile = badgeServiceConfigPath ps
    }

badgeServiceConfigPath :: FilePath -> FilePath
badgeServiceConfigPath tmpPath = tmpPath </> "badge_service.ini"

-- Generates a real issuer key file (the two-line `badge keygen` output: "secret ..\npublic
-- ..") and a real 32-byte code secret (base64-encoded), at fixed names under tmpPath. A6
-- doesn't read either file's contents -- that's B3 and B4 -- but the harness must still hand
-- every later step real files at real paths, per the config keys naming secrets as files.
writeTestBadgeServiceSecrets :: FilePath -> IO (FilePath, FilePath)
writeTestBadgeServiceSecrets tmpPath = do
  let issuerKeyFile = tmpPath </> "badge-issuer.keys"
      codeSecretFile = tmpPath </> "badge-code.secret"
  Right (BBSPublicKey pk, BBSSecretKey sk) <- bbsKeyGen
  writeFile issuerKeyFile $ "secret " <> BC.unpack (strEncode sk) <> "\npublic " <> BC.unpack (strEncode pk) <> "\n"
  codeSecret <- getRandomBytes 32
  writeFile codeSecretFile $ BC.unpack (B64.encode codeSecret) <> "\n"
  pure (issuerKeyFile, codeSecretFile)

issuerCodesIniLines :: FilePath -> FilePath -> [String]
issuerCodesIniLines issuerKeyFile codeSecretFile =
  [ "[issuer]",
    "key_file = " <> issuerKeyFile,
    "key_idx = 1",
    "",
    "[codes]",
    "secret_file = " <> codeSecretFile,
    "default_expiry_days = 365"
  ]

-- Writes a complete but minimal badge_service.ini (required sections only, no provider
-- section) at the path mkBadgeServiceOpts points BadgeServiceOpts's configFile at. Provider
-- sections are omitted until E2 and F1 add them.
writeTestBadgeServiceConfig :: TestParams -> IO ()
writeTestBadgeServiceConfig TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  writeFile (badgeServiceConfigPath tmpPath) $ unlines (issuerCodesIniLines issuerKeyFile codeSecretFile)

withBadgeService :: HasCallStack => TestParams -> (TestCC -> String -> IO ()) -> IO ()
withBadgeService ps = withBadgeServiceConfig ps (writeTestBadgeServiceConfig ps) (pure ())

-- Shared by withBadgeService and testBadgeServiceCompleteConfigStarts: the two-phase startup
-- dance (CreateMyAddress, then ShowMyAddress) is the same regardless of what the config looks
-- like, as long as it's valid; writeConfig is what varies. 'betweenPhases' runs after the
-- first phase's badge service has been killed and before the second one starts: the ONLY
-- window where nothing holds the database open, so a test that needs to seed a row directly
-- (e.g. B1's createPurchase, for a "known signer" case) must do it here, via a fresh
-- 'withFreshBadgeStore' -- opening a second connection to the SAME sqlite file WHILE the
-- service's own phase is running deadlocks against its writer lock (verified: reliably fails
-- 'createDBStore' with a pattern-match-on-Right, i.e. sqlite busy, when tried in that window).
withBadgeServiceConfig :: HasCallStack => TestParams -> IO () -> IO () -> (TestCC -> String -> IO ()) -> IO ()
withBadgeServiceConfig ps writeConfig betweenPhases test = do
  let opts = mkBadgeServiceOpts ps
  writeConfig
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  -- First start: badge service takes the CreateMyAddress branch.
  runBadgeService testCfg opts (pure ())
  -- Reopen the DB to read the link the service created.
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (sLink, _) <- getContactLinks bs False
    bs <## "auto_accept off"
    pure sLink
  betweenPhases
  -- Second start: badge service takes the ShowMyAddress branch, then serves the test body.
  runBadgeService testCfg opts $
    withNewTestChatCfg ps testCfg "client" bobProfile $ \client ->
      test client bsLink

runBadgeService :: ChatConfig -> BadgeServiceOpts -> IO () -> IO ()
runBadgeService cfg opts action = do
  t <- forkIO $ badgeService opts cfg
  threadDelay 500000
  action `finally` killThread t

-- B5 RPC dispatcher -----------------------------------------------------------

-- Sends the JSON-encoded 'BadgeServiceRequest' unsigned.
sendServiceRequest :: TestCC -> String -> BadgeServiceRequest -> IO ()
sendServiceRequest client bsLink req =
  client ##> ("/_service_request 1 " <> bsLink <> " " <> LBC.unpack (J.encode req))

-- Sends the JSON-encoded 'BadgeServiceRequest' signed with 'priv' (the agent verifies the
-- signature and delivers the corresponding public key as CEvtServiceRequest's signerKey).
sendSignedServiceRequest :: TestCC -> String -> C.PrivateKeyEd25519 -> BadgeServiceRequest -> IO ()
sendSignedServiceRequest client bsLink priv req =
  client
    ##> ( "/_service_request 1 " <> bsLink <> " sign_key=" <> BC.unpack (strEncode (C.StoredPrivateKey priv))
            <> " "
            <> LBC.unpack (J.encode req)
        )

-- Reads one raw "service response: {...}" line and returns the decoded JSON object, for
-- assertions that can't be pinned to one exact line (e.g. a retryAfter whose value depends on
-- wall-clock timing).
getServiceResponseObject :: HasCallStack => TestCC -> IO J.Object
getServiceResponseObject client = do
  line <- getTermLine client
  case T.stripPrefix "service response: " (T.pack line) of
    Just json | Just (J.Object o) <- J.decode (LBC.pack (T.unpack json)) -> pure o
    _ -> expectationFailure ("expected a service response line, got: " <> line) >> error "unreachable"

testBadgeRequestCommand :: BadgeMasterKey -> ServicePayment -> BadgeServiceCommand
testBadgeRequestCommand masterKey payment =
  BSCPurchaseBadge {badgeRequest = testBadgeRequest masterKey, payment, upgrade = Nothing}

-- StatementEntry/BadgeBalance/StatementEntryType are constructed positionally: only their
-- constructors are imported (see the import list above), not their field selectors, to avoid
-- colliding with BadgeLedgerEntry's identically-named fields used elsewhere in this file.
-- Field order: entryId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType,
-- wasPausedSince, createdAt, entryType.
testIssueBadgeCommand :: BadgeMasterKey -> UTCTime -> BadgeServiceCommand
testIssueBadgeCommand masterKey now =
  BSCIssueBadge
    { badgeRequest = testBadgeRequest masterKey,
      balance = BadgeBalance (StatementEntry "test-entry" 1 1 now BTSupporter Nothing now (SECredit SCOpening))
    }

-- version 0 is below minSupportedBadgeVersion (1): the version gate must reject it before
-- looking at the command at all, so 'getBadgeCatalog' (which needs no other fields) is enough
-- to isolate the gate.
testBadgeServiceUnsupportedVersion :: HasCallStack => TestParams -> IO ()
testBadgeServiceUnsupportedVersion ps =
  withBadgeService ps $ \client bsLink -> do
    client ##> ("/_service_request 1 " <> bsLink <> " {\"version\":0,\"request\":{\"type\":\"getBadgeCatalog\"}}")
    client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"

-- A syntactically valid JSON object that does not decode into BadgeServiceRequest (missing
-- version and request) must fail at step 1, before the version gate ever runs.
testBadgeServiceMalformedRequest :: HasCallStack => TestParams -> IO ()
testBadgeServiceMalformedRequest ps =
  withBadgeService ps $ \client bsLink -> do
    client ##> ("/_service_request 1 " <> bsLink <> " {\"foo\":\"bar\"}")
    client <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"

-- A request signed by one key but asserting a different key as purchaseKey must be rejected
-- regardless of what it asks for.
testBadgeServiceSignerMismatch :: HasCallStack => TestParams -> IO ()
testBadgeServiceSignerMismatch ps =
  withBadgeService ps $ \client bsLink -> do
    (_signerPub, signerPriv) <- mkTestKeyPair
    (assertedPub, _assertedPriv) <- mkTestKeyPair
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just assertedPub, request = BSCGetBadgeCatalog}
    sendSignedServiceRequest client bsLink signerPriv req
    client <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"

-- issueBadge always requires an existing purchase record; a fresh, never-purchased key must
-- be rejected with unknown_purchase_key before it ever reaches the (not yet implemented) B7
-- handler.
testBadgeServiceIssueBadgeUnknownKey :: HasCallStack => TestParams -> IO ()
testBadgeServiceIssueBadgeUnknownKey ps =
  withBadgeService ps $ \client bsLink -> do
    (pub, priv) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = testIssueBadgeCommand masterKey now}
    sendSignedServiceRequest client bsLink priv req
    client <## "service response: {\"code\":\"unknown_purchase_key\",\"type\":\"error\"}"

-- The rule easy to get backwards (B5 brief): purchaseBadge from an unknown key is the normal
-- first-purchase case, not an identity error. Before B7 lands it reaches the not-implemented
-- handler (internal); after B7 the code classifier answers it -- either way, the assertion
-- that must hold across that later change is that it is never unknown_purchase_key.
testBadgeServicePurchaseBadgeUnknownKeyIsNotUnknownPurchaseKey :: HasCallStack => TestParams -> IO ()
testBadgeServicePurchaseBadgeUnknownKeyIsNotUnknownPurchaseKey ps =
  withBadgeService ps $ \client bsLink -> do
    (pub, priv) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = testBadgeRequestCommand masterKey (SPCode "UNKNOWN-CODE")}
    sendSignedServiceRequest client bsLink priv req
    respObj <- getServiceResponseObject client
    KM.lookup "code" respObj `shouldNotBe` Just (J.String "unknown_purchase_key")
    KM.lookup "code" respObj `shouldBe` Just (J.String "internal")

-- pauseBadge is always bad_request (decision 5 / §6), but ONLY once the signer/record
-- precondition passes -- a signer with a real purchase row (B1's createPurchase) must reach
-- that bad_request, not an identity error.
testBadgeServicePauseBadgeKnownSignerBadRequest :: HasCallStack => TestParams -> IO ()
testBadgeServicePauseBadgeKnownSignerBadRequest ps = do
  (pub, priv) <- mkTestKeyPair
  masterKey <- BadgeMasterKey <$> getRandomBytes 32
  now <- getCurrentTime
  -- The purchase row is seeded in the gap between withBadgeServiceConfig's two startup
  -- phases, reopening the SAME already-migrated store the harness itself reopens there to
  -- read the invite link (chatStore, via a plain TestCC) -- NOT a fresh createDBStore with
  -- just badgeServiceSchemaMigrations, which builds an isolated, from-scratch database (as
  -- withFreshBadgeStore's other callers rely on) and mismatches against the real one, already
  -- carrying the full chat/agent migration history the live service ran.
  let seedPurchase =
        withTestChat ps serviceDbPrefix $ \bs -> do
          bs <## "subscribed 1 connections on server localhost" -- consume, as the harness's own reopen does
          void $ expectRight $ withServiceTransaction (chatStore (chatController bs)) $ \db -> createPurchase db pub masterKey BTSupporter now
  withBadgeServiceConfig ps (writeTestBadgeServiceConfig ps) seedPurchase $ \client bsLink -> do
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = BSCPauseBadge}
    sendSignedServiceRequest client bsLink priv req
    client <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"

-- The same command from a key with no purchase row at all must fail the identity check
-- instead, before pauseBadge's own (always bad_request) handling ever runs.
testBadgeServicePauseBadgeUnknownKey :: HasCallStack => TestParams -> IO ()
testBadgeServicePauseBadgeUnknownKey ps =
  withBadgeService ps $ \client bsLink -> do
    (pub, priv) <- mkTestKeyPair
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = BSCPauseBadge}
    sendSignedServiceRequest client bsLink priv req
    client <## "service response: {\"code\":\"unknown_purchase_key\",\"type\":\"error\"}"

-- Store-evidence verification is out of scope (§6): every non-code payment method is
-- permanently bad_request, not "not implemented".
testBadgeServicePurchaseBadgeAppleBadRequest :: HasCallStack => TestParams -> IO ()
testBadgeServicePurchaseBadgeAppleBadRequest ps =
  withBadgeService ps $ \client bsLink -> do
    (pub, priv) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = testBadgeRequestCommand masterKey (SPApple {jws = "test-jws"})}
    sendSignedServiceRequest client bsLink priv req
    client <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"

-- With the per-signer bucket started at capacity 1 and zero tokens ([throttle] override, B5
-- decision 5), a single purchaseBadge{code} must be rejected BEFORE processing, with
-- rate_limited and a non-zero retryAfter -- the accounting (debit-on-failure) is B10's, this
-- step only asserts the pre-processing check.
testBadgeServicePurchaseCodeThrottlePreCheck :: HasCallStack => TestParams -> IO ()
testBadgeServicePurchaseCodeThrottlePreCheck ps = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets (tmpPath ps)
  let writeConfig =
        writeFile (badgeServiceConfigPath (tmpPath ps)) $
          unlines $
            issuerCodesIniLines issuerKeyFile codeSecretFile
              ++ ["", "[throttle]", "signer_failure_capacity = 1", "signer_failure_start_tokens = 0"]
  withBadgeServiceConfig ps writeConfig (pure ()) $ \client bsLink -> do
    (pub, priv) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = testBadgeRequestCommand masterKey (SPCode "TEST-CODE")}
    sendSignedServiceRequest client bsLink priv req
    respObj <- getServiceResponseObject client
    KM.lookup "code" respObj `shouldBe` Just (J.String "rate_limited")
    case KM.lookup "retryAfter" respObj of
      Just (J.Number n) -> n `shouldSatisfy` (> 0)
      other -> expectationFailure $ "expected a positive retryAfter, got: " <> show other

-- The convincing form (B5 brief): forces a genuine 'error' thunk hidden behind a Just, so it
-- is NOT forced by constructing or returning the response -- only by fully encoding it, which
-- is exactly the laziness gap a "catch around only the IO action" would miss. runHandler must
-- catch it, respond internal, and remain usable for the next call (proving the exception did
-- not corrupt anything or propagate past this function -- the property that keeps
-- processQueuedRequests' single-threaded forever loop alive for every other user).
testBadgeServiceCatchAllContainsPureException :: HasCallStack => TestParams -> IO ()
testBadgeServiceCatchAllContainsPureException _ps = do
  let boom = error "boom: pure exception forced only during response encoding, not before" :: Text
      badResponse = BSPError {code = BSEInternal, message = Just boom, retryAfter = Nothing}
  caughtObj <- runHandler "test-req-pure-exception" (pure badResponse)
  KM.lookup "code" caughtObj `shouldBe` Just (J.String "internal")
  KM.lookup "message" caughtObj `shouldBe` Nothing -- never leaks the caught exception's own text
  goodObj <- runHandler "test-req-after-pure-exception" (pure $ BSPError {code = BSEBadRequest, message = Nothing, retryAfter = Nothing})
  KM.lookup "code" goodObj `shouldBe` Just (J.String "bad_request")

-- Builds a real BadgeServiceEnv directly (real issuer key + code secret files, production-
-- shaped [throttle] defaults) against an already-migrated store, without going through a live
-- service -- so the throttle's own STM state can be inspected and driven directly.
mkTestBadgeServiceEnv :: TestParams -> DBStore -> IO BadgeServiceEnv
mkTestBadgeServiceEnv TestParams {tmpPath} st = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let cfg =
        BadgeServiceConfig
          { issuer = IssuerConfig issuerKeyFile 1,
            codes = CodesConfig codeSecretFile 365,
            web = Nothing,
            btcpay = Nothing,
            stripe = Nothing,
            service = Nothing,
            reconcile = Nothing,
            throttle =
              ThrottleConfig
                { signerFailure = BucketLimits {blCapacity = 10, blStartTokens = 10},
                  globalFailure = BucketLimits {blCapacity = 600, blStartTokens = 600},
                  catalog = BucketLimits {blCapacity = 600, blStartTokens = 600}
                }
          }
  newBadgeServiceEnv cfg st

-- Fix round 1 (unbounded per-signer map): the convincing form the review asked for. Driving
-- many distinct, never-before-seen signer keys through the pre-processing throttle check
-- (checkFailureBuckets, called for every signed purchaseBadge{code}) must NOT insert anything
-- into the per-signer bucket map -- a pre-check, however many times repeated or against
-- however many distinct attacker-minted keys, costs nothing. SignerBucketFamily's Haddock
-- states the property this proves directly: only an actual debit (a real failed redemption)
-- can grow the map.
testBadgeServiceThrottlePeekDoesNotGrowMap :: HasCallStack => TestParams -> IO ()
testBadgeServiceThrottlePeekDoesNotGrowMap ps =
  withFreshBadgeStore ps $ \st -> do
    bsEnv <- mkTestBadgeServiceEnv ps st
    keys <- replicateM 500 (fst <$> mkTestKeyPair)
    mapM_ (checkFailureBuckets bsEnv) keys
    mapSize <- Map.size <$> readTVarIO (sbBuckets (signerFailureBucket bsEnv))
    mapSize `shouldBe` 0

-- The other half: an ACTUAL failure (debitFailureBuckets, called by B7 after a classified
-- code_invalid/used/expired) DOES cost exactly one map entry per distinct signer -- the
-- intended, bounded cost (bounded by the shared global failure budget, since every debit also
-- spends one of its tokens; see SignerBucketFamily's Haddock). A sweep, given a `now'` far
-- enough past for that signer's own bucket to have fully refilled -- the injectable clock,
-- not a real sleep -- then reclaims every such entry.
testBadgeServiceThrottleDebitBoundedAndSweepEvicts :: HasCallStack => TestParams -> IO ()
testBadgeServiceThrottleDebitBoundedAndSweepEvicts ps =
  withFreshBadgeStore ps $ \st -> do
    bsEnv <- mkTestBadgeServiceEnv ps st
    keys <- replicateM 20 (fst <$> mkTestKeyPair)
    mapM_ (debitFailureBuckets bsEnv) keys
    sizeAfterDebits <- Map.size <$> readTVarIO (sbBuckets (signerFailureBucket bsEnv))
    sizeAfterDebits `shouldBe` 20 -- exactly one entry per distinct key that actually failed
    -- 1 hour is far more than the 6 minutes a capacity-10/10-per-hour bucket needs to regain
    -- the single token one debit spent; using the injectable clock, not a real sleep.
    wellPastFullRefill <- addUTCTime 3600 <$> getCurrentTime
    evicted <- atomically $ sweepSignerBuckets wellPastFullRefill (signerFailureBucket bsEnv)
    evicted `shouldBe` 20
    sizeAfterSweep <- Map.size <$> readTVarIO (sbBuckets (signerFailureBucket bsEnv))
    sizeAfterSweep `shouldBe` 0

-- Fix round 1 (minor): a [throttle] capacity of 0 would otherwise reach bucketStatus's own
-- guard (an `error`, since a bucket that never refills has no finite retryAfter) and get
-- silently swallowed into a spurious internal by the catch-all. An operator typo should fail
-- fast at config-parse time instead, like every other malformed value in this file, naming
-- the offending key.
testBadgeServiceConfigThrottleZeroCapacity :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigThrottleZeroCapacity TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "zero-capacity-throttle.ini"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ ["", "[throttle]", "signer_failure_capacity = 0"]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("signer_failure_capacity" `isInfixOf`)

-- Applies every migration except 20260821_badge_service_web, then exercises that one
-- migration's up/down/up cycle directly, checking the three new tables appear and
-- disappear as expected.
testBadgeServiceWebOrderSchemaMigration :: HasCallStack => TestParams -> IO ()
testBadgeServiceWebOrderSchemaMigration ps = do
  let dbOpts = toDBOpts (dbOptions $ coreOptions $ mkBadgeServiceOpts ps) chatSuffix False chatDBFunctions
      priorMigrations = filter ((/= migrationName) . name) badgeServiceSchemaMigrations
      webOrderMigration = fromJust $ find ((== migrationName) . name) badgeServiceSchemaMigrations
      downMigration = fromJust $ toDownMigration webOrderMigration
  Right st <- createDBStore dbOpts priorMigrations (MigrationConfig MCError Nothing)
  mapM_ (assertTableMissing st) newTables
  runMigrationsToRun st $ MTRUp [webOrderMigration]
  mapM_ (assertTableExists st) newTables
  runMigrationsToRun st $ MTRDown [downMigration]
  mapM_ (assertTableMissing st) newTables
  runMigrationsToRun st $ MTRUp [webOrderMigration]
  mapM_ (assertTableExists st) newTables
  closeDBStore st
  where
    migrationName = "20260821_badge_service_web"
    newTables = ["sx_badge_service_web_orders", "sx_badge_service_codes", "sx_badge_service_provider_events"]
    assertTableExists st tbl =
      withConnection st (\db -> DB.query_ db (tableCountQuery tbl)) `shouldReturn` [Only (0 :: Int)]
    assertTableMissing st tbl = do
      r <- try (withConnection st (\db -> DB.query_ db (tableCountQuery tbl))) :: IO (Either SomeException [Only Int])
      case r of
        Left _ -> pure ()
        Right rows -> expectationFailure $ tbl <> " should not exist after down migration, got: " <> show rows
    tableCountQuery tbl = fromString ("SELECT count(*) FROM " <> tbl)

-- Seeds a fresh database, seeds again, and asserts price/offer row counts are unchanged;
-- then deprecates one price directly with SQL (B1's setPriceStatus doesn't exist yet) and
-- asserts a further re-seed leaves it deprecated rather than reviving it.
testBadgeServiceCatalogSeeding :: HasCallStack => TestParams -> IO ()
testBadgeServiceCatalogSeeding ps = do
  let dbOpts = toDBOpts (dbOptions $ coreOptions $ mkBadgeServiceOpts ps) chatSuffix False chatDBFunctions
  Right st <- createDBStore dbOpts badgeServiceSchemaMigrations (MigrationConfig MCError Nothing)
  seedCatalog st
  pricesAfterFirstSeed <- rowCount st "sx_badge_service_badge_prices"
  offersAfterFirstSeed <- rowCount st "sx_badge_service_badge_offers"
  pricesAfterFirstSeed `shouldBe` 2
  offersAfterFirstSeed `shouldBe` 4
  [Only deprecatedPriceId] <-
    withConnection st (\db -> DB.query_ db "SELECT price_id FROM sx_badge_service_badge_prices LIMIT 1") :: IO [Only Text]
  withConnection
    st
    (\db -> DB.execute db "UPDATE sx_badge_service_badge_prices SET status = 'deprecated' WHERE price_id = ?" (Only deprecatedPriceId))
  seedCatalog st
  pricesAfterSecondSeed <- rowCount st "sx_badge_service_badge_prices"
  offersAfterSecondSeed <- rowCount st "sx_badge_service_badge_offers"
  pricesAfterSecondSeed `shouldBe` pricesAfterFirstSeed
  offersAfterSecondSeed `shouldBe` offersAfterFirstSeed
  [Only statusAfterReseed] <-
    withConnection st (\db -> DB.query db "SELECT status FROM sx_badge_service_badge_prices WHERE price_id = ?" (Only deprecatedPriceId)) :: IO [Only Text]
  statusAfterReseed `shouldBe` "deprecated"
  closeDBStore st
  where
    rowCount :: DBStore -> String -> IO Int
    rowCount st tbl = do
      [Only n] <- withConnection st (\db -> DB.query_ db (fromString ("SELECT count(*) FROM " <> tbl)))
      pure n

-- offerTotal must price 3 months at exactly 2x the monthly price and 12 months at exactly
-- 6x, for both badge types (UX §6.12).
testBadgeCatalogOfferTotal :: HasCallStack => TestParams -> IO ()
testBadgeCatalogOfferTotal _ps = do
  now <- getCurrentTime
  let BadgeCatalog {prices, offers} = defaultCatalog now
      priceFor pid = fromJust $ find (\BadgePrice {priceId = pid'} -> pid' == pid) prices
  mapM_ (assertOfferTotal priceFor) offers
  where
    assertOfferTotal priceFor offer@BadgeOffer {months, priceId = Just pid} = do
      let BadgePrice {monthPrice = CurrencyAmount monthly} = priceFor pid
          CurrencyAmount total = offerTotal (priceFor pid) (Just offer)
          multiplier = if months == 3 then 2 else 6 :: Word32
      total `shouldBe` monthly * multiplier
    assertOfferTotal _ BadgeOffer {priceId = Nothing} =
      expectationFailure "seeded offer must be pinned to a price"

-- catalogTotals must fill total for all four seeded offers.
testBadgeCatalogTotalsFillsSeededOffers :: HasCallStack => TestParams -> IO ()
testBadgeCatalogTotalsFillsSeededOffers _ps = do
  now <- getCurrentTime
  let BadgeCatalog {offers} = catalogTotals (defaultCatalog now)
  length offers `shouldBe` 4
  all (\BadgeOffer {total} -> isJust total) offers `shouldBe` True

-- A Word8 subtraction of freeMonths from months is unsigned and unguarded: an offer with
-- freeMonths >= months (a typo, a future repricing) would wrap silently
-- (3 - 12 :: Word8 == 247) and hand out a wildly wrong charge. offerTotal must instead fail
-- loudly, naming the offer, before it ever reaches that subtraction.
testBadgeCatalogOfferTotalRejectsBadFreeMonths :: HasCallStack => TestParams -> IO ()
testBadgeCatalogOfferTotalRejectsBadFreeMonths _ps = do
  now <- getCurrentTime
  let BadgeCatalog {prices} = defaultCatalog now
      price@BadgePrice {priceId} = fromJust $ find (\BadgePrice {badgeType} -> badgeType == BTSupporter) prices
      badOffer =
        BadgeOffer
          { offerId = BadgeOfferId "test-bad-offer-freeMonths-ge-months",
            priceId = Just priceId,
            months = 3,
            discount = ODFreeMonths 12,
            status = BISActive,
            createdAt = now,
            total = Nothing
          }
  result <- try (evaluate (offerTotal price (Just badOffer))) :: IO (Either SomeException CurrencyAmount)
  case result of
    Left _ -> pure ()
    Right (CurrencyAmount total) ->
      expectationFailure $ "offerTotal should reject freeMonths >= months, got: " <> show total

-- BadgeItemStatus's JSON crosses the wire (BadgePrice/BadgeOffer.status), so pinning finding
-- 2's TextEncoding-derived encoding to what the earlier TH-derived instance produced proves
-- the change is invisible on the wire, not just asserted to be.
testBadgeItemStatusJsonWireFormat :: HasCallStack => TestParams -> IO ()
testBadgeItemStatusJsonWireFormat _ps = do
  J.encode BISActive `shouldBe` "\"active\""
  J.encode BISDeprecated `shouldBe` "\"deprecated\""
  J.encode BISDisabled `shouldBe` "\"disabled\""

testBadgeServiceConfigMissingFile :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMissingFile TestParams {tmpPath} = do
  let path = tmpPath </> "missing.ini"
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` (path `isInfixOf`)

testBadgeServiceConfigUnparsableValue :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigUnparsableValue TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "unparsable.ini"
  writeFile path $
    unlines
      [ "[issuer]",
        "key_file = " <> issuerKeyFile,
        "key_idx = not-a-number",
        "",
        "[codes]",
        "secret_file = " <> codeSecretFile,
        "default_expiry_days = 365"
      ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("key_idx" `isInfixOf`)

testBadgeServiceConfigUnknownKey :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigUnknownKey TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "unknown-key.ini"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[web]",
             "port = 8080",
             "base_url = https://badges.example.org",
             "support_contact = https://simplex.chat/contact",
             "bogus_key = 1"
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("bogus_key" `isInfixOf`)

testBadgeServiceConfigMissingIssuerSection :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMissingIssuerSection TestParams {tmpPath} = do
  (_issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "no-issuer.ini"
  writeFile path $ unlines ["[codes]", "secret_file = " <> codeSecretFile, "default_expiry_days = 365"]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("[issuer]" `isInfixOf`)

testBadgeServiceConfigMissingCodesSection :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMissingCodesSection TestParams {tmpPath} = do
  (issuerKeyFile, _codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "no-codes.ini"
  writeFile path $ unlines ["[issuer]", "key_file = " <> issuerKeyFile, "key_idx = 1"]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("[codes]" `isInfixOf`)

testBadgeServiceConfigHalfConfiguredBtcPay :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigHalfConfiguredBtcPay TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "half-btcpay.ini"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[web]",
             "port = 8080",
             "base_url = https://badges.example.org",
             "support_contact = https://simplex.chat/contact",
             "",
             "[btcpay]",
             "url = https://btcpay.example.org",
             "store_id = teststore"
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("btcpay" `isInfixOf`)

testBadgeServiceConfigHalfConfiguredStripe :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigHalfConfiguredStripe TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let stripeKeyFile = tmpPath </> "stripe.key"
      path = tmpPath </> "half-stripe.ini"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[web]",
             "port = 8080",
             "base_url = https://badges.example.org",
             "support_contact = https://simplex.chat/contact",
             "",
             "[stripe]",
             "secret_key_file = " <> stripeKeyFile
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("stripe" `isInfixOf`)

testBadgeServiceConfigProviderRequiresWeb :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigProviderRequiresWeb TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let apiKeyFile = tmpPath </> "btcpay-api.key"
      webhookFile = tmpPath </> "btcpay-webhook.secret"
      path = tmpPath </> "provider-no-web.ini"
  writeFile apiKeyFile "api-key\n"
  writeFile webhookFile "webhook-secret\n"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[btcpay]",
             "url = https://btcpay.example.org",
             "store_id = teststore",
             "api_key_file = " <> apiKeyFile,
             "webhook_secret_file = " <> webhookFile
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("[web]" `isInfixOf`)

testBadgeServiceConfigMinimalStarts :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMinimalStarts TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "minimal.ini"
  writeFile path $ unlines (issuerCodesIniLines issuerKeyFile codeSecretFile)
  result <- readBadgeServiceConfig path
  case result of
    Right BadgeServiceConfig {web} -> web `shouldBe` Nothing
    Left err -> expectationFailure $ "expected a minimal config to parse, got: " <> err

-- Proves a fully populated ini -- issuer, codes, web and both providers -- starts the real
-- service end to end, not just that readBadgeServiceConfig accepts it.
testBadgeServiceCompleteConfigStarts :: HasCallStack => TestParams -> IO ()
testBadgeServiceCompleteConfigStarts ps@TestParams {tmpPath} =
  withBadgeServiceConfig ps writeCompleteConfig (pure ()) $ \client bsLink -> do
    -- What matters here is that the service starts and answers at all; the request omits
    -- purchaseBadge's required badgeRequest on purpose, so the real dispatcher's decode step
    -- (B5) fails it with bad_request -- a stable, step-independent response, unlike e.g.
    -- getBadgeCatalog's answer, which will change once B6 lands.
    let redeemReq =
          "{\"version\":1,\"request\":{\"type\":\"purchaseBadge\",\"payment\":{\"type\":\"code\",\"code\":\"TEST-CODE\"}}}"
    client ##> ("/_service_request 1 " <> bsLink <> " " <> redeemReq)
    client <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"
  where
    writeCompleteConfig = do
      (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
      let apiKeyFile = tmpPath </> "btcpay-api.key"
          btcWebhookFile = tmpPath </> "btcpay-webhook.secret"
          stripeKeyFile = tmpPath </> "stripe.key"
          stripeWebhookFile = tmpPath </> "stripe-webhook.secret"
      writeFile apiKeyFile "btcpay-api-key\n"
      writeFile btcWebhookFile "btcpay-webhook-secret\n"
      writeFile stripeKeyFile "sk_test_123\n"
      writeFile stripeWebhookFile "whsec_test_123\n"
      writeFile (badgeServiceConfigPath tmpPath) $
        unlines $
          issuerCodesIniLines issuerKeyFile codeSecretFile
            ++ [ "",
                 "[web]",
                 "port = 0",
                 "base_url = https://badges.example.org",
                 "support_contact = https://simplex.chat/contact",
                 "",
                 "[btcpay]",
                 "url = https://btcpay.example.org",
                 "store_id = teststore",
                 "api_key_file = " <> apiKeyFile,
                 "webhook_secret_file = " <> btcWebhookFile,
                 "",
                 "[stripe]",
                 "secret_key_file = " <> stripeKeyFile,
                 "webhook_secret_file = " <> stripeWebhookFile
               ]

-- B1 store layer -------------------------------------------------------------

-- A fresh, migrated badge-service database, independent of any running bot: these tests
-- exercise BadgeService.Store directly against BadgeServiceTests' own store, the same way
-- testBadgeServiceCatalogSeeding does.
withFreshBadgeStore :: TestParams -> (DBStore -> IO a) -> IO a
withFreshBadgeStore ps test = do
  let dbOpts = toDBOpts (dbOptions $ coreOptions $ mkBadgeServiceOpts ps) chatSuffix False chatDBFunctions
  Right st <- createDBStore dbOpts badgeServiceSchemaMigrations (MigrationConfig MCError Nothing)
  test st `finally` closeDBStore st

mkTestKeyPair :: IO (C.PublicKeyEd25519, C.PrivateKeyEd25519)
mkTestKeyPair = do
  drg <- C.newRandom
  (pub, priv :: C.PrivateKeyEd25519) <- atomically $ C.generateKeyPair drg
  pure (pub, priv)

expectRight :: HasCallStack => IO (Either ServiceError a) -> IO a
expectRight action =
  action >>= \case
    Right a -> pure a
    Left e -> expectationFailure ("expected Right, got: " <> show e) >> error "unreachable"

-- Creates a purchase, appends three ledger entries (an opening credit and two badge debits)
-- and asserts getLedgerSince returns all three in ascending entry_id order with the expected
-- changeMonths, and getLastLedgerEntry returns exactly the last one appended.
testBadgeStorePurchaseAndLedger :: HasCallStack => TestParams -> IO ()
testBadgeStorePurchaseAndLedger ps =
  withFreshBadgeStore ps $ \st -> do
    (purchaseKey, _) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    BadgePurchaseRow {badgePurchaseId, status} <-
      expectRight $ withServiceTransaction st $ \db -> createPurchase db purchaseKey masterKey BTSupporter now
    status `shouldBe` PSIssued
    let mkEntry changeMonths entryType balanceMonths =
          BadgeLedgerEntry
            { entryId = 0,
              entryUuid = "test-entry-" <> tshow' (badgePurchaseId, changeMonths, balanceMonths),
              badgePurchaseId,
              changeMonths,
              balanceMonths,
              balanceStartTs = now,
              balanceBadgeType = BTSupporter,
              wasPausedSince = Nothing,
              serviceCreatedAt = now,
              createdAt = now,
              entryType
            }
        tshow' = fromString . show
    entry1 <- expectRight $ withServiceTransaction st $ \db -> appendLedgerEntry db (mkEntry 3 (LECredit CTOpening) 3)
    entry2 <- expectRight $ withServiceTransaction st $ \db -> appendLedgerEntry db (mkEntry (-1) (LEDebit DTBadge) 2)
    entry3 <- expectRight $ withServiceTransaction st $ \db -> appendLedgerEntry db (mkEntry (-1) (LEDebit DTBadge) 1)
    entryId entry1 < entryId entry2 && entryId entry2 < entryId entry3 `shouldBe` True
    allEntries <- expectRight $ withServiceTransaction st $ \db -> getLedgerSince db badgePurchaseId Nothing
    map entryId allEntries `shouldBe` [entryId entry1, entryId entry2, entryId entry3]
    map changeMonths allEntries `shouldBe` [3, -1, -1]
    lastEntry <- expectRight $ withServiceTransaction st $ \db -> getLastLedgerEntry db badgePurchaseId
    (entryId <$> lastEntry) `shouldBe` Just (entryId entry3)
    sinceFirst <- expectRight $ withServiceTransaction st $ \db -> getLedgerSince db badgePurchaseId (Just (entryId entry1))
    map entryId sinceFirst `shouldBe` [entryId entry2, entryId entry3]

-- setPriceStatus BISDisabled removes the price and every offer pinned to it from
-- getActiveCatalog, while getPriceById and getOfferById still resolve both.
testBadgeStoreSetPriceStatusDisabled :: HasCallStack => TestParams -> IO ()
testBadgeStoreSetPriceStatusDisabled ps =
  withFreshBadgeStore ps $ \st -> do
    seedCatalog st
    BadgeCatalog {prices = seededPrices} <- expectRight $ withServiceTransaction st getActiveCatalog
    let BadgePrice {priceId = targetPriceId} = head seededPrices
    pinnedOfferIds <- expectRight $ withServiceTransaction st $ \db -> do
      BadgeCatalog {offers} <- getActiveCatalog db
      pure [offerId | BadgeOffer {offerId, priceId = Just pid} <- offers, pid == targetPriceId]
    length pinnedOfferIds > 0 `shouldBe` True
    _ <- expectRight $ withServiceTransaction st $ \db -> setPriceStatus db targetPriceId BISDisabled
    BadgeCatalog {prices = pricesAfter, offers = offersAfter} <- expectRight $ withServiceTransaction st getActiveCatalog
    any (\BadgePrice {priceId} -> priceId == targetPriceId) pricesAfter `shouldBe` False
    any (\BadgeOffer {priceId} -> priceId == Just targetPriceId) offersAfter `shouldBe` False
    priceStillById <- expectRight $ withServiceTransaction st $ \db -> getPriceById db targetPriceId
    case priceStillById of
      Just BadgePrice {status} -> status `shouldBe` BISDisabled
      Nothing -> expectationFailure "disabled price should still resolve by id"
    mapM_
      ( \oid -> do
          offerStillById <- expectRight $ withServiceTransaction st $ \db -> getOfferById db oid
          isJust offerStillById `shouldBe` True
      )
      pinnedOfferIds

-- Inserts a code, marks it redeemed by one purchase's key, and asserts getCodeByHash returns
-- that same purchase key alongside the code row.
testBadgeStoreGetCodeByHashRedeemer :: HasCallStack => TestParams -> IO ()
testBadgeStoreGetCodeByHashRedeemer ps =
  withFreshBadgeStore ps $ \st -> do
    (purchaseKey, _) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    BadgePurchaseRow {badgePurchaseId} <-
      expectRight $ withServiceTransaction st $ \db -> createPurchase db purchaseKey masterKey BTSupporter now
    codeHash <- getRandomBytes 32
    let expiresAt = addUTCTime (30 * nominalDay) now
        newCode = NewBadgeCode {codeHash, badgeType = BTSupporter, months = 3, batch = "test-batch", expiresAt}
    _ <- expectRight $ withServiceTransaction st $ \db -> insertCodes db [newCode] now
    beforeRedeem <- expectRight $ withServiceTransaction st $ \db -> getCodeByHash db codeHash
    case beforeRedeem of
      Just (_, redeemer) -> redeemer `shouldBe` Nothing
      Nothing -> expectationFailure "code should exist before redemption"
    _ <- expectRight $ withServiceTransaction st $ \db -> markCodeRedeemed db codeHash badgePurchaseId now
    afterRedeem <- expectRight $ withServiceTransaction st $ \db -> getCodeByHash db codeHash
    case afterRedeem of
      Just (BadgeCode {redeemedPurchaseId}, redeemer) -> do
        redeemedPurchaseId `shouldBe` Just badgePurchaseId
        redeemer `shouldBe` Just purchaseKey
      Nothing -> expectationFailure "code should exist after redemption"

-- unredeemCode clears redeemed_purchase_id and redeemed_at and sets unredeemed_at, re-opening
-- the code for another redemption.
testBadgeStoreUnredeemCode :: HasCallStack => TestParams -> IO ()
testBadgeStoreUnredeemCode ps =
  withFreshBadgeStore ps $ \st -> do
    (purchaseKey, _) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    BadgePurchaseRow {badgePurchaseId} <-
      expectRight $ withServiceTransaction st $ \db -> createPurchase db purchaseKey masterKey BTSupporter now
    codeHash <- getRandomBytes 32
    let expiresAt = addUTCTime (30 * nominalDay) now
        newCode = NewBadgeCode {codeHash, badgeType = BTSupporter, months = 3, batch = "test-batch", expiresAt}
    _ <- expectRight $ withServiceTransaction st $ \db -> insertCodes db [newCode] now
    _ <- expectRight $ withServiceTransaction st $ \db -> markCodeRedeemed db codeHash badgePurchaseId now
    unredeemAt <- getCurrentTime
    _ <- expectRight $ withServiceTransaction st $ \db -> unredeemCode db codeHash unredeemAt
    Just (BadgeCode {redeemedPurchaseId, redeemedAt, unredeemedAt}, redeemer) <-
      expectRight $ withServiceTransaction st $ \db -> getCodeByHash db codeHash
    redeemedPurchaseId `shouldBe` Nothing
    redeemedAt `shouldBe` Nothing
    -- Postgres TIMESTAMPTZ truncates to microseconds, so an exact Haskell UTCTime round-trip
    -- isn't guaranteed on that backend; comparing within a second confirms the column was
    -- actually set to the given time without depending on sub-microsecond precision surviving.
    case unredeemedAt of
      Just storedUnredeemedAt -> abs (diffUTCTime storedUnredeemedAt unredeemAt) < 1 `shouldBe` True
      Nothing -> expectationFailure "unredeemed_at should be set after unredeemCode"
    redeemer `shouldBe` Nothing

-- B4 issuer key + credential signing -----------------------------------------

sundayEndOfDay :: DiffTime
sundayEndOfDay = secondsToDiffTime (23 * 3600 + 59 * 60 + 59)

testBadgeRequest :: BadgeMasterKey -> BadgeRequest
testBadgeRequest masterKey = BadgeRequest {masterKey, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Nothing, badgeExtra = ""}}

-- Signs a real credential (via issueSignedBadge, never reimplementing BBS) and checks it
-- verifies with the matching public key -- and, the other side of the same property, fails
-- with an unrelated one.
testBadgeCredentialSignAndVerify :: HasCallStack => TestParams -> IO ()
testBadgeCredentialSignAndVerify _ps = do
  Right (pk, sk) <- bbsKeyGen
  Right (otherPk, _) <- bbsKeyGen
  masterKey <- BadgeMasterKey <$> getRandomBytes 32
  now <- getCurrentTime
  result <- issueSignedBadge 1 sk (testBadgeRequest masterKey) now
  case result of
    Left e -> expectationFailure $ "expected a signed credential, got: " <> show e
    Right cred -> do
      verifyCredential pk cred `shouldReturn` True
      verifyCredential otherPk cred `shouldReturn` False

-- A periodEnd that is NOT already a Sunday must still land on a Sunday at 23:59:59 UTC.
testBadgeCredentialExpiryIsSundayEndOfDay :: HasCallStack => TestParams -> IO ()
testBadgeCredentialExpiryIsSundayEndOfDay _ps = do
  Right (_, sk) <- bbsKeyGen
  masterKey <- BadgeMasterKey <$> getRandomBytes 32
  let periodEnd = UTCTime (fromGregorian 2026 8 20) 0 -- a Thursday
  Right BadgeCredential {badgeInfo = BadgeInfo {badgeExpiry}} <- issueSignedBadge 1 sk (testBadgeRequest masterKey) periodEnd
  case badgeExpiry of
    Nothing -> expectationFailure "expected badgeExpiry to be set"
    Just (UTCTime day tod) -> do
      let (_, _, dow) = toWeekDate day -- 1 = Monday .. 7 = Sunday
      dow `shouldBe` 7
      tod `shouldBe` sundayEndOfDay

-- The boundary case: a periodEnd already on a Sunday must expire on the FOLLOWING Sunday, not
-- the same day -- a non-strict implementation would silently cost every such badge a week of
-- validity.
testBadgeCredentialExpirySundayRollsToFollowingSunday :: HasCallStack => TestParams -> IO ()
testBadgeCredentialExpirySundayRollsToFollowingSunday _ps = do
  Right (_, sk) <- bbsKeyGen
  masterKey <- BadgeMasterKey <$> getRandomBytes 32
  let periodEnd = UTCTime (fromGregorian 2026 8 23) 0 -- already a Sunday
      expectedExpiry = UTCTime (fromGregorian 2026 8 30) sundayEndOfDay -- the following Sunday
  Right BadgeCredential {badgeInfo = BadgeInfo {badgeExpiry}} <- issueSignedBadge 1 sk (testBadgeRequest masterKey) periodEnd
  badgeExpiry `shouldBe` Just expectedExpiry

-- issueBadge itself rejects a non-empty badgeExtra; issueSignedBadge must surface that as
-- BSEBadRequest rather than letting the raw BBS error leak.
testBadgeCredentialRejectsNonEmptyBadgeExtra :: HasCallStack => TestParams -> IO ()
testBadgeCredentialRejectsNonEmptyBadgeExtra _ps = do
  Right (_, sk) <- bbsKeyGen
  masterKey <- BadgeMasterKey <$> getRandomBytes 32
  now <- getCurrentTime
  let req = BadgeRequest {masterKey, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Nothing, badgeExtra = "reserved"}}
  result <- issueSignedBadge 1 sk req now
  result `shouldBe` Left BSEBadRequest

-- Round-trips a real `simplex-chat badge keygen`-shaped file (the format written by
-- writeTestBadgeServiceSecrets) through loadIssuerKey and checks the loaded secret matches.
testBadgeIssuerKeyLoadsValidFile :: HasCallStack => TestParams -> IO ()
testBadgeIssuerKeyLoadsValidFile TestParams {tmpPath} = do
  Right (BBSPublicKey pk, sk@(BBSSecretKey skBytes)) <- bbsKeyGen
  let path = tmpPath </> "valid-issuer.keys"
  writeFile path $ "secret " <> BC.unpack (strEncode skBytes) <> "\npublic " <> BC.unpack (strEncode pk) <> "\n"
  loaded <- loadIssuerKey path 1
  loaded `shouldBe` sk

expectDies :: forall a. HasCallStack => IO a -> IO ()
expectDies action = do
  r <- try action :: IO (Either ExitCode a)
  case r of
    Left (ExitFailure _) -> pure ()
    Left ExitSuccess -> expectationFailure "expected a failing exit, got ExitSuccess"
    Right _ -> expectationFailure "expected loadIssuerKey to fail fast, but it returned"

testBadgeIssuerKeyMissingFile :: HasCallStack => TestParams -> IO ()
testBadgeIssuerKeyMissingFile TestParams {tmpPath} =
  expectDies $ loadIssuerKey (tmpPath </> "missing-issuer.keys") 1

testBadgeIssuerKeyMalformedFile :: HasCallStack => TestParams -> IO ()
testBadgeIssuerKeyMalformedFile TestParams {tmpPath} = do
  let path = tmpPath </> "malformed-issuer.keys"
  writeFile path "not the expected keygen output\n"
  expectDies $ loadIssuerKey path 1

testBadgeIssuerKeyNonPositiveIdx :: HasCallStack => TestParams -> IO ()
testBadgeIssuerKeyNonPositiveIdx TestParams {tmpPath} = do
  (issuerKeyFile, _) <- writeTestBadgeServiceSecrets tmpPath
  expectDies $ loadIssuerKey issuerKeyFile 0

#if defined(dbPostgres)
runMigrationsToRun :: DBStore -> MigrationsToRun -> IO ()
runMigrationsToRun st = Migrations.run st Nothing
#else
runMigrationsToRun :: DBStore -> MigrationsToRun -> IO ()
runMigrationsToRun st = Migrations.run st Nothing True
#endif
