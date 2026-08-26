{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.BadgeServiceTests where

import BadgeService.Admin (AdminCmd (..), AdminOpts (..), IssueOpts (..), adminCommandParser, runAdminCmd)
import BadgeService.Catalog (catalogTotals, defaultCatalog, offerTotal, seedCatalog)
-- imported qualified: 'codeHash' and 'normalizeCode' would otherwise be ambiguous against
-- 'BadgeService.Store.BadgeCode''s 'codeHash' field, which this module already uses as a record
-- field (the same reason 'BadgeService.Admin' imports this module qualified).
import qualified BadgeService.Codes as Codes
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
import Bots.BadgeManagerTests (allowPass, gatedClockAt, newBadgeGate, waitPasses)
import ChatClient
import ChatTests.DBUtils
import ChatTests.Profiles (testBadgeKeys)
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forM_, replicateM, unless, void)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (find, isInfixOf, isPrefixOf, nub, sort, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (DiffTime, NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime, nominalDay, secondsToDiffTime)
import Data.Word (Word8, Word32)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
-- qualified: 'defaultPrefs' collides with ChatTests.Utils' own (chat preferences, unrelated)
import qualified Options.Applicative as O
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeMasterKey (..), BadgeRequest (..), BadgeType (..), VerifiedBadgeRequest (..), generateMasterKey, issueBadge, verifyCredential)
import Simplex.Chat.Badges.Months (addMonths)
import Simplex.Chat.Badges.Service
  ( -- 'BadgeBalance', 'StatementEntry' and 'StatementEntryType' import only their
    -- constructors, not '(..)': their field names (entryId, changeMonths, balanceMonths,
    -- createdAt, ...) duplicate 'BadgeLedgerEntry''s (Badges.Types), which the existing B1
    -- ledger tests below already use as bare selectors -- importing the field selectors here
    -- too would make those pre-existing, untouched uses ambiguous. 'BadgeStatement' is safe
    -- to import with '(..)': 'entries'/'previousEntryId' are unique names nothing else here
    -- uses.
    BadgeBalance (BadgeBalance),
    BadgeCatalog (..),
    BadgeOffer (..),
    BadgePrice (..),
    BadgeServiceCommand (..),
    BadgeServiceErrorCode (..),
    BadgeServiceRequest (..),
    BadgeServiceResponse (..),
    BadgeStatement (..),
    -- 'BadgeUpgrade' imports only its constructor: its 'receipt' and 'balance' fields collide
    -- with 'BSPBadgeCredential''s and 'BSCIssueBadge''s, both imported with '(..)' above.
    BadgeUpgrade (BadgeUpgrade),
    StatementCreditType (SCCharge, SCOpening, SCPayment),
    StatementDebitType (SDBadge, SDLapse),
    StatementEntry (StatementEntry),
    StatementEntryType (SECredit, SEDebit),
    pattern VersionBadgeService,
  )
import Simplex.Chat.Badges.Types
  ( BadgeItemStatus (..),
    BadgeLedgerEntry (..),
    BadgeOfferId (..),
    BadgePurchasePayment (BPPCode),
    BadgePurchaseStatus (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (..),
    OfferDiscount (..),
  )
import Simplex.Chat.Controller (ChatCommand (APIPurchaseBadge), ChatConfig (..), ChatController (ChatController, chatStore, currentUser, processServiceRequests))
import Simplex.Chat.Library.Commands (execChatCommand')
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.PaymentService (ServicePayment (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Chat.Types (ChatPeerType (..), ConnectTarget (..), Profile (..), User (User, userId))
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (serviceRequestTimeout))
import Simplex.Messaging.Agent.Protocol (ConnectionMode (CMContact))
import Simplex.Messaging.Agent.Store.Common (DBStore, withConnection, withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, createDBStore)
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationConfig (..), MigrationConfirmation (..), MigrationsToRun (..), toDownMigration)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSPublicKey (..), BBSSecretKey (..), bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode, strEncode)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, hFlush, openFile, stdout)
import Test.Hspec hiding (it)
#if defined(dbPostgres)
import BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations)
import Database.PostgreSQL.Simple (Only (..))
import qualified Simplex.Messaging.Agent.Store.Postgres.Migrations as Migrations
#else
import BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations)
import Database.SQLite.Simple (Only (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
-- Reachable only from the two '#if !defined(dbPostgres)' tests below (the schema-linkage scan and
-- the no-plaintext-at-rest scan), so they are imported here rather than unconditionally: under
-- 'client_postgres' the code that uses them is compiled out and an unconditional import would
-- make that build warn (-Wunused-imports).
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
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
  it "should reject an offer with discount > 100 instead of wrapping into an overcharge" testBadgeCatalogOfferTotalRejectsBadDiscount
  it "should reject a total that overflows Word32 instead of wrapping into a wrong charge" testBadgeCatalogOfferTotalRejectsOverflow
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
  it "should publish the same address to address_file across two starts, matching the client's address" testBadgeServicePublishesAddressFile
  it "should omit a disabled price and its offers from getBadgeCatalog, and keep a deprecated one" testBadgeServiceGetCatalogDisabledDeprecated
  it "should respond unknown_purchase_key to a signed getBadgeCatalog from an unknown key" testBadgeServiceGetCatalogUnknownSignerKey
  it "should heal the ledger on a signed getBadgeCatalog, appending exactly one debit(lapse), and heal nothing on a repeat" testBadgeServiceGetCatalogHealsLedger
  it "should rate_limit a third unsigned getBadgeCatalog once the catalog bucket is drained, without affecting a signed one" testBadgeServiceGetCatalogBucketThrottle
  it "should create a purchase and append ledger entries readable back in order" testBadgeStorePurchaseAndLedger
  it "should disable a price out of the active catalog while both stay reachable by id" testBadgeStoreSetPriceStatusDisabled
  it "should return the redeeming purchase key from getCodeByHash" testBadgeStoreGetCodeByHashRedeemer
  it "should refuse to redeem a revoked code, leaving the row untouched" testBadgeStoreMarkCodeRedeemedRefusesRevoked
  it "should clear both redemption columns and set unredeemed_at" testBadgeStoreUnredeemCode
  it "should redeem a valid code into a verifiable credential, with a credit(payment) then debit(badge) ledger" testBadgeServiceRedeemCodeIssuesCredential
  it "should return the same credential and write no new row when the identical request is repeated" testBadgeServiceRedeemCodeIdempotent
  it "should append exactly one debit(lapse) when the identical request is repeated after months lapsed" testBadgeServiceReplayAfterLapseHealsOnce
  it "should respond code_used to the same code presented by a second purchase key" testBadgeServiceCodeUsedBySecondKey
  it "should answer code_invalid for unknown, mistyped and revoked codes and code_expired for an expired one, consuming none" testBadgeServiceCodeFailureOutcomes
  it "should debit the per-signer bucket once per failed redemption and not at all for a success, replay, badgeExtra or tier mismatch" testBadgeServiceFailureDebitsBucketOncePerFailure
  it "should rate_limit a fresh signer once the global failure budget is drained, and let the same code succeed after it refills" testBadgeServiceGlobalFailureBudgetRefills
  it "should issue the second period with debit(lapse) before debit(badge), then serve the cached credential" testBadgeServiceIssueBadgeSecondPeriod
  it "should serve the cached credential when issueBadge repeats inside the last funded month" testBadgeServiceIssueBadgeCachedInLastFundedMonth
  it "should return no credential and a zero-balance statement when the balance is exhausted" testBadgeServiceIssueBadgeExhaustedBalance
  it "should credit a second code to the existing purchase with its own payment and no second issuance" testBadgeServiceSecondCodeSamePurchaseKey
  it "should sign the tier the code funds whatever tier the request names, and still refuse a tier mismatch on issueBadge" testBadgeServiceSignsFundedTier
  it "should respond bad_request to a purchase carrying an upgrade, leaving the code unredeemed" testBadgeServiceUpgradeIsBadRequest
  it "should respond bad_request to a reserved badgeExtra, leaving the code unredeemed" testBadgeServiceBadgeExtraIsBadRequest
  it "should return only the entries after an asserted cursor, and the full history for an unknown or another purchase's one" testBadgeServiceIssueBadgeCursor
  it "should serve the current month's cached credential across a clamped month boundary" testBadgeServiceCachedIssuanceAtClampedMonthBoundary
#if !defined(dbPostgres)
  it "should have no table referencing both web_orders and badge_purchases" testBadgeServiceNoTableLinksOrdersToPurchases
#endif
  it "should issue 10 codes with distinct hashes, no plaintext at rest, and revoke and report them by batch" testBadgeServiceCodesIssueRevokeStatus
  it "should reject codes issue --type investor at parse time" testBadgeServiceCodesRejectsInvestorType
  it "should sign a credential that verifies with the matching public key, and fail with a different one" testBadgeCredentialSignAndVerify
  it "should set badgeExpiry to the next Sunday at 23:59:59 UTC" testBadgeCredentialExpiryIsSundayEndOfDay
  it "should roll a periodEnd already on a Sunday to the following Sunday" testBadgeCredentialExpirySundayRollsToFollowingSunday
  it "should reject a badgeRequest with non-empty badgeExtra as bad_request" testBadgeCredentialRejectsNonEmptyBadgeExtra
  it "should load a valid issuer key file" testBadgeIssuerKeyLoadsValidFile
  it "should fail fast on a missing issuer key file" testBadgeIssuerKeyMissingFile
  it "should fail fast on an issuer key file without a 'secret' line" testBadgeIssuerKeyMalformedFile
  it "should fail fast on a non-positive key_idx" testBadgeIssuerKeyNonPositiveIdx
  -- C5: the client against the live service
  it "should redeem a code from a profile with no prior connection and show the badge, with the client ledger matching the service's" testC5RedeemCodeShowsBadge
  it "should present the redeemed badge to a contact connected before the redemption" testC5ContactSeesBadgeAfterRedeem
  it "should write a legend badge for a legend code redeemed through the tier-less command" testC5RedeemLegendCodeGivesLegendBadge
  it "should write nothing for an unknown, an already redeemed and an expired code" testC5RefusedCodesWriteNothing
  it "should write nothing when the issued credential does not verify against the configured key" testC5UnverifiableCredentialWritesNothing
  it "should answer internal and write nothing when no badge service address is configured" testC5NoServiceAddressConfigured
  it "should answer internal and write nothing for apple and google payments" testC5StorePaymentsUnsupported
  it "should answer internal and write nothing for both commands with the service stopped" testC5ServiceStoppedIsInternal
  it "should supersede the first purchase on a second redemption, leaving its ledger and months alone" testC5SecondCodeSupersedesFirst
  it "should return the four seeded offers and both prices to a profile with no prior connection, writing nothing" testC5CatalogFromFreshProfile
  it "should issue the second period in one worker pass signalled by the badge state command, keeping the master key" testC5WorkerIssuesSecondPeriod
  it "should not repoint the active profile from a redemption that finishes after a profile switch" testC5RedeemAfterProfileSwitchKeepsActiveUser
  it "should answer code_used to the same code again, and redeem it into a new purchase once unredeemed" testC5SameCodeAgainThenUnredeemed
  it "should redeem through a badge service address given as a full contact request URI" testC5FullContactUriRedeems
  it "should write nothing for a redemption response it cannot record in full" testC5UnrecordableResponsesWriteNothing
  it "should report a response of the wrong shape and one it cannot read, on both commands" testC5UnreadableResponsesAreReported

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
      configFile = badgeServiceConfigPath ps,
      -- the real clock, as in production; 'withBadgeServiceClock' overrides this field for the
      -- tests that advance service time (B10)
      serviceClock = getCurrentTime
    }

badgeServiceConfigPath :: FilePath -> FilePath
badgeServiceConfigPath tmpPath = tmpPath </> "badge_service.ini"

-- The `badge keygen`-shaped file writeTestBadgeServiceSecrets generates the issuer key into,
-- named once so 'readTestIssuerPublicKey' reads back the very key the service loaded.
testIssuerKeyFile :: FilePath -> FilePath
testIssuerKeyFile tmpPath = tmpPath </> "badge-issuer.keys"

-- Generates a real issuer key file (the two-line `badge keygen` output: "secret ..\npublic
-- ..") and a real 32-byte code secret (base64-encoded), at fixed names under tmpPath. A6
-- doesn't read either file's contents -- that's B3 and B4 -- but the harness must still hand
-- every later step real files at real paths, per the config keys naming secrets as files.
writeTestBadgeServiceSecrets :: FilePath -> IO (FilePath, FilePath)
writeTestBadgeServiceSecrets tmpPath = do
  let issuerKeyFile = testIssuerKeyFile tmpPath
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
writeTestBadgeServiceConfig ps = writeTestBadgeServiceConfigWith ps []

-- The same file with extra ini lines appended -- so far only a '[throttle]' override (B5
-- decision 5), which is how B10 drives the failure buckets to their limits in a handful of
-- requests instead of hundreds.
writeTestBadgeServiceConfigWith :: TestParams -> [String] -> IO ()
writeTestBadgeServiceConfigWith TestParams {tmpPath} extraLines = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  writeFile (badgeServiceConfigPath tmpPath) $ unlines (issuerCodesIniLines issuerKeyFile codeSecretFile ++ extraLines)

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
withBadgeServiceConfig ps = withBadgeServiceClock ps getCurrentTime

-- The same harness with the service's own clock replaced (A6: 'BadgeServiceEnv.now' is the only
-- clock any service component reads, and 'BadgeServiceOpts.serviceClock' is what
-- 'newBadgeServiceEnv' installs there). Both service starts get the same clock, so time is
-- continuous across the between-phases window. This is what lets B10 cross a month boundary or a
-- throttle bucket's refill window without any test sleeping -- see 'newTestClock'.
withBadgeServiceClock :: HasCallStack => TestParams -> IO UTCTime -> IO () -> IO () -> (TestCC -> String -> IO ()) -> IO ()
withBadgeServiceClock ps clock writeConfig betweenPhases test =
  withBadgeServiceAddress ps clock writeConfig betweenPhases $ \(sLink, _fullLink) withService ->
    -- Second start: badge service takes the ShowMyAddress branch, then serves the test body.
    withService $
      withNewTestChatCfg ps testCfg "client" bobProfile $ \client ->
        test client sLink

-- | The same first phase, with the SECOND phase handed to the body as @withService@ instead of
-- wrapped around it, and with the service's contact address in both published forms.
--
-- Two things C5 needs and 'withBadgeServiceClock' cannot give: a chat client that OUTLIVES a
-- service restart (the code is redeemed, the service is stopped, its @codes@ row is unredeemed
-- and the service is started again, all with one client running), and a client running with the
-- service NOT started at all. A body that just wants the service up for its whole length calls
-- @withService@ once, which is what 'withBadgeServiceClock' does above.
--
-- The full contact request URI is the second element: it is one of the four target forms
-- 'Simplex.Chat.Library.Commands.resolveServiceTarget' accepts and, without it, only the short
-- link would ever be exercised.
withBadgeServiceAddress :: HasCallStack => TestParams -> IO UTCTime -> IO () -> IO () -> ((String, String) -> (IO () -> IO ()) -> IO ()) -> IO ()
withBadgeServiceAddress ps clock writeConfig betweenPhases test = do
  let opts = (mkBadgeServiceOpts ps) {serviceClock = clock}
  writeConfig
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  -- First start: badge service takes the CreateMyAddress branch.
  runBadgeService testCfg opts (pure ())
  -- Reopen the DB to read the links the service created.
  links <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    links <- getContactLinks bs False
    bs <## "auto_accept off"
    pure links
  betweenPhases
  test links (runBadgeService testCfg opts)

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

-- Decodes a service response object into 'BadgeServiceResponse' (B6): used by every B6 test
-- that inspects the catalog or the statement, rather than digging through the raw JSON object
-- the way B5's throttle tests do (those only ever need 'code'/'retryAfter', which never
-- justified the extra decode step).
getServiceResponse :: HasCallStack => TestCC -> IO BadgeServiceResponse
getServiceResponse client = do
  obj <- getServiceResponseObject client
  case JT.parseEither J.parseJSON (J.Object obj) :: Either String BadgeServiceResponse of
    Right resp -> pure resp
    Left err -> expectationFailure ("failed to decode service response: " <> err) >> error "unreachable"

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
-- first-purchase case, not an identity error. B7's code classifier now answers it: "UNKNOWN-CODE"
-- normalizes to 11 characters and fails the check character, so it is code_invalid -- reached
-- only because the identity check let an unknown key through in the first place.
testBadgeServicePurchaseBadgeUnknownKeyIsNotUnknownPurchaseKey :: HasCallStack => TestParams -> IO ()
testBadgeServicePurchaseBadgeUnknownKeyIsNotUnknownPurchaseKey ps =
  withBadgeService ps $ \client bsLink -> do
    (pub, priv) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = testBadgeRequestCommand masterKey (SPCode "UNKNOWN-CODE")}
    sendSignedServiceRequest client bsLink priv req
    respObj <- getServiceResponseObject client
    KM.lookup "code" respObj `shouldNotBe` Just (J.String "unknown_purchase_key")
    KM.lookup "code" respObj `shouldBe` Just (J.String "code_invalid")

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
  newBadgeServiceEnv cfg st getCurrentTime

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
          multiplier = if months == 3 then 2 else 6 :: Word32
      case offerTotal (priceFor pid) (Just offer) of
        Just (CurrencyAmount total) -> total `shouldBe` monthly * multiplier
        Nothing -> expectationFailure "seeded offer must have a chargeable total"
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
-- (3 - 12 :: Word8 == 247) and hand out a wildly wrong charge. offerTotal must instead
-- answer Nothing -- a typed absence, not an 'error' -- so one malformed row read inside a
-- request (B6) can never take down the single-threaded request loop (§9).
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
  offerTotal price (Just badOffer) `shouldBe` Nothing

-- The sibling hazard, on the ODDiscount side: a Word8 subtraction of percent from 100 is
-- unsigned and unguarded, so an offer with percent > 100 (a typo, a future repricing) would
-- wrap silently (100 - 101 :: Word8 == 255) and hand out a 2.55x OVERCHARGE, worse than the
-- freeMonths hazard above since it inflates the price instead of merely reading as
-- unavailable. offerTotal must instead answer Nothing, the same way, for the same reason.
testBadgeCatalogOfferTotalRejectsBadDiscount :: HasCallStack => TestParams -> IO ()
testBadgeCatalogOfferTotalRejectsBadDiscount _ps = do
  now <- getCurrentTime
  let BadgeCatalog {prices} = defaultCatalog now
      price@BadgePrice {priceId} = fromJust $ find (\BadgePrice {badgeType} -> badgeType == BTSupporter) prices
      badOffer =
        BadgeOffer
          { offerId = BadgeOfferId "test-bad-offer-discount-gt-100",
            priceId = Just priceId,
            months = 3,
            discount = ODDiscount 101,
            status = BISActive,
            createdAt = now,
            total = Nothing
          }
  offerTotal price (Just badOffer) `shouldBe` Nothing

-- The third wrap in this money-computing module, after the two subtractions above: both arms of
-- offerTotal multiply a price by a month count, and a Word32 product wraps at 12 months above a
-- monthly price of ~35,791 currency units. A wrapped product is a WRONG CHARGE -- worse than the
-- freeMonths wrap, which only reads as unavailable -- so the intermediate is Word64 and the
-- result is range-checked back into the Word32 a CurrencyAmount holds. Unreachable at any
-- plausible price, and refused rather than trusted, exactly like its two siblings.
testBadgeCatalogOfferTotalRejectsOverflow :: HasCallStack => TestParams -> IO ()
testBadgeCatalogOfferTotalRejectsOverflow _ps = do
  now <- getCurrentTime
  let BadgeCatalog {prices} = defaultCatalog now
      price@BadgePrice {priceId} = fromJust $ find (\BadgePrice {badgeType} -> badgeType == BTSupporter) prices
      absurdPrice = price {monthPrice = CurrencyAmount (maxBound :: Word32)}
      offerWith discount =
        BadgeOffer
          { offerId = BadgeOfferId "test-overflow-offer",
            priceId = Just priceId,
            months = 12,
            discount,
            status = BISActive,
            createdAt = now,
            total = Nothing
          }
  -- 12 * maxBound overflows both ways round
  offerTotal absurdPrice (Just (offerWith (ODDiscount 0))) `shouldBe` Nothing
  offerTotal absurdPrice (Just (offerWith (ODFreeMonths 1))) `shouldBe` Nothing
  -- but the same absurd price for exactly one month still fits, and is still priced
  offerTotal absurdPrice Nothing `shouldBe` Just (CurrencyAmount maxBound)

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

-- B9 service address publication -----------------------------------------------------------

-- Proves both facts the brief's manual Verify line asks for: 'address_file' holds the SAME
-- address after the first start (before 'betweenPhases') as after the second, and that address
-- -- read straight out of the file, not from 'bsLink' -- is one a client's @/c@ accepts (sends a
-- connection request for, rather than rejecting as malformed). The badge service disables
-- auto-accept ('badgePostStartHook' passes 'False' to 'initializeBotAddress''), so "connection
-- request sent!" is as far as this goes and is the right stopping point -- 'BroadcastTests.hs'
-- (an auto-accepting bot) is the pattern this borrows the client side of.
--
-- Read strictly: Prelude's lazy 'readFile' would leave 'addressFile' open (a thunk holding the
-- handle) across 'betweenPhases' into the second start, and the second start's own write --
-- exclusive against any still-open handle, even a reader's -- would then fail with "resource
-- busy (file is locked)".
testBadgeServicePublishesAddressFile :: HasCallStack => TestParams -> IO ()
testBadgeServicePublishesAddressFile ps@TestParams {tmpPath} = do
  let addressFile = tmpPath </> "bot_address.txt"
      writeConfig = do
        (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
        writeFile (badgeServiceConfigPath tmpPath) $
          unlines $
            issuerCodesIniLines issuerKeyFile codeSecretFile
              ++ ["", "[service]", "address_file = " <> addressFile]
  firstContentsRef <- newIORef T.empty
  withBadgeServiceConfig
    ps
    writeConfig
    (TIO.readFile addressFile >>= writeIORef firstContentsRef)
    $ \client bsLink -> do
      firstContents <- readIORef firstContentsRef
      secondContents <- TIO.readFile addressFile
      firstContents `shouldBe` secondContents
      T.strip secondContents `shouldBe` T.pack bsLink
      client ##> ("/c " <> T.unpack (T.strip secondContents))
      client <## "connection request sent!"

-- B6 getBadgeCatalog -----------------------------------------------------------

-- A disabled price (and every offer pinned to it) must be absent from the RPC catalog, while
-- a deprecated price (and its offers) must still be present -- getActiveCatalog's own
-- invariant (already proved at the store level by testBadgeStoreSetPriceStatusDisabled),
-- surfaced here through the live RPC path handleGetBadgeCatalog actually calls. Also asserts
-- decision 8 at the wire boundary: every remaining offer's total is populated, so a client
-- never has to (and can't, since it doesn't have the prices) compute one itself -- deleting
-- the handler's catalogTotals call would still pass every other assertion in this test file
-- without this one.
testBadgeServiceGetCatalogDisabledDeprecated :: HasCallStack => TestParams -> IO ()
testBadgeServiceGetCatalogDisabledDeprecated ps = do
  priceIdsRef <- newIORef Nothing
  let seedStatuses =
        withTestChat ps serviceDbPrefix $ \bs -> do
          bs <## "subscribed 1 connections on server localhost"
          priceIds <- expectRight $ withServiceTransaction (chatStore (chatController bs)) $ \db -> do
            BadgeCatalog {prices} <- getActiveCatalog db
            case prices of
              [BadgePrice {priceId = pid1}, BadgePrice {priceId = pid2}] -> do
                setPriceStatus db pid1 BISDisabled
                setPriceStatus db pid2 BISDeprecated
                pure (pid1, pid2)
              _ -> error "expected exactly the two default seeded prices"
          writeIORef priceIdsRef (Just priceIds)
  withBadgeServiceConfig ps (writeTestBadgeServiceConfig ps) seedStatuses $ \client bsLink -> do
    Just (disabledId, deprecatedId) <- readIORef priceIdsRef
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Nothing, request = BSCGetBadgeCatalog}
    sendServiceRequest client bsLink req
    resp <- getServiceResponse client
    case resp of
      BSPBadgeCatalog {catalog = BadgeCatalog {prices, offers}} -> do
        any (\BadgePrice {priceId} -> priceId == disabledId) prices `shouldBe` False
        any (\BadgePrice {priceId} -> priceId == deprecatedId) prices `shouldBe` True
        any (\BadgeOffer {priceId} -> priceId == Just disabledId) offers `shouldBe` False
        any (\BadgeOffer {priceId} -> priceId == Just deprecatedId) offers `shouldBe` True
        all (\BadgeOffer {total} -> isJust total) offers `shouldBe` True
      other -> expectationFailure $ "expected BSPBadgeCatalog, got: " <> show other

-- getBadgeCatalog applies checkSignerRecord like every other signed command (B5): a signed
-- request from a key with no purchase row must fail the identity check before
-- handleGetBadgeCatalog ever runs, the same way B5 already proved for issueBadge/pauseBadge.
testBadgeServiceGetCatalogUnknownSignerKey :: HasCallStack => TestParams -> IO ()
testBadgeServiceGetCatalogUnknownSignerKey ps =
  withBadgeService ps $ \client bsLink -> do
    (pub, priv) <- mkTestKeyPair
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = BSCGetBadgeCatalog}
    sendSignedServiceRequest client bsLink priv req
    client <## "service response: {\"code\":\"unknown_purchase_key\",\"type\":\"error\"}"

-- StatementEntry is constructed/matched positionally (see the import list's Haddock): field
-- order entryId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType,
-- wasPausedSince, createdAt, entryType. Used to compare two statements for equality without
-- needing an Eq instance on StatementEntry itself (there isn't one).
statementEntryKey :: StatementEntry -> (Text, Int, Int)
statementEntryKey (StatementEntry entryId changeMonths balanceMonths _ _ _ _ _) = (entryId, changeMonths, balanceMonths)

-- A signed getBadgeCatalog heals the purchase's ledger to `now` (B2's `advance`) in the SAME
-- transaction that reads the statement back (RPC "Statement and balance"), so the balance the
-- client is told is the balance the database holds. With balance_start_ts backdated two
-- months on a balance of 3, healing appends exactly one debit(lapse) of -2, leaving a balance
-- of 1; an identical second request must append nothing further, since the ledger is already
-- healed to (approximately) now.
testBadgeServiceGetCatalogHealsLedger :: HasCallStack => TestParams -> IO ()
testBadgeServiceGetCatalogHealsLedger ps = do
  (pub, priv) <- mkTestKeyPair
  masterKey <- BadgeMasterKey <$> getRandomBytes 32
  purchaseIdRef <- newIORef Nothing
  let seedBackdatedLedger =
        withTestChat ps serviceDbPrefix $ \bs -> do
          bs <## "subscribed 1 connections on server localhost"
          now <- getCurrentTime
          let backdated = addMonths (-2) now
          badgePurchaseId <- expectRight $ withServiceTransaction (chatStore (chatController bs)) $ \db -> do
            BadgePurchaseRow {badgePurchaseId} <- createPurchase db pub masterKey BTSupporter now
            _ <-
              appendLedgerEntry
                db
                BadgeLedgerEntry
                  { entryId = 0,
                    entryUuid = "test-opening-entry",
                    badgePurchaseId,
                    changeMonths = 3,
                    balanceMonths = 3,
                    balanceStartTs = backdated,
                    balanceBadgeType = BTSupporter,
                    wasPausedSince = Nothing,
                    serviceCreatedAt = now,
                    createdAt = now,
                    entryType = LECredit CTOpening
                  }
            pure badgePurchaseId
          writeIORef purchaseIdRef (Just badgePurchaseId)
  statement1Ref <- newIORef Nothing
  statement2Ref <- newIORef Nothing
  withBadgeServiceConfig ps (writeTestBadgeServiceConfig ps) seedBackdatedLedger $ \client bsLink -> do
    let req = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = BSCGetBadgeCatalog}
    sendSignedServiceRequest client bsLink priv req
    resp1 <- getServiceResponse client
    case resp1 of
      BSPBadgeCatalog {badgeStatement = Just stmt} -> writeIORef statement1Ref (Just stmt)
      other -> expectationFailure $ "expected a statement on the first request, got: " <> show other
    sendSignedServiceRequest client bsLink priv req
    resp2 <- getServiceResponse client
    case resp2 of
      BSPBadgeCatalog {badgeStatement = Just stmt} -> writeIORef statement2Ref (Just stmt)
      other -> expectationFailure $ "expected a statement on the second request, got: " <> show other
  Just badgePurchaseId <- readIORef purchaseIdRef
  Just BadgeStatement {entries = entries1} <- readIORef statement1Ref
  Just BadgeStatement {entries = entries2} <- readIORef statement2Ref
  case entries1 of
    [_opening, StatementEntry _ changeMonths balanceMonths _ _ _ _ (SEDebit SDLapse)] -> do
      changeMonths `shouldBe` (-2)
      balanceMonths `shouldBe` 1
    other -> expectationFailure $ "expected exactly [opening, lapse(-2)], got " <> show (length other) <> " entries"
  map statementEntryKey entries2 `shouldBe` map statementEntryKey entries1 -- second request heals nothing further
  -- the balance the RPC reported must match a freshly read getLastLedgerEntry -- reopened
  -- only after the service (and its exclusive hold on the sqlite file) has been killed, same
  -- as withBadgeServiceConfig's own between-phases reopen.
  lastEntry <-
    withTestChat ps serviceDbPrefix $ \bs -> do
      bs <## "subscribed 1 connections on server localhost"
      expectRight $ withServiceTransaction (chatStore (chatController bs)) $ \db -> getLastLedgerEntry db badgePurchaseId
  case (lastEntry, entries1) of
    (Just BadgeLedgerEntry {balanceMonths = storedBalance}, [_, StatementEntry _ _ wireBalance _ _ _ _ _]) ->
      storedBalance `shouldBe` wireBalance
    _ -> expectationFailure "expected a stored last ledger entry matching the wire statement's balance"

-- The catalog bucket (B5 decision 5) is spent by every UNSIGNED getBadgeCatalog, success or
-- not; a signed request never touches it (bounded instead by requiring a purchase row).
-- Overriding the bucket to capacity 2 via A6's [throttle] harness: the first two unsigned
-- requests in the window succeed, a third gives rate_limited with a positive retryAfter, and
-- a signed request (from a key with a real purchase row) is unaffected by the drained bucket.
testBadgeServiceGetCatalogBucketThrottle :: HasCallStack => TestParams -> IO ()
testBadgeServiceGetCatalogBucketThrottle ps = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets (tmpPath ps)
  (pub, priv) <- mkTestKeyPair
  masterKey <- BadgeMasterKey <$> getRandomBytes 32
  let writeConfig =
        writeFile (badgeServiceConfigPath (tmpPath ps)) $
          unlines $
            issuerCodesIniLines issuerKeyFile codeSecretFile
              ++ ["", "[throttle]", "catalog_capacity = 2", "catalog_start_tokens = 2"]
      seedPurchase =
        withTestChat ps serviceDbPrefix $ \bs -> do
          bs <## "subscribed 1 connections on server localhost"
          now <- getCurrentTime
          void $ expectRight $ withServiceTransaction (chatStore (chatController bs)) $ \db -> createPurchase db pub masterKey BTSupporter now
  withBadgeServiceConfig ps writeConfig seedPurchase $ \client bsLink -> do
    let unsignedReq = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Nothing, request = BSCGetBadgeCatalog}
        signedReq = BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = BSCGetBadgeCatalog}
        expectCatalog label client' = do
          resp <- getServiceResponse client'
          case resp of
            BSPBadgeCatalog {} -> pure ()
            other -> expectationFailure $ label <> " expected BSPBadgeCatalog, got: " <> show other
    -- drain the 2-token catalog bucket
    sendServiceRequest client bsLink unsignedReq
    expectCatalog "first unsigned request" client
    sendServiceRequest client bsLink unsignedReq
    expectCatalog "second unsigned request" client
    -- a third request in the same window is rejected before processing
    sendServiceRequest client bsLink unsignedReq
    respObj <- getServiceResponseObject client
    KM.lookup "code" respObj `shouldBe` Just (J.String "rate_limited")
    case KM.lookup "retryAfter" respObj of
      Just (J.Number n) -> n `shouldSatisfy` (> 0)
      other -> expectationFailure $ "expected a positive retryAfter, got: " <> show other
    -- a signed request bypasses the catalog bucket entirely, even fully drained
    sendSignedServiceRequest client bsLink priv signedReq
    expectCatalog "signed request" client

-- B7 purchaseBadge{code} and issueBadge, B8 codes tooling (B10) ---------------
--
-- B7's and B8's Verify lines are both "covered by B10", so everything below is the only check
-- those two steps have. Three rules shape how they assert:
--   * time is the injected clock (A6), never a sleep: 'withBadgeServiceClock' hands the service
--     an 'IORef UTCTime' the test moves forward;
--   * expected values are literal (fixed dates, fixed row shapes), never recomputed the way
--     production computes them;
--   * "wrote nothing" is a row count, never "the response looked the same".

-- | The instant every clock-driven test starts service time at. Fixed rather than derived from
-- 'getCurrentTime' so periods and expiries below are literal dates: 2026-03-10 is a Tuesday, so
-- a first period ends Friday 2026-04-10 and its credential expires Sunday 2026-04-12 (B4).
testClockStart :: UTCTime
testClockStart = UTCTime (fromGregorian 2026 3 10) noonUTC

noonUTC :: DiffTime
noonUTC = secondsToDiffTime (12 * 3600)

-- The end of a period issued at 'testClockStart': one month on, no clamping involved.
firstPeriodEnd :: UTCTime
firstPeriodEnd = UTCTime (fromGregorian 2026 4 10) noonUTC

-- And that period's credential expiry: the Sunday after 'firstPeriodEnd', spelled out rather
-- than computed with 'sundayAfter', which is the function under test.
firstCredentialExpiry :: UTCTime
firstCredentialExpiry = UTCTime (fromGregorian 2026 4 12) sundayEndOfDay

-- Far enough ahead of 'testClockStart' that no seeded code expires during a test.
testCodeExpiry :: UTCTime
testCodeExpiry = UTCTime (fromGregorian 2027 1 1) 0

-- Behind 'testClockStart': the code_expired case.
expiredCodeExpiry :: UTCTime
expiredCodeExpiry = UTCTime (fromGregorian 2026 1 1) 0

testCodeBatch :: Text
testCodeBatch = "b10-test-batch"

-- | The injectable service clock (A6) as a settable fixture: @readIORef@ is what
-- 'withBadgeServiceClock' installs as 'BadgeServiceEnv.now', the only clock any service
-- component reads. Moving it is how every time-dependent behaviour below is driven; no test
-- sleeps.
newTestClock :: UTCTime -> IO (IORef UTCTime)
newTestClock = newIORef

setTestClock :: IORef UTCTime -> UTCTime -> IO ()
setTestClock = writeIORef

advanceTestClockMonths :: IORef UTCTime -> Int -> IO ()
advanceTestClockMonths clock n = modifyIORef' clock (addMonths n)

advanceTestClockSeconds :: IORef UTCTime -> NominalDiffTime -> IO ()
advanceTestClockSeconds clock s = modifyIORef' clock (addUTCTime s)

-- | One client identity: the keypair the request is signed with (which is also the purchase
-- key) and the badge master key its credential is signed over. C4 mints a fresh one per
-- redemption, so most tests below use one signer per purchase.
data TestSigner = TestSigner
  { signerPub :: C.PublicKeyEd25519,
    signerPriv :: C.PrivateKeyEd25519,
    signerMaster :: BadgeMasterKey
  }

newTestSigner :: IO TestSigner
newTestSigner = do
  (signerPub, signerPriv) <- mkTestKeyPair
  signerMaster <- BadgeMasterKey <$> getRandomBytes 32
  pure TestSigner {signerPub, signerPriv, signerMaster}

badgeRequestFor :: TestSigner -> BadgeType -> Text -> BadgeRequest
badgeRequestFor TestSigner {signerMaster} badgeType badgeExtra =
  BadgeRequest {masterKey = signerMaster, badgeInfo = BadgeInfo {badgeType, badgeExpiry = Nothing, badgeExtra}}

-- | A @purchaseBadge{code}@ request with the badgeRequest and upgrade spelled out, so the tests
-- that vary either (a tier the code does not fund, a reserved badgeExtra, an upgrade) go through
-- the same builder as a normal redemption.
purchaseRequest :: TestSigner -> BadgeRequest -> Maybe BadgeUpgrade -> Text -> BadgeServiceRequest
purchaseRequest TestSigner {signerPub} badgeRequest upgrade code =
  BadgeServiceRequest
    { version = VersionBadgeService 1,
      purchaseKey = Just signerPub,
      request = BSCPurchaseBadge {badgeRequest, payment = SPCode code, upgrade}
    }

-- The normal redemption: the tier the code funds, no reserved extra, no upgrade.
purchaseCodeRequest :: TestSigner -> BadgeType -> Text -> BadgeServiceRequest
purchaseCodeRequest signer badgeType = purchaseRequest signer (badgeRequestFor signer badgeType "") Nothing

-- | An @issueBadge@ asserting @entryUuid@ as the last entry the client holds. Only that field of
-- the asserted entry is read by the service ('assertedEntryId'); the rest is filler, constructed
-- positionally (see the import list's Haddock).
issueRequest :: TestSigner -> BadgeType -> Text -> Text -> BadgeServiceRequest
issueRequest signer@TestSigner {signerPub} badgeType badgeExtra entryUuid =
  BadgeServiceRequest
    { version = VersionBadgeService 1,
      purchaseKey = Just signerPub,
      request =
        BSCIssueBadge
          { badgeRequest = badgeRequestFor signer badgeType badgeExtra,
            balance = BadgeBalance (StatementEntry entryUuid 1 1 testClockStart badgeType Nothing testClockStart (SECredit SCOpening))
          }
    }

-- An entry uuid the service holds for nobody.
unknownEntryUuid :: Text
unknownEntryUuid = "b10-no-such-entry"

sendRequest :: TestCC -> String -> TestSigner -> BadgeServiceRequest -> IO ()
sendRequest client bsLink TestSigner {signerPriv} = sendSignedServiceRequest client bsLink signerPriv

-- | Decodes one 'BSPBadgeCredential' and asserts, on every single one of them, that @receipt@ is
-- absent: nothing in this milestone issues a receipt, and B7 hardcodes 'Nothing' -- checking it
-- here is what makes that hold for every credential response in the file rather than one.
expectCredentialResponse :: HasCallStack => String -> TestCC -> IO (Maybe BadgeCredential, BadgeStatement)
expectCredentialResponse label client =
  getServiceResponse client >>= \case
    BSPBadgeCredential {credential, receipt, statement} -> do
      (label, receipt) `shouldBe` (label, Nothing)
      pure (credential, statement)
    other -> expectationFailure (label <> ": expected BSPBadgeCredential, got: " <> show other) >> error "unreachable"

expectCredential :: HasCallStack => String -> TestCC -> IO (BadgeCredential, BadgeStatement)
expectCredential label client =
  expectCredentialResponse label client >>= \case
    (Just cred, statement) -> pure (cred, statement)
    (Nothing, _) -> expectationFailure (label <> ": expected a credential, got none") >> error "unreachable"

expectErrorCode :: HasCallStack => String -> TestCC -> Text -> IO ()
expectErrorCode label client code = do
  obj <- getServiceResponseObject client
  (label, KM.lookup "code" obj) `shouldBe` (label, Just (J.String code))

-- rate_limited always carries a retryAfter the client can act on (B5).
expectRateLimited :: HasCallStack => String -> TestCC -> IO ()
expectRateLimited label client = do
  obj <- getServiceResponseObject client
  (label, KM.lookup "code" obj) `shouldBe` (label, Just (J.String "rate_limited"))
  case KM.lookup "retryAfter" obj of
    Just (J.Number n) -> (label, n > 0) `shouldBe` (label, True)
    other -> expectationFailure $ label <> ": expected a positive retryAfter, got: " <> show other

-- | The wire statement as @(changeMonths, balanceMonths, entry type)@, for comparison against a
-- literal list. 'StatementEntry' is matched positionally (see the import list's Haddock).
statementShape :: BadgeStatement -> [(Int, Int, String)]
statementShape BadgeStatement {entries} = map entryShape entries
  where
    entryShape (StatementEntry _ changeMonths balanceMonths _ _ _ _ entryType) = (changeMonths, balanceMonths, entryTypeLabel entryType)

-- Spelled out rather than derived, so 'credit payment' with an invoiceId can never read the same
-- as one without: a code payment has no invoice (B7 §3), and that is what the client is told.
entryTypeLabel :: StatementEntryType -> String
entryTypeLabel = \case
  SECredit (SCPayment Nothing) -> "credit payment (no invoiceId)"
  SECredit (SCPayment (Just _)) -> "credit payment (with invoiceId)"
  SECredit SCOpening -> "credit opening"
  SECredit _ -> "credit (other)"
  SEDebit SDBadge -> "debit badge"
  SEDebit SDLapse -> "debit lapse"
  SEDebit _ -> "debit (other)"

statementEntryIds :: BadgeStatement -> [Text]
statementEntryIds BadgeStatement {entries} = map (\(StatementEntry entryId _ _ _ _ _ _ _) -> entryId) entries

-- | Runs a store transaction against the service's database while the service is NOT running --
-- the harness's between-phases window, or after its body has returned. Opening a second
-- connection during a running phase deadlocks against the service's writer lock (see
-- 'withBadgeServiceConfig').
withServiceStore :: HasCallStack => TestParams -> (DB.Connection -> ExceptT ServiceError IO a) -> IO a
withServiceStore ps action =
  withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    expectRight $ withServiceTransaction (chatStore (chatController bs)) action

withServiceDB :: HasCallStack => TestParams -> (DB.Connection -> IO a) -> IO a
withServiceDB ps action = withServiceStore ps (liftIO . action)

serviceRowCount :: DB.Connection -> String -> IO Int
serviceRowCount db table = do
  [Only n] <- DB.query_ db (fromString ("SELECT count(*) FROM sx_badge_service_" <> table))
  pure n

-- | The five row kinds one redemption writes, counted together: purchases, payments, ledger
-- entries, issuances, redeemed codes. Every "writes nothing" assertion below pins this tuple,
-- because an unchanged response is not evidence that nothing was written.
serviceRowCounts :: DB.Connection -> IO (Int, Int, Int, Int, Int)
serviceRowCounts db = do
  purchases <- serviceRowCount db "badge_purchases"
  payments <- serviceRowCount db "payments"
  entries <- serviceRowCount db "badge_ledger"
  issuances <- serviceRowCount db "badge_issuances"
  [Only redeemed] <- DB.query_ db "SELECT count(*) FROM sx_badge_service_codes WHERE redeemed_purchase_id IS NOT NULL"
  pure (purchases, payments, entries, issuances, redeemed)

-- | The ledger as the database holds it, in append order: @(changeMonths, balanceMonths,
-- entry_type, entry_credit_type, entry_debit_type, payment_id)@. Read as raw columns rather than
-- through 'getLedgerSince', so the assertion pins what was written rather than what the decoder
-- makes of it.
serviceLedgerRows :: DB.Connection -> IO [(Int, Int, Text, Maybe Text, Maybe Text, Maybe Text)]
serviceLedgerRows db =
  DB.query_
    db
    "SELECT change_months, balance_months, entry_type, entry_credit_type, entry_debit_type, payment_id FROM sx_badge_service_badge_ledger ORDER BY entry_id"

-- | Stored timestamps are compared within a second: Postgres TIMESTAMPTZ truncates to
-- microseconds, so an exact 'UTCTime' round trip is not guaranteed on that backend (the same
-- reason 'testBadgeStoreUnredeemCode' gives). Every value compared this way differs from a wrong
-- one by days, never by microseconds.
shouldBeStoredAt :: HasCallStack => UTCTime -> UTCTime -> Expectation
shouldBeStoredAt stored expected
  | abs (diffUTCTime stored expected) < 1 = pure ()
  | otherwise = expectationFailure $ "stored timestamp " <> show stored <> " should have been " <> show expected

-- | Mints plaintext codes with B3's generator and stores only their hashes, exactly as B8's
-- @codes issue@ does. Returns the plaintexts, which from here on exist only in the test.
seedTestCodes :: HasCallStack => TestParams -> [(BadgeType, Word8, UTCTime)] -> IO [Text]
seedTestCodes ps specs = do
  drg <- C.newRandom
  plainCodes <- replicateM (length specs) (Codes.generateBatchCode drg)
  withServiceStore ps $ \db -> insertCodes db (zipWith newCode plainCodes specs) testClockStart
  pure plainCodes
  where
    newCode plain (badgeType, months, expiresAt) =
      NewBadgeCode {codeHash = Codes.codeHash (Codes.normalizeCode plain), badgeType, months, batch = testCodeBatch, expiresAt}

-- One supporter code funding three months, the shape almost every test below starts from.
seedOneSupporterCode :: HasCallStack => TestParams -> IO Text
seedOneSupporterCode ps =
  seedTestCodes ps [(BTSupporter, 3, testCodeExpiry)] >>= \case
    [code] -> pure code
    other -> expectationFailure ("expected exactly one seeded code, got " <> show (length other)) >> error "unreachable"

-- | A well-formed code (valid check character) that was never inserted: the @code_invalid@ that
-- does reach the database and finds nothing there.
mintUnknownCode :: IO Text
mintUnknownCode = C.newRandom >>= Codes.generateBatchCode

-- | The same code with its check character replaced by a different one. B3 rejects it in
-- 'classifyRedemption' before 'getCodeByHash' is ever forced, so this is the @code_invalid@ that
-- never reaches the database at all.
corruptCheckCharacter :: Text -> Text
corruptCheckCharacter code = T.init code <> (if T.last code == '0' then "1" else "0")

codeIsRedeemed :: HasCallStack => TestParams -> Text -> IO Bool
codeIsRedeemed ps code =
  withServiceStore ps (\db -> getCodeByHash db (Codes.codeHash (Codes.normalizeCode code))) >>= \case
    Just (BadgeCode {redeemedPurchaseId}, _) -> pure (isJust redeemedPurchaseId)
    Nothing -> expectationFailure "expected the seeded code to exist" >> error "unreachable"

-- | The public half of the issuer key 'writeTestBadgeServiceSecrets' generated, read back from
-- the same two-line key file the service loaded its secret from -- so 'verifyCredential' below
-- verifies against the key that actually signed, not one the test made up.
readTestIssuerPublicKey :: HasCallStack => FilePath -> IO BBSPublicKey
readTestIssuerPublicKey tmpPath = do
  contents <- BC.readFile (testIssuerKeyFile tmpPath)
  case mapMaybe (BC.stripPrefix "public ") (BC.lines contents) of
    (b64 : _) -> either (\e -> expectationFailure ("issuer key file: " <> e) >> error "unreachable") pure (strDecode b64)
    [] -> expectationFailure "issuer key file has no 'public' line" >> error "unreachable"

-- B10 item 1 / brief 1-2: the happy path, end to end. Every assertion here is one nothing else
-- in the plan makes: the credential verifies under the issuer key the service actually loaded,
-- the ledger is exactly credit(payment) then debit(badge), the wire statement says the same, the
-- credit entry references the payments row that was written for it, and the issuance's stored
-- expiry is the credential's own (the disagreement 'credentialExpiry' refuses to allow).
testBadgeServiceRedeemCodeIssuesCredential :: HasCallStack => TestParams -> IO ()
testBadgeServiceRedeemCodeIssuesCredential ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    (cred, statement) <- expectCredential "redeem" client
    statementShape statement `shouldBe` [(3, 3, "credit payment (no invoiceId)"), (-1, 2, "debit badge")]
    previousEntryId statement `shouldBe` Nothing
    let BadgeCredential {masterKey = credMasterKey, badgeInfo = BadgeInfo {badgeType, badgeExpiry, badgeExtra}} = cred
    badgeType `shouldBe` BTSupporter
    badgeExtra `shouldBe` ""
    badgeExpiry `shouldBe` Just firstCredentialExpiry
    credMasterKey `shouldBe` signerMaster signer
    issuerPub <- readTestIssuerPublicKey (tmpPath ps)
    verifyCredential issuerPub cred `shouldReturn` True
  withServiceDB ps $ \db -> do
    serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)
    [Only paymentRowId] <- DB.query_ db "SELECT payment_id FROM sx_badge_service_payments"
    [Only purchasePaymentId] <- DB.query_ db "SELECT payment_id FROM sx_badge_service_badge_purchases"
    ledgerRows <- serviceLedgerRows db
    ledgerRows
      `shouldBe` [ (3, 3, "credit", Just "payment", Nothing, Just paymentRowId),
                   (-1, 2, "debit", Nothing, Just "badge", Nothing)
                 ]
    purchasePaymentId `shouldBe` Just (paymentRowId :: Text)
    [(issuanceExpiry, periodStart, periodEnd)] <-
      DB.query_ db "SELECT expiry, period_start, period_end FROM sx_badge_service_badge_issuances"
    issuanceExpiry `shouldBeStoredAt` firstCredentialExpiry
    periodStart `shouldBeStoredAt` testClockStart
    periodEnd `shouldBeStoredAt` firstPeriodEnd
    -- nothing in this milestone issues a receipt, so no row records the hash of one
    [Only receiptHashes] <- DB.query_ db "SELECT count(*) FROM sx_badge_service_payments WHERE receipt_hash IS NOT NULL"
    (receiptHashes :: Int) `shouldBe` 0

-- B10 item 2 (first half) / brief 3: an identical repeat returns the SAME credential and writes
-- no row of any kind. The row counts are the assertion -- an idempotent handler and a handler
-- that redeemed the code twice can return identical-looking responses.
testBadgeServiceRedeemCodeIdempotent :: HasCallStack => TestParams -> IO ()
testBadgeServiceRedeemCodeIdempotent ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    let req = purchaseCodeRequest signer BTSupporter code
    sendRequest client bsLink signer req
    (cred1, statement1) <- expectCredential "first redemption" client
    sendRequest client bsLink signer req
    (cred2, statement2) <- expectCredential "identical repeat" client
    cred2 `shouldBe` cred1
    statementShape statement2 `shouldBe` statementShape statement1
    statementEntryIds statement2 `shouldBe` statementEntryIds statement1
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- B10 item 2 (second half): a repeat AFTER months have lapsed still returns the same credential,
-- and appends exactly one debit(lapse) -- no second credit, payment, debit(badge), issuance or
-- redemption. The heal is intended (the RPC has the service heal before answering any
-- statement); a future reader must not "fix" it away.
testBadgeServiceReplayAfterLapseHealsOnce :: HasCallStack => TestParams -> IO ()
testBadgeServiceReplayAfterLapseHealsOnce ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    let req = purchaseCodeRequest signer BTSupporter code
    sendRequest client bsLink signer req
    (cred1, _) <- expectCredential "first redemption" client
    -- two months on: the balance of 2 covered one of them, so exactly one month lapses
    advanceTestClockMonths clock 2
    sendRequest client bsLink signer req
    (cred2, statement) <- expectCredential "replay after a lapse" client
    cred2 `shouldBe` cred1
    statementShape statement
      `shouldBe` [(3, 3, "credit payment (no invoiceId)"), (-1, 2, "debit badge"), (-1, 1, "debit lapse")]
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 3, 1, 1)

-- Brief 4: the same code from a second purchase key is code_used, and that second key gets no
-- purchase row out of the attempt.
testBadgeServiceCodeUsedBySecondKey :: HasCallStack => TestParams -> IO ()
testBadgeServiceCodeUsedBySecondKey ps = do
  clock <- newTestClock testClockStart
  firstSigner <- newTestSigner
  secondSigner <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    sendRequest client bsLink firstSigner (purchaseCodeRequest firstSigner BTSupporter code)
    _ <- expectCredential "first key redeems" client
    sendRequest client bsLink secondSigner (purchaseCodeRequest secondSigner BTSupporter code)
    expectErrorCode "second key" client "code_used"
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- Brief 5 / B10 item 4 (the outcomes half) and item 5: every classified failure, each answered
-- with its own code, and none of them consuming the code it was presented. A revoked code must
-- read exactly like one that never existed.
testBadgeServiceCodeFailureOutcomes :: HasCallStack => TestParams -> IO ()
testBadgeServiceCodeFailureOutcomes ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codesRef <- newIORef []
  let seedCodes =
        seedTestCodes ps [(BTSupporter, 3, expiredCodeExpiry), (BTSupporter, 3, testCodeExpiry)] >>= \case
          codes@[_expired, revoked] -> do
            withServiceStore ps $ \db -> revokeCode db (Codes.codeHash (Codes.normalizeCode revoked)) testClockStart
            writeIORef codesRef codes
          other -> expectationFailure $ "expected two seeded codes, got " <> show (length other)
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCodes $ \client bsLink -> do
    [expiredCode, revokedCode] <- readIORef codesRef
    unknownCode <- mintUnknownCode
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter unknownCode)
    expectErrorCode "unknown code" client "code_invalid"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter (corruptCheckCharacter unknownCode))
    expectErrorCode "bad check character" client "code_invalid"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter revokedCode)
    expectErrorCode "revoked code" client "code_invalid"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter expiredCode)
    expectErrorCode "expired code" client "code_expired"
  [expiredCode, revokedCode] <- readIORef codesRef
  codeIsRedeemed ps expiredCode `shouldReturn` False
  codeIsRedeemed ps revokedCode `shouldReturn` False
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (0, 0, 0, 0, 0)

-- B10 item 4 (the accounting half) and the brief's per-signer throttle bullet, in one sequence
-- against a bucket of 3 tokens. A success, a same-key replay, a reserved badgeExtra and a tier
-- mismatch must debit NOTHING -- if any of them did, the third genuine failure below would come
-- back rate_limited instead of code_invalid. The fourth genuine failure must be rate_limited,
-- which pins each of the three preceding ones at exactly one token. A fresh signer is unaffected
-- by another signer's drained bucket and gets its own outcome.
--
-- BOTH buckets are overridden, because "debits neither" is a claim about both and every debit
-- spends one token from each. The global budget is set to exactly one more than the number of
-- genuine failures here (3 + the fresh signer's one at the end), so a single spurious GLOBAL
-- debit anywhere in the four non-failure requests leaves nothing for that last request and turns
-- it into rate_limited. With the global left at its default 600, a service that debited the
-- shared budget on every success would have passed this example unchanged.
testBadgeServiceFailureDebitsBucketOncePerFailure :: HasCallStack => TestParams -> IO ()
testBadgeServiceFailureDebitsBucketOncePerFailure ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  otherSigner <- newTestSigner
  codesRef <- newIORef []
  let writeConfig =
        writeTestBadgeServiceConfigWith
          ps
          [ "",
            "[throttle]",
            "signer_failure_capacity = 3",
            "signer_failure_start_tokens = 3",
            "global_failure_capacity = 4",
            "global_failure_start_tokens = 4"
          ]
      seedCodes = seedTestCodes ps [(BTSupporter, 3, testCodeExpiry), (BTLegend, 3, testCodeExpiry)] >>= writeIORef codesRef
  withBadgeServiceClock ps (readIORef clock) writeConfig seedCodes $ \client bsLink -> do
    [supporterCode, legendCode] <- readIORef codesRef
    unknown1 <- mintUnknownCode
    unknown2 <- mintUnknownCode
    unknown3 <- mintUnknownCode
    unknown4 <- mintUnknownCode
    -- none of these four is a failed redemption, so none may spend a token
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter supporterCode)
    _ <- expectCredential "success does not debit" client
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter supporterCode)
    _ <- expectCredential "replay does not debit" client
    sendRequest client bsLink signer (purchaseRequest signer (badgeRequestFor signer BTSupporter "reserved") Nothing supporterCode)
    expectErrorCode "reserved badgeExtra does not debit" client "bad_request"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter legendCode)
    expectErrorCode "tier mismatch does not debit" client "bad_request"
    -- three genuine failures, one token each
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter unknown1)
    expectErrorCode "first failure" client "code_invalid"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter (corruptCheckCharacter unknown2))
    expectErrorCode "second failure (never reaches the database)" client "code_invalid"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter unknown3)
    expectErrorCode "third failure" client "code_invalid"
    -- the bucket is now empty: the fourth is refused before it is even classified
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter unknown4)
    expectRateLimited "fourth failure" client
    -- another signer's bucket is its own -- and this is also the global budget's last token, so
    -- it only answers code_invalid if nothing above spent one it should not have
    sendRequest client bsLink otherSigner (purchaseCodeRequest otherSigner BTSupporter unknown4)
    expectErrorCode "fresh signer" client "code_invalid"

-- The brief's global-budget bullet and B10 item 13: the service-wide failure budget is drained by
-- three DIFFERENT signers, so the fourth -- a fresh signer presenting a perfectly VALID code --
-- is rate_limited before the code is classified. That leaves the code unredeemed, which is what
-- lets the same request succeed once the bucket has refilled. Time moves through A6's clock; no
-- test sleeps.
--
-- The clock is advanced in two PARTIAL steps rather than one full hour, so that "a rate_limited
-- rejection must not debit again" (item 13) is actually pinned rather than merely stated. Both
-- buckets refill at capacity tokens per hour, and 'debitBucket' clamps at zero, so a spurious
-- debit is invisible unless the bucket is left short of the threshold afterwards:
--
--   * 900s at capacity 3 puts the global budget at 0.75 tokens -- still below 1, so the valid
--     code is refused, with 0.25 of margin either side of the threshold;
--   * a further 600s adds 0.5. Without a debit that is 1.25 and the retry succeeds; with one it
--     is 0.5 and the retry would still be refused.
--
-- The per-signer capacity is 1 for the same reason, from the other side: it refills 1 token per
-- hour, so a signer bucket debited by the rejection would hold 1500/3600 = 0.42 tokens at the
-- retry and refuse it. Either bucket being debited by a rejection fails this example.
testBadgeServiceGlobalFailureBudgetRefills :: HasCallStack => TestParams -> IO ()
testBadgeServiceGlobalFailureBudgetRefills ps = do
  clock <- newTestClock testClockStart
  signers <- replicateM 3 newTestSigner
  redeemer <- newTestSigner
  codeRef <- newIORef ""
  let writeConfig =
        writeTestBadgeServiceConfigWith
          ps
          [ "",
            "[throttle]",
            "signer_failure_capacity = 1",
            "signer_failure_start_tokens = 1",
            "global_failure_capacity = 3",
            "global_failure_start_tokens = 3"
          ]
      seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) writeConfig seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    forM_ (zip [1 :: Int ..] signers) $ \(n, failingSigner) -> do
      unknownCode <- mintUnknownCode
      sendRequest client bsLink failingSigner (purchaseCodeRequest failingSigner BTSupporter unknownCode)
      expectErrorCode ("global budget failure " <> show n) client "code_invalid"
    -- 0.75 of a token: refused, and far enough below the threshold that no rounding decides it
    advanceTestClockSeconds clock 900
    sendRequest client bsLink redeemer (purchaseCodeRequest redeemer BTSupporter code)
    expectRateLimited "valid code with the global budget drained" client
    -- +0.5: enough only if the rejection above spent nothing
    advanceTestClockSeconds clock 600
    sendRequest client bsLink redeemer (purchaseCodeRequest redeemer BTSupporter code)
    (_, statement) <- expectCredential "same code after the budget refilled" client
    statementShape statement `shouldBe` [(3, 3, "credit payment (no invoiceId)"), (-1, 2, "debit badge")]
  -- exactly one purchase: the three throttled failures wrote nothing, and the rate_limited
  -- request neither consumed the code nor created a purchase
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- Brief 6 / B10 item 9: two months on, issueBadge writes debit(lapse) and THEN debit(badge) --
-- in that order -- plus a second issuance; a third call inside the same month returns the cached
-- credential and writes nothing.
--
-- The code funds FOUR months, so a month is still funded after the second period is issued. Three
-- would work too now: exhausting the balance in an already-issued month is answered with that
-- month's cached credential since the B7 defect this step found was fixed, and
-- 'testBadgeServiceIssueBadgeCachedInLastFundedMonth' holds exactly that case. Four is kept
-- deliberately, so this example exercises the multi-period path with a balance still funded and
-- stays independent of the cached-in-the-last-month behaviour it used to collide with.
testBadgeServiceIssueBadgeSecondPeriod :: HasCallStack => TestParams -> IO ()
testBadgeServiceIssueBadgeSecondPeriod ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedTestCodes ps [(BTSupporter, 4, testCodeExpiry)] >>= writeIORef codeRef . head
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    (cred1, _) <- expectCredential "redeem" client
    advanceTestClockMonths clock 2
    let req = issueRequest signer BTSupporter "" unknownEntryUuid
    sendRequest client bsLink signer req
    (cred2, statement) <- expectCredential "second period" client
    cred2 `shouldNotBe` cred1
    statementShape statement
      `shouldBe` [ (4, 4, "credit payment (no invoiceId)"),
                   (-1, 3, "debit badge"),
                   (-1, 2, "debit lapse"),
                   (-1, 1, "debit badge")
                 ]
    -- the period issued is [2026-05-10, 2026-06-10); 2026-06-10 is a Wednesday
    badgeCredentialExpiry cred2 `shouldBe` Just (UTCTime (fromGregorian 2026 6 14) sundayEndOfDay)
    sendRequest client bsLink signer req
    (cred3, statement3) <- expectCredential "third call inside the same month" client
    cred3 `shouldBe` cred2
    statementShape statement3 `shouldBe` statementShape statement
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 4, 2, 1)

badgeCredentialExpiry :: BadgeCredential -> Maybe UTCTime
badgeCredentialExpiry BadgeCredential {badgeInfo = BadgeInfo {badgeExpiry}} = badgeExpiry

-- The regression guard for the B7 defect B10 found and fixed (plan §9): inside the month that the
-- LAST funded month paid for, the balance is zero AND the month is already issued, and 'issue'
-- refuses for both reasons at once. Classifying that by the balance answered @credential = null@
-- and lost the client a credential the service had already signed, stored and delivered --
-- exactly the retry-after-a-timeout case RPC §Idempotency exists for, which is C3's worker's
-- normal failure mode. The right answer is the cached credential, both at the instant of issue
-- and later in the same month; the month AFTER it is the genuinely exhausted case, which
-- 'testBadgeServiceIssueBadgeExhaustedBalance' holds.
testBadgeServiceIssueBadgeCachedInLastFundedMonth :: HasCallStack => TestParams -> IO ()
testBadgeServiceIssueBadgeCachedInLastFundedMonth ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedTestCodes ps [(BTSupporter, 1, testCodeExpiry)] >>= writeIORef codeRef . head
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    (cred1, statement1) <- expectCredential "redeem the only funded month" client
    statementShape statement1 `shouldBe` [(1, 1, "credit payment (no invoiceId)"), (-1, 0, "debit badge")]
    let req = issueRequest signer BTSupporter "" unknownEntryUuid
    sendRequest client bsLink signer req
    (cached, statement2) <- expectCredential "issueBadge in the month just issued" client
    cached `shouldBe` cred1
    statementShape statement2 `shouldBe` statementShape statement1
    -- and ten days later, still inside the same period (which ends 2026-04-10)
    advanceTestClockSeconds clock (10 * nominalDay)
    sendRequest client bsLink signer req
    (cachedLater, statement3) <- expectCredential "issueBadge ten days into the issued month" client
    cachedLater `shouldBe` cred1
    statementShape statement3 `shouldBe` statementShape statement1
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- Brief 7 / B10 item 10: an exhausted balance is not an error -- no credential, and a statement
-- ending at zero months that says why.
testBadgeServiceIssueBadgeExhaustedBalance :: HasCallStack => TestParams -> IO ()
testBadgeServiceIssueBadgeExhaustedBalance ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedTestCodes ps [(BTSupporter, 1, testCodeExpiry)] >>= writeIORef codeRef . head
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    (_, statement1) <- expectCredential "redeem one month" client
    statementShape statement1 `shouldBe` [(1, 1, "credit payment (no invoiceId)"), (-1, 0, "debit badge")]
    advanceTestClockMonths clock 1
    sendRequest client bsLink signer (issueRequest signer BTSupporter "" unknownEntryUuid)
    (credential, statement2) <- expectCredentialResponse "exhausted balance" client
    credential `shouldBe` Nothing
    statementShape statement2 `shouldBe` statementShape statement1
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- B10 items 8 and 15 / brief 9: a second code under a purchase key that already exists credits
-- the existing ledger, gets its OWN payments row while the purchase keeps pointing at the first
-- one, and -- because the month is already issued -- writes no second debit(badge) and no second
-- issuance, returning the credential that already exists. A code of a different tier under the
-- same key is bad_request.
testBadgeServiceSecondCodeSamePurchaseKey :: HasCallStack => TestParams -> IO ()
testBadgeServiceSecondCodeSamePurchaseKey ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codesRef <- newIORef []
  let seedCodes =
        seedTestCodes ps [(BTSupporter, 3, testCodeExpiry), (BTSupporter, 3, testCodeExpiry), (BTLegend, 3, testCodeExpiry)]
          >>= writeIORef codesRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCodes $ \client bsLink -> do
    [code1, code2, legendCode] <- readIORef codesRef
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code1)
    (cred1, _) <- expectCredential "first code" client
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code2)
    (cred2, statement) <- expectCredential "second code, same key" client
    cred2 `shouldBe` cred1
    statementShape statement
      `shouldBe` [ (3, 3, "credit payment (no invoiceId)"),
                   (-1, 2, "debit badge"),
                   (3, 5, "credit payment (no invoiceId)")
                 ]
    sendRequest client bsLink signer (purchaseCodeRequest signer BTLegend legendCode)
    expectErrorCode "a different tier under an existing key" client "bad_request"
  withServiceDB ps $ \db -> do
    serviceRowCounts db `shouldReturn` (1, 2, 3, 1, 2)
    creditPayments <-
      DB.query_ db "SELECT payment_id FROM sx_badge_service_badge_ledger WHERE entry_type = 'credit' ORDER BY entry_id"
    [Only purchasePaymentId] <- DB.query_ db "SELECT payment_id FROM sx_badge_service_badge_purchases"
    case creditPayments of
      [Only firstPayment, Only secondPayment] -> do
        (firstPayment :: Text) `shouldNotBe` secondPayment
        purchasePaymentId `shouldBe` Just firstPayment
      other -> expectationFailure $ "expected two credit entries with distinct payments, got: " <> show other

-- B10 item 6, the security assertion of this step, in the form C4 left it: the tier a credential
-- carries is the CODE's, never the request's. A client cannot know a code's tier before it
-- redeems it -- a code carries none (B3) and the response is what states it (core §5) -- so
-- 'purchaseBadge{code}' signs the tier the funding bought and ignores the one the request names
-- ('withFundedBadgeType', plan §9). A LEGEND request over a SUPPORTER code must therefore be
-- answered with a SUPPORTER credential, on a supporter ledger and a supporter purchase row.
--
-- __Each of the four assertions catches a different mutation__, and only the first is the proof
-- of the override:
--
-- * the CREDENTIAL tier is the one 'withFundedBadgeType' decides. Drop the override and this is
--   the only assertion that fails, with 'legend' -- the service handing out a signed credential
--   of a tier the code did not buy, which is the whole point of the test.
-- * the LEDGER tiers and the PURCHASE tier come from @writeTxn@'s own @badgeType@, which is the
--   code's on either side of that mutation. They pin 'planTxn'\/@writeTxn@ instead: that the
--   rows a redemption writes are the funding's tier and not the request's, which no other test
--   states.
-- * the REPLAY returns the credential already issued, rather than refusing a request whose tier
--   differs -- the guard C4 deleted with the refusal it belonged to.
--
-- 'issueBadge' is deliberately unchanged: there the tier is the purchase's own
-- 'current_badge_type', which the client HOLDS and must state, so a mismatch is still
-- bad_request -- and that is why the override lives at this one call site and not in
-- 'issueSignedBadge', which both paths share.
testBadgeServiceSignsFundedTier :: HasCallStack => TestParams -> IO ()
testBadgeServiceSignsFundedTier ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    sendRequest client bsLink signer (purchaseCodeRequest signer BTLegend code)
    (cred1, statement) <- expectCredential "legend request with a supporter code" client
    let BadgeCredential {badgeInfo = BadgeInfo {badgeType = credentialType}} = cred1
    ("credential tier" :: String, credentialType) `shouldBe` ("credential tier", BTSupporter)
    statementShape statement `shouldBe` [(3, 3, "credit payment (no invoiceId)"), (-1, 2, "debit badge")]
    -- the rows the redemption wrote carry the code's tier as well -- this pins writeTxn, not the
    -- override (field 5 of StatementEntry, constructed positionally as everywhere else here)
    let BadgeStatement {entries} = statement
        entryTypes = map (\(StatementEntry _ _ _ _ bt _ _ _) -> bt) entries
    ("ledger tiers" :: String, entryTypes) `shouldBe` ("ledger tiers", [BTSupporter, BTSupporter])
    -- the replay under the same legend request hands back the very same supporter credential
    sendRequest client bsLink signer (purchaseCodeRequest signer BTLegend code)
    (cred2, _) <- expectCredential "legend request replaying the same redemption" client
    cred2 `shouldBe` cred1
    sendRequest client bsLink signer (issueRequest signer BTLegend "" unknownEntryUuid)
    expectErrorCode "issueBadge naming another tier" client "bad_request"
  withServiceDB ps $ \db -> do
    -- exactly what the one redemption writes: the replay and the issueBadge refusal wrote nothing
    serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)
    -- likewise writeTxn's, not the override's
    [Only purchaseType] <- DB.query_ db "SELECT current_badge_type FROM sx_badge_service_badge_purchases"
    ("purchase tier" :: String, purchaseType :: BadgeType) `shouldBe` ("purchase tier", BTSupporter)

-- B10 item 7: a purchase carrying an upgrade is refused before the payment is even looked at, so
-- the code is not consumed by a request whose upgrade would have been silently dropped.
testBadgeServiceUpgradeIsBadRequest :: HasCallStack => TestParams -> IO ()
testBadgeServiceUpgradeIsBadRequest ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  otherSigner <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    -- BadgeUpgrade is constructed positionally: its 'receipt' and 'balance' fields collide with
    -- BadgeServiceResponse's and BSCIssueBadge's, so only its constructor is imported.
    let upgrade =
          BadgeUpgrade
            (signerPub otherSigner)
            "test-receipt"
            (C.sign' (signerPriv otherSigner) "test-receipt")
            (BadgeBalance (StatementEntry unknownEntryUuid 1 1 testClockStart BTSupporter Nothing testClockStart (SECredit SCOpening)))
    sendRequest client bsLink signer (purchaseRequest signer (badgeRequestFor signer BTSupporter "") (Just upgrade) code)
    expectErrorCode "purchase carrying an upgrade" client "bad_request"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    _ <- expectCredential "the same code still redeems" client
    pure ()
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- B10 item 12, first half: a reserved (non-empty) badgeExtra is a CLIENT fault -- bad_request,
-- terminal -- refused by both handlers before anything is classified, planned or signed, leaving
-- the code unredeemed. Its counterpart, a signing failure answering 'internal', cannot be induced
-- from a test without a fault injector and is recorded as untested rather than folded in here:
-- conflating the two is the defect B7's fix round removed.
testBadgeServiceBadgeExtraIsBadRequest :: HasCallStack => TestParams -> IO ()
testBadgeServiceBadgeExtraIsBadRequest ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    sendRequest client bsLink signer (purchaseRequest signer (badgeRequestFor signer BTSupporter "reserved") Nothing code)
    expectErrorCode "purchaseBadge with a reserved badgeExtra" client "bad_request"
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    _ <- expectCredential "the same code still redeems" client
    sendRequest client bsLink signer (issueRequest signer BTSupporter "reserved" unknownEntryUuid)
    expectErrorCode "issueBadge with a reserved badgeExtra" client "bad_request"
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- B10 item 11: the statement cursor. An assertion naming an entry the service holds returns only
-- what follows it and echoes it back; an unknown uuid, or -- the privacy assertion -- one
-- belonging to ANOTHER purchase, returns the complete history with previousEntryId absent, and
-- never a single entry of that other purchase.
testBadgeServiceIssueBadgeCursor :: HasCallStack => TestParams -> IO ()
testBadgeServiceIssueBadgeCursor ps = do
  clock <- newTestClock testClockStart
  signer <- newTestSigner
  otherSigner <- newTestSigner
  codesRef <- newIORef []
  let seedCodes = seedTestCodes ps [(BTSupporter, 3, testCodeExpiry), (BTSupporter, 3, testCodeExpiry)] >>= writeIORef codesRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCodes $ \client bsLink -> do
    [code, otherCode] <- readIORef codesRef
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    (_, ownStatement) <- expectCredential "own redemption" client
    sendRequest client bsLink otherSigner (purchaseCodeRequest otherSigner BTSupporter otherCode)
    (_, otherStatement) <- expectCredential "other purchase's redemption" client
    ownEntries <- case statementEntryIds ownStatement of
      ids@[_, _] -> pure ids
      other -> expectationFailure ("expected two own entries, got: " <> show other) >> error "unreachable"
    otherEntries <- case statementEntryIds otherStatement of
      ids@[_, _] -> pure ids
      other -> expectationFailure ("expected two other entries, got: " <> show other) >> error "unreachable"
    -- a cursor the service holds for THIS purchase: only what follows it
    sendRequest client bsLink signer (issueRequest signer BTSupporter "" (head ownEntries))
    (_, afterCursor) <- expectCredential "cursor hit" client
    statementEntryIds afterCursor `shouldBe` drop 1 ownEntries
    previousEntryId afterCursor `shouldBe` Just (head ownEntries)
    -- a uuid belonging to a different purchase resolves to nothing: the full own history follows
    sendRequest client bsLink signer (issueRequest signer BTSupporter "" (head otherEntries))
    (_, crossPurchase) <- expectCredential "cursor from another purchase" client
    statementEntryIds crossPurchase `shouldBe` ownEntries
    previousEntryId crossPurchase `shouldBe` Nothing
    -- and so does one the service has never seen
    sendRequest client bsLink signer (issueRequest signer BTSupporter "" unknownEntryUuid)
    (_, unknownCursor) <- expectCredential "unknown cursor" client
    statementEntryIds unknownCursor `shouldBe` ownEntries
    previousEntryId unknownCursor `shouldBe` Nothing
  -- three cached issueBadge calls wrote nothing: two purchases, two entries each
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (2, 2, 4, 2, 2)

-- B10 item 14, the regression guard for B7's §9 ruling: the cached credential is fetched by
-- probing the issuance table at NOW, not at @addMonths (-1) balanceStartTs@. Stepping a month
-- back from a clamped boundary (31 Jan + 1 month = 28 Feb; 28 Feb - 1 month = 28 Jan) lands
-- BEFORE the period it came from, so the brief's original formula returns the PREVIOUS month's
-- credential -- a wrong credential, not an absent one. Two consecutive periods are issued across
-- exactly that boundary so the wrong answer is available to be returned.
testBadgeServiceCachedIssuanceAtClampedMonthBoundary :: HasCallStack => TestParams -> IO ()
testBadgeServiceCachedIssuanceAtClampedMonthBoundary ps = do
  clock <- newTestClock (UTCTime (fromGregorian 2025 12 31) noonUTC)
  signer <- newTestSigner
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
  withBadgeServiceClock ps (readIORef clock) (writeTestBadgeServiceConfig ps) seedCode $ \client bsLink -> do
    code <- readIORef codeRef
    -- period 1: [2025-12-31, 2026-01-31)
    sendRequest client bsLink signer (purchaseCodeRequest signer BTSupporter code)
    (cred1, _) <- expectCredential "period starting 2025-12-31" client
    -- period 2: [2026-01-31, 2026-02-28) -- the clamped boundary
    setTestClock clock (UTCTime (fromGregorian 2026 1 31) noonUTC)
    sendRequest client bsLink signer (issueRequest signer BTSupporter "" unknownEntryUuid)
    (cred2, _) <- expectCredential "period starting 2026-01-31" client
    cred2 `shouldNotBe` cred1
    -- inside period 2: the cached credential must be period 2's, never period 1's
    setTestClock clock (UTCTime (fromGregorian 2026 2 15) noonUTC)
    sendRequest client bsLink signer (issueRequest signer BTSupporter "" unknownEntryUuid)
    (cached, _) <- expectCredential "inside the already-issued clamped month" client
    cached `shouldBe` cred2
    cached `shouldNotBe` cred1
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 3, 2, 1)

#if !defined(dbPostgres)
-- What one table holds, as the database itself reports it: its declared foreign-key targets and
-- its column names.
data TableRefs = TableRefs
  { refTable :: Text,
    refFkTargets :: [Text],
    refColumns :: [Text]
  }
  deriving (Show)

-- | A table holds an order (purchase) reference if it declares a foreign key to that table __or__
-- names a column after it. The name half is the load-bearing one for what comes next: a plain
-- @web_order_id TEXT@ with no @REFERENCES@ clause is invisible to 'pragma_foreign_key_list', and
-- D0 -- the very next step -- is what adds order tables. A false alarm from an innocent column
-- name (@sort_order@) is the intended trade: it costs one reader a minute, and the failure it
-- guards against costs the privacy claim the whole web checkout rests on.
holdsOrderRef :: TableRefs -> Bool
holdsOrderRef TableRefs {refFkTargets, refColumns} =
  any ("web_orders" `T.isSuffixOf`) refFkTargets || any ("order" `T.isInfixOf`) refColumns

holdsPurchaseRef :: TableRefs -> Bool
holdsPurchaseRef TableRefs {refFkTargets, refColumns} =
  any ("badge_purchases" `T.isSuffixOf`) refFkTargets || any ("purchase" `T.isInfixOf`) refColumns

-- The brief's schema assertion (§3 Linkage), and the regression guard for the privacy claim the
-- whole web-checkout design rests on: an order and a purchase must never be joinable, so NO table
-- may carry a reference to @web_orders and a reference to @badge_purchases at once. Enumerated
-- from the database itself (sqlite_master, each table's foreign keys and each table's columns),
-- not from the migration source, so it fails the day a later step adds such a column. Reads a
-- SQLite file, hence the guard: A3's Postgres run of this spec would break on it otherwise.
--
-- Three anti-vacuity controls, because an assertion that only ever says "no rows matched" is one
-- typo away from proving nothing: the enumeration must see both tables by name, it must see the
-- real declared foreign key from @codes to @badge_purchases, and -- the control for the half that
-- foreign keys cannot see -- a probe table carrying both columns with NO foreign keys at all must
-- be caught, then dropped before the real scan.
testBadgeServiceNoTableLinksOrdersToPurchases :: HasCallStack => TestParams -> IO ()
testBadgeServiceNoTableLinksOrdersToPurchases ps =
  withFreshBadgeStore ps $ \st -> do
    let refsOf table = do
          fks <- withConnection st $ \db -> DB.query db "SELECT \"table\" FROM pragma_foreign_key_list(?)" (Only table)
          cols <- withConnection st $ \db -> DB.query db "SELECT name FROM pragma_table_info(?)" (Only table)
          pure TableRefs {refTable = table, refFkTargets = map fromOnly fks, refColumns = map fromOnly cols}
        listTables =
          map fromOnly
            <$> withConnection
              st
              (\db -> DB.query_ db "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name")
    -- control 3, first: exactly the shape D0 could add without a REFERENCES clause
    withConnection st $ \db -> DB.execute_ db "CREATE TABLE b10_linkage_probe (web_order_id TEXT, badge_purchase_id INTEGER)"
    probe <- refsOf "b10_linkage_probe"
    (refTable probe, holdsOrderRef probe, holdsPurchaseRef probe) `shouldBe` ("b10_linkage_probe", True, True)
    refFkTargets probe `shouldBe` [] -- and it is caught with no foreign key to go on
    withConnection st $ \db -> DB.execute_ db "DROP TABLE b10_linkage_probe"
    tableNames <- listTables
    -- controls 1 and 2: the enumeration sees both tables, and sees a real declared foreign key
    tableNames `shouldSatisfy` elem "sx_badge_service_web_orders"
    tableNames `shouldSatisfy` elem "sx_badge_service_codes"
    tableNames `shouldSatisfy` notElem "b10_linkage_probe"
    codeRefs <- refsOf "sx_badge_service_codes"
    refFkTargets codeRefs `shouldSatisfy` any ("badge_purchases" `T.isSuffixOf`)
    references <- forM tableNames refsOf
    map refTable (filter holdsOrderRef references) `shouldBe` ["sx_badge_service_web_orders"]
    map refTable (filter (\refs -> holdsOrderRef refs && holdsPurchaseRef refs) references) `shouldBe` []
    -- The column scan above is per-table and cannot see a join built out of two hops. This one
    -- exists and is complete: @web_orders.invoice_id -> @invoices <- @payments.invoice_id, then
    -- @payments.payment_id <- @badge_purchases.payment_id. It is empty only because
    -- 'createCodePayment' hardcodes invoice_id NULL for a code payment -- nothing in the schema
    -- enforces that, so it is asserted here, over rows shaped exactly as a settled web order and
    -- a code redemption leave them.
    withConnection st $ \db -> do
      DB.execute_ db "INSERT INTO sx_badge_service_invoices (invoice_id, provider, price, amount, currency, expires_at, status, created_at, updated_at) VALUES ('b10-invoice','btcpay',1000,1000,'USD','2026-03-10','paid','2026-03-10','2026-03-10')"
      DB.execute_ db "INSERT INTO sx_badge_service_web_orders (order_id, invoice_id, method, short_ref, badge_type, months, status, created_at, updated_at) VALUES ('b10-order','b10-invoice','btc','B10RF','supporter',3,'paid','2026-03-10','2026-03-10')"
      DB.execute_ db "INSERT INTO sx_badge_service_payments (payment_id, invoice_id, provider, status, created_at, updated_at) VALUES ('b10-payment', NULL, 'code', 'settled', '2026-03-10', '2026-03-10')"
      DB.execute_ db "INSERT INTO sx_badge_service_badge_purchases (purchase_key, master_key, initial_badge_type, current_badge_type, payment_id, status, created_at, updated_at) VALUES (x'0102', x'0304', 'supporter', 'supporter', 'b10-payment', 'issued', '2026-03-10', '2026-03-10')"
    -- one long line each: this module is compiled with CPP, which eats a backslash-newline string
    -- gap as a line continuation before GHC ever lexes the literal
    let joinedOrdersToPurchases = do
          [Only n] <-
            withConnection st $ \db ->
              DB.query_ db "SELECT count(*) FROM sx_badge_service_web_orders o JOIN sx_badge_service_payments p ON p.invoice_id = o.invoice_id JOIN sx_badge_service_badge_purchases bp ON bp.payment_id = p.payment_id"
          pure (n :: Int)
    -- positive control: the join is written correctly and DOES resolve an order to a purchase the
    -- moment a payment carries the order's invoice. Without this, "0 rows" could mean a typo.
    withConnection st $ \db -> DB.execute_ db "UPDATE sx_badge_service_payments SET invoice_id = 'b10-invoice'"
    joinedOrdersToPurchases `shouldReturn` 1
    withConnection st $ \db -> DB.execute_ db "UPDATE sx_badge_service_payments SET invoice_id = NULL"
    -- and with the payment shaped as this milestone writes it, there is no path from the order to
    -- the purchase at all (docs/protocol/badges-web.md §7)
    joinedOrdersToPurchases `shouldReturn` 0
#endif

-- B8's Verify line, formally owed by B10: @codes issue@ mints the requested number of codes,
-- prints each plaintext exactly once, stores only distinct hashes of them, @codes revoke@ revokes
-- the whole batch, @codes status@ reports what the row says, and a past @--expires@ is accepted.
-- Driven through 'runAdminCmd' -- the real entry point 'Main.hs' calls, opening its own store and
-- reading the same ini a service run does -- with stdout captured, since the plaintext codes exist
-- nowhere else.
testBadgeServiceCodesIssueRevokeStatus :: HasCallStack => TestParams -> IO ()
testBadgeServiceCodesIssueRevokeStatus ps@TestParams {tmpPath} = do
  writeTestBadgeServiceConfig ps
  let adminOpts cmd =
        AdminOpts
          { adminCoreOptions = coreOptions (mkBadgeServiceOpts ps),
            adminConfigFile = badgeServiceConfigPath tmpPath,
            adminCmd = cmd
          }
      issueOpts batch expires =
        CmdIssue IssueOpts {issueType = BTSupporter, issueMonths = 3, issueCount = 10, issueBatch = batch, issueExpires = expires}
      runCodes label cmd = withCapturedStdout (tmpPath </> (label <> ".out")) (runAdminCmd (adminOpts cmd))
  issued <- runCodes "issue" (issueOpts testCodeBatch Nothing)
  length issued `shouldBe` 10
  length (nub issued) `shouldBe` 10
  all (Codes.verifyChecksum . Codes.normalizeCode) issued `shouldBe` True
  length (nub (map (Codes.codeHash . Codes.normalizeCode) issued)) `shouldBe` 10
  -- every printed code resolves to a stored row of its own -- @codes status@ hashes what it is
  -- given and looks that up, so this is what proves the rows are keyed by the hash of exactly
  -- what was printed. The revoke count below then confirms there are ten of them and no more.
  forM_ (zip [1 :: Int ..] issued) $ \(n, code) -> do
    status <- runCodes ("status-issued-" <> show n) (CmdStatus code)
    (n, status) `shouldSatisfy` any (T.isInfixOf "status=unredeemed") . snd
#if !defined(dbPostgres)
  -- the property the whole design hinges on: no plaintext code anywhere in the database file,
  -- neither as printed nor as normalized. Reads the SQLite file, hence the guard.
  dbBytes <- BC.readFile (tmpPath </> (serviceDbPrefix <> "_chat.db"))
  -- positive control FIRST: the batch label is stored in the clear and must be found in these
  -- bytes. Without it every assertion below is an absence, and an absence passes for the wrong
  -- reason the day the rows move to another file, the file is renamed, or the scan reads a
  -- database that was never written to.
  (testCodeBatch, encodeUtf8 testCodeBatch `BS.isInfixOf` dbBytes) `shouldBe` (testCodeBatch, True)
  forM_ issued $ \code -> do
    (code, encodeUtf8 code `BS.isInfixOf` dbBytes) `shouldBe` (code, False)
    (code, encodeUtf8 (Codes.normalizeCode code) `BS.isInfixOf` dbBytes) `shouldBe` (code, False)
#endif
  statusBefore <- runCodes "status-before" (CmdStatus (head issued))
  statusBefore `shouldSatisfy` any (T.isInfixOf "status=unredeemed")
  statusBefore `shouldSatisfy` any (T.isInfixOf ("batch=" <> testCodeBatch))
  statusBefore `shouldSatisfy` any (T.isInfixOf "type=supporter")
  statusBefore `shouldSatisfy` any (T.isInfixOf "months=3")
  -- a past --expires is stored exactly as given, with no validation beyond the date format
  pastExpiry <- runCodes "issue-past" (issueOpts "b10-expired-batch" (Just (fromGregorian 2000 1 1)))
  length pastExpiry `shouldBe` 10
  statusPast <- runCodes "status-past" (CmdStatus (head pastExpiry))
  statusPast `shouldSatisfy` any (T.isInfixOf "expires=2000-01-01")
  revoked <- runCodes "revoke" (CmdRevoke testCodeBatch)
  revoked `shouldBe` ["10 code(s) revoked in batch " <> testCodeBatch]
  statusAfter <- runCodes "status-after" (CmdStatus (head issued))
  statusAfter `shouldSatisfy` any (T.isInfixOf "status=revoked")
  -- the other batch is untouched: revoke is scoped to the batch it names
  statusOther <- runCodes "status-other-batch" (CmdStatus (head pastExpiry))
  statusOther `shouldSatisfy` any (T.isInfixOf "status=unredeemed")

-- | Runs an action with stdout redirected to a file, returning the lines it printed. The only way
-- to read what @codes issue@ minted: the plaintext codes are printed once and never stored.
withCapturedStdout :: FilePath -> IO () -> IO [Text]
withCapturedStdout path action = do
  saved <- hDuplicate stdout
  h <- openFile path WriteMode
  (hDuplicateTo h stdout >> action) `finally` do
    hFlush stdout
    hDuplicateTo saved stdout
    hClose saved
    hClose h
  T.lines <$> TIO.readFile path

-- B8's remaining Verify item: lifetime badges are out of scope, so @--type investor@ is rejected
-- by the parser itself, before any store is opened.
testBadgeServiceCodesRejectsInvestorType :: HasCallStack => TestParams -> IO ()
testBadgeServiceCodesRejectsInvestorType _ps =
  case O.execParserPure O.defaultPrefs (O.info adminCommandParser O.fullDesc) issueInvestor of
    O.Failure failure -> fst (O.renderFailure failure "codes") `shouldSatisfy` isInfixOf "expected 'supporter' or 'legend'"
    O.Success _ -> expectationFailure "expected --type investor to be rejected"
    O.CompletionInvoked _ -> expectationFailure "expected --type investor to be rejected"
  where
    issueInvestor = ["issue", "--type", "investor", "--months", "3", "--count", "10", "--batch", "b10-investor"]

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

-- markCodeRedeemed refuses a REVOKED code, not only an already-redeemed one. B8's `codes revoke`
-- runs in a second process against the same database, and the service separates classification
-- from this write by the signing IO, so a revocation issued in that window must not be silently
-- overwritten -- which is the whole point of being able to revoke a code that is being abused.
-- SECodeConflict is what the handler re-classifies on, and a revoked code then answers
-- code_invalid.
testBadgeStoreMarkCodeRedeemedRefusesRevoked :: HasCallStack => TestParams -> IO ()
testBadgeStoreMarkCodeRedeemedRefusesRevoked ps =
  withFreshBadgeStore ps $ \st -> do
    (purchaseKey, _) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    BadgePurchaseRow {badgePurchaseId} <-
      expectRight $ withServiceTransaction st $ \db -> createPurchase db purchaseKey masterKey BTSupporter now
    codeHash <- getRandomBytes 32
    let newCode = NewBadgeCode {codeHash, badgeType = BTSupporter, months = 3, batch = "test-batch", expiresAt = addUTCTime (30 * nominalDay) now}
    _ <- expectRight $ withServiceTransaction st $ \db -> insertCodes db [newCode] now
    _ <- expectRight $ withServiceTransaction st $ \db -> revokeCode db codeHash now
    conflicted <- withServiceTransaction st $ \db -> markCodeRedeemed db codeHash badgePurchaseId now
    conflicted `shouldBe` Left SECodeConflict
    -- and the refusal left the row alone: still revoked, still unredeemed
    Just (BadgeCode {redeemedPurchaseId, redeemedAt, revokedAt}, redeemer) <-
      expectRight $ withServiceTransaction st $ \db -> getCodeByHash db codeHash
    redeemedPurchaseId `shouldBe` Nothing
    redeemedAt `shouldBe` Nothing
    isJust revokedAt `shouldBe` True
    redeemer `shouldBe` Nothing

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

-- C5 -- the client against the live badge service -----------------------------
--
-- C4's redeem path, C2's commands and C3's worker, driven through the app's own API against the
-- REAL service rather than a stub. C4's Verify line is "covered by C5", so everything below is
-- the only check that step has, and the rules B10 works to hold here too:
--   * time is injected on BOTH sides -- the service's clock (A6, 'withBadgeServiceAddress') and
--     the client's 'badgeCurrentTime' (C2) -- so no test sleeps and no test waits on
--     'badgePassInterval';
--   * expected values are literal (fixed dates, fixed row shapes), never recomputed the way
--     production computes them;
--   * "wrote nothing" is a row count on BOTH databases, never "the error read the same".

badgeClientDbPrefix :: FilePath
badgeClientDbPrefix = "badge_client"

badgeSecondClientDbPrefix :: FilePath
badgeSecondClientDbPrefix = "badge_client2"

badgeStubDbPrefix :: FilePath
badgeStubDbPrefix = "badge_stub"

-- | The client config that reaches the badge service: the address it published and the issuer key
-- it actually loaded, so a credential it signs verifies here and one signed by anything else does
-- not.
badgeClientCfg :: String -> BBSPublicKey -> ChatConfig
badgeClientCfg address pk =
  testCfg {badgeServiceAddress = Just (badgeServiceTarget address), badgePublicKeys = testBadgeKeys pk}

-- | Parses a published address into the 'ConnectTarget' 'badgeServiceAddress' holds. Both forms
-- the harness reads back decode through this: the short link and the full contact request URI.
badgeServiceTarget :: HasCallStack => String -> ConnectTarget 'CMContact
badgeServiceTarget address = case strDecode (BC.pack address) of
  Right target -> target
  Left e -> error $ "badge service address " <> address <> " is not a connect target: " <> e

-- | The C5 harness: B10's badge service on the given clock, and the client that reaches it.
--
-- The client's config is the body's, because the cases differ in one field each: the issuer key
-- (the unverifiable-credential case configures the WRONG one), the address (the resolution case
-- uses the full URI), the badge clock (the worker case shares the service's, the profile-switch
-- case gates it) and the agent's service request timeout (the stopped-service case has nothing
-- listening). Both published forms of the address are handed to the config builder and to the
-- body, which needs them to configure a SECOND client against the same service.
withBadgeClientCfg :: HasCallStack => TestParams -> IO UTCTime -> IO () -> ((String, String) -> BBSPublicKey -> ChatConfig) -> (TestCC -> (String, String) -> IO ()) -> IO ()
withBadgeClientCfg ps serviceClock seedCodes mkCfg test =
  withBadgeServiceAddress ps serviceClock (writeTestBadgeServiceConfig ps) seedCodes $ \links withService -> do
    pk <- readTestIssuerPublicKey (tmpPath ps)
    withService $
      withNewTestChatCfg ps (mkCfg links pk) badgeClientDbPrefix aliceProfile $ \alice ->
        test alice links

-- | The common shape: ONE clock driving the service and the client's badge time alike, so every
-- date below is a literal on both sides and a month is crossed by moving one 'IORef'.
withBadgeClient :: HasCallStack => TestParams -> IORef UTCTime -> IO () -> (TestCC -> IO ()) -> IO ()
withBadgeClient ps clock seedCodes test =
  withBadgeClientCfg ps (readIORef clock) seedCodes (sharedClockCfg clock) (\alice _ -> test alice)

sharedClockCfg :: IORef UTCTime -> (String, String) -> BBSPublicKey -> ChatConfig
sharedClockCfg clock (sLink, _fullLink) pk = (badgeClientCfg sLink pk) {badgeCurrentTime = readIORef clock}

-- | The same, on the REAL clock on both sides: for the example whose assertion depends on the
-- credential still being unexpired when something OTHER than the badge clock reads it -- a
-- CONTACT's copy of the profile, whose badge status is computed against wall-clock time
-- ('Simplex.Chat.Types.toLocalProfile') and would read as expired for a period issued in a fixed
-- past month.
withBadgeClientNow :: HasCallStack => TestParams -> IO () -> (TestCC -> (String, String) -> IO ()) -> IO ()
withBadgeClientNow ps seedCodes = withBadgeClientCfg ps getCurrentTime seedCodes (\(sLink, _) pk -> badgeClientCfg sLink pk)

-- | Runs an action against the CLIENT's store, the way 'withServiceDB' runs one against the
-- service's.
withClientStore :: TestCC -> (DB.Connection -> IO a) -> IO a
withClientStore TestCC {chatController = ChatController {chatStore}} = withTransaction chatStore

clientUser :: HasCallStack => TestCC -> IO User
clientUser TestCC {chatController = ChatController {currentUser}} =
  readTVarIO currentUser >>= maybe (expectationFailure "no active user" >> error "unreachable") pure

-- | The rows one redemption writes on the CLIENT, counted together: payments, purchases, ledger
-- entries, issuances, and profiles pointing at a shown badge. Every "writes nothing" assertion
-- below pins this tuple, because an unchanged error message is not evidence that nothing was
-- written.
clientBadgeRowCounts :: TestCC -> IO (Int, Int, Int, Int, Int)
clientBadgeRowCounts cc =
  withClientStore cc $ \db -> do
    payments <- countRows db "payments"
    purchases <- countRows db "badge_purchases"
    entries <- countRows db "badge_ledger"
    issuances <- countRows db "badge_issuances"
    [Only shown] <- DB.query_ db "SELECT count(*) FROM users WHERE shown_badge_id IS NOT NULL"
    pure (payments, purchases, entries, issuances, shown)
  where
    countRows db table = do
      [Only n] <- DB.query_ db (fromString ("SELECT count(*) FROM " <> table))
      pure n

noClientBadgeRows :: HasCallStack => String -> TestCC -> Expectation
noClientBadgeRows label cc = do
  counts <- clientBadgeRowCounts cc
  (label, counts) `shouldBe` (label, (0, 0, 0, 0, 0))

-- | The client's ledger replica as the database holds it, in append order.
type ClientLedgerRow = (Text, Int, Int, UTCTime, Text, Text, Maybe Text, Maybe Text, Maybe Text)

clientLedgerRows :: TestCC -> IO [ClientLedgerRow]
clientLedgerRows cc =
  withClientStore cc $ \db ->
    DB.query_ db "SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_badge_type, entry_type, entry_credit_type, entry_debit_type, payment_id FROM badge_ledger ORDER BY entry_id"

-- | The same columns from the SERVICE's own ledger, which is what the replica is compared to.
type ServiceLedgerParityRow = (Text, Int, Int, UTCTime, Text, Text, Maybe Text, Maybe Text)

serviceLedgerParityRows :: DB.Connection -> IO [ServiceLedgerParityRow]
serviceLedgerParityRows db =
  DB.query_ db "SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_badge_type, entry_type, entry_credit_type, entry_debit_type FROM sx_badge_service_badge_ledger ORDER BY entry_id"

-- | §10's row-for-row comparison of the two ledgers: @entry_uuid@, @change_months@,
-- @balance_months@, @balance_start_ts@, @balance_badge_type@ and the entry type must agree
-- exactly, and the client's @payment_id@ must be NULL -- the wire field is the SERVICE's invoice
-- id and the client column holds a CLIENT payments id, so carrying it across would tie the two
-- databases together (C1).
--
-- Compared entry by entry rather than list against list, so a failure names the entry.
shouldMatchServiceLedger :: HasCallStack => [ClientLedgerRow] -> [ServiceLedgerParityRow] -> Expectation
shouldMatchServiceLedger clientRows serviceRows = do
  ("ledger entry count" :: String, length clientRows) `shouldBe` ("ledger entry count", length serviceRows)
  forM_ (zip3 [1 :: Int ..] clientRows serviceRows) $ \(n, c, s) -> do
    let (cUuid, cChange, cBalance, cTs, cBadgeType, cType, cCredit, cDebit, cPayment) = c
        (sUuid, sChange, sBalance, sTs, sBadgeType, sType, sCredit, sDebit) = s
        label = "ledger entry " <> show n
    (label, (cUuid, cChange, cBalance, cBadgeType, cType, cCredit, cDebit))
      `shouldBe` (label, (sUuid, sChange, sBalance, sBadgeType, sType, sCredit, sDebit))
    cTs `shouldBeStoredAt` sTs
    (label <> " client payment_id", cPayment) `shouldBe` (label <> " client payment_id", Nothing)

-- | The two lines 'CRBadgeState' prints -- the badge and the web base url, which no test config
-- sets.
expectBadgeState :: HasCallStack => TestCC -> String -> Expectation
expectBadgeState cc line = do
  cc <## line
  cc <## "badge site: not configured"

-- | Nothing more is reported: no event followed the response.
reportsNoBadgeEvent :: HasCallStack => TestCC -> Expectation
reportsNoBadgeEvent cc = cc <// 500000

purchaseCodeCmd :: Text -> String
purchaseCodeCmd code = "/_badge purchase 1 {\"type\":\"code\",\"code\":\"" <> T.unpack code <> "\"}"

-- The badge state a 3-month code redeemed at 'testClockStart' leaves, spelled out: the period is
-- [2026-03-10, 2026-04-10) and the 2 months still funded run from its end, so the profile is paid
-- through 2026-06-10.
threeMonthBadgeState :: String
threeMonthBadgeState = "supporter badge 1 (shown): issued, 2 month(s) left, paid through 2026-06-10"

-- | The stored credential of every issuance the client holds, oldest period first.
clientIssuedCredentials :: HasCallStack => TestCC -> IO [BadgeCredential]
clientIssuedCredentials cc = do
  rows <- withClientStore cc $ \db -> DB.query_ db "SELECT credential FROM badge_issuances ORDER BY period_start"
  mapM decodeCredential rows
  where
    decodeCredential (Only (Binary bs)) =
      maybe (expectationFailure "a stored badge credential does not decode" >> error "unreachable") pure $
        J.decodeStrict (bs :: BC.ByteString)

-- | The master key of the client's only purchase, as the row holds it.
clientPurchaseMasterKey :: HasCallStack => TestCC -> IO BadgeMasterKey
clientPurchaseMasterKey cc =
  withClientStore cc (\db -> DB.query_ db "SELECT master_key FROM badge_purchases") >>= \case
    [Only (Binary mk)] -> pure $ BadgeMasterKey mk
    other -> expectationFailure ("expected exactly one client purchase, got " <> show (length other)) >> error "unreachable"

credentialMasterKey :: BadgeCredential -> BadgeMasterKey
credentialMasterKey BadgeCredential {masterKey} = masterKey

credentialBadgeType :: BadgeCredential -> BadgeType
credentialBadgeType BadgeCredential {badgeInfo = BadgeInfo {badgeType}} = badgeType

-- The happy path ---------------------------------------------------------------

-- | A profile with no prior connection to the service redeems a seeded code on the first attempt,
-- and everything that redemption leaves behind is checked in one place: the reported state, the
-- profile's own badge, the five client rows and their columns, the credential's verification under
-- the key the service actually loaded, and the ledger replica against the service's own rows.
--
-- One 'IORef' is both clocks, so every date is a literal and the client's badge status is computed
-- at the instant the service issued the period.
testC5RedeemCodeShowsBadge :: HasCallStack => TestParams -> IO ()
testC5RedeemCodeShowsBadge ps = do
  clock <- newTestClock testClockStart
  codeRef <- newIORef ""
  ledgerRef <- newIORef []
  withBadgeClient ps clock (seedOneSupporterCode ps >>= writeIORef codeRef) $ \alice -> do
    code <- readIORef codeRef
    alice ##> purchaseCodeCmd code
    expectBadgeState alice threeMonthBadgeState
    -- the response IS the state: a successful redemption emits no CEvtBadgeChanged, which would
    -- print the badge line a second time
    reportsNoBadgeEvent alice
    -- the profile carries the badge
    alice ##> "/p"
    alice <## "user profile: alice (Alice, * supporter)"
    alice <## "use /p <name> [<bio>] to change it"
    -- and the state reads the same when asked again (which also signals the worker, and it finds
    -- the month already issued and does nothing)
    alice ##> "/_badge state 1"
    expectBadgeState alice threeMonthBadgeState
    reportsNoBadgeEvent alice
    -- the credential verifies under the key the service actually loaded, so it went through
    -- 'verifyUserBadge' rather than being stored on the service's word
    issuerPub <- readTestIssuerPublicKey (tmpPath ps)
    cred <-
      clientIssuedCredentials alice >>= \case
        [c] -> pure c
        other -> expectationFailure ("expected exactly one issuance, got " <> show (length other)) >> error "unreachable"
    verifyCredential issuerPub cred `shouldReturn` True
    credentialBadgeType cred `shouldBe` BTSupporter
    -- and it is bound to the master key the purchase row holds
    clientPurchaseMasterKey alice `shouldReturn` credentialMasterKey cred
    -- exactly one row of each kind, with the columns C1 specified
    clientBadgeRowCounts alice `shouldReturn` (1, 1, 2, 1, 1)
    User {userId = aliceUserId} <- clientUser alice
    withClientStore alice $ \db -> do
      [paymentRow] <- DB.query_ db "SELECT payment_id, provider, invoice_id, status FROM payments"
      let (paymentRowId, provider, invoiceId, paymentStatus) = paymentRow :: (Text, Text, Maybe Text, Text)
      ("payment" :: String, (provider, invoiceId, paymentStatus)) `shouldBe` ("payment", ("code", Nothing, "settled"))
      [purchaseRow] <-
        DB.query_ db "SELECT badge_purchase_id, user_id, initial_badge_type, current_badge_type, status, payment_id FROM badge_purchases"
      let (purchaseId, purchaseUserId, initialType, currentType, purchaseStatus, purchasePayment) =
            purchaseRow :: (Int64, Int64, Text, Text, Text, Maybe Text)
      ("purchase" :: String, (purchaseUserId, initialType, currentType, purchaseStatus, purchasePayment))
        `shouldBe` ("purchase", (aliceUserId, "supporter", "supporter", "issued", Just paymentRowId))
      -- the private key column is the client-only one C1 added, and without it the purchase could
      -- never sign an issueBadge again
      [Only privKey] <- DB.query_ db "SELECT purchase_priv_key FROM badge_purchases"
      ("purchase private key" :: String, isJust (privKey :: Maybe (Binary BC.ByteString)))
        `shouldBe` ("purchase private key", True)
      [Only shownBadgeId] <- DB.query db "SELECT shown_badge_id FROM users WHERE user_id = ?" (Only aliceUserId)
      ("shown badge" :: String, shownBadgeId) `shouldBe` ("shown badge", Just purchaseId)
      -- the issuance carries the period the statement implied, and the credential's own expiry
      [issuanceRow] <-
        DB.query_ db "SELECT badge_purchase_id, badge_type, period_start, period_end, expiry, entry_id FROM badge_issuances"
      let (issuancePurchase, issuanceType, periodStart, periodEnd, expiry, entryId) =
            issuanceRow :: (Int64, Text, UTCTime, UTCTime, UTCTime, Maybe Int64)
      ("issuance" :: String, (issuancePurchase, issuanceType, entryId))
        `shouldBe` ("issuance", (purchaseId, "supporter", Nothing))
      periodStart `shouldBeStoredAt` testClockStart
      periodEnd `shouldBeStoredAt` firstPeriodEnd
      expiry `shouldBeStoredAt` firstCredentialExpiry
    -- held for the parity check below: the service's database cannot be opened while the service
    -- itself is running
    clientLedgerRows alice >>= writeIORef ledgerRef
  clientRows <- readIORef ledgerRef
  withServiceDB ps $ \db -> do
    serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)
    serviceLedgerParityRows db >>= shouldMatchServiceLedger clientRows

-- | The only assertion that the post-lock broadcast actually happens: a contact connected BEFORE
-- the redemption sees the badge on the profile afterwards, with no action of alice's beyond the
-- redemption itself.
--
-- The star on @alice *>@ is the assertion: a contact prints it only for a badge it verified and
-- found ACTIVE, and that status is computed against wall-clock time, hence the real clock here.
testC5ContactSeesBadgeAfterRedeem :: HasCallStack => TestParams -> IO ()
testC5ContactSeesBadgeAfterRedeem ps = do
  codeRef <- newIORef ""
  withBadgeClientNow ps (seedOneSupporterCode ps >>= writeIORef codeRef) $ \alice _links -> do
    issuerPub <- readTestIssuerPublicKey (tmpPath ps)
    withNewTestChatCfg ps (testCfg {badgePublicKeys = testBadgeKeys issuerPub}) badgeSecondClientDbPrefix bobProfile $ \bob -> do
      connectUsers alice bob
      -- before the redemption the profile bob holds carries no badge
      alice #> "@bob hi"
      bob <# "alice> hi"
      code <- readIORef codeRef
      alice ##> purchaseCodeCmd code
      alice <##. "supporter badge 1 (shown): issued, 2 month(s) left, paid through "
      alice <## "badge site: not configured"
      -- the badge XInfo is delivered in order before this message, so bob has stored it
      alice #> "@bob hi again"
      bob <# "alice *> hi again"

-- | The client half of the funded-tier ruling: a LEGEND code redeemed through the same tier-less
-- command yields a LEGEND badge. The request names supporter ('badgeCodeRequest' hardcodes it,
-- plan §9) and the service signs the tier the code funds, so the purchase row, the issuance and
-- the profile must all take the tier the RESPONSE carries -- which fails the day 'createPurchase'
-- is fed the request's tier instead of the credential's.
testC5RedeemLegendCodeGivesLegendBadge :: HasCallStack => TestParams -> IO ()
testC5RedeemLegendCodeGivesLegendBadge ps = do
  clock <- newTestClock testClockStart
  codeRef <- newIORef ""
  let seedCode = seedTestCodes ps [(BTLegend, 3, testCodeExpiry)] >>= writeIORef codeRef . head
  withBadgeClient ps clock seedCode $ \alice -> do
    code <- readIORef codeRef
    alice ##> purchaseCodeCmd code
    expectBadgeState alice "legend badge 1 (shown): issued, 2 month(s) left, paid through 2026-06-10"
    alice ##> "/p"
    alice <## "user profile: alice (Alice, * legend)"
    alice <## "use /p <name> [<bio>] to change it"
    cred <-
      clientIssuedCredentials alice >>= \case
        [c] -> pure c
        other -> expectationFailure ("expected exactly one issuance, got " <> show (length other)) >> error "unreachable"
    credentialBadgeType cred `shouldBe` BTLegend
    withClientStore alice $ \db -> do
      [purchaseTypes] <- DB.query_ db "SELECT initial_badge_type, current_badge_type FROM badge_purchases"
      ("purchase tier" :: String, purchaseTypes :: (Text, Text)) `shouldBe` ("purchase tier", ("legend", "legend"))
      [Only issuanceType] <- DB.query_ db "SELECT badge_type FROM badge_issuances"
      ("issuance tier" :: String, issuanceType :: Text) `shouldBe` ("issuance tier", "legend")

-- The paths that must write nothing ---------------------------------------------

-- | The three service refusals a code can earn, each surfaced with its own code and each leaving
-- the client that asked with no row of any kind. The counts are the assertion: a client that wrote
-- a purchase row and then raised would produce the same three lines.
--
-- The @code_used@ case needs a SECOND client, not a second command from the first: C4 mints a
-- fresh purchase key per redemption, so the code is presented by a key the service has never seen
-- either way, and a second profile of the same app would prove nothing the second client does not.
testC5RefusedCodesWriteNothing :: HasCallStack => TestParams -> IO ()
testC5RefusedCodesWriteNothing ps = do
  clock <- newTestClock testClockStart
  codesRef <- newIORef []
  let seedCodes = seedTestCodes ps [(BTSupporter, 3, expiredCodeExpiry), (BTSupporter, 3, testCodeExpiry)] >>= writeIORef codesRef
  withBadgeClientCfg ps (readIORef clock) seedCodes (sharedClockCfg clock) $ \alice links -> do
    [expiredCode, usedCode] <- readIORef codesRef
    -- a code the service has never seen
    unknownCode <- mintUnknownCode
    alice ##> purchaseCodeCmd unknownCode
    alice <## "badge service error: code_invalid"
    noClientBadgeRows "after an unknown code" alice
    -- one that alice then redeems for herself
    alice ##> purchaseCodeCmd usedCode
    expectBadgeState alice threeMonthBadgeState
    pk <- readTestIssuerPublicKey (tmpPath ps)
    withNewTestChatCfg ps (sharedClockCfg clock links pk) badgeSecondClientDbPrefix bobProfile $ \bob -> do
      bob ##> purchaseCodeCmd usedCode
      bob <## "badge service error: code_used"
      noClientBadgeRows "after a code redeemed by someone else" bob
      bob ##> purchaseCodeCmd expiredCode
      bob <## "badge service error: code_expired"
      noClientBadgeRows "after an expired code" bob
    -- and alice's single redemption is untouched by either refusal
    clientBadgeRowCounts alice `shouldReturn` (1, 1, 2, 1, 1)

-- | The credential is verified BEFORE the transaction opens, so a credential this client cannot
-- verify leaves nothing behind -- even though the service redeemed the code and will never hand it
-- back. The client is started with an issuer key that is not the service's, which is what a
-- rotated or spoofed issuer looks like from here.
testC5UnverifiableCredentialWritesNothing :: HasCallStack => TestParams -> IO ()
testC5UnverifiableCredentialWritesNothing ps = do
  clock <- newTestClock testClockStart
  codeRef <- newIORef ""
  Right (otherPk, _otherSk) <- bbsKeyGen
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
      cfg (sLink, _fullLink) _servicePk = (badgeClientCfg sLink otherPk) {badgeCurrentTime = readIORef clock}
  withBadgeClientCfg ps (readIORef clock) seedCode cfg $ \alice _links -> do
    code <- readIORef codeRef
    alice ##> purchaseCodeCmd code
    alice <## "badge service error: internal, badge credential does not verify against configured key"
    noClientBadgeRows "after an unverifiable credential" alice
  -- and the service DID redeem it: this is the operator-recoverable case C4's first concern names
  code <- readIORef codeRef
  codeIsRedeemed ps code `shouldReturn` True
  withServiceDB ps $ \db -> serviceRowCounts db `shouldReturn` (1, 1, 2, 1, 1)

-- | With no badge service address configured, both commands that need one fail locally, contact
-- nothing and write nothing. This is the failure C3's worker already reports a pass with (see
-- 'Bots.BadgeManagerTests'), reached here through the two user-facing commands.
testC5NoServiceAddressConfigured :: HasCallStack => TestParams -> IO ()
testC5NoServiceAddressConfigured ps =
  withNewTestChatCfg ps testCfg badgeClientDbPrefix aliceProfile $ \alice -> do
    alice ##> "/_badge catalog 1"
    alice <## "badge service error: internal, badge service address is not configured"
    alice ##> purchaseCodeCmd "C5-NO-ADDRESS"
    alice <## "badge service error: internal, badge service address is not configured"
    reportsNoBadgeEvent alice
    noClientBadgeRows "with no service address" alice

-- | Both store payment methods are reachable through the API today and are permanently refused
-- (plan §6): they present the evidence of a @payments@ row an app purchase flow created, and
-- neither that row nor its writer exists.
testC5StorePaymentsUnsupported :: HasCallStack => TestParams -> IO ()
testC5StorePaymentsUnsupported ps =
  withNewTestChatCfg ps testCfg badgeClientDbPrefix aliceProfile $ \alice -> do
    alice ##> "/_badge purchase 1 {\"type\":\"apple\",\"paymentId\":\"p\",\"jws\":\"j\"}"
    alice <## "badge service error: internal, badge store payments are not supported"
    alice ##> "/_badge purchase 1 {\"type\":\"google\",\"paymentId\":\"p\",\"token\":\"t\"}"
    alice <## "badge service error: internal, badge store payments are not supported"
    reportsNoBadgeEvent alice
    noClientBadgeRows "after store payments" alice

-- | With the service's address published but nothing listening on it, both commands report the
-- transport failure as a badge service error and write nothing.
--
-- The agent's 'serviceRequestTimeout' is shortened to two seconds for this example only: its
-- default is thirty, which is longer than any terminal assertion waits, and what is being pinned
-- is what happens AFTER the timeout, not how long the default is.
testC5ServiceStoppedIsInternal :: HasCallStack => TestParams -> IO ()
testC5ServiceStoppedIsInternal ps =
  -- the two startup phases publish the service's address and then it stays down: 'withService' is
  -- never called
  withBadgeServiceAddress ps getCurrentTime (writeTestBadgeServiceConfig ps) (pure ()) $ \(sLink, _fullLink) _withService -> do
    pk <- readTestIssuerPublicKey (tmpPath ps)
    let cfg = (badgeClientCfg sLink pk) {agentConfig = testAgentCfg {serviceRequestTimeout = 2}}
    withNewTestChatCfg ps cfg badgeClientDbPrefix aliceProfile $ \alice -> do
      alice ##> "/_badge catalog 1"
      alice <##. "badge service error: internal, The badge service could not be reached, and the request was not delivered."
      alice ##> purchaseCodeCmd "C5-SERVICE-DOWN"
      alice <##. "badge service error: internal, The badge service could not be reached, and the request was not delivered."
      noClientBadgeRows "with the service stopped" alice

-- Supersession and the slot -----------------------------------------------------

-- | A second code redeemed on the same profile takes the slot: the first purchase becomes
-- @superseded@ and the second is @issued@ and shown. The first purchase keeps everything it had --
-- its ledger rows, its issuance and, above all, its unconsumed months, which never move between
-- purchases because purchases are unlinkable and the service cannot transfer a balance.
testC5SecondCodeSupersedesFirst :: HasCallStack => TestParams -> IO ()
testC5SecondCodeSupersedesFirst ps = do
  clock <- newTestClock testClockStart
  codesRef <- newIORef []
  let seedCodes = seedTestCodes ps [(BTSupporter, 3, testCodeExpiry), (BTLegend, 3, testCodeExpiry)] >>= writeIORef codesRef
  withBadgeClient ps clock seedCodes $ \alice -> do
    [firstCode, secondCode] <- readIORef codesRef
    alice ##> purchaseCodeCmd firstCode
    expectBadgeState alice threeMonthBadgeState
    firstLedger <- clientLedgerRows alice
    alice ##> purchaseCodeCmd secondCode
    expectBadgeState alice "legend badge 2 (shown): issued, 2 month(s) left, paid through 2026-06-10"
    -- two of everything, and one shown badge
    clientBadgeRowCounts alice `shouldReturn` (2, 2, 4, 2, 1)
    User {userId = aliceUserId} <- clientUser alice
    withClientStore alice $ \db -> do
      statuses <- DB.query_ db "SELECT badge_purchase_id, current_badge_type, status FROM badge_purchases ORDER BY badge_purchase_id"
      ("purchase statuses" :: String, statuses :: [(Int64, Text, Text)])
        `shouldBe` ("purchase statuses", [(1, "supporter", "superseded"), (2, "legend", "issued")])
      [Only shownBadgeId] <- DB.query db "SELECT shown_badge_id FROM users WHERE user_id = ?" (Only aliceUserId)
      ("shown badge" :: String, shownBadgeId :: Maybe Int64) `shouldBe` ("shown badge", Just 2)
      -- the superseded purchase still has its issuance
      [Only firstIssuances] <- DB.query_ db "SELECT count(*) FROM badge_issuances WHERE badge_purchase_id = 1"
      ("superseded purchase issuances" :: String, firstIssuances :: Int) `shouldBe` ("superseded purchase issuances", 1)
      -- and the months it did not spend are still on it
      [Only firstBalance] <- DB.query_ db "SELECT balance_months FROM badge_ledger WHERE badge_purchase_id = 1 ORDER BY entry_id DESC LIMIT 1"
      ("superseded purchase months left" :: String, firstBalance :: Int) `shouldBe` ("superseded purchase months left", 2)
    -- its ledger rows are the rows it had before the second redemption, unchanged and still first
    afterLedger <- clientLedgerRows alice
    ("superseded purchase ledger" :: String, take (length firstLedger) afterLedger)
      `shouldBe` ("superseded purchase ledger", firstLedger)

-- The catalog -------------------------------------------------------------------

-- | The catalog from a profile with no prior connection: the four seeded offers with the service's
-- own totals and both prices, written nowhere.
--
-- It is UNSIGNED, and this is what proves it: the service answers a SIGNED @getBadgeCatalog@ from
-- a key it holds no purchase for with @unknown_purchase_key@
-- ('testBadgeServiceGetCatalogUnknownSignerKey'), and this profile has never bought anything, so a
-- signed request could not have returned a catalog at all.
testC5CatalogFromFreshProfile :: HasCallStack => TestParams -> IO ()
testC5CatalogFromFreshProfile ps = do
  clock <- newTestClock testClockStart
  withBadgeClient ps clock (pure ()) $ \alice -> do
    alice ##> "/_badge catalog 1"
    -- neither catalog query orders its rows, so the six lines are compared as a set
    catalogLines <- replicateM 6 (getTermLine alice)
    sort catalogLines
      `shouldBe` sort
        [ "price 2170da16-66e5-481f-9c75-6949e2dd14e1: supporter 700 usd per month, active",
          "price 6a778279-753e-45db-8acc-0e382c1d054a: legend 7000 usd per month, active",
          "offer 29e35444-2f85-43fb-8933-16875e6d3776: 3 month(s), {\"type\":\"freeMonths\",\"freeMonths\":1}, total 1400, active",
          "offer 71bd8ad1-15c4-4735-b3d4-b12a670dfb7e: 12 month(s), {\"type\":\"freeMonths\",\"freeMonths\":6}, total 4200, active",
          "offer 88885a0a-c407-4aaa-bf53-90f9de7bdfc0: 3 month(s), {\"type\":\"freeMonths\",\"freeMonths\":1}, total 14000, active",
          "offer 35ca9daa-4dca-4cc1-af74-631674906cc9: 12 month(s), {\"type\":\"freeMonths\",\"freeMonths\":6}, total 42000, active"
        ]
    reportsNoBadgeEvent alice
    noClientBadgeRows "after the catalog" alice

-- The worker (C3) against the live service ---------------------------------------

-- | A month on, @\/_badge state@ signals the worker and ONE pass issues the second period against
-- the live service: a second issuance, one more @debit(badge)@ in the ledger, and a
-- 'CEvtBadgeChanged' reporting the new state. No test waits on 'badgePassInterval' -- the shared
-- clock is moved and the command is the trigger.
--
-- It also pins the master key across the two credentials. A pass that regenerated it would be
-- answered with a credential bound to the OLD key on the service's cached-issuance path, which is
-- unusable and raises no error anywhere ('badgeIssueRequest', plan §9); both credentials verify and
-- both carry the key @badge_purchases.master_key@ holds.
testC5WorkerIssuesSecondPeriod :: HasCallStack => TestParams -> IO ()
testC5WorkerIssuesSecondPeriod ps = do
  clock <- newTestClock testClockStart
  codeRef <- newIORef ""
  withBadgeClient ps clock (seedOneSupporterCode ps >>= writeIORef codeRef) $ \alice -> do
    code <- readIORef codeRef
    alice ##> purchaseCodeCmd code
    expectBadgeState alice threeMonthBadgeState
    -- both clocks move together: the month the first period covered has ended
    advanceTestClockMonths clock 1
    alice ##> "/_badge state 1"
    -- the state as it is BEFORE the pass the same command signalled
    expectBadgeState alice threeMonthBadgeState
    -- the pass, issuing [2026-04-10, 2026-05-10): one month spent, still paid through 2026-06-10
    alice <## "supporter badge 1 (shown): issued, 1 month(s) left, paid through 2026-06-10"
    reportsNoBadgeEvent alice
    -- one more issuance and one more ledger entry, and nothing else
    clientBadgeRowCounts alice `shouldReturn` (1, 1, 3, 2, 1)
    withClientStore alice $ \db -> do
      entryTypes <- DB.query_ db "SELECT entry_type, entry_credit_type, entry_debit_type FROM badge_ledger ORDER BY entry_id"
      ("ledger" :: String, entryTypes :: [(Text, Maybe Text, Maybe Text)])
        `shouldBe` ( "ledger",
                     [ ("credit", Just "payment", Nothing),
                       ("debit", Nothing, Just "badge"),
                       ("debit", Nothing, Just "badge")
                     ]
                   )
      periods <- DB.query_ db "SELECT period_start, period_end FROM badge_issuances ORDER BY period_start"
      case periods :: [(UTCTime, UTCTime)] of
        [(firstStart, firstEnd), (secondStart, secondEnd)] -> do
          firstStart `shouldBeStoredAt` testClockStart
          firstEnd `shouldBeStoredAt` firstPeriodEnd
          secondStart `shouldBeStoredAt` firstPeriodEnd
          secondEnd `shouldBeStoredAt` UTCTime (fromGregorian 2026 5 10) noonUTC
        other -> expectationFailure $ "expected two issuances, got: " <> show other
    -- the master key is the purchase's on both credentials, and both verify
    issuerPub <- readTestIssuerPublicKey (tmpPath ps)
    purchaseMasterKey <- clientPurchaseMasterKey alice
    creds <- clientIssuedCredentials alice
    ("issued credentials" :: String, length creds) `shouldBe` ("issued credentials", 2)
    forM_ (zip [1 :: Int ..] creds) $ \(n, cred) -> do
      verifyCredential issuerPub cred `shouldReturn` True
      ("credential " <> show n <> " master key", credentialMasterKey cred)
        `shouldBe` ("credential " <> show n <> " master key", purchaseMasterKey)
    -- and the second credential is not the first one served again
    case creds of
      [firstCred, secondCred] ->
        ("second period credential" :: String, secondCred == firstCred) `shouldBe` ("second period credential", False)
      _ -> pure ()

-- The active profile --------------------------------------------------------------

-- | A redemption that FINISHES after the active profile has been switched must not point the app
-- back at the profile it was redeemed for.
--
-- 'withUserId' checks activeness at ENTRY and the round trip that follows is seconds long, so the
-- guard in 'redeemBadgeCode' is load-bearing and needs the racing case, not the static one. The
-- client's 'badgeCurrentTime' is read after the request has been answered and before the first row
-- is written, which is exactly the window that guard covers, so gating that clock (C3's
-- 'BadgeGate') parks the redemption there while the profile is switched.
--
-- The redemption runs off the terminal's input loop, which is single-threaded: a @\/create user@
-- typed at it would queue BEHIND the parked redemption and could never race it.
testC5RedeemAfterProfileSwitchKeepsActiveUser :: HasCallStack => TestParams -> IO ()
testC5RedeemAfterProfileSwitchKeepsActiveUser ps = do
  clock <- newTestClock testClockStart
  gate <- newBadgeGate
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
      cfg (sLink, _fullLink) pk = (badgeClientCfg sLink pk) {badgeCurrentTime = gatedClockAt testClockStart gate}
  withBadgeClientCfg ps (readIORef clock) seedCode cfg $ \alice _links -> do
    -- the chat-start pass reads the gated clock first; let it through so it cannot take the permit
    -- the redemption needs. It finds no purchase and writes nothing.
    allowPass gate
    waitPasses gate 1
    User {userId = aliceUserId} <- clientUser alice
    code <- readIORef codeRef
    redeeming <-
      async $ runReaderT (execChatCommand' (APIPurchaseBadge aliceUserId (BPPCode code)) 0) (chatController alice)
    -- the request has been sent and answered, and the redemption is parked on its clock read
    waitPasses gate 2
    alice ##> "/create user secret"
    alice <## "user profile: secret"
    alice <## "use /p <name> [<bio>] to change it"
    User {userId = secretUserId} <- clientUser alice
    ("switched profile" :: String, secretUserId == aliceUserId) `shouldBe` ("switched profile", False)
    -- released, the redemption writes its rows and answers
    allowPass gate
    wait redeeming >>= \case
      Right _ -> pure ()
      Left e -> expectationFailure $ "the redemption failed: " <> show e
    -- the badge landed on the redeeming profile ...
    withClientStore alice $ \db -> do
      [Only purchaseOwner] <- DB.query_ db "SELECT user_id FROM badge_purchases"
      ("purchase owner" :: String, purchaseOwner :: Int64) `shouldBe` ("purchase owner", aliceUserId)
      [Only shownBadgeId] <- DB.query db "SELECT shown_badge_id FROM users WHERE user_id = ?" (Only aliceUserId)
      ("redeeming profile shown badge" :: String, shownBadgeId :: Maybe Int64)
        `shouldBe` ("redeeming profile shown badge", Just 1)
    -- ... and the active profile did not move back to it
    User {userId = activeAfter} <- clientUser alice
    ("active profile" :: String, activeAfter) `shouldBe` ("active profile", secretUserId)

-- Idempotency and recovery ---------------------------------------------------------

-- | The same code presented again from the same profile mints a FRESH purchase key, so the RPC's
-- "repeat the identical request" idempotency is unreachable from the client by design: the service
-- sees a second key and answers @code_used@. The client writes nothing and the first badge stays
-- shown.
--
-- After the operator clears the redemption, the same code redeems again into a NEW purchase and
-- supersedes the old one -- the recovery path C4's "a response lost in flight consumes the code"
-- depends on. H2's @codes unredeem@ CLI does not exist yet (@BadgeService.Admin@ has issue, revoke
-- and status only), so this drives B1's 'unredeemCode' directly, which is the function that CLI
-- will call.
testC5SameCodeAgainThenUnredeemed :: HasCallStack => TestParams -> IO ()
testC5SameCodeAgainThenUnredeemed ps = do
  clock <- newTestClock testClockStart
  codeRef <- newIORef ""
  withBadgeServiceAddress ps (readIORef clock) (writeTestBadgeServiceConfig ps) (seedOneSupporterCode ps >>= writeIORef codeRef) $
    \links withService -> do
      pk <- readTestIssuerPublicKey (tmpPath ps)
      -- the client is started OUTSIDE the service phases, so it outlives the restart the unredeem
      -- needs: the service's database cannot be opened while the service holds it
      withNewTestChatCfg ps (sharedClockCfg clock links pk) badgeClientDbPrefix aliceProfile $ \alice -> do
        code <- readIORef codeRef
        withService $ do
          alice ##> purchaseCodeCmd code
          expectBadgeState alice threeMonthBadgeState
          -- the same code, the same profile, a new key
          alice ##> purchaseCodeCmd code
          alice <## "badge service error: code_used"
          clientBadgeRowCounts alice `shouldReturn` (1, 1, 2, 1, 1)
          alice ##> "/_badge state 1"
          expectBadgeState alice threeMonthBadgeState
        -- the operator clears the redemption while the service is stopped
        withServiceStore ps $ \db -> unredeemCode db (Codes.codeHash (Codes.normalizeCode code)) testClockStart
        withService $ do
          alice ##> purchaseCodeCmd code
          expectBadgeState alice "supporter badge 2 (shown): issued, 2 month(s) left, paid through 2026-06-10"
          clientBadgeRowCounts alice `shouldReturn` (2, 2, 4, 2, 1)
          withClientStore alice $ \db -> do
            statuses <- DB.query_ db "SELECT badge_purchase_id, status FROM badge_purchases ORDER BY badge_purchase_id"
            ("purchase statuses" :: String, statuses :: [(Int64, Text)])
              `shouldBe` ("purchase statuses", [(1, "superseded"), (2, "issued")])

-- Address resolution -----------------------------------------------------------------

-- | Whether a parsed badge service address is the FULL contact request URI form rather than the
-- short link every other example configures. Top level, and not a @case@ in the example itself,
-- because 'ConnectTarget' is a GADT and matching one in a local binding is what
-- @-Wgadt-mono-local-binds@ warns about.
isFullContactTarget :: ConnectTarget 'CMContact -> Bool
isFullContactTarget target = case target of
  CTFullContact _ -> True
  _ -> False

-- | The service's address given as a full contact request URI redeems exactly as the short link
-- does. 'resolveServiceTarget' accepts four target forms and, without this, only the short link
-- would ever be exercised. The 'CTName' and 'CTDomain' forms need a resolvable SimpleX name, which
-- this harness cannot publish; they are recorded as untested in the plan's §9.
testC5FullContactUriRedeems :: HasCallStack => TestParams -> IO ()
testC5FullContactUriRedeems ps = do
  clock <- newTestClock testClockStart
  codeRef <- newIORef ""
  let seedCode = seedOneSupporterCode ps >>= writeIORef codeRef
      cfg (_sLink, fullLink) pk = (badgeClientCfg fullLink pk) {badgeCurrentTime = readIORef clock}
  withBadgeClientCfg ps (readIORef clock) seedCode cfg $ \alice (_sLink, fullLink) -> do
    -- the configured target really is the FULL form: a full URI that had silently decoded as a
    -- short link would make this the short-link example over again
    ("full contact URI target" :: String, isFullContactTarget (badgeServiceTarget fullLink))
      `shouldBe` ("full contact URI target", True)
    code <- readIORef codeRef
    alice ##> purchaseCodeCmd code
    expectBadgeState alice threeMonthBadgeState
    clientBadgeRowCounts alice `shouldReturn` (1, 1, 2, 1, 1)

-- The stub responder --------------------------------------------------------------------

-- | A stub badge service: a chat client accepting service requests on a contact address of its
-- own, which the test answers with a canned body.
--
-- Six of C5's cases need a response the real service cannot produce -- an absent credential, a
-- statement carrying no issued period, a credential with no expiry, a statement carrying an entry
-- type this build cannot store, a response of the wrong shape for the command, and a body that is
-- not a 'BadgeServiceResponse' at all. Each is a branch of C4's 'redeemBadgeCode' whose whole
-- purpose is to write nothing, and without a responder that can be made to lie all six would be
-- untested.
--
-- The credential the stub signs is real -- 'bbsKeyGen' here and the public half in the client's
-- 'badgePublicKeys' -- so 'verifyUserBadge' passes and the branch under test is the one that
-- fails.
withBadgeStub :: HasCallStack => TestParams -> (TestCC -> TestCC -> BBSSecretKey -> IO ()) -> IO ()
withBadgeStub ps test = do
  Right (pk, sk) <- bbsKeyGen
  withNewTestChatCfgOpts_ ps testCfg testOpts badgeStubDbPrefix True badgeStubProfile $ \stub -> do
    -- SREQ delivery gates on this flag, exactly as 'badgePostStartHook' sets it
    atomically $ writeTVar (processServiceRequests (chatController stub)) True
    -- a DR address: service RPC requires one, which is why the real service creates it with
    -- pqRatchet 'Just True'
    stub ##> "/ad pq_ratchet=on"
    (sLink, _fullLink) <- getContactLinks stub True
    withNewTestChatCfg ps (badgeClientCfg sLink pk) badgeClientDbPrefix aliceProfile $ \alice ->
      test alice stub sk

badgeStubProfile :: Profile
badgeStubProfile = badgeProfile {displayName = "SimpleX Badge Stub"}

-- | Answers the one service request the stub is holding with 'body'. What the request carried is
-- B5's business, not C5's, so it is read and dropped.
answerStubRequest :: HasCallStack => TestCC -> J.Value -> IO ()
answerStubRequest stub body = do
  reqLine <- getTermLine' (Just "service request") stub
  reqId <- case stripPrefix "service request " reqLine of
    Just i -> pure i
    Nothing -> expectationFailure ("expected a service request line, got: " <> reqLine) >> error "unreachable"
  -- 'signed by' precedes the request for a signed purchaseBadge and is absent for the unsigned
  -- getBadgeCatalog
  next <- getTermLine' (Just "signed by, or the request") stub
  unless ("request: " `isPrefixOf` next) $ stub <##. "request: "
  stub ##> ("/_service_response 1 " <> reqId <> " " <> LBC.unpack (J.encode body))
  -- the command's own answer and the agent's SSENT event for the reply, in either order
  forM_ [1 :: Int, 2] $ \_ -> do
    l <- getTermLine' (Just "service reply") stub
    unless ("service reply " `isPrefixOf` l) $
      expectationFailure ("expected a service reply line, got: " <> l)

-- 'StatementEntry' is constructed positionally, as everywhere else in this module (see the import
-- list's Haddock): entryId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType,
-- wasPausedSince, createdAt, entryType.
stubEntry :: Text -> Int -> Int -> UTCTime -> StatementEntryType -> StatementEntry
stubEntry entryId changeMonths balanceMonths balanceStartTs =
  StatementEntry entryId changeMonths balanceMonths balanceStartTs BTSupporter Nothing testClockStart

-- | The statement a three-month code redemption produces: the credit, then the @debit(badge)@ that
-- carries the issued period.
stubRedeemStatement :: BadgeStatement
stubRedeemStatement =
  BadgeStatement
    { entries =
        [ stubEntry "c5-stub-credit" 3 3 testClockStart (SECredit (SCPayment Nothing)),
          stubEntry "c5-stub-debit" (-1) 2 firstPeriodEnd (SEDebit SDBadge)
        ],
      previousEntryId = Nothing
    }

stubCredential :: HasCallStack => BBSSecretKey -> Maybe UTCTime -> IO BadgeCredential
stubCredential sk badgeExpiry = do
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let req = BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry, badgeExtra = ""}}
  issueBadge 1 sk (VerifiedBadgeRequest req)
    >>= either (\e -> expectationFailure ("the stub could not sign a credential: " <> e) >> error "unreachable") pure

stubCredentialResponse :: Maybe BadgeCredential -> BadgeStatement -> J.Value
stubCredentialResponse credential statement =
  J.toJSON BSPBadgeCredential {credential, receipt = Nothing, statement}

-- | Every response a redemption can be refused for once it has arrived, each answered with the
-- message C4 raises and each leaving the client with no row of any kind.
--
-- The order is the order 'redeemBadgeCode' checks them in, and the last is the reason
-- 'checkStatementEntries' exists at all: an unstorable entry type reaching 'insertLedgerEntries'
-- would COMMIT the payment and purchase rows written above it, because a 'Left' inside @withStore@
-- does not roll the transaction back.
testC5UnrecordableResponsesWriteNothing :: HasCallStack => TestParams -> IO ()
testC5UnrecordableResponsesWriteNothing ps =
  withBadgeStub ps $ \alice stub sk -> do
    let redeemAnsweredWith label body message = do
          alice ##> purchaseCodeCmd "C5-STUB-CODE"
          answerStubRequest stub body
          alice <##. message
          noClientBadgeRows label alice
    -- no credential at all: the exhausted balance of an issueBadge, which a redemption cannot be
    redeemAnsweredWith
      "after a redemption with no credential"
      (stubCredentialResponse Nothing stubRedeemStatement)
      "badge service error: internal, badge service redeemed the code without issuing a credential"
    -- a statement that issued no period, so nothing says which month was bought
    cred <- stubCredential sk (Just firstCredentialExpiry)
    let creditOnly =
          BadgeStatement
            { entries = [stubEntry "c5-stub-credit-only" 3 3 testClockStart (SECredit (SCPayment Nothing))],
              previousEntryId = Nothing
            }
    redeemAnsweredWith
      "after a statement with no issued period"
      (stubCredentialResponse (Just cred) creditOnly)
      "badge service error: internal, badge service issued no period for the redeemed code"
    -- a credential with no expiry, which 'issueSignedBadge' never signs
    noExpiryCred <- stubCredential sk Nothing
    redeemAnsweredWith
      "after a credential with no expiry"
      (stubCredentialResponse (Just noExpiryCred) stubRedeemStatement)
      "badge service error: internal, issued badge credential carries no expiry"
    -- an entry type this build cannot store, refused BEFORE the transaction opens
    let unstorable =
          BadgeStatement
            { entries =
                [ stubEntry "c5-stub-charge" 3 3 testClockStart (SECredit (SCCharge "c5-charge")),
                  stubEntry "c5-stub-debit" (-1) 2 firstPeriodEnd (SEDebit SDBadge)
                ],
              previousEntryId = Nothing
            }
    redeemAnsweredWith
      "after an unstorable statement"
      (stubCredentialResponse (Just cred) unstorable)
      "badge service error: internal, badge service statement cannot be stored: "

-- | A response of the wrong shape for the command it answers, and one that is not a
-- 'BadgeServiceResponse' at all, on both commands that send one.
--
-- Neither is a silent no-op: a service speaking a protocol this build does not know is reported,
-- with the response in the message for the first and the app-version sentence for the second.
testC5UnreadableResponsesAreReported :: HasCallStack => TestParams -> IO ()
testC5UnreadableResponsesAreReported ps =
  withBadgeStub ps $ \alice stub sk -> do
    cred <- stubCredential sk (Just firstCredentialExpiry)
    let emptyCatalog = J.toJSON BSPBadgeCatalog {catalog = BadgeCatalog {prices = [], offers = []}, badgeStatement = Nothing}
        credentialBody = stubCredentialResponse (Just cred) stubRedeemStatement
        undecodable = J.object ["type" J..= ("badgeSomethingElse" :: Text)]
    -- a catalog answering a purchase
    alice ##> purchaseCodeCmd "C5-STUB-CODE"
    answerStubRequest stub emptyCatalog
    alice <##. "badge service error: internal, unexpected badge service response to purchaseBadge: "
    noClientBadgeRows "after a catalog answering a purchase" alice
    -- a credential answering a catalog
    alice ##> "/_badge catalog 1"
    answerStubRequest stub credentialBody
    alice <##. "badge service error: internal, unexpected badge service response to getBadgeCatalog: "
    -- a body that does not decode as a response at all, on both commands
    alice ##> purchaseCodeCmd "C5-STUB-CODE"
    answerStubRequest stub undecodable
    alice <##. "badge service error: internal, The badge service answered with something this app version cannot read."
    alice ##> "/_badge catalog 1"
    answerStubRequest stub undecodable
    alice <##. "badge service error: internal, The badge service answered with something this app version cannot read."
    noClientBadgeRows "after unreadable responses" alice
