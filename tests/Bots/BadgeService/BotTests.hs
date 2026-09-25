{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Bots.BadgeService.BotTests where

import BadgeService.Config (BadgeIssuerKey (..), readServiceConfig)
import Bots.BadgeService.ConfigTests (withIssuer)
import Bots.BadgeService.FakeStore
import BadgeService.Options
import BadgeService.Service
import BadgeService.Store (NewStorePurchase (..), createStorePurchase)
import BadgeService.Store.Invoices (markCodePaid)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, readTMVar)
import Control.Monad (forM_, void, when)
import Control.Exception (finally)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (toLower)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import System.Timeout (timeout)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word32)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeMasterKey, BadgeType (..), generateMasterKey)
import Simplex.Chat.Badges.Code (BadgeCode, badgeCodeHash, badgeCodeText, formatBadgeCode, parseBadgeCode, randomBadgeCode)
import Simplex.Chat.Badges.Ledger (addMonths, creditTypeTag, debitTypeTag, emptyEntry, endOfMondayAfter)
import Simplex.Chat.Badges.Service
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatResponse (CRCustomChatResponse))
import Simplex.Chat.Core (sendChatCmdStr)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.PaymentService (ServicePayment (..))
import Simplex.Chat.PaymentService.Types (InvoiceId (..), PaymentProvider (..))
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..))
import Simplex.Messaging.Agent.RetryInterval (RetryInterval (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..))
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSSecretKey, bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode, strEncode, textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import System.FilePath ((</>))
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif
import Test.Hspec hiding (it)

badgeServiceTests :: SpecWith TestParams
badgeServiceTests = do
  it "should answer unsupported_version to unsupported command" testBadgeServiceUnsupported
  it "should redeem an issued code into a badge a contact sees" testRedeemBadgeCode
  it "should return the same badge when the same code is redeemed twice" testRedeemBadgeCodeTwice
  it "should answer code_invalid to an unknown code, indistinguishably from a malformed one" testRedeemUnknownCode
  it "should tell a second profile redeeming the same code that it is used" testRedeemSameCodeOtherProfile
  it "should refuse a second code while a badge is held, leaving it unspent" testRedeemSecondCode
  it "should refuse a code that has not been paid for" testRedeemUnpaidCode
  it "should refuse a badge code past its redemption deadline" testExpiredCode
  it "should keep answering a code redeemed before its deadline" testRedeemedBeforeTheDeadline
  it "should refuse a revoked badge code, and refuse to revoke it twice" testRevokedCode
  it "should refuse to revoke a code that was redeemed, and keep its badge" testRevokeRedeemedCode
  it "should answer revoking an unknown code as no such code" testRevokeUnknownCode
  it "should refuse to issue a code with an unknown badge type or a nonsense month count" testIssueRejectsBadArguments
  it "should refuse a request whose purchaseKey is not the verified signer" testPurchaseKeyMismatch
  it "should refuse to start unless the issuer secret is the key trusted at its index" testIssuerKeyMustMatchConfig
  it "should refuse to start when the [issuer] key is not one clients trust" testIssuerIniKeyMustBeTrusted
  it "should credit a code's months and issue one credential per month" testCodeMonthsRenew
  it "should return the stored credential for a repeat inside an issued period" testRepeatInsideIssuedPeriod
  it "should lapse only the months that elapsed while the client was away" testLapseWhileAway
  it "should round the last month's expiry up to the end of the Monday after it" testLastMonthExpiryRounds
  it "should sign a renewal with the master key stored on the purchase" testRenewalSignsWithStoredMasterKey
  it "should leave the client holding the same ledger rows as the service" testClientReplicatesLedger
  it "should renew a badge whose credential is lapsing, with no command" testWorkerRenews
  it "should request from the wake it set a day before the credential lapses" testRequestWakeFires
  it "should present from the wake it set at the credential's expiry" testPresentWakeFires
  it "should renew a badge whose newest ledger row is of an unknown type" testRenewsAfterUnknownEntry
  it "should catch up the months that lapsed while the client was stopped" testRenewsAfterRestart
  it "should stop showing a badge whose balance ran out, and tell contacts" testWorkerRetiresExpired
  it "should retire when entitlement ends, not when the credential expires" testRetiresWhenEntitlementEnds
  it "should alert that support ended, survive a restart, and go silent once acknowledged" testEndedAlert
  it "should raise a snoozed alert once more when the snooze lapses" testSnoozedAlertReturns
  it "should alert that renewal failed when the service refuses it" testIssueFailedAlert
  it "should wait for the shown credential to lapse before alerting on a failure that can clear" testIssueFailedWaitsForExpiry
  it "should silence an acknowledged run of failures, and alert again on the next run" testIssueFailedAckAndNewRun
  it "should record no failure when the service issues nothing because the months ran out" testNoCredentialMonthsRanOut
  it "should record a failure when the service issues nothing though months are left" testNoCredentialMonthsLeft
  it "should renew a badge on a profile that is not active, without switching to it" testRenewalKeepsActiveProfile
  it "should broadcast the current profile when a renewal presents a badge" testRenewalKeepsProfileEdits
  it "should present the month already issued when a previous pass did not" testPresentationCatchesUp
  describe "store purchases" $ do
    it "should credit the months a store receipt paid for and issue the first" testStorePurchase
    it "should return the same credential for a receipt its key presents again, writing nothing" testStorePurchaseReplay
    it "should refuse a receipt another key was credited with" testStoreReceiptUsed
    it "should refuse a receipt the store does not vouch for, with no retry" testStoreReceiptInvalid
    it "should answer a store it cannot reach as retryable, writing nothing" testStoreUnreachable
    it "should write nothing for a pending purchase, and credit it once when it settles" testStorePending
    it "should refuse a product that grants no badge" testStoreUnknownProduct
    it "should refuse a store purchase that carries an upgrade" testStoreUpgradeRefused
    it "should grant the badge of the product the receipt proves" testStoreBadgeTypeFromProduct
    it "should keep refusing invoice and receipt funding" testNonStoreFundingRefused
    it "should replay a receipt to its own key while the store is down, and to no other" testStoreReplayWhileStoreDown
    it "should answer a throwing Apple verifier as internal, and a failing or hanging Google one as retryable" testStoreVerifierFailures
    it "should credit a transaction claimed twice at once only once" testStoreClaimRace
    it "should refuse a store purchase whose purchaseKey is not the verified signer" testStorePurchaseKeyMismatch
    it "should redeem a Play purchase into a badge, and replay it as the same badge" testPurchaseBadge
    it "should redeem an App Store purchase by its JWS" testPurchaseBadgeAppStore
    it "should drop the keys of a receipt refused for good, and keep them while it is pending" testPurchaseStash
    it "should refuse a store purchase while a badge is held, before anything is sent" testPurchaseWhileBadgeHeld
    it "should answer a receipt presented under a second profile as the profile that bought it" testPurchaseSameReceiptOtherProfile
    it "should deliver a purchase first presented under another profile to that profile" testPurchaseStrandedUnderOtherProfile

badgeProfile :: Profile
badgeProfile = Profile {displayName = "SimpleX Badges", fullName = "", shortDescr = Nothing, description = Nothing, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences = Nothing, badge = Nothing, contactDomain = Nothing}

serviceDbPrefix :: FilePath
serviceDbPrefix = "badge_service"

testIssuerKeyIdx :: Int
testIssuerKeyIdx = 1

mkBadgeServiceOpts :: TestParams -> BBSSecretKey -> BadgeServiceOpts
mkBadgeServiceOpts TestParams {tmpPath = ps} secretKey =
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
      serviceConfigFile = Nothing,
      issuerKey = Right (Just BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey}),
      testing = True
    }

-- | The clock tracks real time plus a test-controlled offset rather than freezing it, so a sleeping worker still waits the correct real duration.
newtype TestClock = TestClock (IORef NominalDiffTime)

newTestClock :: IO TestClock
newTestClock = TestClock <$> newIORef 0

testClockTime :: TestClock -> IO UTCTime
testClockTime (TestClock r) = do
  offset <- readIORef r
  addUTCTime offset <$> getCurrentTime

setClockAt :: TestClock -> UTCTime -> IO ()
setClockAt (TestClock r) t = getCurrentTime >>= \real -> writeIORef r (diffUTCTime t real)

data BadgeServiceEnv = BadgeServiceEnv
  { bsIssuerKey :: BadgeIssuerKey,
    bsClock :: TestClock,
    bsClientCfg :: ChatConfig,
    bsAddress :: String,
    bsController :: ChatController,
    bsStore :: FakeStore
  }

-- | Stop the service for good: requests sent after it go unanswered until they time out. Stopping
-- chat unsubscribes its queues, where killing the thread could still let a request arrive mid-teardown.
stopBadgeService :: ChatController -> IO ()
stopBadgeService cc = void $ sendChatCmdStr cc "/_stop"

withBadgeService :: HasCallStack => TestParams -> (ChatConfig -> String -> ChatController -> IO ()) -> IO ()
withBadgeService ps test =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsAddress, bsController} -> test bsClientCfg bsAddress bsController

withBadgeServiceEnv :: HasCallStack => TestParams -> (BadgeServiceEnv -> IO ()) -> IO ()
withBadgeServiceEnv ps test = do
  Right (pk, sk) <- bbsKeyGen
  clock <- newTestClock
  store <- newFakeStore
  let opts = mkBadgeServiceOpts ps sk
      svcCfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk, badgeCurrentTime = testClockTime clock}
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  runBadgeService store svcCfg opts $ \_ -> pure ()
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (sLink, _) <- getContactLinks bs False
    bs <## "auto_accept off"
    pure sLink
  let clientCfg =
        svcCfg {badgeServiceAddress = Just $ either (error . ("bad badge service address: " <>)) id $ strDecode (B.pack bsLink)}
  runBadgeService store svcCfg opts $ \env -> do
    cc <- atomically $ readTMVar $ serviceCC env
    test BadgeServiceEnv {bsIssuerKey = BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey = sk}, bsClock = clock, bsClientCfg = clientCfg, bsAddress = bsLink, bsController = cc, bsStore = store}

issueCode :: HasCallStack => ChatController -> BadgeType -> Int -> IO BadgeCode
issueCode cc badgeType months = issueCodeAs cc badgeType months "free"

revokeCodeAs :: HasCallStack => ChatController -> BadgeCode -> IO T.Text
revokeCodeAs cc code =
  sendChatCmdStr cc ("//revoke " <> T.unpack (formatBadgeCode code)) >>= \case
    Right (CRCustomChatResponse _ response) -> pure response
    Left e -> pure (T.pack (show e))
    r -> error $ "revoke failed: " <> show (() <$ r)

issueCodeAs :: HasCallStack => ChatController -> BadgeType -> Int -> String -> IO BadgeCode
issueCodeAs cc badgeType months status =
  sendChatCmdStr cc ("//issue " <> T.unpack (textEncode badgeType) <> " " <> show months <> " " <> status) >>= \case
    Right (CRCustomChatResponse _ response) -> case T.stripPrefix "code " response of
      Just c | Just code <- parseBadgeCode c -> pure code
      _ -> error $ "unexpected issue response: " <> T.unpack response
    r -> error $ "issue failed: " <> show (() <$ r)

-- | The post-start hook fills serviceCC once the address exists, so the test waits on it rather than on a fixed delay that would race with startup and let one start's address output arrive during the next test.
runBadgeService :: FakeStore -> ChatConfig -> BadgeServiceOpts -> (ServiceState -> IO ()) -> IO ()
runBadgeService FakeStore {fakeVerifier} cfg opts action = do
  env <- (\s -> s {storeVerifier = fakeVerifier}) <$> newServiceState
  t <- forkIO $ badgeService opts cfg env
  ready <- timeout 30000000 $ atomically $ readTMVar $ serviceCC env
  when (isNothing ready) $ killThread t >> error "badge service did not start"
  action env `finally` killThread t

codeArg :: BadgeCode -> String
codeArg = T.unpack . formatBadgeCode

testBadgeServiceUnsupported :: HasCallStack => TestParams -> IO ()
testBadgeServiceUnsupported ps =
  withBadgeService ps $ \clientCfg bsLink _ ->
    withNewTestChatCfg ps clientCfg "client" bobProfile $ \client -> do
      let req = "{\"version\":1,\"request\":{\"type\":\"pauseBadge\"}}"
      client ##> ("/_service_request 1 " <> bsLink <> " " <> req)
      client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"

testRedeemBadgeCode :: HasCallStack => TestParams -> IO ()
testRedeemBadgeCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps clientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 1
        alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
        alice <## "badge redeemed"
        alice <## "supporter badge - active"
        alice <##. "expires "
        alice ##> "/p"
        alice <## "user profile: alice (Alice, * supporter)"
        alice <## "use /p <name> [<bio>] to change it"
        alice #> "@bob hi"
        bob <# "alice *> hi"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "supporter badge - active"
        bob <##. "expires "
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo

testRedeemBadgeCodeTwice :: HasCallStack => TestParams -> IO ()
testRedeemBadgeCodeTwice ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      alice ##> ("/_redeem_badge_code 1 " <> map toLower (T.unpack $ badgeCodeText code))
      alice <## "badge already redeemed"
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"

testRedeemUnknownCode :: HasCallStack => TestParams -> IO ()
testRedeemUnknownCode ps =
  withBadgeService ps $ \clientCfg bsLink _ ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      g <- C.newRandom
      unknown <- randomBadgeCode g
      alice ##> ("/_redeem_badge_code 1 " <> codeArg unknown)
      alice <## "cannot redeem badge code: badge service error: code_invalid"
      -- a failed check character is refused before anything leaves the device
      alice ##> "/_redeem_badge_code 1 SB-00000-00000-00000-00001"
      alice <## "cannot redeem badge code: invalid code"
      -- sent straight to the service, past the client's own check, the two are one answer
      (_, redeemPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      redeemDirect alice bsLink redeemPriv (T.unpack $ badgeCodeText unknown)
      alice <## "service response: {\"code\":\"code_invalid\",\"type\":\"error\"}"
      redeemDirect alice bsLink redeemPriv "SB-00000-00000-00000-00001"
      alice <## "service response: {\"code\":\"code_invalid\",\"type\":\"error\"}"

redeemDirect :: HasCallStack => TestCC -> String -> C.PrivateKeyEd25519 -> String -> IO ()
redeemDirect cc bsLink signPriv code = do
  let purchaseKey = B.unpack $ strEncode $ C.publicKey signPriv
      signKey = B.unpack $ strEncode (C.StoredPrivateKey signPriv)
      req =
        "{\"version\":1,\"purchaseKey\":\"" <> purchaseKey
          <> "\",\"request\":{\"type\":\"redeemBadgeCode\",\"masterKey\":\"" <> testMasterKeyB64
          <> "\",\"code\":\"" <> code <> "\"}}"
  cc ##> ("/_service_request 1 " <> bsLink <> " sign_key=" <> signKey <> " " <> req)

-- These requests never reach signing, so the master key can be any 32 bytes.
testMasterKeyB64 :: String
testMasterKeyB64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

testIssueRejectsBadArguments :: HasCallStack => TestParams -> IO ()
testIssueRejectsBadArguments ps =
  withBadgeService ps $ \_ _ cc -> do
    let refuses arg = issueRaw cc arg >>= (`shouldSatisfy` isLeft)
    refuses "suporter"
    refuses "supporter 0"
    refuses "supporter 256"
    refuses "supporter 18446744073709551617"
    refuses "supporter 1 gratis"
    refuses ""
    issueRaw cc "supporter 255 paid" >>= (`shouldSatisfy` isRight)

issueRaw :: ChatController -> String -> IO (Either () ())
issueRaw cc args =
  sendChatCmdStr cc ("//issue " <> args) >>= \case
    Right CRCustomChatResponse {} -> pure $ Right ()
    _ -> pure $ Left ()

testRedeemSecondCode :: HasCallStack => TestParams -> IO ()
testRedeemSecondCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      supporter <- issueCode cc BTSupporter 1
      legend <- issueCode cc BTLegend 1
      alice ##> ("/_redeem_badge_code 1 " <> codeArg supporter)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      alice ##> ("/_redeem_badge_code 1 " <> codeArg legend)
      alice <## "cannot redeem badge code: badge already active"
      alice ##> "/p"
      showActiveUser alice "alice (Alice, * supporter)"
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> ("/_redeem_badge_code 2 " <> codeArg legend)
      alice <## "badge redeemed"
      alice <## "legend badge - active"
      alice <##. "expires "

testRedeemSameCodeOtherProfile :: HasCallStack => TestParams -> IO ()
testRedeemSameCodeOtherProfile ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> ("/_redeem_badge_code 2 " <> codeArg code)
      alice <## "cannot redeem badge code: badge service error: code_used"
      alice ##> "/p"
      showActiveUser alice "alisa"
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice, * supporter)"

serviceCmd :: HasCallStack => BadgeServiceEnv -> C.PublicKeyEd25519 -> BadgeServiceCommand -> IO BadgeServiceResponse
serviceCmd BadgeServiceEnv {bsIssuerKey, bsController, bsStore = FakeStore {fakeVerifier}} purchaseKey request =
  badgeServiceResponse bsIssuerKey fakeVerifier bsController (Just purchaseKey) reqObject
  where
    reqObject = case J.toJSON BadgeServiceRequest {version = currentBadgeServiceVersion, purchaseKey = Just purchaseKey, request} of
      J.Object o -> o
      _ -> error "badge service request must encode as an object"

entryOf :: StatementEntry -> (Int, Int, UTCTime)
entryOf StatementEntry {changeMonths, balanceMonths, balanceStartTs} = (changeMonths, balanceMonths, balanceStartTs)

anchorOf :: StatementEntry -> UTCTime
anchorOf StatementEntry {balanceAnchorTs} = balanceAnchorTs

entryTag :: StatementEntry -> Text
entryTag StatementEntry {entryType} = case entryType of
  SECredit c -> creditTypeTag c
  SEDebit d -> debitTypeTag d

statementOf :: HasCallStack => BadgeServiceResponse -> ([StatementEntry], Maybe Text)
statementOf = \case
  BSPBadgeCredential {statement = BadgeStatement {entries, previousEntryId}} -> (entries, previousEntryId)
  r -> error $ "expected badgeCredential, got " <> show (J.toJSON r)

credentialOf :: HasCallStack => BadgeServiceResponse -> Maybe BadgeCredential
credentialOf = \case
  BSPBadgeCredential {credential} -> credential
  r -> error $ "expected badgeCredential, got " <> show (J.toJSON r)

nextDue :: [StatementEntry] -> UTCTime
nextDue entries = let (_, _, start) = entryOf (last entries) in start

newPurchaseKeys :: IO (C.PublicKeyEd25519, BadgeMasterKey)
newPurchaseKeys = do
  g <- C.newRandom
  (purchaseKey, _) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
  (purchaseKey,) <$> generateMasterKey g

assertBalance :: HasCallStack => BadgeServiceEnv -> C.PublicKeyEd25519 -> StatementEntry -> IO BadgeServiceResponse
assertBalance env purchaseKey lastEntry =
  serviceCmd env purchaseKey BSCIssueBadge {balance = BadgeBalance {lastEntry}}

expiryOf :: HasCallStack => BadgeServiceResponse -> Maybe UTCTime
expiryOf r = (\(BadgeCredential _ _ _ BadgeInfo {badgeExpiry}) -> badgeExpiry) <$> credentialOf r

masterKeyOf :: HasCallStack => BadgeServiceResponse -> Maybe BadgeMasterKey
masterKeyOf r = (\(BadgeCredential _ mk _ _) -> mk) <$> credentialOf r

testCodeMonthsRenew :: HasCallStack => TestParams -> IO ()
testCodeMonthsRenew ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 3
    (purchaseKey, masterKey) <- newPurchaseKeys
    redeemed <- serviceCmd env purchaseKey BSCRedeemBadgeCode {masterKey, code = badgeCodeText code}
    let (entries, previousEntryId) = statementOf redeemed
    previousEntryId `shouldBe` Nothing
    map entryTag entries `shouldBe` ["code", "badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries `shouldBe` [(3, 3), (-1, 2)]
    credentialOf redeemed `shouldSatisfy` isJust
    let firstDue = nextDue entries
    r2 <- assertBalance env purchaseKey (last entries)
    map entryTag (fst $ statementOf r2) `shouldBe` []
    setClockAt bsClock firstDue
    r3 <- assertBalance env purchaseKey (last entries)
    let (entries3, prev3) = statementOf r3
    prev3 `shouldBe` Just (entryIdOf $ last entries)
    map entryTag entries3 `shouldBe` ["badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries3 `shouldBe` [(-1, 1)]
    credentialOf r3 `shouldSatisfy` isJust
    credentialOf r3 `shouldNotBe` credentialOf redeemed
  where
    entryIdOf StatementEntry {entryId} = entryId

testRepeatInsideIssuedPeriod :: HasCallStack => TestParams -> IO ()
testRepeatInsideIssuedPeriod ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    code <- issueCode cc BTSupporter 2
    (purchaseKey, masterKey) <- newPurchaseKeys
    redeemed <- serviceCmd env purchaseKey BSCRedeemBadgeCode {masterKey, code = badgeCodeText code}
    let (entries, _) = statementOf redeemed
    repeated <- assertBalance env purchaseKey (last entries)
    map entryTag (fst $ statementOf repeated) `shouldBe` []
    credentialOf repeated `shouldBe` credentialOf redeemed

testLapseWhileAway :: HasCallStack => TestParams -> IO ()
testLapseWhileAway ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 6
    (purchaseKey, masterKey) <- newPurchaseKeys
    redeemed <- serviceCmd env purchaseKey BSCRedeemBadgeCode {masterKey, code = badgeCodeText code}
    let (entries, _) = statementOf redeemed
    -- The clock is moved from the anchor because adding months to an already clipped due date would miss the boundary.
    setClockAt bsClock (addMonths 4 (anchorOf (last entries)))
    away <- assertBalance env purchaseKey (last entries)
    let (entries', _) = statementOf away
    map entryTag entries' `shouldBe` ["lapse", "badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries' `shouldBe` [(-3, 2), (-1, 1)]
    credentialOf away `shouldSatisfy` isJust

testLastMonthExpiryRounds :: HasCallStack => TestParams -> IO ()
testLastMonthExpiryRounds ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 2
    (purchaseKey, masterKey) <- newPurchaseKeys
    redeemed <- serviceCmd env purchaseKey BSCRedeemBadgeCode {masterKey, code = badgeCodeText code}
    let (entries, _) = statementOf redeemed
    setClockAt bsClock (nextDue entries)
    renewed <- assertBalance env purchaseKey (last entries)
    let (entries', _) = statementOf renewed
    map entryTag entries' `shouldBe` ["badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries' `shouldBe` [(-1, 0)]
    expiryOf renewed `shouldBe` Just (endOfMondayAfter (nextDue entries'))

testRenewalSignsWithStoredMasterKey :: HasCallStack => TestParams -> IO ()
testRenewalSignsWithStoredMasterKey ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 2
    (purchaseKey, masterKey) <- newPurchaseKeys
    redeemed <- serviceCmd env purchaseKey BSCRedeemBadgeCode {masterKey, code = badgeCodeText code}
    masterKeyOf redeemed `shouldBe` Just masterKey
    let (entries, _) = statementOf redeemed
    setClockAt bsClock (nextDue entries)
    renewed <- assertBalance env purchaseKey (last entries)
    masterKeyOf renewed `shouldBe` Just masterKey

-- This type omits service_created_at and created_at because the client records when it stored a row, not when the service wrote it, so those columns never match.
type ReplicatedRow = (Text, Int, Int, UTCTime, Text, Maybe Text)

ledgerRows :: ChatController -> String -> IO [ReplicatedRow]
ledgerRows ChatController {chatStore} table =
  withTransaction chatStore $ \db ->
    DB.query_ db . fromString $
      "SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_badge_type, "
        <> "COALESCE(entry_credit_type, entry_debit_type) FROM "
        <> table
        <> " ORDER BY entry_id"

-- the two dates the CLI prints for each row
ledgerTimes :: ChatController -> IO [(UTCTime, UTCTime)]
ledgerTimes ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.query_ db "SELECT service_created_at, balance_start_ts FROM badge_ledger ORDER BY entry_id"

-- | The client's verdict on each row, in ledger order. The service has no such column: it computes
-- the rows rather than checking what someone else computed.
balanceChecks :: ChatController -> IO [Maybe Bool]
balanceChecks ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    map (fmap unBI . fromOnly)
      <$> DB.query_ db "SELECT balance_checked FROM badge_ledger ORDER BY entry_id"

testClientReplicatesLedger :: HasCallStack => TestParams -> IO ()
testClientReplicatesLedger ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      serviceLedger <- ledgerRows cc "sx_badge_service_badge_ledger"
      clientLedger <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) serviceLedger `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge")]
      clientLedger `shouldBe` serviceLedger
      checks <- balanceChecks (chatController alice)
      checks `shouldBe` [Just True, Just True]
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"
      clientLedger' <- ledgerRows (chatController alice) "badge_ledger"
      clientLedger' `shouldBe` serviceLedger
      expiries <- issuedExpiries (chatController alice)
      length expiries `shouldBe` 1
      -- the CLI lists the rows oldest first, with the dates they carry
      times <- ledgerTimes (chatController alice)
      alice ##> "/_badge ledger 1 1"
      forM_ (zip times [("code", "+3", "3"), ("badge", "-1", "2")]) $ \((createdAt, from), (kind, change, balance)) ->
        alice <## (day createdAt <> " " <> kind <> " " <> change <> " -> " <> balance <> ", from " <> day from)
      -- and nothing for a purchase that is another profile's
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> "/_badge ledger 2 1"
      alice <## "no ledger entries"
  where
    day = formatTime defaultTimeLocale "%Y-%m-%d"

dueAtOf :: [ReplicatedRow] -> UTCTime
dueAtOf rows = let (_, _, _, start, _, _) = last rows in start

renewalMoments :: [ReplicatedRow] -> (UTCTime, UTCTime)
renewalMoments rows =
  let expiry = endOfMondayAfter $ dueAtOf rows
   in (addUTCTime (-nominalDay) expiry, expiry)

-- | The margin must be long enough for the arming pass to finish first, because a pass that overruns does the work itself and the test then passes without arming any wake.
badgeWakeMargin :: NominalDiffTime
badgeWakeMargin = 3

-- | Standing the clock just short of t before signalling makes the worker arm the wake at t rather than do the work in this pass, so whatever follows is produced by that wake.
armWakeAt :: HasCallStack => TestCC -> TestClock -> UTCTime -> IO ()
armWakeAt cc clock t = do
  setClockAt clock $ addUTCTime (negate badgeWakeMargin) t
  cc ##> "/_app activate"
  cc <## "ok"

issuedExpiries :: ChatController -> IO [UTCTime]
issuedExpiries ChatController {chatStore} = do
  rows :: [(UTCTime, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT expiry, badge_purchase_id FROM badge_issuances ORDER BY period_end"
  pure $ map fst rows

peerBadgeExpiry :: ChatController -> IO (Maybe UTCTime)
peerBadgeExpiry ChatController {chatStore} = do
  rows :: [(Maybe UTCTime, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT badge_expiry, contact_profile_id FROM contact_profiles WHERE badge_proof IS NOT NULL ORDER BY contact_profile_id"
  pure $ case rows of
    ((t, _) : _) -> t
    [] -> Nothing

setBadgeExpiry :: ChatController -> String -> UTCTime -> IO ()
setBadgeExpiry ChatController {chatStore} whichBadge t =
  withTransaction chatStore $ \db ->
    DB.execute db (fromString $ "UPDATE contact_profiles SET badge_expiry = ? WHERE " <> whichBadge <> " IS NOT NULL") (Only t)

insertUnknownLedgerEntry :: ChatController -> IO ()
insertUnknownLedgerEntry ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.execute_ db . fromString $
      "INSERT INTO badge_ledger"
        <> " (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, entry_type, entry_credit_type,"
        <> "  entry_type_unknown, entry_type_value)"
        <> " SELECT 'unknown-entry', badge_purchase_id, 0, balance_months, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, 'credit', 'grant', 1, '{\"type\":\"grant\"}'"
        <> " FROM badge_ledger ORDER BY entry_id DESC LIMIT 1"

shownAndIssuedExpiry :: ChatController -> IO (Maybe UTCTime, Maybe UTCTime)
shownAndIssuedExpiry ChatController {chatStore} = withTransaction chatStore $ \db -> do
  shown :: [(Maybe UTCTime, Int64)] <-
    DB.query_ db "SELECT badge_expiry, contact_profile_id FROM contact_profiles WHERE badge_signature IS NOT NULL ORDER BY contact_profile_id"
  issued :: [(Maybe UTCTime, Int64)] <-
    DB.query_ db "SELECT expiry, badge_purchase_id FROM badge_issuances ORDER BY period_end DESC LIMIT 1"
  pure (firstOf shown, firstOf issued)
  where
    firstOf = \case
      ((t, _) : _) -> t
      [] -> Nothing

waitShownIssued :: HasCallStack => ChatController -> IO ()
waitShownIssued cc = loop (100 :: Int)
  where
    -- Both values being absent would compare equal, so the presence of a badge is asserted separately.
    loop 0 = shownAndIssuedExpiry cc >>= \(shown, issued) -> do
      shown `shouldSatisfy` isJust
      shown `shouldBe` issued
    loop i =
      shownAndIssuedExpiry cc >>= \(shown, issued) ->
        if isJust shown && shown == issued then pure () else threadDelay 50000 >> loop (i - 1)

waitLedgerRows :: HasCallStack => ChatController -> Int -> IO [ReplicatedRow]
waitLedgerRows cc n = loop (100 :: Int)
  where
    loop 0 = ledgerRows cc "badge_ledger" >>= \rows -> error $ "expected " <> show n <> " ledger rows, got " <> show (length rows)
    loop i = do
      rows <- ledgerRows cc "badge_ledger"
      if length rows >= n then pure rows else threadDelay 50000 >> loop (i - 1)

shownBadgeId :: HasCallStack => ChatController -> IO (Maybe Int64)
shownBadgeId ChatController {chatStore} = do
  -- Selecting two columns rather than one lets the row type avoid a backend-specific Only wrapper.
  rows :: [(Maybe Int64, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT shown_badge_id, user_id FROM users WHERE user_id = 1"
  -- An unexpected row shape raises an error rather than returning Nothing, which would let a wait for Nothing pass without reading the row.
  pure $ case rows of
    [(i, _)] -> i
    _ -> error $ "expected one users row, got " <> show rows

ackedEpisode :: HasCallStack => ChatController -> IO (Maybe Text, Maybe Text)
ackedEpisode ChatController {chatStore} = do
  rows :: [(Maybe Text, Maybe Text)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT alert_acked_kind, alert_acked_episode FROM badge_purchases"
  pure $ case rows of
    [r] -> r
    _ -> error $ "expected one badge purchase, got " <> show rows

waitShownBadge :: HasCallStack => ChatController -> Maybe Int64 -> IO ()
waitShownBadge cc expected = loop (100 :: Int)
  where
    loop 0 = shownBadgeId cc >>= \actual -> actual `shouldBe` expected
    loop i =
      shownBadgeId cc >>= \actual ->
        if actual == expected then pure () else threadDelay 50000 >> loop (i - 1)

redeemFirstBadge :: HasCallStack => TestCC -> BadgeCode -> IO ()
redeemFirstBadge alice code = do
  alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
  alice <## "badge redeemed"
  alice <## "supporter badge - active"
  alice <##. "expires "

testWorkerRenews :: HasCallStack => TestParams -> IO ()
testWorkerRenews ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) redeemed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge")]
      let (requestAt, presentAt) = renewalMoments redeemed
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
      (shownEarly, issuedEarly) <- shownAndIssuedExpiry (chatController alice)
      shownEarly `shouldNotBe` issuedEarly
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)
      let (requestAt2, presentAt2) = renewalMoments renewed
      setClockAt bsClock requestAt2
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "1: supporter"
      renewed2 <- waitLedgerRows (chatController alice) 4
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed2
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge"), (-1, 0, Just "badge")]
      serviceLedger <- ledgerRows cc "sx_badge_service_badge_ledger"
      renewed2 `shouldBe` serviceLedger
      setClockAt bsClock presentAt2
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)

testRequestWakeFires :: HasCallStack => TestParams -> IO ()
testRequestWakeFires ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      armWakeAt alice bsClock $ fst $ renewalMoments redeemed
      ledgerRows (chatController alice) "badge_ledger" >>= (`shouldBe` redeemed)
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]

testPresentWakeFires :: HasCallStack => TestParams -> IO ()
testPresentWakeFires ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      let (requestAt, presentAt) = renewalMoments redeemed
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      void $ waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      armWakeAt alice bsClock presentAt
      shownAndIssuedExpiry (chatController alice) >>= \(shown, issued) -> shown `shouldNotBe` issued
      waitShownIssued (chatController alice)

testRenewsAfterUnknownEntry :: HasCallStack => TestParams -> IO ()
testRenewsAfterUnknownEntry ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      insertUnknownLedgerEntry (chatController alice)
      setClockAt bsClock $ fst $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (0, 2, Just "grant"), (-1, 1, Just "badge")]
      checks <- balanceChecks (chatController alice)
      checks `shouldBe` [Just True, Just True, Nothing, Just True]

testRenewsAfterRestart :: HasCallStack => TestParams -> IO ()
testRenewsAfterRestart ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    rows <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 6
      redeemFirstBadge alice code
      ledgerRows (chatController alice) "badge_ledger"
    let (_, _, _, anchor, _, _) = head rows
    setClockAt bsClock $ addMonths 4 anchor
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(6, 6, Just "code"), (-1, 5, Just "badge"), (-3, 2, Just "lapse"), (-1, 1, Just "badge")]
      serviceLedger <- ledgerRows cc "sx_badge_service_badge_ledger"
      renewed `shouldBe` serviceLedger
      checks <- balanceChecks (chatController alice)
      checks `shouldBe` replicate 4 (Just True)
      waitShownIssued (chatController alice)

testWorkerRetiresExpired :: HasCallStack => TestParams -> IO ()
testWorkerRetiresExpired ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 1
        redeemFirstBadge alice code
        alice #> "@bob hi"
        bob <# "alice *> hi"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "supporter badge - active"
        bob <##. "expires "
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo
        rows <- ledgerRows (chatController alice) "badge_ledger"
        setClockAt bsClock $ dueAtOf rows
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "badge alert: support_ended "
        alice <##. "1: supporter"
        alice <##. "badge alert: support_ended "
        waitShownBadge (chatController alice) Nothing
        alice ##> "/p"
        alice <## "user profile: alice (Alice)"
        alice <## "use /p <name> [<bio>] to change it"
        -- A profile update prints nothing when only the badge changed, because viewContactUpdated compares names and links, so the next message is what confirms the removal reached bob.
        alice #> "@bob after"
        bob <# "alice> after"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo

testRetiresWhenEntitlementEnds :: HasCallStack => TestParams -> IO ()
testRetiresWhenEntitlementEnds ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      armWakeAt alice bsClock $ dueAtOf rows
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"
      alice <##. "badge alert: support_ended "
      alice <##. "1: supporter"
      alice <##. "badge alert: support_ended "
      waitShownBadge (chatController alice) Nothing

testEndedAlert :: HasCallStack => TestParams -> IO ()
testEndedAlert ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    endsAt <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      let endsAt = dueAtOf rows
      setClockAt bsClock endsAt
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "badge alert: support_ended "
      alice <##. "1: supporter"
      alice <##. "badge alert: support_ended "
      pure endsAt
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      alice <##. "badge alert: support_ended "
      alice ##> ("/_badge ack 1 1 support_ended off " <> T.unpack (safeDecodeUtf8 $ strEncode endsAt))
      alice <##. "1: supporter"
      ackedEpisode (chatController alice) `shouldReturn` (Just "support_ended", Just (safeDecodeUtf8 $ strEncode endsAt))
      alice ##> "/_badge state 1"
      alice <##. "1: supporter"
      alice ##> "/_app activate"
      alice <## "ok"
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <name> [<bio>] to change it"

-- A refusal the service will not take back is worth telling the user at once: the badge is still
-- worn and will start showing as expired.
testIssueFailedAlert :: HasCallStack => TestParams -> IO ()
testIssueFailedAlert ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      -- the service no longer knows this purchase, so it refuses with no retryAfter: terminal
      setServicePurchaseKey cc "not the purchase key"
      setClockAt bsClock $ fst $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## badgeServiceRefused
      alice <##. "badge alert: issue_failed "
      -- the state reports the failure, when the next attempt is due, and the alert it raised
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      issueErrorRow (chatController alice) >>= \(_, reason) ->
        reason `shouldBe` Just "service_error final unknown_purchase_key"

-- A failure that can clear on its own is not worth a word while contacts still see the badge as
-- valid: neither the alert nor the state shows it until the shown credential lapses, which is when
-- they stop. It is recorded from the first attempt, so the run's start is not lost.
testIssueFailedWaitsForExpiry :: HasCallStack => TestParams -> IO ()
testIssueFailedWaitsForExpiry ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    -- the redemption runs against the service, so it keeps the ordinary request timeout
    (requestAt, presentAt) <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      renewalMoments <$> ledgerRows (chatController alice) "badge_ledger"
    stopBadgeService cc
    let cfg = failingServiceCfg bsClientCfg
    -- the request goes unanswered and times out, which is a failure that can clear on its own
    setClockAt bsClock requestAt
    failedSince <- withTestChatCfg ps cfg "alice" $ \alice -> do
      alice <##. "1: supporter"
      -- the state carries no failure and no alert: /p prints only its own output
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"
      (since, reason) <- issueErrorRow (chatController alice)
      reason `shouldBe` Just "service_timeout"
      pure since
    -- the shown credential lapses: from here contacts see the badge as expired, and it is worth a word
    setClockAt bsClock presentAt
    withTestChatCfg ps cfg "alice" $ \alice -> do
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      -- still the one run: the alert's episode is when it started, not this pass
      issueErrorRow (chatController alice) >>= \(since, _) -> since `shouldBe` failedSince

-- Acknowledging answers the run that is failing, not the failure: the error stays on the badge
-- screen while the alert goes quiet, and a later run raises it again under a new episode.
testIssueFailedAckAndNewRun :: HasCallStack => TestParams -> IO ()
testIssueFailedAckAndNewRun ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      purchaseKey <- servicePurchaseKey cc
      setServicePurchaseKey cc "not the purchase key"
      let (requestAt, presentAt) = renewalMoments rows
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## badgeServiceRefused
      alice <##. "badge alert: issue_failed "
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      episode <- episodeOf . fst <$> issueErrorRow (chatController alice)
      alice ##> ("/_badge ack 1 1 issue_failed off " <> T.unpack episode)
      -- the state still carries the error, and no alert with it
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      -- the ack signalled the worker, and the pass it ran failed again and raised nothing
      alice <## badgeServiceRefused
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      ackedEpisode (chatController alice) `shouldReturn` (Just "issue_failed", Just episode)
      -- the service is back: the month it owes is issued, which ends the run
      setServicePurchaseKey cc purchaseKey
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      waitIssueErrorCleared (chatController alice)
      -- the month issued is presented when the one on the profile lapses, and only then is the
      -- next renewal asked for - so the run that fails next is a new one
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)
      setServicePurchaseKey cc "not the purchase key"
      setClockAt bsClock $ fst $ renewalMoments renewed
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## badgeServiceRefused
      alice <##. "badge alert: issue_failed "
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      -- a new run, so acknowledging the first one does not silence this one
      newEpisode <- episodeOf . fst <$> issueErrorRow (chatController alice)
      newEpisode `shouldNotBe` episode

-- The service issues nothing when the months it holds have run out - here through a debit the
-- client had not seen. That is support ending, not a failed renewal: the statement brings the
-- balance to nothing and the usual alert follows, where a recorded failure would name a
-- credential that was never issued.
testNoCredentialMonthsRanOut :: HasCallStack => TestParams -> IO ()
testNoCredentialMonthsRanOut ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      insertServiceSupportDebit cc
      -- at the credential's expiry the issued period has ended, so the service holds no current credential either
      setClockAt bsClock $ snd $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "badge alert: support_ended "
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      -- the debit is the client's now, and the next pass retires the badge on it
      rows' <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) rows' `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-2, 0, Just "support")]
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "1: supporter"
      alice <##. "badge alert: support_ended "
      waitShownBadge (chatController alice) Nothing

-- The service issuing nothing while the ledger still owes a month is a fault the client cannot
-- resolve, so it is recorded and told at once.
testNoCredentialMonthsLeft :: HasCallStack => TestParams -> IO ()
testNoCredentialMonthsLeft ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      zeroServiceLedgerTip cc
      setClockAt bsClock $ snd $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## "internal chat error: badge service issued no credential"
      alice <##. "badge alert: issue_failed "
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      (_, reason) <- issueErrorRow (chatController alice)
      reason `shouldBe` Just "unexpected badge service issued no credential"

-- | A debit the service wrote on its own, taking back the months the purchase had: copied from
-- the tip so that it follows it in every column the client checks.
insertServiceSupportDebit :: ChatController -> IO ()
insertServiceSupportDebit ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.execute_ db . fromString $
      "INSERT INTO sx_badge_service_badge_ledger"
        <> " (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, entry_type, entry_debit_type)"
        <> " SELECT 'support-debit', badge_purchase_id, -balance_months, 0, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, 'debit', 'support'"
        <> " FROM sx_badge_service_badge_ledger ORDER BY entry_id DESC LIMIT 1"

-- | The service's tip with its months struck out: nothing to issue, and no row restating the ledger.
zeroServiceLedgerTip :: ChatController -> IO ()
zeroServiceLedgerTip ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.execute_ db "UPDATE sx_badge_service_badge_ledger SET balance_months = 0 WHERE entry_id = (SELECT MAX(entry_id) FROM sx_badge_service_badge_ledger)"

badgeServiceRefused :: String
badgeServiceRefused = "bad chat command: badge service error: unknown_purchase_key"

-- | The alert's episode is the start of the run of failures, as the ack command spells it.
episodeOf :: HasCallStack => Maybe UTCTime -> Text
episodeOf = maybe (error "no failure recorded") (safeDecodeUtf8 . strEncode)

-- | A client that gives up on an unanswered service request in seconds rather than half a minute,
-- and does not retry a failed pass on its own - so every pass is one the test asked for.
failingServiceCfg :: ChatConfig -> ChatConfig
failingServiceCfg cfg =
  cfg
    { agentConfig = (agentConfig cfg) {serviceRequestTimeout = 2},
      badgeRetryInterval = RetryInterval {initialInterval = 3600000000, increaseAfter = 0, maxInterval = 3600000000}
    }

-- | The service reaches a purchase by the verified signer's key and no other way, so changing the
-- key it holds makes it answer unknown_purchase_key, and putting it back makes renewals work again.
servicePurchaseKey :: HasCallStack => ChatController -> IO ByteString
servicePurchaseKey ChatController {chatStore} = do
  rows :: [(Binary ByteString, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT purchase_key, badge_purchase_id FROM sx_badge_service_badge_purchases"
  pure $ case rows of
    [(Binary k, _)] -> k
    _ -> error $ "expected one service purchase, got " <> show (length rows)

setServicePurchaseKey :: ChatController -> ByteString -> IO ()
setServicePurchaseKey ChatController {chatStore} k =
  withTransaction chatStore $ \db ->
    DB.execute db "UPDATE sx_badge_service_badge_purchases SET purchase_key = ?" (Only (Binary k))

-- the run of failed renewals the purchase carries: when it started, and the last failure as stored
issueErrorRow :: HasCallStack => ChatController -> IO (Maybe UTCTime, Maybe Text)
issueErrorRow ChatController {chatStore} = do
  rows :: [(Maybe UTCTime, Maybe Text)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT issue_failed_since, issue_error FROM badge_purchases"
  pure $ case rows of
    [r] -> r
    _ -> error $ "expected one badge purchase, got " <> show (length rows)

-- the clearing is written by the pass that stored the issuance, which the test waits for
waitIssueErrorCleared :: HasCallStack => ChatController -> IO ()
waitIssueErrorCleared cc = loop (100 :: Int)
  where
    loop 0 = issueErrorRow cc >>= (`shouldBe` (Nothing, Nothing))
    loop i =
      issueErrorRow cc >>= \r ->
        if r == (Nothing, Nothing) then pure () else threadDelay 50000 >> loop (i - 1)

testRenewalKeepsActiveProfile :: HasCallStack => TestParams -> IO ()
testRenewalKeepsActiveProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      let (requestAt, presentAt) = renewalMoments rows
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
      alice <##. "[user: alice] 1: supporter"
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)
      alice ##> "/p"
      showActiveUser alice "alisa"

testSnoozedAlertReturns :: HasCallStack => TestParams -> IO ()
testSnoozedAlertReturns ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      let endsAt = dueAtOf rows
      setClockAt bsClock endsAt
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "badge alert: support_ended "
      alice <##. "1: supporter"
      alice <##. "badge alert: support_ended "
      alice ##> ("/_badge ack 1 1 support_ended on " <> T.unpack (safeDecodeUtf8 $ strEncode endsAt))
      alice <##. "1: supporter"
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <name> [<bio>] to change it"
      setClockAt bsClock $ addUTCTime (nominalDay + 60) endsAt
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "badge alert: support_ended "

testRenewalKeepsProfileEdits :: HasCallStack => TestParams -> IO ()
testRenewalKeepsProfileEdits ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 3
        redeemFirstBadge alice code
        alice ##> "/p alice Alice Jones"
        concurrentlyN_
          [ alice <## "user bio changed to Alice Jones (your 1 contacts are notified)",
            bob <## "contact alice updated bio: Alice Jones"
          ]
        rows <- ledgerRows (chatController alice) "badge_ledger"
        let (requestAt, presentAt) = renewalMoments rows
        setClockAt bsClock requestAt
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "1: supporter"
        renewed <- waitLedgerRows (chatController alice) 3
        map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
          `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
        setClockAt bsClock presentAt
        alice ##> "/_app activate"
        alice <## "ok"
        waitShownIssued (chatController alice)
        -- Had the renewal carried the profile the worker started with, bob would print a bio change back to "Alice" before the next message, so that message is what asserts the edited bio survived.
        alice #> "@bob after renewal"
        bob <# "alice *> after renewal"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "supporter badge - active"
        bob <##. "expires "
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo

testPresentationCatchesUp :: HasCallStack => TestParams -> IO ()
testPresentationCatchesUp ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 3
        redeemFirstBadge alice code
        alice #> "@bob hi"
        bob <# "alice *> hi"
        rows <- ledgerRows (chatController alice) "badge_ledger"
        let (requestAt, presentAt) = renewalMoments rows
        setClockAt bsClock requestAt
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "1: supporter"
        void $ waitLedgerRows (chatController alice) 3
        setClockAt bsClock presentAt
        alice ##> "/_app activate"
        alice <## "ok"
        waitShownIssued (chatController alice)
        expiries <- issuedExpiries (chatController alice)
        length expiries `shouldBe` 2
        let firstMonth = head expiries
            latestMonth = last expiries
        -- The test waits here first so that bob does not receive the presentation after the updates below and keep it.
        waitPeerBadgeExpiry (chatController bob) latestMonth
        setBadgeExpiry (chatController alice) "badge_signature" firstMonth
        setBadgeExpiry (chatController bob) "badge_proof" firstMonth
        alice ##> "/_app activate"
        alice <## "ok"
        waitPeerBadgeExpiry (chatController bob) latestMonth
        (shown, issued) <- shownAndIssuedExpiry (chatController alice)
        shown `shouldBe` Just latestMonth
        issued `shouldBe` Just latestMonth
        alice #> "@bob after repair"
        bob <# "alice *> after repair"

waitPeerBadgeExpiry :: HasCallStack => ChatController -> UTCTime -> IO ()
waitPeerBadgeExpiry cc expected = loop (100 :: Int)
  where
    loop 0 = peerBadgeExpiry cc >>= \actual -> actual `shouldBe` Just expected
    loop i =
      peerBadgeExpiry cc >>= \actual ->
        if actual == Just expected then pure () else threadDelay 50000 >> loop (i - 1)

testPurchaseKeyMismatch :: HasCallStack => TestParams -> IO ()
testPurchaseKeyMismatch ps =
  withBadgeService ps $ \clientCfg bsLink _ ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      g <- C.newRandom
      (_, signPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      (claimedPub, _) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      let signKey = B.unpack $ strEncode (C.StoredPrivateKey signPriv)
          claimed = B.unpack $ strEncode claimedPub
          req = "{\"version\":1,\"purchaseKey\":\"" <> claimed <> "\",\"request\":{\"type\":\"pauseBadge\"}}"
      alice ##> ("/_service_request 1 " <> bsLink <> " sign_key=" <> signKey <> " " <> req)
      alice <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"

testIssuerKeyMustMatchConfig :: HasCallStack => TestParams -> IO ()
testIssuerKeyMustMatchConfig ps = do
  Right (pk, sk) <- bbsKeyGen
  Right (_, otherSk) <- bbsKeyGen
  let optsFor sk' = mkBadgeServiceOpts ps sk'
      cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
  checkIssuerKey (optsFor sk) Nothing cfg >>= (`shouldSatisfy` isRight)
  checkIssuerKey (optsFor otherSk) Nothing cfg >>= (`shouldSatisfy` isLeft)
  checkIssuerKey (optsFor sk) Nothing testCfg {badgePublicKeys = M.empty} >>= (`shouldSatisfy` isLeft)
  let halfGiven = (optsFor sk) {issuerKey = Left "--issuer-key-idx and --issuer-secret are given together or not at all"}
  checkIssuerKey halfGiven Nothing cfg >>= (`shouldSatisfy` isLeft)

testIssuerIniKeyMustBeTrusted :: HasCallStack => TestParams -> IO ()
testIssuerIniKeyMustBeTrusted ps = do
  Right (pk, sk) <- bbsKeyGen
  Right (_, untrusted) <- bbsKeyGen
  let cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
      fromCli = mkBadgeServiceOpts ps sk
      fromIni = fromCli {issuerKey = Right Nothing}
      iniWith idx k = withIssuer ["index = " <> tshow idx, "private_key = " <> safeDecodeUtf8 (strEncode k)] readServiceConfig
  Right trusted <- iniWith testIssuerKeyIdx sk
  checkIssuerKey fromIni (Just trusted) cfg `shouldReturn` Right (BadgeIssuerKey testIssuerKeyIdx sk)

  Right wrongSecret <- iniWith testIssuerKeyIdx untrusted
  checkIssuerKey fromIni (Just wrongSecret) cfg >>= (`shouldSatisfy` isLeft)
  checkIssuerKey fromCli (Just wrongSecret) cfg `shouldReturn` Right (BadgeIssuerKey testIssuerKeyIdx sk)

  Right unknownIndex <- iniWith (testIssuerKeyIdx + 8) sk
  checkIssuerKey fromIni (Just unknownIndex) cfg
    `shouldReturn` Left ("no configured badge key at index " <> show (testIssuerKeyIdx + 8) <> ", clients could not verify what this service signs")

testRedeemUnpaidCode :: HasCallStack => TestParams -> IO ()
testRedeemUnpaidCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      unpaid <- issueCodeAs cc BTSupporter 1 "unpaid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg unpaid)
      alice <## "cannot redeem badge code: badge service error: payment_pending"
      paid <- issueCodeAs cc BTSupporter 1 "paid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg paid)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "

-- | A redemption deadline is a year out and cannot be waited for, so the code is issued unpaid and then marked paid with a deadline already in the past.
testExpiredCode :: HasCallStack => TestParams -> IO ()
testExpiredCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCodeAs cc BTSupporter 1 "unpaid"
      now <- getCurrentTime
      withDB' "markCodePaid" cc (\db -> markCodePaid db (badgeCodeHash code) (addUTCTime (-60) now))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "cannot redeem badge code: badge service error: code_expired"

testRedeemedBeforeTheDeadline :: HasCallStack => TestParams -> IO ()
testRedeemedBeforeTheDeadline ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCodeAs cc BTSupporter 1 "unpaid"
      now <- getCurrentTime
      withDB' "markCodePaid" cc (\db -> markCodePaid db (badgeCodeHash code) (addUTCTime 3600 now))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "

      -- The expiry is set with direct SQL because markCodePaid writes it only on the unpaid-to-paid transition, which this code has already made.
      withDB' "expireCode" cc (\db ->
        DB.execute
          db
          "UPDATE sx_badge_service_badge_codes SET expires_at = ? WHERE code_hash = ?"
          (addUTCTime (-60) now, Binary (badgeCodeHash code)))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"

testRevokeRedeemedCode :: HasCallStack => TestParams -> IO ()
testRevokeRedeemedCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCodeAs cc BTSupporter 1 "paid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      refused <- revokeCodeAs cc code
      refused `shouldSatisfy` T.isInfixOf "redeemed already, so it cannot be revoked"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"

testRevokeUnknownCode :: HasCallStack => TestParams -> IO ()
testRevokeUnknownCode ps =
  withBadgeService ps $ \_ _ cc -> do
    g <- C.newRandom
    code <- randomBadgeCode g
    unknown <- revokeCodeAs cc code
    unknown `shouldSatisfy` T.isInfixOf "no such code"

testRevokedCode :: HasCallStack => TestParams -> IO ()
testRevokedCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      paid <- issueCodeAs cc BTSupporter 1 "paid"
      revokeCodeAs cc paid `shouldReturn` "revoked"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg paid)
      alice <## "cannot redeem badge code: badge service error: code_invalid"
      second <- revokeCodeAs cc paid
      second `shouldSatisfy` T.isInfixOf "revoked already"

purchaseCmd :: BadgeMasterKey -> ServicePayment -> BadgeServiceCommand
purchaseCmd masterKey payment = BSCPurchaseBadge {masterKey, payment, upgrade = Nothing}

supporterPlay :: ServicePayment
supporterPlay = googlePayment "badge_supporter_01" googleSupporterToken

paymentArg :: ServicePayment -> String
paymentArg = LB.unpack . J.encode

refusalOf :: HasCallStack => BadgeServiceResponse -> (BadgeServiceErrorCode, Maybe Word32)
refusalOf = \case
  BSPError {code, retryAfter} -> (code, retryAfter)
  r -> error $ "expected an error, got " <> show (J.toJSON r)

badgeTypeOf :: HasCallStack => BadgeServiceResponse -> Maybe BadgeType
badgeTypeOf r = (\(BadgeCredential _ _ _ BadgeInfo {badgeType}) -> badgeType) <$> credentialOf r

rowCount :: ChatController -> String -> IO Int
rowCount ChatController {chatStore} table =
  withTransaction chatStore $ \db -> do
    [Only n] <- DB.query_ db $ fromString $ "SELECT COUNT(*) FROM " <> table
    pure n

-- | Each store payment's provider, amount and currency, and the ledger rows that name it.
storePayments :: ChatController -> IO [(Text, Maybe Int, Maybe Text, Int)]
storePayments ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.query_ db . fromString $
      "SELECT pay.provider, pay.amount, pay.currency, COUNT(l.entry_id) FROM sx_badge_service_payments pay "
        <> "LEFT JOIN sx_badge_service_badge_ledger l ON l.payment_id = pay.payment_id "
        <> "GROUP BY pay.payment_id, pay.provider, pay.amount, pay.currency ORDER BY pay.provider"

nothingPurchased :: HasCallStack => ChatController -> IO ()
nothingPurchased cc = do
  rowCount cc "sx_badge_service_payments" `shouldReturn` 0
  rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 0

testStorePurchase :: HasCallStack => TestParams -> IO ()
testStorePurchase ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    let (entries, previousEntryId) = statementOf purchased
    previousEntryId `shouldBe` Nothing
    map entryTag entries `shouldBe` ["payment", "badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries `shouldBe` [(1, 1), (-1, 0)]
    badgeTypeOf purchased `shouldBe` Just BTSupporter
    masterKeyOf purchased `shouldBe` Just masterKey
    map (\(_, c, m, _, _, t) -> (c, m, t)) <$> ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` [(1, 1, Just "payment"), (-1, 0, Just "badge")]
    -- Play reports no price, and only the credit names the payment
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]

testStorePurchaseReplay :: HasCallStack => TestParams -> IO ()
testStorePurchaseReplay ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    replayed <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    credentialOf replayed `shouldBe` credentialOf purchased
    map entryTag (fst $ statementOf replayed) `shouldBe` map entryTag (fst $ statementOf purchased)
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStoreReceiptUsed :: HasCallStack => TestParams -> IO ()
testStoreReceiptUsed ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (firstKey, firstMasterKey) <- newPurchaseKeys
    _ <- serviceCmd env firstKey $ purchaseCmd firstMasterKey supporterPlay
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    (otherKey, otherMasterKey) <- newPurchaseKeys
    used <- serviceCmd env otherKey $ purchaseCmd otherMasterKey supporterPlay
    refusalOf used `shouldBe` (BSEReceiptUsed, Nothing)
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStoreReceiptInvalid :: HasCallStack => TestParams -> IO ()
testStoreReceiptInvalid ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleSandboxJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    let refused payment = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey payment)
    refused (googlePayment "badge_supporter_01" "not-a-purchase") `shouldReturn` (BSEReceiptInvalid, Nothing)
    refused SPApple {jws = "not.a.jws"} `shouldReturn` (BSEReceiptInvalid, Nothing)
    -- refused by the service, which the store would have vouched for
    refused SPApple {jws = appleSandboxJWS} `shouldReturn` (BSEReceiptInvalid, Nothing)
    nothingPurchased cc

testStoreUnreachable :: HasCallStack => TestParams -> IO ()
testStoreUnreachable ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    unreachable <- serviceCmd env purchaseKey $ purchaseCmd masterKey $ googlePayment "badge_supporter_01" googleUnreachableToken
    refusalOf unreachable `shouldSatisfy` \(code, retryAfter) -> code == BSEProviderUnavailable && isJust retryAfter
    nothingPurchased cc

testStorePending :: HasCallStack => TestParams -> IO ()
testStorePending ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    let unsettled = purchaseCmd masterKey $ googlePayment "badge_supporter_01" googlePendingToken
    waiting <- serviceCmd env purchaseKey unsettled
    refusalOf waiting `shouldSatisfy` \(code, retryAfter) -> code == BSEPaymentPending && isJust retryAfter
    nothingPurchased cc
    settlePending store
    settled <- serviceCmd env purchaseKey unsettled
    badgeTypeOf settled `shouldBe` Just BTSupporter
    again <- serviceCmd env purchaseKey unsettled
    credentialOf again `shouldBe` credentialOf settled
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]

testStoreUnknownProduct :: HasCallStack => TestParams -> IO ()
testStoreUnknownProduct ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    -- a real purchase of a subscription, whose renewals nothing adds to a badge yet
    subscription <- serviceCmd env purchaseKey $ purchaseCmd masterKey $ googlePayment "subscr_badge_supporter_01" googleSubscriptionToken
    refusalOf subscription `shouldBe` (BSEProductUnavailable, Nothing)
    nothingPurchased cc

testStoreUpgradeRefused :: HasCallStack => TestParams -> IO ()
testStoreUpgradeRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    g <- C.newRandom
    (fromPurchaseKey, fromPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
    now <- getCurrentTime
    let upgrade = BadgeUpgrade {fromPurchaseKey, receipt = "receipt", receiptSignature = C.sign' fromPriv "receipt", balance = BadgeBalance {lastEntry = emptyEntry now BTSupporter}}
    upgraded <- serviceCmd env purchaseKey BSCPurchaseBadge {masterKey, payment = supporterPlay, upgrade = Just upgrade}
    refusalOf upgraded `shouldBe` (BSEBadRequest, Nothing)
    nothingPurchased cc

testStoreBadgeTypeFromProduct :: HasCallStack => TestParams -> IO ()
testStoreBadgeTypeFromProduct ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleLegendJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    legend <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleLegendJWS}
    badgeTypeOf legend `shouldBe` Just BTLegend
    map (\StatementEntry {balanceBadgeType} -> balanceBadgeType) (fst $ statementOf legend) `shouldBe` [BTLegend, BTLegend]
    storePayments cc `shouldReturn` [("apple", Just 7000, Just "USD", 1)]

testNonStoreFundingRefused :: HasCallStack => TestParams -> IO ()
testNonStoreFundingRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    invoice <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPInvoice {invoiceId = InvoiceId "inv"}
    refusalOf invoice `shouldBe` (BSEUnknownPurchaseKey, Nothing)
    transfer <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPReceipt {receipt = "receipt"}
    refusalOf transfer `shouldBe` (BSEUnknownPurchaseKey, Nothing)
    nothingPurchased cc

testStoreReplayWhileStoreDown :: HasCallStack => TestParams -> IO ()
testStoreReplayWhileStoreDown ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    setGoogleDown store True
    -- the key it was credited to is answered from the record
    replayed <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    credentialOf replayed `shouldBe` credentialOf purchased
    -- any other key waits for the store, or the answer would tell it which purchases were credited
    (otherKey, otherMasterKey) <- newPurchaseKeys
    other <- serviceCmd env otherKey $ purchaseCmd otherMasterKey supporterPlay
    fst (refusalOf other) `shouldBe` BSEProviderUnavailable
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger

testStoreVerifierFailures :: HasCallStack => TestParams -> IO ()
testStoreVerifierFailures ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleThrowingJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    let answer payment = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey payment)
    -- Apple is checked offline, so a verifier that throws was answered by nothing but its own bug
    answer SPApple {jws = appleThrowingJWS} `shouldReturn` (BSEInternal, Nothing)
    answer (googlePayment "badge_supporter_01" googleThrowingToken) >>= (`shouldSatisfy` \(code, retryAfter) -> code == BSEProviderUnavailable && isJust retryAfter)
    answer (googlePayment "badge_supporter_01" googleHangingToken) >>= (`shouldSatisfy` \(code, retryAfter) -> code == BSEProviderUnavailable && isJust retryAfter)
    nothingPurchased cc

testStoreClaimRace :: HasCallStack => TestParams -> IO ()
testStoreClaimRace ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsController = cc} -> do
    (firstKey, firstMasterKey) <- newPurchaseKeys
    (otherKey, otherMasterKey) <- newPurchaseKeys
    now <- getCurrentTime
    let claim paymentId purchaseKey masterKey =
          withDB' "claim" cc $ \db ->
            createStorePurchase db NewStorePurchase {paymentId, provider = PPGoogle, providerRef = "ref", paid = Nothing, purchaseKey, masterKey, badgeType = BTSupporter} now
    -- both past the read before either wrote, as two requests signing at once are
    claim "p1" firstKey firstMasterKey >>= (`shouldSatisfy` either (const False) isJust)
    claim "p2" otherKey otherMasterKey `shouldReturn` Right Nothing
    rowCount cc "sx_badge_service_payments" `shouldReturn` 1
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStorePurchaseKeyMismatch :: HasCallStack => TestParams -> IO ()
testStorePurchaseKeyMismatch ps =
  withBadgeService ps $ \clientCfg bsLink cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      g <- C.newRandom
      (_, signPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      (claimedPub, _) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      let signKey = B.unpack $ strEncode (C.StoredPrivateKey signPriv)
          claimed = B.unpack $ strEncode claimedPub
          req =
            "{\"version\":1,\"purchaseKey\":\"" <> claimed
              <> "\",\"request\":{\"type\":\"purchaseBadge\",\"masterKey\":\"" <> testMasterKeyB64
              <> "\",\"payment\":" <> paymentArg supporterPlay <> "}}"
      alice ##> ("/_service_request 1 " <> bsLink <> " sign_key=" <> signKey <> " " <> req)
      alice <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"
      nothingPurchased cc

testPurchaseBadge :: HasCallStack => TestParams -> IO ()
testPurchaseBadge ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let purchase = "/_badge purchase 1 " <> paymentArg supporterPlay
      alice ##> purchase
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      -- the app presents a purchase until the badge is stored, and one already stored adds nothing
      alice ##> purchase
      alice <## "badge already redeemed"
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
      map (\(_, c, m, _, _, t) -> (c, m, t)) <$> ledgerRows (chatController alice) "badge_ledger" `shouldReturn` [(1, 1, Just "payment"), (-1, 0, Just "badge")]

testPurchaseBadgeAppStore :: HasCallStack => TestParams -> IO ()
testPurchaseBadgeAppStore ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc, bsStore = FakeStore {appleLegendJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendJWS})
      alice <## "badge redeemed"
      alice <## "legend badge - active"
      alice <##. "expires "
      storePayments cc `shouldReturn` [("apple", Just 7000, Just "USD", 1)]

testPurchaseStash :: HasCallStack => TestParams -> IO ()
testPurchaseStash ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let stashes = rowCount (chatController alice) "badge_store_receipts"
      -- the store does not vouch for it, so the keys stashed for it can never be credited
      alice ##> ("/_badge purchase 1 " <> paymentArg (googlePayment "badge_supporter_01" "not-a-purchase"))
      alice <## "cannot redeem badge code: badge service error: receipt_invalid"
      stashes `shouldReturn` 0
      -- no store transaction to key a stash by, so nothing is stashed or sent
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = "not.a-jws"})
      alice <## "cannot redeem badge code: invalid store receipt"
      stashes `shouldReturn` 0
      nothingPurchased cc
      -- pending keeps the keys, and the settled purchase is credited to them, once
      let unsettled = "/_badge purchase 1 " <> paymentArg (googlePayment "badge_supporter_01" googlePendingToken)
      alice ##> unsettled
      alice <## "cannot redeem badge code: badge service error: payment_pending"
      stashes `shouldReturn` 1
      settlePending store
      alice ##> unsettled
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      stashes `shouldReturn` 1
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testPurchaseWhileBadgeHeld :: HasCallStack => TestParams -> IO ()
testPurchaseWhileBadgeHeld ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      alice <## "cannot redeem badge code: badge already active"
      rowCount (chatController alice) "badge_store_receipts" `shouldReturn` 0
      rowCount cc "sx_badge_service_payments" `shouldReturn` 0

testPurchaseSameReceiptOtherProfile :: HasCallStack => TestParams -> IO ()
testPurchaseSameReceiptOtherProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      -- the store transaction is the device's, so it stays with the profile it was bought under
      alice ##> ("/_badge purchase 2 " <> paymentArg supporterPlay)
      alice <## "[user: alice] badge already redeemed"
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
      alice ##> "/p"
      showActiveUser alice "alisa"

testPurchaseStrandedUnderOtherProfile :: HasCallStack => TestParams -> IO ()
testPurchaseStrandedUnderOtherProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let unsettled userId = "/_badge purchase " <> show (userId :: Int) <> " " <> paymentArg (googlePayment "badge_supporter_01" googlePendingToken)
      alice ##> unsettled 1
      alice <## "cannot redeem badge code: badge service error: payment_pending"
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      settlePending store
      -- presented again under whichever profile is active, the purchase reaches the keys alice stashed
      alice ##> unsettled 2
      alice <## "[user: alice] badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      rowCount (chatController alice) "badge_store_receipts" `shouldReturn` 1
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice, * supporter)"
