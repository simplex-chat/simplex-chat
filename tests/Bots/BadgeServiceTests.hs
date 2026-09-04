{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Bots.BadgeServiceTests where

import BadgeService.Options
import BadgeService.Service
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, readTMVar)
import Control.Monad (void, when)
import Control.Exception (finally)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
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
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeMasterKey, BadgeType (..), generateMasterKey)
import Simplex.Chat.Badges.Code (BadgeCode, badgeCodeText, formatBadgeCode, parseBadgeCode, randomBadgeCode)
import Simplex.Chat.Badges.Ledger (addMonths, creditTypeTag, debitTypeTag, endOfSundayAfter)
import Simplex.Chat.Badges.Service
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatResponse (CRCustomChatResponse))
import Simplex.Chat.Core (sendChatCmdStr)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSSecretKey, bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode, strEncode, textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8)
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
  it "should redeem a second code, and not restore the first badge on replay" testRedeemSecondCode
  it "should refuse to issue a code with an unknown badge type or a nonsense month count" testIssueRejectsBadArguments
  it "should refuse a request whose purchaseKey is not the verified signer" testPurchaseKeyMismatch
  it "should refuse to start unless the issuer secret is the key trusted at its index" testIssuerKeyMustMatchConfig
  it "should credit a code's months and issue one credential per month" testCodeMonthsRenew
  it "should return the stored credential for a repeat inside an issued period" testRepeatInsideIssuedPeriod
  it "should lapse only the months that elapsed while the client was away" testLapseWhileAway
  it "should round the last month's expiry up to the end of the Sunday after it" testLastMonthExpiryRounds
  it "should sign a renewal with the master key stored on the purchase" testRenewalSignsWithStoredMasterKey
  it "should leave the client holding the same ledger rows as the service" testClientReplicatesLedger
  it "should renew a badge whose month has elapsed, with no command" testWorkerRenews
  it "should renew a badge whose newest ledger row is of an unknown type" testRenewsAfterUnknownEntry
  it "should stop showing a badge whose balance ran out, and tell contacts" testWorkerRetiresExpired
  it "should alert that support ended, survive a restart, and go silent once acknowledged" testEndedAlert
  it "should raise a snoozed alert once more when the snooze lapses" testSnoozedAlertReturns
  it "should renew a badge on a profile that is not active, without switching to it" testRenewalKeepsActiveProfile
  it "should broadcast the current profile when a renewal presents a badge" testRenewalKeepsProfileEdits
  it "should present the month already issued when a previous pass did not" testPresentationCatchesUp

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
      issuerKey = Just BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey},
      testing = True
    }

-- | A clock the service and the client both read: real time plus an offset the test moves. It
-- tracks real time rather than freezing it, so a sleeper still sleeps the right real duration.
newtype TestClock = TestClock (IORef NominalDiffTime)

newTestClock :: IO TestClock
newTestClock = TestClock <$> newIORef 0

testClockTime :: TestClock -> IO UTCTime
testClockTime (TestClock r) = do
  offset <- readIORef r
  addUTCTime offset <$> getCurrentTime

-- | Move the clock so that "now" becomes exactly the given time - months are calendar months, so
-- a test crosses a boundary by naming the date rather than adding a duration.
setClockAt :: TestClock -> UTCTime -> IO ()
setClockAt (TestClock r) t = getCurrentTime >>= \real -> writeIORef r (diffUTCTime t real)

-- | Everything a badge test may need from a running service.
data BadgeServiceEnv = BadgeServiceEnv
  { bsIssuerKey :: BadgeIssuerKey,
    bsClock :: TestClock,
    bsClientCfg :: ChatConfig,
    bsAddress :: String,
    bsController :: ChatController
  }

-- | Start the badge service on a fresh issuer key, and hand the test body what depends on it:
-- the client config trusting that key and addressing the service, the address, and the controller.
withBadgeService :: HasCallStack => TestParams -> (ChatConfig -> String -> ChatController -> IO ()) -> IO ()
withBadgeService ps test =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsAddress, bsController} -> test bsClientCfg bsAddress bsController

withBadgeServiceEnv :: HasCallStack => TestParams -> (BadgeServiceEnv -> IO ()) -> IO ()
withBadgeServiceEnv ps test = do
  Right (pk, sk) <- bbsKeyGen
  clock <- newTestClock
  let opts = mkBadgeServiceOpts ps sk
      -- the service refuses to start unless its secret is the key trusted at its index
      svcCfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk, badgeCurrentTime = testClockTime clock}
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  -- First start: badge service takes the CreateMyAddress branch.
  runBadgeService svcCfg opts $ \_ -> pure ()
  -- Reopen the DB to read the link the service created.
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (sLink, _) <- getContactLinks bs False
    bs <## "auto_accept off"
    pure sLink
  let clientCfg =
        svcCfg {badgeServiceAddress = Just $ either (error . ("bad badge service address: " <>)) id $ strDecode (B.pack bsLink)}
  -- Second start: badge service takes the ShowMyAddress branch, then serves the test body.
  runBadgeService svcCfg opts $ \env -> do
    cc <- atomically $ readTMVar $ serviceCC env
    test BadgeServiceEnv {bsIssuerKey = BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey = sk}, bsClock = clock, bsClientCfg = clientCfg, bsAddress = bsLink, bsController = cc}

-- through the operator command the service actually exposes, not the function behind it
issueCode :: HasCallStack => ChatController -> BadgeType -> Int -> IO BadgeCode
issueCode cc badgeType months =
  sendChatCmdStr cc ("//issue " <> T.unpack (textEncode badgeType) <> " " <> show months) >>= \case
    Right (CRCustomChatResponse _ response) -> case T.stripPrefix "code " response of
      Just c | Just code <- parseBadgeCode c -> pure code
      _ -> error $ "unexpected issue response: " <> T.unpack response
    r -> error $ "issue failed: " <> show (() <$ r)

-- | The post-start hook fills serviceCC once the address exists, so waiting on it is the service
-- being ready. A fixed delay here raced with startup and left the address output of one start
-- arriving during the next test.
runBadgeService :: ChatConfig -> BadgeServiceOpts -> (ServiceState -> IO ()) -> IO ()
runBadgeService cfg opts action = do
  env <- newServiceState
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
        -- the service has never seen this purchase key: a first redemption must still succeed
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
      -- Retyped in another case and without separators, so it normalises to the same code and
      -- finds the same stashed keys: a retry the service can recognise as the same signer.
      alice ##> ("/_redeem_badge_code 1 " <> map toLower (T.unpack $ badgeCodeText code))
      alice <## "badge already redeemed"
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"

-- The service answers unknown and malformed alike; the client refuses malformed locally, which
-- is why the two reach the user differently.
testRedeemUnknownCode :: HasCallStack => TestParams -> IO ()
testRedeemUnknownCode ps =
  withBadgeService ps $ \clientCfg bsLink _ ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      g <- C.newRandom
      unknown <- randomBadgeCode g
      alice ##> ("/_redeem_badge_code 1 " <> codeArg unknown)
      alice <## "bad chat command: badge service error: code_invalid"
      -- a failed check character is refused before anything leaves the device
      alice ##> "/_redeem_badge_code 1 SXB-00000-00000-00000-00001"
      alice <## "bad chat command: invalid badge code"
      -- sent straight to the service, past the client's own check, the two are one answer
      (_, redeemPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      redeemDirect alice bsLink redeemPriv (T.unpack $ badgeCodeText unknown)
      alice <## "service response: {\"code\":\"code_invalid\",\"type\":\"error\"}"
      redeemDirect alice bsLink redeemPriv "SXB-00000-00000-00000-00001"
      alice <## "service response: {\"code\":\"code_invalid\",\"type\":\"error\"}"

-- a signed redeemBadgeCode sent as a raw service request, bypassing the client's own checks
redeemDirect :: HasCallStack => TestCC -> String -> C.PrivateKeyEd25519 -> String -> IO ()
redeemDirect cc bsLink signPriv code = do
  let purchaseKey = B.unpack $ strEncode $ C.publicKey signPriv
      signKey = B.unpack $ strEncode (C.StoredPrivateKey signPriv)
      req =
        "{\"version\":1,\"purchaseKey\":\"" <> purchaseKey
          <> "\",\"request\":{\"type\":\"redeemBadgeCode\",\"masterKey\":\"" <> testMasterKeyB64
          <> "\",\"code\":\"" <> code <> "\"}}"
  cc ##> ("/_service_request 1 " <> bsLink <> " sign_key=" <> signKey <> " " <> req)

-- any 32 bytes: these requests never reach signing
testMasterKeyB64 :: String
testMasterKeyB64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

-- BadgeType decodes anything to BTUnknown, so without a check at this boundary an operator
-- typo would issue a code no app can show as a badge.
testIssueRejectsBadArguments :: HasCallStack => TestParams -> IO ()
testIssueRejectsBadArguments ps =
  withBadgeService ps $ \_ _ cc -> do
    let refuses arg = issueRaw cc arg >>= (`shouldSatisfy` isLeft)
    refuses "suporter"
    refuses "supporter 0"
    refuses "supporter 256"
    refuses "supporter 1 gratis"
    refuses ""
    issueRaw cc "supporter 255 paid" >>= (`shouldSatisfy` isRight)

issueRaw :: ChatController -> String -> IO (Either () ())
issueRaw cc args =
  sendChatCmdStr cc ("//issue " <> args) >>= \case
    Right CRCustomChatResponse {} -> pure $ Right ()
    _ -> pure $ Left ()

-- Both purchases fund no payment, so both leave payment_id NULL under UNIQUE(payment_id).
-- The first purchase stays as it is: retiring a superseded badge is not implemented.
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
      alice <## "badge redeemed"
      alice <## "legend badge - active"
      alice <##. "expires "
      alice ##> "/p"
      showActiveUser alice "alice (Alice, * legend)"
      -- replaying the first code must not put supporter back
      alice ##> ("/_redeem_badge_code 1 " <> codeArg supporter)
      alice <## "badge already redeemed"
      alice ##> "/p"
      showActiveUser alice "alice (Alice, * legend)"

-- Each profile stashes its own keys, so the second reaches the service as a different signer -
-- rather than being handed the first profile's badge, or colliding in badge_code_redemptions.
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
      alice <## "bad chat command: badge service error: code_used"
      alice ##> "/p"
      showActiveUser alice "alisa"
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice, * supporter)"

-- Ledger behaviour, driven against the real service - real signing, real rows - through the
-- handler rather than the transport, so the typed response can be asserted. The transport and its
-- purchaseKey guard are covered by the tests above.

serviceCmd :: HasCallStack => BadgeServiceEnv -> C.PublicKeyEd25519 -> BadgeServiceCommand -> IO BadgeServiceResponse
serviceCmd BadgeServiceEnv {bsIssuerKey, bsController} purchaseKey request =
  badgeServiceResponse bsIssuerKey bsController (Just purchaseKey) reqObject
  where
    reqObject = case J.toJSON BadgeServiceRequest {version = currentBadgeServiceVersion, purchaseKey = Just purchaseKey, request} of
      J.Object o -> o
      _ -> error "badge service request must encode as an object"

-- the fields a ledger assertion reads, without the ambiguity of the shared record names
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

-- the next month falls due when the balance start reaches it, which is the last entry's start
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

-- A three month code credits three months and issues the first; a month later the second is
-- issued, and only then. Asserted against the service's own rows.
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
    -- still inside the first month: nothing new is written
    r2 <- assertBalance env purchaseKey (last entries)
    map entryTag (fst $ statementOf r2) `shouldBe` []
    -- the second month falls due
    setClockAt bsClock firstDue
    r3 <- assertBalance env purchaseKey (last entries)
    let (entries3, prev3) = statementOf r3
    prev3 `shouldBe` Just (entryIdOf $ last entries)
    map entryTag entries3 `shouldBe` ["badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries3 `shouldBe` [(-1, 1)]
    credentialOf r3 `shouldSatisfy` isJust
    -- a different credential for a different month, not the same signature returned twice
    credentialOf r3 `shouldNotBe` credentialOf redeemed
  where
    entryIdOf StatementEntry {entryId} = entryId

-- A repeat inside an issued month returns the credential already stored, and writes no row:
-- re-signing the same period would churn the client's credential for nothing.
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

-- Months that passed unissued are lapsed in one row, and the month now current is issued.
testLapseWhileAway :: HasCallStack => TestParams -> IO ()
testLapseWhileAway ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 6
    (purchaseKey, masterKey) <- newPurchaseKeys
    redeemed <- serviceCmd env purchaseKey BSCRedeemBadgeCode {masterKey, code = badgeCodeText code}
    let (entries, _) = statementOf redeemed
    -- away past the fourth boundary of the run: three months lapse, the fourth is due. Counted
    -- from the anchor - adding months to an already clipped due date would miss the boundary.
    setClockAt bsClock (addMonths 4 (anchorOf (last entries)))
    away <- assertBalance env purchaseKey (last entries)
    let (entries', _) = statementOf away
    map entryTag entries' `shouldBe` ["lapse", "badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries' `shouldBe` [(-3, 2), (-1, 1)]
    credentialOf away `shouldSatisfy` isJust

-- Every badge issued in a week expires at the same moment, so the expiry says nothing about when
-- it was bought. On the last month of a balance paidThrough is the period end exactly, so a
-- client-proposed expiry capped the rounding away and put the expiry back on the anniversary.
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
    -- the last month the balance funds: nothing is left to issue after it
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries' `shouldBe` [(-1, 0)]
    expiryOf renewed `shouldBe` Just (endOfSundayAfter (nextDue entries'))

-- A renewal states nothing, so the credential carries the master key stored with the purchase.
-- The service used to sign whatever key the request supplied, checking only the badge type.
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

-- The replicated columns of a ledger, in order. service_created_at and created_at are left out:
-- the client records when it stored a row, which is not when the service wrote it.
type ReplicatedRow = (Text, Int, Int, UTCTime, Text, Maybe Text)

ledgerRows :: ChatController -> String -> IO [ReplicatedRow]
ledgerRows ChatController {chatStore} table =
  withTransaction chatStore $ \db ->
    DB.query_ db . fromString $
      "SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_badge_type, "
        <> "COALESCE(entry_credit_type, entry_debit_type) FROM "
        <> table
        <> " ORDER BY entry_id"

-- The client copies the statement verbatim and authors nothing, so after a redemption both sides
-- hold the same rows under the same entry ids.
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
      -- the code credit and the first month, on both sides
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) serviceLedger `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge")]
      clientLedger `shouldBe` serviceLedger
      -- redeeming again replays the statement, and must not duplicate a single row
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"
      clientLedger' <- ledgerRows (chatController alice) "badge_ledger"
      clientLedger' `shouldBe` serviceLedger
      -- nor a second issuance for the one month issued: the replay names a month already stored
      expiries <- issuedExpiries (chatController alice)
      length expiries `shouldBe` 1

-- the balance start of the last row, which is when the next month falls due
dueAtOf :: [ReplicatedRow] -> UTCTime
dueAtOf rows = let (_, _, _, start, _, _) = last rows in start

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
      DB.query_ db "SELECT badge_expiry, contact_profile_id FROM contact_profiles WHERE badge_proof IS NOT NULL"
  pure $ case rows of
    ((t, _) : _) -> t
    [] -> Nothing

setBadgeExpiry :: ChatController -> String -> UTCTime -> IO ()
setBadgeExpiry ChatController {chatStore} whichBadge t =
  withTransaction chatStore $ \db ->
    DB.execute db (fromString $ "UPDATE contact_profiles SET badge_expiry = ? WHERE " <> whichBadge <> " IS NOT NULL") (Only t)

-- | Copy the newest ledger row as an entry of a type this version does not know: the service is
-- deployed ahead of app releases, so a new type first arrives at clients that predate it.
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

-- | The month the profile is showing, and the month last issued. They must agree: a profile left
-- on an earlier month shows contacts a badge the ledger has already replaced.
shownAndIssuedExpiry :: ChatController -> IO (Maybe UTCTime, Maybe UTCTime)
shownAndIssuedExpiry ChatController {chatStore} = withTransaction chatStore $ \db -> do
  shown :: [(Maybe UTCTime, Int64)] <-
    DB.query_ db "SELECT badge_expiry, contact_profile_id FROM contact_profiles WHERE badge_signature IS NOT NULL"
  issued :: [(Maybe UTCTime, Int64)] <-
    DB.query_ db "SELECT expiry, badge_purchase_id FROM badge_issuances ORDER BY period_end DESC LIMIT 1"
  pure (firstOf shown, firstOf issued)
  where
    firstOf = \case
      ((t, _) : _) -> t
      [] -> Nothing

-- The worker acts on its own schedule, so the test waits for the rows rather than for a response.
-- The budget is generous because a renewal is a real round trip to the in-process service, and
-- the whole suite runs several of them under load.
waitLedgerRows :: HasCallStack => ChatController -> Int -> IO [ReplicatedRow]
waitLedgerRows cc n = loop (600 :: Int)
  where
    loop 0 = ledgerRows cc "badge_ledger" >>= \rows -> error $ "expected " <> show n <> " ledger rows, got " <> show (length rows)
    loop i = do
      rows <- ledgerRows cc "badge_ledger"
      if length rows >= n then pure rows else threadDelay 50000 >> loop (i - 1)

-- the badge the profile shows, which the worker clears when the balance has run out
shownBadgeId :: ChatController -> IO (Maybe Int64)
shownBadgeId ChatController {chatStore} = do
  -- two columns rather than one, so the row type needs no backend-specific Only
  rows :: [(Maybe Int64, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT shown_badge_id, user_id FROM users WHERE user_id = 1"
  pure $ case rows of
    [(i, _)] -> i
    _ -> Nothing

-- the occurrence the user answered, which silences that alert and no other
ackedEpisode :: ChatController -> IO (Maybe Text, Maybe Text)
ackedEpisode ChatController {chatStore} = do
  rows :: [(Maybe Text, Maybe Text)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT alert_acked_kind, alert_acked_episode FROM badge_purchases"
  pure $ case rows of
    [r] -> r
    _ -> (Nothing, Nothing)

waitShownBadge :: HasCallStack => ChatController -> Maybe Int64 -> IO ()
waitShownBadge cc expected = loop (600 :: Int)
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

-- A month elapses and the badge renews from the worker's own pass, with no command sent by the
-- app: chat activate only signals it, and the work is derived from the stored ledger.
testWorkerRenews :: HasCallStack => TestParams -> IO ()
testWorkerRenews ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) redeemed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge")]
      -- the second month falls due while the app is running
      setClockAt bsClock $ dueAtOf redeemed
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      -- the renewal reports itself, no command having asked for it
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
      -- and the third, so this is a schedule rather than a single catch-up
      setClockAt bsClock $ dueAtOf renewed
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "1: supporter"
      renewed2 <- waitLedgerRows (chatController alice) 4
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed2
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge"), (-1, 0, Just "badge")]
      -- the client authored none of them: the service holds exactly the same rows
      serviceLedger <- ledgerRows cc "sx_badge_service_badge_ledger"
      renewed2 `shouldBe` serviceLedger
      -- and the profile shows the month last issued, not an earlier one
      (shown, issued) <- shownAndIssuedExpiry (chatController alice)
      shown `shouldBe` issued
      shown `shouldSatisfy` isJust

-- The newest row being of an unknown type must not stop the renewal: it is stored verbatim and
-- read back to be asserted, rather than leaving the client with no entry to assert at all.
testRenewsAfterUnknownEntry :: HasCallStack => TestParams -> IO ()
testRenewsAfterUnknownEntry ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      insertUnknownLedgerEntry (chatController alice)
      setClockAt bsClock $ dueAtOf rows
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      -- the unknown row is asserted, so the service answers with what it does not hold: the whole
      -- ledger, which re-stores the two rows already held and adds the month it issued
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (0, 2, Just "grant"), (-1, 1, Just "badge")]

-- When the balance is spent and the last period ends, the badge stops being shown and the profile
-- update reaches contacts - the visible half of "the badge expired".
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
        -- bob sees it before it expires
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
        -- the one month it bought has ended and nothing is left to issue
        setClockAt bsClock $ dueAtOf rows
        alice ##> "/_app activate"
        alice <## "ok"
        -- support ended, and the state it changed by retiring the badge carries the same alert
        alice <##. "badge alert: support_ended "
        alice <##. "1: supporter"
        alice <##. "badge alert: support_ended "
        -- the profile stops showing it locally
        waitShownBadge (chatController alice) Nothing
        alice ##> "/p"
        alice <## "user profile: alice (Alice)"
        alice <## "use /p <name> [<bio>] to change it"
        -- The removal travels as a profile update, which prints nothing when only the badge
        -- changed (viewContactUpdated compares names and links). The next message shows it
        -- arrived: bob's prefix loses the badge marker it carried above.
        alice #> "@bob after"
        bob <# "alice> after"
        -- and the badge is gone from the contact's stored profile, not merely from the prefix
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo

-- The alert is derived from stored state rather than kept pending, so it is still there after a
-- restart; acknowledging records the occurrence it answered, and the same one is not raised again.
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
      -- the pass raises the alert, and reports the state it changed by retiring the badge - which
      -- carries the same alert, the alert being part of the state
      alice <##. "badge alert: support_ended "
      alice <##. "1: supporter"
      alice <##. "badge alert: support_ended "
      pure endsAt
    -- nothing was stored as pending, and the alert is derived again on the next start
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      alice <##. "badge alert: support_ended "
      alice ##> ("/_badge ack 1 1 support_ended off " <> T.unpack (safeDecodeUtf8 $ strEncode endsAt))
      alice <##. "1: supporter"
      ackedEpisode (chatController alice) `shouldReturn` (Just "support_ended", Just (safeDecodeUtf8 $ strEncode endsAt))
      -- acknowledged: the state no longer carries the alert, and no event raises it again
      alice ##> "/_badge state 1"
      alice <##. "1: supporter"
      alice ##> "/_app activate"
      alice <## "ok"
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <name> [<bio>] to change it"

-- A worker runs for every profile, not only the one in use, so presenting a renewed badge must not
-- make its profile active - the next message would then be sent from the wrong identity.
testRenewalKeepsActiveProfile :: HasCallStack => TestParams -> IO ()
testRenewalKeepsActiveProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      -- alice's month falls due while alisa is the profile in use
      setClockAt bsClock $ dueAtOf rows
      alice ##> "/_app activate"
      alice <## "ok"
      void $ waitLedgerRows (chatController alice) 3
      -- the renewal is reported for alice, and the prefix is there because alice is not active
      alice <##. "[user: alice] 1: supporter"
      alice ##> "/p"
      showActiveUser alice "alisa"

-- A snooze silences the alert until it lapses, and then it is raised once more, without a restart.
-- A snooze changes nothing else on the purchase, so the occurrence already raised has to count it.
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
      -- snoozed, and the same worker keeps the occurrence it raised: /p prints only what a silent
      -- pass leaves behind
      alice ##> "/_app activate"
      alice <## "ok"
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <name> [<bio>] to change it"
      -- the snooze lapses: nothing else about the badge changed, so only the alert is reported
      setClockAt bsClock $ addUTCTime (nominalDay + 60) endsAt
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "badge alert: support_ended "

-- The worker re-reads the profile each pass. Presenting a renewed badge from a copy captured when
-- the worker started would revert any edit made since and broadcast the profile in its old form.
testRenewalKeepsProfileEdits :: HasCallStack => TestParams -> IO ()
testRenewalKeepsProfileEdits ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 3
        redeemFirstBadge alice code
        -- the profile is edited after the worker started
        alice ##> "/p alice Alice Jones"
        concurrentlyN_
          [ alice <## "user bio changed to Alice Jones (your 1 contacts are notified)",
            bob <## "contact alice updated bio: Alice Jones"
          ]
        rows <- ledgerRows (chatController alice) "badge_ledger"
        setClockAt bsClock $ dueAtOf rows
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "1: supporter"
        void $ waitLedgerRows (chatController alice) 3
        -- The renewal's profile update carries the edited bio. Had it carried the profile the
        -- worker started with, bob would print a bio change back to "Alice" here, before the
        -- message - so the message arriving next is the assertion.
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

-- Forces the state a crash between the issuance write and the profile write leaves behind: the
-- month is issued, unpresented, and no later pass finds it due.
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
        setClockAt bsClock $ dueAtOf rows
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "1: supporter"
        void $ waitLedgerRows (chatController alice) 3
        expiries <- issuedExpiries (chatController alice)
        length expiries `shouldBe` 2
        let firstMonth = head expiries
            latestMonth = last expiries
        -- the renewal's rows are kept; only its presentation is undone, on both sides
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
waitPeerBadgeExpiry cc expected = loop (600 :: Int)
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

-- A secret that is not the key clients trust at its index makes every credential unverifiable,
-- and each code redeemed against it is spent for good - so the service must not start at all.
testIssuerKeyMustMatchConfig :: HasCallStack => TestParams -> IO ()
testIssuerKeyMustMatchConfig ps = do
  Right (pk, sk) <- bbsKeyGen
  Right (_, otherSk) <- bbsKeyGen
  let optsFor sk' = mkBadgeServiceOpts ps sk'
      cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
  checkIssuerKey (optsFor sk) cfg >>= (`shouldSatisfy` isRight)
  checkIssuerKey (optsFor otherSk) cfg >>= (`shouldSatisfy` isLeft)
  -- an index no client trusts is equally fatal
  checkIssuerKey (optsFor sk) testCfg {badgePublicKeys = M.empty} >>= (`shouldSatisfy` isLeft)
