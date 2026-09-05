{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeServiceTests where

import BadgeService.Config (readServiceConfig)
import Bots.BadgeConfigTests (withIssuer)
import BadgeService.Options
import BadgeService.Service
import BadgeService.Store (CodeRedemption (..), IssuedCode (..), RedeemedCode (..), getBadgeCode)
import BadgeService.Store.Invoices (markCodePaid)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, readTMVar)
import Control.Exception (finally)
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeType (..))
import Simplex.Chat.Badges.Code (BadgeCode, badgeCodeHash, badgeCodeText, formatBadgeCode, parseBadgeCode, randomBadgeCode)
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller (ChatConfig (..), ChatController, ChatResponse (CRCustomChatResponse))
import Simplex.Chat.Core (sendChatCmdStr)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSSecretKey, bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode, strEncode, textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import System.FilePath ((</>))
import Test.Hspec hiding (it)

badgeServiceTests :: SpecWith TestParams
badgeServiceTests = do
  it "should answer unsupported_version to unsupported command" testBadgeServiceUnsupported
  it "should redeem an issued code into a badge a contact sees" testRedeemBadgeCode
  it "should return the same badge when the same code is redeemed twice" testRedeemBadgeCodeTwice
  it "should answer code_invalid to an unknown code, indistinguishably from a malformed one" testRedeemUnknownCode
  it "should tell a second profile redeeming the same code that it is used" testRedeemSameCodeOtherProfile
  it "should redeem a second code, and not restore the first badge on replay" testRedeemSecondCode
  it "should refuse a code that has not been paid for" testRedeemUnpaidCode
  it "should grant the months the code was sold for" testRedeemGrantsTheMonthsSold
  it "should refuse a badge code past its redemption deadline" testExpiredCode
  it "should keep answering a code redeemed before its deadline" testRedeemedBeforeTheDeadline
  it "should refuse a revoked badge code, and refuse to revoke it twice" testRevokedCode
  it "should refuse to issue a code with an unknown badge type or a nonsense month count" testIssueRejectsBadArguments
  it "should refuse a request whose purchaseKey is not the verified signer" testPurchaseKeyMismatch
  it "should refuse to start unless the issuer secret is the key trusted at its index" testIssuerKeyMustMatchConfig
  it "should refuse to start when any key in [issuer] is one no client trusts" testIssuerIniKeysAreAllChecked

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

-- | Start the badge service on a fresh issuer key, and hand the test body what depends on it:
-- the client config trusting that key and addressing the service, the address, and the controller.
withBadgeService :: HasCallStack => TestParams -> (ChatConfig -> String -> ChatController -> IO ()) -> IO ()
withBadgeService ps test = do
  Right (pk, sk) <- bbsKeyGen
  let opts = mkBadgeServiceOpts ps sk
      -- the service refuses to start unless its secret is the key trusted at its index
      svcCfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
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
    test clientCfg bsLink cc

-- through the operator command the service actually exposes, not the function behind it
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

runBadgeService :: ChatConfig -> BadgeServiceOpts -> (ServiceState -> IO ()) -> IO ()
runBadgeService cfg opts action = do
  env <- newServiceState
  t <- forkIO $ badgeService opts cfg env
  threadDelay 500000
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
    refuses "supporter 18446744073709551617"
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
  checkIssuerKey (optsFor sk) Nothing cfg >>= (`shouldSatisfy` isRight)
  checkIssuerKey (optsFor otherSk) Nothing cfg >>= (`shouldSatisfy` isLeft)
  -- an index no client trusts is equally fatal
  checkIssuerKey (optsFor sk) Nothing testCfg {badgePublicKeys = M.empty} >>= (`shouldSatisfy` isLeft)
  -- half the pair is a mistake, not a request to fall back to the ini: signing with the key
  -- someone meant to replace is the failure this refuses
  let halfGiven = (optsFor sk) {issuerKey = Left "--issuer-key-idx and --issuer-secret are given together or not at all"}
  checkIssuerKey halfGiven Nothing cfg >>= (`shouldSatisfy` isLeft)

-- Every key in [issuer] is checked, not only the one that signs: a key listed for a future
-- rotation that clients could not verify has to fail now, while someone is reading the error,
-- rather than at the restart that rotates onto it.
testIssuerIniKeysAreAllChecked :: HasCallStack => TestParams -> IO ()
testIssuerIniKeysAreAllChecked ps = do
  Right (pk, sk) <- bbsKeyGen
  Right (_, untrusted) <- bbsKeyGen
  let cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
      -- no key on the command line, so the ini section is what is read
      fromIni = (mkBadgeServiceOpts ps sk) {issuerKey = Right Nothing}
      secretText k = safeDecodeUtf8 (strEncode k)
      iniWith keys = withIssuer (("default = key_" <> tshow testIssuerKeyIdx) : keys) readServiceConfig
  Right onlyTheSigner <- iniWith ["key_" <> tshow testIssuerKeyIdx <> " = " <> secretText sk]
  checkIssuerKey fromIni (Just onlyTheSigner) cfg >>= (`shouldSatisfy` isRight)

  Right alsoARotation <-
    iniWith
      [ "key_" <> tshow testIssuerKeyIdx <> " = " <> secretText sk,
        "key_9 = " <> secretText untrusted
      ]
  checkIssuerKey fromIni (Just alsoARotation) cfg >>= (`shouldSatisfy` isLeft)

-- The web checkout writes the code row when the invoice is created, and settlement is what
-- marks it paid. Redeeming in between would hand out a badge nobody paid for.
testRedeemUnpaidCode :: HasCallStack => TestParams -> IO ()
testRedeemUnpaidCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      unpaid <- issueCodeAs cc BTSupporter 1 "unpaid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg unpaid)
      alice <## "bad chat command: badge service error: payment_pending"
      -- and it redeems once it is paid for, so this is the payment check and not a broken code
      paid <- issueCodeAs cc BTSupporter 1 "paid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg paid)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "

-- A span rather than a date: the badge expiry is rounded up to a Sunday, and the service reads
-- its own clock rather than this test's.
testRedeemGrantsTheMonthsSold :: HasCallStack => TestParams -> IO ()
testRedeemGrantsTheMonthsSold ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      now <- getCurrentTime
      code <- issueCodeAs cc BTSupporter 12 "paid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      Right (Just IssuedCode {redemption = CodeRedeemed RedeemedCode {credential}}) <-
        withDB' "getBadgeCode" cc (`getBadgeCode` badgeCodeHash code)
      let days = diffUTCTime (badgeExpiry (badgeInfo credential)) now / 86400
      days `shouldSatisfy` (> 300)
      days `shouldSatisfy` (< 400)

-- | The deadline is on redeeming, and a year is not something a test can wait for: the code is
-- issued unpaid, then marked paid with a deadline already behind it.
testExpiredCode :: HasCallStack => TestParams -> IO ()
testExpiredCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCodeAs cc BTSupporter 1 "unpaid"
      now <- getCurrentTime
      withDB' "markCodePaid" cc (\db -> markCodePaid db (badgeCodeHash code) (addUTCTime (-60) now))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "bad chat command: badge service error: code_expired"

-- | The deadline is on redeeming, not on holding. A buyer who redeemed in time must not lose the
-- credential when the deadline passes, so the already-redeemed answer has to be reached before
-- the expiry refusal rather than after it.
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

      -- the deadline passes with the code already redeemed. Written directly: `markCodePaid` sets
      -- the expiry only on the unpaid-to-paid transition, which this code has already made.
      withDB' "expireCode" cc (\db ->
        DB.execute
          db
          "UPDATE sx_badge_service_badge_codes SET expires_at = ? WHERE code_hash = ?"
          (addUTCTime (-60) now, Binary (badgeCodeHash code)))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"

testRevokedCode :: HasCallStack => TestParams -> IO ()
testRevokedCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      paid <- issueCodeAs cc BTSupporter 1 "paid"
      revokeCodeAs cc paid `shouldReturn` "revoked"
      -- a revoked code answers as if it never existed, so its holder learns nothing
      alice ##> ("/_redeem_badge_code 1 " <> codeArg paid)
      alice <## "bad chat command: badge service error: code_invalid"
      -- and revoking is not repeatable, so a second operator sees that it was already done
      second <- revokeCodeAs cc paid
      second `shouldSatisfy` T.isInfixOf "revoked already"
