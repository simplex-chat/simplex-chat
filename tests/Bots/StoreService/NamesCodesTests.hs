{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesCodesTests (namesCodesTests) where

import Bots.StoreService.NamesStoreTests (order, ownerA, ownerB, placeCode, placePaid, usable)
import Bots.StoreService.WebTests (withServiceStore)
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Monad (forM_, replicateM_)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, Response, RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges.Code (NameCode (..), NamePrepaidCode, formatNameCode, namePrepaidCodeHash, parseNameCode, randomNamePrepaidCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import StoreService.Config (ListenerConfig (..))
import StoreService.Group.Command (CmdAction (..), GroupCmd (..), groupCmdAction)
import StoreService.Names.Codes
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (NewPrepaidCode (..), PrepaidCode (..), getPrepaidCode, insertPrepaidCode, reservePrepaidCode, revokePrepaidCode, spendPrepaidCode)
import StoreService.Names.Turns (Turns, newTurns)
import StoreService.Store (RevokeResult (..))
import StoreService.Web.Http (codeLimit, newHttpEnv, notFound)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Codes (codeCheckHandler)
import Test.Hspec

namesCodesTests :: Spec
namesCodesTests = describe "StoreService names: prepaid codes" $ do
  describe "state" $
    it "reads each state from the code's row, a spent code as used even if revoked while held" testCodeStates
  describe "check" $ do
    it "answers each state with what the code covers, and changes no code" testCheckStates
    it "folds a code typed in lower case with spaces to the issued one" testCheckFolded
    it "answers code_invalid to an unknown, mistyped, zero-padded, owner or badge code" testCheckInvalid
    it "answers bad_request to a body that is not an object with a code text, and 405 to GET" testCheckBadBody
    it "takes a code token for every check, whatever its outcome" testCheckLimit
  describe "reservations" $ do
    it "reserves an unused code after the registration's write, in its transaction" testReserve
    it "refuses each unusable code with its tag, before the write runs" testReserveRefusals
    it "keeps a code its registration holds even if revoked or expired since, still checking the length" testReserveHeld
    it "refuses a spent code to its own registration with code_used, before the write runs" testReserveSpent
    it "reserves nothing when the write refuses" testReserveWriteRefused
    it "rolls the write back when the code's reservation is refused after it" testReserveRollsBack
    it "lets one of two concurrent reservations of a code win, the other answered code_reserved" testConcurrentReserve
    it "releases and spends a code only after the write, and only for its holder" testReleaseSpend
  describe "group commands" $ do
    it "parses /issue name and /bulk name with the length, years and count" testGroupNameCommands
    it "answers the usage to a bad length, years or count, or a badge option" testGroupNameCommandsRefused

r1, r2 :: RegistrationId
r1 = RegistrationId "r1"
r2 = RegistrationId "r2"

tNow :: UTCTime
tNow = UTCTime (fromGregorian 2026 10 21) 3600

baseCode :: PrepaidCode
baseCode =
  PrepaidCode
    { pcId = 1,
      pcCodeHash = "",
      pcMinLength = 8,
      pcYears = 2,
      pcPaymentStatus = CPSPaid,
      pcReservedBy = Nothing,
      pcReservedAt = Nothing,
      pcSpentAt = Nothing,
      pcRevokedAt = Nothing,
      pcExpiresAt = Nothing,
      pcCreatedAt = tNow
    }

testCodeStates :: IO ()
testCodeStates = do
  let stateOf = codeState tNow
      held = baseCode {pcReservedBy = Just r1, pcReservedAt = Just tNow}
      revoked = Just tNow
  map codeStateText [minBound .. maxBound] `shouldBe` ["unused", "reserved", "used", "revoked", "expired", "unpaid"]
  stateOf baseCode `shouldBe` CSUnused
  stateOf baseCode {pcPaymentStatus = CPSFree, pcExpiresAt = Just (addUTCTime 1 tNow)} `shouldBe` CSUnused
  stateOf held `shouldBe` CSReserved
  stateOf held {pcSpentAt = Just tNow} `shouldBe` CSUsed
  stateOf held {pcSpentAt = Just tNow, pcRevokedAt = revoked} `shouldBe` CSUsed
  stateOf baseCode {pcRevokedAt = revoked} `shouldBe` CSRevoked
  stateOf held {pcRevokedAt = revoked} `shouldBe` CSRevoked
  stateOf baseCode {pcPaymentStatus = CPSUnpaid} `shouldBe` CSUnpaid
  stateOf baseCode {pcPaymentStatus = CPSUnpaid, pcExpiresAt = Just tNow} `shouldBe` CSUnpaid
  stateOf baseCode {pcExpiresAt = Just tNow} `shouldBe` CSExpired
  stateOf held {pcExpiresAt = Just tNow} `shouldBe` CSReserved

-- The route as namesRoutes registers it, since a NamesEnv needs a running chat controller.
withCodeCheck :: (DBStore -> Client -> IO a) -> IO a
withCodeCheck action = withServiceStore $ \st -> do
  http <- newHttpEnv ListenerConfig {lHost = "127.0.0.1", lBadgesPort = 0, lBadgesStaticDir = "", lServeWebapp = False, lBadgesExportDir = Nothing, lTrustForwardedFor = False, lNamesStaticDir = Nothing, lNamesExportDir = Nothing, lNamesPort = Nothing}
  let routes = \case
        ["api", "code", "check"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeCheckHandler st}
        _ -> Nothing
      app req respond = fromMaybe (respond notFound) (namesApp http routes req respond)
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port -> action st (mgr, "http://127.0.0.1:" <> show port)

type Client = (Manager, String)

postCheck :: Client -> LB.ByteString -> IO (Response LB.ByteString)
postCheck (mgr, base) body = do
  req <- parseRequest (base <> "/api/code/check")
  httpLbs req {method = "POST", requestBody = RequestBodyLBS body} mgr

checkText :: Client -> Text -> IO (Response LB.ByteString)
checkText client code = postCheck client (J.encode (J.object ["code" J..= code]))

checkCode :: Client -> NamePrepaidCode -> IO (Response LB.ByteString)
checkCode client = checkText client . formatNameCode . NCPrepaid

answerOf :: Response LB.ByteString -> (Int, Maybe J.Object)
answerOf r = (statusCode (responseStatus r), J.decode (responseBody r))

coverage :: Int -> Int -> Text -> (Int, Maybe J.Object)
coverage minLength years state = (200, Just $ KM.fromList [("minLength", J.toJSON minLength), ("years", J.toJSON years), ("state", J.String state)])

refused :: Int -> Text -> (Int, Maybe J.Object)
refused status tag = (status, Just $ KM.fromList [("error", J.String tag)])

newCode :: Int -> IO NamePrepaidCode
newCode len = C.newRandom >>= (`randomNamePrepaidCode` len)

issueAs :: DBStore -> NamePrepaidCode -> NewPrepaidCode -> IO PrepaidCode
issueAs st code new = withTransaction st $ \db -> do
  Right _ <- insertPrepaidCode db new {npcCodeHash = namePrepaidCodeHash code} tNow
  fromJust <$> getPrepaidCode db (namePrepaidCodeHash code)

readCode :: DBStore -> PrepaidCode -> IO (Maybe PrepaidCode)
readCode st PrepaidCode {pcCodeHash} = withTransaction st (`getPrepaidCode` pcCodeHash)

testCheckStates :: IO ()
testCheckStates = withCodeCheck $ \st client -> do
  now <- getCurrentTime
  [unused, reserved, used, revoked, expired, unpaid] <- mapM newCode [8, 6, 7, 9, 12, 63]
  unusedRow <- issueAs st unused usable {npcYears = 5, npcExpiresAt = Just (addUTCTime 3600 now)}
  reservedRow <- issueAs st reserved usable {npcMinLength = 6}
  usedRow <- issueAs st used usable {npcMinLength = 7}
  revokedRow <- issueAs st revoked usable {npcMinLength = 9, npcPaymentStatus = CPSFree}
  expiredRow <- issueAs st expired usable {npcMinLength = 12, npcExpiresAt = Just (addUTCTime (-60) now)}
  unpaidRow <- issueAs st unpaid usable {npcMinLength = 63, npcYears = 10, npcPaymentStatus = CPSUnpaid}
  placeCode st "r1" ownerA (order "alicebakery" 1) reservedRow `shouldReturn` Right ()
  placeCode st "r2" ownerA (order "bobsbakery" 2) usedRow `shouldReturn` Right ()
  withTransaction st (\db -> spendPrepaidCode db (pcId usedRow) r2 tNow) `shouldReturn` True
  withTransaction st (\db -> revokePrepaidCode db (pcCodeHash revokedRow) tNow) `shouldReturn` Revoked (pcId revokedRow)
  let rows = [unusedRow, reservedRow, usedRow, revokedRow, expiredRow, unpaidRow]
  rowsBefore <- mapM (readCode st) rows
  answerOf <$> checkCode client unused `shouldReturn` coverage 8 5 "unused"
  answerOf <$> checkCode client reserved `shouldReturn` coverage 6 2 "reserved"
  answerOf <$> checkCode client used `shouldReturn` coverage 7 2 "used"
  answerOf <$> checkCode client revoked `shouldReturn` coverage 9 2 "revoked"
  answerOf <$> checkCode client expired `shouldReturn` coverage 12 2 "expired"
  answerOf <$> checkCode client unpaid `shouldReturn` coverage 63 10 "unpaid"
  mapM (readCode st) rows `shouldReturn` rowsBefore

-- The webapp's and core's vector: these two spellings are one code.
testCheckFolded :: IO ()
testCheckFolded = withCodeCheck $ \st client -> do
  Just (NCPrepaid code) <- pure $ parseNameCode "SN7-BJTBP-29596-R3HJ8-25YM7"
  _ <- issueAs st code usable {npcMinLength = 7}
  answerOf <$> checkText client "sn7 bjtbp 29596 r3hj8 25ym7" `shouldReturn` coverage 7 2 "unused"

testCheckInvalid :: IO ()
testCheckInvalid = withCodeCheck $ \st client -> do
  Just (NCPrepaid issued) <- pure $ parseNameCode "SN7-BJTBP-29596-R3HJ8-25YM7"
  _ <- issueAs st issued usable {npcMinLength = 7}
  unknown <- newCode 8
  let invalid = refused 400 "code_invalid"
  answerOf <$> checkCode client unknown `shouldReturn` invalid
  forM_
    [ "SN7-BJTBP-29596-R3HJ8-25YM8",
      "SN07-BJTBP-29596-R3HJ8-25YM7",
      "SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5",
      "SB-00000-00000-00000-00000",
      "alicebakery",
      ""
    ]
    $ \text -> (text,) . answerOf <$> checkText client text `shouldReturn` (text, invalid)

testCheckBadBody :: IO ()
testCheckBadBody = withCodeCheck $ \_ client@(mgr, base) -> do
  let badRequest = refused 400 "bad_request"
  forM_ ["", "not json", "[\"SN7-BJTBP-29596-R3HJ8-25YM7\"]", "{}", "{\"code\": 7}", "{\"code\": null}"] $ \body ->
    (body,) . answerOf <$> postCheck client body `shouldReturn` (body, badRequest)
  let overCap = "{\"code\": \"SN7-BJTBP-29596-R3HJ8-25YM7\", \"pad\": \"" <> LB.replicate 8192 'x' <> "\"}"
  answerOf <$> postCheck client overCap `shouldReturn` badRequest
  req <- parseRequest (base <> "/api/code/check")
  r <- httpLbs req mgr
  statusCode (responseStatus r) `shouldBe` 405
  lookup "Allow" (responseHeaders r) `shouldBe` Just "POST"
  lookup hContentType (responseHeaders r) `shouldBe` Just "application/json"

testCheckLimit :: IO ()
testCheckLimit = withCodeCheck $ \st client -> do
  issued <- newCode 8
  _ <- issueAs st issued usable
  unknown <- newCode 8
  replicateM_ 5 $ checkCode client issued >>= (`shouldBe` 200) . fst . answerOf
  replicateM_ 4 $ checkCode client unknown >>= (`shouldBe` 400) . fst . answerOf
  postCheck client "{}" >>= (`shouldBe` 400) . fst . answerOf
  limited <- checkCode client issued
  answerOf limited `shouldBe` refused 429 "rate_limited"
  lookup "Retry-After" (responseHeaders limited) `shouldSatisfy` maybe False (BC.all (`elem` ['0' .. '9']))

-- | Two paid orders, so a code can be reserved for either.
withOrders :: (Turns -> DBStore -> IO a) -> IO a
withOrders action = withServiceStore $ \st -> do
  placePaid st "r1" ownerA (order "alicebakery" 1) tNow `shouldReturn` Right ()
  placePaid st "r2" ownerB (order "bobsbakery" 2) tNow `shouldReturn` Right ()
  turns <- newTurns
  action turns st

writeOk :: DB.Connection -> IO (Either Text ())
writeOk _ = pure $ Right ()

testReserve :: IO ()
testReserve = withOrders $ \turns st -> do
  code <- newCode 8
  PrepaidCode {pcCodeHash} <- issueAs st code usable
  let holderInWrite db = Right . (pcReservedBy =<<) <$> getPrepaidCode db pcCodeHash :: IO (Either Text (Maybe RegistrationId))
  reserveCode turns st tNow pcCodeHash r1 8 holderInWrite `shouldReturn` Right (Right Nothing)
  (pcReservedBy =<<) <$> withTransaction st (`getPrepaidCode` pcCodeHash) `shouldReturn` Just r1
  reserveCode turns st tNow pcCodeHash r1 11 holderInWrite `shouldReturn` Right (Right (Just r1))

testReserveRefusals :: IO ()
testReserveRefusals = withOrders $ \turns st -> do
  [reserved, used, revoked, expired, unpaid, unknown, short] <- mapM newCode [8, 8, 8, 8, 8, 8, 8]
  reservedRow <- issueAs st reserved usable
  usedRow <- issueAs st used usable
  revokedRow <- issueAs st revoked usable
  expiredRow <- issueAs st expired usable {npcExpiresAt = Just tNow}
  unpaidRow <- issueAs st unpaid usable {npcPaymentStatus = CPSUnpaid}
  shortRow <- issueAs st short usable
  placeCode st "r5" ownerA (order "erinsbakery" 5) reservedRow `shouldReturn` Right ()
  placeCode st "r3" ownerB (order "carolsbakery" 4) usedRow `shouldReturn` Right ()
  withTransaction st (\db -> spendPrepaidCode db (pcId usedRow) (RegistrationId "r3") tNow) `shouldReturn` True
  withTransaction st (\db -> revokePrepaidCode db (pcCodeHash revokedRow) tNow) `shouldReturn` Revoked (pcId revokedRow)
  wrote <- newIORef False
  let reserve hash len = reserveCode turns st tNow hash r1 len (\_ -> Right () <$ writeIORef wrote True :: IO (Either Text ()))
      rows = [reservedRow, usedRow, revokedRow, expiredRow, unpaidRow, shortRow]
  rowsBefore <- mapM (readCode st) rows
  reserve (pcCodeHash reservedRow) 8 `shouldReturn` Left CRReserved
  reserve (pcCodeHash usedRow) 8 `shouldReturn` Left CRUsed
  reserve (pcCodeHash revokedRow) 8 `shouldReturn` Left CRInvalid
  reserve (pcCodeHash expiredRow) 8 `shouldReturn` Left CRExpired
  reserve (pcCodeHash unpaidRow) 8 `shouldReturn` Left CRPaymentPending
  reserve (namePrepaidCodeHash unknown) 8 `shouldReturn` Left CRInvalid
  reserve (pcCodeHash shortRow) 7 `shouldReturn` Left CRNotCovering
  reserve (pcCodeHash revokedRow) 7 `shouldReturn` Left CRInvalid
  readIORef wrote `shouldReturn` False
  mapM (readCode st) rows `shouldReturn` rowsBefore
  map codeRefusalTag [CRInvalid, CRUsed, CRReserved, CRExpired, CRPaymentPending, CRNotCovering]
    `shouldBe` ["code_invalid", "code_used", "code_reserved", "code_expired", "payment_pending", "code_not_covering"]

testReserveHeld :: IO ()
testReserveHeld = withOrders $ \turns st -> do
  code <- newCode 8
  row@PrepaidCode {pcCodeHash} <- issueAs st code usable
  placeCode st "r3" ownerB (order "carolsbakery" 3) row `shouldReturn` Right ()
  let r3 = RegistrationId "r3"
  withTransaction st (\db -> revokePrepaidCode db pcCodeHash tNow) `shouldReturn` Revoked (pcId row)
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_name_prepaid_codes SET expires_at = ? WHERE code_hash = ?" (tNow, DB.Binary pcCodeHash)
  held <- readCode st row
  reserveCode turns st tNow pcCodeHash r3 9 writeOk `shouldReturn` Right (Right ())
  reserveCode turns st tNow pcCodeHash r3 7 writeOk `shouldReturn` Left CRNotCovering
  reserveCode turns st tNow pcCodeHash r1 9 writeOk `shouldReturn` Left CRInvalid
  readCode st row `shouldReturn` held

testReserveSpent :: IO ()
testReserveSpent = withOrders $ \turns st -> do
  code <- newCode 8
  row@PrepaidCode {pcCodeHash} <- issueAs st code usable
  placeCode st "r3" ownerB (order "carolsbakery" 3) row `shouldReturn` Right ()
  let r3 = RegistrationId "r3"
  withTransaction st (\db -> spendPrepaidCode db (pcId row) r3 tNow) `shouldReturn` True
  spent <- readCode st row
  wrote <- newIORef False
  reserveCode turns st tNow pcCodeHash r3 8 (\_ -> Right () <$ writeIORef wrote True :: IO (Either Text ())) `shouldReturn` Left CRUsed
  readIORef wrote `shouldReturn` False
  readCode st row `shouldReturn` spent

testReserveWriteRefused :: IO ()
testReserveWriteRefused = withOrders $ \turns st -> do
  code <- newCode 8
  row@PrepaidCode {pcCodeHash} <- issueAs st code usable
  reserveCode turns st tNow pcCodeHash r1 8 (\_ -> pure (Left "registration_changed" :: Either Text ())) `shouldReturn` Right (Left "registration_changed")
  (pcReservedBy =<<) <$> readCode st row `shouldReturn` Nothing
  reserveCode turns st tNow pcCodeHash r2 8 writeOk `shouldReturn` Right (Right ())
  (pcReservedBy =<<) <$> readCode st row `shouldReturn` Just r2

-- | The write reserves the code for another registration, as a writer outside this process could between the check and the reservation.
testReserveRollsBack :: IO ()
testReserveRollsBack = withOrders $ \turns st -> do
  code <- newCode 8
  row@PrepaidCode {pcId, pcCodeHash} <- issueAs st code usable
  let takenMeanwhile db = Right () <$ reservePrepaidCode db pcId r2 tNow :: IO (Either Text ())
  reserveCode turns st tNow pcCodeHash r1 8 takenMeanwhile `shouldReturn` Left CRReserved
  readCode st row `shouldReturn` Just row

testConcurrentReserve :: IO ()
testConcurrentReserve = withOrders $ \turns st -> do
  code <- newCode 8
  row@PrepaidCode {pcCodeHash} <- issueAs st code usable
  let slowWrite _ = Right () <$ threadDelay 100000 :: IO (Either Text ())
      reserveFor rid = reserveCode turns st tNow pcCodeHash rid 8 slowWrite
  outcomes <- concurrently (reserveFor r1) (reserveFor r2)
  outcomes `shouldSatisfy` (`elem` [(Right (Right ()), Left CRReserved), (Left CRReserved, Right (Right ()))])
  holder <- (pcReservedBy =<<) <$> readCode st row
  holder `shouldBe` Just (if fst outcomes == Right (Right ()) then r1 else r2)

testReleaseSpend :: IO ()
testReleaseSpend = withOrders $ \turns st -> do
  [spent, released] <- mapM newCode [8, 8]
  spentRow <- issueAs st spent usable
  releasedRow <- issueAs st released usable
  placeCode st "r3" ownerB (order "carolsbakery" 3) spentRow `shouldReturn` Right ()
  placeCode st "r4" ownerB (order "davesbakery" 4) releasedRow `shouldReturn` Right ()
  let r3 = RegistrationId "r3"
      r4 = RegistrationId "r4"
      holderInWrite row db = Right . (pcReservedBy =<<) <$> getPrepaidCode db (pcCodeHash row) :: IO (Either Text (Maybe RegistrationId))
      changed _ = pure (Left "registration_changed") :: IO (Either Text ())
  spendCode turns st tNow (pcId spentRow) r3 changed `shouldReturn` Left "registration_changed"
  (pcSpentAt =<<) <$> readCode st spentRow `shouldReturn` Nothing
  spendCode turns st tNow (pcId spentRow) r1 writeOk `shouldReturn` Right ((), False)
  spendCode turns st tNow (pcId spentRow) r3 writeOk `shouldReturn` Right ((), True)
  ((pcReservedBy =<<) &&& (pcSpentAt =<<)) <$> readCode st spentRow `shouldReturn` (Just r3, Just tNow)
  releaseCode turns st (pcId spentRow) r3 writeOk `shouldReturn` Right ((), False)
  releaseCode turns st (pcId releasedRow) r4 changed `shouldReturn` Left "registration_changed"
  releaseCode turns st (pcId releasedRow) r1 writeOk `shouldReturn` Right ((), False)
  releaseCode turns st (pcId releasedRow) r4 (holderInWrite releasedRow) `shouldReturn` Right (Just r4, True)
  (pcReservedBy =<<) <$> readCode st releasedRow `shouldReturn` Nothing

testGroupNameCommands :: IO ()
testGroupNameCommands = do
  let p = groupCmdAction GRModerator
  p "/issue name 8" `shouldBe` RunCmd (GCIssueName 8 2)
  p "/issue name 6 years 5" `shouldBe` RunCmd (GCIssueName 6 5)
  p "/issue name 63 years 10" `shouldBe` RunCmd (GCIssueName 63 10)
  p "/bulk name 7 count 3" `shouldBe` RunCmd (GCBulkName 7 2 3)
  p "/bulk name 12 years 3 count 100" `shouldBe` RunCmd (GCBulkName 12 3 100)
  p "/bulk name 6 count 100" `shouldBe` RunCmd (GCBulkName 6 2 100)
  groupCmdAction GRMember "/issue name 8" `shouldBe` IgnoreMsg

testGroupNameCommandsRefused :: IO ()
testGroupNameCommandsRefused = do
  let p = groupCmdAction GRModerator
      issueUsage = ReplyText "Usage: /issue <kind> [<length>] [years <Y> | months <M>] [uses <N>]\nname <length>: length 6–63, years 2–10\nsupporter, legend, investor: months 1–255, uses 1–1000\ne.g. /issue name 6 years 5 · /issue supporter months 12"
      bulkUsage = ReplyText "Usage: /bulk <kind> [<length>] [years <Y> | months <M>] count <N>\nname <length>: length 6–63, years 2–10, count 1–100\nsupporter, legend, investor: months 1–255, count 1–100\ne.g. /bulk name 6 count 100 · /bulk legend months 3 count 10"
  forM_ ["name", "name 5", "name 64", "name 06", "name 8 years 1", "name 8 years 0", "name 8 years 11", "name 8 uses 2", "name 8 months 2", "name 8 paid", "name 8 years 18446744073709551618", "name 8 count 2", "name 8 count 1"] $ \args ->
    (args, p ("/issue " <> args)) `shouldBe` (args, issueUsage)
  forM_ ["name 8", "name 8 count 0", "name 8 count 101", "name 8 years 1 count 2", "name 5 count 2", "name 08 count 2", "name 8 months 2 count 2", "name 8 count 2 uses 2", "name 8 uses 2 count 2", "name 8 count 2 years 5", "name count 2"] $ \args ->
    (args, p ("/bulk " <> args)) `shouldBe` (args, bulkUsage)
