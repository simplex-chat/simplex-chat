{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesCodesTests (namesCodesTests) where

import Bots.StoreService.NamesStoreTests (order, ownerA, ownerB, placeCode, placePaid, usable)
import Bots.StoreService.WebTests (answerOf, lengthOf, refusedWith, responseFixture, withServiceStore, withApp)
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Network.HTTP.Client (Manager, Response, RequestBody (..), httpLbs, method, parseRequest, requestBody)
import Simplex.Chat.Badges.Code (NamePrepaidCode, formatNameCode, parseNameCode, randomNamePrepaidCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import StoreService.Config (ListenerConfig (..))
import StoreService.Group.Command (CmdAction (..), GroupCmd (..), consoleCmd, groupCmdAction)
import StoreService.Names.Chain (Hex32 (..))
import StoreService.Names.Codes
import StoreService.Names.Orders (OrderRefusal (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (NewPrepaidCode (..), PrepaidCode (..), getPrepaidCode, insertPrepaidCode)
import StoreService.Names.Turns (Turns, newTurns)
import StoreService.Web.Http (codeLimit, newHttpEnv, notFound)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Codes (codeCheckHandler)
import StoreService.Web.Names.Orders (orderRefusal)
import Test.Hspec

namesCodesTests :: Spec
namesCodesTests = describe "StoreService names: prepaid codes" $ do
  describe "state" $
    it "reads each state from the code's row, a spent code as used even if revoked while held, and one a taken order holds as expired past its expiry and the month after the take" testCodeStates
  describe "check" $ do
    it "folds a code typed in lower case with spaces to the issued one, and changes no code" testCheckFolded
    it "answers code_invalid to an unknown code, an owner code or other text" testCheckInvalid
    it "answers bad_request to a body that is not an object with a code text" testCheckBadBody
  describe "reservations" $ do
    it "encodes each refusal of a code by its tag" testCodeRefusalTags
    it "releases and spends a code only after the write, and only for its holder" testReleaseSpend
  describe "group commands" $ do
    it "parses /issue name with its length and years, answering the usage to /bulk name" testGroupNameCommands
    it "answers the usage to a bad length, years or count, or a badge option" testGroupNameCommandsRefused
    it "parses /revoke of a prepaid code, telling a moderator it was not revoked" testGroupRevokeNameCommand

r1 :: RegistrationId
r1 = RegistrationId "r1"

tNow :: UTCTime
tNow = UTCTime (fromGregorian 2026 10 21) 3600

baseCode :: PrepaidCode
baseCode =
  PrepaidCode
    { pcId = 1,
      pcCodeHash = Hex32 "",
      pcMinLength = 8,
      pcYears = 2,
      pcPaymentStatus = CPSPaid,
      pcReservedBy = Nothing,
      pcReservedAt = Nothing,
      pcHolderTakenAt = Nothing,
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
  map textEncode [minBound .. maxBound :: CodeState] `shouldBe` (["unused", "reserved", "used", "revoked", "expired", "unpaid"] :: [Text])
  map textDecode ["unused", "unpaid", "spent"] `shouldBe` [Just CSUnused, Just CSUnpaid, Nothing]
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
  let takenHeld at = held {pcHolderTakenAt = Just at}
      monthAgo = UTCTime (fromGregorian 2026 9 21) 3600
  stateOf (takenHeld monthAgo) `shouldBe` CSReserved
  stateOf (takenHeld monthAgo) {pcExpiresAt = Just tNow} `shouldBe` CSExpired
  stateOf (takenHeld $ addUTCTime 1 monthAgo) {pcExpiresAt = Just tNow} `shouldBe` CSReserved

-- The route as namesRoutes registers it, since a NamesEnv needs a running chat controller.
withCodeCheck :: (DBStore -> Client -> IO a) -> IO a
withCodeCheck action = withServiceStore $ \st -> do
  http <- newHttpEnv ListenerConfig {lHost = "127.0.0.1", lBadgesPort = 0, lBadgesSite = Nothing, lTrustForwardedFor = False, lNamesSite = Nothing, lMetricsPort = Nothing}
  let routes = \case
        ["api", "code", "check"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeCheckHandler st}
        _ -> Nothing
      app req respond = fromMaybe (respond notFound) (namesApp http routes req respond)
  withApp app (action st)

type Client = (Manager, String)

postCheck :: Client -> LB.ByteString -> IO (Response LB.ByteString)
postCheck (mgr, base) body = do
  req <- parseRequest (base <> "/api/code/check")
  httpLbs req {method = "POST", requestBody = RequestBodyLBS body} mgr

checkText :: Client -> Text -> IO (Response LB.ByteString)
checkText client code = postCheck client (J.encode (J.object ["code" J..= code]))

checkCode :: Client -> NamePrepaidCode -> IO (Response LB.ByteString)
checkCode client = checkText client . formatNameCode

coverage :: Int -> Int -> Text -> (Int, Maybe J.Object)
coverage minLength years state = (200, Just $ KM.fromList [("minLength", J.toJSON minLength), ("years", J.toJSON years), ("state", J.String state)])

newCode :: Int -> IO NamePrepaidCode
newCode len = C.newRandom >>= (`randomNamePrepaidCode` lengthOf len)

issueAs :: DBStore -> NamePrepaidCode -> NewPrepaidCode -> IO PrepaidCode
issueAs st code new = withTransaction st $ \db -> do
  Right _ <- insertPrepaidCode db new {npcCodeHash = prepaidCodeHash code} tNow
  fromJust <$> getPrepaidCode db (prepaidCodeHash code)

readCode :: DBStore -> PrepaidCode -> IO (Maybe PrepaidCode)
readCode st PrepaidCode {pcCodeHash} = withTransaction st (`getPrepaidCode` pcCodeHash)

-- The webapp's and core's vector: these two spellings are one code.
testCheckFolded :: IO ()
testCheckFolded = withCodeCheck $ \st client -> do
  Just code <- pure $ parseNameCode "SN7-BJTBP-29596-R3HJ8-25YM7"
  row <- issueAs st code usable {npcMinLength = 7}
  unchanged <- readCode st row
  checked <- checkText client "sn7 bjtbp 29596 r3hj8 25ym7"
  responseFixture "code-unused" checked
  answerOf checked `shouldBe` coverage 7 2 "unused"
  readCode st row `shouldReturn` unchanged

testCheckInvalid :: IO ()
testCheckInvalid = withCodeCheck $ \_ client -> do
  unknown <- newCode 8
  let invalid = refusedWith 400 "code_invalid"
  refused <- checkCode client unknown
  responseFixture "code-invalid" refused
  answerOf refused `shouldBe` invalid
  forM_
    [ "SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5",
      "alicebakery"
    ]
    $ \text -> (text,) . answerOf <$> checkText client text `shouldReturn` (text, invalid)

testCheckBadBody :: IO ()
testCheckBadBody = withCodeCheck $ \_ client -> do
  let badRequest = refusedWith 400 "bad_request"
  forM_ ["", "not json", "[\"SN7-BJTBP-29596-R3HJ8-25YM7\"]", "{}", "{\"code\": 7}", "{\"code\": null}"] $ \body ->
    (body,) . answerOf <$> postCheck client body `shouldReturn` (body, badRequest)
  let overCap = "{\"code\": \"SN7-BJTBP-29596-R3HJ8-25YM7\", \"pad\": \"" <> LB.replicate 8192 'x' <> "\"}"
  answerOf <$> postCheck client overCap `shouldReturn` badRequest

-- | Two paid orders, so a code can be reserved for either.
withOrders :: (Turns -> DBStore -> IO a) -> IO a
withOrders action = withServiceStore $ \st -> do
  placePaid st "r1" ownerA (order "alicebakery" 1) tNow `shouldReturn` Right ()
  placePaid st "r2" ownerB (order "bobsbakery" 2) tNow `shouldReturn` Right ()
  turns <- newTurns
  action turns st

writeOk :: DB.Connection -> IO (Either Text ())
writeOk _ = pure $ Right ()

testCodeRefusalTags :: IO ()
testCodeRefusalTags =
  map (J.toJSON . orderRefusal . ORCode) [CRInvalid, CRUsed, CRReserved, CRExpired, CRPaymentPending, CRNotCovering]
    `shouldBe` map (\t -> J.object ["type" J..= (t :: Text)]) ["code_invalid", "code_used", "code_reserved", "code_expired", "payment_pending", "code_not_covering"]

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
  spendLandedCode turns st tNow (pcId spentRow) r3 changed `shouldReturn` Left "registration_changed"
  (pcSpentAt =<<) <$> readCode st spentRow `shouldReturn` Nothing
  spendLandedCode turns st tNow (pcId spentRow) r1 writeOk `shouldReturn` Right ((), False)
  spendLandedCode turns st tNow (pcId spentRow) r3 writeOk `shouldReturn` Right ((), True)
  ((pcReservedBy =<<) &&& (pcSpentAt =<<)) <$> readCode st spentRow `shouldReturn` (Just r3, Just tNow)
  releaseCode turns st (pcId spentRow) r3 writeOk `shouldReturn` Right ((), False)
  releaseCode turns st (pcId releasedRow) r4 changed `shouldReturn` Left "registration_changed"
  releaseCode turns st (pcId releasedRow) r1 writeOk `shouldReturn` Right ((), False)
  releaseCode turns st (pcId releasedRow) r4 (holderInWrite releasedRow) `shouldReturn` Right (Just r4, True)
  (pcReservedBy =<<) <$> readCode st releasedRow `shouldReturn` Nothing

testGroupNameCommands :: IO ()
testGroupNameCommands = do
  let p = groupCmdAction GRAdmin
  p "/issue name 8" `shouldBe` RunCmd (GCIssueName (lengthOf 8) 2 CPSFree Nothing)
  p "/issue name 63" `shouldBe` RunCmd (GCIssueName (lengthOf 63) 2 CPSFree Nothing)
  p "/issue name 6 years 2" `shouldBe` RunCmd (GCIssueName (lengthOf 6) 2 CPSFree Nothing)
  p "/issue name 63 years 10" `shouldBe` RunCmd (GCIssueName (lengthOf 63) 10 CPSFree Nothing)
  p "/issue name 8 until 2027-03-31" `shouldBe` RunCmd (GCIssueName (lengthOf 8) 2 CPSFree (Just $ UTCTime (fromGregorian 2027 3 31) 86399))
  p "/issue name 6 years 5 until 2030-12-31" `shouldBe` RunCmd (GCIssueName (lengthOf 6) 5 CPSFree (Just $ UTCTime (fromGregorian 2030 12 31) 86399))
  consoleCmd "issue name 7 years 3 paid until 2027-03-31" `shouldBe` Right (GCIssueName (lengthOf 7) 3 CPSPaid (Just $ UTCTime (fromGregorian 2027 3 31) 86399))
  map p ["/bulk name 7 count 3", "/bulk name 12 years 2 count 100", "/bulk name 6 count 1"] `shouldBe` replicate 3 bulkUsage
  groupCmdAction GRMember "/issue name 8" `shouldBe` IgnoreMsg

testGroupRevokeNameCommand :: IO ()
testGroupRevokeNameCommand = do
  code <- newCode 8
  let cmd = "/revoke " <> formatNameCode code
  groupCmdAction GRAdmin cmd `shouldBe` RunCmd (GCRevokeName code)
  groupCmdAction GRModerator cmd `shouldBe` ReplyText "Only admins can revoke codes. This code is now visible to all members."
  groupCmdAction GRAdmin (cmd <> " junk") `shouldBe` ReplyText "Usage: /revoke <code>"

testGroupNameCommandsRefused :: IO ()
testGroupNameCommandsRefused = do
  let p = groupCmdAction GRModerator
  forM_ ["name", "name 5", "name 64", "name 06", "name 8 years 1", "name 8 years 0", "name 8 years 11", "name 8 uses 2", "name 8 months 2", "name 8 paid", "name 8 years 18446744073709551618", "name 8 count 2", "name 8 count 1", "name 8 until", "name 8 until 2027-13-01", "name 8 until 31-03-2027", "name 8 until 2027-03-31 years 2", "supporter until 2027-03-31"] $ \args ->
    (args, p ("/issue " <> args)) `shouldBe` (args, issueUsage)
  forM_ ["name 8", "name 8 count 0", "name 8 count 101", "name 8 years 1 count 2", "name 5 count 2", "name 08 count 2", "name 8 months 2 count 2", "name 8 count 2 uses 2", "name 8 uses 2 count 2", "name 8 count 2 years 5", "name count 2"] $ \args ->
    (args, p ("/bulk " <> args)) `shouldBe` (args, bulkUsage)

issueUsage, bulkUsage :: CmdAction
issueUsage = ReplyText "Usage: /issue <kind> [<length>] [years <Y> | months <M>] [uses <N>] [until <YYYY-MM-DD>]\nname <length>: length 6–63, years 2–10, until <YYYY-MM-DD>\nsupporter, legend, investor: months 1–12, uses 1–1000\ne.g. /issue name 6 years 5 · /issue supporter months 12"
bulkUsage = ReplyText "Usage: /bulk <kind> [months <M>] count <N>\nsupporter, legend, investor: months 1–12, count 1–100\ne.g. /bulk legend months 3 count 10"
