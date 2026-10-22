{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesOwnerTests (namesOwnerTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (launchPricesAnswer, oracle, pricesAnswer, pricesCall, registrarOf, rentPriceAnswer, rentPriceCall, unhex)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesStoreTests (order, ownerA, t0)
import Bots.StoreService.WebTests (digestFixture, jsonObject, statusOf, webGet)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM, forM_, unless, void)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BC
import Data.IORef (readIORef)
import Data.Foldable (toList)
import Data.List (find, sort, sortOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64, Word8)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.EIP712 (serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Abi (addressWord, hex, uint256)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Orders (OrdersEnv (..), signedRequestSeconds)
import StoreService.Names.Phases (RegistrationId (..), phasesWaiting)
import StoreService.Names.Signatures (OrderFields (..), findMessage, parseAnyCaseAddress, personalDigest, retargetMessage, takenCodeMessage)
import StoreService.Names.Store hiding (findLimit)
import StoreService.Names.Turns (turnsCount, withLabelTurn)
import StoreService.Store (RevokeResult (..))
import StoreService.Web.Http (createLimit, findLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Owner (findHandler, retargetHandler, takenCodeHandler)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesOwnerTests :: Spec
namesOwnerTests = describe "StoreService names: owner" $ do
  describe "find" $ do
    it "lists the owner's orders newest first, a moved, an unpaid and a lapsed one read failed included, at most 50, and 404 for none" testFind
    it "refuses another signer's signature, a stale timestamp and a malformed body" testFindRefused
  describe "another name" $ do
    it "refuses a move from registered, queued, and committed without a refused reveal" testMoveRefusedPhases
    it "moves a failed order, a lapsed one, and a committed one whose links were refused, to the same name with other links too" testMoveAccepted
    it "answers a body equal to the current fields with the view unchanged, counting no move, whatever its timestamp" testMoveRepeat
    it "answers the fourth move move_limit with the current view, changing nothing" testMoveLimit
    it "refuses a move meeting another live order of the wallet's or the commitment with a bare order_exists, before the price" testMoveMeetsOrder
    it "runs the order checks on the new fields: invalid_name, the duration, name_taken, name_check_unavailable" testMoveOrderChecks
    it "registers a failed order whose register went through, by its mined register or by ownerOf, answering registration_changed and spending a code" testMoveLanded
    it "answers name_check_unavailable when the register check cannot read the chain, changing nothing" testMoveLandedUnread
    it "answers a dearer name price_too_high with its price, and moves to a cheaper one at its price" testMovePrice
    it "checks a code order's code again: code_not_covering for a shorter name, a held code kept though revoked and expired since" testMoveCodeOrder
    it "reserves a released code again while unused, else answers code_used, code_reserved, code_invalid or code_expired" testMoveReleasedCode
    it "refuses sealed over 11095 characters, a body over 16384 bytes and malformed fields, accepting the longest" testMoveSizes
    it "refuses another signer's signature and a stale timestamp, and answers an unknown order 404" testMoveRefusedSigner
    it "answers registration_changed with the current view when the order changed during the checks, writing nothing" testMoveLostRace
    it "moves A to B while another order moves B to A, in the turns' hash order, without deadlock" testMovesCross
  describe "a code instead" $ do
    it "issues a code for a paid taken order, its minLength 6, 7 and 8 for 6, 7 and 12 letters at the launch prices, kept in the view" testTakenCode
    it "answers an identical repeat 200 whatever its codeLength and timestamp, even after a price change" testTakenCodeRepeat
    it "answers code_length_changed with the current length after a price change, issuing nothing" testCodeLengthChanged
    it "answers code_unavailable when no length qualifies, and 503 provider_unavailable when the prices cannot be read" testCodeUnavailable
    it "refuses a code for an order that is not a paid taken one, another signer, a stale timestamp and a malformed hash" testTakenCodeRefused
    it "answers code_conflict for a hash another code has, changing nothing" testCodeConflict
    it "answers registration_changed with the current view when the order changed during the length read, issuing nothing" testCodeLostRace
  describe "polls" $
    it "wakes a held registration poll by another name and by a code" testPollsWoken

-- fixtures

-- | The owner routes and the registration poll as namesRoutes registers them, with the code orders, since a NamesEnv needs a running chat controller.
withOwner :: (Ctx -> IO a) -> IO a
withOwner = withOwnerAs id

withOwnerAs :: (NamesConfig -> NamesConfig) -> (Ctx -> IO a) -> IO a
withOwnerAs change action = withOrders $ \ctx@Ctx {cOrders, cWeb} -> do
  let orders = cOrders {oeConfig = change (oeConfig cOrders)}
      web = cWeb {weOrders = Just orders}
      routes path = ownerRoutes orders path <|> codeRoutes orders (weProviders web) (weWaiters web) path
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
      ctx' = ctx {cOrders = orders, cWeb = web}
  forM_ ["alicebakery", "dynamis"] $ answerUnowned ctx'
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port -> action ctx' {cClient = (mgr, "http://127.0.0.1:" <> show port)}

ownerRoutes :: OrdersEnv -> [Text] -> Maybe Route
ownerRoutes orders = \case
  ["api", "registration", "find"] -> Just Route {rMethod = "POST", rLimit = findLimit, rHandler = findHandler orders}
  ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler orders holdMicros rid}
  ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retargetHandler orders rid}
  ["api", "registration", rid, "code"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = takenCodeHandler orders rid}
  _ -> Nothing

signWith :: Ctx -> Secp256k1PrivateKey -> ByteString -> IO Text
signWith Ctx {cRandom} key msg = hex . serializeSignature . either (error . show) id <$> signRecoverable cRandom key (personalDigest msg)

clockSeconds :: Ctx -> IO Word64
clockSeconds Ctx {cClock} = seconds <$> readIORef cClock

seconds :: UTCTime -> Word64
seconds = floor . utcTimeToPOSIXSeconds

signedWindow :: Word64
signedWindow = fromInteger signedRequestSeconds

byB :: Order -> Order
byB o = o {oOwner = ownerBText, oKey = keyB}

dynamisOrder :: Order
dynamisOrder = aliceOrder {oLabel = "dynamis", oCommitment = commitmentOf 3, oSealed = "sealed-dynamis-3", oPrice = 20000}

-- viem's selectors of nameExpires(uint256), reservedNames(bytes32) and ownerOf(uint256), as in the calls of the contracts tests
nameExpiresSelector, reservedNamesSelector, ownerOfSelector :: ByteString
nameExpiresSelector = unhex "d6e4fa86"
reservedNamesSelector = unhex "389c0748"
ownerOfSelector = unhex "6352211e"

labelBytes :: Text -> ByteString
labelBytes label = lh
  where
    LabelHash lh = labelHash label

-- | ownerOf of a name nobody holds: the registrar's bare require, a revert without data.
answerUnowned :: Ctx -> Text -> IO ()
answerUnowned Ctx {cNode} label = answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes label) $ Reverts ""

answerOwner :: Ctx -> Text -> Address -> IO ()
answerOwner Ctx {cNode} label owner = answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes label) $ Returns $ addressWord owner

answerExpiry :: Ctx -> Text -> Word64 -> IO ()
answerExpiry Ctx {cNode} label expiry = answerCall cNode (registrarOf cNode) (nameExpiresSelector <> labelBytes label) $ Returns $ uint256 expiry

-- | A register transaction of the order as the sender saves it, then moved to this outcome.
sentRegister :: Ctx -> NameRegistration -> Word8 -> TxStatus -> IO TxHash
sentRegister Ctx {cStore} NameRegistration {nrId} n status = withTransaction cStore $ \db -> do
  let h = TxHash $ digestFixture n
  void $ insertTransaction db (TxRegister nrId) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = fromIntegral n} orderTime
  recordTransactionOutcome db h TSPending status (Just 120) `shouldReturn` True
  pure h

-- | The chain answers the name as available and held by nobody.
answerFree :: Ctx -> Text -> IO ()
answerFree ctx label = answerReserved ctx label 0 >> answerUnowned ctx label

answerReserved :: Ctx -> Text -> Int -> IO ()
answerReserved Ctx {cNode} label reason = do
  answerCall cNode (registrarOf cNode) (nameExpiresSelector <> lh) $ Returns $ uint256 (0 :: Int)
  answerCall cNode fakeController (reservedNamesSelector <> lh) $ Returns $ uint256 reason
  where
    LabelHash lh = labelHash label

-- | The oracle's price list, as codeLengthFor reads it.
answerPrices :: Ctx -> ByteString -> IO ()
answerPrices Ctx {cNode} prices = do
  answerCall cNode fakeController pricesCall $ Returns $ addressWord oracle
  answerCall cNode oracle pricesCall $ Returns prices

-- | A paid order, settled, then written as the worker would write it.
paidIn :: Ctx -> Order -> (NameRegistration -> NameRegistration) -> IO NameRegistration
paidIn ctx o change = do
  (invId, _) <- placed ctx o
  settleNow ctx invId settled `shouldReturn` Right ISPaid
  r <- invoiceRegistration ctx invId
  rewrite ctx r change

-- | A code order of a code of this length, then written as the worker would write it.
codeIn :: Ctx -> Int -> Order -> (NameRegistration -> NameRegistration) -> IO (NameRegistration, PrepaidCode)
codeIn ctx len o change = do
  (text, code) <- issueCode ctx len 2 id
  r <- registrationById ctx =<< registered ctx text o
  (,code) <$> rewrite ctx r change

-- | A write outside the turns, as the worker's, at the stored version.
rewrite :: Ctx -> NameRegistration -> (NameRegistration -> NameRegistration) -> IO NameRegistration
rewrite Ctx {cStore, cClock} NameRegistration {nrId} change = do
  t <- readIORef cClock
  withTransaction cStore $ \db -> do
    current <- fromJust <$> getRegistration db nrId
    updateRegistration db (change current) t >>= \case
      Written _ -> fromJust <$> getRegistration db nrId
      refused -> error $ "not written: " <> show refused

stored :: Ctx -> NameRegistration -> IO NameRegistration
stored ctx = registrationById ctx . nrId

failedPhase, takenPhase, registeredPhase, committedPhase, linksRefusedPhase :: NameRegistration -> NameRegistration
failedPhase r = r {nrPhase = PFailed, nrFailedAt = Just orderTime, nrError = Just "The name could not be registered"}
takenPhase r = r {nrPhase = PTaken}
registeredPhase r = r {nrPhase = PRegistered, nrExpiresAt = Just (seconds orderTime + 3 * 31536000)}
committedPhase r =
  r
    { nrPhase = PCommitted,
      nrCommitTx = Just (TxHash $ digestFixture 8),
      nrCommitBlock = Just 100,
      nrRevealAfter = Just (seconds orderTime + 60),
      nrRevealBefore = Just (seconds orderTime + 86400)
    }
linksRefusedPhase r = (committedPhase r) {nrLinksRefusedAt = Just orderTime}

-- | Another name's fields, with the page's duration for 2 years at the service's clock.
moveTo :: Ctx -> Text -> Int -> IO OrderFields
moveTo Ctx {cClock} label n = do
  t <- readIORef cClock
  pure OrderFields {ofLabelHash = labelHash label, ofLength = T.length label, ofDuration = yearsDuration 2 t, ofCommitment = commitmentOf n, ofSealed = "sealed-" <> label <> "-" <> tshow n}

moveBody :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> OrderFields -> IO J.Object
moveBody ctx key rid fields = clockSeconds ctx >>= moveBodyAt ctx key rid fields

moveBodyAt :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> OrderFields -> Word64 -> IO J.Object
moveBodyAt ctx key rid fields@OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed} timestamp = do
  sig <- signWith ctx key $ retargetMessage rid fields timestamp
  pure $
    KM.fromList
      [ ("labelHash", J.String $ hex lh),
        ("length", J.toJSON ofLength),
        ("duration", J.toJSON ofDuration),
        ("commitment", J.String $ hex ofCommitment),
        ("sealed", J.String ofSealed),
        ("timestamp", J.toJSON timestamp),
        ("signature", J.String sig)
      ]

postMove :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
postMove ctx rid = post ctx (registrationPath rid <> "/name") . J.encode

move :: Ctx -> Secp256k1PrivateKey -> NameRegistration -> OrderFields -> IO (Int, Maybe J.Object)
move ctx key NameRegistration {nrId} fields = moveBody ctx key nrId fields >>= postMove ctx nrId

-- | The view of an accepted move, failing the test otherwise.
moved :: Ctx -> Secp256k1PrivateKey -> NameRegistration -> OrderFields -> IO J.Object
moved ctx key r fields =
  move ctx key r fields >>= \case
    (200, Just v) -> pure v
    other -> error $ "move refused: " <> show other

registrationPath :: RegistrationId -> String
registrationPath (RegistrationId rid) = "/api/registration/" <> T.unpack rid

getView :: Ctx -> NameRegistration -> IO J.Object
getView ctx NameRegistration {nrId} = webGet (cClient ctx) (registrationPath nrId) >>= jsonObject

findBody :: Ctx -> Secp256k1PrivateKey -> Text -> Word64 -> IO J.Object
findBody ctx key owner timestamp = do
  sig <- signWith ctx key $ findMessage (either (error . show) id $ parseAnyCaseAddress owner) timestamp
  pure $ KM.fromList [("owner", J.String owner), ("timestamp", J.toJSON timestamp), ("signature", J.String sig)]

postFind :: Ctx -> J.Object -> IO (Int, Maybe J.Object)
postFind ctx = post ctx "/api/registration/find" . J.encode

-- | A code hash as the page sends it: unpadded base64url of a SHA-256.
takenCodeHash :: Int -> (Text, ByteString)
takenCodeHash n = (T.filter (/= '=') $ safeDecodeUtf8 $ B64U.encode h, h)
  where
    h = C.sha256Hash $ "taken code " <> BC.pack (show n)

codeBodyAt :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> Text -> Int -> Word64 -> IO J.Object
codeBodyAt ctx key rid codeHash codeLength timestamp = do
  sig <- signWith ctx key $ takenCodeMessage rid codeHash codeLength timestamp
  pure $ KM.fromList [("codeHash", J.String codeHash), ("codeLength", J.toJSON codeLength), ("timestamp", J.toJSON timestamp), ("signature", J.String sig)]

postCode :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
postCode ctx rid = post ctx (registrationPath rid <> "/code") . J.encode

takeCode :: Ctx -> Secp256k1PrivateKey -> NameRegistration -> Text -> Int -> IO (Int, Maybe J.Object)
takeCode ctx key NameRegistration {nrId} codeHash codeLength = clockSeconds ctx >>= codeBodyAt ctx key nrId codeHash codeLength >>= postCode ctx nrId

currentFields :: NameRegistration -> OrderFields
currentFields NameRegistration {nrLabelHash, nrLength, nrDuration, nrCommitment, nrSealed} =
  OrderFields {ofLabelHash = nrLabelHash, ofLength = nrLength, ofDuration = nrDuration, ofCommitment = nrCommitment, ofSealed = nrSealed}

fieldsIn :: J.Object -> [(Text, J.Value)] -> Expectation
fieldsIn v = mapM_ (\(k, value) -> (k, field k v) `shouldBe` (k, Just value))

answered :: Maybe J.Object
answered = Just KM.empty

refusedWithView :: Text -> J.Object -> (Int, Maybe J.Object)
refusedWithView tag view = (409, Just $ KM.fromList [("error", J.String tag), ("registration", J.Object view)])

labelHashText :: Text -> J.Value
labelHashText label = J.String $ hex lh
  where
    LabelHash lh = labelHash label

issuedCode :: Ctx -> NameRegistration -> IO PrepaidCode
issuedCode Ctx {cStore} NameRegistration {nrIssuedCodeId} = withTransaction cStore $ \db -> fromJust <$> getPrepaidCodeById db (fromJust nrIssuedCodeId)

codeRow :: Ctx -> PrepaidCode -> IO PrepaidCode
codeRow Ctx {cStore} PrepaidCode {pcId} = withTransaction cStore $ \db -> fromJust <$> getPrepaidCodeById db pcId

-- | Bounded, since a deadlock would otherwise hang the suite.
within :: String -> IO a -> IO a
within what action = timeout 10000000 action >>= maybe (error $ what <> " did not finish") pure

-- | Waits until the turns have at least this many label or code hashes held or waited on.
awaitTurns :: Ctx -> Int -> IO ()
awaitTurns Ctx {cOrders} n = within "turns" loop
  where
    loop = turnsCount (oeTurns cOrders) >>= \held -> unless (held >= n) (threadDelay 1000 >> loop)

-- | Long enough for a request just sent to reach its turns.
parkDelay :: Int
parkDelay = 200000

-- | Each eth_call's delay in the lost-race tests, far longer than a request takes to read its registration.
slowRead :: Int
slowRead = 1000000

-- tests

testFind :: IO ()
testFind = withOwner $ \ctx@Ctx {cStore} -> do
  forM_ [1 .. 50 :: Int] $ \i -> do
    (_, code) <- issueCode ctx 8 2 id
    let new = NewRegistration {nrgId = oldOrder i, nrgOwner = ownerA, nrgOrder = order ("alicebakery" <> tshow i) (fromIntegral i), nrgYears = 2}
    withTransaction cStore (\db -> createCodeRegistration db new code (addUTCTime (fromIntegral i) t0)) `shouldReturn` Right ()
  answerFree ctx "alicebakers"
  answerUnowned ctx "alicebakery50"
  movedOrder <- registrationById ctx (oldOrder 50) >>= \r -> rewrite ctx r failedPhase
  to <- moveTo ctx "alicebakers" 10
  void $ moved ctx keyA movedOrder to
  (InvoiceId invId, _) <- placed ctx aliceOrder
  paid <- invoiceRegistration ctx (InvoiceId invId)
  advance ctx 1
  lapsed <- paidIn ctx dynamisOrder $ \r -> (committedPhase r) {nrRevealBefore = Just (seconds orderTime)}
  findBody ctx keyA (T.toLower ownerAText) (seconds orderTime) >>= postFind ctx >>= \case
    (200, Just v) | Just (J.Array found) <- KM.lookup "registrations" v -> do
      let entries = map objectEntry $ toList found
      map (field "registrationId") entries `shouldBe` map (Just . idText) (nrId lapsed : nrId paid : [oldOrder i | i <- [50, 49 .. 3]])
      case entries of
        lapsedEntry : unpaidEntry : moveEntry : _ -> do
          (field "phase" lapsedEntry, nrPhase lapsed) `shouldBe` (Just "failed", PCommitted)
          sort (KM.keys unpaidEntry) `shouldBe` sort ["registrationId", "invoiceId", "phase", "createdAt", "labelHash", "sealed"]
          unpaidEntry
            `shouldBe` KM.fromList
              [ ("registrationId", idText $ nrId paid),
                ("invoiceId", J.String invId),
                ("phase", "unpaid"),
                ("createdAt", J.toJSON orderTime),
                ("labelHash", labelHashText "alicebakery"),
                ("sealed", "sealed-alicebakery-1")
              ]
          moveEntry
            `shouldBe` KM.fromList
              [ ("registrationId", "old50"),
                ("invoiceId", J.Null),
                ("phase", "queued"),
                ("createdAt", J.toJSON $ addUTCTime 50 t0),
                ("labelHash", labelHashText "alicebakers"),
                ("sealed", "sealed-alicebakers-10")
              ]
        _ -> expectationFailure "fewer than three entries"
    other -> expectationFailure $ "find refused: " <> show other
  (findBody ctx keyB ownerBText (seconds orderTime) >>= postFind ctx) `shouldReturn` refusedWith 404 "not_found"
  where
    oldOrder i = RegistrationId $ "old" <> tshow (i :: Int)
    idText (RegistrationId t) = J.String t
    objectEntry = \case
      J.Object o -> o
      other -> error $ "not an entry: " <> show other

testFindRefused :: IO ()
testFindRefused = withOwner $ \ctx -> do
  void $ placed ctx aliceOrder
  let ts = seconds orderTime
  (findBody ctx keyB ownerAText ts >>= postFind ctx) `shouldReturn` refusedWith 400 "bad_request"
  (findBody ctx keyA ownerAText (ts - signedWindow - 1) >>= postFind ctx) `shouldReturn` refusedWith 400 "bad_request"
  (findBody ctx keyA ownerAText (ts + signedWindow + 1) >>= postFind ctx) `shouldReturn` refusedWith 400 "bad_request"
  body <- findBody ctx keyA ownerAText ts
  postFind ctx (KM.insert "owner" "0x014f9c09" body) `shouldReturn` refusedWith 400 "bad_request"
  postFind ctx (KM.delete "signature" body) `shouldReturn` refusedWith 400 "bad_request"
  post ctx "/api/registration/find" "[]" `shouldReturn` refusedWith 400 "bad_request"
  findBody ctx keyA ownerAText (ts - signedWindow) >>= postFind ctx >>= (`shouldBe` 200) . fst

testMoveRefusedPhases :: IO ()
testMoveRefusedPhases = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  to <- moveTo ctx "alicebakers" 10
  registeredOrder <- paidIn ctx aliceOrder registeredPhase
  committedOrder <- paidIn ctx aliceOrder {oCommitment = commitmentOf 2, oTimestamp = seconds orderTime + 1} committedPhase
  queuedOrder <- paidIn ctx dynamisOrder id
  forM_ [registeredOrder, committedOrder, queuedOrder] $ \r -> do
    (nrPhase r,) <$> move ctx keyA r to `shouldReturn` (nrPhase r, refusedWith 400 "bad_request")
    stored ctx r `shouldReturn` r

testMoveAccepted :: IO ()
testMoveAccepted = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  failedOrder <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  v <- moved ctx keyA failedOrder to
  fieldsIn v
    [ ("phase", "queued"),
      ("labelHash", labelHashText "alicebakers"),
      ("commitment", J.String $ hex $ ofCommitment to),
      ("sealed", J.String $ ofSealed to),
      ("duration", J.toJSON $ ofDuration to),
      ("price", J.Number 10000),
      ("moves", J.Number 1),
      ("version", J.toJSON $ nrVersion failedOrder + 1),
      ("linksRefused", J.Bool False),
      ("label", J.Null),
      ("commitTx", J.Null),
      ("revealAfter", J.Null),
      ("revealBefore", J.Null),
      ("error", J.Null)
    ]
  written <- stored ctx failedOrder
  (nrPhase written, nrLive written, nrMoves written, nrFailedAt written, nrOrderHash written) `shouldBe` (PQueued, True, 1, Nothing, nrOrderHash failedOrder)
  -- the same name with other links, after a reveal refused for its links
  refusedOrder <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} linksRefusedPhase
  sameName <- moveTo ctx "alicebakery" 11
  v2 <- moved ctx keyB refusedOrder sameName
  fieldsIn v2 [("phase", "queued"), ("labelHash", labelHashText "alicebakery"), ("linksRefused", J.Bool False), ("commitTx", J.Null), ("revealAfter", J.Null), ("moves", J.Number 1)]
  (nrLinksRefusedAt &&& nrCommitTx) <$> stored ctx refusedOrder `shouldReturn` (Nothing, Nothing)
  -- a committed order with no reveal past revealBefore reads failed
  lapsed <- paidIn ctx dynamisOrder $ \r -> (committedPhase r) {nrRevealBefore = Just (seconds orderTime - 1)}
  back <- moveTo ctx "alicebakery" 12
  v3 <- moved ctx keyA lapsed back
  fieldsIn v3 [("phase", "queued"), ("error", J.Null), ("price", J.Number 10000)]

testMoveRepeat :: IO ()
testMoveRepeat = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  -- the current fields from failed: nothing moves
  (status, v) <- move ctx keyA r (currentFields r)
  (status, field "phase" =<< v, field "moves" =<< v) `shouldBe` (200, Just "failed", Just (J.Number 0))
  stored ctx r `shouldReturn` r
  to <- moveTo ctx "alicebakers" 10
  body <- moveBody ctx keyA (nrId r) to
  first' <- postMove ctx (nrId r) body
  fst first' `shouldBe` 200
  movedOnce <- stored ctx r
  advance ctx 3600
  postMove ctx (nrId r) body `shouldReturn` first'
  resigned <- moveBody ctx keyA (nrId r) to
  postMove ctx (nrId r) resigned `shouldReturn` first'
  stored ctx r `shouldReturn` movedOnce
  nrMoves movedOnce `shouldBe` 1

testMoveLimit :: IO ()
testMoveLimit = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  forM_ (zip [1 :: Int ..] ["alicebakers", "alicebakery", "alicebakers"]) $ \(n, label) -> do
    to <- moveTo ctx label (10 + n)
    v <- moved ctx keyA r to
    field "moves" v `shouldBe` Just (J.toJSON n)
    rewrite ctx r failedPhase
  limited <- stored ctx r
  to <- moveTo ctx "alicebakery" 20
  view <- getView ctx limited
  move ctx keyA limited to `shouldReturn` refusedWithView "move_limit" view
  (field "moves" view, field "phase" view) `shouldBe` (Just (J.Number 3), Just "failed")
  stored ctx r `shouldReturn` limited

testMoveMeetsOrder :: IO ()
testMoveMeetsOrder = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  void $ placed ctx dynamisOrder
  toDynamis <- moveTo ctx "dynamis" 10
  move ctx keyA r toDynamis `shouldReturn` refusedWith 409 "order_exists"
  void $ placed ctx (byB aliceOrder) {oLabel = "alicebakers", oCommitment = commitmentOf 4}
  toForeign <- moveTo ctx "alicebakers" 4
  move ctx keyA r toForeign `shouldReturn` refusedWith 409 "order_exists"
  stored ctx r `shouldReturn` r

testMoveOrderChecks :: IO ()
testMoveOrderChecks = withOwner $ \ctx@Ctx {cNode} -> do
  r <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  move ctx keyA r to {ofLength = 5} `shouldReturn` refusedWith 400 "invalid_name"
  move ctx keyA r to {ofLength = 64} `shouldReturn` refusedWith 400 "invalid_name"
  answerFree ctx "alicebakers"
  move ctx keyA r to {ofDuration = ofDuration to + 3601} `shouldReturn` refusedWith 400 "bad_request"
  answerReserved ctx "alicebakers" 1
  move ctx keyA r to `shouldReturn` refusedWith 409 "name_taken"
  setFault cNode "eth_call" HttpFailure
  move ctx keyA r to `shouldReturn` refusedWith 503 "name_check_unavailable"
  stored ctx r `shouldReturn` r

testMoveLanded :: IO ()
testMoveLanded = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  minedOrder <- paidIn ctx aliceOrder failedPhase
  (ownedOrder, code) <- codeIn ctx 7 dynamisOrder failedPhase
  notLanded <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} failedPhase
  let expiry = seconds orderTime + 2 * 31536000
  forM_ ["alicebakery", "dynamis"] $ \label -> answerExpiry ctx label expiry
  TxHash minedHash <- sentRegister ctx minedOrder 1 TSMined
  answerOwner ctx "dynamis" (nrOwner ownedOrder)
  void $ sentRegister ctx notLanded 2 TSReverted
  answerOwner ctx "alicebakery" (nrOwner minedOrder)
  forM_ [(minedOrder, Just minedHash), (ownedOrder, Nothing)] $ \(r, hash) -> do
    to <- moveTo ctx "alicebakers" 10
    answer <- move ctx keyA r to
    view <- getView ctx r
    answer `shouldBe` refusedWithView "registration_changed" view
    fieldsIn view [("phase", "registered"), ("expiresAt", J.toJSON expiry), ("moves", J.Number 0), ("error", J.Null), ("registerTx", maybe J.Null (J.String . hex) hash)]
    written <- stored ctx r
    (nrPhase written, nrLive written, nrVersion written, nrLabelHash written) `shouldBe` (PRegistered, False, nrVersion r + 1, nrLabelHash r)
  pcSpentAt <$> codeRow ctx code `shouldReturn` Just orderTime
  to <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyB notLanded to
  fieldsIn v [("phase", "queued"), ("moves", J.Number 1)]

testMoveLandedUnread :: IO ()
testMoveLandedUnread = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  -- a revert with data is a failed read, not an unowned name
  answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes "alicebakery") $ Reverts $ unhex "37ed32e8"
  to <- moveTo ctx "alicebakers" 10
  move ctx keyA r to `shouldReturn` refusedWith 503 "name_check_unavailable"
  stored ctx r `shouldReturn` r

testMovePrice :: IO ()
testMovePrice = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  cheap <- paidIn ctx aliceOrder failedPhase
  toDynamis <- moveTo ctx "dynamis" 10
  move ctx keyA cheap toDynamis `shouldReturn` (409, Just $ KM.fromList [("error", "price_too_high"), ("price", J.Number 20000)])
  stored ctx cheap `shouldReturn` cheap
  dear <- paidIn ctx (byB dynamisOrder) takenPhase
  toCheaper <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyB dear toCheaper
  (field "price" v, field "phase" v) `shouldBe` (Just (J.Number 10000), Just "queued")
  nrPrice <$> stored ctx dear `shouldReturn` 10000

testMoveCodeOrder :: IO ()
testMoveCodeOrder = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  (r, code@PrepaidCode {pcCodeHash}) <- codeIn ctx 8 aliceOrder failedPhase
  toShorter <- moveTo ctx "dynamis" 10
  move ctx keyA r toShorter `shouldReturn` refusedWith 409 "code_not_covering"
  stored ctx r `shouldReturn` r
  withTransaction cStore (\db -> revokePrepaidCode db pcCodeHash orderTime) `shouldReturn` Revoked (pcId code)
  withTransaction cStore $ \db -> DB.execute db "UPDATE sx_badge_service_name_prepaid_codes SET expires_at = ? WHERE code_hash = ?" (orderTime, DB.Binary pcCodeHash)
  to <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyA r to
  (field "phase" v, field "price" v) `shouldBe` (Just "queued", Just (J.Number 0))
  (pcReservedBy &&& pcSpentAt) <$> codeRow ctx code `shouldReturn` (Just (nrId r), Nothing)

testMoveReleasedCode :: IO ()
testMoveReleasedCode = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  spender <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 20} id
  holder <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 21} id
  let refusals :: [(Text, PrepaidCode -> DB.Connection -> IO (), (Int, Maybe J.Object))]
      refusals =
        [ ("spent", \PrepaidCode {pcId} db -> reservePrepaidCode db pcId (nrId spender) orderTime >> void (spendPrepaidCode db pcId (nrId spender) orderTime), refusedWith 409 "code_used"),
          ("held", \PrepaidCode {pcId} db -> void $ reservePrepaidCode db pcId (nrId holder) orderTime, refusedWith 409 "code_reserved"),
          ("revoked", \PrepaidCode {pcCodeHash} db -> void $ revokePrepaidCode db pcCodeHash orderTime, refusedWith 400 "code_invalid"),
          ("expired", \PrepaidCode {pcCodeHash} db -> DB.execute db "UPDATE sx_badge_service_name_prepaid_codes SET expires_at = ? WHERE code_hash = ?" (orderTime, DB.Binary pcCodeHash), refusedWith 409 "code_expired")
        ]
  forM_ (zip [1 :: Int ..] refusals) $ \(n, (what, change, refusal)) -> do
    let label = "codeorder" <> tshow n
    answerFree ctx label
    (r, code) <- codeIn ctx 8 aliceOrder {oLabel = label, oCommitment = commitmentOf (30 + n)} failedPhase
    withTransaction cStore (\db -> releasePrepaidCode db (pcId code) (nrId r)) `shouldReturn` True
    withTransaction cStore $ change code
    to <- moveTo ctx "alicebakers" (40 + n)
    (what,) <$> move ctx keyA r to `shouldReturn` (what, refusal)
    stored ctx r `shouldReturn` r
  (r, code) <- codeIn ctx 8 aliceOrder failedPhase
  withTransaction cStore (\db -> releasePrepaidCode db (pcId code) (nrId r)) `shouldReturn` True
  to <- moveTo ctx "alicebakers" 50
  void $ moved ctx keyA r to
  pcReservedBy <$> codeRow ctx code `shouldReturn` Just (nrId r)

testMoveSizes :: IO ()
testMoveSizes = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  let longest = to {ofSealed = T.replicate 11095 "s"}
  body <- moveBody ctx keyA (nrId r) longest
  postMove ctx (nrId r) (KM.insert "owner" (J.String $ T.replicate 5300 "0") body) `shouldReturn` refusedWith 400 "bad_request"
  move ctx keyA r to {ofSealed = T.replicate 11096 "s"} `shouldReturn` refusedWith 400 "bad_request"
  valid <- moveBody ctx keyA (nrId r) to
  forM_ [KM.insert "labelHash" (J.String $ T.toUpper $ hex $ (\(LabelHash h) -> h) $ ofLabelHash to) valid, KM.delete "signature" valid, KM.insert "length" "11" valid] $ \malformed ->
    postMove ctx (nrId r) malformed `shouldReturn` refusedWith 400 "bad_request"
  post ctx (registrationPath (nrId r) <> "/name") "[]" `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  -- an owner in the body is ignored
  (status, v) <- postMove ctx (nrId r) (KM.insert "owner" (J.String ownerBText) body)
  (status, field "sealed" =<< v, field "owner" =<< v) `shouldBe` (200, Just (J.String $ ofSealed longest), Just (J.String ownerAText))

testMoveRefusedSigner :: IO ()
testMoveRefusedSigner = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  move ctx keyB r to `shouldReturn` refusedWith 400 "bad_request"
  ts <- clockSeconds ctx
  (moveBodyAt ctx keyA (nrId r) to (ts - signedWindow - 1) >>= postMove ctx (nrId r)) `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  let unknown = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"
  (moveBody ctx keyA unknown to >>= postMove ctx unknown) `shouldReturn` refusedWith 404 "not_found"

testMoveLostRace :: IO ()
testMoveLostRace = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  setFault cNode "eth_call" (Slow slowRead)
  moving <- async $ move ctx keyA r to
  awaitTurns ctx 2
  threadDelay (slowRead `div` 5)
  changed <- rewrite ctx r id
  clearFault cNode "eth_call"
  answer <- within "the move" $ wait moving
  view <- getView ctx changed
  answer `shouldBe` refusedWithView "registration_changed" view
  field "version" view `shouldBe` Just (J.toJSON $ nrVersion r + 1)
  stored ctx r `shouldReturn` changed

-- | Each round holds the higher hash's turn while both orders start, the one moving from the lower hash first in odd
-- rounds and last in even ones, so that without the hash order some round has each order holding one turn and waiting
-- for the other's.
testMovesCross :: IO ()
testMovesCross = withOwnerAs (\cfg -> cfg {nMaxOrderMoves = 100}) $ \ctx -> do
  answerFree ctx "alicebakers"
  let (low, high) = case sortOn hashOf ["alicebakery", "alicebakers"] of
        [l, h] -> (l, h)
        _ -> error "two names"
  a <- paidIn ctx aliceOrder {oLabel = low, oCommitment = commitmentOf 60} failedPhase
  b <- paidIn ctx (byB aliceOrder) {oLabel = high, oCommitment = commitmentOf 61} failedPhase
  forM_ [1 .. 6 :: Int] $ \n -> do
    rs <- mapM (stored ctx) [a, b]
    let onName label = fromJust $ find ((== labelHash label) . nrLabelHash) rs
        keyOf r = if nrOwner r == nrOwner a then keyA else keyB
        bodyTo r label k = moveTo ctx label (100 + 2 * n + k) >>= moveBody ctx (keyOf r) (nrId r)
        (fromLow, fromHigh) = (onName low, onName high)
    lowMove <- (fromLow,) <$> bodyTo fromLow high 0
    highMove <- (fromHigh,) <$> bodyTo fromHigh low 1
    holding <- newEmptyMVar
    release <- newEmptyMVar
    holder <- async $ withLabelTurn (oeTurns $ cOrders ctx) (labelHash high) $ putMVar holding () >> takeMVar release
    takeMVar holding
    let started = if odd n then [lowMove, highMove] else [highMove, lowMove]
    moves <- forM started $ \(r, body) -> async (postMove ctx (nrId r) body) <* threadDelay parkDelay
    putMVar release ()
    wait holder
    outcomes <- within ("round " <> show n) $ mapM wait moves
    map fst outcomes `shouldBe` [200, 200]
    forM_ [a, b] $ \r -> rewrite ctx r failedPhase
  where
    hashOf label = (\(LabelHash h) -> h) (labelHash label)

testTakenCode :: IO ()
testTakenCode = withOwner $ \ctx@Ctx {cNode} -> do
  answerPrices ctx launchPricesAnswer
  answerCall cNode fakeController (rentPriceCall 6) $ Returns $ rentPriceAnswer 40000
  answerCall cNode fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  forM_ (zip [1 :: Int ..] [("bakery", 40000, 6), ("dynamis", 20000, 7), ("alicebakery2", 10000, 8)]) $ \(n, (label, price, len)) -> do
    answerFree ctx label
    r <- paidIn ctx aliceOrder {oLabel = label, oPrice = price, oCommitment = commitmentOf (70 + n)} takenPhase
    field "codeLength" <$> getView ctx r `shouldReturn` Just (J.toJSON len)
    let (hashText, hash) = takenCodeHash n
    takeCode ctx keyA r hashText len `shouldReturn` (200, answered)
    written <- stored ctx r
    (nrPhase written, nrLive written, nrVersion written) `shouldBe` (PCodeIssued, False, nrVersion r + 1)
    PrepaidCode {pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, pcReservedBy, pcSpentAt, pcRevokedAt, pcExpiresAt} <- issuedCode ctx written
    (pcCodeHash, pcMinLength, pcYears, pcPaymentStatus) `shouldBe` (hash, len, 2, CPSPaid)
    (pcReservedBy, pcSpentAt, pcRevokedAt, pcExpiresAt) `shouldBe` (Nothing, Nothing, Nothing, Nothing)
    v <- getView ctx r
    (field "phase" v, field "codeLength" v) `shouldBe` (Just "code_issued", Just (J.toJSON len))

-- | 12 letters paid 10000 cents: with these prices 8 letters and longer cost that or less.
takenTwelve :: Ctx -> IO NameRegistration
takenTwelve ctx@Ctx {cNode} = do
  answerPrices ctx launchPricesAnswer
  answerCall cNode fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answerFree ctx "alicebakery2"
  paidIn ctx aliceOrder {oLabel = "alicebakery2", oPrice = 10000} takenPhase

testTakenCodeRepeat :: IO ()
testTakenCodeRepeat = withOwner $ \ctx -> do
  r <- takenTwelve ctx
  let (hashText, _) = takenCodeHash 1
  body <- clockSeconds ctx >>= codeBodyAt ctx keyA (nrId r) hashText 8
  postCode ctx (nrId r) body `shouldReturn` (200, answered)
  issued <- stored ctx r
  -- 9 letters and longer now cost 10000 cents, so a fresh computation would answer 9
  answerPrices ctx $ pricesAnswer 5000 [(8, 6000), (7, 10000), (6, 20000)]
  advance ctx 3600
  postCode ctx (nrId r) body `shouldReturn` (200, answered)
  takeCode ctx keyA r hashText 9 `shouldReturn` (200, answered)
  stored ctx r `shouldReturn` issued
  field "codeLength" <$> getView ctx r `shouldReturn` Just (J.Number 8)
  takeCode ctx keyA r (fst $ takenCodeHash 2) 9 `shouldReturn` refusedWith 400 "bad_request"

testCodeLengthChanged :: IO ()
testCodeLengthChanged = withOwner $ \ctx@Ctx {cStore} -> do
  r <- takenTwelve ctx
  field "codeLength" <$> getView ctx r `shouldReturn` Just (J.Number 8)
  answerPrices ctx $ pricesAnswer 5000 [(8, 6000), (7, 10000), (6, 20000)]
  let (hashText, hash) = takenCodeHash 1
  takeCode ctx keyA r hashText 8 `shouldReturn` (409, Just $ KM.fromList [("error", "code_length_changed"), ("codeLength", J.Number 9)])
  stored ctx r `shouldReturn` r
  withTransaction cStore (`getPrepaidCode` hash) `shouldReturn` Nothing
  takeCode ctx keyA r hashText 9 `shouldReturn` (200, answered)
  pcMinLength <$> (issuedCode ctx =<< stored ctx r) `shouldReturn` 9

testCodeUnavailable :: IO ()
testCodeUnavailable = withOwner $ \ctx@Ctx {cNode} -> do
  r <- takenTwelve ctx
  answerPrices ctx $ pricesAnswer 6000 []
  let (hashText, _) = takenCodeHash 1
  takeCode ctx keyA r hashText 8 `shouldReturn` refusedWith 409 "code_unavailable"
  field "codeLength" <$> getView ctx r `shouldReturn` Just J.Null
  setFault cNode "eth_call" HttpFailure
  takeCode ctx keyA r hashText 8 `shouldReturn` refusedWith 503 "provider_unavailable"
  stored ctx r `shouldReturn` r

testTakenCodeRefused :: IO ()
testTakenCodeRefused = withOwner $ \ctx -> do
  r <- takenTwelve ctx
  let (hashText, _) = takenCodeHash 1
  takeCode ctx keyB r hashText 8 `shouldReturn` refusedWith 400 "bad_request"
  ts <- clockSeconds ctx
  (codeBodyAt ctx keyA (nrId r) hashText 8 (ts - signedWindow - 1) >>= postCode ctx (nrId r)) `shouldReturn` refusedWith 400 "bad_request"
  body <- codeBodyAt ctx keyA (nrId r) hashText 8 ts
  forM_ [KM.insert "codeHash" (J.String $ T.drop 1 hashText) body, KM.insert "codeLength" "8" body, KM.delete "timestamp" body] $ \malformed ->
    postCode ctx (nrId r) malformed `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  (codeOrder, _) <- codeIn ctx 8 aliceOrder {oCommitment = commitmentOf 5} takenPhase
  committedOrder <- paidIn ctx dynamisOrder committedPhase
  forM_ [codeOrder, committedOrder] $ \o -> do
    takeCode ctx keyA o hashText 8 `shouldReturn` refusedWith 400 "bad_request"
    stored ctx o `shouldReturn` o
  let unknown = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"
  (codeBodyAt ctx keyA unknown hashText 8 ts >>= postCode ctx unknown) `shouldReturn` refusedWith 404 "not_found"

testCodeConflict :: IO ()
testCodeConflict = withOwner $ \ctx@Ctx {cStore} -> do
  r <- takenTwelve ctx
  let (hashText, hash) = takenCodeHash 1
  Right _ <- withTransaction cStore $ \db -> insertPrepaidCode db NewPrepaidCode {npcCodeHash = hash, npcMinLength = 8, npcYears = 2, npcPaymentStatus = CPSPaid, npcExpiresAt = Nothing} orderTime
  takeCode ctx keyA r hashText 8 `shouldReturn` refusedWith 409 "code_conflict"
  stored ctx r `shouldReturn` r

testCodeLostRace :: IO ()
testCodeLostRace = withOwner $ \ctx@Ctx {cNode, cStore} -> do
  r <- takenTwelve ctx
  let (hashText, hash) = takenCodeHash 1
  setFault cNode "eth_call" (Slow slowRead)
  issuing <- async $ takeCode ctx keyA r hashText 8
  awaitTurns ctx 1
  threadDelay (slowRead `div` 5)
  changed <- rewrite ctx r id
  clearFault cNode "eth_call"
  answer <- within "the code" $ wait issuing
  view <- getView ctx changed
  answer `shouldBe` refusedWithView "registration_changed" view
  stored ctx r `shouldReturn` changed
  withTransaction cStore (`getPrepaidCode` hash) `shouldReturn` Nothing

testPollsWoken :: IO ()
testPollsWoken = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  poll <- held ctx r
  to <- moveTo ctx "alicebakers" 10
  void $ moved ctx keyA r to
  (status, v) <- woken poll
  (status, field "version" v, field "phase" v) `shouldBe` (200, Just (J.toJSON $ nrVersion r + 1), Just "queued")
  t <- takenTwelve ctx
  poll' <- held ctx t
  takeCode ctx keyA t (fst $ takenCodeHash 1) 8 `shouldReturn` (200, answered)
  (status', v') <- woken poll'
  (status', field "version" v', field "phase" v') `shouldBe` (200, Just (J.toJSON $ nrVersion t + 1), Just "code_issued")
  where
    held ctx'@Ctx {cOrders} NameRegistration {nrId, nrVersion} = do
      waiting <- atomically $ phasesWaiting (oePhases cOrders)
      poll <- async $ webGet (cClient ctx') (registrationPath nrId <> "?seenVersion=" <> show nrVersion) >>= \resp -> (statusOf resp,) <$> jsonObject resp
      within "the hold" $ atomically $ phasesWaiting (oePhases cOrders) >>= check . (> waiting)
      pure poll
    woken :: Async (Int, J.Object) -> IO (Int, J.Object)
    woken poll = timeout 5000000 (wait poll) >>= maybe (cancel poll >> error "the poll was not woken") pure
