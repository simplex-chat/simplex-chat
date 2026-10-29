{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesOwnerTests
  ( namesOwnerTests,
    answerExpiry,
    answerFree,
    answerOwner,
    answerUnowned,
    byB,
    codeIn,
    committedPhase,
    dynamisOrder,
    failedPhase,
    findBody,
    labelBytes,
    moveBody,
    moveTo,
    ownerOfSelector,
    paidIn,
    postFind,
    postMove,
    rewrite,
    sentRegister,
    stored,
  )
where

import Bots.StoreService.ConfigTests (termsVersion)
import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (registrarOf, rentPriceAnswer, unhex)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesStoreTests (btc, insertInvoiceRow, order, ownerA, paidOrder, t0)
import Bots.StoreService.WebTests (capturingStderr, digestFixture, jsonObject, statusOf, webGet)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM, forM_, void)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
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
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.EIP712 (serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Abi (addressWord, hex, uint256)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Orders (OrdersEnv (..), signedRequestSeconds)
import StoreService.Names.Phases (RegistrationId (..), phasesWaiting)
import StoreService.Names.Signatures (OrderFields (..), findMessage, parseAnyCaseAddress, personalDigest, retargetMessage)
import StoreService.Names.Store hiding (findLimit)
import StoreService.Names.Turns (withLabelTurn)
import StoreService.Store (RevokeResult (..))
import StoreService.Web.Http (createLimit, findLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Owner (findHandler, retargetHandler)
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
    it "refuses a move from registered, queued, taken, and committed without a refused reveal" testMoveRefusedPhases
    it "moves a failed order, a lapsed one, and a committed one whose links were refused, to the same name with other links too" testMoveAccepted
    it "answers a body equal to the current fields with the view unchanged, counting no move, whatever its timestamp" testMoveRepeat
    it "answers a repeat after the terms version changed with the view unchanged, counting no second move" testMoveRepeatAfterBump
    it "moves an order by a body differing only in terms, storing them" testMoveTermsOnly
    it "answers terms_changed for an old version or the wrong variant, listed or not, a body differing only in terms included, and moves under the community terms, storing them" testMoveTermsChanged
    it "checks a code order moved to a community name for 2 years, and moved on to another name for its code's years again" testMoveCommunityYears
    it "prices a paid order moved to a community name for 2 years, whatever years it paid for" testMoveCommunityPaidYears
    it "checks the terms after the timestamp, /retry's first check, the existing orders and the duration, before the chain" testMoveTermsCheckOrder
    it "answers move_limit before checking the terms" testMoveLimitBeforeTerms
    it "answers the fourth move move_limit with the current view, changing nothing" testMoveLimit
    it "refuses a move meeting another live order of the wallet's or the commitment with a bare order_exists, before the price" testMoveMeetsOrder
    it "runs the order checks on the new fields: invalid_name, the duration, name_taken, name_check_unavailable" testMoveOrderChecks
    it "registers a failed order whose register went through, by its mined register or by ownerOf, answering registration_changed and spending a code" testMoveLanded
    it "answers name_check_unavailable when the register check cannot read the chain, changing nothing" testMoveLandedUnread
    it "spends a released code still unused when its failed order turns out registered, and alerts once when it was spent elsewhere meanwhile" testMoveLandedReleasedCode
    it "answers a dearer name price_too_high with its price, and moves to a cheaper one at its price, crediting the difference to its balance" testMovePrice
    it "credits nothing for a cheaper move whose compare-and-set loses" testCheaperMoveLostRace
    it "keeps fromBalance through a move to a cheaper name" testMoveKeepsFromBalance
    it "checks a code order's code again: code_not_covering for a shorter name, a held code kept though revoked and expired since" testMoveCodeOrder
    it "reserves a released code again while unused, else answers code_used, code_reserved, code_invalid or code_expired" testMoveReleasedCode
    it "refuses sealed over 11095 characters, a body over 16384 bytes and malformed fields, accepting the longest" testMoveSizes
    it "refuses another signer's signature and a stale timestamp, and answers an unknown order 404" testMoveRefusedSigner
    it "answers registration_changed with the current view when the order changed during the checks, writing nothing" testMoveLostRace
    it "moves A to B while another order moves B to A, in the turns' hash order, without deadlock" testMovesCross
  describe "polls" $
    it "wakes a held registration poll by another name" testPollsWoken

-- fixtures

-- | The owner routes and the registration poll as namesRoutes registers them, with the code orders, since a NamesEnv needs a running chat controller.
withOwner :: (Ctx -> IO a) -> IO a
withOwner = withOwnerAs id

withOwnerAs :: (NamesConfig -> NamesConfig) -> (Ctx -> IO a) -> IO a
withOwnerAs change action = withOrders $ \ctx -> do
  let ctx' = configured change ctx
  forM_ ["alicebakery", "dynamis"] $ answerUnowned ctx'
  serveOwner ctx' action

-- | The owner routes over the context's settings.
serveOwner :: Ctx -> (Ctx -> IO a) -> IO a
serveOwner ctx@Ctx {cOrders = orders, cWeb = web} action = do
  let routes path = ownerRoutes orders path <|> codeRoutes orders (weProviders web) (weWaiters web) path
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port -> action ctx {cClient = (mgr, "http://127.0.0.1:" <> show port)}

ownerRoutes :: OrdersEnv -> [Text] -> Maybe Route
ownerRoutes orders = \case
  ["api", "registration", "find"] -> Just Route {rMethod = "POST", rLimit = findLimit, rHandler = findHandler orders}
  ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler orders holdMicros rid}
  ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retargetHandler orders rid}
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
  answerCall cNode fakeNamePolicy (reservedNamesSelector <> lh) $ Returns $ uint256 reason
  where
    LabelHash lh = labelHash label

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

-- | The amount and credited of the order's balance.
balanceOf :: Ctx -> NameRegistration -> IO (Maybe (Int, Int))
balanceOf Ctx {cStore} NameRegistration {nrId} = fmap (\Balance {blAmount, blCredited} -> (blAmount, blCredited)) <$> withTransaction cStore (\db -> getBalance db nrId orderTime)

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
  pure OrderFields {ofLabelHash = labelHash label, ofLength = T.length label, ofDuration = yearsDuration 2 t, ofCommitment = commitmentOf n, ofSealed = "sealed-" <> label <> "-" <> tshow n, ofAgreedTerms = termsVersion}

moveBody :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> OrderFields -> IO J.Object
moveBody ctx key rid fields = clockSeconds ctx >>= moveBodyAt ctx key rid fields

moveBodyAt :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> OrderFields -> Word64 -> IO J.Object
moveBodyAt ctx key rid fields@OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} timestamp = do
  sig <- signWith ctx key $ retargetMessage rid fields timestamp
  pure $
    KM.fromList
      [ ("labelHash", J.String $ hex lh),
        ("length", J.toJSON ofLength),
        ("duration", J.toJSON ofDuration),
        ("commitment", J.String $ hex ofCommitment),
        ("sealed", J.String ofSealed),
        ("terms", J.String ofAgreedTerms),
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

currentFields :: NameRegistration -> OrderFields
currentFields NameRegistration {nrLabelHash, nrLength, nrDuration, nrCommitment, nrSealed, nrAgreedTerms} =
  OrderFields {ofLabelHash = nrLabelHash, ofLength = nrLength, ofDuration = nrDuration, ofCommitment = nrCommitment, ofSealed = nrSealed, ofAgreedTerms = nrAgreedTerms}

fieldsIn :: J.Object -> [(Text, J.Value)] -> Expectation
fieldsIn v = mapM_ (\(k, value) -> (k, field k v) `shouldBe` (k, Just value))

refusedWithView :: Text -> J.Object -> (Int, Maybe J.Object)
refusedWithView tag view = (409, Just $ KM.fromList [("error", J.String tag), ("registration", J.Object view)])

labelHashText :: Text -> J.Value
labelHashText label = J.String $ hex lh
  where
    LabelHash lh = labelHash label

codeRow :: Ctx -> PrepaidCode -> IO PrepaidCode
codeRow Ctx {cStore} PrepaidCode {pcId} = withTransaction cStore $ \db -> fromJust <$> getPrepaidCodeById db pcId

-- | Bounded, since a deadlock would otherwise hang the suite.
within :: String -> IO a -> IO a
within what action = timeout 10000000 action >>= maybe (error $ what <> " did not finish") pure

-- | A registration poll at the order's version, once the server holds it.
held :: Ctx -> NameRegistration -> IO (Async (Int, J.Object))
held ctx@Ctx {cOrders} NameRegistration {nrId, nrVersion} = do
  waiting <- atomically $ phasesWaiting (oePhases cOrders)
  poll <- async $ webGet (cClient ctx) (registrationPath nrId <> "?seenVersion=" <> show nrVersion) >>= \resp -> (statusOf resp,) <$> jsonObject resp
  within "the hold" $ atomically $ phasesWaiting (oePhases cOrders) >>= check . (> waiting)
  pure poll

woken :: Async (Int, J.Object) -> IO (Int, J.Object)
woken poll = timeout 5000000 (wait poll) >>= maybe (cancel poll >> error "the poll was not woken") pure

-- | Long enough for a request just sent to reach its turns.
parkDelay :: Int
parkDelay = 200000

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
          sort (KM.keys unpaidEntry) `shouldBe` sort ["registrationId", "invoiceId", "phase", "createdAt", "labelHash", "sealed", "balanceSource", "heldBy"]
          unpaidEntry
            `shouldBe` KM.fromList
              [ ("registrationId", idText $ nrId paid),
                ("invoiceId", J.String invId),
                ("phase", "unpaid"),
                ("createdAt", J.toJSON orderTime),
                ("labelHash", labelHashText "alicebakery"),
                ("sealed", "sealed-alicebakery-1"),
                ("balanceSource", J.Null),
                ("heldBy", J.Null)
              ]
          moveEntry
            `shouldBe` KM.fromList
              [ ("registrationId", "old50"),
                ("invoiceId", J.Null),
                ("phase", "queued"),
                ("createdAt", J.toJSON $ addUTCTime 50 t0),
                ("labelHash", labelHashText "alicebakers"),
                ("sealed", "sealed-alicebakers-10"),
                ("balanceSource", J.Null),
                ("heldBy", J.Null)
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
  takenOrder <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 4} takenPhase
  forM_ [registeredOrder, committedOrder, queuedOrder, takenOrder] $ \r -> do
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

testMoveRepeatAfterBump :: IO ()
testMoveRepeatAfterBump = withOwner $ \ctx0 -> do
  answerFree ctx0 "alicebakers"
  r <- paidIn ctx0 aliceOrder failedPhase
  body <- moveBody ctx0 keyA (nrId r) =<< moveTo ctx0 "alicebakers" 10
  first' <- postMove ctx0 (nrId r) body
  fst first' `shouldBe` 200
  movedOnce <- stored ctx0 r
  serveOwner (configured bumpedTerms ctx0) $ \ctx -> do
    advance ctx 60
    postMove ctx (nrId r) body `shouldReturn` first'
    stored ctx r `shouldReturn` movedOnce
    (nrMoves &&& nrAgreedTerms) movedOnce `shouldBe` (1, termsVersion)

testMoveTermsOnly :: IO ()
testMoveTermsOnly = withOwner $ \ctx -> do
  r <- paidIn ctx aliceOrder $ \p -> (failedPhase p) {nrAgreedTerms = "2026-10-01"}
  (status, v) <- move ctx keyA r (currentFields r) {ofAgreedTerms = termsVersion}
  (status, field "phase" =<< v, field "moves" =<< v) `shouldBe` (200, Just "queued", Just (J.Number 1))
  (nrAgreedTerms &&& nrMoves) <$> stored ctx r `shouldReturn` (termsVersion, 1)

testMoveTermsChanged :: IO ()
testMoveTermsChanged = withOwnerAs (listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  forM_ [olderTerms, communityTerms termsVersion] $ \t ->
    move ctx keyA r (currentFields r) {ofCommitment = commitmentOf 11, ofAgreedTerms = t} `shouldReturn` termsChanged termsVersion Nothing
  move ctx keyA r (currentFields r) {ofAgreedTerms = olderTerms} `shouldReturn` termsChanged termsVersion Nothing
  to <- moveTo ctx "alicebakers" 10
  forM_ [termsVersion, communityTerms olderTerms, otherCommunityTerms] $ \t ->
    move ctx keyA r to {ofAgreedTerms = t} `shouldReturn` termsChanged termsVersion (Just bakers)
  stored ctx r `shouldReturn` r
  v <- moved ctx keyA r to {ofAgreedTerms = communityTerms termsVersion}
  fieldsIn v [("terms", J.String $ communityTerms termsVersion), ("community", communityEntry bakers)]
  nrAgreedTerms <$> stored ctx r `shouldReturn` communityTerms termsVersion

testMoveCommunityYears :: IO ()
testMoveCommunityYears = withOwnerAs (listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  (code, _) <- issueCode ctx 8 5 id
  r <- registrationById ctx =<< registered ctx code aliceOrder {oDuration = yearsDuration 5 orderTime}
  failed <- rewrite ctx r failedPhase
  to <- (\f -> f {ofAgreedTerms = communityTerms termsVersion}) <$> moveTo ctx "alicebakers" 10
  move ctx keyA failed to {ofDuration = yearsDuration 5 orderTime} `shouldReturn` refusedWith 400 "bad_request"
  void $ moved ctx keyA failed to
  answerFree ctx "alicecakes"
  again <- rewrite ctx r failedPhase
  toPlain <- moveTo ctx "alicecakes" 11
  void $ moved ctx keyA again toPlain {ofDuration = yearsDuration 5 orderTime}
  (nrYears &&& nrDuration) <$> stored ctx r `shouldReturn` (5, yearsDuration 5 orderTime)

testMoveCommunityPaidYears :: IO ()
testMoveCommunityPaidYears = withOwnerAs (threeYears . listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  answerCall (cNode ctx) fakeController threeYearsOf11 $ Returns $ rentPriceAnswer 15000
  r <- paidIn ctx aliceOrder {oDuration = yearsDuration 3 orderTime, oPrice = 15000} failedPhase
  to <- (\f -> f {ofAgreedTerms = communityTerms termsVersion}) <$> moveTo ctx "alicebakers" 10
  void $ moved ctx keyA r to
  (nrYears &&& nrPrice) <$> stored ctx r `shouldReturn` (3, 10000)
  balanceOf ctx r `shouldReturn` Just (5000, 5000)
  where
    -- viem: rentPriceUSD of 11 letters for 3 years
    threeYearsOf11 = unhex "9ff7ec0900000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000005a39a80000000000000000000000000000000000000000000000000000000000000000b6161616161616161616161000000000000000000000000000000000000000000"

testMoveTermsCheckOrder :: IO ()
testMoveTermsCheckOrder = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  to <- (\f -> f {ofAgreedTerms = olderTerms}) <$> moveTo ctx "alicebakers" 10
  ts <- clockSeconds ctx
  (moveBodyAt ctx keyA (nrId r) to (ts - signedWindow - 1) >>= postMove ctx (nrId r)) `shouldReturn` refusedWith 400 "bad_request"
  setFault cNode "eth_call" HttpFailure
  move ctx keyA r to `shouldReturn` refusedWith 503 "name_check_unavailable"
  clearFault cNode "eth_call"
  void $ placed ctx dynamisOrder
  toDynamis <- moveTo ctx "dynamis" 10
  move ctx keyA r toDynamis {ofAgreedTerms = olderTerms} `shouldReturn` refusedWith 409 "order_exists"
  move ctx keyA r to {ofDuration = ofDuration to + 3601} `shouldReturn` refusedWith 400 "bad_request"
  answerReserved ctx "alicebakers" 1
  move ctx keyA r to `shouldReturn` termsChanged termsVersion Nothing
  stored ctx r `shouldReturn` r
  answerExpiry ctx "alicebakery" (seconds orderTime + 2 * 31536000)
  answerOwner ctx "alicebakery" ownerA
  answer <- move ctx keyA r to
  view <- getView ctx r
  answer `shouldBe` refusedWithView "registration_changed" view
  field "phase" view `shouldBe` Just "registered"

testMoveLimitBeforeTerms :: IO ()
testMoveLimitBeforeTerms = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder $ \p -> (failedPhase p) {nrMoves = 3}
  to <- moveTo ctx "alicebakers" 10
  view <- getView ctx r
  move ctx keyA r to {ofAgreedTerms = olderTerms} `shouldReturn` refusedWithView "move_limit" view
  stored ctx r `shouldReturn` r

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
    poll <- held ctx r
    to <- moveTo ctx "alicebakers" 10
    answer <- move ctx keyA r to
    view <- getView ctx r
    answer `shouldBe` refusedWithView "registration_changed" view
    woken poll `shouldReturn` (200, view)
    fieldsIn view [("phase", "registered"), ("expiresAt", J.toJSON expiry), ("moves", J.Number 0), ("error", J.Null), ("registerTx", maybe J.Null (J.String . hex) hash)]
    written <- stored ctx r
    (nrPhase written, nrLive written, nrVersion written, nrLabelHash written) `shouldBe` (PRegistered, False, nrVersion r + 1, nrLabelHash r)
  pcSpentAt <$> codeRow ctx code `shouldReturn` Just orderTime
  to <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyB notLanded to
  fieldsIn v [("phase", "queued"), ("moves", J.Number 1)]

testMoveLandedReleasedCode :: IO ()
testMoveLandedReleasedCode = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  spender <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 20} id
  (unused, unusedCode) <- codeIn ctx 7 dynamisOrder failedPhase
  (spentElsewhere, spentCode) <- codeIn ctx 8 aliceOrder failedPhase
  let expiry = seconds orderTime + 2 * 31536000
  forM_ [("dynamis", unused), ("alicebakery", spentElsewhere)] $ \(label, r) -> do
    answerExpiry ctx label expiry
    answerOwner ctx label (nrOwner r)
  forM_ [(unused, unusedCode), (spentElsewhere, spentCode)] $ \(r, code) ->
    withTransaction cStore (\db -> releasePrepaidCode db (pcId code) (nrId r)) `shouldReturn` True
  withTransaction cStore $ \db -> do
    reservePrepaidCode db (pcId spentCode) (nrId spender) orderTime `shouldReturn` True
    spendPrepaidCode db (pcId spentCode) (nrId spender) orderTime `shouldReturn` True
  spenderBefore <- stored ctx spender
  spentBefore <- codeRow ctx spentCode
  to <- moveTo ctx "alicebakers" 10
  fst <$> move ctx keyA unused to `shouldReturn` 409
  nrPhase <$> stored ctx unused `shouldReturn` PRegistered
  (pcReservedBy &&& pcSpentAt) <$> codeRow ctx unusedCode `shouldReturn` (Just (nrId unused), Just orderTime)
  to' <- moveTo ctx "alicebakers" 11
  logs <- capturingStderr $ fst <$> move ctx keyA spentElsewhere to' `shouldReturn` 409
  nrPhase <$> stored ctx spentElsewhere `shouldReturn` PRegistered
  length (filter (\l -> "[ERROR " `T.isInfixOf` l && hex (pcCodeHash spentCode) `T.isInfixOf` l && idText (nrId spentElsewhere) `T.isInfixOf` l) $ T.lines logs) `shouldBe` 1
  codeRow ctx spentCode `shouldReturn` spentBefore
  stored ctx spender `shouldReturn` spenderBefore
  where
    idText (RegistrationId t) = t

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
  balanceOf ctx cheap `shouldReturn` Nothing
  dear <- paidIn ctx (byB dynamisOrder) failedPhase
  toCheaper <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyB dear toCheaper
  (field "price" v, field "phase" v, field "fromBalance" v) `shouldBe` (Just (J.Number 10000), Just "queued", Just (J.Number 0))
  field "balance" v `shouldBe` Just (J.object ["amount" J..= (10000 :: Int), "held" J..= False, "heldUntil" J..= J.Null, "refunded" J..= (0 :: Int)])
  nrPrice <$> stored ctx dear `shouldReturn` 10000
  balanceOf ctx dear `shouldReturn` Just (10000, 10000)

testCheaperMoveLostRace :: IO ()
testCheaperMoveLostRace = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx dynamisOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  -- held at the register check's ownerOf read, inside the turns
  (moving, gate) <- pausedAt cNode "eth_call" $ move ctx keyA r to
  changed <- rewrite ctx r id
  releaseGate gate
  fst <$> within "the move" (wait moving) `shouldReturn` 409
  stored ctx r `shouldReturn` changed
  balanceOf ctx r `shouldReturn` Nothing

-- | dynamis for 20000, 5000 of it from another order's balance, moved to alicebakers for 10000.
testMoveKeepsFromBalance :: IO ()
testMoveKeepsFromBalance = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  paying <- paidIn ctx (byB aliceOrder) failedPhase
  withTransaction cStore $ \db -> creditBalance db (nrId paying) 5000 orderTime
  let fields = OrderFields {ofLabelHash = labelHash "dynamis", ofLength = 7, ofDuration = yearsDuration 2 orderTime, ofCommitment = commitmentOf 3, ofSealed = "sealed-dynamis-3", ofAgreedTerms = termsVersion}
      new = NewRegistration {nrgId = RegistrationId "diff", nrgOwner = ownerA, nrgOrder = fields, nrgYears = 2}
  withTransaction cStore $ \db -> do
    insertInvoiceRow db "inv-diff" (addUTCTime 3600 orderTime) "open"
    createPaidRegistration db new (OTBalance (nrId paying) 20000 5000 (Just btc)) (paidOrder "inv-diff") {poPrice = 20000} orderTime `shouldReturn` Right ()
  settleNow ctx (InvoiceId "inv-diff") settled `shouldReturn` Right ISPaid
  r <- registrationById ctx (RegistrationId "diff") >>= \d -> rewrite ctx d failedPhase
  to <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyA r to
  (field "price" v, field "fromBalance" v) `shouldBe` (Just (J.Number 10000), Just (J.Number 5000))
  balanceOf ctx r `shouldReturn` Just (10000, 10000)
  balanceOf ctx paying `shouldReturn` Just (0, 5000)

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
  forM_ [KM.insert "labelHash" (J.String $ T.toUpper $ hex $ (\(LabelHash h) -> h) $ ofLabelHash to) valid, KM.delete "signature" valid, KM.insert "length" "11" valid, KM.delete "terms" valid] $ \malformed ->
    postMove ctx (nrId r) malformed `shouldReturn` refusedWith 400 "bad_request"
  forM_ malformedTerms $ \t -> move ctx keyA r to {ofAgreedTerms = t} `shouldReturn` refusedWith 400 "bad_request"
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
  -- held at the register check's ownerOf read, inside the turns
  (moving, gate) <- pausedAt cNode "eth_call" $ move ctx keyA r to
  changed <- rewrite ctx r id
  releaseGate gate
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

testPollsWoken :: IO ()
testPollsWoken = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  poll <- held ctx r
  to <- moveTo ctx "alicebakers" 10
  void $ moved ctx keyA r to
  (status, v) <- woken poll
  (status, field "version" v, field "phase" v) `shouldBe` (200, Just (J.toJSON $ nrVersion r + 1), Just "queued")
