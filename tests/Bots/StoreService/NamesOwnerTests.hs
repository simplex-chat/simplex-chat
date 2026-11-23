{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesOwnerTests
  ( namesOwnerTests,
    answerDisputeUntil,
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
    ownerOfSelector,
    paidIn,
    postFind,
    postSelection,
    rewrite,
    select,
    selectionBody,
    selectionOf,
    sentRegister,
    stored,
  )
where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (registrarOf, rentPriceAnswer, rentPriceCall, threeYearsOf11)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesStoreTests (hashFixture, order, ownerA, reveal, t0)
import Bots.StoreService.NamesPhasesTests (held, registrationPath, seenVersion, woken)
import Bots.StoreService.WebTests (StubCall (..), agreed, answerFixture, bounded, fieldOf, stubCalls, jsonObject, refusedWith, phaseOf, phaseValue, storeFixture, webGet, withApp)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, void, (<=<))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.IORef (readIORef)
import Data.Foldable (toList)
import Data.List (find, sort, sortOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Data.Word (Word64, Word8)
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Abi (addressWord, hex, selector, uint256)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Codes (CodeState (..), codeState)
import StoreService.Names.Orders (OrdersEnv (..), signedRequestSeconds)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..), findMessage, parseAnyCaseAddress, personalDigest, selectionMessage)
import StoreService.Names.Store hiding (findLimit)
import StoreService.Names.Turns (withLabelTurn)
import StoreService.Names.View (addressText)
import StoreService.Store (RevokeResult (..))
import StoreService.Web.Http (createLimit, findLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Owner (findHandler, selectNameHandler)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import StoreService.Web.Server (WebEnv (..), webApp)
import Test.Hspec

namesOwnerTests :: Spec
namesOwnerTests = describe "StoreService names: owner" $ do
  describe "find" $ do
    it "lists the owner's orders newest first, one with a name selected after a reset, an unpaid and a lapsed one included, at most 50, and none for an owner of none" testFind
    it "refuses another signer's signature, a stale timestamp and a malformed body" testFindRefused
  describe "selection of a name" $ do
    it "refuses a selection for every phase but taken, a failed order and a lapsed one included" testSelectRefusedPhases
    it "selects a name for a taken order however long after its reset, its taken phase in the view having no other field" testSelectAnyTime
    it "selects a name for a taken order at its price, spending its balance, its own name too, and no name another paid order holds" testSelectAccepted
    it "takes a stored link the link check now refuses, as accepted at the order, and refuses another such link" testSelectStoredLink
    it "cancels another buyer's open invoice for the name once a selection holds it" testSelectHoldsName
    it "answers a body equal to the current fields with the view unchanged, whatever its timestamp" testSelectRepeat
    it "selects a name by a body differing only in terms, storing them" testSelectTermsOnly
    it "answers terms_changed for an old version, other terms or the other kind, listed or not, a body differing only in terms included, and selects under the community terms, storing them" testSelectTermsChanged
    it "checks a code order's community name for 2 years, and its next name for its code's years again" testSelectCommunityYears
    it "prices a paid order's community name for 2 years, whatever years it paid for" testSelectCommunityPaidYears
    it "checks the terms after the timestamp and the duration, at the first chain read, answering terms_unavailable while they are not set" testSelectTermsCheckOrder
    it "selects a name again after each reset, as often as the order is reset" testSelectAgain
    it "refuses a selection meeting another live order of the wallet's or the commitment with a bare order_exists before its checks, and closes an overdue order of the same wallet and name, as a new order does" testSelectMeetsOrder
    it "runs the order checks on the new fields: invalid_name, the duration, name_taken and name_check_unavailable" testSelectOrderChecks
    it "answers a name of another price catalog_changed with its price, one priced under the floor name_check_unavailable, a balance short of the price bad_request, and selects at the order's price, spent from its balance" testSelectPrice
    it "checks a code order's code as a new code order's: code_not_covering for a shorter name, and reserves it for the name selected" testSelectCodeOrder
    it "accepts the code the order holds past its expiry until a month after the take, then refuses it with code_expired, refuses it once revoked with code_invalid, changing nothing, and keeps it from other code orders, checked as expired once refused" testSelectRefusedCode
    it "refuses sealed over 11095 characters, a body over 16384 bytes and malformed fields, accepting the longest" testSelectSizes
    it "refuses another signer's signature and a stale timestamp, and answers an unknown order 404" testSelectRefusedSigner
    it "answers registration_changed with the current view when the order changed during the checks, writing nothing" testSelectLostRace
    it "takes both turns in hash order when the order on A selects B while the order on B selects A, without deadlock, each refused as a paid order holds the other's name" testSelectionsCross
  describe "polls" $
    it "wakes a held registration poll by a selection of a name" testPollsWoken

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
serveOwner ctx@Ctx {cOrders = orders, cWeb = web, cHandlers} action = do
  let routes path = ownerRoutes orders web path <|> codeRoutes orders (weProviders web) (weWaiters web) path
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
  withApp (recordingHandlers cHandlers app) $ \client -> action ctx {cClient = client}

ownerRoutes :: OrdersEnv -> WebEnv -> [Text] -> Maybe Route
ownerRoutes orders WebEnv {weHttp, weProviders, weWaiters} = \case
  ["api", "registration", "find"] -> Just Route {rMethod = "POST", rLimit = findLimit, rHandler = findHandler orders}
  ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler orders weHttp holdMicros (RegistrationId rid)}
  ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = selectNameHandler orders weProviders weWaiters (RegistrationId rid)}
  _ -> Nothing

signWith :: Ctx -> Secp256k1PrivateKey -> ByteString -> IO Text
signWith Ctx {cRandom} key msg = hex . smpEncode . either (error . show) id <$> signRecoverable cRandom key (personalDigest msg)

clockSeconds :: Ctx -> IO Word64
clockSeconds Ctx {cClock} = seconds <$> readIORef cClock

signedWindow :: Word64
signedWindow = fromInteger signedRequestSeconds

byB :: Order -> Order
byB o = o {oOwner = ownerBText, oKey = keyB}

dynamisOrder :: Order
dynamisOrder = aliceOrder {oLabel = "dynamis", oSecret = secretOf 3, oSealed = "sealed-dynamis-3", oPrice = 20000}

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

answerDisputeUntil :: Ctx -> Text -> Word64 -> IO ()
answerDisputeUntil Ctx {cNode} label t = answerCall cNode (registrarOf cNode) (selector "disputeUntil(uint256)" <> labelBytes label) $ Returns $ uint256 t

-- | A register transaction of the order as the sender saves it, then moved to this outcome.
sentRegister :: Ctx -> NameRegistration -> Word8 -> TxSettlement -> IO TxHash
sentRegister Ctx {cStore} NameRegistration {nrId} n settlement = withTransaction cStore $ \db -> do
  let h = TxHash $ hashFixture n
  void $ insertTransaction db (TxRegistration RTRegister nrId) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = fromIntegral n} orderTime
  recordTransactionOutcome db h TSPending settlement `shouldReturn` True
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

-- | A paid order, settled, then written as the worker would write it, without the signature its order stored, as after a rare path cleared it.
paidIn :: Ctx -> Order -> (NameRegistration -> NameRegistration) -> IO NameRegistration
paidIn ctx o change = do
  (invId, _) <- placed ctx o
  settleNow ctx invId settled `shouldReturn` ISPaid
  r <- invoiceRegistration ctx invId
  rewrite ctx r (change . unsigned)

-- | A code order of a code of this length, then written as paidIn writes it.
codeIn :: Ctx -> Int -> Order -> (NameRegistration -> NameRegistration) -> IO (NameRegistration, PrepaidCode)
codeIn ctx len o change = do
  (text, code) <- issueCode ctx len 2 id
  r <- registrationById ctx =<< registered ctx text o
  (,code) <$> rewrite ctx r (change . unsigned)

unsigned :: NameRegistration -> NameRegistration
unsigned r = r {nrReveal = Nothing}

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
balanceOf :: Ctx -> NameRegistration -> IO (Maybe (USDCents, USDCents))
balanceOf Ctx {cStore} NameRegistration {nrId} = fmap (\Balance {blAmount, blCredited} -> (blAmount, blCredited)) <$> withTransaction cStore (`getBalance` nrId)

stored :: Ctx -> NameRegistration -> IO NameRegistration
stored ctx = registrationById ctx . nrId

failedPhase, registeredPhase, committedPhase, takenPhase :: NameRegistration -> NameRegistration
failedPhase r = r {nrPhase = OFailed orderTime FRRegistryRefused}
registeredPhase r = r {nrPhase = ORegistered (RoundedSystemTime $ fromIntegral $ seconds orderTime + 3 * 31536000) Nothing orderTime}
committedPhase r = r {nrPhase = OCommitted committedCommit}
takenPhase r = r {nrPhase = OTaken orderTime}

-- | A paid order reset to taken as the taken write sets it, its price credited to its balance.
takenIn :: Ctx -> Order -> (NameRegistration -> NameRegistration) -> IO NameRegistration
takenIn ctx o change = paidIn ctx o change >>= retaken ctx

retaken :: Ctx -> NameRegistration -> IO NameRegistration
retaken ctx@Ctx {cStore} r = do
  t <- rewrite ctx r takenPhase
  t <$ withTransaction cStore (\db -> case nrPayment t of
    OPInvoice {} -> creditBalance db (nrId t) (nrPrice t) orderTime
    OPCode _ -> pure ())

committedCommit :: Commit
committedCommit = Commit {cmTx = Just $ TxRef (TxHash $ hashFixture 8) 100, cmRevealAfter = RoundedSystemTime $ fromIntegral $ seconds orderTime + 60, cmRevealBefore = RoundedSystemTime $ fromIntegral $ seconds orderTime + 86400}

-- | The fields of a name to select, with the page's duration for 2 years at the service's clock.
selectionOf :: Ctx -> Text -> Int -> IO Order
selectionOf Ctx {cClock} label n = do
  t <- readIORef cClock
  pure aliceOrder {oLabel = label, oSecret = secretOf n, oSealed = "sealed-" <> label <> "-" <> tshow n, oDuration = yearsDuration 2 t}

selectionBody :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> Order -> IO J.Object
selectionBody ctx key rid o = clockSeconds ctx >>= selectionBodyAt ctx key rid o

selectionBodyAt :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> Order -> Word64 -> IO J.Object
selectionBodyAt ctx@Ctx {cRandom} key rid o timestamp = do
  owner <- addressFromPrivateKey cRandom key
  let signed = o {oKey = key, oOwner = addressText owner, oTimestamp = timestamp}
      fields = fieldsOf signed
  sig <- signWith ctx key $ selectionMessage rid fields timestamp
  withReveal cRandom signed $ KM.fromList [("fields", fieldsJSON fields), ("timestamp", J.toJSON timestamp), ("signature", J.String sig)]

postSelection :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
postSelection ctx rid = post ctx (registrationPath rid <> "/name") . J.encode

select :: Ctx -> Secp256k1PrivateKey -> NameRegistration -> Order -> IO (Int, Maybe J.Object)
select ctx key NameRegistration {nrId} o = selectionBody ctx key nrId o >>= postSelection ctx nrId

-- | The view of an accepted selection, failing the test otherwise.
selected :: Ctx -> Secp256k1PrivateKey -> NameRegistration -> Order -> IO J.Object
selected ctx key r o =
  select ctx key r o >>= \case
    (200, Just v) -> pure v
    other -> error $ "selection refused: " <> show other

getView :: Ctx -> NameRegistration -> IO J.Object
getView ctx NameRegistration {nrId} = webGet (cClient ctx) (registrationPath nrId) >>= jsonObject

findBody :: Ctx -> Secp256k1PrivateKey -> Text -> Word64 -> IO J.Object
findBody ctx key owner timestamp = do
  sig <- signWith ctx key $ findMessage (either (error . show) id $ parseAnyCaseAddress owner) timestamp
  pure $ KM.fromList [("owner", J.String owner), ("timestamp", J.toJSON timestamp), ("signature", J.String sig)]

postFind :: Ctx -> J.Object -> IO (Int, Maybe J.Object)
postFind ctx = post ctx "/api/registration/find" . J.encode

fieldsIn :: J.Object -> [(Text, J.Value)] -> Expectation
fieldsIn v = mapM_ (\(k, value) -> (k, fieldOf v k) `shouldBe` (k, Just value))

refusedWithView :: Text -> J.Object -> (Int, Maybe J.Object)
refusedWithView tag view = (409, Just $ KM.fromList [("type", J.String tag), ("registration", J.Object view)])

labelHashText :: Text -> J.Value
labelHashText label = J.String $ hex lh
  where
    LabelHash lh = labelHash label

codeRow :: Ctx -> PrepaidCode -> IO PrepaidCode
codeRow Ctx {cStore} PrepaidCode {pcId} = withTransaction cStore (`getPrepaidCodeById` pcId)

-- tests

testFind :: IO ()
testFind = withOwner $ \ctx@Ctx {cStore} -> do
  forM_ [1 .. 50 :: Int] $ \i -> do
    (_, code) <- issueCode ctx 8 2 id
    let o = order ("alicebakery" <> tshow i) (fromIntegral i)
        new = NewRegistration {nrgId = oldOrder i, nrgOwner = ownerA, nrgOrder = o, nrgReveal = reveal, nrgYears = 2, nrgOrderHash = orderHash ownerA o (OTCode $ pcCodeHash code)}
    withTransaction cStore (\db -> createCodeRegistration db new code (addUTCTime (fromIntegral i) t0)) `shouldReturn` Right ()
  answerFree ctx "alicebakers"
  answerFree ctx "alicebakeri"
  selectedOrder <- takenIn ctx aliceOrder {oLabel = "alicebakeri", oSecret = secretOf 20, oSealed = "sealed-alicebakeri-20"} id
  to <- selectionOf ctx "alicebakers" 10
  void $ selected ctx keyA selectedOrder to
  (InvoiceId invId, _) <- placed ctx aliceOrder
  paid <- invoiceRegistration ctx (InvoiceId invId)
  advance ctx 1
  lapsed <- paidIn ctx dynamisOrder $ \r -> r {nrPhase = OCommitted committedCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral $ seconds orderTime}}
  findBody ctx keyA (T.toLower ownerAText) (seconds orderTime) >>= postFind ctx >>= \case
    (200, Just v) | Just (J.Array found) <- KM.lookup "registrations" v -> do
      let entries = map objectEntry $ toList found
      map (`fieldOf` "registrationId") entries `shouldBe` map (Just . idText) (nrId lapsed : nrId paid : nrId selectedOrder : [oldOrder i | i <- [50, 49 .. 4]])
      case entries of
        lapsedEntry : unpaidEntry : selectedEntry : _ -> do
          (fieldOf lapsedEntry "phase", namePhase $ nrPhase lapsed) `shouldBe` (Just "committed", PCommitted)
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
          selectedEntry
            `shouldBe` KM.fromList
              [ ("registrationId", idText $ nrId selectedOrder),
                ("invoiceId", J.toJSON $ paymentInvoice $ nrPayment selectedOrder),
                ("phase", "queued"),
                ("createdAt", J.toJSON orderTime),
                ("labelHash", labelHashText "alicebakers"),
                ("sealed", "sealed-alicebakers-10")
              ]
        _ -> expectationFailure "fewer than three entries"
    other -> expectationFailure $ "find refused: " <> show other
  none <- findBody ctx keyB ownerBText (seconds orderTime) >>= postFind ctx
  storeFixture "find-none" (fst none) (J.Object <$> snd none) Nothing
  none `shouldBe` (200, Just $ KM.fromList [("registrations", J.Array mempty)])
  forM_ [1, 2] $ \i -> do
    (_, code) <- issueCode ctx 8 2 id
    let o = order ("bobbakery" <> tshow i) (fromIntegral $ 100 + i)
        new = NewRegistration {nrgId = listedOrder i, nrgOwner = ownerB, nrgOrder = o, nrgReveal = reveal, nrgYears = 2, nrgOrderHash = orderHash ownerB o (OTCode $ pcCodeHash code)}
    withTransaction cStore (\db -> createCodeRegistration db new code (addUTCTime (fromIntegral i) t0)) `shouldReturn` Right ()
  void $ registrationById ctx (listedOrder 1) >>= \r -> rewrite ctx r failedPhase
  listed <- findBody ctx keyB ownerBText (seconds orderTime) >>= postFind ctx
  storeFixture "find-orders" (fst listed) (J.Object <$> snd listed) Nothing
  case listed of
    (200, Just v) | Just (J.Array found) <- KM.lookup "registrations" v ->
      map ((\e -> (fieldOf e "registrationId", fieldOf e "phase")) . objectEntry) (toList found) `shouldBe` [(Just "listed2", Just "queued"), (Just "listed1", Just "failed")]
    other -> expectationFailure $ "find refused: " <> show other
  where
    ownerB = address "0x39ad11775d14FC8c9118Bd94f374DF3c7ad5655d"
    listedOrder i = RegistrationId $ "listed" <> tshow (i :: Int)
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

testSelectRefusedPhases :: IO ()
testSelectRefusedPhases = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  to <- selectionOf ctx "alicebakers" 10
  r <- paidIn ctx aliceOrder registeredPhase
  select ctx keyA r to `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  failedOrder <- paidIn ctx dynamisOrder failedPhase
  answerFree ctx "carolbakery"
  lapsed <- paidIn ctx aliceOrder {oLabel = "carolbakery", oSecret = secretOf 2} $ \x -> x {nrPhase = OCommitted committedCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral $ seconds orderTime - 1}}
  forM_ [failedOrder, lapsed] $ \x -> do
    select ctx keyA x to `shouldReturn` refusedWith 400 "bad_request"
    stored ctx x `shouldReturn` x

testSelectAnyTime :: IO ()
testSelectAnyTime = withOwner $ \ctx -> do
  forM_ ["alicebakers", "carolbakery", "dianabakery"] $ answerFree ctx
  r <- takenIn ctx aliceOrder id
  (codeText, _) <- issueCode ctx 8 2 id
  codeTaken <- registered ctx codeText aliceOrder {oLabel = "carolbakery", oSecret = secretOf 2, oSealed = "sealed-carolbakery-2"} >>= registrationById ctx >>= retaken ctx
  let takenPhaseView = Just (phaseValue "taken" [])
  v <- getView ctx r
  fieldOf v "phase" `shouldBe` takenPhaseView
  codeView <- getView ctx codeTaken
  fieldOf codeView "phase" `shouldBe` takenPhaseView
  storeFixture "registration-taken-code" 200 (Just $ J.Object $ KM.insert "id" "registration" codeView) Nothing
  advance ctx (31 * 86400)
  to <- selectionOf ctx "alicebakers" 11
  void $ selected ctx keyA r to
  toCode <- selectionOf ctx "dianabakery" 11
  void $ selected ctx keyA codeTaken toCode

testSelectHoldsName :: IO ()
testSelectHoldsName = withOwner $ \ctx@Ctx {cStore, cStub} -> do
  answerFree ctx "alicebakers"
  (aliceInv, _) <- placed ctx aliceOrder {oLabel = "alicebakers", oSecret = secretOf 9}
  aliceRef <- maybe (error "no invoice row") irProviderRef <$> getInvoice cStore aliceInv
  bobTaken <- takenIn ctx (byB aliceOrder) {oSecret = secretOf 2} id
  to <- selectionOf ctx "alicebakers" 10
  void $ selected ctx keyB bobTaken to
  calls <- stubCalls cStub
  [r | StubCancel r <- calls] `shouldBe` [aliceRef]
  nrPhase <$> invoiceRegistration ctx aliceInv `shouldReturn` OClosed ICCancelled

testSelectStoredLink :: IO ()
testSelectStoredLink = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  let refusedNow = "https://smp6.simplex.im/a#" <> T.replicate 1024 "a"
  r <- takenIn ctx aliceOrder $ \x -> x {nrReveal = Just reveal {rvContactLink = Just refusedNow}}
  to <- selectionOf ctx "alicebakers" 10
  select ctx keyA r to {oContact = Just $ refusedNow <> "b"} `shouldReturn` (400, Just $ KM.fromList [("type", "invalid_link"), ("kind", "contact")])
  stored ctx r `shouldReturn` r
  v <- selected ctx keyA r to {oContact = Just refusedNow}
  fieldsIn v [("phase", phaseValue "queued" []), ("labelHash", labelHashText "alicebakers")]
  (rvContactLink <=< nrReveal) <$> stored ctx r `shouldReturn` Just refusedNow

testSelectAccepted :: IO ()
testSelectAccepted = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  takenOrder <- takenIn ctx aliceOrder id
  to <- selectionOf ctx "alicebakers" 10
  v <- selected ctx keyA takenOrder to
  fieldsIn v
    [ ("phase", phaseValue "queued" []),
      ("labelHash", labelHashText "alicebakers"),
      ("commitment", J.toJSON $ ofCommitment $ fieldsOf to),
      ("sealed", J.String $ oSealed to),
      ("duration", J.toJSON $ oDuration to),
      ("price", J.Number 10000),
      ("balance", J.object ["amount" J..= (0 :: Int), "refunded" J..= (0 :: Int)]),
      ("version", J.toJSON $ nrVersion takenOrder + 1),
      ("label", J.String "alicebakers"),
      ("commitTx", J.Null)
    ]
  written <- stored ctx takenOrder
  (nrPhase written, nrOrderHash written) `shouldBe` (OQueued, nrOrderHash takenOrder)
  balanceOf ctx takenOrder `shouldReturn` Just (0, 10000)
  void $ paidIn ctx (byB aliceOrder) {oSecret = secretOf 2} id
  takenDynamis <- takenIn ctx dynamisOrder id
  heldName <- selectionOf ctx "alicebakery" 12
  select ctx keyA takenDynamis heldName `shouldReturn` refusedWith 409 "name_taken"
  sameName <- selectionOf ctx "dynamis" 12
  v3 <- selected ctx keyA takenDynamis sameName
  fieldsIn v3 [("phase", phaseValue "queued" []), ("price", J.Number 20000)]

testSelectRepeat :: IO ()
testSelectRepeat = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  -- the current fields: nothing is selected
  (status, v) <- select ctx keyA r aliceOrder
  (status, phaseOf =<< v) `shouldBe` (200, Just "taken")
  stored ctx r `shouldReturn` r
  to <- selectionOf ctx "alicebakers" 10
  body <- selectionBody ctx keyA (nrId r) to
  first' <- postSelection ctx (nrId r) body
  fst first' `shouldBe` 200
  selectedOnce <- stored ctx r
  advance ctx 3600
  postSelection ctx (nrId r) body `shouldReturn` first'
  resigned <- selectionBody ctx keyA (nrId r) to
  postSelection ctx (nrId r) resigned `shouldReturn` first'
  setGeneralTerms (cNode ctx) fakeNewGeneralTermsText
  postSelection ctx (nrId r) body `shouldReturn` first'
  stored ctx r `shouldReturn` selectedOnce
  nrAgreedTerms selectedOnce `shouldBe` agreed plainTerms

testSelectTermsOnly :: IO ()
testSelectTermsOnly = withOwner $ \ctx -> do
  r <- takenIn ctx aliceOrder $ \p -> p {nrAgreedTerms = agreed olderTerms}
  (status, v) <- select ctx keyA r aliceOrder
  (status, phaseOf =<< v) `shouldBe` (200, Just "queued")
  nrAgreedTerms <$> stored ctx r `shouldReturn` agreed plainTerms

testSelectTermsChanged :: IO ()
testSelectTermsChanged = withOwnerAs (listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  select ctx keyA r aliceOrder {oSecret = secretOf 11, oTerms = communityTerms} `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  select ctx keyA r aliceOrder {oTerms = olderTerms} `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  to <- selectionOf ctx "alicebakers" 10
  select ctx keyA r to {oTerms = plainTerms} `shouldReturn` termsChanged fakeCommunityTermsText (Just bakers)
  stored ctx r `shouldReturn` r
  v <- selected ctx keyA r to {oTerms = communityTerms}
  fieldsIn v [("terms", J.toJSON $ agreed communityTerms), ("community", communityEntry $ Just bakers)]
  nrAgreedTerms <$> stored ctx r `shouldReturn` agreed communityTerms

testSelectCommunityYears :: IO ()
testSelectCommunityYears = withOwnerAs (listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  (code, _) <- issueCode ctx 8 5 id
  r <- registrationById ctx =<< registered ctx code aliceOrder {oDuration = yearsDuration 5 orderTime}
  taken <- retaken ctx r
  to <- (\o -> o {oTerms = communityTerms}) <$> selectionOf ctx "alicebakers" 10
  select ctx keyA taken to {oDuration = yearsDuration 5 orderTime} `shouldReturn` refusedWith 400 "bad_request"
  void $ selected ctx keyA taken to
  answerFree ctx "alicecakes"
  again <- retaken ctx r
  toPlain <- selectionOf ctx "alicecakes" 11
  void $ selected ctx keyA again toPlain {oDuration = yearsDuration 5 orderTime}
  (nrYears &&& nrDuration) <$> stored ctx r `shouldReturn` (5, yearsDuration 5 orderTime)

testSelectCommunityPaidYears :: IO ()
testSelectCommunityPaidYears = withOwnerAs (threeYears . listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  answerCall (cNode ctx) fakeController threeYearsOf11 $ Returns $ rentPriceAnswer 15000
  r <- takenIn ctx aliceOrder {oDuration = yearsDuration 3 orderTime, oPrice = 15000} id
  to <- (\o -> o {oTerms = communityTerms}) <$> selectionOf ctx "alicebakers" 10
  select ctx keyA r to `shouldReturn` (409, Just $ KM.fromList [("type", "catalog_changed"), ("price", J.Number 10000)])
  stored ctx r `shouldReturn` r
  balanceOf ctx r `shouldReturn` Just (15000, 15000)

testSelectTermsCheckOrder :: IO ()
testSelectTermsCheckOrder = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  to <- (\o -> o {oTerms = olderTerms}) <$> selectionOf ctx "alicebakers" 10
  ts <- clockSeconds ctx
  (selectionBodyAt ctx keyA (nrId r) to (ts - signedWindow - 1) >>= postSelection ctx (nrId r)) `shouldReturn` refusedWith 400 "bad_request"
  select ctx keyA r to {oDuration = oDuration to + 3601} `shouldReturn` refusedWith 400 "bad_request"
  setFault cNode "eth_call" HttpFailure
  select ctx keyA r to `shouldReturn` refusedWith 503 "name_check_unavailable"
  clearFault cNode "eth_call"
  setGeneralTerms cNode ""
  select ctx keyA r to `shouldReturn` (503, Just $ KM.fromList [("type", "terms_unavailable"), ("wait", J.Null)])
  setGeneralTerms cNode fakeGeneralTermsText
  answerReserved ctx "alicebakers" 1
  select ctx keyA r to `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  stored ctx r `shouldReturn` r

testSelectAgain :: IO ()
testSelectAgain = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  forM_ (zip [1 :: Int ..] ["alicebakers", "alicebakery", "alicebakers", "alicebakery"]) $ \(n, label) -> do
    to <- selectionOf ctx label (10 + n)
    v <- selected ctx keyA r to
    (phaseOf v, fieldOf v "labelHash") `shouldBe` (Just "queued", Just $ labelHashText label)
    retaken ctx r

testSelectMeetsOrder :: IO ()
testSelectMeetsOrder = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  (ownInv, _) <- placed ctx aliceOrder {oLabel = "alicebakers", oSecret = secretOf 9, oMethod = "card"}
  toOwn <- selectionOf ctx "alicebakers" 10
  select ctx keyA r toOwn `shouldReturn` refusedWith 409 "order_exists"
  let bobs = (byB aliceOrder) {oLabel = "alicebakers", oSecret = secretOf 4}
  void $ placed ctx bobs
  toForeign <- (\o -> o {oCommitment = Just $ ofCommitment $ fieldsOf bobs}) <$> selectionOf ctx "alicebakers" 4
  select ctx keyA r toForeign `shouldReturn` refusedWith 409 "order_exists"
  stored ctx r `shouldReturn` r
  advance ctx (61 * 60)
  toOwnLater <- selectionOf ctx "alicebakers" 11
  void $ selected ctx keyA r toOwnLater
  nrPhase <$> invoiceRegistration ctx ownInv `shouldReturn` OClosed ICExpired

testSelectOrderChecks :: IO ()
testSelectOrderChecks = withOwner $ \ctx@Ctx {cNode} -> do
  r <- takenIn ctx aliceOrder id
  to <- selectionOf ctx "alicebakers" 10
  let withLength len = selectionBody ctx keyA (nrId r) to >>= postSelection ctx (nrId r) . inObject "fields" (KM.insert "length" (J.Number len))
  withLength 5 `shouldReturn` refusedWith 400 "invalid_name"
  withLength 64 `shouldReturn` refusedWith 400 "invalid_name"
  answerFree ctx "alicebakers"
  select ctx keyA r to {oDuration = oDuration to + 3601} `shouldReturn` refusedWith 400 "bad_request"
  answerReserved ctx "alicebakers" 1
  select ctx keyA r to `shouldReturn` refusedWith 409 "name_taken"
  answerCall cNode fakeNamePolicy (reservedNamesSelector <> labelBytes "alicebakers") $ Reverts ""
  select ctx keyA r to `shouldReturn` refusedWith 503 "name_check_unavailable"
  stored ctx r `shouldReturn` r

testSelectPrice :: IO ()
testSelectPrice = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  cheap <- takenIn ctx aliceOrder id
  toDynamis <- selectionOf ctx "dynamis" 10
  priceAnswer <- select ctx keyA cheap toDynamis
  priceAnswer `shouldBe` (409, Just $ KM.fromList [("type", "catalog_changed"), ("price", J.Number 20000)])
  answerFixture "select-catalog-changed" priceAnswer
  dear <- takenIn ctx (byB dynamisOrder) id
  toCheaper <- selectionOf ctx "alicebakers" 11
  select ctx keyB dear toCheaper `shouldReturn` (409, Just $ KM.fromList [("type", "catalog_changed"), ("price", J.Number 10000)])
  toSame <- selectionOf ctx "alicebakers" 12
  answerCallOnce (cNode ctx) fakeController (rentPriceCall 11) $ Returns $ rentPriceAnswer 9999
  select ctx keyA cheap toSame `shouldReturn` refusedWith 503 "name_check_unavailable"
  mapM_ (\r -> stored ctx r `shouldReturn` r) [cheap, dear]
  mapM (balanceOf ctx) [cheap, dear] `shouldReturn` [Just (10000, 10000), Just (20000, 20000)]
  withTransaction cStore (\db -> refundBalance db (nrId cheap) 10000 orderTime) `shouldReturn` True
  select ctx keyA cheap toSame `shouldReturn` refusedWith 400 "bad_request"
  stored ctx cheap `shouldReturn` cheap
  withTransaction cStore $ \db -> creditBalance db (nrId cheap) 10000 orderTime
  v <- selected ctx keyA cheap toSame
  (fieldOf v "price", phaseOf v, fieldOf v "payment") `shouldBe` (Just (J.Number 10000), Just "queued", Just (J.object ["type" J..= ("invoice" :: Text), "method" J..= ("btc" :: Text)]))
  fieldOf v "balance" `shouldBe` Just (J.object ["amount" J..= (0 :: Int), "refunded" J..= (10000 :: Int)])
  balanceOf ctx cheap `shouldReturn` Just (0, 20000)

testSelectCodeOrder :: IO ()
testSelectCodeOrder = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  (r, code) <- codeIn ctx 8 aliceOrder id >>= \(placedOrder, code) -> (,code) <$> retaken ctx placedOrder
  toShorter <- selectionOf ctx "dynamis" 10
  select ctx keyA r toShorter `shouldReturn` refusedWith 409 "code_not_covering"
  stored ctx r `shouldReturn` r
  to <- selectionOf ctx "alicebakers" 11
  v <- selected ctx keyA r to
  (phaseOf v, fieldOf v "price") `shouldBe` (Just "queued", Just (J.Number 0))
  (pcReservedBy &&& pcSpentAt) <$> codeRow ctx code `shouldReturn` (Just (nrId r), Nothing)

testSelectRefusedCode :: IO ()
testSelectRefusedCode = withOwner $ \ctx@Ctx {cStore} -> do
  let takenCodeIn n = do
        let label = "codeorder" <> tshow n
        answerFree ctx label
        (r, code) <- codeIn ctx 8 aliceOrder {oLabel = label, oSecret = secretOf (30 + n)} id >>= \(placedOrder, code) -> (,code) <$> retaken ctx placedOrder
        to <- selectionOf ctx label (40 + n)
        pure (r, code, to)
      expire PrepaidCode {pcCodeHash} = withTransaction cStore $ \db -> DB.execute db "UPDATE sx_badge_service_name_prepaid_codes SET expires_at = ? WHERE code_hash = ?" (orderTime, pcCodeHash)
  (expiredOrder, expired, toExpired) <- takenCodeIn 1
  expire expired
  phaseOf <$> selected ctx keyA expiredOrder toExpired `shouldReturn` Just "queued"
  pcReservedBy <$> codeRow ctx expired `shouldReturn` Just (nrId expiredOrder)
  (revokedOrder, revoked@PrepaidCode {pcCodeHash = revokedHash}, toRevoked) <- takenCodeIn 2
  fst <$> withTransaction cStore (\db -> revokePrepaidCode db revokedHash orderTime) `shouldReturn` Revoked (pcId revoked)
  row <- codeRow ctx revoked
  answer <- select ctx keyA revokedOrder toRevoked
  answer `shouldBe` refusedWith 400 "code_invalid"
  answerFixture "select-code-invalid" answer
  stored ctx revokedOrder `shouldReturn` revokedOrder
  codeRow ctx revoked `shouldReturn` row
  forM_ ["codeorder8", "codeorder9"] $ answerFree ctx
  (keptText, kept) <- issueCode ctx 8 2 id
  holder <- registered ctx keptText aliceOrder {oLabel = "codeorder8", oSecret = secretOf 38} >>= registrationById ctx >>= retaken ctx
  (signedCodeBody ctx keptText (byB aliceOrder) {oLabel = "codeorder9", oSecret = secretOf 39} >>= postRegister ctx) `shouldReturn` refusedWith 409 "code_reserved"
  pcReservedBy <$> codeRow ctx kept `shouldReturn` Just (nrId holder)
  (inMonthOrder, inMonth, _) <- takenCodeIn 3
  (lateOrder, late, _) <- takenCodeIn 4
  mapM_ expire [inMonth, late]
  advance ctx $ diffUTCTime (addMonths 1 orderTime) orderTime - 1
  toInMonth <- selectionOf ctx "codeorder3" 43
  phaseOf <$> selected ctx keyA inMonthOrder toInMonth `shouldReturn` Just "queued"
  advance ctx 1
  toLate <- selectionOf ctx "codeorder4" 44
  lateRow <- codeRow ctx late
  lateAnswer <- select ctx keyA lateOrder toLate
  lateAnswer `shouldBe` refusedWith 409 "code_expired"
  answerFixture "select-code-expired" lateAnswer
  stored ctx lateOrder `shouldReturn` lateOrder
  codeRow ctx late `shouldReturn` lateRow
  now <- readIORef (cClock ctx)
  mapM (fmap (codeState now) . codeRow ctx) [late, inMonth] `shouldReturn` [CSExpired, CSReserved]

testSelectSizes :: IO ()
testSelectSizes = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  to <- selectionOf ctx "alicebakers" 10
  let longest = to {oSealed = T.replicate 11095 "s"}
  body <- selectionBody ctx keyA (nrId r) longest
  postSelection ctx (nrId r) (KM.insert "owner" (J.String $ T.replicate 5300 "0") body) `shouldReturn` refusedWith 400 "bad_request"
  post ctx (registrationPath (nrId r) <> "/name") "[]" `shouldReturn` refusedWith 400 "bad_request"
  select ctx keyA r to {oSealed = T.replicate 11096 "s"} `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  -- an owner in the body is ignored
  (status, v) <- postSelection ctx (nrId r) (KM.insert "owner" (J.String ownerBText) body)
  (status, (`fieldOf` "sealed") =<< v, (`fieldOf` "owner") =<< v) `shouldBe` (200, Just (J.String $ oSealed longest), Just (J.String ownerAText))

testSelectRefusedSigner :: IO ()
testSelectRefusedSigner = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  to <- selectionOf ctx "alicebakers" 10
  select ctx keyB r to `shouldReturn` refusedWith 400 "bad_request"
  (clockSeconds ctx >>= \ts -> selectionBodyAt ctx keyA (nrId r) to (ts - signedWindow - 1) >>= postSelection ctx (nrId r)) `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  let unknown = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"
  (selectionBody ctx keyA unknown to >>= postSelection ctx unknown) `shouldReturn` refusedWith 404 "not_found"

testSelectLostRace :: IO ()
testSelectLostRace = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  (invId, _) <- placed ctx aliceOrder
  settleNow ctx invId settled `shouldReturn` ISPaid
  r <- invoiceRegistration ctx invId >>= retaken ctx
  to <- selectionOf ctx "alicebakers" 10
  -- held at its first chain read, inside the turns
  (selecting, gate) <- pausedAt cNode "eth_call" $ select ctx keyA r to
  changed <- rewrite ctx r id
  releaseGate gate
  answer <- bounded "the selection" $ wait selecting
  view <- getView ctx changed
  answer `shouldBe` refusedWithView "registration_changed" view
  answerFixture "select-registration-changed" answer
  fieldOf view "version" `shouldBe` Just (J.toJSON $ nrVersion r + 1)
  stored ctx r `shouldReturn` changed
  balanceOf ctx r `shouldReturn` Just (10000, 10000)

-- | Each round holds the higher hash's turn while both orders start, the one selecting a name from the lower hash first in odd
-- rounds and last in even ones, so that without the hash order some round has each order holding one turn and waiting
-- for the other's.
testSelectionsCross :: IO ()
testSelectionsCross = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  let (low, high) = case sortOn hashOf ["alicebakery", "alicebakers"] of
        [l, h] -> (l, h)
        _ -> error "two names"
  a <- takenIn ctx aliceOrder {oLabel = low, oSecret = secretOf 60} id
  b <- takenIn ctx (byB aliceOrder) {oLabel = high, oSecret = secretOf 61} id
  void $ paidIn ctx aliceOrder {oLabel = low, oSecret = secretOf 62} id
  void $ paidIn ctx (byB aliceOrder) {oLabel = high, oSecret = secretOf 63} id
  forM_ [1 .. 6 :: Int] $ \n -> do
    rs <- mapM (stored ctx) [a, b]
    let onName label = fromJust $ find ((== labelHash label) . nrLabelHash) rs
        keyOf r = if nrOwner r == nrOwner a then keyA else keyB
        bodyTo r label k = selectionOf ctx label (100 + 2 * n + k) >>= selectionBody ctx (keyOf r) (nrId r)
        (fromLow, fromHigh) = (onName low, onName high)
    lowSelection <- (fromLow,) <$> bodyTo fromLow high 0
    highSelection <- (fromHigh,) <$> bodyTo fromHigh low 1
    holding <- newEmptyMVar
    release <- newEmptyMVar
    holder <- async $ withLabelTurn (oeTurns $ cOrders ctx) (labelHash high) $ putMVar holding () >> takeMVar release
    takeMVar holding
    let started = if odd n then [lowSelection, highSelection] else [highSelection, lowSelection]
    selections <- forM (zip [1 ..] started) $ \(k, (r, body)) -> async (postSelection ctx (nrId r) body) <* blockedRequests ctx k
    putMVar release ()
    wait holder
    outcomes <- bounded ("round " <> show n) $ mapM wait selections
    outcomes `shouldBe` replicate 2 (refusedWith 409 "name_taken")
    mapM (fmap nrLabelHash . stored ctx) [a, b] `shouldReturn` map labelHash [low, high]
  where
    hashOf label = (\(LabelHash h) -> h) (labelHash label)

testPollsWoken :: IO ()
testPollsWoken = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- takenIn ctx aliceOrder id
  poll <- held ctx (0, 1) $ registrationPath (nrId r) <> seenVersion (nrVersion r)
  to <- selectionOf ctx "alicebakers" 10
  void $ selected ctx keyA r to
  (status, v) <- woken "the registration poll" poll
  (status, fieldOf v "version", phaseOf v) `shouldBe` (200, Just (J.toJSON $ nrVersion r + 1), Just "queued")
