{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesSpendTests (namesSpendTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (reservedNamesCall)
import Bots.StoreService.NamesOrdersTests (Ctx (..), Order (..), advance, aliceOrder, commitmentOf, field, fieldsOf, invoiceRegistration, keyB, orderTime, placeOrder, post, refusedWith, registrationById, registrationOf, settleNow, withOrdersVia, yearsDuration)
import Bots.StoreService.NamesStoreTests (at, order, ownerA, placePaid, reg, t0, write)
import Bots.StoreService.WebTests (StubCall (..), StubState (..), jsonObject, rcv, sampleProviderInvoice, statusOf, stubCalls, webGet, webRequestBody)
import Control.Concurrent.Async (Async, async, cancel, concurrently, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (join, replicateM_)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (signRecoverable)
import Simplex.Messaging.Eth.EIP712 (serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import StoreService.Names.Abi (hex, uint256)
import StoreService.Names.Balances (RefundRefusal (..), recordRefund)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), phasesWaiting)
import StoreService.Names.Signatures (OrderFields (..), personalDigest, spendMessage)
import StoreService.Names.Store
import StoreService.Names.View (balanceJSON)
import StoreService.Orders (cancelOpenInvoice)
import StoreService.Providers (Funded (..), OrderDraft (..), PaymentSignal (..), Provider (..), ProviderError (..))
import StoreService.Names.Turns (balanceTurnCount, labelTurnCount, withBalanceTurn)
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import StoreService.Web.Http (createLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Orders (spendHandler)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesSpendTests :: Spec
namesSpendTests = describe "StoreService names: spend" $ do
  describe "orders" $ do
    it "queues an order whose price the balance covers, spending the price and keeping the rest, or all of it" testCoveredSpend
    it "makes an invoice for the difference, the order holding all of the balance until the invoice's expiry" testDifferenceSpend
    it "answers an exact resend with its order in any phase, by its own hash, with a new timestamp and after the balance changed" testSpendResend
    it "keeps a spend and an /api/invoice order of the same fields apart, each meeting the other as order_exists" testInvoiceBodyApart
  describe "refusals" $ do
    it "answers balance_changed with the current balance for a stale amount, a held balance and an order with none" testBalanceChanged
    it "refuses a method with a covered price, none with a difference, and prices and balances not positive" testMethodRule
    it "refuses another signer and a stale timestamp, and answers an unknown order 404" testSignerAndId
    it "answers name_taken, catalog_changed, name_check_unavailable, provider_unavailable and order_exists, spending nothing" testOrderChecks
    it "runs its checks in the contract's order" testCheckOrder
    it "refuses sealed missing or over 11095 characters and a body over 16384 bytes, accepting the longest" testSpendSizes
    it "counts spends against the create limit" testSpendLimit
  describe "holders" $ do
    it "closes the balance's overdue unfunded holder in the spend's transaction, publishing it and cancelling its invoice" testOverdueHolderSpent
    it "goes through when the overdue holder was cancelled meanwhile" testHolderCancelledMeanwhile
    it "answers balance_changed when the overdue holder was funded meanwhile, cancelling the new invoice" testHolderFundedMeanwhile
  describe "races" $ do
    it "lets one of two concurrent spends of one balance win, the other answered balance_changed" testConcurrentSpends
    it "answers balance_changed and cancels the provider invoice when another store changed the balance after the check" testWriteRefusedAtBalance
    it "lets one of a spend and a refund of the balance win" testSpendRacesRefund
    it "takes the new name's turn and then the balance's" testSpendTurns

-- fixtures

-- | The spend route beside the registration poll and the service's routes; the hook runs as each provider invoice is created.
withSpendHook :: (IORef (IO ()) -> Ctx -> IO a) -> IO a
withSpendHook action = do
  hook <- newIORef (pure ())
  withOrdersVia (\p -> p {pCreateInvoice = \m d -> join (readIORef hook) >> pCreateInvoice p m d}) $ \ctx@Ctx {cOrders, cWeb} -> do
    let routes :: [Text] -> Maybe Route
        routes = \case
          ["api", "registration", rid, "spend"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = spendHandler cOrders (weConfig cWeb) (weProviders cWeb) (weWaiters cWeb) rid}
          ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler cOrders holdMicros rid}
          _ -> Nothing
        app req respond = fromMaybe (webApp cWeb req respond) (namesApp (weHttp cWeb) routes req respond)
    mgr <- newManager defaultManagerSettings
    Warp.testWithApplication (pure app) $ \port -> action hook ctx {cClient = (mgr, "http://127.0.0.1:" <> show port)}

withSpend :: (Ctx -> IO a) -> IO a
withSpend action = withSpendHook (const action)

-- | Run by the next provider invoice made, once.
onCreate :: IORef (IO ()) -> IO () -> IO ()
onCreate hook act = writeIORef hook (writeIORef hook (pure ()) >> act)

-- | A taken paid order of owner A with a balance of the cents.
sourceOf :: Ctx -> Text -> Int -> IO RegistrationId
sourceOf Ctx {cStore} rid cents = do
  placePaid cStore rid ownerA (order ("source-" <> rid) 1) {ofCommitment = C.sha256Hash $ encodeUtf8 rid} (at 60) `shouldReturn` Right ()
  taken <- reg cStore rid
  write cStore taken {nrPhase = PTaken} `shouldReturn` Written 2
  credit cStore rid cents
  pure $ RegistrationId rid

credit :: DBStore -> Text -> Int -> IO ()
credit st rid cents = withTransaction st $ \db -> creditBalance db (RegistrationId rid) cents t0

-- | What the page sends: the new order's fields and price, the balance it read, and a method only for a difference.
data SpendBody = SpendBody
  { sbOrder :: Order,
    sbBalance :: Int,
    sbMethod :: Maybe Text
  }

-- | alicebakery, at 10000.
covered :: Int -> SpendBody
covered balance = SpendBody {sbOrder = aliceOrder, sbBalance = balance, sbMethod = Nothing}

difference :: Int -> SpendBody
difference balance = (covered balance) {sbMethod = Just "btc"}

-- | 7 letters, at the launch price of 20000 for 2 years.
dynamisOrder :: Order
dynamisOrder = aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}

dynamis :: Int -> Maybe Text -> SpendBody
dynamis = SpendBody dynamisOrder

-- | Signed and timed at the clock's time, as a page acting later sends it.
timedAt :: UTCTime -> SpendBody -> SpendBody
timedAt now b@SpendBody {sbOrder} = b {sbOrder = sbOrder {oTimestamp = seconds now, oDuration = yearsDuration 2 now}}

spendBody :: Ctx -> RegistrationId -> SpendBody -> IO J.Object
spendBody Ctx {cRandom} source SpendBody {sbOrder = o@Order {oKey, oPrice, oTimestamp}, sbBalance, sbMethod} = do
  sig <- either (error . show) id <$> signRecoverable cRandom oKey (personalDigest $ spendMessage source fields oTimestamp)
  pure $
    KM.fromList $
      [ ("labelHash", J.String $ hex lh),
        ("length", J.toJSON ofLength),
        ("duration", J.toJSON ofDuration),
        ("commitment", J.String $ hex ofCommitment),
        ("sealed", J.String ofSealed),
        ("price", J.toJSON oPrice),
        ("balance", J.toJSON sbBalance),
        ("timestamp", J.toJSON oTimestamp),
        ("signature", J.String $ hex $ serializeSignature sig)
      ]
        <> maybe [] (\m -> [("method", J.String m)]) sbMethod
  where
    fields@OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed} = fieldsOf o

postSpend :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
postSpend ctx (RegistrationId rid) = post ctx ("/api/registration/" <> T.unpack rid <> "/spend") . J.encode

spend :: Ctx -> RegistrationId -> SpendBody -> IO (Int, Maybe J.Object)
spend ctx source b = spendBody ctx source b >>= postSpend ctx source

registrationAnswer :: Maybe J.Object -> RegistrationId
registrationAnswer = \case
  Just v | KM.keys v == ["registrationId"], Just (J.String rid) <- field "registrationId" v -> RegistrationId rid
  other -> error $ "not a registration id: " <> show other

invoiceIdOf :: Maybe J.Object -> InvoiceId
invoiceIdOf v = case field "invoiceId" =<< v of
  Just (J.String t) -> InvoiceId t
  other -> error $ "not an invoice view: " <> show other

registrationPath :: RegistrationId -> String
registrationPath (RegistrationId rid) = "/api/registration/" <> T.unpack rid

viewOf :: Ctx -> RegistrationId -> IO J.Object
viewOf Ctx {cClient} rid = webGet cClient (registrationPath rid) >>= jsonObject

-- | A registration poll at the order's version, once the server holds it.
heldPoll :: Ctx -> NameRegistration -> IO (Async (Int, J.Object))
heldPoll Ctx {cOrders, cClient} NameRegistration {nrId, nrVersion} = do
  waiting <- atomically $ phasesWaiting (oePhases cOrders)
  poll <- async $ webGet cClient (registrationPath nrId <> "?seenVersion=" <> show nrVersion) >>= \resp -> (statusOf resp,) <$> jsonObject resp
  timeout pollSeconds (atomically $ phasesWaiting (oePhases cOrders) >>= check . (> waiting)) >>= maybe (error "the poll was not held") pure
  pure poll

woken :: Async (Int, J.Object) -> IO (Int, J.Object)
woken poll = timeout pollSeconds (wait poll) >>= maybe (cancel poll >> error "the poll was not woken") pure

pollSeconds :: Int
pollSeconds = 5000000

balanceOf :: Ctx -> RegistrationId -> IO (Maybe Balance)
balanceOf Ctx {cStore, cClock} rid = readIORef cClock >>= \now -> withTransaction cStore $ \db -> getBalance db rid now

amountOf :: Ctx -> RegistrationId -> IO (Maybe Int)
amountOf ctx rid = fmap blAmount <$> balanceOf ctx rid

balanceChanged :: Maybe Balance -> (Int, Maybe J.Object)
balanceChanged b = (409, Just $ KM.fromList [("error", "balance_changed"), ("balance", maybe J.Null balanceJSON b)])

-- | The orders a balance paid.
paidFrom :: Ctx -> RegistrationId -> IO [NameRegistration]
paidFrom Ctx {cStore} source = filter ((== Just source) . nrBalanceSource) <$> withTransaction cStore (`findRegistrations` ownerA)

cancels :: Ctx -> IO [Text]
cancels Ctx {cStub} = (\calls -> [r | StubCancel r <- calls]) <$> stubCalls cStub

providerRef :: Ctx -> InvoiceId -> IO Text
providerRef Ctx {cStore} invId = maybe (error "no invoice row") irProviderRef <$> getInvoice cStore invId

seconds :: UTCTime -> Word64
seconds = floor . utcTimeToPOSIXSeconds

btc :: ServicePaymentMethod
btc = SPMCrypto CCBtc

-- tests

testCoveredSpend :: IO ()
testCoveredSpend = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 15000
  rid <- registrationAnswer . snd <$> spend ctx src (covered 15000)
  r <- registrationById ctx rid
  (nrPhase r, nrLive r, nrPrice r, nrPayment r, nrBalanceSource r, nrBalanceUsed r) `shouldBe` (PQueued, True, 10000, RPBalance, Just src, 10000)
  nrOrderHash r `shouldBe` orderHash ownerA (fieldsOf aliceOrder) (OTBalance src 10000 15000 Nothing)
  view <- viewOf ctx rid
  (field "phase" view, field "price" view, field "fromBalance" view, field "balance" view) `shouldBe` (Just "queued", Just (J.Number 10000), Just (J.Number 10000), Just J.Null)
  amountOf ctx src `shouldReturn` Just 5000
  exact <- sourceOf ctx "exact" 20000
  rid' <- registrationAnswer . snd <$> spend ctx exact (dynamis 20000 Nothing)
  nrBalanceUsed <$> registrationById ctx rid' `shouldReturn` 20000
  fmap (\b -> (blAmount b, blHold b)) <$> balanceOf ctx exact `shouldReturn` Just (0, Nothing)
  stubCalls (cStub ctx) `shouldReturn` []

testDifferenceSpend :: IO ()
testDifferenceSpend = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 6000
  (status, v) <- spend ctx src (difference 6000)
  status `shouldBe` 200
  let expiry = addUTCTime (45 * 60) orderTime
      r = registrationOf <$> v
  (field "status" =<< v, field "amount" =<< v, field "expiresAt" =<< v) `shouldBe` (Just "open", Just (J.Number 4000), Just (J.toJSON expiry))
  (field "phase" =<< r, field "price" =<< r, field "fromBalance" =<< r) `shouldBe` (Just "unpaid", Just (J.Number 10000), Just (J.Number 6000))
  stubCalls (cStub ctx) `shouldReturn` [StubCreate btc OrderDraft {odAmount = CurrencyAmount 4000, odCurrency = "usd"}]
  let invId = invoiceIdOf v
  holder <- invoiceRegistration ctx invId
  (nrPhase holder, nrLive holder, nrPrice holder, nrPayment holder, nrBalanceSource holder, nrBalanceUsed holder) `shouldBe` (PUnpaid, True, 10000, RPInvoice btc invId, Just src, 6000)
  nrOrderHash holder `shouldBe` orderHash ownerA (fieldsOf aliceOrder) (OTBalance src 10000 6000 (Just btc))
  fmap irAmount <$> getInvoice (cStore ctx) invId `shouldReturn` Just (CurrencyAmount 4000)
  field "balance" <$> viewOf ctx src `shouldReturn` Just (J.object ["amount" J..= (6000 :: Int), "held" J..= True, "heldUntil" J..= expiry, "refunded" J..= (0 :: Int)])

testSpendResend :: IO ()
testSpendResend = withSpend $ \ctx@Ctx {cStore, cClock} -> do
  src <- sourceOf ctx "src" 6000
  body <- spendBody ctx src (difference 6000)
  (200, first') <- postSpend ctx src body
  let invId = invoiceIdOf first'
  credit cStore "src" 1000
  advance ctx 60
  spend ctx src (difference 6000) {sbOrder = aliceOrder {oTimestamp = seconds orderTime + 60}} `shouldReturn` (200, first')
  advance ctx 700
  postSpend ctx src body `shouldReturn` (200, first')
  holder <- invoiceRegistration ctx invId
  now <- readIORef cClock
  Written _ <- withTransaction cStore $ \db -> updateRegistration db holder {nrPhase = PQueued} now
  (\(status, v) -> (status, invoiceIdOf v, field "phase" . registrationOf =<< v)) <$> postSpend ctx src body `shouldReturn` (200, invId, Just "queued")
  other <- sourceOf ctx "other" 25000
  let paid = timedAt now $ dynamis 25000 Nothing
  coveredBody <- spendBody ctx other paid
  (200, answer) <- postSpend ctx other coveredBody
  r <- registrationById ctx $ registrationAnswer answer
  spend ctx other paid {sbOrder = (sbOrder paid) {oTimestamp = seconds now + 1}} `shouldReturn` (200, answer)
  Written _ <- withTransaction cStore $ \db -> updateRegistration db r {nrPhase = PCommitted, nrRevealAfter = Just (seconds now), nrRevealBefore = Just (seconds now + 86400)} now
  advance ctx 3600
  postSpend ctx other coveredBody `shouldReturn` (200, answer)
  length <$> stubCalls (cStub ctx) `shouldReturn` 1
  amountOf ctx other `shouldReturn` Just 5000

testInvoiceBodyApart :: IO ()
testInvoiceBodyApart = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 6000
  fst <$> spend ctx src (difference 6000) `shouldReturn` 200
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 409 "order_exists"
  fst <$> placeOrder ctx dynamisOrder `shouldReturn` 200
  other <- sourceOf ctx "other" 30000
  spend ctx other (dynamis 30000 Nothing) `shouldReturn` refusedWith 409 "order_exists"
  amountOf ctx other `shouldReturn` Just 30000

testBalanceChanged :: IO ()
testBalanceChanged = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 10000
  stale <- balanceOf ctx src
  spend ctx src (covered 12000) `shouldReturn` balanceChanged stale
  spend ctx src (difference 9000) `shouldReturn` balanceChanged stale
  held <- sourceOf ctx "held" 6000
  fst <$> spend ctx held (dynamis 6000 (Just "btc")) `shouldReturn` 200
  holding <- balanceOf ctx held
  (fmap bhUntil . blHold =<< holding, fmap bhOverdue . blHold =<< holding) `shouldBe` (Just (addUTCTime (45 * 60) orderTime), Just False)
  spend ctx held (difference 6000) `shouldReturn` balanceChanged holding
  placePaid (cStore ctx) "none" ownerA (order "source-none" 3) (at 60) `shouldReturn` Right ()
  spend ctx (RegistrationId "none") (covered 10000) `shouldReturn` balanceChanged Nothing
  length <$> stubCalls (cStub ctx) `shouldReturn` 1
  paidFrom ctx src `shouldReturn` []

testMethodRule :: IO ()
testMethodRule = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 6000
  good <- spendBody ctx src (difference 6000)
  let with :: J.Key -> J.Value -> J.Object
      with k v = KM.insert k v good
  mapM_
    (\body -> postSpend ctx src body `shouldReturn` refusedWith 400 "bad_request")
    [ KM.delete "method" good,
      with "method" "eth",
      with "method" J.Null,
      with "price" (J.Number 0),
      with "price" (J.Number 100.5),
      with "balance" (J.Number 0),
      with "balance" (J.Number (-1)),
      KM.delete "balance" good
    ]
  spend ctx src (covered 6000) {sbOrder = aliceOrder {oPrice = 6000}, sbMethod = Just "btc"} `shouldReturn` refusedWith 400 "bad_request"
  spend ctx src (difference 6000) {sbMethod = Nothing} `shouldReturn` refusedWith 400 "bad_request"
  mapM_ (\len -> postSpend ctx src (with "length" (J.Number len)) `shouldReturn` refusedWith 400 "invalid_name") [5, 64]
  stubCalls (cStub ctx) `shouldReturn` []
  amountOf ctx src `shouldReturn` Just 6000

testSignerAndId :: IO ()
testSignerAndId = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 15000
  spend ctx src (covered 15000) {sbOrder = aliceOrder {oKey = keyB}} `shouldReturn` refusedWith 400 "bad_request"
  mapM_
    (\offset -> spend ctx src (covered 15000) {sbOrder = aliceOrder {oTimestamp = fromInteger (toInteger (seconds orderTime) + offset)}} `shouldReturn` refusedWith 400 "bad_request")
    [-601, 601]
  spend ctx (RegistrationId "unknown") (covered 15000) `shouldReturn` refusedWith 404 "not_found"
  amountOf ctx src `shouldReturn` Just 15000
  paidFrom ctx src `shouldReturn` []

testOrderChecks :: IO ()
testOrderChecks = withSpend $ \ctx@Ctx {cNode = node, cStub} -> do
  src <- sourceOf ctx "src" 30000
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (1 :: Int)
  spend ctx src (covered 30000) `shouldReturn` refusedWith 409 "name_taken"
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oPrice = 9000}} `shouldReturn` (409, Just $ KM.fromList [("error", "catalog_changed"), ("price", J.Number 10000)])
  setFault node "eth_getBlockByNumber" HttpFailure
  spend ctx src (covered 30000) `shouldReturn` refusedWith 503 "name_check_unavailable"
  clearFault node "eth_getBlockByNumber"
  low <- sourceOf ctx "low" 6000
  modifyIORef' cStub $ \s -> s {ssCreateResult = Left (ProviderError "down")}
  spend ctx low (difference 6000) `shouldReturn` refusedWith 503 "provider_unavailable"
  modifyIORef' cStub $ \s -> s {ssCreateResult = Right sampleProviderInvoice}
  fst <$> placeOrder ctx aliceOrder `shouldReturn` 200
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oCommitment = commitmentOf 3}} `shouldReturn` refusedWith 409 "order_exists"
  mapM (amountOf ctx) [src, low] `shouldReturn` [Just 30000, Just 6000]
  mapM (paidFrom ctx) [src, low] `shouldReturn` [[], []]

-- | Each request fails two checks, and is answered by the earlier.
testCheckOrder :: IO ()
testCheckOrder = withSpend $ \ctx@Ctx {cNode = node, cStub} -> do
  src <- sourceOf ctx "src" 30000
  low <- sourceOf ctx "low" 6000
  let unknown = RegistrationId "unknown"
  unsealed <- KM.delete "sealed" <$> spendBody ctx unknown (covered 30000)
  postSpend ctx unknown unsealed `shouldReturn` refusedWith 400 "bad_request"
  spend ctx unknown (covered 30000) {sbOrder = aliceOrder {oKey = keyB}} `shouldReturn` refusedWith 404 "not_found"
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (1 :: Int)
  spend ctx src (covered 31000) {sbOrder = aliceOrder {oTimestamp = seconds orderTime + 601}} `shouldReturn` refusedWith 400 "bad_request"
  stale <- balanceOf ctx src
  spend ctx src (covered 31000) `shouldReturn` balanceChanged stale
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oPrice = 9000}} `shouldReturn` refusedWith 409 "name_taken"
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
  modifyIORef' cStub $ \s -> s {ssCreateResult = Left (ProviderError "down")}
  spend ctx low (difference 6000) {sbOrder = aliceOrder {oPrice = 9000}} `shouldReturn` (409, Just $ KM.fromList [("error", "catalog_changed"), ("price", J.Number 10000)])
  modifyIORef' cStub $ \s -> s {ssCreateResult = Right sampleProviderInvoice}
  fst <$> spend ctx src (covered 30000) `shouldReturn` 200
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oKey = keyB}} `shouldReturn` refusedWith 400 "bad_request"
  spend ctx src (covered 31000) {sbOrder = aliceOrder {oCommitment = commitmentOf 3}} `shouldReturn` refusedWith 409 "order_exists"
  amountOf ctx src `shouldReturn` Just 20000

testSpendSizes :: IO ()
testSpendSizes = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 15000
  missing <- KM.delete "sealed" <$> spendBody ctx src (covered 15000)
  postSpend ctx src missing `shouldReturn` refusedWith 400 "bad_request"
  spend ctx src (covered 15000) {sbOrder = aliceOrder {oSealed = T.replicate 11096 "a"}} `shouldReturn` refusedWith 400 "bad_request"
  padded <- KM.insert "padding" (J.String $ T.replicate 16384 "a") <$> spendBody ctx src (covered 15000)
  postSpend ctx src padded `shouldReturn` refusedWith 400 "bad_request"
  longest <- spendBody ctx src (covered 15000) {sbOrder = aliceOrder {oSealed = T.replicate 11095 "a"}}
  LB.length (J.encode longest) `shouldSatisfy` (< 16384)
  fst <$> postSpend ctx src longest `shouldReturn` 200

testSpendLimit :: IO ()
testSpendLimit = withSpend $ \Ctx {cClient} -> do
  let attempt = statusOf <$> webRequestBody cClient "POST" "/api/registration/src/spend" [(hContentType, "application/json"), ("X-Forwarded-For", "203.0.113.9")] "{}"
  replicateM_ 5 $ attempt `shouldReturn` 400
  attempt `shouldReturn` 429

testOverdueHolderSpent :: IO ()
testOverdueHolderSpent = withSpend $ \ctx@Ctx {cStore, cClock} -> do
  src <- sourceOf ctx "src" 10000
  -- by card, whose invoice does not expire by itself at the provider, past its 60 minutes
  (200, held) <- spend ctx src (dynamis 10000 (Just "card"))
  let holderInv = invoiceIdOf held
  holder <- invoiceRegistration ctx holderInv
  holderRef <- providerRef ctx holderInv
  advance ctx (61 * 60)
  now <- readIORef cClock
  poll <- heldPoll ctx holder
  rid <- registrationAnswer . snd <$> spend ctx src (timedAt now $ covered 10000)
  (\(status, v) -> (status, field "version" v)) <$> woken poll `shouldReturn` (200, Just (J.toJSON $ nrVersion holder + 1))
  fmap irStatus <$> getInvoice cStore holderInv `shouldReturn` Just ISExpired
  (\r -> (nrLive r, nrVersion r)) <$> invoiceRegistration ctx holderInv `shouldReturn` (False, nrVersion holder + 1)
  cancels ctx `shouldReturn` [holderRef]
  (\r -> (nrPhase r, nrBalanceUsed r)) <$> registrationById ctx rid `shouldReturn` (PQueued, 10000)
  fmap (\b -> (blAmount b, blHold b)) <$> balanceOf ctx src `shouldReturn` Just (0, Nothing)

testHolderCancelledMeanwhile :: IO ()
testHolderCancelledMeanwhile = withSpendHook $ \hook ctx@Ctx {cStore, cWeb, cClock} -> do
  src <- sourceOf ctx "src" 6000
  (200, held) <- spend ctx src (difference 6000)
  let holderInv = invoiceIdOf held
  advance ctx (46 * 60)
  now <- readIORef cClock
  onCreate hook $ cancelOpenInvoice cStore (weWaiters cWeb) Nothing holderInv now `shouldReturn` True
  (status, v) <- spend ctx src (timedAt now $ dynamis 6000 (Just "btc"))
  status `shouldBe` 200
  next <- invoiceRegistration ctx (invoiceIdOf v)
  fmap irStatus <$> getInvoice cStore holderInv `shouldReturn` Just ISExpired
  fmap (fmap bhHolder . blHold) <$> balanceOf ctx src `shouldReturn` Just (Just $ nrId next)
  cancels ctx `shouldReturn` []

testHolderFundedMeanwhile :: IO ()
testHolderFundedMeanwhile = withSpendHook $ \hook ctx@Ctx {cStore, cClock} -> do
  src <- sourceOf ctx "src" 6000
  (200, held) <- spend ctx src (difference 6000)
  let holderInv = invoiceIdOf held
  holder <- invoiceRegistration ctx holderInv
  holderRef <- providerRef ctx holderInv
  advance ctx (46 * 60)
  now <- readIORef cClock
  onCreate hook $ settleNow ctx holderInv (SigFunded (rcv 4000 (Just "0.0006")) PaidInFull) `shouldReturn` Right ISOpen
  answer <- spend ctx src (timedAt now $ dynamis 6000 (Just "btc"))
  current <- balanceOf ctx src
  answer `shouldBe` balanceChanged current
  (fmap blAmount current, fmap bhHolder . blHold =<< current) `shouldBe` (Just 6000, Just $ nrId holder)
  calls <- stubCalls (cStub ctx)
  length [() | StubCreate {} <- calls] `shouldBe` 2
  refs <- cancels ctx
  length refs `shouldBe` 1
  refs `shouldNotBe` [holderRef]
  (fmap irStatus <$> getInvoice cStore holderInv) `shouldReturn` Just ISOpen
  nrLive <$> invoiceRegistration ctx holderInv `shouldReturn` True
  length <$> paidFrom ctx src `shouldReturn` 1

testConcurrentSpends :: IO ()
testConcurrentSpends = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 30000
  (a, b) <- concurrently (spend ctx src (covered 30000)) (spend ctx src (dynamis 30000 Nothing))
  sort [fst a, fst b] `shouldBe` [200, 409]
  [won] <- paidFrom ctx src
  current <- balanceOf ctx src
  fmap blAmount current `shouldBe` Just (30000 - nrPrice won)
  filter ((== 409) . fst) [a, b] `shouldBe` [balanceChanged current]

testWriteRefusedAtBalance :: IO ()
testWriteRefusedAtBalance = withSpendHook $ \hook ctx@Ctx {cOpenStore} -> do
  src <- sourceOf ctx "src" 6000
  other <- cOpenStore
  onCreate hook $ credit other "src" 500
  answer <- spend ctx src (difference 6000)
  current <- balanceOf ctx src
  (fmap blAmount current, blHold =<< current) `shouldBe` (Just 6500, Nothing)
  answer `shouldBe` balanceChanged current
  calls <- stubCalls (cStub ctx)
  [() | StubCreate {} <- calls] `shouldBe` [()]
  length <$> cancels ctx `shouldReturn` 1
  paidFrom ctx src `shouldReturn` []

testSpendRacesRefund :: IO ()
testSpendRacesRefund = withSpend $ \ctx@Ctx {cStore, cOrders, cClock} -> do
  src <- sourceOf ctx "src" 10000
  now <- readIORef cClock
  (spent, refunded) <- concurrently (spend ctx src (covered 10000)) (recordRefund (oeTurns cOrders) cStore "src" 10000 now)
  current <- balanceOf ctx src
  fmap blAmount current `shouldBe` Just 0
  case refunded of
    Right _ -> do
      spent `shouldBe` balanceChanged current
      paidFrom ctx src `shouldReturn` []
      fmap blRefunded current `shouldBe` Just 10000
    Left refusal -> do
      (fst spent, refusal) `shouldBe` (200, RefundAmount 0)
      length <$> paidFrom ctx src `shouldReturn` 1
      fmap blRefunded current `shouldBe` Just 0

testSpendTurns :: IO ()
testSpendTurns = withSpend $ \ctx@Ctx {cOrders} -> do
  src <- sourceOf ctx "src" 15000
  let turns = oeTurns cOrders
  inside <- newEmptyMVar
  proceed <- newEmptyMVar
  holding <- async $ withBalanceTurn turns src $ putMVar inside () >> takeMVar proceed
  takeMVar inside
  spent <- async $ spend ctx src (covered 15000)
  -- the spend waits for the balance's turn while it holds the name's
  waiting <- timeout pollSeconds $ atomically $ do
    balanceTurn <- balanceTurnCount turns src
    nameTurn <- labelTurnCount turns (labelHash "alicebakery")
    check (balanceTurn == 2 && nameTurn == 1)
  waiting `shouldBe` Just ()
  putMVar proceed ()
  wait holding
  fst <$> wait spent `shouldReturn` 200
