{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesSpendTests (namesSpendChatTests, namesSpendTests) where

import Bots.StoreService.BotTests (operatorCmd, withStoreService)
import Bots.StoreService.FakeBTCPay (FakeBTCPay (..), setInvoiceState, setRates, webhookEvent, webhookSigHeader, withFakeBTCPay)
import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesBalancesTests (credit)
import Bots.StoreService.NamesContractsTests (rentPriceAnswer, rentPriceCall, reservedNamesCall)
import Bots.StoreService.NamesOrdersTests (Ctx (..), Order (..), advance, aliceOrder, bakers, codesOnly, commitmentOf, communityTerms, configured, fieldsOf, invoiceRegistration, keyA, keyB, listName, listOnChain, olderTerms, orderTime, ownerAText, placeOrder, placed, post, registrationById, registrationOf, registrationsOf, seconds, settleNow, termsChanged, threeYears, withOrdersOn, yearsDuration)
import Bots.StoreService.NamesOwnerTests (findBody, postFind)
import Bots.StoreService.NamesPhasesTests (registrationPath, seenVersion, woken)
import qualified Bots.StoreService.NamesPhasesTests as Phases
import Bots.StoreService.NamesStoreTests (at, order, ownerA, placePaid, reg, write)
import Bots.StoreService.WebTests (StubCall (..), answerOf, bounded, capturingStderr, StubState (..), fieldOf, jsonObject, rcv, refusedWith, sampleProviderInvoice, statusOf, stubCalls, webGet, webRequestBody, withServiceStores, withApp)
import ChatTests.DBUtils (TestParams)
import Control.Concurrent.Async (async, concurrently, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM_, join, replicateM_, void, when)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Types (hContentType)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Simplex.Chat.Badges.Code (nameLengthInt)
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (signRecoverable)
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import StoreService.Config (BTCPayConfig (..), ServiceConfig (..))
import StoreService.Names.Balances (RefundRefusal (..), recordRefund)
import StoreService.Names.Chain (hex, uint256)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Registration (OrderFields (..), RegistrationId (..), personalDigest, spendMessage)
import StoreService.Names.Store
import StoreService.Names.View (addressText, balanceJSON)
import StoreService.Orders (cancelOpenInvoice)
import StoreService.Poller (PollerEnv, creditPass, drainHints, forgetRates, newPollerEnv)
import StoreService.Providers (Funded (..), OrderDraft (..), PaymentSignal (..), Provider (..), ProviderError (..))
import StoreService.Providers.BTCPay (btcpayProvider)
import StoreService.Names.Turns (balanceTurnCount, labelTurnCount, withBalanceTurn)
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import qualified StoreService.Web.Http as Http (findLimit)
import StoreService.Web.Http (Limit (..), checkoutClient, checkoutLimit, createLimit, holdMicros, limitedCheckout, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Orders (spendHandler)
import StoreService.Web.Names.Owner (findHandler)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import Test.Hspec

namesSpendTests :: Spec
namesSpendTests = describe "StoreService names: spend" $ do
  describe "orders" $ do
    it "queues an order whose price the balance covers, spending the price and keeping the rest, or all of it" testCoveredSpend
    it "makes an invoice for the difference, the order holding all of the balance until the invoice's expiry" testDifferenceSpend
    it "refuses a difference from a client network whose checkouts are used up, reaching no provider" testDifferenceLimitedPerNetwork
    it "answers an exact resend with its order in any phase, by its own hash, with a new timestamp and after the balance changed" testSpendResend
    it "keeps a spend and an /api/invoice order of the same fields apart, each meeting the other as order_exists" testInvoiceBodyApart
  describe "refusals" $ do
    it "answers balance_changed with the current balance for a stale amount, a held balance and an order with none" testBalanceChanged
    it "refuses a method with a covered price, none with a difference, and prices and balances not positive" testMethodRule
    it "answers name_taken, catalog_changed, name_check_unavailable, also for a price under the floor, provider_unavailable and order_exists, spending nothing" testOrderChecks
    it "runs its checks in the contract's order" testCheckOrder
    it "refuses a card difference below the card minimum after the price check, accepting it by crypto and at the minimum" testCardMinimum
    it "refuses a malformed order and a body over 16384 bytes, accepting the longest" testSpendSizes
    it "answers 403 codes_only to a covered spend, a difference, a malformed body and an unknown order, spending nothing and reaching no provider" testSpendCodesOnly
  describe "terms" $ do
    it "answers terms_changed for other terms or the other kind, listed or not, and stores the community terms, covered and by difference" testSpendTermsChanged
    it "checks a spend for a community name for 2 years at their price, whatever years the configuration gives, storing those years" testSpendCommunityYears
    it "answers an exact resend after the terms changed on chain, covered and by difference" testSpendResendAfterBump
    it "checks the terms after the timestamp and the duration, with the community listing's read, before the balance and the name check, answering terms_unavailable while they are not set" testSpendTermsCheckOrder
  describe "holders" $ do
    it "closes the balance's overdue unfunded holder in the spend's transaction, publishing it and cancelling its invoice" testOverdueHolderSpent
    it "closes an overdue unfunded holder of another name, or of the same name with a new commitment, for a difference spend, the new invoice holding the balance" testDifferenceClosesHolder
    it "goes through when the overdue holder was cancelled meanwhile" testHolderCancelledMeanwhile
    it "answers balance_changed when the overdue holder was funded meanwhile, cancelling the new invoice" testHolderFundedMeanwhile
  describe "races" $ do
    it "lets one of two concurrent spends of one balance win, the other answered balance_changed" testConcurrentSpends
    it "answers balance_changed and cancels the provider invoice when another store changed the balance after the check" testWriteRefusedAtBalance
    it "lets one of a spend and a refund of the balance win" testSpendRacesRefund
    it "takes the new name's turn and then the balance's" testSpendTurns

namesSpendChatTests :: SpecWith TestParams
namesSpendChatTests =
  describe "StoreService names: spend end to end" $
    it "spends a taken order's balance on a dearer name through BTCPay and the poller, credits a late payment, and refunds the rest" testSpendEndToEnd

testSpendCodesOnly :: IO ()
testSpendCodesOnly = withSpend $ \open -> do
  source <- sourceOf open "s" 10000
  serveSpend (configured codesOnly open) $ \ctx -> do
    spend ctx source (covered 10000) `shouldReturn` refusedWith 403 "codes_only"
    spend ctx source (dynamis 10000 $ Just "btc") `shouldReturn` refusedWith 403 "codes_only"
    postSpend ctx source KM.empty `shouldReturn` refusedWith 403 "codes_only"
    postSpend ctx (RegistrationId "unknown") KM.empty `shouldReturn` refusedWith 403 "codes_only"
    amountOf ctx source `shouldReturn` Just 10000
    paidFrom ctx source `shouldReturn` []
    stubCalls (cStub ctx) `shouldReturn` []
  -- nothing the refusals left meets the same spend with codes_only off
  fst <$> spend open source (covered 10000) `shouldReturn` 200

-- fixtures

-- | Over a new database; the hook runs as each provider invoice is created.
withSpendHook :: (IORef (IO ()) -> Ctx -> IO a) -> IO a
withSpendHook action = do
  hook <- newIORef (pure ())
  withServiceStores $ \open -> withSpendOn open (\p -> p {pCreateInvoice = \m d -> join (readIORef hook) >> pCreateInvoice p m d}) (action hook)

withSpendOn :: IO DBStore -> (Provider -> Provider) -> (Ctx -> IO a) -> IO a
withSpendOn open via action = withOrdersOn open via (`serveSpend` action)

-- | The spend route beside find, the registration poll and the service's routes, over the context's settings.
serveSpend :: Ctx -> (Ctx -> IO a) -> IO a
serveSpend ctx@Ctx {cOrders, cWeb} action = do
  let routes :: [Text] -> Maybe Route
      routes = \case
        ["api", "registration", "find"] -> Just Route {rMethod = "POST", rLimit = Http.findLimit, rHandler = findHandler cOrders}
        ["api", "registration", rid, "spend"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = spendHandler cOrders (weHttp cWeb) (weConfig cWeb) (weProviders cWeb) (weWaiters cWeb) (RegistrationId rid)}
        ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler cOrders (weHttp cWeb) holdMicros (RegistrationId rid)}
        _ -> Nothing
      app :: Application
      app req respond = fromMaybe (webApp cWeb req respond) (namesApp (weHttp cWeb) routes req respond)
  withApp app $ \client -> action ctx {cClient = client}

withSpend :: (Ctx -> IO a) -> IO a
withSpend action = withSpendHook (const action)

-- | Run by the next provider invoice made, once.
onCreate :: IORef (IO ()) -> IO () -> IO ()
onCreate hook act = writeIORef hook (writeIORef hook (pure ()) >> act)

-- | A taken paid order of owner A with a balance of the cents.
sourceOf :: Ctx -> Text -> USDCents -> IO RegistrationId
sourceOf Ctx {cStore} rid cents = do
  placePaid cStore rid ownerA (order ("source-" <> rid) 1) {ofCommitment = C.sha256Hash $ encodeUtf8 rid} (at 60) `shouldReturn` Right ()
  taken <- reg cStore rid
  write cStore taken {nrPhase = OTaken Nothing Nothing} `shouldReturn` Written 2
  credit cStore rid cents
  pure $ RegistrationId rid

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
timedAt now b@SpendBody {sbOrder} = b {sbOrder = orderAt now sbOrder}

orderAt :: UTCTime -> Order -> Order
orderAt now o = o {oTimestamp = seconds now, oDuration = yearsDuration 2 now}

spendBody :: Ctx -> RegistrationId -> SpendBody -> IO J.Object
spendBody Ctx {cRandom} source SpendBody {sbOrder = o@Order {oKey, oPrice, oTimestamp}, sbBalance, sbMethod} = do
  sig <- either (error . show) id <$> signRecoverable cRandom oKey (personalDigest $ spendMessage source fields oTimestamp)
  pure $
    KM.fromList $
      [ ("labelHash", J.String $ hex lh),
        ("length", J.toJSON (nameLengthInt ofLength)),
        ("duration", J.toJSON ofDuration),
        ("commitment", J.String $ hex ofCommitment),
        ("sealed", J.String ofSealed),
        ("price", J.toJSON oPrice),
        ("balance", J.toJSON sbBalance),
        ("terms", J.String $ textEncode ofAgreedTerms),
        ("timestamp", J.toJSON oTimestamp),
        ("signature", J.String $ hex $ smpEncode sig)
      ]
        <> maybe [] (\m -> [("method", J.String m)]) sbMethod
  where
    fields@OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} = fieldsOf o

postSpend :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
postSpend ctx (RegistrationId rid) = post ctx ("/api/registration/" <> T.unpack rid <> "/spend") . J.encode

spend :: Ctx -> RegistrationId -> SpendBody -> IO (Int, Maybe J.Object)
spend ctx source b = spendBody ctx source b >>= postSpend ctx source

registrationAnswer :: Maybe J.Object -> RegistrationId
registrationAnswer = \case
  Just v | KM.keys v == ["registrationId"], Just (J.String rid) <- fieldOf v "registrationId" -> RegistrationId rid
  other -> error $ "not a registration id: " <> show other

invoiceIdOf :: Maybe J.Object -> InvoiceId
invoiceIdOf v = case (`fieldOf` "invoiceId") =<< v of
  Just (J.String t) -> InvoiceId t
  other -> error $ "not an invoice view: " <> show other

viewOf :: Ctx -> RegistrationId -> IO J.Object
viewOf Ctx {cClient} rid = webGet cClient (registrationPath rid) >>= jsonObject

balanceOf :: Ctx -> RegistrationId -> IO (Maybe Balance)
balanceOf Ctx {cStore, cClock} rid = readIORef cClock >>= \now -> withTransaction cStore $ \db -> getBalance db rid now

amountOf :: Ctx -> RegistrationId -> IO (Maybe USDCents)
amountOf ctx rid = fmap blAmount <$> balanceOf ctx rid

balanceChanged :: Maybe Balance -> (Int, Maybe J.Object)
balanceChanged b = (409, Just $ KM.fromList [("error", "balance_changed"), ("balance", maybe J.Null balanceJSON b)])

-- | The orders a balance paid.
paidFrom :: Ctx -> RegistrationId -> IO [NameRegistration]
paidFrom ctx source = filter ((== Just source) . fmap buSource . paymentBalance . nrPayment) <$> registrationsOf ctx ownerA

cancels :: Ctx -> IO [Text]
cancels Ctx {cStub} = (\calls -> [r | StubCancel r <- calls]) <$> stubCalls cStub

providerRef :: Ctx -> InvoiceId -> IO Text
providerRef Ctx {cStore} invId = maybe (error "no invoice row") irProviderRef <$> getInvoice cStore invId

btc :: ServicePaymentMethod
btc = SPMCrypto CCBtc

-- tests

testCoveredSpend :: IO ()
testCoveredSpend = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 15000
  rid <- registrationAnswer . snd <$> spend ctx src (covered 15000)
  r <- registrationById ctx rid
  (nrPhase r, nrPrice r, nrPayment r) `shouldBe` (OQueued Nothing, 10000, OPBalance BalanceUse {buSource = src, buUsed = 10000})
  nrOrderHash r `shouldBe` orderHash ownerA (fieldsOf aliceOrder) (OTBalance Spend {spSource = src, spPrice = 10000, spBalance = 15000, spMethod = Nothing})
  nrAgreedTerms r `shouldBe` agreed (oTerms aliceOrder)
  view <- viewOf ctx rid
  (fieldOf view "phase", fieldOf view "price", fieldOf view "fromBalance", fieldOf view "balance") `shouldBe` (Just "queued", Just (J.Number 10000), Just (J.Number 10000), Just J.Null)
  amountOf ctx src `shouldReturn` Just 5000
  exact <- sourceOf ctx "exact" 20000
  rid' <- registrationAnswer . snd <$> spend ctx exact (dynamis 20000 Nothing)
  nrPayment <$> registrationById ctx rid' `shouldReturn` OPBalance BalanceUse {buSource = exact, buUsed = 20000}
  fmap (\b -> (blAmount b, blHold b)) <$> balanceOf ctx exact `shouldReturn` Just (0, Nothing)
  stubCalls (cStub ctx) `shouldReturn` []

testDifferenceSpend :: IO ()
testDifferenceSpend = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 6000
  (status, v) <- spend ctx src (difference 6000)
  status `shouldBe` 200
  let expiry = addUTCTime (45 * 60) orderTime
      r = registrationOf <$> v
  ((`fieldOf` "status") =<< v, (`fieldOf` "amount") =<< v, (`fieldOf` "expiresAt") =<< v) `shouldBe` (Just "open", Just (J.Number 4000), Just (J.toJSON expiry))
  ((`fieldOf` "phase") =<< r, (`fieldOf` "price") =<< r, (`fieldOf` "fromBalance") =<< r) `shouldBe` (Just "unpaid", Just (J.Number 10000), Just (J.Number 6000))
  let invId = invoiceIdOf v
  holder <- invoiceRegistration ctx invId
  stubCalls (cStub ctx) `shouldReturn` [StubCreate btc OrderDraft {odAmount = CurrencyAmount 4000, odCurrency = "usd", odReference = "name:" <> hex (nrOrderHash holder)}]
  (nrPhase holder, nrPrice holder, nrPayment holder) `shouldBe` (OUnpaid True, 10000, OPDifference BalanceUse {buSource = src, buUsed = 6000} btc invId)
  nrOrderHash holder `shouldBe` orderHash ownerA (fieldsOf aliceOrder) (OTBalance Spend {spSource = src, spPrice = 10000, spBalance = 6000, spMethod = Just btc})
  nrAgreedTerms holder `shouldBe` agreed (oTerms aliceOrder)
  fmap irAmount <$> getInvoice (cStore ctx) invId `shouldReturn` Just (CurrencyAmount 4000)
  (`fieldOf` "balance") <$> viewOf ctx src `shouldReturn` Just (J.object ["amount" J..= (6000 :: Int), "held" J..= True, "heldUntil" J..= expiry, "refunded" J..= (0 :: Int)])

testDifferenceLimitedPerNetwork :: IO ()
testDifferenceLimitedPerNetwork = withSpend $ \ctx@Ctx {cClient, cWeb, cStub} -> do
  src <- sourceOf ctx "src" 6000
  let http = weHttp cWeb
      fromNetwork forwardedFor = checkoutClient http Wai.defaultRequest {Wai.requestHeaders = [("X-Forwarded-For", forwardedFor)]}
  replicateM_ (lmPerMinute checkoutLimit) $ limitedCheckout http (fromNetwork "203.0.113.7") (SPMCrypto CCBtc) (const $ pure ()) (pure ())
  signed <- spendBody ctx src (difference 6000)
  r <- webRequestBody cClient "POST" "/api/registration/src/spend" [(hContentType, "application/json"), ("X-Forwarded-For", "203.0.113.9")] (J.encode signed)
  answerOf r `shouldBe` refusedWith 429 "rate_limited"
  stubCalls cStub `shouldReturn` []
  fst <$> spend ctx src (difference 6000) `shouldReturn` 200

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
  Written _ <- withTransaction cStore $ \db -> updateRegistration db holder {nrPhase = OQueued Nothing} now
  (\(status, v) -> (status, invoiceIdOf v, (`fieldOf` "phase") . registrationOf =<< v)) <$> postSpend ctx src body `shouldReturn` (200, invId, Just "queued")
  other <- sourceOf ctx "other" 25000
  let paid = timedAt now $ dynamis 25000 Nothing
  coveredBody <- spendBody ctx other paid
  (200, answer) <- postSpend ctx other coveredBody
  r <- registrationById ctx $ registrationAnswer answer
  spend ctx other paid {sbOrder = (sbOrder paid) {oTimestamp = seconds now + 1}} `shouldReturn` (200, answer)
  Written _ <- withTransaction cStore $ \db -> updateRegistration db r {nrPhase = OCommitted Commit {cmTx = Nothing, cmRevealAfter = seconds now, cmRevealBefore = seconds now + 86400, cmLinksRefusedAt = Nothing}} now
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
  bhState <$> (blHold =<< holding) `shouldBe` Just (HSOpen (addUTCTime (45 * 60) orderTime))
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

testOrderChecks :: IO ()
testOrderChecks = withSpend $ \ctx@Ctx {cNode = node, cStub} -> do
  src <- sourceOf ctx "src" 30000
  setFault node "eth_getBlockByNumber" HttpFailure
  spend ctx src (covered 30000) `shouldReturn` refusedWith 503 "name_check_unavailable"
  clearFault node "eth_getBlockByNumber"
  answerCallOnce node fakeController (rentPriceCall 11) $ Returns $ rentPriceAnswer 9999
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oPrice = 9999}} `shouldReturn` refusedWith 503 "name_check_unavailable"
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
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (1 :: Int)
  spend ctx src (covered 31000) {sbOrder = aliceOrder {oTimestamp = seconds orderTime + 601}} `shouldReturn` refusedWith 400 "bad_request"
  stale <- balanceOf ctx src
  spend ctx src (covered 31000) `shouldReturn` balanceChanged stale
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oPrice = 9000}} `shouldReturn` refusedWith 409 "name_taken"
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (0 :: Int)
  modifyIORef' cStub $ \s -> s {ssCreateResult = Left (ProviderError "down")}
  spend ctx low (difference 6000) {sbOrder = aliceOrder {oPrice = 9000}} `shouldReturn` (409, Just $ KM.fromList [("error", "catalog_changed"), ("price", J.Number 10000)])
  modifyIORef' cStub $ \s -> s {ssCreateResult = Right sampleProviderInvoice}
  fst <$> spend ctx src (covered 30000) `shouldReturn` 200
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oKey = keyB}} `shouldReturn` refusedWith 400 "bad_request"
  spend ctx src (covered 31000) {sbOrder = aliceOrder {oCommitment = commitmentOf 3}} `shouldReturn` refusedWith 409 "order_exists"
  amountOf ctx src `shouldReturn` Just 20000

testSpendTermsChanged :: IO ()
testSpendTermsChanged = withSpend $ \ctx0 -> serveSpend (configured (listName "dynamis") ctx0) $ \ctx -> do
  listOnChain ctx "dynamis"
  src <- sourceOf ctx "src" 30000
  spend ctx src (covered 30000) {sbOrder = aliceOrder {oTerms = olderTerms}} `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  spend ctx src (dynamis 30000 Nothing) {sbOrder = dynamisOrder {oTerms = plainTerms}} `shouldReturn` termsChanged fakeCommunityTermsText (Just bakers)
  amountOf ctx src `shouldReturn` Just 30000
  rid <- registrationAnswer . snd <$> spend ctx src (dynamis 30000 Nothing) {sbOrder = dynamisOrder {oTerms = communityTerms}}
  nrAgreedTerms <$> registrationById ctx rid `shouldReturn` agreed communityTerms
  serveSpend (configured (listName "alicebakery") ctx) $ \listing -> do
    listOnChain listing "alicebakery"
    low <- sourceOf listing "low" 4000
    (status, v) <- spend listing low (difference 4000) {sbOrder = aliceOrder {oTerms = communityTerms}}
    status `shouldBe` 200
    nrAgreedTerms <$> invoiceRegistration listing (invoiceIdOf v) `shouldReturn` agreed communityTerms

testSpendCommunityYears :: IO ()
testSpendCommunityYears = withSpend $ \ctx0 -> serveSpend (configured (threeYears . listName "dynamis") ctx0) $ \ctx -> do
  listOnChain ctx "dynamis"
  src <- sourceOf ctx "src" 30000
  let community = dynamisOrder {oTerms = communityTerms}
  spend ctx src (dynamis 30000 Nothing) {sbOrder = community {oDuration = yearsDuration 3 orderTime}} `shouldReturn` refusedWith 400 "bad_request"
  rid <- registrationAnswer . snd <$> spend ctx src (dynamis 30000 Nothing) {sbOrder = community}
  nrYears <$> registrationById ctx rid `shouldReturn` 3

testSpendResendAfterBump :: IO ()
testSpendResendAfterBump = withSpend $ \ctx0 -> do
  src <- sourceOf ctx0 "src" 15000
  low <- sourceOf ctx0 "low" 5000
  coveredBody <- spendBody ctx0 src (covered 15000)
  (200, Just coveredAnswer) <- postSpend ctx0 src coveredBody
  differenceBody <- spendBody ctx0 low (dynamis 5000 (Just "btc"))
  (200, Just differenceView) <- postSpend ctx0 low differenceBody
  setGeneralTerms (cNode ctx0) fakeNewGeneralTermsText
  advance ctx0 60
  postSpend ctx0 src coveredBody `shouldReturn` (200, Just coveredAnswer)
  postSpend ctx0 low differenceBody `shouldReturn` (200, Just differenceView)
  mapM (fmap length . paidFrom ctx0) [src, low] `shouldReturn` [1, 1]

testSpendTermsCheckOrder :: IO ()
testSpendTermsCheckOrder = withSpend $ \ctx@Ctx {cNode = node} -> do
  src <- sourceOf ctx "src" 15000
  let old = aliceOrder {oTerms = olderTerms}
  spend ctx src (covered 15000) {sbOrder = old {oTimestamp = seconds orderTime + 601}} `shouldReturn` refusedWith 400 "bad_request"
  spend ctx src (covered 15000) {sbOrder = old {oDuration = oDuration old + 3601}} `shouldReturn` refusedWith 400 "bad_request"
  spend ctx src (covered 14000) {sbOrder = old} `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (1 :: Int)
  spend ctx src (covered 15000) {sbOrder = old {oPrice = 9000}} `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  setGeneralTerms node ""
  spend ctx src (covered 14000) {sbOrder = old {oPrice = 9000}} `shouldReturn` refusedWith 503 "terms_unavailable"
  setGeneralTerms node fakeGeneralTermsText
  setFault node "eth_call" HttpFailure
  spend ctx src (covered 15000) {sbOrder = old} `shouldReturn` refusedWith 503 "name_check_unavailable"
  amountOf ctx src `shouldReturn` Just 15000
  paidFrom ctx src `shouldReturn` []

testCardMinimum :: IO ()
testCardMinimum = withSpend $ \ctx -> do
  small <- sourceOf ctx "small" 9951
  spend ctx small (difference 9951) {sbOrder = aliceOrder {oPrice = 9990}, sbMethod = Just "card"} `shouldReturn` (409, Just $ KM.fromList [("error", "catalog_changed"), ("price", J.Number 10000)])
  spend ctx small (difference 9951) {sbMethod = Just "card"} `shouldReturn` refusedWith 400 "bad_request"
  stubCalls (cStub ctx) `shouldReturn` []
  amountOf ctx small `shouldReturn` Just 9951
  fst <$> spend ctx small (difference 9951) `shouldReturn` 200
  least <- sourceOf ctx "least" 19950
  fst <$> spend ctx least (dynamis 19950 (Just "card")) `shouldReturn` 200
  calls <- stubCalls (cStub ctx)
  [odAmount d | StubCreate _ d <- calls] `shouldBe` [CurrencyAmount 49, CurrencyAmount 50]

testSpendSizes :: IO ()
testSpendSizes = withSpend $ \ctx -> do
  src <- sourceOf ctx "src" 15000
  missing <- KM.delete "sealed" <$> spendBody ctx src (covered 15000)
  postSpend ctx src missing `shouldReturn` refusedWith 400 "bad_request"
  padded <- KM.insert "padding" (J.String $ T.replicate 16384 "a") <$> spendBody ctx src (covered 15000)
  postSpend ctx src padded `shouldReturn` refusedWith 400 "bad_request"
  longest <- spendBody ctx src (covered 15000) {sbOrder = aliceOrder {oSealed = T.replicate 11095 "a"}}
  LB.length (J.encode longest) `shouldSatisfy` (< 16384)
  fst <$> postSpend ctx src longest `shouldReturn` 200

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
  poll <- Phases.held ctx (0, 1) $ registrationPath (nrId holder) <> seenVersion (nrVersion holder)
  rid <- registrationAnswer . snd <$> spend ctx src (timedAt now $ covered 10000)
  (\(status, v) -> (status, fieldOf v "version")) <$> woken "the registration poll" poll `shouldReturn` (200, Just (J.toJSON $ nrVersion holder + 1))
  fmap irStatus <$> getInvoice cStore holderInv `shouldReturn` Just ISExpired
  (\r -> (orderLive r, nrVersion r)) <$> invoiceRegistration ctx holderInv `shouldReturn` (False, nrVersion holder + 1)
  cancels ctx `shouldReturn` [holderRef]
  (\r -> (nrPhase r, buUsed <$> paymentBalance (nrPayment r))) <$> registrationById ctx rid `shouldReturn` (OQueued Nothing, Just 10000)
  fmap (\b -> (blAmount b, blHold b)) <$> balanceOf ctx src `shouldReturn` Just (0, Nothing)

-- | By card, whose invoice does not expire by itself at the provider, past its 60 minutes.
overdueCardHolder :: Ctx -> RegistrationId -> IO (NameRegistration, Text)
overdueCardHolder ctx src = do
  (200, held) <- spend ctx src (difference 6000) {sbMethod = Just "card"}
  let holderInv = invoiceIdOf held
  holder <- invoiceRegistration ctx holderInv
  holderRef <- providerRef ctx holderInv
  advance ctx (61 * 60)
  pure (holder, holderRef)

-- | Whether the holder is live, its version, and its invoice's status.
holderAfter :: Ctx -> NameRegistration -> IO (Bool, Int64, Maybe InvoiceStatus)
holderAfter ctx@Ctx {cStore} NameRegistration {nrPayment} = case paymentInvoice nrPayment of
  Just invId -> do
    r <- invoiceRegistration ctx invId
    status <- fmap irStatus <$> getInvoice cStore invId
    pure (orderLive r, nrVersion r, status)
  Nothing -> error $ "not a paid order: " <> show nrPayment

testDifferenceClosesHolder :: IO ()
testDifferenceClosesHolder =
  -- the second row is the page's "Get a new invoice": the same name with a new commitment
  forM_ [(dynamis 6000 (Just "btc"), False), (SpendBody {sbOrder = aliceOrder {oCommitment = commitmentOf 5}, sbBalance = 6000, sbMethod = Just "card"}, True)] $ \(body, sameName) ->
    withSpend $ \ctx@Ctx {cClock} -> do
      src <- sourceOf ctx "src" 6000
      (holder, holderRef) <- overdueCardHolder ctx src
      now <- readIORef cClock
      (status, v) <- spend ctx src (timedAt now body)
      status `shouldBe` 200
      next <- invoiceRegistration ctx (invoiceIdOf v)
      orderLive next `shouldBe` True
      when sameName $ nrLabelHash next `shouldBe` nrLabelHash holder
      holderAfter ctx holder `shouldReturn` (False, nrVersion holder + 1, Just ISExpired)
      cancels ctx `shouldReturn` [holderRef]
      fmap (fmap bhHolder . blHold) <$> balanceOf ctx src `shouldReturn` Just (Just $ nrId next)

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
  onCreate hook $ settleNow ctx holderInv (SigFunded (rcv 4000 (Just "0.0006")) PaidInFull) `shouldReturn` ISOpen
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
  orderLive <$> invoiceRegistration ctx holderInv `shouldReturn` True
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
  answered <- newIORef Nothing
  logs <- capturingStderr $ spend ctx src (difference 6000) >>= writeIORef answered . Just
  [cancelled] <- cancels ctx
  logs `shouldSatisfy` T.isInfixOf ("the order was refused, so invoice " <> cancelled <> " is cancelled at the provider")
  Just answer <- readIORef answered
  current <- balanceOf ctx src
  (fmap blAmount current, blHold =<< current) `shouldBe` (Just 6500, Nothing)
  answer `shouldBe` balanceChanged current
  calls <- stubCalls (cStub ctx)
  [() | StubCreate {} <- calls] `shouldBe` [()]
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
  bounded "the spend's turns" $ atomically $ do
    balanceTurn <- balanceTurnCount turns src
    nameTurn <- labelTurnCount turns (labelHash "alicebakery")
    check (balanceTurn == 2 && nameTurn == 1)
  putMVar proceed ()
  wait holding
  fst <$> wait spent `shouldReturn` 200

-- end to end

-- | The real BTCPay adapter against the fake, read by the poller when BTCPay's webhook asks, over the running service's store, so
-- //balance reads what the routes wrote; the clock is the wall clock, which the poller and //balance read.
withBTCPaySpend :: TestParams -> (ChatController -> FakeBTCPay -> PollerEnv -> Ctx -> IO ()) -> IO ()
withBTCPaySpend ps action = withStoreService ps $ \_ _ cc -> withFakeBTCPay $ \fake -> do
  btcpay <- btcpayProvider (fbConfig fake)
  let viaFake :: Provider -> Provider
      viaFake p = case pProvider p of
        PPCrypto -> btcpay
        _ -> p
  withSpendOn (pure $ chatStore cc) viaFake $ \ctx@Ctx {cStore, cOrders, cWeb, cClock} -> do
    getCurrentTime >>= writeIORef cClock
    poller <- newPollerEnv cStore (weWaiters cWeb) (Just $ oePhases cOrders) (weHints cWeb) (weProviders cWeb) (poll $ weConfig cWeb)
    action cc fake poller ctx

-- | BTCPay shows the invoice in the state and sends the event, and the poller reads the invoice the event names.
reported :: FakeBTCPay -> PollerEnv -> Ctx -> Text -> Text -> [JT.Pair] -> IO ()
reported fake poller Ctx {cClient} event ref state = do
  setInvoiceState fake ref state
  let body = webhookEvent event ref
  statusOf <$> webRequestBody cClient "POST" "/webhooks/btcpay" (webhookSigHeader (bWebhookSecret $ fbConfig fake) body) body `shouldReturn` 200
  drainHints poller

-- | Paid in full and settled: $100.00 at the fixtures' rate of 108000.12 dollars a bitcoin, as the invoice asked.
settledState :: [JT.Pair]
settledState = ["status" J..= ("Settled" :: Text), "amount" J..= ("0.00092593" :: Text), "paymentMethodPaid" J..= ("0.00092593" :: Text)]

-- | 0.0005 bitcoin confirmed after the $100.00 invoice of 0.00092593 expired, credited as its share of the price, 5399 cents.
latePaid :: [JT.Pair]
latePaid =
  [ "additionalStatus" J..= ("PaidPartial" :: Text),
    "amount" J..= ("0.00092593" :: Text),
    "paymentMethodPaid" J..= lateBtc,
    "payments" J..= [J.object ["id" J..= ("late-tx" :: Text), "receivedDate" J..= (1700003600 :: Int), "value" J..= lateBtc, "fee" J..= ("0.00000000" :: Text), "status" J..= ("Settled" :: Text)]]
  ]
  where
    lateBtc = "0.00050000" :: Text

ratesDown, ratesUp :: J.Value
ratesDown = J.toJSON [J.object ["currencyPair" J..= ("BTC_USD" :: Text), "errors" J..= ["no rate source answered" :: Text], "rate" J..= J.Null]]
ratesUp = J.toJSON [J.object ["currencyPair" J..= ("BTC_USD" :: Text), "errors" J..= ([] :: [Text]), "rate" J..= ("108000.12" :: Text)]]

-- | The worker's write when another wallet holds the name: taken, with the order's whole price credited in the same transaction.
takenNow :: Ctx -> RegistrationId -> IO ()
takenNow ctx@Ctx {cStore, cClock} rid = do
  r <- registrationById ctx rid
  now <- readIORef cClock
  withTransaction cStore (\db -> updateRegistration db r {nrPhase = OTaken Nothing Nothing} now >>= \w -> w <$ creditBalance db rid (nrPrice r) now)
    `shouldReturn` Written (nrVersion r + 1)

-- | Owner A's orders as find lists them: each one's id, the order whose balance paid it, and the order holding its balance.
links :: Ctx -> IO [(Text, Maybe Text, Maybe Text)]
links ctx@Ctx {cClock} = do
  now <- readIORef cClock
  findBody ctx keyA ownerAText (seconds now) >>= postFind ctx >>= \case
    (200, Just v) | Just found <- JT.parseMaybe (\o -> o J..: "registrations" >>= mapM entry) v -> pure (sort found)
    other -> error $ "find refused: " <> show other
  where
    entry :: J.Value -> JT.Parser (Text, Maybe Text, Maybe Text)
    entry = J.withObject "registration" $ \r -> (,,) <$> r J..: "registrationId" <*> r J..: "balanceSource" <*> r J..: "heldBy"

balanceView :: Ctx -> RegistrationId -> IO (Maybe J.Value)
balanceView ctx rid = (`fieldOf` "balance") <$> viewOf ctx rid

-- | A balance nothing holds.
balanceObject :: Int -> Int -> Maybe J.Value
balanceObject amount refunded = Just $ J.object ["amount" J..= amount, "held" J..= False, "heldUntil" J..= J.Null, "refunded" J..= refunded]

ridText :: RegistrationId -> Text
ridText (RegistrationId t) = t

testSpendEndToEnd :: TestParams -> IO ()
testSpendEndToEnd ps = withBTCPaySpend ps $ \cc fake poller ctx@Ctx {cClock} -> do
  now <- readIORef cClock
  let report :: Text -> InvoiceId -> [JT.Pair] -> IO ()
      report event inv state = providerRef ctx inv >>= \ref -> reported fake poller ctx event ref state
      orderOf :: InvoiceId -> IO RegistrationId
      orderOf inv = nrId <$> invoiceRegistration ctx inv
      dynamisAgain :: Int -> SpendBody
      dynamisAgain n = timedAt now $ (dynamis 10000 (Just "btc")) {sbOrder = dynamisOrder {oCommitment = commitmentOf n}}
  -- alicebakery paid at BTCPay and queued by the poller, then taken by another wallet, its price kept as its balance
  (srcInv@(InvoiceId srcInvText), _) <- placed ctx (orderAt now aliceOrder)
  report "InvoiceSettled" srcInv settledState
  src <- orderOf srcInv
  nrPhase <$> registrationById ctx src `shouldReturn` OQueued Nothing
  takenNow ctx src
  balanceView ctx src `shouldReturn` balanceObject 10000 0
  let s = ridText src
  -- the balance on dynamis, with an invoice for the difference that holds all of it until BTCPay expires it
  (200, held) <- spend ctx src (dynamisAgain 2)
  let firstInv = invoiceIdOf held
  first <- orderOf firstInv
  let f = ridText first
  links ctx `shouldReturn` sort [(s, Nothing, Just f), (f, Just s, Nothing)]
  report "InvoiceExpired" firstInv ["status" J..= ("Expired" :: Text)]
  nrPhase <$> registrationById ctx first `shouldReturn` OUnpaid False
  balanceView ctx src `shouldReturn` balanceObject 10000 0
  links ctx `shouldReturn` sort [(s, Nothing, Nothing), (f, Just s, Nothing)]
  -- money paid into the expired invoice: the poller's read cannot credit it while the rate read fails, and its next credit pass does
  setRates fake ratesDown
  report "InvoicePaymentSettled" firstInv latePaid
  (\v -> (fieldOf v "latePayment", fieldOf v "balance")) <$> viewOf ctx first `shouldReturn` (Just $ J.object ["waiting" J..= ("crediting" :: Text)], Just J.Null)
  setRates fake ratesUp
  void $ capturingStderr $ forgetRates poller >> getCurrentTime >>= creditPass poller
  (\v -> (fieldOf v "latePayment", fieldOf v "balance")) <$> viewOf ctx first `shouldReturn` (Just J.Null, balanceObject 5399 0)
  balanceView ctx src `shouldReturn` balanceObject 10000 0
  -- dynamis again with a new commitment: BTCPay settles the difference, and the poller queues the order and spends the hold
  (200, again) <- spend ctx src (dynamisAgain 3)
  let dearerInv@(InvoiceId dearerInvText) = invoiceIdOf again
  dearer <- orderOf dearerInv
  let d = ridText dearer
  links ctx `shouldReturn` sort [(s, Nothing, Just d), (f, Just s, Nothing), (d, Just s, Nothing)]
  report "InvoiceSettled" dearerInv settledState
  (\r -> (nrPhase r, nrPrice r, paymentBalance (nrPayment r))) <$> registrationById ctx dearer `shouldReturn` (OQueued Nothing, 20000, Just BalanceUse {buSource = src, buUsed = 10000})
  balanceView ctx src `shouldReturn` balanceObject 0 0
  links ctx `shouldReturn` sort [(s, Nothing, Nothing), (f, Just s, Nothing), (d, Just s, Nothing)]
  -- that order taken in turn keeps its whole price; a covered spend of alicebakery takes half, and support refunds the rest
  takenNow ctx dearer
  covering <- registrationAnswer . snd <$> spend ctx dearer (timedAt now $ (covered 20000) {sbOrder = aliceOrder {oCommitment = commitmentOf 4}})
  (\r -> (nrPhase r, paymentBalance (nrPayment r))) <$> registrationById ctx covering `shouldReturn` (OQueued Nothing, Just BalanceUse {buSource = dearer, buUsed = 10000})
  dearerRef <- providerRef ctx dearerInv
  srcRef <- providerRef ctx srcInv
  owner <- addressText . nrOwner <$> registrationById ctx dearer
  let paidBy =
        "order " <> d <> " of " <> owner <> ": $100.00 by btc on invoice " <> dearerInvText <> " (provider reference " <> dearerRef <> "), $100.00 from the balance of order " <> s
          <> "; order " <> s <> " of " <> owner <> ": $100.00 by btc on invoice " <> srcInvText <> " (provider reference " <> srcRef <> ")"
  operatorCmd cc ("//balance " <> d)
    `shouldReturn` Right ("Order " <> d <> ": balance $100.00 (credited $200.00, refunded $0.00). Paid by: " <> paidBy <> ". To record its refund: //record_refund " <> d <> " 10000")
  operatorCmd cc ("//record_refund " <> dearerInvText <> " 10000") `shouldReturn` Right ("Recorded a refund of $100.00 for order " <> d <> ". Refund it now, by: " <> paidBy <> ".")
  balanceView ctx dearer `shouldReturn` balanceObject 0 10000
