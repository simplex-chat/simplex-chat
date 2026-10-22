{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesOrdersTests (namesOrdersTests) where

import Bots.StoreService.FakeBTCPay (FakeBTCPay (..), setInvoiceState, withFakeBTCPay)
import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (alice, answerRegistered, answerRegistrar, launchPricesAnswer, nameExpiresCall, noncesCall, oracle, pricesCall, registrarOf, rentPriceAnswer, rentPriceCall, reservedNamesCall, unhex)
import Bots.StoreService.NamesStoreTests (issue, order, ownerA, ownerB, placeCode, placePaid, t0, usable)
import Bots.StoreService.WebTests
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, concurrently, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar)
import qualified Control.Exception as E
import Control.Arrow ((&&&))
import Control.Monad (forM_, void)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (defaultManagerSettings, newManager, responseBody)
import Network.HTTP.Types (hContentType)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentDestination (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withConnection, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, mkPrivateKey, signRecoverable)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.EIP712 (serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD (..), labelHash)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..), ServiceConfig (..))
import StoreService.Names.Abi (addressWord, hex, uint256)
import StoreService.Names.Chain (newChainClient)
import StoreService.Names.Contracts (newContracts)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), awaitChange, newPhases)
import StoreService.Names.Signatures (OrderFields (..), orderMessage, personalDigest)
import StoreService.Names.Store
import StoreService.Names.Turns (newTurns)
import StoreService.Names.View (ViewReads (..), readViewChain, viewJSON)
import StoreService.Orders (cancelOpenInvoice, expireOverdue, settleOrder)
import StoreService.Poller (cleanupInterval, cleanupReadsPerPass, cleanupUnpaid, drainHints, newPollerEnv, newReadHints, queueReadHint, runOnePass, unpaidRetention)
import StoreService.Providers.BTCPay (btcpayProvider)
import StoreService.Providers (Funded (..), OrderDraft (..), PaymentSignal (..), Provider (..), ProviderError (..), ProviderInvoice (..))
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceProduct (..), InvoiceRow (..), OverdueInvoice (..), ProviderRefConflict (..), getInvoice, getInvoiceByProviderRef, overdueInvoices, unpaidRefs)
import StoreService.Waiters (newWaiters)
import StoreService.Web.Server (WebEnv (..), newWebEnv, webApp, webSettings)
import System.Directory (createDirectoryIfMissing)
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)

namesOrdersTests :: Spec
namesOrdersTests = describe "StoreService names: orders" $ do
  describe "paid orders" $ do
    it "places an order as an unpaid live registration and answers the name invoice view" testPaidOrder
    it "accepts the order the webapp's wallet signed, as viem signs it" testViemOrder
    it "answers an exact resend with its invoice, also with a new timestamp and signature, and after the timestamp window" testExactResend
    it "answers an exact resend after its order went past unpaid, before any chain read" testResendPastUnpaid
    it "answers an exact resend of an expired order with its expired invoice" testResendExpired
    it "refuses a resend differing in any one field, or a new commitment, with a bare order_exists" testOneFieldDiffers
    it "refuses an order carrying another wallet's commitment without disclosing anything" testForeignCommitment
    it "refuses a bad signature, or another signer's" testBadSignature
    it "refuses a stale timestamp on a new order" testStaleTimestamp
    it "lets one of two concurrent orders for one owner and name win, the other never reaching the provider" testConcurrentOrders
    it "cancels the provider invoice of an order whose insert lost to another with the same commitment" testLostInsertCancels
    it "cancels the provider invoice and logs it when the order's insert fails" testInsertFailureCancels
    it "checks the duration within the slack, before and after the term start" testDurationSlack
    it "answers catalog_changed with the chain price" testCatalogChanged
    it "answers name_taken for a registered, reserved or being registered name" testNameTaken
    it "answers name_check_unavailable when a status or price read fails" testNameCheckUnavailable
    it "refuses sealed missing or over 11095 characters and a body over 16384 bytes, accepting the longest order" testSizes
    it "refuses malformed fields, an unknown kind, and an order outside 6-63 letters" testFormats
    it "accepts orders for one name from two wallets" testTwoWallets
  describe "closed orders" $ do
    it "accepts a new order of the same wallet and name after a cancel" testOrderAfterCancel
    it "closes an overdue order of the same wallet and name before the poller marks it, cancelling its invoice after" testOverdueClosed
    it "records a payment of the closed order as a late payment, leaving the newer order alone" testLatePaymentAfterNewOrder
    it "keeps an overdue order whose invoice holds money, refusing the next order, and settles it" testFundedOverdueKept
    it "alerts on a late payment as BTCPay reports it for an expired, cancelled or swept invoice, once" testLateBTCPayPayments
    it "alerts once on money read while open when the provider then closes the invoice, also after a restart" testOneLateAlert
    it "answers cancel with funded, not_open, paid when settlement won, and expired with the payment read meanwhile" testCancelAnswers
    it "alerts once when a cancel expires an invoice a payment landed on while the provider was told" testCancelAlertsFunded
    it "leaves a late payment of a cancelled invoice unpaid and alerts" testLatePayment
    it "queues an order settled while open and publishes its version" testSettlementQueues
    it "takes the invoice's row before recording a payment that moves no status" testSettlementLocksInvoice
    it "closes the order of an invoice the provider closed or the sweep expired" testExpiryCloses
    it "sweeps each invoice on its own, skipping one whose order cannot be read" testSweepSkipsBadRow
    it "keeps out of a pass's sweep only the invoices whose read or settlement failed" testPassSweepsAroundFailures
    it "cleans up unpaid orders after 30 days, reading the provider first" testCleanup
    it "reads at most a pass's share of the provider in a cleanup, carrying the rest to the next pass" testCleanupCapped
    it "answers 503 for a name invoice while names are off, cancelling nothing" testNamesOff
  describe "invoice side" $ do
    it "finds an invoice by provider ref in either table, refusing a ref both hold" testProviderRefs
    it "reads and sweeps name invoices with the badge ones" testPollerQueries
    it "agrees between the sweep and the overdue close on which invoices hold money" testFundedAgreement
  describe "registration view" $ do
    it "has the contract's fields" testViewFields
    it "reads the intent nonce in committed and the code length in taken and code_issued" testViewReads
    it "leaves a failed intentNonce or codeLength read null and named in unread" testViewUnread

-- fixtures

-- 21 October 2026 09:15 UTC, the webapp's signed order vector's timestamp
orderTime :: UTCTime
orderTime = posixSecondsToUTCTime 1792574100

termStart :: UTCTime
termStart = posixSecondsToUTCTime 1798761600

yearSeconds :: Word64
yearSeconds = 31536000

slackSeconds :: Int
slackSeconds = 3600

seconds :: UTCTime -> Word64
seconds = floor . utcTimeToPOSIXSeconds

-- | What the page sends: 2 paid years and the free days until the term starts.
durationAt :: UTCTime -> Word64
durationAt now = 2 * yearSeconds + fromIntegral (max 0 (toInteger (seconds termStart) - toInteger (seconds now)))

-- the webapp's wallet, BIP-39 entropy 6f2d1c9a4b7e3f80d15a2c6e9b04f7a3, accounts 0 and 1 as viem derives them
keyA, keyB :: Secp256k1PrivateKey
keyA = privateKey "14bb03a88d47f77e17be83d8fe479ab982ac8b18b5f4bdec7e40ffccb1af9325"
keyB = privateKey "e8068cb3aaf53e5c46b40aefd8b78480453a8265d158df06174ba0e49bf0f130"

privateKey :: ByteString -> Secp256k1PrivateKey
privateKey = either error id . mkPrivateKey . BA.convert . unhex

ownerAText, ownerBText :: Text
ownerAText = "0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73"
ownerBText = "0x39ad11775d14FC8c9118Bd94f374DF3c7ad5655d"

data Order = Order
  { oLabel :: Text,
    oOwner :: Text,
    oKey :: Secp256k1PrivateKey,
    oCommitment :: ByteString,
    oSealed :: Text,
    oDuration :: Word64,
    oMethod :: Text,
    oPrice :: Int,
    oTimestamp :: Word64
  }

aliceOrder :: Order
aliceOrder =
  Order
    { oLabel = "alicebakery",
      oOwner = ownerAText,
      oKey = keyA,
      oCommitment = commitmentOf 1,
      oSealed = "sealed-alicebakery-1",
      oDuration = durationAt orderTime,
      oMethod = "btc",
      oPrice = alicePrice,
      oTimestamp = seconds orderTime
    }

-- the launch price of 11 letters for 2 years
alicePrice :: Int
alicePrice = 10000

commitmentOf :: Int -> ByteString
commitmentOf n = C.sha256Hash $ BC.pack $ "commitment " <> show n

fieldsOf :: Order -> OrderFields
fieldsOf Order {oLabel, oCommitment, oSealed, oDuration} =
  OrderFields {ofLabelHash = labelHash oLabel, ofLength = T.length oLabel, ofDuration = oDuration, ofCommitment = oCommitment, ofSealed = oSealed}

signedBody :: TVar ChaChaDRG -> Order -> IO J.Object
signedBody g o@Order {oKey, oTimestamp} = do
  sig <- either error id <$> signRecoverable g oKey (personalDigest $ orderMessage (fieldsOf o) oTimestamp)
  pure $ withSignature (hex $ serializeSignature sig) (unsignedBody o)

unsignedBody :: Order -> J.Object
unsignedBody o@Order {oOwner, oMethod, oPrice, oTimestamp} =
  KM.fromList
    [ ("kind", "name"),
      ("labelHash", J.String $ hex lh),
      ("length", J.toJSON ofLength),
      ("duration", J.toJSON ofDuration),
      ("owner", J.String oOwner),
      ("commitment", J.String $ hex ofCommitment),
      ("sealed", J.String ofSealed),
      ("method", J.String oMethod),
      ("price", J.toJSON oPrice),
      ("timestamp", J.toJSON oTimestamp)
    ]
  where
    OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed} = fieldsOf o

withSignature :: Text -> J.Object -> J.Object
withSignature sig = KM.insert "signature" (J.String sig)

data Ctx = Ctx
  { cStore :: DBStore,
    cNode :: FakeEthNode,
    cOrders :: OrdersEnv,
    cWeb :: WebEnv,
    cClient :: WebClient,
    cStub :: IORef StubState,
    cClock :: IORef UTCTime,
    cRandom :: TVar ChaChaDRG,
    cIps :: IORef Int
  }

testNamesConfig :: FakeEthNode -> NamesConfig
testNamesConfig node =
  NamesConfig {nTLD = TLDSimplex, nYears = 2, nTermStart = termStart, nTermSlackSeconds = slackSeconds, nRevealMarginSeconds = 900, nMaxOrderMoves = 3, nChain = fakeChainConfig node}

-- | Each invoice the stub creates gets its own provider ref, and a card invoice its client secret.
freshRefs :: IORef Int -> Provider -> Provider
freshRefs counter p = p {pCreateInvoice = \method draft -> pCreateInvoice p method draft >>= traverse fresh}
  where
    fresh pinv = do
      n <- atomicModifyIORef' counter (\i -> (i + 1, i))
      let ref = "pref-" <> tshow n
      pure $ case pProvider p of
        PPStripe -> pinv {piProviderRef = ref, piDestination = SPDCard CPStripe ("pi_secret_" <> tshow n)}
        _ -> pinv {piProviderRef = ref}

withOrders :: (Ctx -> IO a) -> IO a
withOrders = withOrdersVia id

-- | With the providers changed as a test needs them.
withOrdersVia :: (Provider -> Provider) -> (Ctx -> IO a) -> IO a
withOrdersVia via action = withServiceStore $ \cStore -> withFakeEthNode $ \cNode -> do
  cRandom <- C.newRandom
  ct <- newContracts TLDSimplex =<< newChainClient cRandom (fakeChainConfig cNode)
  turns <- newTurns
  phases <- newPhases
  cClock <- newIORef orderTime
  cStub <- newIORef (newStubState (Right sampleProviderInvoice))
  counter <- newIORef (1 :: Int)
  cIps <- newIORef 1
  answerAvailable cNode
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "names-orders" $ \root -> do
    staticDir <- prepareStaticDir root
    let cfg = (testServiceConfig staticDir True) {btcpay = Just testBTCPayConfig}
        cOrders = OrdersEnv {oeStore = cStore, oeConfig = testNamesConfig cNode, oeContracts = ct, oeTurns = turns, oePhases = phases, oeNow = readIORef cClock}
        crypto = via $ freshRefs counter (stubProvider cStub)
        card = via $ freshRefs counter (stubProvider cStub) {pProvider = PPStripe}
    waiters <- newWaiters
    hints <- newReadHints
    built <- newWebEnv cStore cfg waiters hints [crypto, card] Nothing
    let cWeb = built {weOrders = Just cOrders}
    mgr <- newManager defaultManagerSettings
    Warp.testWithApplicationSettings (webSettings (listener cfg)) (pure (webApp cWeb)) $ \port ->
      action Ctx {cStore, cNode, cOrders, cWeb, cClient = (mgr, "http://127.0.0.1:" <> show port), cStub, cClock, cRandom, cIps}

-- | alicebakery and dynamis available, at the launch prices for 2 years.
answerAvailable :: FakeEthNode -> IO ()
answerAvailable node = do
  answerRegistrar node
  setBlock node 100 (seconds orderTime)
  forM_ [nameExpiresCall, dynamisExpiresCall] $ \call -> answerCall node (registrarOf node) call $ Returns $ uint256 (0 :: Int)
  forM_ [reservedNamesCall, dynamisReservedCall] $ \call -> answerCall node fakeController call $ Returns $ uint256 (0 :: Int)
  answerCall node fakeController (rentPriceCall 11) $ Returns $ rentPriceAnswer 10000
  answerCall node fakeController (rentPriceCall 7) $ Returns $ rentPriceAnswer 20000

-- viem: nameExpires and reservedNames of labelhash("dynamis")
dynamisExpiresCall, dynamisReservedCall :: ByteString
dynamisExpiresCall = unhex "d6e4fa8678b602f03f164366531da963764c1f2fdfd588d0487daf2aa261ca799e5ffe33"
dynamisReservedCall = unhex "389c074878b602f03f164366531da963764c1f2fdfd588d0487daf2aa261ca799e5ffe33"

-- | Each request from its own client address, so the create limit counts none twice.
post :: Ctx -> String -> LB.ByteString -> IO (Int, Maybe J.Object)
post Ctx {cClient, cIps} path body = do
  n <- atomicModifyIORef' cIps (\i -> (i + 1, i))
  let ip = "198.51." <> show (n `div` 250) <> "." <> show (n `mod` 250 + 1)
  r <- webRequestBody cClient "POST" path [(hContentType, "application/json"), ("X-Forwarded-For", BC.pack ip)] body
  pure (statusOf r, J.decode (responseBody r))

postOrder :: Ctx -> J.Object -> IO (Int, Maybe J.Object)
postOrder ctx = post ctx "/api/invoice" . J.encode

placeOrder :: Ctx -> Order -> IO (Int, Maybe J.Object)
placeOrder ctx o = signedBody (cRandom ctx) o >>= postOrder ctx

-- | A placed order's invoice id, failing the test unless the order was accepted.
placed :: Ctx -> Order -> IO (InvoiceId, J.Object)
placed ctx o =
  placeOrder ctx o >>= \case
    (200, Just v) | Just (J.String invId) <- KM.lookup "invoiceId" v -> pure (InvoiceId invId, v)
    other -> error $ "order refused: " <> show other

errorOnly :: Text -> Maybe J.Object
errorOnly tag = Just $ KM.fromList [("error", J.String tag)]

refusedWith :: Int -> Text -> (Int, Maybe J.Object)
refusedWith status tag = (status, errorOnly tag)

registrationOf :: J.Object -> J.Object
registrationOf v = case KM.lookup "registration" v of
  Just (J.Object r) -> r
  _ -> error "no registration in the view"

field :: Text -> J.Object -> Maybe J.Value
field k = KM.lookup (K.fromText k)

advance :: Ctx -> NominalDiffTime -> IO ()
advance Ctx {cClock} by = modifyIORef' cClock (addUTCTime by)

registrationsOf :: Ctx -> Address -> IO [NameRegistration]
registrationsOf Ctx {cStore} owner = withTransaction cStore (`findRegistrations` owner)

invoiceRegistration :: Ctx -> InvoiceId -> IO NameRegistration
invoiceRegistration Ctx {cStore} invId =
  getInvoice cStore invId >>= \case
    Just InvoiceRow {irProduct = IPName rid} -> withTransaction cStore $ \db -> fromJust <$> getRegistration db rid
    other -> error $ "not a name invoice: " <> show other

settleNow :: Ctx -> InvoiceId -> PaymentSignal -> IO (Either Text InvoiceStatus)
settleNow Ctx {cStore, cWeb, cOrders, cClock} invId signal = readIORef cClock >>= settleOrder cStore (weWaiters cWeb) (Just $ oePhases cOrders) invId signal

settled :: PaymentSignal
settled = SigSettled (rcv 10000 (Just "0.00156")) orderTime

textOf :: Text -> Maybe J.Object -> Text
textOf k v = case field k =<< v of
  Just (J.String t) -> t
  other -> error $ T.unpack k <> ": " <> show other

objectOf :: J.Value -> J.Object
objectOf = \case
  J.Object o -> o
  v -> error $ "not an object: " <> show v

-- tests

testPaidOrder :: IO ()
testPaidOrder = withOrders $ \ctx -> do
  (invId, v) <- placed ctx aliceOrder
  sort (KM.keys v)
    `shouldBe` sort ["invoiceId", "status", "amount", "currency", "expiresAt", "kind", "registration", "address", "cryptoAmount", "cryptoCurrency", "requiredConfirmations"]
  field "kind" v `shouldBe` Just "name"
  field "status" v `shouldBe` Just "open"
  field "amount" v `shouldBe` Just (J.Number 10000)
  field "currency" v `shouldBe` Just "usd"
  field "expiresAt" v `shouldBe` Just (J.toJSON $ addUTCTime (45 * 60) orderTime)
  let r = registrationOf v
  field "phase" r `shouldBe` Just "unpaid"
  field "version" r `shouldBe` Just (J.Number 1)
  field "owner" r `shouldBe` Just (J.String ownerAText)
  field "labelHash" r `shouldBe` Just (J.String $ hex $ (\(LabelHash h) -> h) alice)
  field "price" r `shouldBe` Just (J.Number 10000)
  field "duration" r `shouldBe` Just (J.toJSON $ durationAt orderTime)
  stubCalls (cStub ctx) `shouldReturn` [StubCreate (SPMCrypto CCBtc) OrderDraft {odAmount = CurrencyAmount 10000, odCurrency = "usd"}]
  reg <- invoiceRegistration ctx invId
  (nrPhase reg, nrLive reg, nrVersion reg, nrYears reg, nrPrice reg, nrPayment reg) `shouldBe` (PUnpaid, True, 1, 2, 10000, RPInvoice (SPMCrypto CCBtc) invId)
  nrOrderHash reg `shouldBe` orderHash ownerA (fieldsOf aliceOrder) (OTInvoice (SPMCrypto CCBtc) 10000)

-- | The body of the webapp's order vector, with viem's signature over it (NamesSignatureTests).
testViemOrder :: IO ()
testViemOrder = withOrders $ \ctx -> do
  let dynamis =
        aliceOrder
          { oLabel = "dynamis",
            oCommitment = unhex "9cf04ddef2eedfb02e6e7ba58f879c0636c584b6acc67ffe9f97a06bfb016ac1",
            oSealed = "q3Zk0Yx7c2VhbGVkLW9yZGVyLWZpeHR1cmU",
            oDuration = 69259500,
            oPrice = 20000
          }
      viemSignature = "0x73e32be461e99e64b093a4ad09b04b4d810e5623a1b0e9c9fdb4701ec2a6a2a84df9ffdccc39c5cb827cde4d846c7662f591665f1b868ab1c15778646d3f414e1b"
  signedBody (cRandom ctx) dynamis >>= (`shouldBe` withSignature viemSignature (unsignedBody dynamis))
  (status, v) <- postOrder ctx $ withSignature viemSignature (unsignedBody dynamis)
  status `shouldBe` 200
  (field "phase" . registrationOf =<< v) `shouldBe` Just "unpaid"

testExactResend :: IO ()
testExactResend = withOrders $ \ctx -> do
  body <- signedBody (cRandom ctx) aliceOrder
  (invId, first') <- placed ctx aliceOrder
  advance ctx 60
  resigned <- placed ctx aliceOrder {oTimestamp = seconds orderTime + 60}
  resigned `shouldBe` (invId, first')
  advance ctx 3600
  postOrder ctx body `shouldReturn` (200, Just first')
  length <$> stubCalls (cStub ctx) `shouldReturn` 1
  length <$> registrationsOf ctx ownerA `shouldReturn` 1

testResendPastUnpaid :: IO ()
testResendPastUnpaid = withOrders $ \ctx -> do
  (invId, _) <- placed ctx aliceOrder
  settleNow ctx invId settled `shouldReturn` Right ISPaid
  setFault (cNode ctx) "eth_call" HttpFailure
  (status, v) <- placeOrder ctx aliceOrder {oTimestamp = seconds orderTime + 1}
  status `shouldBe` 200
  (field "invoiceId" =<< v) `shouldBe` Just (J.String $ (\(InvoiceId t) -> t) invId)
  (field "status" =<< v) `shouldBe` Just "paid"
  (field "phase" . registrationOf =<< v) `shouldBe` Just "queued"

testResendExpired :: IO ()
testResendExpired = withOrders $ \ctx -> do
  (InvoiceId invId, _) <- placed ctx aliceOrder
  fst <$> post ctx ("/api/invoice/" <> T.unpack invId <> "/cancel") "" `shouldReturn` 200
  (status, v) <- placeOrder ctx aliceOrder
  status `shouldBe` 200
  (field "invoiceId" =<< v) `shouldBe` Just (J.String invId)
  (field "status" =<< v) `shouldBe` Just "expired"

testOneFieldDiffers :: IO ()
testOneFieldDiffers = withOrders $ \ctx -> do
  void $ placed ctx aliceOrder
  let variants =
        [ aliceOrder {oLabel = "dynamis"},
          aliceOrder {oCommitment = commitmentOf 2},
          aliceOrder {oSealed = "sealed-alicebakery-2"},
          aliceOrder {oDuration = oDuration aliceOrder + 1},
          aliceOrder {oMethod = "xmr"},
          aliceOrder {oMethod = "card"},
          aliceOrder {oPrice = alicePrice + 1}
        ]
  forM_ variants $ \o -> placeOrder ctx o `shouldReturn` refusedWith 409 "order_exists"
  length <$> stubCalls (cStub ctx) `shouldReturn` 1

testForeignCommitment :: IO ()
testForeignCommitment = withOrders $ \ctx -> do
  void $ placed ctx aliceOrder
  placeOrder ctx aliceOrder {oOwner = ownerBText, oKey = keyB, oLabel = "dynamis", oPrice = 20000}
    `shouldReturn` refusedWith 409 "order_exists"
  placeOrder ctx aliceOrder {oOwner = ownerBText, oKey = keyB}
    `shouldReturn` refusedWith 409 "order_exists"
  length <$> registrationsOf ctx ownerB `shouldReturn` 0

testBadSignature :: IO ()
testBadSignature = withOrders $ \ctx -> do
  byB <- signedBody (cRandom ctx) aliceOrder {oKey = keyB}
  postOrder ctx byB `shouldReturn` refusedWith 400 "bad_request"
  good <- signedBody (cRandom ctx) aliceOrder
  sig <- case KM.lookup "signature" good of
    Just (J.String t) -> pure t
    other -> error $ "signature: " <> show other
  let highS = "0x" <> T.replicate 32 "11" <> "ff" <> T.replicate 31 "11" <> "1b"
  forM_ [T.toUpper sig, T.dropEnd 2 sig <> "1d", highS, ""] $ \bad ->
    postOrder ctx (withSignature bad good) `shouldReturn` refusedWith 400 "bad_request"
  postOrder ctx (KM.delete "signature" good) `shouldReturn` refusedWith 400 "bad_request"
  stubCalls (cStub ctx) `shouldReturn` []

testStaleTimestamp :: IO ()
testStaleTimestamp = withOrders $ \ctx -> do
  let at offset = aliceOrder {oTimestamp = fromIntegral (toInteger (seconds orderTime) + offset)}
  forM_ [-601, 601] $ \offset -> placeOrder ctx (at offset) `shouldReturn` refusedWith 400 "bad_request"
  -- near the top of Word64, where a sum in the timestamp's own type would wrap
  placeOrder ctx aliceOrder {oTimestamp = maxBound} `shouldReturn` refusedWith 400 "bad_request"
  fst <$> placeOrder ctx (at (-600)) `shouldReturn` 200

-- | The provider is slow, so without the name's turn both orders would pass the existing-order check and create an invoice.
testConcurrentOrders :: IO ()
testConcurrentOrders = withOrdersVia (\p -> p {pCreateInvoice = \m d -> threadDelay 300000 >> pCreateInvoice p m d}) $ \ctx -> do
  (a, b) <- concurrently (placeOrder ctx aliceOrder) (placeOrder ctx aliceOrder {oCommitment = commitmentOf 2})
  sort [fst a, fst b] `shouldBe` [200, 409]
  filter ((== 409) . fst) [a, b] `shouldBe` [refusedWith 409 "order_exists"]
  length <$> registrationsOf ctx ownerA `shouldReturn` 1
  length <$> stubCalls (cStub ctx) `shouldReturn` 1

-- | Two names take two turns, and both creates are let through together, so both orders reach the insert.
testLostInsertCancels :: IO ()
testLostInsertCancels = do
  arrived <- newTVarIO (0 :: Int)
  let meet = atomically (modifyTVar' arrived (+ 1)) >> atomically (readTVar arrived >>= check . (>= 2))
  withOrdersVia (\p -> p {pCreateInvoice = \m d -> meet >> pCreateInvoice p m d}) $ \ctx -> do
    (a, b) <- concurrently (placeOrder ctx aliceOrder) (placeOrder ctx aliceOrder {oLabel = "dynamis", oPrice = 20000})
    sort [fst a, fst b] `shouldBe` [200, 409]
    filter ((== 409) . fst) [a, b] `shouldBe` [refusedWith 409 "order_exists"]
    [(_, winner)] <- pure $ filter ((== 200) . fst) [a, b]
    Just InvoiceRow {irProviderRef = won} <- getInvoice (cStore ctx) (InvoiceId $ textOf "invoiceId" winner)
    calls <- stubCalls (cStub ctx)
    let created = length [() | StubCreate {} <- calls]
        cancelled = [r | StubCancel r <- calls]
    created `shouldBe` 2
    length cancelled `shouldBe` 1
    cancelled `shouldNotBe` [won]

-- | The stub gives every invoice one ref, so the second order's insert breaks the unique ref.
testInsertFailureCancels :: IO ()
testInsertFailureCancels = withOrdersVia (\p -> p {pCreateInvoice = \m d -> fmap (\i -> i {piProviderRef = "pref-same"}) <$> pCreateInvoice p m d}) $ \ctx -> do
  void $ placed ctx aliceOrder
  -- a listener that answers the handler's exception instead of rethrowing it into the test
  mgr <- newManager defaultManagerSettings
  logs <- Warp.withApplication (pure $ webApp $ cWeb ctx) $ \port -> do
    let answering = ctx {cClient = (mgr, "http://127.0.0.1:" <> show port)}
    capturingStderr $ fst <$> placeOrder answering aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2} `shouldReturn` 500
  logs `shouldSatisfy` T.isInfixOf "invoice pref-same is cancelled at the provider"
  calls <- stubCalls (cStub ctx)
  [r | StubCancel r <- calls] `shouldBe` ["pref-same"]

testDurationSlack :: IO ()
testDurationSlack = withOrders $ \ctx -> do
  let slack = fromIntegral slackSeconds
      withDuration n d = aliceOrder {oCommitment = commitmentOf n, oDuration = d}
      early = durationAt orderTime
  placeOrder ctx (withDuration 2 (early + slack + 1)) `shouldReturn` refusedWith 400 "bad_request"
  placeOrder ctx (withDuration 3 (early - slack - 1)) `shouldReturn` refusedWith 400 "bad_request"
  fst <$> placeOrder ctx (withDuration 4 (early + slack)) `shouldReturn` 200
  -- after the term start no free days are left, and fewer than the paid years are refused
  let later = addUTCTime 86400 termStart
      afterStart n d = (withDuration n d) {oLabel = "dynamis", oPrice = 20000, oTimestamp = seconds later}
  writeIORef (cClock ctx) later
  durationAt later `shouldBe` 2 * yearSeconds
  placeOrder ctx (afterStart 5 (2 * yearSeconds - 1)) `shouldReturn` refusedWith 400 "bad_request"
  placeOrder ctx (afterStart 6 (2 * yearSeconds + slack + 1)) `shouldReturn` refusedWith 400 "bad_request"
  fst <$> placeOrder ctx (afterStart 7 (2 * yearSeconds + slack)) `shouldReturn` 200

testCatalogChanged :: IO ()
testCatalogChanged = withOrders $ \ctx -> do
  placeOrder ctx aliceOrder {oPrice = 9000} `shouldReturn` (409, Just $ KM.fromList [("error", "catalog_changed"), ("price", J.Number 10000)])
  stubCalls (cStub ctx) `shouldReturn` []

testNameTaken :: IO ()
testNameTaken = withOrders $ \ctx@Ctx {cNode = node} -> do
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (1 :: Int)
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 409 "name_taken"
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
  answerRegistered node (cResolverOf node)
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (seconds orderTime + yearSeconds)
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 409 "name_taken"
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  -- another wallet's order of the name being registered here
  Right () <- placePaid (cStore ctx) "other" ownerB (order "alicebakery" 9) (addUTCTime 3600 t0)
  other <- withTransaction (cStore ctx) $ \db -> fromJust <$> getRegistration db (RegistrationId "other")
  Written _ <- withTransaction (cStore ctx) $ \db -> updateRegistration db other {nrPhase = PRegistering} t0
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 409 "name_taken"
  stubCalls (cStub ctx) `shouldReturn` []
  where
    cResolverOf = cResolver . fakeChainConfig

testNameCheckUnavailable :: IO ()
testNameCheckUnavailable = withOrders $ \ctx@Ctx {cNode = node} -> do
  setFault node "eth_getBlockByNumber" HttpFailure
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 503 "name_check_unavailable"
  clearFault node "eth_getBlockByNumber"
  answerCall node fakeController (rentPriceCall 11) $ Reverts ""
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 503 "name_check_unavailable"
  stubCalls (cStub ctx) `shouldReturn` []

testSizes :: IO ()
testSizes = withOrders $ \ctx -> do
  missing <- KM.delete "sealed" <$> signedBody (cRandom ctx) aliceOrder
  postOrder ctx missing `shouldReturn` refusedWith 400 "bad_request"
  placeOrder ctx aliceOrder {oSealed = T.replicate 11096 "a"} `shouldReturn` refusedWith 400 "bad_request"
  padded <- KM.insert "padding" (J.String $ T.replicate 16384 "a") <$> signedBody (cRandom ctx) aliceOrder
  postOrder ctx padded `shouldReturn` refusedWith 400 "bad_request"
  longest <- signedBody (cRandom ctx) aliceOrder {oSealed = T.replicate 11095 "a"}
  LB.length (J.encode longest) `shouldSatisfy` (< 16384)
  fst <$> postOrder ctx longest `shouldReturn` 200

testFormats :: IO ()
testFormats = withOrders $ \ctx -> do
  good <- signedBody (cRandom ctx) aliceOrder
  let with k v = KM.insert k v good
      lowerOwner = J.String $ T.toLower ownerAText
  forM_
    [ with "labelHash" (J.String $ T.toUpper $ hex $ (\(LabelHash h) -> h) alice),
      with "commitment" (J.String "0x1234"),
      with "owner" lowerOwner,
      with "method" "eth",
      with "price" (J.Number 100.5),
      with "duration" (J.Number (-1)),
      with "timestamp" "1792574100",
      with "length" (J.Number 1e30),
      KM.delete "kind" good,
      with "kind" "badge"
    ]
    $ \body -> postOrder ctx body `shouldReturn` refusedWith 400 "bad_request"
  forM_ [5, 64] $ \len -> postOrder ctx (with "length" (J.Number len)) `shouldReturn` refusedWith 400 "invalid_name"
  stubCalls (cStub ctx) `shouldReturn` []

testTwoWallets :: IO ()
testTwoWallets = withOrders $ \ctx -> do
  (a, _) <- placed ctx aliceOrder
  (b, _) <- placed ctx aliceOrder {oOwner = ownerBText, oKey = keyB, oCommitment = commitmentOf 2}
  a `shouldNotBe` b
  mapM (fmap nrLive . invoiceRegistration ctx) [a, b] `shouldReturn` [True, True]

testOrderAfterCancel :: IO ()
testOrderAfterCancel = withOrders $ \ctx -> do
  (invId@(InvoiceId invText), _) <- placed ctx aliceOrder
  (status, v) <- post ctx ("/api/invoice/" <> T.unpack invText <> "/cancel") ""
  status `shouldBe` 200
  (field "status" =<< v) `shouldBe` Just "expired"
  (field "version" . registrationOf =<< v) `shouldBe` Just (J.Number 2)
  old <- invoiceRegistration ctx invId
  nrLive old `shouldBe` False
  (newInvId, _) <- placed ctx aliceOrder {oCommitment = commitmentOf 2, oTimestamp = seconds orderTime + 1}
  newInvId `shouldNotBe` invId
  nrLive <$> invoiceRegistration ctx newInvId `shouldReturn` True

testOverdueClosed :: IO ()
testOverdueClosed = withOrders $ \ctx -> do
  (oldInv, _) <- placed ctx aliceOrder {oMethod = "card"}
  -- a card invoice expires after the session's 60 minutes; this is past it, before any sweep
  advance ctx (61 * 60)
  now <- readIORef (cClock ctx)
  -- a refused order closes nothing
  placeOrder ctx aliceOrder {oCommitment = commitmentOf 2, oTimestamp = seconds now} `shouldReturn` refusedWith 400 "bad_request"
  nrLive <$> invoiceRegistration ctx oldInv `shouldReturn` True
  (newInv, _) <- placed ctx aliceOrder {oMethod = "card", oCommitment = commitmentOf 2, oTimestamp = seconds now, oDuration = durationAt now}
  Just oldRow <- getInvoice (cStore ctx) oldInv
  irStatus oldRow `shouldBe` ISExpired
  old <- invoiceRegistration ctx oldInv
  (nrPhase old, nrLive old, nrVersion old) `shouldBe` (PUnpaid, False, 2)
  nrLive <$> invoiceRegistration ctx newInv `shouldReturn` True
  calls <- stubCalls (cStub ctx)
  [c | c@StubCancel {} <- calls] `shouldBe` [StubCancel (irProviderRef oldRow)]

testLatePaymentAfterNewOrder :: IO ()
testLatePaymentAfterNewOrder = withOrders $ \ctx -> do
  (oldInv, _) <- placed ctx aliceOrder
  advance ctx (46 * 60)
  now <- readIORef (cClock ctx)
  (newInv, _) <- placed ctx aliceOrder {oCommitment = commitmentOf 2, oTimestamp = seconds now, oDuration = durationAt now}
  logs <- capturingStderr $ settleNow ctx oldInv settled `shouldReturn` Right ISPaid
  logs `shouldSatisfy` T.isInfixOf "refund"
  old <- invoiceRegistration ctx oldInv
  (nrPhase old, nrLive old) `shouldBe` (PUnpaid, False)
  new <- invoiceRegistration ctx newInv
  (nrPhase new, nrLive new, nrVersion new) `shouldBe` (PUnpaid, True, 1)
  settleNow ctx newInv settled `shouldReturn` Right ISPaid
  (nrPhase <$> invoiceRegistration ctx newInv) `shouldReturn` PQueued

-- | Paid in full at minute 44 of 45 and confirming after; a second tab's order at minute 46 must not refund it.
testFundedOverdueKept :: IO ()
testFundedOverdueKept = withOrders $ \ctx -> do
  (inv, _) <- placed ctx aliceOrder
  advance ctx (44 * 60)
  settleNow ctx inv (SigFunded (rcv 10000 (Just "0.00156")) PaidInFull) `shouldReturn` Right ISOpen
  advance ctx (2 * 60)
  now <- readIORef (cClock ctx)
  placeOrder ctx aliceOrder {oCommitment = commitmentOf 2, oTimestamp = seconds now, oDuration = durationAt now} `shouldReturn` refusedWith 409 "order_exists"
  r <- invoiceRegistration ctx inv
  (nrLive r, nrPhase r) `shouldBe` (True, PUnpaid)
  withTransaction (cStore ctx) (\db -> closeOverdue db r now) `shouldReturn` Nothing
  (irStatus <$>) <$> getInvoice (cStore ctx) inv `shouldReturn` Just ISOpen
  settleNow ctx inv settled `shouldReturn` Right ISPaid
  nrPhase <$> invoiceRegistration ctx inv `shouldReturn` PQueued

-- | The real BTCPay adapter against the fake: a payment into an expired invoice reads Expired with PaidLate,
-- into a cancelled one Invalid, and into one only we closed New and paid in part.
testLateBTCPayPayments :: IO ()
testLateBTCPayPayments = withFakeBTCPay $ \fake -> do
  btcpay <- btcpayProvider (fbConfig fake)
  withOrdersVia (const btcpay) $ \ctx@Ctx {cStore = st, cWeb} -> do
    poller <- newPollerEnv st (weWaiters cWeb) (Just $ oePhases $ cOrders ctx) (weHints cWeb) (weProviders cWeb) (poll $ weConfig cWeb)
    let refOf inv = maybe (error "no invoice") irProviderRef <$> getInvoice st inv
        readAt ref = queueReadHint (weHints cWeb) ref >> drainHints poller
        paidLate ref fields = capturingStderr $ setInvoiceState fake ref fields >> readAt ref
        alerted logs = T.isInfixOf "stays unpaid; refund the payment" logs
        later = addUTCTime 3600 orderTime
    (expired, _) <- placed ctx aliceOrder
    expiredRef <- refOf expired
    setInvoiceState fake expiredRef ["status" .= ("Expired" :: Text)]
    readAt expiredRef
    (irStatus <$>) <$> getInvoice st expired `shouldReturn` Just ISExpired
    paidLate expiredRef ["additionalStatus" .= ("PaidLate" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)] >>= (`shouldSatisfy` alerted)
    capturingStderr (readAt expiredRef) >>= (`shouldNotSatisfy` alerted)
    (cancelled, _) <- placed ctx aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}
    cancelledRef <- refOf cancelled
    cancelOpenInvoice st (weWaiters cWeb) (Just $ oePhases $ cOrders ctx) cancelled later `shouldReturn` True
    paidLate cancelledRef ["status" .= ("Invalid" :: Text), "paymentMethodPaid" .= ("0.00078000" :: Text)] >>= (`shouldSatisfy` alerted)
    (swept, _) <- placed ctx aliceOrder {oOwner = ownerBText, oKey = keyB, oCommitment = commitmentOf 3}
    sweptRef <- refOf swept
    expireOverdue st (weWaiters cWeb) (Just $ oePhases $ cOrders ctx) later later [swept] `shouldReturn` [swept]
    paidLate sweptRef ["paymentMethodPaid" .= ("0.00025000" :: Text)] >>= (`shouldSatisfy` alerted)
    forM_ [expired, cancelled, swept] $ \inv -> do
      r <- invoiceRegistration ctx inv
      (nrPhase r, nrLive r) `shouldBe` (PUnpaid, False)

-- | Paid in part while open, then expired by BTCPay with the same figures: one refund line, none from a restarted poller.
testOneLateAlert :: IO ()
testOneLateAlert = withOrders $ \ctx@Ctx {cStore = st, cWeb} -> do
  (inv, _) <- placed ctx aliceOrder
  Just InvoiceRow {irProviderRef = ref} <- getInvoice st inv
  let paid = rcv 5000 (Just "0.00078")
      newPoller = newPollerEnv st (weWaiters cWeb) (Just $ oePhases $ cOrders ctx) (weHints cWeb) (weProviders cWeb) (poll $ weConfig cWeb)
      readWith poller = capturingStderr $ queueReadHint (weHints cWeb) ref >> drainHints poller
      refunds = length . T.breakOnAll "refund"
  poller <- newPoller
  setSignals (cStub ctx) [(ref, SigFunded paid PaidInPart)]
  refunds <$> readWith poller `shouldReturn` 0
  setSignals (cStub ctx) [(ref, SigClosed paid)]
  logs <- readWith poller
  refunds logs `shouldBe` 1
  logs `shouldSatisfy` T.isInfixOf "stays unpaid; refund the payment"
  restarted <- newPoller
  refunds <$> readWith restarted `shouldReturn` 0
  (nrPhase &&& nrLive) <$> invoiceRegistration ctx inv `shouldReturn` (PUnpaid, False)

testCancelAnswers :: IO ()
testCancelAnswers = withOrders $ \ctx -> do
  let cancel = cancelAt ctx
  (funded, _) <- placed ctx aliceOrder
  settleNow ctx funded (SigFunded (rcv 5000 (Just "0.00078")) PaidInPart) `shouldReturn` Right ISOpen
  cancel funded `shouldReturn` refusedWith 409 "funded"
  (other, _) <- placed ctx aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}
  fst <$> cancel other `shouldReturn` 200
  cancel other `shouldReturn` refusedWith 409 "not_open"
  cancel (InvoiceId "no-such-invoice") `shouldReturn` refusedWith 404 "not_found"
  -- the provider settles the invoice, or reads a payment into it, while taking the cancel
  withOrdersCancelling settled $ \ctx' -> do
    (invId, _) <- placed ctx' aliceOrder
    (status, v) <- cancelAt ctx' invId
    (status, field "status" =<< v, field "phase" . registrationOf =<< v) `shouldBe` (200, Just "paid", Just "queued")
  withOrdersCancelling (SigFunded (rcv 0 Nothing) PaidInFull) $ \ctx' -> do
    (invId, _) <- placed ctx' aliceOrder
    (status, v) <- cancelAt ctx' invId
    (status, field "status" =<< v, field "paidInFull" =<< v) `shouldBe` (200, Just "expired", Just (J.Bool True))
    r <- invoiceRegistration ctx' invId
    (nrLive r, nrVersion r) `shouldBe` (False, 2)
  where
    cancelAt c (InvoiceId t) = post c ("/api/invoice/" <> T.unpack t <> "/cancel") ""

-- | Cancel takes the invoice row and decides on the payment it then reads, as settlement does.
testCancelAlertsFunded :: IO ()
testCancelAlertsFunded = withOrdersCancelling (SigFunded paid PaidInPart) $ \ctx -> do
  (invId, _) <- placed ctx aliceOrder
  logs <- capturingStderr $ do
    (status, v) <- post ctx ("/api/invoice/" <> T.unpack (invoiceText invId) <> "/cancel") ""
    (status, field "status" =<< v, field "cryptoAmountPaid" =<< v) `shouldBe` (200, Just "expired", Just "0.00078")
  length (T.breakOnAll (alertOf invId) logs) `shouldBe` 1
  capturingStderr (settleNow ctx invId (SigClosed paid) `shouldReturn` Right ISExpired) >>= (`shouldNotSatisfy` T.isInfixOf (alertOf invId))
  (nrPhase &&& nrLive) <$> invoiceRegistration ctx invId `shouldReturn` (PUnpaid, False)
  where
    paid = rcv 5000 (Just "0.00078")
    invoiceText (InvoiceId t) = t
    alertOf invId = "invoice " <> invoiceText invId <> " was paid after its order closed"

testNamesOff :: IO ()
testNamesOff = withOrders $ \ctx -> do
  (invId@(InvoiceId t), _) <- placed ctx aliceOrder
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure $ webApp (cWeb ctx) {weOrders = Nothing}) $ \port -> do
    let off = ctx {cClient = (mgr, "http://127.0.0.1:" <> show port)}
    logs <- capturingStderr $ do
      r <- webGet (cClient off) ("/api/invoice/" <> T.unpack t)
      (statusOf r, J.decode (responseBody r)) `shouldBe` refusedWith 503 "provider_unavailable"
      post off ("/api/invoice/" <> T.unpack t <> "/cancel") "" `shouldReturn` refusedWith 503 "provider_unavailable"
    logs `shouldSatisfy` T.isInfixOf ("invoice " <> t <> " is a name's, and this service sells no names")
  calls <- stubCalls (cStub ctx)
  [c | c@StubCancel {} <- calls] `shouldBe` []
  (irStatus <$>) <$> getInvoice (cStore ctx) invId `shouldReturn` Just ISOpen

-- | The provider's cancel settles the invoice with the signal before it answers.
withOrdersCancelling :: PaymentSignal -> (Ctx -> IO a) -> IO a
withOrdersCancelling signal action = withOrders $ \ctx -> do
  let Ctx {cWeb} = ctx
      cancelling p = p {pCancelInvoice = \ref -> settleRef ctx ref >> pCancelInvoice p ref}
      web = cWeb {weProviders = map cancelling (weProviders cWeb)}
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure (webApp web)) $ \port -> action ctx {cWeb = web, cClient = (mgr, "http://127.0.0.1:" <> show port)}
  where
    settleRef ctx ref =
      getInvoiceByProviderRef (cStore ctx) ref >>= mapM_ (\InvoiceRow {irInvoiceId} -> settleNow ctx irInvoiceId signal)

testLatePayment :: IO ()
testLatePayment = withOrders $ \ctx -> do
  (invId@(InvoiceId t), _) <- placed ctx aliceOrder
  fst <$> post ctx ("/api/invoice/" <> T.unpack t <> "/cancel") "" `shouldReturn` 200
  closed <- invoiceRegistration ctx invId
  logs <- capturingStderr $ settleNow ctx invId settled `shouldReturn` Right ISPaid
  logs `shouldSatisfy` T.isInfixOf ("registration " <> (\(RegistrationId r) -> r) (nrId closed) <> " stays unpaid")
  invoiceRegistration ctx invId `shouldReturn` closed
  r <- webGet (cClient ctx) ("/api/invoice/" <> T.unpack t)
  v <- jsonObject r
  (field "status" v, field "phase" (registrationOf v)) `shouldBe` (Just "paid", Just "unpaid")

testSettlementQueues :: IO ()
testSettlementQueues = withOrders $ \ctx -> do
  (invId, _) <- placed ctx aliceOrder
  NameRegistration {nrId} <- invoiceRegistration ctx invId
  parked <- newEmptyMVar
  -- the version read is pinned, so only a publish can wake the waiter
  waiter <- async $ awaitChange (oePhases $ cOrders ctx) nrId (putMVar parked () >> pure 1) 1 10000000
  takeMVar parked
  settleNow ctx invId settled `shouldReturn` Right ISPaid
  timeout 2000000 (wait waiter) `shouldReturn` Just True
  r <- invoiceRegistration ctx invId
  (nrPhase r, nrLive r, nrVersion r) `shouldBe` (PQueued, True, 2)

-- | Postgres serializes the payment with a concurrent overdue close only on this row; SQLite shows that it is written.
testSettlementLocksInvoice :: IO ()
testSettlementLocksInvoice = withOrders $ \ctx -> do
  (inv@(InvoiceId t), _) <- placed ctx aliceOrder
  advance ctx 60
  settleNow ctx inv (SigFunded (rcv 10000 (Just "0.00156")) PaidInFull) `shouldReturn` Right ISOpen
  now <- readIORef (cClock ctx)
  withConnection (cStore ctx) (\db -> DB.query db "SELECT updated_at, status FROM sx_badge_service_invoices WHERE invoice_id = ?" [t])
    `shouldReturn` [(now, "open" :: Text)]

testExpiryCloses :: IO ()
testExpiryCloses = withOrders $ \ctx -> do
  (closedByProvider, _) <- placed ctx aliceOrder
  settleNow ctx closedByProvider (SigClosed (rcv 0 Nothing)) `shouldReturn` Right ISExpired
  r <- invoiceRegistration ctx closedByProvider
  (nrPhase r, nrLive r, nrVersion r) `shouldBe` (PUnpaid, False, 2)
  (swept, _) <- placed ctx aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}
  let later = addUTCTime 3600 orderTime
  expireOverdue (cStore ctx) (weWaiters $ cWeb ctx) (Just $ oePhases $ cOrders ctx) later later [swept] `shouldReturn` [swept]
  s <- invoiceRegistration ctx swept
  (nrPhase s, nrLive s, nrVersion s) `shouldBe` (PUnpaid, False, 2)

testSweepSkipsBadRow :: IO ()
testSweepSkipsBadRow = withOrders $ \ctx@Ctx {cStore = st} -> do
  (bad, _) <- placed ctx aliceOrder
  (good, _) <- placed ctx aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}
  NameRegistration {nrId = RegistrationId badId} <- invoiceRegistration ctx bad
  withConnection st $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET phase = 'lost' WHERE registration_id = ? AND phase = ?" (badId, "unpaid" :: Text)
  let later = addUTCTime 3600 orderTime
  logs <- capturingStderr $ expireOverdue st (weWaiters $ cWeb ctx) (Just $ oePhases $ cOrders ctx) later later [bad, good] `shouldReturn` [good]
  logs `shouldSatisfy` T.isInfixOf ("invoice " <> invoiceText bad)
  (irStatus <$>) <$> getInvoice st bad `shouldReturn` Just ISOpen
  r <- invoiceRegistration ctx good
  (nrLive r, nrVersion r) `shouldBe` (False, 2)
  where
    invoiceText (InvoiceId t) = t

-- | The poller reads by the wall clock, so the orders are placed two hours ago by it.
testPassSweepsAroundFailures :: IO ()
testPassSweepsAroundFailures = do
  unreadable <- newIORef ""
  let failing p = p {pReadInvoice = \r -> readIORef unreadable >>= \u -> if r == u then pure (Left $ ProviderError "down") else pReadInvoice p r}
  withOrdersVia failing $ \ctx@Ctx {cStore = st, cWeb} -> do
    placedAt <- addUTCTime (-7200) <$> getCurrentTime
    writeIORef (cClock ctx) placedAt
    let at' o = o {oTimestamp = seconds placedAt, oDuration = durationAt placedAt}
        refOf inv = maybe (error "no invoice") irProviderRef <$> getInvoice st inv
    (poisoned, _) <- placed ctx $ at' aliceOrder
    (unread, _) <- placed ctx $ at' aliceOrder {oLabel = "dynamis", oPrice = 20000, oCommitment = commitmentOf 2}
    (good, _) <- placed ctx $ at' aliceOrder {oOwner = ownerBText, oKey = keyB, oCommitment = commitmentOf 3}
    NameRegistration {nrId = RegistrationId poisonedId} <- invoiceRegistration ctx poisoned
    withConnection st $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET phase = 'lost' WHERE registration_id = ? AND phase = ?" (poisonedId, "unpaid" :: Text)
    poisonedRef <- refOf poisoned
    refOf unread >>= writeIORef unreadable
    setSignals (cStub ctx) [(poisonedRef, SigClosed (rcv 0 Nothing))]
    poller <- newPollerEnv st (weWaiters cWeb) (Just $ oePhases $ cOrders ctx) (weHints cWeb) (weProviders cWeb) (poll $ weConfig cWeb)
    void $ capturingStderr $ runOnePass poller
    mapM (fmap (irStatus <$>) . getInvoice st) [poisoned, unread, good] `shouldReturn` map Just [ISOpen, ISOpen, ISExpired]

testCleanup :: IO ()
testCleanup = withOrders $ \ctx -> testCleanupIn ctx

testCleanupIn :: Ctx -> IO ()
testCleanupIn ctx@Ctx {cStore = st} = do
  let expired = t0
      due = addUTCTime (unpaidRetention + 1) expired
      place rid n = placePaid st rid ownerA (order (T.pack ("alicebakery" <> show n)) n) expired `shouldReturn` Right ()
  mapM_ (uncurry place) [("gone", 1), ("funded", 2), ("late", 3), ("unread", 4)]
  placePaid st "recent" ownerA (order "alicebakery5" 5) (addUTCTime 3600 due) `shouldReturn` Right ()
  settleOrder st (weWaiters $ cWeb ctx) Nothing (InvoiceId "inv-funded") (SigFunded (rcv 100 (Just "0.00001")) PaidInPart) t0 `shouldReturn` Right ISOpen
  setSignals (cStub ctx) [(ref "late", SigClosed (rcv 5000 (Just "0.00078")))]
  failing <- newIORef False
  let reading p = p {pReadInvoice = \r -> readIORef failing >>= \f -> if f && r == ref "unread" then pure (Left $ ProviderError "down") else pReadInvoice p r}
      providers = map reading (weProviders $ cWeb ctx)
  writeIORef failing True
  poller <- newPollerEnv st (weWaiters $ cWeb ctx) Nothing (weHints $ cWeb ctx) providers (poll $ weConfig $ cWeb ctx)
  logs <- capturingStderr $ cleanupUnpaid poller due
  let present rid = isJust <$> withTransaction st (`getRegistration` RegistrationId rid)
  mapM present ["gone", "funded", "late", "unread", "recent"] `shouldReturn` [False, True, True, True, True]
  isNothing <$> getInvoice st (InvoiceId "inv-gone") `shouldReturn` True
  Just InvoiceRow {irPayment = latePayment} <- getInvoice st (InvoiceId "inv-late")
  (ipCryptoPaid <$> latePayment) `shouldBe` Just (Just "0.00078")
  logs `shouldSatisfy` T.isInfixOf "registration late is kept because invoice inv-late received a payment"
  logs `shouldSatisfy` T.isInfixOf "registration unread is kept until the next run"
  reads' <- stubReads
  sort reads' `shouldBe` [ref "gone", ref "late"]
  writeIORef failing False
  cleanupUnpaid poller (addUTCTime cleanupInterval due)
  mapM present ["unread", "recent"] `shouldReturn` [False, True]
  drop (length reads') <$> stubReads `shouldReturn` [ref "unread"]
  where
    stubReads = (\calls -> [r | StubRead r <- calls]) <$> stubCalls (cStub ctx)
    ref rid = "ref-inv-" <> rid

testCleanupCapped :: IO ()
testCleanupCapped = withOrders $ \ctx@Ctx {cStore = st} -> do
  let extra = 3
      count = cleanupReadsPerPass + extra
      due = addUTCTime (unpaidRetention + 1) t0
      reads' = (\calls -> length [() | StubRead _ <- calls]) <$> stubCalls (cStub ctx)
  forM_ [1 .. count] $ \n -> placePaid st ("old" <> tshow n) ownerA (order ("oldname" <> tshow n) (fromIntegral n)) t0 `shouldReturn` Right ()
  poller <- newPollerEnv st (weWaiters $ cWeb ctx) Nothing (weHints $ cWeb ctx) (weProviders $ cWeb ctx) (poll $ weConfig $ cWeb ctx)
  cleanupUnpaid poller due
  reads' `shouldReturn` cleanupReadsPerPass
  cleanupUnpaid poller (addUTCTime 1 due)
  reads' `shouldReturn` count
  length <$> registrationsOf ctx ownerA `shouldReturn` 0

testProviderRefs :: IO ()
testProviderRefs = withServiceStore $ \st -> do
  Right () <- placePaid st "r1" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  getInvoiceByProviderRef st "ref-inv-r1" >>= \case
    Just InvoiceRow {irInvoiceId, irProduct} -> (irInvoiceId, irProduct) `shouldBe` (InvoiceId "inv-r1", IPName (RegistrationId "r1"))
    Nothing -> expectationFailure "the name invoice was not found by its ref"
  withConnection st $ \db ->
    DB.execute
      db
      "INSERT INTO sx_badge_service_invoices (invoice_id, provider, price, amount, currency, expires_at, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
      ("badge-1" :: Text, "crypto" :: Text, 500 :: Int, 500 :: Int, "usd" :: Text, t0, "open" :: Text, t0, t0)
  withConnection st $ \db -> do
    DB.execute db "INSERT INTO sx_badge_service_badge_codes (code_hash, badge_type, months, code_payment_status, created_at) VALUES (?,?,?,?,?)" (DB.Binary ("hash" :: ByteString), "supporter" :: Text, 1 :: Int, "unpaid" :: Text, t0)
    DB.execute db "INSERT INTO sx_badge_service_badge_prices (price_id, badge_type, month_price, currency, status, created_at) VALUES (?,?,?,?,?,?)" ("p1" :: Text, "supporter" :: Text, 500 :: Int, "usd" :: Text, "active" :: Text, t0)
    DB.execute db "INSERT INTO sx_badge_service_badge_code_invoices (invoice_id, badge_code_id, price_id, provider_ref, created_at) VALUES (?, (SELECT badge_code_id FROM sx_badge_service_badge_codes), ?, ?, ?)" ("badge-1" :: Text, "p1" :: Text, "ref-inv-r1" :: Text, t0)
  E.try (getInvoiceByProviderRef st "ref-inv-r1") `shouldReturn` Left (ProviderRefConflict "ref-inv-r1")

testPollerQueries :: IO ()
testPollerQueries = withServiceStore $ \st -> do
  Right () <- placePaid st "r1" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  unpaidRefs st (addUTCTime (-60) t0) `shouldReturn` [("crypto", "ref-inv-r1")]
  map oiProviderRef <$> overdueInvoices st (addUTCTime 7200 t0) `shouldReturn` ["ref-inv-r1"]
  overdueInvoices st t0 `shouldReturn` []

testFundedAgreement :: IO ()
testFundedAgreement = withServiceStore $ \st -> do
  waiters <- newWaiters
  let now = addUTCTime 3600 t0
      shapes =
        [ ("amount", Just (SigFunded (rcv 100 Nothing) PaidInPart), True),
          ("crypto", Just (SigFunded (rcv 0 (Just "0.00001")) PaidInPart), True),
          ("verdict", Just (SigFunded (rcv 0 Nothing) PaidInFull), True),
          ("nothing", Nothing, False)
        ]
  forM_ (zip [1 ..] shapes) $ \(n, (rid, signal, _)) -> do
    placePaid st rid ownerA (order ("fundname" <> rid) n) t0 `shouldReturn` Right ()
    forM_ signal $ \sig -> void $ settleOrder st waiters Nothing (InvoiceId $ "inv-" <> rid) sig t0
  listed <- map oiInvoiceId <$> overdueInvoices st now
  forM_ (zip [1 ..] shapes) $ \(n, (rid, _, funded)) -> do
    let o = order ("fundname" <> rid) n
    (InvoiceId ("inv-" <> rid) `elem` listed) `shouldBe` not funded
    [LiveOrder {loRegistration, loOverdue}] <- withTransaction st $ \db -> getLiveOrders db (ofCommitment o) ownerA (ofLabelHash o) now
    (rid, loOverdue) `shouldBe` (rid, not funded)
    closed <- withTransaction st $ \db -> closeOverdue db loRegistration now
    (rid, closed) `shouldBe` (rid, if funded then Nothing else Just (nrVersion loRegistration + 1))

testViewFields :: IO ()
testViewFields = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  Right () <- placePaid st "r1" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  r <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "r1")
  let v = objectOf $ viewJSON (testNamesConfig node) r ViewReads {vrIntentNonce = Nothing, vrCodeLength = Nothing, vrUnread = []}
  sort (KM.keys v)
    `shouldBe` sort
      [ "id", "version", "moves", "linksRefused", "phase", "labelHash", "label", "owner", "commitment", "sealed", "resolver", "sponsor", "chainId", "tld",
        "years", "duration", "price", "codeLength", "commitTx", "registerTx", "revealAfter", "revealBefore", "intentNonce", "unread", "expiresAt", "error"
      ]
  map (`field` v) ["id", "phase", "owner", "tld", "chainId", "linksRefused", "label", "unread", "sponsor"]
    `shouldBe` map
      Just
      [ "r1",
        "unpaid",
        J.String ownerAText,
        "simplex",
        J.Number 31337,
        J.Bool False,
        J.Null,
        J.Array mempty,
        J.String $ safeDecodeUtf8 $ strEncode fakeSponsor
      ]

testViewReads :: IO ()
testViewReads = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  g <- C.newRandom
  ct <- newContracts TLDSimplex =<< newChainClient g (fakeChainConfig node)
  answerCall node fakeSponsor noncesCall $ Returns $ uint256 (4 :: Int)
  answerCall node fakeController pricesCall $ Returns $ addressWord oracle
  answerCall node oracle pricesCall $ Returns launchPricesAnswer
  Right () <- placePaid st "paid" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  code <- issue st 7 usable
  Right () <- placeCode st "code" ownerB (order "alicebakery" 2) code
  let inPhase rid phase = do
        r <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId rid)
        Written _ <- withTransaction st $ \db -> updateRegistration db r {nrPhase = phase} t0
        withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId rid)
  committed <- inPhase "paid" PCommitted
  readViewChain st ct committed `shouldReturn` ViewReads (Just 4) Nothing []
  taken <- inPhase "paid" PTaken
  -- worth 20000 cents for 2 years at the launch list: 7 letters and longer
  readViewChain st ct taken `shouldReturn` ViewReads Nothing (Just 7) []
  takenCode <- inPhase "code" PTaken
  readViewChain st ct takenCode `shouldReturn` ViewReads Nothing Nothing []
  issued <- issue st 8 usable {npcMinLength = 9}
  Written _ <- withTransaction st $ \db -> updateRegistration db taken {nrPhase = PCodeIssued, nrIssuedCodeId = Just (pcId issued)} t0
  codeIssued <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "paid")
  readViewChain st ct codeIssued `shouldReturn` ViewReads Nothing (Just 9) []

testViewUnread :: IO ()
testViewUnread = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  g <- C.newRandom
  ct <- newContracts TLDSimplex =<< newChainClient g (fakeChainConfig node)
  setFault node "eth_call" HttpFailure
  Right () <- placePaid st "paid" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  r <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "paid")
  readViewChain st ct r {nrPhase = PCommitted} `shouldReturn` ViewReads Nothing Nothing ["intentNonce"]
  readViewChain st ct r {nrPhase = PTaken} `shouldReturn` ViewReads Nothing Nothing ["codeLength"]
  let v = objectOf $ viewJSON (testNamesConfig node) r {nrPhase = PTaken} (ViewReads Nothing Nothing ["codeLength"])
  (field "codeLength" v, field "unread" v) `shouldBe` (Just J.Null, Just (J.toJSON ["codeLength" :: Text]))
