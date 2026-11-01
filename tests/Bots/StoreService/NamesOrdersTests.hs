{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesOrdersTests
  ( namesOrdersTests,
    Ctx (..),
    Order (..),
    withOrders,
    withOrdersCancelling,
    withSettings,
    configured,
    codesOnly,
    aliceOrder,
    commitmentOf,
    orderTime,
    placeOrder,
    placed,
    post,
    settleNow,
    settled,
    invoiceRegistration,
    registrationOf,
    keyA,
    keyB,
    ownerAText,
    ownerBText,
    yearsDuration,
    codeRoutes,
    issueCode,
    registered,
    registrationById,
    field,
    advance,
    refusedWith,
    malformedTerms,
    listName,
    bakers,
    bumpedTerms,
    newerTerms,
    olderTerms,
    communityTerms,
    termsChanged,
  )
where

import Bots.StoreService.ConfigTests (termsUrl, termsVersion)
import Bots.StoreService.FakeBTCPay (FakeBTCPay (..), setInvoiceState, withFakeBTCPay)
import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (alice, answerRegistered, answerRegistrar, nameExpiresCall, noncesCall, registrarOf, rentPriceAnswer, rentPriceCall, reservedNamesCall, unhex)
import Bots.StoreService.NamesStoreTests (btc, order, ownerA, ownerB, placeCode, placePaid, t0)
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
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (defaultManagerSettings, newManager, responseBody)
import Network.HTTP.Types (hContentType)
import Network.Wai (Application, requestHeaders)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges.Code (NameCode (..), formatNameCode, namePrepaidCodeHash, randomNamePrepaidCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
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
import StoreService.Config (ChainConfig (..), NamesConfig (..), ServiceConfig (..), TermsConfig (..))
import StoreService.Names.Abi (hex, uint256)
import StoreService.Names.Chain (newChainClient)
import StoreService.Names.Contracts (newContracts)
import StoreService.Names.Codes (CodeRefusal (..))
import StoreService.Names.Orders (OrdersEnv (..))
import qualified StoreService.Names.Orders as Orders
import StoreService.Names.Phases (RegistrationId (..), awaitChange, newPhases)
import StoreService.Names.Signatures (OrderFields (..), orderMessage, personalDigest)
import StoreService.Names.Store
import StoreService.Names.Turns (newTurns, withCodeTurn)
import StoreService.Names.View (ViewReads (..), readViewChain, viewJSON)
import StoreService.Orders (cancelOpenInvoice, expireOverdue, settleOrder)
import StoreService.Poller (cleanupInterval, cleanupReadsPerPass, cleanupUnpaid, drainHints, newPollerEnv, newReadHints, queueReadHint, runOnePass, unpaidRetention)
import StoreService.Providers.BTCPay (btcpayProvider)
import StoreService.Providers (Funded (..), OrderDraft (..), PaymentSignal (..), Provider (..), ProviderError (..), ProviderInvoice (..))
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceProduct (..), InvoiceRow (..), OverdueInvoice (..), ProviderRefConflict (..), getInvoice, getInvoiceByProviderRef, overdueInvoices, unpaidRefs)
import StoreService.Waiters (Waiters, newWaiters)
import StoreService.Web.Http (codeLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Codes (codeCheckHandler)
import StoreService.Web.Names.Orders (codeOrderHandler)
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
    it "reads an exact resend's unpaid invoice past its expiry as expired, and its lapsed commitment as failed, writing neither" testResendReadsTime
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
    it "answers after the name's turn, so a slow reader of one answer holds no other order for the name, paid or by code" testSlowReaderHoldsNoTurn
  describe "code orders" $ do
    it "places a code order as a queued live registration holding its code, answering only its id" testCodeOrder
    it "answers an exact resend with its id in any phase, the code spent or not, before any chain read" testCodeResend
    it "refuses a resend differing in any one field, the code included, with a bare order_exists" testCodeOneFieldDiffers
    it "refuses a bad signature, or another signer's, after the code check" testCodeBadSignature
    it "refuses a stale timestamp on a new code order" testCodeStaleTimestamp
    it "refuses a code order meeting an open invoice for the name with order_exists" testCodeMeetsInvoice
    it "closes an overdue paid order of the same wallet and name, cancelling its invoice after" testCodeClosesOverdue
    it "refuses each code that cannot pay with its tag, the state before the cover" testCodeRefusals
    it "checks the duration for the code's years" testCodeYears
    it "answers name_taken and name_check_unavailable before the code's state and cover, leaving the code unreserved" testCodeNameChecks
    it "refuses sealed over 11095 characters, a body over 16384 bytes and malformed fields, accepting the longest order" testCodeSizes
    it "runs code orders in the code's turn, one of two spending one code for two names winning, the other answered code_reserved" testConcurrentCodeOrders
    it "answers the code's refusal when its reservation fails in the insert, writing nothing" testCodeNotReservable
    it "counts code orders and code checks against one code limit" testCodeLimitShared
  describe "terms" $ do
    it "answers terms_changed with the current terms and community for an old version or the wrong variant, listed or not, creating nothing" testTermsChanged
    it "accepts the community terms for a listed name as Appendix C signs them, storing them" testCommunityTerms
    it "answers an exact resend after the version changed, and takes the same fields under the new terms as another order" testResendAfterBump
    it "checks the terms after the existing orders, the timestamp and the duration, before the chain and the price" testTermsCheckOrder
    it "answers terms_changed for a code order under old terms, listed or not, leaving the code unreserved, and accepts the community terms" testCodeTermsChanged
    it "answers a code order's exact resend after the version changed" testCodeResendAfterBump
    it "checks a code order's terms after the timestamp, before the chain and the code's state" testCodeTermsCheckOrder
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
  describe "codes only" $ do
    it "answers a name invoice 403 codes_only before reading the order, reaching no provider and writing nothing" testInvoiceCodesOnly
    it "sells a badge as before" testBadgeCodesOnly
  describe "invoice side" $ do
    it "finds an invoice by provider ref in either table, refusing a ref both hold" testProviderRefs
    it "reads and sweeps name invoices with the badge ones" testPollerQueries
    it "agrees between the sweep and the overdue close on which invoices hold money" testFundedAgreement
  describe "registration view" $ do
    it "has the contract's fields" testViewFields
    it "has the community entry of its label hash, null for an unlisted name" testViewCommunity
    it "has the terms the order signed, null for an order stored before orders signed them" testViewTerms
    it "reads the intent nonce in committed and nothing in taken" testViewReads
    it "leaves a failed intentNonce read null and named in unread, reading nothing in taken" testViewUnread

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
durationAt = yearsDuration 2

yearsDuration :: Word64 -> UTCTime -> Word64
yearsDuration years now = years * yearSeconds + fromIntegral (max 0 (toInteger (seconds termStart) - toInteger (seconds now)))

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
    oTerms :: Text,
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
      oTerms = termsVersion,
      oTimestamp = seconds orderTime
    }

-- the launch price of 11 letters for 2 years
alicePrice :: Int
alicePrice = 10000

commitmentOf :: Int -> ByteString
commitmentOf n = C.sha256Hash $ BC.pack $ "commitment " <> show n

fieldsOf :: Order -> OrderFields
fieldsOf Order {oLabel, oCommitment, oSealed, oDuration, oTerms} =
  OrderFields {ofLabelHash = labelHash oLabel, ofLength = T.length oLabel, ofDuration = oDuration, ofCommitment = oCommitment, ofSealed = oSealed, ofAgreedTerms = oTerms}

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
      ("terms", J.String ofAgreedTerms),
      ("timestamp", J.toJSON oTimestamp)
    ]
  where
    OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} = fieldsOf o

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
  NamesConfig
    { nTLD = TLDSimplex,
      nYears = 2,
      nTermStart = termStart,
      nTermSlackSeconds = slackSeconds,
      nRevealMarginSeconds = 900,
      nCodesOnly = False,
      nTerms = TermsConfig {tVersion = termsVersion, tUrl = termsUrl},
      nCommunity = M.empty,
      nChain = fakeChainConfig node
    }

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
    Warp.testWithApplicationSettings (webSettings (listener cfg)) (pure $ ordersApp cWeb cOrders) $ \port ->
      action Ctx {cStore, cNode, cOrders, cWeb, cClient = (mgr, "http://127.0.0.1:" <> show port), cStub, cClock, cRandom, cIps}

-- | The context's store, chain, providers and clock with its names settings changed, as after a restart.
configured :: (NamesConfig -> NamesConfig) -> Ctx -> Ctx
configured change ctx@Ctx {cOrders, cWeb} = ctx {cOrders = orders, cWeb = cWeb {weConfig = (weConfig cWeb) {names = Just namesCfg}, weOrders = Just orders}}
  where
    namesCfg = change (oeConfig cOrders)
    orders = cOrders {oeConfig = namesCfg}

codesOnly :: NamesConfig -> NamesConfig
codesOnly cfg = cfg {nCodesOnly = True}

-- | Another server of the orders routes over the context with its names settings changed.
withSettings :: (NamesConfig -> NamesConfig) -> Ctx -> (Ctx -> IO a) -> IO a
withSettings change ctx action = do
  let ctx'@Ctx {cOrders, cWeb} = configured change ctx
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplicationSettings (webSettings (listener $ weConfig cWeb)) (pure $ ordersApp cWeb cOrders) $ \port ->
    action ctx' {cClient = (mgr, "http://127.0.0.1:" <> show port)}

ordersApp :: WebEnv -> OrdersEnv -> Application
ordersApp web orders req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) (codeRoutes orders (weProviders web) (weWaiters web)) req respond)

-- | The routes as namesRoutes registers them, since a NamesEnv needs a running chat controller.
codeRoutes :: OrdersEnv -> [Provider] -> Waiters -> [Text] -> Maybe Route
codeRoutes orders providers waiters = \case
  ["api", "name", "register"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeOrderHandler orders providers waiters}
  ["api", "code", "check"] -> Just Route {rMethod = "POST", rLimit = codeLimit, rHandler = codeCheckHandler (oeStore orders)}
  _ -> Nothing

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
  field "terms" r `shouldBe` Just (J.String termsVersion)
  let reference = "name:" <> hex (orderHash ownerA (fieldsOf aliceOrder) (OTInvoice (SPMCrypto CCBtc) 10000))
  stubCalls (cStub ctx) `shouldReturn` [StubCreate (SPMCrypto CCBtc) OrderDraft {odAmount = CurrencyAmount 10000, odCurrency = "usd", odReference = reference}]
  reg <- invoiceRegistration ctx invId
  (nrPhase reg, nrLive reg, nrVersion reg, nrYears reg, nrPrice reg, nrPayment reg) `shouldBe` (PUnpaid, True, 1, 2, 10000, RPInvoice (SPMCrypto CCBtc) invId)
  nrOrderHash reg `shouldBe` orderHash ownerA (fieldsOf aliceOrder) (OTInvoice (SPMCrypto CCBtc) 10000)
  nrAgreedTerms reg `shouldBe` termsVersion

-- | The body of the contract's Appendix C order vector, with viem's signature over it (NamesSignatureTests).
testViemOrder :: IO ()
testViemOrder = withOrders $ \ctx0 -> withSettings vectorTermStart ctx0 $ \ctx -> do
  writeIORef (cClock ctx) (posixSecondsToUTCTime $ fromIntegral $ oTimestamp vectorOrder)
  signedBody (cRandom ctx) vectorOrder >>= (`shouldBe` withSignature vectorSignature (unsignedBody vectorOrder))
  (status, v) <- postOrder ctx $ withSignature vectorSignature (unsignedBody vectorOrder)
  status `shouldBe` 200
  (field "phase" . registrationOf =<< v) `shouldBe` Just "unpaid"

-- | Appendix C's order: alicebakery with no links, signed with the terms version.
vectorOrder :: Order
vectorOrder =
  aliceOrder
    { oCommitment = unhex "5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5",
      oSealed = "AAAAAAAAAAAAAAAACWgxqMk-UKfpra_Zh6vc4KNBR6SX1WYpGb4COoWCCbM0bisYCH14WLzghp8hqi_-KyXSi4qf_DAdP_M4bFetD0s",
      oDuration = 69292800,
      oTimestamp = 1792828800
    }

vectorSignature :: Text
vectorSignature = "0x17107b11494e25098efc348aca64a2469ffb832bcc79dc57377a1b26a4004b6701ad73648f50af8b39d4079820a3de7e681ddf15f8cf15b5cff02b4a48cf1fe61c"

-- | Appendix C's duration is the page's at 21 October 2026 00:00 UTC, before its timestamp,
-- so the term starts later by as much for the vector to pass the duration check at its timestamp.
vectorTermStart :: NamesConfig -> NamesConfig
vectorTermStart cfg = cfg {nTermStart = posixSecondsToUTCTime $ fromIntegral $ oTimestamp vectorOrder + oDuration vectorOrder - 2 * yearSeconds}

testExactResend :: IO ()
testExactResend = withOrders $ \ctx -> do
  body <- signedBody (cRandom ctx) aliceOrder
  (invId, first') <- placed ctx aliceOrder
  advance ctx 60
  resigned <- placed ctx aliceOrder {oTimestamp = seconds orderTime + 60}
  resigned `shouldBe` (invId, first')
  advance ctx 600
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

testResendReadsTime :: IO ()
testResendReadsTime = do
  withOrders $ \ctx -> do
    (invId, _) <- placed ctx aliceOrder
    advance ctx (45 * 60 + 1)
    (status, v) <- placeOrder ctx aliceOrder
    (status, field "status" =<< v, field "phase" . registrationOf =<< v) `shouldBe` (200, Just "expired", Just "unpaid")
    fmap irStatus <$> getInvoice (cStore ctx) invId `shouldReturn` Just ISOpen
  withOrders $ \ctx@Ctx {cStore, cClock} -> do
    (invId, _) <- placed ctx aliceOrder
    settleNow ctx invId settled `shouldReturn` Right ISPaid
    r <- invoiceRegistration ctx invId
    now <- readIORef cClock
    let lapse = seconds now + 600
    Written version <- withTransaction cStore $ \db ->
      updateRegistration db r {nrPhase = PCommitted, nrRevealAfter = Just (lapse - 3600), nrRevealBefore = Just lapse} now
    let resend = (\(status, v) -> (status, field "status" =<< v, (field "phase" &&& field "error") . registrationOf <$> v)) <$> placeOrder ctx aliceOrder
    resend `shouldReturn` (200, Just "paid", Just (Just "committed", Just J.Null))
    advance ctx 601
    resend `shouldReturn` (200, Just "paid", Just (Just "failed", Just "The name was not signed in time"))
    (nrPhase &&& nrVersion) <$> invoiceRegistration ctx invId `shouldReturn` (PCommitted, version)

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
          aliceOrder {oPrice = alicePrice + 1},
          aliceOrder {oTerms = termsVersion <> "/community"}
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
      KM.delete "terms" good,
      with "terms" (J.Number 20261023),
      KM.delete "kind" good,
      with "kind" "badge"
    ]
    $ \body -> postOrder ctx body `shouldReturn` refusedWith 400 "bad_request"
  forM_ malformedTerms $ \t -> placeOrder ctx aliceOrder {oTerms = t} `shouldReturn` refusedWith 400 "bad_request"
  forM_ [5, 64] $ \len -> postOrder ctx (with "length" (J.Number len)) `shouldReturn` refusedWith 400 "invalid_name"
  stubCalls (cStub ctx) `shouldReturn` []

-- | The name on the operator's community list.
listName :: Text -> NamesConfig -> NamesConfig
listName label cfg = cfg {nCommunity = M.insert lh bakers (nCommunity cfg)}
  where
    LabelHash lh = labelHash label

bakers :: Text
bakers = "bakers and bakeries in Portugal"

-- | The operator's next terms version, after a restart.
bumpedTerms :: NamesConfig -> NamesConfig
bumpedTerms cfg@NamesConfig {nTerms} = cfg {nTerms = nTerms {tVersion = newerTerms}}

newerTerms, olderTerms :: Text
newerTerms = "2026-11-01"
olderTerms = "2026-10-01"

communityTerms :: Text -> Text
communityTerms version = version <> "/community"

-- | terms_changed with the current version, the terms' URL and the name's community entry.
termsChanged :: Text -> Maybe Text -> (Int, Maybe J.Object)
termsChanged version community =
  ( 409,
    Just $
      KM.fromList
        [ ("error", "terms_changed"),
          ("terms", J.object ["version" .= version, "url" .= termsUrl]),
          ("community", maybe J.Null (\t -> J.object ["for" .= t]) community)
        ]
  )

-- | Signed as sent, so only their format refuses them.
malformedTerms :: [Text]
malformedTerms = ["", T.replicate 49 "a", "2026-10-23 community", "2026-10-23+community", "2026-10-23/communit\233"]

testTwoWallets :: IO ()
testTwoWallets = withOrders $ \ctx -> do
  (a, _) <- placed ctx aliceOrder
  (b, _) <- placed ctx aliceOrder {oOwner = ownerBText, oKey = keyB, oCommitment = commitmentOf 2}
  a `shouldNotBe` b
  mapM (fmap nrLive . invoiceRegistration ctx) [a, b] `shouldReturn` [True, True]

testTermsChanged :: IO ()
testTermsChanged = withOrders $ \ctx0 -> withSettings (listName "dynamis") ctx0 $ \ctx -> do
  let dynamis = aliceOrder {oLabel = "dynamis", oCommitment = commitmentOf 2, oPrice = 20000}
  -- 48 characters pass the format and meet the terms check
  forM_ [olderTerms, communityTerms termsVersion, communityTerms olderTerms, T.replicate 48 "a"] $ \t ->
    placeOrder ctx aliceOrder {oTerms = t} `shouldReturn` termsChanged termsVersion Nothing
  forM_ [termsVersion, olderTerms, communityTerms olderTerms] $ \t ->
    placeOrder ctx dynamis {oTerms = t} `shouldReturn` termsChanged termsVersion (Just bakers)
  stubCalls (cStub ctx) `shouldReturn` []
  registrationsOf ctx ownerA `shouldReturn` []
  (invId, v) <- placed ctx dynamis {oTerms = communityTerms termsVersion}
  field "terms" (registrationOf v) `shouldBe` Just (J.String $ communityTerms termsVersion)
  nrAgreedTerms <$> invoiceRegistration ctx invId `shouldReturn` communityTerms termsVersion

testCommunityTerms :: IO ()
testCommunityTerms = withOrders $ \ctx0 -> withSettings (listName "alicebakery" . vectorTermStart) ctx0 $ \ctx -> do
  writeIORef (cClock ctx) (posixSecondsToUTCTime $ fromIntegral $ oTimestamp vectorOrder)
  postOrder ctx (withSignature vectorSignature $ unsignedBody vectorOrder) `shouldReturn` termsChanged termsVersion (Just bakers)
  let community = vectorOrder {oTerms = communityTerms termsVersion}
      communitySignature = "0x03eccb591b79ab4e3e6f22ccbeb77a6ac32c6dad8cb318557836258c390bc31d78852a3279c6c8cc2ef8d1025641d45ba4101f54dfd84c264deb2ab391c1cbc61c"
  signedBody (cRandom ctx) community >>= (`shouldBe` withSignature communitySignature (unsignedBody community))
  (status, v) <- postOrder ctx $ withSignature communitySignature (unsignedBody community)
  (status, field "terms" . registrationOf =<< v, field "community" . registrationOf =<< v)
    `shouldBe` (200, Just (J.String $ communityTerms termsVersion), Just (J.object ["for" .= bakers]))

testResendAfterBump :: IO ()
testResendAfterBump = withOrders $ \ctx0 -> do
  body <- signedBody (cRandom ctx0) aliceOrder
  (invId@(InvoiceId invText), view) <- placed ctx0 aliceOrder
  withSettings bumpedTerms ctx0 $ \ctx -> do
    advance ctx 60
    let later = aliceOrder {oTimestamp = seconds orderTime + 60}
    postOrder ctx body `shouldReturn` (200, Just view)
    placed ctx later `shouldReturn` (invId, view)
    placeOrder ctx later {oTerms = newerTerms} `shouldReturn` refusedWith 409 "order_exists"
    fst <$> post ctx ("/api/invoice/" <> T.unpack invText <> "/cancel") "" `shouldReturn` 200
    (newer, _) <- placed ctx later {oTerms = newerTerms}
    newer `shouldNotBe` invId
    [n, o] <- mapM (invoiceRegistration ctx) [newer, invId]
    (nrAgreedTerms n, nrAgreedTerms o) `shouldBe` (newerTerms, termsVersion)
    nrOrderHash n `shouldNotBe` nrOrderHash o
    (status, v) <- postOrder ctx body
    (status, field "invoiceId" =<< v, field "status" =<< v) `shouldBe` (200, Just (J.String invText), Just "expired")

testTermsCheckOrder :: IO ()
testTermsCheckOrder = withOrders $ \ctx@Ctx {cNode = node} -> do
  let old = aliceOrder {oTerms = olderTerms}
  void $ placed ctx aliceOrder {oOwner = ownerBText, oKey = keyB, oCommitment = commitmentOf 5}
  placeOrder ctx old {oOwner = ownerBText, oKey = keyB, oCommitment = commitmentOf 6} `shouldReturn` refusedWith 409 "order_exists"
  placeOrder ctx old {oTimestamp = seconds orderTime + 601} `shouldReturn` refusedWith 400 "bad_request"
  placeOrder ctx old {oDuration = oDuration old + fromIntegral slackSeconds + 1} `shouldReturn` refusedWith 400 "bad_request"
  placeOrder ctx old {oPrice = 9000} `shouldReturn` termsChanged termsVersion Nothing
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (1 :: Int)
  placeOrder ctx old `shouldReturn` termsChanged termsVersion Nothing
  setFault node "eth_call" HttpFailure
  placeOrder ctx old `shouldReturn` termsChanged termsVersion Nothing
  length <$> stubCalls (cStub ctx) `shouldReturn` 1

-- code orders

-- | A prepaid code as the operator issues it, with its text as the page sends it.
issueCode :: Ctx -> Int -> Int -> (NewPrepaidCode -> NewPrepaidCode) -> IO (Text, PrepaidCode)
issueCode Ctx {cStore, cRandom} len years change = do
  code <- randomNamePrepaidCode cRandom len
  let new = change NewPrepaidCode {npcCodeHash = namePrepaidCodeHash code, npcMinLength = len, npcYears = years, npcPaymentStatus = CPSPaid, npcExpiresAt = Nothing}
  stored <- withTransaction cStore $ \db ->
    insertPrepaidCode db new orderTime >>= \case
      Right codeId -> fromJust <$> getPrepaidCodeById db codeId
      Left e -> error $ "code not issued: " <> show e
  pure (formatNameCode $ NCPrepaid code, stored)

unissuedCode :: Ctx -> Int -> IO Text
unissuedCode Ctx {cRandom} len = formatNameCode . NCPrepaid <$> randomNamePrepaidCode cRandom len

readCode :: Ctx -> PrepaidCode -> IO PrepaidCode
readCode Ctx {cStore} PrepaidCode {pcId} = withTransaction cStore $ \db -> fromJust <$> getPrepaidCodeById db pcId

-- | The body of /api/name/register: the order's fields and the code, without a kind, method or price.
signedCodeBody :: Ctx -> Text -> Order -> IO J.Object
signedCodeBody ctx code o = KM.insert "code" (J.String code) . KM.delete "kind" . KM.delete "method" . KM.delete "price" <$> signedBody (cRandom ctx) o

postRegister :: Ctx -> J.Object -> IO (Int, Maybe J.Object)
postRegister ctx = post ctx "/api/name/register" . J.encode

register :: Ctx -> Text -> Order -> IO (Int, Maybe J.Object)
register ctx code o = signedCodeBody ctx code o >>= postRegister ctx

-- | A code order's registration id, failing the test unless the order was accepted with that alone.
registered :: Ctx -> Text -> Order -> IO RegistrationId
registered ctx code o =
  register ctx code o >>= \case
    (200, Just v) | [("registrationId", J.String rid)] <- KM.toList v -> pure $ RegistrationId rid
    other -> error $ "code order refused: " <> show other

registrationAnswer :: RegistrationId -> (Int, Maybe J.Object)
registrationAnswer (RegistrationId rid) = (200, Just $ KM.fromList [("registrationId", J.String rid)])

registrationById :: Ctx -> RegistrationId -> IO NameRegistration
registrationById Ctx {cStore} rid = withTransaction cStore $ \db -> fromJust <$> getRegistration db rid

testCodeOrder :: IO ()
testCodeOrder = withOrders $ \ctx -> do
  (code, issued@PrepaidCode {pcId, pcCodeHash}) <- issueCode ctx 8 2 id
  rid <- registered ctx code aliceOrder
  r <- registrationById ctx rid
  (nrPhase r, nrLive r, nrVersion r, nrYears r, nrPrice r, nrPayment r) `shouldBe` (PQueued, True, 1, 2, 0, RPCode pcId)
  (nrOwner r, nrDuration r, nrCommitment r, nrSealed r, nrAgreedTerms r) `shouldBe` (ownerA, durationAt orderTime, oCommitment aliceOrder, oSealed aliceOrder, termsVersion)
  nrOrderHash r `shouldBe` orderHash ownerA (fieldsOf aliceOrder) (OTCode pcCodeHash)
  held <- readCode ctx issued
  (pcReservedBy held, pcReservedAt held, pcSpentAt held) `shouldBe` (Just rid, Just orderTime, Nothing)
  stubCalls (cStub ctx) `shouldReturn` []

testCodeResend :: IO ()
testCodeResend = withOrders $ \ctx -> do
  (code, PrepaidCode {pcId}) <- issueCode ctx 8 2 id
  body <- signedCodeBody ctx code aliceOrder
  rid <- registered ctx code aliceOrder
  advance ctx 60
  register ctx code aliceOrder {oTimestamp = seconds orderTime + 60} `shouldReturn` registrationAnswer rid
  r <- registrationById ctx rid
  now <- readIORef (cClock ctx)
  withTransaction (cStore ctx) $ \db -> do
    updateRegistration db r {nrPhase = PRegistered} now `shouldReturn` Written 2
    spendPrepaidCode db pcId rid now `shouldReturn` True
  advance ctx 3600
  setFault (cNode ctx) "eth_call" HttpFailure
  postRegister ctx body `shouldReturn` registrationAnswer rid
  register ctx code aliceOrder {oTimestamp = seconds orderTime + 3660} `shouldReturn` registrationAnswer rid
  length <$> registrationsOf ctx ownerA `shouldReturn` 1

testCodeTermsChanged :: IO ()
testCodeTermsChanged = withOrders $ \ctx0 -> withSettings (listName "dynamis") ctx0 $ \ctx -> do
  (code, issued) <- issueCode ctx 7 2 id
  let dynamis = aliceOrder {oLabel = "dynamis", oCommitment = commitmentOf 2}
  forM_ [olderTerms, communityTerms termsVersion] $ \t ->
    register ctx code aliceOrder {oTerms = t} `shouldReturn` termsChanged termsVersion Nothing
  forM_ [termsVersion, communityTerms olderTerms] $ \t ->
    register ctx code dynamis {oTerms = t} `shouldReturn` termsChanged termsVersion (Just bakers)
  pcReservedBy <$> readCode ctx issued `shouldReturn` Nothing
  rid <- registered ctx code dynamis {oTerms = communityTerms termsVersion}
  nrAgreedTerms <$> registrationById ctx rid `shouldReturn` communityTerms termsVersion
  pcReservedBy <$> readCode ctx issued `shouldReturn` Just rid

testCodeResendAfterBump :: IO ()
testCodeResendAfterBump = withOrders $ \ctx0 -> do
  (code, _) <- issueCode ctx0 8 2 id
  rid <- registered ctx0 code aliceOrder
  withSettings bumpedTerms ctx0 $ \ctx -> do
    advance ctx 60
    register ctx code aliceOrder `shouldReturn` registrationAnswer rid
    register ctx code aliceOrder {oTimestamp = seconds orderTime + 60} `shouldReturn` registrationAnswer rid
    register ctx code aliceOrder {oTerms = newerTerms, oTimestamp = seconds orderTime + 60} `shouldReturn` refusedWith 409 "order_exists"

testCodeTermsCheckOrder :: IO ()
testCodeTermsCheckOrder = withOrders $ \ctx@Ctx {cStore, cNode = node} -> do
  let old = aliceOrder {oTerms = olderTerms}
  (code, _) <- issueCode ctx 8 2 id
  register ctx code old {oTimestamp = seconds orderTime + 601} `shouldReturn` refusedWith 400 "bad_request"
  register ctx code old {oDuration = oDuration old + fromIntegral slackSeconds + 1} `shouldReturn` refusedWith 400 "bad_request"
  (reserved, reservedCode) <- issueCode ctx 8 2 id
  placeCode cStore "holder" ownerB (order "holderone" 1) reservedCode `shouldReturn` Right ()
  (long, _) <- issueCode ctx 12 2 id
  (unpaid, _) <- issueCode ctx 8 2 $ \c -> c {npcPaymentStatus = CPSUnpaid}
  forM_ [reserved, long, unpaid] $ \c -> register ctx c old `shouldReturn` termsChanged termsVersion Nothing
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (1 :: Int)
  register ctx code old `shouldReturn` termsChanged termsVersion Nothing
  setFault node "eth_call" HttpFailure
  register ctx code old `shouldReturn` termsChanged termsVersion Nothing
  length <$> registrationsOf ctx ownerA `shouldReturn` 0

testCodeOneFieldDiffers :: IO ()
testCodeOneFieldDiffers = withOrders $ \ctx -> do
  (code, _) <- issueCode ctx 7 2 id
  (other, otherCode) <- issueCode ctx 8 2 id
  void $ registered ctx code aliceOrder
  let variants =
        [ (code, aliceOrder {oLabel = "dynamis"}),
          (code, aliceOrder {oCommitment = commitmentOf 2}),
          (code, aliceOrder {oSealed = "sealed-alicebakery-2"}),
          (code, aliceOrder {oDuration = oDuration aliceOrder + 1}),
          (code, aliceOrder {oTerms = termsVersion <> "/community"}),
          (other, aliceOrder)
        ]
  forM_ variants $ \(c, o) -> register ctx c o `shouldReturn` refusedWith 409 "order_exists"
  -- the same fields as a paid order are another order
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 409 "order_exists"
  pcReservedBy <$> readCode ctx otherCode `shouldReturn` Nothing
  length <$> registrationsOf ctx ownerA `shouldReturn` 1

testCodeBadSignature :: IO ()
testCodeBadSignature = withOrders $ \ctx -> do
  (code, issued) <- issueCode ctx 8 2 id
  register ctx code aliceOrder {oKey = keyB} `shouldReturn` refusedWith 400 "bad_request"
  good <- signedCodeBody ctx code aliceOrder
  let highS = "0x" <> T.replicate 32 "11" <> "ff" <> T.replicate 31 "11" <> "1b"
  forM_ [highS, ""] $ \bad -> postRegister ctx (withSignature bad good) `shouldReturn` refusedWith 400 "bad_request"
  postRegister ctx (KM.delete "signature" good) `shouldReturn` refusedWith 400 "bad_request"
  unknown <- unissuedCode ctx 8
  register ctx unknown aliceOrder {oKey = keyB} `shouldReturn` refusedWith 400 "code_invalid"
  pcReservedBy <$> readCode ctx issued `shouldReturn` Nothing
  length <$> registrationsOf ctx ownerA `shouldReturn` 0

testCodeStaleTimestamp :: IO ()
testCodeStaleTimestamp = withOrders $ \ctx -> do
  (code, _) <- issueCode ctx 8 2 id
  let at offset = aliceOrder {oTimestamp = fromIntegral (toInteger (seconds orderTime) + offset)}
  forM_ [-601, 601] $ \offset -> register ctx code (at offset) `shouldReturn` refusedWith 400 "bad_request"
  register ctx code aliceOrder {oTimestamp = maxBound} `shouldReturn` refusedWith 400 "bad_request"
  void $ registered ctx code (at (-600))

testCodeMeetsInvoice :: IO ()
testCodeMeetsInvoice = withOrders $ \ctx -> do
  (code, issued) <- issueCode ctx 8 2 id
  void $ placed ctx aliceOrder
  register ctx code aliceOrder `shouldReturn` refusedWith 409 "order_exists"
  register ctx code aliceOrder {oCommitment = commitmentOf 2} `shouldReturn` refusedWith 409 "order_exists"
  -- another wallet sending the open order's commitment
  register ctx code aliceOrder {oOwner = ownerBText, oKey = keyB} `shouldReturn` refusedWith 409 "order_exists"
  pcReservedBy <$> readCode ctx issued `shouldReturn` Nothing
  length <$> registrationsOf ctx ownerA `shouldReturn` 1
  length <$> registrationsOf ctx ownerB `shouldReturn` 0

testCodeClosesOverdue :: IO ()
testCodeClosesOverdue = withOrders $ \ctx -> do
  (code, _) <- issueCode ctx 8 2 id
  (short, _) <- issueCode ctx 12 2 id
  (oldInv, _) <- placed ctx aliceOrder {oMethod = "card"}
  -- past the card invoice's 60 minutes, before any sweep
  advance ctx (61 * 60)
  now <- readIORef (cClock ctx)
  let next = aliceOrder {oCommitment = commitmentOf 2, oTimestamp = seconds now, oDuration = durationAt now}
  register ctx short next `shouldReturn` refusedWith 409 "code_not_covering"
  nrLive <$> invoiceRegistration ctx oldInv `shouldReturn` True
  rid <- registered ctx code next
  Just oldRow <- getInvoice (cStore ctx) oldInv
  irStatus oldRow `shouldBe` ISExpired
  old <- invoiceRegistration ctx oldInv
  (nrPhase old, nrLive old, nrVersion old) `shouldBe` (PUnpaid, False, 2)
  (nrPhase &&& nrLive) <$> registrationById ctx rid `shouldReturn` (PQueued, True)
  calls <- stubCalls (cStub ctx)
  [c | c@StubCancel {} <- calls] `shouldBe` [StubCancel (irProviderRef oldRow)]

testCodeRefusals :: IO ()
testCodeRefusals = withOrders $ \ctx@Ctx {cStore} -> do
  unknown <- unissuedCode ctx 8
  let typo = T.init unknown <> (if T.last unknown == '0' then "1" else "0")
      ownerCode = "SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5"
  forM_ [unknown, typo, ownerCode, "not a code"] $ \c -> register ctx c aliceOrder `shouldReturn` refusedWith 400 "code_invalid"
  (revoked, revokedCode) <- issueCode ctx 8 2 id
  withTransaction cStore $ \db -> void $ revokePrepaidCode db (pcCodeHash revokedCode) orderTime
  register ctx revoked aliceOrder `shouldReturn` refusedWith 400 "code_invalid"
  (reserved, reservedCode) <- issueCode ctx 8 2 id
  placeCode cStore "holder" ownerB (order "holderone" 1) reservedCode `shouldReturn` Right ()
  register ctx reserved aliceOrder `shouldReturn` refusedWith 409 "code_reserved"
  (used, usedCode) <- issueCode ctx 8 2 id
  placeCode cStore "spender" ownerB (order "spendertwo" 2) usedCode `shouldReturn` Right ()
  withTransaction cStore (\db -> spendPrepaidCode db (pcId usedCode) (RegistrationId "spender") t0) `shouldReturn` True
  register ctx used aliceOrder `shouldReturn` refusedWith 409 "code_used"
  (expired, _) <- issueCode ctx 8 2 $ \c -> c {npcExpiresAt = Just orderTime}
  register ctx expired aliceOrder `shouldReturn` refusedWith 409 "code_expired"
  (unpaid, _) <- issueCode ctx 8 2 $ \c -> c {npcPaymentStatus = CPSUnpaid}
  register ctx unpaid aliceOrder `shouldReturn` refusedWith 409 "payment_pending"
  (long, _) <- issueCode ctx 12 2 id
  register ctx long aliceOrder `shouldReturn` refusedWith 409 "code_not_covering"
  (longUnpaid, _) <- issueCode ctx 12 2 $ \c -> c {npcPaymentStatus = CPSUnpaid}
  register ctx longUnpaid aliceOrder `shouldReturn` refusedWith 409 "payment_pending"
  length <$> registrationsOf ctx ownerA `shouldReturn` 0

testCodeYears :: IO ()
testCodeYears = withOrders $ \ctx -> do
  (code, _) <- issueCode ctx 8 5 id
  register ctx code aliceOrder `shouldReturn` refusedWith 400 "bad_request"
  rid <- registered ctx code aliceOrder {oDuration = yearsDuration 5 orderTime}
  (nrYears &&& nrDuration) <$> registrationById ctx rid `shouldReturn` (5, yearsDuration 5 orderTime)

testCodeNameChecks :: IO ()
testCodeNameChecks = withOrders $ \ctx@Ctx {cNode = node} -> do
  (code, issued) <- issueCode ctx 8 2 id
  (long, _) <- issueCode ctx 12 2 id
  let unreserved = pcReservedBy <$> readCode ctx issued `shouldReturn` Nothing
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (1 :: Int)
  register ctx code aliceOrder `shouldReturn` refusedWith 409 "name_taken"
  register ctx long aliceOrder `shouldReturn` refusedWith 409 "name_taken"
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
  answerRegistered node (cResolver $ fakeChainConfig node)
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (seconds orderTime + yearSeconds)
  register ctx code aliceOrder `shouldReturn` refusedWith 409 "name_taken"
  register ctx long aliceOrder `shouldReturn` refusedWith 409 "name_taken"
  unreserved
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  setFault node "eth_call" HttpFailure
  register ctx code aliceOrder `shouldReturn` refusedWith 503 "name_check_unavailable"
  register ctx long aliceOrder `shouldReturn` refusedWith 503 "name_check_unavailable"
  unreserved
  length <$> registrationsOf ctx ownerA `shouldReturn` 0
  clearFault node "eth_call"
  void $ registered ctx code aliceOrder

testCodeSizes :: IO ()
testCodeSizes = withOrders $ \ctx -> do
  (code, _) <- issueCode ctx 8 2 id
  register ctx code aliceOrder {oSealed = T.replicate 11096 "a"} `shouldReturn` refusedWith 400 "bad_request"
  padded <- KM.insert "padding" (J.String $ T.replicate 16384 "a") <$> signedCodeBody ctx code aliceOrder
  postRegister ctx padded `shouldReturn` refusedWith 400 "bad_request"
  good <- signedCodeBody ctx code aliceOrder
  let with k v = KM.insert k v good
  forM_
    [ KM.delete "sealed" good,
      KM.delete "code" good,
      with "code" (J.Number 8),
      with "labelHash" (J.String $ T.toUpper $ hex $ (\(LabelHash h) -> h) alice),
      with "owner" (J.String $ T.toLower ownerAText),
      with "timestamp" "1792574100",
      KM.delete "terms" good
    ]
    $ \body -> postRegister ctx body `shouldReturn` refusedWith 400 "bad_request"
  forM_ malformedTerms $ \t -> register ctx code aliceOrder {oTerms = t} `shouldReturn` refusedWith 400 "bad_request"
  forM_ ["[]", "{"] $ \body -> post ctx "/api/name/register" body `shouldReturn` (400, errorOnly "bad_request")
  -- the formats and the length come before the code
  forM_ [5, 64] $ \len -> postRegister ctx (KM.insert "code" "not a code" $ with "length" (J.Number len)) `shouldReturn` refusedWith 400 "invalid_name"
  longest <- signedCodeBody ctx code aliceOrder {oSealed = T.replicate 11095 "a"}
  LB.length (J.encode longest) `shouldSatisfy` (< 16384)
  fst <$> postRegister ctx longest `shouldReturn` 200

-- | Both orders wait while the test holds the code's turn, so neither checks or reserves the code outside it.
testConcurrentCodeOrders :: IO ()
testConcurrentCodeOrders = withOrders $ \ctx -> do
  (code, issued@PrepaidCode {pcCodeHash}) <- issueCode ctx 7 2 id
  entered <- newEmptyMVar
  release <- newEmptyMVar
  holder <- async $ withCodeTurn (oeTurns $ cOrders ctx) pcCodeHash $ putMVar entered () >> takeMVar release
  takeMVar entered
  orders <- async $ concurrently (register ctx code aliceOrder) (register ctx code aliceOrder {oLabel = "dynamis", oCommitment = commitmentOf 2})
  timeout 300000 (wait orders) >>= (`shouldSatisfy` isNothing)
  pcReservedBy <$> readCode ctx issued `shouldReturn` Nothing
  putMVar release ()
  wait holder
  (a, b) <- wait orders
  sort [fst a, fst b] `shouldBe` [200, 409]
  filter ((== 409) . fst) [a, b] `shouldBe` [refusedWith 409 "code_reserved"]
  [(_, winner)] <- pure $ filter ((== 200) . fst) [a, b]
  pcReservedBy <$> readCode ctx issued `shouldReturn` Just (RegistrationId $ textOf "registrationId" winner)
  length <$> registrationsOf ctx ownerA `shouldReturn` 1

-- | A writer outside this process, which takes no turn here, reserves or revokes the code between the check and the insert.
testCodeNotReservable :: IO ()
testCodeNotReservable = withOrders $ \ctx@Ctx {cStore} -> do
  (_, reservedLate) <- issueCode ctx 8 2 id
  placeCode cStore "outsider" ownerB (order "holderone" 1) reservedLate `shouldReturn` Right ()
  (_, revokedLate) <- issueCode ctx 8 2 id
  withTransaction cStore $ \db -> void $ revokePrepaidCode db (pcCodeHash revokedLate) orderTime
  req <- fromJust . Orders.parseOrderRequest <$> signedBody (cRandom ctx) aliceOrder
  let new = NewRegistration {nrgId = RegistrationId "late", nrgOwner = ownerA, nrgOrder = fieldsOf aliceOrder, nrgYears = 2}
  forM_ [(reservedLate, CRReserved), (revokedLate, CRInvalid)] $ \(stale, refusal) -> do
    placedOrder <- Orders.placeOrder (cOrders ctx) req (Orders.codeOrderInsert new stale)
    either Just (const Nothing) placedOrder `shouldBe` Just (Orders.ORCode refusal)
  length <$> registrationsOf ctx ownerA `shouldReturn` 0

-- | One client's code checks and code orders share its 10 a minute; another client is not limited by them.
testCodeLimitShared :: IO ()
testCodeLimitShared = withOrders $ \ctx@Ctx {cClient} -> do
  (code, _) <- issueCode ctx 8 2 id
  body <- J.encode <$> signedCodeBody ctx code aliceOrder
  let from path b = statusOf <$> webRequestBody cClient "POST" path [(hContentType, "application/json"), ("X-Forwarded-For", "203.0.113.7")] b
      codeCheck = from "/api/code/check" (J.encode $ J.object ["code" .= code])
      codeOrder = from "/api/name/register" body
  sequence (concat $ replicate 5 [codeCheck, codeOrder]) `shouldReturn` replicate 10 200
  codeCheck `shouldReturn` 429
  codeOrder `shouldReturn` 429
  fst <$> register ctx code aliceOrder `shouldReturn` 200

-- | A response held before it is written, as for a client that reads it slowly, must not hold the name's turn.
testSlowReaderHoldsNoTurn :: IO ()
testSlowReaderHoldsNoTurn = withOrders $ \ctx@Ctx {cWeb, cOrders} -> do
  (codeA, _) <- issueCode ctx 7 2 id
  (codeB, _) <- issueCode ctx 7 2 id
  writing <- newEmptyMVar
  release <- newEmptyMVar
  let app = ordersApp cWeb cOrders
      slow req respond
        | isJust (lookup slowReader $ requestHeaders req) = app req $ \r -> putMVar writing () >> takeMVar release >> respond r
        | otherwise = app req respond
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure slow) $ \port -> do
    let c = ctx {cClient = (mgr, "http://127.0.0.1:" <> show port)}
        slowPost path body = statusOf <$> webRequestBody (cClient c) "POST" path [(hContentType, "application/json"), (slowReader, "1")] (J.encode body)
        whileHeld first second = do
          held <- async first
          takeMVar writing
          timeout 2000000 second >>= (`shouldSatisfy` maybe False ((== 200) . fst))
          putMVar release ()
          wait held `shouldReturn` 200
        byB o = o {oOwner = ownerBText, oKey = keyB, oCommitment = commitmentOf 2}
    whileHeld (signedBody (cRandom c) aliceOrder >>= slowPost "/api/invoice") (placeOrder c $ byB aliceOrder)
    let dynamis = aliceOrder {oLabel = "dynamis", oCommitment = commitmentOf 3}
    whileHeld (signedCodeBody c codeA dynamis >>= slowPost "/api/name/register") (register c codeB (byB dynamis) {oCommitment = commitmentOf 4})
  where
    slowReader = "x-slow-reader"

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
  setSignals (cStub ctx) [(ref, SigClosed paid PaidInPart)]
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
  capturingStderr (settleNow ctx invId (SigClosed paid PaidInPart) `shouldReturn` Right ISExpired) >>= (`shouldNotSatisfy` T.isInfixOf (alertOf invId))
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

testInvoiceCodesOnly :: IO ()
testInvoiceCodesOnly = withOrders $ \open -> withSettings codesOnly open $ \ctx -> do
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 403 "codes_only"
  postOrder ctx (KM.fromList [("kind", "name")]) `shouldReturn` refusedWith 403 "codes_only"
  stubCalls (cStub ctx) `shouldReturn` []
  registrationsOf ctx ownerA `shouldReturn` []
  -- nothing the refusals left meets the same order placed with codes_only off
  void $ placed open aliceOrder

testBadgeCodesOnly :: IO ()
testBadgeCodesOnly = withOrders $ \open -> withSettings codesOnly open $ \ctx@Ctx {cStore} -> do
  seedCheckoutCatalog cStore
  (status, view) <- post ctx "/api/invoice" (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
  (status, field "badgeType" =<< view) `shouldBe` (200, Just "supporter")
  stubCalls (cStub ctx) >>= \case
    [StubCreate method OrderDraft {odReference}] -> (method, T.isPrefixOf "badge:" odReference) `shouldBe` (btc, True)
    other -> expectationFailure ("provider calls: " <> show other)

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
  settleNow ctx closedByProvider (SigClosed (rcv 0 Nothing) PaidInPart) `shouldReturn` Right ISExpired
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
    setSignals (cStub ctx) [(poisonedRef, SigClosed (rcv 0 Nothing) PaidInPart)]
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
  setSignals (cStub ctx) [(ref "late", SigClosed (rcv 5000 (Just "0.00078")) PaidInPart)]
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
  let v = objectOf $ viewJSON (testNamesConfig node) r ViewReads {vrIntentNonce = Nothing, vrUnread = []}
  sort (KM.keys v)
    `shouldBe` sort
      [ "id", "version", "moves", "linksRefused", "phase", "labelHash", "label", "owner", "commitment", "sealed", "resolver", "sponsor", "chainId", "tld",
        "years", "duration", "price", "commitTx", "registerTx", "revealAfter", "revealBefore", "intentNonce", "unread", "expiresAt", "error", "community", "terms"
      ]
  map (`field` v) ["id", "phase", "owner", "tld", "chainId", "linksRefused", "label", "unread", "sponsor", "community", "terms"]
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
        J.String $ safeDecodeUtf8 $ strEncode fakeSponsor,
        J.Null,
        J.String termsVersion
      ]

testViewCommunity :: IO ()
testViewCommunity = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  Right () <- placePaid st "r1" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  r <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "r1")
  let LabelHash aliceHash = labelHash "alicebakery"
      listing :: [(ByteString, Text)] -> Maybe J.Value
      listing entries = field "community" $ objectOf $ viewJSON (testNamesConfig node) {nCommunity = M.fromList entries} r ViewReads {vrIntentNonce = Nothing, vrUnread = []}
  listing [(aliceHash, "bakers and bakeries in Lisbon")] `shouldBe` Just (J.object ["for" .= ("bakers and bakeries in Lisbon" :: Text)])
  let LabelHash bobHash = labelHash "bobsbakery"
  listing [(bobHash, "bakers of Porto")] `shouldBe` Just J.Null

testViewTerms :: IO ()
testViewTerms = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  Right () <- placePaid st "r1" ownerA (order "alicebakery" 1) {ofAgreedTerms = "2026-10-23/community"} (addUTCTime 3600 t0)
  r <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "r1")
  let terms :: NameRegistration -> Maybe J.Value
      terms r' = field "terms" $ objectOf $ viewJSON (testNamesConfig node) r' ViewReads {vrIntentNonce = Nothing, vrUnread = []}
  terms r `shouldBe` Just "2026-10-23/community"
  terms r {nrAgreedTerms = ""} `shouldBe` Just J.Null

testViewReads :: IO ()
testViewReads = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  g <- C.newRandom
  ct <- newContracts TLDSimplex =<< newChainClient g (fakeChainConfig node)
  answerCall node fakeSponsor noncesCall $ Returns $ uint256 (4 :: Int)
  Right () <- placePaid st "paid" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  let inPhase phase = do
        r <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "paid")
        Written _ <- withTransaction st $ \db -> updateRegistration db r {nrPhase = phase} t0
        withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "paid")
  committed <- inPhase PCommitted
  readViewChain ct committed `shouldReturn` ViewReads (Just 4) []
  taken <- inPhase PTaken
  readViewChain ct taken `shouldReturn` ViewReads Nothing []

testViewUnread :: IO ()
testViewUnread = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  g <- C.newRandom
  ct <- newContracts TLDSimplex =<< newChainClient g (fakeChainConfig node)
  setFault node "eth_call" HttpFailure
  Right () <- placePaid st "paid" ownerA (order "alicebakery" 1) (addUTCTime 3600 t0)
  r <- withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId "paid")
  readViewChain ct r {nrPhase = PCommitted} `shouldReturn` ViewReads Nothing ["intentNonce"]
  readViewChain ct r {nrPhase = PTaken} `shouldReturn` ViewReads Nothing []
  let v = objectOf $ viewJSON (testNamesConfig node) r {nrPhase = PCommitted} (ViewReads Nothing ["intentNonce"])
  (field "intentNonce" v, field "unread" v) `shouldBe` (Just J.Null, Just (J.toJSON ["intentNonce" :: Text]))
