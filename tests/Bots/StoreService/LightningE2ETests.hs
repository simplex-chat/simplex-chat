{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.LightningE2ETests (storeLightningE2ETests) where

import Bots.StoreService.NamesOrdersTests (Ctx (..), Order (..), aliceOrder, invoiceRegistration, placed, post, registrationOf, withOrdersVia)
import Bots.StoreService.WebTests (CodeRow (..), capturingStderr, codeHashText, createBody, fieldOf, loggedError, sampleCode, seedCheckoutCatalog, settledCode, stringField, supporterPriceId, testBTCPayConfig, viewInvoice)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as E
import Control.Monad (unless, (<=<))
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (Request (..), RequestBody (..), httpLbs, parseRequest, responseBody, responseTimeoutMicro)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentMethod (..))
import StoreService.Config (BTCPayConfig (..), ListenerConfig (..), ServiceConfig (..))
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), RegistrationPayment (..))
import StoreService.Poller (PollerEnv, drainHints, newPollerEnv, runOnePass)
import StoreService.Providers (Provider (..))
import StoreService.Providers.BTCPay (btcMethodId, btcpayProvider, lnMethodId, xmrMethodId)
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceRow (..), getInvoice)
import StoreService.Web.Server (WebEnv (..), webApp, webSettings)
import System.Environment (getEnvironment)
import System.IO (stderr)
import System.Timeout (timeout)
import Test.Hspec
import Text.Read (readMaybe)

storeLightningE2ETests :: Spec
storeLightningE2ETests = describe "StoreService Lightning e2e" $ do
  stack_ <- runIO lightningStack
  let onStack test = maybe (pendingWith "the regtest stack of scripts/store-service/dev/lightning.sh is not in the environment") test stack_
  it "logs BTC-LN among the methods the store has enabled, at startup" $ onStack testStartupLog
  it "issues the code of a badge order paid at the Lightning node, read on BTCPay's webhook" $ onStack testBadgePaidByWebhook
  it "issues the code of a badge order paid at the Lightning node, read by the poller's passes alone" $ onStack testBadgePaidByPass
  it "keeps a cancelled badge order unpaid when its BOLT11 is paid, reporting the payment and asking for a refund" $ onStack testCancelledBadgePaid
  it "queues a name order paid at the Lightning node" $ onStack testNamePaid

data LightningStack = LightningStack
  { lsBTCPay :: BTCPayConfig,
    lsPayerUrl :: String,
    lsServicePort :: Int
  }

envPrefix :: String
envPrefix = "STORE_LIGHTNING_"

lightningStack :: IO (Maybe LightningStack)
lightningStack = stackOf <$> getEnvironment
  where
    stackOf env = do
      bHost <- text "BTCPAY_URL"
      bApiKey <- text "BTCPAY_API_KEY"
      bStoreId <- text "BTCPAY_STORE_ID"
      bWebhookSecret <- text "BTCPAY_WEBHOOK_SECRET"
      lsPayerUrl <- setting "PAYER_URL"
      lsServicePort <- readMaybe =<< setting "SERVICE_PORT"
      pure LightningStack {lsBTCPay = testBTCPayConfig {bHost, bApiKey, bStoreId, bWebhookSecret}, lsPayerUrl, lsServicePort}
      where
        setting name = lookup (envPrefix <> name) env
        text = fmap T.pack . setting

microsPerSecond :: Int
microsPerSecond = 1000000

settleSeconds :: Int
settleSeconds = 60

settleMicros :: Int
settleMicros = settleSeconds * microsPerSecond

readIntervalMicros :: Int
readIntervalMicros = 200000

paymentSeconds :: Int
paymentSeconds = 60

paymentReplySeconds :: Int
paymentReplySeconds = 30

paymentMicros :: Int
paymentMicros = (paymentSeconds + paymentReplySeconds) * microsPerSecond

feeLimitSat :: Text
feeLimitSat = "1000"

bolt11Prefix :: Text
bolt11Prefix = "lnbcrt"

withService :: LightningStack -> (Ctx -> PollerEnv -> IO a) -> IO a
withService LightningStack {lsBTCPay, lsServicePort} action = do
  provider <- btcpayProvider lsBTCPay
  withOrdersVia (\p -> if pProvider p == PPCrypto then provider else p) $ \ctx@Ctx {cStore, cOrders, cWeb, cClient = (mgr, _)} -> do
    seedCheckoutCatalog cStore
    poller <- newPollerEnv cStore (weWaiters cWeb) (Just $ oePhases cOrders) (weHints cWeb) (weProviders cWeb) (poll $ weConfig cWeb)
    listening <- newEmptyMVar
    let lc = (listener $ weConfig cWeb) {lHost = "0.0.0.0"}
        settings = Warp.setBeforeMainLoop (putMVar listening ()) $ Warp.setPort lsServicePort $ webSettings lc
    withAsync (Warp.runSettings settings $ webApp cWeb) $ \server -> do
      link server
      takeMVar listening
      action ctx {cClient = (mgr, "http://127.0.0.1:" <> show lsServicePort)} poller

printingServiceLog :: IO a -> IO (Text, a)
printingServiceLog action = do
  result <- newEmptyMVar
  logs <- capturingStderr $ E.try action >>= putMVar result
  T.hPutStr stderr logs
  (logs,) <$> (takeMVar result >>= either (E.throwIO :: E.SomeException -> IO a) pure)

orderBadge :: HasCallStack => Ctx -> IO (InvoiceId, Text)
orderBadge ctx@Ctx {cClient} = do
  (status, created) <- post ctx "/api/invoice" (createBody supporterPriceId Nothing "ln" (codeHashText sampleCode))
  status `shouldBe` 200
  invId <- InvoiceId <$> maybe (fail "the order has no body") (`stringField` "invoiceId") created
  open <- viewInvoice cClient invId
  fieldOf open "status" `shouldBe` Just (J.String "open")
  fieldOf open "cryptoCurrency" `shouldBe` Just (J.String "ln")
  fieldOf open "requiredConfirmations" `shouldBe` Nothing
  bolt11 <- stringField open "address"
  bolt11 `shouldSatisfy` T.isPrefixOf bolt11Prefix
  pure (invId, bolt11)

payBolt11 :: HasCallStack => LightningStack -> Ctx -> Text -> IO ()
payBolt11 LightningStack {lsPayerUrl} Ctx {cClient = (mgr, _)} bolt11 = do
  req <- parseRequest (lsPayerUrl <> "/v2/router/send")
  answer <- responseBody <$> httpLbs req {method = "POST", requestBody = RequestBodyLBS payment, responseTimeout = responseTimeoutMicro paymentMicros} mgr
  (answer, paymentStatus =<< J.decode answer) `shouldBe` (answer, Just (J.String "SUCCEEDED"))
  where
    payment =
      J.encode $
        J.object
          [ "payment_request" .= bolt11,
            "timeout_seconds" .= paymentSeconds,
            "fee_limit_sat" .= feeLimitSat,
            "no_inflight_updates" .= True
          ]
    paymentStatus = \case
      J.Object sent | Just (J.Object result) <- KM.lookup "result" sent -> KM.lookup "status" result
      _ -> Nothing

awaitInvoice :: HasCallStack => Ctx -> InvoiceId -> IO () -> (InvoiceRow -> Bool) -> IO ()
awaitInvoice Ctx {cStore} invId readProvider reached =
  timeout settleMicros readUntilReached >>= maybe (expectationFailure ("the awaited state of the invoice was not read within " <> show settleSeconds <> " seconds")) pure
  where
    readUntilReached = do
      readProvider
      row <- getInvoice cStore invId
      unless (maybe False reached row) $ threadDelay readIntervalMicros >> readUntilReached

providerRefOf :: Ctx -> InvoiceId -> IO Text
providerRefOf Ctx {cStore} invId = maybe (fail "the invoice has no row") (pure . irProviderRef) =<< getInvoice cStore invId

testStartupLog :: LightningStack -> IO ()
testStartupLog LightningStack {lsBTCPay} = do
  (logs, _) <- printingServiceLog $ btcpayProvider lsBTCPay
  filter (T.isInfixOf $ "btcpay: store " <> bStoreId lsBTCPay <> " has enabled ") (T.lines logs) `shouldSatisfy` \case
    [enabled] -> all (`T.isInfixOf` enabled) [lnMethodId, btcMethodId]
    _ -> False
  logs `shouldSatisfy` loggedError ("btcpay: this build offers " <> xmrMethodId <> ", which the store has not enabled")

badgePaid :: (PollerEnv -> IO ()) -> LightningStack -> IO (Text, Text)
badgePaid readProvider stack = withService stack $ \ctx@Ctx {cStore, cClient} poller ->
  printingServiceLog $ do
    (invId, bolt11) <- orderBadge ctx
    payBolt11 stack ctx bolt11
    awaitInvoice ctx invId (readProvider poller) ((== ISPaid) . irStatus)
    paid <- viewInvoice cClient invId
    fieldOf paid "status" `shouldBe` Just (J.String "paid")
    fieldOf paid "cryptoAmountPaid" `shouldBe` fieldOf paid "cryptoAmount"
    fieldOf paid "paidInFull" `shouldBe` Just (J.Bool True)
    fieldOf paid "settledAt" `shouldSatisfy` isJust
    bcPaymentStatus <$> settledCode cStore invId `shouldReturn` CPSPaid
    providerRefOf ctx invId

testBadgePaidByWebhook :: LightningStack -> IO ()
testBadgePaidByWebhook stack = do
  (logs, ref) <- badgePaid drainHints stack
  logs `shouldSatisfy` T.isInfixOf ("queued a read of " <> ref)
  logs `shouldSatisfy` T.isInfixOf ("badge poller: order " <> ref <> " ISOpen -> ISPaid")

testBadgePaidByPass :: LightningStack -> IO ()
testBadgePaidByPass stack = do
  (logs, ref) <- badgePaid runOnePass stack
  logs `shouldSatisfy` T.isInfixOf ("badge poller: order " <> ref <> " ISOpen -> ISPaid")

testCancelledBadgePaid :: LightningStack -> IO ()
testCancelledBadgePaid stack = withService stack $ \ctx@Ctx {cStore, cClient} poller -> do
  (logs, (ref, paid)) <- printingServiceLog $ do
    (invId@(InvoiceId invText), bolt11) <- orderBadge ctx
    (status, cancelled) <- post ctx ("/api/invoice/" <> T.unpack invText <> "/cancel") ""
    (status, (`fieldOf` "status") =<< cancelled) `shouldBe` (200, Just (J.String "expired"))
    payBolt11 stack ctx bolt11
    awaitInvoice ctx invId (drainHints poller) (isJust . (ipCryptoPaid <=< irPayment))
    closed <- viewInvoice cClient invId
    fieldOf closed "status" `shouldBe` Just (J.String "expired")
    fieldOf closed "cryptoAmountPaid" `shouldBe` fieldOf closed "cryptoAmount"
    fieldOf closed "paidInFull" `shouldBe` Just (J.Bool True)
    bcPaymentStatus <$> settledCode cStore invId `shouldReturn` CPSUnpaid
    (,) <$> providerRefOf ctx invId <*> stringField closed "cryptoAmountPaid"
  logs `shouldSatisfy` loggedError ("badge poller: order " <> ref <> " expired holding " <> paid <> ", which needs a refund")

testNamePaid :: LightningStack -> IO ()
testNamePaid stack = withService stack $ \ctx@Ctx {cClient} poller -> do
  (logs, ref) <- printingServiceLog $ do
    (invId, order) <- placed ctx aliceOrder {oMethod = "ln"}
    fieldOf order "cryptoCurrency" `shouldBe` Just (J.String "ln")
    fieldOf order "requiredConfirmations" `shouldBe` Nothing
    nrPayment <$> invoiceRegistration ctx invId `shouldReturn` RPInvoice (SPMCrypto CCLn) invId
    bolt11 <- stringField order "address"
    bolt11 `shouldSatisfy` T.isPrefixOf bolt11Prefix
    payBolt11 stack ctx bolt11
    awaitInvoice ctx invId (drainHints poller) ((== ISPaid) . irStatus)
    nrPhase <$> invoiceRegistration ctx invId `shouldReturn` PQueued
    paid <- viewInvoice cClient invId
    (fieldOf paid "status", fieldOf (registrationOf paid) "phase") `shouldBe` (Just (J.String "paid"), Just (J.String "queued"))
    providerRefOf ctx invId
  logs `shouldSatisfy` T.isInfixOf ("queued a read of " <> ref)
