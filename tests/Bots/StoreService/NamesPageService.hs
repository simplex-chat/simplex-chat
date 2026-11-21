{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.StoreService.NamesPageService (namesPageServiceTests) where

import Bots.StoreService.FakeBTCPay (FakeBTCPay (..), setInvoiceState, webhookEvent, webhookSigHeader, withFakeBTCPay)
import Bots.StoreService.FakeStripe (FakeStripe (..), setIntentState, stripeEvent, stripeSigHeader, withFakeStripe)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (atomically, readTMVar)
import qualified Control.Exception as E
import Control.Logger.Simple (setLogLevel)
import Control.Monad (unless)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromMaybe)
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific, scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (Header, Status, badRequest400, conflict409, hContentType, internalServerError500, notFound404, ok200, status501, statusCode)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import Options.Applicative (defaultPrefs, execParserPure, handleParseResult, info)
import Simplex.Chat.Badges.Code (formatNameCode, nameLength)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Controller (ChatController (..), ChatResponse (CRCustomChatResponse))
import Simplex.Chat.Core (sendChatCmdStr)
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentDestination (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Names.Record (USDCents (..))
import StoreService.Config (BTCPayConfig (..), ListenerConfig (..), ServiceConfig (..), StripeConfig (..), readServiceConfig)
import StoreService.Names.Codes (issueNameCode)
import StoreService.Names.Store (Balance (..), CmdSource (..), LatePending (..), balanceByReference, latePending)
import StoreService.Options (storeServiceOpts)
import StoreService.Service (ServiceState (..), newServiceState, storeService)
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceRow (..), currentSecond, getInvoice)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, pendingWith)
import Text.Read (readMaybe)

namesPageServiceTests :: Spec
namesPageServiceTests =
  describe "Names page against the store service" $
    it "serves the page's cases with fake payment providers until it is stopped" $
      ((,) <$> lookupEnv "NAMES_PAGE_RUN_DIR" <*> lookupEnv "NAMES_PAGE_CONTROL_PORT") >>= \case
        (Just dir, Just port) | Just controlPort <- readMaybe port -> serveNamesPage dir controlPort
        _ -> pendingWith "scripts/store-service/dev/contract-run.sh runs it with NAMES_PAGE_RUN_DIR and NAMES_PAGE_CONTROL_PORT"

unreachableSmpServer :: String
unreachableSmpServer = "smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@127.0.0.1:1"

data PageControls = PageControls
  { pcService :: ChatController,
    pcBTCPay :: FakeBTCPay,
    pcStripe :: FakeStripe,
    pcManager :: HTTP.Manager,
    pcWebhooks :: String
  }

type ControlAnswer = Either (Status, Text) J.Value

serveNamesPage :: FilePath -> Int -> IO ()
serveNamesPage dir controlPort =
  withFakeBTCPay $ \pcBTCPay -> withFakeStripe $ \pcStripe -> do
    let ini = dir </> "store_service.ini"
    cfg <- readServiceConfig ini >>= either (fail . ((ini <> ": ") <>)) pure
    let cfg' = cfg {btcpay = Just (fbConfig pcBTCPay), stripe = Just (fsConfig pcStripe)}
        ListenerConfig {lHost, lBadgesPort} = listener cfg'
        pcWebhooks = "http://" <> T.unpack lHost <> ":" <> show lBadgesPort <> "/webhooks/"
        args = ["--no-address", "-d", dir </> "db", "-s", unreachableSmpServer, "--smp-proxy", "always", "--smp-proxy-fallback", "no"]
    opts <- handleParseResult $ execParserPure defaultPrefs (info (storeServiceOpts dir "db") mempty) args
    setLogLevel (serviceLogLevel cfg')
    env <- newServiceState
    pcManager <- HTTP.newManager HTTP.defaultManagerSettings
    race_ (storeService opts (Just cfg') terminalChatConfig env) $ do
      pcService <- atomically $ readTMVar (serviceCC env)
      Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort controlPort Warp.defaultSettings) $
        controlApp PageControls {pcService, pcBTCPay, pcStripe, pcManager, pcWebhooks}

controlApp :: PageControls -> Application
controlApp pc req respond = do
  body <- strictRequestBody req
  answer <- either (\(e :: E.SomeException) -> Left (internalServerError500, T.pack (show e))) id <$> E.try (control body)
  respond $ case answer of
    Right v -> json ok200 v
    Left (st, e) -> json st (J.object ["error" J..= e])
  where
    json st = responseLBS st [(hContentType, "application/json")] . J.encode
    control body = case (requestMethod req, pathInfo req) of
      ("POST", ["control", "code"]) -> withObject body (codeControl pc)
      ("POST", ["control", "refund", ref]) -> withObject body (refundControl pc ref)
      ("POST", ["control", action, invId])
        | action `elem` ["settle", "partial", "confirming", "expire"] -> withObject body (invoiceControl pc action (InvoiceId invId))
      _ -> pure $ Left (status501, "the service's runner has no such control")

withObject :: LB.ByteString -> (J.Object -> IO ControlAnswer) -> IO ControlAnswer
withObject body act = case J.decode body of
  Just (J.Object o) -> act o
  _ | LB.null body -> act mempty
  _ -> pure $ Left (badRequest400, "a control takes a JSON object")

codeControl :: PageControls -> J.Object -> IO ControlAnswer
codeControl PageControls {pcService} o = case JT.parseEither fields o of
  Left e -> pure $ Left (badRequest400, T.pack e)
  Right (minLength, years, state) -> case nameLength minLength of
    Nothing -> pure $ Left (badRequest400, "no names of " <> T.pack (show minLength) <> " letters")
    Just len ->
      issueNameCode pcService FromConsole len (fromMaybe 2 years) CPSFree >>= \case
        Left e -> pure $ Left (internalServerError500, T.pack e)
        Right (code, _) -> case fromMaybe "unused" state of
          "unused" -> pure $ answered code
          "revoked" ->
            sendChatCmdStr pcService ("//revoke " <> T.unpack (formatNameCode code)) >>= \case
              Right CRCustomChatResponse {} -> pure $ answered code
              r -> pure $ Left (internalServerError500, T.pack (show (() <$ r)))
          other -> pure $ Left (status501, "the service's runner makes no " <> other <> " code")
  where
    fields v = (,,) <$> v J..: "minLength" <*> v J..:? "years" <*> v J..:? "state"
    answered code = Right $ J.object ["code" J..= formatNameCode code]

refundControl :: PageControls -> Text -> J.Object -> IO ControlAnswer
refundControl PageControls {pcService} ref o = case JT.parseEither (J..:? "cents") o of
  Left e -> pure $ Left (badRequest400, T.pack e)
  Right cents_ -> do
    now <- currentSecond
    withTransaction (chatStore pcService) (\db -> balanceByReference db ref now) >>= \case
      Nothing -> pure $ Left (notFound404, "no order " <> ref)
      Just (_, balance) -> do
        let cents = fromMaybe (maybe 0 (\Balance {blAmount = USDCents c} -> c) balance) cents_
        sendChatCmdStr pcService ("//record_refund " <> T.unpack ref <> " " <> show cents) >>= \case
          Right (CRCustomChatResponse _ answer) -> pure $ Right $ J.object ["cents" J..= cents, "answer" J..= answer]
          r -> pure $ Left (conflict409, T.pack (show (() <$ r)))

invoiceControl :: PageControls -> Text -> InvoiceId -> J.Object -> IO ControlAnswer
invoiceControl pc action invId o =
  getInvoice (chatStore $ pcService pc) invId >>= \case
    Nothing -> pure $ Left (notFound404, "no invoice " <> T.pack (show invId))
    Just InvoiceRow {irProvider, irProviderRef, irDestination, irStatus} -> case (irProvider, irDestination) of
      (PPCrypto, SPDCrypto {currency, cryptoAmount}) -> case (JT.parseEither (J..:? "paid") o, decimal cryptoAmount) of
        (Right paid_, Just amount)
          | Just paid <- maybe (Just amount) decimal paid_ -> btcpayControl pc invId action irProviderRef (coinPlaces currency) (irStatus == ISOpen) amount paid
        _ -> pure $ Left (badRequest400, "paid must be a decimal amount of the invoice's coin")
      (PPStripe, _) -> stripeControl pc invId action irProviderRef
      _ -> pure $ Left (badRequest400, "the invoice is neither BTCPay's nor Stripe's")
  where
    decimal :: Text -> Maybe Scientific
    decimal = readMaybe . T.unpack

coinPlaces :: CryptoCurrency -> Int
coinPlaces = \case
  CCBtc -> 8
  CCXmr -> 12

btcpayControl :: PageControls -> InvoiceId -> Text -> Text -> Int -> Bool -> Scientific -> Scientific -> IO ControlAnswer
btcpayControl pc@PageControls {pcBTCPay} invId action ref places open amount paid = case action of
  "settle"
    | open -> report "InvoiceSettled" ["status" J..= ("Settled" :: Text), "paymentMethodPaid" J..= coin paid] (statusRead ISPaid)
    | otherwise -> do
        now <- round <$> getPOSIXTime
        report
          "InvoiceReceivedPayment"
          [ "additionalStatus" J..= (if paid >= amount then "PaidLate" else "PaidPartial" :: Text),
            "paymentMethodPaid" J..= coin paid,
            "payments" J..= [J.object ["id" J..= ("late-" <> coin paid), "receivedDate" J..= (now :: Int), "value" J..= coin paid, "fee" J..= coin 0, "status" J..= ("Settled" :: Text)]]
          ]
          (paidRead paid)
  "partial" -> report "InvoiceReceivedPayment" ["additionalStatus" J..= ("PaidPartial" :: Text), "paymentMethodPaid" J..= coin (amount / 2)] (paidRead (amount / 2))
  "confirming" -> report "InvoiceProcessing" ["status" J..= ("Processing" :: Text), "paymentMethodPaid" J..= coin amount] (paidRead amount)
  _ -> report "InvoiceExpired" ["status" J..= ("Expired" :: Text)] (statusRead ISExpired)
  where
    coin :: Scientific -> Text
    coin x = T.pack $ formatScientific Fixed (Just places) $ scientific (floor (toRational x * 10 ^ places)) (negate places)
    paidRead x InvoiceRow {irPayment} = (ipCryptoPaid =<< irPayment) == Just (coin x)
    report event state done = do
      setInvoiceState pcBTCPay ref state
      let body = webhookEvent event ref
      webhook pc "btcpay" (webhookSigHeader (bWebhookSecret $ fbConfig pcBTCPay) body) body
      Right <$> readByService pc invId done

stripeControl :: PageControls -> InvoiceId -> Text -> Text -> IO ControlAnswer
stripeControl pc@PageControls {pcStripe} invId action pid = case action of
  "settle" -> report "succeeded" "payment_intent.succeeded" ISPaid
  "confirming" -> Right (J.object ["read" J..= True]) <$ setIntentState pcStripe pid ["status" J..= ("processing" :: Text)]
  "expire" -> report "canceled" "payment_intent.canceled" ISExpired
  _ -> pure $ Left (badRequest400, "a card payment is never paid in part")
  where
    report :: Text -> Text -> InvoiceStatus -> IO ControlAnswer
    report status event done = do
      setIntentState pcStripe pid ["status" J..= status]
      t <- round <$> getPOSIXTime
      let body = stripeEvent event pid
      webhook pc "stripe" (stripeSigHeader (sWebhookSecret $ fsConfig pcStripe) t body) body
      Right <$> readByService pc invId (statusRead done)

statusRead :: InvoiceStatus -> InvoiceRow -> Bool
statusRead st InvoiceRow {irStatus} = irStatus == st

webhook :: PageControls -> String -> [Header] -> LB.ByteString -> IO ()
webhook PageControls {pcManager, pcWebhooks} path headers body = do
  req <- HTTP.parseRequest (pcWebhooks <> path)
  r <- HTTP.httpLbs req {HTTP.method = "POST", HTTP.requestHeaders = (hContentType, "application/json") : headers, HTTP.requestBody = HTTP.RequestBodyLBS body} pcManager
  let st = statusCode (HTTP.responseStatus r)
  unless (st == 200) $ fail ("the service answered the " <> path <> " webhook with " <> show st)

readByService :: PageControls -> InvoiceId -> (InvoiceRow -> Bool) -> IO J.Value
readByService PageControls {pcService} invId done = (\ok -> J.object ["read" J..= ok]) <$> attempt readAttempts
  where
    st = chatStore pcService
    attempt :: Int -> IO Bool
    attempt left = do
      row <- getInvoice st invId
      late <- withTransaction st (`latePending` invId)
      if maybe False done row && late /= Just LPUncredited
        then pure True
        else if left <= 0 then pure False else threadDelay readIntervalMicros >> attempt (left - 1)

readAttempts, readIntervalMicros :: Int
readAttempts = 100
readIntervalMicros = 100000
