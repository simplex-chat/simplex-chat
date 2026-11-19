{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.StoreService.NamesPageService (namesPageServiceTests) where

import Bots.StoreService.FakeBTCPay (FakeBTCPay (..), fixtureRates, setInvoiceState, setRates, webhookEvent, webhookSigHeader, withFakeBTCPay)
import Bots.StoreService.FakeStripe (FakeStripe (..), setIntentState, stripeEvent, stripeSigHeader, withFakeStripe)
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
import Network.HTTP.Types (Header, Status, badRequest400, hContentType, internalServerError500, notFound404, ok200, status501, statusCode)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import Options.Applicative (defaultPrefs, execParserPure, handleParseResult, info)
import Simplex.Chat.Badges.Code (formatNameCode, nameLength)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentDestination (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import StoreService.Config (BTCPayConfig (..), ListenerConfig (..), ServiceConfig (..), StripeConfig (..), readServiceConfig)
import StoreService.Group.Command (CmdSource (..))
import StoreService.Names.Codes (issueNameCode, revokeNameCode)
import StoreService.Names.Turns (Turns, newTurns)
import StoreService.Options (storeServiceOpts)
import StoreService.Service (ServiceState (..), newServiceState, storeService)
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec
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
    pcTurns :: Turns,
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
    pcTurns <- newTurns
    pcManager <- HTTP.newManager HTTP.defaultManagerSettings
    race_ (storeService opts (Just cfg') terminalChatConfig env) $ do
      pcService <- atomically $ readTMVar (serviceCC env)
      Warp.runSettings (Warp.setHost "127.0.0.1" $ Warp.setPort controlPort Warp.defaultSettings) $
        controlApp PageControls {pcService, pcBTCPay, pcStripe, pcTurns, pcManager, pcWebhooks}

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
      ("POST", ["control", "rate"]) -> withObject body (rateControl pc)
      ("POST", ["control", action, invId])
        | action `elem` ["settle", "partial", "confirming", "expire"] -> withObject body (invoiceControl pc action (InvoiceId invId))
      _ -> pure $ Left (status501, "the service's runner has no such control")

withObject :: LB.ByteString -> (J.Object -> IO ControlAnswer) -> IO ControlAnswer
withObject body act = case J.decode body of
  Just (J.Object o) -> act o
  _ | LB.null body -> act mempty
  _ -> pure $ Left (badRequest400, "a control takes a JSON object")

codeControl :: PageControls -> J.Object -> IO ControlAnswer
codeControl PageControls {pcService, pcTurns} o = case JT.parseEither fields o of
  Left e -> pure $ Left (badRequest400, T.pack e)
  Right (minLength, years, state) -> case nameLength minLength of
    Nothing -> pure $ Left (badRequest400, "no names of " <> T.pack (show minLength) <> " letters")
    Just len ->
      issueNameCode pcService FromConsole len (fromMaybe 2 years) CPSFree >>= \case
        Left e -> pure $ Left (internalServerError500, T.pack e)
        Right (code, _) -> case fromMaybe "unused" state of
          "unused" -> pure $ answered code
          "revoked" -> either (\e -> Left (internalServerError500, e)) (const $ answered code) <$> revokeNameCode pcTurns pcService code
          other -> pure $ Left (status501, "the service's runner makes no " <> other <> " code")
  where
    fields v = (,,) <$> v J..: "minLength" <*> v J..:? "years" <*> v J..:? "state"
    answered code = Right $ J.object ["code" J..= formatNameCode code]

rateControl :: PageControls -> J.Object -> IO ControlAnswer
rateControl PageControls {pcBTCPay} o = case JT.parseEither fields o of
  Left e -> pure $ Left (badRequest400, T.pack e)
  Right (btc, xmr) -> do
    setRates pcBTCPay $ J.toJSON [rate "BTC_USD" btc, rate "XMR_USD" xmr]
    pure $ Right $ J.object ["ok" J..= True]
  where
    fields v = (,) <$> v J..:? "btc" <*> v J..:? "xmr"
    rate :: Text -> Maybe Int -> J.Value
    rate pair dollars = J.object ["currencyPair" J..= pair, "errors" J..= ([] :: [Text]), "rate" J..= maybe (lookup pair fixtureRates) (Just . T.pack . show) dollars]

invoiceControl :: PageControls -> Text -> InvoiceId -> J.Object -> IO ControlAnswer
invoiceControl pc action invId o =
  getInvoice (chatStore $ pcService pc) invId >>= \case
    Nothing -> pure $ Left (notFound404, "no invoice " <> T.pack (show invId))
    Just InvoiceRow {irProvider, irProviderRef, irDestination, irStatus} -> case (irProvider, irDestination) of
      (PPCrypto, SPDCrypto {currency, cryptoAmount}) -> case (JT.parseEither (J..:? "paid") o, decimal cryptoAmount) of
        (Right paid_, Just amount)
          | Just paid <- maybe (Just amount) decimal paid_ -> btcpayControl pc action irProviderRef (coinPlaces currency) (irStatus == ISOpen) amount paid
        _ -> pure $ Left (badRequest400, "paid must be a decimal amount of the invoice's coin")
      (PPStripe, _) -> stripeControl pc action irProviderRef
      _ -> pure $ Left (badRequest400, "the invoice is neither BTCPay's nor Stripe's")
  where
    decimal :: Text -> Maybe Scientific
    decimal = readMaybe . T.unpack

coinPlaces :: CryptoCurrency -> Int
coinPlaces = \case
  CCBtc -> 8
  CCXmr -> 12

btcpayControl :: PageControls -> Text -> Text -> Int -> Bool -> Scientific -> Scientific -> IO ControlAnswer
btcpayControl pc@PageControls {pcBTCPay} action ref places open amount paid = case action of
  "settle"
    | open -> report "InvoiceSettled" ["status" J..= ("Settled" :: Text), "paymentMethodPaid" J..= coin paid]
    | otherwise -> do
        now <- round <$> getPOSIXTime
        report
          "InvoiceReceivedPayment"
          [ "additionalStatus" J..= (if paid >= amount then "PaidLate" else "PaidPartial" :: Text),
            "paymentMethodPaid" J..= coin paid,
            "payments" J..= [J.object ["id" J..= ("late-" <> coin paid), "receivedDate" J..= (now :: Int), "value" J..= coin paid, "fee" J..= coin 0, "status" J..= ("Settled" :: Text)]]
          ]
  "partial" -> report "InvoiceReceivedPayment" ["additionalStatus" J..= ("PaidPartial" :: Text), "paymentMethodPaid" J..= coin (amount / 2)]
  "confirming" -> report "InvoiceProcessing" ["status" J..= ("Processing" :: Text), "paymentMethodPaid" J..= coin amount]
  _ -> report "InvoiceExpired" ["status" J..= ("Expired" :: Text)]
  where
    coin :: Scientific -> Text
    coin x = T.pack $ formatScientific Fixed (Just places) $ scientific (floor (toRational x * 10 ^ places)) (negate places)
    report event state = do
      setInvoiceState pcBTCPay ref state
      let body = webhookEvent event ref
      Right <$> webhook pc "btcpay" (webhookSigHeader (bWebhookSecret $ fbConfig pcBTCPay) body) body

stripeControl :: PageControls -> Text -> Text -> IO ControlAnswer
stripeControl pc@PageControls {pcStripe} action pid = case action of
  "settle" -> report "succeeded" "payment_intent.succeeded"
  "confirming" -> Right (J.object ["ok" J..= True]) <$ setIntentState pcStripe pid ["status" J..= ("processing" :: Text)]
  "expire" -> report "canceled" "payment_intent.canceled"
  _ -> pure $ Left (badRequest400, "a card payment is never paid in part")
  where
    report :: Text -> Text -> IO ControlAnswer
    report status event = do
      setIntentState pcStripe pid ["status" J..= status]
      t <- round <$> getPOSIXTime
      let body = stripeEvent event pid
      Right <$> webhook pc "stripe" (stripeSigHeader (sWebhookSecret $ fsConfig pcStripe) t body) body

webhook :: PageControls -> String -> [Header] -> LB.ByteString -> IO J.Value
webhook PageControls {pcManager, pcWebhooks} provider headers body = do
  req <- HTTP.parseRequest (pcWebhooks <> provider)
  r <- HTTP.httpLbs req {HTTP.method = "POST", HTTP.requestHeaders = (hContentType, "application/json") : headers, HTTP.requestBody = HTTP.RequestBodyLBS body} pcManager
  let st = statusCode (HTTP.responseStatus r)
  unless (st == 200) $ fail ("the service answered the " <> provider <> " webhook with " <> show st)
  pure $ J.object ["ok" J..= True]
