{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Providers
  ( ProviderError (..),
    providerErrorText,
    isTimeout,
    providerCall,
    decodeAnswer,
    objectId,
    WebhookError (..),
    Received (..),
    CryptoReceived (..),
    Confirmation (..),
    Units,
    receivedAmount,
    receivedConfirmed,
    Confirmed (..),
    Uncreditable (..),
    Rate (..),
    unitDecimals,
    unitName,
    PaymentSignal (..),
    ProviderInvoice (..),
    Funded (..),
    OrderDraft (..),
    ListPass (..),
    settleWindow,
    expiresItself,
    Provider (..),
  )
where

import Control.Exception (try)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Network.HTTP.Client (HttpException, Manager, Request, Response (..), brReadSome, parseRequest, withResponse)
import Network.HTTP.Types (Status (..))
import Network.HTTP.Types.Header (Header)
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), CurrencyAmount (..), PaymentProvider (..), ServicePaymentDestination, ServicePaymentMethod)
import Simplex.Messaging.Agent.Store.DB (FromField (..), ToField (..), fromTextField_)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data ProviderError = ProviderError Text | ProviderTimeout Text
  deriving (Eq, Show)

providerErrorText :: ProviderError -> Text
providerErrorText = \case
  ProviderError e -> e
  ProviderTimeout what -> what <> " failed: timed out"

isTimeout :: ProviderError -> Bool
isTimeout = \case
  ProviderTimeout _ -> True
  ProviderError _ -> False

-- BTCPay puts the refusal reason at the end of an inline log, so keep enough bytes to reach it.
maxErrorBytes :: Int64
maxErrorBytes = 4000

-- Far above any BTCPay response, far below what would exhaust the poller thread.
maxProviderBytes :: Int64
maxProviderBytes = 10 * 1024 * 1024

providerCall :: Manager -> Int -> Text -> Text -> (Request -> Request) -> IO ([Header], Either ProviderError LB.ByteString)
providerCall manager micros what url prepare = do
  r <- timeout micros $ try $ do
    req <- prepare <$> parseRequest (T.unpack url)
    -- Read to a bound, not the whole body, so an enormous response cannot exhaust the poller thread.
    withResponse req manager $ \resp -> do
      taken <- brReadSome (responseBody resp) (fromIntegral maxProviderBytes + 1)
      pure (responseHeaders resp, statusCode (responseStatus resp), taken)
  pure $ case r of
    Nothing -> ([], Left (ProviderTimeout what))
    -- http-client hides the Authorization header when showing a Request
    Just (Left e) -> ([], Left (ProviderError (what <> " failed: " <> tshow (e :: HttpException))))
    Just (Right (headers, code, taken)) -> (headers, answer code taken)
  where
    answer code taken
      | LB.length taken > maxProviderBytes =
          Left . ProviderError $
            what <> " failed: HTTP " <> tshow code <> ", and the answer is over " <> tshow maxProviderBytes <> " bytes"
      | code >= 200 && code < 300 = Right taken
      | otherwise = Left . ProviderError $ what <> " failed: HTTP " <> tshow code <> " " <> safeDecodeUtf8 (LB.toStrict (LB.take maxErrorBytes taken))

decodeAnswer :: J.FromJSON a => Text -> LB.ByteString -> Either ProviderError a
decodeAnswer what body = case J.eitherDecode' body of
  Right v -> Right v
  Left e -> Left (ProviderError (what <> ": could not read the response: " <> T.pack e))

objectId :: J.Value -> Maybe Text
objectId v = case J.fromJSON v :: J.Result (KM.KeyMap J.Value) of
  J.Success o -> case KM.lookup "id" o of
    Just (J.String i) -> Just i
    _ -> Nothing
  J.Error _ -> Nothing

-- | WebhookStale is a valid signature with a timestamp too far from our clock.
data WebhookError = WebhookError Text | WebhookStale Text deriving (Eq, Show)

-- | The amounts are the total received on the invoice so far, not the amount of one payment.
data Received
  = RCard {rcAmount :: CurrencyAmount, rcConfirmed :: CurrencyAmount}
  | RCrypto CryptoReceived
  deriving (Eq, Show)

-- crAmount is the crypto received in cents at the invoice's rate, crPaid as the provider wrote it, Nothing while nothing is.
-- crDue is the provider's figure for what is still owed.
-- crUnits is the crypto received in integer atomic units, Nothing when it could not be counted in them.
-- crInvoicedUnits is the crypto the invoice asks in the same units, with any network fee the provider added to it.
-- crRate is the invoice's rate, as the provider wrote it.
data CryptoReceived = CryptoReceived
  { crAmount :: CurrencyAmount,
    crPaid :: Maybe Text,
    crDue :: Maybe Text,
    crUnits :: Maybe Units,
    crInvoicedUnits :: Maybe Units,
    crRate :: Rate,
    crConfirmation :: Confirmation
  }
  deriving (Eq, Show)

-- | The crypto confirmed so far, in atomic units and in cents at the invoice's rate, or why none of it is credited by itself.
data Confirmation = CryptoConfirmed Units CurrencyAmount | Uncreditable Uncreditable
  deriving (Eq, Show)

type Units = Int64

receivedAmount :: Received -> CurrencyAmount
receivedAmount = \case
  RCard {rcAmount} -> rcAmount
  RCrypto CryptoReceived {crAmount} -> crAmount

-- | As a payment row stores it, where uncreditable crypto confirms nothing.
receivedConfirmed :: Received -> Confirmed
receivedConfirmed = \case
  RCard {rcConfirmed} -> Confirmed {cfCents = rcConfirmed, cfUnits = Nothing}
  RCrypto CryptoReceived {crConfirmation} -> case crConfirmation of
    CryptoConfirmed units cents -> Confirmed {cfCents = cents, cfUnits = Just units}
    Uncreditable _ -> Confirmed {cfCents = CurrencyAmount 0, cfUnits = Just 0}

-- | The money confirmed so far: its cents at the invoice's rate, and for crypto its atomic units.
data Confirmed = Confirmed {cfCents :: CurrencyAmount, cfUnits :: Maybe Units}
  deriving (Eq, Show)

-- | Why money on an invoice is never credited by itself: it was made under a policy that settles at zero confirmations,
-- its crypto could not be counted in atomic units, or the provider shows no confirmation of it. Such money confirms nothing.
data Uncreditable = UCHighSpeed | UCUncounted | UCNoConfirmationShown
  deriving (Eq, Show)

-- | A coin's current price in the service's currency, above 0: as the provider wrote it, and as a decimal to compute with.
data Rate = Rate {rateText :: Text, rateValue :: Scientific}
  deriving (Eq, Show)

instance FromField Rate where fromField = fromTextField_ textRate

instance ToField Rate where toField = toField . rateText

textRate :: Text -> Maybe Rate
textRate t = case readMaybe (T.unpack t) of
  Just v | v > 0 -> Just Rate {rateText = t, rateValue = v}
  _ -> Nothing

-- | The decimals of each coin's atomic unit.
unitDecimals :: CryptoCurrency -> Int
unitDecimals = \case
  CCBtc -> 8
  CCXmr -> 12

unitName :: CryptoCurrency -> Text
unitName = \case
  CCBtc -> "satoshi"
  CCXmr -> "piconero"

-- The provider applies its own tolerance, so this cannot be recomputed from the amounts.
data Funded = PaidInFull | PaidInPart
  deriving (Eq, Show)

-- | A closed invoice's Funded says whether the whole amount reached it, late or not.
data PaymentSignal
  = SigFunded Received Funded
  | SigSettled Received UTCTime
  | SigClosed Received Funded
  deriving (Eq, Show)

data ProviderInvoice = ProviderInvoice
  { piProviderRef :: Text,
    piDestination :: ServicePaymentDestination
  }
  deriving (Eq, Show)

-- | BTCPay invoices expire by themselves. Stripe payments stay open until we cancel them.
expiresItself :: PaymentProvider -> Bool
expiresItself = \case
  PPCrypto -> True
  _ -> False

-- A payment can land after the buyer's checkout window closes, so the poller keeps asking about an invoice for this long after it was created.
settleWindow :: NominalDiffTime
settleWindow = 72 * 3600

-- | odReference names the order at the provider, so support can find it there; a badge order's is its invoice id.
data OrderDraft = OrderDraft
  { odAmount :: CurrencyAmount,
    odCurrency :: Text,
    odReference :: Text
  }
  deriving (Eq, Show)

data ListPass = ListPass
  { lpMoved :: [(Text, PaymentSignal)],
    lpSkipped :: [(Maybe Text, Text)]
  }
  deriving (Eq, Show)

data Provider = Provider
  { pProvider :: PaymentProvider,
    pCreateInvoice :: ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice),
    pReadInvoice :: Text -> IO (Either ProviderError (Maybe PaymentSignal)),
    -- Stops the provider from accepting payment for this order.
    pCancelInvoice :: Text -> IO (Either ProviderError ()),
    pListOpen :: IO (Either ProviderError ListPass),
    -- | Nothing for a provider of card payments, which needs no rate.
    pCurrentRate :: Maybe (CryptoCurrency -> IO (Either ProviderError Rate)),
    pVerifyWebhook :: UTCTime -> [Header] -> ByteString -> Either WebhookError (Maybe Text)
  }
