{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Providers
  ( ProviderError (..),
    WebhookError (..),
    Received (..),
    Confirmed (..),
    Uncreditable (..),
    Rate (..),
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

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Network.HTTP.Types.Header (Header)
import Simplex.Chat.PaymentService.Types (CryptoCurrency, CurrencyAmount, PaymentProvider (..), ServicePaymentDestination, ServicePaymentMethod)

newtype ProviderError = ProviderError Text deriving (Eq, Show)

-- | WebhookStale is a valid signature with a timestamp too far from our clock.
data WebhookError = WebhookError Text | WebhookStale Text deriving (Eq, Show)

-- rcvAmount is the total received on the invoice so far, not the amount of one payment.
-- rcvDue is the provider's figure for what is still owed.
-- rcvUnits is the crypto received in integer atomic units, Nothing for card money or crypto that could not be counted in them.
-- rcvRate is the invoice's rate for crypto, as the provider wrote it.
data Received = Received
  { rcvAmount :: CurrencyAmount,
    rcvCrypto :: Maybe Text,
    rcvDue :: Maybe Text,
    rcvUnits :: Maybe Int64,
    rcvRate :: Maybe Text,
    rcvConfirmed :: Confirmed,
    rcvUncreditable :: Maybe Uncreditable
  }
  deriving (Eq, Show)

-- | The money confirmed so far: its cents at the invoice's rate, and for crypto its atomic units.
data Confirmed = Confirmed {cfCents :: CurrencyAmount, cfUnits :: Maybe Int64}
  deriving (Eq, Show)

-- | Why money on an invoice is never credited by itself: it was made under a policy that settles at zero confirmations,
-- its crypto could not be counted in atomic units, or the provider shows no confirmation of it. Such money confirms nothing.
data Uncreditable = UCHighSpeed | UCUncounted | UCNoConfirmationShown
  deriving (Eq, Show)

-- | A coin's current price in the service's currency, above 0: as the provider wrote it, and as a decimal to compute with.
data Rate = Rate {rateText :: Text, rateValue :: Scientific}
  deriving (Eq, Show)

-- The provider applies its own tolerance, so this cannot be recomputed from the amounts.
data Funded = PaidInFull | PaidInPart
  deriving (Eq, Show)

data PaymentSignal
  = SigFunded Received Funded
  | SigSettled Received UTCTime
  | SigClosed Received
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

data OrderDraft = OrderDraft
  { odAmount :: CurrencyAmount,
    odCurrency :: Text
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
