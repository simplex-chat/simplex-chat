{-# LANGUAGE NamedFieldPuns #-}

module BadgeService.Providers
  ( ProviderError (..),
    WebhookError (..),
    Received (..),
    PaymentSignal (..),
    ProviderInvoice (..),
    Funded (..),
    OrderDraft (..),
    ListPass (..),
    settleWindow,
    Provider (..),
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Network.HTTP.Types.Header (Header)
import Simplex.Chat.PaymentService.Types (CurrencyAmount, PaymentProvider, ServicePaymentDestination, ServicePaymentMethod)

newtype ProviderError = ProviderError Text deriving (Eq, Show)

newtype WebhookError = WebhookError Text deriving (Eq, Show)

-- rcvAmount is the total received on the invoice so far, not the amount of one payment.
-- rcvDue is the provider's figure for what is still owed.
data Received = Received {rcvAmount :: CurrencyAmount, rcvCrypto :: Maybe Text, rcvDue :: Maybe Text}
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
    -- Stops the provider accepting payment; cancelling only in our store leaves its invoice open until expiry.
    pCancelInvoice :: Text -> IO (Either ProviderError ()),
    pListOpen :: IO (Either ProviderError ListPass),
    pVerifyWebhook :: [Header] -> ByteString -> Either WebhookError (Maybe Text)
  }
