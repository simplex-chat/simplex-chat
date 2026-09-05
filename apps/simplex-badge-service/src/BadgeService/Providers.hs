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

-- | A running total, not the amount that arrived this time, and @rcvDue@ is the provider's
-- own figure for what is still owed: it carries the fee a partial payment adds.
data Received = Received {rcvAmount :: CurrencyAmount, rcvCrypto :: Maybe Text, rcvDue :: Maybe Text}
  deriving (Eq, Show)

-- | Whether the provider considers the invoice paid. It applies its own payment tolerance,
-- so this cannot be recomputed from the amounts.
data Funded = PaidInFull | PaidInPart
  deriving (Eq, Show)

data PaymentSignal
  = SigFunded Received Funded -- something arrived, the invoice is not settled
  | SigSettled Received UTCTime -- paid in full
  | SigClosed Received -- the window closed
  deriving (Eq, Show)

data ProviderInvoice = ProviderInvoice
  { piProviderRef :: Text,
    piDestination :: ServicePaymentDestination
  }
  deriving (Eq, Show)

-- | How long an invoice stays worth asking about, measured from when it was created. A payment
-- can land after the window the buyer was given closes, so an invoice outlives its own expiry;
-- past this it is left to an operator. A provider that lists rather than reads adds its own
-- expiry window on top, since its cutoff has to cover invoices created this long ago.
settleWindow :: NominalDiffTime
settleWindow = 72 * 3600

data OrderDraft = OrderDraft
  { odAmount :: CurrencyAmount,
    odCurrency :: Text
  }
  deriving (Eq, Show)

-- | Unreadable invoices come back as data rather than log lines, because only the poller
-- can decide how often to warn and only it can tell ours from a stranger's.
data ListPass = ListPass
  { lpMoved :: [(Text, PaymentSignal)],
    lpSkipped :: [(Maybe Text, Text)]
  }
  deriving (Eq, Show)

data Provider = Provider
  { pProvider :: PaymentProvider,
    pCreateInvoice :: ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice),
    pReadInvoice :: Text -> IO (Either ProviderError (Maybe PaymentSignal)),
    -- | Stops the provider accepting payment. Cancelling only in our own store would leave the
    -- provider's invoice open until its own expiry, so a buyer who cancelled could still pay.
    pCancelInvoice :: Text -> IO (Either ProviderError ()),
    pListOpen :: IO (Either ProviderError ListPass),
    pVerifyWebhook :: [Header] -> ByteString -> Either WebhookError (Maybe Text)
  }
