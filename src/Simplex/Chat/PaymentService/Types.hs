{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Simplex.Chat.PaymentService.Types
  ( CurrencyAmount (..),
    InvoiceId (..),
    PaymentId (..),
    PaymentProvider (..),
    CardProvider (..),
    CryptoCurrency (..),
    ServicePaymentMethod (..),
    ServicePaymentDestination (..),
    InvoiceStatus (..),
    StoredInvoice (..),
    StoredPayment (..),
    PaymentFunding (..),
    PaymentTerm (..),
    PaymentStatus (..),
  ) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word32)

-- USD etc. are in minor units, following Stripe etc. convention
newtype CurrencyAmount = CurrencyAmount Word32
  deriving (Eq, Show)

-- confirmed
newtype InvoiceId = InvoiceId Text
  deriving newtype (Eq, Show)

-- confirmed
newtype PaymentId = PaymentId Text
  deriving newtype (Eq, Show)

-- confirmed
data PaymentProvider = PPApple | PPGoogle | PPStripe | PPCrypto | PPCode | PPReceipt
  deriving (Eq, Show)

data CardProvider = CPStripe
  deriving (Eq, Show)

data CryptoCurrency = CCBtc | CCXmr
  deriving (Eq, Show)

data ServicePaymentMethod
  = SPMCard {provider :: CardProvider}
  | SPMCrypto {currency :: CryptoCurrency}
  deriving (Eq, Show)

data ServicePaymentDestination
  = SPDCard
      { provider :: CardProvider,
        url :: Text
      }
  | SPDCrypto
      { currency :: CryptoCurrency,
        address :: Text,
        cryptoAmount :: Text
      }
  deriving (Show)

-- confirmed
data InvoiceStatus = ISOpen | ISPaid | ISExpired
  deriving (Eq, Show)

-- confirmed
data StoredInvoice = StoredInvoice
  { invoiceId :: InvoiceId,
    price :: CurrencyAmount,
    discountAmount :: CurrencyAmount,
    creditAmount :: CurrencyAmount,
    amount :: CurrencyAmount, -- price - discount - credit
    currency :: Text,
    paymentTo :: ServicePaymentDestination,
    expiresAt :: UTCTime,
    status :: InvoiceStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

-- to review
data StoredPayment = StoredPayment
  { paymentId :: PaymentId,
    funding :: PaymentFunding,
    term :: PaymentTerm,
    status :: PaymentStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

-- to review
data PaymentFunding
  = PFInvoice
      { invoiceId :: InvoiceId,
        providerRef :: Text,
        amount :: CurrencyAmount,
        currency :: Text,
        receiptCode :: Maybe Text -- client; the service holds its hash
      }
  | PFApple
      { providerRef :: Text,
        amount :: CurrencyAmount,
        currency :: Text,
        evidence :: Maybe ByteString -- client only
      }
  | PFGoogle
      { providerRef :: Text,
        amount :: CurrencyAmount,
        currency :: Text,
        evidence :: Maybe ByteString -- client only
      }
  | PFCode
  | PFReceipt
  deriving (Show)

-- to review
data PaymentTerm
  = PTOneOff
  | PTSubscription
      { renewsAt :: UTCTime,
        graceUntil :: Maybe UTCTime,
        cancelled :: Bool
      }
  deriving (Show)

-- to review
data PaymentStatus = PSPending | PSSettled | PSFailed {exception :: Text}
  deriving (Show)
