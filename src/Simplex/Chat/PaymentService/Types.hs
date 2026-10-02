{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.PaymentService.Types
  ( CurrencyAmount (..),
    InvoiceId (..),
    PaymentId (..),
    PaymentProvider (..),
    StoreTransactionRef (..),
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

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson.TH as JQ
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word32)
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, taggedObjectJSON)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
#endif

-- USD etc. are in minor units, following Stripe etc. convention
newtype CurrencyAmount = CurrencyAmount Word32
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- confirmed
newtype InvoiceId = InvoiceId Text
  deriving newtype (Eq, Show, ToJSON, FromJSON)

-- confirmed
newtype PaymentId = PaymentId Text
  deriving newtype (Eq, Show)

-- confirmed
data PaymentProvider = PPApple | PPGoogle | PPStripe | PPCrypto | PPCode | PPReceipt
  deriving (Eq, Show)

instance TextEncoding PaymentProvider where
  textEncode = \case
    PPApple -> "apple"
    PPGoogle -> "google"
    PPStripe -> "stripe"
    PPCrypto -> "crypto"
    PPCode -> "code"
    PPReceipt -> "receipt"
  textDecode = \case
    "apple" -> Just PPApple
    "google" -> Just PPGoogle
    "stripe" -> Just PPStripe
    "crypto" -> Just PPCrypto
    "code" -> Just PPCode
    "receipt" -> Just PPReceipt
    _ -> Nothing

instance FromField PaymentProvider where fromField = fromTextField_ textDecode

instance ToField PaymentProvider where toField = toField . textEncode

-- | The stable name of one store transaction, by which a retry resolves to the same row.
data StoreTransactionRef = StoreTransactionRef {provider :: PaymentProvider, transactionRef :: Text}
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
  deriving (Eq, Show)

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
-- TODO [badges] reconcile PFApple and PFGoogle with the badge service's VerifiedStoreTransaction,
-- which is what a store attested rather than a payment recorded here, when this is built out.
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

$(JQ.deriveJSON (enumJSON $ dropPrefix "CP") ''CardProvider)

$(JQ.deriveJSON (enumJSON $ dropPrefix "CC") ''CryptoCurrency)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SPM") ''ServicePaymentMethod)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SPD") ''ServicePaymentDestination)
