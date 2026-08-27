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

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word32)
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import Simplex.Messaging.Encoding.String
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

-- | DB column spelling for @invoices.status@. Neither this type nor 'PaymentStatus' crosses the
-- wire -- an invoice reaches the app as 'Simplex.Chat.PaymentService.ServiceInvoice', which
-- carries no status at all -- so the spelling below is only ever read back from the column it was
-- written to. The column is plain @TEXT NOT NULL@ with no CHECK (@M20261001_user_badges@), so
-- these instances are the only thing that pins it.
instance TextEncoding InvoiceStatus where
  textEncode = \case
    ISOpen -> "open"
    ISPaid -> "paid"
    ISExpired -> "expired"
  textDecode = \case
    "open" -> Just ISOpen
    "paid" -> Just ISPaid
    "expired" -> Just ISExpired
    _ -> Nothing

instance ToField InvoiceStatus where toField = toField . textEncode

instance FromField InvoiceStatus where fromField = fromTextField_ textDecode

-- | DB column spelling for @payments.status@, on the same terms as 'InvoiceStatus'.
--
-- __'textDecode' cannot round-trip 'PSFailed'.__ The failure text is a column of its own,
-- @payments.exception@, so @textDecode "failed"@ can only return an empty one and a reader that
-- wants the text must select that column and fill it in. Encoding is total and lossless, which is
-- the direction both writers use: a redeemed code writes 'PSSettled' and nothing else writes this
-- column yet.
instance TextEncoding PaymentStatus where
  textEncode = \case
    PSPending -> "pending"
    PSSettled -> "settled"
    PSFailed {} -> "failed"
  textDecode = \case
    "pending" -> Just PSPending
    "settled" -> Just PSSettled
    "failed" -> Just PSFailed {exception = ""}
    _ -> Nothing

instance ToField PaymentStatus where toField = toField . textEncode

-- There is deliberately __no 'FromField' instance__. 'textDecode' cannot recover 'PSFailed'\'s
-- text (above), and a 'FromField' would let a row parser turn @SELECT status@ into a
-- 'PaymentStatus' silently, dropping it. Whoever first reads a failed payment must select
-- @status@ and @exception@ together and build the value from both; the missing instance makes
-- that a compile error instead of a silent loss. 'InvoiceStatus' keeps its 'FromField' because
-- its three constructors are nullary and its decode is lossless.

-- | DB column spelling for @invoices.provider@, @payments.provider@ and
-- @provider_events.provider@. 'PaymentProvider' does not cross the wire (the RPC names a
-- provider through 'ServicePaymentMethod'), so this spelling is only ever read back from a
-- column it was written to -- the same arrangement 'InvoiceStatus' and 'PaymentStatus' document
-- above. It replaces the two @codePaymentProviderText = "code"@ literals that the client and
-- the service each carried while no codec existed (plan §9, B1\/C1): the badge service now writes
-- a second provider ('PPStripe' and 'PPCrypto', from D0's 'BadgeService.Store.createOrder'), which
-- is the condition both notes named for adding this.
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

instance ToField PaymentProvider where toField = toField . textEncode

instance FromField PaymentProvider where fromField = fromTextField_ textDecode

-- JSON

-- CardProvider has a single nullary constructor; tagSingleConstructors is needed so it still
-- encodes as a bare string tag rather than as an untagged empty-array product (see MemberCriteria
-- in Types.hs for the same fix).
$(JQ.deriveJSON (enumJSON $ dropPrefix "CP") {J.tagSingleConstructors = True} ''CardProvider)

$(JQ.deriveJSON (enumJSON $ dropPrefix "CC") ''CryptoCurrency)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SPM") ''ServicePaymentMethod)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SPD") ''ServicePaymentDestination)
