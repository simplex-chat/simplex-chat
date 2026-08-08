{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Simplex.Chat.Badges.Service
  ( BadgeServiceRequest (..),
    BadgeServiceCommand (..),
    BadgeServiceVersion,
    VersionBadgeService,
    pattern VersionBadgeService,
    ServicePaymentMethod (..),
    CardProvider (..),
    CryptoCurrency (..),
    CurrencyAmount (..),
    ServicePayment (..),
    BadgeUpgrade (..),
    BadgeServiceResponse (..),
    ServicePaymentDestination (..),
    BadgeServiceErrorCode (..),
    BadgeCatalog (..),
    BadgePrice (..),
    BadgeOffer (..),
    BadgeStatement (..),
    BadgeBalance (..),
    StatementEntry (..),
    StatementEntryType (..),
    StatementCreditType (..),
    StatementDebitType (..),
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8, Word16, Word32)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Store
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Version (VersionScope)
import Simplex.Messaging.Version.Internal (Version (..))

data BadgeServiceVersion

instance VersionScope BadgeServiceVersion

type VersionBadgeService = Version BadgeServiceVersion

pattern VersionBadgeService :: Word16 -> VersionBadgeService
pattern VersionBadgeService v = Version v

data BadgeServiceRequest = BadgeServiceRequest
  { version :: VersionBadgeService,
    purchaseKey :: Maybe C.PublicKeyEd25519, -- optional for BSCGetBadgeCatalog, required for other commands
    request :: BadgeServiceCommand
  }

data BadgeServiceCommand
  = BSCGetBadgeCatalog
  | BSCGetBadgeInvoice
      { priceId :: BadgePriceId,
        offerId :: Maybe BadgeOfferId, -- absent for 1 month at the badge price
        badgeInfo :: BadgeInfo,
        paymentVia :: ServicePaymentMethod,
        upgrade :: Maybe BadgeUpgrade -- upgrade non-store badge
      }
  | BSCPurchaseBadge
      { badgeRequest :: BadgeRequest,
        payment :: ServicePayment,
        upgrade :: Maybe BadgeUpgrade -- upgrade store 1-time badge
      }
  | BSCUpgradeBadgeSubscription
      { badgeRequest :: BadgeRequest,
        payment :: ServicePayment, -- store payments only
        balance :: BadgeBalance
      }
  | BSCIssueBadge
      { badgeRequest :: BadgeRequest,
        balance :: BadgeBalance
      }
  | BSCPauseBadge
  | BSCTransferBadge
      { badgeRequest :: BadgeRequest,
        receipt :: Text
      }

data ServicePaymentMethod
  = SPMCard {provider :: CardProvider}
  | SPMCrypto {currency :: CryptoCurrency}
  deriving (Eq, Show)

data CardProvider = CPStripe
  deriving (Eq, Show)

data CryptoCurrency = CCBtc | CCXmr
  deriving (Eq, Show)

-- USD etc. are in minor units, following Stripe etc. convention
newtype CurrencyAmount = CurrencyAmount Word32
  deriving (Eq, Show)

data ServicePayment
  = SPApple {jws :: Text}
  | SPGoogle {token :: Text}
  | SPInvoice {invoiceId :: InvoiceId}
  | SPCode {code :: Text}
  deriving (Show)

data BadgeUpgrade = BadgeUpgrade
  { fromPurchaseKey :: C.PublicKeyEd25519,
    receipt :: Text,
    receiptSignature :: C.Signature 'C.Ed25519,
    balance :: BadgeBalance
  }

data BadgeServiceResponse
  = BSPBadgeCatalog
      { catalog :: BadgeCatalog,
        badgeStatement :: Maybe BadgeStatement -- for signed getBadgeCatalog
      }
  | BSPBadgeInvoice
      { invoiceId :: InvoiceId,
        badgeType :: BadgeType,
        months :: Word8,
        price :: CurrencyAmount,
        discount :: Maybe CurrencyAmount, -- discount amount from monthly price
        credit :: Maybe CurrencyAmount, -- credit for upgrade
        amount :: CurrencyAmount,
        currency :: Text,
        expiresAt :: UTCTime,
        paymentTo :: ServicePaymentDestination
      }
  | BSPBadgeCredential
      { credential :: Maybe BadgeCredential, -- Nothing when no balance to issueBadge or no current credential for pause
        receipt :: Maybe Text, -- not provided for lifetime badges
        statement :: BadgeStatement
      }
  | BSPError
      { code :: BadgeServiceErrorCode,
        message :: Maybe Text,
        retryAfter :: Maybe Word32
      }

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

data BadgeCatalog = BadgeCatalog
  { prices :: [BadgePrice],
    offers :: [BadgeOffer]
  }
  deriving (Show)

data BadgePrice = BadgePrice
  { priceId :: BadgePriceId,
    badgeType :: BadgeType,
    monthPrice :: CurrencyAmount,
    currency :: Text,
    status :: BadgeItemStatus,
    createdAt :: UTCTime
  }
  deriving (Show)

data BadgeOffer = BadgeOffer
  { offerId :: BadgeOfferId,
    priceId :: Maybe BadgePriceId, -- absent applies to any price
    months :: Word8,
    discount :: OfferDiscount,
    status :: BadgeItemStatus,
    createdAt :: UTCTime
  }
  deriving (Show)

data BadgeStatement = BadgeStatement
  { entries :: [StatementEntry],
    previousEntryId :: Maybe Text -- matches the client's asserted entryId, absent for the full ledger
  }
  deriving (Show)

data BadgeBalance = BadgeBalance
  { lastEntry :: StatementEntry
  }
  deriving (Show)

data StatementEntry = StatementEntry
  { entryId :: Text,
    changeMonths :: Int,
    balanceMonths :: Int,
    balanceStartTs :: UTCTime,
    balanceBadgeType :: BadgeType,
    wasPausedSince :: Maybe UTCTime,
    createdAt :: UTCTime,
    entryType :: StatementEntryType
  }
  deriving (Show)

data StatementEntryType = SECredit {credit :: StatementCreditType} | SEDebit {debit :: StatementDebitType}
  deriving (Show)

data StatementCreditType
  = SCPayment {invoiceId :: Maybe InvoiceId} -- absent for store and code payments
  | SCCharge {chargeId :: Int64}
  | SCSupport
  | SCTransferIn {fromPurchaseKey :: C.PublicKeyEd25519}
  | SCOpening
  | SCUnknown {tag :: Text, json :: J.Object}
  deriving (Show)

data StatementDebitType
  = SDRefund
  | SDUpgrade {toPurchaseKey :: C.PublicKeyEd25519}
  | SDTransferOut {toPurchaseKey :: C.PublicKeyEd25519}
  | SDSupport
  | SDBadge
  | SDLapse
  | SDUnknown {tag :: Text, json :: J.Object}
  deriving (Show)

data BadgeServiceErrorCode
  = BSEBadRequest
  | BSEUnsupportedVersion
  | BSEUnknownPurchaseKey
  | BSEUnknownOfferId
  | BSEOfferDisabled
  | BSEOfferMismatch
  | BSEProductUnavailable
  | BSEPaymentNotEntitled
  | BSEPaymentPending
  | BSEProviderUnavailable
  | BSERateLimited
  | BSECodeInvalid
  | BSECodeUsed
  | BSECodeExpired
  | BSEReceiptInvalid
  | BSEReceiptUsed
  | BSEInternal
  | BSEUnknown Text -- forwards-compatible: service is deployed ahead of clients
  deriving (Eq, Show)

instance TextEncoding BadgeServiceErrorCode where
  textEncode = \case
    BSEBadRequest -> "bad_request"
    BSEUnsupportedVersion -> "unsupported_version"
    BSEUnknownPurchaseKey -> "unknown_purchase_key"
    BSEUnknownOfferId -> "unknown_offer_id"
    BSEOfferDisabled -> "offer_disabled"
    BSEOfferMismatch -> "offer_mismatch"
    BSEProductUnavailable -> "product_unavailable"
    BSEPaymentNotEntitled -> "payment_not_entitled"
    BSEPaymentPending -> "payment_pending"
    BSEProviderUnavailable -> "provider_unavailable"
    BSERateLimited -> "rate_limited"
    BSECodeInvalid -> "code_invalid"
    BSECodeUsed -> "code_used"
    BSECodeExpired -> "code_expired"
    BSEReceiptInvalid -> "receipt_invalid"
    BSEReceiptUsed -> "receipt_used"
    BSEInternal -> "internal"
    BSEUnknown t -> t
  textDecode s = Just $ case s of
    "bad_request" -> BSEBadRequest
    "unsupported_version" -> BSEUnsupportedVersion
    "unknown_purchase_key" -> BSEUnknownPurchaseKey
    "unknown_offer_id" -> BSEUnknownOfferId
    "offer_disabled" -> BSEOfferDisabled
    "offer_mismatch" -> BSEOfferMismatch
    "product_unavailable" -> BSEProductUnavailable
    "payment_not_entitled" -> BSEPaymentNotEntitled
    "payment_pending" -> BSEPaymentPending
    "provider_unavailable" -> BSEProviderUnavailable
    "rate_limited" -> BSERateLimited
    "code_invalid" -> BSECodeInvalid
    "code_used" -> BSECodeUsed
    "code_expired" -> BSECodeExpired
    "receipt_invalid" -> BSEReceiptInvalid
    "receipt_used" -> BSEReceiptUsed
    "internal" -> BSEInternal
    t -> BSEUnknown t

instance ToJSON BadgeServiceErrorCode where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON BadgeServiceErrorCode where
  parseJSON = textParseJSON "BadgeServiceErrorCode"
