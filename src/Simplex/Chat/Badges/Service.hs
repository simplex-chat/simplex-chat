{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Badges.Service
  ( BadgeServiceRequest (..),
    BadgeServiceCommand (..),
    BadgeServiceVersion,
    VersionBadgeService,
    VersionRangeBadgeService,
    pattern VersionBadgeService,
    initialBadgeServiceVersion,
    currentBadgeServiceVersion,
    supportedBadgeServiceVRange,
    BadgeUpgrade (..),
    BadgeServiceResponse (..),
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

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8, Word16, Word32)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Types
import Simplex.Chat.PaymentService
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import Simplex.Messaging.Version (VersionRange, VersionScope, mkVersionRange)
import Simplex.Messaging.Version.Internal (Version (..))

data BadgeServiceVersion

instance VersionScope BadgeServiceVersion

type VersionBadgeService = Version BadgeServiceVersion

pattern VersionBadgeService :: Word16 -> VersionBadgeService
pattern VersionBadgeService v = Version v

type VersionRangeBadgeService = VersionRange BadgeServiceVersion

initialBadgeServiceVersion :: VersionBadgeService
initialBadgeServiceVersion = VersionBadgeService 1

currentBadgeServiceVersion :: VersionBadgeService
currentBadgeServiceVersion = VersionBadgeService 1

-- the service is deployed ahead of app releases, so it answers within the client's version
supportedBadgeServiceVRange :: VersionRangeBadgeService
supportedBadgeServiceVRange = mkVersionRange initialBadgeServiceVersion currentBadgeServiceVersion

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
  | BSCRedeemBadgeCode
      { masterKey :: BadgeMasterKey,
        code :: Text -- no badgeRequest: a code carries no tier for the client to state
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
      { balance :: BadgeBalance -- no badgeRequest: the service holds the key, the tier and the expiry
      }
  | BSCPauseBadge

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
      { invoice :: ServiceInvoice,
        badgeType :: BadgeType,
        months :: Word8
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
    -- the start of the current run of months; every month boundary in it is counted from here,
    -- so that the day of month survives a short month
    balanceAnchorTs :: UTCTime,
    balanceBadgeType :: BadgeType,
    wasPausedSince :: Maybe UTCTime,
    createdAt :: UTCTime,
    entryType :: StatementEntryType
  }
  deriving (Show)

data StatementEntryType = SECredit {credit :: StatementCreditType} | SEDebit {debit :: StatementDebitType}
  deriving (Show)

data StatementCreditType
  = SCPayment {invoiceId :: Maybe InvoiceId} -- absent for store payments
  | SCCode -- a redeemed code; its own invoice belongs to the buyer, not to the redeemer
  | SCCharge {chargeId :: Text}
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

$(pure [])

instance FromJSON StatementCreditType where
  parseJSON v@(J.Object j) =
    $(JQ.mkParseJSON (taggedObjectJSON $ dropPrefix "SC") ''StatementCreditType) v
      <|> SCUnknown <$> j .: "type" <*> pure j
  parseJSON invalid =
    JT.prependFailure "bad StatementCreditType, " (JT.typeMismatch "Object" invalid)

instance ToJSON StatementCreditType where
  toJSON = \case
    SCUnknown _ j -> J.Object j
    v -> $(JQ.mkToJSON (taggedObjectJSON $ dropPrefix "SC") ''StatementCreditType) v
  toEncoding = \case
    SCUnknown _ j -> JE.value $ J.Object j
    v -> $(JQ.mkToEncoding (taggedObjectJSON $ dropPrefix "SC") ''StatementCreditType) v

instance FromJSON StatementDebitType where
  parseJSON v@(J.Object j) =
    $(JQ.mkParseJSON (taggedObjectJSON $ dropPrefix "SD") ''StatementDebitType) v
      <|> SDUnknown <$> j .: "type" <*> pure j
  parseJSON invalid =
    JT.prependFailure "bad StatementDebitType, " (JT.typeMismatch "Object" invalid)

instance ToJSON StatementDebitType where
  toJSON = \case
    SDUnknown _ j -> J.Object j
    v -> $(JQ.mkToJSON (taggedObjectJSON $ dropPrefix "SD") ''StatementDebitType) v
  toEncoding = \case
    SDUnknown _ j -> JE.value $ J.Object j
    v -> $(JQ.mkToEncoding (taggedObjectJSON $ dropPrefix "SD") ''StatementDebitType) v

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SE") ''StatementEntryType)

$(JQ.deriveJSON defaultJSON ''StatementEntry)

$(JQ.deriveJSON defaultJSON ''BadgeStatement)

$(JQ.deriveJSON defaultJSON ''BadgeBalance)

$(JQ.deriveJSON defaultJSON ''BadgeUpgrade)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "BSC") ''BadgeServiceCommand)

$(JQ.deriveJSON defaultJSON ''BadgeServiceRequest)

$(JQ.deriveJSON defaultJSON ''BadgePrice)

$(JQ.deriveJSON defaultJSON ''BadgeOffer)

$(JQ.deriveJSON defaultJSON ''BadgeCatalog)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "BSP") ''BadgeServiceResponse)
