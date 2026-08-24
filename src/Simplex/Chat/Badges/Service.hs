{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Badges.Service
  ( BadgeServiceRequest (..),
    BadgeServiceCommand (..),
    BadgeServiceVersion,
    VersionBadgeService,
    pattern VersionBadgeService,
    minSupportedBadgeVersion,
    currentBadgeVersion,
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

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
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
import Simplex.Messaging.Version (VersionScope)
import Simplex.Messaging.Version.Internal (Version (..))

data BadgeServiceVersion

instance VersionScope BadgeServiceVersion

type VersionBadgeService = Version BadgeServiceVersion

pattern VersionBadgeService :: Word16 -> VersionBadgeService
pattern VersionBadgeService v = Version v

-- | The oldest client version the service still answers (badges-rpc.md:9): a request below
-- this gets 'BSEUnsupportedVersion'. Below 'currentBadgeVersion' only because a version bump
-- is expected to stay backwards compatible for a while, not because the service itself has
-- ever spoken more than one version yet.
minSupportedBadgeVersion :: VersionBadgeService
minSupportedBadgeVersion = VersionBadgeService 1

-- | The newest version this service deployment speaks. A response answers within
-- @min(request.version, currentBadgeVersion)@; at version 1 there is no version-conditional
-- field, so this constant has no runtime effect yet -- it exists so a later version bump has
-- something to gate on from day one.
currentBadgeVersion :: VersionBadgeService
currentBadgeVersion = VersionBadgeService 1

data BadgeServiceRequest = BadgeServiceRequest
  { version :: VersionBadgeService,
    purchaseKey :: Maybe C.PublicKeyEd25519, -- optional for BSCGetBadgeCatalog, required for other commands
    request :: BadgeServiceCommand
  }
  deriving (Show)

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
  deriving (Show)

data BadgeUpgrade = BadgeUpgrade
  { fromPurchaseKey :: C.PublicKeyEd25519,
    receipt :: Text,
    receiptSignature :: C.Signature 'C.Ed25519,
    balance :: BadgeBalance
  }
  deriving (Show)

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
    createdAt :: UTCTime,
    total :: Maybe CurrencyAmount -- absent when the store layer hasn't computed totals yet (catalogTotals, A4); the service always fills it
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
  | SCCharge {chargeId :: Text} -- subscription_charges.charge_id TEXT NOT NULL PRIMARY KEY
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

-- JSON

-- StatementCreditType/StatementDebitType are hand-written (not TH-derived) so that an unrecognised
-- "type" decodes into SCUnknown/SDUnknown and re-encodes verbatim from the stored object, per
-- docs/protocol/badges-rpc.md: "An unknown type is stored as received and decoded after an app upgrade."

(.=?) :: ToJSON v => J.Key -> Maybe v -> [(J.Key, J.Value)] -> [(J.Key, J.Value)]
key .=? value = maybe id ((:) . (key .=)) value

instance FromJSON StatementCreditType where
  parseJSON (J.Object v) = do
    tag <- v .: "type" :: JT.Parser Text
    case tag of
      "payment" -> SCPayment <$> v .:? "invoiceId"
      "charge" -> SCCharge <$> v .: "chargeId"
      "support" -> pure SCSupport
      "transferIn" -> SCTransferIn <$> v .: "fromPurchaseKey"
      "opening" -> pure SCOpening
      _ -> pure $ SCUnknown tag v
  parseJSON invalid = JT.prependFailure "bad StatementCreditType, " (JT.typeMismatch "Object" invalid)

instance ToJSON StatementCreditType where
  toJSON = \case
    SCUnknown {json} -> J.Object json
    SCPayment {invoiceId} -> J.object $ ("invoiceId" .=? invoiceId) ["type" .= ("payment" :: Text)]
    SCCharge {chargeId} -> J.object ["type" .= ("charge" :: Text), "chargeId" .= chargeId]
    SCSupport -> J.object ["type" .= ("support" :: Text)]
    SCTransferIn {fromPurchaseKey} -> J.object ["type" .= ("transferIn" :: Text), "fromPurchaseKey" .= fromPurchaseKey]
    SCOpening -> J.object ["type" .= ("opening" :: Text)]
  toEncoding = \case
    SCUnknown {json} -> JE.value $ J.Object json
    SCPayment {invoiceId} -> J.pairs $ "type" .= ("payment" :: Text) <> maybe mempty ("invoiceId" .=) invoiceId
    SCCharge {chargeId} -> J.pairs $ "type" .= ("charge" :: Text) <> "chargeId" .= chargeId
    SCSupport -> J.pairs $ "type" .= ("support" :: Text)
    SCTransferIn {fromPurchaseKey} -> J.pairs $ "type" .= ("transferIn" :: Text) <> "fromPurchaseKey" .= fromPurchaseKey
    SCOpening -> J.pairs $ "type" .= ("opening" :: Text)

instance FromJSON StatementDebitType where
  parseJSON (J.Object v) = do
    tag <- v .: "type" :: JT.Parser Text
    case tag of
      "refund" -> pure SDRefund
      "upgrade" -> SDUpgrade <$> v .: "toPurchaseKey"
      "transferOut" -> SDTransferOut <$> v .: "toPurchaseKey"
      "support" -> pure SDSupport
      "badge" -> pure SDBadge
      "lapse" -> pure SDLapse
      _ -> pure $ SDUnknown tag v
  parseJSON invalid = JT.prependFailure "bad StatementDebitType, " (JT.typeMismatch "Object" invalid)

instance ToJSON StatementDebitType where
  toJSON = \case
    SDUnknown {json} -> J.Object json
    SDRefund -> J.object ["type" .= ("refund" :: Text)]
    SDUpgrade {toPurchaseKey} -> J.object ["type" .= ("upgrade" :: Text), "toPurchaseKey" .= toPurchaseKey]
    SDTransferOut {toPurchaseKey} -> J.object ["type" .= ("transferOut" :: Text), "toPurchaseKey" .= toPurchaseKey]
    SDSupport -> J.object ["type" .= ("support" :: Text)]
    SDBadge -> J.object ["type" .= ("badge" :: Text)]
    SDLapse -> J.object ["type" .= ("lapse" :: Text)]
  toEncoding = \case
    SDUnknown {json} -> JE.value $ J.Object json
    SDRefund -> J.pairs $ "type" .= ("refund" :: Text)
    SDUpgrade {toPurchaseKey} -> J.pairs $ "type" .= ("upgrade" :: Text) <> "toPurchaseKey" .= toPurchaseKey
    SDTransferOut {toPurchaseKey} -> J.pairs $ "type" .= ("transferOut" :: Text) <> "toPurchaseKey" .= toPurchaseKey
    SDSupport -> J.pairs $ "type" .= ("support" :: Text)
    SDBadge -> J.pairs $ "type" .= ("badge" :: Text)
    SDLapse -> J.pairs $ "type" .= ("lapse" :: Text)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SE") ''StatementEntryType)

$(JQ.deriveJSON defaultJSON ''StatementEntry)

$(JQ.deriveJSON defaultJSON ''BadgeBalance)

$(JQ.deriveJSON defaultJSON ''BadgeStatement)

$(JQ.deriveJSON defaultJSON ''BadgePrice)

$(JQ.deriveJSON defaultJSON ''BadgeOffer)

$(JQ.deriveJSON defaultJSON ''BadgeCatalog)

$(JQ.deriveJSON defaultJSON ''BadgeUpgrade)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "BSP") ''BadgeServiceResponse)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "BSC") ''BadgeServiceCommand)

$(JQ.deriveJSON defaultJSON ''BadgeServiceRequest)
