{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.StoreService
  ( StoreServiceRequest (..),
    StoreServiceCommand (..),
    StoreServiceVersion,
    VersionStoreService,
    VersionRangeStoreService,
    pattern VersionStoreService,
    initialStoreServiceVersion,
    currentStoreServiceVersion,
    supportedStoreServiceVRange,
    BadgeUpgrade (..),
    StoreServiceResponse (..),
    StoreServiceErrorCode (..),
    SubscriptionState (..),
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
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import Simplex.Messaging.Version (VersionRange, VersionScope, mkVersionRange)
import Simplex.Messaging.Version.Internal (Version (..))

data StoreServiceVersion

instance VersionScope StoreServiceVersion

type VersionStoreService = Version StoreServiceVersion

pattern VersionStoreService :: Word16 -> VersionStoreService
pattern VersionStoreService v = Version v

type VersionRangeStoreService = VersionRange StoreServiceVersion

initialStoreServiceVersion :: VersionStoreService
initialStoreServiceVersion = VersionStoreService 1

currentStoreServiceVersion :: VersionStoreService
currentStoreServiceVersion = VersionStoreService 1

-- the service is deployed ahead of app releases, so it answers within the client's version
supportedStoreServiceVRange :: VersionRangeStoreService
supportedStoreServiceVRange = mkVersionRange initialStoreServiceVersion currentStoreServiceVersion

data StoreServiceRequest = StoreServiceRequest
  { version :: VersionStoreService,
    purchaseKey :: Maybe C.PublicKeyEd25519, -- optional for STCGetBadgeCatalog, required for other commands
    request :: StoreServiceCommand
  }

data StoreServiceCommand
  = STCGetBadgeCatalog
  | STCGetBadgeInvoice
      { priceId :: BadgePriceId,
        offerId :: Maybe BadgeOfferId, -- absent for 1 month at the badge price
        badgeInfo :: BadgeInfo,
        paymentVia :: ServicePaymentMethod,
        upgrade :: Maybe BadgeUpgrade -- upgrade non-store badge
      }
  | STCRedeemBadgeCode
      { masterKey :: BadgeMasterKey,
        code :: Text -- no badgeRequest: a code carries no tier for the client to state
      }
  | STCPurchaseBadge
      { masterKey :: BadgeMasterKey, -- no badgeRequest: the tier is the product's, the expiry the one every credential of that week shares
        payment :: ServicePayment,
        upgrade :: Maybe BadgeUpgrade -- upgrade store 1-time badge
      }
  | STCUpgradeBadgeSubscription
      { payment :: ServicePayment, -- names the subscription and the tier to wait for; the store's answer is what is granted
        balance :: BadgeBalance
      }
  | STCTransferBadge
      { masterKey :: BadgeMasterKey,
        payment :: ServicePayment -- the store receipt of the subscription to move to the signing key
      }
  | STCIssueBadge
      { balance :: BadgeBalance -- no badgeRequest: the service holds the key, the tier and the expiry
      }
  | STCPauseBadge

data BadgeUpgrade = BadgeUpgrade
  { fromPurchaseKey :: C.PublicKeyEd25519,
    receipt :: Text,
    receiptSignature :: C.Signature 'C.Ed25519,
    balance :: BadgeBalance
  }

data StoreServiceResponse
  = STPBadgeCatalog
      { catalog :: BadgeCatalog,
        badgeStatement :: Maybe BadgeStatement -- for signed getBadgeCatalog
      }
  | STPBadgeInvoice
      { invoice :: ServiceInvoice,
        badgeType :: BadgeType,
        months :: Word8
      }
  | STPBadgeCredential
      { credential :: Maybe BadgeCredential, -- Nothing when no balance to issueBadge or no current credential for pause
        receipt :: Maybe Text, -- not provided for lifetime badges
        statement :: BadgeStatement,
        subscription :: Maybe SubscriptionState -- present exactly when a store subscription funds the purchase
      }
  | STPError
      { code :: StoreServiceErrorCode,
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

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "STC") ''StoreServiceCommand)

$(JQ.deriveJSON defaultJSON ''StoreServiceRequest)

$(JQ.deriveJSON defaultJSON ''BadgePrice)

$(JQ.deriveJSON defaultJSON ''BadgeOffer)

$(JQ.deriveJSON defaultJSON ''BadgeCatalog)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "STP") ''StoreServiceResponse)
