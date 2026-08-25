{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Badges.Types
  ( BadgePriceId (..),
    BadgeOfferId (..),
    BadgePlan (..),
    BadgeItemStatus (..),
    OfferDiscount (..),
    BadgePurchaseStatus (..),
    LedgerEntryType (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    BadgeAlertKind (..),
    BadgePurchase (..),
    BadgeLedgerEntry (..),
    BadgeCharge (..),
    BadgeIssuance (..),
    BadgeAlert (..),
    UserBadgeState (..),
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Simplex.Chat.Badges hiding (BadgePurchase (..))
import Simplex.Chat.PaymentService.Types (InvoiceId, StoredPayment)
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
#endif

-- confirmed
newtype BadgePriceId = BadgePriceId Text
  deriving newtype (Eq, Show, ToJSON, FromJSON)

-- confirmed
newtype BadgeOfferId = BadgeOfferId Text
  deriving newtype (Eq, Show, ToJSON, FromJSON)

-- unconfirmed draft
data BadgePlan = BPOneTime | BPMonthly | BPAnnual
  deriving (Eq, Show)

-- confirmed
data BadgeItemStatus = BISActive | BISDeprecated | BISDisabled -- disabled is not sent
  deriving (Eq, Show)

-- confirmed
data OfferDiscount
  = ODFreeMonths {freeMonths :: Word8}
  | ODDiscount {discount :: Word8} -- percent
  deriving (Eq, Show)

-- unconfirmed draft
data BadgePurchaseStatus = PSAcquiring | PSIssued | PSSuperseded | PSFailed
  deriving (Eq, Show)

-- confirmed
data LedgerEntryType = LECredit {credit :: LedgerCreditType} | LEDebit {debit :: LedgerDebitType}
  deriving (Eq, Show)

-- confirmed
data LedgerCreditType
  = -- | badge_ledger.payment_id TEXT REFERENCES payments -- the payment's own id, not the
    -- invoice's: a code payment has no invoice at all, and an invoice-funded payment reaches
    -- its invoice through payments.invoice_id.
    --
    -- 'Maybe' because the SERVICE and the CLIENT hold different halves of the same ledger. The
    -- service mints the payment row and names it here; the client copies the same entries into
    -- its own badge_ledger and has no payments row at all for a code redemption -- that row
    -- exists only in the service database, and the wire carries no payment id (the statement's
    -- SCPayment carries the INVOICE's id, which a code payment does not have). So the client
    -- writes NULL, and this field must be able to hold it. It was 'Text' between B7 and B10,
    -- which would have blocked C1 on its first write (plan §9).
    CTPayment {paymentId :: Maybe Text}
  | CTCharge {chargeId :: Int64}
  | CTSupport
  | CTTransferIn {fromPurchaseId :: Maybe Int64}
  | CTOpening
  | CTUnknown {tag :: Text, json :: J.Object}
  deriving (Eq, Show)

-- confirmed
data LedgerDebitType
  = DTRefund
  | DTUpgrade {toPurchaseId :: Int64}
  | DTTransferOut {toPurchaseId :: Int64}
  | DTSupport
  | DTBadge
  | DTLapse
  | DTUnknown {tag :: Text, json :: J.Object}
  deriving (Eq, Show)

-- unconfirmed draft
data BadgeAlertKind = BARenewalApproaching | BAPaymentIssue | BASubscriptionEnded | BAPrepaidEnding | BASupportEnded
  deriving (Eq, Show)

-- to review
data BadgePurchase = BadgePurchase
  { badgePurchaseId :: Int64,
    userId :: UserId,
    purchaseKey :: C.PublicKeyEd25519,
    purchasePrivKey :: C.PrivateKeyEd25519,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType,
    priceId :: Maybe BadgePriceId,
    offerId :: Maybe BadgeOfferId,
    paymentId :: Maybe Text, -- payments.payment_id TEXT REFERENCES @payments (nullable)
    status :: BadgePurchaseStatus,
    credential :: Maybe BadgeCredential,
    alertAcked :: Maybe (BadgeAlertKind, Text),
    alertSnoozeUntil :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

-- confirmed
data BadgeLedgerEntry = BadgeLedgerEntry
  { entryId :: Int64,
    entryUuid :: Text,
    badgePurchaseId :: Int64,
    changeMonths :: Int,
    balanceMonths :: Int,
    balanceStartTs :: UTCTime,
    balanceBadgeType :: BadgeType,
    wasPausedSince :: Maybe UTCTime,
    serviceCreatedAt :: UTCTime,
    createdAt :: UTCTime,
    entryType :: LedgerEntryType
  }
  deriving (Show)

-- unconfirmed draft
data BadgeCharge = BadgeCharge
  { chargeId :: Text, -- subscription_charges.charge_id TEXT NOT NULL PRIMARY KEY
    paymentId :: Text, -- payments.payment_id TEXT NOT NULL PRIMARY KEY
    invoiceUuid :: InvoiceId,
    providerChargeRef :: Text,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    amount :: Int64,
    currency :: Text,
    chargedAt :: UTCTime
  }
  deriving (Show)

-- unconfirmed draft
data BadgeIssuance = BadgeIssuance
  { issuanceId :: Text, -- badge_issuances.issuance_id TEXT NOT NULL PRIMARY KEY
    badgePurchaseId :: Int64,
    badgeType :: BadgeType,
    periodStart :: Maybe UTCTime,
    periodEnd :: Maybe UTCTime,
    expiry :: Maybe UTCTime,
    entryId :: Maybe Int64,
    credential :: BadgeCredential,
    createdAt :: UTCTime
  }
  deriving (Show)

-- unconfirmed draft
data BadgeAlert = BadgeAlert
  { kind :: BadgeAlertKind,
    episode :: Text,
    date :: UTCTime,
    price :: Maybe (Int64, Text)
  }
  deriving (Show)

-- unconfirmed draft
data UserBadgeState = UserBadgeState
  { badges :: [BadgePurchase],
    shownBadgeId :: Maybe Int64,
    payments :: [StoredPayment],
    monthsLeft :: Int,
    paidThrough :: Maybe UTCTime,
    renewsAt :: Maybe UTCTime,
    willRenew :: Bool,
    alert :: Maybe BadgeAlert
  }

-- BadgeItemStatus crosses both the wire (BadgePrice/BadgeOffer JSON) and the
-- badge_prices/badge_offers.status columns (A4's seedCatalog); TextEncoding is the single
-- spelling both ToJSON/FromJSON and ToField/FromField derive from, so the two can't drift
-- into two independent encodings of the same enum.
instance TextEncoding BadgeItemStatus where
  textEncode = \case
    BISActive -> "active"
    BISDeprecated -> "deprecated"
    BISDisabled -> "disabled"
  textDecode s = case s of
    "active" -> Just BISActive
    "deprecated" -> Just BISDeprecated
    "disabled" -> Just BISDisabled
    _ -> Nothing

instance ToJSON BadgeItemStatus where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON BadgeItemStatus where
  parseJSON = textParseJSON "BadgeItemStatus"

instance ToField BadgeItemStatus where toField = toField . textEncode

instance FromField BadgeItemStatus where fromField = fromTextField_ textDecode

-- DB column spelling for BadgePurchaseStatus: the type does not cross the wire, so this spelling
-- is only ever read back from the badge_purchases.status column it was written to. The payment
-- statuses of the same rows are PaymentService.Types' InvoiceStatus and PaymentStatus, which
-- carry their own instances there.
instance TextEncoding BadgePurchaseStatus where
  textEncode = \case
    PSAcquiring -> "acquiring"
    PSIssued -> "issued"
    PSSuperseded -> "superseded"
    PSFailed -> "failed"
  textDecode s = case s of
    "acquiring" -> Just PSAcquiring
    "issued" -> Just PSIssued
    "superseded" -> Just PSSuperseded
    "failed" -> Just PSFailed
    _ -> Nothing

instance ToJSON BadgePurchaseStatus where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON BadgePurchaseStatus where
  parseJSON = textParseJSON "BadgePurchaseStatus"

instance ToField BadgePurchaseStatus where toField = toField . textEncode

instance FromField BadgePurchaseStatus where fromField = fromTextField_ textDecode

-- JSON

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "OD") ''OfferDiscount)
