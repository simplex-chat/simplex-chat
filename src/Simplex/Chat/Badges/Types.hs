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
    BadgeCodePaymentStatus (..),
    LedgerEntryType (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    BadgeAlertKind (..),
    BadgeFunding (..),
    BadgePurchase (..),
    BadgeLedgerEntry (..),
    BadgeCharge (..),
    BadgeIssuance (..),
    BadgeAlert (..),
    BadgeState (..),
    UserBadgeState (..),
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Simplex.Chat.Badges hiding (BadgePurchase (..))
import Simplex.Chat.PaymentService.Types (InvoiceId, PaymentId, StoredPayment)
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, taggedObjectJSON)
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

-- unconfirmed draft
data BadgeCodePaymentStatus = CPSPaid | CPSUnpaid | CPSFree
  deriving (Eq, Show)

-- confirmed
data LedgerEntryType = LECredit {credit :: LedgerCreditType} | LEDebit {debit :: LedgerDebitType}
  deriving (Eq, Show)

-- confirmed
data LedgerCreditType
  = CTPayment {invoiceId :: InvoiceId}
  | CTCode
  | CTCharge {chargeId :: Text}
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

instance TextEncoding BadgeAlertKind where
  textEncode = \case
    BARenewalApproaching -> "renewal_approaching"
    BAPaymentIssue -> "payment_issue"
    BASubscriptionEnded -> "subscription_ended"
    BAPrepaidEnding -> "prepaid_ending"
    BASupportEnded -> "support_ended"
  textDecode = \case
    "renewal_approaching" -> Just BARenewalApproaching
    "payment_issue" -> Just BAPaymentIssue
    "subscription_ended" -> Just BASubscriptionEnded
    "prepaid_ending" -> Just BAPrepaidEnding
    "support_ended" -> Just BASupportEnded
    _ -> Nothing

instance FromField BadgeAlertKind where fromField = fromTextField_ textDecode

instance ToField BadgeAlertKind where toField = toField . textEncode

-- exactly one of these funds a purchase; the schema cannot say so, both columns being nullable
data BadgeFunding
  = BFPayment {paymentId :: PaymentId}
  | BFCodeRedemption {redemptionId :: Int64}
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
    funding :: BadgeFunding,
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
    balanceAnchorTs :: UTCTime,
    balanceBadgeType :: BadgeType,
    wasPausedSince :: Maybe UTCTime,
    serviceCreatedAt :: UTCTime,
    createdAt :: UTCTime,
    entryType :: LedgerEntryType
  }
  deriving (Show)

-- unconfirmed draft
data BadgeCharge = BadgeCharge
  { chargeId :: Int64,
    paymentId :: Int64,
    invoiceUuid :: InvoiceId,
    providerChargeRef :: Text,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    amount :: Int64,
    currency :: Text,
    chargedAt :: UTCTime
  }
  deriving (Show)

-- every issuance covers one month and is written beside exactly one debit(badge) row
data BadgeIssuance = BadgeIssuance
  { issuanceId :: Text,
    badgePurchaseId :: Int64,
    badgeType :: BadgeType,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    expiry :: UTCTime,
    entryId :: Int64,
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

-- | One badge as the badge surfaces render it. The purchase keys are deliberately absent: this
-- travels to the UI and over remote control, and they are secrets that stay in core.
data BadgeState = BadgeState
  { badgePurchaseId :: Int64,
    badgeType :: BadgeType,
    monthsLeft :: Int,
    paidThrough :: UTCTime,
    shown :: Bool,
    alert :: Maybe BadgeAlert
  }
  deriving (Show)

-- TODO [badges] a second code while one is active - supersede the old purchase, refuse, or credit
-- the balance? Each leaves one badge per profile, reducing this to BadgeState with no shown flag
data UserBadgeState = UserBadgeState
  { badges :: [BadgeState],
    shownBadgeId :: Maybe Int64,
    -- payments returns here with the payment types, which this slice neither writes nor encodes
    monthsLeft :: Int,
    paidThrough :: Maybe UTCTime,
    renewsAt :: Maybe UTCTime,
    willRenew :: Bool,
    alert :: Maybe BadgeAlert
  }
  deriving (Show)

instance TextEncoding BadgePurchaseStatus where
  textEncode = \case
    PSAcquiring -> "acquiring"
    PSIssued -> "issued"
    PSSuperseded -> "superseded"
    PSFailed -> "failed"
  textDecode = \case
    "acquiring" -> Just PSAcquiring
    "issued" -> Just PSIssued
    "superseded" -> Just PSSuperseded
    "failed" -> Just PSFailed
    _ -> Nothing

instance FromField BadgePurchaseStatus where fromField = fromTextField_ textDecode

instance ToField BadgePurchaseStatus where toField = toField . textEncode

instance TextEncoding BadgeCodePaymentStatus where
  textEncode = \case
    CPSPaid -> "paid"
    CPSUnpaid -> "unpaid"
    CPSFree -> "free"
  textDecode = \case
    "paid" -> Just CPSPaid
    "unpaid" -> Just CPSUnpaid
    "free" -> Just CPSFree
    _ -> Nothing

instance FromField BadgeCodePaymentStatus where fromField = fromTextField_ textDecode

instance ToField BadgeCodePaymentStatus where toField = toField . textEncode

$(JQ.deriveJSON (enumJSON $ dropPrefix "BIS") ''BadgeItemStatus)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "OD") ''OfferDiscount)

instance ToJSON BadgeAlertKind where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON BadgeAlertKind where
  parseJSON = textParseJSON "BadgeAlertKind"

$(JQ.deriveJSON defaultJSON ''BadgeAlert)

$(JQ.deriveJSON defaultJSON ''BadgeState)

$(JQ.deriveJSON defaultJSON ''UserBadgeState)
