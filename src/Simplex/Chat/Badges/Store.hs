{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Simplex.Chat.Badges.Store
  ( BadgePriceId (..),
    BadgeOfferId (..),
    InvoiceId (..),
    BadgePlan (..),
    BadgeItemStatus (..),
    OfferDiscount (..),
    PaymentProvider (..),
    BadgePaymentStatus (..),
    BadgePurchaseStatus (..),
    LedgerEntryType (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    BadgeAlertKind (..),
    BadgePurchase (..),
    BadgePayment (..),
    BadgeLedgerEntry (..),
    BadgeCharge (..),
    BadgeIssuance (..),
    BadgeAlert (..),
    UserBadgeState (..),
  ) where

import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Simplex.Chat.Badges hiding (BadgePurchase (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import qualified Simplex.Messaging.Crypto as C

-- confirmed
newtype BadgePriceId = BadgePriceId Text
  deriving newtype (Eq, Show)

-- confirmed
newtype BadgeOfferId = BadgeOfferId Text
  deriving newtype (Eq, Show)

-- to review
newtype InvoiceId = InvoiceId Text
  deriving newtype (Eq, Show)

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
data PaymentProvider = PPApple | PPGoogle | PPStripe | PPCrypto | PPCode | PPReceipt
  deriving (Eq, Show)

-- unconfirmed draft
data BadgePaymentStatus = BPSNew | BPSInvoiced | BPSPending | BPSSettled | BPSFailed | BPSExpired
  deriving (Eq, Show)

-- unconfirmed draft
data BadgePurchaseStatus = PSAcquiring | PSIssued | PSSuperseded | PSFailed
  deriving (Eq, Show)

-- confirmed
data LedgerEntryType = LECredit {credit :: LedgerCreditType} | LEDebit {debit :: LedgerDebitType}
  deriving (Eq, Show)

-- confirmed
data LedgerCreditType
  = CTPayment {invoiceId :: Int64}
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
    paymentId :: Int64,
    status :: BadgePurchaseStatus,
    credential :: Maybe BadgeCredential,
    alertAcked :: Maybe (BadgeAlertKind, Text),
    alertSnoozeUntil :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

-- to review
data BadgePayment = BadgePayment
  { paymentId :: Int64,
    userId :: UserId,
    purchaseKey :: C.PublicKeyEd25519,
    badgeType :: BadgeType,
    priceId :: Maybe BadgePriceId,
    offerId :: Maybe BadgeOfferId,
    invoiceUuid :: Maybe InvoiceId,
    months :: Maybe Int,
    amount :: Maybe Int64,
    currency :: Maybe Text,
    provider :: PaymentProvider,
    providerRef :: Maybe Text,
    invoiceUrl :: Maybe Text,
    invoiceAddress :: Maybe Text,
    invoiceCryptoAmount :: Maybe Text,
    invoiceExpiresAt :: Maybe UTCTime,
    evidence :: Maybe ByteString,
    receiptCode :: Maybe Text,
    status :: BadgePaymentStatus,
    exception :: Maybe Text,
    renewsAt :: Maybe UTCTime,
    graceUntil :: Maybe UTCTime,
    cancelled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

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

-- unconfirmed draft
data BadgeIssuance = BadgeIssuance
  { issuanceId :: Int64,
    badgePurchaseId :: Int64,
    periodStart :: Maybe UTCTime,
    periodEnd :: Maybe UTCTime,
    expiry :: Maybe UTCTime,
    entryId :: Maybe Int64,
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
    payments :: [BadgePayment],
    monthsLeft :: Int,
    paidThrough :: Maybe UTCTime,
    renewsAt :: Maybe UTCTime,
    willRenew :: Bool,
    alert :: Maybe BadgeAlert
  }
