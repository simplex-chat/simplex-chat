{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    BadgeServiceErrorCode (..),
    BadgeIssueFailure (..),
    BadgeIssueError (..),
    BadgeFunding (..),
    BadgePurchase (..),
    BadgeLedgerEntry (..),
    BadgeCharge (..),
    BadgeIssuance (..),
    BadgeAlert (..),
    BadgeAlertPrice (..),
    BadgeState (..),
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Simplex.Chat.Badges hiding (BadgePurchase (..))
import Simplex.Chat.PaymentService.Types (InvoiceId, PaymentId, StoredPayment)
import Simplex.Chat.Types (BoolDef (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, sumTypeJSON, taggedObjectJSON)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8)
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
data BadgeAlertKind = BARenewalApproaching | BAPaymentIssue | BASubscriptionEnded | BAPrepaidEnding | BASupportEnded | BAIssueFailed
  deriving (Eq, Show)

instance TextEncoding BadgeAlertKind where
  textEncode = \case
    BARenewalApproaching -> "renewal_approaching"
    BAPaymentIssue -> "payment_issue"
    BASubscriptionEnded -> "subscription_ended"
    BAPrepaidEnding -> "prepaid_ending"
    BASupportEnded -> "support_ended"
    BAIssueFailed -> "issue_failed"
  textDecode = \case
    "renewal_approaching" -> Just BARenewalApproaching
    "payment_issue" -> Just BAPaymentIssue
    "subscription_ended" -> Just BASubscriptionEnded
    "prepaid_ending" -> Just BAPrepaidEnding
    "support_ended" -> Just BASupportEnded
    "issue_failed" -> Just BAIssueFailed
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
    price :: Maybe BadgeAlertPrice
  }
  deriving (Show)

data BadgeAlertPrice = BadgeAlertPrice
  { amount :: Int64,
    currency :: Text
  }
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

-- | Why a renewal request ended without a credential stored. retryable is the service's own view of
-- transience: it gave retryAfter, which holds for codes this version does not know.
data BadgeIssueFailure
  = BIFServiceError {code :: BadgeServiceErrorCode, retryable :: Bool}
  | BIFTimeout
  | BIFNetwork
  | BIFInvalidCredential
  | BIFUnexpected {message :: Text}
  deriving (Eq, Show)

data BadgeIssueError = BadgeIssueError
  { failedSince :: UTCTime,
    lastAttemptAt :: UTCTime,
    reason :: BadgeIssueFailure
  }
  deriving (Eq, Show)

-- | The user's badge as the badge surfaces render it. The private purchase key is deliberately
-- absent: this travels to the UI and over remote control, and it is a secret that stays in core.
data BadgeState = BadgeState
  { badgePurchaseId :: Int64,
    purchaseKey :: C.PublicKeyEd25519, -- the purchase's identifier on the service
    badgeType :: BadgeType,
    shown :: BoolDef,
    monthsLeft :: Int,
    paidThrough :: UTCTime,
    -- payments returns here with the payment types, which this slice neither writes nor encodes
    renewsAt :: Maybe UTCTime,
    willRenew :: Bool,
    alert :: Maybe BadgeAlert,
    issueError :: Maybe BadgeIssueError,
    nextWakeAt :: Maybe UTCTime
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

instance StrEncoding BadgeIssueFailure where
  strEncode = \case
    BIFServiceError {code, retryable} -> "service_error " <> (if retryable then "retry " else "final ") <> encodeUtf8 (textEncode code)
    BIFTimeout -> "timeout"
    BIFNetwork -> "network"
    BIFInvalidCredential -> "invalid_credential"
    BIFUnexpected {message} -> "unexpected " <> encodeUtf8 message
  strP =
    A.takeWhile1 (/= ' ') >>= \case
      "service_error" -> serviceErrorP
      "timeout" -> pure BIFTimeout
      "network" -> pure BIFNetwork
      "invalid_credential" -> pure BIFInvalidCredential
      "unexpected" -> BIFUnexpected . safeDecodeUtf8 <$> (A.space *> A.takeByteString)
      _ -> fail "bad BadgeIssueFailure"
    where
      -- the code is encoded last because a code this version does not know keeps the service's own
      -- text, which may contain a space
      serviceErrorP = do
        retryable_ <- A.space *> retryableP
        code_ <- A.space *> codeP
        pure BIFServiceError {code = code_, retryable = retryable_}
      retryableP =
        A.takeWhile1 (/= ' ') >>= \case
          "retry" -> pure True
          "final" -> pure False
          _ -> fail "bad BadgeIssueFailure retry flag"
      codeP = A.takeByteString >>= maybe (fail "bad BadgeServiceErrorCode") pure . textDecode . safeDecodeUtf8

instance ToField BadgeIssueFailure where toField = toField . decodeLatin1 . strEncode

instance FromField BadgeIssueFailure where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

$(JQ.deriveJSON (enumJSON $ dropPrefix "BIS") ''BadgeItemStatus)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "OD") ''OfferDiscount)

$(JQ.deriveJSON (enumJSON $ dropPrefix "BA") ''BadgeAlertKind)

$(JQ.deriveJSON defaultJSON ''BadgeAlertPrice)

$(JQ.deriveJSON defaultJSON ''BadgeAlert)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "BIF") ''BadgeIssueFailure)

$(JQ.deriveJSON defaultJSON ''BadgeIssueError)

$(JQ.deriveJSON defaultJSON ''BadgeState)
