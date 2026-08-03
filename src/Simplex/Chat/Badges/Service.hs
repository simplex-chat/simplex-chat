{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    BadgeCatalog,
    BadgeStatement,
    BadgeBalance,
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8, Word16, Word32)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Store
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Version (Version (..), VersionScope)

data BadgeServiceVersion

instance VersionScope BadgeServiceVersion

type VersionBadgeService = Version BadgeServiceVersion

pattern VersionBadgeService :: Word16 -> VersionBadgeService
pattern VersionBadgeService v = Version v

data BadgeServiceRequest = BadgeServiceRequest
  { version :: VersionBadgeService,
    purchaseKey :: Maybe C.PublicKeyEd25519, -- absent for BSCGetBadgeCatalog, required for other commands
    request :: BadgeServiceCommand
  }

data BadgeServiceCommand
  = BSCGetBadgeCatalog
  | BSCGetBadgeInvoice
      { offerId :: BadgeOfferId,
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
      { catalog :: BadgeCatalog
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
  | BSPNewBadge
      { credential :: BadgeCredential,
        receipt :: Maybe Text, -- not provided for lifetime badges
        statement :: BadgeStatement
      }
  | BSPBadgeCredential
      { credential :: Maybe BadgeCredential, -- Nothing when no balance to issueBadge or no current credential for pause/transfer
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

data BadgeCatalog

data BadgeStatement

data BadgeBalance

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
  deriving (Eq, Show)
