{-# LANGUAGE DuplicateRecordFields #-}

module Simplex.Chat.Badges.Service
  ( BadgeServiceRequest (..),
    BadgeServiceCommand (..),
    ServicePaymentMethod (..),
    CardProvider (..),
    CryptoCurrency (..),
    ServicePayment (..),
    BadgeServiceResponse (..),
    ServicePaymentDestination (..),
    BadgeServiceErrorCode (..),
    BadgeCatalog,
    BadgeStatement,
    BadgeBalance,
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges
import qualified Simplex.Messaging.Crypto as C

data BadgeServiceRequest = BadgeServiceRequest
  { version :: Int,
    purchaseKey :: Maybe C.PublicKeyEd25519,
    request :: BadgeServiceCommand
  }

data BadgeServiceCommand
  = BSCGetBadgeCatalog
  | BSCGetBadgeInvoice
      { offerId :: BadgeOfferId,
        badgeInfo :: BadgeInfo,
        paymentVia :: ServicePaymentMethod,
        balance :: Maybe BadgeBalance
      }
  | BSCPurchaseBadge
      { badgeRequest :: BadgeRequest,
        payment :: ServicePayment
      }
  | BSCIssueBadge
      { badgeRequest :: BadgeRequest,
        balance :: BadgeBalance
      }
  | BSCPauseBadge
  | BSCTransferBadge
      { receipt :: Text
      }

data ServicePaymentMethod
  = SPMCard {provider :: CardProvider}
  | SPMCrypto {currency :: CryptoCurrency}
  deriving (Eq, Show)

data CardProvider = CPStripe
  deriving (Eq, Show)

data CryptoCurrency = CCBtc | CCXmr
  deriving (Eq, Show)

data ServicePayment
  = SPApple {jws :: Text}
  | SPGoogle {token :: Text}
  | SPInvoice {invoiceId :: BotPaymentRef}
  | SPCode {code :: Text}
  deriving (Show)

data BadgeServiceResponse
  = BSPBadgeCatalog
      { catalog :: BadgeCatalog
      }
  | BSPBadgeInvoice
      { invoiceId :: BotPaymentRef,
        badgeType :: BadgeType,
        months :: Int,
        price :: Int64,
        discount :: Maybe Int64,
        credit :: Maybe Int64,
        amount :: Int64,
        currency :: Text,
        expiresAt :: UTCTime,
        paymentTo :: ServicePaymentDestination
      }
  | BSPNewBadge
      { credential :: BadgeCredential,
        receipt :: Maybe Text,
        statement :: BadgeStatement
      }
  | BSPBadgeCredential
      { credential :: Maybe BadgeCredential,
        statement :: BadgeStatement
      }
  | BSPError
      { code :: BadgeServiceErrorCode,
        message :: Maybe Text,
        retryAfter :: Maybe Int
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
