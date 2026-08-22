{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.PaymentService
  ( ServiceInvoice (..),
    ServicePayment (..),
    module Simplex.Chat.PaymentService.Types,
  ) where

import qualified Data.Aeson.TH as JQ
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)

data ServiceInvoice = ServiceInvoice
  { invoiceId :: InvoiceId,
    price :: CurrencyAmount,
    discount :: Maybe CurrencyAmount, -- discount amount from the price
    credit :: Maybe CurrencyAmount, -- credit for upgrade
    amount :: CurrencyAmount, -- price - discount - credit
    currency :: Text,
    expiresAt :: UTCTime,
    paymentTo :: ServicePaymentDestination
  }
  deriving (Show)

data ServicePayment
  = SPApple {jws :: Text}
  | SPGoogle {token :: Text}
  | SPInvoice {invoiceId :: InvoiceId}
  | SPCode {code :: Text}
  | SPReceipt {receipt :: Text} -- transfer of unissued months
  deriving (Show)

-- JSON

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SP") ''ServicePayment)

$(JQ.deriveJSON defaultJSON ''ServiceInvoice)
