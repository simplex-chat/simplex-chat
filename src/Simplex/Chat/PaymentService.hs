{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.PaymentService
  ( ServiceInvoice (..),
    ServicePayment (..),
    appleTransactionId,
    googlePurchaseRef,
    module Simplex.Chat.PaymentService.Types,
  ) where

import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteString.Base64.URL as B64U
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8)

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
  | SPGoogle {productId :: Text, token :: Text} -- the Publisher API finds a purchase by product and token
  | SPInvoice {invoiceId :: InvoiceId}
  | SPReceipt {receipt :: Text} -- transfer of unissued months
  deriving (Show)

-- | Read without verifying the signature: it names the transaction, and proves nothing about it.
appleTransactionId :: Text -> Maybe Text
appleTransactionId signed = case T.splitOn "." signed of
  [_, payload, _] -> do
    J.Object o <- J.decodeStrict' =<< eitherToMaybe (B64U.decodeUnpadded $ encodeUtf8 payload)
    J.String txId <- JM.lookup "transactionId" o
    pure txId
  _ -> Nothing

-- | A purchase token is a bearer secret, so a purchase is named by its hash.
googlePurchaseRef :: Text -> Text
googlePurchaseRef = safeDecodeUtf8 . strEncode . C.sha256Hash . encodeUtf8

$(JQ.deriveJSON defaultJSON ''ServiceInvoice)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SP") ''ServicePayment)
