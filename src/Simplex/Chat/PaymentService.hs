{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.PaymentService
  ( ServiceInvoice (..),
    ServicePayment (..),
    AppleTransactionFacts (..),
    applePayload,
    appleTransactionRef,
    applePayloadRef,
    appleTransactionFacts,
    googlePurchaseRef,
    module Simplex.Chat.PaymentService.Types,
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Base64.URL as B64U
import Data.Int (Int64)
import Data.Maybe (isJust)
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
applePayload :: Text -> Maybe J.Object
applePayload signed = case T.splitOn "." signed of
  [_, payload, _] -> do
    J.Object o <- J.decodeStrict' =<< eitherToMaybe (B64U.decodeUnpadded $ encodeUtf8 payload)
    Just o
  _ -> Nothing

appleTransactionRef :: Text -> Maybe Text
appleTransactionRef = applePayloadRef <=< applePayload

-- | What an unverified payload says about its transaction, which decides only which payment a held record
-- presents next: the service verifies whatever it is sent.
data AppleTransactionFacts = AppleTransactionFacts
  { transactionId :: Text,
    purchaseDate :: Maybe Int64, -- epoch milliseconds
    expiresDate :: Maybe Int64, -- epoch milliseconds
    revoked :: Bool,
    autoRenewable :: Bool
  }
  deriving (Eq, Show)

-- | A date or type that does not read is taken as absent rather than failing the whole payload.
appleTransactionFacts :: Text -> Maybe AppleTransactionFacts
appleTransactionFacts = JT.parseMaybe facts <=< applePayload
  where
    facts o = do
      transactionId <- o J..: "transactionId"
      purchaseDate <- o J..:? "purchaseDate" <|> pure Nothing
      expiresDate <- o J..:? "expiresDate" <|> pure Nothing
      -- as the service reads it: null is not revoked
      revoked <- isJust <$> (o J..:? "revocationDate" :: JT.Parser (Maybe J.Value))
      type_ <- o J..:? "type" <|> pure Nothing
      pure AppleTransactionFacts {transactionId, purchaseDate, expiresDate, revoked, autoRenewable = type_ == Just ("Auto-Renewable Subscription" :: Text)}

-- | A subscription is named by its original transaction id, which every renewal keeps, so all its
-- charges claim one purchase; any other transaction by its own id.
applePayloadRef :: J.Object -> Maybe Text
applePayloadRef o = case JM.lookup "type" o of
  Just (J.String "Auto-Renewable Subscription") -> field "originalTransactionId"
  _ -> field "transactionId"
  where
    field k = case JM.lookup k o of
      Just (J.String t) -> Just t
      _ -> Nothing

-- | A purchase token is a bearer secret, so a purchase is named by its hash.
googlePurchaseRef :: Text -> Text
googlePurchaseRef = safeDecodeUtf8 . strEncode . C.sha256Hash . encodeUtf8

$(JQ.deriveJSON defaultJSON ''ServiceInvoice)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "SP") ''ServicePayment)
