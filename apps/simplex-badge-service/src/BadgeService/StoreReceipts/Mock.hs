{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.StoreReceipts.Mock (mockStoreVerifier) where

import BadgeService.StoreReceipts
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.ByteString.Base64.URL as B64U
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.PaymentService (appleTransactionId, googlePurchaseRef)
import Simplex.Messaging.Util (eitherToMaybe)

-- | Vouches for any well-formed receipt without verifying anything. Each transaction is named by
-- the same function its claim is named by, so the two cannot differ.
mockStoreVerifier :: StoreVerifier
mockStoreVerifier = noStoreVerifier {verifyApple = Just mockApple, verifyGoogle = Just mockGoogle}

mockApple :: Text -> Either Text StoreTransaction
mockApple signed = case T.splitOn "." signed of
  [_, payload, _] -> do
    o <- case J.decodeStrict' =<< eitherToMaybe (B64U.decodeUnpadded $ encodeUtf8 payload) of
      Just (J.Object obj) -> Right obj
      _ -> Left "payload is not base64url JSON"
    productId <- case JM.lookup "productId" o of
      Just (J.String p) -> Right p
      _ -> Left "no productId"
    transactionRef <- maybe (Left "no transactionId") Right $ appleTransactionId signed
    Right $ vouched transactionRef productId
  _ -> Left "not three dot-separated parts"

mockGoogle :: Text -> Text -> IO (Either StoreRefusal StoreTransaction)
mockGoogle productId token = pure $ Right $ vouched (googlePurchaseRef token) productId

vouched :: Text -> Text -> StoreTransaction
vouched transactionRef productId =
  -- not SETest, though nothing was paid: a test purchase is refused as receipt_invalid, which is
  -- terminal and drops the client's keys, so every dev purchase would fail for good
  StoreTransaction {transactionRef, productId, quantity = 1, environment = SEProduction, paid = Nothing}
