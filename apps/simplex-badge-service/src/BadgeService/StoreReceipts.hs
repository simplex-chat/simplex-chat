{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.StoreReceipts
  ( StoreTransaction (..),
    StoreRefusal (..),
    StoreVerifier (..),
    noStoreVerifier,
    storeVerification,
  )
where

import Data.Text (Text)
import Simplex.Chat.PaymentService (ServicePayment (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount, PaymentProvider (..))
import Simplex.Messaging.Util (catchOwn')

-- | What a store vouches for about one completed transaction. providerRef is its stable id and is
-- stored, so it is never a bearer secret: Apple's transactionId, a hash of Google's purchase token.
data StoreTransaction = StoreTransaction
  { providerRef :: Text,
    productId :: Text,
    quantity :: Int,
    paid :: Maybe (CurrencyAmount, Text) -- in minor units; Google's purchase record carries no price
  }
  deriving (Eq, Show)

-- | The reasons are for the service's log alone and must never quote the receipt.
data StoreRefusal
  = SRInvalid Text -- the store does not vouch for it: forged, malformed, another app's, unknown or refunded
  | SRPending -- a real purchase the store has not settled; it may yet
  | SRUnreachable Text -- the store was not asked, or did not answer
  deriving (Eq, Show)

-- | Not a Provider: a receipt is presented once as proof, with nothing to create, watch or cancel.
-- A verifier answers SRInvalid only on the store's own word, never for failing to reach it.
data StoreVerifier = StoreVerifier
  { verifyApple :: Text -> IO (Either StoreRefusal StoreTransaction), -- the JWS
    verifyGoogle :: Text -> Text -> IO (Either StoreRefusal StoreTransaction) -- the product id and the token
  }

noStoreVerifier :: StoreVerifier
noStoreVerifier = StoreVerifier {verifyApple = \_ -> unconfigured, verifyGoogle = \_ _ -> unconfigured}
  where
    unconfigured = pure $ Left $ SRUnreachable "no verifier configured"

-- | Nothing for a payment no store made. A verifier that throws was not answered, so it is
-- unreachable, never invalid; its exception is not logged, since it can hold a URL with the token.
storeVerification :: StoreVerifier -> ServicePayment -> Maybe (PaymentProvider, IO (Either StoreRefusal StoreTransaction))
storeVerification StoreVerifier {verifyApple, verifyGoogle} = \case
  SPApple {jws} -> Just (PPApple, answered $ verifyApple jws)
  SPGoogle {productId, token} -> Just (PPGoogle, answered $ verifyGoogle productId token)
  SPInvoice {} -> Nothing
  SPReceipt {} -> Nothing
  where
    answered verify = verify `catchOwn'` \_ -> pure $ Left $ SRUnreachable "verifier threw"
