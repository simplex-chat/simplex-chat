{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.StoreReceipts
  ( StoreTransaction (..),
    StoreEnvironment (..),
    StoreRefusal (..),
    StoreVerifier (..),
    StoreReceipt (..),
    noStoreVerifier,
    storeReceipt,
  )
where

import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.PaymentService (ServicePayment (..), appleTransactionId, googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (CurrencyAmount, PaymentProvider (..))
import Simplex.Messaging.Util (catchOwn')
import System.Timeout (timeout)

-- | What a store vouches for about one completed transaction.
data StoreTransaction = StoreTransaction
  { productId :: Text,
    quantity :: Int,
    environment :: StoreEnvironment,
    paid :: Maybe (CurrencyAmount, Text) -- in minor units; Google's purchase record carries no price
  }
  deriving (Eq, Show)

-- | A test purchase costs the buyer nothing: Apple's Sandbox, which a public TestFlight build buys
-- in, and Google's license testers.
data StoreEnvironment = SEProduction | SETest
  deriving (Eq, Show)

-- | The reasons are for the service's log alone and must never quote the receipt.
data StoreRefusal
  = SRInvalid Text -- the store does not vouch for it: forged, malformed, another app's, unknown or refunded
  | SRPending -- a real purchase the store has not settled; it may yet
  | SRUnreachable Text -- the store was not asked, or did not answer
  | SRVerifierFailed Text -- a bug, not the store's answer
  deriving (Eq, Show)

-- | Not a Provider: a receipt is presented once as proof, with nothing to create, watch or cancel.
-- Apple is checked offline, so its verifier is pure and cannot be unreachable; only Google is asked.
-- A store with no verifier deployed is unreachable: its purchases may be real.
data StoreVerifier = StoreVerifier
  { verifyApple :: Maybe (Text -> Either Text StoreTransaction), -- the JWS; Left is why Apple did not sign it
    verifyGoogle :: Maybe (Text -> Text -> IO (Either StoreRefusal StoreTransaction)), -- the product id and the token
    -- microseconds; requests are answered one at a time, so a verifier that does not finish holds up every other one
    verifyTimeout :: Int
  }

-- | A store payment, named by the store's own reference before anything is verified.
data StoreReceipt = StoreReceipt
  { provider :: PaymentProvider,
    providerRef :: Text,
    verifyReceipt :: IO (Either StoreRefusal StoreTransaction)
  }

noStoreVerifier :: StoreVerifier
noStoreVerifier = StoreVerifier {verifyApple = Nothing, verifyGoogle = Nothing, verifyTimeout = 10000000}

-- | Nothing for a payment no store made. Exceptions are not logged, since they can quote the receipt
-- or, from Google, a URL holding the token.
storeReceipt :: StoreVerifier -> ServicePayment -> Maybe (Either StoreRefusal StoreReceipt)
storeReceipt StoreVerifier {verifyApple, verifyGoogle, verifyTimeout} = \case
  SPApple {jws} -> Just $ case appleTransactionId jws of
    Nothing -> Left $ SRInvalid "names no transaction"
    Just ref -> Right $ StoreReceipt PPApple ref $ maybe unconfigured (\verify -> offline $ first SRInvalid $ verify jws) verifyApple
  SPGoogle {productId, token}
    -- the claim is the token's hash, so neither string may name any purchase but the one it claims,
    -- whatever path a verifier builds from them
    | not (googleProductId productId && googleToken token) -> Just $ Left $ SRInvalid "not a Play product id and token"
    | otherwise -> Just $ Right $ StoreReceipt PPGoogle (googlePurchaseRef token) $ maybe unconfigured (\verify -> online $ verify productId token) verifyGoogle
  SPInvoice {} -> Nothing
  SPReceipt {} -> Nothing
  where
    unconfigured = pure $ Left $ SRUnreachable "no verifier configured"
    -- nothing was fetched, so a throw or an overrun is a bug or a malformed receipt, never an outage
    offline verdict =
      (fromMaybe (Left $ SRVerifierFailed "apple verifier timed out") <$> timeout verifyTimeout (forced verdict))
        `catchOwn'` \_ -> pure $ Left $ SRVerifierFailed "apple verifier threw"
    online verify =
      (fromMaybe (Left $ SRUnreachable "google verifier timed out") <$> timeout verifyTimeout (verify >>= forced))
        `catchOwn'` \_ -> pure $ Left $ SRUnreachable "google verifier threw"
    -- a verdict holding a thunk that throws would otherwise throw later, outside these handlers
    forced = either (fmap Left . evaluate) (fmap Right . evaluate)

googleProductId :: Text -> Bool
googleProductId pid = case T.uncons pid of
  Just (c, _) -> T.length pid <= 150 && (isAsciiLower c || isDigit c) && T.all (\x -> isAsciiLower x || isDigit x || x == '_' || x == '.') pid
  Nothing -> False

googleToken :: Text -> Bool
googleToken t = not (T.null t) && T.length t <= 4096 && T.all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '.' || x == '_' || x == '-') t
