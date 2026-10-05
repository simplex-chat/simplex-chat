{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.StoreReceipts
  ( VerifiedStoreTransaction (..),
    StoreRefusal (..),
    StoreVerifier (..),
    StoreReceipt (..),
    noStoreVerifier,
    toStoreReceipt,
  )
where

import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isControl, isDigit, isPrint, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.PaymentService (ServicePayment (..), appleTransactionId, googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (CurrencyAmount, PaymentProvider (..), StoreTransactionRef (..))
import Simplex.Messaging.Util (catchOwn', tshow)
import System.Timeout (timeout)

-- | What a store vouches for about one completed transaction.
-- TODO [badges] PaymentFunding's PFApple and PFGoogle sketch the same thing and have no producer;
-- delete them, or build them out and reconcile.
data VerifiedStoreTransaction = VerifiedStoreTransaction
  { transactionRef :: Text, -- from what was verified: Apple's transactionId, googlePurchaseRef of the token asked about
    productId :: Text,
    quantity :: Int, -- as the store reported it; the service refuses any but 1, as the apps never buy more
    -- costs the buyer nothing: Apple's Sandbox, which a public TestFlight build buys in, and Google's license testers
    testPurchase :: Bool,
    paid :: Maybe (CurrencyAmount, Text) -- in minor units; Google's purchase record carries no price
  }
  deriving (Eq, Show)

-- | The reasons are for the service's log alone and must never quote the receipt.
-- SRInvalid alone is terminal: the client drops the purchase's keys and nothing presents it again.
-- So only a verdict no later attempt could change is SRInvalid; when in doubt, SRUnreachable.
data StoreRefusal
  = SRInvalid Text -- a verdict that cannot change, and the client consumes the purchase: forged, malformed, another app's, refunded
  | SRPending -- a real purchase the store has not settled; it may yet
  | SRUnreachable Text -- no verdict: the store was not asked, did not answer, or does not know the token (a Play 404 may be lag)
  | SRVerifierFailed Text -- a bug, not the store's answer
  | SRNotConfigured -- no verifier for this store is deployed; the purchase may be real
  deriving (Eq, Show)

-- | Apple signs its receipt and ships the certificate chain in it, so it is verified with no network
-- call; a Play token is opaque and must be asked about, which is why only that field is in IO.
data StoreVerifier = StoreVerifier
  { -- the JWS; every Left becomes SRInvalid, so a verifier that cannot reach a verdict throws instead
    verifyApple :: Maybe (Text -> Either Text VerifiedStoreTransaction),
    verifyGoogle :: Maybe (Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)), -- the product id and the token
    -- microseconds; requests are answered one at a time, so a verifier that does not finish holds up every other one
    verifyTimeout :: Int
  }

-- | A store payment, named by the store's own reference before anything is verified.
data StoreReceipt = StoreReceipt
  { txRef :: StoreTransactionRef,
    verifyReceipt :: IO (Either StoreRefusal VerifiedStoreTransaction)
  }

noStoreVerifier :: StoreVerifier
noStoreVerifier = StoreVerifier {verifyApple = Nothing, verifyGoogle = Nothing, verifyTimeout = 10000000}

-- | Nothing for a payment no store made. Exceptions are not logged, since they can quote the receipt
-- or, from Google, a URL holding the token.
toStoreReceipt :: StoreVerifier -> ServicePayment -> Maybe (Either StoreRefusal StoreReceipt)
toStoreReceipt StoreVerifier {verifyApple, verifyGoogle, verifyTimeout} = \case
  SPApple {jws} -> Just $ case appleTransactionId jws of
    Nothing -> Left $ SRInvalid "names no transaction"
    Just ref -> Right $ StoreReceipt (StoreTransactionRef PPApple ref) $ maybe unconfigured (\verify -> offline $ first SRInvalid $ verify jws) verifyApple
  SPGoogle {productId, token}
    -- the claim is the token's hash, so neither string may name any purchase but the one it claims,
    -- whatever path a verifier builds from them
    | not (googleProductId productId) -> Just $ Left $ SRInvalid "not a Play product id"
    -- Play documents no token grammar, so this is our guess, and refusing to ask Play is not its verdict
    | not (googleToken token) -> Just $ Left $ SRUnreachable $ "a Play token this service will not send: " <> tokenShape token
    | otherwise -> Just $ Right $ StoreReceipt (StoreTransactionRef PPGoogle (googlePurchaseRef token)) $ maybe unconfigured (\verify -> online $ verify productId token) verifyGoogle
  SPInvoice {} -> Nothing
  SPReceipt {} -> Nothing
  where
    unconfigured = pure $ Left SRNotConfigured
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

-- | What the log may say about a token: its length and the kinds of character in it, never any of them.
tokenShape :: Text -> Text
tokenShape t = tshow (T.length t) <> " characters: " <> T.intercalate ", " [kind | (kind, isKind) <- kinds, T.any isKind t]
  where
    kinds =
      [ ("lowercase", isAsciiLower),
        ("uppercase", isAsciiUpper),
        ("digits", isDigit),
        ("'.'", (== '.')),
        ("'_'", (== '_')),
        ("'-'", (== '-')),
        ("other printable ASCII", \c -> isAscii c && isPrint c && not (isAlphaNum c) && c `notElem` ("._-" :: String)),
        ("whitespace or control", \c -> isSpace c || isControl c),
        ("non-ASCII", not . isAscii)
      ]
