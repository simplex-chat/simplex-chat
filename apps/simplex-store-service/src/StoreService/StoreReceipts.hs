{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.StoreReceipts
  ( VerifiedStoreTransaction (..),
    StoreRefusal (..),
    StoreVerifier (..),
    StoreReceipt (..),
    noStoreVerifier,
    toStoreReceipt,
  )
where

import Control.Exception (evaluate)
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
    paid :: Maybe (CurrencyAmount, Text), -- in minor units; Google's purchase record carries no price
    alreadyAcknowledged :: Bool -- Play's record says acknowledged or consumed; other stores have nothing to acknowledge
  }
  deriving (Eq, Show)

-- | The reasons are for the service's log alone and must never quote the receipt.
-- SRInvalid and SRTestPurchase are terminal: the client drops the purchase's keys and nothing
-- presents it again. So only a verdict no later attempt could change is either; when in doubt, SRUnreachable.
data StoreRefusal
  = SRInvalid Text -- a verdict that cannot change, and the client consumes the purchase: forged, malformed, refunded
  | SRTestPurchase -- the store vouched for it, but it cost nothing; terminal, and named so the app can say why
  | SRPending -- a real purchase the store has not settled; it may yet
  | SRUnreachable Text -- no verdict: the store was not asked, did not answer, or does not know the token (a Play 404 may be lag)
  | SRVerifierFailed Text -- this service's own failure, its bug or misconfiguration, never the store's answer
  | SRNotConfigured -- no verifier for this store is deployed; the purchase may be real
  deriving (Eq, Show)

-- | Apple signs its receipt and ships the certificate chain in it, so it is verified with no network
-- call; a Play token is opaque and must be asked about, which is why only the Play fields are in IO.
data StoreVerifier = StoreVerifier
  { -- the JWS
    verifyApple :: Maybe (Text -> Either StoreRefusal VerifiedStoreTransaction),
    verifyGoogle :: Maybe (Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)), -- the product id and the token
    acknowledgeGoogle :: Maybe (Text -> Text -> IO (Either StoreRefusal ())),
    -- microseconds; a lane answers one request at a time, so a verifier that does not finish holds up the rest of its lane
    verifyTimeout :: Int
  }

-- | A store payment, named by the store's own reference before anything is verified.
data StoreReceipt = StoreReceipt
  { txRef :: StoreTransactionRef,
    verifyReceipt :: IO (Either StoreRefusal VerifiedStoreTransaction),
    acknowledgeReceipt :: Maybe (VerifiedStoreTransaction -> IO (Either StoreRefusal ()))
  }

noStoreVerifier :: StoreVerifier
noStoreVerifier = StoreVerifier {verifyApple = Nothing, verifyGoogle = Nothing, acknowledgeGoogle = Nothing, verifyTimeout = 10000000}

-- | Nothing for a payment no store made. Exceptions are not logged, since they can quote the receipt
-- or, from Google, a URL holding the token.
toStoreReceipt :: StoreVerifier -> ServicePayment -> Maybe (Either StoreRefusal StoreReceipt)
toStoreReceipt StoreVerifier {verifyApple, verifyGoogle, acknowledgeGoogle, verifyTimeout} = \case
  SPApple {jws} -> Just $ case appleTransactionId jws of
    Nothing -> Left $ SRInvalid "names no transaction"
    Just ref -> Right $ StoreReceipt (StoreTransactionRef PPApple ref) (maybe unconfigured (\verify -> offline $ verify jws) verifyApple) Nothing
  SPGoogle {productId, token}
    -- the claim is the token's hash, so neither string may name any purchase but the one it claims,
    -- whatever path a verifier builds from them
    | not (googleProductId productId) -> Just $ Left $ SRInvalid "not a Play product id"
    -- Play documents no token grammar, so this is our guess, and refusing to ask Play is not its verdict
    | not (googleToken token) -> Just $ Left $ SRUnreachable $ "a Play token this service will not send: " <> tokenShape token
    | otherwise ->
        Just $ Right $
          StoreReceipt
            (StoreTransactionRef PPGoogle (googlePurchaseRef token))
            (maybe unconfigured (\verify -> online "google verifier" $ verify productId token) verifyGoogle)
            ((\acknowledge VerifiedStoreTransaction {alreadyAcknowledged} -> if alreadyAcknowledged then pure (Right ()) else online "acknowledging: google" $ acknowledge productId token) <$> acknowledgeGoogle)
  SPInvoice {} -> Nothing
  SPReceipt {} -> Nothing
  where
    unconfigured = pure $ Left SRNotConfigured
    -- nothing was fetched, so a throw or an overrun is this service's failure or a shape it does not know, never an outage
    offline verdict =
      (fromMaybe (Left $ SRVerifierFailed "apple verifier timed out") <$> timeout verifyTimeout (forced verdict))
        `catchOwn'` \_ -> pure $ Left $ SRVerifierFailed "apple verifier threw"
    online what call =
      (fromMaybe (Left $ SRUnreachable $ what <> " timed out") <$> timeout verifyTimeout (call >>= forced))
        `catchOwn'` \_ -> pure $ Left $ SRUnreachable $ what <> " threw"
    -- a verdict holding a thunk that throws would otherwise throw later, outside these handlers
    forced = either (fmap Left . evaluate) (fmap Right . evaluate)

googleProductId :: Text -> Bool
googleProductId pid = case T.uncons pid of
  Just (c, _) -> T.length pid <= 150 && (isAsciiLower c || isDigit c) && T.all (\x -> isAsciiLower x || isDigit x || x == '_' || x == '.') pid
  Nothing -> False

-- | Not empty or only dots, which a URL path resolves away.
googleToken :: Text -> Bool
googleToken t = not (T.all (== '.') t) && T.length t <= 4096 && T.all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '.' || x == '_' || x == '-') t

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
