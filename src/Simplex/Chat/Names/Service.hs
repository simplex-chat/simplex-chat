{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The client's view of the SimpleX names service.
--
-- The service is reached over an ordinary SimpleX connection, so this is a
-- plain request/response interface with no chain access of its own: the app
-- never builds a transaction, reads a nonce or broadcasts anything. It quotes,
-- pays, hands over signed intents, and independently confirms the result by
-- resolving the name.
--
-- 'NamesService' is a record of operations rather than a class so that the mock
-- and the real SMP-backed client are interchangeable at the call site.
module Simplex.Chat.Names.Service
  ( NamesService (..),
    PaymentProof (..),
    BuyRequest (..),
    NameQuote (..),
    PurchaseId (..),
    RegistrationStatus (..),
    NameRecordView (..),
    ServiceError (..),
    serviceErrorText,
  )
where

import Data.ByteString (ByteString)
import Simplex.Chat.Names.Snrc (SignedIntent)
import Simplex.Messaging.Eth.Address (Address)

-- | Proof that the store (or a web checkout) was paid. Mirrors the shape
-- already sketched by 'Simplex.Chat.Badges.BadgePurchase', so the two can share
-- a validator server-side.
data PaymentProof
  = PPAppleReceipt ByteString
  | PPGoogleToken ByteString
  | PPStripeSession ByteString
  | PPRedeemCode ByteString
  deriving (Eq, Show)

data BuyRequest = BuyRequest
  { brLabel :: ByteString, -- ^ the label only, without the TLD
    brOwner :: Address, -- ^ the profile's derived address
    -- | 1-10 years, bought outright. There is no subscription: extension is
    -- another purchase.
    brYears :: Int,
    brPayment :: PaymentProof,
    brContactLink :: Maybe ByteString,
    brChannelLink :: Maybe ByteString
  }
  deriving (Eq, Show)

data NameQuote = NameQuote
  { nqLabel :: ByteString,
    nqAvailable :: Bool,
    nqPriceCents :: Int,
    nqYears :: Int
  }
  deriving (Eq, Show)

newtype PurchaseId = PurchaseId ByteString
  deriving (Eq, Ord, Show)

data RegistrationStatus
  = -- | committed on-chain, waiting out minCommitmentAge
    RegPending
  | RegConfirmed {rsTxHash :: ByteString, rsExpires :: Integer}
  | RegFailed ByteString
  deriving (Eq, Show)

-- | What resolution returns — the same fields the SMP @RSLV@ path already
-- carries, which is how the app confirms a purchase without trusting the
-- service.
data NameRecordView = NameRecordView
  { nrvName :: ByteString,
    nrvOwner :: Address,
    nrvContact :: [ByteString],
    nrvChannel :: [ByteString],
    nrvExpires :: Integer,
    -- | Relayed record edits still available on this name. Granted at
    -- registration and renewal, consumed only by the sponsored path.
    nrvEditCredits :: Integer
  }
  deriving (Eq, Show)

data ServiceError
  = SEUnavailable ByteString
  | SENameTaken
  | SENameInvalid ByteString
  | SEPaymentRejected ByteString
  | SEBadSignature
  | SENotOwner
  | SEBadNonce
  | SEExpiredIntent
  | SENotFound
  | -- | The relayer has no registration credits left. A hard service stop:
    -- only the beneficiary multisig can grant more.
    SENoRegistrarCredits
  | -- | This name's relayed-edit allowance is exhausted until renewal.
    SENoEditCredits
  deriving (Eq, Show)

serviceErrorText :: ServiceError -> ByteString
serviceErrorText = \case
  SEUnavailable e -> "service unavailable: " <> e
  SENameTaken -> "that name is already taken"
  SENameInvalid e -> "invalid name: " <> e
  SEPaymentRejected e -> "payment rejected: " <> e
  SEBadSignature -> "signature did not verify"
  SENotOwner -> "not the owner of that name"
  SEBadNonce -> "wrong nonce, refresh and retry"
  SEExpiredIntent -> "the signed request expired"
  SENotFound -> "name not found"
  SENoRegistrarCredits -> "the registration service is out of credits, please report this"
  SENoEditCredits -> "no record changes left for this name until you extend it"

data NamesService = NamesService
  { quoteName :: ByteString -> IO (Either ServiceError NameQuote),
    buyName :: BuyRequest -> IO (Either ServiceError PurchaseId),
    registrationStatus :: PurchaseId -> IO (Either ServiceError RegistrationStatus),
    -- | Hand a user-signed intent to the relayer, which pays the gas.
    relayIntent :: SignedIntent -> IO (Either ServiceError ByteString),
    -- | Independent confirmation path; in production this is SMP @RSLV@.
    resolveName :: ByteString -> IO (Either ServiceError NameRecordView),
    -- | Owner to names, for recovery-key import. Advisory: each name is
    -- confirmed with 'resolveName', so a lying service can withhold names but
    -- cannot invent them.
    namesOwnedBy :: Address -> IO (Either ServiceError [ByteString]),
    currentNonce :: Address -> IO (Either ServiceError Integer),
    -- | Relayed edits left on a name, for display before the user tries one.
    editCreditsFor :: ByteString -> IO (Either ServiceError Integer)
  }
