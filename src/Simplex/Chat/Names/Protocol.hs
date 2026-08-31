{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Wire protocol for name registration over the badge service-RPC transport.
--
-- A 'NamesRequest' travels in @APISendServiceRequest.request@ and a
-- 'NamesResponse' comes back in @CRServiceResponse.responseData@ — one response
-- per request. The envelope and both command\/response sums are @type@-tagged
-- JSON objects, the same shape the badge service decodes.
--
-- Registration is commit-then-reveal (see
-- @docs\/rfcs\/2026-08-18-in-app-name-purchase-mvp.md@): 'NRCommit' publishes only
-- @H(name, owner, secret, ttl)@ so the service cannot tell which name it is;
-- 'NRReveal' submits the plaintext once the aged commitment already binds the
-- name to /this/ owner address, so the service cannot front-run it.
module Simplex.Chat.Names.Protocol
  ( NamesVersion,
    currentNamesVersion,
    NameTtl,
    NamesRequest (..),
    NamesCommand (..),
    NamesResponse (..),
    NamesErrorCode (..),
    Commitment (..),
    NameSecret (..),
    TxHash (..),
    RequestId (..),
    RedemptionCode (..),
    IntentSig (..),
    LabelHash (..),
    NameRegPhase (..),
    mkCommitment,
    mkLabelHash,
    validLabel,
  )
where

import Control.Applicative (optional)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (isHexDigit)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word16, Word32)
import Simplex.Messaging.Encoding (smpEncode)
import Simplex.Messaging.Encoding.String
import qualified Data.Text as T
import Simplex.Chat.Names.Snrc (labelHash)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.Eth.Keccak (keccak256)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, taggedObjectJSON)

-- | Protocol version, negotiated the way the badge service version is.
type NamesVersion = Word16

currentNamesVersion :: NamesVersion
currentNamesVersion = 1

-- | Name lifetime in seconds. Real min-commitment-age enforcement is deferred.
type NameTtl = Word32

-- | @H(name, owner, secret, ttl)@ — the only thing a commit reveals.
newtype Commitment = Commitment {unCommitment :: ByteString}
  deriving (Eq, Show)

-- | Per-registration nonce, kept secret until reveal so the commitment is opaque.
newtype NameSecret = NameSecret {unSecret :: ByteString}
  deriving (Eq, Show)

-- | A mock transaction hash. Returned now so block-inclusion checks (out of
-- scope) need no protocol change later.
newtype TxHash = TxHash {unTxHash :: ByteString}
  deriving (Eq, Show)

-- | ENS labelhash of a single label: @keccak256("acme")@, not the namehash of
-- the full name. Quoting a name sends only this, so the label a client is about
-- to register is never revealed to the registrar before it is committed.
newtype LabelHash = LabelHash {unLabelHash :: ByteString}
  deriving (Eq, Show)

-- | All of these are on-chain byte values, so they are encoded the way Ethereum
-- writes them — @0x@-prefixed hex — not base64.
hexEncode :: ByteString -> ByteString
hexEncode = ("0x" <>) . BAE.convertToBase BAE.Base16

hexP :: Parser ByteString
hexP = do
  _ <- optional (A.string "0x")
  s <- A.takeWhile1 isHexDigit
  either fail pure $ BAE.convertFromBase BAE.Base16 s

instance StrEncoding Commitment where
  strEncode = hexEncode . unCommitment
  strP = Commitment <$> hexP

instance StrEncoding LabelHash where
  strEncode = hexEncode . unLabelHash
  strP = LabelHash <$> hexP

instance StrEncoding NameSecret where
  strEncode = hexEncode . unSecret
  strP = NameSecret <$> hexP

instance StrEncoding TxHash where
  strEncode = hexEncode . unTxHash
  strP = TxHash <$> hexP

instance ToJSON Commitment where toJSON = strToJSON; toEncoding = strToJEncoding

instance FromJSON Commitment where parseJSON = strParseJSON "Commitment"

instance ToJSON LabelHash where toJSON = strToJSON; toEncoding = strToJEncoding

instance FromJSON LabelHash where parseJSON = strParseJSON "LabelHash"

instance ToJSON NameSecret where toJSON = strToJSON; toEncoding = strToJEncoding

instance FromJSON NameSecret where parseJSON = strParseJSON "NameSecret"

instance ToJSON TxHash where toJSON = strToJSON; toEncoding = strToJEncoding

instance FromJSON TxHash where parseJSON = strParseJSON "TxHash"

-- | Bind a name to an owner address with a secret and a TTL. Uses keccak256, so
-- the commitment matches what an Ethereum registrar would hash.
mkCommitment :: Text -> Address -> NameSecret -> NameTtl -> Commitment
mkCommitment name owner (NameSecret secret) ttl =
  Commitment . keccak256 $ B.concat [encodeUtf8 name, unAddress owner, secret, smpEncode ttl]

mkLabelHash :: Text -> LabelHash
mkLabelHash = LabelHash . labelHash . encodeUtf8

validNameChar :: Char -> Bool
validNameChar c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'

-- | What the contract accepts in a label. Shared rather than mirrored: a quote
-- carries only 'LabelHash', so the registrar cannot see the charset and the
-- client is the only side that can check it. Two copies that drifted would let
-- a client call a name available that registration then refuses.
--
-- Letter-digit-hyphen, and the hyphen rules that come with it. The charset
-- alone is not enough: it admits @xn--@, and a punycode label is ASCII that
-- renders as something else entirely, which is the same confusable attack the
-- lowercase rule closes. Positions 3 and 4 are refused wholesale rather than
-- @xn--@ specifically, because that slot is reserved for exactly this purpose
-- and the next prefix to be defined should not need a code change.
validLabel :: Text -> Bool
validLabel l =
  T.all validNameChar l
    && not ("-" `T.isPrefixOf` l)
    && not ("-" `T.isSuffixOf` l)
    && not ("--" `T.isPrefixOf` T.drop 2 l)

-- | @{ version, request }@ — the outer envelope. Fields are @nr@-prefixed so
-- they do not shadow local bindings where this module is imported unqualified.
data NamesRequest = NamesRequest
  { nrVersion :: NamesVersion,
    nrRequest :: NamesCommand
  }
  deriving (Eq, Show)

-- | Idempotency key on every mutating call. Matching fields cannot distinguish a
-- resent request from a user genuinely doing the same thing twice.
newtype RequestId = RequestId {unRequestId :: ByteString}
  deriving (Eq, Show)

instance StrEncoding RequestId where
  strEncode = strEncode . unRequestId
  strP = RequestId <$> strP

instance ToJSON RequestId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromJSON RequestId where
  parseJSON = strParseJSON "RequestId"

-- | A redemption code: an unguessable random value issued ahead of time and
-- looked up by the registrar. Opaque to the client, which does not verify it.
newtype RedemptionCode = RedemptionCode {unRedemptionCode :: Text}
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | A 65-byte @r || s || v@ Ethereum signature over a relayed intent.
newtype IntentSig = IntentSig {unIntentSig :: ByteString}
  deriving (Eq, Show)

instance StrEncoding IntentSig where
  strEncode = strEncode . unIntentSig
  strP = IntentSig <$> strP

instance ToJSON IntentSig where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromJSON IntentSig where
  parseJSON = strParseJSON "IntentSig"

data NamesCommand
  = NRCommit {nrCommitment :: Commitment}
  | NRReveal
      { nrName :: Text,
        nrOwner :: Address,
        nrSecret :: NameSecret,
        nrTtl :: NameTtl,
        nrLink :: Text
      }
  | -- | Availability and price. @years@ is an input because a price without a
    -- term is meaningless; the CLI ignores the price, mobile IAP needs it.
    -- @nrLabelLen@ travels alongside because the length cannot be recovered
    -- from the hash and the registry has a minimum. A client gains nothing by
    -- lying: understating it is refused as too short, and overstating it only
    -- defers the refusal to 'NRBuy', which sees the plaintext.
    NRQuote {nrLabelHash :: LabelHash, nrLabelLen :: Word32, nrYears :: Word32}
  | -- | Register against a redemption code. The term is /not/ a field: it comes
    -- from the code's tier, so client and service cannot disagree about what was
    -- paid for. @requestId@ makes a resent request distinguishable from a
    -- genuine second attempt.
    NRBuy
      { nrRequestId :: RequestId,
        nrName :: Text,
        nrOwner :: Address,
        nrCode :: RedemptionCode,
        nrLink :: Text
      }
  | -- | Ask the registrar what a code is worth. Safe to expose: codes are
    -- unguessable random values, so this is not a probing oracle.
    NRVerifyCode {nrCode :: RedemptionCode}
  | NRResolve {nrName :: Text}
  | NROwnedBy {nrAddress :: Address}
  | NRNonce {nrAddress :: Address}
  | -- | Hand a user-signed record edit to the relayer, which pays the gas.
    NRRelayIntent
      { nrRequestId :: RequestId,
        nrName :: Text,
        nrRecordKey :: Text,
        nrValue :: Text,
        nrNonce :: Integer,
        nrDeadline :: Integer,
        nrSig :: IntentSig
      }
  deriving (Eq, Show)

data NamesResponse
  = NRPCommitted {nrTxHash :: TxHash}
  | NRPRegistered {nrName :: Text, nrExpiry :: UTCTime, nrTxHash :: TxHash}
  | NRPQuote
      { nrLabelHash :: LabelHash,
        nrAvailable :: Bool,
        nrTakenUntil :: Maybe UTCTime,
        nrReserved :: Bool,
        nrPriceUsdCents :: Word32,
        nrYears :: Word32
      }
  | NRPRecord
      { nrName :: Text,
        nrOwner :: Address,
        nrContact :: [Text],
        nrChannel :: [Text],
        nrExpiry :: UTCTime,
        nrEditsLeft :: Word32
      }
  | NRPCode {nrMinLength :: Word32, nrYears :: Word32, nrExpires :: UTCTime}
  | NRPNames {nrNames :: [Text]}
  | NRPNonce {nrNonce :: Integer}
  | NRPRelayed {nrTxHash :: TxHash}
  | NRPError {nrCode :: NamesErrorCode, nrMessage :: Maybe Text, nrRetryAfter :: Maybe Word32}
  deriving (Eq, Show)

data NamesErrorCode
  = NECNameTaken
  | NECBadRequest
  | NECUnsupportedVersion
  | NECInternal
  | NECNameReserved
  | NECNameTooShort
  | NECNameInvalid
  | NECPaymentRejected
  | NECCodeSpent
  | NECCodeExpired
  | NECBadSignature
  | NECBadNonce
  | NECExpiredIntent
  | NECNotOwner
  | NECNotFound
  | NECNoEditCredits
  | NECUnknown Text -- forwards-compatible: service may be ahead of clients
  deriving (Eq, Show)

instance TextEncoding NamesErrorCode where
  textEncode = \case
    NECNameTaken -> "name_taken"
    NECBadRequest -> "bad_request"
    NECUnsupportedVersion -> "unsupported_version"
    NECInternal -> "internal"
    NECNameReserved -> "name_reserved"
    NECNameTooShort -> "name_too_short"
    NECNameInvalid -> "name_invalid"
    NECPaymentRejected -> "payment_rejected"
    NECCodeSpent -> "code_spent"
    NECCodeExpired -> "code_expired"
    NECBadSignature -> "bad_signature"
    NECBadNonce -> "bad_nonce"
    NECExpiredIntent -> "expired_intent"
    NECNotOwner -> "not_owner"
    NECNotFound -> "not_found"
    NECNoEditCredits -> "no_edit_credits"
    NECUnknown t -> t
  textDecode = Just . \case
    "name_taken" -> NECNameTaken
    "bad_request" -> NECBadRequest
    "unsupported_version" -> NECUnsupportedVersion
    "internal" -> NECInternal
    "name_reserved" -> NECNameReserved
    "name_too_short" -> NECNameTooShort
    "name_invalid" -> NECNameInvalid
    "payment_rejected" -> NECPaymentRejected
    "code_spent" -> NECCodeSpent
    "code_expired" -> NECCodeExpired
    "bad_signature" -> NECBadSignature
    "bad_nonce" -> NECBadNonce
    "expired_intent" -> NECExpiredIntent
    "not_owner" -> NECNotOwner
    "not_found" -> NECNotFound
    "no_edit_credits" -> NECNoEditCredits
    t -> NECUnknown t

instance ToJSON NamesErrorCode where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON NamesErrorCode where
  parseJSON = textParseJSON "NamesErrorCode"

-- @nrLink@ is the one field whose wire name is not just the prefix dropped.
$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "NR") {J.fieldLabelModifier = \case "nrLink" -> "simplex_link"; f -> dropPrefix "nr" f} ''NamesCommand)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "NRP") {J.fieldLabelModifier = dropPrefix "nr"} ''NamesResponse)

-- spliced last: its instance uses the NamesCommand instances above
$(JQ.deriveJSON defaultJSON {J.fieldLabelModifier = dropPrefix "nr"} ''NamesRequest)

-- | Progress phases streamed from core to the UI as registration advances.
-- Not part of the wire protocol — carried in @CEvtNameRegistrationProgress@.
-- 'NRPhaseCommitted' carries @waitMs@: the commit→reveal wait starts as soon
-- as it is emitted, so it needs no phase of its own.
data NameRegPhase
  = NRPhaseCommitting
  | NRPhaseCommitted
  | NRPhaseRevealing
  | NRPhaseRegistered
  deriving (Eq, Show)

$(JQ.deriveJSON (enumJSON $ \case "NRPhaseCommitting" -> "committing"; "NRPhaseCommitted" -> "committed"; "NRPhaseRevealing" -> "revealing"; "NRPhaseRegistered" -> "registered"; s -> s) ''NameRegPhase)
