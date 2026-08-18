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
-- Registration is commit-then-reveal (see the MVP plan): 'NRCommit' publishes
-- only @H(name, owner, secret, ttl)@ so the service cannot tell which name it
-- is; 'NRReveal' submits the plaintext once the aged commitment already binds
-- the name to /this/ owner address, so the service cannot front-run it.
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
    NameRegPhase (..),
    mkCommitment,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Control.Applicative (optional)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word16, Word32)
import qualified Data.Aeson.TH as JQ
import Simplex.Messaging.Encoding (smpEncode)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (enumJSON)
import Simplex.Messaging.Eth.Address (Address, checksumAddress, parseAddress, unAddress)
import Simplex.Messaging.Eth.Keccak (keccak256)
import Simplex.Messaging.Util (safeDecodeUtf8)

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

-- | All three are on-chain byte values, so they are encoded the way Ethereum
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

instance StrEncoding NameSecret where
  strEncode = hexEncode . unSecret
  strP = NameSecret <$> hexP

instance StrEncoding TxHash where
  strEncode = hexEncode . unTxHash
  strP = TxHash <$> hexP

instance ToJSON Commitment where toJSON = strToJSON; toEncoding = strToJEncoding

instance FromJSON Commitment where parseJSON = strParseJSON "Commitment"

instance ToJSON NameSecret where toJSON = strToJSON; toEncoding = strToJEncoding

instance FromJSON NameSecret where parseJSON = strParseJSON "NameSecret"

instance ToJSON TxHash where toJSON = strToJSON; toEncoding = strToJEncoding

instance FromJSON TxHash where parseJSON = strParseJSON "TxHash"

-- | Bind a name to an owner address with a secret and a TTL. Uses keccak256, so
-- the commitment matches what an Ethereum registrar would hash.
mkCommitment :: Text -> Address -> NameSecret -> NameTtl -> Commitment
mkCommitment name owner (NameSecret secret) ttl =
  Commitment . keccak256 $ B.concat [encodeUtf8 name, unAddress owner, secret, smpEncode ttl]

-- | @{ version, request }@ — the outer envelope. Fields are @nr@-prefixed so
-- they do not shadow local bindings where this module is imported unqualified.
data NamesRequest = NamesRequest
  { nrVersion :: NamesVersion,
    nrRequest :: NamesCommand
  }
  deriving (Eq, Show)

data NamesCommand
  = NRCommit {nrCommitment :: Commitment}
  | NRReveal
      { nrName :: Text,
        nrOwner :: Address,
        nrSecret :: NameSecret,
        nrTtl :: NameTtl,
        nrLink :: Text
      }
  deriving (Eq, Show)

data NamesResponse
  = NRPCommitted {nrTxHash :: TxHash}
  | NRPRegistered {nrName :: Text, nrExpiry :: UTCTime, nrTxHash :: TxHash}
  | NRPError {nrCode :: NamesErrorCode, nrMessage :: Maybe Text, nrRetryAfter :: Maybe Word32}
  deriving (Eq, Show)

data NamesErrorCode
  = NECNameTaken
  | NECBadRequest
  | NECUnsupportedVersion
  | NECInternal
  | NECUnknown Text -- forwards-compatible: service may be ahead of clients
  deriving (Eq, Show)

instance TextEncoding NamesErrorCode where
  textEncode = \case
    NECNameTaken -> "name_taken"
    NECBadRequest -> "bad_request"
    NECUnsupportedVersion -> "unsupported_version"
    NECInternal -> "internal"
    NECUnknown t -> t
  textDecode = Just . \case
    "name_taken" -> NECNameTaken
    "bad_request" -> NECBadRequest
    "unsupported_version" -> NECUnsupportedVersion
    "internal" -> NECInternal
    t -> NECUnknown t

instance ToJSON NamesErrorCode where
  toJSON = textToJSON
  toEncoding = textToEncoding

instance FromJSON NamesErrorCode where
  parseJSON = textParseJSON "NamesErrorCode"

instance ToJSON NamesRequest where
  toJSON NamesRequest {nrVersion, nrRequest} = J.object ["version" .= nrVersion, "request" .= nrRequest]

instance FromJSON NamesRequest where
  parseJSON = J.withObject "NamesRequest" $ \o ->
    NamesRequest <$> o .: "version" <*> o .: "request"

instance ToJSON NamesCommand where
  toJSON = \case
    NRCommit {nrCommitment} ->
      J.object ["type" .= ("commit" :: Text), "commitment" .= nrCommitment]
    NRReveal {nrName, nrOwner, nrSecret, nrTtl, nrLink} ->
      J.object
        [ "type" .= ("reveal" :: Text),
          "name" .= nrName,
          "owner" .= addressJSON nrOwner,
          "secret" .= nrSecret,
          "ttl" .= nrTtl,
          "simplex_link" .= nrLink
        ]

instance FromJSON NamesCommand where
  parseJSON = J.withObject "NamesCommand" $ \o ->
    (o .: "type") >>= \case
      "commit" -> NRCommit <$> o .: "commitment"
      "reveal" ->
        NRReveal
          <$> o .: "name"
          <*> (o .: "owner" >>= parseAddressJSON)
          <*> o .: "secret"
          <*> o .: "ttl"
          <*> o .: "simplex_link"
      t -> fail $ "unknown names command: " <> T.unpack (t :: Text)

instance ToJSON NamesResponse where
  toJSON = \case
    NRPCommitted {nrTxHash} ->
      J.object ["type" .= ("committed" :: Text), "txHash" .= nrTxHash]
    NRPRegistered {nrName, nrExpiry, nrTxHash} ->
      J.object ["type" .= ("registered" :: Text), "name" .= nrName, "expiry" .= nrExpiry, "txHash" .= nrTxHash]
    NRPError {nrCode, nrMessage, nrRetryAfter} ->
      J.object $
        ["type" .= ("error" :: Text), "code" .= nrCode]
          <> ["message" .= m | Just m <- [nrMessage]]
          <> ["retryAfter" .= r | Just r <- [nrRetryAfter]]

instance FromJSON NamesResponse where
  parseJSON = J.withObject "NamesResponse" $ \o ->
    (o .: "type") >>= \case
      "committed" -> NRPCommitted <$> o .: "txHash"
      "registered" -> NRPRegistered <$> o .: "name" <*> o .: "expiry" <*> o .: "txHash"
      "error" -> NRPError <$> o .: "code" <*> o .:? "message" <*> o .:? "retryAfter"
      t -> fail $ "unknown names response: " <> T.unpack (t :: Text)

-- | EIP-55 checksummed hex, the form a user pastes into a block explorer.
addressJSON :: Address -> J.Value
addressJSON = J.String . safeDecodeUtf8 . checksumAddress

parseAddressJSON :: J.Value -> JT.Parser Address
parseAddressJSON = J.withText "Address" $ either fail pure . parseAddress . encodeUtf8

-- | Progress phases streamed from core to the UI as registration advances.
-- Not part of the wire protocol — carried in @CEvtNameRegistrationProgress@.
-- | 'NRPhaseCommitted' carries @waitMs@: the commit→reveal wait starts as soon
-- as it is emitted, so it needs no phase of its own.
data NameRegPhase
  = NRPhaseCommitting
  | NRPhaseCommitted
  | NRPhaseRevealing
  | NRPhaseRegistered
  deriving (Eq, Show)

$(JQ.deriveJSON (enumJSON $ \case "NRPhaseCommitting" -> "committing"; "NRPhaseCommitted" -> "committed"; "NRPhaseRevealing" -> "revealing"; "NRPhaseRegistered" -> "registered"; s -> s) ''NameRegPhase)
