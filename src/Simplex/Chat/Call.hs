{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Call where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Chat.Types (Contact, ContactId, User)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fromTextField_, fstToLower, singleFieldJSON)
import Simplex.Messaging.Util (decodeJSON, encodeJSON)

data Call = Call
  { contactId :: ContactId,
    callId :: CallId,
    chatItemId :: Int64,
    callState :: CallState,
    callTs :: UTCTime
  }
  deriving (Show)

isRcvInvitation :: Call -> Bool
isRcvInvitation Call {callState} = case callState of
  CallInvitationReceived {} -> True
  _ -> False

data CallStateTag
  = CSTCallInvitationSent
  | CSTCallInvitationReceived
  | CSTCallOfferSent
  | CSTCallOfferReceived
  | CSTCallNegotiated
  deriving (Show)

callStateTag :: CallState -> CallStateTag
callStateTag = \case
  CallInvitationSent {} -> CSTCallInvitationSent
  CallInvitationReceived {} -> CSTCallInvitationReceived
  CallOfferSent {} -> CSTCallOfferSent
  CallOfferReceived {} -> CSTCallOfferReceived
  CallNegotiated {} -> CSTCallNegotiated

data CallState
  = CallInvitationSent
      { localCallType :: CallType,
        localDhPrivKey :: Maybe C.PrivateKeyX25519
      }
  | CallInvitationReceived
      { peerCallType :: CallType,
        localDhPubKey :: Maybe C.PublicKeyX25519,
        sharedKey :: Maybe C.Key
      }
  | CallOfferSent
      { localCallType :: CallType,
        peerCallType :: CallType,
        localCallSession :: WebRTCSession,
        sharedKey :: Maybe C.Key
      }
  | CallOfferReceived
      { localCallType :: CallType,
        peerCallType :: CallType,
        peerCallSession :: WebRTCSession,
        sharedKey :: Maybe C.Key
      }
  | CallNegotiated
      { localCallType :: CallType,
        peerCallType :: CallType,
        localCallSession :: WebRTCSession,
        peerCallSession :: WebRTCSession,
        sharedKey :: Maybe C.Key
      }
  deriving (Show)

newtype CallId = CallId ByteString
  deriving (Eq, Show)

instance StrEncoding CallId where
  strEncode (CallId m) = strEncode m
  strDecode s = CallId <$> strDecode s
  strP = CallId <$> strP

instance FromJSON CallId where
  parseJSON = strParseJSON "CallId"

instance ToJSON CallId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField CallId where fromField f = CallId <$> fromField f

instance ToField CallId where toField (CallId m) = toField m

data RcvCallInvitation = RcvCallInvitation
  { user :: User,
    contact :: Contact,
    callType :: CallType,
    sharedKey :: Maybe C.Key,
    callTs :: UTCTime
  }
  deriving (Show)

data CallType = CallType
  { media :: CallMedia,
    capabilities :: CallCapabilities
  }
  deriving (Eq, Show)

defaultCallType :: CallType
defaultCallType = CallType CMVideo $ CallCapabilities {encryption = True}

encryptedCall :: CallType -> Bool
encryptedCall CallType {capabilities = CallCapabilities {encryption}} = encryption

-- | * Types for chat protocol
data CallInvitation = CallInvitation
  { callType :: CallType,
    callDhPubKey :: Maybe C.PublicKeyX25519
  }
  deriving (Eq, Show)

data CallMedia = CMAudio | CMVideo
  deriving (Eq, Show)

data CallCapabilities = CallCapabilities
  { encryption :: Bool
  }
  deriving (Eq, Show)

data CallOffer = CallOffer
  { callType :: CallType,
    rtcSession :: WebRTCSession,
    callDhPubKey :: Maybe C.PublicKeyX25519
  }
  deriving (Eq, Show)

data WebRTCCallOffer = WebRTCCallOffer
  { callType :: CallType,
    rtcSession :: WebRTCSession
  }
  deriving (Eq, Show)

data CallAnswer = CallAnswer
  { rtcSession :: WebRTCSession
  }
  deriving (Eq, Show)

data CallExtraInfo = CallExtraInfo
  { rtcExtraInfo :: WebRTCExtraInfo
  }
  deriving (Eq, Show)

data WebRTCSession = WebRTCSession
  { rtcSession :: Text, -- LZW compressed JSON encoding of offer or answer
    rtcIceCandidates :: Text -- LZW compressed JSON encoding of array of ICE candidates
  }
  deriving (Eq, Show)

data WebRTCExtraInfo = WebRTCExtraInfo
  { rtcIceCandidates :: Text -- LZW compressed JSON encoding of array of ICE candidates
  }
  deriving (Eq, Show)

data WebRTCCallStatus = WCSConnecting | WCSConnected | WCSDisconnected | WCSFailed
  deriving (Show)

instance StrEncoding WebRTCCallStatus where
  strEncode = \case
    WCSConnecting -> "connecting"
    WCSConnected -> "connected"
    WCSDisconnected -> "disconnected"
    WCSFailed -> "failed"
  strP =
    A.takeTill (== ' ') >>= \case
      "connecting" -> pure WCSConnecting
      "connected" -> pure WCSConnected
      "disconnected" -> pure WCSDisconnected
      "failed" -> pure WCSFailed
      _ -> fail "bad WebRTCCallStatus"

$(J.deriveJSON (enumJSON $ dropPrefix "CSTCall") ''CallStateTag)

$(J.deriveJSON (enumJSON $ dropPrefix "CM") ''CallMedia)

$(J.deriveJSON defaultJSON ''CallCapabilities)

$(J.deriveJSON defaultJSON ''CallType)

$(J.deriveJSON defaultJSON ''CallInvitation)

$(J.deriveJSON defaultJSON ''WebRTCSession)

$(J.deriveJSON defaultJSON ''CallOffer)

$(J.deriveJSON defaultJSON ''WebRTCCallOffer)

$(J.deriveJSON defaultJSON ''CallAnswer)

$(J.deriveJSON defaultJSON ''WebRTCExtraInfo)

$(J.deriveJSON defaultJSON ''CallExtraInfo)

-- database representation
$(J.deriveJSON (singleFieldJSON fstToLower) ''CallState)

instance ToField CallState where
  toField = toField . encodeJSON

instance FromField CallState where
  fromField = fromTextField_ decodeJSON

$(J.deriveJSON defaultJSON ''RcvCallInvitation)
