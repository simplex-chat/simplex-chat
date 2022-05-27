{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Call where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, enumJSON)

data Call = Call
  { contactId :: Int64,
    callId :: CallId,
    chatItemId :: Int64,
    callState :: CallState
  }

data CallStateTag
  = CSTCallInvitationSent
  | CSTCallInvitationReceived
  | CSTCallOfferSent
  | CSTCallOfferReceived
  | CSTCallNegotiated
  deriving (Show, Generic)

instance ToJSON CallStateTag where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "CSTCall"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "CSTCall"

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

data CallType = CallType
  { media :: CallMedia,
    capabilities :: CallCapabilities
  }
  deriving (Eq, Show, Generic, FromJSON)

defaultCallType :: CallType
defaultCallType = CallType CMVideo $ CallCapabilities {encryption = True}

encryptedCall :: CallType -> Bool
encryptedCall CallType {capabilities = CallCapabilities {encryption}} = encryption

instance ToJSON CallType where toEncoding = J.genericToEncoding J.defaultOptions

-- | * Types for chat protocol
data CallInvitation = CallInvitation
  { callType :: CallType,
    callDhPubKey :: Maybe C.PublicKeyX25519
  }
  deriving (Eq, Show, Generic)

instance FromJSON CallInvitation where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}

instance ToJSON CallInvitation where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data CallMedia = CMAudio | CMVideo
  deriving (Eq, Show, Generic)

instance FromJSON CallMedia where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "CM"

instance ToJSON CallMedia where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "CM"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "CM"

data CallCapabilities = CallCapabilities
  { encryption :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON CallCapabilities where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data CallOffer = CallOffer
  { callType :: CallType,
    rtcSession :: WebRTCSession,
    callDhPubKey :: Maybe C.PublicKeyX25519
  }
  deriving (Eq, Show, Generic)

instance FromJSON CallOffer where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}

instance ToJSON CallOffer where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data WebRTCCallOffer = WebRTCCallOffer
  { callType :: CallType,
    rtcSession :: WebRTCSession
  }
  deriving (Eq, Show, Generic)

instance FromJSON WebRTCCallOffer where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}

instance ToJSON WebRTCCallOffer where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data CallAnswer = CallAnswer
  { rtcSession :: WebRTCSession
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON CallAnswer where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data CallExtraInfo = CallExtraInfo
  { rtcExtraInfo :: WebRTCExtraInfo
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON CallExtraInfo where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data WebRTCSession = WebRTCSession
  { rtcSession :: Text, -- LZW compressed JSON encoding of offer or answer
    rtcIceCandidates :: Text -- LZW compressed JSON encoding of array of ICE candidates
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON WebRTCSession where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data WebRTCExtraInfo = WebRTCExtraInfo
  { rtcIceCandidates :: Text -- LZW compressed JSON encoding of array of ICE candidates
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON WebRTCExtraInfo where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

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
