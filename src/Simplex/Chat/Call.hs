{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Word (Word16)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Types (Contact, ContactId, User)
import Simplex.Messaging.Agent.Store.DB (Binary (..), fromTextField_)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fstToLower, singleFieldJSON)
import Simplex.Messaging.Util (decodeJSON, encodeJSON)
import Simplex.Messaging.Version
import Simplex.Messaging.Version.Internal

data Call = Call
  { contactId :: ContactId,
    callId :: CallId,
    callUUID :: Text,
    chatItemId :: Int64,
    callState :: CallState,
    callTs :: UTCTime
  }
  deriving (Show)

-- Call version history:
-- 1 - DH secret is used as media key (also assumed when call messages have no version)
-- 2 - media key is derived from DH secret with HKDF (2026-10-01)

data CallVersion

instance VersionScope CallVersion

type VersionCall = Version CallVersion

type VersionRangeCall = VersionRange CallVersion

pattern VersionCall :: Word16 -> VersionCall
pattern VersionCall v = Version v

initialCallVersion :: VersionCall
initialCallVersion = VersionCall 1

callMediaKdfVersion :: VersionCall
callMediaKdfVersion = VersionCall 2

-- This should not be used directly in code, instead use `callVRange` from ChatConfig
currentCallVersion :: VersionCall
currentCallVersion = VersionCall 2

supportedCallVRange :: VersionRangeCall
supportedCallVRange = mkVersionRange initialCallVersion currentCallVersion

callInitialVRange :: VersionRangeCall
callInitialVRange = versionToRange initialCallVersion

newtype CallVersionRange = CallVersionRange {fromCallVRange :: VersionRangeCall}
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via (StrJSON "CallVersionRange" VersionRangeCall)

callMediaKey :: VersionCall -> CallId -> C.PublicKeyX25519 -> C.PrivateKeyX25519 -> C.Key
callMediaKey v (CallId salt) peerPubKey privKey
  | v >= callMediaKdfVersion = C.Key $ C.hkdf salt dhSecret "SimpleXCallMediaKey" callMediaKeySize
  | otherwise = C.Key dhSecret
  where
    dhSecret = C.dhBytes' $ C.dh' peerPubKey privKey

-- AES-256-GCM key used by the apps for frame encryption
callMediaKeySize :: Int
callMediaKeySize = 32

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
        sharedKey :: Maybe C.Key,
        callVersion :: Maybe VersionCall
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
  deriving newtype (FromField)

instance ToField CallId where toField (CallId m) = toField $ Binary m

instance StrEncoding CallId where
  strEncode (CallId m) = strEncode m
  strDecode s = CallId <$> strDecode s
  strP = CallId <$> strP

instance FromJSON CallId where
  parseJSON = strParseJSON "CallId"

instance ToJSON CallId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data RcvCallInvitation = RcvCallInvitation
  { user :: User,
    contact :: Contact,
    callType :: CallType,
    sharedKey :: Maybe C.Key,
    callUUID :: Text,
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
    callDhPubKey :: Maybe C.PublicKeyX25519,
    callVRange :: Maybe CallVersionRange
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
    callDhPubKey :: Maybe C.PublicKeyX25519,
    callVersion :: Maybe VersionCall
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
