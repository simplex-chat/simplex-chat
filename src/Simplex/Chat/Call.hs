{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Call where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.Word (Word16)
import GHC.Generics (Generic)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String

newtype CallId = CallId Word16
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

-- | * Types for chat protocol
data CallInvitation = CallInvitation
  { callDhPubKey :: Maybe C.PublicKeyX25519,
    callMedia :: CallMedia,
    callCapabilities :: CallCapabilities
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
  parseJSON = strParseJSON "CallMedia"

instance ToJSON CallMedia where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance StrEncoding CallMedia where
  strEncode = \case
    CMAudio -> "audio"
    CMVideo -> "video"
  strDecode = \case
    "audio" -> Right CMAudio
    "video" -> Right CMVideo
    _ -> Left "bad CallMedia"

data CallCapabilities = CallCapabilities
  { encryption :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON CallCapabilities where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data CallOffer = CallOffer
  { callDhPubKey :: Maybe C.PublicKeyX25519,
    rtcSession :: WebRTCSession
  }
  deriving (Eq, Show, Generic)

instance FromJSON CallOffer where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}

instance ToJSON CallOffer where
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
  { rtcSession :: J.Value,
    rtcIceCandidates :: [J.Value]
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON WebRTCSession where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data WebRTCExtraInfo = WebRTCExtraInfo
  { rtcIceCandidates :: [J.Value]
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON WebRTCExtraInfo where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions
