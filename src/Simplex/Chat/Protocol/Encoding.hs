{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Simplex.Chat.Protocol.Encoding where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Generics

data AppMessage = AppMessage
  { msgId :: Text,
    minVersion :: Word16,
    maxVersion :: Word16,
    event :: Text,
    params :: J.Object
  }
  deriving (Generic, FromJSON)

instance ToJSON AppMessage where toEncoding = J.genericToEncoding J.defaultOptions
