{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Messages.CIContent.Events.DB where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as J
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.Messages.CIContent.Events.Prefix
import Simplex.Messaging.Parsers (singleFieldJSON)

newtype DBRcvGroupEvent = RGE RcvGroupEvent

instance FromJSON DBRcvGroupEvent where
  parseJSON v = RGE <$> $(J.mkParseJSON (singleFieldJSON dropPrefixRGE) ''RcvGroupEvent) v

instance ToJSON DBRcvGroupEvent where
  toJSON (RGE v) = $(J.mkToJSON (singleFieldJSON dropPrefixRGE) ''RcvGroupEvent) v
  toEncoding (RGE v) = $(J.mkToEncoding (singleFieldJSON dropPrefixRGE) ''RcvGroupEvent) v

newtype DBSndGroupEvent = SGE SndGroupEvent

instance FromJSON DBSndGroupEvent where
  parseJSON v = SGE <$> $(J.mkParseJSON (singleFieldJSON dropPrefixSGE) ''SndGroupEvent) v

instance ToJSON DBSndGroupEvent where
  toJSON (SGE v) = $(J.mkToJSON (singleFieldJSON dropPrefixSGE) ''SndGroupEvent) v
  toEncoding (SGE v) = $(J.mkToEncoding (singleFieldJSON dropPrefixSGE) ''SndGroupEvent) v

newtype DBRcvConnEvent = RCE RcvConnEvent

instance FromJSON DBRcvConnEvent where
  parseJSON v = RCE <$> $(J.mkParseJSON (singleFieldJSON dropPrefixRCE) ''RcvConnEvent) v

instance ToJSON DBRcvConnEvent where
  toJSON (RCE v) = $(J.mkToJSON (singleFieldJSON dropPrefixRCE) ''RcvConnEvent) v
  toEncoding (RCE v) = $(J.mkToEncoding (singleFieldJSON dropPrefixRCE) ''RcvConnEvent) v

newtype DBSndConnEvent = SCE SndConnEvent

instance FromJSON DBSndConnEvent where
  parseJSON v = SCE <$> $(J.mkParseJSON (singleFieldJSON dropPrefixSCE) ''SndConnEvent) v

instance ToJSON DBSndConnEvent where
  toJSON (SCE v) = $(J.mkToJSON (singleFieldJSON dropPrefixSCE) ''SndConnEvent) v
  toEncoding (SCE v) = $(J.mkToEncoding (singleFieldJSON dropPrefixSCE) ''SndConnEvent) v

newtype DBRcvDirectEvent = RDE RcvDirectEvent

instance FromJSON DBRcvDirectEvent where
  parseJSON v = RDE <$> $(J.mkParseJSON (singleFieldJSON dropPrefixRDE) ''RcvDirectEvent) v

instance ToJSON DBRcvDirectEvent where
  toJSON (RDE v) = $(J.mkToJSON (singleFieldJSON dropPrefixRDE) ''RcvDirectEvent) v
  toEncoding (RDE v) = $(J.mkToEncoding (singleFieldJSON dropPrefixRDE) ''RcvDirectEvent) v
