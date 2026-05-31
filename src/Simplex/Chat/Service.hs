{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Service where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Type.Equality
import Simplex.Messaging.Agent.Protocol (AConnectionLink)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, sumTypeJSON)

data ServiceType = STBadge | STDirectory | STNames
  deriving (Eq, Ord, Show)

data SServiceType (s :: ServiceType) where
  SSTBadge :: SServiceType 'STBadge
  SSTDirectory :: SServiceType 'STDirectory
  SSTNames :: SServiceType 'STNames

deriving instance Eq (SServiceType s)

deriving instance Show (SServiceType s)

instance TestEquality SServiceType where
  testEquality SSTBadge SSTBadge = Just Refl
  testEquality SSTDirectory SSTDirectory = Just Refl
  testEquality SSTNames SSTNames = Just Refl
  testEquality _ _ = Nothing

class ServiceTypeI (s :: ServiceType) where sServiceType :: SServiceType s

instance ServiceTypeI 'STBadge where sServiceType = SSTBadge

instance ServiceTypeI 'STDirectory where sServiceType = SSTDirectory

instance ServiceTypeI 'STNames where sServiceType = SSTNames

-- Badge

data BadgeCommand
  = BCIssue Text Text
  | BCRedeem Text
  | BCStripeLink Text
  deriving (Show)

data BadgeResponse
  = BRCredential Text UTCTime Int
  | BRStripeLink Text
  | BRError Text
  deriving (Show)

-- Directory

data DirectoryCommand = DCSearch Text
  deriving (Show)

data DirectoryResponse = DRResult [Text]
  deriving (Show)

-- Names

data NamesCommand = NCResolve Text
  deriving (Show)

data NamesResponse
  = NRResolved AConnectionLink
  | NRNotFound
  | NRError Text
  deriving (Show)

-- Service command/response GADTs

data ServiceCommand (s :: ServiceType) where
  SCBadge :: BadgeCommand -> ServiceCommand 'STBadge
  SCDirectory :: DirectoryCommand -> ServiceCommand 'STDirectory
  SCNames :: NamesCommand -> ServiceCommand 'STNames

deriving instance Show (ServiceCommand s)

data ServiceResponse (s :: ServiceType) where
  SRBadge :: BadgeResponse -> ServiceResponse 'STBadge
  SRDirectory :: DirectoryResponse -> ServiceResponse 'STDirectory
  SRNames :: NamesResponse -> ServiceResponse 'STNames

deriving instance Show (ServiceResponse s)

-- Existential wrappers

data AServiceCommand = forall s. ServiceTypeI s => ASC (SServiceType s) (ServiceCommand s)

deriving instance Show AServiceCommand

data AServiceResponse = forall s. ServiceTypeI s => ASR (SServiceType s) (ServiceResponse s)

deriving instance Show AServiceResponse

$(JQ.deriveJSON (enumJSON $ dropPrefix "ST") ''ServiceType)

instance ToJSON (SServiceType s) where
  toJSON = toJSON . serviceType
    where
      serviceType :: SServiceType s -> ServiceType
      serviceType SSTBadge = STBadge
      serviceType SSTDirectory = STDirectory
      serviceType SSTNames = STNames

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "BC") ''BadgeCommand)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "BR") ''BadgeResponse)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "DC") ''DirectoryCommand)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "DR") ''DirectoryResponse)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "NC") ''NamesCommand)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "NR") ''NamesResponse)

instance ToJSON AServiceCommand where
  toJSON (ASC sst cmd) = J.object ["serviceType" J..= sst, "command" J..= cmdJSON sst cmd]
    where
      cmdJSON :: SServiceType s -> ServiceCommand s -> J.Value
      cmdJSON SSTBadge (SCBadge c) = toJSON c
      cmdJSON SSTDirectory (SCDirectory c) = toJSON c
      cmdJSON SSTNames (SCNames c) = toJSON c

instance ToJSON AServiceResponse where
  toJSON (ASR sst resp) = J.object ["serviceType" J..= sst, "response" J..= respJSON sst resp]
    where
      respJSON :: SServiceType s -> ServiceResponse s -> J.Value
      respJSON SSTBadge (SRBadge r) = toJSON r
      respJSON SSTDirectory (SRDirectory r) = toJSON r
      respJSON SSTNames (SRNames r) = toJSON r

instance FromJSON AServiceCommand where
  parseJSON = J.withObject "AServiceCommand" $ \o -> do
    sType <- o J..: "serviceType"
    case (sType :: ServiceType) of
      STBadge -> ASC SSTBadge . SCBadge <$> o J..: "command"
      STDirectory -> ASC SSTDirectory . SCDirectory <$> o J..: "command"
      STNames -> ASC SSTNames . SCNames <$> o J..: "command"

instance FromJSON AServiceResponse where
  parseJSON = J.withObject "AServiceResponse" $ \o -> do
    sType <- o J..: "serviceType"
    case (sType :: ServiceType) of
      STBadge -> ASR SSTBadge . SRBadge <$> o J..: "response"
      STDirectory -> ASR SSTDirectory . SRDirectory <$> o J..: "response"
      STNames -> ASR SSTNames . SRNames <$> o J..: "response"
