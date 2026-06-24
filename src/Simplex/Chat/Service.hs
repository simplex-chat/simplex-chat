{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Data.Aeson.Types as JT
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Type.Equality
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, singleFieldJSON, sumTypeJSON)

-- Service type

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

toServiceType :: SServiceType s -> ServiceType
toServiceType = \case
  SSTBadge -> STBadge
  SSTDirectory -> STDirectory
  SSTNames -> STNames

aServiceType :: ServiceType -> AServiceType
aServiceType = \case
  STBadge -> AST SSTBadge
  STDirectory -> AST SSTDirectory
  STNames -> AST SSTNames

data AServiceType = forall s. ServiceTypeI s => AST (SServiceType s)

-- Per-service command types

data BadgeCommand
  = BCIssue Text Text
  | BCRedeem Text
  | BCStripeLink Text
  deriving (Show)

data DirectoryCommand = DCSearch Text
  deriving (Show)

data NamesCommand = NCResolve Text
  deriving (Show)

-- Per-service response types

data BadgeResponse
  = BRCredential Text UTCTime Int
  | BRStripeLink Text
  | BRError Text
  deriving (Show)

data DirectoryResponse = DRResult [Text]
  deriving (Show)

data NamesResponse
  = NRResolved Text
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

-- Platform JSON for per-service response types (for chat API -> Kotlin/Swift)

$(JQ.deriveJSON (enumJSON $ dropPrefix "ST") ''ServiceType)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "BR") ''BadgeResponse)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "DR") ''DirectoryResponse)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "NR") ''NamesResponse)

instance ToJSON (SServiceType s) where
  toJSON = J.toJSON . toServiceType
  toEncoding = J.toEncoding . toServiceType

instance ToJSON AServiceResponse where
  toJSON (ASR sst resp) = J.object ["type" J..= sst, "response" J..= respJSON sst resp]
  toEncoding (ASR sst resp) = J.pairs ("type" J..= sst <> "response" J..= respJSON sst resp)

instance FromJSON AServiceResponse where
  parseJSON = J.withObject "AServiceResponse" $ \o -> do
    sType <- o J..: "type"
    case aServiceType sType of
      AST SSTBadge -> ASR SSTBadge . SRBadge <$> o J..: "response"
      AST SSTDirectory -> ASR SSTDirectory . SRDirectory <$> o J..: "response"
      AST SSTNames -> ASR SSTNames . SRNames <$> o J..: "response"

respJSON :: SServiceType s -> ServiceResponse s -> J.Value
respJSON SSTBadge (SRBadge r) = toJSON r
respJSON SSTDirectory (SRDirectory r) = toJSON r
respJSON SSTNames (SRNames r) = toJSON r

-- Wire YAML for service bot messages (platform-independent, singleFieldJSON)

newtype ServiceJSON a = ServiceJSON {serviceJSON :: a}

instance ToJSON (ServiceJSON BadgeResponse) where
  toJSON (ServiceJSON r) = $(JQ.mkToJSON (singleFieldJSON $ dropPrefix "BR") ''BadgeResponse) r
  toEncoding (ServiceJSON r) = $(JQ.mkToEncoding (singleFieldJSON $ dropPrefix "BR") ''BadgeResponse) r

instance FromJSON (ServiceJSON BadgeResponse) where
  parseJSON v = ServiceJSON <$> $(JQ.mkParseJSON (singleFieldJSON $ dropPrefix "BR") ''BadgeResponse) v

instance ToJSON (ServiceJSON DirectoryResponse) where
  toJSON (ServiceJSON r) = $(JQ.mkToJSON (singleFieldJSON $ dropPrefix "DR") ''DirectoryResponse) r
  toEncoding (ServiceJSON r) = $(JQ.mkToEncoding (singleFieldJSON $ dropPrefix "DR") ''DirectoryResponse) r

instance FromJSON (ServiceJSON DirectoryResponse) where
  parseJSON v = ServiceJSON <$> $(JQ.mkParseJSON (singleFieldJSON $ dropPrefix "DR") ''DirectoryResponse) v

instance ToJSON (ServiceJSON NamesResponse) where
  toJSON (ServiceJSON r) = $(JQ.mkToJSON (singleFieldJSON $ dropPrefix "NR") ''NamesResponse) r
  toEncoding (ServiceJSON r) = $(JQ.mkToEncoding (singleFieldJSON $ dropPrefix "NR") ''NamesResponse) r

instance FromJSON (ServiceJSON NamesResponse) where
  parseJSON v = ServiceJSON <$> $(JQ.mkParseJSON (singleFieldJSON $ dropPrefix "NR") ''NamesResponse) v
