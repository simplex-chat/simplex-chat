{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Operators where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import Data.FileEmbed
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Language.Haskell.TH.Syntax (lift)
import Simplex.Chat.Operators.Conditions
import Simplex.Chat.Types.Util (textParseJSON)
import Simplex.Messaging.Agent.Env.SQLite (OperatorId, ServerRoles)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fromTextField_, sumTypeJSON)
import Simplex.Messaging.Protocol (ProtocolType (..), ProtoServerWithAuth)
import Simplex.Messaging.Util (safeDecodeUtf8)

conditionsCommit :: Text
conditionsCommit = "165143a1112308c035ac00ed669b96b60599aa1c"

conditionsText :: Text
conditionsText =
  $( let s = $(embedFile =<< makeRelativeToProject "PRIVACY.md")
      in [| stripFrontMatter (safeDecodeUtf8 $(lift s)) |]
   )

data OperatorTag = OTSimplex | OTXyz
  deriving (Show)

instance FromField OperatorTag where fromField = fromTextField_ textDecode

instance ToField OperatorTag where toField = toField . textEncode

instance FromJSON OperatorTag where
  parseJSON = textParseJSON "OperatorTag"

instance ToJSON OperatorTag where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding OperatorTag where
  textDecode = \case
    "simplex" -> Just OTSimplex
    "xyz" -> Just OTXyz
    _ -> Nothing
  textEncode = \case
    OTSimplex -> "simplex"
    OTXyz -> "xyz"

data ConditionsAcceptance
  = CAAccepted {date :: UTCTime}
  | CARequired {deadline :: Maybe UTCTime, acceptedConditionsCommit :: Maybe Text}
  deriving (Show)

data ServerOperator = ServerOperator
  { operatorId :: OperatorId,
    operatorTag :: Maybe OperatorTag,
    tradeName :: Text,
    legalName :: Maybe Text,
    acceptedConditions :: Maybe ConditionsAcceptance,
    enabled :: Bool,
    roles :: ServerRoles
  }
  deriving (Show)

data UserServers = UserServers
  { operator :: ServerOperator,
    smpServers :: NonEmpty (ProtoServerWithAuth 'PSMP),
    xftpServers :: NonEmpty (ProtoServerWithAuth 'PXFTP)
  }
  deriving (Show)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CA") ''ConditionsAcceptance)

$(JQ.deriveJSON defaultJSON ''ServerOperator)

$(JQ.deriveJSON defaultJSON ''UserServers)
