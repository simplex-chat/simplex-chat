{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Operators where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import Data.FileEmbed
import Data.Int (Int64)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Language.Haskell.TH.Syntax (lift)
import Simplex.Chat.Operators.Conditions
import Simplex.Chat.Types.Util (textParseJSON)
import Simplex.Messaging.Agent.Env.SQLite (OperatorId, ServerCfg (..), ServerRoles)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fromTextField_, sumTypeJSON)
import Simplex.Messaging.Protocol (ProtoServerWithAuth, ProtocolType (..))
import Simplex.Messaging.Util (safeDecodeUtf8)

usageConditionsCommit :: Text
usageConditionsCommit = "165143a1112308c035ac00ed669b96b60599aa1c"

previousConditionsCommit :: Text
previousConditionsCommit = "edf99fcd1d7d38d2501d19608b94c084cf00f2ac"

usageConditionsText :: Text
usageConditionsText =
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

data UsageConditions = UsageConditions
  { conditionsId :: Int64,
    conditionsCommit :: Text,
    notifiedAt :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Show)

data UsageConditionsAction
  = UCAReview {operators :: [ServerOperator], deadline :: Maybe UTCTime, showNotice :: Bool}
  | UCAAccepted {operators :: [ServerOperator]}
  deriving (Show)

-- TODO UI logic
usageConditionsAction :: [ServerOperator] -> UsageConditionsAction
usageConditionsAction _operators = UCAAccepted []

data ConditionsAcceptance
  = CAAccepted {acceptedAt :: Maybe UTCTime}
  | CARequired {deadline :: Maybe UTCTime}
  deriving (Show)

data ServerOperator = ServerOperator
  { operatorId :: OperatorId,
    operatorTag :: Maybe OperatorTag,
    appVendor :: Bool,
    tradeName :: Text,
    legalName :: Maybe Text,
    serverDomains :: [Text],
    conditionsAcceptance :: ConditionsAcceptance,
    enabled :: Bool,
    roles :: ServerRoles
  }
  deriving (Show)

data OperatorEnabled = OperatorEnabled
  { operatorId :: OperatorId,
    enabled :: Bool,
    roles :: ServerRoles
  }
  deriving (Show)

data UserServers = UserServers
  { operator :: Maybe ServerOperator,
    smpServers :: [ServerCfg 'PSMP],
    xftpServers :: [ServerCfg 'PXFTP]
  }
  deriving (Show)

data PresetOperatorServers = PresetOperatorServers
  { operator :: ServerOperator,
    smpServers :: NonEmpty (PresetServer 'PSMP),
    xftpServers :: NonEmpty (PresetServer 'PXFTP),
    useSMP :: Int,
    useXFTP :: Int
  }

data PresetServer p = PresetServer
  { useServer :: Bool,
    server :: ProtoServerWithAuth p
  }

-- this function should be called inside DB transaction to update conditions in the database
-- it returns (current conditions record in the final list, conditions to add, all conditions)
usageConditionsToAdd :: Text -> Text -> UTCTime -> [UsageConditions] -> (UsageConditions, [UsageConditions])
usageConditionsToAdd prevCommit currCommit createdAt = \case
  [] -> (currCond, [prevCond, currCond])
    where
      prevCond = conditions 1 prevCommit 
      currCond = conditions 2 currCommit
  conds -> case find ((currCommit ==) . conditionsCommit) conds of
    Just currCond -> (currCond, [])
    Nothing -> (currCond, [currCond])
      where
        cId = maximum (map conditionsId conds) + 1
        currCond = conditions cId currCommit
  where
    conditions cId commit = UsageConditions {conditionsId = cId, conditionsCommit = commit, notifiedAt = Nothing, createdAt}

groupByOperator :: [ServerOperator] -> [ServerCfg 'PSMP] -> [ServerCfg 'PXFTP] -> [UserServers]
groupByOperator srvOperators smpSrvs xftpSrvs =
  map createOperatorServers (M.toList combinedMap)
  where
    srvOperatorId ServerCfg {operator} = operator
    opId ServerOperator {operatorId} = operatorId
    operatorMap :: Map (Maybe Int64) (Maybe ServerOperator)
    operatorMap = M.fromList [(Just (opId op), Just op) | op <- srvOperators] `M.union` M.singleton Nothing Nothing
    initialMap :: Map (Maybe Int64) ([ServerCfg 'PSMP], [ServerCfg 'PXFTP])
    initialMap = M.fromList [(key, ([], [])) | key <- M.keys operatorMap]
    smpsMap = foldr (\server acc -> M.adjust (\(smps, xftps) -> (server : smps, xftps)) (srvOperatorId server) acc) initialMap smpSrvs
    combinedMap = foldr (\server acc -> M.adjust (\(smps, xftps) -> (smps, server : xftps)) (srvOperatorId server) acc) smpsMap xftpSrvs
    createOperatorServers (key, (groupedSmps, groupedXftps)) =
      UserServers
        { operator = fromMaybe Nothing (M.lookup key operatorMap),
          smpServers = groupedSmps,
          xftpServers = groupedXftps
        }

$(JQ.deriveJSON defaultJSON ''UsageConditions)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CA") ''ConditionsAcceptance)

$(JQ.deriveJSON defaultJSON ''ServerOperator)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "UCA") ''UsageConditionsAction)

$(JQ.deriveJSON defaultJSON ''UserServers)
