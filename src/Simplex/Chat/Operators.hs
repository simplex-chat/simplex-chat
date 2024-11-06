{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Operators where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import Data.FileEmbed
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime, nominalDay)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Language.Haskell.TH.Syntax (lift)
import Simplex.Chat.Operators.Conditions
import Simplex.Chat.Types.Util (textParseJSON)
import Simplex.Messaging.Agent.Env.SQLite (OperatorId, ServerCfg (..), ServerRoles (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fromTextField_, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), ProtoServerWithAuth (..), ProtocolServer (..), ProtocolType (..), SProtocolType (..))
import Simplex.Messaging.Util (safeDecodeUtf8)

usageConditionsCommit :: Text
usageConditionsCommit = "165143a1112308c035ac00ed669b96b60599aa1c"

usageConditionsText :: Text
usageConditionsText =
  $( let s = $(embedFile =<< makeRelativeToProject "PRIVACY.md")
      in [|stripFrontMatter (safeDecodeUtf8 $(lift s))|]
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

usageConditionsAction :: [ServerOperator] -> UsageConditions -> UTCTime -> Maybe UsageConditionsAction
usageConditionsAction operators UsageConditions {createdAt, notifiedAt} now = do
  let enabledOperators = filter (\ServerOperator {enabled} -> enabled) operators
  if null enabledOperators
    then Nothing
    else
      if all conditionsAccepted enabledOperators
        then
          let acceptedForOperators = filter conditionsAccepted operators
           in Just $ UCAAccepted acceptedForOperators
        else
          let acceptForOperators = filter (not . conditionsAccepted) enabledOperators
              deadline = conditionsRequiredOrDeadline createdAt (fromMaybe now notifiedAt)
              showNotice = isNothing notifiedAt
           in Just $ UCAReview acceptForOperators deadline showNotice

conditionsRequiredOrDeadline :: UTCTime -> UTCTime -> Maybe UTCTime
conditionsRequiredOrDeadline createdAt notifiedAtOrNow =
  if notifiedAtOrNow < addUTCTime (14 * nominalDay) createdAt
    then Just $ conditionsDeadline notifiedAtOrNow
    else Nothing -- required
  where
    conditionsDeadline :: UTCTime -> UTCTime
    conditionsDeadline = addUTCTime (31 * nominalDay)

data ConditionsAcceptance
  = CAAccepted {acceptedAt :: Maybe UTCTime}
  | CARequired {deadline :: Maybe UTCTime}
  deriving (Show)

data ServerOperator = ServerOperator
  { operatorId :: OperatorId,
    operatorTag :: Maybe OperatorTag,
    tradeName :: Text,
    legalName :: Maybe Text,
    serverDomains :: [Text],
    conditionsAcceptance :: ConditionsAcceptance,
    enabled :: Bool,
    roles :: ServerRoles
  }
  deriving (Show)

conditionsAccepted :: ServerOperator -> Bool
conditionsAccepted ServerOperator {conditionsAcceptance} = case conditionsAcceptance of
  CAAccepted {} -> True
  _ -> False

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

data UserServersError
  = USEStorageMissing
  | USEProxyMissing
  | USEDuplicateSMP {server :: AProtoServerWithAuth}
  | USEDuplicateXFTP {server :: AProtoServerWithAuth}
  deriving (Show)

validateUserServers :: NonEmpty UserServers -> [UserServersError]
validateUserServers userServers =
  let storageMissing_ = if any (canUseForRole storage) userServers then [] else [USEStorageMissing]
      proxyMissing_ = if any (canUseForRole proxy) userServers then [] else [USEProxyMissing]

      allSMPServers = map (\ServerCfg {server} -> server) $ concatMap (\UserServers {smpServers} -> smpServers) userServers
      duplicateSMPServers = findDuplicatesByHost allSMPServers
      duplicateSMPErrors = map (USEDuplicateSMP . AProtoServerWithAuth SPSMP) duplicateSMPServers

      allXFTPServers = map (\ServerCfg {server} -> server) $ concatMap (\UserServers {xftpServers} -> xftpServers) userServers
      duplicateXFTPServers = findDuplicatesByHost allXFTPServers
      duplicateXFTPErrors = map (USEDuplicateXFTP . AProtoServerWithAuth SPXFTP) duplicateXFTPServers
   in storageMissing_ <> proxyMissing_ <> duplicateSMPErrors <> duplicateXFTPErrors
  where
    canUseForRole :: (ServerRoles -> Bool) -> UserServers -> Bool
    canUseForRole roleSel UserServers {operator, smpServers, xftpServers} = case operator of
      Just ServerOperator {roles} -> roleSel roles
      Nothing -> not (null smpServers) && not (null xftpServers)
    findDuplicatesByHost :: [ProtoServerWithAuth p] -> [ProtoServerWithAuth p]
    findDuplicatesByHost servers =
      let allHosts = concatMap (L.toList . host . protoServer) servers
          hostCounts = M.fromListWith (+) [(host, 1 :: Int) | host <- allHosts]
          duplicateHosts = M.keys $ M.filter (> 1) hostCounts
       in filter (\srv -> any (`elem` duplicateHosts) (L.toList $ host . protoServer $ srv)) servers

$(JQ.deriveJSON defaultJSON ''UsageConditions)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CA") ''ConditionsAcceptance)

$(JQ.deriveJSON defaultJSON ''ServerOperator)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "UCA") ''UsageConditionsAction)

$(JQ.deriveJSON defaultJSON ''UserServers)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "USE") ''UserServersError)
