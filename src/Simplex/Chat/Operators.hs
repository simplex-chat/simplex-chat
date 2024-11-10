{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Operators where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import Data.FileEmbed
import Data.Foldable1 (foldMap1)
import Data.IORef
import Data.Int (Int64)
import Data.List (find, foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime, nominalDay)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Language.Haskell.TH.Syntax (lift)
import Simplex.Chat.Operators.Conditions
import Simplex.Chat.Types.Util (textParseJSON)
import Simplex.Messaging.Agent.Env.SQLite (OperatorId, ServerCfg (..), ServerRoles (..), allRoles)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fromTextField_, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), ProtoServerWithAuth (..), ProtocolServer (..), ProtocolType (..), ProtocolTypeI, SProtocolType (..), UserProtocol)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (atomicModifyIORef'_, safeDecodeUtf8)

usageConditionsCommit :: Text
usageConditionsCommit = "165143a1112308c035ac00ed669b96b60599aa1c"

previousConditionsCommit :: Text
previousConditionsCommit = "edf99fcd1d7d38d2501d19608b94c084cf00f2ac"

usageConditionsText :: Text
usageConditionsText =
  $( let s = $(embedFile =<< makeRelativeToProject "PRIVACY.md")
      in [|stripFrontMatter (safeDecodeUtf8 $(lift s))|]
   )

data DBStored = DBStored | DBNew

data SDBStored (s :: DBStored) where
  SDBStored :: SDBStored 'DBStored
  SDBNew :: SDBStored 'DBNew

data DBEntityId' (s :: DBStored) where
  DBEntityId :: Int64 -> DBEntityId' 'DBStored
  DBNewEntity :: DBEntityId' 'DBNew

deriving instance Show (DBEntityId' s)

type DBEntityId = DBEntityId' 'DBStored

type DBNewEntity = DBEntityId' 'DBNew

data ADBEntityId = forall s. AEI (SDBStored s) (DBEntityId' s)

pattern ADBEntityId :: Int64 -> ADBEntityId
pattern ADBEntityId i = AEI SDBStored (DBEntityId i)

pattern ADBNewEntity :: ADBEntityId
pattern ADBNewEntity = AEI SDBNew DBNewEntity

data OperatorTag = OTSimplex | OTXyz
  deriving (Eq, Ord, Show)

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

-- this and other types only define instances of serialization for known DB IDs only,
-- entities without IDs cannot be serialized to JSON
instance FromField DBEntityId where fromField f = DBEntityId <$> fromField f

instance ToField DBEntityId where toField (DBEntityId i) = toField i

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
  if
    | null enabledOperators -> Nothing
    | all conditionsAccepted enabledOperators ->
        let acceptedForOperators = filter conditionsAccepted operators
         in Just $ UCAAccepted acceptedForOperators
    | otherwise ->
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

type ServerOperator = ServerOperator' 'DBStored

type NewServerOperator = ServerOperator' 'DBNew

data AServerOperator = forall s. ASO (SDBStored s) (ServerOperator' s)

data ServerOperator' s = ServerOperator
  { operatorId :: DBEntityId' s,
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
  { operatorId' :: OperatorId,
    enabled' :: Bool,
    roles' :: ServerRoles
  }
  deriving (Show)

data UserOperatorServers = UserOperatorServers
  { operator :: Maybe ServerOperator,
    smpServers :: [UserServer 'PSMP],
    xftpServers :: [UserServer 'PXFTP]
  }
  deriving (Show)

type UserServer p = UserServer' 'DBStored p

type NewUserServer p = UserServer' 'DBNew p

data AUserServer p = forall s. AUS (SDBStored s) (UserServer' s p)

data UserServer' s p = UserServer
  { serverId :: DBEntityId' s,
    server :: ProtoServerWithAuth p,
    preset :: Bool,
    tested :: Maybe Bool,
    enabled :: Bool
  }
  deriving (Show)

data PresetOperator = PresetOperator
  { operator :: NewServerOperator,
    smp :: NonEmpty (NewUserServer 'PSMP),
    useSMP :: Int,
    xftp :: NonEmpty (NewUserServer 'PXFTP),
    useXFTP :: Int
  }

operatorServers :: UserProtocol p => SProtocolType p -> PresetOperator -> NonEmpty (NewUserServer p)
operatorServers p PresetOperator {smp, xftp} = case p of
  SPSMP -> smp
  SPXFTP -> xftp

operatorServersToUse :: UserProtocol p => SProtocolType p -> PresetOperator -> Int
operatorServersToUse p PresetOperator {useSMP, useXFTP} = case p of
  SPSMP -> useSMP
  SPXFTP -> useXFTP

presetServer :: Bool -> ProtoServerWithAuth p -> NewUserServer p
presetServer enabled server =
  UserServer {serverId = DBNewEntity, server, preset = True, tested = Nothing, enabled}

-- This function should be used inside DB transaction to update conditions in the database
-- it evaluates to (conditions to mark as accepted to SimpleX operator, current conditions, and conditions to add)
usageConditionsToAdd :: Bool -> UTCTime -> [UsageConditions] -> (Maybe UsageConditions, UsageConditions, [UsageConditions])
usageConditionsToAdd = usageConditionsToAdd' previousConditionsCommit usageConditionsCommit

-- This function is used in unit tests
usageConditionsToAdd' :: Text -> Text -> Bool -> UTCTime -> [UsageConditions] -> (Maybe UsageConditions, UsageConditions, [UsageConditions])
usageConditionsToAdd' prevCommit sourceCommit newUser createdAt = \case
  []
    | newUser -> (Just sourceCond, sourceCond, [sourceCond])
    | otherwise -> (Just prevCond, sourceCond, [prevCond, sourceCond])
    where
      prevCond = conditions 1 prevCommit
      sourceCond = conditions 2 sourceCommit
  conds
    | hasSourceCond -> (Nothing, last conds, [])
    | otherwise -> (Nothing, sourceCond, [sourceCond])
    where
      hasSourceCond = any ((sourceCommit ==) . conditionsCommit) conds
      sourceCond = conditions cId sourceCommit
      cId = maximum (map conditionsId conds) + 1
  where
    conditions cId commit = UsageConditions {conditionsId = cId, conditionsCommit = commit, notifiedAt = Nothing, createdAt}

-- This function should be used inside DB transaction to update operators.
-- It allows to add/remove/update preset operators in the database preserving enabled and roles settings,
-- and preserves custom operators without tags for forward compatibility.
updatedServerOperators :: NonEmpty PresetOperator -> [ServerOperator] -> [AServerOperator]
updatedServerOperators presetOps storedOps =
  foldr addPreset [] presetOps
    <> map (ASO SDBStored) (filter (isNothing . operatorTag) storedOps)
  where
    -- TODO remove domains of preset operators from custom
    addPreset PresetOperator {operator = presetOp} = (storedOp' :)
      where
        storedOp' = case find ((operatorTag presetOp ==) . operatorTag) storedOps of
          Just ServerOperator {operatorId, conditionsAcceptance, enabled, roles} ->
            ASO SDBStored presetOp {operatorId, conditionsAcceptance, enabled, roles}
          Nothing -> ASO SDBNew presetOp

-- This function should be used inside DB transaction to update servers.
updatedUserServers :: forall p. UserProtocol p => SProtocolType p -> NonEmpty PresetOperator -> NonEmpty (NewUserServer p) -> [UserServer p] -> NonEmpty (AUserServer p)
updatedUserServers p presetOps randomSrvs = \case
  [] -> L.map (AUS SDBNew) randomSrvs
  srvs ->
    L.map (userServer storedSrvs) presetSrvs
      `L.appendList` map (AUS SDBStored) (filter customServer srvs)
    where
      storedSrvs = foldl' (\ss srv@UserServer {server} -> M.insert server srv ss) M.empty srvs
  where
    customServer srv = not (preset srv) && all (`S.notMember` presetHosts) (srvHost srv)
    presetSrvs :: NonEmpty (NewUserServer p)
    presetSrvs = foldMap1 (operatorServers p) presetOps
    presetHosts :: Set TransportHost
    presetHosts = foldMap1 (S.fromList . L.toList . srvHost) presetSrvs
    userServer :: Map (ProtoServerWithAuth p) (UserServer p) -> NewUserServer p -> AUserServer p
    userServer storedSrvs srv@UserServer {server} = maybe (AUS SDBNew srv) (AUS SDBStored) (M.lookup server storedSrvs)

srvHost :: UserServer' s p -> NonEmpty TransportHost
srvHost UserServer {server = ProtoServerWithAuth srv _} = host srv

useServers :: [(Text, ServerOperator)] -> NonEmpty (UserServer' s p) -> NonEmpty (ServerCfg p)
useServers opDomains = L.map agentServer
  where
    agentServer :: UserServer' s p -> ServerCfg p
    agentServer srv@UserServer {server, enabled} =
      case find (\(d, _) -> any (matchingHost d) (srvHost srv)) opDomains of
        Just (_, ServerOperator {operatorId = DBEntityId opId, enabled = opEnabled, roles}) ->
          ServerCfg {server, operator = Just opId, enabled = opEnabled && enabled, roles}
        Nothing ->
          ServerCfg {server, operator = Nothing, enabled, roles = allRoles}

matchingHost :: Text -> TransportHost -> Bool
matchingHost d = \case
  THDomainName h -> d `T.isSuffixOf` T.pack h
  _ -> False

operatorDomains :: [ServerOperator] -> [(Text, ServerOperator)]
operatorDomains = foldr (\op ds -> foldr (\d -> ((d, op) :)) ds (serverDomains op)) []

groupByOperator :: [ServerOperator] -> [UserServer 'PSMP] -> [UserServer 'PXFTP] -> IO [UserOperatorServers]
groupByOperator ops smpSrvs xftpSrvs = do
  ss <- mapM (\op -> (serverDomains op,) <$> newIORef (UserOperatorServers (Just op) [] [])) ops
  custom <- newIORef $ UserOperatorServers Nothing [] []
  mapM_ (addServer ss custom addSMP) (reverse smpSrvs)
  mapM_ (addServer ss custom addXFTP) (reverse xftpSrvs)
  mapM (readIORef . snd) ss
  where
    addServer :: [([Text], IORef UserOperatorServers)] -> IORef UserOperatorServers -> (UserServer p -> UserOperatorServers -> UserOperatorServers) -> UserServer p -> IO ()
    addServer ss custom add srv = 
      let v = maybe custom snd $ find (\(ds, _) -> any (\d -> any (matchingHost d) (srvHost srv)) ds) ss
       in atomicModifyIORef'_ v $ add srv
    addSMP srv s@UserOperatorServers {smpServers} = s {smpServers = srv : smpServers}
    addXFTP srv s@UserOperatorServers {xftpServers} = s {xftpServers = srv : xftpServers}

data UserServersError
  = USEStorageMissing
  | USEProxyMissing
  | USEDuplicateSMP {server :: AProtoServerWithAuth}
  | USEDuplicateXFTP {server :: AProtoServerWithAuth}
  deriving (Show)

validateUserServers :: NonEmpty UserOperatorServers -> [UserServersError]
validateUserServers userServers =
  let storageMissing_ = if any (canUseForRole storage) userServers then [] else [USEStorageMissing]
      proxyMissing_ = if any (canUseForRole proxy) userServers then [] else [USEProxyMissing]
      allSMPServers = map (\UserServer {server} -> server) $ concatMap (\UserOperatorServers {smpServers} -> smpServers) userServers
      duplicateSMPServers = findDuplicatesByHost allSMPServers
      duplicateSMPErrors = map (USEDuplicateSMP . AProtoServerWithAuth SPSMP) duplicateSMPServers

      allXFTPServers = map (\UserServer {server} -> server) $ concatMap (\UserOperatorServers {xftpServers} -> xftpServers) userServers
      duplicateXFTPServers = findDuplicatesByHost allXFTPServers
      duplicateXFTPErrors = map (USEDuplicateXFTP . AProtoServerWithAuth SPXFTP) duplicateXFTPServers
   in storageMissing_ <> proxyMissing_ <> duplicateSMPErrors <> duplicateXFTPErrors
  where
    canUseForRole :: (ServerRoles -> Bool) -> UserOperatorServers -> Bool
    canUseForRole roleSel UserOperatorServers {operator, smpServers, xftpServers} = case operator of
      Just ServerOperator {roles} -> roleSel roles
      Nothing -> not (null smpServers) && not (null xftpServers)
    findDuplicatesByHost :: [ProtoServerWithAuth p] -> [ProtoServerWithAuth p]
    findDuplicatesByHost servers =
      let allHosts = concatMap (L.toList . host . protoServer) servers
          hostCounts = M.fromListWith (+) [(host, 1 :: Int) | host <- allHosts]
          duplicateHosts = M.keys $ M.filter (> 1) hostCounts
       in filter (\srv -> any (`elem` duplicateHosts) (L.toList $ host . protoServer $ srv)) servers

instance ToJSON DBEntityId where
  toEncoding (DBEntityId i) = toEncoding i
  toJSON (DBEntityId i) = toJSON i

instance FromJSON DBEntityId where
  parseJSON v = DBEntityId <$> parseJSON v

$(JQ.deriveJSON defaultJSON ''UsageConditions)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CA") ''ConditionsAcceptance)

instance ToJSON ServerOperator where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''ServerOperator')
  toJSON = $(JQ.mkToJSON defaultJSON ''ServerOperator')

instance FromJSON ServerOperator where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''ServerOperator')

$(JQ.deriveJSON defaultJSON ''OperatorEnabled)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "UCA") ''UsageConditionsAction)

instance ProtocolTypeI p => ToJSON (UserServer p) where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''UserServer')
  toJSON = $(JQ.mkToJSON defaultJSON ''UserServer')

instance ProtocolTypeI p => FromJSON (UserServer p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserServer')

$(JQ.deriveJSON defaultJSON ''UserOperatorServers)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "USE") ''UserServersError)
