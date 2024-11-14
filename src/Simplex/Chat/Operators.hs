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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Operators where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import Data.FileEmbed
import Data.Foldable (foldMap')
import Data.IORef
import Data.Int (Int64)
import Data.List (find, foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Scientific (floatingOrInteger)
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
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..), ServerRoles (..), allRoles)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fromTextField_, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), AProtocolType (..), ProtoServerWithAuth (..), ProtocolServer (..), ProtocolType (..), ProtocolTypeI, SProtocolType (..), UserProtocol)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (atomicModifyIORef'_, safeDecodeUtf8)

usageConditionsCommit :: Text
usageConditionsCommit = "165143a1112308c035ac00ed669b96b60599aa1c"

previousConditionsCommit :: Text
previousConditionsCommit = "edf99fcd1d7d38d2501d19608b94c084cf00f2ac"

usageConditionsText :: Text
usageConditionsText =
  $( let s = $(embedFile =<< makeRelativeToProject "PRIVACY.md")
      in [|stripFrontMatter $(lift (safeDecodeUtf8 s))|]
   )

data DBStored = DBStored | DBNew

data SDBStored (s :: DBStored) where
  SDBStored :: SDBStored 'DBStored
  SDBNew :: SDBStored 'DBNew

deriving instance Show (SDBStored s)

class DBStoredI s where sdbStored :: SDBStored s

instance DBStoredI 'DBStored where sdbStored = SDBStored

instance DBStoredI 'DBNew where sdbStored = SDBNew

data DBEntityId' (s :: DBStored) where
  DBEntityId :: Int64 -> DBEntityId' 'DBStored
  DBNewEntity :: DBEntityId' 'DBNew

deriving instance Show (DBEntityId' s)

type DBEntityId = DBEntityId' 'DBStored

type DBNewEntity = DBEntityId' 'DBNew

data OperatorTag = OTSimplex | OTFlux
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
    "flux" -> Just OTFlux
    _ -> Nothing
  textEncode = \case
    OTSimplex -> "simplex"
    OTFlux -> "flux"

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

deriving instance Show AServerOperator

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

data UserOperatorServers = UserOperatorServers
  { operator :: Maybe ServerOperator,
    smpServers :: [UserServer 'PSMP],
    xftpServers :: [UserServer 'PXFTP]
  }
  deriving (Show)

data UpdatedUserOperatorServers = UpdatedUserOperatorServers
  { operator :: Maybe ServerOperator,
    smpServers :: [AUserServer 'PSMP],
    xftpServers :: [AUserServer 'PXFTP]
  }
  deriving (Show)

updatedServers :: UserProtocol p => UpdatedUserOperatorServers -> SProtocolType p -> [AUserServer p]
updatedServers UpdatedUserOperatorServers {smpServers, xftpServers} = \case
  SPSMP -> smpServers
  SPXFTP -> xftpServers

type UserServer p = UserServer' 'DBStored p

type NewUserServer p = UserServer' 'DBNew p

data AUserServer p = forall s. AUS (SDBStored s) (UserServer' s p)

deriving instance Show (AUserServer p)

data UserServer' s p = UserServer
  { serverId :: DBEntityId' s,
    server :: ProtoServerWithAuth p,
    preset :: Bool,
    tested :: Maybe Bool,
    enabled :: Bool,
    deleted :: Bool
  }
  deriving (Show)

data PresetOperator = PresetOperator
  { operator :: Maybe NewServerOperator,
    smp :: [NewUserServer 'PSMP],
    useSMP :: Int,
    xftp :: [NewUserServer 'PXFTP],
    useXFTP :: Int
  }

operatorServers :: UserProtocol p => SProtocolType p -> PresetOperator -> [NewUserServer p]
operatorServers p PresetOperator {smp, xftp} = case p of
  SPSMP -> smp
  SPXFTP -> xftp

operatorServersToUse :: UserProtocol p => SProtocolType p -> PresetOperator -> Int
operatorServersToUse p PresetOperator {useSMP, useXFTP} = case p of
  SPSMP -> useSMP
  SPXFTP -> useXFTP

presetServer :: Bool -> ProtoServerWithAuth p -> NewUserServer p
presetServer = newUserServer_ True

newUserServer :: ProtoServerWithAuth p -> NewUserServer p
newUserServer = newUserServer_ False True

newUserServer_ :: Bool -> Bool -> ProtoServerWithAuth p -> NewUserServer p
newUserServer_ preset enabled server =
  UserServer {serverId = DBNewEntity, server, preset, tested = Nothing, enabled, deleted = False}

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
    addPreset PresetOperator {operator} = case operator of
      Nothing -> id
      Just presetOp -> (storedOp' :)
        where
          storedOp' = case find ((operatorTag presetOp ==) . operatorTag) storedOps of
            Just ServerOperator {operatorId, conditionsAcceptance, enabled, roles} ->
              ASO SDBStored presetOp {operatorId, conditionsAcceptance, enabled, roles}
            Nothing -> ASO SDBNew presetOp

-- This function should be used inside DB transaction to update servers.
updatedUserServers :: forall p. UserProtocol p => SProtocolType p -> NonEmpty PresetOperator -> NonEmpty (NewUserServer p) -> [UserServer p] -> NonEmpty (AUserServer p)
updatedUserServers _ _ randomSrvs [] = L.map (AUS SDBNew) randomSrvs
updatedUserServers p presetOps randomSrvs srvs = 
  fromMaybe (L.map (AUS SDBNew) randomSrvs) (L.nonEmpty updatedSrvs)
  where
    updatedSrvs = map userServer presetSrvs <> map (AUS SDBStored) (filter customServer srvs)
    storedSrvs :: Map (ProtoServerWithAuth p) (UserServer p)
    storedSrvs = foldl' (\ss srv@UserServer {server} -> M.insert server srv ss) M.empty srvs
    customServer :: UserServer p -> Bool
    customServer srv = not (preset srv) && all (`S.notMember` presetHosts) (srvHost srv)
    presetSrvs :: [NewUserServer p]
    presetSrvs = concatMap (operatorServers p) presetOps
    presetHosts :: Set TransportHost
    presetHosts = foldMap' (S.fromList . L.toList . srvHost) presetSrvs
    userServer :: NewUserServer p -> AUserServer p
    userServer srv@UserServer {server} = maybe (AUS SDBNew srv) (AUS SDBStored) (M.lookup server storedSrvs)

srvHost :: UserServer' s p -> NonEmpty TransportHost
srvHost UserServer {server = ProtoServerWithAuth srv _} = host srv

agentServerCfgs :: [(Text, ServerOperator)] -> NonEmpty (NewUserServer p) -> [UserServer' s p] -> NonEmpty (ServerCfg p)
agentServerCfgs opDomains randomSrvs =
  fromMaybe fallbackSrvs . L.nonEmpty . mapMaybe enabledOpAgentServer
  where
    fallbackSrvs = L.map (snd . agentServer) randomSrvs
    enabledOpAgentServer srv =
      let (opEnabled, srvCfg) = agentServer srv
       in if opEnabled then Just srvCfg else Nothing
    agentServer :: UserServer' s p -> (Bool, ServerCfg p)
    agentServer srv@UserServer {server, enabled} =
      case find (\(d, _) -> any (matchingHost d) (srvHost srv)) opDomains of
        Just (_, ServerOperator {operatorId = DBEntityId opId, enabled = opEnabled, roles}) ->
          (opEnabled, ServerCfg {server, enabled, operator = Just opId, roles})
        Nothing ->
          (True, ServerCfg {server, enabled, operator = Nothing, roles = allRoles})

matchingHost :: Text -> TransportHost -> Bool
matchingHost d = \case
  THDomainName h -> d `T.isSuffixOf` T.pack h
  _ -> False

operatorDomains :: [ServerOperator] -> [(Text, ServerOperator)]
operatorDomains = foldr (\op ds -> foldr (\d -> ((d, op) :)) ds (serverDomains op)) []

groupByOperator :: ([ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP]) -> IO [UserOperatorServers]
groupByOperator (ops, smpSrvs, xftpSrvs) = do
  ss <- mapM (\op -> (serverDomains op,) <$> newIORef (UserOperatorServers (Just op) [] [])) ops
  custom <- newIORef $ UserOperatorServers Nothing [] []
  mapM_ (addServer ss custom addSMP) (reverse smpSrvs)
  mapM_ (addServer ss custom addXFTP) (reverse xftpSrvs)
  opSrvs <- mapM (readIORef . snd) ss
  customSrvs <- readIORef custom
  pure $ opSrvs <> [customSrvs]
  where
    addServer :: [([Text], IORef UserOperatorServers)] -> IORef UserOperatorServers -> (UserServer p -> UserOperatorServers -> UserOperatorServers) -> UserServer p -> IO ()
    addServer ss custom add srv = 
      let v = maybe custom snd $ find (\(ds, _) -> any (\d -> any (matchingHost d) (srvHost srv)) ds) ss
       in atomicModifyIORef'_ v $ add srv
    addSMP srv s@UserOperatorServers {smpServers} = (s :: UserOperatorServers) {smpServers = srv : smpServers}
    addXFTP srv s@UserOperatorServers {xftpServers} = (s :: UserOperatorServers) {xftpServers = srv : xftpServers}

data UserServersError
  = USEStorageMissing {protocol :: AProtocolType}
  | USEProxyMissing {protocol :: AProtocolType}
  | USEDuplicateServer {protocol :: AProtocolType, duplicateServer :: AProtoServerWithAuth, duplicateHost :: TransportHost}
  deriving (Show)

validateUserServers :: NonEmpty UpdatedUserOperatorServers -> [UserServersError]
validateUserServers uss =
  missingRolesErr SPSMP storage USEStorageMissing
    <> missingRolesErr SPSMP proxy USEProxyMissing
    <> missingRolesErr SPXFTP storage USEStorageMissing
    <> duplicatServerErrs SPSMP
    <> duplicatServerErrs SPXFTP
  where
    missingRolesErr :: (ProtocolTypeI p, UserProtocol p) => SProtocolType p -> (ServerRoles -> Bool) -> (AProtocolType -> UserServersError) -> [UserServersError]
    missingRolesErr p roleSel err = [err (AProtocolType p) | not hasRole]
      where
        hasRole =
          any (\(AUS _ UserServer {deleted, enabled}) -> enabled && not deleted) $
            concatMap (`updatedServers` p) $ filter roleEnabled (L.toList uss)
        roleEnabled UpdatedUserOperatorServers {operator} =
          maybe True (\ServerOperator {enabled, roles} -> enabled && roleSel roles) operator
    duplicatServerErrs :: (ProtocolTypeI p, UserProtocol p) => SProtocolType p -> [UserServersError]
    duplicatServerErrs p = mapMaybe duplicateErr_ srvs
      where
        srvs =
          filter (\(AUS _ UserServer {deleted}) -> not deleted) $
            concatMap (`updatedServers` p) (L.toList uss)
        duplicateErr_ (AUS _ srv@UserServer {server}) =
          USEDuplicateServer (AProtocolType p) (AProtoServerWithAuth p server)
            <$> find (`S.member` duplicateHosts) (srvHost srv)
        duplicateHosts = snd $ foldl' addHost (S.empty, S.empty) allHosts
        allHosts = concatMap (\(AUS _ srv) -> L.toList $ srvHost srv) srvs
        addHost (hs, dups) h
          | h `S.member` hs = (hs, S.insert h dups)
          | otherwise = (S.insert h hs, dups)

instance ToJSON (DBEntityId' s) where
  toEncoding = \case
    DBEntityId i -> toEncoding i
    DBNewEntity -> JE.null_
  toJSON = \case
    DBEntityId i -> toJSON i
    DBNewEntity -> J.Null

instance DBStoredI s => FromJSON (DBEntityId' s) where
  parseJSON v = case (v, sdbStored @s) of
    (J.Null, SDBNew) -> pure DBNewEntity
    (J.Number n, SDBStored) -> case floatingOrInteger n of
      Left (_ :: Double) -> fail "bad DBEntityId"
      Right i -> pure $ DBEntityId (fromInteger i)
    _ -> fail "bad DBEntityId"
  omittedField = case sdbStored @s of
    SDBStored -> Nothing
    SDBNew -> Just DBNewEntity

$(JQ.deriveJSON defaultJSON ''UsageConditions)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CA") ''ConditionsAcceptance)

instance ToJSON (ServerOperator' s) where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''ServerOperator')
  toJSON = $(JQ.mkToJSON defaultJSON ''ServerOperator')

instance DBStoredI s => FromJSON (ServerOperator' s) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''ServerOperator')

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "UCA") ''UsageConditionsAction)

instance ProtocolTypeI p => ToJSON (UserServer' s p) where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''UserServer')
  toJSON = $(JQ.mkToJSON defaultJSON ''UserServer')

instance (DBStoredI s, ProtocolTypeI p) => FromJSON (UserServer' s p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserServer')

instance ProtocolTypeI p => FromJSON (AUserServer p) where
  parseJSON v = (AUS SDBStored <$> parseJSON v) <|> (AUS SDBNew <$> parseJSON v)

$(JQ.deriveJSON defaultJSON ''UserOperatorServers)

instance FromJSON UpdatedUserOperatorServers where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UpdatedUserOperatorServers)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "USE") ''UserServersError)
