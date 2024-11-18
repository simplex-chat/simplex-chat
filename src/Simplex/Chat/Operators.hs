{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Operators where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import Data.Either (partitionEithers)
import Data.FileEmbed
import Data.Foldable (foldMap')
import Data.Functor.Identity
import Data.IORef
import Data.Int (Int64)
import Data.Kind
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
import Simplex.Chat.Types (User)
import Simplex.Chat.Types.Util (textParseJSON)
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..), ServerRoles (..), allRoles)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, fromTextField_, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtocolType (..), ProtoServerWithAuth (..), ProtocolServer (..), ProtocolType (..), ProtocolTypeI, SProtocolType (..), UserProtocol)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (atomicModifyIORef'_, safeDecodeUtf8)

usageConditionsCommit :: Text
usageConditionsCommit = "a5061f3147165a05979d6ace33960aced2d6ac03"

previousConditionsCommit :: Text
previousConditionsCommit = "11a44dc1fd461a93079f897048b46998db55da5c"

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

deriving instance Eq (DBEntityId' s)

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

data ServerOperatorConditions = ServerOperatorConditions
  { serverOperators :: [ServerOperator],
    currentConditions :: UsageConditions,
    conditionsAction :: Maybe UsageConditionsAction
  }
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
    smpRoles :: ServerRoles,
    xftpRoles :: ServerRoles
  }
  deriving (Show)

operatorRoles :: UserProtocol p => SProtocolType p -> ServerOperator -> ServerRoles
operatorRoles p op = case p of
  SPSMP -> smpRoles op
  SPXFTP -> xftpRoles op

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

data ValidatedUserOperatorServers = ValidatedUserOperatorServers
  { operator :: Maybe ServerOperator,
    smpServers :: [AValidatedServer 'PSMP],
    xftpServers :: [AValidatedServer 'PXFTP]
  }
  deriving (Show)

data AValidatedServer p = forall s. AVS (SDBStored s) (ValidatedServer s p)

deriving instance Show (AValidatedServer p)

type ValidatedServer s p = UserServer_ s ValidatedProtoServer p

data ValidatedProtoServer p = ValidatedProtoServer {unVPS :: Either Text (ProtoServerWithAuth p)}
  deriving (Show)

class UserServersClass u where
  type AServer u = (s :: ProtocolType -> Type) | s -> u
  operator' :: u -> Maybe ServerOperator
  partitionValid :: [AServer u p] -> ([Text], [AUserServer p])
  servers' :: UserProtocol p => SProtocolType p -> u -> [AServer u p]

instance UserServersClass UserOperatorServers where
  type AServer UserOperatorServers = UserServer_ 'DBStored ProtoServerWithAuth
  operator' UserOperatorServers {operator} = operator
  partitionValid ss = ([], map (AUS SDBStored) ss)
  servers' p UserOperatorServers {smpServers, xftpServers} = case p of
    SPSMP -> smpServers
    SPXFTP -> xftpServers

instance UserServersClass UpdatedUserOperatorServers where
  type AServer UpdatedUserOperatorServers = AUserServer
  operator' UpdatedUserOperatorServers {operator} = operator
  partitionValid = ([],)
  servers' p UpdatedUserOperatorServers {smpServers, xftpServers} = case p of
    SPSMP -> smpServers
    SPXFTP -> xftpServers

instance UserServersClass ValidatedUserOperatorServers where
  type AServer ValidatedUserOperatorServers = AValidatedServer
  operator' ValidatedUserOperatorServers {operator} = operator
  partitionValid = partitionEithers . map serverOrErr
    where
      serverOrErr :: AValidatedServer p -> Either Text (AUserServer p)
      serverOrErr (AVS s srv@UserServer {server = server'}) = (\server -> AUS s srv {server}) <$> unVPS server'
  servers' p ValidatedUserOperatorServers {smpServers, xftpServers} = case p of
    SPSMP -> smpServers
    SPXFTP -> xftpServers

type UserServer' s p = UserServer_ s ProtoServerWithAuth p

type UserServer p = UserServer' 'DBStored p

type NewUserServer p = UserServer' 'DBNew p

data AUserServer p = forall s. AUS (SDBStored s) (UserServer' s p)

deriving instance Show (AUserServer p)

data UserServer_ s (srv :: ProtocolType -> Type) (p :: ProtocolType) = UserServer
  { serverId :: DBEntityId' s,
    server :: srv p,
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
  deriving (Show)

pOperator :: PresetOperator -> Maybe NewServerOperator
pOperator PresetOperator {operator} = operator

pServers :: UserProtocol p => SProtocolType p -> PresetOperator -> [NewUserServer p]
pServers p PresetOperator {smp, xftp} = case p of
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

presetUserServers :: [(Maybe PresetOperator, Maybe ServerOperator)] -> [UpdatedUserOperatorServers]
presetUserServers = mapMaybe $ \(presetOp_, op) -> mkUS op <$> presetOp_
  where
    mkUS op PresetOperator {smp, xftp} =
      UpdatedUserOperatorServers op (map (AUS SDBNew) smp) (map (AUS SDBNew) xftp)

-- This function should be used inside DB transaction to update operators.
-- It allows to add/remove/update preset operators in the database preserving enabled and roles settings,
-- and preserves custom operators without tags for forward compatibility.
updatedServerOperators :: NonEmpty PresetOperator -> [ServerOperator] -> [(Maybe PresetOperator, Maybe AServerOperator)]
updatedServerOperators presetOps storedOps =
  foldr addPreset [] presetOps
    <> map (\op -> (Nothing, Just $ ASO SDBStored op)) (filter (isNothing . operatorTag) storedOps)
  where
    -- TODO remove domains of preset operators from custom
    addPreset op = ((Just op, storedOp' <$> pOperator op) :)      
      where
        storedOp' presetOp = case find ((operatorTag presetOp ==) . operatorTag) storedOps of
          Just ServerOperator {operatorId, conditionsAcceptance, enabled, smpRoles, xftpRoles} ->
            ASO SDBStored presetOp {operatorId, conditionsAcceptance, enabled, smpRoles, xftpRoles}
          Nothing -> ASO SDBNew presetOp

-- This function should be used inside DB transaction to update servers.
updatedUserServers :: (Maybe PresetOperator, UserOperatorServers) -> UpdatedUserOperatorServers
updatedUserServers (presetOp_, UserOperatorServers {operator, smpServers, xftpServers}) =
  UpdatedUserOperatorServers {operator, smpServers = smp', xftpServers = xftp'}
  where
    stored = map (AUS SDBStored)
    (smp', xftp') = case presetOp_ of
      Nothing -> (stored smpServers, stored xftpServers)
      Just presetOp -> (updated SPSMP smpServers, updated SPXFTP xftpServers)
        where
          updated :: forall p. UserProtocol p => SProtocolType p -> [UserServer p] -> [AUserServer p]
          updated p srvs = map userServer presetSrvs <> stored (filter customServer srvs)
            where
              storedSrvs :: Map (ProtoServerWithAuth p) (UserServer p)
              storedSrvs = foldl' (\ss srv@UserServer {server} -> M.insert server srv ss) M.empty srvs
              customServer :: UserServer p -> Bool
              customServer srv = not (preset srv) && all (`S.notMember` presetHosts) (srvHost srv)
              presetSrvs :: [NewUserServer p]
              presetSrvs = pServers p presetOp
              presetHosts :: Set TransportHost
              presetHosts = foldMap' (S.fromList . L.toList . srvHost) presetSrvs
              userServer :: NewUserServer p -> AUserServer p
              userServer srv@UserServer {server} = maybe (AUS SDBNew srv) (AUS SDBStored) (M.lookup server storedSrvs)

srvHost :: UserServer' s p -> NonEmpty TransportHost
srvHost UserServer {server = ProtoServerWithAuth srv _} = host srv

agentServerCfgs :: UserProtocol p => SProtocolType p -> [(Text, ServerOperator)] -> [UserServer' s p] -> [ServerCfg p]
agentServerCfgs p opDomains = mapMaybe agentServer
  where
    agentServer :: UserServer' s p -> Maybe (ServerCfg p)
    agentServer srv@UserServer {server, enabled} =
      case find (\(d, _) -> any (matchingHost d) (srvHost srv)) opDomains of
        Just (_, op@ServerOperator {operatorId = DBEntityId opId, enabled = opEnabled})
          | opEnabled -> Just ServerCfg {server, enabled, operator = Just opId, roles = operatorRoles p op}
          | otherwise -> Nothing
        Nothing ->
          Just ServerCfg {server, enabled, operator = Nothing, roles = allRoles}

matchingHost :: Text -> TransportHost -> Bool
matchingHost d = \case
  THDomainName h -> d `T.isSuffixOf` T.pack h
  _ -> False

operatorDomains :: [ServerOperator' s] -> [(Text, ServerOperator' s)]
operatorDomains = foldr (\op ds -> foldr (\d -> ((d, op) :)) ds (serverDomains op)) []

class Box b where
  box :: a -> b a
  unbox :: b a -> a

instance Box Identity where
  box = Identity
  unbox = runIdentity

instance Box ((,) (Maybe a)) where
  box = (Nothing,)
  unbox = snd

groupByOperator :: ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP]) -> IO [UserOperatorServers]
groupByOperator (ops, smpSrvs, xftpSrvs) = map runIdentity <$> groupByOperator_ (map Identity ops, smpSrvs, xftpSrvs)

-- For the initial app start this function relies on tuple being Functor/Box
-- to preserve the information about operator being DBNew or DBStored
groupByOperator' :: ([(Maybe PresetOperator, Maybe ServerOperator)], [UserServer 'PSMP], [UserServer 'PXFTP]) -> IO [(Maybe PresetOperator, UserOperatorServers)]
groupByOperator' = groupByOperator_
{-# INLINE groupByOperator' #-}

groupByOperator_ :: forall f. (Box f, Traversable f) => ([f (Maybe ServerOperator)], [UserServer 'PSMP], [UserServer 'PXFTP]) -> IO [f UserOperatorServers]
groupByOperator_ (ops, smpSrvs, xftpSrvs) = do
  let ops' = mapMaybe sequence ops
      customOp_ = find (isNothing . unbox) ops
  ss <- mapM ((\op -> (serverDomains (unbox op),) <$> newIORef (mkUS . Just <$> op))) ops'
  custom <- newIORef $ maybe (box $ mkUS Nothing) (mkUS <$>) customOp_
  mapM_ (addServer ss custom addSMP) (reverse smpSrvs)
  mapM_ (addServer ss custom addXFTP) (reverse xftpSrvs)
  opSrvs <- mapM (readIORef . snd) ss
  customSrvs <- readIORef custom
  pure $ opSrvs <> [customSrvs]
  where
    mkUS op = UserOperatorServers op [] []
    addServer :: [([Text], IORef (f UserOperatorServers))] -> IORef (f UserOperatorServers) -> (UserServer p -> UserOperatorServers -> UserOperatorServers) -> UserServer p -> IO ()
    addServer ss custom add srv = 
      let v = maybe custom snd $ find (\(ds, _) -> any (\d -> any (matchingHost d) (srvHost srv)) ds) ss
       in atomicModifyIORef'_ v (add srv <$>)
    addSMP srv s@UserOperatorServers {smpServers} = (s :: UserOperatorServers) {smpServers = srv : smpServers}
    addXFTP srv s@UserOperatorServers {xftpServers} = (s :: UserOperatorServers) {xftpServers = srv : xftpServers}

data UserServersError
  = USENoServers {protocol :: AProtocolType, user :: Maybe User}
  | USEStorageMissing {protocol :: AProtocolType, user :: Maybe User}
  | USEProxyMissing {protocol :: AProtocolType, user :: Maybe User}
  | USEInvalidServer {protocol :: AProtocolType, invalidServer :: Text}
  | USEDuplicateServer {protocol :: AProtocolType, duplicateServer :: Text, duplicateHost :: TransportHost}
  deriving (Show)

validateUserServers :: UserServersClass u' => [u'] -> [(User, [UserOperatorServers])] -> [UserServersError]
validateUserServers curr others = currUserErrs <> concatMap otherUserErrs others
  where
    currUserErrs = noServersErrs SPSMP Nothing curr <> noServersErrs SPXFTP Nothing curr <> serverErrs SPSMP curr <> serverErrs SPXFTP curr
    otherUserErrs (user, uss) = noServersErrs SPSMP (Just user) uss <> noServersErrs SPXFTP (Just user) uss
    noServersErrs  :: (UserServersClass u, ProtocolTypeI p, UserProtocol p) => SProtocolType p -> Maybe User -> [u] -> [UserServersError]
    noServersErrs p user uss
      | noServers opEnabled = [USENoServers p' user]
      | otherwise = [USEStorageMissing p' user | noServers (hasRole storage)] <> [USEProxyMissing p' user | noServers (hasRole proxy)]
      where
        p' = AProtocolType p
        noServers cond = not $ any srvEnabled $ snd $ partitionValid $ concatMap (servers' p) $ filter cond uss
        opEnabled = maybe True (\ServerOperator {enabled} -> enabled) . operator'
        hasRole roleSel = maybe True (\op@ServerOperator {enabled} -> enabled && roleSel (operatorRoles p op)) . operator'
        srvEnabled (AUS _ UserServer {deleted, enabled}) = enabled && not deleted
    serverErrs :: (UserServersClass u, ProtocolTypeI p, UserProtocol p) => SProtocolType p -> [u] -> [UserServersError]
    serverErrs p uss = map (USEInvalidServer p') invalidSrvs <> mapMaybe duplicateErr_ srvs
      where
        p' = AProtocolType p
        (invalidSrvs, userSrvs) = partitionValid $ concatMap (servers' p) uss
        srvs = filter (\(AUS _ UserServer {deleted}) -> not deleted) userSrvs
        duplicateErr_ (AUS _ srv@UserServer {server}) =
          USEDuplicateServer p' (safeDecodeUtf8 $ strEncode server)
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

$(JQ.deriveJSON defaultJSON ''ServerOperatorConditions)

instance ProtocolTypeI p => ToJSON (UserServer' s p) where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''UserServer_)
  toJSON = $(JQ.mkToJSON defaultJSON ''UserServer_)

instance (DBStoredI s, ProtocolTypeI p) => FromJSON (UserServer' s p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserServer_)

instance ProtocolTypeI p => FromJSON (AUserServer p) where
  parseJSON v = (AUS SDBStored <$> parseJSON v) <|> (AUS SDBNew <$> parseJSON v)

instance ProtocolTypeI p => FromJSON (ValidatedProtoServer p) where
  parseJSON v = ValidatedProtoServer <$> ((Right <$> parseJSON v) <|> (Left <$> parseJSON v))

instance (DBStoredI s, ProtocolTypeI p) => FromJSON (ValidatedServer s p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserServer_)

instance ProtocolTypeI p => FromJSON (AValidatedServer p) where
  parseJSON v = (AVS SDBStored <$> parseJSON v) <|> (AVS SDBNew <$> parseJSON v)

$(JQ.deriveJSON defaultJSON ''UserOperatorServers)

instance FromJSON UpdatedUserOperatorServers where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UpdatedUserOperatorServers)

instance FromJSON ValidatedUserOperatorServers where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''ValidatedUserOperatorServers)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "USE") ''UserServersError)
