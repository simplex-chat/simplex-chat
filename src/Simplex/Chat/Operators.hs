{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime, nominalDay)
import Language.Haskell.TH.Syntax (lift)
import Simplex.Chat.Operators.Conditions
import Simplex.Chat.Types (ConnLinkContact, User)
import Simplex.Chat.Types.Util (textParseJSON)
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..), ServerRoles (..), allRoles)
import Simplex.Messaging.Agent.Protocol (sameConnLinkContact)
import Simplex.Messaging.Agent.Store.DB (FromField (..), ToField (..), fromTextField_)
import Simplex.Messaging.Agent.Store.Entity
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtocolType (..), ProtoServerWithAuth (..), ProtocolServer (..), ProtocolType (..), ProtocolTypeI, SProtocolType (..), UserProtocol)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (atomicModifyIORef'_, safeDecodeUtf8)

usageConditionsCommit :: Text
usageConditionsCommit = "7471fd2af5838dc0467aebc570b5ea75e5df3209"

previousConditionsCommit :: Text
previousConditionsCommit = "a5061f3147165a05979d6ace33960aced2d6ac03"

usageConditionsText :: Text
usageConditionsText =
  $( let s = $(embedFile =<< makeRelativeToProject "PRIVACY.md")
      in [|stripFrontMatter $(lift (safeDecodeUtf8 s))|]
   )

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
  = CAAccepted {acceptedAt :: Maybe UTCTime, autoAccepted :: Bool}
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

data ServerOperatorRoles = ServerOperatorRoles
  { operatorId' :: Int64,
    enabled' :: Bool,
    smpRoles' :: ServerRoles,
    xftpRoles' :: ServerRoles
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
    xftpServers :: [UserServer 'PXFTP],
    chatRelays :: [UserChatRelay]
  }
  deriving (Show)

data UpdatedUserOperatorServers = UpdatedUserOperatorServers
  { operator :: Maybe ServerOperator,
    smpServers :: [AUserServer 'PSMP],
    xftpServers :: [AUserServer 'PXFTP],
    chatRelays :: [AUserChatRelay]
  }
  deriving (Show)

data ValidatedProtoServer p = ValidatedProtoServer {unVPS :: Either Text (ProtoServerWithAuth p)}
  deriving (Show)

class UserServersClass u where
  type AServer u = (s :: ProtocolType -> Type) | s -> u
  type AChatRelay u = (s :: Type) | s -> u
  operator' :: u -> Maybe ServerOperator
  aUserServer' :: AServer u p -> AUserServer p
  servers' :: UserProtocol p => SProtocolType p -> u -> [AServer u p]
  chatRelays' :: u -> [AChatRelay u]
  aUserChatRelay' :: AChatRelay u -> AUserChatRelay

instance UserServersClass UserOperatorServers where
  type AServer UserOperatorServers = UserServer' 'DBStored
  type AChatRelay UserOperatorServers = UserChatRelay' 'DBStored
  operator' UserOperatorServers {operator} = operator
  aUserServer' = AUS SDBStored
  servers' p UserOperatorServers {smpServers, xftpServers} = case p of
    SPSMP -> smpServers
    SPXFTP -> xftpServers
  chatRelays' UserOperatorServers {chatRelays} = chatRelays
  aUserChatRelay' = AUCR SDBStored

instance UserServersClass UpdatedUserOperatorServers where
  type AServer UpdatedUserOperatorServers = AUserServer
  type AChatRelay UpdatedUserOperatorServers = AUserChatRelay
  operator' UpdatedUserOperatorServers {operator} = operator
  aUserServer' = id
  servers' p UpdatedUserOperatorServers {smpServers, xftpServers} = case p of
    SPSMP -> smpServers
    SPXFTP -> xftpServers
  chatRelays' UpdatedUserOperatorServers {chatRelays} = chatRelays
  aUserChatRelay' = id

type UserServer p = UserServer' 'DBStored p

type NewUserServer p = UserServer' 'DBNew p

data AUserServer p = forall s. AUS (SDBStored s) (UserServer' s p)

deriving instance Show (AUserServer p)

data UserServer' s (p :: ProtocolType) = UserServer
  { serverId :: DBEntityId' s,
    server :: ProtoServerWithAuth p,
    preset :: Bool,
    tested :: Maybe Bool,
    enabled :: Bool,
    deleted :: Bool
  }
  deriving (Show)

presetServerAddress :: UserServer' s p -> ProtocolServer p
presetServerAddress UserServer {server = ProtoServerWithAuth srv _} = srv
{-# INLINE presetServerAddress #-}

type UserChatRelay = UserChatRelay' 'DBStored

type NewUserChatRelay = UserChatRelay' 'DBNew

data AUserChatRelay = forall s. AUCR (SDBStored s) (UserChatRelay' s)

deriving instance Show AUserChatRelay

data UserChatRelay' s = UserChatRelay
  { chatRelayId :: DBEntityId' s,
    address :: ConnLinkContact,
    name :: Text,
    domains :: [Text],
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
    useXFTP :: Int,
    chatRelays :: [NewUserChatRelay],
    useChatRelays :: Int
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

presetServer' :: Bool -> ProtocolServer p -> NewUserServer p
presetServer' enabled = presetServer enabled . (`ProtoServerWithAuth` Nothing)
{-# INLINE presetServer' #-}

presetServer :: Bool -> ProtoServerWithAuth p -> NewUserServer p
presetServer = newUserServer_ True
{-# INLINE presetServer #-}

newUserServer :: ProtoServerWithAuth p -> NewUserServer p
newUserServer = newUserServer_ False True
{-# INLINE newUserServer #-}

newUserServer_ :: Bool -> Bool -> ProtoServerWithAuth p -> NewUserServer p
newUserServer_ preset enabled server =
  UserServer {serverId = DBNewEntity, server, preset, tested = Nothing, enabled, deleted = False}

presetChatRelay :: Bool -> Text -> [Text] -> ConnLinkContact -> NewUserChatRelay
presetChatRelay = newChatRelay_ True
{-# INLINE presetChatRelay #-}

newChatRelay :: Text -> [Text] -> ConnLinkContact -> NewUserChatRelay
newChatRelay = newChatRelay_ False True
{-# INLINE newChatRelay #-}

newChatRelay_ :: Bool -> Bool -> Text -> [Text] -> ConnLinkContact -> NewUserChatRelay
newChatRelay_ preset enabled name domains !address =
  UserChatRelay {chatRelayId = DBNewEntity, address, name, domains, preset, tested = Nothing, enabled, deleted = False}

-- This function should be used inside DB transaction to update conditions in the database
-- it evaluates to (current conditions, and conditions to add)
usageConditionsToAdd :: Bool -> UTCTime -> [UsageConditions] -> (UsageConditions, [UsageConditions])
usageConditionsToAdd = usageConditionsToAdd' previousConditionsCommit usageConditionsCommit

-- This function is used in unit tests
usageConditionsToAdd' :: Text -> Text -> Bool -> UTCTime -> [UsageConditions] -> (UsageConditions, [UsageConditions])
usageConditionsToAdd' prevCommit sourceCommit newUser createdAt = \case
  []
    | newUser -> (sourceCond, [sourceCond])
    | otherwise -> (sourceCond, [prevCond, sourceCond])
    where
      prevCond = conditions 1 prevCommit
      sourceCond = conditions 2 sourceCommit
  conds
    | hasSourceCond -> (last conds, [])
    | otherwise -> (sourceCond, [sourceCond])
    where
      hasSourceCond = any ((sourceCommit ==) . conditionsCommit) conds
      sourceCond = conditions cId sourceCommit
      cId = maximum (map conditionsId conds) + 1
  where
    conditions cId commit = UsageConditions {conditionsId = cId, conditionsCommit = commit, notifiedAt = Nothing, createdAt}

presetUserServers :: [(Maybe PresetOperator, Maybe ServerOperator)] -> [UpdatedUserOperatorServers]
presetUserServers = mapMaybe $ \(presetOp_, op) -> mkUS op <$> presetOp_
  where
    mkUS op PresetOperator {smp, xftp, chatRelays} =
      UpdatedUserOperatorServers op (map (AUS SDBNew) smp) (map (AUS SDBNew) xftp) (map (AUCR SDBNew) chatRelays)

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
  UpdatedUserOperatorServers {operator, smpServers = smp', xftpServers = xftp', chatRelays = []}
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
              customServer srv@UserServer {preset} = not preset && all (`S.notMember` presetHosts) (srvHost srv)
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

groupByOperator :: ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP], [UserChatRelay]) -> IO [UserOperatorServers]
groupByOperator (ops, smpSrvs, xftpSrvs, chatRelays) = map runIdentity <$> groupByOperator_ (map Identity ops, smpSrvs, xftpSrvs, chatRelays)

-- For the initial app start this function relies on tuple being Functor/Box
-- to preserve the information about operator being DBNew or DBStored
groupByOperator' :: ([(Maybe PresetOperator, Maybe ServerOperator)], [UserServer 'PSMP], [UserServer 'PXFTP], [UserChatRelay]) -> IO [(Maybe PresetOperator, UserOperatorServers)]
groupByOperator' = groupByOperator_
{-# INLINE groupByOperator' #-}

groupByOperator_ :: forall f. (Box f, Traversable f) => ([f (Maybe ServerOperator)], [UserServer 'PSMP], [UserServer 'PXFTP], [UserChatRelay]) -> IO [f UserOperatorServers]
groupByOperator_ (ops, smpSrvs, xftpSrvs, chatRelays) = do
  let ops' = mapMaybe sequence ops
      customOp_ = find (isNothing . unbox) ops
  ss <- mapM ((\op -> (serverDomains (unbox op),) <$> newIORef (mkUS . Just <$> op))) ops'
  custom <- newIORef $ maybe (box $ mkUS Nothing) (mkUS <$>) customOp_
  mapM_ (addServer ss custom addSMP) (reverse smpSrvs)
  mapM_ (addServer ss custom addXFTP) (reverse xftpSrvs)
  mapM_ (addChatRelay ss custom) chatRelays
  opSrvs <- mapM (readIORef . snd) ss
  customSrvs <- readIORef custom
  pure $ opSrvs <> [customSrvs]
  where
    mkUS op = UserOperatorServers op [] [] []
    addServer :: [([Text], IORef (f UserOperatorServers))] -> IORef (f UserOperatorServers) -> (UserServer p -> UserOperatorServers -> UserOperatorServers) -> UserServer p -> IO ()
    addServer ss custom add srv =
      let v = maybe custom snd $ find (\(ds, _) -> any (\d -> any (matchingHost d) (srvHost srv)) ds) ss
       in atomicModifyIORef'_ v (add srv <$>)
    addSMP srv s@UserOperatorServers {smpServers} = (s :: UserOperatorServers) {smpServers = srv : smpServers}
    addXFTP srv s@UserOperatorServers {xftpServers} = (s :: UserOperatorServers) {xftpServers = srv : xftpServers}
    addChatRelay :: [([Text], IORef (f UserOperatorServers))] -> IORef (f UserOperatorServers) -> UserChatRelay -> IO ()
    addChatRelay ss custom chatRelay =
      let v = maybe custom snd $ find (\(ds, _) -> any (`elem` domains chatRelay) ds) ss
       in atomicModifyIORef'_ v (addCRelay <$>)
      where
        addCRelay s@UserOperatorServers {chatRelays} = (s :: UserOperatorServers) {chatRelays = chatRelay : chatRelays}

data UserServersError
  = USENoServers {protocol :: AProtocolType, user :: Maybe User}
  | USEStorageMissing {protocol :: AProtocolType, user :: Maybe User}
  | USEProxyMissing {protocol :: AProtocolType, user :: Maybe User}
  | USEDuplicateServer {protocol :: AProtocolType, duplicateServer :: Text, duplicateHost :: TransportHost}
  | USEDuplicateChatRelayName {duplicateChatRelay :: Text}
  | USEDuplicateChatRelayAddress {duplicateChatRelay :: Text, duplicateAddress :: ConnLinkContact}
  deriving (Show)

data UserServersWarning = USWNoChatRelays {user :: Maybe User}
  deriving (Show)

validateUserServers :: UserServersClass u' => [u'] -> [(User, [UserOperatorServers])] -> ([UserServersError], [UserServersWarning])
validateUserServers curr others = (currUserErrs <> concatMap otherUserErrs others, currUserWarns <> concatMap otherUserWarns others)
  where
    currUserErrs = noServersErrs SPSMP Nothing curr <> noServersErrs SPXFTP Nothing curr <> serverErrs SPSMP curr <> serverErrs SPXFTP curr <> chatRelayErrs curr
    otherUserErrs (user, uss) = noServersErrs SPSMP (Just user) uss <> noServersErrs SPXFTP (Just user) uss
    noServersErrs :: (UserServersClass u, ProtocolTypeI p, UserProtocol p) => SProtocolType p -> Maybe User -> [u] -> [UserServersError]
    noServersErrs p user uss
      | noServers opEnabled = [USENoServers p' user]
      | otherwise = [USEStorageMissing p' user | noServers (hasRole storage)] <> [USEProxyMissing p' user | noServers (hasRole proxy)]
      where
        p' = AProtocolType p
        noServers cond = not $ any srvEnabled $ userServers p $ filter cond uss
        hasRole roleSel = maybe True (\op@ServerOperator {enabled} -> enabled && roleSel (operatorRoles p op)) . operator'
        srvEnabled (AUS _ UserServer {deleted, enabled}) = enabled && not deleted
    serverErrs :: (UserServersClass u, ProtocolTypeI p, UserProtocol p) => SProtocolType p -> [u] -> [UserServersError]
    serverErrs p uss = mapMaybe duplicateErr_ srvs
      where
        p' = AProtocolType p
        srvs = filter (\(AUS _ UserServer {deleted}) -> not deleted) $ userServers p uss
        duplicateErr_ (AUS _ srv@UserServer {server}) =
          USEDuplicateServer p' (safeDecodeUtf8 $ strEncode server)
            <$> find (`S.member` duplicateHosts) (srvHost srv)
        duplicateHosts = snd $ foldl' addDuplicate (S.empty, S.empty) allHosts
        allHosts = concatMap (\(AUS _ srv) -> L.toList $ srvHost srv) srvs
    userServers :: (UserServersClass u, UserProtocol p) => SProtocolType p -> [u] -> [AUserServer p]
    userServers p = map aUserServer' . concatMap (servers' p)
    chatRelayErrs :: UserServersClass u => [u] -> [UserServersError]
    chatRelayErrs uss = concatMap duplicateErrs_ speers
      where
        speers = filter (\(AUCR _ UserChatRelay {deleted}) -> not deleted) $ userChatRelays uss
        duplicateErrs_ (AUCR _ UserChatRelay {name, address}) =
          [USEDuplicateChatRelayName name | name `elem` duplicateNames]
            <> [USEDuplicateChatRelayAddress name address | address `elem` duplicateAddresses]
        duplicateNames = snd $ foldl' addDuplicate (S.empty, S.empty) allNames
        allNames = map (\(AUCR _ speer) -> name speer) speers
        duplicateAddresses = snd $ foldl' addAddress ([], []) allAddresses
        allAddresses = map (\(AUCR _ speer) -> address speer) speers
        addAddress :: ([ConnLinkContact], [ConnLinkContact]) -> ConnLinkContact -> ([ConnLinkContact], [ConnLinkContact])
        addAddress (xs, dups) x
          | any (sameConnLinkContact x) xs = (xs, x : dups)
          | otherwise = (x : xs, dups)
    currUserWarns = noChatRelaysWarns Nothing curr
    otherUserWarns (user, uss) = noChatRelaysWarns (Just user) uss
    noChatRelaysWarns :: UserServersClass u => Maybe User -> [u] -> [UserServersWarning]
    noChatRelaysWarns user uss
      | noChatRelays opEnabled = [USWNoChatRelays user]
      | otherwise = []
      where
        noChatRelays cond = not $ any speerEnabled $ userChatRelays $ filter cond uss
        speerEnabled (AUCR _ UserChatRelay {deleted, enabled}) = enabled && not deleted
    userChatRelays :: UserServersClass u => [u] -> [AUserChatRelay]
    userChatRelays = map aUserChatRelay' . concatMap chatRelays'
    opEnabled :: UserServersClass u => u -> Bool
    opEnabled = maybe True (\ServerOperator {enabled} -> enabled) . operator'
    addDuplicate :: Ord a => (Set a, Set a) -> a -> (Set a, Set a)
    addDuplicate (xs, dups) x
      | x `S.member` xs = (xs, S.insert x dups)
      | otherwise = (S.insert x xs, dups)

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
  toEncoding = $(JQ.mkToEncoding defaultJSON ''UserServer')
  toJSON = $(JQ.mkToJSON defaultJSON ''UserServer')

instance (DBStoredI s, ProtocolTypeI p) => FromJSON (UserServer' s p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserServer')

instance ProtocolTypeI p => FromJSON (AUserServer p) where
  parseJSON v = (AUS SDBStored <$> parseJSON v) <|> (AUS SDBNew <$> parseJSON v)

instance ToJSON (UserChatRelay' s) where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''UserChatRelay')
  toJSON = $(JQ.mkToJSON defaultJSON ''UserChatRelay')

instance DBStoredI s => FromJSON (UserChatRelay' s) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserChatRelay')

instance FromJSON AUserChatRelay where
  parseJSON v = (AUCR SDBStored <$> parseJSON v) <|> (AUCR SDBNew <$> parseJSON v)

$(JQ.deriveJSON defaultJSON ''UserOperatorServers)

instance FromJSON UpdatedUserOperatorServers where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UpdatedUserOperatorServers)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "USE") ''UserServersError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "USW") ''UserServersWarning)
