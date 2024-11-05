{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Operators where

import Control.Monad (foldM)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import Data.FileEmbed
import Data.Foldable1 (fold1)
import Data.Int (Int64)
import Data.List (find, foldl')
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
import Simplex.Messaging.Protocol (AProtoServerWithAuth, ProtoServerWithAuth, ProtocolType (..), ProtocolTypeI)
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
  deriving (Eq, Show)

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
  { operatorId :: Maybe OperatorId,
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

data UserServers = UserServers
  { operator :: Maybe ServerOperator,
    smpServers :: [UserServer 'PSMP],
    xftpServers :: [UserServer 'PXFTP]
  }
  deriving (Show)

data UserServer p = UserServer
  { serverId :: Maybe Int64,
    serverOperatorId :: Maybe OperatorId,
    server :: ProtoServerWithAuth p,
    tested :: Maybe Bool,
    enabled :: Bool
  }
  deriving (Show)

data PresetOperatorServers = PresetOperatorServers
  { operator :: ServerOperator,
    presetSMPServers :: NonEmpty (PresetServer 'PSMP),
    presetXFTPServers :: NonEmpty (PresetServer 'PXFTP),
    useSMP :: Int,
    useXFTP :: Int
  }

data PresetServer p = PresetServer
  { useServer :: Bool,
    server :: ProtoServerWithAuth p
  }

-- This function should be used inside DB transaction to update conditions in the database
-- it returns (conditions to mark as accepted to SimpleX operator, conditions to add)
usageConditionsToAdd :: Bool -> Text -> Text -> UTCTime -> [UsageConditions] -> (Maybe UsageConditions, [UsageConditions])
usageConditionsToAdd newUser prevCommit sourceCommit createdAt = \case
  []
    | newUser -> (Just sourceCond, [sourceCond])
    | otherwise -> (Just prevCond, [prevCond, sourceCond])
    where
      prevCond = conditions 1 prevCommit 
      sourceCond = conditions 2 sourceCommit
  conds -> (Nothing, if hasSourceCond then [] else [sourceCond])
    where
      hasSourceCond = any ((sourceCommit ==) . conditionsCommit) conds
      sourceCond = conditions cId sourceCommit
      cId = maximum (map conditionsId conds) + 1
  where
    conditions cId commit = UsageConditions {conditionsId = cId, conditionsCommit = commit, notifiedAt = Nothing, createdAt}

-- This function should be used inside DB transaction to update operators.
-- It allows to add/remove/update preset operators in the database preserving enabled and roles settings,
-- and preserves custom operators without tags for forward compatibility.
updatedServerOperators :: NonEmpty PresetOperatorServers -> [ServerOperator] -> [ServerOperator]
updatedServerOperators presetSrvs storedOps =
  foldr addPreset [] presetSrvs <> filter (isNothing . operatorTag) storedOps -- TODO remove domains of preset operators from custom
  where
    addPreset PresetOperatorServers {operator = presetOp} = (storedOp' :)
      where
        storedOp' = case find ((operatorTag presetOp ==) . operatorTag) storedOps of
          Just ServerOperator {operatorId, conditionsAcceptance, enabled, roles} ->
            presetOp {operatorId, conditionsAcceptance, enabled, roles}
          Nothing -> presetOp


data UpdatedServers p = UpdatedServers
  { toUpsert :: [UserServer p],
    toDelete :: [Int64]
  }

-- This function should be used inside DB transaction to update servers.
-- It assumes that the list of operators was amended using updatedServerOperators,
-- that [ServerOperator] has the same operators as [PresetOperatorServers],
-- and that they all have serverOperatorId set.
updatedUserServers :: NonEmpty PresetOperatorServers -> [ServerOperator] -> [UserServer 'PSMP] -> [UserServer 'PXFTP] -> Either String ([UserServer 'PSMP], [UserServer 'PXFTP])
updatedUserServers presetSrvs storedOps smpSrvs xftpSrvs = do
  smpSrvs' <- updatedSrvs useSMP smpSrvs =<< presetSrvsToStore presetSMPServers
  xftpSrvs' <- updatedSrvs useXFTP xftpSrvs =<< presetSrvsToStore presetXFTPServers
  pure (smpSrvs', xftpSrvs')
  where
    presetSrvsToStore :: forall p. (PresetOperatorServers -> NonEmpty (PresetServer p)) -> Either String (NonEmpty (Bool, UserServer p))
    presetSrvsToStore presetSel = fold1 <$> mapM operatorSrvs presetSrvs
      where
        operatorSrvs :: PresetOperatorServers -> Either String (NonEmpty (Bool, UserServer p))
        operatorSrvs op@PresetOperatorServers {operator} = case find ((operatorTag operator ==) . operatorTag) storedOps of
          Nothing -> Left "preset operator not stored"
          Just op' -> Right $ L.map (userSrv op') (presetSel op)
        userSrv op PresetServer {server, useServer} =
          let srv = UserServer {serverId = Nothing, serverOperatorId = operatorId op, server, tested = Nothing, enabled = False}
           in (useServer, srv)

    updatedSrvs :: forall p. (PresetOperatorServers -> Int) -> [UserServer p] -> NonEmpty (Bool, UserServer p) -> Either String [UserServer p]
    updatedSrvs useSel storedSrvs presetSrvs =
      fmap enabledSrvs . addOtherServers =<< foldM updatedSrv (storedSrvs', []) presetSrvs
      where
        storedSrvs' :: Map (ProtoServerWithAuth p) (UserServer p)
        storedSrvs' = foldl' (\m us@UserServer {server} -> M.insert server us m) M.empty storedSrvs
        updatedSrv :: (Map (ProtoServerWithAuth p) (UserServer p), [(Bool, UserServer p)]) -> (Bool, UserServer p) -> Either String (Map (ProtoServerWithAuth p) (UserServer p), [(Bool, UserServer p)])
        updatedSrv srvs srv = undefined
        addOtherServers :: (Map (ProtoServerWithAuth p) (UserServer p), [(Bool, UserServer p)]) -> Either String [(Bool, UserServer p)]
        addOtherServers = undefined
        enabledSrvs :: [(Bool, UserServer p)] -> [UserServer p]
        enabledSrvs = undefined

    -- addSrv srv@ServerCfg {server = ProtocolServerWithAuth ProtocolServer {host}} uss =
    --   case find (\us -> any [\h -> any (\d -> d `T.isSuffixOf` ) serverDomains (operator us)] host) uss of
    --     Just opId 
    --   where
    --     hasOperatorDomain ServerCfg {server = ProtocolServerWithAuth ProtocolServer {host}} us


    --   addSrv srv uss = ... а тут просто найти оператора в списке и вставить ему сервер через add и как то ругнуться если его нет (но такого не должно быть). Либо вообще есть вариант сразу читать в этом формате - сначала прочитать операторов и в цикле читать серверы каждого - это вот может быть еще проще

-- groupByOperator :: [ServerOperator] -> [ServerCfg 'PSMP] -> [ServerCfg 'PXFTP] -> [UserServers]
-- groupByOperator srvOperators smpSrvs xftpSrvs =
--   map createOperatorServers (M.toList combinedMap)
--   where
--     srvOperatorId ServerCfg {operator} = operator
--     opId ServerOperator {operatorId} = operatorId
--     operatorMap :: Map (Maybe Int64) (Maybe ServerOperator)
--     operatorMap = M.fromList [(Just (opId op), Just op) | op <- srvOperators] `M.union` M.singleton Nothing Nothing
--     initialMap :: Map (Maybe Int64) ([ServerCfg 'PSMP], [ServerCfg 'PXFTP])
--     initialMap = M.fromList [(key, ([], [])) | key <- M.keys operatorMap]
--     smpsMap = foldr (\server acc -> M.adjust (\(smps, xftps) -> (server : smps, xftps)) (srvOperatorId server) acc) initialMap smpSrvs
--     combinedMap = foldr (\server acc -> M.adjust (\(smps, xftps) -> (smps, server : xftps)) (srvOperatorId server) acc) smpsMap xftpSrvs
--     createOperatorServers (key, (groupedSmps, groupedXftps)) =
--       UserServers
--         { operator = fromMaybe Nothing (M.lookup key operatorMap),
--           smpServers = groupedSmps,
--           xftpServers = groupedXftps
--         }

data UserServersError
  = USEStorageMissing
  | USEProxyMissing
  | USEDuplicate {server :: AProtoServerWithAuth}
  deriving (Show)

validateUserServers :: NonEmpty UserServers -> [UserServersError]
validateUserServers userServers =
  let storageMissing_ = if any (canUseForRole storage) userServers then [] else [USEStorageMissing]
      proxyMissing_ = if any (canUseForRole proxy) userServers then [] else [USEProxyMissing]
      -- TODO duplicate errors
      -- allSMPServers =
      --   map (\ServerCfg {server} -> server) $
      --     concatMap (\UserServers {smpServers} -> smpServers) userServers
   in storageMissing_ <> proxyMissing_ -- <> duplicateErrors
  where
    canUseForRole :: (ServerRoles -> Bool) -> UserServers -> Bool
    canUseForRole roleSel UserServers {operator, smpServers, xftpServers} = case operator of
      Just ServerOperator {roles} -> roleSel roles
      Nothing -> not (null smpServers) && not (null xftpServers)

$(JQ.deriveJSON defaultJSON ''UsageConditions)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CA") ''ConditionsAcceptance)

$(JQ.deriveJSON defaultJSON ''ServerOperator)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "UCA") ''UsageConditionsAction)

instance ProtocolTypeI p => ToJSON (UserServer p) where
  toEncoding = $(JQ.mkToEncoding defaultJSON ''UserServer)
  toJSON = $(JQ.mkToJSON defaultJSON ''UserServer)

instance ProtocolTypeI p => FromJSON (UserServer p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserServer)

$(JQ.deriveJSON defaultJSON ''UserServers)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "USE") ''UserServersError)
