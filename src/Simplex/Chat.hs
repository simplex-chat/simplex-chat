{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.Bifunctor (bimap, second)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Simplex.Chat.Controller
import Simplex.Chat.Library.Commands
import Simplex.Chat.Operators
import Simplex.Chat.Operators.Presets
import Simplex.Chat.Options
import Simplex.Chat.Options.DB
import Simplex.Chat.Protocol
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Chat.Util (shuffle)
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..), InitialAgentServers (..), ServerCfg (..), allRoles, createAgentStore, defaultAgentConfig, presetServerCfg)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.Common (DBStore (dbNew))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation (..), MigrationError)
import Simplex.Messaging.Client (defaultNetworkConfig)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Protocol (ProtoServerWithAuth (..), ProtocolType (..), SProtocolType (..), SubscriptionMode (..), UserProtocol)
import qualified Simplex.Messaging.TMap as TM
import qualified UnliftIO.Exception as E
import UnliftIO.STM

defaultChatConfig :: ChatConfig
defaultChatConfig =
  ChatConfig
    { agentConfig =
        defaultAgentConfig
          { tcpPort = Nothing, -- agent does not listen to TCP
            tbqSize = 1024
          },
      chatVRange = supportedChatVRange,
      confirmMigrations = MCConsole,
      -- this property should NOT use operator = Nothing
      -- non-operator servers can be passed via options
      presetServers =
        PresetServers
          { operators =
              [ PresetOperator
                  { operator = Just operatorSimpleXChat,
                    smp = simplexChatSMPServers,
                    useSMP = 4,
                    xftp = map (presetServer True) $ L.toList defaultXFTPServers,
                    useXFTP = 3
                  },
                PresetOperator
                  { operator = Just operatorFlux,
                    smp = fluxSMPServers,
                    useSMP = 3,
                    xftp = fluxXFTPServers,
                    useXFTP = 3
                  }
              ],
            ntf = _defaultNtfServers,
            netCfg = defaultNetworkConfig
          },
      -- please note: if these servers are changed, this option needs to be split to two,
      -- to have a different set of servers on the receiving end and on the sending end.
      -- To preserve backward compatibility receiving end should update before the sending.
      shortLinkPresetServers = allPresetServers,
      presetDomains = [".simplex.im", ".simplexonflux.com"],
      tbqSize = 1024,
      fileChunkSize = 15780, -- do not change
      xftpDescrPartSize = 14000,
      inlineFiles = defaultInlineFilesConfig,
      autoAcceptFileSize = 0,
      showReactions = False,
      showReceipts = False,
      logLevel = CLLImportant,
      subscriptionEvents = False,
      hostEvents = False,
      testView = False,
      initialCleanupManagerDelay = 30 * 1000000, -- 30 seconds
      cleanupManagerInterval = 30 * 60, -- 30 minutes
      cleanupManagerStepDelay = 3 * 1000000, -- 3 seconds
      ciExpirationInterval = 30 * 60 * 1000000, -- 30 minutes
      coreApi = False,
      highlyAvailable = False,
      deviceNameForRemote = "",
      chatHooks = defaultChatHooks
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

createChatDatabase :: ChatDbOpts -> MigrationConfirmation -> IO (Either MigrationError ChatDatabase)
createChatDatabase chatDbOpts confirmMigrations = runExceptT $ do
  chatStore <- ExceptT $ createChatStore (toDBOpts chatDbOpts chatSuffix False) confirmMigrations
  agentStore <- ExceptT $ createAgentStore (toDBOpts chatDbOpts agentSuffix False) confirmMigrations
  pure ChatDatabase {chatStore, agentStore}

newChatController :: ChatDatabase -> Maybe User -> ChatConfig -> ChatOpts -> Bool -> IO ChatController
newChatController
  ChatDatabase {chatStore, agentStore}
  user
  cfg@ChatConfig {agentConfig = aCfg, presetServers, inlineFiles, deviceNameForRemote, confirmMigrations}
  ChatOpts {coreOptions = CoreChatOpts {smpServers, xftpServers, simpleNetCfg, logLevel, logConnections, logServerHosts, logFile, tbqSize, deviceName, highlyAvailable, yesToUpMigrations}, optFilesFolder, optTempDirectory, showReactions, allowInstantFiles, autoAcceptFileSize}
  backgroundMode = do
    let inlineFiles' = if allowInstantFiles || autoAcceptFileSize > 0 then inlineFiles else inlineFiles {sendChunks = 0, receiveInstant = False}
        confirmMigrations' = if confirmMigrations == MCConsole && yesToUpMigrations then MCYesUp else confirmMigrations
        config = cfg {logLevel, showReactions, tbqSize, subscriptionEvents = logConnections, hostEvents = logServerHosts, presetServers = presetServers', inlineFiles = inlineFiles', autoAcceptFileSize, highlyAvailable, confirmMigrations = confirmMigrations'}
        firstTime = dbNew chatStore
    currentUser <- newTVarIO user
    randomPresetServers <- chooseRandomServers presetServers'
    let rndSrvs = L.toList randomPresetServers
        operatorWithId (i, op) = (\o -> o {operatorId = DBEntityId i}) <$> pOperator op
        opDomains = operatorDomains $ mapMaybe operatorWithId $ zip [1 ..] rndSrvs
    agentSMP <- randomServerCfgs "agent SMP servers" SPSMP opDomains rndSrvs
    agentXFTP <- randomServerCfgs "agent XFTP servers" SPXFTP opDomains rndSrvs
    let randomAgentServers = RandomAgentServers {smpServers = agentSMP, xftpServers = agentXFTP}
    currentRemoteHost <- newTVarIO Nothing
    servers <- withTransaction chatStore $ \db -> agentServers db config randomPresetServers randomAgentServers
    smpAgent <- getSMPAgentClient aCfg {tbqSize} servers agentStore backgroundMode
    agentAsync <- newTVarIO Nothing
    random <- liftIO C.newRandom
    eventSeq <- newTVarIO 0
    inputQ <- newTBQueueIO tbqSize
    outputQ <- newTBQueueIO tbqSize
    connNetworkStatuses <- TM.emptyIO
    subscriptionMode <- newTVarIO SMSubscribe
    chatLock <- newEmptyTMVarIO
    entityLocks <- TM.emptyIO
    sndFiles <- newTVarIO M.empty
    rcvFiles <- newTVarIO M.empty
    currentCalls <- TM.emptyIO
    localDeviceName <- newTVarIO $ fromMaybe deviceNameForRemote deviceName
    multicastSubscribers <- newTMVarIO 0
    remoteSessionSeq <- newTVarIO 0
    remoteHostSessions <- TM.emptyIO
    remoteHostsFolder <- newTVarIO Nothing
    remoteCtrlSession <- newTVarIO Nothing
    filesFolder <- newTVarIO optFilesFolder
    chatStoreChanged <- newTVarIO False
    expireCIThreads <- TM.emptyIO
    expireCIFlags <- TM.emptyIO
    cleanupManagerAsync <- newTVarIO Nothing
    timedItemThreads <- TM.emptyIO
    chatActivated <- newTVarIO True
    showLiveItems <- newTVarIO False
    encryptLocalFiles <- newTVarIO False
    tempDirectory <- newTVarIO optTempDirectory
    assetsDirectory <- newTVarIO Nothing
    contactMergeEnabled <- newTVarIO True
    pure
      ChatController
        { firstTime,
          currentUser,
          randomPresetServers,
          randomAgentServers,
          currentRemoteHost,
          smpAgent,
          agentAsync,
          chatStore,
          chatStoreChanged,
          random,
          eventSeq,
          inputQ,
          outputQ,
          connNetworkStatuses,
          subscriptionMode,
          chatLock,
          entityLocks,
          sndFiles,
          rcvFiles,
          currentCalls,
          localDeviceName,
          multicastSubscribers,
          remoteSessionSeq,
          remoteHostSessions,
          remoteHostsFolder,
          remoteCtrlSession,
          config,
          filesFolder,
          expireCIThreads,
          expireCIFlags,
          cleanupManagerAsync,
          timedItemThreads,
          chatActivated,
          showLiveItems,
          encryptLocalFiles,
          tempDirectory,
          assetsDirectory,
          logFilePath = logFile,
          contactMergeEnabled
        }
    where
      presetServers' :: PresetServers
      presetServers' = presetServers {operators = operators', netCfg = netCfg'}
        where
          PresetServers {operators, netCfg} = presetServers
          netCfg' = updateNetworkConfig netCfg simpleNetCfg
          operators' = case (smpServers, xftpServers) of
            ([], []) -> operators
            (smpSrvs, []) -> L.map disableSMP operators <> [custom smpSrvs []]
            ([], xftpSrvs) -> L.map disableXFTP operators <> [custom [] xftpSrvs]
            (smpSrvs, xftpSrvs) -> [custom smpSrvs xftpSrvs]
          disableSMP op@PresetOperator {smp} = (op :: PresetOperator) {smp = map disableSrv smp}
          disableXFTP op@PresetOperator {xftp} = (op :: PresetOperator) {xftp = map disableSrv xftp}
          disableSrv :: forall p. NewUserServer p -> NewUserServer p
          disableSrv srv = (srv :: NewUserServer p) {enabled = False}
          custom smpSrvs xftpSrvs =
            PresetOperator
              { operator = Nothing,
                smp = map newUserServer smpSrvs,
                useSMP = 0,
                xftp = map newUserServer xftpSrvs,
                useXFTP = 0
              }
      randomServerCfgs :: UserProtocol p => String -> SProtocolType p -> [(Text, ServerOperator)] -> [PresetOperator] -> IO (NonEmpty (ServerCfg p))
      randomServerCfgs name p opDomains rndSrvs =
        toJustOrError name $ L.nonEmpty $ agentServerCfgs p opDomains $ concatMap (pServers p) rndSrvs
      agentServers :: DB.Connection -> ChatConfig -> NonEmpty PresetOperator -> RandomAgentServers -> IO InitialAgentServers
      agentServers db ChatConfig {presetServers = PresetServers {ntf, netCfg}, presetDomains} presetOps as = do
        users <- getUsers db
        ops <- getUpdateServerOperators db presetOps (null users)
        let opDomains = operatorDomains $ mapMaybe snd ops
        (smp', xftp') <- unzip <$> mapM (getServers ops opDomains) users
        pure InitialAgentServers {smp = M.fromList (optServers smp' smpServers), xftp = M.fromList (optServers xftp' xftpServers), ntf, netCfg, presetDomains}
        where
          optServers :: [(UserId, NonEmpty (ServerCfg p))] -> [ProtoServerWithAuth p] -> [(UserId, NonEmpty (ServerCfg p))]
          optServers srvs overrides_ = case L.nonEmpty overrides_ of
            Just overrides -> map (second $ const $ L.map (presetServerCfg True allRoles Nothing) overrides) srvs
            Nothing -> srvs
          getServers :: [(Maybe PresetOperator, Maybe ServerOperator)] -> [(Text, ServerOperator)] -> User -> IO ((UserId, NonEmpty (ServerCfg 'PSMP)), (UserId, NonEmpty (ServerCfg 'PXFTP)))
          getServers ops opDomains user' = do
            smpSrvs <- getProtocolServers db SPSMP user'
            xftpSrvs <- getProtocolServers db SPXFTP user'
            uss <- groupByOperator' (ops, smpSrvs, xftpSrvs)
            ts <- getCurrentTime
            uss' <- mapM (setUserServers' db user' ts . updatedUserServers) uss
            let auId = aUserId user'
            pure $ bimap (auId,) (auId,) $ useServers as opDomains uss'

chooseRandomServers :: PresetServers -> IO (NonEmpty PresetOperator)
chooseRandomServers PresetServers {operators} =
  forM operators $ \op -> do
    smp' <- opSrvs SPSMP op
    xftp' <- opSrvs SPXFTP op
    pure (op :: PresetOperator) {smp = smp', xftp = xftp'}
  where
    opSrvs :: forall p. UserProtocol p => SProtocolType p -> PresetOperator -> IO [NewUserServer p]
    opSrvs p op = do
      let srvs = pServers p op
          toUse = operatorServersToUse p op
          (enbldSrvs, dsbldSrvs) = partition (\UserServer {enabled} -> enabled) srvs
      if toUse <= 0 || toUse >= length enbldSrvs
        then pure srvs
        else do
          (enbldSrvs', srvsToDisable) <- splitAt toUse <$> shuffle enbldSrvs
          let dsbldSrvs' = map (\srv -> (srv :: NewUserServer p) {enabled = False}) srvsToDisable
          pure $ sortOn server' $ enbldSrvs' <> dsbldSrvs' <> dsbldSrvs
    server' UserServer {server = ProtoServerWithAuth srv _} = srv

toJustOrError :: String -> Maybe a -> IO a
toJustOrError name = \case
  Just a -> pure a
  Nothing -> do
    putStrLn $ name <> ": expected Just, exiting"
    E.throwIO $ userError name
