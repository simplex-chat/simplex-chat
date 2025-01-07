{-# LANGUAGE CPP #-}
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
import Simplex.Chat.Options
import Simplex.Chat.Protocol
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Chat.Util (shuffle)
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..), InitialAgentServers (..), ServerCfg (..), ServerRoles (..), allRoles, createAgentStore, defaultAgentConfig, presetServerCfg)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.Common (DBStore (dbNew))
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation (..), MigrationError)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Client (defaultNetworkConfig)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Protocol (ProtoServerWithAuth (..), ProtocolType (..), SProtocolType (..), SubscriptionMode (..), UserProtocol)
import qualified Simplex.Messaging.TMap as TM
import qualified UnliftIO.Exception as E
import UnliftIO.STM
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (ConnectInfo (..))
#else
import Data.ByteArray (ScrubbedBytes)
#endif

operatorSimpleXChat :: NewServerOperator
operatorSimpleXChat =
  ServerOperator
    { operatorId = DBNewEntity,
      operatorTag = Just OTSimplex,
      tradeName = "SimpleX Chat",
      legalName = Just "SimpleX Chat Ltd",
      serverDomains = ["simplex.im"],
      conditionsAcceptance = CARequired Nothing,
      enabled = True,
      smpRoles = allRoles,
      xftpRoles = allRoles
    }

operatorFlux :: NewServerOperator
operatorFlux =
  ServerOperator
    { operatorId = DBNewEntity,
      operatorTag = Just OTFlux,
      tradeName = "Flux",
      legalName = Just "InFlux Technologies Limited",
      serverDomains = ["simplexonflux.com"],
      conditionsAcceptance = CARequired Nothing,
      enabled = False,
      smpRoles = ServerRoles {storage = False, proxy = True},
      xftpRoles = ServerRoles {storage = False, proxy = True}
    }

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

simplexChatSMPServers :: [NewUserServer 'PSMP]
simplexChatSMPServers =
  map
    (presetServer True)
    [ "smp://0YuTwO05YJWS8rkjn9eLJDjQhFKvIYd8d4xG8X1blIU=@smp8.simplex.im,beccx4yfxxbvyhqypaavemqurytl6hozr47wfc7uuecacjqdvwpw2xid.onion",
      "smp://SkIkI6EPd2D63F4xFKfHk7I1UGZVNn6k1QWZ5rcyr6w=@smp9.simplex.im,jssqzccmrcws6bhmn77vgmhfjmhwlyr3u7puw4erkyoosywgl67slqqd.onion",
      "smp://6iIcWT_dF2zN_w5xzZEY7HI2Prbh3ldP07YTyDexPjE=@smp10.simplex.im,rb2pbttocvnbrngnwziclp2f4ckjq65kebafws6g4hy22cdaiv5dwjqd.onion",
      "smp://1OwYGt-yqOfe2IyVHhxz3ohqo3aCCMjtB-8wn4X_aoY=@smp11.simplex.im,6ioorbm6i3yxmuoezrhjk6f6qgkc4syabh7m3so74xunb5nzr4pwgfqd.onion",
      "smp://UkMFNAXLXeAAe0beCa4w6X_zp18PwxSaSjY17BKUGXQ=@smp12.simplex.im,ie42b5weq7zdkghocs3mgxdjeuycheeqqmksntj57rmejagmg4eor5yd.onion",
      "smp://enEkec4hlR3UtKx2NMpOUK_K4ZuDxjWBO1d9Y4YXVaA=@smp14.simplex.im,aspkyu2sopsnizbyfabtsicikr2s4r3ti35jogbcekhm3fsoeyjvgrid.onion",
      "smp://h--vW7ZSkXPeOUpfxlFGgauQmXNFOzGoizak7Ult7cw=@smp15.simplex.im,oauu4bgijybyhczbnxtlggo6hiubahmeutaqineuyy23aojpih3dajad.onion",
      "smp://hejn2gVIqNU6xjtGM3OwQeuk8ZEbDXVJXAlnSBJBWUA=@smp16.simplex.im,p3ktngodzi6qrf7w64mmde3syuzrv57y55hxabqcq3l5p6oi7yzze6qd.onion",
      "smp://ZKe4uxF4Z_aLJJOEsC-Y6hSkXgQS5-oc442JQGkyP8M=@smp17.simplex.im,ogtwfxyi3h2h5weftjjpjmxclhb5ugufa5rcyrmg7j4xlch7qsr5nuqd.onion",
      "smp://PtsqghzQKU83kYTlQ1VKg996dW4Cw4x_bvpKmiv8uns=@smp18.simplex.im,lyqpnwbs2zqfr45jqkncwpywpbtq7jrhxnib5qddtr6npjyezuwd3nqd.onion",
      "smp://N_McQS3F9TGoh4ER0QstUf55kGnNSd-wXfNPZ7HukcM=@smp19.simplex.im,i53bbtoqhlc365k6kxzwdp5w3cdt433s7bwh3y32rcbml2vztiyyz5id.onion"
    ]
    <> map
      (presetServer False)
      [ "smp://u2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU=@smp4.simplex.im,o5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion",
        "smp://hpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg=@smp5.simplex.im,jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion",
        "smp://PQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo=@smp6.simplex.im,bylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion"
      ]

fluxSMPServers :: [NewUserServer 'PSMP]
fluxSMPServers =
  map
    (presetServer True)
    [ "smp://xQW_ufMkGE20UrTlBl8QqceG1tbuylXhr9VOLPyRJmw=@smp1.simplexonflux.com,qb4yoanyl4p7o33yrknv4rs6qo7ugeb2tu2zo66sbebezs4cpyosarid.onion",
      "smp://LDnWZVlAUInmjmdpQQoIo6FUinRXGe0q3zi5okXDE4s=@smp2.simplexonflux.com,yiqtuh3q4x7hgovkomafsod52wvfjucdljqbbipg5sdssnklgongxbqd.onion",
      "smp://1jne379u7IDJSxAvXbWb_JgoE7iabcslX0LBF22Rej0=@smp3.simplexonflux.com,a5lm4k7ufei66cdck6fy63r4lmkqy3dekmmb7jkfdm5ivi6kfaojshad.onion",
      "smp://xmAmqj75I9mWrUihLUlI0ZuNLXlIwFIlHRq5Pb6cHAU=@smp4.simplexonflux.com,qpcz2axyy66u26hfdd2e23uohcf3y6c36mn7dcuilcgnwjasnrvnxjqd.onion",
      "smp://rWvBYyTamuRCBYb_KAn-nsejg879ndhiTg5Sq3k0xWA=@smp5.simplexonflux.com,4ao347qwiuluyd45xunmii4skjigzuuox53hpdsgbwxqafd4yrticead.onion",
      "smp://PN7-uqLBToqlf1NxHEaiL35lV2vBpXq8Nj8BW11bU48=@smp6.simplexonflux.com,hury6ot3ymebbr2535mlp7gcxzrjpc6oujhtfxcfh2m4fal4xw5fq6qd.onion"
    ]

fluxXFTPServers :: [NewUserServer 'PXFTP]
fluxXFTPServers =
  map
    (presetServer True)
    [ "xftp://92Sctlc09vHl_nAqF2min88zKyjdYJ9mgxRCJns5K2U=@xftp1.simplexonflux.com,apl3pumq3emwqtrztykyyoomdx4dg6ysql5zek2bi3rgznz7ai3odkid.onion",
      "xftp://YBXy4f5zU1CEhnbbCzVWTNVNsaETcAGmYqGNxHntiE8=@xftp2.simplexonflux.com,c5jjecisncnngysah3cz2mppediutfelco4asx65mi75d44njvua3xid.onion",
      "xftp://ARQO74ZSvv2OrulRF3CdgwPz_AMy27r0phtLSq5b664=@xftp3.simplexonflux.com,dc4mohiubvbnsdfqqn7xhlhpqs5u4tjzp7xpz6v6corwvzvqjtaqqiqd.onion",
      "xftp://ub2jmAa9U0uQCy90O-fSUNaYCj6sdhl49Jh3VpNXP58=@xftp4.simplexonflux.com,4qq5pzier3i4yhpuhcrhfbl6j25udc4czoyascrj4yswhodhfwev3nyd.onion",
      "xftp://Rh19D5e4Eez37DEE9hAlXDB3gZa1BdFYJTPgJWPO9OI=@xftp5.simplexonflux.com,q7itltdn32hjmgcqwhow4tay5ijetng3ur32bolssw32fvc5jrwvozad.onion",
      "xftp://0AznwoyfX8Od9T_acp1QeeKtxUi676IBIiQjXVwbdyU=@xftp6.simplexonflux.com,upvzf23ou6nrmaf3qgnhd6cn3d74tvivlmz3p7wdfwq6fhthjrjiiqid.onion"
    ]

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

#if defined(dbPostgres)
createChatDatabase :: ConnectInfo -> String -> MigrationConfirmation -> IO (Either MigrationError ChatDatabase)
createChatDatabase connectInfo schemaPrefix confirmMigrations = runExceptT $ do
  chatStore <- ExceptT $ createChatStore connectInfo (chatSchema schemaPrefix) confirmMigrations
  agentStore <- ExceptT $ createAgentStore connectInfo (agentSchema schemaPrefix) confirmMigrations
  pure ChatDatabase {chatStore, agentStore}
#else
createChatDatabase :: FilePath -> ScrubbedBytes -> Bool -> MigrationConfirmation -> Bool -> IO (Either MigrationError ChatDatabase)
createChatDatabase filePrefix key keepKey confirmMigrations vacuum = runExceptT $ do
  chatStore <- ExceptT $ createChatStore (chatStoreFile filePrefix) key keepKey confirmMigrations vacuum
  agentStore <- ExceptT $ createAgentStore (agentStoreFile filePrefix) key keepKey confirmMigrations vacuum
  pure ChatDatabase {chatStore, agentStore}
#endif

newChatController :: ChatDatabase -> Maybe User -> ChatConfig -> ChatOpts -> Bool -> IO ChatController
newChatController
  ChatDatabase {chatStore, agentStore}
  user
  cfg@ChatConfig {agentConfig = aCfg, presetServers, inlineFiles, deviceNameForRemote, confirmMigrations}
  ChatOpts {coreOptions = CoreChatOpts {smpServers, xftpServers, simpleNetCfg, logLevel, logConnections, logServerHosts, logFile, tbqSize, highlyAvailable, yesToUpMigrations}, deviceName, optFilesFolder, optTempDirectory, showReactions, allowInstantFiles, autoAcceptFileSize}
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
      agentServers db ChatConfig {presetServers = PresetServers {ntf, netCfg}} presetOps as = do
        users <- getUsers db
        ops <- getUpdateServerOperators db presetOps (null users)
        let opDomains = operatorDomains $ mapMaybe snd ops
        (smp', xftp') <- unzip <$> mapM (getServers ops opDomains) users
        pure InitialAgentServers {smp = M.fromList (optServers smp' smpServers), xftp = M.fromList (optServers xftp' xftpServers), ntf, netCfg}
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
