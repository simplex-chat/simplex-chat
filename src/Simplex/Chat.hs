{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat where

import Control.Applicative (optional, (<|>))
import Control.Concurrent.STM (retry)
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (bimap, first, second)
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char
import Data.Constraint (Dict (..))
import Data.Either (fromRight, lefts, partitionEithers, rights)
import Data.Fixed (div')
import Data.Foldable (foldr')
import Data.Functor (($>))
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (find, foldl', isSuffixOf, mapAccumL, partition, sortOn, zipWith4)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDay, nominalDiffTimeToSeconds)
import Data.Type.Equality
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4
import Data.Word (Word32)
import qualified Database.SQLite.Simple as SQL
import Simplex.Chat.Archive
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Files
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.Batch (MsgBatch (..), batchMessages)
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.Operators
import Simplex.Chat.Options
import Simplex.Chat.ProfileGenerator (generateRandomProfile)
import Simplex.Chat.Protocol
import Simplex.Chat.Remote
import Simplex.Chat.Remote.Types
import Simplex.Chat.Stats
import Simplex.Chat.Store
import Simplex.Chat.Store.AppSettings
import Simplex.Chat.Store.Connections
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Files
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Messages
import Simplex.Chat.Store.NoteFolders
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Util (encryptFile, liftIOEither, shuffle)
import qualified Simplex.Chat.Util as U
import Simplex.FileTransfer.Description (maxFileSize, maxFileSizeHard)
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.FileTransfer.Description (FileDescriptionURI (..), ValidFileDescription)
import qualified Simplex.FileTransfer.Description as FD
import Simplex.FileTransfer.Protocol (FileParty (..), FilePartyI)
import qualified Simplex.FileTransfer.Transport as XFTP
import Simplex.FileTransfer.Types (FileErrorType (..), RcvFileId, SndFileId)
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Client (SubInfo (..), agentClientStore, getAgentQueuesInfo, getAgentWorkersDetails, getAgentWorkersSummary, getFastNetworkConfig, ipAddressProtected, withLockMap)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..), InitialAgentServers (..), ServerCfg (..), ServerRoles (..), allRoles, createAgentStore, defaultAgentConfig, presetServerCfg)
import Simplex.Messaging.Agent.Lock (withLock)
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Agent.Protocol as AP (AgentErrorType (..))
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation (..), MigrationError, SQLiteStore (dbNew), execSQL, upMigration, withConnection)
import Simplex.Messaging.Agent.Store.SQLite.DB (SlowQueryStats (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
import Simplex.Messaging.Client (NetworkConfig (..), ProxyClientError (..), SocksMode (SMAlways), defaultNetworkConfig, textToHostMode)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..), pattern IKPQOff, pattern IKPQOn, pattern PQEncOff, pattern PQEncOn, pattern PQSupportOff, pattern PQSupportOn)
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (base64P)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), AProtocolType (..), ErrorType (..), MsgBody, MsgFlags (..), NtfServer, ProtoServerWithAuth (..), ProtocolServer, ProtocolType (..), ProtocolTypeI (..), SProtocolType (..), SubscriptionMode (..), UserProtocol, XFTPServer, userProtocol)
import qualified Simplex.Messaging.Protocol as SMP
import Simplex.Messaging.ServiceScheme (ServiceScheme (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (TransportError (..))
import Simplex.Messaging.Transport.Client (defaultSocksProxyWithAuth)
import Simplex.Messaging.Util
import Simplex.Messaging.Version
import Simplex.RemoteControl.Invitation (RCInvitation (..), RCSignedInvitation (..))
import Simplex.RemoteControl.Types (RCCtrlAddress (..))
import System.Exit (ExitCode, exitSuccess)
import System.FilePath (takeExtension, takeFileName, (</>))
import qualified System.FilePath as FP
import System.IO (Handle, IOMode (..), SeekMode (..), hFlush)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Concurrent (forkFinally, forkIO, mkWeakThreadId, threadDelay)
import UnliftIO.Directory
import qualified UnliftIO.Exception as E
import UnliftIO.IO (hClose, hSeek, hTell, openFile)
import UnliftIO.STM

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

_defaultNtfServers :: [NtfServer]
_defaultNtfServers =
  [ "ntf://FB-Uop7RTaZZEG0ZLD2CIaTjsPh-Fw0zFAnb7QyA8Ks=@ntf2.simplex.im,5ex3mupcazy3zlky64ab27phjhijpemsiby33qzq3pliejipbtx5xgad.onion"
  -- "ntf://KmpZNNXiVZJx_G2T7jRUmDFxWXM3OAnunz3uLT0tqAA=@ntf3.simplex.im,pxculznuryunjdvtvh6s6szmanyadumpbmvevgdpe4wk5c65unyt4yid.onion",
  -- "ntf://CJ5o7X6fCxj2FFYRU2KuCo70y4jSqz7td2HYhLnXWbU=@ntf4.simplex.im,wtvuhdj26jwprmomnyfu5wfuq2hjkzfcc72u44vi6gdhrwxldt6xauad.onion"
  ]

maxImageSize :: Integer
maxImageSize = 261120 * 2 -- auto-receive on mobiles

imageExtensions :: [String]
imageExtensions = [".jpg", ".jpeg", ".png", ".gif"]

maxMsgReactions :: Int
maxMsgReactions = 3

fixedImagePreview :: ImageData
fixedImagePreview = ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAKVJREFUeF7t1kENACEUQ0FQhnVQ9lfGO+xggITQdvbMzArPey+8fa3tAfwAEdABZQspQStgBssEcgAIkSAJkiAJljtEgiRIgmUCSZAESZAESZAEyx0iQRIkwTKBJEiCv5fgvTd1wDmn7QAP4AeIgA4oW0gJWgEzWCZwbQ7gAA7ggLKFOIADOKBMIAeAEAmSIAmSYLlDJEiCJFgmkARJkARJ8N8S/ADTZUewBvnTOQAAAABJRU5ErkJggg=="

smallGroupsRcptsMemLimit :: Int
smallGroupsRcptsMemLimit = 20

imageFilePrefix :: String
imageFilePrefix = "IMG_"

voiceFilePrefix :: String
voiceFilePrefix = "voice_"

videoFilePrefix :: String
videoFilePrefix = "video_"

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

createChatDatabase :: FilePath -> ScrubbedBytes -> Bool -> MigrationConfirmation -> IO (Either MigrationError ChatDatabase)
createChatDatabase filePrefix key keepKey confirmMigrations = runExceptT $ do
  chatStore <- ExceptT $ createChatStore (chatStoreFile filePrefix) key keepKey confirmMigrations
  agentStore <- ExceptT $ createAgentStore (agentStoreFile filePrefix) key keepKey confirmMigrations
  pure ChatDatabase {chatStore, agentStore}

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

updateNetworkConfig :: NetworkConfig -> SimpleNetCfg -> NetworkConfig
updateNetworkConfig cfg SimpleNetCfg {socksProxy, socksMode, hostMode, requiredHostMode, smpProxyMode_, smpProxyFallback_, smpWebPort, tcpTimeout_, logTLSErrors} =
  let cfg1 = maybe cfg (\smpProxyMode -> cfg {smpProxyMode}) smpProxyMode_
      cfg2 = maybe cfg1 (\smpProxyFallback -> cfg1 {smpProxyFallback}) smpProxyFallback_
      cfg3 = maybe cfg2 (\tcpTimeout -> cfg2 {tcpTimeout, tcpConnectTimeout = (tcpTimeout * 3) `div` 2}) tcpTimeout_
   in cfg3 {socksProxy, socksMode, hostMode, requiredHostMode, smpWebPort, logTLSErrors}

withChatLock :: String -> CM a -> CM a
withChatLock name action = asks chatLock >>= \l -> withLock l name action

withEntityLock :: String -> ChatLockEntity -> CM a -> CM a
withEntityLock name entity action = do
  chatLock <- asks chatLock
  ls <- asks entityLocks
  atomically $ unlessM (isEmptyTMVar chatLock) retry
  withLockMap ls entity name action

withInvitationLock :: String -> ByteString -> CM a -> CM a
withInvitationLock name = withEntityLock name . CLInvitation
{-# INLINE withInvitationLock #-}

withConnectionLock :: String -> Int64 -> CM a -> CM a
withConnectionLock name = withEntityLock name . CLConnection
{-# INLINE withConnectionLock #-}

withContactLock :: String -> ContactId -> CM a -> CM a
withContactLock name = withEntityLock name . CLContact
{-# INLINE withContactLock #-}

withGroupLock :: String -> GroupId -> CM a -> CM a
withGroupLock name = withEntityLock name . CLGroup
{-# INLINE withGroupLock #-}

withUserContactLock :: String -> Int64 -> CM a -> CM a
withUserContactLock name = withEntityLock name . CLUserContact
{-# INLINE withUserContactLock #-}

withFileLock :: String -> Int64 -> CM a -> CM a
withFileLock name = withEntityLock name . CLFile
{-# INLINE withFileLock #-}

useServers :: Foldable f => RandomAgentServers -> [(Text, ServerOperator)] -> f UserOperatorServers -> (NonEmpty (ServerCfg 'PSMP), NonEmpty (ServerCfg 'PXFTP))
useServers as opDomains uss =
  let smp' = useServerCfgs SPSMP as opDomains $ concatMap (servers' SPSMP) uss
      xftp' = useServerCfgs SPXFTP as opDomains $ concatMap (servers' SPXFTP) uss
   in (smp', xftp')

useServerCfgs :: forall p. UserProtocol p => SProtocolType p -> RandomAgentServers -> [(Text, ServerOperator)] -> [UserServer p] -> NonEmpty (ServerCfg p)
useServerCfgs p RandomAgentServers {smpServers, xftpServers} opDomains =
  fromMaybe (rndAgentServers p) . L.nonEmpty . agentServerCfgs p opDomains
  where
    rndAgentServers :: SProtocolType p -> NonEmpty (ServerCfg p)
    rndAgentServers = \case
      SPSMP -> smpServers
      SPXFTP -> xftpServers

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

-- enableSndFiles has no effect when mainApp is True
startChatController :: Bool -> Bool -> CM' (Async ())
startChatController mainApp enableSndFiles = do
  asks smpAgent >>= liftIO . resumeAgentClient
  unless mainApp $ chatWriteVar' subscriptionMode SMOnlyCreate
  users <- fromRight [] <$> runExceptT (withFastStore' getUsers)
  restoreCalls
  s <- asks agentAsync
  readTVarIO s >>= maybe (start s users) (pure . fst)
  where
    start s users = do
      a1 <- async agentSubscriber
      a2 <-
        if mainApp
          then Just <$> async (subscribeUsers False users)
          else pure Nothing
      atomically . writeTVar s $ Just (a1, a2)
      if mainApp
        then do
          startXFTP xftpStartWorkers
          void $ forkIO $ startFilesToReceive users
          startCleanupManager
          void $ forkIO $ startExpireCIs users
        else when enableSndFiles $ startXFTP xftpStartSndWorkers
      pure a1
    startXFTP startWorkers = do
      tmp <- readTVarIO =<< asks tempDirectory
      runExceptT (withAgent $ \a -> startWorkers a tmp) >>= \case
        Left e -> liftIO $ putStrLn $ "Error starting XFTP workers: " <> show e
        Right _ -> pure ()
    startCleanupManager = do
      cleanupAsync <- asks cleanupManagerAsync
      readTVarIO cleanupAsync >>= \case
        Nothing -> do
          a <- Just <$> async (void $ runExceptT cleanupManager)
          atomically $ writeTVar cleanupAsync a
        _ -> pure ()
    startExpireCIs users =
      forM_ users $ \user -> do
        ttl <- fromRight Nothing <$> runExceptT (withStore' (`getChatItemTTL` user))
        forM_ ttl $ \_ -> do
          startExpireCIThread user
          setExpireCIFlag user True

subscribeUsers :: Bool -> [User] -> CM' ()
subscribeUsers onlyNeeded users = do
  let (us, us') = partition activeUser users
  vr <- chatVersionRange'
  subscribe vr us
  subscribe vr us'
  where
    subscribe :: VersionRangeChat -> [User] -> CM' ()
    subscribe vr = mapM_ $ runExceptT . subscribeUserConnections vr onlyNeeded Agent.subscribeConnections

startFilesToReceive :: [User] -> CM' ()
startFilesToReceive users = do
  let (us, us') = partition activeUser users
  startReceive us
  startReceive us'
  where
    startReceive :: [User] -> CM' ()
    startReceive = mapM_ $ runExceptT . startReceiveUserFiles

startReceiveUserFiles :: User -> CM ()
startReceiveUserFiles user = do
  filesToReceive <- withStore' (`getRcvFilesToReceive` user)
  forM_ filesToReceive $ \ft ->
    flip catchChatError (toView . CRChatError (Just user)) $
      toView =<< receiveFile' user ft False Nothing Nothing

restoreCalls :: CM' ()
restoreCalls = do
  savedCalls <- fromRight [] <$> runExceptT (withFastStore' getCalls)
  let callsMap = M.fromList $ map (\call@Call {contactId} -> (contactId, call)) savedCalls
  calls <- asks currentCalls
  atomically $ writeTVar calls callsMap

stopChatController :: ChatController -> IO ()
stopChatController ChatController {smpAgent, agentAsync = s, sndFiles, rcvFiles, expireCIFlags, remoteHostSessions, remoteCtrlSession} = do
  readTVarIO remoteHostSessions >>= mapM_ (cancelRemoteHost False . snd)
  atomically (stateTVar remoteCtrlSession (,Nothing)) >>= mapM_ (cancelRemoteCtrl False . snd)
  disconnectAgentClient smpAgent
  readTVarIO s >>= mapM_ (\(a1, a2) -> uninterruptibleCancel a1 >> mapM_ uninterruptibleCancel a2)
  closeFiles sndFiles
  closeFiles rcvFiles
  atomically $ do
    keys <- M.keys <$> readTVar expireCIFlags
    forM_ keys $ \k -> TM.insert k False expireCIFlags
    writeTVar s Nothing
  where
    closeFiles :: TVar (Map Int64 Handle) -> IO ()
    closeFiles files = do
      fs <- readTVarIO files
      mapM_ hClose fs
      atomically $ writeTVar files M.empty

execChatCommand :: Maybe RemoteHostId -> ByteString -> CM' ChatResponse
execChatCommand rh s = do
  u <- readTVarIO =<< asks currentUser
  case parseChatCommand s of
    Left e -> pure $ chatCmdError u e
    Right cmd -> case rh of
      Just rhId
        | allowRemoteCommand cmd -> execRemoteCommand u rhId cmd s
        | otherwise -> pure $ CRChatCmdError u $ ChatErrorRemoteHost (RHId rhId) $ RHELocalCommand
      _ -> do
        cc@ChatController {config = ChatConfig {chatHooks}} <- ask
        liftIO (preCmdHook chatHooks cc cmd) >>= either pure (execChatCommand_ u)

execChatCommand' :: ChatCommand -> CM' ChatResponse
execChatCommand' cmd = asks currentUser >>= readTVarIO >>= (`execChatCommand_` cmd)

execChatCommand_ :: Maybe User -> ChatCommand -> CM' ChatResponse
execChatCommand_ u cmd = handleCommandError u $ processChatCommand cmd

execRemoteCommand :: Maybe User -> RemoteHostId -> ChatCommand -> ByteString -> CM' ChatResponse
execRemoteCommand u rhId cmd s = handleCommandError u $ getRemoteHostClient rhId >>= \rh -> processRemoteCommand rhId rh cmd s

handleCommandError :: Maybe User -> CM ChatResponse -> CM' ChatResponse
handleCommandError u a = either (CRChatCmdError u) id <$> (runExceptT a `E.catches` ioErrors)
  where
    ioErrors =
      [ E.Handler $ \(e :: ExitCode) -> E.throwIO e,
        E.Handler $ pure . Left . mkChatError
      ]

parseChatCommand :: ByteString -> Either String ChatCommand
parseChatCommand = A.parseOnly chatCommandP . B.dropWhileEnd isSpace

-- | Chat API commands interpreted in context of a local zone
processChatCommand :: ChatCommand -> CM ChatResponse
processChatCommand cmd =
  chatVersionRange >>= (`processChatCommand'` cmd)
{-# INLINE processChatCommand #-}

processChatCommand' :: VersionRangeChat -> ChatCommand -> CM ChatResponse
processChatCommand' vr = \case
  ShowActiveUser -> withUser' $ pure . CRActiveUser
  CreateActiveUser NewUser {profile, pastTimestamp} -> do
    forM_ profile $ \Profile {displayName} -> checkValidName displayName
    p@Profile {displayName} <- liftIO $ maybe generateRandomProfile pure profile
    u <- asks currentUser
    users <- withFastStore' getUsers
    forM_ users $ \User {localDisplayName = n, activeUser, viewPwdHash} ->
      when (n == displayName) . throwChatError $
        if activeUser || isNothing viewPwdHash then CEUserExists displayName else CEInvalidDisplayName {displayName, validName = ""}
    (uss, (smp', xftp')) <- chooseServers =<< readTVarIO u
    auId <- withAgent $ \a -> createUser a smp' xftp'
    ts <- liftIO $ getCurrentTime >>= if pastTimestamp then coupleDaysAgo else pure
    user <- withFastStore $ \db -> do
      user <- createUserRecordAt db (AgentUserId auId) p True ts
      mapM_ (setUserServers db user ts) uss
      createPresetContactCards db user `catchStoreError` \_ -> pure ()
      createNoteFolder db user
      pure user
    atomically . writeTVar u $ Just user
    pure $ CRActiveUser user
    where
      createPresetContactCards :: DB.Connection -> User -> ExceptT StoreError IO ()
      createPresetContactCards db user = do
        createContact db user simplexStatusContactProfile
        createContact db user simplexTeamContactProfile
      chooseServers :: Maybe User -> CM ([UpdatedUserOperatorServers], (NonEmpty (ServerCfg 'PSMP), NonEmpty (ServerCfg 'PXFTP)))
      chooseServers user_ = do
        as <- asks randomAgentServers
        mapM (withFastStore . flip getUserServers >=> liftIO . groupByOperator) user_ >>= \case
          Just uss -> do
            let opDomains = operatorDomains $ mapMaybe operator' uss
                uss' = map copyServers uss
            pure $ (uss',) $ useServers as opDomains uss
          Nothing -> do
            ps <- asks randomPresetServers
            uss <- presetUserServers <$> withFastStore' (\db -> getUpdateServerOperators db ps True)
            let RandomAgentServers {smpServers = smp', xftpServers = xftp'} = as
            pure (uss, (smp', xftp'))
      copyServers :: UserOperatorServers -> UpdatedUserOperatorServers
      copyServers UserOperatorServers {operator, smpServers, xftpServers} =
        let new srv = AUS SDBNew srv {serverId = DBNewEntity}
         in UpdatedUserOperatorServers {operator, smpServers = map new smpServers, xftpServers = map new xftpServers}
      coupleDaysAgo t = (`addUTCTime` t) . fromInteger . negate . (+ (2 * day)) <$> randomRIO (0, day)
      day = 86400
  ListUsers -> CRUsersList <$> withFastStore' getUsersInfo
  APISetActiveUser userId' viewPwd_ -> do
    unlessM (lift chatStarted) $ throwChatError CEChatNotStarted
    user_ <- chatReadVar currentUser
    user' <- privateGetUser userId'
    validateUserPassword_ user_ user' viewPwd_
    user'' <- withFastStore' (`setActiveUser` user')
    chatWriteVar currentUser $ Just user''
    pure $ CRActiveUser user''
  SetActiveUser uName viewPwd_ -> do
    tryChatError (withFastStore (`getUserIdByName` uName)) >>= \case
      Left _ -> throwChatError CEUserUnknown
      Right userId -> processChatCommand $ APISetActiveUser userId viewPwd_
  SetAllContactReceipts onOff -> withUser $ \_ -> withFastStore' (`updateAllContactReceipts` onOff) >> ok_
  APISetUserContactReceipts userId' settings -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' Nothing
    withFastStore' $ \db -> updateUserContactReceipts db user' settings
    ok user
  SetUserContactReceipts settings -> withUser $ \User {userId} -> processChatCommand $ APISetUserContactReceipts userId settings
  APISetUserGroupReceipts userId' settings -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' Nothing
    withFastStore' $ \db -> updateUserGroupReceipts db user' settings
    ok user
  SetUserGroupReceipts settings -> withUser $ \User {userId} -> processChatCommand $ APISetUserGroupReceipts userId settings
  APIHideUser userId' (UserPwd viewPwd) -> withUser $ \user -> do
    user' <- privateGetUser userId'
    case viewPwdHash user' of
      Just _ -> throwChatError $ CEUserAlreadyHidden userId'
      _ -> do
        when (T.null viewPwd) $ throwChatError $ CEEmptyUserPassword userId'
        users <- withFastStore' getUsers
        unless (length (filter (isNothing . viewPwdHash) users) > 1) $ throwChatError $ CECantHideLastUser userId'
        viewPwdHash' <- hashPassword
        setUserPrivacy user user' {viewPwdHash = viewPwdHash', showNtfs = False}
        where
          hashPassword = do
            salt <- drgRandomBytes 16
            let hash = B64UrlByteString $ C.sha512Hash $ encodeUtf8 viewPwd <> salt
            pure $ Just UserPwdHash {hash, salt = B64UrlByteString salt}
  APIUnhideUser userId' viewPwd@(UserPwd pwd) -> withUser $ \user -> do
    user' <- privateGetUser userId'
    case viewPwdHash user' of
      Nothing -> throwChatError $ CEUserNotHidden userId'
      _ -> do
        when (T.null pwd) $ throwChatError $ CEEmptyUserPassword userId'
        validateUserPassword user user' $ Just viewPwd
        setUserPrivacy user user' {viewPwdHash = Nothing, showNtfs = True}
  APIMuteUser userId' -> setUserNotifications userId' False
  APIUnmuteUser userId' -> setUserNotifications userId' True
  HideUser viewPwd -> withUser $ \User {userId} -> processChatCommand $ APIHideUser userId viewPwd
  UnhideUser viewPwd -> withUser $ \User {userId} -> processChatCommand $ APIUnhideUser userId viewPwd
  MuteUser -> withUser $ \User {userId} -> processChatCommand $ APIMuteUser userId
  UnmuteUser -> withUser $ \User {userId} -> processChatCommand $ APIUnmuteUser userId
  APIDeleteUser userId' delSMPQueues viewPwd_ -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' viewPwd_
    checkDeleteChatUser user'
    withChatLock "deleteUser" . procCmd $ deleteChatUser user' delSMPQueues
  DeleteUser uName delSMPQueues viewPwd_ -> withUserName uName $ \userId -> APIDeleteUser userId delSMPQueues viewPwd_
  StartChat {mainApp, enableSndFiles} -> withUser' $ \_ ->
    asks agentAsync >>= readTVarIO >>= \case
      Just _ -> pure CRChatRunning
      _ -> checkStoreNotChanged . lift $ startChatController mainApp enableSndFiles $> CRChatStarted
  CheckChatRunning -> maybe CRChatStopped (const CRChatRunning) <$> chatReadVar agentAsync
  APIStopChat -> do
    ask >>= liftIO . stopChatController
    pure CRChatStopped
  APIActivateChat restoreChat -> withUser $ \_ -> do
    lift $ when restoreChat restoreCalls
    lift $ withAgent' foregroundAgent
    chatWriteVar chatActivated True
    when restoreChat $ do
      users <- withFastStore' getUsers
      lift $ do
        void . forkIO $ subscribeUsers True users
        void . forkIO $ startFilesToReceive users
        setAllExpireCIFlags True
    ok_
  APISuspendChat t -> do
    chatWriteVar chatActivated False
    lift $ setAllExpireCIFlags False
    stopRemoteCtrl
    lift $ withAgent' (`suspendAgent` t)
    ok_
  ResubscribeAllConnections -> withStore' getUsers >>= lift . subscribeUsers False >> ok_
  -- has to be called before StartChat
  SetTempFolder tf -> do
    createDirectoryIfMissing True tf
    asks tempDirectory >>= atomically . (`writeTVar` Just tf)
    ok_
  SetFilesFolder ff -> do
    createDirectoryIfMissing True ff
    asks filesFolder >>= atomically . (`writeTVar` Just ff)
    ok_
  SetRemoteHostsFolder rf -> do
    createDirectoryIfMissing True rf
    chatWriteVar remoteHostsFolder $ Just rf
    ok_
  -- has to be called before StartChat
  APISetAppFilePaths cfg -> do
    setFolder filesFolder $ appFilesFolder cfg
    setFolder tempDirectory $ appTempFolder cfg
    setFolder assetsDirectory $ appAssetsFolder cfg
    mapM_ (setFolder remoteHostsFolder) $ appRemoteHostsFolder cfg
    ok_
    where
      setFolder sel f = do
        createDirectoryIfMissing True f
        chatWriteVar sel $ Just f
  APISetEncryptLocalFiles on -> chatWriteVar encryptLocalFiles on >> ok_
  SetContactMergeEnabled onOff -> chatWriteVar contactMergeEnabled onOff >> ok_
  APIExportArchive cfg -> checkChatStopped $ CRArchiveExported <$> lift (exportArchive cfg)
  ExportArchive -> do
    ts <- liftIO getCurrentTime
    let filePath = "simplex-chat." <> formatTime defaultTimeLocale "%FT%H%M%SZ" ts <> ".zip"
    processChatCommand $ APIExportArchive $ ArchiveConfig filePath Nothing Nothing
  APIImportArchive cfg -> checkChatStopped $ do
    fileErrs <- lift $ importArchive cfg
    setStoreChanged
    pure $ CRArchiveImported fileErrs
  APISaveAppSettings as -> withFastStore' (`saveAppSettings` as) >> ok_
  APIGetAppSettings platformDefaults -> CRAppSettings <$> withFastStore' (`getAppSettings` platformDefaults)
  APIDeleteStorage -> withStoreChanged deleteStorage
  APIStorageEncryption cfg -> withStoreChanged $ sqlCipherExport cfg
  TestStorageEncryption key -> sqlCipherTestKey key >> ok_
  ExecChatStoreSQL query -> CRSQLResult <$> withStore' (`execSQL` query)
  ExecAgentStoreSQL query -> CRSQLResult <$> withAgent (`execAgentStoreSQL` query)
  SlowSQLQueries -> do
    ChatController {chatStore, smpAgent} <- ask
    chatQueries <- slowQueries chatStore
    agentQueries <- slowQueries $ agentClientStore smpAgent
    pure CRSlowSQLQueries {chatQueries, agentQueries}
    where
      slowQueries st =
        liftIO $
          map (uncurry SlowSQLQuery . first SQL.fromQuery)
            . sortOn (timeAvg . snd)
            . M.assocs
            <$> withConnection st (readTVarIO . DB.slow)
  APIGetChats {userId, pendingConnections, pagination, query} -> withUserId' userId $ \user -> do
    (errs, previews) <- partitionEithers <$> withFastStore' (\db -> getChatPreviews db vr user pendingConnections pagination query)
    unless (null errs) $ toView $ CRChatErrors (Just user) (map ChatErrorStore errs)
    pure $ CRApiChats user previews
  APIGetChat (ChatRef cType cId) pagination search -> withUser $ \user -> case cType of
    -- TODO optimize queries calculating ChatStats, currently they're disabled
    CTDirect -> do
      (directChat, navInfo) <- withFastStore (\db -> getDirectChat db vr user cId pagination search)
      pure $ CRApiChat user (AChat SCTDirect directChat) navInfo
    CTGroup -> do
      (groupChat, navInfo) <- withFastStore (\db -> getGroupChat db vr user cId pagination search)
      pure $ CRApiChat user (AChat SCTGroup groupChat) navInfo
    CTLocal -> do
      (localChat, navInfo) <- withFastStore (\db -> getLocalChat db user cId pagination search)
      pure $ CRApiChat user (AChat SCTLocal localChat) navInfo
    CTContactRequest -> pure $ chatCmdError (Just user) "not implemented"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APIGetChatItems pagination search -> withUser $ \user -> do
    chatItems <- withFastStore $ \db -> getAllChatItems db vr user pagination search
    pure $ CRChatItems user Nothing chatItems
  APIGetChatItemInfo chatRef itemId -> withUser $ \user -> do
    (aci@(AChatItem cType dir _ ci), versions) <- withFastStore $ \db ->
      (,) <$> getAChatItem db vr user chatRef itemId <*> liftIO (getChatItemVersions db itemId)
    let itemVersions = if null versions then maybeToList $ mkItemVersion ci else versions
    memberDeliveryStatuses <- case (cType, dir) of
      (SCTGroup, SMDSnd) -> L.nonEmpty <$> withFastStore' (`getGroupSndStatuses` itemId)
      _ -> pure Nothing
    forwardedFromChatItem <- getForwardedFromItem user ci
    pure $ CRChatItemInfo user aci ChatItemInfo {itemVersions, memberDeliveryStatuses, forwardedFromChatItem}
    where
      getForwardedFromItem :: User -> ChatItem c d -> CM (Maybe AChatItem)
      getForwardedFromItem user ChatItem {meta = CIMeta {itemForwarded}} = case itemForwarded of
        Just (CIFFContact _ _ (Just ctId) (Just fwdItemId)) ->
          Just <$> withFastStore (\db -> getAChatItem db vr user (ChatRef CTDirect ctId) fwdItemId)
        Just (CIFFGroup _ _ (Just gId) (Just fwdItemId)) ->
          Just <$> withFastStore (\db -> getAChatItem db vr user (ChatRef CTGroup gId) fwdItemId)
        _ -> pure Nothing
  APISendMessages (ChatRef cType chatId) live itemTTL cms -> withUser $ \user -> mapM_ assertAllowedContent' cms >> case cType of
    CTDirect ->
      withContactLock "sendMessage" chatId $
        sendContactContentMessages user chatId live itemTTL (L.map (,Nothing) cms)
    CTGroup ->
      withGroupLock "sendMessage" chatId $
        sendGroupContentMessages user chatId live itemTTL (L.map (,Nothing) cms)
    CTLocal -> pure $ chatCmdError (Just user) "not supported"
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APICreateChatItems folderId cms -> withUser $ \user -> do
    mapM_ assertAllowedContent' cms
    createNoteFolderContentItems user folderId (L.map (,Nothing) cms)
  APIReportMessage gId reportedItemId reportReason reportText -> withUser $ \user ->
    withGroupLock "reportMessage" gId $ do
      (gInfo, ms) <-
        withFastStore $ \db -> do
          gInfo <- getGroupInfo db vr user gId
          (gInfo,) <$> liftIO (getGroupModerators db vr user gInfo)
      let ms' = filter compatibleModerator ms
          mc = MCReport reportText reportReason
          cm = ComposedMessage {fileSource = Nothing, quotedItemId = Just reportedItemId, msgContent = mc}
      when (null ms') $ throwChatError $ CECommandError "no moderators support receiving reports"
      sendGroupContentMessages_ user gInfo ms' False Nothing [(cm, Nothing)]
    where
      compatibleModerator GroupMember {activeConn, memberChatVRange} =
        maxVersion (maybe memberChatVRange peerChatVRange activeConn) >= contentReportsVersion
  ReportMessage {groupName, contactName_, reportReason, reportedMessage} -> withUser $ \user -> do
    gId <- withFastStore $ \db -> getGroupIdByName db user groupName
    reportedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user gId contactName_ reportedMessage
    processChatCommand $ APIReportMessage gId reportedItemId reportReason ""
  APIUpdateChatItem (ChatRef cType chatId) itemId live mc -> withUser $ \user -> assertAllowedContent mc >> case cType of
    CTDirect -> withContactLock "updateChatItem" chatId $ do
      ct@Contact {contactId} <- withFastStore $ \db -> getContact db vr user chatId
      assertDirectAllowed user MDSnd ct XMsgUpdate_
      cci <- withFastStore $ \db -> getDirectCIWithReactions db user ct itemId
      case cci of
        CChatItem SMDSnd ci@ChatItem {meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable}, content = ciContent} -> do
          case (ciContent, itemSharedMsgId, editable) of
            (CISndMsgContent oldMC, Just itemSharedMId, True) -> do
              let changed = mc /= oldMC
              if changed || fromMaybe False itemLive
                then do
                  (SndMessage {msgId}, _) <- sendDirectContactMessage user ct (XMsgUpdate itemSharedMId mc (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive))
                  ci' <- withFastStore' $ \db -> do
                    currentTs <- liftIO getCurrentTime
                    when changed $
                      addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
                    let edited = itemLive /= Just True
                    updateDirectChatItem' db user contactId ci (CISndMsgContent mc) edited live Nothing $ Just msgId
                  startUpdatedTimedItemThread user (ChatRef CTDirect contactId) ci ci'
                  pure $ CRChatItemUpdated user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci')
                else pure $ CRChatItemNotChanged user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTGroup -> withGroupLock "updateChatItem" chatId $ do
      Group gInfo@GroupInfo {groupId, membership} ms <- withFastStore $ \db -> getGroup db vr user chatId
      assertUserGroupRole gInfo GRAuthor
      if prohibitedSimplexLinks gInfo membership mc
        then pure $ chatCmdError (Just user) ("feature not allowed " <> T.unpack (groupFeatureNameText GFSimplexLinks))
        else do
          cci <- withFastStore $ \db -> getGroupCIWithReactions db user gInfo itemId
          case cci of
            CChatItem SMDSnd ci@ChatItem {meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable}, content = ciContent} -> do
              case (ciContent, itemSharedMsgId, editable) of
                (CISndMsgContent oldMC, Just itemSharedMId, True) -> do
                  let changed = mc /= oldMC
                  if changed || fromMaybe False itemLive
                    then do
                      SndMessage {msgId} <- sendGroupMessage user gInfo ms (XMsgUpdate itemSharedMId mc (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive))
                      ci' <- withFastStore' $ \db -> do
                        currentTs <- liftIO getCurrentTime
                        when changed $
                          addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
                        let edited = itemLive /= Just True
                        updateGroupChatItem db user groupId ci (CISndMsgContent mc) edited live $ Just msgId
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId) ci ci'
                      pure $ CRChatItemUpdated user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci')
                    else pure $ CRChatItemNotChanged user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
                _ -> throwChatError CEInvalidChatItemUpdate
            CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTLocal -> do
      (nf@NoteFolder {noteFolderId}, cci) <- withFastStore $ \db -> (,) <$> getNoteFolder db user chatId <*> getLocalChatItem db user chatId itemId
      case cci of
        CChatItem SMDSnd ci@ChatItem {content = CISndMsgContent oldMC}
          | mc == oldMC -> pure $ CRChatItemNotChanged user (AChatItem SCTLocal SMDSnd (LocalChat nf) ci)
          | otherwise -> withFastStore' $ \db -> do
              currentTs <- getCurrentTime
              addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
              ci' <- updateLocalChatItem' db user noteFolderId ci (CISndMsgContent mc) True
              pure $ CRChatItemUpdated user (AChatItem SCTLocal SMDSnd (LocalChat nf) ci')
        _ -> throwChatError CEInvalidChatItemUpdate
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APIDeleteChatItem (ChatRef cType chatId) itemIds mode -> withUser $ \user -> case cType of
    CTDirect -> withContactLock "deleteChatItem" chatId $ do
      (ct, items) <- getCommandDirectChatItems user chatId itemIds
      case mode of
        CIDMInternal -> deleteDirectCIs user ct items True False
        CIDMInternalMark -> markDirectCIsDeleted user ct items True =<< liftIO getCurrentTime
        CIDMBroadcast -> do
          assertDeletable items
          assertDirectAllowed user MDSnd ct XMsgDel_
          let msgIds = itemsMsgIds items
              events = map (\msgId -> XMsgDel msgId Nothing) msgIds
          forM_ (L.nonEmpty events) $ \events' ->
            sendDirectContactMessages user ct events'
          if featureAllowed SCFFullDelete forUser ct
            then deleteDirectCIs user ct items True False
            else markDirectCIsDeleted user ct items True =<< liftIO getCurrentTime
    CTGroup -> withGroupLock "deleteChatItem" chatId $ do
      (gInfo, items) <- getCommandGroupChatItems user chatId itemIds
      ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
      case mode of
        CIDMInternal -> deleteGroupCIs user gInfo items True False Nothing =<< liftIO getCurrentTime
        CIDMInternalMark -> markGroupCIsDeleted user gInfo items True Nothing =<< liftIO getCurrentTime
        CIDMBroadcast -> do
          assertDeletable items
          assertUserGroupRole gInfo GRObserver -- can still delete messages sent earlier
          let msgIds = itemsMsgIds items
              events = L.nonEmpty $ map (`XMsgDel` Nothing) msgIds
          mapM_ (sendGroupMessages user gInfo ms) events
          delGroupChatItems user gInfo items Nothing
    CTLocal -> do
      (nf, items) <- getCommandLocalChatItems user chatId itemIds
      deleteLocalCIs user nf items True False
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    where
      assertDeletable :: forall c. ChatTypeI c => [CChatItem c] -> CM ()
      assertDeletable items = do
        currentTs <- liftIO getCurrentTime
        unless (all (itemDeletable currentTs) items) $ throwChatError CEInvalidChatItemDelete
        where
          itemDeletable :: UTCTime -> CChatItem c -> Bool
          itemDeletable currentTs (CChatItem msgDir ChatItem {meta = CIMeta {itemSharedMsgId, itemTs, itemDeleted}, content}) =
            case msgDir of
              -- We check with a 6 hour margin compared to CIMeta deletable to account for deletion on the border
              SMDSnd -> isJust itemSharedMsgId && deletable' content itemDeleted itemTs (nominalDay + 6 * 3600) currentTs
              SMDRcv -> False
      itemsMsgIds :: [CChatItem c] -> [SharedMsgId]
      itemsMsgIds = mapMaybe (\(CChatItem _ ChatItem {meta = CIMeta {itemSharedMsgId}}) -> itemSharedMsgId)
  APIDeleteMemberChatItem gId itemIds -> withUser $ \user -> withGroupLock "deleteChatItem" gId $ do
    (gInfo@GroupInfo {membership}, items) <- getCommandGroupChatItems user gId itemIds
    ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
    assertDeletable gInfo items
    assertUserGroupRole gInfo GRAdmin -- TODO GRModerator when most users migrate
    let msgMemIds = itemsMsgMemIds gInfo items
        events = L.nonEmpty $ map (\(msgId, memId) -> XMsgDel msgId (Just memId)) msgMemIds
    mapM_ (sendGroupMessages user gInfo ms) events
    delGroupChatItems user gInfo items (Just membership)
    where
      assertDeletable :: GroupInfo -> [CChatItem 'CTGroup] -> CM ()
      assertDeletable GroupInfo {membership = GroupMember {memberRole = membershipMemRole}} items =
        unless (all itemDeletable items) $ throwChatError CEInvalidChatItemDelete
        where
          itemDeletable :: CChatItem 'CTGroup -> Bool
          itemDeletable (CChatItem _ ChatItem {chatDir, meta = CIMeta {itemSharedMsgId}}) =
            case chatDir of
              CIGroupRcv GroupMember {memberRole} -> membershipMemRole >= memberRole && isJust itemSharedMsgId
              CIGroupSnd -> isJust itemSharedMsgId
      itemsMsgMemIds :: GroupInfo -> [CChatItem 'CTGroup] -> [(SharedMsgId, MemberId)]
      itemsMsgMemIds GroupInfo {membership = GroupMember {memberId = membershipMemId}} = mapMaybe itemMsgMemIds
        where
          itemMsgMemIds :: CChatItem 'CTGroup -> Maybe (SharedMsgId, MemberId)
          itemMsgMemIds (CChatItem _ ChatItem {chatDir, meta = CIMeta {itemSharedMsgId}}) =
            join <$> forM itemSharedMsgId $ \msgId -> Just $ case chatDir of
              CIGroupRcv GroupMember {memberId} -> (msgId, memberId)
              CIGroupSnd -> (msgId, membershipMemId)
  APIChatItemReaction (ChatRef cType chatId) itemId add reaction -> withUser $ \user -> case cType of
    CTDirect ->
      withContactLock "chatItemReaction" chatId $
        withFastStore (\db -> (,) <$> getContact db vr user chatId <*> getDirectChatItem db user chatId itemId) >>= \case
          (ct, CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}}) -> do
            unless (featureAllowed SCFReactions forUser ct) $
              throwChatError (CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions))
            unless (ciReactionAllowed ci) $
              throwChatError (CECommandError "reaction not allowed - chat item has no content")
            rs <- withFastStore' $ \db -> getDirectReactions db ct itemSharedMId True
            checkReactionAllowed rs
            (SndMessage {msgId}, _) <- sendDirectContactMessage user ct $ XMsgReact itemSharedMId Nothing reaction add
            createdAt <- liftIO getCurrentTime
            reactions <- withFastStore' $ \db -> do
              setDirectReaction db ct itemSharedMId True reaction add msgId createdAt
              liftIO $ getDirectCIReactions db ct itemSharedMId
            let ci' = CChatItem md ci {reactions}
                r = ACIReaction SCTDirect SMDSnd (DirectChat ct) $ CIReaction CIDirectSnd ci' createdAt reaction
            pure $ CRChatItemReaction user add r
          _ -> throwChatError $ CECommandError "reaction not possible - no shared item ID"
    CTGroup ->
      withGroupLock "chatItemReaction" chatId $
        withFastStore (\db -> (,) <$> getGroup db vr user chatId <*> getGroupChatItem db user chatId itemId) >>= \case
          (Group g@GroupInfo {membership} ms, CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}}) -> do
            unless (groupFeatureAllowed SGFReactions g) $
              throwChatError (CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions))
            unless (ciReactionAllowed ci) $
              throwChatError (CECommandError "reaction not allowed - chat item has no content")
            let GroupMember {memberId = itemMemberId} = chatItemMember g ci
            rs <- withFastStore' $ \db -> getGroupReactions db g membership itemMemberId itemSharedMId True
            checkReactionAllowed rs
            SndMessage {msgId} <- sendGroupMessage user g ms (XMsgReact itemSharedMId (Just itemMemberId) reaction add)
            createdAt <- liftIO getCurrentTime
            reactions <- withFastStore' $ \db -> do
              setGroupReaction db g membership itemMemberId itemSharedMId True reaction add msgId createdAt
              liftIO $ getGroupCIReactions db g itemMemberId itemSharedMId
            let ci' = CChatItem md ci {reactions}
                r = ACIReaction SCTGroup SMDSnd (GroupChat g) $ CIReaction CIGroupSnd ci' createdAt reaction
            pure $ CRChatItemReaction user add r
          _ -> throwChatError $ CECommandError "reaction not possible - no shared item ID"
    CTLocal -> pure $ chatCmdError (Just user) "not supported"
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    where
      checkReactionAllowed rs = do
        when ((reaction `elem` rs) == add) $
          throwChatError (CECommandError $ "reaction already " <> if add then "added" else "removed")
        when (add && length rs >= maxMsgReactions) $
          throwChatError (CECommandError "too many reactions")
  APIGetReactionMembers userId groupId itemId reaction -> withUserId userId $ \user -> do
    memberReactions <- withStore $ \db -> do
      CChatItem _ ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}} <- getGroupChatItem db user groupId itemId
      liftIO $ getReactionMembers db vr user groupId itemSharedMId reaction
    pure $ CRReactionMembers user memberReactions
  APIPlanForwardChatItems (ChatRef fromCType fromChatId) itemIds -> withUser $ \user -> case fromCType of
    CTDirect -> planForward user . snd =<< getCommandDirectChatItems user fromChatId itemIds
    CTGroup -> planForward user . snd =<< getCommandGroupChatItems user fromChatId itemIds
    CTLocal -> planForward user . snd =<< getCommandLocalChatItems user fromChatId itemIds
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    where
      planForward :: User -> [CChatItem c] -> CM ChatResponse
      planForward user items = do
        (itemIds', forwardErrors) <- unzip <$> mapM planItemForward items
        let forwardConfirmation = case catMaybes forwardErrors of
              [] -> Nothing
              errs -> Just $ case mainErr of
                FFENotAccepted _ -> FCFilesNotAccepted fileIds
                FFEInProgress -> FCFilesInProgress filesCount
                FFEMissing -> FCFilesMissing filesCount
                FFEFailed -> FCFilesFailed filesCount
                where
                  mainErr = minimum errs
                  fileIds = catMaybes $ map (\case FFENotAccepted ftId -> Just ftId; _ -> Nothing) errs
                  filesCount = length $ filter (mainErr ==) errs
        pure CRForwardPlan {user, itemsCount = length itemIds, chatItemIds = catMaybes itemIds', forwardConfirmation}
        where
          planItemForward :: CChatItem c -> CM (Maybe ChatItemId, Maybe ForwardFileError)
          planItemForward (CChatItem _ ci) = forwardMsgContent ci >>= maybe (pure (Nothing, Nothing)) (forwardContentPlan ci)
          forwardContentPlan :: ChatItem c d -> MsgContent -> CM (Maybe ChatItemId, Maybe ForwardFileError)
          forwardContentPlan ChatItem {file, meta = CIMeta {itemId}} mc = case file of
            Nothing -> pure (Just itemId, Nothing)
            Just CIFile {fileId, fileStatus, fileSource} -> case ciFileForwardError fileId fileStatus of
              Just err -> pure $ itemIdWithoutFile err
              Nothing -> case fileSource of
                Just CryptoFile {filePath} -> do
                  exists <- doesFileExist =<< lift (toFSFilePath filePath)
                  pure $ if exists then (Just itemId, Nothing) else itemIdWithoutFile FFEMissing
                Nothing -> pure $ itemIdWithoutFile FFEMissing
            where
              itemIdWithoutFile err = (if hasContent then Just itemId else Nothing, Just err)
              hasContent = case mc of
                MCText _ -> True
                MCLink {} -> True
                MCImage {} -> True
                MCVideo {text} -> text /= ""
                MCVoice {text} -> text /= ""
                MCFile t -> t /= ""
                MCReport {} -> True
                MCUnknown {} -> True
  APIForwardChatItems (ChatRef toCType toChatId) (ChatRef fromCType fromChatId) itemIds itemTTL -> withUser $ \user -> case toCType of
    CTDirect -> do
      cmrs <- prepareForward user
      case L.nonEmpty cmrs of
        Just cmrs' ->
          withContactLock "forwardChatItem, to contact" toChatId $
            sendContactContentMessages user toChatId False itemTTL cmrs'
        Nothing -> pure $ CRNewChatItems user []
    CTGroup -> do
      cmrs <- prepareForward user
      case L.nonEmpty cmrs of
        Just cmrs' ->
          withGroupLock "forwardChatItem, to group" toChatId $
            sendGroupContentMessages user toChatId False itemTTL cmrs'
        Nothing -> pure $ CRNewChatItems user []
    CTLocal -> do
      cmrs <- prepareForward user
      case L.nonEmpty cmrs of
        Just cmrs' ->
          createNoteFolderContentItems user toChatId cmrs'
        Nothing -> pure $ CRNewChatItems user []
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    where
      prepareForward :: User -> CM [ComposeMessageReq]
      prepareForward user = case fromCType of
        CTDirect -> withContactLock "forwardChatItem, from contact" fromChatId $ do
          (ct, items) <- getCommandDirectChatItems user fromChatId itemIds
          catMaybes <$> mapM (\ci -> ciComposeMsgReq ct ci <$$> prepareMsgReq ci) items
          where
            ciComposeMsgReq :: Contact -> CChatItem 'CTDirect -> (MsgContent, Maybe CryptoFile) -> ComposeMessageReq
            ciComposeMsgReq ct (CChatItem md ci) (mc', file) =
              let itemId = chatItemId' ci
                  ciff = forwardCIFF ci $ Just (CIFFContact (forwardName ct) (toMsgDirection md) (Just fromChatId) (Just itemId))
               in (ComposedMessage file Nothing mc', ciff)
              where
                forwardName :: Contact -> ContactName
                forwardName Contact {profile = LocalProfile {displayName, localAlias}}
                  | localAlias /= "" = localAlias
                  | otherwise = displayName
        CTGroup -> withGroupLock "forwardChatItem, from group" fromChatId $ do
          (gInfo, items) <- getCommandGroupChatItems user fromChatId itemIds
          catMaybes <$> mapM (\ci -> ciComposeMsgReq gInfo ci <$$> prepareMsgReq ci) items
          where
            ciComposeMsgReq :: GroupInfo -> CChatItem 'CTGroup -> (MsgContent, Maybe CryptoFile) -> ComposeMessageReq
            ciComposeMsgReq gInfo (CChatItem md ci) (mc', file) = do
              let itemId = chatItemId' ci
                  ciff = forwardCIFF ci $ Just (CIFFGroup (forwardName gInfo) (toMsgDirection md) (Just fromChatId) (Just itemId))
               in (ComposedMessage file Nothing mc', ciff)
              where
                forwardName :: GroupInfo -> ContactName
                forwardName GroupInfo {groupProfile = GroupProfile {displayName}} = displayName
        CTLocal -> do
          (_, items) <- getCommandLocalChatItems user fromChatId itemIds
          catMaybes <$> mapM (\ci -> ciComposeMsgReq ci <$$> prepareMsgReq ci) items
          where
            ciComposeMsgReq :: CChatItem 'CTLocal -> (MsgContent, Maybe CryptoFile) -> ComposeMessageReq
            ciComposeMsgReq (CChatItem _ ci) (mc', file) =
              let ciff = forwardCIFF ci Nothing
               in (ComposedMessage file Nothing mc', ciff)
        CTContactRequest -> throwChatError $ CECommandError "not supported"
        CTContactConnection -> throwChatError $ CECommandError "not supported"
        where
          prepareMsgReq :: CChatItem c -> CM (Maybe (MsgContent, Maybe CryptoFile))
          prepareMsgReq (CChatItem _ ci) = forwardMsgContent ci $>>= forwardContent ci
          forwardCIFF :: ChatItem c d -> Maybe CIForwardedFrom -> Maybe CIForwardedFrom
          forwardCIFF ChatItem {meta = CIMeta {itemForwarded}} ciff = case itemForwarded of
            Nothing -> ciff
            Just CIFFUnknown -> ciff
            Just prevCIFF -> Just prevCIFF
          forwardContent :: ChatItem c d -> MsgContent -> CM (Maybe (MsgContent, Maybe CryptoFile))
          forwardContent ChatItem {file} mc = case file of
            Nothing -> pure $ Just (mc, Nothing)
            Just CIFile {fileName, fileStatus, fileSource = Just fromCF@CryptoFile {filePath}}
              | ciFileLoaded fileStatus ->
                  chatReadVar filesFolder >>= \case
                    Nothing ->
                      ifM (doesFileExist filePath) (pure $ Just (mc, Just fromCF)) (pure contentWithoutFile)
                    Just filesFolder -> do
                      let fsFromPath = filesFolder </> filePath
                      ifM
                        (doesFileExist fsFromPath)
                        ( do
                            newFileName <- case mc of
                              MCImage {} -> liftIO $ generateNewFileName fileName imageFilePrefix
                              MCVoice {} -> liftIO $ generateNewFileName fileName voiceFilePrefix
                              MCVideo {} -> liftIO $ generateNewFileName fileName videoFilePrefix
                              _ -> pure fileName
                            fsNewPath <- liftIO $ filesFolder `uniqueCombine` newFileName
                            liftIO $ B.writeFile fsNewPath "" -- create empty file
                            encrypt <- chatReadVar encryptLocalFiles
                            cfArgs <- if encrypt then Just <$> (atomically . CF.randomArgs =<< asks random) else pure Nothing
                            let toCF = CryptoFile fsNewPath cfArgs
                            -- to keep forwarded file in case original is deleted
                            liftIOEither $ runExceptT $ withExceptT (ChatError . CEInternalError . show) $ copyCryptoFile (fromCF {filePath = fsFromPath} :: CryptoFile) toCF
                            pure $ Just (mc, Just (toCF {filePath = takeFileName fsNewPath} :: CryptoFile))
                        )
                        (pure contentWithoutFile)
            _ -> pure contentWithoutFile
            where
              contentWithoutFile = case mc of
                MCImage {} -> Just (mc, Nothing)
                MCLink {} -> Just (mc, Nothing)
                _ | contentText /= "" -> Just (MCText contentText, Nothing)
                _ -> Nothing
              contentText = msgContentText mc
          copyCryptoFile :: CryptoFile -> CryptoFile -> ExceptT CF.FTCryptoError IO ()
          copyCryptoFile fromCF@CryptoFile {filePath = fsFromPath, cryptoArgs = fromArgs} toCF@CryptoFile {cryptoArgs = toArgs} = do
            fromSizeFull <- getFileSize fsFromPath
            let fromSize = fromSizeFull - maybe 0 (const $ toInteger C.authTagSize) fromArgs
            CF.withFile fromCF ReadMode $ \fromH ->
              CF.withFile toCF WriteMode $ \toH -> do
                copyChunks fromH toH fromSize
                forM_ fromArgs $ \_ -> CF.hGetTag fromH
                forM_ toArgs $ \_ -> liftIO $ CF.hPutTag toH
            where
              copyChunks :: CF.CryptoFileHandle -> CF.CryptoFileHandle -> Integer -> ExceptT CF.FTCryptoError IO ()
              copyChunks r w size = do
                let chSize = min size U.chunkSize
                    chSize' = fromIntegral chSize
                    size' = size - chSize
                ch <- liftIO $ CF.hGet r chSize'
                when (B.length ch /= chSize') $ throwError $ CF.FTCEFileIOError "encrypting file: unexpected EOF"
                liftIO . CF.hPut w $ LB.fromStrict ch
                when (size' > 0) $ copyChunks r w size'
          generateNewFileName :: String -> String -> IO String
          generateNewFileName fileName prefix = do
            currentDate <- liftIO getCurrentTime
            let formattedDate = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" currentDate
            let ext = takeExtension fileName
            pure $ prefix <> formattedDate <> ext
  APIUserRead userId -> withUserId userId $ \user -> withFastStore' (`setUserChatsRead` user) >> ok user
  UserRead -> withUser $ \User {userId} -> processChatCommand $ APIUserRead userId
  APIChatRead chatRef@(ChatRef cType chatId) -> withUser $ \_ -> case cType of
    CTDirect -> do
      user <- withFastStore $ \db -> getUserByContactId db chatId
      ts <- liftIO getCurrentTime
      timedItems <- withFastStore' $ \db -> do
        timedItems <- getDirectUnreadTimedItems db user chatId
        updateDirectChatItemsRead db user chatId
        setDirectChatItemsDeleteAt db user chatId timedItems ts
      forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
      ok user
    CTGroup -> do
      user <- withFastStore $ \db -> getUserByGroupId db chatId
      ts <- liftIO getCurrentTime
      timedItems <- withFastStore' $ \db -> do
        timedItems <- getGroupUnreadTimedItems db user chatId
        updateGroupChatItemsRead db user chatId
        setGroupChatItemsDeleteAt db user chatId timedItems ts
      forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
      ok user
    CTLocal -> do
      user <- withFastStore $ \db -> getUserByNoteFolderId db chatId
      withFastStore' $ \db -> updateLocalChatItemsRead db user chatId
      ok user
    CTContactRequest -> pure $ chatCmdError Nothing "not supported"
    CTContactConnection -> pure $ chatCmdError Nothing "not supported"
  APIChatItemsRead chatRef@(ChatRef cType chatId) itemIds -> withUser $ \_ -> case cType of
    CTDirect -> do
      user <- withFastStore $ \db -> getUserByContactId db chatId
      timedItems <- withFastStore' $ \db -> do
        timedItems <- updateDirectChatItemsReadList db user chatId itemIds
        setDirectChatItemsDeleteAt db user chatId timedItems =<< getCurrentTime
      forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
      ok user
    CTGroup -> do
      user <- withFastStore $ \db -> getUserByGroupId db chatId
      timedItems <- withFastStore' $ \db -> do
        timedItems <- updateGroupChatItemsReadList db user chatId itemIds
        setGroupChatItemsDeleteAt db user chatId timedItems =<< getCurrentTime
      forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
      ok user
    CTLocal -> pure $ chatCmdError Nothing "not supported"
    CTContactRequest -> pure $ chatCmdError Nothing "not supported"
    CTContactConnection -> pure $ chatCmdError Nothing "not supported"
  APIChatUnread (ChatRef cType chatId) unreadChat -> withUser $ \user -> case cType of
    CTDirect -> do
      withFastStore $ \db -> do
        ct <- getContact db vr user chatId
        liftIO $ updateContactUnreadChat db user ct unreadChat
      ok user
    CTGroup -> do
      withFastStore $ \db -> do
        Group {groupInfo} <- getGroup db vr user chatId
        liftIO $ updateGroupUnreadChat db user groupInfo unreadChat
      ok user
    CTLocal -> do
      withFastStore $ \db -> do
        nf <- getNoteFolder db user chatId
        liftIO $ updateNoteFolderUnreadChat db user nf unreadChat
      ok user
    _ -> pure $ chatCmdError (Just user) "not supported"
  APIDeleteChat cRef@(ChatRef cType chatId) cdm -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> getContact db vr user chatId
      filesInfo <- withFastStore' $ \db -> getContactFileInfo db user ct
      withContactLock "deleteChat direct" chatId . procCmd $
        case cdm of
          CDMFull notify -> do
            cancelFilesInProgress user filesInfo
            deleteFilesLocally filesInfo
            sendDelDeleteConns ct notify
            -- functions below are called in separate transactions to prevent crashes on android
            -- (possibly, race condition on integrity check?)
            withFastStore' $ \db -> do
              deleteContactConnections db user ct
              deleteContactFiles db user ct
            withFastStore $ \db -> deleteContact db user ct
            pure $ CRContactDeleted user ct
          CDMEntity notify -> do
            cancelFilesInProgress user filesInfo
            sendDelDeleteConns ct notify
            ct' <- withFastStore $ \db -> do
              liftIO $ deleteContactConnections db user ct
              liftIO $ void $ updateContactStatus db user ct CSDeletedByUser
              getContact db vr user chatId
            pure $ CRContactDeleted user ct'
          CDMMessages -> do
            void $ processChatCommand $ APIClearChat cRef
            withFastStore' $ \db -> setContactChatDeleted db user ct True
            pure $ CRContactDeleted user ct {chatDeleted = True}
      where
        sendDelDeleteConns ct notify = do
          let doSendDel = contactReady ct && contactActive ct && notify
          when doSendDel $ void (sendDirectContactMessage user ct XDirectDel) `catchChatError` const (pure ())
          contactConnIds <- map aConnId <$> withFastStore' (\db -> getContactConnections db vr userId ct)
          deleteAgentConnectionsAsync' user contactConnIds doSendDel
    CTContactConnection -> withConnectionLock "deleteChat contactConnection" chatId . procCmd $ do
      conn@PendingContactConnection {pccAgentConnId = AgentConnId acId} <- withFastStore $ \db -> getPendingContactConnection db userId chatId
      deleteAgentConnectionAsync user acId
      withFastStore' $ \db -> deletePendingContactConnection db userId chatId
      pure $ CRContactConnectionDeleted user conn
    CTGroup -> do
      Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db vr user chatId
      let GroupMember {memberRole = membershipMemRole} = membership
      let isOwner = membershipMemRole == GROwner
          canDelete = isOwner || not (memberCurrent membership)
      unless canDelete $ throwChatError $ CEGroupUserRole gInfo GROwner
      filesInfo <- withStore' $ \db -> getGroupFileInfo db user gInfo
      withGroupLock "deleteChat group" chatId . procCmd $ do
        cancelFilesInProgress user filesInfo
        deleteFilesLocally filesInfo
        let doSendDel = memberActive membership && isOwner
        when doSendDel . void $ sendGroupMessage' user gInfo members XGrpDel
        deleteGroupLinkIfExists user gInfo
        deleteMembersConnections' user members doSendDel
        updateCIGroupInvitationStatus user gInfo CIGISRejected `catchChatError` \_ -> pure ()
        -- functions below are called in separate transactions to prevent crashes on android
        -- (possibly, race condition on integrity check?)
        withStore' $ \db -> deleteGroupConnectionsAndFiles db user gInfo members
        withStore' $ \db -> deleteGroupItemsAndMembers db user gInfo members
        withStore' $ \db -> deleteGroup db user gInfo
        let contactIds = mapMaybe memberContactId members
        (errs1, (errs2, connIds)) <- lift $ second unzip . partitionEithers <$> withStoreBatch (\db -> map (deleteUnusedContact db) contactIds)
        let errs = errs1 <> mapMaybe (fmap ChatErrorStore) errs2
        unless (null errs) $ toView $ CRChatErrors (Just user) errs
        deleteAgentConnectionsAsync user $ concat connIds
        pure $ CRGroupDeletedUser user gInfo
      where
        deleteUnusedContact :: DB.Connection -> ContactId -> IO (Either ChatError (Maybe StoreError, [ConnId]))
        deleteUnusedContact db contactId = runExceptT . withExceptT ChatErrorStore $ do
          ct <- getContact db vr user contactId
          ifM
            ((directOrUsed ct ||) . isJust <$> liftIO (checkContactHasGroups db user ct))
            (pure (Nothing, []))
            (getConnections ct)
          where
            getConnections :: Contact -> ExceptT StoreError IO (Maybe StoreError, [ConnId])
            getConnections ct = do
              conns <- liftIO $ getContactConnections db vr userId ct
              e_ <- (setContactDeleted db user ct $> Nothing) `catchStoreError` (pure . Just)
              pure (e_, map aConnId conns)
    CTLocal -> pure $ chatCmdError (Just user) "not supported"
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
  APIClearChat (ChatRef cType chatId) -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> getContact db vr user chatId
      filesInfo <- withFastStore' $ \db -> getContactFileInfo db user ct
      cancelFilesInProgress user filesInfo
      deleteFilesLocally filesInfo
      withFastStore' $ \db -> deleteContactCIs db user ct
      pure $ CRChatCleared user (AChatInfo SCTDirect $ DirectChat ct)
    CTGroup -> do
      gInfo <- withFastStore $ \db -> getGroupInfo db vr user chatId
      filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
      cancelFilesInProgress user filesInfo
      deleteFilesLocally filesInfo
      withFastStore' $ \db -> deleteGroupChatItemsMessages db user gInfo
      membersToDelete <- withFastStore' $ \db -> getGroupMembersForExpiration db vr user gInfo
      forM_ membersToDelete $ \m -> withFastStore' $ \db -> deleteGroupMember db user m
      pure $ CRChatCleared user (AChatInfo SCTGroup $ GroupChat gInfo)
    CTLocal -> do
      nf <- withFastStore $ \db -> getNoteFolder db user chatId
      filesInfo <- withFastStore' $ \db -> getNoteFolderFileInfo db user nf
      deleteFilesLocally filesInfo
      withFastStore' $ \db -> deleteNoteFolderFiles db userId nf
      withFastStore' $ \db -> deleteNoteFolderCIs db user nf
      pure $ CRChatCleared user (AChatInfo SCTLocal $ LocalChat nf)
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
  APIAcceptContact incognito connReqId -> withUser $ \_ -> do
    userContactLinkId <- withFastStore $ \db -> getUserContactLinkIdByCReq db connReqId
    withUserContactLock "acceptContact" userContactLinkId $ do
      (user@User {userId}, cReq) <- withFastStore $ \db -> getContactRequest' db connReqId
      (ct, conn@Connection {connId}, sqSecured) <- acceptContactRequest user cReq incognito
      ucl <- withFastStore $ \db -> getUserContactLinkById db userId userContactLinkId
      let contactUsed = (\(_, groupId_, _) -> isNothing groupId_) ucl
      ct' <- withStore' $ \db -> do
        deleteContactRequestRec db user cReq
        updateContactAccepted db user ct contactUsed
        conn' <-
          if sqSecured
            then conn {connStatus = ConnSndReady} <$ updateConnectionStatusFromTo db connId ConnNew ConnSndReady
            else pure conn
        pure ct {contactUsed, activeConn = Just conn'}
      pure $ CRAcceptingContactRequest user ct'
  APIRejectContact connReqId -> withUser $ \user -> do
    userContactLinkId <- withFastStore $ \db -> getUserContactLinkIdByCReq db connReqId
    withUserContactLock "rejectContact" userContactLinkId $ do
      cReq@UserContactRequest {agentContactConnId = AgentConnId connId, agentInvitationId = AgentInvId invId} <-
        withFastStore $ \db ->
          getContactRequest db user connReqId
            `storeFinally` liftIO (deleteContactRequest db user connReqId)
      withAgent $ \a -> rejectContact a connId invId
      pure $ CRContactRequestRejected user cReq
  APISendCallInvitation contactId callType -> withUser $ \user -> do
    -- party initiating call
    ct <- withFastStore $ \db -> getContact db vr user contactId
    assertDirectAllowed user MDSnd ct XCallInv_
    if featureAllowed SCFCalls forUser ct
      then do
        calls <- asks currentCalls
        withContactLock "sendCallInvitation" contactId $ do
          g <- asks random
          callId <- atomically $ CallId <$> C.randomBytes 16 g
          callUUID <- UUID.toText <$> liftIO V4.nextRandom
          dhKeyPair <- atomically $ if encryptedCall callType then Just <$> C.generateKeyPair g else pure Nothing
          let invitation = CallInvitation {callType, callDhPubKey = fst <$> dhKeyPair}
              callState = CallInvitationSent {localCallType = callType, localDhPrivKey = snd <$> dhKeyPair}
          (msg, _) <- sendDirectContactMessage user ct (XCallInv callId invitation)
          ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndCall CISCallPending 0)
          let call' = Call {contactId, callId, callUUID, chatItemId = chatItemId' ci, callState, callTs = chatItemTs' ci}
          call_ <- atomically $ TM.lookupInsert contactId call' calls
          forM_ call_ $ \call -> updateCallItemStatus user ct call WCSDisconnected Nothing
          toView $ CRNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]
          ok user
      else pure $ chatCmdError (Just user) ("feature not allowed " <> T.unpack (chatFeatureNameText CFCalls))
  SendCallInvitation cName callType -> withUser $ \user -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user cName
    processChatCommand $ APISendCallInvitation contactId callType
  APIRejectCall contactId ->
    -- party accepting call
    withCurrentCall contactId $ \user ct Call {chatItemId, callState} -> case callState of
      CallInvitationReceived {} -> do
        let aciContent = ACIContent SMDRcv $ CIRcvCall CISCallRejected 0
        withFastStore' $ \db -> setDirectChatItemRead db user contactId chatItemId
        timed_ <- contactCITimed ct
        updateDirectChatItemView user ct chatItemId aciContent False False timed_ Nothing
        forM_ (timed_ >>= timedDeleteAt') $
          startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId)
        pure Nothing
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallOffer contactId WebRTCCallOffer {callType, rtcSession} ->
    -- party accepting call
    withCurrentCall contactId $ \user ct call@Call {callId, chatItemId, callState} -> case callState of
      CallInvitationReceived {peerCallType, localDhPubKey, sharedKey} -> do
        let callDhPubKey = if encryptedCall callType then localDhPubKey else Nothing
            offer = CallOffer {callType, rtcSession, callDhPubKey}
            callState' = CallOfferSent {localCallType = callType, peerCallType, localCallSession = rtcSession, sharedKey}
            aciContent = ACIContent SMDRcv $ CIRcvCall CISCallAccepted 0
        (SndMessage {msgId}, _) <- sendDirectContactMessage user ct (XCallOffer callId offer)
        withFastStore' $ \db -> setDirectChatItemRead db user contactId chatItemId
        updateDirectChatItemView user ct chatItemId aciContent False False Nothing $ Just msgId
        pure $ Just call {callState = callState'}
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallAnswer contactId rtcSession ->
    -- party initiating call
    withCurrentCall contactId $ \user ct call@Call {callId, chatItemId, callState} -> case callState of
      CallOfferReceived {localCallType, peerCallType, peerCallSession, sharedKey} -> do
        let callState' = CallNegotiated {localCallType, peerCallType, localCallSession = rtcSession, peerCallSession, sharedKey}
            aciContent = ACIContent SMDSnd $ CISndCall CISCallNegotiated 0
        (SndMessage {msgId}, _) <- sendDirectContactMessage user ct (XCallAnswer callId CallAnswer {rtcSession})
        updateDirectChatItemView user ct chatItemId aciContent False False Nothing $ Just msgId
        pure $ Just call {callState = callState'}
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallExtraInfo contactId rtcExtraInfo ->
    -- any call party
    withCurrentCall contactId $ \user ct call@Call {callId, callState} -> case callState of
      CallOfferSent {localCallType, peerCallType, localCallSession, sharedKey} -> do
        -- TODO update the list of ice servers in localCallSession
        void . sendDirectContactMessage user ct $ XCallExtra callId CallExtraInfo {rtcExtraInfo}
        let callState' = CallOfferSent {localCallType, peerCallType, localCallSession, sharedKey}
        pure $ Just call {callState = callState'}
      CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey} -> do
        -- TODO update the list of ice servers in localCallSession
        void . sendDirectContactMessage user ct $ XCallExtra callId CallExtraInfo {rtcExtraInfo}
        let callState' = CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey}
        pure $ Just call {callState = callState'}
      _ -> throwChatError . CECallState $ callStateTag callState
  APIEndCall contactId ->
    -- any call party
    withCurrentCall contactId $ \user ct call@Call {callId} -> do
      (SndMessage {msgId}, _) <- sendDirectContactMessage user ct (XCallEnd callId)
      updateCallItemStatus user ct call WCSDisconnected $ Just msgId
      pure Nothing
  APIGetCallInvitations -> withUser' $ \_ -> lift $ do
    calls <- asks currentCalls >>= readTVarIO
    let invs = mapMaybe callInvitation $ M.elems calls
    rcvCallInvitations <- rights <$> mapM rcvCallInvitation invs
    pure $ CRCallInvitations rcvCallInvitations
    where
      callInvitation Call {contactId, callUUID, callState, callTs} = case callState of
        CallInvitationReceived {peerCallType, sharedKey} -> Just (contactId, callUUID, callTs, peerCallType, sharedKey)
        _ -> Nothing
      rcvCallInvitation (contactId, callUUID, callTs, peerCallType, sharedKey) = runExceptT . withFastStore $ \db -> do
        user <- getUserByContactId db contactId
        contact <- getContact db vr user contactId
        pure RcvCallInvitation {user, contact, callType = peerCallType, sharedKey, callUUID, callTs}
  APIGetNetworkStatuses -> withUser $ \_ ->
    CRNetworkStatuses Nothing . map (uncurry ConnNetworkStatus) . M.toList <$> chatReadVar connNetworkStatuses
  APICallStatus contactId receivedStatus ->
    withCurrentCall contactId $ \user ct call ->
      updateCallItemStatus user ct call receivedStatus Nothing $> Just call
  APIUpdateProfile userId profile -> withUserId userId (`updateProfile` profile)
  APISetContactPrefs contactId prefs' -> withUser $ \user -> do
    ct <- withFastStore $ \db -> getContact db vr user contactId
    updateContactPrefs user ct prefs'
  APISetContactAlias contactId localAlias -> withUser $ \user@User {userId} -> do
    ct' <- withFastStore $ \db -> do
      ct <- getContact db vr user contactId
      liftIO $ updateContactAlias db userId ct localAlias
    pure $ CRContactAliasUpdated user ct'
  APISetConnectionAlias connId localAlias -> withUser $ \user@User {userId} -> do
    conn' <- withFastStore $ \db -> do
      conn <- getPendingContactConnection db userId connId
      liftIO $ updateContactConnectionAlias db userId conn localAlias
    pure $ CRConnectionAliasUpdated user conn'
  APISetUserUIThemes uId uiThemes -> withUser $ \user@User {userId} -> do
    user'@User {userId = uId'} <- withFastStore $ \db -> do
      user' <- getUser db uId
      liftIO $ setUserUIThemes db user uiThemes
      pure user'
    when (userId == uId') $ chatWriteVar currentUser $ Just (user :: User) {uiThemes}
    ok user'
  APISetChatUIThemes (ChatRef cType chatId) uiThemes -> withUser $ \user -> case cType of
    CTDirect -> do
      withFastStore $ \db -> do
        ct <- getContact db vr user chatId
        liftIO $ setContactUIThemes db user ct uiThemes
      ok user
    CTGroup -> do
      withFastStore $ \db -> do
        g <- getGroupInfo db vr user chatId
        liftIO $ setGroupUIThemes db user g uiThemes
      ok user
    _ -> pure $ chatCmdError (Just user) "not supported"
  APIParseMarkdown text -> pure . CRApiParsedMarkdown $ parseMaybeMarkdownList text
  APIGetNtfToken -> withUser' $ \_ -> crNtfToken <$> withAgent getNtfToken
  APIRegisterToken token mode -> withUser $ \_ ->
    CRNtfTokenStatus <$> withAgent (\a -> registerNtfToken a token mode)
  APIVerifyToken token nonce code -> withUser $ \_ -> withAgent (\a -> verifyNtfToken a token nonce code) >> ok_
  APIDeleteToken token -> withUser $ \_ -> withAgent (`deleteNtfToken` token) >> ok_
  APIGetNtfConns nonce encNtfInfo -> withUser $ \user -> do
    ntfInfos <- withAgent $ \a -> getNotificationConns a nonce encNtfInfo
    (errs, ntfMsgs) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (getMsgConn db) (L.toList ntfInfos))
    unless (null errs) $ toView $ CRChatErrors (Just user) errs
    pure $ CRNtfConns ntfMsgs
    where
      getMsgConn :: DB.Connection -> NotificationInfo -> IO NtfConn
      getMsgConn db NotificationInfo {ntfConnId, ntfMsgMeta = nMsgMeta} = do
        let agentConnId = AgentConnId ntfConnId
        user_ <- getUserByAConnId db agentConnId
        connEntity_ <-
          pure user_ $>>= \user ->
            eitherToMaybe <$> runExceptT (getConnectionEntity db vr user agentConnId)
        pure $
          NtfConn
            { user_,
              connEntity_,
              -- Decrypted ntf meta of the expected message (the one notification was sent for)
              expectedMsg_ = expectedMsgInfo <$> nMsgMeta
            }
  ApiGetConnNtfMessages connIds -> withUser $ \_ -> do
    let acIds = L.map (\(AgentConnId acId) -> acId) connIds
    msgs <- lift $ withAgent' $ \a -> getConnectionMessages a acIds
    let ntfMsgs = L.map (\msg -> receivedMsgInfo <$> msg) msgs
    pure $ CRConnNtfMessages ntfMsgs
  GetUserProtoServers (AProtocolType p) -> withUser $ \user -> withServerProtocol p $ do
    srvs <- withFastStore (`getUserServers` user)
    liftIO $ CRUserServers user <$> groupByOperator (protocolServers p srvs)
  SetUserProtoServers (AProtocolType (p :: SProtocolType p)) srvs -> withUser $ \user@User {userId} -> withServerProtocol p $ do
    userServers_ <- liftIO . groupByOperator =<< withFastStore (`getUserServers` user)
    case L.nonEmpty userServers_ of
      Nothing -> throwChatError $ CECommandError "no servers"
      Just userServers -> case srvs of
        [] -> throwChatError $ CECommandError "no servers"
        _ -> do
          srvs' <- mapM aUserServer srvs
          processChatCommand $ APISetUserServers userId $ L.map (updatedServers p srvs') userServers
    where
      aUserServer :: AProtoServerWithAuth -> CM (AUserServer p)
      aUserServer (AProtoServerWithAuth p' srv) = case testEquality p p' of
        Just Refl -> pure $ AUS SDBNew $ newUserServer srv
        Nothing -> throwChatError $ CECommandError $ "incorrect server protocol: " <> B.unpack (strEncode srv)
  APITestProtoServer userId srv@(AProtoServerWithAuth _ server) -> withUserId userId $ \user ->
    lift $ CRServerTestResult user srv <$> withAgent' (\a -> testProtocolServer a (aUserId user) server)
  TestProtoServer srv -> withUser $ \User {userId} ->
    processChatCommand $ APITestProtoServer userId srv
  APIGetServerOperators -> CRServerOperatorConditions <$> withFastStore getServerOperators
  APISetServerOperators operators -> do
    as <- asks randomAgentServers
    (opsConds, srvs) <- withFastStore $ \db -> do
      liftIO $ setServerOperators db operators
      opsConds <- getServerOperators db
      let ops = serverOperators opsConds
          ops' = map Just ops <> [Nothing]
          opDomains = operatorDomains ops
      liftIO $ fmap (opsConds,) . mapM (getServers db as ops' opDomains) =<< getUsers db
    lift $ withAgent' $ \a -> forM_ srvs $ \(auId, (smp', xftp')) -> do
      setProtocolServers a auId smp'
      setProtocolServers a auId xftp'
    pure $ CRServerOperatorConditions opsConds
    where
      getServers :: DB.Connection -> RandomAgentServers -> [Maybe ServerOperator] -> [(Text, ServerOperator)] -> User -> IO (UserId, (NonEmpty (ServerCfg 'PSMP), NonEmpty (ServerCfg 'PXFTP)))
      getServers db as ops opDomains user = do
        smpSrvs <- getProtocolServers db SPSMP user
        xftpSrvs <- getProtocolServers db SPXFTP user
        uss <- groupByOperator (ops, smpSrvs, xftpSrvs)
        pure $ (aUserId user,) $ useServers as opDomains uss
  SetServerOperators operatorsRoles -> do
    ops <- serverOperators <$> withFastStore getServerOperators
    ops' <- mapM (updateOp ops) operatorsRoles
    processChatCommand $ APISetServerOperators ops'
    where
      updateOp :: [ServerOperator] -> ServerOperatorRoles -> CM ServerOperator
      updateOp ops r =
        case find (\ServerOperator {operatorId = DBEntityId opId} -> operatorId' r == opId) ops of
          Just op -> pure op {enabled = enabled' r, smpRoles = smpRoles' r, xftpRoles = xftpRoles' r}
          Nothing -> throwError $ ChatErrorStore $ SEOperatorNotFound $ operatorId' r
  APIGetUserServers userId -> withUserId userId $ \user -> withFastStore $ \db -> do
    CRUserServers user <$> (liftIO . groupByOperator =<< getUserServers db user)
  APISetUserServers userId userServers -> withUserId userId $ \user -> do
    errors <- validateAllUsersServers userId $ L.toList userServers
    unless (null errors) $ throwChatError (CECommandError $ "user servers validation error(s): " <> show errors)
    uss <- withFastStore $ \db -> do
      ts <- liftIO getCurrentTime
      mapM (setUserServers db user ts) userServers
    as <- asks randomAgentServers
    lift $ withAgent' $ \a -> do
      let auId = aUserId user
          opDomains = operatorDomains $ mapMaybe operator' $ L.toList uss
          (smp', xftp') = useServers as opDomains uss
      setProtocolServers a auId smp'
      setProtocolServers a auId xftp'
    ok_
  APIValidateServers userId userServers -> withUserId userId $ \user ->
    CRUserServersValidation user <$> validateAllUsersServers userId userServers
  APIGetUsageConditions -> do
    (usageConditions, acceptedConditions) <- withFastStore $ \db -> do
      usageConditions <- getCurrentUsageConditions db
      acceptedConditions <- liftIO $ getLatestAcceptedConditions db
      pure (usageConditions, acceptedConditions)
    -- TODO if db commit is different from source commit, conditionsText should be nothing in response
    pure
      CRUsageConditions
        { usageConditions,
          conditionsText = usageConditionsText,
          acceptedConditions
        }
  APISetConditionsNotified condId -> do
    currentTs <- liftIO getCurrentTime
    withFastStore' $ \db -> setConditionsNotified db condId currentTs
    ok_
  APIAcceptConditions condId opIds -> withFastStore $ \db -> do
    currentTs <- liftIO getCurrentTime
    acceptConditions db condId opIds currentTs
    CRServerOperatorConditions <$> getServerOperators db
  APISetChatItemTTL userId newTTL_ -> withUserId userId $ \user ->
    checkStoreNotChanged $
      withChatLock "setChatItemTTL" $ do
        case newTTL_ of
          Nothing -> do
            withFastStore' $ \db -> setChatItemTTL db user newTTL_
            lift $ setExpireCIFlag user False
          Just newTTL -> do
            oldTTL <- withFastStore' (`getChatItemTTL` user)
            when (maybe True (newTTL <) oldTTL) $ do
              lift $ setExpireCIFlag user False
              expireChatItems user newTTL True
            withFastStore' $ \db -> setChatItemTTL db user newTTL_
            lift $ startExpireCIThread user
            lift . whenM chatStarted $ setExpireCIFlag user True
        ok user
  SetChatItemTTL newTTL_ -> withUser' $ \User {userId} -> do
    processChatCommand $ APISetChatItemTTL userId newTTL_
  APIGetChatItemTTL userId -> withUserId' userId $ \user -> do
    ttl <- withFastStore' (`getChatItemTTL` user)
    pure $ CRChatItemTTL user ttl
  GetChatItemTTL -> withUser' $ \User {userId} -> do
    processChatCommand $ APIGetChatItemTTL userId
  APISetNetworkConfig cfg -> withUser' $ \_ -> lift (withAgent' (`setNetworkConfig` cfg)) >> ok_
  APIGetNetworkConfig -> withUser' $ \_ ->
    CRNetworkConfig <$> lift getNetworkConfig
  SetNetworkConfig simpleNetCfg -> do
    cfg <- (`updateNetworkConfig` simpleNetCfg) <$> lift getNetworkConfig
    void . processChatCommand $ APISetNetworkConfig cfg
    pure $ CRNetworkConfig cfg
  APISetNetworkInfo info -> lift (withAgent' (`setUserNetworkInfo` info)) >> ok_
  ReconnectAllServers -> withUser' $ \_ -> lift (withAgent' reconnectAllServers) >> ok_
  ReconnectServer userId srv -> withUserId userId $ \user -> do
    lift (withAgent' $ \a -> reconnectSMPServer a (aUserId user) srv)
    ok_
  APISetChatSettings (ChatRef cType chatId) chatSettings -> withUser $ \user -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> do
        ct <- getContact db vr user chatId
        liftIO $ updateContactSettings db user chatId chatSettings
        pure ct
      forM_ (contactConnId ct) $ \connId ->
        withAgent $ \a -> toggleConnectionNtfs a connId (chatHasNtfs chatSettings)
      ok user
    CTGroup -> do
      ms <- withFastStore $ \db -> do
        Group _ ms <- getGroup db vr user chatId
        liftIO $ updateGroupSettings db user chatId chatSettings
        pure ms
      forM_ (filter memberActive ms) $ \m -> forM_ (memberConnId m) $ \connId ->
        withAgent (\a -> toggleConnectionNtfs a connId $ chatHasNtfs chatSettings) `catchChatError` (toView . CRChatError (Just user))
      ok user
    _ -> pure $ chatCmdError (Just user) "not supported"
  APISetMemberSettings gId gMemberId settings -> withUser $ \user -> do
    m <- withFastStore $ \db -> do
      liftIO $ updateGroupMemberSettings db user gId gMemberId settings
      getGroupMember db vr user gId gMemberId
    let ntfOn = showMessages $ memberSettings m
    toggleNtf user m ntfOn
    ok user
  APIContactInfo contactId -> withUser $ \user@User {userId} -> do
    -- [incognito] print user's incognito profile for this contact
    ct@Contact {activeConn} <- withFastStore $ \db -> getContact db vr user contactId
    incognitoProfile <- case activeConn of
      Nothing -> pure Nothing
      Just Connection {customUserProfileId} ->
        forM customUserProfileId $ \profileId -> withFastStore (\db -> getProfileById db userId profileId)
    connectionStats <- mapM (withAgent . flip getConnectionServers) (contactConnId ct)
    pure $ CRContactInfo user ct connectionStats (fmap fromLocalProfile incognitoProfile)
  APIContactQueueInfo contactId -> withUser $ \user -> do
    ct@Contact {activeConn} <- withFastStore $ \db -> getContact db vr user contactId
    case activeConn of
      Just conn -> getConnQueueInfo user conn
      Nothing -> throwChatError $ CEContactNotActive ct
  APIGroupInfo gId -> withUser $ \user -> do
    (g, s) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> liftIO (getGroupSummary db user gId)
    pure $ CRGroupInfo user g s
  APIGroupMemberInfo gId gMemberId -> withUser $ \user -> do
    (g, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db vr user gId gMemberId
    connectionStats <- mapM (withAgent . flip getConnectionServers) (memberConnId m)
    pure $ CRGroupMemberInfo user g m connectionStats
  APIGroupMemberQueueInfo gId gMemberId -> withUser $ \user -> do
    GroupMember {activeConn} <- withFastStore $ \db -> getGroupMember db vr user gId gMemberId
    case activeConn of
      Just conn -> getConnQueueInfo user conn
      Nothing -> throwChatError CEGroupMemberNotActive
  APISwitchContact contactId -> withUser $ \user -> do
    ct <- withFastStore $ \db -> getContact db vr user contactId
    case contactConnId ct of
      Just connId -> do
        connectionStats <- withAgent $ \a -> switchConnectionAsync a "" connId
        pure $ CRContactSwitchStarted user ct connectionStats
      Nothing -> throwChatError $ CEContactNotActive ct
  APISwitchGroupMember gId gMemberId -> withUser $ \user -> do
    (g, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db vr user gId gMemberId
    case memberConnId m of
      Just connId -> do
        connectionStats <- withAgent (\a -> switchConnectionAsync a "" connId)
        pure $ CRGroupMemberSwitchStarted user g m connectionStats
      _ -> throwChatError CEGroupMemberNotActive
  APIAbortSwitchContact contactId -> withUser $ \user -> do
    ct <- withFastStore $ \db -> getContact db vr user contactId
    case contactConnId ct of
      Just connId -> do
        connectionStats <- withAgent $ \a -> abortConnectionSwitch a connId
        pure $ CRContactSwitchAborted user ct connectionStats
      Nothing -> throwChatError $ CEContactNotActive ct
  APIAbortSwitchGroupMember gId gMemberId -> withUser $ \user -> do
    (g, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db vr user gId gMemberId
    case memberConnId m of
      Just connId -> do
        connectionStats <- withAgent $ \a -> abortConnectionSwitch a connId
        pure $ CRGroupMemberSwitchAborted user g m connectionStats
      _ -> throwChatError CEGroupMemberNotActive
  APISyncContactRatchet contactId force -> withUser $ \user -> withContactLock "syncContactRatchet" contactId $ do
    ct <- withFastStore $ \db -> getContact db vr user contactId
    case contactConn ct of
      Just conn@Connection {pqSupport} -> do
        cStats@ConnectionStats {ratchetSyncState = rss} <- withAgent $ \a -> synchronizeRatchet a (aConnId conn) pqSupport force
        createInternalChatItem user (CDDirectSnd ct) (CISndConnEvent $ SCERatchetSync rss Nothing) Nothing
        pure $ CRContactRatchetSyncStarted user ct cStats
      Nothing -> throwChatError $ CEContactNotActive ct
  APISyncGroupMemberRatchet gId gMemberId force -> withUser $ \user -> withGroupLock "syncGroupMemberRatchet" gId $ do
    (g, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db vr user gId gMemberId
    case memberConnId m of
      Just connId -> do
        cStats@ConnectionStats {ratchetSyncState = rss} <- withAgent $ \a -> synchronizeRatchet a connId PQSupportOff force
        createInternalChatItem user (CDGroupSnd g) (CISndConnEvent . SCERatchetSync rss . Just $ groupMemberRef m) Nothing
        pure $ CRGroupMemberRatchetSyncStarted user g m cStats
      _ -> throwChatError CEGroupMemberNotActive
  APIGetContactCode contactId -> withUser $ \user -> do
    ct@Contact {activeConn} <- withFastStore $ \db -> getContact db vr user contactId
    case activeConn of
      Just conn@Connection {connId} -> do
        code <- getConnectionCode $ aConnId conn
        ct' <- case contactSecurityCode ct of
          Just SecurityCode {securityCode}
            | sameVerificationCode code securityCode -> pure ct
            | otherwise -> do
                withFastStore' $ \db -> setConnectionVerified db user connId Nothing
                pure (ct :: Contact) {activeConn = Just $ (conn :: Connection) {connectionCode = Nothing}}
          _ -> pure ct
        pure $ CRContactCode user ct' code
      Nothing -> throwChatError $ CEContactNotActive ct
  APIGetGroupMemberCode gId gMemberId -> withUser $ \user -> do
    (g, m@GroupMember {activeConn}) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db vr user gId gMemberId
    case activeConn of
      Just conn@Connection {connId} -> do
        code <- getConnectionCode $ aConnId conn
        m' <- case memberSecurityCode m of
          Just SecurityCode {securityCode}
            | sameVerificationCode code securityCode -> pure m
            | otherwise -> do
                withFastStore' $ \db -> setConnectionVerified db user connId Nothing
                pure (m :: GroupMember) {activeConn = Just $ (conn :: Connection) {connectionCode = Nothing}}
          _ -> pure m
        pure $ CRGroupMemberCode user g m' code
      _ -> throwChatError CEGroupMemberNotActive
  APIVerifyContact contactId code -> withUser $ \user -> do
    ct@Contact {activeConn} <- withFastStore $ \db -> getContact db vr user contactId
    case activeConn of
      Just conn -> verifyConnectionCode user conn code
      Nothing -> throwChatError $ CEContactNotActive ct
  APIVerifyGroupMember gId gMemberId code -> withUser $ \user -> do
    GroupMember {activeConn} <- withFastStore $ \db -> getGroupMember db vr user gId gMemberId
    case activeConn of
      Just conn -> verifyConnectionCode user conn code
      _ -> throwChatError CEGroupMemberNotActive
  APIEnableContact contactId -> withUser $ \user -> do
    ct@Contact {activeConn} <- withFastStore $ \db -> getContact db vr user contactId
    case activeConn of
      Just conn -> do
        withFastStore' $ \db -> setAuthErrCounter db user conn 0
        ok user
      Nothing -> throwChatError $ CEContactNotActive ct
  APIEnableGroupMember gId gMemberId -> withUser $ \user -> do
    GroupMember {activeConn} <- withFastStore $ \db -> getGroupMember db vr user gId gMemberId
    case activeConn of
      Just conn -> do
        withFastStore' $ \db -> setAuthErrCounter db user conn 0
        ok user
      _ -> throwChatError CEGroupMemberNotActive
  SetShowMessages cName ntfOn -> updateChatSettings cName (\cs -> cs {enableNtfs = ntfOn})
  SetSendReceipts cName rcptsOn_ -> updateChatSettings cName (\cs -> cs {sendRcpts = rcptsOn_})
  SetShowMemberMessages gName mName showMessages -> withUser $ \user -> do
    (gId, mId) <- getGroupAndMemberId user gName mName
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user gId
    m <- withFastStore $ \db -> getGroupMember db vr user gId mId
    let GroupInfo {membership = GroupMember {memberRole = membershipRole}} = gInfo
    -- TODO GRModerator when most users migrate
    when (membershipRole >= GRAdmin) $ throwChatError $ CECantBlockMemberForSelf gInfo m showMessages
    let settings = (memberSettings m) {showMessages}
    processChatCommand $ APISetMemberSettings gId mId settings
  ContactInfo cName -> withContactName cName APIContactInfo
  ShowGroupInfo gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIGroupInfo groupId
  GroupMemberInfo gName mName -> withMemberName gName mName APIGroupMemberInfo
  ContactQueueInfo cName -> withContactName cName APIContactQueueInfo
  GroupMemberQueueInfo gName mName -> withMemberName gName mName APIGroupMemberQueueInfo
  SwitchContact cName -> withContactName cName APISwitchContact
  SwitchGroupMember gName mName -> withMemberName gName mName APISwitchGroupMember
  AbortSwitchContact cName -> withContactName cName APIAbortSwitchContact
  AbortSwitchGroupMember gName mName -> withMemberName gName mName APIAbortSwitchGroupMember
  SyncContactRatchet cName force -> withContactName cName $ \ctId -> APISyncContactRatchet ctId force
  SyncGroupMemberRatchet gName mName force -> withMemberName gName mName $ \gId mId -> APISyncGroupMemberRatchet gId mId force
  GetContactCode cName -> withContactName cName APIGetContactCode
  GetGroupMemberCode gName mName -> withMemberName gName mName APIGetGroupMemberCode
  VerifyContact cName code -> withContactName cName (`APIVerifyContact` code)
  VerifyGroupMember gName mName code -> withMemberName gName mName $ \gId mId -> APIVerifyGroupMember gId mId code
  EnableContact cName -> withContactName cName APIEnableContact
  EnableGroupMember gName mName -> withMemberName gName mName $ \gId mId -> APIEnableGroupMember gId mId
  ChatHelp section -> pure $ CRChatHelp section
  Welcome -> withUser $ pure . CRWelcome
  APIAddContact userId incognito -> withUserId userId $ \user -> procCmd $ do
    -- [incognito] generate profile for connection
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    subMode <- chatReadVar subscriptionMode
    (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing IKPQOn subMode
    -- TODO PQ pass minVersion from the current range
    conn <- withFastStore' $ \db -> createDirectConnection db user connId cReq ConnNew incognitoProfile subMode initialChatVersion PQSupportOn
    pure $ CRInvitation user cReq conn
  AddContact incognito -> withUser $ \User {userId} ->
    processChatCommand $ APIAddContact userId incognito
  APISetConnectionIncognito connId incognito -> withUser $ \user@User {userId} -> do
    conn'_ <- withFastStore $ \db -> do
      conn@PendingContactConnection {pccConnStatus, customUserProfileId} <- getPendingContactConnection db userId connId
      case (pccConnStatus, customUserProfileId, incognito) of
        (ConnNew, Nothing, True) -> liftIO $ do
          incognitoProfile <- generateRandomProfile
          pId <- createIncognitoProfile db user incognitoProfile
          Just <$> updatePCCIncognito db user conn (Just pId)
        (ConnNew, Just pId, False) -> liftIO $ do
          deletePCCIncognitoProfile db user pId
          Just <$> updatePCCIncognito db user conn Nothing
        _ -> pure Nothing
    case conn'_ of
      Just conn' -> pure $ CRConnectionIncognitoUpdated user conn'
      Nothing -> throwChatError CEConnectionIncognitoChangeProhibited
  APIChangeConnectionUser connId newUserId -> withUser $ \user@User {userId} -> do
    conn <- withFastStore $ \db -> getPendingContactConnection db userId connId
    let PendingContactConnection {pccConnStatus, connReqInv} = conn
    case (pccConnStatus, connReqInv) of
      (ConnNew, Just cReqInv) -> do
        newUser <- privateGetUser newUserId
        conn' <- ifM (canKeepLink cReqInv newUser) (updateConnRecord user conn newUser) (recreateConn user conn newUser)
        pure $ CRConnectionUserChanged user conn conn' newUser
      _ -> throwChatError CEConnectionUserChangeProhibited
    where
      canKeepLink :: ConnReqInvitation -> User -> CM Bool
      canKeepLink (CRInvitationUri crData _) newUser = do
        let ConnReqUriData {crSmpQueues = q :| _} = crData
            SMPQueueUri {queueAddress = SMPQueueAddress {smpServer}} = q
        newUserServers <-
          map protoServer' . L.filter (\ServerCfg {enabled} -> enabled)
            <$> getKnownAgentServers SPSMP newUser
        pure $ smpServer `elem` newUserServers
      updateConnRecord user@User {userId} conn@PendingContactConnection {customUserProfileId} newUser = do
        withAgent $ \a -> changeConnectionUser a (aUserId user) (aConnId' conn) (aUserId newUser)
        withFastStore' $ \db -> do
          conn' <- updatePCCUser db userId conn newUserId
          forM_ customUserProfileId $ \profileId ->
            deletePCCIncognitoProfile db user profileId
          pure conn'
      recreateConn user conn@PendingContactConnection {customUserProfileId} newUser = do
        subMode <- chatReadVar subscriptionMode
        (agConnId, cReq) <- withAgent $ \a -> createConnection a (aUserId newUser) True SCMInvitation Nothing IKPQOn subMode
        conn' <- withFastStore' $ \db -> do
          deleteConnectionRecord db user connId
          forM_ customUserProfileId $ \profileId ->
            deletePCCIncognitoProfile db user profileId
          createDirectConnection db newUser agConnId cReq ConnNew Nothing subMode initialChatVersion PQSupportOn
        deleteAgentConnectionAsync user (aConnId' conn)
        pure conn'
  APIConnectPlan userId cReqUri -> withUserId userId $ \user ->
    CRConnectionPlan user <$> connectPlan user cReqUri
  APIConnect userId incognito (Just (ACR SCMInvitation cReq@(CRInvitationUri crData e2e))) -> withUserId userId $ \user -> withInvitationLock "connect" (strEncode cReq) . procCmd $ do
    subMode <- chatReadVar subscriptionMode
    -- [incognito] generate profile to send
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    let profileToSend = userProfileToSend user incognitoProfile Nothing False
    lift (withAgent' $ \a -> connRequestPQSupport a PQSupportOn cReq) >>= \case
      Nothing -> throwChatError CEInvalidConnReq
      -- TODO PQ the error above should be CEIncompatibleConnReqVersion, also the same API should be called in Plan
      Just (agentV, pqSup') -> do
        let chatV = agentToChatVersion agentV
        dm <- encodeConnInfoPQ pqSup' chatV $ XInfo profileToSend
        withFastStore' (\db -> getConnectionEntityByConnReq db vr user cReqs) >>= \case
          Nothing -> joinNewConn chatV dm
          Just (RcvDirectMsgConnection conn@Connection {connId, connStatus, contactConnInitiated} Nothing)
            | connStatus == ConnNew && contactConnInitiated -> joinNewConn chatV dm -- own connection link
            | connStatus == ConnPrepared -> do
                -- retrying join after error
                pcc <- withFastStore $ \db -> getPendingContactConnection db userId connId
                joinPreparedConn (aConnId conn) pcc dm
          Just ent -> throwChatError $ CECommandError $ "connection exists: " <> show (connEntityInfo ent)
        where
          joinNewConn chatV dm = do
            connId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq pqSup'
            pcc <- withFastStore' $ \db -> createDirectConnection db user connId cReq ConnPrepared (incognitoProfile $> profileToSend) subMode chatV pqSup'
            joinPreparedConn connId pcc dm
          joinPreparedConn connId pcc@PendingContactConnection {pccConnId} dm = do
            void $ withAgent $ \a -> joinConnection a (aUserId user) connId True cReq dm pqSup' subMode
            withFastStore' $ \db -> updateConnectionStatusFromTo db pccConnId ConnPrepared ConnJoined
            pure $ CRSentConfirmation user pcc {pccConnStatus = ConnJoined}
          cReqs =
            ( CRInvitationUri crData {crScheme = SSSimplex} e2e,
              CRInvitationUri crData {crScheme = simplexChat} e2e
            )
  APIConnect userId incognito (Just (ACR SCMContact cReq)) -> withUserId userId $ \user -> connectViaContact user incognito cReq
  APIConnect _ _ Nothing -> throwChatError CEInvalidConnReq
  Connect incognito aCReqUri@(Just cReqUri) -> withUser $ \user@User {userId} -> do
    plan <- connectPlan user cReqUri `catchChatError` const (pure $ CPInvitationLink ILPOk)
    unless (connectionPlanProceed plan) $ throwChatError (CEConnectionPlan plan)
    case plan of
      CPContactAddress (CAPContactViaAddress Contact {contactId}) ->
        processChatCommand $ APIConnectContactViaAddress userId incognito contactId
      _ -> processChatCommand $ APIConnect userId incognito aCReqUri
  Connect _ Nothing -> throwChatError CEInvalidConnReq
  APIConnectContactViaAddress userId incognito contactId -> withUserId userId $ \user -> do
    ct@Contact {activeConn, profile = LocalProfile {contactLink}} <- withFastStore $ \db -> getContact db vr user contactId
    when (isJust activeConn) $ throwChatError (CECommandError "contact already has connection")
    case contactLink of
      Just cReq -> connectContactViaAddress user incognito ct cReq
      Nothing -> throwChatError (CECommandError "no address in contact profile")
  ConnectSimplex incognito -> withUser $ \user@User {userId} -> do
    let cReqUri = ACR SCMContact adminContactReq
    plan <- connectPlan user cReqUri `catchChatError` const (pure $ CPInvitationLink ILPOk)
    unless (connectionPlanProceed plan) $ throwChatError (CEConnectionPlan plan)
    case plan of
      CPContactAddress (CAPContactViaAddress Contact {contactId}) ->
        processChatCommand $ APIConnectContactViaAddress userId incognito contactId
      _ -> processChatCommand $ APIConnect userId incognito (Just cReqUri)
  DeleteContact cName cdm -> withContactName cName $ \ctId -> APIDeleteChat (ChatRef CTDirect ctId) cdm
  ClearContact cName -> withContactName cName $ APIClearChat . ChatRef CTDirect
  APIListContacts userId -> withUserId userId $ \user ->
    CRContactsList user <$> withFastStore' (\db -> getUserContacts db vr user)
  ListContacts -> withUser $ \User {userId} ->
    processChatCommand $ APIListContacts userId
  APICreateMyAddress userId -> withUserId userId $ \user -> procCmd $ do
    subMode <- chatReadVar subscriptionMode
    (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMContact Nothing IKPQOn subMode
    withFastStore $ \db -> createUserContactLink db user connId cReq subMode
    pure $ CRUserContactLinkCreated user cReq
  CreateMyAddress -> withUser $ \User {userId} ->
    processChatCommand $ APICreateMyAddress userId
  APIDeleteMyAddress userId -> withUserId userId $ \user@User {profile = p} -> do
    conns <- withFastStore $ \db -> getUserAddressConnections db vr user
    withChatLock "deleteMyAddress" $ do
      deleteAgentConnectionsAsync user $ map aConnId conns
      withFastStore' (`deleteUserAddress` user)
    let p' = (fromLocalProfile p :: Profile) {contactLink = Nothing}
    r <- updateProfile_ user p' $ withFastStore' $ \db -> setUserProfileContactLink db user Nothing
    let user' = case r of
          CRUserProfileUpdated u' _ _ _ -> u'
          _ -> user
    pure $ CRUserContactLinkDeleted user'
  DeleteMyAddress -> withUser $ \User {userId} ->
    processChatCommand $ APIDeleteMyAddress userId
  APIShowMyAddress userId -> withUserId' userId $ \user ->
    CRUserContactLink user <$> withFastStore (`getUserAddress` user)
  ShowMyAddress -> withUser' $ \User {userId} ->
    processChatCommand $ APIShowMyAddress userId
  APISetProfileAddress userId False -> withUserId userId $ \user@User {profile = p} -> do
    let p' = (fromLocalProfile p :: Profile) {contactLink = Nothing}
    updateProfile_ user p' $ withFastStore' $ \db -> setUserProfileContactLink db user Nothing
  APISetProfileAddress userId True -> withUserId userId $ \user@User {profile = p} -> do
    ucl@UserContactLink {connReqContact} <- withFastStore (`getUserAddress` user)
    let p' = (fromLocalProfile p :: Profile) {contactLink = Just connReqContact}
    updateProfile_ user p' $ withFastStore' $ \db -> setUserProfileContactLink db user $ Just ucl
  SetProfileAddress onOff -> withUser $ \User {userId} ->
    processChatCommand $ APISetProfileAddress userId onOff
  APIAddressAutoAccept userId autoAccept_ -> withUserId userId $ \user -> do
    forM_ autoAccept_ $ \AutoAccept {businessAddress, acceptIncognito} ->
      when (businessAddress && acceptIncognito) $ throwChatError $ CECommandError "requests to business address cannot be accepted incognito"
    contactLink <- withFastStore (\db -> updateUserAddressAutoAccept db user autoAccept_)
    pure $ CRUserContactLinkUpdated user contactLink
  AddressAutoAccept autoAccept_ -> withUser $ \User {userId} ->
    processChatCommand $ APIAddressAutoAccept userId autoAccept_
  AcceptContact incognito cName -> withUser $ \User {userId} -> do
    connReqId <- withFastStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand $ APIAcceptContact incognito connReqId
  RejectContact cName -> withUser $ \User {userId} -> do
    connReqId <- withFastStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand $ APIRejectContact connReqId
  ForwardMessage toChatName fromContactName forwardedMsg -> withUser $ \user -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user fromContactName
    forwardedItemId <- withFastStore $ \db -> getDirectChatItemIdByText' db user contactId forwardedMsg
    toChatRef <- getChatRef user toChatName
    processChatCommand $ APIForwardChatItems toChatRef (ChatRef CTDirect contactId) (forwardedItemId :| []) Nothing
  ForwardGroupMessage toChatName fromGroupName fromMemberName_ forwardedMsg -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user fromGroupName
    forwardedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user groupId fromMemberName_ forwardedMsg
    toChatRef <- getChatRef user toChatName
    processChatCommand $ APIForwardChatItems toChatRef (ChatRef CTGroup groupId) (forwardedItemId :| []) Nothing
  ForwardLocalMessage toChatName forwardedMsg -> withUser $ \user -> do
    folderId <- withFastStore (`getUserNoteFolderId` user)
    forwardedItemId <- withFastStore $ \db -> getLocalChatItemIdByText' db user folderId forwardedMsg
    toChatRef <- getChatRef user toChatName
    processChatCommand $ APIForwardChatItems toChatRef (ChatRef CTLocal folderId) (forwardedItemId :| []) Nothing
  SendMessage (ChatName cType name) msg -> withUser $ \user -> do
    let mc = MCText msg
    case cType of
      CTDirect ->
        withFastStore' (\db -> runExceptT $ getContactIdByName db user name) >>= \case
          Right ctId -> do
            let chatRef = ChatRef CTDirect ctId
            processChatCommand $ APISendMessages chatRef False Nothing (ComposedMessage Nothing Nothing mc :| [])
          Left _ ->
            withFastStore' (\db -> runExceptT $ getActiveMembersByName db vr user name) >>= \case
              Right [(gInfo, member)] -> do
                let GroupInfo {localDisplayName = gName} = gInfo
                    GroupMember {localDisplayName = mName} = member
                processChatCommand $ SendMemberContactMessage gName mName msg
              Right (suspectedMember : _) ->
                throwChatError $ CEContactNotFound name (Just suspectedMember)
              _ ->
                throwChatError $ CEContactNotFound name Nothing
      CTGroup -> do
        gId <- withFastStore $ \db -> getGroupIdByName db user name
        let chatRef = ChatRef CTGroup gId
        processChatCommand $ APISendMessages chatRef False Nothing (ComposedMessage Nothing Nothing mc :| [])
      CTLocal
        | name == "" -> do
            folderId <- withFastStore (`getUserNoteFolderId` user)
            processChatCommand $ APICreateChatItems folderId (ComposedMessage Nothing Nothing mc :| [])
        | otherwise -> throwChatError $ CECommandError "not supported"
      _ -> throwChatError $ CECommandError "not supported"
  SendMemberContactMessage gName mName msg -> withUser $ \user -> do
    (gId, mId) <- getGroupAndMemberId user gName mName
    m <- withFastStore $ \db -> getGroupMember db vr user gId mId
    let mc = MCText msg
    case memberContactId m of
      Nothing -> do
        g <- withFastStore $ \db -> getGroupInfo db vr user gId
        unless (groupFeatureMemberAllowed SGFDirectMessages (membership g) g) $ throwChatError $ CECommandError "direct messages not allowed"
        toView $ CRNoMemberContactCreating user g m
        processChatCommand (APICreateMemberContact gId mId) >>= \case
          cr@(CRNewMemberContact _ Contact {contactId} _ _) -> do
            toView cr
            processChatCommand $ APISendMemberContactInvitation contactId (Just mc)
          cr -> pure cr
      Just ctId -> do
        let chatRef = ChatRef CTDirect ctId
        processChatCommand $ APISendMessages chatRef False Nothing (ComposedMessage Nothing Nothing mc :| [])
  SendLiveMessage chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    let mc = MCText msg
    processChatCommand $ APISendMessages chatRef True Nothing (ComposedMessage Nothing Nothing mc :| [])
  SendMessageBroadcast msg -> withUser $ \user -> do
    contacts <- withFastStore' $ \db -> getUserContacts db vr user
    withChatLock "sendMessageBroadcast" . procCmd $ do
      let ctConns_ = L.nonEmpty $ foldr addContactConn [] contacts
      case ctConns_ of
        Nothing -> do
          timestamp <- liftIO getCurrentTime
          pure CRBroadcastSent {user, msgContent = mc, successes = 0, failures = 0, timestamp}
        Just (ctConns :: NonEmpty (Contact, Connection)) -> do
          let idsEvts = L.map ctSndEvent ctConns
          sndMsgs <- lift $ createSndMessages idsEvts
          let msgReqs_ :: NonEmpty (Either ChatError ChatMsgReq) = L.zipWith (fmap . ctMsgReq) ctConns sndMsgs
          (errs, ctSndMsgs :: [(Contact, SndMessage)]) <-
            partitionEithers . L.toList . zipWith3' combineResults ctConns sndMsgs <$> deliverMessagesB msgReqs_
          timestamp <- liftIO getCurrentTime
          lift . void $ withStoreBatch' $ \db -> map (createCI db user timestamp) ctSndMsgs
          pure CRBroadcastSent {user, msgContent = mc, successes = length ctSndMsgs, failures = length errs, timestamp}
    where
      mc = MCText msg
      addContactConn :: Contact -> [(Contact, Connection)] -> [(Contact, Connection)]
      addContactConn ct ctConns = case contactSendConn_ ct of
        Right conn | directOrUsed ct -> (ct, conn) : ctConns
        _ -> ctConns
      ctSndEvent :: (Contact, Connection) -> (ConnOrGroupId, ChatMsgEvent 'Json)
      ctSndEvent (_, Connection {connId}) = (ConnectionId connId, XMsgNew $ MCSimple (extMsgContent mc Nothing))
      ctMsgReq :: (Contact, Connection) -> SndMessage -> ChatMsgReq
      ctMsgReq (_, conn) SndMessage {msgId, msgBody} = (conn, MsgFlags {notification = hasNotification XMsgNew_}, msgBody, [msgId])
      zipWith3' :: (a -> b -> c -> d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
      zipWith3' f ~(x :| xs) ~(y :| ys) ~(z :| zs) = f x y z :| zipWith3 f xs ys zs
      combineResults :: (Contact, Connection) -> Either ChatError SndMessage -> Either ChatError ([Int64], PQEncryption) -> Either ChatError (Contact, SndMessage)
      combineResults (ct, _) (Right msg') (Right _) = Right (ct, msg')
      combineResults _ (Left e) _ = Left e
      combineResults _ _ (Left e) = Left e
      createCI :: DB.Connection -> User -> UTCTime -> (Contact, SndMessage) -> IO ()
      createCI db user createdAt (ct, sndMsg) =
        void $ createNewSndChatItem db user (CDDirectSnd ct) sndMsg (CISndMsgContent mc) Nothing Nothing Nothing False createdAt
  SendMessageQuote cName (AMsgDirection msgDir) quotedMsg msg -> withUser $ \user@User {userId} -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user cName
    quotedItemId <- withFastStore $ \db -> getDirectChatItemIdByText db userId contactId msgDir quotedMsg
    let mc = MCText msg
    processChatCommand $ APISendMessages (ChatRef CTDirect contactId) False Nothing (ComposedMessage Nothing (Just quotedItemId) mc :| [])
  DeleteMessage chatName deletedMsg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    deletedItemId <- getSentChatItemIdByText user chatRef deletedMsg
    processChatCommand $ APIDeleteChatItem chatRef (deletedItemId :| []) CIDMBroadcast
  DeleteMemberMessage gName mName deletedMsg -> withUser $ \user -> do
    gId <- withFastStore $ \db -> getGroupIdByName db user gName
    deletedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user gId (Just mName) deletedMsg
    processChatCommand $ APIDeleteMemberChatItem gId (deletedItemId :| [])
  EditMessage chatName editedMsg msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    editedItemId <- getSentChatItemIdByText user chatRef editedMsg
    let mc = MCText msg
    processChatCommand $ APIUpdateChatItem chatRef editedItemId False mc
  UpdateLiveMessage chatName chatItemId live msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    let mc = MCText msg
    processChatCommand $ APIUpdateChatItem chatRef chatItemId live mc
  ReactToMessage add reaction chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatItemId <- getChatItemIdByText user chatRef msg
    processChatCommand $ APIChatItemReaction chatRef chatItemId add reaction
  APINewGroup userId incognito gProfile@GroupProfile {displayName} -> withUserId userId $ \user -> do
    checkValidName displayName
    gVar <- asks random
    -- [incognito] generate incognito profile for group membership
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    gInfo <- withFastStore $ \db -> createNewGroup db vr gVar user gProfile incognitoProfile
    let cd = CDGroupSnd gInfo
    createInternalChatItem user cd (CISndGroupE2EEInfo E2EInfo {pqEnabled = PQEncOff}) Nothing
    createGroupFeatureItems user cd CISndGroupFeature gInfo
    pure $ CRGroupCreated user gInfo
  NewGroup incognito gProfile -> withUser $ \User {userId} ->
    processChatCommand $ APINewGroup userId incognito gProfile
  APIAddMember groupId contactId memRole -> withUser $ \user -> withGroupLock "addMember" groupId $ do
    -- TODO for large groups: no need to load all members to determine if contact is a member
    (group, contact) <- withFastStore $ \db -> (,) <$> getGroup db vr user groupId <*> getContact db vr user contactId
    assertDirectAllowed user MDSnd contact XGrpInv_
    let Group gInfo members = group
        Contact {localDisplayName = cName} = contact
    assertUserGroupRole gInfo $ max GRAdmin memRole
    -- [incognito] forbid to invite contact to whom user is connected incognito
    when (contactConnIncognito contact) $ throwChatError CEContactIncognitoCantInvite
    -- [incognito] forbid to invite contacts if user joined the group using an incognito profile
    when (incognitoMembership gInfo) $ throwChatError CEGroupIncognitoCantInvite
    let sendInvitation = sendGrpInvitation user contact gInfo
    case contactMember contact members of
      Nothing -> do
        gVar <- asks random
        subMode <- chatReadVar subscriptionMode
        (agentConnId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing IKPQOff subMode
        member <- withFastStore $ \db -> createNewContactMember db gVar user gInfo contact memRole agentConnId cReq subMode
        sendInvitation member cReq
        pure $ CRSentGroupInvitation user gInfo contact member
      Just member@GroupMember {groupMemberId, memberStatus, memberRole = mRole}
        | memberStatus == GSMemInvited -> do
            unless (mRole == memRole) $ withFastStore' $ \db -> updateGroupMemberRole db user member memRole
            withFastStore' (\db -> getMemberInvitation db user groupMemberId) >>= \case
              Just cReq -> do
                sendInvitation member {memberRole = memRole} cReq
                pure $ CRSentGroupInvitation user gInfo contact member {memberRole = memRole}
              Nothing -> throwChatError $ CEGroupCantResendInvitation gInfo cName
        | otherwise -> throwChatError $ CEGroupDuplicateMember cName
  APIJoinGroup groupId -> withUser $ \user@User {userId} -> do
    withGroupLock "joinGroup" groupId . procCmd $ do
      (invitation, ct) <- withFastStore $ \db -> do
        inv@ReceivedGroupInvitation {fromMember} <- getGroupInvitation db vr user groupId
        (inv,) <$> getContactViaMember db vr user fromMember
      let ReceivedGroupInvitation {fromMember, connRequest, groupInfo = g@GroupInfo {membership}} = invitation
          GroupMember {memberId = membershipMemId} = membership
          Contact {activeConn} = ct
      case activeConn of
        Just Connection {peerChatVRange} -> do
          subMode <- chatReadVar subscriptionMode
          dm <- encodeConnInfo $ XGrpAcpt membershipMemId
          agentConnId <- case memberConn fromMember of
            Nothing -> do
              agentConnId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True connRequest PQSupportOff
              let chatV = vr `peerConnChatVersion` peerChatVRange
              void $ withFastStore' $ \db -> createMemberConnection db userId fromMember agentConnId chatV peerChatVRange subMode
              pure agentConnId
            Just conn -> pure $ aConnId conn
          withFastStore' $ \db -> do
            updateGroupMemberStatus db userId fromMember GSMemAccepted
            updateGroupMemberStatus db userId membership GSMemAccepted
          void (withAgent $ \a -> joinConnection a (aUserId user) agentConnId True connRequest dm PQSupportOff subMode)
            `catchChatError` \e -> do
              withFastStore' $ \db -> do
                updateGroupMemberStatus db userId fromMember GSMemInvited
                updateGroupMemberStatus db userId membership GSMemInvited
              throwError e
          updateCIGroupInvitationStatus user g CIGISAccepted `catchChatError` (toView . CRChatError (Just user))
          pure $ CRUserAcceptedGroupSent user g {membership = membership {memberStatus = GSMemAccepted}} Nothing
        Nothing -> throwChatError $ CEContactNotActive ct
  APIMemberRole groupId memberId memRole -> withUser $ \user -> do
    Group gInfo@GroupInfo {membership} members <- withFastStore $ \db -> getGroup db vr user groupId
    if memberId == groupMemberId' membership
      then changeMemberRole user gInfo members membership $ SGEUserRole memRole
      else case find ((== memberId) . groupMemberId') members of
        Just m -> changeMemberRole user gInfo members m $ SGEMemberRole memberId (fromLocalProfile $ memberProfile m) memRole
        _ -> throwChatError CEGroupMemberNotFound
    where
      changeMemberRole user gInfo members m gEvent = do
        let GroupMember {memberId = mId, memberRole = mRole, memberStatus = mStatus, memberContactId, localDisplayName = cName} = m
        assertUserGroupRole gInfo $ maximum ([GRAdmin, mRole, memRole] :: [GroupMemberRole])
        withGroupLock "memberRole" groupId . procCmd $ do
          unless (mRole == memRole) $ do
            withFastStore' $ \db -> updateGroupMemberRole db user m memRole
            case mStatus of
              GSMemInvited -> do
                withFastStore (\db -> (,) <$> mapM (getContact db vr user) memberContactId <*> liftIO (getMemberInvitation db user $ groupMemberId' m)) >>= \case
                  (Just ct, Just cReq) -> sendGrpInvitation user ct gInfo (m :: GroupMember) {memberRole = memRole} cReq
                  _ -> throwChatError $ CEGroupCantResendInvitation gInfo cName
              _ -> do
                msg <- sendGroupMessage user gInfo members $ XGrpMemRole mId memRole
                ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent gEvent)
                toView $ CRNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci]
          pure CRMemberRoleUser {user, groupInfo = gInfo, member = m {memberRole = memRole}, fromRole = mRole, toRole = memRole}
  APIBlockMemberForAll groupId memberId blocked -> withUser $ \user -> do
    Group gInfo@GroupInfo {membership} members <- withFastStore $ \db -> getGroup db vr user groupId
    when (memberId == groupMemberId' membership) $ throwChatError $ CECommandError "can't block/unblock self"
    case splitMember memberId members of
      Nothing -> throwChatError $ CEException "expected to find a single blocked member"
      Just (bm, remainingMembers) -> do
        let GroupMember {memberId = bmMemberId, memberRole = bmRole, memberProfile = bmp} = bm
        -- TODO GRModerator when most users migrate
        assertUserGroupRole gInfo $ max GRAdmin bmRole
        when (blocked == blockedByAdmin bm) $ throwChatError $ CECommandError $ if blocked then "already blocked" else "already unblocked"
        withGroupLock "blockForAll" groupId . procCmd $ do
          let mrs = if blocked then MRSBlocked else MRSUnrestricted
              event = XGrpMemRestrict bmMemberId MemberRestrictions {restriction = mrs}
          msg <- sendGroupMessage' user gInfo remainingMembers event
          let ciContent = CISndGroupEvent $ SGEMemberBlocked memberId (fromLocalProfile bmp) blocked
          ci <- saveSndChatItem user (CDGroupSnd gInfo) msg ciContent
          toView $ CRNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci]
          bm' <- withFastStore $ \db -> do
            liftIO $ updateGroupMemberBlocked db user groupId memberId mrs
            getGroupMember db vr user groupId memberId
          toggleNtf user bm' (not blocked)
          pure CRMemberBlockedForAllUser {user, groupInfo = gInfo, member = bm', blocked}
    where
      splitMember mId ms = case break ((== mId) . groupMemberId') ms of
        (_, []) -> Nothing
        (ms1, bm : ms2) -> Just (bm, ms1 <> ms2)
  APIRemoveMember groupId memberId -> withUser $ \user -> do
    Group gInfo members <- withFastStore $ \db -> getGroup db vr user groupId
    case find ((== memberId) . groupMemberId') members of
      Nothing -> throwChatError CEGroupMemberNotFound
      Just m@GroupMember {memberId = mId, memberRole = mRole, memberStatus = mStatus, memberProfile} -> do
        assertUserGroupRole gInfo $ max GRAdmin mRole
        withGroupLock "removeMember" groupId . procCmd $ do
          case mStatus of
            GSMemInvited -> do
              deleteMemberConnection user m
              withFastStore' $ \db -> deleteGroupMember db user m
            _ -> do
              msg <- sendGroupMessage user gInfo members $ XGrpMemDel mId
              ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent $ SGEMemberDeleted memberId (fromLocalProfile memberProfile))
              toView $ CRNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci]
              deleteMemberConnection' user m True
              -- undeleted "member connected" chat item will prevent deletion of member record
              deleteOrUpdateMemberRecord user m
          pure $ CRUserDeletedMember user gInfo m {memberStatus = GSMemRemoved}
  APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
    Group gInfo@GroupInfo {membership} members <- withFastStore $ \db -> getGroup db vr user groupId
    filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
    withGroupLock "leaveGroup" groupId . procCmd $ do
      cancelFilesInProgress user filesInfo
      msg <- sendGroupMessage' user gInfo members XGrpLeave
      ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent SGEUserLeft)
      toView $ CRNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci]
      -- TODO delete direct connections that were unused
      deleteGroupLinkIfExists user gInfo
      -- member records are not deleted to keep history
      deleteMembersConnections' user members True
      withFastStore' $ \db -> updateGroupMemberStatus db userId membership GSMemLeft
      pure $ CRLeftMemberUser user gInfo {membership = membership {memberStatus = GSMemLeft}}
  APIListMembers groupId -> withUser $ \user ->
    CRGroupMembers user <$> withFastStore (\db -> getGroup db vr user groupId)
  AddMember gName cName memRole -> withUser $ \user -> do
    (groupId, contactId) <- withFastStore $ \db -> (,) <$> getGroupIdByName db user gName <*> getContactIdByName db user cName
    processChatCommand $ APIAddMember groupId contactId memRole
  JoinGroup gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIJoinGroup groupId
  MemberRole gName gMemberName memRole -> withMemberName gName gMemberName $ \gId gMemberId -> APIMemberRole gId gMemberId memRole
  BlockForAll gName gMemberName blocked -> withMemberName gName gMemberName $ \gId gMemberId -> APIBlockMemberForAll gId gMemberId blocked
  RemoveMember gName gMemberName -> withMemberName gName gMemberName APIRemoveMember
  LeaveGroup gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APILeaveGroup groupId
  DeleteGroup gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIDeleteChat (ChatRef CTGroup groupId) (CDMFull True)
  ClearGroup gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIClearChat (ChatRef CTGroup groupId)
  ListMembers gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIListMembers groupId
  APIListGroups userId contactId_ search_ -> withUserId userId $ \user ->
    CRGroupsList user <$> withFastStore' (\db -> getUserGroupsWithSummary db vr user contactId_ search_)
  ListGroups cName_ search_ -> withUser $ \user@User {userId} -> do
    ct_ <- forM cName_ $ \cName -> withFastStore $ \db -> getContactByName db vr user cName
    processChatCommand $ APIListGroups userId (contactId' <$> ct_) search_
  APIUpdateGroupProfile groupId p' -> withUser $ \user -> do
    g <- withFastStore $ \db -> getGroup db vr user groupId
    runUpdateGroupProfile user g p'
  UpdateGroupNames gName GroupProfile {displayName, fullName} ->
    updateGroupProfileByName gName $ \p -> p {displayName, fullName}
  ShowGroupProfile gName -> withUser $ \user ->
    CRGroupProfile user <$> withFastStore (\db -> getGroupInfoByName db vr user gName)
  UpdateGroupDescription gName description ->
    updateGroupProfileByName gName $ \p -> p {description}
  ShowGroupDescription gName -> withUser $ \user ->
    CRGroupDescription user <$> withFastStore (\db -> getGroupInfoByName db vr user gName)
  APICreateGroupLink groupId mRole -> withUser $ \user -> withGroupLock "createGroupLink" groupId $ do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    assertUserGroupRole gInfo GRAdmin
    when (mRole > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole
    groupLinkId <- GroupLinkId <$> drgRandomBytes 16
    subMode <- chatReadVar subscriptionMode
    let crClientData = encodeJSON $ CRDataGroup groupLinkId
    (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMContact (Just crClientData) IKPQOff subMode
    withFastStore $ \db -> createGroupLink db user gInfo connId cReq groupLinkId mRole subMode
    pure $ CRGroupLinkCreated user gInfo cReq mRole
  APIGroupLinkMemberRole groupId mRole' -> withUser $ \user -> withGroupLock "groupLinkMemberRole" groupId $ do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    (groupLinkId, groupLink, mRole) <- withFastStore $ \db -> getGroupLink db user gInfo
    assertUserGroupRole gInfo GRAdmin
    when (mRole' > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole'
    when (mRole' /= mRole) $ withFastStore' $ \db -> setGroupLinkMemberRole db user groupLinkId mRole'
    pure $ CRGroupLink user gInfo groupLink mRole'
  APIDeleteGroupLink groupId -> withUser $ \user -> withGroupLock "deleteGroupLink" groupId $ do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    deleteGroupLink' user gInfo
    pure $ CRGroupLinkDeleted user gInfo
  APIGetGroupLink groupId -> withUser $ \user -> do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    (_, groupLink, mRole) <- withFastStore $ \db -> getGroupLink db user gInfo
    pure $ CRGroupLink user gInfo groupLink mRole
  APICreateMemberContact gId gMemberId -> withUser $ \user -> do
    (g, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db vr user gId gMemberId
    assertUserGroupRole g GRAuthor
    unless (groupFeatureMemberAllowed SGFDirectMessages (membership g) g) $ throwChatError $ CECommandError "direct messages not allowed"
    case memberConn m of
      Just mConn@Connection {peerChatVRange} -> do
        unless (maxVersion peerChatVRange >= groupDirectInvVersion) $ throwChatError CEPeerChatVRangeIncompatible
        when (isJust $ memberContactId m) $ throwChatError $ CECommandError "member contact already exists"
        subMode <- chatReadVar subscriptionMode
        -- TODO PQ should negotitate contact connection with PQSupportOn?
        (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing IKPQOff subMode
        -- [incognito] reuse membership incognito profile
        ct <- withFastStore' $ \db -> createMemberContact db user connId cReq g m mConn subMode
        -- TODO not sure it is correct to set connections status here?
        lift $ setContactNetworkStatus ct NSConnected
        pure $ CRNewMemberContact user ct g m
      _ -> throwChatError CEGroupMemberNotActive
  APISendMemberContactInvitation contactId msgContent_ -> withUser $ \user -> do
    (g@GroupInfo {groupId}, m, ct, cReq) <- withFastStore $ \db -> getMemberContact db vr user contactId
    when (contactGrpInvSent ct) $ throwChatError $ CECommandError "x.grp.direct.inv already sent"
    case memberConn m of
      Just mConn -> do
        let msg = XGrpDirectInv cReq msgContent_
        (sndMsg, _, _) <- sendDirectMemberMessage mConn msg groupId
        withFastStore' $ \db -> setContactGrpInvSent db ct True
        let ct' = ct {contactGrpInvSent = True}
        forM_ msgContent_ $ \mc -> do
          ci <- saveSndChatItem user (CDDirectSnd ct') sndMsg (CISndMsgContent mc)
          toView $ CRNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct') ci]
        pure $ CRNewMemberContactSentInv user ct' g m
      _ -> throwChatError CEGroupMemberNotActive
  CreateGroupLink gName mRole -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APICreateGroupLink groupId mRole
  GroupLinkMemberRole gName mRole -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIGroupLinkMemberRole groupId mRole
  DeleteGroupLink gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIDeleteGroupLink groupId
  ShowGroupLink gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIGetGroupLink groupId
  SendGroupMessageQuote gName cName quotedMsg msg -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    quotedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user groupId cName quotedMsg
    let mc = MCText msg
    processChatCommand $ APISendMessages (ChatRef CTGroup groupId) False Nothing (ComposedMessage Nothing (Just quotedItemId) mc :| [])
  ClearNoteFolder -> withUser $ \user -> do
    folderId <- withFastStore (`getUserNoteFolderId` user)
    processChatCommand $ APIClearChat (ChatRef CTLocal folderId)
  LastChats count_ -> withUser' $ \user -> do
    let count = fromMaybe 5000 count_
    (errs, previews) <- partitionEithers <$> withFastStore' (\db -> getChatPreviews db vr user False (PTLast count) clqNoFilters)
    unless (null errs) $ toView $ CRChatErrors (Just user) (map ChatErrorStore errs)
    pure $ CRChats previews
  LastMessages (Just chatName) count search -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand $ APIGetChat chatRef (CPLast count) search
    pure $ CRChatItems user (Just chatName) (aChatItems . chat $ chatResp)
  LastMessages Nothing count search -> withUser $ \user -> do
    chatItems <- withFastStore $ \db -> getAllChatItems db vr user (CPLast count) search
    pure $ CRChatItems user Nothing chatItems
  LastChatItemId (Just chatName) index -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand (APIGetChat chatRef (CPLast $ index + 1) Nothing)
    pure $ CRChatItemId user (fmap aChatItemId . listToMaybe . aChatItems . chat $ chatResp)
  LastChatItemId Nothing index -> withUser $ \user -> do
    chatItems <- withFastStore $ \db -> getAllChatItems db vr user (CPLast $ index + 1) Nothing
    pure $ CRChatItemId user (fmap aChatItemId . listToMaybe $ chatItems)
  ShowChatItem (Just itemId) -> withUser $ \user -> do
    chatItem <- withFastStore $ \db -> do
      chatRef <- getChatRefViaItemId db user itemId
      getAChatItem db vr user chatRef itemId
    pure $ CRChatItems user Nothing ((: []) chatItem)
  ShowChatItem Nothing -> withUser $ \user -> do
    chatItems <- withFastStore $ \db -> getAllChatItems db vr user (CPLast 1) Nothing
    pure $ CRChatItems user Nothing chatItems
  ShowChatItemInfo chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    itemId <- getChatItemIdByText user chatRef msg
    processChatCommand $ APIGetChatItemInfo chatRef itemId
  ShowLiveItems on -> withUser $ \_ ->
    asks showLiveItems >>= atomically . (`writeTVar` on) >> ok_
  SendFile chatName f -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    case chatRef of
      ChatRef CTLocal folderId -> processChatCommand $ APICreateChatItems folderId (ComposedMessage (Just f) Nothing (MCFile "") :| [])
      _ -> processChatCommand $ APISendMessages chatRef False Nothing (ComposedMessage (Just f) Nothing (MCFile "") :| [])
  SendImage chatName f@(CryptoFile fPath _) -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    filePath <- lift $ toFSFilePath fPath
    unless (any (`isSuffixOf` map toLower fPath) imageExtensions) $ throwChatError CEFileImageType {filePath}
    fileSize <- getFileSize filePath
    unless (fileSize <= maxImageSize) $ throwChatError CEFileImageSize {filePath}
    -- TODO include file description for preview
    processChatCommand $ APISendMessages chatRef False Nothing (ComposedMessage (Just f) Nothing (MCImage "" fixedImagePreview) :| [])
  ForwardFile chatName fileId -> forwardFile chatName fileId SendFile
  ForwardImage chatName fileId -> forwardFile chatName fileId SendImage
  SendFileDescription _chatName _f -> pure $ chatCmdError Nothing "TODO"
  -- TODO to use priority transactions we need a parameter that differentiates manual and automatic acceptance
  ReceiveFile fileId userApprovedRelays encrypted_ rcvInline_ filePath_ -> withUser $ \_ ->
    withFileLock "receiveFile" fileId . procCmd $ do
      (user, ft@RcvFileTransfer {fileStatus}) <- withStore (`getRcvFileTransferById` fileId)
      encrypt <- (`fromMaybe` encrypted_) <$> chatReadVar encryptLocalFiles
      ft' <- (if encrypt && fileStatus == RFSNew then setFileToEncrypt else pure) ft
      receiveFile' user ft' userApprovedRelays rcvInline_ filePath_
  SetFileToReceive fileId userApprovedRelays encrypted_ -> withUser $ \_ -> do
    withFileLock "setFileToReceive" fileId . procCmd $ do
      encrypt <- (`fromMaybe` encrypted_) <$> chatReadVar encryptLocalFiles
      cfArgs <- if encrypt then Just <$> (atomically . CF.randomArgs =<< asks random) else pure Nothing
      withStore' $ \db -> setRcvFileToReceive db fileId userApprovedRelays cfArgs
      ok_
  CancelFile fileId -> withUser $ \user@User {userId} ->
    withFileLock "cancelFile" fileId . procCmd $
      withFastStore (\db -> getFileTransfer db user fileId) >>= \case
        FTSnd ftm@FileTransferMeta {xftpSndFile, cancelled} fts
          | cancelled -> throwChatError $ CEFileCancel fileId "file already cancelled"
          | not (null fts) && all fileCancelledOrCompleteSMP fts ->
              throwChatError $ CEFileCancel fileId "file transfer is complete"
          | otherwise -> do
              fileAgentConnIds <- cancelSndFile user ftm fts True
              deleteAgentConnectionsAsync user fileAgentConnIds
              withFastStore (\db -> liftIO $ lookupChatRefByFileId db user fileId) >>= \case
                Nothing -> pure ()
                Just (ChatRef CTDirect contactId) -> do
                  (contact, sharedMsgId) <- withFastStore $ \db -> (,) <$> getContact db vr user contactId <*> getSharedMsgIdByFileId db userId fileId
                  void . sendDirectContactMessage user contact $ XFileCancel sharedMsgId
                Just (ChatRef CTGroup groupId) -> do
                  (Group gInfo ms, sharedMsgId) <- withFastStore $ \db -> (,) <$> getGroup db vr user groupId <*> getSharedMsgIdByFileId db userId fileId
                  void . sendGroupMessage user gInfo ms $ XFileCancel sharedMsgId
                Just _ -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
              ci <- withFastStore $ \db -> lookupChatItemByFileId db vr user fileId
              pure $ CRSndFileCancelled user ci ftm fts
          where
            fileCancelledOrCompleteSMP SndFileTransfer {fileStatus = s} =
              s == FSCancelled || (s == FSComplete && isNothing xftpSndFile)
        FTRcv ftr@RcvFileTransfer {cancelled, fileStatus, xftpRcvFile}
          | cancelled -> throwChatError $ CEFileCancel fileId "file already cancelled"
          | rcvFileComplete fileStatus -> throwChatError $ CEFileCancel fileId "file transfer is complete"
          | otherwise -> case xftpRcvFile of
              Nothing -> do
                cancelRcvFileTransfer user ftr >>= mapM_ (deleteAgentConnectionAsync user)
                ci <- withFastStore $ \db -> lookupChatItemByFileId db vr user fileId
                pure $ CRRcvFileCancelled user ci ftr
              Just XFTPRcvFile {agentRcvFileId} -> do
                forM_ (liveRcvFileTransferPath ftr) $ \filePath -> do
                  fsFilePath <- lift $ toFSFilePath filePath
                  liftIO $ removeFile fsFilePath `catchAll_` pure ()
                lift . forM_ agentRcvFileId $ \(AgentRcvFileId aFileId) ->
                  withAgent' (`xftpDeleteRcvFile` aFileId)
                aci_ <- resetRcvCIFileStatus user fileId CIFSRcvInvitation
                pure $ CRRcvFileCancelled user aci_ ftr
  FileStatus fileId -> withUser $ \user -> do
    withFastStore (\db -> lookupChatItemByFileId db vr user fileId) >>= \case
      Nothing -> do
        fileStatus <- withFastStore $ \db -> getFileTransferProgress db user fileId
        pure $ CRFileTransferStatus user fileStatus
      Just ci@(AChatItem _ _ _ ChatItem {file}) -> case file of
        Just CIFile {fileProtocol = FPLocal} ->
          throwChatError $ CECommandError "not supported for local files"
        Just CIFile {fileProtocol = FPXFTP} ->
          pure $ CRFileTransferStatusXFTP user ci
        _ -> do
          fileStatus <- withFastStore $ \db -> getFileTransferProgress db user fileId
          pure $ CRFileTransferStatus user fileStatus
  ShowProfile -> withUser $ \user@User {profile} -> pure $ CRUserProfile user (fromLocalProfile profile)
  UpdateProfile displayName fullName -> withUser $ \user@User {profile} -> do
    let p = (fromLocalProfile profile :: Profile) {displayName = displayName, fullName = fullName}
    updateProfile user p
  UpdateProfileImage image -> withUser $ \user@User {profile} -> do
    let p = (fromLocalProfile profile :: Profile) {image}
    updateProfile user p
  ShowProfileImage -> withUser $ \user@User {profile} -> pure $ CRUserProfileImage user $ fromLocalProfile profile
  SetUserFeature (ACF f) allowed -> withUser $ \user@User {profile} -> do
    let p = (fromLocalProfile profile :: Profile) {preferences = Just . setPreference f (Just allowed) $ preferences' user}
    updateProfile user p
  SetContactFeature (ACF f) cName allowed_ -> withUser $ \user -> do
    ct@Contact {userPreferences} <- withFastStore $ \db -> getContactByName db vr user cName
    let prefs' = setPreference f allowed_ $ Just userPreferences
    updateContactPrefs user ct prefs'
  SetGroupFeature (AGFNR f) gName enabled ->
    updateGroupProfileByName gName $ \p ->
      p {groupPreferences = Just . setGroupPreference f enabled $ groupPreferences p}
  SetGroupFeatureRole (AGFR f) gName enabled role ->
    updateGroupProfileByName gName $ \p ->
      p {groupPreferences = Just . setGroupPreferenceRole f enabled role $ groupPreferences p}
  SetUserTimedMessages onOff -> withUser $ \user@User {profile} -> do
    let allowed = if onOff then FAYes else FANo
        pref = TimedMessagesPreference allowed Nothing
        p = (fromLocalProfile profile :: Profile) {preferences = Just . setPreference' SCFTimedMessages (Just pref) $ preferences' user}
    updateProfile user p
  SetContactTimedMessages cName timedMessagesEnabled_ -> withUser $ \user -> do
    ct@Contact {userPreferences = userPreferences@Preferences {timedMessages}} <- withFastStore $ \db -> getContactByName db vr user cName
    let currentTTL = timedMessages >>= \TimedMessagesPreference {ttl} -> ttl
        pref_ = tmeToPref currentTTL <$> timedMessagesEnabled_
        prefs' = setPreference' SCFTimedMessages pref_ $ Just userPreferences
    updateContactPrefs user ct prefs'
  SetGroupTimedMessages gName ttl_ -> do
    let pref = uncurry TimedMessagesGroupPreference $ maybe (FEOff, Just 86400) (\ttl -> (FEOn, Just ttl)) ttl_
    updateGroupProfileByName gName $ \p ->
      p {groupPreferences = Just . setGroupPreference' SGFTimedMessages pref $ groupPreferences p}
  SetLocalDeviceName name -> chatWriteVar localDeviceName name >> ok_
  ListRemoteHosts -> CRRemoteHostList <$> listRemoteHosts
  SwitchRemoteHost rh_ -> CRCurrentRemoteHost <$> switchRemoteHost rh_
  StartRemoteHost rh_ ca_ bp_ -> do
    (localAddrs, remoteHost_, inv@RCSignedInvitation {invitation = RCInvitation {port}}) <- startRemoteHost rh_ ca_ bp_
    pure CRRemoteHostStarted {remoteHost_, invitation = decodeLatin1 $ strEncode inv, ctrlPort = show port, localAddrs}
  StopRemoteHost rh_ -> closeRemoteHost rh_ >> ok_
  DeleteRemoteHost rh -> deleteRemoteHost rh >> ok_
  StoreRemoteFile rh encrypted_ localPath -> CRRemoteFileStored rh <$> storeRemoteFile rh encrypted_ localPath
  GetRemoteFile rh rf -> getRemoteFile rh rf >> ok_
  ConnectRemoteCtrl inv -> withUser_ $ do
    (remoteCtrl_, ctrlAppInfo) <- connectRemoteCtrlURI inv
    pure CRRemoteCtrlConnecting {remoteCtrl_, ctrlAppInfo, appVersion = currentAppVersion}
  FindKnownRemoteCtrl -> withUser_ $ findKnownRemoteCtrl >> ok_
  ConfirmRemoteCtrl rcId -> withUser_ $ do
    (rc, ctrlAppInfo) <- confirmRemoteCtrl rcId
    pure CRRemoteCtrlConnecting {remoteCtrl_ = Just rc, ctrlAppInfo, appVersion = currentAppVersion}
  VerifyRemoteCtrlSession sessId -> withUser_ $ CRRemoteCtrlConnected <$> verifyRemoteCtrlSession (execChatCommand Nothing) sessId
  StopRemoteCtrl -> withUser_ $ stopRemoteCtrl >> ok_
  ListRemoteCtrls -> withUser_ $ CRRemoteCtrlList <$> listRemoteCtrls
  DeleteRemoteCtrl rc -> withUser_ $ deleteRemoteCtrl rc >> ok_
  APIUploadStandaloneFile userId file@CryptoFile {filePath} -> withUserId userId $ \user -> do
    fsFilePath <- lift $ toFSFilePath filePath
    fileSize <- liftIO $ CF.getFileContentsSize file {filePath = fsFilePath}
    when (fileSize > toInteger maxFileSizeHard) $ throwChatError $ CEFileSize filePath
    (_, _, fileTransferMeta) <- xftpSndFileTransfer_ user file fileSize 1 Nothing
    pure CRSndStandaloneFileCreated {user, fileTransferMeta}
  APIStandaloneFileInfo FileDescriptionURI {clientData} -> pure . CRStandaloneFileInfo $ clientData >>= J.decodeStrict . encodeUtf8
  APIDownloadStandaloneFile userId uri file -> withUserId userId $ \user -> do
    ft <- receiveViaURI user uri file
    pure $ CRRcvStandaloneFileCreated user ft
  QuitChat -> liftIO exitSuccess
  ShowVersion -> do
    -- simplexmqCommitQ makes iOS builds crash m(
    let versionInfo = coreVersionInfo ""
    chatMigrations <- map upMigration <$> withFastStore' (Migrations.getCurrent . DB.conn)
    agentMigrations <- withAgent getAgentMigrations
    pure $ CRVersionInfo {versionInfo, chatMigrations, agentMigrations}
  DebugLocks -> lift $ do
    chatLockName <- atomically . tryReadTMVar =<< asks chatLock
    chatEntityLocks <- getLocks =<< asks entityLocks
    agentLocks <- withAgent' debugAgentLocks
    pure CRDebugLocks {chatLockName, chatEntityLocks, agentLocks}
    where
      getLocks ls = atomically $ M.mapKeys enityLockString . M.mapMaybe id <$> (mapM tryReadTMVar =<< readTVar ls)
      enityLockString cle = case cle of
        CLInvitation bs -> "Invitation " <> B.unpack bs
        CLConnection connId -> "Connection " <> show connId
        CLContact ctId -> "Contact " <> show ctId
        CLGroup gId -> "Group " <> show gId
        CLUserContact ucId -> "UserContact " <> show ucId
        CLFile fId -> "File " <> show fId
  DebugEvent event -> toView event >> ok_
  GetAgentSubsTotal userId -> withUserId userId $ \user -> do
    users <- withStore' $ \db -> getUsers db
    let userIds = map aUserId $ filter (\u -> isNothing (viewPwdHash u) || aUserId u == aUserId user) users
    (subsTotal, hasSession) <- lift $ withAgent' $ \a -> getAgentSubsTotal a userIds
    pure $ CRAgentSubsTotal user subsTotal hasSession
  GetAgentServersSummary userId -> withUserId userId $ \user -> do
    agentServersSummary <- lift $ withAgent' getAgentServersSummary
    withStore' $ \db -> do
      users <- getUsers db
      smpServers <- getServers db user SPSMP
      xftpServers <- getServers db user SPXFTP
      let presentedServersSummary = toPresentedServersSummary agentServersSummary users user smpServers xftpServers _defaultNtfServers
      pure $ CRAgentServersSummary user presentedServersSummary
    where
      getServers :: ProtocolTypeI p => DB.Connection -> User -> SProtocolType p -> IO [ProtocolServer p]
      getServers db user p = map (\UserServer {server} -> protoServer server) <$> getProtocolServers db p user
  ResetAgentServersStats -> withAgent resetAgentServersStats >> ok_
  GetAgentWorkers -> lift $ CRAgentWorkersSummary <$> withAgent' getAgentWorkersSummary
  GetAgentWorkersDetails -> lift $ CRAgentWorkersDetails <$> withAgent' getAgentWorkersDetails
  GetAgentSubs -> lift $ summary <$> withAgent' getAgentSubscriptions
    where
      summary SubscriptionsInfo {activeSubscriptions, pendingSubscriptions, removedSubscriptions} =
        CRAgentSubs
          { activeSubs = foldl' countSubs M.empty activeSubscriptions,
            pendingSubs = foldl' countSubs M.empty pendingSubscriptions,
            removedSubs = foldl' accSubErrors M.empty removedSubscriptions
          }
        where
          countSubs m SubInfo {server} = M.alter (Just . maybe 1 (+ 1)) server m
          accSubErrors m = \case
            SubInfo {server, subError = Just e} -> M.alter (Just . maybe [e] (e :)) server m
            _ -> m
  GetAgentSubsDetails -> lift $ CRAgentSubsDetails <$> withAgent' getAgentSubscriptions
  GetAgentQueuesInfo -> lift $ CRAgentQueuesInfo <$> withAgent' getAgentQueuesInfo
  -- CustomChatCommand is unsupported, it can be processed in preCmdHook
  -- in a modified CLI app or core - the hook should return Either ChatResponse ChatCommand
  CustomChatCommand _cmd -> withUser $ \user -> pure $ chatCmdError (Just user) "not supported"
  where
    -- below code would make command responses asynchronous where they can be slow
    -- in View.hs `r'` should be defined as `id` in this case
    -- procCmd :: m ChatResponse -> m ChatResponse
    -- procCmd action = do
    --   ChatController {chatLock = l, smpAgent = a, outputQ = q, random = gVar} <- ask
    --   corrId <- liftIO $ SMP.CorrId <$> randomBytes gVar 8
    --   void . forkIO $
    --     withAgentLock a . withLock l name $
    --       (atomically . writeTBQueue q) . (Just corrId,) =<< (action `catchChatError` (pure . CRChatError))
    --   pure $ CRCmdAccepted corrId
    -- use function below to make commands "synchronous"
    procCmd :: CM ChatResponse -> CM ChatResponse
    procCmd = id
    ok_ = pure $ CRCmdOk Nothing
    ok = pure . CRCmdOk . Just
    getChatRef :: User -> ChatName -> CM ChatRef
    getChatRef user (ChatName cType name) =
      ChatRef cType <$> case cType of
        CTDirect -> withFastStore $ \db -> getContactIdByName db user name
        CTGroup -> withFastStore $ \db -> getGroupIdByName db user name
        CTLocal
          | name == "" -> withFastStore (`getUserNoteFolderId` user)
          | otherwise -> throwChatError $ CECommandError "not supported"
        _ -> throwChatError $ CECommandError "not supported"
    checkChatStopped :: CM ChatResponse -> CM ChatResponse
    checkChatStopped a = asks agentAsync >>= readTVarIO >>= maybe a (const $ throwChatError CEChatNotStopped)
    setStoreChanged :: CM ()
    setStoreChanged = asks chatStoreChanged >>= atomically . (`writeTVar` True)
    withStoreChanged :: CM () -> CM ChatResponse
    withStoreChanged a = checkChatStopped $ a >> setStoreChanged >> ok_
    checkStoreNotChanged :: CM ChatResponse -> CM ChatResponse
    checkStoreNotChanged = ifM (asks chatStoreChanged >>= readTVarIO) (throwChatError CEChatStoreChanged)
    withUserName :: UserName -> (UserId -> ChatCommand) -> CM ChatResponse
    withUserName uName cmd = withFastStore (`getUserIdByName` uName) >>= processChatCommand . cmd
    withContactName :: ContactName -> (ContactId -> ChatCommand) -> CM ChatResponse
    withContactName cName cmd = withUser $ \user ->
      withFastStore (\db -> getContactIdByName db user cName) >>= processChatCommand . cmd
    withMemberName :: GroupName -> ContactName -> (GroupId -> GroupMemberId -> ChatCommand) -> CM ChatResponse
    withMemberName gName mName cmd = withUser $ \user ->
      getGroupAndMemberId user gName mName >>= processChatCommand . uncurry cmd
    getConnectionCode :: ConnId -> CM Text
    getConnectionCode connId = verificationCode <$> withAgent (`getConnectionRatchetAdHash` connId)
    verifyConnectionCode :: User -> Connection -> Maybe Text -> CM ChatResponse
    verifyConnectionCode user conn@Connection {connId} (Just code) = do
      code' <- getConnectionCode $ aConnId conn
      let verified = sameVerificationCode code code'
      when verified . withFastStore' $ \db -> setConnectionVerified db user connId $ Just code'
      pure $ CRConnectionVerified user verified code'
    verifyConnectionCode user conn@Connection {connId} _ = do
      code' <- getConnectionCode $ aConnId conn
      withFastStore' $ \db -> setConnectionVerified db user connId Nothing
      pure $ CRConnectionVerified user False code'
    getSentChatItemIdByText :: User -> ChatRef -> Text -> CM Int64
    getSentChatItemIdByText user@User {userId, localDisplayName} (ChatRef cType cId) msg = case cType of
      CTDirect -> withFastStore $ \db -> getDirectChatItemIdByText db userId cId SMDSnd msg
      CTGroup -> withFastStore $ \db -> getGroupChatItemIdByText db user cId (Just localDisplayName) msg
      CTLocal -> withFastStore $ \db -> getLocalChatItemIdByText db user cId SMDSnd msg
      _ -> throwChatError $ CECommandError "not supported"
    getChatItemIdByText :: User -> ChatRef -> Text -> CM Int64
    getChatItemIdByText user (ChatRef cType cId) msg = case cType of
      CTDirect -> withFastStore $ \db -> getDirectChatItemIdByText' db user cId msg
      CTGroup -> withFastStore $ \db -> getGroupChatItemIdByText' db user cId msg
      CTLocal -> withFastStore $ \db -> getLocalChatItemIdByText' db user cId msg
      _ -> throwChatError $ CECommandError "not supported"
    connectViaContact :: User -> IncognitoEnabled -> ConnectionRequestUri 'CMContact -> CM ChatResponse
    connectViaContact user@User {userId} incognito cReq@(CRContactUri ConnReqUriData {crClientData}) = withInvitationLock "connectViaContact" (strEncode cReq) $ do
      let groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
          cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
      case groupLinkId of
        -- contact address
        Nothing ->
          withFastStore' (\db -> getConnReqContactXContactId db vr user cReqHash) >>= \case
            (Just contact, _) -> pure $ CRContactAlreadyExists user contact
            (_, xContactId_) -> procCmd $ do
              let randomXContactId = XContactId <$> drgRandomBytes 16
              xContactId <- maybe randomXContactId pure xContactId_
              connect' Nothing cReqHash xContactId False
        -- group link
        Just gLinkId ->
          withFastStore' (\db -> getConnReqContactXContactId db vr user cReqHash) >>= \case
            (Just _contact, _) -> procCmd $ do
              -- allow repeat contact request
              newXContactId <- XContactId <$> drgRandomBytes 16
              connect' (Just gLinkId) cReqHash newXContactId True
            (_, xContactId_) -> procCmd $ do
              let randomXContactId = XContactId <$> drgRandomBytes 16
              xContactId <- maybe randomXContactId pure xContactId_
              connect' (Just gLinkId) cReqHash xContactId True
      where
        connect' groupLinkId cReqHash xContactId inGroup = do
          let pqSup = if inGroup then PQSupportOff else PQSupportOn
          (connId, chatV) <- prepareContact user cReq pqSup
          -- [incognito] generate profile to send
          incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
          subMode <- chatReadVar subscriptionMode
          conn@PendingContactConnection {pccConnId} <- withFastStore' $ \db -> createConnReqConnection db userId connId cReqHash xContactId incognitoProfile groupLinkId subMode chatV pqSup
          joinContact user pccConnId connId cReq incognitoProfile xContactId inGroup pqSup chatV
          pure $ CRSentInvitation user conn incognitoProfile
    connectContactViaAddress :: User -> IncognitoEnabled -> Contact -> ConnectionRequestUri 'CMContact -> CM ChatResponse
    connectContactViaAddress user incognito ct cReq =
      withInvitationLock "connectContactViaAddress" (strEncode cReq) $ do
        newXContactId <- XContactId <$> drgRandomBytes 16
        let pqSup = PQSupportOn
        (connId, chatV) <- prepareContact user cReq pqSup
        let cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
        -- [incognito] generate profile to send
        incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
        subMode <- chatReadVar subscriptionMode
        (pccConnId, ct') <- withFastStore $ \db -> createAddressContactConnection db vr user ct connId cReqHash newXContactId incognitoProfile subMode chatV pqSup
        joinContact user pccConnId connId cReq incognitoProfile newXContactId False pqSup chatV
        pure $ CRSentInvitationToContact user ct' incognitoProfile
    prepareContact :: User -> ConnectionRequestUri 'CMContact -> PQSupport -> CM (ConnId, VersionChat)
    prepareContact user cReq pqSup = do
      -- 0) toggle disabled - PQSupportOff
      -- 1) toggle enabled, address supports PQ (connRequestPQSupport returns Just True) - PQSupportOn, enable support with compression
      -- 2) toggle enabled, address doesn't support PQ - PQSupportOn but without compression, with version range indicating support
      lift (withAgent' $ \a -> connRequestPQSupport a pqSup cReq) >>= \case
        Nothing -> throwChatError CEInvalidConnReq
        Just (agentV, _) -> do
          let chatV = agentToChatVersion agentV
          connId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq pqSup
          pure (connId, chatV)
    joinContact :: User -> Int64 -> ConnId -> ConnectionRequestUri 'CMContact -> Maybe Profile -> XContactId -> Bool -> PQSupport -> VersionChat -> CM ()
    joinContact user pccConnId connId cReq incognitoProfile xContactId inGroup pqSup chatV = do
      let profileToSend = userProfileToSend user incognitoProfile Nothing inGroup
      dm <- encodeConnInfoPQ pqSup chatV (XContact profileToSend $ Just xContactId)
      subMode <- chatReadVar subscriptionMode
      joinPreparedAgentConnection user pccConnId connId cReq dm pqSup subMode
    joinPreparedAgentConnection :: User -> Int64 -> ConnId -> ConnectionRequestUri m -> ByteString -> PQSupport -> SubscriptionMode -> CM ()
    joinPreparedAgentConnection user pccConnId connId cReq connInfo pqSup subMode = do
      void (withAgent $ \a -> joinConnection a (aUserId user) connId True cReq connInfo pqSup subMode)
        `catchChatError` \e -> do
          withFastStore' $ \db -> deleteConnectionRecord db user pccConnId
          withAgent $ \a -> deleteConnectionAsync a False connId
          throwError e
    contactMember :: Contact -> [GroupMember] -> Maybe GroupMember
    contactMember Contact {contactId} =
      find $ \GroupMember {memberContactId = cId, memberStatus = s} ->
        cId == Just contactId && s /= GSMemRemoved && s /= GSMemLeft
    checkSndFile :: CryptoFile -> CM Integer
    checkSndFile (CryptoFile f cfArgs) = do
      fsFilePath <- lift $ toFSFilePath f
      unlessM (doesFileExist fsFilePath) . throwChatError $ CEFileNotFound f
      fileSize <- liftIO $ CF.getFileContentsSize $ CryptoFile fsFilePath cfArgs
      when (fromInteger fileSize > maxFileSize) $ throwChatError $ CEFileSize f
      pure fileSize
    updateProfile :: User -> Profile -> CM ChatResponse
    updateProfile user p' = updateProfile_ user p' $ withFastStore $ \db -> updateUserProfile db user p'
    updateProfile_ :: User -> Profile -> CM User -> CM ChatResponse
    updateProfile_ user@User {profile = p@LocalProfile {displayName = n}} p'@Profile {displayName = n'} updateUser
      | p' == fromLocalProfile p = pure $ CRUserProfileNoChange user
      | otherwise = do
          when (n /= n') $ checkValidName n'
          -- read contacts before user update to correctly merge preferences
          contacts <- withFastStore' $ \db -> getUserContacts db vr user
          user' <- updateUser
          asks currentUser >>= atomically . (`writeTVar` Just user')
          withChatLock "updateProfile" . procCmd $ do
            let changedCts_ = L.nonEmpty $ foldr (addChangedProfileContact user') [] contacts
            summary <- case changedCts_ of
              Nothing -> pure $ UserProfileUpdateSummary 0 0 []
              Just changedCts -> do
                let idsEvts = L.map ctSndEvent changedCts
                msgReqs_ <- lift $ L.zipWith ctMsgReq changedCts <$> createSndMessages idsEvts
                (errs, cts) <- partitionEithers . L.toList . L.zipWith (second . const) changedCts <$> deliverMessagesB msgReqs_
                unless (null errs) $ toView $ CRChatErrors (Just user) errs
                let changedCts' = filter (\ChangedProfileContact {ct, ct'} -> directOrUsed ct' && mergedPreferences ct' /= mergedPreferences ct) cts
                lift $ createContactsSndFeatureItems user' changedCts'
                pure
                  UserProfileUpdateSummary
                    { updateSuccesses = length cts,
                      updateFailures = length errs,
                      changedContacts = map (\ChangedProfileContact {ct'} -> ct') changedCts'
                    }
            pure $ CRUserProfileUpdated user' (fromLocalProfile p) p' summary
      where
        -- [incognito] filter out contacts with whom user has incognito connections
        addChangedProfileContact :: User -> Contact -> [ChangedProfileContact] -> [ChangedProfileContact]
        addChangedProfileContact user' ct changedCts = case contactSendConn_ ct' of
          Right conn
            | not (connIncognito conn) && mergedProfile' /= mergedProfile ->
                ChangedProfileContact ct ct' mergedProfile' conn : changedCts
          _ -> changedCts
          where
            mergedProfile = userProfileToSend user Nothing (Just ct) False
            ct' = updateMergedPreferences user' ct
            mergedProfile' = userProfileToSend user' Nothing (Just ct') False
        ctSndEvent :: ChangedProfileContact -> (ConnOrGroupId, ChatMsgEvent 'Json)
        ctSndEvent ChangedProfileContact {mergedProfile', conn = Connection {connId}} = (ConnectionId connId, XInfo mergedProfile')
        ctMsgReq :: ChangedProfileContact -> Either ChatError SndMessage -> Either ChatError ChatMsgReq
        ctMsgReq ChangedProfileContact {conn} =
          fmap $ \SndMessage {msgId, msgBody} ->
            (conn, MsgFlags {notification = hasNotification XInfo_}, msgBody, [msgId])
    updateContactPrefs :: User -> Contact -> Preferences -> CM ChatResponse
    updateContactPrefs _ ct@Contact {activeConn = Nothing} _ = throwChatError $ CEContactNotActive ct
    updateContactPrefs user@User {userId} ct@Contact {activeConn = Just Connection {customUserProfileId}, userPreferences = contactUserPrefs} contactUserPrefs'
      | contactUserPrefs == contactUserPrefs' = pure $ CRContactPrefsUpdated user ct ct
      | otherwise = do
          assertDirectAllowed user MDSnd ct XInfo_
          ct' <- withStore' $ \db -> updateContactUserPreferences db user ct contactUserPrefs'
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore $ \db -> getProfileById db userId profileId
          let mergedProfile = userProfileToSend user (fromLocalProfile <$> incognitoProfile) (Just ct) False
              mergedProfile' = userProfileToSend user (fromLocalProfile <$> incognitoProfile) (Just ct') False
          when (mergedProfile' /= mergedProfile) $
            withContactLock "updateProfile" (contactId' ct) $ do
              void (sendDirectContactMessage user ct' $ XInfo mergedProfile') `catchChatError` (toView . CRChatError (Just user))
              lift . when (directOrUsed ct') $ createSndFeatureItems user ct ct'
          pure $ CRContactPrefsUpdated user ct ct'
    runUpdateGroupProfile :: User -> Group -> GroupProfile -> CM ChatResponse
    runUpdateGroupProfile user (Group g@GroupInfo {businessChat, groupProfile = p@GroupProfile {displayName = n}} ms) p'@GroupProfile {displayName = n'} = do
      assertUserGroupRole g GROwner
      when (n /= n') $ checkValidName n'
      g' <- withStore $ \db -> updateGroupProfile db user g p'
      msg <- case businessChat of
        Just BusinessChatInfo {businessId} -> do
          let (newMs, oldMs) = partition (\m -> maxVersion (memberChatVRange m) >= businessChatPrefsVersion) ms
          -- this is a fallback to send the members with the old version correct profile of the business when preferences change
          unless (null oldMs) $ do
            GroupMember {memberProfile = LocalProfile {displayName, fullName, image}} <-
              withStore $ \db -> getGroupMemberByMemberId db vr user g businessId
            let p'' = p' {displayName, fullName, image} :: GroupProfile
            void $ sendGroupMessage user g' oldMs (XGrpInfo p'')
          let ps' = fromMaybe defaultBusinessGroupPrefs $ groupPreferences p'
          sendGroupMessage user g' newMs $ XGrpPrefs ps'
        Nothing -> sendGroupMessage user g' ms (XGrpInfo p')
      let cd = CDGroupSnd g'
      unless (sameGroupProfileInfo p p') $ do
        ci <- saveSndChatItem user cd msg (CISndGroupEvent $ SGEGroupUpdated p')
        toView $ CRNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat g') ci]
      createGroupFeatureChangedItems user cd CISndGroupFeature g g'
      pure $ CRGroupUpdated user g g' Nothing
    checkValidName :: GroupName -> CM ()
    checkValidName displayName = do
      when (T.null displayName) $ throwChatError CEInvalidDisplayName {displayName, validName = ""}
      let validName = T.pack $ mkValidName $ T.unpack displayName
      when (displayName /= validName) $ throwChatError CEInvalidDisplayName {displayName, validName}
    assertUserGroupRole :: GroupInfo -> GroupMemberRole -> CM ()
    assertUserGroupRole g@GroupInfo {membership} requiredRole = do
      let GroupMember {memberRole = membershipMemRole} = membership
      when (membershipMemRole < requiredRole) $ throwChatError $ CEGroupUserRole g requiredRole
      when (memberStatus membership == GSMemInvited) $ throwChatError (CEGroupNotJoined g)
      when (memberRemoved membership) $ throwChatError CEGroupMemberUserRemoved
      unless (memberActive membership) $ throwChatError CEGroupMemberNotActive
    delGroupChatItems :: User -> GroupInfo -> [CChatItem 'CTGroup] -> Maybe GroupMember -> CM ChatResponse
    delGroupChatItems user gInfo items byGroupMember = do
      deletedTs <- liftIO getCurrentTime
      if groupFeatureAllowed SGFFullDelete gInfo
        then deleteGroupCIs user gInfo items True False byGroupMember deletedTs
        else markGroupCIsDeleted user gInfo items True byGroupMember deletedTs
    updateGroupProfileByName :: GroupName -> (GroupProfile -> GroupProfile) -> CM ChatResponse
    updateGroupProfileByName gName update = withUser $ \user -> do
      g@(Group GroupInfo {groupProfile = p} _) <- withStore $ \db ->
        getGroupIdByName db user gName >>= getGroup db vr user
      runUpdateGroupProfile user g $ update p
    withCurrentCall :: ContactId -> (User -> Contact -> Call -> CM (Maybe Call)) -> CM ChatResponse
    withCurrentCall ctId action = do
      (user, ct) <- withStore $ \db -> do
        user <- getUserByContactId db ctId
        (user,) <$> getContact db vr user ctId
      calls <- asks currentCalls
      withContactLock "currentCall" ctId $
        atomically (TM.lookup ctId calls) >>= \case
          Nothing -> throwChatError CENoCurrentCall
          Just call@Call {contactId}
            | ctId == contactId -> do
                call_ <- action user ct call
                case call_ of
                  Just call' -> do
                    unless (isRcvInvitation call') $ withStore' $ \db -> deleteCalls db user ctId
                    atomically $ TM.insert ctId call' calls
                  _ -> do
                    withStore' $ \db -> deleteCalls db user ctId
                    atomically $ TM.delete ctId calls
                ok user
            | otherwise -> throwChatError $ CECallContact contactId
    withServerProtocol :: ProtocolTypeI p => SProtocolType p -> (UserProtocol p => CM a) -> CM a
    withServerProtocol p action = case userProtocol p of
      Just Dict -> action
      _ -> throwChatError $ CEServerProtocol $ AProtocolType p
    validateAllUsersServers :: UserServersClass u => Int64 -> [u] -> CM [UserServersError]
    validateAllUsersServers currUserId userServers = withFastStore $ \db -> do
      users' <- filter (\User {userId} -> userId /= currUserId) <$> liftIO (getUsers db)
      others <- mapM (getUserOperatorServers db) users'
      pure $ validateUserServers userServers others
      where
        getUserOperatorServers :: DB.Connection -> User -> ExceptT StoreError IO (User, [UserOperatorServers])
        getUserOperatorServers db user = do
          uss <- liftIO . groupByOperator =<< getUserServers db user
          pure (user, map updatedUserSrvs uss)
        updatedUserSrvs uss = uss {operator = updatedOp <$> operator' uss} :: UserOperatorServers
        updatedOp op = fromMaybe op $ find matchingOp $ mapMaybe operator' userServers
          where
            matchingOp op' = operatorId op' == operatorId op
    forwardFile :: ChatName -> FileTransferId -> (ChatName -> CryptoFile -> ChatCommand) -> CM ChatResponse
    forwardFile chatName fileId sendCommand = withUser $ \user -> do
      withStore (\db -> getFileTransfer db user fileId) >>= \case
        FTRcv RcvFileTransfer {fileStatus = RFSComplete RcvFileInfo {filePath}, cryptoArgs} -> forward filePath cryptoArgs
        FTSnd {fileTransferMeta = FileTransferMeta {filePath, xftpSndFile}} -> forward filePath $ xftpSndFile >>= \XFTPSndFile {cryptoArgs} -> cryptoArgs
        _ -> throwChatError CEFileNotReceived {fileId}
      where
        forward path cfArgs = processChatCommand . sendCommand chatName $ CryptoFile path cfArgs
    getGroupAndMemberId :: User -> GroupName -> ContactName -> CM (GroupId, GroupMemberId)
    getGroupAndMemberId user gName groupMemberName =
      withStore $ \db -> do
        groupId <- getGroupIdByName db user gName
        groupMemberId <- getGroupMemberIdByName db user groupId groupMemberName
        pure (groupId, groupMemberId)
    sendGrpInvitation :: User -> Contact -> GroupInfo -> GroupMember -> ConnReqInvitation -> CM ()
    sendGrpInvitation user ct@Contact {contactId, localDisplayName} gInfo@GroupInfo {groupId, groupProfile, membership, businessChat} GroupMember {groupMemberId, memberId, memberRole = memRole} cReq = do
      currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
      let GroupMember {memberRole = userRole, memberId = userMemberId} = membership
          groupInv =
            GroupInvitation
              { fromMember = MemberIdRole userMemberId userRole,
                invitedMember = MemberIdRole memberId memRole,
                connRequest = cReq,
                groupProfile,
                business = businessChat,
                groupLinkId = Nothing,
                groupSize = Just currentMemCount
              }
      (msg, _) <- sendDirectContactMessage user ct $ XGrpInv groupInv
      let content = CISndGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
      timed_ <- contactCITimed ct
      ci <- saveSndChatItem' user (CDDirectSnd ct) msg content Nothing Nothing Nothing timed_ False
      toView $ CRNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]
      forM_ (timed_ >>= timedDeleteAt') $
        startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId' ci)
    drgRandomBytes :: Int -> CM ByteString
    drgRandomBytes n = asks random >>= atomically . C.randomBytes n
    privateGetUser :: UserId -> CM User
    privateGetUser userId =
      tryChatError (withStore (`getUser` userId)) >>= \case
        Left _ -> throwChatError CEUserUnknown
        Right user -> pure user
    validateUserPassword :: User -> User -> Maybe UserPwd -> CM ()
    validateUserPassword = validateUserPassword_ . Just
    validateUserPassword_ :: Maybe User -> User -> Maybe UserPwd -> CM ()
    validateUserPassword_ user_ User {userId = userId', viewPwdHash} viewPwd_ =
      forM_ viewPwdHash $ \pwdHash ->
        let userId_ = (\User {userId} -> userId) <$> user_
            pwdOk = case viewPwd_ of
              Nothing -> userId_ == Just userId'
              Just (UserPwd viewPwd) -> validPassword viewPwd pwdHash
         in unless pwdOk $ throwChatError CEUserUnknown
    validPassword :: Text -> UserPwdHash -> Bool
    validPassword pwd UserPwdHash {hash = B64UrlByteString hash, salt = B64UrlByteString salt} =
      hash == C.sha512Hash (encodeUtf8 pwd <> salt)
    setUserNotifications :: UserId -> Bool -> CM ChatResponse
    setUserNotifications userId' showNtfs = withUser $ \user -> do
      user' <- privateGetUser userId'
      case viewPwdHash user' of
        Just _ -> throwChatError $ CEHiddenUserAlwaysMuted userId'
        _ -> setUserPrivacy user user' {showNtfs}
    setUserPrivacy :: User -> User -> CM ChatResponse
    setUserPrivacy user@User {userId} user'@User {userId = userId'}
      | userId == userId' = do
          asks currentUser >>= atomically . (`writeTVar` Just user')
          withFastStore' (`updateUserPrivacy` user')
          pure $ CRUserPrivacy {user = user', updatedUser = user'}
      | otherwise = do
          withFastStore' (`updateUserPrivacy` user')
          pure $ CRUserPrivacy {user, updatedUser = user'}
    checkDeleteChatUser :: User -> CM ()
    checkDeleteChatUser user@User {userId} = do
      users <- withFastStore' getUsers
      let otherVisible = filter (\User {userId = userId', viewPwdHash} -> userId /= userId' && isNothing viewPwdHash) users
      when (activeUser user && length otherVisible > 0) $ throwChatError (CECantDeleteActiveUser userId)
    deleteChatUser :: User -> Bool -> CM ChatResponse
    deleteChatUser user delSMPQueues = do
      filesInfo <- withFastStore' (`getUserFileInfo` user)
      cancelFilesInProgress user filesInfo
      deleteFilesLocally filesInfo
      withAgent (\a -> deleteUser a (aUserId user) delSMPQueues)
        `catchChatError` \case
          e@(ChatErrorAgent NO_USER _) -> toView $ CRChatError (Just user) e
          e -> throwError e
      withFastStore' (`deleteUserRecord` user)
      when (activeUser user) $ chatWriteVar currentUser Nothing
      ok_
    updateChatSettings :: ChatName -> (ChatSettings -> ChatSettings) -> CM ChatResponse
    updateChatSettings (ChatName cType name) updateSettings = withUser $ \user -> do
      (chatId, chatSettings) <- case cType of
        CTDirect -> withFastStore $ \db -> do
          ctId <- getContactIdByName db user name
          Contact {chatSettings} <- getContact db vr user ctId
          pure (ctId, chatSettings)
        CTGroup ->
          withFastStore $ \db -> do
            gId <- getGroupIdByName db user name
            GroupInfo {chatSettings} <- getGroupInfo db vr user gId
            pure (gId, chatSettings)
        _ -> throwChatError $ CECommandError "not supported"
      processChatCommand $ APISetChatSettings (ChatRef cType chatId) $ updateSettings chatSettings
    connectPlan :: User -> AConnectionRequestUri -> CM ConnectionPlan
    connectPlan user (ACR SCMInvitation (CRInvitationUri crData e2e)) = do
      withFastStore' (\db -> getConnectionEntityByConnReq db vr user cReqSchemas) >>= \case
        Nothing -> pure $ CPInvitationLink ILPOk
        Just (RcvDirectMsgConnection Connection {connStatus = ConnPrepared} Nothing) ->
          pure $ CPInvitationLink ILPOk
        Just (RcvDirectMsgConnection conn ct_) -> do
          let Connection {connStatus, contactConnInitiated} = conn
          if
            | connStatus == ConnNew && contactConnInitiated ->
                pure $ CPInvitationLink ILPOwnLink
            | not (connReady conn) ->
                pure $ CPInvitationLink (ILPConnecting ct_)
            | otherwise -> case ct_ of
                Just ct -> pure $ CPInvitationLink (ILPKnown ct)
                Nothing -> throwChatError $ CEInternalError "ready RcvDirectMsgConnection connection should have associated contact"
        Just _ -> throwChatError $ CECommandError "found connection entity is not RcvDirectMsgConnection"
      where
        cReqSchemas :: (ConnReqInvitation, ConnReqInvitation)
        cReqSchemas =
          ( CRInvitationUri crData {crScheme = SSSimplex} e2e,
            CRInvitationUri crData {crScheme = simplexChat} e2e
          )
    connectPlan user (ACR SCMContact (CRContactUri crData)) = do
      let ConnReqUriData {crClientData} = crData
          groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
      case groupLinkId of
        -- contact address
        Nothing ->
          withFastStore' (\db -> getUserContactLinkByConnReq db user cReqSchemas) >>= \case
            Just _ -> pure $ CPContactAddress CAPOwnLink
            Nothing ->
              withFastStore' (\db -> getContactConnEntityByConnReqHash db vr user cReqHashes) >>= \case
                Nothing ->
                  withFastStore' (\db -> getContactWithoutConnViaAddress db vr user cReqSchemas) >>= \case
                    Nothing -> pure $ CPContactAddress CAPOk
                    Just ct -> pure $ CPContactAddress (CAPContactViaAddress ct)
                Just (RcvDirectMsgConnection _conn Nothing) -> pure $ CPContactAddress CAPConnectingConfirmReconnect
                Just (RcvDirectMsgConnection _ (Just ct))
                  | not (contactReady ct) && contactActive ct -> pure $ CPContactAddress (CAPConnectingProhibit ct)
                  | contactDeleted ct -> pure $ CPContactAddress CAPOk
                  | otherwise -> pure $ CPContactAddress (CAPKnown ct)
                Just (RcvGroupMsgConnection _ gInfo _) -> groupPlan gInfo
                Just _ -> throwChatError $ CECommandError "found connection entity is not RcvDirectMsgConnection or RcvGroupMsgConnection"
        -- group link
        Just _ ->
          withFastStore' (\db -> getGroupInfoByUserContactLinkConnReq db vr user cReqSchemas) >>= \case
            Just g -> pure $ CPGroupLink (GLPOwnLink g)
            Nothing -> do
              connEnt_ <- withFastStore' $ \db -> getContactConnEntityByConnReqHash db vr user cReqHashes
              gInfo_ <- withFastStore' $ \db -> getGroupInfoByGroupLinkHash db vr user cReqHashes
              case (gInfo_, connEnt_) of
                (Nothing, Nothing) -> pure $ CPGroupLink GLPOk
                (Nothing, Just (RcvDirectMsgConnection _conn Nothing)) -> pure $ CPGroupLink GLPConnectingConfirmReconnect
                (Nothing, Just (RcvDirectMsgConnection _ (Just ct)))
                  | not (contactReady ct) && contactActive ct -> pure $ CPGroupLink (GLPConnectingProhibit gInfo_)
                  | otherwise -> pure $ CPGroupLink GLPOk
                (Nothing, Just _) -> throwChatError $ CECommandError "found connection entity is not RcvDirectMsgConnection"
                (Just gInfo, _) -> groupPlan gInfo
      where
        groupPlan gInfo@GroupInfo {membership}
          | not (memberActive membership) && not (memberRemoved membership) =
              pure $ CPGroupLink (GLPConnectingProhibit $ Just gInfo)
          | memberActive membership = pure $ CPGroupLink (GLPKnown gInfo)
          | otherwise = pure $ CPGroupLink GLPOk
        cReqSchemas :: (ConnReqContact, ConnReqContact)
        cReqSchemas =
          ( CRContactUri crData {crScheme = SSSimplex},
            CRContactUri crData {crScheme = simplexChat}
          )
        cReqHashes :: (ConnReqUriHash, ConnReqUriHash)
        cReqHashes = bimap hash hash cReqSchemas
        hash = ConnReqUriHash . C.sha256Hash . strEncode
    updateCIGroupInvitationStatus user GroupInfo {groupId} newStatus = do
      AChatItem _ _ cInfo ChatItem {content, meta = CIMeta {itemId}} <- withFastStore $ \db -> getChatItemByGroupId db vr user groupId
      case (cInfo, content) of
        (DirectChat ct@Contact {contactId}, CIRcvGroupInvitation ciGroupInv@CIGroupInvitation {status} memRole)
          | status == CIGISPending -> do
              let aciContent = ACIContent SMDRcv $ CIRcvGroupInvitation ciGroupInv {status = newStatus} memRole
              timed_ <- contactCITimed ct
              updateDirectChatItemView user ct itemId aciContent False False timed_ Nothing
              forM_ (timed_ >>= timedDeleteAt') $
                startProximateTimedItemThread user (ChatRef CTDirect contactId, itemId)
        _ -> pure () -- prohibited
    assertAllowedContent :: MsgContent -> CM ()
    assertAllowedContent = \case
      MCReport {} -> throwChatError $ CECommandError "sending reports via this API is not supported"
      _ -> pure ()
    assertAllowedContent' :: ComposedMessage -> CM ()
    assertAllowedContent' ComposedMessage {msgContent} = assertAllowedContent msgContent
    sendContactContentMessages :: User -> ContactId -> Bool -> Maybe Int -> NonEmpty ComposeMessageReq -> CM ChatResponse
    sendContactContentMessages user contactId live itemTTL cmrs = do
      assertMultiSendable live cmrs
      ct@Contact {contactUsed} <- withFastStore $ \db -> getContact db vr user contactId
      assertDirectAllowed user MDSnd ct XMsgNew_
      assertVoiceAllowed ct
      unless contactUsed $ withFastStore' $ \db -> updateContactUsed db user ct
      processComposedMessages ct
      where
        assertVoiceAllowed :: Contact -> CM ()
        assertVoiceAllowed ct =
          when (not (featureAllowed SCFVoice forUser ct) && any (\(ComposedMessage {msgContent}, _) -> isVoice msgContent) cmrs) $
            throwChatError (CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFVoice))
        processComposedMessages :: Contact -> CM ChatResponse
        processComposedMessages ct = do
          (fInvs_, ciFiles_) <- L.unzip <$> setupSndFileTransfers
          timed_ <- sndContactCITimed live ct itemTTL
          (msgContainers, quotedItems_) <- L.unzip <$> prepareMsgs (L.zip cmrs fInvs_) timed_
          msgs_ <- sendDirectContactMessages user ct $ L.map XMsgNew msgContainers
          let itemsData = prepareSndItemsData msgs_ cmrs ciFiles_ quotedItems_
          when (length itemsData /= length cmrs) $ logError "sendContactContentMessages: cmrs and itemsData length mismatch"
          r@(_, cis) <- partitionEithers <$> saveSndChatItems user (CDDirectSnd ct) itemsData timed_ live
          processSendErrs user r
          forM_ (timed_ >>= timedDeleteAt') $ \deleteAt ->
            forM_ cis $ \ci ->
              startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId' ci) deleteAt
          pure $ CRNewChatItems user (map (AChatItem SCTDirect SMDSnd (DirectChat ct)) cis)
          where
            setupSndFileTransfers :: CM (NonEmpty (Maybe FileInvitation, Maybe (CIFile 'MDSnd)))
            setupSndFileTransfers =
              forM cmrs $ \(ComposedMessage {fileSource = file_}, _) -> case file_ of
                Just file -> do
                  fileSize <- checkSndFile file
                  (fInv, ciFile) <- xftpSndFileTransfer user file fileSize 1 $ CGContact ct
                  pure (Just fInv, Just ciFile)
                Nothing -> pure (Nothing, Nothing)
            prepareMsgs :: NonEmpty (ComposeMessageReq, Maybe FileInvitation) -> Maybe CITimed -> CM (NonEmpty (MsgContainer, Maybe (CIQuote 'CTDirect)))
            prepareMsgs cmsFileInvs timed_ =
              forM cmsFileInvs $ \((ComposedMessage {quotedItemId, msgContent = mc}, itemForwarded), fInv_) ->
                case (quotedItemId, itemForwarded) of
                  (Nothing, Nothing) -> pure (MCSimple (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
                  (Nothing, Just _) -> pure (MCForward (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
                  (Just qiId, Nothing) -> do
                    CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
                      withFastStore $ \db -> getDirectChatItem db user contactId qiId
                    (origQmc, qd, sent) <- quoteData qci
                    let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Nothing}
                        qmc = quoteContent mc origQmc file
                        quotedItem = CIQuote {chatDir = qd, itemId = Just qiId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
                    pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Just quotedItem)
                  (Just _, Just _) -> throwChatError CEInvalidQuote
              where
                quoteData :: ChatItem c d -> CM (MsgContent, CIQDirection 'CTDirect, Bool)
                quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} = throwChatError CEInvalidQuote
                quoteData ChatItem {content = CISndMsgContent qmc} = pure (qmc, CIQDirectSnd, True)
                quoteData ChatItem {content = CIRcvMsgContent qmc} = pure (qmc, CIQDirectRcv, False)
                quoteData _ = throwChatError CEInvalidQuote
    sendGroupContentMessages :: User -> GroupId -> Bool -> Maybe Int -> NonEmpty ComposeMessageReq -> CM ChatResponse
    sendGroupContentMessages user groupId live itemTTL cmrs = do
      assertMultiSendable live cmrs
      Group gInfo ms <- withFastStore $ \db -> getGroup db vr user groupId
      sendGroupContentMessages_ user gInfo ms live itemTTL cmrs
    sendGroupContentMessages_ :: User -> GroupInfo -> [GroupMember] -> Bool -> Maybe Int -> NonEmpty ComposeMessageReq -> CM ChatResponse
    sendGroupContentMessages_ user gInfo@GroupInfo {groupId, membership} ms live itemTTL cmrs = do
      assertUserGroupRole gInfo GRAuthor
      assertGroupContentAllowed
      processComposedMessages
      where
        assertGroupContentAllowed :: CM ()
        assertGroupContentAllowed =
          case findProhibited (L.toList cmrs) of
            Just f -> throwChatError (CECommandError $ "feature not allowed " <> T.unpack (groupFeatureNameText f))
            Nothing -> pure ()
          where
            findProhibited :: [ComposeMessageReq] -> Maybe GroupFeature
            findProhibited =
              foldr'
                (\(ComposedMessage {fileSource, msgContent = mc}, _) acc -> prohibitedGroupContent gInfo membership mc fileSource <|> acc)
                Nothing
        processComposedMessages :: CM ChatResponse
        processComposedMessages = do
          (fInvs_, ciFiles_) <- L.unzip <$> setupSndFileTransfers (length $ filter memberCurrent ms)
          timed_ <- sndGroupCITimed live gInfo itemTTL
          (msgContainers, quotedItems_) <- L.unzip <$> prepareMsgs (L.zip cmrs fInvs_) timed_
          (msgs_, gsr) <- sendGroupMessages user gInfo ms $ L.map XMsgNew msgContainers
          let itemsData = prepareSndItemsData (L.toList msgs_) cmrs ciFiles_ quotedItems_
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo) itemsData timed_ live
          when (length itemsData /= length cmrs) $ logError "sendGroupContentMessages: cmrs and cis_ length mismatch"
          createMemberSndStatuses cis_ msgs_ gsr
          let r@(_, cis) = partitionEithers cis_
          processSendErrs user r
          forM_ (timed_ >>= timedDeleteAt') $ \deleteAt ->
            forM_ cis $ \ci ->
              startProximateTimedItemThread user (ChatRef CTGroup groupId, chatItemId' ci) deleteAt
          pure $ CRNewChatItems user (map (AChatItem SCTGroup SMDSnd (GroupChat gInfo)) cis)
          where
            setupSndFileTransfers :: Int -> CM (NonEmpty (Maybe FileInvitation, Maybe (CIFile 'MDSnd)))
            setupSndFileTransfers n =
              forM cmrs $ \(ComposedMessage {fileSource = file_}, _) -> case file_ of
                Just file -> do
                  fileSize <- checkSndFile file
                  (fInv, ciFile) <- xftpSndFileTransfer user file fileSize n $ CGGroup gInfo ms
                  pure (Just fInv, Just ciFile)
                Nothing -> pure (Nothing, Nothing)
            prepareMsgs :: NonEmpty (ComposeMessageReq, Maybe FileInvitation) -> Maybe CITimed -> CM (NonEmpty (MsgContainer, Maybe (CIQuote 'CTGroup)))
            prepareMsgs cmsFileInvs timed_ =
              forM cmsFileInvs $ \((ComposedMessage {quotedItemId, msgContent = mc}, itemForwarded), fInv_) ->
                prepareGroupMsg user gInfo mc quotedItemId itemForwarded fInv_ timed_ live
            createMemberSndStatuses ::
              [Either ChatError (ChatItem 'CTGroup 'MDSnd)] ->
              NonEmpty (Either ChatError SndMessage) ->
              GroupSndResult ->
              CM ()
            createMemberSndStatuses cis_ msgs_ GroupSndResult {sentTo, pending, forwarded} = do
              let msgToItem = mapMsgToItem
              withFastStore' $ \db -> do
                forM_ sentTo (processSentTo db msgToItem)
                forM_ forwarded (processForwarded db)
                forM_ pending (processPending db msgToItem)
              where
                mapMsgToItem :: Map MessageId ChatItemId
                mapMsgToItem = foldr' addItem M.empty (zip (L.toList msgs_) cis_)
                  where
                    addItem (Right SndMessage {msgId}, Right ci) m = M.insert msgId (chatItemId' ci) m
                    addItem _ m = m
                processSentTo :: DB.Connection -> Map MessageId ChatItemId -> (GroupMemberId, Either ChatError [MessageId], Either ChatError ([Int64], PQEncryption)) -> IO ()
                processSentTo db msgToItem (mId, msgIds_, deliveryResult) = forM_ msgIds_ $ \msgIds -> do
                  let ciIds = mapMaybe (`M.lookup` msgToItem) msgIds
                      status = case deliveryResult of
                        Right _ -> GSSNew
                        Left e -> GSSError $ SndErrOther $ tshow e
                  forM_ ciIds $ \ciId -> createGroupSndStatus db ciId mId status
                processForwarded :: DB.Connection -> GroupMember -> IO ()
                processForwarded db GroupMember {groupMemberId} =
                  forM_ cis_ $ \ci_ ->
                    forM_ ci_ $ \ci -> createGroupSndStatus db (chatItemId' ci) groupMemberId GSSForwarded
                processPending :: DB.Connection -> Map MessageId ChatItemId -> (GroupMemberId, Either ChatError MessageId, Either ChatError ()) -> IO ()
                processPending db msgToItem (mId, msgId_, pendingResult) = forM_ msgId_ $ \msgId -> do
                  let ciId_ = M.lookup msgId msgToItem
                      status = case pendingResult of
                        Right _ -> GSSInactive
                        Left e -> GSSError $ SndErrOther $ tshow e
                  forM_ ciId_ $ \ciId -> createGroupSndStatus db ciId mId status
    assertMultiSendable :: Bool -> NonEmpty ComposeMessageReq -> CM ()
    assertMultiSendable live cmrs
      | length cmrs == 1 = pure ()
      | otherwise =
          -- When sending multiple messages only single quote is allowed.
          -- This is to support case of sending multiple attachments while also quoting another message.
          -- UI doesn't allow composing with multiple quotes, so api prohibits it as well, and doesn't bother
          -- batching retrieval of quoted messages (prepareMsgs).
          when (live || length (L.filter (\(ComposedMessage {quotedItemId}, _) -> isJust quotedItemId) cmrs) > 1) $
            throwChatError (CECommandError "invalid multi send: live and more than one quote not supported")
    xftpSndFileTransfer :: User -> CryptoFile -> Integer -> Int -> ContactOrGroup -> CM (FileInvitation, CIFile 'MDSnd)
    xftpSndFileTransfer user file fileSize n contactOrGroup = do
      (fInv, ciFile, ft) <- xftpSndFileTransfer_ user file fileSize n $ Just contactOrGroup
      case contactOrGroup of
        CGContact Contact {activeConn} -> forM_ activeConn $ \conn ->
          withFastStore' $ \db -> createSndFTDescrXFTP db user Nothing conn ft dummyFileDescr
        CGGroup _ ms -> forM_ ms $ \m -> saveMemberFD m `catchChatError` (toView . CRChatError (Just user))
          where
            -- we are not sending files to pending members, same as with inline files
            saveMemberFD m@GroupMember {activeConn = Just conn@Connection {connStatus}} =
              when ((connStatus == ConnReady || connStatus == ConnSndReady) && not (connDisabled conn)) $
                withFastStore' $
                  \db -> createSndFTDescrXFTP db user (Just m) conn ft dummyFileDescr
            saveMemberFD _ = pure ()
      pure (fInv, ciFile)
    prepareSndItemsData ::
      [Either ChatError SndMessage] ->
      NonEmpty ComposeMessageReq ->
      NonEmpty (Maybe (CIFile 'MDSnd)) ->
      NonEmpty (Maybe (CIQuote c)) ->
      [Either ChatError (NewSndChatItemData c)]
    prepareSndItemsData msgs_ cmrs' ciFiles_ quotedItems_ =
      [ ( case msg_ of
            Right msg -> Right $ NewSndChatItemData msg (CISndMsgContent msgContent) f q itemForwarded
            Left e -> Left e -- step over original error
        )
        | (msg_, (ComposedMessage {msgContent}, itemForwarded), f, q) <-
            zipWith4 (,,,) msgs_ (L.toList cmrs') (L.toList ciFiles_) (L.toList quotedItems_)
      ]
    processSendErrs :: User -> ([ChatError], [ChatItem c d]) -> CM ()
    processSendErrs user = \case
      -- no errors
      ([], _) -> pure ()
      -- at least one item is successfully created
      (errs, _ci : _) -> toView $ CRChatErrors (Just user) errs
      -- single error
      ([err], []) -> throwError err
      -- multiple errors
      (errs@(err : _), []) -> do
        toView $ CRChatErrors (Just user) errs
        throwError err
    getCommandDirectChatItems :: User -> Int64 -> NonEmpty ChatItemId -> CM (Contact, [CChatItem 'CTDirect])
    getCommandDirectChatItems user ctId itemIds = do
      ct <- withFastStore $ \db -> getContact db vr user ctId
      (errs, items) <- lift $ partitionEithers <$> withStoreBatch (\db -> map (getDirectCI db) (L.toList itemIds))
      unless (null errs) $ toView $ CRChatErrors (Just user) errs
      pure (ct, items)
      where
        getDirectCI :: DB.Connection -> ChatItemId -> IO (Either ChatError (CChatItem 'CTDirect))
        getDirectCI db itemId = runExceptT . withExceptT ChatErrorStore $ getDirectChatItem db user ctId itemId
    getCommandGroupChatItems :: User -> Int64 -> NonEmpty ChatItemId -> CM (GroupInfo, [CChatItem 'CTGroup])
    getCommandGroupChatItems user gId itemIds = do
      gInfo <- withFastStore $ \db -> getGroupInfo db vr user gId
      (errs, items) <- lift $ partitionEithers <$> withStoreBatch (\db -> map (getGroupCI db) (L.toList itemIds))
      unless (null errs) $ toView $ CRChatErrors (Just user) errs
      pure (gInfo, items)
      where
        getGroupCI :: DB.Connection -> ChatItemId -> IO (Either ChatError (CChatItem 'CTGroup))
        getGroupCI db itemId = runExceptT . withExceptT ChatErrorStore $ getGroupChatItem db user gId itemId
    getCommandLocalChatItems :: User -> Int64 -> NonEmpty ChatItemId -> CM (NoteFolder, [CChatItem 'CTLocal])
    getCommandLocalChatItems user nfId itemIds = do
      nf <- withStore $ \db -> getNoteFolder db user nfId
      (errs, items) <- lift $ partitionEithers <$> withStoreBatch (\db -> map (getLocalCI db) (L.toList itemIds))
      unless (null errs) $ toView $ CRChatErrors (Just user) errs
      pure (nf, items)
      where
        getLocalCI :: DB.Connection -> ChatItemId -> IO (Either ChatError (CChatItem 'CTLocal))
        getLocalCI db itemId = runExceptT . withExceptT ChatErrorStore $ getLocalChatItem db user nfId itemId
    forwardMsgContent :: ChatItem c d -> CM (Maybe MsgContent)
    forwardMsgContent ChatItem {meta = CIMeta {itemDeleted = Just _}} = pure Nothing -- this can be deleted after selection
    forwardMsgContent ChatItem {content = CISndMsgContent fmc} = pure $ Just fmc
    forwardMsgContent ChatItem {content = CIRcvMsgContent fmc} = pure $ Just fmc
    forwardMsgContent _ = throwChatError CEInvalidForward
    createNoteFolderContentItems :: User -> NoteFolderId -> NonEmpty ComposeMessageReq -> CM ChatResponse
    createNoteFolderContentItems user folderId cmrs = do
      assertNoQuotes
      nf <- withFastStore $ \db -> getNoteFolder db user folderId
      createdAt <- liftIO getCurrentTime
      ciFiles_ <- createLocalFiles nf createdAt
      let itemsData = prepareLocalItemsData cmrs ciFiles_
      cis <- createLocalChatItems user (CDLocalSnd nf) itemsData createdAt
      pure $ CRNewChatItems user (map (AChatItem SCTLocal SMDSnd (LocalChat nf)) cis)
      where
        assertNoQuotes :: CM ()
        assertNoQuotes =
          when (any (\(ComposedMessage {quotedItemId}, _) -> isJust quotedItemId) cmrs) $
            throwChatError (CECommandError "createNoteFolderContentItems: quotes not supported")
        createLocalFiles :: NoteFolder -> UTCTime -> CM (NonEmpty (Maybe (CIFile 'MDSnd)))
        createLocalFiles nf createdAt =
          forM cmrs $ \(ComposedMessage {fileSource = file_}, _) ->
            forM file_ $ \cf@CryptoFile {filePath, cryptoArgs} -> do
              fsFilePath <- lift $ toFSFilePath filePath
              fileSize <- liftIO $ CF.getFileContentsSize $ CryptoFile fsFilePath cryptoArgs
              chunkSize <- asks $ fileChunkSize . config
              withFastStore' $ \db -> do
                fileId <- createLocalFile CIFSSndStored db user nf createdAt cf fileSize chunkSize
                pure CIFile {fileId, fileName = takeFileName filePath, fileSize, fileSource = Just cf, fileStatus = CIFSSndStored, fileProtocol = FPLocal}
        prepareLocalItemsData ::
          NonEmpty ComposeMessageReq ->
          NonEmpty (Maybe (CIFile 'MDSnd)) ->
          [(CIContent 'MDSnd, Maybe (CIFile 'MDSnd), Maybe CIForwardedFrom)]
        prepareLocalItemsData cmrs' ciFiles_ =
          [ (CISndMsgContent mc, f, itemForwarded)
            | ((ComposedMessage {msgContent = mc}, itemForwarded), f) <- zip (L.toList cmrs') (L.toList ciFiles_)
          ]
    getConnQueueInfo user Connection {connId, agentConnId = AgentConnId acId} = do
      msgInfo <- withFastStore' (`getLastRcvMsgInfo` connId)
      CRQueueInfo user msgInfo <$> withAgent (`getConnectionQueueInfo` acId)

protocolServers :: UserProtocol p => SProtocolType p -> ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP]) -> ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP])
protocolServers p (operators, smpServers, xftpServers) = case p of
  SPSMP -> (operators, smpServers, [])
  SPXFTP -> (operators, [], xftpServers)

-- disable preset and replace custom servers (groupByOperator always adds custom)
updatedServers :: forall p. UserProtocol p => SProtocolType p -> [AUserServer p] -> UserOperatorServers -> UpdatedUserOperatorServers
updatedServers p' srvs UserOperatorServers {operator, smpServers, xftpServers} = case p' of
  SPSMP -> u (updateSrvs smpServers, map (AUS SDBStored) xftpServers)
  SPXFTP -> u (map (AUS SDBStored) smpServers, updateSrvs xftpServers)
  where
    u = uncurry $ UpdatedUserOperatorServers operator
    updateSrvs :: [UserServer p] -> [AUserServer p]
    updateSrvs pSrvs = map disableSrv pSrvs <> maybe srvs (const []) operator
    disableSrv srv@UserServer {preset} =
      AUS SDBStored $ if preset then srv {enabled = False} else srv {deleted = True}

type ComposeMessageReq = (ComposedMessage, Maybe CIForwardedFrom)

contactCITimed :: Contact -> CM (Maybe CITimed)
contactCITimed ct = sndContactCITimed False ct Nothing

sndContactCITimed :: Bool -> Contact -> Maybe Int -> CM (Maybe CITimed)
sndContactCITimed live = sndCITimed_ live . contactTimedTTL

sndGroupCITimed :: Bool -> GroupInfo -> Maybe Int -> CM (Maybe CITimed)
sndGroupCITimed live = sndCITimed_ live . groupTimedTTL

sndCITimed_ :: Bool -> Maybe (Maybe Int) -> Maybe Int -> CM (Maybe CITimed)
sndCITimed_ live chatTTL itemTTL =
  forM (chatTTL >>= (itemTTL <|>)) $ \ttl ->
    CITimed ttl
      <$> if live
        then pure Nothing
        else Just . addUTCTime (realToFrac ttl) <$> liftIO getCurrentTime

toggleNtf :: User -> GroupMember -> Bool -> CM ()
toggleNtf user m ntfOn =
  when (memberActive m) $
    forM_ (memberConnId m) $ \connId ->
      withAgent (\a -> toggleConnectionNtfs a connId ntfOn) `catchChatError` (toView . CRChatError (Just user))

data ChangedProfileContact = ChangedProfileContact
  { ct :: Contact,
    ct' :: Contact,
    mergedProfile' :: Profile,
    conn :: Connection
  }

prepareGroupMsg :: User -> GroupInfo -> MsgContent -> Maybe ChatItemId -> Maybe CIForwardedFrom -> Maybe FileInvitation -> Maybe CITimed -> Bool -> CM (MsgContainer, Maybe (CIQuote 'CTGroup))
prepareGroupMsg user GroupInfo {groupId, membership} mc quotedItemId_ itemForwarded fInv_ timed_ live = case (quotedItemId_, itemForwarded) of
  (Nothing, Nothing) -> pure (MCSimple (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
  (Nothing, Just _) -> pure (MCForward (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
  (Just quotedItemId, Nothing) -> do
    CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
      withStore $ \db -> getGroupChatItem db user groupId quotedItemId
    (origQmc, qd, sent, GroupMember {memberId}) <- quoteData qci membership
    let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Just memberId}
        qmc = quoteContent mc origQmc file
        quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
    pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Just quotedItem)
  (Just _, Just _) -> throwChatError CEInvalidQuote
  where
    quoteData :: ChatItem c d -> GroupMember -> CM (MsgContent, CIQDirection 'CTGroup, Bool, GroupMember)
    quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} _ = throwChatError CEInvalidQuote
    quoteData ChatItem {chatDir = CIGroupSnd, content = CISndMsgContent qmc} membership' = pure (qmc, CIQGroupSnd, True, membership')
    quoteData ChatItem {chatDir = CIGroupRcv m, content = CIRcvMsgContent qmc} _ = pure (qmc, CIQGroupRcv $ Just m, False, m)
    quoteData _ _ = throwChatError CEInvalidQuote

quoteContent :: forall d. MsgContent -> MsgContent -> Maybe (CIFile d) -> MsgContent
quoteContent mc qmc ciFile_
  | replaceContent = MCText qTextOrFile
  | otherwise = case qmc of
      MCImage _ image -> MCImage qTextOrFile image
      MCFile _ -> MCFile qTextOrFile
      -- consider same for voice messages
      -- MCVoice _ voice -> MCVoice qTextOrFile voice
      _ -> qmc
  where
    -- if the message we're quoting with is one of the "large" MsgContents
    -- we replace the quote's content with MCText
    replaceContent = case mc of
      MCText _ -> False
      MCFile _ -> False
      MCLink {} -> True
      MCImage {} -> True
      MCVideo {} -> True
      MCVoice {} -> False
      MCReport {} -> False
      MCUnknown {} -> True
    qText = msgContentText qmc
    getFileName :: CIFile d -> String
    getFileName CIFile {fileName} = fileName
    qFileName = maybe qText (T.pack . getFileName) ciFile_
    qTextOrFile = if T.null qText then qFileName else qText

assertDirectAllowed :: User -> MsgDirection -> Contact -> CMEventTag e -> CM ()
assertDirectAllowed user dir ct event =
  unless (allowedChatEvent || anyDirectOrUsed ct) . unlessM directMessagesAllowed $
    throwChatError (CEDirectMessagesProhibited dir ct)
  where
    directMessagesAllowed = any (uncurry $ groupFeatureMemberAllowed' SGFDirectMessages) <$> withStore' (\db -> getContactGroupPreferences db user ct)
    allowedChatEvent = case event of
      XMsgNew_ -> False
      XMsgUpdate_ -> False
      XMsgDel_ -> False
      XFile_ -> False
      XGrpInv_ -> False
      XCallInv_ -> False
      _ -> True

prohibitedGroupContent :: GroupInfo -> GroupMember -> MsgContent -> Maybe f -> Maybe GroupFeature
prohibitedGroupContent gInfo m mc file_
  | isVoice mc && not (groupFeatureMemberAllowed SGFVoice m gInfo) = Just GFVoice
  | not (isVoice mc) && isJust file_ && not (groupFeatureMemberAllowed SGFFiles m gInfo) = Just GFFiles
  | prohibitedSimplexLinks gInfo m mc = Just GFSimplexLinks
  | otherwise = Nothing

prohibitedSimplexLinks :: GroupInfo -> GroupMember -> MsgContent -> Bool
prohibitedSimplexLinks gInfo m mc =
  not (groupFeatureMemberAllowed SGFSimplexLinks m gInfo)
    && maybe False (any ftIsSimplexLink) (parseMaybeMarkdownList $ msgContentText mc)
  where
    ftIsSimplexLink :: FormattedText -> Bool
    ftIsSimplexLink FormattedText {format} = maybe False isSimplexLink format

roundedFDCount :: Int -> Int
roundedFDCount n
  | n <= 0 = 4
  | otherwise = max 4 $ fromIntegral $ (2 :: Integer) ^ (ceiling (logBase 2 (fromIntegral n) :: Double) :: Integer)

startExpireCIThread :: User -> CM' ()
startExpireCIThread user@User {userId} = do
  expireThreads <- asks expireCIThreads
  atomically (TM.lookup userId expireThreads) >>= \case
    Nothing -> do
      a <- Just <$> async runExpireCIs
      atomically $ TM.insert userId a expireThreads
    _ -> pure ()
  where
    runExpireCIs = do
      delay <- asks (initialCleanupManagerDelay . config)
      liftIO $ threadDelay' delay
      interval <- asks $ ciExpirationInterval . config
      forever $ do
        flip catchChatError' (toView' . CRChatError (Just user)) $ do
          expireFlags <- asks expireCIFlags
          atomically $ TM.lookup userId expireFlags >>= \b -> unless (b == Just True) retry
          lift waitChatStartedAndActivated
          ttl <- withStore' (`getChatItemTTL` user)
          forM_ ttl $ \t -> expireChatItems user t False
        liftIO $ threadDelay' interval

setExpireCIFlag :: User -> Bool -> CM' ()
setExpireCIFlag User {userId} b = do
  expireFlags <- asks expireCIFlags
  atomically $ TM.insert userId b expireFlags

setAllExpireCIFlags :: Bool -> CM' ()
setAllExpireCIFlags b = do
  expireFlags <- asks expireCIFlags
  atomically $ do
    keys <- M.keys <$> readTVar expireFlags
    forM_ keys $ \k -> TM.insert k b expireFlags

cancelFilesInProgress :: User -> [CIFileInfo] -> CM ()
cancelFilesInProgress user filesInfo = do
  let filesInfo' = filter (not . fileEnded) filesInfo
  (sfs, rfs) <- lift $ splitFTTypes <$> withStoreBatch (\db -> map (getFT db) filesInfo')
  forM_ rfs $ \RcvFileTransfer {fileId} -> lift (closeFileHandle fileId rcvFiles) `catchChatError` \_ -> pure ()
  lift . void . withStoreBatch' $ \db -> map (updateSndFileCancelled db) sfs
  lift . void . withStoreBatch' $ \db -> map (updateRcvFileCancelled db) rfs
  let xsfIds = mapMaybe (\(FileTransferMeta {fileId, xftpSndFile}, _) -> (,fileId) <$> xftpSndFile) sfs
      xrfIds = mapMaybe (\RcvFileTransfer {fileId, xftpRcvFile} -> (,fileId) <$> xftpRcvFile) rfs
  lift $ agentXFTPDeleteSndFilesRemote user xsfIds
  lift $ agentXFTPDeleteRcvFiles xrfIds
  let smpSFConnIds = concatMap (\(ft, sfts) -> mapMaybe (smpSndFileConnId ft) sfts) sfs
      smpRFConnIds = mapMaybe smpRcvFileConnId rfs
  deleteAgentConnectionsAsync user smpSFConnIds
  deleteAgentConnectionsAsync user smpRFConnIds
  where
    fileEnded CIFileInfo {fileStatus} = case fileStatus of
      Just (AFS _ status) -> ciFileEnded status
      Nothing -> True
    getFT :: DB.Connection -> CIFileInfo -> IO (Either ChatError FileTransfer)
    getFT db CIFileInfo {fileId} = runExceptT . withExceptT ChatErrorStore $ getFileTransfer db user fileId
    updateSndFileCancelled :: DB.Connection -> (FileTransferMeta, [SndFileTransfer]) -> IO ()
    updateSndFileCancelled db (FileTransferMeta {fileId}, sfts) = do
      updateFileCancelled db user fileId CIFSSndCancelled
      forM_ sfts updateSndFTCancelled
      where
        updateSndFTCancelled :: SndFileTransfer -> IO ()
        updateSndFTCancelled ft = unless (sndFTEnded ft) $ do
          updateSndFileStatus db ft FSCancelled
          deleteSndFileChunks db ft
    updateRcvFileCancelled :: DB.Connection -> RcvFileTransfer -> IO ()
    updateRcvFileCancelled db ft@RcvFileTransfer {fileId} = do
      updateFileCancelled db user fileId CIFSRcvCancelled
      updateRcvFileStatus db fileId FSCancelled
      deleteRcvFileChunks db ft
    splitFTTypes :: [Either ChatError FileTransfer] -> ([(FileTransferMeta, [SndFileTransfer])], [RcvFileTransfer])
    splitFTTypes = foldr addFT ([], []) . rights
      where
        addFT f (sfs, rfs) = case f of
          FTSnd ft@FileTransferMeta {cancelled} sfts | not cancelled -> ((ft, sfts) : sfs, rfs)
          FTRcv ft@RcvFileTransfer {cancelled} | not cancelled -> (sfs, ft : rfs)
          _ -> (sfs, rfs)
    smpSndFileConnId :: FileTransferMeta -> SndFileTransfer -> Maybe ConnId
    smpSndFileConnId FileTransferMeta {xftpSndFile} sft@SndFileTransfer {agentConnId = AgentConnId acId, fileInline}
      | isNothing xftpSndFile && isNothing fileInline && not (sndFTEnded sft) = Just acId
      | otherwise = Nothing
    smpRcvFileConnId :: RcvFileTransfer -> Maybe ConnId
    smpRcvFileConnId ft@RcvFileTransfer {xftpRcvFile, rcvFileInline}
      | isNothing xftpRcvFile && isNothing rcvFileInline = liveRcvFileTransferConnId ft
      | otherwise = Nothing
    sndFTEnded SndFileTransfer {fileStatus} = fileStatus == FSCancelled || fileStatus == FSComplete

deleteFilesLocally :: [CIFileInfo] -> CM ()
deleteFilesLocally files =
  withFilesFolder $ \filesFolder ->
    liftIO . forM_ files $ \CIFileInfo {filePath} ->
      mapM_ (delete . (filesFolder </>)) filePath
  where
    delete :: FilePath -> IO ()
    delete fPath =
      removeFile fPath `catchAll` \_ ->
        removePathForcibly fPath `catchAll_` pure ()
    -- perform an action only if filesFolder is set (i.e. on mobile devices)
    withFilesFolder :: (FilePath -> CM ()) -> CM ()
    withFilesFolder action = asks filesFolder >>= readTVarIO >>= mapM_ action

updateCallItemStatus :: User -> Contact -> Call -> WebRTCCallStatus -> Maybe MessageId -> CM ()
updateCallItemStatus user ct@Contact {contactId} Call {chatItemId} receivedStatus msgId_ = do
  aciContent_ <- callStatusItemContent user ct chatItemId receivedStatus
  forM_ aciContent_ $ \aciContent -> do
    timed_ <- callTimed ct aciContent
    updateDirectChatItemView user ct chatItemId aciContent False False timed_ msgId_
    forM_ (timed_ >>= timedDeleteAt') $
      startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId)

callTimed :: Contact -> ACIContent -> CM (Maybe CITimed)
callTimed ct aciContent =
  case aciContentCallStatus aciContent of
    Just callStatus
      | callComplete callStatus -> do
          contactCITimed ct
    _ -> pure Nothing
  where
    aciContentCallStatus :: ACIContent -> Maybe CICallStatus
    aciContentCallStatus (ACIContent _ (CISndCall st _)) = Just st
    aciContentCallStatus (ACIContent _ (CIRcvCall st _)) = Just st
    aciContentCallStatus _ = Nothing

updateDirectChatItemView :: User -> Contact -> ChatItemId -> ACIContent -> Bool -> Bool -> Maybe CITimed -> Maybe MessageId -> CM ()
updateDirectChatItemView user ct chatItemId (ACIContent msgDir ciContent) edited live timed_ msgId_ = do
  ci' <- withStore $ \db -> updateDirectChatItem db user ct chatItemId ciContent edited live timed_ msgId_
  toView $ CRChatItemUpdated user (AChatItem SCTDirect msgDir (DirectChat ct) ci')

callStatusItemContent :: User -> Contact -> ChatItemId -> WebRTCCallStatus -> CM (Maybe ACIContent)
callStatusItemContent user Contact {contactId} chatItemId receivedStatus = do
  CChatItem msgDir ChatItem {meta = CIMeta {updatedAt}, content} <-
    withStore $ \db -> getDirectChatItem db user contactId chatItemId
  ts <- liftIO getCurrentTime
  let callDuration :: Int = nominalDiffTimeToSeconds (ts `diffUTCTime` updatedAt) `div'` 1
      callStatus = case content of
        CISndCall st _ -> Just st
        CIRcvCall st _ -> Just st
        _ -> Nothing
      newState_ = case (callStatus, receivedStatus) of
        (Just CISCallProgress, WCSConnected) -> Nothing -- if call in-progress received connected -> no change
        (Just CISCallProgress, WCSDisconnected) -> Just (CISCallEnded, callDuration) -- calculate in-progress duration
        (Just CISCallProgress, WCSFailed) -> Just (CISCallEnded, callDuration) -- whether call disconnected or failed
        (Just CISCallPending, WCSDisconnected) -> Just (CISCallMissed, 0)
        (Just CISCallEnded, _) -> Nothing -- if call already ended or failed -> no change
        (Just CISCallError, _) -> Nothing
        (Just _, WCSConnecting) -> Just (CISCallNegotiated, 0)
        (Just _, WCSConnected) -> Just (CISCallProgress, 0) -- if call ended that was never connected, duration = 0
        (Just _, WCSDisconnected) -> Just (CISCallEnded, 0)
        (Just _, WCSFailed) -> Just (CISCallError, 0)
        (Nothing, _) -> Nothing -- some other content - we should never get here, but no exception is thrown
  pure $ aciContent msgDir <$> newState_
  where
    aciContent :: forall d. SMsgDirection d -> (CICallStatus, Int) -> ACIContent
    aciContent msgDir (callStatus', duration) = case msgDir of
      SMDSnd -> ACIContent SMDSnd $ CISndCall callStatus' duration
      SMDRcv -> ACIContent SMDRcv $ CIRcvCall callStatus' duration

-- mobile clients use file paths relative to app directory (e.g. for the reason ios app directory changes on updates),
-- so we have to differentiate between the file path stored in db and communicated with frontend, and the file path
-- used during file transfer for actual operations with file system
toFSFilePath :: FilePath -> CM' FilePath
toFSFilePath f =
  maybe f (</> f) <$> (chatReadVar' filesFolder)

setFileToEncrypt :: RcvFileTransfer -> CM RcvFileTransfer
setFileToEncrypt ft@RcvFileTransfer {fileId} = do
  cfArgs <- atomically . CF.randomArgs =<< asks random
  withStore' $ \db -> setFileCryptoArgs db fileId cfArgs
  pure (ft :: RcvFileTransfer) {cryptoArgs = Just cfArgs}

receiveFile' :: User -> RcvFileTransfer -> Bool -> Maybe Bool -> Maybe FilePath -> CM ChatResponse
receiveFile' user ft userApprovedRelays rcvInline_ filePath_ = do
  (CRRcvFileAccepted user <$> acceptFileReceive user ft userApprovedRelays rcvInline_ filePath_) `catchChatError` processError
  where
    processError = \case
      -- TODO AChatItem in Cancelled events
      ChatErrorAgent (SMP _ SMP.AUTH) _ -> pure $ CRRcvFileAcceptedSndCancelled user ft
      ChatErrorAgent (CONN DUPLICATE) _ -> pure $ CRRcvFileAcceptedSndCancelled user ft
      e -> throwError e

acceptFileReceive :: User -> RcvFileTransfer -> Bool -> Maybe Bool -> Maybe FilePath -> CM AChatItem
acceptFileReceive user@User {userId} RcvFileTransfer {fileId, xftpRcvFile, fileInvitation = FileInvitation {fileName = fName, fileConnReq, fileInline, fileSize}, fileStatus, grpMemberId, cryptoArgs} userApprovedRelays rcvInline_ filePath_ = do
  unless (fileStatus == RFSNew) $ case fileStatus of
    RFSCancelled _ -> throwChatError $ CEFileCancelled fName
    _ -> throwChatError $ CEFileAlreadyReceiving fName
  vr <- chatVersionRange
  case (xftpRcvFile, fileConnReq) of
    -- direct file protocol
    (Nothing, Just connReq) -> do
      subMode <- chatReadVar subscriptionMode
      dm <- encodeConnInfo $ XFileAcpt fName
      connIds <- joinAgentConnectionAsync user True connReq dm subMode
      filePath <- getRcvFilePath fileId filePath_ fName True
      withStore $ \db -> acceptRcvFileTransfer db vr user fileId connIds ConnJoined filePath subMode
    -- XFTP
    (Just XFTPRcvFile {userApprovedRelays = approvedBeforeReady}, _) -> do
      let userApproved = approvedBeforeReady || userApprovedRelays
      filePath <- getRcvFilePath fileId filePath_ fName False
      (ci, rfd) <- withStore $ \db -> do
        -- marking file as accepted and reading description in the same transaction
        -- to prevent race condition with appending description
        ci <- xftpAcceptRcvFT db vr user fileId filePath userApproved
        rfd <- getRcvFileDescrByRcvFileId db fileId
        pure (ci, rfd)
      receiveViaCompleteFD user fileId rfd userApproved cryptoArgs
      pure ci
    -- group & direct file protocol
    _ -> do
      chatRef <- withStore $ \db -> getChatRefByFileId db user fileId
      case (chatRef, grpMemberId) of
        (ChatRef CTDirect contactId, Nothing) -> do
          ct <- withStore $ \db -> getContact db vr user contactId
          acceptFile CFCreateConnFileInvDirect $ \msg -> void $ sendDirectContactMessage user ct msg
        (ChatRef CTGroup groupId, Just memId) -> do
          GroupMember {activeConn} <- withStore $ \db -> getGroupMember db vr user groupId memId
          case activeConn of
            Just conn -> do
              acceptFile CFCreateConnFileInvGroup $ \msg -> void $ sendDirectMemberMessage conn msg groupId
            _ -> throwChatError $ CEFileInternal "member connection not active"
        _ -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
  where
    acceptFile :: CommandFunction -> (ChatMsgEvent 'Json -> CM ()) -> CM AChatItem
    acceptFile cmdFunction send = do
      filePath <- getRcvFilePath fileId filePath_ fName True
      inline <- receiveInline
      vr <- chatVersionRange
      if
        | inline -> do
            -- accepting inline
            ci <- withStore $ \db -> acceptRcvInlineFT db vr user fileId filePath
            sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
            send $ XFileAcptInv sharedMsgId Nothing fName
            pure ci
        | fileInline == Just IFMSent -> throwChatError $ CEFileAlreadyReceiving fName
        | otherwise -> do
            -- accepting via a new connection
            subMode <- chatReadVar subscriptionMode
            connIds <- createAgentConnectionAsync user cmdFunction True SCMInvitation subMode
            withStore $ \db -> acceptRcvFileTransfer db vr user fileId connIds ConnNew filePath subMode
    receiveInline :: CM Bool
    receiveInline = do
      ChatConfig {fileChunkSize, inlineFiles = InlineFilesConfig {receiveChunks, offerChunks}} <- asks config
      pure $
        rcvInline_ /= Just False
          && fileInline == Just IFMOffer
          && ( fileSize <= fileChunkSize * receiveChunks
                || (rcvInline_ == Just True && fileSize <= fileChunkSize * offerChunks)
             )

receiveViaCompleteFD :: User -> FileTransferId -> RcvFileDescr -> Bool -> Maybe CryptoFileArgs -> CM ()
receiveViaCompleteFD user fileId RcvFileDescr {fileDescrText, fileDescrComplete} userApprovedRelays cfArgs =
  when fileDescrComplete $ do
    rd <- parseFileDescription fileDescrText
    if userApprovedRelays
      then receive' rd True
      else do
        let srvs = fileServers rd
        unknownSrvs <- getUnknownSrvs srvs
        let approved = null unknownSrvs
        ifM
          ((approved ||) <$> ipProtectedForSrvs srvs)
          (receive' rd approved)
          (relaysNotApproved unknownSrvs)
  where
    receive' :: ValidFileDescription 'FRecipient -> Bool -> CM ()
    receive' rd approved = do
      aFileId <- withAgent $ \a -> xftpReceiveFile a (aUserId user) rd cfArgs approved
      startReceivingFile user fileId
      withStore' $ \db -> updateRcvFileAgentId db fileId (Just $ AgentRcvFileId aFileId)
    fileServers :: ValidFileDescription 'FRecipient -> [XFTPServer]
    fileServers (FD.ValidFileDescription FD.FileDescription {chunks}) =
      S.toList $ S.fromList $ concatMap (\FD.FileChunk {replicas} -> map (\FD.FileChunkReplica {server} -> server) replicas) chunks
    getUnknownSrvs :: [XFTPServer] -> CM [XFTPServer]
    getUnknownSrvs srvs = do
      knownSrvs <- L.map protoServer' <$> getKnownAgentServers SPXFTP user
      pure $ filter (`notElem` knownSrvs) srvs
    ipProtectedForSrvs :: [XFTPServer] -> CM Bool
    ipProtectedForSrvs srvs = do
      netCfg <- lift getNetworkConfig
      pure $ all (ipAddressProtected netCfg) srvs
    relaysNotApproved :: [XFTPServer] -> CM ()
    relaysNotApproved unknownSrvs = do
      aci_ <- resetRcvCIFileStatus user fileId CIFSRcvInvitation
      forM_ aci_ $ \aci -> do
        cleanupACIFile aci
        toView $ CRChatItemUpdated user aci
      throwChatError $ CEFileNotApproved fileId unknownSrvs

getKnownAgentServers :: (ProtocolTypeI p, UserProtocol p) => SProtocolType p -> User -> CM (NonEmpty (ServerCfg p))
getKnownAgentServers p user = do
  as <- asks randomAgentServers
  withStore $ \db -> do
    opDomains <- operatorDomains . serverOperators <$> getServerOperators db
    srvs <- liftIO $ getProtocolServers db p user
    pure $ useServerCfgs p as opDomains srvs

protoServer' :: ServerCfg p -> ProtocolServer p
protoServer' ServerCfg {server} = protoServer server

getNetworkConfig :: CM' NetworkConfig
getNetworkConfig = withAgent' $ liftIO . getFastNetworkConfig

resetRcvCIFileStatus :: User -> FileTransferId -> CIFileStatus 'MDRcv -> CM (Maybe AChatItem)
resetRcvCIFileStatus user fileId ciFileStatus = do
  vr <- chatVersionRange
  withStore $ \db -> do
    liftIO $ do
      updateCIFileStatus db user fileId ciFileStatus
      updateRcvFileStatus db fileId FSNew
      updateRcvFileAgentId db fileId Nothing
    lookupChatItemByFileId db vr user fileId

receiveViaURI :: User -> FileDescriptionURI -> CryptoFile -> CM RcvFileTransfer
receiveViaURI user@User {userId} FileDescriptionURI {description} cf@CryptoFile {cryptoArgs} = do
  fileId <- withStore $ \db -> createRcvStandaloneFileTransfer db userId cf fileSize chunkSize
  -- currently the only use case is user migrating via their configured servers, so we pass approvedRelays = True
  aFileId <- withAgent $ \a -> xftpReceiveFile a (aUserId user) description cryptoArgs True
  withStore $ \db -> do
    liftIO $ do
      updateRcvFileStatus db fileId FSConnected
      updateCIFileStatus db user fileId $ CIFSRcvTransfer 0 1
      updateRcvFileAgentId db fileId (Just $ AgentRcvFileId aFileId)
    getRcvFileTransfer db user fileId
  where
    FD.ValidFileDescription FD.FileDescription {size = FD.FileSize fileSize, chunkSize = FD.FileSize chunkSize} = description

startReceivingFile :: User -> FileTransferId -> CM ()
startReceivingFile user fileId = do
  vr <- chatVersionRange
  ci <- withStore $ \db -> do
    liftIO $ updateRcvFileStatus db fileId FSConnected
    liftIO $ updateCIFileStatus db user fileId $ CIFSRcvTransfer 0 1
    getChatItemByFileId db vr user fileId
  toView $ CRRcvFileStart user ci

getRcvFilePath :: FileTransferId -> Maybe FilePath -> String -> Bool -> CM FilePath
getRcvFilePath fileId fPath_ fn keepHandle = case fPath_ of
  Nothing ->
    chatReadVar filesFolder >>= \case
      Nothing -> do
        defaultFolder <- lift getDefaultFilesFolder
        fPath <- liftIO $ defaultFolder `uniqueCombine` fn
        createEmptyFile fPath $> fPath
      Just filesFolder -> do
        fPath <- liftIO $ filesFolder `uniqueCombine` fn
        createEmptyFile fPath
        pure $ takeFileName fPath
  Just fPath ->
    ifM
      (doesDirectoryExist fPath)
      (createInPassedDirectory fPath)
      $ ifM
        (doesFileExist fPath)
        (throwChatError $ CEFileAlreadyExists fPath)
        (createEmptyFile fPath $> fPath)
  where
    createInPassedDirectory :: FilePath -> CM FilePath
    createInPassedDirectory fPathDir = do
      fPath <- liftIO $ fPathDir `uniqueCombine` fn
      createEmptyFile fPath $> fPath
    createEmptyFile :: FilePath -> CM ()
    createEmptyFile fPath = emptyFile `catchThrow` (ChatError . CEFileWrite fPath . show)
      where
        emptyFile :: CM ()
        emptyFile
          | keepHandle = do
              h <- getFileHandle fileId fPath rcvFiles AppendMode
              liftIO $ B.hPut h "" >> hFlush h
          | otherwise = liftIO $ B.writeFile fPath ""

acceptContactRequest :: User -> UserContactRequest -> IncognitoEnabled -> CM (Contact, Connection, SndQueueSecured)
acceptContactRequest user@User {userId} UserContactRequest {agentInvitationId = AgentInvId invId, contactId_, cReqChatVRange, localDisplayName = cName, profileId, profile = cp, userContactLinkId, xContactId, pqSupport} incognito = do
  subMode <- chatReadVar subscriptionMode
  let pqSup = PQSupportOn
      pqSup' = pqSup `CR.pqSupportAnd` pqSupport
  vr <- chatVersionRange
  let chatV = vr `peerConnChatVersion` cReqChatVRange
  (ct, conn, incognitoProfile) <- case contactId_ of
    Nothing -> do
      incognitoProfile <- if incognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
      connId <- withAgent $ \a -> prepareConnectionToAccept a True invId pqSup'
      (ct, conn) <- withStore' $ \db -> createAcceptedContact db user connId chatV cReqChatVRange cName profileId cp userContactLinkId xContactId incognitoProfile subMode pqSup' False
      pure (ct, conn, incognitoProfile)
    Just contactId -> do
      ct <- withFastStore $ \db -> getContact db vr user contactId
      case contactConn ct of
        Nothing -> throwChatError $ CECommandError "contact has no connection"
        Just conn@Connection {customUserProfileId} -> do
          incognitoProfile <- forM customUserProfileId $ \pId -> withFastStore $ \db -> getProfileById db userId pId
          pure (ct, conn, ExistingIncognito <$> incognitoProfile)
  let profileToSend = profileToSendOnAccept user incognitoProfile False
  dm <- encodeConnInfoPQ pqSup' chatV $ XInfo profileToSend
  (ct,conn,) <$> withAgent (\a -> acceptContact a (aConnId conn) True invId dm pqSup' subMode)

acceptContactRequestAsync :: User -> UserContactRequest -> Maybe IncognitoProfile -> Bool -> PQSupport -> CM Contact
acceptContactRequestAsync user cReq@UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange, localDisplayName = cName, profileId, profile = p, userContactLinkId, xContactId} incognitoProfile contactUsed pqSup = do
  subMode <- chatReadVar subscriptionMode
  let profileToSend = profileToSendOnAccept user incognitoProfile False
  vr <- chatVersionRange
  let chatV = vr `peerConnChatVersion` cReqChatVRange
  (cmdId, acId) <- agentAcceptContactAsync user True invId (XInfo profileToSend) subMode pqSup chatV
  withStore' $ \db -> do
    (ct, Connection {connId}) <- createAcceptedContact db user acId chatV cReqChatVRange cName profileId p userContactLinkId xContactId incognitoProfile subMode pqSup contactUsed
    deleteContactRequestRec db user cReq
    setCommandConnId db user cmdId connId
    pure ct

acceptGroupJoinRequestAsync :: User -> GroupInfo -> UserContactRequest -> GroupMemberRole -> Maybe IncognitoProfile -> CM GroupMember
acceptGroupJoinRequestAsync
  user
  gInfo@GroupInfo {groupProfile, membership, businessChat}
  ucr@UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange}
  gLinkMemRole
  incognitoProfile = do
    gVar <- asks random
    (groupMemberId, memberId) <- withStore $ \db -> do
      liftIO $ deleteContactRequestRec db user ucr
      createAcceptedMember db gVar user gInfo ucr gLinkMemRole
    currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
    let Profile {displayName} = profileToSendOnAccept user incognitoProfile True
        GroupMember {memberRole = userRole, memberId = userMemberId} = membership
        msg =
          XGrpLinkInv $
            GroupLinkInvitation
              { fromMember = MemberIdRole userMemberId userRole,
                fromMemberName = displayName,
                invitedMember = MemberIdRole memberId gLinkMemRole,
                groupProfile,
                business = businessChat,
                groupSize = Just currentMemCount
              }
    subMode <- chatReadVar subscriptionMode
    vr <- chatVersionRange
    let chatV = vr `peerConnChatVersion` cReqChatVRange
    connIds <- agentAcceptContactAsync user True invId msg subMode PQSupportOff chatV
    withStore $ \db -> do
      liftIO $ createAcceptedMemberConnection db user connIds chatV ucr groupMemberId subMode
      getGroupMemberById db vr user groupMemberId

acceptBusinessJoinRequestAsync :: User -> UserContactRequest -> CM GroupInfo
acceptBusinessJoinRequestAsync
  user
  ucr@UserContactRequest {contactRequestId, agentInvitationId = AgentInvId invId, cReqChatVRange} = do
    vr <- chatVersionRange
    gVar <- asks random
    let userProfile@Profile {displayName, preferences} = profileToSendOnAccept user Nothing True
        groupPreferences = maybe defaultBusinessGroupPrefs businessGroupPrefs preferences
    (gInfo, clientMember) <- withStore $ \db -> do
      liftIO $ deleteContactRequest db user contactRequestId
      createBusinessRequestGroup db vr gVar user ucr groupPreferences
    let GroupInfo {membership} = gInfo
        GroupMember {memberRole = userRole, memberId = userMemberId} = membership
        GroupMember {groupMemberId, memberId} = clientMember
        msg =
          XGrpLinkInv $
            GroupLinkInvitation
              { fromMember = MemberIdRole userMemberId userRole,
                fromMemberName = displayName,
                invitedMember = MemberIdRole memberId GRMember,
                groupProfile = businessGroupProfile userProfile groupPreferences,
                -- This refers to the "title member" that defines the group name and profile.
                -- This coincides with fromMember to be current user when accepting the connecting user,
                -- but it will be different when inviting somebody else.
                business = Just $ BusinessChatInfo {chatType = BCBusiness, businessId = userMemberId, customerId = memberId},
                groupSize = Just 1
              }
    subMode <- chatReadVar subscriptionMode
    let chatV = vr `peerConnChatVersion` cReqChatVRange
    connIds <- agentAcceptContactAsync user True invId msg subMode PQSupportOff chatV
    withStore' $ \db -> createAcceptedMemberConnection db user connIds chatV ucr groupMemberId subMode
    let cd = CDGroupSnd gInfo
    createInternalChatItem user cd (CISndGroupE2EEInfo E2EInfo {pqEnabled = PQEncOff}) Nothing
    createGroupFeatureItems user cd CISndGroupFeature gInfo
    pure gInfo
    where
      businessGroupProfile :: Profile -> GroupPreferences -> GroupProfile
      businessGroupProfile Profile {displayName, fullName, image} groupPreferences =
        GroupProfile {displayName, fullName, description = Nothing, image, groupPreferences = Just groupPreferences}

profileToSendOnAccept :: User -> Maybe IncognitoProfile -> Bool -> Profile
profileToSendOnAccept user ip = userProfileToSend user (getIncognitoProfile <$> ip) Nothing
  where
    getIncognitoProfile = \case
      NewIncognito p -> p
      ExistingIncognito lp -> fromLocalProfile lp

deleteGroupLink' :: User -> GroupInfo -> CM ()
deleteGroupLink' user gInfo = do
  vr <- chatVersionRange
  conn <- withStore $ \db -> getGroupLinkConnection db vr user gInfo
  deleteGroupLink_ user gInfo conn

deleteGroupLinkIfExists :: User -> GroupInfo -> CM ()
deleteGroupLinkIfExists user gInfo = do
  vr <- chatVersionRange
  conn_ <- eitherToMaybe <$> withStore' (\db -> runExceptT $ getGroupLinkConnection db vr user gInfo)
  mapM_ (deleteGroupLink_ user gInfo) conn_

deleteGroupLink_ :: User -> GroupInfo -> Connection -> CM ()
deleteGroupLink_ user gInfo conn = do
  deleteAgentConnectionAsync user $ aConnId conn
  withStore' $ \db -> deleteGroupLink db user gInfo

agentSubscriber :: CM' ()
agentSubscriber = do
  q <- asks $ subQ . smpAgent
  forever (atomically (readTBQueue q) >>= process)
    `E.catchAny` \e -> do
      toView' $ CRChatError Nothing $ ChatErrorAgent (CRITICAL True $ "Message reception stopped: " <> show e) Nothing
      E.throwIO e
  where
    process :: (ACorrId, AEntityId, AEvt) -> CM' ()
    process (corrId, entId, AEvt e msg) = run $ case e of
      SAENone -> processAgentMessageNoConn msg
      SAEConn -> processAgentMessage corrId entId msg
      SAERcvFile -> processAgentMsgRcvFile corrId entId msg
      SAESndFile -> processAgentMsgSndFile corrId entId msg
      where
        run action = action `catchChatError'` (toView' . CRChatError Nothing)

type AgentBatchSubscribe = AgentClient -> [ConnId] -> ExceptT AgentErrorType IO (Map ConnId (Either AgentErrorType ()))

subscribeUserConnections :: VersionRangeChat -> Bool -> AgentBatchSubscribe -> User -> CM ()
subscribeUserConnections vr onlyNeeded agentBatchSubscribe user = do
  -- get user connections
  ce <- asks $ subscriptionEvents . config
  (conns, cts, ucs, gs, ms, sfts, rfts, pcs) <-
    if onlyNeeded
      then do
        (conns, entities) <- withStore' (`getConnectionsToSubscribe` vr)
        let (cts, ucs, ms, sfts, rfts, pcs) = foldl' addEntity (M.empty, M.empty, M.empty, M.empty, M.empty, M.empty) entities
        pure (conns, cts, ucs, [], ms, sfts, rfts, pcs)
      else do
        withStore' unsetConnectionToSubscribe
        (ctConns, cts) <- getContactConns
        (ucConns, ucs) <- getUserContactLinkConns
        (gs, mConns, ms) <- getGroupMemberConns
        (sftConns, sfts) <- getSndFileTransferConns
        (rftConns, rfts) <- getRcvFileTransferConns
        (pcConns, pcs) <- getPendingContactConns
        let conns = concat ([ctConns, ucConns, mConns, sftConns, rftConns, pcConns] :: [[ConnId]])
        pure (conns, cts, ucs, gs, ms, sfts, rfts, pcs)
  -- subscribe using batched commands
  rs <- withAgent $ \a -> agentBatchSubscribe a conns
  -- send connection events to view
  contactSubsToView rs cts ce
  -- TODO possibly, we could either disable these events or replace with less noisy for API
  contactLinkSubsToView rs ucs
  groupSubsToView rs gs ms ce
  sndFileSubsToView rs sfts
  rcvFileSubsToView rs rfts
  pendingConnSubsToView rs pcs
  where
    addEntity (cts, ucs, ms, sfts, rfts, pcs) = \case
      RcvDirectMsgConnection c (Just ct) -> let cts' = addConn c ct cts in (cts', ucs, ms, sfts, rfts, pcs)
      RcvDirectMsgConnection c Nothing -> let pcs' = addConn c (toPCC c) pcs in (cts, ucs, ms, sfts, rfts, pcs')
      RcvGroupMsgConnection c _g m -> let ms' = addConn c m ms in (cts, ucs, ms', sfts, rfts, pcs)
      SndFileConnection c sft -> let sfts' = addConn c sft sfts in (cts, ucs, ms, sfts', rfts, pcs)
      RcvFileConnection c rft -> let rfts' = addConn c rft rfts in (cts, ucs, ms, sfts, rfts', pcs)
      UserContactConnection c uc -> let ucs' = addConn c uc ucs in (cts, ucs', ms, sfts, rfts, pcs)
    addConn :: Connection -> a -> Map ConnId a -> Map ConnId a
    addConn = M.insert . aConnId
    toPCC Connection {connId, agentConnId, connStatus, viaUserContactLink, groupLinkId, customUserProfileId, localAlias, createdAt} =
      PendingContactConnection
        { pccConnId = connId,
          pccAgentConnId = agentConnId,
          pccConnStatus = connStatus,
          viaContactUri = False,
          viaUserContactLink,
          groupLinkId,
          customUserProfileId,
          connReqInv = Nothing,
          localAlias,
          createdAt,
          updatedAt = createdAt
        }
    getContactConns :: CM ([ConnId], Map ConnId Contact)
    getContactConns = do
      cts <- withStore_ (`getUserContacts` vr)
      let cts' = mapMaybe (\ct -> (,ct) <$> contactConnId ct) $ filter contactActive cts
      pure (map fst cts', M.fromList cts')
    getUserContactLinkConns :: CM ([ConnId], Map ConnId UserContact)
    getUserContactLinkConns = do
      (cs, ucs) <- unzip <$> withStore_ (`getUserContactLinks` vr)
      let connIds = map aConnId cs
      pure (connIds, M.fromList $ zip connIds ucs)
    getGroupMemberConns :: CM ([Group], [ConnId], Map ConnId GroupMember)
    getGroupMemberConns = do
      gs <- withStore_ (`getUserGroups` vr)
      let mPairs = concatMap (\(Group _ ms) -> mapMaybe (\m -> (,m) <$> memberConnId m) (filter (not . memberRemoved) ms)) gs
      pure (gs, map fst mPairs, M.fromList mPairs)
    getSndFileTransferConns :: CM ([ConnId], Map ConnId SndFileTransfer)
    getSndFileTransferConns = do
      sfts <- withStore_ getLiveSndFileTransfers
      let connIds = map sndFileTransferConnId sfts
      pure (connIds, M.fromList $ zip connIds sfts)
    getRcvFileTransferConns :: CM ([ConnId], Map ConnId RcvFileTransfer)
    getRcvFileTransferConns = do
      rfts <- withStore_ getLiveRcvFileTransfers
      let rftPairs = mapMaybe (\ft -> (,ft) <$> liveRcvFileTransferConnId ft) rfts
      pure (map fst rftPairs, M.fromList rftPairs)
    getPendingContactConns :: CM ([ConnId], Map ConnId PendingContactConnection)
    getPendingContactConns = do
      pcs <- withStore_ getPendingContactConnections
      let connIds = map aConnId' pcs
      pure (connIds, M.fromList $ zip connIds pcs)
    contactSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId Contact -> Bool -> CM ()
    contactSubsToView rs cts ce = do
      chatModifyVar connNetworkStatuses $ M.union (M.fromList statuses)
      ifM (asks $ coreApi . config) (notifyAPI statuses) notifyCLI
      where
        notifyCLI = do
          let cRs = resultsFor rs cts
              cErrors = sortOn (\(Contact {localDisplayName = n}, _) -> n) $ filterErrors cRs
          toView . CRContactSubSummary user $ map (uncurry ContactSubStatus) cRs
          when ce $ mapM_ (toView . uncurry (CRContactSubError user)) cErrors
        notifyAPI = toView . CRNetworkStatuses (Just user) . map (uncurry ConnNetworkStatus)
        statuses = M.foldrWithKey' addStatus [] cts
          where
            addStatus :: ConnId -> Contact -> [(AgentConnId, NetworkStatus)] -> [(AgentConnId, NetworkStatus)]
            addStatus _ Contact {activeConn = Nothing} nss = nss
            addStatus connId Contact {activeConn = Just Connection {agentConnId}} nss =
              let ns = (agentConnId, netStatus $ resultErr connId rs)
               in ns : nss
            netStatus :: Maybe ChatError -> NetworkStatus
            netStatus = maybe NSConnected $ NSError . errorNetworkStatus
            errorNetworkStatus :: ChatError -> String
            errorNetworkStatus = \case
              ChatErrorAgent (BROKER _ NETWORK) _ -> "network"
              ChatErrorAgent (SMP _ SMP.AUTH) _ -> "contact deleted"
              e -> show e
    -- TODO possibly below could be replaced with less noisy events for API
    contactLinkSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId UserContact -> CM ()
    contactLinkSubsToView rs = toView . CRUserContactSubSummary user . map (uncurry UserContactSubStatus) . resultsFor rs
    groupSubsToView :: Map ConnId (Either AgentErrorType ()) -> [Group] -> Map ConnId GroupMember -> Bool -> CM ()
    groupSubsToView rs gs ms ce = do
      mapM_ groupSub $
        sortOn (\(Group GroupInfo {localDisplayName = g} _) -> g) gs
      toView . CRMemberSubSummary user $ map (uncurry MemberSubStatus) mRs
      where
        mRs = resultsFor rs ms
        groupSub :: Group -> CM ()
        groupSub (Group g@GroupInfo {membership, groupId = gId} members) = do
          when ce $ mapM_ (toView . uncurry (CRMemberSubError user g)) mErrors
          toView groupEvent
          where
            mErrors :: [(GroupMember, ChatError)]
            mErrors =
              sortOn (\(GroupMember {localDisplayName = n}, _) -> n)
                . filterErrors
                $ filter (\(GroupMember {groupId}, _) -> groupId == gId) mRs
            groupEvent :: ChatResponse
            groupEvent
              | memberStatus membership == GSMemInvited = CRGroupInvitation user g
              | all (\GroupMember {activeConn} -> isNothing activeConn) members =
                  if memberActive membership
                    then CRGroupEmpty user g
                    else CRGroupRemoved user g
              | otherwise = CRGroupSubscribed user g
    sndFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId SndFileTransfer -> CM ()
    sndFileSubsToView rs sfts = do
      let sftRs = resultsFor rs sfts
      forM_ sftRs $ \(ft@SndFileTransfer {fileId, fileStatus}, err_) -> do
        forM_ err_ $ toView . CRSndFileSubError user ft
        void . forkIO $ do
          threadDelay 1000000
          when (fileStatus == FSConnected) . unlessM (isFileActive fileId sndFiles) . withChatLock "subscribe sendFileChunk" $
            sendFileChunk user ft
    rcvFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId RcvFileTransfer -> CM ()
    rcvFileSubsToView rs = mapM_ (toView . uncurry (CRRcvFileSubError user)) . filterErrors . resultsFor rs
    pendingConnSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId PendingContactConnection -> CM ()
    pendingConnSubsToView rs = toView . CRPendingSubSummary user . map (uncurry PendingSubStatus) . resultsFor rs
    withStore_ :: (DB.Connection -> User -> IO [a]) -> CM [a]
    withStore_ a = withStore' (`a` user) `catchChatError` \e -> toView (CRChatError (Just user) e) $> []
    filterErrors :: [(a, Maybe ChatError)] -> [(a, ChatError)]
    filterErrors = mapMaybe (\(a, e_) -> (a,) <$> e_)
    resultsFor :: Map ConnId (Either AgentErrorType ()) -> Map ConnId a -> [(a, Maybe ChatError)]
    resultsFor rs = M.foldrWithKey' addResult []
      where
        addResult :: ConnId -> a -> [(a, Maybe ChatError)] -> [(a, Maybe ChatError)]
        addResult connId = (:) . (,resultErr connId rs)
    resultErr :: ConnId -> Map ConnId (Either AgentErrorType ()) -> Maybe ChatError
    resultErr connId rs = case M.lookup connId rs of
      Just (Left e) -> Just $ ChatErrorAgent e Nothing
      Just _ -> Nothing
      _ -> Just . ChatError . CEAgentNoSubResult $ AgentConnId connId

cleanupManager :: CM ()
cleanupManager = do
  interval <- asks (cleanupManagerInterval . config)
  runWithoutInitialDelay interval
  initialDelay <- asks (initialCleanupManagerDelay . config)
  liftIO $ threadDelay' initialDelay
  stepDelay <- asks (cleanupManagerStepDelay . config)
  forever $ do
    flip catchChatError (toView . CRChatError Nothing) $ do
      lift waitChatStartedAndActivated
      users <- withStore' getUsers
      let (us, us') = partition activeUser users
      forM_ us $ cleanupUser interval stepDelay
      forM_ us' $ cleanupUser interval stepDelay
      cleanupMessages `catchChatError` (toView . CRChatError Nothing)
      -- TODO possibly, also cleanup async commands
      cleanupProbes `catchChatError` (toView . CRChatError Nothing)
    liftIO $ threadDelay' $ diffToMicroseconds interval
  where
    runWithoutInitialDelay cleanupInterval = flip catchChatError (toView . CRChatError Nothing) $ do
      lift waitChatStartedAndActivated
      users <- withStore' getUsers
      let (us, us') = partition activeUser users
      forM_ us $ \u -> cleanupTimedItems cleanupInterval u `catchChatError` (toView . CRChatError (Just u))
      forM_ us' $ \u -> cleanupTimedItems cleanupInterval u `catchChatError` (toView . CRChatError (Just u))
    cleanupUser cleanupInterval stepDelay user = do
      cleanupTimedItems cleanupInterval user `catchChatError` (toView . CRChatError (Just user))
      liftIO $ threadDelay' stepDelay
      cleanupDeletedContacts user `catchChatError` (toView . CRChatError (Just user))
      liftIO $ threadDelay' stepDelay
    cleanupTimedItems cleanupInterval user = do
      ts <- liftIO getCurrentTime
      let startTimedThreadCutoff = addUTCTime cleanupInterval ts
      timedItems <- withStore' $ \db -> getTimedItems db user startTimedThreadCutoff
      forM_ timedItems $ \(itemRef, deleteAt) -> startTimedItemThread user itemRef deleteAt `catchChatError` const (pure ())
    cleanupDeletedContacts user = do
      vr <- chatVersionRange
      contacts <- withStore' $ \db -> getDeletedContacts db vr user
      forM_ contacts $ \ct ->
        withStore (\db -> deleteContactWithoutGroups db user ct)
          `catchChatError` (toView . CRChatError (Just user))
    cleanupMessages = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(30 * nominalDay)) ts
      withStore' (`deleteOldMessages` cutoffTs)
    cleanupProbes = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(14 * nominalDay)) ts
      withStore' (`deleteOldProbes` cutoffTs)

startProximateTimedItemThread :: User -> (ChatRef, ChatItemId) -> UTCTime -> CM ()
startProximateTimedItemThread user itemRef deleteAt = do
  interval <- asks (cleanupManagerInterval . config)
  ts <- liftIO getCurrentTime
  when (diffUTCTime deleteAt ts <= interval) $
    startTimedItemThread user itemRef deleteAt

startTimedItemThread :: User -> (ChatRef, ChatItemId) -> UTCTime -> CM ()
startTimedItemThread user itemRef deleteAt = do
  itemThreads <- asks timedItemThreads
  threadTVar_ <- atomically $ do
    exists <- TM.member itemRef itemThreads
    if not exists
      then do
        threadTVar <- newTVar Nothing
        TM.insert itemRef threadTVar itemThreads
        pure $ Just threadTVar
      else pure Nothing
  forM_ threadTVar_ $ \threadTVar -> do
    tId <- mkWeakThreadId =<< deleteTimedItem user itemRef deleteAt `forkFinally` const (atomically $ TM.delete itemRef itemThreads)
    atomically $ writeTVar threadTVar (Just tId)

deleteTimedItem :: User -> (ChatRef, ChatItemId) -> UTCTime -> CM ()
deleteTimedItem user (ChatRef cType chatId, itemId) deleteAt = do
  ts <- liftIO getCurrentTime
  liftIO $ threadDelay' $ diffToMicroseconds $ diffUTCTime deleteAt ts
  lift waitChatStartedAndActivated
  vr <- chatVersionRange
  case cType of
    CTDirect -> do
      (ct, ci) <- withStore $ \db -> (,) <$> getContact db vr user chatId <*> getDirectChatItem db user chatId itemId
      deleteDirectCIs user ct [ci] True True >>= toView
    CTGroup -> do
      (gInfo, ci) <- withStore $ \db -> (,) <$> getGroupInfo db vr user chatId <*> getGroupChatItem db user chatId itemId
      deletedTs <- liftIO getCurrentTime
      deleteGroupCIs user gInfo [ci] True True Nothing deletedTs >>= toView
    _ -> toView . CRChatError (Just user) . ChatError $ CEInternalError "bad deleteTimedItem cType"

startUpdatedTimedItemThread :: User -> ChatRef -> ChatItem c d -> ChatItem c d -> CM ()
startUpdatedTimedItemThread user chatRef ci ci' =
  case (chatItemTimed ci >>= timedDeleteAt', chatItemTimed ci' >>= timedDeleteAt') of
    (Nothing, Just deleteAt') ->
      startProximateTimedItemThread user (chatRef, chatItemId' ci') deleteAt'
    _ -> pure ()

expireChatItems :: User -> Int64 -> Bool -> CM ()
expireChatItems user@User {userId} ttl sync = do
  currentTs <- liftIO getCurrentTime
  vr <- chatVersionRange
  let expirationDate = addUTCTime (-1 * fromIntegral ttl) currentTs
      -- this is to keep group messages created during last 12 hours even if they're expired according to item_ts
      createdAtCutoff = addUTCTime (-43200 :: NominalDiffTime) currentTs
  lift waitChatStartedAndActivated
  contacts <- withStore' $ \db -> getUserContacts db vr user
  loop contacts $ processContact expirationDate
  lift waitChatStartedAndActivated
  groups <- withStore' $ \db -> getUserGroupDetails db vr user Nothing Nothing
  loop groups $ processGroup vr expirationDate createdAtCutoff
  where
    loop :: [a] -> (a -> CM ()) -> CM ()
    loop [] _ = pure ()
    loop (a : as) process = continue $ do
      process a `catchChatError` (toView . CRChatError (Just user))
      loop as process
    continue :: CM () -> CM ()
    continue a =
      if sync
        then a
        else do
          expireFlags <- asks expireCIFlags
          expire <- atomically $ TM.lookup userId expireFlags
          when (expire == Just True) $ threadDelay 100000 >> a
    processContact :: UTCTime -> Contact -> CM ()
    processContact expirationDate ct = do
      lift waitChatStartedAndActivated
      filesInfo <- withStore' $ \db -> getContactExpiredFileInfo db user ct expirationDate
      cancelFilesInProgress user filesInfo
      deleteFilesLocally filesInfo
      withStore' $ \db -> deleteContactExpiredCIs db user ct expirationDate
    processGroup :: VersionRangeChat -> UTCTime -> UTCTime -> GroupInfo -> CM ()
    processGroup vr expirationDate createdAtCutoff gInfo = do
      lift waitChatStartedAndActivated
      filesInfo <- withStore' $ \db -> getGroupExpiredFileInfo db user gInfo expirationDate createdAtCutoff
      cancelFilesInProgress user filesInfo
      deleteFilesLocally filesInfo
      withStore' $ \db -> deleteGroupExpiredCIs db user gInfo expirationDate createdAtCutoff
      membersToDelete <- withStore' $ \db -> getGroupMembersForExpiration db vr user gInfo
      forM_ membersToDelete $ \m -> withStore' $ \db -> deleteGroupMember db user m

processAgentMessage :: ACorrId -> ConnId -> AEvent 'AEConn -> CM ()
processAgentMessage _ connId (DEL_RCVQ srv qId err_) =
  toView $ CRAgentRcvQueueDeleted (AgentConnId connId) srv (AgentQueueId qId) err_
processAgentMessage _ connId DEL_CONN =
  toView $ CRAgentConnDeleted (AgentConnId connId)
processAgentMessage _ "" (ERR e) =
  toView $ CRChatError Nothing $ ChatErrorAgent e Nothing
processAgentMessage corrId connId msg = do
  lockEntity <- critical (withStore (`getChatLockEntity` AgentConnId connId))
  withEntityLock "processAgentMessage" lockEntity $ do
    vr <- chatVersionRange
    -- getUserByAConnId never throws logical errors, only SEDBBusyError can be thrown here
    critical (withStore' (`getUserByAConnId` AgentConnId connId)) >>= \case
      Just user -> processAgentMessageConn vr user corrId connId msg `catchChatError` (toView . CRChatError (Just user))
      _ -> throwChatError $ CENoConnectionUser (AgentConnId connId)

-- CRITICAL error will be shown to the user as alert with restart button in Android/desktop apps.
-- SEDBBusyError will only be thrown on IO exceptions or SQLError during DB queries,
-- e.g. when database is locked or busy for longer than 3s.
-- In this case there is no better mitigation than showing alert:
-- - without ACK the message delivery will be stuck,
-- - with ACK message will be lost, as it failed to be saved.
-- Full app restart is likely to resolve database condition and the message will be received and processed again.
critical :: CM a -> CM a
critical a =
  a `catchChatError` \case
    ChatErrorStore SEDBBusyError {message} -> throwError $ ChatErrorAgent (CRITICAL True message) Nothing
    e -> throwError e

processAgentMessageNoConn :: AEvent 'AENone -> CM ()
processAgentMessageNoConn = \case
  CONNECT p h -> hostEvent $ CRHostConnected p h
  DISCONNECT p h -> hostEvent $ CRHostDisconnected p h
  DOWN srv conns -> serverEvent srv conns NSDisconnected CRContactsDisconnected
  UP srv conns -> serverEvent srv conns NSConnected CRContactsSubscribed
  SUSPENDED -> toView CRChatSuspended
  DEL_USER agentUserId -> toView $ CRAgentUserDeleted agentUserId
  ERRS cErrs -> errsEvent cErrs
  where
    hostEvent :: ChatResponse -> CM ()
    hostEvent = whenM (asks $ hostEvents . config) . toView
    serverEvent srv conns nsStatus event = do
      chatModifyVar connNetworkStatuses $ \m -> foldl' (\m' cId -> M.insert cId nsStatus m') m connIds
      ifM (asks $ coreApi . config) (notifyAPI connIds) notifyCLI
      where
        connIds = map AgentConnId conns
        notifyAPI = toView . CRNetworkStatus nsStatus
        notifyCLI = do
          cs <- withStore' (`getConnectionsContacts` conns)
          toView $ event srv cs
    errsEvent :: [(ConnId, AgentErrorType)] -> CM ()
    errsEvent cErrs = do
      vr <- chatVersionRange
      errs <- lift $ rights <$> withStoreBatch' (\db -> map (getChatErr vr db) cErrs)
      toView $ CRChatErrors Nothing errs
      where
        getChatErr :: VersionRangeChat -> DB.Connection -> (ConnId, AgentErrorType) -> IO ChatError
        getChatErr vr db (connId, err) =
          let acId = AgentConnId connId
           in ChatErrorAgent err <$> (getUserByAConnId db acId $>>= \user -> eitherToMaybe <$> runExceptT (getConnectionEntity db vr user acId))

processAgentMsgSndFile :: ACorrId -> SndFileId -> AEvent 'AESndFile -> CM ()
processAgentMsgSndFile _corrId aFileId msg = do
  (cRef_, fileId) <- withStore (`getXFTPSndFileDBIds` AgentSndFileId aFileId)
  withEntityLock_ cRef_ . withFileLock "processAgentMsgSndFile" fileId $
    withStore' (`getUserByASndFileId` AgentSndFileId aFileId) >>= \case
      Just user -> process user fileId `catchChatError` (toView . CRChatError (Just user))
      _ -> do
        lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
        throwChatError $ CENoSndFileUser $ AgentSndFileId aFileId
  where
    withEntityLock_ :: Maybe ChatRef -> CM a -> CM a
    withEntityLock_ cRef_ = case cRef_ of
      Just (ChatRef CTDirect contactId) -> withContactLock "processAgentMsgSndFile" contactId
      Just (ChatRef CTGroup groupId) -> withGroupLock "processAgentMsgSndFile" groupId
      _ -> id
    process :: User -> FileTransferId -> CM ()
    process user fileId = do
      (ft@FileTransferMeta {xftpRedirectFor, cancelled}, sfts) <- withStore $ \db -> getSndFileTransfer db user fileId
      vr <- chatVersionRange
      unless cancelled $ case msg of
        SFPROG sndProgress sndTotal -> do
          let status = CIFSSndTransfer {sndProgress, sndTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            lookupChatItemByFileId db vr user fileId
          toView $ CRSndFileProgressXFTP user ci ft sndProgress sndTotal
        SFDONE sndDescr rfds -> do
          withStore' $ \db -> setSndFTPrivateSndDescr db user fileId (fileDescrText sndDescr)
          ci <- withStore $ \db -> lookupChatItemByFileId db vr user fileId
          case ci of
            Nothing -> do
              lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
              withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText rfds)
              case rfds of
                [] -> sendFileError (FileErrOther "no receiver descriptions") "no receiver descriptions" vr ft
                rfd : _ -> case [fd | fd@(FD.ValidFileDescription FD.FileDescription {chunks = [_]}) <- rfds] of
                  [] -> case xftpRedirectFor of
                    Nothing -> xftpSndFileRedirect user fileId rfd >>= toView . CRSndFileRedirectStartXFTP user ft
                    Just _ -> sendFileError (FileErrOther "chaining redirects") "Prohibit chaining redirects" vr ft
                  rfds' -> do
                    -- we have 1 chunk - use it as URI whether it is redirect or not
                    ft' <- maybe (pure ft) (\fId -> withStore $ \db -> getFileTransferMeta db user fId) xftpRedirectFor
                    toView $ CRSndStandaloneFileComplete user ft' $ map (decodeLatin1 . strEncode . FD.fileDescriptionURI) rfds'
            Just (AChatItem _ d cInfo _ci@ChatItem {meta = CIMeta {itemSharedMsgId = msgId_, itemDeleted}}) ->
              case (msgId_, itemDeleted) of
                (Just sharedMsgId, Nothing) -> do
                  when (length rfds < length sfts) $ throwChatError $ CEInternalError "not enough XFTP file descriptions to send"
                  -- TODO either update database status or move to SFPROG
                  toView $ CRSndFileProgressXFTP user ci ft 1 1
                  case (rfds, sfts, d, cInfo) of
                    (rfd : extraRFDs, sft : _, SMDSnd, DirectChat ct) -> do
                      withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText extraRFDs)
                      conn@Connection {connId} <- liftEither $ contactSendConn_ ct
                      sendFileDescriptions (ConnectionId connId) ((conn, sft, fileDescrText rfd) :| []) sharedMsgId >>= \case
                        Just rs -> case L.last rs of
                          Right ([msgDeliveryId], _) ->
                            withStore' $ \db -> updateSndFTDeliveryXFTP db sft msgDeliveryId
                          Right (deliveryIds, _) -> toView $ CRChatError (Just user) $ ChatError $ CEInternalError $ "SFDONE, sendFileDescriptions: expected 1 delivery id, got " <> show (length deliveryIds)
                          Left e -> toView $ CRChatError (Just user) e
                        Nothing -> toView $ CRChatError (Just user) $ ChatError $ CEInternalError "SFDONE, sendFileDescriptions: expected at least 1 result"
                      lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
                    (_, _, SMDSnd, GroupChat g@GroupInfo {groupId}) -> do
                      ms <- withStore' $ \db -> getGroupMembers db vr user g
                      let rfdsMemberFTs = zipWith (\rfd (conn, sft) -> (conn, sft, fileDescrText rfd)) rfds (memberFTs ms)
                          extraRFDs = drop (length rfdsMemberFTs) rfds
                      withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText extraRFDs)
                      forM_ (L.nonEmpty rfdsMemberFTs) $ \rfdsMemberFTs' ->
                        sendFileDescriptions (GroupId groupId) rfdsMemberFTs' sharedMsgId
                      ci' <- withStore $ \db -> do
                        liftIO $ updateCIFileStatus db user fileId CIFSSndComplete
                        getChatItemByFileId db vr user fileId
                      lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
                      toView $ CRSndFileCompleteXFTP user ci' ft
                      where
                        memberFTs :: [GroupMember] -> [(Connection, SndFileTransfer)]
                        memberFTs ms = M.elems $ M.intersectionWith (,) (M.fromList mConns') (M.fromList sfts')
                          where
                            mConns' = mapMaybe useMember ms
                            sfts' = mapMaybe (\sft@SndFileTransfer {groupMemberId} -> (,sft) <$> groupMemberId) sfts
                            -- Should match memberSendAction logic
                            useMember GroupMember {groupMemberId, activeConn = Just conn@Connection {connStatus}}
                              | (connStatus == ConnReady || connStatus == ConnSndReady) && not (connDisabled conn) && not (connInactive conn) =
                                  Just (groupMemberId, conn)
                              | otherwise = Nothing
                            useMember _ = Nothing
                    _ -> pure ()
                _ -> pure () -- TODO error?
        SFWARN e -> do
          let err = tshow e
          logWarn $ "Sent file warning: " <> err
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId (CIFSSndWarning $ agentFileError e)
            lookupChatItemByFileId db vr user fileId
          toView $ CRSndFileWarning user ci ft err
        SFERR e ->
          sendFileError (agentFileError e) (tshow e) vr ft
      where
        fileDescrText :: FilePartyI p => ValidFileDescription p -> T.Text
        fileDescrText = safeDecodeUtf8 . strEncode
        sendFileDescriptions :: ConnOrGroupId -> NonEmpty (Connection, SndFileTransfer, RcvFileDescrText) -> SharedMsgId -> CM (Maybe (NonEmpty (Either ChatError ([Int64], PQEncryption))))
        sendFileDescriptions connOrGroupId connsTransfersDescrs sharedMsgId = do
          lift . void . withStoreBatch' $ \db -> L.map (\(_, sft, rfdText) -> updateSndFTDescrXFTP db user sft rfdText) connsTransfersDescrs
          partSize <- asks $ xftpDescrPartSize . config
          let connsIdsEvts = connDescrEvents partSize
          sndMsgs_ <- lift $ createSndMessages $ L.map snd connsIdsEvts
          let (errs, msgReqs) = partitionEithers . L.toList $ L.zipWith (fmap . toMsgReq) connsIdsEvts sndMsgs_
          delivered <- mapM deliverMessages (L.nonEmpty msgReqs)
          let errs' = errs <> maybe [] (lefts . L.toList) delivered
          unless (null errs') $ toView $ CRChatErrors (Just user) errs'
          pure delivered
          where
            connDescrEvents :: Int -> NonEmpty (Connection, (ConnOrGroupId, ChatMsgEvent 'Json))
            connDescrEvents partSize = L.fromList $ concatMap splitText (L.toList connsTransfersDescrs)
              where
                splitText :: (Connection, SndFileTransfer, RcvFileDescrText) -> [(Connection, (ConnOrGroupId, ChatMsgEvent 'Json))]
                splitText (conn, _, rfdText) =
                  map (\fileDescr -> (conn, (connOrGroupId, XMsgFileDescr {msgId = sharedMsgId, fileDescr}))) (L.toList $ splitFileDescr partSize rfdText)
            toMsgReq :: (Connection, (ConnOrGroupId, ChatMsgEvent 'Json)) -> SndMessage -> ChatMsgReq
            toMsgReq (conn, _) SndMessage {msgId, msgBody} =
              (conn, MsgFlags {notification = hasNotification XMsgFileDescr_}, msgBody, [msgId])
        sendFileError :: FileError -> Text -> VersionRangeChat -> FileTransferMeta -> CM ()
        sendFileError ferr err vr ft = do
          logError $ "Sent file error: " <> err
          ci <- withStore $ \db -> do
            liftIO $ updateFileCancelled db user fileId (CIFSSndError ferr)
            lookupChatItemByFileId db vr user fileId
          lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
          toView $ CRSndFileError user ci ft err

agentFileError :: AgentErrorType -> FileError
agentFileError = \case
  XFTP _ XFTP.AUTH -> FileErrAuth
  FILE NO_FILE -> FileErrNoFile
  BROKER _ e -> brokerError FileErrRelay e
  e -> FileErrOther $ tshow e
  where
    brokerError srvErr = \case
      HOST -> srvErr SrvErrHost
      SMP.TRANSPORT TEVersion -> srvErr SrvErrVersion
      e -> srvErr . SrvErrOther $ tshow e

splitFileDescr :: Int -> RcvFileDescrText -> NonEmpty FileDescr
splitFileDescr partSize rfdText = splitParts 1 rfdText
  where
    splitParts partNo remText =
      let (part, rest) = T.splitAt partSize remText
          complete = T.null rest
          fileDescr = FileDescr {fileDescrText = part, fileDescrPartNo = partNo, fileDescrComplete = complete}
       in if complete
            then fileDescr :| []
            else fileDescr <| splitParts (partNo + 1) rest

processAgentMsgRcvFile :: ACorrId -> RcvFileId -> AEvent 'AERcvFile -> CM ()
processAgentMsgRcvFile _corrId aFileId msg = do
  (cRef_, fileId) <- withStore (`getXFTPRcvFileDBIds` AgentRcvFileId aFileId)
  withEntityLock_ cRef_ . withFileLock "processAgentMsgRcvFile" fileId $
    withStore' (`getUserByARcvFileId` AgentRcvFileId aFileId) >>= \case
      Just user -> process user fileId `catchChatError` (toView . CRChatError (Just user))
      _ -> do
        lift $ withAgent' (`xftpDeleteRcvFile` aFileId)
        throwChatError $ CENoRcvFileUser $ AgentRcvFileId aFileId
  where
    withEntityLock_ :: Maybe ChatRef -> CM a -> CM a
    withEntityLock_ cRef_ = case cRef_ of
      Just (ChatRef CTDirect contactId) -> withContactLock "processAgentMsgRcvFile" contactId
      Just (ChatRef CTGroup groupId) -> withGroupLock "processAgentMsgRcvFile" groupId
      _ -> id
    process :: User -> FileTransferId -> CM ()
    process user fileId = do
      ft <- withStore $ \db -> getRcvFileTransfer db user fileId
      vr <- chatVersionRange
      unless (rcvFileCompleteOrCancelled ft) $ case msg of
        RFPROG rcvProgress rcvTotal -> do
          let status = CIFSRcvTransfer {rcvProgress, rcvTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            lookupChatItemByFileId db vr user fileId
          toView $ CRRcvFileProgressXFTP user ci rcvProgress rcvTotal ft
        RFDONE xftpPath ->
          case liveRcvFileTransferPath ft of
            Nothing -> throwChatError $ CEInternalError "no target path for received XFTP file"
            Just targetPath -> do
              fsTargetPath <- lift $ toFSFilePath targetPath
              renameFile xftpPath fsTargetPath
              ci_ <- withStore $ \db -> do
                liftIO $ do
                  updateRcvFileStatus db fileId FSComplete
                  updateCIFileStatus db user fileId CIFSRcvComplete
                lookupChatItemByFileId db vr user fileId
              agentXFTPDeleteRcvFile aFileId fileId
              toView $ maybe (CRRcvStandaloneFileComplete user fsTargetPath ft) (CRRcvFileComplete user) ci_
        RFWARN e -> do
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId (CIFSRcvWarning $ agentFileError e)
            lookupChatItemByFileId db vr user fileId
          toView $ CRRcvFileWarning user ci e ft
        RFERR e
          | e == FILE NOT_APPROVED -> do
              aci_ <- resetRcvCIFileStatus user fileId CIFSRcvAborted
              forM_ aci_ cleanupACIFile
              agentXFTPDeleteRcvFile aFileId fileId
              forM_ aci_ $ \aci -> toView $ CRChatItemUpdated user aci
          | otherwise -> do
              aci_ <- withStore $ \db -> do
                liftIO $ updateFileCancelled db user fileId (CIFSRcvError $ agentFileError e)
                lookupChatItemByFileId db vr user fileId
              forM_ aci_ cleanupACIFile
              agentXFTPDeleteRcvFile aFileId fileId
              toView $ CRRcvFileError user aci_ e ft

cleanupACIFile :: AChatItem -> CM ()
cleanupACIFile (AChatItem _ _ _ ChatItem {file = Just CIFile {fileSource = Just CryptoFile {filePath}}}) = do
  fsFilePath <- lift $ toFSFilePath filePath
  removeFile fsFilePath `catchChatError` \_ -> pure ()
cleanupACIFile _ = pure ()

processAgentMessageConn :: VersionRangeChat -> User -> ACorrId -> ConnId -> AEvent 'AEConn -> CM ()
processAgentMessageConn vr user@User {userId} corrId agentConnId agentMessage = do
  -- Missing connection/entity errors here will be sent to the view but not shown as CRITICAL alert,
  -- as in this case no need to ACK message - we can't process messages for this connection anyway.
  -- SEDBException will be re-trown as CRITICAL as it is likely to indicate a temporary database condition
  -- that will be resolved with app restart.
  entity <- critical $ withStore (\db -> getConnectionEntity db vr user $ AgentConnId agentConnId) >>= updateConnStatus
  case agentMessage of
    END -> case entity of
      RcvDirectMsgConnection _ (Just ct) -> toView $ CRContactAnotherClient user ct
      _ -> toView $ CRSubscriptionEnd user entity
    MSGNTF msgId msgTs_ -> toView $ CRNtfMessage user entity $ ntfMsgAckInfo msgId msgTs_
    _ -> case entity of
      RcvDirectMsgConnection conn contact_ ->
        processDirectMessage agentMessage entity conn contact_
      RcvGroupMsgConnection conn gInfo m ->
        processGroupMessage agentMessage entity conn gInfo m
      RcvFileConnection conn ft ->
        processRcvFileConn agentMessage entity conn ft
      SndFileConnection conn ft ->
        processSndFileConn agentMessage entity conn ft
      UserContactConnection conn uc ->
        processUserContactRequest agentMessage entity conn uc
  where
    updateConnStatus :: ConnectionEntity -> CM ConnectionEntity
    updateConnStatus acEntity = case agentMsgConnStatus agentMessage of
      Just connStatus -> do
        let conn = (entityConnection acEntity) {connStatus}
        withStore' $ \db -> updateConnectionStatus db conn connStatus
        pure $ updateEntityConnStatus acEntity connStatus
      Nothing -> pure acEntity

    agentMsgConnStatus :: AEvent e -> Maybe ConnStatus
    agentMsgConnStatus = \case
      JOINED True -> Just ConnSndReady
      CONF {} -> Just ConnRequested
      INFO {} -> Just ConnSndReady
      CON _ -> Just ConnReady
      _ -> Nothing

    processCONFpqSupport :: Connection -> PQSupport -> CM Connection
    processCONFpqSupport conn@Connection {connId, pqSupport = pq} pq'
      | pq == PQSupportOn && pq' == PQSupportOff = do
          let pqEnc' = CR.pqSupportToEnc pq'
          withStore' $ \db -> updateConnSupportPQ db connId pq' pqEnc'
          pure (conn {pqSupport = pq', pqEncryption = pqEnc'} :: Connection)
      | pq /= pq' = do
          messageWarning "processCONFpqSupport: unexpected pqSupport change"
          pure conn
      | otherwise = pure conn

    processINFOpqSupport :: Connection -> PQSupport -> CM ()
    processINFOpqSupport Connection {pqSupport = pq} pq' =
      when (pq /= pq') $ messageWarning "processINFOpqSupport: unexpected pqSupport change"

    processDirectMessage :: AEvent e -> ConnectionEntity -> Connection -> Maybe Contact -> CM ()
    processDirectMessage agentMsg connEntity conn@Connection {connId, connChatVersion, peerChatVRange, viaUserContactLink, customUserProfileId, connectionCode} = \case
      Nothing -> case agentMsg of
        CONF confId pqSupport _ connInfo -> do
          conn' <- processCONFpqSupport conn pqSupport
          -- [incognito] send saved profile
          (conn'', inGroup) <- saveConnInfo conn' connInfo
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
          let profileToSend = userProfileToSend user (fromLocalProfile <$> incognitoProfile) Nothing inGroup
          -- [async agent commands] no continuation needed, but command should be asynchronous for stability
          allowAgentConnectionAsync user conn'' confId $ XInfo profileToSend
        INFO pqSupport connInfo -> do
          processINFOpqSupport conn pqSupport
          void $ saveConnInfo conn connInfo
        MSG meta _msgFlags _msgBody ->
          -- We are not saving message (saveDirectRcvMSG) as contact hasn't been created yet,
          -- chat item is also not created here
          withAckMessage' "new contact msg" agentConnId meta $ pure ()
        SENT msgId _proxy -> do
          void $ continueSending connEntity conn
          sentMsgDeliveryEvent conn msgId
        OK ->
          -- [async agent commands] continuation on receiving OK
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        JOINED _ ->
          -- [async agent commands] continuation on receiving JOINED
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        QCONT ->
          void $ continueSending connEntity conn
        MWARN _ err ->
          processConnMWARN connEntity conn err
        MERR _ err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          processConnMERR connEntity conn err
        MERRS _ err -> do
          -- error cannot be AUTH error here
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        ERR err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()
      Just ct@Contact {contactId} -> case agentMsg of
        INV (ACR _ cReq) ->
          -- [async agent commands] XGrpMemIntro continuation on receiving INV
          withCompletedCommand conn agentMsg $ \_ ->
            case cReq of
              directConnReq@(CRInvitationUri _ _) -> do
                contData <- withStore' $ \db -> do
                  setConnConnReqInv db user connId cReq
                  getXGrpMemIntroContDirect db user ct
                forM_ contData $ \(hostConnId, xGrpMemIntroCont) ->
                  sendXGrpMemInv hostConnId (Just directConnReq) xGrpMemIntroCont
              CRContactUri _ -> throwChatError $ CECommandError "unexpected ConnectionRequestUri type"
        MSG msgMeta _msgFlags msgBody -> do
          tags <- newTVarIO []
          withAckMessage "contact msg" agentConnId msgMeta True (Just tags) $ \eInfo -> do
            let MsgMeta {pqEncryption} = msgMeta
            (ct', conn') <- updateContactPQRcv user ct conn pqEncryption
            checkIntegrityCreateItem (CDDirectRcv ct') msgMeta `catchChatError` \_ -> pure ()
            forM_ aChatMsgs $ \case
              Right (ACMsg _ chatMsg) ->
                processEvent ct' conn' tags eInfo chatMsg `catchChatError` \e -> toView $ CRChatError (Just user) e
              Left e -> do
                atomically $ modifyTVar' tags ("error" :)
                logInfo $ "contact msg=error " <> eInfo <> " " <> tshow e
                toView $ CRChatError (Just user) (ChatError . CEException $ "error parsing chat message: " <> e)
            checkSendRcpt ct' $ rights aChatMsgs -- not crucial to use ct'' from processEvent
          where
            aChatMsgs = parseChatMessages msgBody
            processEvent :: Contact -> Connection -> TVar [Text] -> Text -> MsgEncodingI e => ChatMessage e -> CM ()
            processEvent ct' conn' tags eInfo chatMsg@ChatMessage {chatMsgEvent} = do
              let tag = toCMEventTag chatMsgEvent
              atomically $ modifyTVar' tags (tshow tag :)
              logInfo $ "contact msg=" <> tshow tag <> " " <> eInfo
              (conn'', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveDirectRcvMSG conn' msgMeta msgBody chatMsg
              let ct'' = ct' {activeConn = Just conn''} :: Contact
              case event of
                XMsgNew mc -> newContentMessage ct'' mc msg msgMeta
                XMsgFileDescr sharedMsgId fileDescr -> messageFileDescription ct'' sharedMsgId fileDescr
                XMsgUpdate sharedMsgId mContent ttl live -> messageUpdate ct'' sharedMsgId mContent msg msgMeta ttl live
                XMsgDel sharedMsgId _ -> messageDelete ct'' sharedMsgId msg msgMeta
                XMsgReact sharedMsgId _ reaction add -> directMsgReaction ct'' sharedMsgId reaction add msg msgMeta
                -- TODO discontinue XFile
                XFile fInv -> processFileInvitation' ct'' fInv msg msgMeta
                XFileCancel sharedMsgId -> xFileCancel ct'' sharedMsgId
                XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInv ct'' sharedMsgId fileConnReq_ fName
                XInfo p -> xInfo ct'' p
                XDirectDel -> xDirectDel ct'' msg msgMeta
                XGrpInv gInv -> processGroupInvitation ct'' gInv msg msgMeta
                XInfoProbe probe -> xInfoProbe (COMContact ct'') probe
                XInfoProbeCheck probeHash -> xInfoProbeCheck (COMContact ct'') probeHash
                XInfoProbeOk probe -> xInfoProbeOk (COMContact ct'') probe
                XCallInv callId invitation -> xCallInv ct'' callId invitation msg msgMeta
                XCallOffer callId offer -> xCallOffer ct'' callId offer msg
                XCallAnswer callId answer -> xCallAnswer ct'' callId answer msg
                XCallExtra callId extraInfo -> xCallExtra ct'' callId extraInfo msg
                XCallEnd callId -> xCallEnd ct'' callId msg
                BFileChunk sharedMsgId chunk -> bFileChunk ct'' sharedMsgId chunk msgMeta
                _ -> messageError $ "unsupported message: " <> T.pack (show event)
            checkSendRcpt :: Contact -> [AChatMessage] -> CM Bool
            checkSendRcpt ct' aMsgs = do
              let Contact {chatSettings = ChatSettings {sendRcpts}} = ct'
              pure $ fromMaybe (sendRcptsContacts user) sendRcpts && any aChatMsgHasReceipt aMsgs
              where
                aChatMsgHasReceipt (ACMsg _ ChatMessage {chatMsgEvent}) =
                  hasDeliveryReceipt (toCMEventTag chatMsgEvent)
        RCVD msgMeta msgRcpt ->
          withAckMessage' "contact rcvd" agentConnId msgMeta $
            directMsgReceived ct conn msgMeta msgRcpt
        CONF confId pqSupport _ connInfo -> do
          conn' <- processCONFpqSupport conn pqSupport
          ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn' connInfo
          conn'' <- updatePeerChatVRange conn' chatVRange
          case chatMsgEvent of
            -- confirming direct connection with a member
            XGrpMemInfo _memId _memProfile -> do
              -- TODO check member ID
              -- TODO update member profile
              -- [async agent commands] no continuation needed, but command should be asynchronous for stability
              allowAgentConnectionAsync user conn'' confId XOk
            XInfo profile -> do
              ct' <- processContactProfileUpdate ct profile False `catchChatError` const (pure ct)
              -- [incognito] send incognito profile
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore $ \db -> getProfileById db userId profileId
              let p = userProfileToSend user (fromLocalProfile <$> incognitoProfile) (Just ct') False
              allowAgentConnectionAsync user conn'' confId $ XInfo p
              void $ withStore' $ \db -> resetMemberContactFields db ct'
            XGrpLinkInv glInv -> do
              -- XGrpLinkInv here means we are connecting via business contact card, so we replace contact with group
              (gInfo, host) <- withStore $ \db -> do
                liftIO $ deleteContactCardKeepConn db connId ct
                createGroupInvitedViaLink db vr user conn'' glInv
              -- [incognito] send saved profile
              incognitoProfile <- forM customUserProfileId $ \pId -> withStore (\db -> getProfileById db userId pId)
              let profileToSend = userProfileToSend user (fromLocalProfile <$> incognitoProfile) Nothing True
              allowAgentConnectionAsync user conn'' confId $ XInfo profileToSend
              toView $ CRBusinessLinkConnecting user gInfo host ct
            _ -> messageError "CONF for existing contact must have x.grp.mem.info or x.info"
        INFO pqSupport connInfo -> do
          processINFOpqSupport conn pqSupport
          ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
          _conn' <- updatePeerChatVRange conn chatVRange
          case chatMsgEvent of
            XGrpMemInfo _memId _memProfile -> do
              -- TODO check member ID
              -- TODO update member profile
              pure ()
            XInfo profile ->
              void $ processContactProfileUpdate ct profile False
            XOk -> pure ()
            _ -> messageError "INFO for existing contact must have x.grp.mem.info, x.info or x.ok"
        CON pqEnc ->
          withStore' (\db -> getViaGroupMember db vr user ct) >>= \case
            Nothing -> do
              when (pqEnc == PQEncOn) $ withStore' $ \db -> updateConnPQEnabledCON db connId pqEnc
              let conn' = conn {pqSndEnabled = Just pqEnc, pqRcvEnabled = Just pqEnc} :: Connection
                  ct' = ct {activeConn = Just conn'} :: Contact
              -- [incognito] print incognito profile used for this contact
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
              lift $ setContactNetworkStatus ct' NSConnected
              toView $ CRContactConnected user ct' (fmap fromLocalProfile incognitoProfile)
              when (directOrUsed ct') $ do
                unless (contactUsed ct') $ withFastStore' $ \db -> updateContactUsed db user ct'
                createInternalChatItem user (CDDirectRcv ct') (CIRcvDirectE2EEInfo $ E2EInfo pqEnc) Nothing
                createFeatureEnabledItems ct'
              when (contactConnInitiated conn') $ do
                let Connection {groupLinkId} = conn'
                    doProbeContacts = isJust groupLinkId
                probeMatchingContactsAndMembers ct' (contactConnIncognito ct') doProbeContacts
                withStore' $ \db -> resetContactConnInitiated db user conn'
              forM_ viaUserContactLink $ \userContactLinkId -> do
                ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
                let (UserContactLink {autoAccept}, groupId_, gLinkMemRole) = ucl
                when (connChatVersion < batchSend2Version) $ sendAutoReply ct' autoAccept
                forM_ groupId_ $ \groupId -> do
                  groupInfo <- withStore $ \db -> getGroupInfo db vr user groupId
                  subMode <- chatReadVar subscriptionMode
                  groupConnIds <- createAgentConnectionAsync user CFCreateConnGrpInv True SCMInvitation subMode
                  gVar <- asks random
                  withStore $ \db -> createNewContactMemberAsync db gVar user groupInfo ct' gLinkMemRole groupConnIds connChatVersion peerChatVRange subMode
            Just (gInfo, m@GroupMember {activeConn}) ->
              when (maybe False ((== ConnReady) . connStatus) activeConn) $ do
                notifyMemberConnected gInfo m $ Just ct
                let connectedIncognito = contactConnIncognito ct || incognitoMembership gInfo
                when (memberCategory m == GCPreMember) $ probeMatchingContactsAndMembers ct connectedIncognito True
        SENT msgId proxy -> do
          void $ continueSending connEntity conn
          sentMsgDeliveryEvent conn msgId
          checkSndInlineFTComplete conn msgId
          cis <- withStore $ \db -> do
            cis <- updateDirectItemsStatus' db ct conn msgId (CISSndSent SSPComplete)
            liftIO $ forM cis $ \ci -> setDirectSndChatItemViaProxy db user ct ci (isJust proxy)
          let acis = map ctItem cis
          unless (null acis) $ toView $ CRChatItemsStatusesUpdated user acis
          where
            ctItem = AChatItem SCTDirect SMDSnd (DirectChat ct)
        SWITCH qd phase cStats -> do
          toView $ CRContactSwitch user ct (SwitchProgress qd phase cStats)
          when (phase == SPStarted || phase == SPCompleted) $ case qd of
            QDRcv -> createInternalChatItem user (CDDirectSnd ct) (CISndConnEvent $ SCESwitchQueue phase Nothing) Nothing
            QDSnd -> createInternalChatItem user (CDDirectRcv ct) (CIRcvConnEvent $ RCESwitchQueue phase) Nothing
        RSYNC rss cryptoErr_ cStats ->
          case (rss, connectionCode, cryptoErr_) of
            (RSRequired, _, Just cryptoErr) -> processErr cryptoErr
            (RSAllowed, _, Just cryptoErr) -> processErr cryptoErr
            (RSAgreed, Just _, _) -> do
              withStore' $ \db -> setConnectionVerified db user connId Nothing
              let ct' = ct {activeConn = Just $ (conn :: Connection) {connectionCode = Nothing}} :: Contact
              ratchetSyncEventItem ct'
              securityCodeChanged ct'
            _ -> ratchetSyncEventItem ct
          where
            processErr cryptoErr = do
              let e@(mde, n) = agentMsgDecryptError cryptoErr
              ci_ <- withStore $ \db ->
                getDirectChatItemLast db user contactId
                  >>= liftIO
                    . mapM (\(ci, content') -> updateDirectChatItem' db user contactId ci content' False False Nothing Nothing)
                    . mdeUpdatedCI e
              case ci_ of
                Just ci -> toView $ CRChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
                _ -> do
                  toView $ CRContactRatchetSync user ct (RatchetSyncProgress rss cStats)
                  createInternalChatItem user (CDDirectRcv ct) (CIRcvDecryptionError mde n) Nothing
            ratchetSyncEventItem ct' = do
              toView $ CRContactRatchetSync user ct' (RatchetSyncProgress rss cStats)
              createInternalChatItem user (CDDirectRcv ct') (CIRcvConnEvent $ RCERatchetSync rss) Nothing
        OK ->
          -- [async agent commands] continuation on receiving OK
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        JOINED sqSecured ->
          -- [async agent commands] continuation on receiving JOINED
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData ->
            when (directOrUsed ct && sqSecured) $ do
              lift $ setContactNetworkStatus ct NSConnected
              toView $ CRContactSndReady user ct
              forM_ viaUserContactLink $ \userContactLinkId -> do
                ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
                let (UserContactLink {autoAccept}, _, _) = ucl
                when (connChatVersion >= batchSend2Version) $ sendAutoReply ct autoAccept
        QCONT ->
          void $ continueSending connEntity conn
        MWARN msgId err -> do
          updateDirectItemStatus ct conn msgId (CISSndWarning $ agentSndError err)
          processConnMWARN connEntity conn err
        MERR msgId err -> do
          updateDirectItemStatus ct conn msgId (CISSndError $ agentSndError err)
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          processConnMERR connEntity conn err
        MERRS msgIds err -> do
          -- error cannot be AUTH error here
          updateDirectItemsStatusMsgs ct conn (L.toList msgIds) (CISSndError $ agentSndError err)
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        ERR err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()
      where
        sendAutoReply ct = \case
          Just AutoAccept {autoReply = Just mc} -> do
            (msg, _) <- sendDirectContactMessage user ct (XMsgNew $ MCSimple (extMsgContent mc Nothing))
            ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc)
            toView $ CRNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]
          _ -> pure ()

    processGroupMessage :: AEvent e -> ConnectionEntity -> Connection -> GroupInfo -> GroupMember -> CM ()
    processGroupMessage agentMsg connEntity conn@Connection {connId, connChatVersion, connectionCode} gInfo@GroupInfo {groupId, groupProfile, membership, chatSettings} m = case agentMsg of
      INV (ACR _ cReq) ->
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cReq of
            groupConnReq@(CRInvitationUri _ _) -> case cmdFunction of
              -- [async agent commands] XGrpMemIntro continuation on receiving INV
              CFCreateConnGrpMemInv
                | maxVersion (peerChatVRange conn) >= groupDirectInvVersion -> sendWithoutDirectCReq
                | otherwise -> sendWithDirectCReq
                where
                  sendWithoutDirectCReq = do
                    let GroupMember {groupMemberId, memberId} = m
                    hostConnId <- withStore $ \db -> do
                      liftIO $ setConnConnReqInv db user connId cReq
                      getHostConnId db user groupId
                    sendXGrpMemInv hostConnId Nothing XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq}
                  sendWithDirectCReq = do
                    let GroupMember {groupMemberId, memberId} = m
                    contData <- withStore' $ \db -> do
                      setConnConnReqInv db user connId cReq
                      getXGrpMemIntroContGroup db user m
                    forM_ contData $ \(hostConnId, directConnReq) ->
                      sendXGrpMemInv hostConnId (Just directConnReq) XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq}
              -- [async agent commands] group link auto-accept continuation on receiving INV
              CFCreateConnGrpInv -> do
                ct <- withStore $ \db -> getContactViaMember db vr user m
                withStore' $ \db -> setNewContactMemberConnRequest db user m cReq
                groupLinkId <- withStore' $ \db -> getGroupLinkId db user gInfo
                sendGrpInvitation ct m groupLinkId
                toView $ CRSentGroupInvitation user gInfo ct m
                where
                  sendGrpInvitation :: Contact -> GroupMember -> Maybe GroupLinkId -> CM ()
                  sendGrpInvitation ct GroupMember {memberId, memberRole = memRole} groupLinkId = do
                    currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
                    let GroupMember {memberRole = userRole, memberId = userMemberId} = membership
                        groupInv =
                          GroupInvitation
                            { fromMember = MemberIdRole userMemberId userRole,
                              invitedMember = MemberIdRole memberId memRole,
                              connRequest = cReq,
                              groupProfile,
                              business = Nothing,
                              groupLinkId = groupLinkId,
                              groupSize = Just currentMemCount
                            }
                    (_msg, _) <- sendDirectContactMessage user ct $ XGrpInv groupInv
                    -- we could link chat item with sent group invitation message (_msg)
                    createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
              _ -> throwChatError $ CECommandError "unexpected cmdFunction"
            CRContactUri _ -> throwChatError $ CECommandError "unexpected ConnectionRequestUri type"
      CONF confId _pqSupport _ connInfo -> do
        ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
        conn' <- updatePeerChatVRange conn chatVRange
        case memberCategory m of
          GCInviteeMember ->
            case chatMsgEvent of
              XGrpAcpt memId
                | sameMemberId memId m -> do
                    withStore $ \db -> liftIO $ updateGroupMemberStatus db userId m GSMemAccepted
                    -- [async agent commands] no continuation needed, but command should be asynchronous for stability
                    allowAgentConnectionAsync user conn' confId XOk
                | otherwise -> messageError "x.grp.acpt: memberId is different from expected"
              _ -> messageError "CONF from invited member must have x.grp.acpt"
          _ ->
            case chatMsgEvent of
              XGrpMemInfo memId _memProfile
                | sameMemberId memId m -> do
                    let GroupMember {memberId = membershipMemId} = membership
                        membershipProfile = redactedMemberProfile $ fromLocalProfile $ memberProfile membership
                    -- TODO update member profile
                    -- [async agent commands] no continuation needed, but command should be asynchronous for stability
                    allowAgentConnectionAsync user conn' confId $ XGrpMemInfo membershipMemId membershipProfile
                | otherwise -> messageError "x.grp.mem.info: memberId is different from expected"
              _ -> messageError "CONF from member must have x.grp.mem.info"
      INFO _pqSupport connInfo -> do
        ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
        _conn' <- updatePeerChatVRange conn chatVRange
        case chatMsgEvent of
          XGrpMemInfo memId _memProfile
            | sameMemberId memId m -> do
                -- TODO update member profile
                pure ()
            | otherwise -> messageError "x.grp.mem.info: memberId is different from expected"
          XInfo _ -> pure () -- sent when connecting via group link
          XOk -> pure ()
          _ -> messageError "INFO from member must have x.grp.mem.info, x.info or x.ok"
        pure ()
      CON _pqEnc -> do
        withStore' $ \db -> do
          updateGroupMemberStatus db userId m GSMemConnected
          unless (memberActive membership) $
            updateGroupMemberStatus db userId membership GSMemConnected
        -- possible improvement: check for each pending message, requires keeping track of connection state
        unless (connDisabled conn) $ sendPendingGroupMessages user m conn
        withAgent $ \a -> toggleConnectionNtfs a (aConnId conn) $ chatHasNtfs chatSettings
        case memberCategory m of
          GCHostMember -> do
            toView $ CRUserJoinedGroup user gInfo {membership = membership {memberStatus = GSMemConnected}} m {memberStatus = GSMemConnected}
            let cd = CDGroupRcv gInfo m
            createInternalChatItem user cd (CIRcvGroupE2EEInfo E2EInfo {pqEnabled = PQEncOff}) Nothing
            createGroupFeatureItems user cd CIRcvGroupFeature gInfo
            let GroupInfo {groupProfile = GroupProfile {description}} = gInfo
            memberConnectedChatItem gInfo m
            unless expectHistory $ forM_ description $ groupDescriptionChatItem gInfo m
            where
              expectHistory = groupFeatureAllowed SGFHistory gInfo && m `supportsVersion` groupHistoryIncludeWelcomeVersion
          GCInviteeMember -> do
            memberConnectedChatItem gInfo m
            toView $ CRJoinedGroupMember user gInfo m {memberStatus = GSMemConnected}
            let Connection {viaUserContactLink} = conn
            when (isJust viaUserContactLink && isNothing (memberContactId m)) sendXGrpLinkMem
            members <- withStore' $ \db -> getGroupMembers db vr user gInfo
            void . sendGroupMessage user gInfo members . XGrpMemNew $ memberInfo m
            sendIntroductions members
            when (groupFeatureAllowed SGFHistory gInfo) sendHistory
            when (connChatVersion < batchSend2Version) sendGroupAutoReply
            where
              sendXGrpLinkMem = do
                let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                    profileToSend = profileToSendOnAccept user profileMode True
                void $ sendDirectMemberMessage conn (XGrpLinkMem profileToSend) groupId
              sendIntroductions members = do
                intros <- withStore' $ \db -> createIntroductions db (maxVersion vr) members m
                shuffledIntros <- liftIO $ shuffleIntros intros
                if m `supportsVersion` batchSendVersion
                  then do
                    let events = map (memberIntro . reMember) shuffledIntros
                    forM_ (L.nonEmpty events) $ \events' ->
                      sendGroupMemberMessages user conn events' groupId
                  else forM_ shuffledIntros $ \intro ->
                    processIntro intro `catchChatError` (toView . CRChatError (Just user))
              memberIntro :: GroupMember -> ChatMsgEvent 'Json
              memberIntro reMember =
                let mInfo = memberInfo reMember
                    mRestrictions = memberRestrictions reMember
                 in XGrpMemIntro mInfo mRestrictions
              shuffleIntros :: [GroupMemberIntro] -> IO [GroupMemberIntro]
              shuffleIntros intros = do
                let (admins, others) = partition isAdmin intros
                    (admPics, admNoPics) = partition hasPicture admins
                    (othPics, othNoPics) = partition hasPicture others
                mconcat <$> mapM shuffle [admPics, admNoPics, othPics, othNoPics]
                where
                  isAdmin GroupMemberIntro {reMember = GroupMember {memberRole}} = memberRole >= GRAdmin
                  hasPicture GroupMemberIntro {reMember = GroupMember {memberProfile = LocalProfile {image}}} = isJust image
              processIntro intro@GroupMemberIntro {introId} = do
                void $ sendDirectMemberMessage conn (memberIntro $ reMember intro) groupId
                withStore' $ \db -> updateIntroStatus db introId GMIntroSent
              sendHistory =
                when (m `supportsVersion` batchSendVersion) $ do
                  (errs, items) <- partitionEithers <$> withStore' (\db -> getGroupHistoryItems db user gInfo m 100)
                  (errs', events) <- partitionEithers <$> mapM (tryChatError . itemForwardEvents) items
                  let errors = map ChatErrorStore errs <> errs'
                  unless (null errors) $ toView $ CRChatErrors (Just user) errors
                  let events' = maybe (concat events) (\x -> concat events <> [x]) descrEvent_
                  forM_ (L.nonEmpty events') $ \events'' ->
                    sendGroupMemberMessages user conn events'' groupId
              descrEvent_ :: Maybe (ChatMsgEvent 'Json)
              descrEvent_
                | m `supportsVersion` groupHistoryIncludeWelcomeVersion = do
                    let GroupInfo {groupProfile = GroupProfile {description}} = gInfo
                    fmap (\descr -> XMsgNew $ MCSimple $ extMsgContent (MCText descr) Nothing) description
                | otherwise = Nothing
              itemForwardEvents :: CChatItem 'CTGroup -> CM [ChatMsgEvent 'Json]
              itemForwardEvents cci = case cci of
                (CChatItem SMDRcv ci@ChatItem {chatDir = CIGroupRcv sender, content = CIRcvMsgContent mc, file})
                  | not (blockedByAdmin sender) -> do
                      fInvDescr_ <- join <$> forM file getRcvFileInvDescr
                      processContentItem sender ci mc fInvDescr_
                (CChatItem SMDSnd ci@ChatItem {content = CISndMsgContent mc, file}) -> do
                  fInvDescr_ <- join <$> forM file getSndFileInvDescr
                  processContentItem membership ci mc fInvDescr_
                _ -> pure []
                where
                  getRcvFileInvDescr :: CIFile 'MDRcv -> CM (Maybe (FileInvitation, RcvFileDescrText))
                  getRcvFileInvDescr ciFile@CIFile {fileId, fileProtocol, fileStatus} = do
                    expired <- fileExpired
                    if fileProtocol /= FPXFTP || fileStatus == CIFSRcvCancelled || expired
                      then pure Nothing
                      else do
                        rfd <- withStore $ \db -> getRcvFileDescrByRcvFileId db fileId
                        pure $ invCompleteDescr ciFile rfd
                  getSndFileInvDescr :: CIFile 'MDSnd -> CM (Maybe (FileInvitation, RcvFileDescrText))
                  getSndFileInvDescr ciFile@CIFile {fileId, fileProtocol, fileStatus} = do
                    expired <- fileExpired
                    if fileProtocol /= FPXFTP || fileStatus == CIFSSndCancelled || expired
                      then pure Nothing
                      else do
                        -- can also lookup in extra_xftp_file_descriptions, though it can be empty;
                        -- would be best if snd file had a single rcv description for all members saved in files table
                        rfd <- withStore $ \db -> getRcvFileDescrBySndFileId db fileId
                        pure $ invCompleteDescr ciFile rfd
                  fileExpired :: CM Bool
                  fileExpired = do
                    ttl <- asks $ rcvFilesTTL . agentConfig . config
                    cutoffTs <- addUTCTime (-ttl) <$> liftIO getCurrentTime
                    pure $ chatItemTs cci < cutoffTs
                  invCompleteDescr :: CIFile d -> RcvFileDescr -> Maybe (FileInvitation, RcvFileDescrText)
                  invCompleteDescr CIFile {fileName, fileSize} RcvFileDescr {fileDescrText, fileDescrComplete}
                    | fileDescrComplete =
                        let fInvDescr = FileDescr {fileDescrText = "", fileDescrPartNo = 0, fileDescrComplete = False}
                            fInv = xftpFileInvitation fileName fileSize fInvDescr
                         in Just (fInv, fileDescrText)
                    | otherwise = Nothing
                  processContentItem :: GroupMember -> ChatItem 'CTGroup d -> MsgContent -> Maybe (FileInvitation, RcvFileDescrText) -> CM [ChatMsgEvent 'Json]
                  processContentItem sender ChatItem {meta, quotedItem} mc fInvDescr_ =
                    if isNothing fInvDescr_ && not (msgContentHasText mc)
                      then pure []
                      else do
                        let CIMeta {itemTs, itemSharedMsgId, itemTimed} = meta
                            quotedItemId_ = quoteItemId =<< quotedItem
                            fInv_ = fst <$> fInvDescr_
                        (msgContainer, _) <- prepareGroupMsg user gInfo mc quotedItemId_ Nothing fInv_ itemTimed False
                        let senderVRange = memberChatVRange' sender
                            xMsgNewChatMsg = ChatMessage {chatVRange = senderVRange, msgId = itemSharedMsgId, chatMsgEvent = XMsgNew msgContainer}
                        fileDescrEvents <- case (snd <$> fInvDescr_, itemSharedMsgId) of
                          (Just fileDescrText, Just msgId) -> do
                            partSize <- asks $ xftpDescrPartSize . config
                            let parts = splitFileDescr partSize fileDescrText
                            pure . L.toList $ L.map (XMsgFileDescr msgId) parts
                          _ -> pure []
                        let fileDescrChatMsgs = map (ChatMessage senderVRange Nothing) fileDescrEvents
                            GroupMember {memberId} = sender
                            msgForwardEvents = map (\cm -> XGrpMsgForward memberId cm itemTs) (xMsgNewChatMsg : fileDescrChatMsgs)
                        pure msgForwardEvents
          _ -> do
            let memCategory = memberCategory m
            withStore' (\db -> getViaGroupContact db vr user m) >>= \case
              Nothing -> do
                notifyMemberConnected gInfo m Nothing
                let connectedIncognito = memberIncognito membership
                when (memCategory == GCPreMember) $ probeMatchingMemberContact m connectedIncognito
              Just ct@Contact {activeConn} ->
                forM_ activeConn $ \Connection {connStatus} ->
                  when (connStatus == ConnReady) $ do
                    notifyMemberConnected gInfo m $ Just ct
                    let connectedIncognito = contactConnIncognito ct || incognitoMembership gInfo
                    when (memCategory == GCPreMember) $ probeMatchingContactsAndMembers ct connectedIncognito True
            sendXGrpMemCon memCategory
            where
              GroupMember {memberId} = m
              sendXGrpMemCon = \case
                GCPreMember ->
                  forM_ (invitedByGroupMemberId membership) $ \hostId -> do
                    host <- withStore $ \db -> getGroupMember db vr user groupId hostId
                    forM_ (memberConn host) $ \hostConn ->
                      void $ sendDirectMemberMessage hostConn (XGrpMemCon memberId) groupId
                GCPostMember ->
                  forM_ (invitedByGroupMemberId m) $ \invitingMemberId -> do
                    im <- withStore $ \db -> getGroupMember db vr user groupId invitingMemberId
                    forM_ (memberConn im) $ \imConn ->
                      void $ sendDirectMemberMessage imConn (XGrpMemCon memberId) groupId
                _ -> messageWarning "sendXGrpMemCon: member category GCPreMember or GCPostMember is expected"
      MSG msgMeta _msgFlags msgBody -> do
        tags <- newTVarIO []
        withAckMessage "group msg" agentConnId msgMeta True (Just tags) $ \eInfo -> do
          checkIntegrityCreateItem (CDGroupRcv gInfo m) msgMeta `catchChatError` \_ -> pure ()
          forM_ aChatMsgs $ \case
            Right (ACMsg _ chatMsg) ->
              processEvent tags eInfo chatMsg `catchChatError` \e -> toView $ CRChatError (Just user) e
            Left e -> do
              atomically $ modifyTVar' tags ("error" :)
              logInfo $ "group msg=error " <> eInfo <> " " <> tshow e
              toView $ CRChatError (Just user) (ChatError . CEException $ "error parsing chat message: " <> e)
          forwardMsgs (rights aChatMsgs) `catchChatError` (toView . CRChatError (Just user))
          checkSendRcpt $ rights aChatMsgs
        where
          aChatMsgs = parseChatMessages msgBody
          brokerTs = metaBrokerTs msgMeta
          processEvent :: TVar [Text] -> Text -> MsgEncodingI e => ChatMessage e -> CM ()
          processEvent tags eInfo chatMsg@ChatMessage {chatMsgEvent} = do
            let tag = toCMEventTag chatMsgEvent
            atomically $ modifyTVar' tags (tshow tag :)
            logInfo $ "group msg=" <> tshow tag <> " " <> eInfo
            (m', conn', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveGroupRcvMsg user groupId m conn msgMeta msgBody chatMsg
            case event of
              XMsgNew mc -> memberCanSend m' $ newGroupContentMessage gInfo m' mc msg brokerTs False
              XMsgFileDescr sharedMsgId fileDescr -> memberCanSend m' $ groupMessageFileDescription gInfo m' sharedMsgId fileDescr
              XMsgUpdate sharedMsgId mContent ttl live -> memberCanSend m' $ groupMessageUpdate gInfo m' sharedMsgId mContent msg brokerTs ttl live
              XMsgDel sharedMsgId memberId -> groupMessageDelete gInfo m' sharedMsgId memberId msg brokerTs
              XMsgReact sharedMsgId (Just memberId) reaction add -> groupMsgReaction gInfo m' sharedMsgId memberId reaction add msg brokerTs
              -- TODO discontinue XFile
              XFile fInv -> processGroupFileInvitation' gInfo m' fInv msg brokerTs
              XFileCancel sharedMsgId -> xFileCancelGroup gInfo m' sharedMsgId
              XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInvGroup gInfo m' sharedMsgId fileConnReq_ fName
              XInfo p -> xInfoMember gInfo m' p brokerTs
              XGrpLinkMem p -> xGrpLinkMem gInfo m' conn' p
              XGrpMemNew memInfo -> xGrpMemNew gInfo m' memInfo msg brokerTs
              XGrpMemIntro memInfo memRestrictions_ -> xGrpMemIntro gInfo m' memInfo memRestrictions_
              XGrpMemInv memId introInv -> xGrpMemInv gInfo m' memId introInv
              XGrpMemFwd memInfo introInv -> xGrpMemFwd gInfo m' memInfo introInv
              XGrpMemRole memId memRole -> xGrpMemRole gInfo m' memId memRole msg brokerTs
              XGrpMemRestrict memId memRestrictions -> xGrpMemRestrict gInfo m' memId memRestrictions msg brokerTs
              XGrpMemCon memId -> xGrpMemCon gInfo m' memId
              XGrpMemDel memId -> xGrpMemDel gInfo m' memId msg brokerTs
              XGrpLeave -> xGrpLeave gInfo m' msg brokerTs
              XGrpDel -> xGrpDel gInfo m' msg brokerTs
              XGrpInfo p' -> xGrpInfo gInfo m' p' msg brokerTs
              XGrpPrefs ps' -> xGrpPrefs gInfo m' ps'
              XGrpDirectInv connReq mContent_ -> memberCanSend m' $ xGrpDirectInv gInfo m' conn' connReq mContent_ msg brokerTs
              XGrpMsgForward memberId msg' msgTs -> xGrpMsgForward gInfo m' memberId msg' msgTs
              XInfoProbe probe -> xInfoProbe (COMGroupMember m') probe
              XInfoProbeCheck probeHash -> xInfoProbeCheck (COMGroupMember m') probeHash
              XInfoProbeOk probe -> xInfoProbeOk (COMGroupMember m') probe
              BFileChunk sharedMsgId chunk -> bFileChunkGroup gInfo sharedMsgId chunk msgMeta
              _ -> messageError $ "unsupported message: " <> tshow event
          checkSendRcpt :: [AChatMessage] -> CM Bool
          checkSendRcpt aMsgs = do
            currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
            let GroupInfo {chatSettings = ChatSettings {sendRcpts}} = gInfo
            pure $
              fromMaybe (sendRcptsSmallGroups user) sendRcpts
                && any aChatMsgHasReceipt aMsgs
                && currentMemCount <= smallGroupsRcptsMemLimit
            where
              aChatMsgHasReceipt (ACMsg _ ChatMessage {chatMsgEvent}) =
                hasDeliveryReceipt (toCMEventTag chatMsgEvent)
          forwardMsgs :: [AChatMessage] -> CM ()
          forwardMsgs aMsgs = do
            let GroupMember {memberRole = membershipMemRole} = membership
            when (membershipMemRole >= GRAdmin && not (blockedByAdmin m)) $ do
              let forwardedMsgs = mapMaybe (\(ACMsg _ chatMsg) -> forwardedGroupMsg chatMsg) aMsgs
              forM_ (L.nonEmpty forwardedMsgs) $ \forwardedMsgs' -> do
                ChatConfig {highlyAvailable} <- asks config
                -- members introduced to this invited member
                introducedMembers <-
                  if memberCategory m == GCInviteeMember
                    then withStore' $ \db -> getForwardIntroducedMembers db vr user m highlyAvailable
                    else pure []
                -- invited members to which this member was introduced
                invitedMembers <- withStore' $ \db -> getForwardInvitedMembers db vr user m highlyAvailable
                let GroupMember {memberId} = m
                    ms = forwardedToGroupMembers (introducedMembers <> invitedMembers) forwardedMsgs'
                    events = L.map (\cm -> XGrpMsgForward memberId cm brokerTs) forwardedMsgs'
                unless (null ms) $ void $ sendGroupMessages user gInfo ms events
      RCVD msgMeta msgRcpt ->
        withAckMessage' "group rcvd" agentConnId msgMeta $
          groupMsgReceived gInfo m conn msgMeta msgRcpt
      SENT msgId proxy -> do
        continued <- continueSending connEntity conn
        sentMsgDeliveryEvent conn msgId
        checkSndInlineFTComplete conn msgId
        updateGroupItemsStatus gInfo m conn msgId GSSSent (Just $ isJust proxy)
        when continued $ sendPendingGroupMessages user m conn
      SWITCH qd phase cStats -> do
        toView $ CRGroupMemberSwitch user gInfo m (SwitchProgress qd phase cStats)
        when (phase == SPStarted || phase == SPCompleted) $ case qd of
          QDRcv -> createInternalChatItem user (CDGroupSnd gInfo) (CISndConnEvent . SCESwitchQueue phase . Just $ groupMemberRef m) Nothing
          QDSnd -> createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvConnEvent $ RCESwitchQueue phase) Nothing
      RSYNC rss cryptoErr_ cStats ->
        case (rss, connectionCode, cryptoErr_) of
          (RSRequired, _, Just cryptoErr) -> processErr cryptoErr
          (RSAllowed, _, Just cryptoErr) -> processErr cryptoErr
          (RSAgreed, Just _, _) -> do
            withStore' $ \db -> setConnectionVerified db user connId Nothing
            let m' = m {activeConn = Just (conn {connectionCode = Nothing} :: Connection)} :: GroupMember
            ratchetSyncEventItem m'
            toView $ CRGroupMemberVerificationReset user gInfo m'
            createInternalChatItem user (CDGroupRcv gInfo m') (CIRcvConnEvent RCEVerificationCodeReset) Nothing
          _ -> ratchetSyncEventItem m
        where
          processErr cryptoErr = do
            let e@(mde, n) = agentMsgDecryptError cryptoErr
            ci_ <- withStore $ \db ->
              getGroupMemberChatItemLast db user groupId (groupMemberId' m)
                >>= liftIO
                  . mapM (\(ci, content') -> updateGroupChatItem db user groupId ci content' False False Nothing)
                  . mdeUpdatedCI e
            case ci_ of
              Just ci -> toView $ CRChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)
              _ -> do
                toView $ CRGroupMemberRatchetSync user gInfo m (RatchetSyncProgress rss cStats)
                createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvDecryptionError mde n) Nothing
          ratchetSyncEventItem m' = do
            toView $ CRGroupMemberRatchetSync user gInfo m' (RatchetSyncProgress rss cStats)
            createInternalChatItem user (CDGroupRcv gInfo m') (CIRcvConnEvent $ RCERatchetSync rss) Nothing
      OK ->
        -- [async agent commands] continuation on receiving OK
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      JOINED sqSecured ->
        -- [async agent commands] continuation on receiving JOINED
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData ->
          when (sqSecured && connChatVersion >= batchSend2Version) sendGroupAutoReply
      QCONT -> do
        continued <- continueSending connEntity conn
        when continued $ sendPendingGroupMessages user m conn
      MWARN msgId err -> do
        withStore' $ \db -> updateGroupItemsErrorStatus db msgId (groupMemberId' m) (GSSWarning $ agentSndError err)
        processConnMWARN connEntity conn err
      MERR msgId err -> do
        withStore' $ \db -> updateGroupItemsErrorStatus db msgId (groupMemberId' m) (GSSError $ agentSndError err)
        -- group errors are silenced to reduce load on UI event log
        -- toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        processConnMERR connEntity conn err
      MERRS msgIds err -> do
        let newStatus = GSSError $ agentSndError err
        -- error cannot be AUTH error here
        withStore' $ \db -> forM_ msgIds $ \msgId ->
          updateGroupItemsErrorStatus db msgId (groupMemberId' m) newStatus `catchAll_` pure ()
        toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
      ERR err -> do
        toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()
      where
        updateGroupItemsErrorStatus :: DB.Connection -> AgentMsgId -> GroupMemberId -> GroupSndStatus -> IO ()
        updateGroupItemsErrorStatus db msgId groupMemberId newStatus = do
          itemIds <- getChatItemIdsByAgentMsgId db connId msgId
          forM_ itemIds $ \itemId -> updateGroupMemSndStatus' db itemId groupMemberId newStatus
        sendGroupAutoReply = autoReplyMC >>= mapM_ send
          where
            autoReplyMC = do
              let GroupInfo {businessChat} = gInfo
                  GroupMember {memberId = joiningMemberId} = m
              case businessChat of
                Just BusinessChatInfo {customerId, chatType = BCCustomer}
                  | joiningMemberId == customerId -> useReply <$> withStore (`getUserAddress` user)
                  where
                    useReply UserContactLink {autoAccept} = case autoAccept of
                      Just AutoAccept {businessAddress, autoReply} | businessAddress -> autoReply
                      _ -> Nothing
                _ -> pure Nothing
            send mc = do
              msg <- sendGroupMessage' user gInfo [m] (XMsgNew $ MCSimple (extMsgContent mc Nothing))
              ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndMsgContent mc)
              withStore' $ \db -> createGroupSndStatus db (chatItemId' ci) (groupMemberId' m) GSSNew
              toView $ CRNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci]

    agentMsgDecryptError :: AgentCryptoError -> (MsgDecryptError, Word32)
    agentMsgDecryptError = \case
      DECRYPT_AES -> (MDEOther, 1)
      DECRYPT_CB -> (MDEOther, 1)
      RATCHET_HEADER -> (MDERatchetHeader, 1)
      RATCHET_EARLIER _ -> (MDERatchetEarlier, 1)
      RATCHET_SKIPPED n -> (MDETooManySkipped, n)
      RATCHET_SYNC -> (MDERatchetSync, 0)

    mdeUpdatedCI :: (MsgDecryptError, Word32) -> CChatItem c -> Maybe (ChatItem c 'MDRcv, CIContent 'MDRcv)
    mdeUpdatedCI (mde', n') (CChatItem _ ci@ChatItem {content = CIRcvDecryptionError mde n})
      | mde == mde' = case mde of
          MDERatchetHeader -> r (n + n')
          MDETooManySkipped -> r n' -- the numbers are not added as sequential MDETooManySkipped will have it incremented by 1
          MDERatchetEarlier -> r (n + n')
          MDEOther -> r (n + n')
          MDERatchetSync -> r 0
      | otherwise = Nothing
      where
        r n'' = Just (ci, CIRcvDecryptionError mde n'')
    mdeUpdatedCI _ _ = Nothing

    processSndFileConn :: AEvent e -> ConnectionEntity -> Connection -> SndFileTransfer -> CM ()
    processSndFileConn agentMsg connEntity conn ft@SndFileTransfer {fileId, fileName, fileStatus} =
      case agentMsg of
        -- SMP CONF for SndFileConnection happens for direct file protocol
        -- when recipient of the file "joins" connection created by the sender
        CONF confId _pqSupport _ connInfo -> do
          ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
          conn' <- updatePeerChatVRange conn chatVRange
          case chatMsgEvent of
            -- TODO save XFileAcpt message
            XFileAcpt name
              | name == fileName -> do
                  withStore' $ \db -> updateSndFileStatus db ft FSAccepted
                  -- [async agent commands] no continuation needed, but command should be asynchronous for stability
                  allowAgentConnectionAsync user conn' confId XOk
              | otherwise -> messageError "x.file.acpt: fileName is different from expected"
            _ -> messageError "CONF from file connection must have x.file.acpt"
        CON _ -> do
          ci <- withStore $ \db -> do
            liftIO $ updateSndFileStatus db ft FSConnected
            updateDirectCIFileStatus db vr user fileId $ CIFSSndTransfer 0 1
          toView $ CRSndFileStart user ci ft
          sendFileChunk user ft
        SENT msgId _proxy -> do
          withStore' $ \db -> updateSndFileChunkSent db ft msgId
          unless (fileStatus == FSCancelled) $ sendFileChunk user ft
        MERR _ err -> do
          cancelSndFileTransfer user ft True >>= mapM_ (deleteAgentConnectionAsync user)
          case err of
            SMP _ SMP.AUTH -> unless (fileStatus == FSCancelled) $ do
              ci <- withStore $ \db -> do
                liftIO (lookupChatRefByFileId db user fileId) >>= \case
                  Just (ChatRef CTDirect _) -> liftIO $ updateFileCancelled db user fileId CIFSSndCancelled
                  _ -> pure ()
                lookupChatItemByFileId db vr user fileId
              toView $ CRSndFileRcvCancelled user ci ft
            _ -> throwChatError $ CEFileSend fileId err
        MSG meta _ _ ->
          withAckMessage' "file msg" agentConnId meta $ pure ()
        OK ->
          -- [async agent commands] continuation on receiving OK
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        JOINED _ ->
          -- [async agent commands] continuation on receiving JOINED
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        ERR err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    processRcvFileConn :: AEvent e -> ConnectionEntity -> Connection -> RcvFileTransfer -> CM ()
    processRcvFileConn agentMsg connEntity conn ft@RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}, grpMemberId} =
      case agentMsg of
        INV (ACR _ cReq) ->
          withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
            case cReq of
              fileInvConnReq@(CRInvitationUri _ _) -> case cmdFunction of
                -- [async agent commands] direct XFileAcptInv continuation on receiving INV
                CFCreateConnFileInvDirect -> do
                  ct <- withStore $ \db -> getContactByFileId db vr user fileId
                  sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
                  void $ sendDirectContactMessage user ct (XFileAcptInv sharedMsgId (Just fileInvConnReq) fileName)
                -- [async agent commands] group XFileAcptInv continuation on receiving INV
                CFCreateConnFileInvGroup -> case grpMemberId of
                  Just gMemberId -> do
                    GroupMember {groupId, activeConn} <- withStore $ \db -> getGroupMemberById db vr user gMemberId
                    case activeConn of
                      Just gMemberConn -> do
                        sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
                        void $ sendDirectMemberMessage gMemberConn (XFileAcptInv sharedMsgId (Just fileInvConnReq) fileName) groupId
                      _ -> throwChatError $ CECommandError "no GroupMember activeConn"
                  _ -> throwChatError $ CECommandError "no grpMemberId"
                _ -> throwChatError $ CECommandError "unexpected cmdFunction"
              CRContactUri _ -> throwChatError $ CECommandError "unexpected ConnectionRequestUri type"
        -- SMP CONF for RcvFileConnection happens for group file protocol
        -- when sender of the file "joins" connection created by the recipient
        -- (sender doesn't create connections for all group members)
        CONF confId _pqSupport _ connInfo -> do
          ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
          conn' <- updatePeerChatVRange conn chatVRange
          case chatMsgEvent of
            XOk -> allowAgentConnectionAsync user conn' confId XOk -- [async agent commands] no continuation needed, but command should be asynchronous for stability
            _ -> pure ()
        CON _ -> startReceivingFile user fileId
        MSG meta _ msgBody -> do
          -- XXX: not all branches do ACK
          parseFileChunk msgBody >>= receiveFileChunk ft (Just conn) meta
        OK ->
          -- [async agent commands] continuation on receiving OK
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        JOINED _ ->
          -- [async agent commands] continuation on receiving JOINED
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        MERR _ err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          processConnMERR connEntity conn err
        ERR err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    receiveFileChunk :: RcvFileTransfer -> Maybe Connection -> MsgMeta -> FileChunk -> CM ()
    receiveFileChunk ft@RcvFileTransfer {fileId, chunkSize} conn_ meta@MsgMeta {recipient = (msgId, _), integrity} = \case
      FileChunkCancel ->
        unless (rcvFileCompleteOrCancelled ft) $ do
          cancelRcvFileTransfer user ft >>= mapM_ (deleteAgentConnectionAsync user)
          ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
          toView $ CRRcvFileSndCancelled user ci ft
      FileChunk {chunkNo, chunkBytes = chunk} -> do
        case integrity of
          MsgOk -> pure ()
          MsgError MsgDuplicate -> pure () -- TODO remove once agent removes duplicates
          MsgError e ->
            badRcvFileChunk ft $ "invalid file chunk number " <> show chunkNo <> ": " <> show e
        withStore' (\db -> createRcvFileChunk db ft chunkNo msgId) >>= \case
          RcvChunkOk ->
            if B.length chunk /= fromInteger chunkSize
              then badRcvFileChunk ft "incorrect chunk size"
              else withAckMessage' "file msg" agentConnId meta $ appendFileChunk ft chunkNo chunk False
          RcvChunkFinal ->
            if B.length chunk > fromInteger chunkSize
              then badRcvFileChunk ft "incorrect chunk size"
              else do
                appendFileChunk ft chunkNo chunk True
                ci <- withStore $ \db -> do
                  liftIO $ do
                    updateRcvFileStatus db fileId FSComplete
                    updateCIFileStatus db user fileId CIFSRcvComplete
                    deleteRcvFileChunks db ft
                  getChatItemByFileId db vr user fileId
                toView $ CRRcvFileComplete user ci
                forM_ conn_ $ \conn -> deleteAgentConnectionAsync user (aConnId conn)
          RcvChunkDuplicate -> withAckMessage' "file msg" agentConnId meta $ pure ()
          RcvChunkError -> badRcvFileChunk ft $ "incorrect chunk number " <> show chunkNo

    processUserContactRequest :: AEvent e -> ConnectionEntity -> Connection -> UserContact -> CM ()
    processUserContactRequest agentMsg connEntity conn UserContact {userContactLinkId} = case agentMsg of
      REQ invId pqSupport _ connInfo -> do
        ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
        case chatMsgEvent of
          XContact p xContactId_ -> profileContactRequest invId chatVRange p xContactId_ pqSupport
          XInfo p -> profileContactRequest invId chatVRange p Nothing pqSupport
          -- TODO show/log error, other events in contact request
          _ -> pure ()
      MERR _ err -> do
        toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        processConnMERR connEntity conn err
      ERR err -> do
        toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()
      where
        profileContactRequest :: InvitationId -> VersionRangeChat -> Profile -> Maybe XContactId -> PQSupport -> CM ()
        profileContactRequest invId chatVRange p xContactId_ reqPQSup = do
          withStore (\db -> createOrUpdateContactRequest db vr user userContactLinkId invId chatVRange p xContactId_ reqPQSup) >>= \case
            CORContact contact -> toView $ CRContactRequestAlreadyAccepted user contact
            CORGroup gInfo -> toView $ CRBusinessRequestAlreadyAccepted user gInfo
            CORRequest cReq -> do
              ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
              let (UserContactLink {connReqContact, autoAccept}, groupId_, gLinkMemRole) = ucl
                  isSimplexTeam = sameConnReqContact connReqContact adminContactReq
                  v = maxVersion chatVRange
              case autoAccept of
                Just AutoAccept {acceptIncognito, businessAddress}
                  | businessAddress ->
                      if v < groupFastLinkJoinVersion || (isSimplexTeam && v < businessChatsVersion)
                        then do
                          ct <- acceptContactRequestAsync user cReq Nothing True reqPQSup
                          toView $ CRAcceptingContactRequest user ct
                        else do
                          gInfo <- acceptBusinessJoinRequestAsync user cReq
                          toView $ CRAcceptingBusinessRequest user gInfo
                  | otherwise -> case groupId_ of
                      Nothing -> do
                        -- [incognito] generate profile to send, create connection with incognito profile
                        incognitoProfile <- if acceptIncognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
                        ct <- acceptContactRequestAsync user cReq incognitoProfile True reqPQSup
                        toView $ CRAcceptingContactRequest user ct
                      Just groupId -> do
                        gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
                        let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                        if v >= groupFastLinkJoinVersion
                          then do
                            mem <- acceptGroupJoinRequestAsync user gInfo cReq gLinkMemRole profileMode
                            createInternalChatItem user (CDGroupRcv gInfo mem) (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
                            toView $ CRAcceptingGroupJoinRequestMember user gInfo mem
                          else do
                            -- TODO v5.7 remove old API (or v6.0?)
                            ct <- acceptContactRequestAsync user cReq profileMode False PQSupportOff
                            toView $ CRAcceptingGroupJoinRequest user gInfo ct
                _ -> toView $ CRReceivedContactRequest user cReq

    memberCanSend :: GroupMember -> CM () -> CM ()
    memberCanSend GroupMember {memberRole} a
      | memberRole <= GRObserver = messageError "member is not allowed to send messages"
      | otherwise = a

    processConnMERR :: ConnectionEntity -> Connection -> AgentErrorType -> CM ()
    processConnMERR connEntity conn err = do
      case err of
        SMP _ SMP.AUTH -> do
          authErrCounter' <- withStore' $ \db -> incAuthErrCounter db user conn
          when (authErrCounter' >= authErrDisableCount) $ case connEntity of
            RcvDirectMsgConnection ctConn (Just ct) -> do
              toView $ CRContactDisabled user ct {activeConn = Just ctConn {authErrCounter = authErrCounter'}}
            _ -> toView $ CRConnectionDisabled connEntity
        SMP _ SMP.QUOTA ->
          unless (connInactive conn) $ do
            withStore' $ \db -> setQuotaErrCounter db user conn quotaErrSetOnMERR
            toView $ CRConnectionInactive connEntity True
        _ -> pure ()

    processConnMWARN :: ConnectionEntity -> Connection -> AgentErrorType -> CM ()
    processConnMWARN connEntity conn err = do
      case err of
        SMP _ SMP.QUOTA ->
          unless (connInactive conn) $ do
            quotaErrCounter' <- withStore' $ \db -> incQuotaErrCounter db user conn
            when (quotaErrCounter' >= quotaErrInactiveCount) $
              toView $
                CRConnectionInactive connEntity True
        _ -> pure ()

    continueSending :: ConnectionEntity -> Connection -> CM Bool
    continueSending connEntity conn =
      if connInactive conn
        then do
          withStore' $ \db -> setQuotaErrCounter db user conn 0
          toView $ CRConnectionInactive connEntity False
          pure True
        else pure False

    -- TODO v5.7 / v6.0 - together with deprecating old group protocol establishing direct connections?
    -- we could save command records only for agent APIs we process continuations for (INV)
    withCompletedCommand :: forall e. AEntityI e => Connection -> AEvent e -> (CommandData -> CM ()) -> CM ()
    withCompletedCommand Connection {connId} agentMsg action = do
      let agentMsgTag = AEvtTag (sAEntity @e) $ aEventTag agentMsg
      cmdData_ <- withStore' $ \db -> getCommandDataByCorrId db user corrId
      case cmdData_ of
        Just cmdData@CommandData {cmdId, cmdConnId = Just cmdConnId', cmdFunction}
          | connId == cmdConnId' && (agentMsgTag == commandExpectedResponse cmdFunction || agentMsgTag == AEvtTag SAEConn ERR_) -> do
              withStore' $ \db -> deleteCommand db user cmdId
              action cmdData
          | otherwise -> err cmdId $ "not matching connection id or unexpected response, corrId = " <> show corrId
        Just CommandData {cmdId, cmdConnId = Nothing} -> err cmdId $ "no command connection id, corrId = " <> show corrId
        Nothing -> throwChatError . CEAgentCommandError $ "command not found, corrId = " <> show corrId
      where
        err cmdId msg = do
          withStore' $ \db -> updateCommandStatus db user cmdId CSError
          throwChatError . CEAgentCommandError $ msg

    withAckMessage' :: Text -> ConnId -> MsgMeta -> CM () -> CM ()
    withAckMessage' label cId msgMeta action = do
      withAckMessage label cId msgMeta False Nothing $ \_ -> action $> False

    withAckMessage :: Text -> ConnId -> MsgMeta -> Bool -> Maybe (TVar [Text]) -> (Text -> CM Bool) -> CM ()
    withAckMessage label cId msgMeta showCritical tags action = do
      -- [async agent commands] command should be asynchronous
      -- TODO catching error and sending ACK after an error, particularly if it is a database error, will result in the message not processed (and no notification to the user).
      -- Possible solutions are:
      -- 1) retry processing several times
      -- 2) stabilize database
      -- 3) show screen of death to the user asking to restart
      eInfo <- eventInfo
      logInfo $ label <> ": " <> eInfo
      tryChatError (action eInfo) >>= \case
        Right withRcpt ->
          withLog (eInfo <> " ok") $ ackMsg msgMeta $ if withRcpt then Just "" else Nothing
        -- If showCritical is True, then these errors don't result in ACK and show user visible alert
        -- This prevents losing the message that failed to be processed.
        Left (ChatErrorStore SEDBBusyError {message}) | showCritical -> throwError $ ChatErrorAgent (CRITICAL True message) Nothing
        Left e -> do
          withLog (eInfo <> " error: " <> tshow e) $ ackMsg msgMeta Nothing
          throwError e
      where
        eventInfo = do
          v <- asks eventSeq
          eId <- atomically $ stateTVar v $ \i -> (i + 1, i + 1)
          pure $ "conn_id=" <> tshow cId <> " event_id=" <> tshow eId
        withLog eInfo' ack = do
          ts <- showTags
          logInfo $ T.unwords [label, "ack:", ts, eInfo']
          ack
          logInfo $ T.unwords [label, "ack=success:", ts, eInfo']
        showTags = do
          ts <- maybe (pure []) readTVarIO tags
          pure $ case ts of
            [] -> "no_chat_messages"
            [t] -> "chat_message=" <> t
            _ -> "chat_message_batch=" <> T.intercalate "," (reverse ts)
        ackMsg :: MsgMeta -> Maybe MsgReceiptInfo -> CM ()
        ackMsg MsgMeta {recipient = (msgId, _)} rcpt = withAgent $ \a -> ackMessageAsync a "" cId msgId rcpt

    sentMsgDeliveryEvent :: Connection -> AgentMsgId -> CM ()
    sentMsgDeliveryEvent Connection {connId} msgId =
      withStore' $ \db -> updateSndMsgDeliveryStatus db connId msgId MDSSndSent

    agentSndError :: AgentErrorType -> SndError
    agentSndError = \case
      SMP _ AUTH -> SndErrAuth
      SMP _ QUOTA -> SndErrQuota
      BROKER _ e -> brokerError SndErrRelay e
      SMP proxySrv (SMP.PROXY (SMP.BROKER e)) -> brokerError (SndErrProxy proxySrv) e
      AP.PROXY proxySrv _ (ProxyProtocolError (SMP.PROXY (SMP.BROKER e))) -> brokerError (SndErrProxyRelay proxySrv) e
      e -> SndErrOther $ tshow e
      where
        brokerError srvErr = \case
          NETWORK -> SndErrExpired
          TIMEOUT -> SndErrExpired
          HOST -> srvErr SrvErrHost
          SMP.TRANSPORT TEVersion -> srvErr SrvErrVersion
          e -> srvErr . SrvErrOther $ tshow e

    badRcvFileChunk :: RcvFileTransfer -> String -> CM ()
    badRcvFileChunk ft err =
      unless (rcvFileCompleteOrCancelled ft) $ do
        cancelRcvFileTransfer user ft >>= mapM_ (deleteAgentConnectionAsync user)
        throwChatError $ CEFileRcvChunk err

    memberConnectedChatItem :: GroupInfo -> GroupMember -> CM ()
    memberConnectedChatItem gInfo m =
      -- ts should be broker ts but we don't have it for CON
      createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvGroupEvent RGEMemberConnected) Nothing

    groupDescriptionChatItem :: GroupInfo -> GroupMember -> Text -> CM ()
    groupDescriptionChatItem gInfo m descr =
      createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvMsgContent $ MCText descr) Nothing

    notifyMemberConnected :: GroupInfo -> GroupMember -> Maybe Contact -> CM ()
    notifyMemberConnected gInfo m ct_ = do
      memberConnectedChatItem gInfo m
      lift $ mapM_ (`setContactNetworkStatus` NSConnected) ct_
      toView $ CRConnectedToGroupMember user gInfo m ct_

    probeMatchingContactsAndMembers :: Contact -> IncognitoEnabled -> Bool -> CM ()
    probeMatchingContactsAndMembers ct connectedIncognito doProbeContacts = do
      gVar <- asks random
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      if contactMerge && not connectedIncognito
        then do
          (probe, probeId) <- withStore $ \db -> createSentProbe db gVar userId (COMContact ct)
          -- ! when making changes to probe-and-merge mechanism,
          -- ! test scenario in which recipient receives probe after probe hashes (not covered in tests):
          -- sendProbe -> sendProbeHashes (currently)
          -- sendProbeHashes -> sendProbe (reversed - change order in code, may add delay)
          sendProbe probe
          cs <-
            if doProbeContacts
              then map COMContact <$> withStore' (\db -> getMatchingContacts db vr user ct)
              else pure []
          ms <- map COMGroupMember <$> withStore' (\db -> getMatchingMembers db vr user ct)
          sendProbeHashes (cs <> ms) probe probeId
        else sendProbe . Probe =<< liftIO (encodedRandomBytes gVar 32)
      where
        sendProbe :: Probe -> CM ()
        sendProbe probe = void . sendDirectContactMessage user ct $ XInfoProbe probe

    probeMatchingMemberContact :: GroupMember -> IncognitoEnabled -> CM ()
    probeMatchingMemberContact GroupMember {activeConn = Nothing} _ = pure ()
    probeMatchingMemberContact m@GroupMember {groupId, activeConn = Just conn} connectedIncognito = do
      gVar <- asks random
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      if contactMerge && not connectedIncognito
        then do
          (probe, probeId) <- withStore $ \db -> createSentProbe db gVar userId $ COMGroupMember m
          sendProbe probe
          cs <- map COMContact <$> withStore' (\db -> getMatchingMemberContacts db vr user m)
          sendProbeHashes cs probe probeId
        else sendProbe . Probe =<< liftIO (encodedRandomBytes gVar 32)
      where
        sendProbe :: Probe -> CM ()
        sendProbe probe = void $ sendDirectMemberMessage conn (XInfoProbe probe) groupId

    sendProbeHashes :: [ContactOrMember] -> Probe -> Int64 -> CM ()
    sendProbeHashes cgms probe probeId =
      forM_ cgms $ \cgm -> sendProbeHash cgm `catchChatError` \_ -> pure ()
      where
        probeHash = ProbeHash $ C.sha256Hash (unProbe probe)
        sendProbeHash :: ContactOrMember -> CM ()
        sendProbeHash cgm@(COMContact c) = do
          void . sendDirectContactMessage user c $ XInfoProbeCheck probeHash
          withStore' $ \db -> createSentProbeHash db userId probeId cgm
        sendProbeHash (COMGroupMember GroupMember {activeConn = Nothing}) = pure ()
        sendProbeHash cgm@(COMGroupMember m@GroupMember {groupId, activeConn = Just conn}) =
          when (memberCurrent m) $ do
            void $ sendDirectMemberMessage conn (XInfoProbeCheck probeHash) groupId
            withStore' $ \db -> createSentProbeHash db userId probeId cgm

    messageWarning :: Text -> CM ()
    messageWarning = toView . CRMessageError user "warning"

    messageError :: Text -> CM ()
    messageError = toView . CRMessageError user "error"

    newContentMessage :: Contact -> MsgContainer -> RcvMessage -> MsgMeta -> CM ()
    newContentMessage ct@Contact {contactUsed} mc msg@RcvMessage {sharedMsgId_} msgMeta = do
      unless contactUsed $ withStore' $ \db -> updateContactUsed db user ct
      let ExtMsgContent content fInv_ _ _ = mcExtMsgContent mc
      -- Uncomment to test stuck delivery on errors - see test testDirectMessageDelete
      -- case content of
      --   MCText "hello 111" ->
      --     UE.throwIO $ userError "#####################"
      --     -- throwChatError $ CECommandError "#####################"
      --   _ -> pure ()
      if isVoice content && not (featureAllowed SCFVoice forContact ct)
        then do
          void $ newChatItem (CIRcvChatFeatureRejected CFVoice) Nothing Nothing False
        else do
          let ExtMsgContent _ _ itemTTL live_ = mcExtMsgContent mc
              timed_ = rcvContactCITimed ct itemTTL
              live = fromMaybe False live_
          file_ <- processFileInvitation fInv_ content $ \db -> createRcvFileTransfer db userId ct
          newChatItem (CIRcvMsgContent content) (snd <$> file_) timed_ live
          autoAcceptFile file_
      where
        brokerTs = metaBrokerTs msgMeta
        newChatItem ciContent ciFile_ timed_ live = do
          ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs ciContent ciFile_ timed_ live
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getDirectCIReactions db ct sharedMsgId) sharedMsgId_
          toView $ CRNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci {reactions}]

    autoAcceptFile :: Maybe (RcvFileTransfer, CIFile 'MDRcv) -> CM ()
    autoAcceptFile = mapM_ $ \(ft, CIFile {fileSize}) -> do
      -- ! autoAcceptFileSize is only used in tests
      ChatConfig {autoAcceptFileSize = sz} <- asks config
      when (sz > fileSize) $ receiveFile' user ft False Nothing Nothing >>= toView

    messageFileDescription :: Contact -> SharedMsgId -> FileDescr -> CM ()
    messageFileDescription ct@Contact {contactId} sharedMsgId fileDescr = do
      fileId <- withStore $ \db -> getFileIdBySharedMsgId db userId contactId sharedMsgId
      processFDMessage (CDDirectRcv ct) sharedMsgId fileId fileDescr

    groupMessageFileDescription :: GroupInfo -> GroupMember -> SharedMsgId -> FileDescr -> CM ()
    groupMessageFileDescription g@GroupInfo {groupId} m sharedMsgId fileDescr = do
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      processFDMessage (CDGroupRcv g m) sharedMsgId fileId fileDescr

    processFDMessage :: ChatTypeQuotable c => ChatDirection c 'MDRcv -> SharedMsgId -> FileTransferId -> FileDescr -> CM ()
    processFDMessage cd sharedMsgId fileId fileDescr = do
      ft <- withStore $ \db -> getRcvFileTransfer db user fileId
      unless (rcvFileCompleteOrCancelled ft) $ do
        (rfd@RcvFileDescr {fileDescrComplete}, ft'@RcvFileTransfer {fileStatus, xftpRcvFile, cryptoArgs}) <- withStore $ \db -> do
          rfd <- appendRcvFD db userId fileId fileDescr
          -- reading second time in the same transaction as appending description
          -- to prevent race condition with accept
          ft' <- getRcvFileTransfer db user fileId
          pure (rfd, ft')
        when fileDescrComplete $ do
          ci <- withStore $ \db -> getAChatItemBySharedMsgId db user cd sharedMsgId
          toView $ CRRcvFileDescrReady user ci ft' rfd
        case (fileStatus, xftpRcvFile) of
          (RFSAccepted _, Just XFTPRcvFile {userApprovedRelays}) -> receiveViaCompleteFD user fileId rfd userApprovedRelays cryptoArgs
          _ -> pure ()

    processFileInvitation :: Maybe FileInvitation -> MsgContent -> (DB.Connection -> FileInvitation -> Maybe InlineFileMode -> Integer -> ExceptT StoreError IO RcvFileTransfer) -> CM (Maybe (RcvFileTransfer, CIFile 'MDRcv))
    processFileInvitation fInv_ mc createRcvFT = forM fInv_ $ \fInv' -> do
      ChatConfig {fileChunkSize} <- asks config
      let fInv@FileInvitation {fileName, fileSize} = mkValidFileInvitation fInv'
      inline <- receiveInlineMode fInv (Just mc) fileChunkSize
      ft@RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFT db fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
      (filePath, fileStatus, ft') <- case inline of
        Just IFMSent -> do
          encrypt <- chatReadVar encryptLocalFiles
          ft' <- (if encrypt then setFileToEncrypt else pure) ft
          fPath <- getRcvFilePath fileId Nothing fileName True
          withStore' $ \db -> startRcvInlineFT db user ft' fPath inline
          pure (Just fPath, CIFSRcvAccepted, ft')
        _ -> pure (Nothing, CIFSRcvInvitation, ft)
      let RcvFileTransfer {cryptoArgs} = ft'
          fileSource = (`CryptoFile` cryptoArgs) <$> filePath
      pure (ft', CIFile {fileId, fileName, fileSize, fileSource, fileStatus, fileProtocol})

    mkValidFileInvitation :: FileInvitation -> FileInvitation
    mkValidFileInvitation fInv@FileInvitation {fileName} = fInv {fileName = FP.makeValid $ FP.takeFileName fileName}

    messageUpdate :: Contact -> SharedMsgId -> MsgContent -> RcvMessage -> MsgMeta -> Maybe Int -> Maybe Bool -> CM ()
    messageUpdate ct@Contact {contactId} sharedMsgId mc msg@RcvMessage {msgId} msgMeta ttl live_ = do
      updateRcvChatItem `catchCINotFound` \_ -> do
        -- This patches initial sharedMsgId into chat item when locally deleted chat item
        -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
        -- Chat item and update message which created it will have different sharedMsgId in this case...
        let timed_ = rcvContactCITimed ct ttl
        ci <- saveRcvChatItem' user (CDDirectRcv ct) msg (Just sharedMsgId) brokerTs content Nothing timed_ live
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          updateDirectChatItem' db user contactId ci content True live Nothing Nothing
        toView $ CRChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci')
      where
        brokerTs = metaBrokerTs msgMeta
        content = CIRcvMsgContent mc
        live = fromMaybe False live_
        updateRcvChatItem = do
          cci <- withStore $ \db -> getDirectChatItemBySharedMsgId db user contactId sharedMsgId
          case cci of
            CChatItem SMDRcv ci@ChatItem {meta = CIMeta {itemForwarded, itemLive}, content = CIRcvMsgContent oldMC}
              | isNothing itemForwarded -> do
                  let changed = mc /= oldMC
                  if changed || fromMaybe False itemLive
                    then do
                      ci' <- withStore' $ \db -> do
                        when changed $
                          addInitialAndNewCIVersions db (chatItemId' ci) (chatItemTs' ci, oldMC) (brokerTs, mc)
                        reactions <- getDirectCIReactions db ct sharedMsgId
                        let edited = itemLive /= Just True
                        updateDirectChatItem' db user contactId ci {reactions} content edited live Nothing $ Just msgId
                      toView $ CRChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci')
                      startUpdatedTimedItemThread user (ChatRef CTDirect contactId) ci ci'
                    else toView $ CRChatItemNotChanged user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
            _ -> messageError "x.msg.update: contact attempted invalid message update"

    messageDelete :: Contact -> SharedMsgId -> RcvMessage -> MsgMeta -> CM ()
    messageDelete ct@Contact {contactId} sharedMsgId _rcvMessage msgMeta = do
      deleteRcvChatItem `catchCINotFound` (toView . CRChatItemDeletedNotFound user ct)
      where
        brokerTs = metaBrokerTs msgMeta
        deleteRcvChatItem = do
          cci@(CChatItem msgDir ci) <- withStore $ \db -> getDirectChatItemBySharedMsgId db user contactId sharedMsgId
          case msgDir of
            SMDRcv
              | rcvItemDeletable ci brokerTs ->
                  if featureAllowed SCFFullDelete forContact ct
                    then deleteDirectCIs user ct [cci] False False >>= toView
                    else markDirectCIsDeleted user ct [cci] False brokerTs >>= toView
              | otherwise -> messageError "x.msg.del: contact attempted invalid message delete"
            SMDSnd -> messageError "x.msg.del: contact attempted invalid message delete"

    rcvItemDeletable :: ChatItem c d -> UTCTime -> Bool
    rcvItemDeletable ChatItem {meta = CIMeta {itemTs, itemDeleted}} brokerTs =
      -- 78 hours margin to account for possible sending delay
      diffUTCTime brokerTs itemTs < (78 * 3600) && isNothing itemDeleted

    directMsgReaction :: Contact -> SharedMsgId -> MsgReaction -> Bool -> RcvMessage -> MsgMeta -> CM ()
    directMsgReaction ct sharedMsgId reaction add RcvMessage {msgId} MsgMeta {broker = (_, brokerTs)} = do
      when (featureAllowed SCFReactions forContact ct) $ do
        rs <- withStore' $ \db -> getDirectReactions db ct sharedMsgId False
        when (reactionAllowed add reaction rs) $ do
          updateChatItemReaction `catchCINotFound` \_ ->
            withStore' $ \db -> setDirectReaction db ct sharedMsgId False reaction add msgId brokerTs
      where
        updateChatItemReaction = do
          cr_ <- withStore $ \db -> do
            CChatItem md ci <- getDirectChatItemBySharedMsgId db user (contactId' ct) sharedMsgId
            if ciReactionAllowed ci
              then liftIO $ do
                setDirectReaction db ct sharedMsgId False reaction add msgId brokerTs
                reactions <- getDirectCIReactions db ct sharedMsgId
                let ci' = CChatItem md ci {reactions}
                    r = ACIReaction SCTDirect SMDRcv (DirectChat ct) $ CIReaction CIDirectRcv ci' brokerTs reaction
                pure $ Just $ CRChatItemReaction user add r
              else pure Nothing
          mapM_ toView cr_

    groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> MemberId -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM ()
    groupMsgReaction g@GroupInfo {groupId} m sharedMsgId itemMemberId reaction add RcvMessage {msgId} brokerTs = do
      when (groupFeatureAllowed SGFReactions g) $ do
        rs <- withStore' $ \db -> getGroupReactions db g m itemMemberId sharedMsgId False
        when (reactionAllowed add reaction rs) $ do
          updateChatItemReaction `catchCINotFound` \_ ->
            withStore' $ \db -> setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
      where
        updateChatItemReaction = do
          cr_ <- withStore $ \db -> do
            CChatItem md ci <- getGroupMemberCIBySharedMsgId db user groupId itemMemberId sharedMsgId
            if ciReactionAllowed ci
              then liftIO $ do
                setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                reactions <- getGroupCIReactions db g itemMemberId sharedMsgId
                let ci' = CChatItem md ci {reactions}
                    r = ACIReaction SCTGroup SMDRcv (GroupChat g) $ CIReaction (CIGroupRcv m) ci' brokerTs reaction
                pure $ Just $ CRChatItemReaction user add r
              else pure Nothing
          mapM_ toView cr_

    reactionAllowed :: Bool -> MsgReaction -> [MsgReaction] -> Bool
    reactionAllowed add reaction rs = (reaction `elem` rs) /= add && not (add && length rs >= maxMsgReactions)

    catchCINotFound :: CM a -> (SharedMsgId -> CM a) -> CM a
    catchCINotFound f handle =
      f `catchChatError` \case
        ChatErrorStore (SEChatItemSharedMsgIdNotFound sharedMsgId) -> handle sharedMsgId
        e -> throwError e

    newGroupContentMessage :: GroupInfo -> GroupMember -> MsgContainer -> RcvMessage -> UTCTime -> Bool -> CM ()
    newGroupContentMessage gInfo m@GroupMember {memberId, memberRole} mc msg@RcvMessage {sharedMsgId_} brokerTs forwarded
      | blockedByAdmin m = createBlockedByAdmin
      | otherwise = case prohibitedGroupContent gInfo m content fInv_ of
          Just f -> rejected f
          Nothing ->
            withStore' (\db -> getCIModeration db vr user gInfo memberId sharedMsgId_) >>= \case
              Just ciModeration -> do
                applyModeration ciModeration
                withStore' $ \db -> deleteCIModeration db gInfo memberId sharedMsgId_
              Nothing -> createContentItem
      where
        rejected f = void $ newChatItem (CIRcvGroupFeatureRejected f) Nothing Nothing False
        timed' = if forwarded then rcvCITimed_ (Just Nothing) itemTTL else rcvGroupCITimed gInfo itemTTL
        live' = fromMaybe False live_
        ExtMsgContent content fInv_ itemTTL live_ = mcExtMsgContent mc
        createBlockedByAdmin
          | groupFeatureAllowed SGFFullDelete gInfo = do
              ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs CIRcvBlocked Nothing timed' False
              ci' <- withStore' $ \db -> updateGroupCIBlockedByAdmin db user gInfo ci brokerTs
              groupMsgToView gInfo ci'
          | otherwise = do
              file_ <- processFileInv
              ci <- createNonLive file_
              ci' <- withStore' $ \db -> markGroupCIBlockedByAdmin db user gInfo ci
              groupMsgToView gInfo ci'
        applyModeration CIModeration {moderatorMember = moderator@GroupMember {memberRole = moderatorRole}, moderatedAt}
          | moderatorRole < GRModerator || moderatorRole < memberRole =
              createContentItem
          | groupFeatureAllowed SGFFullDelete gInfo = do
              ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs CIRcvModerated Nothing timed' False
              ci' <- withStore' $ \db -> updateGroupChatItemModerated db user gInfo ci moderator moderatedAt
              groupMsgToView gInfo ci'
          | otherwise = do
              file_ <- processFileInv
              ci <- createNonLive file_
              toView =<< markGroupCIsDeleted user gInfo [CChatItem SMDRcv ci] False (Just moderator) moderatedAt
        createNonLive file_ =
          saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs (CIRcvMsgContent content) (snd <$> file_) timed' False
        createContentItem = do
          file_ <- processFileInv
          newChatItem (CIRcvMsgContent content) (snd <$> file_) timed' live'
          when (showMessages $ memberSettings m) $ autoAcceptFile file_
        processFileInv =
          processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId m
        newChatItem ciContent ciFile_ timed_ live = do
          ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs ciContent ciFile_ timed_ live
          ci' <- blockedMember m ci $ withStore' $ \db -> markGroupChatItemBlocked db user gInfo ci
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getGroupCIReactions db gInfo memberId sharedMsgId) sharedMsgId_
          groupMsgToView gInfo ci' {reactions}

    groupMessageUpdate :: GroupInfo -> GroupMember -> SharedMsgId -> MsgContent -> RcvMessage -> UTCTime -> Maybe Int -> Maybe Bool -> CM ()
    groupMessageUpdate gInfo@GroupInfo {groupId} m@GroupMember {groupMemberId, memberId} sharedMsgId mc msg@RcvMessage {msgId} brokerTs ttl_ live_
      | prohibitedSimplexLinks gInfo m mc =
          messageWarning $ "x.msg.update ignored: feature not allowed " <> groupFeatureNameText GFSimplexLinks
      | otherwise = do
          updateRcvChatItem `catchCINotFound` \_ -> do
            -- This patches initial sharedMsgId into chat item when locally deleted chat item
            -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
            -- Chat item and update message which created it will have different sharedMsgId in this case...
            let timed_ = rcvGroupCITimed gInfo ttl_
            ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg (Just sharedMsgId) brokerTs content Nothing timed_ live
            ci' <- withStore' $ \db -> do
              createChatItemVersion db (chatItemId' ci) brokerTs mc
              ci' <- updateGroupChatItem db user groupId ci content True live Nothing
              blockedMember m ci' $ markGroupChatItemBlocked db user gInfo ci'
            toView $ CRChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci')
      where
        content = CIRcvMsgContent mc
        live = fromMaybe False live_
        updateRcvChatItem = do
          cci <- withStore $ \db -> getGroupChatItemBySharedMsgId db user groupId groupMemberId sharedMsgId
          case cci of
            CChatItem SMDRcv ci@ChatItem {chatDir = CIGroupRcv m', meta = CIMeta {itemLive}, content = CIRcvMsgContent oldMC} ->
              if sameMemberId memberId m'
                then do
                  let changed = mc /= oldMC
                  if changed || fromMaybe False itemLive
                    then do
                      ci' <- withStore' $ \db -> do
                        when changed $
                          addInitialAndNewCIVersions db (chatItemId' ci) (chatItemTs' ci, oldMC) (brokerTs, mc)
                        reactions <- getGroupCIReactions db gInfo memberId sharedMsgId
                        let edited = itemLive /= Just True
                        updateGroupChatItem db user groupId ci {reactions} content edited live $ Just msgId
                      toView $ CRChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci')
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId) ci ci'
                    else toView $ CRChatItemNotChanged user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)
                else messageError "x.msg.update: group member attempted to update a message of another member"
            _ -> messageError "x.msg.update: group member attempted invalid message update"

    groupMessageDelete :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> RcvMessage -> UTCTime -> CM ()
    groupMessageDelete gInfo@GroupInfo {groupId, membership} m@GroupMember {memberId, memberRole = senderRole} sharedMsgId sndMemberId_ RcvMessage {msgId} brokerTs = do
      let msgMemberId = fromMaybe memberId sndMemberId_
      withStore' (\db -> runExceptT $ getGroupMemberCIBySharedMsgId db user groupId msgMemberId sharedMsgId) >>= \case
        Right cci@(CChatItem _ ci@ChatItem {chatDir}) -> case chatDir of
          CIGroupRcv mem -> case sndMemberId_ of
            -- regular deletion
            Nothing
              | sameMemberId memberId mem && msgMemberId == memberId && rcvItemDeletable ci brokerTs ->
                  delete cci Nothing >>= toView
              | otherwise ->
                  messageError "x.msg.del: member attempted invalid message delete"
            -- moderation (not limited by time)
            Just _
              | sameMemberId memberId mem && msgMemberId == memberId ->
                  delete cci (Just m) >>= toView
              | otherwise ->
                  moderate mem cci
          CIGroupSnd -> moderate membership cci
        Left e
          | msgMemberId == memberId -> messageError $ "x.msg.del: message not found, " <> tshow e
          | senderRole < GRModerator -> messageError $ "x.msg.del: message not found, message of another member with insufficient member permissions, " <> tshow e
          | otherwise -> withStore' $ \db -> createCIModeration db gInfo m msgMemberId sharedMsgId msgId brokerTs
      where
        moderate :: GroupMember -> CChatItem 'CTGroup -> CM ()
        moderate mem cci = case sndMemberId_ of
          Just sndMemberId
            | sameMemberId sndMemberId mem -> checkRole mem $ delete cci (Just m) >>= toView
            | otherwise -> messageError "x.msg.del: message of another member with incorrect memberId"
          _ -> messageError "x.msg.del: message of another member without memberId"
        checkRole GroupMember {memberRole} a
          | senderRole < GRModerator || senderRole < memberRole =
              messageError "x.msg.del: message of another member with insufficient member permissions"
          | otherwise = a
        delete :: CChatItem 'CTGroup -> Maybe GroupMember -> CM ChatResponse
        delete cci byGroupMember
          | groupFeatureAllowed SGFFullDelete gInfo = deleteGroupCIs user gInfo [cci] False False byGroupMember brokerTs
          | otherwise = markGroupCIsDeleted user gInfo [cci] False byGroupMember brokerTs

    -- TODO remove once XFile is discontinued
    processFileInvitation' :: Contact -> FileInvitation -> RcvMessage -> MsgMeta -> CM ()
    processFileInvitation' ct fInv' msg@RcvMessage {sharedMsgId_} msgMeta = do
      ChatConfig {fileChunkSize} <- asks config
      let fInv@FileInvitation {fileName, fileSize} = mkValidFileInvitation fInv'
      inline <- receiveInlineMode fInv Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFileTransfer db userId ct fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
      ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs (CIRcvMsgContent $ MCFile "") ciFile Nothing False
      toView $ CRNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]
      where
        brokerTs = metaBrokerTs msgMeta

    -- TODO remove once XFile is discontinued
    processGroupFileInvitation' :: GroupInfo -> GroupMember -> FileInvitation -> RcvMessage -> UTCTime -> CM ()
    processGroupFileInvitation' gInfo m fInv@FileInvitation {fileName, fileSize} msg@RcvMessage {sharedMsgId_} brokerTs = do
      ChatConfig {fileChunkSize} <- asks config
      inline <- receiveInlineMode fInv Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvGroupFileTransfer db userId m fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
      ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs (CIRcvMsgContent $ MCFile "") ciFile Nothing False
      ci' <- blockedMember m ci $ withStore' $ \db -> markGroupChatItemBlocked db user gInfo ci
      groupMsgToView gInfo ci'

    blockedMember :: Monad m' => GroupMember -> ChatItem c d -> m' (ChatItem c d) -> m' (ChatItem c d)
    blockedMember m ci blockedCI
      | showMessages (memberSettings m) = pure ci
      | otherwise = blockedCI

    receiveInlineMode :: FileInvitation -> Maybe MsgContent -> Integer -> CM (Maybe InlineFileMode)
    receiveInlineMode FileInvitation {fileSize, fileInline, fileDescr} mc_ chSize = case (fileInline, fileDescr) of
      (Just mode, Nothing) -> do
        InlineFilesConfig {receiveChunks, receiveInstant} <- asks $ inlineFiles . config
        pure $ if fileSize <= receiveChunks * chSize then inline' receiveInstant else Nothing
        where
          inline' receiveInstant = if mode == IFMOffer || (receiveInstant && maybe False isVoice mc_) then fileInline else Nothing
      _ -> pure Nothing

    xFileCancel :: Contact -> SharedMsgId -> CM ()
    xFileCancel Contact {contactId} sharedMsgId = do
      fileId <- withStore $ \db -> getFileIdBySharedMsgId db userId contactId sharedMsgId
      ft <- withStore (\db -> getRcvFileTransfer db user fileId)
      unless (rcvFileCompleteOrCancelled ft) $ do
        cancelRcvFileTransfer user ft >>= mapM_ (deleteAgentConnectionAsync user)
        ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
        toView $ CRRcvFileSndCancelled user ci ft

    xFileAcptInv :: Contact -> SharedMsgId -> Maybe ConnReqInvitation -> String -> CM ()
    xFileAcptInv ct sharedMsgId fileConnReq_ fName = do
      fileId <- withStore $ \db -> getDirectFileIdBySharedMsgId db user ct sharedMsgId
      (AChatItem _ _ _ ci) <- withStore $ \db -> getChatItemByFileId db vr user fileId
      assertSMPAcceptNotProhibited ci
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      -- [async agent commands] no continuation needed, but command should be asynchronous for stability
      if fName == fileName
        then unless cancelled $ case fileConnReq_ of
          -- receiving via a separate connection
          Just fileConnReq -> do
            subMode <- chatReadVar subscriptionMode
            dm <- encodeConnInfo XOk
            connIds <- joinAgentConnectionAsync user True fileConnReq dm subMode
            withStore' $ \db -> createSndDirectFTConnection db vr user fileId connIds subMode
          -- receiving inline
          _ -> do
            event <- withStore $ \db -> do
              ci' <- updateDirectCIFileStatus db vr user fileId $ CIFSSndTransfer 0 1
              sft <- createSndDirectInlineFT db ct ft
              pure $ CRSndFileStart user ci' sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendDirectFileInline user ct ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
        else messageError "x.file.acpt.inv: fileName is different from expected"

    assertSMPAcceptNotProhibited :: ChatItem c d -> CM ()
    assertSMPAcceptNotProhibited ChatItem {file = Just CIFile {fileId, fileProtocol}, content}
      | fileProtocol == FPXFTP && not (imageOrVoice content) = throwChatError $ CEFallbackToSMPProhibited fileId
      | otherwise = pure ()
      where
        imageOrVoice :: CIContent d -> Bool
        imageOrVoice (CISndMsgContent (MCImage _ _)) = True
        imageOrVoice (CISndMsgContent (MCVoice _ _)) = True
        imageOrVoice _ = False
    assertSMPAcceptNotProhibited _ = pure ()

    checkSndInlineFTComplete :: Connection -> AgentMsgId -> CM ()
    checkSndInlineFTComplete conn agentMsgId = do
      sft_ <- withStore' $ \db -> getSndFTViaMsgDelivery db user conn agentMsgId
      forM_ sft_ $ \sft@SndFileTransfer {fileId} -> do
        ci@(AChatItem _ _ _ ChatItem {file}) <- withStore $ \db -> do
          liftIO $ updateSndFileStatus db sft FSComplete
          liftIO $ deleteSndFileChunks db sft
          updateDirectCIFileStatus db vr user fileId CIFSSndComplete
        case file of
          Just CIFile {fileProtocol = FPXFTP} -> do
            ft <- withStore $ \db -> getFileTransferMeta db user fileId
            toView $ CRSndFileCompleteXFTP user ci ft
          _ -> toView $ CRSndFileComplete user ci sft

    allowSendInline :: Integer -> Maybe InlineFileMode -> CM Bool
    allowSendInline fileSize = \case
      Just IFMOffer -> do
        ChatConfig {fileChunkSize, inlineFiles} <- asks config
        pure $ fileSize <= fileChunkSize * offerChunks inlineFiles
      _ -> pure False

    bFileChunk :: Contact -> SharedMsgId -> FileChunk -> MsgMeta -> CM ()
    bFileChunk ct sharedMsgId chunk meta = do
      ft <- withStore $ \db -> getDirectFileIdBySharedMsgId db user ct sharedMsgId >>= getRcvFileTransfer db user
      receiveInlineChunk ft chunk meta

    bFileChunkGroup :: GroupInfo -> SharedMsgId -> FileChunk -> MsgMeta -> CM ()
    bFileChunkGroup GroupInfo {groupId} sharedMsgId chunk meta = do
      ft <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId >>= getRcvFileTransfer db user
      receiveInlineChunk ft chunk meta

    receiveInlineChunk :: RcvFileTransfer -> FileChunk -> MsgMeta -> CM ()
    receiveInlineChunk RcvFileTransfer {fileId, fileStatus = RFSNew} FileChunk {chunkNo} _
      | chunkNo == 1 = throwChatError $ CEInlineFileProhibited fileId
      | otherwise = pure ()
    receiveInlineChunk ft@RcvFileTransfer {fileId} chunk meta = do
      case chunk of
        FileChunk {chunkNo} -> when (chunkNo == 1) $ startReceivingFile user fileId
        _ -> pure ()
      receiveFileChunk ft Nothing meta chunk

    xFileCancelGroup :: GroupInfo -> GroupMember -> SharedMsgId -> CM ()
    xFileCancelGroup GroupInfo {groupId} GroupMember {groupMemberId, memberId} sharedMsgId = do
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      CChatItem msgDir ChatItem {chatDir} <- withStore $ \db -> getGroupChatItemBySharedMsgId db user groupId groupMemberId sharedMsgId
      case (msgDir, chatDir) of
        (SMDRcv, CIGroupRcv m) -> do
          if sameMemberId memberId m
            then do
              ft <- withStore (\db -> getRcvFileTransfer db user fileId)
              unless (rcvFileCompleteOrCancelled ft) $ do
                cancelRcvFileTransfer user ft >>= mapM_ (deleteAgentConnectionAsync user)
                ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
                toView $ CRRcvFileSndCancelled user ci ft
            else messageError "x.file.cancel: group member attempted to cancel file of another member" -- shouldn't happen now that query includes group member id
        (SMDSnd, _) -> messageError "x.file.cancel: group member attempted invalid file cancel"

    xFileAcptInvGroup :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe ConnReqInvitation -> String -> CM ()
    xFileAcptInvGroup GroupInfo {groupId} m@GroupMember {activeConn} sharedMsgId fileConnReq_ fName = do
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      (AChatItem _ _ _ ci) <- withStore $ \db -> getChatItemByFileId db vr user fileId
      assertSMPAcceptNotProhibited ci
      -- TODO check that it's not already accepted
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      if fName == fileName
        then unless cancelled $ case (fileConnReq_, activeConn) of
          (Just fileConnReq, _) -> do
            subMode <- chatReadVar subscriptionMode
            -- receiving via a separate connection
            -- [async agent commands] no continuation needed, but command should be asynchronous for stability
            dm <- encodeConnInfo XOk
            connIds <- joinAgentConnectionAsync user True fileConnReq dm subMode
            withStore' $ \db -> createSndGroupFileTransferConnection db vr user fileId connIds m subMode
          (_, Just conn) -> do
            -- receiving inline
            event <- withStore $ \db -> do
              ci' <- updateDirectCIFileStatus db vr user fileId $ CIFSSndTransfer 0 1
              sft <- liftIO $ createSndGroupInlineFT db m conn ft
              pure $ CRSndFileStart user ci' sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendMemberFileInline m conn ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
          _ -> messageError "x.file.acpt.inv: member connection is not active"
        else messageError "x.file.acpt.inv: fileName is different from expected"

    groupMsgToView :: forall d. MsgDirectionI d => GroupInfo -> ChatItem 'CTGroup d -> CM ()
    groupMsgToView gInfo ci =
      toView $ CRNewChatItems user [AChatItem SCTGroup (msgDirection @d) (GroupChat gInfo) ci]

    processGroupInvitation :: Contact -> GroupInvitation -> RcvMessage -> MsgMeta -> CM ()
    processGroupInvitation ct inv msg msgMeta = do
      let Contact {localDisplayName = c, activeConn} = ct
          GroupInvitation {fromMember = (MemberIdRole fromMemId fromRole), invitedMember = (MemberIdRole memId memRole), connRequest, groupLinkId} = inv
      forM_ activeConn $ \Connection {connId, connChatVersion, peerChatVRange, customUserProfileId, groupLinkId = groupLinkId'} -> do
        when (fromRole < GRAdmin || fromRole < memRole) $ throwChatError (CEGroupContactRole c)
        when (fromMemId == memId) $ throwChatError CEGroupDuplicateMemberId
        -- [incognito] if direct connection with host is incognito, create membership using the same incognito profile
        (gInfo@GroupInfo {groupId, localDisplayName, groupProfile, membership}, hostId) <- withStore $ \db -> createGroupInvitation db vr user ct inv customUserProfileId
        let GroupMember {groupMemberId, memberId = membershipMemId} = membership
        if sameGroupLinkId groupLinkId groupLinkId'
          then do
            subMode <- chatReadVar subscriptionMode
            dm <- encodeConnInfo $ XGrpAcpt membershipMemId
            connIds <- joinAgentConnectionAsync user True connRequest dm subMode
            withStore' $ \db -> do
              setViaGroupLinkHash db groupId connId
              createMemberConnectionAsync db user hostId connIds connChatVersion peerChatVRange subMode
              updateGroupMemberStatusById db userId hostId GSMemAccepted
              updateGroupMemberStatus db userId membership GSMemAccepted
            toView $ CRUserAcceptedGroupSent user gInfo {membership = membership {memberStatus = GSMemAccepted}} (Just ct)
          else do
            let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
            ci <- saveRcvChatItem user (CDDirectRcv ct) msg brokerTs content
            withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
            toView $ CRNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]
            toView $ CRReceivedGroupInvitation {user, groupInfo = gInfo, contact = ct, fromMemberRole = fromRole, memberRole = memRole}
      where
        brokerTs = metaBrokerTs msgMeta
        sameGroupLinkId :: Maybe GroupLinkId -> Maybe GroupLinkId -> Bool
        sameGroupLinkId (Just gli) (Just gli') = gli == gli'
        sameGroupLinkId _ _ = False

    checkIntegrityCreateItem :: forall c. ChatTypeI c => ChatDirection c 'MDRcv -> MsgMeta -> CM ()
    checkIntegrityCreateItem cd MsgMeta {integrity, broker = (_, brokerTs)} = case integrity of
      MsgOk -> pure ()
      MsgError e -> createInternalChatItem user cd (CIRcvIntegrityError e) (Just brokerTs)

    xInfo :: Contact -> Profile -> CM ()
    xInfo c p' = void $ processContactProfileUpdate c p' True

    xDirectDel :: Contact -> RcvMessage -> MsgMeta -> CM ()
    xDirectDel c msg msgMeta =
      if directOrUsed c
        then do
          ct' <- withStore' $ \db -> updateContactStatus db user c CSDeleted
          contactConns <- withStore' $ \db -> getContactConnections db vr userId ct'
          deleteAgentConnectionsAsync user $ map aConnId contactConns
          forM_ contactConns $ \conn -> withStore' $ \db -> updateConnectionStatus db conn ConnDeleted
          activeConn' <- forM (contactConn ct') $ \conn -> pure conn {connStatus = ConnDeleted}
          let ct'' = ct' {activeConn = activeConn'} :: Contact
          ci <- saveRcvChatItem user (CDDirectRcv ct'') msg brokerTs (CIRcvDirectEvent RDEContactDeleted)
          toView $ CRNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct'') ci]
          toView $ CRContactDeletedByContact user ct''
        else do
          contactConns <- withStore' $ \db -> getContactConnections db vr userId c
          deleteAgentConnectionsAsync user $ map aConnId contactConns
          withStore $ \db -> deleteContact db user c
      where
        brokerTs = metaBrokerTs msgMeta

    processContactProfileUpdate :: Contact -> Profile -> Bool -> CM Contact
    processContactProfileUpdate c@Contact {profile = lp} p' createItems
      | p /= p' = do
          c' <- withStore $ \db ->
            if userTTL == rcvTTL
              then updateContactProfile db user c p'
              else do
                c' <- liftIO $ updateContactUserPreferences db user c ctUserPrefs'
                updateContactProfile db user c' p'
          when (directOrUsed c' && createItems) $ do
            createProfileUpdatedItem c'
            lift $ createRcvFeatureItems user c c'
          toView $ CRContactUpdated user c c'
          pure c'
      | otherwise =
          pure c
      where
        p = fromLocalProfile lp
        Contact {userPreferences = ctUserPrefs@Preferences {timedMessages = ctUserTMPref}} = c
        userTTL = prefParam $ getPreference SCFTimedMessages ctUserPrefs
        Profile {preferences = rcvPrefs_} = p'
        rcvTTL = prefParam $ getPreference SCFTimedMessages rcvPrefs_
        ctUserPrefs' =
          let userDefault = getPreference SCFTimedMessages (fullPreferences user)
              userDefaultTTL = prefParam userDefault
              ctUserTMPref' = case ctUserTMPref of
                Just userTM -> Just (userTM :: TimedMessagesPreference) {ttl = rcvTTL}
                _
                  | rcvTTL /= userDefaultTTL -> Just (userDefault :: TimedMessagesPreference) {ttl = rcvTTL}
                  | otherwise -> Nothing
           in setPreference_ SCFTimedMessages ctUserTMPref' ctUserPrefs
        createProfileUpdatedItem c' =
          when visibleProfileUpdated $ do
            let ciContent = CIRcvDirectEvent $ RDEProfileUpdated p p'
            createInternalChatItem user (CDDirectRcv c') ciContent Nothing
          where
            visibleProfileUpdated =
              n' /= n || fn' /= fn || i' /= i || cl' /= cl
            Profile {displayName = n, fullName = fn, image = i, contactLink = cl} = p
            Profile {displayName = n', fullName = fn', image = i', contactLink = cl'} = p'

    xInfoMember :: GroupInfo -> GroupMember -> Profile -> UTCTime -> CM ()
    xInfoMember gInfo m p' brokerTs = void $ processMemberProfileUpdate gInfo m p' True (Just brokerTs)

    xGrpLinkMem :: GroupInfo -> GroupMember -> Connection -> Profile -> CM ()
    xGrpLinkMem gInfo@GroupInfo {membership, businessChat} m@GroupMember {groupMemberId, memberCategory} Connection {viaGroupLink} p' = do
      xGrpLinkMemReceived <- withStore $ \db -> getXGrpLinkMemReceived db groupMemberId
      if (viaGroupLink || isJust businessChat) && isNothing (memberContactId m) && memberCategory == GCHostMember && not xGrpLinkMemReceived
        then do
          m' <- processMemberProfileUpdate gInfo m p' False Nothing
          withStore' $ \db -> setXGrpLinkMemReceived db groupMemberId True
          let connectedIncognito = memberIncognito membership
          probeMatchingMemberContact m' connectedIncognito
        else messageError "x.grp.link.mem error: invalid group link host profile update"

    processMemberProfileUpdate :: GroupInfo -> GroupMember -> Profile -> Bool -> Maybe UTCTime -> CM GroupMember
    processMemberProfileUpdate gInfo m@GroupMember {memberProfile = p, memberContactId} p' createItems itemTs_
      | redactedMemberProfile (fromLocalProfile p) /= redactedMemberProfile p' = do
          updateBusinessChatProfile gInfo
          case memberContactId of
            Nothing -> do
              m' <- withStore $ \db -> updateMemberProfile db user m p'
              createProfileUpdatedItem m'
              toView $ CRGroupMemberUpdated user gInfo m m'
              pure m'
            Just mContactId -> do
              mCt <- withStore $ \db -> getContact db vr user mContactId
              if canUpdateProfile mCt
                then do
                  (m', ct') <- withStore $ \db -> updateContactMemberProfile db user m mCt p'
                  createProfileUpdatedItem m'
                  toView $ CRGroupMemberUpdated user gInfo m m'
                  toView $ CRContactUpdated user mCt ct'
                  pure m'
                else pure m
              where
                canUpdateProfile ct
                  | not (contactActive ct) = True
                  | otherwise = case contactConn ct of
                      Nothing -> True
                      Just conn -> not (connReady conn) || (authErrCounter conn >= 1)
      | otherwise =
          pure m
      where
        updateBusinessChatProfile g@GroupInfo {businessChat} = case businessChat of
          Just bc | isMainBusinessMember bc m -> do
            g' <- withStore $ \db -> updateGroupProfileFromMember db user g p'
            toView $ CRGroupUpdated user g g' (Just m)
          _ -> pure ()
        isMainBusinessMember BusinessChatInfo {chatType, businessId, customerId} GroupMember {memberId} = case chatType of
          BCBusiness -> businessId == memberId
          BCCustomer -> customerId == memberId
        createProfileUpdatedItem m' =
          when createItems $ do
            let ciContent = CIRcvGroupEvent $ RGEMemberProfileUpdated (fromLocalProfile p) p'
            createInternalChatItem user (CDGroupRcv gInfo m') ciContent itemTs_

    createFeatureEnabledItems :: Contact -> CM ()
    createFeatureEnabledItems ct@Contact {mergedPreferences} =
      forM_ allChatFeatures $ \(ACF f) -> do
        let state = featureState $ getContactUserPreference f mergedPreferences
        createInternalChatItem user (CDDirectRcv ct) (uncurry (CIRcvChatFeature $ chatFeature f) state) Nothing

    xInfoProbe :: ContactOrMember -> Probe -> CM ()
    xInfoProbe cgm2 probe = do
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      -- [incognito] unless connected incognito
      when (contactMerge && not (contactOrMemberIncognito cgm2)) $ do
        cgm1s <- withStore' $ \db -> matchReceivedProbe db vr user cgm2 probe
        let cgm1s' = filter (not . contactOrMemberIncognito) cgm1s
        probeMatches cgm1s' cgm2
      where
        probeMatches :: [ContactOrMember] -> ContactOrMember -> CM ()
        probeMatches [] _ = pure ()
        probeMatches (cgm1' : cgm1s') cgm2' = do
          cgm2''_ <- probeMatch cgm1' cgm2' probe `catchChatError` \_ -> pure (Just cgm2')
          let cgm2'' = fromMaybe cgm2' cgm2''_
          probeMatches cgm1s' cgm2''

    xInfoProbeCheck :: ContactOrMember -> ProbeHash -> CM ()
    xInfoProbeCheck cgm1 probeHash = do
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      -- [incognito] unless connected incognito
      when (contactMerge && not (contactOrMemberIncognito cgm1)) $ do
        cgm2Probe_ <- withStore' $ \db -> matchReceivedProbeHash db vr user cgm1 probeHash
        forM_ cgm2Probe_ $ \(cgm2, probe) ->
          unless (contactOrMemberIncognito cgm2) . void $
            probeMatch cgm1 cgm2 probe

    probeMatch :: ContactOrMember -> ContactOrMember -> Probe -> CM (Maybe ContactOrMember)
    probeMatch cgm1 cgm2 probe =
      case cgm1 of
        COMContact c1@Contact {contactId = cId1, profile = p1} ->
          case cgm2 of
            COMContact c2@Contact {contactId = cId2, profile = p2}
              | cId1 /= cId2 && profilesMatch p1 p2 -> do
                  void . sendDirectContactMessage user c1 $ XInfoProbeOk probe
                  COMContact <$$> mergeContacts c1 c2
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or same contact id" >> pure Nothing
            COMGroupMember m2@GroupMember {memberProfile = p2, memberContactId}
              | isNothing memberContactId && profilesMatch p1 p2 -> do
                  void . sendDirectContactMessage user c1 $ XInfoProbeOk probe
                  COMContact <$$> associateMemberAndContact c1 m2
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or member already has contact" >> pure Nothing
        COMGroupMember GroupMember {activeConn = Nothing} -> pure Nothing
        COMGroupMember m1@GroupMember {groupId, memberProfile = p1, memberContactId, activeConn = Just conn} ->
          case cgm2 of
            COMContact c2@Contact {profile = p2}
              | memberCurrent m1 && isNothing memberContactId && profilesMatch p1 p2 -> do
                  void $ sendDirectMemberMessage conn (XInfoProbeOk probe) groupId
                  COMContact <$$> associateMemberAndContact c2 m1
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or member already has contact or member not current" >> pure Nothing
            COMGroupMember _ -> messageWarning "probeMatch ignored: members are not matched with members" >> pure Nothing

    xInfoProbeOk :: ContactOrMember -> Probe -> CM ()
    xInfoProbeOk cgm1 probe = do
      cgm2 <- withStore' $ \db -> matchSentProbe db vr user cgm1 probe
      case cgm1 of
        COMContact c1@Contact {contactId = cId1} ->
          case cgm2 of
            Just (COMContact c2@Contact {contactId = cId2})
              | cId1 /= cId2 -> void $ mergeContacts c1 c2
              | otherwise -> messageWarning "xInfoProbeOk ignored: same contact id"
            Just (COMGroupMember m2@GroupMember {memberContactId})
              | isNothing memberContactId -> void $ associateMemberAndContact c1 m2
              | otherwise -> messageWarning "xInfoProbeOk ignored: member already has contact"
            _ -> pure ()
        COMGroupMember m1@GroupMember {memberContactId} ->
          case cgm2 of
            Just (COMContact c2)
              | isNothing memberContactId -> void $ associateMemberAndContact c2 m1
              | otherwise -> messageWarning "xInfoProbeOk ignored: member already has contact"
            Just (COMGroupMember _) -> messageWarning "xInfoProbeOk ignored: members are not matched with members"
            _ -> pure ()

    -- to party accepting call
    xCallInv :: Contact -> CallId -> CallInvitation -> RcvMessage -> MsgMeta -> CM ()
    xCallInv ct@Contact {contactId} callId CallInvitation {callType, callDhPubKey} msg@RcvMessage {sharedMsgId_} msgMeta = do
      if featureAllowed SCFCalls forContact ct
        then do
          g <- asks random
          dhKeyPair <- atomically $ if encryptedCall callType then Just <$> C.generateKeyPair g else pure Nothing
          ci <- saveCallItem CISCallPending
          callUUID <- UUID.toText <$> liftIO V4.nextRandom
          let sharedKey = C.Key . C.dhBytes' <$> (C.dh' <$> callDhPubKey <*> (snd <$> dhKeyPair))
              callState = CallInvitationReceived {peerCallType = callType, localDhPubKey = fst <$> dhKeyPair, sharedKey}
              call' = Call {contactId, callId, callUUID, chatItemId = chatItemId' ci, callState, callTs = chatItemTs' ci}
          calls <- asks currentCalls
          -- theoretically, the new call invitation for the current contact can mark the in-progress call as ended
          -- (and replace it in ChatController)
          -- practically, this should not happen
          withStore' $ \db -> createCall db user call' $ chatItemTs' ci
          call_ <- atomically (TM.lookupInsert contactId call' calls)
          forM_ call_ $ \call -> updateCallItemStatus user ct call WCSDisconnected Nothing
          toView $ CRCallInvitation RcvCallInvitation {user, contact = ct, callType, sharedKey, callUUID, callTs = chatItemTs' ci}
          toView $ CRNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]
        else featureRejected CFCalls
      where
        brokerTs = metaBrokerTs msgMeta
        saveCallItem status = saveRcvChatItem user (CDDirectRcv ct) msg brokerTs (CIRcvCall status 0)
        featureRejected f = do
          ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs (CIRcvChatFeatureRejected f) Nothing Nothing False
          toView $ CRNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]

    -- to party initiating call
    xCallOffer :: Contact -> CallId -> CallOffer -> RcvMessage -> CM ()
    xCallOffer ct callId CallOffer {callType, rtcSession, callDhPubKey} msg = do
      msgCurrentCall ct callId "x.call.offer" msg $
        \call -> case callState call of
          CallInvitationSent {localCallType, localDhPrivKey} -> do
            let sharedKey = C.Key . C.dhBytes' <$> (C.dh' <$> callDhPubKey <*> localDhPrivKey)
                callState' = CallOfferReceived {localCallType, peerCallType = callType, peerCallSession = rtcSession, sharedKey}
                askConfirmation = encryptedCall localCallType && not (encryptedCall callType)
            toView CRCallOffer {user, contact = ct, callType, offer = rtcSession, sharedKey, askConfirmation}
            pure (Just call {callState = callState'}, Just . ACIContent SMDSnd $ CISndCall CISCallAccepted 0)
          _ -> do
            msgCallStateError "x.call.offer" call
            pure (Just call, Nothing)

    -- to party accepting call
    xCallAnswer :: Contact -> CallId -> CallAnswer -> RcvMessage -> CM ()
    xCallAnswer ct callId CallAnswer {rtcSession} msg = do
      msgCurrentCall ct callId "x.call.answer" msg $
        \call -> case callState call of
          CallOfferSent {localCallType, peerCallType, localCallSession, sharedKey} -> do
            let callState' = CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession = rtcSession, sharedKey}
            toView $ CRCallAnswer user ct rtcSession
            pure (Just call {callState = callState'}, Just . ACIContent SMDRcv $ CIRcvCall CISCallNegotiated 0)
          _ -> do
            msgCallStateError "x.call.answer" call
            pure (Just call, Nothing)

    -- to any call party
    xCallExtra :: Contact -> CallId -> CallExtraInfo -> RcvMessage -> CM ()
    xCallExtra ct callId CallExtraInfo {rtcExtraInfo} msg = do
      msgCurrentCall ct callId "x.call.extra" msg $
        \call -> case callState call of
          CallOfferReceived {localCallType, peerCallType, peerCallSession, sharedKey} -> do
            -- TODO update the list of ice servers in peerCallSession
            let callState' = CallOfferReceived {localCallType, peerCallType, peerCallSession, sharedKey}
            toView $ CRCallExtraInfo user ct rtcExtraInfo
            pure (Just call {callState = callState'}, Nothing)
          CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey} -> do
            -- TODO update the list of ice servers in peerCallSession
            let callState' = CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey}
            toView $ CRCallExtraInfo user ct rtcExtraInfo
            pure (Just call {callState = callState'}, Nothing)
          _ -> do
            msgCallStateError "x.call.extra" call
            pure (Just call, Nothing)

    -- to any call party
    xCallEnd :: Contact -> CallId -> RcvMessage -> CM ()
    xCallEnd ct callId msg =
      msgCurrentCall ct callId "x.call.end" msg $ \Call {chatItemId} -> do
        toView $ CRCallEnded user ct
        (Nothing,) <$> callStatusItemContent user ct chatItemId WCSDisconnected

    msgCurrentCall :: Contact -> CallId -> Text -> RcvMessage -> (Call -> CM (Maybe Call, Maybe ACIContent)) -> CM ()
    msgCurrentCall ct@Contact {contactId = ctId'} callId' eventName RcvMessage {msgId} action = do
      calls <- asks currentCalls
      atomically (TM.lookup ctId' calls) >>= \case
        Nothing -> messageError $ eventName <> ": no current call"
        Just call@Call {contactId, callId, chatItemId}
          | contactId /= ctId' || callId /= callId' -> messageError $ eventName <> ": wrong contact or callId"
          | otherwise -> do
              (call_, aciContent_) <- action call
              case call_ of
                Just call' -> do
                  unless (isRcvInvitation call') $ withStore' $ \db -> deleteCalls db user ctId'
                  atomically $ TM.insert ctId' call' calls
                _ -> do
                  withStore' $ \db -> deleteCalls db user ctId'
                  atomically $ TM.delete ctId' calls
              forM_ aciContent_ $ \aciContent -> do
                timed_ <- callTimed ct aciContent
                updateDirectChatItemView user ct chatItemId aciContent False False timed_ $ Just msgId
                forM_ (timed_ >>= timedDeleteAt') $
                  startProximateTimedItemThread user (ChatRef CTDirect ctId', chatItemId)

    msgCallStateError :: Text -> Call -> CM ()
    msgCallStateError eventName Call {callState} =
      messageError $ eventName <> ": wrong call state " <> T.pack (show $ callStateTag callState)

    mergeContacts :: Contact -> Contact -> CM (Maybe Contact)
    mergeContacts c1 c2 = do
      let Contact {localDisplayName = cLDN1, profile = LocalProfile {displayName}} = c1
          Contact {localDisplayName = cLDN2} = c2
      case (suffixOrd displayName cLDN1, suffixOrd displayName cLDN2) of
        (Just cOrd1, Just cOrd2)
          | cOrd1 < cOrd2 -> merge c1 c2
          | cOrd2 < cOrd1 -> merge c2 c1
          | otherwise -> pure Nothing
        _ -> pure Nothing
      where
        merge c1' c2' = do
          c2'' <- withStore $ \db -> mergeContactRecords db vr user c1' c2'
          toView $ CRContactsMerged user c1' c2' c2''
          when (directOrUsed c2'') $ showSecurityCodeChanged c2''
          pure $ Just c2''
          where
            showSecurityCodeChanged mergedCt = do
              let sc1_ = contactSecurityCode c1'
                  sc2_ = contactSecurityCode c2'
                  scMerged_ = contactSecurityCode mergedCt
              case (sc1_, sc2_) of
                (Just sc1, Nothing)
                  | scMerged_ /= Just sc1 -> securityCodeChanged mergedCt
                  | otherwise -> pure ()
                (Nothing, Just sc2)
                  | scMerged_ /= Just sc2 -> securityCodeChanged mergedCt
                  | otherwise -> pure ()
                _ -> pure ()

    associateMemberAndContact :: Contact -> GroupMember -> CM (Maybe Contact)
    associateMemberAndContact c m = do
      let Contact {localDisplayName = cLDN, profile = LocalProfile {displayName}} = c
          GroupMember {localDisplayName = mLDN} = m
      case (suffixOrd displayName cLDN, suffixOrd displayName mLDN) of
        (Just cOrd, Just mOrd)
          | cOrd < mOrd -> Just <$> associateMemberWithContact c m
          | mOrd < cOrd -> Just <$> associateContactWithMember m c
          | otherwise -> pure Nothing
        _ -> pure Nothing

    suffixOrd :: ContactName -> ContactName -> Maybe Int
    suffixOrd displayName localDisplayName
      | localDisplayName == displayName = Just 0
      | otherwise = case T.stripPrefix (displayName <> "_") localDisplayName of
          Just suffix -> readMaybe $ T.unpack suffix
          Nothing -> Nothing

    associateMemberWithContact :: Contact -> GroupMember -> CM Contact
    associateMemberWithContact c1 m2@GroupMember {groupId} = do
      withStore' $ \db -> associateMemberWithContactRecord db user c1 m2
      g <- withStore $ \db -> getGroupInfo db vr user groupId
      toView $ CRContactAndMemberAssociated user c1 g m2 c1
      pure c1

    associateContactWithMember :: GroupMember -> Contact -> CM Contact
    associateContactWithMember m1@GroupMember {groupId} c2 = do
      c2' <- withStore $ \db -> associateContactWithMemberRecord db vr user m1 c2
      g <- withStore $ \db -> getGroupInfo db vr user groupId
      toView $ CRContactAndMemberAssociated user c2 g m1 c2'
      pure c2'

    saveConnInfo :: Connection -> ConnInfo -> CM (Connection, Bool)
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage activeConn connInfo
      conn' <- updatePeerChatVRange activeConn chatVRange
      case chatMsgEvent of
        XInfo p -> do
          let contactUsed = connDirect activeConn
          ct <- withStore $ \db -> createDirectContact db user conn' p contactUsed
          toView $ CRContactConnecting user ct
          pure (conn', False)
        XGrpLinkInv glInv -> do
          (gInfo, host) <- withStore $ \db -> createGroupInvitedViaLink db vr user conn' glInv
          toView $ CRGroupLinkConnecting user gInfo host
          pure (conn', True)
        -- TODO show/log error, other events in SMP confirmation
        _ -> pure (conn', False)

    xGrpMemNew :: GroupInfo -> GroupMember -> MemberInfo -> RcvMessage -> UTCTime -> CM ()
    xGrpMemNew gInfo m memInfo@(MemberInfo memId memRole _ _) msg brokerTs = do
      checkHostRole m memRole
      unless (sameMemberId memId $ membership gInfo) $
        withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
          Right unknownMember@GroupMember {memberStatus = GSMemUnknown} -> do
            updatedMember <- withStore $ \db -> updateUnknownMemberAnnounced db vr user m unknownMember memInfo
            toView $ CRUnknownMemberAnnounced user gInfo m unknownMember updatedMember
            memberAnnouncedToView updatedMember
          Right _ -> messageError "x.grp.mem.new error: member already exists"
          Left _ -> do
            newMember <- withStore $ \db -> createNewGroupMember db user gInfo m memInfo GCPostMember GSMemAnnounced
            memberAnnouncedToView newMember
      where
        memberAnnouncedToView announcedMember@GroupMember {groupMemberId, memberProfile} = do
          let event = RGEMemberAdded groupMemberId (fromLocalProfile memberProfile)
          ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent event)
          groupMsgToView gInfo ci
          toView $ CRJoinedGroupMemberConnecting user gInfo m announcedMember

    xGrpMemIntro :: GroupInfo -> GroupMember -> MemberInfo -> Maybe MemberRestrictions -> CM ()
    xGrpMemIntro gInfo@GroupInfo {chatSettings} m@GroupMember {memberRole, localDisplayName = c} memInfo@(MemberInfo memId _ memChatVRange _) memRestrictions = do
      case memberCategory m of
        GCHostMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Right _ -> messageError "x.grp.mem.intro ignored: member already exists"
            Left _ -> do
              when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole c)
              subMode <- chatReadVar subscriptionMode
              -- [async agent commands] commands should be asynchronous, continuation is to send XGrpMemInv - have to remember one has completed and process on second
              groupConnIds <- createConn subMode
              directConnIds <- case memChatVRange of
                Nothing -> Just <$> createConn subMode
                Just (ChatVersionRange mcvr)
                  | maxVersion mcvr >= groupDirectInvVersion -> pure Nothing
                  | otherwise -> Just <$> createConn subMode
              let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
                  chatV = maybe (minVersion vr) (\peerVR -> vr `peerConnChatVersion` fromChatVRange peerVR) memChatVRange
              void $ withStore $ \db -> createIntroReMember db user gInfo m chatV memInfo memRestrictions groupConnIds directConnIds customUserProfileId subMode
        _ -> messageError "x.grp.mem.intro can be only sent by host member"
      where
        createConn subMode = createAgentConnectionAsync user CFCreateConnGrpMemInv (chatHasNtfs chatSettings) SCMInvitation subMode

    sendXGrpMemInv :: Int64 -> Maybe ConnReqInvitation -> XGrpMemIntroCont -> CM ()
    sendXGrpMemInv hostConnId directConnReq XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq} = do
      hostConn <- withStore $ \db -> getConnectionById db vr user hostConnId
      let msg = XGrpMemInv memberId IntroInvitation {groupConnReq, directConnReq}
      void $ sendDirectMemberMessage hostConn msg groupId
      withStore' $ \db -> updateGroupMemberStatusById db userId groupMemberId GSMemIntroInvited

    xGrpMemInv :: GroupInfo -> GroupMember -> MemberId -> IntroInvitation -> CM ()
    xGrpMemInv gInfo m memId introInv = do
      case memberCategory m of
        GCInviteeMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Left _ -> messageError "x.grp.mem.inv error: referenced member does not exist"
            Right reMember -> do
              GroupMemberIntro {introId} <- withStore $ \db -> saveIntroInvitation db reMember m introInv
              sendGroupMemberMessage user gInfo reMember (XGrpMemFwd (memberInfo m) introInv) (Just introId) $
                withStore' $
                  \db -> updateIntroStatus db introId GMIntroInvForwarded
        _ -> messageError "x.grp.mem.inv can be only sent by invitee member"

    xGrpMemFwd :: GroupInfo -> GroupMember -> MemberInfo -> IntroInvitation -> CM ()
    xGrpMemFwd gInfo@GroupInfo {membership, chatSettings} m memInfo@(MemberInfo memId memRole memChatVRange _) introInv@IntroInvitation {groupConnReq, directConnReq} = do
      let GroupMember {memberId = membershipMemId} = membership
      checkHostRole m memRole
      toMember <-
        withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
          -- TODO if the missed messages are correctly sent as soon as there is connection before anything else is sent
          -- the situation when member does not exist is an error
          -- member receiving x.grp.mem.fwd should have also received x.grp.mem.new prior to that.
          -- For now, this branch compensates for the lack of delayed message delivery.
          Left _ -> withStore $ \db -> createNewGroupMember db user gInfo m memInfo GCPostMember GSMemAnnounced
          Right m' -> pure m'
      withStore' $ \db -> saveMemberInvitation db toMember introInv
      subMode <- chatReadVar subscriptionMode
      -- [incognito] send membership incognito profile, create direct connection as incognito
      let membershipProfile = redactedMemberProfile $ fromLocalProfile $ memberProfile membership
      dm <- encodeConnInfo $ XGrpMemInfo membershipMemId membershipProfile
      -- [async agent commands] no continuation needed, but commands should be asynchronous for stability
      groupConnIds <- joinAgentConnectionAsync user (chatHasNtfs chatSettings) groupConnReq dm subMode
      directConnIds <- forM directConnReq $ \dcr -> joinAgentConnectionAsync user True dcr dm subMode
      let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
          mcvr = maybe chatInitialVRange fromChatVRange memChatVRange
          chatV = vr `peerConnChatVersion` mcvr
      withStore' $ \db -> createIntroToMemberContact db user m toMember chatV mcvr groupConnIds directConnIds customUserProfileId subMode

    xGrpMemRole :: GroupInfo -> GroupMember -> MemberId -> GroupMemberRole -> RcvMessage -> UTCTime -> CM ()
    xGrpMemRole gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId memRole msg brokerTs
      | membershipMemId == memId =
          let gInfo' = gInfo {membership = membership {memberRole = memRole}}
           in changeMemberRole gInfo' membership $ RGEUserRole memRole
      | otherwise =
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Right member -> changeMemberRole gInfo member $ RGEMemberRole (groupMemberId' member) (fromLocalProfile $ memberProfile member) memRole
            Left _ -> messageError "x.grp.mem.role with unknown member ID"
      where
        GroupMember {memberId = membershipMemId} = membership
        changeMemberRole gInfo' member@GroupMember {memberRole = fromRole} gEvent
          | senderRole < GRAdmin || senderRole < fromRole = messageError "x.grp.mem.role with insufficient member permissions"
          | otherwise = do
              withStore' $ \db -> updateGroupMemberRole db user member memRole
              ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent gEvent)
              groupMsgToView gInfo ci
              toView CRMemberRole {user, groupInfo = gInfo', byMember = m, member = member {memberRole = memRole}, fromRole, toRole = memRole}

    checkHostRole :: GroupMember -> GroupMemberRole -> CM ()
    checkHostRole GroupMember {memberRole, localDisplayName} memRole =
      when (memberRole < GRAdmin || memberRole < memRole) $ throwChatError (CEGroupContactRole localDisplayName)

    xGrpMemRestrict :: GroupInfo -> GroupMember -> MemberId -> MemberRestrictions -> RcvMessage -> UTCTime -> CM ()
    xGrpMemRestrict
      gInfo@GroupInfo {groupId, membership = GroupMember {memberId = membershipMemId}}
      m@GroupMember {memberRole = senderRole}
      memId
      MemberRestrictions {restriction}
      msg
      brokerTs
        | membershipMemId == memId =
            -- member shouldn't receive this message about themselves
            messageError "x.grp.mem.restrict: admin blocks you"
        | otherwise =
            withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
              Right bm@GroupMember {groupMemberId = bmId, memberRole, memberProfile = bmp}
                | senderRole < GRModerator || senderRole < memberRole -> messageError "x.grp.mem.restrict with insufficient member permissions"
                | otherwise -> do
                    bm' <- setMemberBlocked bmId
                    toggleNtf user bm' (not blocked)
                    let ciContent = CIRcvGroupEvent $ RGEMemberBlocked bmId (fromLocalProfile bmp) blocked
                    ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs ciContent
                    groupMsgToView gInfo ci
                    toView CRMemberBlockedForAll {user, groupInfo = gInfo, byMember = m, member = bm, blocked}
              Left (SEGroupMemberNotFoundByMemberId _) -> do
                bm <- createUnknownMember gInfo memId
                bm' <- setMemberBlocked $ groupMemberId' bm
                toView $ CRUnknownMemberBlocked user gInfo m bm'
              Left e -> throwError $ ChatErrorStore e
        where
          setMemberBlocked bmId =
            withStore $ \db -> do
              liftIO $ updateGroupMemberBlocked db user groupId bmId restriction
              getGroupMember db vr user groupId bmId
          blocked = mrsBlocked restriction

    xGrpMemCon :: GroupInfo -> GroupMember -> MemberId -> CM ()
    xGrpMemCon gInfo sendingMember memId = do
      refMember <- withStore $ \db -> getGroupMemberByMemberId db vr user gInfo memId
      case (memberCategory sendingMember, memberCategory refMember) of
        (GCInviteeMember, GCInviteeMember) ->
          withStore' (\db -> runExceptT $ getIntroduction db refMember sendingMember) >>= \case
            Right intro -> inviteeXGrpMemCon intro
            Left _ ->
              withStore' (\db -> runExceptT $ getIntroduction db sendingMember refMember) >>= \case
                Right intro -> forwardMemberXGrpMemCon intro
                Left _ -> messageWarning "x.grp.mem.con: no introduction"
        (GCInviteeMember, _) ->
          withStore' (\db -> runExceptT $ getIntroduction db refMember sendingMember) >>= \case
            Right intro -> inviteeXGrpMemCon intro
            Left _ -> messageWarning "x.grp.mem.con: no introduction"
        (_, GCInviteeMember) ->
          withStore' (\db -> runExceptT $ getIntroduction db sendingMember refMember) >>= \case
            Right intro -> forwardMemberXGrpMemCon intro
            Left _ -> messageWarning "x.grp.mem.con: no introductiosupportn"
        -- Note: we can allow XGrpMemCon to all member categories if we decide to support broader group forwarding,
        -- deduplication (see saveGroupRcvMsg, saveGroupFwdRcvMsg) already supports sending XGrpMemCon
        -- to any forwarding member, not only host/inviting member;
        -- database would track all members connections then
        -- (currently it's done via group_member_intros for introduced connections only)
        _ ->
          messageWarning "x.grp.mem.con: neither member is invitee"
      where
        inviteeXGrpMemCon :: GroupMemberIntro -> CM ()
        inviteeXGrpMemCon GroupMemberIntro {introId, introStatus} = case introStatus of
          GMIntroReConnected -> updateStatus introId GMIntroConnected
          GMIntroToConnected -> pure ()
          GMIntroConnected -> pure ()
          _ -> updateStatus introId GMIntroToConnected
        forwardMemberXGrpMemCon :: GroupMemberIntro -> CM ()
        forwardMemberXGrpMemCon GroupMemberIntro {introId, introStatus} = case introStatus of
          GMIntroToConnected -> updateStatus introId GMIntroConnected
          GMIntroReConnected -> pure ()
          GMIntroConnected -> pure ()
          _ -> updateStatus introId GMIntroReConnected
        updateStatus introId status = withStore' $ \db -> updateIntroStatus db introId status

    xGrpMemDel :: GroupInfo -> GroupMember -> MemberId -> RcvMessage -> UTCTime -> CM ()
    xGrpMemDel gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId msg brokerTs = do
      let GroupMember {memberId = membershipMemId} = membership
      if membershipMemId == memId
        then checkRole membership $ do
          deleteGroupLinkIfExists user gInfo
          -- member records are not deleted to keep history
          members <- withStore' $ \db -> getGroupMembers db vr user gInfo
          deleteMembersConnections user members
          withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemRemoved
          deleteMemberItem RGEUserDeleted
          toView $ CRDeletedMemberUser user gInfo {membership = membership {memberStatus = GSMemRemoved}} m
        else
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Left _ -> messageError "x.grp.mem.del with unknown member ID"
            Right member@GroupMember {groupMemberId, memberProfile} ->
              checkRole member $ do
                -- ? prohibit deleting member if it's the sender - sender should use x.grp.leave
                deleteMemberConnection user member
                -- undeleted "member connected" chat item will prevent deletion of member record
                deleteOrUpdateMemberRecord user member
                deleteMemberItem $ RGEMemberDeleted groupMemberId (fromLocalProfile memberProfile)
                toView $ CRDeletedMember user gInfo m member {memberStatus = GSMemRemoved}
      where
        checkRole GroupMember {memberRole} a
          | senderRole < GRAdmin || senderRole < memberRole =
              messageError "x.grp.mem.del with insufficient member permissions"
          | otherwise = a
        deleteMemberItem gEvent = do
          ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent gEvent)
          groupMsgToView gInfo ci

    xGrpLeave :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM ()
    xGrpLeave gInfo m msg brokerTs = do
      deleteMemberConnection user m
      -- member record is not deleted to allow creation of "member left" chat item
      withStore' $ \db -> updateGroupMemberStatus db userId m GSMemLeft
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent RGEMemberLeft)
      groupMsgToView gInfo ci
      toView $ CRLeftMember user gInfo m {memberStatus = GSMemLeft}

    xGrpDel :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM ()
    xGrpDel gInfo@GroupInfo {membership} m@GroupMember {memberRole} msg brokerTs = do
      when (memberRole /= GROwner) $ throwChatError $ CEGroupUserRole gInfo GROwner
      ms <- withStore' $ \db -> do
        members <- getGroupMembers db vr user gInfo
        updateGroupMemberStatus db userId membership GSMemGroupDeleted
        pure members
      -- member records are not deleted to keep history
      deleteMembersConnections user ms
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent RGEGroupDeleted)
      groupMsgToView gInfo ci
      toView $ CRGroupDeleted user gInfo {membership = membership {memberStatus = GSMemGroupDeleted}} m

    xGrpInfo :: GroupInfo -> GroupMember -> GroupProfile -> RcvMessage -> UTCTime -> CM ()
    xGrpInfo g@GroupInfo {groupProfile = p, businessChat} m@GroupMember {memberRole} p' msg brokerTs
      | memberRole < GROwner = messageError "x.grp.info with insufficient member permissions"
      | otherwise = case businessChat of
          Nothing -> unless (p == p') $ do
            g' <- withStore $ \db -> updateGroupProfile db user g p'
            toView $ CRGroupUpdated user g g' (Just m)
            let cd = CDGroupRcv g' m
            unless (sameGroupProfileInfo p p') $ do
              ci <- saveRcvChatItem user cd msg brokerTs (CIRcvGroupEvent $ RGEGroupUpdated p')
              groupMsgToView g' ci
            createGroupFeatureChangedItems user cd CIRcvGroupFeature g g'
          Just _ -> updateGroupPrefs_ g m $ fromMaybe defaultBusinessGroupPrefs $ groupPreferences p'

    xGrpPrefs :: GroupInfo -> GroupMember -> GroupPreferences -> CM ()
    xGrpPrefs g m@GroupMember {memberRole} ps'
      | memberRole < GROwner = messageError "x.grp.prefs with insufficient member permissions"
      | otherwise = updateGroupPrefs_ g m ps'

    updateGroupPrefs_ :: GroupInfo -> GroupMember -> GroupPreferences -> CM ()
    updateGroupPrefs_ g@GroupInfo {groupProfile = p} m ps' =
      unless (groupPreferences p == Just ps') $ do
        g' <- withStore' $ \db -> updateGroupPreferences db user g ps'
        toView $ CRGroupUpdated user g g' (Just m)
        let cd = CDGroupRcv g' m
        createGroupFeatureChangedItems user cd CIRcvGroupFeature g g'

    xGrpDirectInv :: GroupInfo -> GroupMember -> Connection -> ConnReqInvitation -> Maybe MsgContent -> RcvMessage -> UTCTime -> CM ()
    xGrpDirectInv g m mConn connReq mContent_ msg brokerTs = do
      unless (groupFeatureMemberAllowed SGFDirectMessages m g) $ messageError "x.grp.direct.inv: direct messages not allowed"
      let GroupMember {memberContactId} = m
      subMode <- chatReadVar subscriptionMode
      case memberContactId of
        Nothing -> createNewContact subMode
        Just mContactId -> do
          mCt <- withStore $ \db -> getContact db vr user mContactId
          let Contact {activeConn, contactGrpInvSent} = mCt
          forM_ activeConn $ \Connection {connId} ->
            if contactGrpInvSent
              then do
                ownConnReq <- withStore $ \db -> getConnReqInv db connId
                -- in case both members sent x.grp.direct.inv before receiving other's for processing,
                -- only the one who received greater connReq joins, the other creates items and waits for confirmation
                if strEncode connReq > strEncode ownConnReq
                  then joinExistingContact subMode mCt
                  else createItems mCt m
              else joinExistingContact subMode mCt
      where
        joinExistingContact subMode mCt = do
          connIds <- joinConn subMode
          mCt' <- withStore $ \db -> updateMemberContactInvited db user connIds g mConn mCt subMode
          createItems mCt' m
          securityCodeChanged mCt'
        createNewContact subMode = do
          connIds <- joinConn subMode
          -- [incognito] reuse membership incognito profile
          (mCt', m') <- withStore' $ \db -> createMemberContactInvited db user connIds g m mConn subMode
          createItems mCt' m'
        joinConn subMode = do
          -- [incognito] send membership incognito profile
          let p = userProfileToSend user (fromLocalProfile <$> incognitoMembershipProfile g) Nothing False
          -- TODO PQ should negotitate contact connection with PQSupportOn? (use encodeConnInfoPQ)
          dm <- encodeConnInfo $ XInfo p
          joinAgentConnectionAsync user True connReq dm subMode
        createItems mCt' m' = do
          createInternalChatItem user (CDGroupRcv g m') (CIRcvGroupEvent RGEMemberCreatedContact) Nothing
          toView $ CRNewMemberContactReceivedInv user mCt' g m'
          forM_ mContent_ $ \mc -> do
            ci <- saveRcvChatItem user (CDDirectRcv mCt') msg brokerTs (CIRcvMsgContent mc)
            toView $ CRNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat mCt') ci]

    securityCodeChanged :: Contact -> CM ()
    securityCodeChanged ct = do
      toView $ CRContactVerificationReset user ct
      createInternalChatItem user (CDDirectRcv ct) (CIRcvConnEvent RCEVerificationCodeReset) Nothing

    xGrpMsgForward :: GroupInfo -> GroupMember -> MemberId -> ChatMessage 'Json -> UTCTime -> CM ()
    xGrpMsgForward gInfo@GroupInfo {groupId} m@GroupMember {memberRole, localDisplayName} memberId msg msgTs = do
      when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole localDisplayName)
      withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memberId) >>= \case
        Right author -> processForwardedMsg author msg
        Left (SEGroupMemberNotFoundByMemberId _) -> do
          unknownAuthor <- createUnknownMember gInfo memberId
          toView $ CRUnknownMemberCreated user gInfo m unknownAuthor
          processForwardedMsg unknownAuthor msg
        Left e -> throwError $ ChatErrorStore e
      where
        -- Note: forwarded group events (see forwardedGroupMsg) should include msgId to be deduplicated
        processForwardedMsg :: GroupMember -> ChatMessage 'Json -> CM ()
        processForwardedMsg author chatMsg = do
          let body = LB.toStrict $ J.encode msg
          rcvMsg@RcvMessage {chatMsgEvent = ACME _ event} <- saveGroupFwdRcvMsg user groupId m author body chatMsg
          case event of
            XMsgNew mc -> memberCanSend author $ newGroupContentMessage gInfo author mc rcvMsg msgTs True
            XMsgFileDescr sharedMsgId fileDescr -> memberCanSend author $ groupMessageFileDescription gInfo author sharedMsgId fileDescr
            XMsgUpdate sharedMsgId mContent ttl live -> memberCanSend author $ groupMessageUpdate gInfo author sharedMsgId mContent rcvMsg msgTs ttl live
            XMsgDel sharedMsgId memId -> groupMessageDelete gInfo author sharedMsgId memId rcvMsg msgTs
            XMsgReact sharedMsgId (Just memId) reaction add -> groupMsgReaction gInfo author sharedMsgId memId reaction add rcvMsg msgTs
            XFileCancel sharedMsgId -> xFileCancelGroup gInfo author sharedMsgId
            XInfo p -> xInfoMember gInfo author p msgTs
            XGrpMemNew memInfo -> xGrpMemNew gInfo author memInfo rcvMsg msgTs
            XGrpMemRole memId memRole -> xGrpMemRole gInfo author memId memRole rcvMsg msgTs
            XGrpMemDel memId -> xGrpMemDel gInfo author memId rcvMsg msgTs
            XGrpLeave -> xGrpLeave gInfo author rcvMsg msgTs
            XGrpDel -> xGrpDel gInfo author rcvMsg msgTs
            XGrpInfo p' -> xGrpInfo gInfo author p' rcvMsg msgTs
            XGrpPrefs ps' -> xGrpPrefs gInfo author ps'
            _ -> messageError $ "x.grp.msg.forward: unsupported forwarded event " <> T.pack (show $ toCMEventTag event)

    createUnknownMember :: GroupInfo -> MemberId -> CM GroupMember
    createUnknownMember gInfo memberId = do
      let name = T.take 7 . safeDecodeUtf8 . B64.encode . unMemberId $ memberId
      withStore $ \db -> createNewUnknownGroupMember db vr user gInfo memberId name

    directMsgReceived :: Contact -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> CM ()
    directMsgReceived ct conn@Connection {connId} msgMeta msgRcpts = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta `catchChatError` \_ -> pure ()
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore' $ \db -> updateSndMsgDeliveryStatus db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateDirectItemStatus ct conn agentMsgId $ CISSndRcvd msgRcptStatus SSPComplete

    groupMsgReceived :: GroupInfo -> GroupMember -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> CM ()
    groupMsgReceived gInfo m conn@Connection {connId} msgMeta msgRcpts = do
      checkIntegrityCreateItem (CDGroupRcv gInfo m) msgMeta `catchChatError` \_ -> pure ()
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore' $ \db -> updateSndMsgDeliveryStatus db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateGroupItemsStatus gInfo m conn agentMsgId (GSSRcvd msgRcptStatus) Nothing

    -- Searches chat items for many agent message IDs and updates their status
    updateDirectItemsStatusMsgs :: Contact -> Connection -> [AgentMsgId] -> CIStatus 'MDSnd -> CM ()
    updateDirectItemsStatusMsgs ct conn msgIds newStatus = do
      cis <- withStore' $ \db -> forM msgIds $ \msgId -> runExceptT $ updateDirectItemsStatus' db ct conn msgId newStatus
      let acis = map ctItem $ concat $ rights cis
      unless (null acis) $ toView $ CRChatItemsStatusesUpdated user acis
      where
        ctItem = AChatItem SCTDirect SMDSnd (DirectChat ct)

    updateDirectItemStatus :: Contact -> Connection -> AgentMsgId -> CIStatus 'MDSnd -> CM ()
    updateDirectItemStatus ct conn msgId newStatus = do
      cis <- withStore $ \db -> updateDirectItemsStatus' db ct conn msgId newStatus
      let acis = map ctItem cis
      unless (null acis) $ toView $ CRChatItemsStatusesUpdated user acis
      where
        ctItem = AChatItem SCTDirect SMDSnd (DirectChat ct)

    updateDirectItemsStatus' :: DB.Connection -> Contact -> Connection -> AgentMsgId -> CIStatus 'MDSnd -> ExceptT StoreError IO [ChatItem 'CTDirect 'MDSnd]
    updateDirectItemsStatus' db ct@Contact {contactId} Connection {connId} msgId newStatus = do
      items <- liftIO $ getDirectChatItemsByAgentMsgId db user contactId connId msgId
      catMaybes <$> mapM updateItem items
      where
        updateItem :: CChatItem 'CTDirect -> ExceptT StoreError IO (Maybe (ChatItem 'CTDirect 'MDSnd))
        updateItem = \case
          (CChatItem SMDSnd ChatItem {meta = CIMeta {itemStatus = CISSndRcvd _ _}}) -> pure Nothing
          (CChatItem SMDSnd ChatItem {meta = CIMeta {itemId, itemStatus}})
            | itemStatus == newStatus -> pure Nothing
            | otherwise -> Just <$> updateDirectChatItemStatus db user ct itemId newStatus
          _ -> pure Nothing

    updateGroupMemSndStatus' :: DB.Connection -> ChatItemId -> GroupMemberId -> GroupSndStatus -> IO Bool
    updateGroupMemSndStatus' db itemId groupMemberId newStatus =
      runExceptT (getGroupSndStatus db itemId groupMemberId) >>= \case
        Right (GSSRcvd _) -> pure False
        Right memStatus
          | memStatus == newStatus -> pure False
          | otherwise -> updateGroupSndStatus db itemId groupMemberId newStatus $> True
        _ -> pure False

    updateGroupItemsStatus :: GroupInfo -> GroupMember -> Connection -> AgentMsgId -> GroupSndStatus -> Maybe Bool -> CM ()
    updateGroupItemsStatus gInfo@GroupInfo {groupId} GroupMember {groupMemberId} Connection {connId} msgId newMemStatus viaProxy_ = do
      items <- withStore' (\db -> getGroupChatItemsByAgentMsgId db user groupId connId msgId)
      cis <- catMaybes <$> withStore (\db -> mapM (updateItem db) items)
      let acis = map gItem cis
      unless (null acis) $ toView $ CRChatItemsStatusesUpdated user acis
      where
        gItem = AChatItem SCTGroup SMDSnd (GroupChat gInfo)
        updateItem :: DB.Connection -> CChatItem 'CTGroup -> ExceptT StoreError IO (Maybe (ChatItem 'CTGroup 'MDSnd))
        updateItem db = \case
          (CChatItem SMDSnd ChatItem {meta = CIMeta {itemStatus = CISSndRcvd _ SSPComplete}}) -> pure Nothing
          (CChatItem SMDSnd ChatItem {meta = CIMeta {itemId, itemStatus}}) -> do
            forM_ viaProxy_ $ \viaProxy -> liftIO $ setGroupSndViaProxy db itemId groupMemberId viaProxy
            memStatusChanged <- liftIO $ updateGroupMemSndStatus' db itemId groupMemberId newMemStatus
            if memStatusChanged
              then do
                memStatusCounts <- liftIO $ getGroupSndStatusCounts db itemId
                let newStatus = membersGroupItemStatus memStatusCounts
                if newStatus /= itemStatus
                  then Just <$> updateGroupChatItemStatus db user gInfo itemId newStatus
                  else pure Nothing
              else pure Nothing
          _ -> pure Nothing

createContactPQSndItem :: User -> Contact -> Connection -> PQEncryption -> CM (Contact, Connection)
createContactPQSndItem user ct conn@Connection {pqSndEnabled} pqSndEnabled' =
  flip catchChatError (const $ pure (ct, conn)) $ case (pqSndEnabled, pqSndEnabled') of
    (Just b, b') | b' /= b -> createPQItem $ CISndConnEvent (SCEPqEnabled pqSndEnabled')
    (Nothing, PQEncOn) -> createPQItem $ CISndDirectE2EEInfo (E2EInfo pqSndEnabled')
    _ -> pure (ct, conn)
  where
    createPQItem ciContent = do
      let conn' = conn {pqSndEnabled = Just pqSndEnabled'} :: Connection
          ct' = ct {activeConn = Just conn'} :: Contact
      when (contactPQEnabled ct /= contactPQEnabled ct') $ do
        createInternalChatItem user (CDDirectSnd ct') ciContent Nothing
        toView $ CRContactPQEnabled user ct' pqSndEnabled'
      pure (ct', conn')

updateContactPQRcv :: User -> Contact -> Connection -> PQEncryption -> CM (Contact, Connection)
updateContactPQRcv user ct conn@Connection {connId, pqRcvEnabled} pqRcvEnabled' =
  flip catchChatError (const $ pure (ct, conn)) $ case (pqRcvEnabled, pqRcvEnabled') of
    (Just b, b') | b' /= b -> updatePQ $ CIRcvConnEvent (RCEPqEnabled pqRcvEnabled')
    (Nothing, PQEncOn) -> updatePQ $ CIRcvDirectE2EEInfo (E2EInfo pqRcvEnabled')
    _ -> pure (ct, conn)
  where
    updatePQ ciContent = do
      withStore' $ \db -> updateConnPQRcvEnabled db connId pqRcvEnabled'
      let conn' = conn {pqRcvEnabled = Just pqRcvEnabled'} :: Connection
          ct' = ct {activeConn = Just conn'} :: Contact
      when (contactPQEnabled ct /= contactPQEnabled ct') $ do
        createInternalChatItem user (CDDirectRcv ct') ciContent Nothing
        toView $ CRContactPQEnabled user ct' pqRcvEnabled'
      pure (ct', conn')

metaBrokerTs :: MsgMeta -> UTCTime
metaBrokerTs MsgMeta {broker = (_, brokerTs)} = brokerTs

sameMemberId :: MemberId -> GroupMember -> Bool
sameMemberId memId GroupMember {memberId} = memId == memberId

updatePeerChatVRange :: Connection -> VersionRangeChat -> CM Connection
updatePeerChatVRange conn@Connection {connId, connChatVersion = v, peerChatVRange, connType, pqSupport, pqEncryption} msgVRange = do
  v' <- lift $ upgradedConnVersion v msgVRange
  conn' <-
    if msgVRange /= peerChatVRange || v' /= v
      then do
        withStore' $ \db -> setPeerChatVRange db connId v' msgVRange
        pure conn {connChatVersion = v', peerChatVRange = msgVRange}
      else pure conn
  -- TODO v6.0 remove/review: for contacts only version upgrade should trigger enabling PQ support/encryption
  if connType == ConnContact && v' >= pqEncryptionCompressionVersion && (pqSupport /= PQSupportOn || pqEncryption /= PQEncOn)
    then do
      withStore' $ \db -> updateConnSupportPQ db connId PQSupportOn PQEncOn
      pure conn' {pqSupport = PQSupportOn, pqEncryption = PQEncOn}
    else pure conn'

updateMemberChatVRange :: GroupMember -> Connection -> VersionRangeChat -> CM (GroupMember, Connection)
updateMemberChatVRange mem@GroupMember {groupMemberId} conn@Connection {connId, connChatVersion = v, peerChatVRange} msgVRange = do
  v' <- lift $ upgradedConnVersion v msgVRange
  if msgVRange /= peerChatVRange || v' /= v
    then do
      withStore' $ \db -> do
        setPeerChatVRange db connId v' msgVRange
        setMemberChatVRange db groupMemberId msgVRange
      let conn' = conn {connChatVersion = v', peerChatVRange = msgVRange}
      pure (mem {memberChatVRange = msgVRange, activeConn = Just conn'}, conn')
    else pure (mem, conn)

upgradedConnVersion :: VersionChat -> VersionRangeChat -> CM' VersionChat
upgradedConnVersion v peerVR = do
  vr <- chatVersionRange'
  -- don't allow reducing agreed connection version
  pure $ maybe v (\(Compatible v') -> max v v') $ vr `compatibleVersion` peerVR

parseFileDescription :: FilePartyI p => Text -> CM (ValidFileDescription p)
parseFileDescription =
  liftEither . first (ChatError . CEInvalidFileDescription) . (strDecode . encodeUtf8)

sendDirectFileInline :: User -> Contact -> FileTransferMeta -> SharedMsgId -> CM ()
sendDirectFileInline user ct ft sharedMsgId = do
  msgDeliveryId <- sendFileInline_ ft sharedMsgId $ sendDirectContactMessage user ct
  withStore $ \db -> updateSndDirectFTDelivery db ct ft msgDeliveryId

sendMemberFileInline :: GroupMember -> Connection -> FileTransferMeta -> SharedMsgId -> CM ()
sendMemberFileInline m@GroupMember {groupId} conn ft sharedMsgId = do
  msgDeliveryId <- sendFileInline_ ft sharedMsgId $ \msg -> do
    (sndMsg, msgDeliveryId, _) <- sendDirectMemberMessage conn msg groupId
    pure (sndMsg, msgDeliveryId)
  withStore' $ \db -> updateSndGroupFTDelivery db m conn ft msgDeliveryId

sendFileInline_ :: FileTransferMeta -> SharedMsgId -> (ChatMsgEvent 'Binary -> CM (SndMessage, Int64)) -> CM Int64
sendFileInline_ FileTransferMeta {filePath, chunkSize} sharedMsgId sendMsg =
  sendChunks 1 =<< liftIO . B.readFile =<< lift (toFSFilePath filePath)
  where
    sendChunks chunkNo bytes = do
      let (chunk, rest) = B.splitAt chSize bytes
      (_, msgDeliveryId) <- sendMsg $ BFileChunk sharedMsgId $ FileChunk chunkNo chunk
      if B.null rest
        then pure msgDeliveryId
        else sendChunks (chunkNo + 1) rest
    chSize = fromIntegral chunkSize

parseChatMessage :: Connection -> ByteString -> CM (ChatMessage 'Json)
parseChatMessage conn s = do
  case parseChatMessages s of
    [msg] -> liftEither . first (ChatError . errType) $ (\(ACMsg _ m) -> checkEncoding m) =<< msg
    _ -> throwChatError $ CEException "parseChatMessage: single message is expected"
  where
    errType = CEInvalidChatMessage conn Nothing (safeDecodeUtf8 s)
{-# INLINE parseChatMessage #-}

sendFileChunk :: User -> SndFileTransfer -> CM ()
sendFileChunk user ft@SndFileTransfer {fileId, fileStatus, agentConnId = AgentConnId acId} =
  unless (fileStatus == FSComplete || fileStatus == FSCancelled) $ do
    vr <- chatVersionRange
    withStore' (`createSndFileChunk` ft) >>= \case
      Just chunkNo -> sendFileChunkNo ft chunkNo
      Nothing -> do
        ci <- withStore $ \db -> do
          liftIO $ updateSndFileStatus db ft FSComplete
          liftIO $ deleteSndFileChunks db ft
          updateDirectCIFileStatus db vr user fileId CIFSSndComplete
        toView $ CRSndFileComplete user ci ft
        lift $ closeFileHandle fileId sndFiles
        deleteAgentConnectionAsync user acId

sendFileChunkNo :: SndFileTransfer -> Integer -> CM ()
sendFileChunkNo ft@SndFileTransfer {agentConnId = AgentConnId acId} chunkNo = do
  chunkBytes <- readFileChunk ft chunkNo
  (msgId, _) <- withAgent $ \a -> sendMessage a acId PQEncOff SMP.noMsgFlags $ smpEncode FileChunk {chunkNo, chunkBytes}
  withStore' $ \db -> updateSndFileChunkMsg db ft chunkNo msgId

readFileChunk :: SndFileTransfer -> Integer -> CM ByteString
readFileChunk SndFileTransfer {fileId, filePath, chunkSize} chunkNo = do
  fsFilePath <- lift $ toFSFilePath filePath
  read_ fsFilePath `catchThrow` (ChatError . CEFileRead filePath . show)
  where
    read_ fsFilePath = do
      h <- getFileHandle fileId fsFilePath sndFiles ReadMode
      pos <- hTell h
      let pos' = (chunkNo - 1) * chunkSize
      when (pos /= pos') $ hSeek h AbsoluteSeek pos'
      liftIO . B.hGet h $ fromInteger chunkSize

parseFileChunk :: ByteString -> CM FileChunk
parseFileChunk = liftEither . first (ChatError . CEFileRcvChunk) . smpDecode

appendFileChunk :: RcvFileTransfer -> Integer -> ByteString -> Bool -> CM ()
appendFileChunk ft@RcvFileTransfer {fileId, fileStatus, cryptoArgs, fileInvitation = FileInvitation {fileName}} chunkNo chunk final =
  case fileStatus of
    RFSConnected RcvFileInfo {filePath} -> append_ filePath
    -- sometimes update of file transfer status to FSConnected
    -- doesn't complete in time before MSG with first file chunk
    RFSAccepted RcvFileInfo {filePath} -> append_ filePath
    RFSCancelled _ -> pure ()
    _ -> throwChatError $ CEFileInternal "receiving file transfer not in progress"
  where
    append_ :: FilePath -> CM ()
    append_ filePath = do
      fsFilePath <- lift $ toFSFilePath filePath
      h <- getFileHandle fileId fsFilePath rcvFiles AppendMode
      liftIO (B.hPut h chunk >> hFlush h) `catchThrow` (fileErr . show)
      withStore' $ \db -> updatedRcvFileChunkStored db ft chunkNo
      when final $ do
        lift $ closeFileHandle fileId rcvFiles
        forM_ cryptoArgs $ \cfArgs -> do
          tmpFile <- lift getChatTempDirectory >>= liftIO . (`uniqueCombine` fileName)
          tryChatError (liftError encryptErr $ encryptFile fsFilePath tmpFile cfArgs) >>= \case
            Right () -> do
              removeFile fsFilePath `catchChatError` \_ -> pure ()
              renameFile tmpFile fsFilePath
            Left e -> do
              toView $ CRChatError Nothing e
              removeFile tmpFile `catchChatError` \_ -> pure ()
              withStore' (`removeFileCryptoArgs` fileId)
      where
        encryptErr e = fileErr $ e <> ", received file not encrypted"
        fileErr = ChatError . CEFileWrite filePath

getFileHandle :: Int64 -> FilePath -> (ChatController -> TVar (Map Int64 Handle)) -> IOMode -> CM Handle
getFileHandle fileId filePath files ioMode = do
  fs <- asks files
  h_ <- M.lookup fileId <$> readTVarIO fs
  maybe (newHandle fs) pure h_
  where
    newHandle fs = do
      h <- openFile filePath ioMode `catchThrow` (ChatError . CEFileInternal . show)
      atomically . modifyTVar fs $ M.insert fileId h
      pure h

isFileActive :: Int64 -> (ChatController -> TVar (Map Int64 Handle)) -> CM Bool
isFileActive fileId files = do
  fs <- asks files
  isJust . M.lookup fileId <$> readTVarIO fs

cancelRcvFileTransfer :: User -> RcvFileTransfer -> CM (Maybe ConnId)
cancelRcvFileTransfer user ft@RcvFileTransfer {fileId, xftpRcvFile, rcvFileInline} =
  cancel' `catchChatError` (\e -> toView (CRChatError (Just user) e) $> fileConnId)
  where
    cancel' = do
      lift $ closeFileHandle fileId rcvFiles
      withStore' $ \db -> do
        updateFileCancelled db user fileId CIFSRcvCancelled
        updateRcvFileStatus db fileId FSCancelled
        deleteRcvFileChunks db ft
      case xftpRcvFile of
        Just XFTPRcvFile {agentRcvFileId = Just (AgentRcvFileId aFileId), agentRcvFileDeleted} ->
          unless agentRcvFileDeleted $ agentXFTPDeleteRcvFile aFileId fileId
        _ -> pure ()
      pure fileConnId
    fileConnId = if isNothing xftpRcvFile && isNothing rcvFileInline then liveRcvFileTransferConnId ft else Nothing

cancelSndFile :: User -> FileTransferMeta -> [SndFileTransfer] -> Bool -> CM [ConnId]
cancelSndFile user FileTransferMeta {fileId, xftpSndFile} fts sendCancel = do
  withStore' (\db -> updateFileCancelled db user fileId CIFSSndCancelled)
    `catchChatError` (toView . CRChatError (Just user))
  case xftpSndFile of
    Nothing ->
      catMaybes <$> forM fts (\ft -> cancelSndFileTransfer user ft sendCancel)
    Just xsf -> do
      forM_ fts (\ft -> cancelSndFileTransfer user ft False)
      lift (agentXFTPDeleteSndFileRemote user xsf fileId) `catchChatError` (toView . CRChatError (Just user))
      pure []

-- TODO v6.0 remove
cancelSndFileTransfer :: User -> SndFileTransfer -> Bool -> CM (Maybe ConnId)
cancelSndFileTransfer user@User {userId} ft@SndFileTransfer {fileId, connId, agentConnId = AgentConnId acId, fileStatus, fileInline} sendCancel =
  if fileStatus == FSCancelled || fileStatus == FSComplete
    then pure Nothing
    else cancel' `catchChatError` (\e -> toView (CRChatError (Just user) e) $> fileConnId)
  where
    cancel' = do
      withStore' $ \db -> do
        updateSndFileStatus db ft FSCancelled
        deleteSndFileChunks db ft
      when sendCancel $ case fileInline of
        Just _ -> do
          vr <- chatVersionRange
          (sharedMsgId, conn) <- withStore $ \db -> (,) <$> getSharedMsgIdByFileId db userId fileId <*> getConnectionById db vr user connId
          void $ sendDirectMessage_ conn (BFileChunk sharedMsgId FileChunkCancel) (ConnectionId connId)
        _ -> withAgent $ \a -> void . sendMessage a acId PQEncOff SMP.noMsgFlags $ smpEncode FileChunkCancel
      pure fileConnId
    fileConnId = if isNothing fileInline then Just acId else Nothing

closeFileHandle :: Int64 -> (ChatController -> TVar (Map Int64 Handle)) -> CM' ()
closeFileHandle fileId files = do
  fs <- asks files
  h_ <- atomically . stateTVar fs $ \m -> (M.lookup fileId m, M.delete fileId m)
  liftIO $ mapM_ hClose h_ `catchAll_` pure ()

deleteMembersConnections :: User -> [GroupMember] -> CM ()
deleteMembersConnections user members = deleteMembersConnections' user members False

deleteMembersConnections' :: User -> [GroupMember] -> Bool -> CM ()
deleteMembersConnections' user members waitDelivery = do
  let memberConns =
        filter (\Connection {connStatus} -> connStatus /= ConnDeleted) $
          mapMaybe (\GroupMember {activeConn} -> activeConn) members
  deleteAgentConnectionsAsync' user (map aConnId memberConns) waitDelivery
  lift . void . withStoreBatch' $ \db -> map (\conn -> updateConnectionStatus db conn ConnDeleted) memberConns

deleteMemberConnection :: User -> GroupMember -> CM ()
deleteMemberConnection user mem = deleteMemberConnection' user mem False

deleteMemberConnection' :: User -> GroupMember -> Bool -> CM ()
deleteMemberConnection' user GroupMember {activeConn} waitDelivery = do
  forM_ activeConn $ \conn -> do
    deleteAgentConnectionAsync' user (aConnId conn) waitDelivery
    withStore' $ \db -> updateConnectionStatus db conn ConnDeleted

deleteOrUpdateMemberRecord :: User -> GroupMember -> CM ()
deleteOrUpdateMemberRecord user@User {userId} member =
  withStore' $ \db ->
    checkGroupMemberHasItems db user member >>= \case
      Just _ -> updateGroupMemberStatus db userId member GSMemRemoved
      Nothing -> deleteGroupMember db user member

sendDirectContactMessages :: MsgEncodingI e => User -> Contact -> NonEmpty (ChatMsgEvent e) -> CM [Either ChatError SndMessage]
sendDirectContactMessages user ct events = do
  Connection {connChatVersion = v} <- liftEither $ contactSendConn_ ct
  if v >= batchSend2Version
    then sendDirectContactMessages' user ct events
    else forM (L.toList events) $ \evt ->
      (Right . fst <$> sendDirectContactMessage user ct evt) `catchChatError` \e -> pure (Left e)

sendDirectContactMessages' :: MsgEncodingI e => User -> Contact -> NonEmpty (ChatMsgEvent e) -> CM [Either ChatError SndMessage]
sendDirectContactMessages' user ct events = do
  conn@Connection {connId} <- liftEither $ contactSendConn_ ct
  let idsEvts = L.map (ConnectionId connId,) events
      msgFlags = MsgFlags {notification = any (hasNotification . toCMEventTag) events}
  sndMsgs_ <- lift $ createSndMessages idsEvts
  (sndMsgs', pqEnc_) <- batchSendConnMessagesB user conn msgFlags sndMsgs_
  forM_ pqEnc_ $ \pqEnc' -> void $ createContactPQSndItem user ct conn pqEnc'
  pure sndMsgs'

sendDirectContactMessage :: MsgEncodingI e => User -> Contact -> ChatMsgEvent e -> CM (SndMessage, Int64)
sendDirectContactMessage user ct chatMsgEvent = do
  conn@Connection {connId} <- liftEither $ contactSendConn_ ct
  r <- sendDirectMessage_ conn chatMsgEvent (ConnectionId connId)
  let (sndMessage, msgDeliveryId, pqEnc') = r
  void $ createContactPQSndItem user ct conn pqEnc'
  pure (sndMessage, msgDeliveryId)

contactSendConn_ :: Contact -> Either ChatError Connection
contactSendConn_ ct@Contact {activeConn} = case activeConn of
  Nothing -> err $ CEContactNotReady ct
  Just conn
    | not (connReady conn) -> err $ CEContactNotReady ct
    | not (contactActive ct) -> err $ CEContactNotActive ct
    | connDisabled conn -> err $ CEContactDisabled ct
    | otherwise -> Right conn
  where
    err = Left . ChatError

-- unlike sendGroupMemberMessage, this function will not store message as pending
-- TODO v5.8 we could remove pending messages once all clients support forwarding
sendDirectMemberMessage :: MsgEncodingI e => Connection -> ChatMsgEvent e -> GroupId -> CM (SndMessage, Int64, PQEncryption)
sendDirectMemberMessage conn chatMsgEvent groupId = sendDirectMessage_ conn chatMsgEvent (GroupId groupId)

sendDirectMessage_ :: MsgEncodingI e => Connection -> ChatMsgEvent e -> ConnOrGroupId -> CM (SndMessage, Int64, PQEncryption)
sendDirectMessage_ conn chatMsgEvent connOrGroupId = do
  when (connDisabled conn) $ throwChatError (CEConnectionDisabled conn)
  msg@SndMessage {msgId, msgBody} <- createSndMessage chatMsgEvent connOrGroupId
  -- TODO move compressed body to SndMessage and compress in createSndMessage
  (msgDeliveryId, pqEnc') <- deliverMessage conn (toCMEventTag chatMsgEvent) msgBody msgId
  pure (msg, msgDeliveryId, pqEnc')

createSndMessage :: MsgEncodingI e => ChatMsgEvent e -> ConnOrGroupId -> CM SndMessage
createSndMessage chatMsgEvent connOrGroupId =
  liftEither . runIdentity =<< lift (createSndMessages $ Identity (connOrGroupId, chatMsgEvent))

createSndMessages :: forall e t. (MsgEncodingI e, Traversable t) => t (ConnOrGroupId, ChatMsgEvent e) -> CM' (t (Either ChatError SndMessage))
createSndMessages idsEvents = do
  g <- asks random
  vr <- chatVersionRange'
  withStoreBatch $ \db -> fmap (createMsg db g vr) idsEvents
  where
    createMsg :: DB.Connection -> TVar ChaChaDRG -> VersionRangeChat -> (ConnOrGroupId, ChatMsgEvent e) -> IO (Either ChatError SndMessage)
    createMsg db g vr (connOrGroupId, evnt) = runExceptT $ do
      withExceptT ChatErrorStore $ createNewSndMessage db g connOrGroupId evnt encodeMessage
      where
        encodeMessage sharedMsgId =
          encodeChatMessage maxEncodedMsgLength ChatMessage {chatVRange = vr, msgId = Just sharedMsgId, chatMsgEvent = evnt}

sendGroupMemberMessages :: forall e. MsgEncodingI e => User -> Connection -> NonEmpty (ChatMsgEvent e) -> GroupId -> CM ()
sendGroupMemberMessages user conn events groupId = do
  when (connDisabled conn) $ throwChatError (CEConnectionDisabled conn)
  let idsEvts = L.map (GroupId groupId,) events
  (errs, msgs) <- lift $ partitionEithers . L.toList <$> createSndMessages idsEvts
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  forM_ (L.nonEmpty msgs) $ \msgs' ->
    batchSendConnMessages user conn MsgFlags {notification = True} msgs'

batchSendConnMessages :: User -> Connection -> MsgFlags -> NonEmpty SndMessage -> CM ([Either ChatError SndMessage], Maybe PQEncryption)
batchSendConnMessages user conn msgFlags msgs =
  batchSendConnMessagesB user conn msgFlags $ L.map Right msgs

batchSendConnMessagesB :: User -> Connection -> MsgFlags -> NonEmpty (Either ChatError SndMessage) -> CM ([Either ChatError SndMessage], Maybe PQEncryption)
batchSendConnMessagesB _user conn msgFlags msgs_ = do
  let batched_ = batchSndMessagesJSON msgs_
  case L.nonEmpty batched_ of
    Just batched' -> do
      let msgReqs = L.map (fmap (msgBatchReq conn msgFlags)) batched'
      delivered <- deliverMessagesB msgReqs
      let msgs' = concat $ L.zipWith flattenMsgs batched' delivered
          pqEnc = findLastPQEnc delivered
      when (length msgs' /= length msgs_) $ logError "batchSendConnMessagesB: msgs_ and msgs' length mismatch"
      pure (msgs', pqEnc)
    Nothing -> pure ([], Nothing)
  where
    flattenMsgs :: Either ChatError MsgBatch -> Either ChatError ([Int64], PQEncryption) -> [Either ChatError SndMessage]
    flattenMsgs (Right (MsgBatch _ sndMsgs)) (Right _) = map Right sndMsgs
    flattenMsgs (Right (MsgBatch _ sndMsgs)) (Left ce) = replicate (length sndMsgs) (Left ce)
    flattenMsgs (Left ce) _ = [Left ce] -- restore original ChatError
    findLastPQEnc :: NonEmpty (Either ChatError ([Int64], PQEncryption)) -> Maybe PQEncryption
    findLastPQEnc = foldr' (\x acc -> case x of Right (_, pqEnc) -> Just pqEnc; Left _ -> acc) Nothing

batchSndMessagesJSON :: NonEmpty (Either ChatError SndMessage) -> [Either ChatError MsgBatch]
batchSndMessagesJSON = batchMessages maxEncodedMsgLength . L.toList

msgBatchReq :: Connection -> MsgFlags -> MsgBatch -> ChatMsgReq
msgBatchReq conn msgFlags (MsgBatch batchBody sndMsgs) = (conn, msgFlags, batchBody, map (\SndMessage {msgId} -> msgId) sndMsgs)

encodeConnInfo :: MsgEncodingI e => ChatMsgEvent e -> CM ByteString
encodeConnInfo chatMsgEvent = do
  vr <- chatVersionRange
  encodeConnInfoPQ PQSupportOff (maxVersion vr) chatMsgEvent

encodeConnInfoPQ :: MsgEncodingI e => PQSupport -> VersionChat -> ChatMsgEvent e -> CM ByteString
encodeConnInfoPQ pqSup v chatMsgEvent = do
  vr <- chatVersionRange
  let info = ChatMessage {chatVRange = vr, msgId = Nothing, chatMsgEvent}
  case encodeChatMessage maxEncodedInfoLength info of
    ECMEncoded connInfo -> case pqSup of
      PQSupportOn | v >= pqEncryptionCompressionVersion && B.length connInfo > maxCompressedInfoLength -> do
        let connInfo' = compressedBatchMsgBody_ connInfo
        when (B.length connInfo' > maxCompressedInfoLength) $ throwChatError $ CEException "large compressed info"
        pure connInfo'
      _ -> pure connInfo
    ECMLarge -> throwChatError $ CEException "large info"

deliverMessage :: Connection -> CMEventTag e -> MsgBody -> MessageId -> CM (Int64, PQEncryption)
deliverMessage conn cmEventTag msgBody msgId = do
  let msgFlags = MsgFlags {notification = hasNotification cmEventTag}
  deliverMessage' conn msgFlags msgBody msgId

deliverMessage' :: Connection -> MsgFlags -> MsgBody -> MessageId -> CM (Int64, PQEncryption)
deliverMessage' conn msgFlags msgBody msgId =
  deliverMessages ((conn, msgFlags, msgBody, [msgId]) :| []) >>= \case
    r :| [] -> case r of
      Right ([deliveryId], pqEnc) -> pure (deliveryId, pqEnc)
      Right (deliveryIds, _) -> throwChatError $ CEInternalError $ "deliverMessage: expected 1 delivery id, got " <> show (length deliveryIds)
      Left e -> throwError e
    rs -> throwChatError $ CEInternalError $ "deliverMessage: expected 1 result, got " <> show (length rs)

-- [MessageId] - SndMessage ids inside MsgBatch, or single message id
type ChatMsgReq = (Connection, MsgFlags, MsgBody, [MessageId])

deliverMessages :: NonEmpty ChatMsgReq -> CM (NonEmpty (Either ChatError ([Int64], PQEncryption)))
deliverMessages msgs = deliverMessagesB $ L.map Right msgs

deliverMessagesB :: NonEmpty (Either ChatError ChatMsgReq) -> CM (NonEmpty (Either ChatError ([Int64], PQEncryption)))
deliverMessagesB msgReqs = do
  msgReqs' <- liftIO compressBodies
  sent <- L.zipWith prepareBatch msgReqs' <$> withAgent (`sendMessagesB` snd (mapAccumL toAgent Nothing msgReqs'))
  lift . void $ withStoreBatch' $ \db -> map (updatePQSndEnabled db) (rights . L.toList $ sent)
  lift . withStoreBatch $ \db -> L.map (bindRight $ createDelivery db) sent
  where
    compressBodies =
      forME msgReqs $ \mr@(conn@Connection {pqSupport, connChatVersion = v}, msgFlags, msgBody, msgIds) ->
        runExceptT $ case pqSupport of
          -- we only compress messages when:
          -- 1) PQ support is enabled
          -- 2) version is compatible with compression
          -- 3) message is longer than max compressed size (as this function is not used for batched messages anyway)
          PQSupportOn | v >= pqEncryptionCompressionVersion && B.length msgBody > maxCompressedMsgLength -> do
            let msgBody' = compressedBatchMsgBody_ msgBody
            when (B.length msgBody' > maxCompressedMsgLength) $ throwError $ ChatError $ CEException "large compressed message"
            pure (conn, msgFlags, msgBody', msgIds)
          _ -> pure mr
    toAgent prev = \case
      Right (conn@Connection {connId, pqEncryption}, msgFlags, msgBody, _msgIds) ->
        let cId = case prev of
              Just prevId | prevId == connId -> ""
              _ -> aConnId conn
         in (Just connId, Right (cId, pqEncryption, msgFlags, msgBody))
      Left _ce -> (prev, Left (AP.INTERNAL "ChatError, skip")) -- as long as it is Left, the agent batchers should just step over it
    prepareBatch (Right req) (Right ar) = Right (req, ar)
    prepareBatch (Left ce) _ = Left ce -- restore original ChatError
    prepareBatch _ (Left ae) = Left $ ChatErrorAgent ae Nothing
    createDelivery :: DB.Connection -> (ChatMsgReq, (AgentMsgId, PQEncryption)) -> IO (Either ChatError ([Int64], PQEncryption))
    createDelivery db ((Connection {connId}, _, _, msgIds), (agentMsgId, pqEnc')) = do
      Right . (,pqEnc') <$> mapM (createSndMsgDelivery db (SndMsgDelivery {connId, agentMsgId})) msgIds
    updatePQSndEnabled :: DB.Connection -> (ChatMsgReq, (AgentMsgId, PQEncryption)) -> IO ()
    updatePQSndEnabled db ((Connection {connId, pqSndEnabled}, _, _, _), (_, pqSndEnabled')) =
      case (pqSndEnabled, pqSndEnabled') of
        (Just b, b') | b' /= b -> updatePQ
        (Nothing, PQEncOn) -> updatePQ
        _ -> pure ()
      where
        updatePQ = updateConnPQSndEnabled db connId pqSndEnabled'

sendGroupMessage :: MsgEncodingI e => User -> GroupInfo -> [GroupMember] -> ChatMsgEvent e -> CM SndMessage
sendGroupMessage user gInfo members chatMsgEvent = do
  sendGroupMessages user gInfo members (chatMsgEvent :| []) >>= \case
    ((Right msg) :| [], _) -> pure msg
    _ -> throwChatError $ CEInternalError "sendGroupMessage: expected 1 message"

sendGroupMessage' :: MsgEncodingI e => User -> GroupInfo -> [GroupMember] -> ChatMsgEvent e -> CM SndMessage
sendGroupMessage' user gInfo members chatMsgEvent =
  sendGroupMessages_ user gInfo members (chatMsgEvent :| []) >>= \case
    ((Right msg) :| [], _) -> pure msg
    _ -> throwChatError $ CEInternalError "sendGroupMessage': expected 1 message"

sendGroupMessages :: MsgEncodingI e => User -> GroupInfo -> [GroupMember] -> NonEmpty (ChatMsgEvent e) -> CM (NonEmpty (Either ChatError SndMessage), GroupSndResult)
sendGroupMessages user gInfo members events = do
  when shouldSendProfileUpdate $
    sendProfileUpdate `catchChatError` (toView . CRChatError (Just user))
  sendGroupMessages_ user gInfo members events
  where
    User {profile = p, userMemberProfileUpdatedAt} = user
    GroupInfo {userMemberProfileSentAt} = gInfo
    shouldSendProfileUpdate
      | incognitoMembership gInfo = False
      | otherwise =
          case (userMemberProfileSentAt, userMemberProfileUpdatedAt) of
            (Just lastSentTs, Just lastUpdateTs) -> lastSentTs < lastUpdateTs
            (Nothing, Just _) -> True
            _ -> False
    sendProfileUpdate = do
      let members' = filter (`supportsVersion` memberProfileUpdateVersion) members
          profileUpdateEvent = XInfo $ redactedMemberProfile $ fromLocalProfile p
      void $ sendGroupMessage' user gInfo members' profileUpdateEvent
      currentTs <- liftIO getCurrentTime
      withStore' $ \db -> updateUserMemberProfileSentAt db user gInfo currentTs

data GroupSndResult = GroupSndResult
  { sentTo :: [(GroupMemberId, Either ChatError [MessageId], Either ChatError ([Int64], PQEncryption))],
    pending :: [(GroupMemberId, Either ChatError MessageId, Either ChatError ())],
    forwarded :: [GroupMember]
  }

sendGroupMessages_ :: MsgEncodingI e => User -> GroupInfo -> [GroupMember] -> NonEmpty (ChatMsgEvent e) -> CM (NonEmpty (Either ChatError SndMessage), GroupSndResult)
sendGroupMessages_ _user gInfo@GroupInfo {groupId} members events = do
  let idsEvts = L.map (GroupId groupId,) events
  sndMsgs_ <- lift $ createSndMessages idsEvts
  recipientMembers <- liftIO $ shuffleMembers (filter memberCurrent members)
  let msgFlags = MsgFlags {notification = any (hasNotification . toCMEventTag) events}
      (toSendSeparate, toSendBatched, toPending, forwarded, _, dups) =
        foldr' addMember ([], [], [], [], S.empty, 0 :: Int) recipientMembers
  when (dups /= 0) $ logError $ "sendGroupMessages_: " <> tshow dups <> " duplicate members"
  -- TODO PQ either somehow ensure that group members connections cannot have pqSupport/pqEncryption or pass Off's here
  -- Deliver to toSend members
  let (sendToMemIds, msgReqs) = prepareMsgReqs msgFlags sndMsgs_ toSendSeparate toSendBatched
  delivered <- maybe (pure []) (fmap L.toList . deliverMessagesB) $ L.nonEmpty msgReqs
  when (length delivered /= length sendToMemIds) $ logError "sendGroupMessages_: sendToMemIds and delivered length mismatch"
  -- Save as pending for toPending members
  let (pendingMemIds, pendingReqs) = preparePending sndMsgs_ toPending
  stored <- lift $ withStoreBatch (\db -> map (bindRight $ createPendingMsg db) pendingReqs)
  when (length stored /= length pendingMemIds) $ logError "sendGroupMessages_: pendingMemIds and stored length mismatch"
  -- Zip for easier access to results
  let sentTo = zipWith3 (\mId mReq r -> (mId, fmap (\(_, _, _, msgIds) -> msgIds) mReq, r)) sendToMemIds msgReqs delivered
      pending = zipWith3 (\mId pReq r -> (mId, fmap snd pReq, r)) pendingMemIds pendingReqs stored
  pure (sndMsgs_, GroupSndResult {sentTo, pending, forwarded})
  where
    shuffleMembers :: [GroupMember] -> IO [GroupMember]
    shuffleMembers ms = do
      let (adminMs, otherMs) = partition isAdmin ms
      liftM2 (<>) (shuffle adminMs) (shuffle otherMs)
      where
        isAdmin GroupMember {memberRole} = memberRole >= GRAdmin
    addMember m acc@(toSendSeparate, toSendBatched, pending, forwarded, !mIds, !dups) =
      case memberSendAction gInfo events members m of
        Just a
          | mId `S.member` mIds -> (toSendSeparate, toSendBatched, pending, forwarded, mIds, dups + 1)
          | otherwise -> case a of
              MSASend conn -> ((m, conn) : toSendSeparate, toSendBatched, pending, forwarded, mIds', dups)
              MSASendBatched conn -> (toSendSeparate, (m, conn) : toSendBatched, pending, forwarded, mIds', dups)
              MSAPending -> (toSendSeparate, toSendBatched, m : pending, forwarded, mIds', dups)
              MSAForwarded -> (toSendSeparate, toSendBatched, pending, m : forwarded, mIds', dups)
        Nothing -> acc
      where
        mId = groupMemberId' m
        mIds' = S.insert mId mIds
    prepareMsgReqs :: MsgFlags -> NonEmpty (Either ChatError SndMessage) -> [(GroupMember, Connection)] -> [(GroupMember, Connection)] -> ([GroupMemberId], [Either ChatError ChatMsgReq])
    prepareMsgReqs msgFlags msgs_ toSendSeparate toSendBatched = do
      let batched_ = batchSndMessagesJSON msgs_
      case L.nonEmpty batched_ of
        Just batched' -> do
          let (memsSep, mreqsSep) = foldr' foldMsgs ([], []) toSendSeparate
              (memsBtch, mreqsBtch) = foldr' (foldBatches batched') ([], []) toSendBatched
          (memsSep <> memsBtch, mreqsSep <> mreqsBtch)
        Nothing -> ([], [])
      where
        foldMsgs :: (GroupMember, Connection) -> ([GroupMemberId], [Either ChatError ChatMsgReq]) -> ([GroupMemberId], [Either ChatError ChatMsgReq])
        foldMsgs (GroupMember {groupMemberId}, conn) memIdsReqs =
          foldr' (\msg_ (memIds, reqs) -> (groupMemberId : memIds, fmap sndMessageReq msg_ : reqs)) memIdsReqs msgs_
          where
            sndMessageReq :: SndMessage -> ChatMsgReq
            sndMessageReq SndMessage {msgId, msgBody} = (conn, msgFlags, msgBody, [msgId])
        foldBatches :: NonEmpty (Either ChatError MsgBatch) -> (GroupMember, Connection) -> ([GroupMemberId], [Either ChatError ChatMsgReq]) -> ([GroupMemberId], [Either ChatError ChatMsgReq])
        foldBatches batched' (GroupMember {groupMemberId}, conn) memIdsReqs =
          foldr' (\batch_ (memIds, reqs) -> (groupMemberId : memIds, fmap (msgBatchReq conn msgFlags) batch_ : reqs)) memIdsReqs batched'
    preparePending :: NonEmpty (Either ChatError SndMessage) -> [GroupMember] -> ([GroupMemberId], [Either ChatError (GroupMemberId, MessageId)])
    preparePending msgs_ =
      foldr' foldMsgs ([], [])
      where
        foldMsgs :: GroupMember -> ([GroupMemberId], [Either ChatError (GroupMemberId, MessageId)]) -> ([GroupMemberId], [Either ChatError (GroupMemberId, MessageId)])
        foldMsgs GroupMember {groupMemberId} memIdsReqs =
          foldr' (\msg_ (memIds, reqs) -> (groupMemberId : memIds, fmap pendingReq msg_ : reqs)) memIdsReqs msgs_
          where
            pendingReq :: SndMessage -> (GroupMemberId, MessageId)
            pendingReq SndMessage {msgId} = (groupMemberId, msgId)
    createPendingMsg :: DB.Connection -> (GroupMemberId, MessageId) -> IO (Either ChatError ())
    createPendingMsg db (groupMemberId, msgId) =
      createPendingGroupMessage db groupMemberId msgId Nothing $> Right ()

data MemberSendAction = MSASend Connection | MSASendBatched Connection | MSAPending | MSAForwarded

memberSendAction :: GroupInfo -> NonEmpty (ChatMsgEvent e) -> [GroupMember] -> GroupMember -> Maybe MemberSendAction
memberSendAction gInfo events members m@GroupMember {memberRole} = case memberConn m of
  Nothing -> pendingOrForwarded
  Just conn@Connection {connStatus}
    | connDisabled conn || connStatus == ConnDeleted -> Nothing
    | connInactive conn -> Just MSAPending
    | connStatus == ConnSndReady || connStatus == ConnReady -> sendBatchedOrSeparate conn
    | otherwise -> pendingOrForwarded
  where
    sendBatchedOrSeparate conn
      -- admin doesn't support batch forwarding - send messages separately so that admin can forward one by one
      | memberRole >= GRAdmin && not (m `supportsVersion` batchSend2Version) = Just (MSASend conn)
      -- either member is not admin, or admin supports batched forwarding
      | otherwise = Just (MSASendBatched conn)
    pendingOrForwarded = case memberCategory m of
      GCUserMember -> Nothing -- shouldn't happen
      GCInviteeMember -> Just MSAPending
      GCHostMember -> Just MSAPending
      GCPreMember -> forwardSupportedOrPending (invitedByGroupMemberId $ membership gInfo)
      GCPostMember -> forwardSupportedOrPending (invitedByGroupMemberId m)
      where
        forwardSupportedOrPending invitingMemberId_
          | membersSupport && all isForwardedGroupMsg events = Just MSAForwarded
          | any isXGrpMsgForward events = Nothing
          | otherwise = Just MSAPending
          where
            membersSupport =
              m `supportsVersion` groupForwardVersion && invitingMemberSupportsForward
            invitingMemberSupportsForward = case invitingMemberId_ of
              Just invMemberId ->
                -- can be optimized for large groups by replacing [GroupMember] with Map GroupMemberId GroupMember
                case find (\m' -> groupMemberId' m' == invMemberId) members of
                  Just invitingMember -> invitingMember `supportsVersion` groupForwardVersion
                  Nothing -> False
              Nothing -> False
            isXGrpMsgForward event = case event of
              XGrpMsgForward {} -> True
              _ -> False

sendGroupMemberMessage :: MsgEncodingI e => User -> GroupInfo -> GroupMember -> ChatMsgEvent e -> Maybe Int64 -> CM () -> CM ()
sendGroupMemberMessage user gInfo@GroupInfo {groupId} m@GroupMember {groupMemberId} chatMsgEvent introId_ postDeliver = do
  msg <- createSndMessage chatMsgEvent (GroupId groupId)
  messageMember msg `catchChatError` (toView . CRChatError (Just user))
  where
    messageMember :: SndMessage -> CM ()
    messageMember SndMessage {msgId, msgBody} = forM_ (memberSendAction gInfo (chatMsgEvent :| []) [m] m) $ \case
      MSASend conn -> deliverMessage conn (toCMEventTag chatMsgEvent) msgBody msgId >> postDeliver
      MSASendBatched conn -> deliverMessage conn (toCMEventTag chatMsgEvent) msgBody msgId >> postDeliver
      MSAPending -> withStore' $ \db -> createPendingGroupMessage db groupMemberId msgId introId_
      MSAForwarded -> pure ()

-- TODO ensure order - pending messages interleave with user input messages
sendPendingGroupMessages :: User -> GroupMember -> Connection -> CM ()
sendPendingGroupMessages user GroupMember {groupMemberId} conn = do
  pgms <- withStore' $ \db -> getPendingGroupMessages db groupMemberId
  forM_ (L.nonEmpty pgms) $ \pgms' -> do
    let msgs = L.map (\(sndMsg, _, _) -> sndMsg) pgms'
    void $ batchSendConnMessages user conn MsgFlags {notification = True} msgs
    lift . void . withStoreBatch' $ \db -> L.map (\SndMessage {msgId} -> deletePendingGroupMessage db groupMemberId msgId) msgs
    lift . void . withStoreBatch' $ \db -> L.map (\(_, tag, introId_) -> updateIntro_ db tag introId_) pgms'
  where
    updateIntro_ :: DB.Connection -> ACMEventTag -> Maybe Int64 -> IO ()
    updateIntro_ db tag introId_ = case (tag, introId_) of
      (ACMEventTag _ XGrpMemFwd_, Just introId) -> updateIntroStatus db introId GMIntroInvForwarded
      _ -> pure ()

saveDirectRcvMSG :: MsgEncodingI e => Connection -> MsgMeta -> MsgBody -> ChatMessage e -> CM (Connection, RcvMessage)
saveDirectRcvMSG conn@Connection {connId} agentMsgMeta msgBody ChatMessage {chatVRange, msgId = sharedMsgId_, chatMsgEvent} = do
  conn' <- updatePeerChatVRange conn chatVRange
  let agentMsgId = fst $ recipient agentMsgMeta
      newMsg = NewRcvMessage {chatMsgEvent, msgBody}
      rcvMsgDelivery = RcvMsgDelivery {connId, agentMsgId, agentMsgMeta}
  msg <- withStore $ \db -> createNewMessageAndRcvMsgDelivery db (ConnectionId connId) newMsg sharedMsgId_ rcvMsgDelivery Nothing
  pure (conn', msg)

saveGroupRcvMsg :: MsgEncodingI e => User -> GroupId -> GroupMember -> Connection -> MsgMeta -> MsgBody -> ChatMessage e -> CM (GroupMember, Connection, RcvMessage)
saveGroupRcvMsg user groupId authorMember conn@Connection {connId} agentMsgMeta msgBody ChatMessage {chatVRange, msgId = sharedMsgId_, chatMsgEvent} = do
  (am'@GroupMember {memberId = amMemId, groupMemberId = amGroupMemId}, conn') <- updateMemberChatVRange authorMember conn chatVRange
  let agentMsgId = fst $ recipient agentMsgMeta
      newMsg = NewRcvMessage {chatMsgEvent, msgBody}
      rcvMsgDelivery = RcvMsgDelivery {connId, agentMsgId, agentMsgMeta}
  msg <-
    withStore (\db -> createNewMessageAndRcvMsgDelivery db (GroupId groupId) newMsg sharedMsgId_ rcvMsgDelivery $ Just amGroupMemId)
      `catchChatError` \e -> case e of
        ChatErrorStore (SEDuplicateGroupMessage _ _ _ (Just forwardedByGroupMemberId)) -> do
          vr <- chatVersionRange
          fm <- withStore $ \db -> getGroupMember db vr user groupId forwardedByGroupMemberId
          forM_ (memberConn fm) $ \fmConn ->
            void $ sendDirectMemberMessage fmConn (XGrpMemCon amMemId) groupId
          throwError e
        _ -> throwError e
  pure (am', conn', msg)

saveGroupFwdRcvMsg :: MsgEncodingI e => User -> GroupId -> GroupMember -> GroupMember -> MsgBody -> ChatMessage e -> CM RcvMessage
saveGroupFwdRcvMsg user groupId forwardingMember refAuthorMember@GroupMember {memberId = refMemberId} msgBody ChatMessage {msgId = sharedMsgId_, chatMsgEvent} = do
  let newMsg = NewRcvMessage {chatMsgEvent, msgBody}
      fwdMemberId = Just $ groupMemberId' forwardingMember
      refAuthorId = Just $ groupMemberId' refAuthorMember
  withStore (\db -> createNewRcvMessage db (GroupId groupId) newMsg sharedMsgId_ refAuthorId fwdMemberId)
    `catchChatError` \e -> case e of
      ChatErrorStore (SEDuplicateGroupMessage _ _ (Just authorGroupMemberId) Nothing) -> do
        vr <- chatVersionRange
        am@GroupMember {memberId = amMemberId} <- withStore $ \db -> getGroupMember db vr user groupId authorGroupMemberId
        if sameMemberId refMemberId am
          then forM_ (memberConn forwardingMember) $ \fmConn ->
            void $ sendDirectMemberMessage fmConn (XGrpMemCon amMemberId) groupId
          else toView $ CRMessageError user "error" "saveGroupFwdRcvMsg: referenced author member id doesn't match message member id"
        throwError e
      _ -> throwError e

saveSndChatItem :: ChatTypeI c => User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> CM (ChatItem c 'MDSnd)
saveSndChatItem user cd msg content = saveSndChatItem' user cd msg content Nothing Nothing Nothing Nothing False

saveSndChatItem' :: ChatTypeI c => User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> Maybe (CIFile 'MDSnd) -> Maybe (CIQuote c) -> Maybe CIForwardedFrom -> Maybe CITimed -> Bool -> CM (ChatItem c 'MDSnd)
saveSndChatItem' user cd msg content ciFile quotedItem itemForwarded itemTimed live =
  saveSndChatItems user cd [Right NewSndChatItemData {msg, content, ciFile, quotedItem, itemForwarded}] itemTimed live >>= \case
    [Right ci] -> pure ci
    _ -> throwChatError $ CEInternalError "saveSndChatItem': expected 1 item"

data NewSndChatItemData c = NewSndChatItemData
  { msg :: SndMessage,
    content :: CIContent 'MDSnd,
    ciFile :: Maybe (CIFile 'MDSnd),
    quotedItem :: Maybe (CIQuote c),
    itemForwarded :: Maybe CIForwardedFrom
  }

saveSndChatItems ::
  forall c.
  ChatTypeI c =>
  User ->
  ChatDirection c 'MDSnd ->
  [Either ChatError (NewSndChatItemData c)] ->
  Maybe CITimed ->
  Bool ->
  CM [Either ChatError (ChatItem c 'MDSnd)]
saveSndChatItems user cd itemsData itemTimed live = do
  createdAt <- liftIO getCurrentTime
  when (contactChatDeleted cd || any (\NewSndChatItemData {content} -> ciRequiresAttention content) (rights itemsData)) $
    withStore' (\db -> updateChatTs db user cd createdAt)
  lift $ withStoreBatch (\db -> map (bindRight $ createItem db createdAt) itemsData)
  where
    createItem :: DB.Connection -> UTCTime -> NewSndChatItemData c -> IO (Either ChatError (ChatItem c 'MDSnd))
    createItem db createdAt NewSndChatItemData {msg = msg@SndMessage {sharedMsgId}, content, ciFile, quotedItem, itemForwarded} = do
      ciId <- createNewSndChatItem db user cd msg content quotedItem itemForwarded itemTimed live createdAt
      forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
      pure $ Right $ mkChatItem cd ciId content ciFile quotedItem (Just sharedMsgId) itemForwarded itemTimed live createdAt Nothing createdAt

saveRcvChatItem :: (ChatTypeI c, ChatTypeQuotable c) => User -> ChatDirection c 'MDRcv -> RcvMessage -> UTCTime -> CIContent 'MDRcv -> CM (ChatItem c 'MDRcv)
saveRcvChatItem user cd msg@RcvMessage {sharedMsgId_} brokerTs content =
  saveRcvChatItem' user cd msg sharedMsgId_ brokerTs content Nothing Nothing False

saveRcvChatItem' :: (ChatTypeI c, ChatTypeQuotable c) => User -> ChatDirection c 'MDRcv -> RcvMessage -> Maybe SharedMsgId -> UTCTime -> CIContent 'MDRcv -> Maybe (CIFile 'MDRcv) -> Maybe CITimed -> Bool -> CM (ChatItem c 'MDRcv)
saveRcvChatItem' user cd msg@RcvMessage {forwardedByMember} sharedMsgId_ brokerTs content ciFile itemTimed live = do
  createdAt <- liftIO getCurrentTime
  (ciId, quotedItem, itemForwarded) <- withStore' $ \db -> do
    when (ciRequiresAttention content || contactChatDeleted cd) $ updateChatTs db user cd createdAt
    r@(ciId, _, _) <- createNewRcvChatItem db user cd msg sharedMsgId_ content itemTimed live brokerTs createdAt
    forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
    pure r
  pure $ mkChatItem cd ciId content ciFile quotedItem sharedMsgId_ itemForwarded itemTimed live brokerTs forwardedByMember createdAt

mkChatItem :: (ChatTypeI c, MsgDirectionI d) => ChatDirection c d -> ChatItemId -> CIContent d -> Maybe (CIFile d) -> Maybe (CIQuote c) -> Maybe SharedMsgId -> Maybe CIForwardedFrom -> Maybe CITimed -> Bool -> ChatItemTs -> Maybe GroupMemberId -> UTCTime -> ChatItem c d
mkChatItem cd ciId content file quotedItem sharedMsgId itemForwarded itemTimed live itemTs forwardedByMember currentTs =
  let itemText = ciContentToText content
      itemStatus = ciCreateStatus content
      meta = mkCIMeta ciId content itemText itemStatus Nothing sharedMsgId itemForwarded Nothing False itemTimed (justTrue live) currentTs itemTs forwardedByMember currentTs currentTs
   in ChatItem {chatDir = toCIDirection cd, meta, content, formattedText = parseMaybeMarkdownList itemText, quotedItem, reactions = [], file}

deleteDirectCIs :: User -> Contact -> [CChatItem 'CTDirect] -> Bool -> Bool -> CM ChatResponse
deleteDirectCIs user ct items byUser timed = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  deleteCIFiles user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (deleteItem db) items)
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  pure $ CRChatItemsDeleted user deletions byUser timed
  where
    deleteItem db (CChatItem md ci) = do
      deleteDirectChatItem db user ct ci
      pure $ contactDeletion md ct ci Nothing

deleteGroupCIs :: User -> GroupInfo -> [CChatItem 'CTGroup] -> Bool -> Bool -> Maybe GroupMember -> UTCTime -> CM ChatResponse
deleteGroupCIs user gInfo items byUser timed byGroupMember_ deletedTs = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  deleteCIFiles user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (deleteItem db) items)
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  pure $ CRChatItemsDeleted user deletions byUser timed
  where
    deleteItem :: DB.Connection -> CChatItem 'CTGroup -> IO ChatItemDeletion
    deleteItem db (CChatItem md ci) = do
      ci' <- case byGroupMember_ of
        Just m -> Just <$> updateGroupChatItemModerated db user gInfo ci m deletedTs
        Nothing -> Nothing <$ deleteGroupChatItem db user gInfo ci
      pure $ groupDeletion md gInfo ci ci'

deleteLocalCIs :: User -> NoteFolder -> [CChatItem 'CTLocal] -> Bool -> Bool -> CM ChatResponse
deleteLocalCIs user nf items byUser timed = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  deleteFilesLocally ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (deleteItem db) items)
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  pure $ CRChatItemsDeleted user deletions byUser timed
  where
    deleteItem db (CChatItem md ci) = do
      deleteLocalChatItem db user nf ci
      pure $ ChatItemDeletion (nfItem md ci) Nothing
    nfItem :: MsgDirectionI d => SMsgDirection d -> ChatItem 'CTLocal d -> AChatItem
    nfItem md = AChatItem SCTLocal md (LocalChat nf)

deleteCIFiles :: User -> [CIFileInfo] -> CM ()
deleteCIFiles user filesInfo = do
  cancelFilesInProgress user filesInfo
  deleteFilesLocally filesInfo

markDirectCIsDeleted :: User -> Contact -> [CChatItem 'CTDirect] -> Bool -> UTCTime -> CM ChatResponse
markDirectCIsDeleted user ct items byUser deletedTs = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  cancelFilesInProgress user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (markDeleted db) items)
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  pure $ CRChatItemsDeleted user deletions byUser False
  where
    markDeleted db (CChatItem md ci) = do
      ci' <- markDirectChatItemDeleted db user ct ci deletedTs
      pure $ contactDeletion md ct ci (Just ci')

markGroupCIsDeleted :: User -> GroupInfo -> [CChatItem 'CTGroup] -> Bool -> Maybe GroupMember -> UTCTime -> CM ChatResponse
markGroupCIsDeleted user gInfo items byUser byGroupMember_ deletedTs = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  cancelFilesInProgress user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (markDeleted db) items)
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  pure $ CRChatItemsDeleted user deletions byUser False
  where
    markDeleted db (CChatItem md ci) = do
      ci' <- markGroupChatItemDeleted db user gInfo ci byGroupMember_ deletedTs
      pure $ groupDeletion md gInfo ci (Just ci')

groupDeletion :: MsgDirectionI d => SMsgDirection d -> GroupInfo -> ChatItem 'CTGroup d -> Maybe (ChatItem 'CTGroup d) -> ChatItemDeletion
groupDeletion md g ci ci' = ChatItemDeletion (gItem ci) (gItem <$> ci')
  where
    gItem = AChatItem SCTGroup md (GroupChat g)

contactDeletion :: MsgDirectionI d => SMsgDirection d -> Contact -> ChatItem 'CTDirect d -> Maybe (ChatItem 'CTDirect d) -> ChatItemDeletion
contactDeletion md ct ci ci' = ChatItemDeletion (ctItem ci) (ctItem <$> ci')
  where
    ctItem = AChatItem SCTDirect md (DirectChat ct)

createAgentConnectionAsync :: ConnectionModeI c => User -> CommandFunction -> Bool -> SConnectionMode c -> SubscriptionMode -> CM (CommandId, ConnId)
createAgentConnectionAsync user cmdFunction enableNtfs cMode subMode = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing cmdFunction
  connId <- withAgent $ \a -> createConnectionAsync a (aUserId user) (aCorrId cmdId) enableNtfs cMode IKPQOff subMode
  pure (cmdId, connId)

joinAgentConnectionAsync :: User -> Bool -> ConnectionRequestUri c -> ConnInfo -> SubscriptionMode -> CM (CommandId, ConnId)
joinAgentConnectionAsync user enableNtfs cReqUri cInfo subMode = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing CFJoinConn
  connId <- withAgent $ \a -> joinConnectionAsync a (aUserId user) (aCorrId cmdId) enableNtfs cReqUri cInfo PQSupportOff subMode
  pure (cmdId, connId)

allowAgentConnectionAsync :: MsgEncodingI e => User -> Connection -> ConfirmationId -> ChatMsgEvent e -> CM ()
allowAgentConnectionAsync user conn@Connection {connId, pqSupport, connChatVersion} confId msg = do
  cmdId <- withStore' $ \db -> createCommand db user (Just connId) CFAllowConn
  dm <- encodeConnInfoPQ pqSupport connChatVersion msg
  withAgent $ \a -> allowConnectionAsync a (aCorrId cmdId) (aConnId conn) confId dm
  withStore' $ \db -> updateConnectionStatus db conn ConnAccepted

agentAcceptContactAsync :: MsgEncodingI e => User -> Bool -> InvitationId -> ChatMsgEvent e -> SubscriptionMode -> PQSupport -> VersionChat -> CM (CommandId, ConnId)
agentAcceptContactAsync user enableNtfs invId msg subMode pqSup chatV = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing CFAcceptContact
  dm <- encodeConnInfoPQ pqSup chatV msg
  connId <- withAgent $ \a -> acceptContactAsync a (aCorrId cmdId) enableNtfs invId dm pqSup subMode
  pure (cmdId, connId)

deleteAgentConnectionAsync :: User -> ConnId -> CM ()
deleteAgentConnectionAsync user acId = deleteAgentConnectionAsync' user acId False

deleteAgentConnectionAsync' :: User -> ConnId -> Bool -> CM ()
deleteAgentConnectionAsync' user acId waitDelivery = do
  withAgent (\a -> deleteConnectionAsync a waitDelivery acId) `catchChatError` (toView . CRChatError (Just user))

deleteAgentConnectionsAsync :: User -> [ConnId] -> CM ()
deleteAgentConnectionsAsync user acIds = deleteAgentConnectionsAsync' user acIds False

deleteAgentConnectionsAsync' :: User -> [ConnId] -> Bool -> CM ()
deleteAgentConnectionsAsync' _ [] _ = pure ()
deleteAgentConnectionsAsync' user acIds waitDelivery = do
  withAgent (\a -> deleteConnectionsAsync a waitDelivery acIds) `catchChatError` (toView . CRChatError (Just user))

agentXFTPDeleteRcvFile :: RcvFileId -> FileTransferId -> CM ()
agentXFTPDeleteRcvFile aFileId fileId = do
  lift $ withAgent' (`xftpDeleteRcvFile` aFileId)
  withStore' $ \db -> setRcvFTAgentDeleted db fileId

agentXFTPDeleteRcvFiles :: [(XFTPRcvFile, FileTransferId)] -> CM' ()
agentXFTPDeleteRcvFiles rcvFiles = do
  let rcvFiles' = filter (not . agentRcvFileDeleted . fst) rcvFiles
      rfIds = mapMaybe fileIds rcvFiles'
  withAgent' $ \a -> xftpDeleteRcvFiles a (map fst rfIds)
  void . withStoreBatch' $ \db -> map (setRcvFTAgentDeleted db . snd) rfIds
  where
    fileIds :: (XFTPRcvFile, FileTransferId) -> Maybe (RcvFileId, FileTransferId)
    fileIds (XFTPRcvFile {agentRcvFileId = Just (AgentRcvFileId aFileId)}, fileId) = Just (aFileId, fileId)
    fileIds _ = Nothing

agentXFTPDeleteSndFileRemote :: User -> XFTPSndFile -> FileTransferId -> CM' ()
agentXFTPDeleteSndFileRemote user xsf fileId =
  agentXFTPDeleteSndFilesRemote user [(xsf, fileId)]

agentXFTPDeleteSndFilesRemote :: User -> [(XFTPSndFile, FileTransferId)] -> CM' ()
agentXFTPDeleteSndFilesRemote user sndFiles = do
  (_errs, redirects) <- partitionEithers <$> withStoreBatch' (\db -> map (lookupFileTransferRedirectMeta db user . snd) sndFiles)
  let redirects' = mapMaybe mapRedirectMeta $ concat redirects
      sndFilesAll = redirects' <> sndFiles
      sndFilesAll' = filter (not . agentSndFileDeleted . fst) sndFilesAll
  -- while file is being prepared and uploaded, it would not have description available;
  -- this partitions files into those with and without descriptions -
  -- files with description are deleted remotely, files without description are deleted internally
  (sfsNoDescr, sfsWithDescr) <- partitionSndDescr sndFilesAll' [] []
  withAgent' $ \a -> xftpDeleteSndFilesInternal a sfsNoDescr
  withAgent' $ \a -> xftpDeleteSndFilesRemote a (aUserId user) sfsWithDescr
  void . withStoreBatch' $ \db -> map (setSndFTAgentDeleted db user . snd) sndFilesAll'
  where
    mapRedirectMeta :: FileTransferMeta -> Maybe (XFTPSndFile, FileTransferId)
    mapRedirectMeta FileTransferMeta {fileId = fileId, xftpSndFile = Just sndFileRedirect} = Just (sndFileRedirect, fileId)
    mapRedirectMeta _ = Nothing
    partitionSndDescr ::
      [(XFTPSndFile, FileTransferId)] ->
      [SndFileId] ->
      [(SndFileId, ValidFileDescription 'FSender)] ->
      CM' ([SndFileId], [(SndFileId, ValidFileDescription 'FSender)])
    partitionSndDescr [] filesWithoutDescr filesWithDescr = pure (filesWithoutDescr, filesWithDescr)
    partitionSndDescr ((XFTPSndFile {agentSndFileId = AgentSndFileId aFileId, privateSndFileDescr}, _) : xsfs) filesWithoutDescr filesWithDescr =
      case privateSndFileDescr of
        Nothing -> partitionSndDescr xsfs (aFileId : filesWithoutDescr) filesWithDescr
        Just sfdText ->
          tryChatError' (parseFileDescription sfdText) >>= \case
            Left _ -> partitionSndDescr xsfs (aFileId : filesWithoutDescr) filesWithDescr
            Right sfd -> partitionSndDescr xsfs filesWithoutDescr ((aFileId, sfd) : filesWithDescr)

userProfileToSend :: User -> Maybe Profile -> Maybe Contact -> Bool -> Profile
userProfileToSend user@User {profile = p} incognitoProfile ct inGroup = do
  let p' = fromMaybe (fromLocalProfile p) incognitoProfile
  if inGroup
    then redactedMemberProfile p'
    else
      let userPrefs = maybe (preferences' user) (const Nothing) incognitoProfile
       in (p' :: Profile) {preferences = Just . toChatPrefs $ mergePreferences (userPreferences <$> ct) userPrefs}

createRcvFeatureItems :: User -> Contact -> Contact -> CM' ()
createRcvFeatureItems user ct ct' =
  createFeatureItems user ct ct' CDDirectRcv CIRcvChatFeature CIRcvChatPreference contactPreference

createSndFeatureItems :: User -> Contact -> Contact -> CM' ()
createSndFeatureItems user ct ct' =
  createFeatureItems user ct ct' CDDirectSnd CISndChatFeature CISndChatPreference getPref
  where
    getPref ContactUserPreference {userPreference} = case userPreference of
      CUPContact {preference} -> preference
      CUPUser {preference} -> preference

createContactsSndFeatureItems :: User -> [ChangedProfileContact] -> CM' ()
createContactsSndFeatureItems user cts =
  createContactsFeatureItems user cts' CDDirectSnd CISndChatFeature CISndChatPreference getPref
  where
    cts' = map (\ChangedProfileContact {ct, ct'} -> (ct, ct')) cts
    getPref ContactUserPreference {userPreference} = case userPreference of
      CUPContact {preference} -> preference
      CUPUser {preference} -> preference

type FeatureContent a d = ChatFeature -> a -> Maybe Int -> CIContent d

createFeatureItems ::
  MsgDirectionI d =>
  User ->
  Contact ->
  Contact ->
  (Contact -> ChatDirection 'CTDirect d) ->
  FeatureContent PrefEnabled d ->
  FeatureContent FeatureAllowed d ->
  (forall f. ContactUserPreference (FeaturePreference f) -> FeaturePreference f) ->
  CM' ()
createFeatureItems user ct ct' = createContactsFeatureItems user [(ct, ct')]

createContactsFeatureItems ::
  forall d.
  MsgDirectionI d =>
  User ->
  [(Contact, Contact)] ->
  (Contact -> ChatDirection 'CTDirect d) ->
  FeatureContent PrefEnabled d ->
  FeatureContent FeatureAllowed d ->
  (forall f. ContactUserPreference (FeaturePreference f) -> FeaturePreference f) ->
  CM' ()
createContactsFeatureItems user cts chatDir ciFeature ciOffer getPref = do
  let dirsCIContents = map contactChangedFeatures cts
  (errs, acis) <- partitionEithers <$> createInternalItemsForChats user Nothing dirsCIContents
  unless (null errs) $ toView' $ CRChatErrors (Just user) errs
  toView' $ CRNewChatItems user acis
  where
    contactChangedFeatures :: (Contact, Contact) -> (ChatDirection 'CTDirect d, [CIContent d])
    contactChangedFeatures (Contact {mergedPreferences = cups}, ct'@Contact {mergedPreferences = cups'}) = do
      let contents = mapMaybe (\(ACF f) -> featureCIContent_ f) allChatFeatures
      (chatDir ct', contents)
      where
        featureCIContent_ :: forall f. FeatureI f => SChatFeature f -> Maybe (CIContent d)
        featureCIContent_ f
          | state /= state' = Just $ fContent ciFeature state'
          | prefState /= prefState' = Just $ fContent ciOffer prefState'
          | otherwise = Nothing
          where
            fContent :: FeatureContent a d -> (a, Maybe Int) -> CIContent d
            fContent ci (s, param) = ci f' s param
            f' = chatFeature f
            state = featureState cup
            state' = featureState cup'
            prefState = preferenceState $ getPref cup
            prefState' = preferenceState $ getPref cup'
            cup = getContactUserPreference f cups
            cup' = getContactUserPreference f cups'

createGroupFeatureChangedItems :: MsgDirectionI d => User -> ChatDirection 'CTGroup d -> (GroupFeature -> GroupPreference -> Maybe Int -> Maybe GroupMemberRole -> CIContent d) -> GroupInfo -> GroupInfo -> CM ()
createGroupFeatureChangedItems user cd ciContent GroupInfo {fullGroupPreferences = gps} GroupInfo {fullGroupPreferences = gps'} =
  forM_ allGroupFeatures $ \(AGF f) -> do
    let state = groupFeatureState $ getGroupPreference f gps
        pref' = getGroupPreference f gps'
        state'@(_, param', role') = groupFeatureState pref'
    when (state /= state') $
      createInternalChatItem user cd (ciContent (toGroupFeature f) (toGroupPreference pref') param' role') Nothing

sameGroupProfileInfo :: GroupProfile -> GroupProfile -> Bool
sameGroupProfileInfo p p' = p {groupPreferences = Nothing} == p' {groupPreferences = Nothing}

createGroupFeatureItems :: MsgDirectionI d => User -> ChatDirection 'CTGroup d -> (GroupFeature -> GroupPreference -> Maybe Int -> Maybe GroupMemberRole -> CIContent d) -> GroupInfo -> CM ()
createGroupFeatureItems user cd ciContent GroupInfo {fullGroupPreferences} =
  forM_ allGroupFeatures $ \(AGF f) -> do
    let p = getGroupPreference f fullGroupPreferences
        (_, param, role) = groupFeatureState p
    createInternalChatItem user cd (ciContent (toGroupFeature f) (toGroupPreference p) param role) Nothing

createInternalChatItem :: (ChatTypeI c, MsgDirectionI d) => User -> ChatDirection c d -> CIContent d -> Maybe UTCTime -> CM ()
createInternalChatItem user cd content itemTs_ =
  lift (createInternalItemsForChats user itemTs_ [(cd, [content])]) >>= \case
    [Right aci] -> toView $ CRNewChatItems user [aci]
    [Left e] -> throwError e
    rs -> throwChatError $ CEInternalError $ "createInternalChatItem: expected 1 result, got " <> show (length rs)

createInternalItemsForChats ::
  forall c d.
  (ChatTypeI c, MsgDirectionI d) =>
  User ->
  Maybe UTCTime ->
  [(ChatDirection c d, [CIContent d])] ->
  CM' [Either ChatError AChatItem]
createInternalItemsForChats user itemTs_ dirsCIContents = do
  createdAt <- liftIO getCurrentTime
  let itemTs = fromMaybe createdAt itemTs_
  void . withStoreBatch' $ \db -> map (uncurry $ updateChat db createdAt) dirsCIContents
  withStoreBatch' $ \db -> concatMap (uncurry $ createACIs db itemTs createdAt) dirsCIContents
  where
    updateChat :: DB.Connection -> UTCTime -> ChatDirection c d -> [CIContent d] -> IO ()
    updateChat db createdAt cd contents
      | any ciRequiresAttention contents || contactChatDeleted cd = updateChatTs db user cd createdAt
      | otherwise = pure ()
    createACIs :: DB.Connection -> UTCTime -> UTCTime -> ChatDirection c d -> [CIContent d] -> [IO AChatItem]
    createACIs db itemTs createdAt cd = map $ \content -> do
      ciId <- createNewChatItemNoMsg db user cd content itemTs createdAt
      let ci = mkChatItem cd ciId content Nothing Nothing Nothing Nothing Nothing False itemTs Nothing createdAt
      pure $ AChatItem (chatTypeI @c) (msgDirection @d) (toChatInfo cd) ci

createLocalChatItems ::
  User ->
  ChatDirection 'CTLocal 'MDSnd ->
  [(CIContent 'MDSnd, Maybe (CIFile 'MDSnd), Maybe CIForwardedFrom)] ->
  UTCTime ->
  CM [ChatItem 'CTLocal 'MDSnd]
createLocalChatItems user cd itemsData createdAt = do
  withStore' $ \db -> updateChatTs db user cd createdAt
  (errs, items) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (createItem db) itemsData)
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  pure items
  where
    createItem :: DB.Connection -> (CIContent 'MDSnd, Maybe (CIFile 'MDSnd), Maybe CIForwardedFrom) -> IO (ChatItem 'CTLocal 'MDSnd)
    createItem db (content, ciFile, itemForwarded) = do
      ciId <- createNewChatItem_ db user cd Nothing Nothing content (Nothing, Nothing, Nothing, Nothing, Nothing) itemForwarded Nothing False createdAt Nothing createdAt
      forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
      pure $ mkChatItem cd ciId content ciFile Nothing Nothing itemForwarded Nothing False createdAt Nothing createdAt

withUser' :: (User -> CM ChatResponse) -> CM ChatResponse
withUser' action =
  asks currentUser
    >>= readTVarIO
    >>= maybe (throwChatError CENoActiveUser) run
  where
    run u = action u `catchChatError` (pure . CRChatCmdError (Just u))

withUser :: (User -> CM ChatResponse) -> CM ChatResponse
withUser action = withUser' $ \user ->
  ifM (lift chatStarted) (action user) (throwChatError CEChatNotStarted)

withUser_ :: CM ChatResponse -> CM ChatResponse
withUser_ = withUser . const

withUserId' :: UserId -> (User -> CM ChatResponse) -> CM ChatResponse
withUserId' userId action = withUser' $ \user -> do
  checkSameUser userId user
  action user

withUserId :: UserId -> (User -> CM ChatResponse) -> CM ChatResponse
withUserId userId action = withUser $ \user -> do
  checkSameUser userId user
  action user

checkSameUser :: UserId -> User -> CM ()
checkSameUser userId User {userId = activeUserId} = when (userId /= activeUserId) $ throwChatError (CEDifferentActiveUser userId activeUserId)

chatStarted :: CM' Bool
chatStarted = fmap isJust . readTVarIO =<< asks agentAsync

waitChatStartedAndActivated :: CM' ()
waitChatStartedAndActivated = do
  agentStarted <- asks agentAsync
  chatActivated <- asks chatActivated
  atomically $ do
    started <- readTVar agentStarted
    activated <- readTVar chatActivated
    unless (isJust started && activated) retry

chatVersionRange :: CM VersionRangeChat
chatVersionRange = lift chatVersionRange'
{-# INLINE chatVersionRange #-}

chatVersionRange' :: CM' VersionRangeChat
chatVersionRange' = do
  ChatConfig {chatVRange} <- asks config
  pure chatVRange
{-# INLINE chatVersionRange' #-}

chatCommandP :: Parser ChatCommand
chatCommandP =
  choice
    [ "/mute " *> ((`SetShowMessages` MFNone) <$> chatNameP),
      "/unmute " *> ((`SetShowMessages` MFAll) <$> chatNameP),
      "/unmute mentions " *> ((`SetShowMessages` MFMentions) <$> chatNameP),
      "/receipts " *> (SetSendReceipts <$> chatNameP <* " " <*> ((Just <$> onOffP) <|> ("default" $> Nothing))),
      "/block #" *> (SetShowMemberMessages <$> displayName <* A.space <*> (char_ '@' *> displayName) <*> pure False),
      "/unblock #" *> (SetShowMemberMessages <$> displayName <* A.space <*> (char_ '@' *> displayName) <*> pure True),
      "/_create user " *> (CreateActiveUser <$> jsonP),
      "/create user " *> (CreateActiveUser <$> newUserP),
      "/users" $> ListUsers,
      "/_user " *> (APISetActiveUser <$> A.decimal <*> optional (A.space *> jsonP)),
      ("/user " <|> "/u ") *> (SetActiveUser <$> displayName <*> optional (A.space *> pwdP)),
      "/set receipts all " *> (SetAllContactReceipts <$> onOffP),
      "/_set receipts contacts " *> (APISetUserContactReceipts <$> A.decimal <* A.space <*> receiptSettings),
      "/set receipts contacts " *> (SetUserContactReceipts <$> receiptSettings),
      "/_set receipts groups " *> (APISetUserGroupReceipts <$> A.decimal <* A.space <*> receiptSettings),
      "/set receipts groups " *> (SetUserGroupReceipts <$> receiptSettings),
      "/_hide user " *> (APIHideUser <$> A.decimal <* A.space <*> jsonP),
      "/_unhide user " *> (APIUnhideUser <$> A.decimal <* A.space <*> jsonP),
      "/_mute user " *> (APIMuteUser <$> A.decimal),
      "/_unmute user " *> (APIUnmuteUser <$> A.decimal),
      "/hide user " *> (HideUser <$> pwdP),
      "/unhide user " *> (UnhideUser <$> pwdP),
      "/mute user" $> MuteUser,
      "/unmute user" $> UnmuteUser,
      "/_delete user " *> (APIDeleteUser <$> A.decimal <* " del_smp=" <*> onOffP <*> optional (A.space *> jsonP)),
      "/delete user " *> (DeleteUser <$> displayName <*> pure True <*> optional (A.space *> pwdP)),
      ("/user" <|> "/u") $> ShowActiveUser,
      "/_start " *> do
        mainApp <- "main=" *> onOffP
        enableSndFiles <- " snd_files=" *> onOffP <|> pure mainApp
        pure StartChat {mainApp, enableSndFiles},
      "/_start" $> StartChat True True,
      "/_check running" $> CheckChatRunning,
      "/_stop" $> APIStopChat,
      "/_app activate restore=" *> (APIActivateChat <$> onOffP),
      "/_app activate" $> APIActivateChat True,
      "/_app suspend " *> (APISuspendChat <$> A.decimal),
      "/_resubscribe all" $> ResubscribeAllConnections,
      -- deprecated, use /set file paths
      "/_temp_folder " *> (SetTempFolder <$> filePath),
      -- /_files_folder deprecated, use /set file paths
      ("/_files_folder " <|> "/files_folder ") *> (SetFilesFolder <$> filePath),
      -- deprecated, use /set file paths
      "/remote_hosts_folder " *> (SetRemoteHostsFolder <$> filePath),
      "/set file paths " *> (APISetAppFilePaths <$> jsonP),
      "/_files_encrypt " *> (APISetEncryptLocalFiles <$> onOffP),
      "/contact_merge " *> (SetContactMergeEnabled <$> onOffP),
      "/_db export " *> (APIExportArchive <$> jsonP),
      "/db export" $> ExportArchive,
      "/_db import " *> (APIImportArchive <$> jsonP),
      "/_db delete" $> APIDeleteStorage,
      "/_db encryption " *> (APIStorageEncryption <$> jsonP),
      "/db encrypt " *> (APIStorageEncryption . dbEncryptionConfig "" <$> dbKeyP),
      "/db key " *> (APIStorageEncryption <$> (dbEncryptionConfig <$> dbKeyP <* A.space <*> dbKeyP)),
      "/db decrypt " *> (APIStorageEncryption . (`dbEncryptionConfig` "") <$> dbKeyP),
      "/db test key " *> (TestStorageEncryption <$> dbKeyP),
      "/_save app settings" *> (APISaveAppSettings <$> jsonP),
      "/_get app settings" *> (APIGetAppSettings <$> optional (A.space *> jsonP)),
      "/sql chat " *> (ExecChatStoreSQL <$> textP),
      "/sql agent " *> (ExecAgentStoreSQL <$> textP),
      "/sql slow" $> SlowSQLQueries,
      "/_get chats "
        *> ( APIGetChats
              <$> A.decimal
              <*> (" pcc=on" $> True <|> " pcc=off" $> False <|> pure False)
              <*> (A.space *> paginationByTimeP <|> pure (PTLast 5000))
              <*> (A.space *> jsonP <|> pure clqNoFilters)
           ),
      "/_get chat " *> (APIGetChat <$> chatRefP <* A.space <*> chatPaginationP <*> optional (" search=" *> stringP)),
      "/_get items " *> (APIGetChatItems <$> chatPaginationP <*> optional (" search=" *> stringP)),
      "/_get item info " *> (APIGetChatItemInfo <$> chatRefP <* A.space <*> A.decimal),
      "/_send " *> (APISendMessages <$> chatRefP <*> liveMessageP <*> sendMessageTTLP <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP)),
      "/_create *" *> (APICreateChatItems <$> A.decimal <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP)),
      "/_report #" *> (APIReportMessage <$> A.decimal <* A.space <*> A.decimal <*> (" reason=" *> strP) <*> (A.space *> textP <|> pure "")),
      "/report #" *> (ReportMessage <$> displayName <*> optional (" @" *> displayName) <*> _strP <* A.space <*> msgTextP),
      "/_update item " *> (APIUpdateChatItem <$> chatRefP <* A.space <*> A.decimal <*> liveMessageP <* A.space <*> msgContentP),
      "/_delete item " *> (APIDeleteChatItem <$> chatRefP <*> _strP <*> _strP),
      "/_delete member item #" *> (APIDeleteMemberChatItem <$> A.decimal <*> _strP),
      "/_reaction " *> (APIChatItemReaction <$> chatRefP <* A.space <*> A.decimal <* A.space <*> onOffP <* A.space <*> (knownReaction <$?> jsonP)),
      "/_reaction members " *> (APIGetReactionMembers <$> A.decimal <* " #" <*> A.decimal <* A.space <*> A.decimal <* A.space <*> (knownReaction <$?> jsonP)),
      "/_forward plan " *> (APIPlanForwardChatItems <$> chatRefP <*> _strP),
      "/_forward " *> (APIForwardChatItems <$> chatRefP <* A.space <*> chatRefP <*> _strP <*> sendMessageTTLP),
      "/_read user " *> (APIUserRead <$> A.decimal),
      "/read user" $> UserRead,
      "/_read chat " *> (APIChatRead <$> chatRefP),
      "/_read chat items " *> (APIChatItemsRead <$> chatRefP <*> _strP),
      "/_unread chat " *> (APIChatUnread <$> chatRefP <* A.space <*> onOffP),
      "/_delete " *> (APIDeleteChat <$> chatRefP <*> chatDeleteMode),
      "/_clear chat " *> (APIClearChat <$> chatRefP),
      "/_accept" *> (APIAcceptContact <$> incognitoOnOffP <* A.space <*> A.decimal),
      "/_reject " *> (APIRejectContact <$> A.decimal),
      "/_call invite @" *> (APISendCallInvitation <$> A.decimal <* A.space <*> jsonP),
      "/call " *> char_ '@' *> (SendCallInvitation <$> displayName <*> pure defaultCallType),
      "/_call reject @" *> (APIRejectCall <$> A.decimal),
      "/_call offer @" *> (APISendCallOffer <$> A.decimal <* A.space <*> jsonP),
      "/_call answer @" *> (APISendCallAnswer <$> A.decimal <* A.space <*> jsonP),
      "/_call extra @" *> (APISendCallExtraInfo <$> A.decimal <* A.space <*> jsonP),
      "/_call end @" *> (APIEndCall <$> A.decimal),
      "/_call status @" *> (APICallStatus <$> A.decimal <* A.space <*> strP),
      "/_call get" $> APIGetCallInvitations,
      "/_network_statuses" $> APIGetNetworkStatuses,
      "/_profile " *> (APIUpdateProfile <$> A.decimal <* A.space <*> jsonP),
      "/_set alias @" *> (APISetContactAlias <$> A.decimal <*> (A.space *> textP <|> pure "")),
      "/_set alias :" *> (APISetConnectionAlias <$> A.decimal <*> (A.space *> textP <|> pure "")),
      "/_set prefs @" *> (APISetContactPrefs <$> A.decimal <* A.space <*> jsonP),
      "/_set theme user " *> (APISetUserUIThemes <$> A.decimal <*> optional (A.space *> jsonP)),
      "/_set theme " *> (APISetChatUIThemes <$> chatRefP <*> optional (A.space *> jsonP)),
      "/_parse " *> (APIParseMarkdown . safeDecodeUtf8 <$> A.takeByteString),
      "/_ntf get" $> APIGetNtfToken,
      "/_ntf register " *> (APIRegisterToken <$> strP_ <*> strP),
      "/_ntf verify " *> (APIVerifyToken <$> strP <* A.space <*> strP <* A.space <*> strP),
      "/_ntf delete " *> (APIDeleteToken <$> strP),
      "/_ntf conns " *> (APIGetNtfConns <$> strP <* A.space <*> strP),
      "/_ntf conn messages " *> (ApiGetConnNtfMessages <$> strP),
      "/_add #" *> (APIAddMember <$> A.decimal <* A.space <*> A.decimal <*> memberRole),
      "/_join #" *> (APIJoinGroup <$> A.decimal),
      "/_member role #" *> (APIMemberRole <$> A.decimal <* A.space <*> A.decimal <*> memberRole),
      "/_block #" *> (APIBlockMemberForAll <$> A.decimal <* A.space <*> A.decimal <* A.space <* "blocked=" <*> onOffP),
      "/_remove #" *> (APIRemoveMember <$> A.decimal <* A.space <*> A.decimal),
      "/_leave #" *> (APILeaveGroup <$> A.decimal),
      "/_members #" *> (APIListMembers <$> A.decimal),
      "/_server test " *> (APITestProtoServer <$> A.decimal <* A.space <*> strP),
      "/smp test " *> (TestProtoServer . AProtoServerWithAuth SPSMP <$> strP),
      "/xftp test " *> (TestProtoServer . AProtoServerWithAuth SPXFTP <$> strP),
      "/ntf test " *> (TestProtoServer . AProtoServerWithAuth SPNTF <$> strP),
      "/smp " *> (SetUserProtoServers (AProtocolType SPSMP) . map (AProtoServerWithAuth SPSMP) <$> protocolServersP),
      "/xftp " *> (SetUserProtoServers (AProtocolType SPXFTP) . map (AProtoServerWithAuth SPXFTP) <$> protocolServersP),
      "/smp" $> GetUserProtoServers (AProtocolType SPSMP),
      "/xftp" $> GetUserProtoServers (AProtocolType SPXFTP),
      "/_operators" $> APIGetServerOperators,
      "/_operators " *> (APISetServerOperators <$> jsonP),
      "/operators " *> (SetServerOperators . L.fromList <$> operatorRolesP `A.sepBy1` A.char ','),
      "/_servers " *> (APIGetUserServers <$> A.decimal),
      "/_servers " *> (APISetUserServers <$> A.decimal <* A.space <*> jsonP),
      "/_validate_servers " *> (APIValidateServers <$> A.decimal <* A.space <*> jsonP),
      "/_conditions" $> APIGetUsageConditions,
      "/_conditions_notified " *> (APISetConditionsNotified <$> A.decimal),
      "/_accept_conditions " *> (APIAcceptConditions <$> A.decimal <*> _strP),
      "/_ttl " *> (APISetChatItemTTL <$> A.decimal <* A.space <*> ciTTLDecimal),
      "/ttl " *> (SetChatItemTTL <$> ciTTL),
      "/_ttl " *> (APIGetChatItemTTL <$> A.decimal),
      "/ttl" $> GetChatItemTTL,
      "/_network info " *> (APISetNetworkInfo <$> jsonP),
      "/_network " *> (APISetNetworkConfig <$> jsonP),
      ("/network " <|> "/net ") *> (SetNetworkConfig <$> netCfgP),
      ("/network" <|> "/net") $> APIGetNetworkConfig,
      "/reconnect " *> (ReconnectServer <$> A.decimal <* A.space <*> strP),
      "/reconnect" $> ReconnectAllServers,
      "/_settings " *> (APISetChatSettings <$> chatRefP <* A.space <*> jsonP),
      "/_member settings #" *> (APISetMemberSettings <$> A.decimal <* A.space <*> A.decimal <* A.space <*> jsonP),
      "/_info #" *> (APIGroupMemberInfo <$> A.decimal <* A.space <*> A.decimal),
      "/_info #" *> (APIGroupInfo <$> A.decimal),
      "/_info @" *> (APIContactInfo <$> A.decimal),
      ("/info #" <|> "/i #") *> (GroupMemberInfo <$> displayName <* A.space <* char_ '@' <*> displayName),
      ("/info #" <|> "/i #") *> (ShowGroupInfo <$> displayName),
      ("/info " <|> "/i ") *> char_ '@' *> (ContactInfo <$> displayName),
      "/_queue info #" *> (APIGroupMemberQueueInfo <$> A.decimal <* A.space <*> A.decimal),
      "/_queue info @" *> (APIContactQueueInfo <$> A.decimal),
      ("/queue info #" <|> "/qi #") *> (GroupMemberQueueInfo <$> displayName <* A.space <* char_ '@' <*> displayName),
      ("/queue info " <|> "/qi ") *> char_ '@' *> (ContactQueueInfo <$> displayName),
      "/_switch #" *> (APISwitchGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/_switch @" *> (APISwitchContact <$> A.decimal),
      "/_abort switch #" *> (APIAbortSwitchGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/_abort switch @" *> (APIAbortSwitchContact <$> A.decimal),
      "/_sync #" *> (APISyncGroupMemberRatchet <$> A.decimal <* A.space <*> A.decimal <*> (" force=on" $> True <|> pure False)),
      "/_sync @" *> (APISyncContactRatchet <$> A.decimal <*> (" force=on" $> True <|> pure False)),
      "/switch #" *> (SwitchGroupMember <$> displayName <* A.space <* char_ '@' <*> displayName),
      "/switch " *> char_ '@' *> (SwitchContact <$> displayName),
      "/abort switch #" *> (AbortSwitchGroupMember <$> displayName <* A.space <* char_ '@' <*> displayName),
      "/abort switch " *> char_ '@' *> (AbortSwitchContact <$> displayName),
      "/sync #" *> (SyncGroupMemberRatchet <$> displayName <* A.space <* char_ '@' <*> displayName <*> (" force=on" $> True <|> pure False)),
      "/sync " *> char_ '@' *> (SyncContactRatchet <$> displayName <*> (" force=on" $> True <|> pure False)),
      "/_get code @" *> (APIGetContactCode <$> A.decimal),
      "/_get code #" *> (APIGetGroupMemberCode <$> A.decimal <* A.space <*> A.decimal),
      "/_verify code @" *> (APIVerifyContact <$> A.decimal <*> optional (A.space *> verifyCodeP)),
      "/_verify code #" *> (APIVerifyGroupMember <$> A.decimal <* A.space <*> A.decimal <*> optional (A.space *> verifyCodeP)),
      "/_enable @" *> (APIEnableContact <$> A.decimal),
      "/_enable #" *> (APIEnableGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/code " *> char_ '@' *> (GetContactCode <$> displayName),
      "/code #" *> (GetGroupMemberCode <$> displayName <* A.space <* char_ '@' <*> displayName),
      "/verify " *> char_ '@' *> (VerifyContact <$> displayName <*> optional (A.space *> verifyCodeP)),
      "/verify #" *> (VerifyGroupMember <$> displayName <* A.space <* char_ '@' <*> displayName <*> optional (A.space *> verifyCodeP)),
      "/enable " *> char_ '@' *> (EnableContact <$> displayName),
      "/enable #" *> (EnableGroupMember <$> displayName <* A.space <* char_ '@' <*> displayName),
      ("/help files" <|> "/help file" <|> "/hf") $> ChatHelp HSFiles,
      ("/help groups" <|> "/help group" <|> "/hg") $> ChatHelp HSGroups,
      ("/help contacts" <|> "/help contact" <|> "/hc") $> ChatHelp HSContacts,
      ("/help address" <|> "/ha") $> ChatHelp HSMyAddress,
      ("/help incognito" <|> "/hi") $> ChatHelp HSIncognito,
      ("/help messages" <|> "/hm") $> ChatHelp HSMessages,
      ("/help remote" <|> "/hr") $> ChatHelp HSRemote,
      ("/help settings" <|> "/hs") $> ChatHelp HSSettings,
      ("/help db" <|> "/hd") $> ChatHelp HSDatabase,
      ("/help" <|> "/h") $> ChatHelp HSMain,
      ("/group" <|> "/g") *> (NewGroup <$> incognitoP <* A.space <* char_ '#' <*> groupProfile),
      "/_group " *> (APINewGroup <$> A.decimal <*> incognitoOnOffP <* A.space <*> jsonP),
      ("/add " <|> "/a ") *> char_ '#' *> (AddMember <$> displayName <* A.space <* char_ '@' <*> displayName <*> (memberRole <|> pure GRMember)),
      ("/join " <|> "/j ") *> char_ '#' *> (JoinGroup <$> displayName),
      ("/member role " <|> "/mr ") *> char_ '#' *> (MemberRole <$> displayName <* A.space <* char_ '@' <*> displayName <*> memberRole),
      "/block for all #" *> (BlockForAll <$> displayName <* A.space <*> (char_ '@' *> displayName) <*> pure True),
      "/unblock for all #" *> (BlockForAll <$> displayName <* A.space <*> (char_ '@' *> displayName) <*> pure False),
      ("/remove " <|> "/rm ") *> char_ '#' *> (RemoveMember <$> displayName <* A.space <* char_ '@' <*> displayName),
      ("/leave " <|> "/l ") *> char_ '#' *> (LeaveGroup <$> displayName),
      ("/delete #" <|> "/d #") *> (DeleteGroup <$> displayName),
      ("/delete " <|> "/d ") *> char_ '@' *> (DeleteContact <$> displayName <*> chatDeleteMode),
      "/clear *" $> ClearNoteFolder,
      "/clear #" *> (ClearGroup <$> displayName),
      "/clear " *> char_ '@' *> (ClearContact <$> displayName),
      ("/members " <|> "/ms ") *> char_ '#' *> (ListMembers <$> displayName),
      "/_groups" *> (APIListGroups <$> A.decimal <*> optional (" @" *> A.decimal) <*> optional (A.space *> stringP)),
      ("/groups" <|> "/gs") *> (ListGroups <$> optional (" @" *> displayName) <*> optional (A.space *> stringP)),
      "/_group_profile #" *> (APIUpdateGroupProfile <$> A.decimal <* A.space <*> jsonP),
      ("/group_profile " <|> "/gp ") *> char_ '#' *> (UpdateGroupNames <$> displayName <* A.space <*> groupProfile),
      ("/group_profile " <|> "/gp ") *> char_ '#' *> (ShowGroupProfile <$> displayName),
      "/group_descr " *> char_ '#' *> (UpdateGroupDescription <$> displayName <*> optional (A.space *> msgTextP)),
      "/set welcome " *> char_ '#' *> (UpdateGroupDescription <$> displayName <* A.space <*> (Just <$> msgTextP)),
      "/delete welcome " *> char_ '#' *> (UpdateGroupDescription <$> displayName <*> pure Nothing),
      "/show welcome " *> char_ '#' *> (ShowGroupDescription <$> displayName),
      "/_create link #" *> (APICreateGroupLink <$> A.decimal <*> (memberRole <|> pure GRMember)),
      "/_set link role #" *> (APIGroupLinkMemberRole <$> A.decimal <*> memberRole),
      "/_delete link #" *> (APIDeleteGroupLink <$> A.decimal),
      "/_get link #" *> (APIGetGroupLink <$> A.decimal),
      "/create link #" *> (CreateGroupLink <$> displayName <*> (memberRole <|> pure GRMember)),
      "/set link role #" *> (GroupLinkMemberRole <$> displayName <*> memberRole),
      "/delete link #" *> (DeleteGroupLink <$> displayName),
      "/show link #" *> (ShowGroupLink <$> displayName),
      "/_create member contact #" *> (APICreateMemberContact <$> A.decimal <* A.space <*> A.decimal),
      "/_invite member contact @" *> (APISendMemberContactInvitation <$> A.decimal <*> optional (A.space *> msgContentP)),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayName <* A.space <*> pure Nothing <*> quotedMsg <*> msgTextP),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayName <* A.space <* char_ '@' <*> (Just <$> displayName) <* A.space <*> quotedMsg <*> msgTextP),
      "/_contacts " *> (APIListContacts <$> A.decimal),
      "/contacts" $> ListContacts,
      "/_connect plan " *> (APIConnectPlan <$> A.decimal <* A.space <*> strP),
      "/_connect " *> (APIConnect <$> A.decimal <*> incognitoOnOffP <* A.space <*> ((Just <$> strP) <|> A.takeByteString $> Nothing)),
      "/_connect " *> (APIAddContact <$> A.decimal <*> incognitoOnOffP),
      "/_set incognito :" *> (APISetConnectionIncognito <$> A.decimal <* A.space <*> onOffP),
      "/_set conn user :" *> (APIChangeConnectionUser <$> A.decimal <* A.space <*> A.decimal),
      ("/connect" <|> "/c") *> (Connect <$> incognitoP <* A.space <*> ((Just <$> strP) <|> A.takeTill isSpace $> Nothing)),
      ("/connect" <|> "/c") *> (AddContact <$> incognitoP),
      ForwardMessage <$> chatNameP <* " <- @" <*> displayName <* A.space <*> msgTextP,
      ForwardGroupMessage <$> chatNameP <* " <- #" <*> displayName <* A.space <* A.char '@' <*> (Just <$> displayName) <* A.space <*> msgTextP,
      ForwardGroupMessage <$> chatNameP <* " <- #" <*> displayName <*> pure Nothing <* A.space <*> msgTextP,
      ForwardLocalMessage <$> chatNameP <* " <- * " <*> msgTextP,
      SendMessage <$> chatNameP <* A.space <*> msgTextP,
      "/* " *> (SendMessage (ChatName CTLocal "") <$> msgTextP),
      "@#" *> (SendMemberContactMessage <$> displayName <* A.space <* char_ '@' <*> displayName <* A.space <*> msgTextP),
      "/live " *> (SendLiveMessage <$> chatNameP <*> (A.space *> msgTextP <|> pure "")),
      (">@" <|> "> @") *> sendMsgQuote (AMsgDirection SMDRcv),
      (">>@" <|> ">> @") *> sendMsgQuote (AMsgDirection SMDSnd),
      ("\\ " <|> "\\") *> (DeleteMessage <$> chatNameP <* A.space <*> textP),
      ("\\\\ #" <|> "\\\\#") *> (DeleteMemberMessage <$> displayName <* A.space <* char_ '@' <*> displayName <* A.space <*> textP),
      ("! " <|> "!") *> (EditMessage <$> chatNameP <* A.space <*> (quotedMsg <|> pure "") <*> msgTextP),
      ReactToMessage <$> (("+" $> True) <|> ("-" $> False)) <*> reactionP <* A.space <*> chatNameP' <* A.space <*> textP,
      "/feed " *> (SendMessageBroadcast <$> msgTextP),
      ("/chats" <|> "/cs") *> (LastChats <$> (" all" $> Nothing <|> Just <$> (A.space *> A.decimal <|> pure 20))),
      ("/tail" <|> "/t") *> (LastMessages <$> optional (A.space *> chatNameP) <*> msgCountP <*> pure Nothing),
      ("/search" <|> "/?") *> (LastMessages <$> optional (A.space *> chatNameP) <*> msgCountP <*> (Just <$> (A.space *> stringP))),
      "/last_item_id" *> (LastChatItemId <$> optional (A.space *> chatNameP) <*> (A.space *> A.decimal <|> pure 0)),
      "/show" *> (ShowLiveItems <$> (A.space *> onOffP <|> pure True)),
      "/show " *> (ShowChatItem . Just <$> A.decimal),
      "/item info " *> (ShowChatItemInfo <$> chatNameP <* A.space <*> msgTextP),
      ("/file " <|> "/f ") *> (SendFile <$> chatNameP' <* A.space <*> cryptoFileP),
      ("/image " <|> "/img ") *> (SendImage <$> chatNameP' <* A.space <*> cryptoFileP),
      ("/fforward " <|> "/ff ") *> (ForwardFile <$> chatNameP' <* A.space <*> A.decimal),
      ("/image_forward " <|> "/imgf ") *> (ForwardImage <$> chatNameP' <* A.space <*> A.decimal),
      ("/fdescription " <|> "/fd") *> (SendFileDescription <$> chatNameP' <* A.space <*> filePath),
      ("/freceive " <|> "/fr ") *> (ReceiveFile <$> A.decimal <*> (" approved_relays=" *> onOffP <|> pure False) <*> optional (" encrypt=" *> onOffP) <*> optional (" inline=" *> onOffP) <*> optional (A.space *> filePath)),
      "/_set_file_to_receive " *> (SetFileToReceive <$> A.decimal <*> (" approved_relays=" *> onOffP <|> pure False) <*> optional (" encrypt=" *> onOffP)),
      ("/fcancel " <|> "/fc ") *> (CancelFile <$> A.decimal),
      ("/fstatus " <|> "/fs ") *> (FileStatus <$> A.decimal),
      "/_connect contact " *> (APIConnectContactViaAddress <$> A.decimal <*> incognitoOnOffP <* A.space <*> A.decimal),
      "/simplex" *> (ConnectSimplex <$> incognitoP),
      "/_address " *> (APICreateMyAddress <$> A.decimal),
      ("/address" <|> "/ad") $> CreateMyAddress,
      "/_delete_address " *> (APIDeleteMyAddress <$> A.decimal),
      ("/delete_address" <|> "/da") $> DeleteMyAddress,
      "/_show_address " *> (APIShowMyAddress <$> A.decimal),
      ("/show_address" <|> "/sa") $> ShowMyAddress,
      "/_profile_address " *> (APISetProfileAddress <$> A.decimal <* A.space <*> onOffP),
      ("/profile_address " <|> "/pa ") *> (SetProfileAddress <$> onOffP),
      "/_auto_accept " *> (APIAddressAutoAccept <$> A.decimal <* A.space <*> autoAcceptP),
      "/auto_accept " *> (AddressAutoAccept <$> autoAcceptP),
      ("/accept" <|> "/ac") *> (AcceptContact <$> incognitoP <* A.space <* char_ '@' <*> displayName),
      ("/reject " <|> "/rc ") *> char_ '@' *> (RejectContact <$> displayName),
      ("/markdown" <|> "/m") $> ChatHelp HSMarkdown,
      ("/welcome" <|> "/w") $> Welcome,
      "/set profile image " *> (UpdateProfileImage . Just . ImageData <$> imageP),
      "/delete profile image" $> UpdateProfileImage Nothing,
      "/show profile image" $> ShowProfileImage,
      ("/profile " <|> "/p ") *> (uncurry UpdateProfile <$> profileNames),
      ("/profile" <|> "/p") $> ShowProfile,
      "/set voice #" *> (SetGroupFeatureRole (AGFR SGFVoice) <$> displayName <*> _strP <*> optional memberRole),
      "/set voice @" *> (SetContactFeature (ACF SCFVoice) <$> displayName <*> optional (A.space *> strP)),
      "/set voice " *> (SetUserFeature (ACF SCFVoice) <$> strP),
      "/set files #" *> (SetGroupFeatureRole (AGFR SGFFiles) <$> displayName <*> _strP <*> optional memberRole),
      "/set history #" *> (SetGroupFeature (AGFNR SGFHistory) <$> displayName <*> (A.space *> strP)),
      "/set reactions #" *> (SetGroupFeature (AGFNR SGFReactions) <$> displayName <*> (A.space *> strP)),
      "/set calls @" *> (SetContactFeature (ACF SCFCalls) <$> displayName <*> optional (A.space *> strP)),
      "/set calls " *> (SetUserFeature (ACF SCFCalls) <$> strP),
      "/set delete #" *> (SetGroupFeature (AGFNR SGFFullDelete) <$> displayName <*> (A.space *> strP)),
      "/set delete @" *> (SetContactFeature (ACF SCFFullDelete) <$> displayName <*> optional (A.space *> strP)),
      "/set delete " *> (SetUserFeature (ACF SCFFullDelete) <$> strP),
      "/set direct #" *> (SetGroupFeatureRole (AGFR SGFDirectMessages) <$> displayName <*> _strP <*> optional memberRole),
      "/set disappear #" *> (SetGroupTimedMessages <$> displayName <*> (A.space *> timedTTLOnOffP)),
      "/set disappear @" *> (SetContactTimedMessages <$> displayName <*> optional (A.space *> timedMessagesEnabledP)),
      "/set disappear " *> (SetUserTimedMessages <$> (("yes" $> True) <|> ("no" $> False))),
      "/set links #" *> (SetGroupFeatureRole (AGFR SGFSimplexLinks) <$> displayName <*> _strP <*> optional memberRole),
      ("/incognito" <* optional (A.space *> onOffP)) $> ChatHelp HSIncognito,
      "/set device name " *> (SetLocalDeviceName <$> textP),
      "/list remote hosts" $> ListRemoteHosts,
      "/switch remote host " *> (SwitchRemoteHost <$> ("local" $> Nothing <|> (Just <$> A.decimal))),
      "/start remote host " *> (StartRemoteHost <$> ("new" $> Nothing <|> (Just <$> ((,) <$> A.decimal <*> (" multicast=" *> onOffP <|> pure False)))) <*> optional (A.space *> rcCtrlAddressP) <*> optional (" port=" *> A.decimal)),
      "/stop remote host " *> (StopRemoteHost <$> ("new" $> RHNew <|> RHId <$> A.decimal)),
      "/delete remote host " *> (DeleteRemoteHost <$> A.decimal),
      "/store remote file " *> (StoreRemoteFile <$> A.decimal <*> optional (" encrypt=" *> onOffP) <* A.space <*> filePath),
      "/get remote file " *> (GetRemoteFile <$> A.decimal <* A.space <*> jsonP),
      ("/connect remote ctrl " <|> "/crc ") *> (ConnectRemoteCtrl <$> strP),
      "/find remote ctrl" $> FindKnownRemoteCtrl,
      "/confirm remote ctrl " *> (ConfirmRemoteCtrl <$> A.decimal),
      "/verify remote ctrl " *> (VerifyRemoteCtrlSession <$> textP),
      "/list remote ctrls" $> ListRemoteCtrls,
      "/stop remote ctrl" $> StopRemoteCtrl,
      "/delete remote ctrl " *> (DeleteRemoteCtrl <$> A.decimal),
      "/_upload " *> (APIUploadStandaloneFile <$> A.decimal <* A.space <*> cryptoFileP),
      "/_download info " *> (APIStandaloneFileInfo <$> strP),
      "/_download " *> (APIDownloadStandaloneFile <$> A.decimal <* A.space <*> strP_ <*> cryptoFileP),
      ("/quit" <|> "/q" <|> "/exit") $> QuitChat,
      ("/version" <|> "/v") $> ShowVersion,
      "/debug locks" $> DebugLocks,
      "/debug event " *> (DebugEvent <$> jsonP),
      "/get subs total " *> (GetAgentSubsTotal <$> A.decimal),
      "/get servers summary " *> (GetAgentServersSummary <$> A.decimal),
      "/reset servers stats" $> ResetAgentServersStats,
      "/get subs" $> GetAgentSubs,
      "/get subs details" $> GetAgentSubsDetails,
      "/get workers" $> GetAgentWorkers,
      "/get workers details" $> GetAgentWorkersDetails,
      "/get queues" $> GetAgentQueuesInfo,
      "//" *> (CustomChatCommand <$> A.takeByteString)
    ]
  where
    choice = A.choice . map (\p -> p <* A.takeWhile (== ' ') <* A.endOfInput)
    incognitoP = (A.space *> ("incognito" <|> "i")) $> True <|> pure False
    incognitoOnOffP = (A.space *> "incognito=" *> onOffP) <|> pure False
    imagePrefix = (<>) <$> "data:" <*> ("image/png;base64," <|> "image/jpg;base64,")
    imageP = safeDecodeUtf8 <$> ((<>) <$> imagePrefix <*> (B64.encode <$> base64P))
    chatTypeP = A.char '@' $> CTDirect <|> A.char '#' $> CTGroup <|> A.char '*' $> CTLocal <|> A.char ':' $> CTContactConnection
    chatPaginationP =
      (CPLast <$ "count=" <*> A.decimal)
        <|> (CPAfter <$ "after=" <*> A.decimal <* A.space <* "count=" <*> A.decimal)
        <|> (CPBefore <$ "before=" <*> A.decimal <* A.space <* "count=" <*> A.decimal)
        <|> (CPAround <$ "around=" <*> A.decimal <* A.space <* "count=" <*> A.decimal)
        <|> (CPInitial <$ "initial=" <*> A.decimal)
    paginationByTimeP =
      (PTLast <$ "count=" <*> A.decimal)
        <|> (PTAfter <$ "after=" <*> strP <* A.space <* "count=" <*> A.decimal)
        <|> (PTBefore <$ "before=" <*> strP <* A.space <* "count=" <*> A.decimal)
    mcTextP = MCText . safeDecodeUtf8 <$> A.takeByteString
    msgContentP = "text " *> mcTextP <|> "json " *> jsonP
    chatDeleteMode =
      A.choice
        [ " full" *> (CDMFull <$> notifyP),
          " entity" *> (CDMEntity <$> notifyP),
          " messages" $> CDMMessages,
          CDMFull <$> notifyP -- backwards compatible
        ]
      where
        notifyP = " notify=" *> onOffP <|> pure True
    displayName = safeDecodeUtf8 <$> (quoted "'" <|> takeNameTill isSpace)
      where
        takeNameTill p =
          A.peekChar' >>= \c ->
            if refChar c then A.takeTill p else fail "invalid first character in display name"
        quoted cs = A.choice [A.char c *> takeNameTill (== c) <* A.char c | c <- cs]
        refChar c = c > ' ' && c /= '#' && c /= '@'
    sendMsgQuote msgDir = SendMessageQuote <$> displayName <* A.space <*> pure msgDir <*> quotedMsg <*> msgTextP
    quotedMsg = safeDecodeUtf8 <$> (A.char '(' *> A.takeTill (== ')') <* A.char ')') <* optional A.space
    reactionP = MREmoji <$> (mrEmojiChar <$?> (toEmoji <$> A.anyChar))
    toEmoji = \case
      '1' -> '👍'
      '+' -> '👍'
      '-' -> '👎'
      ')' -> '😀'
      ',' -> '😢'
      '*' -> head "❤️"
      '^' -> '🚀'
      c -> c
    composedMessagesTextP = do
      text <- mcTextP
      pure $ (ComposedMessage Nothing Nothing text) :| []
    liveMessageP = " live=" *> onOffP <|> pure False
    sendMessageTTLP = " ttl=" *> ((Just <$> A.decimal) <|> ("default" $> Nothing)) <|> pure Nothing
    receiptSettings = do
      enable <- onOffP
      clearOverrides <- (" clear_overrides=" *> onOffP) <|> pure False
      pure UserMsgReceiptSettings {enable, clearOverrides}
    onOffP = ("on" $> True) <|> ("off" $> False)
    profileNames = (,) <$> displayName <*> fullNameP
    newUserP = do
      (cName, fullName) <- profileNames
      let profile = Just Profile {displayName = cName, fullName, image = Nothing, contactLink = Nothing, preferences = Nothing}
      pure NewUser {profile, pastTimestamp = False}
    jsonP :: J.FromJSON a => Parser a
    jsonP = J.eitherDecodeStrict' <$?> A.takeByteString
    groupProfile = do
      (gName, fullName) <- profileNames
      let groupPreferences =
            Just
              (emptyGroupPrefs :: GroupPreferences)
                { directMessages = Just DirectMessagesGroupPreference {enable = FEOn, role = Nothing},
                  history = Just HistoryGroupPreference {enable = FEOn}
                }
      pure GroupProfile {displayName = gName, fullName, description = Nothing, image = Nothing, groupPreferences}
    fullNameP = A.space *> textP <|> pure ""
    textP = safeDecodeUtf8 <$> A.takeByteString
    pwdP = jsonP <|> (UserPwd . safeDecodeUtf8 <$> A.takeTill (== ' '))
    verifyCodeP = safeDecodeUtf8 <$> A.takeWhile (\c -> isDigit c || c == ' ')
    msgTextP = jsonP <|> textP
    stringP = T.unpack . safeDecodeUtf8 <$> A.takeByteString
    filePath = stringP
    cryptoFileP = do
      cfArgs <- optional $ CFArgs <$> (" key=" *> strP <* A.space) <*> (" nonce=" *> strP)
      path <- filePath
      pure $ CryptoFile path cfArgs
    memberRole =
      A.choice
        [ " owner" $> GROwner,
          " admin" $> GRAdmin,
          " moderator" $> GRModerator,
          " member" $> GRMember,
          " observer" $> GRObserver
        ]
    chatNameP =
      chatTypeP >>= \case
        CTLocal -> pure $ ChatName CTLocal ""
        ct -> ChatName ct <$> displayName
    chatNameP' = ChatName <$> (chatTypeP <|> pure CTDirect) <*> displayName
    chatRefP = ChatRef <$> chatTypeP <*> A.decimal
    msgCountP = A.space *> A.decimal <|> pure 10
    ciTTLDecimal = ("none" $> Nothing) <|> (Just <$> A.decimal)
    ciTTL =
      ("day" $> Just 86400)
        <|> ("week" $> Just (7 * 86400))
        <|> ("month" $> Just (30 * 86400))
        <|> ("none" $> Nothing)
    timedTTLP =
      ("30s" $> 30)
        <|> ("5min" $> 300)
        <|> ("1h" $> 3600)
        <|> ("8h" $> (8 * 3600))
        <|> ("day" $> 86400)
        <|> ("week" $> (7 * 86400))
        <|> ("month" $> (30 * 86400))
        <|> A.decimal
    timedTTLOnOffP =
      optional ("on" *> A.space) *> (Just <$> timedTTLP)
        <|> ("off" $> Nothing)
    timedMessagesEnabledP =
      optional ("yes" *> A.space) *> (TMEEnableSetTTL <$> timedTTLP)
        <|> ("yes" $> TMEEnableKeepTTL)
        <|> ("no" $> TMEDisableKeepTTL)
    operatorRolesP = do
      operatorId' <- A.decimal
      enabled' <- A.char ':' *> onOffP
      smpRoles' <- (":smp=" *> srvRolesP) <|> pure allRoles
      xftpRoles' <- (":xftp=" *> srvRolesP) <|> pure allRoles
      pure ServerOperatorRoles {operatorId', enabled', smpRoles', xftpRoles'}
    srvRolesP = srvRoles <$?> A.takeTill (\c -> c == ':' || c == ',')
      where
        srvRoles = \case
          "off" -> Right $ ServerRoles False False
          "proxy" -> Right ServerRoles {storage = False, proxy = True}
          "storage" -> Right ServerRoles {storage = True, proxy = False}
          "on" -> Right allRoles
          _ -> Left "bad ServerRoles"
    netCfgP = do
      socksProxy <- "socks=" *> ("off" $> Nothing <|> "on" $> Just defaultSocksProxyWithAuth <|> Just <$> strP)
      socksMode <- " socks-mode=" *> strP <|> pure SMAlways
      hostMode <- " host-mode=" *> (textToHostMode . safeDecodeUtf8 <$?> A.takeTill (== ' ')) <|> pure (defaultHostMode socksProxy)
      requiredHostMode <- (" required-host-mode" $> True) <|> pure False
      smpProxyMode_ <- optional $ " smp-proxy=" *> strP
      smpProxyFallback_ <- optional $ " smp-proxy-fallback=" *> strP
      smpWebPort <- (" smp-web-port" $> True) <|> pure False
      t_ <- optional $ " timeout=" *> A.decimal
      logTLSErrors <- " log=" *> onOffP <|> pure False
      let tcpTimeout_ = (1000000 *) <$> t_
      pure $ SimpleNetCfg {socksProxy, socksMode, hostMode, requiredHostMode, smpProxyMode_, smpProxyFallback_, smpWebPort, tcpTimeout_, logTLSErrors}
    dbKeyP = nonEmptyKey <$?> strP
    nonEmptyKey k@(DBEncryptionKey s) = if BA.null s then Left "empty key" else Right k
    dbEncryptionConfig currentKey newKey = DBEncryptionConfig {currentKey, newKey, keepKey = Just False}
    autoAcceptP = ifM onOffP (Just <$> (businessAA <|> addressAA)) (pure Nothing)
      where
        addressAA = AutoAccept False <$> (" incognito=" *> onOffP <|> pure False) <*> autoReply
        businessAA = AutoAccept True <$> (" business" *> pure False) <*> autoReply
        autoReply = optional (A.space *> msgContentP)
    rcCtrlAddressP = RCCtrlAddress <$> ("addr=" *> strP) <*> (" iface=" *> (jsonP <|> text1P))
    text1P = safeDecodeUtf8 <$> A.takeTill (== ' ')
    char_ = optional . A.char

adminContactReq :: ConnReqContact
adminContactReq =
  either error id $ strDecode "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

simplexTeamContactProfile :: Profile
simplexTeamContactProfile =
  Profile
    { displayName = "SimpleX Chat team",
      fullName = "",
      image = Just (ImageData "data:image/jpg;base64,/9j/4AAQSkZJRgABAgAAAQABAAD/2wBDAAUDBAQEAwUEBAQFBQUGBwwIBwcHBw8KCwkMEQ8SEhEPERATFhwXExQaFRARGCEYGhwdHx8fExciJCIeJBweHx7/2wBDAQUFBQcGBw4ICA4eFBEUHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh7/wAARCAETARMDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD7LooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiivP/iF4yFvv0rSpAZek0yn7v+yPeunC4WpiqihBf8A8rOc5w2UYZ4jEPTourfZDvH3jL7MW03SpR53SWUfw+w96veA/F0erRLY3zKl6owD2k/8Ar15EWLEljknqadDK8MqyxMUdTlWB5Br66WS0Hh/ZLfv1ufiNLj7Mo5m8ZJ3g9OTpy+Xn5/pofRdFcd4B8XR6tEthfMEvVHyk9JB/jXY18fiMPUw9R06i1P3PK80w2aYaOIw8rxf3p9n5hRRRWB6AUUVDe3UFlavc3MixxIMsxppNuyJnOMIuUnZIL26gsrV7m5kWOJBlmNeU+I/Gd9e6sk1hI8FvA2Y1z973NVPGnimfXLoxRFo7JD8if3vc1zefevr8syiNKPtKyvJ9Ox+F8Ycb1cdU+rYCTjTi/iWjk1+nbue3eEPEdtrtoMER3SD95Hn9R7Vu18+6bf3On3kd1aSmOVDkEd/Y17J4P8SW2vWY6R3aD97F/Ue1eVmmVPDP2lP4fyPtODeMoZrBYXFO1Zf+Tf8AB7r5o3qKKK8Q/QgooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAqavbTXmmz20Fw1vJIhVZB1FeDa3p15pWoSWl6hWQHr2YeoNfQlY3izw9Z6/YGGZQky8xSgcqf8K9jKcyWEnyzXuv8D4njLhZ51RVSi7VYLRdGu3k+z+88HzRuq1rWmXmkX8lnexFHU8Hsw9RVLNfcxlGcVKLumfgFahUozdOorSWjT6E0M0kMqyxOyOpyrKcEGvXPAPjCPVolsb9wl6owGPAkH+NeO5p8M0kMqyxOyOpyrA4INcWPy+njKfLLfoz2+HuIMTkmI9pT1i/ij0a/wA+zPpGiuM+H/jCPV4lsL91S+QfKTwJR/jXW3t1BZWslzcyLHFGMsxNfB4jC1aFX2U1r+fof0Rl2bYXMMKsVRl7vXy7p9rBfXVvZWr3NzKscSDLMTXjnjbxVPrtyYoiY7JD8if3vc0zxv4ruNeujFEWjsoz8if3vc1zOa+synKFh0qtVe9+X/BPxvjLjKWZSeEwjtSW7/m/4H5kmaM1HmlB54r3bH51YkzXo3wz8MXMc0es3ZeED/VR5wW9z7VB8O/BpnMerarEREDuhhb+L3Pt7V6cAAAAAAOgFfL5xmqs6FH5v9D9a4H4MlzQzHGq1tYR/KT/AEXzCiiivlj9hCiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAxfFvh208QWBhmASdRmKUdVP+FeH63pl5pGoSWV5EUdTwezD1HtX0VWL4t8O2fiHTzBONk6g+TKByp/wr28pzZ4WXs6msH+B8NxdwhTzeDxGHVqy/8m8n59n954FmjNW9b0y80fUHsr2MpIp4PZh6iqWfevuYyjOKlF3TPwetQnRm6dRWktGmSwzSQyrLE7I6nKsDgg1teIPFOqa3a29vdy4jiUAheN7f3jWBmjNROhTnJTkrtbGtLF4ijSnRpzajPddHbuP3e9Lmo80ua0scth+a9E+HXgw3Hl6tqsZEX3oYmH3vc+1J8OPBZnKavq0eIhzDCw+9/tH29q9SAAAAGAOgr5bOM35b0KD16v8ARH6twXwXz8uPx0dN4xfXzf6IFAUAAAAdBRRRXyZ+wBRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFB4GTXyj+1p+0ONJjufA3ga6DX7qU1DUY24gB4McZH8Xqe38tqFCdefLETaSufQ3h/4geEde8Uah4a0rWra51Ow/wBfCrD8ceuO+OldRX5I+GfEWseG/ENvr2j30ttqFvJ5iSqxyT3z6g96/RH9nD41aT8U9AWGcx2fiK1QC7tC33/+mieqn07V14zL3QXNHVEQnc9dooorzjQKKKKACiis7xHrel+HdGudY1m8is7K2QvLLI2AAP600m3ZAYfxUg8Pr4VutT1+7isYbSMuLp/4Pb3z6V8++HNd0zxDpq6hpVys8DHGRwVPoR2NeIftJ/G7VPifrbWVk8lp4btZD9mtwcGU/wDPR/c9h2rgfh34z1LwdrAurV2ktZCBcW5PyyD/AB9DX2WTyqYWny1Ho+nY+C4t4Wp5tF16CtVX/k3k/Ps/vPr/ADRmsjwx4g07xFpMWpaZOJInHI/iQ9wR61qbq+mVmro/D6tCdGbp1FZrdEma6/4XafpWoa7jUpV3oA0MLdJD/ntXG5p8E0kMqyxOyOhyrKcEGsMTRlWpShGVm+p1ZbiYYPFQr1IKai72fU+nFAUAKAAOABRXEfDnxpFrMK6fqDhL9BhSeko9frXb1+a4rDVMNUdOotT+k8szLD5lh44jDu8X968n5hRRRXOegFFFFABUGoXlvYWkl1dSrHFGMliaL+7t7C0kuruVYoYxlmNeI+OvFtx4huzHFuisYz+7jz97/aNenluW1MbU00it2fM8S8SUMkoXetR/DH9X5fmeteF/E+m+IFkFoxSWMnMb9cev0rbr5t0vULrTb6K8s5TFNGcgj+R9q9w8E+KbXxDYjlY7xB+9i/qPaurNsneE/eUtYfkeTwlxjHNV9XxVo1V90vTz8vmjoqKKK8I+8CiiigAooooAKKKKACiiigD5V/a8+P0mgvdeAvCUskepFdl9eDjyQR9xPfHeviiR3lkaSR2d2OWZjkk+tfoj+058CtP+Jektq2jxRWnie2T91KMKLlR/yzf+h7V+fOuaVqGiarcaXqtpLaXls5jlikXDKRX0mWSpOlaG/U56l76lKtPwtr+reGNetdb0S8ls761cPHJG2D9D6g9MVmUV6TSasyD9Jf2cfjXpPxR0MW9w0dp4gtkAubYnHmf7aeo/lXr1fkh4W1/V/DGuW2taHey2d9bOHjkjP6H1HtX6Jfs5fGvR/inoQgmeOz8RWqD7XaE439vMT1U+navnMfgHRfPD4fyN4Tvoz12iis7xJremeHdEutZ1i7jtLK1jLyyucAAf1rzUm3ZGgeJNb0vw7otzrOs3kVpZWyF5ZZDgAD+Z9q/PL9pP436r8UNZaxs2ks/Dlq5+z24ODMf77+p9B2o/aU+N2p/FDXDZ2LS2fhy1ci3t84Mx/wCej+/oO1eNV9DgMAqS55/F+RhOd9EFFFABJwBkmvUMzqPh34y1Lwjq63FszSWshAntyeHHt719Z2EstzpVlqD2txbR3kCzxLPGUbawyODXK/slfs8nUpbXx144tGFkhElhp8q4849pHB/h9B3r608X+GLDxBpX2WRFiljX9xIowUPYfT2rGnnkMPWVJ6x6vt/XU+P4o4SjmtN4igrVV/5N5Pz7P7z56zRmrmvaVe6LqMljexMkiHg9mHqKoZr6uEozipRd0z8Rq0J0ZunUVmtGmTwTSQTJNC7JIhyrKcEGvZvhz41j1mJdP1GRUv0GFY8CX/69eJZqSCaWCVZYXZHU5VlOCDXDmGXU8bT5ZaPo+x7WQZ9iMlxHtKesX8UejX+fZn1FRXDfDbxtHrUKadqDqmoIuAx4EoHf613NfnWKwtTC1HTqKzR/QGW5lh8yw8cRh3eL+9Ps/MKr6heW1hZyXd3KsUUYyzGjUby20+zku7yZYoY13MzGvDPHvi+48RXpjiZorCM/u4/73+0feuvLMsqY6pZaRW7/AK6nlcScR0MloXetR/DH9X5D/Hni648Q3nlxlo7GM/u48/e9zXL7qZmjNfodDDwoU1TpqyR+AY7G18dXlXryvJ/19w/dVvSdRutMvo7yzlaOVDkY7+xqkDmvTPhn4HMxj1jV4v3Y+aCFh97/AGjWGPxNHDUXKrt27+R15JlWLzHFxp4XSS1v/L53PQ/C+oXGqaJb3t1bNbyyLkoe/v8AQ1p0AAAAAADoBRX5nUkpSbirLsf0lh6c6dKMJy5mkrvv5hRRRUGwUUUUAFFFFABRRRQAV4d+038CdO+JWkyavo8cdp4mtkzHIBhbkD+B/f0Ne40VpSqypSUovUTV9GfkTruk6joer3Ok6taS2d7ayGOaGVdrKRVKv0T/AGnfgXp/xK0h9Y0iOO18TWqZikAwLkD+B/6Gvz51zStQ0TVbjS9UtZbW8tnKSxSLgqRX1GExccRG636o55RcSlWp4V1/VvDGvWut6JeSWl9bOGjkQ4/A+oPpWXRXU0mrMk/RP4LftDeFvF3ge41HxDfW+lappkG+/idsBwP40HfJ7V8o/tJ/G/VPifrbWVk8tn4btn/0e2zgykfxv6n0HavGwSM4JGeuO9JXFRwFKlUc18vIpzbVgoooAJIAGSa7SQr6x/ZM/Z4k1J7Xxz44tClkMSWFhIuDL3Ejg/w+g70fsmfs8NqMtt448c2eLJCJLCwlX/WnqHcH+H0HevtFFVECIoVVGAAMACvFx+PtenTfqzWEOrEjRI41jjUIigBVAwAPSnUUV4ZsYXjLwzZeJNOaCcBLhQfJmA5U/wCFeBa/pV7ompSWF9GUkToccMOxHtX01WF4z8M2XiXTTBOAk6AmGYDlD/hXvZPnEsHL2dTWD/A+K4r4UhmsHXoK1Zf+TeT8+z+8+c80Zq5r2k3ui6jJY30ZSRTwezD1FUM1+gQlGcVKLumfiFWjOjN06is1umTwTSQTJNE7JIh3KynBBr2PwL8QrO701odbnSC5t0yZCcCUD+teK5pd1cWPy2ljoctTdbPqetkme4rJ6rqUHdPdPZ/8Mdb4/wDGFz4ivDFGxisIz+7j/ve5rls1HuozXTQw1PD01TpqyR5+OxlfHV5V68ryf9fcSZozTAa9P+GHgQzmPWdZhIjHzQQMPvf7R9qxxuMpYOk6lR/8E6MpyfEZriFQoL1fRLux/wAMvApmMesazFiP70EDfxf7R9vavWFAUAAAAcACgAAAAAAdBRX5xjsdVxtXnn8l2P3/ACXJcNlGHVGivV9W/wCugUUUVxHrhRRRQAUUUUAFFFFABRRRQAUUUUAFeH/tOfArT/iXpUmsaSsVp4mto/3UuMLcgDhH/oe1e4Vn+I9a0zw7otzrGsXkVpZWyF5ZZGwAB/WtaNSdOalDcTSa1PyZ1zStQ0TVrnStVtZLS8tnMcsUgwVIqlXp/wC0l8S7T4nePn1aw0q3srO3XyYJBGBNOoPDSHv7DtXmFfXU5SlBOSszlYUUUVYAAScDk19Zfsmfs7vqLW3jjx1ZFLMESafYSjmXuJHHZfQd6+VtLvJtO1K2v7cRtLbyrKgkQOpKnIyp4I46Gv0b/Zv+NOjfFDw+lrIIrDX7RAtzZ8AMMffj9V9u1efmVSrCn7m3Vl00m9T16NEjjWONVRFGFUDAA9KWiivmToCiiigAooooAwfGnhiy8S6cYJwEuEH7mYDlT/hXz7r+k32h6lJYahFskQ8Hsw9QfSvpjUr2106ykvLyZYYYxlmY18+/EXxa/ijU1aOMRWkGRCCBuPuT/Svr+GK2KcnTSvT/ACfl/kfmPiBhMvUI1m7Vn0XVefp0fy9Oa3UbqZmjNfa2PynlJM+9AOajzTo5GjkV0YqynIPoaVg5T1P4XeA/P8vWdaiIj+9BAw+9/tH29q9dAAAAAAHQVwPwx8dQ63Ammai6R6hGuFJ4Ew9vf2rvq/Ms5qYmeJaxGjWy6W8j+gOFcPl9LAReBd0931b8+3oFFFFeSfSBRRRQAUUUUAFFFFABRRRQAUUUUAFFFZ3iTW9L8OaJdazrN5HaWNqheWWQ4AH+NNJt2QB4l1vTPDmiXWs6xdx2llaxl5ZHOAAO3ufavzx/aT+N2qfFDWzZWbSWfhy2ci3tg2DKf77+p9B2pf2lfjdqfxQ1trGxeW08N2z/AOj2+cGYj/lo/v6DtXjVfQ4DAKkuefxfkYTnfRBRRQAScAZNeoZhRXv3w2/Zh8V+Lfh7deJprgadcvHv02zlT5rgdcsf4Qe1eHa5pWoaJq1zpWq2ktpeW0hjlikXDKwrOFanUk4xd2htNFKtTwrr+reGNdtta0S8ltL22cPHIhx07H1HtWXRWjSasxH6S/s4/GrSfijoYtp3jtfENqg+1WpON4/vp6j27V69X5IeFfEGr+F9etdc0O9ks7+1cPHKh/QjuD3Ffoj+zl8bNI+KWhLbztFZ+IraMfa7TON+Osieqn07V85j8A6L54fD+RvCd9GevUUUV5hoFVtTvrXTbGW9vJligiXczNRqd9aabYy3t7MsMEQyzMa+ffiN42uvE96YoS0OmxH91F3b/ab3r1spympmFSy0it3+i8z57iDiCjlFG71qPZfq/Id8RPGl14lvTFEzRafGf3cf97/aNclmmZozX6Xh8NTw1NU6askfheNxdbG1pV68ryY/NGTTM16R4J+GVxrGkSX+pSSWfmJ/oq45J7MR6Vni8ZRwkOes7I1y7K8TmNX2WHjd7/0zzvJozV3xDpF7oepyWF/EUkQ8HHDD1FZ+feuiEozipRd0zjq0Z0puE1ZrdE0E8sEyTQu0ciHKspwQa9z+GHjuLXIU0zUpFTUEXCseBKB/WvBs1JBPLBMk0LmORCGVlOCDXn5lllLH0uWWjWz7HsZFnlfJ6/tKesXuu6/z7M+tKK4D4X+PItdhTTNSdY9SQYVicCYDuPf2rv6/M8XhKuEqulVVmj92y7MaGYUFXoO6f4Ps/MKKKK5juCiiigAooooAKKKKACiig9KAM7xLrmleG9EudZ1q8jtLG2QvLK5wAPQep9q/PH9pP43ap8T9beyspJbTw3bSH7NbZx5pH8b+p9u1bH7YPxL8XeJPG114V1G0udH0jT5SIrNuDOR0kbs2e3pXgdfRZfgVTSqT3/IwnO+iCiigAkgAZJr1DMK+s/2TP2d31Brbxz46tNtmMSafp8i8y9/MkB6L0wO9J+yb+zwdSe28b+ObLFmpEljYSr/rT1DuP7voO9faCKqIERQqqMAAYAFeLj8fa9Om/VmsIdWEaJGixooVFGFUDAA9K8Q/ac+BWnfErSZNY0mOO08T2yZilAwtyAPuP/Q9q9worx6VWVKSlF6mrSasfkTrmlahomrXOlaray2l7bSGOaKRcMrCqVfon+098C7D4l6U+s6Skdr4mtY/3UmMC5UdI29/Q1+fOt6XqGi6rcaVqlrJa3ls5SWKQYKkV9RhMXHERut+qOeUeUpVqeFfEGreGNdttb0W7ktb22cNG6HH4H1FZdFdTSasyT9Jf2cPjVpXxR0Fbe4eK18Q2qD7Va7sbx/z0T1H8q9V1O+tdNsZb29mWGCJdzMxr8ovAOoeIdK8W2GoeF5podVhlDQtEefcH2PevsbxP4417xTp1jDq3lQGKFPOigJ2NLj5m59849K4KHD0sTX9x2h18vJHj55xDSyqhd61Hsv1fkaXxG8bXXie9MURaLTo2/dR5+9/tH3rkM1HmjNffYfC08NTVOmrJH4ljMXWxtaVau7yZJmgHmmAmvWfhN8PTceVrmuQkRDDW9uw+9/tN7Vjj8dSwNJ1ar9F3OjK8pr5nXVGivV9Eu7H/Cf4emcx63rkJEfDW9u4+9/tMPT2r2RQFAVQABwAKAAAAAAB0Aor8uzDMKuOq+0qfJdj9zyjKMPlVBUaK9X1bOf8b+FbHxRppt7gCO4UfuZwOUP9R7V86+IdHv8AQtTk0/UIikqHg9mHqD6V9VVz3jnwrY+KNMNvcKEuEBME2OUP+FenkmdywUvZVdab/A8PijheGZw9vQVqq/8AJvJ+fZnzLuo3Ve8Q6Pf6FqclhqERjkQ8Hsw9Qazs1+jwlGpFSi7pn4xVozpTcJqzW6J7eeSCZJoZGjkQhlZTgg17t8LvHsWuQppmpOseooMKxPEw/wAa8DzV3Q7fULvVIIdLWQ3ZcGMx8EH1z2rzs1y2jjaLVTRrZ9v+AezkGcYnK8SpUVzKWjj3/wCD2PrCiqOgx38Oj20eqTJNeLGBK6jAJq9X5VOPLJq9z98pyc4KTVr9H0CiiipLCiiigAooooAKKKKAPK/2hfg3o/xT8PFdsVprlupNnebec/3W9VNfnR4y8Naz4R8RXWg69ZvaXts5V1YcEdmB7g9jX6115V+0P8GtF+Knh05SO0161UmzvQuD/uP6qf0r08DjnRfJP4fyM5wvqj80RycCvrP9kz9ndtRNr458dWTLaAiTT9PlXBl9JJB/d7gd+tXv2bv2Y7yz19vEHxFs1VbKYi1sCQwlZTw7f7PcDvX2CiLGioihVUYAAwAK6cfmGns6T9WTCHVhGiRoqRqFRRgKBgAUtFFeGbBRRRQAV4h+038CtP8AiZpTatpCQ2fia2jPlS4wtyo52P8A0Pavb6K0pVZUpKUXqJq+jPyJ1zStQ0TVrnStVtJbS9tnMcsUgwVIqPS7C61O+isrKFpZ5W2qor9AP2r/AIM6J448OzeJLV7fTtesoyRO3yrcqP4H9/Q14F8OvBlp4XsvMkCTajKP3suM7f8AZX0H86+1yiDzFcy0S3Pms+zqllNLXWb2X6vyH/DnwZaeF7EPIEm1CUDzZcfd/wBke1dfmo80ua+0pUY0oqMVofjWLxNXF1XWrO8mSZozUea9N+B/hTTdau5NUv5opvsrjbak8k9mYelc+OxcMHQlWqbI1y3LqmYYmOHpbvuafwj+HhnMWva5DiMENb27D73ozD09q9oAAAAAAHQCkUBVCqAAOABS1+U5jmNXH1XUqfJdj9yyjKKGV0FRor1fVsKKKK4D1AooooA57xz4UsPFOmG3uFEdwgJgnA5Q/wBR7V84eI9Gv9A1SXT9RhMcqHg/wuOxB7ivrCud8d+E7DxTpZt51CXKDMEwHKn/AAr6LI88lgpeyq603+Hmv1Pj+J+GIZnB16KtVX/k3k/Psz5p0uxu9Tv4rGxheaeVtqIoyTX0T8OPBNp4XsRJKFm1GQfvZf7v+yvtR8OfBFn4UtDIxW41CUfvJsdB/dX0FdfWue568W3RoP3Pz/4BhwvwtHL0sTiVeq9l/L/wQooor5g+3CiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKrarf2ml2E19fTpBbwrud2OAKTVdQtNLsJb6+mWGCJcszGvm34nePLzxXfmGEtDpkTfuos/f/wBpvevZyfJ6uZVbLSC3f6LzPBz3PaOVUbvWb2X6vyH/ABM8d3fiq/MULPDpsR/dRdN3+03vXF5pm6jdX6phsLTw1JUqSskfjGLxVbGVnWrO8mSZ96M0wGnSq8UhjkRkdeCrDBFb2OXlFzWn4b1y/wBA1SPUNPmMciHkdmHoR6Vk7hS596ipTjUi4zV0y6c50pqcHZrZn1X4C8W2HizShc27BLmMATwZ5Q/4V0dfIfhvXL/w/qseo6dMY5U6js47gj0r6Y8BeLtP8WaUtzbER3KAefATyh/qPevzPPshlgJe1pa03+Hk/wBGfr/DfEkcygqNbSqv/JvNefdHSUUUV80fWhRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFVtVv7TS7CW+vp1ht4l3O7HpSatqNnpWny319OsMES7mZjXzP8UfH154tv8AyYWeDS4WPlQ5xvP95vU/yr2smyarmVWy0gt3+i8zws8zylldK71m9l+r8h/xP8eXfiy/MUJaHTIm/cxZ5b/ab3ris0zNGa/V8NhaWFpKlSVkj8bxeKrYuq61Z3kx+aX2pmTXsnwc+GrXBh8Qa/CViB3W9sw5b0Zh6e1YZhj6OAourVfourfY3y3LK+Y11Ror1fRLux3wc+GxuPK1/X4SIgQ1tbuPvf7TD09BXT/Fv4dQ6/bPqukxpFqca5KgYE4Hb6+9ekKAqhVAAHAApa/L62fYupi1ilKzWy6W7f5n63R4bwVPBPBuN0931v3/AMj4wuIZred4J42jlQlWVhgg0zNfRHxc+HUXiCB9W0mNI9TRcso4EwH9a+eLiKW2neCeNo5UO1kYYIPpX6TlOa0cypc8NJLddv8AgH5XnOS1srrck9YvZ9/+CJmtPw1rl/4f1WLUdPmMcqHkZ4Yeh9qys0Zr0qlONSLhNXTPKpznSmpwdmtmfWHgDxfp/i3SVubZhHcoAJ4CfmQ/1HvXSV8feGdd1Dw9q0WpabMY5UPIz8rr3UjuK+nPAHjDT/FulLcW7CO6QYngJ5Q/1FfmGfZBLAS9rS1pv8PJ/oz9c4c4jjmMFRraVV/5N5rz7o6WiiivmT6wKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAOY+JXhRfFvh5rAXDwTod8LA/KW9GHcV8s65pV/oupzadqNu0FxC2GVu/uPUV9m1x/xM8DWHi/TD8qw6jEP3E4HP+6fUV9Tw7n7wEvY1v4b/AAf+Xc+S4k4eWYR9vR/iL8V29ex8q5o+gq9ruk32i6nLp2oQNFPG2CCOvuPUV6v8Gvhk1w0PiDxDBiH71tbOPvejMPT2r9Cx2Z4fB4f283o9rdfQ/OMBlWIxuI+rwjZre/T1F+DPw0NwYfEPiCDEQ+a2tnH3vRmHp6Cvc1AVQqgADgAUKoVQqgAAYAHalr8lzPMq2Y1nVqv0XRI/YsryuhltBUqS9X1bCiiivOPSCvNfi98OYvEVu+raTEseqRrllHAnHoff3r0qiuvBY2tgqyq0nZr8fJnHjsDRx1F0ayun+Hmj4ruIZbad4J42ilQlWRhgg1Hmvoz4vfDiLxDA+raRGseqRjLIOBOP8a8AsdI1K91hdIgtJDetJ5ZiK4Knvn0xX6zleb0Mwoe1Ts1uu3/A8z8dzbJK+XYj2TV0/hff/g+Q3SbC81XUIbCwgee4mYKiKOpr6a+F3ga28IaaWkYTajOo8+Tsv+yvtTPhd4DtPCWnCWULNqcq/vZcfd/2V9q7avh+IeIHjG6FB/u1u+//AAD73hrhuOBSxGIV6j2X8v8AwQooor5M+xCiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAxdd8LaHrd/a32pWKTT2rbo2Pf2PqK2VAVQqgAAYAHalorSVWc4qMm2lt5GcKNOEnKMUm9/MKKKKzNAooooAKKKKACs+HRdLh1iXV4rKFb6VQrzBfmIrQoqozlG/K7XJlCMrOSvYKKKKkoKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA//2Q=="),
      contactLink = Just adminContactReq,
      preferences = Nothing
    }

simplexStatusContactProfile :: Profile
simplexStatusContactProfile =
  Profile
    { displayName = "SimpleX-Status",
      fullName = "",
      image = Just (ImageData "data:image/jpg;base64,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"),
      contactLink = Just (either error id $ strDecode "simplex:/contact/#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FShQuD-rPokbDvkyotKx5NwM8P3oUXHxA%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEA6fSx1k9zrOmF0BJpCaTarZvnZpMTAVQhd3RkDQ35KT0%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"),
      preferences = Nothing
    }

timeItToView :: String -> CM' a -> CM' a
timeItToView s action = do
  t1 <- liftIO getCurrentTime
  a <- action
  t2 <- liftIO getCurrentTime
  let diff = diffToMilliseconds $ diffUTCTime t2 t1
  toView' $ CRTimedAction s diff
  pure a

mkValidName :: String -> String
mkValidName = reverse . dropWhile isSpace . fst3 . foldl' addChar ("", '\NUL', 0 :: Int)
  where
    fst3 (x, _, _) = x
    addChar (r, prev, punct) c = if validChar then (c' : r, c', punct') else (r, prev, punct)
      where
        c' = if isSpace c then ' ' else c
        punct'
          | isPunctuation c = punct + 1
          | isSpace c = punct
          | otherwise = 0
        validChar
          | c == '\'' = False
          | prev == '\NUL' = c > ' ' && c /= '#' && c /= '@' && validFirstChar
          | isSpace prev = validFirstChar || (punct == 0 && isPunctuation c)
          | isPunctuation prev = validFirstChar || isSpace c || (punct < 3 && isPunctuation c)
          | otherwise = validFirstChar || isSpace c || isMark c || isPunctuation c
        validFirstChar = isLetter c || isNumber c || isSymbol c

xftpSndFileTransfer_ :: User -> CryptoFile -> Integer -> Int -> Maybe ContactOrGroup -> CM (FileInvitation, CIFile 'MDSnd, FileTransferMeta)
xftpSndFileTransfer_ user file@(CryptoFile filePath cfArgs) fileSize n contactOrGroup_ = do
  let fileName = takeFileName filePath
      fInv = xftpFileInvitation fileName fileSize dummyFileDescr
  fsFilePath <- lift $ toFSFilePath filePath
  let srcFile = CryptoFile fsFilePath cfArgs
  aFileId <- withAgent $ \a -> xftpSendFile a (aUserId user) srcFile (roundedFDCount n)
  -- TODO CRSndFileStart event for XFTP
  chSize <- asks $ fileChunkSize . config
  ft@FileTransferMeta {fileId} <- withStore' $ \db -> createSndFileTransferXFTP db user contactOrGroup_ file fInv (AgentSndFileId aFileId) Nothing chSize
  let fileSource = Just $ CryptoFile filePath cfArgs
      ciFile = CIFile {fileId, fileName, fileSize, fileSource, fileStatus = CIFSSndStored, fileProtocol = FPXFTP}
  pure (fInv, ciFile, ft)

xftpSndFileRedirect :: User -> FileTransferId -> ValidFileDescription 'FRecipient -> CM FileTransferMeta
xftpSndFileRedirect user ftId vfd = do
  let fileName = "redirect.yaml"
      file = CryptoFile fileName Nothing
      fInv = xftpFileInvitation fileName (fromIntegral $ B.length $ strEncode vfd) dummyFileDescr
  aFileId <- withAgent $ \a -> xftpSendDescription a (aUserId user) vfd (roundedFDCount 1)
  chSize <- asks $ fileChunkSize . config
  withStore' $ \db -> createSndFileTransferXFTP db user Nothing file fInv (AgentSndFileId aFileId) (Just ftId) chSize

dummyFileDescr :: FileDescr
dummyFileDescr = FileDescr {fileDescrText = "", fileDescrPartNo = 0, fileDescrComplete = False}
