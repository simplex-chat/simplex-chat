{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (bimap, first)
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char
import Data.Constraint (Dict (..))
import Data.Either (fromRight, lefts, partitionEithers, rights)
import Data.Fixed (div')
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (find, foldl', isSuffixOf, partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList, (<|))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDay, nominalDiffTimeToSeconds)
import Data.Time.Clock.System (systemToUTCTime)
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
import Simplex.Chat.Options
import Simplex.Chat.ProfileGenerator (generateRandomProfile)
import Simplex.Chat.Protocol
import Simplex.Chat.Remote
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store
import Simplex.Chat.Store.Connections
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Files
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Messages
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Util
import Simplex.Chat.Util (encryptFile, shuffle)
import Simplex.FileTransfer.Client.Main (maxFileSize)
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.FileTransfer.Description (ValidFileDescription, gb, kb, mb)
import Simplex.FileTransfer.Protocol (FileParty (..), FilePartyI)
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Client (AgentStatsKey (..), SubInfo (..), agentClientStore, temporaryAgentError)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..), InitialAgentServers (..), createAgentStore, defaultAgentConfig)
import Simplex.Messaging.Agent.Lock
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation (..), MigrationError, SQLiteStore (dbNew), execSQL, upMigration, withConnection)
import Simplex.Messaging.Agent.Store.SQLite.DB (SlowQueryStats (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
import Simplex.Messaging.Client (defaultNetworkConfig)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (base64P)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), AProtocolType (..), EntityId, ErrorType (..), MsgBody, MsgFlags (..), NtfServer, ProtoServerWithAuth, ProtocolTypeI, SProtocolType (..), SubscriptionMode (..), UserProtocol, userProtocol)
import qualified Simplex.Messaging.Protocol as SMP
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (defaultSocksProxy)
import Simplex.Messaging.Util
import Simplex.Messaging.Version
import Simplex.RemoteControl.Invitation (RCInvitation (..), RCSignedInvitation (..))
import Simplex.RemoteControl.Types (RCCtrlAddress (..))
import System.Exit (ExitCode, exitFailure, exitSuccess)
import System.FilePath (takeFileName, (</>))
import System.IO (Handle, IOMode (..), SeekMode (..), hFlush, stdout)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Concurrent (forkFinally, forkIO, mkWeakThreadId, threadDelay)
import UnliftIO.Directory
import qualified UnliftIO.Exception as E
import UnliftIO.IO (hClose, hSeek, hTell, openFile)
import UnliftIO.STM

defaultChatConfig :: ChatConfig
defaultChatConfig =
  ChatConfig
    { agentConfig =
        defaultAgentConfig
          { tcpPort = undefined, -- agent does not listen to TCP
            tbqSize = 1024
          },
      chatVRange = supportedChatVRange,
      confirmMigrations = MCConsole,
      defaultServers =
        DefaultAgentServers
          { smp = _defaultSMPServers,
            ntf = _defaultNtfServers,
            xftp = defaultXFTPServers,
            netCfg = defaultNetworkConfig
          },
      tbqSize = 1024,
      fileChunkSize = 15780, -- do not change
      xftpDescrPartSize = 14000,
      inlineFiles = defaultInlineFilesConfig,
      autoAcceptFileSize = 0,
      xftpFileConfig = Just defaultXFTPFileConfig,
      tempDir = Nothing,
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
      deviceNameForRemote = ""
    }

_defaultSMPServers :: NonEmpty SMPServerWithAuth
_defaultSMPServers =
  L.fromList
    [ "smp://1OwYGt-yqOfe2IyVHhxz3ohqo3aCCMjtB-8wn4X_aoY=@smp11.simplex.im,6ioorbm6i3yxmuoezrhjk6f6qgkc4syabh7m3so74xunb5nzr4pwgfqd.onion",
      "smp://UkMFNAXLXeAAe0beCa4w6X_zp18PwxSaSjY17BKUGXQ=@smp12.simplex.im,ie42b5weq7zdkghocs3mgxdjeuycheeqqmksntj57rmejagmg4eor5yd.onion",
      "smp://enEkec4hlR3UtKx2NMpOUK_K4ZuDxjWBO1d9Y4YXVaA=@smp14.simplex.im,aspkyu2sopsnizbyfabtsicikr2s4r3ti35jogbcekhm3fsoeyjvgrid.onion",
      "smp://h--vW7ZSkXPeOUpfxlFGgauQmXNFOzGoizak7Ult7cw=@smp15.simplex.im,oauu4bgijybyhczbnxtlggo6hiubahmeutaqineuyy23aojpih3dajad.onion",
      "smp://hejn2gVIqNU6xjtGM3OwQeuk8ZEbDXVJXAlnSBJBWUA=@smp16.simplex.im,p3ktngodzi6qrf7w64mmde3syuzrv57y55hxabqcq3l5p6oi7yzze6qd.onion",
      "smp://ZKe4uxF4Z_aLJJOEsC-Y6hSkXgQS5-oc442JQGkyP8M=@smp17.simplex.im,ogtwfxyi3h2h5weftjjpjmxclhb5ugufa5rcyrmg7j4xlch7qsr5nuqd.onion",
      "smp://PtsqghzQKU83kYTlQ1VKg996dW4Cw4x_bvpKmiv8uns=@smp18.simplex.im,lyqpnwbs2zqfr45jqkncwpywpbtq7jrhxnib5qddtr6npjyezuwd3nqd.onion",
      "smp://N_McQS3F9TGoh4ER0QstUf55kGnNSd-wXfNPZ7HukcM=@smp19.simplex.im,i53bbtoqhlc365k6kxzwdp5w3cdt433s7bwh3y32rcbml2vztiyyz5id.onion"
    ]

_defaultNtfServers :: [NtfServer]
_defaultNtfServers = ["ntf://FB-Uop7RTaZZEG0ZLD2CIaTjsPh-Fw0zFAnb7QyA8Ks=@ntf2.simplex.im,ntg7jdjy2i3qbib3sykiho3enekwiaqg3icctliqhtqcg6jmoh6cxiad.onion"]

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
  cfg@ChatConfig {agentConfig = aCfg, defaultServers, inlineFiles, tempDir, deviceNameForRemote}
  ChatOpts {coreOptions = CoreChatOpts {smpServers, xftpServers, networkConfig, logLevel, logConnections, logServerHosts, logFile, tbqSize, highlyAvailable}, deviceName, optFilesFolder, showReactions, allowInstantFiles, autoAcceptFileSize}
  backgroundMode = do
    let inlineFiles' = if allowInstantFiles || autoAcceptFileSize > 0 then inlineFiles else inlineFiles {sendChunks = 0, receiveInstant = False}
        config = cfg {logLevel, showReactions, tbqSize, subscriptionEvents = logConnections, hostEvents = logServerHosts, defaultServers = configServers, inlineFiles = inlineFiles', autoAcceptFileSize, highlyAvailable}
        firstTime = dbNew chatStore
    currentUser <- newTVarIO user
    currentRemoteHost <- newTVarIO Nothing
    servers <- agentServers config
    smpAgent <- getSMPAgentClient aCfg {tbqSize} servers agentStore backgroundMode
    agentAsync <- newTVarIO Nothing
    random <- liftIO C.newRandom
    inputQ <- newTBQueueIO tbqSize
    outputQ <- newTBQueueIO tbqSize
    connNetworkStatuses <- atomically TM.empty
    subscriptionMode <- newTVarIO SMSubscribe
    chatLock <- newEmptyTMVarIO
    sndFiles <- newTVarIO M.empty
    rcvFiles <- newTVarIO M.empty
    currentCalls <- atomically TM.empty
    localDeviceName <- newTVarIO $ fromMaybe deviceNameForRemote deviceName
    multicastSubscribers <- newTMVarIO 0
    remoteSessionSeq <- newTVarIO 0
    remoteHostSessions <- atomically TM.empty
    remoteHostsFolder <- newTVarIO Nothing
    remoteCtrlSession <- newTVarIO Nothing
    filesFolder <- newTVarIO optFilesFolder
    chatStoreChanged <- newTVarIO False
    expireCIThreads <- newTVarIO M.empty
    expireCIFlags <- newTVarIO M.empty
    cleanupManagerAsync <- newTVarIO Nothing
    timedItemThreads <- atomically TM.empty
    showLiveItems <- newTVarIO False
    encryptLocalFiles <- newTVarIO False
    userXFTPFileConfig <- newTVarIO $ xftpFileConfig cfg
    tempDirectory <- newTVarIO tempDir
    contactMergeEnabled <- newTVarIO True
    pure
      ChatController
        { firstTime,
          currentUser,
          currentRemoteHost,
          smpAgent,
          agentAsync,
          chatStore,
          chatStoreChanged,
          random,
          inputQ,
          outputQ,
          connNetworkStatuses,
          subscriptionMode,
          chatLock,
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
          showLiveItems,
          encryptLocalFiles,
          userXFTPFileConfig,
          tempDirectory,
          logFilePath = logFile,
          contactMergeEnabled
        }
    where
      configServers :: DefaultAgentServers
      configServers =
        let smp' = fromMaybe (defaultServers.smp) (nonEmpty smpServers)
            xftp' = fromMaybe (defaultServers.xftp) (nonEmpty xftpServers)
         in defaultServers {smp = smp', xftp = xftp', netCfg = networkConfig}
      agentServers :: ChatConfig -> IO InitialAgentServers
      agentServers config@ChatConfig {defaultServers = defServers@DefaultAgentServers {ntf, netCfg}} = do
        users <- withTransaction chatStore getUsers
        smp' <- getUserServers users SPSMP
        xftp' <- getUserServers users SPXFTP
        pure InitialAgentServers {smp = smp', xftp = xftp', ntf, netCfg}
        where
          getUserServers :: forall p. (ProtocolTypeI p, UserProtocol p) => [User] -> SProtocolType p -> IO (Map UserId (NonEmpty (ProtoServerWithAuth p)))
          getUserServers users protocol = case users of
            [] -> pure $ M.fromList [(1, cfgServers protocol defServers)]
            _ -> M.fromList <$> initialServers
            where
              initialServers :: IO [(UserId, NonEmpty (ProtoServerWithAuth p))]
              initialServers = mapM (\u -> (aUserId u,) <$> userServers u) users
              userServers :: User -> IO (NonEmpty (ProtoServerWithAuth p))
              userServers user' = activeAgentServers config protocol <$> withTransaction chatStore (`getProtocolServers` user')

activeAgentServers :: UserProtocol p => ChatConfig -> SProtocolType p -> [ServerCfg p] -> NonEmpty (ProtoServerWithAuth p)
activeAgentServers ChatConfig {defaultServers} p =
  fromMaybe (cfgServers p defaultServers)
    . nonEmpty
    . map (\ServerCfg {server} -> server)
    . filter (\ServerCfg {enabled} -> enabled)

cfgServers :: UserProtocol p => SProtocolType p -> (DefaultAgentServers -> NonEmpty (ProtoServerWithAuth p))
cfgServers p s = case p of
  SPSMP -> s.smp
  SPXFTP -> s.xftp

startChatController :: forall m. ChatMonad' m => Bool -> Bool -> Bool -> m (Async ())
startChatController subConns enableExpireCIs startXFTPWorkers = do
  asks smpAgent >>= resumeAgentClient
  unless subConns $
    chatWriteVar subscriptionMode SMOnlyCreate
  users <- fromRight [] <$> runExceptT (withStoreCtx' (Just "startChatController, getUsers") getUsers)
  restoreCalls
  s <- asks agentAsync
  readTVarIO s >>= maybe (start s users) (pure . fst)
  where
    start s users = do
      a1 <- async agentSubscriber
      a2 <-
        if subConns
          then Just <$> async (subscribeUsers False users)
          else pure Nothing
      atomically . writeTVar s $ Just (a1, a2)
      when startXFTPWorkers $ do
        startXFTP
        void $ forkIO $ startFilesToReceive users
      startCleanupManager
      when enableExpireCIs $ startExpireCIs users
      pure a1
    startXFTP = do
      tmp <- readTVarIO =<< asks tempDirectory
      runExceptT (withAgent $ \a -> xftpStartWorkers a tmp) >>= \case
        Left e -> liftIO $ print $ "Error starting XFTP workers: " <> show e
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
        ttl <- fromRight Nothing <$> runExceptT (withStoreCtx' (Just "startExpireCIs, getChatItemTTL") (`getChatItemTTL` user))
        forM_ ttl $ \_ -> do
          startExpireCIThread user
          setExpireCIFlag user True

subscribeUsers :: forall m. ChatMonad' m => Bool -> [User] -> m ()
subscribeUsers onlyNeeded users = do
  let (us, us') = partition activeUser users
  vr <- chatVersionRange
  subscribe vr us
  subscribe vr us'
  where
    subscribe :: VersionRange -> [User] -> m ()
    subscribe vr = mapM_ $ runExceptT . subscribeUserConnections vr onlyNeeded Agent.subscribeConnections

startFilesToReceive :: forall m. ChatMonad' m => [User] -> m ()
startFilesToReceive users = do
  let (us, us') = partition activeUser users
  startReceive us
  startReceive us'
  where
    startReceive :: [User] -> m ()
    startReceive = mapM_ $ runExceptT . startReceiveUserFiles

startReceiveUserFiles :: ChatMonad m => User -> m ()
startReceiveUserFiles user = do
  filesToReceive <- withStoreCtx' (Just "startReceiveUserFiles, getRcvFilesToReceive") (`getRcvFilesToReceive` user)
  forM_ filesToReceive $ \ft ->
    flip catchChatError (toView . CRChatError (Just user)) $
      toView =<< receiveFile' user ft Nothing Nothing

restoreCalls :: ChatMonad' m => m ()
restoreCalls = do
  savedCalls <- fromRight [] <$> runExceptT (withStoreCtx' (Just "restoreCalls, getCalls") $ \db -> getCalls db)
  let callsMap = M.fromList $ map (\call@Call {contactId} -> (contactId, call)) savedCalls
  calls <- asks currentCalls
  atomically $ writeTVar calls callsMap

stopChatController :: forall m. MonadUnliftIO m => ChatController -> m ()
stopChatController ChatController {smpAgent, agentAsync = s, sndFiles, rcvFiles, expireCIFlags, remoteHostSessions, remoteCtrlSession} = do
  readTVarIO remoteHostSessions >>= mapM_ (liftIO . cancelRemoteHost False . snd)
  atomically (stateTVar remoteCtrlSession (,Nothing)) >>= mapM_ (liftIO . cancelRemoteCtrl False . snd)
  disconnectAgentClient smpAgent
  readTVarIO s >>= mapM_ (\(a1, a2) -> uninterruptibleCancel a1 >> mapM_ uninterruptibleCancel a2)
  closeFiles sndFiles
  closeFiles rcvFiles
  atomically $ do
    keys <- M.keys <$> readTVar expireCIFlags
    forM_ keys $ \k -> TM.insert k False expireCIFlags
    writeTVar s Nothing
  where
    closeFiles :: TVar (Map Int64 Handle) -> m ()
    closeFiles files = do
      fs <- readTVarIO files
      mapM_ hClose fs
      atomically $ writeTVar files M.empty

execChatCommand :: ChatMonad' m => Maybe RemoteHostId -> ByteString -> m ChatResponse
execChatCommand rh s = do
  u <- readTVarIO =<< asks currentUser
  case parseChatCommand s of
    Left e -> pure $ chatCmdError u e
    Right cmd -> case rh of
      Just rhId
        | allowRemoteCommand cmd -> execRemoteCommand u rhId cmd s
        | otherwise -> pure $ CRChatCmdError u $ ChatErrorRemoteHost (RHId rhId) $ RHELocalCommand
      _ -> execChatCommand_ u cmd

execChatCommand' :: ChatMonad' m => ChatCommand -> m ChatResponse
execChatCommand' cmd = asks currentUser >>= readTVarIO >>= (`execChatCommand_` cmd)

execChatCommand_ :: ChatMonad' m => Maybe User -> ChatCommand -> m ChatResponse
execChatCommand_ u cmd = handleCommandError u $ processChatCommand cmd

execRemoteCommand :: ChatMonad' m => Maybe User -> RemoteHostId -> ChatCommand -> ByteString -> m ChatResponse
execRemoteCommand u rhId cmd s = handleCommandError u $ getRemoteHostClient rhId >>= \rh -> processRemoteCommand rhId rh cmd s

handleCommandError :: ChatMonad' m => Maybe User -> ExceptT ChatError m ChatResponse -> m ChatResponse
handleCommandError u a = either (CRChatCmdError u) id <$> (runExceptT a `E.catches` ioErrors)
  where
    ioErrors =
      [ E.Handler $ \(e :: ExitCode) -> E.throwIO e,
        E.Handler $ pure . Left . mkChatError
      ]

parseChatCommand :: ByteString -> Either String ChatCommand
parseChatCommand = A.parseOnly chatCommandP . B.dropWhileEnd isSpace

-- | Chat API commands interpreted in context of a local zone
processChatCommand :: forall m. ChatMonad m => ChatCommand -> m ChatResponse
processChatCommand cmd = chatVersionRange >>= (`processChatCommand'` cmd)
{-# INLINE processChatCommand #-}

processChatCommand' :: forall m. ChatMonad m => VersionRange -> ChatCommand -> m ChatResponse
processChatCommand' vr = \case
  ShowActiveUser -> withUser' $ pure . CRActiveUser
  CreateActiveUser NewUser {profile, sameServers, pastTimestamp} -> do
    forM_ profile $ \Profile {displayName} -> checkValidName displayName
    p@Profile {displayName} <- liftIO $ maybe generateRandomProfile pure profile
    u <- asks currentUser
    (smp, smpServers) <- chooseServers SPSMP
    (xftp, xftpServers) <- chooseServers SPXFTP
    auId <-
      withStore' getUsers >>= \case
        [] -> pure 1
        users -> do
          when (any (\User {localDisplayName = n} -> n == displayName) users) $
            throwChatError (CEUserExists displayName)
          withAgent (\a -> createUser a smp xftp)
    ts <- liftIO $ getCurrentTime >>= if pastTimestamp then coupleDaysAgo else pure
    user <- withStore $ \db -> createUserRecordAt db (AgentUserId auId) p True ts
    when (auId == 1) $ withStore (\db -> createContact db user simplexContactProfile) `catchChatError` \_ -> pure ()
    storeServers user smpServers
    storeServers user xftpServers
    atomically . writeTVar u $ Just user
    pure $ CRActiveUser user
    where
      chooseServers :: (ProtocolTypeI p, UserProtocol p) => SProtocolType p -> m (NonEmpty (ProtoServerWithAuth p), [ServerCfg p])
      chooseServers protocol
        | sameServers =
            asks currentUser >>= readTVarIO >>= \case
              Nothing -> throwChatError CENoActiveUser
              Just user -> do
                servers <- withStore' (`getProtocolServers` user)
                cfg <- asks config
                pure (activeAgentServers cfg protocol servers, servers)
        | otherwise = do
            defServers <- asks $ defaultServers . config
            pure (cfgServers protocol defServers, [])
      storeServers user servers =
        unless (null servers) . withStore $
          \db -> overwriteProtocolServers db user servers
      coupleDaysAgo t = (`addUTCTime` t) . fromInteger . negate . (+ (2 * day)) <$> randomRIO (0, day)
      day = 86400
  ListUsers -> CRUsersList <$> withStoreCtx' (Just "ListUsers, getUsersInfo") getUsersInfo
  APISetActiveUser userId' viewPwd_ -> do
    unlessM chatStarted $ throwChatError CEChatNotStarted
    user_ <- chatReadVar currentUser
    user' <- privateGetUser userId'
    validateUserPassword_ user_ user' viewPwd_
    withStoreCtx' (Just "APISetActiveUser, setActiveUser") $ \db -> setActiveUser db userId'
    let user'' = user' {activeUser = True}
    chatWriteVar currentUser $ Just user''
    pure $ CRActiveUser user''
  SetActiveUser uName viewPwd_ -> do
    tryChatError (withStore (`getUserIdByName` uName)) >>= \case
      Left _ -> throwChatError CEUserUnknown
      Right userId -> processChatCommand $ APISetActiveUser userId viewPwd_
  SetAllContactReceipts onOff -> withUser $ \_ -> withStore' (`updateAllContactReceipts` onOff) >> ok_
  APISetUserContactReceipts userId' settings -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' Nothing
    withStore' $ \db -> updateUserContactReceipts db user' settings
    ok user
  SetUserContactReceipts settings -> withUser $ \User {userId} -> processChatCommand $ APISetUserContactReceipts userId settings
  APISetUserGroupReceipts userId' settings -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' Nothing
    withStore' $ \db -> updateUserGroupReceipts db user' settings
    ok user
  SetUserGroupReceipts settings -> withUser $ \User {userId} -> processChatCommand $ APISetUserGroupReceipts userId settings
  APIHideUser userId' (UserPwd viewPwd) -> withUser $ \user -> do
    user' <- privateGetUser userId'
    case viewPwdHash user' of
      Just _ -> throwChatError $ CEUserAlreadyHidden userId'
      _ -> do
        when (T.null viewPwd) $ throwChatError $ CEEmptyUserPassword userId'
        users <- withStore' getUsers
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
  StartChat subConns enableExpireCIs startXFTPWorkers -> withUser' $ \_ ->
    asks agentAsync >>= readTVarIO >>= \case
      Just _ -> pure CRChatRunning
      _ -> checkStoreNotChanged $ startChatController subConns enableExpireCIs startXFTPWorkers $> CRChatStarted
  APIStopChat -> do
    ask >>= stopChatController
    pure CRChatStopped
  APIActivateChat restoreChat -> withUser $ \_ -> do
    when restoreChat restoreCalls
    withAgent foregroundAgent
    when restoreChat $ do
      users <- withStoreCtx' (Just "APIActivateChat, getUsers") getUsers
      void . forkIO $ subscribeUsers True users
      void . forkIO $ startFilesToReceive users
      setAllExpireCIFlags True
    ok_
  APISuspendChat t -> do
    setAllExpireCIFlags False
    stopRemoteCtrl
    withAgent (`suspendAgent` t)
    ok_
  ResubscribeAllConnections -> withStoreCtx' (Just "ResubscribeAllConnections, getUsers") getUsers >>= subscribeUsers False >> ok_
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
  APISetXFTPConfig cfg -> do
    asks userXFTPFileConfig >>= atomically . (`writeTVar` cfg)
    ok_
  APISetEncryptLocalFiles on -> chatWriteVar encryptLocalFiles on >> ok_
  SetContactMergeEnabled onOff -> do
    asks contactMergeEnabled >>= atomically . (`writeTVar` onOff)
    ok_
  APIExportArchive cfg -> checkChatStopped $ exportArchive cfg >> ok_
  ExportArchive -> do
    ts <- liftIO getCurrentTime
    let filePath = "simplex-chat." <> formatTime defaultTimeLocale "%FT%H%M%SZ" ts <> ".zip"
    processChatCommand $ APIExportArchive $ ArchiveConfig filePath Nothing Nothing
  APIImportArchive cfg -> checkChatStopped $ do
    fileErrs <- importArchive cfg
    setStoreChanged
    pure $ CRArchiveImported fileErrs
  APIDeleteStorage -> withStoreChanged deleteStorage
  APIStorageEncryption cfg -> withStoreChanged $ sqlCipherExport cfg
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
    (errs, previews) <- partitionEithers <$> withStore' (\db -> getChatPreviews db vr user pendingConnections pagination query)
    unless (null errs) $ toView $ CRChatErrors (Just user) (map ChatErrorStore errs)
    pure $ CRApiChats user previews
  APIGetChat (ChatRef cType cId) pagination search -> withUser $ \user -> case cType of
    -- TODO optimize queries calculating ChatStats, currently they're disabled
    CTDirect -> do
      directChat <- withStore (\db -> getDirectChat db user cId pagination search)
      pure $ CRApiChat user (AChat SCTDirect directChat)
    CTGroup -> do
      groupChat <- withStore (\db -> getGroupChat db vr user cId pagination search)
      pure $ CRApiChat user (AChat SCTGroup groupChat)
    CTContactRequest -> pure $ chatCmdError (Just user) "not implemented"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APIGetChatItems pagination search -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db vr user pagination search
    pure $ CRChatItems user Nothing chatItems
  APIGetChatItemInfo chatRef itemId -> withUser $ \user -> do
    (aci@(AChatItem cType dir _ ci), versions) <- withStore $ \db ->
      (,) <$> getAChatItem db vr user chatRef itemId <*> liftIO (getChatItemVersions db itemId)
    let itemVersions = if null versions then maybeToList $ mkItemVersion ci else versions
    memberDeliveryStatuses <- case (cType, dir) of
      (SCTGroup, SMDSnd) -> do
        withStore' (`getGroupSndStatuses` itemId) >>= \case
          [] -> pure Nothing
          memStatuses -> pure $ Just $ map (uncurry MemberDeliveryStatus) memStatuses
      _ -> pure Nothing
    pure $ CRChatItemInfo user aci ChatItemInfo {itemVersions, memberDeliveryStatuses}
  APISendMessage (ChatRef cType chatId) live itemTTL (ComposedMessage file_ quotedItemId_ mc) -> withUser $ \user@User {userId} -> withChatLock "sendMessage" $ case cType of
    CTDirect -> do
      ct@Contact {contactId, contactUsed} <- withStore $ \db -> getContact db user chatId
      assertDirectAllowed user MDSnd ct XMsgNew_
      unless contactUsed $ withStore' $ \db -> updateContactUsed db user ct
      if isVoice mc && not (featureAllowed SCFVoice forUser ct)
        then pure $ chatCmdError (Just user) ("feature not allowed " <> T.unpack (chatFeatureNameText CFVoice))
        else do
          (fInv_, ciFile_, ft_) <- unzipMaybe3 <$> setupSndFileTransfer ct
          timed_ <- sndContactCITimed live ct itemTTL
          (msgContainer, quotedItem_) <- prepareMsg fInv_ timed_
          (msg@SndMessage {sharedMsgId}, _) <- sendDirectContactMessage ct (XMsgNew msgContainer)
          ci <- saveSndChatItem' user (CDDirectSnd ct) msg (CISndMsgContent mc) ciFile_ quotedItem_ timed_ live
          case ft_ of
            Just ft@FileTransferMeta {fileInline = Just IFMSent} ->
              sendDirectFileInline ct ft sharedMsgId
            _ -> pure ()
          forM_ (timed_ >>= timedDeleteAt') $
            startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId' ci)
          pure $ CRNewChatItem user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
      where
        setupSndFileTransfer :: Contact -> m (Maybe (FileInvitation, CIFile 'MDSnd, FileTransferMeta))
        setupSndFileTransfer ct = forM file_ $ \file -> do
          (fileSize, fileMode) <- checkSndFile mc file 1
          case fileMode of
            SendFileSMP fileInline -> smpSndFileTransfer file fileSize fileInline
            SendFileXFTP -> xftpSndFileTransfer user file fileSize 1 $ CGContact ct
          where
            smpSndFileTransfer :: CryptoFile -> Integer -> Maybe InlineFileMode -> m (FileInvitation, CIFile 'MDSnd, FileTransferMeta)
            smpSndFileTransfer (CryptoFile _ (Just _)) _ _ = throwChatError $ CEFileInternal "locally encrypted files can't be sent via SMP" -- can only happen if XFTP is disabled
            smpSndFileTransfer (CryptoFile file Nothing) fileSize fileInline = do
              subMode <- chatReadVar subscriptionMode
              (agentConnId_, fileConnReq) <-
                if isJust fileInline
                  then pure (Nothing, Nothing)
                  else bimap Just Just <$> withAgent (\a -> createConnection a (aUserId user) True SCMInvitation Nothing subMode)
              let fileName = takeFileName file
                  fileInvitation = FileInvitation {fileName, fileSize, fileDigest = Nothing, fileConnReq, fileInline, fileDescr = Nothing}
              chSize <- asks $ fileChunkSize . config
              withStore $ \db -> do
                ft@FileTransferMeta {fileId} <- liftIO $ createSndDirectFileTransfer db userId ct file fileInvitation agentConnId_ chSize subMode
                fileStatus <- case fileInline of
                  Just IFMSent -> createSndDirectInlineFT db ct ft $> CIFSSndTransfer 0 1
                  _ -> pure CIFSSndStored
                let fileSource = Just $ CF.plain file
                    ciFile = CIFile {fileId, fileName, fileSize, fileSource, fileStatus, fileProtocol = FPSMP}
                pure (fileInvitation, ciFile, ft)
        prepareMsg :: Maybe FileInvitation -> Maybe CITimed -> m (MsgContainer, Maybe (CIQuote 'CTDirect))
        prepareMsg fInv_ timed_ = case quotedItemId_ of
          Nothing -> pure (MCSimple (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
          Just quotedItemId -> do
            CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
              withStore $ \db -> getDirectChatItem db user chatId quotedItemId
            (origQmc, qd, sent) <- quoteData qci
            let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Nothing}
                qmc = quoteContent mc origQmc file
                quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
            pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Just quotedItem)
          where
            quoteData :: ChatItem c d -> m (MsgContent, CIQDirection 'CTDirect, Bool)
            quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} = throwChatError CEInvalidQuote
            quoteData ChatItem {content = CISndMsgContent qmc} = pure (qmc, CIQDirectSnd, True)
            quoteData ChatItem {content = CIRcvMsgContent qmc} = pure (qmc, CIQDirectRcv, False)
            quoteData _ = throwChatError CEInvalidQuote
    CTGroup -> do
      g@(Group gInfo _) <- withStore $ \db -> getGroup db vr user chatId
      assertUserGroupRole gInfo GRAuthor
      send g
      where
        send g@(Group gInfo@GroupInfo {groupId} ms)
          | isVoice mc && not (groupFeatureAllowed SGFVoice gInfo) = notAllowedError GFVoice
          | not (isVoice mc) && isJust file_ && not (groupFeatureAllowed SGFFiles gInfo) = notAllowedError GFFiles
          | otherwise = do
              (fInv_, ciFile_, ft_) <- unzipMaybe3 <$> setupSndFileTransfer g (length $ filter memberCurrent ms)
              timed_ <- sndGroupCITimed live gInfo itemTTL
              (msgContainer, quotedItem_) <- prepareGroupMsg user gInfo mc quotedItemId_ fInv_ timed_ live
              (msg@SndMessage {sharedMsgId}, sentToMembers) <- sendGroupMessage user gInfo ms (XMsgNew msgContainer)
              ci <- saveSndChatItem' user (CDGroupSnd gInfo) msg (CISndMsgContent mc) ciFile_ quotedItem_ timed_ live
              withStore' $ \db ->
                forM_ sentToMembers $ \GroupMember {groupMemberId} ->
                  createGroupSndStatus db (chatItemId' ci) groupMemberId CISSndNew
              mapM_ (sendGroupFileInline ms sharedMsgId) ft_
              forM_ (timed_ >>= timedDeleteAt') $
                startProximateTimedItemThread user (ChatRef CTGroup groupId, chatItemId' ci)
              pure $ CRNewChatItem user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
        notAllowedError f = pure $ chatCmdError (Just user) ("feature not allowed " <> T.unpack (groupFeatureNameText f))
        setupSndFileTransfer :: Group -> Int -> m (Maybe (FileInvitation, CIFile 'MDSnd, FileTransferMeta))
        setupSndFileTransfer g@(Group gInfo _) n = forM file_ $ \file -> do
          (fileSize, fileMode) <- checkSndFile mc file $ fromIntegral n
          case fileMode of
            SendFileSMP fileInline -> smpSndFileTransfer file fileSize fileInline
            SendFileXFTP -> xftpSndFileTransfer user file fileSize n $ CGGroup g
          where
            smpSndFileTransfer :: CryptoFile -> Integer -> Maybe InlineFileMode -> m (FileInvitation, CIFile 'MDSnd, FileTransferMeta)
            smpSndFileTransfer (CryptoFile _ (Just _)) _ _ = throwChatError $ CEFileInternal "locally encrypted files can't be sent via SMP" -- can only happen if XFTP is disabled
            smpSndFileTransfer (CryptoFile file Nothing) fileSize fileInline = do
              let fileName = takeFileName file
                  fileInvitation = FileInvitation {fileName, fileSize, fileDigest = Nothing, fileConnReq = Nothing, fileInline, fileDescr = Nothing}
                  fileStatus = if fileInline == Just IFMSent then CIFSSndTransfer 0 1 else CIFSSndStored
              chSize <- asks $ fileChunkSize . config
              withStore' $ \db -> do
                ft@FileTransferMeta {fileId} <- createSndGroupFileTransfer db userId gInfo file fileInvitation chSize
                let fileSource = Just $ CF.plain file
                    ciFile = CIFile {fileId, fileName, fileSize, fileSource, fileStatus, fileProtocol = FPSMP}
                pure (fileInvitation, ciFile, ft)
        sendGroupFileInline :: [GroupMember] -> SharedMsgId -> FileTransferMeta -> m ()
        sendGroupFileInline ms sharedMsgId ft@FileTransferMeta {fileInline} =
          when (fileInline == Just IFMSent) . forM_ ms $ \m ->
            processMember m `catchChatError` (toView . CRChatError (Just user))
          where
            processMember m@GroupMember {activeConn = Just conn@Connection {connStatus}} =
              when (connStatus == ConnReady || connStatus == ConnSndReady) $ do
                void . withStore' $ \db -> createSndGroupInlineFT db m conn ft
                sendMemberFileInline m conn ft sharedMsgId
            processMember _ = pure ()
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    where
      xftpSndFileTransfer :: User -> CryptoFile -> Integer -> Int -> ContactOrGroup -> m (FileInvitation, CIFile 'MDSnd, FileTransferMeta)
      xftpSndFileTransfer user file@(CryptoFile filePath cfArgs) fileSize n contactOrGroup = do
        let fileName = takeFileName filePath
            fileDescr = FileDescr {fileDescrText = "", fileDescrPartNo = 0, fileDescrComplete = False}
            fInv = xftpFileInvitation fileName fileSize fileDescr
        fsFilePath <- toFSFilePath filePath
        let srcFile = CryptoFile fsFilePath cfArgs
        aFileId <- withAgent $ \a -> xftpSendFile a (aUserId user) srcFile (roundedFDCount n)
        -- TODO CRSndFileStart event for XFTP
        chSize <- asks $ fileChunkSize . config
        ft@FileTransferMeta {fileId} <- withStore' $ \db -> createSndFileTransferXFTP db user contactOrGroup file fInv (AgentSndFileId aFileId) chSize
        let fileSource = Just $ CryptoFile filePath cfArgs
            ciFile = CIFile {fileId, fileName, fileSize, fileSource, fileStatus = CIFSSndStored, fileProtocol = FPXFTP}
        case contactOrGroup of
          CGContact Contact {activeConn} -> forM_ activeConn $ \conn ->
            withStore' $ \db -> createSndFTDescrXFTP db user Nothing conn ft fileDescr
          CGGroup (Group _ ms) -> forM_ ms $ \m -> saveMemberFD m `catchChatError` (toView . CRChatError (Just user))
            where
              -- we are not sending files to pending members, same as with inline files
              saveMemberFD m@GroupMember {activeConn = Just conn@Connection {connStatus}} =
                when ((connStatus == ConnReady || connStatus == ConnSndReady) && not (connDisabled conn)) $
                  withStore' $
                    \db -> createSndFTDescrXFTP db user (Just m) conn ft fileDescr
              saveMemberFD _ = pure ()
        pure (fInv, ciFile, ft)
      unzipMaybe3 :: Maybe (a, b, c) -> (Maybe a, Maybe b, Maybe c)
      unzipMaybe3 (Just (a, b, c)) = (Just a, Just b, Just c)
      unzipMaybe3 _ = (Nothing, Nothing, Nothing)
  APIUpdateChatItem (ChatRef cType chatId) itemId live mc -> withUser $ \user -> withChatLock "updateChatItem" $ case cType of
    CTDirect -> do
      ct@Contact {contactId} <- withStore $ \db -> getContact db user chatId
      assertDirectAllowed user MDSnd ct XMsgUpdate_
      cci <- withStore $ \db -> getDirectCIWithReactions db user ct itemId
      case cci of
        CChatItem SMDSnd ci@ChatItem {meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable}, content = ciContent} -> do
          case (ciContent, itemSharedMsgId, editable) of
            (CISndMsgContent oldMC, Just itemSharedMId, True) -> do
              let changed = mc /= oldMC
              if changed || fromMaybe False itemLive
                then do
                  (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XMsgUpdate itemSharedMId mc (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive))
                  ci' <- withStore' $ \db -> do
                    currentTs <- liftIO getCurrentTime
                    when changed $
                      addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
                    updateDirectChatItem' db user contactId ci (CISndMsgContent mc) live $ Just msgId
                  startUpdatedTimedItemThread user (ChatRef CTDirect contactId) ci ci'
                  pure $ CRChatItemUpdated user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci')
                else pure $ CRChatItemNotChanged user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTGroup -> do
      Group gInfo@GroupInfo {groupId} ms <- withStore $ \db -> getGroup db vr user chatId
      assertUserGroupRole gInfo GRAuthor
      cci <- withStore $ \db -> getGroupCIWithReactions db user gInfo itemId
      case cci of
        CChatItem SMDSnd ci@ChatItem {meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable}, content = ciContent} -> do
          case (ciContent, itemSharedMsgId, editable) of
            (CISndMsgContent oldMC, Just itemSharedMId, True) -> do
              let changed = mc /= oldMC
              if changed || fromMaybe False itemLive
                then do
                  (SndMessage {msgId}, _) <- sendGroupMessage user gInfo ms (XMsgUpdate itemSharedMId mc (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive))
                  ci' <- withStore' $ \db -> do
                    currentTs <- liftIO getCurrentTime
                    when changed $
                      addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
                    updateGroupChatItem db user groupId ci (CISndMsgContent mc) live $ Just msgId
                  startUpdatedTimedItemThread user (ChatRef CTGroup groupId) ci ci'
                  pure $ CRChatItemUpdated user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci')
                else pure $ CRChatItemNotChanged user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APIDeleteChatItem (ChatRef cType chatId) itemId mode -> withUser $ \user -> withChatLock "deleteChatItem" $ case cType of
    CTDirect -> do
      (ct, CChatItem msgDir ci@ChatItem {meta = CIMeta {itemSharedMsgId, editable}}) <- withStore $ \db -> (,) <$> getContact db user chatId <*> getDirectChatItem db user chatId itemId
      case (mode, msgDir, itemSharedMsgId, editable) of
        (CIDMInternal, _, _, _) -> deleteDirectCI user ct ci True False
        (CIDMBroadcast, SMDSnd, Just itemSharedMId, True) -> do
          assertDirectAllowed user MDSnd ct XMsgDel_
          (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XMsgDel itemSharedMId Nothing)
          if featureAllowed SCFFullDelete forUser ct
            then deleteDirectCI user ct ci True False
            else markDirectCIDeleted user ct ci msgId True =<< liftIO getCurrentTime
        (CIDMBroadcast, _, _, _) -> throwChatError CEInvalidChatItemDelete
    CTGroup -> do
      Group gInfo ms <- withStore $ \db -> getGroup db vr user chatId
      CChatItem msgDir ci@ChatItem {meta = CIMeta {itemSharedMsgId, editable}} <- withStore $ \db -> getGroupChatItem db user chatId itemId
      case (mode, msgDir, itemSharedMsgId, editable) of
        (CIDMInternal, _, _, _) -> deleteGroupCI user gInfo ci True False Nothing =<< liftIO getCurrentTime
        (CIDMBroadcast, SMDSnd, Just itemSharedMId, True) -> do
          assertUserGroupRole gInfo GRObserver -- can still delete messages sent earlier
          (SndMessage {msgId}, _) <- sendGroupMessage user gInfo ms $ XMsgDel itemSharedMId Nothing
          delGroupChatItem user gInfo ci msgId Nothing
        (CIDMBroadcast, _, _, _) -> throwChatError CEInvalidChatItemDelete
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APIDeleteMemberChatItem gId mId itemId -> withUser $ \user -> withChatLock "deleteChatItem" $ do
    Group gInfo@GroupInfo {membership} ms <- withStore $ \db -> getGroup db vr user gId
    CChatItem _ ci@ChatItem {chatDir, meta = CIMeta {itemSharedMsgId}} <- withStore $ \db -> getGroupChatItem db user gId itemId
    case (chatDir, itemSharedMsgId) of
      (CIGroupRcv GroupMember {groupMemberId, memberRole, memberId}, Just itemSharedMId) -> do
        when (groupMemberId /= mId) $ throwChatError CEInvalidChatItemDelete
        assertUserGroupRole gInfo $ max GRAdmin memberRole
        (SndMessage {msgId}, _) <- sendGroupMessage user gInfo ms $ XMsgDel itemSharedMId $ Just memberId
        delGroupChatItem user gInfo ci msgId (Just membership)
      (_, _) -> throwChatError CEInvalidChatItemDelete
  APIChatItemReaction (ChatRef cType chatId) itemId add reaction -> withUser $ \user -> withChatLock "chatItemReaction" $ case cType of
    CTDirect ->
      withStore (\db -> (,) <$> getContact db user chatId <*> getDirectChatItem db user chatId itemId) >>= \case
        (ct, CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}}) -> do
          unless (featureAllowed SCFReactions forUser ct) $
            throwChatError (CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions))
          unless (ciReactionAllowed ci) $
            throwChatError (CECommandError "reaction not allowed - chat item has no content")
          rs <- withStore' $ \db -> getDirectReactions db ct itemSharedMId True
          checkReactionAllowed rs
          (SndMessage {msgId}, _) <- sendDirectContactMessage ct $ XMsgReact itemSharedMId Nothing reaction add
          createdAt <- liftIO getCurrentTime
          reactions <- withStore' $ \db -> do
            setDirectReaction db ct itemSharedMId True reaction add msgId createdAt
            liftIO $ getDirectCIReactions db ct itemSharedMId
          let ci' = CChatItem md ci {reactions}
              r = ACIReaction SCTDirect SMDSnd (DirectChat ct) $ CIReaction CIDirectSnd ci' createdAt reaction
          pure $ CRChatItemReaction user add r
        _ -> throwChatError $ CECommandError "reaction not possible - no shared item ID"
    CTGroup ->
      withStore (\db -> (,) <$> getGroup db vr user chatId <*> getGroupChatItem db user chatId itemId) >>= \case
        (Group g@GroupInfo {membership} ms, CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}}) -> do
          unless (groupFeatureAllowed SGFReactions g) $
            throwChatError (CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions))
          unless (ciReactionAllowed ci) $
            throwChatError (CECommandError "reaction not allowed - chat item has no content")
          let GroupMember {memberId = itemMemberId} = chatItemMember g ci
          rs <- withStore' $ \db -> getGroupReactions db g membership itemMemberId itemSharedMId True
          checkReactionAllowed rs
          (SndMessage {msgId}, _) <- sendGroupMessage user g ms (XMsgReact itemSharedMId (Just itemMemberId) reaction add)
          createdAt <- liftIO getCurrentTime
          reactions <- withStore' $ \db -> do
            setGroupReaction db g membership itemMemberId itemSharedMId True reaction add msgId createdAt
            liftIO $ getGroupCIReactions db g itemMemberId itemSharedMId
          let ci' = CChatItem md ci {reactions}
              r = ACIReaction SCTGroup SMDSnd (GroupChat g) $ CIReaction CIGroupSnd ci' createdAt reaction
          pure $ CRChatItemReaction user add r
        _ -> throwChatError $ CECommandError "reaction not possible - no shared item ID"
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    where
      checkReactionAllowed rs = do
        when ((reaction `elem` rs) == add) $
          throwChatError (CECommandError $ "reaction already " <> if add then "added" else "removed")
        when (add && length rs >= maxMsgReactions) $
          throwChatError (CECommandError "too many reactions")
  APIUserRead userId -> withUserId userId $ \user -> withStore' (`setUserChatsRead` user) >> ok user
  UserRead -> withUser $ \User {userId} -> processChatCommand $ APIUserRead userId
  APIChatRead (ChatRef cType chatId) fromToIds -> withUser $ \_ -> case cType of
    CTDirect -> do
      user <- withStore $ \db -> getUserByContactId db chatId
      timedItems <- withStore' $ \db -> getDirectUnreadTimedItems db user chatId fromToIds
      ts <- liftIO getCurrentTime
      forM_ timedItems $ \(itemId, ttl) -> do
        let deleteAt = addUTCTime (realToFrac ttl) ts
        withStore' $ \db -> setDirectChatItemDeleteAt db user chatId itemId deleteAt
        startProximateTimedItemThread user (ChatRef CTDirect chatId, itemId) deleteAt
      withStore' $ \db -> updateDirectChatItemsRead db user chatId fromToIds
      ok user
    CTGroup -> do
      user@User {userId} <- withStore $ \db -> getUserByGroupId db chatId
      timedItems <- withStore' $ \db -> getGroupUnreadTimedItems db user chatId fromToIds
      ts <- liftIO getCurrentTime
      forM_ timedItems $ \(itemId, ttl) -> do
        let deleteAt = addUTCTime (realToFrac ttl) ts
        withStore' $ \db -> setGroupChatItemDeleteAt db user chatId itemId deleteAt
        startProximateTimedItemThread user (ChatRef CTGroup chatId, itemId) deleteAt
      withStore' $ \db -> updateGroupChatItemsRead db userId chatId fromToIds
      ok user
    CTContactRequest -> pure $ chatCmdError Nothing "not supported"
    CTContactConnection -> pure $ chatCmdError Nothing "not supported"
  APIChatUnread (ChatRef cType chatId) unreadChat -> withUser $ \user -> case cType of
    CTDirect -> do
      withStore $ \db -> do
        ct <- getContact db user chatId
        liftIO $ updateContactUnreadChat db user ct unreadChat
      ok user
    CTGroup -> do
      withStore $ \db -> do
        Group {groupInfo} <- getGroup db vr user chatId
        liftIO $ updateGroupUnreadChat db user groupInfo unreadChat
      ok user
    _ -> pure $ chatCmdError (Just user) "not supported"
  APIDeleteChat (ChatRef cType chatId) notify -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct <- withStore $ \db -> getContact db user chatId
      filesInfo <- withStore' $ \db -> getContactFileInfo db user ct
      withChatLock "deleteChat direct" . procCmd $ do
        deleteFilesAndConns user filesInfo
        when (contactReady ct && contactActive ct && notify) $
          void (sendDirectContactMessage ct XDirectDel) `catchChatError` const (pure ())
        contactConnIds <- map aConnId <$> withStore' (\db -> getContactConnections db userId ct)
        deleteAgentConnectionsAsync user contactConnIds
        -- functions below are called in separate transactions to prevent crashes on android
        -- (possibly, race condition on integrity check?)
        withStore' $ \db -> deleteContactConnectionsAndFiles db userId ct
        withStore' $ \db -> deleteContact db user ct
        pure $ CRContactDeleted user ct
    CTContactConnection -> withChatLock "deleteChat contactConnection" . procCmd $ do
      conn@PendingContactConnection {pccAgentConnId = AgentConnId acId} <- withStore $ \db -> getPendingContactConnection db userId chatId
      deleteAgentConnectionAsync user acId
      withStore' $ \db -> deletePendingContactConnection db userId chatId
      pure $ CRContactConnectionDeleted user conn
    CTGroup -> do
      Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db vr user chatId
      let isOwner = membership.memberRole == GROwner
          canDelete = isOwner || not (memberCurrent membership)
      unless canDelete $ throwChatError $ CEGroupUserRole gInfo GROwner
      filesInfo <- withStore' $ \db -> getGroupFileInfo db user gInfo
      withChatLock "deleteChat group" . procCmd $ do
        deleteFilesAndConns user filesInfo
        when (memberActive membership && isOwner) . void $ sendGroupMessage user gInfo members XGrpDel
        deleteGroupLinkIfExists user gInfo
        deleteMembersConnections user members
        -- functions below are called in separate transactions to prevent crashes on android
        -- (possibly, race condition on integrity check?)
        withStore' $ \db -> deleteGroupConnectionsAndFiles db user gInfo members
        withStore' $ \db -> deleteGroupItemsAndMembers db user gInfo members
        withStore' $ \db -> deleteGroup db user gInfo
        let contactIds = mapMaybe memberContactId members
        deleteAgentConnectionsAsync user . concat =<< mapM deleteUnusedContact contactIds
        pure $ CRGroupDeletedUser user gInfo
      where
        deleteUnusedContact :: ContactId -> m [ConnId]
        deleteUnusedContact contactId =
          (withStore (\db -> getContact db user contactId) >>= delete)
            `catchChatError` (\e -> toView (CRChatError (Just user) e) $> [])
          where
            delete ct
              | directOrUsed ct = pure []
              | otherwise =
                  withStore' (\db -> checkContactHasGroups db user ct) >>= \case
                    Just _ -> pure []
                    Nothing -> do
                      conns <- withStore' $ \db -> getContactConnections db userId ct
                      withStore' (\db -> setContactDeleted db user ct)
                        `catchChatError` (toView . CRChatError (Just user))
                      pure $ map aConnId conns
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
  APIClearChat (ChatRef cType chatId) -> withUser $ \user -> case cType of
    CTDirect -> do
      ct <- withStore $ \db -> getContact db user chatId
      filesInfo <- withStore' $ \db -> getContactFileInfo db user ct
      deleteFilesAndConns user filesInfo
      withStore' $ \db -> deleteContactCIs db user ct
      pure $ CRChatCleared user (AChatInfo SCTDirect $ DirectChat ct)
    CTGroup -> do
      gInfo <- withStore $ \db -> getGroupInfo db vr user chatId
      filesInfo <- withStore' $ \db -> getGroupFileInfo db user gInfo
      deleteFilesAndConns user filesInfo
      withStore' $ \db -> deleteGroupCIs db user gInfo
      membersToDelete <- withStore' $ \db -> getGroupMembersForExpiration db user gInfo
      forM_ membersToDelete $ \m -> withStore' $ \db -> deleteGroupMember db user m
      pure $ CRChatCleared user (AChatInfo SCTGroup $ GroupChat gInfo)
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
  APIAcceptContact incognito connReqId -> withUser $ \_ -> withChatLock "acceptContact" $ do
    (user@User {userId}, cReq@UserContactRequest {userContactLinkId}) <- withStore $ \db -> getContactRequest' db connReqId
    ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
    let contactUsed = (\(_, groupId_, _) -> isNothing groupId_) ucl
    -- [incognito] generate profile to send, create connection with incognito profile
    incognitoProfile <- if incognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
    ct <- acceptContactRequest user cReq incognitoProfile contactUsed
    pure $ CRAcceptingContactRequest user ct
  APIRejectContact connReqId -> withUser $ \user -> withChatLock "rejectContact" $ do
    cReq@UserContactRequest {agentContactConnId = AgentConnId connId, agentInvitationId = AgentInvId invId} <-
      withStore $ \db ->
        getContactRequest db user connReqId
          `storeFinally` liftIO (deleteContactRequest db user connReqId)
    withAgent $ \a -> rejectContact a connId invId
    pure $ CRContactRequestRejected user cReq
  APISendCallInvitation contactId callType -> withUser $ \user -> do
    -- party initiating call
    ct <- withStore $ \db -> getContact db user contactId
    assertDirectAllowed user MDSnd ct XCallInv_
    if featureAllowed SCFCalls forUser ct
      then do
        calls <- asks currentCalls
        withChatLock "sendCallInvitation" $ do
          g <- asks random
          callId <- atomically $ CallId <$> C.randomBytes 16 g
          dhKeyPair <- atomically $ if encryptedCall callType then Just <$> C.generateKeyPair g else pure Nothing
          let invitation = CallInvitation {callType, callDhPubKey = fst <$> dhKeyPair}
              callState = CallInvitationSent {localCallType = callType, localDhPrivKey = snd <$> dhKeyPair}
          (msg, _) <- sendDirectContactMessage ct (XCallInv callId invitation)
          ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndCall CISCallPending 0)
          let call' = Call {contactId, callId, chatItemId = chatItemId' ci, callState, callTs = chatItemTs' ci}
          call_ <- atomically $ TM.lookupInsert contactId call' calls
          forM_ call_ $ \call -> updateCallItemStatus user ct call WCSDisconnected Nothing
          toView $ CRNewChatItem user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
          ok user
      else pure $ chatCmdError (Just user) ("feature not allowed " <> T.unpack (chatFeatureNameText CFCalls))
  SendCallInvitation cName callType -> withUser $ \user -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    processChatCommand $ APISendCallInvitation contactId callType
  APIRejectCall contactId ->
    -- party accepting call
    withCurrentCall contactId $ \user ct Call {chatItemId, callState} -> case callState of
      CallInvitationReceived {} -> do
        let aciContent = ACIContent SMDRcv $ CIRcvCall CISCallRejected 0
        withStore' $ \db -> updateDirectChatItemsRead db user contactId $ Just (chatItemId, chatItemId)
        updateDirectChatItemView user ct chatItemId aciContent False Nothing $> Nothing
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallOffer contactId WebRTCCallOffer {callType, rtcSession} ->
    -- party accepting call
    withCurrentCall contactId $ \user ct call@Call {callId, chatItemId, callState} -> case callState of
      CallInvitationReceived {peerCallType, localDhPubKey, sharedKey} -> do
        let callDhPubKey = if encryptedCall callType then localDhPubKey else Nothing
            offer = CallOffer {callType, rtcSession, callDhPubKey}
            callState' = CallOfferSent {localCallType = callType, peerCallType, localCallSession = rtcSession, sharedKey}
            aciContent = ACIContent SMDRcv $ CIRcvCall CISCallAccepted 0
        (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XCallOffer callId offer)
        withStore' $ \db -> updateDirectChatItemsRead db user contactId $ Just (chatItemId, chatItemId)
        updateDirectChatItemView user ct chatItemId aciContent False $ Just msgId
        pure $ Just call {callState = callState'}
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallAnswer contactId rtcSession ->
    -- party initiating call
    withCurrentCall contactId $ \user ct call@Call {callId, chatItemId, callState} -> case callState of
      CallOfferReceived {localCallType, peerCallType, peerCallSession, sharedKey} -> do
        let callState' = CallNegotiated {localCallType, peerCallType, localCallSession = rtcSession, peerCallSession, sharedKey}
            aciContent = ACIContent SMDSnd $ CISndCall CISCallNegotiated 0
        (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XCallAnswer callId CallAnswer {rtcSession})
        updateDirectChatItemView user ct chatItemId aciContent False $ Just msgId
        pure $ Just call {callState = callState'}
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallExtraInfo contactId rtcExtraInfo ->
    -- any call party
    withCurrentCall contactId $ \_ ct call@Call {callId, callState} -> case callState of
      CallOfferSent {localCallType, peerCallType, localCallSession, sharedKey} -> do
        -- TODO update the list of ice servers in localCallSession
        void . sendDirectContactMessage ct $ XCallExtra callId CallExtraInfo {rtcExtraInfo}
        let callState' = CallOfferSent {localCallType, peerCallType, localCallSession, sharedKey}
        pure $ Just call {callState = callState'}
      CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey} -> do
        -- TODO update the list of ice servers in localCallSession
        void . sendDirectContactMessage ct $ XCallExtra callId CallExtraInfo {rtcExtraInfo}
        let callState' = CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey}
        pure $ Just call {callState = callState'}
      _ -> throwChatError . CECallState $ callStateTag callState
  APIEndCall contactId ->
    -- any call party
    withCurrentCall contactId $ \user ct call@Call {callId} -> do
      (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XCallEnd callId)
      updateCallItemStatus user ct call WCSDisconnected $ Just msgId
      pure Nothing
  APIGetCallInvitations -> withUser $ \_ -> do
    calls <- asks currentCalls >>= readTVarIO
    let invs = mapMaybe callInvitation $ M.elems calls
    rcvCallInvitations <- rights <$> mapM rcvCallInvitation invs
    pure $ CRCallInvitations rcvCallInvitations
    where
      callInvitation Call {contactId, callState, callTs} = case callState of
        CallInvitationReceived {peerCallType, sharedKey} -> Just (contactId, callTs, peerCallType, sharedKey)
        _ -> Nothing
      rcvCallInvitation (contactId, callTs, peerCallType, sharedKey) = runExceptT . withStore $ \db -> do
        user <- getUserByContactId db contactId
        contact <- getContact db user contactId
        pure RcvCallInvitation {user, contact, callType = peerCallType, sharedKey, callTs}
  APIGetNetworkStatuses -> withUser $ \_ ->
    CRNetworkStatuses Nothing . map (uncurry ConnNetworkStatus) . M.toList <$> chatReadVar connNetworkStatuses
  APICallStatus contactId receivedStatus ->
    withCurrentCall contactId $ \user ct call ->
      updateCallItemStatus user ct call receivedStatus Nothing $> Just call
  APIUpdateProfile userId profile -> withUserId userId (`updateProfile` profile)
  APISetContactPrefs contactId prefs' -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    updateContactPrefs user ct prefs'
  APISetContactAlias contactId localAlias -> withUser $ \user@User {userId} -> do
    ct' <- withStore $ \db -> do
      ct <- getContact db user contactId
      liftIO $ updateContactAlias db userId ct localAlias
    pure $ CRContactAliasUpdated user ct'
  APISetConnectionAlias connId localAlias -> withUser $ \user@User {userId} -> do
    conn' <- withStore $ \db -> do
      conn <- getPendingContactConnection db userId connId
      liftIO $ updateContactConnectionAlias db userId conn localAlias
    pure $ CRConnectionAliasUpdated user conn'
  APIParseMarkdown text -> pure . CRApiParsedMarkdown $ parseMaybeMarkdownList text
  APIGetNtfToken -> withUser $ \_ -> crNtfToken <$> withAgent getNtfToken
  APIRegisterToken token mode -> withUser $ \_ ->
    CRNtfTokenStatus <$> withAgent (\a -> registerNtfToken a token mode)
  APIVerifyToken token nonce code -> withUser $ \_ -> withAgent (\a -> verifyNtfToken a token nonce code) >> ok_
  APIDeleteToken token -> withUser $ \_ -> withAgent (`deleteNtfToken` token) >> ok_
  APIGetNtfMessage nonce encNtfInfo -> withUser $ \_ -> do
    (NotificationInfo {ntfConnId, ntfMsgMeta}, msgs) <- withAgent $ \a -> getNotificationMessage a nonce encNtfInfo
    let msgTs' = systemToUTCTime . (\SMP.NMsgMeta {msgTs} -> msgTs) <$> ntfMsgMeta
        agentConnId = AgentConnId ntfConnId
    user_ <- withStore' (`getUserByAConnId` agentConnId)
    connEntity_ <-
      pure user_ $>>= \user ->
        withStore (\db -> Just <$> getConnectionEntity db vr user agentConnId) `catchChatError` (\e -> toView (CRChatError (Just user) e) $> Nothing)
    pure CRNtfMessages {user_, connEntity_, msgTs = msgTs', ntfMessages = map ntfMsgInfo msgs}
  APIGetUserProtoServers userId (AProtocolType p) -> withUserId userId $ \user -> withServerProtocol p $ do
    ChatConfig {defaultServers} <- asks config
    servers <- withStore' (`getProtocolServers` user)
    let defServers = cfgServers p defaultServers
        servers' = fromMaybe (L.map toServerCfg defServers) $ nonEmpty servers
    pure $ CRUserProtoServers user $ AUPS $ UserProtoServers p servers' defServers
    where
      toServerCfg server = ServerCfg {server, preset = True, tested = Nothing, enabled = True}
  GetUserProtoServers aProtocol -> withUser $ \User {userId} ->
    processChatCommand $ APIGetUserProtoServers userId aProtocol
  APISetUserProtoServers userId (APSC p (ProtoServersConfig servers)) -> withUserId userId $ \user -> withServerProtocol p $
    withChatLock "setUserSMPServers" $ do
      withStore $ \db -> overwriteProtocolServers db user servers
      cfg <- asks config
      withAgent $ \a -> setProtocolServers a (aUserId user) $ activeAgentServers cfg p servers
      ok user
  SetUserProtoServers serversConfig -> withUser $ \User {userId} ->
    processChatCommand $ APISetUserProtoServers userId serversConfig
  APITestProtoServer userId srv@(AProtoServerWithAuth p server) -> withUserId userId $ \user ->
    withServerProtocol p $
      CRServerTestResult user srv <$> withAgent (\a -> testProtocolServer a (aUserId user) server)
  TestProtoServer srv -> withUser $ \User {userId} ->
    processChatCommand $ APITestProtoServer userId srv
  APISetChatItemTTL userId newTTL_ -> withUserId userId $ \user ->
    checkStoreNotChanged $
      withChatLock "setChatItemTTL" $ do
        case newTTL_ of
          Nothing -> do
            withStore' $ \db -> setChatItemTTL db user newTTL_
            setExpireCIFlag user False
          Just newTTL -> do
            oldTTL <- withStore' (`getChatItemTTL` user)
            when (maybe True (newTTL <) oldTTL) $ do
              setExpireCIFlag user False
              expireChatItems user newTTL True
            withStore' $ \db -> setChatItemTTL db user newTTL_
            startExpireCIThread user
            whenM chatStarted $ setExpireCIFlag user True
        ok user
  SetChatItemTTL newTTL_ -> withUser' $ \User {userId} -> do
    processChatCommand $ APISetChatItemTTL userId newTTL_
  APIGetChatItemTTL userId -> withUserId' userId $ \user -> do
    ttl <- withStoreCtx' (Just "APIGetChatItemTTL, getChatItemTTL") (`getChatItemTTL` user)
    pure $ CRChatItemTTL user ttl
  GetChatItemTTL -> withUser' $ \User {userId} -> do
    processChatCommand $ APIGetChatItemTTL userId
  APISetNetworkConfig cfg -> withUser' $ \_ -> withAgent (`setNetworkConfig` cfg) >> ok_
  APIGetNetworkConfig -> withUser' $ \_ ->
    CRNetworkConfig <$> withAgent getNetworkConfig
  ReconnectAllServers -> withUser' $ \_ -> withAgent reconnectAllServers >> ok_
  APISetChatSettings (ChatRef cType chatId) chatSettings -> withUser $ \user -> case cType of
    CTDirect -> do
      ct <- withStore $ \db -> do
        ct <- getContact db user chatId
        liftIO $ updateContactSettings db user chatId chatSettings
        pure ct
      forM_ (contactConnId ct) $ \connId ->
        withAgent $ \a -> toggleConnectionNtfs a connId (chatHasNtfs chatSettings)
      ok user
    CTGroup -> do
      ms <- withStore $ \db -> do
        Group _ ms <- getGroup db vr user chatId
        liftIO $ updateGroupSettings db user chatId chatSettings
        pure ms
      forM_ (filter memberActive ms) $ \m -> forM_ (memberConnId m) $ \connId ->
        withAgent (\a -> toggleConnectionNtfs a connId $ chatHasNtfs chatSettings) `catchChatError` (toView . CRChatError (Just user))
      ok user
    _ -> pure $ chatCmdError (Just user) "not supported"
  APISetMemberSettings gId gMemberId settings -> withUser $ \user -> do
    m <- withStore $ \db -> do
      liftIO $ updateGroupMemberSettings db user gId gMemberId settings
      getGroupMember db user gId gMemberId
    when (memberActive m) $ forM_ (memberConnId m) $ \connId -> do
      let ntfOn = showMessages $ memberSettings m
      withAgent (\a -> toggleConnectionNtfs a connId ntfOn) `catchChatError` (toView . CRChatError (Just user))
    ok user
  APIContactInfo contactId -> withUser $ \user@User {userId} -> do
    -- [incognito] print user's incognito profile for this contact
    ct@Contact {activeConn} <- withStore $ \db -> getContact db user contactId
    incognitoProfile <- case activeConn of
      Nothing -> pure Nothing
      Just Connection {customUserProfileId} ->
        forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
    connectionStats <- mapM (withAgent . flip getConnectionServers) (contactConnId ct)
    pure $ CRContactInfo user ct connectionStats (fmap fromLocalProfile incognitoProfile)
  APIGroupInfo gId -> withUser $ \user -> do
    (g, s) <- withStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> liftIO (getGroupSummary db user gId)
    pure $ CRGroupInfo user g s
  APIGroupMemberInfo gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db user gId gMemberId
    connectionStats <- mapM (withAgent . flip getConnectionServers) (memberConnId m)
    pure $ CRGroupMemberInfo user g m connectionStats
  APISwitchContact contactId -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    case contactConnId ct of
      Just connId -> do
        connectionStats <- withAgent $ \a -> switchConnectionAsync a "" connId
        pure $ CRContactSwitchStarted user ct connectionStats
      Nothing -> throwChatError $ CEContactNotActive ct
  APISwitchGroupMember gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db user gId gMemberId
    case memberConnId m of
      Just connId -> do
        connectionStats <- withAgent (\a -> switchConnectionAsync a "" connId)
        pure $ CRGroupMemberSwitchStarted user g m connectionStats
      _ -> throwChatError CEGroupMemberNotActive
  APIAbortSwitchContact contactId -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    case contactConnId ct of
      Just connId -> do
        connectionStats <- withAgent $ \a -> abortConnectionSwitch a connId
        pure $ CRContactSwitchAborted user ct connectionStats
      Nothing -> throwChatError $ CEContactNotActive ct
  APIAbortSwitchGroupMember gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db user gId gMemberId
    case memberConnId m of
      Just connId -> do
        connectionStats <- withAgent $ \a -> abortConnectionSwitch a connId
        pure $ CRGroupMemberSwitchAborted user g m connectionStats
      _ -> throwChatError CEGroupMemberNotActive
  APISyncContactRatchet contactId force -> withUser $ \user -> withChatLock "syncContactRatchet" $ do
    ct <- withStore $ \db -> getContact db user contactId
    case contactConnId ct of
      Just connId -> do
        cStats@ConnectionStats {ratchetSyncState = rss} <- withAgent $ \a -> synchronizeRatchet a connId force
        createInternalChatItem user (CDDirectSnd ct) (CISndConnEvent $ SCERatchetSync rss Nothing) Nothing
        pure $ CRContactRatchetSyncStarted user ct cStats
      Nothing -> throwChatError $ CEContactNotActive ct
  APISyncGroupMemberRatchet gId gMemberId force -> withUser $ \user -> withChatLock "syncGroupMemberRatchet" $ do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db user gId gMemberId
    case memberConnId m of
      Just connId -> do
        cStats@ConnectionStats {ratchetSyncState = rss} <- withAgent $ \a -> synchronizeRatchet a connId force
        createInternalChatItem user (CDGroupSnd g) (CISndConnEvent . SCERatchetSync rss . Just $ groupMemberRef m) Nothing
        pure $ CRGroupMemberRatchetSyncStarted user g m cStats
      _ -> throwChatError CEGroupMemberNotActive
  APIGetContactCode contactId -> withUser $ \user -> do
    ct@Contact {activeConn} <- withStore $ \db -> getContact db user contactId
    case activeConn of
      Just conn@Connection {connId} -> do
        code <- getConnectionCode $ aConnId conn
        ct' <- case contactSecurityCode ct of
          Just SecurityCode {securityCode}
            | sameVerificationCode code securityCode -> pure ct
            | otherwise -> do
                withStore' $ \db -> setConnectionVerified db user connId Nothing
                pure (ct :: Contact) {activeConn = Just $ (conn :: Connection) {connectionCode = Nothing}}
          _ -> pure ct
        pure $ CRContactCode user ct' code
      Nothing -> throwChatError $ CEContactNotActive ct
  APIGetGroupMemberCode gId gMemberId -> withUser $ \user -> do
    (g, m@GroupMember {activeConn}) <- withStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db user gId gMemberId
    case activeConn of
      Just conn@Connection {connId} -> do
        code <- getConnectionCode $ aConnId conn
        m' <- case memberSecurityCode m of
          Just SecurityCode {securityCode}
            | sameVerificationCode code securityCode -> pure m
            | otherwise -> do
                withStore' $ \db -> setConnectionVerified db user connId Nothing
                pure (m :: GroupMember) {activeConn = Just $ (conn :: Connection) {connectionCode = Nothing}}
          _ -> pure m
        pure $ CRGroupMemberCode user g m' code
      _ -> throwChatError CEGroupMemberNotActive
  APIVerifyContact contactId code -> withUser $ \user -> do
    ct@Contact {activeConn} <- withStore $ \db -> getContact db user contactId
    case activeConn of
      Just conn -> verifyConnectionCode user conn code
      Nothing -> throwChatError $ CEContactNotActive ct
  APIVerifyGroupMember gId gMemberId code -> withUser $ \user -> do
    GroupMember {activeConn} <- withStore $ \db -> getGroupMember db user gId gMemberId
    case activeConn of
      Just conn -> verifyConnectionCode user conn code
      _ -> throwChatError CEGroupMemberNotActive
  APIEnableContact contactId -> withUser $ \user -> do
    ct@Contact {activeConn} <- withStore $ \db -> getContact db user contactId
    case activeConn of
      Just conn -> do
        withStore' $ \db -> setConnectionAuthErrCounter db user conn 0
        ok user
      Nothing -> throwChatError $ CEContactNotActive ct
  APIEnableGroupMember gId gMemberId -> withUser $ \user -> do
    GroupMember {activeConn} <- withStore $ \db -> getGroupMember db user gId gMemberId
    case activeConn of
      Just conn -> do
        withStore' $ \db -> setConnectionAuthErrCounter db user conn 0
        ok user
      _ -> throwChatError CEGroupMemberNotActive
  SetShowMessages cName ntfOn -> updateChatSettings cName (\cs -> cs {enableNtfs = ntfOn})
  SetSendReceipts cName rcptsOn_ -> updateChatSettings cName (\cs -> cs {sendRcpts = rcptsOn_})
  SetShowMemberMessages gName mName showMessages -> withUser $ \user -> do
    (gId, mId) <- getGroupAndMemberId user gName mName
    m <- withStore $ \db -> getGroupMember db user gId mId
    let settings = (memberSettings m) {showMessages}
    processChatCommand $ APISetMemberSettings gId mId settings
  ContactInfo cName -> withContactName cName APIContactInfo
  ShowGroupInfo gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIGroupInfo groupId
  GroupMemberInfo gName mName -> withMemberName gName mName APIGroupMemberInfo
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
  APIAddContact userId incognito -> withUserId userId $ \user -> withChatLock "addContact" . procCmd $ do
    -- [incognito] generate profile for connection
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    subMode <- chatReadVar subscriptionMode
    (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing subMode
    conn <- withStore' $ \db -> createDirectConnection db user connId cReq ConnNew incognitoProfile subMode
    pure $ CRInvitation user cReq conn
  AddContact incognito -> withUser $ \User {userId} ->
    processChatCommand $ APIAddContact userId incognito
  APISetConnectionIncognito connId incognito -> withUser $ \user@User {userId} -> do
    conn'_ <- withStore $ \db -> do
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
  APIConnectPlan userId cReqUri -> withUserId userId $ \user ->
    withChatLock "connectPlan" . procCmd $
      CRConnectionPlan user <$> connectPlan user cReqUri
  APIConnect userId incognito (Just (ACR SCMInvitation cReq)) -> withUserId userId $ \user -> withChatLock "connect" . procCmd $ do
    subMode <- chatReadVar subscriptionMode
    -- [incognito] generate profile to send
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    let profileToSend = userProfileToSend user incognitoProfile Nothing
    dm <- directMessage $ XInfo profileToSend
    connId <- withAgent $ \a -> joinConnection a (aUserId user) True cReq dm subMode
    conn <- withStore' $ \db -> createDirectConnection db user connId cReq ConnJoined (incognitoProfile $> profileToSend) subMode
    pure $ CRSentConfirmation user conn
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
    ct@Contact {activeConn, profile = LocalProfile {contactLink}} <- withStore $ \db -> getContact db user contactId
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
  DeleteContact cName -> withContactName cName $ \ctId -> APIDeleteChat (ChatRef CTDirect ctId) True
  ClearContact cName -> withContactName cName $ APIClearChat . ChatRef CTDirect
  APIListContacts userId -> withUserId userId $ \user ->
    CRContactsList user <$> withStore' (`getUserContacts` user)
  ListContacts -> withUser $ \User {userId} ->
    processChatCommand $ APIListContacts userId
  APICreateMyAddress userId -> withUserId userId $ \user -> withChatLock "createMyAddress" . procCmd $ do
    subMode <- chatReadVar subscriptionMode
    (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMContact Nothing subMode
    withStore $ \db -> createUserContactLink db user connId cReq subMode
    pure $ CRUserContactLinkCreated user cReq
  CreateMyAddress -> withUser $ \User {userId} ->
    processChatCommand $ APICreateMyAddress userId
  APIDeleteMyAddress userId -> withUserId userId $ \user@User {profile = p} -> do
    conns <- withStore (`getUserAddressConnections` user)
    withChatLock "deleteMyAddress" $ do
      deleteAgentConnectionsAsync user $ map aConnId conns
      withStore' (`deleteUserAddress` user)
    let p' = (fromLocalProfile p :: Profile) {contactLink = Nothing}
    r <- updateProfile_ user p' $ withStore' $ \db -> setUserProfileContactLink db user Nothing
    let user' = case r of
          CRUserProfileUpdated u' _ _ _ -> u'
          _ -> user
    pure $ CRUserContactLinkDeleted user'
  DeleteMyAddress -> withUser $ \User {userId} ->
    processChatCommand $ APIDeleteMyAddress userId
  APIShowMyAddress userId -> withUserId' userId $ \user ->
    CRUserContactLink user <$> withStoreCtx (Just "APIShowMyAddress, getUserAddress") (`getUserAddress` user)
  ShowMyAddress -> withUser' $ \User {userId} ->
    processChatCommand $ APIShowMyAddress userId
  APISetProfileAddress userId False -> withUserId userId $ \user@User {profile = p} -> do
    let p' = (fromLocalProfile p :: Profile) {contactLink = Nothing}
    updateProfile_ user p' $ withStore' $ \db -> setUserProfileContactLink db user Nothing
  APISetProfileAddress userId True -> withUserId userId $ \user@User {profile = p} -> do
    ucl@UserContactLink {connReqContact} <- withStore (`getUserAddress` user)
    let p' = (fromLocalProfile p :: Profile) {contactLink = Just connReqContact}
    updateProfile_ user p' $ withStore' $ \db -> setUserProfileContactLink db user $ Just ucl
  SetProfileAddress onOff -> withUser $ \User {userId} ->
    processChatCommand $ APISetProfileAddress userId onOff
  APIAddressAutoAccept userId autoAccept_ -> withUserId userId $ \user -> do
    contactLink <- withStore (\db -> updateUserAddressAutoAccept db user autoAccept_)
    pure $ CRUserContactLinkUpdated user contactLink
  AddressAutoAccept autoAccept_ -> withUser $ \User {userId} ->
    processChatCommand $ APIAddressAutoAccept userId autoAccept_
  AcceptContact incognito cName -> withUser $ \User {userId} -> do
    connReqId <- withStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand $ APIAcceptContact incognito connReqId
  RejectContact cName -> withUser $ \User {userId} -> do
    connReqId <- withStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand $ APIRejectContact connReqId
  SendMessage (ChatName cType name) msg -> withUser $ \user -> do
    let mc = MCText msg
    case cType of
      CTDirect ->
        withStore' (\db -> runExceptT $ getContactIdByName db user name) >>= \case
          Right ctId -> do
            let chatRef = ChatRef CTDirect ctId
            processChatCommand . APISendMessage chatRef False Nothing $ ComposedMessage Nothing Nothing mc
          Left _ ->
            withStore' (\db -> runExceptT $ getActiveMembersByName db vr user name) >>= \case
              Right [(gInfo, member)] -> do
                let GroupInfo {localDisplayName = gName} = gInfo
                    GroupMember {localDisplayName = mName} = member
                processChatCommand $ SendMemberContactMessage gName mName msg
              Right (suspectedMember : _) ->
                throwChatError $ CEContactNotFound name (Just suspectedMember)
              _ ->
                throwChatError $ CEContactNotFound name Nothing
      CTGroup -> do
        gId <- withStore $ \db -> getGroupIdByName db user name
        let chatRef = ChatRef CTGroup gId
        processChatCommand . APISendMessage chatRef False Nothing $ ComposedMessage Nothing Nothing mc
      _ -> throwChatError $ CECommandError "not supported"
  SendMemberContactMessage gName mName msg -> withUser $ \user -> do
    (gId, mId) <- getGroupAndMemberId user gName mName
    m <- withStore $ \db -> getGroupMember db user gId mId
    let mc = MCText msg
    case memberContactId m of
      Nothing -> do
        gInfo <- withStore $ \db -> getGroupInfo db vr user gId
        toView $ CRNoMemberContactCreating user gInfo m
        processChatCommand (APICreateMemberContact gId mId) >>= \case
          cr@(CRNewMemberContact _ Contact {contactId} _ _) -> do
            toView cr
            processChatCommand $ APISendMemberContactInvitation contactId (Just mc)
          cr -> pure cr
      Just ctId -> do
        let chatRef = ChatRef CTDirect ctId
        processChatCommand . APISendMessage chatRef False Nothing $ ComposedMessage Nothing Nothing mc
  SendLiveMessage chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    let mc = MCText msg
    processChatCommand . APISendMessage chatRef True Nothing $ ComposedMessage Nothing Nothing mc
  SendMessageBroadcast msg -> withUser $ \user -> do
    contacts <- withStore' (`getUserContacts` user)
    let cts = filter (\ct -> contactReady ct && contactActive ct && directOrUsed ct) contacts
    ChatConfig {logLevel} <- asks config
    withChatLock "sendMessageBroadcast" . procCmd $ do
      (successes, failures) <- foldM (sendAndCount user logLevel) (0, 0) cts
      timestamp <- liftIO getCurrentTime
      pure CRBroadcastSent {user, msgContent = mc, successes, failures, timestamp}
    where
      mc = MCText msg
      sendAndCount user ll (s, f) ct =
        (sendToContact user ct $> (s + 1, f)) `catchChatError` \e -> when (ll <= CLLInfo) (toView $ CRChatError (Just user) e) $> (s, f + 1)
      sendToContact user ct = do
        (sndMsg, _) <- sendDirectContactMessage ct (XMsgNew $ MCSimple (extMsgContent mc Nothing))
        void $ saveSndChatItem user (CDDirectSnd ct) sndMsg (CISndMsgContent mc)
  SendMessageQuote cName (AMsgDirection msgDir) quotedMsg msg -> withUser $ \user@User {userId} -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    quotedItemId <- withStore $ \db -> getDirectChatItemIdByText db userId contactId msgDir quotedMsg
    let mc = MCText msg
    processChatCommand . APISendMessage (ChatRef CTDirect contactId) False Nothing $ ComposedMessage Nothing (Just quotedItemId) mc
  DeleteMessage chatName deletedMsg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    deletedItemId <- getSentChatItemIdByText user chatRef deletedMsg
    processChatCommand $ APIDeleteChatItem chatRef deletedItemId CIDMBroadcast
  DeleteMemberMessage gName mName deletedMsg -> withUser $ \user -> do
    (gId, mId) <- getGroupAndMemberId user gName mName
    deletedItemId <- withStore $ \db -> getGroupChatItemIdByText db user gId (Just mName) deletedMsg
    processChatCommand $ APIDeleteMemberChatItem gId mId deletedItemId
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
    groupInfo <- withStore $ \db -> createNewGroup db vr gVar user gProfile incognitoProfile
    pure $ CRGroupCreated user groupInfo
  NewGroup incognito gProfile -> withUser $ \User {userId} ->
    processChatCommand $ APINewGroup userId incognito gProfile
  APIAddMember groupId contactId memRole -> withUser $ \user -> withChatLock "addMember" $ do
    -- TODO for large groups: no need to load all members to determine if contact is a member
    (group, contact) <- withStore $ \db -> (,) <$> getGroup db vr user groupId <*> getContact db user contactId
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
        (agentConnId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing subMode
        member <- withStore $ \db -> createNewContactMember db gVar user gInfo contact memRole agentConnId cReq subMode
        sendInvitation member cReq
        pure $ CRSentGroupInvitation user gInfo contact member
      Just member@GroupMember {groupMemberId, memberStatus, memberRole = mRole}
        | memberStatus == GSMemInvited -> do
            unless (mRole == memRole) $ withStore' $ \db -> updateGroupMemberRole db user member memRole
            withStore' (\db -> getMemberInvitation db user groupMemberId) >>= \case
              Just cReq -> do
                sendInvitation member {memberRole = memRole} cReq
                pure $ CRSentGroupInvitation user gInfo contact member {memberRole = memRole}
              Nothing -> throwChatError $ CEGroupCantResendInvitation gInfo cName
        | otherwise -> throwChatError $ CEGroupDuplicateMember cName
  APIJoinGroup groupId -> withUser $ \user@User {userId} -> do
    withChatLock "joinGroup" . procCmd $ do
      (invitation, ct) <- withStore $ \db -> do
        inv@ReceivedGroupInvitation {fromMember} <- getGroupInvitation db vr user groupId
        (inv,) <$> getContactViaMember db user fromMember
      let ReceivedGroupInvitation {fromMember, connRequest, groupInfo = g@GroupInfo {membership}} = invitation
          Contact {activeConn} = ct
      case activeConn of
        Just Connection {peerChatVRange} -> do
          subMode <- chatReadVar subscriptionMode
          dm <- directMessage $ XGrpAcpt membership.memberId
          agentConnId <- withAgent $ \a -> joinConnection a (aUserId user) True connRequest dm subMode
          withStore' $ \db -> do
            createMemberConnection db userId fromMember agentConnId (fromJVersionRange peerChatVRange) subMode
            updateGroupMemberStatus db userId fromMember GSMemAccepted
            updateGroupMemberStatus db userId membership GSMemAccepted
          updateCIGroupInvitationStatus user
          pure $ CRUserAcceptedGroupSent user g {membership = membership {memberStatus = GSMemAccepted}} Nothing
        Nothing -> throwChatError $ CEContactNotActive ct
    where
      updateCIGroupInvitationStatus user = do
        AChatItem _ _ cInfo ChatItem {content, meta = CIMeta {itemId}} <- withStore $ \db -> getChatItemByGroupId db vr user groupId
        case (cInfo, content) of
          (DirectChat ct, CIRcvGroupInvitation ciGroupInv memRole) -> do
            let aciContent = ACIContent SMDRcv $ CIRcvGroupInvitation ciGroupInv {status = CIGISAccepted} memRole
            updateDirectChatItemView user ct itemId aciContent False Nothing
          _ -> pure () -- prohibited
  APIMemberRole groupId memberId memRole -> withUser $ \user -> do
    Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db vr user groupId
    if memberId == groupMemberId' membership
      then changeMemberRole user gInfo members membership $ SGEUserRole memRole
      else case find ((== memberId) . groupMemberId') members of
        Just m -> changeMemberRole user gInfo members m $ SGEMemberRole memberId (fromLocalProfile $ memberProfile m) memRole
        _ -> throwChatError CEGroupMemberNotFound
    where
      changeMemberRole user gInfo members m gEvent = do
        let GroupMember {memberId = mId, memberRole = mRole, memberStatus = mStatus, memberContactId, localDisplayName = cName} = m
        assertUserGroupRole gInfo $ maximum [GRAdmin, mRole, memRole]
        withChatLock "memberRole" . procCmd $ do
          unless (mRole == memRole) $ do
            withStore' $ \db -> updateGroupMemberRole db user m memRole
            case mStatus of
              GSMemInvited -> do
                withStore (\db -> (,) <$> mapM (getContact db user) memberContactId <*> liftIO (getMemberInvitation db user $ groupMemberId' m)) >>= \case
                  (Just ct, Just cReq) -> sendGrpInvitation user ct gInfo (m :: GroupMember) {memberRole = memRole} cReq
                  _ -> throwChatError $ CEGroupCantResendInvitation gInfo cName
              _ -> do
                (msg, _) <- sendGroupMessage user gInfo members $ XGrpMemRole mId memRole
                ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent gEvent)
                toView $ CRNewChatItem user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
          pure CRMemberRoleUser {user, groupInfo = gInfo, member = m {memberRole = memRole}, fromRole = mRole, toRole = memRole}
  APIRemoveMember groupId memberId -> withUser $ \user -> do
    Group gInfo members <- withStore $ \db -> getGroup db vr user groupId
    case find ((== memberId) . groupMemberId') members of
      Nothing -> throwChatError CEGroupMemberNotFound
      Just m@GroupMember {memberId = mId, memberRole = mRole, memberStatus = mStatus, memberProfile} -> do
        assertUserGroupRole gInfo $ max GRAdmin mRole
        withChatLock "removeMember" . procCmd $ do
          case mStatus of
            GSMemInvited -> do
              deleteMemberConnection user m
              withStore' $ \db -> deleteGroupMember db user m
            _ -> do
              (msg, _) <- sendGroupMessage user gInfo members $ XGrpMemDel mId
              ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent $ SGEMemberDeleted memberId (fromLocalProfile memberProfile))
              toView $ CRNewChatItem user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
              deleteMemberConnection user m
              -- undeleted "member connected" chat item will prevent deletion of member record
              deleteOrUpdateMemberRecord user m
          pure $ CRUserDeletedMember user gInfo m {memberStatus = GSMemRemoved}
  APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
    Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db vr user groupId
    withChatLock "leaveGroup" . procCmd $ do
      (msg, _) <- sendGroupMessage user gInfo members XGrpLeave
      ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent SGEUserLeft)
      toView $ CRNewChatItem user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
      -- TODO delete direct connections that were unused
      deleteGroupLinkIfExists user gInfo
      -- member records are not deleted to keep history
      deleteMembersConnections user members
      withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemLeft
      pure $ CRLeftMemberUser user gInfo {membership = membership {memberStatus = GSMemLeft}}
  APIListMembers groupId -> withUser $ \user ->
    CRGroupMembers user <$> withStore (\db -> getGroup db vr user groupId)
  AddMember gName cName memRole -> withUser $ \user -> do
    (groupId, contactId) <- withStore $ \db -> (,) <$> getGroupIdByName db user gName <*> getContactIdByName db user cName
    processChatCommand $ APIAddMember groupId contactId memRole
  JoinGroup gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIJoinGroup groupId
  MemberRole gName gMemberName memRole -> withMemberName gName gMemberName $ \gId gMemberId -> APIMemberRole gId gMemberId memRole
  RemoveMember gName gMemberName -> withMemberName gName gMemberName APIRemoveMember
  LeaveGroup gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APILeaveGroup groupId
  DeleteGroup gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIDeleteChat (ChatRef CTGroup groupId) True
  ClearGroup gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIClearChat (ChatRef CTGroup groupId)
  ListMembers gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIListMembers groupId
  APIListGroups userId contactId_ search_ -> withUserId userId $ \user ->
    CRGroupsList user <$> withStore' (\db -> getUserGroupsWithSummary db vr user contactId_ search_)
  ListGroups cName_ search_ -> withUser $ \user@User {userId} -> do
    ct_ <- forM cName_ $ \cName -> withStore $ \db -> getContactByName db user cName
    processChatCommand $ APIListGroups userId (contactId' <$> ct_) search_
  APIUpdateGroupProfile groupId p' -> withUser $ \user -> do
    g <- withStore $ \db -> getGroup db vr user groupId
    runUpdateGroupProfile user g p'
  UpdateGroupNames gName GroupProfile {displayName, fullName} ->
    updateGroupProfileByName gName $ \p -> p {displayName, fullName}
  ShowGroupProfile gName -> withUser $ \user ->
    CRGroupProfile user <$> withStore (\db -> getGroupInfoByName db vr user gName)
  UpdateGroupDescription gName description ->
    updateGroupProfileByName gName $ \p -> p {description}
  ShowGroupDescription gName -> withUser $ \user ->
    CRGroupDescription user <$> withStore (\db -> getGroupInfoByName db vr user gName)
  APICreateGroupLink groupId mRole -> withUser $ \user -> withChatLock "createGroupLink" $ do
    gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
    assertUserGroupRole gInfo GRAdmin
    when (mRole > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole
    groupLinkId <- GroupLinkId <$> drgRandomBytes 16
    subMode <- chatReadVar subscriptionMode
    let crClientData = encodeJSON $ CRDataGroup groupLinkId
    (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMContact (Just crClientData) subMode
    withStore $ \db -> createGroupLink db user gInfo connId cReq groupLinkId mRole subMode
    pure $ CRGroupLinkCreated user gInfo cReq mRole
  APIGroupLinkMemberRole groupId mRole' -> withUser $ \user -> withChatLock "groupLinkMemberRole " $ do
    gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
    (groupLinkId, groupLink, mRole) <- withStore $ \db -> getGroupLink db user gInfo
    assertUserGroupRole gInfo GRAdmin
    when (mRole' > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole'
    when (mRole' /= mRole) $ withStore' $ \db -> setGroupLinkMemberRole db user groupLinkId mRole'
    pure $ CRGroupLink user gInfo groupLink mRole'
  APIDeleteGroupLink groupId -> withUser $ \user -> withChatLock "deleteGroupLink" $ do
    gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
    deleteGroupLink' user gInfo
    pure $ CRGroupLinkDeleted user gInfo
  APIGetGroupLink groupId -> withUser $ \user -> do
    gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
    (_, groupLink, mRole) <- withStore $ \db -> getGroupLink db user gInfo
    pure $ CRGroupLink user gInfo groupLink mRole
  APICreateMemberContact gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db user gId gMemberId
    assertUserGroupRole g GRAuthor
    unless (groupFeatureAllowed SGFDirectMessages g) $ throwChatError $ CECommandError "direct messages not allowed"
    case memberConn m of
      Just mConn@Connection {peerChatVRange} -> do
        unless (isCompatibleRange (fromJVersionRange peerChatVRange) xGrpDirectInvVRange) $ throwChatError CEPeerChatVRangeIncompatible
        when (isJust $ memberContactId m) $ throwChatError $ CECommandError "member contact already exists"
        subMode <- chatReadVar subscriptionMode
        (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing subMode
        -- [incognito] reuse membership incognito profile
        ct <- withStore' $ \db -> createMemberContact db user connId cReq g m mConn subMode
        -- TODO not sure it is correct to set connections status here?
        setContactNetworkStatus ct NSConnected
        pure $ CRNewMemberContact user ct g m
      _ -> throwChatError CEGroupMemberNotActive
  APISendMemberContactInvitation contactId msgContent_ -> withUser $ \user -> do
    (g, m, ct, cReq) <- withStore $ \db -> getMemberContact db vr user contactId
    when (contactGrpInvSent ct) $ throwChatError $ CECommandError "x.grp.direct.inv already sent"
    case memberConn m of
      Just mConn -> do
        let msg = XGrpDirectInv cReq msgContent_
        (sndMsg, _) <- sendDirectMessage mConn msg (GroupId $ g.groupId)
        withStore' $ \db -> setContactGrpInvSent db ct True
        let ct' = ct {contactGrpInvSent = True}
        forM_ msgContent_ $ \mc -> do
          ci <- saveSndChatItem user (CDDirectSnd ct') sndMsg (CISndMsgContent mc)
          toView $ CRNewChatItem user (AChatItem SCTDirect SMDSnd (DirectChat ct') ci)
        pure $ CRNewMemberContactSentInv user ct' g m
      _ -> throwChatError CEGroupMemberNotActive
  CreateGroupLink gName mRole -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APICreateGroupLink groupId mRole
  GroupLinkMemberRole gName mRole -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIGroupLinkMemberRole groupId mRole
  DeleteGroupLink gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIDeleteGroupLink groupId
  ShowGroupLink gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIGetGroupLink groupId
  SendGroupMessageQuote gName cName quotedMsg msg -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    quotedItemId <- withStore $ \db -> getGroupChatItemIdByText db user groupId cName quotedMsg
    let mc = MCText msg
    processChatCommand . APISendMessage (ChatRef CTGroup groupId) False Nothing $ ComposedMessage Nothing (Just quotedItemId) mc
  LastChats count_ -> withUser' $ \user -> do
    let count = fromMaybe 5000 count_
    (errs, previews) <- partitionEithers <$> withStore' (\db -> getChatPreviews db vr user False (PTLast count) clqNoFilters)
    unless (null errs) $ toView $ CRChatErrors (Just user) (map ChatErrorStore errs)
    pure $ CRChats previews
  LastMessages (Just chatName) count search -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand $ APIGetChat chatRef (CPLast count) search
    pure $ CRChatItems user (Just chatName) (aChatItems . chat $ chatResp)
  LastMessages Nothing count search -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db vr user (CPLast count) search
    pure $ CRChatItems user Nothing chatItems
  LastChatItemId (Just chatName) index -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand (APIGetChat chatRef (CPLast $ index + 1) Nothing)
    pure $ CRChatItemId user (fmap aChatItemId . listToMaybe . aChatItems . chat $ chatResp)
  LastChatItemId Nothing index -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db vr user (CPLast $ index + 1) Nothing
    pure $ CRChatItemId user (fmap aChatItemId . listToMaybe $ chatItems)
  ShowChatItem (Just itemId) -> withUser $ \user -> do
    chatItem <- withStore $ \db -> do
      chatRef <- getChatRefViaItemId db user itemId
      getAChatItem db vr user chatRef itemId
    pure $ CRChatItems user Nothing ((: []) chatItem)
  ShowChatItem Nothing -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db vr user (CPLast 1) Nothing
    pure $ CRChatItems user Nothing chatItems
  ShowChatItemInfo chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    itemId <- getChatItemIdByText user chatRef msg
    processChatCommand $ APIGetChatItemInfo chatRef itemId
  ShowLiveItems on -> withUser $ \_ ->
    asks showLiveItems >>= atomically . (`writeTVar` on) >> ok_
  SendFile chatName f -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    processChatCommand . APISendMessage chatRef False Nothing $ ComposedMessage (Just f) Nothing (MCFile "")
  SendImage chatName f@(CryptoFile fPath _) -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    filePath <- toFSFilePath fPath
    unless (any (`isSuffixOf` map toLower fPath) imageExtensions) $ throwChatError CEFileImageType {filePath}
    fileSize <- getFileSize filePath
    unless (fileSize <= maxImageSize) $ throwChatError CEFileImageSize {filePath}
    -- TODO include file description for preview
    processChatCommand . APISendMessage chatRef False Nothing $ ComposedMessage (Just f) Nothing (MCImage "" fixedImagePreview)
  ForwardFile chatName fileId -> forwardFile chatName fileId SendFile
  ForwardImage chatName fileId -> forwardFile chatName fileId SendImage
  SendFileDescription _chatName _f -> pure $ chatCmdError Nothing "TODO"
  ReceiveFile fileId encrypted_ rcvInline_ filePath_ -> withUser $ \_ ->
    withChatLock "receiveFile" . procCmd $ do
      (user, ft) <- withStore (`getRcvFileTransferById` fileId)
      encrypt <- (`fromMaybe` encrypted_) <$> chatReadVar encryptLocalFiles
      ft' <- (if encrypt then setFileToEncrypt else pure) ft
      receiveFile' user ft' rcvInline_ filePath_
  SetFileToReceive fileId encrypted_ -> withUser $ \_ -> do
    withChatLock "setFileToReceive" . procCmd $ do
      encrypt <- (`fromMaybe` encrypted_) <$> chatReadVar encryptLocalFiles
      cfArgs <- if encrypt then Just <$> (atomically . CF.randomArgs =<< asks random) else pure Nothing
      withStore' $ \db -> setRcvFileToReceive db fileId cfArgs
      ok_
  CancelFile fileId -> withUser $ \user@User {userId} ->
    withChatLock "cancelFile" . procCmd $
      withStore (\db -> getFileTransfer db user fileId) >>= \case
        FTSnd ftm@FileTransferMeta {xftpSndFile, cancelled} fts
          | cancelled -> throwChatError $ CEFileCancel fileId "file already cancelled"
          | not (null fts) && all fileCancelledOrCompleteSMP fts ->
              throwChatError $ CEFileCancel fileId "file transfer is complete"
          | otherwise -> do
              fileAgentConnIds <- cancelSndFile user ftm fts True
              deleteAgentConnectionsAsync user fileAgentConnIds
              sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
              withStore (\db -> getChatRefByFileId db user fileId) >>= \case
                ChatRef CTDirect contactId -> do
                  contact <- withStore $ \db -> getContact db user contactId
                  void . sendDirectContactMessage contact $ XFileCancel sharedMsgId
                ChatRef CTGroup groupId -> do
                  Group gInfo ms <- withStore $ \db -> getGroup db vr user groupId
                  void . sendGroupMessage user gInfo ms $ XFileCancel sharedMsgId
                _ -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
              ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
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
                ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
                pure $ CRRcvFileCancelled user ci ftr
              Just XFTPRcvFile {agentRcvFileId} -> do
                forM_ (liveRcvFileTransferPath ftr) $ \filePath -> do
                  fsFilePath <- toFSFilePath filePath
                  liftIO $ removeFile fsFilePath `catchAll_` pure ()
                forM_ agentRcvFileId $ \(AgentRcvFileId aFileId) ->
                  withAgent (`xftpDeleteRcvFile` aFileId)
                ci <- withStore $ \db -> do
                  liftIO $ do
                    updateCIFileStatus db user fileId CIFSRcvInvitation
                    updateRcvFileStatus db fileId FSNew
                    updateRcvFileAgentId db fileId Nothing
                  getChatItemByFileId db vr user fileId
                pure $ CRRcvFileCancelled user ci ftr
  FileStatus fileId -> withUser $ \user -> do
    ci@(AChatItem _ _ _ ChatItem {file}) <- withStore $ \db -> getChatItemByFileId db vr user fileId
    case file of
      Just CIFile {fileProtocol = FPXFTP} ->
        pure $ CRFileTransferStatusXFTP user ci
      _ -> do
        fileStatus <- withStore $ \db -> getFileTransferProgress db user fileId
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
    ct@Contact {userPreferences} <- withStore $ \db -> getContactByName db user cName
    let prefs' = setPreference f allowed_ $ Just userPreferences
    updateContactPrefs user ct prefs'
  SetGroupFeature (AGF f) gName enabled ->
    updateGroupProfileByName gName $ \p ->
      p {groupPreferences = Just . setGroupPreference f enabled $ groupPreferences p}
  SetUserTimedMessages onOff -> withUser $ \user@User {profile} -> do
    let allowed = if onOff then FAYes else FANo
        pref = TimedMessagesPreference allowed Nothing
        p = (fromLocalProfile profile :: Profile) {preferences = Just . setPreference' SCFTimedMessages (Just pref) $ preferences' user}
    updateProfile user p
  SetContactTimedMessages cName timedMessagesEnabled_ -> withUser $ \user -> do
    ct@Contact {userPreferences = userPreferences@Preferences {timedMessages}} <- withStore $ \db -> getContactByName db user cName
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
  QuitChat -> liftIO exitSuccess
  ShowVersion -> do
    -- simplexmqCommitQ makes iOS builds crash m(
    let versionInfo = coreVersionInfo ""
    chatMigrations <- map upMigration <$> withStore' (Migrations.getCurrent . DB.conn)
    agentMigrations <- withAgent getAgentMigrations
    pure $ CRVersionInfo {versionInfo, chatMigrations, agentMigrations}
  DebugLocks -> do
    chatLockName <- atomically . tryReadTMVar =<< asks chatLock
    agentLocks <- withAgent debugAgentLocks
    pure CRDebugLocks {chatLockName, agentLocks}
  GetAgentStats -> CRAgentStats . map stat <$> withAgent getAgentStats
    where
      stat (AgentStatsKey {host, clientTs, cmd, res}, count) =
        map B.unpack [host, clientTs, cmd, res, bshow count]
  ResetAgentStats -> withAgent resetAgentStats >> ok_
  GetAgentSubs -> summary <$> withAgent getAgentSubscriptions
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
  GetAgentSubsDetails -> CRAgentSubsDetails <$> withAgent getAgentSubscriptions
  where
    withChatLock name action = asks chatLock >>= \l -> withLock l name action
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
    procCmd :: m ChatResponse -> m ChatResponse
    procCmd = id
    ok_ = pure $ CRCmdOk Nothing
    ok = pure . CRCmdOk . Just
    getChatRef :: User -> ChatName -> m ChatRef
    getChatRef user (ChatName cType name) =
      ChatRef cType <$> case cType of
        CTDirect -> withStore $ \db -> getContactIdByName db user name
        CTGroup -> withStore $ \db -> getGroupIdByName db user name
        _ -> throwChatError $ CECommandError "not supported"
    checkChatStopped :: m ChatResponse -> m ChatResponse
    checkChatStopped a = asks agentAsync >>= readTVarIO >>= maybe a (const $ throwChatError CEChatNotStopped)
    setStoreChanged :: m ()
    setStoreChanged = asks chatStoreChanged >>= atomically . (`writeTVar` True)
    withStoreChanged :: m () -> m ChatResponse
    withStoreChanged a = checkChatStopped $ a >> setStoreChanged >> ok_
    checkStoreNotChanged :: m ChatResponse -> m ChatResponse
    checkStoreNotChanged = ifM (asks chatStoreChanged >>= readTVarIO) (throwChatError CEChatStoreChanged)
    withUserName :: UserName -> (UserId -> ChatCommand) -> m ChatResponse
    withUserName uName cmd = withStore (`getUserIdByName` uName) >>= processChatCommand . cmd
    withContactName :: ContactName -> (ContactId -> ChatCommand) -> m ChatResponse
    withContactName cName cmd = withUser $ \user ->
      withStore (\db -> getContactIdByName db user cName) >>= processChatCommand . cmd
    withMemberName :: GroupName -> ContactName -> (GroupId -> GroupMemberId -> ChatCommand) -> m ChatResponse
    withMemberName gName mName cmd = withUser $ \user ->
      getGroupAndMemberId user gName mName >>= processChatCommand . uncurry cmd
    getConnectionCode :: ConnId -> m Text
    getConnectionCode connId = verificationCode <$> withAgent (`getConnectionRatchetAdHash` connId)
    verifyConnectionCode :: User -> Connection -> Maybe Text -> m ChatResponse
    verifyConnectionCode user conn@Connection {connId} (Just code) = do
      code' <- getConnectionCode $ aConnId conn
      let verified = sameVerificationCode code code'
      when verified . withStore' $ \db -> setConnectionVerified db user connId $ Just code'
      pure $ CRConnectionVerified user verified code'
    verifyConnectionCode user conn@Connection {connId} _ = do
      code' <- getConnectionCode $ aConnId conn
      withStore' $ \db -> setConnectionVerified db user connId Nothing
      pure $ CRConnectionVerified user False code'
    getSentChatItemIdByText :: User -> ChatRef -> Text -> m Int64
    getSentChatItemIdByText user@User {userId, localDisplayName} (ChatRef cType cId) msg = case cType of
      CTDirect -> withStore $ \db -> getDirectChatItemIdByText db userId cId SMDSnd msg
      CTGroup -> withStore $ \db -> getGroupChatItemIdByText db user cId (Just localDisplayName) msg
      _ -> throwChatError $ CECommandError "not supported"
    getChatItemIdByText :: User -> ChatRef -> Text -> m Int64
    getChatItemIdByText user (ChatRef cType cId) msg = case cType of
      CTDirect -> withStore $ \db -> getDirectChatItemIdByText' db user cId msg
      CTGroup -> withStore $ \db -> getGroupChatItemIdByText' db user cId msg
      _ -> throwChatError $ CECommandError "not supported"
    connectViaContact :: User -> IncognitoEnabled -> ConnectionRequestUri 'CMContact -> m ChatResponse
    connectViaContact user@User {userId} incognito cReq@(CRContactUri ConnReqUriData {crClientData}) = withChatLock "connectViaContact" $ do
      let groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
          cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
      case groupLinkId of
        -- contact address
        Nothing ->
          withStore' (\db -> getConnReqContactXContactId db user cReqHash) >>= \case
            (Just contact, _) -> pure $ CRContactAlreadyExists user contact
            (_, xContactId_) -> procCmd $ do
              let randomXContactId = XContactId <$> drgRandomBytes 16
              xContactId <- maybe randomXContactId pure xContactId_
              connect' Nothing cReqHash xContactId
        -- group link
        Just gLinkId ->
          withStore' (\db -> getConnReqContactXContactId db user cReqHash) >>= \case
            (Just _contact, _) -> procCmd $ do
              -- allow repeat contact request
              newXContactId <- XContactId <$> drgRandomBytes 16
              connect' (Just gLinkId) cReqHash newXContactId
            (_, xContactId_) -> procCmd $ do
              let randomXContactId = XContactId <$> drgRandomBytes 16
              xContactId <- maybe randomXContactId pure xContactId_
              connect' (Just gLinkId) cReqHash xContactId
      where
        connect' groupLinkId cReqHash xContactId = do
          (connId, incognitoProfile, subMode) <- requestContact user incognito cReq xContactId
          conn <- withStore' $ \db -> createConnReqConnection db userId connId cReqHash xContactId incognitoProfile groupLinkId subMode
          pure $ CRSentInvitation user conn incognitoProfile
    connectContactViaAddress :: User -> IncognitoEnabled -> Contact -> ConnectionRequestUri 'CMContact -> m ChatResponse
    connectContactViaAddress user incognito ct cReq =
      withChatLock "connectViaContact" $ do
        newXContactId <- XContactId <$> drgRandomBytes 16
        (connId, incognitoProfile, subMode) <- requestContact user incognito cReq newXContactId
        let cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
        ct' <- withStore $ \db -> createAddressContactConnection db user ct connId cReqHash newXContactId incognitoProfile subMode
        pure $ CRSentInvitationToContact user ct' incognitoProfile
    requestContact :: User -> IncognitoEnabled -> ConnectionRequestUri 'CMContact -> XContactId -> m (ConnId, Maybe Profile, SubscriptionMode)
    requestContact user incognito cReq xContactId = do
      -- [incognito] generate profile to send
      incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
      let profileToSend = userProfileToSend user incognitoProfile Nothing
      dm <- directMessage (XContact profileToSend $ Just xContactId)
      subMode <- chatReadVar subscriptionMode
      connId <- withAgent $ \a -> joinConnection a (aUserId user) True cReq dm subMode
      pure (connId, incognitoProfile, subMode)
    contactMember :: Contact -> [GroupMember] -> Maybe GroupMember
    contactMember Contact {contactId} =
      find $ \GroupMember {memberContactId = cId, memberStatus = s} ->
        cId == Just contactId && s /= GSMemRemoved && s /= GSMemLeft
    checkSndFile :: MsgContent -> CryptoFile -> Integer -> m (Integer, SendFileMode)
    checkSndFile mc (CryptoFile f cfArgs) n = do
      fsFilePath <- toFSFilePath f
      unlessM (doesFileExist fsFilePath) . throwChatError $ CEFileNotFound f
      ChatConfig {fileChunkSize, inlineFiles} <- asks config
      xftpCfg <- readTVarIO =<< asks userXFTPFileConfig
      fileSize <- liftIO $ CF.getFileContentsSize $ CryptoFile fsFilePath cfArgs
      when (fromInteger fileSize > maxFileSize) $ throwChatError $ CEFileSize f
      let chunks = -((-fileSize) `div` fileChunkSize)
          fileInline = inlineFileMode mc inlineFiles chunks n
          fileMode = case xftpCfg of
            Just cfg
              | isJust cfArgs -> SendFileXFTP
              | fileInline == Just IFMSent || fileSize < minFileSize cfg || n <= 0 -> SendFileSMP fileInline
              | otherwise -> SendFileXFTP
            _ -> SendFileSMP fileInline
      pure (fileSize, fileMode)
    inlineFileMode mc InlineFilesConfig {offerChunks, sendChunks, totalSendChunks} chunks n
      | chunks > offerChunks = Nothing
      | chunks <= sendChunks && chunks * n <= totalSendChunks && isVoice mc = Just IFMSent
      | otherwise = Just IFMOffer
    updateProfile :: User -> Profile -> m ChatResponse
    updateProfile user p' = updateProfile_ user p' $ withStore $ \db -> updateUserProfile db user p'
    updateProfile_ :: User -> Profile -> m User -> m ChatResponse
    updateProfile_ user@User {profile = p@LocalProfile {displayName = n}} p'@Profile {displayName = n'} updateUser
      | p' == fromLocalProfile p = pure $ CRUserProfileNoChange user
      | otherwise = do
          when (n /= n') $ checkValidName n'
          -- read contacts before user update to correctly merge preferences
          -- [incognito] filter out contacts with whom user has incognito connections
          contacts <-
            filter (\ct -> contactReady ct && contactActive ct && not (contactConnIncognito ct))
              <$> withStore' (`getUserContacts` user)
          user' <- updateUser
          asks currentUser >>= atomically . (`writeTVar` Just user')
          withChatLock "updateProfile" . procCmd $ do
            ChatConfig {logLevel} <- asks config
            summary <- foldM (processAndCount user' logLevel) (UserProfileUpdateSummary 0 0 0 []) contacts
            pure $ CRUserProfileUpdated user' (fromLocalProfile p) p' summary
      where
        processAndCount user' ll s@UserProfileUpdateSummary {notChanged, updateSuccesses, updateFailures, changedContacts = cts} ct = do
          let mergedProfile = userProfileToSend user Nothing $ Just ct
              ct' = updateMergedPreferences user' ct
              mergedProfile' = userProfileToSend user' Nothing $ Just ct'
          if mergedProfile' == mergedProfile
            then pure s {notChanged = notChanged + 1}
            else
              let cts' = if mergedPreferences ct == mergedPreferences ct' then cts else ct' : cts
               in (notifyContact mergedProfile' ct' $> s {updateSuccesses = updateSuccesses + 1, changedContacts = cts'})
                    `catchChatError` \e -> when (ll <= CLLInfo) (toView $ CRChatError (Just user) e) $> s {updateFailures = updateFailures + 1, changedContacts = cts'}
          where
            notifyContact mergedProfile' ct' = do
              void $ sendDirectContactMessage ct' (XInfo mergedProfile')
              when (directOrUsed ct') $ createSndFeatureItems user' ct ct'
    updateContactPrefs :: User -> Contact -> Preferences -> m ChatResponse
    updateContactPrefs _ ct@Contact {activeConn = Nothing} _ = throwChatError $ CEContactNotActive ct
    updateContactPrefs user@User {userId} ct@Contact {activeConn = Just Connection {customUserProfileId}, userPreferences = contactUserPrefs} contactUserPrefs'
      | contactUserPrefs == contactUserPrefs' = pure $ CRContactPrefsUpdated user ct ct
      | otherwise = do
          assertDirectAllowed user MDSnd ct XInfo_
          ct' <- withStore' $ \db -> updateContactUserPreferences db user ct contactUserPrefs'
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore $ \db -> getProfileById db userId profileId
          let mergedProfile = userProfileToSend user (fromLocalProfile <$> incognitoProfile) (Just ct)
              mergedProfile' = userProfileToSend user (fromLocalProfile <$> incognitoProfile) (Just ct')
          when (mergedProfile' /= mergedProfile) $
            withChatLock "updateProfile" $ do
              void (sendDirectContactMessage ct' $ XInfo mergedProfile') `catchChatError` (toView . CRChatError (Just user))
              when (directOrUsed ct') $ createSndFeatureItems user ct ct'
          pure $ CRContactPrefsUpdated user ct ct'
    runUpdateGroupProfile :: User -> Group -> GroupProfile -> m ChatResponse
    runUpdateGroupProfile user (Group g@GroupInfo {groupProfile = p@GroupProfile {displayName = n}} ms) p'@GroupProfile {displayName = n'} = do
      assertUserGroupRole g GROwner
      when (n /= n') $ checkValidName n'
      g' <- withStore $ \db -> updateGroupProfile db user g p'
      (msg, _) <- sendGroupMessage user g' ms (XGrpInfo p')
      let cd = CDGroupSnd g'
      unless (sameGroupProfileInfo p p') $ do
        ci <- saveSndChatItem user cd msg (CISndGroupEvent $ SGEGroupUpdated p')
        toView $ CRNewChatItem user (AChatItem SCTGroup SMDSnd (GroupChat g') ci)
      createGroupFeatureChangedItems user cd CISndGroupFeature g g'
      pure $ CRGroupUpdated user g g' Nothing
    checkValidName :: GroupName -> m ()
    checkValidName displayName = do
      let validName = T.pack $ mkValidName $ T.unpack displayName
      when (displayName /= validName) $ throwChatError CEInvalidDisplayName {displayName, validName}
    assertUserGroupRole :: GroupInfo -> GroupMemberRole -> m ()
    assertUserGroupRole g@GroupInfo {membership} requiredRole = do
      when (membership.memberRole < requiredRole) $ throwChatError $ CEGroupUserRole g requiredRole
      when (memberStatus membership == GSMemInvited) $ throwChatError (CEGroupNotJoined g)
      when (memberRemoved membership) $ throwChatError CEGroupMemberUserRemoved
      unless (memberActive membership) $ throwChatError CEGroupMemberNotActive
    delGroupChatItem :: MsgDirectionI d => User -> GroupInfo -> ChatItem 'CTGroup d -> MessageId -> Maybe GroupMember -> m ChatResponse
    delGroupChatItem user gInfo ci msgId byGroupMember = do
      deletedTs <- liftIO getCurrentTime
      if groupFeatureAllowed SGFFullDelete gInfo
        then deleteGroupCI user gInfo ci True False byGroupMember deletedTs
        else markGroupCIDeleted user gInfo ci msgId True byGroupMember deletedTs
    updateGroupProfileByName :: GroupName -> (GroupProfile -> GroupProfile) -> m ChatResponse
    updateGroupProfileByName gName update = withUser $ \user -> do
      g@(Group GroupInfo {groupProfile = p} _) <- withStore $ \db ->
        getGroupIdByName db user gName >>= getGroup db vr user
      runUpdateGroupProfile user g $ update p
    withCurrentCall :: ContactId -> (User -> Contact -> Call -> m (Maybe Call)) -> m ChatResponse
    withCurrentCall ctId action = do
      (user, ct) <- withStore $ \db -> do
        user <- getUserByContactId db ctId
        (user,) <$> getContact db user ctId
      calls <- asks currentCalls
      withChatLock "currentCall" $
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
    withServerProtocol :: ProtocolTypeI p => SProtocolType p -> (UserProtocol p => m a) -> m a
    withServerProtocol p action = case userProtocol p of
      Just Dict -> action
      _ -> throwChatError $ CEServerProtocol $ AProtocolType p
    forwardFile :: ChatName -> FileTransferId -> (ChatName -> CryptoFile -> ChatCommand) -> m ChatResponse
    forwardFile chatName fileId sendCommand = withUser $ \user -> do
      withStore (\db -> getFileTransfer db user fileId) >>= \case
        FTRcv RcvFileTransfer {fileStatus = RFSComplete RcvFileInfo {filePath}, cryptoArgs} -> forward filePath cryptoArgs
        FTSnd {fileTransferMeta = FileTransferMeta {filePath, xftpSndFile}} -> forward filePath $ xftpSndFile >>= \f -> f.cryptoArgs
        _ -> throwChatError CEFileNotReceived {fileId}
      where
        forward path cfArgs = processChatCommand . sendCommand chatName $ CryptoFile path cfArgs
    getGroupAndMemberId :: User -> GroupName -> ContactName -> m (GroupId, GroupMemberId)
    getGroupAndMemberId user gName groupMemberName =
      withStore $ \db -> do
        groupId <- getGroupIdByName db user gName
        groupMemberId <- getGroupMemberIdByName db user groupId groupMemberName
        pure (groupId, groupMemberId)
    sendGrpInvitation :: User -> Contact -> GroupInfo -> GroupMember -> ConnReqInvitation -> m ()
    sendGrpInvitation user ct@Contact {localDisplayName} GroupInfo {groupId, groupProfile, membership} GroupMember {groupMemberId, memberId, memberRole = memRole} cReq = do
      let GroupMember {memberRole = userRole, memberId = userMemberId} = membership
          groupInv = GroupInvitation (MemberIdRole userMemberId userRole) (MemberIdRole memberId memRole) cReq groupProfile Nothing
      (msg, _) <- sendDirectContactMessage ct $ XGrpInv groupInv
      let content = CISndGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
      ci <- saveSndChatItem user (CDDirectSnd ct) msg content
      toView $ CRNewChatItem user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
    sndContactCITimed :: Bool -> Contact -> Maybe Int -> m (Maybe CITimed)
    sndContactCITimed live = sndCITimed_ live . contactTimedTTL
    sndGroupCITimed :: Bool -> GroupInfo -> Maybe Int -> m (Maybe CITimed)
    sndGroupCITimed live = sndCITimed_ live . groupTimedTTL
    sndCITimed_ :: Bool -> Maybe (Maybe Int) -> Maybe Int -> m (Maybe CITimed)
    sndCITimed_ live chatTTL itemTTL =
      forM (chatTTL >>= (itemTTL <|>)) $ \ttl ->
        CITimed ttl
          <$> if live
            then pure Nothing
            else Just . addUTCTime (realToFrac ttl) <$> liftIO getCurrentTime
    drgRandomBytes :: Int -> m ByteString
    drgRandomBytes n = asks random >>= atomically . C.randomBytes n
    privateGetUser :: UserId -> m User
    privateGetUser userId =
      tryChatError (withStore (`getUser` userId)) >>= \case
        Left _ -> throwChatError CEUserUnknown
        Right user -> pure user
    validateUserPassword :: User -> User -> Maybe UserPwd -> m ()
    validateUserPassword = validateUserPassword_ . Just
    validateUserPassword_ :: Maybe User -> User -> Maybe UserPwd -> m ()
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
    setUserNotifications :: UserId -> Bool -> m ChatResponse
    setUserNotifications userId' showNtfs = withUser $ \user -> do
      user' <- privateGetUser userId'
      case viewPwdHash user' of
        Just _ -> throwChatError $ CEHiddenUserAlwaysMuted userId'
        _ -> setUserPrivacy user user' {showNtfs}
    setUserPrivacy :: User -> User -> m ChatResponse
    setUserPrivacy user@User {userId} user'@User {userId = userId'}
      | userId == userId' = do
          asks currentUser >>= atomically . (`writeTVar` Just user')
          withStore' (`updateUserPrivacy` user')
          pure $ CRUserPrivacy {user = user', updatedUser = user'}
      | otherwise = do
          withStore' (`updateUserPrivacy` user')
          pure $ CRUserPrivacy {user, updatedUser = user'}
    checkDeleteChatUser :: User -> m ()
    checkDeleteChatUser user@User {userId} = do
      users <- withStore' getUsers
      let otherVisible = filter (\User {userId = userId', viewPwdHash} -> userId /= userId' && isNothing viewPwdHash) users
      when (activeUser user && length otherVisible > 0) $ throwChatError (CECantDeleteActiveUser userId)
    deleteChatUser :: User -> Bool -> m ChatResponse
    deleteChatUser user delSMPQueues = do
      filesInfo <- withStore' (`getUserFileInfo` user)
      forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
      withAgent $ \a -> deleteUser a (aUserId user) delSMPQueues
      withStore' (`deleteUserRecord` user)
      when (activeUser user) $ chatWriteVar currentUser Nothing
      ok_
    updateChatSettings :: ChatName -> (ChatSettings -> ChatSettings) -> m ChatResponse
    updateChatSettings (ChatName cType name) updateSettings = withUser $ \user -> do
      (chatId, chatSettings) <- case cType of
        CTDirect -> withStore $ \db -> do
          ctId <- getContactIdByName db user name
          Contact {chatSettings} <- getContact db user ctId
          pure (ctId, chatSettings)
        CTGroup ->
          withStore $ \db -> do
            gId <- getGroupIdByName db user name
            GroupInfo {chatSettings} <- getGroupInfo db vr user gId
            pure (gId, chatSettings)
        _ -> throwChatError $ CECommandError "not supported"
      processChatCommand $ APISetChatSettings (ChatRef cType chatId) $ updateSettings chatSettings
    connectPlan :: User -> AConnectionRequestUri -> m ConnectionPlan
    connectPlan user (ACR SCMInvitation cReq) = do
      withStore' (\db -> getConnectionEntityByConnReq db vr user cReqSchemas) >>= \case
        Nothing -> pure $ CPInvitationLink ILPOk
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
        cReqSchemas = case cReq of
          (CRInvitationUri crData e2e) ->
            ( CRInvitationUri crData {crScheme = CRSSimplex} e2e,
              CRInvitationUri crData {crScheme = simplexChat} e2e
            )
    connectPlan user (ACR SCMContact cReq) = do
      let CRContactUri ConnReqUriData {crClientData} = cReq
          groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
      case groupLinkId of
        -- contact address
        Nothing ->
          withStore' (\db -> getUserContactLinkByConnReq db user cReqSchemas) >>= \case
            Just _ -> pure $ CPContactAddress CAPOwnLink
            Nothing ->
              withStore' (\db -> getContactConnEntityByConnReqHash db vr user cReqHashes) >>= \case
                Nothing ->
                  withStore' (\db -> getContactWithoutConnViaAddress db user cReqSchemas) >>= \case
                    Nothing -> pure $ CPContactAddress CAPOk
                    Just ct -> pure $ CPContactAddress (CAPContactViaAddress ct)
                Just (RcvDirectMsgConnection _conn Nothing) -> pure $ CPContactAddress CAPConnectingConfirmReconnect
                Just (RcvDirectMsgConnection _ (Just ct))
                  | not (contactReady ct) && contactActive ct -> pure $ CPContactAddress (CAPConnectingProhibit ct)
                  | contactDeleted ct -> pure $ CPContactAddress CAPOk
                  | otherwise -> pure $ CPContactAddress (CAPKnown ct)
                Just _ -> throwChatError $ CECommandError "found connection entity is not RcvDirectMsgConnection"
        -- group link
        Just _ ->
          withStore' (\db -> getGroupInfoByUserContactLinkConnReq db vr user cReqSchemas) >>= \case
            Just g -> pure $ CPGroupLink (GLPOwnLink g)
            Nothing -> do
              connEnt_ <- withStore' $ \db -> getContactConnEntityByConnReqHash db vr user cReqHashes
              gInfo_ <- withStore' $ \db -> getGroupInfoByGroupLinkHash db vr user cReqHashes
              case (gInfo_, connEnt_) of
                (Nothing, Nothing) -> pure $ CPGroupLink GLPOk
                (Nothing, Just (RcvDirectMsgConnection _conn Nothing)) -> pure $ CPGroupLink GLPConnectingConfirmReconnect
                (Nothing, Just (RcvDirectMsgConnection _ (Just ct)))
                  | not (contactReady ct) && contactActive ct -> pure $ CPGroupLink (GLPConnectingProhibit gInfo_)
                  | otherwise -> pure $ CPGroupLink GLPOk
                (Nothing, Just _) -> throwChatError $ CECommandError "found connection entity is not RcvDirectMsgConnection"
                (Just gInfo@GroupInfo {membership}, _)
                  | not (memberActive membership) && not (memberRemoved membership) ->
                      pure $ CPGroupLink (GLPConnectingProhibit gInfo_)
                  | memberActive membership -> pure $ CPGroupLink (GLPKnown gInfo)
                  | otherwise -> pure $ CPGroupLink GLPOk
      where
        cReqSchemas :: (ConnReqContact, ConnReqContact)
        cReqSchemas = case cReq of
          (CRContactUri crData) ->
            ( CRContactUri crData {crScheme = CRSSimplex},
              CRContactUri crData {crScheme = simplexChat}
            )
        cReqHashes :: (ConnReqUriHash, ConnReqUriHash)
        cReqHashes = bimap hash hash cReqSchemas
        hash = ConnReqUriHash . C.sha256Hash . strEncode

prepareGroupMsg :: forall m. ChatMonad m => User -> GroupInfo -> MsgContent -> Maybe ChatItemId -> Maybe FileInvitation -> Maybe CITimed -> Bool -> m (MsgContainer, Maybe (CIQuote 'CTGroup))
prepareGroupMsg user GroupInfo {groupId, membership} mc quotedItemId_ fInv_ timed_ live = case quotedItemId_ of
  Nothing -> pure (MCSimple (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
  Just quotedItemId -> do
    CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
      withStore $ \db -> getGroupChatItem db user groupId quotedItemId
    (origQmc, qd, sent, GroupMember {memberId}) <- quoteData qci membership
    let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Just memberId}
        qmc = quoteContent mc origQmc file
        quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
    pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Just quotedItem)
  where
    quoteData :: ChatItem c d -> GroupMember -> m (MsgContent, CIQDirection 'CTGroup, Bool, GroupMember)
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
      MCUnknown {} -> True
    qText = msgContentText qmc
    getFileName :: CIFile d -> String
    getFileName CIFile {fileName} = fileName
    qFileName = maybe qText (T.pack . getFileName) ciFile_
    qTextOrFile = if T.null qText then qFileName else qText

assertDirectAllowed :: ChatMonad m => User -> MsgDirection -> Contact -> CMEventTag e -> m ()
assertDirectAllowed user dir ct event =
  unless (allowedChatEvent || anyDirectOrUsed ct) . unlessM directMessagesAllowed $
    throwChatError (CEDirectMessagesProhibited dir ct)
  where
    directMessagesAllowed = any (groupFeatureAllowed' SGFDirectMessages) <$> withStore' (\db -> getContactGroupPreferences db user ct)
    allowedChatEvent = case event of
      XMsgNew_ -> False
      XMsgUpdate_ -> False
      XMsgDel_ -> False
      XFile_ -> False
      XGrpInv_ -> False
      XCallInv_ -> False
      _ -> True

roundedFDCount :: Int -> Int
roundedFDCount n
  | n <= 0 = 4
  | otherwise = max 4 $ fromIntegral $ (2 :: Integer) ^ (ceiling (logBase 2 (fromIntegral n) :: Double) :: Integer)

startExpireCIThread :: forall m. ChatMonad' m => User -> m ()
startExpireCIThread user@User {userId} = do
  expireThreads <- asks expireCIThreads
  atomically (TM.lookup userId expireThreads) >>= \case
    Nothing -> do
      a <- Just <$> async (void $ runExceptT runExpireCIs)
      atomically $ TM.insert userId a expireThreads
    _ -> pure ()
  where
    runExpireCIs = do
      delay <- asks (initialCleanupManagerDelay . config)
      liftIO $ threadDelay' delay
      interval <- asks $ ciExpirationInterval . config
      forever $ do
        flip catchChatError (toView . CRChatError (Just user)) $ do
          expireFlags <- asks expireCIFlags
          atomically $ TM.lookup userId expireFlags >>= \b -> unless (b == Just True) retry
          ttl <- withStoreCtx' (Just "startExpireCIThread, getChatItemTTL") (`getChatItemTTL` user)
          forM_ ttl $ \t -> expireChatItems user t False
        liftIO $ threadDelay' interval

setExpireCIFlag :: ChatMonad' m => User -> Bool -> m ()
setExpireCIFlag User {userId} b = do
  expireFlags <- asks expireCIFlags
  atomically $ TM.insert userId b expireFlags

setAllExpireCIFlags :: ChatMonad' m => Bool -> m ()
setAllExpireCIFlags b = do
  expireFlags <- asks expireCIFlags
  atomically $ do
    keys <- M.keys <$> readTVar expireFlags
    forM_ keys $ \k -> TM.insert k b expireFlags

deleteFilesAndConns :: ChatMonad m => User -> [CIFileInfo] -> m ()
deleteFilesAndConns user filesInfo = do
  connIds <- mapM (deleteFile user) filesInfo
  deleteAgentConnectionsAsync user $ concat connIds

deleteFile :: ChatMonad m => User -> CIFileInfo -> m [ConnId]
deleteFile user fileInfo = deleteFile' user fileInfo False

deleteFile' :: forall m. ChatMonad m => User -> CIFileInfo -> Bool -> m [ConnId]
deleteFile' user ciFileInfo@CIFileInfo {filePath} sendCancel = do
  aConnIds <- cancelFile' user ciFileInfo sendCancel
  delete `catchChatError` (toView . CRChatError (Just user))
  pure aConnIds
  where
    delete :: m ()
    delete = withFilesFolder $ \filesFolder ->
      liftIO . forM_ filePath $ \fPath -> do
        let fsFilePath = filesFolder </> fPath
        removeFile fsFilePath `catchAll` \_ ->
          removePathForcibly fsFilePath `catchAll_` pure ()
    -- perform an action only if filesFolder is set (i.e. on mobile devices)
    withFilesFolder :: (FilePath -> m ()) -> m ()
    withFilesFolder action = asks filesFolder >>= readTVarIO >>= mapM_ action

cancelFile' :: forall m. ChatMonad m => User -> CIFileInfo -> Bool -> m [ConnId]
cancelFile' user CIFileInfo {fileId, fileStatus} sendCancel =
  case fileStatus of
    Just fStatus -> cancel' fStatus `catchChatError` (\e -> toView (CRChatError (Just user) e) $> [])
    Nothing -> pure []
  where
    cancel' :: ACIFileStatus -> m [ConnId]
    cancel' (AFS dir status) =
      if ciFileEnded status
        then pure []
        else case dir of
          SMDSnd -> do
            (ftm@FileTransferMeta {cancelled}, fts) <- withStore (\db -> getSndFileTransfer db user fileId)
            if cancelled then pure [] else cancelSndFile user ftm fts sendCancel
          SMDRcv -> do
            ft@RcvFileTransfer {cancelled} <- withStore (\db -> getRcvFileTransfer db user fileId)
            if cancelled then pure [] else maybeToList <$> cancelRcvFileTransfer user ft

updateCallItemStatus :: ChatMonad m => User -> Contact -> Call -> WebRTCCallStatus -> Maybe MessageId -> m ()
updateCallItemStatus user ct Call {chatItemId} receivedStatus msgId_ = do
  aciContent_ <- callStatusItemContent user ct chatItemId receivedStatus
  forM_ aciContent_ $ \aciContent -> updateDirectChatItemView user ct chatItemId aciContent False msgId_

updateDirectChatItemView :: ChatMonad m => User -> Contact -> ChatItemId -> ACIContent -> Bool -> Maybe MessageId -> m ()
updateDirectChatItemView user ct chatItemId (ACIContent msgDir ciContent) live msgId_ = do
  ci' <- withStore $ \db -> updateDirectChatItem db user ct chatItemId ciContent live msgId_
  toView $ CRChatItemUpdated user (AChatItem SCTDirect msgDir (DirectChat ct) ci')

callStatusItemContent :: ChatMonad m => User -> Contact -> ChatItemId -> WebRTCCallStatus -> m (Maybe ACIContent)
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
toFSFilePath :: ChatMonad' m => FilePath -> m FilePath
toFSFilePath f =
  maybe f (</> f) <$> (readTVarIO =<< asks filesFolder)

setFileToEncrypt :: ChatMonad m => RcvFileTransfer -> m RcvFileTransfer
setFileToEncrypt ft@RcvFileTransfer {fileId} = do
  cfArgs <- atomically . CF.randomArgs =<< asks random
  withStore' $ \db -> setFileCryptoArgs db fileId cfArgs
  pure (ft :: RcvFileTransfer) {cryptoArgs = Just cfArgs}

receiveFile' :: ChatMonad m => User -> RcvFileTransfer -> Maybe Bool -> Maybe FilePath -> m ChatResponse
receiveFile' user ft rcvInline_ filePath_ = do
  (CRRcvFileAccepted user <$> acceptFileReceive user ft rcvInline_ filePath_) `catchChatError` processError
  where
    processError = \case
      -- TODO AChatItem in Cancelled events
      ChatErrorAgent (SMP SMP.AUTH) _ -> pure $ CRRcvFileAcceptedSndCancelled user ft
      ChatErrorAgent (CONN DUPLICATE) _ -> pure $ CRRcvFileAcceptedSndCancelled user ft
      e -> throwError e

acceptFileReceive :: forall m. ChatMonad m => User -> RcvFileTransfer -> Maybe Bool -> Maybe FilePath -> m AChatItem
acceptFileReceive user@User {userId} RcvFileTransfer {fileId, xftpRcvFile, fileInvitation = FileInvitation {fileName = fName, fileConnReq, fileInline, fileSize}, fileStatus, grpMemberId, cryptoArgs} rcvInline_ filePath_ = do
  unless (fileStatus == RFSNew) $ case fileStatus of
    RFSCancelled _ -> throwChatError $ CEFileCancelled fName
    _ -> throwChatError $ CEFileAlreadyReceiving fName
  vr <- chatVersionRange
  case (xftpRcvFile, fileConnReq) of
    -- direct file protocol
    (Nothing, Just connReq) -> do
      subMode <- chatReadVar subscriptionMode
      dm <- directMessage $ XFileAcpt fName
      connIds <- joinAgentConnectionAsync user True connReq dm subMode
      filePath <- getRcvFilePath fileId filePath_ fName True
      withStoreCtx (Just "acceptFileReceive, acceptRcvFileTransfer") $ \db -> acceptRcvFileTransfer db vr user fileId connIds ConnJoined filePath subMode
    -- XFTP
    (Just XFTPRcvFile {}, _) -> do
      filePath <- getRcvFilePath fileId filePath_ fName False
      (ci, rfd) <- withStoreCtx (Just "acceptFileReceive, xftpAcceptRcvFT ...") $ \db -> do
        -- marking file as accepted and reading description in the same transaction
        -- to prevent race condition with appending description
        ci <- xftpAcceptRcvFT db vr user fileId filePath
        rfd <- getRcvFileDescrByRcvFileId db fileId
        pure (ci, rfd)
      receiveViaCompleteFD user fileId rfd cryptoArgs
      pure ci
    -- group & direct file protocol
    _ -> do
      chatRef <- withStoreCtx (Just "acceptFileReceive, getChatRefByFileId") $ \db -> getChatRefByFileId db user fileId
      case (chatRef, grpMemberId) of
        (ChatRef CTDirect contactId, Nothing) -> do
          ct <- withStoreCtx (Just "acceptFileReceive, getContact") $ \db -> getContact db user contactId
          acceptFile CFCreateConnFileInvDirect $ \msg -> void $ sendDirectContactMessage ct msg
        (ChatRef CTGroup groupId, Just memId) -> do
          GroupMember {activeConn} <- withStoreCtx (Just "acceptFileReceive, getGroupMember") $ \db -> getGroupMember db user groupId memId
          case activeConn of
            Just conn -> do
              acceptFile CFCreateConnFileInvGroup $ \msg -> void $ sendDirectMessage conn msg $ GroupId groupId
            _ -> throwChatError $ CEFileInternal "member connection not active"
        _ -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
  where
    acceptFile :: CommandFunction -> (ChatMsgEvent 'Json -> m ()) -> m AChatItem
    acceptFile cmdFunction send = do
      filePath <- getRcvFilePath fileId filePath_ fName True
      inline <- receiveInline
      vr <- chatVersionRange
      if
        | inline -> do
            -- accepting inline
            ci <- withStoreCtx (Just "acceptFile, acceptRcvInlineFT") $ \db -> acceptRcvInlineFT db vr user fileId filePath
            sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
            send $ XFileAcptInv sharedMsgId Nothing fName
            pure ci
        | fileInline == Just IFMSent -> throwChatError $ CEFileAlreadyReceiving fName
        | otherwise -> do
            -- accepting via a new connection
            subMode <- chatReadVar subscriptionMode
            connIds <- createAgentConnectionAsync user cmdFunction True SCMInvitation subMode
            withStoreCtx (Just "acceptFile, acceptRcvFileTransfer") $ \db -> acceptRcvFileTransfer db vr user fileId connIds ConnNew filePath subMode
    receiveInline :: m Bool
    receiveInline = do
      ChatConfig {fileChunkSize, inlineFiles = InlineFilesConfig {receiveChunks, offerChunks}} <- asks config
      pure $
        rcvInline_ /= Just False
          && fileInline == Just IFMOffer
          && ( fileSize <= fileChunkSize * receiveChunks
                || (rcvInline_ == Just True && fileSize <= fileChunkSize * offerChunks)
             )

receiveViaCompleteFD :: ChatMonad m => User -> FileTransferId -> RcvFileDescr -> Maybe CryptoFileArgs -> m ()
receiveViaCompleteFD user fileId RcvFileDescr {fileDescrText, fileDescrComplete} cfArgs =
  when fileDescrComplete $ do
    rd <- parseFileDescription fileDescrText
    aFileId <- withAgent $ \a -> xftpReceiveFile a (aUserId user) rd cfArgs
    startReceivingFile user fileId
    withStoreCtx' (Just "receiveViaCompleteFD, updateRcvFileAgentId") $ \db -> updateRcvFileAgentId db fileId (Just $ AgentRcvFileId aFileId)

startReceivingFile :: ChatMonad m => User -> FileTransferId -> m ()
startReceivingFile user fileId = do
  vr <- chatVersionRange
  ci <- withStoreCtx (Just "startReceivingFile, updateRcvFileStatus ...") $ \db -> do
    liftIO $ updateRcvFileStatus db fileId FSConnected
    liftIO $ updateCIFileStatus db user fileId $ CIFSRcvTransfer 0 1
    getChatItemByFileId db vr user fileId
  toView $ CRRcvFileStart user ci

getRcvFilePath :: forall m. ChatMonad m => FileTransferId -> Maybe FilePath -> String -> Bool -> m FilePath
getRcvFilePath fileId fPath_ fn keepHandle = case fPath_ of
  Nothing ->
    chatReadVar filesFolder >>= \case
      Nothing ->
        getDefaultFilesFolder
          >>= (`uniqueCombine` fn)
          >>= createEmptyFile
      Just filesFolder ->
        filesFolder `uniqueCombine` fn
          >>= createEmptyFile
          >>= pure <$> takeFileName
  Just fPath ->
    ifM
      (doesDirectoryExist fPath)
      (fPath `uniqueCombine` fn >>= createEmptyFile)
      $ ifM
        (doesFileExist fPath)
        (throwChatError $ CEFileAlreadyExists fPath)
        (createEmptyFile fPath)
  where
    createEmptyFile :: FilePath -> m FilePath
    createEmptyFile fPath = emptyFile fPath `catchThrow` (ChatError . CEFileWrite fPath . show)
    emptyFile :: FilePath -> m FilePath
    emptyFile fPath = do
      h <-
        if keepHandle
          then getFileHandle fileId fPath rcvFiles AppendMode
          else getTmpHandle fPath
      liftIO $ B.hPut h "" >> hFlush h
      pure fPath
    getTmpHandle :: FilePath -> m Handle
    getTmpHandle fPath = openFile fPath AppendMode `catchThrow` (ChatError . CEFileInternal . show)

acceptContactRequest :: ChatMonad m => User -> UserContactRequest -> Maybe IncognitoProfile -> Bool -> m Contact
acceptContactRequest user UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange, localDisplayName = cName, profileId, profile = cp, userContactLinkId, xContactId} incognitoProfile contactUsed = do
  subMode <- chatReadVar subscriptionMode
  let profileToSend = profileToSendOnAccept user incognitoProfile
  dm <- directMessage $ XInfo profileToSend
  acId <- withAgent $ \a -> acceptContact a True invId dm subMode
  withStore' $ \db -> createAcceptedContact db user acId (fromJVersionRange cReqChatVRange) cName profileId cp userContactLinkId xContactId incognitoProfile subMode contactUsed

acceptContactRequestAsync :: ChatMonad m => User -> UserContactRequest -> Maybe IncognitoProfile -> Bool -> m Contact
acceptContactRequestAsync user UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange, localDisplayName = cName, profileId, profile = p, userContactLinkId, xContactId} incognitoProfile contactUsed = do
  subMode <- chatReadVar subscriptionMode
  let profileToSend = profileToSendOnAccept user incognitoProfile
  (cmdId, acId) <- agentAcceptContactAsync user True invId (XInfo profileToSend) subMode
  withStore' $ \db -> do
    ct@Contact {activeConn} <- createAcceptedContact db user acId (fromJVersionRange cReqChatVRange) cName profileId p userContactLinkId xContactId incognitoProfile subMode contactUsed
    forM_ activeConn $ \Connection {connId} -> setCommandConnId db user cmdId connId
    pure ct

acceptGroupJoinRequestAsync :: ChatMonad m => User -> GroupInfo -> UserContactRequest -> GroupMemberRole -> Maybe IncognitoProfile -> m GroupMember
acceptGroupJoinRequestAsync
  user
  gInfo@GroupInfo {groupProfile, membership}
  ucr@UserContactRequest {agentInvitationId = AgentInvId invId}
  gLinkMemRole
  incognitoProfile = do
    gVar <- asks random
    (groupMemberId, memberId) <- withStore $ \db -> createAcceptedMember db gVar user gInfo ucr gLinkMemRole
    let Profile {displayName} = profileToSendOnAccept user incognitoProfile
        GroupMember {memberRole = userRole, memberId = userMemberId} = membership
        msg = XGrpLinkInv $ GroupLinkInvitation (MemberIdRole userMemberId userRole) displayName (MemberIdRole memberId gLinkMemRole) groupProfile
    subMode <- chatReadVar subscriptionMode
    connIds <- agentAcceptContactAsync user True invId msg subMode
    withStore $ \db -> do
      liftIO $ createAcceptedMemberConnection db user connIds ucr groupMemberId subMode
      getGroupMemberById db user groupMemberId

profileToSendOnAccept :: User -> Maybe IncognitoProfile -> Profile
profileToSendOnAccept user ip = userProfileToSend user (getIncognitoProfile <$> ip) Nothing
  where
    getIncognitoProfile = \case
      NewIncognito p -> p
      ExistingIncognito lp -> fromLocalProfile lp

deleteGroupLink' :: ChatMonad m => User -> GroupInfo -> m ()
deleteGroupLink' user gInfo = do
  conn <- withStore $ \db -> getGroupLinkConnection db user gInfo
  deleteGroupLink_ user gInfo conn

deleteGroupLinkIfExists :: ChatMonad m => User -> GroupInfo -> m ()
deleteGroupLinkIfExists user gInfo = do
  conn_ <- eitherToMaybe <$> withStore' (\db -> runExceptT $ getGroupLinkConnection db user gInfo)
  mapM_ (deleteGroupLink_ user gInfo) conn_

deleteGroupLink_ :: ChatMonad m => User -> GroupInfo -> Connection -> m ()
deleteGroupLink_ user gInfo conn = do
  deleteAgentConnectionAsync user $ aConnId conn
  withStore' $ \db -> deleteGroupLink db user gInfo

agentSubscriber :: forall m. (MonadUnliftIO m, MonadReader ChatController m) => m ()
agentSubscriber = do
  q <- asks $ subQ . smpAgent
  l <- asks chatLock
  forever $ atomically (readTBQueue q) >>= void . process l
  where
    process :: Lock -> (ACorrId, EntityId, APartyCmd 'Agent) -> m (Either ChatError ())
    process l (corrId, entId, APC e msg) = run $ case e of
      SAENone -> processAgentMessageNoConn msg
      SAEConn -> processAgentMessage corrId entId msg
      SAERcvFile -> processAgentMsgRcvFile corrId entId msg
      SAESndFile -> processAgentMsgSndFile corrId entId msg
      where
        run action = do
          let name = "agentSubscriber entity=" <> show e <> " entId=" <> str entId <> " msg=" <> str (aCommandTag msg)
          withLock l name $ runExceptT $ action `catchChatError` (toView . CRChatError Nothing)
        str :: StrEncoding a => a -> String
        str = B.unpack . strEncode

type AgentBatchSubscribe m = AgentClient -> [ConnId] -> ExceptT AgentErrorType m (Map ConnId (Either AgentErrorType ()))

subscribeUserConnections :: forall m. ChatMonad m => VersionRange -> Bool -> AgentBatchSubscribe m -> User -> m ()
subscribeUserConnections vr onlyNeeded agentBatchSubscribe user@User {userId} = do
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
        let conns = concat [ctConns, ucConns, mConns, sftConns, rftConns, pcConns]
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
    getContactConns :: m ([ConnId], Map ConnId Contact)
    getContactConns = do
      cts <- withStore_ ("subscribeUserConnections " <> show userId <> ", getUserContacts") getUserContacts
      let cts' = mapMaybe (\ct -> (,ct) <$> contactConnId ct) $ filter contactActive cts
      pure (map fst cts', M.fromList cts')
    getUserContactLinkConns :: m ([ConnId], Map ConnId UserContact)
    getUserContactLinkConns = do
      (cs, ucs) <- unzip <$> withStore_ ("subscribeUserConnections " <> show userId <> ", getUserContactLinks") getUserContactLinks
      let connIds = map aConnId cs
      pure (connIds, M.fromList $ zip connIds ucs)
    getGroupMemberConns :: m ([Group], [ConnId], Map ConnId GroupMember)
    getGroupMemberConns = do
      gs <- withStore_ ("subscribeUserConnections " <> show userId <> ", getUserGroups") (`getUserGroups` vr)
      let mPairs = concatMap (\(Group _ ms) -> mapMaybe (\m -> (,m) <$> memberConnId m) (filter (not . memberRemoved) ms)) gs
      pure (gs, map fst mPairs, M.fromList mPairs)
    getSndFileTransferConns :: m ([ConnId], Map ConnId SndFileTransfer)
    getSndFileTransferConns = do
      sfts <- withStore_ ("subscribeUserConnections " <> show userId <> ", getLiveSndFileTransfers") getLiveSndFileTransfers
      let connIds = map sndFileTransferConnId sfts
      pure (connIds, M.fromList $ zip connIds sfts)
    getRcvFileTransferConns :: m ([ConnId], Map ConnId RcvFileTransfer)
    getRcvFileTransferConns = do
      rfts <- withStore_ ("subscribeUserConnections " <> show userId <> ", getLiveRcvFileTransfers") getLiveRcvFileTransfers
      let rftPairs = mapMaybe (\ft -> (,ft) <$> liveRcvFileTransferConnId ft) rfts
      pure (map fst rftPairs, M.fromList rftPairs)
    getPendingContactConns :: m ([ConnId], Map ConnId PendingContactConnection)
    getPendingContactConns = do
      pcs <- withStore_ ("subscribeUserConnections " <> show userId <> ", getPendingContactConnections") getPendingContactConnections
      let connIds = map aConnId' pcs
      pure (connIds, M.fromList $ zip connIds pcs)
    contactSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId Contact -> Bool -> m ()
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
              ChatErrorAgent (SMP SMP.AUTH) _ -> "contact deleted"
              e -> show e
    -- TODO possibly below could be replaced with less noisy events for API
    contactLinkSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId UserContact -> m ()
    contactLinkSubsToView rs = toView . CRUserContactSubSummary user . map (uncurry UserContactSubStatus) . resultsFor rs
    groupSubsToView :: Map ConnId (Either AgentErrorType ()) -> [Group] -> Map ConnId GroupMember -> Bool -> m ()
    groupSubsToView rs gs ms ce = do
      mapM_ groupSub $
        sortOn (\(Group GroupInfo {localDisplayName = g} _) -> g) gs
      toView . CRMemberSubSummary user $ map (uncurry MemberSubStatus) mRs
      where
        mRs = resultsFor rs ms
        groupSub :: Group -> m ()
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
    sndFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId SndFileTransfer -> m ()
    sndFileSubsToView rs sfts = do
      let sftRs = resultsFor rs sfts
      forM_ sftRs $ \(ft@SndFileTransfer {fileId, fileStatus}, err_) -> do
        forM_ err_ $ toView . CRSndFileSubError user ft
        void . forkIO $ do
          threadDelay 1000000
          l <- asks chatLock
          when (fileStatus == FSConnected) . unlessM (isFileActive fileId sndFiles) . withLock l "subscribe sendFileChunk" $
            sendFileChunk user ft
    rcvFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId RcvFileTransfer -> m ()
    rcvFileSubsToView rs = mapM_ (toView . uncurry (CRRcvFileSubError user)) . filterErrors . resultsFor rs
    pendingConnSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId PendingContactConnection -> m ()
    pendingConnSubsToView rs = toView . CRPendingSubSummary user . map (uncurry PendingSubStatus) . resultsFor rs
    withStore_ :: String -> (DB.Connection -> User -> IO [a]) -> m [a]
    withStore_ ctx a = withStoreCtx' (Just ctx) (`a` user) `catchChatError` \e -> toView (CRChatError (Just user) e) $> []
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

cleanupManager :: forall m. ChatMonad m => m ()
cleanupManager = do
  interval <- asks (cleanupManagerInterval . config)
  runWithoutInitialDelay interval
  initialDelay <- asks (initialCleanupManagerDelay . config)
  liftIO $ threadDelay' initialDelay
  stepDelay <- asks (cleanupManagerStepDelay . config)
  forever $ do
    flip catchChatError (toView . CRChatError Nothing) $ do
      waitChatStarted
      users <- withStoreCtx' (Just "cleanupManager, getUsers 1") getUsers
      let (us, us') = partition activeUser users
      forM_ us $ cleanupUser interval stepDelay
      forM_ us' $ cleanupUser interval stepDelay
      cleanupMessages `catchChatError` (toView . CRChatError Nothing)
      cleanupProbes `catchChatError` (toView . CRChatError Nothing)
    liftIO $ threadDelay' $ diffToMicroseconds interval
  where
    runWithoutInitialDelay cleanupInterval = flip catchChatError (toView . CRChatError Nothing) $ do
      waitChatStarted
      users <- withStoreCtx' (Just "cleanupManager, getUsers 2") getUsers
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
      timedItems <- withStoreCtx' (Just "cleanupManager, getTimedItems") $ \db -> getTimedItems db user startTimedThreadCutoff
      forM_ timedItems $ \(itemRef, deleteAt) -> startTimedItemThread user itemRef deleteAt `catchChatError` const (pure ())
    cleanupDeletedContacts user = do
      contacts <- withStore' (`getDeletedContacts` user)
      forM_ contacts $ \ct ->
        withStore' (\db -> deleteContactWithoutGroups db user ct)
          `catchChatError` (toView . CRChatError (Just user))
    cleanupMessages = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(30 * nominalDay)) ts
      withStoreCtx' (Just "cleanupManager, deleteOldMessages") (`deleteOldMessages` cutoffTs)
    cleanupProbes = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(14 * nominalDay)) ts
      withStore' (`deleteOldProbes` cutoffTs)

startProximateTimedItemThread :: ChatMonad m => User -> (ChatRef, ChatItemId) -> UTCTime -> m ()
startProximateTimedItemThread user itemRef deleteAt = do
  interval <- asks (cleanupManagerInterval . config)
  ts <- liftIO getCurrentTime
  when (diffUTCTime deleteAt ts <= interval) $
    startTimedItemThread user itemRef deleteAt

startTimedItemThread :: ChatMonad m => User -> (ChatRef, ChatItemId) -> UTCTime -> m ()
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

deleteTimedItem :: ChatMonad m => User -> (ChatRef, ChatItemId) -> UTCTime -> m ()
deleteTimedItem user (ChatRef cType chatId, itemId) deleteAt = do
  ts <- liftIO getCurrentTime
  liftIO $ threadDelay' $ diffToMicroseconds $ diffUTCTime deleteAt ts
  waitChatStarted
  vr <- chatVersionRange
  case cType of
    CTDirect -> do
      (ct, CChatItem _ ci) <- withStore $ \db -> (,) <$> getContact db user chatId <*> getDirectChatItem db user chatId itemId
      deleteDirectCI user ct ci True True >>= toView
    CTGroup -> do
      (gInfo, CChatItem _ ci) <- withStore $ \db -> (,) <$> getGroupInfo db vr user chatId <*> getGroupChatItem db user chatId itemId
      deletedTs <- liftIO getCurrentTime
      deleteGroupCI user gInfo ci True True Nothing deletedTs >>= toView
    _ -> toView . CRChatError (Just user) . ChatError $ CEInternalError "bad deleteTimedItem cType"

startUpdatedTimedItemThread :: ChatMonad m => User -> ChatRef -> ChatItem c d -> ChatItem c d -> m ()
startUpdatedTimedItemThread user chatRef ci ci' =
  case (chatItemTimed ci >>= timedDeleteAt', chatItemTimed ci' >>= timedDeleteAt') of
    (Nothing, Just deleteAt') ->
      startProximateTimedItemThread user (chatRef, chatItemId' ci') deleteAt'
    _ -> pure ()

expireChatItems :: forall m. ChatMonad m => User -> Int64 -> Bool -> m ()
expireChatItems user@User {userId} ttl sync = do
  currentTs <- liftIO getCurrentTime
  vr <- chatVersionRange
  let expirationDate = addUTCTime (-1 * fromIntegral ttl) currentTs
      -- this is to keep group messages created during last 12 hours even if they're expired according to item_ts
      createdAtCutoff = addUTCTime (-43200 :: NominalDiffTime) currentTs
  contacts <- withStoreCtx' (Just "expireChatItems, getUserContacts") (`getUserContacts` user)
  loop contacts $ processContact expirationDate
  groups <- withStoreCtx' (Just "expireChatItems, getUserGroupDetails") (\db -> getUserGroupDetails db vr user Nothing Nothing)
  loop groups $ processGroup expirationDate createdAtCutoff
  where
    loop :: [a] -> (a -> m ()) -> m ()
    loop [] _ = pure ()
    loop (a : as) process = continue $ do
      process a `catchChatError` (toView . CRChatError (Just user))
      loop as process
    continue :: m () -> m ()
    continue a =
      if sync
        then a
        else do
          expireFlags <- asks expireCIFlags
          expire <- atomically $ TM.lookup userId expireFlags
          when (expire == Just True) $ threadDelay 100000 >> a
    processContact :: UTCTime -> Contact -> m ()
    processContact expirationDate ct = do
      filesInfo <- withStoreCtx' (Just "processContact, getContactExpiredFileInfo") $ \db -> getContactExpiredFileInfo db user ct expirationDate
      deleteFilesAndConns user filesInfo
      withStoreCtx' (Just "processContact, deleteContactExpiredCIs") $ \db -> deleteContactExpiredCIs db user ct expirationDate
    processGroup :: UTCTime -> UTCTime -> GroupInfo -> m ()
    processGroup expirationDate createdAtCutoff gInfo = do
      filesInfo <- withStoreCtx' (Just "processGroup, getGroupExpiredFileInfo") $ \db -> getGroupExpiredFileInfo db user gInfo expirationDate createdAtCutoff
      deleteFilesAndConns user filesInfo
      withStoreCtx' (Just "processGroup, deleteGroupExpiredCIs") $ \db -> deleteGroupExpiredCIs db user gInfo expirationDate createdAtCutoff
      membersToDelete <- withStoreCtx' (Just "processGroup, getGroupMembersForExpiration") $ \db -> getGroupMembersForExpiration db user gInfo
      forM_ membersToDelete $ \m -> withStoreCtx' (Just "processGroup, deleteGroupMember") $ \db -> deleteGroupMember db user m

processAgentMessage :: forall m. ChatMonad m => ACorrId -> ConnId -> ACommand 'Agent 'AEConn -> m ()
processAgentMessage _ connId (DEL_RCVQ srv qId err_) =
  toView $ CRAgentRcvQueueDeleted (AgentConnId connId) srv (AgentQueueId qId) err_
processAgentMessage _ connId DEL_CONN =
  toView $ CRAgentConnDeleted (AgentConnId connId)
processAgentMessage corrId connId msg = do
  vr <- chatVersionRange
  withStore' (`getUserByAConnId` AgentConnId connId) >>= \case
    Just user -> processAgentMessageConn vr user corrId connId msg `catchChatError` (toView . CRChatError (Just user))
    _ -> throwChatError $ CENoConnectionUser (AgentConnId connId)

processAgentMessageNoConn :: forall m. ChatMonad m => ACommand 'Agent 'AENone -> m ()
processAgentMessageNoConn = \case
  CONNECT p h -> hostEvent $ CRHostConnected p h
  DISCONNECT p h -> hostEvent $ CRHostDisconnected p h
  DOWN srv conns -> serverEvent srv conns NSDisconnected CRContactsDisconnected
  UP srv conns -> serverEvent srv conns NSConnected CRContactsSubscribed
  SUSPENDED -> toView CRChatSuspended
  DEL_USER agentUserId -> toView $ CRAgentUserDeleted agentUserId
  where
    hostEvent :: ChatResponse -> m ()
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

processAgentMsgSndFile :: forall m. ChatMonad m => ACorrId -> SndFileId -> ACommand 'Agent 'AESndFile -> m ()
processAgentMsgSndFile _corrId aFileId msg =
  withStore' (`getUserByASndFileId` AgentSndFileId aFileId) >>= \case
    Just user -> process user `catchChatError` (toView . CRChatError (Just user))
    _ -> do
      withAgent (`xftpDeleteSndFileInternal` aFileId)
      throwChatError $ CENoSndFileUser $ AgentSndFileId aFileId
  where
    process :: User -> m ()
    process user = do
      (ft@FileTransferMeta {fileId, cancelled}, sfts) <- withStore $ \db -> do
        fileId <- getXFTPSndFileDBId db user $ AgentSndFileId aFileId
        getSndFileTransfer db user fileId
      vr <- chatVersionRange
      unless cancelled $ case msg of
        SFPROG sndProgress sndTotal -> do
          let status = CIFSSndTransfer {sndProgress, sndTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            getChatItemByFileId db vr user fileId
          toView $ CRSndFileProgressXFTP user ci ft sndProgress sndTotal
        SFDONE sndDescr rfds -> do
          withStore' $ \db -> setSndFTPrivateSndDescr db user fileId (fileDescrText sndDescr)
          ci@(AChatItem _ d cInfo _ci@ChatItem {meta = CIMeta {itemSharedMsgId = msgId_, itemDeleted}}) <-
            withStore $ \db -> getChatItemByFileId db vr user fileId
          case (msgId_, itemDeleted) of
            (Just sharedMsgId, Nothing) -> do
              when (length rfds < length sfts) $ throwChatError $ CEInternalError "not enough XFTP file descriptions to send"
              -- TODO either update database status or move to SFPROG
              toView $ CRSndFileProgressXFTP user ci ft 1 1
              case (rfds, sfts, d, cInfo) of
                (rfd : extraRFDs, sft : _, SMDSnd, DirectChat ct) -> do
                  withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText extraRFDs)
                  msgDeliveryId <- sendFileDescription sft rfd sharedMsgId $ sendDirectContactMessage ct
                  withStore' $ \db -> updateSndFTDeliveryXFTP db sft msgDeliveryId
                  withAgent (`xftpDeleteSndFileInternal` aFileId)
                (_, _, SMDSnd, GroupChat g@GroupInfo {groupId}) -> do
                  ms <- withStore' $ \db -> getGroupMembers db user g
                  let rfdsMemberFTs = zip rfds $ memberFTs ms
                      extraRFDs = drop (length rfdsMemberFTs) rfds
                  withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText extraRFDs)
                  forM_ rfdsMemberFTs $ \mt -> sendToMember mt `catchChatError` (toView . CRChatError (Just user))
                  ci' <- withStore $ \db -> do
                    liftIO $ updateCIFileStatus db user fileId CIFSSndComplete
                    getChatItemByFileId db vr user fileId
                  withAgent (`xftpDeleteSndFileInternal` aFileId)
                  toView $ CRSndFileCompleteXFTP user ci' ft
                  where
                    memberFTs :: [GroupMember] -> [(Connection, SndFileTransfer)]
                    memberFTs ms = M.elems $ M.intersectionWith (,) (M.fromList mConns') (M.fromList sfts')
                      where
                        mConns' = mapMaybe useMember ms
                        sfts' = mapMaybe (\sft@SndFileTransfer {groupMemberId} -> (,sft) <$> groupMemberId) sfts
                        useMember GroupMember {groupMemberId, activeConn = Just conn@Connection {connStatus}}
                          | (connStatus == ConnReady || connStatus == ConnSndReady) && not (connDisabled conn) = Just (groupMemberId, conn)
                          | otherwise = Nothing
                        useMember _ = Nothing
                    sendToMember :: (ValidFileDescription 'FRecipient, (Connection, SndFileTransfer)) -> m ()
                    sendToMember (rfd, (conn, sft)) =
                      void $ sendFileDescription sft rfd sharedMsgId $ \msg' -> sendDirectMessage conn msg' $ GroupId groupId
                _ -> pure ()
            _ -> pure () -- TODO error?
        SFERR e
          | temporaryAgentError e ->
              throwChatError $ CEXFTPSndFile fileId (AgentSndFileId aFileId) e
          | otherwise -> do
              ci <- withStore $ \db -> do
                liftIO $ updateFileCancelled db user fileId CIFSSndError
                getChatItemByFileId db vr user fileId
              withAgent (`xftpDeleteSndFileInternal` aFileId)
              toView $ CRSndFileError user ci
      where
        fileDescrText :: FilePartyI p => ValidFileDescription p -> T.Text
        fileDescrText = safeDecodeUtf8 . strEncode
        sendFileDescription :: SndFileTransfer -> ValidFileDescription 'FRecipient -> SharedMsgId -> (ChatMsgEvent 'Json -> m (SndMessage, Int64)) -> m Int64
        sendFileDescription sft rfd msgId sendMsg = do
          let rfdText = fileDescrText rfd
          withStore' $ \db -> updateSndFTDescrXFTP db user sft rfdText
          parts <- splitFileDescr rfdText
          loopSend parts
          where
            -- returns msgDeliveryId of the last file description message
            loopSend :: NonEmpty FileDescr -> m Int64
            loopSend (fileDescr :| fds) = do
              (_, msgDeliveryId) <- sendMsg $ XMsgFileDescr {msgId, fileDescr}
              case L.nonEmpty fds of
                Just fds' -> loopSend fds'
                Nothing -> pure msgDeliveryId

splitFileDescr :: ChatMonad m => RcvFileDescrText -> m (NonEmpty FileDescr)
splitFileDescr rfdText = do
  partSize <- asks $ xftpDescrPartSize . config
  pure $ splitParts 1 partSize rfdText
  where
    splitParts partNo partSize remText =
      let (part, rest) = T.splitAt partSize remText
          complete = T.null rest
          fileDescr = FileDescr {fileDescrText = part, fileDescrPartNo = partNo, fileDescrComplete = complete}
       in if complete
            then fileDescr :| []
            else fileDescr <| splitParts (partNo + 1) partSize rest

processAgentMsgRcvFile :: forall m. ChatMonad m => ACorrId -> RcvFileId -> ACommand 'Agent 'AERcvFile -> m ()
processAgentMsgRcvFile _corrId aFileId msg =
  withStore' (`getUserByARcvFileId` AgentRcvFileId aFileId) >>= \case
    Just user -> process user `catchChatError` (toView . CRChatError (Just user))
    _ -> do
      withAgent (`xftpDeleteRcvFile` aFileId)
      throwChatError $ CENoRcvFileUser $ AgentRcvFileId aFileId
  where
    process :: User -> m ()
    process user = do
      ft@RcvFileTransfer {fileId} <- withStore $ \db -> do
        fileId <- getXFTPRcvFileDBId db $ AgentRcvFileId aFileId
        getRcvFileTransfer db user fileId
      vr <- chatVersionRange
      unless (rcvFileCompleteOrCancelled ft) $ case msg of
        RFPROG rcvProgress rcvTotal -> do
          let status = CIFSRcvTransfer {rcvProgress, rcvTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            getChatItemByFileId db vr user fileId
          toView $ CRRcvFileProgressXFTP user ci rcvProgress rcvTotal
        RFDONE xftpPath ->
          case liveRcvFileTransferPath ft of
            Nothing -> throwChatError $ CEInternalError "no target path for received XFTP file"
            Just targetPath -> do
              fsTargetPath <- toFSFilePath targetPath
              renameFile xftpPath fsTargetPath
              ci <- withStore $ \db -> do
                liftIO $ do
                  updateRcvFileStatus db fileId FSComplete
                  updateCIFileStatus db user fileId CIFSRcvComplete
                getChatItemByFileId db vr user fileId
              agentXFTPDeleteRcvFile aFileId fileId
              toView $ CRRcvFileComplete user ci
        RFERR e
          | temporaryAgentError e ->
              throwChatError $ CEXFTPRcvFile fileId (AgentRcvFileId aFileId) e
          | otherwise -> do
              ci <- withStore $ \db -> do
                liftIO $ updateFileCancelled db user fileId CIFSRcvError
                getChatItemByFileId db vr user fileId
              agentXFTPDeleteRcvFile aFileId fileId
              toView $ CRRcvFileError user ci e

processAgentMessageConn :: forall m. ChatMonad m => VersionRange -> User -> ACorrId -> ConnId -> ACommand 'Agent 'AEConn -> m ()
processAgentMessageConn vr user@User {userId} corrId agentConnId agentMessage = do
  entity <- withStore (\db -> getConnectionEntity db vr user $ AgentConnId agentConnId) >>= updateConnStatus
  case agentMessage of
    END -> case entity of
      RcvDirectMsgConnection _ (Just ct) -> toView $ CRContactAnotherClient user ct
      _ -> toView $ CRSubscriptionEnd user entity
    MSGNTF smpMsgInfo -> toView $ CRNtfMessage user entity $ ntfMsgInfo smpMsgInfo
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
    updateConnStatus :: ConnectionEntity -> m ConnectionEntity
    updateConnStatus acEntity = case agentMsgConnStatus agentMessage of
      Just connStatus -> do
        let conn = (entityConnection acEntity) {connStatus}
        withStore' $ \db -> updateConnectionStatus db conn connStatus
        pure $ updateEntityConnStatus acEntity connStatus
      Nothing -> pure acEntity

    agentMsgConnStatus :: ACommand 'Agent e -> Maybe ConnStatus
    agentMsgConnStatus = \case
      CONF {} -> Just ConnRequested
      INFO _ -> Just ConnSndReady
      CON -> Just ConnReady
      _ -> Nothing

    processDirectMessage :: ACommand 'Agent e -> ConnectionEntity -> Connection -> Maybe Contact -> m ()
    processDirectMessage agentMsg connEntity conn@Connection {connId, peerChatVRange, viaUserContactLink, customUserProfileId, connectionCode} = \case
      Nothing -> case agentMsg of
        CONF confId _ connInfo -> do
          -- [incognito] send saved profile
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
          let profileToSend = userProfileToSend user (fromLocalProfile <$> incognitoProfile) Nothing
          conn' <- saveConnInfo conn connInfo
          -- [async agent commands] no continuation needed, but command should be asynchronous for stability
          allowAgentConnectionAsync user conn' confId $ XInfo profileToSend
        INFO connInfo -> do
          _conn' <- saveConnInfo conn connInfo
          pure ()
        MSG meta _msgFlags msgBody -> do
          cmdId <- createAckCmd conn
          -- TODO only acknowledge without saving message?
          -- probably this branch is never executed, so there should be no reason
          -- to save message if contact hasn't been created yet - chat item isn't created anyway
          withAckMessage agentConnId cmdId meta $ do
            (_conn', _) <- saveDirectRcvMSG conn meta cmdId msgBody
            pure False
        SENT msgId ->
          sentMsgDeliveryEvent conn msgId
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \CommandData {cmdFunction, cmdId} ->
            when (cmdFunction == CFAckMessage) $ ackMsgDeliveryEvent conn cmdId
        MERR _ err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          incAuthErrCounter connEntity conn err
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
          cmdId <- createAckCmd conn
          withAckMessage agentConnId cmdId msgMeta $ do
            (conn', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveDirectRcvMSG conn msgMeta cmdId msgBody
            let ct' = ct {activeConn = Just conn'} :: Contact
            assertDirectAllowed user MDRcv ct' $ toCMEventTag event
            updateChatLock "directMessage" event
            case event of
              XMsgNew mc -> newContentMessage ct' mc msg msgMeta
              XMsgFileDescr sharedMsgId fileDescr -> messageFileDescription ct' sharedMsgId fileDescr msgMeta
              XMsgUpdate sharedMsgId mContent ttl live -> messageUpdate ct' sharedMsgId mContent msg msgMeta ttl live
              XMsgDel sharedMsgId _ -> messageDelete ct' sharedMsgId msg msgMeta
              XMsgReact sharedMsgId _ reaction add -> directMsgReaction ct' sharedMsgId reaction add msg msgMeta
              -- TODO discontinue XFile
              XFile fInv -> processFileInvitation' ct' fInv msg msgMeta
              XFileCancel sharedMsgId -> xFileCancel ct' sharedMsgId msgMeta
              XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInv ct' sharedMsgId fileConnReq_ fName msgMeta
              XInfo p -> xInfo ct' p
              XDirectDel -> xDirectDel ct' msg msgMeta
              XGrpInv gInv -> processGroupInvitation ct' gInv msg msgMeta
              XInfoProbe probe -> xInfoProbe (COMContact ct') probe
              XInfoProbeCheck probeHash -> xInfoProbeCheck (COMContact ct') probeHash
              XInfoProbeOk probe -> xInfoProbeOk (COMContact ct') probe
              XCallInv callId invitation -> xCallInv ct' callId invitation msg msgMeta
              XCallOffer callId offer -> xCallOffer ct' callId offer msg msgMeta
              XCallAnswer callId answer -> xCallAnswer ct' callId answer msg msgMeta
              XCallExtra callId extraInfo -> xCallExtra ct' callId extraInfo msg msgMeta
              XCallEnd callId -> xCallEnd ct' callId msg msgMeta
              BFileChunk sharedMsgId chunk -> bFileChunk ct' sharedMsgId chunk msgMeta
              _ -> messageError $ "unsupported message: " <> T.pack (show event)
            let Contact {chatSettings = ChatSettings {sendRcpts}} = ct'
            pure $ fromMaybe (sendRcptsContacts user) sendRcpts && hasDeliveryReceipt (toCMEventTag event)
        RCVD msgMeta msgRcpt ->
          withAckMessage' agentConnId conn msgMeta $
            directMsgReceived ct conn msgMeta msgRcpt
        CONF confId _ connInfo -> do
          ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
          conn' <- updatePeerChatVRange conn chatVRange
          case chatMsgEvent of
            -- confirming direct connection with a member
            XGrpMemInfo _memId _memProfile -> do
              -- TODO check member ID
              -- TODO update member profile
              -- [async agent commands] no continuation needed, but command should be asynchronous for stability
              allowAgentConnectionAsync user conn' confId XOk
            XInfo profile -> do
              ct' <- processContactProfileUpdate ct profile False `catchChatError` const (pure ct)
              -- [incognito] send incognito profile
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore $ \db -> getProfileById db userId profileId
              let p = userProfileToSend user (fromLocalProfile <$> incognitoProfile) (Just ct')
              allowAgentConnectionAsync user conn' confId $ XInfo p
              void $ withStore' $ \db -> resetMemberContactFields db ct'
            _ -> messageError "CONF for existing contact must have x.grp.mem.info or x.info"
        INFO connInfo -> do
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
        CON ->
          withStore' (\db -> getViaGroupMember db vr user ct) >>= \case
            Nothing -> do
              -- [incognito] print incognito profile used for this contact
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
              setContactNetworkStatus ct NSConnected
              toView $ CRContactConnected user ct (fmap fromLocalProfile incognitoProfile)
              when (directOrUsed ct) $ createFeatureEnabledItems ct
              when (contactConnInitiated conn) $ do
                let Connection {groupLinkId} = conn
                    doProbeContacts = isJust groupLinkId
                probeMatchingContactsAndMembers ct (contactConnIncognito ct) doProbeContacts
                withStore' $ \db -> resetContactConnInitiated db user conn
              forM_ viaUserContactLink $ \userContactLinkId -> do
                ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
                let (UserContactLink {autoAccept}, groupId_, gLinkMemRole) = ucl
                forM_ autoAccept $ \(AutoAccept {autoReply = mc_}) ->
                  forM_ mc_ $ \mc -> do
                    (msg, _) <- sendDirectContactMessage ct (XMsgNew $ MCSimple (extMsgContent mc Nothing))
                    ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc)
                    toView $ CRNewChatItem user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
                forM_ groupId_ $ \groupId -> do
                  groupInfo <- withStore $ \db -> getGroupInfo db vr user groupId
                  subMode <- chatReadVar subscriptionMode
                  groupConnIds <- createAgentConnectionAsync user CFCreateConnGrpInv True SCMInvitation subMode
                  gVar <- asks random
                  withStore $ \db -> createNewContactMemberAsync db gVar user groupInfo ct gLinkMemRole groupConnIds (fromJVersionRange peerChatVRange) subMode
            Just (gInfo, m@GroupMember {activeConn}) ->
              when (maybe False ((== ConnReady) . connStatus) activeConn) $ do
                notifyMemberConnected gInfo m $ Just ct
                let connectedIncognito = contactConnIncognito ct || incognitoMembership gInfo
                when (memberCategory m == GCPreMember) $ probeMatchingContactsAndMembers ct connectedIncognito True
        SENT msgId -> do
          sentMsgDeliveryEvent conn msgId
          checkSndInlineFTComplete conn msgId
          updateDirectItemStatus ct conn msgId $ CISSndSent SSPComplete
        SWITCH qd phase cStats -> do
          toView $ CRContactSwitch user ct (SwitchProgress qd phase cStats)
          when (phase `elem` [SPStarted, SPCompleted]) $ case qd of
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
                getDirectChatItemsLast db user contactId 1 ""
                  >>= liftIO
                    . mapM (\(ci, content') -> updateDirectChatItem' db user contactId ci content' False Nothing)
                    . (mdeUpdatedCI e <=< headMaybe)
              case ci_ of
                Just ci -> toView $ CRChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
                _ -> do
                  toView $ CRContactRatchetSync user ct (RatchetSyncProgress rss cStats)
                  createInternalChatItem user (CDDirectRcv ct) (CIRcvDecryptionError mde n) Nothing
            headMaybe = \case
              x : _ -> Just x
              _ -> Nothing
            ratchetSyncEventItem ct' = do
              toView $ CRContactRatchetSync user ct' (RatchetSyncProgress rss cStats)
              createInternalChatItem user (CDDirectRcv ct') (CIRcvConnEvent $ RCERatchetSync rss) Nothing
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \CommandData {cmdFunction, cmdId} ->
            when (cmdFunction == CFAckMessage) $ ackMsgDeliveryEvent conn cmdId
        MERR msgId err -> do
          updateDirectItemStatus ct conn msgId $ agentErrToItemStatus err
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          incAuthErrCounter connEntity conn err
        ERR err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    processGroupMessage :: ACommand 'Agent e -> ConnectionEntity -> Connection -> GroupInfo -> GroupMember -> m ()
    processGroupMessage agentMsg connEntity conn@Connection {connId, connectionCode} gInfo@GroupInfo {groupId, groupProfile, membership, chatSettings} m = case agentMsg of
      INV (ACR _ cReq) ->
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cReq of
            groupConnReq@(CRInvitationUri _ _) -> case cmdFunction of
              -- [async agent commands] XGrpMemIntro continuation on receiving INV
              CFCreateConnGrpMemInv
                | isCompatibleRange (fromJVersionRange $ peerChatVRange conn) groupNoDirectVRange -> sendWithoutDirectCReq
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
                ct <- withStore $ \db -> getContactViaMember db user m
                withStore' $ \db -> setNewContactMemberConnRequest db user m cReq
                groupLinkId <- withStore' $ \db -> getGroupLinkId db user gInfo
                sendGrpInvitation ct m groupLinkId
                toView $ CRSentGroupInvitation user gInfo ct m
                where
                  sendGrpInvitation :: Contact -> GroupMember -> Maybe GroupLinkId -> m ()
                  sendGrpInvitation ct GroupMember {memberId, memberRole = memRole} groupLinkId = do
                    let GroupMember {memberRole = userRole, memberId = userMemberId} = membership
                        groupInv = GroupInvitation (MemberIdRole userMemberId userRole) (MemberIdRole memberId memRole) cReq groupProfile groupLinkId
                    (_msg, _) <- sendDirectContactMessage ct $ XGrpInv groupInv
                    -- we could link chat item with sent group invitation message (_msg)
                    createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
              _ -> throwChatError $ CECommandError "unexpected cmdFunction"
            CRContactUri _ -> throwChatError $ CECommandError "unexpected ConnectionRequestUri type"
      CONF confId _ connInfo -> do
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
                    -- TODO update member profile
                    -- [async agent commands] no continuation needed, but command should be asynchronous for stability
                    allowAgentConnectionAsync user conn' confId $ XGrpMemInfo membership.memberId (fromLocalProfile $ memberProfile membership)
                | otherwise -> messageError "x.grp.mem.info: memberId is different from expected"
              _ -> messageError "CONF from member must have x.grp.mem.info"
      INFO connInfo -> do
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
      CON -> do
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
            createGroupFeatureItems gInfo m
            let GroupInfo {groupProfile = GroupProfile {description}} = gInfo
            memberConnectedChatItem gInfo m
            forM_ description $ groupDescriptionChatItem gInfo m
          GCInviteeMember -> do
            memberConnectedChatItem gInfo m
            toView $ CRJoinedGroupMember user gInfo m {memberStatus = GSMemConnected}
            let Connection {viaUserContactLink} = conn
            when (isJust viaUserContactLink && isNothing (memberContactId m)) sendXGrpLinkMem
            members <- withStore' $ \db -> getGroupMembers db user gInfo
            void . sendGroupMessage user gInfo members . XGrpMemNew $ memberInfo m
            sendIntroductions members
            when (groupFeatureAllowed SGFHistory gInfo) sendHistory
            where
              sendXGrpLinkMem = do
                let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                    profileToSend = profileToSendOnAccept user profileMode
                void $ sendDirectMessage conn (XGrpLinkMem profileToSend) (GroupId groupId)
              sendIntroductions members = do
                intros <- withStore' $ \db -> createIntroductions db (maxVersion vr) members m
                shuffledIntros <- liftIO $ shuffleIntros intros
                if isCompatibleRange (memberChatVRange' m) batchSendVRange
                  then do
                    let events = map (XGrpMemIntro . memberInfo . reMember) shuffledIntros
                    forM_ (L.nonEmpty events) $ \events' ->
                      sendGroupMemberMessages user conn events' groupId
                  else forM_ shuffledIntros $ \intro ->
                    processIntro intro `catchChatError` (toView . CRChatError (Just user))
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
                void $ sendDirectMessage conn (XGrpMemIntro $ memberInfo (reMember intro)) (GroupId groupId)
                withStore' $ \db -> updateIntroStatus db introId GMIntroSent
              sendHistory =
                when (isCompatibleRange (memberChatVRange' m) batchSendVRange) $ do
                  (errs, items) <- partitionEithers <$> withStore' (\db -> getGroupHistoryItems db user gInfo 100)
                  (errs', events) <- partitionEithers <$> mapM (tryChatError . itemForwardEvents) items
                  let errors = map ChatErrorStore errs <> errs'
                  unless (null errors) $ toView $ CRChatErrors (Just user) errors
                  forM_ (L.nonEmpty $ concat events) $ \events' ->
                    sendGroupMemberMessages user conn events' groupId
              itemForwardEvents :: CChatItem 'CTGroup -> m [ChatMsgEvent 'Json]
              itemForwardEvents cci = case cci of
                (CChatItem SMDRcv ci@ChatItem {chatDir = CIGroupRcv sender, content = CIRcvMsgContent mc, file}) -> do
                  fInvDescr_ <- join <$> forM file getRcvFileInvDescr
                  processContentItem sender ci mc fInvDescr_
                (CChatItem SMDSnd ci@ChatItem {content = CISndMsgContent mc, file}) -> do
                  fInvDescr_ <- join <$> forM file getSndFileInvDescr
                  processContentItem membership ci mc fInvDescr_
                _ -> pure []
                where
                  getRcvFileInvDescr :: CIFile 'MDRcv -> m (Maybe (FileInvitation, RcvFileDescrText))
                  getRcvFileInvDescr ciFile@CIFile {fileId, fileProtocol, fileStatus} = do
                    expired <- fileExpired
                    if fileProtocol /= FPXFTP || fileStatus == CIFSRcvCancelled || expired
                      then pure Nothing
                      else do
                        rfd <- withStore $ \db -> getRcvFileDescrByRcvFileId db fileId
                        pure $ invCompleteDescr ciFile rfd
                  getSndFileInvDescr :: CIFile 'MDSnd -> m (Maybe (FileInvitation, RcvFileDescrText))
                  getSndFileInvDescr ciFile@CIFile {fileId, fileProtocol, fileStatus} = do
                    expired <- fileExpired
                    if fileProtocol /= FPXFTP || fileStatus == CIFSSndCancelled || expired
                      then pure Nothing
                      else do
                        -- can also lookup in extra_xftp_file_descriptions, though it can be empty;
                        -- would be best if snd file had a single rcv description for all members saved in files table
                        rfd <- withStore $ \db -> getRcvFileDescrBySndFileId db fileId
                        pure $ invCompleteDescr ciFile rfd
                  fileExpired :: m Bool
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
                  processContentItem :: GroupMember -> ChatItem 'CTGroup d -> MsgContent -> Maybe (FileInvitation, RcvFileDescrText) -> m [ChatMsgEvent Json]
                  processContentItem sender ChatItem {meta, quotedItem} mc fInvDescr_ =
                    if isNothing fInvDescr_ && not (msgContentHasText mc)
                      then pure []
                      else do
                        let CIMeta {itemTs, itemSharedMsgId, itemTimed} = meta
                            quotedItemId_ = quoteItemId =<< quotedItem
                            fInv_ = fst <$> fInvDescr_
                        (msgContainer, _) <- prepareGroupMsg user gInfo mc quotedItemId_ fInv_ itemTimed False
                        let senderVRange = memberChatVRange' sender
                            xMsgNewChatMsg = ChatMessage {chatVRange = senderVRange, msgId = itemSharedMsgId, chatMsgEvent = XMsgNew msgContainer}
                        fileDescrEvents <- case (snd <$> fInvDescr_, itemSharedMsgId) of
                          (Just fileDescrText, Just msgId) -> do
                            parts <- splitFileDescr fileDescrText
                            pure . toList $ L.map (XMsgFileDescr msgId) parts
                          _ -> pure []
                        let fileDescrChatMsgs = map (ChatMessage senderVRange Nothing) fileDescrEvents
                            GroupMember {memberId} = sender
                            msgForwardEvents = map (\cm -> XGrpMsgForward memberId cm itemTs) (xMsgNewChatMsg : fileDescrChatMsgs)
                        pure msgForwardEvents
          _ -> do
            let memCategory = memberCategory m
            withStore' (\db -> getViaGroupContact db user m) >>= \case
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
              sendXGrpMemCon = \case
                GCPreMember ->
                  forM_ (invitedByGroupMemberId membership) $ \hostId -> do
                    host <- withStore $ \db -> getGroupMember db user groupId hostId
                    forM_ (memberConn host) $ \hostConn ->
                      void $ sendDirectMessage hostConn (XGrpMemCon m.memberId) (GroupId groupId)
                GCPostMember ->
                  forM_ (invitedByGroupMemberId m) $ \invitingMemberId -> do
                    im <- withStore $ \db -> getGroupMember db user groupId invitingMemberId
                    forM_ (memberConn im) $ \imConn ->
                      void $ sendDirectMessage imConn (XGrpMemCon m.memberId) (GroupId groupId)
                _ -> messageWarning "sendXGrpMemCon: member category GCPreMember or GCPostMember is expected"
      MSG msgMeta _msgFlags msgBody -> do
        checkIntegrityCreateItem (CDGroupRcv gInfo m) msgMeta `catchChatError` \_ -> pure ()
        cmdId <- createAckCmd conn
        let aChatMsgs = parseChatMessages msgBody
        withAckMessage agentConnId cmdId msgMeta $ do
          forM_ aChatMsgs $ \case
            Right (ACMsg _ chatMsg) ->
              processEvent cmdId chatMsg `catchChatError` \e -> toView $ CRChatError (Just user) e
            Left e -> toView $ CRChatError (Just user) (ChatError . CEException $ "error parsing chat message: " <> e)
          checkSendRcpt $ rights aChatMsgs
        -- currently only a single message is forwarded
        when (membership.memberRole >= GRAdmin) $ case aChatMsgs of
          [Right (ACMsg _ chatMsg)] -> forwardMsg_ chatMsg
          _ -> pure ()
        where
          brokerTs = metaBrokerTs msgMeta
          processEvent :: MsgEncodingI e => CommandId -> ChatMessage e -> m ()
          processEvent cmdId chatMsg = do
            (m', conn', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveGroupRcvMsg user groupId m conn msgMeta cmdId msgBody chatMsg
            updateChatLock "groupMessage" event
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
              XInfo p -> xInfoMember gInfo m' p
              XGrpLinkMem p -> xGrpLinkMem gInfo m' conn' p
              XGrpMemNew memInfo -> xGrpMemNew gInfo m' memInfo msg brokerTs
              XGrpMemIntro memInfo -> xGrpMemIntro gInfo m' memInfo
              XGrpMemInv memId introInv -> xGrpMemInv gInfo m' memId introInv
              XGrpMemFwd memInfo introInv -> xGrpMemFwd gInfo m' memInfo introInv
              XGrpMemRole memId memRole -> xGrpMemRole gInfo m' memId memRole msg brokerTs
              XGrpMemCon memId -> xGrpMemCon gInfo m' memId
              XGrpMemDel memId -> xGrpMemDel gInfo m' memId msg brokerTs
              XGrpLeave -> xGrpLeave gInfo m' msg brokerTs
              XGrpDel -> xGrpDel gInfo m' msg brokerTs
              XGrpInfo p' -> xGrpInfo gInfo m' p' msg brokerTs
              XGrpDirectInv connReq mContent_ -> memberCanSend m' $ xGrpDirectInv gInfo m' conn' connReq mContent_ msg brokerTs
              XGrpMsgForward memberId msg' msgTs -> xGrpMsgForward gInfo m' memberId msg' msgTs
              XInfoProbe probe -> xInfoProbe (COMGroupMember m') probe
              XInfoProbeCheck probeHash -> xInfoProbeCheck (COMGroupMember m') probeHash
              XInfoProbeOk probe -> xInfoProbeOk (COMGroupMember m') probe
              BFileChunk sharedMsgId chunk -> bFileChunkGroup gInfo sharedMsgId chunk msgMeta
              _ -> messageError $ "unsupported message: " <> T.pack (show event)
          checkSendRcpt :: [AChatMessage] -> m Bool
          checkSendRcpt aChatMsgs = do
            currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
            let GroupInfo {chatSettings = ChatSettings {sendRcpts}} = gInfo
            pure $
              fromMaybe (sendRcptsSmallGroups user) sendRcpts
                && any aChatMsgHasReceipt aChatMsgs
                && currentMemCount <= smallGroupsRcptsMemLimit
            where
              aChatMsgHasReceipt (ACMsg _ ChatMessage {chatMsgEvent}) =
                hasDeliveryReceipt (toCMEventTag chatMsgEvent)
          forwardMsg_ :: MsgEncodingI e => ChatMessage e -> m ()
          forwardMsg_ chatMsg =
            forM_ (forwardedGroupMsg chatMsg) $ \chatMsg' -> do
              ChatConfig {highlyAvailable} <- asks config
              -- members introduced to this invited member
              introducedMembers <-
                if memberCategory m == GCInviteeMember
                  then withStore' $ \db -> getForwardIntroducedMembers db user m highlyAvailable
                  else pure []
              -- invited members to which this member was introduced
              invitedMembers <- withStore' $ \db -> getForwardInvitedMembers db user m highlyAvailable
              let ms = introducedMembers <> invitedMembers
                  msg = XGrpMsgForward m.memberId chatMsg' brokerTs
              unless (null ms) . void $
                sendGroupMessage user gInfo ms msg
      RCVD msgMeta msgRcpt ->
        withAckMessage' agentConnId conn msgMeta $
          groupMsgReceived gInfo m conn msgMeta msgRcpt
      SENT msgId -> do
        sentMsgDeliveryEvent conn msgId
        checkSndInlineFTComplete conn msgId
        updateGroupItemStatus gInfo m conn msgId $ CISSndSent SSPComplete
      SWITCH qd phase cStats -> do
        toView $ CRGroupMemberSwitch user gInfo m (SwitchProgress qd phase cStats)
        when (phase `elem` [SPStarted, SPCompleted]) $ case qd of
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
                  . mapM (\(ci, content') -> updateGroupChatItem db user groupId ci content' False Nothing)
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
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction, cmdId} ->
          when (cmdFunction == CFAckMessage) $ ackMsgDeliveryEvent conn cmdId
      MERR msgId err -> do
        chatItemId_ <- withStore' $ \db -> getChatItemIdByAgentMsgId db connId msgId
        forM_ chatItemId_ $ \itemId -> do
          let GroupMember {groupMemberId} = m
          updateGroupMemSndStatus itemId groupMemberId $ agentErrToItemStatus err
        -- group errors are silenced to reduce load on UI event log
        -- toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        incAuthErrCounter connEntity conn err
      ERR err -> do
        toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()

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

    processSndFileConn :: ACommand 'Agent e -> ConnectionEntity -> Connection -> SndFileTransfer -> m ()
    processSndFileConn agentMsg connEntity conn ft@SndFileTransfer {fileId, fileName, fileStatus} =
      case agentMsg of
        -- SMP CONF for SndFileConnection happens for direct file protocol
        -- when recipient of the file "joins" connection created by the sender
        CONF confId _ connInfo -> do
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
        CON -> do
          ci <- withStore $ \db -> do
            liftIO $ updateSndFileStatus db ft FSConnected
            updateDirectCIFileStatus db vr user fileId $ CIFSSndTransfer 0 1
          toView $ CRSndFileStart user ci ft
          sendFileChunk user ft
        SENT msgId -> do
          withStore' $ \db -> updateSndFileChunkSent db ft msgId
          unless (fileStatus == FSCancelled) $ sendFileChunk user ft
        MERR _ err -> do
          cancelSndFileTransfer user ft True >>= mapM_ (deleteAgentConnectionAsync user)
          case err of
            SMP SMP.AUTH -> unless (fileStatus == FSCancelled) $ do
              ci <- withStore $ \db -> do
                getChatRefByFileId db user fileId >>= \case
                  ChatRef CTDirect _ -> liftIO $ updateFileCancelled db user fileId CIFSSndCancelled
                  _ -> pure ()
                getChatItemByFileId db vr user fileId
              toView $ CRSndFileRcvCancelled user ci ft
            _ -> throwChatError $ CEFileSend fileId err
        MSG meta _ _ -> withAckMessage' agentConnId conn meta $ pure ()
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        ERR err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    processRcvFileConn :: ACommand 'Agent e -> ConnectionEntity -> Connection -> RcvFileTransfer -> m ()
    processRcvFileConn agentMsg connEntity conn ft@RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}, grpMemberId} =
      case agentMsg of
        INV (ACR _ cReq) ->
          withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
            case cReq of
              fileInvConnReq@(CRInvitationUri _ _) -> case cmdFunction of
                -- [async agent commands] direct XFileAcptInv continuation on receiving INV
                CFCreateConnFileInvDirect -> do
                  ct <- withStore $ \db -> getContactByFileId db user fileId
                  sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
                  void $ sendDirectContactMessage ct (XFileAcptInv sharedMsgId (Just fileInvConnReq) fileName)
                -- [async agent commands] group XFileAcptInv continuation on receiving INV
                CFCreateConnFileInvGroup -> case grpMemberId of
                  Just gMemberId -> do
                    GroupMember {groupId, activeConn} <- withStore $ \db -> getGroupMemberById db user gMemberId
                    case activeConn of
                      Just gMemberConn -> do
                        sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
                        void $ sendDirectMessage gMemberConn (XFileAcptInv sharedMsgId (Just fileInvConnReq) fileName) $ GroupId groupId
                      _ -> throwChatError $ CECommandError "no GroupMember activeConn"
                  _ -> throwChatError $ CECommandError "no grpMemberId"
                _ -> throwChatError $ CECommandError "unexpected cmdFunction"
              CRContactUri _ -> throwChatError $ CECommandError "unexpected ConnectionRequestUri type"
        -- SMP CONF for RcvFileConnection happens for group file protocol
        -- when sender of the file "joins" connection created by the recipient
        -- (sender doesn't create connections for all group members)
        CONF confId _ connInfo -> do
          ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
          conn' <- updatePeerChatVRange conn chatVRange
          case chatMsgEvent of
            XOk -> allowAgentConnectionAsync user conn' confId XOk -- [async agent commands] no continuation needed, but command should be asynchronous for stability
            _ -> pure ()
        CON -> startReceivingFile user fileId
        MSG meta _ msgBody -> do
          parseFileChunk msgBody >>= receiveFileChunk ft (Just conn) meta
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        MERR _ err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          incAuthErrCounter connEntity conn err
        ERR err -> do
          toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    receiveFileChunk :: RcvFileTransfer -> Maybe Connection -> MsgMeta -> FileChunk -> m ()
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
              else ack $ appendFileChunk ft chunkNo chunk False
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
          RcvChunkDuplicate -> ack $ pure ()
          RcvChunkError -> badRcvFileChunk ft $ "incorrect chunk number " <> show chunkNo
      where
        ack a = case conn_ of
          Just conn -> withAckMessage' agentConnId conn meta a
          Nothing -> a

    processUserContactRequest :: ACommand 'Agent e -> ConnectionEntity -> Connection -> UserContact -> m ()
    processUserContactRequest agentMsg connEntity conn UserContact {userContactLinkId} = case agentMsg of
      REQ invId _ connInfo -> do
        ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
        case chatMsgEvent of
          XContact p xContactId_ -> profileContactRequest invId chatVRange p xContactId_
          XInfo p -> profileContactRequest invId chatVRange p Nothing
          -- TODO show/log error, other events in contact request
          _ -> pure ()
      MERR _ err -> do
        toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        incAuthErrCounter connEntity conn err
      ERR err -> do
        toView $ CRChatError (Just user) (ChatErrorAgent err $ Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()
      where
        profileContactRequest :: InvitationId -> VersionRange -> Profile -> Maybe XContactId -> m ()
        profileContactRequest invId chatVRange p xContactId_ = do
          withStore (\db -> createOrUpdateContactRequest db user userContactLinkId invId chatVRange p xContactId_) >>= \case
            CORContact contact -> toView $ CRContactRequestAlreadyAccepted user contact
            CORRequest cReq -> do
              ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
              let (UserContactLink {autoAccept}, groupId_, gLinkMemRole) = ucl
              case autoAccept of
                Just AutoAccept {acceptIncognito} -> case groupId_ of
                  Nothing -> do
                    -- [incognito] generate profile to send, create connection with incognito profile
                    incognitoProfile <- if acceptIncognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
                    ct <- acceptContactRequestAsync user cReq incognitoProfile True
                    toView $ CRAcceptingContactRequest user ct
                  Just groupId -> do
                    gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
                    let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                    if isCompatibleRange chatVRange groupLinkNoContactVRange
                      then do
                        mem <- acceptGroupJoinRequestAsync user gInfo cReq gLinkMemRole profileMode
                        createInternalChatItem user (CDGroupRcv gInfo mem) (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
                        toView $ CRAcceptingGroupJoinRequestMember user gInfo mem
                      else do
                        ct <- acceptContactRequestAsync user cReq profileMode False
                        toView $ CRAcceptingGroupJoinRequest user gInfo ct
                _ -> toView $ CRReceivedContactRequest user cReq

    memberCanSend :: GroupMember -> m () -> m ()
    memberCanSend mem a
      | mem.memberRole <= GRObserver = messageError "member is not allowed to send messages"
      | otherwise = a

    incAuthErrCounter :: ConnectionEntity -> Connection -> AgentErrorType -> m ()
    incAuthErrCounter connEntity conn err = do
      case err of
        SMP SMP.AUTH -> do
          authErrCounter' <- withStore' $ \db -> incConnectionAuthErrCounter db user conn
          when (authErrCounter' >= authErrDisableCount) $ do
            toView $ CRConnectionDisabled connEntity
        _ -> pure ()

    updateChatLock :: MsgEncodingI enc => String -> ChatMsgEvent enc -> m ()
    updateChatLock name event = do
      l <- asks chatLock
      atomically $ tryReadTMVar l >>= mapM_ (swapTMVar l . (<> s))
      where
        s = " " <> name <> "=" <> B.unpack (strEncode $ toCMEventTag event)

    withCompletedCommand :: forall e. AEntityI e => Connection -> ACommand 'Agent e -> (CommandData -> m ()) -> m ()
    withCompletedCommand Connection {connId} agentMsg action = do
      let agentMsgTag = APCT (sAEntity @e) $ aCommandTag agentMsg
      cmdData_ <- withStore' $ \db -> getCommandDataByCorrId db user corrId
      case cmdData_ of
        Just cmdData@CommandData {cmdId, cmdConnId = Just cmdConnId', cmdFunction}
          | connId == cmdConnId' && (agentMsgTag == commandExpectedResponse cmdFunction || agentMsgTag == APCT SAEConn ERR_) -> do
              withStore' $ \db -> deleteCommand db user cmdId
              action cmdData
          | otherwise -> err cmdId $ "not matching connection id or unexpected response, corrId = " <> show corrId
        Just CommandData {cmdId, cmdConnId = Nothing} -> err cmdId $ "no command connection id, corrId = " <> show corrId
        Nothing -> throwChatError . CEAgentCommandError $ "command not found, corrId = " <> show corrId
      where
        err cmdId msg = do
          withStore' $ \db -> updateCommandStatus db user cmdId CSError
          throwChatError . CEAgentCommandError $ msg

    createAckCmd :: Connection -> m CommandId
    createAckCmd Connection {connId} = do
      withStore' $ \db -> createCommand db user (Just connId) CFAckMessage

    withAckMessage' :: ConnId -> Connection -> MsgMeta -> m () -> m ()
    withAckMessage' cId conn msgMeta action = do
      cmdId <- createAckCmd conn
      withAckMessage cId cmdId msgMeta $ action $> False

    withAckMessage :: ConnId -> CommandId -> MsgMeta -> m Bool -> m ()
    withAckMessage cId cmdId msgMeta action = do
      -- [async agent commands] command should be asynchronous, continuation is ackMsgDeliveryEvent
      -- TODO catching error and sending ACK after an error, particularly if it is a database error, will result in the message not processed (and no notification to the user).
      -- Possible solutions are:
      -- 1) retry processing several times
      -- 2) stabilize database
      -- 3) show screen of death to the user asking to restart
      tryChatError action >>= \case
        Right withRcpt -> ackMsg cId cmdId msgMeta $ if withRcpt then Just "" else Nothing
        Left e -> ackMsg cId cmdId msgMeta Nothing >> throwError e

    ackMsg :: ConnId -> CommandId -> MsgMeta -> Maybe MsgReceiptInfo -> m ()
    ackMsg cId cmdId MsgMeta {recipient = (msgId, _)} rcpt = withAgent $ \a -> ackMessageAsync a (aCorrId cmdId) cId msgId rcpt

    ackMsgDeliveryEvent :: Connection -> CommandId -> m ()
    ackMsgDeliveryEvent Connection {connId} ackCmdId =
      withStore' $ \db -> updateRcvMsgDeliveryStatus db connId ackCmdId MDSRcvAcknowledged

    sentMsgDeliveryEvent :: Connection -> AgentMsgId -> m ()
    sentMsgDeliveryEvent Connection {connId} msgId =
      withStore' $ \db -> updateSndMsgDeliveryStatus db connId msgId MDSSndSent

    agentErrToItemStatus :: AgentErrorType -> CIStatus 'MDSnd
    agentErrToItemStatus (SMP AUTH) = CISSndErrorAuth
    agentErrToItemStatus err = CISSndError . T.unpack . safeDecodeUtf8 $ strEncode err

    badRcvFileChunk :: RcvFileTransfer -> String -> m ()
    badRcvFileChunk ft err =
      unless (rcvFileCompleteOrCancelled ft) $ do
        cancelRcvFileTransfer user ft >>= mapM_ (deleteAgentConnectionAsync user)
        throwChatError $ CEFileRcvChunk err

    memberConnectedChatItem :: GroupInfo -> GroupMember -> m ()
    memberConnectedChatItem gInfo m =
      -- ts should be broker ts but we don't have it for CON
      createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvGroupEvent RGEMemberConnected) Nothing

    groupDescriptionChatItem :: GroupInfo -> GroupMember -> Text -> m ()
    groupDescriptionChatItem gInfo m descr =
      createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvMsgContent $ MCText descr) Nothing

    notifyMemberConnected :: GroupInfo -> GroupMember -> Maybe Contact -> m ()
    notifyMemberConnected gInfo m ct_ = do
      memberConnectedChatItem gInfo m
      mapM_ (`setContactNetworkStatus` NSConnected) ct_
      toView $ CRConnectedToGroupMember user gInfo m ct_

    probeMatchingContactsAndMembers :: Contact -> IncognitoEnabled -> Bool -> m ()
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
              then map COMContact <$> withStore' (\db -> getMatchingContacts db user ct)
              else pure []
          ms <- map COMGroupMember <$> withStore' (\db -> getMatchingMembers db user ct)
          sendProbeHashes (cs <> ms) probe probeId
        else sendProbe . Probe =<< liftIO (encodedRandomBytes gVar 32)
      where
        sendProbe :: Probe -> m ()
        sendProbe probe = void . sendDirectContactMessage ct $ XInfoProbe probe

    probeMatchingMemberContact :: GroupMember -> IncognitoEnabled -> m ()
    probeMatchingMemberContact GroupMember {activeConn = Nothing} _ = pure ()
    probeMatchingMemberContact m@GroupMember {groupId, activeConn = Just conn} connectedIncognito = do
      gVar <- asks random
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      if contactMerge && not connectedIncognito
        then do
          (probe, probeId) <- withStore $ \db -> createSentProbe db gVar userId $ COMGroupMember m
          sendProbe probe
          cs <- map COMContact <$> withStore' (\db -> getMatchingMemberContacts db user m)
          sendProbeHashes cs probe probeId
        else sendProbe . Probe =<< liftIO (encodedRandomBytes gVar 32)
      where
        sendProbe :: Probe -> m ()
        sendProbe probe = void $ sendDirectMessage conn (XInfoProbe probe) (GroupId groupId)

    sendProbeHashes :: [ContactOrMember] -> Probe -> Int64 -> m ()
    sendProbeHashes cgms probe probeId =
      forM_ cgms $ \cgm -> sendProbeHash cgm `catchChatError` \_ -> pure ()
      where
        probeHash = ProbeHash $ C.sha256Hash (unProbe probe)
        sendProbeHash :: ContactOrMember -> m ()
        sendProbeHash cgm@(COMContact c) = do
          void . sendDirectContactMessage c $ XInfoProbeCheck probeHash
          withStore' $ \db -> createSentProbeHash db userId probeId cgm
        sendProbeHash (COMGroupMember GroupMember {activeConn = Nothing}) = pure ()
        sendProbeHash cgm@(COMGroupMember m@GroupMember {groupId, activeConn = Just conn}) =
          when (memberCurrent m) $ do
            void $ sendDirectMessage conn (XInfoProbeCheck probeHash) (GroupId groupId)
            withStore' $ \db -> createSentProbeHash db userId probeId cgm

    messageWarning :: Text -> m ()
    messageWarning = toView . CRMessageError user "warning"

    messageError :: Text -> m ()
    messageError = toView . CRMessageError user "error"

    newContentMessage :: Contact -> MsgContainer -> RcvMessage -> MsgMeta -> m ()
    newContentMessage ct@Contact {contactUsed} mc msg@RcvMessage {sharedMsgId_} msgMeta = do
      unless contactUsed $ withStore' $ \db -> updateContactUsed db user ct
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
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
          toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci {reactions})

    autoAcceptFile :: Maybe (RcvFileTransfer, CIFile 'MDRcv) -> m ()
    autoAcceptFile = mapM_ $ \(ft, CIFile {fileSize}) -> do
      ChatConfig {autoAcceptFileSize = sz} <- asks config
      when (sz > fileSize) $ receiveFile' user ft Nothing Nothing >>= toView

    messageFileDescription :: Contact -> SharedMsgId -> FileDescr -> MsgMeta -> m ()
    messageFileDescription ct@Contact {contactId} sharedMsgId fileDescr msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      fileId <- withStore $ \db -> getFileIdBySharedMsgId db userId contactId sharedMsgId
      processFDMessage fileId fileDescr

    groupMessageFileDescription :: GroupInfo -> GroupMember -> SharedMsgId -> FileDescr -> m ()
    groupMessageFileDescription GroupInfo {groupId} _m sharedMsgId fileDescr = do
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      processFDMessage fileId fileDescr

    processFDMessage :: FileTransferId -> FileDescr -> m ()
    processFDMessage fileId fileDescr = do
      ft <- withStore $ \db -> getRcvFileTransfer db user fileId
      unless (rcvFileCompleteOrCancelled ft) $ do
        (rfd, RcvFileTransfer {fileStatus, xftpRcvFile, cryptoArgs}) <- withStore $ \db -> do
          rfd <- appendRcvFD db userId fileId fileDescr
          -- reading second time in the same transaction as appending description
          -- to prevent race condition with accept
          ft' <- getRcvFileTransfer db user fileId
          pure (rfd, ft')
        case (fileStatus, xftpRcvFile) of
          (RFSAccepted _, Just XFTPRcvFile {}) -> receiveViaCompleteFD user fileId rfd cryptoArgs
          _ -> pure ()

    processFileInvitation :: Maybe FileInvitation -> MsgContent -> (DB.Connection -> FileInvitation -> Maybe InlineFileMode -> Integer -> ExceptT StoreError IO RcvFileTransfer) -> m (Maybe (RcvFileTransfer, CIFile 'MDRcv))
    processFileInvitation fInv_ mc createRcvFT = forM fInv_ $ \fInv@FileInvitation {fileName, fileSize} -> do
      ChatConfig {fileChunkSize} <- asks config
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

    messageUpdate :: Contact -> SharedMsgId -> MsgContent -> RcvMessage -> MsgMeta -> Maybe Int -> Maybe Bool -> m ()
    messageUpdate ct@Contact {contactId} sharedMsgId mc msg@RcvMessage {msgId} msgMeta ttl live_ = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      updateRcvChatItem `catchCINotFound` \_ -> do
        -- This patches initial sharedMsgId into chat item when locally deleted chat item
        -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
        -- Chat item and update message which created it will have different sharedMsgId in this case...
        let timed_ = rcvContactCITimed ct ttl
        ci <- saveRcvChatItem' user (CDDirectRcv ct) msg (Just sharedMsgId) brokerTs content Nothing timed_ live
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          updateDirectChatItem' db user contactId ci content live Nothing
        toView $ CRChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci')
      where
        brokerTs = metaBrokerTs msgMeta
        content = CIRcvMsgContent mc
        live = fromMaybe False live_
        updateRcvChatItem = do
          cci <- withStore $ \db -> getDirectChatItemBySharedMsgId db user contactId sharedMsgId
          case cci of
            CChatItem SMDRcv ci@ChatItem {meta = CIMeta {itemLive}, content = CIRcvMsgContent oldMC} -> do
              let changed = mc /= oldMC
              if changed || fromMaybe False itemLive
                then do
                  ci' <- withStore' $ \db -> do
                    when changed $
                      addInitialAndNewCIVersions db (chatItemId' ci) (chatItemTs' ci, oldMC) (brokerTs, mc)
                    reactions <- getDirectCIReactions db ct sharedMsgId
                    updateDirectChatItem' db user contactId ci {reactions} content live $ Just msgId
                  toView $ CRChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci')
                  startUpdatedTimedItemThread user (ChatRef CTDirect contactId) ci ci'
                else toView $ CRChatItemNotChanged user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
            _ -> messageError "x.msg.update: contact attempted invalid message update"

    messageDelete :: Contact -> SharedMsgId -> RcvMessage -> MsgMeta -> m ()
    messageDelete ct@Contact {contactId} sharedMsgId RcvMessage {msgId} msgMeta@MsgMeta {broker = (_, brokerTs)} = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      deleteRcvChatItem `catchCINotFound` (toView . CRChatItemDeletedNotFound user ct)
      where
        deleteRcvChatItem = do
          CChatItem msgDir ci <- withStore $ \db -> getDirectChatItemBySharedMsgId db user contactId sharedMsgId
          case msgDir of
            SMDRcv ->
              if featureAllowed SCFFullDelete forContact ct
                then deleteDirectCI user ct ci False False >>= toView
                else markDirectCIDeleted user ct ci msgId False brokerTs >>= toView
            SMDSnd -> messageError "x.msg.del: contact attempted invalid message delete"

    directMsgReaction :: Contact -> SharedMsgId -> MsgReaction -> Bool -> RcvMessage -> MsgMeta -> m ()
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

    groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> MemberId -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> m ()
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

    catchCINotFound :: m a -> (SharedMsgId -> m a) -> m a
    catchCINotFound f handle =
      f `catchChatError` \case
        ChatErrorStore (SEChatItemSharedMsgIdNotFound sharedMsgId) -> handle sharedMsgId
        e -> throwError e

    newGroupContentMessage :: GroupInfo -> GroupMember -> MsgContainer -> RcvMessage -> UTCTime -> Bool -> m ()
    newGroupContentMessage gInfo m@GroupMember {memberId, memberRole} mc msg@RcvMessage {sharedMsgId_} brokerTs forwarded
      | isVoice content && not (groupFeatureAllowed SGFVoice gInfo) = rejected GFVoice
      | not (isVoice content) && isJust fInv_ && not (groupFeatureAllowed SGFFiles gInfo) = rejected GFFiles
      | otherwise = do
          let timed_ =
                if forwarded
                  then rcvCITimed_ (Just Nothing) itemTTL
                  else rcvGroupCITimed gInfo itemTTL
              live = fromMaybe False live_
          withStore' (\db -> getCIModeration db user gInfo memberId sharedMsgId_) >>= \case
            Just ciModeration -> do
              applyModeration timed_ live ciModeration
              withStore' $ \db -> deleteCIModeration db gInfo memberId sharedMsgId_
            Nothing -> createItem timed_ live
      where
        rejected f = void $ newChatItem (CIRcvGroupFeatureRejected f) Nothing Nothing False
        ExtMsgContent content fInv_ itemTTL live_ = mcExtMsgContent mc
        applyModeration timed_ live CIModeration {moderatorMember = moderator@GroupMember {memberRole = moderatorRole}, createdByMsgId, moderatedAt}
          | moderatorRole < GRAdmin || moderatorRole < memberRole =
              createItem timed_ live
          | groupFeatureAllowed SGFFullDelete gInfo = do
              ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs CIRcvModerated Nothing timed_ False
              ci' <- withStore' $ \db -> updateGroupChatItemModerated db user gInfo ci moderator moderatedAt
              toView $ CRNewChatItem user $ AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci'
          | otherwise = do
              file_ <- processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId m
              ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs (CIRcvMsgContent content) (snd <$> file_) timed_ False
              toView =<< markGroupCIDeleted user gInfo ci createdByMsgId False (Just moderator) moderatedAt
        createItem timed_ live = do
          file_ <- processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId m
          newChatItem (CIRcvMsgContent content) (snd <$> file_) timed_ live
          when (showMessages $ memberSettings m) $ autoAcceptFile file_
        newChatItem ciContent ciFile_ timed_ live = do
          ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ brokerTs ciContent ciFile_ timed_ live
          ci' <- blockedMember m ci $ withStore' $ \db -> markGroupChatItemBlocked db user gInfo ci
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getGroupCIReactions db gInfo memberId sharedMsgId) sharedMsgId_
          groupMsgToView gInfo ci' {reactions}

    groupMessageUpdate :: GroupInfo -> GroupMember -> SharedMsgId -> MsgContent -> RcvMessage -> UTCTime -> Maybe Int -> Maybe Bool -> m ()
    groupMessageUpdate gInfo@GroupInfo {groupId} m@GroupMember {groupMemberId, memberId} sharedMsgId mc msg@RcvMessage {msgId} brokerTs ttl_ live_ =
      updateRcvChatItem `catchCINotFound` \_ -> do
        -- This patches initial sharedMsgId into chat item when locally deleted chat item
        -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
        -- Chat item and update message which created it will have different sharedMsgId in this case...
        let timed_ = rcvGroupCITimed gInfo ttl_
        ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg (Just sharedMsgId) brokerTs content Nothing timed_ live
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          ci' <- updateGroupChatItem db user groupId ci content live Nothing
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
                        updateGroupChatItem db user groupId ci {reactions} content live $ Just msgId
                      toView $ CRChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci')
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId) ci ci'
                    else toView $ CRChatItemNotChanged user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)
                else messageError "x.msg.update: group member attempted to update a message of another member"
            _ -> messageError "x.msg.update: group member attempted invalid message update"

    groupMessageDelete :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> RcvMessage -> UTCTime -> m ()
    groupMessageDelete gInfo@GroupInfo {groupId, membership} m@GroupMember {memberId, memberRole = senderRole} sharedMsgId sndMemberId_ RcvMessage {msgId} brokerTs = do
      let msgMemberId = fromMaybe memberId sndMemberId_
      withStore' (\db -> runExceptT $ getGroupMemberCIBySharedMsgId db user groupId msgMemberId sharedMsgId) >>= \case
        Right (CChatItem _ ci@ChatItem {chatDir}) -> case chatDir of
          CIGroupRcv mem
            | sameMemberId memberId mem && msgMemberId == memberId -> delete ci Nothing >>= toView
            | otherwise -> deleteMsg mem ci
          CIGroupSnd -> deleteMsg membership ci
        Left e
          | msgMemberId == memberId -> messageError $ "x.msg.del: message not found, " <> tshow e
          | senderRole < GRAdmin -> messageError $ "x.msg.del: message not found, message of another member with insufficient member permissions, " <> tshow e
          | otherwise -> withStore' $ \db -> createCIModeration db gInfo m msgMemberId sharedMsgId msgId brokerTs
      where
        deleteMsg :: MsgDirectionI d => GroupMember -> ChatItem 'CTGroup d -> m ()
        deleteMsg mem ci = case sndMemberId_ of
          Just sndMemberId
            | sameMemberId sndMemberId mem -> checkRole mem $ delete ci (Just m) >>= toView
            | otherwise -> messageError "x.msg.del: message of another member with incorrect memberId"
          _ -> messageError "x.msg.del: message of another member without memberId"
        checkRole GroupMember {memberRole} a
          | senderRole < GRAdmin || senderRole < memberRole =
              messageError "x.msg.del: message of another member with insufficient member permissions"
          | otherwise = a
        delete :: MsgDirectionI d => ChatItem 'CTGroup d -> Maybe GroupMember -> m ChatResponse
        delete ci byGroupMember
          | groupFeatureAllowed SGFFullDelete gInfo = deleteGroupCI user gInfo ci False False byGroupMember brokerTs
          | otherwise = markGroupCIDeleted user gInfo ci msgId False byGroupMember brokerTs

    -- TODO remove once XFile is discontinued
    processFileInvitation' :: Contact -> FileInvitation -> RcvMessage -> MsgMeta -> m ()
    processFileInvitation' ct fInv@FileInvitation {fileName, fileSize} msg@RcvMessage {sharedMsgId_} msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      ChatConfig {fileChunkSize} <- asks config
      inline <- receiveInlineMode fInv Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFileTransfer db userId ct fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
      ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs (CIRcvMsgContent $ MCFile "") ciFile Nothing False
      toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
      where
        brokerTs = metaBrokerTs msgMeta

    -- TODO remove once XFile is discontinued
    processGroupFileInvitation' :: GroupInfo -> GroupMember -> FileInvitation -> RcvMessage -> UTCTime -> m ()
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

    receiveInlineMode :: FileInvitation -> Maybe MsgContent -> Integer -> m (Maybe InlineFileMode)
    receiveInlineMode FileInvitation {fileSize, fileInline, fileDescr} mc_ chSize = case (fileInline, fileDescr) of
      (Just mode, Nothing) -> do
        InlineFilesConfig {receiveChunks, receiveInstant} <- asks $ inlineFiles . config
        pure $ if fileSize <= receiveChunks * chSize then inline' receiveInstant else Nothing
        where
          inline' receiveInstant = if mode == IFMOffer || (receiveInstant && maybe False isVoice mc_) then fileInline else Nothing
      _ -> pure Nothing

    xFileCancel :: Contact -> SharedMsgId -> MsgMeta -> m ()
    xFileCancel ct@Contact {contactId} sharedMsgId msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      fileId <- withStore $ \db -> getFileIdBySharedMsgId db userId contactId sharedMsgId
      ft <- withStore (\db -> getRcvFileTransfer db user fileId)
      unless (rcvFileCompleteOrCancelled ft) $ do
        cancelRcvFileTransfer user ft >>= mapM_ (deleteAgentConnectionAsync user)
        ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
        toView $ CRRcvFileSndCancelled user ci ft

    xFileAcptInv :: Contact -> SharedMsgId -> Maybe ConnReqInvitation -> String -> MsgMeta -> m ()
    xFileAcptInv ct sharedMsgId fileConnReq_ fName msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
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
            dm <- directMessage XOk
            connIds <- joinAgentConnectionAsync user True fileConnReq dm subMode
            withStore' $ \db -> createSndDirectFTConnection db user fileId connIds subMode
          -- receiving inline
          _ -> do
            event <- withStore $ \db -> do
              ci' <- updateDirectCIFileStatus db vr user fileId $ CIFSSndTransfer 0 1
              sft <- createSndDirectInlineFT db ct ft
              pure $ CRSndFileStart user ci' sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendDirectFileInline ct ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
        else messageError "x.file.acpt.inv: fileName is different from expected"

    assertSMPAcceptNotProhibited :: ChatItem c d -> m ()
    assertSMPAcceptNotProhibited ChatItem {file = Just CIFile {fileId, fileProtocol}, content}
      | fileProtocol == FPXFTP && not (imageOrVoice content) = throwChatError $ CEFallbackToSMPProhibited fileId
      | otherwise = pure ()
      where
        imageOrVoice :: CIContent d -> Bool
        imageOrVoice (CISndMsgContent (MCImage _ _)) = True
        imageOrVoice (CISndMsgContent (MCVoice _ _)) = True
        imageOrVoice _ = False
    assertSMPAcceptNotProhibited _ = pure ()

    checkSndInlineFTComplete :: Connection -> AgentMsgId -> m ()
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

    allowSendInline :: Integer -> Maybe InlineFileMode -> m Bool
    allowSendInline fileSize = \case
      Just IFMOffer -> do
        ChatConfig {fileChunkSize, inlineFiles} <- asks config
        pure $ fileSize <= fileChunkSize * offerChunks inlineFiles
      _ -> pure False

    bFileChunk :: Contact -> SharedMsgId -> FileChunk -> MsgMeta -> m ()
    bFileChunk ct sharedMsgId chunk meta = do
      ft <- withStore $ \db -> getDirectFileIdBySharedMsgId db user ct sharedMsgId >>= getRcvFileTransfer db user
      receiveInlineChunk ft chunk meta

    bFileChunkGroup :: GroupInfo -> SharedMsgId -> FileChunk -> MsgMeta -> m ()
    bFileChunkGroup GroupInfo {groupId} sharedMsgId chunk meta = do
      ft <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId >>= getRcvFileTransfer db user
      receiveInlineChunk ft chunk meta

    receiveInlineChunk :: RcvFileTransfer -> FileChunk -> MsgMeta -> m ()
    receiveInlineChunk RcvFileTransfer {fileId, fileStatus = RFSNew} FileChunk {chunkNo} _
      | chunkNo == 1 = throwChatError $ CEInlineFileProhibited fileId
      | otherwise = pure ()
    receiveInlineChunk ft@RcvFileTransfer {fileId} chunk meta = do
      case chunk of
        FileChunk {chunkNo} -> when (chunkNo == 1) $ startReceivingFile user fileId
        _ -> pure ()
      receiveFileChunk ft Nothing meta chunk

    xFileCancelGroup :: GroupInfo -> GroupMember -> SharedMsgId -> m ()
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

    xFileAcptInvGroup :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe ConnReqInvitation -> String -> m ()
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
            dm <- directMessage XOk
            connIds <- joinAgentConnectionAsync user True fileConnReq dm subMode
            withStore' $ \db -> createSndGroupFileTransferConnection db user fileId connIds m subMode
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

    groupMsgToView :: GroupInfo -> ChatItem 'CTGroup 'MDRcv -> m ()
    groupMsgToView gInfo ci =
      toView $ CRNewChatItem user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)

    processGroupInvitation :: Contact -> GroupInvitation -> RcvMessage -> MsgMeta -> m ()
    processGroupInvitation ct inv msg msgMeta = do
      let Contact {localDisplayName = c, activeConn} = ct
          GroupInvitation {fromMember = (MemberIdRole fromMemId fromRole), invitedMember = (MemberIdRole memId memRole), connRequest, groupLinkId} = inv
      forM_ activeConn $ \Connection {connId, peerChatVRange, customUserProfileId, groupLinkId = groupLinkId'} -> do
        checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
        when (fromRole < GRAdmin || fromRole < memRole) $ throwChatError (CEGroupContactRole c)
        when (fromMemId == memId) $ throwChatError CEGroupDuplicateMemberId
        -- [incognito] if direct connection with host is incognito, create membership using the same incognito profile
        (gInfo@GroupInfo {groupId, localDisplayName, groupProfile, membership = membership@GroupMember {groupMemberId, memberId}}, hostId) <-
          withStore $ \db -> createGroupInvitation db vr user ct inv customUserProfileId
        if sameGroupLinkId groupLinkId groupLinkId'
          then do
            subMode <- chatReadVar subscriptionMode
            dm <- directMessage $ XGrpAcpt memberId
            connIds <- joinAgentConnectionAsync user True connRequest dm subMode
            withStore' $ \db -> do
              setViaGroupLinkHash db groupId connId
              createMemberConnectionAsync db user hostId connIds (fromJVersionRange peerChatVRange) subMode
              updateGroupMemberStatusById db userId hostId GSMemAccepted
              updateGroupMemberStatus db userId membership GSMemAccepted
            toView $ CRUserAcceptedGroupSent user gInfo {membership = membership {memberStatus = GSMemAccepted}} (Just ct)
          else do
            let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
            ci <- saveRcvChatItem user (CDDirectRcv ct) msg brokerTs content
            withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
            toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
            toView $ CRReceivedGroupInvitation {user, groupInfo = gInfo, contact = ct, fromMemberRole = fromRole, memberRole = memRole}
      where
        brokerTs = metaBrokerTs msgMeta
        sameGroupLinkId :: Maybe GroupLinkId -> Maybe GroupLinkId -> Bool
        sameGroupLinkId (Just gli) (Just gli') = gli == gli'
        sameGroupLinkId _ _ = False

    checkIntegrityCreateItem :: forall c. ChatTypeI c => ChatDirection c 'MDRcv -> MsgMeta -> m ()
    checkIntegrityCreateItem cd MsgMeta {integrity, broker = (_, brokerTs)} = case integrity of
      MsgOk -> pure ()
      MsgError e -> createInternalChatItem user cd (CIRcvIntegrityError e) (Just brokerTs)

    xInfo :: Contact -> Profile -> m ()
    xInfo c p' = void $ processContactProfileUpdate c p' True

    xDirectDel :: Contact -> RcvMessage -> MsgMeta -> m ()
    xDirectDel c msg msgMeta =
      if directOrUsed c
        then do
          checkIntegrityCreateItem (CDDirectRcv c) msgMeta
          ct' <- withStore' $ \db -> updateContactStatus db user c CSDeleted
          contactConns <- withStore' $ \db -> getContactConnections db userId ct'
          deleteAgentConnectionsAsync user $ map aConnId contactConns
          forM_ contactConns $ \conn -> withStore' $ \db -> updateConnectionStatus db conn ConnDeleted
          activeConn' <- forM (contactConn ct') $ \conn -> pure conn {connStatus = ConnDeleted}
          let ct'' = ct' {activeConn = activeConn'} :: Contact
          ci <- saveRcvChatItem user (CDDirectRcv ct'') msg brokerTs (CIRcvDirectEvent RDEContactDeleted)
          toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct'') ci)
          toView $ CRContactDeletedByContact user ct''
        else do
          contactConns <- withStore' $ \db -> getContactConnections db userId c
          deleteAgentConnectionsAsync user $ map aConnId contactConns
          withStore' $ \db -> deleteContact db user c
      where
        brokerTs = metaBrokerTs msgMeta

    processContactProfileUpdate :: Contact -> Profile -> Bool -> m Contact
    processContactProfileUpdate c@Contact {profile = p} p' createItems
      | fromLocalProfile p /= p' = do
          c' <- withStore $ \db ->
            if userTTL == rcvTTL
              then updateContactProfile db user c p'
              else do
                c' <- liftIO $ updateContactUserPreferences db user c ctUserPrefs'
                updateContactProfile db user c' p'
          when (directOrUsed c' && createItems) $ createRcvFeatureItems user c c'
          toView $ CRContactUpdated user c c'
          pure c'
      | otherwise =
          pure c
      where
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

    xInfoMember :: GroupInfo -> GroupMember -> Profile -> m ()
    xInfoMember gInfo m p' = void $ processMemberProfileUpdate gInfo m p'

    xGrpLinkMem :: GroupInfo -> GroupMember -> Connection -> Profile -> m ()
    xGrpLinkMem gInfo@GroupInfo {membership} m@GroupMember {groupMemberId, memberCategory} Connection {viaGroupLink} p' = do
      xGrpLinkMemReceived <- withStore $ \db -> getXGrpLinkMemReceived db groupMemberId
      if viaGroupLink && isNothing (memberContactId m) && memberCategory == GCHostMember && not xGrpLinkMemReceived
        then do
          m' <- processMemberProfileUpdate gInfo m p'
          withStore' $ \db -> setXGrpLinkMemReceived db groupMemberId True
          let connectedIncognito = memberIncognito membership
          probeMatchingMemberContact m' connectedIncognito
        else messageError "x.grp.link.mem error: invalid group link host profile update"

    processMemberProfileUpdate :: GroupInfo -> GroupMember -> Profile -> m GroupMember
    processMemberProfileUpdate gInfo m@GroupMember {memberContactId} p' =
      case memberContactId of
        Nothing -> do
          m' <- withStore $ \db -> updateMemberProfile db user m p'
          toView $ CRGroupMemberUpdated user gInfo m m'
          pure m'
        Just mContactId -> do
          mCt <- withStore $ \db -> getContact db user mContactId
          Contact {profile} <- processContactProfileUpdate mCt p' True
          pure m {memberProfile = profile}

    createFeatureEnabledItems :: Contact -> m ()
    createFeatureEnabledItems ct@Contact {mergedPreferences} =
      forM_ allChatFeatures $ \(ACF f) -> do
        let state = featureState $ getContactUserPreference f mergedPreferences
        createInternalChatItem user (CDDirectRcv ct) (uncurry (CIRcvChatFeature $ chatFeature f) state) Nothing

    createGroupFeatureItems :: GroupInfo -> GroupMember -> m ()
    createGroupFeatureItems g@GroupInfo {fullGroupPreferences} m =
      forM_ allGroupFeatureItems $ \(AGF f) -> do
        let p = getGroupPreference f fullGroupPreferences
            (_, param) = groupFeatureState p
        createInternalChatItem user (CDGroupRcv g m) (CIRcvGroupFeature (toGroupFeature f) (toGroupPreference p) param) Nothing

    xInfoProbe :: ContactOrMember -> Probe -> m ()
    xInfoProbe cgm2 probe = do
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      -- [incognito] unless connected incognito
      when (contactMerge && not (contactOrMemberIncognito cgm2)) $ do
        cgm1s <- withStore' $ \db -> matchReceivedProbe db user cgm2 probe
        let cgm1s' = filter (not . contactOrMemberIncognito) cgm1s
        probeMatches cgm1s' cgm2
      where
        probeMatches :: [ContactOrMember] -> ContactOrMember -> m ()
        probeMatches [] _ = pure ()
        probeMatches (cgm1' : cgm1s') cgm2' = do
          cgm2''_ <- probeMatch cgm1' cgm2' probe `catchChatError` \_ -> pure (Just cgm2')
          let cgm2'' = fromMaybe cgm2' cgm2''_
          probeMatches cgm1s' cgm2''

    xInfoProbeCheck :: ContactOrMember -> ProbeHash -> m ()
    xInfoProbeCheck cgm1 probeHash = do
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      -- [incognito] unless connected incognito
      when (contactMerge && not (contactOrMemberIncognito cgm1)) $ do
        cgm2Probe_ <- withStore' $ \db -> matchReceivedProbeHash db user cgm1 probeHash
        forM_ cgm2Probe_ $ \(cgm2, probe) ->
          unless (contactOrMemberIncognito cgm2) . void $
            probeMatch cgm1 cgm2 probe

    probeMatch :: ContactOrMember -> ContactOrMember -> Probe -> m (Maybe ContactOrMember)
    probeMatch cgm1 cgm2 probe =
      case cgm1 of
        COMContact c1@Contact {contactId = cId1, profile = p1} ->
          case cgm2 of
            COMContact c2@Contact {contactId = cId2, profile = p2}
              | cId1 /= cId2 && profilesMatch p1 p2 -> do
                  void . sendDirectContactMessage c1 $ XInfoProbeOk probe
                  COMContact <$$> mergeContacts c1 c2
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or same contact id" >> pure Nothing
            COMGroupMember m2@GroupMember {memberProfile = p2, memberContactId}
              | isNothing memberContactId && profilesMatch p1 p2 -> do
                  void . sendDirectContactMessage c1 $ XInfoProbeOk probe
                  COMContact <$$> associateMemberAndContact c1 m2
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or member already has contact" >> pure Nothing
        COMGroupMember GroupMember {activeConn = Nothing} -> pure Nothing
        COMGroupMember m1@GroupMember {groupId, memberProfile = p1, memberContactId, activeConn = Just conn} ->
          case cgm2 of
            COMContact c2@Contact {profile = p2}
              | memberCurrent m1 && isNothing memberContactId && profilesMatch p1 p2 -> do
                  void $ sendDirectMessage conn (XInfoProbeOk probe) (GroupId groupId)
                  COMContact <$$> associateMemberAndContact c2 m1
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or member already has contact or member not current" >> pure Nothing
            COMGroupMember _ -> messageWarning "probeMatch ignored: members are not matched with members" >> pure Nothing

    xInfoProbeOk :: ContactOrMember -> Probe -> m ()
    xInfoProbeOk cgm1 probe = do
      cgm2 <- withStore' $ \db -> matchSentProbe db user cgm1 probe
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
    xCallInv :: Contact -> CallId -> CallInvitation -> RcvMessage -> MsgMeta -> m ()
    xCallInv ct@Contact {contactId} callId CallInvitation {callType, callDhPubKey} msg@RcvMessage {sharedMsgId_} msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      if featureAllowed SCFCalls forContact ct
        then do
          g <- asks random
          dhKeyPair <- atomically $ if encryptedCall callType then Just <$> C.generateKeyPair g else pure Nothing
          ci <- saveCallItem CISCallPending
          let sharedKey = C.Key . C.dhBytes' <$> (C.dh' <$> callDhPubKey <*> (snd <$> dhKeyPair))
              callState = CallInvitationReceived {peerCallType = callType, localDhPubKey = fst <$> dhKeyPair, sharedKey}
              call' = Call {contactId, callId, chatItemId = chatItemId' ci, callState, callTs = chatItemTs' ci}
          calls <- asks currentCalls
          -- theoretically, the new call invitation for the current contact can mark the in-progress call as ended
          -- (and replace it in ChatController)
          -- practically, this should not happen
          withStore' $ \db -> createCall db user call' $ chatItemTs' ci
          call_ <- atomically (TM.lookupInsert contactId call' calls)
          forM_ call_ $ \call -> updateCallItemStatus user ct call WCSDisconnected Nothing
          toView $ CRCallInvitation RcvCallInvitation {user, contact = ct, callType, sharedKey, callTs = chatItemTs' ci}
          toView $ CRNewChatItem user $ AChatItem SCTDirect SMDRcv (DirectChat ct) ci
        else featureRejected CFCalls
      where
        brokerTs = metaBrokerTs msgMeta
        saveCallItem status = saveRcvChatItem user (CDDirectRcv ct) msg brokerTs (CIRcvCall status 0)
        featureRejected f = do
          ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs (CIRcvChatFeatureRejected f) Nothing Nothing False
          toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)

    -- to party initiating call
    xCallOffer :: Contact -> CallId -> CallOffer -> RcvMessage -> MsgMeta -> m ()
    xCallOffer ct callId CallOffer {callType, rtcSession, callDhPubKey} msg msgMeta = do
      msgCurrentCall ct callId "x.call.offer" msg msgMeta $
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
    xCallAnswer :: Contact -> CallId -> CallAnswer -> RcvMessage -> MsgMeta -> m ()
    xCallAnswer ct callId CallAnswer {rtcSession} msg msgMeta = do
      msgCurrentCall ct callId "x.call.answer" msg msgMeta $
        \call -> case callState call of
          CallOfferSent {localCallType, peerCallType, localCallSession, sharedKey} -> do
            let callState' = CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession = rtcSession, sharedKey}
            toView $ CRCallAnswer user ct rtcSession
            pure (Just call {callState = callState'}, Just . ACIContent SMDRcv $ CIRcvCall CISCallNegotiated 0)
          _ -> do
            msgCallStateError "x.call.answer" call
            pure (Just call, Nothing)

    -- to any call party
    xCallExtra :: Contact -> CallId -> CallExtraInfo -> RcvMessage -> MsgMeta -> m ()
    xCallExtra ct callId CallExtraInfo {rtcExtraInfo} msg msgMeta = do
      msgCurrentCall ct callId "x.call.extra" msg msgMeta $
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
    xCallEnd :: Contact -> CallId -> RcvMessage -> MsgMeta -> m ()
    xCallEnd ct callId msg msgMeta =
      msgCurrentCall ct callId "x.call.end" msg msgMeta $ \Call {chatItemId} -> do
        toView $ CRCallEnded user ct
        (Nothing,) <$> callStatusItemContent user ct chatItemId WCSDisconnected

    msgCurrentCall :: Contact -> CallId -> Text -> RcvMessage -> MsgMeta -> (Call -> m (Maybe Call, Maybe ACIContent)) -> m ()
    msgCurrentCall ct@Contact {contactId = ctId'} callId' eventName RcvMessage {msgId} msgMeta action = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
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
              forM_ aciContent_ $ \aciContent ->
                updateDirectChatItemView user ct chatItemId aciContent False $ Just msgId

    msgCallStateError :: Text -> Call -> m ()
    msgCallStateError eventName Call {callState} =
      messageError $ eventName <> ": wrong call state " <> T.pack (show $ callStateTag callState)

    mergeContacts :: Contact -> Contact -> m (Maybe Contact)
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
          c2'' <- withStore $ \db -> mergeContactRecords db user c1' c2'
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

    associateMemberAndContact :: Contact -> GroupMember -> m (Maybe Contact)
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

    associateMemberWithContact :: Contact -> GroupMember -> m Contact
    associateMemberWithContact c1 m2@GroupMember {groupId} = do
      withStore' $ \db -> associateMemberWithContactRecord db user c1 m2
      g <- withStore $ \db -> getGroupInfo db vr user groupId
      toView $ CRContactAndMemberAssociated user c1 g m2 c1
      pure c1

    associateContactWithMember :: GroupMember -> Contact -> m Contact
    associateContactWithMember m1@GroupMember {groupId} c2 = do
      c2' <- withStore $ \db -> associateContactWithMemberRecord db user m1 c2
      g <- withStore $ \db -> getGroupInfo db vr user groupId
      toView $ CRContactAndMemberAssociated user c2 g m1 c2'
      pure c2'

    saveConnInfo :: Connection -> ConnInfo -> m Connection
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage activeConn connInfo
      conn' <- updatePeerChatVRange activeConn chatVRange
      case chatMsgEvent of
        XInfo p -> do
          let contactUsed = connDirect activeConn
          ct <- withStore $ \db -> createDirectContact db user conn' p contactUsed
          toView $ CRContactConnecting user ct
          pure conn'
        XGrpLinkInv glInv -> do
          (gInfo, host) <- withStore $ \db -> createGroupInvitedViaLink db vr user conn' glInv
          toView $ CRGroupLinkConnecting user gInfo host
          pure conn'
        -- TODO show/log error, other events in SMP confirmation
        _ -> pure conn'

    xGrpMemNew :: GroupInfo -> GroupMember -> MemberInfo -> RcvMessage -> UTCTime -> m ()
    xGrpMemNew gInfo m memInfo@(MemberInfo memId memRole _ memberProfile) msg brokerTs = do
      checkHostRole m memRole
      unless (sameMemberId memId $ membership gInfo) $
        withStore' (\db -> runExceptT $ getGroupMemberByMemberId db user gInfo memId) >>= \case
          Right _ -> messageError "x.grp.mem.new error: member already exists"
          Left _ -> do
            newMember@GroupMember {groupMemberId} <- withStore $ \db -> createNewGroupMember db user gInfo m memInfo GCPostMember GSMemAnnounced
            ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent $ RGEMemberAdded groupMemberId memberProfile)
            groupMsgToView gInfo ci
            toView $ CRJoinedGroupMemberConnecting user gInfo m newMember

    xGrpMemIntro :: GroupInfo -> GroupMember -> MemberInfo -> m ()
    xGrpMemIntro gInfo@GroupInfo {chatSettings} m@GroupMember {memberRole, localDisplayName = c} memInfo@(MemberInfo memId _ memChatVRange _) = do
      case memberCategory m of
        GCHostMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db user gInfo memId) >>= \case
            Right _ -> messageError "x.grp.mem.intro ignored: member already exists"
            Left _ -> do
              when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole c)
              subMode <- chatReadVar subscriptionMode
              -- [async agent commands] commands should be asynchronous, continuation is to send XGrpMemInv - have to remember one has completed and process on second
              groupConnIds <- createConn subMode
              directConnIds <- case memChatVRange of
                Nothing -> Just <$> createConn subMode
                Just mcvr
                  | isCompatibleRange (fromChatVRange mcvr) groupNoDirectVRange -> pure Nothing
                  | otherwise -> Just <$> createConn subMode
              let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
              void $ withStore $ \db -> createIntroReMember db user gInfo m memInfo groupConnIds directConnIds customUserProfileId subMode
        _ -> messageError "x.grp.mem.intro can be only sent by host member"
      where
        createConn subMode = createAgentConnectionAsync user CFCreateConnGrpMemInv (chatHasNtfs chatSettings) SCMInvitation subMode

    sendXGrpMemInv :: Int64 -> Maybe ConnReqInvitation -> XGrpMemIntroCont -> m ()
    sendXGrpMemInv hostConnId directConnReq XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq} = do
      hostConn <- withStore $ \db -> getConnectionById db user hostConnId
      let msg = XGrpMemInv memberId IntroInvitation {groupConnReq, directConnReq}
      void $ sendDirectMessage hostConn msg (GroupId groupId)
      withStore' $ \db -> updateGroupMemberStatusById db userId groupMemberId GSMemIntroInvited

    xGrpMemInv :: GroupInfo -> GroupMember -> MemberId -> IntroInvitation -> m ()
    xGrpMemInv gInfo@GroupInfo {groupId} m memId introInv = do
      case memberCategory m of
        GCInviteeMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db user gInfo memId) >>= \case
            Left _ -> messageError "x.grp.mem.inv error: referenced member does not exist"
            Right reMember -> do
              GroupMemberIntro {introId} <- withStore $ \db -> saveIntroInvitation db reMember m introInv
              sendGroupMemberMessage user reMember (XGrpMemFwd (memberInfo m) introInv) groupId (Just introId) $
                withStore' $
                  \db -> updateIntroStatus db introId GMIntroInvForwarded
        _ -> messageError "x.grp.mem.inv can be only sent by invitee member"

    xGrpMemFwd :: GroupInfo -> GroupMember -> MemberInfo -> IntroInvitation -> m ()
    xGrpMemFwd gInfo@GroupInfo {membership, chatSettings} m memInfo@(MemberInfo memId memRole memChatVRange _) introInv@IntroInvitation {groupConnReq, directConnReq} = do
      checkHostRole m memRole
      toMember <-
        withStore' (\db -> runExceptT $ getGroupMemberByMemberId db user gInfo memId) >>= \case
          -- TODO if the missed messages are correctly sent as soon as there is connection before anything else is sent
          -- the situation when member does not exist is an error
          -- member receiving x.grp.mem.fwd should have also received x.grp.mem.new prior to that.
          -- For now, this branch compensates for the lack of delayed message delivery.
          Left _ -> withStore $ \db -> createNewGroupMember db user gInfo m memInfo GCPostMember GSMemAnnounced
          Right m' -> pure m'
      withStore' $ \db -> saveMemberInvitation db toMember introInv
      subMode <- chatReadVar subscriptionMode
      -- [incognito] send membership incognito profile, create direct connection as incognito
      dm <- directMessage $ XGrpMemInfo membership.memberId (fromLocalProfile $ memberProfile membership)
      -- [async agent commands] no continuation needed, but commands should be asynchronous for stability
      groupConnIds <- joinAgentConnectionAsync user (chatHasNtfs chatSettings) groupConnReq dm subMode
      directConnIds <- forM directConnReq $ \dcr -> joinAgentConnectionAsync user True dcr dm subMode
      let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
          mcvr = maybe chatInitialVRange fromChatVRange memChatVRange
      withStore' $ \db -> createIntroToMemberContact db user m toMember mcvr groupConnIds directConnIds customUserProfileId subMode

    xGrpMemRole :: GroupInfo -> GroupMember -> MemberId -> GroupMemberRole -> RcvMessage -> UTCTime -> m ()
    xGrpMemRole gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId memRole msg brokerTs
      | membership.memberId == memId =
          let gInfo' = gInfo {membership = membership {memberRole = memRole}}
           in changeMemberRole gInfo' membership $ RGEUserRole memRole
      | otherwise =
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db user gInfo memId) >>= \case
            Right member -> changeMemberRole gInfo member $ RGEMemberRole (groupMemberId' member) (fromLocalProfile $ memberProfile member) memRole
            Left _ -> messageError "x.grp.mem.role with unknown member ID"
      where
        changeMemberRole gInfo' member@GroupMember {memberRole = fromRole} gEvent
          | senderRole < GRAdmin || senderRole < fromRole = messageError "x.grp.mem.role with insufficient member permissions"
          | otherwise = do
              withStore' $ \db -> updateGroupMemberRole db user member memRole
              ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent gEvent)
              groupMsgToView gInfo ci
              toView CRMemberRole {user, groupInfo = gInfo', byMember = m, member = member {memberRole = memRole}, fromRole, toRole = memRole}

    checkHostRole :: GroupMember -> GroupMemberRole -> m ()
    checkHostRole GroupMember {memberRole, localDisplayName} memRole =
      when (memberRole < GRAdmin || memberRole < memRole) $ throwChatError (CEGroupContactRole localDisplayName)

    xGrpMemCon :: GroupInfo -> GroupMember -> MemberId -> m ()
    xGrpMemCon gInfo sendingMember memId = do
      refMember <- withStore $ \db -> getGroupMemberByMemberId db user gInfo memId
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
        inviteeXGrpMemCon :: GroupMemberIntro -> m ()
        inviteeXGrpMemCon GroupMemberIntro {introId, introStatus}
          | introStatus == GMIntroReConnected = updateStatus introId GMIntroConnected
          | introStatus `elem` [GMIntroToConnected, GMIntroConnected] = pure ()
          | otherwise = updateStatus introId GMIntroToConnected
        forwardMemberXGrpMemCon :: GroupMemberIntro -> m ()
        forwardMemberXGrpMemCon GroupMemberIntro {introId, introStatus}
          | introStatus == GMIntroToConnected = updateStatus introId GMIntroConnected
          | introStatus `elem` [GMIntroReConnected, GMIntroConnected] = pure ()
          | otherwise = updateStatus introId GMIntroReConnected
        updateStatus introId status = withStore' $ \db -> updateIntroStatus db introId status

    xGrpMemDel :: GroupInfo -> GroupMember -> MemberId -> RcvMessage -> UTCTime -> m ()
    xGrpMemDel gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId msg brokerTs = do
      if membership.memberId == memId
        then checkRole membership $ do
          deleteGroupLinkIfExists user gInfo
          -- member records are not deleted to keep history
          members <- withStore' $ \db -> getGroupMembers db user gInfo
          deleteMembersConnections user members
          withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemRemoved
          deleteMemberItem RGEUserDeleted
          toView $ CRDeletedMemberUser user gInfo {membership = membership {memberStatus = GSMemRemoved}} m
        else
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db user gInfo memId) >>= \case
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

    xGrpLeave :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> m ()
    xGrpLeave gInfo m msg brokerTs = do
      deleteMemberConnection user m
      -- member record is not deleted to allow creation of "member left" chat item
      withStore' $ \db -> updateGroupMemberStatus db userId m GSMemLeft
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent RGEMemberLeft)
      groupMsgToView gInfo ci
      toView $ CRLeftMember user gInfo m {memberStatus = GSMemLeft}

    xGrpDel :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> m ()
    xGrpDel gInfo@GroupInfo {membership} m@GroupMember {memberRole} msg brokerTs = do
      when (memberRole /= GROwner) $ throwChatError $ CEGroupUserRole gInfo GROwner
      ms <- withStore' $ \db -> do
        members <- getGroupMembers db user gInfo
        updateGroupMemberStatus db userId membership GSMemGroupDeleted
        pure members
      -- member records are not deleted to keep history
      deleteMembersConnections user ms
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent RGEGroupDeleted)
      groupMsgToView gInfo ci
      toView $ CRGroupDeleted user gInfo {membership = membership {memberStatus = GSMemGroupDeleted}} m

    xGrpInfo :: GroupInfo -> GroupMember -> GroupProfile -> RcvMessage -> UTCTime -> m ()
    xGrpInfo g@GroupInfo {groupProfile = p} m@GroupMember {memberRole} p' msg brokerTs
      | memberRole < GROwner = messageError "x.grp.info with insufficient member permissions"
      | otherwise = unless (p == p') $ do
          g' <- withStore $ \db -> updateGroupProfile db user g p'
          toView $ CRGroupUpdated user g g' (Just m)
          let cd = CDGroupRcv g' m
          unless (sameGroupProfileInfo p p') $ do
            ci <- saveRcvChatItem user cd msg brokerTs (CIRcvGroupEvent $ RGEGroupUpdated p')
            groupMsgToView g' ci
          createGroupFeatureChangedItems user cd CIRcvGroupFeature g g'

    xGrpDirectInv :: GroupInfo -> GroupMember -> Connection -> ConnReqInvitation -> Maybe MsgContent -> RcvMessage -> UTCTime -> m ()
    xGrpDirectInv g m mConn connReq mContent_ msg brokerTs = do
      unless (groupFeatureAllowed SGFDirectMessages g) $ messageError "x.grp.direct.inv: direct messages not allowed"
      let GroupMember {memberContactId} = m
      subMode <- chatReadVar subscriptionMode
      case memberContactId of
        Nothing -> createNewContact subMode
        Just mContactId -> do
          mCt <- withStore $ \db -> getContact db user mContactId
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
          let p = userProfileToSend user (fromLocalProfile <$> incognitoMembershipProfile g) Nothing
          dm <- directMessage $ XInfo p
          joinAgentConnectionAsync user True connReq dm subMode
        createItems mCt' m' = do
          createInternalChatItem user (CDGroupRcv g m') (CIRcvGroupEvent RGEMemberCreatedContact) Nothing
          toView $ CRNewMemberContactReceivedInv user mCt' g m'
          forM_ mContent_ $ \mc -> do
            ci <- saveRcvChatItem user (CDDirectRcv mCt') msg brokerTs (CIRcvMsgContent mc)
            toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat mCt') ci)

    securityCodeChanged :: Contact -> m ()
    securityCodeChanged ct = do
      toView $ CRContactVerificationReset user ct
      createInternalChatItem user (CDDirectRcv ct) (CIRcvConnEvent RCEVerificationCodeReset) Nothing

    xGrpMsgForward :: GroupInfo -> GroupMember -> MemberId -> ChatMessage 'Json -> UTCTime -> m ()
    xGrpMsgForward gInfo@GroupInfo {groupId} m memberId msg msgTs = do
      when (m.memberRole < GRAdmin) $ throwChatError (CEGroupContactRole m.localDisplayName)
      author <- withStore $ \db -> getGroupMemberByMemberId db user gInfo memberId
      processForwardedMsg author msg
      where
        -- Note: forwarded group events (see forwardedGroupMsg) should include msgId to be deduplicated
        processForwardedMsg :: GroupMember -> ChatMessage 'Json -> m ()
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
            XInfo p -> xInfoMember gInfo author p
            XGrpMemNew memInfo -> xGrpMemNew gInfo author memInfo rcvMsg msgTs
            XGrpMemRole memId memRole -> xGrpMemRole gInfo author memId memRole rcvMsg msgTs
            XGrpMemDel memId -> xGrpMemDel gInfo author memId rcvMsg msgTs
            XGrpLeave -> xGrpLeave gInfo author rcvMsg msgTs
            XGrpDel -> xGrpDel gInfo author rcvMsg msgTs
            XGrpInfo p' -> xGrpInfo gInfo author p' rcvMsg msgTs
            _ -> messageError $ "x.grp.msg.forward: unsupported forwarded event " <> T.pack (show $ toCMEventTag event)

    directMsgReceived :: Contact -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> m ()
    directMsgReceived ct conn@Connection {connId} msgMeta msgRcpts = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore' $ \db -> updateSndMsgDeliveryStatus db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateDirectItemStatus ct conn agentMsgId $ CISSndRcvd msgRcptStatus SSPComplete

    -- TODO [batch send] update status of all messages in batch
    -- - this is for when we implement identifying inactive connections
    -- - regular messages sent in batch would all be marked as delivered by a single receipt
    -- - repeat for directMsgReceived if same logic is applied to direct messages
    -- - getChatItemIdByAgentMsgId to return [ChatItemId]
    groupMsgReceived :: GroupInfo -> GroupMember -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> m ()
    groupMsgReceived gInfo m conn@Connection {connId} msgMeta msgRcpts = do
      checkIntegrityCreateItem (CDGroupRcv gInfo m) msgMeta
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore' $ \db -> updateSndMsgDeliveryStatus db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateGroupItemStatus gInfo m conn agentMsgId $ CISSndRcvd msgRcptStatus SSPComplete

    updateDirectItemStatus :: Contact -> Connection -> AgentMsgId -> CIStatus 'MDSnd -> m ()
    updateDirectItemStatus ct@Contact {contactId} Connection {connId} msgId newStatus =
      withStore' (\db -> getDirectChatItemByAgentMsgId db user contactId connId msgId) >>= \case
        Just (CChatItem SMDSnd ChatItem {meta = CIMeta {itemStatus = CISSndRcvd _ _}}) -> pure ()
        Just (CChatItem SMDSnd ChatItem {meta = CIMeta {itemId, itemStatus}})
          | itemStatus == newStatus -> pure ()
          | otherwise -> do
              chatItem <- withStore $ \db -> updateDirectChatItemStatus db user ct itemId newStatus
              toView $ CRChatItemStatusUpdated user (AChatItem SCTDirect SMDSnd (DirectChat ct) chatItem)
        _ -> pure ()

    updateGroupMemSndStatus :: ChatItemId -> GroupMemberId -> CIStatus 'MDSnd -> m Bool
    updateGroupMemSndStatus itemId groupMemberId newStatus =
      runExceptT (withStore $ \db -> getGroupSndStatus db itemId groupMemberId) >>= \case
        Right (CISSndRcvd _ _) -> pure False
        Right memStatus
          | memStatus == newStatus -> pure False
          | otherwise -> withStore' (\db -> updateGroupSndStatus db itemId groupMemberId newStatus) $> True
        _ -> pure False

    updateGroupItemStatus :: GroupInfo -> GroupMember -> Connection -> AgentMsgId -> CIStatus 'MDSnd -> m ()
    updateGroupItemStatus gInfo@GroupInfo {groupId} GroupMember {groupMemberId} Connection {connId} msgId newMemStatus =
      withStore' (\db -> getGroupChatItemByAgentMsgId db user groupId connId msgId) >>= \case
        Just (CChatItem SMDSnd ChatItem {meta = CIMeta {itemStatus = CISSndRcvd _ SSPComplete}}) -> pure ()
        Just (CChatItem SMDSnd ChatItem {meta = CIMeta {itemId, itemStatus}}) -> do
          memStatusChanged <- updateGroupMemSndStatus itemId groupMemberId newMemStatus
          when memStatusChanged $ do
            memStatusCounts <- withStore' (`getGroupSndStatusCounts` itemId)
            let newStatus = membersGroupItemStatus memStatusCounts
            when (newStatus /= itemStatus) $ do
              chatItem <- withStore $ \db -> updateGroupChatItemStatus db user gInfo itemId newStatus
              toView $ CRChatItemStatusUpdated user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) chatItem)
        _ -> pure ()

metaBrokerTs :: MsgMeta -> UTCTime
metaBrokerTs MsgMeta {broker = (_, brokerTs)} = brokerTs

sameMemberId :: MemberId -> GroupMember -> Bool
sameMemberId memId GroupMember {memberId} = memId == memberId

updatePeerChatVRange :: ChatMonad m => Connection -> VersionRange -> m Connection
updatePeerChatVRange conn@Connection {connId, peerChatVRange} msgChatVRange = do
  let jMsgChatVRange = JVersionRange msgChatVRange
  if jMsgChatVRange /= peerChatVRange
    then do
      withStore' $ \db -> setPeerChatVRange db connId msgChatVRange
      pure conn {peerChatVRange = jMsgChatVRange}
    else pure conn

updateMemberChatVRange :: ChatMonad m => GroupMember -> Connection -> VersionRange -> m (GroupMember, Connection)
updateMemberChatVRange mem@GroupMember {groupMemberId} conn@Connection {connId, peerChatVRange} msgChatVRange = do
  let jMsgChatVRange = JVersionRange msgChatVRange
  if jMsgChatVRange /= peerChatVRange
    then do
      withStore' $ \db -> do
        setPeerChatVRange db connId msgChatVRange
        setMemberChatVRange db groupMemberId msgChatVRange
      let conn' = conn {peerChatVRange = jMsgChatVRange}
      pure (mem {memberChatVRange = jMsgChatVRange, activeConn = Just conn'}, conn')
    else pure (mem, conn)

parseFileDescription :: (ChatMonad m, FilePartyI p) => Text -> m (ValidFileDescription p)
parseFileDescription =
  liftEither . first (ChatError . CEInvalidFileDescription) . (strDecode . encodeUtf8)

sendDirectFileInline :: ChatMonad m => Contact -> FileTransferMeta -> SharedMsgId -> m ()
sendDirectFileInline ct ft sharedMsgId = do
  msgDeliveryId <- sendFileInline_ ft sharedMsgId $ sendDirectContactMessage ct
  withStore $ \db -> updateSndDirectFTDelivery db ct ft msgDeliveryId

sendMemberFileInline :: ChatMonad m => GroupMember -> Connection -> FileTransferMeta -> SharedMsgId -> m ()
sendMemberFileInline m@GroupMember {groupId} conn ft sharedMsgId = do
  msgDeliveryId <- sendFileInline_ ft sharedMsgId $ \msg -> sendDirectMessage conn msg $ GroupId groupId
  withStore' $ \db -> updateSndGroupFTDelivery db m conn ft msgDeliveryId

sendFileInline_ :: ChatMonad m => FileTransferMeta -> SharedMsgId -> (ChatMsgEvent 'Binary -> m (SndMessage, Int64)) -> m Int64
sendFileInline_ FileTransferMeta {filePath, chunkSize} sharedMsgId sendMsg =
  sendChunks 1 =<< liftIO . B.readFile =<< toFSFilePath filePath
  where
    sendChunks chunkNo bytes = do
      let (chunk, rest) = B.splitAt chSize bytes
      (_, msgDeliveryId) <- sendMsg $ BFileChunk sharedMsgId $ FileChunk chunkNo chunk
      if B.null rest
        then pure msgDeliveryId
        else sendChunks (chunkNo + 1) rest
    chSize = fromIntegral chunkSize

parseChatMessage :: ChatMonad m => Connection -> ByteString -> m (ChatMessage 'Json)
parseChatMessage conn s = do
  case parseChatMessages s of
    [msg] -> liftEither . first (ChatError . errType) $ (\(ACMsg _ m) -> checkEncoding m) =<< msg
    _ -> throwChatError $ CEException "parseChatMessage: single message is expected"
  where
    errType = CEInvalidChatMessage conn Nothing (safeDecodeUtf8 s)
{-# INLINE parseChatMessage #-}

sendFileChunk :: ChatMonad m => User -> SndFileTransfer -> m ()
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
        closeFileHandle fileId sndFiles
        deleteAgentConnectionAsync user acId

sendFileChunkNo :: ChatMonad m => SndFileTransfer -> Integer -> m ()
sendFileChunkNo ft@SndFileTransfer {agentConnId = AgentConnId acId} chunkNo = do
  chunkBytes <- readFileChunk ft chunkNo
  msgId <- withAgent $ \a -> sendMessage a acId SMP.noMsgFlags $ smpEncode FileChunk {chunkNo, chunkBytes}
  withStore' $ \db -> updateSndFileChunkMsg db ft chunkNo msgId

readFileChunk :: ChatMonad m => SndFileTransfer -> Integer -> m ByteString
readFileChunk SndFileTransfer {fileId, filePath, chunkSize} chunkNo = do
  fsFilePath <- toFSFilePath filePath
  read_ fsFilePath `catchThrow` (ChatError . CEFileRead filePath . show)
  where
    read_ fsFilePath = do
      h <- getFileHandle fileId fsFilePath sndFiles ReadMode
      pos <- hTell h
      let pos' = (chunkNo - 1) * chunkSize
      when (pos /= pos') $ hSeek h AbsoluteSeek pos'
      liftIO . B.hGet h $ fromInteger chunkSize

parseFileChunk :: ChatMonad m => ByteString -> m FileChunk
parseFileChunk = liftEither . first (ChatError . CEFileRcvChunk) . smpDecode

appendFileChunk :: forall m. ChatMonad m => RcvFileTransfer -> Integer -> ByteString -> Bool -> m ()
appendFileChunk ft@RcvFileTransfer {fileId, fileStatus, cryptoArgs} chunkNo chunk final =
  case fileStatus of
    RFSConnected RcvFileInfo {filePath} -> append_ filePath
    -- sometimes update of file transfer status to FSConnected
    -- doesn't complete in time before MSG with first file chunk
    RFSAccepted RcvFileInfo {filePath} -> append_ filePath
    RFSCancelled _ -> pure ()
    _ -> throwChatError $ CEFileInternal "receiving file transfer not in progress"
  where
    append_ :: FilePath -> m ()
    append_ filePath = do
      fsFilePath <- toFSFilePath filePath
      h <- getFileHandle fileId fsFilePath rcvFiles AppendMode
      liftIO (B.hPut h chunk >> hFlush h) `catchThrow` (fileErr . show)
      withStore' $ \db -> updatedRcvFileChunkStored db ft chunkNo
      when final $ do
        closeFileHandle fileId rcvFiles
        forM_ cryptoArgs $ \cfArgs -> do
          tmpFile <- getChatTempDirectory >>= (`uniqueCombine` ft.fileInvitation.fileName)
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

getFileHandle :: ChatMonad m => Int64 -> FilePath -> (ChatController -> TVar (Map Int64 Handle)) -> IOMode -> m Handle
getFileHandle fileId filePath files ioMode = do
  fs <- asks files
  h_ <- M.lookup fileId <$> readTVarIO fs
  maybe (newHandle fs) pure h_
  where
    newHandle fs = do
      h <- openFile filePath ioMode `catchThrow` (ChatError . CEFileInternal . show)
      atomically . modifyTVar fs $ M.insert fileId h
      pure h

isFileActive :: ChatMonad m => Int64 -> (ChatController -> TVar (Map Int64 Handle)) -> m Bool
isFileActive fileId files = do
  fs <- asks files
  isJust . M.lookup fileId <$> readTVarIO fs

cancelRcvFileTransfer :: ChatMonad m => User -> RcvFileTransfer -> m (Maybe ConnId)
cancelRcvFileTransfer user ft@RcvFileTransfer {fileId, xftpRcvFile, rcvFileInline} =
  cancel' `catchChatError` (\e -> toView (CRChatError (Just user) e) $> fileConnId)
  where
    cancel' = do
      closeFileHandle fileId rcvFiles
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

cancelSndFile :: ChatMonad m => User -> FileTransferMeta -> [SndFileTransfer] -> Bool -> m [ConnId]
cancelSndFile user FileTransferMeta {fileId, xftpSndFile} fts sendCancel = do
  withStore' (\db -> updateFileCancelled db user fileId CIFSSndCancelled)
    `catchChatError` (toView . CRChatError (Just user))
  case xftpSndFile of
    Nothing ->
      catMaybes <$> forM fts (\ft -> cancelSndFileTransfer user ft sendCancel)
    Just xsf -> do
      forM_ fts (\ft -> cancelSndFileTransfer user ft False)
      agentXFTPDeleteSndFileRemote user xsf fileId `catchChatError` (toView . CRChatError (Just user))
      pure []

cancelSndFileTransfer :: ChatMonad m => User -> SndFileTransfer -> Bool -> m (Maybe ConnId)
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
          (sharedMsgId, conn) <- withStore $ \db -> (,) <$> getSharedMsgIdByFileId db userId fileId <*> getConnectionById db user connId
          void . sendDirectMessage conn (BFileChunk sharedMsgId FileChunkCancel) $ ConnectionId connId
        _ -> withAgent $ \a -> void . sendMessage a acId SMP.noMsgFlags $ smpEncode FileChunkCancel
      pure fileConnId
    fileConnId = if isNothing fileInline then Just acId else Nothing

closeFileHandle :: ChatMonad m => Int64 -> (ChatController -> TVar (Map Int64 Handle)) -> m ()
closeFileHandle fileId files = do
  fs <- asks files
  h_ <- atomically . stateTVar fs $ \m -> (M.lookup fileId m, M.delete fileId m)
  liftIO $ mapM_ hClose h_ `catchAll_` pure ()

deleteMembersConnections :: ChatMonad m => User -> [GroupMember] -> m ()
deleteMembersConnections user members = do
  let memberConns =
        filter (\Connection {connStatus} -> connStatus /= ConnDeleted) $
          mapMaybe (\GroupMember {activeConn} -> activeConn) members
  deleteAgentConnectionsAsync user $ map aConnId memberConns
  forM_ memberConns $ \conn -> withStore' $ \db -> updateConnectionStatus db conn ConnDeleted

deleteMemberConnection :: ChatMonad m => User -> GroupMember -> m ()
deleteMemberConnection user GroupMember {activeConn} = do
  forM_ activeConn $ \conn -> do
    deleteAgentConnectionAsync user $ aConnId conn
    withStore' $ \db -> updateConnectionStatus db conn ConnDeleted

deleteOrUpdateMemberRecord :: ChatMonad m => User -> GroupMember -> m ()
deleteOrUpdateMemberRecord user@User {userId} member =
  withStore' $ \db ->
    checkGroupMemberHasItems db user member >>= \case
      Just _ -> updateGroupMemberStatus db userId member GSMemRemoved
      Nothing -> deleteGroupMember db user member

sendDirectContactMessage :: (MsgEncodingI e, ChatMonad m) => Contact -> ChatMsgEvent e -> m (SndMessage, Int64)
sendDirectContactMessage ct@Contact {activeConn = Nothing} _ = throwChatError $ CEContactNotReady ct
sendDirectContactMessage ct@Contact {activeConn = Just conn@Connection {connId, connStatus}, contactStatus} chatMsgEvent
  | connStatus /= ConnReady && connStatus /= ConnSndReady = throwChatError $ CEContactNotReady ct
  | contactStatus /= CSActive = throwChatError $ CEContactNotActive ct
  | connDisabled conn = throwChatError $ CEContactDisabled ct
  | otherwise = sendDirectMessage conn chatMsgEvent (ConnectionId connId)

sendDirectMessage :: (MsgEncodingI e, ChatMonad m) => Connection -> ChatMsgEvent e -> ConnOrGroupId -> m (SndMessage, Int64)
sendDirectMessage conn chatMsgEvent connOrGroupId = do
  when (connDisabled conn) $ throwChatError (CEConnectionDisabled conn)
  msg@SndMessage {msgId, msgBody} <- createSndMessage chatMsgEvent connOrGroupId
  (msg,) <$> deliverMessage conn (toCMEventTag chatMsgEvent) msgBody msgId

createSndMessage :: (MsgEncodingI e, ChatMonad m) => ChatMsgEvent e -> ConnOrGroupId -> m SndMessage
createSndMessage chatMsgEvent connOrGroupId = do
  gVar <- asks random
  vr <- chatVersionRange
  withStore $ \db -> createNewSndMessage db gVar connOrGroupId chatMsgEvent (encodeMessage vr)
  where
    encodeMessage chatVRange sharedMsgId =
      encodeChatMessage ChatMessage {chatVRange, msgId = Just sharedMsgId, chatMsgEvent}

sendGroupMemberMessages :: forall e m. (MsgEncodingI e, ChatMonad m) => User -> Connection -> NonEmpty (ChatMsgEvent e) -> GroupId -> m ()
sendGroupMemberMessages user conn@Connection {connId} events groupId = do
  when (connDisabled conn) $ throwChatError (CEConnectionDisabled conn)
  (errs, msgs) <- partitionEithers <$> createSndMessages
  unless (null errs) $ toView $ CRChatErrors (Just user) errs
  unless (null msgs) $ do
    let (errs', msgBatches) = partitionEithers $ batchMessages maxChatMsgSize msgs
    -- shouldn't happen, as large messages would have caused createNewSndMessage to throw SELargeMsg
    unless (null errs') $ toView $ CRChatErrors (Just user) errs'
    forM_ msgBatches $ \batch ->
      processBatch batch `catchChatError` (toView . CRChatError (Just user))
  where
    processBatch :: MsgBatch -> m ()
    processBatch (MsgBatch builder sndMsgs) = do
      let batchBody = LB.toStrict $ toLazyByteString builder
      agentMsgId <- withAgent $ \a -> sendMessage a (aConnId conn) MsgFlags {notification = True} batchBody
      let sndMsgDelivery = SndMsgDelivery {connId, agentMsgId}
      void . withStoreBatch' $ \db -> map (\SndMessage {msgId} -> createSndMsgDelivery db sndMsgDelivery msgId) sndMsgs
    createSndMessages :: m [Either ChatError SndMessage]
    createSndMessages = do
      gVar <- asks random
      vr <- chatVersionRange
      withStoreBatch $ \db -> map (createMsg db gVar vr) (toList events)
    createMsg db gVar chatVRange evnt = do
      r <- runExceptT $ createNewSndMessage db gVar (GroupId groupId) evnt (encodeMessage chatVRange evnt)
      pure $ first ChatErrorStore r
    encodeMessage chatVRange evnt sharedMsgId =
      encodeChatMessage ChatMessage {chatVRange, msgId = Just sharedMsgId, chatMsgEvent = evnt}

directMessage :: (MsgEncodingI e, ChatMonad m) => ChatMsgEvent e -> m ByteString
directMessage chatMsgEvent = do
  chatVRange <- chatVersionRange
  let r = encodeChatMessage ChatMessage {chatVRange, msgId = Nothing, chatMsgEvent}
  case r of
    ECMEncoded encodedBody -> pure . LB.toStrict $ encodedBody
    ECMLarge -> throwChatError $ CEException "large message"

deliverMessage :: ChatMonad m => Connection -> CMEventTag e -> LazyMsgBody -> MessageId -> m Int64
deliverMessage conn cmEventTag msgBody msgId = do
  let msgFlags = MsgFlags {notification = hasNotification cmEventTag}
  deliverMessage' conn msgFlags msgBody msgId

deliverMessage' :: ChatMonad m => Connection -> MsgFlags -> LazyMsgBody -> MessageId -> m Int64
deliverMessage' conn msgFlags msgBody msgId =
  deliverMessages [(conn, msgFlags, msgBody, msgId)] >>= \case
    [r] -> liftEither r
    rs -> throwChatError $ CEInternalError $ "deliverMessage: expected 1 result, got " <> show (length rs)

deliverMessages :: ChatMonad' m => [(Connection, MsgFlags, LazyMsgBody, MessageId)] -> m [Either ChatError Int64]
deliverMessages msgReqs = do
  sent <- zipWith prepareBatch msgReqs <$> withAgent' (`sendMessages` aReqs)
  withStoreBatch $ \db -> map (bindRight $ createDelivery db) sent
  where
    aReqs = map (\(conn, msgFlags, msgBody, _msgId) -> (aConnId conn, msgFlags, LB.toStrict msgBody)) msgReqs
    prepareBatch req = bimap (`ChatErrorAgent` Nothing) (req,)
    createDelivery :: DB.Connection -> ((Connection, MsgFlags, LazyMsgBody, MessageId), AgentMsgId) -> IO (Either ChatError Int64)
    createDelivery db ((Connection {connId}, _, _, msgId), agentMsgId) =
      Right <$> createSndMsgDelivery db (SndMsgDelivery {connId, agentMsgId}) msgId

sendGroupMessage :: (MsgEncodingI e, ChatMonad m) => User -> GroupInfo -> [GroupMember] -> ChatMsgEvent e -> m (SndMessage, [GroupMember])
sendGroupMessage user GroupInfo {groupId} members chatMsgEvent = do
  msg@SndMessage {msgId, msgBody} <- createSndMessage chatMsgEvent (GroupId groupId)
  recipientMembers <- liftIO $ shuffleMembers (filter memberCurrent members)
  let msgFlags = MsgFlags {notification = hasNotification $ toCMEventTag chatMsgEvent}
      (toSend, pending) = foldr addMember ([], []) recipientMembers
      msgReqs = map (\(_, conn) -> (conn, msgFlags, msgBody, msgId)) toSend
  delivered <- deliverMessages msgReqs
  let errors = lefts delivered
  unless (null errors) $ toView $ CRChatErrors (Just user) errors
  stored <- withStoreBatch' $ \db -> map (\m -> createPendingGroupMessage db (groupMemberId' m) msgId Nothing) pending
  let sentToMembers = filterSent delivered toSend fst <> filterSent stored pending id
  pure (msg, sentToMembers)
  where
    shuffleMembers :: [GroupMember] -> IO [GroupMember]
    shuffleMembers ms = do
      let (adminMs, otherMs) = partition isAdmin ms
      liftM2 (<>) (shuffle adminMs) (shuffle otherMs)
      where
        isAdmin GroupMember {memberRole} = memberRole >= GRAdmin
    addMember m (toSend, pending) = case memberSendAction chatMsgEvent members m of
      Just (MSASend conn) -> ((m, conn) : toSend, pending)
      Just MSAPending -> (toSend, m : pending)
      Nothing -> (toSend, pending)
    filterSent :: [Either ChatError a] -> [mem] -> (mem -> GroupMember) -> [GroupMember]
    filterSent rs ms mem = [mem m | (Right _, m) <- zip rs ms]

data MemberSendAction = MSASend Connection | MSAPending

memberSendAction :: ChatMsgEvent e -> [GroupMember] -> GroupMember -> Maybe MemberSendAction
memberSendAction chatMsgEvent members m = case memberConn m of
  Nothing -> pendingOrForwarded
  Just conn@Connection {connStatus}
    | connDisabled conn || connStatus == ConnDeleted -> Nothing
    | connStatus == ConnSndReady || connStatus == ConnReady -> Just (MSASend conn)
    | otherwise -> pendingOrForwarded
  where
    pendingOrForwarded
      | forwardSupported && isForwardedGroupMsg chatMsgEvent = Nothing
      | isXGrpMsgForward chatMsgEvent = Nothing
      | otherwise = Just MSAPending
      where
        forwardSupported =
          let mcvr = memberChatVRange' m
           in isCompatibleRange mcvr groupForwardVRange && invitingMemberSupportsForward
        invitingMemberSupportsForward = case m.invitedByGroupMemberId of
          Just invMemberId ->
            -- can be optimized for large groups by replacing [GroupMember] with Map GroupMemberId GroupMember
            case find (\m' -> groupMemberId' m' == invMemberId) members of
              Just invitingMember -> do
                let mcvr = memberChatVRange' invitingMember
                isCompatibleRange mcvr groupForwardVRange
              Nothing -> False
          Nothing -> False
        isXGrpMsgForward ev = case ev of
          XGrpMsgForward {} -> True
          _ -> False

sendGroupMemberMessage :: forall e m. (MsgEncodingI e, ChatMonad m) => User -> GroupMember -> ChatMsgEvent e -> Int64 -> Maybe Int64 -> m () -> m ()
sendGroupMemberMessage user m@GroupMember {groupMemberId} chatMsgEvent groupId introId_ postDeliver = do
  msg <- createSndMessage chatMsgEvent (GroupId groupId)
  messageMember msg `catchChatError` (\e -> toView (CRChatError (Just user) e))
  where
    messageMember :: SndMessage -> m ()
    messageMember SndMessage {msgId, msgBody} = forM_ (memberSendAction chatMsgEvent [m] m) $ \case
      MSASend conn -> deliverMessage conn (toCMEventTag chatMsgEvent) msgBody msgId >> postDeliver
      MSAPending -> withStore' $ \db -> createPendingGroupMessage db groupMemberId msgId introId_

sendPendingGroupMessages :: ChatMonad m => User -> GroupMember -> Connection -> m ()
sendPendingGroupMessages user GroupMember {groupMemberId, localDisplayName} conn = do
  pendingMessages <- withStore' $ \db -> getPendingGroupMessages db groupMemberId
  -- TODO ensure order - pending messages interleave with user input messages
  forM_ pendingMessages $ \pgm ->
    processPendingMessage pgm `catchChatError` (toView . CRChatError (Just user))
  where
    processPendingMessage PendingGroupMessage {msgId, cmEventTag = ACMEventTag _ tag, msgBody, introId_} = do
      void $ deliverMessage conn tag msgBody msgId
      withStore' $ \db -> deletePendingGroupMessage db groupMemberId msgId
      case tag of
        XGrpMemFwd_ -> case introId_ of
          Just introId -> withStore' $ \db -> updateIntroStatus db introId GMIntroInvForwarded
          _ -> throwChatError $ CEGroupMemberIntroNotFound localDisplayName
        _ -> pure ()

-- TODO [batch send] refactor direct message processing same as groups (e.g. checkIntegrity before processing)
saveDirectRcvMSG :: ChatMonad m => Connection -> MsgMeta -> CommandId -> MsgBody -> m (Connection, RcvMessage)
saveDirectRcvMSG conn@Connection {connId} agentMsgMeta agentAckCmdId msgBody =
  case parseChatMessages msgBody of
    [Right (ACMsg _ ChatMessage {chatVRange, msgId = sharedMsgId_, chatMsgEvent})] -> do
      conn' <- updatePeerChatVRange conn chatVRange
      let agentMsgId = fst $ recipient agentMsgMeta
          newMsg = NewRcvMessage {chatMsgEvent, msgBody}
          rcvMsgDelivery = RcvMsgDelivery {connId, agentMsgId, agentMsgMeta, agentAckCmdId}
      msg <- withStore $ \db -> createNewMessageAndRcvMsgDelivery db (ConnectionId connId) newMsg sharedMsgId_ rcvMsgDelivery Nothing
      pure (conn', msg)
    [Left e] -> error $ "saveDirectRcvMSG: error parsing chat message: " <> e
    _ -> error "saveDirectRcvMSG: batching not supported"

saveGroupRcvMsg :: (MsgEncodingI e, ChatMonad m) => User -> GroupId -> GroupMember -> Connection -> MsgMeta -> CommandId -> MsgBody -> ChatMessage e -> m (GroupMember, Connection, RcvMessage)
saveGroupRcvMsg user groupId authorMember conn@Connection {connId} agentMsgMeta agentAckCmdId msgBody ChatMessage {chatVRange, msgId = sharedMsgId_, chatMsgEvent} = do
  (am', conn') <- updateMemberChatVRange authorMember conn chatVRange
  let agentMsgId = fst $ recipient agentMsgMeta
      newMsg = NewRcvMessage {chatMsgEvent, msgBody}
      rcvMsgDelivery = RcvMsgDelivery {connId, agentMsgId, agentMsgMeta, agentAckCmdId}
      amId = Just am'.groupMemberId
  msg <-
    withStore (\db -> createNewMessageAndRcvMsgDelivery db (GroupId groupId) newMsg sharedMsgId_ rcvMsgDelivery amId)
      `catchChatError` \e -> case e of
        ChatErrorStore (SEDuplicateGroupMessage _ _ _ (Just forwardedByGroupMemberId)) -> do
          fm <- withStore $ \db -> getGroupMember db user groupId forwardedByGroupMemberId
          forM_ (memberConn fm) $ \fmConn ->
            void $ sendDirectMessage fmConn (XGrpMemCon am'.memberId) (GroupId groupId)
          throwError e
        _ -> throwError e
  pure (am', conn', msg)

saveGroupFwdRcvMsg :: (MsgEncodingI e, ChatMonad m) => User -> GroupId -> GroupMember -> GroupMember -> MsgBody -> ChatMessage e -> m RcvMessage
saveGroupFwdRcvMsg user groupId forwardingMember refAuthorMember msgBody ChatMessage {msgId = sharedMsgId_, chatMsgEvent} = do
  let newMsg = NewRcvMessage {chatMsgEvent, msgBody}
      fwdMemberId = Just $ groupMemberId' forwardingMember
      refAuthorId = Just $ groupMemberId' refAuthorMember
  withStore (\db -> createNewRcvMessage db (GroupId groupId) newMsg sharedMsgId_ refAuthorId fwdMemberId)
    `catchChatError` \e -> case e of
      ChatErrorStore (SEDuplicateGroupMessage _ _ (Just authorGroupMemberId) Nothing) -> do
        am <- withStore $ \db -> getGroupMember db user groupId authorGroupMemberId
        if sameMemberId refAuthorMember.memberId am
          then forM_ (memberConn forwardingMember) $ \fmConn ->
            void $ sendDirectMessage fmConn (XGrpMemCon am.memberId) (GroupId groupId)
          else toView $ CRMessageError user "error" "saveGroupFwdRcvMsg: referenced author member id doesn't match message member id"
        throwError e
      _ -> throwError e

saveSndChatItem :: ChatMonad m => User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> m (ChatItem c 'MDSnd)
saveSndChatItem user cd msg content = saveSndChatItem' user cd msg content Nothing Nothing Nothing False

saveSndChatItem' :: ChatMonad m => User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> Maybe (CIFile 'MDSnd) -> Maybe (CIQuote c) -> Maybe CITimed -> Bool -> m (ChatItem c 'MDSnd)
saveSndChatItem' user cd msg@SndMessage {sharedMsgId} content ciFile quotedItem itemTimed live = do
  createdAt <- liftIO getCurrentTime
  ciId <- withStore' $ \db -> do
    when (ciRequiresAttention content) $ updateChatTs db user cd createdAt
    ciId <- createNewSndChatItem db user cd msg content quotedItem itemTimed live createdAt
    forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
    pure ciId
  liftIO $ mkChatItem cd ciId content ciFile quotedItem (Just sharedMsgId) itemTimed live createdAt Nothing createdAt

saveRcvChatItem :: ChatMonad m => User -> ChatDirection c 'MDRcv -> RcvMessage -> UTCTime -> CIContent 'MDRcv -> m (ChatItem c 'MDRcv)
saveRcvChatItem user cd msg@RcvMessage {sharedMsgId_} brokerTs content =
  saveRcvChatItem' user cd msg sharedMsgId_ brokerTs content Nothing Nothing False

saveRcvChatItem' :: ChatMonad m => User -> ChatDirection c 'MDRcv -> RcvMessage -> Maybe SharedMsgId -> UTCTime -> CIContent 'MDRcv -> Maybe (CIFile 'MDRcv) -> Maybe CITimed -> Bool -> m (ChatItem c 'MDRcv)
saveRcvChatItem' user cd msg@RcvMessage {forwardedByMember} sharedMsgId_ brokerTs content ciFile itemTimed live = do
  createdAt <- liftIO getCurrentTime
  (ciId, quotedItem) <- withStore' $ \db -> do
    when (ciRequiresAttention content) $ updateChatTs db user cd createdAt
    (ciId, quotedItem) <- createNewRcvChatItem db user cd msg sharedMsgId_ content itemTimed live brokerTs createdAt
    forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
    pure (ciId, quotedItem)
  liftIO $ mkChatItem cd ciId content ciFile quotedItem sharedMsgId_ itemTimed live brokerTs forwardedByMember createdAt

mkChatItem :: forall c d. MsgDirectionI d => ChatDirection c d -> ChatItemId -> CIContent d -> Maybe (CIFile d) -> Maybe (CIQuote c) -> Maybe SharedMsgId -> Maybe CITimed -> Bool -> ChatItemTs -> Maybe GroupMemberId -> UTCTime -> IO (ChatItem c d)
mkChatItem cd ciId content file quotedItem sharedMsgId itemTimed live itemTs forwardedByMember currentTs = do
  let itemText = ciContentToText content
      itemStatus = ciCreateStatus content
      meta = mkCIMeta ciId content itemText itemStatus sharedMsgId Nothing False itemTimed (justTrue live) currentTs itemTs forwardedByMember currentTs currentTs
  pure ChatItem {chatDir = toCIDirection cd, meta, content, formattedText = parseMaybeMarkdownList itemText, quotedItem, reactions = [], file}

deleteDirectCI :: (ChatMonad m, MsgDirectionI d) => User -> Contact -> ChatItem 'CTDirect d -> Bool -> Bool -> m ChatResponse
deleteDirectCI user ct ci@ChatItem {file} byUser timed = do
  deleteCIFile user file
  withStoreCtx' (Just "deleteDirectCI, deleteDirectChatItem") $ \db -> deleteDirectChatItem db user ct ci
  pure $ CRChatItemDeleted user (AChatItem SCTDirect msgDirection (DirectChat ct) ci) Nothing byUser timed

deleteGroupCI :: (ChatMonad m, MsgDirectionI d) => User -> GroupInfo -> ChatItem 'CTGroup d -> Bool -> Bool -> Maybe GroupMember -> UTCTime -> m ChatResponse
deleteGroupCI user gInfo ci@ChatItem {file} byUser timed byGroupMember_ deletedTs = do
  deleteCIFile user file
  toCi <- withStoreCtx' (Just "deleteGroupCI, deleteGroupChatItem ...") $ \db ->
    case byGroupMember_ of
      Nothing -> deleteGroupChatItem db user gInfo ci $> Nothing
      Just m -> Just <$> updateGroupChatItemModerated db user gInfo ci m deletedTs
  pure $ CRChatItemDeleted user (gItem ci) (gItem <$> toCi) byUser timed
  where
    gItem = AChatItem SCTGroup msgDirection (GroupChat gInfo)

deleteCIFile :: (ChatMonad m, MsgDirectionI d) => User -> Maybe (CIFile d) -> m ()
deleteCIFile user file_ =
  forM_ file_ $ \file -> do
    fileAgentConnIds <- deleteFile' user (mkCIFileInfo file) True
    deleteAgentConnectionsAsync user fileAgentConnIds

markDirectCIDeleted :: (ChatMonad m, MsgDirectionI d) => User -> Contact -> ChatItem 'CTDirect d -> MessageId -> Bool -> UTCTime -> m ChatResponse
markDirectCIDeleted user ct ci@ChatItem {file} msgId byUser deletedTs = do
  cancelCIFile user file
  ci' <- withStore' $ \db -> markDirectChatItemDeleted db user ct ci msgId deletedTs
  pure $ CRChatItemDeleted user (ctItem ci) (Just $ ctItem ci') byUser False
  where
    ctItem = AChatItem SCTDirect msgDirection (DirectChat ct)

markGroupCIDeleted :: (ChatMonad m, MsgDirectionI d) => User -> GroupInfo -> ChatItem 'CTGroup d -> MessageId -> Bool -> Maybe GroupMember -> UTCTime -> m ChatResponse
markGroupCIDeleted user gInfo ci@ChatItem {file} msgId byUser byGroupMember_ deletedTs = do
  cancelCIFile user file
  ci' <- withStore' $ \db -> markGroupChatItemDeleted db user gInfo ci msgId byGroupMember_ deletedTs
  pure $ CRChatItemDeleted user (gItem ci) (Just $ gItem ci') byUser False
  where
    gItem = AChatItem SCTGroup msgDirection (GroupChat gInfo)

cancelCIFile :: (ChatMonad m, MsgDirectionI d) => User -> Maybe (CIFile d) -> m ()
cancelCIFile user file_ =
  forM_ file_ $ \file -> do
    fileAgentConnIds <- cancelFile' user (mkCIFileInfo file) True
    deleteAgentConnectionsAsync user fileAgentConnIds

createAgentConnectionAsync :: forall m c. (ChatMonad m, ConnectionModeI c) => User -> CommandFunction -> Bool -> SConnectionMode c -> SubscriptionMode -> m (CommandId, ConnId)
createAgentConnectionAsync user cmdFunction enableNtfs cMode subMode = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing cmdFunction
  connId <- withAgent $ \a -> createConnectionAsync a (aUserId user) (aCorrId cmdId) enableNtfs cMode subMode
  pure (cmdId, connId)

joinAgentConnectionAsync :: ChatMonad m => User -> Bool -> ConnectionRequestUri c -> ConnInfo -> SubscriptionMode -> m (CommandId, ConnId)
joinAgentConnectionAsync user enableNtfs cReqUri cInfo subMode = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing CFJoinConn
  connId <- withAgent $ \a -> joinConnectionAsync a (aUserId user) (aCorrId cmdId) enableNtfs cReqUri cInfo subMode
  pure (cmdId, connId)

allowAgentConnectionAsync :: (MsgEncodingI e, ChatMonad m) => User -> Connection -> ConfirmationId -> ChatMsgEvent e -> m ()
allowAgentConnectionAsync user conn@Connection {connId} confId msg = do
  cmdId <- withStore' $ \db -> createCommand db user (Just connId) CFAllowConn
  dm <- directMessage msg
  withAgent $ \a -> allowConnectionAsync a (aCorrId cmdId) (aConnId conn) confId dm
  withStore' $ \db -> updateConnectionStatus db conn ConnAccepted

agentAcceptContactAsync :: (MsgEncodingI e, ChatMonad m) => User -> Bool -> InvitationId -> ChatMsgEvent e -> SubscriptionMode -> m (CommandId, ConnId)
agentAcceptContactAsync user enableNtfs invId msg subMode = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing CFAcceptContact
  dm <- directMessage msg
  connId <- withAgent $ \a -> acceptContactAsync a (aCorrId cmdId) enableNtfs invId dm subMode
  pure (cmdId, connId)

deleteAgentConnectionAsync :: ChatMonad m => User -> ConnId -> m ()
deleteAgentConnectionAsync user acId =
  withAgent (`deleteConnectionAsync` acId) `catchChatError` (toView . CRChatError (Just user))

deleteAgentConnectionsAsync :: ChatMonad m => User -> [ConnId] -> m ()
deleteAgentConnectionsAsync _ [] = pure ()
deleteAgentConnectionsAsync user acIds =
  withAgent (`deleteConnectionsAsync` acIds) `catchChatError` (toView . CRChatError (Just user))

agentXFTPDeleteRcvFile :: ChatMonad m => RcvFileId -> FileTransferId -> m ()
agentXFTPDeleteRcvFile aFileId fileId = do
  withAgent (`xftpDeleteRcvFile` aFileId)
  withStore' $ \db -> setRcvFTAgentDeleted db fileId

agentXFTPDeleteSndFileRemote :: ChatMonad m => User -> XFTPSndFile -> FileTransferId -> m ()
agentXFTPDeleteSndFileRemote user XFTPSndFile {agentSndFileId = AgentSndFileId aFileId, privateSndFileDescr, agentSndFileDeleted} fileId =
  unless agentSndFileDeleted $
    forM_ privateSndFileDescr $ \sfdText -> do
      sd <- parseFileDescription sfdText
      withAgent $ \a -> xftpDeleteSndFileRemote a (aUserId user) aFileId sd
      withStore' $ \db -> setSndFTAgentDeleted db user fileId

userProfileToSend :: User -> Maybe Profile -> Maybe Contact -> Profile
userProfileToSend user@User {profile = p} incognitoProfile ct =
  let p' = fromMaybe (fromLocalProfile p) incognitoProfile
      userPrefs = maybe (preferences' user) (const Nothing) incognitoProfile
   in (p' :: Profile) {preferences = Just . toChatPrefs $ mergePreferences (userPreferences <$> ct) userPrefs}

createRcvFeatureItems :: forall m. ChatMonad m => User -> Contact -> Contact -> m ()
createRcvFeatureItems user ct ct' =
  createFeatureItems user ct ct' CDDirectRcv CIRcvChatFeature CIRcvChatPreference contactPreference

createSndFeatureItems :: forall m. ChatMonad m => User -> Contact -> Contact -> m ()
createSndFeatureItems user ct ct' =
  createFeatureItems user ct ct' CDDirectSnd CISndChatFeature CISndChatPreference getPref
  where
    getPref u = (userPreference u).preference

type FeatureContent a d = ChatFeature -> a -> Maybe Int -> CIContent d

createFeatureItems ::
  forall d m.
  (MsgDirectionI d, ChatMonad m) =>
  User ->
  Contact ->
  Contact ->
  (Contact -> ChatDirection 'CTDirect d) ->
  FeatureContent PrefEnabled d ->
  FeatureContent FeatureAllowed d ->
  (forall f. ContactUserPreference (FeaturePreference f) -> FeaturePreference f) ->
  m ()
createFeatureItems user Contact {mergedPreferences = cups} ct'@Contact {mergedPreferences = cups'} chatDir ciFeature ciOffer getPref =
  forM_ allChatFeatures $ \(ACF f) -> createItem f
  where
    createItem :: forall f. FeatureI f => SChatFeature f -> m ()
    createItem f
      | state /= state' = create ciFeature state'
      | prefState /= prefState' = create ciOffer prefState'
      | otherwise = pure ()
      where
        create :: FeatureContent a d -> (a, Maybe Int) -> m ()
        create ci (s, param) = createInternalChatItem user (chatDir ct') (ci f' s param) Nothing
        f' = chatFeature f
        state = featureState cup
        state' = featureState cup'
        prefState = preferenceState $ getPref cup
        prefState' = preferenceState $ getPref cup'
        cup = getContactUserPreference f cups
        cup' = getContactUserPreference f cups'

createGroupFeatureChangedItems :: (MsgDirectionI d, ChatMonad m) => User -> ChatDirection 'CTGroup d -> (GroupFeature -> GroupPreference -> Maybe Int -> CIContent d) -> GroupInfo -> GroupInfo -> m ()
createGroupFeatureChangedItems user cd ciContent GroupInfo {fullGroupPreferences = gps} GroupInfo {fullGroupPreferences = gps'} =
  forM_ allGroupFeatureItems $ \(AGF f) -> do
    let state = groupFeatureState $ getGroupPreference f gps
        pref' = getGroupPreference f gps'
        state'@(_, int') = groupFeatureState pref'
    when (state /= state') $
      createInternalChatItem user cd (ciContent (toGroupFeature f) (toGroupPreference pref') int') Nothing

sameGroupProfileInfo :: GroupProfile -> GroupProfile -> Bool
sameGroupProfileInfo p p' = p {groupPreferences = Nothing} == p' {groupPreferences = Nothing}

createInternalChatItem :: forall c d m. (ChatTypeI c, MsgDirectionI d, ChatMonad m) => User -> ChatDirection c d -> CIContent d -> Maybe UTCTime -> m ()
createInternalChatItem user cd content itemTs_ = do
  createdAt <- liftIO getCurrentTime
  let itemTs = fromMaybe createdAt itemTs_
  ciId <- withStore' $ \db -> do
    when (ciRequiresAttention content) $ updateChatTs db user cd createdAt
    createNewChatItemNoMsg db user cd content itemTs createdAt
  ci <- liftIO $ mkChatItem cd ciId content Nothing Nothing Nothing Nothing False itemTs Nothing createdAt
  toView $ CRNewChatItem user (AChatItem (chatTypeI @c) (msgDirection @d) (toChatInfo cd) ci)

getCreateActiveUser :: SQLiteStore -> Bool -> IO User
getCreateActiveUser st testView = do
  user <-
    withTransaction st getUsers >>= \case
      [] -> newUser
      users -> maybe (selectUser users) pure (find activeUser users)
  unless testView $ putStrLn $ "Current user: " <> userStr user
  pure user
  where
    newUser :: IO User
    newUser = do
      putStrLn
        "No user profiles found, it will be created now.\n\
        \Please choose your display name and your full name.\n\
        \They will be sent to your contacts when you connect.\n\
        \They are only stored on your device and you can change them later."
      loop
      where
        loop = do
          displayName <- getContactName
          withTransaction st (\db -> runExceptT $ createUserRecord db (AgentUserId 1) (profileFromName displayName) True) >>= \case
            Left SEDuplicateName -> do
              putStrLn "chosen display name is already used by another profile on this device, choose another one"
              loop
            Left e -> putStrLn ("database error " <> show e) >> exitFailure
            Right user -> pure user
    selectUser :: [User] -> IO User
    selectUser [user] = do
      withTransaction st (`setActiveUser` user.userId)
      pure user
    selectUser users = do
      putStrLn "Select user profile:"
      forM_ (zip [1 ..] users) $ \(n :: Int, user) -> putStrLn $ show n <> " - " <> userStr user
      loop
      where
        loop = do
          nStr <- getWithPrompt $ "user profile number (1 .. " <> show (length users) <> ")"
          case readMaybe nStr :: Maybe Int of
            Nothing -> putStrLn "invalid user number" >> loop
            Just n
              | n <= 0 || n > length users -> putStrLn "invalid user number" >> loop
              | otherwise -> do
                  let user = users !! (n - 1)
                  withTransaction st (`setActiveUser` user.userId)
                  pure user
    userStr :: User -> String
    userStr User {localDisplayName, profile = LocalProfile {fullName}} =
      T.unpack $ localDisplayName <> if T.null fullName || localDisplayName == fullName then "" else " (" <> fullName <> ")"
    getContactName :: IO ContactName
    getContactName = do
      displayName <- getWithPrompt "display name"
      let validName = mkValidName displayName
      if
        | null displayName -> putStrLn "display name can't be empty" >> getContactName
        | null validName -> putStrLn "display name is invalid, please choose another" >> getContactName
        | displayName /= validName -> putStrLn ("display name is invalid, you could use this one: " <> validName) >> getContactName
        | otherwise -> pure $ T.pack displayName
    getWithPrompt :: String -> IO String
    getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

withUser' :: ChatMonad m => (User -> m ChatResponse) -> m ChatResponse
withUser' action =
  asks currentUser
    >>= readTVarIO
    >>= maybe (throwChatError CENoActiveUser) run
  where
    run u = action u `catchChatError` (pure . CRChatCmdError (Just u))

withUser :: ChatMonad m => (User -> m ChatResponse) -> m ChatResponse
withUser action = withUser' $ \user ->
  ifM chatStarted (action user) (throwChatError CEChatNotStarted)

withUser_ :: ChatMonad m => m ChatResponse -> m ChatResponse
withUser_ = withUser . const

withUserId' :: ChatMonad m => UserId -> (User -> m ChatResponse) -> m ChatResponse
withUserId' userId action = withUser' $ \user -> do
  checkSameUser userId user
  action user

withUserId :: ChatMonad m => UserId -> (User -> m ChatResponse) -> m ChatResponse
withUserId userId action = withUser $ \user -> do
  checkSameUser userId user
  action user

checkSameUser :: ChatMonad m => UserId -> User -> m ()
checkSameUser userId User {userId = activeUserId} = when (userId /= activeUserId) $ throwChatError (CEDifferentActiveUser userId activeUserId)

chatStarted :: ChatMonad m => m Bool
chatStarted = fmap isJust . readTVarIO =<< asks agentAsync

waitChatStarted :: ChatMonad m => m ()
waitChatStarted = do
  agentStarted <- asks agentAsync
  atomically $ readTVar agentStarted >>= \a -> unless (isJust a) retry

chatVersionRange :: ChatMonad' m => m VersionRange
chatVersionRange = do
  ChatConfig {chatVRange} <- asks config
  pure chatVRange

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
      "/_start subscribe=" *> (StartChat <$> onOffP <* " expire=" <*> onOffP <* " xftp=" <*> onOffP),
      "/_start" $> StartChat True True True,
      "/_stop" $> APIStopChat,
      "/_app activate restore=" *> (APIActivateChat <$> onOffP),
      "/_app activate" $> APIActivateChat True,
      "/_app suspend " *> (APISuspendChat <$> A.decimal),
      "/_resubscribe all" $> ResubscribeAllConnections,
      "/_temp_folder " *> (SetTempFolder <$> filePath),
      ("/_files_folder " <|> "/files_folder ") *> (SetFilesFolder <$> filePath),
      "/remote_hosts_folder " *> (SetRemoteHostsFolder <$> filePath),
      "/_xftp " *> (APISetXFTPConfig <$> ("on " *> (Just <$> jsonP) <|> ("off" $> Nothing))),
      "/xftp " *> (APISetXFTPConfig <$> ("on" *> (Just <$> xftpCfgP) <|> ("off" $> Nothing))),
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
      "/_send " *> (APISendMessage <$> chatRefP <*> liveMessageP <*> sendMessageTTLP <*> (" json " *> jsonP <|> " text " *> (ComposedMessage Nothing Nothing <$> mcTextP))),
      "/_update item " *> (APIUpdateChatItem <$> chatRefP <* A.space <*> A.decimal <*> liveMessageP <* A.space <*> msgContentP),
      "/_delete item " *> (APIDeleteChatItem <$> chatRefP <* A.space <*> A.decimal <* A.space <*> ciDeleteMode),
      "/_delete member item #" *> (APIDeleteMemberChatItem <$> A.decimal <* A.space <*> A.decimal <* A.space <*> A.decimal),
      "/_reaction " *> (APIChatItemReaction <$> chatRefP <* A.space <*> A.decimal <* A.space <*> onOffP <* A.space <*> jsonP),
      "/_read user " *> (APIUserRead <$> A.decimal),
      "/read user" $> UserRead,
      "/_read chat " *> (APIChatRead <$> chatRefP <*> optional (A.space *> ((,) <$> ("from=" *> A.decimal) <* A.space <*> ("to=" *> A.decimal)))),
      "/_unread chat " *> (APIChatUnread <$> chatRefP <* A.space <*> onOffP),
      "/_delete " *> (APIDeleteChat <$> chatRefP <*> (A.space *> "notify=" *> onOffP <|> pure True)),
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
      "/_parse " *> (APIParseMarkdown . safeDecodeUtf8 <$> A.takeByteString),
      "/_ntf get" $> APIGetNtfToken,
      "/_ntf register " *> (APIRegisterToken <$> strP_ <*> strP),
      "/_ntf verify " *> (APIVerifyToken <$> strP <* A.space <*> strP <* A.space <*> strP),
      "/_ntf delete " *> (APIDeleteToken <$> strP),
      "/_ntf message " *> (APIGetNtfMessage <$> strP <* A.space <*> strP),
      "/_add #" *> (APIAddMember <$> A.decimal <* A.space <*> A.decimal <*> memberRole),
      "/_join #" *> (APIJoinGroup <$> A.decimal),
      "/_member role #" *> (APIMemberRole <$> A.decimal <* A.space <*> A.decimal <*> memberRole),
      "/_remove #" *> (APIRemoveMember <$> A.decimal <* A.space <*> A.decimal),
      "/_leave #" *> (APILeaveGroup <$> A.decimal),
      "/_members #" *> (APIListMembers <$> A.decimal),
      "/_server test " *> (APITestProtoServer <$> A.decimal <* A.space <*> strP),
      "/smp test " *> (TestProtoServer . AProtoServerWithAuth SPSMP <$> strP),
      "/xftp test " *> (TestProtoServer . AProtoServerWithAuth SPXFTP <$> strP),
      "/_servers " *> (APISetUserProtoServers <$> A.decimal <* A.space <*> srvCfgP),
      "/smp " *> (SetUserProtoServers . APSC SPSMP . ProtoServersConfig . map toServerCfg <$> protocolServersP),
      "/smp default" $> SetUserProtoServers (APSC SPSMP $ ProtoServersConfig []),
      "/xftp " *> (SetUserProtoServers . APSC SPXFTP . ProtoServersConfig . map toServerCfg <$> protocolServersP),
      "/xftp default" $> SetUserProtoServers (APSC SPXFTP $ ProtoServersConfig []),
      "/_servers " *> (APIGetUserProtoServers <$> A.decimal <* A.space <*> strP),
      "/smp" $> GetUserProtoServers (AProtocolType SPSMP),
      "/xftp" $> GetUserProtoServers (AProtocolType SPXFTP),
      "/_ttl " *> (APISetChatItemTTL <$> A.decimal <* A.space <*> ciTTLDecimal),
      "/ttl " *> (SetChatItemTTL <$> ciTTL),
      "/_ttl " *> (APIGetChatItemTTL <$> A.decimal),
      "/ttl" $> GetChatItemTTL,
      "/_network " *> (APISetNetworkConfig <$> jsonP),
      ("/network " <|> "/net ") *> (APISetNetworkConfig <$> netCfgP),
      ("/network" <|> "/net") $> APIGetNetworkConfig,
      "/reconnect" $> ReconnectAllServers,
      "/_settings " *> (APISetChatSettings <$> chatRefP <* A.space <*> jsonP),
      "/_member settings #" *> (APISetMemberSettings <$> A.decimal <* A.space <*> A.decimal <* A.space <*> jsonP),
      "/_info #" *> (APIGroupMemberInfo <$> A.decimal <* A.space <*> A.decimal),
      "/_info #" *> (APIGroupInfo <$> A.decimal),
      "/_info @" *> (APIContactInfo <$> A.decimal),
      ("/info #" <|> "/i #") *> (GroupMemberInfo <$> displayName <* A.space <* char_ '@' <*> displayName),
      ("/info #" <|> "/i #") *> (ShowGroupInfo <$> displayName),
      ("/info " <|> "/i ") *> char_ '@' *> (ContactInfo <$> displayName),
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
      ("/remove " <|> "/rm ") *> char_ '#' *> (RemoveMember <$> displayName <* A.space <* char_ '@' <*> displayName),
      ("/leave " <|> "/l ") *> char_ '#' *> (LeaveGroup <$> displayName),
      ("/delete #" <|> "/d #") *> (DeleteGroup <$> displayName),
      ("/delete " <|> "/d ") *> char_ '@' *> (DeleteContact <$> displayName),
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
      ("/connect" <|> "/c") *> (Connect <$> incognitoP <* A.space <*> ((Just <$> strP) <|> A.takeTill isSpace $> Nothing)),
      ("/connect" <|> "/c") *> (AddContact <$> incognitoP),
      SendMessage <$> chatNameP <* A.space <*> msgTextP,
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
      ("/freceive " <|> "/fr ") *> (ReceiveFile <$> A.decimal <*> optional (" encrypt=" *> onOffP) <*> optional (" inline=" *> onOffP) <*> optional (A.space *> filePath)),
      "/_set_file_to_receive " *> (SetFileToReceive <$> A.decimal <*> optional (" encrypt=" *> onOffP)),
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
      "/set voice #" *> (SetGroupFeature (AGF SGFVoice) <$> displayName <*> (A.space *> strP)),
      "/set voice @" *> (SetContactFeature (ACF SCFVoice) <$> displayName <*> optional (A.space *> strP)),
      "/set voice " *> (SetUserFeature (ACF SCFVoice) <$> strP),
      "/set files #" *> (SetGroupFeature (AGF SGFFiles) <$> displayName <*> (A.space *> strP)),
      "/set history #" *> (SetGroupFeature (AGF SGFHistory) <$> displayName <*> (A.space *> strP)),
      "/set calls @" *> (SetContactFeature (ACF SCFCalls) <$> displayName <*> optional (A.space *> strP)),
      "/set calls " *> (SetUserFeature (ACF SCFCalls) <$> strP),
      "/set delete #" *> (SetGroupFeature (AGF SGFFullDelete) <$> displayName <*> (A.space *> strP)),
      "/set delete @" *> (SetContactFeature (ACF SCFFullDelete) <$> displayName <*> optional (A.space *> strP)),
      "/set delete " *> (SetUserFeature (ACF SCFFullDelete) <$> strP),
      "/set direct #" *> (SetGroupFeature (AGF SGFDirectMessages) <$> displayName <*> (A.space *> strP)),
      "/set disappear #" *> (SetGroupTimedMessages <$> displayName <*> (A.space *> timedTTLOnOffP)),
      "/set disappear @" *> (SetContactTimedMessages <$> displayName <*> optional (A.space *> timedMessagesEnabledP)),
      "/set disappear " *> (SetUserTimedMessages <$> (("yes" $> True) <|> ("no" $> False))),
      ("/incognito" <* optional (A.space *> onOffP)) $> ChatHelp HSIncognito,
      "/set device name " *> (SetLocalDeviceName <$> textP),
      "/list remote hosts" $> ListRemoteHosts,
      "/switch remote host " *> (SwitchRemoteHost <$> ("local" $> Nothing <|> (Just <$> A.decimal))),
      "/start remote host " *> (StartRemoteHost <$> ("new" $> Nothing <|> (Just <$> ((,) <$> A.decimal <*> (" multicast=" *> onOffP <|> pure False)))) <*> optional (A.space *> rcCtrlAddressP) <*> optional (" port=" *> A.decimal)),
      "/stop remote host " *> (StopRemoteHost <$> ("new" $> RHNew <|> RHId <$> A.decimal)),
      "/delete remote host " *> (DeleteRemoteHost <$> A.decimal),
      "/store remote file " *> (StoreRemoteFile <$> A.decimal <*> optional (" encrypt=" *> onOffP) <* A.space <*> filePath),
      "/get remote file " *> (GetRemoteFile <$> A.decimal <* A.space <*> jsonP),
      "/connect remote ctrl " *> (ConnectRemoteCtrl <$> strP),
      "/find remote ctrl" $> FindKnownRemoteCtrl,
      "/confirm remote ctrl " *> (ConfirmRemoteCtrl <$> A.decimal),
      "/verify remote ctrl " *> (VerifyRemoteCtrlSession <$> textP),
      "/list remote ctrls" $> ListRemoteCtrls,
      "/stop remote ctrl" $> StopRemoteCtrl,
      "/delete remote ctrl " *> (DeleteRemoteCtrl <$> A.decimal),
      ("/quit" <|> "/q" <|> "/exit") $> QuitChat,
      ("/version" <|> "/v") $> ShowVersion,
      "/debug locks" $> DebugLocks,
      "/get stats" $> GetAgentStats,
      "/reset stats" $> ResetAgentStats,
      "/get subs" $> GetAgentSubs,
      "/get subs details" $> GetAgentSubsDetails
    ]
  where
    choice = A.choice . map (\p -> p <* A.takeWhile (== ' ') <* A.endOfInput)
    incognitoP = (A.space *> ("incognito" <|> "i")) $> True <|> pure False
    incognitoOnOffP = (A.space *> "incognito=" *> onOffP) <|> pure False
    imagePrefix = (<>) <$> "data:" <*> ("image/png;base64," <|> "image/jpg;base64,")
    imageP = safeDecodeUtf8 <$> ((<>) <$> imagePrefix <*> (B64.encode <$> base64P))
    chatTypeP = A.char '@' $> CTDirect <|> A.char '#' $> CTGroup <|> A.char ':' $> CTContactConnection
    chatPaginationP =
      (CPLast <$ "count=" <*> A.decimal)
        <|> (CPAfter <$ "after=" <*> A.decimal <* A.space <* "count=" <*> A.decimal)
        <|> (CPBefore <$ "before=" <*> A.decimal <* A.space <* "count=" <*> A.decimal)
    paginationByTimeP =
      (PTLast <$ "count=" <*> A.decimal)
        <|> (PTAfter <$ "after=" <*> strP <* A.space <* "count=" <*> A.decimal)
        <|> (PTBefore <$ "before=" <*> strP <* A.space <* "count=" <*> A.decimal)
    mcTextP = MCText . safeDecodeUtf8 <$> A.takeByteString
    msgContentP = "text " *> mcTextP <|> "json " *> jsonP
    ciDeleteMode = "broadcast" $> CIDMBroadcast <|> "internal" $> CIDMInternal
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
    liveMessageP = " live=" *> onOffP <|> pure False
    sendMessageTTLP = " ttl=" *> ((Just <$> A.decimal) <|> ("default" $> Nothing)) <|> pure Nothing
    receiptSettings = do
      enable <- onOffP
      clearOverrides <- (" clear_overrides=" *> onOffP) <|> pure False
      pure UserMsgReceiptSettings {enable, clearOverrides}
    onOffP = ("on" $> True) <|> ("off" $> False)
    profileNames = (,) <$> displayName <*> fullNameP
    newUserP = do
      sameServers <- "same_servers=" *> onOffP <* A.space <|> pure False
      (cName, fullName) <- profileNames
      let profile = Just Profile {displayName = cName, fullName, image = Nothing, contactLink = Nothing, preferences = Nothing}
      pure NewUser {profile, sameServers, pastTimestamp = False}
    jsonP :: J.FromJSON a => Parser a
    jsonP = J.eitherDecodeStrict' <$?> A.takeByteString
    groupProfile = do
      (gName, fullName) <- profileNames
      let groupPreferences =
            Just
              (emptyGroupPrefs :: GroupPreferences)
                { directMessages = Just DirectMessagesGroupPreference {enable = FEOn},
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
          " member" $> GRMember,
          " observer" $> GRObserver
        ]
    chatNameP = ChatName <$> chatTypeP <*> displayName
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
    netCfgP = do
      socksProxy <- "socks=" *> ("off" $> Nothing <|> "on" $> Just defaultSocksProxy <|> Just <$> strP)
      t_ <- optional $ " timeout=" *> A.decimal
      logErrors <- " log=" *> onOffP <|> pure False
      let tcpTimeout = 1000000 * fromMaybe (maybe 5 (const 10) socksProxy) t_
      pure $ fullNetworkConfig socksProxy tcpTimeout logErrors
    xftpCfgP = XFTPFileConfig <$> (" size=" *> fileSizeP <|> pure 0)
    fileSizeP =
      A.choice
        [ gb <$> A.decimal <* "gb",
          mb <$> A.decimal <* "mb",
          kb <$> A.decimal <* "kb",
          A.decimal
        ]
    dbKeyP = nonEmptyKey <$?> strP
    nonEmptyKey k@(DBEncryptionKey s) = if BA.null s then Left "empty key" else Right k
    dbEncryptionConfig currentKey newKey = DBEncryptionConfig {currentKey, newKey, keepKey = Just False}
    autoAcceptP =
      ifM
        onOffP
        (Just <$> (AutoAccept <$> (" incognito=" *> onOffP <|> pure False) <*> optional (A.space *> msgContentP)))
        (pure Nothing)
    srvCfgP = strP >>= \case AProtocolType p -> APSC p <$> (A.space *> jsonP)
    toServerCfg server = ServerCfg {server, preset = False, tested = Nothing, enabled = True}
    rcCtrlAddressP = RCCtrlAddress <$> ("addr=" *> strP) <*> (" iface=" *> text1P)
    text1P = safeDecodeUtf8 <$> A.takeTill (== ' ')
    char_ = optional . A.char

adminContactReq :: ConnReqContact
adminContactReq =
  either error id $ strDecode "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

simplexContactProfile :: Profile
simplexContactProfile =
  Profile
    { displayName = "SimpleX Chat team",
      fullName = "",
      image = Just (ImageData "data:image/jpg;base64,/9j/4AAQSkZJRgABAgAAAQABAAD/2wBDAAUDBAQEAwUEBAQFBQUGBwwIBwcHBw8KCwkMEQ8SEhEPERATFhwXExQaFRARGCEYGhwdHx8fExciJCIeJBweHx7/2wBDAQUFBQcGBw4ICA4eFBEUHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh7/wAARCAETARMDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD7LooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiivP/iF4yFvv0rSpAZek0yn7v+yPeunC4WpiqihBf8A8rOc5w2UYZ4jEPTourfZDvH3jL7MW03SpR53SWUfw+w96veA/F0erRLY3zKl6owD2k/8Ar15EWLEljknqadDK8MqyxMUdTlWB5Br66WS0Hh/ZLfv1ufiNLj7Mo5m8ZJ3g9OTpy+Xn5/pofRdFcd4B8XR6tEthfMEvVHyk9JB/jXY18fiMPUw9R06i1P3PK80w2aYaOIw8rxf3p9n5hRRRWB6AUUVDe3UFlavc3MixxIMsxppNuyJnOMIuUnZIL26gsrV7m5kWOJBlmNeU+I/Gd9e6sk1hI8FvA2Y1z973NVPGnimfXLoxRFo7JD8if3vc1zefevr8syiNKPtKyvJ9Ox+F8Ycb1cdU+rYCTjTi/iWjk1+nbue3eEPEdtrtoMER3SD95Hn9R7Vu18+6bf3On3kd1aSmOVDkEd/Y17J4P8SW2vWY6R3aD97F/Ue1eVmmVPDP2lP4fyPtODeMoZrBYXFO1Zf+Tf8AB7r5o3qKKK8Q/QgooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAqavbTXmmz20Fw1vJIhVZB1FeDa3p15pWoSWl6hWQHr2YeoNfQlY3izw9Z6/YGGZQky8xSgcqf8K9jKcyWEnyzXuv8D4njLhZ51RVSi7VYLRdGu3k+z+88HzRuq1rWmXmkX8lnexFHU8Hsw9RVLNfcxlGcVKLumfgFahUozdOorSWjT6E0M0kMqyxOyOpyrKcEGvXPAPjCPVolsb9wl6owGPAkH+NeO5p8M0kMqyxOyOpyrA4INcWPy+njKfLLfoz2+HuIMTkmI9pT1i/ij0a/wA+zPpGiuM+H/jCPV4lsL91S+QfKTwJR/jXW3t1BZWslzcyLHFGMsxNfB4jC1aFX2U1r+fof0Rl2bYXMMKsVRl7vXy7p9rBfXVvZWr3NzKscSDLMTXjnjbxVPrtyYoiY7JD8if3vc0zxv4ruNeujFEWjsoz8if3vc1zOa+synKFh0qtVe9+X/BPxvjLjKWZSeEwjtSW7/m/4H5kmaM1HmlB54r3bH51YkzXo3wz8MXMc0es3ZeED/VR5wW9z7VB8O/BpnMerarEREDuhhb+L3Pt7V6cAAAAAAOgFfL5xmqs6FH5v9D9a4H4MlzQzHGq1tYR/KT/AEXzCiiivlj9hCiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAxfFvh208QWBhmASdRmKUdVP+FeH63pl5pGoSWV5EUdTwezD1HtX0VWL4t8O2fiHTzBONk6g+TKByp/wr28pzZ4WXs6msH+B8NxdwhTzeDxGHVqy/8m8n59n954FmjNW9b0y80fUHsr2MpIp4PZh6iqWfevuYyjOKlF3TPwetQnRm6dRWktGmSwzSQyrLE7I6nKsDgg1teIPFOqa3a29vdy4jiUAheN7f3jWBmjNROhTnJTkrtbGtLF4ijSnRpzajPddHbuP3e9Lmo80ua0scth+a9E+HXgw3Hl6tqsZEX3oYmH3vc+1J8OPBZnKavq0eIhzDCw+9/tH29q9SAAAAGAOgr5bOM35b0KD16v8ARH6twXwXz8uPx0dN4xfXzf6IFAUAAAAdBRRRXyZ+wBRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFB4GTXyj+1p+0ONJjufA3ga6DX7qU1DUY24gB4McZH8Xqe38tqFCdefLETaSufQ3h/4geEde8Uah4a0rWra51Ow/wBfCrD8ceuO+OldRX5I+GfEWseG/ENvr2j30ttqFvJ5iSqxyT3z6g96/RH9nD41aT8U9AWGcx2fiK1QC7tC33/+mieqn07V14zL3QXNHVEQnc9dooorzjQKKKKACiis7xHrel+HdGudY1m8is7K2QvLLI2AAP600m3ZAYfxUg8Pr4VutT1+7isYbSMuLp/4Pb3z6V8++HNd0zxDpq6hpVys8DHGRwVPoR2NeIftJ/G7VPifrbWVk8lp4btZD9mtwcGU/wDPR/c9h2rgfh34z1LwdrAurV2ktZCBcW5PyyD/AB9DX2WTyqYWny1Ho+nY+C4t4Wp5tF16CtVX/k3k/Ps/vPr/ADRmsjwx4g07xFpMWpaZOJInHI/iQ9wR61qbq+mVmro/D6tCdGbp1FZrdEma6/4XafpWoa7jUpV3oA0MLdJD/ntXG5p8E0kMqyxOyOhyrKcEGsMTRlWpShGVm+p1ZbiYYPFQr1IKai72fU+nFAUAKAAOABRXEfDnxpFrMK6fqDhL9BhSeko9frXb1+a4rDVMNUdOotT+k8szLD5lh44jDu8X968n5hRRRXOegFFFFABUGoXlvYWkl1dSrHFGMliaL+7t7C0kuruVYoYxlmNeI+OvFtx4huzHFuisYz+7jz97/aNenluW1MbU00it2fM8S8SUMkoXetR/DH9X5fmeteF/E+m+IFkFoxSWMnMb9cev0rbr5t0vULrTb6K8s5TFNGcgj+R9q9w8E+KbXxDYjlY7xB+9i/qPaurNsneE/eUtYfkeTwlxjHNV9XxVo1V90vTz8vmjoqKKK8I+8CiiigAooooAKKKKACiiigD5V/a8+P0mgvdeAvCUskepFdl9eDjyQR9xPfHeviiR3lkaSR2d2OWZjkk+tfoj+058CtP+Jektq2jxRWnie2T91KMKLlR/yzf+h7V+fOuaVqGiarcaXqtpLaXls5jlikXDKRX0mWSpOlaG/U56l76lKtPwtr+reGNetdb0S8ls761cPHJG2D9D6g9MVmUV6TSasyD9Jf2cfjXpPxR0MW9w0dp4gtkAubYnHmf7aeo/lXr1fkh4W1/V/DGuW2taHey2d9bOHjkjP6H1HtX6Jfs5fGvR/inoQgmeOz8RWqD7XaE439vMT1U+navnMfgHRfPD4fyN4Tvoz12iis7xJremeHdEutZ1i7jtLK1jLyyucAAf1rzUm3ZGgeJNb0vw7otzrOs3kVpZWyF5ZZDgAD+Z9q/PL9pP436r8UNZaxs2ks/Dlq5+z24ODMf77+p9B2o/aU+N2p/FDXDZ2LS2fhy1ci3t84Mx/wCej+/oO1eNV9DgMAqS55/F+RhOd9EFFFABJwBkmvUMzqPh34y1Lwjq63FszSWshAntyeHHt719Z2EstzpVlqD2txbR3kCzxLPGUbawyODXK/slfs8nUpbXx144tGFkhElhp8q4849pHB/h9B3r608X+GLDxBpX2WRFiljX9xIowUPYfT2rGnnkMPWVJ6x6vt/XU+P4o4SjmtN4igrVV/5N5Pz7P7z56zRmrmvaVe6LqMljexMkiHg9mHqKoZr6uEozipRd0z8Rq0J0ZunUVmtGmTwTSQTJNC7JIhyrKcEGvZvhz41j1mJdP1GRUv0GFY8CX/69eJZqSCaWCVZYXZHU5VlOCDXDmGXU8bT5ZaPo+x7WQZ9iMlxHtKesX8UejX+fZn1FRXDfDbxtHrUKadqDqmoIuAx4EoHf613NfnWKwtTC1HTqKzR/QGW5lh8yw8cRh3eL+9Ps/MKr6heW1hZyXd3KsUUYyzGjUby20+zku7yZYoY13MzGvDPHvi+48RXpjiZorCM/u4/73+0feuvLMsqY6pZaRW7/AK6nlcScR0MloXetR/DH9X5D/Hni648Q3nlxlo7GM/u48/e9zXL7qZmjNfodDDwoU1TpqyR+AY7G18dXlXryvJ/19w/dVvSdRutMvo7yzlaOVDkY7+xqkDmvTPhn4HMxj1jV4v3Y+aCFh97/AGjWGPxNHDUXKrt27+R15JlWLzHFxp4XSS1v/L53PQ/C+oXGqaJb3t1bNbyyLkoe/v8AQ1p0AAAAAADoBRX5nUkpSbirLsf0lh6c6dKMJy5mkrvv5hRRRUGwUUUUAFFFFABRRRQAV4d+038CdO+JWkyavo8cdp4mtkzHIBhbkD+B/f0Ne40VpSqypSUovUTV9GfkTruk6joer3Ok6taS2d7ayGOaGVdrKRVKv0T/AGnfgXp/xK0h9Y0iOO18TWqZikAwLkD+B/6Gvz51zStQ0TVbjS9UtZbW8tnKSxSLgqRX1GExccRG636o55RcSlWp4V1/VvDGvWut6JeSWl9bOGjkQ4/A+oPpWXRXU0mrMk/RP4LftDeFvF3ge41HxDfW+lappkG+/idsBwP40HfJ7V8o/tJ/G/VPifrbWVk8tn4btn/0e2zgykfxv6n0HavGwSM4JGeuO9JXFRwFKlUc18vIpzbVgoooAJIAGSa7SQr6x/ZM/Z4k1J7Xxz44tClkMSWFhIuDL3Ejg/w+g70fsmfs8NqMtt448c2eLJCJLCwlX/WnqHcH+H0HevtFFVECIoVVGAAMACvFx+PtenTfqzWEOrEjRI41jjUIigBVAwAPSnUUV4ZsYXjLwzZeJNOaCcBLhQfJmA5U/wCFeBa/pV7ompSWF9GUkToccMOxHtX01WF4z8M2XiXTTBOAk6AmGYDlD/hXvZPnEsHL2dTWD/A+K4r4UhmsHXoK1Zf+TeT8+z+8+c80Zq5r2k3ui6jJY30ZSRTwezD1FUM1+gQlGcVKLumfiFWjOjN06is1umTwTSQTJNE7JIh3KynBBr2PwL8QrO701odbnSC5t0yZCcCUD+teK5pd1cWPy2ljoctTdbPqetkme4rJ6rqUHdPdPZ/8Mdb4/wDGFz4ivDFGxisIz+7j/ve5rls1HuozXTQw1PD01TpqyR5+OxlfHV5V68ryf9fcSZozTAa9P+GHgQzmPWdZhIjHzQQMPvf7R9qxxuMpYOk6lR/8E6MpyfEZriFQoL1fRLux/wAMvApmMesazFiP70EDfxf7R9vavWFAUAAAAcACgAAAAAAdBRX5xjsdVxtXnn8l2P3/ACXJcNlGHVGivV9W/wCugUUUVxHrhRRRQAUUUUAFFFFABRRRQAUUUUAFeH/tOfArT/iXpUmsaSsVp4mto/3UuMLcgDhH/oe1e4Vn+I9a0zw7otzrGsXkVpZWyF5ZZGwAB/WtaNSdOalDcTSa1PyZ1zStQ0TVrnStVtZLS8tnMcsUgwVIqlXp/wC0l8S7T4nePn1aw0q3srO3XyYJBGBNOoPDSHv7DtXmFfXU5SlBOSszlYUUUVYAAScDk19Zfsmfs7vqLW3jjx1ZFLMESafYSjmXuJHHZfQd6+VtLvJtO1K2v7cRtLbyrKgkQOpKnIyp4I46Gv0b/Zv+NOjfFDw+lrIIrDX7RAtzZ8AMMffj9V9u1efmVSrCn7m3Vl00m9T16NEjjWONVRFGFUDAA9KWiivmToCiiigAooooAwfGnhiy8S6cYJwEuEH7mYDlT/hXz7r+k32h6lJYahFskQ8Hsw9QfSvpjUr2106ykvLyZYYYxlmY18+/EXxa/ijU1aOMRWkGRCCBuPuT/Svr+GK2KcnTSvT/ACfl/kfmPiBhMvUI1m7Vn0XVefp0fy9Oa3UbqZmjNfa2PynlJM+9AOajzTo5GjkV0YqynIPoaVg5T1P4XeA/P8vWdaiIj+9BAw+9/tH29q9dAAAAAAHQVwPwx8dQ63Ammai6R6hGuFJ4Ew9vf2rvq/Ms5qYmeJaxGjWy6W8j+gOFcPl9LAReBd0931b8+3oFFFFeSfSBRRRQAUUUUAFFFFABRRRQAUUUUAFFFZ3iTW9L8OaJdazrN5HaWNqheWWQ4AH+NNJt2QB4l1vTPDmiXWs6xdx2llaxl5ZHOAAO3ufavzx/aT+N2qfFDWzZWbSWfhy2ci3tg2DKf77+p9B2pf2lfjdqfxQ1trGxeW08N2z/AOj2+cGYj/lo/v6DtXjVfQ4DAKkuefxfkYTnfRBRRQAScAZNeoZhRXv3w2/Zh8V+Lfh7deJprgadcvHv02zlT5rgdcsf4Qe1eHa5pWoaJq1zpWq2ktpeW0hjlikXDKwrOFanUk4xd2htNFKtTwrr+reGNdtta0S8ltL22cPHIhx07H1HtWXRWjSasxH6S/s4/GrSfijoYtp3jtfENqg+1WpON4/vp6j27V69X5IeFfEGr+F9etdc0O9ks7+1cPHKh/QjuD3Ffoj+zl8bNI+KWhLbztFZ+IraMfa7TON+Osieqn07V85j8A6L54fD+RvCd9GevUUUV5hoFVtTvrXTbGW9vJligiXczNRqd9aabYy3t7MsMEQyzMa+ffiN42uvE96YoS0OmxH91F3b/ab3r1spympmFSy0it3+i8z57iDiCjlFG71qPZfq/Id8RPGl14lvTFEzRafGf3cf97/aNclmmZozX6Xh8NTw1NU6askfheNxdbG1pV68ryY/NGTTM16R4J+GVxrGkSX+pSSWfmJ/oq45J7MR6Vni8ZRwkOes7I1y7K8TmNX2WHjd7/0zzvJozV3xDpF7oepyWF/EUkQ8HHDD1FZ+feuiEozipRd0zjq0Z0puE1ZrdE0E8sEyTQu0ciHKspwQa9z+GHjuLXIU0zUpFTUEXCseBKB/WvBs1JBPLBMk0LmORCGVlOCDXn5lllLH0uWWjWz7HsZFnlfJ6/tKesXuu6/z7M+tKK4D4X+PItdhTTNSdY9SQYVicCYDuPf2rv6/M8XhKuEqulVVmj92y7MaGYUFXoO6f4Ps/MKKKK5juCiiigAooooAKKKKACiig9KAM7xLrmleG9EudZ1q8jtLG2QvLK5wAPQep9q/PH9pP43ap8T9beyspJbTw3bSH7NbZx5pH8b+p9u1bH7YPxL8XeJPG114V1G0udH0jT5SIrNuDOR0kbs2e3pXgdfRZfgVTSqT3/IwnO+iCiigAkgAZJr1DMK+s/2TP2d31Brbxz46tNtmMSafp8i8y9/MkB6L0wO9J+yb+zwdSe28b+ObLFmpEljYSr/rT1DuP7voO9faCKqIERQqqMAAYAFeLj8fa9Om/VmsIdWEaJGixooVFGFUDAA9K8Q/ac+BWnfErSZNY0mOO08T2yZilAwtyAPuP/Q9q9worx6VWVKSlF6mrSasfkTrmlahomrXOlaray2l7bSGOaKRcMrCqVfon+098C7D4l6U+s6Skdr4mtY/3UmMC5UdI29/Q1+fOt6XqGi6rcaVqlrJa3ls5SWKQYKkV9RhMXHERut+qOeUeUpVqeFfEGreGNdttb0W7ktb22cNG6HH4H1FZdFdTSasyT9Jf2cPjVpXxR0Fbe4eK18Q2qD7Va7sbx/z0T1H8q9V1O+tdNsZb29mWGCJdzMxr8ovAOoeIdK8W2GoeF5podVhlDQtEefcH2PevsbxP4417xTp1jDq3lQGKFPOigJ2NLj5m59849K4KHD0sTX9x2h18vJHj55xDSyqhd61Hsv1fkaXxG8bXXie9MURaLTo2/dR5+9/tH3rkM1HmjNffYfC08NTVOmrJH4ljMXWxtaVau7yZJmgHmmAmvWfhN8PTceVrmuQkRDDW9uw+9/tN7Vjj8dSwNJ1ar9F3OjK8pr5nXVGivV9Eu7H/Cf4emcx63rkJEfDW9u4+9/tMPT2r2RQFAVQABwAKAAAAAAB0Aor8uzDMKuOq+0qfJdj9zyjKMPlVBUaK9X1bOf8b+FbHxRppt7gCO4UfuZwOUP9R7V86+IdHv8AQtTk0/UIikqHg9mHqD6V9VVz3jnwrY+KNMNvcKEuEBME2OUP+FenkmdywUvZVdab/A8PijheGZw9vQVqq/8AJvJ+fZnzLuo3Ve8Q6Pf6FqclhqERjkQ8Hsw9Qazs1+jwlGpFSi7pn4xVozpTcJqzW6J7eeSCZJoZGjkQhlZTgg17t8LvHsWuQppmpOseooMKxPEw/wAa8DzV3Q7fULvVIIdLWQ3ZcGMx8EH1z2rzs1y2jjaLVTRrZ9v+AezkGcYnK8SpUVzKWjj3/wCD2PrCiqOgx38Oj20eqTJNeLGBK6jAJq9X5VOPLJq9z98pyc4KTVr9H0CiiipLCiiigAooooAKKKKAPK/2hfg3o/xT8PFdsVprlupNnebec/3W9VNfnR4y8Naz4R8RXWg69ZvaXts5V1YcEdmB7g9jX6115V+0P8GtF+Knh05SO0161UmzvQuD/uP6qf0r08DjnRfJP4fyM5wvqj80RycCvrP9kz9ndtRNr458dWTLaAiTT9PlXBl9JJB/d7gd+tXv2bv2Y7yz19vEHxFs1VbKYi1sCQwlZTw7f7PcDvX2CiLGioihVUYAAwAK6cfmGns6T9WTCHVhGiRoqRqFRRgKBgAUtFFeGbBRRRQAV4h+038CtP8AiZpTatpCQ2fia2jPlS4wtyo52P8A0Pavb6K0pVZUpKUXqJq+jPyJ1zStQ0TVrnStVtJbS9tnMcsUgwVIqPS7C61O+isrKFpZ5W2qor9AP2r/AIM6J448OzeJLV7fTtesoyRO3yrcqP4H9/Q14F8OvBlp4XsvMkCTajKP3suM7f8AZX0H86+1yiDzFcy0S3Pms+zqllNLXWb2X6vyH/DnwZaeF7EPIEm1CUDzZcfd/wBke1dfmo80ua+0pUY0oqMVofjWLxNXF1XWrO8mSZozUea9N+B/hTTdau5NUv5opvsrjbak8k9mYelc+OxcMHQlWqbI1y3LqmYYmOHpbvuafwj+HhnMWva5DiMENb27D73ozD09q9oAAAAAAHQCkUBVCqAAOABS1+U5jmNXH1XUqfJdj9yyjKKGV0FRor1fVsKKKK4D1AooooA57xz4UsPFOmG3uFEdwgJgnA5Q/wBR7V84eI9Gv9A1SXT9RhMcqHg/wuOxB7ivrCud8d+E7DxTpZt51CXKDMEwHKn/AAr6LI88lgpeyq603+Hmv1Pj+J+GIZnB16KtVX/k3k/Psz5p0uxu9Tv4rGxheaeVtqIoyTX0T8OPBNp4XsRJKFm1GQfvZf7v+yvtR8OfBFn4UtDIxW41CUfvJsdB/dX0FdfWue568W3RoP3Pz/4BhwvwtHL0sTiVeq9l/L/wQooor5g+3CiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKrarf2ml2E19fTpBbwrud2OAKTVdQtNLsJb6+mWGCJcszGvm34nePLzxXfmGEtDpkTfuos/f/wBpvevZyfJ6uZVbLSC3f6LzPBz3PaOVUbvWb2X6vyH/ABM8d3fiq/MULPDpsR/dRdN3+03vXF5pm6jdX6phsLTw1JUqSskfjGLxVbGVnWrO8mSZ96M0wGnSq8UhjkRkdeCrDBFb2OXlFzWn4b1y/wBA1SPUNPmMciHkdmHoR6Vk7hS596ipTjUi4zV0y6c50pqcHZrZn1X4C8W2HizShc27BLmMATwZ5Q/4V0dfIfhvXL/w/qseo6dMY5U6js47gj0r6Y8BeLtP8WaUtzbER3KAefATyh/qPevzPPshlgJe1pa03+Hk/wBGfr/DfEkcygqNbSqv/JvNefdHSUUUV80fWhRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFFFFABRRRQAUUUUAFVtVv7TS7CW+vp1ht4l3O7HpSatqNnpWny319OsMES7mZjXzP8UfH154tv8AyYWeDS4WPlQ5xvP95vU/yr2smyarmVWy0gt3+i8zws8zylldK71m9l+r8h/xP8eXfiy/MUJaHTIm/cxZ5b/ab3ris0zNGa/V8NhaWFpKlSVkj8bxeKrYuq61Z3kx+aX2pmTXsnwc+GrXBh8Qa/CViB3W9sw5b0Zh6e1YZhj6OAourVfourfY3y3LK+Y11Ror1fRLux3wc+GxuPK1/X4SIgQ1tbuPvf7TD09BXT/Fv4dQ6/bPqukxpFqca5KgYE4Hb6+9ekKAqhVAAHAApa/L62fYupi1ilKzWy6W7f5n63R4bwVPBPBuN0931v3/AMj4wuIZred4J42jlQlWVhgg0zNfRHxc+HUXiCB9W0mNI9TRcso4EwH9a+eLiKW2neCeNo5UO1kYYIPpX6TlOa0cypc8NJLddv8AgH5XnOS1srrck9YvZ9/+CJmtPw1rl/4f1WLUdPmMcqHkZ4Yeh9qys0Zr0qlONSLhNXTPKpznSmpwdmtmfWHgDxfp/i3SVubZhHcoAJ4CfmQ/1HvXSV8feGdd1Dw9q0WpabMY5UPIz8rr3UjuK+nPAHjDT/FulLcW7CO6QYngJ5Q/1FfmGfZBLAS9rS1pv8PJ/oz9c4c4jjmMFRraVV/5N5rz7o6WiiivmT6wKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAOY+JXhRfFvh5rAXDwTod8LA/KW9GHcV8s65pV/oupzadqNu0FxC2GVu/uPUV9m1x/xM8DWHi/TD8qw6jEP3E4HP+6fUV9Tw7n7wEvY1v4b/AAf+Xc+S4k4eWYR9vR/iL8V29ex8q5o+gq9ruk32i6nLp2oQNFPG2CCOvuPUV6v8Gvhk1w0PiDxDBiH71tbOPvejMPT2r9Cx2Z4fB4f283o9rdfQ/OMBlWIxuI+rwjZre/T1F+DPw0NwYfEPiCDEQ+a2tnH3vRmHp6Cvc1AVQqgADgAUKoVQqgAAYAHalr8lzPMq2Y1nVqv0XRI/YsryuhltBUqS9X1bCiiivOPSCvNfi98OYvEVu+raTEseqRrllHAnHoff3r0qiuvBY2tgqyq0nZr8fJnHjsDRx1F0ayun+Hmj4ruIZbad4J42ilQlWRhgg1Hmvoz4vfDiLxDA+raRGseqRjLIOBOP8a8AsdI1K91hdIgtJDetJ5ZiK4Knvn0xX6zleb0Mwoe1Ts1uu3/A8z8dzbJK+XYj2TV0/hff/g+Q3SbC81XUIbCwgee4mYKiKOpr6a+F3ga28IaaWkYTajOo8+Tsv+yvtTPhd4DtPCWnCWULNqcq/vZcfd/2V9q7avh+IeIHjG6FB/u1u+//AAD73hrhuOBSxGIV6j2X8v8AwQooor5M+xCiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAxdd8LaHrd/a32pWKTT2rbo2Pf2PqK2VAVQqgAAYAHalorSVWc4qMm2lt5GcKNOEnKMUm9/MKKKKzNAooooAKKKKACs+HRdLh1iXV4rKFb6VQrzBfmIrQoqozlG/K7XJlCMrOSvYKKKKkoKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA//2Q=="),
      contactLink = Just adminContactReq,
      preferences = Nothing
    }

timeItToView :: ChatMonad' m => String -> m a -> m a
timeItToView s action = do
  t1 <- liftIO getCurrentTime
  a <- action
  t2 <- liftIO getCurrentTime
  let diff = diffToMilliseconds $ diffUTCTime t2 t1
  toView $ CRTimedAction s diff
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
