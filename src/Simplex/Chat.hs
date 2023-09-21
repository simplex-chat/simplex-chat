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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat where

import Control.Applicative (optional, (<|>))
import Control.Concurrent.STM (retry)
import qualified Control.Exception as E
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (drgNew)
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace, toLower)
import Data.Constraint (Dict (..))
import Data.Either (fromRight, rights)
import Data.Fixed (div')
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (find, foldl', isSuffixOf, partition, sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDay, nominalDiffTimeToSeconds)
import Data.Time.Clock.System (SystemTime, systemToUTCTime)
import Data.Word (Word32)
import qualified Database.SQLite.Simple as SQL
import Simplex.Chat.Archive
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Options
import Simplex.Chat.ProfileGenerator (generateRandomProfile)
import Simplex.Chat.Protocol
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
import Simplex.Chat.Util (encryptFile)
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
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (combine, splitExtensions, takeFileName, (</>))
import System.IO (Handle, IOMode (..), SeekMode (..), hFlush, stdout)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Concurrent (forkFinally, forkIO, mkWeakThreadId, threadDelay)
import UnliftIO.Directory
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
      ciExpirationInterval = 30 * 60 * 1000000 -- 30 minutes
    }

_defaultSMPServers :: NonEmpty SMPServerWithAuth
_defaultSMPServers =
  L.fromList
    [ "smp://h--vW7ZSkXPeOUpfxlFGgauQmXNFOzGoizak7Ult7cw=@smp15.simplex.im,oauu4bgijybyhczbnxtlggo6hiubahmeutaqineuyy23aojpih3dajad.onion",
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

createChatDatabase :: FilePath -> String -> MigrationConfirmation -> IO (Either MigrationError ChatDatabase)
createChatDatabase filePrefix key confirmMigrations = runExceptT $ do
  chatStore <- ExceptT $ createChatStore (chatStoreFile filePrefix) key confirmMigrations
  agentStore <- ExceptT $ createAgentStore (agentStoreFile filePrefix) key confirmMigrations
  pure ChatDatabase {chatStore, agentStore}

newChatController :: ChatDatabase -> Maybe User -> ChatConfig -> ChatOpts -> Maybe (Notification -> IO ()) -> IO ChatController
newChatController ChatDatabase {chatStore, agentStore} user cfg@ChatConfig {agentConfig = aCfg, defaultServers, inlineFiles, tempDir} ChatOpts {coreOptions = CoreChatOpts {smpServers, xftpServers, networkConfig, logLevel, logConnections, logServerHosts, logFile, tbqSize}, optFilesFolder, showReactions, allowInstantFiles, autoAcceptFileSize} sendToast = do
  let inlineFiles' = if allowInstantFiles || autoAcceptFileSize > 0 then inlineFiles else inlineFiles {sendChunks = 0, receiveInstant = False}
      config = cfg {logLevel, showReactions, tbqSize, subscriptionEvents = logConnections, hostEvents = logServerHosts, defaultServers = configServers, inlineFiles = inlineFiles', autoAcceptFileSize}
      sendNotification = fromMaybe (const $ pure ()) sendToast
      firstTime = dbNew chatStore
  activeTo <- newTVarIO ActiveNone
  currentUser <- newTVarIO user
  servers <- agentServers config
  smpAgent <- getSMPAgentClient aCfg {tbqSize} servers agentStore
  agentAsync <- newTVarIO Nothing
  idsDrg <- newTVarIO =<< liftIO drgNew
  inputQ <- newTBQueueIO tbqSize
  outputQ <- newTBQueueIO tbqSize
  notifyQ <- newTBQueueIO tbqSize
  subscriptionMode <- newTVarIO SMSubscribe
  chatLock <- newEmptyTMVarIO
  sndFiles <- newTVarIO M.empty
  rcvFiles <- newTVarIO M.empty
  currentCalls <- atomically TM.empty
  filesFolder <- newTVarIO optFilesFolder
  chatStoreChanged <- newTVarIO False
  expireCIThreads <- newTVarIO M.empty
  expireCIFlags <- newTVarIO M.empty
  cleanupManagerAsync <- newTVarIO Nothing
  timedItemThreads <- atomically TM.empty
  showLiveItems <- newTVarIO False
  userXFTPFileConfig <- newTVarIO $ xftpFileConfig cfg
  tempDirectory <- newTVarIO tempDir
  pure ChatController {activeTo, firstTime, currentUser, smpAgent, agentAsync, chatStore, chatStoreChanged, idsDrg, inputQ, outputQ, notifyQ, subscriptionMode, chatLock, sndFiles, rcvFiles, currentCalls, config, sendNotification, filesFolder, expireCIThreads, expireCIFlags, cleanupManagerAsync, timedItemThreads, showLiveItems, userXFTPFileConfig, tempDirectory, logFilePath = logFile}
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
      a1 <- async $ race_ notificationSubscriber agentSubscriber
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
  subscribe us
  subscribe us'
  where
    subscribe :: [User] -> m ()
    subscribe = mapM_ $ runExceptT . subscribeUserConnections onlyNeeded Agent.subscribeConnections

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
stopChatController ChatController {smpAgent, agentAsync = s, sndFiles, rcvFiles, expireCIFlags} = do
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

execChatCommand :: ChatMonad' m => ByteString -> m ChatResponse
execChatCommand s = do
  u <- readTVarIO =<< asks currentUser
  case parseChatCommand s of
    Left e -> pure $ chatCmdError u e
    Right cmd -> execChatCommand_ u cmd

execChatCommand' :: ChatMonad' m => ChatCommand -> m ChatResponse
execChatCommand' cmd = asks currentUser >>= readTVarIO >>= (`execChatCommand_` cmd)

execChatCommand_ :: ChatMonad' m => Maybe User -> ChatCommand -> m ChatResponse
execChatCommand_ u cmd = either (CRChatCmdError u) id <$> runExceptT (processChatCommand cmd)

parseChatCommand :: ByteString -> Either String ChatCommand
parseChatCommand = A.parseOnly chatCommandP . B.dropWhileEnd isSpace

toView :: ChatMonad' m => ChatResponse -> m ()
toView event = do
  q <- asks outputQ
  atomically $ writeTBQueue q (Nothing, event)

processChatCommand :: forall m. ChatMonad m => ChatCommand -> m ChatResponse
processChatCommand = \case
  ShowActiveUser -> withUser' $ pure . CRActiveUser
  CreateActiveUser NewUser {profile, sameServers, pastTimestamp} -> do
    p@Profile {displayName} <- liftIO $ maybe generateRandomProfile pure profile
    u <- asks currentUser
    (smp, smpServers) <- chooseServers SPSMP
    (xftp, xftpServers) <- chooseServers SPXFTP
    auId <-
      withStore' getUsers >>= \case
        [] -> pure 1
        users -> do
          when (any (\User {localDisplayName = n} -> n == displayName) users) $
            throwChatError $ CEUserExists displayName
          withAgent (\a -> createUser a smp xftp)
    ts <- liftIO $ getCurrentTime >>= if pastTimestamp then coupleDaysAgo else pure
    user <- withStore $ \db -> createUserRecordAt db (AgentUserId auId) p True ts
    storeServers user smpServers
    storeServers user xftpServers
    setActive ActiveNone
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
        unless (null servers) $
          withStore $ \db -> overwriteProtocolServers db user servers
      coupleDaysAgo t = (`addUTCTime` t) . fromInteger . negate . (+ (2 * day)) <$> randomRIO (0, day)
      day = 86400
  ListUsers -> CRUsersList <$> withStoreCtx' (Just "ListUsers, getUsersInfo") getUsersInfo
  APISetActiveUser userId' viewPwd_ -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' viewPwd_
    withStoreCtx' (Just "APISetActiveUser, setActiveUser") $ \db -> setActiveUser db userId'
    setActive ActiveNone
    let user'' = user' {activeUser = True}
    asks currentUser >>= atomically . (`writeTVar` Just user'')
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
  APIActivateChat -> withUser $ \_ -> do
    restoreCalls
    withAgent foregroundAgent
    users <- withStoreCtx' (Just "APIActivateChat, getUsers") getUsers
    void . forkIO $ subscribeUsers True users
    void . forkIO $ startFilesToReceive users
    setAllExpireCIFlags True
    ok_
  APISuspendChat t -> do
    setAllExpireCIFlags False
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
  APISetXFTPConfig cfg -> do
    asks userXFTPFileConfig >>= atomically . (`writeTVar` cfg)
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
  APIGetChats userId withPCC -> withUserId userId $ \user ->
    CRApiChats user <$> withStoreCtx' (Just "APIGetChats, getChatPreviews") (\db -> getChatPreviews db user withPCC)
  APIGetChat (ChatRef cType cId) pagination search -> withUser $ \user -> case cType of
    -- TODO optimize queries calculating ChatStats, currently they're disabled
    CTDirect -> do
      directChat <- withStore (\db -> getDirectChat db user cId pagination search)
      pure $ CRApiChat user (AChat SCTDirect directChat)
    CTGroup -> do
      groupChat <- withStore (\db -> getGroupChat db user cId pagination search)
      pure $ CRApiChat user (AChat SCTGroup groupChat)
    CTContactRequest -> pure $ chatCmdError (Just user) "not implemented"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APIGetChatItems pagination search -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db user pagination search
    pure $ CRChatItems user chatItems
  APIGetChatItemInfo chatRef itemId -> withUser $ \user -> do
    (aci@(AChatItem cType dir _ ci), versions) <- withStore $ \db ->
      (,) <$> getAChatItem db user chatRef itemId <*> liftIO (getChatItemVersions db itemId)
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
      ct@Contact {contactId, localDisplayName = c, contactUsed} <- withStore $ \db -> getContact db user chatId
      assertDirectAllowed user MDSnd ct XMsgNew_
      unless contactUsed $ withStore' $ \db -> updateContactUsed db user ct
      if isVoice mc && not (featureAllowed SCFVoice forUser ct)
        then pure $ chatCmdError (Just user) ("feature not allowed " <> T.unpack (chatFeatureNameText CFVoice))
        else do
          (fInv_, ciFile_, ft_) <- unzipMaybe3 <$> setupSndFileTransfer ct
          timed_ <- sndContactCITimed live ct itemTTL
          (msgContainer, quotedItem_) <- prepareMsg fInv_ timed_
          (msg@SndMessage {sharedMsgId}, _) <- sendDirectContactMessage ct (XMsgNew msgContainer)
          case ft_ of
            Just ft@FileTransferMeta {fileInline = Just IFMSent} ->
              sendDirectFileInline ct ft sharedMsgId
            _ -> pure ()
          ci <- saveSndChatItem' user (CDDirectSnd ct) msg (CISndMsgContent mc) ciFile_ quotedItem_ timed_ live
          forM_ (timed_ >>= timedDeleteAt') $
            startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId' ci)
          setActive $ ActiveC c
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
              withStore' $ \db -> do
                ft@FileTransferMeta {fileId} <- createSndDirectFileTransfer db userId ct file fileInvitation agentConnId_ chSize subMode
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
                qmc = quoteContent origQmc file
                quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
            pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Just quotedItem)
          where
            quoteData :: ChatItem c d -> m (MsgContent, CIQDirection 'CTDirect, Bool)
            quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} = throwChatError CEInvalidQuote
            quoteData ChatItem {content = CISndMsgContent qmc} = pure (qmc, CIQDirectSnd, True)
            quoteData ChatItem {content = CIRcvMsgContent qmc} = pure (qmc, CIQDirectRcv, False)
            quoteData _ = throwChatError CEInvalidQuote
    CTGroup -> do
      g@(Group gInfo _) <- withStore $ \db -> getGroup db user chatId
      assertUserGroupRole gInfo GRAuthor
      send g
      where
        send g@(Group gInfo@GroupInfo {groupId, membership, localDisplayName = gName} ms)
          | isVoice mc && not (groupFeatureAllowed SGFVoice gInfo) = notAllowedError GFVoice
          | not (isVoice mc) && isJust file_ && not (groupFeatureAllowed SGFFiles gInfo) = notAllowedError GFFiles
          | otherwise = do
            (fInv_, ciFile_, ft_) <- unzipMaybe3 <$> setupSndFileTransfer g (length $ filter memberCurrent ms)
            timed_ <- sndGroupCITimed live gInfo itemTTL
            (msgContainer, quotedItem_) <- prepareMsg fInv_ timed_ membership
            (msg@SndMessage {sharedMsgId}, sentToMembers) <- sendGroupMessage user gInfo ms (XMsgNew msgContainer)
            mapM_ (sendGroupFileInline ms sharedMsgId) ft_
            ci <- saveSndChatItem' user (CDGroupSnd gInfo) msg (CISndMsgContent mc) ciFile_ quotedItem_ timed_ live
            withStore' $ \db ->
              forM_ sentToMembers $ \GroupMember {groupMemberId} ->
                createGroupSndStatus db (chatItemId' ci) groupMemberId CISSndNew
            forM_ (timed_ >>= timedDeleteAt') $
              startProximateTimedItemThread user (ChatRef CTGroup groupId, chatItemId' ci)
            setActive $ ActiveG gName
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
        prepareMsg :: Maybe FileInvitation -> Maybe CITimed -> GroupMember -> m (MsgContainer, Maybe (CIQuote 'CTGroup))
        prepareMsg fInv_ timed_ membership = case quotedItemId_ of
          Nothing -> pure (MCSimple (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
          Just quotedItemId -> do
            CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
              withStore $ \db -> getGroupChatItem db user chatId quotedItemId
            (origQmc, qd, sent, GroupMember {memberId}) <- quoteData qci membership
            let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Just memberId}
                qmc = quoteContent origQmc file
                quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
            pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fInv_ (ttl' <$> timed_) (justTrue live)), Just quotedItem)
          where
            quoteData :: ChatItem c d -> GroupMember -> m (MsgContent, CIQDirection 'CTGroup, Bool, GroupMember)
            quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} _ = throwChatError CEInvalidQuote
            quoteData ChatItem {chatDir = CIGroupSnd, content = CISndMsgContent qmc} membership' = pure (qmc, CIQGroupSnd, True, membership')
            quoteData ChatItem {chatDir = CIGroupRcv m, content = CIRcvMsgContent qmc} _ = pure (qmc, CIQGroupRcv $ Just m, False, m)
            quoteData _ _ = throwChatError CEInvalidQuote
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    where
      quoteContent :: forall d. MsgContent -> Maybe (CIFile d) -> MsgContent
      quoteContent qmc ciFile_
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
          getFileName CIFile{fileName} = fileName
          qFileName = maybe qText (T.pack . getFileName) ciFile_
          qTextOrFile = if T.null qText then qFileName else qText
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
          CGContact Contact {activeConn} -> withStore' $ \db -> createSndFTDescrXFTP db user Nothing activeConn ft fileDescr
          CGGroup (Group _ ms) -> forM_ ms $ \m -> saveMemberFD m `catchChatError` (toView . CRChatError (Just user))
            where
              -- we are not sending files to pending members, same as with inline files
              saveMemberFD m@GroupMember {activeConn = Just conn@Connection {connStatus}} =
                when ((connStatus == ConnReady || connStatus == ConnSndReady) && not (connDisabled conn)) $
                  withStore' $ \db -> createSndFTDescrXFTP db user (Just m) conn ft fileDescr
              saveMemberFD _ = pure ()
        pure (fInv, ciFile, ft)
      unzipMaybe3 :: Maybe (a, b, c) -> (Maybe a, Maybe b, Maybe c)
      unzipMaybe3 (Just (a, b, c)) = (Just a, Just b, Just c)
      unzipMaybe3 _ = (Nothing, Nothing, Nothing)
  APIUpdateChatItem (ChatRef cType chatId) itemId live mc -> withUser $ \user -> withChatLock "updateChatItem" $ case cType of
    CTDirect -> do
      (ct@Contact {contactId, localDisplayName = c}, cci) <- withStore $ \db -> (,) <$> getContact db user chatId <*> getDirectChatItem db user chatId itemId
      assertDirectAllowed user MDSnd ct XMsgUpdate_
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
                  setActive $ ActiveC c
                  pure $ CRChatItemUpdated user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci')
                else pure $ CRChatItemNotChanged user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTGroup -> do
      Group gInfo@GroupInfo {groupId, localDisplayName = gName} ms <- withStore $ \db -> getGroup db user chatId
      assertUserGroupRole gInfo GRAuthor
      cci <- withStore $ \db -> getGroupChatItem db user chatId itemId
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
                  setActive $ ActiveG gName
                  pure $ CRChatItemUpdated user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci')
                else pure $ CRChatItemNotChanged user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
  APIDeleteChatItem (ChatRef cType chatId) itemId mode -> withUser $ \user -> withChatLock "deleteChatItem" $ case cType of
    CTDirect -> do
      (ct@Contact {localDisplayName = c}, ci@(CChatItem msgDir ChatItem {meta = CIMeta {itemSharedMsgId, editable}})) <- withStore $ \db -> (,) <$> getContact db user chatId <*> getDirectChatItem db user chatId itemId
      case (mode, msgDir, itemSharedMsgId, editable) of
        (CIDMInternal, _, _, _) -> deleteDirectCI user ct ci True False
        (CIDMBroadcast, SMDSnd, Just itemSharedMId, True) -> do
          assertDirectAllowed user MDSnd ct XMsgDel_
          (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XMsgDel itemSharedMId Nothing)
          setActive $ ActiveC c
          if featureAllowed SCFFullDelete forUser ct
            then deleteDirectCI user ct ci True False
            else markDirectCIDeleted user ct ci msgId True =<< liftIO getCurrentTime
        (CIDMBroadcast, _, _, _) -> throwChatError CEInvalidChatItemDelete
    CTGroup -> do
      Group gInfo ms <- withStore $ \db -> getGroup db user chatId
      ci@(CChatItem msgDir ChatItem {meta = CIMeta {itemSharedMsgId, editable}}) <- withStore $ \db -> getGroupChatItem db user chatId itemId
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
    Group gInfo@GroupInfo {membership} ms <- withStore $ \db -> getGroup db user gId
    ci@(CChatItem _ ChatItem {chatDir, meta = CIMeta {itemSharedMsgId}}) <- withStore $ \db -> getGroupChatItem db user gId itemId
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
            throwChatError $ CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions)
          unless (ciReactionAllowed ci) $
            throwChatError $ CECommandError "reaction not allowed - chat item has no content"
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
      withStore (\db -> (,) <$> getGroup db user chatId <*> getGroupChatItem db user chatId itemId) >>= \case
        (Group g@GroupInfo {membership} ms, CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}}) -> do
          unless (groupFeatureAllowed SGFReactions g) $
            throwChatError $ CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions)
          unless (ciReactionAllowed ci) $
            throwChatError $ CECommandError "reaction not allowed - chat item has no content"
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
          throwChatError $ CECommandError $ "reaction already " <> if add then "added" else "removed"
        when (add && length rs >= maxMsgReactions) $
          throwChatError $ CECommandError "too many reactions"
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
        Group {groupInfo} <- getGroup db user chatId
        liftIO $ updateGroupUnreadChat db user groupInfo unreadChat
      ok user
    _ -> pure $ chatCmdError (Just user) "not supported"
  APIDeleteChat (ChatRef cType chatId) -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct@Contact {localDisplayName} <- withStore $ \db -> getContact db user chatId
      filesInfo <- withStore' $ \db -> getContactFileInfo db user ct
      contactConnIds <- map aConnId <$> withStore (\db -> getContactConnections db userId ct)
      withChatLock "deleteChat direct" . procCmd $ do
        fileAgentConnIds <- concat <$> forM filesInfo (deleteFile user)
        deleteAgentConnectionsAsync user $ fileAgentConnIds <> contactConnIds
        -- functions below are called in separate transactions to prevent crashes on android
        -- (possibly, race condition on integrity check?)
        withStore' $ \db -> deleteContactConnectionsAndFiles db userId ct
        withStore' $ \db -> deleteContact db user ct
        unsetActive $ ActiveC localDisplayName
        pure $ CRContactDeleted user ct
    CTContactConnection -> withChatLock "deleteChat contactConnection" . procCmd $ do
      conn@PendingContactConnection {pccAgentConnId = AgentConnId acId} <- withStore $ \db -> getPendingContactConnection db userId chatId
      deleteAgentConnectionAsync user acId
      withStore' $ \db -> deletePendingContactConnection db userId chatId
      pure $ CRContactConnectionDeleted user conn
    CTGroup -> do
      Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db user chatId
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
                    conns <- withStore $ \db -> getContactConnections db userId ct
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
      gInfo <- withStore $ \db -> getGroupInfo db user chatId
      filesInfo <- withStore' $ \db -> getGroupFileInfo db user gInfo
      deleteFilesAndConns user filesInfo
      withStore' $ \db -> deleteGroupCIs db user gInfo
      membersToDelete <- withStore' $ \db -> getGroupMembersForExpiration db user gInfo
      forM_ membersToDelete $ \m -> withStore' $ \db -> deleteGroupMember db user m
      pure $ CRChatCleared user (AChatInfo SCTGroup $ GroupChat gInfo)
    CTContactConnection -> pure $ chatCmdError (Just user) "not supported"
    CTContactRequest -> pure $ chatCmdError (Just user) "not supported"
  APIAcceptContact incognito connReqId -> withUser $ \_ -> withChatLock "acceptContact" $ do
    (user, cReq) <- withStore $ \db -> getContactRequest' db connReqId
    -- [incognito] generate profile to send, create connection with incognito profile
    incognitoProfile <- if incognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
    ct <- acceptContactRequest user cReq incognitoProfile
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
          callId <- CallId <$> drgRandomBytes 16
          dhKeyPair <- if encryptedCall callType then Just <$> liftIO C.generateKeyPair' else pure Nothing
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
    let ntfMessages = map (\SMP.SMPMsgMeta {msgTs, msgFlags} -> NtfMsgInfo {msgTs = systemToUTCTime msgTs, msgFlags}) msgs
        getMsgTs :: SMP.NMsgMeta -> SystemTime
        getMsgTs SMP.NMsgMeta{msgTs} = msgTs
        msgTs' = systemToUTCTime . getMsgTs <$> ntfMsgMeta
        agentConnId = AgentConnId ntfConnId
    user_ <- withStore' (`getUserByAConnId` agentConnId)
    connEntity <-
      pure user_ $>>= \user ->
        withStore (\db -> Just <$> getConnectionEntity db user agentConnId) `catchChatError` (\e -> toView (CRChatError (Just user) e) $> Nothing)
    pure CRNtfMessages {user_, connEntity, msgTs = msgTs', ntfMessages}
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
  APISetChatItemTTL userId newTTL_ -> withUser $ \user -> do
    checkSameUser userId user
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
  APIGetChatItemTTL userId -> withUserId userId $ \user -> do
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
      withAgent $ \a -> toggleConnectionNtfs a (contactConnId ct) (enableNtfs chatSettings)
      ok user
    CTGroup -> do
      ms <- withStore $ \db -> do
        Group _ ms <- getGroup db user chatId
        liftIO $ updateGroupSettings db user chatId chatSettings
        pure ms
      forM_ (filter memberActive ms) $ \m -> forM_ (memberConnId m) $ \connId ->
        withAgent (\a -> toggleConnectionNtfs a connId $ enableNtfs chatSettings) `catchChatError` (toView . CRChatError (Just user))
      ok user
    _ -> pure $ chatCmdError (Just user) "not supported"
  APIContactInfo contactId -> withUser $ \user@User {userId} -> do
    -- [incognito] print user's incognito profile for this contact
    ct@Contact {activeConn = Connection {customUserProfileId}} <- withStore $ \db -> getContact db user contactId
    incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
    connectionStats <- withAgent (`getConnectionServers` contactConnId ct)
    pure $ CRContactInfo user ct connectionStats (fmap fromLocalProfile incognitoProfile)
  APIGroupInfo gId -> withUser $ \user -> do
    (g, s) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> liftIO (getGroupSummary db user gId)
    pure $ CRGroupInfo user g s
  APIGroupMemberInfo gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> getGroupMember db user gId gMemberId
    connectionStats <- mapM (withAgent . flip getConnectionServers) (memberConnId m)
    pure $ CRGroupMemberInfo user g m connectionStats
  APISwitchContact contactId -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    connectionStats <- withAgent $ \a -> switchConnectionAsync a "" $ contactConnId ct
    pure $ CRContactSwitchStarted user ct connectionStats
  APISwitchGroupMember gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> getGroupMember db user gId gMemberId
    case memberConnId m of
      Just connId -> do
        connectionStats <- withAgent (\a -> switchConnectionAsync a "" connId)
        pure $ CRGroupMemberSwitchStarted user g m connectionStats
      _ -> throwChatError CEGroupMemberNotActive
  APIAbortSwitchContact contactId -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    connectionStats <- withAgent $ \a -> abortConnectionSwitch a $ contactConnId ct
    pure $ CRContactSwitchAborted user ct connectionStats
  APIAbortSwitchGroupMember gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> getGroupMember db user gId gMemberId
    case memberConnId m of
      Just connId -> do
        connectionStats <- withAgent $ \a -> abortConnectionSwitch a connId
        pure $ CRGroupMemberSwitchAborted user g m connectionStats
      _ -> throwChatError CEGroupMemberNotActive
  APISyncContactRatchet contactId force -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    cStats@ConnectionStats {ratchetSyncState = rss} <- withAgent $ \a -> synchronizeRatchet a (contactConnId ct) force
    createInternalChatItem user (CDDirectSnd ct) (CISndConnEvent $ SCERatchetSync rss Nothing) Nothing
    pure $ CRContactRatchetSyncStarted user ct cStats
  APISyncGroupMemberRatchet gId gMemberId force -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> getGroupMember db user gId gMemberId
    case memberConnId m of
      Just connId -> do
        cStats@ConnectionStats {ratchetSyncState = rss} <- withAgent $ \a -> synchronizeRatchet a connId force
        createInternalChatItem user (CDGroupSnd g) (CISndConnEvent . SCERatchetSync rss . Just $ groupMemberRef m) Nothing
        pure $ CRGroupMemberRatchetSyncStarted user g m cStats
      _ -> throwChatError CEGroupMemberNotActive
  APIGetContactCode contactId -> withUser $ \user -> do
    ct@Contact {activeConn = conn@Connection {connId}} <- withStore $ \db -> getContact db user contactId
    code <- getConnectionCode (contactConnId ct)
    ct' <- case contactSecurityCode ct of
      Just SecurityCode {securityCode}
        | sameVerificationCode code securityCode -> pure ct
        | otherwise -> do
          withStore' $ \db -> setConnectionVerified db user connId Nothing
          pure (ct :: Contact) {activeConn = conn {connectionCode = Nothing}}
      _ -> pure ct
    pure $ CRContactCode user ct' code
  APIGetGroupMemberCode gId gMemberId -> withUser $ \user -> do
    (g, m@GroupMember {activeConn}) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> getGroupMember db user gId gMemberId
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
    Contact {activeConn} <- withStore $ \db -> getContact db user contactId
    verifyConnectionCode user activeConn code
  APIVerifyGroupMember gId gMemberId code -> withUser $ \user -> do
    GroupMember {activeConn} <- withStore $ \db -> getGroupMember db user gId gMemberId
    case activeConn of
      Just conn -> verifyConnectionCode user conn code
      _ -> throwChatError CEGroupMemberNotActive
  APIEnableContact contactId -> withUser $ \user -> do
    Contact {activeConn} <- withStore $ \db -> getContact db user contactId
    withStore' $ \db -> setConnectionAuthErrCounter db user activeConn 0
    ok user
  APIEnableGroupMember gId gMemberId -> withUser $ \user -> do
    GroupMember {activeConn} <- withStore $ \db -> getGroupMember db user gId gMemberId
    case activeConn of
      Just conn -> do
        withStore' $ \db -> setConnectionAuthErrCounter db user conn 0
        ok user
      _ -> throwChatError CEGroupMemberNotActive
  SetShowMessages cName ntfOn -> updateChatSettings cName (\cs -> cs {enableNtfs = ntfOn})
  SetSendReceipts cName rcptsOn_ -> updateChatSettings cName (\cs -> cs {sendRcpts = rcptsOn_})
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
    toView $ CRNewContactConnection user conn
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
  APIConnect userId incognito (Just (ACR SCMInvitation cReq)) -> withUserId userId $ \user -> withChatLock "connect" . procCmd $ do
    subMode <- chatReadVar subscriptionMode
    -- [incognito] generate profile to send
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    let profileToSend = userProfileToSend user incognitoProfile Nothing
    dm <- directMessage $ XInfo profileToSend
    connId <- withAgent $ \a -> joinConnection a (aUserId user) True cReq dm subMode
    conn <- withStore' $ \db -> createDirectConnection db user connId cReq ConnJoined (incognitoProfile $> profileToSend) subMode
    toView $ CRNewContactConnection user conn
    pure $ CRSentConfirmation user
  APIConnect userId incognito (Just (ACR SCMContact cReq)) -> withUserId userId $ \user -> connectViaContact user incognito cReq
  APIConnect _ _ Nothing -> throwChatError CEInvalidConnReq
  Connect incognito cReqUri -> withUser $ \User {userId} ->
    processChatCommand $ APIConnect userId incognito cReqUri
  ConnectSimplex incognito -> withUser $ \user ->
    -- [incognito] generate profile to send
    connectViaContact user incognito adminContactReq
  DeleteContact cName -> withContactName cName $ APIDeleteChat . ChatRef CTDirect
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
  APIShowMyAddress userId -> withUserId userId $ \user ->
    CRUserContactLink user <$> withStoreCtx (Just "APIShowMyAddress, getUserAddress") (`getUserAddress` user)
  ShowMyAddress -> withUser $ \User {userId} ->
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
            withStore' (\db -> runExceptT $ getActiveMembersByName db user name) >>= \case
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
        gInfo <- withStore $ \db -> getGroupInfo db user gId
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
    let cts = filter (\ct -> isReady ct && directOrUsed ct) contacts
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
  APINewGroup userId gProfile -> withUserId userId $ \user -> do
    gVar <- asks idsDrg
    groupInfo <- withStore $ \db -> createNewGroup db gVar user gProfile
    pure $ CRGroupCreated user groupInfo
  NewGroup gProfile -> withUser $ \User {userId} ->
    processChatCommand $ APINewGroup userId gProfile
  APIAddMember groupId contactId memRole -> withUser $ \user -> withChatLock "addMember" $ do
    -- TODO for large groups: no need to load all members to determine if contact is a member
    (group, contact) <- withStore $ \db -> (,) <$> getGroup db user groupId <*> getContact db user contactId
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
        gVar <- asks idsDrg
        subMode <- chatReadVar subscriptionMode
        (agentConnId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing subMode
        member <- withStore $ \db -> createNewContactMember db gVar user groupId contact memRole agentConnId cReq subMode
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
    (invitation, ct) <- withStore $ \db -> do
      inv@ReceivedGroupInvitation {fromMember} <- getGroupInvitation db user groupId
      (inv,) <$> getContactViaMember db user fromMember
    let ReceivedGroupInvitation {fromMember, connRequest, groupInfo = g@GroupInfo {membership}} = invitation
        Contact {activeConn = Connection {peerChatVRange}} = ct
    withChatLock "joinGroup" . procCmd $ do
      subMode <- chatReadVar subscriptionMode
      dm <- directMessage $ XGrpAcpt membership.memberId
      agentConnId <- withAgent $ \a -> joinConnection a (aUserId user) True connRequest dm subMode
      withStore' $ \db -> do
        createMemberConnection db userId fromMember agentConnId (fromJVersionRange peerChatVRange) subMode
        updateGroupMemberStatus db userId fromMember GSMemAccepted
        updateGroupMemberStatus db userId membership GSMemAccepted
      updateCIGroupInvitationStatus user
      pure $ CRUserAcceptedGroupSent user g {membership = membership {memberStatus = GSMemAccepted}} Nothing
    where
      updateCIGroupInvitationStatus user = do
        AChatItem _ _ cInfo ChatItem {content, meta = CIMeta {itemId}} <- withStore $ \db -> getChatItemByGroupId db user groupId
        case (cInfo, content) of
          (DirectChat ct, CIRcvGroupInvitation ciGroupInv memRole) -> do
            let aciContent = ACIContent SMDRcv $ CIRcvGroupInvitation ciGroupInv {status = CIGISAccepted} memRole
            updateDirectChatItemView user ct itemId aciContent False Nothing
          _ -> pure () -- prohibited
  APIMemberRole groupId memberId memRole -> withUser $ \user -> do
    Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db user groupId
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
    Group gInfo members <- withStore $ \db -> getGroup db user groupId
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
    Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db user groupId
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
    CRGroupMembers user <$> withStore (\db -> getGroup db user groupId)
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
    processChatCommand $ APIDeleteChat (ChatRef CTGroup groupId)
  ClearGroup gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIClearChat (ChatRef CTGroup groupId)
  ListMembers gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIListMembers groupId
  APIListGroups userId contactId_ search_ -> withUserId userId $ \user ->
    CRGroupsList user <$> withStore' (\db -> getUserGroupsWithSummary db user contactId_ search_)
  ListGroups cName_ search_ -> withUser $ \user@User {userId} -> do
    ct_ <- forM cName_ $ \cName -> withStore $ \db -> getContactByName db user cName
    processChatCommand $ APIListGroups userId (contactId' <$> ct_) search_
  APIUpdateGroupProfile groupId p' -> withUser $ \user -> do
    g <- withStore $ \db -> getGroup db user groupId
    runUpdateGroupProfile user g p'
  UpdateGroupNames gName GroupProfile {displayName, fullName} ->
    updateGroupProfileByName gName $ \p -> p {displayName, fullName}
  ShowGroupProfile gName -> withUser $ \user ->
    CRGroupProfile user <$> withStore (\db -> getGroupInfoByName db user gName)
  UpdateGroupDescription gName description ->
    updateGroupProfileByName gName $ \p -> p {description}
  ShowGroupDescription gName -> withUser $ \user ->
    CRGroupDescription user <$> withStore (\db -> getGroupInfoByName db user gName)
  APICreateGroupLink groupId mRole -> withUser $ \user -> withChatLock "createGroupLink" $ do
    gInfo <- withStore $ \db -> getGroupInfo db user groupId
    assertUserGroupRole gInfo GRAdmin
    when (mRole > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole
    groupLinkId <- GroupLinkId <$> drgRandomBytes 16
    subMode <- chatReadVar subscriptionMode
    let crClientData = encodeJSON $ CRDataGroup groupLinkId
    (connId, cReq) <- withAgent $ \a -> createConnection a (aUserId user) True SCMContact (Just crClientData) subMode
    withStore $ \db -> createGroupLink db user gInfo connId cReq groupLinkId mRole subMode
    pure $ CRGroupLinkCreated user gInfo cReq mRole
  APIGroupLinkMemberRole groupId mRole' -> withUser $ \user -> withChatLock "groupLinkMemberRole " $ do
    gInfo <- withStore $ \db -> getGroupInfo db user groupId
    (groupLinkId, groupLink, mRole) <- withStore $ \db -> getGroupLink db user gInfo
    assertUserGroupRole gInfo GRAdmin
    when (mRole' > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole'
    when (mRole' /= mRole) $ withStore' $ \db -> setGroupLinkMemberRole db user groupLinkId mRole'
    pure $ CRGroupLink user gInfo groupLink mRole'
  APIDeleteGroupLink groupId -> withUser $ \user -> withChatLock "deleteGroupLink" $ do
    gInfo <- withStore $ \db -> getGroupInfo db user groupId
    deleteGroupLink' user gInfo
    pure $ CRGroupLinkDeleted user gInfo
  APIGetGroupLink groupId -> withUser $ \user -> do
    gInfo <- withStore $ \db -> getGroupInfo db user groupId
    (_, groupLink, mRole) <- withStore $ \db -> getGroupLink db user gInfo
    pure $ CRGroupLink user gInfo groupLink mRole
  APICreateMemberContact gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> getGroupMember db user gId gMemberId
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
        pure $ CRNewMemberContact user ct g m
      _ -> throwChatError CEGroupMemberNotActive
  APISendMemberContactInvitation contactId msgContent_ -> withUser $ \user -> do
    (g, m, ct, cReq) <- withStore $ \db -> getMemberContact db user contactId
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
    chats <- withStore' $ \db -> getChatPreviews db user False
    pure $ CRChats $ maybe id take count_ chats
  LastMessages (Just chatName) count search -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand $ APIGetChat chatRef (CPLast count) search
    setActive $ chatActiveTo chatName
    pure $ CRChatItems user (aChatItems . chat $ chatResp)
  LastMessages Nothing count search -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db user (CPLast count) search
    pure $ CRChatItems user chatItems
  LastChatItemId (Just chatName) index -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand (APIGetChat chatRef (CPLast $ index + 1) Nothing)
    pure $ CRChatItemId user (fmap aChatItemId . listToMaybe . aChatItems . chat $ chatResp)
  LastChatItemId Nothing index -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db user (CPLast $ index + 1) Nothing
    pure $ CRChatItemId user (fmap aChatItemId . listToMaybe $ chatItems)
  ShowChatItem (Just itemId) -> withUser $ \user -> do
    chatItem <- withStore $ \db -> do
      chatRef <- getChatRefViaItemId db user itemId
      getAChatItem db user chatRef itemId
    pure $ CRChatItems user ((: []) chatItem)
  ShowChatItem Nothing -> withUser $ \user -> do
    chatItems <- withStore $ \db -> getAllChatItems db user (CPLast 1) Nothing
    pure $ CRChatItems user chatItems
  ShowChatItemInfo chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    itemId <- getChatItemIdByText user chatRef msg
    processChatCommand $ APIGetChatItemInfo chatRef itemId
  ShowLiveItems on -> withUser $ \_ ->
    asks showLiveItems >>= atomically . (`writeTVar` on) >> ok_
  SendFile chatName f -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    processChatCommand . APISendMessage chatRef False Nothing $ ComposedMessage (Just $ CF.plain f) Nothing (MCFile "")
  SendImage chatName f -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    filePath <- toFSFilePath f
    unless (any (`isSuffixOf` map toLower f) imageExtensions) $ throwChatError CEFileImageType {filePath}
    fileSize <- getFileSize filePath
    unless (fileSize <= maxImageSize) $ throwChatError CEFileImageSize {filePath}
    -- TODO include file description for preview
    processChatCommand . APISendMessage chatRef False Nothing $ ComposedMessage (Just $ CF.plain f) Nothing (MCImage "" fixedImagePreview)
  ForwardFile chatName fileId -> forwardFile chatName fileId SendFile
  ForwardImage chatName fileId -> forwardFile chatName fileId SendImage
  SendFileDescription _chatName _f -> pure $ chatCmdError Nothing "TODO"
  ReceiveFile fileId encrypted rcvInline_ filePath_ -> withUser $ \_ ->
    withChatLock "receiveFile" . procCmd $ do
      (user, ft) <- withStore (`getRcvFileTransferById` fileId)
      ft' <- if encrypted then encryptLocalFile ft else pure ft
      receiveFile' user ft' rcvInline_ filePath_
    where
      encryptLocalFile ft = do
        cfArgs <- liftIO $ CF.randomArgs
        withStore' $ \db -> setFileCryptoArgs db fileId cfArgs
        pure (ft :: RcvFileTransfer) {cryptoArgs = Just cfArgs}
  SetFileToReceive fileId encrypted -> withUser $ \_ -> do
    withChatLock "setFileToReceive" . procCmd $ do
      cfArgs <- if encrypted then Just <$> liftIO CF.randomArgs else pure Nothing
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
                Group gInfo ms <- withStore $ \db -> getGroup db user groupId
                void . sendGroupMessage user gInfo ms $ XFileCancel sharedMsgId
              _ -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
            ci <- withStore $ \db -> getChatItemByFileId db user fileId
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
              ci <- withStore $ \db -> getChatItemByFileId db user fileId
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
                getChatItemByFileId db user fileId
              pure $ CRRcvFileCancelled user ci ftr
  FileStatus fileId -> withUser $ \user -> do
    ci@(AChatItem _ _ _ ChatItem {file}) <- withStore $ \db -> getChatItemByFileId db user fileId
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
  QuitChat -> liftIO exitSuccess
  ShowVersion -> do
    let versionInfo = coreVersionInfo $(simplexmqCommitQ)
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
    --   ChatController {chatLock = l, smpAgent = a, outputQ = q, idsDrg = gVar} <- ask
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
      let cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
      withStore' (\db -> getConnReqContactXContactId db user cReqHash) >>= \case
        (Just contact, _) -> pure $ CRContactAlreadyExists user contact
        (_, xContactId_) -> procCmd $ do
          let randomXContactId = XContactId <$> drgRandomBytes 16
          xContactId <- maybe randomXContactId pure xContactId_
          subMode <- chatReadVar subscriptionMode
          -- [incognito] generate profile to send
          incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
          let profileToSend = userProfileToSend user incognitoProfile Nothing
          dm <- directMessage (XContact profileToSend $ Just xContactId)
          connId <- withAgent $ \a -> joinConnection a (aUserId user) True cReq dm subMode
          let groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
          conn <- withStore' $ \db -> createConnReqConnection db userId connId cReqHash xContactId incognitoProfile groupLinkId subMode
          toView $ CRNewContactConnection user conn
          pure $ CRSentInvitation user incognitoProfile
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
      let chunks = - ((- fileSize) `div` fileChunkSize)
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
    updateProfile_ user@User {profile = p} p' updateUser
      | p' == fromLocalProfile p = pure $ CRUserProfileNoChange user
      | otherwise = do
        -- read contacts before user update to correctly merge preferences
        -- [incognito] filter out contacts with whom user has incognito connections
        contacts <-
          filter (\ct -> isReady ct && not (contactConnIncognito ct))
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
    updateContactPrefs user@User {userId} ct@Contact {activeConn = Connection {customUserProfileId}, userPreferences = contactUserPrefs} contactUserPrefs'
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
    runUpdateGroupProfile user (Group g@GroupInfo {groupProfile = p} ms) p' = do
      assertUserGroupRole g GROwner
      g' <- withStore $ \db -> updateGroupProfile db user g p'
      (msg, _) <- sendGroupMessage user g' ms (XGrpInfo p')
      let cd = CDGroupSnd g'
      unless (sameGroupProfileInfo p p') $ do
        ci <- saveSndChatItem user cd msg (CISndGroupEvent $ SGEGroupUpdated p')
        toView $ CRNewChatItem user (AChatItem SCTGroup SMDSnd (GroupChat g') ci)
      createGroupFeatureChangedItems user cd CISndGroupFeature g g'
      pure $ CRGroupUpdated user g g' Nothing
    assertUserGroupRole :: GroupInfo -> GroupMemberRole -> m ()
    assertUserGroupRole g@GroupInfo {membership} requiredRole = do
      when (membership.memberRole < requiredRole) $ throwChatError $ CEGroupUserRole g requiredRole
      when (memberStatus membership == GSMemInvited) $ throwChatError (CEGroupNotJoined g)
      when (memberRemoved membership) $ throwChatError CEGroupMemberUserRemoved
      unless (memberActive membership) $ throwChatError CEGroupMemberNotActive
    delGroupChatItem :: User -> GroupInfo -> CChatItem 'CTGroup -> MessageId -> Maybe GroupMember -> m ChatResponse
    delGroupChatItem user gInfo@GroupInfo {localDisplayName = gName} ci msgId byGroupMember = do
      setActive $ ActiveG gName
      deletedTs <- liftIO getCurrentTime
      if groupFeatureAllowed SGFFullDelete gInfo
        then deleteGroupCI user gInfo ci True False byGroupMember deletedTs
        else markGroupCIDeleted user gInfo ci msgId True byGroupMember deletedTs
    updateGroupProfileByName :: GroupName -> (GroupProfile -> GroupProfile) -> m ChatResponse
    updateGroupProfileByName gName update = withUser $ \user -> do
      g@(Group GroupInfo {groupProfile = p} _) <- withStore $ \db ->
        getGroupIdByName db user gName >>= getGroup db user
      runUpdateGroupProfile user g $ update p
    isReady :: Contact -> Bool
    isReady ct =
      let s = connStatus $ ct.activeConn
       in s == ConnReady || s == ConnSndReady
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
    forwardFile :: ChatName -> FileTransferId -> (ChatName -> FilePath -> ChatCommand) -> m ChatResponse
    forwardFile chatName fileId sendCommand = withUser $ \user -> do
      withStore (\db -> getFileTransfer db user fileId) >>= \case
        FTRcv RcvFileTransfer {fileStatus = RFSComplete RcvFileInfo {filePath}} -> forward filePath
        FTSnd {fileTransferMeta = FileTransferMeta {filePath}} -> forward filePath
        _ -> throwChatError CEFileNotReceived {fileId}
      where
        forward = processChatCommand . sendCommand chatName
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
      setActive $ ActiveG localDisplayName
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
    drgRandomBytes n = asks idsDrg >>= liftIO . (`randomBytes` n)
    privateGetUser :: UserId -> m User
    privateGetUser userId =
      tryChatError (withStore (`getUser` userId)) >>= \case
        Left _ -> throwChatError CEUserUnknown
        Right user -> pure user
    validateUserPassword :: User -> User -> Maybe UserPwd -> m ()
    validateUserPassword User {userId} User {userId = userId', viewPwdHash} viewPwd_ =
      forM_ viewPwdHash $ \pwdHash ->
        let pwdOk = case viewPwd_ of
              Nothing -> userId == userId'
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
      when (activeUser user) $ throwChatError (CECantDeleteActiveUser userId)
      users <- withStore' getUsers
      unless (length users > 1 && (isJust (viewPwdHash user) || length (filter (isNothing . viewPwdHash) users) > 1)) $
        throwChatError (CECantDeleteLastUser userId)
      setActive ActiveNone
    deleteChatUser :: User -> Bool -> m ChatResponse
    deleteChatUser user delSMPQueues = do
      filesInfo <- withStore' (`getUserFileInfo` user)
      forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
      withAgent $ \a -> deleteUser a (aUserId user) delSMPQueues
      withStore' (`deleteUserRecord` user)
      ok_
    updateChatSettings :: ChatName -> (ChatSettings -> ChatSettings) -> m ChatResponse
    updateChatSettings (ChatName cType name) updateSettings = withUser $ \user -> do
      (chatId, chatSettings) <- case cType of
        CTDirect -> withStore $ \db -> do
          ctId <- getContactIdByName db user name
          Contact {chatSettings} <- getContact db user ctId
          pure (ctId, chatSettings)
        CTGroup -> withStore $ \db -> do
          gId <- getGroupIdByName db user name
          GroupInfo {chatSettings} <- getGroupInfo db user gId
          pure (gId, chatSettings)
        _ -> throwChatError $ CECommandError "not supported"
      processChatCommand $ APISetChatSettings (ChatRef cType chatId) $ updateSettings chatSettings

assertDirectAllowed :: ChatMonad m => User -> MsgDirection -> Contact -> CMEventTag e -> m ()
assertDirectAllowed user dir ct event =
  unless (allowedChatEvent || anyDirectOrUsed ct) . unlessM directMessagesAllowed $
    throwChatError $ CEDirectMessagesProhibited dir ct
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
updateDirectChatItemView user ct@Contact {contactId} chatItemId (ACIContent msgDir ciContent) live msgId_ = do
  ci' <- withStore $ \db -> updateDirectChatItem db user contactId chatItemId ciContent live msgId_
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
  case (xftpRcvFile, fileConnReq) of
    -- direct file protocol
    (Nothing, Just connReq) -> do
      subMode <- chatReadVar subscriptionMode
      dm <- directMessage $ XFileAcpt fName
      connIds <- joinAgentConnectionAsync user True connReq dm subMode
      filePath <- getRcvFilePath fileId filePath_ fName True
      withStoreCtx (Just "acceptFileReceive, acceptRcvFileTransfer") $ \db -> acceptRcvFileTransfer db user fileId connIds ConnJoined filePath subMode
    -- XFTP
    (Just XFTPRcvFile {}, _) -> do
      filePath <- getRcvFilePath fileId filePath_ fName False
      (ci, rfd) <- withStoreCtx (Just "acceptFileReceive, xftpAcceptRcvFT ...") $ \db -> do
        -- marking file as accepted and reading description in the same transaction
        -- to prevent race condition with appending description
        ci <- xftpAcceptRcvFT db user fileId filePath
        rfd <- getRcvFileDescrByFileId db fileId
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
      if
          | inline -> do
            -- accepting inline
            ci <- withStoreCtx (Just "acceptFile, acceptRcvInlineFT") $ \db -> acceptRcvInlineFT db user fileId filePath
            sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
            send $ XFileAcptInv sharedMsgId Nothing fName
            pure ci
          | fileInline == Just IFMSent -> throwChatError $ CEFileAlreadyReceiving fName
          | otherwise -> do
            -- accepting via a new connection
            subMode <- chatReadVar subscriptionMode
            connIds <- createAgentConnectionAsync user cmdFunction True SCMInvitation subMode
            withStoreCtx (Just "acceptFile, acceptRcvFileTransfer") $ \db -> acceptRcvFileTransfer db user fileId connIds ConnNew filePath subMode
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
  ci <- withStoreCtx (Just "startReceivingFile, updateRcvFileStatus ...") $ \db -> do
    liftIO $ updateRcvFileStatus db fileId FSConnected
    liftIO $ updateCIFileStatus db user fileId $ CIFSRcvTransfer 0 1
    getChatItemByFileId db user fileId
  toView $ CRRcvFileStart user ci

getRcvFilePath :: forall m. ChatMonad m => FileTransferId -> Maybe FilePath -> String -> Bool -> m FilePath
getRcvFilePath fileId fPath_ fn keepHandle = case fPath_ of
  Nothing ->
    asks filesFolder >>= readTVarIO >>= \case
      Nothing -> do
        dir <- (`combine` "Downloads") <$> getHomeDirectory
        ifM (doesDirectoryExist dir) (pure dir) getChatTempDirectory
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

uniqueCombine :: MonadIO m => FilePath -> String -> m FilePath
uniqueCombine filePath fileName = tryCombine (0 :: Int)
  where
    tryCombine n =
      let (name, ext) = splitExtensions fileName
          suffix = if n == 0 then "" else "_" <> show n
          f = filePath `combine` (name <> suffix <> ext)
       in ifM (doesFileExist f) (tryCombine $ n + 1) (pure f)

getChatTempDirectory :: ChatMonad m => m FilePath
getChatTempDirectory = chatReadVar tempDirectory >>= maybe getTemporaryDirectory pure

acceptContactRequest :: ChatMonad m => User -> UserContactRequest -> Maybe IncognitoProfile -> m Contact
acceptContactRequest user UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange, localDisplayName = cName, profileId, profile = cp, userContactLinkId, xContactId} incognitoProfile = do
  subMode <- chatReadVar subscriptionMode
  let profileToSend = profileToSendOnAccept user incognitoProfile
  dm <- directMessage $ XInfo profileToSend
  acId <- withAgent $ \a -> acceptContact a True invId dm subMode
  withStore' $ \db -> createAcceptedContact db user acId (fromJVersionRange cReqChatVRange) cName profileId cp userContactLinkId xContactId incognitoProfile subMode

acceptContactRequestAsync :: ChatMonad m => User -> UserContactRequest -> Maybe IncognitoProfile -> m Contact
acceptContactRequestAsync user UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange, localDisplayName = cName, profileId, profile = p, userContactLinkId, xContactId} incognitoProfile = do
  subMode <- chatReadVar subscriptionMode
  let profileToSend = profileToSendOnAccept user incognitoProfile
  (cmdId, acId) <- agentAcceptContactAsync user True invId (XInfo profileToSend) subMode
  withStore' $ \db -> do
    ct@Contact {activeConn = Connection {connId}} <- createAcceptedContact db user acId (fromJVersionRange cReqChatVRange) cName profileId p userContactLinkId xContactId incognitoProfile subMode
    setCommandConnId db user cmdId connId
    pure ct

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

subscribeUserConnections :: forall m. ChatMonad m => Bool -> AgentBatchSubscribe m -> User -> m ()
subscribeUserConnections onlyNeeded agentBatchSubscribe user@User {userId} = do
  -- get user connections
  ce <- asks $ subscriptionEvents . config
  (conns, cts, ucs, gs, ms, sfts, rfts, pcs) <-
    if onlyNeeded
      then do
        (conns, entities) <- withStore' getConnectionsToSubscribe
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
      let connIds = map contactConnId cts
      pure (connIds, M.fromList $ zip connIds cts)
    getUserContactLinkConns :: m ([ConnId], Map ConnId UserContact)
    getUserContactLinkConns = do
      (cs, ucs) <- unzip <$> withStore_ ("subscribeUserConnections " <> show userId <> ", getUserContactLinks") getUserContactLinks
      let connIds = map aConnId cs
      pure (connIds, M.fromList $ zip connIds ucs)
    getGroupMemberConns :: m ([Group], [ConnId], Map ConnId GroupMember)
    getGroupMemberConns = do
      gs <- withStore_ ("subscribeUserConnections " <> show userId <> ", getUserGroups") getUserGroups
      let mPairs = concatMap (\(Group _ ms) -> mapMaybe (\m -> (,m) <$> memberConnId m) ms) gs
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
      toView . CRContactSubSummary user $ map (uncurry ContactSubStatus) cRs
      when ce $ mapM_ (toView . uncurry (CRContactSubError user)) cErrors
      where
        cRs = resultsFor rs cts
        cErrors = sortOn (\(Contact {localDisplayName = n}, _) -> n) $ filterErrors cRs
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
        addResult connId = (:) . (,err)
          where
            err = case M.lookup connId rs of
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
      let cutoffTs = addUTCTime (- (30 * nominalDay)) ts
      withStoreCtx' (Just "cleanupManager, deleteOldMessages") (`deleteOldMessages` cutoffTs)

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
  case cType of
    CTDirect -> do
      (ct, ci) <- withStoreCtx (Just "deleteTimedItem, getContact ...") $ \db -> (,) <$> getContact db user chatId <*> getDirectChatItem db user chatId itemId
      deleteDirectCI user ct ci True True >>= toView
    CTGroup -> do
      (gInfo, ci) <- withStoreCtx (Just "deleteTimedItem, getGroupInfo ...") $ \db -> (,) <$> getGroupInfo db user chatId <*> getGroupChatItem db user chatId itemId
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
  let expirationDate = addUTCTime (-1 * fromIntegral ttl) currentTs
      -- this is to keep group messages created during last 12 hours even if they're expired according to item_ts
      createdAtCutoff = addUTCTime (-43200 :: NominalDiffTime) currentTs
  contacts <- withStoreCtx' (Just "expireChatItems, getUserContacts") (`getUserContacts` user)
  loop contacts $ processContact expirationDate
  groups <- withStoreCtx' (Just "expireChatItems, getUserGroupDetails") (\db -> getUserGroupDetails db user Nothing Nothing)
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
processAgentMessage corrId connId msg =
  withStore' (`getUserByAConnId` AgentConnId connId) >>= \case
    Just user -> processAgentMessageConn user corrId connId msg `catchChatError` (toView . CRChatError (Just user))
    _ -> throwChatError $ CENoConnectionUser (AgentConnId connId)

processAgentMessageNoConn :: forall m. ChatMonad m => ACommand 'Agent 'AENone -> m ()
processAgentMessageNoConn = \case
  CONNECT p h -> hostEvent $ CRHostConnected p h
  DISCONNECT p h -> hostEvent $ CRHostDisconnected p h
  DOWN srv conns -> serverEvent srv conns CRContactsDisconnected "disconnected"
  UP srv conns -> serverEvent srv conns CRContactsSubscribed "connected"
  SUSPENDED -> toView CRChatSuspended
  DEL_USER agentUserId -> toView $ CRAgentUserDeleted agentUserId
  where
    hostEvent :: ChatResponse -> m ()
    hostEvent = whenM (asks $ hostEvents . config) . toView
    serverEvent srv@(SMPServer host _ _) conns event str = do
      cs <- withStore' $ \db -> getConnectionsContacts db conns
      toView $ event srv cs
      showToast ("server " <> str) (safeDecodeUtf8 $ strEncode host)

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
      unless cancelled $ case msg of
        SFPROG sndProgress sndTotal -> do
          let status = CIFSSndTransfer {sndProgress, sndTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            getChatItemByFileId db user fileId
          toView $ CRSndFileProgressXFTP user ci ft sndProgress sndTotal
        SFDONE sndDescr rfds -> do
          withStore' $ \db -> setSndFTPrivateSndDescr db user fileId (fileDescrText sndDescr)
          ci@(AChatItem _ d cInfo _ci@ChatItem {meta = CIMeta {itemSharedMsgId = msgId_, itemDeleted}}) <-
            withStore $ \db -> getChatItemByFileId db user fileId
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
                    getChatItemByFileId db user fileId
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
              getChatItemByFileId db user fileId
            withAgent (`xftpDeleteSndFileInternal` aFileId)
            toView $ CRSndFileError user ci
      where
        fileDescrText :: FilePartyI p => ValidFileDescription p -> T.Text
        fileDescrText = safeDecodeUtf8 . strEncode
        sendFileDescription :: SndFileTransfer -> ValidFileDescription 'FRecipient -> SharedMsgId -> (ChatMsgEvent 'Json -> m (SndMessage, Int64)) -> m Int64
        sendFileDescription sft rfd msgId sendMsg = do
          let rfdText = fileDescrText rfd
          withStore' $ \db -> updateSndFTDescrXFTP db user sft rfdText
          partSize <- asks $ xftpDescrPartSize . config
          sendParts 1 partSize rfdText
          where
            sendParts partNo partSize rfdText = do
              let (part, rest) = T.splitAt partSize rfdText
                  complete = T.null rest
                  fileDescr = FileDescr {fileDescrText = part, fileDescrPartNo = partNo, fileDescrComplete = complete}
              (_, msgDeliveryId) <- sendMsg $ XMsgFileDescr {msgId, fileDescr}
              if complete
                then pure msgDeliveryId
                else sendParts (partNo + 1) partSize rest

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
      unless (rcvFileCompleteOrCancelled ft) $ case msg of
        RFPROG rcvProgress rcvTotal -> do
          let status = CIFSRcvTransfer {rcvProgress, rcvTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            getChatItemByFileId db user fileId
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
                getChatItemByFileId db user fileId
              agentXFTPDeleteRcvFile aFileId fileId
              toView $ CRRcvFileComplete user ci
        RFERR e
          | temporaryAgentError e ->
            throwChatError $ CEXFTPRcvFile fileId (AgentRcvFileId aFileId) e
          | otherwise -> do
            ci <- withStore $ \db -> do
              liftIO $ updateFileCancelled db user fileId CIFSRcvError
              getChatItemByFileId db user fileId
            agentXFTPDeleteRcvFile aFileId fileId
            toView $ CRRcvFileError user ci e

processAgentMessageConn :: forall m. ChatMonad m => User -> ACorrId -> ConnId -> ACommand 'Agent 'AEConn -> m ()
processAgentMessageConn user _ agentConnId END =
  withStore (\db -> getConnectionEntity db user $ AgentConnId agentConnId) >>= \case
    RcvDirectMsgConnection _ (Just ct@Contact {localDisplayName = c}) -> do
      toView $ CRContactAnotherClient user ct
      whenUserNtfs user $ showToast (c <> "> ") "connected to another client"
      unsetActive $ ActiveC c
    entity -> toView $ CRSubscriptionEnd user entity
processAgentMessageConn user@User {userId} corrId agentConnId agentMessage = do
  entity <- withStore (\db -> getConnectionEntity db user $ AgentConnId agentConnId) >>= updateConnStatus
  case entity of
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

    isMember :: MemberId -> GroupInfo -> [GroupMember] -> Bool
    isMember memId GroupInfo {membership} members =
      sameMemberId memId membership || isJust (find (sameMemberId memId) members)

    agentMsgConnStatus :: ACommand 'Agent e -> Maybe ConnStatus
    agentMsgConnStatus = \case
      CONF {} -> Just ConnRequested
      INFO _ -> Just ConnSndReady
      CON -> Just ConnReady
      _ -> Nothing

    processDirectMessage :: ACommand 'Agent e -> ConnectionEntity -> Connection -> Maybe Contact -> m ()
    processDirectMessage agentMsg connEntity conn@Connection {connId, peerChatVRange, viaUserContactLink, groupLinkId, customUserProfileId, connectionCode} = \case
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
          withAckMessage agentConnId cmdId meta $ do
            (_conn', _) <- saveRcvMSG conn (ConnectionId connId) meta msgBody cmdId
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
      Just ct@Contact {localDisplayName = c, contactId} -> case agentMsg of
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
            (conn', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveRcvMSG conn (ConnectionId connId) msgMeta msgBody cmdId
            let ct' = ct {activeConn = conn'} :: Contact
            assertDirectAllowed user MDRcv ct' $ toCMEventTag event
            updateChatLock "directMessage" event
            case event of
              XMsgNew mc -> newContentMessage ct' mc msg msgMeta
              XMsgFileDescr sharedMsgId fileDescr -> messageFileDescription ct' sharedMsgId fileDescr msgMeta
              XMsgFileCancel sharedMsgId -> cancelMessageFile ct' sharedMsgId msgMeta
              XMsgUpdate sharedMsgId mContent ttl live -> messageUpdate ct' sharedMsgId mContent msg msgMeta ttl live
              XMsgDel sharedMsgId _ -> messageDelete ct' sharedMsgId msg msgMeta
              XMsgReact sharedMsgId _ reaction add -> directMsgReaction ct' sharedMsgId reaction add msg msgMeta
              -- TODO discontinue XFile
              XFile fInv -> processFileInvitation' ct' fInv msg msgMeta
              XFileCancel sharedMsgId -> xFileCancel ct' sharedMsgId msgMeta
              XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInv ct' sharedMsgId fileConnReq_ fName msgMeta
              XInfo p -> xInfo ct' p
              XGrpInv gInv -> processGroupInvitation ct' gInv msg msgMeta
              XInfoProbe probe -> xInfoProbe (CGMContact ct') probe
              XInfoProbeCheck probeHash -> xInfoProbeCheck ct' probeHash
              XInfoProbeOk probe -> xInfoProbeOk ct' probe
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
          withStore' (\db -> getViaGroupMember db user ct) >>= \case
            Nothing -> do
              -- [incognito] print incognito profile used for this contact
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
              toView $ CRContactConnected user ct (fmap fromLocalProfile incognitoProfile)
              when (directOrUsed ct) $ createFeatureEnabledItems ct
              whenUserNtfs user $ do
                setActive $ ActiveC c
                showToast (c <> "> ") "connected"
              forM_ groupLinkId $ \_ -> probeMatchingContacts ct $ contactConnIncognito ct
              forM_ viaUserContactLink $ \userContactLinkId ->
                withStore' (\db -> getUserContactLinkById db userId userContactLinkId) >>= \case
                  Just (UserContactLink {autoAccept = Just AutoAccept {autoReply = mc_}}, groupId_, gLinkMemRole) -> do
                    forM_ mc_ $ \mc -> do
                      (msg, _) <- sendDirectContactMessage ct (XMsgNew $ MCSimple (extMsgContent mc Nothing))
                      ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc)
                      toView $ CRNewChatItem user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
                    forM_ groupId_ $ \groupId -> do
                      subMode <- chatReadVar subscriptionMode
                      gVar <- asks idsDrg
                      groupConnIds <- createAgentConnectionAsync user CFCreateConnGrpInv True SCMInvitation subMode
                      withStore $ \db -> createNewContactMemberAsync db gVar user groupId ct gLinkMemRole groupConnIds (fromJVersionRange peerChatVRange) subMode
                  _ -> pure ()
            Just (gInfo, m@GroupMember {activeConn}) ->
              when (maybe False ((== ConnReady) . connStatus) activeConn) $ do
                notifyMemberConnected gInfo m $ Just ct
                let connectedIncognito = contactConnIncognito ct || incognitoMembership gInfo
                when (memberCategory m == GCPreMember) $ probeMatchingContacts ct connectedIncognito
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
              let ct' = ct {activeConn = conn {connectionCode = Nothing}} :: Contact
              ratchetSyncEventItem ct'
              toView $ CRContactVerificationReset user ct'
              createInternalChatItem user (CDDirectRcv ct') (CIRcvConnEvent RCEVerificationCodeReset) Nothing
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
    processGroupMessage agentMsg connEntity conn@Connection {connId, connectionCode} gInfo@GroupInfo {groupId, localDisplayName = gName, groupProfile, membership, chatSettings} m = case agentMsg of
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
          XOk -> pure ()
          _ -> messageError "INFO from member must have x.grp.mem.info"
        pure ()
      CON -> do
        members <- withStore' $ \db -> getGroupMembers db user gInfo
        withStore' $ \db -> do
          updateGroupMemberStatus db userId m GSMemConnected
          unless (memberActive membership) $
            updateGroupMemberStatus db userId membership GSMemConnected
        -- possible improvement: check for each pending message, requires keeping track of connection state
        unless (connDisabled conn) $ sendPendingGroupMessages user m conn
        withAgent $ \a -> toggleConnectionNtfs a (aConnId conn) $ enableNtfs chatSettings
        case memberCategory m of
          GCHostMember -> do
            toView $ CRUserJoinedGroup user gInfo {membership = membership {memberStatus = GSMemConnected}} m {memberStatus = GSMemConnected}
            createGroupFeatureItems gInfo m
            let GroupInfo {groupProfile = GroupProfile {description}} = gInfo
            memberConnectedChatItem gInfo m
            forM_ description $ groupDescriptionChatItem gInfo m
            whenUserNtfs user $ do
              setActive $ ActiveG gName
              showToast ("#" <> gName) "you are connected to group"
          GCInviteeMember -> do
            memberConnectedChatItem gInfo m
            toView $ CRJoinedGroupMember user gInfo m {memberStatus = GSMemConnected}
            whenGroupNtfs user gInfo $ do
              setActive $ ActiveG gName
              showToast ("#" <> gName) $ "member " <> m.localDisplayName <> " is connected"
            intros <- withStore' $ \db -> createIntroductions db members m
            void . sendGroupMessage user gInfo members . XGrpMemNew $ memberInfo m
            forM_ intros $ \intro ->
              processIntro intro `catchChatError` (toView . CRChatError (Just user))
            where
              processIntro intro@GroupMemberIntro {introId} = do
                void $ sendDirectMessage conn (XGrpMemIntro $ memberInfo (reMember intro)) (GroupId groupId)
                withStore' $ \db -> updateIntroStatus db introId GMIntroSent
          _ -> do
            -- TODO notify member who forwarded introduction - question - where it is stored? There is via_contact but probably there should be via_member in group_members table
            withStore' (\db -> getViaGroupContact db user m) >>= \case
              Nothing -> do
                notifyMemberConnected gInfo m Nothing
                let connectedIncognito = memberIncognito membership
                when (memberCategory m == GCPreMember) $ probeMatchingMemberContact gInfo m connectedIncognito
              Just ct@Contact {activeConn = Connection {connStatus}} ->
                when (connStatus == ConnReady) $ do
                  notifyMemberConnected gInfo m $ Just ct
                  let connectedIncognito = contactConnIncognito ct || incognitoMembership gInfo
                  when (memberCategory m == GCPreMember) $ probeMatchingContacts ct connectedIncognito
      MSG msgMeta _msgFlags msgBody -> do
        cmdId <- createAckCmd conn
        withAckMessage agentConnId cmdId msgMeta $ do
          (conn', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveRcvMSG conn (GroupId groupId) msgMeta msgBody cmdId
          let m' = m {activeConn = Just conn'} :: GroupMember
          updateChatLock "groupMessage" event
          case event of
            XMsgNew mc -> canSend m' $ newGroupContentMessage gInfo m' mc msg msgMeta
            XMsgFileDescr sharedMsgId fileDescr -> canSend m' $ groupMessageFileDescription gInfo m' sharedMsgId fileDescr msgMeta
            XMsgFileCancel sharedMsgId -> cancelGroupMessageFile gInfo m' sharedMsgId msgMeta
            XMsgUpdate sharedMsgId mContent ttl live -> canSend m' $ groupMessageUpdate gInfo m' sharedMsgId mContent msg msgMeta ttl live
            XMsgDel sharedMsgId memberId -> groupMessageDelete gInfo m' sharedMsgId memberId msg msgMeta
            XMsgReact sharedMsgId (Just memberId) reaction add -> groupMsgReaction gInfo m' sharedMsgId memberId reaction add msg msgMeta
            -- TODO discontinue XFile
            XFile fInv -> processGroupFileInvitation' gInfo m' fInv msg msgMeta
            XFileCancel sharedMsgId -> xFileCancelGroup gInfo m' sharedMsgId msgMeta
            XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInvGroup gInfo m' sharedMsgId fileConnReq_ fName msgMeta
            XGrpMemNew memInfo -> xGrpMemNew gInfo m' memInfo msg msgMeta
            XGrpMemIntro memInfo -> xGrpMemIntro gInfo m' memInfo
            XGrpMemInv memId introInv -> xGrpMemInv gInfo m' memId introInv
            XGrpMemFwd memInfo introInv -> xGrpMemFwd gInfo m' memInfo introInv
            XGrpMemRole memId memRole -> xGrpMemRole gInfo m' memId memRole msg msgMeta
            XGrpMemDel memId -> xGrpMemDel gInfo m' memId msg msgMeta
            XGrpLeave -> xGrpLeave gInfo m' msg msgMeta
            XGrpDel -> xGrpDel gInfo m' msg msgMeta
            XGrpInfo p' -> xGrpInfo gInfo m' p' msg msgMeta
            XGrpDirectInv connReq mContent_ -> canSend m' $ xGrpDirectInv gInfo m' conn' connReq mContent_ msg msgMeta
            XInfoProbe probe -> xInfoProbe (CGMGroupMember gInfo m') probe
            -- XInfoProbeCheck -- TODO merge members?
            -- XInfoProbeOk -- TODO merge members?
            BFileChunk sharedMsgId chunk -> bFileChunkGroup gInfo sharedMsgId chunk msgMeta
            _ -> messageError $ "unsupported message: " <> T.pack (show event)
          currentMemCount <- withStore' $ \db -> getGroupCurrentMembersCount db user gInfo
          let GroupInfo {chatSettings = ChatSettings {sendRcpts}} = gInfo
          pure $
            fromMaybe (sendRcptsSmallGroups user) sendRcpts
              && hasDeliveryReceipt (toCMEventTag event)
              && currentMemCount <= smallGroupsRcptsMemLimit
        where
          canSend :: GroupMember -> m () -> m ()
          canSend mem a
            | mem.memberRole <= GRObserver = messageError "member is not allowed to send messages"
            | otherwise = a
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

    mdeUpdatedCI :: (MsgDecryptError, Word32) -> CChatItem c -> Maybe (ChatItem c 'MDRcv, CIContent 'MDRcv)
    mdeUpdatedCI (mde', n') (CChatItem _ ci@ChatItem {content = CIRcvDecryptionError mde n})
      | mde == mde' = case mde of
        MDERatchetHeader -> r (n + n')
        MDETooManySkipped -> r n' -- the numbers are not added as sequential MDETooManySkipped will have it incremented by 1
        MDERatchetEarlier -> r (n + n')
        MDEOther -> r (n + n')
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
            updateDirectCIFileStatus db user fileId $ CIFSSndTransfer 0 1
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
                getChatItemByFileId db user fileId
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
          ci <- withStore $ \db -> getChatItemByFileId db user fileId
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
                  getChatItemByFileId db user fileId
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
            CORRequest cReq@UserContactRequest {localDisplayName} -> do
              withStore' (\db -> getUserContactLinkById db userId userContactLinkId) >>= \case
                Just (UserContactLink {autoAccept}, groupId_, _) ->
                  case autoAccept of
                    Just AutoAccept {acceptIncognito} -> case groupId_ of
                      Nothing -> do
                        -- [incognito] generate profile to send, create connection with incognito profile
                        incognitoProfile <- if acceptIncognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
                        ct <- acceptContactRequestAsync user cReq incognitoProfile
                        toView $ CRAcceptingContactRequest user ct
                      Just groupId -> do
                        gInfo <- withStore $ \db -> getGroupInfo db user groupId
                        let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                        ct <- acceptContactRequestAsync user cReq profileMode
                        toView $ CRAcceptingGroupJoinRequest user gInfo ct
                    _ -> do
                      toView $ CRReceivedContactRequest user cReq
                      whenUserNtfs user $
                        showToast (localDisplayName <> "> ") "wants to connect to you"
                _ -> pure ()

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
    withAckMessage cId cmdId MsgMeta {recipient = (msgId, _)} action = do
      -- [async agent commands] command should be asynchronous, continuation is ackMsgDeliveryEvent
      -- TODO catching error and sending ACK after an error, particularly if it is a database error, will result in the message not processed (and no notification to the user).
      -- Possible solutions are:
      -- 1) retry processing several times
      -- 2) stabilize database
      -- 3) show screen of death to the user asking to restart
      tryChatError action >>= \case
        Right withRcpt -> ack $ if withRcpt then Just "" else Nothing
        Left e -> ack Nothing >> throwError e
      where
        ack rcpt = withAgent $ \a -> ackMessageAsync a (aCorrId cmdId) cId msgId rcpt

    ackMsgDeliveryEvent :: Connection -> CommandId -> m ()
    ackMsgDeliveryEvent Connection {connId} ackCmdId =
      withStoreCtx'
        (Just $ "createRcvMsgDeliveryEvent, connId: " <> show connId <> ", ackCmdId: " <> show ackCmdId <> ", msgDeliveryStatus: MDSRcvAcknowledged")
        $ \db -> createRcvMsgDeliveryEvent db connId ackCmdId MDSRcvAcknowledged

    sentMsgDeliveryEvent :: Connection -> AgentMsgId -> m ()
    sentMsgDeliveryEvent Connection {connId} msgId =
      withStoreCtx
        (Just $ "createSndMsgDeliveryEvent, connId: " <> show connId <> ", msgId: " <> show msgId <> ", msgDeliveryStatus: MDSSndSent")
        $ \db -> createSndMsgDeliveryEvent db connId msgId MDSSndSent

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
    notifyMemberConnected gInfo m@GroupMember {localDisplayName = c} ct_ = do
      memberConnectedChatItem gInfo m
      toView $ CRConnectedToGroupMember user gInfo m ct_
      let g = groupName' gInfo
      whenGroupNtfs user gInfo $ do
        setActive $ ActiveG g
        showToast ("#" <> g) $ "member " <> c <> " is connected"

    probeMatchingContacts :: Contact -> IncognitoEnabled -> m ()
    probeMatchingContacts ct connectedIncognito = do
      gVar <- asks idsDrg
      if connectedIncognito
        then sendProbe . Probe =<< liftIO (encodedRandomBytes gVar 32)
        else do
          (probe, probeId) <- withStore $ \db -> createSentProbe db gVar userId (CGMContact ct)
          sendProbe probe
          cs <- withStore' $ \db -> getMatchingContacts db user ct
          sendProbeHashes cs probe probeId
      where
        sendProbe :: Probe -> m ()
        sendProbe probe = void . sendDirectContactMessage ct $ XInfoProbe probe

    probeMatchingMemberContact :: GroupInfo -> GroupMember -> IncognitoEnabled -> m ()
    probeMatchingMemberContact _ GroupMember {activeConn = Nothing} _ = pure ()
    probeMatchingMemberContact g m@GroupMember {groupId, activeConn = Just conn} connectedIncognito = do
      gVar <- asks idsDrg
      if connectedIncognito
        then sendProbe . Probe =<< liftIO (encodedRandomBytes gVar 32)
        else do
          (probe, probeId) <- withStore $ \db -> createSentProbe db gVar userId $ CGMGroupMember g m
          sendProbe probe
          cs <- withStore' $ \db -> getMatchingMemberContacts db user m
          sendProbeHashes cs probe probeId
      where
        sendProbe :: Probe -> m ()
        sendProbe probe = void $ sendDirectMessage conn (XInfoProbe probe) (GroupId groupId)

    -- TODO currently we only send probe hashes to contacts
    sendProbeHashes :: [Contact] -> Probe -> Int64 -> m ()
    sendProbeHashes cs probe probeId =
      forM_ cs $ \c -> sendProbeHash c `catchChatError` \_ -> pure ()
      where
        probeHash = ProbeHash $ C.sha256Hash (unProbe probe)
        sendProbeHash :: Contact -> m ()
        sendProbeHash c = do
          void . sendDirectContactMessage c $ XInfoProbeCheck probeHash
          withStore' $ \db -> createSentProbeHash db userId probeId $ CGMContact c

    messageWarning :: Text -> m ()
    messageWarning = toView . CRMessageError user "warning"

    messageError :: Text -> m ()
    messageError = toView . CRMessageError user "error"

    newContentMessage :: Contact -> MsgContainer -> RcvMessage -> MsgMeta -> m ()
    newContentMessage ct@Contact {localDisplayName = c, contactUsed} mc msg@RcvMessage {sharedMsgId_} msgMeta = do
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
          setActive $ ActiveC c
        else do
          let ExtMsgContent _ _ itemTTL live_ = mcExtMsgContent mc
              timed_ = rcvContactCITimed ct itemTTL
              live = fromMaybe False live_
          file_ <- processFileInvitation fInv_ content $ \db -> createRcvFileTransfer db userId ct
          ChatItem {formattedText} <- newChatItem (CIRcvMsgContent content) (snd <$> file_) timed_ live
          autoAcceptFile file_
          whenContactNtfs user ct $ do
            showMsgToast (c <> "> ") content formattedText
            setActive $ ActiveC c
      where
        newChatItem ciContent ciFile_ timed_ live = do
          ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ msgMeta ciContent ciFile_ timed_ live
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getDirectCIReactions db ct sharedMsgId) sharedMsgId_
          toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci {reactions})
          pure ci

    autoAcceptFile :: Maybe (RcvFileTransfer, CIFile 'MDRcv) -> m ()
    autoAcceptFile = mapM_ $ \(ft, CIFile {fileSize}) -> do
      ChatConfig {autoAcceptFileSize = sz} <- asks config
      when (sz > fileSize) $ receiveFile' user ft Nothing Nothing >>= toView

    messageFileDescription :: Contact -> SharedMsgId -> FileDescr -> MsgMeta -> m ()
    messageFileDescription ct@Contact {contactId} sharedMsgId fileDescr msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      fileId <- withStore $ \db -> getFileIdBySharedMsgId db userId contactId sharedMsgId
      processFDMessage fileId fileDescr

    groupMessageFileDescription :: GroupInfo -> GroupMember -> SharedMsgId -> FileDescr -> MsgMeta -> m ()
    groupMessageFileDescription GroupInfo {groupId} _m sharedMsgId fileDescr _msgMeta = do
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

    cancelMessageFile :: Contact -> SharedMsgId -> MsgMeta -> m ()
    cancelMessageFile ct _sharedMsgId msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      -- find the original chat item and file
      -- mark file as cancelled, remove description if exists
      pure ()

    cancelGroupMessageFile :: GroupInfo -> GroupMember -> SharedMsgId -> MsgMeta -> m ()
    cancelGroupMessageFile _gInfo _m _sharedMsgId _msgMeta = do
      pure ()

    processFileInvitation :: Maybe FileInvitation -> MsgContent -> (DB.Connection -> FileInvitation -> Maybe InlineFileMode -> Integer -> ExceptT StoreError IO RcvFileTransfer) -> m (Maybe (RcvFileTransfer, CIFile 'MDRcv))
    processFileInvitation fInv_ mc createRcvFT = forM fInv_ $ \fInv@FileInvitation {fileName, fileSize} -> do
      ChatConfig {fileChunkSize} <- asks config
      inline <- receiveInlineMode fInv (Just mc) fileChunkSize
      ft@RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFT db fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
      (filePath, fileStatus) <- case inline of
        Just IFMSent -> do
          fPath <- getRcvFilePath fileId Nothing fileName True
          withStore' $ \db -> startRcvInlineFT db user ft fPath inline
          pure (Just fPath, CIFSRcvAccepted)
        _ -> pure (Nothing, CIFSRcvInvitation)
      let fileSource = CF.plain <$> filePath
      pure (ft, CIFile {fileId, fileName, fileSize, fileSource, fileStatus, fileProtocol})

    messageUpdate :: Contact -> SharedMsgId -> MsgContent -> RcvMessage -> MsgMeta -> Maybe Int -> Maybe Bool -> m ()
    messageUpdate ct@Contact {contactId, localDisplayName = c} sharedMsgId mc msg@RcvMessage {msgId} msgMeta ttl live_ = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      updateRcvChatItem `catchCINotFound` \_ -> do
        -- This patches initial sharedMsgId into chat item when locally deleted chat item
        -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
        -- Chat item and update message which created it will have different sharedMsgId in this case...
        let timed_ = rcvContactCITimed ct ttl
        ci <- saveRcvChatItem' user (CDDirectRcv ct) msg (Just sharedMsgId) msgMeta content Nothing timed_ live
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          updateDirectChatItem' db user contactId ci content live Nothing
        toView $ CRChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci')
        setActive $ ActiveC c
      where
        MsgMeta {broker = (_, brokerTs)} = msgMeta
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
                    updateDirectChatItem' db user contactId ci content live $ Just msgId
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
          ci@(CChatItem msgDir _) <- withStore $ \db -> getDirectChatItemBySharedMsgId db user contactId sharedMsgId
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

    groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> MemberId -> MsgReaction -> Bool -> RcvMessage -> MsgMeta -> m ()
    groupMsgReaction g@GroupInfo {groupId} m sharedMsgId itemMemberId reaction add RcvMessage {msgId} MsgMeta {broker = (_, brokerTs)} = do
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

    newGroupContentMessage :: GroupInfo -> GroupMember -> MsgContainer -> RcvMessage -> MsgMeta -> m ()
    newGroupContentMessage gInfo m@GroupMember {localDisplayName = c, memberId, memberRole} mc msg@RcvMessage {sharedMsgId_} msgMeta
      | isVoice content && not (groupFeatureAllowed SGFVoice gInfo) = rejected GFVoice
      | not (isVoice content) && isJust fInv_ && not (groupFeatureAllowed SGFFiles gInfo) = rejected GFFiles
      | otherwise = do
        -- TODO integrity message check
        -- check if message moderation event was received ahead of message
        let timed_ = rcvGroupCITimed gInfo itemTTL
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
            ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ msgMeta CIRcvModerated Nothing timed_ False
            ci' <- withStore' $ \db -> updateGroupChatItemModerated db user gInfo (CChatItem SMDRcv ci) moderator moderatedAt
            toView $ CRNewChatItem user ci'
          | otherwise = do
            file_ <- processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId m
            ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ msgMeta (CIRcvMsgContent content) (snd <$> file_) timed_ False
            cr <- markGroupCIDeleted user gInfo (CChatItem SMDRcv ci) createdByMsgId False (Just moderator) moderatedAt
            toView cr
        createItem timed_ live = do
          file_ <- processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId m
          ChatItem {formattedText} <- newChatItem (CIRcvMsgContent content) (snd <$> file_) timed_ live
          autoAcceptFile file_
          let g = groupName' gInfo
          whenGroupNtfs user gInfo $ do
            showMsgToast ("#" <> g <> " " <> c <> "> ") content formattedText
            setActive $ ActiveG g
        newChatItem ciContent ciFile_ timed_ live = do
          ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ msgMeta ciContent ciFile_ timed_ live
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getGroupCIReactions db gInfo memberId sharedMsgId) sharedMsgId_
          groupMsgToView gInfo m ci {reactions} msgMeta
          pure ci

    groupMessageUpdate :: GroupInfo -> GroupMember -> SharedMsgId -> MsgContent -> RcvMessage -> MsgMeta -> Maybe Int -> Maybe Bool -> m ()
    groupMessageUpdate gInfo@GroupInfo {groupId, localDisplayName = g} m@GroupMember {groupMemberId, memberId} sharedMsgId mc msg@RcvMessage {msgId} msgMeta ttl_ live_ =
      updateRcvChatItem `catchCINotFound` \_ -> do
        -- This patches initial sharedMsgId into chat item when locally deleted chat item
        -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
        -- Chat item and update message which created it will have different sharedMsgId in this case...
        let timed_ = rcvGroupCITimed gInfo ttl_
        ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg (Just sharedMsgId) msgMeta content Nothing timed_ live
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          updateGroupChatItem db user groupId ci content live Nothing
        toView $ CRChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci')
        setActive $ ActiveG g
      where
        MsgMeta {broker = (_, brokerTs)} = msgMeta
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
                        updateGroupChatItem db user groupId ci content live $ Just msgId
                      toView $ CRChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci')
                      setActive $ ActiveG g
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId) ci ci'
                    else toView $ CRChatItemNotChanged user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)
                else messageError "x.msg.update: group member attempted to update a message of another member"
            _ -> messageError "x.msg.update: group member attempted invalid message update"

    groupMessageDelete :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> RcvMessage -> MsgMeta -> m ()
    groupMessageDelete gInfo@GroupInfo {groupId, membership} m@GroupMember {memberId, memberRole = senderRole} sharedMsgId sndMemberId_ RcvMessage {msgId} MsgMeta {broker = (_, brokerTs)} = do
      let msgMemberId = fromMaybe memberId sndMemberId_
      withStore' (\db -> runExceptT $ getGroupMemberCIBySharedMsgId db user groupId msgMemberId sharedMsgId) >>= \case
        Right ci@(CChatItem _ ChatItem {chatDir}) -> case chatDir of
          CIGroupRcv mem
            | sameMemberId memberId mem && msgMemberId == memberId -> delete ci Nothing >>= toView
            | otherwise -> deleteMsg mem ci
          CIGroupSnd -> deleteMsg membership ci
        Left e
          | msgMemberId == memberId -> messageError $ "x.msg.del: message not found, " <> tshow e
          | senderRole < GRAdmin -> messageError $ "x.msg.del: message not found, message of another member with insufficient member permissions, " <> tshow e
          | otherwise -> withStore' $ \db -> createCIModeration db gInfo m msgMemberId sharedMsgId msgId brokerTs
      where
        deleteMsg :: GroupMember -> CChatItem 'CTGroup -> m ()
        deleteMsg mem ci = case sndMemberId_ of
          Just sndMemberId
            | sameMemberId sndMemberId mem -> checkRole mem $ delete ci (Just m) >>= toView
            | otherwise -> messageError "x.msg.del: message of another member with incorrect memberId"
          _ -> messageError "x.msg.del: message of another member without memberId"
        checkRole GroupMember {memberRole} a
          | senderRole < GRAdmin || senderRole < memberRole =
            messageError "x.msg.del: message of another member with insufficient member permissions"
          | otherwise = a
        delete ci byGroupMember
          | groupFeatureAllowed SGFFullDelete gInfo = deleteGroupCI user gInfo ci False False byGroupMember brokerTs
          | otherwise = markGroupCIDeleted user gInfo ci msgId False byGroupMember brokerTs

    -- TODO remove once XFile is discontinued
    processFileInvitation' :: Contact -> FileInvitation -> RcvMessage -> MsgMeta -> m ()
    processFileInvitation' ct@Contact {localDisplayName = c} fInv@FileInvitation {fileName, fileSize} msg@RcvMessage {sharedMsgId_} msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      ChatConfig {fileChunkSize} <- asks config
      inline <- receiveInlineMode fInv Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFileTransfer db userId ct fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
      ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ msgMeta (CIRcvMsgContent $ MCFile "") ciFile Nothing False
      toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
      whenContactNtfs user ct $ do
        showToast (c <> "> ") "wants to send a file"
        setActive $ ActiveC c

    -- TODO remove once XFile is discontinued
    processGroupFileInvitation' :: GroupInfo -> GroupMember -> FileInvitation -> RcvMessage -> MsgMeta -> m ()
    processGroupFileInvitation' gInfo m@GroupMember {localDisplayName = c} fInv@FileInvitation {fileName, fileSize} msg@RcvMessage {sharedMsgId_} msgMeta = do
      ChatConfig {fileChunkSize} <- asks config
      inline <- receiveInlineMode fInv Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvGroupFileTransfer db userId m fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
      ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg sharedMsgId_ msgMeta (CIRcvMsgContent $ MCFile "") ciFile Nothing False
      groupMsgToView gInfo m ci msgMeta
      let g = groupName' gInfo
      whenGroupNtfs user gInfo $ do
        showToast ("#" <> g <> " " <> c <> "> ") "wants to send a file"
        setActive $ ActiveG g

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
        ci <- withStore $ \db -> getChatItemByFileId db user fileId
        toView $ CRRcvFileSndCancelled user ci ft

    xFileAcptInv :: Contact -> SharedMsgId -> Maybe ConnReqInvitation -> String -> MsgMeta -> m ()
    xFileAcptInv ct sharedMsgId fileConnReq_ fName msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      fileId <- withStore $ \db -> getDirectFileIdBySharedMsgId db user ct sharedMsgId
      (AChatItem _ _ _ ci) <- withStore $ \db -> getChatItemByFileId db user fileId
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
              ci' <- updateDirectCIFileStatus db user fileId $ CIFSSndTransfer 0 1
              sft <- liftIO $ createSndDirectInlineFT db ct ft
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
          updateDirectCIFileStatus db user fileId CIFSSndComplete
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

    xFileCancelGroup :: GroupInfo -> GroupMember -> SharedMsgId -> MsgMeta -> m ()
    xFileCancelGroup g@GroupInfo {groupId} mem@GroupMember {groupMemberId, memberId} sharedMsgId msgMeta = do
      checkIntegrityCreateItem (CDGroupRcv g mem) msgMeta
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      CChatItem msgDir ChatItem {chatDir} <- withStore $ \db -> getGroupChatItemBySharedMsgId db user groupId groupMemberId sharedMsgId
      case (msgDir, chatDir) of
        (SMDRcv, CIGroupRcv m) -> do
          if sameMemberId memberId m
            then do
              ft <- withStore (\db -> getRcvFileTransfer db user fileId)
              unless (rcvFileCompleteOrCancelled ft) $ do
                cancelRcvFileTransfer user ft >>= mapM_ (deleteAgentConnectionAsync user)
                ci <- withStore $ \db -> getChatItemByFileId db user fileId
                toView $ CRRcvFileSndCancelled user ci ft
            else messageError "x.file.cancel: group member attempted to cancel file of another member" -- shouldn't happen now that query includes group member id
        (SMDSnd, _) -> messageError "x.file.cancel: group member attempted invalid file cancel"

    xFileAcptInvGroup :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe ConnReqInvitation -> String -> MsgMeta -> m ()
    xFileAcptInvGroup g@GroupInfo {groupId} m@GroupMember {activeConn} sharedMsgId fileConnReq_ fName msgMeta = do
      checkIntegrityCreateItem (CDGroupRcv g m) msgMeta
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      (AChatItem _ _ _ ci) <- withStore $ \db -> getChatItemByFileId db user fileId
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
              ci' <- updateDirectCIFileStatus db user fileId $ CIFSSndTransfer 0 1
              sft <- liftIO $ createSndGroupInlineFT db m conn ft
              pure $ CRSndFileStart user ci' sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendMemberFileInline m conn ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
          _ -> messageError "x.file.acpt.inv: member connection is not active"
        else messageError "x.file.acpt.inv: fileName is different from expected"

    groupMsgToView :: GroupInfo -> GroupMember -> ChatItem 'CTGroup 'MDRcv -> MsgMeta -> m ()
    groupMsgToView gInfo m ci msgMeta = do
      checkIntegrityCreateItem (CDGroupRcv gInfo m) msgMeta
      toView $ CRNewChatItem user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)

    processGroupInvitation :: Contact -> GroupInvitation -> RcvMessage -> MsgMeta -> m ()
    processGroupInvitation ct inv msg msgMeta = do
      let Contact {localDisplayName = c, activeConn = Connection {peerChatVRange, customUserProfileId, groupLinkId = groupLinkId'}} = ct
          GroupInvitation {fromMember = (MemberIdRole fromMemId fromRole), invitedMember = (MemberIdRole memId memRole), connRequest, groupLinkId} = inv
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      when (fromRole < GRAdmin || fromRole < memRole) $ throwChatError (CEGroupContactRole c)
      when (fromMemId == memId) $ throwChatError CEGroupDuplicateMemberId
      -- [incognito] if direct connection with host is incognito, create membership using the same incognito profile
      (gInfo@GroupInfo {groupId, localDisplayName, groupProfile, membership = membership@GroupMember {groupMemberId, memberId}}, hostId) <- withStore $ \db -> createGroupInvitation db user ct inv customUserProfileId
      if sameGroupLinkId groupLinkId groupLinkId'
        then do
          subMode <- chatReadVar subscriptionMode
          dm <- directMessage $ XGrpAcpt memberId
          connIds <- joinAgentConnectionAsync user True connRequest dm subMode
          withStore' $ \db -> do
            createMemberConnectionAsync db user hostId connIds (fromJVersionRange peerChatVRange) subMode
            updateGroupMemberStatusById db userId hostId GSMemAccepted
            updateGroupMemberStatus db userId membership GSMemAccepted
          toView $ CRUserAcceptedGroupSent user gInfo {membership = membership {memberStatus = GSMemAccepted}} (Just ct)
        else do
          let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
          ci <- saveRcvChatItem user (CDDirectRcv ct) msg msgMeta content
          withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
          toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
          toView $ CRReceivedGroupInvitation {user, groupInfo = gInfo, contact = ct, fromMemberRole = fromRole, memberRole = memRole}
          whenContactNtfs user ct $
            showToast ("#" <> localDisplayName <> " " <> c <> "> ") "invited you to join the group"
      where
        sameGroupLinkId :: Maybe GroupLinkId -> Maybe GroupLinkId -> Bool
        sameGroupLinkId (Just gli) (Just gli') = gli == gli'
        sameGroupLinkId _ _ = False

    checkIntegrityCreateItem :: forall c. ChatTypeI c => ChatDirection c 'MDRcv -> MsgMeta -> m ()
    checkIntegrityCreateItem cd MsgMeta {integrity, broker = (_, brokerTs)} = case integrity of
      MsgOk -> pure ()
      MsgError e -> createInternalChatItem user cd (CIRcvIntegrityError e) (Just brokerTs)

    xInfo :: Contact -> Profile -> m ()
    xInfo c p' = void $ processContactProfileUpdate c p' True

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

    createFeatureEnabledItems :: Contact -> m ()
    createFeatureEnabledItems ct@Contact {mergedPreferences} =
      forM_ allChatFeatures $ \(ACF f) -> do
        let state = featureState $ getContactUserPreference f mergedPreferences
        createInternalChatItem user (CDDirectRcv ct) (uncurry (CIRcvChatFeature $ chatFeature f) state) Nothing

    createGroupFeatureItems :: GroupInfo -> GroupMember -> m ()
    createGroupFeatureItems g@GroupInfo {fullGroupPreferences} m =
      forM_ allGroupFeatures $ \(AGF f) -> do
        let p = getGroupPreference f fullGroupPreferences
            (_, param) = groupFeatureState p
        createInternalChatItem user (CDGroupRcv g m) (CIRcvGroupFeature (toGroupFeature f) (toGroupPreference p) param) Nothing

    xInfoProbe :: ContactOrGroupMember -> Probe -> m ()
    xInfoProbe cgm2 probe =
      -- [incognito] unless connected incognito
      unless (contactOrGroupMemberIncognito cgm2) $ do
        r <- withStore' $ \db -> matchReceivedProbe db user cgm2 probe
        forM_ r $ \case
          CGMContact c1 -> probeMatch c1 cgm2 probe
          CGMGroupMember _ _ -> messageWarning "xInfoProbe ignored: matched member (no probe hashes sent to members)"

    -- TODO currently we send probe hashes only to contacts
    xInfoProbeCheck :: Contact -> ProbeHash -> m ()
    xInfoProbeCheck c1 probeHash =
      -- [incognito] unless connected incognito
      unless (contactConnIncognito c1) $ do
        r <- withStore' $ \db -> matchReceivedProbeHash db user (CGMContact c1) probeHash
        forM_ r . uncurry $ probeMatch c1

    probeMatch :: Contact -> ContactOrGroupMember -> Probe -> m ()
    probeMatch c1@Contact {contactId = cId1, profile = p1} cgm2 probe =
      case cgm2 of
        CGMContact c2@Contact {contactId = cId2, profile = p2}
          | cId1 /= cId2 && profilesMatch p1 p2 -> do
            void . sendDirectContactMessage c1 $ XInfoProbeOk probe
            mergeContacts c1 c2
          | otherwise -> messageWarning "probeMatch ignored: profiles don't match or same contact id"
        CGMGroupMember g m2@GroupMember {memberProfile = p2, memberContactId}
          | isNothing memberContactId && profilesMatch p1 p2 -> do
            void . sendDirectContactMessage c1 $ XInfoProbeOk probe
            connectContactToMember c1 g m2
          | otherwise -> messageWarning "probeMatch ignored: profiles don't match or member already has contact"

    -- TODO currently we send probe hashes only to contacts
    xInfoProbeOk :: Contact -> Probe -> m ()
    xInfoProbeOk c1@Contact {contactId = cId1} probe =
      withStore' (\db -> matchSentProbe db user (CGMContact c1) probe) >>= \case
        Just (CGMContact c2@Contact {contactId = cId2})
          | cId1 /= cId2 -> mergeContacts c1 c2
          | otherwise -> messageWarning "xInfoProbeOk ignored: same contact id"
        Just (CGMGroupMember g m2@GroupMember {memberContactId})
          | isNothing memberContactId -> connectContactToMember c1 g m2
          | otherwise -> messageWarning "xInfoProbeOk ignored: member already has contact"
        _ -> pure ()

    -- to party accepting call
    xCallInv :: Contact -> CallId -> CallInvitation -> RcvMessage -> MsgMeta -> m ()
    xCallInv ct@Contact {contactId} callId CallInvitation {callType, callDhPubKey} msg@RcvMessage {sharedMsgId_} msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      if featureAllowed SCFCalls forContact ct
        then do
          dhKeyPair <- if encryptedCall callType then Just <$> liftIO C.generateKeyPair' else pure Nothing
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
        saveCallItem status = saveRcvChatItem user (CDDirectRcv ct) msg msgMeta (CIRcvCall status 0)
        featureRejected f = do
          ci <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ msgMeta (CIRcvChatFeatureRejected f) Nothing Nothing False
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

    mergeContacts :: Contact -> Contact -> m ()
    mergeContacts c1 c2 = do
      withStore' $ \db -> mergeContactRecords db userId c1 c2
      toView $ CRContactsMerged user c1 c2

    connectContactToMember :: Contact -> GroupInfo -> GroupMember -> m ()
    connectContactToMember c1 g m2 = do
      withStore' $ \db -> updateMemberContact db user c1 m2
      toView $ CRMemberContactConnected user c1 g m2

    saveConnInfo :: Connection -> ConnInfo -> m Connection
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage activeConn connInfo
      conn' <- updatePeerChatVRange activeConn chatVRange
      case chatMsgEvent of
        XInfo p -> do
          ct <- withStore $ \db -> createDirectContact db user conn' p
          toView $ CRContactConnecting user ct
          pure conn'
        -- TODO show/log error, other events in SMP confirmation
        _ -> pure conn'

    xGrpMemNew :: GroupInfo -> GroupMember -> MemberInfo -> RcvMessage -> MsgMeta -> m ()
    xGrpMemNew gInfo m memInfo@(MemberInfo memId memRole _ memberProfile) msg msgMeta = do
      checkHostRole m memRole
      members <- withStore' $ \db -> getGroupMembers db user gInfo
      unless (sameMemberId memId $ membership gInfo) $
        if isMember memId gInfo members
          then messageError "x.grp.mem.new error: member already exists"
          else do
            newMember@GroupMember {groupMemberId} <- withStore $ \db -> createNewGroupMember db user gInfo memInfo GCPostMember GSMemAnnounced
            ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent $ RGEMemberAdded groupMemberId memberProfile)
            groupMsgToView gInfo m ci msgMeta
            toView $ CRJoinedGroupMemberConnecting user gInfo m newMember

    xGrpMemIntro :: GroupInfo -> GroupMember -> MemberInfo -> m ()
    xGrpMemIntro gInfo@GroupInfo {chatSettings = ChatSettings {enableNtfs}} m@GroupMember {memberRole, localDisplayName = c} memInfo@(MemberInfo memId _ memberChatVRange _) = do
      case memberCategory m of
        GCHostMember -> do
          members <- withStore' $ \db -> getGroupMembers db user gInfo
          if isMember memId gInfo members
            then messageWarning "x.grp.mem.intro ignored: member already exists"
            else do
              when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole c)
              subMode <- chatReadVar subscriptionMode
              -- [async agent commands] commands should be asynchronous, continuation is to send XGrpMemInv - have to remember one has completed and process on second
              groupConnIds <- createConn subMode
              directConnIds <- case memberChatVRange of
                Nothing -> Just <$> createConn subMode
                Just mcvr
                  | isCompatibleRange (fromChatVRange mcvr) groupNoDirectVRange -> pure Nothing
                  | otherwise -> Just <$> createConn subMode
              let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
              void $ withStore $ \db -> createIntroReMember db user gInfo m memInfo groupConnIds directConnIds customUserProfileId subMode
        _ -> messageError "x.grp.mem.intro can be only sent by host member"
      where
        createConn subMode = createAgentConnectionAsync user CFCreateConnGrpMemInv enableNtfs SCMInvitation subMode

    sendXGrpMemInv :: Int64 -> Maybe ConnReqInvitation -> XGrpMemIntroCont -> m ()
    sendXGrpMemInv hostConnId directConnReq XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq} = do
      hostConn <- withStore $ \db -> getConnectionById db user hostConnId
      let msg = XGrpMemInv memberId IntroInvitation {groupConnReq, directConnReq}
      void $ sendDirectMessage hostConn msg (GroupId groupId)
      withStore' $ \db -> updateGroupMemberStatusById db userId groupMemberId GSMemIntroInvited

    xGrpMemInv :: GroupInfo -> GroupMember -> MemberId -> IntroInvitation -> m ()
    xGrpMemInv gInfo@GroupInfo {groupId} m memId introInv = do
      case memberCategory m of
        GCInviteeMember -> do
          members <- withStore' $ \db -> getGroupMembers db user gInfo
          case find (sameMemberId memId) members of
            Nothing -> messageError "x.grp.mem.inv error: referenced member does not exist"
            Just reMember -> do
              GroupMemberIntro {introId} <- withStore $ \db -> saveIntroInvitation db reMember m introInv
              void . sendGroupMessage' user [reMember] (XGrpMemFwd (memberInfo m) introInv) groupId (Just introId) $
                withStore' $ \db -> updateIntroStatus db introId GMIntroInvForwarded
        _ -> messageError "x.grp.mem.inv can be only sent by invitee member"

    xGrpMemFwd :: GroupInfo -> GroupMember -> MemberInfo -> IntroInvitation -> m ()
    xGrpMemFwd gInfo@GroupInfo {membership, chatSettings = ChatSettings {enableNtfs}} m memInfo@(MemberInfo memId memRole memberChatVRange _) introInv@IntroInvitation {groupConnReq, directConnReq} = do
      checkHostRole m memRole
      members <- withStore' $ \db -> getGroupMembers db user gInfo
      toMember <- case find (sameMemberId memId) members of
        -- TODO if the missed messages are correctly sent as soon as there is connection before anything else is sent
        -- the situation when member does not exist is an error
        -- member receiving x.grp.mem.fwd should have also received x.grp.mem.new prior to that.
        -- For now, this branch compensates for the lack of delayed message delivery.
        Nothing -> withStore $ \db -> createNewGroupMember db user gInfo memInfo GCPostMember GSMemAnnounced
        Just m' -> pure m'
      withStore' $ \db -> saveMemberInvitation db toMember introInv
      subMode <- chatReadVar subscriptionMode
      -- [incognito] send membership incognito profile, create direct connection as incognito
      dm <- directMessage $ XGrpMemInfo membership.memberId (fromLocalProfile $ memberProfile membership)
      -- [async agent commands] no continuation needed, but commands should be asynchronous for stability
      groupConnIds <- joinAgentConnectionAsync user enableNtfs groupConnReq dm subMode
      directConnIds <- forM directConnReq $ \dcr -> joinAgentConnectionAsync user enableNtfs dcr dm subMode
      let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
          mcvr = maybe chatInitialVRange fromChatVRange memberChatVRange
      withStore' $ \db -> createIntroToMemberContact db user m toMember mcvr groupConnIds directConnIds customUserProfileId subMode

    xGrpMemRole :: GroupInfo -> GroupMember -> MemberId -> GroupMemberRole -> RcvMessage -> MsgMeta -> m ()
    xGrpMemRole gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId memRole msg msgMeta
      | membership.memberId == memId =
        let gInfo' = gInfo {membership = membership {memberRole = memRole}}
         in changeMemberRole gInfo' membership $ RGEUserRole memRole
      | otherwise = do
        members <- withStore' $ \db -> getGroupMembers db user gInfo
        case find (sameMemberId memId) members of
          Just member -> changeMemberRole gInfo member $ RGEMemberRole (groupMemberId' member) (fromLocalProfile $ memberProfile member) memRole
          _ -> messageError "x.grp.mem.role with unknown member ID"
      where
        changeMemberRole gInfo' member@GroupMember {memberRole = fromRole} gEvent
          | senderRole < GRAdmin || senderRole < fromRole = messageError "x.grp.mem.role with insufficient member permissions"
          | otherwise = do
            withStore' $ \db -> updateGroupMemberRole db user member memRole
            ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent gEvent)
            groupMsgToView gInfo m ci msgMeta
            toView CRMemberRole {user, groupInfo = gInfo', byMember = m, member = member {memberRole = memRole}, fromRole, toRole = memRole}

    checkHostRole :: GroupMember -> GroupMemberRole -> m ()
    checkHostRole GroupMember {memberRole, localDisplayName} memRole =
      when (memberRole < GRAdmin || memberRole < memRole) $ throwChatError (CEGroupContactRole localDisplayName)

    xGrpMemDel :: GroupInfo -> GroupMember -> MemberId -> RcvMessage -> MsgMeta -> m ()
    xGrpMemDel gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId msg msgMeta = do
      members <- withStore' $ \db -> getGroupMembers db user gInfo
      if membership.memberId == memId
        then checkRole membership $ do
          deleteGroupLinkIfExists user gInfo
          -- member records are not deleted to keep history
          deleteMembersConnections user members
          withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemRemoved
          deleteMemberItem RGEUserDeleted
          toView $ CRDeletedMemberUser user gInfo {membership = membership {memberStatus = GSMemRemoved}} m
        else case find (sameMemberId memId) members of
          Nothing -> messageError "x.grp.mem.del with unknown member ID"
          Just member@GroupMember {groupMemberId, memberProfile} ->
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
          ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent gEvent)
          groupMsgToView gInfo m ci msgMeta

    sameMemberId :: MemberId -> GroupMember -> Bool
    sameMemberId memId GroupMember {memberId} = memId == memberId

    xGrpLeave :: GroupInfo -> GroupMember -> RcvMessage -> MsgMeta -> m ()
    xGrpLeave gInfo m msg msgMeta = do
      deleteMemberConnection user m
      -- member record is not deleted to allow creation of "member left" chat item
      withStore' $ \db -> updateGroupMemberStatus db userId m GSMemLeft
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent RGEMemberLeft)
      groupMsgToView gInfo m ci msgMeta
      toView $ CRLeftMember user gInfo m {memberStatus = GSMemLeft}

    xGrpDel :: GroupInfo -> GroupMember -> RcvMessage -> MsgMeta -> m ()
    xGrpDel gInfo@GroupInfo {membership} m@GroupMember {memberRole} msg msgMeta = do
      when (memberRole /= GROwner) $ throwChatError $ CEGroupUserRole gInfo GROwner
      ms <- withStore' $ \db -> do
        members <- getGroupMembers db user gInfo
        updateGroupMemberStatus db userId membership GSMemGroupDeleted
        pure members
      -- member records are not deleted to keep history
      deleteMembersConnections user ms
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent RGEGroupDeleted)
      groupMsgToView gInfo m ci msgMeta
      toView $ CRGroupDeleted user gInfo {membership = membership {memberStatus = GSMemGroupDeleted}} m

    xGrpInfo :: GroupInfo -> GroupMember -> GroupProfile -> RcvMessage -> MsgMeta -> m ()
    xGrpInfo g@GroupInfo {groupProfile = p} m@GroupMember {memberRole} p' msg msgMeta
      | memberRole < GROwner = messageError "x.grp.info with insufficient member permissions"
      | otherwise = unless (p == p') $ do
        g' <- withStore $ \db -> updateGroupProfile db user g p'
        toView $ CRGroupUpdated user g g' (Just m)
        let cd = CDGroupRcv g' m
        unless (sameGroupProfileInfo p p') $ do
          ci <- saveRcvChatItem user cd msg msgMeta (CIRcvGroupEvent $ RGEGroupUpdated p')
          groupMsgToView g' m ci msgMeta
        createGroupFeatureChangedItems user cd CIRcvGroupFeature g g'

    xGrpDirectInv :: GroupInfo -> GroupMember -> Connection -> ConnReqInvitation -> Maybe MsgContent -> RcvMessage -> MsgMeta -> m ()
    xGrpDirectInv g m mConn connReq mContent_ msg msgMeta = do
      unless (groupFeatureAllowed SGFDirectMessages g) $ messageError "x.grp.direct.inv: direct messages not allowed"
      let GroupMember {memberContactId} = m
      subMode <- chatReadVar subscriptionMode
      case memberContactId of
        Nothing -> createNewContact subMode
        Just mContactId -> do
          mCt <- withStore $ \db -> getContact db user mContactId
          let Contact {activeConn = Connection {connId}, contactGrpInvSent} = mCt
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
          mCt' <- withStore' $ \db -> updateMemberContactInvited db user connIds g mConn mCt subMode
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
          checkIntegrityCreateItem (CDGroupRcv g m') msgMeta
          createInternalChatItem user (CDGroupRcv g m') (CIRcvGroupEvent RGEMemberCreatedContact) Nothing
          toView $ CRNewMemberContactReceivedInv user mCt' g m'
          forM_ mContent_ $ \mc -> do
            ci <- saveRcvChatItem user (CDDirectRcv mCt') msg msgMeta (CIRcvMsgContent mc)
            toView $ CRNewChatItem user (AChatItem SCTDirect SMDRcv (DirectChat mCt') ci)
        securityCodeChanged ct = do
          toView $ CRContactVerificationReset user ct
          createInternalChatItem user (CDDirectRcv ct) (CIRcvConnEvent RCEVerificationCodeReset) Nothing

    directMsgReceived :: Contact -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> m ()
    directMsgReceived ct conn@Connection {connId} msgMeta msgRcpts = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore $ \db -> createSndMsgDeliveryEvent db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateDirectItemStatus ct conn agentMsgId $ CISSndRcvd msgRcptStatus SSPComplete

    groupMsgReceived :: GroupInfo -> GroupMember -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> m ()
    groupMsgReceived gInfo m conn@Connection {connId} msgMeta msgRcpts = do
      checkIntegrityCreateItem (CDGroupRcv gInfo m) msgMeta
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore $ \db -> createSndMsgDeliveryEvent db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateGroupItemStatus gInfo m conn agentMsgId $ CISSndRcvd msgRcptStatus SSPComplete

    updateDirectItemStatus :: Contact -> Connection -> AgentMsgId -> CIStatus 'MDSnd -> m ()
    updateDirectItemStatus ct@Contact {contactId} Connection {connId} msgId newStatus =
      withStore' (\db -> getDirectChatItemByAgentMsgId db user contactId connId msgId) >>= \case
        Just (CChatItem SMDSnd ChatItem {meta = CIMeta {itemStatus = CISSndRcvd _ _}}) -> pure ()
        Just (CChatItem SMDSnd ChatItem {meta = CIMeta {itemId, itemStatus}})
          | itemStatus == newStatus -> pure ()
          | otherwise -> do
            chatItem <- withStore $ \db -> updateDirectChatItemStatus db user contactId itemId newStatus
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
              chatItem <- withStore $ \db -> updateGroupChatItemStatus db user groupId itemId newStatus
              toView $ CRChatItemStatusUpdated user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) chatItem)
        _ -> pure ()

updatePeerChatVRange :: ChatMonad m => Connection -> VersionRange -> m Connection
updatePeerChatVRange conn@Connection {connId, peerChatVRange} msgChatVRange = do
  let jMsgChatVRange = JVersionRange msgChatVRange
  if jMsgChatVRange /= peerChatVRange
    then do
      withStore' $ \db -> setPeerChatVRange db connId msgChatVRange
      pure conn {peerChatVRange = jMsgChatVRange}
    else pure conn

parseFileDescription :: (ChatMonad m, FilePartyI p) => Text -> m (ValidFileDescription p)
parseFileDescription =
  liftEither . first (ChatError . CEInvalidFileDescription) . (strDecode . encodeUtf8)

sendDirectFileInline :: ChatMonad m => Contact -> FileTransferMeta -> SharedMsgId -> m ()
sendDirectFileInline ct ft sharedMsgId = do
  msgDeliveryId <- sendFileInline_ ft sharedMsgId $ sendDirectContactMessage ct
  withStore' $ \db -> updateSndDirectFTDelivery db ct ft msgDeliveryId

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
parseChatMessage conn = parseChatMessage_ conn Nothing
{-# INLINE parseChatMessage #-}

parseAChatMessage :: ChatMonad m => Connection -> MsgMeta -> ByteString -> m AChatMessage
parseAChatMessage conn msgMeta = parseChatMessage_ conn (Just msgMeta)
{-# INLINE parseAChatMessage #-}

parseChatMessage_ :: (ChatMonad m, StrEncoding s) => Connection -> Maybe MsgMeta -> ByteString -> m s
parseChatMessage_ conn msgMeta s = liftEither . first (ChatError . errType) $ strDecode s
  where
    errType = CEInvalidChatMessage conn (msgMetaToJson <$> msgMeta) (safeDecodeUtf8 s)

sendFileChunk :: ChatMonad m => User -> SndFileTransfer -> m ()
sendFileChunk user ft@SndFileTransfer {fileId, fileStatus, agentConnId = AgentConnId acId} =
  unless (fileStatus == FSComplete || fileStatus == FSCancelled) $
    withStore' (`createSndFileChunk` ft) >>= \case
      Just chunkNo -> sendFileChunkNo ft chunkNo
      Nothing -> do
        ci <- withStore $ \db -> do
          liftIO $ updateSndFileStatus db ft FSComplete
          liftIO $ deleteSndFileChunks db ft
          updateDirectCIFileStatus db user fileId CIFSSndComplete
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
      liftIO (B.hPut h chunk >> hFlush h) `catchThrow` (fileErr  . show)
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

throwChatError :: ChatMonad m => ChatErrorType -> m a
throwChatError = throwError . ChatError

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
sendDirectContactMessage ct@Contact {activeConn = conn@Connection {connId, connStatus}} chatMsgEvent
  | connStatus /= ConnReady && connStatus /= ConnSndReady = throwChatError $ CEContactNotReady ct
  | connDisabled conn = throwChatError $ CEContactDisabled ct
  | otherwise = sendDirectMessage conn chatMsgEvent (ConnectionId connId)

sendDirectMessage :: (MsgEncodingI e, ChatMonad m) => Connection -> ChatMsgEvent e -> ConnOrGroupId -> m (SndMessage, Int64)
sendDirectMessage conn chatMsgEvent connOrGroupId = do
  when (connDisabled conn) $ throwChatError (CEConnectionDisabled conn)
  msg@SndMessage {msgId, msgBody} <- createSndMessage chatMsgEvent connOrGroupId
  (msg,) <$> deliverMessage conn (toCMEventTag chatMsgEvent) msgBody msgId

createSndMessage :: (MsgEncodingI e, ChatMonad m) => ChatMsgEvent e -> ConnOrGroupId -> m SndMessage
createSndMessage chatMsgEvent connOrGroupId = do
  gVar <- asks idsDrg
  ChatConfig {chatVRange} <- asks config
  withStore $ \db -> createNewSndMessage db gVar connOrGroupId $ \sharedMsgId ->
    let msgBody = strEncode ChatMessage {chatVRange, msgId = Just sharedMsgId, chatMsgEvent}
     in NewMessage {chatMsgEvent, msgBody}

directMessage :: (MsgEncodingI e, ChatMonad m) => ChatMsgEvent e -> m ByteString
directMessage chatMsgEvent = do
  ChatConfig {chatVRange} <- asks config
  pure $ strEncode ChatMessage {chatVRange, msgId = Nothing, chatMsgEvent}

deliverMessage :: ChatMonad m => Connection -> CMEventTag e -> MsgBody -> MessageId -> m Int64
deliverMessage conn@Connection {connId} cmEventTag msgBody msgId = do
  let msgFlags = MsgFlags {notification = hasNotification cmEventTag}
  agentMsgId <- withAgent $ \a -> sendMessage a (aConnId conn) msgFlags msgBody
  let sndMsgDelivery = SndMsgDelivery {connId, agentMsgId}
  withStoreCtx'
    (Just $ "createSndMsgDelivery, sndMsgDelivery: " <> show sndMsgDelivery <> ", msgId: " <> show msgId <> ", cmEventTag: " <> show cmEventTag <> ", msgDeliveryStatus: MDSSndAgent")
    $ \db -> createSndMsgDelivery db sndMsgDelivery msgId

sendGroupMessage :: (MsgEncodingI e, ChatMonad m) => User -> GroupInfo -> [GroupMember] -> ChatMsgEvent e -> m (SndMessage, [GroupMember])
sendGroupMessage user GroupInfo {groupId} members chatMsgEvent =
  sendGroupMessage' user members chatMsgEvent groupId Nothing $ pure ()

sendGroupMessage' :: forall e m. (MsgEncodingI e, ChatMonad m) => User -> [GroupMember] -> ChatMsgEvent e -> Int64 -> Maybe Int64 -> m () -> m (SndMessage, [GroupMember])
sendGroupMessage' user members chatMsgEvent groupId introId_ postDeliver = do
  msg <- createSndMessage chatMsgEvent (GroupId groupId)
  -- TODO collect failed deliveries into a single error
  rs <- forM (filter memberCurrent members) $ \m ->
    messageMember m msg `catchChatError` (\e -> toView (CRChatError (Just user) e) $> Nothing)
  let sentToMembers = catMaybes rs
  pure (msg, sentToMembers)
  where
    messageMember :: GroupMember -> SndMessage -> m (Maybe GroupMember)
    messageMember m@GroupMember {groupMemberId} SndMessage {msgId, msgBody} = case memberConn m of
      Nothing -> do
        withStore' $ \db -> createPendingGroupMessage db groupMemberId msgId introId_
        pure $ Just m
      Just conn@Connection {connStatus}
        | connDisabled conn || connStatus == ConnDeleted -> pure Nothing
        | connStatus == ConnSndReady || connStatus == ConnReady -> do
          let tag = toCMEventTag chatMsgEvent
          deliverMessage conn tag msgBody msgId >> postDeliver
          pure $ Just m
        | otherwise -> do
          withStore' $ \db -> createPendingGroupMessage db groupMemberId msgId introId_
          pure $ Just m

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

saveRcvMSG :: ChatMonad m => Connection -> ConnOrGroupId -> MsgMeta -> MsgBody -> CommandId -> m (Connection, RcvMessage)
saveRcvMSG conn@Connection {connId} connOrGroupId agentMsgMeta msgBody agentAckCmdId = do
  ACMsg _ ChatMessage {chatVRange, msgId = sharedMsgId_, chatMsgEvent} <- parseAChatMessage conn agentMsgMeta msgBody
  conn' <- updatePeerChatVRange conn chatVRange
  let agentMsgId = fst $ recipient agentMsgMeta
      newMsg = NewMessage {chatMsgEvent, msgBody}
      rcvMsgDelivery = RcvMsgDelivery {connId, agentMsgId, agentMsgMeta, agentAckCmdId}
  msg <- withStoreCtx'
    (Just $ "createNewMessageAndRcvMsgDelivery, rcvMsgDelivery: " <> show rcvMsgDelivery <> ", sharedMsgId_: " <> show sharedMsgId_ <> ", msgDeliveryStatus: MDSRcvAgent")
    $ \db -> createNewMessageAndRcvMsgDelivery db connOrGroupId newMsg sharedMsgId_ rcvMsgDelivery
  pure (conn', msg)

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
  liftIO $ mkChatItem cd ciId content ciFile quotedItem (Just sharedMsgId) itemTimed live createdAt createdAt

saveRcvChatItem :: ChatMonad m => User -> ChatDirection c 'MDRcv -> RcvMessage -> MsgMeta -> CIContent 'MDRcv -> m (ChatItem c 'MDRcv)
saveRcvChatItem user cd msg@RcvMessage {sharedMsgId_} msgMeta content =
  saveRcvChatItem' user cd msg sharedMsgId_ msgMeta content Nothing Nothing False

saveRcvChatItem' :: ChatMonad m => User -> ChatDirection c 'MDRcv -> RcvMessage -> Maybe SharedMsgId -> MsgMeta -> CIContent 'MDRcv -> Maybe (CIFile 'MDRcv) -> Maybe CITimed -> Bool -> m (ChatItem c 'MDRcv)
saveRcvChatItem' user cd msg sharedMsgId_ MsgMeta {broker = (_, brokerTs)} content ciFile itemTimed live = do
  createdAt <- liftIO getCurrentTime
  (ciId, quotedItem) <- withStore' $ \db -> do
    when (ciRequiresAttention content) $ updateChatTs db user cd createdAt
    (ciId, quotedItem) <- createNewRcvChatItem db user cd msg sharedMsgId_ content itemTimed live brokerTs createdAt
    forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
    pure (ciId, quotedItem)
  liftIO $ mkChatItem cd ciId content ciFile quotedItem sharedMsgId_ itemTimed live brokerTs createdAt

mkChatItem :: forall c d. MsgDirectionI d => ChatDirection c d -> ChatItemId -> CIContent d -> Maybe (CIFile d) -> Maybe (CIQuote c) -> Maybe SharedMsgId -> Maybe CITimed -> Bool -> ChatItemTs -> UTCTime -> IO (ChatItem c d)
mkChatItem cd ciId content file quotedItem sharedMsgId itemTimed live itemTs currentTs = do
  let itemText = ciContentToText content
      itemStatus = ciCreateStatus content
      meta = mkCIMeta ciId content itemText itemStatus sharedMsgId Nothing False itemTimed (justTrue live) currentTs itemTs currentTs currentTs
  pure ChatItem {chatDir = toCIDirection cd, meta, content, formattedText = parseMaybeMarkdownList itemText, quotedItem, reactions = [], file}

deleteDirectCI :: ChatMonad m => User -> Contact -> CChatItem 'CTDirect -> Bool -> Bool -> m ChatResponse
deleteDirectCI user ct ci@(CChatItem msgDir deletedItem@ChatItem {file}) byUser timed = do
  deleteCIFile user file
  withStoreCtx' (Just "deleteDirectCI, deleteDirectChatItem") $ \db -> deleteDirectChatItem db user ct ci
  pure $ CRChatItemDeleted user (AChatItem SCTDirect msgDir (DirectChat ct) deletedItem) Nothing byUser timed

deleteGroupCI :: ChatMonad m => User -> GroupInfo -> CChatItem 'CTGroup -> Bool -> Bool -> Maybe GroupMember -> UTCTime -> m ChatResponse
deleteGroupCI user gInfo ci@(CChatItem msgDir deletedItem@ChatItem {file}) byUser timed byGroupMember_ deletedTs = do
  deleteCIFile user file
  toCi <- withStoreCtx' (Just "deleteGroupCI, deleteGroupChatItem ...") $ \db ->
    case byGroupMember_ of
      Nothing -> deleteGroupChatItem db user gInfo ci $> Nothing
      Just m -> Just <$> updateGroupChatItemModerated db user gInfo ci m deletedTs
  pure $ CRChatItemDeleted user (AChatItem SCTGroup msgDir (GroupChat gInfo) deletedItem) toCi byUser timed

deleteCIFile :: (ChatMonad m, MsgDirectionI d) => User -> Maybe (CIFile d) -> m ()
deleteCIFile user file_ =
  forM_ file_ $ \file -> do
    fileAgentConnIds <- deleteFile' user (mkCIFileInfo file) True
    deleteAgentConnectionsAsync user fileAgentConnIds

markDirectCIDeleted :: ChatMonad m => User -> Contact -> CChatItem 'CTDirect -> MessageId -> Bool -> UTCTime -> m ChatResponse
markDirectCIDeleted user ct@Contact {contactId} ci@(CChatItem _ ChatItem {file}) msgId byUser deletedTs = do
  cancelCIFile user file
  toCi <- withStore $ \db -> do
    liftIO $ markDirectChatItemDeleted db user ct ci msgId deletedTs
    getDirectChatItem db user contactId (cchatItemId ci)
  pure $ CRChatItemDeleted user (ctItem ci) (Just $ ctItem toCi) byUser False
  where
    ctItem (CChatItem msgDir ci') = AChatItem SCTDirect msgDir (DirectChat ct) ci'

markGroupCIDeleted :: ChatMonad m => User -> GroupInfo -> CChatItem 'CTGroup -> MessageId -> Bool -> Maybe GroupMember -> UTCTime -> m ChatResponse
markGroupCIDeleted user gInfo@GroupInfo {groupId} ci@(CChatItem _ ChatItem {file}) msgId byUser byGroupMember_ deletedTs = do
  cancelCIFile user file
  toCi <- withStore $ \db -> do
    liftIO $ markGroupChatItemDeleted db user gInfo ci msgId byGroupMember_ deletedTs
    getGroupChatItem db user groupId (cchatItemId ci)
  pure $ CRChatItemDeleted user (gItem ci) (Just $ gItem toCi) byUser False
  where
    gItem (CChatItem msgDir ci') = AChatItem SCTGroup msgDir (GroupChat gInfo) ci'

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
  forM_ allGroupFeatures $ \(AGF f) -> do
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
  ci <- liftIO $ mkChatItem cd ciId content Nothing Nothing Nothing Nothing False itemTs createdAt
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
          fullName <- T.pack <$> getWithPrompt "full name (optional)"
          withTransaction st (\db -> runExceptT $ createUserRecord db (AgentUserId 1) Profile {displayName, fullName, image = Nothing, contactLink = Nothing, preferences = Nothing} True) >>= \case
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
      displayName <- getWithPrompt "display name (no spaces)"
      if null displayName || isJust (find (== ' ') displayName)
        then putStrLn "display name has space(s), choose another one" >> getContactName
        else pure $ T.pack displayName
    getWithPrompt :: String -> IO String
    getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

whenUserNtfs :: ChatMonad' m => User -> m () -> m ()
whenUserNtfs User {showNtfs, activeUser} = when $ showNtfs || activeUser

whenContactNtfs :: ChatMonad' m => User -> Contact -> m () -> m ()
whenContactNtfs user Contact {chatSettings} = whenUserNtfs user . when (enableNtfs chatSettings)

whenGroupNtfs :: ChatMonad' m => User -> GroupInfo -> m () -> m ()
whenGroupNtfs user GroupInfo {chatSettings} = whenUserNtfs user . when (enableNtfs chatSettings)

showMsgToast :: ChatMonad' m => Text -> MsgContent -> Maybe MarkdownList -> m ()
showMsgToast from mc md_ = showToast from $ maybe (msgContentText mc) (mconcat . map hideSecret) md_
  where
    hideSecret :: FormattedText -> Text
    hideSecret FormattedText {format = Just Secret} = "..."
    hideSecret FormattedText {text} = text

showToast :: ChatMonad' m => Text -> Text -> m ()
showToast title text = atomically . (`writeTBQueue` Notification {title, text}) =<< asks notifyQ

notificationSubscriber :: ChatMonad' m => m ()
notificationSubscriber = do
  ChatController {notifyQ, sendNotification} <- ask
  forever $ atomically (readTBQueue notifyQ) >>= liftIO . sendNotification

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

withAgent :: ChatMonad m => (AgentClient -> ExceptT AgentErrorType m a) -> m a
withAgent action =
  asks smpAgent
    >>= runExceptT . action
    >>= liftEither . first (`ChatErrorAgent` Nothing)

withStore' :: ChatMonad m => (DB.Connection -> IO a) -> m a
withStore' action = withStore $ liftIO . action

withStore :: ChatMonad m => (DB.Connection -> ExceptT StoreError IO a) -> m a
withStore = withStoreCtx Nothing

withStoreCtx' :: ChatMonad m => Maybe String -> (DB.Connection -> IO a) -> m a
withStoreCtx' ctx_ action = withStoreCtx ctx_ $ liftIO . action

withStoreCtx :: ChatMonad m => Maybe String -> (DB.Connection -> ExceptT StoreError IO a) -> m a
withStoreCtx ctx_ action = do
  ChatController {chatStore} <- ask
  liftEitherError ChatErrorStore $ case ctx_ of
    Nothing -> withTransaction chatStore (runExceptT . action) `E.catch` handleInternal ""
    -- uncomment to debug store performance
    -- Just ctx -> do
    --   t1 <- liftIO getCurrentTime
    --   putStrLn $ "withStoreCtx start       :: " <> show t1 <> " :: " <> ctx
    --   r <- withTransactionCtx ctx_ chatStore (runExceptT . action) `E.catch` handleInternal (" (" <> ctx <> ")")
    --   t2 <- liftIO getCurrentTime
    --   putStrLn $ "withStoreCtx end         :: " <> show t2 <> " :: " <> ctx <> " :: duration=" <> show (diffToMilliseconds $ diffUTCTime t2 t1)
    --   pure r
    Just _ -> withTransaction chatStore (runExceptT . action) `E.catch` handleInternal ""
  where
    handleInternal :: String -> E.SomeException -> IO (Either StoreError a)
    handleInternal ctxStr e = pure . Left . SEInternalError $ show e <> ctxStr

chatCommandP :: Parser ChatCommand
chatCommandP =
  choice
    [ "/mute " *> ((`SetShowMessages` False) <$> chatNameP),
      "/unmute " *> ((`SetShowMessages` True) <$> chatNameP),
      "/receipts " *> (SetSendReceipts <$> chatNameP <* " " <*> ((Just <$> onOffP) <|> ("default" $> Nothing))),
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
      "/_app activate" $> APIActivateChat,
      "/_app suspend " *> (APISuspendChat <$> A.decimal),
      "/_resubscribe all" $> ResubscribeAllConnections,
      "/_temp_folder " *> (SetTempFolder <$> filePath),
      ("/_files_folder " <|> "/files_folder ") *> (SetFilesFolder <$> filePath),
      "/_xftp " *> (APISetXFTPConfig <$> ("on " *> (Just <$> jsonP) <|> ("off" $> Nothing))),
      "/xftp " *> (APISetXFTPConfig <$> ("on" *> (Just <$> xftpCfgP) <|> ("off" $> Nothing))),
      "/_db export " *> (APIExportArchive <$> jsonP),
      "/db export" $> ExportArchive,
      "/_db import " *> (APIImportArchive <$> jsonP),
      "/_db delete" $> APIDeleteStorage,
      "/_db encryption " *> (APIStorageEncryption <$> jsonP),
      "/db encrypt " *> (APIStorageEncryption . DBEncryptionConfig "" <$> dbKeyP),
      "/db key " *> (APIStorageEncryption <$> (DBEncryptionConfig <$> dbKeyP <* A.space <*> dbKeyP)),
      "/db decrypt " *> (APIStorageEncryption . (`DBEncryptionConfig` "") <$> dbKeyP),
      "/sql chat " *> (ExecChatStoreSQL <$> textP),
      "/sql agent " *> (ExecAgentStoreSQL <$> textP),
      "/sql slow" $> SlowSQLQueries,
      "/_get chats " *> (APIGetChats <$> A.decimal <*> (" pcc=on" $> True <|> " pcc=off" $> False <|> pure False)),
      "/_get chat " *> (APIGetChat <$> chatRefP <* A.space <*> chatPaginationP <*> optional (" search=" *> stringP)),
      "/_get items " *> (APIGetChatItems <$> chatPaginationP <*> optional (" search=" *> stringP)),
      "/_get item info " *> (APIGetChatItemInfo <$> chatRefP <* A.space <*> A.decimal),
      "/_send " *> (APISendMessage <$> chatRefP <*> liveMessageP <*> sendMessageTTLP <*> (" json " *> jsonP <|> " text " *> (ComposedMessage Nothing Nothing <$> mcTextP))),
      "/_update item " *> (APIUpdateChatItem <$> chatRefP <* A.space <*> A.decimal <*> liveMessageP <* A.space <*> msgContentP),
      "/_delete item " *> (APIDeleteChatItem <$> chatRefP <* A.space <*> A.decimal <* A.space <*> ciDeleteMode),
      "/_delete member item #" *> (APIDeleteMemberChatItem <$> A.decimal <* A.space <*> A.decimal <* A.space <*> A.decimal),
      "/_reaction " *> (APIChatItemReaction <$> chatRefP <* A.space <*> A.decimal <* A.space <*> onOffP <* A.space <*> jsonP),
      "/_read chat " *> (APIChatRead <$> chatRefP <*> optional (A.space *> ((,) <$> ("from=" *> A.decimal) <* A.space <*> ("to=" *> A.decimal)))),
      "/_unread chat " *> (APIChatUnread <$> chatRefP <* A.space <*> onOffP),
      "/_delete " *> (APIDeleteChat <$> chatRefP),
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
      "/_verify code @" *> (APIVerifyContact <$> A.decimal <*> optional (A.space *> textP)),
      "/_verify code #" *> (APIVerifyGroupMember <$> A.decimal <* A.space <*> A.decimal <*> optional (A.space *> textP)),
      "/_enable @" *> (APIEnableContact <$> A.decimal),
      "/_enable #" *> (APIEnableGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/code " *> char_ '@' *> (GetContactCode <$> displayName),
      "/code #" *> (GetGroupMemberCode <$> displayName <* A.space <* char_ '@' <*> displayName),
      "/verify " *> char_ '@' *> (VerifyContact <$> displayName <*> optional (A.space *> textP)),
      "/verify #" *> (VerifyGroupMember <$> displayName <* A.space <* char_ '@' <*> displayName <*> optional (A.space *> textP)),
      "/enable " *> char_ '@' *> (EnableContact <$> displayName),
      "/enable #" *> (EnableGroupMember <$> displayName <* A.space <* char_ '@' <*> displayName),
      ("/help files" <|> "/help file" <|> "/hf") $> ChatHelp HSFiles,
      ("/help groups" <|> "/help group" <|> "/hg") $> ChatHelp HSGroups,
      ("/help contacts" <|> "/help contact" <|> "/hc") $> ChatHelp HSContacts,
      ("/help address" <|> "/ha") $> ChatHelp HSMyAddress,
      "/help incognito" $> ChatHelp HSIncognito,
      ("/help messages" <|> "/hm") $> ChatHelp HSMessages,
      ("/help settings" <|> "/hs") $> ChatHelp HSSettings,
      ("/help db" <|> "/hd") $> ChatHelp HSDatabase,
      ("/help" <|> "/h") $> ChatHelp HSMain,
      ("/group " <|> "/g ") *> char_ '#' *> (NewGroup <$> groupProfile),
      "/_group " *> (APINewGroup <$> A.decimal <* A.space <*> jsonP),
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
      "/_connect " *> (APIConnect <$> A.decimal <*> incognitoOnOffP <* A.space <*> ((Just <$> strP) <|> A.takeByteString $> Nothing)),
      "/_connect " *> (APIAddContact <$> A.decimal <*> incognitoOnOffP),
      "/_set incognito :" *> (APISetConnectionIncognito <$> A.decimal <* A.space <*> onOffP),
      ("/connect" <|> "/c") *> (Connect <$> incognitoP <* A.space <*> ((Just <$> strP) <|> A.takeByteString $> Nothing)),
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
      ("/file " <|> "/f ") *> (SendFile <$> chatNameP' <* A.space <*> filePath),
      ("/image " <|> "/img ") *> (SendImage <$> chatNameP' <* A.space <*> filePath),
      ("/fforward " <|> "/ff ") *> (ForwardFile <$> chatNameP' <* A.space <*> A.decimal),
      ("/image_forward " <|> "/imgf ") *> (ForwardImage <$> chatNameP' <* A.space <*> A.decimal),
      ("/fdescription " <|> "/fd") *> (SendFileDescription <$> chatNameP' <* A.space <*> filePath),
      ("/freceive " <|> "/fr ") *> (ReceiveFile <$> A.decimal <*> (" encrypt=" *> onOffP <|> pure False) <*> optional (" inline=" *> onOffP) <*> optional (A.space *> filePath)),
      "/_set_file_to_receive " *> (SetFileToReceive <$> A.decimal <*> (" encrypt=" *> onOffP <|> pure False)),
      ("/fcancel " <|> "/fc ") *> (CancelFile <$> A.decimal),
      ("/fstatus " <|> "/fs ") *> (FileStatus <$> A.decimal),
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
    mcTextP = MCText . safeDecodeUtf8 <$> A.takeByteString
    msgContentP = "text " *> mcTextP <|> "json " *> jsonP
    ciDeleteMode = "broadcast" $> CIDMBroadcast <|> "internal" $> CIDMInternal
    displayName = safeDecodeUtf8 <$> (B.cons <$> A.satisfy refChar <*> A.takeTill (== ' '))
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
    refChar c = c > ' ' && c /= '#' && c /= '@'
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
      let groupPreferences = Just (emptyGroupPrefs :: GroupPreferences) {directMessages = Just DirectMessagesGroupPreference {enable = FEOn}}
      pure GroupProfile {displayName = gName, fullName, description = Nothing, image = Nothing, groupPreferences}
    fullNameP = A.space *> textP <|> pure ""
    textP = safeDecodeUtf8 <$> A.takeByteString
    pwdP = jsonP <|> (UserPwd . safeDecodeUtf8 <$> A.takeTill (== ' '))
    msgTextP = jsonP <|> textP
    stringP = T.unpack . safeDecodeUtf8 <$> A.takeByteString
    filePath = stringP
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
    nonEmptyKey k@(DBEncryptionKey s) = if null s then Left "empty key" else Right k
    autoAcceptP =
      ifM
        onOffP
        (Just <$> (AutoAccept <$> (" incognito=" *> onOffP <|> pure False) <*> optional (A.space *> msgContentP)))
        (pure Nothing)
    srvCfgP = strP >>= \case AProtocolType p -> APSC p <$> (A.space *> jsonP)
    toServerCfg server = ServerCfg {server, preset = False, tested = Nothing, enabled = True}
    char_ = optional . A.char

adminContactReq :: ConnReqContact
adminContactReq =
  either error id $ strDecode "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

timeItToView :: ChatMonad' m => String -> m a -> m a
timeItToView s action = do
  t1 <- liftIO getCurrentTime
  a <- action
  t2 <- liftIO getCurrentTime
  let diff = diffToMilliseconds $ diffUTCTime t2 t1
  toView $ CRTimedAction s diff
  pure a
