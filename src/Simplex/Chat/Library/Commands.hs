{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Library.Commands where

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
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char
import Data.Constraint (Dict (..))
import Data.Either (fromRight, partitionEithers, rights)
import Data.Foldable (foldr')
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (dropWhileEnd, find, foldl', isSuffixOf, partition, sortOn, zipWith4)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime, getCurrentTime, nominalDay)
import Data.Type.Equality
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4
import Simplex.Chat.Library.Subscriber
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Files
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.Operators
import Simplex.Chat.Options
import Simplex.Chat.ProfileGenerator (generateRandomProfile)
import Simplex.Chat.Protocol
import Simplex.Chat.Remote
import Simplex.Chat.Remote.Types
import Simplex.Chat.Library.Internal
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
import Simplex.Chat.Util (liftIOEither, zipWith3')
import qualified Simplex.Chat.Util as U
import Simplex.FileTransfer.Description (FileDescriptionURI (..), maxFileSize, maxFileSizeHard)
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..), ServerRoles (..), allRoles)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.Interface (execSQL)
import Simplex.Messaging.Agent.Store.Shared (upMigration)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (getCurrentMigrations)
import Simplex.Messaging.Client (NetworkConfig (..), SMPWebPortServers (..), SocksMode (SMAlways), textToHostMode)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..), pattern IKPQOff, pattern IKPQOn, pattern PQEncOff, pattern PQSupportOff, pattern PQSupportOn)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (base64P)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), AProtocolType (..), MsgFlags (..), NtfServer, ProtoServerWithAuth (..), ProtocolServer, ProtocolType (..), ProtocolTypeI (..), SProtocolType (..), SubscriptionMode (..), UserProtocol, userProtocol)
import qualified Simplex.Messaging.Protocol as SMP
import Simplex.Messaging.ServiceScheme (ServiceScheme (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (defaultSocksProxyWithAuth)
import Simplex.Messaging.Util
import Simplex.Messaging.Version
import Simplex.RemoteControl.Invitation (RCInvitation (..), RCSignedInvitation (..))
import Simplex.RemoteControl.Types (RCCtrlAddress (..))
import System.Exit (ExitCode, exitSuccess)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (Handle, IOMode (..))
import System.Random (randomRIO)
import UnliftIO.Async
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Directory
import qualified UnliftIO.Exception as E
import UnliftIO.IO (hClose)
import UnliftIO.STM
#if defined(dbPostgres)
import Data.Bifunctor (bimap, second)
import Simplex.Messaging.Agent.Client (SubInfo (..), getAgentQueuesInfo, getAgentWorkersDetails, getAgentWorkersSummary)
#else
import Data.Bifunctor (bimap, first, second)
import qualified Data.ByteArray as BA
import qualified Database.SQLite.Simple as SQL
import Simplex.Chat.Archive
import Simplex.Messaging.Agent.Client (SubInfo (..), agentClientStore, getAgentQueuesInfo, getAgentWorkersDetails, getAgentWorkersSummary)
import Simplex.Messaging.Agent.Store.Common (withConnection)
import Simplex.Messaging.Agent.Store.SQLite.DB (SlowQueryStats (..))
#endif

_defaultNtfServers :: [NtfServer]
_defaultNtfServers =
  [ -- "ntf://FB-Uop7RTaZZEG0ZLD2CIaTjsPh-Fw0zFAnb7QyA8Ks=@ntf2.simplex.im,5ex3mupcazy3zlky64ab27phjhijpemsiby33qzq3pliejipbtx5xgad.onion"
    "ntf://KmpZNNXiVZJx_G2T7jRUmDFxWXM3OAnunz3uLT0tqAA=@ntf3.simplex.im,pxculznuryunjdvtvh6s6szmanyadumpbmvevgdpe4wk5c65unyt4yid.onion",
    "ntf://CJ5o7X6fCxj2FFYRU2KuCo70y4jSqz7td2HYhLnXWbU=@ntf4.simplex.im,wtvuhdj26jwprmomnyfu5wfuq2hjkzfcc72u44vi6gdhrwxldt6xauad.onion"
  ]

maxImageSize :: Integer
maxImageSize = 261120 * 2 -- auto-receive on mobiles

imageExtensions :: [String]
imageExtensions = [".jpg", ".jpeg", ".png", ".gif"]

fixedImagePreview :: ImageData
fixedImagePreview = ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAKVJREFUeF7t1kENACEUQ0FQhnVQ9lfGO+xggITQdvbMzArPey+8fa3tAfwAEdABZQspQStgBssEcgAIkSAJkiAJljtEgiRIgmUCSZAESZAESZAEyx0iQRIkwTKBJEiCv5fgvTd1wDmn7QAP4AeIgA4oW0gJWgEzWCZwbQ7gAA7ggLKFOIADOKBMIAeAEAmSIAmSYLlDJEiCJFgmkARJkARJ8N8S/ADTZUewBvnTOQAAAABJRU5ErkJggg=="

imageFilePrefix :: String
imageFilePrefix = "IMG_"

voiceFilePrefix :: String
voiceFilePrefix = "voice_"

videoFilePrefix :: String
videoFilePrefix = "video_"

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
          void $ forkIO $ mapM_ startExpireCIs users
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
    startExpireCIs user = whenM shouldExpireChats $ do
      startExpireCIThread user
      setExpireCIFlag user True
      where
        shouldExpireChats = 
          fmap (fromRight False) $ runExceptT $ withStore' $ \db -> do
            ttl <- getChatItemTTL db user
            ttlCount <- getChatTTLCount db user
            pure $ ttl > 0 || ttlCount > 0

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
    flip catchChatError eToView $
      toView =<< receiveFileEvt' user ft False Nothing Nothing

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

updateNetworkConfig :: NetworkConfig -> SimpleNetCfg -> NetworkConfig
updateNetworkConfig cfg SimpleNetCfg {socksProxy, socksMode, hostMode, requiredHostMode, smpProxyMode_, smpProxyFallback_, smpWebPortServers, tcpTimeout_, logTLSErrors} =
  let cfg1 = maybe cfg (\smpProxyMode -> cfg {smpProxyMode}) smpProxyMode_
      cfg2 = maybe cfg1 (\smpProxyFallback -> cfg1 {smpProxyFallback}) smpProxyFallback_
      cfg3 = maybe cfg2 (\tcpTimeout -> cfg2 {tcpTimeout, tcpConnectTimeout = (tcpTimeout * 3) `div` 2}) tcpTimeout_
   in cfg3 {socksProxy, socksMode, hostMode, requiredHostMode, smpWebPortServers, logTLSErrors}

useServers :: Foldable f => RandomAgentServers -> [(Text, ServerOperator)] -> f UserOperatorServers -> (NonEmpty (ServerCfg 'PSMP), NonEmpty (ServerCfg 'PXFTP))
useServers as opDomains uss =
  let smp' = useServerCfgs SPSMP as opDomains $ concatMap (servers' SPSMP) uss
      xftp' = useServerCfgs SPXFTP as opDomains $ concatMap (servers' SPXFTP) uss
   in (smp', xftp')

execChatCommand :: Maybe RemoteHostId -> ByteString -> CM' (Either ChatError ChatResponse)
execChatCommand rh s =
  case parseChatCommand s of
    Left e -> pure $ chatCmdError e
    Right cmd -> case rh of
      Just rhId
        | allowRemoteCommand cmd -> execRemoteCommand rhId cmd s
        | otherwise -> pure $ Left $ ChatErrorRemoteHost (RHId rhId) $ RHELocalCommand
      _ -> do
        cc@ChatController {config = ChatConfig {chatHooks}} <- ask
        case preCmdHook chatHooks of
          Just hook -> liftIO (hook cc cmd) >>= either pure execChatCommand'
          Nothing -> execChatCommand' cmd

execChatCommand' :: ChatCommand -> CM' (Either ChatError ChatResponse)
execChatCommand' cmd = handleCommandError $ processChatCommand cmd

execRemoteCommand :: RemoteHostId -> ChatCommand -> ByteString -> CM' (Either ChatError ChatResponse)
execRemoteCommand rhId cmd s = handleCommandError $ getRemoteHostClient rhId >>= \rh -> processRemoteCommand rhId rh cmd s

handleCommandError :: CM ChatResponse -> CM' (Either ChatError ChatResponse)
handleCommandError a = runExceptT a `E.catches` ioErrors
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
#if !defined(dbPostgres)
  APIExportArchive cfg -> checkChatStopped $ CRArchiveExported <$> lift (exportArchive cfg)
  ExportArchive -> do
    ts <- liftIO getCurrentTime
    let filePath = "simplex-chat." <> formatTime defaultTimeLocale "%FT%H%M%SZ" ts <> ".zip"
    processChatCommand $ APIExportArchive $ ArchiveConfig filePath Nothing Nothing
  APIImportArchive cfg -> checkChatStopped $ do
    fileErrs <- lift $ importArchive cfg
    setStoreChanged
    pure $ CRArchiveImported fileErrs
  APIDeleteStorage -> withStoreChanged deleteStorage
  APIStorageEncryption cfg -> withStoreChanged $ sqlCipherExport cfg
  TestStorageEncryption key -> sqlCipherTestKey key >> ok_
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
#endif
  ExecChatStoreSQL query -> CRSQLResult <$> withStore' (`execSQL` query)
  ExecAgentStoreSQL query -> CRSQLResult <$> withAgent (`execAgentStoreSQL` query)
  APISaveAppSettings as -> withFastStore' (`saveAppSettings` as) >> ok_
  APIGetAppSettings platformDefaults -> CRAppSettings <$> withFastStore' (`getAppSettings` platformDefaults)
  APIGetChatTags userId -> withUserId' userId $ \user -> do
    tags <- withFastStore' (`getUserChatTags` user)
    pure $ CRChatTags user tags
  APIGetChats {userId, pendingConnections, pagination, query} -> withUserId' userId $ \user -> do
    (errs, previews) <- partitionEithers <$> withFastStore' (\db -> getChatPreviews db vr user pendingConnections pagination query)
    unless (null errs) $ toView $ CEvtChatErrors (map ChatErrorStore errs)
    pure $ CRApiChats user previews
  APIGetChat (ChatRef cType cId) contentFilter pagination search -> withUser $ \user -> case cType of
    -- TODO optimize queries calculating ChatStats, currently they're disabled
    CTDirect -> do
      when (isJust contentFilter) $ throwChatError $ CECommandError "content filter not supported"
      (directChat, navInfo) <- withFastStore (\db -> getDirectChat db vr user cId pagination search)
      pure $ CRApiChat user (AChat SCTDirect directChat) navInfo
    CTGroup -> do
      (groupChat, navInfo) <- withFastStore (\db -> getGroupChat db vr user cId contentFilter pagination search)
      pure $ CRApiChat user (AChat SCTGroup groupChat) navInfo
    CTLocal -> do
      when (isJust contentFilter) $ throwChatError $ CECommandError "content filter not supported"
      (localChat, navInfo) <- withFastStore (\db -> getLocalChat db user cId pagination search)
      pure $ CRApiChat user (AChat SCTLocal localChat) navInfo
    CTContactRequest -> throwCmdError "not implemented"
    CTContactConnection -> throwCmdError "not supported"
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
  APISendMessages sendRef live itemTTL cms -> withUser $ \user -> mapM_ assertAllowedContent' cms >> case sendRef of
    SRDirect chatId -> do
      mapM_ assertNoMentions cms
      withContactLock "sendMessage" chatId $
        sendContactContentMessages user chatId live itemTTL (L.map composedMessageReq cms)
    SRGroup chatId directMemId_ ->
      withGroupLock "sendMessage" chatId $ do
        (gInfo, cmrs) <- withFastStore $ \db -> do
          g <- getGroupInfo db vr user chatId
          (g,) <$> mapM (composedMessageReqMentions db user g) cms
        sendGroupContentMessages user gInfo directMemId_ live itemTTL cmrs
  APICreateChatTag (ChatTagData emoji text) -> withUser $ \user -> withFastStore' $ \db -> do
    _ <- createChatTag db user emoji text
    CRChatTags user <$> getUserChatTags db user
  APISetChatTags (ChatRef cType chatId) tagIds -> withUser $ \user -> case cType of
    CTDirect -> withFastStore' $ \db -> do
      updateDirectChatTags db chatId (maybe [] L.toList tagIds)
      CRTagsUpdated user <$> getUserChatTags db user <*> getDirectChatTags db chatId
    CTGroup -> withFastStore' $ \db -> do
      updateGroupChatTags db chatId (maybe [] L.toList tagIds)
      CRTagsUpdated user <$> getUserChatTags db user <*> getGroupChatTags db chatId
    _ -> throwCmdError "not supported"
  APIDeleteChatTag tagId -> withUser $ \user -> do
    withFastStore' $ \db -> deleteChatTag db user tagId
    ok user
  APIUpdateChatTag tagId (ChatTagData emoji text) -> withUser $ \user -> do
    withFastStore' $ \db -> updateChatTag db user tagId emoji text
    ok user
  APIReorderChatTags tagIds -> withUser $ \user -> do
    withFastStore' $ \db -> reorderChatTags db user $ L.toList tagIds
    ok user
  APICreateChatItems folderId cms -> withUser $ \user -> do
    forM_ cms $ \cm -> assertAllowedContent' cm >> assertNoMentions cm
    createNoteFolderContentItems user folderId (L.map composedMessageReq cms)
  APIReportMessage gId reportedItemId reportReason reportText -> withUser $ \user ->
    withGroupLock "reportMessage" gId $ do
      (gInfo, ms) <-
        withFastStore $ \db -> do
          gInfo <- getGroupInfo db vr user gId
          (gInfo,) <$> liftIO (getGroupModerators db vr user gInfo)
      let ms' = filter compatibleModerator ms
          mc = MCReport reportText reportReason
          cm = ComposedMessage {fileSource = Nothing, quotedItemId = Just reportedItemId, msgContent = mc, mentions = M.empty}
      when (null ms') $ throwChatError $ CECommandError "no moderators support receiving reports"
      let numFileInvs = length $ filter memberCurrent ms'
      sendGroupContentMessages_ user gInfo Nothing ms' numFileInvs False Nothing [composedMessageReq cm]
    where
      compatibleModerator GroupMember {activeConn, memberChatVRange} =
        maxVersion (maybe memberChatVRange peerChatVRange activeConn) >= contentReportsVersion
  ReportMessage {groupName, contactName_, reportReason, reportedMessage} -> withUser $ \user -> do
    gId <- withFastStore $ \db -> getGroupIdByName db user groupName
    reportedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user gId contactName_ reportedMessage
    processChatCommand $ APIReportMessage gId reportedItemId reportReason ""
  APIUpdateChatItem (ChatRef cType chatId) itemId live (UpdatedMessage mc mentions) -> withUser $ \user -> assertAllowedContent mc >> case cType of
    CTDirect -> withContactLock "updateChatItem" chatId $ do
      unless (null mentions) $ throwChatError $ CECommandError "mentions are not supported in this chat"
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
                  (SndMessage {msgId}, _) <- sendDirectContactMessage user ct (XMsgUpdate itemSharedMId mc M.empty (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive))
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
      let (_, ft_) = msgContentTexts mc
      if prohibitedSimplexLinks gInfo membership ft_
        then throwCmdError ("feature not allowed " <> T.unpack (groupFeatureNameText GFSimplexLinks))
        else do
          cci <- withFastStore $ \db -> getGroupCIWithReactions db user gInfo itemId
          case cci of
            CChatItem SMDSnd ci@ChatItem {meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable}, content = ciContent} -> do
              case (ciContent, itemSharedMsgId, editable) of
                (CISndMsgContent oldMC, Just itemSharedMId, True) -> do
                  let changed = mc /= oldMC
                  if changed || fromMaybe False itemLive
                    then do
                      ciMentions <- withFastStore $ \db -> getCIMentions db user gInfo ft_ mentions
                      let mentions' = M.map (\CIMention {memberId} -> MsgMention {memberId}) ciMentions
                      -- TODO [knocking] send separately to pending approval member
                      SndMessage {msgId} <- sendGroupMessage user gInfo ms (XMsgUpdate itemSharedMId mc mentions' (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive))
                      ci' <- withFastStore' $ \db -> do
                        currentTs <- liftIO getCurrentTime
                        when changed $
                          addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
                        let edited = itemLive /= Just True
                        ci' <- updateGroupChatItem db user groupId ci (CISndMsgContent mc) edited live $ Just msgId
                        updateGroupCIMentions db gInfo ci' ciMentions
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId) ci ci'
                      pure $ CRChatItemUpdated user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci')
                    else pure $ CRChatItemNotChanged user (AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci)
                _ -> throwChatError CEInvalidChatItemUpdate
            CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTLocal -> do
      unless (null mentions) $ throwChatError $ CECommandError "mentions are not supported in this chat"
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
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
  APIDeleteChatItem (ChatRef cType chatId) itemIds mode -> withUser $ \user -> case cType of
    CTDirect -> withContactLock "deleteChatItem" chatId $ do
      (ct, items) <- getCommandDirectChatItems user chatId itemIds
      deletions <- case mode of
        CIDMInternal -> deleteDirectCIs user ct items
        CIDMInternalMark -> markDirectCIsDeleted user ct items =<< liftIO getCurrentTime
        CIDMBroadcast -> do
          assertDeletable items
          assertDirectAllowed user MDSnd ct XMsgDel_
          let msgIds = itemsMsgIds items
              events = map (\msgId -> XMsgDel msgId Nothing) msgIds
          forM_ (L.nonEmpty events) $ \events' ->
            sendDirectContactMessages user ct events'
          if featureAllowed SCFFullDelete forUser ct
            then deleteDirectCIs user ct items
            else markDirectCIsDeleted user ct items =<< liftIO getCurrentTime
      pure $ CRChatItemsDeleted user deletions True False
    CTGroup -> withGroupLock "deleteChatItem" chatId $ do
      (gInfo, items) <- getCommandGroupChatItems user chatId itemIds
      deletions <- case mode of
        CIDMInternal -> deleteGroupCIs user gInfo items Nothing =<< liftIO getCurrentTime
        CIDMInternalMark -> markGroupCIsDeleted user gInfo items Nothing =<< liftIO getCurrentTime
        CIDMBroadcast -> do
          ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
          assertDeletable items
          assertUserGroupRole gInfo GRObserver -- can still delete messages sent earlier
          let msgIds = itemsMsgIds items
              events = L.nonEmpty $ map (`XMsgDel` Nothing) msgIds
          -- TODO [knocking] validate: only current members or only single pending approval member
          mapM_ (sendGroupMessages user gInfo ms) events
          delGroupChatItems user gInfo items False
      pure $ CRChatItemsDeleted user deletions True False
    CTLocal -> do
      (nf, items) <- getCommandLocalChatItems user chatId itemIds
      deleteLocalCIs user nf items True False
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
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
    (gInfo, items) <- getCommandGroupChatItems user gId itemIds
    ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
    deletions <- delGroupChatItemsForMembers user gInfo ms items
    pure $ CRChatItemsDeleted user deletions True False
  APIArchiveReceivedReports gId -> withUser $ \user -> withFastStore $ \db -> do
    g <- getGroupInfo db vr user gId
    deleteTs <- liftIO getCurrentTime
    ciIds <- liftIO $ markReceivedGroupReportsDeleted db user g deleteTs
    pure $ CRGroupChatItemsDeleted user g ciIds True (Just $ membership g)
  APIDeleteReceivedReports gId itemIds mode -> withUser $ \user -> withGroupLock "deleteReports" gId $ do
    (gInfo, items) <- getCommandGroupChatItems user gId itemIds
    unless (all isRcvReport items) $ throwChatError $ CECommandError "some items are not received reports"
    deletions <- case mode of
      CIDMInternal -> deleteGroupCIs user gInfo items Nothing =<< liftIO getCurrentTime
      CIDMInternalMark -> markGroupCIsDeleted user gInfo items Nothing =<< liftIO getCurrentTime
      CIDMBroadcast -> do
        ms <- withFastStore' $ \db -> getGroupModerators db vr user gInfo
        delGroupChatItemsForMembers user gInfo ms items
    pure $ CRChatItemsDeleted user deletions True False
    where
      isRcvReport = \case
        CChatItem _ ChatItem {content = CIRcvMsgContent (MCReport {})} -> True
        _ -> False
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
      withGroupLock "chatItemReaction" chatId $ do
        (Group g@GroupInfo {membership} ms, CChatItem md ci) <- withFastStore $ \db -> do
          gr@(Group g _) <- getGroup db vr user chatId
          (gr,) <$> getGroupCIWithReactions db user g itemId
        case ci of
          ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}} -> do
            unless (groupFeatureAllowed SGFReactions g) $
              throwChatError (CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions))
            unless (ciReactionAllowed ci) $
              throwChatError (CECommandError "reaction not allowed - chat item has no content")
            let GroupMember {memberId = itemMemberId} = chatItemMember g ci
            rs <- withFastStore' $ \db -> getGroupReactions db g membership itemMemberId itemSharedMId True
            checkReactionAllowed rs
            -- TODO [knocking] send separately to pending approval member
            SndMessage {msgId} <- sendGroupMessage user g ms (XMsgReact itemSharedMId (Just itemMemberId) reaction add)
            createdAt <- liftIO getCurrentTime
            reactions <- withFastStore' $ \db -> do
              setGroupReaction db g membership itemMemberId itemSharedMId True reaction add msgId createdAt
              liftIO $ getGroupCIReactions db g itemMemberId itemSharedMId
            let ci' = CChatItem md ci {reactions}
                r = ACIReaction SCTGroup SMDSnd (GroupChat g) $ CIReaction CIGroupSnd ci' createdAt reaction
            pure $ CRChatItemReaction user add r
          _ -> throwChatError $ CECommandError "reaction not possible - no shared item ID"
    CTLocal -> throwCmdError "not supported"
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
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
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
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
  APIForwardChatItems toChat@(ChatRef toCType toChatId) fromChat@(ChatRef fromCType fromChatId) itemIds itemTTL -> withUser $ \user -> case toCType of
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
          withGroupLock "forwardChatItem, to group" toChatId $ do
            gInfo <- withFastStore $ \db -> getGroupInfo db vr user toChatId
            sendGroupContentMessages user gInfo Nothing False itemTTL cmrs'
        Nothing -> pure $ CRNewChatItems user []
    CTLocal -> do
      cmrs <- prepareForward user
      case L.nonEmpty cmrs of
        Just cmrs' ->
          createNoteFolderContentItems user toChatId cmrs'
        Nothing -> pure $ CRNewChatItems user []
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
    where
      prepareForward :: User -> CM [ComposedMessageReq]
      prepareForward user = case fromCType of
        CTDirect -> withContactLock "forwardChatItem, from contact" fromChatId $ do
          (ct, items) <- getCommandDirectChatItems user fromChatId itemIds
          catMaybes <$> mapM (\ci -> ciComposeMsgReq ct ci <$$> prepareMsgReq ci) items
          where
            ciComposeMsgReq :: Contact -> CChatItem 'CTDirect -> (MsgContent, Maybe CryptoFile) -> ComposedMessageReq
            ciComposeMsgReq ct (CChatItem md ci) (mc', file) =
              let itemId = chatItemId' ci
                  ciff = forwardCIFF ci $ Just (CIFFContact (forwardName ct) (toMsgDirection md) (Just fromChatId) (Just itemId))
               in (composedMessage file mc', ciff, msgContentTexts mc', M.empty)
              where
                forwardName :: Contact -> ContactName
                forwardName Contact {profile = LocalProfile {displayName, localAlias}}
                  | localAlias /= "" = localAlias
                  | otherwise = displayName
        CTGroup -> withGroupLock "forwardChatItem, from group" fromChatId $ do
          (gInfo, items) <- getCommandGroupChatItems user fromChatId itemIds
          catMaybes <$> mapM (\ci -> ciComposeMsgReq gInfo ci <$$> prepareMsgReq ci) items
          where
            ciComposeMsgReq :: GroupInfo -> CChatItem 'CTGroup -> (MsgContent, Maybe CryptoFile) -> ComposedMessageReq
            ciComposeMsgReq gInfo (CChatItem md ci@ChatItem {mentions, formattedText}) (mc, file) = do
              let itemId = chatItemId' ci
                  ciff = forwardCIFF ci $ Just (CIFFGroup (forwardName gInfo) (toMsgDirection md) (Just fromChatId) (Just itemId))
                  -- updates text to reflect current mentioned member names
                  (mc', _, mentions') = updatedMentionNames mc formattedText mentions
                  -- only includes mentions when forwarding to the same group
                  ciMentions = if toChat == fromChat then mentions' else M.empty
                  -- no need to have mentions in ComposedMessage, they are in ciMentions
               in (ComposedMessage file Nothing mc' M.empty, ciff, msgContentTexts mc', ciMentions)
              where
                forwardName :: GroupInfo -> ContactName
                forwardName GroupInfo {groupProfile = GroupProfile {displayName}} = displayName
        CTLocal -> do
          (_, items) <- getCommandLocalChatItems user fromChatId itemIds
          catMaybes <$> mapM (\ci -> ciComposeMsgReq ci <$$> prepareMsgReq ci) items
          where
            ciComposeMsgReq :: CChatItem 'CTLocal -> (MsgContent, Maybe CryptoFile) -> ComposedMessageReq
            ciComposeMsgReq (CChatItem _ ci) (mc', file) =
              let ciff = forwardCIFF ci Nothing
               in (composedMessage file mc', ciff, msgContentTexts mc', M.empty)
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
                            newFileName <- liftIO $ maybe (pure fileName) (generateNewFileName fileName) $ mediaFilePrefix mc
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
          mediaFilePrefix :: MsgContent -> Maybe FilePath
          mediaFilePrefix = \case
            MCImage {} -> Just imageFilePrefix
            MCVoice {} -> Just voiceFilePrefix
            MCVideo {} -> Just videoFilePrefix
            _ -> Nothing
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
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
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
    CTLocal -> throwCmdError "not supported"
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
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
    _ -> throwCmdError "not supported"
  APIDeleteChat cRef@(ChatRef cType chatId) cdm -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> getContact db vr user chatId
      filesInfo <- withFastStore' $ \db -> getContactFileInfo db user ct
      withContactLock "deleteChat direct" chatId . procCmd $
        case cdm of
          CDMFull notify -> do
            deleteCIFiles user filesInfo
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
          deleteAgentConnectionsAsync' contactConnIds doSendDel
    CTContactConnection -> withConnectionLock "deleteChat contactConnection" chatId . procCmd $ do
      conn@PendingContactConnection {pccAgentConnId = AgentConnId acId} <- withFastStore $ \db -> getPendingContactConnection db userId chatId
      deleteAgentConnectionAsync acId
      withFastStore' $ \db -> deletePendingContactConnection db userId chatId
      pure $ CRContactConnectionDeleted user conn
    CTGroup -> do
      Group gInfo@GroupInfo {membership} members <- withFastStore $ \db -> getGroup db vr user chatId
      let GroupMember {memberRole = membershipMemRole} = membership
      let isOwner = membershipMemRole == GROwner
          canDelete = isOwner || not (memberCurrent membership)
      unless canDelete $ throwChatError $ CEGroupUserRole gInfo GROwner
      filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
      withGroupLock "deleteChat group" chatId . procCmd $ do
        deleteCIFiles user filesInfo
        let doSendDel = memberActive membership && isOwner
        -- TODO [knocking] send to pending approval members (move `memberCurrent` filter from sendGroupMessages_ to call sites)
        when doSendDel . void $ sendGroupMessage' user gInfo members XGrpDel
        deleteGroupLinkIfExists user gInfo
        deleteMembersConnections' user members doSendDel
        updateCIGroupInvitationStatus user gInfo CIGISRejected `catchChatError` \_ -> pure ()
        withFastStore' $ \db -> deleteGroupChatItems db user gInfo
        withFastStore' $ \db -> cleanupHostGroupLinkConn db user gInfo
        withFastStore' $ \db -> deleteGroupMembers db user gInfo
        withFastStore' $ \db -> deleteGroup db user gInfo
        pure $ CRGroupDeletedUser user gInfo
    CTLocal -> throwCmdError "not supported"
    CTContactRequest -> throwCmdError "not supported"
  APIClearChat (ChatRef cType chatId) -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> getContact db vr user chatId
      filesInfo <- withFastStore' $ \db -> getContactFileInfo db user ct
      deleteCIFiles user filesInfo
      withFastStore' $ \db -> deleteContactCIs db user ct
      pure $ CRChatCleared user (AChatInfo SCTDirect $ DirectChat ct)
    CTGroup -> do
      gInfo <- withFastStore $ \db -> getGroupInfo db vr user chatId
      filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
      deleteCIFiles user filesInfo
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
    CTContactConnection -> throwCmdError "not supported"
    CTContactRequest -> throwCmdError "not supported"
  APIAcceptContact incognito connReqId -> withUser $ \_ -> do
    userContactLinkId <- withFastStore $ \db -> getUserContactLinkIdByCReq db connReqId
    withUserContactLock "acceptContact" userContactLinkId $ do
      (user@User {userId}, cReq) <- withFastStore $ \db -> getContactRequest' db connReqId
      (ct, conn@Connection {connId}, sqSecured) <- acceptContactRequest user cReq incognito
      ucl <- withFastStore $ \db -> getUserContactLinkById db userId userContactLinkId
      let contactUsed = (\(_, gLinkInfo_) -> isNothing gLinkInfo_) ucl
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
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]
          ok user
      else throwCmdError ("feature not allowed " <> T.unpack (chatFeatureNameText CFCalls))
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
  APISetGroupAlias gId localAlias -> withUser $ \user@User {userId} -> do
    gInfo' <- withFastStore $ \db -> do
      gInfo <- getGroupInfo db vr user gId
      liftIO $ updateGroupAlias db userId gInfo localAlias
    pure $ CRGroupAliasUpdated user gInfo'
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
    _ -> throwCmdError "not supported"
  APIGetNtfToken -> withUser' $ \_ -> crNtfToken <$> withAgent getNtfToken
  APIRegisterToken token mode -> withUser $ \_ ->
    CRNtfTokenStatus <$> withAgent (\a -> registerNtfToken a token mode)
  APIVerifyToken token nonce code -> withUser $ \_ -> withAgent (\a -> verifyNtfToken a token nonce code) >> ok_
  APICheckToken token -> withUser $ \_ ->
    CRNtfTokenStatus <$> withAgent (`checkNtfToken` token)
  APIDeleteToken token -> withUser $ \_ -> withAgent (`deleteNtfToken` token) >> ok_
  APIGetNtfConns nonce encNtfInfo -> withUser $ \_ -> do
    ntfInfos <- withAgent $ \a -> getNotificationConns a nonce encNtfInfo
    (errs, ntfMsgs) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (getMsgConn db) (L.toList ntfInfos))
    unless (null errs) $ toView $ CEvtChatErrors errs
    pure $ CRNtfConns $ catMaybes ntfMsgs
    where
      getMsgConn :: DB.Connection -> NotificationInfo -> IO (Maybe NtfConn)
      getMsgConn db NotificationInfo {ntfConnId, ntfDbQueueId, ntfMsgMeta = nMsgMeta} = do
        let agentConnId = AgentConnId ntfConnId
            mkNtfConn user connEntity = NtfConn {user, agentConnId, agentDbQueueId = ntfDbQueueId, connEntity, expectedMsg_ = expectedMsgInfo <$> nMsgMeta}
        getUserByAConnId db agentConnId
          $>>= \user -> fmap (mkNtfConn user) . eitherToMaybe <$> runExceptT (getConnectionEntity db vr user agentConnId)
  APIGetConnNtfMessages connMsgs -> withUser $ \_ -> do
    msgs <- lift $ withAgent' (`getConnectionMessages` connMsgs)
    let ntfMsgs = L.map receivedMsgInfo msgs
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
  APISetChatTTL userId (ChatRef cType chatId) newTTL_ ->
    withUserId userId $ \user -> checkStoreNotChanged $ withChatLock "setChatTTL" $ do
      (oldTTL_, globalTTL, ttlCount) <- withStore' $ \db ->
        (,,) <$> getSetChatTTL db <*> getChatItemTTL db user <*> getChatTTLCount db user
      let newTTL = fromMaybe globalTTL newTTL_
          oldTTL = fromMaybe globalTTL oldTTL_
      when (newTTL > 0 && (newTTL < oldTTL || oldTTL == 0)) $ do
        lift $ setExpireCIFlag user False
        expireChat user globalTTL `catchChatError` eToView
      lift $ setChatItemsExpiration user globalTTL ttlCount
      ok user
    where
      getSetChatTTL db = case cType of
        CTDirect -> getDirectChatTTL db chatId <* setDirectChatTTL db chatId newTTL_
        CTGroup -> getGroupChatTTL db chatId <* setGroupChatTTL db chatId newTTL_
        _ -> pure Nothing
      expireChat user globalTTL = do
        currentTs <- liftIO getCurrentTime
        case cType of
          CTDirect -> expireContactChatItems user vr globalTTL chatId
          CTGroup ->
            let createdAtCutoff = addUTCTime (-43200 :: NominalDiffTime) currentTs
              in expireGroupChatItems user vr globalTTL createdAtCutoff chatId
          _ -> throwChatError $ CECommandError "not supported"
  SetChatTTL chatName newTTL -> withUser' $ \user@User {userId} -> do
    chatRef <- getChatRef user chatName
    processChatCommand $ APISetChatTTL userId chatRef newTTL
  GetChatTTL chatName -> withUser' $ \user -> do
    ChatRef cType chatId <- getChatRef user chatName
    ttl <- case cType of
      CTDirect -> withFastStore' (`getDirectChatTTL` chatId)
      CTGroup -> withFastStore' (`getGroupChatTTL` chatId)
      _ -> throwChatError $ CECommandError "not supported"
    pure $ CRChatItemTTL user ttl
  APISetChatItemTTL userId newTTL -> withUserId userId $ \user ->
    checkStoreNotChanged $
      withChatLock "setChatItemTTL" $ do
        (oldTTL, ttlCount) <- withFastStore' $ \db ->
          (,) <$> getChatItemTTL db user <* setChatItemTTL db user newTTL <*> getChatTTLCount db user
        when (newTTL > 0 && (newTTL < oldTTL || oldTTL == 0)) $ do
          lift $ setExpireCIFlag user False
          expireChatItems user newTTL True
        lift $ setChatItemsExpiration user newTTL ttlCount
        ok user
  SetChatItemTTL newTTL_ -> withUser' $ \User {userId} -> do
    processChatCommand $ APISetChatItemTTL userId newTTL_
  APIGetChatItemTTL userId -> withUserId' userId $ \user -> do
    ttl <- withFastStore' (`getChatItemTTL` user)
    pure $ CRChatItemTTL user (Just ttl)
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
        withAgent (\a -> toggleConnectionNtfs a connId $ chatHasNtfs chatSettings) `catchChatError` eToView
      ok user
    _ -> throwCmdError "not supported"
  APISetMemberSettings gId gMemberId settings -> withUser $ \user -> do
    m <- withFastStore $ \db -> do
      liftIO $ updateGroupMemberSettings db user gId gMemberId settings
      getGroupMember db vr user gId gMemberId
    let ntfOn = showMessages $ memberSettings m
    toggleNtf m ntfOn
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
  APIAddContact userId short incognito -> withUserId userId $ \user -> procCmd $ do
    -- [incognito] generate profile for connection
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    subMode <- chatReadVar subscriptionMode
    let userData = shortLinkUserData short
    (connId, ccLink) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation userData Nothing IKPQOn subMode
    ccLink' <- shortenCreatedLink ccLink
    -- TODO PQ pass minVersion from the current range
    conn <- withFastStore' $ \db -> createDirectConnection db user connId ccLink' ConnNew incognitoProfile subMode initialChatVersion PQSupportOn
    pure $ CRInvitation user ccLink' conn
  AddContact short incognito -> withUser $ \User {userId} ->
    processChatCommand $ APIAddContact userId short incognito
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
    let PendingContactConnection {pccConnStatus, connLinkInv} = conn
    case (pccConnStatus, connLinkInv) of
      (ConnNew, Just (CCLink cReqInv _)) -> do
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
      recreateConn user conn@PendingContactConnection {customUserProfileId, connLinkInv} newUser = do
        subMode <- chatReadVar subscriptionMode
        let userData = shortLinkUserData $ isJust $ connShortLink =<< connLinkInv
        (agConnId, ccLink) <- withAgent $ \a -> createConnection a (aUserId newUser) True SCMInvitation userData Nothing IKPQOn subMode
        ccLink' <- shortenCreatedLink ccLink
        conn' <- withFastStore' $ \db -> do
          deleteConnectionRecord db user connId
          forM_ customUserProfileId $ \profileId ->
            deletePCCIncognitoProfile db user profileId
          createDirectConnection db newUser agConnId ccLink' ConnNew Nothing subMode initialChatVersion PQSupportOn
        deleteAgentConnectionAsync (aConnId' conn)
        pure conn'
  APIConnectPlan userId cLink -> withUserId userId $ \user ->
    uncurry (CRConnectionPlan user) <$> connectPlan user cLink
  APIConnect userId incognito (Just (ACCL SCMInvitation (CCLink cReq@(CRInvitationUri crData e2e) sLnk_))) -> withUserId userId $ \user -> withInvitationLock "connect" (strEncode cReq) . procCmd $ do
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
            let ccLink = CCLink cReq $ serverShortLink <$> sLnk_
            pcc <- withFastStore' $ \db -> createDirectConnection db user connId ccLink ConnPrepared (incognitoProfile $> profileToSend) subMode chatV pqSup'
            joinPreparedConn connId pcc dm
          joinPreparedConn connId pcc@PendingContactConnection {pccConnId} dm = do
            void $ withAgent $ \a -> joinConnection a (aUserId user) connId True cReq dm pqSup' subMode
            withFastStore' $ \db -> updateConnectionStatusFromTo db pccConnId ConnPrepared ConnJoined
            pure $ CRSentConfirmation user pcc {pccConnStatus = ConnJoined}
          cReqs =
            ( CRInvitationUri crData {crScheme = SSSimplex} e2e,
              CRInvitationUri crData {crScheme = simplexChat} e2e
            )
  APIConnect userId incognito (Just (ACCL SCMContact ccLink)) -> withUserId userId $ \user -> connectViaContact user incognito ccLink
  APIConnect _ _ Nothing -> throwChatError CEInvalidConnReq
  Connect incognito (Just cLink@(ACL m cLink')) -> withUser $ \user -> do
    (ccLink, plan) <- connectPlan user cLink `catchChatError` \e -> case cLink' of CLFull cReq -> pure (ACCL m (CCLink cReq Nothing), CPInvitationLink ILPOk); _ -> throwError e
    connectWithPlan user incognito ccLink plan
  Connect _ Nothing -> throwChatError CEInvalidConnReq
  APIConnectContactViaAddress userId incognito contactId -> withUserId userId $ \user -> do
    ct@Contact {activeConn, profile = LocalProfile {contactLink}} <- withFastStore $ \db -> getContact db vr user contactId
    when (isJust activeConn) $ throwChatError (CECommandError "contact already has connection")
    ccLink <- case contactLink of
      Just (CLFull cReq) -> pure $ CCLink cReq Nothing
      Just (CLShort sLnk) -> do
        cReq <- getShortLinkConnReq user sLnk
        pure $ CCLink cReq $ Just sLnk
      Nothing -> throwChatError (CECommandError "no address in contact profile")
    connectContactViaAddress user incognito ct ccLink
  ConnectSimplex incognito -> withUser $ \user -> do
    plan <- contactRequestPlan user adminContactReq `catchChatError` const (pure $ CPContactAddress CAPOk)
    connectWithPlan user incognito (ACCL SCMContact (CCLink adminContactReq Nothing)) plan
  DeleteContact cName cdm -> withContactName cName $ \ctId -> APIDeleteChat (ChatRef CTDirect ctId) cdm
  ClearContact cName -> withContactName cName $ APIClearChat . ChatRef CTDirect
  APIListContacts userId -> withUserId userId $ \user ->
    CRContactsList user <$> withFastStore' (\db -> getUserContacts db vr user)
  ListContacts -> withUser $ \User {userId} ->
    processChatCommand $ APIListContacts userId
  APICreateMyAddress userId short -> withUserId userId $ \user -> procCmd $ do
    subMode <- chatReadVar subscriptionMode
    let userData = shortLinkUserData short
    (connId, ccLink) <- withAgent $ \a -> createConnection a (aUserId user) True SCMContact userData Nothing IKPQOn subMode
    ccLink' <- shortenCreatedLink ccLink
    withFastStore $ \db -> createUserContactLink db user connId ccLink' subMode
    pure $ CRUserContactLinkCreated user ccLink'
  CreateMyAddress short -> withUser $ \User {userId} ->
    processChatCommand $ APICreateMyAddress userId short
  APIDeleteMyAddress userId -> withUserId userId $ \user@User {profile = p} -> do
    conns <- withFastStore $ \db -> getUserAddressConnections db vr user
    withChatLock "deleteMyAddress" $ do
      deleteAgentConnectionsAsync $ map aConnId conns
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
    ucl@UserContactLink {connLinkContact = CCLink cReq _} <- withFastStore (`getUserAddress` user)
    -- TODO [short links] replace with short links
    let p' = (fromLocalProfile p :: Profile) {contactLink = Just $ CLFull cReq}
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
            let sendRef = SRDirect ctId
            processChatCommand $ APISendMessages sendRef False Nothing [composedMessage Nothing mc]
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
        (gId, mentions) <- withFastStore $ \db -> do
          gId <- getGroupIdByName db user name
          (gId,) <$> liftIO (getMessageMentions db user gId msg)
        let sendRef = SRGroup gId Nothing
        processChatCommand $ APISendMessages sendRef False Nothing [ComposedMessage Nothing Nothing mc mentions]
      CTLocal
        | name == "" -> do
            folderId <- withFastStore (`getUserNoteFolderId` user)
            processChatCommand $ APICreateChatItems folderId [composedMessage Nothing mc]
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
        toView $ CEvtNoMemberContactCreating user g m
        processChatCommand (APICreateMemberContact gId mId) >>= \case
          CRNewMemberContact _ ct@Contact {contactId} _ _ -> do
            toViewTE $ TENewMemberContact user ct g m
            processChatCommand $ APISendMemberContactInvitation contactId (Just mc)
          cr -> pure cr
      Just ctId -> do
        let sendRef = SRDirect ctId
        processChatCommand $ APISendMessages sendRef False Nothing [composedMessage Nothing mc]
  SendLiveMessage chatName msg -> withUser $ \user -> do
    (chatRef, mentions) <- getChatRefAndMentions user chatName msg
    withSendRef chatRef $ \sendRef -> do
      let mc = MCText msg
      processChatCommand $ APISendMessages sendRef True Nothing [ComposedMessage Nothing Nothing mc mentions]
  SendMessageBroadcast mc -> withUser $ \user -> do
    contacts <- withFastStore' $ \db -> getUserContacts db vr user
    withChatLock "sendMessageBroadcast" . procCmd $ do
      let ctConns_ = L.nonEmpty $ foldr addContactConn [] contacts
      case ctConns_ of
        Nothing -> do
          timestamp <- liftIO getCurrentTime
          pure CRBroadcastSent {user, msgContent = mc, successes = 0, failures = 0, timestamp}
        Just (ctConns :: NonEmpty (Contact, Connection)) -> do
          let idsEvts = L.map ctSndEvent ctConns
          -- TODO Broadcast rework
          -- In createNewSndMessage and encodeChatMessage we could use Nothing for sharedMsgId,
          -- then we could reuse message body across broadcast.
          -- Encoding different sharedMsgId and reusing body is meaningless as referencing will not work anyway.
          -- As an improvement, single message record with its sharedMsgId could be created for new "broadcast" entity.
          -- Then all recipients could refer to broadcast message using same sharedMsgId.
          sndMsgs <- lift $ createSndMessages idsEvts
          let msgReqs_ :: NonEmpty (Either ChatError ChatMsgReq) = L.zipWith (fmap . ctMsgReq) ctConns sndMsgs
          (errs, ctSndMsgs :: [(Contact, SndMessage)]) <-
            partitionEithers . L.toList . zipWith3' combineResults ctConns sndMsgs <$> deliverMessagesB msgReqs_
          timestamp <- liftIO getCurrentTime
          lift . void $ withStoreBatch' $ \db -> map (createCI db user timestamp) ctSndMsgs
          pure CRBroadcastSent {user, msgContent = mc, successes = length ctSndMsgs, failures = length errs, timestamp}
    where
      addContactConn :: Contact -> [(Contact, Connection)] -> [(Contact, Connection)]
      addContactConn ct ctConns = case contactSendConn_ ct of
        Right conn | directOrUsed ct -> (ct, conn) : ctConns
        _ -> ctConns
      ctSndEvent :: (Contact, Connection) -> (ConnOrGroupId, ChatMsgEvent 'Json)
      ctSndEvent (_, Connection {connId}) = (ConnectionId connId, XMsgNew $ MCSimple (extMsgContent mc Nothing))
      ctMsgReq :: (Contact, Connection) -> SndMessage -> ChatMsgReq
      ctMsgReq (_, conn) SndMessage {msgId, msgBody} = (conn, MsgFlags {notification = hasNotification XMsgNew_}, (vrValue msgBody, [msgId]))
      combineResults :: (Contact, Connection) -> Either ChatError SndMessage -> Either ChatError ([Int64], PQEncryption) -> Either ChatError (Contact, SndMessage)
      combineResults (ct, _) (Right msg') (Right _) = Right (ct, msg')
      combineResults _ (Left e) _ = Left e
      combineResults _ _ (Left e) = Left e
      createCI :: DB.Connection -> User -> UTCTime -> (Contact, SndMessage) -> IO ()
      createCI db user createdAt (ct, sndMsg) =
        void $ createNewSndChatItem db user (CDDirectSnd ct) Nothing sndMsg (CISndMsgContent mc) Nothing Nothing Nothing False createdAt
  SendMessageQuote cName (AMsgDirection msgDir) quotedMsg msg -> withUser $ \user@User {userId} -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user cName
    quotedItemId <- withFastStore $ \db -> getDirectChatItemIdByText db userId contactId msgDir quotedMsg
    let mc = MCText msg
    processChatCommand $ APISendMessages (SRDirect contactId) False Nothing [ComposedMessage Nothing (Just quotedItemId) mc M.empty]
  DeleteMessage chatName deletedMsg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    deletedItemId <- getSentChatItemIdByText user chatRef deletedMsg
    processChatCommand $ APIDeleteChatItem chatRef (deletedItemId :| []) CIDMBroadcast
  DeleteMemberMessage gName mName deletedMsg -> withUser $ \user -> do
    gId <- withFastStore $ \db -> getGroupIdByName db user gName
    deletedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user gId (Just mName) deletedMsg
    processChatCommand $ APIDeleteMemberChatItem gId (deletedItemId :| [])
  EditMessage chatName editedMsg msg -> withUser $ \user -> do
    (chatRef, mentions) <- getChatRefAndMentions user chatName msg
    editedItemId <- getSentChatItemIdByText user chatRef editedMsg
    let mc = MCText msg
    processChatCommand $ APIUpdateChatItem chatRef editedItemId False $ UpdatedMessage mc mentions
  UpdateLiveMessage chatName chatItemId live msg -> withUser $ \user -> do
    (chatRef, mentions) <- getChatRefAndMentions user chatName msg
    let mc = MCText msg
    processChatCommand $ APIUpdateChatItem chatRef chatItemId live $ UpdatedMessage mc mentions
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
        (agentConnId, CCLink cReq _) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing Nothing IKPQOff subMode
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
  APIJoinGroup groupId enableNtfs -> withUser $ \user@User {userId} -> do
    withGroupLock "joinGroup" groupId . procCmd $ do
      (invitation, ct) <- withFastStore $ \db -> do
        inv@ReceivedGroupInvitation {fromMember} <- getGroupInvitation db vr user groupId
        (inv,) <$> getContactViaMember db vr user fromMember
      let ReceivedGroupInvitation {fromMember, connRequest, groupInfo = g@GroupInfo {membership, chatSettings}} = invitation
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
            -- MFAll is default for new groups
            unless (enableNtfs == MFAll) $ updateGroupSettings db user groupId chatSettings {enableNtfs}
          void (withAgent $ \a -> joinConnection a (aUserId user) agentConnId (enableNtfs /= MFNone) connRequest dm PQSupportOff subMode)
            `catchChatError` \e -> do
              withFastStore' $ \db -> do
                updateGroupMemberStatus db userId fromMember GSMemInvited
                updateGroupMemberStatus db userId membership GSMemInvited
              throwError e
          updateCIGroupInvitationStatus user g CIGISAccepted `catchChatError` eToView
          pure $ CRUserAcceptedGroupSent user g {membership = membership {memberStatus = GSMemAccepted}} Nothing
        Nothing -> throwChatError $ CEContactNotActive ct
  APIAcceptMember groupId gmId role -> withUser $ \user -> do
    (gInfo, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user groupId <*> getGroupMemberById db vr user gmId
    assertUserGroupRole gInfo GRAdmin
    when (memberStatus m /= GSMemPendingApproval) $ throwChatError $ CECommandError "member is not pending approval"
    case memberConn m of
      Just mConn -> do
        let msg = XGrpLinkAcpt role
        void $ sendDirectMemberMessage mConn msg groupId
        m' <- withFastStore' $ \db -> updateGroupMemberAccepted db user m role
        introduceToGroup vr user gInfo m'
        pure $ CRJoinedGroupMember user gInfo m'
      _ -> throwChatError CEGroupMemberNotActive
  APIMembersRole groupId memberIds newRole -> withUser $ \user ->
    withGroupLock "memberRole" groupId . procCmd $ do
      g@(Group gInfo members) <- withFastStore $ \db -> getGroup db vr user groupId
      when (selfSelected gInfo) $ throwChatError $ CECommandError "can't change role for self"
      let (invitedMems, currentMems, unchangedMems, maxRole, anyAdmin, anyPending) = selectMembers members
      when (length invitedMems + length currentMems + length unchangedMems /= length memberIds) $ throwChatError CEGroupMemberNotFound
      when (length memberIds > 1 && (anyAdmin || newRole >= GRAdmin)) $
        throwChatError $ CECommandError "can't change role of multiple members when admins selected, or new role is admin"
      when anyPending $ throwChatError $ CECommandError "can't change role of members pending approval"
      assertUserGroupRole gInfo $ maximum ([GRAdmin, maxRole, newRole] :: [GroupMemberRole])
      (errs1, changed1) <- changeRoleInvitedMems user gInfo invitedMems
      (errs2, changed2, acis) <- changeRoleCurrentMems user g currentMems
      unless (null acis) $ toView $ CEvtNewChatItems user acis
      let errs = errs1 <> errs2
      unless (null errs) $ toView $ CEvtChatErrors errs
      pure $ CRMembersRoleUser {user, groupInfo = gInfo, members = changed1 <> changed2, toRole = newRole} -- same order is not guaranteed
    where
      selfSelected GroupInfo {membership} = elem (groupMemberId' membership) memberIds
      selectMembers :: [GroupMember] -> ([GroupMember], [GroupMember], [GroupMember], GroupMemberRole, Bool, Bool)
      selectMembers = foldr' addMember ([], [], [], GRObserver, False, False)
        where
          addMember m@GroupMember {groupMemberId, memberStatus, memberRole} (invited, current, unchanged, maxRole, anyAdmin, anyPending)
            | groupMemberId `elem` memberIds =
                let maxRole' = max maxRole memberRole
                    anyAdmin' = anyAdmin || memberRole >= GRAdmin
                    anyPending' = anyPending || memberStatus == GSMemPendingApproval
                 in
                  if
                    | memberRole == newRole -> (invited, current, m : unchanged, maxRole', anyAdmin', anyPending')
                    | memberStatus == GSMemInvited -> (m : invited, current, unchanged, maxRole', anyAdmin', anyPending')
                    | otherwise -> (invited, m : current, unchanged, maxRole', anyAdmin', anyPending')
            | otherwise = (invited, current, unchanged, maxRole, anyAdmin, anyPending)
      changeRoleInvitedMems :: User -> GroupInfo -> [GroupMember] -> CM ([ChatError], [GroupMember])
      changeRoleInvitedMems user gInfo memsToChange = do
        -- not batched, as we need to send different invitations to different connections anyway
        mems_ <- forM memsToChange $ \m -> (Right <$> changeRole m) `catchChatError` (pure . Left)
        pure $ partitionEithers mems_
        where
          changeRole :: GroupMember -> CM GroupMember
          changeRole m@GroupMember {groupMemberId, memberContactId, localDisplayName = cName} = do
            withFastStore (\db -> (,) <$> mapM (getContact db vr user) memberContactId <*> liftIO (getMemberInvitation db user groupMemberId)) >>= \case
              (Just ct, Just cReq) -> do
                sendGrpInvitation user ct gInfo (m :: GroupMember) {memberRole = newRole} cReq
                withFastStore' $ \db -> updateGroupMemberRole db user m newRole
                pure (m :: GroupMember) {memberRole = newRole}
              _ -> throwChatError $ CEGroupCantResendInvitation gInfo cName
      changeRoleCurrentMems :: User -> Group -> [GroupMember] -> CM ([ChatError], [GroupMember], [AChatItem])
      changeRoleCurrentMems user (Group gInfo members) memsToChange = case L.nonEmpty memsToChange of
        Nothing -> pure ([], [], [])
        Just memsToChange' -> do
          let events = L.map (\GroupMember {memberId} -> XGrpMemRole memberId newRole) memsToChange'
          (msgs_, _gsr) <- sendGroupMessages user gInfo members events
          let itemsData = zipWith (fmap . sndItemData) memsToChange (L.toList msgs_)
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo) Nothing itemsData Nothing False
          when (length cis_ /= length memsToChange) $ logError "changeRoleCurrentMems: memsToChange and cis_ length mismatch"
          (errs, changed) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (updMember db) memsToChange)
          let acis = map (AChatItem SCTGroup SMDSnd (GroupChat gInfo)) $ rights cis_
          pure (errs, changed, acis)
          where
            sndItemData :: GroupMember -> SndMessage -> NewSndChatItemData c
            sndItemData GroupMember {groupMemberId, memberProfile} msg =
              let content = CISndGroupEvent $ SGEMemberRole groupMemberId (fromLocalProfile memberProfile) newRole
                  ts = ciContentTexts content
               in NewSndChatItemData msg content ts M.empty Nothing Nothing Nothing
            updMember db m = do
              updateGroupMemberRole db user m newRole
              pure (m :: GroupMember) {memberRole = newRole}
  APIBlockMembersForAll groupId memberIds blockFlag -> withUser $ \user ->
    withGroupLock "blockForAll" groupId . procCmd $ do
      Group gInfo members <- withFastStore $ \db -> getGroup db vr user groupId
      when (selfSelected gInfo) $ throwChatError $ CECommandError "can't block/unblock self"
      let (blockMems, remainingMems, maxRole, anyAdmin, anyPending) = selectMembers members
      when (length blockMems /= length memberIds) $ throwChatError CEGroupMemberNotFound
      when (length memberIds > 1 && anyAdmin) $ throwChatError $ CECommandError "can't block/unblock multiple members when admins selected"
      when anyPending $ throwChatError $ CECommandError "can't block/unblock members pending approval"
      assertUserGroupRole gInfo $ max GRModerator maxRole
      blockMembers user gInfo blockMems remainingMems
    where
      selfSelected GroupInfo {membership} = elem (groupMemberId' membership) memberIds
      selectMembers :: [GroupMember] -> ([GroupMember], [GroupMember], GroupMemberRole, Bool, Bool)
      selectMembers = foldr' addMember ([], [], GRObserver, False, False)
        where
          addMember m@GroupMember {groupMemberId, memberRole, memberStatus} (block, remaining, maxRole, anyAdmin, anyPending)
            | groupMemberId `elem` memberIds =
                let maxRole' = max maxRole memberRole
                    anyAdmin' = anyAdmin || memberRole >= GRAdmin
                    anyPending' = anyPending || memberStatus == GSMemPendingApproval
                 in (m : block, remaining, maxRole', anyAdmin', anyPending')
            | otherwise = (block, m : remaining, maxRole, anyAdmin, anyPending)
      blockMembers :: User -> GroupInfo -> [GroupMember] -> [GroupMember] -> CM ChatResponse
      blockMembers user gInfo blockMems remainingMems = case L.nonEmpty blockMems of
        Nothing -> throwChatError $ CECommandError "no members to block/unblock"
        Just blockMems' -> do
          let mrs = if blockFlag then MRSBlocked else MRSUnrestricted
              events = L.map (\GroupMember {memberId} -> XGrpMemRestrict memberId MemberRestrictions {restriction = mrs}) blockMems'
          (msgs_, _gsr) <- sendGroupMessages user gInfo remainingMems events
          let itemsData = zipWith (fmap . sndItemData) blockMems (L.toList msgs_)
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo) Nothing itemsData Nothing False
          when (length cis_ /= length blockMems) $ logError "blockMembers: blockMems and cis_ length mismatch"
          let acis = map (AChatItem SCTGroup SMDSnd (GroupChat gInfo)) $ rights cis_
          unless (null acis) $ toView $ CEvtNewChatItems user acis
          (errs, blocked) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (updateGroupMemberBlocked db user gInfo mrs) blockMems)
          unless (null errs) $ toView $ CEvtChatErrors errs
          -- TODO not batched - requires agent batch api
          forM_ blocked $ \m -> toggleNtf m (not blockFlag)
          pure CRMembersBlockedForAllUser {user, groupInfo = gInfo, members = blocked, blocked = blockFlag}
        where
          sndItemData :: GroupMember -> SndMessage -> NewSndChatItemData c
          sndItemData GroupMember {groupMemberId, memberProfile} msg =
            let content = CISndGroupEvent $ SGEMemberBlocked groupMemberId (fromLocalProfile memberProfile) blockFlag
                ts = ciContentTexts content
             in NewSndChatItemData msg content ts M.empty Nothing Nothing Nothing
  APIRemoveMembers {groupId, groupMemberIds, withMessages} -> withUser $ \user ->
    withGroupLock "removeMembers" groupId . procCmd $ do
      Group gInfo members <- withFastStore $ \db -> getGroup db vr user groupId
      let (count, invitedMems, pendingMems, currentMems, maxRole, anyAdmin) = selectMembers members
          memCount = S.size groupMemberIds
      when (count /= memCount) $ throwChatError CEGroupMemberNotFound
      when (memCount > 1 && anyAdmin) $ throwChatError $ CECommandError "can't remove multiple members when admins selected"
      assertUserGroupRole gInfo $ max GRAdmin maxRole
      (errs1, deleted1) <- deleteInvitedMems user invitedMems
      (errs2, deleted2, acis2) <- deleteMemsSend user gInfo members currentMems
      rs <- forM pendingMems $ \m -> deleteMemsSend user gInfo [m] [m] -- TODO [knocking]
      let (errs3, deleted3, acis3) = concatTuples rs
          acis = acis2 <> acis3
          errs = errs1 <> errs2 <> errs3
      unless (null acis) $ toView $ CEvtNewChatItems user acis
      unless (null errs) $ toView $ CEvtChatErrors errs
      when withMessages $ deleteMessages user gInfo $ currentMems <> pendingMems
      pure $ CRUserDeletedMembers user gInfo (deleted1 <> deleted2 <> deleted3) withMessages -- same order is not guaranteed
    where
      selectMembers :: [GroupMember] -> (Int, [GroupMember], [GroupMember], [GroupMember], GroupMemberRole, Bool)
      selectMembers = foldl' addMember (0, [], [], [], GRObserver, False)
        where
          addMember acc@(n, invited, pending, current, maxRole, anyAdmin) m@GroupMember {groupMemberId, memberStatus, memberRole}
            | groupMemberId `S.member` groupMemberIds =
                let maxRole' = max maxRole memberRole
                    anyAdmin' = anyAdmin || memberRole >= GRAdmin
                    n' = n + 1
                 in case memberStatus of
                      GSMemInvited -> (n', m : invited, pending, current, maxRole', anyAdmin')
                      GSMemPendingApproval -> (n', invited, m : pending, current, maxRole', anyAdmin')
                      _ -> (n', invited, pending, m : current, maxRole', anyAdmin')
            | otherwise = acc
      deleteInvitedMems :: User -> [GroupMember] -> CM ([ChatError], [GroupMember])
      deleteInvitedMems user memsToDelete = do
        deleteMembersConnections user memsToDelete
        lift $ partitionEithers <$> withStoreBatch' (\db -> map (delMember db) memsToDelete)
        where
          delMember db m = do
            deleteGroupMember db user m
            pure m {memberStatus = GSMemRemoved}
      deleteMemsSend :: User -> GroupInfo -> [GroupMember] -> [GroupMember] -> CM ([ChatError], [GroupMember], [AChatItem])
      deleteMemsSend user gInfo sendToMems memsToDelete = case L.nonEmpty memsToDelete of
        Nothing -> pure ([], [], [])
        Just memsToDelete' -> do
          let events = L.map (\GroupMember {memberId} -> XGrpMemDel memberId withMessages) memsToDelete'
          (msgs_, _gsr) <- sendGroupMessages user gInfo sendToMems events
          let itemsData = zipWith (fmap . sndItemData) memsToDelete (L.toList msgs_)
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo) Nothing itemsData Nothing False
          when (length cis_ /= length memsToDelete) $ logError "deleteCurrentMems: memsToDelete and cis_ length mismatch"
          deleteMembersConnections' user memsToDelete True
          (errs, deleted) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (delMember db) memsToDelete)
          let acis = map (AChatItem SCTGroup SMDSnd (GroupChat gInfo)) $ rights cis_
          pure (errs, deleted, acis)
          where
            sndItemData :: GroupMember -> SndMessage -> NewSndChatItemData c
            sndItemData GroupMember {groupMemberId, memberProfile} msg =
              let content = CISndGroupEvent $ SGEMemberDeleted groupMemberId (fromLocalProfile memberProfile)
                  ts = ciContentTexts content
               in NewSndChatItemData msg content ts M.empty Nothing Nothing Nothing
            delMember db m = do
              deleteOrUpdateMemberRecordIO db user m
              pure m {memberStatus = GSMemRemoved}
      deleteMessages user gInfo@GroupInfo {membership} ms
        | groupFeatureMemberAllowed SGFFullDelete membership gInfo = deleteGroupMembersCIs user gInfo ms membership
        | otherwise = markGroupMembersCIsDeleted user gInfo ms membership
      concatTuples :: [([a], [b], [c])] -> ([a], [b], [c])
      concatTuples xs = (concat as, concat bs, concat cs)
        where (as, bs, cs) = unzip3 xs
  APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
    Group gInfo@GroupInfo {membership} members <- withFastStore $ \db -> getGroup db vr user groupId
    filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
    withGroupLock "leaveGroup" groupId . procCmd $ do
      cancelFilesInProgress user filesInfo
      -- TODO [knocking] send to pending approval members (move `memberCurrent` filter from sendGroupMessages_ to call sites)
      msg <- sendGroupMessage' user gInfo members XGrpLeave
      ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent SGEUserLeft)
      toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci]
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
  JoinGroup gName enableNtfs -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIJoinGroup groupId enableNtfs
  MemberRole gName gMemberName memRole -> withMemberName gName gMemberName $ \gId gMemberId -> APIMembersRole gId [gMemberId] memRole
  BlockForAll gName gMemberName blocked -> withMemberName gName gMemberName $ \gId gMemberId -> APIBlockMembersForAll gId [gMemberId] blocked
  RemoveMembers gName gMemberNames withMessages -> withUser $ \user -> do
    (gId, gMemberIds) <- withStore $ \db -> do
      gId <- getGroupIdByName db user gName
      gMemberIds <- S.fromList <$> mapM (getGroupMemberIdByName db user gId) (S.toList gMemberNames)
      pure (gId, gMemberIds)
    processChatCommand $ APIRemoveMembers gId gMemberIds withMessages
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
  APICreateGroupLink groupId mRole short -> withUser $ \user -> withGroupLock "createGroupLink" groupId $ do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    assertUserGroupRole gInfo GRAdmin
    when (mRole > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole
    groupLinkId <- GroupLinkId <$> drgRandomBytes 16
    subMode <- chatReadVar subscriptionMode
    let crClientData = encodeJSON $ CRDataGroup groupLinkId
        userData = shortLinkUserData short
    (connId, ccLink) <- withAgent $ \a -> createConnection a (aUserId user) True SCMContact userData (Just crClientData) IKPQOff subMode
    ccLink' <- createdGroupLink <$> shortenCreatedLink ccLink
    withFastStore $ \db -> createGroupLink db user gInfo connId ccLink' groupLinkId mRole subMode
    pure $ CRGroupLinkCreated user gInfo ccLink' mRole
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
        (connId, CCLink cReq _) <- withAgent $ \a -> createConnection a (aUserId user) True SCMInvitation Nothing Nothing IKPQOff subMode
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
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct') ci]
        pure $ CRNewMemberContactSentInv user ct' g m
      _ -> throwChatError CEGroupMemberNotActive
  CreateGroupLink gName mRole short -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APICreateGroupLink groupId mRole short
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
    (groupId, quotedItemId, mentions) <-
      withFastStore $ \db -> do
        gId <- getGroupIdByName db user gName
        qiId <- getGroupChatItemIdByText db user gId cName quotedMsg
        (gId, qiId,) <$> liftIO (getMessageMentions db user gId msg)
    let mc = MCText msg
    processChatCommand $ APISendMessages (SRGroup groupId Nothing) False Nothing [ComposedMessage Nothing (Just quotedItemId) mc mentions]
  ClearNoteFolder -> withUser $ \user -> do
    folderId <- withFastStore (`getUserNoteFolderId` user)
    processChatCommand $ APIClearChat (ChatRef CTLocal folderId)
  LastChats count_ -> withUser' $ \user -> do
    let count = fromMaybe 5000 count_
    (errs, previews) <- partitionEithers <$> withFastStore' (\db -> getChatPreviews db vr user False (PTLast count) clqNoFilters)
    unless (null errs) $ toView $ CEvtChatErrors (map ChatErrorStore errs)
    pure $ CRChats previews
  LastMessages (Just chatName) count search -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand $ APIGetChat chatRef Nothing (CPLast count) search
    pure $ CRChatItems user (Just chatName) (aChatItems . chat $ chatResp)
  LastMessages Nothing count search -> withUser $ \user -> do
    chatItems <- withFastStore $ \db -> getAllChatItems db vr user (CPLast count) search
    pure $ CRChatItems user Nothing chatItems
  LastChatItemId (Just chatName) index -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand (APIGetChat chatRef Nothing (CPLast $ index + 1) Nothing)
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
      ChatRef CTLocal folderId -> processChatCommand $ APICreateChatItems folderId [composedMessage (Just f) (MCFile "")]
      _ -> withSendRef chatRef $ \sendRef -> processChatCommand $ APISendMessages sendRef False Nothing [composedMessage (Just f) (MCFile "")]
  SendImage chatName f@(CryptoFile fPath _) -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    withSendRef chatRef $ \sendRef -> do
      filePath <- lift $ toFSFilePath fPath
      unless (any (`isSuffixOf` map toLower fPath) imageExtensions) $ throwChatError CEFileImageType {filePath}
      fileSize <- getFileSize filePath
      unless (fileSize <= maxImageSize) $ throwChatError CEFileImageSize {filePath}
      -- TODO include file description for preview
      processChatCommand $ APISendMessages sendRef False Nothing [composedMessage (Just f) (MCImage "" fixedImagePreview)]
  ForwardFile chatName fileId -> forwardFile chatName fileId SendFile
  ForwardImage chatName fileId -> forwardFile chatName fileId SendImage
  SendFileDescription _chatName _f -> throwCmdError "TODO"
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
              deleteAgentConnectionsAsync fileAgentConnIds
              withFastStore (\db -> liftIO $ lookupChatRefByFileId db user fileId) >>= \case
                Nothing -> pure ()
                Just (ChatRef CTDirect contactId) -> do
                  (contact, sharedMsgId) <- withFastStore $ \db -> (,) <$> getContact db vr user contactId <*> getSharedMsgIdByFileId db userId fileId
                  void . sendDirectContactMessage user contact $ XFileCancel sharedMsgId
                Just (ChatRef CTGroup groupId) -> do
                  (Group gInfo ms, sharedMsgId) <- withFastStore $ \db -> (,) <$> getGroup db vr user groupId <*> getSharedMsgIdByFileId db userId fileId
                  -- TODO [knocking] send separately to pending approval member
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
                cancelRcvFileTransfer user ftr >>= mapM_ deleteAgentConnectionAsync
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
    chatMigrations <- map upMigration <$> withFastStore' getCurrentMigrations
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
  -- in a modified CLI app or core - the hook should return Either (Either ChatError ChatResponse) ChatCommand,
  -- where Left means command result, and Right – some other command to be processed by this function.
  CustomChatCommand _cmd -> withUser $ \_ -> throwCmdError "not supported"
  where
    procCmd :: CM ChatResponse -> CM ChatResponse
    procCmd = id
    {-# INLINE procCmd #-}
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
    getChatRefAndMentions :: User -> ChatName -> Text -> CM (ChatRef, Map MemberName GroupMemberId)
    getChatRefAndMentions user cName msg = do
      chatRef@(ChatRef cType chatId) <- getChatRef user cName
      (chatRef,) <$> case cType of
        CTGroup -> withFastStore' $ \db -> getMessageMentions db user chatId msg
        _ -> pure []
#if !defined(dbPostgres)
    checkChatStopped :: CM ChatResponse -> CM ChatResponse
    checkChatStopped a = asks agentAsync >>= readTVarIO >>= maybe a (const $ throwChatError CEChatNotStopped)
    setStoreChanged :: CM ()
    setStoreChanged = asks chatStoreChanged >>= atomically . (`writeTVar` True)
    withStoreChanged :: CM () -> CM ChatResponse
    withStoreChanged a = checkChatStopped $ a >> setStoreChanged >> ok_
#endif
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
    connectViaContact :: User -> IncognitoEnabled -> CreatedLinkContact -> CM ChatResponse
    connectViaContact user@User {userId} incognito (CCLink cReq@(CRContactUri ConnReqUriData {crClientData}) sLnk) = withInvitationLock "connectViaContact" (strEncode cReq) $ do
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
          let sLnk' = serverShortLink <$> sLnk
          conn@PendingContactConnection {pccConnId} <- withFastStore' $ \db -> createConnReqConnection db userId connId cReqHash sLnk' xContactId incognitoProfile groupLinkId subMode chatV pqSup
          joinContact user pccConnId connId cReq incognitoProfile xContactId inGroup pqSup chatV
          pure $ CRSentInvitation user conn incognitoProfile
    connectContactViaAddress :: User -> IncognitoEnabled -> Contact -> CreatedLinkContact -> CM ChatResponse
    connectContactViaAddress user incognito ct (CCLink cReq shortLink) =
      withInvitationLock "connectContactViaAddress" (strEncode cReq) $ do
        newXContactId <- XContactId <$> drgRandomBytes 16
        let pqSup = PQSupportOn
        (connId, chatV) <- prepareContact user cReq pqSup
        let cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
        -- [incognito] generate profile to send
        incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
        subMode <- chatReadVar subscriptionMode
        (pccConnId, ct') <- withFastStore $ \db -> createAddressContactConnection db vr user ct connId cReqHash shortLink newXContactId incognitoProfile subMode chatV pqSup
        joinContact user pccConnId connId cReq incognitoProfile newXContactId False pqSup chatV
        pure $ CRSentInvitationToContact user ct' incognitoProfile
    prepareContact :: User -> ConnReqContact -> PQSupport -> CM (ConnId, VersionChat)
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
    joinContact :: User -> Int64 -> ConnId -> ConnReqContact -> Maybe Profile -> XContactId -> Bool -> PQSupport -> VersionChat -> CM ()
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
        cId == Just contactId && s /= GSMemRejected && s /= GSMemRemoved && s /= GSMemLeft
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
                unless (null errs) $ toView $ CEvtChatErrors errs
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
            (conn, MsgFlags {notification = hasNotification XInfo_}, (vrValue msgBody, [msgId]))
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
              void (sendDirectContactMessage user ct' $ XInfo mergedProfile') `catchChatError` eToView
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
            -- TODO [knocking] send to pending approval members (move `memberCurrent` filter from sendGroupMessages_ to call sites)
            void $ sendGroupMessage user g' oldMs (XGrpInfo p'')
          let ps' = fromMaybe defaultBusinessGroupPrefs $ groupPreferences p'
          sendGroupMessage user g' newMs $ XGrpPrefs ps'
        Nothing -> sendGroupMessage user g' ms (XGrpInfo p')
      let cd = CDGroupSnd g'
      unless (sameGroupProfileInfo p p') $ do
        ci <- saveSndChatItem user cd msg (CISndGroupEvent $ SGEGroupUpdated p')
        toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat g') ci]
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
    delGroupChatItemsForMembers :: User -> GroupInfo -> [GroupMember] -> [CChatItem 'CTGroup] -> CM [ChatItemDeletion]
    delGroupChatItemsForMembers user gInfo ms items = do
      assertDeletable gInfo items
      assertUserGroupRole gInfo GRAdmin -- TODO GRModerator when most users migrate
      let msgMemIds = itemsMsgMemIds gInfo items
          events = L.nonEmpty $ map (\(msgId, memId) -> XMsgDel msgId (Just memId)) msgMemIds
      -- TODO [knocking] validate: only current members or only single pending approval member,
      -- TODO            or prohibit pending approval members (only moderation and reports use this)
      mapM_ (sendGroupMessages user gInfo ms) events
      delGroupChatItems user gInfo items True
      where
        assertDeletable :: GroupInfo -> [CChatItem 'CTGroup] -> CM ()
        assertDeletable GroupInfo {membership = GroupMember {memberRole = membershipMemRole}} items' =
          unless (all itemDeletable items') $ throwChatError CEInvalidChatItemDelete
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

    delGroupChatItems :: User -> GroupInfo -> [CChatItem 'CTGroup] -> Bool -> CM [ChatItemDeletion]
    delGroupChatItems user gInfo@GroupInfo {membership} items moderation = do
      deletedTs <- liftIO getCurrentTime
      when moderation $ do
        ciIds <- concat <$> withStore' (\db -> forM items $ \(CChatItem _ ci) -> markMessageReportsDeleted db user gInfo ci membership deletedTs)
        unless (null ciIds) $ toView $ CEvtGroupChatItemsDeleted user gInfo ciIds True (Just membership)
      let m = if moderation then Just membership else Nothing
      if groupFeatureMemberAllowed SGFFullDelete membership gInfo
        then deleteGroupCIs user gInfo items m deletedTs
        else markGroupCIsDeleted user gInfo items m deletedTs
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
      toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]
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
      deleteCIFiles user filesInfo
      withAgent (\a -> deleteUser a (aUserId user) delSMPQueues)
        `catchChatError` \case
          e@(ChatErrorAgent NO_USER _) -> eToView e
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
    connectPlan :: User -> AConnectionLink -> CM (ACreatedConnLink, ConnectionPlan)
    connectPlan user (ACL SCMInvitation cLink) = case cLink of
      CLFull cReq -> invitationReqAndPlan cReq Nothing
      CLShort l -> do
        let l' = serverShortLink l
        withFastStore' (\db -> getConnectionEntityViaShortLink db vr user l') >>= \case
          Just (cReq, ent) ->
            (ACCL SCMInvitation (CCLink cReq (Just l')),) <$> (invitationEntityPlan ent `catchChatError` (pure . CPError))
          Nothing -> getShortLinkConnReq user l' >>= (`invitationReqAndPlan` Just l')
      where
        invitationReqAndPlan cReq sLnk_ = do
          plan <- inviationRequestPlan user cReq `catchChatError` (pure . CPError)
          pure (ACCL SCMInvitation (CCLink cReq sLnk_), plan)
    connectPlan user (ACL SCMContact cLink) = case cLink of
      CLFull cReq -> contactReqAndPlan cReq Nothing
      CLShort l@(CSLContact _ ct _ _) -> do
        let l' = serverShortLink l
        case ct of
          CCTContact ->
            withFastStore' (\db -> getUserContactLinkViaShortLink db user l') >>= \case
              Just (UserContactLink (CCLink cReq _) _) -> pure (ACCL SCMContact $ CCLink cReq (Just l'), CPContactAddress CAPOwnLink)
              Nothing -> getShortLinkConnReq user l' >>= (`contactReqAndPlan` Just l')
          CCTGroup ->
            withFastStore' (\db -> getGroupInfoViaUserShortLink db vr user l') >>= \case
              Just (cReq, g) -> pure (ACCL SCMContact $ CCLink cReq (Just l'), CPGroupLink (GLPOwnLink g))
              Nothing -> getShortLinkConnReq user l' >>= (`contactReqAndPlan` Just l')
          CCTChannel -> throwChatError $ CECommandError "channel links are not supported in this version"
      where
        contactReqAndPlan cReq sLnk_ = do
          plan <- contactRequestPlan user cReq `catchChatError` (pure . CPError)
          pure (ACCL SCMContact $ CCLink cReq sLnk_, plan)
    connectWithPlan :: User -> IncognitoEnabled -> ACreatedConnLink -> ConnectionPlan -> CM ChatResponse
    connectWithPlan user@User {userId} incognito ccLink plan
      | connectionPlanProceed plan = do
          case plan of CPError e -> eToView e; _ -> pure ()
          case plan of
            CPContactAddress (CAPContactViaAddress Contact {contactId}) ->
              processChatCommand $ APIConnectContactViaAddress userId incognito contactId
            _ -> processChatCommand $ APIConnect userId incognito (Just ccLink)
      | otherwise = pure $ CRConnectionPlan user ccLink plan
    inviationRequestPlan :: User -> ConnReqInvitation -> CM ConnectionPlan
    inviationRequestPlan user cReq = do
      withFastStore' (\db -> getConnectionEntityByConnReq db vr user $ cReqSchemas cReq) >>= \case
        Nothing -> pure $ CPInvitationLink ILPOk
        Just ent -> invitationEntityPlan ent
      where
        cReqSchemas :: ConnReqInvitation -> (ConnReqInvitation, ConnReqInvitation)
        cReqSchemas (CRInvitationUri crData e2e) =
          ( CRInvitationUri crData {crScheme = SSSimplex} e2e,
            CRInvitationUri crData {crScheme = simplexChat} e2e
          )
    invitationEntityPlan :: ConnectionEntity -> CM ConnectionPlan
    invitationEntityPlan = \case
      RcvDirectMsgConnection Connection {connStatus = ConnPrepared} Nothing ->
        pure $ CPInvitationLink ILPOk
      RcvDirectMsgConnection conn ct_ -> do
        let Connection {connStatus, contactConnInitiated} = conn
        if
          | connStatus == ConnNew && contactConnInitiated ->
              pure $ CPInvitationLink ILPOwnLink
          | not (connReady conn) ->
              pure $ CPInvitationLink (ILPConnecting ct_)
          | otherwise -> case ct_ of
              Just ct -> pure $ CPInvitationLink (ILPKnown ct)
              Nothing -> throwChatError $ CEInternalError "ready RcvDirectMsgConnection connection should have associated contact"
      _ -> throwChatError $ CECommandError "found connection entity is not RcvDirectMsgConnection"
    contactRequestPlan :: User -> ConnReqContact -> CM ConnectionPlan
    contactRequestPlan user (CRContactUri crData) = do
      let ConnReqUriData {crClientData} = crData
          groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
          cReqHashes = bimap hash hash cReqSchemas
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
          | memberStatus membership == GSMemRejected = pure $ CPGroupLink (GLPKnown gInfo)
          | not (memberActive membership) && not (memberRemoved membership) =
              pure $ CPGroupLink (GLPConnectingProhibit $ Just gInfo)
          | memberActive membership = pure $ CPGroupLink (GLPKnown gInfo)
          | otherwise = pure $ CPGroupLink GLPOk
        cReqSchemas :: (ConnReqContact, ConnReqContact)
        cReqSchemas =
          ( CRContactUri crData {crScheme = SSSimplex},
            CRContactUri crData {crScheme = simplexChat}
          )
        hash :: ConnReqContact -> ConnReqUriHash
        hash = ConnReqUriHash . C.sha256Hash . strEncode
    getShortLinkConnReq :: User -> ConnShortLink m -> CM (ConnectionRequestUri m)
    getShortLinkConnReq user l = do
      l' <- restoreShortLink' l
      (cReq, cData) <- withAgent (\a -> getConnShortLink a (aUserId user) l')
      case cData of
        ContactLinkData {direct} | not direct -> throwChatError CEUnsupportedConnReq
        _ -> pure ()
      pure cReq
    -- This function is needed, as UI uses simplex:/ schema in message view, so that the links can be handled without browser,
    -- and short links are stored with server hostname schema, so they wouldn't match without it.
    serverShortLink :: ConnShortLink m -> ConnShortLink m
    serverShortLink = \case
      CSLInvitation _ srv lnkId linkKey -> CSLInvitation SLSServer srv lnkId linkKey
      CSLContact _ ct srv linkKey -> CSLContact SLSServer ct srv linkKey
    restoreShortLink' l = (`restoreShortLink` l) <$> asks (shortLinkPresetServers . config)
    shortLinkUserData short = if short then Just "" else Nothing
    shortenCreatedLink :: CreatedConnLink m -> CM (CreatedConnLink m)
    shortenCreatedLink (CCLink cReq sLnk) = CCLink cReq <$> mapM (\l -> (`shortenShortLink` l) <$> asks (shortLinkPresetServers . config)) sLnk
    createdGroupLink :: CreatedLinkContact -> CreatedLinkContact
    createdGroupLink (CCLink cReq shortLink) = CCLink cReq (toGroupLink <$> shortLink)
      where
        toGroupLink :: ShortLinkContact -> ShortLinkContact
        toGroupLink (CSLContact sch _ srv k) = CSLContact sch CCTGroup srv k
    updateCIGroupInvitationStatus :: User -> GroupInfo -> CIGroupInvitationStatus -> CM ()
    updateCIGroupInvitationStatus user GroupInfo {groupId} newStatus = do
      AChatItem _ _ cInfo ChatItem {content, meta = CIMeta {itemId}} <- withFastStore $ \db -> getChatItemByGroupId db vr user groupId
      case (cInfo, content) of
        (DirectChat ct@Contact {contactId}, CIRcvGroupInvitation ciGroupInv@CIGroupInvitation {status} memRole)
          | status == CIGISPending -> do
              let aciContent = ACIContent SMDRcv $ CIRcvGroupInvitation (ciGroupInv {status = newStatus} :: CIGroupInvitation) memRole
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
    assertNoMentions :: ComposedMessage -> CM ()
    assertNoMentions ComposedMessage {mentions}
      | null mentions = pure ()
      | otherwise = throwChatError $ CECommandError "mentions are not supported in this chat"
    sendContactContentMessages :: User -> ContactId -> Bool -> Maybe Int -> NonEmpty ComposedMessageReq -> CM ChatResponse
    sendContactContentMessages user contactId live itemTTL cmrs = do
      assertMultiSendable live cmrs
      ct <- withFastStore $ \db -> getContact db vr user contactId
      assertDirectAllowed user MDSnd ct XMsgNew_
      assertVoiceAllowed ct
      processComposedMessages ct
      where
        assertVoiceAllowed :: Contact -> CM ()
        assertVoiceAllowed ct =
          when (not (featureAllowed SCFVoice forUser ct) && any (\(ComposedMessage {msgContent}, _, _, _) -> isVoice msgContent) cmrs) $
            throwChatError (CECommandError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFVoice))
        processComposedMessages :: Contact -> CM ChatResponse
        processComposedMessages ct = do
          (fInvs_, ciFiles_) <- L.unzip <$> setupSndFileTransfers
          timed_ <- sndContactCITimed live ct itemTTL
          (msgContainers, quotedItems_) <- L.unzip <$> prepareMsgs (L.zip cmrs fInvs_) timed_
          msgs_ <- sendDirectContactMessages user ct $ L.map XMsgNew msgContainers
          let itemsData = prepareSndItemsData (L.toList cmrs) (L.toList ciFiles_) (L.toList quotedItems_) msgs_
          when (length itemsData /= length cmrs) $ logError "sendContactContentMessages: cmrs and itemsData length mismatch"
          r@(_, cis) <- partitionEithers <$> saveSndChatItems user (CDDirectSnd ct) Nothing itemsData timed_ live
          processSendErrs r
          forM_ (timed_ >>= timedDeleteAt') $ \deleteAt ->
            forM_ cis $ \ci ->
              startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId' ci) deleteAt
          pure $ CRNewChatItems user (map (AChatItem SCTDirect SMDSnd (DirectChat ct)) cis)
          where
            setupSndFileTransfers :: CM (NonEmpty (Maybe FileInvitation, Maybe (CIFile 'MDSnd)))
            setupSndFileTransfers =
              forM cmrs $ \(ComposedMessage {fileSource = file_}, _, _, _) -> case file_ of
                Just file -> do
                  fileSize <- checkSndFile file
                  (fInv, ciFile) <- xftpSndFileTransfer user file fileSize 1 $ CGContact ct
                  pure (Just fInv, Just ciFile)
                Nothing -> pure (Nothing, Nothing)
            prepareMsgs :: NonEmpty (ComposedMessageReq, Maybe FileInvitation) -> Maybe CITimed -> CM (NonEmpty (MsgContainer, Maybe (CIQuote 'CTDirect)))
            prepareMsgs cmsFileInvs timed_ = withFastStore $ \db ->
              forM cmsFileInvs $ \((ComposedMessage {quotedItemId, msgContent = mc}, itemForwarded, _, _), fInv_) -> do
                case (quotedItemId, itemForwarded) of
                  (Nothing, Nothing) -> pure (MCSimple (ExtMsgContent mc M.empty fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
                  (Nothing, Just _) -> pure (MCForward (ExtMsgContent mc M.empty fInv_ (ttl' <$> timed_) (justTrue live)), Nothing)
                  (Just qiId, Nothing) -> do
                    CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
                      getDirectChatItem db user contactId qiId
                    (origQmc, qd, sent) <- quoteData qci
                    let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Nothing}
                        qmc = quoteContent mc origQmc file
                        quotedItem = CIQuote {chatDir = qd, itemId = Just qiId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
                    pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc M.empty fInv_ (ttl' <$> timed_) (justTrue live)), Just quotedItem)
                  (Just _, Just _) -> throwError SEInvalidQuote
              where
                quoteData :: ChatItem c d -> ExceptT StoreError IO (MsgContent, CIQDirection 'CTDirect, Bool)
                quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} = throwError SEInvalidQuote
                quoteData ChatItem {content = CISndMsgContent qmc} = pure (qmc, CIQDirectSnd, True)
                quoteData ChatItem {content = CIRcvMsgContent qmc} = pure (qmc, CIQDirectRcv, False)
                quoteData _ = throwError SEInvalidQuote
    sendGroupContentMessages :: User -> GroupInfo -> Maybe GroupMemberId -> Bool -> Maybe Int -> NonEmpty ComposedMessageReq -> CM ChatResponse
    sendGroupContentMessages user gInfo@GroupInfo {membership} directMemId_ live itemTTL cmrs = do
      assertMultiSendable live cmrs
      (ms, numFileInvs, notInHistory_) <- case directMemId_ of
        Nothing -> do
          ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
          pure (ms, length $ filter memberCurrent ms, Nothing)
        Just dmId -> do
          when (dmId == groupMemberId' membership) $ throwChatError $ CECommandError "cannot send to self"
          dm <- withFastStore $ \db -> getGroupMemberById db vr user dmId
          unless (memberStatus dm == GSMemPendingApproval) $ throwChatError $ CECommandError "cannot send directly to member not pending approval"
          pure ([dm], 1, Just NotInHistory)
      sendGroupContentMessages_ user gInfo notInHistory_ ms numFileInvs live itemTTL cmrs
    sendGroupContentMessages_ :: User -> GroupInfo -> Maybe NotInHistory -> [GroupMember] -> Int -> Bool -> Maybe Int -> NonEmpty ComposedMessageReq -> CM ChatResponse
    sendGroupContentMessages_ user gInfo@GroupInfo {groupId, membership} notInHistory_ ms numFileInvs live itemTTL cmrs = do
      -- TODO [knocking] pass GroupSndScope?
      let allowedRole = case ms of
            [m] | memberCategory m == GCHostMember && memberStatus membership == GSMemPendingApproval -> Nothing
            _ -> Just GRAuthor
      forM_ allowedRole $ assertUserGroupRole gInfo
      assertGroupContentAllowed
      processComposedMessages
      where
        assertGroupContentAllowed :: CM ()
        assertGroupContentAllowed =
          case findProhibited (L.toList cmrs) of
            Just f -> throwChatError (CECommandError $ "feature not allowed " <> T.unpack (groupFeatureNameText f))
            Nothing -> pure ()
          where
            findProhibited :: [ComposedMessageReq] -> Maybe GroupFeature
            findProhibited =
              foldr'
                (\(ComposedMessage {fileSource, msgContent = mc}, _, (_, ft), _) acc -> prohibitedGroupContent gInfo membership mc ft fileSource True <|> acc)
                Nothing
        processComposedMessages :: CM ChatResponse
        processComposedMessages = do
          (fInvs_, ciFiles_) <- L.unzip <$> setupSndFileTransfers numFileInvs
          timed_ <- sndGroupCITimed live gInfo itemTTL
          (chatMsgEvents, quotedItems_) <- L.unzip <$> prepareMsgs (L.zip cmrs fInvs_) timed_
          (msgs_, gsr) <- sendGroupMessages user gInfo ms chatMsgEvents
          let itemsData = prepareSndItemsData (L.toList cmrs) (L.toList ciFiles_) (L.toList quotedItems_) (L.toList msgs_)
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo) notInHistory_ itemsData timed_ live
          when (length cis_ /= length cmrs) $ logError "sendGroupContentMessages: cmrs and cis_ length mismatch"
          createMemberSndStatuses cis_ msgs_ gsr
          let r@(_, cis) = partitionEithers cis_
          processSendErrs r
          forM_ (timed_ >>= timedDeleteAt') $ \deleteAt ->
            forM_ cis $ \ci ->
              startProximateTimedItemThread user (ChatRef CTGroup groupId, chatItemId' ci) deleteAt
          pure $ CRNewChatItems user (map (AChatItem SCTGroup SMDSnd (GroupChat gInfo)) cis)
          where
            setupSndFileTransfers :: Int -> CM (NonEmpty (Maybe FileInvitation, Maybe (CIFile 'MDSnd)))
            setupSndFileTransfers n =
              forM cmrs $ \(ComposedMessage {fileSource = file_}, _, _, _) -> case file_ of
                Just file -> do
                  fileSize <- checkSndFile file
                  (fInv, ciFile) <- xftpSndFileTransfer user file fileSize n $ CGGroup gInfo ms
                  pure (Just fInv, Just ciFile)
                Nothing -> pure (Nothing, Nothing)
            prepareMsgs :: NonEmpty (ComposedMessageReq, Maybe FileInvitation) -> Maybe CITimed -> CM (NonEmpty (ChatMsgEvent 'Json, Maybe (CIQuote 'CTGroup)))
            prepareMsgs cmsFileInvs timed_ = withFastStore $ \db ->
              forM cmsFileInvs $ \((ComposedMessage {quotedItemId, msgContent = mc}, itemForwarded, _, ciMentions), fInv_) ->
                let mentions = M.map (\CIMention {memberId} -> MsgMention {memberId}) ciMentions
                 in prepareGroupMsg db user gInfo mc mentions quotedItemId itemForwarded fInv_ timed_ live
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
    assertMultiSendable :: Bool -> NonEmpty ComposedMessageReq -> CM ()
    assertMultiSendable live cmrs
      | length cmrs == 1 = pure ()
      | otherwise =
          -- When sending multiple messages only single quote is allowed.
          -- This is to support case of sending multiple attachments while also quoting another message.
          -- UI doesn't allow composing with multiple quotes, so api prohibits it as well, and doesn't bother
          -- batching retrieval of quoted messages (prepareMsgs).
          when (live || length (L.filter (\(ComposedMessage {quotedItemId}, _, _, _) -> isJust quotedItemId) cmrs) > 1) $
            throwChatError (CECommandError "invalid multi send: live and more than one quote not supported")
    xftpSndFileTransfer :: User -> CryptoFile -> Integer -> Int -> ContactOrGroup -> CM (FileInvitation, CIFile 'MDSnd)
    xftpSndFileTransfer user file fileSize n contactOrGroup = do
      (fInv, ciFile, ft) <- xftpSndFileTransfer_ user file fileSize n $ Just contactOrGroup
      case contactOrGroup of
        CGContact Contact {activeConn} -> forM_ activeConn $ \conn ->
          withFastStore' $ \db -> createSndFTDescrXFTP db user Nothing conn ft dummyFileDescr
        CGGroup _ ms -> forM_ ms $ \m -> saveMemberFD m `catchChatError` eToView
          where
            -- we are not sending files to pending members, same as with inline files
            saveMemberFD m@GroupMember {activeConn = Just conn@Connection {connStatus}} =
              when ((connStatus == ConnReady || connStatus == ConnSndReady) && not (connDisabled conn)) $
                withFastStore' $
                  \db -> createSndFTDescrXFTP db user (Just m) conn ft dummyFileDescr
            saveMemberFD _ = pure ()
      pure (fInv, ciFile)
    prepareSndItemsData ::
      [ComposedMessageReq] ->
      [Maybe (CIFile 'MDSnd)] ->
      [Maybe (CIQuote c)] ->
      [Either ChatError SndMessage] ->
      [Either ChatError (NewSndChatItemData c)]
    prepareSndItemsData =
      zipWith4 $ \(ComposedMessage {msgContent}, itemForwarded, ts, mm) f q -> \case
        Right msg -> Right $ NewSndChatItemData msg (CISndMsgContent msgContent) ts mm f q itemForwarded
        Left e -> Left e -- step over original error
    processSendErrs :: ([ChatError], [ChatItem c d]) -> CM ()
    processSendErrs = \case
      -- no errors
      ([], _) -> pure ()
      -- at least one item is successfully created
      (errs, _ci : _) -> toView $ CEvtChatErrors errs
      -- single error
      ([err], []) -> throwError err
      -- multiple errors
      (errs@(err : _), []) -> do
        toView $ CEvtChatErrors errs
        throwError err
    getCommandDirectChatItems :: User -> Int64 -> NonEmpty ChatItemId -> CM (Contact, [CChatItem 'CTDirect])
    getCommandDirectChatItems user ctId itemIds = do
      ct <- withFastStore $ \db -> getContact db vr user ctId
      (errs, items) <- lift $ partitionEithers <$> withStoreBatch (\db -> map (getDirectCI db) (L.toList itemIds))
      unless (null errs) $ toView $ CEvtChatErrors errs
      pure (ct, items)
      where
        getDirectCI :: DB.Connection -> ChatItemId -> IO (Either ChatError (CChatItem 'CTDirect))
        getDirectCI db itemId = runExceptT . withExceptT ChatErrorStore $ getDirectChatItem db user ctId itemId
    getCommandGroupChatItems :: User -> Int64 -> NonEmpty ChatItemId -> CM (GroupInfo, [CChatItem 'CTGroup])
    getCommandGroupChatItems user gId itemIds = do
      gInfo <- withFastStore $ \db -> getGroupInfo db vr user gId
      (errs, items) <- lift $ partitionEithers <$> withStoreBatch (\db -> map (getGroupCI db gInfo) (L.toList itemIds))
      unless (null errs) $ toView $ CEvtChatErrors errs
      pure (gInfo, items)
      where
        getGroupCI :: DB.Connection -> GroupInfo -> ChatItemId -> IO (Either ChatError (CChatItem 'CTGroup))
        getGroupCI db gInfo itemId = runExceptT . withExceptT ChatErrorStore $ getGroupCIWithReactions db user gInfo itemId
    getCommandLocalChatItems :: User -> Int64 -> NonEmpty ChatItemId -> CM (NoteFolder, [CChatItem 'CTLocal])
    getCommandLocalChatItems user nfId itemIds = do
      nf <- withStore $ \db -> getNoteFolder db user nfId
      (errs, items) <- lift $ partitionEithers <$> withStoreBatch (\db -> map (getLocalCI db) (L.toList itemIds))
      unless (null errs) $ toView $ CEvtChatErrors errs
      pure (nf, items)
      where
        getLocalCI :: DB.Connection -> ChatItemId -> IO (Either ChatError (CChatItem 'CTLocal))
        getLocalCI db itemId = runExceptT . withExceptT ChatErrorStore $ getLocalChatItem db user nfId itemId
    forwardMsgContent :: ChatItem c d -> CM (Maybe MsgContent)
    forwardMsgContent ChatItem {meta = CIMeta {itemDeleted = Just _}} = pure Nothing -- this can be deleted after selection
    forwardMsgContent ChatItem {content = CISndMsgContent fmc} = pure $ Just fmc
    forwardMsgContent ChatItem {content = CIRcvMsgContent fmc} = pure $ Just fmc
    forwardMsgContent _ = throwChatError CEInvalidForward
    createNoteFolderContentItems :: User -> NoteFolderId -> NonEmpty ComposedMessageReq -> CM ChatResponse
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
          when (any (\(ComposedMessage {quotedItemId}, _, _, _) -> isJust quotedItemId) cmrs) $
            throwChatError (CECommandError "createNoteFolderContentItems: quotes not supported")
        createLocalFiles :: NoteFolder -> UTCTime -> CM (NonEmpty (Maybe (CIFile 'MDSnd)))
        createLocalFiles nf createdAt =
          forM cmrs $ \(ComposedMessage {fileSource = file_}, _, _, _) ->
            forM file_ $ \cf@CryptoFile {filePath, cryptoArgs} -> do
              fsFilePath <- lift $ toFSFilePath filePath
              fileSize <- liftIO $ CF.getFileContentsSize $ CryptoFile fsFilePath cryptoArgs
              chunkSize <- asks $ fileChunkSize . config
              withFastStore' $ \db -> do
                fileId <- createLocalFile CIFSSndStored db user nf createdAt cf fileSize chunkSize
                pure CIFile {fileId, fileName = takeFileName filePath, fileSize, fileSource = Just cf, fileStatus = CIFSSndStored, fileProtocol = FPLocal}
        prepareLocalItemsData ::
          NonEmpty ComposedMessageReq ->
          NonEmpty (Maybe (CIFile 'MDSnd)) ->
          NonEmpty (CIContent 'MDSnd, Maybe (CIFile 'MDSnd), Maybe CIForwardedFrom, (Text, Maybe MarkdownList))
        prepareLocalItemsData =
          L.zipWith $ \(ComposedMessage {msgContent = mc}, itemForwarded, ts, _) f ->
            (CISndMsgContent mc, f, itemForwarded, ts)
    getConnQueueInfo user Connection {connId, agentConnId = AgentConnId acId} = do
      msgInfo <- withFastStore' (`getLastRcvMsgInfo` connId)
      CRQueueInfo user msgInfo <$> withAgent (`getConnectionQueueInfo` acId)
    withSendRef :: ChatRef -> (SendRef -> CM ChatResponse) -> CM ChatResponse
    withSendRef chatRef a = case chatRef of
      ChatRef CTDirect cId -> a $ SRDirect cId
      ChatRef CTGroup gId -> a $ SRGroup gId Nothing
      _ -> throwChatError $ CECommandError "not supported"

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

type ComposedMessageReq = (ComposedMessage, Maybe CIForwardedFrom, (Text, Maybe MarkdownList), Map MemberName CIMention)

composedMessage :: Maybe CryptoFile -> MsgContent -> ComposedMessage
composedMessage f mc = ComposedMessage {fileSource = f, quotedItemId = Nothing, msgContent = mc, mentions = M.empty}

composedMessageReq :: ComposedMessage -> ComposedMessageReq
composedMessageReq cm@ComposedMessage {msgContent = mc} = (cm, Nothing, msgContentTexts mc, M.empty)

composedMessageReqMentions :: DB.Connection -> User -> GroupInfo -> ComposedMessage -> ExceptT StoreError IO ComposedMessageReq
composedMessageReqMentions db user g cm@ComposedMessage {msgContent = mc, mentions} = do
  let ts@(_, ft_) = msgContentTexts mc
  (cm,Nothing,ts,) <$> getCIMentions db user g ft_ mentions

data ChangedProfileContact = ChangedProfileContact
  { ct :: Contact,
    ct' :: Contact,
    mergedProfile' :: Profile,
    conn :: Connection
  }

createContactsSndFeatureItems :: User -> [ChangedProfileContact] -> CM' ()
createContactsSndFeatureItems user cts =
  createContactsFeatureItems user cts' CDDirectSnd CISndChatFeature CISndChatPreference getPref
  where
    cts' = map (\ChangedProfileContact {ct, ct'} -> (ct, ct')) cts
    getPref ContactUserPreference {userPreference} = case userPreference of
      CUPContact {preference} -> preference
      CUPUser {preference} -> preference

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
        flip catchChatError' (eToView') $ do
          expireFlags <- asks expireCIFlags
          atomically $ TM.lookup userId expireFlags >>= \b -> unless (b == Just True) retry
          lift waitChatStartedAndActivated
          ttl <- withStore' (`getChatItemTTL` user)
          expireChatItems user ttl False
        liftIO $ threadDelay' interval

setChatItemsExpiration :: User -> Int64 -> Int -> CM' ()
setChatItemsExpiration user newTTL ttlCount 
  | newTTL > 0 || ttlCount > 0 = do
      startExpireCIThread user
      whenM chatStarted $ setExpireCIFlag user True
  | otherwise = setExpireCIFlag user False

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

agentSubscriber :: CM' ()
agentSubscriber = do
  q <- asks $ subQ . smpAgent
  forever (atomically (readTBQueue q) >>= process)
    `E.catchAny` \e -> do
      eToView' $ ChatErrorAgent (CRITICAL True $ "Message reception stopped: " <> show e) Nothing
      E.throwIO e
  where
    process :: (ACorrId, AEntityId, AEvt) -> CM' ()
    process (corrId, entId, AEvt e msg) = run $ case e of
      SAENone -> processAgentMessageNoConn msg
      SAEConn -> processAgentMessage corrId entId msg
      SAERcvFile -> processAgentMsgRcvFile corrId entId msg
      SAESndFile -> processAgentMsgSndFile corrId entId msg
      where
        run action = action `catchChatError'` (eToView')

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
  unlessM (asks $ coreApi . config) $ do
    contactLinkSubsToView rs ucs
    groupSubsToView rs gs ms ce
    sndFileSubsToView rs sfts
    rcvFileSubsToView rs rfts
    pendingConnSubsToView rs pcs
  where
    addEntity (cts, ucs, ms, sfts, rfts, pcs) = \case
      RcvDirectMsgConnection c (Just ct) -> let cts' = addConn c ct cts in (cts', ucs, ms, sfts, rfts, pcs)
      RcvDirectMsgConnection c Nothing -> let pcs' = addConn c (toPCC c) pcs in (cts, ucs, ms, sfts, rfts, pcs')
      RcvGroupMsgConnection c _g m -> let ms' = addConn c (toShortMember m c) ms in (cts, ucs, ms', sfts, rfts, pcs)
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
          connLinkInv = Nothing,
          localAlias,
          createdAt,
          updatedAt = createdAt
        }
    toShortMember GroupMember {groupMemberId, groupId, localDisplayName} Connection {agentConnId} =
      ShortGroupMember
        { groupMemberId,
          groupId,
          memberName = localDisplayName,
          connId = agentConnId
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
    getGroupMemberConns :: CM ([ShortGroup], [ConnId], Map ConnId ShortGroupMember)
    getGroupMemberConns = do
      gs <- withStore_ getUserGroupsToSubscribe
      let mPairs = concatMap (\(ShortGroup _ ms) -> map (\m -> (shortMemConnId m, m)) ms) gs
      pure (gs, map fst mPairs, M.fromList mPairs)
      where
        shortMemConnId ShortGroupMember{connId = AgentConnId acId} = acId
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
          toView . CEvtContactSubSummary user $ map (uncurry ContactSubStatus) cRs
          when ce $ mapM_ (toView . uncurry (CEvtContactSubError user)) cErrors
        notifyAPI = toView . CEvtNetworkStatuses (Just user) . map (uncurry ConnNetworkStatus)
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
    contactLinkSubsToView rs = toView . CEvtUserContactSubSummary user . map (uncurry UserContactSubStatus) . resultsFor rs
    groupSubsToView :: Map ConnId (Either AgentErrorType ()) -> [ShortGroup] -> Map ConnId ShortGroupMember -> Bool -> CM ()
    groupSubsToView rs gs ms ce = do
      mapM_ groupSub $
        sortOn (\(ShortGroup ShortGroupInfo {groupName = g} _) -> g) gs
      toViewTE . TEMemberSubSummary user $ map (uncurry MemberSubStatus) mRs
      where
        mRs = resultsFor rs ms
        groupSub :: ShortGroup -> CM ()
        groupSub (ShortGroup g@ShortGroupInfo {groupId = gId, membershipStatus} members) = do
          when ce $ mapM_ (toViewTE . uncurry (TEMemberSubError user g)) mErrors
          toViewTE groupEvent
          where
            mErrors :: [(ShortGroupMember, ChatError)]
            mErrors =
              sortOn (\(ShortGroupMember {memberName = n}, _) -> n)
                . filterErrors
                $ filter (\(ShortGroupMember {groupId}, _) -> groupId == gId) mRs
            groupEvent :: TerminalEvent
            groupEvent
              | membershipStatus == GSMemInvited = TEGroupInvitation user g
              | null members = TEGroupEmpty user g
              | otherwise = TEGroupSubscribed user g
    sndFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId SndFileTransfer -> CM ()
    sndFileSubsToView rs sfts = do
      let sftRs = resultsFor rs sfts
      forM_ sftRs $ \(ft@SndFileTransfer {fileId, fileStatus}, err_) -> do
        forM_ err_ $ toViewTE . TESndFileSubError user ft
        void . forkIO $ do
          threadDelay 1000000
          when (fileStatus == FSConnected) . unlessM (isFileActive fileId sndFiles) . withChatLock "subscribe sendFileChunk" $
            sendFileChunk user ft
    rcvFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId RcvFileTransfer -> CM ()
    rcvFileSubsToView rs = mapM_ (toViewTE . uncurry (TERcvFileSubError user)) . filterErrors . resultsFor rs
    pendingConnSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId PendingContactConnection -> CM ()
    pendingConnSubsToView rs = toViewTE . TEPendingSubSummary user . map (uncurry PendingSubStatus) . resultsFor rs
    withStore_ :: (DB.Connection -> User -> IO [a]) -> CM [a]
    withStore_ a = withStore' (`a` user) `catchChatError` \e -> eToView e $> []
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
    flip catchChatError eToView $ do
      lift waitChatStartedAndActivated
      users <- withStore' getUsers
      let (us, us') = partition activeUser users
      forM_ us $ cleanupUser interval stepDelay
      forM_ us' $ cleanupUser interval stepDelay
      cleanupMessages `catchChatError` eToView
      -- TODO possibly, also cleanup async commands
      cleanupProbes `catchChatError` eToView
    liftIO $ threadDelay' $ diffToMicroseconds interval
  where
    runWithoutInitialDelay cleanupInterval = flip catchChatError eToView $ do
      lift waitChatStartedAndActivated
      users <- withStore' getUsers
      let (us, us') = partition activeUser users
      forM_ us $ \u -> cleanupTimedItems cleanupInterval u `catchChatError` eToView
      forM_ us' $ \u -> cleanupTimedItems cleanupInterval u `catchChatError` eToView
    cleanupUser cleanupInterval stepDelay user = do
      cleanupTimedItems cleanupInterval user `catchChatError` eToView
      liftIO $ threadDelay' stepDelay
      -- TODO remove in future versions: legacy step - contacts are no longer marked as deleted
      cleanupDeletedContacts user `catchChatError` eToView
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
          `catchChatError` eToView
    cleanupMessages = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(30 * nominalDay)) ts
      withStore' (`deleteOldMessages` cutoffTs)
    cleanupProbes = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(14 * nominalDay)) ts
      withStore' (`deleteOldProbes` cutoffTs)

expireChatItems :: User -> Int64 -> Bool -> CM ()
expireChatItems user@User {userId} globalTTL sync = do
  currentTs <- liftIO getCurrentTime
  vr <- chatVersionRange
  -- this is to keep group messages created during last 12 hours even if they're expired according to item_ts
  let createdAtCutoff = addUTCTime (-43200 :: NominalDiffTime) currentTs
  lift waitChatStartedAndActivated
  contactIds <- withStore' $ \db -> getUserContactsToExpire db user globalTTL
  loop contactIds $ expireContactChatItems user vr globalTTL
  lift waitChatStartedAndActivated
  groupIds <- withStore' $ \db -> getUserGroupsToExpire db user globalTTL
  loop groupIds $ expireGroupChatItems user vr globalTTL createdAtCutoff
  where
    loop :: [Int64] -> (Int64 -> CM ()) -> CM ()
    loop [] _ = pure ()
    loop (a : as) process = continue $ do
      process a `catchChatError` eToView
      loop as process
    continue :: CM () -> CM ()
    continue a =
      if sync
        then a
        else do
          expireFlags <- asks expireCIFlags
          expire <- atomically $ TM.lookup userId expireFlags
          when (expire == Just True) $ threadDelay 100000 >> a

expireContactChatItems :: User -> VersionRangeChat -> Int64 -> ContactId -> CM ()
expireContactChatItems user vr globalTTL ctId =
  -- reading contacts and groups inside the loop,
  -- to allow ttl changing while processing and to reduce memory usage
  tryChatError (withStore $ \db -> getContact db vr user ctId) >>= mapM_ process
  where
    process ct@Contact {chatItemTTL} =
      withExpirationDate globalTTL chatItemTTL $ \expirationDate -> do
        lift waitChatStartedAndActivated
        filesInfo <- withStore' $ \db -> getContactExpiredFileInfo db user ct expirationDate
        deleteCIFiles user filesInfo
        withStore' $ \db -> deleteContactExpiredCIs db user ct expirationDate

expireGroupChatItems :: User -> VersionRangeChat -> Int64 -> UTCTime -> GroupId -> CM ()
expireGroupChatItems user vr globalTTL createdAtCutoff groupId =
  tryChatError (withStore $ \db -> getGroupInfo db vr user groupId) >>= mapM_ process
  where
    process gInfo@GroupInfo {chatItemTTL} =
      withExpirationDate globalTTL chatItemTTL $ \expirationDate -> do
        lift waitChatStartedAndActivated
        filesInfo <- withStore' $ \db -> getGroupExpiredFileInfo db user gInfo expirationDate createdAtCutoff
        deleteCIFiles user filesInfo
        withStore' $ \db -> deleteGroupExpiredCIs db user gInfo expirationDate createdAtCutoff
        membersToDelete <- withStore' $ \db -> getGroupMembersForExpiration db vr user gInfo
        forM_ membersToDelete $ \m -> withStore' $ \db -> deleteGroupMember db user m

withExpirationDate :: Int64 -> Maybe Int64 -> (UTCTime -> CM ()) -> CM ()
withExpirationDate globalTTL chatItemTTL action = do
  currentTs <- liftIO getCurrentTime
  let ttl = fromMaybe globalTTL chatItemTTL
  when (ttl > 0) $ action $ addUTCTime (-1 * fromIntegral ttl) currentTs

chatCommandP :: Parser ChatCommand
chatCommandP =
  choice
    [ "/mute " *> ((`SetShowMessages` MFNone) <$> chatNameP),
      "/unmute " *> ((`SetShowMessages` MFAll) <$> chatNameP),
      "/unmute mentions " *> ((`SetShowMessages` MFMentions) <$> chatNameP),
      "/receipts " *> (SetSendReceipts <$> chatNameP <* " " <*> ((Just <$> onOffP) <|> ("default" $> Nothing))),
      "/block #" *> (SetShowMemberMessages <$> displayNameP <* A.space <*> (char_ '@' *> displayNameP) <*> pure False),
      "/unblock #" *> (SetShowMemberMessages <$> displayNameP <* A.space <*> (char_ '@' *> displayNameP) <*> pure True),
      "/_create user " *> (CreateActiveUser <$> jsonP),
      "/create user " *> (CreateActiveUser <$> newUserP),
      "/users" $> ListUsers,
      "/_user " *> (APISetActiveUser <$> A.decimal <*> optional (A.space *> jsonP)),
      ("/user " <|> "/u ") *> (SetActiveUser <$> displayNameP <*> optional (A.space *> pwdP)),
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
      "/delete user " *> (DeleteUser <$> displayNameP <*> pure True <*> optional (A.space *> pwdP)),
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
#if !defined(dbPostgres)
      "/_db export " *> (APIExportArchive <$> jsonP),
      "/db export" $> ExportArchive,
      "/_db import " *> (APIImportArchive <$> jsonP),
      "/_db delete" $> APIDeleteStorage,
      "/_db encryption " *> (APIStorageEncryption <$> jsonP),
      "/db encrypt " *> (APIStorageEncryption . dbEncryptionConfig "" <$> dbKeyP),
      "/db key " *> (APIStorageEncryption <$> (dbEncryptionConfig <$> dbKeyP <* A.space <*> dbKeyP)),
      "/db decrypt " *> (APIStorageEncryption . (`dbEncryptionConfig` "") <$> dbKeyP),
      "/db test key " *> (TestStorageEncryption <$> dbKeyP),
      "/sql slow" $> SlowSQLQueries,
#endif
      "/_save app settings" *> (APISaveAppSettings <$> jsonP),
      "/_get app settings" *> (APIGetAppSettings <$> optional (A.space *> jsonP)),
      "/sql chat " *> (ExecChatStoreSQL <$> textP),
      "/sql agent " *> (ExecAgentStoreSQL <$> textP),
      "/_get tags " *> (APIGetChatTags <$> A.decimal),
      "/_get chats "
        *> ( APIGetChats
              <$> A.decimal
              <*> (" pcc=on" $> True <|> " pcc=off" $> False <|> pure False)
              <*> (A.space *> paginationByTimeP <|> pure (PTLast 5000))
              <*> (A.space *> jsonP <|> pure clqNoFilters)
           ),
      "/_get chat " *> (APIGetChat <$> chatRefP <*> optional (" content=" *> strP) <* A.space <*> chatPaginationP <*> optional (" search=" *> stringP)),
      "/_get items " *> (APIGetChatItems <$> chatPaginationP <*> optional (" search=" *> stringP)),
      "/_get item info " *> (APIGetChatItemInfo <$> chatRefP <* A.space <*> A.decimal),
      "/_send " *> (APISendMessages <$> sendRefP <*> liveMessageP <*> sendMessageTTLP <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP)),
      "/_create tag " *> (APICreateChatTag <$> jsonP),
      "/_tags " *> (APISetChatTags <$> chatRefP <*> optional _strP),
      "/_delete tag " *> (APIDeleteChatTag <$> A.decimal),
      "/_update tag " *> (APIUpdateChatTag <$> A.decimal <* A.space <*> jsonP),
      "/_reorder tags " *> (APIReorderChatTags <$> strP),
      "/_create *" *> (APICreateChatItems <$> A.decimal <*> (" json " *> jsonP <|> " text " *> composedMessagesTextP)),
      "/_report #" *> (APIReportMessage <$> A.decimal <* A.space <*> A.decimal <*> (" reason=" *> strP) <*> (A.space *> textP <|> pure "")),
      "/report #" *> (ReportMessage <$> displayNameP <*> optional (" @" *> displayNameP) <*> _strP <* A.space <*> msgTextP),
      "/_update item " *> (APIUpdateChatItem <$> chatRefP <* A.space <*> A.decimal <*> liveMessageP <*> (" json" *> jsonP <|> " text " *> updatedMessagesTextP)),
      "/_delete item " *> (APIDeleteChatItem <$> chatRefP <*> _strP <*> _strP),
      "/_delete member item #" *> (APIDeleteMemberChatItem <$> A.decimal <*> _strP),
      "/_archive reports #" *> (APIArchiveReceivedReports <$> A.decimal),
      "/_delete reports #" *> (APIDeleteReceivedReports <$> A.decimal <*> _strP <*> _strP),
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
      "/call " *> char_ '@' *> (SendCallInvitation <$> displayNameP <*> pure defaultCallType),
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
      "/_set alias #" *> (APISetGroupAlias <$> A.decimal <*> (A.space *> textP <|> pure "")),
      "/_set alias :" *> (APISetConnectionAlias <$> A.decimal <*> (A.space *> textP <|> pure "")),
      "/_set prefs @" *> (APISetContactPrefs <$> A.decimal <* A.space <*> jsonP),
      "/_set theme user " *> (APISetUserUIThemes <$> A.decimal <*> optional (A.space *> jsonP)),
      "/_set theme " *> (APISetChatUIThemes <$> chatRefP <*> optional (A.space *> jsonP)),
      "/_ntf get" $> APIGetNtfToken,
      "/_ntf register " *> (APIRegisterToken <$> strP_ <*> strP),
      "/_ntf verify " *> (APIVerifyToken <$> strP <* A.space <*> strP <* A.space <*> strP),
      "/_ntf check " *> (APICheckToken <$> strP),
      "/_ntf delete " *> (APIDeleteToken <$> strP),
      "/_ntf conns " *> (APIGetNtfConns <$> strP <* A.space <*> strP),
      "/_ntf conn messages " *> (APIGetConnNtfMessages <$> connMsgsP),
      "/_add #" *> (APIAddMember <$> A.decimal <* A.space <*> A.decimal <*> memberRole),
      "/_join #" *> (APIJoinGroup <$> A.decimal <*> pure MFAll), -- needs to be changed to support in UI
      "/_accept member #" *> (APIAcceptMember <$> A.decimal <* A.space <*> A.decimal <*> memberRole),
      "/_member role #" *> (APIMembersRole <$> A.decimal <*> _strP <*> memberRole),
      "/_block #" *> (APIBlockMembersForAll <$> A.decimal <*> _strP <* " blocked=" <*> onOffP),
      "/_remove #" *> (APIRemoveMembers <$> A.decimal <*> _strP <*> (" messages=" *> onOffP <|> pure False)),
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
      "/_ttl " *> (APISetChatItemTTL <$> A.decimal <* A.space <*> A.decimal),
      "/_ttl " *> (APISetChatTTL <$> A.decimal <* A.space <*> chatRefP <* A.space <*> ciTTLDecimal),
      "/_ttl " *> (APIGetChatItemTTL <$> A.decimal),
      "/ttl " *> (SetChatItemTTL <$> ciTTL),
      "/ttl" $> GetChatItemTTL,
      "/ttl " *> (SetChatTTL <$> chatNameP <* A.space <*> (("default" $> Nothing) <|> (Just <$> ciTTL))),
      "/ttl " *> (GetChatTTL <$> chatNameP),
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
      ("/info #" <|> "/i #") *> (GroupMemberInfo <$> displayNameP <* A.space <* char_ '@' <*> displayNameP),
      ("/info #" <|> "/i #") *> (ShowGroupInfo <$> displayNameP),
      ("/info " <|> "/i ") *> char_ '@' *> (ContactInfo <$> displayNameP),
      "/_queue info #" *> (APIGroupMemberQueueInfo <$> A.decimal <* A.space <*> A.decimal),
      "/_queue info @" *> (APIContactQueueInfo <$> A.decimal),
      ("/queue info #" <|> "/qi #") *> (GroupMemberQueueInfo <$> displayNameP <* A.space <* char_ '@' <*> displayNameP),
      ("/queue info " <|> "/qi ") *> char_ '@' *> (ContactQueueInfo <$> displayNameP),
      "/_switch #" *> (APISwitchGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/_switch @" *> (APISwitchContact <$> A.decimal),
      "/_abort switch #" *> (APIAbortSwitchGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/_abort switch @" *> (APIAbortSwitchContact <$> A.decimal),
      "/_sync #" *> (APISyncGroupMemberRatchet <$> A.decimal <* A.space <*> A.decimal <*> (" force=on" $> True <|> pure False)),
      "/_sync @" *> (APISyncContactRatchet <$> A.decimal <*> (" force=on" $> True <|> pure False)),
      "/switch #" *> (SwitchGroupMember <$> displayNameP <* A.space <* char_ '@' <*> displayNameP),
      "/switch " *> char_ '@' *> (SwitchContact <$> displayNameP),
      "/abort switch #" *> (AbortSwitchGroupMember <$> displayNameP <* A.space <* char_ '@' <*> displayNameP),
      "/abort switch " *> char_ '@' *> (AbortSwitchContact <$> displayNameP),
      "/sync #" *> (SyncGroupMemberRatchet <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <*> (" force=on" $> True <|> pure False)),
      "/sync " *> char_ '@' *> (SyncContactRatchet <$> displayNameP <*> (" force=on" $> True <|> pure False)),
      "/_get code @" *> (APIGetContactCode <$> A.decimal),
      "/_get code #" *> (APIGetGroupMemberCode <$> A.decimal <* A.space <*> A.decimal),
      "/_verify code @" *> (APIVerifyContact <$> A.decimal <*> optional (A.space *> verifyCodeP)),
      "/_verify code #" *> (APIVerifyGroupMember <$> A.decimal <* A.space <*> A.decimal <*> optional (A.space *> verifyCodeP)),
      "/_enable @" *> (APIEnableContact <$> A.decimal),
      "/_enable #" *> (APIEnableGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/code " *> char_ '@' *> (GetContactCode <$> displayNameP),
      "/code #" *> (GetGroupMemberCode <$> displayNameP <* A.space <* char_ '@' <*> displayNameP),
      "/verify " *> char_ '@' *> (VerifyContact <$> displayNameP <*> optional (A.space *> verifyCodeP)),
      "/verify #" *> (VerifyGroupMember <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <*> optional (A.space *> verifyCodeP)),
      "/enable " *> char_ '@' *> (EnableContact <$> displayNameP),
      "/enable #" *> (EnableGroupMember <$> displayNameP <* A.space <* char_ '@' <*> displayNameP),
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
      ("/add " <|> "/a ") *> char_ '#' *> (AddMember <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <*> (memberRole <|> pure GRMember)),
      ("/join " <|> "/j ") *> char_ '#' *> (JoinGroup <$> displayNameP <*> (" mute" $> MFNone <|> pure MFAll)),
      ("/member role " <|> "/mr ") *> char_ '#' *> (MemberRole <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <*> memberRole),
      "/block for all #" *> (BlockForAll <$> displayNameP <* A.space <*> (char_ '@' *> displayNameP) <*> pure True),
      "/unblock for all #" *> (BlockForAll <$> displayNameP <* A.space <*> (char_ '@' *> displayNameP) <*> pure False),
      ("/remove " <|> "/rm ") *> char_ '#' *> (RemoveMembers <$> displayNameP <* A.space <*> (S.fromList <$> (char_ '@' *> displayNameP) `A.sepBy1'` A.char ',') <*> (" messages=" *> onOffP <|> pure False)),
      ("/leave " <|> "/l ") *> char_ '#' *> (LeaveGroup <$> displayNameP),
      ("/delete #" <|> "/d #") *> (DeleteGroup <$> displayNameP),
      ("/delete " <|> "/d ") *> char_ '@' *> (DeleteContact <$> displayNameP <*> chatDeleteMode),
      "/clear *" $> ClearNoteFolder,
      "/clear #" *> (ClearGroup <$> displayNameP),
      "/clear " *> char_ '@' *> (ClearContact <$> displayNameP),
      ("/members " <|> "/ms ") *> char_ '#' *> (ListMembers <$> displayNameP),
      "/_groups" *> (APIListGroups <$> A.decimal <*> optional (" @" *> A.decimal) <*> optional (A.space *> stringP)),
      ("/groups" <|> "/gs") *> (ListGroups <$> optional (" @" *> displayNameP) <*> optional (A.space *> stringP)),
      "/_group_profile #" *> (APIUpdateGroupProfile <$> A.decimal <* A.space <*> jsonP),
      ("/group_profile " <|> "/gp ") *> char_ '#' *> (UpdateGroupNames <$> displayNameP <* A.space <*> groupProfile),
      ("/group_profile " <|> "/gp ") *> char_ '#' *> (ShowGroupProfile <$> displayNameP),
      "/group_descr " *> char_ '#' *> (UpdateGroupDescription <$> displayNameP <*> optional (A.space *> msgTextP)),
      "/set welcome " *> char_ '#' *> (UpdateGroupDescription <$> displayNameP <* A.space <*> (Just <$> msgTextP)),
      "/delete welcome " *> char_ '#' *> (UpdateGroupDescription <$> displayNameP <*> pure Nothing),
      "/show welcome " *> char_ '#' *> (ShowGroupDescription <$> displayNameP),
      "/_create link #" *> (APICreateGroupLink <$> A.decimal <*> (memberRole <|> pure GRMember) <*> shortOnOffP),
      "/_set link role #" *> (APIGroupLinkMemberRole <$> A.decimal <*> memberRole),
      "/_delete link #" *> (APIDeleteGroupLink <$> A.decimal),
      "/_get link #" *> (APIGetGroupLink <$> A.decimal),
      "/create link #" *> (CreateGroupLink <$> displayNameP <*> (memberRole <|> pure GRMember) <*> shortP),
      "/set link role #" *> (GroupLinkMemberRole <$> displayNameP <*> memberRole),
      "/delete link #" *> (DeleteGroupLink <$> displayNameP),
      "/show link #" *> (ShowGroupLink <$> displayNameP),
      "/_create member contact #" *> (APICreateMemberContact <$> A.decimal <* A.space <*> A.decimal),
      "/_invite member contact @" *> (APISendMemberContactInvitation <$> A.decimal <*> optional (A.space *> msgContentP)),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayNameP <* A.space <*> pure Nothing <*> quotedMsg <*> msgTextP),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayNameP <* A.space <* char_ '@' <*> (Just <$> displayNameP) <* A.space <*> quotedMsg <*> msgTextP),
      "/_contacts " *> (APIListContacts <$> A.decimal),
      "/contacts" $> ListContacts,
      "/_connect plan " *> (APIConnectPlan <$> A.decimal <* A.space <*> strP),
      "/_connect " *> (APIAddContact <$> A.decimal <*> shortOnOffP <*> incognitoOnOffP),
      "/_connect " *> (APIConnect <$> A.decimal <*> incognitoOnOffP <* A.space <*> connLinkP),
      "/_set incognito :" *> (APISetConnectionIncognito <$> A.decimal <* A.space <*> onOffP),
      "/_set conn user :" *> (APIChangeConnectionUser <$> A.decimal <* A.space <*> A.decimal),
      ("/connect" <|> "/c") *> (AddContact <$> shortP <*> incognitoP),
      ("/connect" <|> "/c") *> (Connect <$> incognitoP <* A.space <*> ((Just <$> strP) <|> A.takeTill isSpace $> Nothing)),
      ForwardMessage <$> chatNameP <* " <- @" <*> displayNameP <* A.space <*> msgTextP,
      ForwardGroupMessage <$> chatNameP <* " <- #" <*> displayNameP <* A.space <* A.char '@' <*> (Just <$> displayNameP) <* A.space <*> msgTextP,
      ForwardGroupMessage <$> chatNameP <* " <- #" <*> displayNameP <*> pure Nothing <* A.space <*> msgTextP,
      ForwardLocalMessage <$> chatNameP <* " <- * " <*> msgTextP,
      SendMessage <$> chatNameP <* A.space <*> msgTextP,
      "/* " *> (SendMessage (ChatName CTLocal "") <$> msgTextP),
      "@#" *> (SendMemberContactMessage <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <* A.space <*> msgTextP),
      "/live " *> (SendLiveMessage <$> chatNameP <*> (A.space *> msgTextP <|> pure "")),
      (">@" <|> "> @") *> sendMsgQuote (AMsgDirection SMDRcv),
      (">>@" <|> ">> @") *> sendMsgQuote (AMsgDirection SMDSnd),
      ("\\ " <|> "\\") *> (DeleteMessage <$> chatNameP <* A.space <*> textP),
      ("\\\\ #" <|> "\\\\#") *> (DeleteMemberMessage <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <* A.space <*> textP),
      ("! " <|> "!") *> (EditMessage <$> chatNameP <* A.space <*> (quotedMsg <|> pure "") <*> msgTextP),
      ReactToMessage <$> (("+" $> True) <|> ("-" $> False)) <*> reactionP <* A.space <*> chatNameP' <* A.space <*> textP,
      "/feed " *> (SendMessageBroadcast . MCText <$> msgTextP),
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
      "/_address " *> (APICreateMyAddress <$> A.decimal <*> shortOnOffP),
      ("/address" <|> "/ad") *> (CreateMyAddress <$> shortP),
      "/_delete_address " *> (APIDeleteMyAddress <$> A.decimal),
      ("/delete_address" <|> "/da") $> DeleteMyAddress,
      "/_show_address " *> (APIShowMyAddress <$> A.decimal),
      ("/show_address" <|> "/sa") $> ShowMyAddress,
      "/_profile_address " *> (APISetProfileAddress <$> A.decimal <* A.space <*> onOffP),
      ("/profile_address " <|> "/pa ") *> (SetProfileAddress <$> onOffP),
      "/_auto_accept " *> (APIAddressAutoAccept <$> A.decimal <* A.space <*> autoAcceptP),
      "/auto_accept " *> (AddressAutoAccept <$> autoAcceptP),
      ("/accept" <|> "/ac") *> (AcceptContact <$> incognitoP <* A.space <* char_ '@' <*> displayNameP),
      ("/reject " <|> "/rc ") *> char_ '@' *> (RejectContact <$> displayNameP),
      ("/markdown" <|> "/m") $> ChatHelp HSMarkdown,
      ("/welcome" <|> "/w") $> Welcome,
      "/set profile image " *> (UpdateProfileImage . Just . ImageData <$> imageP),
      "/delete profile image" $> UpdateProfileImage Nothing,
      "/show profile image" $> ShowProfileImage,
      ("/profile " <|> "/p ") *> (uncurry UpdateProfile <$> profileNames),
      ("/profile" <|> "/p") $> ShowProfile,
      "/set voice #" *> (SetGroupFeatureRole (AGFR SGFVoice) <$> displayNameP <*> _strP <*> optional memberRole),
      "/set voice @" *> (SetContactFeature (ACF SCFVoice) <$> displayNameP <*> optional (A.space *> strP)),
      "/set voice " *> (SetUserFeature (ACF SCFVoice) <$> strP),
      "/set files #" *> (SetGroupFeatureRole (AGFR SGFFiles) <$> displayNameP <*> _strP <*> optional memberRole),
      "/set history #" *> (SetGroupFeature (AGFNR SGFHistory) <$> displayNameP <*> (A.space *> strP)),
      "/set reactions #" *> (SetGroupFeature (AGFNR SGFReactions) <$> displayNameP <*> (A.space *> strP)),
      "/set calls @" *> (SetContactFeature (ACF SCFCalls) <$> displayNameP <*> optional (A.space *> strP)),
      "/set calls " *> (SetUserFeature (ACF SCFCalls) <$> strP),
      "/set delete #" *> (SetGroupFeatureRole (AGFR SGFFullDelete) <$> displayNameP <*> _strP <*> optional memberRole),
      "/set delete @" *> (SetContactFeature (ACF SCFFullDelete) <$> displayNameP <*> optional (A.space *> strP)),
      "/set delete " *> (SetUserFeature (ACF SCFFullDelete) <$> strP),
      "/set direct #" *> (SetGroupFeatureRole (AGFR SGFDirectMessages) <$> displayNameP <*> _strP <*> optional memberRole),
      "/set disappear #" *> (SetGroupTimedMessages <$> displayNameP <*> (A.space *> timedTTLOnOffP)),
      "/set disappear @" *> (SetContactTimedMessages <$> displayNameP <*> optional (A.space *> timedMessagesEnabledP)),
      "/set disappear " *> (SetUserTimedMessages <$> (("yes" $> True) <|> ("no" $> False))),
      "/set reports #" *> (SetGroupFeature (AGFNR SGFReports) <$> displayNameP <*> _strP),
      "/set links #" *> (SetGroupFeatureRole (AGFR SGFSimplexLinks) <$> displayNameP <*> _strP <*> optional memberRole),
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
    connLinkP = do
      ((Just <$> strP) <|> A.takeTill (== ' ') $> Nothing)
        >>= mapM (\(ACR m cReq) -> ACCL m . CCLink cReq <$> optional (A.space *> strP))
    shortP = (A.space *> ("short" <|> "s")) $> True <|> pure False
    incognitoP = (A.space *> ("incognito" <|> "i")) $> True <|> pure False
    shortOnOffP = (A.space *> "short=" *> onOffP) <|> pure False
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
    sendMsgQuote msgDir = SendMessageQuote <$> displayNameP <* A.space <*> pure msgDir <*> quotedMsg <*> msgTextP
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
      pure [composedMessage Nothing text]
    updatedMessagesTextP = (`UpdatedMessage` []) <$> mcTextP
    liveMessageP = " live=" *> onOffP <|> pure False
    sendMessageTTLP = " ttl=" *> ((Just <$> A.decimal) <|> ("default" $> Nothing)) <|> pure Nothing
    receiptSettings = do
      enable <- onOffP
      clearOverrides <- (" clear_overrides=" *> onOffP) <|> pure False
      pure UserMsgReceiptSettings {enable, clearOverrides}
    onOffP = ("on" $> True) <|> ("off" $> False)
    profileNames = (,) <$> displayNameP <*> fullNameP
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
      pure GroupProfile {displayName = gName, fullName, description = Nothing, image = Nothing, groupPreferences, memberAdmission = Nothing}
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
    connMsgsP = L.fromList <$> connMsgP `A.sepBy1'` A.char ','
    connMsgP = do
      AgentConnId msgConnId <- strP <* A.char ':'
      msgDbQueueId <- strP <* A.char ':'
      ts <- strP
      pure ConnMsgReq {msgConnId, msgDbQueueId, msgTs = Just ts}
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
        ct -> ChatName ct <$> displayNameP
    chatNameP' = ChatName <$> (chatTypeP <|> pure CTDirect) <*> displayNameP
    chatRefP = ChatRef <$> chatTypeP <*> A.decimal
    sendRefP =
      (A.char '@' $> SRDirect <*> A.decimal)
        <|> (A.char '#' $> SRGroup <*> A.decimal <*> optional (" @" *> A.decimal))
    msgCountP = A.space *> A.decimal <|> pure 10
    ciTTLDecimal = ("default" $> Nothing) <|> (Just <$> A.decimal)
    ciTTL =
      ("day" $> 86400)
        <|> ("week" $> (7 * 86400))
        <|> ("month" $> (30 * 86400))
        <|> ("year" $> (365 * 86400))
        <|> ("none" $> 0)
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
      smpWebPortServers <- (" smp-web-port-servers=" *> strP) <|> (" smp-web-port" $> SWPAll) <|> pure SWPPreset
      t_ <- optional $ " timeout=" *> A.decimal
      logTLSErrors <- " log=" *> onOffP <|> pure False
      let tcpTimeout_ = (1000000 *) <$> t_
      pure $ SimpleNetCfg {socksProxy, socksMode, hostMode, requiredHostMode, smpProxyMode_, smpProxyFallback_, smpWebPortServers, tcpTimeout_, logTLSErrors}
#if !defined(dbPostgres)
    dbKeyP = nonEmptyKey <$?> strP
    nonEmptyKey k@(DBEncryptionKey s) = if BA.null s then Left "empty key" else Right k
    dbEncryptionConfig currentKey newKey = DBEncryptionConfig {currentKey, newKey, keepKey = Just False}
#endif
    autoAcceptP = ifM onOffP (Just <$> (businessAA <|> addressAA)) (pure Nothing)
      where
        addressAA = AutoAccept False <$> (" incognito=" *> onOffP <|> pure False) <*> autoReply
        businessAA = AutoAccept True <$> (" business" *> pure False) <*> autoReply
        autoReply = optional (A.space *> msgContentP)
    rcCtrlAddressP = RCCtrlAddress <$> ("addr=" *> strP) <*> (" iface=" *> (jsonP <|> text1P))
    text1P = safeDecodeUtf8 <$> A.takeTill (== ' ')
    char_ = optional . A.char

displayNameP :: Parser Text
displayNameP = safeDecodeUtf8 <$> (quoted '\'' <|> takeNameTill (\c -> isSpace c || c == ','))
  where
    takeNameTill p =
      A.peekChar' >>= \c ->
        if refChar c then A.takeTill p else fail "invalid first character in display name"
    quoted c = A.char c *> takeNameTill (== c) <* A.char c
    refChar c = c > ' ' && c /= '#' && c /= '@' && c /= '\''

mkValidName :: String -> String
mkValidName = dropWhileEnd isSpace . take 50 . reverse . fst3 . foldl' addChar ("", '\NUL', 0 :: Int)
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
