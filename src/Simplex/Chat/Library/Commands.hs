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
import qualified Data.Attoparsec.Combinator as A
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char
import Data.Constraint (Dict (..))
import Data.Either (fromRight, isRight, partitionEithers, rights)
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
import Simplex.Chat.Store.ContactRequest
import Simplex.Chat.Store.Connections
import Simplex.Chat.Store.Delivery
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
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..), ServerRoles (..), allRoles)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.Entity
import Simplex.Messaging.Agent.Store.Interface (execSQL)
import Simplex.Messaging.Agent.Store.Shared (upMigration)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (getCurrentMigrations)
import Simplex.Messaging.Client (NetworkConfig (..), NetworkRequestMode (..), NetworkTimeout (..), SMPWebPortServers (..), SocksMode (SMAlways), textToHostMode)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..), pattern IKPQOff, pattern IKPQOn, pattern PQEncOff, pattern PQSupportOff, pattern PQSupportOn)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (base64P)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), AProtocolType (..), MsgFlags (..), NtfServer, ProtoServerWithAuth (..), ProtocolServer, ProtocolType (..), ProtocolTypeI (..), SProtocolType (..), SubscriptionMode (..), UserProtocol, userProtocol)
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
import Simplex.Messaging.Agent.Client (SubInfo (..), getAgentQueuesInfo, getAgentWorkersDetails, getAgentWorkersSummary, temporaryOrHostError)
#else
import Data.Bifunctor (bimap, first, second)
import qualified Data.ByteArray as BA
import qualified Database.SQLite.Simple as SQL
import Simplex.Chat.Archive
import Simplex.Messaging.Agent.Client (SubInfo (..), agentClientStore, getAgentQueuesInfo, getAgentWorkersDetails, getAgentWorkersSummary, temporaryOrHostError)
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
  runExceptT (syncConnections' users) >>= \case
    Left e -> liftIO $ putStrLn $ "Error synchronizing connections: " <> show e
    Right _ -> pure ()
  restoreCalls
  s <- asks agentAsync
  readTVarIO s >>= maybe (start s users) (pure . fst)
  where
    syncConnections' users =
      whenM (withFastStore' shouldSyncConnections) $ do
        let aUserIds = map aUserId users
        connIds <- concat <$> forM users getConnsToSub
        (userDiff, connDiff) <- withAgent (\a -> syncConnections a aUserIds connIds)
        withFastStore' setConnectionsSyncTs
        toView $ CEvtConnectionsDiff (AgentUserId <$> userDiff) (AgentConnId <$> connDiff)
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
          startDeliveryWorkers
          startRelayRequestWorker_
          startCleanupManager
          void $ forkIO $ mapM_ startExpireCIs users
          startRelayChecks users
        else when enableSndFiles $ startXFTP xftpStartSndWorkers
      pure a1
    startXFTP startWorkers = do
      tmp <- readTVarIO =<< asks tempDirectory
      runExceptT (withAgent $ \a -> startWorkers a tmp) >>= \case
        Left e -> liftIO $ putStrLn $ "Error starting XFTP workers: " <> show e
        Right _ -> pure ()
    startDeliveryWorkers =
      runExceptT (startDeliveryTaskWorkers >> startDeliveryJobWorkers) >>= \case
        Left e -> liftIO $ putStrLn $ "Error starting delivery workers: " <> show e
        Right _ -> pure ()
    startRelayRequestWorker_ =
      runExceptT startRelayRequestWorker >>= \case
        Left e -> liftIO $ putStrLn $ "Error starting relay request worker: " <> show e
        Right _ -> pure ()
    startCleanupManager = do
      cleanupAsync <- asks cleanupManagerAsync
      readTVarIO cleanupAsync >>= \case
        Nothing -> do
          a <- Just <$> async (void $ runExceptT cleanupManager)
          atomically $ writeTVar cleanupAsync a
        _ -> pure ()
    startRelayChecks users = do
      let relayUser_ = find (\User {userChatRelay} -> isTrue userChatRelay) users
      forM_ relayUser_ $ \relayUser -> do
        relayAsync <- asks relayGroupLinkChecksAsync
        readTVarIO relayAsync >>= \case
          Nothing -> do
            a <- Just <$> async (void $ runExceptT $ runRelayGroupLinkChecks relayUser)
            atomically $ writeTVar relayAsync a
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

getConnsToSub :: User -> CM [ConnId]
getConnsToSub user =
  withFastStore' $ \db -> do
    ctConnIds <- getContactConnsToSub db user False
    uclConnIds <- getUCLConnsToSub db user False
    memberConnIds <- getMemberConnsToSub db user False
    pendingConnIds <- getPendingConnsToSub db user False
    pure $ ctConnIds <> uclConnIds <> memberConnIds <> pendingConnIds

subscribeUsers :: Bool -> [User] -> CM' ()
subscribeUsers onlyNeeded users = do
  let activeUserId_ = (\User {agentUserId = AgentUserId uId} -> uId) <$> find activeUser users
  withAgent (\a -> subscribeAllConnections a onlyNeeded activeUserId_) `catchAllErrors'` eToView'

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
    flip catchAllErrors eToView $
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
  readTVarIO s >>= mapM_ (\(a1, a2) -> forkIO $ uninterruptibleCancel a1 >> mapM_ uninterruptibleCancel a2)
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
      cfg3 = maybe cfg2 (\t -> cfg2 {tcpTimeout = nt t, tcpConnectTimeout = nt ((t * 3) `div` 2)}) tcpTimeout_
      nt t = NetworkTimeout {backgroundTimeout = t * 3, interactiveTimeout = t}
   in cfg3 {socksProxy, socksMode, hostMode, requiredHostMode, smpWebPortServers, logTLSErrors}

useServers :: Foldable f => RandomAgentServers -> [(Text, ServerOperator)] -> f UserOperatorServers -> (NonEmpty (ServerCfg 'PSMP), NonEmpty (ServerCfg 'PXFTP))
useServers as opDomains uss =
  let smp' = useServerCfgs SPSMP as opDomains $ concatMap (servers' SPSMP) uss
      xftp' = useServerCfgs SPXFTP as opDomains $ concatMap (servers' SPXFTP) uss
   in (smp', xftp')

execChatCommand :: Maybe RemoteHostId -> ByteString -> Int -> CM' (Either ChatError ChatResponse)
execChatCommand rh s retryNum =
  case parseChatCommand s of
    Left e -> pure $ chatCmdError e
    Right cmd -> case rh of
      Just rhId
        | allowRemoteCommand cmd -> execRemoteCommand rhId cmd s retryNum
        | otherwise -> pure $ Left $ ChatErrorRemoteHost (RHId rhId) $ RHELocalCommand
      _ -> do
        cc@ChatController {config = ChatConfig {chatHooks}} <- ask
        case preCmdHook chatHooks of
          Just hook -> liftIO (hook cc cmd) >>= either pure (`execChatCommand'` retryNum)
          Nothing -> execChatCommand' cmd retryNum

execChatCommand' :: ChatCommand -> Int -> CM' (Either ChatError ChatResponse)
execChatCommand' cmd retryNum = handleCommandError $ do
  vr <- chatVersionRange
  processChatCommand vr (NRMInteractive' retryNum) cmd

execRemoteCommand :: RemoteHostId -> ChatCommand -> ByteString -> Int -> CM' (Either ChatError ChatResponse)
execRemoteCommand rhId cmd s retryNum = handleCommandError $ getRemoteHostClient rhId >>= \rh -> processRemoteCommand rhId rh cmd s retryNum

handleCommandError :: CM ChatResponse -> CM' (Either ChatError ChatResponse)
handleCommandError a = runExceptT a `E.catches` ioErrors
  where
    ioErrors =
      [ E.Handler $ \(e :: ExitCode) -> E.throwIO e,
        E.Handler $ pure . Left . fromSomeException
      ]

parseChatCommand :: ByteString -> Either String ChatCommand
parseChatCommand = A.parseOnly chatCommandP . B.dropWhileEnd isSpace

-- | Chat API commands interpreted in context of a local zone
processChatCommand :: VersionRangeChat -> NetworkRequestMode -> ChatCommand -> CM ChatResponse
processChatCommand vr nm = \case
  ShowActiveUser -> withUser' $ pure . CRActiveUser
  CreateActiveUser NewUser {profile, pastTimestamp, userChatRelay} -> do
    forM_ profile $ \Profile {displayName} -> checkValidName displayName
    p@Profile {displayName} <- liftIO $ maybe generateRandomProfile pure profile
    u <- asks currentUser
    users <- withFastStore' getUsers
    forM_ users $ \User {localDisplayName = n, activeUser, viewPwdHash, userChatRelay = userChatRelay'} -> do
      when (n == displayName) . throwChatError $
        if activeUser || isNothing viewPwdHash then CEUserExists displayName else CEInvalidDisplayName {displayName, validName = ""}
      when (userChatRelay && isTrue userChatRelay') $ throwChatError CEChatRelayExists
    (uss, (smp', xftp')) <- chooseServers =<< readTVarIO u
    auId <- withAgent $ \a -> createUser a smp' xftp'
    ts <- liftIO $ getCurrentTime >>= if pastTimestamp then coupleDaysAgo else pure
    user <- withFastStore $ \db -> do
      user <- createUserRecordAt db (AgentUserId auId) p userChatRelay True ts
      mapM_ (setUserServers db user ts) uss
      createPresetContactCards db user `catchAllErrors` \_ -> pure ()
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
      copyServers UserOperatorServers {operator, smpServers, xftpServers, chatRelays} =
        let newSrv srv = AUS SDBNew srv {serverId = DBNewEntity}
            newCRelay chatRelay = AUCR SDBNew chatRelay {chatRelayId = DBNewEntity}
         in
          UpdatedUserOperatorServers {
            operator,
            smpServers = map newSrv smpServers,
            xftpServers = map newSrv xftpServers,
            chatRelays = map newCRelay chatRelays
          }
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
    tryAllErrors (withFastStore (`getUserIdByName` uName)) >>= \case
      Left _ -> throwChatError CEUserUnknown
      Right userId -> processChatCommand vr nm $ APISetActiveUser userId viewPwd_
  SetAllContactReceipts onOff -> withUser $ \_ -> withFastStore' (`updateAllContactReceipts` onOff) >> ok_
  APISetUserContactReceipts userId' settings -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' Nothing
    withFastStore' $ \db -> updateUserContactReceipts db user' settings
    ok user
  SetUserContactReceipts settings -> withUser $ \User {userId} -> processChatCommand vr nm $ APISetUserContactReceipts userId settings
  APISetUserGroupReceipts userId' settings -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' Nothing
    withFastStore' $ \db -> updateUserGroupReceipts db user' settings
    ok user
  SetUserGroupReceipts settings -> withUser $ \User {userId} -> processChatCommand vr nm $ APISetUserGroupReceipts userId settings
  APISetUserAutoAcceptMemberContacts userId' onOff -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' Nothing
    withFastStore' $ \db -> updateUserAutoAcceptMemberContacts db user' onOff
    ok user
  SetUserAutoAcceptMemberContacts onOff -> withUser $ \User {userId} -> processChatCommand vr nm $ APISetUserAutoAcceptMemberContacts userId onOff
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
  HideUser viewPwd -> withUser $ \User {userId} -> processChatCommand vr nm $ APIHideUser userId viewPwd
  UnhideUser viewPwd -> withUser $ \User {userId} -> processChatCommand vr nm $ APIUnhideUser userId viewPwd
  MuteUser -> withUser $ \User {userId} -> processChatCommand vr nm $ APIMuteUser userId
  UnmuteUser -> withUser $ \User {userId} -> processChatCommand vr nm $ APIUnmuteUser userId
  APIDeleteUser userId' delSMPQueues viewPwd_ -> withUser $ \user -> do
    user' <- privateGetUser userId'
    validateUserPassword user user' viewPwd_
    checkDeleteChatUser user'
    withChatLock "deleteUser" $ deleteChatUser user' delSMPQueues
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
  ShowConnectionsDiff showIds -> do
    users <- withFastStore' getUsers
    let aUserIds = map aUserId users
    connIds <- concat <$> forM users getConnsToSub
    (userDiff, connDiff) <- withAgent (\a -> compareConnections a aUserIds connIds)
    pure $ CRConnectionsDiff showIds (AgentUserId <$> userDiff) (AgentConnId <$> connDiff)
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
    processChatCommand vr nm $ APIExportArchive $ ArchiveConfig filePath Nothing Nothing
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
  APIGetChat (ChatRef cType cId scope_) contentFilter pagination search -> withUser $ \user -> case cType of
    -- TODO optimize queries calculating ChatStats, currently they're disabled
    CTDirect -> do
      (directChat, navInfo) <- withFastStore (\db -> getDirectChat db vr user cId contentFilter pagination search)
      pure $ CRApiChat user (AChat SCTDirect directChat) navInfo
    CTGroup -> do
      (groupChat, navInfo) <- withFastStore (\db -> getGroupChat db vr user cId scope_ contentFilter pagination search)
      groupChat' <- checkSupportChatAttention user groupChat
      pure $ CRApiChat user (AChat SCTGroup groupChat') navInfo
    CTLocal -> do
      (localChat, navInfo) <- withFastStore (\db -> getLocalChat db user cId contentFilter pagination search)
      pure $ CRApiChat user (AChat SCTLocal localChat) navInfo
    CTContactRequest -> throwCmdError "not implemented"
    CTContactConnection -> throwCmdError "not supported"
    where
      checkSupportChatAttention :: User -> Chat 'CTGroup -> CM (Chat 'CTGroup)
      checkSupportChatAttention user groupChat@Chat {chatInfo, chatItems} =
        case chatInfo of
          GroupChat gInfo (Just GCSIMemberSupport {groupMember_ = Just scopeMem@GroupMember {supportChat = Just suppChat}}) -> do
            case correctedMemAttention (groupMemberId' scopeMem) suppChat chatItems of
              Just newMemAttention -> do
                (gInfo', scopeMem') <-
                  withFastStore' $ \db -> setSupportChatMemberAttention db vr user gInfo scopeMem newMemAttention
                pure (groupChat {chatInfo = GroupChat gInfo' (Just $ GCSIMemberSupport (Just scopeMem'))} :: Chat 'CTGroup)
              Nothing -> pure groupChat
          _ -> pure groupChat
        where
          correctedMemAttention :: GroupMemberId -> GroupSupportChat -> [CChatItem 'CTGroup] -> Maybe Int64
          correctedMemAttention scopeGMId GroupSupportChat {memberAttention} items =
            let numNewFromMember = fromIntegral . length . takeWhile newFromMember $ reverse items
             in if numNewFromMember == memberAttention then Nothing else Just numNewFromMember
            where
              newFromMember :: CChatItem 'CTGroup -> Bool
              newFromMember (CChatItem _ ChatItem {chatDir = CIGroupRcv m, meta = CIMeta {itemStatus = CISRcvNew}}) =
                groupMemberId' m == scopeGMId
              newFromMember _ = False
  APIGetChatContentTypes chatRef -> withUser $ \user ->
    CRChatContentTypes <$> withStore (\db -> getChatContentTypes db user chatRef)
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
          Just <$> withFastStore (\db -> getAChatItem db vr user (ChatRef CTDirect ctId Nothing) fwdItemId)
        Just (CIFFGroup _ _ (Just gId) (Just fwdItemId)) ->
          -- TODO [knocking] getAChatItem doesn't differentiate how to read based on scope - it should, instead of using group filter
          Just <$> withFastStore (\db -> getAChatItem db vr user (ChatRef CTGroup gId Nothing) fwdItemId)
        _ -> pure Nothing
  APISendMessages sendRef live itemTTL cms -> withUser $ \user -> mapM_ assertAllowedContent' cms >> case sendRef of
    SRDirect chatId -> do
      mapM_ assertNoMentions cms
      withContactLock "sendMessage" chatId $
        sendContactContentMessages user chatId live itemTTL (L.map composedMessageReq cms)
    SRGroup chatId gsScope ->
      withGroupLock "sendMessage" chatId $ do
        (gInfo, cmrs) <- withFastStore $ \db -> do
          g <- getGroupInfo db vr user chatId
          (g,) <$> mapM (composedMessageReqMentions db user g) cms
        sendGroupContentMessages user gInfo gsScope live itemTTL cmrs
  APICreateChatTag (ChatTagData emoji text) -> withUser $ \user -> withFastStore' $ \db -> do
    _ <- createChatTag db user emoji text
    CRChatTags user <$> getUserChatTags db user
  APISetChatTags (ChatRef cType chatId scope) tagIds -> withUser $ \user -> case cType of
    CTDirect -> withFastStore' $ \db -> do
      updateDirectChatTags db chatId (maybe [] L.toList tagIds)
      CRTagsUpdated user <$> getUserChatTags db user <*> getDirectChatTags db chatId
    CTGroup | isNothing scope -> withFastStore' $ \db -> do
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
      gInfo <- withFastStore $ \db -> getGroupInfo db vr user gId
      let mc = MCReport reportText reportReason
          cm = ComposedMessage {fileSource = Nothing, quotedItemId = Just reportedItemId, msgContent = mc, mentions = M.empty}
      sendGroupContentMessages user gInfo (Just $ GCSMemberSupport Nothing) False Nothing [composedMessageReq cm]
  ReportMessage {groupName, contactName_, reportReason, reportedMessage} -> withUser $ \user -> do
    gId <- withFastStore $ \db -> getGroupIdByName db user groupName
    reportedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user gId contactName_ reportedMessage
    processChatCommand vr nm $ APIReportMessage gId reportedItemId reportReason ""
  APIUpdateChatItem (ChatRef cType chatId scope) itemId live (UpdatedMessage mc mentions) -> withUser $ \user -> assertAllowedContent mc >> case cType of
    CTDirect -> withContactLock "updateChatItem" chatId $ do
      unless (null mentions) $ throwCmdError "mentions are not supported in this chat"
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
                  let event = XMsgUpdate itemSharedMId mc M.empty (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive) Nothing
                  (SndMessage {msgId}, _) <- sendDirectContactMessage user ct event
                  ci' <- withFastStore' $ \db -> do
                    currentTs <- liftIO getCurrentTime
                    when changed $
                      addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
                    let edited = itemLive /= Just True
                    updateDirectChatItem' db user contactId ci (CISndMsgContent mc) edited live Nothing $ Just msgId
                  startUpdatedTimedItemThread user (ChatRef CTDirect contactId Nothing) ci ci'
                  pure $ CRChatItemUpdated user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci')
                else pure $ CRChatItemNotChanged user (AChatItem SCTDirect SMDSnd (DirectChat ct) ci)
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTGroup -> withGroupLock "updateChatItem" chatId $ do
      gInfo@GroupInfo {groupId, membership} <- withFastStore $ \db -> getGroupInfo db vr user chatId
      when (isNothing scope) $ assertUserGroupRole gInfo GRAuthor
      let (_, ft_) = msgContentTexts mc
      if prohibitedSimplexLinks gInfo membership ft_
        then throwCmdError ("feature not allowed " <> T.unpack (groupFeatureNameText GFSimplexLinks))
        else do
          -- TODO [knocking] check chat item scope?
          cci <- withFastStore $ \db -> getGroupCIWithReactions db user gInfo itemId
          case cci of
            CChatItem SMDSnd ci@ChatItem {meta = CIMeta {itemSharedMsgId, itemTimed, itemLive, editable}, content = ciContent} -> do
              case (ciContent, itemSharedMsgId, editable) of
                (CISndMsgContent oldMC, Just itemSharedMId, True) -> do
                  chatScopeInfo <- mapM (getChatScopeInfo vr user) scope
                  recipients <- getGroupRecipients vr user gInfo chatScopeInfo groupKnockingVersion
                  let changed = mc /= oldMC
                  if changed || fromMaybe False itemLive
                    then do
                      ciMentions <- withFastStore $ \db -> getCIMentions db user gInfo ft_ mentions
                      let msgScope = toMsgScope gInfo <$> chatScopeInfo
                          mentions' = M.map (\CIMention {memberId} -> MsgMention {memberId}) ciMentions
                          event = XMsgUpdate itemSharedMId mc mentions' (ttl' <$> itemTimed) (justTrue . (live &&) =<< itemLive) msgScope
                      SndMessage {msgId} <- sendGroupMessage user gInfo scope recipients event
                      ci' <- withFastStore' $ \db -> do
                        currentTs <- liftIO getCurrentTime
                        when changed $
                          addInitialAndNewCIVersions db itemId (chatItemTs' ci, oldMC) (currentTs, mc)
                        let edited = itemLive /= Just True
                        ci' <- updateGroupChatItem db user groupId ci (CISndMsgContent mc) edited live $ Just msgId
                        updateGroupCIMentions db gInfo ci' ciMentions
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId scope) ci ci'
                      pure $ CRChatItemUpdated user (AChatItem SCTGroup SMDSnd (GroupChat gInfo chatScopeInfo) ci')
                    else pure $ CRChatItemNotChanged user (AChatItem SCTGroup SMDSnd (GroupChat gInfo chatScopeInfo) ci)
                _ -> throwChatError CEInvalidChatItemUpdate
            CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTLocal -> do
      unless (null mentions) $ throwCmdError "mentions are not supported in this chat"
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
  APIDeleteChatItem (ChatRef cType chatId scope) itemIds mode -> withUser $ \user -> case cType of
    CTDirect -> withContactLock "deleteChatItem" chatId $ do
      (ct, items) <- getCommandDirectChatItems user chatId itemIds
      deletions <- case mode of
        CIDMInternal -> deleteDirectCIs user ct items
        CIDMInternalMark -> markDirectCIsDeleted user ct items =<< liftIO getCurrentTime
        CIDMBroadcast -> do
          assertDeletable items
          assertDirectAllowed user MDSnd ct XMsgDel_
          let msgIds = itemsMsgIds items
              events = map (\msgId -> XMsgDel msgId Nothing Nothing) msgIds
          forM_ (L.nonEmpty events) $ \events' ->
            sendDirectContactMessages user ct events'
          if featureAllowed SCFFullDelete forUser ct
            then deleteDirectCIs user ct items
            else markDirectCIsDeleted user ct items =<< liftIO getCurrentTime
      pure $ CRChatItemsDeleted user deletions True False
    CTGroup -> withGroupLock "deleteChatItem" chatId $ do
      (gInfo, items) <- getCommandGroupChatItems user chatId itemIds
      -- TODO [knocking] check scope for all items?
      chatScopeInfo <- mapM (getChatScopeInfo vr user) scope
      deletions <- case mode of
        CIDMInternal -> do
          deleteGroupCIs user gInfo chatScopeInfo items Nothing =<< liftIO getCurrentTime
        CIDMInternalMark -> do
          markGroupCIsDeleted user gInfo chatScopeInfo items Nothing =<< liftIO getCurrentTime
        CIDMBroadcast -> do
          recipients <- getGroupRecipients vr user gInfo chatScopeInfo groupKnockingVersion
          assertDeletable items
          assertUserGroupRole gInfo GRObserver -- can still delete messages sent earlier
          let msgIds = itemsMsgIds items
              events = L.nonEmpty $ map (\msgId -> XMsgDel msgId Nothing $ toMsgScope gInfo <$> chatScopeInfo) msgIds
          mapM_ (sendGroupMessages user gInfo Nothing recipients) events
          -- TODO delGroupChatItems sends deletion events too. Are they needed?
          delGroupChatItems user gInfo chatScopeInfo items False
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
    -- TODO [knocking] check scope is Nothing for all items? (prohibit moderation in support chats?)
    ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
    let recipients = filter memberCurrent ms
    deletions <- delGroupChatItemsForMembers user gInfo Nothing recipients items
    pure $ CRChatItemsDeleted user deletions True False
  APIArchiveReceivedReports gId -> withUser $ \user -> withFastStore $ \db -> do
    g <- getGroupInfo db vr user gId
    deleteTs <- liftIO getCurrentTime
    ciIds <- liftIO $ markReceivedGroupReportsDeleted db user g deleteTs
    pure $ CRGroupChatItemsDeleted user g ciIds True (Just $ membership g)
  APIDeleteReceivedReports gId itemIds mode -> withUser $ \user -> withGroupLock "deleteReports" gId $ do
    (gInfo, items) <- getCommandGroupChatItems user gId itemIds
    unless (all isRcvReport items) $ throwCmdError "some items are not received reports"
    -- TODO [knocking] scope can be different for each item if reports are from different members
    -- TODO            (currently we pass Nothing as scope which is wrong)
    deletions <- case mode of
      CIDMInternal -> deleteGroupCIs user gInfo Nothing items Nothing =<< liftIO getCurrentTime
      CIDMInternalMark -> markGroupCIsDeleted user gInfo Nothing items Nothing =<< liftIO getCurrentTime
      CIDMBroadcast -> do
        ms <- withFastStore' $ \db -> getGroupModerators db vr user gInfo
        let recipients = filter memberCurrent ms
        delGroupChatItemsForMembers user gInfo Nothing recipients items
    pure $ CRChatItemsDeleted user deletions True False
    where
      isRcvReport = \case
        CChatItem _ ChatItem {content = CIRcvMsgContent (MCReport {})} -> True
        _ -> False
  APIChatItemReaction (ChatRef cType chatId scope) itemId add reaction -> withUser $ \user -> case cType of
    CTDirect ->
      withContactLock "chatItemReaction" chatId $
        withFastStore (\db -> (,) <$> getContact db vr user chatId <*> getDirectChatItem db user chatId itemId) >>= \case
          (ct, CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}}) -> do
            unless (featureAllowed SCFReactions forUser ct) $
              throwCmdError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions)
            unless (ciReactionAllowed ci) $
              throwCmdError "reaction not allowed - chat item has no content"
            rs <- withFastStore' $ \db -> getDirectReactions db ct itemSharedMId True
            checkReactionAllowed rs
            (SndMessage {msgId}, _) <- sendDirectContactMessage user ct $ XMsgReact itemSharedMId Nothing Nothing reaction add
            createdAt <- liftIO getCurrentTime
            reactions <- withFastStore' $ \db -> do
              setDirectReaction db ct itemSharedMId True reaction add msgId createdAt
              liftIO $ getDirectCIReactions db ct itemSharedMId
            let ci' = CChatItem md ci {reactions}
                r = ACIReaction SCTDirect SMDSnd (DirectChat ct) $ CIReaction CIDirectSnd ci' createdAt reaction
            pure $ CRChatItemReaction user add r
          _ -> throwCmdError "reaction not possible - no shared item ID"
    CTGroup ->
      withGroupLock "chatItemReaction" chatId $ do
        -- TODO [knocking] check chat item scope?
        (g@GroupInfo {membership}, CChatItem md ci) <- withFastStore $ \db -> do
          g <- getGroupInfo db vr user chatId
          (g,) <$> getGroupCIWithReactions db user g itemId
        chatScopeInfo <- mapM (getChatScopeInfo vr user) scope
        recipients <- getGroupRecipients vr user g chatScopeInfo groupKnockingVersion
        case ci of
          ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}} -> do
            unless (groupFeatureAllowed SGFReactions g) $
              throwCmdError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFReactions)
            unless (ciReactionAllowed ci) $
              throwCmdError "reaction not allowed - chat item has no content"
            let GroupMember {memberId = itemMemberId} = chatItemMember g ci
            rs <- withFastStore' $ \db -> getGroupReactions db g membership itemMemberId itemSharedMId True
            checkReactionAllowed rs
            SndMessage {msgId} <- sendGroupMessage user g scope recipients (XMsgReact itemSharedMId (Just itemMemberId) (toMsgScope g <$> chatScopeInfo) reaction add)
            createdAt <- liftIO getCurrentTime
            reactions <- withFastStore' $ \db -> do
              setGroupReaction db g membership itemMemberId itemSharedMId True reaction add msgId createdAt
              liftIO $ getGroupCIReactions db g itemMemberId itemSharedMId
            let ci' = CChatItem md ci {reactions}
                r = ACIReaction SCTGroup SMDSnd (GroupChat g chatScopeInfo) $ CIReaction CIGroupSnd ci' createdAt reaction
            pure $ CRChatItemReaction user add r
          _ -> throwCmdError "invalid reaction"
    CTLocal -> throwCmdError "not supported"
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
    where
      checkReactionAllowed rs = do
        when ((reaction `elem` rs) == add) $
          throwCmdError $ "reaction already " <> if add then "added" else "removed"
        when (add && length rs >= maxMsgReactions) $
          throwCmdError "too many reactions"
  APIGetReactionMembers userId groupId itemId reaction -> withUserId userId $ \user -> do
    memberReactions <- withStore $ \db -> do
      CChatItem _ ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}} <- getGroupChatItem db user groupId itemId
      liftIO $ getReactionMembers db vr user groupId itemSharedMId reaction
    pure $ CRReactionMembers user memberReactions
  -- TODO [knocking] forward from scope?
  APIPlanForwardChatItems (ChatRef fromCType fromChatId _scope) itemIds -> withUser $ \user -> case fromCType of
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
                MCChat {} -> True
                MCUnknown {} -> True
  -- TODO [knocking] forward from / to scope
  APIForwardChatItems toChat@(ChatRef toCType toChatId toScope) fromChat@(ChatRef fromCType fromChatId _fromScope) itemIds itemTTL -> withUser $ \user -> case toCType of
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
            sendGroupContentMessages user gInfo toScope False itemTTL cmrs'
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
        -- TODO [knocking] from scope?
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
        CTContactRequest -> throwCmdError "not supported"
        CTContactConnection -> throwCmdError "not supported"
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
  UserRead -> withUser $ \User {userId} -> processChatCommand vr nm $ APIUserRead userId
  APIChatRead chatRef@(ChatRef cType chatId scope_) -> withUser $ \_ -> case cType of
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
      (user, gInfo) <- withFastStore $ \db -> do
        user <- getUserByGroupId db chatId
        gInfo <- getGroupInfo db vr user chatId
        pure (user, gInfo)
      ts <- liftIO getCurrentTime
      case scope_ of
        Nothing -> do
          timedItems <- withFastStore' $ \db -> do
            timedItems <- getGroupUnreadTimedItems db user chatId Nothing
            updateGroupChatItemsRead db user gInfo
            setGroupChatItemsDeleteAt db user chatId timedItems ts
          forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
          ok user
        Just scope -> do
          scopeInfo <- getChatScopeInfo vr user scope
          (gInfo', m', timedItems) <- withFastStore' $ \db -> do
            timedItems <- getGroupUnreadTimedItems db user chatId (Just scope)
            (gInfo', m') <- updateSupportChatItemsRead db vr user gInfo scopeInfo
            timedItems' <- setGroupChatItemsDeleteAt db user chatId timedItems ts
            pure (gInfo', m', timedItems')
          forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
          pure $ CRMemberSupportChatRead user gInfo' m'
    CTLocal -> do
      user <- withFastStore $ \db -> getUserByNoteFolderId db chatId
      withFastStore' $ \db -> updateLocalChatItemsRead db user chatId
      ok user
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
  APIChatItemsRead chatRef@(ChatRef cType chatId scope) itemIds -> withUser $ \_ -> case cType of
    CTDirect -> do
      (user, ct) <- withFastStore $ \db -> do
        user <- getUserByContactId db chatId
        ct <- getContact db vr user chatId
        pure (user, ct)
      timedItems <- withFastStore' $ \db -> do
        timedItems <- updateDirectChatItemsReadList db user chatId itemIds
        setDirectChatItemsDeleteAt db user chatId timedItems =<< getCurrentTime
      forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
      pure $ CRItemsReadForChat user (AChatInfo SCTDirect $ DirectChat ct)
    CTGroup -> do
      (user, gInfo) <- withFastStore $ \db -> do
        user <- getUserByGroupId db chatId
        gInfo <- getGroupInfo db vr user chatId
        pure (user, gInfo)
      chatScopeInfo <- mapM (getChatScopeInfo vr user) scope
      (timedItems, gInfo') <- withFastStore $ \db -> do
        (timedItems, gInfo') <- updateGroupChatItemsReadList db vr user gInfo chatScopeInfo itemIds
        timedItems' <- liftIO $ setGroupChatItemsDeleteAt db user chatId timedItems =<< getCurrentTime
        pure (timedItems', gInfo')
      forM_ timedItems $ \(itemId, deleteAt) -> startProximateTimedItemThread user (chatRef, itemId) deleteAt
      pure $ CRItemsReadForChat user (AChatInfo SCTGroup $ GroupChat gInfo' Nothing)
    CTLocal -> throwCmdError "not supported"
    CTContactRequest -> throwCmdError "not supported"
    CTContactConnection -> throwCmdError "not supported"
  APIChatUnread (ChatRef cType chatId scope) unreadChat -> withUser $ \user -> case cType of
    CTDirect -> do
      withFastStore $ \db -> do
        ct <- getContact db vr user chatId
        liftIO $ updateContactUnreadChat db user ct unreadChat
      ok user
    -- TODO [knocking] set support chat as unread?
    CTGroup | isNothing scope -> do
      withFastStore $ \db -> do
        gInfo <- getGroupInfo db vr user chatId
        liftIO $ updateGroupUnreadChat db user gInfo unreadChat
      ok user
    CTLocal -> do
      withFastStore $ \db -> do
        nf <- getNoteFolder db user chatId
        liftIO $ updateNoteFolderUnreadChat db user nf unreadChat
      ok user
    _ -> throwCmdError "not supported"
  APIDeleteChat cRef@(ChatRef cType chatId scope) cdm -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> getContact db vr user chatId
      filesInfo <- withFastStore' $ \db -> getContactFileInfo db user ct
      withContactLock "deleteChat direct" chatId $
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
            void $ processChatCommand vr nm $ APIClearChat cRef
            withFastStore' $ \db -> setContactChatDeleted db user ct True
            pure $ CRContactDeleted user ct {chatDeleted = True}
      where
        sendDelDeleteConns ct notify = do
          let doSendDel = contactReady ct && contactActive ct && notify
          when doSendDel $ void (sendDirectContactMessage user ct XDirectDel) `catchAllErrors` const (pure ())
          contactConnIds <- map aConnId <$> withFastStore' (\db -> getContactConnections db vr userId ct)
          deleteAgentConnectionsAsync' contactConnIds doSendDel
    CTContactConnection -> withConnectionLock "deleteChat contactConnection" chatId $ do
      conn@PendingContactConnection {pccAgentConnId = AgentConnId acId} <- withFastStore $ \db -> getPendingContactConnection db userId chatId
      deleteAgentConnectionAsync acId
      withFastStore' $ \db -> deletePendingContactConnection db userId chatId
      pure $ CRContactConnectionDeleted user conn
    CTGroup | isNothing scope -> do
      gInfo@GroupInfo {membership} <- withFastStore $ \db -> getGroupInfo db vr user chatId
      let isOwner = memberRole' membership == GROwner
          canDelete = isOwner || not (memberCurrent membership)
      unless canDelete $ throwChatError $ CEGroupUserRole gInfo GROwner
      filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
      withGroupLock "deleteChat group" chatId $ do
        deleteCIFiles user filesInfo
        (members, recipients) <- getRecipients gInfo
        let doSendDel = memberActive membership && isOwner
        when doSendDel . void $ sendGroupMessage' user gInfo recipients XGrpDel
        deleteGroupLinkIfExists user gInfo
        deleteMembersConnections' user members doSendDel
        updateCIGroupInvitationStatus user gInfo CIGISRejected `catchAllErrors` \_ -> pure ()
        withFastStore' $ \db -> deleteGroupChatItems db user gInfo
        withFastStore' $ \db -> cleanupHostGroupLinkConn db user gInfo
        withFastStore' $ \db -> deleteGroupMembers db user gInfo
        withFastStore' $ \db -> deleteGroup db user gInfo
        pure $ CRGroupDeletedUser user gInfo
        where
          getRecipients gInfo
            | useRelays' gInfo = do
                relays <- withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo
                pure (relays, relays)
            | otherwise = do
                ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
                pure (ms, filter memberCurrentOrPending ms)
    _ -> throwCmdError "not supported"
  APIClearChat (ChatRef cType chatId scope) -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> getContact db vr user chatId
      filesInfo <- withFastStore' $ \db -> getContactFileInfo db user ct
      deleteCIFiles user filesInfo
      withFastStore' $ \db -> deleteContactCIs db user ct
      pure $ CRChatCleared user (AChatInfo SCTDirect $ DirectChat ct)
    CTGroup | isNothing scope -> do
      gInfo <- withFastStore $ \db -> getGroupInfo db vr user chatId
      filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
      deleteCIFiles user filesInfo
      withFastStore' $ \db -> deleteGroupChatItemsMessages db user gInfo
      membersToDelete <- withFastStore' $ \db -> getGroupMembersForExpiration db vr user gInfo
      forM_ membersToDelete $ \m -> withFastStore' $ \db -> deleteGroupMember db user m
      pure $ CRChatCleared user (AChatInfo SCTGroup $ GroupChat gInfo Nothing)
    CTLocal -> do
      nf <- withFastStore $ \db -> getNoteFolder db user chatId
      filesInfo <- withFastStore' $ \db -> getNoteFolderFileInfo db user nf
      deleteFilesLocally filesInfo
      withFastStore' $ \db -> deleteNoteFolderFiles db userId nf
      withFastStore' $ \db -> deleteNoteFolderCIs db user nf
      pure $ CRChatCleared user (AChatInfo SCTLocal $ LocalChat nf)
    _ -> throwCmdError "not supported"
  APIAcceptContact incognito connReqId -> withUser $ \user@User {userId} -> do
    uclData_ <- withFastStore $ \db -> do
      uclId_ <- getUserContactLinkIdByCReq db connReqId
      forM uclId_ $ \uclId -> do -- address may be deleted
        uclGLinkInfo <- getUserContactLinkById db userId uclId
        pure (uclId, uclGLinkInfo)
    withContactRequestLock "acceptContact" connReqId $ case uclData_ of
      Nothing -> do -- address was deleted
        when incognito $ throwCmdError "incognito not allowed when address is not found"
        cReq <- withFastStore $ \db -> getContactRequest db user connReqId
        (ct, _sqSecured) <- acceptCReq user cReq True
        pure $ CRAcceptingContactRequest user ct
      Just (uclId, (ucl@UserContactLink {shortLinkDataSet}, gLinkInfo_)) -> do
        when (shortLinkDataSet && incognito) $ throwCmdError "incognito not allowed for address with short link data"
        withUserContactLock "acceptContact" uclId $ do
          cReq <- withFastStore $ \db -> getContactRequest db user connReqId
          let contactUsed = isNothing gLinkInfo_ -- for redundancy, as group link requests are auto-accepted
          (ct, sqSecured) <- acceptCReq user cReq contactUsed
          when sqSecured $ sendWelcomeMsg user ct ucl cReq
          pure $ CRAcceptingContactRequest user ct
    where
      acceptCReq user cReq contactUsed = do
        (ct, conn, sqSecured) <- acceptContactRequest nm user cReq incognito
        ct' <- withStore' $ \db -> do
          updateContactAccepted db user ct contactUsed
          conn' <-
            if sqSecured
              then updateConnectionStatusFromTo db conn ConnNew ConnSndReady
              else pure conn
          pure ct {contactUsed, activeConn = Just conn'}
        pure (ct', sqSecured)
      sendWelcomeMsg user ct ucl UserContactRequest {welcomeSharedMsgId} =
        forM_ (autoReply $ addressSettings ucl) $ \mc -> case welcomeSharedMsgId of
          Just smId ->
            void $ sendDirectContactMessage user ct $ XMsgUpdate smId mc M.empty Nothing Nothing Nothing
          Nothing -> do
            (msg, _) <- sendDirectContactMessage user ct $ XMsgNew $ MCSimple $ extMsgContent mc Nothing
            ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]
  APIRejectContact connReqId -> withUser $ \user -> do
    uclId_ <- withFastStore $ \db -> getUserContactLinkIdByCReq db connReqId
    withContactRequestLock "rejectContact" connReqId $ case uclId_ of
      Nothing -> rejectCReq user -- address was deleted
      Just uclId -> withUserContactLock "rejectContact" uclId $ rejectCReq user
    where
      rejectCReq user = do
        (cReq@UserContactRequest {agentInvitationId = AgentInvId invId}, ct_) <-
          withFastStore $ \db -> do
            cReq@UserContactRequest {contactId_} <- getContactRequest db user connReqId
            ct_ <- forM contactId_ $ \contactId -> do
              ct <- getContact db vr user contactId
              deleteContact db user ct
              pure ct
            liftIO $ deleteContactRequest db user connReqId
            pure (cReq, ct_)
        withAgent (`rejectContact` invId)
        pure $ CRContactRequestRejected user cReq ct_
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
      else throwCmdError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFCalls)
  SendCallInvitation cName callType -> withUser $ \user -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user cName
    processChatCommand vr nm $ APISendCallInvitation contactId callType
  APIRejectCall contactId ->
    -- party accepting call
    withCurrentCall contactId $ \user ct Call {chatItemId, callState} -> case callState of
      CallInvitationReceived {} -> do
        let aciContent = ACIContent SMDRcv $ CIRcvCall CISCallRejected 0
        withFastStore' $ \db -> setDirectChatItemRead db user contactId chatItemId
        timed_ <- contactCITimed ct
        updateDirectChatItemView user ct chatItemId aciContent False False timed_ Nothing
        forM_ (timed_ >>= timedDeleteAt') $
          startProximateTimedItemThread user (ChatRef CTDirect contactId Nothing, chatItemId)
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
  APISetChatUIThemes (ChatRef cType chatId scope) uiThemes -> withUser $ \user -> case cType of
    CTDirect -> do
      withFastStore $ \db -> do
        ct <- getContact db vr user chatId
        liftIO $ setContactUIThemes db user ct uiThemes
      ok user
    CTGroup | isNothing scope -> do
      withFastStore $ \db -> do
        g <- getGroupInfo db vr user chatId
        liftIO $ setGroupUIThemes db user g uiThemes
      ok user
    _ -> throwCmdError "not supported"
  APIGetNtfToken -> withUser' $ \_ -> crNtfToken <$> withAgent getNtfToken
  APIRegisterToken token mode -> withUser $ \_ ->
    CRNtfTokenStatus <$> withAgent (\a -> registerNtfToken a nm token mode)
  APIVerifyToken token nonce code -> withUser $ \_ -> withAgent (\a -> verifyNtfToken a nm token nonce code) >> ok_
  APICheckToken token -> withUser $ \_ ->
    CRNtfTokenStatus <$> withAgent (\a -> checkNtfToken a nm token)
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
    liftIO $ CRUserServers user <$> groupByOperator (onlyProtocolServers p srvs)
  SetUserProtoServers (AProtocolType (p :: SProtocolType p)) srvs -> withUser $ \user@User {userId} -> withServerProtocol p $ do
    userServers_ <- liftIO . groupByOperator =<< withFastStore (`getUserServers` user)
    case L.nonEmpty userServers_ of
      Nothing -> throwCmdError "no servers"
      Just userServers -> case srvs of
        [] -> throwCmdError "no servers"
        _ -> do
          srvs' <- mapM aUserServer srvs
          processChatCommand vr nm $ APISetUserServers userId $ L.map (updatedServers p srvs') userServers
    where
      aUserServer :: AProtoServerWithAuth -> CM (AUserServer p)
      aUserServer (AProtoServerWithAuth p' srv) = case testEquality p p' of
        Just Refl -> pure $ AUS SDBNew $ newUserServer srv
        Nothing -> throwCmdError $ "incorrect server protocol: " <> B.unpack (strEncode srv)
  APITestProtoServer userId srv@(AProtoServerWithAuth _ server) -> withUserId userId $ \user ->
    lift $ CRServerTestResult user srv <$> withAgent' (\a -> testProtocolServer a nm (aUserId user) server)
  TestProtoServer srv -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APITestProtoServer userId srv
  GetUserChatRelays -> withUser $ \user -> do
    srvs <- withFastStore (`getUserServers` user)
    liftIO $ CRUserServers user <$> groupByOperator (onlyRelays srvs)
  SetUserChatRelays relays -> withUser $ \user@User {userId} -> do
    userServers_ <- liftIO . groupByOperator =<< withFastStore (`getUserServers` user)
    case L.nonEmpty userServers_ of
      Nothing -> throwCmdError "no relays"
      Just userServers -> case relays of
        [] -> throwCmdError "no relays"
        _ -> do
          let relays' = map aUserRelay relays
          processChatCommand vr nm $ APISetUserServers userId $ L.map (updatedRelays relays') userServers
    where
      aUserRelay :: CLINewRelay -> AUserChatRelay
      aUserRelay CLINewRelay {address, name} = AUCR SDBNew $ newChatRelay name [""] address
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
        chatRelays <- getChatRelays db user
        uss <- groupByOperator (ops, smpSrvs, xftpSrvs, chatRelays)
        pure $ (aUserId user,) $ useServers as opDomains uss
  SetServerOperators operatorsRoles -> do
    ops <- serverOperators <$> withFastStore getServerOperators
    ops' <- mapM (updateOp ops) operatorsRoles
    processChatCommand vr nm $ APISetServerOperators ops'
    where
      updateOp :: [ServerOperator] -> ServerOperatorRoles -> CM ServerOperator
      updateOp ops r =
        case find (\ServerOperator {operatorId = DBEntityId opId} -> operatorId' r == opId) ops of
          Just op -> pure op {enabled = enabled' r, smpRoles = smpRoles' r, xftpRoles = xftpRoles' r}
          Nothing -> throwError $ ChatErrorStore $ SEOperatorNotFound $ operatorId' r
  APIGetUserServers userId -> withUserId userId $ \user -> withFastStore $ \db -> do
    CRUserServers user <$> (liftIO . groupByOperator =<< getUserServers db user)
  APISetUserServers userId userServers -> withUserId userId $ \user -> do
    (errors, warnings) <- validateAllUsersServers userId $ L.toList userServers
    unless (null errors) $ throwCmdError $ "user servers validation error(s): " <> show errors
    unless (null warnings) $ logWarn $ "user servers validation warning(s): " <> tshow warnings
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
    uncurry (CRUserServersValidation user) <$> validateAllUsersServers userId userServers
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
  APISetChatTTL userId (ChatRef cType chatId scope) newTTL_ ->
    withUserId userId $ \user -> checkStoreNotChanged $ withChatLock "setChatTTL" $ do
      (oldTTL_, globalTTL, ttlCount) <- withStore' $ \db ->
        (,,) <$> getSetChatTTL db <*> getChatItemTTL db user <*> getChatTTLCount db user
      let newTTL = fromMaybe globalTTL newTTL_
          oldTTL = fromMaybe globalTTL oldTTL_
      when (newTTL > 0 && (newTTL < oldTTL || oldTTL == 0)) $ do
        lift $ setExpireCIFlag user False
        expireChat user globalTTL `catchAllErrors` eToView
      lift $ setChatItemsExpiration user globalTTL ttlCount
      ok user
    where
      getSetChatTTL db = case cType of
        CTDirect -> getDirectChatTTL db chatId <* setDirectChatTTL db chatId newTTL_
        CTGroup | isNothing scope -> getGroupChatTTL db chatId <* setGroupChatTTL db chatId newTTL_
        _ -> pure Nothing
      expireChat user globalTTL = do
        currentTs <- liftIO getCurrentTime
        case cType of
          CTDirect -> expireContactChatItems user vr globalTTL chatId
          CTGroup | isNothing scope ->
            let createdAtCutoff = addUTCTime (-43200 :: NominalDiffTime) currentTs
              in expireGroupChatItems user vr globalTTL createdAtCutoff chatId
          _ -> throwCmdError "not supported"
  SetChatTTL chatName newTTL -> withUser' $ \user@User {userId} -> do
    chatRef <- getChatRef user chatName
    processChatCommand vr nm $ APISetChatTTL userId chatRef newTTL
  GetChatTTL chatName -> withUser' $ \user -> do
    -- TODO [knocking] support scope in CLI apis
    ChatRef cType chatId _ <- getChatRef user chatName
    ttl <- case cType of
      CTDirect -> withFastStore' (`getDirectChatTTL` chatId)
      CTGroup -> withFastStore' (`getGroupChatTTL` chatId)
      _ -> throwCmdError "not supported"
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
    processChatCommand vr nm $ APISetChatItemTTL userId newTTL_
  APIGetChatItemTTL userId -> withUserId' userId $ \user -> do
    ttl <- withFastStore' (`getChatItemTTL` user)
    pure $ CRChatItemTTL user (Just ttl)
  GetChatItemTTL -> withUser' $ \User {userId} -> do
    processChatCommand vr nm $ APIGetChatItemTTL userId
  APISetNetworkConfig cfg -> withUser' $ \_ -> lift (withAgent' (`setNetworkConfig` cfg)) >> ok_
  APIGetNetworkConfig -> withUser' $ \_ ->
    CRNetworkConfig <$> lift getNetworkConfig
  SetNetworkConfig simpleNetCfg -> do
    cfg <- (`updateNetworkConfig` simpleNetCfg) <$> lift getNetworkConfig
    void . processChatCommand vr nm $ APISetNetworkConfig cfg
    pure $ CRNetworkConfig cfg
  APISetNetworkInfo info -> lift (withAgent' (`setUserNetworkInfo` info)) >> ok_
  ReconnectAllServers -> withUser' $ \_ -> lift (withAgent' reconnectAllServers) >> ok_
  ReconnectServer userId srv -> withUserId userId $ \user -> do
    lift (withAgent' $ \a -> reconnectSMPServer a (aUserId user) srv)
    ok_
  APISetChatSettings (ChatRef cType chatId scope) chatSettings -> withUser $ \user -> case cType of
    CTDirect -> do
      ct <- withFastStore $ \db -> do
        ct <- getContact db vr user chatId
        liftIO $ updateContactSettings db user chatId chatSettings
        pure ct
      forM_ (contactConnId ct) $ \connId ->
        withAgent $ \a -> toggleConnectionNtfs a connId (chatHasNtfs chatSettings)
      ok user
    CTGroup | isNothing scope -> do
      ms <- withFastStore $ \db -> do
        gInfo <- getGroupInfo db vr user chatId
        ms <- liftIO $ getMembers db gInfo
        liftIO $ updateGroupSettings db user chatId chatSettings
        pure ms
      forM_ (filter memberActive ms) $ \m -> forM_ (memberConnId m) $ \connId ->
        withAgent (\a -> toggleConnectionNtfs a connId $ chatHasNtfs chatSettings) `catchAllErrors` eToView
      ok user
      where
        getMembers db gInfo
          | useRelays' gInfo = getGroupRelayMembers db vr user gInfo
          | otherwise = getGroupMembers db vr user gInfo
    _ -> throwCmdError "not supported"
  APISetMemberSettings gId gMemberId settings -> withUser $ \user -> do
    m <- withFastStore $ \db -> do
      liftIO $ updateGroupMemberSettings db user gId gMemberId settings
      getGroupMember db vr user gId gMemberId
    let ntfOn = not (memberBlocked m)
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
  APIGroupInfo gId -> withUser $ \user ->
    CRGroupInfo user <$> withFastStore (\db -> getGroupInfo db vr user gId)
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
        (g', m', scopeInfo) <- mkGroupChatScope g m
        createInternalChatItem user (CDGroupSnd g' scopeInfo) (CISndConnEvent . SCERatchetSync rss . Just $ groupMemberRef m') Nothing
        pure $ CRGroupMemberRatchetSyncStarted user g' m' cStats
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
    when (membershipRole >= GRModerator) $ throwChatError $ CECantBlockMemberForSelf gInfo m showMessages
    let settings = (memberSettings m) {showMessages}
    processChatCommand vr nm $ APISetMemberSettings gId mId settings
  ContactInfo cName -> withContactName cName APIContactInfo
  ShowGroupInfo gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIGroupInfo groupId
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
  APIAddContact userId incognito -> withUserId userId $ \user -> do
    -- [incognito] generate profile for connection
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    subMode <- chatReadVar subscriptionMode
    let userData = contactShortLinkData (userProfileDirect user incognitoProfile Nothing True) Nothing
        userLinkData = UserInvLinkData userData
    -- TODO [certs rcv]
    (connId, (ccLink, _serviceId)) <- withAgent $ \a -> createConnection a nm (aUserId user) True False SCMInvitation (Just userLinkData) Nothing IKPQOn subMode
    ccLink' <- shortenCreatedLink ccLink
    -- TODO PQ pass minVersion from the current range
    conn <- withFastStore' $ \db -> createDirectConnection db user connId ccLink' Nothing ConnNew incognitoProfile subMode initialChatVersion PQSupportOn
    pure $ CRInvitation user ccLink' conn
  AddContact incognito -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APIAddContact userId incognito
  APISetConnectionIncognito connId incognito -> withUser $ \user@User {userId} -> do
    conn <- withFastStore $ \db -> getPendingContactConnection db userId connId
    let PendingContactConnection {pccConnStatus, customUserProfileId} = conn
    case (pccConnStatus, customUserProfileId, incognito) of
      (ConnNew, Nothing, True) -> do
        incognitoProfile <- liftIO generateRandomProfile
        sLnk <- updatePCCShortLinkData conn $ userProfileDirect user (Just incognitoProfile) Nothing True
        conn' <- withFastStore' $ \db -> do
          pId <- createIncognitoProfile db user incognitoProfile
          updatePCCIncognito db user conn (Just pId) sLnk
        pure $ CRConnectionIncognitoUpdated user conn' (Just incognitoProfile)
      (ConnNew, Just pId, False) -> do
        sLnk <- updatePCCShortLinkData conn $ userProfileDirect user Nothing Nothing True
        conn' <- withFastStore' $ \db -> do
          deletePCCIncognitoProfile db user pId
          updatePCCIncognito db user conn Nothing sLnk
        pure $ CRConnectionIncognitoUpdated user conn' Nothing
      _ -> throwChatError CEConnectionIncognitoChangeProhibited
  APIChangeConnectionUser connId newUserId -> withUser $ \user@User {userId} -> do
    conn <- withFastStore $ \db -> getPendingContactConnection db userId connId
    let PendingContactConnection {pccConnStatus, connLinkInv} = conn
    case (pccConnStatus, connLinkInv) of
      (ConnNew, Just _ccLink) -> do
        newUser <- privateGetUser newUserId
        conn' <- recreateConn user conn newUser
        pure $ CRConnectionUserChanged user conn conn' newUser
      _ -> throwChatError CEConnectionUserChangeProhibited
    where
      recreateConn user conn@PendingContactConnection {customUserProfileId, connLinkInv} newUser = do
        subMode <- chatReadVar subscriptionMode
        let short = isJust $ connShortLink =<< connLinkInv
            userLinkData_
              | short = Just $ UserInvLinkData $ contactShortLinkData (userProfileDirect newUser Nothing Nothing True) Nothing
              | otherwise = Nothing
        -- TODO [certs rcv]
        (agConnId, (ccLink, _serviceId)) <- withAgent $ \a -> createConnection a nm (aUserId newUser) True False SCMInvitation userLinkData_ Nothing IKPQOn subMode
        ccLink' <- shortenCreatedLink ccLink
        conn' <- withFastStore' $ \db -> do
          deleteConnectionRecord db user connId
          forM_ customUserProfileId $ \profileId ->
            deletePCCIncognitoProfile db user profileId
          createDirectConnection db newUser agConnId ccLink' Nothing ConnNew Nothing subMode initialChatVersion PQSupportOn
        deleteAgentConnectionAsync (aConnId' conn)
        pure conn'
  APIConnectPlan userId (Just cLink) -> withUserId userId $ \user ->
    uncurry (CRConnectionPlan user) <$> connectPlan user cLink
  APIConnectPlan _ Nothing -> throwChatError CEInvalidConnReq
  APIPrepareContact userId accLink contactSLinkData -> withUserId userId $ \user -> do
    let ContactShortLinkData {profile, message, business} = contactSLinkData
    welcomeSharedMsgId <- forM message $ \_ -> getSharedMsgId
    case accLink of
      ACCL SCMContact ccLink
        | business -> do
            let Profile {preferences} = profile
                groupPreferences = maybe defaultBusinessGroupPrefs businessGroupPrefs preferences
                groupProfile = businessGroupProfile profile groupPreferences
            gVar <- asks random
            (gInfo, hostMember) <- withStore $ \db -> createPreparedGroup db gVar vr user groupProfile True ccLink welcomeSharedMsgId False
            void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing (Just epochStart)
            let cd = CDGroupRcv gInfo Nothing hostMember
                createItem sharedMsgId content = createChatItem user cd True content sharedMsgId Nothing
                cInfo = GroupChat gInfo Nothing
            void $ createGroupFeatureItems_ user cd True CIRcvGroupFeature gInfo
            aci <- mapM (createItem welcomeSharedMsgId . CIRcvMsgContent) message
            let chat = case aci of
                  Just (AChatItem SCTGroup dir _ ci) -> Chat cInfo [CChatItem dir ci] emptyChatStats {unreadCount = 1, minUnreadItemId = chatItemId' ci}
                  _ -> Chat cInfo [] emptyChatStats
            pure $ CRNewPreparedChat user $ AChat SCTGroup chat
      ACCL _ (CCLink cReq _) -> do
        ct <- withStore $ \db -> createPreparedContact db vr user profile accLink welcomeSharedMsgId
        void $ createChatItem user (CDDirectSnd ct) False CIChatBanner Nothing (Just epochStart)
        let cd = CDDirectRcv ct
            createItem sharedMsgId content = createChatItem user cd False content sharedMsgId Nothing
            cInfo = DirectChat ct
        void $ createItem Nothing $ CIRcvDirectE2EEInfo $ E2EInfo $ connRequestPQEncryption cReq
        void $ createFeatureEnabledItems_ user ct
        aci <- mapM (createItem welcomeSharedMsgId . CIRcvMsgContent) message
        let chat = case aci of
              Just (AChatItem SCTDirect dir _ ci) -> Chat cInfo [CChatItem dir ci] emptyChatStats {unreadCount = 1, minUnreadItemId = chatItemId' ci}
              _ -> Chat cInfo [] emptyChatStats
        pure $ CRNewPreparedChat user $ AChat SCTDirect chat
  APIPrepareGroup userId ccLink direct groupSLinkData -> withUserId userId $ \user -> do
    let GroupShortLinkData {groupProfile = gp@GroupProfile {description}} = groupSLinkData
    welcomeSharedMsgId <- forM description $ \_ -> getSharedMsgId
    let useRelays = not direct
    gVar <- asks random
    (gInfo, hostMember) <- withStore $ \db -> createPreparedGroup db gVar vr user gp False ccLink welcomeSharedMsgId useRelays
    void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing (Just epochStart)
    -- TODO [relays] member: TBC save items as message from channel
    -- TODO   - hostMember to later be associated with owner profile when relays send it
    -- TODO   - pick any owner at random from initial introductions, find unknown member in group?
    -- TODO   - alternatively support not having a member in CDGroupRcv direction?
    let cd = CDGroupRcv gInfo Nothing hostMember
        cInfo = GroupChat gInfo Nothing
    void $ createGroupFeatureItems_ user cd True CIRcvGroupFeature gInfo
    aci <- forM description $ \descr -> createChatItem user cd True (CIRcvMsgContent $ MCText descr) welcomeSharedMsgId Nothing
    let chat = case aci of
          Just (AChatItem SCTGroup dir _ ci) -> Chat cInfo [CChatItem dir ci] emptyChatStats {unreadCount = 1, minUnreadItemId = chatItemId' ci}
          _ -> Chat cInfo [] emptyChatStats
    pure $ CRNewPreparedChat user $ AChat SCTGroup chat
  APIChangePreparedContactUser contactId newUserId -> withUser $ \user -> do
    ct@Contact {preparedContact} <- withFastStore $ \db -> getContact db vr user contactId
    when (isNothing preparedContact) $ throwCmdError "contact doesn't have link to connect"
    when (isJust $ contactConn ct) $ throwCmdError "contact already has connection"
    newUser <- privateGetUser newUserId
    ct' <- withFastStore $ \db -> updatePreparedContactUser db vr user ct newUser
    -- create changed feature items (new user may have different preferences)
    lift $ createContactChangedFeatureItems user ct ct'
    pure $ CRContactUserChanged user ct newUser ct'
  APIChangePreparedGroupUser groupId newUserId -> withUser $ \user -> do
    (gInfo, hostMember) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user groupId <*> getHostMember db vr user groupId
    when (isNothing $ preparedGroup gInfo) $ throwCmdError "group doesn't have link to connect"
    when (isJust $ memberConn hostMember) $ throwCmdError "host member already has connection"
    newUser <- privateGetUser newUserId
    gInfo' <- withFastStore $ \db -> updatePreparedGroupUser db vr user gInfo hostMember newUser
    pure $ CRGroupUserChanged user gInfo newUser gInfo'
  APIConnectPreparedContact contactId incognito msgContent_ -> withUser $ \user -> do
    ct@Contact {preparedContact} <- withFastStore $ \db -> getContact db vr user contactId
    case preparedContact of
      Nothing -> throwCmdError "contact doesn't have link to connect"
      Just PreparedContact {connLinkToConnect = ACCL SCMInvitation ccLink} -> do
        (_, customUserProfile) <- connectViaInvitation user incognito ccLink (Just contactId) `catchAllErrors` \e -> do
          -- get updated contact, in case connection was started - in UI it would lock ability to change
          -- user or incognito profile for contact, in case server received request while client got network error
          ct' <- withFastStore $ \db -> getContact db vr user contactId
          toView $ CEvtChatInfoUpdated user (AChatInfo SCTDirect $ DirectChat ct')
          throwError e
        -- get updated contact with connection
        ct' <- withFastStore $ \db -> getContact db vr user contactId
        -- create changed feature items (connecting incognito sends default preferences, instead of user preferences)
        lift . when incognito $ createContactChangedFeatureItems user ct ct'
        forM_ msgContent_ $ \mc -> do
          let evt = XMsgNew $ MCSimple (extMsgContent mc Nothing)
          (msg, _) <- sendDirectContactMessage user ct' evt
          ci <- saveSndChatItem user (CDDirectSnd ct') msg (CISndMsgContent mc)
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct') ci]
        pure $ CRStartedConnectionToContact user ct' customUserProfile
      Just PreparedContact {connLinkToConnect = ACCL SCMContact ccLink, welcomeSharedMsgId, requestSharedMsgId} -> do
        msg_ <- forM msgContent_ $ \mc -> case requestSharedMsgId of
          Just smId -> pure (smId, mc)
          Nothing -> do
            smId <- getSharedMsgId
            withFastStore' $ \db -> setRequestSharedMsgIdForContact db contactId smId
            pure (smId, mc)
        r <- connectViaContact user (Just $ PCEContact ct) incognito ccLink welcomeSharedMsgId msg_ `catchAllErrors` \e -> do
          -- get updated contact, in case connection was started - in UI it would lock ability to change
          -- user or incognito profile for contact, in case server received request while client got network error
          ct' <- withFastStore $ \db -> getContact db vr user contactId
          toView $ CEvtChatInfoUpdated user (AChatInfo SCTDirect $ DirectChat ct')
          throwError e
        case r of
          CVRSentInvitation _conn customUserProfile -> do
            -- get updated contact with connection
            ct' <- withFastStore $ \db -> getContact db vr user contactId
            -- create changed feature items (connecting incognito sends default preferences, instead of user preferences)
            lift . when incognito $ createContactChangedFeatureItems user ct ct'
            forM_ msg_ $ \(sharedMsgId, mc) -> do
              ci <- createChatItem user (CDDirectSnd ct') False (CISndMsgContent mc) (Just sharedMsgId) Nothing
              toView $ CEvtNewChatItems user [ci]
            pure $ CRStartedConnectionToContact user ct' customUserProfile
          CVRConnectedContact ct' -> pure $ CRContactAlreadyExists user ct'
  APIConnectPreparedGroup groupId incognito msgContent_ -> withUser $ \user -> do
    (gInfo, hostMember) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user groupId <*> getHostMember db vr user groupId
    case gInfo of
      GroupInfo {preparedGroup = Nothing} -> throwCmdError "group doesn't have link to connect"
      GroupInfo {useRelays = BoolDef True, preparedGroup = Just PreparedGroup {connLinkToConnect}} -> do
        sLnk <- case toShortLinkContact connLinkToConnect of
          Just sl -> pure sl
          Nothing -> throwChatError $ CEException "failed to retrieve relays: no short link"
        (mainCReq@(CRContactUri crData), ContactLinkData _ UserContactData {relays}) <- getShortLinkConnReq nm user sLnk
        -- Set group link info and incognito profile once before connecting to relays
        incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
        let cReqHash = ConnReqUriHash . C.sha256Hash . strEncode $ CRContactUri crData {crScheme = SSSimplex}
        gInfo' <- withFastStore $ \db -> setPreparedGroupLinkInfo db vr user gInfo mainCReq cReqHash incognitoProfile
        results <- mapConcurrently (connectToRelay gInfo') relays
        let relayFailed = \case (_, _, Left _) -> True; _ -> False
            (failed, succeeded) = partition relayFailed results
        if null succeeded
          then do
            -- updated group info (connLinkPreparedConnection) - in UI it would lock ability to change
            -- user or incognito profile for group, in case server received request while client got network error
            toView $ CEvtChatInfoUpdated user (AChatInfo SCTGroup $ GroupChat gInfo' Nothing)
            -- TODO [relays] member: prefer throwing temporary network connection error to enable retry
            case failed of
              (_, _, Left e) : _ -> throwError e
              _ -> throwChatError $ CEException "no relay connection results" -- shouldn't happen
          else do
            withFastStore' $ \db -> setPreparedGroupStartedConnection db groupId
            -- Async retry failed relays with temporary errors
            let retryable = [(l, m) | (l, m, Left ChatErrorAgent {agentError = e}) <- failed, temporaryOrHostError e]
            void $ mapConcurrently (uncurry $ retryRelayConnectionAsync gInfo') retryable
            -- TODO [relays] member: TBC response type for UI to display state of relays connection
            -- TODO   - differentiate success, temporary failure, permanent failure
            -- TODO   - possibly, additional status on relay member record
            pure $ CRStartedConnectionToGroup user gInfo' incognitoProfile
        where
          connectToRelay gInfo'@GroupInfo {groupId} relayLink = do
            gVar <- asks random
            -- Save relayLink to re-use relay member record on retry (check by relayLink)
            relayMember <- withFastStore $ \db -> getCreateRelayForMember db vr gVar user gInfo' relayLink
            r <- tryAllErrors $ do
              (cReq, _cData) <- getShortLinkConnReq nm user relayLink
              let relayLinkToConnect = CCLink cReq (Just relayLink)
              void $ connectViaContact user (Just $ PCEGroup gInfo' relayMember) incognito relayLinkToConnect Nothing Nothing
            -- Re-read member to get updated activeConn
            relayMember' <- withFastStore $ \db -> getGroupMember db vr user groupId (groupMemberId' relayMember)
            pure (relayLink, relayMember', r)
          retryRelayConnectionAsync gInfo' relayLink relayMember@GroupMember {activeConn} = do
            forM_ activeConn $ \conn -> do
              deleteAgentConnectionAsync $ aConnId conn
              withStore' $ \db -> deleteConnectionRecord db user (dbConnId conn)
            subMode <- chatReadVar subscriptionMode
            newConnIds <- getAgentConnShortLinkAsync user relayLink
            withStore' $ \db -> createRelayMemberConnectionAsync db user gInfo' relayMember relayLink newConnIds subMode
      GroupInfo {preparedGroup = Just PreparedGroup {connLinkToConnect, welcomeSharedMsgId, requestSharedMsgId}} -> do
        msg_ <- forM msgContent_ $ \mc -> case requestSharedMsgId of
          Just smId -> pure (smId, mc)
          Nothing -> do
            smId <- getSharedMsgId
            withFastStore' $ \db -> setRequestSharedMsgIdForGroup db groupId smId
            pure (smId, mc)
        r <- connectViaContact user (Just $ PCEGroup gInfo hostMember) incognito connLinkToConnect welcomeSharedMsgId msg_ `catchAllErrors` \e -> do
          -- get updated group info, in case connection was started (connLinkPreparedConnection) - in UI it would lock ability to change
          -- user or incognito profile for group or business chat, in case server received request while client got network error
          gInfo' <- withFastStore $ \db -> getGroupInfo db vr user groupId
          toView $ CEvtChatInfoUpdated user (AChatInfo SCTGroup $ GroupChat gInfo' Nothing)
          throwError e
        case r of
          CVRSentInvitation _conn customUserProfile -> do
            -- get updated group info (connLinkStartedConnection and incognito membership)
            gInfo' <- withFastStore $ \db -> do
              liftIO $ setPreparedGroupStartedConnection db groupId
              getGroupInfo db vr user groupId
            forM_ msg_ $ \(sharedMsgId, mc) -> do
              ci <- createChatItem user (CDGroupSnd gInfo' Nothing) False (CISndMsgContent mc) (Just sharedMsgId) Nothing
              toView $ CEvtNewChatItems user [ci]
            pure $ CRStartedConnectionToGroup user gInfo' customUserProfile
          CVRConnectedContact _ct -> throwChatError $ CEException "contact already exists when connecting to group"
  APIConnect userId incognito (Just acl) -> withUserId userId $ \user -> case acl of
    ACCL SCMInvitation ccLink -> do
      (conn, incognitoProfile) <- connectViaInvitation user incognito ccLink Nothing
      let pcc = mkPendingContactConnection conn $ Just ccLink
      pure $ CRSentConfirmation user pcc incognitoProfile
    ACCL SCMContact ccLink ->
      connectViaContact user Nothing incognito ccLink Nothing Nothing >>= \case
        CVRConnectedContact ct -> pure $ CRContactAlreadyExists user ct
        CVRSentInvitation conn incognitoProfile -> pure $ CRSentInvitation user (mkPendingContactConnection conn Nothing) incognitoProfile
  APIConnect _ _ Nothing -> throwChatError CEInvalidConnReq
  Connect incognito (Just cLink@(ACL m cLink')) -> withUser $ \user -> do
    -- TODO [relays] member: /c api to support groups with relays
    -- TODO   - possibly by going through APIPrepareGroup -> APIConnectPreparedGroup
    (ccLink, plan) <- connectPlan user cLink `catchAllErrors` \e -> case cLink' of CLFull cReq -> pure (ACCL m (CCLink cReq Nothing), CPInvitationLink (ILPOk Nothing)); _ -> throwError e
    connectWithPlan user incognito ccLink plan
  Connect _ Nothing -> throwChatError CEInvalidConnReq
  APIConnectContactViaAddress userId incognito contactId -> withUserId userId $ \user -> do
    ct@Contact {profile = LocalProfile {contactLink}} <- withFastStore $ \db -> getContact db vr user contactId
    ccLink <- case contactLink of
      Just (CLFull cReq) -> pure $ CCLink cReq Nothing
      Just (CLShort sLnk) -> do
        (cReq, _cData) <- getShortLinkConnReq nm user sLnk
        pure $ CCLink cReq $ Just sLnk
      Nothing -> throwCmdError "no address in contact profile"
    connectContactViaAddress user incognito ct ccLink `catchAllErrors` \e -> do
      -- get updated contact, in case connection was started - in UI it would lock ability to change incognito choice
      -- on next connection attempt, in case server received request while client got network error
      ct' <- withFastStore $ \db -> getContact db vr user contactId
      toView $ CEvtChatInfoUpdated user (AChatInfo SCTDirect $ DirectChat ct')
      throwError e
  ConnectSimplex incognito -> withUser $ \user -> do
    plan <- contactRequestPlan user adminContactReq Nothing `catchAllErrors` const (pure $ CPContactAddress (CAPOk Nothing))
    connectWithPlan user incognito (ACCL SCMContact (CCLink adminContactReq Nothing)) plan
  DeleteContact cName cdm -> withContactName cName $ \ctId -> APIDeleteChat (ChatRef CTDirect ctId Nothing) cdm
  ClearContact cName -> withContactName cName $ \chatId -> APIClearChat $ ChatRef CTDirect chatId Nothing
  APIListContacts userId -> withUserId userId $ \user ->
    CRContactsList user <$> withFastStore' (\db -> getUserContacts db vr user)
  ListContacts -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APIListContacts userId
  APICreateMyAddress userId -> withUserId userId $ \user@User {userChatRelay} -> do
    withFastStore' (\db -> runExceptT $ getUserAddress db user) >>= \case
      Left SEUserContactLinkNotFound -> pure ()
      Left e -> throwError $ ChatErrorStore e
      Right _ -> throwError $ ChatErrorStore SEDuplicateContactLink
    subMode <- chatReadVar subscriptionMode
    -- TODO [relays] relay: address creation
    -- TODO   - add relay key, identity to link data
    -- TODO   - validate short link is created (returned by agent)
    let userData = contactShortLinkData (userProfileDirect user Nothing Nothing True) Nothing
        userLinkData = UserContactLinkData UserContactData {direct = True, owners = [], relays = [], userData}
    -- TODO [certs rcv]
    (connId, (ccLink, _serviceId)) <- withAgent $ \a -> createConnection a nm (aUserId user) True True SCMContact (Just userLinkData) Nothing IKPQOn subMode
    ccLink' <- shortenCreatedLink ccLink
    let ccLink'' = if isTrue userChatRelay then createdRelayLink ccLink' else ccLink'
    withFastStore $ \db -> createUserContactLink db user connId ccLink'' subMode
    pure $ CRUserContactLinkCreated user ccLink''
  CreateMyAddress -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APICreateMyAddress userId
  APIDeleteMyAddress userId -> withUserId userId $ \user@User {profile = p} -> do
    conn <- withFastStore $ \db -> getUserAddressConnection db vr user
    withChatLock "deleteMyAddress" $ do
      deleteAgentConnectionAsync $ aConnId conn
      withFastStore' (`deleteUserAddress` user)
    let p' = (fromLocalProfile p :: Profile) {contactLink = Nothing}
    r <- updateProfile_ user p' False $ withFastStore' $ \db -> setUserProfileContactLink db user Nothing
    let user' = case r of
          CRUserProfileUpdated u' _ _ _ -> u'
          _ -> user
    pure $ CRUserContactLinkDeleted user'
  DeleteMyAddress -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APIDeleteMyAddress userId
  APIShowMyAddress userId -> withUserId' userId $ \user ->
    CRUserContactLink user <$> withFastStore (`getUserAddress` user)
  ShowMyAddress -> withUser' $ \User {userId} ->
    processChatCommand vr nm $ APIShowMyAddress userId
  APIAddMyAddressShortLink userId -> withUserId' userId $ \user ->
    CRUserContactLink user <$> (withFastStore (`getUserAddress` user) >>= setMyAddressData user)
  APISetProfileAddress userId False -> withUserId userId $ \user@User {profile = p} -> do
    let p' = (fromLocalProfile p :: Profile) {contactLink = Nothing}
    updateProfile_ user p' True $ withFastStore' $ \db -> setUserProfileContactLink db user Nothing
  APISetProfileAddress userId True -> withUserId userId $ \user@User {profile = p} -> do
    ucl <- withFastStore (`getUserAddress` user)
    -- TODO [short links] replace with short links
    let p' = (fromLocalProfile p :: Profile) {contactLink = Just $ profileContactLink ucl}
    updateProfile_ user p' True $ withFastStore' $ \db -> setUserProfileContactLink db user $ Just ucl
  SetProfileAddress onOff -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APISetProfileAddress userId onOff
  APISetAddressSettings userId settings@AddressSettings {businessAddress, autoAccept} -> withUserId userId $ \user -> do
    ucl@UserContactLink {userContactLinkId, shortLinkDataSet, addressSettings} <- withFastStore (`getUserAddress` user)
    forM_ autoAccept $ \AutoAccept {acceptIncognito} -> do
      when (shortLinkDataSet && acceptIncognito) $ throwCmdError "incognito not allowed for address with short link data"
      when (businessAddress && acceptIncognito) $ throwCmdError "requests to business address cannot be accepted incognito"
    if addressSettings == settings
      then pure $ CRUserContactLinkUpdated user ucl
      else do
        let ucl' = ucl {addressSettings = settings}
        ucl'' <- if shortLinkDataSet then setMyAddressData user ucl' else pure ucl'
        withFastStore' $ \db -> updateUserAddressSettings db userContactLinkId settings
        pure $ CRUserContactLinkUpdated user ucl''
  SetAddressSettings settings -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APISetAddressSettings userId settings
  AcceptContact incognito cName -> withUser $ \User {userId} -> do
    connReqId <- withFastStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand vr nm $ APIAcceptContact incognito connReqId
  RejectContact cName -> withUser $ \User {userId} -> do
    connReqId <- withFastStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand vr nm $ APIRejectContact connReqId
  ForwardMessage toChatName fromContactName forwardedMsg -> withUser $ \user -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user fromContactName
    forwardedItemId <- withFastStore $ \db -> getDirectChatItemIdByText' db user contactId forwardedMsg
    toChatRef <- getChatRef user toChatName
    processChatCommand vr nm $ APIForwardChatItems toChatRef (ChatRef CTDirect contactId Nothing) (forwardedItemId :| []) Nothing
  ForwardGroupMessage toChatName fromGroupName fromMemberName_ forwardedMsg -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user fromGroupName
    forwardedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user groupId fromMemberName_ forwardedMsg
    toChatRef <- getChatRef user toChatName
    processChatCommand vr nm $ APIForwardChatItems toChatRef (ChatRef CTGroup groupId Nothing) (forwardedItemId :| []) Nothing
  ForwardLocalMessage toChatName forwardedMsg -> withUser $ \user -> do
    folderId <- withFastStore (`getUserNoteFolderId` user)
    forwardedItemId <- withFastStore $ \db -> getLocalChatItemIdByText' db user folderId forwardedMsg
    toChatRef <- getChatRef user toChatName
    processChatCommand vr nm $ APIForwardChatItems toChatRef (ChatRef CTLocal folderId Nothing) (forwardedItemId :| []) Nothing
  SendMessage sendName msg -> withUser $ \user -> do
    let mc = MCText msg
    case sendName of
      SNDirect name ->
        withFastStore' (\db -> runExceptT $ getContactIdByName db user name) >>= \case
          Right ctId -> do
            let sendRef = SRDirect ctId
            processChatCommand vr nm $ APISendMessages sendRef False Nothing [composedMessage Nothing mc]
          Left _ ->
            withFastStore' (\db -> runExceptT $ getActiveMembersByName db vr user name) >>= \case
              Right [(gInfo, member)] -> do
                let GroupInfo {localDisplayName = gName} = gInfo
                    GroupMember {localDisplayName = mName} = member
                processChatCommand vr nm $ SendMemberContactMessage gName mName msg
              Right (suspectedMember : _) ->
                throwChatError $ CEContactNotFound name (Just suspectedMember)
              _ ->
                throwChatError $ CEContactNotFound name Nothing
      SNGroup name scope_ -> do
        (gId, cScope_, mentions) <- withFastStore $ \db -> do
          gId <- getGroupIdByName db user name
          cScope_ <-
            forM scope_ $ \(GSNMemberSupport mName_) ->
              GCSMemberSupport <$> mapM (getGroupMemberIdByName db user gId) mName_
          (gId,cScope_,) <$> liftIO (getMessageMentions db user gId msg)
        let sendRef = SRGroup gId cScope_
        processChatCommand vr nm $ APISendMessages sendRef False Nothing [ComposedMessage Nothing Nothing mc mentions]
      SNLocal -> do
        folderId <- withFastStore (`getUserNoteFolderId` user)
        processChatCommand vr nm $ APICreateChatItems folderId [composedMessage Nothing mc]
  SendMemberContactMessage gName mName msg -> withUser $ \user -> do
    (gId, mId) <- getGroupAndMemberId user gName mName
    m <- withFastStore $ \db -> getGroupMember db vr user gId mId
    let mc = MCText msg
    case memberContactId m of
      Nothing -> do
        g <- withFastStore $ \db -> getGroupInfo db vr user gId
        unless (groupFeatureUserAllowed SGFDirectMessages g) $ throwCmdError "direct messages not allowed"
        toView $ CEvtNoMemberContactCreating user g m
        processChatCommand vr nm (APICreateMemberContact gId mId) >>= \case
          CRNewMemberContact _ ct@Contact {contactId} _ _ -> do
            toViewTE $ TENewMemberContact user ct g m
            processChatCommand vr nm $ APISendMemberContactInvitation contactId (Just mc)
          cr -> pure cr
      Just ctId -> do
        let sendRef = SRDirect ctId
        processChatCommand vr nm $ APISendMessages sendRef False Nothing [composedMessage Nothing mc]
  AcceptMemberContact cName -> withUser $ \user -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user cName
    processChatCommand vr nm $ APIAcceptMemberContact contactId
  SendLiveMessage chatName msg -> withUser $ \user -> do
    (chatRef, mentions) <- getChatRefAndMentions user chatName msg
    withSendRef chatRef $ \sendRef -> do
      let mc = MCText msg
      processChatCommand vr nm $ APISendMessages sendRef True Nothing [ComposedMessage Nothing Nothing mc mentions]
  SendMessageBroadcast mc -> withUser $ \user -> do
    contacts <- withFastStore' $ \db -> getUserContacts db vr user
    withChatLock "sendMessageBroadcast" $ do
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
        void $ createNewSndChatItem db user (CDDirectSnd ct) sndMsg (CISndMsgContent mc) Nothing Nothing Nothing False createdAt
  SendMessageQuote cName (AMsgDirection msgDir) quotedMsg msg -> withUser $ \user@User {userId} -> do
    contactId <- withFastStore $ \db -> getContactIdByName db user cName
    quotedItemId <- withFastStore $ \db -> getDirectChatItemIdByText db userId contactId msgDir quotedMsg
    let mc = MCText msg
    processChatCommand vr nm $ APISendMessages (SRDirect contactId) False Nothing [ComposedMessage Nothing (Just quotedItemId) mc M.empty]
  DeleteMessage chatName deletedMsg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    deletedItemId <- getSentChatItemIdByText user chatRef deletedMsg
    processChatCommand vr nm $ APIDeleteChatItem chatRef (deletedItemId :| []) CIDMBroadcast
  DeleteMemberMessage gName mName deletedMsg -> withUser $ \user -> do
    gId <- withFastStore $ \db -> getGroupIdByName db user gName
    deletedItemId <- withFastStore $ \db -> getGroupChatItemIdByText db user gId (Just mName) deletedMsg
    processChatCommand vr nm $ APIDeleteMemberChatItem gId (deletedItemId :| [])
  EditMessage chatName editedMsg msg -> withUser $ \user -> do
    (chatRef, mentions) <- getChatRefAndMentions user chatName msg
    editedItemId <- getSentChatItemIdByText user chatRef editedMsg
    let mc = MCText msg
    processChatCommand vr nm $ APIUpdateChatItem chatRef editedItemId False $ UpdatedMessage mc mentions
  UpdateLiveMessage chatName chatItemId live msg -> withUser $ \user -> do
    (chatRef, mentions) <- getChatRefAndMentions user chatName msg
    let mc = MCText msg
    processChatCommand vr nm $ APIUpdateChatItem chatRef chatItemId live $ UpdatedMessage mc mentions
  ReactToMessage add reaction chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatItemId <- getChatItemIdByText user chatRef msg
    processChatCommand vr nm $ APIChatItemReaction chatRef chatItemId add reaction
  APINewGroup userId incognito gProfile -> withUserId userId $ \user -> do
    gInfo <- newGroup user incognito gProfile False
    pure $ CRGroupCreated user gInfo
  NewGroup incognito gProfile -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APINewGroup userId incognito gProfile
  APINewPublicGroup userId incognito relayIds gProfile -> withUserId userId $ \user -> do
    gInfo <- newGroup user incognito gProfile True
    (gInfo', gLink, groupRelays) <- setupLink user gInfo `catchAllErrors` \e -> do
      deleteInProgressGroup user gInfo
      throwError e
    pure $ CRPublicGroupCreated user gInfo' gLink groupRelays
    where
      setupLink :: User -> GroupInfo -> CM (GroupInfo, GroupLink, [GroupRelay])
      setupLink user gInfo = do
        (gInfo', gLink, sLnk) <- newGroupLink user gInfo
        relays <- withFastStore $ \db -> mapM (getChatRelayById db user) (L.toList relayIds)
        groupRelays <- addRelays user gInfo' sLnk relays
        pure (gInfo', gLink, groupRelays)
      newGroupLink :: User -> GroupInfo -> CM (GroupInfo, GroupLink, ShortLinkContact)
      newGroupLink user gInfo@GroupInfo {groupProfile} = do
        groupLinkId <- GroupLinkId <$> drgRandomBytes 16
        subMode <- chatReadVar subscriptionMode
        -- TODO [relays] owner: prepare group link without initially creating on server
        -- TODO   - add link and owner key to group profile, sign profile
        -- TODO   - create group link on server with signed profile as data
        -- / link creation
        let userData = encodeShortLinkData $ GroupShortLinkData groupProfile
            userLinkData = UserContactLinkData UserContactData {direct = False, owners = [], relays = [], userData}
            crClientData = encodeJSON $ CRDataGroup groupLinkId
        (connId, (ccLink, _serviceId)) <- withAgent $ \a -> createConnection a nm (aUserId user) True True SCMContact (Just userLinkData) (Just crClientData) IKPQOff subMode
        ccLink' <- createdGroupLink <$> shortenCreatedLink ccLink
        sLnk <- case toShortLinkContact ccLink' of
          Just sl -> pure sl
          Nothing -> throwChatError $ CEException "failed to create relayed group link: no short link"
        let groupProfile' = (groupProfile :: GroupProfile) {groupLink = Just sLnk}
            userData' = encodeShortLinkData $ GroupShortLinkData groupProfile'
            userLinkData' = UserContactLinkData UserContactData {direct = False, owners = [], relays = [], userData = userData'}
        void $ withAgent (\a -> setConnShortLink a nm connId SCMContact userLinkData' (Just crClientData))
        -- link creation /
        gVar <- asks random
        (gInfo', gLink) <- withFastStore $ \db -> do
          gLink <- createGroupLink db gVar user gInfo connId ccLink' groupLinkId GRMember subMode
          gInfo' <- updateGroupProfile db user gInfo groupProfile'
          pure (gInfo', gLink)
        pure (gInfo', gLink, sLnk)
  NewPublicGroup incognito relayIds gProfile -> withUser $ \User {userId} ->
    processChatCommand vr nm $ APINewPublicGroup userId incognito relayIds gProfile
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
        -- TODO [certs rcv]
        (agentConnId, (CCLink cReq _, _serviceId)) <- withAgent $ \a -> createConnection a nm (aUserId user) True False SCMInvitation Nothing Nothing IKPQOff subMode
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
    withGroupLock "joinGroup" groupId $ do
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
          void (withAgent $ \a -> joinConnection a nm (aUserId user) agentConnId (enableNtfs /= MFNone) connRequest dm PQSupportOff subMode)
            `catchAllErrors` \e -> do
              withFastStore' $ \db -> do
                updateGroupMemberStatus db userId fromMember GSMemInvited
                updateGroupMemberStatus db userId membership GSMemInvited
              throwError e
          updateCIGroupInvitationStatus user g CIGISAccepted `catchAllErrors` eToView
          pure $ CRUserAcceptedGroupSent user g {membership = membership {memberStatus = GSMemAccepted}} Nothing
        Nothing -> throwChatError $ CEContactNotActive ct
  APIAcceptMember groupId gmId role -> withUser $ \user@User {userId} -> do
    (gInfo, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user groupId <*> getGroupMemberById db vr user gmId
    -- TODO check that user's role is > role, possibly restrict role to only observer and member
    assertUserGroupRole gInfo GRModerator
    case memberStatus m of
      GSMemPendingApproval | memberCategory m == GCInviteeMember -> do -- only host can approve
        let GroupInfo {groupProfile = GroupProfile {memberAdmission}} = gInfo
        case memberConn m of
          Just mConn ->
            case memberAdmission >>= review of
              Just MCAll -> do
                introduceToModerators vr user gInfo m
                withFastStore' $ \db -> updateGroupMemberStatus db userId m GSMemPendingReview
                let m' = m {memberStatus = GSMemPendingReview}
                pure $ CRMemberAccepted user gInfo m'
              Nothing -> do
                let msg = XGrpLinkAcpt GAAccepted role (memberId' m)
                void $ sendDirectMemberMessage mConn msg groupId
                introduceToRemaining vr user gInfo m {memberRole = role}
                when (groupFeatureAllowed SGFHistory gInfo) $ sendHistory user gInfo m
                (m', gInfo') <- withFastStore' $ \db -> do
                  m' <- updateGroupMemberAccepted db user m GSMemConnected role
                  gInfo' <- updateGroupMembersRequireAttention db user gInfo m m'
                  pure (m', gInfo')
                -- create item in both scopes
                createInternalChatItem user (CDGroupRcv gInfo' Nothing m') (CIRcvGroupEvent RGEMemberConnected) Nothing
                let scopeInfo = Just GCSIMemberSupport {groupMember_ = Just m'}
                    gEvent = SGEMemberAccepted gmId (fromLocalProfile $ memberProfile m')
                createInternalChatItem user (CDGroupSnd gInfo' scopeInfo) (CISndGroupEvent gEvent) Nothing
                pure $ CRMemberAccepted user gInfo' m'
          Nothing -> throwChatError CEGroupMemberNotActive
      GSMemPendingReview -> do
        let scope = Just $ GCSMemberSupport $ Just (groupMemberId' m)
        modMs <- withFastStore' $ \db -> getGroupModerators db vr user gInfo
        let rcpModMs' = filter memberCurrent modMs
            msg = XGrpLinkAcpt GAAccepted role (memberId' m)
        void $ sendGroupMessage user gInfo scope ([m] <> rcpModMs') msg
        when (maxVersion (memberChatVRange m) < groupKnockingVersion) $
          forM_ (memberConn m) $ \mConn -> do
            let msg2 = XMsgNew $ MCSimple $ extMsgContent (MCText acceptedToGroupMessage) Nothing
            void $ sendDirectMemberMessage mConn msg2 groupId
        when (memberCategory m == GCInviteeMember) $ do
          introduceToRemaining vr user gInfo m {memberRole = role}
          when (groupFeatureAllowed SGFHistory gInfo) $ sendHistory user gInfo m
        (m', gInfo') <- withFastStore' $ \db -> do
          m' <- updateGroupMemberAccepted db user m newMemberStatus role
          gInfo' <- updateGroupMembersRequireAttention db user gInfo m m'
          pure (m', gInfo')
        -- create item in both scopes
        createInternalChatItem user (CDGroupRcv gInfo' Nothing m') (CIRcvGroupEvent RGEMemberConnected) Nothing
        let scopeInfo = Just GCSIMemberSupport {groupMember_ = Just m'}
            gEvent = SGEMemberAccepted gmId (fromLocalProfile $ memberProfile m')
        createInternalChatItem user (CDGroupSnd gInfo' scopeInfo) (CISndGroupEvent gEvent) Nothing
        pure $ CRMemberAccepted user gInfo' m'
        where
          newMemberStatus = case memberConn m of
            Just c | connReady c -> GSMemConnected
            _ -> GSMemAnnounced
      _ -> throwCmdError "member should be pending approval and invitee, or pending review and not invitee"
  APIDeleteMemberSupportChat groupId gmId -> withUser $ \user -> do
    (gInfo, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user groupId <*> getGroupMemberById db vr user gmId
    when (isNothing $ supportChat m) $ throwCmdError "member has no support chat"
    when (memberPending m) $ throwCmdError "member is pending"
    (gInfo', m') <- withFastStore' $ \db -> do
      gInfo' <-
        if gmRequiresAttention m
          then decreaseGroupMembersRequireAttention db user gInfo
          else pure gInfo
      m' <- deleteGroupMemberSupportChat db m
      pure (gInfo', m')
    pure $ CRMemberSupportChatDeleted user gInfo' m'
  APIMembersRole groupId memberIds newRole -> withUser $ \user ->
    withGroupLock "memberRole" groupId $ do
      -- TODO [channels fwd] possible optimization is to read only required members + relays
      g@(Group gInfo members) <- withFastStore $ \db -> getGroup db vr user groupId
      when (selfSelected gInfo) $ throwCmdError "can't change role for self"
      let (invitedMems, currentMems, unchangedMems, maxRole, anyAdmin, anyPending) = selectMembers members
      when (length invitedMems + length currentMems + length unchangedMems /= length memberIds) $ throwChatError CEGroupMemberNotFound
      when (length memberIds > 1 && (anyAdmin || newRole >= GRAdmin)) $
        throwCmdError "can't change role of multiple members when admins selected, or new role is admin"
      when anyPending $ throwCmdError "can't change role of members pending approval"
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
                    anyPending' = anyPending || memberPending m
                 in
                  if
                    | memberRole == newRole -> (invited, current, m : unchanged, maxRole', anyAdmin', anyPending')
                    | memberStatus == GSMemInvited -> (m : invited, current, unchanged, maxRole', anyAdmin', anyPending')
                    | otherwise -> (invited, m : current, unchanged, maxRole', anyAdmin', anyPending')
            | otherwise = (invited, current, unchanged, maxRole, anyAdmin, anyPending)
      changeRoleInvitedMems :: User -> GroupInfo -> [GroupMember] -> CM ([ChatError], [GroupMember])
      changeRoleInvitedMems user gInfo memsToChange = do
        -- not batched, as we need to send different invitations to different connections anyway
        mems_ <- forM memsToChange $ \m -> (Right <$> changeRole m) `catchAllErrors` (pure . Left)
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
              recipients = filter memberCurrent members
          (msgs_, _gsr) <- sendGroupMessages user gInfo Nothing recipients events
          let itemsData = zipWith (fmap . sndItemData) memsToChange (L.toList msgs_)
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo Nothing) itemsData Nothing False
          when (length cis_ /= length memsToChange) $ logError "changeRoleCurrentMems: memsToChange and cis_ length mismatch"
          (errs, changed) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (updMember db) memsToChange)
          let acis = map (AChatItem SCTGroup SMDSnd (GroupChat gInfo Nothing)) $ rights cis_
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
    withGroupLock "blockForAll" groupId $ do
      -- TODO [channels fwd] possible optimization is to read only required members + relays
      Group gInfo members <- withFastStore $ \db -> getGroup db vr user groupId
      when (selfSelected gInfo) $ throwCmdError "can't block/unblock self"
      -- TODO [channels fwd] consider sending restriction to all members (remove filtering), as we do in delivery jobs
      let (blockMems, remainingMems, maxRole, anyAdmin, anyPending) = selectMembers members
      when (length blockMems /= length memberIds) $ throwChatError CEGroupMemberNotFound
      when (length memberIds > 1 && anyAdmin) $ throwCmdError "can't block/unblock multiple members when admins selected"
      when anyPending $ throwCmdError "can't block/unblock members pending approval"
      assertUserGroupRole gInfo $ max GRModerator maxRole
      blockMembers user gInfo blockMems remainingMems
    where
      selfSelected GroupInfo {membership} = elem (groupMemberId' membership) memberIds
      selectMembers :: [GroupMember] -> ([GroupMember], [GroupMember], GroupMemberRole, Bool, Bool)
      selectMembers = foldr' addMember ([], [], GRObserver, False, False)
        where
          addMember m@GroupMember {groupMemberId, memberRole} (block, remaining, maxRole, anyAdmin, anyPending)
            | groupMemberId `elem` memberIds =
                let maxRole' = max maxRole memberRole
                    anyAdmin' = anyAdmin || memberRole >= GRAdmin
                    anyPending' = anyPending || memberPending m
                 in (m : block, remaining, maxRole', anyAdmin', anyPending')
            | otherwise = (block, m : remaining, maxRole, anyAdmin, anyPending)
      blockMembers :: User -> GroupInfo -> [GroupMember] -> [GroupMember] -> CM ChatResponse
      blockMembers user gInfo blockMems remainingMems = case L.nonEmpty blockMems of
        Nothing -> throwCmdError "no members to block/unblock"
        Just blockMems' -> do
          let mrs = if blockFlag then MRSBlocked else MRSUnrestricted
              events = L.map (\GroupMember {memberId} -> XGrpMemRestrict memberId MemberRestrictions {restriction = mrs}) blockMems'
              recipients = filter memberCurrent remainingMems
          (msgs_, _gsr) <- sendGroupMessages_ user gInfo recipients events
          let itemsData = zipWith (fmap . sndItemData) blockMems (L.toList msgs_)
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo Nothing) itemsData Nothing False
          when (length cis_ /= length blockMems) $ logError "blockMembers: blockMems and cis_ length mismatch"
          let acis = map (AChatItem SCTGroup SMDSnd (GroupChat gInfo Nothing)) $ rights cis_
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
    withGroupLock "removeMembers" groupId $ do
      -- TODO [channels fwd] possible optimization is to read only required members + relays
      Group gInfo members <- withFastStore $ \db -> getGroup db vr user groupId
      let (count, invitedMems, pendingApprvMems, pendingRvwMems, currentMems, maxRole, anyAdmin) = selectMembers gmIds members
          gmIds = S.fromList $ L.toList groupMemberIds
          memCount = length groupMemberIds
      when (count /= memCount) $ throwChatError CEGroupMemberNotFound
      when (memCount > 1 && anyAdmin) $ throwCmdError "can't remove multiple members when admins selected"
      assertUserGroupRole gInfo $ max GRAdmin maxRole
      (errs1, deleted1) <- deleteInvitedMems user invitedMems
      let recipients = filter memberCurrent members
      (errs2, deleted2, acis2) <- deleteMemsSend user gInfo Nothing recipients currentMems
      (errs3, deleted3, acis3) <-
        foldM (\acc m -> deletePendingMember acc user gInfo [m] m) ([], [], []) pendingApprvMems
      let moderators = filter (\GroupMember {memberRole} -> memberRole >= GRModerator) members
      (errs4, deleted4, acis4) <-
        foldM (\acc m -> deletePendingMember acc user gInfo (m : moderators) m) ([], [], []) pendingRvwMems
      let acis = acis2 <> acis3 <> acis4
          errs = errs1 <> errs2 <> errs3 <> errs4
          deleted = deleted1 <> deleted2 <> deleted3 <> deleted4
      -- Read group info with updated membersRequireAttention
      gInfo' <- withFastStore $ \db -> getGroupInfo db vr user groupId
      let acis' = map (updateACIGroupInfo gInfo') acis
      unless (null acis') $ toView $ CEvtNewChatItems user acis'
      unless (null errs) $ toView $ CEvtChatErrors errs
      when withMessages $ deleteMessages user gInfo' deleted
      pure $ CRUserDeletedMembers user gInfo' deleted withMessages -- same order is not guaranteed
    where
      selectMembers :: S.Set GroupMemberId -> [GroupMember] -> (Int, [GroupMember], [GroupMember], [GroupMember], [GroupMember], GroupMemberRole, Bool)
      selectMembers gmIds = foldl' addMember (0, [], [], [], [], GRObserver, False)
        where
          addMember acc@(n, invited, pendingApprv, pendingRvw, current, maxRole, anyAdmin) m@GroupMember {groupMemberId, memberStatus, memberRole}
            | groupMemberId `S.member` gmIds =
                let maxRole' = max maxRole memberRole
                    anyAdmin' = anyAdmin || memberRole >= GRAdmin
                    n' = n + 1
                 in case memberStatus of
                      GSMemInvited -> (n', m : invited, pendingApprv, pendingRvw, current, maxRole', anyAdmin')
                      GSMemPendingApproval -> (n', invited, m : pendingApprv, pendingRvw, current, maxRole', anyAdmin')
                      GSMemPendingReview -> (n', invited, pendingApprv, m : pendingRvw, current, maxRole', anyAdmin')
                      _ -> (n', invited, pendingApprv, pendingRvw, m : current, maxRole', anyAdmin')
            | otherwise = acc
      deleteInvitedMems :: User -> [GroupMember] -> CM ([ChatError], [GroupMember])
      deleteInvitedMems user memsToDelete = do
        deleteMembersConnections user memsToDelete
        lift $ partitionEithers <$> withStoreBatch' (\db -> map (delMember db) memsToDelete)
        where
          delMember db m = do
            deleteGroupMember db user m
            pure m {memberStatus = GSMemRemoved}
      deletePendingMember :: ([ChatError], [GroupMember], [AChatItem]) -> User -> GroupInfo -> [GroupMember] -> GroupMember -> CM ([ChatError], [GroupMember], [AChatItem])
      deletePendingMember (accErrs, accDeleted, accACIs) user gInfo recipients m = do
        (m', scopeInfo) <- mkMemberSupportChatInfo m
        (errs, deleted, acis) <- deleteMemsSend user gInfo (Just scopeInfo) recipients [m']
        pure (errs <> accErrs, deleted <> accDeleted, acis <> accACIs)
      deleteMemsSend :: User -> GroupInfo -> Maybe GroupChatScopeInfo -> [GroupMember] -> [GroupMember] -> CM ([ChatError], [GroupMember], [AChatItem])
      deleteMemsSend user gInfo chatScopeInfo recipients memsToDelete = case L.nonEmpty memsToDelete of
        Nothing -> pure ([], [], [])
        Just memsToDelete' -> do
          let chatScope = toChatScope <$> chatScopeInfo
              events = L.map (\GroupMember {memberId} -> XGrpMemDel memberId withMessages) memsToDelete'
          (msgs_, _gsr) <- sendGroupMessages user gInfo chatScope recipients events
          let itemsData_ = zipWith (fmap . sndItemData) memsToDelete (L.toList msgs_)
              skipUnwantedItem = \case
                Right Nothing -> Nothing
                Right (Just a) -> Just $ Right a
                Left e -> Just $ Left e
              itemsData = mapMaybe skipUnwantedItem itemsData_
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo chatScopeInfo) itemsData Nothing False
          deleteMembersConnections' user memsToDelete True
          (errs, deleted) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (delMember db) memsToDelete)
          let acis = map (AChatItem SCTGroup SMDSnd (GroupChat gInfo chatScopeInfo)) $ rights cis_
          pure (errs, deleted, acis)
          where
            sndItemData :: GroupMember -> SndMessage -> Maybe (NewSndChatItemData c)
            sndItemData GroupMember {groupMemberId, memberProfile, memberStatus} msg
              | memberStatus == GSMemRemoved || memberStatus == GSMemLeft = Nothing
              | otherwise =
                  let content = CISndGroupEvent $ SGEMemberDeleted groupMemberId (fromLocalProfile memberProfile)
                      ts = ciContentTexts content
                   in Just $ NewSndChatItemData msg content ts M.empty Nothing Nothing Nothing
            delMember db m = do
              -- We're in a function used in batch member deletion, and since we're passing same gInfo for each member,
              -- voided result (updated group info) may have incorrect state of membersRequireAttention.
              -- To avoid complicating code by chaining group info updates,
              -- instead we re-read it once after deleting all members before response.
              void $ deleteOrUpdateMemberRecordIO db user gInfo m
              pure m {memberStatus = GSMemRemoved}
      deleteMessages user gInfo@GroupInfo {membership} ms
        | groupFeatureUserAllowed SGFFullDelete gInfo = deleteGroupMembersCIs user gInfo ms membership
        | otherwise = markGroupMembersCIsDeleted user gInfo ms membership
  APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
    gInfo@GroupInfo {membership} <- withFastStore $ \db -> getGroupInfo db vr user groupId
    filesInfo <- withFastStore' $ \db -> getGroupFileInfo db user gInfo
    withGroupLock "leaveGroup" groupId $ do
      cancelFilesInProgress user filesInfo
      (members, recipients) <- getRecipients user gInfo
      msg <- sendGroupMessage' user gInfo recipients XGrpLeave
      (gInfo', scopeInfo) <- mkLocalGroupChatScope gInfo
      ci <- saveSndChatItem user (CDGroupSnd gInfo' scopeInfo) msg (CISndGroupEvent SGEUserLeft)
      toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo' scopeInfo) ci]
      -- TODO delete direct connections that were unused
      deleteGroupLinkIfExists user gInfo'
      -- member records are not deleted to keep history
      deleteMembersConnections' user members True
      withFastStore' $ \db -> updateGroupMemberStatus db userId membership GSMemLeft
      pure $ CRLeftMemberUser user gInfo' {membership = membership {memberStatus = GSMemLeft}}
    where
      getRecipients user gInfo
        | useRelays' gInfo = do
            relays <- withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo
            pure (relays, relays)
        | otherwise = do
            ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo
            pure (ms, filter memberCurrentOrPending ms)
  APIListMembers groupId -> withUser $ \user ->
    CRGroupMembers user <$> withFastStore (\db -> getGroup db vr user groupId)
  -- -- validate: prohibit to delete/archive if member is pending (has to communicate approval or rejection)
  -- APIDeleteGroupConversations groupId _gcId -> withUser $ \user -> do
  --   _gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
  --   ok_ -- CRGroupConversationsArchived
  -- APIArchiveGroupConversations groupId _gcId -> withUser $ \user -> do
  --   _gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
  --   ok_ -- CRGroupConversationsDeleted
  AddMember gName cName memRole -> withUser $ \user -> do
    (groupId, contactId) <- withFastStore $ \db -> (,) <$> getGroupIdByName db user gName <*> getContactIdByName db user cName
    processChatCommand vr nm $ APIAddMember groupId contactId memRole
  JoinGroup gName enableNtfs -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIJoinGroup groupId enableNtfs
  AcceptMember gName gMemberName memRole -> withMemberName gName gMemberName $ \gId gMemberId -> APIAcceptMember gId gMemberId memRole
  MemberRole gName gMemberName memRole -> withMemberName gName gMemberName $ \gId gMemberId -> APIMembersRole gId [gMemberId] memRole
  BlockForAll gName gMemberName blocked -> withMemberName gName gMemberName $ \gId gMemberId -> APIBlockMembersForAll gId [gMemberId] blocked
  RemoveMembers gName gMemberNames withMessages -> withUser $ \user -> do
    (gId, gMemberIds) <- withStore $ \db -> do
      gId <- getGroupIdByName db user gName
      gMemberIds <- mapM (getGroupMemberIdByName db user gId) gMemberNames
      pure (gId, gMemberIds)
    processChatCommand vr nm $ APIRemoveMembers gId gMemberIds withMessages
  LeaveGroup gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APILeaveGroup groupId
  DeleteGroup gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIDeleteChat (ChatRef CTGroup groupId Nothing) (CDMFull True)
  ClearGroup gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIClearChat (ChatRef CTGroup groupId Nothing)
  ListMembers gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIListMembers groupId
  ListMemberSupportChats gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    (Group gInfo members) <- withFastStore $ \db -> getGroup db vr user groupId
    let memberSupportChats = filter (isJust . supportChat) members
    pure $ CRMemberSupportChats user gInfo memberSupportChats
  APIListGroups userId contactId_ search_ -> withUserId userId $ \user ->
    CRGroupsList user <$> withFastStore' (\db -> getBaseGroupDetails db vr user contactId_ search_)
  ListGroups cName_ search_ -> withUser $ \user@User {userId} -> do
    ct_ <- forM cName_ $ \cName -> withFastStore $ \db -> getContactByName db vr user cName
    processChatCommand vr nm $ APIListGroups userId (contactId' <$> ct_) search_
  APIUpdateGroupProfile groupId p' -> withUser $ \user -> do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    runUpdateGroupProfile user gInfo p'
  UpdateGroupNames gName GroupProfile {displayName, fullName, shortDescr} ->
    updateGroupProfileByName gName $ \p -> p {displayName, fullName, shortDescr}
  ShowGroupProfile gName -> withUser $ \user ->
    CRGroupProfile user <$> withFastStore (\db -> getGroupInfoByName db vr user gName)
  UpdateGroupDescription gName description ->
    updateGroupProfileByName gName $ \p -> p {description}
  ShowGroupDescription gName -> withUser $ \user ->
    CRGroupDescription user <$> withFastStore (\db -> getGroupInfoByName db vr user gName)
  APICreateGroupLink groupId mRole -> withUser $ \user -> withGroupLock "createGroupLink" groupId $ do
    gInfo@GroupInfo {groupProfile} <- withFastStore $ \db -> getGroupInfo db vr user groupId
    assertUserGroupRole gInfo GRAdmin
    when (mRole > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole
    groupLinkId <- GroupLinkId <$> drgRandomBytes 16
    subMode <- chatReadVar subscriptionMode
    let userData = encodeShortLinkData $ GroupShortLinkData groupProfile
        userLinkData = UserContactLinkData UserContactData {direct = True, owners = [], relays = [], userData}
        crClientData = encodeJSON $ CRDataGroup groupLinkId
    -- TODO [certs rcv]
    (connId, (ccLink, _serviceId)) <- withAgent $ \a -> createConnection a nm (aUserId user) True True SCMContact (Just userLinkData) (Just crClientData) IKPQOff subMode
    ccLink' <- createdGroupLink <$> shortenCreatedLink ccLink
    gVar <- asks random
    gLink <- withFastStore $ \db -> createGroupLink db gVar user gInfo connId ccLink' groupLinkId mRole subMode
    pure $ CRGroupLinkCreated user gInfo gLink
  APIGroupLinkMemberRole groupId mRole' -> withUser $ \user -> withGroupLock "groupLinkMemberRole" groupId $ do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    gLnk@GroupLink {acceptMemberRole} <- withFastStore $ \db -> getGroupLink db user gInfo
    assertUserGroupRole gInfo GRAdmin
    when (mRole' > GRMember) $ throwChatError $ CEGroupMemberInitialRole gInfo mRole'
    gLnk' <-
      if mRole' /= acceptMemberRole
        then withFastStore' $ \db -> setGroupLinkMemberRole db user gLnk mRole'
        else pure gLnk
    pure $ CRGroupLink user gInfo gLnk'
  APIDeleteGroupLink groupId -> withUser $ \user -> withGroupLock "deleteGroupLink" groupId $ do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    deleteGroupLink' user gInfo
    pure $ CRGroupLinkDeleted user gInfo
  APIGetGroupLink groupId -> withUser $ \user -> do
    gInfo <- withFastStore $ \db -> getGroupInfo db vr user groupId
    gLnk <- withFastStore $ \db -> getGroupLink db user gInfo
    pure $ CRGroupLink user gInfo gLnk
  APIAddGroupShortLink groupId -> withUser $ \user -> do
    (gInfo, gLink) <- withFastStore $ \db -> do
      gInfo <- getGroupInfo db vr user groupId
      gLink <- getGroupLink db user gInfo
      pure (gInfo, gLink)
    gLink' <- setGroupLinkData nm user gInfo gLink
    pure $ CRGroupLink user gInfo gLink'
  APICreateMemberContact gId gMemberId -> withUser $ \user -> do
    (g, m) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user gId <*> getGroupMember db vr user gId gMemberId
    assertUserGroupRole g GRAuthor
    unless (groupFeatureUserAllowed SGFDirectMessages g) $ throwCmdError "direct messages not allowed"
    case memberConn m of
      Just mConn@Connection {peerChatVRange} -> do
        unless (maxVersion peerChatVRange >= groupDirectInvVersion) $ throwChatError CEPeerChatVRangeIncompatible
        when (isJust $ memberContactId m) $ throwCmdError "member contact already exists"
        subMode <- chatReadVar subscriptionMode
        -- TODO PQ should negotitate contact connection with PQSupportOn?
        -- TODO [certs rcv]
        (connId, (CCLink cReq _, _serviceId)) <- withAgent $ \a -> createConnection a nm (aUserId user) True False SCMInvitation Nothing Nothing IKPQOff subMode
        -- [incognito] reuse membership incognito profile
        ct <- withFastStore' $ \db -> createMemberContact db user connId cReq g m mConn subMode
        void $ createChatItem user (CDDirectSnd ct) False CIChatBanner Nothing (Just epochStart)
        -- TODO not sure it is correct to set connections status here?
        pure $ CRNewMemberContact user ct g m
      _ -> throwChatError CEGroupMemberNotActive
  APISendMemberContactInvitation contactId msgContent_ -> withUser $ \user -> do
    (g@GroupInfo {groupId}, m, ct, cReq) <- withFastStore $ \db -> getMemberContact db vr user contactId
    when (contactGrpInvSent ct) $ throwCmdError "x.grp.direct.inv already sent"
    case memberConn m of
      Just mConn -> do
        -- TODO [knocking] send in correct scope - modiy API
        let msg = XGrpDirectInv cReq msgContent_ Nothing
        (sndMsg, _, _) <- sendDirectMemberMessage mConn msg groupId
        withFastStore' $ \db -> setContactGrpInvSent db ct True
        let ct' = ct {contactGrpInvSent = True}
        forM_ msgContent_ $ \mc -> do
          ci <- saveSndChatItem user (CDDirectSnd ct') sndMsg (CISndMsgContent mc)
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct') ci]
        pure $ CRNewMemberContactSentInv user ct' g m
      _ -> throwChatError CEGroupMemberNotActive
  APIAcceptMemberContact contactId -> withUser $ \user -> do
    (g, mConn, ct, groupDirectInv) <- withFastStore $ \db -> getMemberContactInvited db vr user contactId
    when (groupDirectInvStartedConnection groupDirectInv) $ throwCmdError "connection already started"
    connectMemberContact user g mConn ct groupDirectInv `catchAllErrors` \e -> do
      -- get updated contact, in case connection was started
      ct' <- withFastStore $ \db -> getContact db vr user contactId
      toView $ CEvtChatInfoUpdated user (AChatInfo SCTDirect $ DirectChat ct')
      throwError e
    -- get updated contact (groupDirectInvStartedConnection) with connection
    ct' <- withFastStore $ \db -> do
      liftIO $ setMemberContactStartedConnection db ct
      getContact db vr user contactId
    pure $ CRMemberContactAccepted user ct'
    where
      connectMemberContact user gInfo mConn Contact {activeConn} GroupDirectInvitation {groupDirectInvLink = cReq} =
        withInvitationLock "connect" (strEncode cReq) $ do
          subMode <- chatReadVar subscriptionMode
          case activeConn of
            -- Nothing is legacy branch for exisiting contacts without prepared connection;
            -- for new member contacts connection is prepared immediately (on xGrpDirectInv),
            -- so incognito profile can be attached to it and be visible in UI before accepting
            Nothing -> joinNewConn subMode
            Just conn@Connection {connStatus} -> case connStatus of
              ConnPrepared -> joinPreparedConn subMode conn
              _ -> throwChatError $ CEException "connection already started (past prepared status)"
        where
          joinNewConn subMode = do
            -- possible improvement: use agent connRequestPQSupport to determine pqSupport here;
            -- for joinPreparedConn below - same + encodeConnInfoPQ;
            -- same for auto-accept on xGrpDirectInv
            acId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq PQSupportOff
            conn <- withStore $ \db -> do
              connId <- liftIO $ createMemberContactConn db user acId Nothing gInfo mConn ConnPrepared contactId subMode
              getConnectionById db vr user connId
            joinPreparedConn subMode conn
          joinPreparedConn subMode conn = do
            -- [incognito] send membership incognito profile
            let p = userProfileDirect user (fromLocalProfile <$> incognitoMembershipProfile gInfo) Nothing True
            dm <- encodeConnInfo $ XInfo p
            (sqSecured, _serviceId) <- withAgent $ \a -> joinConnection a nm (aUserId user) (aConnId conn) True cReq dm PQSupportOff subMode
            let newStatus = if sqSecured then ConnSndReady else ConnJoined
            void $ withFastStore' $ \db -> updateConnectionStatusFromTo db conn ConnPrepared newStatus
  CreateGroupLink gName mRole -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APICreateGroupLink groupId mRole
  GroupLinkMemberRole gName mRole -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIGroupLinkMemberRole groupId mRole
  DeleteGroupLink gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIDeleteGroupLink groupId
  ShowGroupLink gName -> withUser $ \user -> do
    groupId <- withFastStore $ \db -> getGroupIdByName db user gName
    processChatCommand vr nm $ APIGetGroupLink groupId
  SendGroupMessageQuote gName cName quotedMsg msg -> withUser $ \user -> do
    (groupId, quotedItemId, mentions) <-
      withFastStore $ \db -> do
        gId <- getGroupIdByName db user gName
        qiId <- getGroupChatItemIdByText db user gId cName quotedMsg
        (gId, qiId,) <$> liftIO (getMessageMentions db user gId msg)
    let mc = MCText msg
    processChatCommand vr nm $ APISendMessages (SRGroup groupId Nothing) False Nothing [ComposedMessage Nothing (Just quotedItemId) mc mentions]
  ClearNoteFolder -> withUser $ \user -> do
    folderId <- withFastStore (`getUserNoteFolderId` user)
    processChatCommand vr nm $ APIClearChat (ChatRef CTLocal folderId Nothing)
  LastChats count_ -> withUser' $ \user -> do
    let count = fromMaybe 5000 count_
    (errs, previews) <- partitionEithers <$> withFastStore' (\db -> getChatPreviews db vr user False (PTLast count) clqNoFilters)
    unless (null errs) $ toView $ CEvtChatErrors (map ChatErrorStore errs)
    pure $ CRChats previews
  LastMessages (Just chatName) count search -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand vr nm $ APIGetChat chatRef Nothing (CPLast count) search
    pure $ CRChatItems user (Just chatName) (aChatItems . chat $ chatResp)
  LastMessages Nothing count search -> withUser $ \user -> do
    chatItems <- withFastStore $ \db -> getAllChatItems db vr user (CPLast count) search
    pure $ CRChatItems user Nothing chatItems
  LastChatItemId (Just chatName) index -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    chatResp <- processChatCommand vr nm $ APIGetChat chatRef Nothing (CPLast $ index + 1) Nothing
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
    processChatCommand vr nm $ APIGetChatItemInfo chatRef itemId
  ShowLiveItems on -> withUser $ \_ ->
    asks showLiveItems >>= atomically . (`writeTVar` on) >> ok_
  SendFile chatName f -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    case chatRef of
      ChatRef CTLocal folderId _ -> processChatCommand vr nm $ APICreateChatItems folderId [composedMessage (Just f) (MCFile "")]
      _ -> withSendRef chatRef $ \sendRef -> processChatCommand vr nm $ APISendMessages sendRef False Nothing [composedMessage (Just f) (MCFile "")]
  SendImage chatName f@(CryptoFile fPath _) -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    withSendRef chatRef $ \sendRef -> do
      filePath <- lift $ toFSFilePath fPath
      unless (any (`isSuffixOf` map toLower fPath) imageExtensions) $ throwChatError CEFileImageType {filePath}
      fileSize <- getFileSize filePath
      unless (fileSize <= maxImageSize) $ throwChatError CEFileImageSize {filePath}
      -- TODO include file description for preview
      processChatCommand vr nm $ APISendMessages sendRef False Nothing [composedMessage (Just f) (MCImage "" fixedImagePreview)]
  ForwardFile chatName fileId -> forwardFile chatName fileId SendFile
  ForwardImage chatName fileId -> forwardFile chatName fileId SendImage
  SendFileDescription _chatName _f -> throwCmdError "TODO"
  -- TODO to use priority transactions we need a parameter that differentiates manual and automatic acceptance
  ReceiveFile fileId userApprovedRelays encrypted_ rcvInline_ filePath_ -> withUser $ \_ ->
    withFileLock "receiveFile" fileId $ do
      (user, ft@RcvFileTransfer {fileStatus}) <- withStore (`getRcvFileTransferById` fileId)
      encrypt <- (`fromMaybe` encrypted_) <$> chatReadVar encryptLocalFiles
      ft' <- (if encrypt && fileStatus == RFSNew then setFileToEncrypt else pure) ft
      receiveFile' user ft' userApprovedRelays rcvInline_ filePath_
  SetFileToReceive fileId userApprovedRelays encrypted_ -> withUser $ \_ -> do
    withFileLock "setFileToReceive" fileId $ do
      encrypt <- (`fromMaybe` encrypted_) <$> chatReadVar encryptLocalFiles
      cfArgs <- if encrypt then Just <$> (atomically . CF.randomArgs =<< asks random) else pure Nothing
      withStore' $ \db -> setRcvFileToReceive db fileId userApprovedRelays cfArgs
      ok_
  CancelFile fileId -> withUser $ \user@User {userId} ->
    withFileLock "cancelFile" fileId $
      withFastStore (\db -> getFileTransfer db user fileId) >>= \case
        FTSnd ftm@FileTransferMeta {xftpSndFile, cancelled} fts
          | cancelled -> throwChatError $ CEFileCancel fileId "file already cancelled"
          | not (null fts) && all fileCancelledOrCompleteSMP fts ->
              throwChatError $ CEFileCancel fileId "file transfer is complete"
          | otherwise -> do
              cancelSndFile user ftm fts True
              cref_ <- withFastStore' $ \db -> lookupChatRefByFileId db user fileId
              aci_ <- withFastStore $ \db -> lookupChatItemByFileId db vr user fileId
              case (cref_, aci_) of
                (Nothing, _) ->
                  pure $ CRSndFileCancelled user Nothing ftm fts
                (Just (ChatRef CTDirect contactId _), Just aci) -> do
                  (contact, sharedMsgId) <- withFastStore $ \db -> (,) <$> getContact db vr user contactId <*> getSharedMsgIdByFileId db userId fileId
                  void . sendDirectContactMessage user contact $ XFileCancel sharedMsgId
                  pure $ CRSndFileCancelled user (Just aci) ftm fts
                (Just (ChatRef CTGroup groupId scope), Just aci) -> do
                  (gInfo, sharedMsgId) <- withFastStore $ \db -> (,) <$> getGroupInfo db vr user groupId <*> getSharedMsgIdByFileId db userId fileId
                  chatScopeInfo <- mapM (getChatScopeInfo vr user) scope
                  recipients <- getGroupRecipients vr user gInfo chatScopeInfo groupKnockingVersion
                  void . sendGroupMessage user gInfo scope recipients $ XFileCancel sharedMsgId
                  pure $ CRSndFileCancelled user (Just aci) ftm fts
                (Just _, _) -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
          where
            fileCancelledOrCompleteSMP SndFileTransfer {fileStatus = s} =
              s == FSCancelled || (s == FSComplete && isNothing xftpSndFile)
        FTRcv ftr@RcvFileTransfer {cancelled, fileStatus, xftpRcvFile}
          | cancelled -> throwChatError $ CEFileCancel fileId "file already cancelled"
          | rcvFileComplete fileStatus -> throwChatError $ CEFileCancel fileId "file transfer is complete"
          | otherwise -> case xftpRcvFile of
              Nothing -> do
                cancelRcvFileTransfer user ftr
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
          throwCmdError "not supported for local files"
        Just CIFile {fileProtocol = FPXFTP} ->
          pure $ CRFileTransferStatusXFTP user ci
        _ -> do
          fileStatus <- withFastStore $ \db -> getFileTransferProgress db user fileId
          pure $ CRFileTransferStatus user fileStatus
  ShowProfile -> withUser $ \user@User {profile} -> pure $ CRUserProfile user (fromLocalProfile profile)
  SetBotCommands commands -> withUser $ \user@User {profile} -> do
    let LocalProfile {preferences} = profile
        prefs = Just (fromMaybe emptyChatPrefs preferences :: Preferences) {commands = Just commands}
        p = (fromLocalProfile profile :: Profile) {preferences = prefs, peerType = Just CPTBot}
    updateProfile user p
  UpdateProfile displayName shortDescr -> withUser $ \user@User {profile} -> do
    let p = (fromLocalProfile profile :: Profile) {displayName, shortDescr, fullName = ""}
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
  SetGroupMemberAdmissionReview gName reviewAdmissionApplication ->
    updateGroupProfileByName gName $ \p@GroupProfile {memberAdmission} ->
      case memberAdmission of
        Nothing -> p {memberAdmission = Just (emptyGroupMemberAdmission :: GroupMemberAdmission) {review = reviewAdmissionApplication}}
        Just ma -> p {memberAdmission = Just (ma :: GroupMemberAdmission) {review = reviewAdmissionApplication}}
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
  VerifyRemoteCtrlSession sessId -> withUser_ $ verifyRemoteCtrlSession (execChatCommand Nothing) sessId
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
    chatMigrations <- map upMigration <$> withFastStore' (getCurrentMigrations Nothing)
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
        CLInvitation bs -> "Invitation " <> safeDecodeUtf8 bs
        CLConnection connId -> "Connection " <> tshow connId
        CLContact ctId -> "Contact " <> tshow ctId
        CLGroup gId -> "Group " <> tshow gId
        CLUserContact ucId -> "UserContact " <> tshow ucId
        CLContactRequest crId -> "ContactRequest " <> tshow crId
        CLFile fId -> "File " <> tshow fId
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
    ok_ = pure $ CRCmdOk Nothing
    ok = pure . CRCmdOk . Just
    getChatRef :: User -> ChatName -> CM ChatRef
    getChatRef user (ChatName cType name) = do
      chatId <- case cType of
        CTDirect -> withFastStore $ \db -> getContactIdByName db user name
        CTGroup -> withFastStore $ \db -> getGroupIdByName db user name
        CTLocal
          | name == "" -> withFastStore (`getUserNoteFolderId` user)
          | otherwise -> throwCmdError "not supported"
        _ -> throwCmdError "not supported"
      pure $ ChatRef cType chatId Nothing
    getChatRefAndMentions :: User -> ChatName -> Text -> CM (ChatRef, Map MemberName GroupMemberId)
    getChatRefAndMentions user cName msg = do
      chatRef@(ChatRef cType chatId _) <- getChatRef user cName
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
    withUserName uName cmd = withFastStore (`getUserIdByName` uName) >>= processChatCommand vr nm . cmd
    withContactName :: ContactName -> (ContactId -> ChatCommand) -> CM ChatResponse
    withContactName cName cmd = withUser $ \user ->
      withFastStore (\db -> getContactIdByName db user cName) >>= processChatCommand vr nm . cmd
    withMemberName :: GroupName -> ContactName -> (GroupId -> GroupMemberId -> ChatCommand) -> CM ChatResponse
    withMemberName gName mName cmd = withUser $ \user ->
      getGroupAndMemberId user gName mName >>= processChatCommand vr nm . uncurry cmd
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
    getSentChatItemIdByText user@User {userId, localDisplayName} (ChatRef cType cId _scope) msg = case cType of
      CTDirect -> withFastStore $ \db -> getDirectChatItemIdByText db userId cId SMDSnd msg
      CTGroup -> withFastStore $ \db -> getGroupChatItemIdByText db user cId (Just localDisplayName) msg
      CTLocal -> withFastStore $ \db -> getLocalChatItemIdByText db user cId SMDSnd msg
      _ -> throwCmdError "not supported"
    getChatItemIdByText :: User -> ChatRef -> Text -> CM Int64
    getChatItemIdByText user (ChatRef cType cId _scope) msg = case cType of
      CTDirect -> withFastStore $ \db -> getDirectChatItemIdByText' db user cId msg
      CTGroup -> withFastStore $ \db -> getGroupChatItemIdByText' db user cId msg
      CTLocal -> withFastStore $ \db -> getLocalChatItemIdByText' db user cId msg
      _ -> throwCmdError "not supported"
    connectViaInvitation :: User -> IncognitoEnabled -> CreatedLinkInvitation -> Maybe ContactId -> CM (Connection, Maybe Profile)
    connectViaInvitation user@User {userId} incognito (CCLink cReq@(CRInvitationUri crData e2e) sLnk_) contactId_ =
      withInvitationLock "connect" (strEncode cReq) $ do
        subMode <- chatReadVar subscriptionMode
        lift (withAgent' $ \a -> connRequestPQSupport a PQSupportOn cReq) >>= \case
          Nothing -> throwChatError CEInvalidConnReq
          -- TODO PQ the error above should be CEIncompatibleConnReqVersion, also the same API should be called in Plan
          Just (agentV, pqSup') -> do
            let chatV = agentToChatVersion agentV
            withFastStore' (\db -> getConnectionEntityByConnReq db vr user cReqs) >>= \case
              Nothing -> joinNewConn chatV
              Just (RcvDirectMsgConnection conn@Connection {connStatus, contactConnInitiated, customUserProfileId} _ct_)
                | connStatus == ConnNew && contactConnInitiated -> joinNewConn chatV -- own connection link
                | connStatus == ConnPrepared -> do -- retrying join after error
                    localIncognitoProfile <- forM customUserProfileId $ \pId -> withFastStore $ \db -> getProfileById db userId pId
                    joinPreparedConn conn (fromLocalProfile <$> localIncognitoProfile) chatV
              Just ent -> throwCmdError $ "connection is not RcvDirectMsgConnection: " <> show (connEntityInfo ent)
            where
              joinNewConn chatV = do
                -- [incognito] generate profile to send
                incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
                connId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq pqSup'
                let ccLink = CCLink cReq $ serverShortLink <$> sLnk_
                conn <- withFastStore' $ \db -> createDirectConnection' db userId connId ccLink contactId_ ConnPrepared incognitoProfile subMode chatV pqSup'
                joinPreparedConn conn incognitoProfile chatV
              joinPreparedConn conn incognitoProfile chatV = do
                let profileToSend = userProfileDirect user incognitoProfile Nothing True
                dm <- encodeConnInfoPQ pqSup' chatV $ XInfo profileToSend
                (sqSecured, _serviceId) <- withAgent $ \a -> joinConnection a nm (aUserId user) (aConnId conn) True cReq dm pqSup' subMode
                let newStatus = if sqSecured then ConnSndReady else ConnJoined
                conn' <- withFastStore' $ \db -> updateConnectionStatusFromTo db conn ConnPrepared newStatus
                pure (conn', incognitoProfile)
              cReqs =
                ( CRInvitationUri crData {crScheme = SSSimplex} e2e,
                  CRInvitationUri crData {crScheme = simplexChat} e2e
                )
    connectViaContact :: User -> Maybe PreparedChatEntity -> IncognitoEnabled -> CreatedLinkContact -> Maybe SharedMsgId -> Maybe (SharedMsgId, MsgContent) -> CM ConnectViaContactResult
    connectViaContact user@User {userId} preparedEntity_ incognito (CCLink cReq@(CRContactUri crData@ConnReqUriData {crClientData}) sLnk) welcomeSharedMsgId msg_ = withInvitationLock "connectViaContact" (strEncode cReq) $ do
      let groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
      -- groupLinkId is Nothing for business chats
      when (isJust msg_ && isJust groupLinkId) $ throwChatError CEConnReqMessageProhibited
      case preparedEntity_ of
        Just (PCEContact ct@Contact {activeConn}) -> case activeConn of
          Nothing -> connect' Nothing Nothing Nothing
          Just conn@Connection {connStatus, xContactId} -> case connStatus of
            ConnPrepared -> joinPreparedConn' xContactId conn Nothing
            _ -> pure $ CVRConnectedContact ct
        Just (PCEGroup gInfo GroupMember {activeConn}) -> case activeConn of
          Nothing -> connect' groupLinkId Nothing (Just $ Just gInfo)
          Just conn@Connection {connStatus, xContactId} -> case connStatus of
            ConnPrepared -> joinPreparedConn' xContactId conn (Just $ Just gInfo)
            _ -> connect' groupLinkId xContactId (Just $ Just gInfo) -- why not "already connected" for host member?
        Nothing ->
          withFastStore' (\db -> getConnReqContactXContactId db vr user cReqHash1 cReqHash2) >>= \case
            Right ct@Contact {activeConn} -> case groupLinkId of
              Nothing -> case activeConn of
                Just conn@Connection {connStatus = ConnPrepared, xContactId} -> joinPreparedConn' xContactId conn Nothing
                _ -> pure $ CVRConnectedContact ct
              Just gLinkId ->
                -- allow repeat contact request
                -- TODO [short links] is this branch needed? it probably remained from the time we created host contact
                connect' (Just gLinkId) Nothing (Just Nothing)
            Left conn_ -> case conn_ of
              Just conn@Connection {connStatus = ConnPrepared, xContactId} -> joinPreparedConn' xContactId conn (groupLinkId $> Nothing)
              -- TODO [short links] this is executed on repeat request after success
              -- it probably should send the second message without creating the second connection?
              Just Connection {xContactId} -> connect' groupLinkId xContactId (groupLinkId $> Nothing)
              Nothing -> connect' groupLinkId Nothing (groupLinkId $> Nothing)
      where
        cReqHash = ConnReqUriHash . C.sha256Hash . strEncode
        cReqHash1 = cReqHash $ CRContactUri crData {crScheme = SSSimplex}
        cReqHash2 = cReqHash $ CRContactUri crData {crScheme = simplexChat}
        joinPreparedConn' xContactId_ conn@Connection {customUserProfileId} gInfo_ = do
          when (incognito /= isJust customUserProfileId) $ throwCmdError "incognito mode is different from prepared connection"
          -- TODO [relays] member: refactor joinContact and up avoiding parallel ifs, xContactId is not used
          xContactId <- mkXContactId xContactId_
          localIncognitoProfile <- forM customUserProfileId $ \pId -> withFastStore $ \db -> getProfileById db userId pId
          let incognitoProfile = fromLocalProfile <$> localIncognitoProfile
          conn' <- joinContact user conn cReq incognitoProfile xContactId welcomeSharedMsgId msg_ gInfo_ PQSupportOn
          pure $ CVRSentInvitation conn' incognitoProfile
        connect' groupLinkId xContactId_ gInfo_ = do
          let inGroup = isJust groupLinkId
              pqSup = if inGroup then PQSupportOff else PQSupportOn
          (connId, chatV) <- prepareContact user cReq pqSup
          xContactId <- mkXContactId xContactId_
          -- [incognito] generate profile to send, or use membership profile for relay groups
          incognitoProfile_ <- case gInfo_ of
            Just (Just gInfo) | useRelays' gInfo -> pure $ ExistingIncognito <$> incognitoMembershipProfile gInfo
            _ -> if incognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
          let incognitoProfile = fromIncognitoProfile <$> incognitoProfile_
          subMode <- chatReadVar subscriptionMode
          let sLnk' = serverShortLink <$> sLnk
          conn <- withFastStore' $ \db -> createConnReqConnection db userId connId preparedEntity_ cReq cReqHash1 sLnk' xContactId incognitoProfile_ groupLinkId subMode chatV pqSup
          conn' <- joinContact user conn cReq incognitoProfile xContactId welcomeSharedMsgId msg_ gInfo_ pqSup
          pure $ CVRSentInvitation conn' incognitoProfile
    connectContactViaAddress :: User -> IncognitoEnabled -> Contact -> CreatedLinkContact -> CM ChatResponse
    connectContactViaAddress user@User {userId} incognito ct@Contact {contactId, activeConn} (CCLink cReq shortLink) =
      withInvitationLock "connectContactViaAddress" (strEncode cReq) $
        case activeConn of
          Nothing -> do
            let pqSup = PQSupportOn
            (connId, chatV) <- prepareContact user cReq pqSup
            newXContactId <- XContactId <$> drgRandomBytes 16
            -- [incognito] generate profile to send
            incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
            subMode <- chatReadVar subscriptionMode
            let cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
            conn <- withFastStore' $ \db -> createConnReqConnection db userId connId (Just $ PCEContact ct) cReq cReqHash shortLink newXContactId (NewIncognito <$> incognitoProfile) Nothing subMode chatV pqSup
            void $ joinContact user conn cReq incognitoProfile newXContactId Nothing Nothing Nothing pqSup
            ct' <- withStore $ \db -> getContact db vr user contactId
            pure $ CRSentInvitationToContact user ct' incognitoProfile
          Just conn@Connection {connStatus, xContactId = xContactId_, customUserProfileId} -> case connStatus of
            ConnPrepared -> do
              when (incognito /= isJust customUserProfileId) $ throwCmdError "incognito mode is different from prepared connection"
              xContactId <- mkXContactId xContactId_
              localIncognitoProfile <- forM customUserProfileId $ \pId -> withFastStore $ \db -> getProfileById db userId pId
              let incognitoProfile = fromLocalProfile <$> localIncognitoProfile
              void $ joinContact user conn cReq incognitoProfile xContactId Nothing Nothing Nothing PQSupportOn
              ct' <- withStore $ \db -> getContact db vr user contactId
              pure $ CRSentInvitationToContact user ct' incognitoProfile
            _ -> throwCmdError "contact already has connection"
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
    mkXContactId :: Maybe XContactId -> CM XContactId
    mkXContactId = maybe (XContactId <$> drgRandomBytes 16) pure
    joinContact :: User -> Connection -> ConnReqContact -> Maybe Profile -> XContactId -> Maybe SharedMsgId -> Maybe (SharedMsgId, MsgContent) -> Maybe (Maybe GroupInfo) -> PQSupport -> CM Connection
    joinContact user conn@Connection {connChatVersion = chatV} cReq incognitoProfile xContactId welcomeSharedMsgId msg_ gInfo_ pqSup = do
      -- gInfo_ is Maybe (Maybe GroupInfo), where Just Nothing means "some unknown group", e.g. when joining via link without profile
      let profileToSend = case gInfo_ of
            Just gInfo_' ->
              let allowSimplexLinks = maybe True (groupFeatureUserAllowed SGFSimplexLinks) gInfo_'
               in userProfileInGroup' user allowSimplexLinks incognitoProfile
            Nothing -> userProfileDirect user incognitoProfile Nothing True
          chatEvent = case gInfo_ of
            Just (Just gInfo) | useRelays' gInfo ->
              let GroupInfo {membership = GroupMember {memberId}} = gInfo
               in XMember profileToSend memberId
            _ -> XContact profileToSend (Just xContactId) welcomeSharedMsgId msg_
      dm <- encodeConnInfoPQ pqSup chatV chatEvent
      subMode <- chatReadVar subscriptionMode
      void $ withAgent $ \a -> joinConnection a nm (aUserId user) (aConnId conn) True cReq dm pqSup subMode
      withFastStore' $ \db -> updateConnectionStatusFromTo db conn ConnPrepared ConnJoined
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
    updateProfile user p' = updateProfile_ user p' True $ withFastStore $ \db -> updateUserProfile db user p'
    updateProfile_ :: User -> Profile -> Bool -> CM User -> CM ChatResponse
    updateProfile_ user@User {profile = p@LocalProfile {displayName = n}} p'@Profile {displayName = n'} shouldUpdateAddressData updateUser
      | p' == fromLocalProfile p = pure $ CRUserProfileNoChange user
      | otherwise = do
          when (n /= n') $ checkValidName n'
          -- read contacts before user update to correctly merge preferences
          contacts <- withFastStore' $ \db -> getUserContacts db vr user
          user' <- updateUser
          asks currentUser >>= atomically . (`writeTVar` Just user')
          withChatLock "updateProfile" $ do
            when shouldUpdateAddressData $ setMyAddressData' user'
            summary <- sendUpdateToContacts user' contacts
            pure $ CRUserProfileUpdated user' (fromLocalProfile p) p' summary
      where
        setMyAddressData' :: User -> CM ()
        setMyAddressData' user' =
          withFastStore' (\db -> runExceptT $ getUserAddress db user) >>= \case
            Right ucl@UserContactLink {shortLinkDataSet}
              | shortLinkDataSet -> void $ setMyAddressData user' ucl
            _ -> pure ()
        sendUpdateToContacts :: User -> [Contact] -> CM UserProfileUpdateSummary
        sendUpdateToContacts user' contacts = do
          let changedCts_ = L.nonEmpty $ foldr addChangedProfileContact [] contacts
          case changedCts_ of
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
          where
            -- [incognito] filter out contacts with whom user has incognito connections
            addChangedProfileContact :: Contact -> [ChangedProfileContact] -> [ChangedProfileContact]
            addChangedProfileContact ct changedCts = case contactSendConn_ ct' of
              Right conn
                | not (connIncognito conn) && mergedProfile' /= mergedProfile ->
                    ChangedProfileContact ct ct' mergedProfile' conn : changedCts
              _ -> changedCts
              where
                mergedProfile = userProfileDirect user Nothing (Just ct) False
                ct' = updateMergedPreferences user' ct
                mergedProfile' = userProfileDirect user' Nothing (Just ct') False
            ctSndEvent :: ChangedProfileContact -> (ConnOrGroupId, ChatMsgEvent 'Json)
            ctSndEvent ChangedProfileContact {mergedProfile', conn = Connection {connId}} = (ConnectionId connId, XInfo mergedProfile')
            ctMsgReq :: ChangedProfileContact -> Either ChatError SndMessage -> Either ChatError ChatMsgReq
            ctMsgReq ChangedProfileContact {conn} =
              fmap $ \SndMessage {msgId, msgBody} ->
                (conn, MsgFlags {notification = hasNotification XInfo_}, (vrValue msgBody, [msgId]))
    setMyAddressData :: User -> UserContactLink -> CM UserContactLink
    setMyAddressData user ucl@UserContactLink {userContactLinkId, connLinkContact = CCLink connFullLink _sLnk_, addressSettings} = do
      conn <- withFastStore $ \db -> getUserAddressConnection db vr user
      let shortLinkProfile = userProfileDirect user Nothing Nothing True
          -- TODO [short links] do not save address to server if data did not change, spinners, error handling
          userData = contactShortLinkData shortLinkProfile $ Just addressSettings
          userLinkData = UserContactLinkData UserContactData {direct = True, owners = [], relays = [], userData}
      sLnk <- shortenShortLink' =<< withAgent (\a -> setConnShortLink a nm (aConnId conn) SCMContact userLinkData Nothing)
      withFastStore' $ \db -> setUserContactLinkShortLink db userContactLinkId sLnk
      let autoAccept' = (\aa -> aa {acceptIncognito = False}) <$> autoAccept addressSettings
          ucl' = (ucl :: UserContactLink) {connLinkContact = CCLink connFullLink (Just sLnk), shortLinkDataSet = True, shortLinkLargeDataSet = BoolDef True, addressSettings = addressSettings {autoAccept = autoAccept'}}
      pure ucl'
    updateContactPrefs :: User -> Contact -> Preferences -> CM ChatResponse
    updateContactPrefs _ ct@Contact {activeConn = Nothing} _ = throwChatError $ CEContactNotActive ct
    updateContactPrefs user@User {userId} ct@Contact {activeConn = Just Connection {customUserProfileId}, userPreferences = contactUserPrefs} contactUserPrefs'
      | contactUserPrefs == contactUserPrefs' = pure $ CRContactPrefsUpdated user ct ct
      | otherwise = do
          assertDirectAllowed user MDSnd ct XInfo_
          ct' <- withStore' $ \db -> updateContactUserPreferences db user ct contactUserPrefs'
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore $ \db -> getProfileById db userId profileId
          let mergedProfile = userProfileDirect user (fromLocalProfile <$> incognitoProfile) (Just ct) False
              mergedProfile' = userProfileDirect user (fromLocalProfile <$> incognitoProfile) (Just ct') False
          when (mergedProfile' /= mergedProfile) $
            withContactLock "updateContactPrefs" (contactId' ct) $ do
              void (sendDirectContactMessage user ct' $ XInfo mergedProfile') `catchAllErrors` eToView
              lift . when (directOrUsed ct') $ createSndFeatureItems user ct ct'
          pure $ CRContactPrefsUpdated user ct ct'
    runUpdateGroupProfile :: User -> GroupInfo -> GroupProfile -> CM ChatResponse
    runUpdateGroupProfile user gInfo@GroupInfo {businessChat, groupProfile = p@GroupProfile {displayName = n}} p'@GroupProfile {displayName = n'} = do
      assertUserGroupRole gInfo GROwner
      when (n /= n') $ checkValidName n'
      gInfo' <- withStore $ \db -> updateGroupProfile db user gInfo p'
      msg <- case businessChat of
        Just BusinessChatInfo {businessId} -> do
          ms <- withStore' $ \db -> getGroupMembers db vr user gInfo'
          let (newMs, oldMs) = partition (\m -> maxVersion (memberChatVRange m) >= businessChatPrefsVersion) ms
          -- this is a fallback to send the members with the old version correct profile of the business when preferences change
          unless (null oldMs) $ do
            GroupMember {memberProfile = LocalProfile {displayName, fullName, shortDescr, image}} <-
              withStore $ \db -> getGroupMemberByMemberId db vr user gInfo' businessId
            let p'' = p' {displayName, fullName, shortDescr, image} :: GroupProfile
                recipients = filter memberCurrentOrPending oldMs
            void $ sendGroupMessage user gInfo' Nothing recipients (XGrpInfo p'')
          let ps' = fromMaybe defaultBusinessGroupPrefs $ groupPreferences p'
              recipients = filter memberCurrentOrPending newMs
          sendGroupMessage user gInfo' Nothing recipients $ XGrpPrefs ps'
        Nothing -> do
          void $ setGroupLinkData' nm user gInfo'
          recipients <- getRecipients
          sendGroupMessage user gInfo' Nothing recipients (XGrpInfo p')
          where
            getRecipients
              | useRelays' gInfo' = withFastStore' $ \db -> getGroupRelayMembers db vr user gInfo'
              | otherwise = do
                  ms <- withFastStore' $ \db -> getGroupMembers db vr user gInfo'
                  pure $ filter memberCurrentOrPending ms
      let cd = CDGroupSnd gInfo' Nothing
      unless (sameGroupProfileInfo p p') $ do
        ci <- saveSndChatItem user cd msg (CISndGroupEvent $ SGEGroupUpdated p')
        toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo' Nothing) ci]
      createGroupFeatureChangedItems user cd CISndGroupFeature gInfo gInfo'
      pure $ CRGroupUpdated user gInfo gInfo' Nothing
    checkValidName :: GroupName -> CM ()
    checkValidName displayName = do
      when (T.null displayName) $ throwChatError CEInvalidDisplayName {displayName, validName = ""}
      let validName = T.pack $ mkValidName $ T.unpack displayName
      when (displayName /= validName) $ throwChatError CEInvalidDisplayName {displayName, validName}
    assertUserGroupRole :: GroupInfo -> GroupMemberRole -> CM ()
    assertUserGroupRole g@GroupInfo {membership} requiredRole = do
      when (memberRole' membership < requiredRole) $ throwChatError $ CEGroupUserRole g requiredRole
      when (memberStatus membership == GSMemInvited) $ throwChatError (CEGroupNotJoined g)
      when (memberRemoved membership) $ throwChatError CEGroupMemberUserRemoved
      unless (memberActive membership) $ throwChatError CEGroupMemberNotActive
    delGroupChatItemsForMembers :: User -> GroupInfo -> Maybe GroupChatScopeInfo -> [GroupMember] -> [CChatItem 'CTGroup] -> CM [ChatItemDeletion]
    delGroupChatItemsForMembers user gInfo chatScopeInfo ms items = do
      assertDeletable gInfo items
      assertUserGroupRole gInfo GRModerator
      let msgMemIds = itemsMsgMemIds gInfo items
          events = L.nonEmpty $ map (\(msgId, memId) -> XMsgDel msgId (Just memId) $ toMsgScope gInfo <$> chatScopeInfo) msgMemIds
      mapM_ (sendGroupMessages_ user gInfo ms) events
      delGroupChatItems user gInfo chatScopeInfo items True
      where
        assertDeletable :: GroupInfo -> [CChatItem 'CTGroup] -> CM ()
        assertDeletable GroupInfo {membership} items' =
          unless (all itemDeletable items') $ throwChatError CEInvalidChatItemDelete
          where
            itemDeletable :: CChatItem 'CTGroup -> Bool
            itemDeletable (CChatItem _ ChatItem {chatDir, meta = CIMeta {itemSharedMsgId}}) =
              case chatDir of
                CIGroupRcv GroupMember {memberRole} -> memberRole' membership >= memberRole && isJust itemSharedMsgId
                CIGroupSnd -> isJust itemSharedMsgId
        itemsMsgMemIds :: GroupInfo -> [CChatItem 'CTGroup] -> [(SharedMsgId, MemberId)]
        itemsMsgMemIds GroupInfo {membership = GroupMember {memberId = membershipMemId}} = mapMaybe itemMsgMemIds
          where
            itemMsgMemIds :: CChatItem 'CTGroup -> Maybe (SharedMsgId, MemberId)
            itemMsgMemIds (CChatItem _ ChatItem {chatDir, meta = CIMeta {itemSharedMsgId}}) =
              join <$> forM itemSharedMsgId $ \msgId -> Just $ case chatDir of
                CIGroupRcv GroupMember {memberId} -> (msgId, memberId)
                CIGroupSnd -> (msgId, membershipMemId)

    delGroupChatItems :: User -> GroupInfo -> Maybe GroupChatScopeInfo -> [CChatItem 'CTGroup] -> Bool -> CM [ChatItemDeletion]
    delGroupChatItems user gInfo@GroupInfo {membership} chatScopeInfo items moderation = do
      deletedTs <- liftIO getCurrentTime
      when moderation $ do
        ciIds <- concat <$> withStore' (\db -> forM items $ \(CChatItem _ ci) -> markMessageReportsDeleted db user gInfo ci membership deletedTs)
        unless (null ciIds) $ toView $ CEvtGroupChatItemsDeleted user gInfo ciIds True (Just membership)
      let m = if moderation then Just membership else Nothing
      if groupFeatureUserAllowed SGFFullDelete gInfo
        then deleteGroupCIs user gInfo chatScopeInfo items m deletedTs
        else markGroupCIsDeleted user gInfo chatScopeInfo items m deletedTs
    updateGroupProfileByName :: GroupName -> (GroupProfile -> GroupProfile) -> CM ChatResponse
    updateGroupProfileByName gName update = withUser $ \user -> do
      gInfo@GroupInfo {groupProfile = p} <- withStore $ \db ->
        getGroupIdByName db user gName >>= getGroupInfo db vr user
      runUpdateGroupProfile user gInfo $ update p
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
    validateAllUsersServers :: UserServersClass u => Int64 -> [u] -> CM ([UserServersError], [UserServersWarning])
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
        FTRcv RcvFileTransfer {fileStatus = RFSComplete filePath, cryptoArgs} -> forward filePath cryptoArgs
        FTSnd {fileTransferMeta = FileTransferMeta {filePath, xftpSndFile}} -> forward filePath $ xftpSndFile >>= \XFTPSndFile {cryptoArgs} -> cryptoArgs
        _ -> throwChatError CEFileNotReceived {fileId}
      where
        forward path cfArgs = processChatCommand vr nm $ sendCommand chatName $ CryptoFile path cfArgs
    getGroupAndMemberId :: User -> GroupName -> ContactName -> CM (GroupId, GroupMemberId)
    getGroupAndMemberId user gName groupMemberName =
      withStore $ \db -> do
        groupId <- getGroupIdByName db user gName
        groupMemberId <- getGroupMemberIdByName db user groupId groupMemberName
        pure (groupId, groupMemberId)
    newGroup :: User -> IncognitoEnabled -> GroupProfile -> Bool -> CM GroupInfo
    newGroup user incognito gProfile@GroupProfile {displayName} useRelays = do
      checkValidName displayName
      gVar <- asks random
      -- [incognito] generate incognito profile for group membership
      incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
      gInfo <- withFastStore $ \db -> createNewGroup db vr gVar user gProfile incognitoProfile useRelays
      let cd = CDGroupSnd gInfo Nothing
      createInternalChatItem user cd CIChatBanner (Just epochStart)
      createInternalChatItem user cd (CISndGroupE2EEInfo E2EInfo {pqEnabled = Just PQEncOff}) Nothing
      createGroupFeatureItems user cd CISndGroupFeature gInfo
      pure gInfo
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
        startProximateTimedItemThread user (ChatRef CTDirect contactId Nothing, chatItemId' ci)
    addRelays :: User -> GroupInfo -> ShortLinkContact -> [UserChatRelay] -> CM [GroupRelay]
    addRelays user gInfo@GroupInfo {membership} groupSLink relays =
      mapConcurrently addRelay relays
      where
        addRelay :: UserChatRelay -> CM GroupRelay
        addRelay relay@UserChatRelay {address} = do
          -- TODO [relays] owner: can update relay profile from data retrieved via getConnShortLink
          (cReq, _cData) <- getShortLinkConnReq nm user address
          lift (withAgent' $ \a -> connRequestPQSupport a PQSupportOff cReq) >>= \case
            Nothing -> throwChatError CEInvalidConnReq
            Just (agentV, _) -> do
              let chatV = agentToChatVersion agentV
              gVar <- asks random
              subMode <- chatReadVar subscriptionMode
              connId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq PQSupportOff
              (relayMember, conn, groupRelay) <- withFastStore $ \db -> do
                relayMember <- createRelayForOwner db vr gVar user gInfo relay
                groupRelay <- createGroupRelayRecord db gInfo relayMember relay
                conn <- createRelayConnection db vr user (groupMemberId' relayMember) connId ConnPrepared chatV subMode
                pure (relayMember, conn, groupRelay)
              let GroupMember {memberRole = userRole, memberId = userMemberId} = membership
                  allowSimplexLinks = groupFeatureUserAllowed SGFSimplexLinks gInfo
                  membershipProfile = redactedMemberProfile allowSimplexLinks $ fromLocalProfile $ memberProfile membership
                  GroupMember {memberRole = relayRole, memberId = relayMemberId} = relayMember
                  relayInv = GroupRelayInvitation {
                    fromMember = MemberIdRole userMemberId userRole,
                    fromMemberProfile = membershipProfile,
                    invitedMember = MemberIdRole relayMemberId relayRole,
                    groupLink = groupSLink
                  }
              dm <- encodeConnInfo $ XGrpRelayInv relayInv
              (sqSecured, _serviceId) <- withAgent $ \a -> joinConnection a nm (aUserId user) (aConnId conn) True cReq dm PQSupportOff subMode
              let newConnStatus = if sqSecured then ConnSndReady else ConnJoined
              withFastStore' $ \db -> do
                void $ updateConnectionStatusFromTo db conn ConnPrepared newConnStatus
                updateRelayStatusFromTo db groupRelay RSNew RSInvited
    privateGetUser :: UserId -> CM User
    privateGetUser userId =
      tryAllErrors (withStore (`getUser` userId)) >>= \case
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
        `catchAllErrors` \case
          e@(ChatErrorAgent NO_USER _ _) -> eToView e
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
        _ -> throwCmdError "not supported"
      processChatCommand vr nm $ APISetChatSettings (ChatRef cType chatId Nothing) $ updateSettings chatSettings
    connectPlan :: User -> AConnectionLink -> CM (ACreatedConnLink, ConnectionPlan)
    connectPlan user (ACL SCMInvitation cLink) = case cLink of
      CLFull cReq -> invitationReqAndPlan cReq Nothing Nothing
      CLShort l -> do
        let l' = serverShortLink l
        knownLinkPlans l' >>= \case
          Just r -> pure r
          Nothing -> do
            (cReq, cData) <- getShortLinkConnReq nm user l'
            contactSLinkData_ <- liftIO $ decodeLinkUserData cData
            invitationReqAndPlan cReq (Just l') contactSLinkData_
      where
        knownLinkPlans l' = withFastStore $ \db -> do
          let inv cReq = ACCL SCMInvitation $ CCLink cReq (Just l')
          liftIO (getConnectionEntityViaShortLink db vr user l') >>= \case
            Just (cReq, ent) -> pure $ Just (inv cReq, invitationEntityPlan Nothing ent)
            -- deleted contact is returned as known, as invitation link cannot be re-used too connect anyway
            Nothing -> bimap inv (CPInvitationLink . ILPKnown) <$$> getContactViaShortLinkToConnect db vr user l'
        invitationReqAndPlan cReq sLnk_ contactSLinkData_ = do
          plan <- invitationRequestPlan user cReq contactSLinkData_ `catchAllErrors` (pure . CPError)
          pure (ACCL SCMInvitation (CCLink cReq sLnk_), plan)
    connectPlan user (ACL SCMContact cLink) = case cLink of
      CLFull cReq -> do
        plan <- contactOrGroupRequestPlan user cReq `catchAllErrors` (pure . CPError)
        pure (ACCL SCMContact $ CCLink cReq Nothing, plan)
      CLShort l@(CSLContact _ ct _ _) -> do
        let l' = serverShortLink l
            con cReq = ACCL SCMContact $ CCLink cReq (Just l')
            gPlan (cReq, g) = if memberRemoved (membership g) then Nothing else Just (con cReq, CPGroupLink (GLPKnown g))
        case ct of
          CCTContact ->
            knownLinkPlans >>= \case
              Just r -> pure r
              Nothing -> do
                (cReq, cData) <- getShortLinkConnReq nm user l'
                withFastStore' (\db -> getContactWithoutConnViaShortAddress db vr user l') >>= \case
                  Just ct' | not (contactDeleted ct') -> pure (con cReq, CPContactAddress (CAPContactViaAddress ct'))
                  _ -> do
                    contactSLinkData_ <- liftIO $ decodeLinkUserData cData
                    plan <- contactRequestPlan user cReq contactSLinkData_
                    pure (con cReq, plan)
            where
              knownLinkPlans = withFastStore $ \db ->
                liftIO (getUserContactLinkViaShortLink db user l') >>= \case
                  Just UserContactLink {connLinkContact = CCLink cReq _} -> pure $ Just (con cReq, CPContactAddress CAPOwnLink)
                  Nothing ->
                    getContactViaShortLinkToConnect db vr user l' >>= \case
                      Just (cReq, ct') -> pure $ if contactDeleted ct' then Nothing else Just (con cReq, CPContactAddress (CAPKnown ct'))
                      Nothing -> (gPlan =<<) <$> getGroupViaShortLinkToConnect db vr user l'
          CCTGroup ->
            knownLinkPlans >>= \case
              Just r -> pure r
              Nothing -> do
                (cReq, cData@(ContactLinkData _ UserContactData {direct})) <- getShortLinkConnReq nm user l'
                groupSLinkData_ <- liftIO $ decodeLinkUserData cData
                plan <- groupJoinRequestPlan user cReq direct groupSLinkData_
                pure (con cReq, plan)
            where
              knownLinkPlans = withFastStore $ \db ->
                liftIO (getGroupInfoViaUserShortLink db vr user l') >>= \case
                  Just (cReq, g) -> pure $ Just (con cReq, CPGroupLink (GLPOwnLink g))
                  Nothing -> (gPlan =<<) <$> getGroupViaShortLinkToConnect db vr user l'
          CCTChannel -> throwCmdError "channel links are not supported in this version"
          CCTRelay -> throwCmdError "chat relay links are not supported in this version"
    connectWithPlan :: User -> IncognitoEnabled -> ACreatedConnLink -> ConnectionPlan -> CM ChatResponse
    connectWithPlan user@User {userId} incognito ccLink plan
      | connectionPlanProceed plan = do
          case plan of CPError e -> eToView e; _ -> pure ()
          case plan of
            CPContactAddress (CAPContactViaAddress Contact {contactId}) ->
              processChatCommand vr nm $ APIConnectContactViaAddress userId incognito contactId
            _ -> processChatCommand vr nm $ APIConnect userId incognito $ Just ccLink
      | otherwise = pure $ CRConnectionPlan user ccLink plan
    invitationRequestPlan :: User -> ConnReqInvitation -> Maybe ContactShortLinkData -> CM ConnectionPlan
    invitationRequestPlan user cReq contactSLinkData_ = do
      maybe (CPInvitationLink (ILPOk contactSLinkData_)) (invitationEntityPlan contactSLinkData_)
        <$> withFastStore' (\db -> getConnectionEntityByConnReq db vr user $ invCReqSchemas cReq)
      where
        invCReqSchemas :: ConnReqInvitation -> (ConnReqInvitation, ConnReqInvitation)
        invCReqSchemas (CRInvitationUri crData e2e) =
          ( CRInvitationUri crData {crScheme = SSSimplex} e2e,
            CRInvitationUri crData {crScheme = simplexChat} e2e
          )
    invitationEntityPlan :: Maybe ContactShortLinkData -> ConnectionEntity -> ConnectionPlan
    invitationEntityPlan contactSLinkData_ = \case
      RcvDirectMsgConnection Connection {connStatus, contactConnInitiated} ct_ -> case ct_ of
        Just ct
          | contactActive ct -> CPInvitationLink (ILPKnown ct)
          | otherwise -> CPInvitationLink (ILPOk contactSLinkData_)
        Nothing
          | connStatus == ConnNew && contactConnInitiated -> CPInvitationLink ILPOwnLink
          | connStatus == ConnPrepared -> CPInvitationLink (ILPOk contactSLinkData_)
          | otherwise -> CPInvitationLink (ILPConnecting Nothing)
      _ -> CPError $ ChatError $ CECommandError "found connection entity is not RcvDirectMsgConnection"
    contactOrGroupRequestPlan ::  User -> ConnReqContact -> CM ConnectionPlan
    contactOrGroupRequestPlan user cReq@(CRContactUri crData) = do
      let ConnReqUriData {crClientData} = crData
          groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
      case groupLinkId of
        Nothing -> contactRequestPlan user cReq Nothing
        Just _ -> groupJoinRequestPlan user cReq True Nothing
    contactRequestPlan :: User -> ConnReqContact -> Maybe ContactShortLinkData -> CM ConnectionPlan
    contactRequestPlan user (CRContactUri crData) contactSLinkData_ = do
      let cReqSchemas = contactCReqSchemas crData
          cReqHashes = bimap contactCReqHash contactCReqHash cReqSchemas
      withFastStore' (\db -> getUserContactLinkByConnReq db user cReqSchemas) >>= \case
        Just _ -> pure $ CPContactAddress CAPOwnLink
        Nothing ->
          withFastStore' (\db -> getContactConnEntityByConnReqHash db vr user cReqHashes) >>= \case
            Nothing ->
              withFastStore' (\db -> getContactWithoutConnViaAddress db vr user cReqSchemas) >>= \case
                Just ct | not (contactDeleted ct) -> pure $ CPContactAddress (CAPContactViaAddress ct)
                _ -> pure $ CPContactAddress (CAPOk contactSLinkData_)
            Just (RcvDirectMsgConnection Connection {connStatus} Nothing)
              | connStatus == ConnPrepared -> pure $ CPContactAddress (CAPOk contactSLinkData_)
              | otherwise -> pure $ CPContactAddress CAPConnectingConfirmReconnect
            Just (RcvDirectMsgConnection _ (Just ct))
              | not (contactReady ct) && contactActive ct -> pure $ CPContactAddress (CAPConnectingProhibit ct)
              | contactDeleted ct -> pure $ CPContactAddress (CAPOk contactSLinkData_)
              | otherwise -> pure $ CPContactAddress (CAPKnown ct)
            -- TODO [short links] RcvGroupMsgConnection branch is deprecated? (old group link protocol?)
            Just (RcvGroupMsgConnection _ gInfo _) -> groupPlan gInfo True Nothing
            Just _ -> throwCmdError "found connection entity is not RcvDirectMsgConnection or RcvGroupMsgConnection"
    groupJoinRequestPlan :: User -> ConnReqContact -> Bool -> Maybe GroupShortLinkData -> CM ConnectionPlan
    groupJoinRequestPlan user (CRContactUri crData) direct groupSLinkData_ = do
      let cReqSchemas = contactCReqSchemas crData
          cReqHashes = bimap contactCReqHash contactCReqHash cReqSchemas
      withFastStore' (\db -> getGroupInfoByUserContactLinkConnReq db vr user cReqSchemas) >>= \case
        Just g -> pure $ CPGroupLink (GLPOwnLink g)
        Nothing -> do
          connEnt_ <- withFastStore' $ \db -> getContactConnEntityByConnReqHash db vr user cReqHashes
          gInfo_ <- withFastStore' $ \db -> getGroupInfoByGroupLinkHash db vr user cReqHashes
          case (gInfo_, connEnt_) of
            (Nothing, Nothing) -> pure $ CPGroupLink (GLPOk direct groupSLinkData_)
            -- TODO [short links] RcvDirectMsgConnection branches are deprecated? (old group link protocol?)
            (Nothing, Just (RcvDirectMsgConnection _conn Nothing)) -> pure $ CPGroupLink GLPConnectingConfirmReconnect
            (Nothing, Just (RcvDirectMsgConnection _ (Just ct)))
              | not (contactReady ct) && contactActive ct -> pure $ CPGroupLink (GLPConnectingProhibit gInfo_)
              | otherwise -> pure $ CPGroupLink (GLPOk direct groupSLinkData_)
            (Nothing, Just _) -> throwCmdError "found connection entity is not RcvDirectMsgConnection"
            (Just gInfo, _) -> groupPlan gInfo direct groupSLinkData_
    groupPlan :: GroupInfo -> Bool -> Maybe GroupShortLinkData -> CM ConnectionPlan
    groupPlan gInfo@GroupInfo {membership} direct groupSLinkData_
      | memberStatus membership == GSMemRejected = pure $ CPGroupLink (GLPKnown gInfo)
      | not (memberActive membership) && not (memberRemoved membership) =
          pure $ CPGroupLink (GLPConnectingProhibit $ Just gInfo)
      | memberActive membership = pure $ CPGroupLink (GLPKnown gInfo)
      | otherwise = pure $ CPGroupLink (GLPOk direct groupSLinkData_)
    contactCReqSchemas :: ConnReqUriData -> (ConnReqContact, ConnReqContact)
    contactCReqSchemas crData =
      ( CRContactUri crData {crScheme = SSSimplex},
        CRContactUri crData {crScheme = simplexChat}
      )
    contactCReqHash :: ConnReqContact -> ConnReqUriHash
    contactCReqHash = ConnReqUriHash . C.sha256Hash . strEncode

    -- This function is needed, as UI uses simplex:/ schema in message view, so that the links can be handled without browser,
    -- and short links are stored with server hostname schema, so they wouldn't match without it.
    serverShortLink :: ConnShortLink m -> ConnShortLink m
    serverShortLink = \case
      CSLInvitation _ srv lnkId linkKey -> CSLInvitation SLSServer srv lnkId linkKey
      CSLContact _ ct srv linkKey -> CSLContact SLSServer ct srv linkKey
    contactShortLinkData :: Profile -> Maybe AddressSettings -> UserLinkData
    contactShortLinkData p settings =
      let msg = autoReply =<< settings
          business = maybe False businessAddress settings
          contactData = ContactShortLinkData p msg business
       in encodeShortLinkData contactData
    updatePCCShortLinkData :: PendingContactConnection -> Profile -> CM (Maybe ShortLinkInvitation)
    updatePCCShortLinkData conn@PendingContactConnection {connLinkInv} profile =
      forM (connShortLink =<< connLinkInv) $ \_ -> do
        let userData = contactShortLinkData profile Nothing
            userLinkData = UserInvLinkData userData
        shortenShortLink' =<< withAgent (\a -> setConnShortLink a nm (aConnId' conn) SCMInvitation userLinkData Nothing)
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
                startProximateTimedItemThread user (ChatRef CTDirect contactId Nothing, itemId)
        _ -> pure () -- prohibited
    assertAllowedContent :: MsgContent -> CM ()
    assertAllowedContent = \case
      MCReport {} -> throwCmdError "sending reports via this API is not supported"
      _ -> pure ()
    assertAllowedContent' :: ComposedMessage -> CM ()
    assertAllowedContent' ComposedMessage {msgContent} = assertAllowedContent msgContent
    assertNoMentions :: ComposedMessage -> CM ()
    assertNoMentions ComposedMessage {mentions}
      | null mentions = pure ()
      | otherwise = throwCmdError "mentions are not supported in this chat"
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
            throwCmdError $ "feature not allowed " <> T.unpack (chatFeatureNameText CFVoice)
        processComposedMessages :: Contact -> CM ChatResponse
        processComposedMessages ct = do
          (fInvs_, ciFiles_) <- L.unzip <$> setupSndFileTransfers
          timed_ <- sndContactCITimed live ct itemTTL
          (msgContainers, quotedItems_) <- L.unzip <$> prepareMsgs (L.zip cmrs fInvs_) timed_
          msgs_ <- sendDirectContactMessages user ct $ L.map XMsgNew msgContainers
          let itemsData = prepareSndItemsData (L.toList cmrs) (L.toList ciFiles_) (L.toList quotedItems_) msgs_
          when (length itemsData /= length cmrs) $ logError "sendContactContentMessages: cmrs and itemsData length mismatch"
          r@(_, cis) <- partitionEithers <$> saveSndChatItems user (CDDirectSnd ct) itemsData timed_ live
          processSendErrs r
          forM_ (timed_ >>= timedDeleteAt') $ \deleteAt ->
            forM_ cis $ \ci ->
              startProximateTimedItemThread user (ChatRef CTDirect contactId Nothing, chatItemId' ci) deleteAt
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
                  (Nothing, Nothing) -> pure (MCSimple (ExtMsgContent mc M.empty fInv_ (ttl' <$> timed_) (justTrue live) Nothing), Nothing)
                  (Nothing, Just _) -> pure (MCForward (ExtMsgContent mc M.empty fInv_ (ttl' <$> timed_) (justTrue live) Nothing), Nothing)
                  (Just qiId, Nothing) -> do
                    CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
                      getDirectChatItem db user contactId qiId
                    (origQmc, qd, sent) <- quoteData qci
                    let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Nothing}
                        qmc = quoteContent mc origQmc file
                        quotedItem = CIQuote {chatDir = qd, itemId = Just qiId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
                    pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc M.empty fInv_ (ttl' <$> timed_) (justTrue live) Nothing), Just quotedItem)
                  (Just _, Just _) -> throwError SEInvalidQuote
              where
                quoteData :: ChatItem c d -> ExceptT StoreError IO (MsgContent, CIQDirection 'CTDirect, Bool)
                quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} = throwError SEInvalidQuote
                quoteData ChatItem {content = CISndMsgContent qmc} = pure (qmc, CIQDirectSnd, True)
                quoteData ChatItem {content = CIRcvMsgContent qmc} = pure (qmc, CIQDirectRcv, False)
                quoteData _ = throwError SEInvalidQuote
    sendGroupContentMessages :: User -> GroupInfo -> Maybe GroupChatScope -> Bool -> Maybe Int -> NonEmpty ComposedMessageReq -> CM ChatResponse
    sendGroupContentMessages user gInfo scope live itemTTL cmrs = do
      assertMultiSendable live cmrs
      chatScopeInfo <- mapM (getChatScopeInfo vr user) scope
      recipients <- getGroupRecipients vr user gInfo chatScopeInfo modsCompatVersion
      sendGroupContentMessages_ user gInfo scope chatScopeInfo recipients live itemTTL cmrs
        where
          hasReport = any (\(ComposedMessage {msgContent}, _, _, _) -> isReport msgContent) cmrs
          modsCompatVersion = if hasReport then contentReportsVersion else groupKnockingVersion
    sendGroupContentMessages_ :: User -> GroupInfo -> Maybe GroupChatScope -> Maybe GroupChatScopeInfo -> [GroupMember] -> Bool -> Maybe Int -> NonEmpty ComposedMessageReq -> CM ChatResponse
    sendGroupContentMessages_ user gInfo@GroupInfo {groupId, membership} scope chatScopeInfo recipients live itemTTL cmrs = do
      forM_ allowedRole $ assertUserGroupRole gInfo
      assertGroupContentAllowed
      processComposedMessages
      where
        allowedRole :: Maybe GroupMemberRole
        allowedRole = case scope of
          Nothing -> Just GRAuthor
          Just (GCSMemberSupport Nothing)
            | memberPending membership -> Nothing
            | otherwise -> Just GRObserver
          Just (GCSMemberSupport (Just _gmId)) -> Just GRModerator
        assertGroupContentAllowed :: CM ()
        assertGroupContentAllowed =
          case findProhibited (L.toList cmrs) of
            Just f -> throwCmdError $ "feature not allowed " <> T.unpack (groupFeatureNameText f)
            Nothing -> pure ()
          where
            findProhibited :: [ComposedMessageReq] -> Maybe GroupFeature
            findProhibited =
              foldr'
                (\(ComposedMessage {fileSource, msgContent = mc}, _, (_, ft), _) acc -> prohibitedGroupContent gInfo membership chatScopeInfo mc ft fileSource True <|> acc)
                Nothing
        processComposedMessages ::  CM ChatResponse
        processComposedMessages = do
          -- TODO [channels fwd] single description for all recipients
          (fInvs_, ciFiles_) <- L.unzip <$> setupSndFileTransfers (length recipients)
          timed_ <- sndGroupCITimed live gInfo itemTTL
          (chatMsgEvents, quotedItems_) <- L.unzip <$> prepareMsgs (L.zip cmrs fInvs_) timed_
          (msgs_, gsr) <- sendGroupMessages user gInfo Nothing recipients chatMsgEvents
          let itemsData = prepareSndItemsData (L.toList cmrs) (L.toList ciFiles_) (L.toList quotedItems_) (L.toList msgs_)
          cis_ <- saveSndChatItems user (CDGroupSnd gInfo chatScopeInfo) itemsData timed_ live
          when (length cis_ /= length cmrs) $ logError "sendGroupContentMessages: cmrs and cis_ length mismatch"
          createMemberSndStatuses cis_ msgs_ gsr
          let r@(_, cis) = partitionEithers cis_
          processSendErrs r
          forM_ (timed_ >>= timedDeleteAt') $ \deleteAt ->
            forM_ cis $ \ci ->
              startProximateTimedItemThread user (ChatRef CTGroup groupId scope, chatItemId' ci) deleteAt
          pure $ CRNewChatItems user (map (AChatItem SCTGroup SMDSnd (GroupChat gInfo chatScopeInfo)) cis)
          where
            setupSndFileTransfers :: Int -> CM (NonEmpty (Maybe FileInvitation, Maybe (CIFile 'MDSnd)))
            setupSndFileTransfers n =
              forM cmrs $ \(ComposedMessage {fileSource = file_}, _, _, _) -> case file_ of
                Just file -> do
                  fileSize <- checkSndFile file
                  (fInv, ciFile) <- xftpSndFileTransfer user file fileSize n $ CGGroup gInfo recipients
                  pure (Just fInv, Just ciFile)
                Nothing -> pure (Nothing, Nothing)
            prepareMsgs :: NonEmpty (ComposedMessageReq, Maybe FileInvitation) -> Maybe CITimed -> CM (NonEmpty (ChatMsgEvent 'Json, Maybe (CIQuote 'CTGroup)))
            prepareMsgs cmsFileInvs timed_ = withFastStore $ \db ->
              forM cmsFileInvs $ \((ComposedMessage {quotedItemId, msgContent = mc}, itemForwarded, _, ciMentions), fInv_) ->
                let msgScope = toMsgScope gInfo <$> chatScopeInfo
                    mentions = M.map (\CIMention {memberId} -> MsgMention {memberId}) ciMentions
                 in prepareGroupMsg db user gInfo msgScope mc mentions quotedItemId itemForwarded fInv_ timed_ live
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
            throwCmdError "invalid multi send: live and more than one quote not supported"
    xftpSndFileTransfer :: User -> CryptoFile -> Integer -> Int -> ContactOrGroup -> CM (FileInvitation, CIFile 'MDSnd)
    xftpSndFileTransfer user file fileSize n contactOrGroup = do
      (fInv, ciFile, ft) <- xftpSndFileTransfer_ user file fileSize n $ Just contactOrGroup
      case contactOrGroup of
        CGContact Contact {activeConn} -> forM_ activeConn $ \conn ->
          withFastStore' $ \db -> createSndFTDescrXFTP db user Nothing conn ft dummyFileDescr
        CGGroup _ ms -> forM_ ms $ \m -> saveMemberFD m `catchAllErrors` eToView
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
            throwCmdError "createNoteFolderContentItems: quotes not supported"
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
      CRQueueInfo user msgInfo <$> withAgent (\a -> getConnectionQueueInfo a nm acId)
    withSendRef :: ChatRef -> (SendRef -> CM ChatResponse) -> CM ChatResponse
    withSendRef chatRef a = case chatRef of
      ChatRef CTDirect cId _ -> a $ SRDirect cId
      ChatRef CTGroup gId scope -> a $ SRGroup gId scope
      _ -> throwCmdError "not supported"
    getSharedMsgId :: CM SharedMsgId
    getSharedMsgId = do
      gVar <- asks random
      liftIO $ SharedMsgId <$> encodedRandomBytes gVar 12

data ConnectViaContactResult
  = CVRConnectedContact Contact
  | CVRSentInvitation Connection (Maybe Profile)

onlyProtocolServers :: UserProtocol p => SProtocolType p -> ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP], [UserChatRelay]) -> ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP], [UserChatRelay])
onlyProtocolServers p (operators, smpServers, xftpServers, _chatRelays) = case p of
  SPSMP -> (operators, smpServers, [], [])
  SPXFTP -> (operators, [], xftpServers, [])

-- disable preset and replace custom servers (groupByOperator always adds custom)
updatedServers :: forall p. UserProtocol p => SProtocolType p -> [AUserServer p] -> UserOperatorServers -> UpdatedUserOperatorServers
updatedServers p' srvs UserOperatorServers {operator, smpServers, xftpServers, chatRelays} = case p' of
  SPSMP -> u (updateSrvs smpServers, map (AUS SDBStored) xftpServers, map (AUCR SDBStored) chatRelays)
  SPXFTP -> u (map (AUS SDBStored) smpServers, updateSrvs xftpServers, map (AUCR SDBStored) chatRelays)
  where
    u = uncurry3 $ UpdatedUserOperatorServers operator
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f ~(a,b,c) = f a b c
    updateSrvs :: [UserServer p] -> [AUserServer p]
    updateSrvs pSrvs = map disableSrv pSrvs <> maybe srvs (const []) operator
    disableSrv srv@UserServer {preset} =
      AUS SDBStored $ if preset then srv {enabled = False} else srv {deleted = True}

onlyRelays :: ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP], [UserChatRelay]) -> ([Maybe ServerOperator], [UserServer 'PSMP], [UserServer 'PXFTP], [UserChatRelay])
onlyRelays (operators, _smpServers, _xftpServers, chatRelays) = (operators, [], [], chatRelays)

-- disable preset and replace custom chat relays (groupByOperator always adds custom)
updatedRelays :: [AUserChatRelay] -> UserOperatorServers -> UpdatedUserOperatorServers
updatedRelays relays UserOperatorServers {operator, smpServers, xftpServers, chatRelays} =
  UpdatedUserOperatorServers operator (map (AUS SDBStored) smpServers) (map (AUS SDBStored) xftpServers) (updateRelays chatRelays)
  where
    updateRelays :: [UserChatRelay] -> [AUserChatRelay]
    updateRelays pRelays = map disableRelay pRelays <> maybe relays (const []) operator
    disableRelay relay@UserChatRelay {preset} =
      AUCR SDBStored $ if preset then relay {enabled = False} else relay {deleted = True}

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
        flip catchAllErrors' (eToView') $ do
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
      eToView' $ ChatErrorAgent (CRITICAL True $ "Message reception stopped: " <> show e) (AgentConnId "") Nothing
      E.throwIO e
  where
    process :: (ACorrId, AEntityId, AEvt) -> CM' ()
    process (corrId, entId, AEvt e msg) = run $ case e of
      SAENone -> processAgentMessageNoConn msg
      SAEConn -> processAgentMessage corrId entId msg
      SAERcvFile -> processAgentMsgRcvFile corrId entId msg
      SAESndFile -> processAgentMsgSndFile corrId entId msg
      where
        run action = action `catchAllErrors'` (eToView')

type AgentSubResult = Map ConnId (Either AgentErrorType (Maybe ClientServiceId))

cleanupManager :: CM ()
cleanupManager = do
  interval <- asks (cleanupManagerInterval . config)
  runWithoutInitialDelay interval
  initialDelay <- asks (initialCleanupManagerDelay . config)
  liftIO $ threadDelay' initialDelay
  stepDelay <- asks (cleanupManagerStepDelay . config)
  forever $ do
    flip catchAllErrors eToView $ do
      lift waitChatStartedAndActivated
      users <- withStore' getUsers
      let (us, us') = partition activeUser users
      forM_ us $ cleanupUser interval stepDelay
      forM_ us' $ cleanupUser interval stepDelay
      cleanupMessages `catchAllErrors` eToView
      cleanupDeliveryTasks `catchAllErrors` eToView
      cleanupDeliveryJobs `catchAllErrors` eToView
      -- TODO possibly, also cleanup async commands
      cleanupProbes `catchAllErrors` eToView
    liftIO $ threadDelay' $ diffToMicroseconds interval
  where
    runWithoutInitialDelay cleanupInterval = flip catchAllErrors eToView $ do
      lift waitChatStartedAndActivated
      users <- withStore' getUsers
      let (us, us') = partition activeUser users
      forM_ us $ \u -> cleanupTimedItems cleanupInterval u `catchAllErrors` eToView
      forM_ us' $ \u -> cleanupTimedItems cleanupInterval u `catchAllErrors` eToView
    cleanupUser cleanupInterval stepDelay user = do
      cleanupTimedItems cleanupInterval user `catchAllErrors` eToView
      liftIO $ threadDelay' stepDelay
      -- TODO remove in future versions: legacy step - contacts are no longer marked as deleted
      cleanupDeletedContacts user `catchAllErrors` eToView
      liftIO $ threadDelay' stepDelay
      cleanupInProgressGroups user `catchAllErrors` eToView
      liftIO $ threadDelay' stepDelay
    cleanupTimedItems cleanupInterval user = do
      ts <- liftIO getCurrentTime
      let startTimedThreadCutoff = addUTCTime cleanupInterval ts
      timedItems <- withStore' $ \db -> getTimedItems db user startTimedThreadCutoff
      forM_ timedItems $ \(itemRef, deleteAt) -> startTimedItemThread user itemRef deleteAt `catchAllErrors` const (pure ())
    cleanupDeletedContacts user = do
      vr <- chatVersionRange
      contacts <- withStore' $ \db -> getDeletedContacts db vr user
      forM_ contacts $ \ct ->
        withStore (\db -> deleteContactWithoutGroups db user ct)
          `catchAllErrors` eToView
    cleanupInProgressGroups user = do
      vr <- chatVersionRange
      ts <- liftIO getCurrentTime
      -- older than 30 minutes to avoid deleting a newly created group
      let cutoffTs = addUTCTime (- 1800) ts
      inProgressGroups <- withStore' $ \db -> getInProgressGroups db vr user cutoffTs
      forM_ inProgressGroups $ \gInfo ->
        deleteInProgressGroup user gInfo `catchAllErrors` eToView
    cleanupMessages = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(30 * nominalDay)) ts
      withStore' (`deleteOldMessages` cutoffTs)
    cleanupDeliveryTasks = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(7 * nominalDay)) ts
      withStore' (`deleteDoneDeliveryTasks` cutoffTs)
    cleanupDeliveryJobs = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(7 * nominalDay)) ts
      withStore' (`deleteDoneDeliveryJobs` cutoffTs)
    cleanupProbes = do
      ts <- liftIO getCurrentTime
      let cutoffTs = addUTCTime (-(14 * nominalDay)) ts
      withStore' (`deleteOldProbes` cutoffTs)

deleteInProgressGroup :: User -> GroupInfo -> CM ()
deleteInProgressGroup user gInfo = do
  deleteGroupLinkIfExists user gInfo
  deleteGroupConnections user gInfo False
  withFastStore' $ \db -> deleteGroup db user gInfo

runRelayGroupLinkChecks :: User -> CM ()
runRelayGroupLinkChecks _user = do
  -- TODO [relays] relay: periodically check presence of relay link in group links of served groups
  -- TODO   - retrieve group link data
  -- TODO   - if relay link is present, update relay status to RSActive
  -- TODO   - if relay link is absent and status was RSActive -> update to new "Removed" status?
  pure ()

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
      process a `catchAllErrors` eToView
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
  tryAllErrors (withStore $ \db -> getContact db vr user ctId) >>= mapM_ process
  where
    process ct@Contact {chatItemTTL} =
      withExpirationDate globalTTL chatItemTTL $ \expirationDate -> do
        lift waitChatStartedAndActivated
        filesInfo <- withStore' $ \db -> getContactExpiredFileInfo db user ct expirationDate
        deleteCIFiles user filesInfo
        withStore' $ \db -> deleteContactExpiredCIs db user ct expirationDate

expireGroupChatItems :: User -> VersionRangeChat -> Int64 -> UTCTime -> GroupId -> CM ()
expireGroupChatItems user vr globalTTL createdAtCutoff groupId =
  tryAllErrors (withStore $ \db -> getGroupInfo db vr user groupId) >>= mapM_ process
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
      "/create user " *> (CreateActiveUser <$> newUserP False),
      "/create chat relay user " *> (CreateActiveUser <$> newUserP True),
      "/create bot " *> (CreateActiveUser <$> newBotUserP),
      "/users" $> ListUsers,
      "/_user " *> (APISetActiveUser <$> A.decimal <*> optional (A.space *> jsonP)),
      ("/user " <|> "/u ") *> (SetActiveUser <$> displayNameP <*> optional (A.space *> pwdP)),
      "/set receipts all " *> (SetAllContactReceipts <$> onOffP),
      "/_set receipts contacts " *> (APISetUserContactReceipts <$> A.decimal <* A.space <*> receiptSettings),
      "/set receipts contacts " *> (SetUserContactReceipts <$> receiptSettings),
      "/_set receipts groups " *> (APISetUserGroupReceipts <$> A.decimal <* A.space <*> receiptSettings),
      "/set receipts groups " *> (SetUserGroupReceipts <$> receiptSettings),
      "/_set accept member contacts " *> (APISetUserAutoAcceptMemberContacts <$> A.decimal <* A.space <*> onOffP),
      "/set accept member contacts " *> (SetUserAutoAcceptMemberContacts <$> onOffP),
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
      "/_start" $> StartChat {mainApp = True, enableSndFiles = True},
      "/_check running" $> CheckChatRunning,
      "/_stop" $> APIStopChat,
      "/_app activate restore=" *> (APIActivateChat <$> onOffP),
      "/_app activate" $> APIActivateChat True,
      "/_app suspend " *> (APISuspendChat <$> A.decimal),
      "/_connections diff" *> (ShowConnectionsDiff <$> (" show_ids=" *> onOffP <|> pure False)),
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
      "/_get chat " *> (APIGetChat <$> chatRefP <*> optional (" content=" *> strP) <* A.space <*> chatPaginationP <*> optional (" search=" *> textP)),
      "/_get content types " *> (APIGetChatContentTypes <$> chatRefP),
      "/_get items " *> (APIGetChatItems <$> chatPaginationP <*> optional (" search=" *> textP)),
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
      "/_delete member chat #" *> (APIDeleteMemberSupportChat <$> A.decimal <* A.space <*> A.decimal),
      "/_member role #" *> (APIMembersRole <$> A.decimal <*> _strP <*> memberRole),
      "/_block #" *> (APIBlockMembersForAll <$> A.decimal <*> _strP <* " blocked=" <*> onOffP),
      "/_remove #" *> (APIRemoveMembers <$> A.decimal <*> _strP <*> (" messages=" *> onOffP <|> pure False)),
      "/_leave #" *> (APILeaveGroup <$> A.decimal),
      "/_members #" *> (APIListMembers <$> A.decimal),
      -- "/_archive conversations #" *> (APIArchiveGroupConversations <$> A.decimal <*> _strP),
      -- "/_delete conversations #" *> (APIDeleteGroupConversations <$> A.decimal <*> _strP),
      "/_server test " *> (APITestProtoServer <$> A.decimal <* A.space <*> strP),
      "/smp test " *> (TestProtoServer . AProtoServerWithAuth SPSMP <$> strP),
      "/xftp test " *> (TestProtoServer . AProtoServerWithAuth SPXFTP <$> strP),
      "/ntf test " *> (TestProtoServer . AProtoServerWithAuth SPNTF <$> strP),
      "/smp " *> (SetUserProtoServers (AProtocolType SPSMP) . map (AProtoServerWithAuth SPSMP) <$> protocolServersP),
      "/xftp " *> (SetUserProtoServers (AProtocolType SPXFTP) . map (AProtoServerWithAuth SPXFTP) <$> protocolServersP),
      "/smp" $> GetUserProtoServers (AProtocolType SPSMP),
      "/xftp" $> GetUserProtoServers (AProtocolType SPXFTP),
      "/relays " *> (SetUserChatRelays <$> chatRelaysP),
      "/relays" $> GetUserChatRelays,
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
      ("/public group" <|> "/pg") *> (NewPublicGroup <$> incognitoP <* " relays=" <*> strP <* A.space <* char_ '#' <*> groupProfile),
      "/_public group " *> (APINewPublicGroup <$> A.decimal <*> incognitoOnOffP <*> _strP <* A.space <*> jsonP),
      ("/add " <|> "/a ") *> char_ '#' *> (AddMember <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <*> (memberRole <|> pure GRMember)),
      ("/join " <|> "/j ") *> char_ '#' *> (JoinGroup <$> displayNameP <*> (" mute" $> MFNone <|> pure MFAll)),
      "/accept member " *> char_ '#' *> (AcceptMember <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <*> (memberRole <|> pure GRMember)),
      ("/member role " <|> "/mr ") *> char_ '#' *> (MemberRole <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <*> memberRole),
      "/block for all #" *> (BlockForAll <$> displayNameP <* A.space <*> (char_ '@' *> displayNameP) <*> pure True),
      "/unblock for all #" *> (BlockForAll <$> displayNameP <* A.space <*> (char_ '@' *> displayNameP) <*> pure False),
      ("/remove " <|> "/rm ") *> char_ '#' *> (RemoveMembers <$> displayNameP <* A.space <*> (L.fromList <$> (char_ '@' *> displayNameP) `A.sepBy1'` A.char ',') <*> (" messages=" *> onOffP <|> pure False)),
      ("/leave " <|> "/l ") *> char_ '#' *> (LeaveGroup <$> displayNameP),
      ("/delete #" <|> "/d #") *> (DeleteGroup <$> displayNameP),
      ("/delete " <|> "/d ") *> char_ '@' *> (DeleteContact <$> displayNameP <*> chatDeleteMode),
      "/clear *" $> ClearNoteFolder,
      "/clear #" *> (ClearGroup <$> displayNameP),
      "/clear " *> char_ '@' *> (ClearContact <$> displayNameP),
      ("/members " <|> "/ms ") *> char_ '#' *> (ListMembers <$> displayNameP),
      "/member support chats #" *> (ListMemberSupportChats <$> displayNameP),
      "/_groups" *> (APIListGroups <$> A.decimal <*> optional (" @" *> A.decimal) <*> optional (A.space *> textP)),
      ("/groups" <|> "/gs") *> (ListGroups <$> optional (" @" *> displayNameP) <*> optional (A.space *> textP)),
      "/_group_profile #" *> (APIUpdateGroupProfile <$> A.decimal <* A.space <*> jsonP),
      ("/group_profile " <|> "/gp ") *> char_ '#' *> (UpdateGroupNames <$> displayNameP <* A.space <*> groupProfile),
      ("/group_profile " <|> "/gp ") *> char_ '#' *> (ShowGroupProfile <$> displayNameP),
      "/group_descr " *> char_ '#' *> (UpdateGroupDescription <$> displayNameP <*> optional (A.space *> msgTextP)),
      "/set welcome " *> char_ '#' *> (UpdateGroupDescription <$> displayNameP <* A.space <*> (Just <$> msgTextP)),
      "/delete welcome " *> char_ '#' *> (UpdateGroupDescription <$> displayNameP <*> pure Nothing),
      "/show welcome " *> char_ '#' *> (ShowGroupDescription <$> displayNameP),
      "/_create link #" *> (APICreateGroupLink <$> A.decimal <*> (memberRole <|> pure GRMember)),
      "/_set link role #" *> (APIGroupLinkMemberRole <$> A.decimal <*> memberRole),
      "/_delete link #" *> (APIDeleteGroupLink <$> A.decimal),
      "/_get link #" *> (APIGetGroupLink <$> A.decimal),
      "/_short link #" *> (APIAddGroupShortLink <$> A.decimal),
      "/create link #" *> (CreateGroupLink <$> displayNameP <*> (memberRole <|> pure GRMember)),
      "/set link role #" *> (GroupLinkMemberRole <$> displayNameP <*> memberRole),
      "/delete link #" *> (DeleteGroupLink <$> displayNameP),
      "/show link #" *> (ShowGroupLink <$> displayNameP),
      "/_create member contact #" *> (APICreateMemberContact <$> A.decimal <* A.space <*> A.decimal),
      "/_invite member contact @" *> (APISendMemberContactInvitation <$> A.decimal <*> optional (A.space *> msgContentP)),
      "/_accept member contact @" *> (APIAcceptMemberContact <$> A.decimal),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayNameP <* A.space <*> pure Nothing <*> quotedMsg <*> msgTextP),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayNameP <* A.space <* char_ '@' <*> (Just <$> displayNameP) <* A.space <*> quotedMsg <*> msgTextP),
      "/_contacts " *> (APIListContacts <$> A.decimal),
      "/contacts" $> ListContacts,
      "/_connect plan " *> (APIConnectPlan <$> A.decimal <* A.space <*> ((Just <$> strP) <|> A.takeTill (== ' ') $> Nothing)),
      "/_prepare contact " *> (APIPrepareContact <$> A.decimal <* A.space <*> connLinkP <* A.space <*> jsonP),
      "/_prepare group " *> (APIPrepareGroup <$> A.decimal <* A.space <*> connLinkP' <*> (" direct=" *> onOffP <|> pure True) <* A.space <*> jsonP),
      "/_set contact user @" *> (APIChangePreparedContactUser <$> A.decimal <* A.space <*> A.decimal),
      "/_set group user #" *> (APIChangePreparedGroupUser <$> A.decimal <* A.space <*> A.decimal),
      "/_connect contact @" *> (APIConnectPreparedContact <$> A.decimal <*> incognitoOnOffP <*> optional (A.space *> msgContentP)),
      "/_connect group #" *> (APIConnectPreparedGroup <$> A.decimal <*> incognitoOnOffP <*> optional (A.space *> msgContentP)),
      "/_connect " *> (APIAddContact <$> A.decimal <*> incognitoOnOffP),
      "/_connect " *> (APIConnect <$> A.decimal <*> incognitoOnOffP <* A.space <*> connLinkP_),
      "/_set incognito :" *> (APISetConnectionIncognito <$> A.decimal <* A.space <*> onOffP),
      "/_set conn user :" *> (APIChangeConnectionUser <$> A.decimal <* A.space <*> A.decimal),
      ("/connect" <|> "/c") *> (AddContact <$> incognitoP),
      ("/connect" <|> "/c") *> (Connect <$> incognitoP <* A.space <*> ((Just <$> strP) <|> A.takeTill isSpace $> Nothing)),
      ForwardMessage <$> chatNameP <* " <- @" <*> displayNameP <* A.space <*> msgTextP,
      ForwardGroupMessage <$> chatNameP <* " <- #" <*> displayNameP <* A.space <* A.char '@' <*> (Just <$> displayNameP) <* A.space <*> msgTextP,
      ForwardGroupMessage <$> chatNameP <* " <- #" <*> displayNameP <*> pure Nothing <* A.space <*> msgTextP,
      ForwardLocalMessage <$> chatNameP <* " <- * " <*> msgTextP,
      SendMessage <$> sendNameP <* A.space <*> msgTextP,
      "@#" *> (SendMemberContactMessage <$> displayNameP <* A.space <* char_ '@' <*> displayNameP <* A.space <*> msgTextP),
      "/accept_member_contact @" *> (AcceptMemberContact <$> displayNameP),
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
      ("/search" <|> "/?") *> (LastMessages <$> optional (A.space *> chatNameP) <*> msgCountP <*> (Just <$> (A.space *> textP))),
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
      "/_short_link_address " *> (APIAddMyAddressShortLink <$> A.decimal),
      "/_profile_address " *> (APISetProfileAddress <$> A.decimal <* A.space <*> onOffP),
      ("/profile_address " <|> "/pa ") *> (SetProfileAddress <$> onOffP),
      "/_address_settings " *> (APISetAddressSettings <$> A.decimal <* A.space <*> jsonP),
      "/auto_accept " *> (SetAddressSettings <$> autoAcceptP),
      ("/accept" <|> "/ac") *> (AcceptContact <$> incognitoP <* A.space <* char_ '@' <*> displayNameP),
      ("/reject " <|> "/rc ") *> char_ '@' *> (RejectContact <$> displayNameP),
      ("/markdown" <|> "/m") $> ChatHelp HSMarkdown,
      ("/welcome" <|> "/w") $> Welcome,
      "/set profile image " *> (UpdateProfileImage . Just . ImageData <$> imageP),
      "/delete profile image" $> UpdateProfileImage Nothing,
      "/show profile image" $> ShowProfileImage,
      ("/profile " <|> "/p ") *> (uncurry UpdateProfile <$> profileNameDescr),
      ("/profile" <|> "/p") $> ShowProfile,
      "/set bot commands " *> (SetBotCommands <$> botCommandsP),
      "/delete bot commands" $> SetBotCommands [],
      "/set voice #" *> (SetGroupFeatureRole (AGFR SGFVoice) <$> displayNameP <*> _strP <*> optional memberRole),
      "/set voice @" *> (SetContactFeature (ACF SCFVoice) <$> displayNameP <*> optional (A.space *> strP)),
      "/set voice " *> (SetUserFeature (ACF SCFVoice) <$> strP),
      "/set files #" *> (SetGroupFeatureRole (AGFR SGFFiles) <$> displayNameP <*> _strP <*> optional memberRole),
      "/set files @" *> (SetContactFeature (ACF SCFFiles) <$> displayNameP <*> optional (A.space *> strP)),
      "/set files " *> (SetUserFeature (ACF SCFFiles) <$> strP),
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
      "/set admission review #" *> (SetGroupMemberAdmissionReview <$> displayNameP <*> (A.space *> memberCriteriaP)),
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
      (ACR m cReq) <- strP
      sLink_ <- optional (A.space *> strP)
      pure $ ACCL m (CCLink cReq sLink_)
    connLinkP' = do
      cReq <- strP
      sLink_ <- optional (A.space *> strP)
      pure $ CCLink cReq sLink_
    connLinkP_ =
      ((Just <$> connLinkP) <|> A.takeTill (== ' ') $> Nothing)
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
    profileNameDescr = (,) <$> displayNameP <*> shortDescrP
    -- 'Help with bot':'link <ID>','Menu of commands':[...]
    botCommandsP :: Parser [ChatBotCommand]
    botCommandsP = commandP `A.sepBy'` A.char ','
      where
        commandP = do
          label <- safeDecodeUtf8 <$> ((quoted <|> A.takeTill (== ':')) <* A.char ':')
          when (T.null label) $ fail "empty command label"
          A.peekChar' >>= \case
            '{' -> A.char '{' *> (CBCMenu label <$> botCommandsP) <* A.char '}'
            _ -> do
              cmd <- safeDecodeUtf8 <$> (optional (A.char '/') *> (quoted <|> A.takeTill (A.inClass ":,}")))
              (keyword, params) <- case T.words cmd of
                [] -> fail "empty command"
                k : ws -> pure (k, if null ws then Nothing else Just $ T.unwords ws)
              pure CBCCommand {label, keyword, params}
        quoted = A.char '\'' *> A.takeTill (== '\'') <* A.char '\''
    newUserP userChatRelay = do
      (cName, shortDescr) <- profileNameDescr
      let profile = Just Profile {displayName = cName, fullName = "", shortDescr, image = Nothing, contactLink = Nothing, peerType = Nothing, preferences = Nothing}
      pure NewUser {profile, pastTimestamp = False, userChatRelay}
    newBotUserP = do
      files_ <- optional $ "files=" *> onOffP <* A.space
      (cName, shortDescr) <- profileNameDescr
      let preferences = case files_ of
            Just True -> Nothing
            _ -> Just (emptyChatPrefs :: Preferences) {files = Just FilesPreference {allow = FANo}}
          profile = Just Profile {displayName = cName, fullName = "", shortDescr, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences}
      pure NewUser {profile, pastTimestamp = False, userChatRelay = False}
    jsonP :: J.FromJSON a => Parser a
    jsonP = J.eitherDecodeStrict' <$?> A.takeByteString
    groupProfile = do
      (gName, shortDescr) <- profileNameDescr
      let groupPreferences =
            Just
              (emptyGroupPrefs :: GroupPreferences)
                { directMessages = Just DirectMessagesGroupPreference {enable = FEOn, role = Nothing},
                  history = Just HistoryGroupPreference {enable = FEOn}
                }
      pure GroupProfile {displayName = gName, fullName = "", shortDescr, description = Nothing, image = Nothing, groupLink = Nothing, groupPreferences, memberAdmission = Nothing}
    memberCriteriaP = ("all" $> Just MCAll) <|> ("off" $> Nothing)
    shortDescrP = do
      descr <- A.takeWhile1 isSpace *> (T.dropWhileEnd isSpace <$> textP) <|> pure ""
      pure $ if T.null descr then Nothing else Just $ T.take 160 descr
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
    chatRefP = do
      chatTypeP >>= \case
        CTGroup -> ChatRef CTGroup <$> A.decimal <*> optional gcScopeP
        cType -> (\chatId -> ChatRef cType chatId Nothing) <$> A.decimal
    sendRefP =
      (A.char '@' $> SRDirect <*> A.decimal)
        <|> (A.char '#' $> SRGroup <*> A.decimal <*> optional gcScopeP)
    gcScopeP = "(_support" *> (GCSMemberSupport <$> optional (A.char ':' *> A.decimal)) <* A.char ')'
    sendNameP =
      (A.char '@' $> SNDirect <*> displayNameP)
        <|> (A.char '#' $> SNGroup <*> displayNameP <*> gScopeNameP)
        <|> ("/*" $> SNLocal)
    gScopeNameP =
      (supportPfx *> (Just . GSNMemberSupport <$> optional supportMember) <* A.char ')')
        -- this branch fails on "(support" followed by incorrect syntax,
        -- to avoid sending message to the whole group as `optional gScopeNameP` would do
        <|> (optional supportPfx >>= mapM (\_ -> fail "bad chat scope"))
      where
        supportPfx = A.takeWhile isSpace *> "(support"
        supportMember = safeDecodeUtf8 <$> (A.char ':' *> A.takeWhile isSpace *> (A.take . lengthTillLastParen =<< A.lookAhead displayNameP_))
        lengthTillLastParen s = case B.unsnoc s of
          Just (_, ')') -> B.length s - 1
          _ -> B.length s
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
    chatRelaysP = chatRelayP `A.sepBy1` A.char ' '
    chatRelayP = do
      name <- "name=" *> text1P
      address <- _strP
      pure CLINewRelay {name, address}
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
    -- TODO [short links] parser for address settings
    autoAcceptP = ifM onOffP (businessAA <|> addressAA) (pure $ AddressSettings False Nothing Nothing)
      where
        addressAA = AddressSettings False <$> (Just . AutoAccept <$> (" incognito=" *> onOffP <|> pure False)) <*> autoReply
        businessAA = " business" *> (AddressSettings True (Just $ AutoAccept False) <$> autoReply)
        autoReply = optional (A.space *> msgContentP)
    rcCtrlAddressP = RCCtrlAddress <$> ("addr=" *> strP) <*> (" iface=" *> (jsonP <|> text1P))
    text1P = safeDecodeUtf8 <$> A.takeTill (== ' ')
    char_ = optional . A.char

displayNameP :: Parser Text
displayNameP = safeDecodeUtf8 <$> displayNameP_
{-# INLINE displayNameP #-}

displayNameP_ :: Parser ByteString
displayNameP_ = quoted '\'' <|> takeNameTill (\c -> isSpace c || c == ',')
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
