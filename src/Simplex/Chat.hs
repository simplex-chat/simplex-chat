{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat where

import Control.Applicative (optional, (<|>))
import Control.Concurrent.STM (retry, stateTVar)
import Control.Logger.Simple
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
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Fixed (div')
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (find, isSuffixOf, sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, addUTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.System (SystemTime, systemToUTCTime)
import Data.Time.LocalTime (getCurrentTimeZone, getZonedTime)
import qualified Database.SQLite.Simple as DB
import Simplex.Chat.Archive
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Options
import Simplex.Chat.ProfileGenerator (generateRandomProfile)
import Simplex.Chat.Protocol
import Simplex.Chat.Store
import Simplex.Chat.Types
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..), AgentDatabase (..), InitialAgentServers (..), createAgentStore, defaultAgentConfig)
import Simplex.Messaging.Agent.Lock
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (dbNew), execSQL)
import Simplex.Messaging.Client (defaultNetworkConfig)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (base64P)
import Simplex.Messaging.Protocol (ErrorType (..), MsgBody, MsgFlags (..), NtfServer)
import qualified Simplex.Messaging.Protocol as SMP
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (defaultSocksProxy)
import Simplex.Messaging.Util
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (combine, splitExtensions, takeFileName)
import System.IO (Handle, IOMode (..), SeekMode (..), hFlush, openFile, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Directory
import qualified UnliftIO.Exception as E
import UnliftIO.IO (hClose, hSeek, hTell)
import UnliftIO.STM

defaultChatConfig :: ChatConfig
defaultChatConfig =
  ChatConfig
    { agentConfig =
        defaultAgentConfig
          { tcpPort = undefined, -- agent does not listen to TCP
            database = AgentDBFile {dbFile = "simplex_v1_agent", dbKey = ""},
            yesToMigrations = False
          },
      yesToMigrations = False,
      defaultServers =
        InitialAgentServers
          { smp = _defaultSMPServers,
            ntf = _defaultNtfServers,
            netCfg = defaultNetworkConfig
          },
      tbqSize = 64,
      fileChunkSize = 15780, -- do not change
      inlineFiles = defaultInlineFilesConfig,
      subscriptionConcurrency = 16,
      subscriptionEvents = False,
      hostEvents = False,
      testView = False
    }

_defaultSMPServers :: NonEmpty SMPServerWithAuth
_defaultSMPServers =
  L.fromList
    [ "smp://0YuTwO05YJWS8rkjn9eLJDjQhFKvIYd8d4xG8X1blIU=@smp8.simplex.im,beccx4yfxxbvyhqypaavemqurytl6hozr47wfc7uuecacjqdvwpw2xid.onion",
      "smp://SkIkI6EPd2D63F4xFKfHk7I1UGZVNn6k1QWZ5rcyr6w=@smp9.simplex.im,jssqzccmrcws6bhmn77vgmhfjmhwlyr3u7puw4erkyoosywgl67slqqd.onion",
      "smp://6iIcWT_dF2zN_w5xzZEY7HI2Prbh3ldP07YTyDexPjE=@smp10.simplex.im,rb2pbttocvnbrngnwziclp2f4ckjq65kebafws6g4hy22cdaiv5dwjqd.onion"
    ]

_defaultNtfServers :: [NtfServer]
_defaultNtfServers = ["ntf://FB-Uop7RTaZZEG0ZLD2CIaTjsPh-Fw0zFAnb7QyA8Ks=@ntf2.simplex.im,ntg7jdjy2i3qbib3sykiho3enekwiaqg3icctliqhtqcg6jmoh6cxiad.onion"]

maxImageSize :: Integer
maxImageSize = 236700

fixedImagePreview :: ImageData
fixedImagePreview = ImageData "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAKVJREFUeF7t1kENACEUQ0FQhnVQ9lfGO+xggITQdvbMzArPey+8fa3tAfwAEdABZQspQStgBssEcgAIkSAJkiAJljtEgiRIgmUCSZAESZAESZAEyx0iQRIkwTKBJEiCv5fgvTd1wDmn7QAP4AeIgA4oW0gJWgEzWCZwbQ7gAA7ggLKFOIADOKBMIAeAEAmSIAmSYLlDJEiCJFgmkARJkARJ8N8S/ADTZUewBvnTOQAAAABJRU5ErkJggg=="

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

createChatDatabase :: FilePath -> String -> Bool -> IO ChatDatabase
createChatDatabase filePrefix key yesToMigrations = do
  chatStore <- createChatStore (chatStoreFile filePrefix) key yesToMigrations
  agentStore <- createAgentStore (agentStoreFile filePrefix) key yesToMigrations
  pure ChatDatabase {chatStore, agentStore}

newChatController :: ChatDatabase -> Maybe User -> ChatConfig -> ChatOpts -> Maybe (Notification -> IO ()) -> IO ChatController
newChatController ChatDatabase {chatStore, agentStore} user cfg@ChatConfig {agentConfig = aCfg, tbqSize, defaultServers, inlineFiles} ChatOpts {smpServers, networkConfig, logConnections, logServerHosts, allowInstantFiles} sendToast = do
  let inlineFiles' = if allowInstantFiles then inlineFiles else inlineFiles {sendChunks = 0, receiveInstant = False}
      config = cfg {subscriptionEvents = logConnections, hostEvents = logServerHosts, defaultServers = configServers, inlineFiles = inlineFiles'}
      sendNotification = fromMaybe (const $ pure ()) sendToast
      firstTime = dbNew chatStore
  activeTo <- newTVarIO ActiveNone
  currentUser <- newTVarIO user
  smpAgent <- getSMPAgentClient aCfg {database = AgentDB agentStore} =<< agentServers config
  agentAsync <- newTVarIO Nothing
  idsDrg <- newTVarIO =<< drgNew
  inputQ <- newTBQueueIO tbqSize
  outputQ <- newTBQueueIO tbqSize
  notifyQ <- newTBQueueIO tbqSize
  chatLock <- newEmptyTMVarIO
  sndFiles <- newTVarIO M.empty
  rcvFiles <- newTVarIO M.empty
  currentCalls <- atomically TM.empty
  filesFolder <- newTVarIO Nothing
  incognitoMode <- newTVarIO False
  chatStoreChanged <- newTVarIO False
  expireCIsAsync <- newTVarIO Nothing
  expireCIs <- newTVarIO False
  pure ChatController {activeTo, firstTime, currentUser, smpAgent, agentAsync, chatStore, chatStoreChanged, idsDrg, inputQ, outputQ, notifyQ, chatLock, sndFiles, rcvFiles, currentCalls, config, sendNotification, incognitoMode, filesFolder, expireCIsAsync, expireCIs}
  where
    configServers :: InitialAgentServers
    configServers =
      let smp' = fromMaybe (smp defaultServers) (nonEmpty smpServers)
       in defaultServers {smp = smp', netCfg = networkConfig}
    agentServers :: ChatConfig -> IO InitialAgentServers
    agentServers config@ChatConfig {defaultServers = ss@InitialAgentServers {smp}} = do
      smp' <- maybe (pure smp) userServers user
      pure ss {smp = smp'}
      where
        userServers user' = activeAgentServers config <$> withTransaction chatStore (`getSMPServers` user')

activeAgentServers :: ChatConfig -> [ServerCfg] -> NonEmpty SMPServerWithAuth
activeAgentServers ChatConfig {defaultServers = InitialAgentServers {smp}} =
  fromMaybe smp
    . nonEmpty
    . map (\ServerCfg {server} -> server)
    . filter (\ServerCfg {enabled} -> enabled)

startChatController :: (MonadUnliftIO m, MonadReader ChatController m) => User -> Bool -> Bool -> m (Async ())
startChatController user subConns enableExpireCIs = do
  asks smpAgent >>= resumeAgentClient
  restoreCalls user
  s <- asks agentAsync
  readTVarIO s >>= maybe (start s) (pure . fst)
  where
    start s = do
      a1 <- async $ race_ notificationSubscriber agentSubscriber
      a2 <-
        if subConns
          then Just <$> async (void . runExceptT $ subscribeUserConnections Agent.subscribeConnections user)
          else pure Nothing
      atomically . writeTVar s $ Just (a1, a2)
      when enableExpireCIs startExpireCIs
      pure a1
    startExpireCIs = do
      expireAsync <- asks expireCIsAsync
      readTVarIO expireAsync >>= \case
        Nothing -> do
          a <- Just <$> async (void $ runExceptT runExpireCIs)
          atomically $ writeTVar expireAsync a
          setExpireCIs True
        _ -> setExpireCIs True
    runExpireCIs = forever $ do
      flip catchError (toView . CRChatError) $ do
        expire <- asks expireCIs
        atomically $ readTVar expire >>= \b -> unless b retry
        ttl <- withStore' (`getChatItemTTL` user)
        forM_ ttl $ \t -> expireChatItems user t False
      threadDelay $ 1800 * 1000000 -- 30 minutes

restoreCalls :: (MonadUnliftIO m, MonadReader ChatController m) => User -> m ()
restoreCalls user = do
  savedCalls <- fromRight [] <$> runExceptT (withStore' $ \db -> getCalls db user)
  let callsMap = M.fromList $ map (\call@Call {contactId} -> (contactId, call)) savedCalls
  calls <- asks currentCalls
  atomically $ writeTVar calls callsMap

stopChatController :: MonadUnliftIO m => ChatController -> m ()
stopChatController ChatController {smpAgent, agentAsync = s, expireCIs} = do
  disconnectAgentClient smpAgent
  readTVarIO s >>= mapM_ (\(a1, a2) -> uninterruptibleCancel a1 >> mapM_ uninterruptibleCancel a2)
  atomically $ do
    writeTVar expireCIs False
    writeTVar s Nothing

execChatCommand :: (MonadUnliftIO m, MonadReader ChatController m) => ByteString -> m ChatResponse
execChatCommand s = case parseChatCommand s of
  Left e -> pure $ chatCmdError e
  Right cmd -> either CRChatCmdError id <$> runExceptT (processChatCommand cmd)

parseChatCommand :: ByteString -> Either String ChatCommand
parseChatCommand = A.parseOnly chatCommandP . B.dropWhileEnd isSpace

toView :: ChatMonad m => ChatResponse -> m ()
toView event = do
  q <- asks outputQ
  atomically $ writeTBQueue q (Nothing, event)

processChatCommand :: forall m. ChatMonad m => ChatCommand -> m ChatResponse
processChatCommand = \case
  ShowActiveUser -> withUser' $ pure . CRActiveUser
  CreateActiveUser p -> do
    u <- asks currentUser
    whenM (isJust <$> readTVarIO u) $ throwChatError CEActiveUserExists
    user <- withStore $ \db -> createUser db p True
    atomically . writeTVar u $ Just user
    pure $ CRActiveUser user
  StartChat subConns enableExpireCIs -> withUser' $ \user ->
    asks agentAsync >>= readTVarIO >>= \case
      Just _ -> pure CRChatRunning
      _ -> checkStoreNotChanged $ startChatController user subConns enableExpireCIs $> CRChatStarted
  APIStopChat -> do
    ask >>= stopChatController
    pure CRChatStopped
  APIActivateChat -> do
    withUser $ \user -> restoreCalls user
    withAgent activateAgent
    setExpireCIs True
    pure CRCmdOk
  APISuspendChat t -> do
    setExpireCIs False
    withAgent (`suspendAgent` t)
    pure CRCmdOk
  ResubscribeAllConnections -> withUser (subscribeUserConnections Agent.resubscribeConnections) $> CRCmdOk
  SetFilesFolder filesFolder' -> do
    createDirectoryIfMissing True filesFolder'
    ff <- asks filesFolder
    atomically . writeTVar ff $ Just filesFolder'
    pure CRCmdOk
  SetIncognito onOff -> do
    incognito <- asks incognitoMode
    atomically . writeTVar incognito $ onOff
    pure CRCmdOk
  APIExportArchive cfg -> checkChatStopped $ exportArchive cfg $> CRCmdOk
  APIImportArchive cfg -> withStoreChanged $ importArchive cfg
  APIDeleteStorage -> withStoreChanged deleteStorage
  APIStorageEncryption cfg -> withStoreChanged $ sqlCipherExport cfg
  ExecChatStoreSQL query -> CRSQLResult <$> withStore' (`execSQL` query)
  ExecAgentStoreSQL query -> CRSQLResult <$> withAgent (`execAgentStoreSQL` query)
  APIGetChats withPCC -> CRApiChats <$> withUser' (\user -> withStore' $ \db -> getChatPreviews db user withPCC)
  APIGetChat (ChatRef cType cId) pagination search -> withUser $ \user -> case cType of
    -- TODO optimize queries calculating ChatStats, currently they're disabled
    CTDirect -> do
      directChat@Chat {chatInfo} <- withStore (\db -> getDirectChat db user cId pagination search)
      case chatInfo of DirectChat ct@Contact {contactUsed} -> unless contactUsed $ withStore' $ \db -> updateContactUsed db user ct
      pure . CRApiChat $ AChat SCTDirect directChat
    CTGroup -> CRApiChat . AChat SCTGroup <$> withStore (\db -> getGroupChat db user cId pagination search)
    CTContactRequest -> pure $ chatCmdError "not implemented"
    CTContactConnection -> pure $ chatCmdError "not supported"
  APIGetChatItems _pagination -> pure $ chatCmdError "not implemented"
  APISendMessage (ChatRef cType chatId) (ComposedMessage file_ quotedItemId_ mc) -> withUser $ \user@User {userId} -> withChatLock "sendMessage" $ case cType of
    CTDirect -> do
      ct@Contact {localDisplayName = c, contactUsed} <- withStore $ \db -> getContact db user chatId
      assertDirectAllowed user MDSnd ct XMsgNew_
      unless contactUsed $ withStore' $ \db -> updateContactUsed db user ct
      if isVoice mc && not (featureAllowed CFVoice forUser ct)
        then pure $ chatCmdError $ "feature not allowed " <> T.unpack (chatFeatureToText CFVoice)
        else do
          (fileInvitation_, ciFile_, ft_) <- unzipMaybe3 <$> setupSndFileTransfer ct
          (msgContainer, quotedItem_) <- prepareMsg fileInvitation_
          (msg@SndMessage {sharedMsgId}, _) <- sendDirectContactMessage ct (XMsgNew msgContainer)
          case ft_ of
            Just ft@FileTransferMeta {fileInline = Just IFMSent} ->
              sendDirectFileInline ct ft sharedMsgId
            _ -> pure ()
          ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc) ciFile_ quotedItem_
          setActive $ ActiveC c
          pure . CRNewChatItem $ AChatItem SCTDirect SMDSnd (DirectChat ct) ci
      where
        setupSndFileTransfer :: Contact -> m (Maybe (FileInvitation, CIFile 'MDSnd, FileTransferMeta))
        setupSndFileTransfer ct = forM file_ $ \file -> do
          (fileSize, chSize, fileInline) <- checkSndFile mc file 1
          (agentConnId_, fileConnReq) <-
            if isJust fileInline
              then pure (Nothing, Nothing)
              else bimap Just Just <$> withAgent (\a -> createConnection a True SCMInvitation Nothing)
          let fileName = takeFileName file
              fileInvitation = FileInvitation {fileName, fileSize, fileConnReq, fileInline}
          withStore' $ \db -> do
            ft@FileTransferMeta {fileId} <- createSndDirectFileTransfer db userId ct file fileInvitation agentConnId_ chSize
            fileStatus <- case fileInline of
              Just IFMSent -> createSndDirectInlineFT db ct ft $> CIFSSndTransfer
              _ -> pure CIFSSndStored
            let ciFile = CIFile {fileId, fileName, fileSize, filePath = Just file, fileStatus}
            pure (fileInvitation, ciFile, ft)
        prepareMsg :: Maybe FileInvitation -> m (MsgContainer, Maybe (CIQuote 'CTDirect))
        prepareMsg fileInvitation_ = case quotedItemId_ of
          Nothing -> pure (MCSimple (ExtMsgContent mc fileInvitation_), Nothing)
          Just quotedItemId -> do
            CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
              withStore $ \db -> getDirectChatItem db userId chatId quotedItemId
            (origQmc, qd, sent) <- quoteData qci
            let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Nothing}
                qmc = quoteContent origQmc file
                quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
            pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fileInvitation_), Just quotedItem)
          where
            quoteData :: ChatItem c d -> m (MsgContent, CIQDirection 'CTDirect, Bool)
            quoteData ChatItem {meta = CIMeta {itemDeleted = True}} = throwChatError CEInvalidQuote
            quoteData ChatItem {content = CISndMsgContent qmc} = pure (qmc, CIQDirectSnd, True)
            quoteData ChatItem {content = CIRcvMsgContent qmc} = pure (qmc, CIQDirectRcv, False)
            quoteData _ = throwChatError CEInvalidQuote
    CTGroup -> do
      Group gInfo@GroupInfo {membership, localDisplayName = gName} ms <- withStore $ \db -> getGroup db user chatId
      unless (memberActive membership) $ throwChatError CEGroupMemberUserRemoved
      if isVoice mc && not (groupFeatureAllowed GFVoice gInfo)
        then pure $ chatCmdError $ "feature not allowed " <> T.unpack (groupFeatureToText GFVoice)
        else do
          (fileInvitation_, ciFile_, ft_) <- unzipMaybe3 <$> setupSndFileTransfer gInfo (length ms)
          (msgContainer, quotedItem_) <- prepareMsg fileInvitation_ membership
          msg@SndMessage {sharedMsgId} <- sendGroupMessage gInfo ms (XMsgNew msgContainer)
          mapM_ (sendGroupFileInline ms sharedMsgId) ft_
          ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndMsgContent mc) ciFile_ quotedItem_
          setActive $ ActiveG gName
          pure . CRNewChatItem $ AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci
      where
        setupSndFileTransfer :: GroupInfo -> Int -> m (Maybe (FileInvitation, CIFile 'MDSnd, FileTransferMeta))
        setupSndFileTransfer gInfo n = forM file_ $ \file -> do
          (fileSize, chSize, fileInline) <- checkSndFile mc file $ fromIntegral n
          let fileName = takeFileName file
              fileInvitation = FileInvitation {fileName, fileSize, fileConnReq = Nothing, fileInline}
              fileStatus = if fileInline == Just IFMSent then CIFSSndTransfer else CIFSSndStored
          withStore' $ \db -> do
            ft@FileTransferMeta {fileId} <- createSndGroupFileTransfer db userId gInfo file fileInvitation chSize
            let ciFile = CIFile {fileId, fileName, fileSize, filePath = Just file, fileStatus}
            pure (fileInvitation, ciFile, ft)
        sendGroupFileInline :: [GroupMember] -> SharedMsgId -> FileTransferMeta -> m ()
        sendGroupFileInline ms sharedMsgId ft@FileTransferMeta {fileInline} =
          when (fileInline == Just IFMSent) . forM_ ms $ \case
            m@GroupMember {activeConn = Just conn@Connection {connStatus}} ->
              when (connStatus == ConnReady || connStatus == ConnSndReady) $ do
                void . withStore' $ \db -> createSndGroupInlineFT db m conn ft
                sendMemberFileInline m conn ft sharedMsgId
            _ -> pure ()
        prepareMsg :: Maybe FileInvitation -> GroupMember -> m (MsgContainer, Maybe (CIQuote 'CTGroup))
        prepareMsg fileInvitation_ membership = case quotedItemId_ of
          Nothing -> pure (MCSimple (ExtMsgContent mc fileInvitation_), Nothing)
          Just quotedItemId -> do
            CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, file} <-
              withStore $ \db -> getGroupChatItem db user chatId quotedItemId
            (origQmc, qd, sent, GroupMember {memberId}) <- quoteData qci membership
            let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Just memberId}
                qmc = quoteContent origQmc file
                quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc, formattedText}
            pure (MCQuote QuotedMsg {msgRef, content = qmc} (ExtMsgContent mc fileInvitation_), Just quotedItem)
          where
            quoteData :: ChatItem c d -> GroupMember -> m (MsgContent, CIQDirection 'CTGroup, Bool, GroupMember)
            quoteData ChatItem {meta = CIMeta {itemDeleted = True}} _ = throwChatError CEInvalidQuote
            quoteData ChatItem {chatDir = CIGroupSnd, content = CISndMsgContent qmc} membership' = pure (qmc, CIQGroupSnd, True, membership')
            quoteData ChatItem {chatDir = CIGroupRcv m, content = CIRcvMsgContent qmc} _ = pure (qmc, CIQGroupRcv $ Just m, False, m)
            quoteData _ _ = throwChatError CEInvalidQuote
    CTContactRequest -> pure $ chatCmdError "not supported"
    CTContactConnection -> pure $ chatCmdError "not supported"
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
            MCVoice {} -> False
            MCUnknown {} -> True
          qText = msgContentText qmc
          qFileName = maybe qText (T.pack . (fileName :: CIFile d -> String)) ciFile_
          qTextOrFile = if T.null qText then qFileName else qText
      unzipMaybe3 :: Maybe (a, b, c) -> (Maybe a, Maybe b, Maybe c)
      unzipMaybe3 (Just (a, b, c)) = (Just a, Just b, Just c)
      unzipMaybe3 _ = (Nothing, Nothing, Nothing)
  APIUpdateChatItem (ChatRef cType chatId) itemId mc -> withUser $ \user@User {userId} -> withChatLock "updateChatItem" $ case cType of
    CTDirect -> do
      (ct@Contact {contactId, localDisplayName = c}, ci) <- withStore $ \db -> (,) <$> getContact db user chatId <*> getDirectChatItem db userId chatId itemId
      assertDirectAllowed user MDSnd ct XMsgUpdate_
      case ci of
        CChatItem SMDSnd ChatItem {meta = CIMeta {itemSharedMsgId}, content = ciContent} -> do
          case (ciContent, itemSharedMsgId) of
            (CISndMsgContent _, Just itemSharedMId) -> do
              (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XMsgUpdate itemSharedMId mc)
              updCi <- withStore $ \db -> updateDirectChatItem db userId contactId itemId (CISndMsgContent mc) $ Just msgId
              setActive $ ActiveC c
              pure . CRChatItemUpdated $ AChatItem SCTDirect SMDSnd (DirectChat ct) updCi
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTGroup -> do
      Group gInfo@GroupInfo {groupId, localDisplayName = gName, membership} ms <- withStore $ \db -> getGroup db user chatId
      unless (memberActive membership) $ throwChatError CEGroupMemberUserRemoved
      ci <- withStore $ \db -> getGroupChatItem db user chatId itemId
      case ci of
        CChatItem SMDSnd ChatItem {meta = CIMeta {itemSharedMsgId}, content = ciContent} -> do
          case (ciContent, itemSharedMsgId) of
            (CISndMsgContent _, Just itemSharedMId) -> do
              SndMessage {msgId} <- sendGroupMessage gInfo ms (XMsgUpdate itemSharedMId mc)
              updCi <- withStore $ \db -> updateGroupChatItem db user groupId itemId (CISndMsgContent mc) msgId
              setActive $ ActiveG gName
              pure . CRChatItemUpdated $ AChatItem SCTGroup SMDSnd (GroupChat gInfo) updCi
            _ -> throwChatError CEInvalidChatItemUpdate
        CChatItem SMDRcv _ -> throwChatError CEInvalidChatItemUpdate
    CTContactRequest -> pure $ chatCmdError "not supported"
    CTContactConnection -> pure $ chatCmdError "not supported"
  APIDeleteChatItem (ChatRef cType chatId) itemId mode -> withUser $ \user@User {userId} -> withChatLock "deleteChatItem" $ case cType of
    CTDirect -> do
      (ct@Contact {localDisplayName = c}, ci@(CChatItem msgDir ChatItem {meta = CIMeta {itemSharedMsgId}})) <- withStore $ \db -> (,) <$> getContact db user chatId <*> getDirectChatItem db userId chatId itemId
      assertDirectAllowed user MDSnd ct XMsgDel_
      case (mode, msgDir, itemSharedMsgId) of
        (CIDMInternal, _, _) -> deleteDirectCI user ct ci True
        (CIDMBroadcast, SMDSnd, Just itemSharedMId) -> do
          (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XMsgDel itemSharedMId)
          setActive $ ActiveC c
          if featureAllowed CFFullDelete forUser ct
            then deleteDirectCI user ct ci True
            else markDirectCIDeleted user ct ci msgId True
        (CIDMBroadcast, _, _) -> throwChatError CEInvalidChatItemDelete
    CTGroup -> do
      Group gInfo@GroupInfo {localDisplayName = gName, membership} ms <- withStore $ \db -> getGroup db user chatId
      unless (memberActive membership) $ throwChatError CEGroupMemberUserRemoved
      ci@(CChatItem msgDir ChatItem {meta = CIMeta {itemSharedMsgId}}) <- withStore $ \db -> getGroupChatItem db user chatId itemId
      case (mode, msgDir, itemSharedMsgId) of
        (CIDMInternal, _, _) -> deleteGroupCI user gInfo ci True
        (CIDMBroadcast, SMDSnd, Just itemSharedMId) -> do
          SndMessage {msgId} <- sendGroupMessage gInfo ms (XMsgDel itemSharedMId)
          setActive $ ActiveG gName
          if groupFeatureAllowed GFFullDelete gInfo
            then deleteGroupCI user gInfo ci True
            else markGroupCIDeleted user gInfo ci msgId True
        (CIDMBroadcast, _, _) -> throwChatError CEInvalidChatItemDelete
    CTContactRequest -> pure $ chatCmdError "not supported"
    CTContactConnection -> pure $ chatCmdError "not supported"
  APIChatRead (ChatRef cType chatId) fromToIds -> case cType of
    CTDirect -> withStore' (\db -> updateDirectChatItemsRead db chatId fromToIds) $> CRCmdOk
    CTGroup -> withStore' (\db -> updateGroupChatItemsRead db chatId fromToIds) $> CRCmdOk
    CTContactRequest -> pure $ chatCmdError "not supported"
    CTContactConnection -> pure $ chatCmdError "not supported"
  APIChatUnread (ChatRef cType chatId) unreadChat -> withUser $ \user -> case cType of
    CTDirect -> do
      withStore $ \db -> do
        ct <- getContact db user chatId
        liftIO $ updateContactUnreadChat db user ct unreadChat
      pure CRCmdOk
    CTGroup -> do
      withStore $ \db -> do
        Group {groupInfo} <- getGroup db user chatId
        liftIO $ updateGroupUnreadChat db user groupInfo unreadChat
      pure CRCmdOk
    _ -> pure $ chatCmdError "not supported"
  APIDeleteChat (ChatRef cType chatId) -> withUser $ \user@User {userId} -> case cType of
    CTDirect -> do
      ct@Contact {localDisplayName} <- withStore $ \db -> getContact db user chatId
      filesInfo <- withStore' $ \db -> getContactFileInfo db user ct
      conns <- withStore $ \db -> getContactConnections db userId ct
      withChatLock "deleteChat direct" . procCmd $ do
        forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
        forM_ conns $ \conn -> deleteAgentConnectionAsync user conn `catchError` \_ -> pure ()
        -- functions below are called in separate transactions to prevent crashes on android
        -- (possibly, race condition on integrity check?)
        withStore' $ \db -> deleteContactConnectionsAndFiles db userId ct
        withStore' $ \db -> deleteContact db user ct
        unsetActive $ ActiveC localDisplayName
        pure $ CRContactDeleted ct
    CTContactConnection -> withChatLock "deleteChat contactConnection" . procCmd $ do
      conn@PendingContactConnection {pccConnId, pccAgentConnId} <- withStore $ \db -> getPendingContactConnection db userId chatId
      deleteAgentConnectionAsync' user pccConnId pccAgentConnId
      withStore' $ \db -> deletePendingContactConnection db userId chatId
      pure $ CRContactConnectionDeleted conn
    CTGroup -> do
      Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db user chatId
      let canDelete = memberRole (membership :: GroupMember) == GROwner || not (memberCurrent membership)
      unless canDelete $ throwChatError CEGroupUserRole
      filesInfo <- withStore' $ \db -> getGroupFileInfo db user gInfo
      withChatLock "deleteChat group" . procCmd $ do
        forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
        when (memberActive membership) . void $ sendGroupMessage gInfo members XGrpDel
        deleteGroupLink' user gInfo `catchError` \_ -> pure ()
        forM_ members $ deleteMemberConnection user
        -- functions below are called in separate transactions to prevent crashes on android
        -- (possibly, race condition on integrity check?)
        withStore' $ \db -> deleteGroupConnectionsAndFiles db user gInfo members
        withStore' $ \db -> deleteGroupItemsAndMembers db user gInfo members
        withStore' $ \db -> deleteGroup db user gInfo
        pure $ CRGroupDeletedUser gInfo
    CTContactRequest -> pure $ chatCmdError "not supported"
  APIClearChat (ChatRef cType chatId) -> withUser $ \user -> case cType of
    CTDirect -> do
      ct <- withStore $ \db -> getContact db user chatId
      filesInfo <- withStore' $ \db -> getContactFileInfo db user ct
      maxItemTs_ <- withStore' $ \db -> getContactMaxItemTs db user ct
      forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
      withStore' $ \db -> deleteContactCIs db user ct
      ct' <- case maxItemTs_ of
        Just ts -> do
          withStore' $ \db -> updateContactTs db user ct ts
          pure (ct :: Contact) {updatedAt = ts}
        _ -> pure ct
      pure $ CRChatCleared (AChatInfo SCTDirect (DirectChat ct'))
    CTGroup -> do
      gInfo <- withStore $ \db -> getGroupInfo db user chatId
      filesInfo <- withStore' $ \db -> getGroupFileInfo db user gInfo
      maxItemTs_ <- withStore' $ \db -> getGroupMaxItemTs db user gInfo
      forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
      withStore' $ \db -> deleteGroupCIs db user gInfo
      membersToDelete <- withStore' $ \db -> getGroupMembersForExpiration db user gInfo
      forM_ membersToDelete $ \m -> withStore' $ \db -> deleteGroupMember db user m
      gInfo' <- case maxItemTs_ of
        Just ts -> do
          withStore' $ \db -> updateGroupTs db user gInfo ts
          pure (gInfo :: GroupInfo) {updatedAt = ts}
        _ -> pure gInfo
      pure $ CRChatCleared (AChatInfo SCTGroup (GroupChat gInfo'))
    CTContactConnection -> pure $ chatCmdError "not supported"
    CTContactRequest -> pure $ chatCmdError "not supported"
  APIAcceptContact connReqId -> withUser $ \user@User {userId} -> withChatLock "acceptContact" $ do
    cReq <- withStore $ \db -> getContactRequest db userId connReqId
    -- [incognito] generate profile to send, create connection with incognito profile
    incognito <- readTVarIO =<< asks incognitoMode
    incognitoProfile <- if incognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
    ct <- acceptContactRequest user cReq incognitoProfile
    pure $ CRAcceptingContactRequest ct
  APIRejectContact connReqId -> withUser $ \User {userId} -> withChatLock "rejectContact" $ do
    cReq@UserContactRequest {agentContactConnId = AgentConnId connId, agentInvitationId = AgentInvId invId} <-
      withStore $ \db ->
        getContactRequest db userId connReqId
          `E.finally` liftIO (deleteContactRequest db userId connReqId)
    withAgent $ \a -> rejectContact a connId invId
    pure $ CRContactRequestRejected cReq
  APISendCallInvitation contactId callType -> withUser $ \user@User {userId} -> do
    -- party initiating call
    ct <- withStore $ \db -> getContact db user contactId
    assertDirectAllowed user MDSnd ct XCallInv_
    calls <- asks currentCalls
    withChatLock "sendCallInvitation" $ do
      callId <- CallId <$> (asks idsDrg >>= liftIO . (`randomBytes` 16))
      dhKeyPair <- if encryptedCall callType then Just <$> liftIO C.generateKeyPair' else pure Nothing
      let invitation = CallInvitation {callType, callDhPubKey = fst <$> dhKeyPair}
          callState = CallInvitationSent {localCallType = callType, localDhPrivKey = snd <$> dhKeyPair}
      (msg, _) <- sendDirectContactMessage ct (XCallInv callId invitation)
      ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndCall CISCallPending 0) Nothing Nothing
      let call' = Call {contactId, callId, chatItemId = chatItemId' ci, callState, callTs = chatItemTs' ci}
      call_ <- atomically $ TM.lookupInsert contactId call' calls
      forM_ call_ $ \call -> updateCallItemStatus userId ct call WCSDisconnected Nothing
      toView . CRNewChatItem $ AChatItem SCTDirect SMDSnd (DirectChat ct) ci
      pure CRCmdOk
  SendCallInvitation cName callType -> withUser $ \user -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    processChatCommand $ APISendCallInvitation contactId callType
  APIRejectCall contactId ->
    -- party accepting call
    withCurrentCall contactId $ \userId ct Call {chatItemId, callState} -> case callState of
      CallInvitationReceived {} -> do
        let aciContent = ACIContent SMDRcv $ CIRcvCall CISCallRejected 0
        withStore' $ \db -> updateDirectChatItemsRead db contactId $ Just (chatItemId, chatItemId)
        updateDirectChatItemView userId ct chatItemId aciContent Nothing $> Nothing
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallOffer contactId WebRTCCallOffer {callType, rtcSession} ->
    -- party accepting call
    withCurrentCall contactId $ \userId ct call@Call {callId, chatItemId, callState} -> case callState of
      CallInvitationReceived {peerCallType, localDhPubKey, sharedKey} -> do
        let callDhPubKey = if encryptedCall callType then localDhPubKey else Nothing
            offer = CallOffer {callType, rtcSession, callDhPubKey}
            callState' = CallOfferSent {localCallType = callType, peerCallType, localCallSession = rtcSession, sharedKey}
            aciContent = ACIContent SMDRcv $ CIRcvCall CISCallAccepted 0
        (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XCallOffer callId offer)
        withStore' $ \db -> updateDirectChatItemsRead db contactId $ Just (chatItemId, chatItemId)
        updateDirectChatItemView userId ct chatItemId aciContent $ Just msgId
        pure $ Just call {callState = callState'}
      _ -> throwChatError . CECallState $ callStateTag callState
  APISendCallAnswer contactId rtcSession ->
    -- party initiating call
    withCurrentCall contactId $ \userId ct call@Call {callId, chatItemId, callState} -> case callState of
      CallOfferReceived {localCallType, peerCallType, peerCallSession, sharedKey} -> do
        let callState' = CallNegotiated {localCallType, peerCallType, localCallSession = rtcSession, peerCallSession, sharedKey}
            aciContent = ACIContent SMDSnd $ CISndCall CISCallNegotiated 0
        (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XCallAnswer callId CallAnswer {rtcSession})
        updateDirectChatItemView userId ct chatItemId aciContent $ Just msgId
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
    withCurrentCall contactId $ \userId ct call@Call {callId} -> do
      (SndMessage {msgId}, _) <- sendDirectContactMessage ct (XCallEnd callId)
      updateCallItemStatus userId ct call WCSDisconnected $ Just msgId
      pure Nothing
  APIGetCallInvitations -> withUser $ \user -> do
    calls <- asks currentCalls >>= readTVarIO
    let invs = mapMaybe callInvitation $ M.elems calls
    CRCallInvitations <$> mapM (rcvCallInvitation user) invs
    where
      callInvitation Call {contactId, callState, callTs} = case callState of
        CallInvitationReceived {peerCallType, sharedKey} -> Just (contactId, callTs, peerCallType, sharedKey)
        _ -> Nothing
      rcvCallInvitation user (contactId, callTs, peerCallType, sharedKey) = do
        contact <- withStore (\db -> getContact db user contactId)
        pure RcvCallInvitation {contact, callType = peerCallType, sharedKey, callTs}
  APICallStatus contactId receivedStatus ->
    withCurrentCall contactId $ \userId ct call ->
      updateCallItemStatus userId ct call receivedStatus Nothing $> Just call
  APIUpdateProfile profile -> withUser (`updateProfile` profile)
  APISetContactPrefs contactId prefs' -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    updateContactPrefs user ct prefs'
  APISetContactAlias contactId localAlias -> withUser $ \user@User {userId} -> do
    ct' <- withStore $ \db -> do
      ct <- getContact db user contactId
      liftIO $ updateContactAlias db userId ct localAlias
    pure $ CRContactAliasUpdated ct'
  APISetConnectionAlias connId localAlias -> withUser $ \User {userId} -> do
    conn' <- withStore $ \db -> do
      conn <- getPendingContactConnection db userId connId
      liftIO $ updateContactConnectionAlias db userId conn localAlias
    pure $ CRConnectionAliasUpdated conn'
  APIParseMarkdown text -> pure . CRApiParsedMarkdown $ parseMaybeMarkdownList text
  APIGetNtfToken -> withUser $ \_ -> crNtfToken <$> withAgent getNtfToken
  APIRegisterToken token mode -> CRNtfTokenStatus <$> withUser (\_ -> withAgent $ \a -> registerNtfToken a token mode)
  APIVerifyToken token nonce code -> withUser $ \_ -> withAgent (\a -> verifyNtfToken a token nonce code) $> CRCmdOk
  APIDeleteToken token -> withUser $ \_ -> withAgent (`deleteNtfToken` token) $> CRCmdOk
  APIGetNtfMessage nonce encNtfInfo -> withUser $ \user -> do
    (NotificationInfo {ntfConnId, ntfMsgMeta}, msgs) <- withAgent $ \a -> getNotificationMessage a nonce encNtfInfo
    let ntfMessages = map (\SMP.SMPMsgMeta {msgTs, msgFlags} -> NtfMsgInfo {msgTs = systemToUTCTime msgTs, msgFlags}) msgs
        msgTs' = systemToUTCTime . (SMP.msgTs :: SMP.NMsgMeta -> SystemTime) <$> ntfMsgMeta
    connEntity <- withStore (\db -> Just <$> getConnectionEntity db user (AgentConnId ntfConnId)) `catchError` \_ -> pure Nothing
    pure CRNtfMessages {connEntity, msgTs = msgTs', ntfMessages}
  GetUserSMPServers -> do
    ChatConfig {defaultServers = InitialAgentServers {smp = defaultSMPServers}} <- asks config
    smpServers <- withUser (\user -> withStore' (`getSMPServers` user))
    let smpServers' = fromMaybe (L.map toServerCfg defaultSMPServers) $ nonEmpty smpServers
    pure $ CRUserSMPServers smpServers' defaultSMPServers
    where
      toServerCfg server = ServerCfg {server, preset = True, tested = Nothing, enabled = True}
  SetUserSMPServers (SMPServersConfig smpServers) -> withUser $ \user -> withChatLock "setUserSMPServers" $ do
    withStore $ \db -> overwriteSMPServers db user smpServers
    cfg <- asks config
    withAgent $ \a -> setSMPServers a $ activeAgentServers cfg smpServers
    pure CRCmdOk
  TestSMPServer smpServer -> CRSmpTestResult <$> withAgent (`testSMPServerConnection` smpServer)
  APISetChatItemTTL newTTL_ -> withUser' $ \user ->
    checkStoreNotChanged $
      withChatLock "setChatItemTTL" $ do
        case newTTL_ of
          Nothing -> do
            withStore' $ \db -> setChatItemTTL db user newTTL_
            setExpireCIs False
          Just newTTL -> do
            oldTTL <- withStore' (`getChatItemTTL` user)
            when (maybe True (newTTL <) oldTTL) $ do
              setExpireCIs False
              expireChatItems user newTTL True
            withStore' $ \db -> setChatItemTTL db user newTTL_
            whenM chatStarted $ setExpireCIs True
        pure CRCmdOk
  APIGetChatItemTTL -> CRChatItemTTL <$> withUser (\user -> withStore' (`getChatItemTTL` user))
  APISetNetworkConfig cfg -> withUser' $ \_ -> withAgent (`setNetworkConfig` cfg) $> CRCmdOk
  APIGetNetworkConfig -> CRNetworkConfig <$> withUser' (\_ -> withAgent getNetworkConfig)
  APISetChatSettings (ChatRef cType chatId) chatSettings -> withUser $ \user -> case cType of
    CTDirect -> do
      ct <- withStore $ \db -> do
        ct <- getContact db user chatId
        liftIO $ updateContactSettings db user chatId chatSettings
        pure ct
      withAgent $ \a -> toggleConnectionNtfs a (contactConnId ct) (enableNtfs chatSettings)
      pure CRCmdOk
    CTGroup -> do
      ms <- withStore $ \db -> do
        Group _ ms <- getGroup db user chatId
        liftIO $ updateGroupSettings db user chatId chatSettings
        pure ms
      forM_ (filter memberActive ms) $ \m -> forM_ (memberConnId m) $ \connId ->
        withAgent (\a -> toggleConnectionNtfs a connId $ enableNtfs chatSettings) `catchError` (toView . CRChatError)
      pure CRCmdOk
    _ -> pure $ chatCmdError "not supported"
  APIContactInfo contactId -> withUser $ \user@User {userId} -> do
    -- [incognito] print user's incognito profile for this contact
    ct@Contact {activeConn = Connection {customUserProfileId}} <- withStore $ \db -> getContact db user contactId
    incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
    connectionStats <- withAgent (`getConnectionServers` contactConnId ct)
    pure $ CRContactInfo ct connectionStats (fmap fromLocalProfile incognitoProfile)
  APIGroupMemberInfo gId gMemberId -> withUser $ \user -> do
    (g, m) <- withStore $ \db -> (,) <$> getGroupInfo db user gId <*> getGroupMember db user gId gMemberId
    connectionStats <- mapM (withAgent . flip getConnectionServers) (memberConnId m)
    pure $ CRGroupMemberInfo g m connectionStats
  APISwitchContact contactId -> withUser $ \user -> do
    ct <- withStore $ \db -> getContact db user contactId
    withAgent $ \a -> switchConnectionAsync a "" $ contactConnId ct
    pure CRCmdOk
  APISwitchGroupMember gId gMemberId -> withUser $ \user -> do
    m <- withStore $ \db -> getGroupMember db user gId gMemberId
    case memberConnId m of
      Just connId -> withAgent (\a -> switchConnectionAsync a "" connId) $> CRCmdOk
      _ -> throwChatError CEGroupMemberNotActive
  ShowMessages (ChatName cType name) ntfOn -> withUser $ \user -> do
    chatId <- case cType of
      CTDirect -> withStore $ \db -> getContactIdByName db user name
      CTGroup -> withStore $ \db -> getGroupIdByName db user name
      _ -> throwChatError $ CECommandError "not supported"
    processChatCommand $ APISetChatSettings (ChatRef cType chatId) $ ChatSettings ntfOn
  ContactInfo cName -> withUser $ \user -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    processChatCommand $ APIContactInfo contactId
  GroupMemberInfo gName mName -> withUser $ \user -> do
    (gId, mId) <- withStore $ \db -> getGroupIdByName db user gName >>= \gId -> (gId,) <$> getGroupMemberIdByName db user gId mName
    processChatCommand $ APIGroupMemberInfo gId mId
  SwitchContact cName -> withUser $ \user -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    processChatCommand $ APISwitchContact contactId
  SwitchGroupMember gName mName -> withUser $ \user -> do
    (gId, mId) <- withStore $ \db -> getGroupIdByName db user gName >>= \gId -> (gId,) <$> getGroupMemberIdByName db user gId mName
    processChatCommand $ APISwitchGroupMember gId mId
  ChatHelp section -> pure $ CRChatHelp section
  Welcome -> withUser $ pure . CRWelcome
  AddContact -> withUser $ \User {userId} -> withChatLock "addContact" . procCmd $ do
    -- [incognito] generate profile for connection
    incognito <- readTVarIO =<< asks incognitoMode
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    (connId, cReq) <- withAgent $ \a -> createConnection a True SCMInvitation Nothing
    conn <- withStore' $ \db -> createDirectConnection db userId connId cReq ConnNew incognitoProfile
    toView $ CRNewContactConnection conn
    pure $ CRInvitation cReq
  Connect (Just (ACR SCMInvitation cReq)) -> withUser $ \user@User {userId} -> withChatLock "connect" . procCmd $ do
    -- [incognito] generate profile to send
    incognito <- readTVarIO =<< asks incognitoMode
    incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
    let profileToSend = userProfileToSend user incognitoProfile Nothing
    connId <- withAgent $ \a -> joinConnection a True cReq . directMessage $ XInfo profileToSend
    conn <- withStore' $ \db -> createDirectConnection db userId connId cReq ConnJoined $ incognitoProfile $> profileToSend
    toView $ CRNewContactConnection conn
    pure CRSentConfirmation
  Connect (Just (ACR SCMContact cReq)) -> withUser $ \user ->
    -- [incognito] generate profile to send
    connectViaContact user cReq
  Connect Nothing -> throwChatError CEInvalidConnReq
  ConnectSimplex -> withUser $ \user ->
    -- [incognito] generate profile to send
    connectViaContact user adminContactReq
  DeleteContact cName -> withUser $ \user -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    processChatCommand $ APIDeleteChat (ChatRef CTDirect contactId)
  ClearContact cName -> withUser $ \user -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    processChatCommand $ APIClearChat (ChatRef CTDirect contactId)
  ListContacts -> withUser $ \user -> CRContactsList <$> withStore' (`getUserContacts` user)
  CreateMyAddress -> withUser $ \User {userId} -> withChatLock "createMyAddress" . procCmd $ do
    (connId, cReq) <- withAgent $ \a -> createConnection a True SCMContact Nothing
    withStore $ \db -> createUserContactLink db userId connId cReq
    pure $ CRUserContactLinkCreated cReq
  DeleteMyAddress -> withUser $ \user -> withChatLock "deleteMyAddress" $ do
    conns <- withStore (`getUserAddressConnections` user)
    procCmd $ do
      forM_ conns $ \conn -> deleteAgentConnectionAsync user conn `catchError` \_ -> pure ()
      withStore' (`deleteUserAddress` user)
      pure CRUserContactLinkDeleted
  ShowMyAddress -> withUser $ \User {userId} ->
    CRUserContactLink <$> withStore (`getUserAddress` userId)
  AddressAutoAccept autoAccept_ -> withUser $ \User {userId} -> do
    CRUserContactLinkUpdated <$> withStore (\db -> updateUserAddressAutoAccept db userId autoAccept_)
  AcceptContact cName -> withUser $ \User {userId} -> do
    connReqId <- withStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand $ APIAcceptContact connReqId
  RejectContact cName -> withUser $ \User {userId} -> do
    connReqId <- withStore $ \db -> getContactRequestIdByName db userId cName
    processChatCommand $ APIRejectContact connReqId
  SendMessage chatName msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    let mc = MCText $ safeDecodeUtf8 msg
    processChatCommand . APISendMessage chatRef $ ComposedMessage Nothing Nothing mc
  SendMessageBroadcast msg -> withUser $ \user -> do
    contacts <- withStore' (`getUserContacts` user)
    withChatLock "sendMessageBroadcast" . procCmd $ do
      let mc = MCText $ safeDecodeUtf8 msg
          cts = filter (\ct -> isReady ct && directContact ct) contacts
      forM_ cts $ \ct ->
        void
          ( do
              (sndMsg, _) <- sendDirectContactMessage ct (XMsgNew $ MCSimple (ExtMsgContent mc Nothing))
              saveSndChatItem user (CDDirectSnd ct) sndMsg (CISndMsgContent mc) Nothing Nothing
          )
          `catchError` (toView . CRChatError)
      CRBroadcastSent mc (length cts) <$> liftIO getZonedTime
  SendMessageQuote cName (AMsgDirection msgDir) quotedMsg msg -> withUser $ \user@User {userId} -> do
    contactId <- withStore $ \db -> getContactIdByName db user cName
    quotedItemId <- withStore $ \db -> getDirectChatItemIdByText db userId contactId msgDir (safeDecodeUtf8 quotedMsg)
    let mc = MCText $ safeDecodeUtf8 msg
    processChatCommand . APISendMessage (ChatRef CTDirect contactId) $ ComposedMessage Nothing (Just quotedItemId) mc
  DeleteMessage chatName deletedMsg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    deletedItemId <- getSentChatItemIdByText user chatRef deletedMsg
    processChatCommand $ APIDeleteChatItem chatRef deletedItemId CIDMBroadcast
  EditMessage chatName editedMsg msg -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    editedItemId <- getSentChatItemIdByText user chatRef editedMsg
    let mc = MCText $ safeDecodeUtf8 msg
    processChatCommand $ APIUpdateChatItem chatRef editedItemId mc
  NewGroup gProfile -> withUser $ \user -> do
    gVar <- asks idsDrg
    groupInfo <- withStore (\db -> createNewGroup db gVar user gProfile)
    pure $ CRGroupCreated groupInfo
  APIAddMember groupId contactId memRole -> withUser $ \user -> withChatLock "addMember" $ do
    -- TODO for large groups: no need to load all members to determine if contact is a member
    (group, contact) <- withStore $ \db -> (,) <$> getGroup db user groupId <*> getContact db user contactId
    assertDirectAllowed user MDSnd contact XGrpInv_
    let Group gInfo@GroupInfo {membership} members = group
        GroupMember {memberRole = userRole} = membership
        Contact {localDisplayName = cName} = contact
    -- [incognito] forbid to invite contact to whom user is connected incognito
    when (contactConnIncognito contact) $ throwChatError CEContactIncognitoCantInvite
    -- [incognito] forbid to invite contacts if user joined the group using an incognito profile
    when (memberIncognito membership) $ throwChatError CEGroupIncognitoCantInvite
    when (userRole < GRAdmin || userRole < memRole) $ throwChatError CEGroupUserRole
    when (memberStatus membership == GSMemInvited) $ throwChatError (CEGroupNotJoined gInfo)
    unless (memberActive membership) $ throwChatError CEGroupMemberNotActive
    let sendInvitation = sendGrpInvitation user contact gInfo
    case contactMember contact members of
      Nothing -> do
        gVar <- asks idsDrg
        (agentConnId, cReq) <- withAgent $ \a -> createConnection a True SCMInvitation Nothing
        member <- withStore $ \db -> createNewContactMember db gVar user groupId contact memRole agentConnId cReq
        sendInvitation member cReq
        pure $ CRSentGroupInvitation gInfo contact member
      Just member@GroupMember {groupMemberId, memberStatus, memberRole = mRole}
        | memberStatus == GSMemInvited -> do
          unless (mRole == memRole) $ withStore' $ \db -> updateGroupMemberRole db user member memRole
          withStore' (\db -> getMemberInvitation db user groupMemberId) >>= \case
            Just cReq -> sendInvitation member {memberRole = memRole} cReq $> CRSentGroupInvitation gInfo contact member {memberRole = memRole}
            Nothing -> throwChatError $ CEGroupCantResendInvitation gInfo cName
        | otherwise -> throwChatError $ CEGroupDuplicateMember cName
  APIJoinGroup groupId -> withUser $ \user@User {userId} -> do
    ReceivedGroupInvitation {fromMember, connRequest, groupInfo = g@GroupInfo {membership}} <- withStore $ \db -> getGroupInvitation db user groupId
    withChatLock "joinGroup" . procCmd $ do
      agentConnId <- withAgent $ \a -> joinConnection a True connRequest . directMessage $ XGrpAcpt (memberId (membership :: GroupMember))
      withStore' $ \db -> do
        createMemberConnection db userId fromMember agentConnId
        updateGroupMemberStatus db userId fromMember GSMemAccepted
        updateGroupMemberStatus db userId membership GSMemAccepted
      updateCIGroupInvitationStatus user
      pure $ CRUserAcceptedGroupSent g {membership = membership {memberStatus = GSMemAccepted}} Nothing
    where
      updateCIGroupInvitationStatus user@User {userId} = do
        AChatItem _ _ cInfo ChatItem {content, meta = CIMeta {itemId}} <- withStore $ \db -> getChatItemByGroupId db user groupId
        case (cInfo, content) of
          (DirectChat ct, CIRcvGroupInvitation ciGroupInv memRole) -> do
            let aciContent = ACIContent SMDRcv $ CIRcvGroupInvitation ciGroupInv {status = CIGISAccepted} memRole
            updateDirectChatItemView userId ct itemId aciContent Nothing
          _ -> pure () -- prohibited
  APIMemberRole groupId memberId memRole -> withUser $ \user -> do
    Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db user groupId
    if memberId == groupMemberId' membership
      then changeMemberRole user gInfo members membership $ SGEUserRole memRole
      else case find ((== memberId) . groupMemberId') members of
        Just m -> changeMemberRole user gInfo members m $ SGEMemberRole memberId (fromLocalProfile $ memberProfile m) memRole
        _ -> throwChatError CEGroupMemberNotFound
    where
      changeMemberRole user gInfo@GroupInfo {membership} members m gEvent = do
        let GroupMember {memberId = mId, memberRole = mRole, memberStatus = mStatus, memberContactId, localDisplayName = cName} = m
            GroupMember {memberRole = userRole} = membership
            canChangeRole = userRole >= GRAdmin && userRole >= mRole && userRole >= memRole && memberCurrent membership
        unless canChangeRole $ throwChatError CEGroupUserRole
        withChatLock "memberRole" . procCmd $ do
          unless (mRole == memRole) $ do
            withStore' $ \db -> updateGroupMemberRole db user m memRole
            case mStatus of
              GSMemInvited -> do
                withStore (\db -> (,) <$> mapM (getContact db user) memberContactId <*> liftIO (getMemberInvitation db user $ groupMemberId' m)) >>= \case
                  (Just ct, Just cReq) -> sendGrpInvitation user ct gInfo (m :: GroupMember) {memberRole = memRole} cReq
                  _ -> throwChatError $ CEGroupCantResendInvitation gInfo cName
              _ -> do
                msg <- sendGroupMessage gInfo members $ XGrpMemRole mId memRole
                ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent gEvent) Nothing Nothing
                toView . CRNewChatItem $ AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci
          pure CRMemberRoleUser {groupInfo = gInfo, member = m {memberRole = memRole}, fromRole = mRole, toRole = memRole}
  APIRemoveMember groupId memberId -> withUser $ \user@User {userId} -> do
    Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db user groupId
    case find ((== memberId) . groupMemberId') members of
      Nothing -> throwChatError CEGroupMemberNotFound
      Just m@GroupMember {memberId = mId, memberRole = mRole, memberStatus = mStatus, memberProfile} -> do
        let userRole = memberRole (membership :: GroupMember)
            canRemove = userRole >= GRAdmin && userRole >= mRole && memberCurrent membership
        unless canRemove $ throwChatError CEGroupUserRole
        withChatLock "removeMember" . procCmd $ do
          case mStatus of
            GSMemInvited -> do
              deleteMemberConnection user m
              withStore' $ \db -> deleteGroupMember db user m
            _ -> do
              msg <- sendGroupMessage gInfo members $ XGrpMemDel mId
              ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent $ SGEMemberDeleted memberId (fromLocalProfile memberProfile)) Nothing Nothing
              toView . CRNewChatItem $ AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci
              deleteMemberConnection user m
              withStore' $ \db ->
                checkGroupMemberHasItems db user m >>= \case
                  Just _ -> updateGroupMemberStatus db userId m GSMemRemoved
                  Nothing -> deleteGroupMember db user m
          pure $ CRUserDeletedMember gInfo m {memberStatus = GSMemRemoved}
  APILeaveGroup groupId -> withUser $ \user@User {userId} -> do
    Group gInfo@GroupInfo {membership} members <- withStore $ \db -> getGroup db user groupId
    withChatLock "leaveGroup" . procCmd $ do
      msg <- sendGroupMessage gInfo members XGrpLeave
      ci <- saveSndChatItem user (CDGroupSnd gInfo) msg (CISndGroupEvent SGEUserLeft) Nothing Nothing
      toView . CRNewChatItem $ AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci
      -- TODO delete direct connections that were unused
      deleteGroupLink' user gInfo `catchError` \_ -> pure ()
      forM_ members $ deleteMemberConnection user
      withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemLeft
      pure $ CRLeftMemberUser gInfo {membership = membership {memberStatus = GSMemLeft}}
  APIListMembers groupId -> CRGroupMembers <$> withUser (\user -> withStore (\db -> getGroup db user groupId))
  AddMember gName cName memRole -> withUser $ \user -> do
    (groupId, contactId) <- withStore $ \db -> (,) <$> getGroupIdByName db user gName <*> getContactIdByName db user cName
    processChatCommand $ APIAddMember groupId contactId memRole
  JoinGroup gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIJoinGroup groupId
  MemberRole gName groupMemberName memRole -> do
    (groupId, groupMemberId) <- getGroupAndMemberId gName groupMemberName
    processChatCommand $ APIMemberRole groupId groupMemberId memRole
  RemoveMember gName groupMemberName -> do
    (groupId, groupMemberId) <- getGroupAndMemberId gName groupMemberName
    processChatCommand $ APIRemoveMember groupId groupMemberId
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
  ListGroups -> CRGroupsList <$> withUser (\user -> withStore' (`getUserGroupDetails` user))
  APIUpdateGroupProfile groupId p' -> withUser $ \user -> do
    g <- withStore $ \db -> getGroup db user groupId
    runUpdateGroupProfile user g p'
  UpdateGroupProfile gName profile -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIUpdateGroupProfile groupId profile
  APICreateGroupLink groupId -> withUser $ \user -> withChatLock "createGroupLink" $ do
    gInfo@GroupInfo {membership = membership@GroupMember {memberRole = userRole}} <- withStore $ \db -> getGroupInfo db user groupId
    when (userRole < GRAdmin) $ throwChatError CEGroupUserRole
    when (memberStatus membership == GSMemInvited) $ throwChatError (CEGroupNotJoined gInfo)
    unless (memberActive membership) $ throwChatError CEGroupMemberNotActive
    groupLinkId <- GroupLinkId <$> (asks idsDrg >>= liftIO . (`randomBytes` 16))
    let crClientData = encodeJSON $ CRDataGroup groupLinkId
    (connId, cReq) <- withAgent $ \a -> createConnection a True SCMContact $ Just crClientData
    withStore $ \db -> createGroupLink db user gInfo connId cReq groupLinkId
    pure $ CRGroupLinkCreated gInfo cReq
  APIDeleteGroupLink groupId -> withUser $ \user -> withChatLock "deleteGroupLink" $ do
    gInfo <- withStore $ \db -> getGroupInfo db user groupId
    deleteGroupLink' user gInfo
    pure $ CRGroupLinkDeleted gInfo
  APIGetGroupLink groupId -> withUser $ \user -> do
    gInfo <- withStore $ \db -> getGroupInfo db user groupId
    CRGroupLink gInfo <$> withStore (\db -> getGroupLink db user gInfo)
  CreateGroupLink gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APICreateGroupLink groupId
  DeleteGroupLink gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIDeleteGroupLink groupId
  ShowGroupLink gName -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    processChatCommand $ APIGetGroupLink groupId
  SendGroupMessageQuote gName cName quotedMsg msg -> withUser $ \user -> do
    groupId <- withStore $ \db -> getGroupIdByName db user gName
    quotedItemId <- withStore $ \db -> getGroupChatItemIdByText db user groupId cName (safeDecodeUtf8 quotedMsg)
    let mc = MCText $ safeDecodeUtf8 msg
    processChatCommand . APISendMessage (ChatRef CTGroup groupId) $ ComposedMessage Nothing (Just quotedItemId) mc
  LastMessages (Just chatName) count search -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    CRLastMessages . aChatItems . chat <$> processChatCommand (APIGetChat chatRef (CPLast count) search)
  LastMessages Nothing count search -> withUser $ \user -> withStore $ \db ->
    CRLastMessages <$> getAllChatItems db user (CPLast count) search
  SendFile chatName f -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    processChatCommand . APISendMessage chatRef $ ComposedMessage (Just f) Nothing (MCFile "")
  SendImage chatName f -> withUser $ \user -> do
    chatRef <- getChatRef user chatName
    filePath <- toFSFilePath f
    unless (".jpg" `isSuffixOf` f || ".jpeg" `isSuffixOf` f) $ throwChatError CEFileImageType {filePath}
    fileSize <- getFileSize filePath
    unless (fileSize <= maxImageSize) $ throwChatError CEFileImageSize {filePath}
    processChatCommand . APISendMessage chatRef $ ComposedMessage (Just f) Nothing (MCImage "" fixedImagePreview)
  ForwardFile chatName fileId -> forwardFile chatName fileId SendFile
  ForwardImage chatName fileId -> forwardFile chatName fileId SendImage
  ReceiveFile fileId rcvInline_ filePath_ -> withUser $ \user ->
    withChatLock "receiveFile" . procCmd $ do
      ft <- withStore $ \db -> getRcvFileTransfer db user fileId
      (CRRcvFileAccepted <$> acceptFileReceive user ft rcvInline_ filePath_) `catchError` processError ft
    where
      processError ft = \case
        -- TODO AChatItem in Cancelled events
        ChatErrorAgent (SMP SMP.AUTH) -> pure $ CRRcvFileAcceptedSndCancelled ft
        ChatErrorAgent (CONN DUPLICATE) -> pure $ CRRcvFileAcceptedSndCancelled ft
        e -> throwError e
  CancelFile fileId -> withUser $ \user@User {userId} ->
    withChatLock "cancelFile" . procCmd $
      withStore (\db -> getFileTransfer db user fileId) >>= \case
        FTSnd ftm@FileTransferMeta {cancelled} fts -> do
          unless cancelled $ do
            cancelSndFile user ftm fts
            sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
            withStore (\db -> getChatRefByFileId db user fileId) >>= \case
              ChatRef CTDirect contactId -> do
                contact <- withStore $ \db -> getContact db user contactId
                void . sendDirectContactMessage contact $ XFileCancel sharedMsgId
              ChatRef CTGroup groupId -> do
                Group gInfo ms <- withStore $ \db -> getGroup db user groupId
                void . sendGroupMessage gInfo ms $ XFileCancel sharedMsgId
              _ -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
          ci <- withStore $ \db -> getChatItemByFileId db user fileId
          pure $ CRSndGroupFileCancelled ci ftm fts
        FTRcv ftr@RcvFileTransfer {cancelled} -> do
          unless cancelled $ cancelRcvFileTransfer user ftr
          pure $ CRRcvFileCancelled ftr
  FileStatus fileId ->
    CRFileTransferStatus <$> withUser (\user -> withStore $ \db -> getFileTransferProgress db user fileId)
  ShowProfile -> withUser $ \User {profile} -> pure $ CRUserProfile (fromLocalProfile profile)
  UpdateProfile displayName fullName -> withUser $ \user@User {profile} -> do
    let p = (fromLocalProfile profile :: Profile) {displayName = displayName, fullName = fullName}
    updateProfile user p
  UpdateProfileImage image -> withUser $ \user@User {profile} -> do
    let p = (fromLocalProfile profile :: Profile) {image}
    updateProfile user p
  SetUserFeature f allowed -> withUser $ \user@User {profile} -> do
    let p = (fromLocalProfile profile :: Profile) {preferences = Just . setPreference f (Just allowed) $ preferences' user}
    updateProfile user p
  SetContactFeature f cName allowed_ -> withUser $ \user -> do
    ct@Contact {userPreferences} <- withStore $ \db -> getContactByName db user cName
    let prefs' = setPreference f allowed_ $ Just userPreferences
    updateContactPrefs user ct prefs'
  SetGroupFeature f gName enabled -> withUser $ \user -> do
    g@(Group GroupInfo {groupProfile = p} _) <- withStore $ \db -> getGroup db user =<< getGroupIdByName db user gName
    let p' = p {groupPreferences = Just . setGroupPreference f enabled $ groupPreferences p}
    runUpdateGroupProfile user g p'
  QuitChat -> liftIO exitSuccess
  ShowVersion -> pure $ CRVersionInfo versionNumber
  DebugLocks -> do
    chatLockName <- atomically . tryReadTMVar =<< asks chatLock
    agentLocks <- withAgent debugAgentLocks
    pure CRDebugLocks {chatLockName, agentLocks}
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
    --       (atomically . writeTBQueue q) . (Just corrId,) =<< (action `catchError` (pure . CRChatError))
    --   pure $ CRCmdAccepted corrId
    -- use function below to make commands "synchronous"
    procCmd :: m ChatResponse -> m ChatResponse
    procCmd = id
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
    withStoreChanged a = checkChatStopped $ a >> setStoreChanged $> CRCmdOk
    checkStoreNotChanged :: m ChatResponse -> m ChatResponse
    checkStoreNotChanged = ifM (asks chatStoreChanged >>= readTVarIO) (throwChatError CEChatStoreChanged)
    getSentChatItemIdByText :: User -> ChatRef -> ByteString -> m Int64
    getSentChatItemIdByText user@User {userId, localDisplayName} (ChatRef cType cId) msg = case cType of
      CTDirect -> withStore $ \db -> getDirectChatItemIdByText db userId cId SMDSnd (safeDecodeUtf8 msg)
      CTGroup -> withStore $ \db -> getGroupChatItemIdByText db user cId (Just localDisplayName) (safeDecodeUtf8 msg)
      _ -> throwChatError $ CECommandError "not supported"
    connectViaContact :: User -> ConnectionRequestUri 'CMContact -> m ChatResponse
    connectViaContact user@User {userId} cReq@(CRContactUri ConnReqUriData {crClientData}) = withChatLock "connectViaContact" $ do
      let cReqHash = ConnReqUriHash . C.sha256Hash $ strEncode cReq
      withStore' (\db -> getConnReqContactXContactId db user cReqHash) >>= \case
        (Just contact, _) -> pure $ CRContactAlreadyExists contact
        (_, xContactId_) -> procCmd $ do
          let randomXContactId = XContactId <$> (asks idsDrg >>= liftIO . (`randomBytes` 16))
          xContactId <- maybe randomXContactId pure xContactId_
          -- [incognito] generate profile to send
          -- if user makes a contact request using main profile, then turns on incognito mode and repeats the request,
          -- an incognito profile will be sent even though the address holder will have user's main profile received as well;
          -- we ignore this edge case as we already allow profile updates on repeat contact requests;
          -- alternatively we can re-send the main profile even if incognito mode is enabled
          incognito <- readTVarIO =<< asks incognitoMode
          incognitoProfile <- if incognito then Just <$> liftIO generateRandomProfile else pure Nothing
          let profileToSend = userProfileToSend user incognitoProfile Nothing
          connId <- withAgent $ \a -> joinConnection a True cReq $ directMessage (XContact profileToSend $ Just xContactId)
          let groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
          conn <- withStore' $ \db -> createConnReqConnection db userId connId cReqHash xContactId incognitoProfile groupLinkId
          toView $ CRNewContactConnection conn
          pure $ CRSentInvitation incognitoProfile
    contactMember :: Contact -> [GroupMember] -> Maybe GroupMember
    contactMember Contact {contactId} =
      find $ \GroupMember {memberContactId = cId, memberStatus = s} ->
        cId == Just contactId && s /= GSMemRemoved && s /= GSMemLeft
    checkSndFile :: MsgContent -> FilePath -> Integer -> m (Integer, Integer, Maybe InlineFileMode)
    checkSndFile mc f n = do
      fsFilePath <- toFSFilePath f
      unlessM (doesFileExist fsFilePath) . throwChatError $ CEFileNotFound f
      ChatConfig {fileChunkSize, inlineFiles} <- asks config
      fileSize <- getFileSize fsFilePath
      let chunks = - ((- fileSize) `div` fileChunkSize)
      pure (fileSize, fileChunkSize, inlineFileMode mc inlineFiles chunks n)
    inlineFileMode mc InlineFilesConfig {offerChunks, sendChunks, totalSendChunks} chunks n
      | chunks > offerChunks = Nothing
      | chunks <= sendChunks && chunks * n <= totalSendChunks && isVoice mc = Just IFMSent
      | otherwise = Just IFMOffer
    updateProfile :: User -> Profile -> m ChatResponse
    updateProfile user@User {profile = p@LocalProfile {profileId, localAlias}} p'@Profile {displayName}
      | p' == fromLocalProfile p = pure CRUserProfileNoChange
      | otherwise = do
        withStore $ \db -> updateUserProfile db user p'
        let user' = (user :: User) {localDisplayName = displayName, profile = toLocalProfile profileId p' localAlias}
        asks currentUser >>= atomically . (`writeTVar` Just user')
        -- [incognito] filter out contacts with whom user has incognito connections
        contacts <-
          filter (\ct -> isReady ct && not (contactConnIncognito ct))
            <$> withStore' (`getUserContacts` user)
        withChatLock "updateProfile" . procCmd $ do
          forM_ contacts $ \ct -> do
            let mergedProfile = userProfileToSend user' Nothing $ Just ct
                ct' = updateMergedPreferences user' ct
            void (sendDirectContactMessage ct $ XInfo mergedProfile) `catchError` (toView . CRChatError)
            when (directContact ct) $ createFeatureChangedItems user' ct ct' CDDirectSnd CISndChatFeature
          pure $ CRUserProfileUpdated (fromLocalProfile p) p'
    updateContactPrefs :: User -> Contact -> Preferences -> m ChatResponse
    updateContactPrefs user@User {userId} ct@Contact {activeConn = Connection {customUserProfileId}, userPreferences = contactUserPrefs} contactUserPrefs'
      | contactUserPrefs == contactUserPrefs' = pure $ CRContactPrefsUpdated ct ct
      | otherwise = do
        assertDirectAllowed user MDSnd ct XInfo_
        ct' <- withStore' $ \db -> updateContactUserPreferences db user ct contactUserPrefs'
        incognitoProfile <- forM customUserProfileId $ \profileId -> withStore $ \db -> getProfileById db userId profileId
        let p' = userProfileToSend user (fromLocalProfile <$> incognitoProfile) (Just ct')
        withChatLock "updateProfile" . procCmd $ do
          void (sendDirectContactMessage ct' $ XInfo p') `catchError` (toView . CRChatError)
          when (directContact ct) $ createFeatureChangedItems user ct ct' CDDirectSnd CISndChatFeature
          pure $ CRContactPrefsUpdated ct ct'
    runUpdateGroupProfile :: User -> Group -> GroupProfile -> m ChatResponse
    runUpdateGroupProfile user (Group g@GroupInfo {groupProfile = p} ms) p' = do
      let s = memberStatus $ membership g
          canUpdate =
            memberRole (membership g :: GroupMember) == GROwner
              || (s == GSMemRemoved || s == GSMemLeft || s == GSMemGroupDeleted || s == GSMemInvited)
      unless canUpdate $ throwChatError CEGroupUserRole
      g' <- withStore $ \db -> updateGroupProfile db user g p'
      msg <- sendGroupMessage g' ms (XGrpInfo p')
      let cd = CDGroupSnd g'
      unless (sameGroupProfileInfo p p') $ do
        ci <- saveSndChatItem user cd msg (CISndGroupEvent $ SGEGroupUpdated p') Nothing Nothing
        toView . CRNewChatItem $ AChatItem SCTGroup SMDSnd (GroupChat g') ci
      createGroupFeatureChangedItems user cd CISndGroupFeature p p'
      pure $ CRGroupUpdated g g' Nothing
    isReady :: Contact -> Bool
    isReady ct =
      let s = connStatus $ activeConn (ct :: Contact)
       in s == ConnReady || s == ConnSndReady
    withCurrentCall :: ContactId -> (UserId -> Contact -> Call -> m (Maybe Call)) -> m ChatResponse
    withCurrentCall ctId action = withUser $ \user@User {userId} -> do
      ct <- withStore $ \db -> getContact db user ctId
      calls <- asks currentCalls
      withChatLock "currentCall" $
        atomically (TM.lookup ctId calls) >>= \case
          Nothing -> throwChatError CENoCurrentCall
          Just call@Call {contactId}
            | ctId == contactId -> do
              call_ <- action userId ct call
              case call_ of
                Just call' -> do
                  unless (isRcvInvitation call') $ withStore' $ \db -> deleteCalls db user ctId
                  atomically $ TM.insert ctId call' calls
                _ -> do
                  withStore' $ \db -> deleteCalls db user ctId
                  atomically $ TM.delete ctId calls
              pure CRCmdOk
            | otherwise -> throwChatError $ CECallContact contactId
    forwardFile :: ChatName -> FileTransferId -> (ChatName -> FilePath -> ChatCommand) -> m ChatResponse
    forwardFile chatName fileId sendCommand = withUser $ \user -> do
      withStore (\db -> getFileTransfer db user fileId) >>= \case
        FTRcv RcvFileTransfer {fileStatus = RFSComplete RcvFileInfo {filePath}} -> forward filePath
        FTSnd {fileTransferMeta = FileTransferMeta {filePath}} -> forward filePath
        _ -> throwChatError CEFileNotReceived {fileId}
      where
        forward = processChatCommand . sendCommand chatName
    getGroupAndMemberId :: GroupName -> ContactName -> m (GroupId, GroupMemberId)
    getGroupAndMemberId gName groupMemberName = withUser $ \user -> do
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
      ci <- saveSndChatItem user (CDDirectSnd ct) msg content Nothing Nothing
      toView . CRNewChatItem $ AChatItem SCTDirect SMDSnd (DirectChat ct) ci
      setActive $ ActiveG localDisplayName

assertDirectAllowed :: ChatMonad m => User -> MsgDirection -> Contact -> CMEventTag e -> m ()
assertDirectAllowed user dir ct event =
  unless (allowedChatEvent || anyDirectContact ct) . unlessM directMessagesAllowed $
    throwChatError $ CEDirectMessagesProhibited dir ct
  where
    directMessagesAllowed = any (groupFeatureAllowed' GFDirectMessages) <$> withStore' (\db -> getContactGroupPreferences db user ct)
    allowedChatEvent = case event of
      XMsgNew_ -> False
      XMsgUpdate_ -> False
      XMsgDel_ -> False
      XFile_ -> False
      XGrpInv_ -> False
      XCallInv_ -> False
      _ -> True

setExpireCIs :: (MonadUnliftIO m, MonadReader ChatController m) => Bool -> m ()
setExpireCIs b = do
  expire <- asks expireCIs
  atomically $ writeTVar expire b

deleteFile :: forall m. ChatMonad m => User -> CIFileInfo -> m ()
deleteFile user CIFileInfo {filePath, fileId, fileStatus} =
  (cancel' >> delete) `catchError` (toView . CRChatError)
  where
    cancel' = forM_ fileStatus $ \(AFS dir status) ->
      unless (ciFileEnded status) $
        case dir of
          SMDSnd -> do
            (ftm@FileTransferMeta {cancelled}, fts) <- withStore (\db -> getSndFileTransfer db user fileId)
            unless cancelled $ cancelSndFile user ftm fts
          SMDRcv -> do
            ft@RcvFileTransfer {cancelled} <- withStore (\db -> getRcvFileTransfer db user fileId)
            unless cancelled $ cancelRcvFileTransfer user ft
    delete = withFilesFolder $ \filesFolder ->
      forM_ filePath $ \fPath -> do
        let fsFilePath = filesFolder <> "/" <> fPath
        removeFile fsFilePath `E.catch` \(_ :: E.SomeException) ->
          removePathForcibly fsFilePath `E.catch` \(_ :: E.SomeException) -> pure ()
    -- perform an action only if filesFolder is set (i.e. on mobile devices)
    withFilesFolder :: (FilePath -> m ()) -> m ()
    withFilesFolder action = asks filesFolder >>= readTVarIO >>= mapM_ action

updateCallItemStatus :: ChatMonad m => UserId -> Contact -> Call -> WebRTCCallStatus -> Maybe MessageId -> m ()
updateCallItemStatus userId ct Call {chatItemId} receivedStatus msgId_ = do
  aciContent_ <- callStatusItemContent userId ct chatItemId receivedStatus
  forM_ aciContent_ $ \aciContent -> updateDirectChatItemView userId ct chatItemId aciContent msgId_

updateDirectChatItemView :: ChatMonad m => UserId -> Contact -> ChatItemId -> ACIContent -> Maybe MessageId -> m ()
updateDirectChatItemView userId ct@Contact {contactId} chatItemId (ACIContent msgDir ciContent) msgId_ = do
  updCi <- withStore $ \db -> updateDirectChatItem db userId contactId chatItemId ciContent msgId_
  toView . CRChatItemUpdated $ AChatItem SCTDirect msgDir (DirectChat ct) updCi

callStatusItemContent :: ChatMonad m => UserId -> Contact -> ChatItemId -> WebRTCCallStatus -> m (Maybe ACIContent)
callStatusItemContent userId Contact {contactId} chatItemId receivedStatus = do
  CChatItem msgDir ChatItem {meta = CIMeta {updatedAt}, content} <-
    withStore $ \db -> getDirectChatItem db userId contactId chatItemId
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
toFSFilePath :: ChatMonad m => FilePath -> m FilePath
toFSFilePath f =
  maybe f (<> "/" <> f) <$> (readTVarIO =<< asks filesFolder)

acceptFileReceive :: forall m. ChatMonad m => User -> RcvFileTransfer -> Maybe Bool -> Maybe FilePath -> m AChatItem
acceptFileReceive user@User {userId} RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName = fName, fileConnReq, fileInline, fileSize}, fileStatus, grpMemberId} rcvInline_ filePath_ = do
  unless (fileStatus == RFSNew) $ case fileStatus of
    RFSCancelled _ -> throwChatError $ CEFileCancelled fName
    _ -> pure () -- throwChatError $ CEFileAlreadyReceiving fName
  case fileConnReq of
    -- direct file protocol
    Just connReq -> do
      agentConnId <- withAgent $ \a -> joinConnection a True connReq . directMessage $ XFileAcpt fName
      filePath <- getRcvFilePath fileId filePath_ fName
      withStore $ \db -> acceptRcvFileTransfer db user fileId agentConnId ConnJoined filePath
    -- group & direct file protocol
    Nothing -> do
      chatRef <- withStore $ \db -> getChatRefByFileId db user fileId
      case (chatRef, grpMemberId) of
        (ChatRef CTDirect contactId, Nothing) -> do
          ct <- withStore $ \db -> getContact db user contactId
          (msg, ci) <- acceptFile
          void $ sendDirectContactMessage ct msg
          pure ci
        (ChatRef CTGroup groupId, Just memId) -> do
          GroupMember {activeConn} <- withStore $ \db -> getGroupMember db user groupId memId
          case activeConn of
            Just conn -> do
              (msg, ci) <- acceptFile
              void $ sendDirectMessage conn msg $ GroupId groupId
              pure ci
            _ -> throwChatError $ CEFileInternal "member connection not active"
        _ -> throwChatError $ CEFileInternal "invalid chat ref for file transfer"
  where
    acceptFile :: m (ChatMsgEvent 'Json, AChatItem)
    acceptFile = do
      sharedMsgId <- withStore $ \db -> getSharedMsgIdByFileId db userId fileId
      filePath <- getRcvFilePath fileId filePath_ fName
      inline <- receiveInline
      if
          | inline -> do
            -- accepting inline
            ci <- withStore $ \db -> acceptRcvInlineFT db user fileId filePath
            pure (XFileAcptInv sharedMsgId Nothing fName, ci)
          | fileInline == Just IFMSent -> throwChatError $ CEFileAlreadyReceiving fName
          | otherwise -> do
            -- accepting via a new connection
            (agentConnId, fileInvConnReq) <- withAgent $ \a -> createConnection a True SCMInvitation Nothing
            ci <- withStore $ \db -> acceptRcvFileTransfer db user fileId agentConnId ConnNew filePath
            pure (XFileAcptInv sharedMsgId (Just fileInvConnReq) fName, ci)
    receiveInline :: m Bool
    receiveInline = do
      ChatConfig {fileChunkSize, inlineFiles = InlineFilesConfig {receiveChunks, offerChunks}} <- asks config
      pure $
        rcvInline_ /= Just False
          && fileInline == Just IFMOffer
          && ( fileSize <= fileChunkSize * receiveChunks
                 || (rcvInline_ == Just True && fileSize <= fileChunkSize * offerChunks)
             )

getRcvFilePath :: forall m. ChatMonad m => FileTransferId -> Maybe FilePath -> String -> m FilePath
getRcvFilePath fileId fPath_ fn = case fPath_ of
  Nothing ->
    asks filesFolder >>= readTVarIO >>= \case
      Nothing -> do
        dir <- (`combine` "Downloads") <$> getHomeDirectory
        ifM (doesDirectoryExist dir) (pure dir) getTemporaryDirectory
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
    createEmptyFile fPath = emptyFile fPath `E.catch` (throwChatError . CEFileWrite fPath . (show :: E.SomeException -> String))
    emptyFile :: FilePath -> m FilePath
    emptyFile fPath = do
      h <- getFileHandle fileId fPath rcvFiles AppendMode
      liftIO $ B.hPut h "" >> hFlush h
      pure fPath
    uniqueCombine :: FilePath -> String -> m FilePath
    uniqueCombine filePath fileName = tryCombine (0 :: Int)
      where
        tryCombine n =
          let (name, ext) = splitExtensions fileName
              suffix = if n == 0 then "" else "_" <> show n
              f = filePath `combine` (name <> suffix <> ext)
           in ifM (doesFileExist f) (tryCombine $ n + 1) (pure f)

acceptContactRequest :: ChatMonad m => User -> UserContactRequest -> Maybe IncognitoProfile -> m Contact
acceptContactRequest user UserContactRequest {agentInvitationId = AgentInvId invId, localDisplayName = cName, profileId, profile = cp, userContactLinkId, xContactId} incognitoProfile = do
  let profileToSend = profileToSendOnAccept user incognitoProfile
  acId <- withAgent $ \a -> acceptContact a True invId . directMessage $ XInfo profileToSend
  withStore' $ \db -> createAcceptedContact db user acId cName profileId cp userContactLinkId xContactId incognitoProfile

acceptContactRequestAsync :: ChatMonad m => User -> UserContactRequest -> Maybe IncognitoProfile -> m Contact
acceptContactRequestAsync user UserContactRequest {agentInvitationId = AgentInvId invId, localDisplayName = cName, profileId, profile = p, userContactLinkId, xContactId} incognitoProfile = do
  let profileToSend = profileToSendOnAccept user incognitoProfile
  (cmdId, acId) <- agentAcceptContactAsync user True invId $ XInfo profileToSend
  withStore' $ \db -> do
    ct@Contact {activeConn = Connection {connId}} <- createAcceptedContact db user acId cName profileId p userContactLinkId xContactId incognitoProfile
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
  deleteAgentConnectionAsync user conn `catchError` \_ -> pure ()
  withStore' $ \db -> deleteGroupLink db user gInfo

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
agentSubscriber = do
  q <- asks $ subQ . smpAgent
  l <- asks chatLock
  forever $ do
    (corrId, connId, msg) <- atomically $ readTBQueue q
    u <- readTVarIO =<< asks currentUser
    let name = "agentSubscriber connId=" <> str connId <> " corrId=" <> str corrId <> " msg=" <> str (aCommandTag msg)
    withLock l name . void . runExceptT $
      processAgentMessage u corrId connId msg `catchError` (toView . CRChatError)
  where
    str :: StrEncoding a => a -> String
    str = B.unpack . strEncode

type AgentBatchSubscribe m = AgentClient -> [ConnId] -> ExceptT AgentErrorType m (Map ConnId (Either AgentErrorType ()))

subscribeUserConnections :: forall m. ChatMonad m => AgentBatchSubscribe m -> User -> m ()
subscribeUserConnections agentBatchSubscribe user = do
  -- get user connections
  ce <- asks $ subscriptionEvents . config
  (ctConns, cts) <- getContactConns
  (ucConns, ucs) <- getUserContactLinkConns
  (gs, mConns, ms) <- getGroupMemberConns
  (sftConns, sfts) <- getSndFileTransferConns
  (rftConns, rfts) <- getRcvFileTransferConns
  (pcConns, pcs) <- getPendingContactConns
  -- subscribe using batched commands
  rs <- withAgent (`agentBatchSubscribe` concat [ctConns, ucConns, mConns, sftConns, rftConns, pcConns])
  -- send connection events to view
  contactSubsToView rs cts
  contactLinkSubsToView rs ucs
  groupSubsToView rs gs ms ce
  sndFileSubsToView rs sfts
  rcvFileSubsToView rs rfts
  pendingConnSubsToView rs pcs
  where
    getContactConns :: m ([ConnId], Map ConnId Contact)
    getContactConns = do
      cts <- withStore_ getUserContacts
      let connIds = map contactConnId cts
      pure (connIds, M.fromList $ zip connIds cts)
    getUserContactLinkConns :: m ([ConnId], Map ConnId UserContact)
    getUserContactLinkConns = do
      (cs, ucs) <- unzip <$> withStore_ getUserContactLinks
      let connIds = map aConnId cs
      pure (connIds, M.fromList $ zip connIds ucs)
    getGroupMemberConns :: m ([Group], [ConnId], Map ConnId GroupMember)
    getGroupMemberConns = do
      gs <- withStore_ getUserGroups
      let mPairs = concatMap (\(Group _ ms) -> mapMaybe (\m -> (,m) <$> memberConnId m) ms) gs
      pure (gs, map fst mPairs, M.fromList mPairs)
    getSndFileTransferConns :: m ([ConnId], Map ConnId SndFileTransfer)
    getSndFileTransferConns = do
      sfts <- withStore_ getLiveSndFileTransfers
      let connIds = map sndFileTransferConnId sfts
      pure (connIds, M.fromList $ zip connIds sfts)
    getRcvFileTransferConns :: m ([ConnId], Map ConnId RcvFileTransfer)
    getRcvFileTransferConns = do
      rfts <- withStore_ getLiveRcvFileTransfers
      let rftPairs = mapMaybe (\ft -> (,ft) <$> liveRcvFileTransferConnId ft) rfts
      pure (map fst rftPairs, M.fromList rftPairs)
    getPendingContactConns :: m ([ConnId], Map ConnId PendingContactConnection)
    getPendingContactConns = do
      pcs <- withStore_ getPendingContactConnections
      let connIds = map aConnId' pcs
      pure (connIds, M.fromList $ zip connIds pcs)
    contactSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId Contact -> m ()
    contactSubsToView rs = toView . CRContactSubSummary . map (uncurry ContactSubStatus) . resultsFor rs
    contactLinkSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId UserContact -> m ()
    contactLinkSubsToView rs = toView . CRUserContactSubSummary . map (uncurry UserContactSubStatus) . resultsFor rs
    groupSubsToView :: Map ConnId (Either AgentErrorType ()) -> [Group] -> Map ConnId GroupMember -> Bool -> m ()
    groupSubsToView rs gs ms ce = do
      mapM_ groupSub $
        sortOn (\(Group GroupInfo {localDisplayName = g} _) -> g) gs
      toView . CRMemberSubSummary $ map (uncurry MemberSubStatus) mRs
      where
        mRs = resultsFor rs ms
        groupSub :: Group -> m ()
        groupSub (Group g@GroupInfo {membership, groupId = gId} members) = do
          when ce $ mapM_ (toView . uncurry (CRMemberSubError g)) mErrors
          toView groupEvent
          where
            mErrors :: [(GroupMember, ChatError)]
            mErrors =
              sortOn (\(GroupMember {localDisplayName = n}, _) -> n)
                . filterErrors
                $ filter (\(GroupMember {groupId}, _) -> groupId == gId) mRs
            groupEvent :: ChatResponse
            groupEvent
              | memberStatus membership == GSMemInvited = CRGroupInvitation g
              | all (\GroupMember {activeConn} -> isNothing activeConn) members =
                if memberActive membership
                  then CRGroupEmpty g
                  else CRGroupRemoved g
              | otherwise = CRGroupSubscribed g
    sndFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId SndFileTransfer -> m ()
    sndFileSubsToView rs sfts = do
      let sftRs = resultsFor rs sfts
      forM_ sftRs $ \(ft@SndFileTransfer {fileId, fileStatus}, err_) -> do
        forM_ err_ $ toView . CRSndFileSubError ft
        void . forkIO $ do
          threadDelay 1000000
          l <- asks chatLock
          when (fileStatus == FSConnected) . unlessM (isFileActive fileId sndFiles) . withLock l "subscribe sendFileChunk" $
            sendFileChunk user ft
    rcvFileSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId RcvFileTransfer -> m ()
    rcvFileSubsToView rs = mapM_ (toView . uncurry CRRcvFileSubError) . filterErrors . resultsFor rs
    pendingConnSubsToView :: Map ConnId (Either AgentErrorType ()) -> Map ConnId PendingContactConnection -> m ()
    pendingConnSubsToView rs = toView . CRPendingSubSummary . map (uncurry PendingSubStatus) . resultsFor rs
    withStore_ :: (DB.Connection -> User -> IO [a]) -> m [a]
    withStore_ a = withStore' (`a` user) `catchError` \_ -> pure []
    filterErrors :: [(a, Maybe ChatError)] -> [(a, ChatError)]
    filterErrors = mapMaybe (\(a, e_) -> (a,) <$> e_)
    resultsFor :: Map ConnId (Either AgentErrorType ()) -> Map ConnId a -> [(a, Maybe ChatError)]
    resultsFor rs = M.foldrWithKey' addResult []
      where
        addResult :: ConnId -> a -> [(a, Maybe ChatError)] -> [(a, Maybe ChatError)]
        addResult connId = (:) . (,err)
          where
            err = case M.lookup connId rs of
              Just (Left e) -> Just $ ChatErrorAgent e
              Just _ -> Nothing
              _ -> Just . ChatError . CEAgentNoSubResult $ AgentConnId connId

expireChatItems :: forall m. ChatMonad m => User -> Int64 -> Bool -> m ()
expireChatItems user ttl sync = do
  currentTs <- liftIO getCurrentTime
  let expirationDate = addUTCTime (-1 * fromIntegral ttl) currentTs
      -- this is to keep group messages created during last 12 hours even if they're expired according to item_ts
      createdAtCutoff = addUTCTime (-43200 :: NominalDiffTime) currentTs
  expire <- asks expireCIs
  contacts <- withStore' (`getUserContacts` user)
  loop expire contacts $ processContact expirationDate
  groups <- withStore' (`getUserGroupDetails` user)
  loop expire groups $ processGroup expirationDate createdAtCutoff
  where
    loop :: TVar Bool -> [a] -> (a -> m ()) -> m ()
    loop _ [] _ = pure ()
    loop expire (a : as) process = continue expire $ do
      process a `catchError` (toView . CRChatError)
      loop expire as process
    continue :: TVar Bool -> m () -> m ()
    continue expire = if sync then id else \a -> whenM (readTVarIO expire) $ threadDelay 100000 >> a
    processContact :: UTCTime -> Contact -> m ()
    processContact expirationDate ct = do
      filesInfo <- withStore' $ \db -> getContactExpiredFileInfo db user ct expirationDate
      maxItemTs_ <- withStore' $ \db -> getContactMaxItemTs db user ct
      forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
      withStore' $ \db -> deleteContactExpiredCIs db user ct expirationDate
      withStore' $ \db -> do
        ciCount_ <- getContactCICount db user ct
        case (maxItemTs_, ciCount_) of
          (Just ts, Just count) -> when (count == 0) $ updateContactTs db user ct ts
          _ -> pure ()
    processGroup :: UTCTime -> UTCTime -> GroupInfo -> m ()
    processGroup expirationDate createdAtCutoff gInfo = do
      filesInfo <- withStore' $ \db -> getGroupExpiredFileInfo db user gInfo expirationDate createdAtCutoff
      maxItemTs_ <- withStore' $ \db -> getGroupMaxItemTs db user gInfo
      forM_ filesInfo $ \fileInfo -> deleteFile user fileInfo
      withStore' $ \db -> deleteGroupExpiredCIs db user gInfo expirationDate createdAtCutoff
      membersToDelete <- withStore' $ \db -> getGroupMembersForExpiration db user gInfo
      forM_ membersToDelete $ \m -> withStore' $ \db -> deleteGroupMember db user m
      withStore' $ \db -> do
        ciCount_ <- getGroupCICount db user gInfo
        case (maxItemTs_, ciCount_) of
          (Just ts, Just count) -> when (count == 0) $ updateGroupTs db user gInfo ts
          _ -> pure ()

processAgentMessage :: forall m. ChatMonad m => Maybe User -> ConnId -> ACorrId -> ACommand 'Agent -> m ()
processAgentMessage Nothing _ _ _ = throwChatError CENoActiveUser
processAgentMessage (Just User {userId}) _ "" agentMessage = case agentMessage of
  CONNECT p h -> hostEvent $ CRHostConnected p h
  DISCONNECT p h -> hostEvent $ CRHostDisconnected p h
  DOWN srv conns -> serverEvent srv conns CRContactsDisconnected "disconnected"
  UP srv conns -> serverEvent srv conns CRContactsSubscribed "connected"
  SUSPENDED -> toView CRChatSuspended
  _ -> pure ()
  where
    hostEvent = whenM (asks $ hostEvents . config) . toView
    serverEvent srv@(SMPServer host _ _) conns event str = do
      cs <- withStore' $ \db -> getConnectionsContacts db userId conns
      toView $ event srv cs
      showToast ("server " <> str) (safeDecodeUtf8 $ strEncode host)
processAgentMessage (Just user) _ agentConnId END =
  withStore (\db -> getConnectionEntity db user $ AgentConnId agentConnId) >>= \case
    RcvDirectMsgConnection _ (Just ct@Contact {localDisplayName = c}) -> do
      toView $ CRContactAnotherClient ct
      showToast (c <> "> ") "connected to another client"
      unsetActive $ ActiveC c
    entity -> toView $ CRSubscriptionEnd entity
processAgentMessage (Just user@User {userId}) corrId agentConnId agentMessage =
  (withStore (\db -> getConnectionEntity db user $ AgentConnId agentConnId) >>= updateConnStatus) >>= \case
    RcvDirectMsgConnection conn contact_ ->
      processDirectMessage agentMessage conn contact_
    RcvGroupMsgConnection conn gInfo m ->
      processGroupMessage agentMessage conn gInfo m
    RcvFileConnection conn ft ->
      processRcvFileConn agentMessage conn ft
    SndFileConnection conn ft ->
      processSndFileConn agentMessage conn ft
    UserContactConnection conn uc ->
      processUserContactRequest agentMessage conn uc
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

    agentMsgConnStatus :: ACommand 'Agent -> Maybe ConnStatus
    agentMsgConnStatus = \case
      CONF {} -> Just ConnRequested
      INFO _ -> Just ConnSndReady
      CON -> Just ConnReady
      _ -> Nothing

    processDirectMessage :: ACommand 'Agent -> Connection -> Maybe Contact -> m ()
    processDirectMessage agentMsg conn@Connection {connId, viaUserContactLink, groupLinkId, customUserProfileId} = \case
      Nothing -> case agentMsg of
        CONF confId _ connInfo -> do
          -- [incognito] send saved profile
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
          let profileToSend = userProfileToSend user (fromLocalProfile <$> incognitoProfile) Nothing
          saveConnInfo conn connInfo
          -- [async agent commands] no continuation needed, but command should be asynchronous for stability
          allowAgentConnectionAsync user conn confId $ XInfo profileToSend
        INFO connInfo ->
          saveConnInfo conn connInfo
        MSG meta _msgFlags msgBody -> do
          cmdId <- createAckCmd conn
          _ <- saveRcvMSG conn (ConnectionId connId) meta msgBody cmdId
          withAckMessage agentConnId cmdId meta $ pure ()
        SENT msgId ->
          -- ? updateDirectChatItemStatus
          sentMsgDeliveryEvent conn msgId
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \CommandData {cmdFunction, cmdId} ->
            when (cmdFunction == CFAckMessage) $ ackMsgDeliveryEvent conn cmdId
        MERR _ err -> toView . CRChatError $ ChatErrorAgent err -- ? updateDirectChatItemStatus
        ERR err -> do
          toView . CRChatError $ ChatErrorAgent err
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
                  sendXGrpMemInv hostConnId directConnReq xGrpMemIntroCont
              CRContactUri _ -> throwChatError $ CECommandError "unexpected ConnectionRequestUri type"
        MSG msgMeta _msgFlags msgBody -> do
          cmdId <- createAckCmd conn
          withAckMessage agentConnId cmdId msgMeta $ do
            msg@RcvMessage {chatMsgEvent = ACME _ event} <- saveRcvMSG conn (ConnectionId connId) msgMeta msgBody cmdId
            assertDirectAllowed user MDRcv ct $ toCMEventTag event
            updateChatLock "directMessage" event
            case event of
              XMsgNew mc -> newContentMessage ct mc msg msgMeta
              XMsgUpdate sharedMsgId mContent -> messageUpdate ct sharedMsgId mContent msg msgMeta
              XMsgDel sharedMsgId -> messageDelete ct sharedMsgId msg msgMeta
              -- TODO discontinue XFile
              XFile fInv -> processFileInvitation' ct fInv msg msgMeta
              XFileCancel sharedMsgId -> xFileCancel ct sharedMsgId msgMeta
              XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInv ct sharedMsgId fileConnReq_ fName msgMeta
              XInfo p -> xInfo ct p
              XGrpInv gInv -> processGroupInvitation ct gInv msg msgMeta
              XInfoProbe probe -> xInfoProbe ct probe
              XInfoProbeCheck probeHash -> xInfoProbeCheck ct probeHash
              XInfoProbeOk probe -> xInfoProbeOk ct probe
              XCallInv callId invitation -> xCallInv ct callId invitation msg msgMeta
              XCallOffer callId offer -> xCallOffer ct callId offer msg msgMeta
              XCallAnswer callId answer -> xCallAnswer ct callId answer msg msgMeta
              XCallExtra callId extraInfo -> xCallExtra ct callId extraInfo msg msgMeta
              XCallEnd callId -> xCallEnd ct callId msg msgMeta
              BFileChunk sharedMsgId chunk -> bFileChunk ct sharedMsgId chunk msgMeta
              _ -> messageError $ "unsupported message: " <> T.pack (show event)
        CONF confId _ connInfo -> do
          -- confirming direct connection with a member
          ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
          case chatMsgEvent of
            XGrpMemInfo _memId _memProfile -> do
              -- TODO check member ID
              -- TODO update member profile
              -- [async agent commands] no continuation needed, but command should be asynchronous for stability
              allowAgentConnectionAsync user conn confId XOk
            _ -> messageError "CONF from member must have x.grp.mem.info"
        INFO connInfo -> do
          ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
          case chatMsgEvent of
            XGrpMemInfo _memId _memProfile -> do
              -- TODO check member ID
              -- TODO update member profile
              pure ()
            XInfo _profile -> do
              -- TODO update contact profile
              pure ()
            XOk -> pure ()
            _ -> messageError "INFO for existing contact must have x.grp.mem.info, x.info or x.ok"
        CON ->
          withStore' (\db -> getViaGroupMember db user ct) >>= \case
            Nothing -> do
              -- [incognito] print incognito profile used for this contact
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
              toView $ CRContactConnected ct (fmap fromLocalProfile incognitoProfile)
              when (directContact ct) $ createFeatureEnabledItems ct
              setActive $ ActiveC c
              showToast (c <> "> ") "connected"
              forM_ groupLinkId $ \_ -> probeMatchingContacts ct $ contactConnIncognito ct
              forM_ viaUserContactLink $ \userContactLinkId ->
                withStore' (\db -> getUserContactLinkById db userId userContactLinkId) >>= \case
                  Just (UserContactLink {autoAccept = Just AutoAccept {autoReply = mc_}}, groupId_) -> do
                    forM_ mc_ $ \mc -> do
                      (msg, _) <- sendDirectContactMessage ct (XMsgNew $ MCSimple (ExtMsgContent mc Nothing))
                      ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc) Nothing Nothing
                      toView . CRNewChatItem $ AChatItem SCTDirect SMDSnd (DirectChat ct) ci
                    forM_ groupId_ $ \groupId -> do
                      gVar <- asks idsDrg
                      groupConnIds <- createAgentConnectionAsync user CFCreateConnGrpInv True SCMInvitation
                      withStore $ \db -> createNewContactMemberAsync db gVar user groupId ct GRMember groupConnIds
                  _ -> pure ()
            Just (gInfo@GroupInfo {membership}, m@GroupMember {activeConn}) ->
              when (maybe False ((== ConnReady) . connStatus) activeConn) $ do
                notifyMemberConnected gInfo m
                let connectedIncognito = contactConnIncognito ct || memberIncognito membership
                when (memberCategory m == GCPreMember) $ probeMatchingContacts ct connectedIncognito
        SENT msgId -> do
          sentMsgDeliveryEvent conn msgId
          checkSndInlineFTComplete conn msgId
          withStore' (\db -> getDirectChatItemByAgentMsgId db userId contactId connId msgId) >>= \case
            Just (CChatItem SMDSnd ci) -> do
              chatItem <- withStore $ \db -> updateDirectChatItemStatus db userId contactId (chatItemId' ci) CISSndSent
              toView $ CRChatItemStatusUpdated (AChatItem SCTDirect SMDSnd (DirectChat ct) chatItem)
            _ -> pure ()
        SWITCH qd phase cStats -> do
          toView . CRContactSwitch ct $ SwitchProgress qd phase cStats
          when (phase /= SPConfirmed) $ case qd of
            QDRcv -> createInternalChatItem user (CDDirectSnd ct) (CISndConnEvent $ SCESwitchQueue phase Nothing) Nothing
            QDSnd -> createInternalChatItem user (CDDirectRcv ct) (CIRcvConnEvent $ RCESwitchQueue phase) Nothing
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \CommandData {cmdFunction, cmdId} ->
            when (cmdFunction == CFAckMessage) $ ackMsgDeliveryEvent conn cmdId
        MERR msgId err -> do
          chatItemId_ <- withStore' $ \db -> getChatItemIdByAgentMsgId db connId msgId
          forM_ chatItemId_ $ \chatItemId -> do
            chatItem <- withStore $ \db -> updateDirectChatItemStatus db userId contactId chatItemId (agentErrToItemStatus err)
            toView $ CRChatItemStatusUpdated (AChatItem SCTDirect SMDSnd (DirectChat ct) chatItem)
        ERR err -> do
          toView . CRChatError $ ChatErrorAgent err
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    processGroupMessage :: ACommand 'Agent -> Connection -> GroupInfo -> GroupMember -> m ()
    processGroupMessage agentMsg conn@Connection {connId} gInfo@GroupInfo {groupId, localDisplayName = gName, groupProfile, membership, chatSettings} m = case agentMsg of
      INV (ACR _ cReq) ->
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cReq of
            groupConnReq@(CRInvitationUri _ _) -> case cmdFunction of
              -- [async agent commands] XGrpMemIntro continuation on receiving INV
              CFCreateConnGrpMemInv -> do
                contData <- withStore' $ \db -> do
                  setConnConnReqInv db user connId cReq
                  getXGrpMemIntroContGroup db user m
                forM_ contData $ \(hostConnId, directConnReq) -> do
                  let GroupMember {groupMemberId, memberId} = m
                  sendXGrpMemInv hostConnId directConnReq XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq}
              -- [async agent commands] group link auto-accept continuation on receiving INV
              CFCreateConnGrpInv ->
                withStore' (\db -> getContactViaMember db user m) >>= \case
                  Nothing -> messageError "implementation error: invitee does not have contact"
                  Just ct -> do
                    withStore' $ \db -> setNewContactMemberConnRequest db user m cReq
                    groupLinkId <- withStore' $ \db -> getGroupLinkId db user gInfo
                    sendGrpInvitation ct m groupLinkId
                    toView $ CRSentGroupInvitation gInfo ct m
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
        ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
        case memberCategory m of
          GCInviteeMember ->
            case chatMsgEvent of
              XGrpAcpt memId
                | sameMemberId memId m -> do
                  withStore $ \db -> liftIO $ updateGroupMemberStatus db userId m GSMemAccepted
                  -- [async agent commands] no continuation needed, but command should be asynchronous for stability
                  allowAgentConnectionAsync user conn confId XOk
                | otherwise -> messageError "x.grp.acpt: memberId is different from expected"
              _ -> messageError "CONF from invited member must have x.grp.acpt"
          _ ->
            case chatMsgEvent of
              XGrpMemInfo memId _memProfile
                | sameMemberId memId m -> do
                  -- TODO update member profile
                  -- [async agent commands] no continuation needed, but command should be asynchronous for stability
                  allowAgentConnectionAsync user conn confId $ XGrpMemInfo (memberId (membership :: GroupMember)) (fromLocalProfile $ memberProfile membership)
                | otherwise -> messageError "x.grp.mem.info: memberId is different from expected"
              _ -> messageError "CONF from member must have x.grp.mem.info"
      INFO connInfo -> do
        ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
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
        sendPendingGroupMessages m conn
        withAgent $ \a -> toggleConnectionNtfs a (aConnId conn) $ enableNtfs chatSettings
        case memberCategory m of
          GCHostMember -> do
            toView $ CRUserJoinedGroup gInfo {membership = membership {memberStatus = GSMemConnected}} m {memberStatus = GSMemConnected}
            createGroupFeatureItems gInfo m
            memberConnectedChatItem gInfo m
            setActive $ ActiveG gName
            showToast ("#" <> gName) "you are connected to group"
          GCInviteeMember -> do
            memberConnectedChatItem gInfo m
            toView $ CRJoinedGroupMember gInfo m {memberStatus = GSMemConnected}
            setActive $ ActiveG gName
            showToast ("#" <> gName) $ "member " <> localDisplayName (m :: GroupMember) <> " is connected"
            intros <- withStore' $ \db -> createIntroductions db members m
            void . sendGroupMessage gInfo members . XGrpMemNew $ memberInfo m
            forM_ intros $ \intro@GroupMemberIntro {introId} -> do
              void $ sendDirectMessage conn (XGrpMemIntro . memberInfo $ reMember intro) (GroupId groupId)
              withStore' $ \db -> updateIntroStatus db introId GMIntroSent
          _ -> do
            -- TODO send probe and decide whether to use existing contact connection or the new contact connection
            -- TODO notify member who forwarded introduction - question - where it is stored? There is via_contact but probably there should be via_member in group_members table
            withStore' (\db -> getViaGroupContact db user m) >>= \case
              Nothing -> do
                notifyMemberConnected gInfo m
                messageWarning "connected member does not have contact"
              Just ct@Contact {activeConn = Connection {connStatus}} ->
                when (connStatus == ConnReady) $ do
                  notifyMemberConnected gInfo m
                  let connectedIncognito = contactConnIncognito ct || memberIncognito membership
                  when (memberCategory m == GCPreMember) $ probeMatchingContacts ct connectedIncognito
      MSG msgMeta _msgFlags msgBody -> do
        cmdId <- createAckCmd conn
        msg@RcvMessage {chatMsgEvent = ACME _ event} <- saveRcvMSG conn (GroupId groupId) msgMeta msgBody cmdId
        updateChatLock "groupMessage" event
        withAckMessage agentConnId cmdId msgMeta $
          case event of
            XMsgNew mc -> newGroupContentMessage gInfo m mc msg msgMeta
            XMsgUpdate sharedMsgId mContent -> groupMessageUpdate gInfo m sharedMsgId mContent msg msgMeta
            XMsgDel sharedMsgId -> groupMessageDelete gInfo m sharedMsgId msg
            -- TODO discontinue XFile
            XFile fInv -> processGroupFileInvitation' gInfo m fInv msg msgMeta
            XFileCancel sharedMsgId -> xFileCancelGroup gInfo m sharedMsgId msgMeta
            XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInvGroup gInfo m sharedMsgId fileConnReq_ fName msgMeta
            XGrpMemNew memInfo -> xGrpMemNew gInfo m memInfo msg msgMeta
            XGrpMemIntro memInfo -> xGrpMemIntro gInfo m memInfo
            XGrpMemInv memId introInv -> xGrpMemInv gInfo m memId introInv
            XGrpMemFwd memInfo introInv -> xGrpMemFwd gInfo m memInfo introInv
            XGrpMemRole memId memRole -> xGrpMemRole gInfo m memId memRole msg msgMeta
            XGrpMemDel memId -> xGrpMemDel gInfo m memId msg msgMeta
            XGrpLeave -> xGrpLeave gInfo m msg msgMeta
            XGrpDel -> xGrpDel gInfo m msg msgMeta
            XGrpInfo p' -> xGrpInfo gInfo m p' msg msgMeta
            BFileChunk sharedMsgId chunk -> bFileChunkGroup gInfo sharedMsgId chunk msgMeta
            _ -> messageError $ "unsupported message: " <> T.pack (show event)
      SENT msgId -> do
        sentMsgDeliveryEvent conn msgId
        checkSndInlineFTComplete conn msgId
      SWITCH qd phase cStats -> do
        toView . CRGroupMemberSwitch gInfo m $ SwitchProgress qd phase cStats
        when (phase /= SPConfirmed) $ case qd of
          QDRcv -> createInternalChatItem user (CDGroupSnd gInfo) (CISndConnEvent . SCESwitchQueue phase . Just $ groupMemberRef m) Nothing
          QDSnd -> createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvConnEvent $ RCESwitchQueue phase) Nothing
      OK ->
        -- [async agent commands] continuation on receiving OK
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction, cmdId} ->
          when (cmdFunction == CFAckMessage) $ ackMsgDeliveryEvent conn cmdId
      MERR _ err -> toView . CRChatError $ ChatErrorAgent err
      ERR err -> do
        toView . CRChatError $ ChatErrorAgent err
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()

    processSndFileConn :: ACommand 'Agent -> Connection -> SndFileTransfer -> m ()
    processSndFileConn agentMsg conn ft@SndFileTransfer {fileId, fileName, fileStatus} =
      case agentMsg of
        -- SMP CONF for SndFileConnection happens for direct file protocol
        -- when recipient of the file "joins" connection created by the sender
        CONF confId _ connInfo -> do
          ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
          case chatMsgEvent of
            -- TODO save XFileAcpt message
            XFileAcpt name
              | name == fileName -> do
                withStore' $ \db -> updateSndFileStatus db ft FSAccepted
                -- [async agent commands] no continuation needed, but command should be asynchronous for stability
                allowAgentConnectionAsync user conn confId XOk
              | otherwise -> messageError "x.file.acpt: fileName is different from expected"
            _ -> messageError "CONF from file connection must have x.file.acpt"
        CON -> do
          ci <- withStore $ \db -> do
            liftIO $ updateSndFileStatus db ft FSConnected
            updateDirectCIFileStatus db user fileId CIFSSndTransfer
          toView $ CRSndFileStart ci ft
          sendFileChunk user ft
        SENT msgId -> do
          withStore' $ \db -> updateSndFileChunkSent db ft msgId
          unless (fileStatus == FSCancelled) $ sendFileChunk user ft
        MERR _ err -> do
          cancelSndFileTransfer user ft
          case err of
            SMP SMP.AUTH -> unless (fileStatus == FSCancelled) $ do
              ci <- withStore $ \db -> getChatItemByFileId db user fileId
              toView $ CRSndFileRcvCancelled ci ft
            _ -> throwChatError $ CEFileSend fileId err
        MSG meta _ _ -> do
          cmdId <- createAckCmd conn
          withAckMessage agentConnId cmdId meta $ pure ()
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        ERR err -> do
          toView . CRChatError $ ChatErrorAgent err
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    processRcvFileConn :: ACommand 'Agent -> Connection -> RcvFileTransfer -> m ()
    processRcvFileConn agentMsg conn ft =
      case agentMsg of
        -- SMP CONF for RcvFileConnection happens for group file protocol
        -- when sender of the file "joins" connection created by the recipient
        -- (sender doesn't create connections for all group members)
        CONF confId _ connInfo -> do
          ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
          case chatMsgEvent of
            XOk -> allowAgentConnectionAsync user conn confId XOk -- [async agent commands] no continuation needed, but command should be asynchronous for stability
            _ -> pure ()
        CON -> startReceivingFile ft
        MSG meta _ msgBody -> do
          cmdId <- createAckCmd conn
          withAckMessage agentConnId cmdId meta $
            parseFileChunk msgBody >>= receiveFileChunk ft (Just conn) meta
        OK ->
          -- [async agent commands] continuation on receiving OK
          withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        MERR _ err -> toView . CRChatError $ ChatErrorAgent err
        ERR err -> do
          toView . CRChatError $ ChatErrorAgent err
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    startReceivingFile :: RcvFileTransfer -> m ()
    startReceivingFile ft@RcvFileTransfer {fileId} = do
      ci <- withStore $ \db -> do
        liftIO $ updateRcvFileStatus db ft FSConnected
        liftIO $ updateCIFileStatus db user fileId CIFSRcvTransfer
        getChatItemByFileId db user fileId
      toView $ CRRcvFileStart ci

    receiveFileChunk :: RcvFileTransfer -> Maybe Connection -> MsgMeta -> FileChunk -> m ()
    receiveFileChunk ft@RcvFileTransfer {fileId, chunkSize, cancelled} conn_ MsgMeta {recipient = (msgId, _), integrity} = \case
      FileChunkCancel ->
        unless cancelled $ do
          cancelRcvFileTransfer user ft
          toView (CRRcvFileSndCancelled ft)
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
              else appendFileChunk ft chunkNo chunk
          RcvChunkFinal ->
            if B.length chunk > fromInteger chunkSize
              then badRcvFileChunk ft "incorrect chunk size"
              else do
                appendFileChunk ft chunkNo chunk
                ci <- withStore $ \db -> do
                  liftIO $ do
                    updateRcvFileStatus db ft FSComplete
                    updateCIFileStatus db user fileId CIFSRcvComplete
                    deleteRcvFileChunks db ft
                  getChatItemByFileId db user fileId
                toView $ CRRcvFileComplete ci
                closeFileHandle fileId rcvFiles
                mapM_ (deleteAgentConnectionAsync user) conn_
          RcvChunkDuplicate -> pure ()
          RcvChunkError -> badRcvFileChunk ft $ "incorrect chunk number " <> show chunkNo

    processUserContactRequest :: ACommand 'Agent -> Connection -> UserContact -> m ()
    processUserContactRequest agentMsg conn UserContact {userContactLinkId} = case agentMsg of
      REQ invId _ connInfo -> do
        ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
        case chatMsgEvent of
          XContact p xContactId_ -> profileContactRequest invId p xContactId_
          XInfo p -> profileContactRequest invId p Nothing
          -- TODO show/log error, other events in contact request
          _ -> pure ()
      MERR _ err -> toView . CRChatError $ ChatErrorAgent err
      ERR err -> do
        toView . CRChatError $ ChatErrorAgent err
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()
      where
        profileContactRequest :: InvitationId -> Profile -> Maybe XContactId -> m ()
        profileContactRequest invId p xContactId_ = do
          withStore (\db -> createOrUpdateContactRequest db user userContactLinkId invId p xContactId_) >>= \case
            CORContact contact -> toView $ CRContactRequestAlreadyAccepted contact
            CORRequest cReq@UserContactRequest {localDisplayName} -> do
              withStore' (\db -> getUserContactLinkById db userId userContactLinkId) >>= \case
                Just (UserContactLink {autoAccept}, groupId_) ->
                  case autoAccept of
                    Just AutoAccept {acceptIncognito} -> case groupId_ of
                      Nothing -> do
                        -- [incognito] generate profile to send, create connection with incognito profile
                        incognitoProfile <- if acceptIncognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
                        ct <- acceptContactRequestAsync user cReq incognitoProfile
                        toView $ CRAcceptingContactRequest ct
                      Just groupId -> do
                        gInfo@GroupInfo {membership = membership@GroupMember {memberProfile}} <- withStore $ \db -> getGroupInfo db user groupId
                        let profileMode = if memberIncognito membership then Just $ ExistingIncognito memberProfile else Nothing
                        ct <- acceptContactRequestAsync user cReq profileMode
                        toView $ CRAcceptingGroupJoinRequest gInfo ct
                    _ -> do
                      toView $ CRReceivedContactRequest cReq
                      showToast (localDisplayName <> "> ") "wants to connect to you"
                _ -> pure ()

    updateChatLock :: MsgEncodingI e => String -> ChatMsgEvent e -> m ()
    updateChatLock name event = do
      l <- asks chatLock
      atomically $ tryReadTMVar l >>= mapM_ (swapTMVar l . (<> s))
      where
        s = " " <> name <> "=" <> B.unpack (strEncode $ toCMEventTag event)

    withCompletedCommand :: Connection -> ACommand 'Agent -> (CommandData -> m ()) -> m ()
    withCompletedCommand Connection {connId} agentMsg action = do
      let agentMsgTag = aCommandTag agentMsg
      cmdData_ <- withStore' $ \db -> getCommandDataByCorrId db user corrId
      case cmdData_ of
        Just cmdData@CommandData {cmdId, cmdConnId = Just cmdConnId', cmdFunction}
          | connId == cmdConnId' && (agentMsgTag == commandExpectedResponse cmdFunction || agentMsgTag == ERR_) -> do
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

    withAckMessage :: ConnId -> CommandId -> MsgMeta -> m () -> m ()
    withAckMessage cId cmdId MsgMeta {recipient = (msgId, _)} action =
      -- [async agent commands] command should be asynchronous, continuation is ackMsgDeliveryEvent
      action `E.finally` withAgent (\a -> ackMessageAsync a (aCorrId cmdId) cId msgId `catchError` \_ -> pure ())

    ackMsgDeliveryEvent :: Connection -> CommandId -> m ()
    ackMsgDeliveryEvent Connection {connId} ackCmdId =
      withStore' $ \db -> createRcvMsgDeliveryEvent db connId ackCmdId MDSRcvAcknowledged

    sentMsgDeliveryEvent :: Connection -> AgentMsgId -> m ()
    sentMsgDeliveryEvent Connection {connId} msgId =
      withStore $ \db -> createSndMsgDeliveryEvent db connId msgId MDSSndSent

    agentErrToItemStatus :: AgentErrorType -> CIStatus 'MDSnd
    agentErrToItemStatus (SMP AUTH) = CISSndErrorAuth
    agentErrToItemStatus err = CISSndError . T.unpack . safeDecodeUtf8 $ strEncode err

    badRcvFileChunk :: RcvFileTransfer -> String -> m ()
    badRcvFileChunk ft@RcvFileTransfer {cancelled} err =
      unless cancelled $ do
        cancelRcvFileTransfer user ft
        throwChatError $ CEFileRcvChunk err

    memberConnectedChatItem :: GroupInfo -> GroupMember -> m ()
    memberConnectedChatItem gInfo m =
      -- ts should be broker ts but we don't have it for CON
      createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvGroupEvent RGEMemberConnected) Nothing

    notifyMemberConnected :: GroupInfo -> GroupMember -> m ()
    notifyMemberConnected gInfo m@GroupMember {localDisplayName = c} = do
      memberConnectedChatItem gInfo m
      toView $ CRConnectedToGroupMember gInfo m
      let g = groupName' gInfo
      setActive $ ActiveG g
      showToast ("#" <> g) $ "member " <> c <> " is connected"

    probeMatchingContacts :: Contact -> Bool -> m ()
    probeMatchingContacts ct connectedIncognito = do
      gVar <- asks idsDrg
      (probe, probeId) <- withStore $ \db -> createSentProbe db gVar userId ct
      void . sendDirectContactMessage ct $ XInfoProbe probe
      if connectedIncognito
        then withStore' $ \db -> deleteSentProbe db userId probeId
        else do
          cs <- withStore' $ \db -> getMatchingContacts db user ct
          let probeHash = ProbeHash $ C.sha256Hash (unProbe probe)
          forM_ cs $ \c -> sendProbeHash c probeHash probeId `catchError` const (pure ())
      where
        sendProbeHash :: Contact -> ProbeHash -> Int64 -> m ()
        sendProbeHash c probeHash probeId = do
          void . sendDirectContactMessage c $ XInfoProbeCheck probeHash
          withStore' $ \db -> createSentProbeHash db userId probeId c

    messageWarning :: Text -> m ()
    messageWarning = toView . CRMessageError "warning"

    messageError :: Text -> m ()
    messageError = toView . CRMessageError "error"

    newContentMessage :: Contact -> MsgContainer -> RcvMessage -> MsgMeta -> m ()
    newContentMessage ct@Contact {localDisplayName = c, contactUsed, chatSettings} mc msg msgMeta = do
      unless contactUsed $ withStore' $ \db -> updateContactUsed db user ct
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      let ExtMsgContent content fileInvitation_ = mcExtMsgContent mc
      if isVoice content && not (featureAllowed CFVoice forContact ct)
        then void $ newChatItem (CIRcvChatFeatureRejected CFVoice) Nothing
        else do
          ciFile_ <- processFileInvitation fileInvitation_ content $ \db -> createRcvFileTransfer db userId ct
          ChatItem {formattedText} <- newChatItem (CIRcvMsgContent content) ciFile_
          when (enableNtfs chatSettings) $ showMsgToast (c <> "> ") content formattedText
      setActive $ ActiveC c
      where
        newChatItem ciContent ciFile_ = do
          ci <- saveRcvChatItem user (CDDirectRcv ct) msg msgMeta ciContent ciFile_
          toView . CRNewChatItem $ AChatItem SCTDirect SMDRcv (DirectChat ct) ci
          pure ci

    processFileInvitation :: Maybe FileInvitation -> MsgContent -> (DB.Connection -> FileInvitation -> Maybe InlineFileMode -> Integer -> IO RcvFileTransfer) -> m (Maybe (CIFile 'MDRcv))
    processFileInvitation fInv_ mc createRcvFT = forM fInv_ $ \fInv@FileInvitation {fileName, fileSize} -> do
      chSize <- asks $ fileChunkSize . config
      inline <- receiveInlineMode fInv (Just mc) chSize
      ft@RcvFileTransfer {fileId} <- withStore' $ \db -> createRcvFT db fInv inline chSize
      (filePath, fileStatus) <- case inline of
        Just IFMSent -> do
          fPath <- getRcvFilePath fileId Nothing fileName
          withStore' $ \db -> startRcvInlineFT db user ft fPath
          pure (Just fPath, CIFSRcvAccepted)
        _ -> pure (Nothing, CIFSRcvInvitation)
      pure CIFile {fileId, fileName, fileSize, filePath, fileStatus}

    messageUpdate :: Contact -> SharedMsgId -> MsgContent -> RcvMessage -> MsgMeta -> m ()
    messageUpdate ct@Contact {contactId, localDisplayName = c} sharedMsgId mc msg@RcvMessage {msgId} msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      updateRcvChatItem `catchError` \e ->
        case e of
          (ChatErrorStore (SEChatItemSharedMsgIdNotFound _)) -> do
            -- This patches initial sharedMsgId into chat item when locally deleted chat item
            -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
            -- Chat item and update message which created it will have different sharedMsgId in this case...
            ci <- saveRcvChatItem' user (CDDirectRcv ct) msg (Just sharedMsgId) msgMeta (CIRcvMsgContent mc) Nothing
            toView . CRChatItemUpdated $ AChatItem SCTDirect SMDRcv (DirectChat ct) ci
            setActive $ ActiveC c
          _ -> throwError e
      where
        updateRcvChatItem = do
          CChatItem msgDir ChatItem {meta = CIMeta {itemId}} <- withStore $ \db -> getDirectChatItemBySharedMsgId db userId contactId sharedMsgId
          case msgDir of
            SMDRcv -> updateDirectChatItemView userId ct itemId (ACIContent SMDRcv $ CIRcvMsgContent mc) $ Just msgId
            SMDSnd -> messageError "x.msg.update: contact attempted invalid message update"

    messageDelete :: Contact -> SharedMsgId -> RcvMessage -> MsgMeta -> m ()
    messageDelete ct@Contact {contactId} sharedMsgId RcvMessage {msgId} msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      deleteRcvChatItem `catchError` \e ->
        case e of
          (ChatErrorStore (SEChatItemSharedMsgIdNotFound sMsgId)) -> toView $ CRChatItemDeletedNotFound ct sMsgId
          _ -> throwError e
      where
        deleteRcvChatItem = do
          ci@(CChatItem msgDir _) <- withStore $ \db -> getDirectChatItemBySharedMsgId db userId contactId sharedMsgId
          case msgDir of
            SMDRcv ->
              if featureAllowed CFFullDelete forContact ct
                then deleteDirectCI user ct ci False >>= toView
                else markDirectCIDeleted user ct ci msgId False >>= toView
            SMDSnd -> messageError "x.msg.del: contact attempted invalid message delete"

    newGroupContentMessage :: GroupInfo -> GroupMember -> MsgContainer -> RcvMessage -> MsgMeta -> m ()
    newGroupContentMessage gInfo@GroupInfo {chatSettings} m@GroupMember {localDisplayName = c} mc msg msgMeta = do
      let (ExtMsgContent content fInv_) = mcExtMsgContent mc
      if isVoice content && not (groupFeatureAllowed GFVoice gInfo)
        then void $ newChatItem (CIRcvGroupFeatureRejected GFVoice) Nothing
        else do
          ciFile_ <- processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId m
          ChatItem {formattedText} <- newChatItem (CIRcvMsgContent content) ciFile_
          let g = groupName' gInfo
          when (enableNtfs chatSettings) $ showMsgToast ("#" <> g <> " " <> c <> "> ") content formattedText
          setActive $ ActiveG g
      where
        newChatItem ciContent ciFile_ = do
          ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta ciContent ciFile_
          groupMsgToView gInfo m ci msgMeta
          pure ci

    groupMessageUpdate :: GroupInfo -> GroupMember -> SharedMsgId -> MsgContent -> RcvMessage -> MsgMeta -> m ()
    groupMessageUpdate gInfo@GroupInfo {groupId, localDisplayName = g} m@GroupMember {groupMemberId, memberId} sharedMsgId mc msg@RcvMessage {msgId} msgMeta =
      updateRcvChatItem `catchError` \e ->
        case e of
          (ChatErrorStore (SEChatItemSharedMsgIdNotFound _)) -> do
            -- This patches initial sharedMsgId into chat item when locally deleted chat item
            -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
            -- Chat item and update message which created it will have different sharedMsgId in this case...
            ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) msg (Just sharedMsgId) msgMeta (CIRcvMsgContent mc) Nothing
            toView . CRChatItemUpdated $ AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci
            setActive $ ActiveG g
          _ -> throwError e
      where
        updateRcvChatItem = do
          CChatItem msgDir ChatItem {chatDir, meta = CIMeta {itemId}} <- withStore $ \db -> getGroupChatItemBySharedMsgId db user groupId groupMemberId sharedMsgId
          case (msgDir, chatDir) of
            (SMDRcv, CIGroupRcv m') ->
              if sameMemberId memberId m'
                then do
                  updCi <- withStore $ \db -> updateGroupChatItem db user groupId itemId (CIRcvMsgContent mc) msgId
                  toView . CRChatItemUpdated $ AChatItem SCTGroup SMDRcv (GroupChat gInfo) updCi
                  setActive $ ActiveG g
                else messageError "x.msg.update: group member attempted to update a message of another member" -- shouldn't happen now that query includes group member id
            (SMDSnd, _) -> messageError "x.msg.update: group member attempted invalid message update"

    groupMessageDelete :: GroupInfo -> GroupMember -> SharedMsgId -> RcvMessage -> m ()
    groupMessageDelete gInfo@GroupInfo {groupId} GroupMember {groupMemberId, memberId} sharedMsgId RcvMessage {msgId} = do
      ci@(CChatItem msgDir ChatItem {chatDir}) <- withStore $ \db -> getGroupChatItemBySharedMsgId db user groupId groupMemberId sharedMsgId
      case (msgDir, chatDir) of
        (SMDRcv, CIGroupRcv m) ->
          if sameMemberId memberId m
            then
              if groupFeatureAllowed GFFullDelete gInfo
                then deleteGroupCI user gInfo ci False >>= toView
                else markGroupCIDeleted user gInfo ci msgId False >>= toView
            else messageError "x.msg.del: group member attempted to delete a message of another member" -- shouldn't happen now that query includes group member id
        (SMDSnd, _) -> messageError "x.msg.del: group member attempted invalid message delete"

    -- TODO remove once XFile is discontinued
    processFileInvitation' :: Contact -> FileInvitation -> RcvMessage -> MsgMeta -> m ()
    processFileInvitation' ct@Contact {localDisplayName = c} fInv@FileInvitation {fileName, fileSize} msg msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      chSize <- asks $ fileChunkSize . config
      inline <- receiveInlineMode fInv Nothing chSize
      RcvFileTransfer {fileId} <- withStore' $ \db -> createRcvFileTransfer db userId ct fInv inline chSize
      let ciFile = Just $ CIFile {fileId, fileName, fileSize, filePath = Nothing, fileStatus = CIFSRcvInvitation}
      ci <- saveRcvChatItem user (CDDirectRcv ct) msg msgMeta (CIRcvMsgContent $ MCFile "") ciFile
      toView . CRNewChatItem $ AChatItem SCTDirect SMDRcv (DirectChat ct) ci
      showToast (c <> "> ") "wants to send a file"
      setActive $ ActiveC c

    -- TODO remove once XFile is discontinued
    processGroupFileInvitation' :: GroupInfo -> GroupMember -> FileInvitation -> RcvMessage -> MsgMeta -> m ()
    processGroupFileInvitation' gInfo m@GroupMember {localDisplayName = c} fInv@FileInvitation {fileName, fileSize} msg msgMeta = do
      chSize <- asks $ fileChunkSize . config
      inline <- receiveInlineMode fInv Nothing chSize
      RcvFileTransfer {fileId} <- withStore' $ \db -> createRcvGroupFileTransfer db userId m fInv inline chSize
      let ciFile = Just $ CIFile {fileId, fileName, fileSize, filePath = Nothing, fileStatus = CIFSRcvInvitation}
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvMsgContent $ MCFile "") ciFile
      groupMsgToView gInfo m ci msgMeta
      let g = groupName' gInfo
      showToast ("#" <> g <> " " <> c <> "> ") "wants to send a file"
      setActive $ ActiveG g

    receiveInlineMode :: FileInvitation -> Maybe MsgContent -> Integer -> m (Maybe InlineFileMode)
    receiveInlineMode FileInvitation {fileSize, fileInline} mc_ chSize = case fileInline of
      Just mode -> do
        InlineFilesConfig {receiveChunks, receiveInstant} <- asks $ inlineFiles . config
        pure $ if fileSize <= receiveChunks * chSize then inline' receiveInstant else Nothing
        where
          inline' receiveInstant = if mode == IFMOffer || (receiveInstant && maybe False isVoice mc_) then fileInline else Nothing
      _ -> pure Nothing

    xFileCancel :: Contact -> SharedMsgId -> MsgMeta -> m ()
    xFileCancel ct@Contact {contactId} sharedMsgId msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      fileId <- withStore $ \db -> getFileIdBySharedMsgId db userId contactId sharedMsgId
      ft@RcvFileTransfer {cancelled} <- withStore (\db -> getRcvFileTransfer db user fileId)
      unless cancelled $ do
        cancelRcvFileTransfer user ft
        toView $ CRRcvFileSndCancelled ft

    xFileAcptInv :: Contact -> SharedMsgId -> Maybe ConnReqInvitation -> String -> MsgMeta -> m ()
    xFileAcptInv ct sharedMsgId fileConnReq_ fName msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      fileId <- withStore $ \db -> getDirectFileIdBySharedMsgId db user ct sharedMsgId
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      -- [async agent commands] no continuation needed, but command should be asynchronous for stability
      if fName == fileName
        then unless cancelled $ case fileConnReq_ of
          -- receiving via a separate connection
          Just fileConnReq -> do
            connIds <- joinAgentConnectionAsync user True fileConnReq $ directMessage XOk
            withStore' $ \db -> createSndDirectFTConnection db user fileId connIds
          -- receiving inline
          _ -> do
            event <- withStore $ \db -> do
              ci <- updateDirectCIFileStatus db user fileId CIFSSndTransfer
              sft <- liftIO $ createSndDirectInlineFT db ct ft
              pure $ CRSndFileStart ci sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendDirectFileInline ct ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
        else messageError "x.file.acpt.inv: fileName is different from expected"

    checkSndInlineFTComplete :: Connection -> AgentMsgId -> m ()
    checkSndInlineFTComplete conn agentMsgId = do
      ft_ <- withStore' $ \db -> getSndInlineFTViaMsgDelivery db user conn agentMsgId
      forM_ ft_ $ \ft@SndFileTransfer {fileId} -> do
        ci <- withStore $ \db -> do
          liftIO $ updateSndFileStatus db ft FSComplete
          liftIO $ deleteSndFileChunks db ft
          updateDirectCIFileStatus db user fileId CIFSSndComplete
        toView $ CRSndFileComplete ci ft

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
    receiveInlineChunk ft chunk meta = do
      case chunk of
        FileChunk {chunkNo} -> when (chunkNo == 1) $ startReceivingFile ft
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
              ft@RcvFileTransfer {cancelled} <- withStore (\db -> getRcvFileTransfer db user fileId)
              unless cancelled $ do
                cancelRcvFileTransfer user ft
                toView $ CRRcvFileSndCancelled ft
            else messageError "x.file.cancel: group member attempted to cancel file of another member" -- shouldn't happen now that query includes group member id
        (SMDSnd, _) -> messageError "x.file.cancel: group member attempted invalid file cancel"

    xFileAcptInvGroup :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe ConnReqInvitation -> String -> MsgMeta -> m ()
    xFileAcptInvGroup g@GroupInfo {groupId} m@GroupMember {activeConn} sharedMsgId fileConnReq_ fName msgMeta = do
      checkIntegrityCreateItem (CDGroupRcv g m) msgMeta
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      -- TODO check that it's not already accpeted
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      if fName == fileName
        then unless cancelled $ case (fileConnReq_, activeConn) of
          (Just fileConnReq, _) -> do
            -- receiving via a separate connection
            -- [async agent commands] no continuation needed, but command should be asynchronous for stability
            connIds <- joinAgentConnectionAsync user True fileConnReq $ directMessage XOk
            withStore' $ \db -> createSndGroupFileTransferConnection db user fileId connIds m
          (_, Just conn) -> do
            -- receiving inline
            event <- withStore $ \db -> do
              ci <- updateDirectCIFileStatus db user fileId CIFSSndTransfer
              sft <- liftIO $ createSndGroupInlineFT db m conn ft
              pure $ CRSndFileStart ci sft
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
      toView . CRNewChatItem $ AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci

    processGroupInvitation :: Contact -> GroupInvitation -> RcvMessage -> MsgMeta -> m ()
    processGroupInvitation ct@Contact {localDisplayName = c, activeConn = Connection {customUserProfileId, groupLinkId = groupLinkId'}} inv@GroupInvitation {fromMember = (MemberIdRole fromMemId fromRole), invitedMember = (MemberIdRole memId memRole), connRequest, groupLinkId} msg msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
      when (fromRole < GRMember || fromRole < memRole) $ throwChatError (CEGroupContactRole c)
      when (fromMemId == memId) $ throwChatError CEGroupDuplicateMemberId
      -- [incognito] if direct connection with host is incognito, create membership using the same incognito profile
      (gInfo@GroupInfo {groupId, localDisplayName, groupProfile, membership = membership@GroupMember {groupMemberId, memberId}}, hostId) <- withStore $ \db -> createGroupInvitation db user ct inv customUserProfileId
      if sameGroupLinkId groupLinkId groupLinkId'
        then do
          connIds <- joinAgentConnectionAsync user True connRequest . directMessage $ XGrpAcpt memberId
          withStore' $ \db -> do
            createMemberConnectionAsync db user hostId connIds
            updateGroupMemberStatusById db userId hostId GSMemAccepted
            updateGroupMemberStatus db userId membership GSMemAccepted
          toView $ CRUserAcceptedGroupSent gInfo {membership = membership {memberStatus = GSMemAccepted}} (Just ct)
        else do
          let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
          ci <- saveRcvChatItem user (CDDirectRcv ct) msg msgMeta content Nothing
          withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
          toView . CRNewChatItem $ AChatItem SCTDirect SMDRcv (DirectChat ct) ci
          toView $ CRReceivedGroupInvitation gInfo ct memRole
          showToast ("#" <> localDisplayName <> " " <> c <> "> ") "invited you to join the group"
      where
        sameGroupLinkId :: Maybe GroupLinkId -> Maybe GroupLinkId -> Bool
        sameGroupLinkId (Just gli) (Just gli') = gli == gli'
        sameGroupLinkId _ _ = False

    checkIntegrityCreateItem :: forall c. ChatTypeI c => ChatDirection c 'MDRcv -> MsgMeta -> m ()
    checkIntegrityCreateItem cd MsgMeta {integrity, broker = (_, brokerTs)} = case integrity of
      MsgOk -> pure ()
      MsgError e -> case e of
        MsgSkipped {} -> createInternalChatItem user cd (CIRcvIntegrityError e) (Just brokerTs)
        _ -> toView $ CRMsgIntegrityError e

    xInfo :: Contact -> Profile -> m ()
    xInfo c@Contact {profile = p} p' = unless (fromLocalProfile p == p') $ do
      c' <- withStore $ \db -> updateContactProfile db user c p'
      toView $ CRContactUpdated c c'
      when (directContact c) $ createFeatureChangedItems user c c' CDDirectRcv CIRcvChatFeature

    createFeatureEnabledItems :: Contact -> m ()
    createFeatureEnabledItems ct@Contact {mergedPreferences} =
      forM_ allChatFeatures $ \f -> do
        let ContactUserPreference {enabled} = getContactUserPreference f mergedPreferences
        createInternalChatItem user (CDDirectRcv ct) (CIRcvChatFeature f enabled) Nothing

    createGroupFeatureItems :: GroupInfo -> GroupMember -> m ()
    createGroupFeatureItems g@GroupInfo {groupProfile} m = do
      let prefs = mergeGroupPreferences $ groupPreferences groupProfile
      forM_ allGroupFeatures $ \f -> do
        let p = getGroupPreference f prefs
        createInternalChatItem user (CDGroupRcv g m) (CIRcvGroupFeature f p) Nothing

    xInfoProbe :: Contact -> Probe -> m ()
    xInfoProbe c2 probe =
      -- [incognito] unless connected incognito
      unless (contactConnIncognito c2) $ do
        r <- withStore' $ \db -> matchReceivedProbe db user c2 probe
        forM_ r $ \c1 -> probeMatch c1 c2 probe

    xInfoProbeCheck :: Contact -> ProbeHash -> m ()
    xInfoProbeCheck c1 probeHash =
      -- [incognito] unless connected incognito
      unless (contactConnIncognito c1) $ do
        r <- withStore' $ \db -> matchReceivedProbeHash db user c1 probeHash
        forM_ r . uncurry $ probeMatch c1

    probeMatch :: Contact -> Contact -> Probe -> m ()
    probeMatch c1@Contact {contactId = cId1, profile = p1} c2@Contact {contactId = cId2, profile = p2} probe =
      if profilesMatch (fromLocalProfile p1) (fromLocalProfile p2) && cId1 /= cId2
        then do
          void . sendDirectContactMessage c1 $ XInfoProbeOk probe
          mergeContacts c1 c2
        else messageWarning "probeMatch ignored: profiles don't match or same contact id"

    xInfoProbeOk :: Contact -> Probe -> m ()
    xInfoProbeOk c1@Contact {contactId = cId1} probe = do
      r <- withStore' $ \db -> matchSentProbe db user c1 probe
      forM_ r $ \c2@Contact {contactId = cId2} ->
        if cId1 /= cId2
          then mergeContacts c1 c2
          else messageWarning "xInfoProbeOk ignored: same contact id"

    -- to party accepting call
    xCallInv :: Contact -> CallId -> CallInvitation -> RcvMessage -> MsgMeta -> m ()
    xCallInv ct@Contact {contactId} callId CallInvitation {callType, callDhPubKey} msg msgMeta = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta
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
      forM_ call_ $ \call -> updateCallItemStatus userId ct call WCSDisconnected Nothing
      toView . CRCallInvitation $ RcvCallInvitation {contact = ct, callType, sharedKey, callTs = chatItemTs' ci}
      toView . CRNewChatItem $ AChatItem SCTDirect SMDRcv (DirectChat ct) ci
      where
        saveCallItem status = saveRcvChatItem user (CDDirectRcv ct) msg msgMeta (CIRcvCall status 0) Nothing

    -- to party initiating call
    xCallOffer :: Contact -> CallId -> CallOffer -> RcvMessage -> MsgMeta -> m ()
    xCallOffer ct callId CallOffer {callType, rtcSession, callDhPubKey} msg msgMeta = do
      msgCurrentCall ct callId "x.call.offer" msg msgMeta $
        \call -> case callState call of
          CallInvitationSent {localCallType, localDhPrivKey} -> do
            let sharedKey = C.Key . C.dhBytes' <$> (C.dh' <$> callDhPubKey <*> localDhPrivKey)
                callState' = CallOfferReceived {localCallType, peerCallType = callType, peerCallSession = rtcSession, sharedKey}
                askConfirmation = encryptedCall localCallType && not (encryptedCall callType)
            toView CRCallOffer {contact = ct, callType, offer = rtcSession, sharedKey, askConfirmation}
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
            toView $ CRCallAnswer ct rtcSession
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
            toView $ CRCallExtraInfo ct rtcExtraInfo
            pure (Just call {callState = callState'}, Nothing)
          CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey} -> do
            -- TODO update the list of ice servers in peerCallSession
            let callState' = CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey}
            toView $ CRCallExtraInfo ct rtcExtraInfo
            pure (Just call {callState = callState'}, Nothing)
          _ -> do
            msgCallStateError "x.call.extra" call
            pure (Just call, Nothing)

    -- to any call party
    xCallEnd :: Contact -> CallId -> RcvMessage -> MsgMeta -> m ()
    xCallEnd ct callId msg msgMeta =
      msgCurrentCall ct callId "x.call.end" msg msgMeta $ \Call {chatItemId} -> do
        toView $ CRCallEnded ct
        (Nothing,) <$> callStatusItemContent userId ct chatItemId WCSDisconnected

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
              updateDirectChatItemView userId ct chatItemId aciContent $ Just msgId

    msgCallStateError :: Text -> Call -> m ()
    msgCallStateError eventName Call {callState} =
      messageError $ eventName <> ": wrong call state " <> T.pack (show $ callStateTag callState)

    mergeContacts :: Contact -> Contact -> m ()
    mergeContacts to from = do
      withStore' $ \db -> mergeContactRecords db userId to from
      toView $ CRContactsMerged to from

    saveConnInfo :: Connection -> ConnInfo -> m ()
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatMsgEvent} <- parseChatMessage connInfo
      case chatMsgEvent of
        XInfo p -> do
          ct <- withStore $ \db -> createDirectContact db user activeConn p
          toView $ CRContactConnecting ct
        -- TODO show/log error, other events in SMP confirmation
        _ -> pure ()

    xGrpMemNew :: GroupInfo -> GroupMember -> MemberInfo -> RcvMessage -> MsgMeta -> m ()
    xGrpMemNew gInfo m memInfo@(MemberInfo memId memRole memberProfile) msg msgMeta = do
      checkHostRole m memRole
      members <- withStore' $ \db -> getGroupMembers db user gInfo
      unless (sameMemberId memId $ membership gInfo) $
        if isMember memId gInfo members
          then messageError "x.grp.mem.new error: member already exists"
          else do
            newMember@GroupMember {groupMemberId} <- withStore $ \db -> createNewGroupMember db user gInfo memInfo GCPostMember GSMemAnnounced
            ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent $ RGEMemberAdded groupMemberId memberProfile) Nothing
            groupMsgToView gInfo m ci msgMeta
            toView $ CRJoinedGroupMemberConnecting gInfo m newMember

    xGrpMemIntro :: GroupInfo -> GroupMember -> MemberInfo -> m ()
    xGrpMemIntro gInfo@GroupInfo {membership, chatSettings = ChatSettings {enableNtfs}} m@GroupMember {memberRole, localDisplayName = c} memInfo@(MemberInfo memId _ _) = do
      case memberCategory m of
        GCHostMember -> do
          members <- withStore' $ \db -> getGroupMembers db user gInfo
          if isMember memId gInfo members
            then messageWarning "x.grp.mem.intro ignored: member already exists"
            else do
              when (memberRole < GRMember) $ throwChatError (CEGroupContactRole c)
              -- [async agent commands] commands should be asynchronous, continuation is to send XGrpMemInv - have to remember one has completed and process on second
              groupConnIds <- createAgentConnectionAsync user CFCreateConnGrpMemInv enableNtfs SCMInvitation
              directConnIds <- createAgentConnectionAsync user CFCreateConnGrpMemInv enableNtfs SCMInvitation
              -- [incognito] direct connection with member has to be established using the same incognito profile [that was known to host and used for group membership]
              let customUserProfileId = if memberIncognito membership then Just (localProfileId $ memberProfile membership) else Nothing
              void $ withStore $ \db -> createIntroReMember db user gInfo m memInfo groupConnIds directConnIds customUserProfileId
        _ -> messageError "x.grp.mem.intro can be only sent by host member"

    sendXGrpMemInv :: Int64 -> ConnReqInvitation -> XGrpMemIntroCont -> m ()
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
              void . sendGroupMessage' [reMember] (XGrpMemFwd (memberInfo m) introInv) groupId (Just introId) $
                withStore' $ \db -> updateIntroStatus db introId GMIntroInvForwarded
        _ -> messageError "x.grp.mem.inv can be only sent by invitee member"

    xGrpMemFwd :: GroupInfo -> GroupMember -> MemberInfo -> IntroInvitation -> m ()
    xGrpMemFwd gInfo@GroupInfo {membership, chatSettings = ChatSettings {enableNtfs}} m memInfo@(MemberInfo memId memRole _) introInv@IntroInvitation {groupConnReq, directConnReq} = do
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
      -- [incognito] send membership incognito profile, create direct connection as incognito
      let msg = XGrpMemInfo (memberId (membership :: GroupMember)) (fromLocalProfile $ memberProfile membership)
      -- [async agent commands] no continuation needed, but commands should be asynchronous for stability
      groupConnIds <- joinAgentConnectionAsync user enableNtfs groupConnReq $ directMessage msg
      directConnIds <- joinAgentConnectionAsync user enableNtfs directConnReq $ directMessage msg
      let customUserProfileId = if memberIncognito membership then Just (localProfileId $ memberProfile membership) else Nothing
      withStore' $ \db -> createIntroToMemberContact db user m toMember groupConnIds directConnIds customUserProfileId

    xGrpMemRole :: GroupInfo -> GroupMember -> MemberId -> GroupMemberRole -> RcvMessage -> MsgMeta -> m ()
    xGrpMemRole gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId memRole msg msgMeta
      | memberId (membership :: GroupMember) == memId =
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
            ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent gEvent) Nothing
            groupMsgToView gInfo m ci msgMeta
            toView CRMemberRole {groupInfo = gInfo', byMember = m, member = member {memberRole = memRole}, fromRole, toRole = memRole}

    checkHostRole :: GroupMember -> GroupMemberRole -> m ()
    checkHostRole GroupMember {memberRole, localDisplayName} memRole =
      when (memberRole < GRMember || memberRole < memRole) $ throwChatError (CEGroupContactRole localDisplayName)

    xGrpMemDel :: GroupInfo -> GroupMember -> MemberId -> RcvMessage -> MsgMeta -> m ()
    xGrpMemDel gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId msg msgMeta = do
      members <- withStore' $ \db -> getGroupMembers db user gInfo
      if memberId (membership :: GroupMember) == memId
        then checkRole membership $ do
          deleteGroupLink' user gInfo `catchError` \_ -> pure ()
          forM_ members $ deleteMemberConnection user
          deleteMember membership RGEUserDeleted
          toView $ CRDeletedMemberUser gInfo {membership = membership {memberStatus = GSMemRemoved}} m
        else case find (sameMemberId memId) members of
          Nothing -> messageError "x.grp.mem.del with unknown member ID"
          Just member@GroupMember {groupMemberId, memberProfile} ->
            checkRole member $ do
              deleteMemberConnection user member
              deleteMember member $ RGEMemberDeleted groupMemberId (fromLocalProfile memberProfile)
              toView $ CRDeletedMember gInfo m member {memberStatus = GSMemRemoved}
      where
        checkRole GroupMember {memberRole} a
          | senderRole < GRAdmin || senderRole < memberRole =
            messageError "x.grp.mem.del with insufficient member permissions"
          | otherwise = a
        deleteMember member gEvent = do
          withStore' $ \db -> updateGroupMemberStatus db userId member GSMemRemoved
          ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent gEvent) Nothing
          groupMsgToView gInfo m ci msgMeta

    sameMemberId :: MemberId -> GroupMember -> Bool
    sameMemberId memId GroupMember {memberId} = memId == memberId

    xGrpLeave :: GroupInfo -> GroupMember -> RcvMessage -> MsgMeta -> m ()
    xGrpLeave gInfo m msg msgMeta = do
      deleteMemberConnection user m
      withStore' $ \db -> updateGroupMemberStatus db userId m GSMemLeft
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent RGEMemberLeft) Nothing
      groupMsgToView gInfo m ci msgMeta
      toView $ CRLeftMember gInfo m {memberStatus = GSMemLeft}

    xGrpDel :: GroupInfo -> GroupMember -> RcvMessage -> MsgMeta -> m ()
    xGrpDel gInfo@GroupInfo {membership} m@GroupMember {memberRole} msg msgMeta = do
      when (memberRole /= GROwner) $ throwChatError CEGroupUserRole
      ms <- withStore' $ \db -> do
        members <- getGroupMembers db user gInfo
        updateGroupMemberStatus db userId membership GSMemGroupDeleted
        pure members
      forM_ ms $ deleteMemberConnection user
      ci <- saveRcvChatItem user (CDGroupRcv gInfo m) msg msgMeta (CIRcvGroupEvent RGEGroupDeleted) Nothing
      groupMsgToView gInfo m ci msgMeta
      toView $ CRGroupDeleted gInfo {membership = membership {memberStatus = GSMemGroupDeleted}} m

    xGrpInfo :: GroupInfo -> GroupMember -> GroupProfile -> RcvMessage -> MsgMeta -> m ()
    xGrpInfo g@GroupInfo {groupProfile = p} m@GroupMember {memberRole} p' msg msgMeta
      | memberRole < GROwner = messageError "x.grp.info with insufficient member permissions"
      | otherwise = unless (p == p') $ do
        g' <- withStore $ \db -> updateGroupProfile db user g p'
        toView . CRGroupUpdated g g' $ Just m
        let cd = CDGroupRcv g' m
        unless (sameGroupProfileInfo p p') $ do
          ci <- saveRcvChatItem user cd msg msgMeta (CIRcvGroupEvent $ RGEGroupUpdated p') Nothing
          groupMsgToView g' m ci msgMeta
        createGroupFeatureChangedItems user cd CIRcvGroupFeature p p'

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

parseChatMessage :: ChatMonad m => ByteString -> m (ChatMessage 'Json)
parseChatMessage = liftEither . first (ChatError . CEInvalidChatMessage) . strDecode

parseAChatMessage :: ChatMonad m => ByteString -> m AChatMessage
parseAChatMessage = liftEither . first (ChatError . CEInvalidChatMessage) . strDecode

sendFileChunk :: ChatMonad m => User -> SndFileTransfer -> m ()
sendFileChunk user ft@SndFileTransfer {fileId, fileStatus, connId, agentConnId} =
  unless (fileStatus == FSComplete || fileStatus == FSCancelled) $
    withStore' (`createSndFileChunk` ft) >>= \case
      Just chunkNo -> sendFileChunkNo ft chunkNo
      Nothing -> do
        ci <- withStore $ \db -> do
          liftIO $ updateSndFileStatus db ft FSComplete
          liftIO $ deleteSndFileChunks db ft
          updateDirectCIFileStatus db user fileId CIFSSndComplete
        toView $ CRSndFileComplete ci ft
        closeFileHandle fileId sndFiles
        deleteAgentConnectionAsync' user connId agentConnId

sendFileChunkNo :: ChatMonad m => SndFileTransfer -> Integer -> m ()
sendFileChunkNo ft@SndFileTransfer {agentConnId = AgentConnId acId} chunkNo = do
  chunkBytes <- readFileChunk ft chunkNo
  msgId <- withAgent $ \a -> sendMessage a acId SMP.noMsgFlags $ smpEncode FileChunk {chunkNo, chunkBytes}
  withStore' $ \db -> updateSndFileChunkMsg db ft chunkNo msgId

readFileChunk :: ChatMonad m => SndFileTransfer -> Integer -> m ByteString
readFileChunk SndFileTransfer {fileId, filePath, chunkSize} chunkNo = do
  fsFilePath <- toFSFilePath filePath
  read_ fsFilePath `E.catch` (throwChatError . CEFileRead filePath . (show :: E.SomeException -> String))
  where
    read_ fsFilePath = do
      h <- getFileHandle fileId fsFilePath sndFiles ReadMode
      pos <- hTell h
      let pos' = (chunkNo - 1) * chunkSize
      when (pos /= pos') $ hSeek h AbsoluteSeek pos'
      liftIO . B.hGet h $ fromInteger chunkSize

parseFileChunk :: ChatMonad m => ByteString -> m FileChunk
parseFileChunk = liftEither . first (ChatError . CEFileRcvChunk) . smpDecode

appendFileChunk :: ChatMonad m => RcvFileTransfer -> Integer -> ByteString -> m ()
appendFileChunk ft@RcvFileTransfer {fileId, fileStatus} chunkNo chunk =
  case fileStatus of
    RFSConnected RcvFileInfo {filePath} -> append_ filePath
    -- sometimes update of file transfer status to FSConnected
    -- doesn't complete in time before MSG with first file chunk
    RFSAccepted RcvFileInfo {filePath} -> append_ filePath
    RFSCancelled _ -> pure ()
    _ -> throwChatError $ CEFileInternal "receiving file transfer not in progress"
  where
    append_ filePath = do
      fsFilePath <- toFSFilePath filePath
      h <- getFileHandle fileId fsFilePath rcvFiles AppendMode
      E.try (liftIO $ B.hPut h chunk >> hFlush h) >>= \case
        Left (e :: E.SomeException) -> throwChatError . CEFileWrite fsFilePath $ show e
        Right () -> withStore' $ \db -> updatedRcvFileChunkStored db ft chunkNo

getFileHandle :: ChatMonad m => Int64 -> FilePath -> (ChatController -> TVar (Map Int64 Handle)) -> IOMode -> m Handle
getFileHandle fileId filePath files ioMode = do
  fs <- asks files
  h_ <- M.lookup fileId <$> readTVarIO fs
  maybe (newHandle fs) pure h_
  where
    newHandle fs = do
      -- TODO handle errors
      h <- liftIO (openFile filePath ioMode)
      atomically . modifyTVar fs $ M.insert fileId h
      pure h

isFileActive :: ChatMonad m => Int64 -> (ChatController -> TVar (Map Int64 Handle)) -> m Bool
isFileActive fileId files = do
  fs <- asks files
  isJust . M.lookup fileId <$> readTVarIO fs

cancelRcvFileTransfer :: ChatMonad m => User -> RcvFileTransfer -> m ()
cancelRcvFileTransfer user ft@RcvFileTransfer {fileId, fileStatus, rcvFileInline} = do
  closeFileHandle fileId rcvFiles
  withStore' $ \db -> do
    updateFileCancelled db user fileId CIFSRcvCancelled
    updateRcvFileStatus db ft FSCancelled
    deleteRcvFileChunks db ft
  when (isNothing rcvFileInline) $ case fileStatus of
    RFSAccepted RcvFileInfo {connId, agentConnId} ->
      deleteAgentConnectionAsync' user connId agentConnId
    RFSConnected RcvFileInfo {connId, agentConnId} ->
      deleteAgentConnectionAsync' user connId agentConnId
    _ -> pure ()

cancelSndFile :: ChatMonad m => User -> FileTransferMeta -> [SndFileTransfer] -> m ()
cancelSndFile user FileTransferMeta {fileId} fts = do
  withStore' $ \db -> updateFileCancelled db user fileId CIFSSndCancelled
  forM_ fts $ \ft' -> cancelSndFileTransfer user ft'

cancelSndFileTransfer :: ChatMonad m => User -> SndFileTransfer -> m ()
cancelSndFileTransfer user ft@SndFileTransfer {connId, agentConnId = agentConnId@(AgentConnId acId), fileStatus} =
  unless (fileStatus == FSCancelled || fileStatus == FSComplete) $ do
    withStore' $ \db -> do
      updateSndFileStatus db ft FSCancelled
      deleteSndFileChunks db ft
    withAgent $ \a -> void (sendMessage a acId SMP.noMsgFlags $ smpEncode FileChunkCancel) `catchError` \_ -> pure ()
    deleteAgentConnectionAsync' user connId agentConnId

closeFileHandle :: ChatMonad m => Int64 -> (ChatController -> TVar (Map Int64 Handle)) -> m ()
closeFileHandle fileId files = do
  fs <- asks files
  h_ <- atomically . stateTVar fs $ \m -> (M.lookup fileId m, M.delete fileId m)
  mapM_ hClose h_ `E.catch` \(_ :: E.SomeException) -> pure ()

throwChatError :: ChatMonad m => ChatErrorType -> m a
throwChatError = throwError . ChatError

deleteMemberConnection :: ChatMonad m => User -> GroupMember -> m ()
deleteMemberConnection user GroupMember {activeConn} = do
  forM_ activeConn $ \conn -> do
    deleteAgentConnectionAsync user conn `catchError` \_ -> pure ()
    withStore' $ \db -> updateConnectionStatus db conn ConnDeleted

sendDirectContactMessage :: (MsgEncodingI e, ChatMonad m) => Contact -> ChatMsgEvent e -> m (SndMessage, Int64)
sendDirectContactMessage ct@Contact {activeConn = conn@Connection {connId, connStatus}} chatMsgEvent = do
  if connStatus == ConnReady || connStatus == ConnSndReady
    then sendDirectMessage conn chatMsgEvent (ConnectionId connId)
    else throwChatError $ CEContactNotReady ct

sendDirectMessage :: (MsgEncodingI e, ChatMonad m) => Connection -> ChatMsgEvent e -> ConnOrGroupId -> m (SndMessage, Int64)
sendDirectMessage conn chatMsgEvent connOrGroupId = do
  msg@SndMessage {msgId, msgBody} <- createSndMessage chatMsgEvent connOrGroupId
  (msg,) <$> deliverMessage conn (toCMEventTag chatMsgEvent) msgBody msgId

createSndMessage :: (MsgEncodingI e, ChatMonad m) => ChatMsgEvent e -> ConnOrGroupId -> m SndMessage
createSndMessage chatMsgEvent connOrGroupId = do
  gVar <- asks idsDrg
  withStore $ \db -> createNewSndMessage db gVar connOrGroupId $ \sharedMsgId ->
    let msgBody = strEncode ChatMessage {msgId = Just sharedMsgId, chatMsgEvent}
     in NewMessage {chatMsgEvent, msgBody}

directMessage :: MsgEncodingI e => ChatMsgEvent e -> ByteString
directMessage chatMsgEvent = strEncode ChatMessage {msgId = Nothing, chatMsgEvent}

deliverMessage :: ChatMonad m => Connection -> CMEventTag e -> MsgBody -> MessageId -> m Int64
deliverMessage conn@Connection {connId} cmEventTag msgBody msgId = do
  let msgFlags = MsgFlags {notification = hasNotification cmEventTag}
  agentMsgId <- withAgent $ \a -> sendMessage a (aConnId conn) msgFlags msgBody
  let sndMsgDelivery = SndMsgDelivery {connId, agentMsgId}
  withStore' $ \db -> createSndMsgDelivery db sndMsgDelivery msgId

sendGroupMessage :: (MsgEncodingI e, ChatMonad m) => GroupInfo -> [GroupMember] -> ChatMsgEvent e -> m SndMessage
sendGroupMessage GroupInfo {groupId} members chatMsgEvent =
  sendGroupMessage' members chatMsgEvent groupId Nothing $ pure ()

sendGroupMessage' :: (MsgEncodingI e, ChatMonad m) => [GroupMember] -> ChatMsgEvent e -> Int64 -> Maybe Int64 -> m () -> m SndMessage
sendGroupMessage' members chatMsgEvent groupId introId_ postDeliver = do
  msg@SndMessage {msgId, msgBody} <- createSndMessage chatMsgEvent (GroupId groupId)
  -- TODO collect failed deliveries into a single error
  forM_ (filter memberCurrent members) $ \m@GroupMember {groupMemberId} ->
    case memberConn m of
      Nothing -> withStore' $ \db -> createPendingGroupMessage db groupMemberId msgId introId_
      Just conn@Connection {connStatus}
        | connStatus == ConnSndReady || connStatus == ConnReady -> do
          let tag = toCMEventTag chatMsgEvent
          (deliverMessage conn tag msgBody msgId >> postDeliver) `catchError` const (pure ())
        | connStatus == ConnDeleted -> pure ()
        | otherwise -> withStore' $ \db -> createPendingGroupMessage db groupMemberId msgId introId_
  pure msg

sendPendingGroupMessages :: ChatMonad m => GroupMember -> Connection -> m ()
sendPendingGroupMessages GroupMember {groupMemberId, localDisplayName} conn = do
  pendingMessages <- withStore' $ \db -> getPendingGroupMessages db groupMemberId
  -- TODO ensure order - pending messages interleave with user input messages
  forM_ pendingMessages $ \PendingGroupMessage {msgId, cmEventTag = ACMEventTag _ tag, msgBody, introId_} -> do
    void $ deliverMessage conn tag msgBody msgId
    withStore' $ \db -> deletePendingGroupMessage db groupMemberId msgId
    case tag of
      XGrpMemFwd_ -> case introId_ of
        Just introId -> withStore' $ \db -> updateIntroStatus db introId GMIntroInvForwarded
        _ -> throwChatError $ CEGroupMemberIntroNotFound localDisplayName
      _ -> pure ()

saveRcvMSG :: ChatMonad m => Connection -> ConnOrGroupId -> MsgMeta -> MsgBody -> CommandId -> m RcvMessage
saveRcvMSG Connection {connId} connOrGroupId agentMsgMeta msgBody agentAckCmdId = do
  ACMsg _ ChatMessage {msgId = sharedMsgId_, chatMsgEvent} <- parseAChatMessage msgBody
  let agentMsgId = fst $ recipient agentMsgMeta
      newMsg = NewMessage {chatMsgEvent, msgBody}
      rcvMsgDelivery = RcvMsgDelivery {connId, agentMsgId, agentMsgMeta, agentAckCmdId}
  withStore' $ \db -> createNewMessageAndRcvMsgDelivery db connOrGroupId newMsg sharedMsgId_ rcvMsgDelivery

saveSndChatItem :: ChatMonad m => User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> Maybe (CIFile 'MDSnd) -> Maybe (CIQuote c) -> m (ChatItem c 'MDSnd)
saveSndChatItem user cd msg@SndMessage {sharedMsgId} content ciFile quotedItem = do
  createdAt <- liftIO getCurrentTime
  ciId <- withStore' $ \db -> createNewSndChatItem db user cd msg content quotedItem createdAt
  forM_ ciFile $ \CIFile {fileId} -> withStore' $ \db -> updateFileTransferChatItemId db fileId ciId
  liftIO $ mkChatItem cd ciId content ciFile quotedItem (Just sharedMsgId) createdAt createdAt

saveRcvChatItem :: ChatMonad m => User -> ChatDirection c 'MDRcv -> RcvMessage -> MsgMeta -> CIContent 'MDRcv -> Maybe (CIFile 'MDRcv) -> m (ChatItem c 'MDRcv)
saveRcvChatItem user cd msg@RcvMessage {sharedMsgId_} = saveRcvChatItem' user cd msg sharedMsgId_

saveRcvChatItem' :: ChatMonad m => User -> ChatDirection c 'MDRcv -> RcvMessage -> Maybe SharedMsgId -> MsgMeta -> CIContent 'MDRcv -> Maybe (CIFile 'MDRcv) -> m (ChatItem c 'MDRcv)
saveRcvChatItem' user cd msg sharedMsgId_ MsgMeta {broker = (_, brokerTs)} content ciFile = do
  createdAt <- liftIO getCurrentTime
  (ciId, quotedItem) <- withStore' $ \db -> createNewRcvChatItem db user cd msg sharedMsgId_ content brokerTs createdAt
  forM_ ciFile $ \CIFile {fileId} -> withStore' $ \db -> updateFileTransferChatItemId db fileId ciId
  liftIO $ mkChatItem cd ciId content ciFile quotedItem sharedMsgId_ brokerTs createdAt

mkChatItem :: ChatDirection c d -> ChatItemId -> CIContent d -> Maybe (CIFile d) -> Maybe (CIQuote c) -> Maybe SharedMsgId -> ChatItemTs -> UTCTime -> IO (ChatItem c d)
mkChatItem cd ciId content file quotedItem sharedMsgId itemTs currentTs = do
  tz <- getCurrentTimeZone
  let itemText = ciContentToText content
      itemStatus = ciCreateStatus content
      meta = mkCIMeta ciId content itemText itemStatus sharedMsgId False False tz currentTs itemTs currentTs currentTs
  pure ChatItem {chatDir = toCIDirection cd, meta, content, formattedText = parseMaybeMarkdownList itemText, quotedItem, file}

deleteDirectCI :: ChatMonad m => User -> Contact -> CChatItem 'CTDirect -> Bool -> m ChatResponse
deleteDirectCI user ct ci@(CChatItem msgDir deletedItem@ChatItem {file}) byUser = do
  deleteCIFile user file
  withStore' $ \db -> deleteDirectChatItem db user ct ci
  pure $ CRChatItemDeleted (AChatItem SCTDirect msgDir (DirectChat ct) deletedItem) Nothing byUser

deleteGroupCI :: ChatMonad m => User -> GroupInfo -> CChatItem 'CTGroup -> Bool -> m ChatResponse
deleteGroupCI user gInfo ci@(CChatItem msgDir deletedItem@ChatItem {file}) byUser = do
  deleteCIFile user file
  withStore' $ \db -> deleteGroupChatItem db user gInfo ci
  pure $ CRChatItemDeleted (AChatItem SCTGroup msgDir (GroupChat gInfo) deletedItem) Nothing byUser

deleteCIFile :: (ChatMonad m, MsgDirectionI d) => User -> Maybe (CIFile d) -> m ()
deleteCIFile user file =
  forM_ file $ \CIFile {fileId, filePath, fileStatus} -> do
    let fileInfo = CIFileInfo {fileId, fileStatus = Just $ AFS msgDirection fileStatus, filePath}
    deleteFile user fileInfo

markDirectCIDeleted :: ChatMonad m => User -> Contact -> CChatItem 'CTDirect -> MessageId -> Bool -> m ChatResponse
markDirectCIDeleted user ct ci@(CChatItem msgDir deletedItem) msgId byUser = do
  toCi <- withStore' $ \db -> markDirectChatItemDeleted db user ct ci msgId
  pure $ CRChatItemDeleted (AChatItem SCTDirect msgDir (DirectChat ct) deletedItem) (Just toCi) byUser

markGroupCIDeleted :: ChatMonad m => User -> GroupInfo -> CChatItem 'CTGroup -> MessageId -> Bool -> m ChatResponse
markGroupCIDeleted user gInfo ci@(CChatItem msgDir deletedItem) msgId byUser = do
  toCi <- withStore' $ \db -> markGroupChatItemDeleted db user gInfo ci msgId
  pure $ CRChatItemDeleted (AChatItem SCTGroup msgDir (GroupChat gInfo) deletedItem) (Just toCi) byUser

createAgentConnectionAsync :: forall m c. (ChatMonad m, ConnectionModeI c) => User -> CommandFunction -> Bool -> SConnectionMode c -> m (CommandId, ConnId)
createAgentConnectionAsync user cmdFunction enableNtfs cMode = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing cmdFunction
  connId <- withAgent $ \a -> createConnectionAsync a (aCorrId cmdId) enableNtfs cMode
  pure (cmdId, connId)

joinAgentConnectionAsync :: ChatMonad m => User -> Bool -> ConnectionRequestUri c -> ConnInfo -> m (CommandId, ConnId)
joinAgentConnectionAsync user enableNtfs cReqUri cInfo = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing CFJoinConn
  connId <- withAgent $ \a -> joinConnectionAsync a (aCorrId cmdId) enableNtfs cReqUri cInfo
  pure (cmdId, connId)

allowAgentConnectionAsync :: (MsgEncodingI e, ChatMonad m) => User -> Connection -> ConfirmationId -> ChatMsgEvent e -> m ()
allowAgentConnectionAsync user conn@Connection {connId} confId msg = do
  cmdId <- withStore' $ \db -> createCommand db user (Just connId) CFAllowConn
  withAgent $ \a -> allowConnectionAsync a (aCorrId cmdId) (aConnId conn) confId $ directMessage msg
  withStore' $ \db -> updateConnectionStatus db conn ConnAccepted

agentAcceptContactAsync :: (MsgEncodingI e, ChatMonad m) => User -> Bool -> InvitationId -> ChatMsgEvent e -> m (CommandId, ConnId)
agentAcceptContactAsync user enableNtfs invId msg = do
  cmdId <- withStore' $ \db -> createCommand db user Nothing CFAcceptContact
  connId <- withAgent $ \a -> acceptContactAsync a (aCorrId cmdId) enableNtfs invId $ directMessage msg
  pure (cmdId, connId)

deleteAgentConnectionAsync :: ChatMonad m => User -> Connection -> m ()
deleteAgentConnectionAsync user Connection {agentConnId, connId} =
  deleteAgentConnectionAsync' user connId agentConnId

deleteAgentConnectionAsync' :: ChatMonad m => User -> Int64 -> AgentConnId -> m ()
deleteAgentConnectionAsync' user connId (AgentConnId acId) = do
  cmdId <- withStore' $ \db -> createCommand db user (Just connId) CFDeleteConn
  withAgent $ \a -> deleteConnectionAsync a (aCorrId cmdId) acId

userProfileToSend :: User -> Maybe Profile -> Maybe Contact -> Profile
userProfileToSend user@User {profile = p} incognitoProfile ct =
  let p' = fromMaybe (fromLocalProfile p) incognitoProfile
      userPrefs = maybe (preferences' user) (const Nothing) incognitoProfile
   in (p' :: Profile) {preferences = Just . toChatPrefs $ mergePreferences (userPreferences <$> ct) userPrefs}

createFeatureChangedItems :: (MsgDirectionI d, ChatMonad m) => User -> Contact -> Contact -> (Contact -> ChatDirection 'CTDirect d) -> (ChatFeature -> PrefEnabled -> CIContent d) -> m ()
createFeatureChangedItems user Contact {mergedPreferences = cups} ct'@Contact {mergedPreferences = cups'} chatDir ciContent =
  forM_ allChatFeatures $ \f -> do
    let ContactUserPreference {enabled} = getContactUserPreference f cups
        ContactUserPreference {enabled = enabled'} = getContactUserPreference f cups'
    unless (enabled == enabled') $
      createInternalChatItem user (chatDir ct') (ciContent f enabled') Nothing

createGroupFeatureChangedItems :: (MsgDirectionI d, ChatMonad m) => User -> ChatDirection 'CTGroup d -> (GroupFeature -> GroupPreference -> CIContent d) -> GroupProfile -> GroupProfile -> m ()
createGroupFeatureChangedItems user cd ciContent p p' =
  forM_ allGroupFeatures $ \f -> do
    let pref = getGroupPreference f $ groupPreferences p
        pref' = getGroupPreference f $ groupPreferences p'
    unless (pref == pref') $
      createInternalChatItem user cd (ciContent f pref') Nothing

sameGroupProfileInfo :: GroupProfile -> GroupProfile -> Bool
sameGroupProfileInfo p p' = p {groupPreferences = Nothing} == p' {groupPreferences = Nothing}

createInternalChatItem :: forall c d m. (ChatTypeI c, MsgDirectionI d, ChatMonad m) => User -> ChatDirection c d -> CIContent d -> Maybe UTCTime -> m ()
createInternalChatItem user cd content itemTs_ = do
  createdAt <- liftIO getCurrentTime
  let itemTs = fromMaybe createdAt itemTs_
  ciId <- withStore' $ \db -> createNewChatItemNoMsg db user cd content itemTs createdAt
  ci <- liftIO $ mkChatItem cd ciId content Nothing Nothing Nothing itemTs createdAt
  toView $ CRNewChatItem $ AChatItem (chatTypeI @c) (msgDirection @d) (toChatInfo cd) ci

getCreateActiveUser :: SQLiteStore -> IO User
getCreateActiveUser st = do
  user <-
    withTransaction st getUsers >>= \case
      [] -> newUser
      users -> maybe (selectUser users) pure (find activeUser users)
  putStrLn $ "Current user: " <> userStr user
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
          withTransaction st (\db -> runExceptT $ createUser db Profile {displayName, fullName, image = Nothing, preferences = Nothing} True) >>= \case
            Left SEDuplicateName -> do
              putStrLn "chosen display name is already used by another profile on this device, choose another one"
              loop
            Left e -> putStrLn ("database error " <> show e) >> exitFailure
            Right user -> pure user
    selectUser :: [User] -> IO User
    selectUser [user] = do
      withTransaction st (`setActiveUser` userId user)
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
                withTransaction st (`setActiveUser` userId user)
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

showMsgToast :: (MonadUnliftIO m, MonadReader ChatController m) => Text -> MsgContent -> Maybe MarkdownList -> m ()
showMsgToast from mc md_ = showToast from $ maybe (msgContentText mc) (mconcat . map hideSecret) md_
  where
    hideSecret :: FormattedText -> Text
    hideSecret FormattedText {format = Just Secret} = "..."
    hideSecret FormattedText {text} = text

showToast :: (MonadUnliftIO m, MonadReader ChatController m) => Text -> Text -> m ()
showToast title text = atomically . (`writeTBQueue` Notification {title, text}) =<< asks notifyQ

notificationSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
notificationSubscriber = do
  ChatController {notifyQ, sendNotification} <- ask
  forever $ atomically (readTBQueue notifyQ) >>= liftIO . sendNotification

withUser' :: ChatMonad m => (User -> m a) -> m a
withUser' action =
  asks currentUser
    >>= readTVarIO
    >>= maybe (throwChatError CENoActiveUser) action

withUser :: ChatMonad m => (User -> m a) -> m a
withUser action = withUser' $ \user ->
  ifM chatStarted (action user) (throwChatError CEChatNotStarted)

chatStarted :: ChatMonad m => m Bool
chatStarted = fmap isJust . readTVarIO =<< asks agentAsync

withAgent :: ChatMonad m => (AgentClient -> ExceptT AgentErrorType m a) -> m a
withAgent action =
  asks smpAgent
    >>= runExceptT . action
    >>= liftEither . first ChatErrorAgent

withStore' :: ChatMonad m => (DB.Connection -> IO a) -> m a
withStore' action = withStore $ liftIO . action

withStore ::
  ChatMonad m =>
  (DB.Connection -> ExceptT StoreError IO a) ->
  m a
withStore action = do
  ChatController {chatStore} <- ask
  liftEitherError ChatErrorStore $
    withTransaction chatStore (runExceptT . action) `E.catch` handleInternal
  where
    handleInternal :: E.SomeException -> IO (Either StoreError a)
    handleInternal = pure . Left . SEInternalError . show

chatCommandP :: Parser ChatCommand
chatCommandP =
  choice
    [ "/mute " *> ((`ShowMessages` False) <$> chatNameP'),
      "/unmute " *> ((`ShowMessages` True) <$> chatNameP'),
      ("/user " <|> "/u ") *> (CreateActiveUser <$> userProfile),
      ("/user" <|> "/u") $> ShowActiveUser,
      "/_start subscribe=" *> (StartChat <$> onOffP <* " expire=" <*> onOffP),
      "/_start" $> StartChat True True,
      "/_stop" $> APIStopChat,
      "/_app activate" $> APIActivateChat,
      "/_app suspend " *> (APISuspendChat <$> A.decimal),
      "/_resubscribe all" $> ResubscribeAllConnections,
      "/_files_folder " *> (SetFilesFolder <$> filePath),
      "/_db export " *> (APIExportArchive <$> jsonP),
      "/_db import " *> (APIImportArchive <$> jsonP),
      "/_db delete" $> APIDeleteStorage,
      "/_db encryption " *> (APIStorageEncryption <$> jsonP),
      "/db encrypt " *> (APIStorageEncryption . DBEncryptionConfig "" <$> dbKeyP),
      "/db key " *> (APIStorageEncryption <$> (DBEncryptionConfig <$> dbKeyP <* A.space <*> dbKeyP)),
      "/db decrypt " *> (APIStorageEncryption . (`DBEncryptionConfig` "") <$> dbKeyP),
      "/sql chat " *> (ExecChatStoreSQL <$> textP),
      "/sql agent " *> (ExecAgentStoreSQL <$> textP),
      "/_get chats" *> (APIGetChats <$> (" pcc=on" $> True <|> " pcc=off" $> False <|> pure False)),
      "/_get chat " *> (APIGetChat <$> chatRefP <* A.space <*> chatPaginationP <*> optional (" search=" *> stringP)),
      "/_get items count=" *> (APIGetChatItems <$> A.decimal),
      "/_send " *> (APISendMessage <$> chatRefP <*> (" json " *> jsonP <|> " text " *> (ComposedMessage Nothing Nothing <$> mcTextP))),
      "/_update item " *> (APIUpdateChatItem <$> chatRefP <* A.space <*> A.decimal <* A.space <*> msgContentP),
      "/_delete item " *> (APIDeleteChatItem <$> chatRefP <* A.space <*> A.decimal <* A.space <*> ciDeleteMode),
      "/_read chat " *> (APIChatRead <$> chatRefP <*> optional (A.space *> ((,) <$> ("from=" *> A.decimal) <* A.space <*> ("to=" *> A.decimal)))),
      "/_unread chat " *> (APIChatUnread <$> chatRefP <* A.space <*> onOffP),
      "/_delete " *> (APIDeleteChat <$> chatRefP),
      "/_clear chat " *> (APIClearChat <$> chatRefP),
      "/_accept " *> (APIAcceptContact <$> A.decimal),
      "/_reject " *> (APIRejectContact <$> A.decimal),
      "/_call invite @" *> (APISendCallInvitation <$> A.decimal <* A.space <*> jsonP),
      ("/call @" <|> "/call ") *> (SendCallInvitation <$> displayName <*> pure defaultCallType),
      "/_call reject @" *> (APIRejectCall <$> A.decimal),
      "/_call offer @" *> (APISendCallOffer <$> A.decimal <* A.space <*> jsonP),
      "/_call answer @" *> (APISendCallAnswer <$> A.decimal <* A.space <*> jsonP),
      "/_call extra @" *> (APISendCallExtraInfo <$> A.decimal <* A.space <*> jsonP),
      "/_call end @" *> (APIEndCall <$> A.decimal),
      "/_call status @" *> (APICallStatus <$> A.decimal <* A.space <*> strP),
      "/_call get" $> APIGetCallInvitations,
      "/_profile " *> (APIUpdateProfile <$> jsonP),
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
      -- /smp_servers is deprecated, use /smp and /_smp
      "/smp_servers default" $> SetUserSMPServers (SMPServersConfig []),
      "/smp_servers " *> (SetUserSMPServers . SMPServersConfig . map toServerCfg <$> smpServersP),
      "/smp_servers" $> GetUserSMPServers,
      "/smp default" $> SetUserSMPServers (SMPServersConfig []),
      "/smp test " *> (TestSMPServer <$> strP),
      "/_smp " *> (SetUserSMPServers <$> jsonP),
      "/smp " *> (SetUserSMPServers . SMPServersConfig . map toServerCfg <$> smpServersP),
      "/smp" $> GetUserSMPServers,
      "/_ttl " *> (APISetChatItemTTL <$> ciTTLDecimal),
      "/ttl " *> (APISetChatItemTTL <$> ciTTL),
      "/ttl" $> APIGetChatItemTTL,
      "/_network " *> (APISetNetworkConfig <$> jsonP),
      ("/network " <|> "/net ") *> (APISetNetworkConfig <$> netCfgP),
      ("/network" <|> "/net") $> APIGetNetworkConfig,
      "/_settings " *> (APISetChatSettings <$> chatRefP <* A.space <*> jsonP),
      "/_info #" *> (APIGroupMemberInfo <$> A.decimal <* A.space <*> A.decimal),
      "/_info @" *> (APIContactInfo <$> A.decimal),
      ("/info #" <|> "/i #") *> (GroupMemberInfo <$> displayName <* A.space <* optional (A.char '@') <*> displayName),
      ("/info @" <|> "/info " <|> "/i @" <|> "/i ") *> (ContactInfo <$> displayName),
      "/_switch #" *> (APISwitchGroupMember <$> A.decimal <* A.space <*> A.decimal),
      "/_switch @" *> (APISwitchContact <$> A.decimal),
      "/switch #" *> (SwitchGroupMember <$> displayName <* A.space <* optional (A.char '@') <*> displayName),
      ("/switch @" <|> "/switch ") *> (SwitchContact <$> displayName),
      ("/help files" <|> "/help file" <|> "/hf") $> ChatHelp HSFiles,
      ("/help groups" <|> "/help group" <|> "/hg") $> ChatHelp HSGroups,
      ("/help address" <|> "/ha") $> ChatHelp HSMyAddress,
      ("/help messages" <|> "/hm") $> ChatHelp HSMessages,
      ("/help settings" <|> "/hs") $> ChatHelp HSSettings,
      ("/help" <|> "/h") $> ChatHelp HSMain,
      ("/group #" <|> "/group " <|> "/g #" <|> "/g ") *> (NewGroup <$> groupProfile),
      "/_group " *> (NewGroup <$> jsonP),
      ("/add #" <|> "/add " <|> "/a #" <|> "/a ") *> (AddMember <$> displayName <* A.space <* optional (A.char '@') <*> displayName <*> memberRole),
      ("/join #" <|> "/join " <|> "/j #" <|> "/j ") *> (JoinGroup <$> displayName),
      ("/member role #" <|> "/member role " <|> "/mr #" <|> "/mr ") *> (MemberRole <$> displayName <* A.space <* optional (A.char '@') <*> displayName <*> memberRole),
      ("/remove #" <|> "/remove " <|> "/rm #" <|> "/rm ") *> (RemoveMember <$> displayName <* A.space <* optional (A.char '@') <*> displayName),
      ("/leave #" <|> "/leave " <|> "/l #" <|> "/l ") *> (LeaveGroup <$> displayName),
      ("/delete #" <|> "/d #") *> (DeleteGroup <$> displayName),
      ("/delete @" <|> "/delete " <|> "/d @" <|> "/d ") *> (DeleteContact <$> displayName),
      "/clear #" *> (ClearGroup <$> displayName),
      ("/clear @" <|> "/clear ") *> (ClearContact <$> displayName),
      ("/members #" <|> "/members " <|> "/ms #" <|> "/ms ") *> (ListMembers <$> displayName),
      ("/groups" <|> "/gs") $> ListGroups,
      "/_group_profile #" *> (APIUpdateGroupProfile <$> A.decimal <* A.space <*> jsonP),
      -- TODO group profile update via terminal should not reset image and preferences to Nothing (now it does)
      ("/group_profile #" <|> "/gp #" <|> "/group_profile " <|> "/gp ") *> (UpdateGroupProfile <$> displayName <* A.space <*> groupProfile),
      "/_create link #" *> (APICreateGroupLink <$> A.decimal),
      "/_delete link #" *> (APIDeleteGroupLink <$> A.decimal),
      "/_get link #" *> (APIGetGroupLink <$> A.decimal),
      "/create link #" *> (CreateGroupLink <$> displayName),
      "/delete link #" *> (DeleteGroupLink <$> displayName),
      "/show link #" *> (ShowGroupLink <$> displayName),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayName <* A.space <*> pure Nothing <*> quotedMsg <*> A.takeByteString),
      (">#" <|> "> #") *> (SendGroupMessageQuote <$> displayName <* A.space <* optional (A.char '@') <*> (Just <$> displayName) <* A.space <*> quotedMsg <*> A.takeByteString),
      ("/contacts" <|> "/cs") $> ListContacts,
      ("/connect " <|> "/c ") *> (Connect <$> ((Just <$> strP) <|> A.takeByteString $> Nothing)),
      ("/connect" <|> "/c") $> AddContact,
      SendMessage <$> chatNameP <* A.space <*> A.takeByteString,
      (">@" <|> "> @") *> sendMsgQuote (AMsgDirection SMDRcv),
      (">>@" <|> ">> @") *> sendMsgQuote (AMsgDirection SMDSnd),
      ("\\ " <|> "\\") *> (DeleteMessage <$> chatNameP <* A.space <*> A.takeByteString),
      ("! " <|> "!") *> (EditMessage <$> chatNameP <* A.space <*> (quotedMsg <|> pure "") <*> A.takeByteString),
      "/feed " *> (SendMessageBroadcast <$> A.takeByteString),
      ("/tail" <|> "/t") *> (LastMessages <$> optional (A.space *> chatNameP) <*> msgCountP <*> pure Nothing),
      ("/search" <|> "/?") *> (LastMessages <$> optional (A.space *> chatNameP) <*> msgCountP <*> (Just <$> (A.space *> stringP))),
      ("/file " <|> "/f ") *> (SendFile <$> chatNameP' <* A.space <*> filePath),
      ("/image " <|> "/img ") *> (SendImage <$> chatNameP' <* A.space <*> filePath),
      ("/fforward " <|> "/ff ") *> (ForwardFile <$> chatNameP' <* A.space <*> A.decimal),
      ("/image_forward " <|> "/imgf ") *> (ForwardImage <$> chatNameP' <* A.space <*> A.decimal),
      ("/freceive " <|> "/fr ") *> (ReceiveFile <$> A.decimal <*> optional (" inline=" *> onOffP) <*> optional (A.space *> filePath)),
      ("/fcancel " <|> "/fc ") *> (CancelFile <$> A.decimal),
      ("/fstatus " <|> "/fs ") *> (FileStatus <$> A.decimal),
      "/simplex" $> ConnectSimplex,
      ("/address" <|> "/ad") $> CreateMyAddress,
      ("/delete_address" <|> "/da") $> DeleteMyAddress,
      ("/show_address" <|> "/sa") $> ShowMyAddress,
      "/auto_accept " *> (AddressAutoAccept <$> autoAcceptP),
      ("/accept @" <|> "/accept " <|> "/ac @" <|> "/ac ") *> (AcceptContact <$> displayName),
      ("/reject @" <|> "/reject " <|> "/rc @" <|> "/rc ") *> (RejectContact <$> displayName),
      ("/markdown" <|> "/m") $> ChatHelp HSMarkdown,
      ("/welcome" <|> "/w") $> Welcome,
      "/profile_image " *> (UpdateProfileImage . Just . ImageData <$> imageP),
      "/profile_image" $> UpdateProfileImage Nothing,
      ("/profile " <|> "/p ") *> (uncurry UpdateProfile <$> userNames),
      ("/profile" <|> "/p") $> ShowProfile,
      "/set voice #" *> (SetGroupFeature GFVoice <$> displayName <*> (A.space *> strP)),
      "/set voice @" *> (SetContactFeature CFVoice <$> displayName <*> optional (A.space *> strP)),
      "/set voice " *> (SetUserFeature CFVoice <$> strP),
      "/set delete #" *> (SetGroupFeature GFFullDelete <$> displayName <*> (A.space *> strP)),
      "/set delete @" *> (SetContactFeature CFFullDelete <$> displayName <*> optional (A.space *> strP)),
      "/set delete " *> (SetUserFeature CFFullDelete <$> strP),
      "/set direct #" *> (SetGroupFeature GFDirectMessages <$> displayName <*> (A.space *> strP)),
      "/incognito " *> (SetIncognito <$> onOffP),
      ("/quit" <|> "/q" <|> "/exit") $> QuitChat,
      ("/version" <|> "/v") $> ShowVersion,
      "/debug locks" $> DebugLocks
    ]
  where
    choice = A.choice . map (\p -> p <* A.takeWhile (== ' ') <* A.endOfInput)
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
    sendMsgQuote msgDir = SendMessageQuote <$> displayName <* A.space <*> pure msgDir <*> quotedMsg <*> A.takeByteString
    quotedMsg = A.char '(' *> A.takeTill (== ')') <* A.char ')' <* optional A.space
    refChar c = c > ' ' && c /= '#' && c /= '@'
    onOffP = ("on" $> True) <|> ("off" $> False)
    userNames = do
      cName <- displayName
      fullName <- fullNameP cName
      pure (cName, fullName)
    userProfile = do
      (cName, fullName) <- userNames
      pure Profile {displayName = cName, fullName, image = Nothing, preferences = Nothing}
    jsonP :: J.FromJSON a => Parser a
    jsonP = J.eitherDecodeStrict' <$?> A.takeByteString
    groupProfile = do
      gName <- displayName
      fullName <- fullNameP gName
      let groupPreferences = Just (emptyGroupPrefs :: GroupPreferences) {directMessages = Just GroupPreference {enable = FEOn}}
      pure GroupProfile {displayName = gName, fullName, image = Nothing, groupPreferences}
    fullNameP name = do
      n <- (A.space *> A.takeByteString) <|> pure ""
      pure $ if B.null n then name else safeDecodeUtf8 n
    textP = safeDecodeUtf8 <$> A.takeByteString
    stringP = T.unpack . safeDecodeUtf8 <$> A.takeByteString
    filePath = stringP
    memberRole =
      A.choice
        [ " owner" $> GROwner,
          " admin" $> GRAdmin,
          " member" $> GRMember,
          -- " author" $> GRAuthor,
          pure GRAdmin
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
    netCfgP = do
      socksProxy <- "socks=" *> ("off" $> Nothing <|> "on" $> Just defaultSocksProxy <|> Just <$> strP)
      t_ <- optional $ " timeout=" *> A.decimal
      let tcpTimeout = 1000000 * fromMaybe (maybe 5 (const 10) socksProxy) t_
      pure $ fullNetworkConfig socksProxy tcpTimeout
    dbKeyP = nonEmptyKey <$?> strP
    nonEmptyKey k@(DBEncryptionKey s) = if null s then Left "empty key" else Right k
    autoAcceptP =
      ifM
        onOffP
        (Just <$> (AutoAccept <$> (" incognito=" *> onOffP <|> pure False) <*> optional (A.space *> msgContentP)))
        (pure Nothing)
    toServerCfg server = ServerCfg {server, preset = False, tested = Nothing, enabled = True}

adminContactReq :: ConnReqContact
adminContactReq =
  either error id $ strDecode "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"
