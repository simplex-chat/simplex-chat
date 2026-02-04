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

module Simplex.Chat.Library.Subscriber where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (lefts, partitionEithers, rights)
import Data.Foldable (foldr')
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4
import Data.Word (Word32)
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Delivery
import Simplex.Chat.Library.Internal
import Simplex.Chat.Messages
import Simplex.Chat.Messages.Batch (batchDeliveryTasks1)
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.ProfileGenerator (generateRandomProfile)
import Simplex.Chat.Protocol
import Simplex.Chat.Store
import Simplex.Chat.Store.Connections
import Simplex.Chat.Store.ContactRequest
import Simplex.Chat.Store.Delivery
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Files
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Messages
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.RelayRequests
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.MemberRelations
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.FileTransfer.Description (ValidFileDescription)
import qualified Simplex.FileTransfer.Description as FD
import Simplex.FileTransfer.Protocol (FilePartyI)
import qualified Simplex.FileTransfer.Transport as XFTP
import Simplex.FileTransfer.Types (FileErrorType (..), RcvFileId, SndFileId)
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Client (getAgentWorker, temporaryOrHostError, waitForUserNetwork, waitForWork, waitWhileSuspended, withWorkItems, withWork_)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..), Worker (..))
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Agent.Protocol as AP (AgentErrorType (..))
import Simplex.Messaging.Agent.RetryInterval (withRetryInterval)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Client (NetworkRequestMode (..), ProxyClientError (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..), pattern PQEncOff, pattern PQEncOn, pattern PQSupportOff, pattern PQSupportOn)
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Protocol (ErrorType (..), MsgFlags (..))
import qualified Simplex.Messaging.Protocol as SMP
import Simplex.Messaging.ServiceScheme (ServiceScheme (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (TransportError (..))
import Simplex.Messaging.Util
import Simplex.Messaging.Version
import qualified System.FilePath as FP
import Text.Read (readMaybe)
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Directory
import UnliftIO.STM

smallGroupsRcptsMemLimit :: Int
smallGroupsRcptsMemLimit = 20

processAgentMessage :: ACorrId -> ConnId -> AEvent 'AEConn -> CM ()
processAgentMessage _ _ (DEL_RCVQS delQs) =
  toView $ CEvtAgentRcvQueuesDeleted $ L.map rcvQ delQs
  where
    rcvQ (connId, server, rcvId, err_) = DeletedRcvQueue (AgentConnId connId) server (AgentQueueId rcvId) err_
processAgentMessage _ _ (DEL_CONNS connIds) =
  toView $ CEvtAgentConnsDeleted $ L.map AgentConnId connIds
processAgentMessage _ "" (ERR e) =
  eToView $ ChatErrorAgent e (AgentConnId "") Nothing
processAgentMessage corrId connId msg = do
  lockEntity <- critical connId (withStore (`getChatLockEntity` AgentConnId connId))
  withEntityLock "processAgentMessage" lockEntity $ do
    vr <- chatVersionRange
    -- getUserByAConnId never throws logical errors, only SEDBBusyError can be thrown here
    critical connId (withStore' (`getUserByAConnId` AgentConnId connId)) >>= \case
      Just user -> processAgentMessageConn vr user corrId connId msg `catchAllErrors` eToView
      _ -> throwChatError $ CENoConnectionUser (AgentConnId connId)

-- CRITICAL error will be shown to the user as alert with restart button in Android/desktop apps.
-- SEDBBusyError will only be thrown on IO exceptions or SQLError during DB queries,
-- e.g. when database is locked or busy for longer than 3s.
-- In this case there is no better mitigation than showing alert:
-- - without ACK the message delivery will be stuck,
-- - with ACK message will be lost, as it failed to be saved.
-- Full app restart is likely to resolve database condition and the message will be received and processed again.
critical :: ConnId -> CM a -> CM a
critical agentConnId a =
  a `catchAllErrors` \case
    ChatErrorStore SEDBBusyError {message} -> throwError $ ChatErrorAgent (CRITICAL True message) (AgentConnId agentConnId) Nothing
    e -> throwError e

processAgentMessageNoConn :: AEvent 'AENone -> CM ()
processAgentMessageNoConn = \case
  CONNECT p h -> hostEvent $ CEvtHostConnected p h
  DISCONNECT p h -> hostEvent $ CEvtHostDisconnected p h
  DOWN srv conns -> serverEvent srv SSPending conns
  UP srv conns -> serverEvent srv SSActive conns
  SUSPENDED -> toView CEvtChatSuspended
  DEL_USER agentUserId -> toView $ CEvtAgentUserDeleted agentUserId
  ERRS cErrs -> errsEvent $ L.toList cErrs
  where
    hostEvent :: ChatEvent -> CM ()
    hostEvent = whenM (asks $ hostEvents . config) . toView
    serverEvent :: SMPServer -> SubscriptionStatus -> [ConnId] -> CM ()
    serverEvent srv nsStatus conns = toView $ CEvtSubscriptionStatus srv nsStatus $ map AgentConnId conns
    errsEvent :: [(ConnId, AgentErrorType)] -> CM ()
    errsEvent = toView . CEvtChatErrors . map (\(cId, e) -> ChatErrorAgent e (AgentConnId cId) Nothing)

processAgentMsgSndFile :: ACorrId -> SndFileId -> AEvent 'AESndFile -> CM ()
processAgentMsgSndFile _corrId aFileId msg = do
  (cRef_, fileId) <- withStore (`getXFTPSndFileDBIds` AgentSndFileId aFileId)
  withEntityLock_ cRef_ . withFileLock "processAgentMsgSndFile" fileId $
    withStore' (`getUserByASndFileId` AgentSndFileId aFileId) >>= \case
      Just user -> process user fileId `catchAllErrors` eToView
      _ -> do
        lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
        throwChatError $ CENoSndFileUser $ AgentSndFileId aFileId
  where
    withEntityLock_ :: Maybe ChatRef -> CM a -> CM a
    withEntityLock_ = \case
      Just (ChatRef CTDirect contactId _) -> withContactLock "processAgentMsgSndFile" contactId
      Just (ChatRef CTGroup groupId _scope) -> withGroupLock "processAgentMsgSndFile" groupId
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
          toView $ CEvtSndFileProgressXFTP user ci ft sndProgress sndTotal
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
                    Nothing -> xftpSndFileRedirect user fileId rfd >>= toView . CEvtSndFileRedirectStartXFTP user ft
                    Just _ -> sendFileError (FileErrOther "chaining redirects") "Prohibit chaining redirects" vr ft
                  rfds' -> do
                    -- we have 1 chunk - use it as URI whether it is redirect or not
                    ft' <- maybe (pure ft) (\fId -> withStore $ \db -> getFileTransferMeta db user fId) xftpRedirectFor
                    toView $ CEvtSndStandaloneFileComplete user ft' $ map (decodeLatin1 . strEncode . FD.fileDescriptionURI) rfds'
            Just (AChatItem _ d cInfo _ci@ChatItem {meta = CIMeta {itemSharedMsgId = msgId_, itemDeleted}}) ->
              case (msgId_, itemDeleted) of
                (Just sharedMsgId, Nothing) -> do
                  when (length rfds < length sfts) $ throwChatError $ CEInternalError "not enough XFTP file descriptions to send"
                  -- TODO either update database status or move to SFPROG
                  toView $ CEvtSndFileProgressXFTP user ci ft 1 1
                  case (rfds, sfts, d, cInfo) of
                    (rfd : extraRFDs, sft : _, SMDSnd, DirectChat ct) -> do
                      withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText extraRFDs)
                      conn@Connection {connId} <- liftEither $ contactSendConn_ ct
                      sendFileDescriptions (ConnectionId connId) ((conn, sft, fileDescrText rfd) :| []) sharedMsgId >>= \case
                        Just rs -> case L.last rs of
                          Right ([msgDeliveryId], _) ->
                            withStore' $ \db -> updateSndFTDeliveryXFTP db sft msgDeliveryId
                          Right (deliveryIds, _) -> eToView $ ChatError $ CEInternalError $ "SFDONE, sendFileDescriptions: expected 1 delivery id, got " <> show (length deliveryIds)
                          Left e -> eToView e
                        Nothing -> eToView $ ChatError $ CEInternalError "SFDONE, sendFileDescriptions: expected at least 1 result"
                      lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
                    (_, _, SMDSnd, GroupChat g@GroupInfo {groupId} _scope) -> do
                      -- TODO [relays] single description for all recipients
                      ms <- getRecipients
                      let rfdsMemberFTs = zipWith (\rfd (conn, sft) -> (conn, sft, fileDescrText rfd)) rfds (memberFTs ms)
                          extraRFDs = drop (length rfdsMemberFTs) rfds
                      withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText extraRFDs)
                      forM_ (L.nonEmpty rfdsMemberFTs) $ \rfdsMemberFTs' ->
                        sendFileDescriptions (GroupId groupId) rfdsMemberFTs' sharedMsgId
                      ci' <- withStore $ \db -> do
                        liftIO $ updateCIFileStatus db user fileId CIFSSndComplete
                        getChatItemByFileId db vr user fileId
                      lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
                      toView $ CEvtSndFileCompleteXFTP user ci' ft
                      where
                        getRecipients
                          | useRelays' g = withStore' $ \db -> getGroupRelayMembers db vr user g
                          | otherwise = withStore' $ \db -> getGroupMembers db vr user g
                        memberFTs :: [GroupMember] -> [(Connection, SndFileTransfer)]
                        memberFTs ms = M.elems $ M.intersectionWith (,) (M.fromList mConns') (M.fromList sfts')
                          where
                            mConns' = mapMaybe readyMemberConn ms
                            sfts' = mapMaybe (\sft@SndFileTransfer {groupMemberId} -> (,sft) <$> groupMemberId) sfts
                    _ -> pure ()
                _ -> pure () -- TODO error?
        SFWARN e -> do
          let err = tshow e
          logWarn $ "Sent file warning: " <> err
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId (CIFSSndWarning $ agentFileError e)
            lookupChatItemByFileId db vr user fileId
          toView $ CEvtSndFileWarning user ci ft err
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
          unless (null errs') $ toView $ CEvtChatErrors errs'
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
              (conn, MsgFlags {notification = hasNotification XMsgFileDescr_}, (vrValue msgBody, [msgId]))
        sendFileError :: FileError -> Text -> VersionRangeChat -> FileTransferMeta -> CM ()
        sendFileError ferr err vr ft = do
          logError $ "Sent file error: " <> err
          ci <- withStore $ \db -> do
            liftIO $ updateFileCancelled db user fileId (CIFSSndError ferr)
            lookupChatItemByFileId db vr user fileId
          lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
          toView $ CEvtSndFileError user ci ft err

agentFileError :: AgentErrorType -> FileError
agentFileError = \case
  XFTP _ XFTP.AUTH -> FileErrAuth
  XFTP srv (XFTP.BLOCKED info) -> FileErrBlocked srv info
  FILE NO_FILE -> FileErrNoFile
  BROKER _ e -> brokerError FileErrRelay e
  e -> FileErrOther $ tshow e
  where
    brokerError srvErr = \case
      HOST -> srvErr SrvErrHost
      SMP.TRANSPORT TEVersion -> srvErr SrvErrVersion
      e -> srvErr . SrvErrOther $ tshow e

processAgentMsgRcvFile :: ACorrId -> RcvFileId -> AEvent 'AERcvFile -> CM ()
processAgentMsgRcvFile _corrId aFileId msg = do
  (cRef_, fileId) <- withStore (`getXFTPRcvFileDBIds` AgentRcvFileId aFileId)
  withEntityLock_ cRef_ . withFileLock "processAgentMsgRcvFile" fileId $
    withStore' (`getUserByARcvFileId` AgentRcvFileId aFileId) >>= \case
      Just user -> process user fileId `catchAllErrors` eToView
      _ -> do
        lift $ withAgent' (`xftpDeleteRcvFile` aFileId)
        throwChatError $ CENoRcvFileUser $ AgentRcvFileId aFileId
  where
    withEntityLock_ :: Maybe ChatRef -> CM a -> CM a
    withEntityLock_ = \case
      Just (ChatRef CTDirect contactId _) -> withContactLock "processAgentMsgRcvFile" contactId
      Just (ChatRef CTGroup groupId _scope) -> withGroupLock "processAgentMsgRcvFile" groupId
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
          toView $ CEvtRcvFileProgressXFTP user ci rcvProgress rcvTotal ft
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
              toView $ maybe (CEvtRcvStandaloneFileComplete user fsTargetPath ft) (CEvtRcvFileComplete user) ci_
        RFWARN e -> do
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId (CIFSRcvWarning $ agentFileError e)
            lookupChatItemByFileId db vr user fileId
          toView $ CEvtRcvFileWarning user ci e ft
        RFERR e
          | e == FILE NOT_APPROVED -> do
              aci_ <- resetRcvCIFileStatus user fileId CIFSRcvAborted
              forM_ aci_ cleanupACIFile
              agentXFTPDeleteRcvFile aFileId fileId
              forM_ aci_ $ \aci -> toView $ CEvtChatItemUpdated user aci
          | otherwise -> do
              aci_ <- withStore $ \db -> do
                liftIO $ updateFileCancelled db user fileId (CIFSRcvError $ agentFileError e)
                lookupChatItemByFileId db vr user fileId
              forM_ aci_ cleanupACIFile
              agentXFTPDeleteRcvFile aFileId fileId
              toView $ CEvtRcvFileError user aci_ e ft

type ShouldDeleteGroupConns = Bool

processAgentMessageConn :: VersionRangeChat -> User -> ACorrId -> ConnId -> AEvent 'AEConn -> CM ()
processAgentMessageConn vr user@User {userId} corrId agentConnId agentMessage = do
  -- Missing connection/entity errors here will be sent to the view but not shown as CRITICAL alert,
  -- as in this case no need to ACK message - we can't process messages for this connection anyway.
  -- SEDBException will be re-trown as CRITICAL as it is likely to indicate a temporary database condition
  -- that will be resolved with app restart.
  entity <- critical agentConnId $ withStore (\db -> getConnectionEntity db vr user $ AgentConnId agentConnId) >>= updateConnStatus
  case agentMessage of
    END -> case entity of
      RcvDirectMsgConnection _ (Just ct) -> toView $ CEvtContactAnotherClient user ct
      _ -> toView $ CEvtSubscriptionEnd user entity
    MSGNTF msgId msgTs_ -> toView $ CEvtNtfMessage user entity $ ntfMsgAckInfo msgId msgTs_
    _ -> case entity of
      RcvDirectMsgConnection conn contact_ ->
        processDirectMessage agentMessage entity conn contact_
      RcvGroupMsgConnection conn gInfo m ->
        processGroupMessage agentMessage entity conn gInfo m
      UserContactConnection conn uc ->
        processContactConnMessage agentMessage entity conn uc
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
      JOINED True _ -> Just ConnSndReady
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
          (conn'', gInfo_) <- saveConnInfo conn' connInfo
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
          let profileToSend = case gInfo_ of
                Just gInfo -> userProfileInGroup user gInfo (fromLocalProfile <$> incognitoProfile)
                Nothing -> userProfileDirect user (fromLocalProfile <$> incognitoProfile) Nothing True
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
        -- TODO [certs rcv]
        JOINED _ _serviceId ->
          -- [async agent commands] continuation on receiving JOINED
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        QCONT ->
          void $ continueSending connEntity conn
        MWARN _ err ->
          processConnMWARN connEntity conn err
        MERR _ err -> do
          eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
          processConnMERR connEntity conn err
        MERRS _ err -> do
          -- error cannot be AUTH error here
          eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        ERR err -> do
          eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()
      Just ct@Contact {contactId} -> case agentMsg of
        -- TODO [certs rcv]
        INV (ACR _ cReq) _serviceId ->
          -- [async agent commands] XGrpMemIntro continuation on receiving INV
          withCompletedCommand conn agentMsg $ \_ ->
            case cReq of
              CRInvitationUri _ _ -> withStore' $ \db -> setConnConnReqInv db user connId cReq
              CRContactUri _ -> throwChatError $ CECommandError "unexpected ConnectionRequestUri type"
        MSG msgMeta _msgFlags msgBody -> do
          tags <- newTVarIO []
          withAckMessage "contact msg" agentConnId msgMeta True (Just tags) $ \eInfo -> do
            let MsgMeta {pqEncryption} = msgMeta
            (ct', conn') <- updateContactPQRcv user ct conn pqEncryption
            checkIntegrityCreateItem (CDDirectRcv ct') msgMeta `catchAllErrors` \_ -> pure ()
            forM_ aChatMsgs $ \case
              Right (ACMsg _ chatMsg) ->
                processEvent ct' conn' tags eInfo chatMsg `catchAllErrors` \e -> eToView e
              Left e -> do
                atomically $ modifyTVar' tags ("error" :)
                logInfo $ "contact msg=error " <> eInfo <> " " <> tshow e
                eToView (ChatError . CEException $ "error parsing chat message: " <> e)
            withRcpt <- checkSendRcpt ct' $ rights aChatMsgs -- not crucial to use ct'' from processEvent
            pure (withRcpt, False)
          where
            aChatMsgs = parseChatMessages msgBody
            processEvent :: Contact -> Connection -> TVar [Text] -> Text -> MsgEncodingI e => ChatMessage e -> CM ()
            processEvent ct' conn' tags eInfo chatMsg@ChatMessage {chatMsgEvent} = do
              let tag = toCMEventTag chatMsgEvent
              atomically $ modifyTVar' tags (tshow tag :)
              logInfo $ "contact msg=" <> tshow tag <> " " <> eInfo
              let body = chatMsgToBody chatMsg
              (conn'', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveDirectRcvMSG conn' msgMeta body chatMsg
              let ct'' = ct' {activeConn = Just conn''} :: Contact
              case event of
                XMsgNew mc -> newContentMessage ct'' mc msg msgMeta
                XMsgFileDescr sharedMsgId fileDescr -> messageFileDescription ct'' sharedMsgId fileDescr
                XMsgUpdate sharedMsgId mContent _ ttl live _msgScope -> messageUpdate ct'' sharedMsgId mContent msg msgMeta ttl live
                XMsgDel sharedMsgId _ _ -> messageDelete ct'' sharedMsgId msg msgMeta
                XMsgReact sharedMsgId _ _ reaction add -> directMsgReaction ct'' sharedMsgId reaction add msg msgMeta
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
              ct' <- processContactProfileUpdate ct profile False `catchAllErrors` const (pure ct)
              -- [incognito] send incognito profile
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore $ \db -> getProfileById db userId profileId
              let p = userProfileDirect user (fromLocalProfile <$> incognitoProfile) (Just ct') True
              allowAgentConnectionAsync user conn'' confId $ XInfo p
              void $ withStore' $ \db -> resetMemberContactFields db ct'
            XGrpLinkInv glInv -> do
              -- XGrpLinkInv here means we are connecting via business contact card, so we replace contact with group
              (gInfo, host) <- withStore $ \db -> do
                liftIO $ deleteContactCardKeepConn db connId ct
                createGroupInvitedViaLink db vr user conn'' glInv
              void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing (Just epochStart)
              -- [incognito] send saved profile
              incognitoProfile <- forM customUserProfileId $ \pId -> withStore (\db -> getProfileById db userId pId)
              let profileToSend = userProfileInGroup user gInfo (fromLocalProfile <$> incognitoProfile)
              allowAgentConnectionAsync user conn'' confId $ XInfo profileToSend
              toView $ CEvtBusinessLinkConnecting user gInfo host ct
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
            XInfo profile -> do
              let prepared = isJust (preparedContact ct) || isJust (contactRequestId' ct)
              void $ processContactProfileUpdate ct profile prepared
            XOk -> pure ()
            _ -> messageError "INFO for existing contact must have x.grp.mem.info, x.info or x.ok"
        CON pqEnc -> do
          when (pqEnc == PQEncOn) $ withStore' $ \db -> updateConnPQEnabledCON db connId pqEnc
          let conn' = conn {pqSndEnabled = Just pqEnc, pqRcvEnabled = Just pqEnc} :: Connection
              ct' = ct {activeConn = Just conn'} :: Contact
          -- [incognito] print incognito profile used for this contact
          incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
          toView $ CEvtContactConnected user ct' (fmap fromLocalProfile incognitoProfile)
          let createE2EItem = createInternalChatItem user (CDDirectRcv ct') (CIRcvDirectE2EEInfo $ E2EInfo $ Just pqEnc) Nothing
          -- TODO [short links] get contact request by contactRequestId, check encryption (UserContactRequest.pqSupport)?
          when (directOrUsed ct') $ case (preparedContact ct', contactRequestId' ct') of
            (Nothing, Nothing) -> do
              unlessM (withStore' $ \db -> checkContactHasItems db user ct') $
                createInternalChatItem user (CDDirectSnd ct') CIChatBanner (Just epochStart)
              createE2EItem
              createFeatureEnabledItems user ct'
            (Just PreparedContact {connLinkToConnect = ACCL _ (CCLink cReq _)}, _) ->
              unless (Just pqEnc == connRequestPQEncryption cReq) createE2EItem
            (_, Just connReqId) ->
              withStore' (\db -> getContactRequest' db user connReqId) >>= \case
                Just UserContactRequest {pqSupport} | CR.pqSupportToEnc pqSupport == pqEnc -> pure ()
                _ -> createE2EItem
          when (contactConnInitiated conn') $ do
            probeMatchingMembers ct' (contactConnIncognito ct')
            withStore' $ \db -> resetContactConnInitiated db user conn'
          forM_ viaUserContactLink $ \userContactLinkId -> do
            (ucl, gli_) <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
            -- let UserContactLink {addressSettings = AddressSettings {autoReply}} = ucl
            when (connChatVersion < batchSend2Version) $ forM_ (autoReply $ addressSettings ucl) $ \mc -> sendAutoReply ct' mc Nothing -- old versions only
            -- TODO REMOVE LEGACY vvv
            forM_ gli_ $ \GroupLinkInfo {groupId, memberRole = gLinkMemRole} -> do
              groupInfo <- withStore $ \db -> getGroupInfo db vr user groupId
              subMode <- chatReadVar subscriptionMode
              groupConnIds <- createAgentConnectionAsync user CFCreateConnGrpInv True SCMInvitation subMode
              gVar <- asks random
              withStore $ \db -> createNewContactMemberAsync db gVar user groupInfo ct' gLinkMemRole groupConnIds connChatVersion peerChatVRange subMode
        -- TODO REMOVE LEGACY ^^^
        SENT msgId proxy -> do
          void $ continueSending connEntity conn
          sentMsgDeliveryEvent conn msgId
          checkSndInlineFTComplete conn msgId
          cis <- withStore $ \db -> do
            cis <- updateDirectItemsStatus' db ct conn msgId (CISSndSent SSPComplete)
            liftIO $ forM cis $ \ci -> setDirectSndChatItemViaProxy db user ct ci (isJust proxy)
          let acis = map ctItem cis
          unless (null acis) $ toView $ CEvtChatItemsStatusesUpdated user acis
          where
            ctItem = AChatItem SCTDirect SMDSnd (DirectChat ct)
        SWITCH qd phase cStats -> do
          toView $ CEvtContactSwitch user ct (SwitchProgress qd phase cStats)
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
                Just ci -> toView $ CEvtChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
                _ -> do
                  toView $ CEvtContactRatchetSync user ct (RatchetSyncProgress rss cStats)
                  createInternalChatItem user (CDDirectRcv ct) (CIRcvDecryptionError mde n) Nothing
            ratchetSyncEventItem ct' = do
              toView $ CEvtContactRatchetSync user ct' (RatchetSyncProgress rss cStats)
              createInternalChatItem user (CDDirectRcv ct') (CIRcvConnEvent $ RCERatchetSync rss) Nothing
        OK ->
          -- [async agent commands] continuation on receiving OK
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO [certs rcv]
        JOINED sqSecured _serviceId ->
          -- [async agent commands] continuation on receiving JOINED
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData ->
            when (directOrUsed ct && sqSecured) $ do
              toView $ CEvtContactSndReady user ct
              when (connChatVersion >= batchSend2Version) $ forM_ viaUserContactLink $ \userContactLinkId -> do
                (ucl, _) <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
                forM_ (autoReply $ addressSettings ucl) $ \mc -> do
                  connReq_ <- pure (contactRequestId' ct) $>>= \connReqId -> withStore' (\db -> getContactRequest' db user connReqId)
                  sendAutoReply ct mc connReq_
        QCONT ->
          void $ continueSending connEntity conn
        MWARN msgId err -> do
          updateDirectItemStatus ct conn msgId (CISSndWarning $ agentSndError err)
          processConnMWARN connEntity conn err
        MERR msgId err -> do
          updateDirectItemStatus ct conn msgId (CISSndError $ agentSndError err)
          eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
          processConnMERR connEntity conn err
        MERRS msgIds err -> do
          -- error cannot be AUTH error here
          updateDirectItemsStatusMsgs ct conn (L.toList msgIds) (CISSndError $ agentSndError err)
          eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        ERR err -> do
          eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()
      where
        sendAutoReply ct mc = \case
          Just UserContactRequest {welcomeSharedMsgId = Just smId} ->
            void $ sendDirectContactMessage user ct $ XMsgUpdate smId mc M.empty Nothing Nothing Nothing
          _ -> do
            (msg, _) <- sendDirectContactMessage user ct $ XMsgNew $ MCSimple $ extMsgContent mc Nothing
            ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]

    processGroupMessage :: AEvent e -> ConnectionEntity -> Connection -> GroupInfo -> GroupMember -> CM ()
    processGroupMessage agentMsg connEntity conn@Connection {connId, connChatVersion, customUserProfileId, connectionCode} gInfo@GroupInfo {groupId, groupProfile, membership, chatSettings} m = case agentMsg of
      -- TODO [certs rcv]
      INV (ACR _ cReq) _serviceId ->
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cReq of
            groupConnReq@(CRInvitationUri _ _) -> case cmdFunction of
              -- [async agent commands] XGrpMemIntro continuation on receiving INV
              CFCreateConnGrpMemInv
                | maxVersion (peerChatVRange conn) >= groupDirectInvVersion -> sendWithoutDirectCReq
                | otherwise -> messageError "processGroupMessage INV: member chat version range incompatible"
                where
                  sendWithoutDirectCReq = do
                    let GroupMember {groupMemberId, memberId} = m
                    hostConnId <- withStore $ \db -> do
                      liftIO $ setConnConnReqInv db user connId cReq
                      getHostConnId db user groupId
                    sendXGrpMemInv hostConnId Nothing XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq}
              -- TODO REMOVE LEGACY vvv
              -- [async agent commands] group link auto-accept continuation on receiving INV
              CFCreateConnGrpInv -> do
                (ct, groupLinkId) <- withStore $ \db -> do
                  ct <- getContactViaMember db vr user m
                  liftIO $ setNewContactMemberConnRequest db user m cReq
                  liftIO $ (ct,) <$> getGroupLinkId db user gInfo
                sendGrpInvitation ct m groupLinkId
                toView $ CEvtSentGroupInvitation user gInfo ct m
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
                    createInternalChatItem user (CDGroupRcv gInfo Nothing m) (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
              -- TODO REMOVE LEGACY ^^^
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
              XGrpRelayAcpt relayLink
                | memberRole' membership == GROwner && isRelay m -> do
                    withStore $ \db -> do
                      relay <- getGroupRelayByGMId db (groupMemberId' m)
                      liftIO $ updateGroupMemberStatus db userId m GSMemAccepted
                      void $ liftIO $ setRelayLinkAccepted db relay relayLink
                    allowAgentConnectionAsync user conn' confId XOk
                | otherwise -> messageError "x.grp.relay.acpt: only owner can add relay"
              _ -> messageError "CONF from invited member must have x.grp.acpt"
          GCHostMember ->
            case chatMsgEvent of
              XGrpLinkInv glInv -> do
                -- XGrpLinkInv here means we are connecting via prepared group, and we have to update user and host member records
                (gInfo', m') <- withStore $ \db -> updatePreparedUserAndHostMembersInvited db vr user gInfo m glInv
                -- [incognito] send saved profile
                incognitoProfile <- forM customUserProfileId $ \pId -> withStore (\db -> getProfileById db userId pId)
                let profileToSend = userProfileInGroup user gInfo (fromLocalProfile <$> incognitoProfile)
                allowAgentConnectionAsync user conn' confId $ XInfo profileToSend
                toView $ CEvtGroupLinkConnecting user gInfo' m'
              XGrpLinkReject glRjct@GroupLinkRejection {rejectionReason} -> do
                (gInfo', m') <- withStore $ \db -> updatePreparedUserAndHostMembersRejected db vr user gInfo m glRjct
                toView $ CEvtGroupLinkConnecting user gInfo' m'
                toViewTE $ TEGroupLinkRejected user gInfo' rejectionReason
              _ -> messageError "CONF from host member in prepared group must have x.grp.link.inv or x.grp.link.reject"
          _ ->
            case chatMsgEvent of
              XGrpMemInfo memId _memProfile
                | sameMemberId memId m -> do
                    let GroupMember {memberId = membershipMemId} = membership
                        allowSimplexLinks = groupFeatureUserAllowed SGFSimplexLinks gInfo
                        membershipProfile = redactedMemberProfile allowSimplexLinks $ fromLocalProfile $ memberProfile membership
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
          -- sent when connecting via group link
          XInfo _ ->
            -- TODO Keep rejected member to allow them to appeal against rejection.
            when (memberStatus m == GSMemRejected) $ do
              deleteMemberConnection' m True
              withStore' $ \db -> deleteGroupMember db user m
          XOk -> pure ()
          _ -> messageError "INFO from member must have x.grp.mem.info, x.info or x.ok"
        pure ()
      CON _pqEnc -> unless (memberStatus m == GSMemRejected || memberStatus membership == GSMemRejected) $ do
        -- TODO [knocking] send pending messages after accepting?
        -- possible improvement: check for each pending message, requires keeping track of connection state
        unless (connDisabled conn) $ sendPendingGroupMessages user m conn
        withAgent $ \a -> toggleConnectionNtfs a (aConnId conn) $ chatHasNtfs chatSettings
        case memberCategory m of
          GCHostMember -> do
            (m', gInfo') <- withStore' $ \db -> do
              updateGroupMemberStatus db userId m GSMemConnected
              gInfo' <-
                if not (memberPending membership)
                  then do
                    updateGroupMemberStatus db userId membership GSMemConnected
                    pure gInfo {membership = membership {memberStatus = GSMemConnected}}
                  else pure gInfo
              pure (m {memberStatus = GSMemConnected}, gInfo')
            toView $ CEvtUserJoinedGroup user gInfo' m'
            (gInfo'', m'', scopeInfo) <- mkGroupChatScope gInfo' m'
            -- Create e2ee, feature and group description chat items only on first connected relay
            ifM
              firstConnectedHost
              ( do
                  let cd = CDGroupRcv gInfo'' scopeInfo m''
                  createInternalChatItem user cd (CIRcvGroupE2EEInfo E2EInfo {pqEnabled = Just PQEncOff}) Nothing
                  let prepared = preparedGroup gInfo''
                  unless (isJust prepared) $ createGroupFeatureItems user cd CIRcvGroupFeature gInfo''
                  memberConnectedChatItem gInfo'' scopeInfo m''
                  let welcomeMsgId_ = (\PreparedGroup {welcomeSharedMsgId = mId} -> mId) <$> prepared
                  unless (memberPending membership || isJust welcomeMsgId_) $ maybeCreateGroupDescrLocal gInfo'' m''
              )
              (memberConnectedChatItem gInfo'' scopeInfo m'')
            where
              firstConnectedHost
                | useRelays' gInfo = do
                    relayMems <- withStore' $ \db -> getGroupRelayMembers db vr user gInfo
                    let numConnected = length $ filter (\GroupMember {memberStatus = ms} -> ms == GSMemConnected) relayMems
                    pure $ numConnected == 1
                | otherwise = pure True
          GCInviteeMember
            | isRelay m -> do
                withStore' $ \db -> updateGroupMemberStatus db userId m GSMemConnected
                gLink <- withStore $ \db -> getGroupLink db user gInfo
                setGroupLinkDataAsync user gInfo gLink
            | otherwise -> do
                (gInfo', mStatus) <-
                  if not (memberPending m)
                    then do
                      mStatus <- withStore' $ \db -> updateGroupMemberStatus db userId m GSMemConnected $> GSMemConnected
                      pure (gInfo, mStatus)
                    else do
                      gInfo' <- withStore' $ \db -> increaseGroupMembersRequireAttention db user gInfo
                      pure (gInfo', memberStatus m)
                (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo' m
                memberConnectedChatItem gInfo'' scopeInfo m'
                case scopeInfo of
                  Just (GCSIMemberSupport _) -> do
                    createInternalChatItem user (CDGroupRcv gInfo'' scopeInfo m') (CIRcvGroupEvent RGENewMemberPendingReview) Nothing
                  _ -> pure ()
                toView $ CEvtJoinedGroupMember user gInfo'' m' {memberStatus = mStatus}
                let Connection {viaUserContactLink} = conn
                when (isJust viaUserContactLink && isNothing (memberContactId m')) $ sendXGrpLinkMem gInfo''
                when (connChatVersion < batchSend2Version) $ getAutoReplyMsg >>= mapM_ (\mc -> sendGroupAutoReply mc Nothing)
                if useRelays' gInfo''
                  then do
                    introduceModerators vr user gInfo'' m'
                    when (groupFeatureAllowed SGFHistory gInfo'') $ sendHistory user gInfo'' m'
                  else case mStatus of
                    GSMemPendingApproval -> pure ()
                    GSMemPendingReview -> introduceToModerators vr user gInfo'' m'
                    _ -> do
                      introduceToAll vr user gInfo'' m'
                      let memberIsCustomer = case businessChat gInfo'' of
                            Just BusinessChatInfo {chatType = BCCustomer, customerId} -> memberId' m' == customerId
                            _ -> False
                      when (groupFeatureAllowed SGFHistory gInfo'' && not memberIsCustomer) $ sendHistory user gInfo'' m'
            where
              sendXGrpLinkMem gInfo'' = do
                let incognitoProfile = ExistingIncognito <$> incognitoMembershipProfile gInfo''
                    profileToSend = userProfileInGroup user gInfo (fromIncognitoProfile <$> incognitoProfile)
                void $ sendDirectMemberMessage conn (XGrpLinkMem profileToSend) groupId
          _ -> do
            unless (memberPending m) $ withStore' $ \db -> updateGroupMemberStatus db userId m GSMemConnected
            notifyMemberConnected gInfo m Nothing
            let memCategory = memberCategory m
                connectedIncognito = memberIncognito membership
            when (memCategory == GCPreMember) $
              probeMatchingMemberContact m connectedIncognito
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
          -- possible improvement is to choose scope based on event (some events specify scope)
          (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
          checkIntegrityCreateItem (CDGroupRcv gInfo' scopeInfo m') msgMeta `catchAllErrors` \_ -> pure ()
          newDeliveryTasks <- reverse <$> foldM (processAChatMsg gInfo' m' tags eInfo) [] aChatMsgs
          shouldDelConns <-
            if isUserGrpFwdRelay gInfo' && not (blockedByAdmin m)
              then createDeliveryTasks gInfo' m' newDeliveryTasks
              else pure False
          withRcpt <- checkSendRcpt $ rights aChatMsgs
          pure (withRcpt, shouldDelConns)
        where
          aChatMsgs = parseChatMessages msgBody
          brokerTs = metaBrokerTs msgMeta
          processAChatMsg ::
            GroupInfo ->
            GroupMember ->
            TVar [Text] ->
            Text ->
            [NewMessageDeliveryTask] ->
            Either String AChatMessage ->
            CM [NewMessageDeliveryTask]
          processAChatMsg gInfo' m' tags eInfo newDeliveryTasks = \case
            Right (ACMsg SJson chatMsg) -> do
              newTask_ <- processEvent gInfo' m' tags eInfo chatMsg `catchAllErrors` \e -> eToView e $> Nothing
              pure $ maybe newDeliveryTasks (: newDeliveryTasks) newTask_
            Right (ACMsg SBinary chatMsg) -> do
              void (processEvent gInfo' m' tags eInfo chatMsg) `catchAllErrors` \e -> eToView e
              pure newDeliveryTasks
            Left e -> do
              atomically $ modifyTVar' tags ("error" :)
              logInfo $ "group msg=error " <> eInfo <> " " <> tshow e
              eToView (ChatError . CEException $ "error parsing chat message: " <> e)
              pure newDeliveryTasks
          processEvent :: forall e. MsgEncodingI e => GroupInfo -> GroupMember -> TVar [Text] -> Text -> ChatMessage e -> CM (Maybe NewMessageDeliveryTask)
          processEvent gInfo' m' tags eInfo chatMsg@ChatMessage {chatMsgEvent} = do
            let tag = toCMEventTag chatMsgEvent
            atomically $ modifyTVar' tags (tshow tag :)
            logInfo $ "group msg=" <> tshow tag <> " " <> eInfo
            let body = chatMsgToBody chatMsg
            (m'', conn', msg@RcvMessage {msgId, chatMsgEvent = ACME _ event}) <- saveGroupRcvMsg user groupId m' conn msgMeta body chatMsg
            let isChannelOwner = useRelays' gInfo' && memberRole' m'' == GROwner
                showGroupAsSender' = case event of
                  XMsgNew mc -> fromMaybe False (asGroup (mcExtMsgContent mc))
                  XMsgUpdate {} -> isChannelOwner
                  XMsgDel {} -> isChannelOwner
                  XMsgReact {} -> isChannelOwner
                  XMsgFileDescr {} -> isChannelOwner
                  XFileCancel {} -> isChannelOwner
                  _ -> False
            -- ! see isForwardedGroupMsg: processing functions should return DeliveryJobScope for same events
            deliveryJobScope_ <- case event of
              XMsgNew mc ->
                canSendAsGroup $
                  if fromMaybe False asGroup
                    then newChannelContentMessage_ gInfo' mc msg brokerTs False
                    else memberCanSend m'' scope $ newGroupContentMessage gInfo' m'' mc msg brokerTs False
                where
                  ExtMsgContent {scope, asGroup} = mcExtMsgContent mc
                  canSendAsGroup a
                    | asGroup == Just True && memberRole' m'' < GROwner =
                        messageError "member is not allowed to send as group" $> Nothing
                    | otherwise = a
              -- file description is always allowed, to allow sending files to support scope
              XMsgFileDescr sharedMsgId fileDescr
                | isChannelOwner -> groupMessageFileDescription gInfo' Nothing sharedMsgId fileDescr
                | otherwise -> groupMessageFileDescription gInfo' (Just $ memberId' m'') sharedMsgId fileDescr
              XMsgUpdate sharedMsgId mContent mentions ttl live msgScope
                | isChannelOwner -> channelMessageUpdate_ gInfo' sharedMsgId mContent mentions msg brokerTs ttl live False
                | otherwise -> memberCanSend m'' msgScope $ groupMessageUpdate gInfo' m'' sharedMsgId mContent mentions msgScope msg brokerTs ttl live
              XMsgDel sharedMsgId memberId_ scope_
                | isChannelOwner -> channelMessageDelete gInfo' sharedMsgId msg brokerTs
                | otherwise -> groupMessageDelete gInfo' m'' sharedMsgId memberId_ scope_ msg brokerTs
              XMsgReact sharedMsgId memberId scope_ reaction add -> groupMsgReaction gInfo' m'' sharedMsgId memberId scope_ reaction add msg brokerTs
              -- TODO discontinue XFile
              XFile fInv -> Nothing <$ processGroupFileInvitation' gInfo' m'' fInv msg brokerTs
              XFileCancel sharedMsgId
                | isChannelOwner -> xFileCancelGroup gInfo' Nothing sharedMsgId
                | otherwise -> xFileCancelGroup gInfo' (Just $ memberId' m'') sharedMsgId
              XFileAcptInv sharedMsgId fileConnReq_ fName -> Nothing <$ xFileAcptInvGroup gInfo' m'' sharedMsgId fileConnReq_ fName
              XInfo p -> xInfoMember gInfo' m'' p brokerTs
              XGrpLinkMem p -> Nothing <$ xGrpLinkMem gInfo' m'' conn' p
              XGrpLinkAcpt acceptance role memberId -> Nothing <$ xGrpLinkAcpt gInfo' m'' acceptance role memberId msg brokerTs
              XGrpMemNew memInfo msgScope -> xGrpMemNew gInfo' m'' memInfo msgScope msg brokerTs
              XGrpMemIntro memInfo memRestrictions_ -> Nothing <$ xGrpMemIntro gInfo' m'' memInfo memRestrictions_
              XGrpMemInv memId introInv -> Nothing <$ xGrpMemInv gInfo' m'' memId introInv
              XGrpMemFwd memInfo introInv -> Nothing <$ xGrpMemFwd gInfo' m'' memInfo introInv
              XGrpMemRole memId memRole -> xGrpMemRole gInfo' m'' memId memRole msg brokerTs
              XGrpMemRestrict memId memRestrictions -> xGrpMemRestrict gInfo' m'' memId memRestrictions msg brokerTs
              XGrpMemCon memId -> Nothing <$ xGrpMemCon gInfo' m'' memId
              XGrpMemDel memId withMessages -> case encoding @e of
                SJson -> xGrpMemDel gInfo' m'' memId withMessages chatMsg msg brokerTs False
                SBinary -> pure Nothing -- impossible
              XGrpLeave -> xGrpLeave gInfo' m'' msg brokerTs
              XGrpDel -> Just (DJSGroup {jobSpec = DJRelayRemoved}) <$ xGrpDel gInfo' m'' msg brokerTs
              XGrpInfo p' -> xGrpInfo gInfo' m'' p' msg brokerTs
              XGrpPrefs ps' -> xGrpPrefs gInfo' m'' ps'
              -- TODO [knocking] why don't we forward these messages?
              XGrpDirectInv connReq mContent_ msgScope -> memberCanSend m'' msgScope $ Nothing <$ xGrpDirectInv gInfo' m'' conn' connReq mContent_ msg brokerTs
              XGrpMsgForward memberId memberName msg' msgTs -> Nothing <$ xGrpMsgForward gInfo' m'' memberId memberName msg' msgTs brokerTs
              XInfoProbe probe -> Nothing <$ xInfoProbe (COMGroupMember m'') probe
              XInfoProbeCheck probeHash -> Nothing <$ xInfoProbeCheck (COMGroupMember m'') probeHash
              XInfoProbeOk probe -> Nothing <$ xInfoProbeOk (COMGroupMember m'') probe
              BFileChunk sharedMsgId chunk -> Nothing <$ bFileChunkGroup gInfo' sharedMsgId chunk msgMeta
              _ -> Nothing <$ messageError ("unsupported message: " <> tshow event)
            forM deliveryJobScope_ $ \jobScope ->
              pure $ NewMessageDeliveryTask {messageId = msgId, jobScope, showGroupAsSender = showGroupAsSender'}
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
          createDeliveryTasks :: GroupInfo -> GroupMember -> [NewMessageDeliveryTask] -> CM ShouldDeleteGroupConns
          createDeliveryTasks gInfo'@GroupInfo {groupId = gId} m' newDeliveryTasks = do
            let relayRemovedTask_ = find (\NewMessageDeliveryTask {jobScope} -> isRelayRemoved jobScope) newDeliveryTasks
            createdDeliveryTasks <- case relayRemovedTask_ of
              Nothing -> do
                withStore' $ \db ->
                  forM_ newDeliveryTasks $ \newTask ->
                    createMsgDeliveryTask db gInfo' m' newTask
                pure newDeliveryTasks
              Just relayRemovedTask -> do
                -- if relay is removed, delete all other tasks and jobs
                withStore' $ \db -> do
                  deleteGroupDeliveryTasks db gInfo'
                  deleteGroupDeliveryJobs db gInfo'
                  createMsgDeliveryTask db gInfo' m' relayRemovedTask
                pure [relayRemovedTask]
            lift $ forM_ (uniqueWorkerScopes createdDeliveryTasks) $ \workerScope ->
              getDeliveryTaskWorker True (gId, workerScope)
            pure $ isJust relayRemovedTask_
            where
              uniqueWorkerScopes :: [NewMessageDeliveryTask] -> [DeliveryWorkerScope]
              uniqueWorkerScopes createdDeliveryTasks =
                let workerScopes = map (\NewMessageDeliveryTask {jobScope} -> toWorkerScope jobScope) createdDeliveryTasks
                 in foldr' addWorkerScope [] workerScopes
                where
                  addWorkerScope workerScope acc
                    | workerScope `elem` acc = acc
                    | otherwise = workerScope : acc
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
        toView $ CEvtGroupMemberSwitch user gInfo m (SwitchProgress qd phase cStats)
        (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
        when (phase == SPStarted || phase == SPCompleted) $ case qd of
          QDRcv -> createInternalChatItem user (CDGroupSnd gInfo' scopeInfo) (CISndConnEvent . SCESwitchQueue phase . Just $ groupMemberRef m') Nothing
          QDSnd -> createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m') (CIRcvConnEvent $ RCESwitchQueue phase) Nothing
      RSYNC rss cryptoErr_ cStats -> do
        (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
        case (rss, connectionCode, cryptoErr_) of
          (RSRequired, _, Just cryptoErr) -> processErr gInfo' scopeInfo m' cryptoErr
          (RSAllowed, _, Just cryptoErr) -> processErr gInfo' scopeInfo m' cryptoErr
          (RSAgreed, Just _, _) -> do
            withStore' $ \db -> setConnectionVerified db user connId Nothing
            let m'' = m' {activeConn = Just (conn {connectionCode = Nothing} :: Connection)} :: GroupMember
            ratchetSyncEventItem gInfo' scopeInfo m''
            toViewTE $ TEGroupMemberVerificationReset user gInfo' m''
            createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m'') (CIRcvConnEvent RCEVerificationCodeReset) Nothing
          _ -> ratchetSyncEventItem gInfo' scopeInfo m'
        where
          processErr gInfo' scopeInfo m' cryptoErr = do
            let e@(mde, n) = agentMsgDecryptError cryptoErr
            ci_ <- withStore $ \db ->
              getGroupMemberChatItemLast db user groupId (groupMemberId' m')
                >>= liftIO
                  . mapM (\(ci, content') -> updateGroupChatItem db user groupId ci content' False False Nothing)
                  . mdeUpdatedCI e
            case ci_ of
              Just ci -> toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo' scopeInfo) ci)
              _ -> do
                toView $ CEvtGroupMemberRatchetSync user gInfo' m' (RatchetSyncProgress rss cStats)
                createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m') (CIRcvDecryptionError mde n) Nothing
          ratchetSyncEventItem gInfo' scopeInfo m' = do
            toView $ CEvtGroupMemberRatchetSync user gInfo' m' (RatchetSyncProgress rss cStats)
            createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m') (CIRcvConnEvent $ RCERatchetSync rss) Nothing
      OK ->
        -- [async agent commands] continuation on receiving OK
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO [certs rcv]
      JOINED sqSecured _serviceId ->
        -- [async agent commands] continuation on receiving JOINED
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData ->
          when (sqSecured && connChatVersion >= batchSend2Version) $ do
            mc_ <- getAutoReplyMsg
            forM_ mc_ $ \mc -> do
              connReq_ <- withStore' $ \db -> getBusinessContactRequest db user groupId
              sendGroupAutoReply mc connReq_
      LDATA FixedLinkData {linkConnReq = cReq} _cData ->
        -- [async agent commands] CFGetConnShortLink continuation - join relay connection with resolved link
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cmdFunction of
            CFGetShortLink -> case cReq of
              CRContactUri crData@ConnReqUriData {crClientData} -> do
                let pqSup = PQSupportOff
                lift (withAgent' $ \a -> connRequestPQSupport a pqSup cReq) >>= \case
                  Nothing -> throwChatError CEInvalidConnReq
                  Just (agentV, _) -> do
                    let chatV = agentToChatVersion agentV
                        groupLinkId = crClientData >>= decodeJSON >>= \(CRDataGroup gli) -> Just gli
                        cReqHash = contactCReqHash $ CRContactUri crData {crScheme = SSSimplex}
                    -- Update connection with data derived from cReq, now available after getConnShortLinkAsync
                    withStore' $ \db -> updateConnLinkData db user conn cReq cReqHash groupLinkId chatV pqSup
                    let GroupMember {memberId = membershipMemId} = membership
                        incognitoProfile = fromLocalProfile <$> incognitoMembershipProfile gInfo
                        profileToSend = userProfileInGroup user gInfo incognitoProfile
                    dm <- encodeConnInfo $ XMember profileToSend membershipMemId
                    subMode <- chatReadVar subscriptionMode
                    void $ joinAgentConnectionAsync user (Just conn) True cReq dm subMode
            _ -> throwChatError $ CECommandError "unexpected cmdFunction"
      QCONT -> do
        continued <- continueSending connEntity conn
        when continued $ sendPendingGroupMessages user m conn
      MWARN msgId err -> do
        withStore' $ \db -> updateGroupItemsErrorStatus db msgId (groupMemberId' m) (GSSWarning $ agentSndError err)
        processConnMWARN connEntity conn err
      MERR msgId err -> do
        withStore' $ \db -> updateGroupItemsErrorStatus db msgId (groupMemberId' m) (GSSError $ agentSndError err)
        -- group errors are silenced to reduce load on UI event log
        -- eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        processConnMERR connEntity conn err
      MERRS msgIds err -> do
        let newStatus = GSSError $ agentSndError err
        -- error cannot be AUTH error here
        withStore' $ \db -> forM_ msgIds $ \msgId ->
          updateGroupItemsErrorStatus db msgId (groupMemberId' m) newStatus `catchAll_` pure ()
        eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
      ERR err -> do
        eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()
      where
        updateGroupItemsErrorStatus :: DB.Connection -> AgentMsgId -> GroupMemberId -> GroupSndStatus -> IO ()
        updateGroupItemsErrorStatus db msgId groupMemberId newStatus = do
          itemIds <- getChatItemIdsByAgentMsgId db connId msgId
          forM_ itemIds $ \itemId -> updateGroupMemSndStatus' db itemId groupMemberId newStatus
        getAutoReplyMsg = do
          let GroupInfo {businessChat} = gInfo
              GroupMember {memberId = joiningMemberId} = m
          case businessChat of
            Just BusinessChatInfo {customerId, chatType = BCCustomer}
              | joiningMemberId == customerId -> useReply <$> withStore (`getUserAddress` user)
              where
                useReply UserContactLink {addressSettings = AddressSettings {autoReply}} = autoReply
            _ -> pure Nothing
        sendGroupAutoReply mc = \case
          Just UserContactRequest {welcomeSharedMsgId = Just smId} ->
            void $ sendGroupMessage' user gInfo [m] $ XMsgUpdate smId mc M.empty Nothing Nothing Nothing
          _ -> do
            msg <- sendGroupMessage' user gInfo [m] $ XMsgNew $ MCSimple $ extMsgContent mc Nothing
            ci <- saveSndChatItem user (CDGroupSnd gInfo Nothing) msg (CISndMsgContent mc)
            withStore' $ \db -> createGroupSndStatus db (chatItemId' ci) (groupMemberId' m) GSSNew
            toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo Nothing) ci]

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

    receiveFileChunk :: RcvFileTransfer -> Maybe Connection -> MsgMeta -> FileChunk -> CM ()
    receiveFileChunk ft@RcvFileTransfer {fileId, chunkSize} conn_ meta@MsgMeta {recipient = (msgId, _), integrity} = \case
      FileChunkCancel ->
        unless (rcvFileCompleteOrCancelled ft) $ do
          cancelRcvFileTransfer user ft
          ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
          toView $ CEvtRcvFileSndCancelled user ci ft
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
                toView $ CEvtRcvFileComplete user ci
                mapM_ (deleteAgentConnectionAsync . aConnId) conn_
          RcvChunkDuplicate -> withAckMessage' "file msg" agentConnId meta $ pure ()
          RcvChunkError -> badRcvFileChunk ft $ "incorrect chunk number " <> show chunkNo

    processContactConnMessage :: AEvent e -> ConnectionEntity -> Connection -> UserContact -> CM ()
    processContactConnMessage agentMsg connEntity conn UserContact {userContactLinkId = uclId, groupId = ucGroupId_} = case agentMsg of
      REQ invId pqSupport _ connInfo -> do
        ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage conn connInfo
        case chatMsgEvent of
          XContact p xContactId_ welcomeMsgId_ requestMsg_ -> profileContactRequest invId chatVRange p xContactId_ welcomeMsgId_ requestMsg_ pqSupport
          XMember p joiningMemberId -> memberJoinRequestViaRelay invId chatVRange p joiningMemberId
          XInfo p -> profileContactRequest invId chatVRange p Nothing Nothing Nothing pqSupport
          XGrpRelayInv groupRelayInv -> xGrpRelayInv invId chatVRange groupRelayInv
          -- TODO show/log error, other events in contact request
          _ -> pure ()
      LINK _link auData ->
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cmdFunction of
            CFSetShortLink ->
              case (ucGroupId_, auData) of
                (Just groupId, UserContactLinkData UserContactData {relays = relayLinks}) -> do
                  (gInfo, gLink, relays) <- withStore $ \db -> do
                    gInfo <- getGroupInfo db vr user groupId
                    gLink <- getGroupLink db user gInfo
                    relays <- liftIO $ getGroupRelays db gInfo
                    relays' <- liftIO $ mapM (updateRelay db) relays
                    liftIO $ setGroupInProgressDone db gInfo
                    pure (gInfo, gLink, relays')
                  -- TODO [relays] owner: "relays updated" chat item?
                  toView $ CEvtGroupLinkRelaysUpdated user gInfo gLink relays
                  where
                    -- TODO [relays] owner: on relay deletion (link absent from relayLinks)
                    -- TODO          move status RSActive to new "Removed" status / remove relay record
                    updateRelay :: DB.Connection -> GroupRelay -> IO GroupRelay
                    updateRelay db relay@GroupRelay {relayLink, relayStatus} =
                      case relayLink of
                        Just rLink
                          | rLink `elem` relayLinks && relayStatus == RSAccepted ->
                              updateRelayStatus db relay RSActive
                        _ -> pure relay
                _ -> throwChatError $ CECommandError "LINK event expected for a group link only"
            _ -> throwChatError $ CECommandError "unexpected cmdFunction"
      MERR _ err -> do
        eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        processConnMERR connEntity conn err
      ERR err -> do
        eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()
      where
        profileContactRequest :: InvitationId -> VersionRangeChat -> Profile -> Maybe XContactId -> Maybe SharedMsgId -> Maybe (SharedMsgId, MsgContent) -> PQSupport -> CM ()
        profileContactRequest invId chatVRange p@Profile {displayName} xContactId_ welcomeMsgId_ requestMsg_ reqPQSup = do
          (ucl, gLinkInfo_) <- withStore $ \db -> getUserContactLinkById db userId uclId
          let v = maxVersion chatVRange
          case gLinkInfo_ of
            -- ##### Contact requests (regular and business contacts) #####
            Nothing -> do
              let UserContactLink {connLinkContact = CCLink connReq _, shortLinkDataSet, addressSettings} = ucl
                  AddressSettings {autoAccept} = addressSettings
                  isSimplexTeam = sameConnReqContact connReq adminContactReq
              gVar <- asks random
              withStore (\db -> createOrUpdateContactRequest db gVar vr user uclId ucl isSimplexTeam invId chatVRange p xContactId_ welcomeMsgId_ requestMsg_ reqPQSup) >>= \case
                RSAcceptedRequest _ucr re -> case re of
                  REContact ct ->
                    -- TODO [short links] update request msg
                    toView $ CEvtContactRequestAlreadyAccepted user ct
                  REBusinessChat gInfo _clientMember ->
                    -- TODO [short links] update request msg
                    toView $ CEvtBusinessRequestAlreadyAccepted user gInfo
                RSCurrentRequest prevUcr_ ucr@UserContactRequest {welcomeSharedMsgId} re_ -> case re_ of
                  Nothing -> toView $ CEvtReceivedContactRequest user ucr Nothing
                  Just (REContact ct) -> do
                    let cd = CDDirectRcv ct
                    aci_ <- case prevUcr_ of
                      Just UserContactRequest {requestSharedMsgId = prevSharedMsgId_} ->
                        -- TODO [short links] this branch does not update feature items and e2e items, as they are highly unlikely to change
                        -- they will be updated after connection is accepted.
                        upsertDirectRequestItem cd (requestMsg_, prevSharedMsgId_)
                      Nothing -> do
                        void $ createChatItem user (CDDirectSnd ct) False CIChatBanner Nothing (Just epochStart)
                        let e2eContent = CIRcvDirectE2EEInfo $ E2EInfo $ Just $ CR.pqSupportToEnc $ reqPQSup
                        void $ createChatItem user cd False e2eContent Nothing Nothing
                        void $ createFeatureEnabledItems_ user ct
                        forM_ (autoReply addressSettings) $ \mc -> forM_ welcomeSharedMsgId $ \sharedMsgId ->
                          createChatItem user (CDDirectSnd ct) False (CISndMsgContent mc) (Just sharedMsgId) Nothing
                        mapM (createRequestItem cd) requestMsg_
                    case autoAccept of
                      Nothing -> do
                        let cInfo = DirectChat ct
                            chat = AChat SCTDirect $ case aci_ of
                              Just (AChatItem SCTDirect dir _ ci) -> Chat cInfo [CChatItem dir ci] emptyChatStats {unreadCount = 1, minUnreadItemId = chatItemId' ci}
                              _ -> Chat cInfo [] emptyChatStats
                        toView $ CEvtReceivedContactRequest user ucr $ Just chat
                      Just AutoAccept {acceptIncognito} -> do
                        incognitoProfile <-
                          if not shortLinkDataSet && acceptIncognito
                            then Just . NewIncognito <$> liftIO generateRandomProfile
                            else pure Nothing
                        ct' <- acceptContactRequestAsync user uclId ct ucr incognitoProfile
                        toView $ CEvtAcceptingContactRequest user ct'
                  Just (REBusinessChat gInfo clientMember) -> do
                    (_gInfo', _clientMember') <- acceptBusinessJoinRequestAsync user uclId gInfo clientMember ucr
                    let cd = CDGroupRcv gInfo Nothing clientMember
                    void $ case prevUcr_ of
                      Just UserContactRequest {requestSharedMsgId = prevSharedMsgId_} ->
                        -- TODO [short links] this branch does not update feature items and e2e items, as they are highly unlikely to change
                        -- they will be updated after connection is accepted.
                        upsertBusinessRequestItem cd (requestMsg_, prevSharedMsgId_)
                      Nothing -> do
                        void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing (Just epochStart)
                        -- TODO [short links] possibly, we can just keep them created where they are created on the business side due to auto-accept
                        -- let e2eContent = CIRcvGroupE2EEInfo $ E2EInfo $ Just False -- no PQ encryption in groups
                        -- void $ createChatItem user cd False e2eContent Nothing Nothing
                        -- void $ createFeatureEnabledItems_ user ct
                        forM_ (autoReply addressSettings) $ \arMC -> forM_ welcomeSharedMsgId $ \sharedMsgId ->
                          createChatItem user (CDGroupSnd gInfo Nothing) False (CISndMsgContent arMC) (Just sharedMsgId) Nothing
                        mapM (createRequestItem cd) requestMsg_
                    toView $ CEvtAcceptingBusinessRequest user gInfo
              where
                upsertDirectRequestItem :: ChatDirection 'CTDirect 'MDRcv -> (Maybe (SharedMsgId, MsgContent), Maybe SharedMsgId) -> CM (Maybe AChatItem)
                upsertDirectRequestItem cd@(CDDirectRcv ct@Contact {contactId}) = upsertRequestItem cd updateRequestItem markRequestItemDeleted
                  where
                    updateRequestItem (sharedMsgId, mc) =
                      withStore (\db -> getDirectChatItemBySharedMsgId db user contactId sharedMsgId) >>= \case
                        CChatItem SMDRcv ci@ChatItem {content = CIRcvMsgContent oldMC}
                          | mc /= oldMC -> do
                              currentTs <- liftIO getCurrentTime
                              aci <- withStore' $ \db -> do
                                addInitialAndNewCIVersions db (chatItemId' ci) (chatItemTs' ci, oldMC) (currentTs, mc)
                                aChatItem <$> updateDirectChatItem' db user contactId ci (CIRcvMsgContent mc) True False Nothing Nothing
                              toView $ CEvtChatItemUpdated user aci
                              pure $ Just aci
                          | otherwise -> pure $ Just $ aChatItem ci
                        _ -> pure Nothing
                      where
                        aChatItem = AChatItem SCTDirect SMDRcv (DirectChat ct)
                    markRequestItemDeleted sharedMsgId =
                      withStore' (\db -> runExceptT $ getDirectChatItemBySharedMsgId db user contactId sharedMsgId) >>= \case
                        Right (cci@(CChatItem SMDRcv _)) -> do
                          currentTs <- liftIO getCurrentTime
                          deletions <-
                            if featureAllowed SCFFullDelete forContact ct
                              then deleteDirectCIs user ct [cci]
                              else markDirectCIsDeleted user ct [cci] currentTs
                          toView $ CEvtChatItemsDeleted user deletions False False
                        _ -> pure ()
                upsertBusinessRequestItem :: ChatDirection 'CTGroup 'MDRcv -> (Maybe (SharedMsgId, MsgContent), Maybe SharedMsgId) -> CM (Maybe AChatItem)
                upsertBusinessRequestItem cd@(CDGroupRcv gInfo@GroupInfo {groupId} _ clientMember) = upsertRequestItem cd updateRequestItem markRequestItemDeleted
                  where
                    updateRequestItem (sharedMsgId, mc) =
                      withStore (\db -> getGroupChatItemBySharedMsgId db user gInfo (groupMemberId' clientMember) sharedMsgId) >>= \case
                        CChatItem SMDRcv ci@ChatItem {chatDir = CIGroupRcv m', content = CIRcvMsgContent oldMC}
                          | sameMemberId (memberId' clientMember) m' ->
                              if mc /= oldMC
                                then do
                                  currentTs <- liftIO getCurrentTime
                                  aci <- withStore' $ \db -> do
                                    addInitialAndNewCIVersions db (chatItemId' ci) (chatItemTs' ci, oldMC) (currentTs, mc)
                                    aChatItem <$> updateGroupChatItem db user groupId ci (CIRcvMsgContent mc) True False Nothing
                                  toView $ CEvtChatItemUpdated user aci
                                  pure $ Just aci
                                else pure $ Just $ aChatItem ci
                        _ -> pure Nothing
                      where
                        aChatItem = AChatItem SCTGroup SMDRcv (GroupChat gInfo Nothing)
                    markRequestItemDeleted sharedMsgId =
                      withStore' (\db -> runExceptT $ getGroupMemberCIBySharedMsgId db user gInfo (memberId' clientMember) sharedMsgId) >>= \case
                        Right cci@(CChatItem SMDRcv ChatItem {chatDir = CIGroupRcv m'})
                          | sameMemberId (memberId' clientMember) m' -> do
                              currentTs <- liftIO getCurrentTime
                              deletions <-
                                if groupFeatureMemberAllowed SGFFullDelete clientMember gInfo
                                  then deleteGroupCIs user gInfo Nothing [cci] Nothing currentTs
                                  else markGroupCIsDeleted user gInfo Nothing [cci] Nothing currentTs
                              toView $ CEvtChatItemsDeleted user deletions False False
                        _ -> pure ()
                upsertBusinessRequestItem (CDChannelRcv _ _) = const $ pure Nothing
                createRequestItem :: ChatTypeI c => ChatDirection c 'MDRcv -> (SharedMsgId, MsgContent) -> CM AChatItem
                createRequestItem cd (sharedMsgId, mc) = do
                  aci <- createChatItem user cd False (CIRcvMsgContent mc) (Just sharedMsgId) Nothing
                  toView $ CEvtNewChatItems user [aci]
                  pure aci
                upsertRequestItem :: ChatTypeI c => ChatDirection c 'MDRcv -> ((SharedMsgId, MsgContent) -> CM (Maybe AChatItem)) -> (SharedMsgId -> CM ()) -> (Maybe (SharedMsgId, MsgContent), Maybe SharedMsgId) -> CM (Maybe AChatItem)
                upsertRequestItem cd update delete = \case
                  (Just msg, Nothing) -> Just <$> createRequestItem cd msg
                  (Just msg@(sharedMsgId, _), Just prevSharedMsgId)
                    | sharedMsgId == prevSharedMsgId ->
                        update msg `catchCINotFound` \_ -> Just <$> createRequestItem cd msg
                  (Nothing, Just prevSharedMsgId) -> Nothing <$ delete prevSharedMsgId
                  _ -> pure Nothing
            -- ##### Group link join requests (don't create contact requests) #####
            Just gli@GroupLinkInfo {groupId, memberRole = gLinkMemRole} -> do
              -- TODO [short links] deduplicate request by xContactId?
              gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
              acceptMember_ <- asks $ acceptMember . chatHooks . config
              maybe (pure $ Right (GAAccepted, gLinkMemRole)) (\am -> liftIO $ am gInfo gli p) acceptMember_ >>= \case
                Right (acceptance, useRole)
                  | v < groupFastLinkJoinVersion ->
                      messageError "processContactConnMessage: chat version range incompatible for accepting group join request"
                  | otherwise -> do
                      let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                      mem <- acceptGroupJoinRequestAsync user uclId gInfo invId chatVRange p xContactId_ Nothing welcomeMsgId_ acceptance useRole profileMode
                      (gInfo', mem', scopeInfo) <- mkGroupChatScope gInfo mem
                      createInternalChatItem user (CDGroupRcv gInfo' scopeInfo mem') (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
                      toView $ CEvtAcceptingGroupJoinRequestMember user gInfo' mem'
                Left rjctReason
                  | v < groupJoinRejectVersion ->
                      messageWarning $ "processContactConnMessage (group " <> groupName' gInfo <> "): joining of " <> displayName <> " is blocked"
                  | otherwise -> do
                      mem <- acceptGroupJoinSendRejectAsync user uclId gInfo invId chatVRange p xContactId_ rjctReason
                      toViewTE $ TERejectingGroupJoinRequestMember user gInfo mem rjctReason
        xGrpRelayInv :: InvitationId -> VersionRangeChat -> GroupRelayInvitation -> CM ()
        xGrpRelayInv invId chatVRange groupRelayInv = do
          (_gInfo, _ownerMember) <- withStore $ \db -> createRelayRequestGroup db vr user groupRelayInv invId chatVRange
          lift $ void $ getRelayRequestWorker True
        -- TODO [relays] owner, relays: TBC how to communicate member rejection rules from owner to relays
        -- TODO [relays] relay: TBC communicate rejection when memberId already exists (currently checked in createJoiningMember)
        memberJoinRequestViaRelay :: InvitationId -> VersionRangeChat -> Profile -> MemberId -> CM ()
        memberJoinRequestViaRelay invId chatVRange p joiningMemberId = do
          (_ucl, gLinkInfo_) <- withStore $ \db -> getUserContactLinkById db userId uclId
          case gLinkInfo_ of
            Just GroupLinkInfo {groupId, memberRole = gLinkMemRole} -> do
              gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
              mem <- acceptGroupJoinRequestAsync user uclId gInfo invId chatVRange p Nothing (Just joiningMemberId) Nothing GAAccepted gLinkMemRole Nothing
              (gInfo', mem', scopeInfo) <- mkGroupChatScope gInfo mem
              createInternalChatItem user (CDGroupRcv gInfo' scopeInfo mem') (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
              toView $ CEvtAcceptingGroupJoinRequestMember user gInfo' mem'
            Nothing ->
              messageError "memberJoinRequestViaRelay: no group link info for relay link"

    memberCanSend ::
      GroupMember ->
      Maybe MsgScope ->
      CM (Maybe DeliveryJobScope) ->
      CM (Maybe DeliveryJobScope)
    memberCanSend m@GroupMember {memberRole} msgScope a = case msgScope of
      Just MSMember {} -> a
      Nothing
        | memberRole > GRObserver || memberPending m -> a
        | otherwise -> messageError "member is not allowed to send messages" $> Nothing

    processConnMERR :: ConnectionEntity -> Connection -> AgentErrorType -> CM ()
    processConnMERR connEntity conn err = do
      case err of
        SMP _ SMP.AUTH -> do
          authErrCounter' <- withStore' $ \db -> incAuthErrCounter db user conn
          when (authErrCounter' >= authErrDisableCount) $ case connEntity of
            RcvDirectMsgConnection ctConn (Just ct) -> do
              toView $ CEvtContactDisabled user ct {activeConn = Just ctConn {authErrCounter = authErrCounter'}}
            _ -> toView $ CEvtConnectionDisabled connEntity
        SMP _ SMP.QUOTA ->
          unless (connInactive conn) $ do
            withStore' $ \db -> setQuotaErrCounter db user conn quotaErrSetOnMERR
            toView $ CEvtConnectionInactive connEntity True
        _ -> pure ()

    processConnMWARN :: ConnectionEntity -> Connection -> AgentErrorType -> CM ()
    processConnMWARN connEntity conn err = do
      case err of
        SMP _ SMP.QUOTA ->
          unless (connInactive conn) $ do
            quotaErrCounter' <- withStore' $ \db -> incQuotaErrCounter db user conn
            when (quotaErrCounter' >= quotaErrInactiveCount) $
              toView (CEvtConnectionInactive connEntity True)
        _ -> pure ()

    continueSending :: ConnectionEntity -> Connection -> CM Bool
    continueSending connEntity conn =
      if connInactive conn
        then do
          withStore' $ \db -> setQuotaErrCounter db user conn 0
          toView $ CEvtConnectionInactive connEntity False
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
      withAckMessage label cId msgMeta False Nothing $ \_ -> action $> (False, False)

    withAckMessage :: Text -> ConnId -> MsgMeta -> Bool -> Maybe (TVar [Text]) -> (Text -> CM (Bool, ShouldDeleteGroupConns)) -> CM ()
    withAckMessage label cId msgMeta showCritical tags action = do
      -- [async agent commands] command should be asynchronous
      -- TODO catching error and sending ACK after an error, particularly if it is a database error, will result in the message not processed (and no notification to the user).
      -- Possible solutions are:
      -- 1) retry processing several times
      -- 2) stabilize database
      -- 3) show screen of death to the user asking to restart
      eInfo <- eventInfo
      logInfo $ label <> ": " <> eInfo
      tryAllErrors (action eInfo) >>= \case
        Right (withRcpt, shouldDelConns) ->
          unless shouldDelConns $ withLog (eInfo <> " ok") $ ackMsg msgMeta $ if withRcpt then Just "" else Nothing
        -- If showCritical is True, then these errors don't result in ACK and show user visible alert
        -- This prevents losing the message that failed to be processed.
        Left (ChatErrorStore SEDBBusyError {message}) | showCritical -> throwError $ ChatErrorAgent (CRITICAL True message) (AgentConnId "") Nothing
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
          NETWORK _ -> SndErrExpired
          TIMEOUT -> SndErrExpired
          HOST -> srvErr SrvErrHost
          SMP.TRANSPORT TEVersion -> srvErr SrvErrVersion
          e -> srvErr . SrvErrOther $ tshow e

    badRcvFileChunk :: RcvFileTransfer -> String -> CM ()
    badRcvFileChunk ft err =
      unless (rcvFileCompleteOrCancelled ft) $ do
        cancelRcvFileTransfer user ft
        throwChatError $ CEFileRcvChunk err

    memberConnectedChatItem :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> CM ()
    memberConnectedChatItem gInfo scopeInfo m =
      -- ts should be broker ts but we don't have it for CON
      createInternalChatItem user (CDGroupRcv gInfo scopeInfo m) (CIRcvGroupEvent RGEMemberConnected) Nothing

    notifyMemberConnected :: GroupInfo -> GroupMember -> Maybe Contact -> CM ()
    notifyMemberConnected gInfo m ct_ = do
      (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
      memberConnectedChatItem gInfo' scopeInfo m'
      toView $ CEvtConnectedToGroupMember user gInfo' m' ct_

    probeMatchingMembers :: Contact -> IncognitoEnabled -> CM ()
    probeMatchingMembers ct connectedIncognito = do
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
          ms <- map COMGroupMember <$> withStore' (\db -> getMatchingMembers db vr user ct)
          sendProbeHashes ms probe probeId
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
      forM_ cgms $ \cgm -> sendProbeHash cgm `catchAllErrors` \_ -> pure ()
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
    messageWarning = toView . CEvtMessageError user "warning"

    messageError :: Text -> CM ()
    messageError = toView . CEvtMessageError user "error"

    newContentMessage :: Contact -> MsgContainer -> RcvMessage -> MsgMeta -> CM ()
    newContentMessage ct mc msg@RcvMessage {sharedMsgId_} msgMeta = do
      let ExtMsgContent content _ fInv_ _ _ _ _ = mcExtMsgContent mc
      -- Uncomment to test stuck delivery on errors - see test testDirectMessageDelete
      -- case content of
      --   MCText "hello 111" ->
      --     UE.throwIO $ userError "#####################"
      --     -- throwChatError $ CECommandError "#####################"
      --   _ -> pure ()
      if isVoice content && not (featureAllowed SCFVoice forContact ct)
        then do
          void $ newChatItem (ciContentNoParse $ CIRcvChatFeatureRejected CFVoice) Nothing Nothing False
        else do
          let ExtMsgContent _ _ _ itemTTL live_ _ _ = mcExtMsgContent mc
              timed_ = rcvContactCITimed ct itemTTL
              live = fromMaybe False live_
          file_ <- processFileInvitation fInv_ content $ \db -> createRcvFileTransfer db userId ct
          newChatItem (CIRcvMsgContent content, msgContentTexts content) (snd <$> file_) timed_ live
          autoAcceptFile file_
      where
        brokerTs = metaBrokerTs msgMeta
        newChatItem content ciFile_ timed_ live = do
          (ci, cInfo) <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs content ciFile_ timed_ live M.empty
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getDirectCIReactions db ct sharedMsgId) sharedMsgId_
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci {reactions}]

    autoAcceptFile :: Maybe (RcvFileTransfer, CIFile 'MDRcv) -> CM ()
    autoAcceptFile = mapM_ $ \(ft, CIFile {fileSize}) -> do
      -- ! autoAcceptFileSize is only used in tests
      ChatConfig {autoAcceptFileSize = sz} <- asks config
      when (sz > fileSize) $ receiveFileEvt' user ft False Nothing Nothing >>= toView

    messageFileDescription :: Contact -> SharedMsgId -> FileDescr -> CM ()
    messageFileDescription Contact {contactId} sharedMsgId fileDescr = do
      (fileId, aci) <- withStore $ \db -> do
        fileId <- getFileIdBySharedMsgId db userId contactId sharedMsgId
        aci <- getChatItemByFileId db vr user fileId
        pure (fileId, aci)
      processFDMessage fileId aci fileDescr

    groupMessageFileDescription :: GroupInfo -> Maybe MemberId -> SharedMsgId -> FileDescr -> CM (Maybe DeliveryJobScope)
    groupMessageFileDescription g@GroupInfo {groupId} memberId sharedMsgId fileDescr = do
      (fileId, aci) <- withStore $ \db -> do
        fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
        aci <- getChatItemByFileId db vr user fileId
        pure (fileId, aci)
      case aci of
        AChatItem SCTGroup SMDRcv (GroupChat _g scopeInfo) ChatItem {chatDir} -> case (memberId, chatDir) of
          (Just itemMemberId, CIGroupRcv m)
            | sameMemberId itemMemberId m -> do
                -- in processFDMessage some paths are programmed as errors,
                -- for example failure on not approved relays (CEFileNotApproved).
                -- we catch error, so that even if processFDMessage fails, message can still be forwarded.
                processFDMessage fileId aci fileDescr `catchAllErrors` \_ -> pure ()
                pure $ Just $ infoToDeliveryScope g scopeInfo
            | otherwise -> messageError "x.msg.file.descr: file of another member" $> Nothing
          (Nothing, CIChannelRcv) -> do
            processFDMessage fileId aci fileDescr `catchAllErrors` \_ -> pure ()
            pure $ Just $ infoToDeliveryScope g scopeInfo
          _ -> messageError "x.msg.file.descr: invalid file description part" $> Nothing
        _ -> messageError "x.msg.file.descr: invalid file description part" $> Nothing

    processFDMessage :: FileTransferId -> AChatItem -> FileDescr -> CM ()
    processFDMessage fileId aci fileDescr = do
      ft <- withStore $ \db -> getRcvFileTransfer db user fileId
      unless (rcvFileCompleteOrCancelled ft) $ do
        (rfd@RcvFileDescr {fileDescrComplete}, ft'@RcvFileTransfer {fileStatus, xftpRcvFile, cryptoArgs}) <- withStore $ \db -> do
          rfd <- appendRcvFD db userId fileId fileDescr
          -- reading second time in the same transaction as appending description
          -- to prevent race condition with accept
          ft' <- getRcvFileTransfer db user fileId
          pure (rfd, ft')
        when fileDescrComplete $ toView $ CEvtRcvFileDescrReady user aci ft' rfd
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
            ts = ciContentTexts content
        (ci, cInfo) <- saveRcvChatItem' user (CDDirectRcv ct) msg (Just sharedMsgId) brokerTs (content, ts) Nothing timed_ live M.empty
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          updateDirectChatItem' db user contactId ci content True live Nothing Nothing
        toView $ CEvtChatItemUpdated user (AChatItem SCTDirect SMDRcv cInfo ci')
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
                      toView $ CEvtChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci')
                      startUpdatedTimedItemThread user (ChatRef CTDirect contactId Nothing) ci ci'
                    else toView $ CEvtChatItemNotChanged user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci)
            _ -> messageError "x.msg.update: contact attempted invalid message update"

    messageDelete :: Contact -> SharedMsgId -> RcvMessage -> MsgMeta -> CM ()
    messageDelete ct@Contact {contactId} sharedMsgId _rcvMessage msgMeta = do
      deleteRcvChatItem `catchCINotFound` (toView . CEvtChatItemDeletedNotFound user ct)
      where
        brokerTs = metaBrokerTs msgMeta
        deleteRcvChatItem = do
          cci@(CChatItem msgDir ci) <- withStore $ \db -> getDirectChatItemBySharedMsgId db user contactId sharedMsgId
          case msgDir of
            SMDRcv
              | rcvItemDeletable ci brokerTs -> do
                  deletions <-
                    if featureAllowed SCFFullDelete forContact ct
                      then deleteDirectCIs user ct [cci]
                      else markDirectCIsDeleted user ct [cci] brokerTs
                  toView $ CEvtChatItemsDeleted user deletions False False
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
          cEvt_ <- withStore $ \db -> do
            CChatItem md ci <- getDirectChatItemBySharedMsgId db user (contactId' ct) sharedMsgId
            if ciReactionAllowed ci
              then liftIO $ do
                setDirectReaction db ct sharedMsgId False reaction add msgId brokerTs
                reactions <- getDirectCIReactions db ct sharedMsgId
                let ci' = CChatItem md ci {reactions}
                    r = ACIReaction SCTDirect SMDRcv (DirectChat ct) $ CIReaction CIDirectRcv ci' brokerTs reaction
                pure $ Just $ CEvtChatItemReaction user add r
              else pure Nothing
          mapM_ toView cEvt_

    groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    groupMsgReaction g m@GroupMember {memberRole} sharedMsgId itemMemberId scope_ reaction add RcvMessage {msgId} brokerTs
      | groupFeatureAllowed SGFReactions g = do
          rs <- withStore' $ \db -> getGroupReactions db g m itemMemberId sharedMsgId False
          if reactionAllowed add reaction rs
            then
              updateChatItemReaction `catchCINotFound` \_ -> case scope_ of
                Just (MSMember scopeMemberId)
                  | memberRole >= GRModerator || scopeMemberId == memberId' m ->
                      withStore $ \db -> do
                        liftIO $ setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                        Just . DJSMemberSupport <$> getScopeMemberIdViaMemberId db user g m scopeMemberId
                  | otherwise -> pure Nothing
                Nothing -> do
                  withStore' $ \db -> setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                  pure $ Just DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}
            else pure Nothing
      | otherwise = pure Nothing
      where
        updateChatItemReaction = do
          (CChatItem md ci, scopeInfo) <- withStore $ \db -> do
            cci <- case itemMemberId of
              Just itemMemberId' -> getGroupMemberCIBySharedMsgId db user g itemMemberId' sharedMsgId
              Nothing -> getGroupCIBySharedMsgId' db user g sharedMsgId
            scopeInfo <- getGroupChatScopeInfoForItem db vr user g (cChatItemId cci)
            pure (cci, scopeInfo)
          if ciReactionAllowed ci
            then do
              reactions <- withStore' $ \db -> do
                setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                getGroupCIReactions db g itemMemberId sharedMsgId
              let ci' = CChatItem md ci {reactions}
                  r = ACIReaction SCTGroup SMDRcv (GroupChat g scopeInfo) $ CIReaction (CIGroupRcv m) ci' brokerTs reaction
              toView $ CEvtChatItemReaction user add r
              pure $ Just $ infoToDeliveryScope g scopeInfo
            else pure Nothing

    reactionAllowed :: Bool -> MsgReaction -> [MsgReaction] -> Bool
    reactionAllowed add reaction rs = (reaction `elem` rs) /= add && not (add && length rs >= maxMsgReactions)

    catchCINotFound :: CM a -> (SharedMsgId -> CM a) -> CM a
    catchCINotFound f handle =
      f `catchAllErrors` \case
        ChatErrorStore (SEChatItemSharedMsgIdNotFound sharedMsgId) -> handle sharedMsgId
        e -> throwError e

    newGroupContentMessage :: GroupInfo -> GroupMember -> MsgContainer -> RcvMessage -> UTCTime -> Bool -> CM (Maybe DeliveryJobScope)
    newGroupContentMessage gInfo m@GroupMember {memberId, memberRole} mc msg@RcvMessage {sharedMsgId_} brokerTs forwarded = do
      (gInfo', m', scopeInfo) <- mkGetMessageChatScope vr user gInfo m content msgScope_
      if blockedByAdmin m'
        then createBlockedByAdmin gInfo' m' scopeInfo $> Nothing
        else case prohibitedGroupContent gInfo' m' scopeInfo content ft_ fInv_ False of
          Just f -> rejected gInfo' m' scopeInfo f $> Nothing
          Nothing ->
            withStore' (\db -> getCIModeration db vr user gInfo' memberId sharedMsgId_) >>= \case
              Just ciModeration -> do
                applyModeration gInfo' m' scopeInfo ciModeration
                withStore' $ \db -> deleteCIModeration db gInfo' memberId sharedMsgId_
                pure Nothing
              Nothing -> do
                createContentItem gInfo' m' scopeInfo
                pure $ Just $ infoToDeliveryScope gInfo scopeInfo
      where
        rejected gInfo' m' scopeInfo f = newChatItem gInfo' m' scopeInfo (ciContentNoParse $ CIRcvGroupFeatureRejected f) Nothing Nothing False
        timed' gInfo' = if forwarded then rcvCITimed_ (Just Nothing) itemTTL else rcvGroupCITimed gInfo' itemTTL
        live' = fromMaybe False live_
        ExtMsgContent content mentions fInv_ itemTTL live_ msgScope_ _ = mcExtMsgContent mc
        ts@(_, ft_) = msgContentTexts content
        saveRcvCI gInfo' m' scopeInfo = saveRcvChatItem' user (CDGroupRcv gInfo' scopeInfo m') msg sharedMsgId_ brokerTs
        createBlockedByAdmin gInfo' m' scopeInfo
          | groupFeatureAllowed SGFFullDelete gInfo' = do
              -- ignores member role when blocked by admin
              (ci, cInfo) <- saveRcvCI gInfo' m' scopeInfo (ciContentNoParse CIRcvBlocked) Nothing (timed' gInfo') False M.empty
              ci' <- withStore' $ \db -> updateGroupCIBlockedByAdmin db user gInfo' ci brokerTs
              groupMsgToView cInfo ci'
          | otherwise = do
              file_ <- processFileInv gInfo' m'
              (ci, cInfo) <- createNonLive gInfo' m' scopeInfo file_
              ci' <- withStore' $ \db -> markGroupCIBlockedByAdmin db user gInfo' ci
              groupMsgToView cInfo ci'
        applyModeration gInfo' m' scopeInfo CIModeration {moderatorMember = moderator@GroupMember {memberRole = moderatorRole}, moderatedAt}
          | moderatorRole < GRModerator || moderatorRole < memberRole =
              createContentItem gInfo' m' scopeInfo
          | groupFeatureMemberAllowed SGFFullDelete moderator gInfo' = do
              (ci, cInfo) <- saveRcvCI gInfo' m' scopeInfo (ciContentNoParse CIRcvModerated) Nothing (timed' gInfo') False M.empty
              ci' <- withStore' $ \db -> updateGroupChatItemModerated db user gInfo' ci moderator moderatedAt
              groupMsgToView cInfo ci'
          | otherwise = do
              file_ <- processFileInv gInfo' m'
              (ci, _cInfo) <- createNonLive gInfo' m' scopeInfo file_
              deletions <- markGroupCIsDeleted user gInfo' scopeInfo [CChatItem SMDRcv ci] (Just moderator) moderatedAt
              toView $ CEvtChatItemsDeleted user deletions False False
        createNonLive gInfo' m' scopeInfo file_ = do
          saveRcvCI gInfo' m' scopeInfo (CIRcvMsgContent content, ts) (snd <$> file_) (timed' gInfo') False mentions
        createContentItem gInfo' m' scopeInfo = do
          file_ <- processFileInv gInfo' m'
          newChatItem gInfo' m' scopeInfo (CIRcvMsgContent content, ts) (snd <$> file_) (timed' gInfo') live'
          unless (memberBlocked m') $ autoAcceptFile file_
        processFileInv gInfo' m' =
          processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId gInfo' (Just m')
        newChatItem gInfo' m' scopeInfo ciContent ciFile_ timed_ live = do
          let mentions' = if memberBlocked m' then [] else mentions
          (ci, cInfo) <- saveRcvCI gInfo' m' scopeInfo ciContent ciFile_ timed_ live mentions'
          ci' <- blockedMemberCI gInfo' m' ci
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getGroupCIReactions db gInfo' (Just memberId) sharedMsgId) sharedMsgId_
          groupMsgToView cInfo ci' {reactions}

    -- Channel content message: creates CDChannelRcv item and returns delivery scope
    newChannelContentMessage_ :: GroupInfo -> MsgContainer -> RcvMessage -> UTCTime -> Bool -> CM (Maybe DeliveryJobScope)
    newChannelContentMessage_ gInfo mc msg@RcvMessage {sharedMsgId_} brokerTs forwarded = do
      let ExtMsgContent content mentions fInv_ itemTTL live_ _ _ = mcExtMsgContent mc
          ts = msgContentTexts content
          live' = fromMaybe False live_
          timed_ = if forwarded then rcvCITimed_ (Just Nothing) itemTTL else rcvGroupCITimed gInfo itemTTL
      file_ <- processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId gInfo Nothing
      let saveRcvCI = saveRcvChatItem' user (CDChannelRcv gInfo Nothing) msg sharedMsgId_ brokerTs
      (ci, cInfo) <- saveRcvCI (CIRcvMsgContent content, ts) (snd <$> file_) timed_ live' mentions
      reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getGroupCIReactions db gInfo Nothing sharedMsgId) sharedMsgId_
      groupMsgToView cInfo ci {reactions}
      autoAcceptFile file_
      pure $ Just $ infoToDeliveryScope gInfo Nothing

    -- Channel message update: looks up channel item, updates it, returns delivery scope
    channelMessageUpdate_ :: GroupInfo -> SharedMsgId -> MsgContent -> Map MemberName MsgMention -> RcvMessage -> UTCTime -> Maybe Int -> Maybe Bool -> Bool -> CM (Maybe DeliveryJobScope)
    channelMessageUpdate_ gInfo@GroupInfo {groupId} sharedMsgId mc mentions msg@RcvMessage {msgId} brokerTs ttl_ live_ forwarded = do
      updateRcvChatItem `catchCINotFound` \_ -> do
        let timed_ = if forwarded then rcvCITimed_ (Just Nothing) ttl_ else rcvGroupCITimed gInfo ttl_
        (ci, cInfo) <- saveRcvChatItem' user (CDChannelRcv gInfo Nothing) msg (Just sharedMsgId) brokerTs (content, ts) Nothing timed_ live M.empty
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          updateGroupChatItem db user groupId ci content True live Nothing
        toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv cInfo ci')
        pure $ Just $ infoToDeliveryScope gInfo Nothing
      where
        content = CIRcvMsgContent mc
        ts@(_, ft_) = msgContentTexts mc
        live = fromMaybe False live_
        updateRcvChatItem = do
          (cci, scopeInfo) <- withStore $ \db -> do
            cci <- getGroupCIBySharedMsgId' db user gInfo sharedMsgId
            (cci,) <$> getGroupChatScopeInfoForItem db vr user gInfo (cChatItemId cci)
          case cci of
            CChatItem SMDRcv ci@ChatItem {chatDir = CIChannelRcv, meta = CIMeta {itemLive}, content = CIRcvMsgContent oldMC} -> do
              let changed = mc /= oldMC
              if changed || fromMaybe False itemLive
                then do
                  ci' <- withStore' $ \db -> do
                    when changed $
                      addInitialAndNewCIVersions db (chatItemId' ci) (chatItemTs' ci, oldMC) (brokerTs, mc)
                    reactions <- getGroupCIReactions db gInfo Nothing sharedMsgId
                    let edited = itemLive /= Just True
                    ciMentions <- getRcvCIMentions db user gInfo ft_ mentions
                    ci' <- updateGroupChatItem db user groupId ci {reactions} content edited live $ Just msgId
                    updateGroupCIMentions db gInfo ci' ciMentions
                  toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo scopeInfo) ci')
                  startUpdatedTimedItemThread user (ChatRef CTGroup groupId $ toChatScope <$> scopeInfo) ci ci'
                  pure $ Just $ infoToDeliveryScope gInfo scopeInfo
                else do
                  toView $ CEvtChatItemNotChanged user (AChatItem SCTGroup SMDRcv (GroupChat gInfo scopeInfo) ci)
                  pure Nothing
            _ -> messageError "x.msg.update: invalid channel message update" $> Nothing

    -- Channel message delete: looks up channel item, deletes it, returns delivery scope
    channelMessageDelete :: GroupInfo -> SharedMsgId -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    channelMessageDelete gInfo sharedMsgId _rcvMsg brokerTs = do
      withStore' (\db -> runExceptT $ getGroupCIBySharedMsgId' db user gInfo sharedMsgId) >>= \case
        Right cci@(CChatItem _ ChatItem {chatDir}) -> case chatDir of
          CIChannelRcv -> do
            scopeInfo <- withStore $ \db -> getGroupChatScopeInfoForItem db vr user gInfo (cChatItemId cci)
            deletions <-
              if groupFeatureAllowed SGFFullDelete gInfo
                then deleteGroupCIs user gInfo scopeInfo [cci] Nothing brokerTs
                else markGroupCIsDeleted user gInfo scopeInfo [cci] Nothing brokerTs
            toView $ CEvtChatItemsDeleted user deletions False False
            pure $ Just $ infoToDeliveryScope gInfo scopeInfo
          _ -> messageError "x.msg.del: invalid channel message delete" $> Nothing
        Left e ->
          messageError ("x.msg.del: channel message not found, " <> tshow e) $> Nothing

    -- Channel file description: processes file description for channel item, returns delivery scope
    channelMessageFileDescription :: GroupInfo -> SharedMsgId -> FileDescr -> CM (Maybe DeliveryJobScope)
    channelMessageFileDescription gInfo sharedMsgId fileDescr =
      groupMessageFileDescription gInfo Nothing sharedMsgId fileDescr

    xFileCancelChannel :: GroupInfo -> SharedMsgId -> CM (Maybe DeliveryJobScope)
    xFileCancelChannel gInfo sharedMsgId =
      xFileCancelGroup gInfo Nothing sharedMsgId

    groupMessageUpdate :: GroupInfo -> GroupMember -> SharedMsgId -> MsgContent -> Map MemberName MsgMention -> Maybe MsgScope -> RcvMessage -> UTCTime -> Maybe Int -> Maybe Bool -> CM (Maybe DeliveryJobScope)
    groupMessageUpdate gInfo@GroupInfo {groupId} m@GroupMember {groupMemberId, memberId} sharedMsgId mc mentions msgScope_ msg@RcvMessage {msgId} brokerTs ttl_ live_
      | prohibitedSimplexLinks gInfo m ft_ =
          messageWarning ("x.msg.update ignored: feature not allowed " <> groupFeatureNameText GFSimplexLinks) $> Nothing
      | otherwise = do
          updateRcvChatItem `catchCINotFound` \_ -> do
            -- This patches initial sharedMsgId into chat item when locally deleted chat item
            -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
            -- Chat item and update message which created it will have different sharedMsgId in this case...
            let timed_ = rcvGroupCITimed gInfo ttl_
                mentions' = if memberBlocked m then [] else mentions
            (gInfo', m', scopeInfo) <- mkGetMessageChatScope vr user gInfo m mc msgScope_
            (ci, cInfo) <- saveRcvChatItem' user (CDGroupRcv gInfo' scopeInfo m') msg (Just sharedMsgId) brokerTs (content, ts) Nothing timed_ live mentions'
            ci' <- withStore' $ \db -> do
              createChatItemVersion db (chatItemId' ci) brokerTs mc
              updateGroupChatItem db user groupId ci content True live Nothing
            ci'' <- blockedMemberCI gInfo' m' ci'
            toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv cInfo ci'')
            pure $ Just $ infoToDeliveryScope gInfo scopeInfo
      where
        content = CIRcvMsgContent mc
        ts@(_, ft_) = msgContentTexts mc
        live = fromMaybe False live_
        updateRcvChatItem = do
          (cci, scopeInfo) <- withStore $ \db -> do
            cci <- getGroupChatItemBySharedMsgId db user gInfo groupMemberId sharedMsgId
            (cci,) <$> getGroupChatScopeInfoForItem db vr user gInfo (cChatItemId cci)
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
                        reactions <- getGroupCIReactions db gInfo (Just memberId) sharedMsgId
                        let edited = itemLive /= Just True
                        ciMentions <- getRcvCIMentions db user gInfo ft_ mentions
                        ci' <- updateGroupChatItem db user groupId ci {reactions} content edited live $ Just msgId
                        updateGroupCIMentions db gInfo ci' ciMentions
                      toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo scopeInfo) ci')
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId $ toChatScope <$> scopeInfo) ci ci'
                      pure $ Just $ infoToDeliveryScope gInfo scopeInfo
                    else do
                      toView $ CEvtChatItemNotChanged user (AChatItem SCTGroup SMDRcv (GroupChat gInfo scopeInfo) ci)
                      pure Nothing
                else messageError "x.msg.update: group member attempted to update a message of another member" $> Nothing
            _ -> messageError "x.msg.update: group member attempted invalid message update" $> Nothing

    groupMessageDelete :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    groupMessageDelete gInfo@GroupInfo {membership} m@GroupMember {memberId, memberRole = senderRole} sharedMsgId sndMemberId_ scope_ RcvMessage {msgId} brokerTs = do
      let msgMemberId = fromMaybe memberId sndMemberId_
      withStore' (\db -> runExceptT $ getGroupMemberCIBySharedMsgId db user gInfo msgMemberId sharedMsgId) >>= \case
        Right cci@(CChatItem _ ci@ChatItem {chatDir}) -> case chatDir of
          CIGroupRcv mem -> case sndMemberId_ of
            -- regular deletion
            Nothing
              | sameMemberId memberId mem && msgMemberId == memberId && rcvItemDeletable ci brokerTs ->
                  Just <$> delete cci Nothing
              | otherwise ->
                  messageError "x.msg.del: member attempted invalid message delete" $> Nothing
            -- moderation (not limited by time)
            Just _
              | sameMemberId memberId mem && msgMemberId == memberId ->
                  Just <$> delete cci (Just m)
              | otherwise ->
                  moderate mem cci
          CIGroupSnd -> moderate membership cci
          CIChannelRcv -> messageError "x.msg.del: unexpected channel message in member delete" $> Nothing
        Left e
          | msgMemberId == memberId ->
              messageError ("x.msg.del: message not found, " <> tshow e) $> Nothing
          | senderRole < GRModerator -> do
              messageError $ "x.msg.del: message not found, message of another member with insufficient member permissions, " <> tshow e
              pure Nothing
          | otherwise -> case scope_ of
              Just (MSMember scopeMemberId) ->
                withStore $ \db -> do
                  liftIO $ createCIModeration db gInfo m msgMemberId sharedMsgId msgId brokerTs
                  Just . DJSMemberSupport <$> getScopeMemberIdViaMemberId db user gInfo m scopeMemberId
              Nothing -> do
                withStore' $ \db -> createCIModeration db gInfo m msgMemberId sharedMsgId msgId brokerTs
                pure $ Just DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}
      where
        moderate :: GroupMember -> CChatItem 'CTGroup -> CM (Maybe DeliveryJobScope)
        moderate mem cci = case sndMemberId_ of
          Just sndMemberId
            | sameMemberId sndMemberId mem -> checkRole mem $ do
                jobScope <- delete cci (Just m)
                archiveMessageReports cci m
                pure $ Just jobScope
            | otherwise -> messageError "x.msg.del: message of another member with incorrect memberId" $> Nothing
          _ -> messageError "x.msg.del: message of another member without memberId" $> Nothing
        checkRole GroupMember {memberRole} a
          | senderRole < GRModerator || senderRole < memberRole =
              messageError "x.msg.del: message of another member with insufficient member permissions" $> Nothing
          | otherwise = a
        delete :: CChatItem 'CTGroup -> Maybe GroupMember -> CM DeliveryJobScope
        delete cci byGroupMember = do
          scopeInfo <- withStore $ \db -> getGroupChatScopeInfoForItem db vr user gInfo (cChatItemId cci)
          deletions <-
            if groupFeatureMemberAllowed SGFFullDelete m gInfo
              then deleteGroupCIs user gInfo scopeInfo [cci] byGroupMember brokerTs
              else markGroupCIsDeleted user gInfo scopeInfo [cci] byGroupMember brokerTs
          toView $ CEvtChatItemsDeleted user deletions False False
          pure $ infoToDeliveryScope gInfo scopeInfo
        archiveMessageReports :: CChatItem 'CTGroup -> GroupMember -> CM ()
        archiveMessageReports (CChatItem _ ci) byMember = do
          ciIds <- withStore' $ \db -> markMessageReportsDeleted db user gInfo ci byMember brokerTs
          unless (null ciIds) $ toView $ CEvtGroupChatItemsDeleted user gInfo ciIds False (Just byMember)

    -- TODO remove once XFile is discontinued
    processFileInvitation' :: Contact -> FileInvitation -> RcvMessage -> MsgMeta -> CM ()
    processFileInvitation' ct fInv' msg@RcvMessage {sharedMsgId_} msgMeta = do
      ChatConfig {fileChunkSize} <- asks config
      let fInv@FileInvitation {fileName, fileSize} = mkValidFileInvitation fInv'
      inline <- receiveInlineMode fInv Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFileTransfer db userId ct fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
          content = ciContentNoParse $ CIRcvMsgContent $ MCFile ""
      (ci, cInfo) <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs content ciFile Nothing False M.empty
      toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]
      where
        brokerTs = metaBrokerTs msgMeta

    -- TODO remove once XFile is discontinued
    processGroupFileInvitation' :: GroupInfo -> GroupMember -> FileInvitation -> RcvMessage -> UTCTime -> CM ()
    processGroupFileInvitation' gInfo m fInv@FileInvitation {fileName, fileSize} msg@RcvMessage {sharedMsgId_} brokerTs = do
      ChatConfig {fileChunkSize} <- asks config
      inline <- receiveInlineMode fInv Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvGroupFileTransfer db userId gInfo (Just m) fInv inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
          content = ciContentNoParse $ CIRcvMsgContent $ MCFile ""
      (ci, cInfo) <- saveRcvChatItem' user (CDGroupRcv gInfo Nothing m) msg sharedMsgId_ brokerTs content ciFile Nothing False M.empty
      ci' <- blockedMemberCI gInfo m ci
      groupMsgToView cInfo ci'

    blockedMemberCI :: GroupInfo -> GroupMember -> ChatItem 'CTGroup 'MDRcv -> CM (ChatItem 'CTGroup 'MDRcv)
    blockedMemberCI gInfo m ci
      | blockedByAdmin m =
          withStore' $ \db -> markGroupCIBlockedByAdmin db user gInfo ci
      | not (showMessages $ memberSettings m) =
          withStore' $ \db -> markGroupChatItemBlocked db user gInfo ci
      | otherwise =
          pure ci

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
      (fileId, ft) <- withStore $ \db -> do
        fileId <- getFileIdBySharedMsgId db userId contactId sharedMsgId
        (fileId,) <$> getRcvFileTransfer db user fileId
      unless (rcvFileCompleteOrCancelled ft) $ do
        cancelRcvFileTransfer user ft
        ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
        toView $ CEvtRcvFileSndCancelled user ci ft

    xFileAcptInv :: Contact -> SharedMsgId -> Maybe ConnReqInvitation -> String -> CM ()
    xFileAcptInv ct sharedMsgId fileConnReq_ fName = do
      (fileId, AChatItem _ _ _ ci) <- withStore $ \db -> do
        fileId <- getDirectFileIdBySharedMsgId db user ct sharedMsgId
        (fileId,) <$> getChatItemByFileId db vr user fileId
      assertSMPAcceptNotProhibited ci
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      -- [async agent commands] no continuation needed, but command should be asynchronous for stability
      if fName == fileName
        then unless cancelled $ case fileConnReq_ of
          -- receiving inline
          Nothing -> do
            event <- withStore $ \db -> do
              ci' <- updateDirectCIFileStatus db vr user fileId $ CIFSSndTransfer 0 1
              sft <- createSndDirectInlineFT db ct ft
              pure $ CEvtSndFileStart user ci' sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendDirectFileInline user ct ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
          Just _fileConnReq -> messageError "x.file.acpt.inv: receiving file via a separate connection is deprecated"
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
          updateDirectCIFileStatus db vr user fileId CIFSSndComplete
        case file of
          Just CIFile {fileProtocol = FPXFTP} -> do
            ft <- withStore $ \db -> getFileTransferMeta db user fileId
            toView $ CEvtSndFileCompleteXFTP user ci ft
          _ -> toView $ CEvtSndFileComplete user ci sft

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

    xFileCancelGroup :: GroupInfo -> Maybe MemberId -> SharedMsgId -> CM (Maybe DeliveryJobScope)
    xFileCancelGroup g@GroupInfo {groupId} memberId sharedMsgId = do
      (fileId, aci) <- withStore $ \db -> do
        fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
        (fileId,) <$> getChatItemByFileId db vr user fileId
      case aci of
        AChatItem SCTGroup SMDRcv (GroupChat _g scopeInfo) ChatItem {chatDir} -> case (memberId, chatDir) of
          (Just itemMemberId, CIGroupRcv m)
            | sameMemberId itemMemberId m -> do
                ft <- withStore $ \db -> getRcvFileTransfer db user fileId
                unless (rcvFileCompleteOrCancelled ft) $ do
                  cancelRcvFileTransfer user ft
                  toView $ CEvtRcvFileSndCancelled user aci ft
                pure $ Just $ infoToDeliveryScope g scopeInfo
            -- shouldn't happen now that query includes group member id
            | otherwise -> messageError "x.file.cancel: group member attempted to cancel file of another member" $> Nothing
          (Nothing, CIChannelRcv) -> do
            ft <- withStore $ \db -> getRcvFileTransfer db user fileId
            unless (rcvFileCompleteOrCancelled ft) $ do
              cancelRcvFileTransfer user ft
              toView $ CEvtRcvFileSndCancelled user aci ft
            pure $ Just $ infoToDeliveryScope g Nothing
          _ -> messageError "x.file.cancel: group member attempted invalid file cancel" $> Nothing
        _ -> messageError "x.file.cancel: group member attempted invalid file cancel" $> Nothing

    xFileAcptInvGroup :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe ConnReqInvitation -> String -> CM ()
    xFileAcptInvGroup GroupInfo {groupId} m@GroupMember {activeConn} sharedMsgId fileConnReq_ fName = do
      (fileId, AChatItem _ _ _ ci) <- withStore $ \db -> do
        fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
        (fileId,) <$> getChatItemByFileId db vr user fileId
      assertSMPAcceptNotProhibited ci
      -- TODO check that it's not already accepted
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      if fName == fileName
        then unless cancelled $ case (fileConnReq_, activeConn) of
          (Nothing, Just conn) -> do
            -- receiving inline
            event <- withStore $ \db -> do
              ci' <- updateDirectCIFileStatus db vr user fileId $ CIFSSndTransfer 0 1
              sft <- liftIO $ createSndGroupInlineFT db m conn ft
              pure $ CEvtSndFileStart user ci' sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendMemberFileInline m conn ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
          (Just _fileConnReq, _) -> messageError "x.file.acpt.inv: receiving file via a separate connection is deprecated"
          _ -> messageError "x.file.acpt.inv: member connection is not active"
        else messageError "x.file.acpt.inv: fileName is different from expected"

    groupMsgToView :: forall d. MsgDirectionI d => ChatInfo 'CTGroup -> ChatItem 'CTGroup d -> CM ()
    groupMsgToView cInfo ci = do
      toView $ CEvtNewChatItems user [AChatItem SCTGroup (msgDirection @d) cInfo ci]

    processGroupInvitation :: Contact -> GroupInvitation -> RcvMessage -> MsgMeta -> CM ()
    processGroupInvitation ct inv msg msgMeta = do
      let Contact {localDisplayName = c, activeConn} = ct
          GroupInvitation {fromMember = (MemberIdRole fromMemId fromRole), invitedMember = (MemberIdRole memId memRole), connRequest, groupLinkId} = inv
      forM_ activeConn $ \Connection {connId, connChatVersion, peerChatVRange, customUserProfileId, groupLinkId = groupLinkId'} -> do
        when (fromRole < GRAdmin || fromRole < memRole) $ throwChatError (CEGroupContactRole c)
        when (fromMemId == memId) $ throwChatError CEGroupDuplicateMemberId
        -- [incognito] if direct connection with host is incognito, create membership using the same incognito profile
        (gInfo@GroupInfo {groupId, localDisplayName, groupProfile, membership}, hostId) <- withStore $ \db -> createGroupInvitation db vr user ct inv customUserProfileId
        void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing (Just epochStart)
        let GroupMember {groupMemberId, memberId = membershipMemId} = membership
        if sameGroupLinkId groupLinkId groupLinkId'
          then do
            subMode <- chatReadVar subscriptionMode
            dm <- encodeConnInfo $ XGrpAcpt membershipMemId
            connIds <- joinAgentConnectionAsync user Nothing True connRequest dm subMode
            withStore' $ \db -> do
              setViaGroupLinkUri db groupId connId
              createMemberConnectionAsync db user hostId connIds connChatVersion peerChatVRange subMode
              updateGroupMemberStatusById db userId hostId GSMemAccepted
              updateGroupMemberStatus db userId membership GSMemAccepted
            toView $ CEvtUserAcceptedGroupSent user gInfo {membership = membership {memberStatus = GSMemAccepted}} (Just ct)
          else do
            let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
            (ci, cInfo) <- saveRcvChatItemNoParse user (CDDirectRcv ct) msg brokerTs content
            withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]
            toView $ CEvtReceivedGroupInvitation {user, groupInfo = gInfo, contact = ct, fromMemberRole = fromRole, memberRole = memRole}
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
          (ct', contactConns) <- withStore' $ \db -> do
            ct' <- updateContactStatus db user c CSDeleted
            (ct',) <$> getContactConnections db vr userId ct'
          deleteAgentConnectionsAsync $ map aConnId contactConns
          forM_ contactConns $ \conn -> withStore' $ \db -> updateConnectionStatus db conn ConnDeleted
          activeConn' <- forM (contactConn ct') $ \conn -> pure conn {connStatus = ConnDeleted}
          let ct'' = ct' {activeConn = activeConn'} :: Contact
          (ci, cInfo) <- saveRcvChatItemNoParse user (CDDirectRcv ct'') msg brokerTs (CIRcvDirectEvent RDEContactDeleted)
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]
          toView $ CEvtContactDeletedByContact user ct''
        else do
          contactConns <- withStore' $ \db -> getContactConnections db vr userId c
          deleteAgentConnectionsAsync $ map aConnId contactConns
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
          toView $ CEvtContactUpdated user c c'
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
              n' /= n || fn' /= fn || sd /= sd' || i' /= i || cl' /= cl
            Profile {displayName = n, fullName = fn, shortDescr = sd, image = i, contactLink = cl} = p
            Profile {displayName = n', fullName = fn', shortDescr = sd', image = i', contactLink = cl'} = p'

    xInfoMember :: GroupInfo -> GroupMember -> Profile -> UTCTime -> CM (Maybe DeliveryJobScope)
    xInfoMember gInfo m p' brokerTs = do
      void $ processMemberProfileUpdate gInfo m p' True (Just brokerTs)
      pure $ memberEventDeliveryScope m

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

    xGrpLinkAcpt :: GroupInfo -> GroupMember -> GroupAcceptance -> GroupMemberRole -> MemberId -> RcvMessage -> UTCTime -> CM ()
    xGrpLinkAcpt gInfo@GroupInfo {membership} m acceptance role memberId msg brokerTs
      | sameMemberId memberId membership = processUserAccepted
      | otherwise =
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memberId) >>= \case
            Left _ -> messageError "x.grp.link.acpt error: referenced member does not exist"
            Right referencedMember -> do
              (referencedMember', gInfo') <- withStore' $ \db -> do
                referencedMember' <- updateGroupMemberAccepted db user referencedMember (newMemberStatus referencedMember) role
                gInfo' <- updateGroupMembersRequireAttention db user gInfo referencedMember referencedMember'
                pure (referencedMember', gInfo')
              when (memberCategory referencedMember == GCInviteeMember) $ introduceToRemainingMembers referencedMember'
              -- create item in both scopes
              memberConnectedChatItem gInfo' Nothing referencedMember'
              let scopeInfo = Just $ GCSIMemberSupport {groupMember_ = Just referencedMember'}
                  gEvent = RGEMemberAccepted (groupMemberId' referencedMember') (fromLocalProfile $ memberProfile referencedMember')
              (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo' scopeInfo m) msg brokerTs (CIRcvGroupEvent gEvent)
              groupMsgToView cInfo ci
              toView $ CEvtMemberAcceptedByOther user gInfo' m referencedMember'
              where
                newMemberStatus refMem = case memberConn refMem of
                  Just c | connReady c -> GSMemConnected
                  _ -> GSMemAnnounced
      where
        processUserAccepted = case acceptance of
          GAAccepted -> do
            membership' <- withStore' $ \db -> updateGroupMemberAccepted db user membership GSMemConnected role
            -- create item in both scopes
            let gInfo' = gInfo {membership = membership'}
                cd = CDGroupRcv gInfo' Nothing m
            createInternalChatItem user cd (CIRcvGroupE2EEInfo E2EInfo {pqEnabled = Just PQEncOff}) Nothing
            let prepared = preparedGroup gInfo'
            unless (isJust prepared) $ createGroupFeatureItems user cd CIRcvGroupFeature gInfo'
            let welcomeMsgId_ = (\PreparedGroup {welcomeSharedMsgId = mId} -> mId) <$> preparedGroup gInfo'
            unless (isJust welcomeMsgId_) $ maybeCreateGroupDescrLocal gInfo' m
            createInternalChatItem user cd (CIRcvGroupEvent RGEUserAccepted) Nothing
            let scopeInfo = Just $ GCSIMemberSupport {groupMember_ = Nothing}
            createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m) (CIRcvGroupEvent RGEUserAccepted) Nothing
            toView $ CEvtUserJoinedGroup user gInfo' m
          GAPendingReview -> do
            membership' <- withStore' $ \db -> updateGroupMemberAccepted db user membership GSMemPendingReview role
            let gInfo' = gInfo {membership = membership'}
                scopeInfo = Just $ GCSIMemberSupport {groupMember_ = Nothing}
            createInternalChatItem user (CDGroupSnd gInfo' scopeInfo) (CISndGroupEvent SGEUserPendingReview) Nothing
            toView $ CEvtMemberAcceptedByOther user gInfo' m membership'
          GAPendingApproval ->
            messageWarning "x.grp.link.acpt: unexpected group acceptance - pending approval"
        introduceToRemainingMembers acceptedMember = do
          introduceToRemaining vr user gInfo acceptedMember
          when (groupFeatureAllowed SGFHistory gInfo) $ sendHistory user gInfo acceptedMember

    maybeCreateGroupDescrLocal :: GroupInfo -> GroupMember -> CM ()
    maybeCreateGroupDescrLocal gInfo@GroupInfo {groupProfile = GroupProfile {description}} m =
      unless expectHistory $ forM_ description $ \descr ->
        createInternalChatItem user (CDGroupRcv gInfo Nothing m) (CIRcvMsgContent $ MCText descr) Nothing
      where
        expectHistory = groupFeatureAllowed SGFHistory gInfo && m `supportsVersion` groupHistoryIncludeWelcomeVersion

    processMemberProfileUpdate :: GroupInfo -> GroupMember -> Profile -> Bool -> Maybe UTCTime -> CM GroupMember
    processMemberProfileUpdate gInfo m@GroupMember {memberProfile = p, memberContactId} p' createItems itemTs_
      | redactedMemberProfile allowSimplexLinks (fromLocalProfile p) /= redactedMemberProfile allowSimplexLinks p' = do
          updateBusinessChatProfile gInfo
          case memberContactId of
            Nothing -> do
              m' <- withStore $ \db -> updateMemberProfile db user m p'
              createProfileUpdatedItem m'
              toView $ CEvtGroupMemberUpdated user gInfo m m'
              pure m'
            Just mContactId -> do
              mCt <- withStore $ \db -> getContact db vr user mContactId
              if canUpdateProfile mCt
                then do
                  (m', ct') <- withStore $ \db -> updateContactMemberProfile db user m mCt p'
                  createProfileUpdatedItem m'
                  toView $ CEvtGroupMemberUpdated user gInfo m m'
                  toView $ CEvtContactUpdated user mCt ct'
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
        allowSimplexLinks = groupFeatureMemberAllowed SGFSimplexLinks m gInfo
        updateBusinessChatProfile g@GroupInfo {businessChat} = case businessChat of
          Just bc | isMainBusinessMember bc m -> do
            g' <- withStore $ \db -> updateGroupProfileFromMember db user g p'
            toView $ CEvtGroupUpdated user g g' (Just m)
          _ -> pure ()
        isMainBusinessMember BusinessChatInfo {chatType, businessId, customerId} GroupMember {memberId} = case chatType of
          BCBusiness -> businessId == memberId
          BCCustomer -> customerId == memberId
        createProfileUpdatedItem m' =
          when createItems $ do
            (gInfo', m'', scopeInfo) <- mkGroupChatScope gInfo m'
            let ciContent = CIRcvGroupEvent $ RGEMemberProfileUpdated (fromLocalProfile p) p'
            createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m'') ciContent itemTs_

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
          cgm2''_ <- probeMatch cgm1' cgm2' probe `catchAllErrors` \_ -> pure (Just cgm2')
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
        COMContact c1@Contact {profile = p1} ->
          case cgm2 of
            COMGroupMember m2@GroupMember {memberProfile = p2, memberContactId}
              | isNothing memberContactId && profilesMatch p1 p2 -> do
                  void . sendDirectContactMessage user c1 $ XInfoProbeOk probe
                  COMContact <$$> associateMemberAndContact c1 m2
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or member already has contact" >> pure Nothing
            COMContact _ -> messageWarning "probeMatch ignored: contacts are not merged" >> pure Nothing
        COMGroupMember m1@GroupMember {groupId, memberProfile = p1, memberContactId} ->
          case cgm2 of
            COMContact c2@Contact {profile = p2}
              | memberCurrent m1 && isNothing memberContactId && profilesMatch p1 p2 ->
                  case memberConn m1 of
                    Just conn -> do
                      void $ sendDirectMemberMessage conn (XInfoProbeOk probe) groupId
                      COMContact <$$> associateMemberAndContact c2 m1
                    _ -> messageWarning "probeMatch ignored: matched member doesn't have connection" >> pure Nothing
              | otherwise -> messageWarning "probeMatch ignored: profiles don't match or member already has contact or member not current" >> pure Nothing
            COMGroupMember _ -> messageWarning "probeMatch ignored: members are not matched with members" >> pure Nothing

    xInfoProbeOk :: ContactOrMember -> Probe -> CM ()
    xInfoProbeOk cgm1 probe = do
      cgm2 <- withStore' $ \db -> matchSentProbe db vr user cgm1 probe
      case cgm1 of
        COMContact c1 ->
          case cgm2 of
            Just (COMGroupMember m2@GroupMember {memberContactId})
              | isNothing memberContactId -> void $ associateMemberAndContact c1 m2
              | otherwise -> messageWarning "xInfoProbeOk ignored: member already has contact"
            Just (COMContact _) -> messageWarning "xInfoProbeOk ignored: contacts are not merged"
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
          (ci, cInfo) <- saveCallItem CISCallPending
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
          toView $ CEvtCallInvitation RcvCallInvitation {user, contact = ct, callType, sharedKey, callUUID, callTs = chatItemTs' ci}
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]
        else featureRejected CFCalls
      where
        brokerTs = metaBrokerTs msgMeta
        saveCallItem status = saveRcvChatItemNoParse user (CDDirectRcv ct) msg brokerTs (CIRcvCall status 0)
        featureRejected f = do
          let content = ciContentNoParse $ CIRcvChatFeatureRejected f
          (ci, cInfo) <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs content Nothing Nothing False M.empty
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]

    -- to party initiating call
    xCallOffer :: Contact -> CallId -> CallOffer -> RcvMessage -> CM ()
    xCallOffer ct callId CallOffer {callType, rtcSession, callDhPubKey} msg = do
      msgCurrentCall ct callId "x.call.offer" msg $
        \call -> case callState call of
          CallInvitationSent {localCallType, localDhPrivKey} -> do
            let sharedKey = C.Key . C.dhBytes' <$> (C.dh' <$> callDhPubKey <*> localDhPrivKey)
                callState' = CallOfferReceived {localCallType, peerCallType = callType, peerCallSession = rtcSession, sharedKey}
                askConfirmation = encryptedCall localCallType && not (encryptedCall callType)
            toView CEvtCallOffer {user, contact = ct, callType, offer = rtcSession, sharedKey, askConfirmation}
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
            toView $ CEvtCallAnswer user ct rtcSession
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
            toView $ CEvtCallExtraInfo user ct rtcExtraInfo
            pure (Just call {callState = callState'}, Nothing)
          CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey} -> do
            -- TODO update the list of ice servers in peerCallSession
            let callState' = CallNegotiated {localCallType, peerCallType, localCallSession, peerCallSession, sharedKey}
            toView $ CEvtCallExtraInfo user ct rtcExtraInfo
            pure (Just call {callState = callState'}, Nothing)
          _ -> do
            msgCallStateError "x.call.extra" call
            pure (Just call, Nothing)

    -- to any call party
    xCallEnd :: Contact -> CallId -> RcvMessage -> CM ()
    xCallEnd ct callId msg =
      msgCurrentCall ct callId "x.call.end" msg $ \Call {chatItemId} -> do
        toView $ CEvtCallEnded user ct
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
                  startProximateTimedItemThread user (ChatRef CTDirect ctId' Nothing, chatItemId)

    msgCallStateError :: Text -> Call -> CM ()
    msgCallStateError eventName Call {callState} =
      messageError $ eventName <> ": wrong call state " <> T.pack (show $ callStateTag callState)

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
      g <- withStore $ \db -> do
        liftIO $ associateMemberWithContactRecord db user c1 m2
        getGroupInfo db vr user groupId
      toView $ CEvtContactAndMemberAssociated user c1 g m2 c1
      pure c1

    associateContactWithMember :: GroupMember -> Contact -> CM Contact
    associateContactWithMember m1@GroupMember {groupId} c2 = do
      (c2', g) <- withStore $ \db ->
        liftM2 (,) (associateContactWithMemberRecord db vr user m1 c2) (getGroupInfo db vr user groupId)
      toView $ CEvtContactAndMemberAssociated user c2 g m1 c2'
      pure c2'

    saveConnInfo :: Connection -> ConnInfo -> CM (Connection, Maybe GroupInfo)
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage activeConn connInfo
      conn' <- updatePeerChatVRange activeConn chatVRange
      case chatMsgEvent of
        XInfo p -> do
          ct <- withStore $ \db -> createDirectContact db vr user conn' p
          toView $ CEvtContactConnecting user ct
          pure (conn', Nothing)
        XGrpLinkInv glInv -> do
          (gInfo, host) <- withStore $ \db -> createGroupInvitedViaLink db vr user conn' glInv
          toView $ CEvtGroupLinkConnecting user gInfo host
          pure (conn', Just gInfo)
        XGrpLinkReject glRjct@GroupLinkRejection {rejectionReason} -> do
          (gInfo, host) <- withStore $ \db -> createGroupRejectedViaLink db vr user conn' glRjct
          toView $ CEvtGroupLinkConnecting user gInfo host
          toViewTE $ TEGroupLinkRejected user gInfo rejectionReason
          pure (conn', Just gInfo)
        -- TODO show/log error, other events in SMP confirmation
        _ -> pure (conn', Nothing)

    xGrpMemNew :: GroupInfo -> GroupMember -> MemberInfo -> Maybe MsgScope -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpMemNew gInfo m memInfo@(MemberInfo memId memRole _ _) msgScope_ msg brokerTs = do
      checkHostRole m memRole
      if sameMemberId memId (membership gInfo)
        then pure Nothing
        else do
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Right unknownMember@GroupMember {memberStatus = GSMemUnknown} -> do
              (updatedMember, gInfo') <- withStore $ \db -> do
                updatedMember <- updateUnknownMemberAnnounced db vr user m unknownMember memInfo initialStatus
                gInfo' <-
                  if memberPending updatedMember
                    then liftIO $ increaseGroupMembersRequireAttention db user gInfo
                    else pure gInfo
                pure (updatedMember, gInfo')
              toView $ CEvtUnknownMemberAnnounced user gInfo' m unknownMember updatedMember
              memberAnnouncedToView updatedMember gInfo'
              pure $ deliveryJobScope updatedMember
            Right _ -> messageError "x.grp.mem.new error: member already exists" $> Nothing
            Left _ -> do
              (newMember, gInfo') <- withStore $ \db -> do
                newMember <- createNewGroupMember db user gInfo m memInfo GCPostMember initialStatus
                gInfo' <-
                  if memberPending newMember
                    then liftIO $ increaseGroupMembersRequireAttention db user gInfo
                    else pure gInfo
                pure (newMember, gInfo')
              memberAnnouncedToView newMember gInfo'
              pure $ deliveryJobScope newMember
      where
        initialStatus = case msgScope_ of
          Just (MSMember _) -> GSMemPendingReview
          _ -> GSMemAnnounced
        deliveryJobScope GroupMember {groupMemberId, memberStatus}
          | memberStatus == GSMemPendingApproval = Nothing
          | memberStatus == GSMemPendingReview = Just $ DJSMemberSupport groupMemberId
          | otherwise = Just DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}
        memberAnnouncedToView announcedMember@GroupMember {groupMemberId, memberProfile} gInfo' = do
          (announcedMember', scopeInfo) <- getMemNewChatScope announcedMember
          let event = RGEMemberAdded groupMemberId (fromLocalProfile memberProfile)
          (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo' scopeInfo m) msg brokerTs (CIRcvGroupEvent event)
          groupMsgToView cInfo ci
          case scopeInfo of
            Just (GCSIMemberSupport _) -> do
              createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m) (CIRcvGroupEvent RGENewMemberPendingReview) (Just brokerTs)
            _ -> pure ()
          toView $ CEvtJoinedGroupMemberConnecting user gInfo' m announcedMember'
        getMemNewChatScope announcedMember = case msgScope_ of
          Nothing -> pure (announcedMember, Nothing)
          Just (MSMember _) -> do
            (announcedMember', scopeInfo) <- mkMemberSupportChatInfo announcedMember
            pure (announcedMember', Just scopeInfo)

    xGrpMemIntro :: GroupInfo -> GroupMember -> MemberInfo -> Maybe MemberRestrictions -> CM ()
    xGrpMemIntro gInfo@GroupInfo {chatSettings} m@GroupMember {memberRole, localDisplayName = c} memInfo@(MemberInfo memId _ memChatVRange _) memRestrictions = do
      case memberCategory m of
        GCHostMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Right _ ->
              unless (useRelays' gInfo) $
                messageError "x.grp.mem.intro ignored: member already exists"
            Left _
              | useRelays' gInfo ->
                  void $ withStore $ \db -> createIntroReMember db user gInfo memInfo memRestrictions
              | otherwise -> do
                  when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole c)
                  case memChatVRange of
                    Nothing -> messageError "x.grp.mem.intro: member chat version range incompatible"
                    Just (ChatVersionRange mcvr)
                      | maxVersion mcvr >= groupDirectInvVersion -> do
                          subMode <- chatReadVar subscriptionMode
                          -- [async agent commands] commands should be asynchronous, continuation is to send XGrpMemInv - have to remember one has completed and process on second
                          groupConnIds <- createConn subMode
                          let chatV = maybe (minVersion vr) (\peerVR -> vr `peerConnChatVersion` fromChatVRange peerVR) memChatVRange
                          void $ withStore $ \db -> do
                            reMember <- createIntroReMember db user gInfo memInfo memRestrictions
                            createIntroReMemberConn db user m reMember chatV memInfo groupConnIds subMode
                      | otherwise -> messageError "x.grp.mem.intro: member chat version range incompatible"
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
            Right reMember -> sendGroupMemberMessage gInfo reMember $ XGrpMemFwd (memberInfo gInfo m) introInv
        _ -> messageError "x.grp.mem.inv can be only sent by invitee member"

    xGrpMemFwd :: GroupInfo -> GroupMember -> MemberInfo -> IntroInvitation -> CM ()
    xGrpMemFwd gInfo@GroupInfo {membership, chatSettings} m memInfo@(MemberInfo memId memRole memChatVRange _) IntroInvitation {groupConnReq, directConnReq} = do
      let GroupMember {memberId = membershipMemId} = membership
      checkHostRole m memRole
      toMember <- withStore $ \db -> do
        toMember <-
          getGroupMemberByMemberId db vr user gInfo memId
            -- TODO if the missed messages are correctly sent as soon as there is connection before anything else is sent
            -- the situation when member does not exist is an error
            -- member receiving x.grp.mem.fwd should have also received x.grp.mem.new prior to that.
            -- For now, this branch compensates for the lack of delayed message delivery.
            `catchError` \case
              SEGroupMemberNotFoundByMemberId _ -> createNewGroupMember db user gInfo m memInfo GCPostMember GSMemAnnounced
              e -> throwError e
        -- TODO [knocking] separate pending statuses from GroupMemberStatus?
        -- TODO            add GSMemIntroInvitedPending, GSMemConnectedPending, etc.?
        -- TODO            keep as is? (GSMemIntroInvited has no purpose)
        let newMemberStatus = if memberPending toMember then memberStatus toMember else GSMemIntroInvited
        liftIO $ updateGroupMemberStatus db userId toMember newMemberStatus
        pure toMember
      subMode <- chatReadVar subscriptionMode
      -- [incognito] send membership incognito profile, create direct connection as incognito
      let membershipProfile = redactedMemberProfile allowSimplexLinks $ fromLocalProfile $ memberProfile membership
          allowSimplexLinks = groupFeatureUserAllowed SGFSimplexLinks gInfo
      dm <- encodeConnInfo $ XGrpMemInfo membershipMemId membershipProfile
      -- [async agent commands] no continuation needed, but commands should be asynchronous for stability
      groupConnIds <- joinAgentConnectionAsync user Nothing (chatHasNtfs chatSettings) groupConnReq dm subMode
      directConnIds <- forM directConnReq $ \dcr -> joinAgentConnectionAsync user Nothing True dcr dm subMode
      let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
          mcvr = maybe chatInitialVRange fromChatVRange memChatVRange
          chatV = vr `peerConnChatVersion` mcvr
      withStore' $ \db -> createIntroToMemberContact db user m toMember chatV mcvr groupConnIds directConnIds customUserProfileId subMode

    xGrpMemRole :: GroupInfo -> GroupMember -> MemberId -> GroupMemberRole -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpMemRole gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId memRole msg brokerTs
      | membershipMemId == memId =
          let gInfo' = gInfo {membership = membership {memberRole = memRole}}
           in changeMemberRole gInfo' membership $ RGEUserRole memRole
      | otherwise =
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Right member -> changeMemberRole gInfo member $ RGEMemberRole (groupMemberId' member) (fromLocalProfile $ memberProfile member) memRole
            Left _ -> messageError "x.grp.mem.role with unknown member ID" $> Nothing
      where
        GroupMember {memberId = membershipMemId} = membership
        changeMemberRole gInfo' member@GroupMember {memberRole = fromRole} gEvent
          | senderRole < GRAdmin || senderRole < fromRole =
              messageError "x.grp.mem.role with insufficient member permissions" $> Nothing
          | otherwise = do
              withStore' $ \db -> updateGroupMemberRole db user member memRole
              (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo' m
              (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo'' scopeInfo m') msg brokerTs (CIRcvGroupEvent gEvent)
              groupMsgToView cInfo ci
              toView CEvtMemberRole {user, groupInfo = gInfo'', byMember = m', member = member {memberRole = memRole}, fromRole, toRole = memRole}
              pure $ memberEventDeliveryScope member

    checkHostRole :: GroupMember -> GroupMemberRole -> CM ()
    checkHostRole GroupMember {memberRole, localDisplayName} memRole =
      when (memberRole < GRAdmin || memberRole < memRole) $ throwChatError (CEGroupContactRole localDisplayName)

    xGrpMemRestrict :: GroupInfo -> GroupMember -> MemberId -> MemberRestrictions -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpMemRestrict
      gInfo@GroupInfo {membership = GroupMember {memberId = membershipMemId}}
      m@GroupMember {memberRole = senderRole}
      memId
      MemberRestrictions {restriction}
      msg
      brokerTs
        | membershipMemId == memId = pure Nothing -- ignore - XGrpMemRestrict can be sent to restricted member for efficiency
        | otherwise = do
            (bm, unknown) <- withStore $ \db -> getCreateUnknownGMByMemberId db vr user gInfo memId Nothing
            let GroupMember {groupMemberId = bmId, memberRole, blockedByAdmin, memberProfile = bmp} = bm
            if
              | blockedByAdmin == mrsBlocked restriction -> pure Nothing
              | senderRole < GRModerator || senderRole < memberRole ->
                  messageError "x.grp.mem.restrict with insufficient member permissions" $> Nothing
              | otherwise -> do
                  bm' <- setMemberBlocked bm
                  toggleNtf bm' (not blocked)
                  let ciContent = CIRcvGroupEvent $ RGEMemberBlocked bmId (fromLocalProfile bmp) blocked
                  (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
                  (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo' scopeInfo m') msg brokerTs ciContent
                  when unknown $ toView $ CEvtUnknownMemberBlocked user gInfo m bm'
                  groupMsgToView cInfo ci
                  toView CEvtMemberBlockedForAll {user, groupInfo = gInfo', byMember = m', member = bm', blocked}
                  pure $ memberEventDeliveryScope bm
        where
          setMemberBlocked bm = withStore' $ \db -> updateGroupMemberBlocked db user gInfo restriction bm
          blocked = mrsBlocked restriction

    xGrpMemCon :: GroupInfo -> GroupMember -> MemberId -> CM ()
    xGrpMemCon gInfo sendingMem memId = do
      refMem <- withStore $ \db -> getGroupMemberByMemberId db vr user gInfo memId
      -- Updating vectors in separate transactions to avoid deadlocks.
      withStore $ \db -> setMemberVectorRelationConnected db sendingMem refMem MRSubjectConnected
      withStore $ \db -> setMemberVectorRelationConnected db refMem sendingMem MRReferencedConnected

    xGrpMemDel :: GroupInfo -> GroupMember -> MemberId -> Bool -> ChatMessage 'Json -> RcvMessage -> UTCTime -> Bool -> CM (Maybe DeliveryJobScope)
    xGrpMemDel gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId withMessages chatMsg msg brokerTs forwarded = do
      let GroupMember {memberId = membershipMemId} = membership
      if membershipMemId == memId
        then checkRole membership $ do
          deleteGroupLinkIfExists user gInfo
          -- TODO [relays] possible improvement is to immediately delete rcv queues if isUserGrpFwdRelay
          unless (isUserGrpFwdRelay gInfo) $ deleteGroupConnections user gInfo False
          withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemRemoved
          let membership' = membership {memberStatus = GSMemRemoved}
          when withMessages $ deleteMessages gInfo membership' SMDSnd
          deleteMemberItem gInfo RGEUserDeleted
          toView $ CEvtDeletedMemberUser user gInfo {membership = membership'} m withMessages
          pure $ Just DJSGroup {jobSpec = DJRelayRemoved}
        else
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Left _ -> do
              messageError "x.grp.mem.del with unknown member ID"
              pure $ Just DJSGroup {jobSpec = DJDeliveryJob {includePending = True}}
            Right deletedMember@GroupMember {groupMemberId, memberProfile, memberStatus} ->
              checkRole deletedMember $ do
                -- ? prohibit deleting member if it's the sender - sender should use x.grp.leave
                let shouldForward = isUserGrpFwdRelay gInfo && not forwarded
                if shouldForward
                  then do
                    -- Special case: forward before deleting connection.
                    forwardToMember deletedMember
                    deleteMemberConnection' deletedMember True
                  else deleteMemberConnection deletedMember
                let deliveryScope = memberEventDeliveryScope deletedMember
                gInfo' <- case deliveryScope of
                  -- Keep member record if it's support scope - it will be required for forwarding inside that scope.
                  Just (DJSMemberSupport _) | shouldForward -> updateMemberRecordDeleted user gInfo deletedMember GSMemRemoved
                  -- Undeleted "member connected" chat item will prevent deletion of member record.
                  _ -> deleteOrUpdateMemberRecord user gInfo deletedMember
                let wasDeleted = memberStatus == GSMemRemoved || memberStatus == GSMemLeft
                    deletedMember' = deletedMember {memberStatus = GSMemRemoved}
                when withMessages $ deleteMessages gInfo' deletedMember' SMDRcv
                unless wasDeleted $ deleteMemberItem gInfo' $ RGEMemberDeleted groupMemberId (fromLocalProfile memberProfile)
                toView $ CEvtDeletedMember user gInfo' m deletedMember' withMessages
                pure deliveryScope
      where
        checkRole GroupMember {memberRole} a
          | senderRole < GRAdmin || senderRole < memberRole =
              messageError "x.grp.mem.del with insufficient member permissions" $> Nothing
          | otherwise = a
        deleteMemberItem gi gEvent = do
          (gi', m', scopeInfo) <- mkGroupChatScope gi m
          (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gi' scopeInfo m') msg brokerTs (CIRcvGroupEvent gEvent)
          groupMsgToView cInfo ci
        deleteMessages :: MsgDirectionI d => GroupInfo -> GroupMember -> SMsgDirection d -> CM ()
        deleteMessages gInfo' delMem msgDir
          | groupFeatureMemberAllowed SGFFullDelete m gInfo' = deleteGroupMemberCIs user gInfo' delMem m msgDir
          | otherwise = markGroupMemberCIsDeleted user gInfo' delMem m
        forwardToMember :: GroupMember -> CM ()
        forwardToMember member = do
          let GroupMember {memberId} = m
              memberName = Just $ memberShortenedName m
              event = XGrpMsgForward (Just memberId) memberName chatMsg brokerTs
          sendGroupMemberMessage gInfo member event

    isUserGrpFwdRelay :: GroupInfo -> Bool
    isUserGrpFwdRelay gInfo@GroupInfo {membership}
      | useRelays' gInfo = isRelay membership
      | otherwise = memberRole' membership >= GRAdmin

    isMemberGrpFwdRelay :: GroupInfo -> GroupMember -> Bool
    isMemberGrpFwdRelay gInfo m
      | useRelays' gInfo = isRelay m
      | otherwise = memberRole' m >= GRAdmin

    xGrpLeave :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpLeave gInfo m msg brokerTs = do
      deleteMemberConnection m
      -- member record is not deleted to allow creation of "member left" chat item
      gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
      (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo' m
      (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo'' scopeInfo m') msg brokerTs (CIRcvGroupEvent RGEMemberLeft)
      groupMsgToView cInfo ci
      toView $ CEvtLeftMember user gInfo'' m' {memberStatus = GSMemLeft}
      pure $ memberEventDeliveryScope m

    xGrpDel :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM ()
    xGrpDel gInfo@GroupInfo {membership} m@GroupMember {memberRole} msg brokerTs = do
      when (memberRole /= GROwner) $ throwChatError $ CEGroupUserRole gInfo GROwner
      withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemGroupDeleted
      -- TODO [relays] possible improvement is to immediately delete rcv queues if isUserGrpFwdRelay
      unless (isUserGrpFwdRelay gInfo) $ deleteGroupConnections user gInfo False
      (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo m
      (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo'' scopeInfo m') msg brokerTs (CIRcvGroupEvent RGEGroupDeleted)
      groupMsgToView cInfo ci
      toView $ CEvtGroupDeleted user gInfo'' {membership = membership {memberStatus = GSMemGroupDeleted}} m'

    xGrpInfo :: GroupInfo -> GroupMember -> GroupProfile -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpInfo g@GroupInfo {groupProfile = p, businessChat} m@GroupMember {memberRole} p' msg brokerTs
      | memberRole < GROwner = messageError "x.grp.info with insufficient member permissions" $> Nothing
      | otherwise = do
          case businessChat of
            Nothing -> unless (p == p') $ do
              g' <- withStore $ \db -> updateGroupProfile db user g p'
              (g'', m', scopeInfo) <- mkGroupChatScope g' m
              toView $ CEvtGroupUpdated user g g'' (Just m')
              let cd = CDGroupRcv g'' scopeInfo m'
              unless (sameGroupProfileInfo p p') $ do
                (ci, cInfo) <- saveRcvChatItemNoParse user cd msg brokerTs (CIRcvGroupEvent $ RGEGroupUpdated p')
                groupMsgToView cInfo ci
              createGroupFeatureChangedItems user cd CIRcvGroupFeature g g''
              void $ forkIO $ void $ setGroupLinkData' NRMBackground user g''
            Just _ -> updateGroupPrefs_ g m $ fromMaybe defaultBusinessGroupPrefs $ groupPreferences p'
          pure $ Just DJSGroup {jobSpec = DJDeliveryJob {includePending = True}}

    xGrpPrefs :: GroupInfo -> GroupMember -> GroupPreferences -> CM (Maybe DeliveryJobScope)
    xGrpPrefs g m@GroupMember {memberRole} ps'
      | memberRole < GROwner = messageError "x.grp.prefs with insufficient member permissions" $> Nothing
      | otherwise = updateGroupPrefs_ g m ps' $> Just DJSGroup {jobSpec = DJDeliveryJob {includePending = True}}

    updateGroupPrefs_ :: GroupInfo -> GroupMember -> GroupPreferences -> CM ()
    updateGroupPrefs_ g@GroupInfo {groupProfile = p} m ps' =
      unless (groupPreferences p == Just ps') $ do
        g' <- withStore' $ \db -> updateGroupPreferences db user g ps'
        toView $ CEvtGroupUpdated user g g' (Just m)
        (g'', m', scopeInfo) <- mkGroupChatScope g' m
        let cd = CDGroupRcv g'' scopeInfo m'
        createGroupFeatureChangedItems user cd CIRcvGroupFeature g g''

    xGrpDirectInv :: GroupInfo -> GroupMember -> Connection -> ConnReqInvitation -> Maybe MsgContent -> RcvMessage -> UTCTime -> CM ()
    xGrpDirectInv g@GroupInfo {groupId, groupProfile = gp} m mConn@Connection {connId = mConnId} connReq mContent_ msg brokerTs
      | not (groupFeatureMemberAllowed SGFDirectMessages m g) = messageError "x.grp.direct.inv: direct messages not allowed"
      | memberBlocked m = messageWarning "x.grp.direct.inv: member is blocked (ignoring)"
      | otherwise = do
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
        groupDirectInv =
          GroupDirectInvitation
            { groupDirectInvLink = connReq,
              fromGroupId_ = Just groupId,
              fromGroupMemberId_ = Just (groupMemberId' m),
              fromGroupMemberConnId_ = Just mConnId,
              groupDirectInvStartedConnection = isTrue $ autoAcceptMemberContacts user
            }
        joinExistingContact subMode mCt@Contact {contactId = mContactId}
          | isTrue (autoAcceptMemberContacts user) = do
              (cmdId, acId) <- joinConn subMode
              mCt' <- withStore $ \db -> do
                updateMemberContactInvited db user mCt groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId (Just cmdId) g mConn ConnJoined mContactId subMode
                getContact db vr user mContactId
              securityCodeChanged mCt'
              createItems mCt' m
          | otherwise = do
              acId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True connReq PQSupportOff
              mCt' <- withStore $ \db -> do
                updateMemberContactInvited db user mCt groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId Nothing g mConn ConnPrepared mContactId subMode
                getContact db vr user mContactId
              securityCodeChanged mCt'
              createInternalChatItem user (CDDirectRcv mCt') (CIRcvDirectEvent $ RDEGroupInvLinkReceived gp) Nothing
              createItems mCt' m
        createNewContact subMode
          | isTrue (autoAcceptMemberContacts user) = do
              (cmdId, acId) <- joinConn subMode
              -- [incognito] reuse membership incognito profile
              (mCt, m') <- withStore $ \db -> do
                (mContactId, m') <- liftIO $ createMemberContactInvited db user g m groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId (Just cmdId) g mConn ConnJoined mContactId subMode
                mCt <- getContact db vr user mContactId
                pure (mCt, m')
              createInternalChatItem user (CDDirectSnd mCt) CIChatBanner (Just epochStart)
              createItems mCt m'
          | otherwise = do
              acId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True connReq PQSupportOff
              (mCt, m') <- withStore $ \db -> do
                (mContactId, m') <- liftIO $ createMemberContactInvited db user g m groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId Nothing g mConn ConnPrepared mContactId subMode
                mCt <- getContact db vr user mContactId
                pure (mCt, m')
              createInternalChatItem user (CDDirectSnd mCt) CIChatBanner (Just epochStart)
              createInternalChatItem user (CDDirectRcv mCt) (CIRcvDirectEvent $ RDEGroupInvLinkReceived gp) Nothing
              createItems mCt m'
        joinConn subMode = do
          -- [incognito] send membership incognito profile
          let p = userProfileDirect user (fromLocalProfile <$> incognitoMembershipProfile g) Nothing True
          -- TODO PQ should negotitate contact connection with PQSupportOn? (use encodeConnInfoPQ)
          dm <- encodeConnInfo $ XInfo p
          joinAgentConnectionAsync user Nothing True connReq dm subMode
        createItems mCt' m' = do
          (g', m'', scopeInfo) <- mkGroupChatScope g m'
          createInternalChatItem user (CDGroupRcv g' scopeInfo m'') (CIRcvGroupEvent RGEMemberCreatedContact) Nothing
          toView $ CEvtNewMemberContactReceivedInv user mCt' g' m''
          forM_ mContent_ $ \mc -> do
            (ci, cInfo) <- saveRcvChatItem user (CDDirectRcv mCt') msg brokerTs (CIRcvMsgContent mc, msgContentTexts mc)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]

    securityCodeChanged :: Contact -> CM ()
    securityCodeChanged ct = do
      toViewTE $ TEContactVerificationReset user ct
      createInternalChatItem user (CDDirectRcv ct) (CIRcvConnEvent RCEVerificationCodeReset) Nothing

    xGrpMsgForward :: GroupInfo -> GroupMember -> Maybe MemberId -> Maybe ContactName -> ChatMessage 'Json -> UTCTime -> UTCTime -> CM ()
    xGrpMsgForward gInfo m@GroupMember {localDisplayName} memberId_ memberName_ chatMsg msgTs brokerTs = do
      unless (isMemberGrpFwdRelay gInfo m) $ throwChatError (CEGroupContactRole localDisplayName)
      case memberId_ of
        Just memberId -> do
          (author, unknown) <- withStore $ \db -> getCreateUnknownGMByMemberId db vr user gInfo memberId memberName_
          when unknown $ toView $ CEvtUnknownMemberCreated user gInfo m author
          processForwardedMsg author
        Nothing -> processForwardedChannelMsg
      where
        -- ! see isForwardedGroupMsg: forwarded group events should include msgId to be deduplicated
        processForwardedMsg :: GroupMember -> CM ()
        processForwardedMsg author = do
          let body = chatMsgToBody chatMsg
          rcvMsg_ <- saveGroupFwdRcvMsg user gInfo m (Just author) body chatMsg brokerTs
          forM_ rcvMsg_ $ \rcvMsg@RcvMessage {chatMsgEvent = ACME _ event} -> case event of
            XMsgNew mc -> void $ memberCanSend author scope $ (const Nothing) <$> newGroupContentMessage gInfo author mc rcvMsg msgTs True
              where
                ExtMsgContent {scope} = mcExtMsgContent mc
            -- file description is always allowed, to allow sending files to support scope
            XMsgFileDescr sharedMsgId fileDescr -> void $ groupMessageFileDescription gInfo (Just $ memberId' author) sharedMsgId fileDescr
            XMsgUpdate sharedMsgId mContent mentions ttl live msgScope -> void $ memberCanSend author msgScope $ (const Nothing) <$> groupMessageUpdate gInfo author sharedMsgId mContent mentions msgScope rcvMsg msgTs ttl live
            XMsgDel sharedMsgId memId scope_ -> void $ groupMessageDelete gInfo author sharedMsgId memId scope_ rcvMsg msgTs
            XMsgReact sharedMsgId (Just memId) scope_ reaction add -> void $ groupMsgReaction gInfo author sharedMsgId (Just memId) scope_ reaction add rcvMsg msgTs
            XMsgReact sharedMsgId Nothing _scope_ reaction add -> fwdChannelReaction gInfo (Just author) sharedMsgId reaction add rcvMsg msgTs
            XFileCancel sharedMsgId -> void $ xFileCancelGroup gInfo (Just $ memberId' author) sharedMsgId
            XInfo p -> void $ xInfoMember gInfo author p msgTs
            XGrpMemNew memInfo msgScope -> void $ xGrpMemNew gInfo author memInfo msgScope rcvMsg msgTs
            XGrpMemRole memId memRole -> void $ xGrpMemRole gInfo author memId memRole rcvMsg msgTs
            XGrpMemDel memId withMessages -> void $ xGrpMemDel gInfo author memId withMessages chatMsg rcvMsg msgTs True
            XGrpLeave -> void $ xGrpLeave gInfo author rcvMsg msgTs
            XGrpDel -> void $ xGrpDel gInfo author rcvMsg msgTs
            XGrpInfo p' -> void $ xGrpInfo gInfo author p' rcvMsg msgTs
            XGrpPrefs ps' -> void $ xGrpPrefs gInfo author ps'
            _ -> messageError $ "x.grp.msg.forward: unsupported forwarded event " <> T.pack (show $ toCMEventTag event)
        processForwardedChannelMsg :: CM ()
        processForwardedChannelMsg = do
          let body = chatMsgToBody chatMsg
          rcvMsg_ <- saveGroupFwdRcvMsg user gInfo m Nothing body chatMsg brokerTs
          forM_ rcvMsg_ $ \rcvMsg@RcvMessage {chatMsgEvent = ACME _ event} -> case event of
            XMsgNew mc -> void $ newChannelContentMessage_ gInfo mc rcvMsg msgTs True
            XMsgFileDescr sharedMsgId fileDescr -> void $ channelMessageFileDescription gInfo sharedMsgId fileDescr
            XMsgUpdate sharedMsgId mContent mentions ttl live _msgScope -> void $ channelMessageUpdate_ gInfo sharedMsgId mContent mentions rcvMsg msgTs ttl live True
            XMsgDel sharedMsgId _memId _scope_ -> void $ channelMessageDelete gInfo sharedMsgId rcvMsg msgTs
            XMsgReact sharedMsgId _ _scope_ reaction add -> fwdChannelReaction gInfo Nothing sharedMsgId reaction add rcvMsg msgTs
            XFileCancel sharedMsgId -> void $ xFileCancelChannel gInfo sharedMsgId
            _ -> messageError $ "x.grp.msg.forward: unsupported channel event " <> T.pack (show $ toCMEventTag event)

    fwdChannelReaction :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM ()
    fwdChannelReaction g reactor_ sharedMsgId reaction add RcvMessage {msgId} brokerTs
      | groupFeatureAllowed SGFReactions g = do
          rs <- withStore' $ \db -> getGroupReactions db g reactor Nothing sharedMsgId False
          when (reactionAllowed add reaction rs) $
            updateChatItemReaction `catchCINotFound` \_ ->
              withStore' $ \db -> setGroupReaction db g reactor Nothing sharedMsgId False reaction add msgId brokerTs
      | otherwise = pure ()
      where
        GroupInfo {membership} = g
        reactor = fromMaybe membership reactor_
        ciReaction = maybe CIChannelRcv CIGroupRcv reactor_
        updateChatItemReaction = do
          (CChatItem md ci, scopeInfo) <- withStore $ \db -> do
            cci <- getGroupCIBySharedMsgId' db user g sharedMsgId
            scopeInfo <- getGroupChatScopeInfoForItem db vr user g (cChatItemId cci)
            pure (cci, scopeInfo)
          when (ciReactionAllowed ci) $ do
            reactions <- withStore' $ \db -> do
              setGroupReaction db g reactor Nothing sharedMsgId False reaction add msgId brokerTs
              getGroupCIReactions db g Nothing sharedMsgId
            let ci' = CChatItem md ci {reactions}
                r = ACIReaction SCTGroup SMDRcv (GroupChat g scopeInfo) $ CIReaction ciReaction ci' brokerTs reaction
            toView $ CEvtChatItemReaction user add r

    directMsgReceived :: Contact -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> CM ()
    directMsgReceived ct conn@Connection {connId} msgMeta msgRcpts = do
      checkIntegrityCreateItem (CDDirectRcv ct) msgMeta `catchAllErrors` \_ -> pure ()
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore' $ \db -> updateSndMsgDeliveryStatus db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateDirectItemStatus ct conn agentMsgId $ CISSndRcvd msgRcptStatus SSPComplete

    groupMsgReceived :: GroupInfo -> GroupMember -> Connection -> MsgMeta -> NonEmpty MsgReceipt -> CM ()
    groupMsgReceived gInfo m conn@Connection {connId} msgMeta msgRcpts = do
      (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
      checkIntegrityCreateItem (CDGroupRcv gInfo' scopeInfo m') msgMeta `catchAllErrors` \_ -> pure ()
      forM_ msgRcpts $ \MsgReceipt {agentMsgId, msgRcptStatus} -> do
        withStore' $ \db -> updateSndMsgDeliveryStatus db connId agentMsgId $ MDSSndRcvd msgRcptStatus
        updateGroupItemsStatus gInfo' m' conn agentMsgId (GSSRcvd msgRcptStatus) Nothing

    -- Searches chat items for many agent message IDs and updates their status
    updateDirectItemsStatusMsgs :: Contact -> Connection -> [AgentMsgId] -> CIStatus 'MDSnd -> CM ()
    updateDirectItemsStatusMsgs ct conn msgIds newStatus = do
      cis <- withStore' $ \db -> forM msgIds $ \msgId -> runExceptT $ updateDirectItemsStatus' db ct conn msgId newStatus
      let acis = map ctItem $ concat $ rights cis
      unless (null acis) $ toView $ CEvtChatItemsStatusesUpdated user acis
      where
        ctItem = AChatItem SCTDirect SMDSnd (DirectChat ct)

    updateDirectItemStatus :: Contact -> Connection -> AgentMsgId -> CIStatus 'MDSnd -> CM ()
    updateDirectItemStatus ct conn msgId newStatus = do
      cis <- withStore $ \db -> updateDirectItemsStatus' db ct conn msgId newStatus
      let acis = map ctItem cis
      unless (null acis) $ toView $ CEvtChatItemsStatusesUpdated user acis
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
      acis <- withStore $ \db -> do
        items <- liftIO $ getGroupChatItemsByAgentMsgId db user groupId connId msgId
        cis <- catMaybes <$> mapM (updateItem db) items
        -- SENT and RCVD events are received for messages that may be batched in single scope,
        -- so we can look up scope of first item
        scopeInfo <- case cis of
          (ci : _) -> getGroupChatScopeInfoForItem db vr user gInfo (chatItemId' ci)
          _ -> pure Nothing
        pure $ map (gItem scopeInfo) cis
      unless (null acis) $ toView $ CEvtChatItemsStatusesUpdated user acis
      where
        gItem scopeInfo ci = AChatItem SCTGroup SMDSnd (GroupChat gInfo scopeInfo) ci
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

deleteGroupConnections :: User -> GroupInfo -> Bool -> CM ()
deleteGroupConnections user gInfo waitDelivery = do
  vr <- chatVersionRange
  -- member records are not deleted to keep history
  members <- getMembers vr
  deleteMembersConnections' user members waitDelivery
  where
    getMembers vr
      | useRelays' gInfo = withStore' $ \db -> getGroupRelayMembers db vr user gInfo
      | otherwise = withStore' $ \db -> getGroupMembers db vr user gInfo

startDeliveryTaskWorkers :: CM ()
startDeliveryTaskWorkers = do
  workerScopes <- withStore' $ \db -> getPendingDeliveryTaskScopes db
  lift $ forM_ workerScopes resumeDeliveryTaskWork

resumeDeliveryTaskWork :: DeliveryWorkerKey -> CM' ()
resumeDeliveryTaskWork = void . getDeliveryTaskWorker False

getDeliveryTaskWorker :: Bool -> DeliveryWorkerKey -> CM' Worker
getDeliveryTaskWorker hasWork deliveryKey = do
  ws <- asks deliveryTaskWorkers
  a <- asks smpAgent
  getAgentWorker "delivery_task" hasWork a deliveryKey ws $
    runDeliveryTaskWorker a deliveryKey

runDeliveryTaskWorker :: AgentClient -> DeliveryWorkerKey -> Worker -> CM ()
runDeliveryTaskWorker a deliveryKey Worker {doWork} = do
  delay <- asks $ deliveryWorkerDelay . config
  vr <- chatVersionRange
  -- TODO [relays] in future may be required to read groupInfo and user on each iteration for up to date state
  -- TODO   - same for delivery jobs (runDeliveryJobWorker)
  gInfo <- withStore $ \db -> do
    user <- getUserByGroupId db groupId
    getGroupInfo db vr user groupId
  forever $ do
    unless (delay == 0) $ liftIO $ threadDelay' delay
    lift $ waitForWork doWork
    runDeliveryTaskOperation vr gInfo
  where
    (groupId, workerScope) = deliveryKey
    runDeliveryTaskOperation :: VersionRangeChat -> GroupInfo -> CM ()
    runDeliveryTaskOperation vr gInfo = do
      withWork_ a doWork (withStore' $ \db -> getNextDeliveryTask db deliveryKey) $ \task ->
        processDeliveryTask task
          `catchAllErrors` \e -> do
            withStore' $ \db -> setDeliveryTaskErrStatus db (deliveryTaskId task) (tshow e)
            eToView e
      where
        processDeliveryTask :: MessageDeliveryTask -> CM ()
        processDeliveryTask task@MessageDeliveryTask {jobScope} =
          case jobScopeImpliedSpec jobScope of
            DJDeliveryJob _includePending ->
              withWorkItems a doWork (withStore' $ \db -> getNextDeliveryTasks db gInfo task) $ \nextTasks -> do
                let (body, taskIds, largeTaskIds) = batchDeliveryTasks1 vr maxEncodedMsgLength nextTasks
                withStore' $ \db -> do
                  createMsgDeliveryJob db gInfo jobScope (singleSenderGMId_ nextTasks) body
                  forM_ taskIds $ \taskId -> updateDeliveryTaskStatus db taskId DTSProcessed
                  forM_ largeTaskIds $ \taskId -> setDeliveryTaskErrStatus db taskId "large"
                lift . void $ getDeliveryJobWorker True deliveryKey
              where
                singleSenderGMId_ :: NonEmpty MessageDeliveryTask -> Maybe GroupMemberId
                singleSenderGMId_ (MessageDeliveryTask {senderGMId = senderGMId'} :| ts)
                  | all (\MessageDeliveryTask {senderGMId} -> senderGMId == senderGMId') ts = Just senderGMId'
                  | otherwise = Nothing
            DJRelayRemoved
              | workerScope /= DWSGroup ->
                  throwChatError $ CEInternalError "delivery task worker: relay removed task in wrong worker scope"
              | otherwise -> do
                  let MessageDeliveryTask {senderGMId, fwdSender, brokerTs, chatMessage} = task
                      (memberId_, memberName_) = case fwdSender of
                        FwdMember mid mname -> (Just mid, Just mname)
                        FwdChannel -> (Nothing, Nothing)
                      fwdEvt = XGrpMsgForward memberId_ memberName_ chatMessage brokerTs
                      cm = ChatMessage {chatVRange = vr, msgId = Nothing, chatMsgEvent = fwdEvt}
                      body = chatMsgToBody cm
                  withStore' $ \db -> do
                    createMsgDeliveryJob db gInfo jobScope (Just senderGMId) body
                    updateDeliveryTaskStatus db (deliveryTaskId task) DTSProcessed
                  lift . void $ getDeliveryJobWorker True deliveryKey

startDeliveryJobWorkers :: CM ()
startDeliveryJobWorkers = do
  workerScopes <- withStore' $ \db -> getPendingDeliveryJobScopes db
  lift $ forM_ workerScopes resumeDeliveryJobWork

resumeDeliveryJobWork :: DeliveryWorkerKey -> CM' ()
resumeDeliveryJobWork = void . getDeliveryJobWorker False

getDeliveryJobWorker :: Bool -> DeliveryWorkerKey -> CM' Worker
getDeliveryJobWorker hasWork deliveryKey = do
  ws <- asks deliveryJobWorkers
  a <- asks smpAgent
  getAgentWorker "delivery_job" hasWork a deliveryKey ws $
    runDeliveryJobWorker a deliveryKey

runDeliveryJobWorker :: AgentClient -> DeliveryWorkerKey -> Worker -> CM ()
runDeliveryJobWorker a deliveryKey Worker {doWork} = do
  delay <- asks $ deliveryWorkerDelay . config
  vr <- chatVersionRange
  (user, gInfo) <- withStore $ \db -> do
    user <- getUserByGroupId db groupId
    gInfo <- getGroupInfo db vr user groupId
    pure (user, gInfo)
  forever $ do
    unless (delay == 0) $ liftIO $ threadDelay' delay
    lift $ waitForWork doWork
    runDeliveryJobOperation vr user gInfo
  where
    (groupId, workerScope) = deliveryKey
    runDeliveryJobOperation :: VersionRangeChat -> User -> GroupInfo -> CM ()
    runDeliveryJobOperation vr user gInfo = do
      withWork_ a doWork (withStore' $ \db -> getNextDeliveryJob db deliveryKey) $ \job ->
        processDeliveryJob job
          `catchAllErrors` \e -> do
            withStore' $ \db -> setDeliveryJobErrStatus db (deliveryJobId job) (tshow e)
            eToView e
      where
        processDeliveryJob :: MessageDeliveryJob -> CM ()
        processDeliveryJob job =
          case jobScopeImpliedSpec jobScope of
            DJDeliveryJob _includePending -> do
              sendBodyToMembers
              withStore' $ \db -> updateDeliveryJobStatus db jobId DJSComplete
            DJRelayRemoved
              | workerScope /= DWSGroup ->
                  throwChatError $ CEInternalError "delivery job worker: relay removed job in wrong worker scope"
              | otherwise -> do
                  sendBodyToMembers
                  deleteGroupConnections user gInfo True
                  withStore' $ \db -> updateDeliveryJobStatus db jobId DJSComplete
          where
            MessageDeliveryJob {jobId, jobScope, singleSenderGMId_, body, cursorGMId_ = startingCursor} = job
            sendBodyToMembers :: CM ()
            sendBodyToMembers
              -- channel
              | useRelays' gInfo = case jobScope of
                  -- there's no member review in channels, so job spec includePending is ignored
                  DJSGroup {} -> do
                    bucketSize <- asks $ deliveryBucketSize . config
                    sendLoop bucketSize startingCursor
                    where
                      sendLoop :: Int -> Maybe GroupMemberId -> CM ()
                      sendLoop bucketSize cursorGMId_ = do
                        mems <- withStore' $ \db -> getGroupMembersByCursor db vr user gInfo cursorGMId_ singleSenderGMId_ bucketSize
                        unless (null mems) $ do
                          deliver body mems
                          let cursorGMId' = groupMemberId' $ last mems
                          withStore' $ \db -> updateDeliveryJobCursor db jobId cursorGMId'
                          unless (length mems < bucketSize) $ sendLoop bucketSize (Just cursorGMId')
                  DJSMemberSupport scopeGMId -> do
                    -- for member support scope we just load all recipients in one go, without cursor
                    modMs <- withStore' $ \db -> getGroupModerators db vr user gInfo
                    let moderatorFilter m =
                          memberCurrent m
                            && maxVersion (memberChatVRange m) >= groupKnockingVersion
                            && Just (groupMemberId' m) /= singleSenderGMId_
                        modMs' = filter moderatorFilter modMs
                    mems <-
                      if Just scopeGMId == singleSenderGMId_
                        then pure modMs'
                        else do
                          scopeMem <- withStore $ \db -> getGroupMemberById db vr user scopeGMId
                          pure $ scopeMem : modMs'
                    unless (null mems) $ deliver body mems
              -- fully connected group
              | otherwise = case singleSenderGMId_ of
                  Nothing -> throwChatError $ CEInternalError "delivery job worker: singleSenderGMId is required when not using relays"
                  Just singleSenderGMId -> do
                    sender <- withStore $ \db -> getGroupMemberById db vr user singleSenderGMId
                    ms <- buildMemberList sender
                    unless (null ms) $ deliver body ms
                    where
                      buildMemberList sender = do
                        vec <- withStore (`getMemberRelationsVector` sender)
                        -- this excludes the sender
                        let introducedMemsIdxs = getRelationsIndexes MRIntroduced vec
                        case jobScope of
                          DJSGroup {jobSpec} -> do
                            ms <- withStore' $ \db -> getGroupMembersByIndexes db vr user gInfo introducedMemsIdxs
                            pure $ filter shouldForwardTo ms
                            where
                              shouldForwardTo m
                                | jobSpecImpliedPending jobSpec = memberCurrentOrPending m
                                | otherwise = memberCurrent m
                          DJSMemberSupport scopeGMId -> do
                            ms <- withStore' $ \db -> getSupportScopeMembersByIndexes db vr user gInfo scopeGMId introducedMemsIdxs
                            pure $ filter shouldForwardTo ms
                            where
                              shouldForwardTo m = groupMemberId' m == scopeGMId || currentModerator m
                              currentModerator m@GroupMember {memberRole} =
                                memberRole >= GRModerator
                                  && memberCurrent m
                                  && maxVersion (memberChatVRange m) >= groupKnockingVersion
              where
                deliver :: ByteString -> [GroupMember] -> CM ()
                deliver msgBody mems =
                  let mConns = mapMaybe (fmap snd . readyMemberConn) mems
                      msgReqs = foldMemConns mConns
                   in void $ withAgent (`sendMessages` msgReqs)
                  where
                    foldMemConns :: [Connection] -> [MsgReq]
                    foldMemConns mConns = snd $ foldr' addReq (lastMemIdx_, []) mConns
                      where
                        lastMemIdx_ = let len = length mConns in if len > 1 then Just len else Nothing
                        addReq :: Connection -> (Maybe Int, [MsgReq]) -> (Maybe Int, [MsgReq])
                        addReq conn (memIdx_, reqs) =
                          (subtract 1 <$> memIdx_, req : reqs)
                          where
                            req = (aConnId conn, PQEncOff, MsgFlags False, vrValue_)
                            vrValue_ = case memIdx_ of
                              Nothing -> VRValue Nothing msgBody -- sending to one member, do not reference body
                              Just 1 -> VRValue (Just 1) msgBody
                              Just _ -> VRRef 1

-- Single worker processes all relay requests (XGrpRelayInv).
-- We use map with a single key 1 to fit into existing worker management framework.
relayRequestWorkerKey :: Int
relayRequestWorkerKey = 1

startRelayRequestWorker :: CM ()
startRelayRequestWorker = do
  hasPending <- withStore' hasPendingRelayRequests
  when hasPending $ lift resumeRelayRequestWork

resumeRelayRequestWork :: CM' ()
resumeRelayRequestWork = void $ getRelayRequestWorker False

getRelayRequestWorker :: Bool -> CM' Worker
getRelayRequestWorker hasWork = do
  ws <- asks relayRequestWorkers
  a <- asks smpAgent
  getAgentWorker "relay_request" hasWork a relayRequestWorkerKey ws $
    runRelayRequestWorker a

runRelayRequestWorker :: AgentClient -> Worker -> CM ()
runRelayRequestWorker a Worker {doWork} = do
  vr <- chatVersionRange
  (user, uclId) <- withStore $ \db -> do
    user <- getRelayUser db
    UserContactLink {userContactLinkId} <- getUserAddress db user
    pure (user, userContactLinkId)
  forever $ do
    lift $ waitForWork doWork
    runRelayRequestOperation vr user uclId
  where
    runRelayRequestOperation :: VersionRangeChat -> User -> Int64 -> CM ()
    runRelayRequestOperation vr user uclId =
      withWork_ a doWork (withStore' getNextPendingRelayRequest) $
        \(groupId, rrd) -> do
          ri <- asks $ reconnectInterval . agentConfig . config
          withRetryInterval ri $ \_ loop -> do
            liftIO $ waitWhileSuspended a
            liftIO $ waitForUserNetwork a
            processRelayRequest groupId rrd `catchAllErrors` retryTmpError loop groupId
      where
        retryTmpError :: CM () -> GroupId -> ChatError -> CM ()
        retryTmpError loop groupId = \case
          ChatErrorAgent {agentError} | temporaryOrHostError agentError -> loop
          e -> do
            withStore' $ \db -> setRelayRequestErr db groupId (tshow e)
            eToView e
        processRelayRequest :: GroupId -> RelayRequestData -> CM ()
        processRelayRequest groupId rrd = do
          gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
          -- Check if relay link already exists (recovery case)
          withStore' (\db -> runExceptT $ getGroupLink db user gInfo) >>= \case
            Right GroupLink {connLinkContact = CCLink _ sLnk_} ->
              case sLnk_ of
                Just sLnk -> acceptOwnerConnection rrd gInfo sLnk
                Nothing -> throwChatError $ CEException "processRelayRequest: relay link doesn't have short link"
            Left _ -> do
              (gInfo', sLnk) <- getLinkDataCreateRelayLink rrd gInfo
              acceptOwnerConnection rrd gInfo' sLnk
          where
            getLinkDataCreateRelayLink :: RelayRequestData -> GroupInfo -> CM (GroupInfo, ShortLinkContact)
            getLinkDataCreateRelayLink RelayRequestData {reqGroupLink} gInfo = do
              (_fd, cData) <- getShortLinkConnReq NRMBackground user reqGroupLink
              liftIO (decodeLinkUserData cData) >>= \case
                Nothing -> throwChatError $ CEException "getLinkDataCreateRelayLink: no group link data"
                Just (GroupShortLinkData gp) -> do
                  validateGroupProfile gp
                  gInfo' <- withStore $ \db -> updateGroupProfile db user gInfo gp
                  sLnk <- createRelayLink gInfo'
                  pure (gInfo', sLnk)
              where
                validateGroupProfile :: GroupProfile -> CM ()
                validateGroupProfile _groupProfile = do
                  -- TODO [relays] relay: validate group profile, verify owner's signature
                  pure ()
                createRelayLink :: GroupInfo -> CM ShortLinkContact
                createRelayLink gi@GroupInfo {groupProfile} = do
                  -- TODO [relays] relay: set relay link data
                  -- TODO   - link data: relay key for group, relay identity (profile, certificate, relay identity key)
                  -- TODO   - TBC link's member role - owner to communicate in invitation?
                  groupLinkId <- GroupLinkId <$> drgRandomBytes 16
                  subMode <- chatReadVar subscriptionMode
                  let userData = encodeShortLinkData $ GroupShortLinkData groupProfile
                      userLinkData = UserContactLinkData UserContactData {direct = True, owners = [], relays = [], userData}
                      crClientData = encodeJSON $ CRDataGroup groupLinkId
                  (connId, (ccLink, _serviceId)) <- withAgent $ \a' -> createConnection a' NRMBackground (aUserId user) True True SCMContact (Just userLinkData) (Just crClientData) CR.IKPQOff subMode
                  ccLink' <- createdGroupLink <$> shortenCreatedLink ccLink
                  sLnk <- case toShortLinkContact ccLink' of
                    Just sl -> pure sl
                    Nothing -> throwChatError $ CEException "failed to create relay link: no short link"
                  gVar <- asks random
                  void $ withFastStore $ \db -> createGroupLink db gVar user gi connId ccLink' groupLinkId GRMember subMode
                  pure sLnk
            acceptOwnerConnection :: RelayRequestData -> GroupInfo -> ShortLinkContact -> CM ()
            acceptOwnerConnection RelayRequestData {relayInvId, reqChatVRange} gi relayLink = do
              ownerMember <- withStore $ \db -> getHostMember db vr user groupId
              void $ acceptRelayJoinRequestAsync user uclId gi ownerMember relayInvId reqChatVRange relayLink
              -- TODO [relays] relay: group invite accepted event, chat item (?)
              pure ()
