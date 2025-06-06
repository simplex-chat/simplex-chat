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
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Library.Subscriber where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (lefts, partitionEithers, rights)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock (UTCTime, diffUTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4
import Data.Word (Word32)
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Library.Internal
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages.CIContent.Events
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
import Simplex.Chat.Types.Shared
import Simplex.FileTransfer.Description (ValidFileDescription)
import qualified Simplex.FileTransfer.Description as FD
import Simplex.FileTransfer.Protocol (FilePartyI)
import qualified Simplex.FileTransfer.Transport as XFTP
import Simplex.FileTransfer.Types (FileErrorType (..), RcvFileId, SndFileId)
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Agent.Protocol as AP (AgentErrorType (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Client (ProxyClientError (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..), pattern PQEncOff, pattern PQEncOn, pattern PQSupportOff, pattern PQSupportOn)
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Protocol (ErrorType (..), MsgFlags (..))
import qualified Simplex.Messaging.Protocol as SMP
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (TransportError (..))
import Simplex.Messaging.Util
import Simplex.Messaging.Version
import qualified System.FilePath as FP
import Text.Read (readMaybe)
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
  eToView $ ChatErrorAgent e Nothing
processAgentMessage corrId connId msg = do
  lockEntity <- critical (withStore (`getChatLockEntity` AgentConnId connId))
  withEntityLock "processAgentMessage" lockEntity $ do
    vr <- chatVersionRange
    -- getUserByAConnId never throws logical errors, only SEDBBusyError can be thrown here
    critical (withStore' (`getUserByAConnId` AgentConnId connId)) >>= \case
      Just user -> processAgentMessageConn vr user corrId connId msg `catchChatError` eToView
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
  CONNECT p h -> hostEvent $ CEvtHostConnected p h
  DISCONNECT p h -> hostEvent $ CEvtHostDisconnected p h
  DOWN srv conns -> serverEvent srv conns NSDisconnected CEvtContactsDisconnected
  UP srv conns -> serverEvent srv conns NSConnected CEvtContactsSubscribed
  SUSPENDED -> toView CEvtChatSuspended
  DEL_USER agentUserId -> toView $ CEvtAgentUserDeleted agentUserId
  ERRS cErrs -> errsEvent cErrs
  where
    hostEvent :: ChatEvent -> CM ()
    hostEvent = whenM (asks $ hostEvents . config) . toView
    serverEvent srv conns nsStatus event = do
      chatModifyVar connNetworkStatuses $ \m -> foldl' (\m' cId -> M.insert cId nsStatus m') m connIds
      ifM (asks $ coreApi . config) (notifyAPI connIds) notifyCLI
      where
        connIds = map AgentConnId conns
        notifyAPI = toView . CEvtNetworkStatus nsStatus
        notifyCLI = do
          cs <- withStore' (`getConnectionsContacts` conns)
          toView $ event srv cs
    errsEvent :: [(ConnId, AgentErrorType)] -> CM ()
    errsEvent cErrs = do
      vr <- chatVersionRange
      errs <- lift $ rights <$> withStoreBatch' (\db -> map (getChatErr vr db) cErrs)
      toView $ CEvtChatErrors errs
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
      Just user -> process user fileId `catchChatError` eToView
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
                      toView $ CEvtSndFileCompleteXFTP user ci' ft
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
      Just user -> process user fileId `catchChatError` eToView
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

processAgentMessageConn :: VersionRangeChat -> User -> ACorrId -> ConnId -> AEvent 'AEConn -> CM ()
processAgentMessageConn vr user@User {userId} corrId agentConnId agentMessage = do
  -- Missing connection/entity errors here will be sent to the view but not shown as CRITICAL alert,
  -- as in this case no need to ACK message - we can't process messages for this connection anyway.
  -- SEDBException will be re-trown as CRITICAL as it is likely to indicate a temporary database condition
  -- that will be resolved with app restart.
  entity <- critical $ withStore (\db -> getConnectionEntity db vr user $ AgentConnId agentConnId) >>= updateConnStatus
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
          eToView (ChatErrorAgent err $ Just connEntity)
          processConnMERR connEntity conn err
        MERRS _ err -> do
          -- error cannot be AUTH error here
          eToView (ChatErrorAgent err $ Just connEntity)
        ERR err -> do
          eToView (ChatErrorAgent err $ Just connEntity)
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
                processEvent ct' conn' tags eInfo chatMsg `catchChatError` \e -> eToView e
              Left e -> do
                atomically $ modifyTVar' tags ("error" :)
                logInfo $ "contact msg=error " <> eInfo <> " " <> tshow e
                eToView (ChatError . CEException $ "error parsing chat message: " <> e)
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
                XMsgUpdate sharedMsgId mContent _ ttl live -> messageUpdate ct'' sharedMsgId mContent msg msgMeta ttl live
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
              toView $ CEvtContactConnected user ct' (fmap fromLocalProfile incognitoProfile)
              when (directOrUsed ct') $ do
                createInternalChatItem user (CDDirectRcv ct') (CIRcvDirectE2EEInfo $ E2EInfo pqEnc) Nothing
                createFeatureEnabledItems ct'
              when (contactConnInitiated conn') $ do
                let Connection {groupLinkId} = conn'
                    doProbeContacts = isJust groupLinkId
                probeMatchingContactsAndMembers ct' (contactConnIncognito ct') doProbeContacts
                withStore' $ \db -> resetContactConnInitiated db user conn'
              forM_ viaUserContactLink $ \userContactLinkId -> do
                ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
                let (UserContactLink {autoAccept}, gli_) = ucl
                when (connChatVersion < batchSend2Version) $ sendAutoReply ct' autoAccept
                -- TODO REMOVE LEGACY vvv
                forM_ gli_ $ \GroupLinkInfo {groupId, memberRole = gLinkMemRole} -> do
                  groupInfo <- withStore $ \db -> getGroupInfo db vr user groupId
                  subMode <- chatReadVar subscriptionMode
                  groupConnIds <- createAgentConnectionAsync user CFCreateConnGrpInv True SCMInvitation subMode
                  gVar <- asks random
                  withStore $ \db -> createNewContactMemberAsync db gVar user groupInfo ct' gLinkMemRole groupConnIds connChatVersion peerChatVRange subMode
            -- TODO REMOVE LEGACY ^^^
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
        JOINED sqSecured ->
          -- [async agent commands] continuation on receiving JOINED
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData ->
            when (directOrUsed ct && sqSecured) $ do
              lift $ setContactNetworkStatus ct NSConnected
              toView $ CEvtContactSndReady user ct
              forM_ viaUserContactLink $ \userContactLinkId -> do
                ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
                let (UserContactLink {autoAccept}, _) = ucl
                when (connChatVersion >= batchSend2Version) $ sendAutoReply ct autoAccept
        QCONT ->
          void $ continueSending connEntity conn
        MWARN msgId err -> do
          updateDirectItemStatus ct conn msgId (CISSndWarning $ agentSndError err)
          processConnMWARN connEntity conn err
        MERR msgId err -> do
          updateDirectItemStatus ct conn msgId (CISSndError $ agentSndError err)
          eToView (ChatErrorAgent err $ Just connEntity)
          processConnMERR connEntity conn err
        MERRS msgIds err -> do
          -- error cannot be AUTH error here
          updateDirectItemsStatusMsgs ct conn (L.toList msgIds) (CISSndError $ agentSndError err)
          eToView (ChatErrorAgent err $ Just connEntity)
        ERR err -> do
          eToView (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()
      where
        sendAutoReply ct = \case
          Just AutoAccept {autoReply = Just mc} -> do
            (msg, _) <- sendDirectContactMessage user ct (XMsgNew $ MCSimple (extMsgContent mc Nothing))
            ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]
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
                ct <- withStore $ \db -> getContactViaMember db vr user m
                withStore' $ \db -> setNewContactMemberConnRequest db user m cReq
                groupLinkId <- withStore' $ \db -> getGroupLinkId db user gInfo
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
                    createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
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
          -- sent when connecting via group link
          XInfo _ ->
            -- TODO Keep rejected member to allow them to appeal against rejection.
            when (memberStatus m == GSMemRejected) $ do
              deleteMemberConnection' m True
              withStore' $ \db -> deleteGroupMember db user m
          XOk -> pure ()
          _ -> messageError "INFO from member must have x.grp.mem.info, x.info or x.ok"
        pure ()
      CON _pqEnc -> unless (memberStatus m == GSMemRejected) $ do
        status' <- case memberStatus m of
          GSMemPendingApproval -> pure GSMemPendingApproval
          _ -> do
            withStore' $ \db -> do
              updateGroupMemberStatus db userId m GSMemConnected
              unless (memberActive membership) $
                updateGroupMemberStatus db userId membership GSMemConnected
            -- possible improvement: check for each pending message, requires keeping track of connection state
            unless (connDisabled conn) $ sendPendingGroupMessages user m conn
            pure GSMemConnected
        withAgent $ \a -> toggleConnectionNtfs a (aConnId conn) $ chatHasNtfs chatSettings
        case memberCategory m of
          GCHostMember -> do
            toView $ CEvtUserJoinedGroup user gInfo {membership = membership {memberStatus = status'}} m {memberStatus = status'}
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
            toView $ CEvtJoinedGroupMember user gInfo m {memberStatus = status'}
            let Connection {viaUserContactLink} = conn
            when (isJust viaUserContactLink && isNothing (memberContactId m)) sendXGrpLinkMem
            when (connChatVersion < batchSend2Version) sendGroupAutoReply
            unless (status' == GSMemPendingApproval) $ introduceToGroup vr user gInfo m
            where
              sendXGrpLinkMem = do
                let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                    profileToSend = profileToSendOnAccept user profileMode True
                void $ sendDirectMemberMessage conn (XGrpLinkMem profileToSend) groupId
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
              processEvent tags eInfo chatMsg `catchChatError` \e -> eToView e
            Left e -> do
              atomically $ modifyTVar' tags ("error" :)
              logInfo $ "group msg=error " <> eInfo <> " " <> tshow e
              eToView (ChatError . CEException $ "error parsing chat message: " <> e)
          forwardMsgs (rights aChatMsgs) `catchChatError` eToView
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
              XMsgUpdate sharedMsgId mContent mentions ttl live -> memberCanSend m' $ groupMessageUpdate gInfo m' sharedMsgId mContent mentions msg brokerTs ttl live
              XMsgDel sharedMsgId memberId -> groupMessageDelete gInfo m' sharedMsgId memberId msg brokerTs
              XMsgReact sharedMsgId (Just memberId) reaction add -> groupMsgReaction gInfo m' sharedMsgId memberId reaction add msg brokerTs
              -- TODO discontinue XFile
              XFile fInv -> processGroupFileInvitation' gInfo m' fInv msg brokerTs
              XFileCancel sharedMsgId -> xFileCancelGroup gInfo m' sharedMsgId
              XFileAcptInv sharedMsgId fileConnReq_ fName -> xFileAcptInvGroup gInfo m' sharedMsgId fileConnReq_ fName
              XInfo p -> xInfoMember gInfo m' p brokerTs
              XGrpLinkMem p -> xGrpLinkMem gInfo m' conn' p
              XGrpLinkAcpt role -> xGrpLinkAcpt gInfo m' role
              XGrpMemNew memInfo -> xGrpMemNew gInfo m' memInfo msg brokerTs
              XGrpMemIntro memInfo memRestrictions_ -> xGrpMemIntro gInfo m' memInfo memRestrictions_
              XGrpMemInv memId introInv -> xGrpMemInv gInfo m' memId introInv
              XGrpMemFwd memInfo introInv -> xGrpMemFwd gInfo m' memInfo introInv
              XGrpMemRole memId memRole -> xGrpMemRole gInfo m' memId memRole msg brokerTs
              XGrpMemRestrict memId memRestrictions -> xGrpMemRestrict gInfo m' memId memRestrictions msg brokerTs
              XGrpMemCon memId -> xGrpMemCon gInfo m' memId
              XGrpMemDel memId withMessages -> xGrpMemDel gInfo m' memId withMessages msg brokerTs
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
        toView $ CEvtGroupMemberSwitch user gInfo m (SwitchProgress qd phase cStats)
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
            toViewTE $ TEGroupMemberVerificationReset user gInfo m'
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
              Just ci -> toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)
              _ -> do
                toView $ CEvtGroupMemberRatchetSync user gInfo m (RatchetSyncProgress rss cStats)
                createInternalChatItem user (CDGroupRcv gInfo m) (CIRcvDecryptionError mde n) Nothing
          ratchetSyncEventItem m' = do
            toView $ CEvtGroupMemberRatchetSync user gInfo m' (RatchetSyncProgress rss cStats)
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
        -- eToView (ChatErrorAgent err $ Just connEntity)
        processConnMERR connEntity conn err
      MERRS msgIds err -> do
        let newStatus = GSSError $ agentSndError err
        -- error cannot be AUTH error here
        withStore' $ \db -> forM_ msgIds $ \msgId ->
          updateGroupItemsErrorStatus db msgId (groupMemberId' m) newStatus `catchAll_` pure ()
        eToView (ChatErrorAgent err $ Just connEntity)
      ERR err -> do
        eToView (ChatErrorAgent err $ Just connEntity)
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
              toView $ CEvtNewChatItems user [AChatItem SCTGroup SMDSnd (GroupChat gInfo) ci]

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
          toView $ CEvtSndFileStart user ci ft
          sendFileChunk user ft
        SENT msgId _proxy -> do
          withStore' $ \db -> updateSndFileChunkSent db ft msgId
          unless (fileStatus == FSCancelled) $ sendFileChunk user ft
        MERR _ err -> do
          cancelSndFileTransfer user ft True >>= mapM_ deleteAgentConnectionAsync
          case err of
            SMP _ SMP.AUTH -> unless (fileStatus == FSCancelled) $ do
              ci <- withStore $ \db -> do
                liftIO (lookupChatRefByFileId db user fileId) >>= \case
                  Just (ChatRef CTDirect _) -> liftIO $ updateFileCancelled db user fileId CIFSSndCancelled
                  _ -> pure ()
                lookupChatItemByFileId db vr user fileId
              toView $ CEvtSndFileRcvCancelled user ci ft
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
          eToView (ChatErrorAgent err $ Just connEntity)
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
          eToView (ChatErrorAgent err $ Just connEntity)
          processConnMERR connEntity conn err
        ERR err -> do
          eToView (ChatErrorAgent err $ Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()

    receiveFileChunk :: RcvFileTransfer -> Maybe Connection -> MsgMeta -> FileChunk -> CM ()
    receiveFileChunk ft@RcvFileTransfer {fileId, chunkSize} conn_ meta@MsgMeta {recipient = (msgId, _), integrity} = \case
      FileChunkCancel ->
        unless (rcvFileCompleteOrCancelled ft) $ do
          cancelRcvFileTransfer user ft >>= mapM_ deleteAgentConnectionAsync
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
        eToView (ChatErrorAgent err $ Just connEntity)
        processConnMERR connEntity conn err
      ERR err -> do
        eToView (ChatErrorAgent err $ Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
      -- TODO add debugging output
      _ -> pure ()
      where
        profileContactRequest :: InvitationId -> VersionRangeChat -> Profile -> Maybe XContactId -> PQSupport -> CM ()
        profileContactRequest invId chatVRange p@Profile {displayName} xContactId_ reqPQSup = do
          withStore (\db -> createOrUpdateContactRequest db vr user userContactLinkId invId chatVRange p xContactId_ reqPQSup) >>= \case
            CORContact contact -> toView $ CEvtContactRequestAlreadyAccepted user contact
            CORGroup gInfo -> toView $ CEvtBusinessRequestAlreadyAccepted user gInfo
            CORRequest cReq -> do
              ucl <- withStore $ \db -> getUserContactLinkById db userId userContactLinkId
              let (UserContactLink {connLinkContact = CCLink connReq _, autoAccept}, gLinkInfo_) = ucl
                  isSimplexTeam = sameConnReqContact connReq adminContactReq
                  v = maxVersion chatVRange
              case autoAccept of
                Just AutoAccept {acceptIncognito, businessAddress}
                  | businessAddress ->
                      if isSimplexTeam && v < businessChatsVersion
                        then do
                          ct <- acceptContactRequestAsync user cReq Nothing reqPQSup
                          toView $ CEvtAcceptingContactRequest user ct
                        else do
                          gInfo <- acceptBusinessJoinRequestAsync user cReq
                          toView $ CEvtAcceptingBusinessRequest user gInfo
                  | otherwise -> case gLinkInfo_ of
                      Nothing -> do
                        -- [incognito] generate profile to send, create connection with incognito profile
                        incognitoProfile <- if acceptIncognito then Just . NewIncognito <$> liftIO generateRandomProfile else pure Nothing
                        ct <- acceptContactRequestAsync user cReq incognitoProfile reqPQSup
                        toView $ CEvtAcceptingContactRequest user ct
                      Just gli@GroupLinkInfo {groupId, memberRole = gLinkMemRole} -> do
                        gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
                        acceptMember_ <- asks $ acceptMember . chatHooks . config
                        maybe (pure $ Right (GAAccepted, gLinkMemRole)) (\am -> liftIO $ am gInfo gli p) acceptMember_ >>= \case
                          Right (acceptance, useRole)
                            | v < groupFastLinkJoinVersion ->
                                messageError "processUserContactRequest: chat version range incompatible for accepting group join request"
                            | otherwise -> do
                                let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                                mem <- acceptGroupJoinRequestAsync user gInfo cReq acceptance useRole profileMode
                                createInternalChatItem user (CDGroupRcv gInfo mem) (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
                                toView $ CEvtAcceptingGroupJoinRequestMember user gInfo mem
                          Left rjctReason
                            | v < groupJoinRejectVersion ->
                                messageWarning $ "processUserContactRequest (group " <> groupName' gInfo <> "): joining of " <> displayName <> " is blocked"
                            | otherwise -> do
                                mem <- acceptGroupJoinSendRejectAsync user gInfo cReq rjctReason
                                toViewTE $ TERejectingGroupJoinRequestMember user gInfo mem rjctReason
                _ -> toView $ CEvtReceivedContactRequest user cReq

    -- TODO [knocking] review
    memberCanSend :: GroupMember -> CM () -> CM ()
    memberCanSend GroupMember {memberRole, memberStatus} a
      | memberRole > GRObserver || memberStatus == GSMemPendingApproval = a
      | otherwise = messageError "member is not allowed to send messages"

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
              toView $ CEvtConnectionInactive connEntity True
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
        cancelRcvFileTransfer user ft >>= mapM_ deleteAgentConnectionAsync
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
      toView $ CEvtConnectedToGroupMember user gInfo m ct_

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
    messageWarning = toView . CEvtMessageError user "warning"

    messageError :: Text -> CM ()
    messageError = toView . CEvtMessageError user "error"

    newContentMessage :: Contact -> MsgContainer -> RcvMessage -> MsgMeta -> CM ()
    newContentMessage ct mc msg@RcvMessage {sharedMsgId_} msgMeta = do
      let ExtMsgContent content _ fInv_ _ _ = mcExtMsgContent mc
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
          let ExtMsgContent _ _ _ itemTTL live_ = mcExtMsgContent mc
              timed_ = rcvContactCITimed ct itemTTL
              live = fromMaybe False live_
          file_ <- processFileInvitation fInv_ content $ \db -> createRcvFileTransfer db userId ct
          newChatItem (CIRcvMsgContent content, msgContentTexts content) (snd <$> file_) timed_ live
          autoAcceptFile file_
      where
        brokerTs = metaBrokerTs msgMeta
        newChatItem content ciFile_ timed_ live = do
          ci <- saveRcvChatItem' user (CDDirectRcv ct) Nothing msg sharedMsgId_ brokerTs content ciFile_ timed_ live M.empty
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getDirectCIReactions db ct sharedMsgId) sharedMsgId_
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci {reactions}]

    autoAcceptFile :: Maybe (RcvFileTransfer, CIFile 'MDRcv) -> CM ()
    autoAcceptFile = mapM_ $ \(ft, CIFile {fileSize}) -> do
      -- ! autoAcceptFileSize is only used in tests
      ChatConfig {autoAcceptFileSize = sz} <- asks config
      when (sz > fileSize) $ receiveFileEvt' user ft False Nothing Nothing >>= toView

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
          toView $ CEvtRcvFileDescrReady user ci ft' rfd
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
        ci <- saveRcvChatItem' user (CDDirectRcv ct) Nothing msg (Just sharedMsgId) brokerTs (content, ts) Nothing timed_ live M.empty
        ci' <- withStore' $ \db -> do
          createChatItemVersion db (chatItemId' ci) brokerTs mc
          updateDirectChatItem' db user contactId ci content True live Nothing Nothing
        toView $ CEvtChatItemUpdated user (AChatItem SCTDirect SMDRcv (DirectChat ct) ci')
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
                      startUpdatedTimedItemThread user (ChatRef CTDirect contactId) ci ci'
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
                  deletions <- if featureAllowed SCFFullDelete forContact ct
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

    groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> MemberId -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM ()
    groupMsgReaction g m sharedMsgId itemMemberId reaction add RcvMessage {msgId} brokerTs = do
      when (groupFeatureAllowed SGFReactions g) $ do
        rs <- withStore' $ \db -> getGroupReactions db g m itemMemberId sharedMsgId False
        when (reactionAllowed add reaction rs) $ do
          updateChatItemReaction `catchCINotFound` \_ ->
            withStore' $ \db -> setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
      where
        updateChatItemReaction = do
          cEvt_ <- withStore $ \db -> do
            CChatItem md ci <- getGroupMemberCIBySharedMsgId db user g itemMemberId sharedMsgId
            if ciReactionAllowed ci
              then liftIO $ do
                setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                reactions <- getGroupCIReactions db g itemMemberId sharedMsgId
                let ci' = CChatItem md ci {reactions}
                    r = ACIReaction SCTGroup SMDRcv (GroupChat g) $ CIReaction (CIGroupRcv m) ci' brokerTs reaction
                pure $ Just $ CEvtChatItemReaction user add r
              else pure Nothing
          mapM_ toView cEvt_

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
      | otherwise = case prohibitedGroupContent gInfo m content ft_ fInv_ False of
          Just f -> rejected f
          Nothing ->
            withStore' (\db -> getCIModeration db vr user gInfo memberId sharedMsgId_) >>= \case
              Just ciModeration -> do
                applyModeration ciModeration
                withStore' $ \db -> deleteCIModeration db gInfo memberId sharedMsgId_
              Nothing -> createContentItem
      where
        rejected f = newChatItem (ciContentNoParse $ CIRcvGroupFeatureRejected f) Nothing Nothing False
        timed' = if forwarded then rcvCITimed_ (Just Nothing) itemTTL else rcvGroupCITimed gInfo itemTTL
        live' = fromMaybe False live_
        ExtMsgContent content mentions fInv_ itemTTL live_ = mcExtMsgContent mc
        ts@(_, ft_) = msgContentTexts content
        saveRcvCI = saveRcvChatItem' user (CDGroupRcv gInfo m) (memberNotInHistory m) msg sharedMsgId_ brokerTs
        createBlockedByAdmin
          | groupFeatureAllowed SGFFullDelete gInfo = do
              -- ignores member role when blocked by admin
              ci <- saveRcvCI (ciContentNoParse CIRcvBlocked) Nothing timed' False M.empty
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
          | groupFeatureMemberAllowed SGFFullDelete moderator gInfo = do
              ci <- saveRcvCI (ciContentNoParse CIRcvModerated) Nothing timed' False M.empty
              ci' <- withStore' $ \db -> updateGroupChatItemModerated db user gInfo ci moderator moderatedAt
              groupMsgToView gInfo ci'
          | otherwise = do
              file_ <- processFileInv
              ci <- createNonLive file_
              deletions <- markGroupCIsDeleted user gInfo [CChatItem SMDRcv ci] (Just moderator) moderatedAt
              toView $ CEvtChatItemsDeleted user deletions False False
        createNonLive file_ =
          saveRcvCI (CIRcvMsgContent content, ts) (snd <$> file_) timed' False mentions
        createContentItem = do
          file_ <- processFileInv
          newChatItem (CIRcvMsgContent content, ts) (snd <$> file_) timed' live'
          when (showMessages $ memberSettings m) $ autoAcceptFile file_
        processFileInv =
          processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId m
        newChatItem ciContent ciFile_ timed_ live = do
          let mentions' = if showMessages (memberSettings m) then mentions else []
          ci <- saveRcvCI ciContent ciFile_ timed_ live mentions'
          ci' <- blockedMember m ci $ withStore' $ \db -> markGroupChatItemBlocked db user gInfo ci
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getGroupCIReactions db gInfo memberId sharedMsgId) sharedMsgId_
          groupMsgToView gInfo ci' {reactions}

    groupMessageUpdate :: GroupInfo -> GroupMember -> SharedMsgId -> MsgContent -> Map MemberName MsgMention -> RcvMessage -> UTCTime -> Maybe Int -> Maybe Bool -> CM ()
    groupMessageUpdate gInfo@GroupInfo {groupId} m@GroupMember {groupMemberId, memberId} sharedMsgId mc mentions msg@RcvMessage {msgId} brokerTs ttl_ live_
      | prohibitedSimplexLinks gInfo m ft_ =
          messageWarning $ "x.msg.update ignored: feature not allowed " <> groupFeatureNameText GFSimplexLinks
      | otherwise = do
          updateRcvChatItem `catchCINotFound` \_ -> do
            -- This patches initial sharedMsgId into chat item when locally deleted chat item
            -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
            -- Chat item and update message which created it will have different sharedMsgId in this case...
            let timed_ = rcvGroupCITimed gInfo ttl_
                mentions' = if showMessages (memberSettings m) then mentions else []
            ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) (memberNotInHistory m) msg (Just sharedMsgId) brokerTs (content, ts) Nothing timed_ live mentions'
            ci' <- withStore' $ \db -> do
              createChatItemVersion db (chatItemId' ci) brokerTs mc
              ci' <- updateGroupChatItem db user groupId ci content True live Nothing
              blockedMember m ci' $ markGroupChatItemBlocked db user gInfo ci'
            toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci')
      where
        content = CIRcvMsgContent mc
        ts@(_, ft_) = msgContentTexts mc
        live = fromMaybe False live_
        updateRcvChatItem = do
          cci <- withStore $ \db -> getGroupChatItemBySharedMsgId db user gInfo groupMemberId sharedMsgId
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
                        ciMentions <- getRcvCIMentions db user gInfo ft_ mentions
                        ci' <- updateGroupChatItem db user groupId ci {reactions} content edited live $ Just msgId
                        updateGroupCIMentions db gInfo ci' ciMentions
                      toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci')
                      startUpdatedTimedItemThread user (ChatRef CTGroup groupId) ci ci'
                    else toView $ CEvtChatItemNotChanged user (AChatItem SCTGroup SMDRcv (GroupChat gInfo) ci)
                else messageError "x.msg.update: group member attempted to update a message of another member"
            _ -> messageError "x.msg.update: group member attempted invalid message update"

    memberNotInHistory :: GroupMember -> Maybe NotInHistory
    memberNotInHistory = \case
      GroupMember {memberStatus = GSMemPendingApproval} -> Just NotInHistory
      _ -> Nothing

    groupMessageDelete :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> RcvMessage -> UTCTime -> CM ()
    groupMessageDelete gInfo@GroupInfo {membership} m@GroupMember {memberId, memberRole = senderRole} sharedMsgId sndMemberId_ RcvMessage {msgId} brokerTs = do
      let msgMemberId = fromMaybe memberId sndMemberId_
      withStore' (\db -> runExceptT $ getGroupMemberCIBySharedMsgId db user gInfo msgMemberId sharedMsgId) >>= \case
        Right cci@(CChatItem _ ci@ChatItem {chatDir}) -> case chatDir of
          CIGroupRcv mem -> case sndMemberId_ of
            -- regular deletion
            Nothing
              | sameMemberId memberId mem && msgMemberId == memberId && rcvItemDeletable ci brokerTs ->
                  delete cci Nothing
              | otherwise ->
                  messageError "x.msg.del: member attempted invalid message delete"
            -- moderation (not limited by time)
            Just _
              | sameMemberId memberId mem && msgMemberId == memberId ->
                  delete cci (Just m)
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
            | sameMemberId sndMemberId mem -> checkRole mem $ do
                delete cci (Just m)
                archiveMessageReports cci m
            | otherwise -> messageError "x.msg.del: message of another member with incorrect memberId"
          _ -> messageError "x.msg.del: message of another member without memberId"
        checkRole GroupMember {memberRole} a
          | senderRole < GRModerator || senderRole < memberRole =
              messageError "x.msg.del: message of another member with insufficient member permissions"
          | otherwise = a
        delete :: CChatItem 'CTGroup -> Maybe GroupMember -> CM ()
        delete cci byGroupMember = do
          deletions <- if groupFeatureMemberAllowed SGFFullDelete m gInfo
            then deleteGroupCIs user gInfo [cci] byGroupMember brokerTs
            else markGroupCIsDeleted user gInfo [cci] byGroupMember brokerTs
          toView $ CEvtChatItemsDeleted user deletions False False
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
      ci <- saveRcvChatItem' user (CDDirectRcv ct) Nothing msg sharedMsgId_ brokerTs content ciFile Nothing False M.empty
      toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]
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
          content = ciContentNoParse $ CIRcvMsgContent $ MCFile ""
      ci <- saveRcvChatItem' user (CDGroupRcv gInfo m) Nothing msg sharedMsgId_ brokerTs content ciFile Nothing False M.empty
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
        cancelRcvFileTransfer user ft >>= mapM_ deleteAgentConnectionAsync
        ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
        toView $ CEvtRcvFileSndCancelled user ci ft

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
              pure $ CEvtSndFileStart user ci' sft
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

    xFileCancelGroup :: GroupInfo -> GroupMember -> SharedMsgId -> CM ()
    xFileCancelGroup g@GroupInfo {groupId} GroupMember {groupMemberId, memberId} sharedMsgId = do
      fileId <- withStore $ \db -> getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
      CChatItem msgDir ChatItem {chatDir} <- withStore $ \db -> getGroupChatItemBySharedMsgId db user g groupMemberId sharedMsgId
      case (msgDir, chatDir) of
        (SMDRcv, CIGroupRcv m) -> do
          if sameMemberId memberId m
            then do
              ft <- withStore (\db -> getRcvFileTransfer db user fileId)
              unless (rcvFileCompleteOrCancelled ft) $ do
                cancelRcvFileTransfer user ft >>= mapM_ deleteAgentConnectionAsync
                ci <- withStore $ \db -> getChatItemByFileId db vr user fileId
                toView $ CEvtRcvFileSndCancelled user ci ft
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
              pure $ CEvtSndFileStart user ci' sft
            toView event
            ifM
              (allowSendInline fileSize fileInline)
              (sendMemberFileInline m conn ft sharedMsgId)
              (messageError "x.file.acpt.inv: fileSize is bigger than allowed to send inline")
          _ -> messageError "x.file.acpt.inv: member connection is not active"
        else messageError "x.file.acpt.inv: fileName is different from expected"

    groupMsgToView :: forall d. MsgDirectionI d => GroupInfo -> ChatItem 'CTGroup d -> CM ()
    groupMsgToView gInfo ci =
      toView $ CEvtNewChatItems user [AChatItem SCTGroup (msgDirection @d) (GroupChat gInfo) ci]

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
            toView $ CEvtUserAcceptedGroupSent user gInfo {membership = membership {memberStatus = GSMemAccepted}} (Just ct)
          else do
            let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
            ci <- saveRcvChatItemNoParse user (CDDirectRcv ct) msg brokerTs content
            withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]
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
          ct' <- withStore' $ \db -> updateContactStatus db user c CSDeleted
          contactConns <- withStore' $ \db -> getContactConnections db vr userId ct'
          deleteAgentConnectionsAsync $ map aConnId contactConns
          forM_ contactConns $ \conn -> withStore' $ \db -> updateConnectionStatus db conn ConnDeleted
          activeConn' <- forM (contactConn ct') $ \conn -> pure conn {connStatus = ConnDeleted}
          let ct'' = ct' {activeConn = activeConn'} :: Contact
          ci <- saveRcvChatItemNoParse user (CDDirectRcv ct'') msg brokerTs (CIRcvDirectEvent RDEContactDeleted)
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct'') ci]
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
              n' /= n || fn' /= fn || i' /= i || cl' /= cl
            Profile {displayName = n, fullName = fn, image = i, contactLink = cl} = p
            Profile {displayName = n', fullName = fn', image = i', contactLink = cl'} = p'

    xInfoMember :: GroupInfo -> GroupMember -> Profile -> UTCTime -> CM ()
    xInfoMember gInfo m p' brokerTs = void $ processMemberProfileUpdate gInfo m p' True (Just brokerTs)

    xGrpLinkMem :: GroupInfo -> GroupMember -> Connection -> Profile -> CM ()
    xGrpLinkMem gInfo@GroupInfo {membership, businessChat} m@GroupMember {groupMemberId, memberCategory, memberStatus} Connection {viaGroupLink} p' = do
      xGrpLinkMemReceived <- withStore $ \db -> getXGrpLinkMemReceived db groupMemberId
      if (viaGroupLink || isJust businessChat) && isNothing (memberContactId m) && memberCategory == GCHostMember && not xGrpLinkMemReceived
        then do
          m' <- processMemberProfileUpdate gInfo m p' False Nothing
          withStore' $ \db -> setXGrpLinkMemReceived db groupMemberId True
          unless (memberStatus == GSMemPendingApproval) $ do
            let connectedIncognito = memberIncognito membership
            probeMatchingMemberContact m' connectedIncognito
        else messageError "x.grp.link.mem error: invalid group link host profile update"

    xGrpLinkAcpt :: GroupInfo -> GroupMember -> GroupMemberRole -> CM ()
    xGrpLinkAcpt gInfo@GroupInfo {membership} m role = do
      membership' <- withStore' $ \db -> do
        updateGroupMemberStatus db userId m GSMemConnected
        updateGroupMemberAccepted db user membership role
      let m' = m {memberStatus = GSMemConnected}
      toView $ CEvtUserJoinedGroup user gInfo {membership = membership'} m'
      let connectedIncognito = memberIncognito membership
      probeMatchingMemberContact m' connectedIncognito

    processMemberProfileUpdate :: GroupInfo -> GroupMember -> Profile -> Bool -> Maybe UTCTime -> CM GroupMember
    processMemberProfileUpdate gInfo m@GroupMember {memberProfile = p, memberContactId} p' createItems itemTs_
      | redactedMemberProfile (fromLocalProfile p) /= redactedMemberProfile p' = do
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
          toView $ CEvtCallInvitation RcvCallInvitation {user, contact = ct, callType, sharedKey, callUUID, callTs = chatItemTs' ci}
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]
        else featureRejected CFCalls
      where
        brokerTs = metaBrokerTs msgMeta
        saveCallItem status = saveRcvChatItemNoParse user (CDDirectRcv ct) msg brokerTs (CIRcvCall status 0)
        featureRejected f = do
          let content = ciContentNoParse $ CIRcvChatFeatureRejected f
          ci <- saveRcvChatItem' user (CDDirectRcv ct) Nothing msg sharedMsgId_ brokerTs content Nothing Nothing False M.empty
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat ct) ci]

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
          toView $ CEvtContactsMerged user c1' c2' c2''
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
      toView $ CEvtContactAndMemberAssociated user c1 g m2 c1
      pure c1

    associateContactWithMember :: GroupMember -> Contact -> CM Contact
    associateContactWithMember m1@GroupMember {groupId} c2 = do
      c2' <- withStore $ \db -> associateContactWithMemberRecord db vr user m1 c2
      g <- withStore $ \db -> getGroupInfo db vr user groupId
      toView $ CEvtContactAndMemberAssociated user c2 g m1 c2'
      pure c2'

    saveConnInfo :: Connection -> ConnInfo -> CM (Connection, Bool)
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage activeConn connInfo
      conn' <- updatePeerChatVRange activeConn chatVRange
      case chatMsgEvent of
        XInfo p -> do
          ct <- withStore $ \db -> createDirectContact db user conn' p
          toView $ CEvtContactConnecting user ct
          pure (conn', False)
        XGrpLinkInv glInv -> do
          (gInfo, host) <- withStore $ \db -> createGroupInvitedViaLink db vr user conn' glInv
          toView $ CEvtGroupLinkConnecting user gInfo host
          pure (conn', True)
        XGrpLinkReject glRjct@GroupLinkRejection {rejectionReason} -> do
          (gInfo, host) <- withStore $ \db -> createGroupRejectedViaLink db vr user conn' glRjct
          toView $ CEvtGroupLinkConnecting user gInfo host
          toViewTE $ TEGroupLinkRejected user gInfo rejectionReason
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
            toView $ CEvtUnknownMemberAnnounced user gInfo m unknownMember updatedMember
            memberAnnouncedToView updatedMember
          Right _ -> messageError "x.grp.mem.new error: member already exists"
          Left _ -> do
            newMember <- withStore $ \db -> createNewGroupMember db user gInfo m memInfo GCPostMember GSMemAnnounced
            memberAnnouncedToView newMember
      where
        memberAnnouncedToView announcedMember@GroupMember {groupMemberId, memberProfile} = do
          let event = RGEMemberAdded groupMemberId (fromLocalProfile memberProfile)
          ci <- saveRcvChatItemNoParse user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent event)
          groupMsgToView gInfo ci
          toView $ CEvtJoinedGroupMemberConnecting user gInfo m announcedMember

    xGrpMemIntro :: GroupInfo -> GroupMember -> MemberInfo -> Maybe MemberRestrictions -> CM ()
    xGrpMemIntro gInfo@GroupInfo {chatSettings} m@GroupMember {memberRole, localDisplayName = c} memInfo@(MemberInfo memId _ memChatVRange _) memRestrictions = do
      case memberCategory m of
        GCHostMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Right _ -> messageError "x.grp.mem.intro ignored: member already exists"
            Left _ -> do
              when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole c)
              case memChatVRange of
                Nothing -> messageError "x.grp.mem.intro: member chat version range incompatible"
                Just (ChatVersionRange mcvr)
                  | maxVersion mcvr >= groupDirectInvVersion -> do
                      subMode <- chatReadVar subscriptionMode
                      -- [async agent commands] commands should be asynchronous, continuation is to send XGrpMemInv - have to remember one has completed and process on second
                      groupConnIds <- createConn subMode
                      let chatV = maybe (minVersion vr) (\peerVR -> vr `peerConnChatVersion` fromChatVRange peerVR) memChatVRange
                      void $ withStore $ \db -> createIntroReMember db user gInfo m chatV memInfo memRestrictions groupConnIds subMode
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
            Right reMember -> do
              GroupMemberIntro {introId} <- withStore $ \db -> saveIntroInvitation db reMember m introInv
              sendGroupMemberMessage gInfo reMember (XGrpMemFwd (memberInfo m) introInv) (Just introId) $
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
              ci <- saveRcvChatItemNoParse user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent gEvent)
              groupMsgToView gInfo ci
              toView CEvtMemberRole {user, groupInfo = gInfo', byMember = m, member = member {memberRole = memRole}, fromRole, toRole = memRole}

    checkHostRole :: GroupMember -> GroupMemberRole -> CM ()
    checkHostRole GroupMember {memberRole, localDisplayName} memRole =
      when (memberRole < GRAdmin || memberRole < memRole) $ throwChatError (CEGroupContactRole localDisplayName)

    xGrpMemRestrict :: GroupInfo -> GroupMember -> MemberId -> MemberRestrictions -> RcvMessage -> UTCTime -> CM ()
    xGrpMemRestrict
      gInfo@GroupInfo {membership = GroupMember {memberId = membershipMemId}}
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
              Right bm@GroupMember {groupMemberId = bmId, memberRole, blockedByAdmin, memberProfile = bmp}
                | blockedByAdmin == mrsBlocked restriction -> pure ()
                | senderRole < GRModerator || senderRole < memberRole -> messageError "x.grp.mem.restrict with insufficient member permissions"
                | otherwise -> do
                    bm' <- setMemberBlocked bm
                    toggleNtf bm' (not blocked)
                    let ciContent = CIRcvGroupEvent $ RGEMemberBlocked bmId (fromLocalProfile bmp) blocked
                    ci <- saveRcvChatItemNoParse user (CDGroupRcv gInfo m) msg brokerTs ciContent
                    groupMsgToView gInfo ci
                    toView CEvtMemberBlockedForAll {user, groupInfo = gInfo, byMember = m, member = bm, blocked}
              Left (SEGroupMemberNotFoundByMemberId _) -> do
                bm <- createUnknownMember gInfo memId
                bm' <- setMemberBlocked bm
                toView $ CEvtUnknownMemberBlocked user gInfo m bm'
              Left e -> throwError $ ChatErrorStore e
        where
          setMemberBlocked bm = withStore' $ \db -> updateGroupMemberBlocked db user gInfo restriction bm
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

    xGrpMemDel :: GroupInfo -> GroupMember -> MemberId -> Bool -> RcvMessage -> UTCTime -> CM ()
    xGrpMemDel gInfo@GroupInfo {membership} m@GroupMember {memberRole = senderRole} memId withMessages msg brokerTs = do
      let GroupMember {memberId = membershipMemId} = membership
      if membershipMemId == memId
        then checkRole membership $ do
          deleteGroupLinkIfExists user gInfo
          -- member records are not deleted to keep history
          members <- withStore' $ \db -> getGroupMembers db vr user gInfo
          deleteMembersConnections user members
          withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemRemoved
          when withMessages $ deleteMessages membership SMDSnd
          deleteMemberItem RGEUserDeleted
          toView $ CEvtDeletedMemberUser user gInfo {membership = membership {memberStatus = GSMemRemoved}} m withMessages
        else
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memId) >>= \case
            Left _ -> messageError "x.grp.mem.del with unknown member ID"
            Right member@GroupMember {groupMemberId, memberProfile} ->
              checkRole member $ do
                -- ? prohibit deleting member if it's the sender - sender should use x.grp.leave
                deleteMemberConnection member
                -- undeleted "member connected" chat item will prevent deletion of member record
                deleteOrUpdateMemberRecord user member
                when withMessages $ deleteMessages member SMDRcv
                deleteMemberItem $ RGEMemberDeleted groupMemberId (fromLocalProfile memberProfile)
                toView $ CEvtDeletedMember user gInfo m member {memberStatus = GSMemRemoved} withMessages
      where
        checkRole GroupMember {memberRole} a
          | senderRole < GRAdmin || senderRole < memberRole =
              messageError "x.grp.mem.del with insufficient member permissions"
          | otherwise = a
        deleteMemberItem gEvent = do
          ci <- saveRcvChatItemNoParse user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent gEvent)
          groupMsgToView gInfo ci
        deleteMessages :: MsgDirectionI d => GroupMember -> SMsgDirection d -> CM ()
        deleteMessages delMem msgDir
          | groupFeatureMemberAllowed SGFFullDelete m gInfo = deleteGroupMemberCIs user gInfo delMem m msgDir
          | otherwise = markGroupMemberCIsDeleted user gInfo delMem m

    xGrpLeave :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM ()
    xGrpLeave gInfo m msg brokerTs = do
      deleteMemberConnection m
      -- member record is not deleted to allow creation of "member left" chat item
      withStore' $ \db -> updateGroupMemberStatus db userId m GSMemLeft
      ci <- saveRcvChatItemNoParse user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent RGEMemberLeft)
      groupMsgToView gInfo ci
      toView $ CEvtLeftMember user gInfo m {memberStatus = GSMemLeft}

    xGrpDel :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM ()
    xGrpDel gInfo@GroupInfo {membership} m@GroupMember {memberRole} msg brokerTs = do
      when (memberRole /= GROwner) $ throwChatError $ CEGroupUserRole gInfo GROwner
      ms <- withStore' $ \db -> do
        members <- getGroupMembers db vr user gInfo
        updateGroupMemberStatus db userId membership GSMemGroupDeleted
        pure members
      -- member records are not deleted to keep history
      deleteMembersConnections user ms
      ci <- saveRcvChatItemNoParse user (CDGroupRcv gInfo m) msg brokerTs (CIRcvGroupEvent RGEGroupDeleted)
      groupMsgToView gInfo ci
      toView $ CEvtGroupDeleted user gInfo {membership = membership {memberStatus = GSMemGroupDeleted}} m

    xGrpInfo :: GroupInfo -> GroupMember -> GroupProfile -> RcvMessage -> UTCTime -> CM ()
    xGrpInfo g@GroupInfo {groupProfile = p, businessChat} m@GroupMember {memberRole} p' msg brokerTs
      | memberRole < GROwner = messageError "x.grp.info with insufficient member permissions"
      | otherwise = case businessChat of
          Nothing -> unless (p == p') $ do
            g' <- withStore $ \db -> updateGroupProfile db user g p'
            toView $ CEvtGroupUpdated user g g' (Just m)
            let cd = CDGroupRcv g' m
            unless (sameGroupProfileInfo p p') $ do
              ci <- saveRcvChatItemNoParse user cd msg brokerTs (CIRcvGroupEvent $ RGEGroupUpdated p')
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
        toView $ CEvtGroupUpdated user g g' (Just m)
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
          toView $ CEvtNewMemberContactReceivedInv user mCt' g m'
          forM_ mContent_ $ \mc -> do
            ci <- saveRcvChatItem user (CDDirectRcv mCt') msg brokerTs (CIRcvMsgContent mc, msgContentTexts mc)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv (DirectChat mCt') ci]

    securityCodeChanged :: Contact -> CM ()
    securityCodeChanged ct = do
      toViewTE $ TEContactVerificationReset user ct
      createInternalChatItem user (CDDirectRcv ct) (CIRcvConnEvent RCEVerificationCodeReset) Nothing

    xGrpMsgForward :: GroupInfo -> GroupMember -> MemberId -> ChatMessage 'Json -> UTCTime -> CM ()
    xGrpMsgForward gInfo@GroupInfo {groupId} m@GroupMember {memberRole, localDisplayName} memberId msg msgTs = do
      when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole localDisplayName)
      withStore' (\db -> runExceptT $ getGroupMemberByMemberId db vr user gInfo memberId) >>= \case
        Right author -> processForwardedMsg author msg
        Left (SEGroupMemberNotFoundByMemberId _) -> do
          unknownAuthor <- createUnknownMember gInfo memberId
          toView $ CEvtUnknownMemberCreated user gInfo m unknownAuthor
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
            XMsgUpdate sharedMsgId mContent mentions ttl live -> memberCanSend author $ groupMessageUpdate gInfo author sharedMsgId mContent mentions rcvMsg msgTs ttl live
            XMsgDel sharedMsgId memId -> groupMessageDelete gInfo author sharedMsgId memId rcvMsg msgTs
            XMsgReact sharedMsgId (Just memId) reaction add -> groupMsgReaction gInfo author sharedMsgId memId reaction add rcvMsg msgTs
            XFileCancel sharedMsgId -> xFileCancelGroup gInfo author sharedMsgId
            XInfo p -> xInfoMember gInfo author p msgTs
            XGrpMemNew memInfo -> xGrpMemNew gInfo author memInfo rcvMsg msgTs
            XGrpMemRole memId memRole -> xGrpMemRole gInfo author memId memRole rcvMsg msgTs
            XGrpMemDel memId withMessages -> xGrpMemDel gInfo author memId withMessages rcvMsg msgTs
            XGrpLeave -> xGrpLeave gInfo author rcvMsg msgTs
            XGrpDel -> xGrpDel gInfo author rcvMsg msgTs
            XGrpInfo p' -> xGrpInfo gInfo author p' rcvMsg msgTs
            XGrpPrefs ps' -> xGrpPrefs gInfo author ps'
            _ -> messageError $ "x.grp.msg.forward: unsupported forwarded event " <> T.pack (show $ toCMEventTag event)

    createUnknownMember :: GroupInfo -> MemberId -> CM GroupMember
    createUnknownMember gInfo memberId = do
      let name = nameFromMemberId memberId
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
      items <- withStore' (\db -> getGroupChatItemsByAgentMsgId db user groupId connId msgId)
      cis <- catMaybes <$> withStore (\db -> mapM (updateItem db) items)
      let acis = map gItem cis
      unless (null acis) $ toView $ CEvtChatItemsStatusesUpdated user acis
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
