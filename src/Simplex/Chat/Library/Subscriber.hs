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
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (lefts, partitionEithers, rights)
import Data.Foldable (foldr', foldrM)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (find, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import qualified Data.IntSet as IS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4
import Data.Word (Word32)
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Delivery
import Simplex.Chat.Files (getChatTempDirectory)
import Simplex.Chat.Library.Internal
import Simplex.Chat.Web (channelContentChanged, channelProfileUpdated, channelRemoved)
import Simplex.Chat.Messages
import Simplex.Chat.Messages.Batch (batchDeliveryTasks1, batchProfiles, batchProfilesWithBody, encodeBinaryBatch, encodeFwdElement, maxBatchElementSize)
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
import Simplex.Chat.Operators
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
import Simplex.Messaging.Agent.Env.SQLite (Worker (..))
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Agent.Protocol as AP (AgentErrorType (..))
import Simplex.Messaging.Agent.RetryInterval (RetryInterval (..), nextRetryDelay)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Client (NetworkRequestMode (..), ProxyClientError (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..), pattern PQEncOff, pattern PQEncOn, pattern PQSupportOff, pattern PQSupportOn)
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import qualified Simplex.Messaging.Crypto.Lazy as LC
import Simplex.Messaging.Encoding (smpEncode)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Protocol (ErrorType (..), MsgFlags (..), ServiceSub (..), ServiceSubError (..), ServiceSubResult (..))
import qualified Simplex.Messaging.Protocol as SMP
import Simplex.Messaging.ServiceScheme (ServiceScheme (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (TransportError (..))
import Simplex.Messaging.Util
import Simplex.Messaging.Version
import qualified System.FilePath as FP
import System.Mem.Weak (Weak)
import Text.Read (readMaybe)
import UnliftIO.Concurrent (ThreadId, forkIO, mkWeakThreadId)
import UnliftIO.Directory
import UnliftIO.STM

smallGroupsRcptsMemLimit :: Int
smallGroupsRcptsMemLimit = 20

-- Verifies member signatures over CBGroup <> (publicGroupId, memberId) <> signedBody under the given key.
-- signatures is NonEmpty so the verification can't be vacuously true.
verifyGroupSig :: C.PublicKeyEd25519 -> B64UrlByteString -> MemberId -> NonEmpty MsgSignature -> ByteString -> Bool
verifyGroupSig key publicGroupId memberId signatures signedBody =
  let prefix = smpEncode CBGroup <> smpEncode (publicGroupId, memberId)
   in all (\case (MsgSignature KRMember sig) -> C.verify (C.APublicVerifyKey C.SEd25519 key) sig (prefix <> signedBody)) signatures

processAgentMessage :: ACorrId -> ConnId -> AEvent 'AEConn -> CM ()
processAgentMessage _ _ (DEL_RCVQS delQs) =
  toView $ CEvtAgentRcvQueuesDeleted $ L.map rcvQ delQs
  where
    rcvQ (connId, server, rcvId, err_) = DeletedRcvQueue (AgentConnId connId) server (AgentQueueId rcvId) err_
processAgentMessage _ _ (DEL_CONNS connIds) =
  toView $ CEvtAgentConnsDeleted $ L.map AgentConnId connIds
processAgentMessage _ "" (ERR e) =
  eToView $ chatErrorAgent e
processAgentMessage corrId connId msg = do
  lockEntity <- critical connId (withStore (`getChatLockEntity` AgentConnId connId))
  withEntityLock "processAgentMessage" lockEntity $ do
    cxt <- chatStoreCxt
    -- getUserByAConnId never throws logical errors, only SEDBBusyError can be thrown here
    critical connId (withStore' (`getUserByAConnId` AgentConnId connId)) >>= \case
      Just user -> processAgentMessageConn cxt user corrId connId msg `catchAllErrors` eToView
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
  SERVICE_UP srv (ServiceSubResult e_ ss) -> serviceEvent srv $ ServiceSubUp (errText <$> e_) (smpQueueCount ss)
    where
      errText = \case
        SSErrorServiceId {} -> "unexpected service ID"
        SSErrorQueueCount {expectedQueueCount = n} -> "expected " <> tshow n <> " connections"
        SSErrorQueueIdsHash {} -> "different IDs hash"
  SERVICE_DOWN srv ss -> serviceEvent srv $ ServiceSubDown $ smpQueueCount ss
  SERVICE_ALL srv -> serviceEvent srv ServiceSubAll
  SERVICE_END srv ss -> serviceEvent srv $ ServiceSubEnd $ smpQueueCount ss
  ERRS cErrs -> errsEvent $ L.toList cErrs
  where
    hostEvent :: ChatEvent -> CM ()
    hostEvent = whenM (asks $ hostEvents . config) . toView
    serverEvent :: SMPServer -> SubscriptionStatus -> [ConnId] -> CM ()
    serverEvent srv nsStatus conns = toView $ CEvtSubscriptionStatus srv nsStatus $ map AgentConnId conns
    serviceEvent :: SMPServer -> ServiceSubEvent -> CM ()
    serviceEvent srv = toView . CEvtServiceSubStatus srv
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
      cxt <- chatStoreCxt
      unless cancelled $ case msg of
        SFPROG sndProgress sndTotal -> do
          let status = CIFSSndTransfer {sndProgress, sndTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            lookupChatItemByFileId db cxt user fileId
          toView $ CEvtSndFileProgressXFTP user ci ft sndProgress sndTotal
        SFDONE sndDescr rfds -> do
          withStore' $ \db -> setSndFTPrivateSndDescr db user fileId (fileDescrText sndDescr)
          ci <- withStore $ \db -> lookupChatItemByFileId db cxt user fileId
          case ci of
            Nothing -> do
              lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
              withStore' $ \db -> createExtraSndFTDescrs db user fileId (map fileDescrText rfds)
              case rfds of
                [] -> sendFileError (FileErrOther "no receiver descriptions") "no receiver descriptions" cxt ft
                rfd : _ -> case [fd | fd@(FD.ValidFileDescription FD.FileDescription {chunks = [_]}) <- rfds] of
                  [] -> case xftpRedirectFor of
                    Nothing -> xftpSndFileRedirect user fileId rfd >>= toView . CEvtSndFileRedirectStartXFTP user ft
                    Just _ -> sendFileError (FileErrOther "chaining redirects") "Prohibit chaining redirects" cxt ft
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
                        getChatItemByFileId db cxt user fileId
                      lift $ withAgent' (`xftpDeleteSndFileInternal` aFileId)
                      toView $ CEvtSndFileCompleteXFTP user ci' ft
                      where
                        getRecipients
                          | useRelays' g = withStore' $ \db -> getGroupRelayMembers db cxt user g
                          | otherwise = withStore' $ \db -> getGroupMembers db cxt user g
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
            lookupChatItemByFileId db cxt user fileId
          toView $ CEvtSndFileWarning user ci ft err
        SFERR e ->
          sendFileError (agentFileError e) (tshow e) cxt ft
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
            connDescrEvents :: Int -> NonEmpty (Connection, (ConnOrGroupId, Maybe MsgSigning, ChatMsgEvent 'Json))
            connDescrEvents partSize = L.fromList $ concatMap splitText (L.toList connsTransfersDescrs)
              where
                splitText :: (Connection, SndFileTransfer, RcvFileDescrText) -> [(Connection, (ConnOrGroupId, Maybe MsgSigning, ChatMsgEvent 'Json))]
                splitText (conn, _, rfdText) =
                  map (\fileDescr -> (conn, (connOrGroupId, Nothing, XMsgFileDescr {msgId = sharedMsgId, fileDescr}))) (L.toList $ splitFileDescr partSize rfdText)
            toMsgReq :: (Connection, (ConnOrGroupId, Maybe MsgSigning, ChatMsgEvent 'Json)) -> SndMessage -> ChatMsgReq
            toMsgReq (conn, _) SndMessage {msgId, msgBody} =
              (conn, MsgFlags {notification = hasNotification XMsgFileDescr_}, (vrValue msgBody, [msgId]))
        sendFileError :: FileError -> Text -> StoreCxt -> FileTransferMeta -> CM ()
        sendFileError ferr err cxt ft = do
          logError $ "Sent file error: " <> err
          ci <- withStore $ \db -> do
            liftIO $ updateFileCancelled db user fileId (CIFSSndError ferr)
            lookupChatItemByFileId db cxt user fileId
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
      cxt <- chatStoreCxt
      unless (rcvFileCompleteOrCancelled ft) $ case msg of
        RFPROG rcvProgress rcvTotal -> do
          let status = CIFSRcvTransfer {rcvProgress, rcvTotal}
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId status
            lookupChatItemByFileId db cxt user fileId
          toView $ CEvtRcvFileProgressXFTP user ci rcvProgress rcvTotal ft
        RFDONE xftpPath ->
          case liveRcvFileTransferPath ft of
            Nothing -> throwChatError $ CEInternalError "no target path for received XFTP file"
            Just targetPath -> do
              fsTargetPath <- lift $ toFSFilePath targetPath
              renameFile xftpPath fsTargetPath
              badDigest <- case ft of
                RcvFileTransfer {fileInvitation = FileInvitation {fileDigest = Just d}, cryptoArgs} ->
                  (/= d) <$> cryptoFileDigest (CryptoFile fsTargetPath cryptoArgs)
                _ -> pure False
              if badDigest
                then do
                  aci_ <- resetRcvCIFileStatus user fileId (CIFSRcvError $ FileErrOther "file digest")
                  forM_ aci_ cleanupACIFile
                  agentXFTPDeleteRcvFile aFileId fileId
                  forM_ aci_ $ \aci -> toView $ CEvtChatItemUpdated user aci
                else do
                  ci_ <- withStore $ \db -> do
                    liftIO $ do
                      updateRcvFileStatus db fileId FSComplete
                      updateCIFileStatus db user fileId CIFSRcvComplete
                    lookupChatItemByFileId db cxt user fileId
                  agentXFTPDeleteRcvFile aFileId fileId
                  toView $ maybe (CEvtRcvStandaloneFileComplete user fsTargetPath ft) (CEvtRcvFileComplete user) ci_
        RFWARN e -> do
          ci <- withStore $ \db -> do
            liftIO $ updateCIFileStatus db user fileId (CIFSRcvWarning $ agentFileError e)
            lookupChatItemByFileId db cxt user fileId
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
                lookupChatItemByFileId db cxt user fileId
              forM_ aci_ cleanupACIFile
              agentXFTPDeleteRcvFile aFileId fileId
              toView $ CEvtRcvFileError user aci_ e ft

type ShouldDeleteGroupConns = Bool

processAgentMessageConn :: StoreCxt -> User -> ACorrId -> ConnId -> AEvent 'AEConn -> CM ()
processAgentMessageConn cxt user@User {userId} corrId agentConnId agentMessage = do
  -- Missing connection/entity errors here will be sent to the view but not shown as CRITICAL alert,
  -- as in this case no need to ACK message - we can't process messages for this connection anyway.
  entity <- critical agentConnId $ withStore (\db -> getConnectionEntity db cxt user $ AgentConnId agentConnId) >>= updateConnStatus
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
    updateConnStatus acEntity = case agentMsgConnStatus (entityConnection acEntity) agentMessage of
      Just connStatus -> do
        let conn = (entityConnection acEntity) {connStatus}
        withStore' $ \db -> updateConnectionStatus db conn connStatus
        pure $ updateEntityConnStatus acEntity connStatus
      Nothing -> pure acEntity

    agentMsgConnStatus :: Connection -> AEvent e -> Maybe ConnStatus
    agentMsgConnStatus Connection {connStatus = cs} = \case
      JOINED True -> Just ConnSndReady
      CONF {} -> Just ConnRequested
      INFO {} -> Just ConnSndReady
      CON _ -> Just ConnReady
      ERR err | cs /= ConnReady && not (temporaryOrHostError err) -> Just $ ConnFailed (tshow err)
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
          chatRelayTests_ <- asks chatRelayTests
          relayTest_ <- atomically $ TM.lookup agentConnId chatRelayTests_
          case relayTest_ of
            Just RelayTest {challenge, rootKey, result = testVar} -> do
              r <- tryAllErrors $ do
                ChatMessage {chatMsgEvent} <- parseChatMessage conn connInfo
                case chatMsgEvent of
                  XGrpRelayTest _challenge sigBytes_ ->
                    case sigBytes_ of
                      Just sigBytes -> case C.decodeSignature sigBytes of
                        Right sig
                          | C.verify' rootKey sig challenge ->
                              atomically $ putTMVar testVar Nothing
                          | otherwise ->
                              atomically $ putTMVar testVar (Just $ RelayTestFailure RTSVerify (ChatError $ CERelayTestError "invalid signature"))
                        Left e ->
                          atomically $ putTMVar testVar (Just $ RelayTestFailure RTSVerify (ChatError $ CERelayTestError $ "signature decoding failed: " <> e))
                      Nothing ->
                        atomically $ putTMVar testVar (Just $ RelayTestFailure RTSVerify (ChatError $ CERelayTestError "no signature in response"))
                  _ ->
                    atomically $ putTMVar testVar (Just $ RelayTestFailure RTSWaitResponse (ChatError $ CERelayTestError "unexpected message type"))
              case r of
                Left e ->
                  atomically $ putTMVar testVar (Just $ RelayTestFailure RTSWaitResponse e)
                Right () -> pure ()
            Nothing -> do
              conn' <- processCONFpqSupport conn pqSupport
              -- [incognito] send saved profile
              (conn'', gInfo_) <- saveConnInfo conn' connInfo
              incognitoProfile <- forM customUserProfileId $ \profileId -> withStore (\db -> getProfileById db userId profileId)
              profileToSend <-
                presentUserBadge user incognitoProfile $ case gInfo_ of
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
        JOINED _ ->
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
        INV (ACR _ cReq) ->
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
              Right (APMsg _ (ParsedMsg _ _ chatMsg)) ->
                processEvent ct' conn' tags eInfo chatMsg `catchAllErrors` \e -> eToView e
              Left e -> do
                atomically $ modifyTVar' tags ("error" :)
                logInfo $ "contact msg=error " <> eInfo <> " " <> tshow e
                createInternalChatItem user (CDDirectRcv ct') (CIRcvMsgError $ RMEParseError $ T.pack e) Nothing
                  `catchAllErrors` \_ -> pure ()
            withRcpt <- checkSendRcpt ct' $ rights aChatMsgs -- not crucial to use ct'' from processEvent
            pure (withRcpt, False)
          where
            aChatMsgs = parseChatMessages msgBody
            processEvent :: Contact -> Connection -> TVar [Text] -> Text -> MsgEncodingI e => ChatMessage e -> CM ()
            processEvent ct' conn' tags eInfo chatMsg@ChatMessage {chatMsgEvent} = do
              let tag = toCMEventTag chatMsgEvent
              atomically $ modifyTVar' tags (tshow tag :)
              logInfo $ "contact msg=" <> tshow tag <> " " <> eInfo
              (conn'', msg@RcvMessage {chatMsgEvent = ACME _ event}) <- saveDirectRcvMSG conn' msgMeta chatMsg
              let ct'' = ct' {activeConn = Just conn''} :: Contact
              case event of
                XMsgNew mc -> newContentMessage ct'' mc msg msgMeta
                XMsgFileDescr sharedMsgId fileDescr -> messageFileDescription ct'' sharedMsgId fileDescr
                XMsgUpdate sharedMsgId mContent _ ttl live _msgScope _ -> messageUpdate ct'' sharedMsgId mContent msg msgMeta ttl live
                XMsgDel sharedMsgId _ _ _ -> messageDelete ct'' sharedMsgId msg msgMeta
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
            checkSendRcpt :: Contact -> [AParsedMsg] -> CM Bool
            checkSendRcpt ct' aMsgs = do
              let Contact {chatSettings = ChatSettings {sendRcpts}} = ct'
              pure $ fromMaybe (sendRcptsContacts user) sendRcpts && any aChatMsgHasReceipt aMsgs
              where
                aChatMsgHasReceipt (APMsg _ (ParsedMsg _ _ ChatMessage {chatMsgEvent})) =
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
              p <- presentUserBadge user incognitoProfile $ userProfileDirect user (fromLocalProfile <$> incognitoProfile) (Just ct') True
              allowAgentConnectionAsync user conn'' confId $ XInfo p
              void $ withStore' $ \db -> resetMemberContactFields db ct'
            XGrpLinkInv glInv -> do
              -- XGrpLinkInv here means we are connecting via business contact card, so we replace contact with group
              (gInfo, host) <- withStore $ \db -> do
                liftIO $ deleteContactCardKeepConn db connId ct
                createGroupInvitedViaLink db cxt user conn'' glInv
              void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing Nothing (Just epochStart)
              -- [incognito] send saved profile
              incognitoProfile <- forM customUserProfileId $ \pId -> withStore (\db -> getProfileById db userId pId)
              profileToSend <- presentUserBadge user incognitoProfile $ userProfileInGroup user gInfo (fromLocalProfile <$> incognitoProfile)
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
          let createE2EItem = createInternalChatItem user (CDDirectRcv ct') (CIRcvDirectE2EEInfo $ e2eInfoEncrypted $ Just pqEnc) Nothing
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
              groupInfo <- withStore $ \db -> getGroupInfo db cxt user groupId
              subMode <- chatReadVar subscriptionMode
              groupConnIds@(cmdId, grpConnId) <- prepareAgentCreation user CFCreateConnGrpInv True SCMInvitation
              gVar <- asks random
              withStore $ \db -> createNewContactMemberAsync db gVar user groupInfo ct' gLinkMemRole groupConnIds connChatVersion peerChatVRange subMode
              withAgent $ \a -> createConnectionAsync a (aCorrId cmdId) grpConnId True SCMInvitation CR.IKPQOff subMode
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
        JOINED sqSecured ->
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
        ERR (AGENT (A_DUPLICATE (Just DroppedMsg {brokerTs, attempts}))) ->
          createInternalChatItem user (CDDirectRcv ct) (CIRcvMsgError $ RMEDropped attempts) (Just brokerTs)
        ERR err -> do
          eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
          when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        -- TODO add debugging output
        _ -> pure ()
      where
        sendAutoReply ct mc = \case
          Just UserContactRequest {welcomeSharedMsgId = Just smId} ->
            void $ sendDirectContactMessage user ct $ XMsgUpdate smId mc M.empty Nothing Nothing Nothing Nothing
          _ -> do
            (msg, _) <- sendDirectContactMessage user ct $ XMsgNew $ mcSimple mc
            ci <- saveSndChatItem user (CDDirectSnd ct) msg (CISndMsgContent mc)
            toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDSnd (DirectChat ct) ci]

    processGroupMessage :: AEvent e -> ConnectionEntity -> Connection -> GroupInfo -> GroupMember -> CM ()
    processGroupMessage agentMsg connEntity conn@Connection {connId, connChatVersion, customUserProfileId, connectionCode} gInfo@GroupInfo {groupId, groupProfile, membership, chatSettings} m = case agentMsg of
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
                (ct, groupLinkId) <- withStore $ \db -> do
                  ct <- getContactViaMember db cxt user m
                  liftIO $ setNewContactMemberConnRequest db user m cReq
                  liftIO $ (ct,) <$> getGroupLinkId db user gInfo
                if memberRole' membership >= GRAdmin
                  then do
                    sendGrpInvitation ct m groupLinkId
                    toView $ CEvtSentGroupInvitation user gInfo ct m
                  else messageError "processGroupMessage: group link host no longer has admin role"
                where
                  sendGrpInvitation :: Contact -> GroupMember -> Maybe GroupLinkId -> CM ()
                  sendGrpInvitation ct GroupMember {memberId, memberRole = memRole} groupLinkId = do
                    let currentMemCount = fromIntegral $ currentMembers $ groupSummary gInfo
                        GroupMember {memberRole = userRole, memberId = userMemberId} = membership
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
              XGrpRelayAcpt relayLink relayCap
                | memberRole' membership == GROwner && isRelay m -> do
                    withStore' $ \db -> do
                      setRelayLinkConfId db m confId relayLink
                      updateRelayCapabilities db m relayCap
                    void $ getAgentConnShortLinkAsync user CFGetRelayDataAccept (Just conn') relayLink
                | otherwise -> messageError "x.grp.relay.acpt: only owner can add relay"
              XGrpRelayReject reason
                | memberRole' membership == GROwner && isRelay m -> do
                    -- GSMemLeft (not GSMemRejected): owner UI treats this identically to an explicit /leave from the relay; GSMemRejected has knocking-admission semantics.
                    (relay', m') <- withStore $ \db -> do
                      relay <- getGroupRelayByGMId db (groupMemberId' m)
                      relay' <- if relayStatus relay == RSInvited
                        then liftIO $ updateRelayStatusFromTo db relay RSInvited RSRejected
                        else pure relay
                      liftIO $ updateGroupMemberStatus db userId m GSMemLeft
                      pure (relay', m {memberStatus = GSMemLeft})
                    -- complete the contact handshake so the relay receives INFO and cleans up its transient bookkeeping
                    allowAgentConnectionAsync user conn' confId XOk
                    toView $ CEvtGroupRelayUpdated user gInfo m' relay'
                    toViewTE $ TERelayRejected user gInfo reason
                | otherwise -> messageError "x.grp.relay.reject: only owner should receive relay rejection"
              _ -> messageError "CONF from invited member must have x.grp.acpt"
          GCHostMember ->
            case chatMsgEvent of
              XGrpLinkInv glInv@GroupLinkInvitation {groupProfile = GroupProfile {publicGroup = rcvPG}}
                | let GroupInfo {groupProfile = GroupProfile {publicGroup = curPG}} = gInfo
                      pgId = fmap (\PublicGroupProfile {publicGroupId} -> publicGroupId),
                  useRelays' gInfo == isJust rcvPG && pgId rcvPG == pgId curPG -> do
                    -- XGrpLinkInv here means we are connecting via prepared group, and we have to update user and host member records
                    (gInfo', m') <- withStore $ \db -> updatePreparedUserAndHostMembersInvited db cxt user gInfo m glInv
                    -- [incognito] send saved profile
                    incognitoProfile <- forM customUserProfileId $ \pId -> withStore (\db -> getProfileById db userId pId)
                    profileToSend <- presentUserBadge user incognitoProfile $ userProfileInGroup user gInfo (fromLocalProfile <$> incognitoProfile)
                    allowAgentConnectionAsync user conn' confId $ XInfo profileToSend
                    toView $ CEvtGroupLinkConnecting user gInfo' m'
                | otherwise -> messageError "x.grp.link.inv: publicGroupId mismatch"
              XGrpLinkReject glRjct@GroupLinkRejection {rejectionReason} -> do
                (gInfo', m') <- withStore $ \db -> updatePreparedUserAndHostMembersRejected db cxt user gInfo m glRjct
                toView $ CEvtGroupLinkConnecting user gInfo' m'
                toViewTE $ TEGroupLinkRejected user gInfo' rejectionReason
              _ -> messageError "CONF from host member in prepared group must have x.grp.link.inv or x.grp.link.reject"
          _ ->
            case chatMsgEvent of
              XGrpMemInfo memId _memProfile
                | sameMemberId memId m -> do
                    let GroupMember {memberId = membershipMemId} = membership
                    membershipProfile <- presentUserBadge user (incognitoMembershipProfile gInfo) $ redactedMemberProfile gInfo membership $ fromLocalProfile $ memberProfile membership
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
          XOk ->
            -- transient relay-reject row cleanup after the rejection handshake completes
            when (memberCategory m == GCHostMember && not (relayServesGroup gInfo)) $ do
              deleteMemberConnection' m True
              withStore' $ \db -> do
                deleteGroupMember db user m
                deleteGroup db user gInfo
          _ -> messageError "INFO from member must have x.grp.mem.info, x.info or x.ok"
        pure ()
      CON _pqEnc -> unless rejected $ do
        -- TODO [knocking] send pending messages after accepting?
        -- possible improvement: check for each pending message, requires keeping track of connection state
        unless (connDisabled conn) $ sendPendingGroupMessages user gInfo m conn
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
            when (isRelay membership) $ do
              cc <- ask
              atomically $ channelProfileUpdated cc groupId groupProfile
            (gInfo'', m'', scopeInfo) <- mkGroupChatScope gInfo' m'
            -- Create e2ee, feature and group description chat items only on first connected relay
            ifM
              firstConnectedHost
              ( do
                  let cd = CDGroupRcv gInfo'' scopeInfo m''
                  createInternalChatItem user cd (CIRcvGroupE2EEInfo $ e2eInfoGroup gInfo'') Nothing
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
                    relayMems <- withStore' $ \db -> getGroupRelayMembers db cxt user gInfo
                    let numConnected = length $ filter (\GroupMember {memberStatus = ms} -> ms == GSMemConnected) relayMems
                    pure $ numConnected == 1
                | otherwise = pure True
          GCInviteeMember
            | isRelay m -> do
                withStore' $ \db -> updateGroupMemberStatus db userId m GSMemConnected
                if m `supportsVersion` groupRosterVersion
                  then do
                    -- send the relay a roster (materializing version 0 for old channels with NULL roster_version);
                    -- the relay stays RSInvited (unpublishable) until it acks, so no joiner can impersonate a privileged member
                    gInfo' <- case rosterVersion gInfo of
                      Just _ -> pure gInfo
                      Nothing -> do
                        withStore' $ \db -> setGroupRosterVersion db gInfo (VersionRoster 0)
                        pure gInfo {rosterVersion = Just (VersionRoster 0)}
                    sendGroupRosterToRelay user gInfo' m
                  else do
                    -- a relay below groupRosterVersion can't ack a roster; publish it on connect as before
                    -- the handshake (getPublishableGroupRelays and the LINK handler include/activate it by version)
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
                    introduceInChannel cxt user gInfo'' m'
                    when (groupFeatureAllowed SGFHistory gInfo'') $ sendHistory user gInfo'' m'
                  else case mStatus of
                    GSMemPendingApproval -> pure ()
                    GSMemPendingReview -> introduceToModerators cxt user gInfo'' m'
                    _ -> do
                      introduceToAll cxt user gInfo'' m'
                      let memberIsCustomer = case businessChat gInfo'' of
                            Just BusinessChatInfo {chatType = BCCustomer, customerId} -> memberId' m' == customerId
                            _ -> False
                      when (groupFeatureAllowed SGFHistory gInfo'' && not memberIsCustomer) $ sendHistory user gInfo'' m'
            where
              sendXGrpLinkMem gInfo'' = do
                let incognitoProfile = ExistingIncognito <$> incognitoMembershipProfile gInfo''
                profileToSend <- presentUserBadge user incognitoProfile $ userProfileInGroup user gInfo (fromIncognitoProfile <$> incognitoProfile)
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
                    host <- withStore $ \db -> getGroupMember db cxt user groupId hostId
                    forM_ (memberConn host) $ \hostConn ->
                      void $ sendDirectMemberMessage hostConn (XGrpMemCon memberId) groupId
                GCPostMember ->
                  forM_ (invitedByGroupMemberId m) $ \invitingMemberId -> do
                    im <- withStore $ \db -> getGroupMember db cxt user groupId invitingMemberId
                    forM_ (memberConn im) $ \imConn ->
                      void $ sendDirectMemberMessage imConn (XGrpMemCon memberId) groupId
                _ -> messageWarning "sendXGrpMemCon: member category GCPreMember or GCPostMember is expected"
        where
          rejected =
            memberStatus m `elem` ([GSMemRejected, GSMemLeft, GSMemRemoved, GSMemGroupDeleted] :: [GroupMemberStatus])
              || memberStatus membership == GSMemRejected
              || not (relayServesGroup gInfo)
      MSG msgMeta _msgFlags msgBody -> do
        tags <- newTVarIO []
        withAckMessage "group msg" agentConnId msgMeta True (Just tags) $ \eInfo -> do
          -- possible improvement is to choose scope based on event (some events specify scope)
          (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
          checkIntegrityCreateItem (CDGroupRcv gInfo' scopeInfo m') msgMeta `catchAllErrors` \_ -> pure ()
          newDeliveryTasks <- reverse <$> foldM (processAChatMsg gInfo' scopeInfo m' tags eInfo) [] aChatMsgs
          shouldDelConns <-
            if isUserGrpFwdRelay gInfo' && not (blockedByAdmin m)
              then
                let tasks
                      | not (relayServesGroup gInfo') = filter relayRemovedNewTask newDeliveryTasks
                      | otherwise = newDeliveryTasks
                 in createDeliveryTasks gInfo' m' tasks
              else pure False
          withRcpt <- checkSendRcpt $ rights aChatMsgs
          pure (withRcpt, shouldDelConns)
        where
          aChatMsgs = parseChatMessages msgBody
          brokerTs = metaBrokerTs msgMeta
          processAChatMsg ::
            GroupInfo ->
            Maybe GroupChatScopeInfo ->
            GroupMember ->
            TVar [Text] ->
            Text ->
            [NewMessageDeliveryTask] ->
            Either String AParsedMsg ->
            CM [NewMessageDeliveryTask]
          processAChatMsg gInfo' scopeInfo m' tags eInfo newDeliveryTasks = \case
            Right (APMsg enc (parsedMsg@(ParsedMsg fwd_ _ ChatMessage {chatMsgEvent}))) -> do
              let tag = toCMEventTag chatMsgEvent
              atomically $ modifyTVar' tags (tshow tag :)
              case fwd_ of
                Just fwd | SJson <- enc -> do
                  logInfo $ "group fwd=" <> tshow tag <> " " <> eInfo
                  xGrpMsgForward gInfo' scopeInfo m' fwd parsedMsg brokerTs
                    `catchAllErrors` \e -> eToView e
                  pure newDeliveryTasks
                -- direct JSON and binary messages; binary events don't produce delivery tasks
                _ -> do
                  logInfo $ "group msg=" <> tshow tag <> " " <> eInfo
                  newTask_ <- join <$> withVerifiedMsg gInfo' scopeInfo m' parsedMsg brokerTs
                    (\verifiedMsg -> processEvent gInfo' m' verifiedMsg `catchAllErrors` \e -> eToView e $> Nothing)
                  pure $ maybe id (:) newTask_ newDeliveryTasks
            Left e -> do
              atomically $ modifyTVar' tags ("error" :)
              logInfo $ "group msg=error " <> eInfo <> " " <> tshow e
              if isRelay membership
                then
                  eToView (ChatError . CEException $ "error parsing chat message: " <> e)
                else
                  createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m') (CIRcvMsgError $ RMEParseError $ T.pack e) Nothing
                    `catchAllErrors` \_ -> pure ()
              pure newDeliveryTasks
          processEvent :: forall e. MsgEncodingI e => GroupInfo -> GroupMember -> VerifiedMsg e -> CM (Maybe NewMessageDeliveryTask)
          processEvent gInfo' m' verifiedMsg = do
            cc <- ask
            (m'', conn', msg@RcvMessage {msgId, sharedMsgId_, chatMsgEvent = ACME _ event}) <- saveGroupRcvMsg user groupId m' conn msgMeta verifiedMsg
            let ctx js = DeliveryTaskContext js False
                checkSendAsGroup :: Maybe Bool -> CM (Maybe DeliveryTaskContext) -> CM (Maybe DeliveryTaskContext)
                checkSendAsGroup asGroup_ a
                  | asGroup_ == Just True && memberRole' m'' < GROwner =
                      messageError "member is not allowed to send as group" $> Nothing
                  | otherwise = a
            -- ! see isForwardedGroupMsg: processing functions should return DeliveryJobScope for same events
            deliveryTaskContext_ <- case event of
              XMsgNew mc ->
                checkSendAsGroup asGroup $
                  memberCanSend (Just m'') scope $ newGroupContentMessage gInfo' (Just m'') mc msg brokerTs False
                where
                  MsgContainer {scope, asGroup} = mc
              -- file description is always allowed, to allow sending files to support scope
              XMsgFileDescr sharedMsgId fileDescr -> groupMessageFileDescription gInfo' (Just m'') sharedMsgId fileDescr
              XMsgUpdate sharedMsgId mContent mentions ttl live msgScope asGroup_ ->
                checkSendAsGroup asGroup_ $
                  memberCanSend (Just m'') msgScope $
                    groupMessageUpdate gInfo' (Just m'') sharedMsgId mContent mentions msgScope msg brokerTs ttl live asGroup_
              XMsgDel sharedMsgId memberId_ scope_ onlyHistory ->
                groupMessageDelete gInfo' (Just m'') sharedMsgId memberId_ scope_ onlyHistory msg brokerTs
              XMsgReact sharedMsgId memberId scope_ reaction add -> groupMsgReaction gInfo' m'' sharedMsgId memberId scope_ reaction add msg brokerTs
              -- TODO discontinue XFile
              XFile fInv -> Nothing <$ processGroupFileInvitation' gInfo' m'' fInv msg brokerTs
              XFileCancel sharedMsgId -> xFileCancelGroup gInfo' (Just m'') sharedMsgId
              XFileAcptInv sharedMsgId fileConnReq_ fName -> Nothing <$ xFileAcptInvGroup gInfo' m'' sharedMsgId fileConnReq_ fName
              XInfo p -> fmap ctx <$> xInfoMember gInfo' m'' p msg brokerTs
              XGrpLinkMem p -> Nothing <$ xGrpLinkMem gInfo' m'' conn' p
              XGrpLinkAcpt acceptance role memberId -> Nothing <$ xGrpLinkAcpt gInfo' m'' acceptance role memberId msg brokerTs
              XGrpRelayNew rl -> fmap ctx <$> xGrpRelayNew gInfo' m'' rl
              XGrpRelayCap relayCap
                | memberRole' membership == GROwner && isRelay m'' ->
                    Nothing <$ withStore' (\db -> updateRelayCapabilities db m'' relayCap)
                | otherwise -> Nothing <$ messageWarning "x.grp.relay.cap: only owner should receive relay capabilities"
              XGrpMemNew memInfo msgScope -> fmap ctx <$> xGrpMemNew gInfo' m'' memInfo msgScope msg brokerTs
              XGrpMemIntro memInfo memRestrictions_ -> Nothing <$ xGrpMemIntro gInfo' m'' memInfo memRestrictions_
              XGrpMemInv memId introInv -> Nothing <$ xGrpMemInv gInfo' m'' memId introInv
              XGrpMemFwd memInfo introInv -> Nothing <$ xGrpMemFwd gInfo' m'' memInfo introInv
              XGrpMemRole memId memRole memberKey rosterVer -> fmap ctx <$> xGrpMemRole gInfo' Nothing m'' memId memRole memberKey rosterVer msg brokerTs
              XGrpMemRestrict memId memRestrictions -> fmap ctx <$> xGrpMemRestrict gInfo' m'' memId memRestrictions msg brokerTs
              XGrpMemCon memId -> Nothing <$ xGrpMemCon gInfo' m'' memId
              XGrpMemDel memId withMessages rosterVer -> case encoding @e of
                SJson -> fmap ctx <$> xGrpMemDel gInfo' Nothing m'' memId withMessages rosterVer verifiedMsg msg brokerTs False
                SBinary -> pure Nothing
              XGrpLeave -> fmap ctx <$> xGrpLeave gInfo' m'' msg brokerTs
              XGrpDel -> Just (DeliveryTaskContext (DJSGroup {jobSpec = DJRelayRemoved}) False) <$ xGrpDel gInfo' m'' msg brokerTs
              XGrpInfo p' -> fmap ctx <$> xGrpInfo gInfo' m'' p' msg brokerTs
              XGrpPrefs ps' -> fmap ctx <$> xGrpPrefs gInfo' m'' ps' msg
              XGrpRoster gr -> fmap ctx <$> xGrpRoster gInfo' m'' m'' gr verifiedMsg sharedMsgId_ brokerTs
              XGrpRosterAck ackVer ackErr -> Nothing <$ xGrpRosterAck gInfo' m'' ackVer ackErr
              XGrpRosterRequest reqVer -> Nothing <$ xGrpRosterRequest gInfo' m'' reqVer
              -- TODO [knocking] why don't we forward these messages?
              XGrpDirectInv connReq mContent_ msgScope -> memberCanSend (Just m'') msgScope $ Nothing <$ xGrpDirectInv gInfo' m'' conn' connReq mContent_ msg brokerTs
              XGrpMsgForward fwd msg' -> Nothing <$ xGrpMsgForward gInfo' Nothing m'' fwd (ParsedMsg Nothing Nothing msg') brokerTs
              XInfoProbe probe -> Nothing <$ xInfoProbe (COMGroupMember m'') probe
              XInfoProbeCheck probeHash -> Nothing <$ xInfoProbeCheck (COMGroupMember m'') probeHash
              XInfoProbeOk probe -> Nothing <$ xInfoProbeOk (COMGroupMember m'') probe
              BFileChunk sharedMsgId chunk -> Nothing <$ bFileChunkGroup gInfo' m'' sharedMsgId chunk msgMeta
              _ -> Nothing <$ messageError ("unsupported message: " <> tshow event)
            forM deliveryTaskContext_ $ \taskContext -> do
              let contentChanged :: CM ()
                  contentChanged = atomically $ channelContentChanged cc groupId
              case event of
                XMsgNew {} -> contentChanged
                XMsgUpdate {} -> contentChanged
                XMsgDel {} -> contentChanged
                XMsgReact {} -> contentChanged
                XGrpInfo p' -> atomically $ channelProfileUpdated cc groupId p'
                XGrpDel {} -> atomically $ channelRemoved cc groupId
                _ -> pure ()
              pure $ NewMessageDeliveryTask {messageId = msgId, taskContext}
          checkSendRcpt :: [AParsedMsg] -> CM Bool
          checkSendRcpt aMsgs = do
            let currentMemCount = fromIntegral $ currentMembers $ groupSummary gInfo
                GroupInfo {chatSettings = ChatSettings {sendRcpts}} = gInfo
            pure $
              fromMaybe (sendRcptsSmallGroups user) sendRcpts
                && any aChatMsgHasReceipt aMsgs
                && currentMemCount <= smallGroupsRcptsMemLimit
            where
              aChatMsgHasReceipt (APMsg _ (ParsedMsg _ _ ChatMessage {chatMsgEvent})) =
                hasDeliveryReceipt (toCMEventTag chatMsgEvent)
          relayRemovedNewTask :: NewMessageDeliveryTask -> Bool
          relayRemovedNewTask NewMessageDeliveryTask {taskContext = DeliveryTaskContext {jobScope}} = isRelayRemoved jobScope
          createDeliveryTasks :: GroupInfo -> GroupMember -> [NewMessageDeliveryTask] -> CM ShouldDeleteGroupConns
          createDeliveryTasks gInfo'@GroupInfo {groupId = gId} m' newDeliveryTasks = do
            let relayRemovedTask_ = find (\NewMessageDeliveryTask {taskContext = DeliveryTaskContext {jobScope}} -> isRelayRemoved jobScope) newDeliveryTasks
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
                let workerScopes = map (\NewMessageDeliveryTask {taskContext = DeliveryTaskContext {jobScope}} -> toWorkerScope jobScope) createdDeliveryTasks
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
        when continued $ do
          when (isUserGrpFwdRelay gInfo) $ serveRoster user gInfo m -- roster ahead of the resumed backlog
          sendPendingGroupMessages user gInfo m conn
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
      JOINED sqSecured ->
        -- [async agent commands] continuation on receiving JOINED
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData ->
          when (sqSecured && connChatVersion >= batchSend2Version) $ do
            mc_ <- getAutoReplyMsg
            forM_ mc_ $ \mc -> do
              connReq_ <- withStore' $ \db -> getBusinessContactRequest db user groupId
              sendGroupAutoReply mc connReq_
      LDATA FixedLinkData {linkConnReq = cReq, rootKey = relayKey, linkEntityId} cData ->
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cmdFunction of
            CFGetRelayDataJoin -> do
              -- Update relay member with key, memberId and profile from link
              relayLinkData_ <- liftIO $ decodeLinkUserData cData
              relayMemberId <- case (relayLinkData_, linkEntityId) of
                (Just RelayShortLinkData {relayProfile = p}, Just entityId) -> do
                  withStore $ \db -> updateRelayMemberData db cxt user m (MemberId entityId) (MemberKey relayKey) p
                  pure $ MemberId entityId
                _ -> throwChatError $ CEException "relay link: no relay link data or entity id"
              case cReq of
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
                      let incognitoProfile = fromLocalProfile <$> incognitoMembershipProfile gInfo
                      profileToSend <- presentUserBadge user incognitoProfile $ userProfileInGroup user gInfo incognitoProfile
                      dm <- encodeXMemberConnInfo gInfo relayMemberId profileToSend
                      subMode <- chatReadVar subscriptionMode
                      (cmdId, connId') <- prepareAgentJoin user (Just conn) True cReq
                      joinAgentConnectionAsync cmdId True connId' True cReq dm subMode
            CFGetRelayDataAccept -> do
              let GroupMember {memberId = MemberId expectedMemberId} = m
              if linkEntityId == Just expectedMemberId
                then do
                  relayProfile <- liftIO (decodeLinkUserData cData) >>= \case
                    Just RelayShortLinkData {relayProfile = p} -> pure p
                    Nothing -> throwChatError $ CEException "relay link: no relay link data"
                  (confId, m', relay)  <- withStore $ \db -> do
                    confId <- getRelayConfId db m
                    liftIO $ updateGroupMemberStatus db userId m GSMemAccepted
                    (m', relay) <- setRelayLinkAccepted db cxt user m (MemberKey relayKey) relayProfile
                    pure (confId, m', relay)
                  allowAgentConnectionAsync user conn confId XOk
                  toView $ CEvtGroupRelayUpdated user gInfo m' relay
                else
                  -- TODO [relays] owner: TBC failed RelayStatus?
                  messageError "relay link: relay member ID mismatch"
            _ -> throwChatError $ CECommandError "unexpected cmdFunction"
      QCONT -> do
        continued <- continueSending connEntity conn
        when continued $ do
          when (isUserGrpFwdRelay gInfo) $ serveRoster user gInfo m -- roster ahead of the resumed backlog
          sendPendingGroupMessages user gInfo m conn
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
      ERR err@(AGENT (A_DUPLICATE (Just DroppedMsg {brokerTs, attempts})))
        | isRelay membership ->
            eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        | otherwise -> do
            (gInfo', m', scopeInfo) <- mkGroupChatScope gInfo m
            createInternalChatItem user (CDGroupRcv gInfo' scopeInfo m') (CIRcvMsgError $ RMEDropped attempts) (Just brokerTs)
      ERR err -> do
        eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
        when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
        when (isConnFailed $ connStatus conn) $
          toView $ CEvtGroupMemberUpdated user gInfo m m
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
            void $ sendGroupMessage' user gInfo [m] $ XMsgUpdate smId mc M.empty Nothing Nothing Nothing Nothing
          _ -> do
            msg <- sendGroupMessage' user gInfo [m] $ XMsgNew $ mcSimple mc
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

    receiveFileChunk :: Maybe GroupInfo -> RcvFileTransfer -> Maybe Connection -> MsgMeta -> FileChunk -> CM ()
    receiveFileChunk gInfo_ ft@RcvFileTransfer {fileId, fileType, chunkSize} conn_ MsgMeta {recipient = (msgId, _), integrity} = \case
      FileChunkCancel -> case fileType of
        -- cancel only this source's transfer; other relays' in-flight transfers are independent
        FTRoster -> do
          t_ <- withStore' $ \db -> getRosterTransfer db fileId
          forM_ t_ $ \RcvRosterTransfer {rosterTransferId} -> cleanupRosterTransferById rosterTransferId
        FTNormal ->
          unless (rcvFileCompleteOrCancelled ft) $ do
            cancelRcvFileTransfer user ft
            ci <- withStore $ \db -> getChatItemByFileId db cxt user fileId
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
              else appendFileChunk ft chunkNo chunk False
          RcvChunkFinal ->
            if B.length chunk > fromInteger chunkSize
              then badRcvFileChunk ft "incorrect chunk size"
              else do
                appendFileChunk ft chunkNo chunk True
                case fileType of
                  FTRoster -> forM_ gInfo_ $ \gInfo -> rosterCompletion gInfo ft
                  FTNormal -> do
                    ci <- withStore $ \db -> do
                      liftIO $ do
                        updateRcvFileStatus db fileId FSComplete
                        updateCIFileStatus db user fileId CIFSRcvComplete
                        deleteRcvFileChunks db ft
                      getChatItemByFileId db cxt user fileId
                    toView $ CEvtRcvFileComplete user ci
                    mapM_ (deleteAgentConnectionAsync . aConnId) conn_
          RcvChunkDuplicate -> pure ()
          RcvChunkError -> badRcvFileChunk ft $ "incorrect chunk number " <> show chunkNo

    processContactConnMessage :: AEvent e -> ConnectionEntity -> Connection -> UserContact -> CM ()
    processContactConnMessage agentMsg connEntity conn UserContact {userContactLinkId = uclId, groupId = ucGroupId_} = case agentMsg of
      REQ invId pqSupport _ connInfo -> do
        (signedMsg_, ChatMessage {chatVRange, chatMsgEvent}) <- parseChatMessage' conn connInfo
        case chatMsgEvent of
          XContact p xContactId_ welcomeMsgId_ requestMsg_ -> profileContactRequest invId chatVRange p xContactId_ welcomeMsgId_ requestMsg_ pqSupport
          XMember p joiningMemberId joiningMemberKey viaRelay -> memberJoinRequestViaRelay invId chatVRange signedMsg_ p joiningMemberId joiningMemberKey viaRelay
          XInfo p -> profileContactRequest invId chatVRange p Nothing Nothing Nothing pqSupport
          XGrpRelayInv groupRelayInv -> xGrpRelayInv invId chatVRange groupRelayInv
          XGrpRelayTest challenge _ -> xGrpRelayTest invId chatVRange challenge
          -- TODO show/log error, other events in contact request
          _ -> pure ()
      LINK _link auData ->
        withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
          case cmdFunction of
            CFSetShortLink ->
              case (ucGroupId_, auData) of
                (Just groupId, UserContactLinkData UserContactData {relays = relayLinks}) -> do
                  (gInfo, gLink, relays, relaysChanged, newlyActiveLinks) <- withStore $ \db -> do
                    gInfo <- getGroupInfo db cxt user groupId
                    gLink <- getGroupLink db user gInfo
                    relays <- liftIO $ getGroupRelays db gInfo
                    (relays', changed, newlyActiveLinks) <- liftIO $ foldrM (updateRelay db) ([], False, []) relays
                    liftIO $ setGroupInProgressDone db gInfo
                    pure (gInfo, gLink, relays', changed, newlyActiveLinks)
                  toView $ CEvtGroupLinkDataUpdated user gInfo gLink relays relaysChanged
                  let GroupSummary {publicMemberCount} = groupSummary gInfo
                  -- Owner is counted in publicMemberCount; > 1 means at least one subscriber.
                  -- TODO [relays] multi-owner: with N owners, threshold should be > N (or use a
                  -- dedicated subscriber count).
                  when (fromMaybe 0 publicMemberCount > 1) $
                    forM_ (L.nonEmpty newlyActiveLinks) $ \newlyActive -> do
                      allRelayMembers <- withFastStore' $ \db -> getGroupRelayMembers db cxt user gInfo
                      let recipients =
                            filter
                              (\GroupMember {memberStatus, relayLink} ->
                                 memberStatus == GSMemConnected && relayLink `notElem` map Just newlyActiveLinks)
                              allRelayMembers
                          events = XGrpRelayNew <$> newlyActive
                      unless (null recipients) $
                        void $ sendGroupMessages user gInfo Nothing False recipients False events
                  where
                    updateRelay :: DB.Connection -> GroupRelay -> ([GroupRelay], Bool, [ShortLinkContact]) -> IO ([GroupRelay], Bool, [ShortLinkContact])
                    updateRelay db relay@GroupRelay {relayLink, relayStatus} (acc, changed, newlyActiveLinks) =
                      case relayLink of
                        Just rLink
                          -- version is gated upstream at publish (getPublishableGroupRelays): an RSAccepted relay
                          -- whose link is in the published data is necessarily pre-roster, so activate it too
                          | rLink `elem` relayLinks && (relayStatus == RSAcknowledgedRoster || relayStatus == RSAccepted) -> do
                              relay' <- updateRelayStatus db relay RSActive
                              pure (relay' : acc, True, rLink : newlyActiveLinks)
                          | rLink `elem` relayLinks -> pure (relay : acc, changed, newlyActiveLinks)
                          | relayStatus == RSActive -> do
                              -- Relay link absent from link data — deactivate.
                              -- RSAccepted relays are not deactivated: their own link data update
                              -- may not have been processed yet (race with concurrent relay connections).
                              -- TODO [relays] multi-owner: Another owner removing a relay updates link data on
                              -- TODO   the SMP server, but this owner won't receive a LINK callback for it
                              -- TODO   (LINK only fires in response to own setConnShortLink calls).
                              relay' <- updateRelayStatus db relay RSInactive
                              pure (relay' : acc, True, newlyActiveLinks)
                        _ -> pure (relay : acc, changed, newlyActiveLinks)
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
              withStore (\db -> createOrUpdateContactRequest db gVar cxt user uclId ucl isSimplexTeam invId chatVRange p xContactId_ welcomeMsgId_ requestMsg_ reqPQSup) >>= \case
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
                        void $ createChatItem user (CDDirectSnd ct) False CIChatBanner Nothing Nothing (Just epochStart)
                        let e2eContent = CIRcvDirectE2EEInfo $ e2eInfoEncrypted $ Just $ CR.pqSupportToEnc $ reqPQSup
                        void $ createChatItem user cd False e2eContent Nothing Nothing Nothing
                        void $ createFeatureEnabledItems_ user ct
                        forM_ (autoReply addressSettings) $ \mc -> forM_ welcomeSharedMsgId $ \sharedMsgId ->
                          createChatItem user (CDDirectSnd ct) False (CISndMsgContent mc) (Just sharedMsgId) Nothing Nothing
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
                        void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing Nothing (Just epochStart)
                        -- TODO [short links] possibly, we can just keep them created where they are created on the business side due to auto-accept
                        -- let e2eContent = CIRcvGroupE2EEInfo $ E2EInfo $ Just False -- no PQ encryption in groups
                        -- void $ createChatItem user cd False e2eContent Nothing Nothing Nothing
                        -- void $ createFeatureEnabledItems_ user ct
                        forM_ (autoReply addressSettings) $ \arMC -> forM_ welcomeSharedMsgId $ \sharedMsgId ->
                          createChatItem user (CDGroupSnd gInfo Nothing) False (CISndMsgContent arMC) (Just sharedMsgId) Nothing Nothing
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
                      withStore (\db -> getGroupChatItemBySharedMsgId db user gInfo (Just $ groupMemberId' clientMember) sharedMsgId) >>= \case
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
                  aci <- createChatItem user cd False (CIRcvMsgContent mc) (Just sharedMsgId) Nothing Nothing
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
              gInfo <- withStore $ \db -> getGroupInfo db cxt user groupId
              if
                | useRelays' gInfo ->
                    messageWarning $ "processContactConnMessage (group " <> groupName' gInfo <> "): ignored direct join request from " <> displayName <> " (group uses relays)"
                | memberRole' (membership gInfo) < GRAdmin ->
                    messageWarning $ "processContactConnMessage (group " <> groupName' gInfo <> "): ignored join request because host is no longer admin"
                | otherwise -> do
                  acceptMember_ <- asks $ acceptMember . chatHooks . config
                  maybe (pure $ Right (GAAccepted, gLinkMemRole)) (\am -> liftIO $ am gInfo gli p) acceptMember_ >>= \case
                    Right (acceptance, useRole)
                      | v < groupFastLinkJoinVersion ->
                          messageError "processContactConnMessage: chat version range incompatible for accepting group join request"
                      | otherwise -> do
                          let profileMode = ExistingIncognito <$> incognitoMembershipProfile gInfo
                          mem <- acceptGroupJoinRequestAsync user uclId gInfo invId chatVRange p xContactId_ Nothing welcomeMsgId_ acceptance useRole profileMode Nothing Nothing
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
        xGrpRelayInv invId chatVRange groupRelayInv@GroupRelayInvitation {groupLink} = do
          rejected <- withStore' $ \db -> isRelayGroupRejected db user groupLink
          initialDelay <- asks $ initialInterval . relayRequestRetryInterval . config
          if rejected
            then rejectRelayInvitationAsync user uclId cxt groupRelayInv invId chatVRange initialDelay RRRRejoinRejected
            else do
              (_gInfo, _ownerMember) <- withStore $ \db ->
                createRelayRequestGroup db cxt user groupRelayInv invId chatVRange initialDelay GSMemAccepted RSInvited
              lift $ void $ getRelayRequestWorker True
        xGrpRelayTest :: InvitationId -> VersionRangeChat -> ByteString -> CM ()
        xGrpRelayTest invId chatVRange challenge
          | isTrue userChatRelay && isNothing ucGroupId_ =
              withAgent (`getConnLinkPrivKey` aConnId conn) >>= \case
                Nothing -> eToView $ ChatError (CEInternalError "no short link key for relay address")
                Just privKey -> do
                  let sig = C.signatureBytes $ C.sign' privKey challenge
                      msg = XGrpRelayTest challenge (Just sig)
                  subMode <- chatReadVar subscriptionMode
                  let chatV = vr cxt `peerConnChatVersion` chatVRange
                  (cmdId, acId) <- prepareAgentAccept user True invId PQSupportOff
                  withStore $ \db -> do
                    Connection {connId = testCId} <- createRelayTestConnection db cxt user acId ConnAccepted chatV subMode
                    liftIO $ setCommandConnId db user cmdId testCId
                  agentAcceptContactAsync cmdId acId True invId msg PQSupportOff chatV subMode
          | otherwise = messageError "relay test sent to non-relay link"
            where
              User {userChatRelay} = user
        -- TODO [relays] owner, relays: TBC how to communicate member rejection rules from owner to relays
        memberJoinRequestViaRelay :: InvitationId -> VersionRangeChat -> Maybe SignedMsg -> Profile -> MemberId -> MemberKey -> Maybe MemberId -> CM ()
        memberJoinRequestViaRelay invId chatVRange signedMsg_ p joiningMemberId joiningMemberKey@(MemberKey joiningKey) viaRelay = do
          (_ucl, gLinkInfo_) <- withStore $ \db -> getUserContactLinkById db userId uclId
          case gLinkInfo_ of
            Just GroupLinkInfo {groupId, memberRole = gLinkMemRole} -> do
              gInfo <- withStore $ \db -> getGroupInfo db cxt user groupId
              existing_ <- withStore' $ \db -> eitherToMaybe <$> runExceptT (getGroupMemberByMemberId db cxt user gInfo joiningMemberId)
              case existing_ of
                Just rosterMem
                  -- a privileged memberId's key is owner-authoritative (the roster); the joiner must prove
                  -- possession of that exact key, otherwise this is an attempt to impersonate it
                  | isRosterRole (memberRole' rosterMem) ->
                      if verifyKey gInfo rosterMem
                        then acceptJoin gInfo (Just rosterMem) (memberRole' rosterMem)
                        else messageError "memberJoinRequestViaRelay: rejected join claiming privileged memberId (key mismatch or invalid signature)"
                _ -> acceptJoin gInfo Nothing gLinkMemRole
            Nothing ->
              messageError "memberJoinRequestViaRelay: no group link info for relay link"
          where
            -- replay defense: the viaRelay == own memberId check (viaRelay is in the signed body); without it a sibling relay could replay a privileged member's signed join
            verifyKey gInfo rosterMem = case (signedMsg_, groupKeys gInfo) of
              (Just SignedMsg {chatBinding = CBGroup, signatures, signedBody}, Just GroupKeys {publicGroupId}) ->
                memberPubKey rosterMem == Just joiningKey
                  && verifyGroupSig joiningKey publicGroupId joiningMemberId signatures signedBody
                  && viaRelay == Just (memberId' (membership gInfo))
              _ -> False
            acceptJoin gInfo existingMem_ acceptRole = do
              mem <- acceptGroupJoinRequestAsync user uclId gInfo invId chatVRange p Nothing (Just joiningMemberId) Nothing GAAccepted acceptRole Nothing (Just joiningMemberKey) existingMem_
              (gInfo', mem', scopeInfo) <- mkGroupChatScope gInfo mem
              createInternalChatItem user (CDGroupRcv gInfo' scopeInfo mem') (CIRcvGroupEvent RGEInvitedViaGroupLink) Nothing
              toView $ CEvtAcceptingGroupJoinRequestMember user gInfo' mem'

    muteEventInChannel :: GroupInfo -> GroupMember -> Bool
    muteEventInChannel gInfo@GroupInfo {membership} m =
      useRelays' gInfo
        && not (isRelay membership) -- relay users see all events
        && not (isRelay m) -- relay events (e.g. leave) are visible to all
        && memberRole' membership < GRModerator
        && memberRole' m < GRModerator

    memberCanSend :: Maybe GroupMember -> Maybe MsgScope -> CM (Maybe DeliveryTaskContext) -> CM (Maybe DeliveryTaskContext)
    memberCanSend Nothing _ a = a -- channel message - was previously checked and allowed by relay
    memberCanSend (Just m@GroupMember {memberRole}) msgScope a = case msgScope of
      Just (MSMember mId)
        | sameMemberId mId m || memberRole >= GRModerator -> a
        | otherwise -> messageError "member is not allowed to send to this support chat" $> Nothing
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
        Left (ChatErrorStore SEDBBusyError {message}) | showCritical -> throwError $ chatErrorAgent $ CRITICAL True message
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
          ms <- map COMGroupMember <$> withStore' (\db -> getMatchingMembers db cxt user ct)
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
          cs <- map COMContact <$> withStore' (\db -> getMatchingMemberContacts db cxt user m)
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
      let MsgContainer {content = c, file = fInv_} = mc
      content <- case c of
        MCChat {text, chatLink, ownerSig = Just LinkOwnerSig {chatBinding = B64UrlByteString binding}} -> do
          keepSig <- case contactConn ct of
            Nothing -> pure False
            Just conn -> do
              adHash <- withAgent (`getConnectionRatchetAdHash` aConnId conn)
              pure $ encodeChatBinding CBDirect adHash == binding
          pure $ if keepSig then c else MCChat {text, chatLink, ownerSig = Nothing}
        _ -> pure c
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
          let MsgContainer {ttl = itemTTL, live = live_} = mc
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
        aci <- getChatItemByFileId db cxt user fileId
        pure (fileId, aci)
      processFDMessage fileId aci fileDescr

    groupMessageFileDescription :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> FileDescr -> CM (Maybe DeliveryTaskContext)
    groupMessageFileDescription g@GroupInfo {groupId} m_ sharedMsgId fileDescr = do
      (fileId, aci) <- withStore $ \db -> do
        fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
        aci <- getChatItemByFileId db cxt user fileId
        pure (fileId, aci)
      case aci of
        AChatItem SCTGroup SMDRcv (GroupChat _g scopeInfo) ChatItem {chatDir}
          | validSender m_ chatDir -> do
              -- in processFDMessage some paths are programmed as errors,
              -- for example failure on not approved relays (CEFileNotApproved).
              -- we catch error, so that even if processFDMessage fails, message can still be forwarded.
              processFDMessage fileId aci fileDescr `catchAllErrors` \_ -> pure ()
              pure $ Just $ infoToDeliveryContext g scopeInfo (isChannelDir chatDir)
          | otherwise -> messageError "x.msg.file.descr: file/sender mismatch" $> Nothing
        _ -> messageError "x.msg.file.descr: invalid file description part" $> Nothing

    processFDMessage :: FileTransferId -> AChatItem -> FileDescr -> CM ()
    processFDMessage fileId aci fileDescr = do
      ft <- withStore $ \db -> getRcvFileTransfer db user fileId
      unless (rcvFileCompleteOrCancelled ft) $ do
        (rfd@RcvFileDescr {fileDescrComplete}, ft'@RcvFileTransfer {fileStatus, xftpRcvFile, cryptoArgs, fileInvitation = FileInvitation {fileSize}}) <- withStore $ \db -> do
          rfd <- appendRcvFD db userId fileId fileDescr
          -- reading second time in the same transaction as appending description
          -- to prevent race condition with accept
          ft' <- getRcvFileTransfer db user fileId
          pure (rfd, ft')
        when fileDescrComplete $ toView $ CEvtRcvFileDescrReady user aci ft' rfd
        case (fileStatus, xftpRcvFile) of
          (RFSAccepted _, Just XFTPRcvFile {userApprovedRelays}) -> receiveViaCompleteFD user fileId rfd fileSize userApprovedRelays cryptoArgs
          _ -> pure ()

    processFileInvitation :: Maybe FileInvitation -> MsgContent -> (DB.Connection -> FileInvitation -> Maybe InlineFileMode -> Integer -> ExceptT StoreError IO RcvFileTransfer) -> CM (Maybe (RcvFileTransfer, CIFile 'MDRcv))
    processFileInvitation fInv_ mc createRcvFT = forM fInv_ $ \fInv -> do
      ChatConfig {fileChunkSize} <- asks config
      fInv'@FileInvitation {fileName, fileSize} <- validateFileInvitation fInv
      inline <- receiveInlineMode fInv' (Just mc) fileChunkSize
      ft@RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFT db fInv' inline fileChunkSize
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

    validateFileInvitation :: FileInvitation -> CM FileInvitation
    validateFileInvitation fInv@FileInvitation {fileName, fileSize}
      | fileSize > 0 = pure $ mkValidFileInvitation fInv
      | otherwise = throwChatError $ CEFileSize fileName

    messageUpdate :: Contact -> SharedMsgId -> MsgContent -> RcvMessage -> MsgMeta -> Maybe Int -> Maybe Bool -> CM ()
    messageUpdate ct@Contact {contactId} sharedMsgId mc msg@RcvMessage {msgId} msgMeta ttl live_ = do
      updateRcvChatItem `catchCINotFound` \_ -> do
        -- This patches initial sharedMsgId into chat item when locally deleted chat item
        -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
        -- Chat item and update message which created it will have different sharedMsgId in this case...
        if isVoice mc && not (featureAllowed SCFVoice forContact ct)
          then do
            let ciContent = ciContentNoParse $ CIRcvChatFeatureRejected CFVoice
            (ci, cInfo) <- saveRcvChatItem' user (CDDirectRcv ct) msg (Just sharedMsgId) brokerTs ciContent Nothing Nothing False M.empty
            toView $ CEvtChatItemUpdated user (AChatItem SCTDirect SMDRcv cInfo ci)
          else do
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

    groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM (Maybe DeliveryTaskContext)
    groupMsgReaction g m sharedMsgId itemMemberId scope_ reaction add RcvMessage {msgId} brokerTs
      | groupFeatureAllowed SGFReactions g = do
          rs <- withStore' $ \db -> getGroupReactions db g m itemMemberId sharedMsgId False
          if reactionAllowed add reaction rs
            then
              updateChatItemReaction `catchCINotFound` \_ -> case scope_ of
                Just (MSMember scopeMemberId)
                  | memberRole' m >= GRModerator || scopeMemberId == memberId' m -> do
                      djScope <- withStore $ \db -> do
                        liftIO $ setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                        Just . DJSMemberSupport <$> getScopeMemberIdViaMemberId db user g m scopeMemberId
                      pure $ fmap (\js -> DeliveryTaskContext js False) djScope
                  | otherwise -> pure Nothing
                Nothing -> do
                  withStore' $ \db -> setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                  pure $ Just $ DeliveryTaskContext (DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}) False
            else pure Nothing
      | otherwise = pure Nothing
      where
        updateChatItemReaction = do
          (CChatItem md ci, scopeInfo) <- withStore $ \db -> do
            cci <- case itemMemberId of
              Just itemMemberId' -> getGroupMemberCIBySharedMsgId db user g itemMemberId' sharedMsgId
              Nothing -> getGroupChatItemBySharedMsgId db user g Nothing sharedMsgId
            scopeInfo <- getGroupChatScopeInfoForItem db cxt user g (cChatItemId cci)
            pure (cci, scopeInfo)
          if ciReactionAllowed ci
            then do
              reactions <- withStore' $ \db -> do
                setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
                getGroupCIReactions db g itemMemberId sharedMsgId
              let ci' = CChatItem md ci {reactions}
                  r = ACIReaction SCTGroup SMDRcv (GroupChat g scopeInfo) $ CIReaction (CIGroupRcv m) ci' brokerTs reaction
              toView $ CEvtChatItemReaction user add r
              pure $ Just $ infoToDeliveryContext g scopeInfo False
            else pure Nothing

    reactionAllowed :: Bool -> MsgReaction -> [MsgReaction] -> Bool
    reactionAllowed add reaction rs = (reaction `elem` rs) /= add && not (add && length rs >= maxMsgReactions)

    catchCINotFound :: CM a -> (SharedMsgId -> CM a) -> CM a
    catchCINotFound f handle =
      f `catchAllErrors` \case
        ChatErrorStore (SEChatItemSharedMsgIdNotFound sharedMsgId) -> handle sharedMsgId
        e -> throwError e

    validSender :: Maybe GroupMember -> CIDirection 'CTGroup 'MDRcv -> Bool
    validSender (Just m) (CIGroupRcv mem) = sameMemberId (memberId' m) mem
    validSender m_ CIChannelRcv = maybe True (\m -> memberRole' m == GROwner) m_
    validSender _ _ = False

    isChannelDir :: CIDirection 'CTGroup 'MDRcv -> ShowGroupAsSender
    isChannelDir CIChannelRcv = True
    isChannelDir _ = False

    newGroupContentMessage :: GroupInfo -> Maybe GroupMember -> MsgContainer -> RcvMessage -> UTCTime -> Bool -> CM (Maybe DeliveryTaskContext)
    newGroupContentMessage gInfo m_ mc msg@RcvMessage {sharedMsgId_} brokerTs forwarded = case m_ of
      Nothing -> do
        createContentItem gInfo Nothing Nothing
        -- no delivery task - message already forwarded by relay
        pure Nothing
      Just m@GroupMember {memberId}
        -- only an owner may post as the channel; a non-owner's signed asGroup post (e.g. relay-injected) must not render as the channel
        | sentAsGroup && memberRole' m < GROwner ->
            messageError "x.msg.new: member is not allowed to send as group" $> Nothing
        | otherwise -> do
            (gInfo', m', scopeInfo) <- mkGetMessageChatScope cxt user gInfo m content msgScope_
            if blockedByAdmin m'
              then createBlockedByAdmin gInfo' (Just m') scopeInfo $> Nothing
              else case prohibitedGroupContent gInfo' m' scopeInfo content ft_ fInv_ False of
                Just f -> rejected gInfo' (Just m') scopeInfo f $> Nothing
                Nothing ->
                  withStore' (\db -> getCIModeration db cxt user gInfo' memberId sharedMsgId_) >>= \case
                    Just ciModeration -> do
                      applyModeration gInfo' m' scopeInfo ciModeration
                      withStore' $ \db -> deleteCIModeration db gInfo' memberId sharedMsgId_
                      pure Nothing
                    Nothing -> do
                      createContentItem gInfo' (Just m') scopeInfo
                      pure $ Just $ infoToDeliveryContext gInfo' scopeInfo sentAsGroup
      where
        rejected gInfo' m' scopeInfo f = newChatItem gInfo' m' scopeInfo (ciContentNoParse $ CIRcvGroupFeatureRejected f) Nothing Nothing False
        timed_ gInfo' = if forwarded then rcvCITimed_ (Just Nothing) itemTTL else rcvGroupCITimed gInfo' itemTTL
        live' = fromMaybe False live_
        MsgContainer {content = c, mentions = MsgMentions mentions, file = fInv_, ttl = itemTTL, live = live_, scope = msgScope_, asGroup = asGroup_} = mc
        content = case c of
          MCChat {text, chatLink, ownerSig = Just LinkOwnerSig {chatBinding = B64UrlByteString binding}} -> case publicGroup of
            Just pgp | maybe False (binding ==) (expectedBinding pgp) -> c
            _ -> MCChat {text, chatLink, ownerSig = Nothing}
          _ -> c
        expectedBinding PublicGroupProfile {publicGroupId}
          | sentAsGroup = Just $ encodeChatBinding CBChannel (smpEncode publicGroupId)
          | otherwise = (\GroupMember {memberId} -> encodeChatBinding CBGroup (smpEncode (publicGroupId, memberId))) <$> m_
        GroupInfo {groupProfile = GroupProfile {publicGroup}} = gInfo
        sentAsGroup = asGroup_ == Just True
        ts@(_, ft_) = msgContentTexts content
        -- m' is Maybe GroupMember
        saveRcvCI gInfo' m' scopeInfo =
          let itemMember_ = if sentAsGroup then Nothing else m'
              chatDir = maybe (CDChannelRcv gInfo' scopeInfo) (CDGroupRcv gInfo' scopeInfo) itemMember_
           in saveRcvChatItem' user chatDir msg sharedMsgId_ brokerTs
        createBlockedByAdmin gInfo' m' scopeInfo
          | groupFeatureAllowed SGFFullDelete gInfo' = do
              -- ignores member role when blocked by admin
              (ci, cInfo) <- saveRcvCI gInfo' m' scopeInfo (ciContentNoParse CIRcvBlocked) Nothing (timed_ gInfo') False M.empty
              ci' <- withStore' $ \db -> updateGroupCIBlockedByAdmin db user gInfo' ci brokerTs
              groupMsgToView cInfo ci'
          | otherwise = do
              file_ <- processFileInv gInfo' m'
              (ci, cInfo) <- createNonLive gInfo' m' scopeInfo file_
              ci' <- withStore' $ \db -> markGroupCIBlockedByAdmin db user gInfo' ci
              groupMsgToView cInfo ci'
        applyModeration gInfo' m'@GroupMember {memberRole} scopeInfo CIModeration {moderatorMember = moderator@GroupMember {memberRole = moderatorRole}, moderatedAt}
          | moderatorRole < GRModerator || moderatorRole < memberRole =
              createContentItem gInfo' (Just m') scopeInfo
          | groupFeatureMemberAllowed SGFFullDelete moderator gInfo' = do
              (ci, cInfo) <- saveRcvCI gInfo' (Just m') scopeInfo (ciContentNoParse CIRcvModerated) Nothing (timed_ gInfo') False M.empty
              ci' <- withStore' $ \db -> updateGroupChatItemModerated db user gInfo' ci moderator moderatedAt
              groupMsgToView cInfo ci'
          | otherwise = do
              file_ <- processFileInv gInfo' (Just m')
              (ci, _cInfo) <- createNonLive gInfo' (Just m') scopeInfo file_
              deletions <- markGroupCIsDeleted user gInfo' scopeInfo [CChatItem SMDRcv ci] (Just moderator) moderatedAt
              toView $ CEvtChatItemsDeleted user deletions False False
        -- m' is Maybe GroupMember
        createNonLive gInfo' m' scopeInfo file_ = do
          saveRcvCI gInfo' m' scopeInfo (CIRcvMsgContent content, ts) (snd <$> file_) (timed_ gInfo') False mentions
        createContentItem gInfo' m' scopeInfo = do
          file_ <- processFileInv gInfo' m'
          newChatItem gInfo' m' scopeInfo (CIRcvMsgContent content, ts) (snd <$> file_) (timed_ gInfo') live'
          unless (maybe False memberBlocked m') $ autoAcceptFile file_
        processFileInv gInfo' m' =
          let fileMember_ = if sentAsGroup then Nothing else m'
           in processFileInvitation fInv_ content $ \db -> createRcvGroupFileTransfer db userId gInfo' fileMember_ FTNormal sharedMsgId_
        newChatItem gInfo' m' scopeInfo ciContent ciFile_ timed live = do
          let mentions' = if maybe False memberBlocked m' then M.empty else mentions
          (ci, cInfo) <- saveRcvCI gInfo' m' scopeInfo ciContent ciFile_ timed live mentions'
          ci' <- maybe (pure ci) (\m -> blockedMemberCI gInfo' m ci) m'
          let memberId_ = memberId' <$> m'
          reactions <- maybe (pure []) (\sharedMsgId -> withStore' $ \db -> getGroupCIReactions db gInfo' memberId_ sharedMsgId) sharedMsgId_
          groupMsgToView cInfo ci' {reactions}

    groupMessageUpdate :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> MsgContent -> Map MemberName MsgMention -> Maybe MsgScope -> RcvMessage -> UTCTime -> Maybe Int -> Maybe Bool -> Maybe Bool -> CM (Maybe DeliveryTaskContext)
    groupMessageUpdate gInfo@GroupInfo {groupId} m_ sharedMsgId mc mentions msgScope_ msg@RcvMessage {msgId, msgSigned, signedMsg_, signedByGMId_} brokerTs ttl_ live_ asGroup_ = do
      updateRcvChatItem `catchCINotFound` \_ -> do
        -- This patches initial sharedMsgId into chat item when locally deleted chat item
        -- received an update from the sender, so that it can be referenced later (e.g. by broadcast delete).
        -- Chat item and update message which created it will have different sharedMsgId in this case...
        let timed_ = rcvGroupCITimed gInfo ttl_
            showGroupAsSender = fromMaybe (isNothing m_) asGroup_
        if showGroupAsSender && maybe False (\m -> memberRole' m < GROwner) m_
          then messageError "x.msg.update: member attempted to update as group" $> Nothing
          else do
            (gInfo', chatDir, mentions', scopeInfo) <-
              if showGroupAsSender
                then pure (gInfo, CDChannelRcv gInfo Nothing, mentions, Nothing)
                else case m_ of
                  Just m -> do
                    let mentions' = if memberBlocked m then [] else mentions
                    (gInfo', m', scopeInfo) <- mkGetMessageChatScope cxt user gInfo m mc msgScope_
                    pure (gInfo', CDGroupRcv gInfo' scopeInfo m', mentions', scopeInfo)
                  Nothing -> pure (gInfo, CDChannelRcv gInfo Nothing, mentions, Nothing)
            case m_ >>= \m -> prohibitedGroupContent gInfo' m scopeInfo mc ft_ (Nothing :: Maybe String) False of
              Just f -> do
                let ciContent = ciContentNoParse $ CIRcvGroupFeatureRejected f
                (ci, cInfo) <- saveRcvChatItem' user chatDir msg (Just sharedMsgId) brokerTs ciContent Nothing timed_ False M.empty
                groupMsgToView cInfo ci
                pure Nothing
              Nothing -> do
                (ci, cInfo) <- saveRcvChatItem' user chatDir msg (Just sharedMsgId) brokerTs (content, ts) Nothing timed_ live mentions'
                ci' <- withStore' $ \db -> do
                  createChatItemVersion db (chatItemId' ci) brokerTs mc
                  updateGroupChatItem db user groupId ci content True live Nothing
                ci'' <- case chatDir of
                  CDGroupRcv gi' _ m' -> blockedMemberCI gi' m' ci'
                  CDChannelRcv {} -> pure ci'
                toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv cInfo ci'')
                pure $ Just $ infoToDeliveryContext gInfo' scopeInfo showGroupAsSender
      where
        content = CIRcvMsgContent mc
        ts@(_, ft_) = msgContentTexts mc
        live = fromMaybe False live_
        updateRcvChatItem = do
          (cci, scopeInfo) <- withStore $ \db -> do
            cci <-
              if asGroup_ == Just True
                then getGroupChatItemBySharedMsgId db user gInfo Nothing sharedMsgId
                else case m_ of
                  Just m -> getGroupMemberCIBySharedMsgId db user gInfo (memberId' m) sharedMsgId
                  Nothing -> getGroupChatItemBySharedMsgId db user gInfo Nothing sharedMsgId
            (cci,) <$> getGroupChatScopeInfoForItem db cxt user gInfo (cChatItemId cci)
          case cci of
            CChatItem SMDRcv ci@ChatItem {chatDir = CIGroupRcv m', meta = CIMeta {itemLive, msgVerified = itemVerified}, content = CIRcvMsgContent oldMC}
              | isSender m' -> requireVerifiedEdit (CDGroupRcv gInfo scopeInfo m') itemVerified $ updateCI False ci scopeInfo oldMC itemLive (Just $ memberId' m')
              | otherwise -> messageError "x.msg.update: group member attempted to update a message of another member" $> Nothing
            CChatItem SMDRcv ci@ChatItem {chatDir = CIChannelRcv, meta = CIMeta {itemLive, msgVerified = itemVerified}, content = CIRcvMsgContent oldMC}
              | maybe True (\m -> memberRole' m == GROwner) m_ -> requireVerifiedEdit (CDChannelRcv gInfo scopeInfo) itemVerified $ updateCI True ci scopeInfo oldMC itemLive Nothing
              | otherwise -> messageError "x.msg.update: member attempted to update channel message" $> Nothing
            _ -> messageError "x.msg.update: invalid message update" $> Nothing
          where
            isSender m' = maybe False (\m -> sameMemberId (memberId' m) m') m_
            -- a verified item requires a verified edit (fail-closed): unsigned is a forgery (bad-signature item); signed-but-no-key is unverifiable (drop with a log)
            requireVerifiedEdit :: ChatDirection 'CTGroup 'MDRcv -> Maybe MsgVerified -> CM (Maybe DeliveryTaskContext) -> CM (Maybe DeliveryTaskContext)
            requireVerifiedEdit cd itemVerified action
              | itemVerified == Just (MVSigned MSSVerified) =
                  case msgSigned of
                    Just MSSVerified -> action
                    Just MSSSignedNoKey -> logWarn "x.msg.update: unverified update of a signed item (no key to verify), dropped" $> Nothing
                    Nothing -> createInternalChatItem user cd (CIRcvGroupEvent RGEMsgBadSignature) (Just brokerTs) $> Nothing
              | otherwise = action
        updateCI :: ShowGroupAsSender -> ChatItem 'CTGroup 'MDRcv -> Maybe GroupChatScopeInfo -> MsgContent -> Maybe Bool -> Maybe MemberId -> CM (Maybe DeliveryTaskContext)
        updateCI showGroupAsSender ci scopeInfo oldMC itemLive memberId = do
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
                updateChatItemSignedMsg db (chatItemId' ci) signedMsg_ signedByGMId_
                updateGroupCIMentions db gInfo ci' ciMentions
              toView $ CEvtChatItemUpdated user (AChatItem SCTGroup SMDRcv (GroupChat gInfo scopeInfo) ci')
              startUpdatedTimedItemThread user (ChatRef CTGroup groupId $ toChatScope <$> scopeInfo) ci ci'
              pure $ Just $ infoToDeliveryContext gInfo scopeInfo showGroupAsSender
            else do
              toView $ CEvtChatItemNotChanged user (AChatItem SCTGroup SMDRcv (GroupChat gInfo scopeInfo) ci)
              pure Nothing

    groupMessageDelete :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> Bool -> RcvMessage -> UTCTime -> CM (Maybe DeliveryTaskContext)
    groupMessageDelete gInfo@GroupInfo {membership} m_ sharedMsgId sndMemberId_ scope_ onlyHistory rcvMsg brokerTs =
      findItem >>= \case
        Right cci@(CChatItem _ ci@ChatItem {chatDir}) -> requireVerifiedDelete cci $ case (chatDir, m_) of
          (CIGroupRcv mem, Just m@GroupMember {memberId}) ->
            let msgMemberId = fromMaybe memberId sndMemberId_
                isAuthor = sameMemberId memberId mem
            in case sndMemberId_ of
              -- regular deletion
              Nothing
                | isAuthor && onlyHistory && publicGroupEditor gInfo m ->
                    delete cci False Nothing $> Nothing
                | isAuthor && not onlyHistory && rcvItemDeletable ci brokerTs ->
                    delete cci False Nothing
                | otherwise ->
                    messageError "x.msg.del: member attempted invalid message delete" $> Nothing
              -- moderation (not limited by time)
              Just _
                | isAuthor && msgMemberId == memberId ->
                    delete cci False (Just m)
                | otherwise -> moderate m mem cci
          (CIChannelRcv, _)
            | isNothing sndMemberId_ && isOwner ->
                (if onlyHistory then ($> Nothing) else id) $ delete cci True Nothing
            | otherwise -> messageError "x.msg.del: invalid channel message delete" $> Nothing
          (CIGroupSnd, Just m) -> moderate m membership cci
          _ -> messageError "x.msg.del: invalid message deletion" $> Nothing
        Left e -> case m_ of
          Just m@GroupMember {memberId, memberRole = senderRole} -> do
            let msgMemberId = fromMaybe memberId sndMemberId_
            if
              | msgMemberId == memberId ->
                  messageError ("x.msg.del: message not found, " <> tshow e) $> Nothing
              | senderRole < GRModerator -> do
                  messageError $ "x.msg.del: message not found, message of another member with insufficient member permissions, " <> tshow e
                  pure Nothing
              -- a forged unsigned moderation would pre-censor a not-yet-received post via CIModeration; require verified (relay moderation always signs)
              | useRelays' gInfo && msgSigned /= Just MSSVerified ->
                  messageError ("x.msg.del: unverified moderation of message not yet received, " <> tshow e) $> Nothing
              | otherwise -> case scope_ of
                  Just (MSMember scopeMemberId) ->
                    withStore $ \db -> do
                      liftIO $ createCIModeration db gInfo m msgMemberId sharedMsgId msgId brokerTs
                      supportGMId <- getScopeMemberIdViaMemberId db user gInfo m scopeMemberId
                      pure $ Just $ DeliveryTaskContext {jobScope = DJSMemberSupport supportGMId, sentAsGroup = False}
                  Nothing -> do
                    withStore' $ \db -> createCIModeration db gInfo m msgMemberId sharedMsgId msgId brokerTs
                    pure $ Just $ DeliveryTaskContext {jobScope = DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}, sentAsGroup = False}
          Nothing ->
            messageError ("x.msg.del: channel message not found, " <> tshow e) $> Nothing
      where
        isOwner = maybe True (\m -> memberRole' m == GROwner) m_
        RcvMessage {msgId, msgSigned} = rcvMsg
        findItem = do
          let tryMemberLookup mId =
                withStore' (\db -> runExceptT $ getGroupMemberCIBySharedMsgId db user gInfo mId sharedMsgId)
              tryChannelLookup =
                withStore' (\db -> runExceptT $ getGroupChatItemBySharedMsgId db user gInfo Nothing sharedMsgId)
          case sndMemberId_ of
            Just sId -> tryMemberLookup sId
            Nothing -> case m_ of
              Just GroupMember {memberId} ->
                tryMemberLookup memberId >>= \case
                  Right cci -> pure (Right cci)
                  Left e ->
                    tryChannelLookup >>= \case
                      Right cci -> pure (Right cci)
                      Left _ -> pure (Left e)
              Nothing -> tryChannelLookup
        moderate :: GroupMember -> GroupMember -> CChatItem 'CTGroup -> CM (Maybe DeliveryTaskContext)
        moderate sender mem cci = case sndMemberId_ of
          Just sndMemberId
            | sameMemberId sndMemberId mem -> checkRole (memberRole' sender) mem $ do
                ctx_ <- delete cci False (Just sender)
                archiveMessageReports cci sender
                pure ctx_
            | otherwise -> messageError "x.msg.del: message of another member with incorrect memberId" $> Nothing
          _ -> messageError "x.msg.del: message of another member without memberId" $> Nothing
        checkRole senderRole GroupMember {memberRole} a
          | senderRole < GRModerator || senderRole < memberRole =
              messageError "x.msg.del: message of another member with insufficient member permissions" $> Nothing
          | otherwise = a
        -- a verified item requires a verified delete (fail-closed): unsigned is a forgery (bad-signature item); signed-but-no-key is unverifiable (drop with a log)
        requireVerifiedDelete :: CChatItem 'CTGroup -> CM (Maybe DeliveryTaskContext) -> CM (Maybe DeliveryTaskContext)
        requireVerifiedDelete cci@(CChatItem _ ChatItem {chatDir, meta = CIMeta {msgVerified = itemVerified}}) action
          | itemVerified == Just (MVSigned MSSVerified) =
              case msgSigned of
                Just MSSVerified -> action
                Just MSSSignedNoKey -> logWarn "x.msg.del: unverified delete of a signed item (no key to verify), dropped" $> Nothing
                Nothing -> do
                  scopeInfo <- withStore $ \db -> getGroupChatScopeInfoForItem db cxt user gInfo (cChatItemId cci)
                  let cd :: ChatDirection 'CTGroup 'MDRcv
                      cd = case chatDir of
                        CIGroupRcv mem -> CDGroupRcv gInfo scopeInfo mem
                        CIChannelRcv -> CDChannelRcv gInfo scopeInfo
                        CIGroupSnd -> CDGroupRcv gInfo scopeInfo membership
                  createInternalChatItem user cd (CIRcvGroupEvent RGEMsgBadSignature) (Just brokerTs)
                  pure Nothing
          | otherwise = action
        delete :: CChatItem 'CTGroup -> Bool -> Maybe GroupMember -> CM (Maybe DeliveryTaskContext)
        delete cci asGroup byGroupMember = do
          scopeInfo <- withStore $ \db -> getGroupChatScopeInfoForItem db cxt user gInfo (cChatItemId cci)
          let fullDelete
                | asGroup = groupFeatureAllowed SGFFullDelete gInfo
                | otherwise = maybe False (\m -> groupFeatureMemberAllowed SGFFullDelete m gInfo) m_
          deletions <-
            if fullDelete
              then deleteGroupCIs user gInfo scopeInfo [cci] byGroupMember brokerTs
              else markGroupCIsDeleted user gInfo scopeInfo [cci] byGroupMember brokerTs
          toView $ CEvtChatItemsDeleted user deletions False False
          pure $ if isNothing m_ then Nothing else Just $ infoToDeliveryContext gInfo scopeInfo asGroup
        archiveMessageReports :: CChatItem 'CTGroup -> GroupMember -> CM ()
        archiveMessageReports (CChatItem _ ci) byMember = do
          ciIds <- withStore' $ \db -> markMessageReportsDeleted db user gInfo ci byMember brokerTs
          unless (null ciIds) $ toView $ CEvtGroupChatItemsDeleted user gInfo ciIds False (Just byMember)

    -- TODO remove once XFile is discontinued
    processFileInvitation' :: Contact -> FileInvitation -> RcvMessage -> MsgMeta -> CM ()
    processFileInvitation' ct fInv msg@RcvMessage {sharedMsgId_} msgMeta = do
      ChatConfig {fileChunkSize} <- asks config
      fInv'@FileInvitation {fileName, fileSize} <- validateFileInvitation fInv
      inline <- receiveInlineMode fInv' Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvFileTransfer db userId ct fInv' inline fileChunkSize
      let fileProtocol = if isJust xftpRcvFile then FPXFTP else FPSMP
          ciFile = Just $ CIFile {fileId, fileName, fileSize, fileSource = Nothing, fileStatus = CIFSRcvInvitation, fileProtocol}
          content = ciContentNoParse $ CIRcvMsgContent $ MCFile ""
      (ci, cInfo) <- saveRcvChatItem' user (CDDirectRcv ct) msg sharedMsgId_ brokerTs content ciFile Nothing False M.empty
      toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]
      where
        brokerTs = metaBrokerTs msgMeta

    -- TODO remove once XFile is discontinued
    processGroupFileInvitation' :: GroupInfo -> GroupMember -> FileInvitation -> RcvMessage -> UTCTime -> CM ()
    processGroupFileInvitation' gInfo m fInv msg@RcvMessage {sharedMsgId_} brokerTs = do
      ChatConfig {fileChunkSize} <- asks config
      fInv'@FileInvitation {fileName, fileSize} <- validateFileInvitation fInv
      inline <- receiveInlineMode fInv' Nothing fileChunkSize
      RcvFileTransfer {fileId, xftpRcvFile} <- withStore $ \db -> createRcvGroupFileTransfer db userId gInfo (Just m) FTNormal sharedMsgId_ fInv' inline fileChunkSize
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
        ci <- withStore $ \db -> getChatItemByFileId db cxt user fileId
        toView $ CEvtRcvFileSndCancelled user ci ft

    xFileAcptInv :: Contact -> SharedMsgId -> Maybe ConnReqInvitation -> String -> CM ()
    xFileAcptInv ct sharedMsgId fileConnReq_ fName = do
      (fileId, AChatItem _ _ _ ci) <- withStore $ \db -> do
        fileId <- getDirectFileIdBySharedMsgId db user ct sharedMsgId
        (fileId,) <$> getChatItemByFileId db cxt user fileId
      assertSMPAcceptNotProhibited ci
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      -- [async agent commands] no continuation needed, but command should be asynchronous for stability
      if fName == fileName
        then unless cancelled $ case fileConnReq_ of
          -- receiving inline
          Nothing -> do
            event <- withStore $ \db -> do
              ci' <- updateDirectCIFileStatus db cxt user fileId $ CIFSSndTransfer 0 1
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
          updateDirectCIFileStatus db cxt user fileId CIFSSndComplete
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

    -- A group BFileChunk is a normal inline file chunk or a roster blob chunk, both located by
    -- (group_id, shared_msg_id). A chunk matching no in-flight transfer (an orphaned re-served roster
    -- chunk, or a missing normal file) is ignored; the outer withAckMessage acks it.
    bFileChunkGroup :: GroupInfo -> GroupMember -> SharedMsgId -> FileChunk -> MsgMeta -> CM ()
    bFileChunkGroup gInfo@GroupInfo {groupId} fromMember sharedMsgId chunk meta = do
      fileId_ <- withStore' $ \db -> getGroupRcvFileId db userId groupId (groupMemberId' fromMember) sharedMsgId
      forM_ fileId_ $ \fileId -> do
        ft <- withStore $ \db -> getRcvFileTransfer db user fileId
        case fileType ft of
          FTRoster -> receiveRosterChunk gInfo ft meta chunk
          FTNormal -> receiveInlineChunk ft chunk meta

    receiveInlineChunk :: RcvFileTransfer -> FileChunk -> MsgMeta -> CM ()
    receiveInlineChunk RcvFileTransfer {fileId, fileStatus = RFSNew} FileChunk {chunkNo} _
      | chunkNo == 1 = throwChatError $ CEInlineFileProhibited fileId
      | otherwise = pure ()
    receiveInlineChunk ft@RcvFileTransfer {fileId} chunk meta = do
      case chunk of
        FileChunk {chunkNo} -> when (chunkNo == 1) $ startReceivingFile user fileId
        _ -> pure ()
      receiveFileChunk Nothing ft Nothing meta chunk

    -- A roster re-serve re-sends the blob from chunk 1; discard any partial first, else chunk 1 over a
    -- partial is out-of-order (RcvChunkError) and appending after the stale prefix corrupts the blob.
    receiveRosterChunk :: GroupInfo -> RcvFileTransfer -> MsgMeta -> FileChunk -> CM ()
    receiveRosterChunk gInfo ft meta chunk = do
      case chunk of
        FileChunk {chunkNo} | chunkNo == 1 -> do
          last_ <- withStore' $ \db -> getRcvFileLastChunkNo db ft
          when (isJust last_) $ resetRosterPartialChunks ft
        _ -> pure ()
      receiveFileChunk (Just gInfo) ft Nothing meta chunk

    xFileCancelGroup :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> CM (Maybe DeliveryTaskContext)
    xFileCancelGroup g@GroupInfo {groupId} m_ sharedMsgId = do
      (fileId, aci) <- withStore $ \db -> do
        fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
        (fileId,) <$> getChatItemByFileId db cxt user fileId
      case aci of
        AChatItem SCTGroup SMDRcv (GroupChat _g scopeInfo) ChatItem {chatDir}
          | validSender m_ chatDir -> do
              ft <- withStore $ \db -> getRcvFileTransfer db user fileId
              unless (rcvFileCompleteOrCancelled ft) $ do
                cancelRcvFileTransfer user ft
                toView $ CEvtRcvFileSndCancelled user aci ft
              pure $ Just $ infoToDeliveryContext g scopeInfo (isChannelDir chatDir)
          | otherwise -> messageError "x.file.cancel: file cancel sender mismatch" $> Nothing
        _ -> messageError "x.file.cancel: group member attempted invalid file cancel" $> Nothing

    xFileAcptInvGroup :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe ConnReqInvitation -> String -> CM ()
    xFileAcptInvGroup GroupInfo {groupId} m@GroupMember {activeConn} sharedMsgId fileConnReq_ fName = do
      (fileId, AChatItem _ _ _ ci) <- withStore $ \db -> do
        fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
        (fileId,) <$> getChatItemByFileId db cxt user fileId
      assertSMPAcceptNotProhibited ci
      -- TODO check that it's not already accepted
      ft@FileTransferMeta {fileName, fileSize, fileInline, cancelled} <- withStore (\db -> getFileTransferMeta db user fileId)
      if fName == fileName
        then unless cancelled $ case (fileConnReq_, activeConn) of
          (Nothing, Just conn) -> do
            -- receiving inline
            event <- withStore $ \db -> do
              ci' <- updateDirectCIFileStatus db cxt user fileId $ CIFSSndTransfer 0 1
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
    processGroupInvitation ct inv msg msgMeta
      | isJust publicGroup = messageError "x.grp.inv: can't invite to channel"
      | otherwise = do
          let Contact {localDisplayName = c, activeConn} = ct
              GroupInvitation {fromMember = (MemberIdRole fromMemId fromRole), invitedMember = (MemberIdRole memId memRole), connRequest, groupLinkId} = inv
          forM_ activeConn $ \Connection {connId, connChatVersion, peerChatVRange, customUserProfileId, groupLinkId = groupLinkId'} -> do
            when (fromRole < GRAdmin || fromRole < memRole) $ throwChatError (CEGroupContactRole c)
            when (fromMemId == memId) $ throwChatError CEGroupDuplicateMemberId
            -- [incognito] if direct connection with host is incognito, create membership using the same incognito profile
            (gInfo@GroupInfo {groupId, localDisplayName, groupProfile, membership}, hostId) <- withStore $ \db -> createGroupInvitation db cxt user ct inv customUserProfileId
            void $ createChatItem user (CDGroupSnd gInfo Nothing) False CIChatBanner Nothing Nothing (Just epochStart)
            let GroupMember {groupMemberId, memberId = membershipMemId} = membership
            if sameGroupLinkId groupLinkId groupLinkId'
              then do
                subMode <- chatReadVar subscriptionMode
                dm <- encodeConnInfo $ XGrpAcpt membershipMemId
                connIds@(cmdId, acId) <- prepareAgentJoin user Nothing True connRequest
                withStore' $ \db -> do
                  setViaGroupLinkUri db groupId connId
                  createMemberConnectionAsync db user hostId connIds connChatVersion peerChatVRange subMode
                  updateGroupMemberStatusById db userId hostId GSMemAccepted
                  updateGroupMemberStatus db userId membership GSMemAccepted
                joinAgentConnectionAsync cmdId False acId True connRequest dm subMode
                toView $ CEvtUserAcceptedGroupSent user gInfo {membership = membership {memberStatus = GSMemAccepted}} (Just ct)
              else do
                let content = CIRcvGroupInvitation (CIGroupInvitation {groupId, groupMemberId, localDisplayName, groupProfile, status = CIGISPending}) memRole
                (ci, cInfo) <- saveRcvChatItemNoParse user (CDDirectRcv ct) msg brokerTs content
                withStore' $ \db -> setGroupInvitationChatItemId db user groupId (chatItemId' ci)
                toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]
                toView $ CEvtReceivedGroupInvitation {user, groupInfo = gInfo, contact = ct, fromMemberRole = fromRole, memberRole = memRole}
      where
        GroupInvitation {groupProfile = GroupProfile {publicGroup}} = inv
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
            (ct',) <$> getContactConnections db cxt userId ct'
          deleteAgentConnectionsAsync $ map aConnId contactConns
          forM_ contactConns $ \conn -> withStore' $ \db -> updateConnectionStatus db conn ConnDeleted
          activeConn' <- forM (contactConn ct') $ \conn -> pure conn {connStatus = ConnDeleted}
          let ct'' = ct' {activeConn = activeConn'} :: Contact
          (ci, cInfo) <- saveRcvChatItemNoParse user (CDDirectRcv ct'') msg brokerTs (CIRcvDirectEvent RDEContactDeleted)
          toView $ CEvtNewChatItems user [AChatItem SCTDirect SMDRcv cInfo ci]
          toView $ CEvtContactDeletedByContact user ct''
        else do
          contactConns <- withStore' $ \db -> getContactConnections db cxt userId c
          deleteAgentConnectionsAsync $ map aConnId contactConns
          withStore $ \db -> deleteContact db user c
      where
        brokerTs = metaBrokerTs msgMeta

    processContactProfileUpdate :: Contact -> Profile -> Bool -> CM Contact
    processContactProfileUpdate c@Contact {profile = lp} p' createItems
      -- a failed/unknown-key badge is re-verified even when content is unchanged, so it heals after an app update adds the key
      | contentChanged || badgeNeedsReverify lp = do
          c' <- withStore $ \db ->
            if userTTL == rcvTTL
              then updateContactProfile db cxt user c p'
              else do
                c' <- liftIO $ updateContactUserPreferences db user c ctUserPrefs'
                updateContactProfile db cxt user c' p'
          when (contentChanged && directOrUsed c' && createItems) $ do
            createProfileUpdatedItem c'
            lift $ createRcvFeatureItems user c c'
          toView $ CEvtContactUpdated user c c'
          pure c'
      | otherwise =
          pure c
      where
        contentChanged = not (sameProfileContent p p')
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

    xInfoMember :: GroupInfo -> GroupMember -> Profile -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xInfoMember gInfo m p' msg brokerTs = do
      void $ processMemberProfileUpdate gInfo m p' (Just (msg, brokerTs))
      pure $ memberEventDeliveryScope m

    xGrpLinkMem :: GroupInfo -> GroupMember -> Connection -> Profile -> CM ()
    xGrpLinkMem gInfo@GroupInfo {membership, businessChat} m@GroupMember {groupMemberId, memberCategory} Connection {viaGroupLink} p' = do
      xGrpLinkMemReceived <- withStore $ \db -> getXGrpLinkMemReceived db groupMemberId
      if (viaGroupLink || isJust businessChat) && isNothing (memberContactId m) && memberCategory == GCHostMember && not xGrpLinkMemReceived
        then do
          m' <- processMemberProfileUpdate gInfo m p' Nothing
          withStore' $ \db -> setXGrpLinkMemReceived db groupMemberId True
          let connectedIncognito = memberIncognito membership
          probeMatchingMemberContact m' connectedIncognito
        else messageError "x.grp.link.mem error: invalid group link host profile update"

    xGrpLinkAcpt :: GroupInfo -> GroupMember -> GroupAcceptance -> GroupMemberRole -> MemberId -> RcvMessage -> UTCTime -> CM ()
    xGrpLinkAcpt gInfo@GroupInfo {membership} m acceptance role memberId msg brokerTs
      | memberRole' m < GRModerator || memberRole' m < role =
          messageError "x.grp.link.acpt with insufficient member permissions"
      | sameMemberId memberId membership = processUserAccepted
      | otherwise =
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db cxt user gInfo memberId) >>= \case
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
            createInternalChatItem user cd (CIRcvGroupE2EEInfo $ e2eInfoGroup gInfo') Nothing
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
          introduceToRemaining cxt user gInfo acceptedMember
          when (groupFeatureAllowed SGFHistory gInfo) $ sendHistory user gInfo acceptedMember

    maybeCreateGroupDescrLocal :: GroupInfo -> GroupMember -> CM ()
    maybeCreateGroupDescrLocal gInfo@GroupInfo {groupProfile = GroupProfile {description}} m =
      unless expectHistory $ forM_ description $ \descr ->
        createInternalChatItem user (CDGroupRcv gInfo Nothing m) (CIRcvMsgContent $ MCText descr) Nothing
      where
        expectHistory = groupFeatureAllowed SGFHistory gInfo && m `supportsVersion` groupHistoryIncludeWelcomeVersion

    processMemberProfileUpdate :: GroupInfo -> GroupMember -> Profile -> Maybe (RcvMessage, UTCTime) -> CM GroupMember
    processMemberProfileUpdate gInfo m@GroupMember {memberProfile = p, memberContactId} p' msgTs_
      -- a failed/unknown-key badge is re-verified even when content is unchanged, so it heals after an app update adds the key
      | contentChanged || badgeNeedsReverify p = do
          when contentChanged $ updateBusinessChatProfile gInfo
          case memberContactId of
            Nothing -> do
              m' <- withStore $ \db -> updateMemberProfile db cxt user m p''
              unless (muteEventInChannel gInfo m') $ do
                when contentChanged $ forM_ msgTs_ $ createProfileUpdatedItem m'
                toView $ CEvtGroupMemberUpdated user gInfo m m'
              pure m'
            Just mContactId -> do
              mCt <- withStore $ \db -> getContact db cxt user mContactId
              if canUpdateProfile mCt
                then do
                  (m', ct') <- withStore $ \db -> updateContactMemberProfile db cxt user m mCt p'
                  unless (muteEventInChannel gInfo m') $ do
                    when contentChanged $ forM_ msgTs_ $ createProfileUpdatedItem m'
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
        p'' = redactedMemberProfile gInfo m p'
        contentChanged = not (sameProfileContent (redactedMemberProfile gInfo m (fromLocalProfile p)) p'')
        updateBusinessChatProfile g@GroupInfo {businessChat} = case businessChat of
          Just bc | isMainBusinessMember bc m -> do
            g' <- withStore $ \db -> updateGroupProfileFromMember db user g p'
            toView $ CEvtGroupUpdated user g g' (Just m) Nothing
          _ -> pure ()
        isMainBusinessMember BusinessChatInfo {chatType, businessId, customerId} GroupMember {memberId} = case chatType of
          BCBusiness -> businessId == memberId
          BCCustomer -> customerId == memberId
        createProfileUpdatedItem m' (msg, brokerTs) = do
          (gInfo', m'', scopeInfo) <- mkGroupChatScope gInfo m'
          let createItem scopeInfo_ m_ = do
                let ciContent = CIRcvGroupEvent $ RGEMemberProfileUpdated (fromLocalProfile p) p'
                    cd = CDGroupRcv gInfo' scopeInfo_ m_
                (ci, cInfo) <- saveRcvChatItemNoParse user cd msg brokerTs ciContent
                groupMsgToView cInfo ci
          case scopeInfo of
            Just _ -> createItem scopeInfo m''
            Nothing
              | useRelays' gInfo' && not (isRelay m'') && memberRole' m'' < GRModerator ->
                  forM_ (supportChat m'') $ \_ ->
                    createItem (Just GCSIMemberSupport {groupMember_ = Just m''}) m''
              | otherwise -> createItem Nothing m''

    xInfoProbe :: ContactOrMember -> Probe -> CM ()
    xInfoProbe cgm2 probe = do
      contactMerge <- readTVarIO =<< asks contactMergeEnabled
      -- [incognito] unless connected incognito
      when (contactMerge && not (contactOrMemberIncognito cgm2)) $ do
        cgm1s <- withStore' $ \db -> matchReceivedProbe db cxt user cgm2 probe
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
        cgm2Probe_ <- withStore' $ \db -> matchReceivedProbeHash db cxt user cgm1 probeHash
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
      cgm2 <- withStore' $ \db -> matchSentProbe db cxt user cgm1 probe
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
        getGroupInfo db cxt user groupId
      toView $ CEvtContactAndMemberAssociated user c1 g m2 c1
      pure c1

    associateContactWithMember :: GroupMember -> Contact -> CM Contact
    associateContactWithMember m1@GroupMember {groupId} c2 = do
      (c2', g) <- withStore $ \db ->
        liftM2 (,) (associateContactWithMemberRecord db cxt user m1 c2) (getGroupInfo db cxt user groupId)
      toView $ CEvtContactAndMemberAssociated user c2 g m1 c2'
      pure c2'

    saveConnInfo :: Connection -> ConnInfo -> CM (Connection, Maybe GroupInfo)
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatVRange, chatMsgEvent} <- parseChatMessage activeConn connInfo
      conn' <- updatePeerChatVRange activeConn chatVRange
      case chatMsgEvent of
        XInfo p -> do
          ct <- withStore $ \db -> createDirectContact db cxt user conn' p
          toView $ CEvtContactConnecting user ct
          pure (conn', Nothing)
        XGrpLinkInv glInv -> do
          (gInfo, host) <- withStore $ \db -> createGroupInvitedViaLink db cxt user conn' glInv
          toView $ CEvtGroupLinkConnecting user gInfo host
          pure (conn', Just gInfo)
        XGrpLinkReject glRjct@GroupLinkRejection {rejectionReason} -> do
          (gInfo, host) <- withStore $ \db -> createGroupRejectedViaLink db cxt user conn' glRjct
          toView $ CEvtGroupLinkConnecting user gInfo host
          toViewTE $ TEGroupLinkRejected user gInfo rejectionReason
          pure (conn', Just gInfo)
        -- TODO show/log error, other events in SMP confirmation
        _ -> pure (conn', Nothing)

    xGrpMemNew :: GroupInfo -> GroupMember -> MemberInfo -> Maybe MsgScope -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpMemNew gInfo m memInfo@(MemberInfo memId memRole _ _ assertedKey_) msgScope_ msg brokerTs = do
      unless (useRelays' gInfo) $ checkHostRole m memRole
      if sameMemberId memId (membership gInfo)
        then pure Nothing
        else
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db cxt user gInfo memId) >>= \case
            Right unknownMember@GroupMember {memberStatus = GSMemUnknown}
              -- roster-established privileged member: the relay may update the profile only,
              -- never the role or key (those are owner-authoritative via the roster, and
              -- XGrpMemNew is unsigned)
              | useRelays' gInfo && isPrivilegedRole (memberRole' unknownMember) -> do
                  -- a member's key is immutable per memberId and identical across relays; mismatch
                  -- is unambiguous relay misbehavior (role can legitimately differ across relays
                  -- under multi-relay skew, so we deliberately don't warn on role)
                  let assertedKey = (\(MemberKey k) -> k) <$> assertedKey_
                  -- TODO [relays] member: surface relay-key-mismatch as a dedicated event / chat item / relay state
                  when (assertedKey /= memberPubKey unknownMember) $
                    messageWarning $ "x.grp.mem.new: relay asserted key differs from roster-established key, keeping roster key, memberId=" <> safeDecodeUtf8 (strEncode memId)
                  updatedMember <- withStore $ \db -> updateRosterMemberAnnounced db cxt user m unknownMember memInfo initialStatus
                  -- roster members can't be pending, so no members-require-attention update
                  gInfo' <- updatePublicGroupData user gInfo
                  toView $ CEvtUnknownMemberAnnounced user gInfo' m unknownMember updatedMember
                  memberAnnouncedToView updatedMember gInfo'
                  pure $ deliveryJobScope updatedMember
              -- asserted privileged but NOT roster-established: relay conjuring a privileged member
              | useRelays' gInfo && isPrivilegedRole memRole ->
                  messageError "x.grp.mem.new: privileged role not established by roster" $> Nothing
              | otherwise -> do
                  (updatedMember, gInfo') <- withStore $ \db -> do
                    updatedMember <- updateUnknownMemberAnnounced db cxt user m unknownMember memInfo initialStatus
                    gInfo' <-
                      if memberPending updatedMember
                        then liftIO $ increaseGroupMembersRequireAttention db user gInfo
                        else pure gInfo
                    pure (updatedMember, gInfo')
                  gInfo'' <- updatePublicGroupData user gInfo'
                  toView $ CEvtUnknownMemberAnnounced user gInfo'' m unknownMember updatedMember
                  memberAnnouncedToView updatedMember gInfo''
                  pure $ deliveryJobScope updatedMember
            Right _
              | useRelays' gInfo -> logInfo "x.grp.mem.new: member already created via another relay" $> Nothing
              | otherwise -> messageError "x.grp.mem.new error: member already exists" $> Nothing
            Left _
              -- a privileged member absent from the roster is a relay conjuring one
              | useRelays' gInfo && isPrivilegedRole memRole -> messageError "x.grp.mem.new: privileged member not established by roster" $> Nothing
              | otherwise -> do
                  (newMember, gInfo') <- withStore $ \db -> do
                    newMember <- createNewGroupMember db cxt user gInfo m memInfo GCPostMember initialStatus
                    gInfo' <-
                      if memberPending newMember
                        then liftIO $ increaseGroupMembersRequireAttention db user gInfo
                        else pure gInfo
                    pure (newMember, gInfo')
                  gInfo'' <- updatePublicGroupData user gInfo'
                  memberAnnouncedToView newMember gInfo''
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
          unless (useRelays' gInfo') $ do
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
    xGrpMemIntro gInfo@GroupInfo {chatSettings} m@GroupMember {memberRole, localDisplayName = c} memInfo@(MemberInfo memId _ memChatVRange _ _) memRestrictions = do
      case memberCategory m of
        GCHostMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db cxt user gInfo memId) >>= \case
            Right existingMember
              | useRelays' gInfo -> do
                  updatedMember <- withStore $ \db -> updatePreparedChannelMember db cxt user existingMember memInfo
                  toView $ CEvtGroupMemberUpdated user gInfo existingMember updatedMember
              | otherwise ->
                  messageError "x.grp.mem.intro ignored: member already exists"
            Left _
              | useRelays' gInfo -> do
                  -- role + key are owner-authoritative (roster); an intro establishes neither - a privileged
                  -- claim is created at the channel default with no key until the owner-signed roster confirms it
                  defaultRole <- unknownMemberRole gInfo
                  let memInfo' = case memInfo of
                        MemberInfo mId mRole v p _
                          | mRole >= GRMember -> MemberInfo mId defaultRole v p Nothing
                        _ -> memInfo
                  void $ withStore $ \db -> createIntroReMember db cxt user gInfo memInfo' memRestrictions
              | otherwise -> do
                  when (memberRole < GRAdmin) $ throwChatError (CEGroupContactRole c)
                  case memChatVRange of
                    Nothing -> messageError "x.grp.mem.intro: member chat version range incompatible"
                    Just (ChatVersionRange mcvr)
                      | maxVersion mcvr >= groupDirectInvVersion -> do
                          subMode <- chatReadVar subscriptionMode
                          groupConnIds@(cmdId, connId) <- prepareAgentCreation user CFCreateConnGrpMemInv (chatHasNtfs chatSettings) SCMInvitation
                          let chatV = maybe (minVersion (vr cxt)) (\peerVR -> vr cxt `peerConnChatVersion` fromChatVRange peerVR) memChatVRange
                          void $ withStore $ \db -> do
                            reMember <- createIntroReMember db cxt user gInfo memInfo memRestrictions
                            createIntroReMemberConn db user m reMember chatV memInfo groupConnIds subMode
                          withAgent $ \a -> createConnectionAsync a (aCorrId cmdId) connId (chatHasNtfs chatSettings) SCMInvitation CR.IKPQOff subMode
                      | otherwise -> messageError "x.grp.mem.intro: member chat version range incompatible"
        _ -> messageError "x.grp.mem.intro can be only sent by host member"

    sendXGrpMemInv :: Int64 -> Maybe ConnReqInvitation -> XGrpMemIntroCont -> CM ()
    sendXGrpMemInv hostConnId directConnReq XGrpMemIntroCont {groupId, groupMemberId, memberId, groupConnReq} = do
      hostConn <- withStore $ \db -> getConnectionById db cxt user hostConnId
      let msg = XGrpMemInv memberId IntroInvitation {groupConnReq, directConnReq}
      void $ sendDirectMemberMessage hostConn msg groupId
      withStore' $ \db -> updateGroupMemberStatusById db userId groupMemberId GSMemIntroInvited

    xGrpMemInv :: GroupInfo -> GroupMember -> MemberId -> IntroInvitation -> CM ()
    xGrpMemInv gInfo m memId introInv = do
      case memberCategory m of
        GCInviteeMember ->
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db cxt user gInfo memId) >>= \case
            Left _ -> messageError "x.grp.mem.inv error: referenced member does not exist"
            Right reMember -> sendGroupMemberMessage gInfo reMember $ XGrpMemFwd (memberInfo gInfo m) introInv
        _ -> messageError "x.grp.mem.inv can be only sent by invitee member"

    xGrpMemFwd :: GroupInfo -> GroupMember -> MemberInfo -> IntroInvitation -> CM ()
    xGrpMemFwd gInfo@GroupInfo {membership, chatSettings} m memInfo@(MemberInfo memId memRole memChatVRange _ _) IntroInvitation {groupConnReq, directConnReq} = do
      let GroupMember {memberId = membershipMemId} = membership
      checkHostRole m memRole
      toMember <- withStore $ \db -> do
        toMember <-
          getGroupMemberByMemberId db cxt user gInfo memId
            -- TODO if the missed messages are correctly sent as soon as there is connection before anything else is sent
            -- the situation when member does not exist is an error
            -- member receiving x.grp.mem.fwd should have also received x.grp.mem.new prior to that.
            -- For now, this branch compensates for the lack of delayed message delivery.
            `catchError` \case
              SEGroupMemberNotFoundByMemberId _ -> createNewGroupMember db cxt user gInfo m memInfo GCPostMember GSMemAnnounced
              e -> throwError e
        -- TODO [knocking] separate pending statuses from GroupMemberStatus?
        -- TODO            add GSMemIntroInvitedPending, GSMemConnectedPending, etc.?
        -- TODO            keep as is? (GSMemIntroInvited has no purpose)
        let newMemberStatus = if memberPending toMember then memberStatus toMember else GSMemIntroInvited
        liftIO $ updateGroupMemberStatus db userId toMember newMemberStatus
        pure toMember
      subMode <- chatReadVar subscriptionMode
      -- [incognito] send membership incognito profile, create direct connection as incognito
      membershipProfile <- presentUserBadge user (incognitoMembershipProfile gInfo) $ redactedMemberProfile gInfo membership $ fromLocalProfile $ memberProfile membership
      dm <- encodeConnInfo $ XGrpMemInfo membershipMemId membershipProfile
      -- [async agent commands] no continuation needed, but commands should be asynchronous for stability
      let enableNtfsGrp = chatHasNtfs chatSettings
      groupConnIds@(gCmdId, gAcId) <- prepareAgentJoin user Nothing enableNtfsGrp groupConnReq
      directConnIds <- mapM (prepareAgentJoin user Nothing True) directConnReq
      let customUserProfileId = localProfileId <$> incognitoMembershipProfile gInfo
          mcvr = maybe chatInitialVRange fromChatVRange memChatVRange
          chatV = vr cxt `peerConnChatVersion` mcvr
      withStore' $ \db -> createIntroToMemberContact db user m toMember chatV mcvr groupConnIds directConnIds customUserProfileId subMode
      joinAgentConnectionAsync gCmdId False gAcId enableNtfsGrp groupConnReq dm subMode
      forM_ ((,) <$> directConnIds <*> directConnReq) $ \((dCmdId, dAcId), dcr) ->
        joinAgentConnectionAsync dCmdId False dAcId True dcr dm subMode

    -- rollback defense (channels): apply an owner-signed role/removal only at a version >= the persisted
    -- roster_version (not the batch-constant gInfo, which a relay can stale by reordering events in one
    -- batch), then advance it in the same transaction; a strictly lower version is a replay and is ignored.
    -- Only an owner sender may advance it: a non-owner signed event is rejected by the action that follows,
    -- but must not bump roster_version first, or every later owner roster at a lower version is dropped.
    applyAtRosterVersion :: GroupInfo -> Maybe GroupMember -> GroupMember -> Maybe VersionRoster -> CM (Maybe DeliveryJobScope) -> CM (Maybe DeliveryJobScope)
    applyAtRosterVersion gInfo fwdRelay_ sender rosterVer_ action
      | not (useRelays' gInfo) = action
      | otherwise = case rosterVer_ of
          Nothing -> action
          Just _ | memberRole' sender /= GROwner -> action
          Just v -> do
            (accept, prevComplete) <- withStore' $ \db -> do
              gate <- getGroupRosterVersion db gInfo
              prevComplete <- getCompleteRosterVersion db gInfo
              let fresh = maybe True (v >=) gate
              when fresh $ do
                setGroupRosterVersion db gInfo v
                -- advance the frontier when this delta is the next version. One version can carry several deltas
                -- (a multi-member role change), delivered in order, so seeing any one of them advances the frontier
                -- past that whole version.
                when (v == nextCompleteVersion prevComplete) $
                  setCompleteRosterVersion db gInfo v
              pure (fresh, prevComplete)
            if accept
              then (requestRosterOnGap v prevComplete `catchAllErrors` eToView) >> action
              else messageWarning "x.grp.mem: roster version not newer than current, ignoring" $> Nothing
      where
        -- the next contiguous version after the complete frontier. With no frontier the baseline is the first
        -- roster version (VersionRoster 0, see broadcastRoster): a subscriber seeing v0 without a prior roster is
        -- current, not gapped, as it cannot have missed an earlier version - so v0 neither requests nor stays behind.
        nextCompleteVersion = \case
          Just (VersionRoster c) -> VersionRoster (c + 1)
          Nothing -> VersionRoster 0
        -- a subscriber whose complete frontier (before this delta) lags more than one below it has missed versions:
        -- ask the relay that forwarded it (it holds >= v = the new gate) to re-serve the full roster, carrying the
        -- previous frontier so only a fuller snapshot is served. A stuck frontier re-asks on every following delta
        -- until a roster fills it. Best-effort; relays and the direct path (fwdRelay_ = Nothing) don't ask, nor a
        -- relay that predates roster support.
        requestRosterOnGap v prevComplete
          | isUserGrpFwdRelay gInfo = pure ()
          | otherwise = case fwdRelay_ of
              Just relay
                | gap, relay `supportsVersion` groupRosterVersion ->
                    void $ sendGroupMessage' user gInfo [relay] (XGrpRosterRequest prevComplete)
              _ -> pure ()
          where
            gap = v > nextCompleteVersion prevComplete

    xGrpMemRole :: GroupInfo -> Maybe GroupMember -> GroupMember -> MemberId -> GroupMemberRole -> Maybe MemberKey -> Maybe VersionRoster -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpMemRole gInfo@GroupInfo {membership} fwdRelay_ m@GroupMember {memberRole = senderRole} memId memRole memberKey_ rosterVer_ msg@RcvMessage {msgSigned} brokerTs
      | memRole == GRRelay = messageError "x.grp.mem.role: relay role can't be assigned" $> Nothing
      | membershipMemId == memId =
          applyAtRosterVersion gInfo fwdRelay_ m rosterVer_ $
            let gInfo' = gInfo {membership = membership {memberRole = memRole}}
             in changeMemberRole gInfo' membership False (\db -> updateGroupMemberRole db user membership memRole) (RGEUserRole memRole) True
      | otherwise = applyAtRosterVersion gInfo fwdRelay_ m rosterVer_ $ do
          defaultRole <- unknownMemberRole gInfo
          -- an owner-signed event with a key TOFU-creates an unknown member only for a roster role; else a plain lookup
          let allowCreate = useRelays' gInfo && senderRole == GROwner && isRosterRole memRole && isJust memberKey_
          withStore' (\db -> runExceptT $ getCreateUnknownGMByMemberId db cxt user gInfo memId (nameFromMemberId memId) defaultRole allowCreate) >>= \case
            Right (Just (member, created))
              -- just created (keyless, and allowCreate ensured the event carries its key): pin key + role
              | created, Just (MemberKey pubKey) <- memberKey_ ->
                  let gEvent = RGEMemberRole (groupMemberId' member) (fromLocalProfile $ memberProfile member) memRole
                   in changeMemberRole gInfo member created (\db -> void $ applyMemberKeyRole db member pubKey memRole) gEvent (not $ useRelays' gInfo)
              -- known member: apply the role (its key is established via roster/intro; the event's key is ignored)
              | otherwise ->
                  let gEvent = RGEMemberRole (groupMemberId' member) (fromLocalProfile $ memberProfile member) memRole
                   in changeMemberRole gInfo member created (\db -> updateGroupMemberRole db user member memRole) gEvent (not $ useRelays' gInfo)
            -- in relay groups the roster may deliver role update for previously-unknown privileged members
            _ | useRelays' gInfo -> pure Nothing
              | otherwise -> messageError "x.grp.mem.role with unknown member ID" $> Nothing
      where
        GroupMember {memberId = membershipMemId} = membership
        -- applyMember writes the change (role, or role + pinned key for a freshly TOFU-created member);
        -- the delivery scope (relay forwarding) is computed on the pre-change role
        changeMemberRole gInfo' member@GroupMember {memberRole = fromRole} created applyMember gEvent createItem
          | fromRole == GRRelay =
              messageError "x.grp.mem.role: relay role can't be changed" $> Nothing
          | senderRole < roleRequiredToChange fromRole memRole =
              messageError "x.grp.mem.role with insufficient member permissions" $> Nothing
          | useRelays' gInfo && (isRosterRole memRole || isRosterRole fromRole) && senderRole /= GROwner =
              messageError "x.grp.mem.role: only the owner can change member, moderator and admin roles in relay groups" $> Nothing
          -- a forwarded role event the roster already applied is a no-op; suppress it.
          -- a just-created member is keyless here, so fall through to pin its owner-attested key.
          | useRelays' gInfo && not created && fromRole == memRole = pure $ memberEventDeliveryScope member
          | otherwise = do
              withStore' applyMember
              (gInfo'', m') <-
                if createItem
                  then do
                    (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo' m
                    (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo'' scopeInfo m') msg brokerTs (CIRcvGroupEvent gEvent)
                    groupMsgToView cInfo ci
                    pure (gInfo'', m')
                  else pure (gInfo', m)
              toView CEvtMemberRole {user, groupInfo = gInfo'', byMember = m', member = member {memberRole = memRole}, fromRole, toRole = memRole, msgSigned}
              pure $ memberEventDeliveryScope member

    -- The header only starts the transfer; the roster is applied and the version bumped only at
    -- blob completion, so a withheld or corrupted blob leaves the last good roster intact.
    -- fromMember is the relay that delivered THIS roster copy (the owner on a relay receiving directly,
    -- a relay on a member receiving a forward); author is the owner who signed it.
    xGrpRoster :: GroupInfo -> GroupMember -> GroupMember -> GroupRoster -> VerifiedMsg e -> Maybe SharedMsgId -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpRoster gInfo fromMember author GroupRoster {version = newVer, fileInv = InlineFileInvitation {fileSize, fileDigest}} verifiedMsg sharedMsgId_ brokerTs
      -- only an owner may sign a roster; otherwise a relay could route it as a member whose key it controls
      | memberRole' author /= GROwner = messageError "x.grp.roster: not signed by an owner" $> Nothing
      | fileSize > maxGroupRosterBytes = messageError "x.grp.roster: roster blob size exceeds limit" $> Nothing
      | otherwise = case verifiedMsg of
          -- unreachable: XGrpRoster is in requiresSignature, so withVerifiedMsg rejected unsigned
          VMUnsigned _ -> pure Nothing
          VMSigned _ sm _ -> case sharedMsgId_ of
            Nothing -> Nothing <$ messageWarning "x.grp.roster: missing shared message id"
            Just sharedMsgId -> do
              -- per-source pending version (THIS relay's own in-flight transfer), not a single group slot
              pendingVer_ <- withStore' $ \db -> getRosterTransferVersion db gInfo (groupMemberId' fromMember)
              -- accept a version not below BOTH applied and this source's pending (>=, Nothing below 0): a preceding
              -- signed event may have already advanced rosterVersion to this blob's version; a lower one is a downgrade.
              if newVer `notBelowRoster` rosterVersion gInfo && newVer `notBelowRoster` pendingVer_
                then startRosterTransfer sm sharedMsgId
                else pure Nothing
      where
        startRosterTransfer sm sharedMsgId = do
          -- supersede THIS source's own in-flight transfer (older version or a restart); other relays' transfers are independent
          cleanupRosterTransfer gInfo (groupMemberId' fromMember)
          let relayHdr = if isUserGrpFwdRelay gInfo then Just sm else Nothing
          chSize <- asks $ fileChunkSize . config
          let rosterFInv = FileInvitation {fileName = "roster", fileSize, fileDigest = Nothing, fileConnReq = Nothing, fileInline = Just IFMSent, fileDescr = Nothing}
          -- transfer record + its scratch file in one transaction (file owned by the transfer, keyed per source)
          rft@RcvFileTransfer {fileId} <- withStore $ \db -> do
            transferId <- liftIO $ createRosterTransfer db gInfo (groupMemberId' fromMember) newVer fileDigest (groupMemberId' author) brokerTs relayHdr
            createRosterRcvFile db userId gInfo fromMember transferId sharedMsgId rosterFInv (Just IFMSent) chSize
          -- accept the chat-item-free file before chunk 1 (FIFO before it) so chunk 1 isn't rejected on RFSNew
          -- transient scratch file (consumed into roster_blob, then deleted): temp folder, not the user's files folder / Downloads
          tmpDir <- lift getChatTempDirectory
          rosterTs <- liftIO getCurrentTime
          let GroupInfo {groupId = gId} = gInfo
              rosterFile = "roster_" <> show gId <> "_" <> show (groupMemberId' fromMember) <> "_" <> formatTime defaultTimeLocale "%Y%m%d_%H%M%S" rosterTs
          filePath <- getRcvFilePath fileId (Just tmpDir) rosterFile False
          withStore' $ \db -> startRcvInlineFT db user rft filePath (Just IFMSent)
          pure Nothing

    -- Roster version comparison treating Nothing (un-materialized) as below 0. Non-strict (>=) so a relay
    -- accepts the owner's blob at the version a preceding signed event already advanced rosterVersion to.
    notBelowRoster :: VersionRoster -> Maybe VersionRoster -> Bool
    notBelowRoster v = maybe True (v >=)

    -- Blob arrived: verify the owner-attested digest over the plaintext and guard against
    -- downgrade before applying; on a relay, ack the owner and re-serve to members.
    rosterCompletion :: GroupInfo -> RcvFileTransfer -> CM ()
    rosterCompletion gInfo RcvFileTransfer {fileId, fileStatus} =
      withStore' (\db -> getRosterTransfer db fileId) >>= \case
        -- defensive: the file always has its transfer (created together, deleted together)
        Nothing -> lift (closeFileHandle fileId rcvFiles) >> forM_ (rosterFilePath fileStatus) removeFsFile
        Just RcvRosterTransfer {rosterTransferId = transferId, rosterTransferVersion = pendingVer, rosterTransferDigest = pendingDigest, rosterTransferOwnerGMId = ownerGMId, rosterTransferBrokerTs = rosterBrokerTs, rosterTransferHeader = header_} -> do
          owner_ <- withStore' $ \db -> eitherToMaybe <$> runExceptT (getGroupMemberById db cxt user ownerGMId)
          blob <- readAssembledRoster
          let isRelay' = isUserGrpFwdRelay gInfo
              ackErr err = do
                cleanupRosterTransferById transferId
                when isRelay' $ forM_ owner_ $ \owner -> sendRosterAck gInfo owner pendingVer (Just err)
          if FD.FileDigest (LC.sha512Hash (LB.fromStrict blob)) /= pendingDigest
            then ackErr "relay could not verify the roster blob"
            else case parseAll rosterBlobP blob of
              Left _ -> ackErr "relay could not parse the roster blob"
              Right entries -> case owner_ of
                Nothing -> cleanupRosterTransferById transferId
                Just author -> do
                  defaultRole <- unknownMemberRole gInfo
                  -- gate against the persisted roster_version inside the apply transaction: a roster from another
                  -- relay (or a preceding signed event) may already have advanced it past this one; a stale
                  -- completion (e.g. relay1 sent v5 then v6, relay2's v5 completes after v6) is rejected.
                  results_ <- withStore $ \db -> do
                    cur <- liftIO $ getGroupRosterVersion db gInfo
                    if maybe False (pendingVer <) cur
                      then pure Nothing
                      else do
                        res <- processRosterEntries db gInfo defaultRole (validateGroupRoster entries)
                        liftIO $ setGroupLiveRoster db gInfo pendingVer ownerGMId rosterBrokerTs header_ blob
                        pure (Just res)
                  cleanupRosterTransferById transferId
                  forM_ results_ $ \results -> do
                    emitRosterResults gInfo author rosterBrokerTs results
                    -- ack while setting up (own status accepted/acknowledged); a serving (active) relay must not ack broadcasts.
                    when (isRelay' && (relayOwnStatus gInfo == Just RSAccepted || relayOwnStatus gInfo == Just RSAcknowledgedRoster)) $ do
                      sendRosterAck gInfo author pendingVer Nothing
                      withStore' $ \db -> void $ updateRelayOwnStatusFromTo db gInfo RSAccepted RSAcknowledgedRoster
      where
        rosterFilePath = \case
          RFSAccepted p -> Just p
          RFSConnected p -> Just p
          RFSComplete p -> Just p
          _ -> Nothing
        readAssembledRoster = case rosterFilePath fileStatus of
          Just fp -> readAt fp
          Nothing -> throwChatError $ CEInternalError "roster file not in progress"
        readAt fp = lift (toFSFilePath fp) >>= liftIO . B.readFile

    -- TOFU-apply an owner-signed (key, role) to a resolved member: pin the key if absent; for a keyed
    -- member keep the trusted key (Left = reject a different one), else update the role. Right
    -- (Just (member-at-new-role, fromRole)) when the role changed, Right Nothing when already current.
    applyMemberKeyRole :: DB.Connection -> GroupMember -> C.PublicKeyEd25519 -> GroupMemberRole -> IO (Either MemberId (Maybe (GroupMember, GroupMemberRole)))
    applyMemberKeyRole db m pubKey role = case memberPubKey m of
      Just k
        | k /= pubKey -> pure (Left (memberId' m))
        | memberRole' m == role -> pure (Right Nothing)
        | otherwise -> updateGroupMemberRole db user m role $> Right (Just ((m :: GroupMember) {memberRole = role}, memberRole' m))
      Nothing -> setGroupMemberKeyRole db m pubKey role $> Right (Just ((m :: GroupMember) {memberRole = role}, memberRole' m))

    -- TOFU apply: pin each member's key on first use, then update roles.
    processRosterEntries :: DB.Connection -> GroupInfo -> GroupMemberRole -> [RosterMember] -> ExceptT StoreError IO ([MemberId], [(GroupMember, GroupMemberRole, Bool)])
    processRosterEntries db gInfo defaultRole entries = do
      let rosterIds = map (\RosterMember {memberId} -> memberId) entries
      (cs, as) <- foldrM applyRosterEntry ([], []) entries
      currentPriv <- liftIO $ getGroupRosterMembers db cxt user gInfo
      reverted <- liftIO $ fmap catMaybes $ forM currentPriv $ \m ->
        if memberId' m `notElem` rosterIds
          then updateGroupMemberRole db user m defaultRole $> Just ((m :: GroupMember) {memberRole = defaultRole}, memberRole' m, False)
          else pure Nothing
      pure (cs, as <> reverted)
      where
        -- entry-level failure (StoreError or IO exception) is muted; the entry is dropped
        applyRosterEntry RosterMember {memberId, key = MemberKey pubKey, role} (cs, as) =
          ( getCreateUnknownGMByMemberId db cxt user gInfo memberId (nameFromMemberId memberId) defaultRole True >>= \case
              Nothing -> pure (cs, as)
              Just (m, created) -> liftIO (applyMemberKeyRole db m pubKey role) >>= \case
                Left mid -> pure (mid : cs, as)
                Right Nothing -> pure (cs, as)
                Right (Just (rm, fromR)) -> pure (cs, (rm, fromR, created) : as)
          )
            `catchAllErrors` \_ -> pure (cs, as)

    emitRosterResults :: GroupInfo -> GroupMember -> UTCTime -> ([MemberId], [(GroupMember, GroupMemberRole, Bool)]) -> CM ()
    emitRosterResults gInfo@GroupInfo {membership} author rosterBrokerTs (conflicts, applied) = do
      forM_ conflicts $ \mid' ->
        messageWarning $ "x.grp.roster: member key conflict, keeping trusted key, memberId=" <> safeDecodeUtf8 (strEncode mid')
      forM_ applied $ \(member, fromRole, created) ->
        unless created $ emitRoleChange member fromRole
      where
        emitRoleChange member fromRole = do
          let toRole = memberRole' member
          (gInfo', author') <-
            if sameMemberId (memberId' membership) member
              then do
                (gInfo', author', scopeInfo) <- mkGroupChatScope gInfo author
                ci <- createChatItem user (CDGroupRcv gInfo' scopeInfo author') False (CIRcvGroupEvent $ RGEUserRole toRole) Nothing (Just MSSVerified) (Just rosterBrokerTs)
                toView $ CEvtNewChatItems user [ci]
                pure (gInfo', author')
              else pure (gInfo, author)
          toView CEvtMemberRole {user, groupInfo = gInfo', byMember = author', member, fromRole, toRole, msgSigned = Just MSSVerified}

    sendRosterAck :: GroupInfo -> GroupMember -> VersionRoster -> Maybe Text -> CM ()
    sendRosterAck gInfo owner ackVer err = void $ sendGroupMessage' user gInfo [owner] (XGrpRosterAck ackVer err)

    xGrpRosterAck :: GroupInfo -> GroupMember -> VersionRoster -> Maybe Text -> CM ()
    xGrpRosterAck gInfo m ackVer err = do
      relay_ <- withStore' $ \db -> eitherToMaybe <$> runExceptT (getGroupRelayByGMId db (groupMemberId' m))
      case relay_ of
        Just relay@GroupRelay {relayStatus = RSAccepted} -> case err of
          Nothing
            | rosterVersion gInfo == Just ackVer -> do
                (relay', gLink) <- withStore $ \db -> do
                  relay' <- liftIO $ updateRelayStatus db relay RSAcknowledgedRoster
                  gLink <- getGroupLink db user gInfo
                  pure (relay', gLink)
                setGroupLinkDataAsync user gInfo gLink
                toView $ CEvtGroupRelayUpdated user gInfo m relay'
            | otherwise -> messageWarning "x.grp.roster.ack: stale version, awaiting ack for the current roster"
          Just e -> do
            relay' <- withStore' $ \db -> updateRelayStatusFromTo db relay RSAccepted RSRejected
            toView $ CEvtGroupRelayUpdated user gInfo m relay'
            messageError $ "x.grp.roster.ack: relay could not save roster, marked rejected: " <> e
        _ -> pure ()

    -- A relay re-serves the full roster to a subscriber that detected a version gap, but only when its STORED
    -- blob is newer than BOTH the requester's version (Nothing = none) and the version it last served this member
    -- - the latter bounds reflected amplification (a member can't re-trigger a full serve). Gating on the stored
    -- blob (not roster_version, the gate) means the relay serves only a blob the requester will accept.
    -- serveRoster records the served version (on all serve paths) and is a no-op without a roster.
    xGrpRosterRequest :: GroupInfo -> GroupMember -> Maybe VersionRoster -> CM ()
    xGrpRosterRequest gInfo m reqVer_ =
      when (isUserGrpFwdRelay gInfo) $ do
        (stored_, served_) <- withStore' $ \db ->
          (,) <$> getStoredRosterVersion db gInfo <*> getMemberRosterServedVersion db m
        forM_ stored_ $ \stored ->
          when (maybe True (stored >) reqVer_ && maybe True (stored >) served_) $ serveRoster user gInfo m

    checkHostRole :: GroupMember -> GroupMemberRole -> CM ()
    checkHostRole GroupMember {memberRole, localDisplayName} memRole =
      when (memberRole < GRAdmin || memberRole < memRole) $ throwChatError (CEGroupContactRole localDisplayName)

    xGrpMemRestrict :: GroupInfo -> GroupMember -> MemberId -> MemberRestrictions -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpMemRestrict
      gInfo@GroupInfo {membership = GroupMember {memberId = membershipMemId}}
      m@GroupMember {memberRole = senderRole}
      memId
      MemberRestrictions {restriction}
      msg@RcvMessage {msgSigned}
      brokerTs
        | membershipMemId == memId = pure Nothing -- ignore - XGrpMemRestrict can be sent to restricted member for efficiency
        | otherwise = do
            unknownRole <- unknownMemberRole gInfo
            withStore (\db -> getCreateUnknownGMByMemberId db cxt user gInfo memId "" unknownRole True) >>= \case
              Nothing -> messageError "x.grp.mem.restrict: no member" $> Nothing -- shouldn't happen
              Just (bm, unknown) -> do
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
                      toView CEvtMemberBlockedForAll {user, groupInfo = gInfo', byMember = m', member = bm', blocked, msgSigned}
                      pure $ memberEventDeliveryScope bm
        where
          setMemberBlocked bm = withStore' $ \db -> updateGroupMemberBlocked db user gInfo restriction bm
          blocked = mrsBlocked restriction

    xGrpMemCon :: GroupInfo -> GroupMember -> MemberId -> CM ()
    xGrpMemCon gInfo sendingMem memId = do
      refMem <- withStore $ \db -> getGroupMemberByMemberId db cxt user gInfo memId
      -- Updating vectors in separate transactions to avoid deadlocks.
      withStore $ \db -> setMemberVectorRelationConnected db sendingMem refMem MRSubjectConnected
      withStore $ \db -> setMemberVectorRelationConnected db refMem sendingMem MRReferencedConnected

    xGrpMemDel :: GroupInfo -> Maybe GroupMember -> GroupMember -> MemberId -> Bool -> Maybe VersionRoster -> VerifiedMsg 'Json -> RcvMessage -> UTCTime -> Bool -> CM (Maybe DeliveryJobScope)
    xGrpMemDel gInfo@GroupInfo {membership} fwdRelay_ m@GroupMember {memberRole = senderRole} memId withMessages rosterVer_ verifiedMsg msg@RcvMessage {msgSigned} brokerTs forwarded = do
      let GroupMember {memberId = membershipMemId} = membership
      if membershipMemId == memId
        then applyAtRosterVersion gInfo fwdRelay_ m rosterVer_ $ checkRole membership $ do
          deleteGroupLinkIfExists user gInfo
          -- TODO [relays] possible improvement is to immediately delete rcv queues if isUserGrpFwdRelay
          unless (isUserGrpFwdRelay gInfo) $ deleteGroupConnections user gInfo False
          withStore' $ \db -> do
            updateGroupMemberStatus db userId membership GSMemRemoved
            when (maybe False (/= RSRejected) (relayOwnStatus gInfo)) $ updateRelayOwnStatus_ db gInfo RSInactive
          let membership' = membership {memberStatus = GSMemRemoved}
          when withMessages $ deleteMessages gInfo membership'
          deleteMemberItem msg gInfo RGEUserDeleted
          toView $ CEvtDeletedMemberUser user gInfo {membership = membership'} m withMessages msgSigned
          pure $ Just DJSGroup {jobSpec = DJRelayRemoved}
        else applyAtRosterVersion gInfo fwdRelay_ m rosterVer_ $
          withStore' (\db -> runExceptT $ getGroupMemberByMemberId db cxt user gInfo memId) >>= \case
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
                    deletedMember' = deletedMember {memberStatus = GSMemRemoved}
                when withMessages $ deleteMessages gInfo deletedMember'
                gInfo' <- case deliveryScope of
                  -- Keep member record if it's support scope - it will be required for forwarding inside that scope.
                  Just (DJSMemberSupport _) | shouldForward -> updateMemberRecordDeleted user gInfo deletedMember GSMemRemoved
                  _
                    | withMessages && groupFeatureMemberAllowed SGFFullDelete m gInfo ->
                        fullyDeleteMemberRecord user gInfo deletedMember
                    -- Undeleted "member connected" chat item will prevent deletion of member record.
                    | otherwise -> deleteOrUpdateMemberRecord user gInfo deletedMember
                gInfo'' <- updatePublicGroupData user gInfo'
                let wasDeleted = memberStatus == GSMemRemoved || memberStatus == GSMemLeft
                -- Clear forwardedByMember if it references the deleted member,
                -- as the member record was already deleted above.
                let RcvMessage {forwardedByMember = fwdBy} = msg
                    msg' = if fwdBy == Just groupMemberId then (msg :: RcvMessage) {forwardedByMember = Nothing} else msg
                unless wasDeleted $ deleteMemberItem msg' gInfo'' $ RGEMemberDeleted groupMemberId (fromLocalProfile memberProfile)
                toView $ CEvtDeletedMember user gInfo'' m deletedMember' withMessages msgSigned
                pure deliveryScope
      where
        checkRole GroupMember {memberRole} a
          | senderRole < GRAdmin || senderRole < memberRole =
              messageError "x.grp.mem.del with insufficient member permissions" $> Nothing
          | otherwise = a
        deleteMemberItem msg' gi gEvent = do
          (gi', m', scopeInfo) <- mkGroupChatScope gi m
          (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gi' scopeInfo m') msg' brokerTs (CIRcvGroupEvent gEvent)
          groupMsgToView cInfo ci
        deleteMessages :: GroupInfo -> GroupMember -> CM ()
        deleteMessages gInfo' delMem
          | groupFeatureMemberAllowed SGFFullDelete m gInfo' = deleteGroupMemberCIs user gInfo' delMem
          | otherwise = markGroupMemberCIsDeleted user gInfo' delMem m
        forwardToMember :: GroupMember -> CM ()
        forwardToMember member =
          let fwd = GrpMsgForward {fwdSender = FwdMember (memberId' m) (memberShortenedName m), fwdBrokerTs = brokerTs}
           in sendFwdMemberMessage member fwd verifiedMsg

    isUserGrpFwdRelay :: GroupInfo -> Bool
    isUserGrpFwdRelay gInfo@GroupInfo {membership}
      | useRelays' gInfo = isRelay membership
      | otherwise = memberRole' membership >= GRAdmin

    isMemberGrpFwdRelay :: GroupInfo -> GroupMember -> Bool
    isMemberGrpFwdRelay gInfo m
      | useRelays' gInfo = isRelay m
      | otherwise = memberRole' m >= GRAdmin

    unknownMemberRole :: GroupInfo -> CM GroupMemberRole
    unknownMemberRole gInfo
      | useRelays' gInfo = asks $ channelSubscriberRole . config
      | otherwise = pure GRAuthor

    xGrpLeave :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpLeave gInfo m msg@RcvMessage {msgSigned} brokerTs = do
      deleteMemberConnection m
      -- member record is not deleted to allow creation of "member left" chat item
      gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
      gInfo'' <- updatePublicGroupData user gInfo'
      unless (muteEventInChannel gInfo'' m) $ do
        (gInfo''', m', scopeInfo) <- mkGroupChatScope gInfo'' m
        (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo''' scopeInfo m') msg brokerTs (CIRcvGroupEvent RGEMemberLeft)
        groupMsgToView cInfo ci
        toView $ CEvtLeftMember user gInfo''' m' {memberStatus = GSMemLeft} msgSigned
      pure $ memberEventDeliveryScope m

    xGrpDel :: GroupInfo -> GroupMember -> RcvMessage -> UTCTime -> CM ()
    xGrpDel gInfo@GroupInfo {membership} m@GroupMember {memberRole} msg@RcvMessage {msgSigned} brokerTs = do
      when (memberRole /= GROwner) $ throwChatError $ CEGroupUserRole gInfo GROwner
      deleteGroupLinkIfExists user gInfo
      withStore' $ \db -> updateGroupMemberStatus db userId membership GSMemGroupDeleted
      -- TODO [relays] possible improvement is to immediately delete rcv queues if isUserGrpFwdRelay
      unless (isUserGrpFwdRelay gInfo) $ deleteGroupConnections user gInfo False
      (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo m
      (ci, cInfo) <- saveRcvChatItemNoParse user (CDGroupRcv gInfo'' scopeInfo m') msg brokerTs (CIRcvGroupEvent RGEGroupDeleted)
      groupMsgToView cInfo ci
      toView $ CEvtGroupDeleted user gInfo'' {membership = membership {memberStatus = GSMemGroupDeleted}} m' msgSigned

    xGrpInfo :: GroupInfo -> GroupMember -> GroupProfile -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
    xGrpInfo g@GroupInfo {groupProfile = p@GroupProfile {publicGroup = pg}, businessChat} m@GroupMember {memberRole} p'@GroupProfile {publicGroup = pg'} msg@RcvMessage {msgSigned} brokerTs
      | memberRole < GROwner = messageError "x.grp.info with insufficient member permissions" $> Nothing
      | let pgId = fmap (\PublicGroupProfile {publicGroupId} -> publicGroupId),
        useRelays' g && (isNothing pg' || pgId pg' /= pgId pg) = messageError "x.grp.info: publicGroupId mismatch for channel" $> Nothing
      | not (useRelays' g) && isJust pg' = messageError "x.grp.info: publicGroup not allowed in p2p groups" $> Nothing
      | otherwise = do
          case businessChat of
            Nothing -> unless (p == p') $ do
              g' <- withStore $ \db -> updateGroupProfile db user g p'
              (g'', m', scopeInfo) <- mkGroupChatScope g' m
              toView $ CEvtGroupUpdated user g g'' (Just m') msgSigned
              let cd = CDGroupRcv g'' scopeInfo m'
              unless (sameGroupProfileInfo p p') $ do
                (ci, cInfo) <- saveRcvChatItemNoParse user cd msg brokerTs (CIRcvGroupEvent $ RGEGroupUpdated p')
                groupMsgToView cInfo ci
              createGroupFeatureChangedItems user cd CIRcvGroupFeature g g''
              -- in channels, link data is updated by the owner making the change in runUpdateGroupProfile;
              -- other owners receiving the update do not refresh the same link
              unless (useRelays' g'') $
                void $ forkIO $ void $ setGroupLinkData' NRMBackground user g''
            Just _ -> updateGroupPrefs_ msgSigned g m $ fromMaybe defaultBusinessGroupPrefs $ groupPreferences p'
          -- relay advertises its web capability now that the owner's version is known (bumped by saveGroupRcvMsg)
          when (isRelay (membership g)) $ sendRelayCapIfNeeded user g
          pure $ Just DJSGroup {jobSpec = DJDeliveryJob {includePending = True}}

    xGrpPrefs :: GroupInfo -> GroupMember -> GroupPreferences -> RcvMessage -> CM (Maybe DeliveryJobScope)
    xGrpPrefs g m@GroupMember {memberRole} ps' RcvMessage {msgSigned}
      | memberRole < GROwner = messageError "x.grp.prefs with insufficient member permissions" $> Nothing
      | otherwise = updateGroupPrefs_ msgSigned g m ps' $> Just DJSGroup {jobSpec = DJDeliveryJob {includePending = True}}

    updateGroupPrefs_ :: Maybe MsgSigStatus -> GroupInfo -> GroupMember -> GroupPreferences -> CM ()
    updateGroupPrefs_ msgSigned g@GroupInfo {groupProfile = p} m ps' =
      unless (groupPreferences p == Just ps') $ do
        g' <- withStore' $ \db -> updateGroupPreferences db user g ps'
        toView $ CEvtGroupUpdated user g g' (Just m) msgSigned
        (g'', m', scopeInfo) <- mkGroupChatScope g' m
        let cd = CDGroupRcv g'' scopeInfo m'
        createGroupFeatureChangedItems user cd CIRcvGroupFeature g g''

    xGrpRelayNew :: GroupInfo -> GroupMember -> ShortLinkContact -> CM (Maybe DeliveryJobScope)
    xGrpRelayNew gInfo GroupMember {memberRole} rl
      | memberRole < GROwner = messageError "x.grp.relay.new with insufficient member permissions" $> Nothing
      | otherwise = do
          unless (isUserGrpFwdRelay gInfo) $ connectToRelayAsync user gInfo rl
          pure $ Just DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}

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
              mCt <- withStore $ \db -> getContact db cxt user mContactId
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
              groupDirectInvStartedConnection = autoAcceptMemberContacts user
            }
        joinExistingContact subMode mCt@Contact {contactId = mContactId}
          | autoAcceptMemberContacts user = do
              (cmdId, acId) <- prepareAgentJoin user Nothing True connReq
              mCt' <- withStore $ \db -> do
                updateMemberContactInvited db user mCt groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId (Just cmdId) g mConn ConnJoined mContactId subMode
                getContact db cxt user mContactId
              joinMemberContactAsync cmdId acId subMode
              securityCodeChanged mCt'
              createItems mCt' m
          | otherwise = do
              acId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True connReq PQSupportOff
              mCt' <- withStore $ \db -> do
                updateMemberContactInvited db user mCt groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId Nothing g mConn ConnPrepared mContactId subMode
                getContact db cxt user mContactId
              securityCodeChanged mCt'
              createInternalChatItem user (CDDirectRcv mCt') (CIRcvDirectEvent $ RDEGroupInvLinkReceived gp) Nothing
              createItems mCt' m
        createNewContact subMode
          | autoAcceptMemberContacts user = do
              (cmdId, acId) <- prepareAgentJoin user Nothing True connReq
              -- [incognito] reuse membership incognito profile
              (mCt, m') <- withStore $ \db -> do
                (mContactId, m') <- liftIO $ createMemberContactInvited db user g m groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId (Just cmdId) g mConn ConnJoined mContactId subMode
                mCt <- getContact db cxt user mContactId
                pure (mCt, m')
              joinMemberContactAsync cmdId acId subMode
              createInternalChatItem user (CDDirectSnd mCt) CIChatBanner (Just epochStart)
              createItems mCt m'
          | otherwise = do
              acId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True connReq PQSupportOff
              (mCt, m') <- withStore $ \db -> do
                (mContactId, m') <- liftIO $ createMemberContactInvited db user g m groupDirectInv
                void $ liftIO $ createMemberContactConn db user acId Nothing g mConn ConnPrepared mContactId subMode
                mCt <- getContact db cxt user mContactId
                pure (mCt, m')
              createInternalChatItem user (CDDirectSnd mCt) CIChatBanner (Just epochStart)
              createInternalChatItem user (CDDirectRcv mCt) (CIRcvDirectEvent $ RDEGroupInvLinkReceived gp) Nothing
              createItems mCt m'
        joinMemberContactAsync cmdId acId subMode = do
          -- [incognito] send membership incognito profile
          p <- presentUserBadge user (incognitoMembershipProfile g) $ userProfileDirect user (fromLocalProfile <$> incognitoMembershipProfile g) Nothing True
          -- TODO PQ should negotitate contact connection with PQSupportOn? (use encodeConnInfoPQ)
          dm <- encodeConnInfo $ XInfo p
          joinAgentConnectionAsync cmdId False acId True connReq dm subMode
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

    xGrpMsgForward :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> GrpMsgForward -> ParsedMsg 'Json -> UTCTime -> CM ()
    xGrpMsgForward gInfo scopeInfo m@GroupMember {localDisplayName} GrpMsgForward {fwdSender, fwdBrokerTs = msgTs} parsedMsg@(ParsedMsg _ _ chatMsg@ChatMessage {chatMsgEvent}) brokerTs = do
      unless (isMemberGrpFwdRelay gInfo m) $ throwChatError (CEGroupContactRole localDisplayName)
      case fwdSender of
        FwdMember memberId memberName -> do
          unknownRole <- unknownMemberRole gInfo
          let allowCreate = toCMEventTag chatMsgEvent /= XGrpLeave_
          withStore (\db -> getCreateUnknownGMByMemberId db cxt user gInfo memberId memberName unknownRole allowCreate) >>= \case
            Just (author, unknown)
              | memberRemoved author ->
                  logInfo $ "x.grp.msg.forward: ignoring content from removed member, group " <> tshow (groupId' gInfo) <> ", member " <> safeDecodeUtf8 (strEncode memberId) <> ", event " <> tshow (toCMEventTag chatMsgEvent)
              | not (useRelays' gInfo) && not (expectedForwarder author) ->
                  logInfo $ "x.grp.msg.forward: ignoring content from unexpected forwarder, group " <> tshow (groupId' gInfo) <> ", forwarder " <> tshow (groupMemberId' m) <> ", member " <> safeDecodeUtf8 (strEncode memberId) <> ", event " <> tshow (toCMEventTag chatMsgEvent)
              | otherwise -> do
                  when unknown $ toView $ CEvtUnknownMemberCreated user gInfo m author
                  void $ withVerifiedMsg gInfo scopeInfo author parsedMsg msgTs $
                    (`processForwardedMsg` Just author)
            Nothing -> pure ()
        FwdChannel -> processForwardedMsg (VMUnsigned chatMsg) Nothing
      where
        -- Forwards are only expected from the member that introduced us to the author: our host, or
        -- the author's inviter. Unknown members have no such record, so any admin may forward theirs.
        expectedForwarder :: GroupMember -> Bool
        expectedForwarder author =
          memberCategory m == GCHostMember
            || invitedByGroupMemberId author == Just (groupMemberId' m)
            || memberStatus author == GSMemUnknown
        -- ! see isForwardedGroupMsg: forwarded group events should include msgId to be deduplicated
        processForwardedMsg :: VerifiedMsg 'Json -> Maybe GroupMember -> CM ()
        processForwardedMsg verifiedMsg author_ = do
          rcvMsg_ <- saveGroupFwdRcvMsg user gInfo m author_ verifiedMsg brokerTs
          forM_ rcvMsg_ $ \rcvMsg@RcvMessage {sharedMsgId_, chatMsgEvent = ACME _ event} -> case event of
            XMsgNew mc ->
              void $ memberCanSend author_ scope $ newGroupContentMessage gInfo author_ mc rcvMsg msgTs True
              where
                MsgContainer {scope} = mc
            -- file description is always allowed, to allow sending files to support scope
            XMsgFileDescr sharedMsgId fileDescr -> void $ groupMessageFileDescription gInfo author_ sharedMsgId fileDescr
            XMsgUpdate sharedMsgId mContent mentions ttl live msgScope asGroup_ ->
              void $ memberCanSend author_ msgScope $ groupMessageUpdate gInfo author_ sharedMsgId mContent mentions msgScope rcvMsg msgTs ttl live asGroup_
            XMsgDel sharedMsgId memId scope_ _ -> void $ groupMessageDelete gInfo author_ sharedMsgId memId scope_ False rcvMsg msgTs
            XMsgReact sharedMsgId memId scope_ reaction add -> withAuthor XMsgReact_ $ \author -> void $ groupMsgReaction gInfo author sharedMsgId memId scope_ reaction add rcvMsg msgTs
            XFileCancel sharedMsgId -> void $ xFileCancelGroup gInfo author_ sharedMsgId
            XInfo p -> withAuthor XInfo_ $ \author -> void $ xInfoMember gInfo author p rcvMsg msgTs
            XGrpRelayNew rl -> withAuthor XGrpRelayNew_ $ \author -> void $ xGrpRelayNew gInfo author rl
            XGrpMemNew memInfo msgScope -> withAuthor XGrpMemNew_ $ \author -> void $ xGrpMemNew gInfo author memInfo msgScope rcvMsg msgTs
            XGrpMemRole memId memRole memberKey rosterVer -> withAuthor XGrpMemRole_ $ \author -> void $ xGrpMemRole gInfo (Just m) author memId memRole memberKey rosterVer rcvMsg msgTs
            XGrpMemRestrict memId memRestrictions -> withAuthor XGrpMemRestrict_ $ \author -> void $ xGrpMemRestrict gInfo author memId memRestrictions rcvMsg msgTs
            XGrpMemDel memId withMessages rosterVer -> withAuthor XGrpMemDel_ $ \author -> void $ xGrpMemDel gInfo (Just m) author memId withMessages rosterVer verifiedMsg rcvMsg msgTs True
            XGrpLeave -> withAuthor XGrpLeave_ $ \author -> void $ xGrpLeave gInfo author rcvMsg msgTs
            XGrpDel -> withAuthor XGrpDel_ $ \author -> void $ xGrpDel gInfo author rcvMsg msgTs
            XGrpInfo p' -> withAuthor XGrpInfo_ $ \author -> void $ xGrpInfo gInfo author p' rcvMsg msgTs
            XGrpPrefs ps' -> withAuthor XGrpPrefs_ $ \author -> void $ xGrpPrefs gInfo author ps' rcvMsg
            XGrpRoster gr -> withAuthor XGrpRoster_ $ \author -> void $ xGrpRoster gInfo m author gr verifiedMsg sharedMsgId_ msgTs
            _ -> messageError $ "x.grp.msg.forward: unsupported forwarded event " <> T.pack (show $ toCMEventTag event)
          where
            withAuthor :: CMEventTag e -> (GroupMember -> CM ()) -> CM ()
            withAuthor tag action = case author_ of
              Just author -> action author
              Nothing -> messageError $ "x.grp.msg.forward: event " <> tshow tag <> " requires author"

    withVerifiedMsg :: MsgEncodingI e => GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> ParsedMsg e -> UTCTime -> (VerifiedMsg e -> CM a) -> CM (Maybe a)
    withVerifiedMsg gInfo@GroupInfo {membership} scopeInfo member (ParsedMsg _ signedMsg_ chatMsg@ChatMessage {chatMsgEvent}) ts action =
      case verified of
        Just verifiedMsg -> Just <$> action verifiedMsg
        Nothing -> do
          createInternalChatItem user (CDGroupRcv gInfo scopeInfo member) (CIRcvGroupEvent RGEMsgBadSignature) (Just ts)
          pure Nothing
      where
        verified = case signedMsg_ of
          Just sm@SignedMsg {chatBinding, signatures, signedBody}
            | GroupMember {memberPubKey = Just pubKey, memberId} <- member ->
                case chatBinding of
                  CBGroup
                    | Just GroupKeys {publicGroupId} <- groupKeys gInfo ->
                        signed MSSVerified <$ guard (verifyGroupSig pubKey publicGroupId memberId signatures signedBody)
                    | otherwise ->
                        let prefix = smpEncode chatBinding <> smpEncode (memberId, pubKey) -- forward compatibility for verifying signed messages in p2p groups
                         in signed MSSVerified <$ guard (all (\case (MsgSignature KRMember sig) -> C.verify (C.APublicVerifyKey C.SEd25519 pubKey) sig (prefix <> signedBody)) signatures)
                  _ -> signed MSSSignedNoKey <$ guard signatureOptional
            | otherwise -> signed MSSSignedNoKey <$ guard (signatureOptional || unverifiedAllowed membership member tag)
            where
              signed status = VMSigned status sm chatMsg
          Nothing -> VMUnsigned chatMsg <$ guard signatureOptional
          where
            tag = toCMEventTag chatMsgEvent
            signatureOptional = not (useRelays' gInfo) || not (requiresSignature tag)

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
          (ci : _) -> getGroupChatScopeInfoForItem db cxt user gInfo (chatItemId' ci)
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
deleteGroupConnections user gInfo@GroupInfo {membership} waitDelivery = do
  cxt <- chatStoreCxt
  -- member records are not deleted to keep history
  members <- getMembers cxt
  deleteMembersConnections' user members waitDelivery
  where
    getMembers cxt
      | useRelays' gInfo, not (isRelay membership) = withStore' $ \db -> getGroupRelayMembers db cxt user gInfo
      | otherwise = withStore' $ \db -> getGroupMembers db cxt user gInfo

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
  cxt <- chatStoreCxt
  -- TODO [relays] in future may be required to read groupInfo and user on each iteration for up to date state
  -- TODO   - same for delivery jobs (runDeliveryJobWorker)
  gInfo <- withStore $ \db -> do
    user <- getUserByGroupId db groupId
    getGroupInfo db cxt user groupId
  forever $ do
    unless (delay == 0) $ liftIO $ threadDelay' delay
    lift $ waitForWork doWork
    runDeliveryTaskOperation cxt gInfo
  where
    (groupId, workerScope) = deliveryKey
    runDeliveryTaskOperation :: StoreCxt -> GroupInfo -> CM ()
    runDeliveryTaskOperation cxt gInfo = do
      withWork_ a doWork (withStore' $ \db -> getNextDeliveryTask db deliveryKey) $ \task ->
        processDeliveryTask task
          `catchAllErrors` \e -> do
            withStore' $ \db -> setDeliveryTaskErrStatus db (deliveryTaskId task) (tshow e)
            eToView e
      where
        processDeliveryTask :: MessageDeliveryTask -> CM ()
        processDeliveryTask task@MessageDeliveryTask {jobScope} =
          case jobScopeImpliedSpec jobScope of
            DJDeliveryJob _includePending
              | not (relayServesGroup gInfo) -> do
                  logWarn "delivery task worker: relay inactive"
                  withStore' $ \db -> setDeliveryTaskErrStatus db (deliveryTaskId task) "relay inactive"
              | otherwise ->
                  withWorkItems a doWork (withStore' $ \db -> getNextDeliveryTasks db gInfo task) $ \nextTasks -> do
                    let (body_, acceptedTasks, largeTasks) = batchDeliveryTasks1 (vr cxt) maxEncodedMsgLength nextTasks
                        senderGMIds = S.toList . S.fromList $ map (\MessageDeliveryTask {senderGMId} -> senderGMId) acceptedTasks
                    withStore' $ \db -> do
                      forM_ body_ $ \body -> createMsgDeliveryJob db gInfo jobScope senderGMIds body
                      forM_ acceptedTasks $ \t -> updateDeliveryTaskStatus db (deliveryTaskId t) DTSProcessed
                      forM_ largeTasks $ \t -> setDeliveryTaskErrStatus db (deliveryTaskId t) "large"
                    when (isJust body_) . lift . void $ getDeliveryJobWorker True deliveryKey
            -- DJRelayRemoved is allowed when RSInactive - it forwards XGrpMemDel about relay's own deletion
            DJRelayRemoved
              | workerScope /= DWSGroup ->
                  throwChatError $ CEInternalError "delivery task worker: relay removed task in wrong worker scope"
              | otherwise -> do
                  let MessageDeliveryTask {senderGMId, fwdSender, brokerTs = fwdBrokerTs, verifiedMsg} = task
                      fwd = GrpMsgForward {fwdSender, fwdBrokerTs}
                      body = encodeBinaryBatch [encodeFwdElement fwd verifiedMsg]
                  withStore' $ \db -> do
                    createMsgDeliveryJob db gInfo jobScope [senderGMId] body
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

-- TODO [relays] dissemination here is unsigned (relay-asserted profile).
-- Future: members sign an XMember on channel join, relay stores it per
-- member and forwards the signed XMember via this sidecar — enables
-- subscribers to verify member profiles out-of-band without trusting the relay.

-- | Encode an XGrpMemNew for first-introduction dissemination as a direct
-- (non-forwarded) batch element. 'Left' when the encoded element wouldn't
-- fit a singleton batch (see 'maxBatchElementSize').
encodeMemberNew :: VersionRangeChat -> GroupInfo -> GroupMember -> Either ChatError ByteString
encodeMemberNew vr gInfo member = case encodeChatMessage maxBatchElementSize chatMsg of
  ECMEncoded bs -> Right bs
  ECMLarge -> Left $ ChatError $ CEException $ "large profile element for member " <> show (groupMemberId' member)
  where
    chatMsg :: ChatMessage 'Json
    chatMsg =
      ChatMessage
        { chatVRange = vr,
          msgId = Nothing,
          chatMsgEvent = XGrpMemNew (memberInfo gInfo member) Nothing
        }

runDeliveryJobWorker :: AgentClient -> DeliveryWorkerKey -> Worker -> CM ()
runDeliveryJobWorker a deliveryKey Worker {doWork} = do
  delay <- asks $ deliveryWorkerDelay . config
  cxt <- chatStoreCxt
  (user, gInfo) <- withStore $ \db -> do
    user <- getUserByGroupId db groupId
    gInfo <- getGroupInfo db cxt user groupId
    pure (user, gInfo)
  forever $ do
    unless (delay == 0) $ liftIO $ threadDelay' delay
    lift $ waitForWork doWork
    runDeliveryJobOperation cxt user gInfo
  where
    (groupId, workerScope) = deliveryKey
    runDeliveryJobOperation :: StoreCxt -> User -> GroupInfo -> CM ()
    runDeliveryJobOperation cxt user gInfo = do
      withWork_ a doWork (withStore' $ \db -> getNextDeliveryJob db deliveryKey) $ \job ->
        processDeliveryJob job
          `catchAllErrors` \e -> do
            withStore' $ \db -> setDeliveryJobErrStatus db (deliveryJobId job) (tshow e)
            eToView e
      where
        processDeliveryJob :: MessageDeliveryJob -> CM ()
        processDeliveryJob job =
          case jobScopeImpliedSpec jobScope of
            DJDeliveryJob _includePending
              | not (relayServesGroup gInfo) -> do
                  logWarn "delivery job worker: relay inactive"
                  withStore' $ \db -> setDeliveryJobErrStatus db (deliveryJobId job) "relay inactive"
              | otherwise -> do
                  sendBodyToMembers
                  withStore' $ \db -> updateDeliveryJobStatus db jobId DJSComplete
            -- DJRelayRemoved is allowed when RSInactive - it forwards XGrpMemDel about relay's own deletion
            DJRelayRemoved
              | workerScope /= DWSGroup ->
                  throwChatError $ CEInternalError "delivery job worker: relay removed job in wrong worker scope"
              | otherwise -> do
                  sendBodyToMembers
                  deleteGroupConnections user gInfo True
                  withStore' $ \db -> updateDeliveryJobStatus db jobId DJSComplete
          where
            MessageDeliveryJob {jobId, jobScope, senderGMIds, body, cursorGMId_ = startingCursor} = job
            singleSenderGMId_ = case senderGMIds of
              [s] -> Just s
              _ -> Nothing
            sendBodyToMembers :: CM ()
            sendBodyToMembers
              -- channel
              | useRelays' gInfo = case jobScope of
                  -- there's no member review in channels, so job spec includePending is ignored
                  DJSGroup {} -> do
                    bucketSize <- asks $ deliveryBucketSize . config
                    senders <- withStore' $ \db ->
                      fmap catMaybes . forM senderGMIds $ \sId ->
                        fmap (join . eitherToMaybe) . runExceptT $ do
                          sender <- getNonRemovedMemberById db cxt user sId
                          -- owners are already known to every member (group link + owner-intro in introduceInChannel),
                          -- so we never disseminate their profile (redundant, and races with joins re-announcing the owner)
                          if memberRole' sender == GROwner
                            then pure Nothing
                            else do
                              vec <- getMemberRelationsVector db sender
                              pure $ Just (sender, vec)
                    let missingSenders = length senderGMIds - length senders
                    when (missingSenders > 0) $
                      logInfo $ "delivery job " <> tshow jobId <> ": " <> tshow missingSenders <> " senders missing; skipping their profile prepend"
                    -- Small profiles ride inline (extBody); the rest spill
                    -- into standalone batches that ship before the body.
                    (extBody, inBodySenders, overflowBatches, activeSenders) <-
                      if null senders
                        then pure (body, [], [], [])
                        else do
                          -- all members' profiles disseminate; privileged key/role come from the roster, not here
                          let (encoderErrs, validLabeled) = partitionEithers [(\bs -> (s, bs)) <$> encodeMemberNew (vr cxt) gInfo s | (s, _) <- senders]
                              (extBody', inBody, overflowLabeled, large1) = batchProfilesWithBody maxEncodedMsgLength body validLabeled
                              (overflowBatches', large2) = batchProfiles maxEncodedMsgLength overflowLabeled
                              packerErrs = [ChatError (CEInternalError $ "oversized profile element for member " <> show (groupMemberId' s)) | s <- large1 <> large2]
                              allErrs = encoderErrs <> packerErrs
                          unless (null allErrs) $ do
                            logInfo $ "delivery job " <> tshow jobId <> ": dropping " <> tshow (length allErrs) <> " oversized profile element(s)"
                            toView $ CEvtChatErrors allErrs
                          let active = inBody <> concatMap snd overflowBatches'
                          pure (extBody', inBody, overflowBatches', active)
                    -- Per-job constants — independent of the cursor page in sendLoop.
                    let senderVec = M.fromList [(groupMemberId' s, v) | (s, v) <- senders]
                        -- Body IDs: 0 = plain body, 1 = extBody, 2.. = overflow batches in order.
                        overflowWithIds = zip [2 :: Int ..] overflowBatches
                    sendLoop bucketSize startingCursor senderVec overflowWithIds inBodySenders extBody activeSenders
                    where
                      sendLoop :: Int -> Maybe GroupMemberId -> Map GroupMemberId ByteString -> [(Int, (ByteString, [GroupMember]))] -> [GroupMember] -> ByteString -> [GroupMember] -> CM ()
                      sendLoop bucketSize cursorGMId_ senderVec overflowWithIds inBodySenders extBody activeSenders = do
                        mems <- withStore' $ \db -> getGroupMembersByCursor db cxt user gInfo cursorGMId_ singleSenderGMId_ bucketSize
                        unless (null mems) $ do
                          let msgReqs = buildMsgReqs mems
                          unless (null msgReqs) $ void $ withAgent (`sendMessages` msgReqs)
                          -- Mark only (sender, recipient) pairs where the bit was MRNew —
                          -- skip recipients already MRIntroduced (steady-case savings).
                          let readyMems = [m | m <- mems, isJust (readyMemberConn m)]
                              markFor sender = do
                                vec <- M.lookup (groupMemberId' sender) senderVec
                                let ms = [(indexInGroup r, (IDSubjectIntroduced, MRIntroduced)) | r <- readyMems, getRelation (indexInGroup r) vec == MRNew]
                                if null ms then Nothing else Just (sender, ms)
                              senderMarks = mapMaybe markFor activeSenders
                          unless (null senderMarks) $
                            withStore' $ \db ->
                              forM_ senderMarks $ \(sender, ms) ->
                                setMemberVectorNewRelations db sender ms
                          let cursorGMId' = groupMemberId' $ last mems
                          withStore' $ \db -> updateDeliveryJobCursor db jobId cursorGMId'
                          unless (length mems < bucketSize) $
                            sendLoop bucketSize (Just cursorGMId') senderVec overflowWithIds inBodySenders extBody activeSenders
                        where
                          -- First recipient needing body i carries VRValue (Just i); rest use VRRef i.
                          -- First piece per connection: aConnId; rest: empty (agent convention).
                          buildMsgReqs :: [GroupMember] -> [MsgReq]
                          buildMsgReqs mems = reverse . snd $ foldl' addRecipient (IS.empty, []) mems
                            where
                              addRecipient acc r = case readyMemberConn r of
                                Just (_, conn) -> snd $ foldl' (addPiece conn) (0 :: Int, acc) (recipientBodyPieces r)
                                Nothing -> acc
                              addPiece conn (k, (issued, reqs)) (bid, msgBody) =
                                let vor
                                      | IS.member bid issued = VRRef bid
                                      | otherwise = VRValue (Just bid) msgBody
                                    issued' = IS.insert bid issued
                                    connId = if k == 0 then aConnId conn else B.empty
                                 in (k + 1, (issued', (connId, PQEncOff, MsgFlags False, vor) : reqs))
                              recipientBodyPieces r =
                                [(i, b) | (i, (b, ss)) <- overflowWithIds, any missing ss]
                                  <> [if any missing inBodySenders then (1, extBody) else (0, body)]
                                where
                                  missing s = case M.lookup (groupMemberId' s) senderVec of
                                    Just vec -> getRelation (indexInGroup r) vec == MRNew
                                    Nothing -> True
                  DJSMemberSupport scopeGMId -> do
                    -- for member support scope we just load all recipients in one go, without cursor
                    modMs <- withStore' $ \db -> getGroupModerators db cxt user gInfo
                    let moderatorFilter m =
                          memberCurrent m
                            && maxVersion (memberChatVRange m) >= groupKnockingVersion
                            && Just (groupMemberId' m) /= singleSenderGMId_
                        modMs' = filter moderatorFilter modMs
                    mems <-
                      if Just scopeGMId == singleSenderGMId_
                        then pure modMs'
                        else do
                          scopeMem <- withStore $ \db -> getGroupMemberById db cxt user scopeGMId
                          pure $ scopeMem : modMs'
                    unless (null mems) $ deliver body mems
              -- fully connected group
              | otherwise = case singleSenderGMId_ of
                  Nothing -> throwChatError $ CEInternalError "delivery job worker: singleSenderGMId is required when not using relays"
                  Just sId -> do
                    sender <- withStore $ \db -> getGroupMemberById db cxt user sId
                    ms <- buildMemberList sender
                    unless (null ms) $ deliver body ms
                    where
                      buildMemberList sender = do
                        vec <- withStore (`getMemberRelationsVector` sender)
                        -- this excludes the sender
                        let introducedMemsIdxs = getRelationsIndexes MRIntroduced vec
                        case jobScope of
                          DJSGroup {jobSpec} -> do
                            ms <- withStore' $ \db -> getGroupMembersByIndexes db cxt user gInfo introducedMemsIdxs
                            pure $ filter shouldForwardTo ms
                            where
                              shouldForwardTo m
                                | jobSpecImpliedPending jobSpec = memberCurrentOrPending m
                                | otherwise = memberCurrent m
                          DJSMemberSupport scopeGMId -> do
                            ms <- withStore' $ \db -> getSupportScopeMembersByIndexes db cxt user gInfo scopeGMId introducedMemsIdxs
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
  cxt <- chatStoreCxt
  (user, uclId) <- withStore $ \db -> do
    user <- getRelayUser db
    UserContactLink {userContactLinkId} <- getUserAddress db user
    pure (user, userContactLinkId)
  delayThreads <- liftIO TM.emptyIO
  forever $ do
    lift $ waitForWork doWork
    runRelayRequestOperation delayThreads cxt user uclId
  where
    runRelayRequestOperation :: TM.TMap GroupId (TMVar (Weak ThreadId)) -> StoreCxt -> User -> Int64 -> CM ()
    runRelayRequestOperation delayThreads cxt user uclId =
      withWork_ a doWork getReadyRelayRequest $
        \(groupId, rrd) -> do
          ChatConfig {relayRequestExpiry} <- asks config
          liftIO $ waitWhileSuspended a
          liftIO $ waitForUserNetwork a
          processRelayRequest groupId rrd `catchAllErrors` retryTmpError relayRequestExpiry groupId rrd
      where
        getReadyRelayRequest :: CM (Either StoreError (Maybe (GroupId, RelayRequestData)))
        getReadyRelayRequest =
          withStore' getNextPendingRelayRequest >>= \case
            Right (Just (groupId, rrd@RelayRequestData {reqExecuteAt})) -> do
              currentTs <- liftIO getCurrentTime
              let delay = diffUTCTime reqExecuteAt currentTs
              if delay <= 1
                then pure $ Right (Just (groupId, rrd))
                else Right Nothing <$ scheduleRequest groupId delay
            r -> pure r
        scheduleRequest :: GroupId -> NominalDiffTime -> CM ()
        scheduleRequest groupId delay = do
          v_ <- liftIO $ atomically $
            ifM
              (isNothing <$> TM.lookup groupId delayThreads)
              (newEmptyTMVar >>= \v -> TM.insert groupId v delayThreads $> Just v)
              (pure Nothing)
          forM_ v_ $ \v -> do
            tId <- liftIO $ forkIO $ do
              threadDelay' $ diffToMicroseconds delay
              atomically $ TM.delete groupId delayThreads
              void $ atomically $ tryPutTMVar doWork ()
            weakTId <- liftIO $ mkWeakThreadId tId
            liftIO $ atomically $ putTMVar v weakTId
        retryTmpError :: (Int, NominalDiffTime) -> GroupId -> RelayRequestData -> ChatError -> CM ()
        retryTmpError (retriesThreshold, ttl) groupId RelayRequestData {reqDelay, reqRetries, reqCreatedAt} = \case
          ChatErrorAgent {agentError} | temporaryOrHostError agentError -> do
            currentTs <- liftIO getCurrentTime
            if reqRetries >= retriesThreshold && diffUTCTime currentTs reqCreatedAt >= ttl
              then withStore' $ \db -> setRelayRequestErr db groupId "expired"
              else do
                ri <- asks $ relayRequestRetryInterval . config
                let executeAt = addUTCTime (fromIntegral reqDelay / 1000000) currentTs
                    nextDelay = nextRetryDelay 0 reqDelay ri
                withStore' $ \db -> updateRelayRequestRetries db groupId nextDelay executeAt
          e -> do
            withStore' $ \db -> setRelayRequestErr db groupId (tshow e)
            eToView e
        processRelayRequest :: GroupId -> RelayRequestData -> CM ()
        processRelayRequest groupId rrd = do
          (gInfo, groupLink_) <- withStore $ \db -> do
            gInfo <- getGroupInfo db cxt user groupId
            groupLink_ <- liftIO $ runExceptT $ getGroupLink db user gInfo
            pure (gInfo, groupLink_)
          -- Check if relay link already exists (recovery case)
          case groupLink_ of
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
              (FixedLinkData {linkEntityId, rootKey}, cData@(ContactLinkData _ UserContactData {owners})) <- getShortLinkConnReq' NRMBackground user reqGroupLink
              liftIO (decodeLinkUserData cData) >>= \case
                Nothing -> throwChatError $ CEException "getLinkDataCreateRelayLink: no group link data"
                Just GroupShortLinkData {groupProfile = gp@GroupProfile {publicGroup}} -> do
                  pg <- case (linkEntityId, publicGroup) of
                    (Just entityId, Just pg@PublicGroupProfile {publicGroupId})
                      | B64UrlByteString entityId == publicGroupId -> pure pg
                    _ -> throwChatError $ CEException "getLinkDataCreateRelayLink: linkEntityId does not match profile publicGroupId"
                  validateGroupProfile gp
                  ((_, memberPrivKey), sLnk) <- createRelayLink gInfo
                  gInfo' <- withStore $ \db -> do
                    void $ updateGroupProfile db user gInfo gp
                    updateRelayGroupKeys db user gInfo pg rootKey memberPrivKey owners
                    getGroupInfo db cxt user groupId
                  pure (gInfo', sLnk)
              where
                validateGroupProfile :: GroupProfile -> CM ()
                validateGroupProfile _groupProfile = do
                  -- TODO [relays] relay: validate group profile, verify owner's signature
                  pure ()
                createRelayLink :: GroupInfo -> CM (C.KeyPairEd25519, ShortLinkContact)
                createRelayLink gi = do
                  let GroupInfo {membership} = gi
                      GroupMember {memberId = MemberId relayMemId, memberProfile = p} = membership
                  gVar <- asks random
                  groupLinkId <- GroupLinkId <$> drgRandomBytes 16
                  subMode <- chatReadVar subscriptionMode
                  sigKeys <- liftIO $ atomically $ C.generateKeyPair gVar
                  let crClientData = encodeJSON $ CRDataGroup groupLinkId
                  -- prepare link with relayMemId as linkEntityId (no server request)
                  (ccLink, preparedParams) <- withAgent $ \a' -> prepareConnectionLink a' (aUserId user) sigKeys relayMemId True (Just crClientData) Nothing
                  ccLink' <- setShortLinkType CCTGroup <$> shortenCreatedLink ccLink
                  sLnk <- case connShortLink' ccLink' of
                    Just sl -> pure sl
                    Nothing -> throwChatError $ CEException "failed to create relay link: no short link"
                  let userData = encodeShortLinkData $ RelayShortLinkData {relayProfile = fromLocalProfile p}
                      userLinkData = UserContactLinkData UserContactData {direct = True, owners = [], relays = [], userData}
                  -- create connection with prepared link (single network call)
                  connId <- withAgent $ \a' -> createConnectionForLink a' NRMBackground (aUserId user) True ccLink preparedParams userLinkData CR.IKPQOff subMode
                  -- TODO [relays] starting role should be communicated in protocol from owner to relays
                  subRole <- asks $ channelSubscriberRole . config
                  void $ withFastStore $ \db -> createGroupLink db gVar user gi connId ccLink' groupLinkId subRole subMode
                  pure (sigKeys, sLnk)
            acceptOwnerConnection :: RelayRequestData -> GroupInfo -> ShortLinkContact -> CM ()
            acceptOwnerConnection RelayRequestData {relayInvId, reqChatVRange} gi relayLink = do
              ownerMember <- withStore $ \db -> getHostMember db cxt user groupId
              void $ acceptRelayJoinRequestAsync user uclId gi ownerMember relayInvId reqChatVRange relayLink
