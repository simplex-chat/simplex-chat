{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store.Files
  ( getLiveSndFileTransfers,
    getLiveRcvFileTransfers,
    getPendingSndChunks,
    createSndDirectFileTransfer,
    createSndDirectFTConnection,
    createSndGroupFileTransfer,
    createSndGroupFileTransferConnection,
    createSndDirectInlineFT,
    createSndGroupInlineFT,
    updateSndDirectFTDelivery,
    updateSndGroupFTDelivery,
    getSndFTViaMsgDelivery,
    createSndFileTransferXFTP,
    createSndFTDescrXFTP,
    setSndFTPrivateSndDescr,
    updateSndFTDescrXFTP,
    createExtraSndFTDescrs,
    updateSndFTDeliveryXFTP,
    setSndFTAgentDeleted,
    getXFTPSndFileDBId,
    getXFTPRcvFileDBId,
    updateFileCancelled,
    updateCIFileStatus,
    getSharedMsgIdByFileId,
    getFileIdBySharedMsgId,
    getGroupFileIdBySharedMsgId,
    getDirectFileIdBySharedMsgId,
    getChatRefByFileId,
    updateSndFileStatus,
    createSndFileChunk,
    updateSndFileChunkMsg,
    updateSndFileChunkSent,
    deleteSndFileChunks,
    createRcvFileTransfer,
    createRcvGroupFileTransfer,
    appendRcvFD,
    getRcvFileDescrByRcvFileId,
    getRcvFileDescrBySndFileId,
    updateRcvFileAgentId,
    getRcvFileTransferById,
    getRcvFileTransfer,
    acceptRcvFileTransfer,
    getContactByFileId,
    acceptRcvInlineFT,
    startRcvInlineFT,
    xftpAcceptRcvFT,
    setRcvFileToReceive,
    setFileCryptoArgs,
    removeFileCryptoArgs,
    getRcvFilesToReceive,
    setRcvFTAgentDeleted,
    updateRcvFileStatus,
    createRcvFileChunk,
    updatedRcvFileChunkStored,
    deleteRcvFileChunks,
    updateFileTransferChatItemId,
    getFileTransfer,
    getFileTransferProgress,
    getFileTransferMeta,
    getSndFileTransfer,
    getSndFileTransfers,
    getContactFileInfo,
    getLocalCryptoFile,
    updateDirectCIFileStatus,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Either (rights)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime (..), getCurrentTime, nominalDay)
import Data.Type.Equality
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Messages
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Util (week)
import Simplex.Messaging.Agent.Protocol (AgentMsgId, ConnId, UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Protocol (SubscriptionMode (..))
import Simplex.Messaging.Version (VersionRange)

getLiveSndFileTransfers :: DB.Connection -> User -> IO [SndFileTransfer]
getLiveSndFileTransfers db User {userId} = do
  cutoffTs <- addUTCTime (-week) <$> getCurrentTime
  fileIds :: [Int64] <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT DISTINCT f.file_id
          FROM files f
          JOIN snd_files s USING (file_id)
          WHERE f.user_id = ?
            AND s.file_status IN (?, ?, ?)
            AND s.file_descr_id IS NULL
            AND s.file_inline IS NULL
            AND s.created_at > ?
        |]
        (userId, FSNew, FSAccepted, FSConnected, cutoffTs)
  concatMap (filter liveTransfer) . rights <$> mapM (getSndFileTransfers_ db userId) fileIds
  where
    liveTransfer :: SndFileTransfer -> Bool
    liveTransfer SndFileTransfer {fileStatus} = fileStatus `elem` [FSNew, FSAccepted, FSConnected]

getLiveRcvFileTransfers :: DB.Connection -> User -> IO [RcvFileTransfer]
getLiveRcvFileTransfers db user@User {userId} = do
  cutoffTs <- addUTCTime (-week) <$> getCurrentTime
  fileIds :: [Int64] <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT f.file_id
          FROM files f
          JOIN rcv_files r USING (file_id)
          WHERE f.user_id = ? AND r.file_status IN (?, ?)
            AND r.rcv_file_inline IS NULL
            AND r.file_descr_id IS NULL
            AND r.created_at > ?
        |]
        (userId, FSAccepted, FSConnected, cutoffTs)
  rights <$> mapM (runExceptT . getRcvFileTransfer db user) fileIds

getPendingSndChunks :: DB.Connection -> Int64 -> Int64 -> IO [Integer]
getPendingSndChunks db fileId connId =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chunk_number
        FROM snd_file_chunks
        WHERE file_id = ? AND connection_id = ? AND chunk_agent_msg_id IS NULL
        ORDER BY chunk_number
      |]
      (fileId, connId)

createSndDirectFileTransfer :: DB.Connection -> UserId -> Contact -> FilePath -> FileInvitation -> Maybe ConnId -> Integer -> SubscriptionMode -> IO FileTransferMeta
createSndDirectFileTransfer db userId Contact {contactId} filePath FileInvitation {fileName, fileSize, fileInline} acId_ chunkSize subMode = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO files (user_id, contact_id, file_name, file_path, file_size, chunk_size, file_inline, ci_file_status, protocol, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?)"
    ((userId, contactId, fileName, filePath, fileSize, chunkSize) :. (fileInline, CIFSSndStored, FPSMP, currentTs, currentTs))
  fileId <- insertedRowId db
  forM_ acId_ $ \acId -> do
    Connection {connId} <- createSndFileConnection_ db userId fileId acId subMode
    let fileStatus = FSNew
    DB.execute
      db
      "INSERT INTO snd_files (file_id, file_status, file_inline, connection_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (fileId, fileStatus, fileInline, connId, currentTs, currentTs)
  pure FileTransferMeta {fileId, xftpSndFile = Nothing, fileName, filePath, fileSize, fileInline, chunkSize, cancelled = False}

createSndDirectFTConnection :: DB.Connection -> User -> Int64 -> (CommandId, ConnId) -> SubscriptionMode -> IO ()
createSndDirectFTConnection db user@User {userId} fileId (cmdId, acId) subMode = do
  currentTs <- getCurrentTime
  Connection {connId} <- createSndFileConnection_ db userId fileId acId subMode
  setCommandConnId db user cmdId connId
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, connection_id, created_at, updated_at) VALUES (?,?,?,?,?)"
    (fileId, FSAccepted, connId, currentTs, currentTs)

createSndGroupFileTransfer :: DB.Connection -> UserId -> GroupInfo -> FilePath -> FileInvitation -> Integer -> IO FileTransferMeta
createSndGroupFileTransfer db userId GroupInfo {groupId} filePath FileInvitation {fileName, fileSize, fileInline} chunkSize = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO files (user_id, group_id, file_name, file_path, file_size, chunk_size, file_inline, ci_file_status, protocol, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?)"
    ((userId, groupId, fileName, filePath, fileSize, chunkSize) :. (fileInline, CIFSSndStored, FPSMP, currentTs, currentTs))
  fileId <- insertedRowId db
  pure FileTransferMeta {fileId, xftpSndFile = Nothing, fileName, filePath, fileSize, fileInline, chunkSize, cancelled = False}

createSndGroupFileTransferConnection :: DB.Connection -> User -> Int64 -> (CommandId, ConnId) -> GroupMember -> SubscriptionMode -> IO ()
createSndGroupFileTransferConnection db user@User {userId} fileId (cmdId, acId) GroupMember {groupMemberId} subMode = do
  currentTs <- getCurrentTime
  Connection {connId} <- createSndFileConnection_ db userId fileId acId subMode
  setCommandConnId db user cmdId connId
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, connection_id, group_member_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (fileId, FSAccepted, connId, groupMemberId, currentTs, currentTs)

createSndDirectInlineFT :: DB.Connection -> Contact -> FileTransferMeta -> ExceptT StoreError IO SndFileTransfer
createSndDirectInlineFT _ Contact {localDisplayName, activeConn = Nothing} _ = throwError $ SEContactNotReady localDisplayName
createSndDirectInlineFT db Contact {localDisplayName = n, activeConn = Just Connection {connId, agentConnId}} FileTransferMeta {fileId, fileName, filePath, fileSize, chunkSize, fileInline} = liftIO $ do
  currentTs <- getCurrentTime
  let fileStatus = FSConnected
      fileInline' = Just $ fromMaybe IFMOffer fileInline
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, file_inline, connection_id, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (fileId, fileStatus, fileInline', connId, currentTs, currentTs)
  pure SndFileTransfer {fileId, fileName, filePath, fileSize, chunkSize, recipientDisplayName = n, connId, agentConnId, groupMemberId = Nothing, fileStatus, fileDescrId = Nothing, fileInline = fileInline'}

createSndGroupInlineFT :: DB.Connection -> GroupMember -> Connection -> FileTransferMeta -> IO SndFileTransfer
createSndGroupInlineFT db GroupMember {groupMemberId, localDisplayName = n} Connection {connId, agentConnId} FileTransferMeta {fileId, fileName, filePath, fileSize, chunkSize, fileInline} = do
  currentTs <- getCurrentTime
  let fileStatus = FSConnected
      fileInline' = Just $ fromMaybe IFMOffer fileInline
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, file_inline, connection_id, group_member_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
    (fileId, fileStatus, fileInline', connId, groupMemberId, currentTs, currentTs)
  pure SndFileTransfer {fileId, fileName, filePath, fileSize, chunkSize, recipientDisplayName = n, connId, agentConnId, groupMemberId = Just groupMemberId, fileStatus, fileDescrId = Nothing, fileInline = fileInline'}

updateSndDirectFTDelivery :: DB.Connection -> Contact -> FileTransferMeta -> Int64 -> ExceptT StoreError IO ()
updateSndDirectFTDelivery _ Contact {localDisplayName, activeConn = Nothing} _ _ = throwError $ SEContactNotReady localDisplayName
updateSndDirectFTDelivery db Contact {activeConn = Just Connection {connId}} FileTransferMeta {fileId} msgDeliveryId =
  liftIO $
    DB.execute
      db
      "UPDATE snd_files SET last_inline_msg_delivery_id = ? WHERE connection_id = ? AND file_id = ? AND file_inline IS NOT NULL"
      (msgDeliveryId, connId, fileId)

updateSndGroupFTDelivery :: DB.Connection -> GroupMember -> Connection -> FileTransferMeta -> Int64 -> IO ()
updateSndGroupFTDelivery db GroupMember {groupMemberId} Connection {connId} FileTransferMeta {fileId} msgDeliveryId =
  DB.execute
    db
    "UPDATE snd_files SET last_inline_msg_delivery_id = ? WHERE group_member_id = ? AND connection_id = ? AND file_id = ? AND file_inline IS NOT NULL"
    (msgDeliveryId, groupMemberId, connId, fileId)

getSndFTViaMsgDelivery :: DB.Connection -> User -> Connection -> AgentMsgId -> IO (Maybe SndFileTransfer)
getSndFTViaMsgDelivery db User {userId} Connection {connId, agentConnId} agentMsgId = do
  (sndFileTransfer_ <=< listToMaybe)
    <$> DB.query
      db
      [sql|
        SELECT s.file_id, s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, s.file_descr_id, s.file_inline, s.group_member_id, c.local_display_name, m.local_display_name
        FROM msg_deliveries d
        JOIN snd_files s ON s.connection_id = d.connection_id AND s.last_inline_msg_delivery_id = d.msg_delivery_id
        JOIN files f ON f.file_id = s.file_id
        LEFT JOIN contacts c USING (contact_id)
        LEFT JOIN group_members m USING (group_member_id)
        WHERE d.connection_id = ? AND d.agent_msg_id = ? AND f.user_id = ?
          AND (s.file_descr_id IS NOT NULL OR s.file_inline IS NOT NULL)
      |]
      (connId, agentMsgId, userId)
  where
    sndFileTransfer_ :: (Int64, FileStatus, String, Integer, Integer, FilePath, Maybe Int64, Maybe InlineFileMode, Maybe Int64, Maybe ContactName, Maybe ContactName) -> Maybe SndFileTransfer
    sndFileTransfer_ (fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileDescrId, fileInline, groupMemberId, contactName_, memberName_) =
      (\n -> SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileDescrId, fileInline, groupMemberId, recipientDisplayName = n, connId, agentConnId})
        <$> (contactName_ <|> memberName_)

createSndFileTransferXFTP :: DB.Connection -> User -> ContactOrGroup -> CryptoFile -> FileInvitation -> AgentSndFileId -> Integer -> IO FileTransferMeta
createSndFileTransferXFTP db User {userId} contactOrGroup (CryptoFile filePath cryptoArgs) FileInvitation {fileName, fileSize} agentSndFileId chunkSize = do
  currentTs <- getCurrentTime
  let xftpSndFile = Just XFTPSndFile {agentSndFileId, privateSndFileDescr = Nothing, agentSndFileDeleted = False, cryptoArgs}
  DB.execute
    db
    "INSERT INTO files (contact_id, group_id, user_id, file_name, file_path, file_crypto_key, file_crypto_nonce, file_size, chunk_size, agent_snd_file_id, ci_file_status, protocol, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    (contactAndGroupIds contactOrGroup :. (userId, fileName, filePath, CF.fileKey <$> cryptoArgs, CF.fileNonce <$> cryptoArgs, fileSize, chunkSize, agentSndFileId, CIFSSndStored, FPXFTP, currentTs, currentTs))
  fileId <- insertedRowId db
  pure FileTransferMeta {fileId, xftpSndFile, fileName, filePath, fileSize, fileInline = Nothing, chunkSize, cancelled = False}

createSndFTDescrXFTP :: DB.Connection -> User -> Maybe GroupMember -> Connection -> FileTransferMeta -> FileDescr -> IO ()
createSndFTDescrXFTP db User {userId} m Connection {connId} FileTransferMeta {fileId} FileDescr {fileDescrText, fileDescrPartNo, fileDescrComplete} = do
  currentTs <- getCurrentTime
  let fileStatus = FSNew
  DB.execute
    db
    "INSERT INTO xftp_file_descriptions (user_id, file_descr_text, file_descr_part_no, file_descr_complete, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (userId, fileDescrText, fileDescrPartNo, fileDescrComplete, currentTs, currentTs)
  fileDescrId <- insertedRowId db
  DB.execute
    db
    "INSERT INTO snd_files (file_id, file_status, file_descr_id, group_member_id, connection_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
    (fileId, fileStatus, fileDescrId, groupMemberId' <$> m, connId, currentTs, currentTs)

setSndFTPrivateSndDescr :: DB.Connection -> User -> FileTransferId -> Text -> IO ()
setSndFTPrivateSndDescr db User {userId} fileId sfdText = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE files SET private_snd_file_descr = ?, updated_at = ? WHERE user_id = ? AND file_id = ?"
    (sfdText, currentTs, userId, fileId)

updateSndFTDescrXFTP :: DB.Connection -> User -> SndFileTransfer -> Text -> IO ()
updateSndFTDescrXFTP db user@User {userId} sft@SndFileTransfer {fileId, fileDescrId} rfdText = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE xftp_file_descriptions
      SET file_descr_text = ?, file_descr_part_no = ?, file_descr_complete = ?, updated_at = ?
      WHERE user_id = ? AND file_descr_id = ?
    |]
    (rfdText, 1 :: Int, True, currentTs, userId, fileDescrId)
  updateCIFileStatus db user fileId $ CIFSSndTransfer 1 1
  updateSndFileStatus db sft FSConnected

createExtraSndFTDescrs :: DB.Connection -> User -> FileTransferId -> [Text] -> IO ()
createExtraSndFTDescrs db User {userId} fileId rfdTexts = do
  currentTs <- getCurrentTime
  forM_ rfdTexts $ \rfdText ->
    DB.execute
      db
      "INSERT INTO extra_xftp_file_descriptions (file_id, user_id, file_descr_text, created_at, updated_at) VALUES (?,?,?,?,?)"
      (fileId, userId, rfdText, currentTs, currentTs)

updateSndFTDeliveryXFTP :: DB.Connection -> SndFileTransfer -> Int64 -> IO ()
updateSndFTDeliveryXFTP db SndFileTransfer {connId, fileId, fileDescrId} msgDeliveryId =
  DB.execute
    db
    "UPDATE snd_files SET last_inline_msg_delivery_id = ? WHERE connection_id = ? AND file_id = ? AND file_descr_id = ?"
    (msgDeliveryId, connId, fileId, fileDescrId)

setSndFTAgentDeleted :: DB.Connection -> User -> FileTransferId -> IO ()
setSndFTAgentDeleted db User {userId} fileId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE files SET agent_snd_file_deleted = 1, updated_at = ? WHERE user_id = ? AND file_id = ?"
    (currentTs, userId, fileId)

getXFTPSndFileDBId :: DB.Connection -> User -> AgentSndFileId -> ExceptT StoreError IO FileTransferId
getXFTPSndFileDBId db User {userId} aSndFileId =
  ExceptT . firstRow fromOnly (SESndFileNotFoundXFTP aSndFileId) $
    DB.query db "SELECT file_id FROM files WHERE user_id = ? AND agent_snd_file_id = ?" (userId, aSndFileId)

getXFTPRcvFileDBId :: DB.Connection -> AgentRcvFileId -> ExceptT StoreError IO FileTransferId
getXFTPRcvFileDBId db aRcvFileId =
  ExceptT . firstRow fromOnly (SERcvFileNotFoundXFTP aRcvFileId) $
    DB.query db "SELECT file_id FROM rcv_files WHERE agent_rcv_file_id = ?" (Only aRcvFileId)

updateFileCancelled :: MsgDirectionI d => DB.Connection -> User -> Int64 -> CIFileStatus d -> IO ()
updateFileCancelled db User {userId} fileId ciFileStatus = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE files SET cancelled = 1, ci_file_status = ?, updated_at = ? WHERE user_id = ? AND file_id = ?" (ciFileStatus, currentTs, userId, fileId)

updateCIFileStatus :: MsgDirectionI d => DB.Connection -> User -> Int64 -> CIFileStatus d -> IO ()
updateCIFileStatus db User {userId} fileId ciFileStatus = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE files SET ci_file_status = ?, updated_at = ? WHERE user_id = ? AND file_id = ?" (ciFileStatus, currentTs, userId, fileId)

getSharedMsgIdByFileId :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO SharedMsgId
getSharedMsgIdByFileId db userId fileId =
  ExceptT . firstRow fromOnly (SESharedMsgIdNotFoundByFileId fileId) $
    DB.query
      db
      [sql|
        SELECT i.shared_msg_id
        FROM chat_items i
        JOIN files f ON f.chat_item_id = i.chat_item_id
        WHERE f.user_id = ? AND f.file_id = ?
      |]
      (userId, fileId)

getFileIdBySharedMsgId :: DB.Connection -> UserId -> Int64 -> SharedMsgId -> ExceptT StoreError IO Int64
getFileIdBySharedMsgId db userId contactId sharedMsgId =
  ExceptT . firstRow fromOnly (SEFileIdNotFoundBySharedMsgId sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT f.file_id
        FROM files f
        JOIN chat_items i ON i.chat_item_id = f.chat_item_id
        WHERE i.user_id = ? AND i.contact_id = ? AND i.shared_msg_id = ?
      |]
      (userId, contactId, sharedMsgId)

getGroupFileIdBySharedMsgId :: DB.Connection -> UserId -> Int64 -> SharedMsgId -> ExceptT StoreError IO Int64
getGroupFileIdBySharedMsgId db userId groupId sharedMsgId =
  ExceptT . firstRow fromOnly (SEFileIdNotFoundBySharedMsgId sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT f.file_id
        FROM files f
        JOIN chat_items i ON i.chat_item_id = f.chat_item_id
        WHERE i.user_id = ? AND i.group_id = ? AND i.shared_msg_id = ?
      |]
      (userId, groupId, sharedMsgId)

getDirectFileIdBySharedMsgId :: DB.Connection -> User -> Contact -> SharedMsgId -> ExceptT StoreError IO Int64
getDirectFileIdBySharedMsgId db User {userId} Contact {contactId} sharedMsgId =
  ExceptT . firstRow fromOnly (SEFileIdNotFoundBySharedMsgId sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT f.file_id
        FROM files f
        JOIN chat_items i ON i.chat_item_id = f.chat_item_id
        WHERE i.user_id = ? AND i.contact_id = ? AND i.shared_msg_id = ?
      |]
      (userId, contactId, sharedMsgId)

getChatRefByFileId :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO ChatRef
getChatRefByFileId db User {userId} fileId =
  liftIO getChatRef >>= \case
    [(Just contactId, Nothing)] -> pure $ ChatRef CTDirect contactId
    [(Nothing, Just groupId)] -> pure $ ChatRef CTGroup groupId
    _ -> throwError $ SEInternalError "could not retrieve chat ref by file id"
  where
    getChatRef =
      DB.query
        db
        [sql|
          SELECT contact_id, group_id
          FROM files
          WHERE user_id = ? AND file_id = ?
          LIMIT 1
        |]
        (userId, fileId)

createSndFileConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> SubscriptionMode -> IO Connection
createSndFileConnection_ db userId fileId agentConnId subMode = do
  currentTs <- getCurrentTime
  createConnection_ db userId ConnSndFile (Just fileId) agentConnId chatInitialVRange Nothing Nothing Nothing 0 currentTs subMode

updateSndFileStatus :: DB.Connection -> SndFileTransfer -> FileStatus -> IO ()
updateSndFileStatus db SndFileTransfer {fileId, connId} status = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE snd_files SET file_status = ?, updated_at = ? WHERE file_id = ? AND connection_id = ?" (status, currentTs, fileId, connId)

createSndFileChunk :: DB.Connection -> SndFileTransfer -> IO (Maybe Integer)
createSndFileChunk db SndFileTransfer {fileId, connId, fileSize, chunkSize} = do
  chunkNo <- getLastChunkNo
  insertChunk chunkNo
  pure chunkNo
  where
    getLastChunkNo = do
      ns <- DB.query db "SELECT chunk_number FROM snd_file_chunks WHERE file_id = ? AND connection_id = ? AND chunk_sent = 1 ORDER BY chunk_number DESC LIMIT 1" (fileId, connId)
      pure $ case map fromOnly ns of
        [] -> Just 1
        n : _ -> if n * chunkSize >= fileSize then Nothing else Just (n + 1)
    insertChunk chunkNo_ = forM_ chunkNo_ $ \chunkNo -> do
      currentTs <- getCurrentTime
      DB.execute
        db
        "INSERT OR REPLACE INTO snd_file_chunks (file_id, connection_id, chunk_number, created_at, updated_at) VALUES (?,?,?,?,?)"
        (fileId, connId, chunkNo, currentTs, currentTs)

updateSndFileChunkMsg :: DB.Connection -> SndFileTransfer -> Integer -> AgentMsgId -> IO ()
updateSndFileChunkMsg db SndFileTransfer {fileId, connId} chunkNo msgId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE snd_file_chunks
      SET chunk_agent_msg_id = ?, updated_at = ?
      WHERE file_id = ? AND connection_id = ? AND chunk_number = ?
    |]
    (msgId, currentTs, fileId, connId, chunkNo)

updateSndFileChunkSent :: DB.Connection -> SndFileTransfer -> AgentMsgId -> IO ()
updateSndFileChunkSent db SndFileTransfer {fileId, connId} msgId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE snd_file_chunks
      SET chunk_sent = 1, updated_at = ?
      WHERE file_id = ? AND connection_id = ? AND chunk_agent_msg_id = ?
    |]
    (currentTs, fileId, connId, msgId)

deleteSndFileChunks :: DB.Connection -> SndFileTransfer -> IO ()
deleteSndFileChunks db SndFileTransfer {fileId, connId} =
  DB.execute db "DELETE FROM snd_file_chunks WHERE file_id = ? AND connection_id = ?" (fileId, connId)

createRcvFileTransfer :: DB.Connection -> UserId -> Contact -> FileInvitation -> Maybe InlineFileMode -> Integer -> ExceptT StoreError IO RcvFileTransfer
createRcvFileTransfer db userId Contact {contactId, localDisplayName = c} f@FileInvitation {fileName, fileSize, fileConnReq, fileInline, fileDescr} rcvFileInline chunkSize = do
  currentTs <- liftIO getCurrentTime
  rfd_ <- mapM (createRcvFD_ db userId currentTs) fileDescr
  let rfdId = (\RcvFileDescr {fileDescrId} -> fileDescrId) <$> rfd_
      -- cryptoArgs = Nothing here, the decision to encrypt is made when receiving it
      xftpRcvFile = (\rfd -> XFTPRcvFile {rcvFileDescription = rfd, agentRcvFileId = Nothing, agentRcvFileDeleted = False}) <$> rfd_
      fileProtocol = if isJust rfd_ then FPXFTP else FPSMP
  fileId <- liftIO $ do
    DB.execute
      db
      "INSERT INTO files (user_id, contact_id, file_name, file_size, chunk_size, file_inline, ci_file_status, protocol, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?)"
      (userId, contactId, fileName, fileSize, chunkSize, fileInline, CIFSRcvInvitation, fileProtocol, currentTs, currentTs)
    insertedRowId db
  liftIO $
    DB.execute
      db
      "INSERT INTO rcv_files (file_id, file_status, file_queue_info, file_inline, rcv_file_inline, file_descr_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
      (fileId, FSNew, fileConnReq, fileInline, rcvFileInline, rfdId, currentTs, currentTs)
  pure RcvFileTransfer {fileId, xftpRcvFile, fileInvitation = f, fileStatus = RFSNew, rcvFileInline, senderDisplayName = c, chunkSize, cancelled = False, grpMemberId = Nothing, cryptoArgs = Nothing}

createRcvGroupFileTransfer :: DB.Connection -> UserId -> GroupMember -> FileInvitation -> Maybe InlineFileMode -> Integer -> ExceptT StoreError IO RcvFileTransfer
createRcvGroupFileTransfer db userId GroupMember {groupId, groupMemberId, localDisplayName = c} f@FileInvitation {fileName, fileSize, fileConnReq, fileInline, fileDescr} rcvFileInline chunkSize = do
  currentTs <- liftIO getCurrentTime
  rfd_ <- mapM (createRcvFD_ db userId currentTs) fileDescr
  let rfdId = (\RcvFileDescr {fileDescrId} -> fileDescrId) <$> rfd_
      -- cryptoArgs = Nothing here, the decision to encrypt is made when receiving it
      xftpRcvFile = (\rfd -> XFTPRcvFile {rcvFileDescription = rfd, agentRcvFileId = Nothing, agentRcvFileDeleted = False}) <$> rfd_
      fileProtocol = if isJust rfd_ then FPXFTP else FPSMP
  fileId <- liftIO $ do
    DB.execute
      db
      "INSERT INTO files (user_id, group_id, file_name, file_size, chunk_size, file_inline, ci_file_status, protocol, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?)"
      (userId, groupId, fileName, fileSize, chunkSize, fileInline, CIFSRcvInvitation, fileProtocol, currentTs, currentTs)
    insertedRowId db
  liftIO $
    DB.execute
      db
      "INSERT INTO rcv_files (file_id, file_status, file_queue_info, file_inline, rcv_file_inline, group_member_id, file_descr_id, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
      (fileId, FSNew, fileConnReq, fileInline, rcvFileInline, groupMemberId, rfdId, currentTs, currentTs)
  pure RcvFileTransfer {fileId, xftpRcvFile, fileInvitation = f, fileStatus = RFSNew, rcvFileInline, senderDisplayName = c, chunkSize, cancelled = False, grpMemberId = Just groupMemberId, cryptoArgs = Nothing}

createRcvFD_ :: DB.Connection -> UserId -> UTCTime -> FileDescr -> ExceptT StoreError IO RcvFileDescr
createRcvFD_ db userId currentTs FileDescr {fileDescrText, fileDescrPartNo, fileDescrComplete} = do
  when (fileDescrPartNo /= 0) $ throwError SERcvFileInvalidDescrPart
  fileDescrId <- liftIO $ do
    DB.execute
      db
      "INSERT INTO xftp_file_descriptions (user_id, file_descr_text, file_descr_part_no, file_descr_complete, created_at, updated_at) VALUES (?,?,?,?,?,?)"
      (userId, fileDescrText, fileDescrPartNo, fileDescrComplete, currentTs, currentTs)
    insertedRowId db
  pure RcvFileDescr {fileDescrId, fileDescrPartNo, fileDescrText, fileDescrComplete}

appendRcvFD :: DB.Connection -> UserId -> FileTransferId -> FileDescr -> ExceptT StoreError IO RcvFileDescr
appendRcvFD db userId fileId fd@FileDescr {fileDescrText, fileDescrPartNo, fileDescrComplete} = do
  currentTs <- liftIO getCurrentTime
  liftIO (getRcvFileDescrByRcvFileId_ db fileId) >>= \case
    Nothing -> do
      rfd@RcvFileDescr {fileDescrId} <- createRcvFD_ db userId currentTs fd
      liftIO $
        DB.execute
          db
          "UPDATE rcv_files SET file_descr_id = ?, updated_at = ? WHERE file_id = ?"
          (fileDescrId, currentTs, fileId)
      pure rfd
    Just
      RcvFileDescr
        { fileDescrId,
          fileDescrText = rfdText,
          fileDescrPartNo = rfdPNo,
          fileDescrComplete = rfdComplete
        } -> do
        when (fileDescrPartNo /= rfdPNo + 1 || rfdComplete) $ throwError SERcvFileInvalidDescrPart
        let fileDescrText' = rfdText <> fileDescrText
        liftIO $
          DB.execute
            db
            [sql|
              UPDATE xftp_file_descriptions
              SET file_descr_text = ?, file_descr_part_no = ?, file_descr_complete = ?
              WHERE file_descr_id = ?
            |]
            (fileDescrText', fileDescrPartNo, fileDescrComplete, fileDescrId)
        pure RcvFileDescr {fileDescrId, fileDescrText = fileDescrText', fileDescrPartNo, fileDescrComplete}

getRcvFileDescrByRcvFileId :: DB.Connection -> FileTransferId -> ExceptT StoreError IO RcvFileDescr
getRcvFileDescrByRcvFileId db fileId = do
  liftIO (getRcvFileDescrByRcvFileId_ db fileId) >>= \case
    Nothing -> throwError $ SERcvFileDescrNotFound fileId
    Just rfd -> pure rfd

getRcvFileDescrByRcvFileId_ :: DB.Connection -> FileTransferId -> IO (Maybe RcvFileDescr)
getRcvFileDescrByRcvFileId_ db fileId =
  maybeFirstRow toRcvFileDescr $
    DB.query
      db
      [sql|
        SELECT d.file_descr_id, d.file_descr_text, d.file_descr_part_no, d.file_descr_complete
        FROM xftp_file_descriptions d
        JOIN rcv_files f ON f.file_descr_id = d.file_descr_id
        WHERE f.file_id = ?
        LIMIT 1
      |]
      (Only fileId)

getRcvFileDescrBySndFileId :: DB.Connection -> FileTransferId -> ExceptT StoreError IO RcvFileDescr
getRcvFileDescrBySndFileId db fileId = do
  liftIO (getRcvFileDescrBySndFileId_ db fileId) >>= \case
    Nothing -> throwError $ SERcvFileDescrNotFound fileId
    Just rfd -> pure rfd

getRcvFileDescrBySndFileId_ :: DB.Connection -> FileTransferId -> IO (Maybe RcvFileDescr)
getRcvFileDescrBySndFileId_ db fileId =
  maybeFirstRow toRcvFileDescr $
    DB.query
      db
      [sql|
        SELECT d.file_descr_id, d.file_descr_text, d.file_descr_part_no, d.file_descr_complete
        FROM xftp_file_descriptions d
        JOIN snd_files f ON f.file_descr_id = d.file_descr_id
        WHERE f.file_id = ?
        LIMIT 1
      |]
      (Only fileId)

toRcvFileDescr :: (Int64, Text, Int, Bool) -> RcvFileDescr
toRcvFileDescr (fileDescrId, fileDescrText, fileDescrPartNo, fileDescrComplete) =
  RcvFileDescr {fileDescrId, fileDescrText, fileDescrPartNo, fileDescrComplete}

updateRcvFileAgentId :: DB.Connection -> FileTransferId -> Maybe AgentRcvFileId -> IO ()
updateRcvFileAgentId db fileId aFileId = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE rcv_files SET agent_rcv_file_id = ?, updated_at = ? WHERE file_id = ?" (aFileId, currentTs, fileId)

getRcvFileTransferById :: DB.Connection -> FileTransferId -> ExceptT StoreError IO (User, RcvFileTransfer)
getRcvFileTransferById db fileId = do
  user <- getUserByFileId db fileId
  (user,) <$> getRcvFileTransfer db user fileId

getRcvFileTransfer :: DB.Connection -> User -> FileTransferId -> ExceptT StoreError IO RcvFileTransfer
getRcvFileTransfer db User {userId} = getRcvFileTransfer_ db userId

getRcvFileTransfer_ :: DB.Connection -> UserId -> FileTransferId -> ExceptT StoreError IO RcvFileTransfer
getRcvFileTransfer_ db userId fileId = do
  rftRow <-
    ExceptT . firstRow id (SERcvFileNotFound fileId) $
      DB.query
        db
        [sql|
          SELECT r.file_status, r.file_queue_info, r.group_member_id, f.file_name,
            f.file_size, f.chunk_size, f.cancelled, cs.local_display_name, m.local_display_name,
            f.file_path, f.file_crypto_key, f.file_crypto_nonce, r.file_inline, r.rcv_file_inline, r.agent_rcv_file_id, r.agent_rcv_file_deleted, c.connection_id, c.agent_conn_id
          FROM rcv_files r
          JOIN files f USING (file_id)
          LEFT JOIN connections c ON r.file_id = c.rcv_file_id
          LEFT JOIN contacts cs USING (contact_id)
          LEFT JOIN group_members m USING (group_member_id)
          WHERE f.user_id = ? AND f.file_id = ?
        |]
        (userId, fileId)
  rfd_ <- liftIO $ getRcvFileDescrByRcvFileId_ db fileId
  rcvFileTransfer rfd_ rftRow
  where
    rcvFileTransfer ::
      Maybe RcvFileDescr ->
      (FileStatus, Maybe ConnReqInvitation, Maybe Int64, String, Integer, Integer, Maybe Bool) :. (Maybe ContactName, Maybe ContactName, Maybe FilePath, Maybe C.SbKey, Maybe C.CbNonce, Maybe InlineFileMode, Maybe InlineFileMode, Maybe AgentRcvFileId, Bool) :. (Maybe Int64, Maybe AgentConnId) ->
      ExceptT StoreError IO RcvFileTransfer
    rcvFileTransfer rfd_ ((fileStatus', fileConnReq, grpMemberId, fileName, fileSize, chunkSize, cancelled_) :. (contactName_, memberName_, filePath_, fileKey, fileNonce, fileInline, rcvFileInline, agentRcvFileId, agentRcvFileDeleted) :. (connId_, agentConnId_)) =
      case contactName_ <|> memberName_ of
        Nothing -> throwError $ SERcvFileInvalid fileId
        Just name -> do
          case fileStatus' of
            FSNew -> pure $ ft name RFSNew
            FSAccepted -> ft name . RFSAccepted <$> rfi
            FSConnected -> ft name . RFSConnected <$> rfi
            FSComplete -> ft name . RFSComplete <$> rfi
            FSCancelled -> ft name . RFSCancelled <$> rfi_
      where
        ft senderDisplayName fileStatus =
          let fileInvitation = FileInvitation {fileName, fileSize, fileDigest = Nothing, fileConnReq, fileInline, fileDescr = Nothing}
              cryptoArgs = CFArgs <$> fileKey <*> fileNonce
              xftpRcvFile = (\rfd -> XFTPRcvFile {rcvFileDescription = rfd, agentRcvFileId, agentRcvFileDeleted}) <$> rfd_
           in RcvFileTransfer {fileId, xftpRcvFile, fileInvitation, fileStatus, rcvFileInline, senderDisplayName, chunkSize, cancelled, grpMemberId, cryptoArgs}
        rfi = maybe (throwError $ SERcvFileInvalid fileId) pure =<< rfi_
        rfi_ = case (filePath_, connId_, agentConnId_) of
          (Just filePath, connId, agentConnId) -> pure $ Just RcvFileInfo {filePath, connId, agentConnId}
          _ -> pure Nothing
        cancelled = fromMaybe False cancelled_

acceptRcvFileTransfer :: DB.Connection -> VersionRange -> User -> Int64 -> (CommandId, ConnId) -> ConnStatus -> FilePath -> SubscriptionMode -> ExceptT StoreError IO AChatItem
acceptRcvFileTransfer db vr user@User {userId} fileId (cmdId, acId) connStatus filePath subMode = ExceptT $ do
  currentTs <- getCurrentTime
  acceptRcvFT_ db user fileId filePath Nothing currentTs
  DB.execute
    db
    "INSERT INTO connections (agent_conn_id, conn_status, conn_type, rcv_file_id, user_id, created_at, updated_at, to_subscribe) VALUES (?,?,?,?,?,?,?,?)"
    (acId, connStatus, ConnRcvFile, fileId, userId, currentTs, currentTs, subMode == SMOnlyCreate)
  connId <- insertedRowId db
  setCommandConnId db user cmdId connId
  runExceptT $ getChatItemByFileId db vr user fileId

getContactByFileId :: DB.Connection -> User -> FileTransferId -> ExceptT StoreError IO Contact
getContactByFileId db user@User {userId} fileId = do
  cId <- getContactIdByFileId
  getContact db user cId
  where
    getContactIdByFileId =
      ExceptT . firstRow fromOnly (SEContactNotFoundByFileId fileId) $
        DB.query db "SELECT contact_id FROM files WHERE user_id = ? AND file_id = ?" (userId, fileId)

acceptRcvInlineFT :: DB.Connection -> VersionRange -> User -> FileTransferId -> FilePath -> ExceptT StoreError IO AChatItem
acceptRcvInlineFT db vr user fileId filePath = do
  liftIO $ acceptRcvFT_ db user fileId filePath (Just IFMOffer) =<< getCurrentTime
  getChatItemByFileId db vr user fileId

startRcvInlineFT :: DB.Connection -> User -> RcvFileTransfer -> FilePath -> Maybe InlineFileMode -> IO ()
startRcvInlineFT db user RcvFileTransfer {fileId} filePath rcvFileInline =
  acceptRcvFT_ db user fileId filePath rcvFileInline =<< getCurrentTime

xftpAcceptRcvFT :: DB.Connection -> VersionRange -> User -> FileTransferId -> FilePath -> ExceptT StoreError IO AChatItem
xftpAcceptRcvFT db vr user fileId filePath = do
  liftIO $ acceptRcvFT_ db user fileId filePath Nothing =<< getCurrentTime
  getChatItemByFileId db vr user fileId

acceptRcvFT_ :: DB.Connection -> User -> FileTransferId -> FilePath -> Maybe InlineFileMode -> UTCTime -> IO ()
acceptRcvFT_ db User {userId} fileId filePath rcvFileInline currentTs = do
  DB.execute
    db
    "UPDATE files SET file_path = ?, ci_file_status = ?, updated_at = ? WHERE user_id = ? AND file_id = ?"
    (filePath, CIFSRcvAccepted, currentTs, userId, fileId)
  DB.execute
    db
    "UPDATE rcv_files SET rcv_file_inline = ?, file_status = ?, updated_at = ? WHERE file_id = ?"
    (rcvFileInline, FSAccepted, currentTs, fileId)

setRcvFileToReceive :: DB.Connection -> FileTransferId -> Maybe CryptoFileArgs -> IO ()
setRcvFileToReceive db fileId cfArgs_ = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE rcv_files SET to_receive = 1, updated_at = ? WHERE file_id = ?" (currentTs, fileId)
  forM_ cfArgs_ $ \cfArgs -> setFileCryptoArgs_ db fileId cfArgs currentTs

setFileCryptoArgs :: DB.Connection -> FileTransferId -> CryptoFileArgs -> IO ()
setFileCryptoArgs db fileId cfArgs = setFileCryptoArgs_ db fileId cfArgs =<< getCurrentTime

setFileCryptoArgs_ :: DB.Connection -> FileTransferId -> CryptoFileArgs -> UTCTime -> IO ()
setFileCryptoArgs_ db fileId (CFArgs key nonce) currentTs =
  DB.execute
    db
    "UPDATE files SET file_crypto_key = ?, file_crypto_nonce = ?, updated_at = ? WHERE file_id = ?"
    (key, nonce, currentTs, fileId)

removeFileCryptoArgs :: DB.Connection -> FileTransferId -> IO ()
removeFileCryptoArgs db fileId = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE files SET file_crypto_key = NULL, file_crypto_nonce = NULL, updated_at = ? WHERE file_id = ?" (currentTs, fileId)

getRcvFilesToReceive :: DB.Connection -> User -> IO [RcvFileTransfer]
getRcvFilesToReceive db user@User {userId} = do
  cutoffTs <- addUTCTime (-(2 * nominalDay)) <$> getCurrentTime
  fileIds :: [Int64] <-
    map fromOnly
      <$> DB.query
        db
        [sql|
          SELECT r.file_id
          FROM rcv_files r
          JOIN files f ON f.file_id = r.file_id
          WHERE f.user_id = ? AND r.file_status = ?
            AND r.to_receive = 1 AND r.created_at > ?
        |]
        (userId, FSNew, cutoffTs)
  rights <$> mapM (runExceptT . getRcvFileTransfer db user) fileIds

setRcvFTAgentDeleted :: DB.Connection -> FileTransferId -> IO ()
setRcvFTAgentDeleted db fileId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE rcv_files SET agent_rcv_file_deleted = 1, updated_at = ? WHERE file_id = ?"
    (currentTs, fileId)

updateRcvFileStatus :: DB.Connection -> FileTransferId -> FileStatus -> IO ()
updateRcvFileStatus db fileId status = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE rcv_files SET file_status = ?, updated_at = ? WHERE file_id = ?" (status, currentTs, fileId)

createRcvFileChunk :: DB.Connection -> RcvFileTransfer -> Integer -> AgentMsgId -> IO RcvChunkStatus
createRcvFileChunk db RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileSize}, chunkSize} chunkNo msgId = do
  status <- getLastChunkNo
  unless (status == RcvChunkError) $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      "INSERT OR REPLACE INTO rcv_file_chunks (file_id, chunk_number, chunk_agent_msg_id, created_at, updated_at) VALUES (?,?,?,?,?)"
      (fileId, chunkNo, msgId, currentTs, currentTs)
  pure status
  where
    getLastChunkNo = do
      ns <- DB.query db "SELECT chunk_number FROM rcv_file_chunks WHERE file_id = ? ORDER BY chunk_number DESC LIMIT 1" (Only fileId)
      pure $ case map fromOnly ns of
        []
          | chunkNo == 1 ->
              if chunkSize >= fileSize
                then RcvChunkFinal
                else RcvChunkOk
          | otherwise -> RcvChunkError
        n : _
          | chunkNo == n -> RcvChunkDuplicate
          | chunkNo == n + 1 ->
              let prevSize = n * chunkSize
               in if prevSize >= fileSize
                    then RcvChunkError
                    else
                      if prevSize + chunkSize >= fileSize
                        then RcvChunkFinal
                        else RcvChunkOk
          | otherwise -> RcvChunkError

updatedRcvFileChunkStored :: DB.Connection -> RcvFileTransfer -> Integer -> IO ()
updatedRcvFileChunkStored db RcvFileTransfer {fileId} chunkNo = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE rcv_file_chunks
      SET chunk_stored = 1, updated_at = ?
      WHERE file_id = ? AND chunk_number = ?
    |]
    (currentTs, fileId, chunkNo)

deleteRcvFileChunks :: DB.Connection -> RcvFileTransfer -> IO ()
deleteRcvFileChunks db RcvFileTransfer {fileId} =
  DB.execute db "DELETE FROM rcv_file_chunks WHERE file_id = ?" (Only fileId)

updateFileTransferChatItemId :: DB.Connection -> FileTransferId -> ChatItemId -> UTCTime -> IO ()
updateFileTransferChatItemId db fileId ciId currentTs =
  DB.execute db "UPDATE files SET chat_item_id = ?, updated_at = ? WHERE file_id = ?" (ciId, currentTs, fileId)

getFileTransferProgress :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO (FileTransfer, [Integer])
getFileTransferProgress db user fileId = do
  ft <- getFileTransfer db user fileId
  liftIO $
    (ft,) . map fromOnly <$> case ft of
      FTSnd _ [] -> pure [Only 0]
      FTSnd _ _ -> DB.query db "SELECT COUNT(*) FROM snd_file_chunks WHERE file_id = ? and chunk_sent = 1 GROUP BY connection_id" (Only fileId)
      FTRcv _ -> DB.query db "SELECT COUNT(*) FROM rcv_file_chunks WHERE file_id = ? AND chunk_stored = 1" (Only fileId)

getFileTransfer :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO FileTransfer
getFileTransfer db user@User {userId} fileId =
  fileTransfer =<< liftIO (getFileTransferRow_ db userId fileId)
  where
    fileTransfer :: [(Maybe Int64, Maybe Int64)] -> ExceptT StoreError IO FileTransfer
    fileTransfer [(Nothing, Just _)] = FTRcv <$> getRcvFileTransfer db user fileId
    fileTransfer _ = do
      (ftm, fts) <- getSndFileTransfer db user fileId
      pure $ FTSnd {fileTransferMeta = ftm, sndFileTransfers = fts}

getFileTransferRow_ :: DB.Connection -> UserId -> Int64 -> IO [(Maybe Int64, Maybe Int64)]
getFileTransferRow_ db userId fileId =
  DB.query
    db
    [sql|
      SELECT s.file_id, r.file_id
      FROM files f
      LEFT JOIN snd_files s ON s.file_id = f.file_id
      LEFT JOIN rcv_files r ON r.file_id = f.file_id
      WHERE user_id = ? AND f.file_id = ?
    |]
    (userId, fileId)

getSndFileTransfer :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO (FileTransferMeta, [SndFileTransfer])
getSndFileTransfer db user fileId = do
  fileTransferMeta <- getFileTransferMeta db user fileId
  sndFileTransfers <- getSndFileTransfers db user fileId
  pure (fileTransferMeta, sndFileTransfers)

getSndFileTransfers :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO [SndFileTransfer]
getSndFileTransfers db User {userId} fileId = ExceptT $ getSndFileTransfers_ db userId fileId

getSndFileTransfers_ :: DB.Connection -> UserId -> Int64 -> IO (Either StoreError [SndFileTransfer])
getSndFileTransfers_ db userId fileId =
  mapM sndFileTransfer
    <$> DB.query
      db
      [sql|
        SELECT s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, s.file_descr_id, s.file_inline, s.connection_id, c.agent_conn_id, s.group_member_id,
          cs.local_display_name, m.local_display_name
        FROM snd_files s
        JOIN files f USING (file_id)
        JOIN connections c USING (connection_id)
        LEFT JOIN contacts cs USING (contact_id)
        LEFT JOIN group_members m USING (group_member_id)
        WHERE f.user_id = ? AND f.file_id = ?
      |]
      (userId, fileId)
  where
    sndFileTransfer :: (FileStatus, String, Integer, Integer, FilePath) :. (Maybe Int64, Maybe InlineFileMode, Int64, AgentConnId, Maybe Int64, Maybe ContactName, Maybe ContactName) -> Either StoreError SndFileTransfer
    sndFileTransfer ((fileStatus, fileName, fileSize, chunkSize, filePath) :. (fileDescrId, fileInline, connId, agentConnId, groupMemberId, contactName_, memberName_)) =
      case contactName_ <|> memberName_ of
        Just recipientDisplayName -> Right SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileDescrId, fileInline, recipientDisplayName, connId, agentConnId, groupMemberId}
        Nothing -> Left $ SESndFileInvalid fileId

getFileTransferMeta :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO FileTransferMeta
getFileTransferMeta db User {userId} = getFileTransferMeta_ db userId

getFileTransferMeta_ :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO FileTransferMeta
getFileTransferMeta_ db userId fileId =
  ExceptT . firstRow fileTransferMeta (SEFileNotFound fileId) $
    DB.query
      db
      [sql|
        SELECT file_name, file_size, chunk_size, file_path, file_crypto_key, file_crypto_nonce, file_inline, agent_snd_file_id, agent_snd_file_deleted, private_snd_file_descr, cancelled
        FROM files
        WHERE user_id = ? AND file_id = ?
      |]
      (userId, fileId)
  where
    fileTransferMeta :: (String, Integer, Integer, FilePath, Maybe C.SbKey, Maybe C.CbNonce, Maybe InlineFileMode, Maybe AgentSndFileId, Bool, Maybe Text, Maybe Bool) -> FileTransferMeta
    fileTransferMeta (fileName, fileSize, chunkSize, filePath, fileKey, fileNonce, fileInline, aSndFileId_, agentSndFileDeleted, privateSndFileDescr, cancelled_) =
      let cryptoArgs = CFArgs <$> fileKey <*> fileNonce
          xftpSndFile = (\fId -> XFTPSndFile {agentSndFileId = fId, privateSndFileDescr, agentSndFileDeleted, cryptoArgs}) <$> aSndFileId_
       in FileTransferMeta {fileId, xftpSndFile, fileName, fileSize, chunkSize, filePath, fileInline, cancelled = fromMaybe False cancelled_}

getContactFileInfo :: DB.Connection -> User -> Contact -> IO [CIFileInfo]
getContactFileInfo db User {userId} Contact {contactId} =
  map toFileInfo
    <$> DB.query db (fileInfoQuery <> " WHERE i.user_id = ? AND i.contact_id = ?") (userId, contactId)

getLocalCryptoFile :: DB.Connection -> UserId -> Int64 -> Bool -> ExceptT StoreError IO CryptoFile
getLocalCryptoFile db userId fileId sent =
  liftIO (getFileTransferRow_ db userId fileId) >>= \case
    [(Nothing, Just _)] -> do
      when sent $ throwError $ SEFileNotFound fileId
      RcvFileTransfer {fileStatus, cryptoArgs} <- getRcvFileTransfer_ db userId fileId
      case fileStatus of
        RFSComplete RcvFileInfo {filePath} -> pure $ CryptoFile filePath cryptoArgs
        _ -> throwError $ SEFileNotFound fileId
    _ -> do
      unless sent $ throwError $ SEFileNotFound fileId
      FileTransferMeta {filePath, xftpSndFile} <- getFileTransferMeta_ db userId fileId
      pure $ CryptoFile filePath $ xftpSndFile >>= \f -> f.cryptoArgs

updateDirectCIFileStatus :: forall d. MsgDirectionI d => DB.Connection -> VersionRange -> User -> Int64 -> CIFileStatus d -> ExceptT StoreError IO AChatItem
updateDirectCIFileStatus db vr user fileId fileStatus = do
  aci@(AChatItem cType d cInfo ci) <- getChatItemByFileId db vr user fileId
  case (cType, testEquality d $ msgDirection @d) of
    (SCTDirect, Just Refl) -> do
      liftIO $ updateCIFileStatus db user fileId fileStatus
      pure $ AChatItem SCTDirect d cInfo $ updateFileStatus ci fileStatus
    _ -> pure aci
