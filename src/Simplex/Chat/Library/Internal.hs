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

module Simplex.Chat.Library.Internal where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (retry)
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (ChaChaDRG)
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers, rights)
import Data.Fixed (div')
import Data.Foldable (foldr')
import Data.Functor (($>))
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (find, mapAccumL, partition)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Files
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.Batch (MsgBatch (..), batchMessages)
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.Operators
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
import Simplex.Chat.Util (encryptFile, shuffle)
import Simplex.FileTransfer.Description (FileDescriptionURI (..), ValidFileDescription)
import qualified Simplex.FileTransfer.Description as FD
import Simplex.FileTransfer.Protocol (FileParty (..), FilePartyI)
import Simplex.FileTransfer.Types (RcvFileId, SndFileId)
import Simplex.Messaging.Agent as Agent
import Simplex.Messaging.Agent.Client (getFastNetworkConfig, ipAddressProtected, withLockMap)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..), ServerCfg (..))
import Simplex.Messaging.Agent.Lock (withLock)
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Agent.Protocol as AP (AgentErrorType (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Client (NetworkConfig (..))
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..), pattern IKPQOff, pattern PQEncOff, pattern PQEncOn, pattern PQSupportOff, pattern PQSupportOn)
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Protocol (MsgBody, MsgFlags (..), ProtoServerWithAuth (..), ProtocolServer, ProtocolTypeI (..), SProtocolType (..), SubscriptionMode (..), UserProtocol, XFTPServer)
import qualified Simplex.Messaging.Protocol as SMP
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util
import Simplex.Messaging.Version
import System.FilePath (takeFileName, (</>))
import System.IO (Handle, IOMode (..), SeekMode (..), hFlush)
import UnliftIO.Concurrent (forkFinally, mkWeakThreadId)
import UnliftIO.Directory
import UnliftIO.IO (hClose, hSeek, hTell, openFile)
import UnliftIO.STM

maxMsgReactions :: Int
maxMsgReactions = 3

maxRcvMentions :: Int
maxRcvMentions = 5

maxSndMentions :: Int
maxSndMentions = 3

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

useServerCfgs :: forall p. UserProtocol p => SProtocolType p -> RandomAgentServers -> [(Text, ServerOperator)] -> [UserServer p] -> NonEmpty (ServerCfg p)
useServerCfgs p RandomAgentServers {smpServers, xftpServers} opDomains =
  fromMaybe (rndAgentServers p) . L.nonEmpty . agentServerCfgs p opDomains
  where
    rndAgentServers :: SProtocolType p -> NonEmpty (ServerCfg p)
    rndAgentServers = \case
      SPSMP -> smpServers
      SPXFTP -> xftpServers

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

toggleNtf :: GroupMember -> Bool -> CM ()
toggleNtf m ntfOn =
  when (memberActive m) $
    forM_ (memberConnId m) $ \connId ->
      withAgent (\a -> toggleConnectionNtfs a connId ntfOn) `catchChatError` eToView

prepareGroupMsg :: DB.Connection -> User -> GroupInfo -> MsgContent -> Map MemberName MsgMention -> Maybe ChatItemId -> Maybe CIForwardedFrom -> Maybe FileInvitation -> Maybe CITimed -> Bool -> ExceptT StoreError IO (ChatMsgEvent 'Json, Maybe (CIQuote 'CTGroup))
prepareGroupMsg db user g@GroupInfo {membership} mc mentions quotedItemId_ itemForwarded fInv_ timed_ live = case (quotedItemId_, itemForwarded) of
  (Nothing, Nothing) ->
    let mc' = MCSimple $ ExtMsgContent mc mentions fInv_ (ttl' <$> timed_) (justTrue live)
     in pure (XMsgNew mc', Nothing)
  (Nothing, Just _) ->
    let mc' = MCForward $ ExtMsgContent mc mentions fInv_ (ttl' <$> timed_) (justTrue live)
     in pure (XMsgNew mc', Nothing)
  (Just quotedItemId, Nothing) -> do
    CChatItem _ qci@ChatItem {meta = CIMeta {itemTs, itemSharedMsgId}, formattedText, mentions = quoteMentions, file} <-
      getGroupCIWithReactions db user g quotedItemId
    (origQmc, qd, sent, GroupMember {memberId}) <- quoteData qci membership
    let msgRef = MsgRef {msgId = itemSharedMsgId, sentAt = itemTs, sent, memberId = Just memberId}
        qmc = quoteContent mc origQmc file
        (qmc', ft', _) = updatedMentionNames qmc formattedText quoteMentions
        quotedItem = CIQuote {chatDir = qd, itemId = Just quotedItemId, sharedMsgId = itemSharedMsgId, sentAt = itemTs, content = qmc', formattedText = ft'}
        mc' = MCQuote QuotedMsg {msgRef, content = qmc'} (ExtMsgContent mc mentions fInv_ (ttl' <$> timed_) (justTrue live))
    pure (XMsgNew mc', Just quotedItem)
  (Just _, Just _) -> throwError SEInvalidQuote
  where
    quoteData :: ChatItem c d -> GroupMember -> ExceptT StoreError IO (MsgContent, CIQDirection 'CTGroup, Bool, GroupMember)
    quoteData ChatItem {meta = CIMeta {itemDeleted = Just _}} _ = throwError SEInvalidQuote
    quoteData ChatItem {chatDir = CIGroupSnd, content = CISndMsgContent qmc} membership' = pure (qmc, CIQGroupSnd, True, membership')
    quoteData ChatItem {chatDir = CIGroupRcv m, content = CIRcvMsgContent qmc} _ = pure (qmc, CIQGroupRcv $ Just m, False, m)
    quoteData _ _ = throwError SEInvalidQuote

updatedMentionNames :: MsgContent -> Maybe MarkdownList -> Map MemberName CIMention -> (MsgContent, Maybe MarkdownList, Map MemberName CIMention)
updatedMentionNames mc ft_ mentions = case ft_ of
  Just ft
    | not (null ft) && not (null mentions) && not (all sameName $ M.assocs mentions) ->
        let (mentions', ft') = mapAccumL update M.empty ft
            text = T.concat $ map markdownText ft'
         in (mc {text} :: MsgContent, Just ft', mentions')
  _ -> (mc, ft_, mentions)
  where
    sameName (name, CIMention {memberRef}) = case memberRef of
      Just CIMentionMember {displayName} -> case T.stripPrefix displayName name of
        Just rest
          | T.null rest -> True
          | otherwise -> case T.uncons rest of
              Just ('_', suffix) -> T.all isDigit suffix
              _ -> False
        Nothing -> False
      Nothing -> True
    update mentions' ft@(FormattedText f _) = case f of
      Just (Mention name) -> case M.lookup name mentions of
        Just mm@CIMention {memberRef} ->
          let name' = uniqueMentionName 0 $ case memberRef of
                Just CIMentionMember {displayName} -> displayName
                Nothing -> name
           in (M.insert name' mm mentions', FormattedText (Just $ Mention name') ('@' `T.cons` viewName name'))
        Nothing -> (mentions', ft)
      _ -> (mentions', ft)
      where
        uniqueMentionName :: Int -> Text -> Text
        uniqueMentionName pfx name =
          let prefixed = if pfx == 0 then name else (name `T.snoc` '_') <> tshow pfx
           in if prefixed `M.member` mentions' then uniqueMentionName (pfx + 1) name else prefixed

getCIMentions :: DB.Connection -> User -> GroupInfo -> Maybe MarkdownList -> Map MemberName GroupMemberId -> ExceptT StoreError IO (Map MemberName CIMention)
getCIMentions db user GroupInfo {groupId} ft_ mentions = case ft_ of
  Just ft | not (null ft) && not (null mentions) -> do
    let msgMentions = S.fromList $ mentionedNames ft
        n = M.size mentions
    -- prevent "invisible" and repeated-with-different-name mentions (when the same member is mentioned via another name)
    unless (n <= maxSndMentions && all (`S.member` msgMentions) (M.keys mentions) && S.size (S.fromList $ M.elems mentions) == n) $
      throwError SEInvalidMention
    mapM (getMentionedGroupMember db user groupId) mentions
  _ -> pure M.empty

getRcvCIMentions :: DB.Connection -> User -> GroupInfo -> Maybe MarkdownList -> Map MemberName MsgMention -> IO (Map MemberName CIMention)
getRcvCIMentions db user GroupInfo {groupId} ft_ mentions = case ft_ of
  Just ft
    | not (null ft) && not (null mentions) ->
        let mentions' = uniqueMsgMentions maxRcvMentions mentions $ mentionedNames ft
         in mapM (getMentionedMemberByMemberId db user groupId) mentions'
  _ -> pure M.empty

-- prevent "invisible" and repeated-with-different-name mentions
uniqueMsgMentions :: Int -> Map MemberName MsgMention -> [ContactName] -> Map MemberName MsgMention
uniqueMsgMentions maxMentions mentions = go M.empty S.empty 0
  where
    go acc _ _ [] = acc
    go acc seen n (name : rest)
      | n >= maxMentions = acc
      | otherwise = case M.lookup name mentions of
          Just mm@MsgMention {memberId}
            | S.notMember memberId seen ->
                go (M.insert name mm acc) (S.insert memberId seen) (n + 1) rest
          _ -> go acc seen n rest

getMessageMentions :: DB.Connection -> User -> GroupId -> Text -> IO (Map MemberName GroupMemberId)
getMessageMentions db user gId msg = case parseMaybeMarkdownList msg of
  Just ft | not (null ft) -> M.fromList . catMaybes <$> mapM get (nubOrd $ mentionedNames ft)
  _ -> pure M.empty
  where
    get name =
      fmap (name,) . eitherToMaybe
        <$> runExceptT (getGroupMemberIdByName db user gId name)

msgContentTexts :: MsgContent -> (Text, Maybe MarkdownList)
msgContentTexts mc = let t = msgContentText mc in (t, parseMaybeMarkdownList t)

ciContentTexts :: CIContent d -> (Text, Maybe MarkdownList)
ciContentTexts content = let t = ciContentToText content in (t, parseMaybeMarkdownList t)

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

prohibitedGroupContent :: GroupInfo -> GroupMember -> MsgContent -> Maybe MarkdownList -> Maybe f -> Bool -> Maybe GroupFeature
prohibitedGroupContent gInfo@GroupInfo {membership = GroupMember {memberRole = userRole}} m mc ft file_ sent
  | isVoice mc && not (groupFeatureMemberAllowed SGFVoice m gInfo) = Just GFVoice
  | not (isVoice mc) && isJust file_ && not (groupFeatureMemberAllowed SGFFiles m gInfo) = Just GFFiles
  | isReport mc && (badReportUser || not (groupFeatureAllowed SGFReports gInfo)) = Just GFReports
  | prohibitedSimplexLinks gInfo m ft = Just GFSimplexLinks
  | otherwise = Nothing
  where
    -- admins cannot send reports, non-admins cannot receive reports
    badReportUser
      | sent = userRole >= GRModerator
      | otherwise = userRole < GRModerator

prohibitedSimplexLinks :: GroupInfo -> GroupMember -> Maybe MarkdownList -> Bool
prohibitedSimplexLinks gInfo m ft =
  not (groupFeatureMemberAllowed SGFSimplexLinks m gInfo)
    && maybe False (any ftIsSimplexLink) ft
  where
    ftIsSimplexLink :: FormattedText -> Bool
    ftIsSimplexLink FormattedText {format} = maybe False isSimplexLink format

roundedFDCount :: Int -> Int
roundedFDCount n
  | n <= 0 = 4
  | otherwise = max 4 $ fromIntegral $ (2 :: Integer) ^ (ceiling (logBase 2 (fromIntegral n) :: Double) :: Integer)

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
  deleteAgentConnectionsAsync smpSFConnIds
  deleteAgentConnectionsAsync smpRFConnIds
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

deleteDirectCIs :: User -> Contact -> [CChatItem 'CTDirect] -> CM [ChatItemDeletion]
deleteDirectCIs user ct items = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  deleteCIFiles user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (deleteItem db) items)
  unless (null errs) $ toView $ CEvtChatErrors errs
  pure deletions
  where
    deleteItem db (CChatItem md ci) = do
      deleteDirectChatItem db user ct ci
      pure $ contactDeletion md ct ci Nothing

deleteGroupCIs :: User -> GroupInfo -> [CChatItem 'CTGroup] -> Maybe GroupMember -> UTCTime -> CM [ChatItemDeletion]
deleteGroupCIs user gInfo items byGroupMember_ deletedTs = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  deleteCIFiles user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (deleteItem db) items)
  unless (null errs) $ toView $ CEvtChatErrors errs
  pure deletions
  where
    deleteItem :: DB.Connection -> CChatItem 'CTGroup -> IO ChatItemDeletion
    deleteItem db (CChatItem md ci) = do
      ci' <- case byGroupMember_ of
        Just m -> Just <$> updateGroupChatItemModerated db user gInfo ci m deletedTs
        Nothing -> Nothing <$ deleteGroupChatItem db user gInfo ci
      pure $ groupDeletion md gInfo ci ci'

deleteGroupMemberCIs :: MsgDirectionI d => User -> GroupInfo -> GroupMember -> GroupMember -> SMsgDirection d -> CM ()
deleteGroupMemberCIs user gInfo member byGroupMember msgDir = do
  deletedTs <- liftIO getCurrentTime
  filesInfo <- withStore' $ \db -> deleteGroupMemberCIs_ db user gInfo member byGroupMember msgDir deletedTs
  deleteCIFiles user filesInfo

deleteGroupMembersCIs :: User -> GroupInfo -> [GroupMember] -> GroupMember -> CM ()
deleteGroupMembersCIs user gInfo members byGroupMember = do
  deletedTs <- liftIO getCurrentTime
  filesInfo <- withStore' $ \db -> fmap concat $ forM members $ \m -> deleteGroupMemberCIs_ db user gInfo m byGroupMember SMDRcv deletedTs
  deleteCIFiles user filesInfo

deleteGroupMemberCIs_ :: MsgDirectionI d => DB.Connection -> User -> GroupInfo -> GroupMember -> GroupMember -> SMsgDirection d -> UTCTime -> IO [CIFileInfo]
deleteGroupMemberCIs_ db user gInfo member byGroupMember msgDir deletedTs = do
  fs <- getGroupMemberFileInfo db user gInfo member
  updateMemberCIsModerated db user gInfo member byGroupMember msgDir deletedTs
  pure fs

deleteLocalCIs :: User -> NoteFolder -> [CChatItem 'CTLocal] -> Bool -> Bool -> CM ChatResponse
deleteLocalCIs user nf items byUser timed = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  deleteFilesLocally ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (deleteItem db) items)
  unless (null errs) $ toView $ CEvtChatErrors errs
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

markDirectCIsDeleted :: User -> Contact -> [CChatItem 'CTDirect] -> UTCTime -> CM [ChatItemDeletion]
markDirectCIsDeleted user ct items deletedTs = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  cancelFilesInProgress user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (markDeleted db) items)
  unless (null errs) $ toView $ CEvtChatErrors errs
  pure deletions
  where
    markDeleted db (CChatItem md ci) = do
      ci' <- markDirectChatItemDeleted db user ct ci deletedTs
      pure $ contactDeletion md ct ci (Just ci')

markGroupCIsDeleted :: User -> GroupInfo -> [CChatItem 'CTGroup] -> Maybe GroupMember -> UTCTime -> CM [ChatItemDeletion]
markGroupCIsDeleted user gInfo items byGroupMember_ deletedTs = do
  let ciFilesInfo = mapMaybe (\(CChatItem _ ChatItem {file}) -> mkCIFileInfo <$> file) items
  cancelFilesInProgress user ciFilesInfo
  (errs, deletions) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (markDeleted db) items)
  unless (null errs) $ toView $ CEvtChatErrors errs
  pure deletions
  -- pure $ CRChatItemsDeleted user deletions byUser False
  where
    markDeleted db (CChatItem md ci) = do
      ci' <- markGroupChatItemDeleted db user gInfo ci byGroupMember_ deletedTs
      pure $ groupDeletion md gInfo ci (Just ci')

markGroupMemberCIsDeleted :: User -> GroupInfo -> GroupMember -> GroupMember -> CM ()
markGroupMemberCIsDeleted user gInfo member byGroupMember = do
  deletedTs <- liftIO getCurrentTime
  filesInfo <- withStore' $ \db -> markGroupMemberCIsDeleted_ db user gInfo member byGroupMember deletedTs
  cancelFilesInProgress user filesInfo

markGroupMembersCIsDeleted :: User -> GroupInfo -> [GroupMember] -> GroupMember -> CM ()
markGroupMembersCIsDeleted user gInfo members byGroupMember = do
  deletedTs <- liftIO getCurrentTime
  filesInfo <- withStore' $ \db -> fmap concat $ forM members $ \m -> markGroupMemberCIsDeleted_ db user gInfo m byGroupMember deletedTs
  cancelFilesInProgress user filesInfo

markGroupMemberCIsDeleted_ :: DB.Connection -> User -> GroupInfo -> GroupMember -> GroupMember -> UTCTime -> IO [CIFileInfo]
markGroupMemberCIsDeleted_ db user gInfo member byGroupMember deletedTs = do
  fs <- getGroupMemberFileInfo db user gInfo member
  markMemberCIsDeleted db user gInfo member byGroupMember deletedTs
  pure fs

groupDeletion :: MsgDirectionI d => SMsgDirection d -> GroupInfo -> ChatItem 'CTGroup d -> Maybe (ChatItem 'CTGroup d) -> ChatItemDeletion
groupDeletion md g ci ci' = ChatItemDeletion (gItem ci) (gItem <$> ci')
  where
    gItem = AChatItem SCTGroup md (GroupChat g)

contactDeletion :: MsgDirectionI d => SMsgDirection d -> Contact -> ChatItem 'CTDirect d -> Maybe (ChatItem 'CTDirect d) -> ChatItemDeletion
contactDeletion md ct ci ci' = ChatItemDeletion (ctItem ci) (ctItem <$> ci')
  where
    ctItem = AChatItem SCTDirect md (DirectChat ct)

updateCallItemStatus :: User -> Contact -> Call -> WebRTCCallStatus -> Maybe MessageId -> CM ()
updateCallItemStatus user ct@Contact {contactId} Call {chatItemId} receivedStatus msgId_ = do
  aciContent_ <- callStatusItemContent user ct chatItemId receivedStatus
  forM_ aciContent_ $ \aciContent -> do
    timed_ <- callTimed ct aciContent
    updateDirectChatItemView user ct chatItemId aciContent False False timed_ msgId_
    forM_ (timed_ >>= timedDeleteAt') $
      startProximateTimedItemThread user (ChatRef CTDirect contactId, chatItemId)

updateDirectChatItemView :: User -> Contact -> ChatItemId -> ACIContent -> Bool -> Bool -> Maybe CITimed -> Maybe MessageId -> CM ()
updateDirectChatItemView user ct chatItemId (ACIContent msgDir ciContent) edited live timed_ msgId_ = do
  ci' <- withStore $ \db -> updateDirectChatItem db user ct chatItemId ciContent edited live timed_ msgId_
  toView $ CEvtChatItemUpdated user (AChatItem SCTDirect msgDir (DirectChat ct) ci')

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
    -- TODO AChatItem in Cancelled events
    processError e
      | rctFileCancelled e = pure $ CRRcvFileAcceptedSndCancelled user ft
      | otherwise = throwError e

receiveFileEvt' :: User -> RcvFileTransfer -> Bool -> Maybe Bool -> Maybe FilePath -> CM ChatEvent
receiveFileEvt' user ft userApprovedRelays rcvInline_ filePath_ = do
  (CEvtRcvFileAccepted user <$> acceptFileReceive user ft userApprovedRelays rcvInline_ filePath_) `catchChatError` processError
  where
    -- TODO AChatItem in Cancelled events
    processError e
      | rctFileCancelled e = pure $ CEvtRcvFileAcceptedSndCancelled user ft
      | otherwise = throwError e

rctFileCancelled :: ChatError -> Bool
rctFileCancelled = \case
  ChatErrorAgent (SMP _ SMP.AUTH) _ -> True
  ChatErrorAgent (CONN DUPLICATE) _ -> True
  _ -> False

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
        toView $ CEvtChatItemUpdated user aci
      throwChatError $ CEFileNotApproved fileId unknownSrvs

cleanupACIFile :: AChatItem -> CM ()
cleanupACIFile (AChatItem _ _ _ ChatItem {file = Just CIFile {fileSource = Just CryptoFile {filePath}}}) = do
  fsFilePath <- lift $ toFSFilePath filePath
  removeFile fsFilePath `catchChatError` \_ -> pure ()
cleanupACIFile _ = pure ()

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
  toView $ CEvtRcvFileStart user ci

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

acceptContactRequestAsync :: User -> UserContactRequest -> Maybe IncognitoProfile -> PQSupport -> CM Contact
acceptContactRequestAsync user cReq@UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange, localDisplayName = cName, profileId, profile = p, userContactLinkId, xContactId} incognitoProfile pqSup = do
  subMode <- chatReadVar subscriptionMode
  let profileToSend = profileToSendOnAccept user incognitoProfile False
  vr <- chatVersionRange
  let chatV = vr `peerConnChatVersion` cReqChatVRange
  (cmdId, acId) <- agentAcceptContactAsync user True invId (XInfo profileToSend) subMode pqSup chatV
  withStore' $ \db -> do
    (ct, Connection {connId}) <- createAcceptedContact db user acId chatV cReqChatVRange cName profileId p userContactLinkId xContactId incognitoProfile subMode pqSup True
    deleteContactRequestRec db user cReq
    setCommandConnId db user cmdId connId
    pure ct

acceptGroupJoinRequestAsync :: User -> GroupInfo -> UserContactRequest -> GroupAcceptance -> GroupMemberRole -> Maybe IncognitoProfile -> CM GroupMember
acceptGroupJoinRequestAsync
  user
  gInfo@GroupInfo {groupProfile, membership, businessChat}
  ucr@UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange}
  gAccepted
  gLinkMemRole
  incognitoProfile = do
    gVar <- asks random
    let initialStatus = acceptanceToStatus gAccepted
    (groupMemberId, memberId) <- withStore $ \db -> do
      liftIO $ deleteContactRequestRec db user ucr
      createJoiningMember db gVar user gInfo ucr gLinkMemRole initialStatus
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
                accepted = Just gAccepted,
                business = businessChat,
                groupSize = Just currentMemCount
              }
    subMode <- chatReadVar subscriptionMode
    vr <- chatVersionRange
    let chatV = vr `peerConnChatVersion` cReqChatVRange
    connIds <- agentAcceptContactAsync user True invId msg subMode PQSupportOff chatV
    withStore $ \db -> do
      liftIO $ createJoiningMemberConnection db user connIds chatV ucr groupMemberId subMode
      getGroupMemberById db vr user groupMemberId

acceptGroupJoinSendRejectAsync :: User -> GroupInfo -> UserContactRequest -> GroupRejectionReason -> CM GroupMember
acceptGroupJoinSendRejectAsync
  user
  gInfo@GroupInfo {groupProfile, membership}
  ucr@UserContactRequest {agentInvitationId = AgentInvId invId, cReqChatVRange}
  rejectionReason = do
    gVar <- asks random
    (groupMemberId, memberId) <- withStore $ \db -> do
      liftIO $ deleteContactRequestRec db user ucr
      createJoiningMember db gVar user gInfo ucr GRObserver GSMemRejected
    let GroupMember {memberRole = userRole, memberId = userMemberId} = membership
        msg =
          XGrpLinkReject $
            GroupLinkRejection
              { fromMember = MemberIdRole userMemberId userRole,
                invitedMember = MemberIdRole memberId GRObserver,
                groupProfile,
                rejectionReason
              }
    subMode <- chatReadVar subscriptionMode
    vr <- chatVersionRange
    let chatV = vr `peerConnChatVersion` cReqChatVRange
    connIds <- agentAcceptContactAsync user False invId msg subMode PQSupportOff chatV
    withStore $ \db -> do
      liftIO $ createJoiningMemberConnection db user connIds chatV ucr groupMemberId subMode
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
                accepted = Just GAAccepted,
                -- This refers to the "title member" that defines the group name and profile.
                -- This coincides with fromMember to be current user when accepting the connecting user,
                -- but it will be different when inviting somebody else.
                business = Just $ BusinessChatInfo {chatType = BCBusiness, businessId = userMemberId, customerId = memberId},
                groupSize = Just 1
              }
    subMode <- chatReadVar subscriptionMode
    let chatV = vr `peerConnChatVersion` cReqChatVRange
    connIds <- agentAcceptContactAsync user True invId msg subMode PQSupportOff chatV
    withStore' $ \db -> createJoiningMemberConnection db user connIds chatV ucr groupMemberId subMode
    let cd = CDGroupSnd gInfo
    createInternalChatItem user cd (CISndGroupE2EEInfo E2EInfo {pqEnabled = PQEncOff}) Nothing
    createGroupFeatureItems user cd CISndGroupFeature gInfo
    pure gInfo
    where
      businessGroupProfile :: Profile -> GroupPreferences -> GroupProfile
      businessGroupProfile Profile {displayName, fullName, image} groupPreferences =
        GroupProfile {displayName, fullName, description = Nothing, image, groupPreferences = Just groupPreferences, memberAdmission = Nothing}

profileToSendOnAccept :: User -> Maybe IncognitoProfile -> Bool -> Profile
profileToSendOnAccept user ip = userProfileToSend user (getIncognitoProfile <$> ip) Nothing
  where
    getIncognitoProfile = \case
      NewIncognito p -> p
      ExistingIncognito lp -> fromLocalProfile lp

introduceToGroup :: VersionRangeChat -> User -> GroupInfo -> GroupMember -> CM ()
introduceToGroup _ _ _ GroupMember {activeConn = Nothing} = throwChatError $ CEInternalError "member connection not active"
introduceToGroup vr user gInfo@GroupInfo {groupId, membership} m@GroupMember {activeConn = Just conn} = do
  members <- withStore' $ \db -> getGroupMembers db vr user gInfo
  void . sendGroupMessage user gInfo members . XGrpMemNew $ memberInfo m
  sendIntroductions members
  when (groupFeatureAllowed SGFHistory gInfo) sendHistory
  where
    sendIntroductions members = do
      intros <- withStore' $ \db -> createIntroductions db (maxVersion vr) members m
      shuffledIntros <- liftIO $ shuffleIntros intros
      if m `supportsVersion` batchSendVersion
        then do
          let events = map (memberIntro . reMember) shuffledIntros
          forM_ (L.nonEmpty events) $ \events' ->
            sendGroupMemberMessages user conn events' groupId
        else forM_ shuffledIntros $ \intro ->
          processIntro intro `catchChatError` eToView
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
        unless (null errors) $ toView $ CEvtChatErrors errors
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
        processContentItem sender ChatItem {formattedText, meta, quotedItem, mentions} mc fInvDescr_ =
          if isNothing fInvDescr_ && not (msgContentHasText mc)
            then pure []
            else do
              let CIMeta {itemTs, itemSharedMsgId, itemTimed} = meta
                  quotedItemId_ = quoteItemId =<< quotedItem
                  fInv_ = fst <$> fInvDescr_
                  (mc', _, mentions') = updatedMentionNames mc formattedText mentions
                  mentions'' = M.map (\CIMention {memberId} -> MsgMention {memberId}) mentions'
              (chatMsgEvent, _) <- withStore $ \db -> prepareGroupMsg db user gInfo mc' mentions'' quotedItemId_ Nothing fInv_ itemTimed False
              let senderVRange = memberChatVRange' sender
                  xMsgNewChatMsg = ChatMessage {chatVRange = senderVRange, msgId = itemSharedMsgId, chatMsgEvent}
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
  deleteAgentConnectionAsync $ aConnId conn
  withStore' $ \db -> deleteGroupLink db user gInfo

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
      deletions <- deleteDirectCIs user ct [ci]
      toView $ CEvtChatItemsDeleted user deletions True True
    CTGroup -> do
      (gInfo, ci) <- withStore $ \db -> (,) <$> getGroupInfo db vr user chatId <*> getGroupChatItem db user chatId itemId
      deletedTs <- liftIO getCurrentTime
      deletions <- deleteGroupCIs user gInfo [ci] Nothing deletedTs
      toView $ CEvtChatItemsDeleted user deletions True True
    _ -> eToView $ ChatError $ CEInternalError "bad deleteTimedItem cType"

startUpdatedTimedItemThread :: User -> ChatRef -> ChatItem c d -> ChatItem c d -> CM ()
startUpdatedTimedItemThread user chatRef ci ci' =
  case (chatItemTimed ci >>= timedDeleteAt', chatItemTimed ci' >>= timedDeleteAt') of
    (Nothing, Just deleteAt') ->
      startProximateTimedItemThread user (chatRef, chatItemId' ci') deleteAt'
    _ -> pure ()

metaBrokerTs :: MsgMeta -> UTCTime
metaBrokerTs MsgMeta {broker = (_, brokerTs)} = brokerTs

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
        toView $ CEvtContactPQEnabled user ct' pqSndEnabled'
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
        toView $ CEvtContactPQEnabled user ct' pqRcvEnabled'
      pure (ct', conn')

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
        toView $ CEvtSndFileComplete user ci ft
        lift $ closeFileHandle fileId sndFiles
        deleteAgentConnectionAsync acId

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
              eToView e
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
  cancel' `catchChatError` (\e -> eToView e $> fileConnId)
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
    `catchChatError` eToView
  case xftpSndFile of
    Nothing ->
      catMaybes <$> forM fts (\ft -> cancelSndFileTransfer user ft sendCancel)
    Just xsf -> do
      forM_ fts (\ft -> cancelSndFileTransfer user ft False)
      lift (agentXFTPDeleteSndFileRemote user xsf fileId) `catchChatError` eToView
      pure []

-- TODO v6.0 remove
cancelSndFileTransfer :: User -> SndFileTransfer -> Bool -> CM (Maybe ConnId)
cancelSndFileTransfer user@User {userId} ft@SndFileTransfer {fileId, connId, agentConnId = AgentConnId acId, fileStatus, fileInline} sendCancel =
  if fileStatus == FSCancelled || fileStatus == FSComplete
    then pure Nothing
    else cancel' `catchChatError` (\e -> eToView e $> fileConnId)
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
  let memberConns = mapMaybe (\GroupMember {activeConn} -> activeConn) members
  deleteAgentConnectionsAsync' (map aConnId memberConns) waitDelivery
  lift . void . withStoreBatch' $ \db -> map (\Connection {connId} -> deleteConnectionRecord db user connId) memberConns

deleteMemberConnection :: GroupMember -> CM ()
deleteMemberConnection mem = deleteMemberConnection' mem False

deleteMemberConnection' :: GroupMember -> Bool -> CM ()
deleteMemberConnection' GroupMember {activeConn} waitDelivery = do
  forM_ activeConn $ \conn -> do
    deleteAgentConnectionAsync' (aConnId conn) waitDelivery
    withStore' $ \db -> updateConnectionStatus db conn ConnDeleted

deleteOrUpdateMemberRecord :: User -> GroupMember -> CM ()
deleteOrUpdateMemberRecord user member =
  withStore' $ \db -> deleteOrUpdateMemberRecordIO db user member

deleteOrUpdateMemberRecordIO :: DB.Connection -> User -> GroupMember -> IO ()
deleteOrUpdateMemberRecordIO db user@User {userId} member =
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
  unless (null errs) $ toView $ CEvtChatErrors errs
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
      let msgReqs = L.map (fmap msgBatchReq_) batched'
      delivered <- deliverMessagesB msgReqs
      let msgs' = concat $ L.zipWith flattenMsgs batched' delivered
          pqEnc = findLastPQEnc delivered
      when (length msgs' /= length msgs_) $ logError "batchSendConnMessagesB: msgs_ and msgs' length mismatch"
      pure (msgs', pqEnc)
    Nothing -> pure ([], Nothing)
  where
    msgBatchReq_ :: MsgBatch -> ChatMsgReq
    msgBatchReq_ (MsgBatch batchBody sndMsgs) =
      (conn, msgFlags, (vrValue batchBody, map (\SndMessage {msgId} -> msgId) sndMsgs))
    flattenMsgs :: Either ChatError MsgBatch -> Either ChatError ([Int64], PQEncryption) -> [Either ChatError SndMessage]
    flattenMsgs (Right (MsgBatch _ sndMsgs)) (Right _) = map Right sndMsgs
    flattenMsgs (Right (MsgBatch _ sndMsgs)) (Left ce) = replicate (length sndMsgs) (Left ce)
    flattenMsgs (Left ce) _ = [Left ce] -- restore original ChatError
    findLastPQEnc :: NonEmpty (Either ChatError ([Int64], PQEncryption)) -> Maybe PQEncryption
    findLastPQEnc = foldr' (\x acc -> case x of Right (_, pqEnc) -> Just pqEnc; Left _ -> acc) Nothing

batchSndMessagesJSON :: NonEmpty (Either ChatError SndMessage) -> [Either ChatError MsgBatch]
batchSndMessagesJSON = batchMessages maxEncodedMsgLength . L.toList

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
  deliverMessages ((conn, msgFlags, (vrValue msgBody, [msgId])) :| []) >>= \case
    r :| [] -> case r of
      Right ([deliveryId], pqEnc) -> pure (deliveryId, pqEnc)
      Right (deliveryIds, _) -> throwChatError $ CEInternalError $ "deliverMessage: expected 1 delivery id, got " <> show (length deliveryIds)
      Left e -> throwError e
    rs -> throwChatError $ CEInternalError $ "deliverMessage: expected 1 result, got " <> show (length rs)

-- [MessageId] - SndMessage ids inside MsgBatch, or single message id
type ChatMsgReq = (Connection, MsgFlags, (ValueOrRef MsgBody, [MessageId]))

deliverMessages :: NonEmpty ChatMsgReq -> CM (NonEmpty (Either ChatError ([Int64], PQEncryption)))
deliverMessages msgs = deliverMessagesB $ L.map Right msgs

deliverMessagesB :: NonEmpty (Either ChatError ChatMsgReq) -> CM (NonEmpty (Either ChatError ([Int64], PQEncryption)))
deliverMessagesB msgReqs = do
  msgReqs' <- if any connSupportsPQ msgReqs then liftIO compressBodies else pure msgReqs
  sent <- L.zipWith prepareBatch msgReqs' <$> withAgent (`sendMessagesB` snd (mapAccumL toAgent Nothing msgReqs'))
  lift . void $ withStoreBatch' $ \db -> map (updatePQSndEnabled db) (rights . L.toList $ sent)
  lift . withStoreBatch $ \db -> L.map (bindRight $ createDelivery db) sent
  where
    connSupportsPQ = \case
      Right (Connection {pqSupport = PQSupportOn, connChatVersion = v}, _, _) -> v >= pqEncryptionCompressionVersion
      _ -> False
    compressBodies =
      forME msgReqs $ \(conn, msgFlags, (mbr, msgIds)) -> runExceptT $ do
        mbr' <- case mbr of
          VRValue i msgBody | B.length msgBody > maxCompressedMsgLength -> do
            let msgBody' = compressedBatchMsgBody_ msgBody
            when (B.length msgBody' > maxCompressedMsgLength) $ throwError $ ChatError $ CEException "large compressed message"
            pure $ VRValue i msgBody'
          v -> pure v
        pure (conn, msgFlags, (mbr', msgIds))
    toAgent prev = \case
      Right (conn@Connection {connId, pqEncryption}, msgFlags, (mbr, _msgIds)) ->
        let cId = case prev of
              Just prevId | prevId == connId -> ""
              _ -> aConnId conn
         in (Just connId, Right (cId, pqEncryption, msgFlags, mbr))
      Left _ce -> (prev, Left (AP.INTERNAL "ChatError, skip")) -- as long as it is Left, the agent batchers should just step over it
    prepareBatch (Right req) (Right ar) = Right (req, ar)
    prepareBatch (Left ce) _ = Left ce -- restore original ChatError
    prepareBatch _ (Left ae) = Left $ ChatErrorAgent ae Nothing
    createDelivery :: DB.Connection -> (ChatMsgReq, (AgentMsgId, PQEncryption)) -> IO (Either ChatError ([Int64], PQEncryption))
    createDelivery db ((Connection {connId}, _, (_, msgIds)), (agentMsgId, pqEnc')) = do
      Right . (,pqEnc') <$> mapM (createSndMsgDelivery db (SndMsgDelivery {connId, agentMsgId})) msgIds
    updatePQSndEnabled :: DB.Connection -> (ChatMsgReq, (AgentMsgId, PQEncryption)) -> IO ()
    updatePQSndEnabled db ((Connection {connId, pqSndEnabled}, _, _), (_, pqSndEnabled')) =
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
  -- TODO [knocking] when sending to all, send profile update to pending approval members too, then filter for next step?
  when shouldSendProfileUpdate $
    sendProfileUpdate `catchChatError` eToView
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
  -- TODO [knocking] Possibly we need to pass GroupSndScope through all functions to here to avoid ad-hoc filtering.
  recipientMembers <- case members of
    [m] | memberStatus m == GSMemPendingApproval -> pure [m]
    _ -> liftIO $ shuffleMembers (filter memberCurrent members)
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
  let sentTo = zipWith3 (\mId mReq r -> (mId, fmap (\(_, _, (_, msgIds)) -> msgIds) mReq, r)) sendToMemIds msgReqs delivered
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
    prepareMsgReqs msgFlags msgs toSendSeparate toSendBatched = do
      let batched_ = batchSndMessagesJSON msgs
      case L.nonEmpty batched_ of
        Just batched' -> do
          let lenMsgs = length msgs
              (memsSep, mreqsSep) = foldMembers lenMsgs sndMessageMBR msgs toSendSeparate
              (memsBtch, mreqsBtch) = foldMembers (length batched' + lenMsgs) msgBatchMBR batched' toSendBatched
          (memsSep <> memsBtch, mreqsSep <> mreqsBtch)
        Nothing -> ([], [])
      where
        foldMembers :: forall a. Int -> (Maybe Int -> Int -> a -> (ValueOrRef MsgBody, [MessageId])) -> NonEmpty (Either ChatError a) -> [(GroupMember, Connection)] -> ([GroupMemberId], [Either ChatError ChatMsgReq])
        foldMembers lastRef mkMb mbs mems = snd $ foldr' foldMsgBodies (lastMemIdx_, ([], [])) mems
          where
            lastMemIdx_ = let len = length mems in if len > 1 then Just len else Nothing
            foldMsgBodies :: (GroupMember, Connection) -> (Maybe Int, ([GroupMemberId], [Either ChatError ChatMsgReq])) -> (Maybe Int, ([GroupMemberId], [Either ChatError ChatMsgReq]))
            foldMsgBodies (GroupMember {groupMemberId}, conn) (memIdx_, memIdsReqs) =
              (subtract 1 <$> memIdx_,) $ snd $ foldr' addBody (lastRef, memIdsReqs) mbs
              where
                addBody :: Either ChatError a -> (Int, ([GroupMemberId], [Either ChatError ChatMsgReq])) -> (Int, ([GroupMemberId], [Either ChatError ChatMsgReq]))
                addBody mb (i, (memIds, reqs)) =
                  let req = (conn,msgFlags,) . mkMb memIdx_ i <$> mb
                   in (i - 1, (groupMemberId : memIds, req : reqs))
        sndMessageMBR :: Maybe Int -> Int -> SndMessage -> (ValueOrRef MsgBody, [MessageId])
        sndMessageMBR memIdx_ i SndMessage {msgId, msgBody} = (vrValue_ memIdx_ i msgBody, [msgId])
        msgBatchMBR :: Maybe Int -> Int -> MsgBatch -> (ValueOrRef MsgBody, [MessageId])
        msgBatchMBR memIdx_ i (MsgBatch batchBody sndMsgs) = (vrValue_ memIdx_ i batchBody, map (\SndMessage {msgId} -> msgId) sndMsgs)
        vrValue_ memIdx_ i v = case memIdx_ of
          Nothing -> VRValue Nothing v -- sending to one member, do not reference bodies
          Just 1 -> VRValue (Just i) v
          Just _ -> VRRef i
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
memberSendAction gInfo events members m@GroupMember {memberRole, memberStatus} = case memberConn m of
  Nothing -> pendingOrForwarded
  Just conn@Connection {connStatus}
    | connDisabled conn || connStatus == ConnDeleted || memberStatus == GSMemRejected -> Nothing
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

sendGroupMemberMessage :: MsgEncodingI e => GroupInfo -> GroupMember -> ChatMsgEvent e -> Maybe Int64 -> CM () -> CM ()
sendGroupMemberMessage gInfo@GroupInfo {groupId} m@GroupMember {groupMemberId} chatMsgEvent introId_ postDeliver = do
  msg <- createSndMessage chatMsgEvent (GroupId groupId)
  messageMember msg `catchChatError` eToView
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
          else toView $ CEvtMessageError user "error" "saveGroupFwdRcvMsg: referenced author member id doesn't match message member id"
        throwError e
      _ -> throwError e

saveSndChatItem :: ChatTypeI c => User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> CM (ChatItem c 'MDSnd)
saveSndChatItem user cd msg content = saveSndChatItem' user cd msg content Nothing Nothing Nothing Nothing False

-- TODO [mentions] optimize by avoiding unnecesary parsing of control messages
saveSndChatItem' :: ChatTypeI c => User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> Maybe (CIFile 'MDSnd) -> Maybe (CIQuote c) -> Maybe CIForwardedFrom -> Maybe CITimed -> Bool -> CM (ChatItem c 'MDSnd)
saveSndChatItem' user cd msg content ciFile quotedItem itemForwarded itemTimed live = do
  let itemTexts = ciContentTexts content
  saveSndChatItems user cd Nothing [Right NewSndChatItemData {msg, content, itemTexts, itemMentions = M.empty, ciFile, quotedItem, itemForwarded}] itemTimed live >>= \case
    [Right ci] -> pure ci
    _ -> throwChatError $ CEInternalError "saveSndChatItem': expected 1 item"

data NewSndChatItemData c = NewSndChatItemData
  { msg :: SndMessage,
    content :: CIContent 'MDSnd,
    itemTexts :: (Text, Maybe MarkdownList),
    itemMentions :: Map MemberName CIMention,
    ciFile :: Maybe (CIFile 'MDSnd),
    quotedItem :: Maybe (CIQuote c),
    itemForwarded :: Maybe CIForwardedFrom
  }

saveSndChatItems ::
  forall c.
  ChatTypeI c =>
  User ->
  ChatDirection c 'MDSnd ->
  Maybe NotInHistory ->
  [Either ChatError (NewSndChatItemData c)] ->
  Maybe CITimed ->
  Bool ->
  CM [Either ChatError (ChatItem c 'MDSnd)]
saveSndChatItems user cd notInHistory_ itemsData itemTimed live = do
  createdAt <- liftIO getCurrentTime
  when (contactChatDeleted cd || any (\NewSndChatItemData {content} -> ciRequiresAttention content) (rights itemsData)) $
    withStore' (\db -> updateChatTs db user cd createdAt)
  lift $ withStoreBatch (\db -> map (bindRight $ createItem db createdAt) itemsData)
  where
    createItem :: DB.Connection -> UTCTime -> NewSndChatItemData c -> IO (Either ChatError (ChatItem c 'MDSnd))
    createItem db createdAt NewSndChatItemData {msg = msg@SndMessage {sharedMsgId}, content, itemTexts, itemMentions, ciFile, quotedItem, itemForwarded} = do
      ciId <- createNewSndChatItem db user cd notInHistory_ msg content quotedItem itemForwarded itemTimed live createdAt
      forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
      let ci = mkChatItem_ cd ciId content itemTexts ciFile quotedItem (Just sharedMsgId) itemForwarded itemTimed live False createdAt Nothing createdAt
      Right <$> case cd of
        CDGroupSnd g | not (null itemMentions) -> createGroupCIMentions db g ci itemMentions
        _ -> pure ci

saveRcvChatItemNoParse :: (ChatTypeI c, ChatTypeQuotable c) => User -> ChatDirection c 'MDRcv -> RcvMessage -> UTCTime -> CIContent 'MDRcv -> CM (ChatItem c 'MDRcv)
saveRcvChatItemNoParse user cd msg brokerTs = saveRcvChatItem user cd msg brokerTs . ciContentNoParse

saveRcvChatItem :: (ChatTypeI c, ChatTypeQuotable c) => User -> ChatDirection c 'MDRcv -> RcvMessage -> UTCTime -> (CIContent 'MDRcv, (Text, Maybe MarkdownList)) -> CM (ChatItem c 'MDRcv)
saveRcvChatItem user cd msg@RcvMessage {sharedMsgId_} brokerTs content =
  saveRcvChatItem' user cd Nothing msg sharedMsgId_ brokerTs content Nothing Nothing False M.empty

ciContentNoParse :: CIContent 'MDRcv -> (CIContent 'MDRcv, (Text, Maybe MarkdownList))
ciContentNoParse content = (content, (ciContentToText content, Nothing))

saveRcvChatItem' :: (ChatTypeI c, ChatTypeQuotable c) => User -> ChatDirection c 'MDRcv -> Maybe NotInHistory -> RcvMessage -> Maybe SharedMsgId -> UTCTime -> (CIContent 'MDRcv, (Text, Maybe MarkdownList)) -> Maybe (CIFile 'MDRcv) -> Maybe CITimed -> Bool -> Map MemberName MsgMention -> CM (ChatItem c 'MDRcv)
saveRcvChatItem' user cd notInHistory_ msg@RcvMessage {chatMsgEvent, forwardedByMember} sharedMsgId_ brokerTs (content, (t, ft_)) ciFile itemTimed live mentions = do
  createdAt <- liftIO getCurrentTime
  withStore' $ \db -> do
    when (ciRequiresAttention content || contactChatDeleted cd) $ updateChatTs db user cd createdAt
    (mentions' :: Map MemberName CIMention, userMention) <- case cd of
      CDGroupRcv g@GroupInfo {membership} _ -> do
        mentions' <- getRcvCIMentions db user g ft_ mentions
        let userReply = case cmToQuotedMsg chatMsgEvent of
              Just QuotedMsg {msgRef = MsgRef {memberId = Just mId}} -> sameMemberId mId membership
              _ -> False
            userMention' = userReply || any (\CIMention {memberId} -> sameMemberId memberId membership) mentions'
         in pure (mentions', userMention')
      CDDirectRcv _ -> pure (M.empty, False)
    (ciId, quotedItem, itemForwarded) <- createNewRcvChatItem db user cd notInHistory_ msg sharedMsgId_ content itemTimed live userMention brokerTs createdAt
    forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
    let ci = mkChatItem_ cd ciId content (t, ft_) ciFile quotedItem sharedMsgId_ itemForwarded itemTimed live userMention brokerTs forwardedByMember createdAt
    case cd of
      CDGroupRcv g _ | not (null mentions') -> createGroupCIMentions db g ci mentions'
      _ -> pure ci

-- TODO [mentions] optimize by avoiding unnecessary parsing
mkChatItem :: (ChatTypeI c, MsgDirectionI d) => ChatDirection c d -> ChatItemId -> CIContent d -> Maybe (CIFile d) -> Maybe (CIQuote c) -> Maybe SharedMsgId -> Maybe CIForwardedFrom -> Maybe CITimed -> Bool -> Bool -> ChatItemTs -> Maybe GroupMemberId -> UTCTime -> ChatItem c d
mkChatItem cd ciId content file quotedItem sharedMsgId itemForwarded itemTimed live userMention itemTs forwardedByMember currentTs =
  let ts = ciContentTexts content
   in mkChatItem_ cd ciId content ts file quotedItem sharedMsgId itemForwarded itemTimed live userMention itemTs forwardedByMember currentTs

mkChatItem_ :: (ChatTypeI c, MsgDirectionI d) => ChatDirection c d -> ChatItemId -> CIContent d -> (Text, Maybe MarkdownList) -> Maybe (CIFile d) -> Maybe (CIQuote c) -> Maybe SharedMsgId -> Maybe CIForwardedFrom -> Maybe CITimed -> Bool -> Bool -> ChatItemTs -> Maybe GroupMemberId -> UTCTime -> ChatItem c d
mkChatItem_ cd ciId content (itemText, formattedText) file quotedItem sharedMsgId itemForwarded itemTimed live userMention itemTs forwardedByMember currentTs =
  let itemStatus = ciCreateStatus content
      meta = mkCIMeta ciId content itemText itemStatus Nothing sharedMsgId itemForwarded Nothing False itemTimed (justTrue live) userMention currentTs itemTs forwardedByMember currentTs currentTs
   in ChatItem {chatDir = toCIDirection cd, meta, content, mentions = M.empty, formattedText, quotedItem, reactions = [], file}

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

deleteAgentConnectionAsync :: ConnId -> CM ()
deleteAgentConnectionAsync acId = deleteAgentConnectionAsync' acId False
{-# INLINE deleteAgentConnectionAsync #-}

deleteAgentConnectionAsync' :: ConnId -> Bool -> CM ()
deleteAgentConnectionAsync' acId waitDelivery = do
  withAgent (\a -> deleteConnectionAsync a waitDelivery acId) `catchChatError` eToView

deleteAgentConnectionsAsync :: [ConnId] -> CM ()
deleteAgentConnectionsAsync acIds = deleteAgentConnectionsAsync' acIds False
{-# INLINE deleteAgentConnectionsAsync #-}

deleteAgentConnectionsAsync' :: [ConnId] -> Bool -> CM ()
deleteAgentConnectionsAsync' [] _ = pure ()
deleteAgentConnectionsAsync' acIds waitDelivery = do
  withAgent (\a -> deleteConnectionsAsync a waitDelivery acIds) `catchChatError` eToView

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
  unless (null errs) $ toView' $ CEvtChatErrors errs
  toView' $ CEvtNewChatItems user acis
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
    [Right aci] -> toView $ CEvtNewChatItems user [aci]
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
      let ci = mkChatItem cd ciId content Nothing Nothing Nothing Nothing Nothing False False itemTs Nothing createdAt
      pure $ AChatItem (chatTypeI @c) (msgDirection @d) (toChatInfo cd) ci

createLocalChatItems ::
  User ->
  ChatDirection 'CTLocal 'MDSnd ->
  NonEmpty (CIContent 'MDSnd, Maybe (CIFile 'MDSnd), Maybe CIForwardedFrom, (Text, Maybe MarkdownList)) ->
  UTCTime ->
  CM [ChatItem 'CTLocal 'MDSnd]
createLocalChatItems user cd itemsData createdAt = do
  withStore' $ \db -> updateChatTs db user cd createdAt
  (errs, items) <- lift $ partitionEithers <$> withStoreBatch' (\db -> map (createItem db) $ L.toList itemsData)
  unless (null errs) $ toView $ CEvtChatErrors errs
  pure items
  where
    createItem :: DB.Connection -> (CIContent 'MDSnd, Maybe (CIFile 'MDSnd), Maybe CIForwardedFrom, (Text, Maybe MarkdownList)) -> IO (ChatItem 'CTLocal 'MDSnd)
    createItem db (content, ciFile, itemForwarded, ts) = do
      ciId <- createNewChatItem_ db user cd Nothing Nothing Nothing content (Nothing, Nothing, Nothing, Nothing, Nothing) itemForwarded Nothing False False createdAt Nothing createdAt
      forM_ ciFile $ \CIFile {fileId} -> updateFileTransferChatItemId db fileId ciId createdAt
      pure $ mkChatItem_ cd ciId content ts ciFile Nothing Nothing itemForwarded Nothing False False createdAt Nothing createdAt

withUser' :: (User -> CM ChatResponse) -> CM ChatResponse
withUser' action =
  asks currentUser
    >>= readTVarIO
    >>= maybe (throwChatError CENoActiveUser) action

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

adminContactReq :: ConnReqContact
adminContactReq =
  either error id $ strDecode "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

simplexTeamContactProfile :: Profile
simplexTeamContactProfile =
  Profile
    { displayName = "SimpleX Chat team",
      fullName = "",
      image = Just (ImageData "data:image/jpg;base64,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"),
      contactLink = Just $ CLFull adminContactReq,
      preferences = Nothing
    }

simplexStatusContactProfile :: Profile
simplexStatusContactProfile =
  Profile
    { displayName = "SimpleX-Status",
      fullName = "",
      image = Just (ImageData "data:image/jpg;base64,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"),
      contactLink = Just (either error CLFull $ strDecode "simplex:/contact/#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FShQuD-rPokbDvkyotKx5NwM8P3oUXHxA%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEA6fSx1k9zrOmF0BJpCaTarZvnZpMTAVQhd3RkDQ35KT0%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"),
      preferences = Nothing
    }

timeItToView :: String -> CM' a -> CM' a
timeItToView s action = do
  t1 <- liftIO getCurrentTime
  a <- action
  t2 <- liftIO getCurrentTime
  let diff = diffToMilliseconds $ diffUTCTime t2 t1
  toView' $ CEvtTimedAction s diff
  pure a
