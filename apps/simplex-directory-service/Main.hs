{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Directory.Events
import Directory.Options
import Directory.Store
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
-- import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Options
-- import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Types
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import System.Directory (getAppUserDataDirectory)

main :: IO ()
main = do
  opts@DirectoryOpts {directoryLog} <- welcomeGetOpts
  st <- getDirectoryStore directoryLog
  simplexChatCore terminalChatConfig (mkChatOpts opts) Nothing $ directoryService st opts

welcomeGetOpts :: IO DirectoryOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@DirectoryOpts {coreOptions = CoreChatOpts {dbFilePrefix}} <- getDirectoryOpts appDir "simplex_directory_service"
  putStrLn $ "SimpleX Directory Service Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

directoryService :: DirectoryStore -> DirectoryOpts -> User -> ChatController -> IO ()
directoryService st@DirectoryStore {} DirectoryOpts {welcomeMessage} _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    forM_ (crDirectoryEvent resp) $ \case
      DEContactConnected ct -> do
        contactConnected ct
        sendMessage cc ct welcomeMessage
      DEGroupInvitation {contact = ct, groupInfo = g, fromMemberRole, memberRole} -> case badInvitation fromMemberRole memberRole of
        -- TODO check duplicate group name and ask to confirm
        Just msg -> sendMessage cc ct msg
        Nothing -> do
          let GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = g
          atomically $ addGroupReg st g
          r <- sendChatCmd cc $ APIJoinGroup groupId
          sendMessage cc ct $ T.unpack $ case r of
            CRUserAcceptedGroupSent {} -> "Joining the group #" <> displayName <> "…"
            _ -> "Error joining group " <> displayName <> ", please re-send the invitation!"
      DEServiceJoinedGroup ctId g -> withContact ctId g "joined group" $ \ct -> do
        let GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = g
        sendMessage cc ct $ T.unpack $ "Joined the group #" <> displayName <> ", creating the link…"
        r <- sendChatCmd cc $ APICreateGroupLink groupId GRMember
        sendMessage cc ct $ case r of
          CRGroupLinkCreated {connReqContact} ->
              "Created the public link to join the group via this service that is always online\n\n\
              \Please add it to the group welcome message.\n\
              \For example, add:\n\n\
              \Link to join the group: " <> B.unpack (strEncode connReqContact)
          CRChatCmdError _ (ChatError e) -> case e of
            CEGroupUserRole {} -> "Failed creating group link, as service is no longer an admin."
            CEGroupMemberUserRemoved -> "Failed creating group link, as service is removed from the group."
            CEGroupNotJoined _ -> unexpectedError "group not joined"
            CEGroupMemberNotActive -> unexpectedError "service membership is not active"
            _ -> unexpectedError "can't create group link"
          _ -> unexpectedError "can't create group link"
      DEGroupUpdated {contactId, fromGroup, toGroup} -> withContact contactId toGroup "group updated" $ \ct -> do
        -- TODO we need to find registration here to see if link is expected in profile or we can ignore it at this point
        let GroupInfo {groupId, groupProfile = p@GroupProfile {displayName = n, description}} = fromGroup
            GroupInfo {groupProfile = p'@GroupProfile {displayName = n', description = description'}} = toGroup
        unless (sameProfile p p') $ do
          sendChatCmd cc (APIGetGroupLink groupId) >>= \case
            CRGroupLink {connReqContact} -> do
              let groupLink = safeDecodeUtf8 $ strEncode connReqContact
                  hadLinkBefore = groupLink `isInfix` description
                  hasLinkNow = groupLink `isInfix` description'
              case (hadLinkBefore, hasLinkNow) of
                (True, True) -> do
                  sendMessage cc ct $ "The group profile is updated: the group registration is suspended and it will not appear in search results until re-approved"
                  -- TODO suspend group listing, send for approval
                (True, False) -> do
                  sendMessage cc ct $ "The group link is removed, the group registration is suspended and it will not appear in search results"
                  -- TODO suspend group listing, remove approval code
                (False, True) -> do
                  sendMessage cc ct $ "The group link added to the welcome message - thank you!"
                  -- check status and possibly send for approval
                (False, False) -> pure ()
                  -- check status, remove approval code, remove listing
            _ -> pure () -- TODO handle errors
        where
          isInfix l d_ = l `T.isInfixOf` fromMaybe "" d_
          sameProfile
            GroupProfile {displayName = n, fullName = fn, image = i, description = d}
            GroupProfile {displayName = n', fullName = fn', image = i', description = d'} =
              n == n' && fn == fn' && i == i' && d == d'
      DEContactRoleChanged _ctId _g _role -> pure ()
      DEServiceRoleChanged _g _role -> pure ()
      DEContactRemovedFromGroup _ctId _g -> pure ()
      DEContactLeftGroup _ctId _g -> pure ()
      DEServiceRemovedFromGroup _g -> pure ()
      DEGroupDeleted _g -> pure ()
      DEUnsupportedMessage _ct _ciId -> pure ()
      DEItemEditIgnored _ct -> pure ()
      DEItemDeleteIgnored _ct -> pure ()
      DEContactCommand _ct aCmd -> case aCmd of
        ADC SDRUser cmd -> case cmd of
          DCHelp -> pure ()
          DCSearchGroup _s -> pure ()
          DCConfirmDuplicateGroup _ugrId _gName -> pure ()
          DCListUserGroups -> pure ()
          DCDeleteGroup _ugrId _gName -> pure ()
          DCUnknownCommand _ciId -> pure ()
          DCCommandError _ciId _tag -> pure ()
        ADC SDRSuperUser cmd -> case cmd of
          DCApproveGroup _gaId _gName -> pure ()
          DCRejectGroup _gaId _gName -> pure ()
          DCSuspendGroup _gId _gName -> pure ()
          DCResumeGroup _gId _gName -> pure ()
          DCListGroups -> pure ()
          DCCommandError _ciId _tag -> pure ()


    --   CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat ct) ci@ChatItem {content = CIRcvMsgContent mc})
    --     | publisher `elem` publishers ->
    --       if allowContent mc
    --         then do
    --           sendChatCmd cc "/contacts" >>= \case
    --             CRContactsList _ cts -> void . forkIO $ do
    --               let cts' = filter broadcastTo cts
    --               forM_ cts' $ \ct' -> sendComposedMessage cc ct' Nothing mc
    --               sendReply $ "Forwarded to " <> show (length cts') <> " contact(s)"
    --             r -> putStrLn $ "Error getting contacts list: " <> show r
    --         else sendReply "!1 Message is not supported!"
    --     | otherwise -> do
    --       sendReply prohibitedMessage
    --       deleteMessage cc ct $ chatItemId' ci
    --     where
    --       sendReply = sendComposedMessage cc ct (Just $ chatItemId' ci) . textMsgContent
    --       publisher = Publisher {contactId = contactId' ct, localDisplayName = localDisplayName' ct}
    --       allowContent = \case
    --         MCText _ -> True
    --         MCLink {} -> True
    --         MCImage {} -> True
    --         _ -> False
    --       broadcastTo ct'@Contact {activeConn = conn@Connection {connStatus}} =
    --         (connStatus == ConnSndReady || connStatus == ConnReady)
    --           && not (connDisabled conn)
    --           && contactId' ct' /= contactId' ct
  where
    contactConnected ct = putStrLn $ T.unpack (localDisplayName' ct) <> " connected"
    withContact ctId GroupInfo {localDisplayName} err action = do
      getContact cc ctId >>= \case
        Just ct -> action ct
        Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find contact ID " <> tshow ctId

badInvitation :: GroupMemberRole -> GroupMemberRole -> Maybe String
badInvitation contactRole serviceRole = case (contactRole, serviceRole) of
  (GROwner, GRAdmin) -> Nothing
  (_, GRAdmin) -> Just "You must have a group _owner_ role to register the group"
  (GROwner, _) -> Just "You must grant directory service _admin_ role to register the group"
  _ -> Just "You must have a group _owner_ role and you must grant directory service _admin_ role to register the group"

getContact :: ChatController -> Int64 -> IO (Maybe Contact)
getContact cc ctId = resp <$> sendChatCmd cc (APIGetChat (ChatRef CTDirect ctId) (CPLast 0) Nothing)
  where
    resp :: ChatResponse -> Maybe Contact
    resp = \case
      CRApiChat _ (AChat SCTDirect Chat {chatInfo = DirectChat ct}) -> Just ct
      _ -> Nothing

unexpectedError :: String -> String
unexpectedError err = "Unexpected error: " <> err <> ", please notify the developers."
