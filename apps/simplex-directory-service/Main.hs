{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
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
import Simplex.Chat.Bot.KnownContacts
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
directoryService st@DirectoryStore {} DirectoryOpts {welcomeMessage, superUsers} _user cc = do
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
          atomically $ addGroupReg st ct g
          r <- sendChatCmd cc $ APIJoinGroup groupId
          sendMessage cc ct $ T.unpack $ case r of
            CRUserAcceptedGroupSent {} -> "Joining the group #" <> displayName <> "…"
            _ -> "Error joining group " <> displayName <> ", please re-send the invitation!"
      DEServiceJoinedGroup ctId g -> withGroupReg g "joined group" $ \gr@GroupReg {dbContactId, groupRegStatus} -> do
        let GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = g
        sendMessage' cc ctId $ T.unpack $ "Joined the group #" <> displayName <> ", creating the link…"
        sendChatCmd cc (APICreateGroupLink groupId GRMember) >>= \case
          CRGroupLinkCreated {connReqContact} -> do
            atomically $ writeTVar groupRegStatus GRSPendingUpdate
            sendMessage' cc ctId $
              "Created the public link to join the group via this service that is always online\n\n\
              \Please add it to the group welcome message.\n\
              \For example, add:\n\n\
              \Link to join the group: " <> B.unpack (strEncode connReqContact)
          CRChatCmdError _ (ChatError e) -> case e of
            CEGroupUserRole {} -> sendMessage' cc ctId "Failed creating group link, as service is no longer an admin."
            CEGroupMemberUserRemoved -> sendMessage' cc ctId "Failed creating group link, as service is removed from the group."
            CEGroupNotJoined _ -> sendMessage' cc ctId $ unexpectedError "group not joined"
            CEGroupMemberNotActive -> sendMessage' cc ctId $ unexpectedError "service membership is not active"
            _ -> sendMessage' cc ctId $ unexpectedError "can't create group link"
          _ -> sendMessage' cc ctId $ unexpectedError "can't create group link"
      DEGroupUpdated {contactId, fromGroup, toGroup} -> do
        putStrLn "DEGroupUpdated"
        unless (sameProfile p p') $ do
          atomically $ unlistGroup st groupId
          withGroupReg toGroup "group updated" $ \gr@GroupReg {dbContactId, groupRegStatus} -> do
            readTVarIO groupRegStatus >>= \case
              GRSPendingConfirmation -> pure ()
              GRSProposed -> pure ()
              GRSPendingUpdate ->
                when (contactId == dbContactId) $ -- we do not need to process updates made by other members in this case
                  sendChatCmd cc (APIGetGroupLink groupId) >>= \case
                    CRGroupLink {connReqContact} -> do
                      let groupLink = safeDecodeUtf8 $ strEncode connReqContact
                          hadLinkBefore = groupLink `isInfix` description
                          hasLinkNow = groupLink `isInfix` description'
                      putStrLn $ "hadLinkBefore " <> show hadLinkBefore
                      putStrLn $ "hasLinkNow " <> show hasLinkNow
                      case (hadLinkBefore, hasLinkNow) of
                        (True, True) -> do
                          sendMessage' cc contactId $ "The group profile is updated: the group registration is suspended and it will not appear in search results until re-approved"
                          -- TODO suspend group listing, send for approval
                        (True, False) -> do
                          sendMessage' cc contactId $ "The group link is removed, the group registration is suspended and it will not appear in search results"
                          -- TODO suspend group listing, remove approval code
                          atomically $ writeTVar groupRegStatus GRSPendingUpdate
                        (False, True) -> do
                          sendMessage' cc contactId $ "The group link added to the welcome message - thank you!"
                          let gaId = 1
                          atomically $ writeTVar groupRegStatus $ GRSPendingApproval gaId
                          sendForApproval toGroup gr gaId
                        (False, False) -> pure ()
                          -- check status, remove approval code, remove listing
                    _ -> pure () -- TODO handle errors
              GRSPendingApproval n -> do
                let gaId = n + 1
                atomically $ writeTVar groupRegStatus $ GRSPendingApproval gaId
                notifySuperUsers $ T.unpack $ "Group registration updated for ID " <> tshow groupId <> ": " <> localDisplayName
                sendForApproval toGroup gr gaId
              GRSActive -> do
                let gaId = 1
                atomically $ writeTVar groupRegStatus $ GRSPendingApproval gaId
                notifySuperUsers $ T.unpack $ "Group profile updated, group suspended for ID " <> tshow groupId <> ": " <> localDisplayName
                sendForApproval toGroup gr gaId
                sendMessage' cc dbContactId $ T.unpack $ "The group profile is updated, the group registration is suspended until re-approved for ID " <> tshow (userGroupRegId gr) <> ": " <> n'
              GRSSuspended -> pure ()
        where
          isInfix l d_ = l `T.isInfixOf` fromMaybe "" d_
          GroupInfo {groupId, groupProfile = p@GroupProfile {displayName = n, description}} = fromGroup
          GroupInfo {localDisplayName, groupProfile = p'@GroupProfile {displayName = n', description = description'}} = toGroup
          sameProfile
            GroupProfile {displayName = n, fullName = fn, image = i, description = d}
            GroupProfile {displayName = n', fullName = fn', image = i', description = d'} =
              n == n' && fn == fn' && i == i' && d == d'
          sendForApproval GroupInfo {localDisplayName} GroupReg {dbGroupId} gaId = do
            notifySuperUsers $ T.unpack $ "To approve group ID " <> tshow dbGroupId <> ": " <> localDisplayName <> " send:\n/approve " <> tshow dbGroupId <> ":" <> localDisplayName <> " " <> tshow gaId
      DEContactRoleChanged _ctId _g _role -> pure ()
      DEServiceRoleChanged _g _role -> pure ()
      DEContactRemovedFromGroup _ctId _g -> pure ()
      DEContactLeftGroup _ctId _g -> pure ()
      DEServiceRemovedFromGroup _g -> pure ()
      DEGroupDeleted _g -> pure ()
      DEUnsupportedMessage _ct _ciId -> pure ()
      DEItemEditIgnored _ct -> pure ()
      DEItemDeleteIgnored _ct -> pure ()
      DEContactCommand ct ciId aCmd -> case aCmd of
        ADC SDRUser cmd -> case cmd of
          DCHelp -> pure ()
          DCSearchGroup _s -> pure ()
          DCConfirmDuplicateGroup _ugrId _gName -> pure ()
          DCListUserGroups -> pure ()
          DCDeleteGroup _ugrId _gName -> pure ()
          DCUnknownCommand -> sendReply ct ciId "Unknown command"
          DCCommandError tag -> sendReply ct ciId $ "Command error: " <> show tag
        ADC SDRSuperUser cmd -- TODO check group status
          | superUser `elem` superUsers -> case cmd of
            DCApproveGroup {groupId, localDisplayName = n, groupApprovalId} ->
              withGroupReg' groupId "approve group" $ \GroupReg {dbContactId, groupRegStatus} ->
                getGroup cc groupId >>= \case
                  Just GroupInfo {localDisplayName = n'}
                    | n == n' -> do
                      atomically $ do
                        writeTVar groupRegStatus GRSActive
                        listGroup st groupId
                      sendMessage cc ct "Group approved!"
                      sendMessage' cc dbContactId $ T.unpack $ "Group approved and listed for ID " <> tshow groupId <> ": " <> n'
                    | otherwise -> sendMessage cc ct "Incorrect group name"
                  Nothing -> pure ()
            DCRejectGroup _gaId _gName -> pure ()
            DCSuspendGroup _gId _gName -> pure ()
            DCResumeGroup _gId _gName -> pure ()
            DCListGroups -> pure ()
            DCCommandError tag -> sendReply ct ciId $ "Command error: " <> show tag
          | otherwise -> sendReply ct ciId "You are not allowed to use this command"
          where
            superUser = KnownContact {contactId = contactId' ct, localDisplayName = localDisplayName' ct}
        where
          sendReply ct ciId = sendComposedMessage cc ct (Just ciId) . textMsgContent
  where
    contactConnected ct = putStrLn $ T.unpack (localDisplayName' ct) <> " connected"
    withContactGroupReg ctId g err action = withContact ctId g err $ withGroupReg g err . action
    notifySuperUsers s = void . forkIO $ forM_ superUsers $ \KnownContact {contactId} -> sendMessage' cc contactId s
    withContact ctId GroupInfo {localDisplayName} err action = do
      getContact cc ctId >>= \case
        Just ct -> action ct
        Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find contact ID " <> tshow ctId
    withGroupReg GroupInfo {groupId, localDisplayName} err action = do
      atomically (getGroupReg st groupId) >>= \case
        Just gr -> action gr
        Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find group registration ID " <> tshow groupId
    withGroupReg' groupId err action = do
      atomically (getGroupReg st groupId) >>= \case
        Just gr -> action gr
        Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", can't find group registration ID " <> tshow groupId

badInvitation :: GroupMemberRole -> GroupMemberRole -> Maybe String
badInvitation contactRole serviceRole = case (contactRole, serviceRole) of
  (GROwner, GRAdmin) -> Nothing
  (_, GRAdmin) -> Just "You must have a group _owner_ role to register the group"
  (GROwner, _) -> Just "You must grant directory service _admin_ role to register the group"
  _ -> Just "You must have a group _owner_ role and you must grant directory service _admin_ role to register the group"

getContact :: ChatController -> ContactId -> IO (Maybe Contact)
getContact cc ctId = resp <$> sendChatCmd cc (APIGetChat (ChatRef CTDirect ctId) (CPLast 0) Nothing)
  where
    resp :: ChatResponse -> Maybe Contact
    resp = \case
      CRApiChat _ (AChat SCTDirect Chat {chatInfo = DirectChat ct}) -> Just ct
      _ -> Nothing

getGroup :: ChatController -> GroupId -> IO (Maybe GroupInfo)
getGroup cc gId = resp <$> sendChatCmd cc (APIGetChat (ChatRef CTGroup gId) (CPLast 0) Nothing)
  where
    resp :: ChatResponse -> Maybe GroupInfo
    resp = \case
      CRApiChat _ (AChat SCTGroup Chat {chatInfo = GroupChat g}) -> Just g
      _ -> Nothing

unexpectedError :: String -> String
unexpectedError err = "Unexpected error: " <> err <> ", please notify the developers."
