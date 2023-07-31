{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Directory.Service
  ( welcomeGetOpts,
    directoryService,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
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
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Types
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import System.Directory (getAppUserDataDirectory)

data GroupProfileUpdate = GPNoServiceLink | GPServiceLinkAdded | GPServiceLinkRemoved | GPHasServiceLink | GPServiceLinkError

welcomeGetOpts :: IO DirectoryOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@DirectoryOpts {coreOptions = CoreChatOpts {dbFilePrefix}} <- getDirectoryOpts appDir "simplex_directory_service"
  putStrLn $ "SimpleX Directory Service Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

directoryService :: DirectoryStore -> DirectoryOpts -> User -> ChatController -> IO ()
directoryService st DirectoryOpts {superUsers, serviceName} User {userId} cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    forM_ (crDirectoryEvent resp) $ \case
      DEContactConnected ct -> deContactConnected ct
      DEGroupInvitation {contact = ct, groupInfo = g, fromMemberRole, memberRole} -> deGroupInvitation ct g fromMemberRole memberRole
      DEServiceJoinedGroup ctId g -> deServiceJoinedGroup ctId g
      DEGroupUpdated {contactId, fromGroup, toGroup} -> deGroupUpdated contactId fromGroup toGroup
      DEContactRoleChanged ctId g role -> deContactRoleChanged ctId g role
      DEServiceRoleChanged g role -> deServiceRoleChanged g role
      DEContactRemovedFromGroup ctId g -> deContactRemovedFromGroup ctId g
      DEContactLeftGroup ctId g -> deContactLeftGroup ctId g
      DEServiceRemovedFromGroup g -> deServiceRemovedFromGroup g
      DEGroupDeleted _g -> pure ()
      DEUnsupportedMessage _ct _ciId -> pure ()
      DEItemEditIgnored _ct -> pure ()
      DEItemDeleteIgnored _ct -> pure ()
      DEContactCommand ct ciId aCmd -> case aCmd of
        ADC SDRUser cmd -> deUserCommand ct ciId cmd
        ADC SDRSuperUser cmd -> deSuperUserCommand ct ciId cmd
  where
    -- withContactGroupReg ctId g err action = withContact ctId g err $ withGroupReg g err . action
    withSuperUsers action = void . forkIO $ forM_ superUsers $ \KnownContact {contactId} -> action contactId
    notifySuperUsers s = withSuperUsers $ \contactId -> sendMessage' cc contactId s
    -- withContact ctId GroupInfo {localDisplayName} err action = do
    --   getContact cc ctId >>= \case
    --     Just ct -> action ct
    --     Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find contact ID " <> tshow ctId
    withGroupReg GroupInfo {groupId, localDisplayName} err action = do
      atomically (getGroupReg st groupId) >>= \case
        Just gr -> action gr
        Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find group registration ID " <> tshow groupId
    -- withGroupReg' groupId err action = do
    --   atomically (getGroupReg st groupId) >>= \case
    --     Just gr -> action gr
    --     Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", can't find group registration ID " <> tshow groupId
    groupInfoText GroupProfile {displayName = n, fullName = fn, description = d} =
      n <> (if n == fn || T.null fn then "" else " (" <> fn <> ")") <> maybe "" ("\nWelcome message:\n" <>) d
    groupReference GroupInfo {groupId, groupProfile = p'@GroupProfile {displayName}} =
      "ID " <> show groupId <> " (" <> T.unpack displayName <> ")"

    deContactConnected :: Contact -> IO ()
    deContactConnected ct = do
      putStrLn $ T.unpack (localDisplayName' ct) <> " connected"
      sendMessage cc ct $
        "Welcome to " <> serviceName <> " service!\n\
        \Send a search string to find groups or */help* to learn how to add groups to directory.\n\n\
        \For example, send _privacy_ to find groups about privacy."

    deGroupInvitation :: Contact -> GroupInfo -> GroupMemberRole -> GroupMemberRole -> IO ()
    deGroupInvitation ct g fromMemberRole memberRole =
      case badInvitation fromMemberRole memberRole of
        -- TODO check duplicate group name and ask to confirm
        Just msg -> sendMessage cc ct msg
        Nothing -> do
          let GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = g
          atomically $ addGroupReg st ct g
          r <- sendChatCmd cc $ APIJoinGroup groupId
          sendMessage cc ct $ T.unpack $ case r of
            CRUserAcceptedGroupSent {} -> "Joining the group #" <> displayName <> "…"
            _ -> "Error joining group " <> displayName <> ", please re-send the invitation!"

    deServiceJoinedGroup :: ContactId -> GroupInfo -> IO ()
    deServiceJoinedGroup ctId g =
      withGroupReg g "joined group" $ \GroupReg {groupRegStatus} -> do
        let GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = g
        sendMessage' cc ctId $ T.unpack $ "Joined the group #" <> displayName <> ", creating the link…"
        sendChatCmd cc (APICreateGroupLink groupId GRMember) >>= \case
          CRGroupLinkCreated {connReqContact} -> do
            atomically $ writeTVar groupRegStatus GRSPendingUpdate
            sendMessage' cc ctId
              "Created the public link to join the group via this directory service that is always online.\n\n\
              \Please add it to the group welcome message.\n\
              \For example, add:"
            sendMessage' cc ctId $ "Link to join the group " <> T.unpack displayName <> ": " <> B.unpack (strEncode connReqContact)
          CRChatCmdError _ (ChatError e) -> case e of
            CEGroupUserRole {} -> sendMessage' cc ctId "Failed creating group link, as service is no longer an admin."
            CEGroupMemberUserRemoved -> sendMessage' cc ctId "Failed creating group link, as service is removed from the group."
            CEGroupNotJoined _ -> sendMessage' cc ctId $ unexpectedError "group not joined"
            CEGroupMemberNotActive -> sendMessage' cc ctId $ unexpectedError "service membership is not active"
            _ -> sendMessage' cc ctId $ unexpectedError "can't create group link"
          _ -> sendMessage' cc ctId $ unexpectedError "can't create group link"

    deGroupUpdated :: ContactId -> GroupInfo -> GroupInfo -> IO ()
    deGroupUpdated contactId fromGroup toGroup =
      unless (sameProfile p p') $ do
        atomically $ unlistGroup st groupId
        withGroupReg toGroup "group updated" $ \gr@GroupReg {dbContactId} -> do
          readTVarIO (groupRegStatus gr) >>= \case
            GRSPendingConfirmation -> pure ()
            GRSProposed -> pure ()
            GRSPendingUpdate -> groupProfileUpdate >>= \case
              GPNoServiceLink ->
                when (contactId == dbContactId) $ sendMessage' cc contactId $ "Profile updated for " <> groupRef <> ", but the group link is not added to the welcome message."
              GPServiceLinkAdded
                | contactId == dbContactId -> do
                  sendMessage' cc contactId $ "Thank you! The group link for group " <> groupRef <> " added to the welcome message.\nYou will be notified once the group is added to the directory - it may take up to 24 hours."
                  let gaId = 1
                  atomically $ writeTVar (groupRegStatus gr) $ GRSPendingApproval gaId
                  sendForApproval gr gaId
                | otherwise -> sendMessage' cc contactId "The group link is added by another group member, your registration will not be processed."
              GPServiceLinkRemoved -> when (contactId == dbContactId) sendLinkRemoved
              GPHasServiceLink -> pure ()
              GPServiceLinkError -> do
                when (contactId == dbContactId) $ sendMessage' cc contactId $ "Error: " <> serviceName <> " has no link for group " <> groupRef <> ". Please report the error to the developers."
                putStrLn $ "Error: no link for group " <> groupRef
            GRSPendingApproval n -> processProfileChange gr $ n + 1
            GRSActive -> processProfileChange gr 1
            GRSSuspended -> processProfileChange gr 1
            GRSRemoved -> pure ()
      where
        isInfix l d_ = l `T.isInfixOf` fromMaybe "" d_
        GroupInfo {groupId, groupProfile = p} = fromGroup
        GroupInfo {localDisplayName, groupProfile = p'@GroupProfile {image = image'}} = toGroup
        groupRef = groupReference toGroup
        sameProfile
          GroupProfile {displayName = n, fullName = fn, image = i, description = d}
          GroupProfile {displayName = n', fullName = fn', image = i', description = d'} =
            n == n' && fn == fn' && i == i' && d == d'
        processProfileChange gr n' = groupProfileUpdate >>= \case
          GPNoServiceLink -> noLink
          GPServiceLinkRemoved -> noLink
          GPServiceLinkAdded -> hasLink
          GPHasServiceLink -> hasLink
          GPServiceLinkError -> putStrLn $ "Error: no link for group " <> groupRef <> " pending approval."
          where
            noLink = do
              atomically $ writeTVar (groupRegStatus gr) GRSPendingUpdate
              sendLinkRemoved
              notifySuperUsers $ "The link is removed from the group " <> groupRef <> "."
            hasLink = do
              atomically $ writeTVar (groupRegStatus gr) $ GRSPendingApproval n'
              notifySuperUsers $ "The group " <> groupRef <> " is updated."
              sendForApproval gr n'
        groupProfileUpdate = profileUpdate <$> sendChatCmd cc (APIGetGroupLink groupId)
          where
            profileUpdate = \case
              CRGroupLink {connReqContact} ->
                let groupLink = safeDecodeUtf8 $ strEncode connReqContact
                    hadLinkBefore = groupLink `isInfix` description p
                    hasLinkNow = groupLink `isInfix` description p'
                in if
                      | hadLinkBefore && hasLinkNow -> GPHasServiceLink
                      | hadLinkBefore -> GPServiceLinkRemoved
                      | hasLinkNow -> GPServiceLinkAdded
                      | otherwise -> GPNoServiceLink
              _ -> GPServiceLinkError
        sendForApproval GroupReg {dbGroupId, dbContactId} gaId = do
          ct_ <-  getContact cc dbContactId
          let text = maybe ("The group ID " <> tshow dbGroupId <> " submitted: ") (\c -> localDisplayName' c <> " submitted the group ID " <> tshow dbGroupId <> ": ") ct_
                      <> groupInfoText p' <> "\n\nTo approve send:"
              msg = maybe (MCText text) (\image -> MCImage {text, image}) image'
          withSuperUsers $ \ctId -> do
            sendComposedMessage' cc ctId Nothing msg
            sendMessage' cc ctId $ "/approve " <> show dbGroupId <> ":" <> T.unpack localDisplayName <> " " <> show gaId
        sendLinkRemoved = sendMessage' cc contactId $ "The link for group " <> groupRef <> " is removed from the welcome message, please add it."

    deContactRoleChanged :: ContactId -> GroupInfo -> GroupMemberRole -> IO ()
    deContactRoleChanged ctId g role = undefined

    deServiceRoleChanged :: GroupInfo -> GroupMemberRole -> IO ()
    deServiceRoleChanged g role = undefined

    deContactRemovedFromGroup :: ContactId -> GroupInfo -> IO ()
    deContactRemovedFromGroup ctId g =
      withGroupReg g "contact removed" $ \gr -> do
        atomically $ writeTVar (groupRegStatus gr) GRSRemoved
        let groupRef = groupReference g
        sendMessage' cc ctId $ "You are removed from the group " <> groupRef <> ".\n\nGroup is no longer listed in the directory."
        notifySuperUsers $ "The group " <> groupRef <> " is de-listed (group owner is removed)."

    deContactLeftGroup :: ContactId -> GroupInfo -> IO ()
    deContactLeftGroup ctId g =
      withGroupReg g "contact left" $ \gr -> do
        atomically $ writeTVar (groupRegStatus gr) GRSRemoved
        let groupRef = groupReference g
        sendMessage' cc ctId $ "You left the group " <> groupRef <> ".\n\nGroup is no longer listed in the directory."
        notifySuperUsers $ "The group " <> groupRef <> " is de-listed (group owner left)."

    deServiceRemovedFromGroup :: GroupInfo -> IO ()
    deServiceRemovedFromGroup g =
      withGroupReg g "contact left" $ \gr@GroupReg {dbContactId} -> do
        atomically $ writeTVar (groupRegStatus gr) GRSRemoved
        let groupRef = groupReference g
        sendMessage' cc dbContactId $ serviceName <> " is removed from the group " <> groupRef <> ".\n\nGroup is no longer listed in the directory."
        notifySuperUsers $ "The group " <> groupRef <> " is de-listed (directory service is removed)."

    deUserCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRUser -> IO ()
    deUserCommand ct ciId = \case
      DCHelp ->
        sendMessage cc ct $
          "You must be the owner to add the group to the directory:\n\
          \1. Invite " <> serviceName <> " bot to your group as *admin*.\n\
          \2. " <> serviceName <> " bot will create a public group link for the new members to join even when you are offline.\n\
          \3. You will then need to add this link to the group welcome message.\n\
          \4. Once the link is added, service admins will approve the group (it can take up to 24 hours), and everybody will be able to find it in directory.\n\n\
          \Start from inviting the bot to your group as admin - it will guide you through the process"
      DCSearchGroup s -> do
        sendChatCmd cc (APIListGroups userId Nothing $ Just $ T.unpack s) >>= \case
          CRGroupsList {groups} ->
            atomically (filterListedGroups st groups) >>= \case
              [] -> sendReply "No groups found"
              gs -> do
                sendReply $ "Found " <> show (length gs) <> " group(s)"
                void . forkIO $ forM_ gs $ \GroupInfo {groupProfile = p@GroupProfile {image = image_}} -> do
                  let text = groupInfoText p
                      msg = maybe (MCText text) (\image -> MCImage {text, image}) image_
                  sendComposedMessage cc ct Nothing msg
          _ -> sendReply "Unexpected error"
      DCConfirmDuplicateGroup _ugrId _gName -> pure ()
      DCListUserGroups -> pure ()
      DCDeleteGroup _ugrId _gName -> pure ()
      DCUnknownCommand -> sendReply "Unknown command"
      DCCommandError tag -> sendReply $ "Command error: " <> show tag
      where
        sendReply = sendComposedMessage cc ct (Just ciId) . textMsgContent

    deSuperUserCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRSuperUser -> IO ()
    deSuperUserCommand ct ciId cmd
      | superUser `elem` superUsers = case cmd of
        DCApproveGroup {groupId, localDisplayName = n, groupApprovalId} ->
          atomically (getGroupReg st groupId) >>= \case
            Nothing -> sendMessage cc ct $ "Group ID " <> show groupId <> " not found"
            Just GroupReg {dbContactId, groupRegStatus} -> do
              readTVarIO groupRegStatus >>= \case
                GRSPendingApproval gaId
                  | gaId == groupApprovalId -> do
                    getGroup cc groupId >>= \case
                      Just GroupInfo {localDisplayName = n'}
                        | n == n' -> do
                          atomically $ do
                            writeTVar groupRegStatus GRSActive
                            listGroup st groupId
                          sendReply "Group approved!"
                          sendMessage' cc dbContactId $ "The group ID " <> show groupId <> " (" <> T.unpack n <> ") is approved and listed in directory!\nPlease note: if you change the group profile it will be hidden from directory until it is re-approved."
                        | otherwise -> sendReply "Incorrect group name"
                      Nothing -> pure ()
                  | otherwise -> sendReply "Incorrect approval code"
                _ -> sendReply $ "Error: the group ID " <> show groupId <> " (" <> T.unpack n <> ") is not pending approval."
        DCRejectGroup _gaId _gName -> pure ()
        DCSuspendGroup _gId _gName -> pure ()
        DCResumeGroup _gId _gName -> pure ()
        DCListGroups -> pure ()
        DCCommandError tag -> sendReply $ "Command error: " <> show tag
      | otherwise = sendReply "You are not allowed to use this command"
      where
        superUser = KnownContact {contactId = contactId' ct, localDisplayName = localDisplayName' ct}
        sendReply = sendComposedMessage cc ct (Just ciId) . textMsgContent

badInvitation :: GroupMemberRole -> GroupMemberRole -> Maybe String
badInvitation contactRole serviceRole = case (contactRole, serviceRole) of
  (GROwner, GRAdmin) -> Nothing
  (_, GRAdmin) -> Just "You must have a group *owner* role to register the group"
  (GROwner, _) -> Just "You must grant directory service *admin* role to register the group"
  _ -> Just "You must have a group *owner* role and you must grant directory service *admin* role to register the group"

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
