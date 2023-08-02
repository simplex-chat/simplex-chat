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
import qualified Data.Set as S
import Data.Text (Text)
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

data DuplicateGroup
  = DGUnique -- display name or full name is unique
  | DGRegistered -- the group with the same names is registered, additional confirmation is required
  | DGListed -- the group with the same names is listed, the registration is not allowed

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
    withSuperUsers action = void . forkIO $ forM_ superUsers $ \KnownContact {contactId} -> action contactId
    notifySuperUsers s = withSuperUsers $ \contactId -> sendMessage' cc contactId s
    -- withContact ctId GroupInfo {localDisplayName} err action = do
    --   getContact cc ctId >>= \case
    --     Just ct -> action ct
    --     Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find contact ID " <> tshow ctId
    notifyOwner GroupReg {dbContactId} = sendMessage' cc dbContactId
    ctId `isOwner` GroupReg {dbContactId} = ctId == dbContactId
    withGroupReg GroupInfo {groupId, localDisplayName} err action = do
      atomically (getGroupReg st groupId) >>= \case
        Just gr -> action gr
        Nothing -> putStrLn $ T.unpack $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find group registration ID " <> tshow groupId
    setGroupInactive GroupReg {groupRegStatus, dbGroupId} grStatus = atomically $ do
      writeTVar groupRegStatus grStatus
      unlistGroup st dbGroupId

    groupInfoText GroupProfile {displayName = n, fullName = fn, description = d} =
      n <> (if n == fn || T.null fn then "" else " (" <> fn <> ")") <> maybe "" ("\nWelcome message:\n" <>) d
    groupReference GroupInfo {groupId, groupProfile = GroupProfile {displayName}} =
      "ID " <> show groupId <> " (" <> T.unpack displayName <> ")"

    getGroups :: Text -> IO (Maybe [GroupInfo])
    getGroups search =
      sendChatCmd cc (APIListGroups userId Nothing $ Just $ T.unpack search) >>= \case
        CRGroupsList {groups} -> pure $ Just groups
        _ -> pure Nothing

    getDuplicateGroup :: GroupInfo -> IO (Maybe DuplicateGroup)
    getDuplicateGroup GroupInfo {groupId, groupProfile = GroupProfile {displayName, fullName}} =
      getGroups fullName >>= mapM duplicateGroup
      where
        sameGroup GroupInfo {groupId = gId, groupProfile = GroupProfile {displayName = n, fullName = fn}} =
          gId /= groupId && n == displayName && fn == fullName
        duplicateGroup [] = pure DGUnique
        duplicateGroup groups = do
          let gs = filter sameGroup groups
          if null gs
            then pure DGUnique
            else do
              lgs <- readTVarIO $ listedGroups st
              let listed = any (\GroupInfo {groupId = gId} -> gId `S.member` lgs) gs
              pure $ if listed then DGListed else DGRegistered

    processInvitation :: Contact -> GroupInfo -> IO ()
    processInvitation ct g@GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = do
      atomically $ addGroupReg st ct g GRSProposed
      r <- sendChatCmd cc $ APIJoinGroup groupId
      sendMessage cc ct $ T.unpack $ case r of
        CRUserAcceptedGroupSent {} -> "Joining the group " <> displayName <> "…"
        _ -> "Error joining group " <> displayName <> ", please re-send the invitation!"

    deContactConnected :: Contact -> IO ()
    deContactConnected ct = do
      putStrLn $ T.unpack (localDisplayName' ct) <> " connected"
      sendMessage cc ct $
        "Welcome to " <> serviceName <> " service!\n\
        \Send a search string to find groups or */help* to learn how to add groups to directory.\n\n\
        \For example, send _privacy_ to find groups about privacy."

    deGroupInvitation :: Contact -> GroupInfo -> GroupMemberRole -> GroupMemberRole -> IO ()
    deGroupInvitation ct g@GroupInfo {groupId, groupProfile = GroupProfile {displayName, fullName}} fromMemberRole memberRole = do
      case badInvitation fromMemberRole memberRole of
        Just msg -> sendMessage cc ct msg
        Nothing -> getDuplicateGroup g >>= \case
          Just DGUnique -> processInvitation ct g
          Just DGRegistered -> askConfirmation
          Just DGListed -> sendMessage cc ct $ T.unpack $ "The group " <> displayName <> " (" <> fullName <> ") is already listed in the directory, please choose another name."
          Nothing -> sendMessage cc ct $ "Unexpected error, please notify the developers."
      where
        badInvitation contactRole serviceRole = case (contactRole, serviceRole) of
          (GROwner, GRAdmin) -> Nothing
          (_, GRAdmin) -> Just "You must have a group *owner* role to register the group"
          (GROwner, _) -> Just "You must grant directory service *admin* role to register the group"
          _ -> Just "You must have a group *owner* role and you must grant directory service *admin* role to register the group"
        askConfirmation = do
          atomically $ addGroupReg st ct g GRSPendingConfirmation
          sendMessage cc ct $ T.unpack $ "The group " <> displayName <> " (" <> fullName <> ") is already submitted to the directory.\nTo confirm the registration, please send:"
          sendMessage cc ct $ "/confirm " <> show groupId <> ":" <> T.unpack displayName

    deServiceJoinedGroup :: ContactId -> GroupInfo -> IO ()
    deServiceJoinedGroup ctId g =
      withGroupReg g "joined group" $ \gr ->
        when (ctId `isOwner` gr) $ do
          let GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = g
          notifyOwner gr $ T.unpack $ "Joined the group " <> displayName <> ", creating the link…"
          sendChatCmd cc (APICreateGroupLink groupId GRMember) >>= \case
            CRGroupLinkCreated {connReqContact} -> do
              setGroupInactive gr GRSPendingUpdate
              notifyOwner gr
                "Created the public link to join the group via this directory service that is always online.\n\n\
                \Please add it to the group welcome message.\n\
                \For example, add:"
              notifyOwner gr $ "Link to join the group " <> T.unpack displayName <> ": " <> B.unpack (strEncode connReqContact)
            CRChatCmdError _ (ChatError e) -> case e of
              CEGroupUserRole {} -> notifyOwner gr "Failed creating group link, as service is no longer an admin."
              CEGroupMemberUserRemoved -> notifyOwner gr "Failed creating group link, as service is removed from the group."
              CEGroupNotJoined _ -> notifyOwner gr $ unexpectedError "group not joined"
              CEGroupMemberNotActive -> notifyOwner gr $ unexpectedError "service membership is not active"
              _ -> notifyOwner gr $ unexpectedError "can't create group link"
            _ -> notifyOwner gr $ unexpectedError "can't create group link"

    deGroupUpdated :: ContactId -> GroupInfo -> GroupInfo -> IO ()
    deGroupUpdated ctId fromGroup toGroup =
      unless (sameProfile p p') $ do
        atomically $ unlistGroup st groupId
        withGroupReg toGroup "group updated" $ \gr -> do
          readTVarIO (groupRegStatus gr) >>= \case
            GRSPendingConfirmation -> pure ()
            GRSProposed -> pure ()
            GRSPendingUpdate -> groupProfileUpdate >>= \case
              GPNoServiceLink ->
                when (ctId `isOwner` gr) $ notifyOwner gr $ "The profile updated for " <> groupRef <> ", but the group link is not added to the welcome message."
              GPServiceLinkAdded
                | ctId `isOwner` gr -> groupLinkAdded gr
                | otherwise -> notifyOwner gr "The group link is added by another group member, your registration will not be processed.\n\nPlease update the group profile yourself."
              GPServiceLinkRemoved -> when (ctId `isOwner` gr) $ notifyOwner gr $ "The group link of " <> groupRef <> " is removed from the welcome message, please add it."
              GPHasServiceLink -> when (ctId `isOwner` gr) $ groupLinkAdded gr
              GPServiceLinkError -> do
                when (ctId `isOwner` gr) $ notifyOwner gr $ "Error: " <> serviceName <> " has no group link for " <> groupRef <> ". Please report the error to the developers."
                putStrLn $ "Error: no group link for " <> groupRef
            GRSPendingApproval n -> processProfileChange gr $ n + 1
            GRSActive -> processProfileChange gr 1
            GRSSuspended -> processProfileChange gr 1
            GRSRemoved -> pure ()
      where
        isInfix l d_ = l `T.isInfixOf` fromMaybe "" d_
        GroupInfo {groupId, groupProfile = p} = fromGroup
        GroupInfo {localDisplayName, groupProfile = p'@GroupProfile {displayName = displayName', image = image'}} = toGroup
        groupRef = groupReference toGroup
        sameProfile
          GroupProfile {displayName = n, fullName = fn, image = i, description = d}
          GroupProfile {displayName = n', fullName = fn', image = i', description = d'} =
            n == n' && fn == fn' && i == i' && d == d'
        groupLinkAdded gr = do
          notifyOwner gr $ "Thank you! The group link for " <> groupRef <> " is added to the welcome message.\nYou will be notified once the group is added to the directory - it may take up to 24 hours."
          let gaId = 1
          setGroupInactive gr $ GRSPendingApproval gaId
          sendForApproval gr gaId
        processProfileChange gr n' = groupProfileUpdate >>= \case
          GPNoServiceLink -> do
            setGroupInactive gr GRSPendingUpdate
            notifyOwner gr $ "The group profile is updated " <> groupRef <> ", but no link is added to the welcome message.\n\nThe group will remain hidden from the directory until the group link is added and the group is re-approved."
          GPServiceLinkRemoved -> do
            setGroupInactive gr GRSPendingUpdate
            notifyOwner gr $ "The group link for " <> groupRef <> " is removed from the welcome message.\n\nThe group is hidden from the directory until the group link is added and the group is re-approved."
            notifySuperUsers $ "The group link is removed from " <> groupRef <> ", de-listed."
          GPServiceLinkAdded -> do
            setGroupInactive gr $ GRSPendingApproval n'
            notifyOwner gr $ "The group link is added to " <> groupRef <> "!\nIt is hidden from the directory until approved."
            notifySuperUsers $ "The group link is added to " <> groupRef <> "."
            sendForApproval gr n'
          GPHasServiceLink -> do
            setGroupInactive gr $ GRSPendingApproval n'
            notifyOwner gr $ "The group " <> groupRef <> " is updated!\nIt is hidden from the directory until approved."
            notifySuperUsers $ "The group " <> groupRef <> " is updated."
            sendForApproval gr n'
          GPServiceLinkError -> putStrLn $ "Error: no group link for " <> groupRef <> " pending approval."
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
          withSuperUsers $ \cId -> do
            sendComposedMessage' cc cId Nothing msg
            sendMessage' cc cId $ "/approve " <> show dbGroupId <> ":" <> T.unpack displayName' <> " " <> show gaId

    deContactRoleChanged :: ContactId -> GroupInfo -> GroupMemberRole -> IO ()
    deContactRoleChanged ctId g role = undefined

    deServiceRoleChanged :: GroupInfo -> GroupMemberRole -> IO ()
    deServiceRoleChanged g role = undefined

    deContactRemovedFromGroup :: ContactId -> GroupInfo -> IO ()
    deContactRemovedFromGroup ctId g =
      withGroupReg g "contact removed" $ \gr -> do
        when (ctId `isOwner` gr) $ do
          setGroupInactive gr GRSRemoved
          let groupRef = groupReference g
          notifyOwner gr $ "You are removed from the group " <> groupRef <> ".\n\nGroup is no longer listed in the directory."
          notifySuperUsers $ "The group " <> groupRef <> " is de-listed (group owner is removed)."

    deContactLeftGroup :: ContactId -> GroupInfo -> IO ()
    deContactLeftGroup ctId g =
      withGroupReg g "contact left" $ \gr -> do
        when (ctId `isOwner` gr) $ do
          setGroupInactive gr GRSRemoved
          let groupRef = groupReference g
          notifyOwner gr $ "You left the group " <> groupRef <> ".\n\nGroup is no longer listed in the directory."
          notifySuperUsers $ "The group " <> groupRef <> " is de-listed (group owner left)."

    deServiceRemovedFromGroup :: GroupInfo -> IO ()
    deServiceRemovedFromGroup g =
      withGroupReg g "service removed" $ \gr -> do
        setGroupInactive gr GRSRemoved
        let groupRef = groupReference g
        notifyOwner gr $ serviceName <> " is removed from the group " <> groupRef <> ".\n\nGroup is no longer listed in the directory."
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
      DCSearchGroup s ->
        getGroups s >>= \case
          Just groups ->
            atomically (filterListedGroups st groups) >>= \case
              [] -> sendReply "No groups found"
              gs -> do
                sendReply $ "Found " <> show (length gs) <> " group(s)"
                void . forkIO $ forM_ gs $ \GroupInfo {groupProfile = p@GroupProfile {image = image_}} -> do
                  let text = groupInfoText p
                      msg = maybe (MCText text) (\image -> MCImage {text, image}) image_
                  sendComposedMessage cc ct Nothing msg
          Nothing -> sendReply "Unexpected error, please notify the developers."
      DCConfirmDuplicateGroup ugrId gName ->
        atomically (getGroupReg st ugrId) >>= \case
          Nothing -> sendReply $ "Group ID " <> show ugrId <> " not found"
          Just GroupReg {dbGroupId, groupRegStatus} -> do
            getGroup cc dbGroupId >>= \case
              Nothing -> sendReply $ "Group ID " <> show ugrId <> " not found"
              Just g@GroupInfo {groupProfile = GroupProfile {displayName, fullName}}
                | displayName == gName ->
                  readTVarIO groupRegStatus >>= \case
                    GRSPendingConfirmation -> do
                      getDuplicateGroup g >>= \case
                        Nothing -> sendMessage cc ct $ "Unexpected error, please notify the developers."
                        Just DGListed -> sendMessage cc ct $ T.unpack $ "The group " <> displayName <> " (" <> fullName <> ") is already listed in the directory, please choose another name."
                        _ -> processInvitation ct g
                    _ -> sendReply $ "Error: the group ID " <> show ugrId <> " (" <> T.unpack displayName <> ") is not pending confirmation."
                | otherwise -> sendReply $ "Group ID " <> show ugrId <> " has the display name " <> T.unpack displayName
      DCListUserGroups -> pure ()
      DCDeleteGroup _ugrId _gName -> pure ()
      DCUnknownCommand -> sendReply "Unknown command"
      DCCommandError tag -> sendReply $ "Command error: " <> show tag
      where
        sendReply = sendComposedMessage cc ct (Just ciId) . textMsgContent

    deSuperUserCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRSuperUser -> IO ()
    deSuperUserCommand ct ciId cmd
      | superUser `elem` superUsers = case cmd of
        DCApproveGroup {groupId, displayName = n, groupApprovalId} ->
          atomically (getGroupReg st groupId) >>= \case
            Nothing -> sendReply $ "Group ID " <> show groupId <> " not found"
            Just GroupReg {dbContactId, groupRegStatus} -> do
              readTVarIO groupRegStatus >>= \case
                GRSPendingApproval gaId
                  | gaId == groupApprovalId -> do
                    getGroup cc groupId >>= \case
                      Just g@GroupInfo {groupProfile = GroupProfile {displayName = n'}}
                        | n == n' ->
                          getDuplicateGroup g >>= \case
                            Nothing -> sendReply $ "Unexpected error, please notify the developers."
                            Just DGListed -> sendReply $ "The group " <> groupRef <> " is already listed in the directory."
                            _ -> do
                              atomically $ do
                                writeTVar groupRegStatus GRSActive
                                listGroup st groupId
                              sendReply "Group approved!"
                              sendMessage' cc dbContactId $ "The group " <> groupRef <> " is approved and listed in directory!\nPlease note: if you change the group profile it will be hidden from directory until it is re-approved."
                        | otherwise -> sendReply "Incorrect group name"
                      Nothing -> pure ()
                  | otherwise -> sendReply "Incorrect approval code"
                _ -> sendReply $ "Error: the group " <> groupRef <> " is not pending approval."
          where
            groupRef = "ID " <> show groupId <> " (" <> T.unpack n <> ")"
        DCRejectGroup _gaId _gName -> pure ()
        DCSuspendGroup _gId _gName -> pure ()
        DCResumeGroup _gId _gName -> pure ()
        DCListGroups -> pure ()
        DCCommandError tag -> sendReply $ "Command error: " <> show tag
      | otherwise = sendReply "You are not allowed to use this command"
      where
        superUser = KnownContact {contactId = contactId' ct, localDisplayName = localDisplayName' ct}
        sendReply = sendComposedMessage cc ct (Just ciId) . textMsgContent

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
