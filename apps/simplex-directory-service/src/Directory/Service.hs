{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Service
  ( welcomeGetOpts,
    directoryService,
    directoryServiceCLI,
    newServiceState,
    acceptMemberHook
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Directory.BlockedWords
import Directory.Captcha
import Directory.Events
import Directory.Options
import Directory.Search
import Directory.Store
import Simplex.Chat.Bot
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Markdown (FormattedText (..), Format (..), parseMaybeMarkdownList, viewName)
import Simplex.Chat.Messages
import Simplex.Chat.Options
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Store (GroupLink (..))
import Simplex.Chat.Store.Direct (getContact)
import Simplex.Chat.Store.Groups (getGroupInfo, getGroupLink, getGroupSummary, setGroupCustomData)
import Simplex.Chat.Store.Profiles (GroupLinkInfo (..), getGroupLinkInfo)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.View (serializeChatError, serializeChatResponse, simplexChatContact, viewContactName, viewGroupName)
import Simplex.Messaging.Agent.Protocol (AConnectionLink (..), ConnectionLink (..), CreatedConnLink (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Agent.Protocol (SConnectionMode (..), sameConnReqContact, sameShortLinkContact)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util (safeDecodeUtf8, tshow, ($>>=), (<$$>))
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)
import System.Process (readProcess)

data GroupProfileUpdate
  = GPNoServiceLink
  | GPServiceLinkAdded {linkNow :: Text}
  | GPServiceLinkRemoved
  | GPHasServiceLink {linkBefore :: Text, linkNow :: Text}
  | GPServiceLinkError

data DuplicateGroup
  = DGUnique -- display name or full name is unique
  | DGRegistered -- the group with the same names is registered, additional confirmation is required
  | DGReserved -- the group with the same names is listed, the registration is not allowed

data GroupRolesStatus
  = GRSOk
  | GRSServiceNotAdmin
  | GRSContactNotOwner
  | GRSBadRoles
  deriving (Eq)

data ServiceState = ServiceState
  { searchRequests :: TMap ContactId SearchRequest,
    blockedWordsCfg :: BlockedWordsConfig,
    pendingCaptchas :: TMap GroupMemberId PendingCaptcha
  }

data PendingCaptcha = PendingCaptcha
  { captchaText :: Text,
    sentAt :: UTCTime,
    attempts :: Int
  }

captchaLength :: Int
captchaLength = 7

maxCaptchaAttempts :: Int
maxCaptchaAttempts = 5

captchaTTL :: NominalDiffTime
captchaTTL = 600 -- 10 minutes

newServiceState :: DirectoryOpts -> IO ServiceState
newServiceState opts = do
  searchRequests <- TM.emptyIO
  blockedWordsCfg <- readBlockedWordsConfig opts
  pendingCaptchas <- TM.emptyIO
  pure ServiceState {searchRequests, blockedWordsCfg, pendingCaptchas}

welcomeGetOpts :: IO DirectoryOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@DirectoryOpts {coreOptions, testing, superUsers, adminUsers, ownersGroup} <- getDirectoryOpts appDir "simplex_directory_service"
  unless testing $ do
    putStrLn $ "SimpleX Directory Service Bot v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ knownContacts "superuser" superUsers
    putStrLn $ knownContacts "admin user" adminUsers
    putStrLn $ case ownersGroup of
      Nothing -> "No owner's group"
      Just KnownGroup {groupId, localDisplayName = n} -> "Owners' group: " <> knownName groupId n
  pure opts
  where
    knownContacts userType = \case
      [] -> "No " <> userType <> "s"
      cts -> show (length cts) <> " " <> userType <> "(s): " <> intercalate ", " (map knownContact cts)
    knownContact KnownContact {contactId, localDisplayName = n} = knownName contactId n
    knownName i n = show i <> ":" <> T.unpack (viewName n)

directoryServiceCLI :: DirectoryStore -> DirectoryOpts -> IO ()
directoryServiceCLI st opts = do
  env <- newServiceState opts
  eventQ <- newTQueueIO
  let eventHook cc resp = atomically $ resp <$ writeTQueue eventQ (cc, resp)
      chatHooks = defaultChatHooks {postStartHook = Just postStartHook, eventHook = Just eventHook, acceptMember = Just $ acceptMemberHook opts env}
  race_
    (simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing)
    (processEvents eventQ env)
  where
    processEvents eventQ env = forever $ do
      (cc, resp) <- atomically $ readTQueue eventQ
      u_ <- readTVarIO (currentUser cc)
      forM_ u_ $ \user -> directoryServiceEvent st opts env user cc resp
    postStartHook cc =
      readTVarIO (currentUser cc) >>= \case
        Nothing -> putStrLn "No current user" >> exitFailure
        Just User {userId, profile = p@LocalProfile {preferences}} -> do
          let cmds = fromMaybe [] $ preferences >>= commands_
          unless (cmds == directoryCommands) $ do
            let prefs = (fromMaybe emptyChatPrefs preferences) {files = Just FilesPreference {allow = FANo}, commands = Just directoryCommands} :: Preferences
                p' = (fromLocalProfile p) {displayName = serviceName opts, peerType = Just CPTBot, preferences = Just prefs} :: Profile
            liftIO $ sendChatCmd cc (APIUpdateProfile userId p') >>= \case
              Right CRUserProfileUpdated {} -> putStrLn "Updated directory commands"
              Right r -> putStrLn ("Error: unexpected response " <> show r) >> exitFailure
              Left e -> putStrLn ("Error: " <> show e) >> exitFailure

directoryCommands :: [ChatBotCommand]
directoryCommands =
  [ CBCCommand "new" "New groups" Nothing,
    CBCCommand "help" "How to submit your group" Nothing,
    CBCCommand "list" "Your own groups" Nothing,
    CBCMenu
      "Group settings"
      [ CBCCommand "role" "View new member role" idParam,
        CBCCommand "filter" "Anti-spam filter" idParam,
        CBCCommand "link" "View and upgrade group link" idParam,
        CBCCommand "delete" "Remove a group from directory" (Just "<ID>:'<NAME>'")
      ]
  ]
  where
    idParam = Just "<ID>"

directoryService :: DirectoryStore -> DirectoryOpts -> ServiceState -> User -> ChatController -> IO ()
directoryService st opts@DirectoryOpts {testing} env user cc = do
  initializeBotAddress' (not testing) cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    directoryServiceEvent st opts env user cc resp

acceptMemberHook :: DirectoryOpts -> ServiceState -> GroupInfo -> GroupLinkInfo -> Profile -> IO (Either GroupRejectionReason (GroupAcceptance, GroupMemberRole))
acceptMemberHook
  DirectoryOpts {profileNameLimit}
  ServiceState {blockedWordsCfg}
  g
  GroupLinkInfo {memberRole}
  Profile {displayName, image = img} = runExceptT $ do
    let a = groupMemberAcceptance g
    when (useMemberFilter img $ rejectNames a) checkName
    pure $
      if
        | useMemberFilter img (passCaptcha a) -> (GAPendingApproval, GRMember)
        | useMemberFilter img (makeObserver a) -> (GAAccepted, GRObserver)
        | otherwise -> (GAAccepted, memberRole)
    where
      checkName :: ExceptT GroupRejectionReason IO ()
      checkName
        | T.length displayName > profileNameLimit = throwError GRRLongName
        | otherwise = do
            when (hasBlockedFragments blockedWordsCfg displayName) $ throwError GRRBlockedName
            when (hasBlockedWords blockedWordsCfg displayName) $ throwError GRRBlockedName

groupMemberAcceptance :: GroupInfo -> DirectoryMemberAcceptance
groupMemberAcceptance GroupInfo {customData} = memberAcceptance $ fromCustomData customData

useMemberFilter :: Maybe ImageData -> Maybe ProfileCondition -> Bool
useMemberFilter img_ = \case
  Just PCAll -> True
  Just PCNoImage -> maybe True (\(ImageData i) -> i == "") img_
  Nothing -> False

readBlockedWordsConfig :: DirectoryOpts -> IO BlockedWordsConfig
readBlockedWordsConfig DirectoryOpts {blockedFragmentsFile, blockedWordsFile, nameSpellingFile, blockedExtensionRules, testing} = do
  extensionRules <- maybe (pure []) (fmap read . readFile) blockedExtensionRules
  spelling <- maybe (pure M.empty) (fmap (M.fromList . read) . readFile) nameSpellingFile
  blockedFragments <- S.fromList <$> maybe (pure []) (fmap T.lines . T.readFile) blockedFragmentsFile
  bws <- maybe (pure []) (fmap lines . readFile) blockedWordsFile
  let blockedWords = S.fromList $ concatMap (wordVariants extensionRules) bws
  unless testing $ putStrLn $ "Blocked fragments: " <> show (length blockedFragments) <> ", blocked words: " <> show (length blockedWords) <> ", spelling rules: " <> show (M.size spelling)
  pure BlockedWordsConfig {blockedFragments, blockedWords, extensionRules, spelling}

directoryServiceEvent :: DirectoryStore -> DirectoryOpts -> ServiceState -> User -> ChatController -> Either ChatError ChatEvent -> IO ()
directoryServiceEvent st opts@DirectoryOpts {adminUsers, superUsers, serviceName, ownersGroup, searchResults} env@ServiceState {searchRequests} user@User {userId} cc event =
  forM_ (crDirectoryEvent event) $ \case
    DEContactConnected ct -> deContactConnected ct
    DEGroupInvitation {contact = ct, groupInfo = g, fromMemberRole, memberRole} -> deGroupInvitation ct g fromMemberRole memberRole
    DEServiceJoinedGroup ctId g owner -> deServiceJoinedGroup ctId g owner
    DEGroupUpdated {member, fromGroup, toGroup} -> deGroupUpdated member fromGroup toGroup
    DEPendingMember g m -> dePendingMember g m
    DEPendingMemberMsg g m ciId t -> dePendingMemberMsg g m ciId t
    DEContactRoleChanged g ctId role -> deContactRoleChanged g ctId role
    DEServiceRoleChanged g role -> deServiceRoleChanged g role
    DEContactRemovedFromGroup ctId g -> deContactRemovedFromGroup ctId g
    DEContactLeftGroup ctId g -> deContactLeftGroup ctId g
    DEServiceRemovedFromGroup g -> deServiceRemovedFromGroup g
    DEGroupDeleted g -> deGroupDeleted g
    DEUnsupportedMessage _ct _ciId -> pure ()
    DEItemEditIgnored _ct -> pure ()
    DEItemDeleteIgnored _ct -> pure ()
    DEContactCommand ct ciId (ADC sUser cmd) -> do
      logInfo $ "command received " <> directoryCmdTag cmd
      case sUser of
        SDRUser -> deUserCommand ct ciId cmd
        SDRAdmin -> deAdminCommand ct ciId cmd
        SDRSuperUser -> deSuperUserCommand ct ciId cmd
    DELogChatResponse r -> logInfo r
  where
    groupLinkText (CCLink cReq sLnk_) = maybe (strEncodeTxt $ simplexChatContact cReq) strEncodeTxt sLnk_
    withAdminUsers action = void . forkIO $ do
      forM_ superUsers $ \KnownContact {contactId} -> action contactId
      forM_ adminUsers $ \KnownContact {contactId} -> action contactId
    withSuperUsers action = void . forkIO $ forM_ superUsers $ \KnownContact {contactId} -> action contactId
    notifyAdminUsers s = withAdminUsers $ \contactId -> sendMessage' cc contactId s
    notifyOwner GroupReg {dbContactId} = sendMessage' cc dbContactId
    ctId `isOwner` GroupReg {dbContactId} = ctId == dbContactId
    withGroupReg GroupInfo {groupId, localDisplayName} err action = do
      getGroupReg st groupId >>= \case
        Just gr -> action gr
        Nothing -> logError $ "Error: " <> err <> ", group: " <> localDisplayName <> ", can't find group registration ID " <> tshow groupId
    groupInfoText p@GroupProfile {description = d} = groupNameDescr p <> maybe "" ("\nWelcome message:\n" <>) d
    groupNameDescr GroupProfile {displayName = n, fullName = fn, shortDescr = sd_} =
      n <> maybe "" (\d' -> " (" <> d' <> ")") descr
      where
        descr
          | n == fn || T.null fn = if sd_ == Just "" then Nothing else sd_
          | otherwise = Just fn
    userGroupReference gr GroupInfo {groupProfile = GroupProfile {displayName}} = userGroupReference' gr displayName
    userGroupReference' GroupReg {userGroupRegId} displayName = groupReference' userGroupRegId displayName
    groupReference GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = groupReference' groupId displayName
    groupReference' groupId displayName = "ID " <> tshow groupId <> " (" <> displayName <> ")"
    groupAlreadyListed GroupInfo {groupProfile = p} =
      "The group " <> groupNameDescr p <> " is already listed in the directory, please choose another name."

    getGroups :: Text -> IO (Maybe [GroupInfoSummary])
    getGroups = getGroups_ . Just

    getGroups_ :: Maybe Text -> IO (Maybe [GroupInfoSummary])
    getGroups_ search_ =
      sendChatCmd cc (APIListGroups userId Nothing $ T.unpack <$> search_) >>= \case
        Right CRGroupsList {groups} -> pure $ Just groups
        _ -> pure Nothing

    getDuplicateGroup :: GroupInfo -> IO (Maybe DuplicateGroup)
    getDuplicateGroup GroupInfo {groupId, groupProfile = GroupProfile {displayName, fullName}} =
      getGroups fullName >>= mapM duplicateGroup
      where
        sameGroupNotRemoved (GIS g@GroupInfo {groupId = gId, groupProfile = GroupProfile {displayName = n, fullName = fn}} _) =
          gId /= groupId && n == displayName && fn == fullName && not (memberRemoved $ membership g)
        duplicateGroup [] = pure DGUnique
        duplicateGroup groups = do
          let gs = filter sameGroupNotRemoved groups
          if null gs
            then pure DGUnique
            else do
              (lgs, rgs) <- atomically $ (,) <$> readTVar (listedGroups st) <*> readTVar (reservedGroups st)
              let reserved = any (\(GIS GroupInfo {groupId = gId} _) -> gId `S.member` lgs || gId `S.member` rgs) gs
              if reserved
                then pure DGReserved
                else do
                  removed <- foldM (\r -> fmap (r &&) . isGroupRemoved) True gs
                  pure $ if removed then DGUnique else DGRegistered
        isGroupRemoved (GIS GroupInfo {groupId = gId} _) =
          getGroupReg st gId >>= \case
            Just GroupReg {groupRegStatus} -> groupRemoved <$> readTVarIO groupRegStatus
            Nothing -> pure True

    processInvitation :: Contact -> GroupInfo -> IO ()
    processInvitation ct g@GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = do
      void $ addGroupReg st ct g GRSProposed
      r <- sendChatCmd cc $ APIJoinGroup groupId MFNone
      sendMessage cc ct $ case r of
        Right  CRUserAcceptedGroupSent {} -> "Joining the group " <> displayName <> "…"
        _ -> "Error joining group " <> displayName <> ", please re-send the invitation!"

    deContactConnected :: Contact -> IO ()
    deContactConnected ct = when (contactDirect ct) $ do
      logInfo $ (viewContactName ct) <> " connected"
      sendMessage cc ct $
        ("Welcome to " <> serviceName <> "!\n\n")
          <> "🔍 Send search string to find groups - try _security_.\n\
             \/help - how to submit your group.\n\
             \/new - recent groups.\n\n\
             \[Directory rules](https://simplex.chat/docs/directory.html)."

    deGroupInvitation :: Contact -> GroupInfo -> GroupMemberRole -> GroupMemberRole -> IO ()
    deGroupInvitation ct g@GroupInfo {groupProfile = p@GroupProfile {displayName}} fromMemberRole memberRole = do
      logInfo $ "invited to group " <> viewGroupName g <> " by " <> viewContactName ct
      case badRolesMsg $ groupRolesStatus fromMemberRole memberRole of
        Just msg -> sendMessage cc ct msg
        Nothing ->
          getDuplicateGroup g >>= \case
            Just DGUnique -> processInvitation ct g
            Just DGRegistered -> askConfirmation
            Just DGReserved -> sendMessage cc ct $ groupAlreadyListed g
            Nothing -> sendMessage cc ct "Error: getDuplicateGroup. Please notify the developers."
      where
        askConfirmation = do
          ugrId <- addGroupReg st ct g GRSPendingConfirmation
          sendMessage cc ct $ "The group " <> groupNameDescr p <> " is already submitted to the directory.\nTo confirm the registration, please send:"
          sendMessage cc ct $ "/confirm " <> tshow ugrId <> ":" <> viewName displayName

    badRolesMsg :: GroupRolesStatus -> Maybe Text
    badRolesMsg = \case
      GRSOk -> Nothing
      GRSServiceNotAdmin -> Just "You must grant directory service *admin* role to register the group"
      GRSContactNotOwner -> Just "You must have a group *owner* role to register the group"
      GRSBadRoles -> Just "You must have a group *owner* role and you must grant directory service *admin* role to register the group"

    getGroupRolesStatus :: GroupInfo -> GroupReg -> IO (Maybe GroupRolesStatus)
    getGroupRolesStatus GroupInfo {membership = GroupMember {memberRole = serviceRole}} gr =
      rStatus <$$> getGroupMember gr
      where
        rStatus GroupMember {memberRole} = groupRolesStatus memberRole serviceRole

    groupRolesStatus :: GroupMemberRole -> GroupMemberRole -> GroupRolesStatus
    groupRolesStatus contactRole serviceRole = case (contactRole, serviceRole) of
      (GROwner, GRAdmin) -> GRSOk
      (_, GRAdmin) -> GRSContactNotOwner
      (GROwner, _) -> GRSServiceNotAdmin
      _ -> GRSBadRoles

    getGroupMember :: GroupReg -> IO (Maybe GroupMember)
    getGroupMember GroupReg {dbGroupId, dbOwnerMemberId} =
      readTVarIO dbOwnerMemberId
        $>>= \mId -> resp <$> sendChatCmd cc (APIGroupMemberInfo dbGroupId mId)
      where
        resp = \case
          Right CRGroupMemberInfo {member} -> Just member
          _ -> Nothing

    deServiceJoinedGroup :: ContactId -> GroupInfo -> GroupMember -> IO ()
    deServiceJoinedGroup ctId g owner = do
      logInfo $ "service joined group " <> viewGroupName g
      withGroupReg g "joined group" $ \gr ->
        when (ctId `isOwner` gr) $ do
          setGroupRegOwner st gr owner
          let GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = g
          notifyOwner gr $ "Joined the group " <> displayName <> ", creating the link…"
          sendChatCmd cc (APICreateGroupLink groupId GRMember) >>= \case
            Right CRGroupLinkCreated {groupLink = GroupLink {connLinkContact = gLink}} -> do
              setGroupStatus st gr GRSPendingUpdate
              notifyOwner
                gr
                "Created the public link to join the group via this directory service that is always online.\n\n\
                \Please add it to the group welcome message.\n\
                \For example, add:"
              notifyOwner gr $ "Link to join the group " <> displayName <> ": " <> groupLinkText gLink
            Left (ChatError e) -> case e of
              CEGroupUserRole {} -> notifyOwner gr "Failed creating group link, as service is no longer an admin."
              CEGroupMemberUserRemoved -> notifyOwner gr "Failed creating group link, as service is removed from the group."
              CEGroupNotJoined _ -> notifyOwner gr $ unexpectedError "group not joined"
              CEGroupMemberNotActive -> notifyOwner gr $ unexpectedError "service membership is not active"
              _ -> notifyOwner gr $ unexpectedError "can't create group link"
            _ -> notifyOwner gr $ unexpectedError "can't create group link"

    deGroupUpdated :: GroupMember -> GroupInfo -> GroupInfo -> IO ()
    deGroupUpdated m@GroupMember {memberProfile = LocalProfile {displayName = mName}} fromGroup toGroup = do
      logInfo $ "group updated " <> viewGroupName toGroup
      unless (sameProfile p p') $ do
        withGroupReg toGroup "group updated" $ \gr -> do
          let userGroupRef = userGroupReference gr toGroup
              byMember = case memberContactId m of
                Just ctId | ctId `isOwner` gr -> "" -- group registration owner, not any group owner.
                _ -> " by " <> mName -- owner notification from directory will include the name.
          readTVarIO (groupRegStatus gr) >>= \case
            GRSPendingConfirmation -> pure ()
            GRSProposed -> pure ()
            GRSPendingUpdate ->
              groupProfileUpdate >>= \case
                GPNoServiceLink ->
                  notifyOwner gr $ "The profile updated for " <> userGroupRef <> byMember <> ", but the group link is not added to the welcome message."
                GPServiceLinkAdded _ -> groupLinkAdded gr byMember
                GPServiceLinkRemoved ->
                  notifyOwner gr $
                    "The group link of " <> userGroupRef <> " is removed from the welcome message" <> byMember <> ", please add it."
                GPHasServiceLink {} -> groupLinkAdded gr byMember
                GPServiceLinkError -> do
                  notifyOwner gr $
                    ("Error: " <> serviceName <> " has no group link for " <> userGroupRef)
                      <> " after profile was updated" <> byMember <> ". Please report the error to the developers."
                  logError $ "Error: no group link for " <> userGroupRef
            GRSPendingApproval n -> processProfileChange gr byMember False $ n + 1
            GRSActive -> processProfileChange gr byMember True 1
            GRSSuspended -> processProfileChange gr byMember False 1
            GRSSuspendedBadRoles -> processProfileChange gr byMember False 1
            GRSRemoved -> pure ()
      where
        GroupInfo {groupId, groupProfile = p} = fromGroup
        GroupInfo {groupProfile = p'} = toGroup
        sameProfile
          GroupProfile {displayName = n, fullName = fn, shortDescr = sd, image = i, description = d}
          GroupProfile {displayName = n', fullName = fn', shortDescr = sd', image = i', description = d'} =
            n == n' && fn == fn' && i == i' && sd == sd' && (T.words <$> d) == (T.words <$> d')
        groupLinkAdded gr byMember = do
          getDuplicateGroup toGroup >>= \case
            Nothing -> notifyOwner gr "Error: getDuplicateGroup. Please notify the developers."
            Just DGReserved -> notifyOwner gr $ groupAlreadyListed toGroup
            _ -> do
              let gaId = 1
              setGroupStatus st gr $ GRSPendingApproval gaId
              notifyOwner gr $
                ("Thank you! The group link for " <> userGroupReference gr toGroup <> " is added to the welcome message" <> byMember)
                  <> ".\nYou will be notified once the group is added to the directory - it may take up to 48 hours."
              checkRolesSendToApprove gr gaId
        processProfileChange gr byMember isActive n' = do
          let userGroupRef = userGroupReference gr toGroup
              groupRef = groupReference toGroup
          groupProfileUpdate >>= \case
            GPNoServiceLink -> do
              setGroupStatus st gr GRSPendingUpdate
              notifyOwner gr $
                ("The group profile is updated for " <> userGroupRef <> byMember <> ", but no link is added to the welcome message.\n\n")
                  <> "The group will remain hidden from the directory until the group link is added and the group is re-approved."
            GPServiceLinkRemoved -> do
              setGroupStatus st gr GRSPendingUpdate
              notifyOwner gr $
                ("The group link for " <> userGroupRef <> " is removed from the welcome message" <> byMember)
                  <> ".\n\nThe group is hidden from the directory until the group link is added and the group is re-approved."
              notifyAdminUsers $ "The group link is removed from " <> groupRef <> ", de-listed."
            GPServiceLinkAdded _ -> do
              setGroupStatus st gr $ GRSPendingApproval n'
              notifyOwner gr $
                ("The group link is added to " <> userGroupRef <> byMember)
                  <> "!\nIt is hidden from the directory until approved."
              notifyAdminUsers $ "The group link is added to " <> groupRef <> byMember <> "."
              checkRolesSendToApprove gr n'
            GPHasServiceLink {linkBefore, linkNow}
              | isActive && onlyLinkChanged p p' -> do
                  notifyOwner gr $
                    ("The group " <> userGroupRef <> " is updated" <> byMember)
                      <> "!\nThe group is listed in directory."
                  notifyAdminUsers $ "The group " <> groupRef <> " is updated" <> byMember <> " - only link or whitespace changes.\nThe group remained listed in directory."
              | otherwise -> do
                  setGroupStatus st gr $ GRSPendingApproval n'
                  notifyOwner gr $
                    ("The group " <> userGroupRef <> " is updated" <> byMember)
                      <> "!\nIt is hidden from the directory until approved."
                  notifyAdminUsers $ "The group " <> groupRef <> " is updated" <> byMember <> "."
                  checkRolesSendToApprove gr n'
              where
                onlyLinkChanged
                  GroupProfile {displayName = dn, fullName = fn, shortDescr = sd, image = i, description = d}
                  GroupProfile {displayName = dn', fullName = fn', shortDescr = sd', image = i', description = d'} =
                    dn == dn' && fn == fn' && i == i' && sd == sd' && (T.words . T.replace linkBefore "" <$> d) == (T.words . T.replace linkNow "" <$> d')
            GPServiceLinkError -> logError $ "Error: no group link for " <> groupRef <> " pending approval."
        groupProfileUpdate = profileUpdate <$> sendChatCmd cc (APIGetGroupLink groupId)
          where
            profileUpdate = \case
              Right CRGroupLink {groupLink = GroupLink {connLinkContact = CCLink cr sl_}} ->
                let linkBefore_ = profileGroupLinkText fromGroup
                    linkNow_ = profileGroupLinkText toGroup
                    profileGroupLinkText GroupInfo {groupProfile = gp} =
                      maybe Nothing (fmap (\(FormattedText _ t) -> t) . find ftHasLink) $ parseMaybeMarkdownList =<< description gp
                    ftHasLink = \case
                      FormattedText (Just SimplexLink {simplexUri = ACL SCMContact cLink}) _ -> case cLink of
                        CLFull cr' -> sameConnReqContact cr' cr
                        CLShort sl' -> maybe False (sameShortLinkContact sl') sl_
                      _ -> False
                 in case (linkBefore_, linkNow_) of
                      (Just linkBefore, Just linkNow) -> GPHasServiceLink linkBefore linkNow
                      (Just _, Nothing) -> GPServiceLinkRemoved
                      (Nothing, Just linkNow) -> GPServiceLinkAdded linkNow
                      (Nothing, Nothing) -> GPNoServiceLink
              _ -> GPServiceLinkError
        checkRolesSendToApprove gr gaId = do
          (badRolesMsg <$$> getGroupRolesStatus toGroup gr) >>= \case
            Nothing -> notifyOwner gr "Error: getGroupRolesStatus. Please notify the developers."
            Just (Just msg) -> notifyOwner gr msg
            Just Nothing -> sendToApprove toGroup gr gaId

    dePendingMember :: GroupInfo -> GroupMember -> IO ()
    dePendingMember g@GroupInfo {groupProfile = GroupProfile {displayName}}  m
      | memberRequiresCaptcha a m = sendMemberCaptcha g m Nothing captchaNotice 0
      | otherwise = approvePendingMember a g m
      where
        a = groupMemberAcceptance g
        captchaNotice = "Captcha is generated by SimpleX Directory service.\n\n*Send captcha text* to join the group " <> displayName <> "."

    sendMemberCaptcha :: GroupInfo -> GroupMember -> Maybe ChatItemId -> Text -> Int -> IO ()
    sendMemberCaptcha GroupInfo {groupId} m quotedId noticeText prevAttempts = do
      s <- getCaptchaStr captchaLength ""
      mc <- getCaptcha s
      sentAt <- getCurrentTime
      let captcha = PendingCaptcha {captchaText = T.pack s, sentAt, attempts = prevAttempts + 1}
      atomically $ TM.insert gmId captcha $ pendingCaptchas env
      sendCaptcha mc
      where
        getCaptcha s = case captchaGenerator opts of
          Nothing -> pure textMsg
          Just script -> content <$> readProcess script [s] ""
          where
            textMsg = MCText $ T.pack s
            content r = case T.lines $ T.pack r of
              [] -> textMsg
              "" : _ -> textMsg
              img : _ -> MCImage "" $ ImageData img
        sendCaptcha mc = sendComposedMessages_ cc (SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)) [(quotedId, MCText noticeText), (Nothing, mc)]
        gmId = groupMemberId' m

    approvePendingMember :: DirectoryMemberAcceptance -> GroupInfo -> GroupMember -> IO ()
    approvePendingMember a g@GroupInfo {groupId} m@GroupMember {memberProfile = LocalProfile {displayName, image}} = do
      gli_ <- join <$> withDB' "getGroupLinkInfo" cc (\db -> getGroupLinkInfo db userId groupId)
      let role = if useMemberFilter image (makeObserver a) then GRObserver else maybe GRMember (\GroupLinkInfo {memberRole} -> memberRole) gli_
          gmId = groupMemberId' m
      sendChatCmd cc (APIAcceptMember groupId gmId role) >>= \case
        Right CRMemberAccepted {member} -> do
          atomically $ TM.delete gmId $ pendingCaptchas env
          if memberStatus member == GSMemPendingReview
            then logInfo $ "Member " <> viewName displayName <> " accepted and pending review, group " <> tshow groupId <> ":" <> viewGroupName g
            else logInfo $ "Member " <> viewName displayName <> " accepted, group " <> tshow groupId <> ":" <> viewGroupName g
        r -> logError $ "unexpected accept member response: " <> tshow r

    dePendingMemberMsg :: GroupInfo -> GroupMember -> ChatItemId -> Text -> IO ()
    dePendingMemberMsg g@GroupInfo {groupId, groupProfile = GroupProfile {displayName = n}} m@GroupMember {memberProfile = LocalProfile {displayName}} ciId msgText
      | memberRequiresCaptcha a m = do
          ts <- getCurrentTime
          atomically (TM.lookup (groupMemberId' m) $ pendingCaptchas env) >>= \case
            Just PendingCaptcha {captchaText, sentAt, attempts}
              | ts `diffUTCTime` sentAt > captchaTTL -> sendMemberCaptcha g m (Just ciId) captchaExpired $ attempts - 1
              | matchCaptchaStr captchaText msgText -> do
                  sendComposedMessages_ cc (SRGroup groupId $ Just $ GCSMemberSupport (Just $ groupMemberId' m)) [(Just ciId, MCText $ "Correct, you joined the group " <> n)]
                  approvePendingMember a g m
              | attempts >= maxCaptchaAttempts -> rejectPendingMember tooManyAttempts
              | otherwise -> sendMemberCaptcha g m (Just ciId) (wrongCaptcha attempts) attempts
            Nothing -> sendMemberCaptcha g m (Just ciId) noCaptcha 0
      | otherwise = approvePendingMember a g m
      where
        a = groupMemberAcceptance g
        rejectPendingMember rjctNotice = do
          let gmId = groupMemberId' m
          sendComposedMessages cc (SRGroup groupId $ Just $ GCSMemberSupport (Just gmId)) [MCText rjctNotice]
          sendChatCmd cc (APIRemoveMembers groupId [gmId] False) >>= \case
            Right (CRUserDeletedMembers _ _ (_ : _) _) -> do
              atomically $ TM.delete gmId $ pendingCaptchas env
              logInfo $ "Member " <> viewName displayName <> " rejected, group " <> tshow groupId <> ":" <> viewGroupName g
            r -> logError $ "unexpected remove member response: " <> tshow r
        captchaExpired = "Captcha expired, please try again."
        wrongCaptcha attempts
          | attempts == maxCaptchaAttempts - 1 = "Incorrect text, please try again - this is your last attempt."
          | otherwise = "Incorrect text, please try again."
        noCaptcha = "Unexpected message, please try again."
        tooManyAttempts = "Too many failed attempts, you can't join group."

    memberRequiresCaptcha :: DirectoryMemberAcceptance -> GroupMember -> Bool
    memberRequiresCaptcha a GroupMember {memberProfile = LocalProfile {image}} =
      useMemberFilter image $ passCaptcha a

    sendToApprove :: GroupInfo -> GroupReg -> GroupApprovalId -> IO ()
    sendToApprove GroupInfo {groupProfile = p@GroupProfile {displayName, image = image'}} GroupReg {dbGroupId, dbContactId} gaId = do
      ct_ <- getContact' cc user dbContactId
      gr_ <- getGroupAndSummary cc user dbGroupId
      let membersStr = maybe "" (\(_, s) -> "_" <> tshow (currentMembers s) <> " members_\n") gr_
          text =
            maybe ("The group ID " <> tshow dbGroupId <> " submitted: ") (\c -> localDisplayName' c <> " submitted the group ID " <> tshow dbGroupId <> ": ") ct_
              <> ("\n" <> groupInfoText p <> "\n" <> membersStr <> "\nTo approve send:")
          msg = maybe (MCText text) (\image -> MCImage {text, image}) image'
      withAdminUsers $ \cId -> do
        sendComposedMessage' cc cId Nothing msg
        sendMessage' cc cId $ "/approve " <> tshow dbGroupId <> ":" <> viewName displayName <> " " <> tshow gaId

    deContactRoleChanged :: GroupInfo -> ContactId -> GroupMemberRole -> IO ()
    deContactRoleChanged g@GroupInfo {membership = GroupMember {memberRole = serviceRole}} ctId contactRole = do
      logInfo $ "contact ID " <> tshow ctId <> " role changed in group " <> viewGroupName g <> " to " <> tshow contactRole
      withGroupReg g "contact role changed" $ \gr -> do
        let userGroupRef = userGroupReference gr g
            uCtRole = "Your role in the group " <> userGroupRef <> " is changed to " <> ctRole
        when (ctId `isOwner` gr) $ do
          readTVarIO (groupRegStatus gr) >>= \case
            GRSSuspendedBadRoles -> when (rStatus == GRSOk) $ do
              setGroupStatus st gr GRSActive
              notifyOwner gr $ uCtRole <> ".\n\nThe group is listed in the directory again."
              notifyAdminUsers $ "The group " <> groupRef <> " is listed " <> suCtRole
            GRSPendingApproval gaId -> when (rStatus == GRSOk) $ do
              sendToApprove g gr gaId
              notifyOwner gr $ uCtRole <> ".\n\nThe group is submitted for approval."
            GRSActive -> when (rStatus /= GRSOk) $ do
              setGroupStatus st gr GRSSuspendedBadRoles
              notifyOwner gr $ uCtRole <> ".\n\nThe group is no longer listed in the directory."
              notifyAdminUsers $ "The group " <> groupRef <> " is de-listed " <> suCtRole
            _ -> pure ()
      where
        rStatus = groupRolesStatus contactRole serviceRole
        groupRef = groupReference g
        ctRole = "*" <> strEncodeTxt contactRole <> "*"
        suCtRole = "(user role is set to " <> ctRole <> ")."

    deServiceRoleChanged :: GroupInfo -> GroupMemberRole -> IO ()
    deServiceRoleChanged g serviceRole = do
      logInfo $ "service role changed in group " <> viewGroupName g <> " to " <> tshow serviceRole
      withGroupReg g "service role changed" $ \gr -> do
        let userGroupRef = userGroupReference gr g
            uSrvRole = serviceName <> " role in the group " <> userGroupRef <> " is changed to " <> srvRole
        readTVarIO (groupRegStatus gr) >>= \case
          GRSSuspendedBadRoles -> when (serviceRole == GRAdmin) $
            whenContactIsOwner gr $ do
              setGroupStatus st gr GRSActive
              notifyOwner gr $ uSrvRole <> ".\n\nThe group is listed in the directory again."
              notifyAdminUsers $ "The group " <> groupRef <> " is listed " <> suSrvRole
          GRSPendingApproval gaId -> when (serviceRole == GRAdmin) $
            whenContactIsOwner gr $ do
              sendToApprove g gr gaId
              notifyOwner gr $ uSrvRole <> ".\n\nThe group is submitted for approval."
          GRSActive -> when (serviceRole /= GRAdmin) $ do
            setGroupStatus st gr GRSSuspendedBadRoles
            notifyOwner gr $ uSrvRole <> ".\n\nThe group is no longer listed in the directory."
            notifyAdminUsers $ "The group " <> groupRef <> " is de-listed " <> suSrvRole
          _ -> pure ()
      where
        groupRef = groupReference g
        srvRole = "*" <> strEncodeTxt serviceRole <> "*"
        suSrvRole = "(" <> serviceName <> " role is changed to " <> srvRole <> ")."
        whenContactIsOwner gr action =
          getGroupMember gr
            >>= mapM_ (\cm@GroupMember {memberRole} -> when (memberRole == GROwner && memberActive cm) action)

    deContactRemovedFromGroup :: ContactId -> GroupInfo -> IO ()
    deContactRemovedFromGroup ctId g = do
      logInfo $ "contact ID " <> tshow ctId <> " removed from group " <> viewGroupName g
      withGroupReg g "contact removed" $ \gr -> do
        when (ctId `isOwner` gr) $ do
          setGroupStatus st gr GRSRemoved
          notifyOwner gr $ "You are removed from the group " <> userGroupReference gr g <> ".\n\nThe group is no longer listed in the directory."
          notifyAdminUsers $ "The group " <> groupReference g <> " is de-listed (group owner is removed)."

    deContactLeftGroup :: ContactId -> GroupInfo -> IO ()
    deContactLeftGroup ctId g = do
      logInfo $ "contact ID " <> tshow ctId <> " left group " <> viewGroupName g
      withGroupReg g "contact left" $ \gr -> do
        when (ctId `isOwner` gr) $ do
          setGroupStatus st gr GRSRemoved
          notifyOwner gr $ "You left the group " <> userGroupReference gr g <> ".\n\nThe group is no longer listed in the directory."
          notifyAdminUsers $ "The group " <> groupReference g <> " is de-listed (group owner left)."

    deServiceRemovedFromGroup :: GroupInfo -> IO ()
    deServiceRemovedFromGroup g = do
      logInfo $ "service removed from group " <> viewGroupName g
      withGroupReg g "service removed" $ \gr -> do
        setGroupStatus st gr GRSRemoved
        notifyOwner gr $ serviceName <> " is removed from the group " <> userGroupReference gr g <> ".\n\nThe group is no longer listed in the directory."
        notifyAdminUsers $ "The group " <> groupReference g <> " is de-listed (directory service is removed)."

    deGroupDeleted :: GroupInfo -> IO ()
    deGroupDeleted g = do
      logInfo $ "group removed " <> viewGroupName g
      withGroupReg g "group removed" $ \gr -> do
        setGroupStatus st gr GRSRemoved
        notifyOwner gr $ "The group " <> userGroupReference gr g <> " is deleted.\n\nThe group is no longer listed in the directory."
        notifyAdminUsers $ "The group " <> groupReference g <> " is de-listed (group is deleted)."

    deUserCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRUser -> IO ()
    deUserCommand ct ciId = \case
      DCHelp DHSRegistration ->
        sendMessage cc ct $
          "You must be the group owner to add it to the directory:\n\n\
          \1️⃣ *Invite* "
            <> serviceName
            <> " bot to your group as *admin* - it will create a link for new members to join.\n\
               \2️⃣ *Add* this link to the group's welcome message.\n\
               \3️⃣ We *review* your group. Once *approved*, anybody can find it.\n\n\
               \_We usually approve within a day, except holidays_. [More details](https://simplex.chat/docs/directory.html#adding-groups-to-the-directory)."
      DCHelp DHSCommands ->
        sendMessage cc ct $
          "/'help commands' - receive this help message.\n\
          \/help - how to register your group to be added to directory.\n\
          \/list - list the groups you registered.\n\
          \`/role <ID>` - view and set default member role for your group.\n\
          \`/filter <ID>` - view and set spam filter settings for group.\n\
          \`/link <ID>` - view and upgrade group link.\n\
          \`/delete <ID>:<NAME>` - remove the group you submitted from directory, with _ID_ and _name_ as shown by /list command.\n\n\
          \To search for groups, send the search text."
      DCSearchGroup s -> withFoundListedGroups (Just s) $ sendSearchResults s
      DCSearchNext ->
        atomically (TM.lookup (contactId' ct) searchRequests) >>= \case
          Just search@SearchRequest {searchType, searchTime} -> do
            currentTime <- getCurrentTime
            if diffUTCTime currentTime searchTime > 300 -- 5 minutes
              then do
                atomically $ TM.delete (contactId' ct) searchRequests
                showAllGroups
              else case searchType of
                STSearch s -> withFoundListedGroups (Just s) $ sendNextSearchResults takeTop search
                STAll -> withFoundListedGroups Nothing $ sendNextSearchResults takeTop search
                STRecent -> withFoundListedGroups Nothing $ sendNextSearchResults takeRecent search
          Nothing -> showAllGroups
        where
          showAllGroups = deUserCommand ct ciId DCAllGroups
      DCAllGroups -> withFoundListedGroups Nothing $ sendAllGroups takeTop "top" STAll
      DCRecentGroups -> withFoundListedGroups Nothing $ sendAllGroups takeRecent "the most recent" STRecent
      DCSubmitGroup _link -> pure ()
      DCConfirmDuplicateGroup ugrId gName ->
        withUserGroupReg ugrId gName $ \g@GroupInfo {groupProfile = GroupProfile {displayName}} gr ->
          readTVarIO (groupRegStatus gr) >>= \case
            GRSPendingConfirmation ->
              getDuplicateGroup g >>= \case
                Nothing -> sendMessage cc ct "Error: getDuplicateGroup. Please notify the developers."
                Just DGReserved -> sendMessage cc ct $ groupAlreadyListed g
                _ -> processInvitation ct g
            _ -> sendReply $ "Error: the group ID " <> tshow ugrId <> " (" <> displayName <> ") is not pending confirmation."
      DCListUserGroups ->
        getUserGroupRegs st (contactId' ct) >>= \grs -> do
          sendReply $ tshow (length grs) <> " registered group(s)"
          void . forkIO $ forM_ (reverse grs) $ \gr@GroupReg {dbGroupId, userGroupRegId} ->
            let useGroupId = if isAdmin then dbGroupId else userGroupRegId
             in sendGroupInfo ct gr useGroupId Nothing
      DCDeleteGroup gId gName ->
        (if isAdmin then withGroupAndReg sendReply else withUserGroupReg) gId gName $ \GroupInfo {groupProfile = GroupProfile {displayName}} gr -> do
          delGroupReg st gr
          sendReply $ (if isAdmin then "The group " else "Your group ") <> displayName <> " is deleted from the directory"
      DCMemberRole gId gName_ mRole_ ->
        (if isAdmin then withGroupAndReg_ sendReply else withUserGroupReg_) gId gName_ $ \g _gr -> do
          let GroupInfo {groupProfile = GroupProfile {displayName = n}} = g
          case mRole_ of
            Nothing ->
              getGroupLink' cc user g >>= \case
                Just GroupLink {connLinkContact = gLink, acceptMemberRole} -> do
                  let anotherRole = case acceptMemberRole of GRObserver -> GRMember; _ -> GRObserver
                  sendReply $
                    initialRole n acceptMemberRole
                      <> ("Send /'role " <> tshow gId <> " " <> strEncodeTxt anotherRole <> "' to change it.\n\n")
                      <> onlyViaLink gLink
                Nothing -> sendReply $ "Error: failed reading the initial member role for the group " <> n
            Just mRole -> do
              setGroupLinkRole cc g mRole >>= \case
                Just gLink -> sendReply $ initialRole n mRole <> "\n" <> onlyViaLink gLink
                Nothing -> sendReply $ "Error: the initial member role for the group " <> n <> " was NOT upgated."
        where
          initialRole n mRole = "The initial member role for the group " <> n <> " is set to *" <> strEncodeTxt mRole <> "*\n"
          onlyViaLink gLink = "*Please note*: it applies only to members joining via this link: " <> groupLinkText gLink
      DCGroupFilter gId gName_ acceptance_ ->
        (if isAdmin then withGroupAndReg_ sendReply else withUserGroupReg_) gId gName_ $ \g _gr -> do
          let GroupInfo {groupProfile = GroupProfile {displayName = n}} = g
              a = groupMemberAcceptance g
          case acceptance_ of
            Just a' | a /= a' -> do
              let d = toCustomData $ DirectoryGroupData a'
              withDB' "setGroupCustomData" cc (\db -> setGroupCustomData db user g $ Just d) >>= \case
                Just () -> sendSettigns n a' " set to"
                Nothing -> sendReply $ "Error changing spam filter settings for group " <> n
            _ -> sendSettigns n a ""
        where
          sendSettigns n a setTo =
            sendReply $
              T.unlines $
                [ "Spam filter settings for group " <> n <> setTo <> ":",
                  "- reject long/inappropriate names: " <> showCondition (rejectNames a),
                  "- pass captcha to join: " <> showCondition (passCaptcha a),
                  -- "- make observer: " <> showCondition (makeObserver a) <> (if isJust (makeObserver a) then "" else " (use default set with /role command)"),
                  ""
                  -- "Use */filter " <> tshow gId <> " <level>* to change spam filter level: no (disable), basic, moderate, strong.",
                  -- "Or use */filter " <> tshow gId <> " [name[=noimage]] [captcha[=noimage]] [observer[=noimage]]* for advanced filter configuration."
                ]
                  <> ["/'filter " <> tshow gId <> " name' - enable name filter" | isNothing (rejectNames a)]
                  <> ["/'filter " <> tshow gId <> " captcha' - enable captcha challenge" | isNothing (passCaptcha a)]
                  <> ["/'filter " <> tshow gId <> " name captcha' - enable both" | isNothing (rejectNames a) || isNothing (passCaptcha a)]
                  <> ["/'filter " <> tshow gId <> " off' - disable filter" | isJust (rejectNames a) || isJust (passCaptcha a)]
          showCondition = \case
            Nothing -> "_disabled_"
            Just PCAll -> "_enabled_"
            Just PCNoImage -> "_enabled for profiles without image_"
      DCShowUpgradeGroupLink gId gName_ ->
        (if isAdmin then withGroupAndReg_ sendReply else withUserGroupReg_) gId gName_ $ \GroupInfo {groupId, localDisplayName = gName} _ -> do
          let groupRef = groupReference' gId gName
          withGroupLinkResult groupRef (sendChatCmd cc $ APIGetGroupLink groupId) $
            \GroupLink {connLinkContact = gLink@(CCLink _ sLnk_), acceptMemberRole, shortLinkDataSet, shortLinkLargeDataSet = BoolDef slLargeDataSet} -> do
              let shouldBeUpgraded = isNothing sLnk_ || not shortLinkDataSet || not slLargeDataSet
              sendReply $ T.unlines $
                [ "The link to join the group " <> groupRef <> ":",
                  groupLinkText gLink,
                  "New member role: " <> strEncodeTxt acceptMemberRole
                ]
                  <> ["The link is being upgraded..." | shouldBeUpgraded]
              when shouldBeUpgraded $ do
                let send = sendComposedMessage cc ct Nothing . MCText . T.unlines
                withGroupLinkResult groupRef (sendChatCmd cc $ APIAddGroupShortLink groupId) $
                  \GroupLink {connLinkContact = CCLink _ sLnk_'} -> case (sLnk_, sLnk_') of
                    (Just _, Just _) ->
                      send ["The group link is upgraded for: " <> groupRef, "No changes to group needed."]
                    (Nothing, Just sLnk) ->
                      sendComposedMessages cc (SRDirect $ contactId' ct)
                        [ MCText $ T.unlines
                            [ "Please replace the old link in welcome message of your group " <> groupRef,
                              "If this is the only change, the group will remain listed in directory without re-approval.",
                              "",
                              "The new link:"
                            ],
                          MCText $ strEncodeTxt sLnk
                        ]
                    (_, Nothing) ->
                      send ["The short link is not created for " <> groupRef, "Please report it to the developers."]
        where
          withGroupLinkResult groupRef a cb =
            a >>= \case
              Right CRGroupLink {groupLink} -> cb groupLink
              Left (ChatErrorStore (SEGroupLinkNotFound _)) ->
                sendReply $ "The group " <> groupRef <> " has no public link."
              Right r -> do
                ts <- getCurrentTime
                tz <- getCurrentTimeZone
                let resp = T.pack $ serializeChatResponse (Nothing, Just user) (config cc) ts tz Nothing r
                sendReply $ "Unexpected error:\n" <> resp
              Left e -> do
                let resp = T.pack $ serializeChatError True (config cc) e
                sendReply $ "Unexpected error:\n" <> resp
      DCUnknownCommand -> sendReply "Unknown command"
      DCCommandError tag -> sendReply $ "Command error: " <> tshow tag
      where
        knownCt = knownContact ct
        isAdmin = knownCt `elem` adminUsers || knownCt `elem` superUsers
        withUserGroupReg ugrId = withUserGroupReg_ ugrId . Just
        withUserGroupReg_ ugrId gName_ action =
          getUserGroupReg st (contactId' ct) ugrId >>= \case
            Nothing -> sendReply $ "Group ID " <> tshow ugrId <> " not found"
            Just gr@GroupReg {dbGroupId} -> do
              getGroup cc user dbGroupId >>= \case
                Nothing -> sendReply $ "Group ID " <> tshow ugrId <> " not found"
                Just g@GroupInfo {groupProfile = GroupProfile {displayName}}
                  | maybe True (displayName ==) gName_ -> action g gr
                  | otherwise -> sendReply $ "Group ID " <> tshow ugrId <> " has the display name " <> displayName
        sendReply = mkSendReply ct ciId
        withFoundListedGroups s_ action =
          getGroups_ s_ >>= \case
            Just groups -> filterListedGroups st groups >>= action
            Nothing -> sendReply "Error: getGroups. Please notify the developers."
        sendSearchResults s = \case
          [] -> sendReply "No groups found"
          gs -> do
            let gs' = takeTop searchResults gs
                moreGroups = length gs - length gs'
                more = if moreGroups > 0 then ", sending top " <> tshow (length gs') else ""
                reply = "Found " <> tshow (length gs) <> " group(s)" <> more <> "."
            updateSearchRequest (STSearch s) $ groupIds gs'
            sendFoundGroups reply gs' moreGroups
        sendAllGroups takeFirst sortName searchType = \case
          [] -> sendReply "No groups listed"
          gs -> do
            let gs' = takeFirst searchResults gs
                moreGroups = length gs - length gs'
                more = if moreGroups > 0 then ", sending " <> sortName <> " " <> tshow (length gs') else ""
                reply = tshow (length gs) <> " group(s) listed" <> more <> "."
            updateSearchRequest searchType $ groupIds gs'
            sendFoundGroups reply gs' moreGroups
        sendNextSearchResults takeFirst SearchRequest {searchType, sentGroups} = \case
          [] -> do
            sendReply "Sorry, no more groups"
            atomically $ TM.delete (contactId' ct) searchRequests
          gs -> do
            let gs' = takeFirst searchResults $ filterNotSent sentGroups gs
                sentGroups' = sentGroups <> groupIds gs'
                moreGroups = length gs - S.size sentGroups'
                reply = "Sending " <> tshow (length gs') <> " more group(s)."
            updateSearchRequest searchType sentGroups'
            sendFoundGroups reply gs' moreGroups
        updateSearchRequest :: SearchType -> Set GroupId -> IO ()
        updateSearchRequest searchType sentGroups = do
          searchTime <- getCurrentTime
          let search = SearchRequest {searchType, searchTime, sentGroups}
          atomically $ TM.insert (contactId' ct) search searchRequests
        sendFoundGroups reply gs moreGroups =
          void . forkIO $ sendComposedMessages_ cc (SRDirect $ contactId' ct) msgs
          where
            msgs = replyMsg :| map foundGroup gs <> [moreMsg | moreGroups > 0]
            replyMsg = (Just ciId, MCText reply)
            foundGroup (GIS GroupInfo {groupId, groupProfile = p@GroupProfile {image = image_}} GroupSummary {currentMembers}) =
              let membersStr = "_" <> tshow currentMembers <> " members_"
                  showId = if isAdmin then tshow groupId <> ". " else ""
                  text = showId <> groupInfoText p <> "\n" <> membersStr
               in (Nothing, maybe (MCText text) (\image -> MCImage {text, image}) image_)
            moreMsg = (Nothing, MCText $ "Send /next for " <> tshow moreGroups <> " more result(s).")

    deAdminCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRAdmin -> IO ()
    deAdminCommand ct ciId cmd
      | knownCt `elem` adminUsers || knownCt `elem` superUsers = case cmd of
          DCApproveGroup {groupId, displayName = n, groupApprovalId} ->
            withGroupAndReg sendReply groupId n $ \g gr@GroupReg {userGroupRegId = ugrId} ->
              readTVarIO (groupRegStatus gr) >>= \case
                GRSPendingApproval gaId
                  | gaId == groupApprovalId -> do
                      getDuplicateGroup g >>= \case
                        Nothing -> sendReply "Error: getDuplicateGroup. Please notify the developers."
                        Just DGReserved -> sendReply $ "The group " <> groupRef <> " is already listed in the directory."
                        _ -> do
                          getGroupRolesStatus g gr >>= \case
                            Just GRSOk -> do
                              setGroupStatus st gr GRSActive
                              let approved = "The group " <> userGroupReference' gr n <> " is approved"
                              notifyOwner gr $
                                (approved <> " and listed in directory - please moderate it!\n")
                                  <> "_Please note_: if you change the group profile it will be hidden from directory until it is re-approved.\n\n"
                                  <> "Supported commands:\n"
                                  <> ("/'filter " <> tshow ugrId <> "' - to configure anti-spam filter.\n")
                                  <> ("/'role " <> tshow ugrId <> "' - to set default member role.\n")
                                  <> ("/'link " <> tshow ugrId <> "' - to view/upgrade group link.")
                              invited <-
                                forM ownersGroup $ \og@KnownGroup {localDisplayName = ogName} -> do
                                  inviteToOwnersGroup og gr $ \case
                                    Right () -> do
                                      owner <- groupOwnerInfo groupRef $ dbContactId gr
                                      pure $ "Invited " <> owner <> " to owners' group " <> viewName ogName
                                    Left err -> pure err
                              sendReply $ "Group approved!" <> maybe "" ("\n" <>) invited
                              notifyOtherSuperUsers $ approved <> " by " <> viewName (localDisplayName' ct) <> maybe "" ("\n" <>) invited
                            Just GRSServiceNotAdmin -> replyNotApproved serviceNotAdmin
                            Just GRSContactNotOwner -> replyNotApproved "user is not an owner."
                            Just GRSBadRoles -> replyNotApproved $ "user is not an owner, " <> serviceNotAdmin
                            Nothing -> sendReply "Error: getGroupRolesStatus. Please notify the developers."
                          where
                            replyNotApproved reason = sendReply $ "Group is not approved: " <> reason
                            serviceNotAdmin = serviceName <> " is not an admin."
                  | otherwise -> sendReply "Incorrect approval code"
                _ -> sendReply $ "Error: the group " <> groupRef <> " is not pending approval."
            where
              groupRef = groupReference' groupId n
          DCRejectGroup _gaId _gName -> pure ()
          DCSuspendGroup groupId gName -> do
            let groupRef = groupReference' groupId gName
            withGroupAndReg sendReply groupId gName $ \_ gr ->
              readTVarIO (groupRegStatus gr) >>= \case
                GRSActive -> do
                  setGroupStatus st gr GRSSuspended
                  let suspended = "The group " <> userGroupReference' gr gName <> " is suspended"
                  notifyOwner gr $ suspended <> " and hidden from directory. Please contact the administrators."
                  sendReply "Group suspended!"
                  notifyOtherSuperUsers $ suspended <> " by " <> viewName (localDisplayName' ct)
                _ -> sendReply $ "The group " <> groupRef <> " is not active, can't be suspended."
          DCResumeGroup groupId gName -> do
            let groupRef = groupReference' groupId gName
            withGroupAndReg sendReply groupId gName $ \_ gr ->
              readTVarIO (groupRegStatus gr) >>= \case
                GRSSuspended -> do
                  setGroupStatus st gr GRSActive
                  let groupStr = "The group " <> userGroupReference' gr gName
                  notifyOwner gr $ groupStr <> " is listed in the directory again!"
                  sendReply "Group listing resumed!"
                  notifyOtherSuperUsers $ groupStr <> " listing resumed by " <> viewName (localDisplayName' ct)
                _ -> sendReply $ "The group " <> groupRef <> " is not suspended, can't be resumed."
          DCListLastGroups count -> listGroups count False
          DCListPendingGroups count -> listGroups count True
          DCSendToGroupOwner groupId gName msg -> do
            let groupRef = groupReference' groupId gName
            withGroupAndReg sendReply groupId gName $ \_ gr@GroupReg {dbContactId} -> do
              notifyOwner gr msg
              owner <- groupOwnerInfo groupRef dbContactId
              sendReply $ "Forwarded to " <> owner
          DCInviteOwnerToGroup groupId gName -> case ownersGroup of
            Just og@KnownGroup {localDisplayName = ogName} ->
              withGroupAndReg sendReply groupId gName $ \_ gr@GroupReg {dbContactId = ctId} -> do
                inviteToOwnersGroup og gr $ \case
                  Right () -> do
                    let groupRef = groupReference' groupId gName
                    owner <- groupOwnerInfo groupRef ctId
                    let invited =  " invited " <> owner <> " to owners' group " <> viewName ogName
                    notifyOtherSuperUsers $ viewName (localDisplayName' ct) <> invited
                    sendReply $ "you" <> invited
                  Left err -> sendReply err
            Nothing -> sendReply "owners' group is not specified"
          -- DCAddBlockedWord _word -> pure ()
          -- DCRemoveBlockedWord _word -> pure ()
          DCCommandError tag -> sendReply $ "Command error: " <> tshow tag
      | otherwise = sendReply "You are not allowed to use this command"
      where
        knownCt = knownContact ct
        sendReply = mkSendReply ct ciId
        notifyOtherSuperUsers s = withSuperUsers $ \ctId -> unless (ctId == contactId' ct) $ sendMessage' cc ctId s
        listGroups count pending =
          readTVarIO (groupRegs st) >>= \groups -> do
            grs <-
              if pending
                then filterM (fmap pendingApproval . readTVarIO . groupRegStatus) groups
                else pure groups
            sendReply $ tshow (length grs) <> " registered group(s)" <> (if length grs > count then ", showing the last " <> tshow count else "")
            void . forkIO $ forM_ (reverse $ take count grs) $ \gr@GroupReg {dbGroupId, dbContactId} -> do
              ct_ <- getContact' cc user dbContactId
              let ownerStr = "Owner: " <> maybe "getContact error" localDisplayName' ct_
              sendGroupInfo ct gr dbGroupId $ Just ownerStr
        inviteToOwnersGroup :: KnownGroup -> GroupReg -> (Either Text () -> IO a) -> IO a
        inviteToOwnersGroup KnownGroup {groupId = ogId} GroupReg {dbContactId = ctId} cont =
          sendChatCmd cc (APIListMembers ogId) >>= \case
            Right (CRGroupMembers _ (Group _ ms))
              | alreadyMember ms -> cont $ Left "Owner is already a member of owners' group"
              | otherwise -> do
                  sendChatCmd cc (APIAddMember ogId ctId GRMember) >>= \case
                    Right CRSentGroupInvitation {} -> do
                      printLog cc CLLInfo $ "invited contact ID " <> show ctId <> " to owners' group"
                      cont $ Right ()
                    r -> contErr r
            r -> contErr r
          where
            alreadyMember = isJust . find ((Just ctId == ) . memberContactId)
            contErr r = do
              let err = "error inviting contact ID " <> tshow ctId <> " to owners' group: " <> tshow r
              putStrLn $ T.unpack err
              cont $ Left err
        groupOwnerInfo groupRef dbContactId = do
          owner_ <- getContact' cc user dbContactId
          let ownerInfo = "the owner of the group " <> groupRef
              ownerName ct' = "@" <> viewName (localDisplayName' ct') <> ", "
          pure $ maybe "" ownerName owner_ <> ownerInfo

    deSuperUserCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRSuperUser -> IO ()
    deSuperUserCommand ct ciId cmd
      | knownContact ct `elem` superUsers = case cmd of
          DCExecuteCommand cmdStr ->
            sendChatCmdStr cc cmdStr >>= \case
              Right r -> do
                ts <- getCurrentTime
                tz <- getCurrentTimeZone
                sendReply $ T.pack $ serializeChatResponse (Nothing, Just user) (config cc) ts tz Nothing r
              Left e ->
                sendReply $ T.pack $ serializeChatError True (config cc) e
          DCCommandError tag -> sendReply $ "Command error: " <> tshow tag
      | otherwise = sendReply "You are not allowed to use this command"
      where
        sendReply = mkSendReply ct ciId

    knownContact :: Contact -> KnownContact
    knownContact ct = KnownContact {contactId = contactId' ct, localDisplayName = localDisplayName' ct}

    mkSendReply :: Contact -> ChatItemId -> Text -> IO ()
    mkSendReply ct ciId = sendComposedMessage cc ct (Just ciId) . MCText

    withGroupAndReg :: (Text -> IO ()) -> GroupId -> GroupName -> (GroupInfo -> GroupReg -> IO ()) -> IO ()
    withGroupAndReg sendReply gId = withGroupAndReg_ sendReply gId . Just

    withGroupAndReg_ :: (Text -> IO ()) -> GroupId -> Maybe GroupName -> (GroupInfo -> GroupReg -> IO ()) -> IO ()
    withGroupAndReg_ sendReply gId gName_ action =
      getGroup cc user gId >>= \case
        Nothing -> sendReply $ "Group ID " <> tshow gId <> " not found (getGroup)"
        Just g@GroupInfo {groupProfile = GroupProfile {displayName}}
          | maybe False (displayName ==) gName_ ->
              getGroupReg st gId >>= \case
                Nothing -> sendReply $ "Registration for group ID " <> tshow gId <> " not found (getGroupReg)"
                Just gr -> action g gr
          | otherwise ->
              sendReply $ "Group ID " <> tshow gId <> " has the display name " <> displayName

    sendGroupInfo :: Contact -> GroupReg -> GroupId -> Maybe Text -> IO ()
    sendGroupInfo ct gr@GroupReg {dbGroupId} useGroupId ownerStr_ = do
      grStatus <- readTVarIO $ groupRegStatus gr
      let statusStr = "Status: " <> groupRegStatusText grStatus
      getGroupAndSummary cc user dbGroupId >>= \case
        Just (GroupInfo {groupProfile = p@GroupProfile {image = image_}}, GroupSummary {currentMembers}) -> do
          let membersStr = "_" <> tshow currentMembers <> " members_"
              cmds = "/'role " <> tshow useGroupId <> "', /'filter " <> tshow useGroupId <> "'"
              text = T.unlines $ [tshow useGroupId <> ". " <> groupInfoText p] <> maybeToList ownerStr_ <> [membersStr, statusStr, cmds]
              msg = maybe (MCText text) (\image -> MCImage {text, image}) image_
          sendComposedMessage cc ct Nothing msg
        Nothing -> do
          let text = T.unlines $ [tshow useGroupId <> ". Error: getGroup. Please notify the developers."] <> maybeToList ownerStr_ <> [statusStr]
          sendComposedMessage cc ct Nothing $ MCText text

getContact' :: ChatController -> User -> ContactId -> IO (Maybe Contact)
getContact' cc user ctId = withDB "getContact" cc $ \db -> getContact db (vr cc) user ctId

getGroup :: ChatController -> User -> GroupId -> IO (Maybe GroupInfo)
getGroup cc user gId = withDB "getGroupInfo" cc $ \db -> getGroupInfo db (vr cc) user gId

withDB' :: Text -> ChatController -> (DB.Connection -> IO a) -> IO (Maybe a)
withDB' cxt cc a = withDB cxt cc $ ExceptT . fmap Right . a

withDB :: Text -> ChatController -> (DB.Connection -> ExceptT StoreError IO a) -> IO (Maybe a)
withDB cxt ChatController {chatStore} action = do
  r_ :: Either ChatError a <- withTransaction chatStore (runExceptT . withExceptT ChatErrorStore . action) `E.catches` handleDBErrors
  case r_ of
    Right r -> pure $ Just r
    Left e -> Nothing <$ logError ("Database error: " <> cxt <> " " <> tshow e)

getGroupAndSummary :: ChatController -> User -> GroupId -> IO (Maybe (GroupInfo, GroupSummary))
getGroupAndSummary cc user gId =
  withDB "getGroupAndSummary" cc $ \db -> (,) <$> getGroupInfo db (vr cc) user gId <*> liftIO (getGroupSummary db user gId)

vr :: ChatController -> VersionRangeChat
vr ChatController {config = ChatConfig {chatVRange}} = chatVRange
{-# INLINE vr #-}

getGroupLink' :: ChatController -> User -> GroupInfo -> IO (Maybe GroupLink)
getGroupLink' cc user gInfo =
  withDB "getGroupLink" cc $ \db -> getGroupLink db user gInfo

setGroupLinkRole :: ChatController -> GroupInfo -> GroupMemberRole -> IO (Maybe CreatedLinkContact)
setGroupLinkRole cc GroupInfo {groupId} mRole = resp <$> sendChatCmd cc (APIGroupLinkMemberRole groupId mRole)
  where
    resp = \case
      Right (CRGroupLink {groupLink = GroupLink {connLinkContact}}) -> Just connLinkContact
      _ -> Nothing

unexpectedError :: Text -> Text
unexpectedError err = "Unexpected error: " <> err <> ", please notify the developers."

strEncodeTxt :: StrEncoding a => a -> Text
strEncodeTxt = safeDecodeUtf8 . strEncode
