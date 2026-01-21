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
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Service
  ( welcomeGetOpts,
    directoryService,
    directoryServiceCLI,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Directory.BlockedWords
import Directory.Captcha
import Directory.Events
import Directory.Listing
import Directory.Options
import Directory.Search
import Directory.Store
import Directory.Store.Migrate
import Directory.Util
import Simplex.Chat.Bot
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Markdown (Format (..), FormattedText (..), parseMaybeMarkdownList, viewName)
import Simplex.Chat.Messages
import Simplex.Chat.Options
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Store.Direct (getContact)
import Simplex.Chat.Store.Groups (getGroupLink, getGroupMember, setGroupCustomData) -- TODO remove setGroupCustomData
import Simplex.Chat.Store.Profiles (GroupLinkInfo (..), getGroupLinkInfo)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.View (serializeChatError, serializeChatResponse, simplexChatContact, viewContactName, viewGroupName)
import Simplex.Messaging.Agent.Protocol (AConnectionLink (..), ConnectionLink (..), CreatedConnLink (..), SConnectionMode (..), sameConnReqContact, sameShortLinkContact)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util (eitherToMaybe, raceAny_, safeDecodeUtf8, tshow, unlessM, (<$$>))
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
    pendingCaptchas :: TMap GroupMemberId PendingCaptcha,
    updateListingsJob :: TMVar ChatController
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
  updateListingsJob <- newEmptyTMVarIO
  pure ServiceState {searchRequests, blockedWordsCfg, pendingCaptchas, updateListingsJob}

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

directoryServiceCLI :: DirectoryLog -> DirectoryOpts -> IO ()
directoryServiceCLI st opts = do
  env <- newServiceState opts
  eventQ <- newTQueueIO
  let eventHook cc resp = atomically $ resp <$ writeTQueue eventQ (cc, resp)
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ directoryPreStartHook opts,
            postStartHook = Just $ directoryPostStartHook opts env,
            eventHook = Just eventHook,
            acceptMember = Just $ acceptMemberHook opts env
          }
  raceAny_ $
    [ simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing,
      processEvents eventQ env
    ]
      <> updateListingsThread_ opts env
  where
    processEvents eventQ env = forever $ do
      (cc, resp) <- atomically $ readTQueue eventQ
      u_ <- readTVarIO (currentUser cc)
      forM_ u_ $ \user -> directoryServiceEvent st opts env user cc resp

updateListingDelay :: Int
updateListingDelay = 5 * 60 * 1000000 -- update every 5 minutes

updateListingsThread_ :: DirectoryOpts -> ServiceState -> [IO ()]
updateListingsThread_ opts env = maybe [] (\f -> [updateListingsThread f]) $ webFolder opts
  where
    updateListingsThread f = do
      cc <- atomically $ takeTMVar $ updateListingsJob env
      forever $ do
        u <- readTVarIO $ currentUser cc
        forM_ u $ \user -> updateGroupListingFiles cc user f
        delay <- registerDelay updateListingDelay
        atomically $ void (takeTMVar $ updateListingsJob env) `orElse` unlessM (readTVar delay) retry

listingsUpdated :: ServiceState -> ChatController -> IO ()
listingsUpdated env = void . atomically . tryPutTMVar (updateListingsJob env)

directoryPreStartHook :: DirectoryOpts -> ChatController -> IO ()
directoryPreStartHook opts ChatController {config, chatStore} = runDirectoryMigrations opts config chatStore

directoryPostStartHook :: DirectoryOpts -> ServiceState -> ChatController -> IO ()
directoryPostStartHook opts@DirectoryOpts {noAddress, testing} env cc =
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just User {userId, profile = p@LocalProfile {preferences}} -> do
      unless noAddress $ initializeBotAddress' (not testing) cc
      listingsUpdated env cc
      let cmds = fromMaybe [] $ preferences >>= commands_
      unless (cmds == directoryCommands) $ do
        let prefs = (fromMaybe emptyChatPrefs preferences) {files = Just FilesPreference {allow = FANo}, commands = Just directoryCommands} :: Preferences
            p' = (fromLocalProfile p) {displayName = serviceName opts, peerType = Just CPTBot, preferences = Just prefs} :: Profile
        liftIO $
          sendChatCmd cc (APIUpdateProfile userId p') >>= \case
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

directoryService :: DirectoryLog -> DirectoryOpts -> ChatConfig -> IO ()
directoryService st opts cfg = do
  env <- newServiceState opts
  let chatHooks =
        defaultChatHooks
          { preStartHook = Just $ directoryPreStartHook opts,
            postStartHook = Just $ directoryPostStartHook opts env,
            acceptMember = Just $ acceptMemberHook opts env
          }
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \user cc ->
    raceAny_ $
      [ forever $ void getLine,
        forever $ do
          (_, resp) <- atomically . readTBQueue $ outputQ cc
          directoryServiceEvent st opts env user cc resp
      ]
        <> updateListingsThread_ opts env

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
groupMemberAcceptance GroupInfo {customData} = (\DirectoryGroupData {memberAcceptance = ma} -> ma) $ fromCustomData customData

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

directoryServiceEvent :: DirectoryLog -> DirectoryOpts -> ServiceState -> User -> ChatController -> Either ChatError ChatEvent -> IO ()
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
    notifyOwner = sendMessage' cc . dbContactId
    ctId `isOwner` GroupReg {dbContactId} = ctId == dbContactId
    withGroupReg :: GroupInfo -> Text -> (GroupReg -> IO ()) -> IO ()
    withGroupReg GroupInfo {groupId, localDisplayName} err action =
      getGroupReg cc groupId >>= \case
        Right gr -> action gr
        Left e -> do
          let msg = "Error: " <> err <> ", group: " <> tshow groupId <> " " <> localDisplayName <> ", " <> T.pack e
          notifyAdminUsers msg
          logError msg
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

    getDuplicateGroup :: GroupInfo -> IO (Either String DuplicateGroup)
    getDuplicateGroup GroupInfo {groupId, groupProfile = GroupProfile {displayName}} =
      duplicateGroup <$$> getDuplicateGroupRegs cc user displayName
      where
        duplicateGroup [] = DGUnique
        duplicateGroup ((GroupInfo {groupId = gId, membership}, GroupReg {groupRegStatus = status}) : groups)
          | gId == groupId || memberRemoved membership = duplicateGroup groups
          | otherwise = case grDirectoryStatus status of
              DSListed -> DGReserved
              DSReserved -> DGReserved
              DSRegistered -> case duplicateGroup groups of
                DGReserved -> DGReserved
                _ -> DGRegistered
              DSRemoved -> duplicateGroup groups

    processInvitation :: Contact -> GroupInfo -> Maybe GroupReg -> IO ()
    processInvitation ct g@GroupInfo {groupId, groupProfile = GroupProfile {displayName}} = \case
      Nothing -> addGroupReg notifyAdminUsers st cc ct g GRSProposed joinGroup
      Just _gr -> setGroupStatus notifyAdminUsers st env cc groupId GRSProposed joinGroup
      where
        joinGroup _ = do
          r <- sendChatCmd cc $ APIJoinGroup groupId MFNone
          sendMessage cc ct $ case r of
            Right CRUserAcceptedGroupSent {} -> "Joining the group " <> displayName <> "…"
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
            Right DGUnique -> processInvitation ct g Nothing
            Right DGRegistered -> askConfirmation
            Right DGReserved -> sendMessage cc ct $ groupAlreadyListed g
            Left e -> sendMessage cc ct $ "Error: getDuplicateGroup. Please notify the developers.\n" <> T.pack e
      where
        askConfirmation =
          addGroupReg notifyAdminUsers st cc ct g GRSPendingConfirmation $ \GroupReg {userGroupRegId} -> do
            sendMessage cc ct $ "The group " <> groupNameDescr p <> " is already submitted to the directory.\nTo confirm the registration, please send:"
            sendMessage cc ct $ "/confirm " <> tshow userGroupRegId <> ":" <> viewName displayName

    badRolesMsg :: GroupRolesStatus -> Maybe Text
    badRolesMsg = \case
      GRSOk -> Nothing
      GRSServiceNotAdmin -> Just "You must grant directory service *admin* role to register the group"
      GRSContactNotOwner -> Just "You must have a group *owner* role to register the group"
      GRSBadRoles -> Just "You must have a group *owner* role and you must grant directory service *admin* role to register the group"

    getGroupRolesStatus :: GroupInfo -> GroupReg -> IO (Either String GroupRolesStatus)
    getGroupRolesStatus GroupInfo {groupId, membership = GroupMember {memberRole = serviceRole}} gr =
      rStatus <$$> getOwnerGroupMember groupId gr
      where
        rStatus GroupMember {memberRole} = groupRolesStatus memberRole serviceRole

    groupRolesStatus :: GroupMemberRole -> GroupMemberRole -> GroupRolesStatus
    groupRolesStatus contactRole serviceRole = case (contactRole, serviceRole) of
      (GROwner, GRAdmin) -> GRSOk
      (_, GRAdmin) -> GRSContactNotOwner
      (GROwner, _) -> GRSServiceNotAdmin
      _ -> GRSBadRoles

    getOwnerGroupMember :: GroupId -> GroupReg -> IO (Either String GroupMember)
    getOwnerGroupMember gId GroupReg {dbOwnerMemberId} = case dbOwnerMemberId of
      Just mId -> withDB "getGroupMember" cc $ \db -> withExceptT show $ getGroupMember db (vr cc) user gId mId
      Nothing -> pure $ Left "no owner member in group registration"

    deServiceJoinedGroup :: ContactId -> GroupInfo -> GroupMember -> IO ()
    deServiceJoinedGroup ctId g@GroupInfo {groupId} owner = do
      logInfo $ "service joined group " <> viewGroupName g
      withGroupReg g "joined group" $ \gr ->
        when (ctId `isOwner` gr) $ do
          let GroupInfo {groupProfile = GroupProfile {displayName}} = g
          setGroupRegOwner cc groupId owner >>= \case
            Left e -> do
              let msg = "Error updating group " <> tshow groupId <> " owner: " <> T.pack e
              logError msg
              notifyOwner gr msg
            Right () -> do
              logGUpdateOwner st groupId $ groupMemberId' owner
              notifyOwner gr $ "Joined the group " <> displayName <> ", creating the link…"
              sendChatCmd cc (APICreateGroupLink groupId GRMember) >>= \case
                Right CRGroupLinkCreated {groupLink = GroupLink {connLinkContact = gLink}} ->
                  setGroupStatus notifyAdminUsers st env cc groupId GRSPendingUpdate $ \gr' -> do
                    notifyOwner
                      gr'
                      "Created the public link to join the group via this directory service that is always online.\n\n\
                      \Please add it to the group welcome message.\n\
                      \For example, add:"
                    notifyOwner gr' $ "Link to join the group " <> displayName <> ": " <> groupLinkText gLink
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
        withGroupReg toGroup "group updated" $ \gr@GroupReg {groupRegStatus} -> do
          let userGroupRef = userGroupReference gr toGroup
              byMember = case memberContactId m of
                Just ctId | ctId `isOwner` gr -> "" -- group registration owner, not any group owner.
                _ -> " by " <> mName -- owner notification from directory will include the name.
          case groupRegStatus of
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
                      <> " after profile was updated"
                      <> byMember
                      <> ". Please report the error to the developers."
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
        groupLinkAdded gr byMember =
          getDuplicateGroup toGroup >>= \case
            Left e -> notifyOwner gr $ "Error: getDuplicateGroup. Please notify the developers.\n" <> T.pack e
            Right DGReserved -> notifyOwner gr $ groupAlreadyListed toGroup
            _ -> setGroupStatus notifyAdminUsers st env cc groupId (GRSPendingApproval gaId) $ \gr' -> do
              notifyOwner gr' $
                ("Thank you! The group link for " <> userGroupReference gr' toGroup <> " is added to the welcome message" <> byMember)
                  <> ".\nYou will be notified once the group is added to the directory - it may take up to 48 hours."
              checkRolesSendToApprove gr' gaId
              where
                gaId = 1
        processProfileChange gr byMember isActive n' = do
          let userGroupRef = userGroupReference gr toGroup
              groupRef = groupReference toGroup
          groupProfileUpdate >>= \case
            GPNoServiceLink -> setGroupStatus notifyAdminUsers st env cc groupId GRSPendingUpdate $ \gr' -> do
              notifyOwner gr' $
                ("The group profile is updated for " <> userGroupRef <> byMember <> ", but no link is added to the welcome message.\n\n")
                  <> "The group will remain hidden from the directory until the group link is added and the group is re-approved."
            GPServiceLinkRemoved -> setGroupStatus notifyAdminUsers st env cc groupId GRSPendingUpdate $ \gr' -> do
              notifyOwner gr' $
                ("The group link for " <> userGroupRef <> " is removed from the welcome message" <> byMember)
                  <> ".\n\nThe group is hidden from the directory until the group link is added and the group is re-approved."
              notifyAdminUsers $ "The group link is removed from " <> groupRef <> ", de-listed."
            GPServiceLinkAdded _ -> setGroupStatus notifyAdminUsers st env cc groupId (GRSPendingApproval n') $ \gr' -> do
              notifyOwner gr' $
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
              | otherwise -> setGroupStatus notifyAdminUsers st env cc groupId (GRSPendingApproval n') $ \gr' -> do
                  notifyOwner gr' $
                    ("The group " <> userGroupRef <> " is updated" <> byMember)
                      <> "!\nIt is hidden from the directory until approved."
                  notifyAdminUsers $ "The group " <> groupRef <> " is updated" <> byMember <> "."
                  checkRolesSendToApprove gr' n'
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
            Left e -> notifyOwner gr $ "Error: getGroupRolesStatus. Please notify the developers.\n" <> T.pack e
            Right (Just msg) -> notifyOwner gr msg
            Right Nothing -> sendToApprove toGroup gr gaId

    dePendingMember :: GroupInfo -> GroupMember -> IO ()
    dePendingMember g@GroupInfo {groupProfile = GroupProfile {displayName}} m
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
      gli_ <- join . eitherToMaybe <$> withDB' "getGroupLinkInfo" cc (\db -> getGroupLinkInfo db userId groupId)
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
    sendToApprove GroupInfo {groupId, groupProfile = p@GroupProfile {displayName, image = image'}, groupSummary} GroupReg {dbContactId, promoted} gaId = do
      ct_ <- getContact' cc user dbContactId
      let membersStr = "_" <> tshow (currentMembers groupSummary) <> " members_\n"
          text =
            either (\_ -> "The group ID " <> tshow groupId <> " submitted: ") (\c -> localDisplayName' c <> " submitted the group ID " <> tshow groupId <> ": ") ct_
              <> ("\n" <> groupInfoText p <> "\n" <> membersStr <> "\nTo approve send:")
          msg = maybe (MCText text) (\image -> MCImage {text, image}) image'
      withAdminUsers $ \cId -> do
        sendComposedMessage' cc cId Nothing msg
        sendMessage' cc cId $ "/approve " <> tshow groupId <> ":" <> viewName displayName <> " " <> tshow gaId <> if promoted then " promote=on" else ""

    deContactRoleChanged :: GroupInfo -> ContactId -> GroupMemberRole -> IO ()
    deContactRoleChanged g@GroupInfo {groupId, membership = GroupMember {memberRole = serviceRole}} ctId contactRole = do
      logInfo $ "contact ID " <> tshow ctId <> " role changed in group " <> viewGroupName g <> " to " <> tshow contactRole
      withGroupReg g "contact role changed" $ \gr@GroupReg {groupRegStatus} -> do
        let userGroupRef = userGroupReference gr g
            uCtRole = "Your role in the group " <> userGroupRef <> " is changed to " <> ctRole
        when (ctId `isOwner` gr) $
          case groupRegStatus of
            GRSSuspendedBadRoles | rStatus == GRSOk ->
              setGroupStatus notifyAdminUsers st env cc groupId GRSActive $ \gr' -> do
                notifyOwner gr' $ uCtRole <> ".\n\nThe group is listed in the directory again."
                notifyAdminUsers $ "The group " <> groupRef <> " is listed " <> suCtRole
            GRSPendingApproval gaId | rStatus == GRSOk -> do
              sendToApprove g gr gaId
              notifyOwner gr $ uCtRole <> ".\n\nThe group is submitted for approval."
            GRSActive | rStatus /= GRSOk ->
              setGroupStatus notifyAdminUsers st env cc groupId GRSSuspendedBadRoles $ \gr' -> do
                notifyOwner gr' $ uCtRole <> ".\n\nThe group is no longer listed in the directory."
                notifyAdminUsers $ "The group " <> groupRef <> " is de-listed " <> suCtRole
            _ -> pure ()
      where
        rStatus = groupRolesStatus contactRole serviceRole
        groupRef = groupReference g
        ctRole = "*" <> textEncode contactRole <> "*"
        suCtRole = "(user role is set to " <> ctRole <> ")."

    deServiceRoleChanged :: GroupInfo -> GroupMemberRole -> IO ()
    deServiceRoleChanged g@GroupInfo {groupId} serviceRole = do
      logInfo $ "service role changed in group " <> viewGroupName g <> " to " <> tshow serviceRole
      withGroupReg g "service role changed" $ \gr@GroupReg {groupRegStatus} -> do
        let userGroupRef = userGroupReference gr g
            uSrvRole = serviceName <> " role in the group " <> userGroupRef <> " is changed to " <> srvRole
        case groupRegStatus of
          GRSSuspendedBadRoles | serviceRole == GRAdmin ->
            whenContactIsOwner gr $
              setGroupStatus notifyAdminUsers st env cc groupId GRSActive $ \gr' -> do
                notifyOwner gr' $ uSrvRole <> ".\n\nThe group is listed in the directory again."
                notifyAdminUsers $ "The group " <> groupRef <> " is listed " <> suSrvRole
          GRSPendingApproval gaId | serviceRole == GRAdmin ->
            whenContactIsOwner gr $ do
              sendToApprove g gr gaId
              notifyOwner gr $ uSrvRole <> ".\n\nThe group is submitted for approval."
          GRSActive | serviceRole /= GRAdmin ->
            setGroupStatus notifyAdminUsers st env cc groupId GRSSuspendedBadRoles $ \gr' -> do
              notifyOwner gr' $ uSrvRole <> ".\n\nThe group is no longer listed in the directory."
              notifyAdminUsers $ "The group " <> groupRef <> " is de-listed " <> suSrvRole
          _ -> pure ()
      where
        groupRef = groupReference g
        srvRole = "*" <> textEncode serviceRole <> "*"
        suSrvRole = "(" <> serviceName <> " role is changed to " <> srvRole <> ")."
        whenContactIsOwner gr action =
          getOwnerGroupMember groupId gr
            >>= mapM_ (\cm@GroupMember {memberRole} -> when (memberRole == GROwner && memberActive cm) action)

    deContactRemovedFromGroup :: ContactId -> GroupInfo -> IO ()
    deContactRemovedFromGroup ctId g@GroupInfo {groupId} = do
      logInfo $ "contact ID " <> tshow ctId <> " removed from group " <> viewGroupName g
      withGroupReg g "contact removed" $ \gr -> do
        when (ctId `isOwner` gr) $
          setGroupStatus notifyAdminUsers st env cc groupId GRSRemoved $ \gr' -> do
            notifyOwner gr' $ "You are removed from the group " <> userGroupReference gr' g <> ".\n\nThe group is no longer listed in the directory."
            notifyAdminUsers $ "The group " <> groupReference g <> " is de-listed (group owner is removed)."

    deContactLeftGroup :: ContactId -> GroupInfo -> IO ()
    deContactLeftGroup ctId g@GroupInfo {groupId} = do
      logInfo $ "contact ID " <> tshow ctId <> " left group " <> viewGroupName g
      -- TODO combine
      withGroupReg g "contact left" $ \gr ->
        when (ctId `isOwner` gr) $
          setGroupStatus notifyAdminUsers st env cc groupId GRSRemoved $ \gr' -> do
            notifyOwner gr' $ "You left the group " <> userGroupReference gr g <> ".\n\nThe group is no longer listed in the directory."
            notifyAdminUsers $ "The group " <> groupReference g <> " is de-listed (group owner left)."

    deServiceRemovedFromGroup :: GroupInfo -> IO ()
    deServiceRemovedFromGroup g@GroupInfo {groupId} = do
      logInfo $ "service removed from group " <> viewGroupName g
      setGroupStatus notifyAdminUsers st env cc groupId GRSRemoved $ \gr -> do
        notifyOwner gr $ serviceName <> " is removed from the group " <> userGroupReference gr g <> ".\n\nThe group is no longer listed in the directory."
        notifyAdminUsers $ "The group " <> groupReference g <> " is de-listed (directory service is removed)."

    deGroupDeleted :: GroupInfo -> IO ()
    deGroupDeleted g@GroupInfo {groupId} = do
      logInfo $ "group removed " <> viewGroupName g
      setGroupStatus notifyAdminUsers st env cc groupId GRSRemoved $ \gr -> do
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
      DCSearchGroup s ->
        sendFoundListedGroups (STSearch s) Nothing "No groups found" $ \gs n ->  -- $ sendSearchResults s
          let more = if n > length gs then ", sending top " <> tshow (length gs) else ""
           in "Found " <> tshow n <> " group(s)" <> more <> "."
      DCSearchNext ->
        atomically (TM.lookup (contactId' ct) searchRequests) >>= \case
          Just SearchRequest {searchType, searchTime, lastGroup} -> do
            currentTime <- getCurrentTime
            if diffUTCTime currentTime searchTime > 300 -- 5 minutes
              then do
                atomically $ TM.delete (contactId' ct) searchRequests
                showAllGroups
              else
                sendFoundListedGroups searchType (Just lastGroup) "No more groups" $ \gs _ ->
                  "Sending " <> tshow (length gs) <> " more group(s)."
          Nothing -> showAllGroups
        where
          showAllGroups = deUserCommand ct ciId DCAllGroups
      DCAllGroups -> sendFoundListedGroups STAll Nothing "No groups listed" $ allGroupsReply "top"
      DCRecentGroups -> sendFoundListedGroups STRecent Nothing "No groups listed" $ allGroupsReply "the most recent"
      DCSubmitGroup _link -> pure ()
      DCConfirmDuplicateGroup ugrId gName ->
        withUserGroupReg ugrId gName $ \g@GroupInfo {groupProfile = GroupProfile {displayName}} gr@GroupReg {groupRegStatus} -> case groupRegStatus of
          GRSPendingConfirmation ->
            getDuplicateGroup g >>= \case
              Left e -> sendMessage cc ct $ "Error: getDuplicateGroup. Please notify the developers.\n" <> T.pack e
              Right DGReserved -> sendMessage cc ct $ groupAlreadyListed g
              _ -> processInvitation ct g $ Just gr
          _ -> sendReply $ "Error: the group ID " <> tshow ugrId <> " (" <> displayName <> ") is not pending confirmation."
      DCListUserGroups ->
        getUserGroupRegs cc user (contactId' ct) >>= \case
          Left e -> sendReply $ "Error reading groups: " <> T.pack e
          Right gs -> sendGroupsInfo ct ciId isAdmin (gs, length gs)
      DCDeleteGroup gId gName ->
        (if isAdmin then withGroupAndReg sendReply else withUserGroupReg) gId gName $ \GroupInfo {groupProfile = GroupProfile {displayName}} GroupReg {dbGroupId} -> do
          delGroupReg cc dbGroupId >>= \case
            Right () -> do
              logGDelete st dbGroupId
              sendReply $ (if isAdmin then "The group " else "Your group ") <> displayName <> " is deleted from the directory"
            Left e -> sendReply $ "Error deleting group " <> displayName <> ": " <> T.pack e
      DCMemberRole gId gName_ mRole_ ->
        (if isAdmin then withGroupAndReg_ sendReply else withUserGroupReg_) gId gName_ $ \g _gr -> do
          let GroupInfo {groupProfile = GroupProfile {displayName = n}} = g
          case mRole_ of
            Nothing ->
              getGroupLink' cc user g >>= \case
                Right GroupLink {connLinkContact = gLink, acceptMemberRole} -> do
                  let anotherRole = case acceptMemberRole of GRObserver -> GRMember; _ -> GRObserver
                  sendReply $
                    initialRole n acceptMemberRole
                      <> ("Send /'role " <> tshow gId <> " " <> textEncode anotherRole <> "' to change it.\n\n")
                      <> onlyViaLink gLink
                Left _ -> sendReply $ "Error: failed reading the initial member role for the group " <> n
            Just mRole -> do
              setGroupLinkRole cc g mRole >>= \case
                Just gLink -> sendReply $ initialRole n mRole <> "\n" <> onlyViaLink gLink
                Nothing -> sendReply $ "Error: the initial member role for the group " <> n <> " was NOT upgated."
        where
          initialRole n mRole = "The initial member role for the group " <> n <> " is set to *" <> textEncode mRole <> "*\n"
          onlyViaLink gLink = "*Please note*: it applies only to members joining via this link: " <> groupLinkText gLink
      DCGroupFilter gId gName_ acceptance_ ->
        (if isAdmin then withGroupAndReg_ sendReply else withUserGroupReg_) gId gName_ $ \g _gr -> do
          let GroupInfo {groupProfile = GroupProfile {displayName = n}} = g
              a = groupMemberAcceptance g
          case acceptance_ of
            Just a' | a /= a' -> do
              let d = toCustomData $ DirectoryGroupData a'
              withDB' "setGroupCustomData" cc (\db -> setGroupCustomData db user g $ Just d) >>= \case
                Right () -> sendSettigns n a' " set to"
                Left e -> sendReply $ "Error changing spam filter settings for group " <> n <> ": " <> T.pack e
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
              sendReply $
                T.unlines $
                  [ "The link to join the group " <> groupRef <> ":",
                    groupLinkText gLink,
                    "New member role: " <> textEncode acceptMemberRole
                  ]
                    <> ["The link is being upgraded..." | shouldBeUpgraded]
              when shouldBeUpgraded $ do
                let send = sendComposedMessage cc ct Nothing . MCText . T.unlines
                withGroupLinkResult groupRef (sendChatCmd cc $ APIAddGroupShortLink groupId) $
                  \GroupLink {connLinkContact = CCLink _ sLnk_'} -> case (sLnk_, sLnk_') of
                    (Just _, Just _) ->
                      send ["The group link is upgraded for: " <> groupRef, "No changes to group needed."]
                    (Nothing, Just sLnk) ->
                      sendComposedMessages
                        cc
                        (SRDirect $ contactId' ct)
                        [ MCText $
                            T.unlines
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
          getUserGroupReg cc user (contactId' ct) ugrId >>= \case
            -- TODO differentiate group not found error
            Left e -> sendReply $ "Group ID " <> tshow ugrId <> " error:" <> T.pack e
            Right (g@GroupInfo {groupProfile = GroupProfile {displayName}}, gr)
              | maybe True (displayName ==) gName_ -> action g gr
              | otherwise -> sendReply $ "Group ID " <> tshow ugrId <> " has the display name " <> displayName
        sendReply = mkSendReply ct ciId
        sendFoundListedGroups searchType lastGroup_ notFound replyStr =
          searchListedGroups cc user searchType lastGroup_ searchResults >>= \case
            Right ([], _) -> do
              atomically $ TM.delete (contactId' ct) searchRequests
              sendReply notFound
            Right (gs, n) -> do
              let moreGroups = n - length gs
              updateSearchRequest searchType $ last gs
              sendFoundGroups (replyStr gs n) gs moreGroups
            Left e -> sendReply $ "Error: searchListedGroups. Please notify the developers.\n" <> T.pack e
        allGroupsReply sortName gs n =
          let more = if n > length gs then ", sending " <> sortName <> " " <> tshow (length gs) else ""
           in tshow n <> " group(s) listed" <> more <> "."
        updateSearchRequest :: SearchType -> (GroupInfo, GroupReg) -> IO ()
        updateSearchRequest searchType (GroupInfo {groupId}, _) = do
          searchTime <- getCurrentTime
          let search = SearchRequest {searchType, searchTime, lastGroup = groupId}
          atomically $ TM.insert (contactId' ct) search searchRequests
        sendFoundGroups reply gs moreGroups =
          void . forkIO $ sendComposedMessages_ cc (SRDirect $ contactId' ct) msgs
          where
            msgs = replyMsg :| map foundGroup gs <> [moreMsg | moreGroups > 0]
            replyMsg = (Just ciId, MCText reply)
            foundGroup (GroupInfo {groupId, groupProfile = p@GroupProfile {image = image_}, groupSummary = GroupSummary {currentMembers}}, _) =
              let membersStr = "_" <> tshow currentMembers <> " members_"
                  showId = if isAdmin then tshow groupId <> ". " else ""
                  text = showId <> groupInfoText p <> "\n" <> membersStr
               in (Nothing, maybe (MCText text) (\image -> MCImage {text, image}) image_)
            moreMsg = (Nothing, MCText $ "Send /next for " <> tshow moreGroups <> " more result(s).")

    deAdminCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRAdmin -> IO ()
    deAdminCommand ct ciId cmd
      | knownCt `elem` adminUsers || knownCt `elem` superUsers = case cmd of
          DCApproveGroup {groupId, displayName = n, groupApprovalId, promote} ->
            withGroupAndReg sendReply groupId n $ \g gr@GroupReg {userGroupRegId = ugrId, promoted} ->
              case groupRegStatus gr of
                GRSPendingApproval gaId
                  | gaId == groupApprovalId ->
                      getDuplicateGroup g >>= \case
                        Left e -> sendReply $ "Error: getDuplicateGroup. Please notify the developers.\n" <> T.pack e
                        Right DGReserved -> sendReply $ "The group " <> groupRef <> " is already listed in the directory."
                        _ -> getGroupRolesStatus g gr >>= \case
                          Right GRSOk -> do
                            let grPromoted'
                                  | promoted || knownCt `elem` superUsers = fromMaybe promoted promote
                                  | otherwise = False
                            setGroupStatusPromo sendReply st env cc gr GRSActive grPromoted' $ do
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
                              sendReply $ "Group approved" <> (if grPromoted' then " (promoted)" else "") <> "!" <> maybe "" ("\n" <>) invited
                              notifyOtherSuperUsers $ approved <> " by " <> viewName (localDisplayName' ct) <> maybe "" ("\n" <>) invited
                          Right GRSServiceNotAdmin -> replyNotApproved serviceNotAdmin
                          Right GRSContactNotOwner -> replyNotApproved "user is not an owner."
                          Right GRSBadRoles -> replyNotApproved $ "user is not an owner, " <> serviceNotAdmin
                          Left e -> sendReply $ "Error: getGroupRolesStatus. Please notify the developers.\n" <> T.pack e
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
              case groupRegStatus gr of
                GRSActive -> setGroupStatus sendReply st env cc groupId GRSSuspended $ \gr' -> do
                  let suspended = "The group " <> userGroupReference' gr gName <> " is suspended"
                  notifyOwner gr' $ suspended <> " and hidden from directory. Please contact the administrators."
                  sendReply "Group suspended!"
                  notifyOtherSuperUsers $ suspended <> " by " <> viewName (localDisplayName' ct)
                _ -> sendReply $ "The group " <> groupRef <> " is not active, can't be suspended."
          DCResumeGroup groupId gName -> do
            let groupRef = groupReference' groupId gName
            withGroupAndReg sendReply groupId gName $ \_ gr ->
              case groupRegStatus gr of
                GRSSuspended -> setGroupStatus sendReply st env cc groupId GRSActive $ \gr' -> do
                  let groupStr = "The group " <> userGroupReference' gr gName
                  notifyOwner gr' $ groupStr <> " is listed in the directory again!"
                  sendReply "Group listing resumed!"
                  notifyOtherSuperUsers $ groupStr <> " listing resumed by " <> viewName (localDisplayName' ct)
                _ -> sendReply $ "The group " <> groupRef <> " is not suspended, can't be resumed."
          DCListLastGroups count ->
            listLastGroups cc user count >>= \case
              Left e -> sendReply $ "Error reading groups: " <> T.pack e
              Right gs -> sendGroupsInfo ct ciId True $ first reverse gs
          DCListPendingGroups count ->
            listPendingGroups cc user count >>= \case
              Left e -> sendReply $ "Error reading groups: " <> T.pack e
              Right gs -> sendGroupsInfo ct ciId True $ first reverse gs
          DCSendToGroupOwner groupId gName msg -> do
            let groupRef = groupReference' groupId gName
            withGroupAndReg sendReply groupId gName $ \_ gr@GroupReg {dbContactId = ctId} -> do
              notifyOwner gr msg
              owner <- groupOwnerInfo groupRef ctId
              sendReply $ "Forwarded to " <> owner
          DCInviteOwnerToGroup groupId gName -> case ownersGroup of
            Just og@KnownGroup {localDisplayName = ogName} ->
              withGroupAndReg sendReply groupId gName $ \_ gr@GroupReg {dbContactId = ctId} -> do
                inviteToOwnersGroup og gr $ \case
                  Right () -> do
                    let groupRef = groupReference' groupId gName
                    owner <- groupOwnerInfo groupRef ctId
                    let invited = " invited " <> owner <> " to owners' group " <> viewName ogName
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
            alreadyMember = isJust . find ((Just ctId ==) . memberContactId)
            contErr r = do
              let err = "error inviting contact ID " <> tshow ctId <> " to owners' group: " <> tshow r
              putStrLn $ T.unpack err
              cont $ Left err
        groupOwnerInfo groupRef dbContactId = do
          owner_ <- getContact' cc user dbContactId
          let ownerInfo = "the owner of the group " <> groupRef
              ownerName ct' = "@" <> viewName (localDisplayName' ct') <> ", "
          pure $ either (const "") ownerName owner_ <> ownerInfo

    deSuperUserCommand :: Contact -> ChatItemId -> DirectoryCmd 'DRSuperUser -> IO ()
    deSuperUserCommand ct ciId cmd
      | knownContact ct `elem` superUsers = case cmd of
          DCPromoteGroup groupId gName promote' ->
            withGroupAndReg sendReply groupId gName $ \_ gr@GroupReg {groupRegStatus, promoted} -> do
              let notify = sendReply $ "Group promotion " <> (if promote' then "enabled" <> (if groupRegStatus == GRSActive then "." else ", but the group is not listed.") else "disabled.")
              if promote' /= promoted
                then setGroupPromoted sendReply st env cc gr promote' notify
                else notify
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
      getGroupAndReg cc user gId >>= \case
        Left e -> sendReply $ "Group " <> tshow gId <> " error (getGroup): " <> T.pack e
        Right (g@GroupInfo {groupProfile = GroupProfile {displayName}}, gr)
          | maybe False (displayName ==) gName_ ->
              action g gr
          | otherwise ->
              sendReply $ "Group ID " <> tshow gId <> " has the display name " <> displayName

    getOwnersInfo :: [(GroupInfo, GroupReg)] -> IO [((GroupInfo, GroupReg), Maybe (Either String Contact))]
    getOwnersInfo gs =
      fmap (either (\e -> map (,Just (Left e)) gs) id) $ withDB' "getOwnersInfo" cc $ \db ->
        mapM (\g@(_, gr) -> fmap ((g,) . Just . first show) $ runExceptT $ getContact db (vr cc) user $ dbContactId gr) gs

    sendGroupsInfo :: Contact -> ChatItemId -> Bool -> ([(GroupInfo, GroupReg)], Int) -> IO ()
    sendGroupsInfo ct ciId isAdmin (gs, n) = do
      let more = if n > length gs then ", showing the last " <> tshow (length gs) else ""
          replyMsg = (Just ciId, MCText $ tshow n <> " registered group(s)" <> more)
      gs' <- if isAdmin then getOwnersInfo gs else pure $ map (,Nothing) gs
      sendComposedMessages_ cc (SRDirect $ contactId' ct) $ replyMsg :| map groupMessage gs'
      where
        groupMessage ((g, gr), ct_) =
          let GroupInfo {groupId, groupProfile = p@GroupProfile {image = image_}, groupSummary} = g
              GroupReg {userGroupRegId, groupRegStatus} = gr
              useGroupId = if isAdmin then groupId else userGroupRegId
              statusStr = "Status: " <> groupRegStatusText groupRegStatus
              membersStr = "_" <> tshow (currentMembers groupSummary) <> " members_"
              cmds = "/'role " <> tshow useGroupId <> "', /'filter " <> tshow useGroupId <> "'"
              ownerStr = maybe "" (("Owner: " <>) . either (("getContact error: " <>) . T.pack) localDisplayName') ct_
              text = T.unlines $ [tshow useGroupId <> ". " <> groupInfoText p] ++ [ownerStr | isAdmin] ++ [membersStr, statusStr, cmds]
              msg = maybe (MCText text) (\image -> MCImage {text, image}) image_
           in (Nothing, msg)

setGroupStatusPromo :: (Text -> IO ()) -> DirectoryLog -> ServiceState -> ChatController -> GroupReg -> GroupRegStatus -> Bool -> IO () -> IO ()
setGroupStatusPromo sendReply st env cc GroupReg {dbGroupId = gId} grStatus' grPromoted' continue = do
  let status' = grDirectoryStatus grStatus'
  setGroupStatusPromoStore cc gId grStatus' grPromoted' >>= \case
    Left e -> sendReply $ "Error updating group " <> tshow gId <> " status: " <> T.pack e
    Right (status, grPromoted) -> do
      when ((status == DSListed || status' == DSListed) && (status /= status' || grPromoted /= grPromoted')) $
        listingsUpdated env cc
      logGUpdateStatus st gId grStatus'
      logGUpdatePromotion st gId grPromoted'
      continue

addGroupReg :: (Text -> IO ()) -> DirectoryLog -> ChatController -> Contact -> GroupInfo -> GroupRegStatus -> (GroupReg -> IO ()) -> IO ()
addGroupReg sendMsg st cc ct g@GroupInfo {groupId} grStatus continue =
  addGroupRegStore cc ct g grStatus >>= \case
    Left e -> sendMsg $ "Error creating group registation for group " <> tshow groupId <> ": " <> T.pack e
    Right gr -> do
      logGCreate st gr
      continue gr

setGroupStatus :: (Text -> IO ()) -> DirectoryLog -> ServiceState -> ChatController -> GroupId -> GroupRegStatus -> (GroupReg -> IO ()) -> IO ()
setGroupStatus sendMsg st env cc gId grStatus' continue = do
  let status' = grDirectoryStatus grStatus'
  setGroupStatusStore cc gId grStatus' >>= \case
    Left e -> sendMsg $ "Error updating group " <> tshow gId <> " status: " <> T.pack e
    Right (grStatus, gr) -> do
      let status = grDirectoryStatus grStatus
      when ((status == DSListed || status' == DSListed) && status /= status') $ listingsUpdated env cc
      logGUpdateStatus st gId grStatus'
      continue gr

setGroupPromoted :: (Text -> IO ()) -> DirectoryLog -> ServiceState -> ChatController -> GroupReg -> Bool -> IO () -> IO ()
setGroupPromoted sendReply st env cc GroupReg {dbGroupId = gId} grPromoted' continue =
  setGroupPromotedStore cc gId grPromoted' >>= \case
    Left e -> sendReply $ "Error updating group " <> tshow gId <> " status: " <> T.pack e
    Right (status, grPromoted) -> do
      when (status == DSListed && grPromoted' /= grPromoted) $ listingsUpdated env cc
      logGUpdatePromotion st gId grPromoted'
      continue

updateGroupListingFiles :: ChatController -> User -> FilePath -> IO ()
updateGroupListingFiles cc u dir =
  getAllListedGroups cc u >>= \case
    Right gs -> generateListing dir gs
    Left e -> logError $ "generateListing error: failed to read groups: " <> T.pack e

getContact' :: ChatController -> User -> ContactId -> IO (Either String Contact)
getContact' cc user ctId = withDB "getContact" cc $ \db ->  withExceptT show $ getContact db (vr cc) user ctId

getGroupLink' :: ChatController -> User -> GroupInfo -> IO (Either String GroupLink)
getGroupLink' cc user gInfo =
  withDB "getGroupLink" cc $ \db -> withExceptT groupDBError $ getGroupLink db user gInfo

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
