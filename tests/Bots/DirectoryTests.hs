{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Bots.DirectoryTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, when)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), NominalDiffTime, addUTCTime, nominalDay)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.System (systemEpochDay)
import qualified Options.Applicative as OA
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Directory.BlockedWords
import Directory.Captcha
import Directory.Events
import Directory.Listing
import Directory.Options
import Directory.Search
import Directory.Service
import Directory.Store
import Simplex.Messaging.Encoding.String (strEncode, strDecode)
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller (ChatConfig (..), ChatEvent (..), ChatError (..), ChatErrorType (..))
import Simplex.Chat.Store (StoreError (..))
import qualified Simplex.Chat.Markdown as MD
import Simplex.Chat.Options (ChatCmdLog (..), ChatOpts (..), CoreChatOpts (..), CreateBotOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Types (AgentConnId (..), ChatPeerType (..), CustomData (..), GroupSummary (..), Profile (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Agent.Protocol (AgentErrorType (..), BrokerErrorType (..))
import Simplex.Messaging.Protocol (NetworkError (..))
import System.Directory (emptyPermissions, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))
import System.IO (hClose)
import Test.Hspec hiding (it)

directoryServiceTests :: SpecWith TestParams
directoryServiceTests = do
  it "should register group" testDirectoryService
  it "should suspend and resume group, send message to owner" testSuspendResume
  it "should delete group registration" testDeleteGroup
  it "admin should delete group registration" testDeleteGroupAdmin
  it "should change initial member role" testSetRole
  it "should join found group via link" testJoinGroup
  it "should support group names with spaces" testGroupNameWithSpaces
  it "should return more groups in search, all and recent groups" testSearchGroups
  it "should invite to owners' group if specified" testInviteToOwnersGroup
  describe "de-listing the group" $ do
    it "should de-list if owner leaves the group" testDelistedOwnerLeaves
    it "should de-list if owner is removed from the group" testDelistedOwnerRemoved
    it "should NOT de-list if another member leaves the group" testNotDelistedMemberLeaves
    it "should NOT de-list if another member is removed from the group" testNotDelistedMemberRemoved
    it "should de-list if service is removed from the group" testDelistedServiceRemoved
    it "should de-list if group is deleted" testDelistedGroupDeleted
    it "should de-list/re-list when service/owner roles change" testDelistedRoleChanges
    it "should NOT de-list if another member role changes" testNotDelistedMemberRoleChanged
    it "should NOT send to approval if roles are incorrect" testNotSentApprovalBadRoles
    it "should NOT allow approving if roles are incorrect" testNotApprovedBadRoles
  describe "should require re-approval if profile is changed by" $ do
    it "the registration owner" testRegOwnerChangedProfile
    it "another owner" testAnotherOwnerChangedProfile
    it "another owner not connected to directory" testNotConnectedOwnerChangedProfile
  describe "should require profile update if group link is removed by " $ do
    it "the registration owner" testRegOwnerRemovedLink
    it "another owner" testAnotherOwnerRemovedLink
    it "another owner not connected to directory" testNotConnectedOwnerRemovedLink
  describe "duplicate groups (same display name and full name)" $ do
    it "should ask for confirmation if a duplicate group is submitted" testDuplicateAskConfirmation
    it "should prohibit registration if a duplicate group is listed" testDuplicateProhibitRegistration
    it "should prohibit confirmation if a duplicate group is listed" testDuplicateProhibitConfirmation
    it "should prohibit when profile is updated and not send for approval" testDuplicateProhibitWhenUpdated
    it "should prohibit approval if a duplicate group is listed" testDuplicateProhibitApproval
  describe "list and promote groups" $ do
    it "should list and promote user's groups" $ testListUserGroups True
  describe "member admission" $ do
    it "should ask member to pass captcha screen" testCapthaScreening
    it "should send voice captcha on /audio command" testVoiceCaptchaScreening
    it "should retry with voice captcha after switching to audio mode" testVoiceCaptchaRetry
    it "should reject member after too many captcha attempts" testCaptchaTooManyAttempts
    it "should respond to unknown command during captcha" testCaptchaUnknownCommand
  describe "store log" $ do
    it "should restore directory service state" testRestoreDirectory
  describe "captcha" $ do
    it "should accept some incorrect spellings" testCaptcha
    it "should generate captcha of correct length" testGetCaptchaStr
  describe "blocked words" $ do
    it "should detect blocked words with spelling variations" testBlockedWords
    it "should detect blocked fragments" testBlockedFragments
    it "should remove triple characters" testRemoveTriples
    it "should generate word variants with extension rules" testWordVariants
    it "should detect blocked words with double-space splitting" testBlockedWordsDoubleSpacer
    it "should normalize text with spelling and special chars" testNormalizeText
    it "should generate all substitutions" testAllSubstitutions
  describe "help commands" $ do
    it "should show commands help" testHelpCommands
    it "should not list audio command" testHelpNoAudio
    it "should reject audio command in DM" testAudioCommandInDM
  describe "admin commands" $ do
    it "should suspend and resume group" testAdminSuspendResume
    -- Note: /reject not implemented in service (DCRejectGroup -> pure ())
    it "should list last and pending groups" testAdminListGroups
    it "should send message to group owner" testAdminSendToOwner
  describe "events parsing" $ do
    it "should parse directory commands" testCommandParsing
    it "should handle command errors and edge cases" testCommandParsingEdgeCases
    it "should map chat events to directory events" testCrDirectoryEvent
    it "should show event types" testEventsShowInstances
  describe "listing" $ do
    it "should compute recent rounded time" testRecentRoundedTime
    it "should format text to markdown" testToFormattedText
    it "should define correct path constants" testListingPaths
    it "should define newOrActive constant" testNewOrActive
    it "should JSON round-trip DirectoryListing" testDirectoryListingJSON
    it "should JSON round-trip DirectoryEntryType" testDirectoryEntryTypeJSON
  describe "store" $ do
    it "should encode and decode GroupRegStatus" testGroupRegStatusEncoding
    it "should encode and decode DirectoryLogRecord" testDirectoryLogRecordEncoding
    it "should encode and decode GroupReg" testGroupRegEncoding
    it "should return correct status text" testGroupRegStatusText
    it "should map status to directory status" testGrDirectoryStatus
    it "should detect pending approval" testPendingApproval
    it "should detect removed groups" testGroupRemoved
    it "should define filter presets" testFilterPresets
    it "should round-trip custom data" testCustomData
    it "should read directory log data" testReadDirectoryLogData
    it "should open directory log with Nothing" testOpenDirectoryLogNothing
    it "should convert store errors" testGroupDBError
    it "should handle log warnings for missing groups" testReadDirectoryLogWarnings
    it "should JSON round-trip ProfileCondition" testProfileConditionJSON
    it "should JSON round-trip DirectoryMemberAcceptance" testDirectoryMemberAcceptanceJSON
    it "should JSON round-trip DirectoryGroupData" testDirectoryGroupDataJSON
    it "should open directory log with Just path" testOpenDirectoryLogJust
    it "should show store types" testStoreShowInstances
  describe "options" $ do
    it "should create chat opts from directory opts" testMkChatOpts
    it "should cover MigrateLog constructors" testMigrateLogConstructors
    it "should parse directory options" testDirectoryOptsParser
  describe "search" $ do
    it "should construct search types" testSearchTypes

directoryProfile :: Profile
directoryProfile = Profile {displayName = "SimpleX Directory", fullName = "", shortDescr = Nothing, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences = Nothing}

mkDirectoryOpts :: TestParams -> [KnownContact] -> Maybe KnownGroup -> Maybe FilePath -> DirectoryOpts
mkDirectoryOpts TestParams {tmpPath = ps} superUsers ownersGroup webFolder =
  DirectoryOpts
    { coreOptions =
        testCoreOpts
          { dbOptions =
              (dbOptions testCoreOpts)
#if defined(dbPostgres)
                {dbSchemaPrefix = "client_" <> serviceDbPrefix}
#else
                {dbFilePrefix = ps </> serviceDbPrefix}
#endif

          },
      adminUsers = [],
      superUsers,
      ownersGroup,
      noAddress = False,
      blockedFragmentsFile = Nothing,
      blockedWordsFile = Nothing,
      blockedExtensionRules = Nothing,
      nameSpellingFile = Nothing,
      profileNameLimit = maxBound,
      captchaGenerator = Nothing,
      voiceCaptchaGenerator = Nothing,
      directoryLog = Just $ ps </> "directory_service.log",
      migrateDirectoryLog = Nothing,
      serviceName = "SimpleX Directory",
      runCLI = False,
      searchResults = 3,
      webFolder,
      testing = True
    }

serviceDbPrefix :: FilePath
serviceDbPrefix = "directory_service"

viewName :: String -> String
viewName = T.unpack . MD.viewName . T.pack

testDirectoryService :: HasCallStack => TestParams -> IO ()
testDirectoryService ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        bob #> "@'SimpleX Directory' privacy"
        bob <# "'SimpleX Directory'> > privacy"
        bob <## "      No groups found"
        -- putStrLn "*** create a group"
        bob ##> "/g PSA Privacy, Security & Anonymity"
        bob <## "group #PSA (Privacy, Security & Anonymity) is created"
        bob <## "to add members use /a PSA <name> or /create link #PSA"
        bob ##> "/a PSA 'SimpleX Directory' member"
        bob <## "invitation to join the group #PSA sent to 'SimpleX Directory'"
        bob <# "'SimpleX Directory'> You must grant directory service admin role to register the group"
        bob ##> "/mr PSA 'SimpleX Directory' admin"
        -- putStrLn "*** discover service joins group and creates the link for profile"
        bob <## "#PSA: you changed the role of 'SimpleX Directory' to admin"
        bob <# "'SimpleX Directory'> Joining the group PSA…"
        bob <## "#PSA: 'SimpleX Directory' joined the group"
        bob <# "'SimpleX Directory'> Joined the group PSA, creating the link…"
        bob <# "'SimpleX Directory'> Created the public link to join the group via this directory service that is always online."
        bob <## ""
        bob <## "Please add it to the group welcome message."
        bob <## "For example, add:"
        welcomeWithLink <- dropStrPrefix "'SimpleX Directory'> " . dropTime <$> getTermLine bob
        -- putStrLn "*** update profile without link"
        updateGroupProfile bob "Welcome!"
        bob <# "'SimpleX Directory'> The profile updated for ID 1 (PSA), but the group link is not added to the welcome message."
        (superUser </)
        -- putStrLn "*** update profile so that it has link"
        updateGroupProfile bob welcomeWithLink
        bob <# "'SimpleX Directory'> Thank you! The group link for ID 1 (PSA) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        approvalRequested superUser welcomeWithLink (1 :: Int)
        -- putStrLn "*** update profile so that it still has link"
        let welcomeWithLink' = "Welcome! " <> welcomeWithLink
        updateGroupProfile bob welcomeWithLink'
        bob <# "'SimpleX Directory'> The group ID 1 (PSA) is updated!"
        bob <## "It is hidden from the directory until approved."
        superUser <# "'SimpleX Directory'> The group ID 1 (PSA) is updated."
        approvalRequested superUser welcomeWithLink' (2 :: Int)
        -- putStrLn "*** try approving with the old registration code"
        bob #> "@'SimpleX Directory' /approve 1:PSA 1"
        bob <# "'SimpleX Directory'> > /approve 1:PSA 1"
        bob <## "      You are not allowed to use this command"
        superUser #> "@'SimpleX Directory' /approve 1:PSA 1"
        superUser <# "'SimpleX Directory'> > /approve 1:PSA 1"
        superUser <## "      Incorrect approval code"
        -- putStrLn "*** update profile so that it has no link"
        updateGroupProfile bob "Welcome!"
        bob <# "'SimpleX Directory'> The group link for ID 1 (PSA) is removed from the welcome message."
        bob <## ""
        bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
        superUser <# "'SimpleX Directory'> The group link is removed from ID 1 (PSA), de-listed."
        superUser #> "@'SimpleX Directory' /approve 1:PSA 2"
        superUser <# "'SimpleX Directory'> > /approve 1:PSA 2"
        superUser <## "      Error: the group ID 1 (PSA) is not pending approval."
        -- putStrLn "*** update profile so that it has link again"
        updateGroupProfile bob welcomeWithLink'
        bob <# "'SimpleX Directory'> Thank you! The group link for ID 1 (PSA) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        approvalRequested superUser welcomeWithLink' (1 :: Int)
        superUser #> "@'SimpleX Directory' /pending"
        superUser <# "'SimpleX Directory'> > /pending"
        superUser <## "      1 registered group(s)"
        superUser <# "'SimpleX Directory'> 1. PSA (Privacy, Security & Anonymity)"
        superUser <## "Welcome message:"
        superUser <##. "Welcome! Link to join the group PSA: "
        superUser <## "Owner: bob"
        superUser <## "2 members"
        superUser <## "Status: pending admin approval"
        superUser <## "/'role 1', /'filter 1'"
        superUser #> "@'SimpleX Directory' /approve 1:PSA 1"
        superUser <# "'SimpleX Directory'> > /approve 1:PSA 1"
        superUser <## "      Group approved!"
        bob <# "'SimpleX Directory'> The group ID 1 (PSA) is approved and listed in directory - please moderate it!"
        bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
        bob <## ""
        bob <## "Supported commands:"
        bob <## "/'filter 1' - to configure anti-spam filter."
        bob <## "/'role 1' - to set default member role."
        bob <## "/'link 1' - to view/upgrade group link."
        search bob "privacy" welcomeWithLink'
        search bob "security" welcomeWithLink'
        cath `connectVia` dsLink
        search cath "privacy" welcomeWithLink'
        bob #> "@'SimpleX Directory' /exec /contacts"
        bob <# "'SimpleX Directory'> > /exec /contacts"
        bob <## "      You are not allowed to use this command"
        superUser #> "@'SimpleX Directory' /exec /contacts"
        superUser <# "'SimpleX Directory'> > /exec /contacts"
        superUser <## "      alice (Alice)"
        superUser <## "bob (Bob)"
        superUser <## "cath (Catherine)"
  where
    search u s welcome = do
      u #> ("@'SimpleX Directory' " <> s)
      u <# ("'SimpleX Directory'> > " <> s)
      u <## "      Found 1 group(s)."
      u <# "'SimpleX Directory'> PSA (Privacy, Security & Anonymity)"
      u <## "Welcome message:"
      u <## welcome
      u <## "2 members"
    updateGroupProfile u welcome = do
      u ##> ("/set welcome #PSA " <> welcome)
      u <## "welcome message changed to:"
      u <## welcome
    approvalRequested su welcome grId = do
      su <# "'SimpleX Directory'> bob submitted the group ID 1:"
      su <## "PSA (Privacy, Security & Anonymity)"
      su <## "Welcome message:"
      su <## welcome
      su <## "2 members"
      su <## ""
      su <## "To approve send:"
      su <# ("'SimpleX Directory'> /approve 1:PSA " <> show grId)

testSuspendResume :: HasCallStack => TestParams -> IO ()
testSuspendResume ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      groupFound bob "privacy"
      superUser #> "@'SimpleX Directory' /suspend 1:privacy"
      superUser <# "'SimpleX Directory'> > /suspend 1:privacy"
      superUser <## "      Group suspended!"
      bob <# "'SimpleX Directory'> The group ID 1 (privacy) is suspended and hidden from directory. Please contact the administrators."
      groupNotFound bob "privacy"
      superUser #> "@'SimpleX Directory' /resume 1:privacy"
      superUser <# "'SimpleX Directory'> > /resume 1:privacy"
      superUser <## "      Group listing resumed!"
      bob <# "'SimpleX Directory'> The group ID 1 (privacy) is listed in the directory again!"
      groupFound bob "privacy"
      superUser #> "@'SimpleX Directory' privacy"
      groupFoundN_ "" (Just 1) 2 superUser "privacy"
      superUser #> "@'SimpleX Directory' /link 1:privacy"
      superUser <# "'SimpleX Directory'> > /link 1:privacy"
      superUser <## "      The link to join the group ID 1 (privacy):"
      superUser <##. "https://localhost/g#"
      superUser <## "New member role: member"
      -- get and change the link to the equivalent - should not ask to re-approve
      bob #> "@'SimpleX Directory' /link 1"
      bob <# "'SimpleX Directory'> > /link 1"
      bob <## "      The link to join the group ID 1 (privacy):"
      gLink <- getTermLine bob
      gLink `shouldStartWith` "https://localhost/g#"
      bob <## "New member role: member"
      bob ##> "/show welcome #privacy"
      bob <## "Welcome message:"
      bob <## ("Link to join the group privacy: " <> gLink)
      bob ##> ("/set welcome #privacy Link to join the group privacy: " <> gLink <> "?same_link=true")
      bob <## "welcome message changed to:"
      bob <## ("Link to join the group privacy: " <> gLink <> "?same_link=true")
      bob <# "'SimpleX Directory'> The group ID 1 (privacy) is updated!"
      bob <## "The group is listed in directory."
      superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is updated - only link or whitespace changes."
      superUser <## "The group remained listed in directory."
#if !defined(dbPostgres)
      -- upgrade link
      -- make it upgradeable first
      superUser #> "@'SimpleX Directory' /x /sql chat UPDATE user_contact_links SET short_link_contact = NULL"
      superUser <# "'SimpleX Directory'> > /x /sql chat UPDATE user_contact_links SET short_link_contact = NULL"
      superUser <## ""
      bob #> "@'SimpleX Directory' /link 1"
      bob <# "'SimpleX Directory'> > /link 1"
      bob <## "      The link to join the group ID 1 (privacy):"
      bob <##. "https://simplex.chat/contact#/"
      bob <## "New member role: member"
      bob <## "The link is being upgraded..."
      bob <# "'SimpleX Directory'> Please replace the old link in welcome message of your group ID 1 (privacy)"
      bob <## "If this is the only change, the group will remain listed in directory without re-approval."
      bob <## ""
      bob <## "The new link:"
      gLink' <- dropStrPrefix "'SimpleX Directory'> " . dropTime <$> getTermLine bob
      bob ##> ("/set welcome #privacy Link to join the group privacy: " <> gLink')
      bob <## "welcome message changed to:"
      bob <## ("Link to join the group privacy: " <> gLink')
      bob <# "'SimpleX Directory'> The group ID 1 (privacy) is updated!"
      bob <## "The group is listed in directory."
      superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is updated - only link or whitespace changes."
      superUser <## "The group remained listed in directory."
      -- send message to group owner
      superUser #> "@'SimpleX Directory' /owner 1:privacy hello there"
      superUser <# "'SimpleX Directory'> > /owner 1:privacy hello there"
      superUser <## "      Forwarded to @bob, the owner of the group ID 1 (privacy)"
      bob <# "'SimpleX Directory'> hello there"
#endif

testDeleteGroup :: HasCallStack => TestParams -> IO ()
testDeleteGroup ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      groupFound bob "privacy"
      bob #> "@'SimpleX Directory' /delete 1:privacy"
      bob <# "'SimpleX Directory'> > /delete 1:privacy"
      bob <## "      Your group privacy is deleted from the directory"
      groupNotFound bob "privacy"

testDeleteGroupAdmin :: HasCallStack => TestParams -> IO ()
testDeleteGroupAdmin ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        cath `connectVia` dsLink
        registerGroupId superUser cath "security" "Security" 2 1
        groupFound bob "privacy"
        groupFound bob "security"
        listUserGroup bob "privacy" "Privacy"
        listUserGroup cath "security" "Security"
        superUser #> "@'SimpleX Directory' /last"
        superUser <# "'SimpleX Directory'> > /last"
        superUser <## "      2 registered group(s)"
        memberGroupListing superUser bob 1 "privacy" "Privacy" 2 "active"
        memberGroupListing superUser cath 2 "security" "Security" 2 "active"
        -- trying to register group with the same name
        submitGroup bob "security" "Security"
        bob <# "'SimpleX Directory'> The group security (Security) is already listed in the directory, please choose another name."
        bob ##> "/d #security"
        bob <## "#security: you deleted the group"
        -- admin can delete the group
        superUser #> "@'SimpleX Directory' /delete 2:security"
        superUser <# "'SimpleX Directory'> > /delete 2:security"
        superUser <## "      The group security is deleted from the directory"
        groupFound cath "privacy"
        listUserGroup bob "privacy" "Privacy"
        groupNotFound bob "security"
        sendListCommand cath 0
        -- another user can register the group with the same name
        registerGroupId superUser bob "security" "Security" 4 2

testSetRole :: HasCallStack => TestParams -> IO ()
testSetRole ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        groupFound bob "privacy"
        bob #> "@'SimpleX Directory' /role 1:privacy observer"
        bob <# "'SimpleX Directory'> > /role 1:privacy observer"
        bob <## "      The initial member role for the group privacy is set to observer"
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group"
        cath <#. "#privacy 'SimpleX Directory'> Link to join the group privacy: https://localhost/g#"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"
        bob ##> "/ms #privacy"
        bob <## "bob (Bob): owner, you, created group"
        bob <## "'SimpleX Directory': admin, invited, connected"
        bob <## "cath (Catherine): observer, connected"
        cath ##> "#privacy hello"
        cath <## "#privacy: you don't have permission to send messages"

testJoinGroup :: HasCallStack => TestParams -> IO ()
testJoinGroup ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath ->
        withNewTestChat ps "dan" danProfile $ \dan -> do
          bob `connectVia` dsLink
          registerGroup superUser bob "privacy" "Privacy"
          cath `connectVia` dsLink
          cath #> "@'SimpleX Directory' privacy"
          cath <# "'SimpleX Directory'> > privacy"
          cath <## "      Found 1 group(s)."
          cath <# "'SimpleX Directory'> privacy (Privacy)"
          cath <## "Welcome message:"
          welcomeMsg <- getTermLine cath
          let groupLink = dropStrPrefix "Link to join the group privacy: " welcomeMsg
          cath <## "2 members"
          cath ##> ("/c " <> groupLink)
          cath <## "connection request sent!"
          cath <## "#privacy: joining the group..."
          cath <## "#privacy: you joined the group"
          cath
            <### [ "contact and member are merged: 'SimpleX Directory', #privacy 'SimpleX Directory_1'",
                   "use @'SimpleX Directory' <message> to send messages",
                   Predicate (\l -> l == welcomeMsg || dropTime_ l == Just ("#privacy 'SimpleX Directory'> " <> welcomeMsg) || dropTime_ l == Just ("#privacy 'SimpleX Directory_1'> " <> welcomeMsg))
                 ]
          cath <## "#privacy: member bob (Bob) is connected"
          bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
          bob <## "#privacy: new member cath is connected"
          bob ##> "/create link #privacy"
          bobLink <- getGroupLink bob "privacy" GRMember True
          dan ##> ("/c " <> bobLink)
          dan <## "connection request sent!"
          concurrentlyN_
            [ do
                bob <## "dan (Daniel): accepting request to join group #privacy..."
                bob <## "#privacy: dan joined the group",
              do
                dan <## "#privacy: joining the group..."
                dan <## "#privacy: you joined the group"
                dan <# ("#privacy bob> " <> welcomeMsg)
                dan
                  <### [ "#privacy: member 'SimpleX Directory' is connected",
                         "#privacy: member cath (Catherine) is connected"
                       ],
              do
                cath <## "#privacy: bob added dan (Daniel) to the group (connecting...)"
                cath <## "#privacy: new member dan is connected"
            ]

testGroupNameWithSpaces :: HasCallStack => TestParams -> IO ()
testGroupNameWithSpaces ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "Privacy & Security" ""
      groupFound bob "Privacy & Security"
      superUser #> "@'SimpleX Directory' /suspend 1:'Privacy & Security'"
      superUser <# "'SimpleX Directory'> > /suspend 1:'Privacy & Security'"
      superUser <## "      Group suspended!"
      bob <# "'SimpleX Directory'> The group ID 1 (Privacy & Security) is suspended and hidden from directory. Please contact the administrators."
      groupNotFound bob "privacy"
      superUser #> "@'SimpleX Directory' /resume 1:'Privacy & Security'"
      superUser <# "'SimpleX Directory'> > /resume 1:'Privacy & Security'"
      superUser <## "      Group listing resumed!"
      bob <# "'SimpleX Directory'> The group ID 1 (Privacy & Security) is listed in the directory again!"
      groupFound bob "Privacy & Security"

testSearchGroups :: HasCallStack => TestParams -> IO ()
testSearchGroups ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        forM_ [1..8 :: Int] $ \i -> registerGroupId superUser bob (groups !! (i - 1)) "" i i
        connectUsers bob cath
        fullAddMember "MyGroup" "" bob cath GRMember
        joinGroup "MyGroup" cath bob
        cath <## "#MyGroup: member 'SimpleX Directory_1' is connected"
        cath <## "contact and member are merged: 'SimpleX Directory', #MyGroup 'SimpleX Directory_1'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        cath #> "@'SimpleX Directory' MyGroup"
        cath <# "'SimpleX Directory'> > MyGroup"
        cath <## "      Found 7 group(s), sending top 3."
        receivedGroup cath 0 3
        receivedGroup cath 1 2
        receivedGroup cath 2 2
        cath <# "'SimpleX Directory'> Send /next for 4 more result(s)."
        cath #> "@'SimpleX Directory' /next"
        cath <# "'SimpleX Directory'> > /next"
        cath <## "      Sending 3 more group(s)."
        receivedGroup cath 3 2
        receivedGroup cath 4 2
        receivedGroup cath 5 2
        cath <# "'SimpleX Directory'> Send /next for 1 more result(s)."
        -- search of another user does not affect the search of the first user
        groupFound bob "Another"
        cath #> "@'SimpleX Directory' ."
        cath <# "'SimpleX Directory'> > ."
        cath <## "      Sending 1 more group(s)."
        receivedGroup cath 6 2
        cath #> "@'SimpleX Directory' /all"
        cath <# "'SimpleX Directory'> > /all"
        cath <## "      8 group(s) listed, sending top 3."
        receivedGroup cath 0 3
        receivedGroup cath 1 2
        receivedGroup cath 2 2
        cath <# "'SimpleX Directory'> Send /next for 5 more result(s)."
        cath #> "@'SimpleX Directory' /new"
        cath <# "'SimpleX Directory'> > /new"
        cath <## "      8 group(s) listed, sending the most recent 3."
        receivedGroup cath 7 2
        receivedGroup cath 6 2
        receivedGroup cath 5 2
        cath <# "'SimpleX Directory'> Send /next for 5 more result(s)."
        cath #> "@'SimpleX Directory' term3"
        cath <# "'SimpleX Directory'> > term3"
        cath <## "      Found 3 group(s)."
        receivedGroup cath 4 2
        receivedGroup cath 5 2
        receivedGroup cath 6 2
        cath #> "@'SimpleX Directory' term1"
        cath <# "'SimpleX Directory'> > term1"
        cath <## "      Found 6 group(s), sending top 3."
        receivedGroup cath 1 2
        receivedGroup cath 2 2
        receivedGroup cath 3 2
        cath <# "'SimpleX Directory'> Send /next for 3 more result(s)."
        cath #> "@'SimpleX Directory' ."
        cath <# "'SimpleX Directory'> > ."
        cath <## "      Sending 3 more group(s)."
        receivedGroup cath 4 2
        receivedGroup cath 5 2
        receivedGroup cath 6 2
  where
    groups :: [String]
    groups =
      [ "MyGroup",
        "MyGroup term1 1",
        "MyGroup term1 2",
        "MyGroup term1 term2",
        "MyGroup term1 term2 term3",
        "MyGroup term1 term2 term3 term4",
        "MyGroup term1 term2 term3 term4 term5",
        "Another"
      ]
    receivedGroup :: TestCC -> Int -> Int -> IO ()
    receivedGroup u ix count = do
      u <#. ("'SimpleX Directory'> " <> groups !! ix)
      u <## "Welcome message:"
      u <##. "Link to join the group "
      u <## (show count <> " members")

testInviteToOwnersGroup :: HasCallStack => TestParams -> IO ()
testInviteToOwnersGroup ps =
  withDirectoryServiceCfgOwnersGroup ps testCfg True Nothing $ \superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroupId superUser bob "privacy" "Privacy" 2 1
      bob <## "#owners: 'SimpleX Directory' invites you to join the group as member"
      bob <## "use /j owners to accept"
      superUser <## "Invited @bob, the owner of the group ID 2 (privacy) to owners' group owners"
      bob ##> "/j owners"
      bob <## "#owners: you joined the group"
      bob <## "#owners: member alice (Alice) is connected"
      superUser <## "#owners: 'SimpleX Directory' added bob (Bob) to the group (connecting...)"
      superUser <## "#owners: new member bob is connected"
      -- second group
      registerGroupId superUser bob "security" "Security" 3 2
      superUser <## "Owner is already a member of owners' group"

testDelistedOwnerLeaves :: HasCallStack => TestParams -> IO ()
testDelistedOwnerLeaves ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        leaveGroup "privacy" bob
        cath <## "#privacy: bob left the group"
        bob <# "'SimpleX Directory'> You left the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is de-listed (group owner left)."
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        groupNotFound cath "privacy"

testDelistedOwnerRemoved :: HasCallStack => TestParams -> IO ()
testDelistedOwnerRemoved ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        removeMember "privacy" cath bob
        bob <# "'SimpleX Directory'> You are removed from the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is de-listed (group owner is removed)."
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        groupNotFound cath "privacy"

testNotDelistedMemberLeaves :: HasCallStack => TestParams -> IO ()
testNotDelistedMemberLeaves ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        leaveGroup "privacy" cath
        bob <## "#privacy: cath left the group"
        (superUser </)
        cath `connectVia` dsLink
        cath #> "@'SimpleX Directory_1' privacy"
        groupFoundN_ "_1" Nothing 2 cath "privacy"

testNotDelistedMemberRemoved :: HasCallStack => TestParams -> IO ()
testNotDelistedMemberRemoved ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        removeMember "privacy" bob cath
        (superUser </)
        cath `connectVia` dsLink
        cath #> "@'SimpleX Directory_1' privacy"
        groupFoundN_ "_1" Nothing 2 cath "privacy"

testDelistedServiceRemoved :: HasCallStack => TestParams -> IO ()
testDelistedServiceRemoved ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        bob ##> "/rm #privacy 'SimpleX Directory'"
        bob <## "#privacy: you removed 'SimpleX Directory' from the group"
        cath <## "#privacy: bob removed 'SimpleX Directory' from the group"
        bob <# "'SimpleX Directory'> SimpleX Directory is removed from the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is de-listed (directory service is removed)."
        cath `connectVia` dsLink
        groupNotFound_ "_1" cath "privacy"

testDelistedGroupDeleted :: HasCallStack => TestParams -> IO ()
testDelistedGroupDeleted ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        connectUsers bob cath
        fullAddMember "privacy" "Privacy" bob cath GROwner
        joinGroup "privacy" cath bob
        cath <## "#privacy: member 'SimpleX Directory_1' is connected"
        cath <## "contact and member are merged: 'SimpleX Directory', #privacy 'SimpleX Directory_1'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        bob ##> "/d #privacy"
        bob <## "#privacy: you deleted the group"
        bob <# "'SimpleX Directory'> The group ID 1 (privacy) is deleted."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        cath <## "#privacy: bob deleted the group"
        cath <## "use /d #privacy to delete the local copy of the group"
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is de-listed (group is deleted)."
        groupNotFound cath "privacy"

testDelistedRoleChanges :: HasCallStack => TestParams -> IO ()
testDelistedRoleChanges ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        groupFoundN 3 cath "privacy"
        -- de-listed if service role changed
        bob ##> "/mr privacy 'SimpleX Directory' member"
        bob <## "#privacy: you changed the role of 'SimpleX Directory' to member"
        cath <## "#privacy: bob changed the role of 'SimpleX Directory' from admin to member"
        bob <# "'SimpleX Directory'> SimpleX Directory role in the group ID 1 (privacy) is changed to member."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is de-listed (SimpleX Directory role is changed to member)."
        groupNotFound cath "privacy"
        -- re-listed if service role changed back without profile changes
        cath ##> "/mr privacy 'SimpleX Directory' admin"
        cath <## "#privacy: you changed the role of 'SimpleX Directory' to admin"
        bob <## "#privacy: cath changed the role of 'SimpleX Directory' from member to admin"
        bob <# "'SimpleX Directory'> SimpleX Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is listed in the directory again."
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is listed (SimpleX Directory role is changed to admin)."
        groupFoundN 3 cath "privacy"
        -- de-listed if owner role changed
        cath ##> "/mr privacy bob admin"
        cath <## "#privacy: you changed the role of bob to admin"
        bob <## "#privacy: cath changed your role from owner to admin"
        bob <# "'SimpleX Directory'> Your role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is de-listed (user role is set to admin)."
        groupNotFound cath "privacy"
        -- re-listed if owner role changed back without profile changes
        cath ##> "/mr privacy bob owner"
        cath <## "#privacy: you changed the role of bob to owner"
        bob <## "#privacy: cath changed your role from admin to owner"
        bob <# "'SimpleX Directory'> Your role in the group ID 1 (privacy) is changed to owner."
        bob <## ""
        bob <## "The group is listed in the directory again."
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is listed (user role is set to owner)."
        groupFoundN 3 cath "privacy"

testNotDelistedMemberRoleChanged :: HasCallStack => TestParams -> IO ()
testNotDelistedMemberRoleChanged ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        groupFoundN 3 cath "privacy"
        bob ##> "/mr privacy cath member"
        bob <## "#privacy: you changed the role of cath to member"
        cath <## "#privacy: bob changed your role from owner to member"
        groupFoundN 3 cath "privacy"

testNotSentApprovalBadRoles :: HasCallStack => TestParams -> IO ()
testNotSentApprovalBadRoles ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        welcomeWithLink <- groupAccepted bob "privacy"
        bob ##> "/mr privacy 'SimpleX Directory' member"
        bob <## "#privacy: you changed the role of 'SimpleX Directory' to member"
        updateProfileWithLink bob "privacy" welcomeWithLink 1
        bob <# "'SimpleX Directory'> You must grant directory service admin role to register the group"
        bob ##> "/mr privacy 'SimpleX Directory' admin"
        bob <## "#privacy: you changed the role of 'SimpleX Directory' to admin"
        bob <# "'SimpleX Directory'> SimpleX Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is submitted for approval."
        notifySuperUser superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupNotFound cath "privacy"
        approveRegistration superUser bob "privacy" 1
        groupFound cath "privacy"

testNotApprovedBadRoles :: HasCallStack => TestParams -> IO ()
testNotApprovedBadRoles ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        welcomeWithLink <- groupAccepted bob "privacy"
        updateProfileWithLink bob "privacy" welcomeWithLink 1
        notifySuperUser superUser bob "privacy" "Privacy" welcomeWithLink 1
        bob ##> "/mr privacy 'SimpleX Directory' member"
        bob <## "#privacy: you changed the role of 'SimpleX Directory' to member"
        let approve = "/approve 1:privacy 1"
        superUser #> ("@'SimpleX Directory' " <> approve)
        superUser <# ("'SimpleX Directory'> > " <> approve)
        superUser <## "      Group is not approved: SimpleX Directory is not an admin."
        groupNotFound cath "privacy"
        bob ##> "/mr privacy 'SimpleX Directory' admin"
        bob <## "#privacy: you changed the role of 'SimpleX Directory' to admin"
        bob <# "'SimpleX Directory'> SimpleX Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is submitted for approval."
        notifySuperUser superUser bob "privacy" "Privacy" welcomeWithLink 1
        approveRegistration superUser bob "privacy" 1
        groupFound cath "privacy"

testRegOwnerChangedProfile :: HasCallStack => TestParams -> IO ()
testRegOwnerChangedProfile ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        bob ##> "/gp privacy privacy Privacy and Security"
        bob <## "description changed to: Privacy and Security"
        bob <# "'SimpleX Directory'> The group ID 1 (privacy) is updated!"
        bob <## "It is hidden from the directory until approved."
        cath <## "bob updated group #privacy:"
        cath <## "description changed to: Privacy and Security"
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        groupNotFound cath "privacy"
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is updated."
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testAnotherOwnerChangedProfile :: HasCallStack => TestParams -> IO ()
testAnotherOwnerChangedProfile ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        cath ##> "/gp privacy privacy Privacy and Security"
        cath <## "description changed to: Privacy and Security"
        bob <## "cath updated group #privacy:"
        bob <## "description changed to: Privacy and Security"
        bob <# "'SimpleX Directory'> The group ID 1 (privacy) is updated by cath!"
        bob <## "It is hidden from the directory until approved."
        groupNotFound cath "privacy"
        superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is updated by cath."
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testNotConnectedOwnerChangedProfile :: HasCallStack => TestParams -> IO ()
testNotConnectedOwnerChangedProfile ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          bob `connectVia` dsLink
          dan `connectVia` dsLink
          registerGroup superUser bob "privacy" "Privacy"
          addCathAsOwner bob cath
          cath ##> "/gp privacy privacy Privacy and Security"
          cath <## "description changed to: Privacy and Security"
          bob <## "cath updated group #privacy:"
          bob <## "description changed to: Privacy and Security"
          bob <# "'SimpleX Directory'> The group ID 1 (privacy) is updated by cath!"
          bob <## "It is hidden from the directory until approved."
          groupNotFound dan "privacy"
          superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is updated by cath."
          reapproveGroup 3 superUser bob
          groupFoundN 3 dan "privacy"

testRegOwnerRemovedLink :: HasCallStack => TestParams -> IO ()
testRegOwnerRemovedLink ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        bob ##> "/show welcome #privacy"
        bob <## "Welcome message:"
        welcomeWithLink <- getTermLine bob
        bob ##> "/set welcome #privacy Welcome!"
        bob <## "welcome message changed to:"
        bob <## "Welcome!"
        bob <# "'SimpleX Directory'> The group link for ID 1 (privacy) is removed from the welcome message."
        bob <## ""
        bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
        cath <## "bob updated group #privacy:"
        cath <## "welcome message changed to:"
        cath <## "Welcome!"
        superUser <# "'SimpleX Directory'> The group link is removed from ID 1 (privacy), de-listed."
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        groupNotFound cath "privacy"
        let withChangedLink = T.unpack $ T.replace "contact#/?v=2-7&" "contact#/?v=3-7&" $ T.pack welcomeWithLink
        bob ##> ("/set welcome #privacy " <> withChangedLink)
        bob <## "welcome message changed to:"
        bob <## withChangedLink
        bob <# "'SimpleX Directory'> Thank you! The group link for ID 1 (privacy) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        cath <## "bob updated group #privacy:"
        cath <## "welcome message changed to:"
        cath <## withChangedLink
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testAnotherOwnerRemovedLink :: HasCallStack => TestParams -> IO ()
testAnotherOwnerRemovedLink ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        bob ##> "/show welcome #privacy"
        bob <## "Welcome message:"
        welcomeWithLink <- getTermLine bob
        cath ##> "/set welcome #privacy Welcome!"
        cath <## "welcome message changed to:"
        cath <## "Welcome!"
        bob <## "cath updated group #privacy:"
        bob <## "welcome message changed to:"
        bob <## "Welcome!"
        bob <# "'SimpleX Directory'> The group link for ID 1 (privacy) is removed from the welcome message by cath."
        bob <## ""
        bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
        superUser <# "'SimpleX Directory'> The group link is removed from ID 1 (privacy), de-listed."
        groupNotFound cath "privacy"
        cath ##> ("/set welcome #privacy " <> welcomeWithLink)
        cath <## "welcome message changed to:"
        cath <## welcomeWithLink
        bob <## "cath updated group #privacy:"
        bob <## "welcome message changed to:"
        bob <## welcomeWithLink
        bob <# "'SimpleX Directory'> Thank you! The group link for ID 1 (privacy) is added to the welcome message by cath."
        bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testNotConnectedOwnerRemovedLink :: HasCallStack => TestParams -> IO ()
testNotConnectedOwnerRemovedLink ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        withNewTestChat ps "dan" danProfile $ \dan -> do
          bob `connectVia` dsLink
          dan `connectVia` dsLink
          registerGroup superUser bob "privacy" "Privacy"
          addCathAsOwner bob cath
          bob ##> "/show welcome #privacy"
          bob <## "Welcome message:"
          welcomeWithLink <- getTermLine bob
          cath ##> "/set welcome #privacy Welcome!"
          cath <## "welcome message changed to:"
          cath <## "Welcome!"
          bob <## "cath updated group #privacy:"
          bob <## "welcome message changed to:"
          bob <## "Welcome!"
          bob <# "'SimpleX Directory'> The group link for ID 1 (privacy) is removed from the welcome message by cath."
          bob <## ""
          bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
          superUser <# "'SimpleX Directory'> The group link is removed from ID 1 (privacy), de-listed."
          groupNotFound dan "privacy"
          cath ##> ("/set welcome #privacy " <> welcomeWithLink)
          cath <## "welcome message changed to:"
          cath <## welcomeWithLink
          bob <## "cath updated group #privacy:"
          bob <## "welcome message changed to:"
          bob <## welcomeWithLink
          -- bob <# "'SimpleX Directory'> The group link is added by another group member, your registration will not be processed."
          -- bob <## ""
          -- bob <## "Please update the group profile yourself."
          -- bob ##> ("/set welcome #privacy " <> welcomeWithLink <> " - welcome!")
          -- bob <## "welcome message changed to:"
          -- bob <## (welcomeWithLink <> " - welcome!")
          bob <# "'SimpleX Directory'> Thank you! The group link for ID 1 (privacy) is added to the welcome message by cath."
          bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
          -- cath <## "bob updated group #privacy:"
          -- cath <## "welcome message changed to:"
          -- cath <## (welcomeWithLink <> " - welcome!")
          reapproveGroup 3 superUser bob
          groupFoundN 3 dan "privacy"

testDuplicateAskConfirmation :: HasCallStack => TestParams -> IO ()
testDuplicateAskConfirmation ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        _ <- groupAccepted bob "privacy"
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        cath #> "@'SimpleX Directory' /confirm 1:privacy"
        welcomeWithLink <- groupAccepted cath "privacy"
        groupNotFound bob "privacy"
        completeRegistrationId superUser cath "privacy" "Privacy" welcomeWithLink 2 1
        groupFound bob "privacy"

testDuplicateProhibitRegistration :: HasCallStack => TestParams -> IO ()
testDuplicateProhibitRegistration ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        cath `connectVia` dsLink
        groupFound cath "privacy"
        _ <- submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already listed in the directory, please choose another name."

testDuplicateProhibitConfirmation :: HasCallStack => TestParams -> IO ()
testDuplicateProhibitConfirmation ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        welcomeWithLink <- groupAccepted bob "privacy"
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        groupNotFound cath "privacy"
        completeRegistration superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupFound cath "privacy"
        cath #> "@'SimpleX Directory' /confirm 1:privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already listed in the directory, please choose another name."

testDuplicateProhibitWhenUpdated :: HasCallStack => TestParams -> IO ()
testDuplicateProhibitWhenUpdated ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        welcomeWithLink <- groupAccepted bob "privacy"
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        cath #> "@'SimpleX Directory' /confirm 1:privacy"
        welcomeWithLink' <- groupAccepted cath "privacy"
        groupNotFound cath "privacy"
        completeRegistration superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupFound cath "privacy"
        cath ##> ("/set welcome privacy " <> welcomeWithLink')
        cath <## "welcome message changed to:"
        cath <## welcomeWithLink'
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already listed in the directory, please choose another name."
        cath ##> "/gp privacy security Security"
        cath <## "changed to #security (Security)"
        cath <# "'SimpleX Directory'> Thank you! The group link for ID 1 (security) is added to the welcome message."
        cath <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        notifySuperUser superUser cath "security" "Security" welcomeWithLink' 2
        approveRegistrationId superUser cath "security" 2 1
        groupFound bob "security"
        groupFound cath "security"

testDuplicateProhibitApproval :: HasCallStack => TestParams -> IO ()
testDuplicateProhibitApproval ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        welcomeWithLink <- groupAccepted bob "privacy"
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        cath #> "@'SimpleX Directory' /confirm 1:privacy"
        welcomeWithLink' <- groupAccepted cath "privacy"
        updateProfileWithLink cath "privacy" welcomeWithLink' 1
        notifySuperUser superUser cath "privacy" "Privacy" welcomeWithLink' 2
        groupNotFound cath "privacy"
        completeRegistration superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupFound cath "privacy"
        -- fails at approval, as already listed
        let approve = "/approve 2:privacy 1"
        superUser #> ("@'SimpleX Directory' " <> approve)
        superUser <# ("'SimpleX Directory'> > " <> approve)
        superUser <## "      The group ID 2 (privacy) is already listed in the directory."

testListUserGroups :: HasCallStack => Bool -> TestParams -> IO ()
testListUserGroups promote ps =
  withDirectoryServiceCfgOwnersGroup ps testCfg False (Just "./tests/tmp/web") $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        checkListings ["privacy"] []
        connectUsers bob cath
        fullAddMember "privacy" "Privacy" bob cath GRMember
        joinGroup "privacy" cath bob
        cath <## "#privacy: member 'SimpleX Directory_1' is connected"
        cath <## "contact and member are merged: 'SimpleX Directory', #privacy 'SimpleX Directory_1'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        registerGroupId superUser bob "security" "Security" 2 2
        checkListings ["privacy", "security"] []
        registerGroupId superUser cath "anonymity" "Anonymity" 3 1
        checkListings ["privacy", "security", "anonymity"] []
        listUserGroup cath "anonymity" "Anonymity"
        -- with de-listed group
        groupFound cath "anonymity"
        cath ##> "/mr anonymity 'SimpleX Directory' member"
        cath <## "#anonymity: you changed the role of 'SimpleX Directory' to member"
        cath <# "'SimpleX Directory'> SimpleX Directory role in the group ID 1 (anonymity) is changed to member."
        cath <## ""
        cath <## "The group is no longer listed in the directory."
        superUser <# "'SimpleX Directory'> The group ID 3 (anonymity) is de-listed (SimpleX Directory role is changed to member)."
        checkListings ["privacy", "security"] []
        groupNotFound cath "anonymity"
        listGroups superUser bob cath
        when promote $ do
          superUser #> "@'SimpleX Directory' /promote 1:privacy on"
          superUser <# "'SimpleX Directory'> > /promote 1:privacy on"
          superUser <## "      Group promotion enabled."
          checkListings ["privacy", "security"] ["privacy"]
          bob ##> "/gp privacy privacy"
          bob <## "description removed"
          bob <# "'SimpleX Directory'> The group ID 1 (privacy) is updated!"
          bob <## "It is hidden from the directory until approved."
          cath <## "bob updated group #privacy:"
          cath <## "description removed"
          superUser <# "'SimpleX Directory'> The group ID 1 (privacy) is updated."
          superUser <# "'SimpleX Directory'> bob submitted the group ID 1:"
          superUser <## "privacy"
          superUser <## "Welcome message:"
          superUser <##. "Link to join the group privacy: https://localhost/g#"
          superUser <## "3 members"
          superUser <## ""
          superUser <## "To approve send:"
          superUser <# "'SimpleX Directory'> /approve 1:privacy 1 promote=on"
          checkListings ["security"] []
          superUser #> "@'SimpleX Directory' /approve 1:privacy 1"
          superUser <# "'SimpleX Directory'> > /approve 1:privacy 1"
          superUser <## "      Group approved (promoted)!"
          bob <# "'SimpleX Directory'> The group ID 1 (privacy) is approved and listed in directory - please moderate it!"
          bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
          bob <## ""
          bob <## "Supported commands:"
          bob <## "/'filter 1' - to configure anti-spam filter."
          bob <## "/'role 1' - to set default member role."
          bob <## "/'link 1' - to view/upgrade group link."
          checkListings ["privacy", "security"] ["privacy"]

checkListings :: HasCallStack => [T.Text] -> [T.Text] -> IO ()
checkListings listed promoted = do
  threadDelay 100000
  checkListing listingFileName listed
  checkListing promotedFileName promoted
  where
    checkListing f expected = do
      Just (DirectoryListing gs) <- J.decodeFileStrict $ "./tests/tmp/web/data" </> f
      map groupName gs `shouldBe` expected
    groupName DirectoryEntry {displayName} = displayName

testCapthaScreening :: HasCallStack => TestParams -> IO ()
testCapthaScreening ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        -- check default role
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        -- enable captcha
        bob #> "@'SimpleX Directory' /filter 1 captcha"
        bob <# "'SimpleX Directory'> > /filter 1 captcha"
        bob <## "      Spam filter settings for group privacy set to:"
        bob <## "- reject long/inappropriate names: disabled"
        bob <## "- pass captcha to join: enabled"
        bob <## ""
        bob <## "/'filter 1 name' - enable name filter"
        bob <## "/'filter 1 name captcha' - enable both"
        bob <## "/'filter 1 off' - disable filter"
        -- connect with captcha screen
        _ <- join cath groupLink
        cath #> "#privacy (support) 123" -- sending incorrect captcha
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath 123"
        cath <## "      Incorrect text, please try again."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        sendCaptcha cath captcha
        cath <#. "#privacy 'SimpleX Directory'> Link to join the group privacy: https://"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"
        cath #> "#privacy hello"
        bob <# "#privacy cath> hello"
        cath ##> "/l privacy"
        cath <## "#privacy: you left the group"
        cath <## "use /d #privacy to delete the group"
        bob <## "#privacy: cath left the group"
        cath ##> "/d #privacy"
        cath <## "#privacy: you deleted the group"
        -- change default role to observer
        bob #> "@'SimpleX Directory' /role 1 observer"
        bob <# "'SimpleX Directory'> > /role 1 observer"
        bob <## "      The initial member role for the group privacy is set to observer"
        bob <## ""
        bob <##. "Please note: it applies only to members joining via this link: https://"
        -- connect with captcha screen again, as observer
        captcha' <- join cath groupLink
        sendCaptcha cath captcha'
        -- message from cath that left
        pastMember <- dropStrPrefix "#privacy: 'SimpleX Directory' forwarded a message from an unknown member, creating unknown member record " <$> getTermLine cath
        cath <# ("#privacy " <> pastMember <> "> hello [>>]")
        cath <#. "#privacy 'SimpleX Directory'> Link to join the group privacy: https://"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath_1 (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath_1 is connected"
        cath ##> "#privacy hello"
        cath <## "#privacy: you don't have permission to send messages"
        (bob </)
        cath ##> "/ms privacy"
        cath <## "cath (Catherine): observer, you, connected"
        cath <## "'SimpleX Directory': admin, host, connected"
        cath <## "bob (Bob): owner, connected"
        cath <## (pastMember <> ": author, status unknown")
  where
    join cath groupLink = do
      cath ##> ("/c " <> groupLink)
      cath <## "connection request sent!"
      cath <## "#privacy: joining the group..."
      cath <## "#privacy: you joined the group, pending approval"
      cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
      cath <## ""
      cath <## "Send captcha text to join the group privacy."
      dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
    sendCaptcha cath captcha = do
      cath #> ("#privacy (support) " <> captcha)
      cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
      cath <## "      Correct, you joined the group privacy"
      cath <## "#privacy: you joined the group"

testVoiceCaptchaScreening :: HasCallStack => TestParams -> IO ()
testVoiceCaptchaScreening ps@TestParams {tmpPath} = do
  let mockScript = tmpPath </> "mock_voice_gen.py"
  -- Mock script writes a dummy audio file, prints path and duration
  writeFile mockScript $ unlines
    [ "#!/usr/bin/env python3",
      "import os, tempfile",
      "out = os.environ.get('VOICE_CAPTCHA_OUT')",
      "if not out:",
      "    fd, out = tempfile.mkstemp(suffix='.m4a')",
      "    os.close(fd)",
      "open(out, 'wb').write(b'\\x00' * 100)",
      "print(out)",
      "print(5)"
    ]
  setPermissions mockScript $ setOwnerExecutable True $ setOwnerReadable True $ setOwnerWritable True emptyPermissions
  withDirectoryServiceVoiceCaptcha ps mockScript $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        -- get group link
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        -- enable captcha
        bob #> "@'SimpleX Directory' /filter 1 captcha"
        bob <# "'SimpleX Directory'> > /filter 1 captcha"
        bob <## "      Spam filter settings for group privacy set to:"
        bob <## "- reject long/inappropriate names: disabled"
        bob <## "- pass captcha to join: enabled"
        bob <## ""
        bob <## "/'filter 1 name' - enable name filter"
        bob <## "/'filter 1 name captcha' - enable both"
        bob <## "/'filter 1 off' - disable filter"
        -- cath joins, receives text captcha with /audio hint
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        cath <## "Send /'audio' to receive a voice captcha."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        -- cath requests audio captcha
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 1"
        -- send correct captcha
        sendCaptcha cath captcha
        cath <#. "#privacy 'SimpleX Directory'> Link to join the group privacy: https://"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"
  where
    sendCaptcha cath captcha = do
      cath #> ("#privacy (support) " <> captcha)
      cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
      cath <## "      Correct, you joined the group privacy"
      cath <## "#privacy: you joined the group"

testVoiceCaptchaRetry :: HasCallStack => TestParams -> IO ()
testVoiceCaptchaRetry ps@TestParams {tmpPath} = do
  let mockScript = tmpPath </> "mock_voice_gen_retry.py"
  writeFile mockScript $ unlines
    [ "#!/usr/bin/env python3",
      "import os, tempfile",
      "out = os.environ.get('VOICE_CAPTCHA_OUT')",
      "if not out:",
      "    fd, out = tempfile.mkstemp(suffix='.m4a')",
      "    os.close(fd)",
      "open(out, 'wb').write(b'\\x00' * 100)",
      "print(out)",
      "print(5)"
    ]
  setPermissions mockScript $ setOwnerExecutable True $ setOwnerReadable True $ setOwnerWritable True emptyPermissions
  withDirectoryServiceVoiceCaptcha ps mockScript $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        bob #> "@'SimpleX Directory' /filter 1 captcha"
        bob <# "'SimpleX Directory'> > /filter 1 captcha"
        bob <## "      Spam filter settings for group privacy set to:"
        bob <## "- reject long/inappropriate names: disabled"
        bob <## "- pass captcha to join: enabled"
        bob <## ""
        bob <## "/'filter 1 name' - enable name filter"
        bob <## "/'filter 1 name captcha' - enable both"
        bob <## "/'filter 1 off' - disable filter"
        -- cath joins, receives text captcha with /audio hint
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        cath <## "Send /'audio' to receive a voice captcha."
        _ <- getTermLine cath -- captcha image/text
        -- cath requests audio captcha
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 1"
        -- cath sends WRONG answer after switching to audio mode
        cath #> "#privacy (support) wrong_answer"
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath wrong_answer"
        cath <## "      Incorrect text, please try again."
        -- KEY ASSERTION: retry sends VOICE captcha (not image) because captchaMode=CMAudio
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 2"

withDirectoryServiceVoiceCaptcha :: HasCallStack => TestParams -> FilePath -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceVoiceCaptcha ps voiceScript test = do
  dsLink <-
    withNewTestChatCfg ps testCfg serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChatCfg ps testCfg "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        ds ##> "/ad"
        getContactLink ds True
  let opts = (mkDirectoryOpts ps [KnownContact 2 "alice"] Nothing Nothing) {voiceCaptchaGenerator = Just voiceScript}
  runDirectory testCfg opts $
    withTestChatCfg ps testCfg "super_user" $ \superUser -> do
      superUser <## "subscribed 1 connections on server localhost"
      test superUser dsLink

testRestoreDirectory :: HasCallStack => TestParams -> IO ()
testRestoreDirectory ps = do
  testListUserGroups False ps
  restoreDirectoryService ps 11 $ \superUser _dsLink ->
    withTestChat ps "bob" $ \bob ->
      withTestChat ps "cath" $ \cath -> do
        bob <## "subscribed 5 connections on server localhost"
        cath <## "subscribed 5 connections on server localhost"
        listGroups superUser bob cath
        groupFoundN 3 bob "privacy"
        groupFound bob "security"
        groupFoundN 3 cath "privacy"
        cath #> "@'SimpleX Directory' security"
        groupFoundN' 2 cath "security"

testCaptcha :: HasCallStack => TestParams -> IO ()
testCaptcha _ps = do
  let captcha = "23456789ABCDEFGHIJKLMNOPQRSTUVWXYZabdefghijkmnpqrty"
  matchCaptchaStr captcha captcha `shouldBe` True
  matchCaptchaStr captcha "23456789ABcDEFGH1JKLMNoPQRsTuvwxYzabdefghijkmnpqrty" `shouldBe` True
  matchCaptchaStr "23456789ABcDEFGH1JKLMNoPQRsTuvwxYzabdefghijkmnpqrty" captcha `shouldBe` True
  matchCaptchaStr "OOIICPSUVWXZ" "OOIICPSUVWXZ" `shouldBe` True
  matchCaptchaStr "OOIICPSUVWXZ" "0o1lcpsuvwxz" `shouldBe` True
  matchCaptchaStr "0o1lcpsuvwxz" "OOIICPSUVWXZ" `shouldBe` True
  matchCaptchaStr "OOIICPSUVWXZ" "" `shouldBe` False
  matchCaptchaStr "OOIICPSUVWXZ" "0o1lcpsuvwx" `shouldBe` False
  matchCaptchaStr "OOIICPSUVWXZ" "0o1lcpsuvwxzz" `shouldBe` False

listGroups :: HasCallStack => TestCC -> TestCC -> TestCC -> IO ()
listGroups superUser bob cath = do
  sendListCommand bob 2
  groupListing bob 1 "privacy" "Privacy" 3 "active"
  groupListing bob 2 "security" "Security" 2 "active"
  sendListCommand cath 1
  groupListing cath 1 "anonymity" "Anonymity" 2 "suspended because roles changed"
  -- superuser lists all groups
  bob #> "@'SimpleX Directory' /last"
  bob <# "'SimpleX Directory'> > /last"
  bob <## "      You are not allowed to use this command"
  superUser #> "@'SimpleX Directory' /last"
  superUser <# "'SimpleX Directory'> > /last"
  superUser <## "      3 registered group(s)"
  memberGroupListing superUser bob 1 "privacy" "Privacy" 3 "active"
  memberGroupListing superUser bob 2 "security" "Security" 2 "active"
  memberGroupListing superUser cath 3 "anonymity" "Anonymity" 2 "suspended because roles changed"
  -- showing last 1 group
  superUser #> "@'SimpleX Directory' /last 1"
  superUser <# "'SimpleX Directory'> > /last 1"
  superUser <## "      3 registered group(s), showing the last 1"
  memberGroupListing superUser cath 3 "anonymity" "Anonymity" 2 "suspended because roles changed"

listUserGroup :: HasCallStack => TestCC -> String -> String -> IO ()
listUserGroup u n fn = do
  sendListCommand u 1
  groupListing u 1 n fn 2 "active"

sendListCommand :: HasCallStack => TestCC -> Int -> IO ()
sendListCommand u count = do
  u #> "@'SimpleX Directory' /list"
  u <# "'SimpleX Directory'> > /list"
  u <## ("      " <> show count <> " registered group(s)")

groupListing :: HasCallStack => TestCC -> Int -> String -> String -> Int -> String -> IO ()
groupListing u = groupListing_ u Nothing

memberGroupListing :: HasCallStack => TestCC -> TestCC -> Int -> String -> String -> Int -> String -> IO ()
memberGroupListing su owner = groupListing_ su (Just owner)

groupListing_ :: HasCallStack => TestCC -> Maybe TestCC -> Int -> String -> String -> Int -> String -> IO ()
groupListing_ su owner_ gId n fn count status = do
  su <# ("'SimpleX Directory'> " <> show gId <> ". " <> n <> " (" <> fn <> ")")
  su <## "Welcome message:"
  su <##. ("Link to join the group " <> n <> ": ")
  forM_ owner_ $ \owner -> do
    ownerName <- userName owner
    su <## ("Owner: " <> ownerName)
  su <## (show count <> " members")
  su <## ("Status: " <> status)
  su <## ("/'role " <> show gId <> "', /'filter " <> show gId <> "'")

reapproveGroup :: HasCallStack => Int -> TestCC -> TestCC -> IO ()
reapproveGroup count superUser bob = do
  superUser <# "'SimpleX Directory'> bob submitted the group ID 1:"
  superUser <##. "privacy ("
  superUser <## "Welcome message:"
  superUser <##. "Link to join the group privacy: "
  superUser <## (show count <> " members")
  superUser <## ""
  superUser <## "To approve send:"
  superUser <# "'SimpleX Directory'> /approve 1:privacy 1"
  superUser #> "@'SimpleX Directory' /approve 1:privacy 1"
  superUser <# "'SimpleX Directory'> > /approve 1:privacy 1"
  superUser <## "      Group approved!"
  bob <# "'SimpleX Directory'> The group ID 1 (privacy) is approved and listed in directory - please moderate it!"
  bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
  bob <## ""
  bob <## "Supported commands:"
  bob <## "/'filter 1' - to configure anti-spam filter."
  bob <## "/'role 1' - to set default member role."
  bob <## "/'link 1' - to view/upgrade group link."

addCathAsOwner :: HasCallStack => TestCC -> TestCC -> IO ()
addCathAsOwner bob cath = do
  connectUsers bob cath
  fullAddMember "privacy" "Privacy" bob cath GROwner
  joinGroup "privacy" cath bob
  cath <## "#privacy: member 'SimpleX Directory' is connected"

withDirectoryService :: HasCallStack => TestParams -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryService ps = withDirectoryServiceCfg ps testCfg

withDirectoryServiceCfg :: HasCallStack => TestParams -> ChatConfig -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceCfg ps cfg = withDirectoryServiceCfgOwnersGroup ps cfg False Nothing

withDirectoryServiceCfgOwnersGroup :: HasCallStack => TestParams -> ChatConfig -> Bool -> Maybe FilePath -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceCfgOwnersGroup ps cfg createOwnersGroup webFolder test = do
  dsLink <-
    withNewTestChatCfg ps cfg serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChatCfg ps cfg "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        when createOwnersGroup $ do
          superUser ##> "/g owners"
          superUser <## "group #owners is created"
          superUser <## "to add members use /a owners <name> or /create link #owners"
          superUser ##> "/a owners 'SimpleX Directory' admin"
          superUser <## "invitation to join the group #owners sent to 'SimpleX Directory'"
          ds <## "#owners: alice invites you to join the group as admin"
          ds <## "use /j owners to accept"
          ds ##> "/j owners"
          ds <## "#owners: you joined the group"
          superUser <## "#owners: 'SimpleX Directory' joined the group"
        ds ##> "/ad"
        getContactLink ds True
  withDirectoryOwnersGroup ps cfg dsLink createOwnersGroup webFolder test

restoreDirectoryService :: HasCallStack => TestParams -> Int -> (TestCC -> String -> IO ()) -> IO ()
restoreDirectoryService ps connCount test = do
  dsLink <-
    withTestChat ps serviceDbPrefix $ \ds -> do
      ds .<## ("subscribed " <> show connCount <> " connections on server localhost")
      ds ##> "/sa"
      dsLink <- getContactLink ds False
      ds <## "auto_accept on"
      pure dsLink
  withDirectory ps testCfg dsLink test

withDirectory :: HasCallStack => TestParams -> ChatConfig -> String -> (TestCC -> String -> IO ()) -> IO ()
withDirectory ps cfg dsLink = withDirectoryOwnersGroup ps cfg dsLink False Nothing

withDirectoryOwnersGroup :: HasCallStack => TestParams -> ChatConfig -> String -> Bool -> Maybe FilePath -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryOwnersGroup ps cfg dsLink createOwnersGroup webFolder test = do
  let opts = mkDirectoryOpts ps [KnownContact 2 "alice"] (if createOwnersGroup then Just $ KnownGroup 1 "owners" else Nothing) webFolder
  runDirectory cfg opts $
    withTestChatCfg ps cfg "super_user" $ \superUser -> do
      if createOwnersGroup
        then superUser <## "subscribed 2 connections on server localhost"
        else superUser <## "subscribed 1 connections on server localhost"
      test superUser dsLink

runDirectory :: ChatConfig -> DirectoryOpts -> IO () -> IO ()
runDirectory cfg opts@DirectoryOpts {directoryLog} action = do
  st <- openDirectoryLog directoryLog
  t <- forkIO $ directoryService st opts cfg
  threadDelay 500000
  action `finally` (mapM_ hClose (directoryLogFile st) >> killThread t)

registerGroup :: TestCC -> TestCC -> String -> String -> IO ()
registerGroup su u n fn = registerGroupId su u n fn 1 1

registerGroupId :: TestCC -> TestCC -> String -> String -> Int -> Int -> IO ()
registerGroupId su u n fn gId ugId = do
  submitGroup u n fn
  welcomeWithLink <- groupAccepted u n
  completeRegistrationId su u n fn welcomeWithLink gId ugId

submitGroup :: TestCC -> String -> String -> IO ()
submitGroup u n fn = do
  u ##> ("/g " <> viewName n <> if null fn then "" else " " <> fn)
  u <## ("group #" <> viewName n <> (if null fn then "" else " (" <> fn <> ")") <> " is created")
  u <## ("to add members use /a " <> viewName n <> " <name> or /create link #" <> viewName n)
  u ##> ("/a " <> viewName n <> " 'SimpleX Directory' admin")
  u <## ("invitation to join the group #" <> viewName n <> " sent to 'SimpleX Directory'")

groupAccepted :: TestCC -> String -> IO String
groupAccepted u n = do
  u <###
    [ WithTime ("'SimpleX Directory'> Joining the group " <> n <> "…"),
      ConsoleString ("#" <> viewName n <> ": 'SimpleX Directory' joined the group")
    ]
  u <# ("'SimpleX Directory'> Joined the group " <> n <> ", creating the link…")
  u <# "'SimpleX Directory'> Created the public link to join the group via this directory service that is always online."
  u <## ""
  u <## "Please add it to the group welcome message."
  u <## "For example, add:"
  dropStrPrefix "'SimpleX Directory'> " . dropTime <$> getTermLine u -- welcome message with link

completeRegistration :: TestCC -> TestCC -> String -> String -> String -> Int -> IO ()
completeRegistration su u n fn welcomeWithLink gId =
  completeRegistrationId su u n fn welcomeWithLink gId gId

completeRegistrationId :: TestCC -> TestCC -> String -> String -> String -> Int -> Int -> IO ()
completeRegistrationId su u n fn welcomeWithLink gId ugId = do
  updateProfileWithLink u n welcomeWithLink ugId
  notifySuperUser su u n fn welcomeWithLink gId
  approveRegistrationId su u n gId ugId

updateProfileWithLink :: TestCC -> String -> String -> Int -> IO ()
updateProfileWithLink u n welcomeWithLink ugId = do
  u ##> ("/set welcome " <> viewName n <> " " <> welcomeWithLink)
  u <## "welcome message changed to:"
  u <## welcomeWithLink
  u <# ("'SimpleX Directory'> Thank you! The group link for ID " <> show ugId <> " (" <> n <> ") is added to the welcome message.")
  u <## "You will be notified once the group is added to the directory - it may take up to 48 hours."

notifySuperUser :: TestCC -> TestCC -> String -> String -> String -> Int -> IO ()
notifySuperUser su u n fn welcomeWithLink gId = do
  uName <- userName u
  su <# ("'SimpleX Directory'> " <> uName <> " submitted the group ID " <> show gId <> ":")
  su <## (n <> if null fn then "" else " (" <> fn <> ")")
  su <## "Welcome message:"
  su <## welcomeWithLink
  su .<## "members"
  su <## ""
  su <## "To approve send:"
  let approve = "/approve " <> show gId <> ":" <> viewName n <> " 1"
  su <# ("'SimpleX Directory'> " <> approve)

approveRegistration :: TestCC -> TestCC -> String -> Int -> IO ()
approveRegistration su u n gId =
  approveRegistrationId su u n gId gId

approveRegistrationId :: TestCC -> TestCC -> String -> Int -> Int -> IO ()
approveRegistrationId su u n gId ugId = do
  let approve = "/approve " <> show gId <> ":" <> viewName n <> " 1"
  su #> ("@'SimpleX Directory' " <> approve)
  su <# ("'SimpleX Directory'> > " <> approve)
  su <## "      Group approved!"
  u <# ("'SimpleX Directory'> The group ID " <> show ugId <> " (" <> n <> ") is approved and listed in directory - please moderate it!")
  u <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
  u <## ""
  u <## "Supported commands:"
  u <## ("/'filter " <> show ugId <> "' - to configure anti-spam filter.")
  u <## ("/'role " <> show ugId <> "' - to set default member role.")
  u <## ("/'link " <> show ugId <> "' - to view/upgrade group link.")

connectVia :: TestCC -> String -> IO ()
u `connectVia` dsLink = do
  u ##> ("/c " <> dsLink)
  u <## "connection request sent!"
  u .<## ": contact is connected"
  u .<# "> Welcome to SimpleX Directory!"
  u <## ""
  u <## "🔍 Send search string to find groups - try security."
  u <## "/help - how to submit your group."
  u <## "/new - recent groups."
  u <## ""
  u <## "[Directory rules](https://simplex.chat/docs/directory.html)."

joinGroup :: String -> TestCC -> TestCC -> IO ()
joinGroup gName member host = do
  let gn = "#" <> gName
  memberName <- userName member
  hostName <- userName host
  member ##> ("/j " <> gName)
  member <## (gn <> ": you joined the group")
  member <#. (gn <> " " <> hostName <> "> Link to join the group " <> gName <> ": ")
  host <## (gn <> ": " <> memberName <> " joined the group")

leaveGroup :: String -> TestCC -> IO ()
leaveGroup gName member = do
  let gn = "#" <> gName
  member ##> ("/l " <> gName)
  member <## (gn <> ": you left the group")
  member <## ("use /d " <> gn <> " to delete the group")

removeMember :: String -> TestCC -> TestCC -> IO ()
removeMember gName admin removed = do
  let gn = "#" <> gName
  adminName <- userName admin
  removedName <- userName removed
  admin ##> ("/rm " <> gName <> " " <> removedName)
  admin <## (gn <> ": you removed " <> removedName <> " from the group")
  removed <## (gn <> ": " <> adminName <> " removed you from the group")
  removed <## ("use /d " <> gn <> " to delete the group")

groupFound :: TestCC -> String -> IO ()
groupFound = groupFoundN 2

groupFoundN :: Int -> TestCC -> String -> IO ()
groupFoundN count u name = do
  u #> ("@'SimpleX Directory' " <> name)
  groupFoundN' count u name

groupFoundN' :: Int -> TestCC -> String -> IO ()
groupFoundN' = groupFoundN_ "" Nothing

groupFoundN_ :: String -> Maybe Int -> Int -> TestCC -> String -> IO ()
groupFoundN_ suffix shownId_ count u name = do
  u <# ("'SimpleX Directory" <> suffix <> "'> > " <> name)
  u <## "      Found 1 group(s)."
  u <#. ("'SimpleX Directory" <> suffix <> "'> " <> maybe "" (\gId -> show gId <> ". ") shownId_ <> name)
  u <## "Welcome message:"
  u <##. "Link to join the group "
  u <## (show count <> " members")

groupNotFound :: TestCC -> String -> IO ()
groupNotFound = groupNotFound_ ""

groupNotFound_ :: String -> TestCC -> String -> IO ()
groupNotFound_ suffix u s = do
  u #> ("@'SimpleX Directory" <> suffix <> "' " <> s)
  u <# ("'SimpleX Directory" <> suffix <> "'> > " <> s)
  u <## "      No groups found"

testCaptchaTooManyAttempts :: HasCallStack => TestParams -> IO ()
testCaptchaTooManyAttempts ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        bob #> "@'SimpleX Directory' /filter 1 captcha"
        bob <# "'SimpleX Directory'> > /filter 1 captcha"
        bob <## "      Spam filter settings for group privacy set to:"
        bob <## "- reject long/inappropriate names: disabled"
        bob <## "- pass captcha to join: enabled"
        bob <## ""
        bob <## "/'filter 1 name' - enable name filter"
        bob <## "/'filter 1 name captcha' - enable both"
        bob <## "/'filter 1 off' - disable filter"
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        _ <- getTermLine cath
        forM_ [1 :: Int .. 4] $ \i -> do
          cath #> "#privacy (support) wrong"
          cath <# "#privacy (support) 'SimpleX Directory'!> > cath wrong"
          if i == 4
            then cath <## "      Incorrect text, please try again - this is your last attempt."
            else cath <## "      Incorrect text, please try again."
          _ <- getTermLine cath
          pure ()
        cath #> "#privacy (support) wrong"
        cath <# "#privacy (support) 'SimpleX Directory'> Too many failed attempts, you can't join group."
        -- member removal produces multiple messages
        _ <- getTermLine cath
        _ <- getTermLine cath
        _ <- getTermLine cath
        pure ()

testCaptchaUnknownCommand :: HasCallStack => TestParams -> IO ()
testCaptchaUnknownCommand ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        bob #> "@'SimpleX Directory' /filter 1 captcha"
        bob <# "'SimpleX Directory'> > /filter 1 captcha"
        bob <## "      Spam filter settings for group privacy set to:"
        bob <## "- reject long/inappropriate names: disabled"
        bob <## "- pass captcha to join: enabled"
        bob <## ""
        bob <## "/'filter 1 name' - enable name filter"
        bob <## "/'filter 1 name captcha' - enable both"
        bob <## "/'filter 1 off' - disable filter"
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        _ <- getTermLine cath
        cath #> "#privacy (support) /help"
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath /help"
        cath <## "      Unknown command, please enter captcha text."

testBlockedWords :: HasCallStack => TestParams -> IO ()
testBlockedWords _ps = do
  let cfg = BlockedWordsConfig
        { blockedWords = S.fromList ["spam", "scam"],
          blockedFragments = S.empty,
          extensionRules = [],
          spelling = M.fromList [('@', "a@"), ('4', "a4"), ('$', "s$"), ('5', "s5")]
        }
  hasBlockedWords cfg "hello world" `shouldBe` False
  hasBlockedWords cfg "this is spam" `shouldBe` True
  hasBlockedWords cfg "sp@m here" `shouldBe` True
  hasBlockedWords cfg "$pam here" `shouldBe` True
  hasBlockedWords cfg "5p4m here" `shouldBe` True
  hasBlockedWords cfg "SPAM" `shouldBe` True
  hasBlockedWords cfg "scam alert" `shouldBe` True
  hasBlockedWords cfg "$c4m" `shouldBe` True
  hasBlockedWords cfg "spamm" `shouldBe` False
  hasBlockedWords cfg "scammer" `shouldBe` False
  -- field selectors for BlockedWordsConfig
  S.member "spam" (blockedWords cfg) `shouldBe` True
  blockedFragments cfg `shouldBe` S.empty
  extensionRules cfg `shouldBe` []
  spelling cfg `shouldSatisfy` (not . M.null)

testBlockedFragments :: HasCallStack => TestParams -> IO ()
testBlockedFragments _ps = do
  let cfg = BlockedWordsConfig
        { blockedWords = S.empty,
          blockedFragments = S.fromList ["spam", "scam"],
          extensionRules = [],
          spelling = M.fromList [('@', "a@"), ('4', "a4"), ('$', "s$"), ('5', "s5")]
        }
  hasBlockedFragments cfg "hello world" `shouldBe` False
  hasBlockedFragments cfg "this is spam" `shouldBe` True
  hasBlockedFragments cfg "spammer" `shouldBe` True
  hasBlockedFragments cfg "scammer" `shouldBe` True
  hasBlockedFragments cfg "5p4m" `shouldBe` True
  hasBlockedFragments cfg "$c4m" `shouldBe` True

testHelpCommands :: HasCallStack => TestParams -> IO ()
testHelpCommands ps =
  withDirectoryService ps $ \_ dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      bob #> "@'SimpleX Directory' /help commands"
      bob <# "'SimpleX Directory'> /'help commands' - receive this help message."
      bob <## "/help - how to register your group to be added to directory."
      bob <## "/list - list the groups you registered."
      bob <## "`/role <ID>` - view and set default member role for your group."
      bob <## "`/filter <ID>` - view and set spam filter settings for group."
      bob <## "`/link <ID>` - view and upgrade group link."
      bob <## "`/delete <ID>:<NAME>` - remove the group you submitted from directory, with ID and name as shown by /list command."
      bob <## ""
      bob <## "To search for groups, send the search text."

testHelpNoAudio :: HasCallStack => TestParams -> IO ()
testHelpNoAudio ps =
  withDirectoryService ps $ \_ dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      -- commands help should not mention /audio
      bob #> "@'SimpleX Directory' /help commands"
      bob <# "'SimpleX Directory'> /'help commands' - receive this help message."
      bob <## "/help - how to register your group to be added to directory."
      bob <## "/list - list the groups you registered."
      bob <## "`/role <ID>` - view and set default member role for your group."
      bob <## "`/filter <ID>` - view and set spam filter settings for group."
      bob <## "`/link <ID>` - view and upgrade group link."
      bob <## "`/delete <ID>:<NAME>` - remove the group you submitted from directory, with ID and name as shown by /list command."
      bob <## ""
      bob <## "To search for groups, send the search text."

testAudioCommandInDM :: HasCallStack => TestParams -> IO ()
testAudioCommandInDM ps =
  withDirectoryService ps $ \_ dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      bob #> "@'SimpleX Directory' /audio"
      bob <# "'SimpleX Directory'> > /audio"
      bob <## "      Unknown command"

testRemoveTriples :: HasCallStack => TestParams -> IO ()
testRemoveTriples _ps = do
  removeTriples "hello" `shouldBe` "hello"
  removeTriples "helllo" `shouldBe` "hello"
  removeTriples "heeelllo" `shouldBe` "heello"
  removeTriples "aaa" `shouldBe` "aa"
  removeTriples "aaaa" `shouldBe` "aa"
  removeTriples "aaaabbbcccc" `shouldBe` "aabbcc"
  removeTriples "" `shouldBe` ""
  removeTriples "a" `shouldBe` "a"
  removeTriples "aa" `shouldBe` "aa"
  -- Exercise initial False argument (prev='\0' matches first char '\0',
  -- so samePrev=False is forced in the first iteration)
  removeTriples "\0\0\0" `shouldBe` "\0"
  removeTriples "\0" `shouldBe` "\0"

testWordVariants :: HasCallStack => TestParams -> IO ()
testWordVariants _ps = do
  let rules = [("er", ["a", ""]), ("ing", [""])]
  wordVariants [] "spam" `shouldBe` [T.pack "spam"]
  wordVariants rules "spammer" `shouldBe` [T.pack "spammer", T.pack "spamma", T.pack "spamm"]
  wordVariants rules "running" `shouldBe` [T.pack "running", T.pack "runn"]
  wordVariants [("ph", ["f"])] "phish" `shouldBe` [T.pack "phish", T.pack "fish"]

testBlockedWordsDoubleSpacer :: HasCallStack => TestParams -> IO ()
testBlockedWordsDoubleSpacer _ps = do
  -- normalizeText removes spaces, so "hel lo" (single-spaced) normalizes to "hello"
  -- Double-space acts as a hard word boundary for ws2 check
  let cfg = BlockedWordsConfig
        { blockedWords = S.fromList ["hello"],
          blockedFragments = S.empty,
          extensionRules = [],
          spelling = M.empty
        }
  -- "hel lo" (no double space): ws2 = ["hel lo"] -> ["hello"] -> match
  hasBlockedWords cfg "hel lo" `shouldBe` True
  -- "hel lo  world": ws2 = ["hel lo", "world"] -> ["hello", "world"] -> match
  hasBlockedWords cfg "hel lo  world" `shouldBe` True
  -- "hel  lo": ws2 = ["hel", "lo"] -> neither matches "hello"
  hasBlockedWords cfg "hel  lo" `shouldBe` False
  -- Direct match
  hasBlockedWords cfg "hello" `shouldBe` True

testAdminSuspendResume :: HasCallStack => TestParams -> IO ()
testAdminSuspendResume ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      groupFound bob "privacy"
      let suspend = "/suspend 1:privacy"
      superUser #> ("@'SimpleX Directory' " <> suspend)
      superUser <# ("'SimpleX Directory'> > " <> suspend)
      superUser <## "      Group suspended!"
      bob <# "'SimpleX Directory'> The group ID 1 (privacy) is suspended and hidden from directory. Please contact the administrators."
      groupNotFound bob "privacy"
      let resume = "/resume 1:privacy"
      superUser #> ("@'SimpleX Directory' " <> resume)
      superUser <# ("'SimpleX Directory'> > " <> resume)
      superUser <## "      Group listing resumed!"
      bob <# "'SimpleX Directory'> The group ID 1 (privacy) is listed in the directory again!"
      groupFound bob "privacy"

-- Note: /reject command is not implemented in the service, so no test for it

testAdminListGroups :: HasCallStack => TestParams -> IO ()
testAdminListGroups ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      superUser #> "@'SimpleX Directory' /last"
      superUser <# "'SimpleX Directory'> > /last"
      superUser <## "      1 registered group(s)"
      -- Group listing format: name, welcome message, link, owner, members, status, commands
      superUser <# "'SimpleX Directory'> 1. privacy (Privacy)"
      superUser <## "Welcome message:"
      superUser <##. "Link to join the group privacy:"
      superUser <## "Owner: bob"
      superUser <## "2 members"
      superUser <## "Status: active"
      superUser <## "/'role 1', /'filter 1'"
      superUser #> "@'SimpleX Directory' /pending"
      superUser <# "'SimpleX Directory'> > /pending"
      superUser <## "      0 registered group(s)"

testAdminSendToOwner :: HasCallStack => TestParams -> IO ()
testAdminSendToOwner ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      let ownerMsg = "/owner 1:privacy Hello from admin!"
      superUser #> ("@'SimpleX Directory' " <> ownerMsg)
      superUser <# ("'SimpleX Directory'> > " <> ownerMsg)
      superUser <## "      Forwarded to @bob, the owner of the group ID 1 (privacy)"
      bob <# "'SimpleX Directory'> Hello from admin!"

testCommandParsing :: HasCallStack => TestParams -> IO ()
testCommandParsing _ps = do
  parseCmd "/help" `shouldBe` "help"
  parseCmd "/help registration" `shouldBe` "help"
  parseCmd "/help commands" `shouldBe` "help"
  parseCmd "/h" `shouldBe` "help"
  parseCmd "/all" `shouldBe` "all"
  parseCmd "/new" `shouldBe` "new"
  parseCmd "/next" `shouldBe` "next"
  parseCmd "." `shouldBe` "next"
  parseCmd "/list" `shouldBe` "list"
  parseCmd "/ls" `shouldBe` "list"
  parseCmd "security" `shouldBe` "search"
  parseCmd "/approve 1:test 1" `shouldBe` "approve"
  parseCmd "/reject 1:test" `shouldBe` "reject"
  parseCmd "/suspend 1:test" `shouldBe` "suspend"
  parseCmd "/resume 1:test" `shouldBe` "resume"
  parseCmd "/last" `shouldBe` "last"
  parseCmd "/last 5" `shouldBe` "last"
  parseCmd "/pending" `shouldBe` "pending"
  parseCmd "/owner 1:test Hello" `shouldBe` "owner"
  parseCmd "/exec echo test" `shouldBe` "exec"
  parseCmd "/x echo test" `shouldBe` "exec"
  parseCmd "/unknown_cmd" `shouldBe` "unknown"
  parseCmd "/delete 1:test" `shouldBe` "delete"
  parseCmd "/role 1" `shouldBe` "role"
  parseCmd "/role 1 member" `shouldBe` "role"
  parseCmd "/filter 1" `shouldBe` "filter"
  parseCmd "/filter 1 off" `shouldBe` "filter"
  parseCmd "/filter 1 basic" `shouldBe` "filter"
  parseCmd "/audio" `shouldBe` "unknown"
  parseCmd "/confirm 1:test" `shouldBe` "confirm"
  parseCmd "/link 1" `shouldBe` "link"
  parseCmd "/promote 1:test on" `shouldBe` "promote"
  parseCmd "/invite 1:test" `shouldBe` "invite"
  where
    parseCmd :: T.Text -> T.Text
    parseCmd t = getCmdTag $ parseDirectoryCmd $ T.dropWhileEnd isSpace t
    getCmdTag (ADC _ cmd) = directoryCmdTag cmd
    parseDirectoryCmd t = case A.parseOnly (directoryCmdP <* A.endOfInput) t of
      Right cmd -> cmd
      Left _ -> ADC SDRUser DCUnknownCommand

testRecentRoundedTime :: HasCallStack => TestParams -> IO ()
testRecentRoundedTime _ps = do
  let baseTime = UTCTime (fromGregorian 2024 1 15) 43200
      recentTime = addUTCTime (-86400 :: NominalDiffTime) baseTime
      oldTime = addUTCTime (-40 * 86400 :: NominalDiffTime) baseTime
  recentRoundedTime 900 baseTime recentTime `shouldSatisfy` isJust
  recentRoundedTime 900 baseTime oldTime `shouldBe` Nothing
  recentRoundedTime 86400 baseTime recentTime `shouldSatisfy` isJust
  where
    isJust (Just _) = True
    isJust Nothing = False

testGroupRegStatusEncoding :: HasCallStack => TestParams -> IO ()
testGroupRegStatusEncoding _ps = do
  roundTrip GRSPendingConfirmation
  roundTrip GRSProposed
  roundTrip GRSPendingUpdate
  roundTrip (GRSPendingApproval 42)
  roundTrip GRSActive
  roundTrip GRSSuspended
  roundTrip GRSSuspendedBadRoles
  roundTrip GRSRemoved
  where
    roundTrip :: GroupRegStatus -> IO ()
    roundTrip s = strDecode (strEncode s) `shouldBe` Right s

testDirectoryLogRecordEncoding :: HasCallStack => TestParams -> IO ()
testDirectoryLogRecordEncoding _ps = do
  let gr = GroupReg
        { dbGroupId = 1,
          userGroupRegId = 1,
          dbContactId = 2,
          dbOwnerMemberId = Just 3,
          groupRegStatus = GRSActive,
          promoted = True,
          createdAt = UTCTime (fromGregorian 2024 1 1) 0
        }
  roundTrip (GRCreate gr)
  roundTrip (GRDelete 1)
  roundTrip (GRUpdateStatus 1 GRSActive)
  roundTrip (GRUpdateStatus 1 (GRSPendingApproval 5))
  roundTrip (GRUpdatePromotion 1 True)
  roundTrip (GRUpdatePromotion 1 False)
  roundTrip (GRUpdateOwner 1 42)
  where
    roundTrip :: DirectoryLogRecord -> IO ()
    roundTrip r = case strDecode (strEncode r) :: Either String DirectoryLogRecord of
      Right _ -> pure ()
      Left e -> expectationFailure $ "Failed to decode: " <> e

testNormalizeText :: HasCallStack => TestParams -> IO ()
testNormalizeText _ps = do
  let sp = M.fromList [('a', ['a', '@']), ('e', ['e', '3']), ('o', ['o', '0'])]
  normalizeText sp "Hello" `shouldSatisfy` any (== "hello")
  normalizeText sp "H.e" `shouldSatisfy` any (== "he")
  normalizeText sp "H e" `shouldSatisfy` any (== "he")
  normalizeText sp "café" `shouldSatisfy` any (== "cafe")
  normalizeText M.empty "" `shouldBe` [""]

testAllSubstitutions :: HasCallStack => TestParams -> IO ()
testAllSubstitutions _ps = do
  let sp = M.fromList [('a', ['a', '@'])]
  allSubstitutions sp "ab" `shouldBe` ["ab", "@b"]
  allSubstitutions sp "aa" `shouldBe` ["aa", "a@", "@a", "@@"]
  allSubstitutions M.empty "xy" `shouldBe` ["xy"]
  allSubstitutions sp "" `shouldBe` [""]

testCommandParsingEdgeCases :: HasCallStack => TestParams -> IO ()
testCommandParsingEdgeCases _ps = do
  parseCmd "/help" `shouldShowAs` "ADC SDRUser (DCHelp DHSRegistration)"
  parseCmd "/help commands" `shouldShowAs` "ADC SDRUser (DCHelp DHSCommands)"
  parseCmd "/help c" `shouldShowAs` "ADC SDRUser (DCHelp DHSCommands)"
  parseCmd "/help r" `shouldShowAs` "ADC SDRUser (DCHelp DHSRegistration)"
  parseCmd "/next" `shouldShowAs` "ADC SDRUser DCSearchNext"
  parseCmd "." `shouldShowAs` "ADC SDRUser DCSearchNext"
  parseCmd "/all" `shouldShowAs` "ADC SDRUser DCAllGroups"
  parseCmd "/new" `shouldShowAs` "ADC SDRUser DCRecentGroups"
  parseCmd "/list" `shouldShowAs` "ADC SDRUser DCListUserGroups"
  parseCmd "/ls" `shouldShowAs` "ADC SDRUser DCListUserGroups"
  parseCmd "/last" `shouldShowAs` "ADC SDRAdmin (DCListLastGroups 10)"
  parseCmd "/last 5" `shouldShowAs` "ADC SDRAdmin (DCListLastGroups 5)"
  parseCmd "/pending" `shouldShowAs` "ADC SDRAdmin (DCListPendingGroups 10)"
  parseCmd "/pending 3" `shouldShowAs` "ADC SDRAdmin (DCListPendingGroups 3)"
  parseCmd "/badcommand" `shouldShowAs` "ADC SDRUser DCUnknownCommand"
  parseCmd "search term" `shouldShowAs` "ADC SDRUser (DCSearchGroup \"search term\")"
  parseCmd "/exec ls" `shouldShowAs` "ADC SDRSuperUser (DCExecuteCommand \"ls\")"
  parseCmd "/x ls" `shouldShowAs` "ADC SDRSuperUser (DCExecuteCommand \"ls\")"
  parseCmd "/audio" `shouldShowAs` "ADC SDRUser DCUnknownCommand"
  parseCmd "/suspend 1:test" `shouldShowAs` "ADC SDRAdmin (DCSuspendGroup 1 \"test\")"
  parseCmd "/resume 1:test" `shouldShowAs` "ADC SDRAdmin (DCResumeGroup 1 \"test\")"
  parseCmd "/reject 1:test" `shouldShowAs` "ADC SDRAdmin (DCRejectGroup 1 \"test\")"
  parseCmd "/delete 1:test" `shouldShowAs` "ADC SDRUser (DCDeleteGroup 1 \"test\")"
  parseCmd "/confirm 1:test" `shouldShowAs` "ADC SDRUser (DCConfirmDuplicateGroup 1 \"test\")"
  parseCmd "/role 1" `shouldShowAs` "ADC SDRUser (DCMemberRole 1 Nothing Nothing)"
  parseCmd "/role 1:test member" `shouldShowAs` "ADC SDRUser (DCMemberRole 1 (Just \"test\") (Just GRMember))"
  parseCmd "/role 1:test observer" `shouldShowAs` "ADC SDRUser (DCMemberRole 1 (Just \"test\") (Just GRObserver))"
  parseCmd "/filter 1:test off" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 (Just \"test\") (Just (DirectoryMemberAcceptance {rejectNames = Nothing, passCaptcha = Nothing, makeObserver = Nothing})))"
  parseCmd "/filter 1:test basic" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 (Just \"test\") (Just (DirectoryMemberAcceptance {rejectNames = Just PCNoImage, passCaptcha = Nothing, makeObserver = Nothing})))"
  parseCmd "/link 1:test" `shouldShowAs` "ADC SDRUser (DCShowUpgradeGroupLink 1 (Just \"test\"))"
  parseCmd "/link 1" `shouldShowAs` "ADC SDRUser (DCShowUpgradeGroupLink 1 Nothing)"
  parseCmd "/invite 1:test" `shouldShowAs` "ADC SDRAdmin (DCInviteOwnerToGroup 1 \"test\")"
  parseCmd "/promote 1:test on" `shouldShowAs` "ADC SDRSuperUser (DCPromoteGroup 1 \"test\" True)"
  parseCmd "/promote 1:test off" `shouldShowAs` "ADC SDRSuperUser (DCPromoteGroup 1 \"test\" False)"
  parseCmd "/owner 1:test hello" `shouldShowAs` "ADC SDRAdmin (DCSendToGroupOwner 1 \"test\" \"hello\")"
  parseCmd "/filter 1:test moderate" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 (Just \"test\") (Just (DirectoryMemberAcceptance {rejectNames = Just PCAll, passCaptcha = Just PCNoImage, makeObserver = Nothing})))"
  parseCmd "/filter 1:test mod" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 (Just \"test\") (Just (DirectoryMemberAcceptance {rejectNames = Just PCAll, passCaptcha = Just PCNoImage, makeObserver = Nothing})))"
  parseCmd "/filter 1:test strong" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 (Just \"test\") (Just (DirectoryMemberAcceptance {rejectNames = Just PCAll, passCaptcha = Just PCAll, makeObserver = Nothing})))"
  -- "/" alone: tagP reads "" -> fails, fallback to DCUnknownCommand (line 191)
  parseCmd "/" `shouldShowAs` "ADC SDRUser DCUnknownCommand"
  -- filter with name=all exercises "=all" $> PCAll branch (line 270)
  parseCmd "/filter 1 name=all" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 Nothing (Just (DirectoryMemberAcceptance {rejectNames = Just PCAll, passCaptcha = Nothing, makeObserver = Nothing})))"
  -- /submit without args (covers DCSubmitGroup_ tag)
  getCmdTag "/submit" `shouldBe` "error"
  -- /submit with invalid link: DCCommandError fallback doesn't consume remaining input,
  -- so `endOfInput` fails and the whole parse returns Left -> "unknown".
  -- This matches production behavior (Service.hs:636, Events.hs:97 use the same pattern).
  getCmdTag "/submit invalidlink" `shouldBe` "unknown"
  -- missing args falls back to DCCommandError, exercises directoryCmdTag "error" branch
  getCmdTag "/approve" `shouldBe` "error"
  getCmdTag "/delete" `shouldBe` "error"
  getCmdTag "/suspend" `shouldBe` "error"
  -- filter with name=noimage
  parseCmd "/filter 1 name=noimage" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 Nothing (Just (DirectoryMemberAcceptance {rejectNames = Just PCNoImage, passCaptcha = Nothing, makeObserver = Nothing})))"
  -- filter with both name=noimage and captcha=noimage
  parseCmd "/filter 1 name=noimage captcha=noimage" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 Nothing (Just (DirectoryMemberAcceptance {rejectNames = Just PCNoImage, passCaptcha = Just PCNoImage, makeObserver = Nothing})))"
  -- filter with all three custom conditions
  parseCmd "/filter 1 name captcha observer" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 Nothing (Just (DirectoryMemberAcceptance {rejectNames = Just PCAll, passCaptcha = Just PCAll, makeObserver = Just PCAll})))"
  -- filter with name=no_image alternate spelling
  parseCmd "/filter 1 name=no_image" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 Nothing (Just (DirectoryMemberAcceptance {rejectNames = Just PCNoImage, passCaptcha = Nothing, makeObserver = Nothing})))"
  -- filter with name=no-image alternate spelling
  parseCmd "/filter 1 name=no-image" `shouldShowAs` "ADC SDRUser (DCGroupFilter 1 Nothing (Just (DirectoryMemberAcceptance {rejectNames = Just PCNoImage, passCaptcha = Nothing, makeObserver = Nothing})))"
  -- approve with promote=on
  parseCmd "/approve 1:test 1 promote=on" `shouldShowAs` "ADC SDRAdmin (DCApproveGroup {groupId = 1, displayName = \"test\", groupApprovalId = 1, promote = Just True})"
  -- approve with promote=off
  parseCmd "/approve 1:test 1 promote=off" `shouldShowAs` "ADC SDRAdmin (DCApproveGroup {groupId = 1, displayName = \"test\", groupApprovalId = 1, promote = Just False})"
  -- approve without promote
  parseCmd "/approve 1:test 1" `shouldShowAs` "ADC SDRAdmin (DCApproveGroup {groupId = 1, displayName = \"test\", groupApprovalId = 1, promote = Nothing})"
  where
    parseCmd :: T.Text -> ADirectoryCmd
    parseCmd t = case A.parseOnly (directoryCmdP <* A.endOfInput) t of
      Right cmd -> cmd
      Left _ -> ADC SDRUser DCUnknownCommand
    shouldShowAs :: ADirectoryCmd -> String -> IO ()
    shouldShowAs cmd expected = show cmd `shouldBe` expected
    getCmdTag :: T.Text -> T.Text
    getCmdTag t = case A.parseOnly (directoryCmdP <* A.endOfInput) t of
      Right (ADC _ cmd) -> directoryCmdTag cmd
      Left _ -> "unknown"

testCrDirectoryEvent :: HasCallStack => TestParams -> IO ()
testCrDirectoryEvent _ps = do
  crDirectoryEvent (Left $ ChatErrorAgent (BROKER "addr" (NETWORK (NEConnectError "err"))) (AgentConnId "") Nothing) `shouldSatisfy` isNothing
  crDirectoryEvent (Left $ ChatErrorAgent (BROKER "addr" TIMEOUT) (AgentConnId "") Nothing) `shouldSatisfy` isNothing
  crDirectoryEvent (Left $ ChatError CENoActiveUser) `shouldSatisfy` isJust
  crDirectoryEvent (Right CEvtChatSuspended) `shouldSatisfy` isNothing
  -- CEvtChatErrors path
  crDirectoryEvent (Right $ CEvtChatErrors {chatErrors = [ChatError CENoActiveUser]}) `shouldSatisfy` isJust
  where
    isJust (Just _) = True
    isJust Nothing = False
    isNothing Nothing = True
    isNothing _ = False

testToFormattedText :: HasCallStack => TestParams -> IO ()
testToFormattedText _ps = do
  toFormattedText "hello" `shouldBe` [MD.FormattedText Nothing "hello"]
  toFormattedText "*bold*" `shouldBe` [MD.FormattedText (Just MD.Bold) "bold"]
  toFormattedText "" `shouldBe` [MD.FormattedText Nothing ""]

testListingPaths :: HasCallStack => TestParams -> IO ()
testListingPaths _ps = do
  directoryDataPath `shouldBe` "data"
  listingFileName `shouldBe` "listing.json"
  promotedFileName `shouldBe` "promoted.json"
  listingImageFolder `shouldBe` "images"

testGroupRegEncoding :: HasCallStack => TestParams -> IO ()
testGroupRegEncoding _ps = do
  roundTrip grBase
  roundTrip grBase {promoted = True}
  roundTrip grBase {groupRegStatus = GRSPendingApproval 7}
  roundTrip grBase {dbOwnerMemberId = Nothing}
  where
    grBase = GroupReg
      { dbGroupId = 10,
        userGroupRegId = 2,
        dbContactId = 5,
        dbOwnerMemberId = Just 8,
        groupRegStatus = GRSActive,
        promoted = False,
        createdAt = UTCTime (fromGregorian 2024 6 1) 0
      }
    roundTrip :: GroupReg -> IO ()
    roundTrip gr = case strDecode (strEncode gr) of
      Right gr' -> do
        dbGroupId gr' `shouldBe` dbGroupId gr
        userGroupRegId gr' `shouldBe` userGroupRegId (gr :: GroupReg)
        dbContactId gr' `shouldBe` dbContactId (gr :: GroupReg)
        dbOwnerMemberId gr' `shouldBe` dbOwnerMemberId (gr :: GroupReg)
        groupRegStatus gr' `shouldBe` groupRegStatus (gr :: GroupReg)
        promoted gr' `shouldBe` promoted (gr :: GroupReg)
      Left e -> expectationFailure $ "Failed to decode GroupReg: " <> e

testGroupRegStatusText :: HasCallStack => TestParams -> IO ()
testGroupRegStatusText _ps = do
  groupRegStatusText GRSPendingConfirmation `shouldBe` "pending confirmation (duplicate names)"
  groupRegStatusText GRSProposed `shouldBe` "proposed"
  groupRegStatusText GRSPendingUpdate `shouldBe` "pending profile update"
  groupRegStatusText (GRSPendingApproval 1) `shouldBe` "pending admin approval"
  groupRegStatusText GRSActive `shouldBe` "active"
  groupRegStatusText GRSSuspended `shouldBe` "suspended by admin"
  groupRegStatusText GRSSuspendedBadRoles `shouldBe` "suspended because roles changed"
  groupRegStatusText GRSRemoved `shouldBe` "removed"

testGrDirectoryStatus :: HasCallStack => TestParams -> IO ()
testGrDirectoryStatus _ps = do
  (grDirectoryStatus GRSActive == DSListed) `shouldBe` True
  (grDirectoryStatus GRSSuspended == DSReserved) `shouldBe` True
  (grDirectoryStatus GRSSuspendedBadRoles == DSReserved) `shouldBe` True
  (grDirectoryStatus GRSRemoved == DSRemoved) `shouldBe` True
  (grDirectoryStatus GRSProposed == DSRegistered) `shouldBe` True
  (grDirectoryStatus GRSPendingUpdate == DSRegistered) `shouldBe` True
  (grDirectoryStatus GRSPendingConfirmation == DSRegistered) `shouldBe` True
  (grDirectoryStatus (GRSPendingApproval 1) == DSRegistered) `shouldBe` True

testPendingApproval :: HasCallStack => TestParams -> IO ()
testPendingApproval _ps = do
  pendingApproval (GRSPendingApproval 1) `shouldBe` True
  pendingApproval GRSActive `shouldBe` False
  pendingApproval GRSSuspended `shouldBe` False
  pendingApproval GRSRemoved `shouldBe` False
  pendingApproval GRSProposed `shouldBe` False

testGroupRemoved :: HasCallStack => TestParams -> IO ()
testGroupRemoved _ps = do
  groupRemoved GRSRemoved `shouldBe` True
  groupRemoved GRSActive `shouldBe` False
  groupRemoved GRSSuspended `shouldBe` False
  groupRemoved (GRSPendingApproval 1) `shouldBe` False

testFilterPresets :: HasCallStack => TestParams -> IO ()
testFilterPresets _ps = do
  noJoinFilter `shouldBe` DirectoryMemberAcceptance Nothing Nothing Nothing
  basicJoinFilter `shouldBe` DirectoryMemberAcceptance (Just PCNoImage) Nothing Nothing
  moderateJoinFilter `shouldBe` DirectoryMemberAcceptance (Just PCAll) (Just PCNoImage) Nothing
  strongJoinFilter `shouldBe` DirectoryMemberAcceptance (Just PCAll) (Just PCAll) Nothing

testCustomData :: HasCallStack => TestParams -> IO ()
testCustomData _ps = do
  let dgd = DirectoryGroupData {memberAcceptance = basicJoinFilter}
      cd = toCustomData dgd
      dgd' = fromCustomData (Just cd)
  memberAcceptance dgd' `shouldBe` basicJoinFilter
  let dgdNone = fromCustomData Nothing
  memberAcceptance dgdNone `shouldBe` noJoinFilter
  let dgdEmpty = fromCustomData (Just $ CustomData mempty)
  memberAcceptance dgdEmpty `shouldBe` noJoinFilter

testReadDirectoryLogData :: HasCallStack => TestParams -> IO ()
testReadDirectoryLogData ps = do
  let logFile = tmpPath ps </> "test_read_dir_log.log"
      gr = GroupReg
        { dbGroupId = 1,
          userGroupRegId = 1,
          dbContactId = 2,
          dbOwnerMemberId = Just 3,
          groupRegStatus = GRSActive,
          promoted = False,
          createdAt = UTCTime systemEpochDay 0
        }
  B.writeFile logFile $
    B.unlines
      [ strEncode (GRCreate gr),
        strEncode (GRUpdateStatus 1 GRSSuspended),
        strEncode (GRCreate gr {dbGroupId = 2, userGroupRegId = 2}),
        strEncode (GRDelete 2),
        strEncode (GRUpdatePromotion 1 True),
        strEncode (GRUpdateOwner 1 99)
      ]
  regs <- readDirectoryLogData logFile
  length regs `shouldBe` 1
  let r = head regs
  dbGroupId r `shouldBe` 1
  groupRegStatus r `shouldBe` GRSSuspended
  promoted r `shouldBe` True
  dbOwnerMemberId r `shouldBe` Just 99
  let GroupReg {createdAt} = r
  createdAt `shouldBe` UTCTime systemEpochDay 0

testOpenDirectoryLogNothing :: HasCallStack => TestParams -> IO ()
testOpenDirectoryLogNothing _ps = do
  dl <- openDirectoryLog Nothing
  directoryLogFile dl `shouldBe` Nothing

testMkChatOpts :: HasCallStack => TestParams -> IO ()
testMkChatOpts ps = do
  let dirOpts = mkDirectoryOpts ps [KnownContact 1 "admin"] Nothing Nothing
      co = mkChatOpts dirOpts
      ChatOpts {chatCmd, chatCmdDelay, chatCmdLog, chatServerPort, optFilesFolder, optTempDirectory, showReactions, allowInstantFiles, autoAcceptFileSize, muteNotifications, markRead, createBot} = co
  chatCmd `shouldBe` ""
  chatCmdDelay `shouldBe` 3
  (chatCmdLog == CCLNone) `shouldBe` True
  chatServerPort `shouldBe` Nothing
  optFilesFolder `shouldBe` Nothing
  optTempDirectory `shouldBe` Nothing
  showReactions `shouldBe` False
  allowInstantFiles `shouldBe` True
  autoAcceptFileSize `shouldBe` 0
  muteNotifications `shouldBe` True
  markRead `shouldBe` False
  case createBot of
    Just CreateBotOpts {botDisplayName} -> botDisplayName `shouldBe` "SimpleX Directory"
    Nothing -> expectationFailure "createBot should be Just"

testSearchTypes :: HasCallStack => TestParams -> IO ()
testSearchTypes _ps = do
  let st1 = STAll
      st2 = STRecent
      st3 = STSearch "test"
  case st1 of STAll -> pure (); _ -> expectationFailure "expected STAll"
  case st2 of STRecent -> pure (); _ -> expectationFailure "expected STRecent"
  case st3 of STSearch t -> t `shouldBe` "test"; _ -> expectationFailure "expected STSearch"
  -- field selectors for SearchRequest
  let sr = SearchRequest STAll (UTCTime systemEpochDay 0) 0
  case searchType sr of STAll -> pure (); _ -> expectationFailure "expected STAll"
  searchTime sr `shouldBe` UTCTime systemEpochDay 0
  lastGroup sr `shouldBe` 0

testGetCaptchaStr :: HasCallStack => TestParams -> IO ()
testGetCaptchaStr _ps = do
  s0 <- getCaptchaStr 0 ""
  s0 `shouldBe` ""
  s7 <- getCaptchaStr 7 ""
  length s7 `shouldBe` 7
  all (`elem` ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" :: String)) s7 `shouldBe` True

testNewOrActive :: HasCallStack => TestParams -> IO ()
testNewOrActive _ps =
  newOrActive `shouldBe` (30 * nominalDay)

testDirectoryListingJSON :: HasCallStack => TestParams -> IO ()
testDirectoryListingJSON _ps = do
  let listing = DirectoryListing {entries = []}
      encoded = J.encode listing
  case J.eitherDecode encoded of
    Right listing' -> null (entries (listing' :: DirectoryListing)) `shouldBe` True
    Left e -> expectationFailure $ "Failed to decode DirectoryListing: " <> e

testGroupDBError :: HasCallStack => TestParams -> IO ()
testGroupDBError _ps = do
  groupDBError (SEGroupNotFound 1) `shouldBe` "group not found"
  groupDBError (SEInternalError "test") `shouldBe` show (SEInternalError "test")

testReadDirectoryLogWarnings :: HasCallStack => TestParams -> IO ()
testReadDirectoryLogWarnings ps = do
  let logFile = tmpPath ps </> "test_log_warnings.log"
      gr = GroupReg
        { dbGroupId = 1,
          userGroupRegId = 1,
          dbContactId = 2,
          dbOwnerMemberId = Just 3,
          groupRegStatus = GRSActive,
          promoted = False,
          createdAt = UTCTime systemEpochDay 0
        }
  B.writeFile logFile $
    B.unlines
      [ strEncode (GRCreate gr),
        strEncode (GRDelete 99),
        strEncode (GRUpdateStatus 99 GRSActive),
        strEncode (GRUpdatePromotion 99 True),
        strEncode (GRUpdateOwner 99 42),
        strEncode (GRCreate gr),
        "invalid log line"
      ]
  regs <- readDirectoryLogData logFile
  length regs `shouldBe` 1
  dbGroupId (head regs) `shouldBe` 1
  groupRegStatus (head regs) `shouldBe` GRSActive

testProfileConditionJSON :: HasCallStack => TestParams -> IO ()
testProfileConditionJSON _ps = do
  jsonRoundTrip PCAll
  jsonRoundTrip PCNoImage
  where
    jsonRoundTrip :: ProfileCondition -> IO ()
    jsonRoundTrip pc = case J.eitherDecode (J.encode pc) of
      Right pc' -> pc' `shouldBe` pc
      Left e -> expectationFailure $ "Failed to decode ProfileCondition: " <> e

testDirectoryMemberAcceptanceJSON :: HasCallStack => TestParams -> IO ()
testDirectoryMemberAcceptanceJSON _ps = do
  jsonRoundTrip noJoinFilter
  jsonRoundTrip basicJoinFilter
  jsonRoundTrip moderateJoinFilter
  jsonRoundTrip strongJoinFilter
  where
    jsonRoundTrip :: DirectoryMemberAcceptance -> IO ()
    jsonRoundTrip dma = case J.eitherDecode (J.encode dma) of
      Right dma' -> dma' `shouldBe` dma
      Left e -> expectationFailure $ "Failed to decode DirectoryMemberAcceptance: " <> e

testDirectoryGroupDataJSON :: HasCallStack => TestParams -> IO ()
testDirectoryGroupDataJSON _ps = do
  jsonRoundTrip (DirectoryGroupData noJoinFilter)
  jsonRoundTrip (DirectoryGroupData basicJoinFilter)
  jsonRoundTrip (DirectoryGroupData moderateJoinFilter)
  jsonRoundTrip (DirectoryGroupData strongJoinFilter)
  where
    jsonRoundTrip :: DirectoryGroupData -> IO ()
    jsonRoundTrip dgd = case J.eitherDecode (J.encode dgd) of
      Right dgd' -> memberAcceptance (dgd' :: DirectoryGroupData) `shouldBe` memberAcceptance (dgd :: DirectoryGroupData)
      Left e -> expectationFailure $ "Failed to decode DirectoryGroupData: " <> e

testOpenDirectoryLogJust :: HasCallStack => TestParams -> IO ()
testOpenDirectoryLogJust ps = do
  let logFile = tmpPath ps </> "test_open_dir_log.log"
  dl <- openDirectoryLog (Just logFile)
  case directoryLogFile dl of
    Just h -> hClose h
    Nothing -> expectationFailure "directoryLogFile should be Just"

testMigrateLogConstructors :: HasCallStack => TestParams -> IO ()
testMigrateLogConstructors _ps = do
  check MLCheck
  check MLImport
  check MLExport
  check MLListing
  where
    check :: MigrateLog -> IO ()
    check ml = case ml of
      MLCheck -> pure ()
      MLImport -> pure ()
      MLExport -> pure ()
      MLListing -> pure ()

testDirectoryOptsParser :: HasCallStack => TestParams -> IO ()
testDirectoryOptsParser ps = do
  let parser = OA.info (directoryOpts (tmpPath ps) "test_db") mempty
  -- 3a. Minimal args (covers parser skeleton + all defaults)
  case OA.execParserPure OA.defaultPrefs parser ["--super-users", "1:admin"] of
    OA.Success opts -> do
      length (superUsers opts) `shouldBe` 1
      length (adminUsers opts) `shouldBe` 0
      case ownersGroup (opts :: DirectoryOpts) of Nothing -> pure (); Just _ -> expectationFailure "expected Nothing"
      noAddress opts `shouldBe` False
      blockedWordsFile opts `shouldBe` Nothing
      blockedFragmentsFile opts `shouldBe` Nothing
      blockedExtensionRules opts `shouldBe` Nothing
      nameSpellingFile opts `shouldBe` Nothing
      profileNameLimit opts `shouldBe` maxBound
      captchaGenerator opts `shouldBe` Nothing
      voiceCaptchaGenerator opts `shouldBe` Nothing
      directoryLog (opts :: DirectoryOpts) `shouldBe` Nothing
      case migrateDirectoryLog opts of Nothing -> pure (); Just _ -> expectationFailure "expected Nothing"
      serviceName opts `shouldBe` "SimpleX Directory"
      runCLI opts `shouldBe` False
      searchResults opts `shouldBe` 10
      webFolder (opts :: DirectoryOpts) `shouldBe` Nothing
      testing opts `shouldBe` False
    OA.Failure e -> expectationFailure $ "Parser failed (minimal): " <> show e
    OA.CompletionInvoked _ -> expectationFailure "Unexpected completion"
  -- 3b. Non-default args (covers switch/option branches)
  case OA.execParserPure OA.defaultPrefs parser
        [ "--super-users", "1:super",
          "--admin-users", "2:admin",
          "--no-address",
          "--run-cli",
          "--service-name", "Test",
          "--profile-name-limit", "50",
          "--directory-file", "/tmp/test.log",
          "--blocked-words-file", "/tmp/w.txt",
          "--blocked-fragments-file", "/tmp/f.txt",
          "--blocked-extenstion-rules", "/tmp/r.txt",
          "--name-spelling-file", "/tmp/s.txt",
          "--captcha-generator", "/tmp/cg",
          "--voice-captcha-generator", "/tmp/vcg",
          "--web-folder", "/tmp/web"
        ] of
    OA.Success opts -> do
      length (superUsers opts) `shouldBe` 1
      length (adminUsers opts) `shouldBe` 1
      noAddress opts `shouldBe` True
      runCLI opts `shouldBe` True
      serviceName opts `shouldBe` "Test"
      profileNameLimit opts `shouldBe` 50
      directoryLog (opts :: DirectoryOpts) `shouldBe` Just "/tmp/test.log"
      blockedWordsFile opts `shouldBe` Just "/tmp/w.txt"
      blockedFragmentsFile opts `shouldBe` Just "/tmp/f.txt"
      blockedExtensionRules opts `shouldBe` Just "/tmp/r.txt"
      nameSpellingFile opts `shouldBe` Just "/tmp/s.txt"
      captchaGenerator opts `shouldBe` Just "/tmp/cg"
      voiceCaptchaGenerator opts `shouldBe` Just "/tmp/vcg"
      webFolder (opts :: DirectoryOpts) `shouldBe` Just "/tmp/web"
    OA.Failure e -> expectationFailure $ "Parser failed (non-default): " <> show e
    OA.CompletionInvoked _ -> expectationFailure "Unexpected completion"
  -- 3c. MigrateLog (covers parseMigrateLog parser)
  forM_ [("check", MLCheck), ("import", MLImport), ("export", MLExport), ("listing", MLListing)] $
    \(s, expected) ->
      case OA.execParserPure OA.defaultPrefs parser ["--super-users", "1:admin", "--migrate-directory-file", s] of
        OA.Success opts -> case migrateDirectoryLog opts of
          Just ml -> case (ml, expected) of
            (MLCheck, MLCheck) -> pure ()
            (MLImport, MLImport) -> pure ()
            (MLExport, MLExport) -> pure ()
            (MLListing, MLListing) -> pure ()
            _ -> expectationFailure $ "MigrateLog mismatch for: " <> s
          Nothing -> expectationFailure $ "Expected Just MigrateLog for: " <> s
        OA.Failure e -> expectationFailure $ "Parser failed for migrate-directory-file " <> s <> ": " <> show e
        OA.CompletionInvoked _ -> expectationFailure "Unexpected completion"

testEventsShowInstances :: HasCallStack => TestParams -> IO ()
testEventsShowInstances _ps = do
  -- SDirectoryRole Show
  show SDRUser `shouldSatisfy` (not . null)
  show SDRAdmin `shouldSatisfy` (not . null)
  show SDRSuperUser `shouldSatisfy` (not . null)
  -- SDirectoryRole showList
  show [SDRUser] `shouldSatisfy` (not . null)
  -- DirectoryHelpSection Show
  show DHSRegistration `shouldSatisfy` (not . null)
  show DHSCommands `shouldSatisfy` (not . null)
  -- DirectoryHelpSection showList
  show [DHSRegistration, DHSCommands] `shouldSatisfy` (not . null)
  -- DirectoryEvent Show (covers deriving instance)
  show (DELogChatResponse "test") `shouldSatisfy` (not . null)
  -- DirectoryEvent showList
  show [DELogChatResponse "test"] `shouldSatisfy` (not . null)
  -- DirectoryCmdTag Show (direct calls bypass existential wrapper)
  show DCHelp_ `shouldSatisfy` (not . null)
  show DCApproveGroup_ `shouldSatisfy` (not . null)
  show DCExecuteCommand_ `shouldSatisfy` (not . null)
  -- DirectoryCmdTag showList
  show [DCHelp_] `shouldSatisfy` (not . null)
  -- DirectoryCmd Show (direct call, not through ADirectoryCmd)
  show (DCHelp DHSRegistration) `shouldSatisfy` (not . null)
  -- DirectoryCmd showList
  show [DCHelp DHSRegistration] `shouldSatisfy` (not . null)
  -- ADirectoryCmd showList
  show [ADC SDRUser DCUnknownCommand, ADC SDRUser DCSearchNext] `shouldSatisfy` (not . null)
  -- DCApproveGroup field selectors (line 162)
  let cmd = DCApproveGroup {groupId = 1, displayName = "test", groupApprovalId = 2, promote = Just True}
  cmd.groupId `shouldBe` 1
  cmd.displayName `shouldBe` "test"
  cmd.groupApprovalId `shouldBe` 2
  cmd.promote `shouldBe` Just True

testStoreShowInstances :: HasCallStack => TestParams -> IO ()
testStoreShowInstances _ps = do
  show noJoinFilter `shouldSatisfy` (not . null)
  show PCAll `shouldSatisfy` (not . null)
  show PCNoImage `shouldSatisfy` (not . null)
  show GRSActive `shouldSatisfy` (not . null)
  show (GRSPendingApproval 1) `shouldSatisfy` (not . null)
  show GRSSuspended `shouldSatisfy` (not . null)
  show GRSSuspendedBadRoles `shouldSatisfy` (not . null)
  show GRSRemoved `shouldSatisfy` (not . null)
  show GRSProposed `shouldSatisfy` (not . null)
  show GRSPendingUpdate `shouldSatisfy` (not . null)
  show GRSPendingConfirmation `shouldSatisfy` (not . null)

testDirectoryEntryTypeJSON :: HasCallStack => TestParams -> IO ()
testDirectoryEntryTypeJSON _ps = do
  let det = DETGroup Nothing (GroupSummary 0)
      encoded = J.encode det
  case J.eitherDecode encoded of
    Right det' -> do
      admission (det' :: DirectoryEntryType) `shouldBe` Nothing
      let GroupSummary {currentMembers} = summary det'
      currentMembers `shouldBe` 0
    Left e -> expectationFailure $ "Failed to decode DirectoryEntryType: " <> e
  -- with non-zero members
  let det2 = DETGroup Nothing (GroupSummary 42)
  case J.eitherDecode (J.encode det2) of
    Right det2' -> do
      let GroupSummary {currentMembers = cm2} = summary det2'
      cm2 `shouldBe` 42
    Left e -> expectationFailure $ "Failed to decode DirectoryEntryType: " <> e
