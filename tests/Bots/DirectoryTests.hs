{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Bots.DirectoryTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Groups (memberJoinChannel, prepareChannel1Relay)
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, when, void)
import qualified Data.Aeson as J
import qualified Data.Text as T
import Directory.Captcha
import Directory.Listing
import Directory.Options
import Directory.Service
import System.Directory (emptyPermissions, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller (ChatConfig (..))
import qualified Simplex.Chat.Markdown as MD
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Protocol (memberSupportVoiceVersion)
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Simplex.Messaging.Version
import NameResolver
import System.FilePath ((</>))
import Test.Hspec hiding (it)

directoryServiceTests :: SpecWith TestParams
directoryServiceTests = do
  it "should register group" testDirectoryService
  it "should suspend and resume group, send message to owner" testSuspendResume
  it "should delete group registration" testDeleteGroup
  it "admin should delete group registration" testDeleteGroupAdmin
  it "should change initial member role" testSetRole
  it "should join found group via link" testJoinGroup
  it "should find registered group by link" testSearchByLink
  it "should support group names with spaces" testGroupNameWithSpaces
  it "should return more groups in search, all and recent groups" testSearchGroups
  it "should invite to owners' group if specified" testInviteToOwnersGroup
  it "should re-invite owner who left owners' group" testInviteOwnerAfterLeavingOwnersGroup
  describe "de-listing the group" $ do
    it "should de-list if owner leaves the group" testDelistedOwnerLeaves
    it "should de-list if owner is removed from the group" testDelistedOwnerRemoved
    it "should NOT de-list if another member leaves the group" testNotDelistedMemberLeaves
    it "should NOT de-list if another member is removed from the group" testNotDelistedMemberRemoved
    it "should NOT de-list if the owner rejoins via the group link and leaves the second membership" testNotDelistedOwnerRejoinsViaLink
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
  describe "should NOT require re-approval if group link is added or removed by" $ do
    it "the registration owner" testRegOwnerRemovedLink
    it "another owner" testAnotherOwnerRemovedLink
    it "another owner not connected to directory" testNotConnectedOwnerRemovedLink
  describe "duplicate groups (same display name and full name)" $ do
    it "should ask for confirmation if a duplicate group is submitted" testDuplicateAskConfirmation
    it "should prohibit registration if a duplicate group is listed" testDuplicateProhibitRegistration
    it "should prohibit confirmation if a duplicate group is listed" testDuplicateProhibitConfirmation
    it "should allow to rename and approve a duplicate registration" testDuplicateProhibitWhenUpdated
    it "should prohibit approval if a duplicate group is listed" testDuplicateProhibitApproval
  describe "list and promote groups" $ do
    it "should list and promote user's groups" $ testListUserGroups True
  describe "member admission" $ do
    it "should require captcha by default for new groups" testCaptchaByDefault
    it "should require captcha in all groups with --always-captcha" testAlwaysCaptcha
    it "should require admin review in all groups with --knocking" testKnocking
    it "should ask member to pass captcha screen" testCapthaScreening
    it "should send voice captcha on /audio command" testVoiceCaptchaScreening
    it "should retry with voice captcha after switching to audio mode" testVoiceCaptchaRetry
    it "should send voice captcha when voice disabled but client supports v17" testVoiceCaptchaVoiceDisabled
    it "should show unavailable message for old client in voice-disabled group" testVoiceCaptchaOldClient
    it "should reject member after too many captcha attempts" testCaptchaTooManyAttempts
    it "should respond to unknown command during captcha" testCaptchaUnknownCommand
  describe "store log" $ do
    it "should restore directory service state" testRestoreDirectory
  describe "captcha" $ do
    it "should accept some incorrect spellings" testCaptcha
    it "should generate captcha of correct length" testGetCaptchaStr
  describe "help commands" $ do
    it "should not list audio command" testHelpNoAudio
    it "should reject audio command in DM" testAudioCommandInDM
  describe "public group registration" $ do
    it "should register channel via shared link card" testRegisterChannelViaCard
    it "should suggest share via chat when link sent as text" testLinkAsTextSearch
    it "should reject card shared by non-owner" testNonOwnerSharesCard
    it "should delete channel registration and leave" testDeleteChannelRegistration
    it "should handle re-registration when already listed" testReregistrationAlreadyListed
    it "should update subscriber count periodically" testLinkCheckUpdatesCount

-- separate spec from directoryServiceTests: these need a names-enabled SMP server (withSmpServerAndNames)
directoryNameTests :: SpecWith TestParams
directoryNameTests = do
  it "should verify and show a channel's SimpleX name" testDirectoryChannelName
  it "should mark an inconsistent SimpleX name as not verified" testDirectoryChannelNameNotVerified

directoryProfile :: Profile
directoryProfile = Profile {displayName = "SimpleX Directory", fullName = "", shortDescr = Nothing, description = Nothing, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences = Nothing, badge = Nothing, contactDomain = Nothing}

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
      serviceName = "SimpleX Directory",
      clientService = True,
      runCLI = False,
      searchResults = 3,
      webFolder,
      linkCheckInterval = 0,
      prohibitedToObserver = False,
      alwaysCaptcha = False,
      knocking = False,
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
        -- putStrLn "*** discover service joins group and sends the registration for approval"
        bob <## "#PSA: you changed the role of 'SimpleX Directory' to admin"
        bob <# "'SimpleX Directory'> Joining the group PSA…"
        bob <## "#PSA: 'SimpleX Directory' joined the group"
        bob <# "'SimpleX Directory'> Joined the group PSA. Registration is pending approval — it may take up to 48 hours."
        bob <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
        bob <## "Captcha verification is enabled. Use /'filter 1' to change it."
        notifySuperUser_ superUser bob "PSA" "Privacy, Security & Anonymity" Nothing 1 1
        -- putStrLn "*** update profile before approval - new approval code"
        updateGroupProfile bob "Welcome!"
        groupUpdatedHidden superUser bob "PSA" ""
        notifySuperUser_ superUser bob "PSA" "Privacy, Security & Anonymity" (Just "Welcome!") 1 2
        -- putStrLn "*** try approving with the old registration code"
        bob #> "@'SimpleX Directory' /approve 1:PSA 1"
        bob <# "'SimpleX Directory'> > /approve 1:PSA 1"
        bob <## "      You are not allowed to use this command"
        superUser #> "@'SimpleX Directory' /approve 1:PSA 1"
        superUser <# "'SimpleX Directory'> > /approve 1:PSA 1"
        superUser <## "      Incorrect approval code"
        superUser #> "@'SimpleX Directory' /pending"
        superUser <# "'SimpleX Directory'> > /pending"
        superUser <## "      1 registered group(s)"
        superUser <# "'SimpleX Directory'> 1. PSA (Privacy, Security & Anonymity)"
        superUser <## "Welcome message:"
        superUser <## "Welcome!"
        superUser <## "Owner: bob"
        superUser <## "2 members"
        superUser <## "Status: pending admin approval"
        superUser <## "/'role 1', /'filter 1'"
        welcomeWithLink <- approveRegistration_ superUser bob "PSA" 1 1 2
        -- putStrLn "*** add the link to the welcome message - the group remains listed"
        let welcomeWithLink' = "Welcome! " <> welcomeWithLink
        updateGroupProfile bob welcomeWithLink'
        groupUpdatedListed superUser bob "PSA" ""
        search bob "privacy" welcomeWithLink'
        search bob "security" welcomeWithLink'
        cath `connectVia` dsLink
        search cath "privacy" welcomeWithLink'
        -- putStrLn "*** remove the link from the welcome message - the group remains listed"
        updateGroupProfile bob "Welcome!"
        groupUpdatedListed superUser bob "PSA" ""
        bob #> "@'SimpleX Directory' privacy"
        bob <# "'SimpleX Directory'> > privacy"
        bob <## "      Found 1 group(s)."
        bob <# "'SimpleX Directory'> PSA (Privacy, Security & Anonymity)"
        bob <## "Welcome message:"
        bob <## "Welcome!"
        bob <##. "Link to join the group PSA: "
        bob <## "2 members"
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
      -- add the link to the welcome message - the group remains listed
      bob #> "@'SimpleX Directory' /link 1"
      bob <# "'SimpleX Directory'> > /link 1"
      bob <## "      The link to join the group ID 1 (privacy):"
      gLink <- getTermLine bob
      gLink `shouldStartWith` "https://localhost/g#"
      bob <## "New member role: member"
      setWelcomeMessage bob [] ("Link to join the group privacy: " <> gLink)
      groupUpdatedListed superUser bob "privacy" ""
      -- change the link to the equivalent - should not ask to re-approve
      setWelcomeMessage bob [] ("Link to join the group privacy: " <> gLink <> "?same_link=true")
      groupUpdatedListed superUser bob "privacy" ""
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
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        cath #> ("#privacy (support) " <> captcha)
        cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
        cath <## "      Correct, you joined the group privacy"
        cath <## "#privacy: you joined the group"
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
          linkLine <- getTermLine cath
          let groupLink = dropStrPrefix "Link to join the group privacy: " linkLine
          cath <## "2 members"
          cath ##> ("/c " <> groupLink)
          cath <## "connection request sent!"
          cath <## "#privacy: joining the group..."
          cath <## "#privacy: you joined the group, pending approval"
          cath <# "#privacy (support) 'SimpleX Directory_1'> Captcha is generated by SimpleX Directory service."
          cath <## ""
          cath <## "Send captcha text to join the group privacy."
          captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory_1'> " . dropTime <$> getTermLine cath
          cath <## "contact and member are merged: 'SimpleX Directory', #privacy 'SimpleX Directory_1'"
          cath <## "use @'SimpleX Directory' <message> to send messages"
          cath #> ("#privacy (support) " <> captcha)
          cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
          cath <## "      Correct, you joined the group privacy"
          cath <## "#privacy: you joined the group"
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
                dan
                  <### [ "#privacy: member 'SimpleX Directory' is connected",
                         "#privacy: member cath (Catherine) is connected"
                       ],
              do
                cath <## "#privacy: bob added dan (Daniel) to the group (connecting...)"
                cath <## "#privacy: new member dan is connected"
            ]

testSearchByLink :: HasCallStack => TestParams -> IO ()
testSearchByLink ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      submitGroup bob "privacy" "Privacy"
      groupAccepted bob "privacy" 1
      notifySuperUser superUser bob "privacy" "Privacy" 1
      welcomeWithLink <- approveRegistration superUser bob "privacy" 1
      let link = dropStrPrefix "Link to join the group privacy: " welcomeWithLink
      -- user finds the listed group by link
      bob #> ("@'SimpleX Directory' " <> link)
      bob <# ("'SimpleX Directory'> > " <> link)
      bob <## "      Found group:"
      bob <# "'SimpleX Directory'> privacy (Privacy)"
      bob <##. "Link to join the group privacy: "
      bob <## "2 members"
      -- admin receives the group with status
      superUser #> ("@'SimpleX Directory' " <> link)
      superUser <# ("'SimpleX Directory'> > " <> link)
      superUser <## "      1 registered group(s)"
      memberGroupListing superUser bob 1 "privacy" "Privacy" 2 "active"
      -- content change hides the group from user search, admin still finds it by link
      setWelcomeMessage bob [] "Welcome!"
      groupUpdatedHidden superUser bob "privacy" ""
      notifySuperUser_ superUser bob "privacy" "Privacy" (Just "Welcome!") 1 1
      bob #> ("@'SimpleX Directory' " <> link)
      bob <# ("'SimpleX Directory'> > " <> link)
      bob <## "      No groups found."
      bob <## "To register a group or a channel, please use \"Share via chat\" feature."
      superUser #> ("@'SimpleX Directory' " <> link)
      superUser <# ("'SimpleX Directory'> > " <> link)
      superUser <## "      1 registered group(s)"
      superUser <# "'SimpleX Directory'> 1. privacy (Privacy)"
      superUser <## "Welcome message:"
      superUser <## "Welcome!"
      superUser <## "Owner: bob"
      superUser <## "2 members"
      superUser <## "Status: pending admin approval"
      superUser <## "/'role 1', /'filter 1'"

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

testInviteOwnerAfterLeavingOwnersGroup :: HasCallStack => TestParams -> IO ()
testInviteOwnerAfterLeavingOwnersGroup ps =
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
      -- owner leaves owners' group; GroupMember row keeps status GSMemLeft
      leaveGroup "owners" bob
      superUser <## "#owners: bob left the group"
      -- owners' group has no GroupReg, so directory service notifies admins on contact left
      superUser <# "'SimpleX Directory'> Error: contact left, group: 1 owners, group registration not found"
      -- super-user re-invites via /invite — must send a fresh invitation, not "already a member"
      superUser #> "@'SimpleX Directory' /invite 2:privacy"
      superUser <# "'SimpleX Directory'> > /invite 2:privacy"
      superUser <## "      you invited @bob, the owner of the group ID 2 (privacy) to owners' group owners"
      bob <## "#owners_1: 'SimpleX Directory' invites you to join the group as member"
      bob <## "use /j owners_1 to accept"

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

-- Reproduces the de-listing bug where a non-owner member associated with the
-- registration owner's contact (via the probe-and-merge mechanism) de-lists the
-- group when it leaves. The owner joins the directory-managed link a second time
-- (a single client owning both connection ends completes the merge with no
-- modified client), then leaves that second membership while remaining the owner.
testNotDelistedOwnerRejoinsViaLink :: HasCallStack => TestParams -> IO ()
testNotDelistedOwnerRejoinsViaLink ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      submitGroup bob "privacy" "Privacy"
      groupAccepted bob "privacy" 1
      welcomeWithLink <- completeRegistration superUser bob "privacy" "Privacy" 1
      let groupLink = dropStrPrefix "Link to join the group privacy: " welcomeWithLink
      -- turn off the captcha filter so the owner's re-join is not screened
      bob #> "@'SimpleX Directory' /filter 1 off"
      bob <# "'SimpleX Directory'> > /filter 1 off"
      bob <## "      Spam filter settings for group privacy set to:"
      bob <## "- reject long/inappropriate names: disabled"
      bob <## "- pass captcha to join: disabled"
      bob <## ""
      bob <## "/'filter 1 name' - enable name filter"
      bob <## "/'filter 1 captcha' - enable captcha challenge"
      bob <## "/'filter 1 name captcha' - enable both"
      -- the registration owner connects to the directory-managed link again,
      -- creating a second membership that the probe-and-merge mechanism
      -- associates with the owner's own contact on the directory service
      bob ##> ("/c " <> groupLink)
      bob <## "connection request sent!"
      bob <## "#privacy_1: joining the group..."
      bob <## "#privacy_1: you joined the group"
      bob
        <### [ "#privacy: 'SimpleX Directory' added bob_1 (Bob) to the group (connecting...)",
               "contact and member are merged: 'SimpleX Directory', #privacy_1 'SimpleX Directory_1'",
               "use @'SimpleX Directory' <message> to send messages",
               "#privacy_1: member bob_2 (Bob) is connected",
               "#privacy: new member bob_1 is connected"
             ]
      -- allow the directory service to complete the contact/member merge that
      -- associates the second membership (bob_1) with bob's contact
      threadDelay 3000000
      -- owner leaves the second membership, which is not the owner member
      bob ##> "/l privacy_1"
      bob <## "#privacy_1: you left the group"
      bob <## "use /d #privacy_1 to delete the group"
      bob <## "#privacy: bob_1 left the group"
      -- the group must remain listed: the leaving member is not the owner member
      (superUser </)
      groupFound bob "privacy"

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
        groupAccepted bob "privacy" 1
        notifySuperUser superUser bob "privacy" "Privacy" 1
        bob ##> "/mr privacy 'SimpleX Directory' member"
        bob <## "#privacy: you changed the role of 'SimpleX Directory' to member"
        bob ##> "/gp privacy privacy Privacy!"
        bob <## "description changed to: Privacy!"
        groupUpdatedHidden superUser bob "privacy" ""
        bob <# "'SimpleX Directory'> You must grant directory service admin role to register the group"
        bob ##> "/mr privacy 'SimpleX Directory' admin"
        bob <## "#privacy: you changed the role of 'SimpleX Directory' to admin"
        bob <# "'SimpleX Directory'> SimpleX Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is submitted for approval."
        notifySuperUser_ superUser bob "privacy" "Privacy!" Nothing 1 2
        groupNotFound cath "privacy"
        void $ approveRegistration_ superUser bob "privacy" 1 1 2
        groupFound cath "privacy"

testNotApprovedBadRoles :: HasCallStack => TestParams -> IO ()
testNotApprovedBadRoles ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        groupAccepted bob "privacy" 1
        notifySuperUser superUser bob "privacy" "Privacy" 1
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
        notifySuperUser superUser bob "privacy" "Privacy" 1
        void $ approveRegistration superUser bob "privacy" 1
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
        -- setting the welcome message requires re-approval
        setWelcomeMessage bob [cath] "Welcome!"
        groupUpdatedHidden superUser bob "privacy" ""
        reapproveGroup_ 3 superUser bob (Just "Welcome!")
        -- adding the link keeps the group listed
        gLink <- getGroupLinkFromBot bob
        setWelcomeMessage bob [cath] ("Welcome! Link to join the group privacy: " <> gLink)
        groupUpdatedListed superUser bob "privacy" ""
        -- removing the link keeps the group listed
        setWelcomeMessage bob [cath] "Welcome!"
        groupUpdatedListed superUser bob "privacy" ""
        cath `connectVia` dsLink
        cath <## "contact and member are merged: 'SimpleX Directory_1', #privacy 'SimpleX Directory'"
        cath <## "use @'SimpleX Directory' <message> to send messages"
        groupFoundWelcome 3 cath "privacy" "Welcome!"

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
        -- setting the welcome message requires re-approval
        setWelcomeMessage cath [bob] "Welcome!"
        groupUpdatedHidden superUser bob "privacy" " by cath"
        reapproveGroup_ 3 superUser bob (Just "Welcome!")
        -- another owner adds the link - the group remains listed
        gLink <- getGroupLinkFromBot bob
        setWelcomeMessage cath [bob] ("Welcome! Link to join the group privacy: " <> gLink)
        groupUpdatedListed superUser bob "privacy" " by cath"
        -- another owner removes the link - the group remains listed
        setWelcomeMessage cath [bob] "Welcome!"
        groupUpdatedListed superUser bob "privacy" " by cath"
        groupFoundWelcome 3 cath "privacy" "Welcome!"

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
          -- setting the welcome message requires re-approval
          setWelcomeMessage cath [bob] "Welcome!"
          groupUpdatedHidden superUser bob "privacy" " by cath"
          groupNotFound dan "privacy"
          reapproveGroup_ 3 superUser bob (Just "Welcome!")
          -- the not connected owner adds the link - the group remains listed
          gLink <- getGroupLinkFromBot bob
          setWelcomeMessage cath [bob] ("Welcome! Link to join the group privacy: " <> gLink)
          groupUpdatedListed superUser bob "privacy" " by cath"
          -- the not connected owner removes the link - the group remains listed
          setWelcomeMessage cath [bob] "Welcome!"
          groupUpdatedListed superUser bob "privacy" " by cath"
          groupFoundWelcome 3 dan "privacy" "Welcome!"

testDuplicateAskConfirmation :: HasCallStack => TestParams -> IO ()
testDuplicateAskConfirmation ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        groupAccepted bob "privacy" 1
        notifySuperUser superUser bob "privacy" "Privacy" 1
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        cath #> "@'SimpleX Directory' /confirm 1:privacy"
        groupAccepted cath "privacy" 1
        groupNotFound bob "privacy"
        void $ completeRegistrationId superUser cath "privacy" "Privacy" 2 1
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
        groupAccepted bob "privacy" 1
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        groupNotFound cath "privacy"
        void $ completeRegistration superUser bob "privacy" "Privacy" 1
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
        groupAccepted bob "privacy" 1
        notifySuperUser superUser bob "privacy" "Privacy" 1
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        cath #> "@'SimpleX Directory' /confirm 1:privacy"
        groupAccepted cath "privacy" 1
        notifySuperUser superUser cath "privacy" "Privacy" 2
        groupNotFound cath "privacy"
        void $ approveRegistration superUser bob "privacy" 1
        groupFound cath "privacy"
        -- the duplicate registration is renamed and approved
        cath ##> "/gp privacy security Security"
        cath <## "changed to #security (Security)"
        cath <# "'SimpleX Directory'> The group ID 1 (security) is updated!"
        cath <## "It is hidden from the directory until approved."
        superUser <# "'SimpleX Directory'> The group ID 2 (security) is updated."
        notifySuperUser_ superUser cath "security" "Security" Nothing 2 2
        void $ approveRegistration_ superUser cath "security" 2 1 2
        groupFound bob "security"
        groupFound cath "security"

testDuplicateProhibitApproval :: HasCallStack => TestParams -> IO ()
testDuplicateProhibitApproval ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        groupAccepted bob "privacy" 1
        notifySuperUser superUser bob "privacy" "Privacy" 1
        cath `connectVia` dsLink
        submitGroup cath "privacy" "Privacy"
        cath <# "'SimpleX Directory'> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "'SimpleX Directory'> /confirm 1:privacy"
        cath #> "@'SimpleX Directory' /confirm 1:privacy"
        groupAccepted cath "privacy" 1
        notifySuperUser superUser cath "privacy" "Privacy" 2
        groupNotFound cath "privacy"
        void $ approveRegistration superUser bob "privacy" 1
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
          cath <## "bob updated group #privacy:"
          cath <## "description removed"
          groupUpdatedHidden superUser bob "privacy" ""
          superUser <# "'SimpleX Directory'> bob submitted the group ID 1:"
          superUser <## "privacy"
          superUser <## "3 members"
          superUser <## ""
          superUser <## "To approve send:"
          superUser <# "'SimpleX Directory'> /approve 1:privacy 1 promote=on"
          checkListings ["security"] []
          superUser #> "@'SimpleX Directory' /approve 1:privacy 1"
          superUser <# "'SimpleX Directory'> > /approve 1:privacy 1"
          superUser <## "      Group approved (promoted)!"
          void $ groupApprovedNotification bob "privacy" 1
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

testAlwaysCaptcha :: HasCallStack => TestParams -> IO ()
testAlwaysCaptcha ps =
  withDirectoryServiceOpts ps (\o -> o {alwaysCaptcha = True}) $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        -- disable the per-group captcha filter; --always-captcha must still force it
        bob #> "@'SimpleX Directory' /filter 1 off"
        bob <# "'SimpleX Directory'> > /filter 1 off"
        bob <## "      Spam filter settings for group privacy set to:"
        bob <## "- reject long/inappropriate names: disabled"
        bob <## "- pass captcha to join: disabled"
        bob <## ""
        bob <## "/'filter 1 name' - enable name filter"
        bob <## "/'filter 1 captcha' - enable captcha challenge"
        bob <## "/'filter 1 name captcha' - enable both"
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        cath #> ("#privacy (support) " <> captcha)
        cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
        cath <## "      Correct, you joined the group privacy"
        cath <## "#privacy: you joined the group"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"

testKnocking :: HasCallStack => TestParams -> IO ()
testKnocking ps =
  withDirectoryServiceOpts ps (\o -> o {knocking = True}) $ \superUser dsLink ->
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
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, connecting to group moderators for admission to group"
        cath <## "#privacy: 'SimpleX Directory' accepted you to the group, pending review"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting and pending review...), use /_accept member #1 3 <role> to accept member"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: new member cath is connected and pending review, use /_accept member #1 3 <role> to accept member"

testCaptchaByDefault :: HasCallStack => TestParams -> IO ()
testCaptchaByDefault ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        -- the owner never ran /filter; captcha is on by default for new groups
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        cath #> ("#privacy (support) " <> captcha)
        cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
        cath <## "      Correct, you joined the group privacy"
        cath <## "#privacy: you joined the group"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"

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
        -- connect with captcha screen
        _ <- join cath groupLink
        cath #> "#privacy (support) 123" -- sending incorrect captcha
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath 123"
        cath <## "      Incorrect text, please try again."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        sendCaptcha cath captcha
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
        -- cath joins, receives text captcha with /audio hint
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        cath <## "Send /audio to receive a voice captcha."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        -- cath requests audio captcha
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 1"
        -- cath sends /audio again, already enabled
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath /audio"
        cath <## "      Audio captcha is already enabled."
        -- send correct captcha
        sendCaptcha cath captcha
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
        -- cath joins, receives text captcha with /audio hint
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        cath <## "Send /audio to receive a voice captcha."
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
        -- KEY ASSERTION: retry sends BOTH image and voice because captchaMode=CMAudio
        _ <- getTermLine cath -- captcha image/text
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 2"

testVoiceCaptchaVoiceDisabled :: HasCallStack => TestParams -> IO ()
testVoiceCaptchaVoiceDisabled ps@TestParams {tmpPath} = do
  let mockScript = tmpPath </> "mock_voice_gen_vdisabled.py"
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
        -- disable voice messages in the group
        bob ##> "/set voice #privacy off"
        bob <## "updated group preferences:"
        bob <## "Voice messages: off"
        -- cath (new client, supports v17 exemption) joins, /audio hint shown
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        cath <## "Send /audio to receive a voice captcha."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        -- voice captcha works despite voice being disabled (v17 host approval exemption)
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'> voice message (00:05)"
        cath <#. "#privacy (support) 'SimpleX Directory'> sends file "
        cath <##. "use /fr 1"
        sendCaptcha cath captcha
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"
  where
    sendCaptcha cath captcha = do
      cath #> ("#privacy (support) " <> captcha)
      cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
      cath <## "      Correct, you joined the group privacy"
      cath <## "#privacy: you joined the group"

testVoiceCaptchaOldClient :: HasCallStack => TestParams -> IO ()
testVoiceCaptchaOldClient ps@TestParams {tmpPath} = do
  let mockScript = tmpPath </> "mock_voice_gen_oldclient.py"
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
      withNewTestChatCfg ps testCfg {chatVRange = (chatVRange testCfg) {maxVersion = prevVersion memberSupportVoiceVersion}} "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        bob #> "@'SimpleX Directory' /role 1"
        bob <# "'SimpleX Directory'> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /'role 1 observer' to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        -- disable voice messages in the group
        bob ##> "/set voice #privacy off"
        bob <## "updated group preferences:"
        bob <## "Voice messages: off"
        -- cath (old client, max version < v17) joins, /audio hint NOT shown
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group, pending approval"
        cath <# "#privacy (support) 'SimpleX Directory'> Captcha is generated by SimpleX Directory service."
        cath <## ""
        cath <## "Send captcha text to join the group privacy."
        captcha <- dropStrPrefix "#privacy (support) 'SimpleX Directory'> " . dropTime <$> getTermLine cath
        -- /audio unavailable: old client can't receive voice in voice-disabled group
        cath #> "#privacy (support) /audio"
        cath <# "#privacy (support) 'SimpleX Directory'!> > cath /audio"
        cath <## "      Voice captcha is not available - please update SimpleX Chat to v6.5+ or use text captcha."
        -- text captcha still works
        sendCaptcha cath captcha
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: 'SimpleX Directory' added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"
  where
    sendCaptcha cath captcha = do
      cath #> ("#privacy (support) " <> captcha)
      cath <# ("#privacy (support) 'SimpleX Directory'!> > cath " <> captcha)
      cath <## "      Correct, you joined the group privacy"
      cath <## "#privacy: you joined the group"

withDirectoryServiceOpts :: HasCallStack => TestParams -> (DirectoryOpts -> DirectoryOpts) -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceOpts ps modOpts test = do
  dsLink <-
    withNewTestChatCfg ps testCfg serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChatCfg ps testCfg "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        ds ##> "/ad"
        getContactLink ds True
  let opts = modOpts $ mkDirectoryOpts ps [KnownContact 2 "alice"] Nothing Nothing
  runDirectory testCfg opts $
    withTestChatCfg ps testCfg "super_user" $ \superUser -> do
      superUser <## "subscribed 1 connections on server localhost"
      test superUser dsLink

withDirectoryServiceVoiceCaptcha :: HasCallStack => TestParams -> FilePath -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceVoiceCaptcha ps voiceScript =
  withDirectoryServiceOpts ps (\o -> o {voiceCaptchaGenerator = Just voiceScript})

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
  forM_ owner_ $ \owner -> do
    ownerName <- userName owner
    su <## ("Owner: " <> ownerName)
  su <## (show count <> " members")
  su <## ("Status: " <> status)
  su <## ("/'role " <> show gId <> "', /'filter " <> show gId <> "'")

reapproveGroup :: HasCallStack => Int -> TestCC -> TestCC -> IO ()
reapproveGroup count superUser bob = reapproveGroup_ count superUser bob Nothing

reapproveGroup_ :: HasCallStack => Int -> TestCC -> TestCC -> Maybe String -> IO ()
reapproveGroup_ count superUser bob welcome_ = do
  superUser <# "'SimpleX Directory'> bob submitted the group ID 1:"
  superUser <##. "privacy ("
  forM_ welcome_ $ \welcome -> do
    superUser <## "Welcome message:"
    superUser <## welcome
  superUser <## (show count <> " members")
  superUser <## ""
  superUser <## "To approve send:"
  superUser <# "'SimpleX Directory'> /approve 1:privacy 1"
  superUser #> "@'SimpleX Directory' /approve 1:privacy 1"
  superUser <# "'SimpleX Directory'> > /approve 1:privacy 1"
  superUser <## "      Group approved!"
  void $ groupApprovedNotification bob "privacy" 1

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
        enableNamesRole ds
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
runDirectory cfg opts action = do
  t <- forkIO $ directoryService opts cfg
  threadDelay 500000
  action `finally` killThread t

registerGroup :: TestCC -> TestCC -> String -> String -> IO ()
registerGroup su u n fn = registerGroupId su u n fn 1 1

registerGroupId :: TestCC -> TestCC -> String -> String -> Int -> Int -> IO ()
registerGroupId su u n fn gId ugId = do
  submitGroup u n fn
  groupAccepted u n ugId
  void $ completeRegistrationId su u n fn gId ugId

submitGroup :: TestCC -> String -> String -> IO ()
submitGroup u n fn = do
  u ##> ("/g " <> viewName n <> if null fn then "" else " " <> fn)
  u <## ("group #" <> viewName n <> (if null fn then "" else " (" <> fn <> ")") <> " is created")
  u <## ("to add members use /a " <> viewName n <> " <name> or /create link #" <> viewName n)
  u ##> ("/a " <> viewName n <> " 'SimpleX Directory' admin")
  u <## ("invitation to join the group #" <> viewName n <> " sent to 'SimpleX Directory'")

groupAccepted :: TestCC -> String -> Int -> IO ()
groupAccepted u n ugId = do
  u <###
    [ WithTime ("'SimpleX Directory'> Joining the group " <> n <> "…"),
      ConsoleString ("#" <> viewName n <> ": 'SimpleX Directory' joined the group")
    ]
  u <# ("'SimpleX Directory'> Joined the group " <> n <> ". Registration is pending approval — it may take up to 48 hours.")
  u <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
  u <## ("Captcha verification is enabled. Use /'filter " <> show ugId <> "' to change it.")

completeRegistration :: TestCC -> TestCC -> String -> String -> Int -> IO String
completeRegistration su u n fn gId =
  completeRegistrationId su u n fn gId gId

completeRegistrationId :: TestCC -> TestCC -> String -> String -> Int -> Int -> IO String
completeRegistrationId su u n fn gId ugId = do
  notifySuperUser su u n fn gId
  approveRegistrationId su u n gId ugId

notifySuperUser :: TestCC -> TestCC -> String -> String -> Int -> IO ()
notifySuperUser su u n fn gId = notifySuperUser_ su u n fn Nothing gId 1

notifySuperUser_ :: TestCC -> TestCC -> String -> String -> Maybe String -> Int -> Int -> IO ()
notifySuperUser_ su u n fn welcome_ gId gaId = do
  uName <- userName u
  su <# ("'SimpleX Directory'> " <> uName <> " submitted the group ID " <> show gId <> ":")
  su <## (n <> if null fn then "" else " (" <> fn <> ")")
  forM_ welcome_ $ \welcome -> do
    su <## "Welcome message:"
    su <## welcome
  su .<## "members"
  su <## ""
  su <## "To approve send:"
  let approve = "/approve " <> show gId <> ":" <> viewName n <> " " <> show gaId
  su <# ("'SimpleX Directory'> " <> approve)

approveRegistration :: TestCC -> TestCC -> String -> Int -> IO String
approveRegistration su u n gId =
  approveRegistrationId su u n gId gId

approveRegistrationId :: TestCC -> TestCC -> String -> Int -> Int -> IO String
approveRegistrationId su u n gId ugId = approveRegistration_ su u n gId ugId 1

approveRegistration_ :: TestCC -> TestCC -> String -> Int -> Int -> Int -> IO String
approveRegistration_ su u n gId ugId gaId = do
  let approve = "/approve " <> show gId <> ":" <> viewName n <> " " <> show gaId
  su #> ("@'SimpleX Directory' " <> approve)
  su <# ("'SimpleX Directory'> > " <> approve)
  su <## "      Group approved!"
  groupApprovedNotification u n ugId

groupApprovedNotification :: TestCC -> String -> Int -> IO String
groupApprovedNotification u n ugId = do
  u <# ("'SimpleX Directory'> The group ID " <> show ugId <> " (" <> n <> ") is approved and listed in directory - please moderate it!")
  u <## "To help people join, copy the next message with the group link and add it to the end of the group welcome message. The group will remain listed. Any other change to the group profile hides it from the directory until it is re-approved."
  u <## ""
  u <## "Supported commands:"
  u <## ("/'filter " <> show ugId <> "' - to configure anti-spam filter.")
  u <## ("/'role " <> show ugId <> "' - to set default member role.")
  u <## ("/'link " <> show ugId <> "' - to view group link.")
  dropStrPrefix "'SimpleX Directory'> " . dropTime <$> getTermLine u

groupUpdatedHidden :: HasCallStack => TestCC -> TestCC -> String -> String -> IO ()
groupUpdatedHidden superUser u n byMember = do
  u <# ("'SimpleX Directory'> The group ID 1 (" <> n <> ") is updated" <> byMember <> "!")
  u <## "It is hidden from the directory until approved."
  superUser <# ("'SimpleX Directory'> The group ID 1 (" <> n <> ") is updated" <> byMember <> ".")

groupUpdatedListed :: HasCallStack => TestCC -> TestCC -> String -> String -> IO ()
groupUpdatedListed superUser u n byMember = do
  u <# ("'SimpleX Directory'> The group ID 1 (" <> n <> ") is updated" <> byMember <> "!")
  u <## "The group is listed in directory."
  superUser <# ("'SimpleX Directory'> The group ID 1 (" <> n <> ") is updated" <> byMember <> " - only link or whitespace changes.")
  superUser <## "The group remained listed in directory."

setWelcomeMessage :: HasCallStack => TestCC -> [TestCC] -> String -> IO ()
setWelcomeMessage u others welcome = do
  uName <- userName u
  u ##> ("/set welcome #privacy " <> welcome)
  u <## "welcome message changed to:"
  u <## welcome
  forM_ others $ \m -> do
    m <## (uName <> " updated group #privacy:")
    m <## "welcome message changed to:"
    m <## welcome

connectVia :: TestCC -> String -> IO ()
u `connectVia` dsLink = do
  u ##> ("/c " <> dsLink)
  u <## "connection request sent!"
  u .<## ": contact is connected"
  u .<# "> Welcome to SimpleX Directory!"
  u <## ""
  u <## "🔍 Send search string to find groups - try security."
  u <## "/help - how to submit your group or channel."
  u <## "/new - recent groups."
  u <## ""
  u <## "[Directory rules](https://simplex.chat/docs/directory.html)."

joinGroup :: String -> TestCC -> TestCC -> IO ()
joinGroup gName member host = do
  let gn = "#" <> gName
  memberName <- userName member
  member ##> ("/j " <> gName)
  member <## (gn <> ": you joined the group")
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
  u <##. "Link to join the group "
  u <## (show count <> " members")

groupFoundWelcome :: HasCallStack => Int -> TestCC -> String -> String -> IO ()
groupFoundWelcome count u name welcome = do
  u #> ("@'SimpleX Directory' " <> name)
  u <# ("'SimpleX Directory'> > " <> name)
  u <## "      Found 1 group(s)."
  u <#. ("'SimpleX Directory'> " <> name)
  u <## "Welcome message:"
  u <## welcome
  u <##. "Link to join the group "
  u <## (show count <> " members")

getGroupLinkFromBot :: HasCallStack => TestCC -> IO String
getGroupLinkFromBot u = do
  u #> "@'SimpleX Directory' /link 1"
  u <# "'SimpleX Directory'> > /link 1"
  u <## "      The link to join the group ID 1 (privacy):"
  gLink <- getTermLine u
  u <## "New member role: member"
  pure gLink

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

testHelpNoAudio :: HasCallStack => TestParams -> IO ()
testHelpNoAudio ps =
  withDirectoryService ps $ \_ dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      -- commands help should not mention /audio
      bob #> "@'SimpleX Directory' /help commands"
      bob <# "'SimpleX Directory'> /'help commands' - receive this help message."
      bob <## "/help - how to register your group or channel to be added to directory."
      bob <## "/list - list the groups you registered."
      bob <## "`/role <ID>` - view and set default member role for your group."
      bob <## "`/filter <ID>` - view and set spam filter settings for group."
      bob <## "`/link <ID>` - view group link."
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

testRegisterChannelViaCard :: HasCallStack => TestParams -> IO ()
testRegisterChannelViaCard ps =
  withDirectoryServiceCfg ps testCfg $ \superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
      withRelay ps $ \relay -> do
        -- bob connects to directory service first
        bob `connectVia` dsLink
        -- bob creates a channel with a relay
        (_shortLink, _fullLink) <- prepareChannel1Relay "news" bob relay
        -- bob shares the channel card with directory bot
        bob ##> "/share chat #news @'SimpleX Directory'"
        bob <# "@'SimpleX Directory' link to join channel #news (signed):"
        _ <- getTermLine bob -- short link
        _ <- getTermLine bob -- ownerSig JSON
        -- directory bot validates and joins via relay
        bob <# "'SimpleX Directory'> Joining the channel news…"
        concurrentlyN_
          [ do
              relay <## "'SimpleX Directory': accepting request to join group #news..."
              relay <## "#news: 'SimpleX Directory' joined the group",
            bob <## "#news: relay introduced 'SimpleX Directory_1' in the channel"
          ]
        -- owner sends a message to trigger member introduction
        bob <# "'SimpleX Directory'> Joined the channel news. Registration is pending approval — it may take up to 48 hours."
        bob <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
        bob <## "Captcha verification is enabled. Use /'filter 1' to change it."
        superUser <# "'SimpleX Directory'> bob submitted the channel ID 1:"
        superUser <## "news"
        superUser <##. "Link to join channel: "
        superUser <## "You need SimpleX Chat app v6.5 to join."
        superUser <## "1 subscribers"
        superUser <## ""
        superUser <## "To approve send:"
        superUser <# "'SimpleX Directory'> /approve 1:news 1"
        -- superuser approves
        let approve = "/approve 1:news 1"
        superUser #> ("@'SimpleX Directory' " <> approve)
        superUser <# ("'SimpleX Directory'> > " <> approve)
        superUser <## "      Channel approved!"
        bob <# ("'SimpleX Directory'> The channel ID 1 (news) is approved and listed in directory - please moderate it!")
        bob <## "Please note: if you change the channel profile it will be hidden from directory until it is re-approved."
        -- owner updates channel profile, triggering re-approval
        bob ##> "/gp news news News and Updates"
        bob <## "description changed to: News and Updates"
        bob <# "'SimpleX Directory'> The channel ID 1 (news) is updated."
        bob <## "It is hidden from the directory until approved."
        relay <## "bob updated group #news: (signed)"
        relay <## "description changed to: News and Updates"
        superUser <# "'SimpleX Directory'> The channel ID 1 (news) is updated."
        superUser <# ("'SimpleX Directory'> bob submitted the channel ID 1:")
        superUser <## "news (News and Updates)"
        superUser <##. "Link to join channel: "
        superUser <## "You need SimpleX Chat app v6.5 to join."
        superUser <## "2 subscribers"
        superUser <## ""
        superUser <## "To approve send:"
        superUser <# "'SimpleX Directory'> /approve 1:news 1"
        -- re-approve after profile update
        let approve2 = "/approve 1:news 1"
        superUser #> ("@'SimpleX Directory' " <> approve2)
        superUser <# ("'SimpleX Directory'> > " <> approve2)
        superUser <## "      Channel approved!"
        bob <# ("'SimpleX Directory'> The channel ID 1 (news) is approved and listed in directory - please moderate it!")
        bob <## "Please note: if you change the channel profile it will be hidden from directory until it is re-approved."
        -- owner leaves channel, triggering de-listing and bot leaving
        bob ##> "/leave #news"
        concurrentlyN_
          [ do
              bob <## "#news: you left the group"
              bob <## "use /d #news to delete the group",
            relay <## "#news: bob left the group (signed)"
          ]
        bob <# "'SimpleX Directory'> You left the channel ID 1 (news)."
        bob <## ""
        bob <## "The channel is no longer listed in the directory."
        superUser <# "'SimpleX Directory'> The channel ID 1 (news) is de-listed (channel owner left)."
        relay <## "#news: 'SimpleX Directory' left the group (signed)"

-- owner sets a name; directory verifies name<->link consistency and shows the verified name to the admin
testDirectoryChannelName :: HasCallStack => TestParams -> IO ()
testDirectoryChannelName ps = withSmpServerAndNames $ \reg ->
  withDirectoryServiceCfg ps testCfg $ \superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
      withRelay ps $ \relay -> do
        enableNamesRole bob
        bob `connectVia` dsLink
        (shortLink, _fullLink) <- prepareChannel1Relay "news" bob relay
        registerName reg newsName (channelNameRecord "news" (T.pack shortLink))
        bob ##> "/public group access #news domain=news.simplex"
        bob <## "updated public group access: domain=news.simplex"
        relay <## "bob updated group #news: (signed)"
        relay <## "updated public group access: domain=news.simplex"
        bob ##> "/share chat #news @'SimpleX Directory'"
        bob <# "@'SimpleX Directory' link to join channel #news (signed):"
        _ <- getTermLine bob -- short link
        _ <- getTermLine bob -- ownerSig JSON
        bob <# "'SimpleX Directory'> Joining the channel news…"
        concurrentlyN_
          [ do
              relay <## "'SimpleX Directory': accepting request to join group #news..."
              relay <## "#news: 'SimpleX Directory' joined the group",
            bob <## "#news: relay introduced 'SimpleX Directory_1' in the channel"
          ]
        bob <# "'SimpleX Directory'> Joined the channel news. Registration is pending approval — it may take up to 48 hours."
        bob <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
        bob <## "Captcha verification is enabled. Use /'filter 1' to change it."
        -- the directory verified the name against the channel link and shows it to the admin
        superUser <# "'SimpleX Directory'> bob submitted the channel ID 1:"
        superUser <## "news"
        superUser <## "SimpleX name: #news"
        superUser <##. "Link to join channel: "
        superUser <## "You need SimpleX Chat app v6.5 to join."
        superUser <## "1 subscribers"
        superUser <## ""
        superUser <## "To approve send:"
        superUser <# "'SimpleX Directory'> /approve 1:news 1"
  where
    newsName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "news" [])

-- registry re-pointed to a different link after the owner set the name: directory verification fails
testDirectoryChannelNameNotVerified :: HasCallStack => TestParams -> IO ()
testDirectoryChannelNameNotVerified ps = withSmpServerAndNames $ \reg ->
  withDirectoryServiceCfg ps testCfg $ \superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
      withRelay ps $ \relay -> do
        enableNamesRole bob
        bob `connectVia` dsLink
        (shortLink, _fullLink) <- prepareChannel1Relay "news" bob relay
        registerName reg newsName (channelNameRecord "news" (T.pack shortLink))
        bob ##> "/public group access #news domain=news.simplex"
        bob <## "updated public group access: domain=news.simplex"
        relay <## "bob updated group #news: (signed)"
        relay <## "updated public group access: domain=news.simplex"
        -- the name is re-pointed to a different link after the owner set it
        registerName reg newsName (channelNameRecord "news" "https://simplex.chat/other")
        bob ##> "/share chat #news @'SimpleX Directory'"
        bob <# "@'SimpleX Directory' link to join channel #news (signed):"
        _ <- getTermLine bob -- short link
        _ <- getTermLine bob -- ownerSig JSON
        bob <# "'SimpleX Directory'> Joining the channel news…"
        concurrentlyN_
          [ do
              relay <## "'SimpleX Directory': accepting request to join group #news..."
              relay <## "#news: 'SimpleX Directory' joined the group",
            bob <## "#news: relay introduced 'SimpleX Directory_1' in the channel"
          ]
        bob <# "'SimpleX Directory'> Joined the channel news. Registration is pending approval — it may take up to 48 hours."
        bob <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
        bob <## "Captcha verification is enabled. Use /'filter 1' to change it."
        superUser <# "'SimpleX Directory'> bob submitted the channel ID 1:"
        superUser <## "news"
        superUser <## "SimpleX name: #news (NOT verified - will not be shown)"
        superUser <##. "Link to join channel: "
        superUser <## "You need SimpleX Chat app v6.5 to join."
        superUser <## "1 subscribers"
        superUser <## ""
        superUser <## "To approve send:"
        superUser <# "'SimpleX Directory'> /approve 1:news 1"
  where
    newsName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "news" [])

testLinkAsTextSearch :: HasCallStack => TestParams -> IO ()
testLinkAsTextSearch ps =
  withDirectoryServiceCfg ps testCfg $ \_superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
      withRelay ps $ \relay -> do
        bob `connectVia` dsLink
        (shortLink, _fullLink) <- prepareChannel1Relay "news" bob relay
        bob #> ("@'SimpleX Directory' " <> shortLink)
        bob <# ("'SimpleX Directory'> > " <> shortLink)
        bob <## "      No groups found."
        bob <## "To register a group or a channel, please use \"Share via chat\" feature."

testNonOwnerSharesCard :: HasCallStack => TestParams -> IO ()
testNonOwnerSharesCard ps =
  withDirectoryServiceCfg ps testCfg $ \_superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
      withRelay ps $ \relay ->
        withNewTestChatCfg ps testCfg "cath" cathProfile $ \cath -> do
          bob `connectVia` dsLink
          cath `connectVia` dsLink
          (shortLink, fullLink) <- prepareChannel1Relay "news" bob relay
          memberJoinChannel "news" [relay] [bob] shortLink fullLink cath
          cath ##> "/share chat #news @'SimpleX Directory'"
          cath <# "@'SimpleX Directory' link to join channel #news:"
          _ <- getTermLine cath -- short link
          cath <# "'SimpleX Directory'> To add a channel to directory you must be the owner."

testDeleteChannelRegistration :: HasCallStack => TestParams -> IO ()
testDeleteChannelRegistration ps =
  withDirectoryServiceCfg ps testCfg $ \superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
      withRelay ps $ \relay -> do
        bob `connectVia` dsLink
        (_shortLink, _fullLink) <- prepareChannel1Relay "news" bob relay
        bob ##> "/share chat #news @'SimpleX Directory'"
        bob <# "@'SimpleX Directory' link to join channel #news (signed):"
        _ <- getTermLine bob -- short link
        _ <- getTermLine bob -- ownerSig JSON
        bob <# "'SimpleX Directory'> Joining the channel news…"
        concurrentlyN_
          [ do
              relay <## "'SimpleX Directory': accepting request to join group #news..."
              relay <## "#news: 'SimpleX Directory' joined the group",
            bob <## "#news: relay introduced 'SimpleX Directory_1' in the channel"
          ]
        bob <# "'SimpleX Directory'> Joined the channel news. Registration is pending approval — it may take up to 48 hours."
        bob <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
        bob <## "Captcha verification is enabled. Use /'filter 1' to change it."
        superUser <# "'SimpleX Directory'> bob submitted the channel ID 1:"
        superUser <## "news"
        superUser <##. "Link to join channel: "
        superUser <## "You need SimpleX Chat app v6.5 to join."
        superUser <## "1 subscribers"
        superUser <## ""
        superUser <## "To approve send:"
        superUser <# "'SimpleX Directory'> /approve 1:news 1"
        let approve = "/approve 1:news 1"
        superUser #> ("@'SimpleX Directory' " <> approve)
        superUser <# ("'SimpleX Directory'> > " <> approve)
        superUser <## "      Channel approved!"
        bob <# ("'SimpleX Directory'> The channel ID 1 (news) is approved and listed in directory - please moderate it!")
        bob <## "Please note: if you change the channel profile it will be hidden from directory until it is re-approved."
        -- owner deletes registration
        bob #> "@'SimpleX Directory' /delete 1:news"
        bob
          <###
            [ WithTime "'SimpleX Directory'> > /delete 1:news",
              "      Your channel news is deleted from the directory",
              "#news: 'SimpleX Directory_1' left the group (signed)"
            ]
        relay <## "#news: 'SimpleX Directory' left the group (signed)"

testReregistrationAlreadyListed :: HasCallStack => TestParams -> IO ()
testReregistrationAlreadyListed ps =
  withDirectoryServiceCfg ps testCfg $ \superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
      withRelay ps $ \relay -> do
        bob `connectVia` dsLink
        (_shortLink, _fullLink) <- prepareChannel1Relay "news" bob relay
        -- register and approve
        bob ##> "/share chat #news @'SimpleX Directory'"
        bob <# "@'SimpleX Directory' link to join channel #news (signed):"
        _ <- getTermLine bob -- short link
        _ <- getTermLine bob -- ownerSig JSON
        bob <# "'SimpleX Directory'> Joining the channel news…"
        concurrentlyN_
          [ do
              relay <## "'SimpleX Directory': accepting request to join group #news..."
              relay <## "#news: 'SimpleX Directory' joined the group",
            bob <## "#news: relay introduced 'SimpleX Directory_1' in the channel"
          ]
        bob <# "'SimpleX Directory'> Joined the channel news. Registration is pending approval — it may take up to 48 hours."
        bob <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
        bob <## "Captcha verification is enabled. Use /'filter 1' to change it."
        superUser <# "'SimpleX Directory'> bob submitted the channel ID 1:"
        superUser <## "news"
        superUser <##. "Link to join channel: "
        superUser <## "You need SimpleX Chat app v6.5 to join."
        superUser <## "1 subscribers"
        superUser <## ""
        superUser <## "To approve send:"
        superUser <# "'SimpleX Directory'> /approve 1:news 1"
        let approve = "/approve 1:news 1"
        superUser #> ("@'SimpleX Directory' " <> approve)
        superUser <# ("'SimpleX Directory'> > " <> approve)
        superUser <## "      Channel approved!"
        bob <# ("'SimpleX Directory'> The channel ID 1 (news) is approved and listed in directory - please moderate it!")
        bob <## "Please note: if you change the channel profile it will be hidden from directory until it is re-approved."
        -- search finds the channel with its link
        bob #> "@'SimpleX Directory' news"
        bob <# "'SimpleX Directory'> > news"
        bob <## "      Found 1 group(s)."
        bob <# "'SimpleX Directory'> news"
        bob <##. "Link to join channel: "
        bob <## "You need SimpleX Chat app v6.5 to join."
        bob <## "1 subscribers"
        -- owner re-shares card while already listed
        bob ##> "/share chat #news @'SimpleX Directory'"
        bob <# "@'SimpleX Directory' link to join channel #news (signed):"
        _ <- getTermLine bob -- short link
        _ <- getTermLine bob -- ownerSig JSON
        bob <# "'SimpleX Directory'> Channel is already listed in the directory."

testLinkCheckUpdatesCount :: HasCallStack => TestParams -> IO ()
testLinkCheckUpdatesCount ps = do
  dsLink <-
    withNewTestChatCfg ps testCfg serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChatCfg ps testCfg "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        ds ##> "/ad"
        getContactLink ds True
  let opts = (mkDirectoryOpts ps [KnownContact 2 "alice"] Nothing Nothing) {linkCheckInterval = 1}
  runDirectory testCfg opts $
    withTestChatCfg ps testCfg "super_user" $ \superUser -> do
      superUser <## "subscribed 1 connections on server localhost"
      withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob ->
        withRelay ps $ \relay ->
          withNewTestChatCfg ps testCfg "cath" cathProfile $ \cath -> do
            bob `connectVia` dsLink
            (shortLink, fullLink) <- prepareChannel1Relay "news" bob relay
            -- register and approve
            bob ##> "/share chat #news @'SimpleX Directory'"
            bob <# "@'SimpleX Directory' link to join channel #news (signed):"
            _ <- getTermLine bob -- short link
            _ <- getTermLine bob -- ownerSig JSON
            bob <# "'SimpleX Directory'> Joining the channel news…"
            concurrentlyN_
              [ do
                  relay <## "'SimpleX Directory': accepting request to join group #news..."
                  relay <## "#news: 'SimpleX Directory' joined the group",
                bob <## "#news: relay introduced 'SimpleX Directory_1' in the channel"
              ]
            bob <# "'SimpleX Directory'> Joined the channel news. Registration is pending approval — it may take up to 48 hours."
            bob <# "'SimpleX Directory'> We recommend allowing direct messages, media, voice, and SimpleX links only for group moderators and admins. Use group preferences to set them."
            bob <## "Captcha verification is enabled. Use /'filter 1' to change it."
            superUser <# "'SimpleX Directory'> bob submitted the channel ID 1:"
            superUser <## "news"
            superUser <##. "Link to join channel: "
            superUser <## "You need SimpleX Chat app v6.5 to join."
            superUser <## "1 subscribers"
            superUser <## ""
            superUser <## "To approve send:"
            superUser <# "'SimpleX Directory'> /approve 1:news 1"
            let approve = "/approve 1:news 1"
            superUser #> ("@'SimpleX Directory' " <> approve)
            superUser <# ("'SimpleX Directory'> > " <> approve)
            superUser <## "      Channel approved!"
            bob <# ("'SimpleX Directory'> The channel ID 1 (news) is approved and listed in directory - please moderate it!")
            bob <## "Please note: if you change the channel profile it will be hidden from directory until it is re-approved."
            -- link check updates count (bot joined)
            threadDelay 1000000
            bob #> "@'SimpleX Directory' news"
            bob <# "'SimpleX Directory'> > news"
            bob <## "      Found 1 group(s)."
            bob <# "'SimpleX Directory'> news"
            bob <##. "Link to join channel: "
            bob <## "You need SimpleX Chat app v6.5 to join."
            bob <## "2 subscribers"
            -- second subscriber joins
            memberJoinChannel "news" [relay] [bob] shortLink fullLink cath
            -- link check updates count again
            threadDelay 1000000
            bob #> "@'SimpleX Directory' news"
            bob <# "'SimpleX Directory'> > news"
            bob <## "      Found 1 group(s)."
            bob <# "'SimpleX Directory'> news"
            bob <##. "Link to join channel: "
            bob <## "You need SimpleX Chat app v6.5 to join."
            bob <## "3 subscribers"

testGetCaptchaStr :: HasCallStack => TestParams -> IO ()
testGetCaptchaStr _ps = do
  s0 <- getCaptchaStr 0 ""
  s0 `shouldBe` ""
  s7 <- getCaptchaStr 7 ""
  length s7 `shouldBe` 7
  all (`elem` ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" :: String)) s7 `shouldBe` True
