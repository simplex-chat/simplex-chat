{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Bots.DirectoryTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, when)
import qualified Data.Text as T
import Directory.Captcha
import Directory.Options
import Directory.Service
import Directory.Store
import GHC.IO.Handle (hClose)
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller (ChatConfig (..), ChatHooks (..), defaultChatHooks)
import Simplex.Chat.Core
import qualified Simplex.Chat.Markdown as MD
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Types (Profile (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
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
  describe "list groups" $ do
    it "should list user's groups" testListUserGroups
  describe "member admission" $ do
    it "should ask member to pass captcha screen" testCapthaScreening
  describe "store log" $ do
    it "should restore directory service state" testRestoreDirectory
  describe "captcha" $ do
    it "should accept some incorrect spellings" testCaptcha

directoryProfile :: Profile
directoryProfile = Profile {displayName = "SimpleX-Directory", fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}

mkDirectoryOpts :: TestParams -> [KnownContact] -> Maybe KnownGroup -> DirectoryOpts
mkDirectoryOpts TestParams {tmpPath = ps} superUsers ownersGroup =
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
      blockedFragmentsFile = Nothing,
      blockedWordsFile = Nothing,
      blockedExtensionRules = Nothing,
      nameSpellingFile = Nothing,
      profileNameLimit = maxBound,
      captchaGenerator = Nothing,
      directoryLog = Just $ ps </> "directory_service.log",
      serviceName = "SimpleX-Directory",
      runCLI = False,
      searchResults = 3,
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
        bob #> "@SimpleX-Directory privacy"
        bob <# "SimpleX-Directory> > privacy"
        bob <## "      No groups found"
        -- putStrLn "*** create a group"
        bob ##> "/g PSA Privacy, Security & Anonymity"
        bob <## "group #PSA (Privacy, Security & Anonymity) is created"
        bob <## "to add members use /a PSA <name> or /create link #PSA"
        bob ##> "/a PSA SimpleX-Directory member"
        bob <## "invitation to join the group #PSA sent to SimpleX-Directory"
        bob <# "SimpleX-Directory> You must grant directory service admin role to register the group"
        bob ##> "/mr PSA SimpleX-Directory admin"
        -- putStrLn "*** discover service joins group and creates the link for profile"
        bob <## "#PSA: you changed the role of SimpleX-Directory to admin"
        bob <# "SimpleX-Directory> Joining the group PSA…"
        bob <## "#PSA: SimpleX-Directory joined the group"
        bob <# "SimpleX-Directory> Joined the group PSA, creating the link…"
        bob <# "SimpleX-Directory> Created the public link to join the group via this directory service that is always online."
        bob <## ""
        bob <## "Please add it to the group welcome message."
        bob <## "For example, add:"
        welcomeWithLink <- dropStrPrefix "SimpleX-Directory> " . dropTime <$> getTermLine bob
        -- putStrLn "*** update profile without link"
        updateGroupProfile bob "Welcome!"
        bob <# "SimpleX-Directory> The profile updated for ID 1 (PSA), but the group link is not added to the welcome message."
        (superUser </)
        -- putStrLn "*** update profile so that it has link"
        updateGroupProfile bob welcomeWithLink
        bob <# "SimpleX-Directory> Thank you! The group link for ID 1 (PSA) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        approvalRequested superUser welcomeWithLink (1 :: Int)
        -- putStrLn "*** update profile so that it still has link"
        let welcomeWithLink' = "Welcome! " <> welcomeWithLink
        updateGroupProfile bob welcomeWithLink'
        bob <# "SimpleX-Directory> The group ID 1 (PSA) is updated!"
        bob <## "It is hidden from the directory until approved."
        superUser <# "SimpleX-Directory> The group ID 1 (PSA) is updated."
        approvalRequested superUser welcomeWithLink' (2 :: Int)
        -- putStrLn "*** try approving with the old registration code"
        bob #> "@SimpleX-Directory /approve 1:PSA 1"
        bob <# "SimpleX-Directory> > /approve 1:PSA 1"
        bob <## "      You are not allowed to use this command"
        superUser #> "@SimpleX-Directory /approve 1:PSA 1"
        superUser <# "SimpleX-Directory> > /approve 1:PSA 1"
        superUser <## "      Incorrect approval code"
        -- putStrLn "*** update profile so that it has no link"
        updateGroupProfile bob "Welcome!"
        bob <# "SimpleX-Directory> The group link for ID 1 (PSA) is removed from the welcome message."
        bob <## ""
        bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
        superUser <# "SimpleX-Directory> The group link is removed from ID 1 (PSA), de-listed."
        superUser #> "@SimpleX-Directory /approve 1:PSA 2"
        superUser <# "SimpleX-Directory> > /approve 1:PSA 2"
        superUser <## "      Error: the group ID 1 (PSA) is not pending approval."
        -- putStrLn "*** update profile so that it has link again"
        updateGroupProfile bob welcomeWithLink'
        bob <# "SimpleX-Directory> Thank you! The group link for ID 1 (PSA) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        approvalRequested superUser welcomeWithLink' (1 :: Int)
        superUser #> "@SimpleX-Directory /pending"
        superUser <# "SimpleX-Directory> > /pending"
        superUser <## "      1 registered group(s)"
        superUser <# "SimpleX-Directory> 1. PSA (Privacy, Security & Anonymity)"
        superUser <## "Welcome message:"
        superUser <##. "Welcome! Link to join the group PSA: "
        superUser <## "Owner: bob"
        superUser <## "2 members"
        superUser <## "Status: pending admin approval"
        superUser #> "@SimpleX-Directory /approve 1:PSA 1"
        superUser <# "SimpleX-Directory> > /approve 1:PSA 1"
        superUser <## "      Group approved!"
        bob <# "SimpleX-Directory> The group ID 1 (PSA) is approved and listed in directory - please moderate it!"
        bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
        bob <## ""
        bob <## "Supported commands:"
        bob <## "- /filter 1 - to configure anti-spam filter."
        bob <## "- /role 1 - to set default member role."
        bob <## "- /help commands - other commands."
        search bob "privacy" welcomeWithLink'
        search bob "security" welcomeWithLink'
        cath `connectVia` dsLink
        search cath "privacy" welcomeWithLink'
        bob #> "@SimpleX-Directory /exec /contacts"
        bob <# "SimpleX-Directory> > /exec /contacts"
        bob <## "      You are not allowed to use this command"
        superUser #> "@SimpleX-Directory /exec /contacts"
        superUser <# "SimpleX-Directory> > /exec /contacts"
        superUser <## "      alice (Alice)"
        superUser <## "bob (Bob)"
        superUser <## "cath (Catherine)"
  where
    search u s welcome = do
      u #> ("@SimpleX-Directory " <> s)
      u <# ("SimpleX-Directory> > " <> s)
      u <## "      Found 1 group(s)."
      u <# "SimpleX-Directory> PSA (Privacy, Security & Anonymity)"
      u <## "Welcome message:"
      u <## welcome
      u <## "2 members"
    updateGroupProfile u welcome = do
      u ##> ("/set welcome #PSA " <> welcome)
      u <## "description changed to:"
      u <## welcome
    approvalRequested su welcome grId = do
      su <# "SimpleX-Directory> bob submitted the group ID 1:"
      su <## "PSA (Privacy, Security & Anonymity)"
      su <## "Welcome message:"
      su <## welcome
      su <## "2 members"
      su <## ""
      su <## "To approve send:"
      su <# ("SimpleX-Directory> /approve 1:PSA " <> show grId)

testSuspendResume :: HasCallStack => TestParams -> IO ()
testSuspendResume ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      groupFound bob "privacy"
      superUser #> "@SimpleX-Directory /suspend 1:privacy"
      superUser <# "SimpleX-Directory> > /suspend 1:privacy"
      superUser <## "      Group suspended!"
      bob <# "SimpleX-Directory> The group ID 1 (privacy) is suspended and hidden from directory. Please contact the administrators."
      groupNotFound bob "privacy"
      superUser #> "@SimpleX-Directory /resume 1:privacy"
      superUser <# "SimpleX-Directory> > /resume 1:privacy"
      superUser <## "      Group listing resumed!"
      bob <# "SimpleX-Directory> The group ID 1 (privacy) is listed in the directory again!"
      groupFound bob "privacy"
      superUser #> "@SimpleX-Directory privacy"
      groupFoundN_ "" (Just 1) 2 superUser "privacy"
      superUser #> "@SimpleX-Directory /link 1:privacy"
      superUser <# "SimpleX-Directory> > /link 1:privacy"
      superUser <## "      The link to join the group ID 1 (privacy):"
      superUser <##. "https://simplex.chat/contact"
      superUser <## "New member role: member"
      superUser #> "@SimpleX-Directory /owner 1:privacy hello there"
      superUser <# "SimpleX-Directory> > /owner 1:privacy hello there"
      superUser <## "      Forwarded to @bob, the owner of the group ID 1 (privacy)"
      bob <# "SimpleX-Directory> hello there"

testDeleteGroup :: HasCallStack => TestParams -> IO ()
testDeleteGroup ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      groupFound bob "privacy"
      bob #> "@SimpleX-Directory /delete 1:privacy"
      bob <# "SimpleX-Directory> > /delete 1:privacy"
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
        superUser #> "@SimpleX-Directory /last"
        superUser <# "SimpleX-Directory> > /last"
        superUser <## "      2 registered group(s)"
        memberGroupListing superUser bob 1 "privacy" "Privacy" 2 "active"
        memberGroupListing superUser cath 2 "security" "Security" 2 "active"
        -- trying to register group with the same name
        submitGroup bob "security" "Security"
        bob <# "SimpleX-Directory> The group security (Security) is already listed in the directory, please choose another name."
        bob ##> "/d #security"
        bob <## "#security: you deleted the group"
        -- admin can delete the group
        superUser #> "@SimpleX-Directory /delete 2:security"
        superUser <# "SimpleX-Directory> > /delete 2:security"
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
        bob #> "@SimpleX-Directory /role 1:privacy observer"
        bob <# "SimpleX-Directory> > /role 1:privacy observer"
        bob <## "      The initial member role for the group privacy is set to observer"
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        cath ##> ("/c " <> groupLink)
        cath <## "connection request sent!"
        cath <## "#privacy: joining the group..."
        cath <## "#privacy: you joined the group"
        cath <#. "#privacy SimpleX-Directory> Link to join the group privacy: https://simplex.chat/"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: SimpleX-Directory added cath (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath is connected"
        bob ##> "/ms #privacy"
        bob <## "bob (Bob): owner, you, created group"
        bob <## "SimpleX-Directory: admin, invited, connected"
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
          cath #> "@SimpleX-Directory privacy"
          cath <# "SimpleX-Directory> > privacy"
          cath <## "      Found 1 group(s)."
          cath <# "SimpleX-Directory> privacy (Privacy)"
          cath <## "Welcome message:"
          welcomeMsg <- getTermLine cath
          let groupLink = dropStrPrefix "Link to join the group privacy: " welcomeMsg
          cath <## "2 members"
          cath ##> ("/c " <> groupLink)
          cath <## "connection request sent!"
          cath <## "#privacy: joining the group..."
          cath <## "#privacy: you joined the group"
          cath <## "contact and member are merged: SimpleX-Directory, #privacy SimpleX-Directory_1"
          cath <## "use @SimpleX-Directory <message> to send messages"
          cath <# ("#privacy SimpleX-Directory> " <> welcomeMsg)
          cath <## "#privacy: member bob (Bob) is connected"
          bob <## "#privacy: SimpleX-Directory added cath (Catherine) to the group (connecting...)"
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
                  <### [ "#privacy: member SimpleX-Directory is connected",
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
      superUser #> "@SimpleX-Directory /suspend 1:'Privacy & Security'"
      superUser <# "SimpleX-Directory> > /suspend 1:'Privacy & Security'"
      superUser <## "      Group suspended!"
      bob <# "SimpleX-Directory> The group ID 1 (Privacy & Security) is suspended and hidden from directory. Please contact the administrators."
      groupNotFound bob "privacy"
      superUser #> "@SimpleX-Directory /resume 1:'Privacy & Security'"
      superUser <# "SimpleX-Directory> > /resume 1:'Privacy & Security'"
      superUser <## "      Group listing resumed!"
      bob <# "SimpleX-Directory> The group ID 1 (Privacy & Security) is listed in the directory again!"
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
        cath <## "#MyGroup: member SimpleX-Directory_1 is connected"
        cath <## "contact and member are merged: SimpleX-Directory, #MyGroup SimpleX-Directory_1"
        cath <## "use @SimpleX-Directory <message> to send messages"
        cath #> "@SimpleX-Directory MyGroup"
        cath <# "SimpleX-Directory> > MyGroup"
        cath <## "      Found 7 group(s), sending top 3."
        receivedGroup cath 0 3
        receivedGroup cath 1 2
        receivedGroup cath 2 2
        cath <# "SimpleX-Directory> Send /next or just . for 4 more result(s)."
        cath #> "@SimpleX-Directory /next"
        cath <# "SimpleX-Directory> > /next"
        cath <## "      Sending 3 more group(s)."
        receivedGroup cath 3 2
        receivedGroup cath 4 2
        receivedGroup cath 5 2
        cath <# "SimpleX-Directory> Send /next or just . for 1 more result(s)."
        -- search of another user does not affect the search of the first user
        groupFound bob "Another"
        cath #> "@SimpleX-Directory ."
        cath <# "SimpleX-Directory> > ."
        cath <## "      Sending 1 more group(s)."
        receivedGroup cath 6 2
        cath #> "@SimpleX-Directory /all"
        cath <# "SimpleX-Directory> > /all"
        cath <## "      8 group(s) listed, sending top 3."
        receivedGroup cath 0 3
        receivedGroup cath 1 2
        receivedGroup cath 2 2
        cath <# "SimpleX-Directory> Send /next or just . for 5 more result(s)."
        cath #> "@SimpleX-Directory /new"
        cath <# "SimpleX-Directory> > /new"
        cath <## "      8 group(s) listed, sending the most recent 3."
        receivedGroup cath 7 2
        receivedGroup cath 6 2
        receivedGroup cath 5 2
        cath <# "SimpleX-Directory> Send /next or just . for 5 more result(s)."
        cath #> "@SimpleX-Directory term3"
        cath <# "SimpleX-Directory> > term3"
        cath <## "      Found 3 group(s)."
        receivedGroup cath 4 2
        receivedGroup cath 5 2
        receivedGroup cath 6 2
        cath #> "@SimpleX-Directory term1"
        cath <# "SimpleX-Directory> > term1"
        cath <## "      Found 6 group(s), sending top 3."
        receivedGroup cath 1 2
        receivedGroup cath 2 2
        receivedGroup cath 3 2
        cath <# "SimpleX-Directory> Send /next or just . for 3 more result(s)."
        cath #> "@SimpleX-Directory ."
        cath <# "SimpleX-Directory> > ."
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
      u <#. ("SimpleX-Directory> " <> groups !! ix)
      u <## "Welcome message:"
      u <##. "Link to join the group "
      u <## (show count <> " members")

testInviteToOwnersGroup :: HasCallStack => TestParams -> IO ()
testInviteToOwnersGroup ps =
  withDirectoryServiceCfgOwnersGroup ps testCfg True $ \superUser dsLink ->
    withNewTestChatCfg ps testCfg "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroupId superUser bob "privacy" "Privacy" 2 1
      bob <## "#owners: SimpleX-Directory invites you to join the group as member"
      bob <## "use /j owners to accept"
      superUser <## "Invited @bob, the owner of the group ID 2 (privacy) to owners' group owners"
      bob ##> "/j owners"
      bob <## "#owners: you joined the group"
      bob <## "#owners: member alice (Alice) is connected"
      superUser <## "#owners: SimpleX-Directory added bob (Bob) to the group (connecting...)"
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
        bob <# "SimpleX-Directory> You left the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (group owner left)."
        cath `connectVia` dsLink
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
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
        bob <# "SimpleX-Directory> You are removed from the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (group owner is removed)."
        cath `connectVia` dsLink
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
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
        cath #> "@SimpleX-Directory_1 privacy"
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
        cath #> "@SimpleX-Directory_1 privacy"
        groupFoundN_ "_1" Nothing 2 cath "privacy"

testDelistedServiceRemoved :: HasCallStack => TestParams -> IO ()
testDelistedServiceRemoved ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        bob ##> "/rm #privacy SimpleX-Directory"
        bob <## "#privacy: you removed SimpleX-Directory from the group"
        cath <## "#privacy: bob removed SimpleX-Directory from the group"
        bob <# "SimpleX-Directory> SimpleX-Directory is removed from the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (directory service is removed)."
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
        cath <## "#privacy: member SimpleX-Directory_1 is connected"
        cath <## "contact and member are merged: SimpleX-Directory, #privacy SimpleX-Directory_1"
        cath <## "use @SimpleX-Directory <message> to send messages"
        bob ##> "/d #privacy"
        bob <## "#privacy: you deleted the group"
        bob <# "SimpleX-Directory> The group ID 1 (privacy) is deleted."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        cath <## "#privacy: bob deleted the group"
        cath <## "use /d #privacy to delete the local copy of the group"
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (group is deleted)."
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
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
        groupFoundN 3 cath "privacy"
        -- de-listed if service role changed
        bob ##> "/mr privacy SimpleX-Directory member"
        bob <## "#privacy: you changed the role of SimpleX-Directory to member"
        cath <## "#privacy: bob changed the role of SimpleX-Directory from admin to member"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to member."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (SimpleX-Directory role is changed to member)."
        groupNotFound cath "privacy"
        -- re-listed if service role changed back without profile changes
        cath ##> "/mr privacy SimpleX-Directory admin"
        cath <## "#privacy: you changed the role of SimpleX-Directory to admin"
        bob <## "#privacy: cath changed the role of SimpleX-Directory from member to admin"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is listed in the directory again."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is listed (SimpleX-Directory role is changed to admin)."
        groupFoundN 3 cath "privacy"
        -- de-listed if owner role changed
        cath ##> "/mr privacy bob admin"
        cath <## "#privacy: you changed the role of bob to admin"
        bob <## "#privacy: cath changed your role from owner to admin"
        bob <# "SimpleX-Directory> Your role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (user role is set to admin)."
        groupNotFound cath "privacy"
        -- re-listed if owner role changed back without profile changes
        cath ##> "/mr privacy bob owner"
        cath <## "#privacy: you changed the role of bob to owner"
        bob <## "#privacy: cath changed your role from admin to owner"
        bob <# "SimpleX-Directory> Your role in the group ID 1 (privacy) is changed to owner."
        bob <## ""
        bob <## "The group is listed in the directory again."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is listed (user role is set to owner)."
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
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
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
        bob ##> "/mr privacy SimpleX-Directory member"
        bob <## "#privacy: you changed the role of SimpleX-Directory to member"
        updateProfileWithLink bob "privacy" welcomeWithLink 1
        bob <# "SimpleX-Directory> You must grant directory service admin role to register the group"
        bob ##> "/mr privacy SimpleX-Directory admin"
        bob <## "#privacy: you changed the role of SimpleX-Directory to admin"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to admin."
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
        bob ##> "/mr privacy SimpleX-Directory member"
        bob <## "#privacy: you changed the role of SimpleX-Directory to member"
        let approve = "/approve 1:privacy 1"
        superUser #> ("@SimpleX-Directory " <> approve)
        superUser <# ("SimpleX-Directory> > " <> approve)
        superUser <## "      Group is not approved: SimpleX-Directory is not an admin."
        groupNotFound cath "privacy"
        bob ##> "/mr privacy SimpleX-Directory admin"
        bob <## "#privacy: you changed the role of SimpleX-Directory to admin"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to admin."
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
        bob <## "full name changed to: Privacy and Security"
        bob <# "SimpleX-Directory> The group ID 1 (privacy) is updated!"
        bob <## "It is hidden from the directory until approved."
        cath <## "bob updated group #privacy:"
        cath <## "full name changed to: Privacy and Security"
        cath `connectVia` dsLink
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
        groupNotFound cath "privacy"
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is updated."
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
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
        cath ##> "/gp privacy privacy Privacy and Security"
        cath <## "full name changed to: Privacy and Security"
        bob <## "cath updated group #privacy:"
        bob <## "full name changed to: Privacy and Security"
        bob <# "SimpleX-Directory> The group ID 1 (privacy) is updated by cath!"
        bob <## "It is hidden from the directory until approved."
        groupNotFound cath "privacy"
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is updated by cath."
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
          cath <## "full name changed to: Privacy and Security"
          bob <## "cath updated group #privacy:"
          bob <## "full name changed to: Privacy and Security"
          bob <# "SimpleX-Directory> The group ID 1 (privacy) is updated by cath!"
          bob <## "It is hidden from the directory until approved."
          groupNotFound dan "privacy"
          superUser <# "SimpleX-Directory> The group ID 1 (privacy) is updated by cath."
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
        bob <## "description changed to:"
        bob <## "Welcome!"
        bob <# "SimpleX-Directory> The group link for ID 1 (privacy) is removed from the welcome message."
        bob <## ""
        bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
        cath <## "bob updated group #privacy:"
        cath <## "description changed to:"
        cath <## "Welcome!"
        superUser <# "SimpleX-Directory> The group link is removed from ID 1 (privacy), de-listed."
        cath `connectVia` dsLink
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
        groupNotFound cath "privacy"
        let withChangedLink = T.unpack $ T.replace "contact#/?v=2-7&" "contact#/?v=3-7&" $ T.pack welcomeWithLink
        bob ##> ("/set welcome #privacy " <> withChangedLink)
        bob <## "description changed to:"
        bob <## withChangedLink
        bob <# "SimpleX-Directory> Thank you! The group link for ID 1 (privacy) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        cath <## "bob updated group #privacy:"
        cath <## "description changed to:"
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
        cath <## "contact and member are merged: SimpleX-Directory_1, #privacy SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
        bob ##> "/show welcome #privacy"
        bob <## "Welcome message:"
        welcomeWithLink <- getTermLine bob
        cath ##> "/set welcome #privacy Welcome!"
        cath <## "description changed to:"
        cath <## "Welcome!"
        bob <## "cath updated group #privacy:"
        bob <## "description changed to:"
        bob <## "Welcome!"
        bob <# "SimpleX-Directory> The group link for ID 1 (privacy) is removed from the welcome message by cath."
        bob <## ""
        bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
        superUser <# "SimpleX-Directory> The group link is removed from ID 1 (privacy), de-listed."
        groupNotFound cath "privacy"
        cath ##> ("/set welcome #privacy " <> welcomeWithLink)
        cath <## "description changed to:"
        cath <## welcomeWithLink
        bob <## "cath updated group #privacy:"
        bob <## "description changed to:"
        bob <## welcomeWithLink
        bob <# "SimpleX-Directory> Thank you! The group link for ID 1 (privacy) is added to the welcome message by cath."
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
          cath <## "description changed to:"
          cath <## "Welcome!"
          bob <## "cath updated group #privacy:"
          bob <## "description changed to:"
          bob <## "Welcome!"
          bob <# "SimpleX-Directory> The group link for ID 1 (privacy) is removed from the welcome message by cath."
          bob <## ""
          bob <## "The group is hidden from the directory until the group link is added and the group is re-approved."
          superUser <# "SimpleX-Directory> The group link is removed from ID 1 (privacy), de-listed."
          groupNotFound dan "privacy"
          cath ##> ("/set welcome #privacy " <> welcomeWithLink)
          cath <## "description changed to:"
          cath <## welcomeWithLink
          bob <## "cath updated group #privacy:"
          bob <## "description changed to:"
          bob <## welcomeWithLink
          -- bob <# "SimpleX-Directory> The group link is added by another group member, your registration will not be processed."
          -- bob <## ""
          -- bob <## "Please update the group profile yourself."
          -- bob ##> ("/set welcome #privacy " <> welcomeWithLink <> " - welcome!")
          -- bob <## "description changed to:"
          -- bob <## (welcomeWithLink <> " - welcome!")
          bob <# "SimpleX-Directory> Thank you! The group link for ID 1 (privacy) is added to the welcome message by cath."
          bob <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
          -- cath <## "bob updated group #privacy:"
          -- cath <## "description changed to:"
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
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "SimpleX-Directory> /confirm 1:privacy"
        cath #> "@SimpleX-Directory /confirm 1:privacy"
        welcomeWithLink <- groupAccepted cath "privacy"
        groupNotFound bob "privacy"
        completeRegistration superUser cath "privacy" "Privacy" welcomeWithLink 2
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
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already listed in the directory, please choose another name."

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
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "SimpleX-Directory> /confirm 1:privacy"
        groupNotFound cath "privacy"
        completeRegistration superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupFound cath "privacy"
        cath #> "@SimpleX-Directory /confirm 1:privacy"
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already listed in the directory, please choose another name."

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
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "SimpleX-Directory> /confirm 1:privacy"
        cath #> "@SimpleX-Directory /confirm 1:privacy"
        welcomeWithLink' <- groupAccepted cath "privacy"
        groupNotFound cath "privacy"
        completeRegistration superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupFound cath "privacy"
        cath ##> ("/set welcome privacy " <> welcomeWithLink')
        cath <## "description changed to:"
        cath <## welcomeWithLink'
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already listed in the directory, please choose another name."
        cath ##> "/gp privacy security Security"
        cath <## "changed to #security (Security)"
        cath <# "SimpleX-Directory> Thank you! The group link for ID 2 (security) is added to the welcome message."
        cath <## "You will be notified once the group is added to the directory - it may take up to 48 hours."
        notifySuperUser superUser cath "security" "Security" welcomeWithLink' 2
        approveRegistration superUser cath "security" 2
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
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already submitted to the directory."
        cath <## "To confirm the registration, please send:"
        cath <# "SimpleX-Directory> /confirm 1:privacy"
        cath #> "@SimpleX-Directory /confirm 1:privacy"
        welcomeWithLink' <- groupAccepted cath "privacy"
        updateProfileWithLink cath "privacy" welcomeWithLink' 2
        notifySuperUser superUser cath "privacy" "Privacy" welcomeWithLink' 2
        groupNotFound cath "privacy"
        completeRegistration superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupFound cath "privacy"
        -- fails at approval, as already listed
        let approve = "/approve 2:privacy 1"
        superUser #> ("@SimpleX-Directory " <> approve)
        superUser <# ("SimpleX-Directory> > " <> approve)
        superUser <## "      The group ID 2 (privacy) is already listed in the directory."

testListUserGroups :: HasCallStack => TestParams -> IO ()
testListUserGroups ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        connectUsers bob cath
        fullAddMember "privacy" "Privacy" bob cath GRMember
        joinGroup "privacy" cath bob
        cath <## "#privacy: member SimpleX-Directory_1 is connected"
        cath <## "contact and member are merged: SimpleX-Directory, #privacy SimpleX-Directory_1"
        cath <## "use @SimpleX-Directory <message> to send messages"
        registerGroupId superUser bob "security" "Security" 2 2
        registerGroupId superUser cath "anonymity" "Anonymity" 3 1
        listUserGroup cath "anonymity" "Anonymity"
        -- with de-listed group
        groupFound cath "anonymity"
        cath ##> "/mr anonymity SimpleX-Directory member"
        cath <## "#anonymity: you changed the role of SimpleX-Directory to member"
        cath <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (anonymity) is changed to member."
        cath <## ""
        cath <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 3 (anonymity) is de-listed (SimpleX-Directory role is changed to member)."
        groupNotFound cath "anonymity"
        listGroups superUser bob cath

testCapthaScreening :: HasCallStack => TestParams -> IO ()
testCapthaScreening ps =
  withDirectoryService ps $ \superUser dsLink ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        -- check default role
        bob #> "@SimpleX-Directory /role 1"
        bob <# "SimpleX-Directory> > /role 1"
        bob <## "      The initial member role for the group privacy is set to member"
        bob <## "Send /role 1 observer to change it."
        bob <## ""
        note <- getTermLine bob
        let groupLink = dropStrPrefix "Please note: it applies only to members joining via this link: " note
        -- enable captcha
        bob #> "@SimpleX-Directory /filter 1 captcha"
        bob <# "SimpleX-Directory> > /filter 1 captcha"
        bob <## "      Spam filter settings for group privacy set to:"
        bob <## "- reject long/inappropriate names: disabled"
        bob <## "- pass captcha to join: enabled"
        bob <## ""
        bob <## "Use /filter 1 [name] [captcha] to enable and /filter 1 off to disable filter."
        -- connect with captcha screen
        _ <- join cath groupLink
        cath #> "#privacy 123" -- sending incorrect captcha
        cath <# "#privacy SimpleX-Directory!> > cath 123"
        cath <## "      Incorrect text, please try again."
        captcha <- dropStrPrefix "#privacy SimpleX-Directory> " . dropTime <$> getTermLine cath
        sendCaptcha cath captcha
        cath <#. "#privacy SimpleX-Directory> Link to join the group privacy: https://"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: SimpleX-Directory added cath (Catherine) to the group (connecting...)"
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
        bob #> "@SimpleX-Directory /role 1 observer"
        bob <# "SimpleX-Directory> > /role 1 observer"
        bob <## "      The initial member role for the group privacy is set to observer"
        bob <## ""
        bob <##. "Please note: it applies only to members joining via this link: https://"
        -- connect with captcha screen again, as observer
        captcha' <- join cath groupLink
        sendCaptcha cath captcha'
        -- message from cath that left
        pastMember <- dropStrPrefix "#privacy: SimpleX-Directory forwarded a message from an unknown member, creating unknown member record " <$> getTermLine cath
        cath <# ("#privacy " <> pastMember <> "> hello [>>]")
        cath <#. "#privacy SimpleX-Directory> Link to join the group privacy: https://"
        cath <## "#privacy: member bob (Bob) is connected"
        bob <## "#privacy: SimpleX-Directory added cath_1 (Catherine) to the group (connecting...)"
        bob <## "#privacy: new member cath_1 is connected"
        cath ##> "#privacy hello"
        cath <## "#privacy: you don't have permission to send messages"
        (bob </)
        cath ##> "/ms privacy"
        cath <## "cath (Catherine): observer, you, connected"
        cath <## "SimpleX-Directory: admin, host, connected"
        cath <## "bob (Bob): owner, connected"
        cath <## (pastMember <> ": author, status unknown")
  where
    join cath groupLink = do
      cath ##> ("/c " <> groupLink)
      cath <## "connection request sent!"
      cath <## "#privacy: joining the group..."
      cath <## "#privacy: you joined the group, pending approval"
      cath <# "#privacy SimpleX-Directory> Captcha is generated by SimpleX Directory service."
      cath <## ""
      cath <## "Send captcha text to join the group privacy."
      dropStrPrefix "#privacy SimpleX-Directory> " . dropTime <$> getTermLine cath
    sendCaptcha cath captcha = do
      cath #> ("#privacy " <> captcha)
      cath <# ("#privacy SimpleX-Directory!> > cath " <> captcha)
      cath <## "      Correct, you joined the group privacy"
      cath <## "#privacy: you joined the group"

testRestoreDirectory :: HasCallStack => TestParams -> IO ()
testRestoreDirectory ps = do
  testListUserGroups ps
  restoreDirectoryService ps 3 3 $ \superUser _dsLink ->
    withTestChat ps "bob" $ \bob ->
      withTestChat ps "cath" $ \cath -> do
        bob <## "2 contacts connected (use /cs for the list)"
        bob
          <### [ "#privacy: connected to server(s)",
                 "#security: connected to server(s)"
               ]
        cath <## "2 contacts connected (use /cs for the list)"
        cath
          <### [ "#privacy: connected to server(s)",
                 "#anonymity: connected to server(s)"
               ]
        listGroups superUser bob cath
        groupFoundN 3 bob "privacy"
        groupFound bob "security"
        groupFoundN 3 cath "privacy"
        cath #> "@SimpleX-Directory security"
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
  bob #> "@SimpleX-Directory /last"
  bob <# "SimpleX-Directory> > /last"
  bob <## "      You are not allowed to use this command"
  superUser #> "@SimpleX-Directory /last"
  superUser <# "SimpleX-Directory> > /last"
  superUser <## "      3 registered group(s)"
  memberGroupListing superUser bob 1 "privacy" "Privacy" 3 "active"
  memberGroupListing superUser bob 2 "security" "Security" 2 "active"
  memberGroupListing superUser cath 3 "anonymity" "Anonymity" 2 "suspended because roles changed"
  -- showing last 1 group
  superUser #> "@SimpleX-Directory /last 1"
  superUser <# "SimpleX-Directory> > /last 1"
  superUser <## "      3 registered group(s), showing the last 1"
  memberGroupListing superUser cath 3 "anonymity" "Anonymity" 2 "suspended because roles changed"

listUserGroup :: HasCallStack => TestCC -> String -> String -> IO ()
listUserGroup u n fn = do
  sendListCommand u 1
  groupListing u 1 n fn 2 "active"

sendListCommand :: HasCallStack => TestCC -> Int -> IO ()
sendListCommand u count = do
  u #> "@SimpleX-Directory /list"
  u <# "SimpleX-Directory> > /list"
  u <## ("      " <> show count <> " registered group(s)")

groupListing :: HasCallStack => TestCC -> Int -> String -> String -> Int -> String -> IO ()
groupListing u = groupListing_ u Nothing

memberGroupListing :: HasCallStack => TestCC -> TestCC -> Int -> String -> String -> Int -> String -> IO ()
memberGroupListing su owner = groupListing_ su (Just owner)

groupListing_ :: HasCallStack => TestCC -> Maybe TestCC -> Int -> String -> String -> Int -> String -> IO ()
groupListing_ su owner_ gId n fn count status = do
  su <# ("SimpleX-Directory> " <> show gId <> ". " <> n <> " (" <> fn <> ")")
  su <## "Welcome message:"
  su <##. ("Link to join the group " <> n <> ": ")
  forM_ owner_ $ \owner -> do
    ownerName <- userName owner
    su <## ("Owner: " <> ownerName)
  su <## (show count <> " members")
  su <## ("Status: " <> status)

reapproveGroup :: HasCallStack => Int -> TestCC -> TestCC -> IO ()
reapproveGroup count superUser bob = do
  superUser <# "SimpleX-Directory> bob submitted the group ID 1:"
  superUser <##. "privacy ("
  superUser <## "Welcome message:"
  superUser <##. "Link to join the group privacy: "
  superUser <## (show count <> " members")
  superUser <## ""
  superUser <## "To approve send:"
  superUser <# "SimpleX-Directory> /approve 1:privacy 1"
  superUser #> "@SimpleX-Directory /approve 1:privacy 1"
  superUser <# "SimpleX-Directory> > /approve 1:privacy 1"
  superUser <## "      Group approved!"
  bob <# "SimpleX-Directory> The group ID 1 (privacy) is approved and listed in directory - please moderate it!"
  bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
  bob <## ""
  bob <## "Supported commands:"
  bob <## "- /filter 1 - to configure anti-spam filter."
  bob <## "- /role 1 - to set default member role."
  bob <## "- /help commands - other commands."

addCathAsOwner :: HasCallStack => TestCC -> TestCC -> IO ()
addCathAsOwner bob cath = do
  connectUsers bob cath
  fullAddMember "privacy" "Privacy" bob cath GROwner
  joinGroup "privacy" cath bob
  cath <## "#privacy: member SimpleX-Directory is connected"

withDirectoryService :: HasCallStack => TestParams -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryService ps = withDirectoryServiceCfg ps testCfg

withDirectoryServiceCfg :: HasCallStack => TestParams -> ChatConfig -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceCfg ps cfg = withDirectoryServiceCfgOwnersGroup ps cfg False

withDirectoryServiceCfgOwnersGroup :: HasCallStack => TestParams -> ChatConfig -> Bool -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceCfgOwnersGroup ps cfg createOwnersGroup test = do
  dsLink <-
    withNewTestChatCfg ps cfg serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChatCfg ps cfg "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        when createOwnersGroup $ do
          superUser ##> "/g owners"
          superUser <## "group #owners is created"
          superUser <## "to add members use /a owners <name> or /create link #owners"
          superUser ##> "/a owners SimpleX-Directory admin"
          superUser <## "invitation to join the group #owners sent to SimpleX-Directory"
          ds <## "#owners: alice invites you to join the group as admin"
          ds <## "use /j owners to accept"
          ds ##> "/j owners"
          ds <## "#owners: you joined the group"
          superUser <## "#owners: SimpleX-Directory joined the group"
        ds ##> "/ad"
        getContactLink ds True
  withDirectoryOwnersGroup ps cfg dsLink createOwnersGroup test

restoreDirectoryService :: HasCallStack => TestParams -> Int -> Int -> (TestCC -> String -> IO ()) -> IO ()
restoreDirectoryService ps ctCount grCount test = do
  dsLink <-
    withTestChat ps serviceDbPrefix $ \ds -> do
      ds <## (show ctCount <> " contacts connected (use /cs for the list)")
      ds <## "Your address is active! To show: /sa"
      ds <## (show grCount <> " group links active")
      forM_ [1 .. grCount] $ \_ -> ds <##. "#"
      ds ##> "/sa"
      dsLink <- getContactLink ds False
      ds <## "auto_accept on"
      pure dsLink
  withDirectory ps testCfg dsLink test

withDirectory :: HasCallStack => TestParams -> ChatConfig -> String -> (TestCC -> String -> IO ()) -> IO ()
withDirectory ps cfg dsLink = withDirectoryOwnersGroup ps cfg dsLink False

withDirectoryOwnersGroup :: HasCallStack => TestParams -> ChatConfig -> String -> Bool -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryOwnersGroup ps cfg dsLink createOwnersGroup test = do
  let opts = mkDirectoryOpts ps [KnownContact 2 "alice"] $ if createOwnersGroup then Just $ KnownGroup 1 "owners" else Nothing
  runDirectory cfg opts $
    withTestChatCfg ps cfg "super_user" $ \superUser -> do
      superUser <## "1 contacts connected (use /cs for the list)"
      when createOwnersGroup $
        superUser <## "#owners: connected to server(s)"
      test superUser dsLink

runDirectory :: ChatConfig -> DirectoryOpts -> IO () -> IO ()
runDirectory cfg opts@DirectoryOpts {directoryLog} action = do
  st <- restoreDirectoryStore directoryLog
  t <- forkIO $ bot st
  threadDelay 500000
  action `finally` (mapM_ hClose (directoryLogFile st) >> killThread t)
  where
    bot st = do
      env <- newServiceState opts
      let cfg' = cfg {chatHooks = defaultChatHooks {acceptMember = Just $ acceptMemberHook opts env}}
      simplexChatCore cfg' (mkChatOpts opts) $ directoryService st opts env

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
  u ##> ("/a " <> viewName n <> " SimpleX-Directory admin")
  u <## ("invitation to join the group #" <> viewName n <> " sent to SimpleX-Directory")

groupAccepted :: TestCC -> String -> IO String
groupAccepted u n = do
  u <# ("SimpleX-Directory> Joining the group " <> n <> "…")
  u <## ("#" <> viewName n <> ": SimpleX-Directory joined the group")
  u <# ("SimpleX-Directory> Joined the group " <> n <> ", creating the link…")
  u <# "SimpleX-Directory> Created the public link to join the group via this directory service that is always online."
  u <## ""
  u <## "Please add it to the group welcome message."
  u <## "For example, add:"
  dropStrPrefix "SimpleX-Directory> " . dropTime <$> getTermLine u -- welcome message with link

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
  u <## "description changed to:"
  u <## welcomeWithLink
  u <# ("SimpleX-Directory> Thank you! The group link for ID " <> show ugId <> " (" <> n <> ") is added to the welcome message.")
  u <## "You will be notified once the group is added to the directory - it may take up to 48 hours."

notifySuperUser :: TestCC -> TestCC -> String -> String -> String -> Int -> IO ()
notifySuperUser su u n fn welcomeWithLink gId = do
  uName <- userName u
  su <# ("SimpleX-Directory> " <> uName <> " submitted the group ID " <> show gId <> ":")
  su <## (n <> if null fn then "" else " (" <> fn <> ")")
  su <## "Welcome message:"
  su <## welcomeWithLink
  su .<## "members"
  su <## ""
  su <## "To approve send:"
  let approve = "/approve " <> show gId <> ":" <> viewName n <> " 1"
  su <# ("SimpleX-Directory> " <> approve)

approveRegistration :: TestCC -> TestCC -> String -> Int -> IO ()
approveRegistration su u n gId =
  approveRegistrationId su u n gId gId

approveRegistrationId :: TestCC -> TestCC -> String -> Int -> Int -> IO ()
approveRegistrationId su u n gId ugId = do
  let approve = "/approve " <> show gId <> ":" <> viewName n <> " 1"
  su #> ("@SimpleX-Directory " <> approve)
  su <# ("SimpleX-Directory> > " <> approve)
  su <## "      Group approved!"
  u <# ("SimpleX-Directory> The group ID " <> show ugId <> " (" <> n <> ") is approved and listed in directory - please moderate it!")
  u <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
  u <## ""
  u <## "Supported commands:"
  u <## ("- /filter " <> show ugId <> " - to configure anti-spam filter.")
  u <## ("- /role " <> show ugId <> " - to set default member role.")
  u <## "- /help commands - other commands."

connectVia :: TestCC -> String -> IO ()
u `connectVia` dsLink = do
  u ##> ("/c " <> dsLink)
  u <## "connection request sent!"
  u .<## ": contact is connected"
  u .<# "> Welcome to SimpleX-Directory service!"
  u <## "Send a search string to find groups or /help to learn how to add groups to directory."
  u <## ""
  u <## "For example, send privacy to find groups about privacy."
  u <## "Or send /all or /new to list groups."
  u <## ""
  u <## "Content and privacy policy: https://simplex.chat/docs/directory.html"

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
  u #> ("@SimpleX-Directory " <> name)
  groupFoundN' count u name

groupFoundN' :: Int -> TestCC -> String -> IO ()
groupFoundN' = groupFoundN_ "" Nothing

groupFoundN_ :: String -> Maybe Int -> Int -> TestCC -> String -> IO ()
groupFoundN_ suffix shownId_ count u name = do
  u <# ("SimpleX-Directory" <> suffix <> "> > " <> name)
  u <## "      Found 1 group(s)."
  u <#. ("SimpleX-Directory" <> suffix <> "> " <> maybe "" (\gId -> show gId <> ". ") shownId_ <> name)
  u <## "Welcome message:"
  u <##. "Link to join the group "
  u <## (show count <> " members")

groupNotFound :: TestCC -> String -> IO ()
groupNotFound = groupNotFound_ ""

groupNotFound_ :: String -> TestCC -> String -> IO ()
groupNotFound_ suffix u s = do
  u #> ("@SimpleX-Directory" <> suffix <> " " <> s)
  u <# ("SimpleX-Directory" <> suffix <> "> > " <> s)
  u <## "      No groups found"
