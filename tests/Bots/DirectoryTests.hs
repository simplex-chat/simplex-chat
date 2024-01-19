{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Bots.DirectoryTests where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_)
import Directory.Events (viewName)
import Directory.Options
import Directory.Service
import Directory.Store
import GHC.IO.Handle (hClose)
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller (ChatConfig (..))
import Simplex.Chat.Core
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Types (GroupMemberRole (..), Profile (..))
import System.FilePath ((</>))
import Test.Hspec

directoryServiceTests :: SpecWith FilePath
directoryServiceTests = do
  it "should register group" testDirectoryService
  it "should suspend and resume group" testSuspendResume
  it "should join found group via link" testJoinGroup
  it "should support group names with spaces" testGroupNameWithSpaces
  it "should return more groups in search, all and recent groups" testSearchGroups
  describe "de-listing the group" $ do
    it "should de-list if owner leaves the group" testDelistedOwnerLeaves
    it "should de-list if owner is removed from the group" testDelistedOwnerRemoved
    it "should NOT de-list if another member leaves the group" testNotDelistedMemberLeaves
    it "should NOT de-list if another member is removed from the group" testNotDelistedMemberRemoved
    it "should de-list if service is removed from the group" testDelistedServiceRemoved
    it "should de-list/re-list when service/owner roles change" testDelistedRoleChanges
    it "should NOT de-list if another member role changes" testNotDelistedMemberRoleChanged
    it "should NOT send to approval if roles are incorrect" testNotSentApprovalBadRoles
    it "should NOT allow approving if roles are incorrect" testNotApprovedBadRoles
  describe "should require re-approval if profile is changed by" $ do
    it "the registration owner" testRegOwnerChangedProfile
    it "another owner" testAnotherOwnerChangedProfile
  describe "should require profile update if group link is removed by " $ do
    it "the registration owner" testRegOwnerRemovedLink
    it "another owner" testAnotherOwnerRemovedLink
  describe "duplicate groups (same display name and full name)" $ do
    it "should ask for confirmation if a duplicate group is submitted" testDuplicateAskConfirmation
    it "should prohibit registration if a duplicate group is listed" testDuplicateProhibitRegistration
    it "should prohibit confirmation if a duplicate group is listed" testDuplicateProhibitConfirmation
    it "should prohibit when profile is updated and not send for approval" testDuplicateProhibitWhenUpdated
    it "should prohibit approval if a duplicate group is listed" testDuplicateProhibitApproval
  describe "list groups" $ do
    it "should list user's groups" testListUserGroups
  describe "store log" $ do
    it "should restore directory service state" testRestoreDirectory

directoryProfile :: Profile
directoryProfile = Profile {displayName = "SimpleX-Directory", fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}

mkDirectoryOpts :: FilePath -> [KnownContact] -> DirectoryOpts
mkDirectoryOpts tmp superUsers =
  DirectoryOpts
    { coreOptions = testOpts.coreOptions {dbFilePrefix = tmp </> serviceDbPrefix},
      superUsers,
      directoryLog = Just $ tmp </> "directory_service.log",
      serviceName = "SimpleX-Directory",
      searchResults = 3,
      testing = True
    }

serviceDbPrefix :: FilePath
serviceDbPrefix = "directory_service"

testDirectoryService :: HasCallStack => FilePath -> IO ()
testDirectoryService tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
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
        bob <## "#PSA: you changed the role of SimpleX-Directory from member to admin"
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
        bob <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
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
        bob <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
        approvalRequested superUser welcomeWithLink' (1 :: Int)
        superUser #> "@SimpleX-Directory /approve 1:PSA 1"
        superUser <# "SimpleX-Directory> > /approve 1:PSA 1"
        superUser <## "      Group approved!"
        bob <# "SimpleX-Directory> The group ID 1 (PSA) is approved and listed in directory!"
        bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
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

testSuspendResume :: HasCallStack => FilePath -> IO ()
testSuspendResume tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
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

testJoinGroup :: HasCallStack => FilePath -> IO ()
testJoinGroup tmp =
  withDirectoryServiceCfg tmp testCfgGroupLinkViaContact $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgGroupLinkViaContact "bob" bobProfile $ \bob -> do
      withNewTestChatCfg tmp testCfgGroupLinkViaContact "cath" cathProfile $ \cath ->
        withNewTestChatCfg tmp testCfgGroupLinkViaContact "dan" danProfile $ \dan -> do
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
          cath <## "SimpleX-Directory_1: contact is connected"
          cath <## "contact SimpleX-Directory_1 is merged into SimpleX-Directory"
          cath <## "use @SimpleX-Directory <message> to send messages"
          cath <## "#privacy: you joined the group"
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
                bob <## "dan (Daniel): contact is connected"
                bob <## "dan invited to group #privacy via your group link"
                bob <## "#privacy: dan joined the group",
              do
                dan <## "bob (Bob): contact is connected"
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

testGroupNameWithSpaces :: HasCallStack => FilePath -> IO ()
testGroupNameWithSpaces tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
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

testSearchGroups :: HasCallStack => FilePath -> IO ()
testSearchGroups tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
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

testDelistedOwnerLeaves :: HasCallStack => FilePath -> IO ()
testDelistedOwnerLeaves tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        leaveGroup "privacy" bob
        cath <## "#privacy: bob left the group"
        bob <# "SimpleX-Directory> You left the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (group owner left)."
        groupNotFound cath "privacy"

testDelistedOwnerRemoved :: HasCallStack => FilePath -> IO ()
testDelistedOwnerRemoved tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        removeMember "privacy" cath bob
        bob <# "SimpleX-Directory> You are removed from the group ID 1 (privacy)."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (group owner is removed)."
        groupNotFound cath "privacy"

testNotDelistedMemberLeaves :: HasCallStack => FilePath -> IO ()
testNotDelistedMemberLeaves tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        leaveGroup "privacy" cath
        bob <## "#privacy: cath left the group"
        (superUser </)
        groupFound cath "privacy"

testNotDelistedMemberRemoved :: HasCallStack => FilePath -> IO ()
testNotDelistedMemberRemoved tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        removeMember "privacy" bob cath
        (superUser </)
        groupFound cath "privacy"

testDelistedServiceRemoved :: HasCallStack => FilePath -> IO ()
testDelistedServiceRemoved tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
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
        groupNotFound cath "privacy"

testDelistedRoleChanges :: HasCallStack => FilePath -> IO ()
testDelistedRoleChanges tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        groupFoundN 3 cath "privacy"
        -- de-listed if service role changed
        bob ##> "/mr privacy SimpleX-Directory member"
        bob <## "#privacy: you changed the role of SimpleX-Directory from admin to member"
        cath <## "#privacy: bob changed the role of SimpleX-Directory from admin to member"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to member."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (SimpleX-Directory role is changed to member)."
        groupNotFound cath "privacy"
        -- re-listed if service role changed back without profile changes
        cath ##> "/mr privacy SimpleX-Directory admin"
        cath <## "#privacy: you changed the role of SimpleX-Directory from member to admin"
        bob <## "#privacy: cath changed the role of SimpleX-Directory from member to admin"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is listed in the directory again."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is listed (SimpleX-Directory role is changed to admin)."
        groupFoundN 3 cath "privacy"
        -- de-listed if owner role changed
        cath ##> "/mr privacy bob admin"
        cath <## "#privacy: you changed the role of bob from owner to admin"
        bob <## "#privacy: cath changed your role from owner to admin"
        bob <# "SimpleX-Directory> Your role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (user role is set to admin)."
        groupNotFound cath "privacy"
        -- re-listed if owner role changed back without profile changes
        cath ##> "/mr privacy bob owner"
        cath <## "#privacy: you changed the role of bob from admin to owner"
        bob <## "#privacy: cath changed your role from admin to owner"
        bob <# "SimpleX-Directory> Your role in the group ID 1 (privacy) is changed to owner."
        bob <## ""
        bob <## "The group is listed in the directory again."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is listed (user role is set to owner)."
        groupFoundN 3 cath "privacy"

testNotDelistedMemberRoleChanged :: HasCallStack => FilePath -> IO ()
testNotDelistedMemberRoleChanged tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        groupFoundN 3 cath "privacy"
        bob ##> "/mr privacy cath member"
        bob <## "#privacy: you changed the role of cath from owner to member"
        cath <## "#privacy: bob changed your role from owner to member"
        groupFoundN 3 cath "privacy"

testNotSentApprovalBadRoles :: HasCallStack => FilePath -> IO ()
testNotSentApprovalBadRoles tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        welcomeWithLink <- groupAccepted bob "privacy"
        bob ##> "/mr privacy SimpleX-Directory member"
        bob <## "#privacy: you changed the role of SimpleX-Directory from admin to member"
        updateProfileWithLink bob "privacy" welcomeWithLink 1
        bob <# "SimpleX-Directory> You must grant directory service admin role to register the group"
        bob ##> "/mr privacy SimpleX-Directory admin"
        bob <## "#privacy: you changed the role of SimpleX-Directory from member to admin"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is submitted for approval."
        notifySuperUser superUser bob "privacy" "Privacy" welcomeWithLink 1
        groupNotFound cath "privacy"
        approveRegistration superUser bob "privacy" 1
        groupFound cath "privacy"

testNotApprovedBadRoles :: HasCallStack => FilePath -> IO ()
testNotApprovedBadRoles tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        submitGroup bob "privacy" "Privacy"
        welcomeWithLink <- groupAccepted bob "privacy"
        updateProfileWithLink bob "privacy" welcomeWithLink 1
        notifySuperUser superUser bob "privacy" "Privacy" welcomeWithLink 1
        bob ##> "/mr privacy SimpleX-Directory member"
        bob <## "#privacy: you changed the role of SimpleX-Directory from admin to member"
        let approve = "/approve 1:privacy 1"
        superUser #> ("@SimpleX-Directory " <> approve)
        superUser <# ("SimpleX-Directory> > " <> approve)
        superUser <## "      Group is not approved: user is not an owner."
        groupNotFound cath "privacy"
        bob ##> "/mr privacy SimpleX-Directory admin"
        bob <## "#privacy: you changed the role of SimpleX-Directory from member to admin"
        bob <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (privacy) is changed to admin."
        bob <## ""
        bob <## "The group is submitted for approval."
        notifySuperUser superUser bob "privacy" "Privacy" welcomeWithLink 1
        approveRegistration superUser bob "privacy" 1
        groupFound cath "privacy"

testRegOwnerChangedProfile :: HasCallStack => FilePath -> IO ()
testRegOwnerChangedProfile tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        bob ##> "/gp privacy privacy Privacy and Security"
        bob <## "full name changed to: Privacy and Security"
        bob <# "SimpleX-Directory> The group ID 1 (privacy) is updated!"
        bob <## "It is hidden from the directory until approved."
        cath <## "bob updated group #privacy:"
        cath <## "full name changed to: Privacy and Security"
        groupNotFound cath "privacy"
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is updated."
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testAnotherOwnerChangedProfile :: HasCallStack => FilePath -> IO ()
testAnotherOwnerChangedProfile tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        addCathAsOwner bob cath
        cath ##> "/gp privacy privacy Privacy and Security"
        cath <## "full name changed to: Privacy and Security"
        bob <## "cath updated group #privacy:"
        bob <## "full name changed to: Privacy and Security"
        bob <# "SimpleX-Directory> The group ID 1 (privacy) is updated!"
        bob <## "It is hidden from the directory until approved."
        groupNotFound cath "privacy"
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is updated."
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testRegOwnerRemovedLink :: HasCallStack => FilePath -> IO ()
testRegOwnerRemovedLink tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
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
        groupNotFound cath "privacy"
        bob ##> ("/set welcome #privacy " <> welcomeWithLink)
        bob <## "description changed to:"
        bob <## welcomeWithLink
        bob <# "SimpleX-Directory> Thank you! The group link for ID 1 (privacy) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
        cath <## "bob updated group #privacy:"
        cath <## "description changed to:"
        cath <## welcomeWithLink
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testAnotherOwnerRemovedLink :: HasCallStack => FilePath -> IO ()
testAnotherOwnerRemovedLink tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
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
        bob <# "SimpleX-Directory> The group link for ID 1 (privacy) is removed from the welcome message."
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
        bob <# "SimpleX-Directory> The group link is added by another group member, your registration will not be processed."
        bob <## ""
        bob <## "Please update the group profile yourself."
        bob ##> ("/set welcome #privacy " <> welcomeWithLink <> " - welcome!")
        bob <## "description changed to:"
        bob <## (welcomeWithLink <> " - welcome!")
        bob <# "SimpleX-Directory> Thank you! The group link for ID 1 (privacy) is added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
        cath <## "bob updated group #privacy:"
        cath <## "description changed to:"
        cath <## (welcomeWithLink <> " - welcome!")
        reapproveGroup 3 superUser bob
        groupFoundN 3 cath "privacy"

testDuplicateAskConfirmation :: HasCallStack => FilePath -> IO ()
testDuplicateAskConfirmation tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
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

testDuplicateProhibitRegistration :: HasCallStack => FilePath -> IO ()
testDuplicateProhibitRegistration tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        cath `connectVia` dsLink
        groupFound cath "privacy"
        _ <- submitGroup cath "privacy" "Privacy"
        cath <# "SimpleX-Directory> The group privacy (Privacy) is already listed in the directory, please choose another name."

testDuplicateProhibitConfirmation :: HasCallStack => FilePath -> IO ()
testDuplicateProhibitConfirmation tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
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

testDuplicateProhibitWhenUpdated :: HasCallStack => FilePath -> IO ()
testDuplicateProhibitWhenUpdated tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
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
        cath <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
        notifySuperUser superUser cath "security" "Security" welcomeWithLink' 2
        approveRegistration superUser cath "security" 2
        groupFound bob "security"
        groupFound cath "security"

testDuplicateProhibitApproval :: HasCallStack => FilePath -> IO ()
testDuplicateProhibitApproval tmp =
  withDirectoryService tmp $ \superUser dsLink ->
    withNewTestChat tmp "bob" bobProfile $ \bob ->
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
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

testListUserGroups :: HasCallStack => FilePath -> IO ()
testListUserGroups tmp =
  withDirectoryServiceCfg tmp testCfgCreateGroupDirect $ \superUser dsLink ->
    withNewTestChatCfg tmp testCfgCreateGroupDirect "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp testCfgCreateGroupDirect "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        cath `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        connectUsers bob cath
        fullAddMember "privacy" "Privacy" bob cath GRMember
        joinGroup "privacy" cath bob
        cath <## "#privacy: member SimpleX-Directory_1 is connected"
        cath <## "contact SimpleX-Directory_1 is merged into SimpleX-Directory"
        cath <## "use @SimpleX-Directory <message> to send messages"
        registerGroupId superUser bob "security" "Security" 2 2
        registerGroupId superUser cath "anonymity" "Anonymity" 3 1
        cath #> "@SimpleX-Directory /list"
        cath <# "SimpleX-Directory> > /list"
        cath <## "      1 registered group(s)"
        cath <# "SimpleX-Directory> 1. anonymity (Anonymity)"
        cath <## "Welcome message:"
        cath <##. "Link to join the group anonymity: "
        cath <## "2 members"
        cath <## "Status: active"
        -- with de-listed group
        groupFound cath "anonymity"
        cath ##> "/mr anonymity SimpleX-Directory member"
        cath <## "#anonymity: you changed the role of SimpleX-Directory from admin to member"
        cath <# "SimpleX-Directory> SimpleX-Directory role in the group ID 1 (anonymity) is changed to member."
        cath <## ""
        cath <## "The group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 3 (anonymity) is de-listed (SimpleX-Directory role is changed to member)."
        groupNotFound cath "anonymity"
        listGroups superUser bob cath

testRestoreDirectory :: HasCallStack => FilePath -> IO ()
testRestoreDirectory tmp = do
  testListUserGroups tmp
  restoreDirectoryService tmp 3 3 $ \superUser _dsLink ->
    withTestChat tmp "bob" $ \bob ->
      withTestChat tmp "cath" $ \cath -> do
        bob <## "2 contacts connected (use /cs for the list)"
        bob
          <### [ "#privacy (Privacy): connected to server(s)",
                 "#security (Security): connected to server(s)"
               ]
        cath <## "2 contacts connected (use /cs for the list)"
        cath
          <### [ "#privacy (Privacy): connected to server(s)",
                 "#anonymity (Anonymity): connected to server(s)"
               ]
        listGroups superUser bob cath
        groupFoundN 3 bob "privacy"
        groupFound bob "security"
        groupFoundN 3 cath "privacy"
        groupFound cath "security"

listGroups :: HasCallStack => TestCC -> TestCC -> TestCC -> IO ()
listGroups superUser bob cath = do
  bob #> "@SimpleX-Directory /list"
  bob <# "SimpleX-Directory> > /list"
  bob <## "      2 registered group(s)"
  bob <# "SimpleX-Directory> 1. privacy (Privacy)"
  bob <## "Welcome message:"
  bob <##. "Link to join the group privacy: "
  bob <## "3 members"
  bob <## "Status: active"
  bob <# "SimpleX-Directory> 2. security (Security)"
  bob <## "Welcome message:"
  bob <##. "Link to join the group security: "
  bob <## "2 members"
  bob <## "Status: active"
  cath #> "@SimpleX-Directory /list"
  cath <# "SimpleX-Directory> > /list"
  cath <## "      1 registered group(s)"
  cath <# "SimpleX-Directory> 1. anonymity (Anonymity)"
  cath <## "Welcome message:"
  cath <##. "Link to join the group anonymity: "
  cath <## "2 members"
  cath <## "Status: suspended because roles changed"
  -- superuser lists all groups
  bob #> "@SimpleX-Directory /last"
  bob <# "SimpleX-Directory> > /last"
  bob <## "      You are not allowed to use this command"
  superUser #> "@SimpleX-Directory /last"
  superUser <# "SimpleX-Directory> > /last"
  superUser <## "      3 registered group(s)"
  superUser <# "SimpleX-Directory> 1. privacy (Privacy)"
  superUser <## "Welcome message:"
  superUser <##. "Link to join the group privacy: "
  superUser <## "Owner: bob"
  superUser <## "3 members"
  superUser <## "Status: active"
  superUser <# "SimpleX-Directory> 2. security (Security)"
  superUser <## "Welcome message:"
  superUser <##. "Link to join the group security: "
  superUser <## "Owner: bob"
  superUser <## "2 members"
  superUser <## "Status: active"
  superUser <# "SimpleX-Directory> 3. anonymity (Anonymity)"
  superUser <## "Welcome message:"
  superUser <##. "Link to join the group anonymity: "
  superUser <## "Owner: cath"
  superUser <## "2 members"
  superUser <## "Status: suspended because roles changed"
  -- showing last 1 group
  superUser #> "@SimpleX-Directory /last 1"
  superUser <# "SimpleX-Directory> > /last 1"
  superUser <## "      3 registered group(s), showing the last 1"
  superUser <# "SimpleX-Directory> 3. anonymity (Anonymity)"
  superUser <## "Welcome message:"
  superUser <##. "Link to join the group anonymity: "
  superUser <## "Owner: cath"
  superUser <## "2 members"
  superUser <## "Status: suspended because roles changed"

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
  bob <# "SimpleX-Directory> The group ID 1 (privacy) is approved and listed in directory!"
  bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."

addCathAsOwner :: HasCallStack => TestCC -> TestCC -> IO ()
addCathAsOwner bob cath = do
  connectUsers bob cath
  fullAddMember "privacy" "Privacy" bob cath GROwner
  joinGroup "privacy" cath bob
  cath <## "#privacy: member SimpleX-Directory is connected"

withDirectoryService :: HasCallStack => FilePath -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryService tmp = withDirectoryServiceCfg tmp testCfg

withDirectoryServiceCfg :: HasCallStack => FilePath -> ChatConfig -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryServiceCfg tmp cfg test = do
  dsLink <-
    withNewTestChatCfg tmp cfg serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChatCfg tmp cfg "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        ds ##> "/ad"
        getContactLink ds True
  withDirectory tmp cfg dsLink test

restoreDirectoryService :: HasCallStack => FilePath -> Int -> Int -> (TestCC -> String -> IO ()) -> IO ()
restoreDirectoryService tmp ctCount grCount test = do
  dsLink <-
    withTestChat tmp serviceDbPrefix $ \ds -> do
      ds <## (show ctCount <> " contacts connected (use /cs for the list)")
      ds <## "Your address is active! To show: /sa"
      ds <## (show grCount <> " group links active")
      forM_ [1 .. grCount] $ \_ -> ds <##. "#"
      ds ##> "/sa"
      dsLink <- getContactLink ds False
      ds <## "auto_accept on"
      pure dsLink
  withDirectory tmp testCfg dsLink test

withDirectory :: HasCallStack => FilePath -> ChatConfig -> String -> (TestCC -> String -> IO ()) -> IO ()
withDirectory tmp cfg dsLink test = do
  let opts = mkDirectoryOpts tmp [KnownContact 2 "alice"]
  runDirectory cfg opts $
    withTestChatCfg tmp cfg "super_user" $ \superUser -> do
      superUser <## "1 contacts connected (use /cs for the list)"
      test superUser dsLink

runDirectory :: ChatConfig -> DirectoryOpts -> IO () -> IO ()
runDirectory cfg opts@DirectoryOpts {directoryLog} action = do
  st <- restoreDirectoryStore directoryLog
  t <- forkIO $ bot st
  threadDelay 500000
  action `finally` (mapM_ hClose (directoryLogFile st) >> killThread t)
  where
    bot st = simplexChatCore cfg (mkChatOpts opts) $ directoryService st opts

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
  u <## "You will be notified once the group is added to the directory - it may take up to 24 hours."

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
  u <# ("SimpleX-Directory> The group ID " <> show ugId <> " (" <> n <> ") is approved and listed in directory!")
  u <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."

connectVia :: TestCC -> String -> IO ()
u `connectVia` dsLink = do
  u ##> ("/c " <> dsLink)
  u <## "connection request sent!"
  u <## "SimpleX-Directory: contact is connected"
  u <# "SimpleX-Directory> Welcome to SimpleX-Directory service!"
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
  u <# ("SimpleX-Directory> > " <> name)
  u <## "      Found 1 group(s)."
  u <#. ("SimpleX-Directory> " <> name)
  u <## "Welcome message:"
  u <##. "Link to join the group "
  u <## (show count <> " members")

groupNotFound :: TestCC -> String -> IO ()
groupNotFound u s = do
  u #> ("@SimpleX-Directory " <> s)
  u <# ("SimpleX-Directory> > " <> s)
  u <## "      No groups found"
