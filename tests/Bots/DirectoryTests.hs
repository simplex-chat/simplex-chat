{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Bots.DirectoryTests where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Directory.Options
import Directory.Service
import Directory.Store
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Core
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Types (Profile (..))
import System.FilePath ((</>))
import Test.Hspec

directoryServiceTests :: SpecWith FilePath
directoryServiceTests = do
  fit "should register group" testDirectoryService
  fit "should de-list group if owner leaves the group" testDelistedOwnerLeaves
  fit "should de-list group if owner is removed from the group" testDelistedOwnerRemoved
  fit "should de-list group if service is removed from the group" testDelistedServiceRemoved

directoryProfile :: Profile
directoryProfile = Profile {displayName = "SimpleX-Directory", fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}

mkDirectoryOpts :: FilePath -> [KnownContact] -> DirectoryOpts
mkDirectoryOpts tmp superUsers =
  DirectoryOpts
    { coreOptions = (coreOptions (testOpts :: ChatOpts)) {dbFilePrefix = tmp </> serviceDbPrefix},
      superUsers,
      directoryLog = tmp </> "directory_service.log",
      serviceName = "SimpleX-Directory"
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
        bob <# "SimpleX-Directory> Joining the group #PSA…"
        bob <## "#PSA: SimpleX-Directory joined the group"
        bob <# "SimpleX-Directory> Joined the group #PSA, creating the link…"
        bob <# "SimpleX-Directory> Created the public link to join the group via this directory service that is always online."
        bob <## ""
        bob <## "Please add it to the group welcome message."
        bob <## "For example, add:"
        welcomeWithLink <- dropStrPrefix "SimpleX-Directory> " . dropTime <$> getTermLine bob
        -- putStrLn "*** update profile without link"
        updateGroupProfile bob "Welcome!"
        bob <# "SimpleX-Directory> Profile updated for ID 1 (PSA), but the group link is not added to the welcome message."
        (superUser </)
        -- putStrLn "*** update profile so that it has link"
        updateGroupProfile bob welcomeWithLink
        bob <# "SimpleX-Directory> Thank you! The group link for group ID 1 (PSA) added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
        approvalRequested superUser welcomeWithLink (1 :: Int)
        -- putStrLn "*** update profile so that it still has link"
        let welcomeWithLink' = "Welcome! " <> welcomeWithLink
        updateGroupProfile bob welcomeWithLink'
        superUser <# "SimpleX-Directory> The group ID 1 (PSA) is updated."
        approvalRequested superUser welcomeWithLink' (2 :: Int)
        -- putStrLn "*** try approving with the old registration code"
        superUser #> "@SimpleX-Directory /approve 1:PSA 1"
        superUser <# "SimpleX-Directory> > /approve 1:PSA 1"
        superUser <## "      Incorrect approval code"
        -- putStrLn "*** update profile so that it has no link"
        updateGroupProfile bob "Welcome!"
        bob <# "SimpleX-Directory> The link for group ID 1 (PSA) is removed from the welcome message, please add it."
        superUser <# "SimpleX-Directory> The link is removed from the group ID 1 (PSA)."
        superUser #> "@SimpleX-Directory /approve 1:PSA 2"
        superUser <# "SimpleX-Directory> > /approve 1:PSA 2"
        superUser <## "      Error: the group ID 1 (PSA) is not pending approval."
        -- putStrLn "*** update profile so that it has link again"
        updateGroupProfile bob welcomeWithLink'
        bob <# "SimpleX-Directory> Thank you! The group link for group ID 1 (PSA) added to the welcome message."
        bob <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
        -- superUser <# "SimpleX-Directory> The group ID 1 (PSA) is updated."
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
  where
    search u s welcome = do
      u #> ("@SimpleX-Directory " <> s)
      u <# ("SimpleX-Directory> > " <> s)
      u <## "      Found 1 group(s)"
      u <# "SimpleX-Directory> PSA (Privacy, Security & Anonymity)"
      u <## "Welcome message:"
      u <## welcome
    updateGroupProfile u welcome = do
      u ##> ("/group_descr PSA " <> welcome)
      u <## "description changed to:"
      u <## welcome
    approvalRequested su welcome grId = do
      su <# "SimpleX-Directory> bob submitted the group ID 1: PSA (Privacy, Security & Anonymity)"
      su <## "Welcome message:"
      su <## welcome
      su <## ""
      su <## "To approve send:"
      su <# ("SimpleX-Directory> /approve 1:PSA " <> show grId)

testDelistedOwnerLeaves :: HasCallStack => FilePath -> IO ()
testDelistedOwnerLeaves tmp =
  withDirectoryService tmp $ \superUser dsLink -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      bob ##> "/l privacy"
      bob <## "#privacy: you left the group"
      bob <## "use /d #privacy to delete the group"
      bob <# "SimpleX-Directory> You left the group ID 1 (privacy)."
      bob <## ""
      bob <## "Group is no longer listed in the directory."
      superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (group owner left)."

testDelistedOwnerRemoved :: HasCallStack => FilePath -> IO ()
testDelistedOwnerRemoved tmp =
  withDirectoryService tmp $ \superUser dsLink -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      withNewTestChat tmp "cath" cathProfile $ \cath -> do
        bob `connectVia` dsLink
        registerGroup superUser bob "privacy" "Privacy"
        connectUsers bob cath
        bob ##> "/a privacy cath owner"
        bob <## "invitation to join the group #privacy sent to cath"
        cath <## "#privacy (Privacy): bob invites you to join the group as owner"
        cath <## "use /j privacy to accept"
        cath ##> "/j privacy"
        cath <## "#privacy: you joined the group"
        cath <#. "#privacy bob> Link to join the group privacy: "
        cath <## "#privacy: member SimpleX-Directory is connected"
        bob <## "#privacy: cath joined the group"
        cath ##> "/rm privacy bob"
        cath <## "#privacy: you removed bob from the group"
        bob <## "#privacy: cath removed you from the group"
        bob <## "use /d #privacy to delete the group"
        bob <# "SimpleX-Directory> You are removed from the group ID 1 (privacy)."
        bob <## ""
        bob <## "Group is no longer listed in the directory."
        superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (group owner is removed)."

testDelistedServiceRemoved :: HasCallStack => FilePath -> IO ()
testDelistedServiceRemoved tmp =
  withDirectoryService tmp $ \superUser dsLink -> do
    withNewTestChat tmp "bob" bobProfile $ \bob -> do
      bob `connectVia` dsLink
      registerGroup superUser bob "privacy" "Privacy"
      bob ##> "/rm #privacy SimpleX-Directory"
      bob <## "#privacy: you removed SimpleX-Directory from the group"
      bob <# "SimpleX-Directory> SimpleX-Directory is removed from the group ID 1 (privacy)."
      bob <## ""
      bob <## "Group is no longer listed in the directory."
      superUser <# "SimpleX-Directory> The group ID 1 (privacy) is de-listed (directory service is removed)."

withDirectoryService :: HasCallStack => FilePath -> (TestCC -> String -> IO ()) -> IO ()
withDirectoryService tmp test = do
  dsLink <-
    withNewTestChat tmp serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChat tmp "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        ds ##> "/ad"
        getContactLink ds True
  let opts = mkDirectoryOpts tmp [KnownContact 2 "alice"]
  withDirectory opts $
    withTestChat tmp "super_user" $ \superUser -> do
      superUser <## "1 contacts connected (use /cs for the list)"
      test superUser dsLink
  where
    withDirectory :: DirectoryOpts -> IO () -> IO ()
    withDirectory opts@DirectoryOpts {directoryLog} action = do
      st <- getDirectoryStore directoryLog
      t <- forkIO $ bot st
      threadDelay 500000
      action `finally` killThread t
      where
        bot st = simplexChatCore testCfg (mkChatOpts opts) Nothing $ directoryService st opts

registerGroup :: TestCC -> TestCC -> String -> String -> IO ()
registerGroup su u n fn = do
  u ##> ("/g " <> n <> " " <> fn)
  u <## ("group #" <> n <> " (" <> fn <> ") is created")
  u <## ("to add members use /a " <> n <> " <name> or /create link #" <> n)
  u ##> ("/a " <> n <> " SimpleX-Directory admin")
  u <## ("invitation to join the group #" <> n <> " sent to SimpleX-Directory")
  u <# ("SimpleX-Directory> Joining the group #" <> n <> "…")
  u <## ("#" <> n <> ": SimpleX-Directory joined the group")
  u <# ("SimpleX-Directory> Joined the group #" <> n <> ", creating the link…")
  u <# "SimpleX-Directory> Created the public link to join the group via this directory service that is always online."
  u <## ""
  u <## "Please add it to the group welcome message."
  u <## "For example, add:"
  welcomeWithLink <- dropStrPrefix "SimpleX-Directory> " . dropTime <$> getTermLine u
  u ##> ("/group_descr " <> n <> " " <> welcomeWithLink)
  u <## "description changed to:"
  u <## welcomeWithLink
  u <# ("SimpleX-Directory> Thank you! The group link for group ID 1 (" <> n <> ") added to the welcome message.")
  u <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
  su <# ("SimpleX-Directory> bob submitted the group ID 1: " <> n <> " (" <> fn <> ")")
  su <## "Welcome message:"
  su <## welcomeWithLink
  su <## ""
  su <## "To approve send:"
  let approve = "/approve 1:" <> n <> " 1"
  su <# ("SimpleX-Directory> " <> approve)
  su #> ("@SimpleX-Directory " <> approve)
  su <# ("SimpleX-Directory> > " <> approve)
  su <## "      Group approved!"
  u <# ("SimpleX-Directory> The group ID 1 (" <> n <> ") is approved and listed in directory!")
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
