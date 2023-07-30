{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.DirectoryTests where

import ChatClient
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Directory.Options
import Directory.Service
import Directory.Store
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Core
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Types (Profile (..))
import Simplex.Messaging.Parsers (dropPrefix)
import System.FilePath ((</>))
import Test.Hspec

directoryServiceTests :: SpecWith FilePath
directoryServiceTests = do
  it "should register group" testDirectoryService

withDirectoryService :: DirectoryOpts -> IO () -> IO ()
withDirectoryService opts@DirectoryOpts {directoryLog} test = do
  st <- getDirectoryStore directoryLog
  bracket (forkIO $ bot st) killThread (\_ -> threadDelay 500000 >> test)
  where
    bot st = simplexChatCore testCfg (mkChatOpts opts) Nothing $ directoryService st opts

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
testDirectoryService tmp = do
  dsLink <-
    withNewTestChat tmp serviceDbPrefix directoryProfile $ \ds ->
      withNewTestChat tmp "super_user" aliceProfile $ \superUser -> do
        connectUsers ds superUser
        ds ##> "/ad"
        getContactLink ds True
  let opts = mkDirectoryOpts tmp [KnownContact 2 "alice"]
  withDirectoryService opts $
    withTestChat tmp "super_user" $ \superUser ->
      withNewTestChat tmp "bob" bobProfile $ \bob -> do
        withNewTestChat tmp "cath" cathProfile $ \cath -> do
          superUser <## "1 contacts connected (use /cs for the list)"
          bob `connectVia` dsLink
          bob #> "@SimpleX-Directory privacy"
          bob <# "SimpleX-Directory> > privacy"
          bob <## "      No groups found"
          bob ##> "/g PSA Privacy, Security & Anonymity"
          bob <## "group #PSA (Privacy, Security & Anonymity) is created"
          bob <## "to add members use /a PSA <name> or /create link #PSA"
          bob ##> "/a PSA SimpleX-Directory member"
          bob <## "invitation to join the group #PSA sent to SimpleX-Directory"
          bob <# "SimpleX-Directory> You must grant directory service admin role to register the group"
          bob ##> "/mr PSA SimpleX-Directory admin"
          bob <## "#PSA: you changed the role of SimpleX-Directory from member to admin"
          bob <# "SimpleX-Directory> Joining the group #PSA…"
          bob <## "#PSA: SimpleX-Directory joined the group"
          bob <# "SimpleX-Directory> Joined the group #PSA, creating the link…"
          bob <# "SimpleX-Directory> Created the public link to join the group via this directory service that is always online."
          bob <## ""
          bob <## "Please add it to the group welcome message."
          bob <## "For example, add:"
          welcomeWithLink <- dropPrefix "SimpleX-Directory> " . dropTime <$> getTermLine bob
          bob ##> ("/group_descr PSA " <> welcomeWithLink)
          bob <## "description changed to:"
          bob <## welcomeWithLink
          bob <# "SimpleX-Directory> Thank you! The group link for group ID 1 (PSA) added to the welcome message."
          bob <## "You will be notified once the group is added to the directory - it may take up to 24 hours."
          superUser <# "SimpleX-Directory> bob submitted the group ID 1: PSA (Privacy, Security & Anonymity)"
          superUser <## "Welcome message:"
          superUser <## welcomeWithLink
          superUser <## ""
          superUser <## "To approve send:"
          superUser <# "SimpleX-Directory> /approve 1:PSA 1"
          superUser #> "@SimpleX-Directory /approve 1:PSA 1"
          superUser <# "SimpleX-Directory> > /approve 1:PSA 1"
          superUser <## "      Group approved!"
          bob <# "SimpleX-Directory> The group ID 1 (PSA) is approved and listed in directory!"
          bob <## "Please note: if you change the group profile it will be hidden from directory until it is re-approved."
          bob `search` "privacy"
          bob <## welcomeWithLink
          cath `connectVia` dsLink
          cath `search` "privacy"
          cath <## welcomeWithLink
          -- bob <## ""
  where
    cc `connectVia` dsLink = do
      cc ##> ("/c " <> dsLink)
      cc <## "connection request sent!"
      cc <## "SimpleX-Directory: contact is connected"
      cc <# "SimpleX-Directory> Welcome to SimpleX-Directory service!"
      cc <## "Send a search string to find groups or /help to learn how to add groups to directory."
      cc <## ""
      cc <## "For example, send privacy to find groups about privacy."
    cc `search` s = do
      cc #> ("@SimpleX-Directory " <> s)
      cc <# ("SimpleX-Directory> > " <> s)
      cc <## "      Found 1 group(s)"
      cc <# "SimpleX-Directory> PSA (Privacy, Security & Anonymity)"
      cc <## "Welcome message:"

