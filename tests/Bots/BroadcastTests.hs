{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BroadcastTests where

import Broadcast.Bot
import Broadcast.Options
import ChatClient
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Core
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Types (Profile (..))
import System.FilePath ((</>))
import Test.Hspec hiding (it)

broadcastBotTests :: SpecWith FilePath
broadcastBotTests = do
  it "should broadcast message" testBroadcastMessages

withBroadcastBot :: BroadcastBotOpts -> IO () -> IO ()
withBroadcastBot opts test =
  bracket (forkIO bot) killThread (\_ -> threadDelay 500000 >> test)
  where
    bot = simplexChatCore testCfg (mkChatOpts opts) $ broadcastBot opts

broadcastBotProfile :: Profile
broadcastBotProfile = Profile {displayName = "broadcast_bot", fullName = "Broadcast Bot", image = Nothing, contactLink = Nothing, preferences = Nothing}

mkBotOpts :: FilePath -> [KnownContact] -> BroadcastBotOpts
mkBotOpts tmp publishers =
  BroadcastBotOpts
    { coreOptions = testCoreOpts {dbFilePrefix = tmp </> botDbPrefix},
      publishers,
      welcomeMessage = defaultWelcomeMessage publishers,
      prohibitedMessage = defaultWelcomeMessage publishers
    }

botDbPrefix :: FilePath
botDbPrefix = "broadcast_bot"

testBroadcastMessages :: HasCallStack => FilePath -> IO ()
testBroadcastMessages tmp = do
  botLink <-
    withNewTestChat tmp botDbPrefix broadcastBotProfile $ \bc_bot ->
      withNewTestChat tmp "alice" aliceProfile $ \alice -> do
        connectUsers bc_bot alice
        bc_bot ##> "/ad"
        getContactLink bc_bot True
  let botOpts = mkBotOpts tmp [KnownContact 2 "alice"]
  withBroadcastBot botOpts $
    withTestChat tmp "alice" $ \alice ->
      withNewTestChat tmp "bob" bobProfile $ \bob ->
        withNewTestChat tmp "cath" cathProfile $ \cath -> do
          alice <## "1 contacts connected (use /cs for the list)"
          bob `connectVia` botLink
          bob #> "@broadcast_bot hello"
          bob <# "broadcast_bot> > hello"
          bob <## "      Hello! I am a broadcast bot."
          bob <## "I broadcast messages to all connected users from @alice."
          cath `connectVia` botLink
          alice #> "@broadcast_bot hello all!"
          bob <# "broadcast_bot> hello all!"
          cath <# "broadcast_bot> hello all!"
          alice <# "broadcast_bot> > hello all!"
          alice <## "      Forwarded to 2 contact(s)"
  where
    cc `connectVia` botLink = do
      cc ##> ("/c " <> botLink)
      cc <## "connection request sent!"
      cc <## "broadcast_bot (Broadcast Bot): contact is connected"
      cc <# "broadcast_bot> Hello! I am a broadcast bot."
      cc <## "I broadcast messages to all connected users from @alice."
