{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTests where

import ChatClient
import Control.Concurrent.STM
import Data.List (dropWhileEnd, find, isPrefixOf)
import Simplex.Chat.Controller
import Simplex.Chat.Terminal
import Simplex.Chat.Types (Profile (..))
import System.Terminal.Internal (VirtualTerminal (..))
import Test.Hspec

aliceProfile :: Profile
aliceProfile = Profile {contactRef = "alice", displayName = "Alice"}

bobProfile :: Profile
bobProfile = Profile {contactRef = "bob", displayName = "Bob"}

testAddContact :: Spec
testAddContact = describe "add chat contact" $
  xit "add contact and send/receive message" $
    testChat2 aliceProfile bobProfile $ \alice bob -> do
      alice ##> "/a"
      Just inv <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv)
      bob <## "alice is connected"
      alice <## "bob is connected"
      alice #> "@bob hello"
      bob <# "alice> hello"
      bob #> "@alice hi"
      alice <# "bob> hi"

(##>) :: ChatController -> String -> IO ()
(##>) cc cmd = do
  chatCommand cc cmd
  cc <## cmd

(#>) :: ChatController -> String -> IO ()
(#>) cc cmd = do
  chatCommand cc cmd
  cc <# cmd

chatCommand :: ChatController -> String -> IO ()
chatCommand cc cmd = atomically $ writeTBQueue (inputQ cc) $ InputCommand cmd

(<##) :: ChatController -> String -> Expectation
cc <## line = (lastOutput <$> getWindow cc) `shouldReturn` line

(<#) :: ChatController -> String -> Expectation
cc <# line = (dropTime . lastOutput <$> getWindow cc) `shouldReturn` line

dropTime :: String -> String
dropTime = drop 6

getWindow :: ChatController -> IO [String]
getWindow cc = withVirtualChatTerm (chatTerminal cc) $ \case
  Just t -> do
    let w = virtualWindow t
    win <- readTVarIO w
    atomically $ do
      win' <- readTVar w
      if win' /= win then pure win' else retry
  Nothing -> error "expected virtual terminal"

invitation :: [String] -> Maybe String
invitation win = dropWhileEnd (== ' ') <$> find ("smp::" `isPrefixOf`) win

lastOutput :: [String] -> String
lastOutput win = dropWhileEnd (== ' ') $ win !! (length win - 2) -- (- 2) to exclude prompt
