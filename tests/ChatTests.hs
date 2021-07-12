{-# LANGUAGE OverloadedStrings #-}

module ChatTests where

import ChatClient
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Data.Char (isDigit)
import Data.List (dropWhileEnd, isPrefixOf)
import Simplex.Chat.Controller
import Simplex.Chat.Types (Profile (..))
import System.Terminal.Internal (VirtualTerminal (..))
import Test.Hspec

aliceProfile :: Profile
aliceProfile = Profile {contactRef = "alice", displayName = "Alice"}

bobProfile :: Profile
bobProfile = Profile {contactRef = "bob", displayName = "Bob"}

testAddContact :: Spec
testAddContact = describe "add chat contact" $
  it "add contact and send/receive message" $
    testChat2 aliceProfile bobProfile $ \alice bob -> do
      alice ##> "/a"
      Just inv <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv)
      concurrently_
        (bob <## "alice is connected")
        (alice <## "bob is connected")
      alice #> "@bob hello"
      bob <# "alice> hello"
      bob #> "@alice hi"
      alice <# "bob> hi"
      -- testing adding the same contact one more time - local name will be different
      alice ##> "/a"
      Just inv' <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv')
      concurrently_
        (bob <## "alice_1 is connected")
        (alice <## "bob_1 is connected")
      alice #> "@bob_1 hello"
      bob <# "alice_1> hello"
      bob #> "@alice_1 hi"
      alice <# "bob_1> hi"

(##>) :: TestCC -> String -> IO ()
(##>) cc cmd = do
  chatCommand cc cmd
  cc <## cmd

(#>) :: TestCC -> String -> IO ()
(#>) cc cmd = do
  chatCommand cc cmd
  cc <# cmd

chatCommand :: TestCC -> String -> IO ()
chatCommand (TestCC cc _ _) cmd = atomically $ writeTBQueue (inputQ cc) $ InputCommand cmd

(<##) :: TestCC -> String -> Expectation
cc <## line = (lastOutput <$> getWindow cc) `shouldReturn` line

(<#) :: TestCC -> String -> Expectation
cc <# line = (dropTime . lastOutput <$> getWindow cc) `shouldReturn` line

dropTime :: String -> String
dropTime msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then text else error "invalid time"
  _ -> error "invalid time"

getWindow :: TestCC -> IO [String]
getWindow (TestCC _ t _) = do
  let w = virtualWindow t
  win <- readTVarIO w
  atomically $ do
    win' <- readTVar w
    if win' /= win then pure win' else retry

invitation :: [String] -> Maybe String
invitation win = lastMaybe $ map (dropWhileEnd (== ' ')) $ filter ("smp::" `isPrefixOf`) win

lastOutput :: [String] -> String
lastOutput win = dropWhileEnd (== ' ') $ win !! (length win - 2) -- (- 2) to exclude prompt

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs
