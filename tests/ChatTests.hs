{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTests where

import ChatClient
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Data.Char (isDigit)
import Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Types (Profile (..), User (..))
import System.Terminal.Internal (VirtualTerminal (..))
import Test.Hspec

aliceProfile :: Profile
aliceProfile = Profile {displayName = "alice", fullName = "Alice"}

bobProfile :: Profile
bobProfile = Profile {displayName = "bob", fullName = "Bob"}

cathProfile :: Profile
cathProfile = Profile {displayName = "cath", fullName = "Catherine"}

danProfile :: Profile
danProfile = Profile {displayName = "dan", fullName = "Daniel"}

chatTests :: Spec
chatTests = do
  describe "direct messages" $
    it "add contact and send/receive message" testAddContact
  describe "chat groups" $ do
    it "add contacts, create group and send/receive messages" testGroup
    it "create and join group with 4 members" testGroup2

testAddContact :: IO ()
testAddContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/a"
      Just inv <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv)
      concurrently_
        (bob <## "alice (Alice) is connected")
        (alice <## "bob (Bob) is connected")
      alice #> "@bob hello"
      bob <# "alice> hello"
      bob #> "@alice hi"
      alice <# "bob> hi"
      -- test adding the same contact one more time - local name will be different
      alice ##> "/a"
      Just inv' <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv')
      concurrently_
        (bob <## "alice_1 (Alice) is connected")
        (alice <## "bob_1 (Bob) is connected")
      alice #> "@bob_1 hello"
      bob <# "alice_1> hello"
      bob #> "@alice_1 hi"
      alice <# "bob_1> hi"
      -- test deleting contact
      alice ##> "/d bob_1"
      alice <## "bob_1 is deleted"
      alice #:> "@bob_1 hey"
      alice <## "no contact bob_1"

testGroup :: IO ()
testGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      alice #:> "/g team"
      -- TODO this occasionally fails in case getWindow is run before the command above is printed
      alice <## "use /a team <name> to add members"
      alice ##> "/a team bob"
      alice <## "invitation to join the group #team sent to bob"
      bob <## "use /j team to accept"
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      alice ##> "/a team cath"
      alice <## "invitation to join the group #team sent to cath"
      cath <## "use /j team to accept"
      cath ##> "/j team"
      concurrentlyN_
        [ alice <## "#team: cath joined the group",
          do
            cath <## "#team: you joined the group"
            cath <## "#team: member bob (Bob) is connected",
          do
            bob <## "#team: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#team: new member cath is connected"
        ]
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath <# "#team alice> hello")
      bob #> "#team hi there"
      concurrently_
        (alice <# "#team bob> hi there")
        (cath <# "#team bob> hi there")
      cath #> "#team hey"
      concurrently_
        (alice <# "#team cath> hey")
        (bob <# "#team cath> hey")
      bob #> "@cath hello cath"
      cath <# "bob> hello cath"
      cath #> "@bob hello bob"
      bob <# "cath> hello bob"

testGroup2 :: IO ()
testGroup2 =
  testChat4 aliceProfile bobProfile cathProfile danProfile $
    \alice bob cath dan -> do
      connectUsers alice bob
      connectUsers alice cath
      connectUsers bob dan
      connectUsers alice dan
      alice #:> "/g club"
      -- TODO this occasionally fails in case getWindow is run before the command above is printed
      alice <## "use /a club <name> to add members"
      alice ##> "/a club bob"
      alice <## "invitation to join the group #club sent to bob"
      bob <## "use /j club to accept"
      alice ##> "/a club cath"
      alice <## "invitation to join the group #club sent to cath"
      cath <## "use /j club to accept"
      bob ##> "/j club"
      concurrently_
        (alice <## "#club: bob joined the group")
        (bob <## "#club: you joined the group")
      cath ##> "/j club"
      concurrentlyN_
        [ alice <## "#club: cath joined the group",
          do
            cath <## "#club: you joined the group"
            cath <## "#club: member bob (Bob) is connected",
          do
            bob <## "#club: alice added cath (Catherine) to the group (connecting...)"
            bob <## "#club: new member cath is connected"
        ]
      bob ##> "/a club dan"
      bob <## "invitation to join the group #club sent to dan"
      dan <## "use /j club to accept"
      dan ##> "/j club"
      concurrentlyN_
        [ bob <## "#club: dan joined the group",
          do
            dan <## "#club: you joined the group"
            dan <### ["#club: member alice_1 (Alice) is connected", "#club: member cath (Catherine) is connected"],
          do
            alice <## "#club: bob added dan_1 (Daniel) to the group (connecting...)"
            alice <## "#club: new member dan_1 is connected",
          do
            cath <## "#club: bob added dan (Daniel) to the group (connecting...)"
            cath <## "#club: new member dan is connected"
        ]
      alice #> "#club hello"
      concurrentlyN_
        [ bob <# "#club alice> hello",
          cath <# "#club alice> hello",
          dan <# "#club alice_1> hello"
        ]
      bob #> "#club hi there"
      concurrentlyN_
        [ alice <# "#club bob> hi there",
          cath <# "#club bob> hi there",
          dan <# "#club bob> hi there"
        ]
      cath #> "#club hey"
      concurrentlyN_
        [ alice <# "#club cath> hey",
          bob <# "#club cath> hey",
          dan <# "#club cath> hey"
        ]
      dan #> "#club how is it going?"
      concurrentlyN_
        [ alice <# "#club dan_1> how is it going?",
          bob <# "#club dan> how is it going?",
          cath <# "#club dan> how is it going?"
        ]
      bob #> "@cath hi cath"
      cath <# "bob> hi cath"
      cath #> "@bob hi bob"
      bob <# "cath> hi bob"
      dan #> "@cath hey cath"
      cath <# "dan> hey cath"
      cath #> "@dan hey dan"
      dan <# "cath> hey dan"
      dan #> "@alice_1 hi alice"
      alice <# "dan_1> hi alice"
      alice #> "@dan_1 hello dan"
      dan <# "alice_1> hello dan"

connectUsers :: TestCC -> TestCC -> IO ()
connectUsers cc1 cc2 = do
  cc1 ##> "/a"
  Just inv <- invitation <$> getWindow cc1
  cc2 ##> ("/c " <> inv)
  concurrently_
    (cc2 <## (showName cc1 <> " is connected"))
    (cc1 <## (showName cc2 <> " is connected"))
  where
    showName :: TestCC -> String
    showName (TestCC ChatController {currentUser = User {localDisplayName, profile = Profile {fullName}}} _ _) =
      T.unpack $ localDisplayName <> " (" <> fullName <> ")"

(##>) :: TestCC -> String -> IO ()
(##>) cc cmd = do
  cc #:> cmd
  cc <## cmd

(#>) :: TestCC -> String -> IO ()
(#>) cc cmd = do
  cc #:> cmd
  cc <# cmd

(#:>) :: TestCC -> String -> IO ()
(#:>) (TestCC cc _ _) cmd = atomically $ writeTBQueue (inputQ cc) $ InputCommand cmd

(<##) :: TestCC -> String -> Expectation
cc <## line = (lastOutput <$> getWindow cc) `shouldReturn` line

(<###) :: TestCC -> [String] -> Expectation
_ <### [] = pure ()
cc <### ls = do
  line <- lastOutput <$> getWindow cc
  if line `elem` ls
    then cc <### filter (/= line) ls
    else error $ "unexpected output: " <> line

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
