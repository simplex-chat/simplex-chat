{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests where

import ChatClient
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Data.Char (isDigit)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Types (Profile (..), User (..))
import System.Terminal.Internal (VirtualTerminal (..))
import System.Timeout (timeout)
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
    it "create and delete group" testGroupDelete
    it "remove contact from group and add again" testGroupRemoveAdd

testAddContact :: IO ()
testAddContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/c"
      Just inv <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv)
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice #> "@bob hello"
      bob <# "alice> hello"
      bob #> "@alice hi"
      alice <# "bob> hi"
      -- test adding the same contact one more time - local name will be different
      alice ##> "/c"
      Just inv' <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv')
      concurrently_
        (bob <## "alice_1 (Alice): contact is connected")
        (alice <## "bob_1 (Bob): contact is connected")
      alice #> "@bob_1 hello"
      bob <# "alice_1> hello"
      bob #> "@alice_1 hi"
      alice <# "bob_1> hi"
      -- test deleting contact
      alice ##> "/d bob_1"
      alice <## "bob_1: contact is deleted"
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
      alice <## "group #team is created\nuse /a team <name> to add members"
      alice ##> "/a team bob"
      concurrently_
        (alice <## "invitation to join the group #team sent to bob")
        (bob <## "#team: alice invites you to join the group as admin\nuse /j team to accept")
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      alice ##> "/a team cath"
      concurrently_
        (alice <## "invitation to join the group #team sent to cath")
        (cath <## "#team: alice invites you to join the group as admin\nuse /j team to accept")
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
      bob <##> cath
      -- remove member
      bob ##> "/rm team cath"
      concurrentlyN_
        [ bob <## "#team: you removed cath from the group",
          alice <## "#team: bob removed cath from the group",
          cath <## "#team: bob removed you from the group\nuse /d #team to delete the group"
        ]
      bob #> "#team hi"
      concurrently_
        (alice <# "#team bob> hi")
        (cath </)
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath </)
      cath #:> "#team hello"
      cath <## "you are no longer the member of the group"
      bob <##> cath

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
      alice <## "group #club is created\nuse /a club <name> to add members"
      alice ##> "/a club bob"
      concurrently_
        (alice <## "invitation to join the group #club sent to bob")
        (bob <## "#club: alice invites you to join the group as admin\nuse /j club to accept")
      alice ##> "/a club cath"
      concurrently_
        (alice <## "invitation to join the group #club sent to cath")
        (cath <## "#club: alice invites you to join the group as admin\nuse /j club to accept")
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
      concurrently_
        (bob <## "invitation to join the group #club sent to dan")
        (dan <## "#club: bob invites you to join the group as admin\nuse /j club to accept")
      dan ##> "/j club"
      concurrentlyN_
        [ bob <## "#club: dan joined the group",
          do
            dan <## "#club: you joined the group"
            dan <### ["#club: member alice_1 (Alice) is connected", "#club: member cath (Catherine) is connected"]
            dan <## "contact alice_1 is merged into alice\nuse @alice <message> to send messages",
          do
            alice <## "#club: bob added dan_1 (Daniel) to the group (connecting...)"
            alice <## "#club: new member dan_1 is connected"
            alice <## "contact dan_1 is merged into dan\nuse @dan <message> to send messages",
          do
            cath <## "#club: bob added dan (Daniel) to the group (connecting...)"
            cath <## "#club: new member dan is connected"
        ]
      alice #> "#club hello"
      concurrentlyN_
        [ bob <# "#club alice> hello",
          cath <# "#club alice> hello",
          dan <# "#club alice> hello"
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
        [ alice <# "#club dan> how is it going?",
          bob <# "#club dan> how is it going?",
          cath <# "#club dan> how is it going?"
        ]
      bob <##> cath
      dan <##> cath
      dan <##> alice
      -- remove member
      cath ##> "/rm club dan"
      concurrentlyN_
        [ cath <## "#club: you removed dan from the group",
          alice <## "#club: cath removed dan from the group",
          bob <## "#club: cath removed dan from the group",
          dan <## "#club: cath removed you from the group\nuse /d #club to delete the group"
        ]
      alice #> "#club hello"
      concurrentlyN_
        [ bob <# "#club alice> hello",
          cath <# "#club alice> hello",
          (dan </)
        ]
      bob #> "#club hi there"
      concurrentlyN_
        [ alice <# "#club bob> hi there",
          cath <# "#club bob> hi there",
          (dan </)
        ]
      cath #> "#club hey"
      concurrentlyN_
        [ alice <# "#club cath> hey",
          bob <# "#club cath> hey",
          (dan </)
        ]
      dan #:> "#club how is it going?"
      dan <## "you are no longer the member of the group"
      dan <##> cath
      dan <##> alice
      -- member leaves
      bob ##> "/l club"
      concurrentlyN_
        [ bob <## "#club: you left the group\nuse /d #club to delete the group",
          alice <## "#club: bob left the group",
          cath <## "#club: bob left the group"
        ]
      alice #> "#club hello"
      concurrently_
        (cath <# "#club alice> hello")
        (bob </)
      cath #> "#club hey"
      concurrently_
        (alice <# "#club cath> hey")
        (bob </)
      bob #:> "#club how is it going?"
      bob <## "you are no longer the member of the group"
      bob <##> cath
      bob <##> alice

testGroupDelete :: IO ()
testGroupDelete =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice ##> "/d #team"
      concurrentlyN_
        [ alice <## "#team: you deleted the group",
          bob <## "#team: alice deleted the group\nuse /d #team to delete the local copy of the group",
          cath <## "#team: alice deleted the group\nuse /d #team to delete the local copy of the group"
        ]
      bob #:> "/d #team"
      bob <## "#team: you deleted the group"
      cath #:> "#team hi"
      cath <## "you are no longer the member of the group"

testGroupRemoveAdd :: IO ()
testGroupRemoveAdd =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      -- remove member
      alice ##> "/rm team bob"
      concurrentlyN_
        [ alice <## "#team: you removed bob from the group",
          bob <## "#team: alice removed you from the group\nuse /d #team to delete the group",
          cath <## "#team: alice removed bob from the group"
        ]
      alice ##> "/a team bob"
      bob <## "#team_1 (team): alice invites you to join the group as admin\nuse /j team_1 to accept"
      bob ##> "/j team_1"
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team_1: you joined the group"
            bob <## "#team_1: member cath_1 (Catherine) is connected"
            bob <## "contact cath_1 is merged into cath\nuse @cath <message> to send messages",
          do
            cath <## "#team: alice added bob_1 (Bob) to the group (connecting...)"
            cath <## "#team: new member bob_1 is connected"
            cath <## "contact bob_1 is merged into bob\nuse @bob <message> to send messages"
        ]
      alice #> "#team hi"
      concurrently_
        (bob <# "#team_1 alice> hi")
        (cath <# "#team alice> hi")
      bob #> "#team_1 hey"
      concurrently_
        (alice <# "#team bob> hey")
        (cath <# "#team bob> hey")
      cath #> "#team hello"
      concurrently_
        (alice <# "#team cath> hello")
        (bob <# "#team_1 cath> hello")

connectUsers :: TestCC -> TestCC -> IO ()
connectUsers cc1 cc2 = do
  cc1 ##> "/c"
  Just inv <- invitation <$> getWindow cc1
  cc2 ##> ("/c " <> inv)
  concurrently_
    (cc2 <## (showName cc1 <> ": contact is connected"))
    (cc1 <## (showName cc2 <> ": contact is connected"))

showName :: TestCC -> String
showName (TestCC ChatController {currentUser = User {localDisplayName, profile = Profile {fullName}}} _ _) =
  T.unpack $ localDisplayName <> " (" <> fullName <> ")"

createGroup3 :: String -> TestCC -> TestCC -> TestCC -> IO ()
createGroup3 gName cc1 cc2 cc3 = do
  connectUsers cc1 cc2
  connectUsers cc1 cc3
  cc1 #:> ("/g " <> gName)
  cc1 <## ("use /a " <> gName <> " <name> to add members")
  cc1 ##> ("/a team " <> name cc2)
  concurrently_
    (cc1 <## ("invitation to join the group #" <> gName <> " sent to " <> name cc2))
    (cc2 <## ("use /j " <> gName <> " to accept"))
  cc2 ##> ("/j " <> gName)
  concurrently_
    (cc1 <## ("#" <> gName <> ": " <> name cc2 <> " joined the group"))
    (cc2 <## ("#" <> gName <> ": you joined the group"))
  cc1 ##> ("/a team " <> name cc3)
  concurrently_
    (cc1 <## ("invitation to join the group #" <> gName <> " sent to " <> name cc3))
    (cc3 <## ("use /j " <> gName <> " to accept"))
  cc3 ##> ("/j " <> gName)
  concurrentlyN_
    [ cc1 <## ("#" <> gName <> ": " <> name cc3 <> " joined the group"),
      do
        cc3 <## ("#" <> gName <> ": you joined the group")
        cc3 <## ("#" <> gName <> ": member " <> showName cc2 <> " is connected"),
      do
        cc2 <## ("#" <> gName <> ": alice added " <> showName cc3 <> " to the group (connecting...)")
        cc2 <## ("#" <> gName <> ": new member " <> name cc3 <> " is connected")
    ]
  where
    name :: TestCC -> String
    name (TestCC ChatController {currentUser = User {localDisplayName}} _ _) =
      T.unpack localDisplayName

-- | test sending direct messages
(<##>) :: TestCC -> TestCC -> IO ()
cc1 <##> cc2 = do
  cc1 #> ("@" <> name cc2 <> " hi")
  cc2 <# (name cc1 <> "> hi")
  cc2 #> ("@" <> name cc1 <> " hey")
  cc1 <# (name cc2 <> "> hey")
  where
    name (TestCC ChatController {currentUser = User {localDisplayName}} _ _) = T.unpack localDisplayName

(##>) :: TestCC -> String -> IO ()
cc ##> cmd = do
  cc #:> cmd
  cc <## cmd

(#>) :: TestCC -> String -> IO ()
cc #> cmd = do
  cc #:> cmd
  cc <# cmd

(#:>) :: TestCC -> String -> IO ()
(TestCC cc _ _) #:> cmd = atomically $ writeTBQueue (inputQ cc) $ InputCommand cmd

(<##) :: TestCC -> String -> Expectation
cc <## line =
  let n = length $ lines line
   in (lastOutput n <$> getWindow cc) `shouldReturn` line

(<###) :: TestCC -> [String] -> Expectation
_ <### [] = pure ()
cc <### ls = do
  line <- lastOutput 1 <$> getWindow cc
  if line `elem` ls
    then cc <### filter (/= line) ls
    else error $ "unexpected output: " <> line

(<#) :: TestCC -> String -> Expectation
cc <# line =
  let n = length $ lines line
   in (dropTime . lastOutput n <$> getWindow cc) `shouldReturn` line

(</) :: TestCC -> Expectation
(</) cc = timeout 500000 (getWindow cc) `shouldReturn` Nothing

dropTime :: String -> String
dropTime msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then text else error "invalid time"
  _ -> error "invalid time"

getWindow :: TestCC -> IO [String]
getWindow (TestCC _ t _) = do
  let w = virtualWindow t
  win <- readTVarIO w
  -- TODO to debug - putStrLn (lastOutput 1 win') - before returning it
  atomically $ do
    win' <- readTVar w
    if win' /= win then pure win' else retry

invitation :: [String] -> Maybe String
invitation win = lastMaybe $ map (dropWhileEnd (== ' ')) $ filter ("smp::" `isPrefixOf`) win

lastOutput :: Int -> [String] -> String
lastOutput n win = intercalate "\n" $ map (dropWhileEnd (== ' ')) $ take n $ drop (length win - n - 1) win -- - 1 to exclude prompt

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs
