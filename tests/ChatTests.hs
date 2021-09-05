{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests where

import ChatClient
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import qualified Data.ByteString as B
import Data.Char (isDigit)
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Types (Profile (..), User (..))
import Simplex.Chat.Util (unlessM)
import System.Directory (doesFileExist)
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
  describe "user profiles" $
    it "update user profiles and notify contacts" testUpdateProfile
  describe "sending and receiving files" $ do
    it "send and receive file" testFileTransfer
    it "sender cancelled file transfer" testFileSndCancel
    it "recipient cancelled file transfer" testFileRcvCancel
    it "send and receive file to group" testGroupFileTransfer

testAddContact :: IO ()
testAddContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/c"
      inv <- getInvitation alice
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
      inv' <- getInvitation alice
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
      alice #> "@bob_1 hey"
      alice <## "no contact bob_1"

testGroup :: IO ()
testGroup =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      connectUsers alice bob
      connectUsers alice cath
      alice ##> "/g team"
      alice <## "group #team is created"
      alice <## "use /a team <name> to add members"
      alice ##> "/a team bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to bob",
          do
            bob <## "#team: alice invites you to join the group as admin"
            bob <## "use /j team to accept"
        ]
      bob ##> "/j team"
      concurrently_
        (alice <## "#team: bob joined the group")
        (bob <## "#team: you joined the group")
      alice ##> "/a team cath"
      concurrentlyN_
        [ alice <## "invitation to join the group #team sent to cath",
          do
            cath <## "#team: alice invites you to join the group as admin"
            cath <## "use /j team to accept"
        ]
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
          do
            cath <## "#team: bob removed you from the group"
            cath <## "use /d #team to delete the group"
        ]
      bob #> "#team hi"
      concurrently_
        (alice <# "#team bob> hi")
        (cath </)
      alice #> "#team hello"
      concurrently_
        (bob <# "#team alice> hello")
        (cath </)
      cath #> "#team hello"
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
      alice ##> "/g club"
      alice <## "group #club is created"
      alice <## "use /a club <name> to add members"
      alice ##> "/a club bob"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to bob",
          do
            bob <## "#club: alice invites you to join the group as admin"
            bob <## "use /j club to accept"
        ]
      alice ##> "/a club cath"
      concurrentlyN_
        [ alice <## "invitation to join the group #club sent to cath",
          do
            cath <## "#club: alice invites you to join the group as admin"
            cath <## "use /j club to accept"
        ]
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
      concurrentlyN_
        [ bob <## "invitation to join the group #club sent to dan",
          do
            dan <## "#club: bob invites you to join the group as admin"
            dan <## "use /j club to accept"
        ]
      dan ##> "/j club"
      concurrentlyN_
        [ bob <## "#club: dan joined the group",
          do
            dan <## "#club: you joined the group"
            dan
              <### [ "#club: member alice_1 (Alice) is connected",
                     "contact alice_1 is merged into alice",
                     "use @alice <message> to send messages",
                     "#club: member cath (Catherine) is connected"
                   ],
          do
            alice <## "#club: bob added dan_1 (Daniel) to the group (connecting...)"
            alice <## "#club: new member dan_1 is connected"
            alice <## "contact dan_1 is merged into dan"
            alice <## "use @dan <message> to send messages",
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
          do
            dan <## "#club: cath removed you from the group"
            dan <## "use /d #club to delete the group"
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
      dan #> "#club how is it going?"
      dan <## "you are no longer the member of the group"
      dan <##> cath
      dan <##> alice
      -- member leaves
      bob ##> "/l club"
      concurrentlyN_
        [ do
            bob <## "#club: you left the group"
            bob <## "use /d #club to delete the group",
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
      bob #> "#club how is it going?"
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
          do
            bob <## "#team: alice deleted the group"
            bob <## "use /d #team to delete the local copy of the group",
          do
            cath <## "#team: alice deleted the group"
            cath <## "use /d #team to delete the local copy of the group"
        ]
      bob ##> "/d #team"
      bob <## "#team: you deleted the group"
      cath #> "#team hi"
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
          do
            bob <## "#team: alice removed you from the group"
            bob <## "use /d #team to delete the group",
          cath <## "#team: alice removed bob from the group"
        ]
      alice ##> "/a team bob"
      alice <## "invitation to join the group #team sent to bob"
      bob <## "#team_1 (team): alice invites you to join the group as admin"
      bob <## "use /j team_1 to accept"
      bob ##> "/j team_1"
      concurrentlyN_
        [ alice <## "#team: bob joined the group",
          do
            bob <## "#team_1: you joined the group"
            bob <## "#team_1: member cath_1 (Catherine) is connected"
            bob <## "contact cath_1 is merged into cath"
            bob <## "use @cath <message> to send messages",
          do
            cath <## "#team: alice added bob_1 (Bob) to the group (connecting...)"
            cath <## "#team: new member bob_1 is connected"
            cath <## "contact bob_1 is merged into bob"
            cath <## "use @bob <message> to send messages"
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

testUpdateProfile :: IO ()
testUpdateProfile =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <display name> [<full name>] to change it"
      alice <## "(the updated profile will be sent to all your contacts)"
      alice ##> "/p alice"
      concurrentlyN_
        [ alice <## "user full name removed (your contacts are notified)",
          bob <## "contact alice removed full name",
          cath <## "contact alice removed full name"
        ]
      alice ##> "/p alice Alice Jones"
      concurrentlyN_
        [ alice <## "user full name changed to Alice Jones (your contacts are notified)",
          bob <## "contact alice updated full name: Alice Jones",
          cath <## "contact alice updated full name: Alice Jones"
        ]
      cath ##> "/p cate"
      concurrentlyN_
        [ cath <## "user profile is changed to cate (your contacts are notified)",
          do
            alice <## "contact cath changed to cate"
            alice <## "use @cate <message> to send messages",
          do
            bob <## "contact cath changed to cate"
            bob <## "use @cate <message> to send messages"
        ]
      cath ##> "/p cat Cate"
      concurrentlyN_
        [ cath <## "user profile is changed to cat (Cate) (your contacts are notified)",
          do
            alice <## "contact cate changed to cat (Cate)"
            alice <## "use @cat <message> to send messages",
          do
            bob <## "contact cate changed to cat (Cate)"
            bob <## "use @cat <message> to send messages"
        ]

testFileTransfer :: IO ()
testFileTransfer =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer alice bob
      concurrentlyN_
        [ do
            bob #> "@alice receiving here..."
            bob <## "completed receiving file 1 (test.jpg) from alice",
          do
            alice <# "bob> receiving here..."
            alice <## "completed sending file 1 (test.jpg) to bob"
        ]
      src <- B.readFile "./tests/fixtures/test.jpg"
      dest <- B.readFile "./tests/tmp/test.jpg"
      dest `shouldBe` src

testFileSndCancel :: IO ()
testFileSndCancel =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer alice bob
      alice ##> "/fc 1"
      concurrentlyN_
        [ do
            alice <## "cancelled sending file 1 (test.jpg) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) cancelled",
          do
            bob <## "alice cancelled sending file 1 (test.jpg)"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test.jpg) cancelled, received part path: ./tests/tmp/test.jpg"
        ]
      checkPartialTransfer

testFileRcvCancel :: IO ()
testFileRcvCancel =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      startFileTransfer alice bob
      bob ##> "/fs 1"
      getTermLine bob >>= (`shouldStartWith` "receiving file 1 (test.jpg) progress")
      waitFileExists "./tests/tmp/test.jpg"
      bob ##> "/fc 1"
      concurrentlyN_
        [ do
            bob <## "cancelled receiving file 1 (test.jpg) from alice"
            bob ##> "/fs 1"
            bob <## "receiving file 1 (test.jpg) cancelled, received part path: ./tests/tmp/test.jpg",
          do
            alice <## "bob cancelled receiving file 1 (test.jpg)"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg) cancelled"
        ]
      checkPartialTransfer
  where
    waitFileExists f = unlessM (doesFileExist f) $ waitFileExists f

testGroupFileTransfer :: IO ()
testGroupFileTransfer =
  testChat3 aliceProfile bobProfile cathProfile $
    \alice bob cath -> do
      createGroup3 "team" alice bob cath
      alice #> "/f #team ./tests/fixtures/test.jpg"
      alice <## "use /fc 1 to cancel sending"
      concurrentlyN_
        [ do
            bob <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            bob <## "use /fr 1 [<dir>/ | <path>] to receive it",
          do
            cath <# "#team alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
            cath <## "use /fr 1 [<dir>/ | <path>] to receive it"
        ]
      alice ##> "/fs 1"
      getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg) not accepted")
      bob ##> "/fr 1 ./tests/tmp/"
      bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to bob"
            alice <## "completed sending file 1 (test.jpg) to bob"
            alice ##> "/fs 1"
            alice <## "sending file 1 (test.jpg):"
            alice <### ["  complete: bob", "  not accepted: cath"],
          do
            bob <## "started receiving file 1 (test.jpg) from alice"
            bob <## "completed receiving file 1 (test.jpg) from alice"
        ]
      cath ##> "/fr 1 ./tests/tmp/"
      cath <## "saving file 1 from alice to ./tests/tmp/test_1.jpg"
      concurrentlyN_
        [ do
            alice <## "started sending file 1 (test.jpg) to cath"
            alice <## "completed sending file 1 (test.jpg) to cath"
            alice ##> "/fs 1"
            getTermLine alice >>= (`shouldStartWith` "sending file 1 (test.jpg) complete"),
          do
            cath <## "started receiving file 1 (test.jpg) from alice"
            cath <## "completed receiving file 1 (test.jpg) from alice"
        ]

startFileTransfer :: TestCC -> TestCC -> IO ()
startFileTransfer alice bob = do
  alice #> "/f @bob ./tests/fixtures/test.jpg"
  alice <## "use /fc 1 to cancel sending"
  bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
  bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
  bob ##> "/fr 1 ./tests/tmp"
  bob <## "saving file 1 from alice to ./tests/tmp/test.jpg"
  concurrently_
    (bob <## "started receiving file 1 (test.jpg) from alice")
    (alice <## "started sending file 1 (test.jpg) to bob")

checkPartialTransfer :: IO ()
checkPartialTransfer = do
  src <- B.readFile "./tests/fixtures/test.jpg"
  dest <- B.readFile "./tests/tmp/test.jpg"
  B.unpack src `shouldStartWith` B.unpack dest
  B.length src > B.length dest `shouldBe` True

connectUsers :: TestCC -> TestCC -> IO ()
connectUsers cc1 cc2 = do
  name1 <- showName cc1
  name2 <- showName cc2
  cc1 ##> "/c"
  inv <- getInvitation cc1
  cc2 ##> ("/c " <> inv)
  concurrently_
    (cc2 <## (name1 <> ": contact is connected"))
    (cc1 <## (name2 <> ": contact is connected"))

showName :: TestCC -> IO String
showName (TestCC ChatController {currentUser} _ _ _ _) = do
  User {localDisplayName, profile = Profile {fullName}} <- readTVarIO currentUser
  pure . T.unpack $ localDisplayName <> " (" <> fullName <> ")"

createGroup3 :: String -> TestCC -> TestCC -> TestCC -> IO ()
createGroup3 gName cc1 cc2 cc3 = do
  connectUsers cc1 cc2
  connectUsers cc1 cc3
  name2 <- userName cc2
  name3 <- userName cc3
  sName2 <- showName cc2
  sName3 <- showName cc3
  cc1 ##> ("/g " <> gName)
  cc1 <## ("group #" <> gName <> " is created")
  cc1 <## ("use /a " <> gName <> " <name> to add members")
  addMember cc2
  cc2 ##> ("/j " <> gName)
  concurrently_
    (cc1 <## ("#" <> gName <> ": " <> name2 <> " joined the group"))
    (cc2 <## ("#" <> gName <> ": you joined the group"))
  addMember cc3
  cc3 ##> ("/j " <> gName)
  concurrentlyN_
    [ cc1 <## ("#" <> gName <> ": " <> name3 <> " joined the group"),
      do
        cc3 <## ("#" <> gName <> ": you joined the group")
        cc3 <## ("#" <> gName <> ": member " <> sName2 <> " is connected"),
      do
        cc2 <## ("#" <> gName <> ": alice added " <> sName3 <> " to the group (connecting...)")
        cc2 <## ("#" <> gName <> ": new member " <> name3 <> " is connected")
    ]
  where
    addMember :: TestCC -> IO ()
    addMember mem = do
      name1 <- userName cc1
      memName <- userName mem
      cc1 ##> ("/a " <> gName <> " " <> memName)
      concurrentlyN_
        [ cc1 <## ("invitation to join the group #" <> gName <> " sent to " <> memName),
          do
            mem <## ("#" <> gName <> ": " <> name1 <> " invites you to join the group as admin")
            mem <## ("use /j " <> gName <> " to accept")
        ]

-- | test sending direct messages
(<##>) :: TestCC -> TestCC -> IO ()
cc1 <##> cc2 = do
  name1 <- userName cc1
  name2 <- userName cc2
  cc1 #> ("@" <> name2 <> " hi")
  cc2 <# (name1 <> "> hi")
  cc2 #> ("@" <> name1 <> " hey")
  cc1 <# (name2 <> "> hey")

userName :: TestCC -> IO [Char]
userName (TestCC ChatController {currentUser} _ _ _ _) = T.unpack . localDisplayName <$> readTVarIO currentUser

(##>) :: TestCC -> String -> IO ()
cc ##> cmd = do
  cc `send` cmd
  cc <## cmd

(#>) :: TestCC -> String -> IO ()
cc #> cmd = do
  cc `send` cmd
  cc <# cmd

send :: TestCC -> String -> IO ()
send TestCC {chatController = cc} cmd = atomically $ writeTBQueue (inputQ cc) $ InputCommand cmd

(<##) :: TestCC -> String -> Expectation
cc <## line = getTermLine cc `shouldReturn` line

(<###) :: TestCC -> [String] -> Expectation
_ <### [] = pure ()
cc <### ls = do
  line <- getTermLine cc
  if line `elem` ls
    then cc <### filter (/= line) ls
    else error $ "unexpected output: " <> line

(<#) :: TestCC -> String -> Expectation
cc <# line = (dropTime <$> getTermLine cc) `shouldReturn` line

(</) :: TestCC -> Expectation
(</) cc = timeout 500000 (getTermLine cc) `shouldReturn` Nothing

dropTime :: String -> String
dropTime msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then text else error "invalid time"
  _ -> error "invalid time"

getTermLine :: TestCC -> IO String
getTermLine = atomically . readTQueue . termQ

getInvitation :: TestCC -> IO String
getInvitation cc = do
  cc <## "pass this invitation to your contact (via another channel):"
  cc <## ""
  inv <- getTermLine cc
  cc <## ""
  cc <## "and ask them to connect: /c <invitation_above>"
  pure inv
