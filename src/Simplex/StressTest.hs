{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Simplex.StressTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Monad (forever, when)
import qualified Data.ByteString as B
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Types (Profile (..), User (..))
import Simplex.Messaging.Agent (disconnectAgentClient)
import Simplex.StressTest.ChatClient
import System.Directory
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
chatTests =
  describe "server stress test" $
    -- fit "should stress server with many chats and messages" testStressServer
    fit "server stress test" testStressServerConnectOnly

testStressServerConnectOnly :: IO ()
testStressServerConnectOnly = do
  connectionsTVar <- newTVarIO (0 :: Int)
  concurrentlyN_ $
    ( do
        threadDelay 5000000
        connections <- readTVarIO connectionsTVar
        print $ "total connections over time: " <> show connections
    ) :
    map
      ( \i -> do
          dirExists <- doesDirectoryExist "tests/tmp"
          if not dirExists
            then do
              createDirectoryIfMissing False "tests/tmp"
              testChat2' (i * 2 -1, aliceProfile) (i * 2, bobProfile) $
                \alice bob -> do
                  print $ show i <> " - connected +2"
                  connectUsers alice bob
                  threadDelay 1000000
                  atomically $ modifyTVar connectionsTVar (+ 2)
            else do
              testChat2'' (i * 2 -1) (i * 2) $
                \alice bob -> do
                  print $ show i <> " - connected +2"
                  alice `send` "/help"
                  bob `send` "/help"
                  threadDelay 1000000
                  atomically $ modifyTVar connectionsTVar (+ 2)
      )
      (take 100 ([1 ..] :: [Int]))

-- testStressServerConnectOnly :: IO ()
-- testStressServerConnectOnly =
--   withTmpFiles $ do
--     connectionsTVar <- newTVarIO (0 :: Int)
--     concurrentlyN_ $
--       forever
--         ( do
--             threadDelay 5000000
--             connections <- readTVarIO connectionsTVar
--             print $ "total connections over time: " <> show connections
--         ) :
--       map
--         ( \i -> do
--             testChat2' (i * 2 -1, aliceProfile) (i * 2, bobProfile) $
--               \alice bob -> do
--                 connectUsers alice bob
--                 atomically $ modifyTVar connectionsTVar (+ 2)
--                 disconnectAgent alice
--                 disconnectAgent bob
--             forever $ do
--               threadDelay 5000000
--               testChat2'' (i * 2 -1) (i * 2) $
--                 \alice bob -> do
--                   alice `send` "/help"
--                   bob `send` "/help"
--                   atomically $ modifyTVar connectionsTVar (+ 2)
--                   disconnectAgent alice
--                   disconnectAgent bob
--                   threadDelay 5000000
--                   alice `send` "/help"
--                   bob `send` "/help"
--         )
--         (take 1 ([1 ..] :: [Int]))
--   where
--     disconnectAgent TestCC {chatController = ChatController {smpAgent}} =
--       disconnectAgentClient smpAgent

testStressServer :: IO ()
testStressServer =
  withTmpFiles $ do
    sentTVar <- newTVarIO (0 :: Int)
    connectedTVar <- newTVarIO (0 :: Int)
    concurrentlyN_ $
      forever
        ( do
            threadDelay 5000000
            sent <- readTVarIO sentTVar
            connected <- readTVarIO connectedTVar
            print $ "connected: " <> show connected <> " -- sent: " <> show sent
        ) :
      map
        ( \i ->
            testChat2' (i * 2 -1, aliceProfile) (i * 2, bobProfile) $
              \alice bob -> do
                print $ show i <> " - connected +2"
                atomically $ modifyTVar connectedTVar (+ 2)
                connectUsers alice bob
                loop i alice bob sentTVar 1
        )
        (take 100 ([1 ..] :: [Int]))
  where
    loop :: Int -> TestCC -> TestCC -> TVar Int -> Int -> IO ()
    loop i alice bob sentTVar k = do
      alice `send` "@bob hi"
      bob `send` "@alice hi"
      when (k `mod` 100 == 0) $ do
        print $ show i <> " - +200"
        atomically $ modifyTVar sentTVar (+ 200)
      -- threadDelay 500
      loop i alice bob sentTVar $ k + 1

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
  cc2 <## "confirmation sent!"
  concurrently_
    (cc2 <## (name1 <> ": contact is connected"))
    (cc1 <## (name2 <> ": contact is connected"))

showName :: TestCC -> IO String
showName (TestCC ChatController {currentUser} _ _ _ _) = do
  Just User {localDisplayName, profile = Profile {fullName}} <- readTVarIO currentUser
  pure . T.unpack $ localDisplayName <> " (" <> fullName <> ")"

createGroup2 :: String -> TestCC -> TestCC -> IO ()
createGroup2 gName cc1 cc2 = do
  connectUsers cc1 cc2
  name2 <- userName cc2
  cc1 ##> ("/g " <> gName)
  cc1 <## ("group #" <> gName <> " is created")
  cc1 <## ("use /a " <> gName <> " <name> to add members")
  addMember gName cc1 cc2
  cc2 ##> ("/j " <> gName)
  concurrently_
    (cc1 <## ("#" <> gName <> ": " <> name2 <> " joined the group"))
    (cc2 <## ("#" <> gName <> ": you joined the group"))

createGroup3 :: String -> TestCC -> TestCC -> TestCC -> IO ()
createGroup3 gName cc1 cc2 cc3 = do
  createGroup2 gName cc1 cc2
  connectUsers cc1 cc3
  name3 <- userName cc3
  sName2 <- showName cc2
  sName3 <- showName cc3
  addMember gName cc1 cc3
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

addMember :: String -> TestCC -> TestCC -> IO ()
addMember gName inviting invitee = do
  name1 <- userName inviting
  memName <- userName invitee
  inviting ##> ("/a " <> gName <> " " <> memName)
  concurrentlyN_
    [ inviting <## ("invitation to join the group #" <> gName <> " sent to " <> memName),
      do
        invitee <## ("#" <> gName <> ": " <> name1 <> " invites you to join the group as admin")
        invitee <## ("use /j " <> gName <> " to accept")
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
userName (TestCC ChatController {currentUser} _ _ _ _) = T.unpack . localDisplayName . fromJust <$> readTVarIO currentUser

(##>) :: TestCC -> String -> IO ()
cc ##> cmd = do
  cc `send` cmd
  cc <## cmd

(#>) :: TestCC -> String -> IO ()
cc #> cmd = do
  cc `send` cmd
  cc <# cmd

(#$>) :: (Eq a, Show a) => TestCC -> (String, String -> a, a) -> Expectation
cc #$> (cmd, f, res) = do
  cc ##> cmd
  (f <$> getTermLine cc) `shouldReturn` res

chat :: String -> [(Int, String)]
chat = read

(#$$>) :: TestCC -> (String, [(String, String)]) -> Expectation
cc #$$> (cmd, res) = do
  cc ##> cmd
  line <- getTermLine cc
  let chats = read line
  chats `shouldMatchList` res

send :: TestCC -> String -> IO ()
send TestCC {chatController = cc} cmd = atomically $ writeTBQueue (inputQ cc) cmd

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
(</) = (<// 500000)

(<#?) :: TestCC -> TestCC -> Expectation
cc1 <#? cc2 = do
  name <- userName cc2
  sName <- showName cc2
  cc2 <## "connection request sent!"
  cc1 <## (sName <> " wants to connect to you!")
  cc1 <## ("to accept: /ac " <> name)
  cc1 <## ("to reject: /rc " <> name <> " (the sender will NOT be notified)")

dropTime :: String -> String
dropTime msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then text else error "invalid time"
  _ -> error "invalid time"

getInvitation :: TestCC -> IO String
getInvitation cc = do
  cc <## "pass this invitation link to your contact (via another channel):"
  cc <## ""
  inv <- getTermLine cc
  cc <## ""
  cc <## "and ask them to connect: /c <invitation_link_above>"
  pure inv

getContactLink :: TestCC -> Bool -> IO String
getContactLink cc created = do
  cc <## if created then "Your new chat address is created!" else "Your chat address:"
  cc <## ""
  link <- getTermLine cc
  cc <## ""
  cc <## "Anybody can send you contact requests with: /c <contact_link_above>"
  cc <## "to show it again: /sa"
  cc <## "to delete it: /da (accepted contacts will remain connected)"
  pure link
