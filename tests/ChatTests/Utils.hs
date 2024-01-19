{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ChatTests.Utils where

import ChatClient
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.String
import qualified Data.Text as T
import Database.SQLite.Simple (Only (..))
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), InlineFilesConfig (..), defaultInlineFilesConfig)
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Profiles (getUserContactProfiles)
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.FileTransfer.Client.Main (xftpClientCLI)
import Simplex.Messaging.Agent.Store.SQLite (maybeFirstRow, withTransaction)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Version
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, withArgs)
import System.FilePath ((</>))
import System.IO.Silently (capture_)
import System.Info (os)
import Test.Hspec

defaultPrefs :: Maybe Preferences
defaultPrefs = Just $ toChatPrefs defaultChatPrefs

aliceDesktopProfile :: Profile
aliceDesktopProfile = Profile {displayName = "alice_desktop", fullName = "Alice Desktop", image = Nothing, contactLink = Nothing, preferences = defaultPrefs}

aliceProfile :: Profile
aliceProfile = Profile {displayName = "alice", fullName = "Alice", image = Nothing, contactLink = Nothing, preferences = defaultPrefs}

bobProfile :: Profile
bobProfile = Profile {displayName = "bob", fullName = "Bob", image = Just (ImageData "data:image/png;base64,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"), contactLink = Nothing, preferences = defaultPrefs}

cathProfile :: Profile
cathProfile = Profile {displayName = "cath", fullName = "Catherine", image = Nothing, contactLink = Nothing, preferences = defaultPrefs}

danProfile :: Profile
danProfile = Profile {displayName = "dan", fullName = "Daniel", image = Nothing, contactLink = Nothing, preferences = defaultPrefs}

xit' :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
xit' = if os == "linux" then xit else it

xit'' :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)
xit'' = ifCI xit it

xdescribe'' :: HasCallStack => String -> SpecWith a -> SpecWith a
xdescribe'' = ifCI xdescribe describe

ifCI :: HasCallStack => (HasCallStack => String -> a -> SpecWith b) -> (HasCallStack => String -> a -> SpecWith b) -> String -> a -> SpecWith b
ifCI xrun run d t = do
  ci <- runIO $ lookupEnv "CI"
  (if ci == Just "true" then xrun else run) d t

versionTestMatrix2 :: (HasCallStack => TestCC -> TestCC -> IO ()) -> SpecWith FilePath
versionTestMatrix2 runTest = do
  it "current" $ testChat2 aliceProfile bobProfile runTest
  it "prev" $ testChatCfg2 testCfgVPrev aliceProfile bobProfile runTest
  it "prev to curr" $ runTestCfg2 testCfg testCfgVPrev runTest
  it "curr to prev" $ runTestCfg2 testCfgVPrev testCfg runTest
  it "v1" $ testChatCfg2 testCfgV1 aliceProfile bobProfile runTest
  it "v1 to v2" $ runTestCfg2 testCfg testCfgV1 runTest
  it "v2 to v1" $ runTestCfg2 testCfgV1 testCfg runTest

versionTestMatrix3 :: (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> SpecWith FilePath
versionTestMatrix3 runTest = do
  it "current" $ testChat3 aliceProfile bobProfile cathProfile runTest
  it "prev" $ testChatCfg3 testCfgVPrev aliceProfile bobProfile cathProfile runTest
  it "prev to curr" $ runTestCfg3 testCfg testCfgVPrev testCfgVPrev runTest
  it "curr+prev to curr" $ runTestCfg3 testCfg testCfg testCfgVPrev runTest
  it "curr to prev" $ runTestCfg3 testCfgVPrev testCfg testCfg runTest
  it "curr+prev to prev" $ runTestCfg3 testCfgVPrev testCfg testCfgVPrev runTest

inlineCfg :: Integer -> ChatConfig
inlineCfg n = testCfg {inlineFiles = defaultInlineFilesConfig {sendChunks = 0, offerChunks = n, receiveChunks = n}}

fileTestMatrix2 :: (HasCallStack => TestCC -> TestCC -> IO ()) -> SpecWith FilePath
fileTestMatrix2 runTest = do
  it "via connection" $ runTestCfg2 viaConn viaConn runTest
  it "inline (accepting)" $ runTestCfg2 inline inline runTest
  it "via connection (inline offered)" $ runTestCfg2 inline viaConn runTest
  it "via connection (inline supported)" $ runTestCfg2 viaConn inline runTest
  where
    inline = inlineCfg 100
    viaConn = inlineCfg 0

fileTestMatrix3 :: (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> SpecWith FilePath
fileTestMatrix3 runTest = do
  it "via connection" $ runTestCfg3 viaConn viaConn viaConn runTest
  it "inline" $ runTestCfg3 inline inline inline runTest
  it "via connection (inline offered)" $ runTestCfg3 inline viaConn viaConn runTest
  it "via connection (inline supported)" $ runTestCfg3 viaConn inline inline runTest
  where
    inline = inlineCfg 100
    viaConn = inlineCfg 0

runTestCfg2 :: ChatConfig -> ChatConfig -> (HasCallStack => TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
runTestCfg2 aliceCfg bobCfg runTest tmp =
  withNewTestChatCfg tmp aliceCfg "alice" aliceProfile $ \alice ->
    withNewTestChatCfg tmp bobCfg "bob" bobProfile $ \bob ->
      runTest alice bob

runTestCfg3 :: ChatConfig -> ChatConfig -> ChatConfig -> (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
runTestCfg3 aliceCfg bobCfg cathCfg runTest tmp =
  withNewTestChatCfg tmp aliceCfg "alice" aliceProfile $ \alice ->
    withNewTestChatCfg tmp bobCfg "bob" bobProfile $ \bob ->
      withNewTestChatCfg tmp cathCfg "cath" cathProfile $ \cath ->
        runTest alice bob cath

withTestChatGroup3Connected :: HasCallStack => FilePath -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatGroup3Connected tmp dbPrefix action = do
  withTestChat tmp dbPrefix $ \cc -> do
    cc <## "2 contacts connected (use /cs for the list)"
    cc <## "#team: connected to server(s)"
    action cc

withTestChatGroup3Connected' :: HasCallStack => FilePath -> String -> IO ()
withTestChatGroup3Connected' tmp dbPrefix = withTestChatGroup3Connected tmp dbPrefix $ \_ -> pure ()

withTestChatContactConnected :: HasCallStack => FilePath -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatContactConnected tmp dbPrefix action =
  withTestChat tmp dbPrefix $ \cc -> do
    cc <## "1 contacts connected (use /cs for the list)"
    action cc

withTestChatContactConnected' :: HasCallStack => FilePath -> String -> IO ()
withTestChatContactConnected' tmp dbPrefix = withTestChatContactConnected tmp dbPrefix $ \_ -> pure ()

withTestChatContactConnectedV1 :: HasCallStack => FilePath -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatContactConnectedV1 tmp dbPrefix action =
  withTestChatV1 tmp dbPrefix $ \cc -> do
    cc <## "1 contacts connected (use /cs for the list)"
    action cc

withTestChatContactConnectedV1' :: HasCallStack => FilePath -> String -> IO ()
withTestChatContactConnectedV1' tmp dbPrefix = withTestChatContactConnectedV1 tmp dbPrefix $ \_ -> pure ()

-- | test sending direct messages
(<##>) :: HasCallStack => TestCC -> TestCC -> IO ()
cc1 <##> cc2 = do
  name1 <- userName cc1
  name2 <- userName cc2
  cc1 #> ("@" <> name2 <> " hi")
  cc2 <# (name1 <> "> hi")
  cc2 #> ("@" <> name1 <> " hey")
  cc1 <# (name2 <> "> hey")

(##>) :: HasCallStack => TestCC -> String -> IO ()
cc ##> cmd = do
  cc `send` cmd
  cc <## cmd

(#>) :: HasCallStack => TestCC -> String -> IO ()
cc #> cmd = do
  cc `send` cmd
  cc <# cmd

(?#>) :: HasCallStack => TestCC -> String -> IO ()
cc ?#> cmd = do
  cc `send` cmd
  cc <# ("i " <> cmd)

(#$>) :: (Eq a, Show a, HasCallStack) => TestCC -> (String, String -> a, a) -> Expectation
cc #$> (cmd, f, res) = do
  cc ##> cmd
  (f <$> getTermLine cc) `shouldReturn` res

chat :: String -> [(Int, String)]
chat = map (\(a, _, _) -> a) . chat''

chat' :: String -> [((Int, String), Maybe (Int, String))]
chat' = map (\(a, b, _) -> (a, b)) . chat''

chatF :: String -> [((Int, String), Maybe String)]
chatF = map (\(a, _, c) -> (a, c)) . chat''

chat'' :: String -> [((Int, String), Maybe (Int, String), Maybe String)]
chat'' = read

chatFeatures :: [(Int, String)]
chatFeatures = map (\(a, _, _) -> a) chatFeatures''

chatFeatures' :: [((Int, String), Maybe (Int, String))]
chatFeatures' = map (\(a, b, _) -> (a, b)) chatFeatures''

chatFeaturesF :: [((Int, String), Maybe String)]
chatFeaturesF = map (\(a, _, c) -> (a, c)) chatFeatures''

chatFeatures'' :: [((Int, String), Maybe (Int, String), Maybe String)]
chatFeatures'' =
  [ ((0, "Disappearing messages: allowed"), Nothing, Nothing),
    ((0, "Full deletion: off"), Nothing, Nothing),
    ((0, "Message reactions: enabled"), Nothing, Nothing),
    ((0, "Voice messages: enabled"), Nothing, Nothing),
    ((0, "Audio/video calls: enabled"), Nothing, Nothing)
  ]

lastChatFeature :: String
lastChatFeature = snd $ last chatFeatures

groupFeatures :: [(Int, String)]
groupFeatures = map (\(a, _, _) -> a) groupFeatures''

groupFeatures'' :: [((Int, String), Maybe (Int, String), Maybe String)]
groupFeatures'' =
  [ ((0, "Disappearing messages: off"), Nothing, Nothing),
    ((0, "Direct messages: on"), Nothing, Nothing),
    ((0, "Full deletion: off"), Nothing, Nothing),
    ((0, "Message reactions: on"), Nothing, Nothing),
    ((0, "Voice messages: on"), Nothing, Nothing),
    ((0, "Files and media: on"), Nothing, Nothing)
    -- ((0, "Recent history: on"), Nothing, Nothing)
  ]

itemId :: Int -> String
itemId i = show $ length chatFeatures + i

(@@@) :: HasCallStack => TestCC -> [(String, String)] -> Expectation
(@@@) cc res = do
  threadDelay 10000
  getChats mapChats cc res

mapChats :: [(String, String, Maybe ConnStatus)] -> [(String, String)]
mapChats = map $ \(ldn, msg, _) -> (ldn, msg)

chats :: String -> [(String, String)]
chats = mapChats . read

(@@@!) :: HasCallStack => TestCC -> [(String, String, Maybe ConnStatus)] -> Expectation
(@@@!) = getChats id

getChats :: HasCallStack => (Eq a, Show a) => ([(String, String, Maybe ConnStatus)] -> [a]) -> TestCC -> [a] -> Expectation
getChats f cc res = do
  cc ##> "/_get chats 1 pcc=on"
  line <- getTermLine cc
  f (read line) `shouldMatchList` res

send :: TestCC -> String -> IO ()
send TestCC {chatController = cc} cmd = atomically $ writeTBQueue (inputQ cc) cmd

(<##) :: HasCallStack => TestCC -> String -> Expectation
cc <## line = do
  l <- getTermLine cc
  when (l /= line) $ print ("expected: " <> line, ", got: " <> l)
  l `shouldBe` line

(<##.) :: HasCallStack => TestCC -> String -> Expectation
cc <##. line = do
  l <- getTermLine cc
  let prefix = line `isPrefixOf` l
  unless prefix $ print ("expected to start from: " <> line, ", got: " <> l)
  prefix `shouldBe` True

(.<##) :: HasCallStack => TestCC -> String -> Expectation
cc .<## line = do
  l <- getTermLine cc
  let suffix = line `isSuffixOf` l
  unless suffix $ print ("expected to end with: " <> line, ", got: " <> l)
  suffix `shouldBe` True

(<#.) :: HasCallStack => TestCC -> String -> Expectation
cc <#. line = do
  l <- dropTime <$> getTermLine cc
  let prefix = line `isPrefixOf` l
  unless prefix $ print ("expected to start from: " <> line, ", got: " <> l)
  prefix `shouldBe` True

(<##..) :: HasCallStack => TestCC -> [String] -> Expectation
cc <##.. ls = do
  l <- getTermLine cc
  let prefix = any (`isPrefixOf` l) ls
  unless prefix $ print ("expected to start from one of: " <> show ls, ", got: " <> l)
  prefix `shouldBe` True

data ConsoleResponse
  = ConsoleString String
  | WithTime String
  | EndsWith String
  | StartsWith String
  | Predicate (String -> Bool)

instance IsString ConsoleResponse where fromString = ConsoleString

-- this assumes that the string can only match one option
getInAnyOrder :: HasCallStack => (String -> String) -> TestCC -> [ConsoleResponse] -> Expectation
getInAnyOrder _ _ [] = pure ()
getInAnyOrder f cc ls = do
  line <- f <$> getTermLine cc
  let rest = filterFirst (expected line) ls
  if length rest < length ls
    then getInAnyOrder f cc rest
    else error $ "unexpected output: " <> line
  where
    expected :: String -> ConsoleResponse -> Bool
    expected l = \case
      ConsoleString s -> l == s
      WithTime s -> dropTime_ l == Just s
      EndsWith s -> s `isSuffixOf` l
      StartsWith s -> s `isPrefixOf` l
      Predicate p -> p l
    filterFirst :: (a -> Bool) -> [a] -> [a]
    filterFirst _ [] = []
    filterFirst p (x : xs)
      | p x = xs
      | otherwise = x : filterFirst p xs

(<###) :: HasCallStack => TestCC -> [ConsoleResponse] -> Expectation
(<###) = getInAnyOrder id

(<##?) :: HasCallStack => TestCC -> [ConsoleResponse] -> Expectation
(<##?) = getInAnyOrder dropTime

(<#) :: HasCallStack => TestCC -> String -> Expectation
cc <# line = (dropTime <$> getTermLine cc) `shouldReturn` line

(*<#) :: HasCallStack => [TestCC] -> String -> Expectation
ccs *<# line = concurrentlyN_ $ map (<# line) ccs

(?<#) :: HasCallStack => TestCC -> String -> Expectation
cc ?<# line = (dropTime <$> getTermLine cc) `shouldReturn` "i " <> line

($<#) :: HasCallStack => (TestCC, String) -> String -> Expectation
(cc, uName) $<# line = (dropTime . dropUser uName <$> getTermLine cc) `shouldReturn` line

(^<#) :: HasCallStack => (TestCC, String) -> String -> Expectation
(cc, p) ^<# line = (dropTime . dropStrPrefix p <$> getTermLine cc) `shouldReturn` line

(⩗) :: HasCallStack => TestCC -> String -> Expectation
cc ⩗ line = (dropTime . dropReceipt <$> getTermLine cc) `shouldReturn` line

(%) :: HasCallStack => TestCC -> String -> Expectation
cc % line = (dropTime . dropPartialReceipt <$> getTermLine cc) `shouldReturn` line

(</) :: HasCallStack => TestCC -> Expectation
(</) = (<// 500000)

(<#?) :: HasCallStack => TestCC -> TestCC -> Expectation
cc1 <#? cc2 = do
  name <- userName cc2
  sName <- showName cc2
  cc2 <## "connection request sent!"
  cc1 <## (sName <> " wants to connect to you!")
  cc1 <## ("to accept: /ac " <> name)
  cc1 <## ("to reject: /rc " <> name <> " (the sender will NOT be notified)")

dropUser :: HasCallStack => String -> String -> String
dropUser uName msg = fromMaybe err $ dropUser_ uName msg
  where
    err = error $ "invalid user: " <> msg

dropUser_ :: String -> String -> Maybe String
dropUser_ uName msg = do
  let userPrefix = "[user: " <> uName <> "] "
  if userPrefix `isPrefixOf` msg
    then Just $ drop (length userPrefix) msg
    else Nothing

dropTime :: HasCallStack => String -> String
dropTime msg = fromMaybe err $ dropTime_ msg
  where
    err = error $ "invalid time: " <> msg

dropTime_ :: String -> Maybe String
dropTime_ msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then Just text else Nothing
  _ -> Nothing

dropStrPrefix :: HasCallStack => String -> String -> String
dropStrPrefix pfx s =
  let (p, rest) = splitAt (length pfx) s
   in if p == pfx then rest else error $ "no prefix " <> pfx <> " in string : " <> s

dropReceipt :: HasCallStack => String -> String
dropReceipt msg = fromMaybe err $ dropReceipt_ msg
  where
    err = error $ "invalid receipt: " <> msg

dropReceipt_ :: String -> Maybe String
dropReceipt_ msg = case splitAt 2 msg of
  ("⩗ ", text) -> Just text
  _ -> Nothing

dropPartialReceipt :: HasCallStack => String -> String
dropPartialReceipt msg = fromMaybe err $ dropPartialReceipt_ msg
  where
    err = error $ "invalid partial receipt: " <> msg

dropPartialReceipt_ :: String -> Maybe String
dropPartialReceipt_ msg = case splitAt 2 msg of
  ("% ", text) -> Just text
  _ -> Nothing

getInvitation :: HasCallStack => TestCC -> IO String
getInvitation cc = do
  cc <## "pass this invitation link to your contact (via another channel):"
  cc <## ""
  inv <- getTermLine cc
  cc <## ""
  cc <## "and ask them to connect: /c <invitation_link_above>"
  pure inv

getContactLink :: HasCallStack => TestCC -> Bool -> IO String
getContactLink cc created = do
  cc <## if created then "Your new chat address is created!" else "Your chat address:"
  cc <## ""
  link <- getTermLine cc
  cc <## ""
  cc <## "Anybody can send you contact requests with: /c <contact_link_above>"
  cc <## "to show it again: /sa"
  cc <## "to share with your contacts: /profile_address on"
  cc <## "to delete it: /da (accepted contacts will remain connected)"
  pure link

getGroupLink :: HasCallStack => TestCC -> String -> GroupMemberRole -> Bool -> IO String
getGroupLink cc gName mRole created = do
  cc <## if created then "Group link is created!" else "Group link:"
  cc <## ""
  link <- getTermLine cc
  cc <## ""
  cc <## ("Anybody can connect to you and join group as " <> B.unpack (strEncode mRole) <> " with: /c <group_link_above>")
  cc <## ("to show it again: /show link #" <> gName)
  cc <## ("to delete it: /delete link #" <> gName <> " (joined members will remain connected to you)")
  pure link

hasContactProfiles :: HasCallStack => TestCC -> [ContactName] -> Expectation
hasContactProfiles cc names =
  getContactProfiles cc >>= \ps -> ps `shouldMatchList` names

getContactProfiles :: TestCC -> IO [ContactName]
getContactProfiles cc = do
  user_ <- readTVarIO (currentUser $ chatController cc)
  case user_ of
    Nothing -> pure []
    Just user -> do
      profiles <- withTransaction (chatStore $ chatController cc) $ \db -> getUserContactProfiles db user
      pure $ map (\Profile {displayName} -> displayName) profiles

withCCUser :: TestCC -> (User -> IO a) -> IO a
withCCUser cc action = do
  user_ <- readTVarIO (currentUser $ chatController cc)
  case user_ of
    Nothing -> error "no user"
    Just user -> action user

withCCTransaction :: TestCC -> (DB.Connection -> IO a) -> IO a
withCCTransaction cc action =
  withTransaction (chatStore $ chatController cc) $ \db -> action db

getProfilePictureByName :: TestCC -> String -> IO (Maybe String)
getProfilePictureByName cc displayName =
  withTransaction (chatStore $ chatController cc) $ \db ->
    maybeFirstRow fromOnly $
      DB.query db "SELECT image FROM contact_profiles WHERE display_name = ? LIMIT 1" (Only displayName)

lastItemId :: HasCallStack => TestCC -> IO String
lastItemId cc = do
  cc ##> "/last_item_id"
  getTermLine cc

showActiveUser :: HasCallStack => TestCC -> String -> Expectation
showActiveUser cc name = do
  cc <## ("user profile: " <> name)
  cc <## "use /p <display name> to change it"
  cc <## "(the updated profile will be sent to all your contacts)"

connectUsers :: HasCallStack => TestCC -> TestCC -> IO ()
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
showName (TestCC ChatController {currentUser} _ _ _ _ _) = do
  Just User {localDisplayName, profile = LocalProfile {fullName}} <- readTVarIO currentUser
  pure . T.unpack $ localDisplayName <> optionalFullName localDisplayName fullName

createGroup2 :: HasCallStack => String -> TestCC -> TestCC -> IO ()
createGroup2 gName cc1 cc2 = createGroup2' gName cc1 cc2 True

createGroup2' :: HasCallStack => String -> TestCC -> TestCC -> Bool -> IO ()
createGroup2' gName cc1 cc2 doConnectUsers = do
  when doConnectUsers $ connectUsers cc1 cc2
  name2 <- userName cc2
  cc1 ##> ("/g " <> gName)
  cc1 <## ("group #" <> gName <> " is created")
  cc1 <## ("to add members use /a " <> gName <> " <name> or /create link #" <> gName)
  addMember gName cc1 cc2 GRAdmin
  cc2 ##> ("/j " <> gName)
  concurrently_
    (cc1 <## ("#" <> gName <> ": " <> name2 <> " joined the group"))
    (cc2 <## ("#" <> gName <> ": you joined the group"))

createGroup3 :: HasCallStack => String -> TestCC -> TestCC -> TestCC -> IO ()
createGroup3 gName cc1 cc2 cc3 = do
  createGroup2 gName cc1 cc2
  connectUsers cc1 cc3
  name1 <- userName cc1
  name3 <- userName cc3
  sName2 <- showName cc2
  sName3 <- showName cc3
  addMember gName cc1 cc3 GRAdmin
  cc3 ##> ("/j " <> gName)
  concurrentlyN_
    [ cc1 <## ("#" <> gName <> ": " <> name3 <> " joined the group"),
      do
        cc3 <## ("#" <> gName <> ": you joined the group")
        cc3 <## ("#" <> gName <> ": member " <> sName2 <> " is connected"),
      do
        cc2 <## ("#" <> gName <> ": " <> name1 <> " added " <> sName3 <> " to the group (connecting...)")
        cc2 <## ("#" <> gName <> ": new member " <> name3 <> " is connected")
    ]

create2Groups3 :: HasCallStack => String -> String -> TestCC -> TestCC -> TestCC -> IO ()
create2Groups3 gName1 gName2 cc1 cc2 cc3 = do
  createGroup3 gName1 cc1 cc2 cc3
  createGroup2' gName2 cc1 cc2 False
  name1 <- userName cc1
  name3 <- userName cc3
  addMember gName2 cc1 cc3 GRAdmin
  cc3 ##> ("/j " <> gName2)
  concurrentlyN_
    [ cc1 <## ("#" <> gName2 <> ": " <> name3 <> " joined the group"),
      do
        cc3 <## ("#" <> gName2 <> ": you joined the group")
        cc3 <##. ("#" <> gName2 <> ": member "), -- "#gName2: member sName2 is connected"
      do
        cc2 <##. ("#" <> gName2 <> ": " <> name1 <> " added ") -- "#gName2: name1 added sName3 to the group (connecting...)"
        cc2 <##. ("#" <> gName2 <> ": new member ") -- "#gName2: new member name3 is connected"
    ]

addMember :: HasCallStack => String -> TestCC -> TestCC -> GroupMemberRole -> IO ()
addMember gName = fullAddMember gName ""

fullAddMember :: HasCallStack => String -> String -> TestCC -> TestCC -> GroupMemberRole -> IO ()
fullAddMember gName fullName inviting invitee role = do
  name1 <- userName inviting
  memName <- userName invitee
  inviting ##> ("/a " <> gName <> " " <> memName <> " " <> B.unpack (strEncode role))
  let fullName' = if null fullName || fullName == gName then "" else " (" <> fullName <> ")"
  concurrentlyN_
    [ inviting <## ("invitation to join the group #" <> gName <> " sent to " <> memName),
      do
        invitee <## ("#" <> gName <> fullName' <> ": " <> name1 <> " invites you to join the group as " <> B.unpack (strEncode role))
        invitee <## ("use /j " <> gName <> " to accept")
    ]

checkActionDeletesFile :: HasCallStack => FilePath -> IO () -> IO ()
checkActionDeletesFile file action = do
  fileExistsBefore <- doesFileExist file
  fileExistsBefore `shouldBe` True
  action
  fileExistsAfter <- doesFileExist file
  fileExistsAfter `shouldBe` False

startFileTransferWithDest' :: HasCallStack => TestCC -> TestCC -> String -> String -> Maybe String -> IO ()
startFileTransferWithDest' cc1 cc2 fileName fileSize fileDest_ = do
  name1 <- userName cc1
  name2 <- userName cc2
  cc1 #> ("/f @" <> name2 <> " ./tests/fixtures/" <> fileName)
  cc1 <## "use /fc 1 to cancel sending"
  cc2 <# (name1 <> "> sends file " <> fileName <> " (" <> fileSize <> ")")
  cc2 <## "use /fr 1 [<dir>/ | <path>] to receive it"
  cc2 ##> ("/fr 1" <> maybe "" (" " <>) fileDest_)
  cc2 <## ("saving file 1 from " <> name1 <> " to " <> maybe id (</>) fileDest_ fileName)
  concurrently_
    (cc2 <## ("started receiving file 1 (" <> fileName <> ") from " <> name1))
    (cc1 <## ("started sending file 1 (" <> fileName <> ") to " <> name2))

currentChatVRangeInfo :: String
currentChatVRangeInfo =
  "peer chat protocol version range: " <> vRangeStr supportedChatVRange

vRangeStr :: VersionRange -> String
vRangeStr (VersionRange minVer maxVer) = "(" <> show minVer <> ", " <> show maxVer <> ")"

linkAnotherSchema :: String -> String
linkAnotherSchema link
  | "https://simplex.chat/" `isPrefixOf` link =
      T.unpack $ T.replace "https://simplex.chat/" "simplex:/" $ T.pack link
  | "simplex:/" `isPrefixOf` link =
      T.unpack $ T.replace "simplex:/" "https://simplex.chat/" $ T.pack link
  | otherwise = error "link starts with neither https://simplex.chat/ nor simplex:/"

xftpCLI :: [String] -> IO [String]
xftpCLI params = lines <$> capture_ (withArgs params xftpClientCLI)
