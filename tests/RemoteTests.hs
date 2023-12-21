{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RemoteTests where

import ChatClient
import ChatTests.Utils
import Control.Logger.Simple
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as M
import Simplex.Chat.Archive (archiveFilesFolder)
import Simplex.Chat.Controller (ChatConfig (..), XFTPFileConfig (..), versionNumber)
import qualified Simplex.Chat.Controller as Controller
import Simplex.Chat.Mobile.File
import Simplex.Chat.Remote.Types
import Simplex.Messaging.Crypto.File (CryptoFileArgs (..))
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.Directory

remoteTests :: SpecWith FilePath
remoteTests = describe "Remote" $ do
  describe "protocol handshake" $ do
    it "connects with new pairing (stops mobile)" $ remoteHandshakeTest False
    it "connects with new pairing (stops desktop)" $ remoteHandshakeTest True
    it "connects with stored pairing" remoteHandshakeStoredTest
    it "connects with multicast discovery" remoteHandshakeDiscoverTest
    it "refuses invalid client cert" remoteHandshakeRejectTest
    it "connects with stored server bindings" storedBindingsTest
  it "sends messages" remoteMessageTest
  describe "remote files" $ do
    it "store/get/send/receive files" remoteStoreFileTest
    it "should send files from CLI without /store" remoteCLIFileTest
  it "switches remote hosts" switchRemoteHostTest
  it "indicates remote hosts" indicateRemoteHostTest
  it "works with multiple profiles" multipleProfilesTest

-- * Chat commands

remoteHandshakeTest :: HasCallStack => Bool -> FilePath -> IO ()
remoteHandshakeTest viaDesktop = testChat2 aliceProfile aliceDesktopProfile $ \mobile desktop -> do
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"
  mobile ##> "/list remote ctrls"
  mobile <## "No remote controllers"

  startRemote mobile desktop

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <## "1. Mobile (connected)"

  mobile ##> "/list remote ctrls"
  mobile <## "Remote controllers:"
  mobile <## "1. My desktop (connected)"

  if viaDesktop then stopDesktop mobile desktop else stopMobile mobile desktop

  desktop ##> "/delete remote host 1"
  desktop <## "ok"
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"

  mobile ##> "/delete remote ctrl 1"
  mobile <## "ok"
  mobile ##> "/list remote ctrls"
  mobile <## "No remote controllers"

remoteHandshakeStoredTest :: HasCallStack => FilePath -> IO ()
remoteHandshakeStoredTest = testChat2 aliceProfile aliceDesktopProfile $ \mobile desktop -> do
  logNote "Starting new session"
  startRemote mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

  logNote "Starting stored session"
  startRemoteStored mobile desktop
  stopDesktop mobile desktop `catchAny` (logError . tshow)

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <## "1. Mobile"
  mobile ##> "/list remote ctrls"
  mobile <## "Remote controllers:"
  mobile <## "1. My desktop"

  logNote "Starting stored session again"
  startRemoteStored mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

remoteHandshakeDiscoverTest :: HasCallStack => FilePath -> IO ()
remoteHandshakeDiscoverTest = testChat2 aliceProfile aliceDesktopProfile $ \mobile desktop -> do
  logNote "Preparing new session"
  startRemote mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

  logNote "Starting stored session with multicast"
  startRemoteDiscover mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

remoteHandshakeRejectTest :: HasCallStack => FilePath -> IO ()
remoteHandshakeRejectTest = testChat3 aliceProfile aliceDesktopProfile bobProfile $ \mobile desktop mobileBob -> do
  logNote "Starting new session"
  startRemote mobile desktop
  stopMobile mobile desktop

  mobileBob ##> "/set device name MobileBob"
  mobileBob <## "ok"
  desktop ##> "/start remote host 1"
  desktop <##. "remote host 1 started on "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop
  mobileBob ##> ("/connect remote ctrl " <> inv)
  mobileBob <## ("connecting new remote controller: My desktop, v" <> versionNumber)
  mobileBob <## "remote controller stopped"

  -- the server remains active after rejecting invalid client
  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting remote controller 1: My desktop, v" <> versionNumber)
  desktop <## "remote host 1 connecting"
  desktop <## "Compare session code with host:"
  sessId <- getTermLine desktop
  mobile <## "remote controller 1 connected"
  mobile <## "Compare session code with controller and use:"
  mobile <## ("/verify remote ctrl " <> sessId)
  mobile ##> ("/verify remote ctrl " <> sessId)
  mobile <## "remote controller 1 session started with My desktop"
  desktop <## "remote host 1 connected"
  stopMobile mobile desktop

storedBindingsTest :: HasCallStack => FilePath -> IO ()
storedBindingsTest = testChat2 aliceProfile aliceDesktopProfile $ \mobile desktop -> do
  desktop ##> "/set device name My desktop"
  desktop <## "ok"
  mobile ##> "/set device name Mobile"
  mobile <## "ok"

  desktop ##> "/start remote host new addr=127.0.0.1 iface=lo port=52230"
  desktop <##. "new remote host started on 127.0.0.1:52230" -- TODO: show ip?
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop

  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting new remote controller: My desktop, v" <> versionNumber)
  desktop <## "new remote host connecting"
  mobile <## "new remote controller connected"
  verifyRemoteCtrl mobile desktop
  mobile <## "remote controller 1 session started with My desktop"
  desktop <## "new remote host 1 added: Mobile"
  desktop <## "remote host 1 connected"

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <##. "1. Mobile (connected) ["
  stopDesktop mobile desktop
  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <##. "1. Mobile ["

-- TODO: more parser tests

remoteMessageTest :: HasCallStack => FilePath -> IO ()
remoteMessageTest = testChat3 aliceProfile aliceDesktopProfile bobProfile $ \mobile desktop bob -> do
  startRemote mobile desktop
  contactBob desktop bob

  logNote "sending messages"
  desktop #> "@bob hello there 🙂"
  bob <# "alice> hello there 🙂"
  bob #> "@alice hi"
  desktop <# "bob> hi"

  logNote "post-remote checks"
  stopMobile mobile desktop

  mobile ##> "/contacts"
  mobile <## "bob (Bob)"

  bob ##> "/contacts"
  bob <## "alice (Alice)"

  desktop ##> "/contacts"
  -- empty contact list on desktop-local

  threadDelay 1000000
  logNote "done"

remoteStoreFileTest :: HasCallStack => FilePath -> IO ()
remoteStoreFileTest =
  testChatCfg3 cfg aliceProfile aliceDesktopProfile bobProfile $ \mobile desktop bob ->
    withXFTPServer $ do
      let mobileFiles = "./tests/tmp/mobile_files"
      mobile ##> ("/_files_folder " <> mobileFiles)
      mobile <## "ok"
      let desktopFiles = "./tests/tmp/desktop_files"
      desktop ##> ("/_files_folder " <> desktopFiles)
      desktop <## "ok"
      let desktopHostFiles = "./tests/tmp/remote_hosts_data"
      desktop ##> ("/remote_hosts_folder " <> desktopHostFiles)
      desktop <## "ok"
      let bobFiles = "./tests/tmp/bob_files"
      bob ##> ("/_files_folder " <> bobFiles)
      bob <## "ok"

      startRemote mobile desktop
      contactBob desktop bob

      rhs <- readTVarIO (Controller.remoteHostSessions $ chatController desktop)
      desktopHostStore <- case M.lookup (RHId 1) rhs of
        Just (_, RHSessionConnected {storePath}) -> pure $ desktopHostFiles </> storePath </> archiveFilesFolder
        _ -> fail "Host session 1 should be started"
      desktop ##> "/store remote file 1 tests/fixtures/test.pdf"
      desktop <## "file test.pdf stored on remote host 1"
      src <- B.readFile "tests/fixtures/test.pdf"
      B.readFile (mobileFiles </> "test.pdf") `shouldReturn` src
      B.readFile (desktopHostStore </> "test.pdf") `shouldReturn` src
      desktop ##> "/store remote file 1 tests/fixtures/test.pdf"
      desktop <## "file test_1.pdf stored on remote host 1"
      B.readFile (mobileFiles </> "test_1.pdf") `shouldReturn` src
      B.readFile (desktopHostStore </> "test_1.pdf") `shouldReturn` src
      desktop ##> "/store remote file 1 encrypt=on tests/fixtures/test.pdf"
      desktop <## "file test_2.pdf stored on remote host 1"
      Just cfArgs@(CFArgs key nonce) <- J.decode . LB.pack <$> getTermLine desktop
      chatReadFile (mobileFiles </> "test_2.pdf") (strEncode key) (strEncode nonce) `shouldReturn` Right (LB.fromStrict src)
      chatReadFile (desktopHostStore </> "test_2.pdf") (strEncode key) (strEncode nonce) `shouldReturn` Right (LB.fromStrict src)

      removeFile (desktopHostStore </> "test_1.pdf")
      removeFile (desktopHostStore </> "test_2.pdf")

      -- cannot get file before it is used
      desktop ##> "/get remote file 1 {\"userId\": 1, \"fileId\": 1, \"sent\": true, \"fileSource\": {\"filePath\": \"test_1.pdf\"}}"
      hostError desktop "SEFileNotFound"
      -- send file not encrypted locally on mobile host
      desktop ##> "/_send @2 json {\"filePath\": \"test_1.pdf\", \"msgContent\": {\"type\": \"file\", \"text\": \"sending a file\"}}"
      desktop <# "@bob sending a file"
      desktop <# "/f @bob test_1.pdf"
      desktop <## "use /fc 1 to cancel sending"
      bob <# "alice> sending a file"
      bob <# "alice> sends file test_1.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 1 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 1"
      concurrentlyN_
        [ do
            desktop <## "completed uploading file 1 (test_1.pdf) for bob",
          do
            bob <## "saving file 1 from alice to test_1.pdf"
            bob <## "started receiving file 1 (test_1.pdf) from alice"
            bob <## "completed receiving file 1 (test_1.pdf) from alice"
        ]
      B.readFile (bobFiles </> "test_1.pdf") `shouldReturn` src
      -- returns error for inactive user
      desktop ##> "/get remote file 1 {\"userId\": 2, \"fileId\": 1, \"sent\": true, \"fileSource\": {\"filePath\": \"test_1.pdf\"}}"
      hostError desktop "CEDifferentActiveUser"
      -- returns error with incorrect file ID
      desktop ##> "/get remote file 1 {\"userId\": 1, \"fileId\": 2, \"sent\": true, \"fileSource\": {\"filePath\": \"test_1.pdf\"}}"
      hostError desktop "SEFileNotFound"
      -- gets file
      doesFileExist (desktopHostStore </> "test_1.pdf") `shouldReturn` False
      desktop ##> "/get remote file 1 {\"userId\": 1, \"fileId\": 1, \"sent\": true, \"fileSource\": {\"filePath\": \"test_1.pdf\"}}"
      desktop <## "ok"
      B.readFile (desktopHostStore </> "test_1.pdf") `shouldReturn` src

      -- send file encrypted locally on mobile host
      desktop ##> ("/_send @2 json {\"fileSource\": {\"filePath\":\"test_2.pdf\", \"cryptoArgs\": " <> LB.unpack (J.encode cfArgs) <> "}, \"msgContent\": {\"type\": \"file\", \"text\": \"\"}}")
      desktop <# "/f @bob test_2.pdf"
      desktop <## "use /fc 2 to cancel sending"
      bob <# "alice> sends file test_2.pdf (266.0 KiB / 272376 bytes)"
      bob <## "use /fr 2 [<dir>/ | <path>] to receive it"
      bob ##> "/fr 2"
      concurrentlyN_
        [ do
            desktop <## "completed uploading file 2 (test_2.pdf) for bob",
          do
            bob <## "saving file 2 from alice to test_2.pdf"
            bob <## "started receiving file 2 (test_2.pdf) from alice"
            bob <## "completed receiving file 2 (test_2.pdf) from alice"
        ]
      B.readFile (bobFiles </> "test_2.pdf") `shouldReturn` src

      -- receive file via remote host
      copyFile "./tests/fixtures/test.jpg" (bobFiles </> "test.jpg")
      bob #> "/f @alice test.jpg"
      bob <## "use /fc 3 to cancel sending"
      desktop <# "bob> sends file test.jpg (136.5 KiB / 139737 bytes)"
      desktop <## "use /fr 3 [<dir>/ | <path>] to receive it"
      desktop ##> "/fr 3 encrypt=on"
      concurrentlyN_
        [ do
            bob <## "completed uploading file 3 (test.jpg) for alice",
          do
            desktop <## "saving file 3 from bob to test.jpg"
            desktop <## "started receiving file 3 (test.jpg) from bob"
            desktop <## "completed receiving file 3 (test.jpg) from bob"
        ]
      Just cfArgs'@(CFArgs key' nonce') <- J.decode . LB.pack <$> getTermLine desktop
      desktop <## "File received to connected remote host 1"
      desktop <## "To download to this device use:"
      getCmd <- getTermLine desktop
      getCmd `shouldBe` ("/get remote file 1 {\"userId\":1,\"fileId\":3,\"sent\":false,\"fileSource\":{\"filePath\":\"test.jpg\",\"cryptoArgs\":" <> LB.unpack (J.encode cfArgs') <> "}}")
      src' <- B.readFile (bobFiles </> "test.jpg")
      chatReadFile (mobileFiles </> "test.jpg") (strEncode key') (strEncode nonce') `shouldReturn` Right (LB.fromStrict src')
      doesFileExist (desktopHostStore </> "test.jpg") `shouldReturn` False
      -- returns error with incorrect key
      desktop ##> "/get remote file 1 {\"userId\": 1, \"fileId\": 3, \"sent\": false, \"fileSource\": {\"filePath\": \"test.jpg\", \"cryptoArgs\": null}}"
      hostError desktop "SEFileNotFound"
      doesFileExist (desktopHostStore </> "test.jpg") `shouldReturn` False
      desktop ##> getCmd
      desktop <## "ok"
      chatReadFile (desktopHostStore </> "test.jpg") (strEncode key') (strEncode nonce') `shouldReturn` Right (LB.fromStrict src')

      stopMobile mobile desktop
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp/tmp"}
    hostError cc err = do
      r <- getTermLine cc
      r `shouldStartWith` "remote host 1 error"
      r `shouldContain` err

remoteCLIFileTest :: HasCallStack => FilePath -> IO ()
remoteCLIFileTest = testChatCfg3 cfg aliceProfile aliceDesktopProfile bobProfile $ \mobile desktop bob -> withXFTPServer $ do
  createDirectoryIfMissing True "./tests/tmp/tmp/"
  let mobileFiles = "./tests/tmp/mobile_files"
  mobile ##> ("/_files_folder " <> mobileFiles)
  mobile <## "ok"
  let bobFiles = "./tests/tmp/bob_files/"
  createDirectoryIfMissing True bobFiles
  let desktopHostFiles = "./tests/tmp/remote_hosts_data"
  desktop ##> ("/remote_hosts_folder " <> desktopHostFiles)
  desktop <## "ok"

  startRemote mobile desktop
  contactBob desktop bob

  rhs <- readTVarIO (Controller.remoteHostSessions $ chatController desktop)
  desktopHostStore <- case M.lookup (RHId 1) rhs of
    Just (_, RHSessionConnected {storePath}) -> pure $ desktopHostFiles </> storePath </> archiveFilesFolder
    _ -> fail "Host session 1 should be started"

  mobileName <- userName mobile

  bob #> ("/f @" <> mobileName <> " " <> "tests/fixtures/test.pdf")
  bob <## "use /fc 1 to cancel sending"

  desktop <# "bob> sends file test.pdf (266.0 KiB / 272376 bytes)"
  desktop <## "use /fr 1 [<dir>/ | <path>] to receive it"
  desktop ##> "/fr 1"
  concurrentlyN_
    [ do
        bob <## "completed uploading file 1 (test.pdf) for alice",
      do
        desktop <## "saving file 1 from bob to test.pdf"
        desktop <## "started receiving file 1 (test.pdf) from bob"
        desktop <## "completed receiving file 1 (test.pdf) from bob"
    ]

  desktop <## "File received to connected remote host 1"
  desktop <## "To download to this device use:"
  getCmd <- getTermLine desktop
  src <- B.readFile "tests/fixtures/test.pdf"
  B.readFile (mobileFiles </> "test.pdf") `shouldReturn` src
  doesFileExist (desktopHostStore </> "test.pdf") `shouldReturn` False
  desktop ##> getCmd
  desktop <## "ok"
  B.readFile (desktopHostStore </> "test.pdf") `shouldReturn` src

  desktop `send` "/f @bob tests/fixtures/test.jpg"
  desktop <# "/f @bob test.jpg"
  desktop <## "use /fc 2 to cancel sending"

  bob <# "alice> sends file test.jpg (136.5 KiB / 139737 bytes)"
  bob <## "use /fr 2 [<dir>/ | <path>] to receive it"
  bob ##> ("/fr 2 " <> bobFiles)
  concurrentlyN_
    [ do
        desktop <## "completed uploading file 2 (test.jpg) for bob",
      do
        bob <## "saving file 2 from alice to ./tests/tmp/bob_files/test.jpg"
        bob <## "started receiving file 2 (test.jpg) from alice"
        bob <## "completed receiving file 2 (test.jpg) from alice"
    ]

  src' <- B.readFile "tests/fixtures/test.jpg"
  B.readFile (mobileFiles </> "test.jpg") `shouldReturn` src'
  B.readFile (desktopHostStore </> "test.jpg") `shouldReturn` src'
  B.readFile (bobFiles </> "test.jpg") `shouldReturn` src'

  stopMobile mobile desktop
  where
    cfg = testCfg {xftpFileConfig = Just $ XFTPFileConfig {minFileSize = 0}, tempDir = Just "./tests/tmp/tmp"}

switchRemoteHostTest :: FilePath -> IO ()
switchRemoteHostTest = testChat3 aliceProfile aliceDesktopProfile bobProfile $ \mobile desktop bob -> do
  startRemote mobile desktop
  contactBob desktop bob

  desktop ##> "/contacts"
  desktop <## "bob (Bob)"

  desktop ##> "/switch remote host local"
  desktop <## "Using local profile"
  desktop ##> "/contacts"

  desktop ##> "/switch remote host 1"
  desktop <## "Using remote host 1 (Mobile)"
  desktop ##> "/contacts"
  desktop <## "bob (Bob)"

  desktop ##> "/switch remote host 123"
  desktop <## "no remote host 123"

  stopDesktop mobile desktop
  desktop ##> "/contacts"
  desktop ##> "/switch remote host 1"
  desktop <## "remote host 1 error: RHEInactive"
  desktop ##> "/contacts"

indicateRemoteHostTest :: FilePath -> IO ()
indicateRemoteHostTest = testChat4 aliceProfile aliceDesktopProfile bobProfile cathProfile $ \mobile desktop bob cath -> do
  connectUsers desktop cath
  startRemote mobile desktop
  contactBob desktop bob
  -- remote contact -> remote host
  bob #> "@alice hi"
  desktop <#. "bob> hi"
  -- local -> remote
  cath #> "@alice_desktop hello"
  (desktop, "[local] ") ^<# "cath> hello"
  -- local -> local
  desktop ##> "/switch remote host local"
  desktop <## "Using local profile"
  desktop <##> cath
  -- local -> remote
  bob #> "@alice what's up?"
  (desktop, "[remote: 1] ") ^<# "bob> what's up?"

  -- local -> local after disconnect
  stopDesktop mobile desktop
  desktop <##> cath
  cath <##> desktop

multipleProfilesTest :: FilePath -> IO ()
multipleProfilesTest = testChat4 aliceProfile aliceDesktopProfile bobProfile cathProfile $ \mobile desktop bob cath -> do
  connectUsers desktop cath

  desktop ##> "/create user desk_bottom"
  desktop <## "user profile: desk_bottom"
  desktop <## "use /p <display name> to change it"
  desktop <## "(the updated profile will be sent to all your contacts)"
  desktop ##> "/users"
  desktop <## "alice_desktop (Alice Desktop)"
  desktop <## "desk_bottom (active)"

  startRemote mobile desktop
  contactBob desktop bob
  desktop ##> "/users"
  desktop <## "alice (Alice) (active)"

  desktop ##> "/create user alt_alice"
  desktop <## "user profile: alt_alice"
  desktop <## "use /p <display name> to change it"
  desktop <## "(the updated profile will be sent to all your contacts)"

  desktop ##> "/users"
  desktop <## "alice (Alice)"
  desktop <## "alt_alice (active)"

  desktop ##> "/user"
  desktop <## "user profile: alt_alice"
  desktop <## "use /p <display name> to change it"
  desktop <## "(the updated profile will be sent to all your contacts)"

  bob #> "@alice hi"
  (desktop, "[user: alice] ") ^<# "bob> hi"

  cath #> "@alice_desktop hello"
  (desktop, "[local, user: alice_desktop] ") ^<# "cath> hello"

  desktop ##> "/switch remote host local"
  desktop <## "Using local profile"
  desktop ##> "/user"
  desktop <## "user profile: desk_bottom"
  desktop <## "use /p <display name> to change it"
  desktop <## "(the updated profile will be sent to all your contacts)"

  bob #> "@alice hey"
  (desktop, "[remote: 1, user: alice] ") ^<# "bob> hey"

  stopDesktop mobile desktop

-- * Utils

startRemote :: TestCC -> TestCC -> IO ()
startRemote mobile desktop = do
  desktop ##> "/set device name My desktop"
  desktop <## "ok"
  mobile ##> "/set device name Mobile"
  mobile <## "ok"
  desktop ##> "/start remote host new"
  desktop <##. "new remote host started on "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop
  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting new remote controller: My desktop, v" <> versionNumber)
  desktop <## "new remote host connecting"
  mobile <## "new remote controller connected"
  verifyRemoteCtrl mobile desktop
  mobile <## "remote controller 1 session started with My desktop"
  desktop <## "new remote host 1 added: Mobile"
  desktop <## "remote host 1 connected"

startRemoteStored :: TestCC -> TestCC -> IO ()
startRemoteStored mobile desktop = do
  desktop ##> "/start remote host 1"
  desktop <##. "remote host 1 started on "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop
  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting remote controller 1: My desktop, v" <> versionNumber)
  desktop <## "remote host 1 connecting"
  mobile <## "remote controller 1 connected"
  verifyRemoteCtrl mobile desktop
  mobile <## "remote controller 1 session started with My desktop"
  desktop <## "remote host 1 connected"

startRemoteDiscover :: TestCC -> TestCC -> IO ()
startRemoteDiscover mobile desktop = do
  desktop ##> "/start remote host 1 multicast=on"
  desktop <##. "remote host 1 started on "
  desktop <## "Remote session invitation:"
  _inv <- getTermLine desktop -- will use multicast instead
  mobile ##> "/find remote ctrl"
  mobile <## "ok"
  mobile <## ("remote controller 1 found: My desktop, v" <> versionNumber)
  mobile <## "use /confirm remote ctrl 1 to connect"
  mobile ##> "/confirm remote ctrl 1"

  mobile <## ("connecting remote controller 1: My desktop, v" <> versionNumber)
  desktop <## "remote host 1 connecting"
  mobile <## "remote controller 1 connected"
  verifyRemoteCtrl mobile desktop
  mobile <## "remote controller 1 session started with My desktop"
  desktop <## "remote host 1 connected"

verifyRemoteCtrl :: TestCC -> TestCC -> IO ()
verifyRemoteCtrl mobile desktop = do
  desktop <## "Compare session code with host:"
  sessId <- getTermLine desktop
  mobile <## "Compare session code with controller and use:"
  mobile <## ("/verify remote ctrl " <> sessId)
  mobile ##> ("/verify remote ctrl " <> sessId)

contactBob :: TestCC -> TestCC -> IO ()
contactBob desktop bob = do
  logNote "exchanging contacts"
  bob ##> "/c"
  inv' <- getInvitation bob
  desktop ##> ("/c " <> inv')
  desktop <## "confirmation sent!"
  concurrently_
    (desktop <## "bob (Bob): contact is connected")
    (bob <## "alice (Alice): contact is connected")

stopDesktop :: HasCallStack => TestCC -> TestCC -> IO ()
stopDesktop mobile desktop = do
  logWarn "stopping via desktop"
  desktop ##> "/stop remote host 1"
  desktop <## "ok"
  eventually 3 $ mobile <## "remote controller stopped"

stopMobile :: HasCallStack => TestCC -> TestCC -> IO ()
stopMobile mobile desktop = do
  logWarn "stopping via mobile"
  mobile ##> "/stop remote ctrl"
  mobile <## "ok"
  eventually 3 $ desktop <## "remote host 1 stopped"

-- | Run action with extended timeout
eventually :: Int -> IO a -> IO a
eventually retries action =
  tryAny action >>= \case
    -- TODO: only catch timeouts
    Left err | retries == 0 -> throwIO err
    Left _ -> eventually (retries - 1) action
    Right r -> pure r
