{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module RemoteTests where

import ChatClient
import ChatTests.Utils
import Control.Logger.Simple
import Control.Monad.Except (runExceptT)
import Crypto.Random (drgNew)
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Network.TLS as TLS
import Simplex.Chat.Archive (archiveFilesFolder)
import Simplex.Chat.Controller (ChatConfig (..), XFTPFileConfig (..))
import qualified Simplex.Chat.Controller as Controller
import Simplex.Chat.Mobile.File
import Simplex.Chat.Remote.Types
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFileArgs (..))
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Util
import Simplex.RemoteControl.Client
import qualified Simplex.RemoteControl.Client as RC
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.Directory
import Data.Bifunctor (first)

remoteTests :: SpecWith FilePath
remoteTests = describe "Remote" $ do
  -- it "generates usable credentials" genCredentialsTest
  -- it "OOB encoding, decoding, and signatures are correct" oobCodecTest
  -- it "connects announcer with discoverer over reverse-http2" announceDiscoverHttp2Test
  fit "RemoteControl TLS Hello works" rcTLSTest
  it "performs protocol handshake" remoteHandshakeTest
  it "performs protocol handshake (again)" remoteHandshakeTest -- leaking servers regression check
  it "sends messages" remoteMessageTest
  describe "remote files" $ do
    it "store/get/send/receive files" remoteStoreFileTest
    it "should sends files from CLI wihtout /store" remoteCLIFileTest

-- * Low-level TLS with ephemeral credentials

rcTLSTest :: HasCallStack => FilePath -> IO ()
rcTLSTest _tmp = do
  drg <- drgNew >>= newTVarIO
  hp <- RC.newRCHostPairing
  invVar <- newEmptyMVar
  ctrl <- async . runExceptT $ do
    logNote "c 1"
    (inv, hc, var) <- RC.connectRCHost drg hp (J.String "app")
    logNote "c 2"
    putMVar invVar (inv, hc)
    logNote "c 3"
    (rcHostSession, rcHelloBody, hp') <- atomically $ takeTMVar var
    logNote "c 4"
    threadDelay 1000000
    logNote $ tshow rcHelloBody
    logNote "ctrl: ciao"
    liftIO $ RC.cancelHostClient hc

  (inv, hc) <- takeMVar invVar
  -- logNote $ decodeUtf8 $ strEncode inv

  host <- async . runExceptT $ do
    logNote "h 1"
    (rcCtrlClient, var) <- RC.connectRCCtrlURI drg inv Nothing (J.String "app")
    logNote "h 2"
    (rcCtrlSession, rcCtrlPairing) <- atomically $ takeTMVar var
    logNote "h 3"
    liftIO $ RC.confirmCtrlSession rcCtrlClient True
    logNote "h 4"
    threadDelay 1000000
    logNote "ctrl: adios"

  timeout 10000000 (waitCatch ctrl) >>= \case
    Just (Right (Right ())) -> pure ()
    err -> fail $ "Unexpected controller result: " <> show err

  waitCatch hc.action >>= \case
    Left err -> fromException err `shouldBe` Just AsyncCancelled
    Right () -> fail "Unexpected controller finish"

  timeout 10000000 (waitCatch host) >>= \case
    Just (Right (Right ())) -> pure ()
    err -> fail $ "Unexpected host result: " <> show err

-- -- XXX: extract
-- genCredentialsTest :: (HasCallStack) => FilePath -> IO ()
-- genCredentialsTest _tmp = do
--   (fingerprint, credentials) <- genTestCredentials
--   started <- newEmptyTMVarIO
--   bracket (startTLSServer started credentials serverHandler) cancel $ \_server -> do
--     ok <- atomically (readTMVar started)
--     port <- maybe (error "TLS server failed to start") pure ok
--     logNote $ "Assigned port: " <> tshow port
--     connectTLSClient ("127.0.0.1", fromIntegral port) fingerprint clientHandler
--   where
--     serverHandler serverTls = do
--       logNote "Sending from server"
--       Transport.putLn serverTls "hi client"
--       logNote "Reading from server"
--       Transport.getLn serverTls `shouldReturn` "hi server"
--     clientHandler clientTls = do
--       logNote "Sending from client"
--       Transport.putLn clientTls "hi server"
--       logNote "Reading from client"
--       Transport.getLn clientTls `shouldReturn` "hi client"

-- * UDP discovery and rever HTTP2

-- oobCodecTest :: (HasCallStack) => FilePath -> IO ()
-- oobCodecTest _tmp = do
--   subscribers <- newTMVarIO 0
--   localAddr <- Discovery.getLocalAddress subscribers >>= maybe (fail "unable to get local address") pure
--   (fingerprint, _credentials) <- genTestCredentials
--   (_dhKey, _sigKey, _ann, signedOOB@(SignedOOB oob _sig)) <- Discovery.startSession (Just "Desktop") (localAddr, read Discovery.DISCOVERY_PORT) fingerprint
--   verifySignedOOB signedOOB `shouldBe` True
--   strDecode (strEncode oob) `shouldBe` Right oob
--   strDecode (strEncode signedOOB) `shouldBe` Right signedOOB

-- announceDiscoverHttp2Test :: (HasCallStack) => FilePath -> IO ()
-- announceDiscoverHttp2Test _tmp = do
--   subscribers <- newTMVarIO 0
--   localAddr <- Discovery.getLocalAddressMulticast subscribers >>= maybe (fail "unable to get local address") pure
--   (fingerprint, credentials) <- genTestCredentials
--   started <- newEmptyTMVarIO
--   (_dhKey, sigKey, ann, _oob) <- Discovery.startSession (Just "Desktop") (localAddr, 0) fingerprint
--   tasks <- newTVarIO []
--   finished <- newEmptyMVar
--   controller <- async $ do
--     logNote "Controller: starting"
--     bracket
--       (announceRevHTTP2 tasks started (sigKey, ann) credentials (putMVar finished ()) >>= either (fail . show) pure)
--       closeHTTP2Client
--       ( \http -> do
--           logNote "Controller: got client"
--           sendRequest http (C.requestNoBody "GET" "/" []) (Just 10000000) >>= \case
--             Left err -> do
--               logNote "Controller: got error"
--               fail $ show err
--             Right HTTP2Response {} ->
--               logNote "Controller: got response"
--       )
--   host <- async $ Discovery.withListener subscribers $ \sock -> do
--     (N.SockAddrInet _port addr, sigAnn) <- Discovery.recvAnnounce sock
--     SignedAnnounce Announce {caFingerprint, serviceAddress=(hostAddr, port)} _sig <- either fail pure $ smpDecode sigAnn
--     caFingerprint `shouldBe` fingerprint
--     addr `shouldBe` hostAddr
--     let service = (THIPv4 $ N.hostAddressToTuple hostAddr, port)
--     logNote $ "Host: connecting to " <> tshow service
--     server <- async $ Discovery.connectTLSClient service fingerprint $ \tls -> do
--       logNote "Host: got tls"
--       flip attachHTTP2Server tls $ \HTTP2Request {sendResponse} -> do
--         logNote "Host: got request"
--         sendResponse $ S.responseNoBody ok200 []
--         logNote "Host: sent response"
--     takeMVar finished `finally` cancel server
--     logNote "Host: finished"
--   tasks `registerAsync` controller
--   tasks `registerAsync` host
--   (waitBoth host controller `shouldReturn` ((), ())) `finally` cancelTasks tasks

-- * Chat commands

remoteHandshakeTest :: HasCallStack => FilePath -> IO ()
remoteHandshakeTest = testChat2 aliceProfile bobProfile $ \desktop mobile -> do
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"

  startRemote mobile desktop

  logNote "Session active"

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <## "1.  (active)"
  mobile ##> "/list remote ctrls"
  mobile <## "Remote controllers:"
  mobile <## "1. My desktop (active)"

  stopMobile mobile desktop `catchAny` (logError . tshow)
  -- TODO: add a case for 'stopDesktop'

  desktop ##> "/delete remote host 1"
  desktop <## "ok"
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"

  mobile ##> "/delete remote ctrl 1"
  mobile <## "ok"
  mobile ##> "/list remote ctrls"
  mobile <## "No remote controllers"

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
      desktopHostStore <- case M.lookup 1 rhs of
        Just RemoteHostSession {storePath} -> pure $ desktopHostFiles </> storePath </> archiveFilesFolder
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
  desktopHostStore <- case M.lookup 1 rhs of
    Just RemoteHostSession {storePath} -> pure $ desktopHostFiles </> storePath </> archiveFilesFolder
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

-- * Utils

startRemote :: TestCC -> TestCC -> IO ()
startRemote mobile desktop = do
  desktop ##> "/set device name My desktop"
  desktop <## "ok"
  desktop ##> "/create remote host"
  desktop <## "remote host 1 created"
  -- A new host is started [automatically] by UI
  desktop ##> "/start remote host 1"
  desktop <## "ok"
  desktop <## "remote host 1 started"
  desktop <## "connection code:"
  oobLink <- getTermLine desktop
  -- OOB {caFingerprint = oobFingerprint} <- either (fail . mappend "OOB link failed: ") pure $ decodeOOBLink (fromString oobLink)
  -- Desktop displays OOB QR code

  mobile ##> "/set device name Mobile"
  mobile <## "ok"
  -- mobile ##> "/find remote ctrl"
  -- mobile <## "ok"
  -- mobile <## "remote controller announced"
  -- mobile <## "connection code:"
  -- annFingerprint <- getTermLine mobile
  -- -- The user scans OOB QR code and confirms it matches the announced stuff
  -- fromString annFingerprint `shouldBe` strEncode oobFingerprint

  mobile ##> ("/connect remote ctrl " <> oobLink)
  -- mobile <## "remote controller 1 registered"
  mobile <## "ok"
  -- mobile ##> "/confirm remote ctrl 1"
  -- mobile <## "ok"
  mobile <## "remote controller 1 connecting to My desktop"

  mobile <## "remote controller 1 connected to My desktop"
  mobile <## "Compare session code with controller and use:"
  verifyCmd <- getTermLine mobile
  mobile ##> verifyCmd
  mobile <## "ok"
  concurrently_
    (mobile <## "remote controller 1 session started with My desktop")
    (desktop <## "remote host 1 connected")

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

genTestCredentials :: IO (C.KeyHash, TLS.Credentials)
genTestCredentials = do
  caCreds <- liftIO $ genCredentials Nothing (0, 24) "CA"
  sessionCreds <- liftIO $ genCredentials (Just caCreds) (0, 24) "Session"
  pure . tlsCredentials $ sessionCreds :| [caCreds]

stopDesktop :: HasCallStack => TestCC -> TestCC -> IO ()
stopDesktop mobile desktop = do
  logWarn "stopping via desktop"
  desktop ##> "/stop remote host 1"
  -- desktop <## "ok"
  concurrently_
    (desktop <## "remote host 1 stopped")
    (eventually 3 $ mobile <## "remote controller stopped")

stopMobile :: HasCallStack => TestCC -> TestCC -> IO ()
stopMobile mobile desktop = do
  logWarn "stopping via mobile"
  mobile ##> "/stop remote ctrl"
  mobile <## "ok"
  concurrently_
    (mobile <## "remote controller stopped")
    (eventually 3 $ desktop <## "remote host 1 stopped")

-- | Run action with extended timeout
eventually :: Int -> IO a -> IO a
eventually retries action =
  tryAny action >>= \case
    -- TODO: only catch timeouts
    Left err | retries == 0 -> throwIO err
    Left _ -> eventually (retries - 1) action
    Right r -> pure r
