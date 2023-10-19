{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RemoteTests where

import ChatClient
import ChatTests.Utils
import Control.Logger.Simple
import Control.Monad
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Network.HTTP.Types (ok200)
import qualified Network.HTTP2.Client as C
import qualified Network.HTTP2.Server as S
import qualified Network.Socket as N
import qualified Network.TLS as TLS
import qualified Simplex.Chat.Controller as Controller
import Simplex.Chat.Remote.Types
import qualified Simplex.Chat.Remote.Discovery as Discovery
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import qualified Simplex.Messaging.Transport as Transport
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Response (..), closeHTTP2Client, sendRequest)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Simplex.Messaging.Util
import System.FilePath (makeRelative, (</>))
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.Directory

remoteTests :: SpecWith FilePath
remoteTests = fdescribe "Remote" $ do
  it "generates usable credentials" genCredentialsTest
  it "connects announcer with discoverer over reverse-http2" announceDiscoverHttp2Test
  it "performs protocol handshake" remoteHandshakeTest
  it "performs protocol handshake (again)" remoteHandshakeTest -- leaking servers regression check
  it "sends messages" remoteMessageTest
  xit "sends files" remoteFileTest

-- * Low-level TLS with ephemeral credentials

genCredentialsTest :: (HasCallStack) => FilePath -> IO ()
genCredentialsTest _tmp = do
  (fingerprint, credentials) <- genTestCredentials
  started <- newEmptyTMVarIO
  bracket (Discovery.startTLSServer started credentials serverHandler) cancel $ \_server -> do
    ok <- atomically (readTMVar started)
    unless ok $ error "TLS server failed to start"
    Discovery.connectTLSClient "127.0.0.1" fingerprint clientHandler
  where
    serverHandler serverTls = do
      logNote "Sending from server"
      Transport.putLn serverTls "hi client"
      logNote "Reading from server"
      Transport.getLn serverTls `shouldReturn` "hi server"
    clientHandler clientTls = do
      logNote "Sending from client"
      Transport.putLn clientTls "hi server"
      logNote "Reading from client"
      Transport.getLn clientTls `shouldReturn` "hi client"

-- * UDP discovery and rever HTTP2

announceDiscoverHttp2Test :: (HasCallStack) => FilePath -> IO ()
announceDiscoverHttp2Test _tmp = do
  (fingerprint, credentials) <- genTestCredentials
  tasks <- newTVarIO []
  finished <- newEmptyMVar
  controller <- async $ do
    logNote "Controller: starting"
    bracket
      (Discovery.announceRevHTTP2 tasks fingerprint credentials (putMVar finished ()) >>= either (fail . show) pure)
      closeHTTP2Client
      ( \http -> do
          logNote "Controller: got client"
          sendRequest http (C.requestNoBody "GET" "/" []) (Just 10000000) >>= \case
            Left err -> do
              logNote "Controller: got error"
              fail $ show err
            Right HTTP2Response {} ->
              logNote "Controller: got response"
      )
  host <- async $ Discovery.withListener $ \sock -> do
    (N.SockAddrInet _port addr, invite) <- Discovery.recvAnnounce sock
    strDecode invite `shouldBe` Right fingerprint
    logNote "Host: connecting"
    server <- async $ Discovery.connectTLSClient (THIPv4 $ N.hostAddressToTuple addr) fingerprint $ \tls -> do
      logNote "Host: got tls"
      flip Discovery.attachHTTP2Server tls $ \HTTP2Request {sendResponse} -> do
        logNote "Host: got request"
        sendResponse $ S.responseNoBody ok200 []
        logNote "Host: sent response"
    takeMVar finished `finally` cancel server
    logNote "Host: finished"
  tasks `registerAsync` controller
  tasks `registerAsync` host
  (waitBoth host controller `shouldReturn` ((), ())) `finally` cancelTasks tasks

-- * Chat commands

remoteHandshakeTest :: (HasCallStack) => FilePath -> IO ()
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

remoteMessageTest :: (HasCallStack) => FilePath -> IO ()
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

remoteFileTest :: (HasCallStack) => FilePath -> IO ()
remoteFileTest = testChat3 aliceProfile aliceDesktopProfile bobProfile $ \mobile desktop bob -> do
  let mobileFiles = "./tests/tmp/mobile_files"
  mobile ##> ("/_files_folder " <> mobileFiles)
  mobile <## "ok"
  let desktopFiles = "./tests/tmp/desktop_files"
  desktop ##> ("/_files_folder " <> desktopFiles)
  desktop <## "ok"
  let bobFiles = "./tests/tmp/bob_files"
  bob ##> ("/_files_folder " <> bobFiles)
  bob <## "ok"

  startRemote mobile desktop
  contactBob desktop bob

  rhs <- readTVarIO (Controller.remoteHostSessions $ chatController desktop)
  desktopStore <- case M.lookup 1 rhs of
    Just RemoteHostSessionStarted {remoteHostClient, storePath} -> pure storePath
    _ -> fail "Host session 1 should be started"

  doesFileExist "./tests/tmp/mobile_files/test.pdf" `shouldReturn` False
  doesFileExist (desktopFiles </> desktopStore </> "test.pdf") `shouldReturn` False
  mobileName <- userName mobile

  bobsFile <- makeRelative bobFiles <$> makeAbsolute "tests/fixtures/test.pdf"
  bob #> ("/f @" <> mobileName <> " " <> bobsFile)
  bob <## "use /fc 1 to cancel sending"

  desktop <# "bob> sends file test.pdf (266.0 KiB / 272376 bytes)"
  desktop <## "use /fr 1 [<dir>/ | <path>] to receive it"
  desktop ##> "/fr 1"
  concurrently_
    do
      bob <## "started sending file 1 (test.pdf) to alice"
      bob <## "completed sending file 1 (test.pdf) to alice"

    do
      desktop <## "saving file 1 from bob to test.pdf"
      desktop <## "started receiving file 1 (test.pdf) from bob"

  let desktopReceived = desktopFiles </> desktopStore </> "test.pdf"
  -- desktop <## ("completed receiving file 1 (" <> desktopReceived <> ") from bob")
  desktop <## "completed receiving file 1 (test.pdf) from bob"
  bobsFileSize <- getFileSize bobsFile
  -- getFileSize desktopReceived `shouldReturn` bobsFileSize
  bobsFileBytes <- B.readFile bobsFile
  -- B.readFile desktopReceived `shouldReturn` bobsFileBytes

  -- test file transit on mobile
  mobile ##> "/fs 1"
  mobile <## "receiving file 1 (test.pdf) complete, path: test.pdf"
  getFileSize (mobileFiles </> "test.pdf") `shouldReturn` bobsFileSize
  B.readFile (mobileFiles </> "test.pdf") `shouldReturn` bobsFileBytes

  logNote "file received"

  desktopFile <- makeRelative desktopFiles <$> makeAbsolute "tests/fixtures/logo.jpg" -- XXX: not necessary for _send, but required for /f
  logNote $ "sending " <> tshow desktopFile
  doesFileExist (bobFiles </> "logo.jpg") `shouldReturn` False
  doesFileExist (mobileFiles </> "logo.jpg") `shouldReturn` False
  desktop ##> "/_send @2 json {\"filePath\": \"./tests/fixtures/logo.jpg\", \"msgContent\": {\"type\": \"text\", \"text\": \"hi, sending a file\"}}"
  desktop <# "@bob hi, sending a file"
  desktop <# "/f @bob logo.jpg"
  desktop <## "use /fc 2 to cancel sending"

  bob <# "alice> hi, sending a file"
  bob <# "alice> sends file logo.jpg (31.3 KiB / 32080 bytes)"
  bob <## "use /fr 2 [<dir>/ | <path>] to receive it"
  bob ##> "/fr 2"
  concurrently_
    do
      bob <## "saving file 2 from alice to logo.jpg"
      bob <## "started receiving file 2 (logo.jpg) from alice"
      bob <## "completed receiving file 2 (logo.jpg) from alice"
      bob ##> "/fs 2"
      bob <## "receiving file 2 (logo.jpg) complete, path: logo.jpg"
    do
      desktop <## "started sending file 2 (logo.jpg) to bob"
      desktop <## "completed sending file 2 (logo.jpg) to bob"
  desktopFileSize <- getFileSize desktopFile
  getFileSize (bobFiles </> "logo.jpg") `shouldReturn` desktopFileSize
  getFileSize (mobileFiles </> "logo.jpg") `shouldReturn` desktopFileSize

  desktopFileBytes <- B.readFile desktopFile
  B.readFile (bobFiles </> "logo.jpg") `shouldReturn` desktopFileBytes
  B.readFile (mobileFiles </> "logo.jpg") `shouldReturn` desktopFileBytes

  logNote "file sent"

  stopMobile mobile desktop

-- * Utils

startRemote :: TestCC -> TestCC -> IO ()
startRemote mobile desktop = do
  desktop ##> "/create remote host"
  desktop <## "remote host 1 created"
  desktop <## "connection code:"
  fingerprint <- getTermLine desktop

  desktop ##> "/start remote host 1"
  desktop <## "ok"

  mobile ##> "/start remote ctrl"
  mobile <## "ok"
  mobile <## "remote controller announced"
  mobile <## "connection code:"
  fingerprint' <- getTermLine mobile
  fingerprint' `shouldBe` fingerprint
  mobile ##> ("/register remote ctrl " <> fingerprint' <> " " <> "My desktop")
  mobile <## "remote controller 1 registered"
  mobile ##> "/accept remote ctrl 1"
  mobile <## "ok" -- alternative scenario: accepted before controller start
  mobile <## "remote controller 1 connecting to My desktop"
  mobile <## "remote controller 1 connected, My desktop"
  desktop <## "remote host 1 connected"

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
eventually retries action = tryAny action >>= \case -- TODO: only catch timeouts
  Left err | retries == 0 -> throwIO err
  Left _ -> eventually (retries - 1) action
  Right r -> pure r
