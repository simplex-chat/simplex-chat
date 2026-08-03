{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RemoteTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (find, isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word16)
import qualified Network.Socket as NS
import Simplex.Chat.Controller (ChatCommand (..), ChatConfig (..), ChatHooks (..), defaultChatHooks, versionNumber)
import qualified Simplex.Chat.Controller as Controller
import Simplex.Chat.Library.Commands (execChatCommand, parseChatCommand)
import Simplex.Chat.Mobile.File
import Simplex.Chat.Remote (cancelRemoteCtrlSession, cleanupRemoteHostTransport, remoteFilesFolder, stopRemoteCtrlByController)
import Simplex.Chat.Remote.Types
import Simplex.Messaging.Crypto.File (CryptoFileArgs (..))
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import qualified Simplex.Messaging.Transport.HTTP2.Client as H2
import Simplex.Messaging.Util
import Simplex.RemoteControl.Invitation (RCInvitation (..), RCSignedInvitation (..))
import Simplex.RemoteControl.Types (RCCtrlAddress (..))
import System.FilePath ((</>))
import Test.Hspec hiding (it)
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.Directory

remoteTests :: SpecWith TestParams
remoteTests = describe "Remote" $ do
  describe "/start remote host parser" $ do
    it "parses iface name with a space followed by port=" $ \_ ->
      parseChatCommand "/start remote host new addr=192.168.1.5 iface=\"Ethernet 2\" port=12345"
        `shouldSatisfy` \case
          Right (StartRemoteHost Nothing (Just (RCCtrlAddress _ "Ethernet 2")) (Just 12345)) -> True
          _ -> False
  describe "/_abort remote ctrl parser" $ do
    it "parses the exact session sequence" $ \_ ->
      parseChatCommand "/_abort remote ctrl 42"
        `shouldSatisfy` \case
          Right (APIAbortRemoteCtrl 42) -> True
          _ -> False
  describe "reconnect compatibility" $ do
    it "closes the transport without waiting for blocked cleanup" $ \_ -> do
      transportClosed <- newEmptyTMVarIO
      cleanupStarted <- newEmptyTMVarIO
      cleanupRelease <- newEmptyTMVarIO
      cleanupFinished <- newEmptyTMVarIO
      let cleanup =
            uninterruptibleMask_ (atomically (putTMVar cleanupStarted ()) >> atomically (takeTMVar cleanupRelease))
              `finally` atomically (putTMVar cleanupFinished ())
      ( do
          timeout 1000000 (cleanupRemoteHostTransport (atomically $ putTMVar transportClosed ()) cleanup) `shouldReturn` Just ()
          timeout 1000000 (atomically $ takeTMVar transportClosed) `shouldReturn` Just ()
          timeout 1000000 (atomically $ takeTMVar cleanupStarted) `shouldReturn` Just ()
          atomically (tryReadTMVar cleanupFinished) `shouldReturn` Nothing
        )
        `finally` do
          atomically $ void $ tryPutTMVar cleanupRelease ()
          void $ timeout 1000000 $ atomically $ takeTMVar cleanupFinished
  xdescribe "No compression" $ aroundWith (. ((False, False),)) runRemoteTests
  xdescribe "Mobile offers compression" $ aroundWith (. ((True, False),)) runRemoteTests
  xdescribe "Desktop offers compression" $ aroundWith (. ((False, True),)) runRemoteTests
  describe "With compression" $ aroundWith (. ((True, True),)) runRemoteTests

runRemoteTests :: SpecWith ((Bool, Bool), TestParams)
runRemoteTests = do
  describe "protocol handshake" $ do
    it "connects with new pairing (stops mobile)" $ remoteHandshakeTest False
    it "connects with new pairing (stops desktop)" $ remoteHandshakeTest True
    it "connects with stored pairing" remoteHandshakeStoredTest
    it "drops the controller transport without stopping the pairing" remoteCtrlTransportDropTest
    it "stops after deleting the active mobile user" remoteStopAfterUserDeletionTest
    it "reconnects with the same invitation after transport loss" remoteHandshakeReconnectTest
    xitMacCI "connects with multicast discovery" remoteHandshakeDiscoverTest
    it "refuses invalid client cert" remoteHandshakeRejectTest
    it "connects with stored server bindings" storedBindingsTest
  describe "controller attempt teardown" $ do
    it "returns the session sequence for discovery" remoteCtrlAsyncFindTest
    it "returns the session sequence while the network handshake is blocked" remoteCtrlAsyncStartTest
    it "reports a network failure after accepting the connection attempt" remoteCtrlAsyncFailureTest
    it "aborts only the matching start attempt without a stop event" remoteCtrlExactAbortTest
  it "sends messages" remoteMessageTest
  describe "remote files" $ do
    it "store/get/send/receive files" remoteStoreFileTest
    it "should send files from CLI without /store" remoteCLIFileTest
  it "switches remote hosts" switchRemoteHostTest
  it "indicates remote hosts" indicateRemoteHostTest
  it "works with multiple profiles" multipleProfilesTest

-- * Chat commands

remoteHandshakeTest :: HasCallStack => Bool -> ((Bool, Bool), TestParams) -> IO ()
remoteHandshakeTest viaDesktop = testRemoteWithEvents $ \events compress mobile desktop -> do
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"
  mobile ##> "/list remote ctrls"
  mobile <## "No remote controllers"

  startRemote compress mobile desktop

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <## "1. Mobile (connected)"

  mobile ##> "/list remote ctrls"
  mobile <## "Remote controllers:"
  mobile <## "1. My desktop (connected)"

  if viaDesktop
    then do
      stopDesktop mobile desktop
      expectRemoteCtrlStopReason events $ \case
        Controller.RCSRControllerStopped -> True
        _ -> False
      readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
        Nothing -> pure ()
        Just _ -> expectationFailure "Remote controller session should be stopped"
    else stopMobile mobile desktop

  desktop ##> "/delete remote host 1"
  desktop <## "ok"
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"

  mobile ##> "/delete remote ctrl 1"
  mobile <## "ok"
  mobile ##> "/list remote ctrls"
  mobile <## "No remote controllers"

remoteHandshakeStoredTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteHandshakeStoredTest = testRemote $ \compress mobile desktop -> do
  logNote "Starting new session"
  startRemote compress mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

  logNote "Starting stored session"
  startRemoteStored compress mobile desktop
  stopDesktop mobile desktop `catchAny` (logError . tshow)

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <## "1. Mobile"
  mobile ##> "/list remote ctrls"
  mobile <## "Remote controllers:"
  mobile <## "1. My desktop"

  logNote "Starting stored session again"
  startRemoteStored compress mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

remoteCtrlTransportDropTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteCtrlTransportDropTest = testRemoteWithEvents $ \events compress mobile desktop -> do
  inv <- startRemoteInvitation compress mobile desktop
  (oldSessionSeq, oldSessionState, oldStopRequested) <-
    readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
      Just (sseq, session@Controller.RCSessionConnected {remoteStopRequested}) -> pure (sseq, Controller.rcsSessionState session, remoteStopRequested)
      _ -> fail "Remote controller session should be connected"
  let oldSessionCode = case oldSessionState of
        Controller.RCSConnected {sessionCode} -> sessionCode
        _ -> error "Connected session should have a session code"

  mobile ##> ("/_drop remote ctrl " <> T.unpack oldSessionCode)
  mobile <## "ok"
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Nothing -> pure ()
    Just _ -> expectationFailure "Remote controller session should be dropped"
  expectNoRemoteCtrlStop events
  eventually 3 $ desktop <## "remote host 1 stopped"

  mobile ##> "/list remote ctrls"
  mobile <## "Remote controllers:"
  mobile <## "1. My desktop"

  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting remote controller 1: My desktop, v" <> versionNumber)
  desktop <## "remote host 1 connecting"
  mobile <## "remote controller 1 connected"
  verifyRemoteCtrl mobile desktop
  mobile <## ("remote controller 1 session started with My desktop" <> compress)
  desktop <## ("remote host 1 connected" <> compress)
  newSessionCode <- remoteCtrlSessionCode mobile
  newSessionCode `shouldNotBe` oldSessionCode

  runReaderT (runExceptT $ cancelRemoteCtrlSession oldSessionSeq) (chatController mobile)
    >>= either (expectationFailure . show) pure
  remoteCtrlSessionCode mobile `shouldReturn` newSessionCode

  mobile ##> ("/_drop remote ctrl " <> T.unpack oldSessionCode)
  mobile <## "ok"
  remoteCtrlSessionCode mobile `shouldReturn` newSessionCode
  expectNoRemoteCtrlStop events

  runReaderT (runExceptT $ stopRemoteCtrlByController oldSessionSeq oldSessionState oldStopRequested) (chatController mobile)
    >>= either (expectationFailure . show) pure
  eventually 3 $ mobile <## "remote controller stopped"
  expectRemoteCtrlStopReason events $ \case
    Controller.RCSRControllerStopped -> True
    _ -> False
  remoteCtrlSessionCode mobile `shouldReturn` newSessionCode
  stopMobile mobile desktop

remoteStopAfterUserDeletionTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteStopAfterUserDeletionTest = testRemoteWithEvents $ \events compress mobile desktop -> do
  startRemote compress mobile desktop
  mobile ##> "/_delete user 1 del_smp=off"
  mobile <## "ok"
  readTVarIO (Controller.currentUser $ chatController mobile) >>= \case
    Nothing -> pure ()
    Just _ -> expectationFailure "Mobile should have no active user"
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Just (_, Controller.RCSessionConnected {}) -> pure ()
    _ -> expectationFailure "Remote controller session should remain connected"
  desktop ##> "/stop remote host 1"
  desktop <## "ok"
  eventually 3 $ mobile <## "remote controller stopped"
  expectRemoteCtrlStopReason events $ \case
    Controller.RCSRControllerStopped -> True
    _ -> False
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Nothing -> pure ()
    Just _ -> expectationFailure "Remote controller session should be stopped"

remoteCtrlSessionCode :: TestCC -> IO T.Text
remoteCtrlSessionCode mobile =
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Just (_, Controller.RCSessionConnected {tls}) -> pure $ tlsSessionCode tls
    _ -> fail "Remote controller session should be connected"

remoteCtrlExactAbortTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteCtrlExactAbortTest = testRemoteWithEvents $ \events _ mobile _ -> do
  (firstAction, firstRelease, firstCancelled) <- uninterruptibleBlockedAction
  atomically . writeTVar (Controller.remoteCtrlSession $ chatController mobile) $
    Just (42, Controller.RCSessionStarting (Just firstAction))
  timeout 1000000 (mobile ##> "/_abort remote ctrl 42" >> mobile <## "ok") `shouldReturn` Just ()
  poll firstAction >>= \case
    Nothing -> pure ()
    Just _ -> expectationFailure "Abort waited for controller attempt cleanup"
  atomically $ putTMVar firstRelease ()
  timeout 1000000 (atomically $ takeTMVar firstCancelled) `shouldReturn` Just ()
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Nothing -> pure ()
    Just _ -> expectationFailure "Matching abort did not remove the controller attempt"
  expectNoRemoteCtrlStop events

  (secondAction, secondCancelled) <- blockedAction
  atomically . writeTVar (Controller.remoteCtrlSession $ chatController mobile) $
    Just (43, Controller.RCSessionStarting (Just secondAction))
  mobile ##> "/_abort remote ctrl 42"
  mobile <## "ok"
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Just (43, _) -> pure ()
    _ -> expectationFailure "Stale abort removed the current controller attempt"
  poll secondAction >>= \case
    Nothing -> pure ()
    Just _ -> expectationFailure "Stale abort cancelled the current controller attempt"
  mobile ##> "/_abort remote ctrl 43"
  mobile <## "ok"
  timeout 1000000 (atomically $ takeTMVar secondCancelled) `shouldReturn` Just ()
  expectNoRemoteCtrlStop events
  where
    blockedAction = do
      started <- newEmptyTMVarIO
      blocked <- newEmptyTMVarIO
      cancelled <- newEmptyTMVarIO
      action <- async $ (atomically (putTMVar started ()) >> atomically (takeTMVar blocked)) `finally` atomically (putTMVar cancelled ())
      atomically $ takeTMVar started
      pure (action, cancelled)
    uninterruptibleBlockedAction = do
      started <- newEmptyTMVarIO
      release <- newEmptyTMVarIO
      cancelled <- newEmptyTMVarIO
      action <- async $ uninterruptibleMask_ (atomically (putTMVar started ()) >> atomically (takeTMVar release)) `finally` atomically (putTMVar cancelled ())
      atomically $ takeTMVar started
      pure (action, release, cancelled)

remoteCtrlAsyncStartTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteCtrlAsyncStartTest = testRemoteWithEvents $ \events _ mobile desktop -> do
  (inv, port) <- stoppedRemoteInvitation desktop
  withStalledTCPServer port $ \accepted -> do
    response <- timeout 1000000 $ runReaderT (execChatCommand Nothing (BC.pack $ "/connect remote ctrl " <> inv) 0) (chatController mobile)
    sessionSeq <- case response of
      Just (Right Controller.CRRemoteCtrlConnecting {sessionSeq}) -> pure sessionSeq
      Just r -> expectationFailure ("Unexpected connect response: " <> show r) >> fail "connect response"
      Nothing -> expectationFailure "Connect command waited for the network handshake" >> fail "connect timeout"
    timeout 1000000 (atomically $ takeTMVar accepted) `shouldReturn` Just ()
    readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
      Just (currentSeq, Controller.RCSessionStarting {rcsConnectAction = Just _}) -> currentSeq `shouldBe` sessionSeq
      Just (currentSeq, Controller.RCSessionConnecting {}) -> currentSeq `shouldBe` sessionSeq
      _ -> expectationFailure "Controller attempt was not cancellable while starting"
    mobile ##> ("/_abort remote ctrl " <> show sessionSeq)
    mobile <## "ok"
    readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
      Nothing -> pure ()
      Just _ -> expectationFailure "Abort did not remove the blocked controller attempt"
    expectNoRemoteCtrlStop events

remoteCtrlAsyncFindTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteCtrlAsyncFindTest = testRemote $ \compress mobile desktop -> do
  startRemote compress mobile desktop
  stopMobile mobile desktop
  response <- timeout 1000000 $ runReaderT (execChatCommand Nothing "/find remote ctrl" 0) (chatController mobile)
  sessionSeq <- case response of
    Just (Right Controller.CRRemoteCtrlSearching {sessionSeq}) -> pure sessionSeq
    Just r -> expectationFailure ("Unexpected discovery response: " <> show r) >> fail "discovery response"
    Nothing -> expectationFailure "Discovery command did not return" >> fail "discovery timeout"
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Just (currentSeq, Controller.RCSessionSearching {}) -> currentSeq `shouldBe` sessionSeq
    _ -> expectationFailure "Controller discovery was not active"
  mobile ##> ("/_abort remote ctrl " <> show sessionSeq)
  mobile <## "ok"

remoteCtrlAsyncFailureTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteCtrlAsyncFailureTest = testRemoteWithEvents $ \events _ mobile desktop -> do
  (inv, _) <- stoppedRemoteInvitation desktop
  response <- timeout 1000000 $ runReaderT (execChatCommand Nothing (BC.pack $ "/connect remote ctrl " <> inv) 0) (chatController mobile)
  sessionSeq <- case response of
    Just (Right Controller.CRRemoteCtrlConnecting {sessionSeq}) -> pure sessionSeq
    Just r -> expectationFailure ("Unexpected connect response: " <> show r) >> fail "connect response"
    Nothing -> expectationFailure "Connect command waited for the network failure" >> fail "connect timeout"
  failure <- timeout 5000000 . atomically $ nextConnectionFailure events
  failure `shouldBe` Just sessionSeq
  mobile <## "remote controller stopped"
  readTVarIO (Controller.remoteCtrlSession $ chatController mobile) >>= \case
    Nothing -> pure ()
    Just _ -> expectationFailure "Failed connection attempt was not removed"

stoppedRemoteInvitation :: HasCallStack => TestCC -> IO (String, Word16)
stoppedRemoteInvitation desktop = do
  desktop ##> "/set device name My desktop"
  desktop <## "ok"
  desktop ##> "/start remote host new"
  desktop <##. "new remote host started on "
  desktop <##. "other addresses: "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop
  port <- case strDecode (BC.pack inv) of
    Right RCSignedInvitation {invitation = RCInvitation {port}} -> pure port
    Left err -> fail err
  desktop ##> "/stop remote host new"
  desktop <## "ok"
  pure (inv, port)

nextConnectionFailure :: TQueue (Either Controller.ChatError Controller.ChatEvent) -> STM SessionSeq
nextConnectionFailure events =
  readTQueue events >>= \case
    Right Controller.CEvtRemoteCtrlStopped {rcStopReason = Controller.RCSRConnectionFailed {}, sessionSeq} -> pure sessionSeq
    _ -> nextConnectionFailure events

withStalledTCPServer :: Word16 -> (TMVar () -> IO a) -> IO a
withStalledTCPServer port action = bracket open NS.close $ \listener -> do
  accepted <- newEmptyTMVarIO
  release <- newEmptyTMVarIO
  withAsync (serve listener accepted release) $ \_ ->
    action accepted `finally` atomically (void $ tryPutTMVar release ())
  where
    open = do
      listener <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.setSocketOption listener NS.ReuseAddr 1
      NS.bind listener $ NS.SockAddrInet (fromIntegral port) 0
      NS.listen listener 1
      pure listener
    serve listener accepted release =
      bracket (NS.accept listener) (NS.close . fst) $ \_ -> do
        atomically $ putTMVar accepted ()
        atomically $ takeTMVar release

remoteHandshakeReconnectTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteHandshakeReconnectTest = testRemoteWithBlockedUser $ \events blockCommand commandStarted commandRelease stopEventDuringCleanup compress mobile desktop ->
  ( do
      inv <- startRemoteInvitation compress mobile desktop
      readTVarIO (Controller.currentRemoteHost $ chatController desktop) `shouldReturn` Just 1
      (oldPollAction, oldHTTPAction) <-
        readTVarIO (Controller.remoteHostSessions $ chatController desktop) >>= \sessions ->
          case M.lookup (RHId 1) sessions of
            Just (_, RHSessionConnected {rhClient = RemoteHostClient {httpClient}, pollAction}) -> pure (pollAction, H2.action httpClient)
            _ -> fail "Remote host session should be connected"

      atomically $ writeTVar blockCommand True
      request <- async $ runReaderT (execChatCommand (Just 1) "/user" 0) (chatController desktop)
      timeout 1000000 (atomically $ takeTMVar commandStarted) `shouldReturn` Just ()
      forM_ oldHTTPAction $ \a -> void . forkIO $ cancel a
      concurrently_
        (eventually 3 $ mobile <## "remote controller stopped")
        (eventually 3 $ desktop <## "remote host 1 stopped")
      timeout 1000000 (atomically $ takeTMVar stopEventDuringCleanup) `shouldReturn` Just True
      expectRemoteCtrlStopReason events $ \case
        Controller.RCSRDisconnected -> True
        _ -> False
      readTVarIO (Controller.currentRemoteHost $ chatController desktop) `shouldReturn` Just 1
      timeout 1000000 (waitCatch request) >>= \case
        Just (Right (Left Controller.ChatErrorRemoteHost {})) -> pure ()
        Just result -> expectationFailure $ "Unexpected in-flight command result: " <> show result
        Nothing -> expectationFailure "In-flight remote command stayed blocked after transport loss"
      atomically $ putTMVar commandRelease ()
      timeout 5000000 (waitCatch oldPollAction) >>= \case
        Just _ -> pure ()
        Nothing -> expectationFailure "Remote host poll should stop after transport loss"
      readTVarIO (Controller.remoteHostSessions $ chatController desktop) >>= \sessions ->
        case M.lookup (RHId 1) sessions of
          Just (_, RHSessionConnecting {invitation}) -> T.unpack invitation `shouldBe` inv
          _ -> expectationFailure "Remote host should retain the invitation while reconnecting"
      desktop <// 500000

      mobile ##> ("/connect remote ctrl " <> inv)
      mobile <## ("connecting remote controller 1: My desktop, v" <> versionNumber)
      desktop <## "remote host 1 connecting"
      mobile <## "remote controller 1 connected"
      verifyRemoteCtrl mobile desktop
      mobile <## ("remote controller 1 session started with My desktop" <> compress)
      desktop <## ("remote host 1 connected" <> compress)
      readTVarIO (Controller.currentRemoteHost $ chatController desktop) `shouldReturn` Just 1
      desktop ##> "/user"
      desktop <## "user profile: alice (Alice)"
      desktop <## "use /p <name> [<bio>] to change it"
      stopMobile mobile desktop
  )
    `finally` atomically (void $ tryPutTMVar commandRelease ())

remoteHandshakeDiscoverTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteHandshakeDiscoverTest = testRemote $ \compress mobile desktop -> do
  logNote "Preparing new session"
  startRemote compress mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

  logNote "Starting stored session with multicast"
  startRemoteDiscover compress mobile desktop
  stopMobile mobile desktop `catchAny` (logError . tshow)

remoteHandshakeRejectTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteHandshakeRejectTest = testRemote3 $ \compress mobile desktop mobileBob -> do
  logNote "Starting new session"
  startRemote compress mobile desktop
  stopMobile mobile desktop

  mobileBob ##> "/set device name MobileBob"
  mobileBob <## "ok"
  desktop ##> "/start remote host 1"
  desktop <##. "remote host 1 started on "
  desktop <##. "other addresses: "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop
  mobileBob ##> ("/connect remote ctrl " <> inv)
  mobileBob <## ("connecting new remote controller: My desktop, v" <> versionNumber)
  mobileBob <## "remote controller stopped: this link was used with another controller, please create a new link on the host"

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
  mobile <## ("remote controller 1 session started with My desktop" <> compress)
  desktop <## ("remote host 1 connected" <> compress)
  stopMobile mobile desktop

storedBindingsTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
storedBindingsTest = testRemote $ \compress mobile desktop -> do
  desktop ##> "/set device name My desktop"
  desktop <## "ok"
  mobile ##> "/set device name Mobile"
  mobile <## "ok"

  desktop ##> "/start remote host new"
  desktop <##. "new remote host started on "
  addrs <- words . dropStrPrefix "other addresses: " <$> getTermLine desktop
  Just localAddress <- pure $ find ("127." `isPrefixOf`) addrs
  desktop <## "Remote session invitation:"
  void $ getTermLine desktop
  desktop ##> "/stop remote host new"
  desktop <## "ok"

  desktop ##> ("/start remote host new addr=" <> localAddress <> " iface=\"lo\" port=52230")
  desktop <## ("new remote host started on " <> localAddress <> ":52230")
  desktop <##. "other addresses: "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop

  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting new remote controller: My desktop, v" <> versionNumber)
  desktop <## "new remote host connecting"
  mobile <## "new remote controller connected"
  verifyRemoteCtrl mobile desktop
  mobile <## ("remote controller 1 session started with My desktop" <> compress)
  desktop <## "new remote host 1 added: Mobile"
  desktop <## ("remote host 1 connected" <> compress)

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <##. "1. Mobile (connected) ["
  stopDesktop mobile desktop
  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <##. "1. Mobile ["

-- TODO: more parser tests

remoteMessageTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteMessageTest = testRemote3 $ \compress mobile desktop bob -> do  
  startRemote compress mobile desktop
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

remoteStoreFileTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteStoreFileTest =
  testRemote3 $ \compress mobile desktop bob ->
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

      startRemote compress mobile desktop
      contactBob desktop bob

      rhs <- readTVarIO (Controller.remoteHostSessions $ chatController desktop)
      desktopHostStore <- case M.lookup (RHId 1) rhs of
        Just (_, RHSessionConnected {storePath}) -> pure $ desktopHostFiles </> storePath </> remoteFilesFolder
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
      desktop ##> "/_send @2 json [{\"filePath\": \"test_1.pdf\", \"msgContent\": {\"type\": \"file\", \"text\": \"sending a file\"}}]"
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
      desktop ##> ("/_send @2 json [{\"fileSource\": {\"filePath\":\"test_2.pdf\", \"cryptoArgs\": " <> LB.unpack (J.encode cfArgs) <> "}, \"msgContent\": {\"type\": \"file\", \"text\": \"\"}}]")
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
    hostError cc err = do
      r <- getTermLine cc
      r `shouldStartWith` "remote host 1 error"
      r `shouldContain` err

remoteCLIFileTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
remoteCLIFileTest = testRemote3 $ \compress mobile desktop bob -> withXFTPServer $ do
  let mobileFiles = "./tests/tmp/mobile_files"
  mobile ##> ("/_files_folder " <> mobileFiles)
  mobile <## "ok"
  let bobFiles = "./tests/tmp/bob_files/"
  createDirectoryIfMissing True bobFiles
  let desktopHostFiles = "./tests/tmp/remote_hosts_data"
  desktop ##> ("/remote_hosts_folder " <> desktopHostFiles)
  desktop <## "ok"

  startRemote compress mobile desktop
  contactBob desktop bob

  rhs <- readTVarIO (Controller.remoteHostSessions $ chatController desktop)
  desktopHostStore <- case M.lookup (RHId 1) rhs of
    Just (_, RHSessionConnected {storePath}) -> pure $ desktopHostFiles </> storePath </> remoteFilesFolder
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

switchRemoteHostTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
switchRemoteHostTest = testRemote3 $ \compress mobile desktop bob -> do
  startRemote compress mobile desktop
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

indicateRemoteHostTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
indicateRemoteHostTest = testRemote4 $ \compress mobile desktop bob cath -> do
  connectUsers desktop cath
  startRemote compress mobile desktop
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

multipleProfilesTest :: HasCallStack => ((Bool, Bool), TestParams) -> IO ()
multipleProfilesTest = testRemote4 $ \compress mobile desktop bob cath -> do
  connectUsers desktop cath

  desktop ##> "/create user desk_bottom"
  desktop <## "user profile: desk_bottom"
  desktop <## "use /p <name> [<bio>] to change it"
  desktop ##> "/users"
  desktop <## "alice_desktop (Alice Desktop)"
  desktop <## "desk_bottom (active)"

  startRemote compress mobile desktop
  contactBob desktop bob
  desktop ##> "/users"
  desktop <## "alice (Alice) (active)"

  desktop ##> "/create user alt_alice"
  desktop <## "user profile: alt_alice"
  desktop <## "use /p <name> [<bio>] to change it"

  desktop ##> "/users"
  desktop <## "alice (Alice)"
  desktop <## "alt_alice (active)"

  desktop ##> "/user"
  desktop <## "user profile: alt_alice"
  desktop <## "use /p <name> [<bio>] to change it"

  bob #> "@alice hi"
  (desktop, "[user: alice] ") ^<# "bob> hi"

  cath #> "@alice_desktop hello"
  (desktop, "[local, user: alice_desktop] ") ^<# "cath> hello"

  desktop ##> "/switch remote host local"
  desktop <## "Using local profile"
  desktop ##> "/user"
  desktop <## "user profile: desk_bottom"
  desktop <## "use /p <name> [<bio>] to change it"

  bob #> "@alice hey"
  (desktop, "[remote: 1, user: alice] ") ^<# "bob> hey"

  stopDesktop mobile desktop

-- * Utils

testRemote :: HasCallStack => (String -> TestCC -> TestCC -> IO()) -> ((Bool, Bool), TestParams) -> IO ()
testRemote test ((mobileCompression, desktopCompression), ps) =
  withNewTestChatCfg ps testCfg {remoteCompression = mobileCompression} "mobile" aliceProfile $ \mobile ->
    withNewTestChatCfg ps testCfg {remoteCompression = desktopCompression} "desktop" aliceDesktopProfile $ \desktop ->
      let compress = " (" <> (if mobileCompression && desktopCompression then "with" else "no") <> " compression)"
       in test compress mobile desktop
  
testRemoteWithEvents :: HasCallStack => (TQueue (Either Controller.ChatError Controller.ChatEvent) -> String -> TestCC -> TestCC -> IO ()) -> ((Bool, Bool), TestParams) -> IO ()
testRemoteWithEvents test ((mobileCompression, desktopCompression), ps) = do
  events <- newTQueueIO
  let hooks = defaultChatHooks {eventHook = Just $ \_ event -> event <$ atomically (writeTQueue events event)}
  withNewTestChatCfg ps testCfg {remoteCompression = mobileCompression, chatHooks = hooks} "mobile" aliceProfile $ \mobile ->
    withNewTestChatCfg ps testCfg {remoteCompression = desktopCompression} "desktop" aliceDesktopProfile $ \desktop ->
      let compress = " (" <> (if mobileCompression && desktopCompression then "with" else "no") <> " compression)"
       in test events compress mobile desktop

testRemoteWithBlockedUser :: HasCallStack => (TQueue (Either Controller.ChatError Controller.ChatEvent) -> TVar Bool -> TMVar () -> TMVar () -> TMVar Bool -> String -> TestCC -> TestCC -> IO ()) -> ((Bool, Bool), TestParams) -> IO ()
testRemoteWithBlockedUser test ((mobileCompression, desktopCompression), ps) = do
  events <- newTQueueIO
  commandStarted <- newEmptyTMVarIO
  commandRelease <- newEmptyTMVarIO
  stopEventDuringCleanup <- newEmptyTMVarIO
  blockCommand <- newTVarIO False
  let blockUser _ cmd@ShowActiveUser = do
        shouldBlock <- atomically $ do
          b <- readTVar blockCommand
          writeTVar blockCommand False
          pure b
        when shouldBlock $ uninterruptibleMask_ $ do
          atomically $ putTMVar commandStarted ()
          atomically $ takeTMVar commandRelease
        pure $ Right cmd
      blockUser _ cmd = pure $ Right cmd
      hooks =
        defaultChatHooks
          { preCmdHook = Just blockUser,
            eventHook = Just $ \_ event -> event <$ atomically (writeTQueue events event)
          }
      desktopHooks =
        defaultChatHooks
          { eventHook = Just $ \cc event -> do
              case event of
                Right Controller.CEvtRemoteHostStopped {} -> do
                  sessions <- readTVarIO $ Controller.remoteHostSessions cc
                  let duringCleanup = any (\case (_, RHSessionDisconnecting {}) -> True; _ -> False) $ M.elems sessions
                  atomically $ void $ tryPutTMVar stopEventDuringCleanup duringCleanup
                _ -> pure ()
              pure event
          }
  withNewTestChatCfg ps testCfg {remoteCompression = mobileCompression, chatHooks = hooks} "mobile" aliceProfile $ \mobile ->
    withNewTestChatCfg ps testCfg {remoteCompression = desktopCompression, chatHooks = desktopHooks} "desktop" aliceDesktopProfile $ \desktop ->
      let compress = " (" <> (if mobileCompression && desktopCompression then "with" else "no") <> " compression)"
       in test events blockCommand commandStarted commandRelease stopEventDuringCleanup compress mobile desktop

expectRemoteCtrlStopReason :: HasCallStack => TQueue (Either Controller.ChatError Controller.ChatEvent) -> (Controller.RemoteCtrlStopReason -> Bool) -> IO ()
expectRemoteCtrlStopReason events expected = do
  reason_ <- timeout 5000000 . atomically $ nextStopReason
  reason_ `shouldSatisfy` maybe False expected
  where
    nextStopReason =
      readTQueue events >>= \case
        Right (Controller.CEvtRemoteCtrlStopped _ reason _) -> pure reason
        _ -> nextStopReason

expectNoRemoteCtrlStop :: HasCallStack => TQueue (Either Controller.ChatError Controller.ChatEvent) -> IO ()
expectNoRemoteCtrlStop events = do
  stop_ <- timeout 1000000 . atomically $ nextStop
  stop_ `shouldBe` Nothing
  where
    nextStop =
      readTQueue events >>= \case
        Right Controller.CEvtRemoteCtrlStopped {} -> pure ()
        _ -> nextStop

testRemote3 :: HasCallStack => (String -> TestCC -> TestCC -> TestCC -> IO()) -> ((Bool, Bool), TestParams) -> IO ()
testRemote3 test ps =
  testRemote
    (\compress mobile desktop -> withNewTestChat (snd ps) "bob" bobProfile $ test compress mobile desktop)
    ps

testRemote4 :: HasCallStack => (String -> TestCC -> TestCC -> TestCC -> TestCC -> IO()) -> ((Bool, Bool), TestParams) -> IO ()
testRemote4 test ps =
  testRemote3
    (\compress mobile desktop bob -> withNewTestChat (snd ps) "cath" cathProfile $ test compress mobile desktop bob)
    ps

startRemote :: String -> TestCC -> TestCC -> IO ()
startRemote compress mobile desktop = void $ startRemoteInvitation compress mobile desktop

startRemoteInvitation :: String -> TestCC -> TestCC -> IO String
startRemoteInvitation compress mobile desktop = do
  desktop ##> "/set device name My desktop"
  desktop <## "ok"
  mobile ##> "/set device name Mobile"
  mobile <## "ok"
  desktop ##> "/start remote host new"
  desktop <##. "new remote host started on "
  desktop <##. "other addresses: "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop
  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting new remote controller: My desktop, v" <> versionNumber)
  desktop <## "new remote host connecting"
  mobile <## "new remote controller connected"
  verifyRemoteCtrl mobile desktop
  mobile <## ("remote controller 1 session started with My desktop" <> compress)
  desktop <## "new remote host 1 added: Mobile"
  desktop <## ("remote host 1 connected" <> compress)
  pure inv

startRemoteStored :: String -> TestCC -> TestCC -> IO ()
startRemoteStored compress mobile desktop = do
  desktop ##> "/start remote host 1"
  desktop <##. "remote host 1 started on "
  desktop <##. "other addresses: "
  desktop <## "Remote session invitation:"
  inv <- getTermLine desktop
  mobile ##> ("/connect remote ctrl " <> inv)
  mobile <## ("connecting remote controller 1: My desktop, v" <> versionNumber)
  desktop <## "remote host 1 connecting"
  mobile <## "remote controller 1 connected"
  verifyRemoteCtrl mobile desktop
  mobile <## ("remote controller 1 session started with My desktop" <> compress)
  desktop <## ("remote host 1 connected" <> compress)

startRemoteDiscover :: String -> TestCC -> TestCC -> IO ()
startRemoteDiscover compress mobile desktop = do
  desktop ##> "/start remote host 1 multicast=on"
  desktop <##. "remote host 1 started on "
  desktop <##. "other addresses: "
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
  mobile <## ("remote controller 1 session started with My desktop" <> compress)
  desktop <## ("remote host 1 connected" <> compress)

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
