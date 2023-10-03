{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RemoteTests where

import Control.Monad.Reader (runReaderT)
import ChatClient
import ChatTests.Utils
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (fromString)
import Debug.Trace
import Network.HTTP.Types (ok200)
import qualified Network.HTTP2.Client as C
import qualified Network.HTTP2.Server as S
import qualified Network.Socket as N
import qualified Network.TLS as TLS
import Simplex.Chat (execChatCommand)
import Simplex.Chat.Controller (ChatResponse (..), RemoteCtrlInfo (..), RemoteHostInfo (..), RemoteHostOOB (..))
import qualified Simplex.Chat.Remote.Discovery as Discovery
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import qualified Simplex.Messaging.Transport as Transport
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Response (..), closeHTTP2Client, sendRequest)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

remoteTests :: SpecWith FilePath
remoteTests = do
  remoteHandshakeTests

remoteHandshakeTests :: SpecWith FilePath
remoteHandshakeTests = describe "Handshake" $ do
  it "generates usable credentials" genCredentialsTest
  it "connects announcer with discoverer over reverse-http2" announceDiscoverHttp2Test
  it "stores data for controllers and hosts" remoteStoreTest

-- * Low-level TLS with ephemeral credentials

genCredentialsTest :: (HasCallStack) => FilePath -> IO ()
genCredentialsTest _tmp = do
  (fingerprint, credentials) <- genTestCredentials
  started <- newEmptyTMVarIO
  server <- Discovery.spawnTLSServer started credentials serverHandler
  ok <- atomically (readTMVar started)
  unless ok $ cancel server >> error "TLS server failed to start"
  Discovery.connectTLSClient "127.0.0.1" fingerprint clientHandler
  cancel server
  where
    serverHandler serverTls = do
      traceM "    - Sending from server"
      Transport.putLn serverTls "hi client"
      traceM "    - Reading from server"
      Transport.getLn serverTls `shouldReturn` "hi server"
    clientHandler clientTls = do
      traceM "    - Sending from client"
      Transport.putLn clientTls "hi server"
      traceM "    - Reading from client"
      Transport.getLn clientTls `shouldReturn` "hi client"

-- * UDP discovery and rever HTTP2

announceDiscoverHttp2Test :: (HasCallStack) => FilePath -> IO ()
announceDiscoverHttp2Test _tmp = do
  (fingerprint, credentials) <- genTestCredentials
  finished <- newEmptyMVar
  announcer <- async $ do
    traceM "    - Controller: starting"
    http <- Discovery.announceRevHttp2 (putMVar finished ()) fingerprint credentials >>= either (fail . show) pure
    traceM "    - Controller: got client"
    sendRequest http (C.requestNoBody "GET" "/" []) (Just 10000000) >>= \case
      Left err -> do
        traceM "    - Controller: got error"
        fail $ show err
      Right HTTP2Response {} ->
        traceM "    - Controller: got response"
    closeHTTP2Client http
  dis <- async $ do
    sock <- Discovery.openListener
    (N.SockAddrInet _port addr, invite) <- Discovery.recvAnnounce sock
    strDecode invite `shouldBe` Right fingerprint
    traceM "    - Host: connecting"
    server <- async $ Discovery.connectTLSClient (THIPv4 $ N.hostAddressToTuple addr) fingerprint $ \tls -> do
      traceM "    - Host: got tls"
      flip Discovery.attachHttp2Server tls $ \HTTP2Request {sendResponse} -> do
        traceM "    - Host: got request"
        sendResponse $ S.responseNoBody ok200 []
        traceM "    - Host: sent response"
    takeMVar finished
    cancel server
    traceM "    - Host: finished"
  waitBoth dis announcer `shouldReturn` ((), ())

-- * Chat commands

cmd :: TestCC -> String -> IO ChatResponse
cmd cc s = runReaderT (execChatCommand Nothing (encodeUtf8 $ fromString s)) cc.chatController

remoteStoreTest :: HasCallStack => FilePath -> IO ()
remoteStoreTest = testChat2 aliceProfile bobProfile $ \desktop mobile -> do
  CRRemoteHostList [] <- cmd desktop "/list remote hosts"
  CRRemoteHostCreated{remoteHostId=tmpHost, oobData=RemoteHostOOB{caFingerprint=tmpFP}} <- cmd desktop "/create remote host TmpHost"
  cmd desktop "/list remote hosts" >>= \case
    CRRemoteHostList [RemoteHostInfo {remoteHostId, displayName, sessionActive}] -> do
      remoteHostId `shouldBe` tmpHost
      displayName `shouldBe` ("TmpHost" :: Text)
      sessionActive `shouldBe` False
      -- XXX: check rhi.storePath exists?
    unexpected -> fail $ "Unexpected: " <> show unexpected

  CRRemoteHostStarted{remoteHostId=hostStartedId} <- cmd desktop $ "/start remote host " <> show tmpHost
  hostStartedId `shouldBe` tmpHost

  CRRemoteCtrlList [] <- cmd mobile "/list remote ctrls"
  let ctrlRegisterCmd = "/register remote ctrl TmpCtrl " <> BS8.unpack (strEncode tmpFP)
  CRRemoteCtrlRegistered{remoteCtrlId=tmpCtrl} <- cmd mobile ctrlRegisterCmd
  cmd mobile "/list remote ctrls" >>= \case
    CRRemoteCtrlList [RemoteCtrlInfo {remoteCtrlId, displayName, sessionActive}] -> do
      remoteCtrlId `shouldBe` tmpCtrl
      displayName `shouldBe` ("TmpCtrl" :: Text)
      sessionActive `shouldBe` False
    unexpected -> fail $ "Unexpected: " <> show unexpected
  CRRemoteCtrlStarted <- cmd mobile "/start remote ctrl"

  threadDelay 2000000 -- wait 2 broadcast intervals for discovery
  -- TODO: read events

  CRRemoteCtrlAccepted{remoteCtrlId=tmpCtrlAccepted} <- cmd mobile $ "/accept remote ctrl " <> show tmpCtrl
  tmpCtrlAccepted `shouldBe` tmpCtrl

  CRRemoteCtrlStopped{remoteCtrlId=tmpCtrlStopped} <- cmd mobile $ "/stop remote ctrl " <> show tmpCtrl
  tmpCtrlStopped `shouldBe` tmpCtrl
  CRRemoteCtrlDisposed{remoteCtrlId=tmpCtrlDisposed} <- cmd mobile $ "/dispose remote ctrl " <> show tmpCtrl
  tmpCtrlDisposed `shouldBe` tmpCtrl
  CRRemoteCtrlList noCtrls <- cmd mobile "/list remote ctrls"
  null noCtrls `shouldBe` True

  CRRemoteHostStopped{remoteHostId=tmpHostStopped} <- cmd desktop $ "/stop remote host " <> show tmpHost
  tmpHostStopped `shouldBe` tmpHost
  CRRemoteHostDisposed{remoteHostId=tmpHostDisposed} <- cmd desktop $ "/dispose remote host " <> show tmpHost
  tmpHostDisposed `shouldBe` tmpHost
  CRRemoteHostList noHosts <- cmd desktop "/list remote hosts"
  null noHosts `shouldBe` True

  traceM "tests done"

-- * Utils

genTestCredentials :: IO (C.KeyHash, TLS.Credentials)
genTestCredentials = do
  caCreds <- liftIO $ genCredentials Nothing (0, 24) "CA"
  sessionCreds <- liftIO $ genCredentials (Just caCreds) (0, 24) "Session"
  pure . tlsCredentials $ sessionCreds :| [caCreds]
