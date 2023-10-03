{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module RemoteTests where

import ChatClient
import ChatTests.Utils
import Control.Monad
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty (..))
import Debug.Trace
import Network.HTTP.Types (ok200)
import qualified Network.HTTP2.Client as C
import qualified Network.HTTP2.Server as S
import qualified Network.Socket as N
import qualified Network.TLS as TLS
import Simplex.Chat.Mobile
import Simplex.Chat.Mobile.Shared
import qualified Simplex.Chat.Remote.Discovery as Discovery
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import qualified Simplex.Messaging.Transport as Transport
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (Credentials, genCredentials, tlsCredentials)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Response (..), closeHTTP2Client, sendRequest)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..), closeHTTP2Server)
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent

remoteTests :: SpecWith FilePath
remoteTests = do
  remoteHandshakeTests

remoteHandshakeTests :: SpecWith FilePath
remoteHandshakeTests = describe "Handshake" $ do
  it "generates usable credentials" genCredentialsTest
  it "connects announcer with discoverer over reverse-http2" announceDiscoverHttp2Test

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

-- * Utils

genTestCredentials :: IO (C.KeyHash, TLS.Credentials)
genTestCredentials = do
  caCreds <- liftIO $ genCredentials Nothing (0, 24) "CA"
  sessionCreds <- liftIO $ genCredentials (Just caCreds) (0, 24) "Session"
  pure . tlsCredentials $ sessionCreds :| [caCreds]
