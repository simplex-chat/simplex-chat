{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module RemoteTests where

import ChatClient
import ChatTests.Utils
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Debug.Trace
import Network.HTTP.Types (ok200)
import qualified Network.HTTP2.Client as C
import qualified Network.HTTP2.Server as S
import qualified Network.Socket as N
import qualified Network.TLS as TLS
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

remoteTests :: SpecWith FilePath
remoteTests = describe "Handshake" $ do
  it "generates usable credentials" genCredentialsTest
  it "connects announcer with discoverer over reverse-http2" announceDiscoverHttp2Test
  xit "connects desktop and mobile" remoteHandshakeTest
  it "send messages via remote desktop" remoteCommandTest

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
  controller <- async $ do
    traceM "    - Controller: starting"
    bracket
      (Discovery.announceRevHTTP2 (putMVar finished ()) fingerprint credentials >>= either (fail . show) pure)
      closeHTTP2Client
      ( \http -> do
          traceM "    - Controller: got client"
          sendRequest http (C.requestNoBody "GET" "/" []) (Just 10000000) >>= \case
            Left err -> do
              traceM "    - Controller: got error"
              fail $ show err
            Right HTTP2Response {} ->
              traceM "    - Controller: got response"
      )
  host <- async $ Discovery.withListener $ \sock -> do
    (N.SockAddrInet _port addr, invite) <- Discovery.recvAnnounce sock
    strDecode invite `shouldBe` Right fingerprint
    traceM "    - Host: connecting"
    server <- async $ Discovery.connectTLSClient (THIPv4 $ N.hostAddressToTuple addr) fingerprint $ \tls -> do
      traceM "    - Host: got tls"
      flip Discovery.attachHTTP2Server tls $ \HTTP2Request {sendResponse} -> do
        traceM "    - Host: got request"
        sendResponse $ S.responseNoBody ok200 []
        traceM "    - Host: sent response"
    takeMVar finished `finally` cancel server
    traceM "    - Host: finished"
  (waitBoth host controller `shouldReturn` ((), ())) `onException` (cancel host >> cancel controller)

-- * Chat commands

remoteHandshakeTest :: (HasCallStack) => FilePath -> IO ()
remoteHandshakeTest = testChat2 aliceProfile bobProfile $ \desktop mobile -> do
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"
  desktop ##> "/create remote host"
  desktop <## "remote host 1 created"
  desktop <## "connection code:"
  fingerprint <- getTermLine desktop

  desktop ##> "/list remote hosts"
  desktop <## "Remote hosts:"
  desktop <## "1. TODO" -- TODO host name probably should be Maybe, as when host is created there is no name yet
  desktop ##> "/start remote host 1"
  desktop <## "remote host 1 started"

  mobile ##> "/start remote ctrl"
  mobile <## "remote controller started"
  mobile <## "remote controller announced"
  mobile <## "connection code:"
  fingerprint' <- getTermLine mobile
  fingerprint' `shouldBe` fingerprint
  mobile ##> "/list remote ctrls"
  mobile <## "No remote controllers"
  mobile ##> ("/register remote ctrl " <> fingerprint')
  mobile <## "remote controller 1 registered"
  mobile ##> "/list remote ctrls"
  mobile <## "Remote controllers:"
  mobile <## "1. TODO"
  mobile ##> "/accept remote ctrl 1"
  mobile <## "remote controller 1 accepted" -- alternative scenario: accepted before controller start
  mobile <## "remote controller 1 connecting to TODO"
  mobile <## "remote controller 1 connected, TODO"
  mobile ##> "/stop remote ctrl"
  mobile <## "ok"
  mobile <## "remote controller stopped"
  mobile ##> "/delete remote ctrl 1"
  mobile <## "remote controller 1 deleted"
  mobile ##> "/list remote ctrls"
  mobile <## "No remote controllers"

  desktop ##> "/stop remote host 1"
  desktop <## "remote host 1 stopped"
  desktop ##> "/delete remote host 1"
  desktop <## "remote host 1 deleted"
  desktop ##> "/list remote hosts"
  desktop <## "No remote hosts"

remoteCommandTest :: (HasCallStack) => FilePath -> IO ()
remoteCommandTest = testChat3 aliceProfile aliceDesktopProfile bobProfile $ \mobile desktop bob -> do
  desktop ##> "/create remote host"
  desktop <## "remote host 1 created"
  desktop <## "connection code:"
  fingerprint <- getTermLine desktop

  desktop ##> "/start remote host 1"
  desktop <## "remote host 1 started"

  mobile ##> "/start remote ctrl"
  mobile <## "remote controller started"
  mobile <## "remote controller announced"
  mobile <## "connection code:"
  fingerprint' <- getTermLine mobile
  fingerprint' `shouldBe` fingerprint
  mobile ##> ("/register remote ctrl " <> fingerprint')
  mobile <## "remote controller 1 registered"
  mobile ##> "/accept remote ctrl 1"
  mobile <## "remote controller 1 accepted" -- alternative scenario: accepted before controller start
  mobile <## "remote controller 1 connecting to TODO"
  mobile <## "remote controller 1 connected, TODO"
  desktop <## "remote host 1 connected"

  traceM "    - exchanging contacts"
  bob ##> "/c"
  inv' <- getInvitation bob
  desktop ##> ("/c " <> inv')
  desktop <## "confirmation sent!"
  concurrently_
    (desktop <## "bob (Bob): contact is connected")
    (bob <## "alice (Alice): contact is connected")

  traceM "    - sending messages"
  desktop #> "@bob hello there 🙂"
  bob <# "alice> hello there 🙂"
  bob #> "@alice hi"
  desktop <# "bob> hi"

  traceM "    - post-remote checks"
  mobile ##> "/stop remote ctrl"
  mobile <## "ok"
  concurrently_
    (mobile <## "remote controller stopped")
    (desktop <## "remote host 1 stopped")
  mobile ##> "/contacts"
  mobile <## "bob (Bob)"

  traceM "    - done"

-- * Utils

genTestCredentials :: IO (C.KeyHash, TLS.Credentials)
genTestCredentials = do
  caCreds <- liftIO $ genCredentials Nothing (0, 24) "CA"
  sessionCreds <- liftIO $ genCredentials (Just caCreds) (0, 24) "Session"
  pure . tlsCredentials $ sessionCreds :| [caCreds]
