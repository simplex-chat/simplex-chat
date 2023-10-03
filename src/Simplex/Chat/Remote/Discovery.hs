{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Simplex.Chat.Remote.Discovery
  ( -- * Announce
    announceRevHttp2,
    runAnnouncer,
    spawnTLSServer,
    attachHttp2Client,

    -- * Discovery
    connectRevHttp2,
    openListener,
    recvAnnounce,
    connectTLSClient,
    attachHttp2Server,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.String (IsString)
import Debug.Trace
import qualified Network.Socket as N
import qualified Network.TLS as TLS
import qualified Network.UDP as UDP
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Transport (supportedParameters)
import qualified Simplex.Messaging.Transport as Transport
import Simplex.Messaging.Transport.Client (TransportHost (..), defaultTransportClientConfig, runTransportClient)
import Simplex.Messaging.Transport.HTTP2 (defaultHTTP2BufferSize, getHTTP2Body)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError, attachHTTP2Client, defaultHTTP2ClientConfig)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..), runHTTP2ServerWith)
import Simplex.Messaging.Transport.Server (defaultTransportServerConfig, runTransportServer)
import Simplex.Messaging.Util (whenM)
import UnliftIO
import UnliftIO.Concurrent

-- | Link-local broadcast address.
pattern BROADCAST_ADDR_V4 :: (IsString a, Eq a) => a
pattern BROADCAST_ADDR_V4 = "255.255.255.255"

pattern BROADCAST_PORT :: (IsString a, Eq a) => a
pattern BROADCAST_PORT = "5226"

-- | Announce tls server, wait for connection and attach http2 client to it.
--
-- Announcer is started when TLS server is started and stopped when a connection is made.
announceRevHttp2 :: (StrEncoding invite, MonadUnliftIO m) => IO () -> invite -> TLS.Credentials -> m (Either HTTP2ClientError HTTP2Client)
announceRevHttp2 finishAction invite credentials = do
  httpClient <- newEmptyMVar
  started <- newEmptyTMVarIO
  finished <- newEmptyMVar
  announcer <- async . liftIO . whenM (atomically $ takeTMVar started) $ runAnnouncer (strEncode invite)
  tlsServer <- spawnTLSServer started credentials $ \tls -> cancel announcer >> attachHttp2Client finished httpClient tls
  _ <- forkIO . liftIO $ do
    readMVar finished
    cancel tlsServer
    finishAction
  readMVar httpClient

-- | Broadcast invite with link-local datagrams
runAnnouncer :: ByteString -> IO ()
runAnnouncer inviteBS = do
  sock <- UDP.clientSocket BROADCAST_ADDR_V4 BROADCAST_PORT False
  N.setSocketOption (UDP.udpSocket sock) N.Broadcast 1
  forever $ do
    UDP.send sock inviteBS
    threadDelay 1000000

spawnTLSServer :: (MonadUnliftIO m) => TMVar Bool -> TLS.Credentials -> (Transport.TLS -> IO ()) -> m (Async ())
spawnTLSServer started credentials = async . liftIO . runTransportServer started BROADCAST_PORT serverParams defaultTransportServerConfig
  where
    serverParams =
      def
        { TLS.serverWantClientCert = False,
          TLS.serverShared = def {TLS.sharedCredentials = credentials},
          TLS.serverHooks = def,
          TLS.serverSupported = supportedParameters
        }

-- | Attach HTTP2 client and hold the TLS until the attached client finishes.
attachHttp2Client :: MVar () -> MVar (Either HTTP2ClientError HTTP2Client) -> Transport.TLS -> IO ()
attachHttp2Client finishedVar clientVar tls = do
  let partyHost = "255.255.255.255" -- XXX: get from tls somehow? not required as host verification is disabled.
  attachHTTP2Client defaultHTTP2ClientConfig partyHost BROADCAST_PORT (putMVar finishedVar ()) defaultHTTP2BufferSize tls >>= putMVar clientVar
  readMVar finishedVar

openListener :: (MonadIO m) => m UDP.ListenSocket
openListener = liftIO $ do
  sock <- UDP.serverSocket (BROADCAST_ADDR_V4, read BROADCAST_PORT)
  N.setSocketOption (UDP.listenSocket sock) N.Broadcast 1
  pure sock

recvAnnounce :: (MonadIO m) => UDP.ListenSocket -> m (N.SockAddr, ByteString)
recvAnnounce sock = liftIO $ do
  (invite, UDP.ClientSockAddr source _cmsg) <- UDP.recvFrom sock
  pure (source, invite)

connectRevHttp2 :: (MonadUnliftIO m) => TransportHost -> C.KeyHash -> (HTTP2Request -> m ()) -> m ()
connectRevHttp2 host fingerprint = connectTLSClient host fingerprint . attachHttp2Server

connectTLSClient :: (MonadUnliftIO m) => TransportHost -> C.KeyHash -> (Transport.TLS -> m a) -> m a
connectTLSClient host caFingerprint = runTransportClient defaultTransportClientConfig Nothing host BROADCAST_PORT (Just caFingerprint)

attachHttp2Server :: (MonadUnliftIO m) => (HTTP2Request -> m ()) -> Transport.TLS -> m ()
attachHttp2Server processRequest tls = do
  withRunInIO $ \unlift ->
    runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
      reqBody <- getHTTP2Body r defaultHTTP2BufferSize
      unlift $ processRequest HTTP2Request {sessionId, request = r, reqBody, sendResponse}
