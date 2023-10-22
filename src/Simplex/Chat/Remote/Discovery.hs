{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Simplex.Chat.Remote.Discovery
  ( -- * Announce
    announceRevHTTP2,
    runAnnouncer,
    startTLSServer,
    runHTTP2Client,

    -- * Discovery
    connectRevHTTP2,
    withListener,
    openListener,
    recvAnnounce,
    connectTLSClient,
    attachHTTP2Server,
  )
where

import Control.Logger.Simple
import Control.Monad
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.String (IsString)
import qualified Network.Socket as N
import qualified Network.TLS as TLS
import qualified Network.UDP as UDP
import Simplex.Chat.Remote.Multicast (setMembership)
import Simplex.Chat.Remote.Types (Tasks, registerAsync)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Transport (supportedParameters)
import qualified Simplex.Messaging.Transport as Transport
import Simplex.Messaging.Transport.Client (TransportHost (..), defaultTransportClientConfig, runTransportClient)
import Simplex.Messaging.Transport.HTTP2 (defaultHTTP2BufferSize, getHTTP2Body)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError (..), attachHTTP2Client, bodyHeadSize, connTimeout, defaultHTTP2ClientConfig)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..), runHTTP2ServerWith)
import Simplex.Messaging.Transport.Server (defaultTransportServerConfig, runTransportServer)
import Simplex.Messaging.Util (ifM, tshow, whenM)
import UnliftIO
import UnliftIO.Concurrent

-- | mDNS multicast group
pattern MULTICAST_ADDR_V4 :: (IsString a, Eq a) => a
pattern MULTICAST_ADDR_V4 = "224.0.0.251"

pattern ANY_ADDR_V4 :: (IsString a, Eq a) => a
pattern ANY_ADDR_V4 = "0.0.0.0"

pattern DISCOVERY_PORT :: (IsString a, Eq a) => a
pattern DISCOVERY_PORT = "5226"

-- | Announce tls server, wait for connection and attach http2 client to it.
--
-- Announcer is started when TLS server is started and stopped when a connection is made.
announceRevHTTP2 :: StrEncoding a => Tasks -> a -> TLS.Credentials -> IO () -> IO (Either HTTP2ClientError HTTP2Client)
announceRevHTTP2 tasks invite credentials finishAction = do
  httpClient <- newEmptyMVar
  started <- newEmptyTMVarIO
  finished <- newEmptyMVar
  _ <- forkIO $ readMVar finished >> finishAction -- attach external cleanup action to session lock
  announcer <- async . liftIO . whenM (atomically $ takeTMVar started) $ do
    logInfo $ "Starting announcer for " <> tshow (strEncode invite)
    runAnnouncer (strEncode invite)
  tasks `registerAsync` announcer
  tlsServer <- startTLSServer started credentials $ \tls -> do
    logInfo $ "Incoming connection for " <> tshow (strEncode invite)
    cancel announcer
    runHTTP2Client finished httpClient tls `catchAny` (logError . tshow)
    logInfo $ "Client finished for " <> tshow (strEncode invite)
  -- BUG: this should be handled in HTTP2Client wrapper
  _ <- forkIO $ do
    waitCatch tlsServer >>= \case
      Left err | fromException err == Just AsyncCancelled -> logDebug "tlsServer cancelled"
      Left err -> do
        logError $ "tlsServer failed to start: " <> tshow err
        void $ tryPutMVar httpClient $ Left HCNetworkError
        void . atomically $ tryPutTMVar started False
      Right () -> pure ()
    void $ tryPutMVar finished ()
  tasks `registerAsync` tlsServer
  logInfo $ "Waiting for client for " <> tshow (strEncode invite)
  readMVar httpClient

-- | Broadcast invite with link-local datagrams
runAnnouncer :: ByteString -> IO ()
runAnnouncer inviteBS = do
  bracket (UDP.clientSocket MULTICAST_ADDR_V4 DISCOVERY_PORT False) UDP.close $ \sock -> do
    let raw = UDP.udpSocket sock
    N.setSocketOption raw N.Broadcast 1
    N.setSocketOption raw N.ReuseAddr 1
    void $ setMembership raw (N.tupleToHostAddress (224, 0, 0, 251)) True
    forever $ do
      UDP.send sock inviteBS
      threadDelay 1000000

-- XXX: Do we need to start multiple TLS servers for different mobile hosts?
startTLSServer :: (MonadUnliftIO m) => TMVar Bool -> TLS.Credentials -> (Transport.TLS -> IO ()) -> m (Async ())
startTLSServer started credentials = async . liftIO . runTransportServer started DISCOVERY_PORT serverParams defaultTransportServerConfig
  where
    serverParams =
      def
        { TLS.serverWantClientCert = False,
          TLS.serverShared = def {TLS.sharedCredentials = credentials},
          TLS.serverHooks = def,
          TLS.serverSupported = supportedParameters
        }

-- | Attach HTTP2 client and hold the TLS until the attached client finishes.
runHTTP2Client :: MVar () -> MVar (Either HTTP2ClientError HTTP2Client) -> Transport.TLS -> IO ()
runHTTP2Client finishedVar clientVar tls =
  ifM (isEmptyMVar clientVar)
    attachClient
    (logError "HTTP2 session already started on this listener")
  where
    attachClient = do
      client <- attachHTTP2Client config ANY_ADDR_V4 DISCOVERY_PORT (putMVar finishedVar ()) defaultHTTP2BufferSize tls
      putMVar clientVar client
      readMVar finishedVar
    -- TODO connection timeout
    config = defaultHTTP2ClientConfig {bodyHeadSize = doNotPrefetchHead, connTimeout = maxBound}

withListener :: (MonadUnliftIO m) => (UDP.ListenSocket -> m a) -> m a
withListener = bracket openListener (liftIO . UDP.stop)

openListener :: (MonadIO m) => m UDP.ListenSocket
openListener = liftIO $ do
  sock <- UDP.serverSocket (MULTICAST_ADDR_V4, read DISCOVERY_PORT)
  N.setSocketOption (UDP.listenSocket sock) N.Broadcast 1
  pure sock

recvAnnounce :: (MonadIO m) => UDP.ListenSocket -> m (N.SockAddr, ByteString)
recvAnnounce sock = liftIO $ do
  (invite, UDP.ClientSockAddr source _cmsg) <- UDP.recvFrom sock
  pure (source, invite)

connectRevHTTP2 :: (MonadUnliftIO m) => TransportHost -> C.KeyHash -> (HTTP2Request -> m ()) -> m ()
connectRevHTTP2 host fingerprint = connectTLSClient host fingerprint . attachHTTP2Server

connectTLSClient :: (MonadUnliftIO m) => TransportHost -> C.KeyHash -> (Transport.TLS -> m a) -> m a
connectTLSClient host caFingerprint = runTransportClient defaultTransportClientConfig Nothing host DISCOVERY_PORT (Just caFingerprint)

attachHTTP2Server :: (MonadUnliftIO m) => (HTTP2Request -> m ()) -> Transport.TLS -> m ()
attachHTTP2Server processRequest tls = do
  withRunInIO $ \unlift ->
    runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
      reqBody <- getHTTP2Body r doNotPrefetchHead
      unlift $ processRequest HTTP2Request {sessionId, request = r, reqBody, sendResponse}

-- | Suppress storing initial chunk in bodyHead, forcing clients and servers to stream chunks
doNotPrefetchHead :: Int
doNotPrefetchHead = 0
