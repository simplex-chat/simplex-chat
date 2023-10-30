{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Simplex.Chat.Remote.Discovery where

import Control.Logger.Simple
import Control.Monad
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import Data.Default (def)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock.System (getSystemTime)
import Data.Word (Word16)
import qualified Network.Socket as N
import qualified Network.TLS as TLS
import qualified Network.UDP as UDP
import Simplex.Chat.Remote.Multicast (setMembership)
import Simplex.Chat.Remote.Types
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Transport (supportedParameters)
import qualified Simplex.Messaging.Transport as Transport
import Simplex.Messaging.Transport.Client (TransportHost (..), defaultTransportClientConfig, runTransportClient)
import Simplex.Messaging.Transport.HTTP2 (defaultHTTP2BufferSize, getHTTP2Body)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError (..), attachHTTP2Client, bodyHeadSize, connTimeout, defaultHTTP2ClientConfig)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..), runHTTP2ServerWith)
import Simplex.Messaging.Transport.Server (defaultTransportServerConfig, runTransportServerSocket, startTCPServer)
import Simplex.Messaging.Util (ifM, tshow)
import Simplex.Messaging.Version (mkVersionRange)
import UnliftIO
import UnliftIO.Concurrent

-- | mDNS multicast group
pattern MULTICAST_ADDR_V4 :: (IsString a, Eq a) => a
pattern MULTICAST_ADDR_V4 = "224.0.0.251"

pattern ANY_ADDR_V4 :: (IsString a, Eq a) => a
pattern ANY_ADDR_V4 = "0.0.0.0"

pattern DISCOVERY_PORT :: (IsString a, Eq a) => a
pattern DISCOVERY_PORT = "5227"

startSession :: MonadIO m => Maybe Text -> (N.HostAddress, Word16) -> C.KeyHash -> m ((C.APublicDhKey, C.APrivateDhKey), C.PrivateKeyEd25519, Announce, SignedOOB)
startSession deviceName serviceAddress caFingerprint = liftIO $ do
  sessionStart <- getSystemTime
  dh@(C.APublicDhKey C.SX25519 sessionDH, _) <- C.generateDhKeyPair C.SX25519
  (C.APublicVerifyKey C.SEd25519 sigPubKey, C.APrivateSignKey C.SEd25519 sigSecretKey) <- C.generateSignatureKeyPair C.SEd25519
  let
    announce =
      Announce
        { versionRange = announceVersionRange,
          sessionStart,
          announceCounter = 0,
          serviceAddress,
          caFingerprint,
          sessionDH,
          announceKey = sigPubKey
        }
  authToken <- decodeUtf8 . B64U.encode <$> getRandomBytes 12
  let
    oob =
      OOB
        { caFingerprint,
          authToken,
          host = decodeUtf8 . strEncode $ THIPv4 . N.hostAddressToTuple $ fst serviceAddress,
          port = snd serviceAddress,
          version = mkVersionRange 1 1,
          appName = "simplex-chat",
          sigPubKey,
          deviceName
        }
  pure (dh, sigSecretKey, announce, signOOB sigSecretKey oob)

getLocalAddress :: MonadIO m => TMVar Int -> m (Maybe N.HostAddress)
getLocalAddress subscribers = liftIO $ do
  probe <- mkIpProbe
  let bytes = smpEncode probe
  withListener subscribers $ \receiver ->
    withSender $ \sender -> do
      UDP.send sender bytes
      let expect = do
            UDP.recvFrom receiver >>= \case
              (p, _) | p /= bytes -> expect
              (_, UDP.ClientSockAddr (N.SockAddrInet _port host) _cmsg) -> pure host
              (_, UDP.ClientSockAddr _badAddr _) -> error "receiving from IPv4 socket"
      timeout 1000000 expect

mkIpProbe :: MonadIO m => m IpProbe
mkIpProbe = do
  randomNonce <- liftIO $ getRandomBytes 32
  pure IpProbe {versionRange = ipProbeVersionRange, randomNonce}

-- | Announce tls server, wait for connection and attach http2 client to it.
--
-- Announcer is started when TLS server is started and stopped when a connection is made.
announceRevHTTP2 :: Tasks -> (C.PrivateKeyEd25519, Announce) -> TLS.Credentials -> IO () -> IO (Either HTTP2ClientError HTTP2Client)
announceRevHTTP2 tasks (sigKey, announce@Announce {caFingerprint, serviceAddress=(host, _port)}) credentials finishAction = do
  httpClient <- newEmptyMVar
  started <- newEmptyTMVarIO
  finished <- newEmptyMVar
  _ <- forkIO $ readMVar finished >> finishAction -- attach external cleanup action to session lock
  announcer <- async . liftIO $ atomically (takeTMVar started) >>= \case
    Nothing -> pure () -- TLS server failed to start, skipping announcer
    Just givenPort -> do
      logInfo $ "Starting announcer for " <> ident <> " at " <> tshow (host, givenPort)
      runAnnouncer (sigKey, announce {serviceAddress = (host, fromIntegral givenPort)})
  tasks `registerAsync` announcer
  tlsServer <- startTLSServer started credentials $ \tls -> do
    logInfo $ "Incoming connection for " <> ident
    cancel announcer
    runHTTP2Client finished httpClient tls `catchAny` (logError . tshow)
    logInfo $ "Client finished for " <> ident
  -- BUG: this should be handled in HTTP2Client wrapper, partially handled in startTLSServer
  _ <- forkIO $ waitCatch tlsServer >> void (tryPutMVar finished ())
  tasks `registerAsync` tlsServer
  logInfo $ "Waiting for client for " <> ident
  readMVar httpClient
  where
    ident = decodeUtf8 $ strEncode caFingerprint

-- | Send replay-proof announce datagrams
runAnnouncer :: (C.PrivateKeyEd25519, Announce) -> IO ()
runAnnouncer (announceKey, initialAnnounce) = withSender $ loop initialAnnounce
  where
    loop announce sock = do
      UDP.send sock $ smpEncode (signAnnounce announceKey announce)
      threadDelay 1000000
      loop announce {announceCounter = announceCounter announce + 1} sock

startTLSServer :: (MonadUnliftIO m) => TMVar (Maybe N.PortNumber) -> TLS.Credentials -> (Transport.TLS -> IO ()) -> m (Async ())
startTLSServer started credentials server = async . liftIO $ do
  startedOk <- newEmptyTMVarIO
  bracketOnError (startTCPServer startedOk "0") (\_e -> void . atomically $ tryPutTMVar started Nothing) $ \socket ->
    ifM
      (atomically $ readTMVar startedOk)
      do
        port <- N.socketPort socket
        logInfo $ "System-assigned port: " <> tshow port
        atomically $ putTMVar started (Just port)
        runTransportServerSocket startedOk (pure socket) "RCP TLS" serverParams defaultTransportServerConfig server
      (void . atomically $ tryPutTMVar started Nothing)
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

withSender :: MonadUnliftIO m => (UDP.UDPSocket -> m a) -> m a
withSender = bracket (liftIO $ UDP.clientSocket MULTICAST_ADDR_V4 DISCOVERY_PORT False) (liftIO . UDP.close)

withListener :: MonadUnliftIO m => TMVar Int -> (UDP.ListenSocket -> m a) -> m a
withListener subscribers = bracket (openListener subscribers) (closeListener subscribers)

openListener :: MonadIO m => TMVar Int -> m UDP.ListenSocket
openListener subscribers = liftIO $ do
  sock <- UDP.serverSocket (MULTICAST_ADDR_V4, read DISCOVERY_PORT)
  logDebug $ "Discovery listener socket: " <> tshow sock
  let raw = UDP.listenSocket sock
  -- N.setSocketOption raw N.Broadcast 1
  joinMulticast subscribers raw (listenerHostAddr4 sock)
  pure sock

closeListener :: MonadIO m => TMVar Int -> UDP.ListenSocket -> m ()
closeListener subscribers sock = liftIO $
  partMulticast subscribers (UDP.listenSocket sock) (listenerHostAddr4 sock) `finally` UDP.stop sock

joinMulticast :: TMVar Int -> N.Socket -> N.HostAddress -> IO ()
joinMulticast subscribers sock group = do
  now <- atomically $ takeTMVar subscribers
  when (now == 0) $ do
    setMembership sock group True >>= \case
      Left e -> atomically (putTMVar subscribers now) >> logError ("setMembership failed " <> tshow e)
      Right () -> atomically $ putTMVar subscribers (now + 1)

partMulticast :: TMVar Int -> N.Socket -> N.HostAddress -> IO ()
partMulticast subscribers sock group = do
  now <- atomically $ takeTMVar subscribers
  when (now == 1) $
    setMembership sock group False >>= \case
      Left e -> atomically (putTMVar subscribers now) >> logError ("setMembership failed " <> tshow e)
      Right () -> atomically $ putTMVar subscribers (now - 1)

listenerHostAddr4 :: UDP.ListenSocket -> N.HostAddress
listenerHostAddr4 sock = case UDP.mySockAddr sock of
  N.SockAddrInet _port host -> host
  _ -> error "MULTICAST_ADDR_V4 is V4"

recvAnnounce :: (MonadIO m) => UDP.ListenSocket -> m (N.SockAddr, ByteString)
recvAnnounce sock = liftIO $ do
  (invite, UDP.ClientSockAddr source _cmsg) <- UDP.recvFrom sock
  pure (source, invite)

connectRevHTTP2 :: (MonadUnliftIO m) => (TransportHost, Word16) -> C.KeyHash -> (HTTP2Request -> m ()) -> m ()
connectRevHTTP2 serviceAddress fingerprint = connectTLSClient serviceAddress fingerprint . attachHTTP2Server

connectTLSClient :: (MonadUnliftIO m) => (TransportHost, Word16) -> C.KeyHash -> (Transport.TLS -> m a) -> m a
connectTLSClient (host, port) caFingerprint = runTransportClient defaultTransportClientConfig Nothing host (show port) (Just caFingerprint)

attachHTTP2Server :: (MonadUnliftIO m) => (HTTP2Request -> m ()) -> Transport.TLS -> m ()
attachHTTP2Server processRequest tls = do
  withRunInIO $ \unlift ->
    runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
      reqBody <- getHTTP2Body r doNotPrefetchHead
      unlift $ processRequest HTTP2Request {sessionId, request = r, reqBody, sendResponse}

-- | Suppress storing initial chunk in bodyHead, forcing clients and servers to stream chunks
doNotPrefetchHead :: Int
doNotPrefetchHead = 0
