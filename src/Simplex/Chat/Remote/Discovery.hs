{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Simplex.Chat.Remote.Discovery
  ( -- * Announce
    runAnnouncer,

    -- * Discovery
    openListener,
    recvAnnounce,
    connectSessionHost,
    attachServer,
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
import UnliftIO
import UnliftIO.Concurrent

-- | Link-local broadcast address.
pattern BROADCAST_ADDR_V4 :: (IsString a, Eq a) => a
pattern BROADCAST_ADDR_V4 = "255.255.255.255"

pattern BROADCAST_PORT :: (IsString a, Eq a) => a
pattern BROADCAST_PORT = "5226"

runAnnouncer :: (StrEncoding invite, MonadUnliftIO m) => IO () -> invite -> TLS.Credentials -> m (Either HTTP2ClientError HTTP2Client)
runAnnouncer finished invite credentials = do
  started <- newEmptyTMVarIO
  aPid <- async $ announcer started (strEncode invite)
  let serverParams =
        def
          { TLS.serverWantClientCert = False,
            TLS.serverShared = def {TLS.sharedCredentials = credentials},
            TLS.serverHooks = def,
            TLS.serverSupported = supportedParameters
          }
  httpClient <- newEmptyMVar
  liftIO $ runTransportServer started BROADCAST_PORT serverParams defaultTransportServerConfig (run aPid httpClient)
  takeMVar httpClient
  where
    announcer started inviteBS = do
      atomically (takeTMVar started) >>= \case
        False ->
          error "Server not started?.."
        True -> liftIO $ do
          traceM $ "TCP server started at " <> BROADCAST_PORT
          sock <- UDP.clientSocket BROADCAST_ADDR_V4 BROADCAST_PORT False
          N.setSocketOption (UDP.udpSocket sock) N.Broadcast 1
          traceM $ "UDP announce started at " <> BROADCAST_ADDR_V4 <> ":" <> BROADCAST_PORT
          traceM $ "Server invite: " <> show inviteBS
          forever $ do
            UDP.send sock inviteBS
            threadDelay 1000000

    run :: Async () -> MVar (Either HTTP2ClientError HTTP2Client) -> Transport.TLS -> IO ()
    run aPid clientVar tls = do
      cancel aPid
      let partyHost = "255.255.255.255" -- XXX: get from tls somehow? not required as host verification is disabled.
      attachHTTP2Client defaultHTTP2ClientConfig partyHost BROADCAST_PORT finished defaultHTTP2BufferSize tls >>= putMVar clientVar

openListener :: (MonadIO m) => m UDP.ListenSocket
openListener = liftIO $ do
  sock <- UDP.serverSocket (BROADCAST_ADDR_V4, read BROADCAST_PORT)
  N.setSocketOption (UDP.listenSocket sock) N.Broadcast 1
  pure sock

recvAnnounce :: (MonadIO m) => UDP.ListenSocket -> m (N.SockAddr, ByteString)
recvAnnounce sock = liftIO $ do
  (invite, UDP.ClientSockAddr source _cmsg) <- UDP.recvFrom sock
  pure (source, invite)

connectSessionHost :: (MonadUnliftIO m) => TransportHost -> C.KeyHash -> (Transport.TLS -> m a) -> m a
connectSessionHost host caFingerprint = runTransportClient defaultTransportClientConfig Nothing host BROADCAST_PORT (Just caFingerprint)

attachServer :: (MonadUnliftIO m) => (HTTP2Request -> m ()) -> Transport.TLS -> m ()
attachServer processRequest tls = do
  withRunInIO $ \unlift ->
    runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
      reqBody <- getHTTP2Body r defaultHTTP2BufferSize
      unlift $ processRequest HTTP2Request {sessionId, request = r, reqBody, sendResponse}
