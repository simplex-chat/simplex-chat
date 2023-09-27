{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Remote.Discovery
  ( runAnnouncer,
    runDiscoverer,
  )
where

import Control.Monad
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
  liftIO $ runTransportServer started partyPort serverParams defaultTransportServerConfig (run aPid httpClient)
  takeMVar httpClient
  where
    announcer started inviteBS = do
      atomically (takeTMVar started) >>= \case
        False ->
          error "Server not started?.."
        True -> liftIO $ do
          traceM $ "TCP server started at " <> partyPort
          sock <- UDP.clientSocket broadcastAddrV4 partyPort False
          N.setSocketOption (UDP.udpSocket sock) N.Broadcast 1
          traceM $ "UDP announce started at " <> broadcastAddrV4 <> ":" <> partyPort
          traceM $ "Server invite: " <> show inviteBS
          forever $ do
            UDP.send sock inviteBS
            threadDelay 1000000

    run :: Async () -> MVar (Either HTTP2ClientError HTTP2Client) -> Transport.TLS -> IO ()
    run aPid clientVar tls = do
      cancel aPid
      let partyHost = "255.255.255.255" -- XXX: get from tls somehow? not required as host verification is disabled.
      attachHTTP2Client defaultHTTP2ClientConfig partyHost partyPort finished defaultHTTP2BufferSize tls >>= putMVar clientVar

-- | Link-local broadcast address.
broadcastAddrV4 :: (IsString a) => a
broadcastAddrV4 = "255.255.255.255"

partyPort :: (IsString a) => a
partyPort = "5226" -- XXX: should be `0` or something, to get a random port and announce it

runDiscoverer :: IO [(C.KeyHash, ctx)] -> (ctx -> IO Bool) -> (ctx -> Maybe SomeException -> IO ()) -> (ctx -> HTTP2Request -> IO ()) -> IO ()
runDiscoverer getFingerprints started finished processRequest = do
  sock <- UDP.serverSocket (broadcastAddrV4, read partyPort)
  N.setSocketOption (UDP.listenSocket sock) N.Broadcast 1
  traceM $ "runDiscoverer: " <> show sock
  go sock
  where
    go sock = do
      (invite, UDP.ClientSockAddr source _cmsg) <- UDP.recvFrom sock
      case strDecode invite of
        Left err -> do
          traceM $ "Inivite decode error: " <> err
          go sock
        Right inviteHash -> do
          expected <- getFingerprints
          case lookup inviteHash expected of
            Nothing -> do
              traceM $ "Unexpected invite: " <> show (invite, source)
              go sock
            Just ctx -> do
              host <- case source of
                N.SockAddrInet _port addr -> do
                  pure $ THIPv4 (N.hostAddressToTuple addr)
                unexpected ->
                  -- TODO: actually, Apple mandates IPv6 support
                  fail $ "Discoverer: expected an IPv4 party, got " <> show unexpected
              runTransportClient defaultTransportClientConfig Nothing host partyPort (Just inviteHash) $ \tls -> do
                accepted <- started ctx
                if not accepted
                  then go sock -- Ignore rejected invites and wait for another
                  else do
                    res <- try $ runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
                      reqBody <- getHTTP2Body r 16384
                      processRequest ctx HTTP2Request {sessionId, request = r, reqBody, sendResponse}
                    finished ctx $ either Just (\() -> Nothing) res
