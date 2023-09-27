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
import Data.ByteString.Builder (Builder, intDec)
import Data.Default (def)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Server as HTTP2
import qualified Network.Socket as N
import qualified Network.TLS as TLS
import qualified Network.UDP as UDP
import Simplex.Chat.Controller (ChatMonad)
import Simplex.Chat.Types ()
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

runAnnouncer :: (StrEncoding invite, ChatMonad m) => IO () -> invite -> TLS.Credentials -> m (Either HTTP2ClientError HTTP2Client)
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

runDiscoverer :: (ChatMonad m) => Text -> m ()
runDiscoverer oobData =
  case strDecode (encodeUtf8 oobData) of
    Left err -> traceM $ "oobData decode error: " <> err
    Right expected -> liftIO $ do
      traceM $ "runDiscoverer: locating " <> show oobData
      sock <- UDP.serverSocket (broadcastAddrV4, read partyPort)
      N.setSocketOption (UDP.listenSocket sock) N.Broadcast 1
      traceM $ "runDiscoverer: " <> show sock
      go sock expected
  where
    go sock expected = do
      (invite, UDP.ClientSockAddr source _cmsg) <- UDP.recvFrom sock
      traceShowM (invite, source)
      let expect hash = hash `elem` [expected] -- XXX: can be a callback to fetch actual invite list just in time
      case strDecode invite of
        Left err -> do
          traceM $ "Inivite decode error: " <> err
          go sock expected
        Right inviteHash | not (expect inviteHash) -> do
          traceM $ "Skipping unexpected invite " <> show (strEncode inviteHash)
          go sock expected
        Right _expected -> do
          host <- case source of
            N.SockAddrInet _port addr -> do
              pure $ THIPv4 (N.hostAddressToTuple addr)
            unexpected ->
              -- TODO: actually, Apple mandates IPv6 support
              fail $ "Discoverer: expected an IPv4 party, got " <> show unexpected
          traceM $ "Discoverer: go connect " <> show host
          runTransportClient defaultTransportClientConfig Nothing host partyPort (Just expected) $ \tls -> do
            traceM "2PTTH server starting"
            run tls
            traceM "2PTTH server finished"

    run tls = runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
      reqBody <- getHTTP2Body r 16384
      processRequest HTTP2Request {sessionId, request = r, reqBody, sendResponse}

    processRequest req = do
      traceM $ "Got request: " <> show (request req)
      -- TODO: sendResponse req . HTTP2.promiseResponse $ HTTP2.pushPromise path response weight
      sendResponse req $ HTTP2.responseStreaming HTTP.ok200 sseHeaders sseExample

    sseHeaders = [(HTTP.hContentType, "text/event-stream")]

    sseExample :: (Builder -> IO ()) -> IO () -> IO ()
    sseExample write flush = forM_ [1 .. 10] $ \i -> do
      let payload = "[" <> intDec i <> ", \"blah\"]"
      write "event: message\n" -- XXX: SSE header line
      write $ "data: " <> payload <> "\n" -- XXX: SSE payload line
      write "\n" -- XXX: SSE delimiter
      flush
      threadDelay 1000000
