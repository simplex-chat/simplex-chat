{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Discovery
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
import Network.HTTP2.Client (requestNoBody)
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
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Response (..), attachHTTP2Client, defaultHTTP2ClientConfig, sendRequest)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..), runHTTP2ServerWith)
import Simplex.Messaging.Transport.Server (defaultTransportServerConfig, runTransportServer)
import UnliftIO
import UnliftIO.Async
import UnliftIO.Concurrent

runAnnouncer :: (StrEncoding invite, ChatMonad m) => invite -> TLS.Credentials -> m ()
runAnnouncer invite credentials = do
  started <- newEmptyTMVarIO
  aPid <- async $ announcer started (strEncode invite)
  let serverParams =
        def
          { TLS.serverWantClientCert = False,
            TLS.serverShared = def {TLS.sharedCredentials = credentials},
            TLS.serverHooks = def,
            TLS.serverSupported = supportedParameters
          }
  liftIO $ runTransportServer started partyPort serverParams defaultTransportServerConfig (run aPid)
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

    run :: Async () -> Transport.TLS -> IO ()
    run aPid tls = do
      cancel aPid
      finished <- newEmptyTMVarIO
      let partyHost = "255.255.255.255" -- XXX: get from tls somehow?
      res <- attachHTTP2Client defaultHTTP2ClientConfig partyHost partyPort (atomically $ putTMVar finished ()) defaultHTTP2BufferSize tls
      case res of
        Left clientError -> do
          traceM $ "attachHTTP2Client client error: " <> show clientError
        Right client -> do
          traceM "Party connected, have 2ptth client"
          let exampleRequest = requestNoBody HTTP.methodGet "/example/events" [(HTTP.hAccept, "text/event-stream")]
          sendRequest client exampleRequest (Just 10000000) >>= \case
            Left clientError -> do
              traceM $ "attachHTTP2Client client error (request): " <> show clientError
            Right HTTP2Response {response, respBody} -> do
              traceM "attachHTTP2Client: got response"
              traceShowM response
          -- TODO: read events
          -- TODO: receive push
          -- TODO: start pumping events
          atomically $ takeTMVar finished
          traceM "Party disconnected"

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
      write "event: message\n" -- XXX: SSE header line
      write $ "data: " <> "[" <> intDec i <> ", \"blah\"]" <> "\n" -- XXX: SSE payload line
      write "\n" -- XXX: SSE delimiter
      flush
      threadDelay 1000000
