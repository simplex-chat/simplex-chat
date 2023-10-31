{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Remote.RevHTTP where

import Simplex.RemoteControl.Discovery
import Simplex.RemoteControl.Types
import Control.Logger.Simple
import Data.Word (Word16)
import qualified Network.TLS as TLS
import qualified Simplex.Messaging.Crypto as C
import qualified Simplex.Messaging.Transport as Transport
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.HTTP2 (defaultHTTP2BufferSize, getHTTP2Body)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError (..), attachHTTP2Client, bodyHeadSize, connTimeout, defaultHTTP2ClientConfig)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..), runHTTP2ServerWith)
import Simplex.Messaging.Util (ifM)
import UnliftIO

announceRevHTTP2 :: MonadUnliftIO m => Tasks -> (C.PrivateKeyEd25519, Announce) -> TLS.Credentials -> m () -> m (Either HTTP2ClientError HTTP2Client)
announceRevHTTP2 = announceCtrl runHTTP2Client

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

connectRevHTTP2 :: (MonadUnliftIO m) => (TransportHost, Word16) -> C.KeyHash -> (HTTP2Request -> m ()) -> m ()
connectRevHTTP2 serviceAddress fingerprint = connectTLSClient serviceAddress fingerprint . attachHTTP2Server

attachHTTP2Server :: (MonadUnliftIO m) => (HTTP2Request -> m ()) -> Transport.TLS -> m ()
attachHTTP2Server processRequest tls = do
  withRunInIO $ \unlift ->
    runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
      reqBody <- getHTTP2Body r doNotPrefetchHead
      unlift $ processRequest HTTP2Request {sessionId, request = r, reqBody, sendResponse}

-- | Suppress storing initial chunk in bodyHead, forcing clients and servers to stream chunks
doNotPrefetchHead :: Int
doNotPrefetchHead = 0
