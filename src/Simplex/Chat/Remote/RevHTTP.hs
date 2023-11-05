{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Remote.RevHTTP where

import Control.Logger.Simple
import Data.Text (Text)
import Network.Socket (PortNumber)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Transport (TLS)
import Simplex.Messaging.Transport.Client (TransportHost)
import Simplex.Messaging.Transport.HTTP2 (defaultHTTP2BufferSize, getHTTP2Body)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError (..), attachHTTP2Client, bodyHeadSize, connTimeout, defaultHTTP2ClientConfig)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..), runHTTP2ServerWith)
import Simplex.Messaging.Util (ifM)
import Simplex.Messaging.Version (VersionRange)
import Simplex.RemoteControl.Discovery
import Simplex.RemoteControl.Types
import UnliftIO

announceRevHTTP2 :: MonadUnliftIO m => Tasks -> TMVar (Maybe PortNumber) -> Maybe (Text, VersionRange) -> Maybe Text -> C.PrivateKeyEd25519 -> CtrlSessionKeys -> TransportHost -> m () -> m (Either HTTP2ClientError HTTP2Client)
announceRevHTTP2 = announceCtrl runHTTP2Client

-- | Attach HTTP2 client and hold the TLS until the attached client finishes.
runHTTP2Client :: MVar (Either HTTP2ClientError HTTP2Client) -> MVar () -> TLS -> IO ()
runHTTP2Client started finished tls =
  ifM
    (isEmptyMVar started)
    attachClient
    (logError "HTTP2 session already started on this listener")
  where
    attachClient = do
      client <- attachHTTP2Client config ANY_ADDR_V4 "0" (putMVar finished ()) defaultHTTP2BufferSize tls
      putMVar started client
      readMVar finished
    -- TODO connection timeout
    config = defaultHTTP2ClientConfig {bodyHeadSize = doNotPrefetchHead, connTimeout = maxBound}

attachHTTP2Server :: MonadUnliftIO m => TLS -> (HTTP2Request -> m ()) -> m ()
attachHTTP2Server tls processRequest = do
  withRunInIO $ \unlift ->
    runHTTP2ServerWith defaultHTTP2BufferSize ($ tls) $ \sessionId r sendResponse -> do
      reqBody <- getHTTP2Body r doNotPrefetchHead
      unlift $ processRequest HTTP2Request {sessionId, request = r, reqBody, sendResponse}

-- | Suppress storing initial chunk in bodyHead, forcing clients and servers to stream chunks
doNotPrefetchHead :: Int
doNotPrefetchHead = 0
