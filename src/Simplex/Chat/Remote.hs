{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Remote where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import qualified Data.Binary.Builder as Binary
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2Client
import Simplex.Chat.Controller
import Simplex.Chat.Remote.Types
import Simplex.Chat.Types
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..))
import qualified Simplex.Messaging.Transport.HTTP2.Client as HTTP2
import qualified Simplex.Messaging.Transport.HTTP2.Server as HTTP2
import Simplex.Messaging.Util (bshow)
import System.Directory (getFileSize)

withRemoteHostSession :: (ChatMonad m) => RemoteHostId -> (RemoteHostSession -> m a) -> m a
withRemoteHostSession remoteHostId action = do
  chatReadVar remoteHostSessions >>= maybe err action . M.lookup remoteHostId
  where
    err = throwError $ ChatErrorRemoteHost remoteHostId RHMissing

closeRemoteHostSession :: (ChatMonad m) => RemoteHostId -> m ()
closeRemoteHostSession rh = withRemoteHostSession rh (liftIO . HTTP2.closeHTTP2Client . ctrlClient)

processRemoteCommand :: (ChatMonad m) => RemoteHostSession -> (ByteString, ChatCommand) -> m ChatResponse
processRemoteCommand rhs = \case
  -- XXX: intercept and filter some commands
  -- TODO: store missing files on remote host
  (s, _cmd) -> relayCommand rhs s

relayCommand :: (ChatMonad m) => RemoteHostSession -> ByteString -> m ChatResponse
relayCommand RemoteHostSession {ctrlClient} s =
  postBytestring Nothing ctrlClient "/relay" mempty s >>= \case
    Left e -> error "TODO: http2chatError"
    Right HTTP2.HTTP2Response {respBody = HTTP2Body {bodyHead}} -> do
      remoteChatResponse <-
        if iTax
          then case J.eitherDecodeStrict bodyHead of -- XXX: large JSONs can overflow into buffered chunks
            Left e -> error "TODO: json2chatError" e
            Right (raw :: J.Value) -> case J.fromJSON (sum2tagged raw) of
              J.Error e -> error "TODO: json2chatError" e
              J.Success cr -> pure cr
          else case J.eitherDecodeStrict bodyHead of -- XXX: large JSONs can overflow into buffered chunks
            Left e -> error "TODO: json2chatError" e
            Right cr -> pure cr
      case remoteChatResponse of
        -- TODO: intercept file responses and fetch files when needed
        -- XXX: is that even possible, to have a file response to a command?
        _ -> pure remoteChatResponse
  where
    iTax = True -- TODO: get from RemoteHost
    -- XXX: extract to http2 transport
    postBytestring timeout c path hs body = liftIO $ HTTP2.sendRequest c req timeout
      where
        req = HTTP2Client.requestBuilder "POST" path hs (Binary.fromByteString body)

storeRemoteFile :: (ChatMonad m) => RemoteHostSession -> FilePath -> m ChatResponse
storeRemoteFile RemoteHostSession {ctrlClient} localFile = do
  postFile Nothing ctrlClient "/store" mempty localFile >>= \case
    Left e -> error "TODO: http2chatError"
    Right HTTP2.HTTP2Response {response} -> case HTTP.statusCode <$> HTTP2Client.responseStatus response of
      Just 200 -> pure $ CRCmdOk Nothing
      unexpected -> error "TODO: http2chatError"
  where
    postFile timeout c path hs file = liftIO $ do
      fileSize <- fromIntegral <$> getFileSize file
      HTTP2.sendRequest c (req fileSize) timeout
      where
        req size = HTTP2Client.requestFile "POST" path hs (HTTP2Client.FileSpec file 0 size)

fetchRemoteFile :: (ChatMonad m) => RemoteHostSession -> FileTransferId -> m ChatResponse
fetchRemoteFile RemoteHostSession {ctrlClient, storePath} remoteFileId = do
  liftIO (HTTP2.sendRequest ctrlClient req Nothing) >>= \case
    Left e -> error "TODO: http2chatError"
    Right HTTP2.HTTP2Response {respBody} -> do
      error "TODO: stream body into a local file" -- XXX: consult headers for a file name?
  where
    req = HTTP2Client.requestNoBody "GET" path mempty
    path = "/fetch/" <> bshow remoteFileId

-- | Convert swift single-field sum encoding into tagged/discriminator-field
sum2tagged :: J.Value -> J.Value
sum2tagged = \case
  J.Object todo'convert -> J.Object todo'convert
  skip -> skip

-- withRemoteCtrlSession :: (ChatMonad m) => RemoteCtrlId -> (RemoteCtrlSession -> m a) -> m a
-- withRemoteCtrlSession remoteCtrlId action = do
--   chatReadVar remoteHostSessions >>= maybe err action . M.lookup remoteCtrlId
--   where
--     err = throwError $ ChatErrorRemoteCtrl (Just remoteCtrlId) RCMissing

processControllerCommand :: (ChatMonad m) => RemoteCtrlId -> HTTP2.HTTP2Request -> m ()
processControllerCommand rc req = error "TODO: processControllerCommand"
