{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Socket
import Numeric.Natural (Natural)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Types
import Simplex.Messaging.Transport.Server (runTCPServer)
import Simplex.Messaging.Util (raceAny_)
import System.IO
import UnliftIO.Exception
import UnliftIO.STM

simplexChatServer :: ChatServerConfig -> ChatConfig -> ChatOpts -> IO ()
simplexChatServer srvCfg cfg opts =
  simplexChatCore cfg opts Nothing . const $ runChatServer srvCfg

data ChatServerConfig = ChatServerConfig
  { chatPort :: ServiceName,
    clientQSize :: Natural
  }

defaultChatServerConfig :: ChatServerConfig
defaultChatServerConfig =
  ChatServerConfig
    { chatPort = "5225",
      clientQSize = 1
    }

data ChatServerEnv = ChatServerEnv
  { user :: User,
    chatController :: ChatController
  }

newChatServerEnv :: User -> ChatController -> IO ChatServerEnv
newChatServerEnv user chatController = pure ChatServerEnv {user, chatController}

data ChatSrvRequest = ChatSrvRequest {corrId :: Text, cmd :: Text}
  deriving (Generic, FromJSON)

data ChatSrvResponse = ChatSrvResponse {corrId :: Text, resp :: ChatResponse}
  deriving (Generic)

instance ToJSON ChatSrvResponse where
  toEncoding = J.genericToEncoding J.defaultOptions

data ChatClient = ChatClient
  { rcvQ :: TBQueue (Text, ChatCommand),
    sndQ :: TBQueue ChatSrvResponse
  }

newChatServerClient :: Natural -> STM ChatClient
newChatServerClient qSize = do
  rcvQ <- newTBQueue qSize
  sndQ <- newTBQueue qSize
  pure ChatClient {rcvQ, sndQ}

runChatServer :: ChatServerConfig -> ChatController -> IO ()
runChatServer ChatServerConfig {chatPort, clientQSize} cc = do
  started <- newEmptyTMVarIO
  runTCPServer started chatPort $ \sock -> do
    h <- liftIO $ getHandle sock
    c <- atomically $ newChatServerClient clientQSize
    raceAny_ [send h c, client c, output c, receive h c]
      `finally` clientDisconnected c
  where
    getHandle sock = do
      h <- socketToHandle sock ReadWriteMode
      hSetBuffering h LineBuffering
      hSetNewlineMode h universalNewlineMode
      pure h
    send h ChatClient {sndQ} =
      forever $
        atomically (readTBQueue sndQ) >>= B.hPutStrLn h . LB.toStrict . J.encode
    client ChatClient {rcvQ, sndQ} = forever $ do
      atomically (readTBQueue rcvQ)
        >>= processCommand
        >>= atomically . writeTBQueue sndQ
    output ChatClient {sndQ} = forever $ do
      (_, resp) <- atomically . readTBQueue $ outputQ cc
      atomically $ writeTBQueue sndQ ChatSrvResponse {corrId = "", resp}
    receive h ChatClient {rcvQ, sndQ} = forever $ do
      s <- B.hGetLine h
      case J.decodeStrict' s of
        Just ChatSrvRequest {corrId, cmd} -> case parseChatCommand $ encodeUtf8 cmd of
          Right command -> atomically $ writeTBQueue rcvQ (corrId, command)
          Left e -> sendError corrId e
        Nothing -> sendError "" "invalid request"
      where
        sendError corrId e = atomically $ writeTBQueue sndQ ChatSrvResponse {corrId, resp = chatCmdError e}
    processCommand (corrId, cmd) =
      runReaderT (runExceptT $ processChatCommand cmd) cc >>= \case
        Right resp -> response resp
        Left e -> response $ CRChatCmdError e
      where
        response resp = pure ChatSrvResponse {corrId, resp}
    clientDisconnected _ = pure ()
