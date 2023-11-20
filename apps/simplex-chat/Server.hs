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
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Socket
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Messaging.Transport.Server (runTCPServer)
import Simplex.Messaging.Util (raceAny_)
import UnliftIO.Exception
import UnliftIO.STM

simplexChatServer :: ChatServerConfig -> ChatConfig -> ChatOpts -> IO ()
simplexChatServer srvCfg cfg opts =
  simplexChatCore cfg opts . const $ runChatServer srvCfg

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

data ChatSrvRequest = ChatSrvRequest {corrId :: Text, cmd :: Text}
  deriving (Generic, FromJSON)

data ChatSrvResponse = ChatSrvResponse {corrId :: Maybe Text, resp :: ChatResponse}
  deriving (Generic)

instance ToJSON ChatSrvResponse where
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}

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
    ws <- liftIO $ getConnection sock
    c <- atomically $ newChatServerClient clientQSize
    putStrLn "client connected"
    raceAny_ [send ws c, client c, output c, receive ws c]
      `finally` clientDisconnected c
  where
    getConnection sock = WS.makePendingConnection sock WS.defaultConnectionOptions >>= WS.acceptRequest
    send ws ChatClient {sndQ} =
      forever $
        atomically (readTBQueue sndQ) >>= WS.sendTextData ws . J.encode
    client ChatClient {rcvQ, sndQ} = forever $ do
      atomically (readTBQueue rcvQ)
        >>= processCommand
        >>= atomically . writeTBQueue sndQ
    output ChatClient {sndQ} = forever $ do
      (_, _, resp) <- atomically . readTBQueue $ outputQ cc
      atomically $ writeTBQueue sndQ ChatSrvResponse {corrId = Nothing, resp}
    receive ws ChatClient {rcvQ, sndQ} = forever $ do
      s <- WS.receiveData ws
      case J.decodeStrict' s of
        Just ChatSrvRequest {corrId, cmd} -> do
          putStrLn $ "received command " <> show corrId <> " : " <> show cmd
          case parseChatCommand $ encodeUtf8 cmd of
            Right command -> atomically $ writeTBQueue rcvQ (corrId, command)
            Left e -> sendError (Just corrId) e
        Nothing -> sendError Nothing "invalid request"
      where
        sendError corrId e = atomically $ writeTBQueue sndQ ChatSrvResponse {corrId, resp = chatCmdError Nothing e}
    processCommand (corrId, cmd) =
      runReaderT (runExceptT $ processChatCommand cmd) cc >>= \case
        Right resp -> response resp
        Left e -> response $ CRChatCmdError Nothing e
      where
        response resp = pure ChatSrvResponse {corrId = Just corrId, resp}
    clientDisconnected _ = pure ()
